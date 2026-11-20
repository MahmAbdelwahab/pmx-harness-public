# Approval-ledger recording tool (integrity batch 3b tranche 2, item 1).
#
# gates/<ID>/approvals.jsonl is an append-only, git-anchored chain of approval
# events. approve_gate() is the RECORDING STEP run AFTER the SME has signed the
# markdown by hand: it verifies the record passes the full require_gate content
# checks, extracts signer / date / decision FROM THE RECORD through the one
# strict parser (never invents them; refuses when either is empty), refuses a
# stale store (batch 4), a chain break or a duplicate (idempotence), and
# appends exactly one line. It NEVER edits the markdown. revoke_gate() appends a `revoked`
# event (an SME act) — require_gate then refuses the gate until a new approving
# event supersedes it. verify_approvals_ledger() recomputes every event_id and
# checks the per-task chain.
#
# Threat model is ERRING AGENTS ONLY: approvals are digest-in-record and
# git-anchored, not cryptographically signed. The append path is sanctioned
# (the PreToolUse hook blocks agent Edit/Write of gates/**; this tool runs via
# Rscript, which the hook does not intercept).
#
# Run:
#   Rscript tools/approve_gate.R approve <gate_file>     # refuses if the store is stale
#   Rscript tools/approve_gate.R revoke  <gate_file> "<reason>"
#   Rscript tools/approve_gate.R release <ID> <task> --reason "<text>" --signer <initials>
#   Rscript tools/approve_gate.R verify  [<analysis-id>]   # all IDs if omitted

.approve_tools_dir <- function() {
  fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(fa)) return(normalizePath(dirname(sub("^--file=", "", fa[1])), mustWork = FALSE))
  normalizePath("tools", mustWork = FALSE)
}

local({
  td <- .approve_tools_dir()
  if (!exists("require_gate", inherits = TRUE))
    source(file.path(td, "gate.R"))
  if (!exists("parse_gate_record", inherits = TRUE))
    source(file.path(td, "gate.R"))
})

# --- record parsing ----------------------------------------------------------
# ONE parser (batch 4, item A): decision / signer / date / identity all come
# from parse_gate_record (tools/gate.R). The ledger's task key stays the gate
# filename stem and the analysis id its parent dir — the parser verifies the
# record's own Stage: / Analysis ID: lines against exactly those.
.gate_task_key <- function(gate_file) sub("\\.md$", "", basename(gate_file))
.gate_analysis_id <- function(gate_file) basename(dirname(normalizePath(gate_file, mustWork = FALSE)))

# sha256 over the canonical concatenation of the artifacts table's path + sha256
# columns (path then sha256 per row, each terminated by LF, in table order).
# Records with no artifacts table (or no sha256 column) digest the empty string.
.gate_artifact_manifest_digest <- function(block) {
  parsed <- .gate_parse_artifacts(block)
  payload <- ""
  if (isTRUE(parsed$has_table) && is.null(parsed$error) &&
      all(c("path", "sha256") %in% (parsed$cols %||% character(0)))) {
    payload <- paste0(vapply(parsed$rows, function(r)
      paste0(unname(r[["path"]]), "\n", unname(r[["sha256"]]), "\n"),
      character(1)), collapse = "")
  }
  paste0("sha256:", digest::digest(payload, algo = "sha256", serialize = FALSE))
}
`%||%` <- function(a, b) if (is.null(a)) b else a

# --- git anchoring -----------------------------------------------------------
.git_head <- function() {
  out <- suppressWarnings(tryCatch(
    system2("git", c("rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE),
    error = function(e) character(0)))
  if (length(out)) trimws(out[1]) else "UNKNOWN"
}
# The WORKTREE BASE REVISION at event time: the HEAD sha, or the literal
# "UNCOMMITTED" when the gate file itself has uncommitted changes. It is NOT a
# claim about the commit that will contain this event — an event cannot name
# the future commit that will carry it — and it is not an error: a gate is
# normally signed and ledgered before the change is committed, so "UNCOMMITTED"
# is the ordinary case. Recorded honestly rather than silently blessed;
# tools/ci_check.R reports how many events carry it (informational, not a FAIL).
.gate_package_revision <- function(gate_file) {
  st <- suppressWarnings(tryCatch(
    system2("git", c("status", "--porcelain", "--", gate_file), stdout = TRUE,
            stderr = FALSE), error = function(e) character(0)))
  if (length(st) && nzchar(trimws(paste(st, collapse = "")))) "UNCOMMITTED" else .git_head()
}

# --- JSON line writer --------------------------------------------------------
.ledger_to_json <- function(ev) {
  q <- function(v) if (is.null(v) || (length(v) == 1 && is.na(v))) "null"
                   else paste0('"', .json_escape(as.character(v)), '"')
  order <- c("schema_version", "event_id", "analysis_id", "task",
             "previous_event_id", "decision", "signer", "signed_at",
             "recorded_at_utc", "package_revision", "record_digest",
             "package_digest", "artifact_manifest_digest", "reason")
  pairs <- vapply(order, function(f) {
    if (f == "schema_version") paste0('"schema_version":', as.integer(ev$schema_version))
    else paste0('"', f, '":', q(ev[[f]]))
  }, character(1))
  paste0("{", paste(pairs, collapse = ","), "}")
}

# Build an event (event_id computed last from all other fields).
# ONE IMMUTABLE RECORD SNAPSHOT (SME decision D14-1, 2026-09-05; Codex round 13
# R13-P1-2). Every ledger event is built from a single read of the record
# bytes: those bytes are parsed, validated, hashed and (for a release)
# archived, and the digest that goes into the event is the digest OF THOSE
# BYTES. Until integrity batch 14 the approval parsed the record under the lock
# and then let .build_event() hash the file again, so an SME editor saving
# between the two reads produced an event carrying the old signer with the new
# record's digest -- and a release computed its archive name from one read and
# its archive bytes from another. Editors are NOT required to take the ledger
# lock (D14-1); instead the drift is DETECTED, before the event is committed,
# by comparing the live file with the snapshot, and a later edit of the live
# record fails closed at the next gate check.
.gate_snapshot <- function(gate_file) {
  if (!file.exists(gate_file)) stop("Gate record not found: ", gate_file, call. = FALSE)
  bytes <- readBin(gate_file, what = "raw", n = file.info(gate_file)$size)
  txt <- rawToChar(bytes); Encoding(txt) <- "UTF-8"
  lines <- strsplit(txt, "\r\n|\n|\r")[[1]]
  list(bytes = bytes, lines = lines,
       digest = paste0("sha256:", digest::digest(bytes, algo = "sha256", serialize = FALSE)))
}

# Refuse when the live record no longer equals the snapshot the event was built
# from. Called immediately before the ledger append on every writing path.
.gate_snapshot_assert <- function(who, gate_file, snap) {
  live <- if (file.exists(gate_file)) .gate_sha256(gate_file) else "<missing>"
  if (!identical(live, snap$digest))
    stop(who, ": ", gate_file, " drifted after the snapshot this event was built ",
         "from (snapshot ", snap$digest, ", live now ", live, "). An event is ",
         "built from ONE immutable snapshot of the record bytes -- parsed, ",
         "validated, hashed and archived from that same read -- so a record ",
         "rewritten while the event was being prepared is a refusal, never a ",
         "mixture of old fields and a new digest. Nothing was recorded. ",
         "ESCALATE to the SME.", call. = FALSE)
  invisible(TRUE)
}

.build_event <- function(gate_file, decision, reason = NA_character_,
                         parsed = NULL, record_digest = NULL) {
  if (is.null(parsed) || is.null(record_digest))
    stop(".build_event: an event is built from ONE record snapshot; the caller ",
         "passes both the parse of that snapshot and its digest (SME decision ",
         "D14-1). Never present a freshly re-read digest as the digest of ",
         "previously parsed fields.", call. = FALSE)
  ev <- list(
    schema_version = 1L,
    analysis_id = .gate_analysis_id(gate_file),
    task = .gate_task_key(gate_file),
    previous_event_id = NA_character_,   # set by caller
    decision = decision,
    signer = parsed$signer,
    signed_at = parsed$date,
    recorded_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    package_revision = .gate_package_revision(gate_file),
    record_digest = record_digest,
    package_digest = parsed$package_digest,
    artifact_manifest_digest = .gate_artifact_manifest_digest(parsed$block),
    reason = reason)
  ev
}

.append_event <- function(gate_file, ev) {
  if (.gate_reserved_signer(ev$signer))
    stop("Approval ledger signer uses a reserved role label.", call. = FALSE)
  ev$event_id <- .ledger_event_id(ev)
  path <- .ledger_path(gate_file)
  if (file.exists(path) && file.info(path)$size > 0) {
    con <- file(path, "rb")
    on.exit(close(con), add = TRUE)
    seek(con, where = -1, origin = "end")
    if (!identical(readBin(con, "raw", n = 1L), charToRaw("\n")))
      stop("Approval ledger does not end with a newline: ", path, call. = FALSE)
  }
  cat(.ledger_to_json(ev), "\n", sep = "", file = path, append = TRUE)
  invisible(ev)
}

# Chain + completeness verification for one task's events: every stored
# event_id must recompute, previous_event_id links must form one unbroken chain
# in append order, and (batch 4, item B) every event must carry its required
# fields — an approving event with a null signer or signed_at is a defect even
# when its chain link is intact. Shared primitive: tools/gate.R::.ledger_verify_chain.
.verify_task_chain <- function(events) {
  .verify_approval_events(events)
}

# Shared approval append path. Callers hold the per-ledger lock and pass one
# validated immutable record snapshot. Keeping chain verification, event
# construction, duplicate detection, the final drift assertion, and append in
# one helper prevents the manual and staged approval paths from diverging.
.append_approval_under_lock <- function(who, gate_file, snap, parsed,
                                        expected_ledger = NULL,
                                        .before_append = NULL,
                                        guard_regular_gate = FALSE) {
  decision <- parsed$decision
  if (!decision %in% .LEDGER_APPROVING ||
      is.na(parsed$signer) || !nzchar(trimws(parsed$signer)) ||
      is.na(parsed$date) || !nzchar(trimws(parsed$date)) ||
      is.null(parsed$package_digest) || is.na(parsed$package_digest) ||
      !nzchar(trimws(parsed$package_digest)))
    stop(who, ": the record validated under the ledger lock is not an approving, ",
         "signed, digest-bearing record; nothing was recorded.", call. = FALSE)

  if (!is.null(expected_ledger)) {
    current_ledger <- .approval_ledger_snapshot(gate_file, who)
    if (!identical(current_ledger$digest, expected_ledger$digest) ||
        !identical(current_ledger$bytes, expected_ledger$bytes))
      stop(who, ": approval ledger drifted after its locked snapshot; nothing ",
           "was recorded.", call. = FALSE)
  }
  events <- if (!is.null(expected_ledger)) {
    .approval_ledger_events(expected_ledger, gate_file, who)
  } else tryCatch(.read_ledger(.ledger_path(gate_file)), error = function(e)
    stop(who, ": approval ledger cannot be parsed: ", conditionMessage(e),
         ". Nothing was recorded.", call. = FALSE))
  task <- .gate_task_key(gate_file)
  task_events <- Filter(function(e) identical(as.character(e$task), task), events)
  chain_problems <- .verify_task_chain(task_events)
  if (length(chain_problems))
    stop(who, ": existing ledger chain for '", task, "' is broken:\n  - ",
         paste(chain_problems, collapse = "\n  - "), call. = FALSE)

  latest <- .ledger_latest_for_task(events, task)
  ev <- .build_event(gate_file, decision, parsed = parsed,
                     record_digest = snap$digest)
  if (!is.null(latest)) {
    if (identical(as.character(latest$decision), decision) &&
        identical(as.character(latest$record_digest), ev$record_digest) &&
        identical(as.character(latest$package_digest),
                  ev$package_digest %||% NA_character_))
      stop(who, ": an identical latest event already exists for '", task,
           "' (idempotence); nothing to record.", call. = FALSE)
    ev$previous_event_id <- as.character(latest$event_id)
  }
  if (isTRUE(guard_regular_gate))
    .approval_gate_snapshot_assert(who, gate_file, snap)
  else
    .gate_snapshot_assert(who, gate_file, snap)
  if (!is.null(expected_ledger)) {
    final_ledger <- .approval_ledger_snapshot(gate_file, who)
    if (!identical(final_ledger$digest, expected_ledger$digest) ||
        !identical(final_ledger$bytes, expected_ledger$bytes))
      stop(who, ": approval ledger drifted before append; nothing was recorded.",
           call. = FALSE)
  }
  if (is.function(.before_append)) .before_append(gate_file, ev)
  if (isTRUE(guard_regular_gate))
    .approval_gate_snapshot_assert(who, gate_file, snap)
  .append_event(gate_file, ev)
}

# ============================================================================
# Public API
# ============================================================================

# Record the SME's hand signature of an approving gate. Refuses on: content
# check failure, a non-approving record, a broken existing chain, or an
# already-recorded identical latest event (idempotence).
# check_store (batch 4, item B; SME ruling 2026-09-02): approving is refused when
# the gate's `<task>_package` target is OUTDATED in the analysis store — a
# verifier/comparator code change requires fresh execution before any new
# approval. FALSE is for hermetic tests of the ledger alone.
# `.after_lock` is a TEST-ONLY injection point (integrity batch 13, R12-P1-1):
# a function run immediately after the ledger lock is acquired and before the
# record is re-parsed, so the battery can rewrite the record in exactly the
# window this function closes. Production callers never pass it.
# `.after_snapshot` is a second TEST-ONLY injection point (integrity batch 14,
# R13-P1-2): a function run immediately after the record snapshot is taken and
# validated under the lock, so the battery can rewrite the live record in
# exactly the window the drift check closes. Production callers never pass it.
approve_gate <- function(gate_file, check_store = TRUE, .after_lock = NULL,
                         .after_snapshot = NULL, .after_live_validation = NULL,
                         .after_snapshot_read = NULL) {
  suppressMessages(require_gate(gate_file, enforce_ledger = FALSE))  # content only
  parsed <- parse_gate_record(gate_file)
  if (is.function(.after_live_validation)) .after_live_validation()
  pre_digest <- .gate_sha256(gate_file)
  decision <- parsed$decision
  if (!decision %in% .LEDGER_APPROVING)
    stop("approve_gate: record decision is '", decision,
         "', not an approval. Use revoke_gate() for a revocation.", call. = FALSE)
  # Never record an approving event with an empty signer or date (item B).
  if (is.na(parsed$signer) || !nzchar(trimws(parsed$signer)))
    stop("approve_gate: the record's Signed: line carries no signer; refusing to ",
         "record an approving event.", call. = FALSE)
  if (is.na(parsed$date) || !nzchar(trimws(parsed$date)))
    stop("approve_gate: the record carries no signature date (Date: on the Signed: ",
         "line or its own line); refusing to record an approving event.", call. = FALSE)
  # Never record an approving event with an empty package_digest (item B). A
  # legacy DIGESTLESS record (frozen in tools/legacy_gates.yaml, no
  # Package-Digest line) passes require_gate on the legacy path, but the ledger
  # requires package_digest on every approving event, so recording it would
  # break the task chain. This refusal is independent of check_store.
  if (is.null(parsed$package_digest) || is.na(parsed$package_digest) ||
      !nzchar(trimws(parsed$package_digest)))
    stop("approve_gate: the record carries no Package-Digest line (legacy ",
         "digestless signature); a legacy digestless record cannot enter the ",
         "ledger. The SME releases it by hand (clearing the decision fields), ",
         "the package is re-rendered from a fresh store and signed with a ",
         "digest, and only then can the ledger record it.", call. = FALSE)
  if (check_store) {
    if (!exists("require_fresh_store", mode = "function"))
      source(file.path(.approve_tools_dir(), "gate_package.R"))
    require_fresh_store(.gate_analysis_id(gate_file), .gate_task_key(gate_file))
  }

  # THE LEDGER LOCK (integrity batch 12, R11-P1-1). Every writer of a ledger
  # -- approve, revoke, release -- takes the same per-ledger lock before its
  # FIRST ledger read and holds it through the append. An approval that slipped
  # in between a release's tail comparison and its append gave two events the
  # same predecessor: a forked chain, which the append-order verifier then
  # refuses. Serialising all three writers closes that window.
  .ledger_lock <- .release_lock_acquire(.ledger_path(gate_file), who = "approve_gate")
  on.exit(.release_lock_free(.ledger_lock), add = TRUE)
  if (is.function(.after_lock)) .after_lock()

  # RE-PARSE UNDER THE LOCK (integrity batch 13, Codex round 12, R12-P1-1).
  # The checks above ran on a record that another writer -- a release, an
  # SME edit -- may have replaced while this process waited for the lock. The
  # event used to combine those stale fields (decision, signer, date, package
  # digest) with a record digest computed NOW, so a waiter could append an
  # approval built from bytes it never validated. The record is therefore
  # required to be byte-identical to what was validated, and every field of
  # the event is taken from the parse made under the lock.
  # ONE snapshot (SME decision D14-1): the bytes read here are the bytes
  # parsed, validated and hashed below, and the digest of these bytes is the
  # digest that goes into the event.
  snap <- .gate_snapshot(gate_file)
  post_digest <- snap$digest
  if (!identical(post_digest, pre_digest))
    stop("approve_gate: ", gate_file, " changed between the preliminary validation ",
         "and the ledger lock (validated ", pre_digest, ", under the lock ",
         post_digest, "); another writer rewrote the record while this approval ",
         "waited. Nothing was recorded. Re-read the record as it now stands and ",
         "approve it again if it is still the one the SME signed.", call. = FALSE)
  if (is.function(.after_snapshot_read)) .after_snapshot_read()
  # FULL VALIDATION RUNS ON THE SNAPSHOT (integrity batch 15, Codex round 14
  # R14-P1-3). This call used to re-read the path, so the record that satisfied
  # the signature/package binding and the artifact rehash was not necessarily
  # the record whose fields, digest and archive went into the event. Handing
  # `require_gate()` the snapshot's lines and bytes closes that gap: what is
  # validated here is exactly what is parsed on the next line and hashed into
  # the event.
  suppressMessages(require_gate(gate_file, enforce_ledger = FALSE,
                                lines = snap$lines, bytes = snap$bytes))
  parsed <- parse_gate_record(gate_file, lines = snap$lines)
  if (is.function(.after_snapshot)) .after_snapshot()
  rec <- .append_approval_under_lock("approve_gate", gate_file, snap, parsed)
  message("Recorded ", decision, " event for ", gate_file,
          " (event_id ", rec$event_id, ")")
  invisible(rec)
}

# --- staged sign + approve transaction --------------------------------------
# `absent` is deliberately not a digest-shaped value, so a missing ledger and
# an existing empty ledger are distinct transaction preconditions.
.APPROVAL_LEDGER_ABSENT <- "absent"

.approval_raw_digest <- function(bytes) paste0(
  "sha256:", digest::digest(bytes, algo = "sha256", serialize = FALSE))

.approval_assert_regular_path <- function(path, who, allow_absent = FALSE) {
  link <- Sys.readlink(path)
  if (length(link) && !is.na(link[1]) && nzchar(link[1]))
    stop(who, ": refusing symlink path ", path, ".", call. = FALSE)
  if (!file.exists(path)) {
    if (allow_absent) return(invisible(NULL))
    stop(who, ": file not found: ", path, call. = FALSE)
  }
  info <- file.info(path)
  if (isTRUE(info$isdir) || is.na(info$size) ||
      (exists(".gate_is_regular_file", mode = "function") &&
       !isTRUE(.gate_is_regular_file(path))))
    stop(who, ": expected a regular file: ", path, call. = FALSE)
  invisible(info)
}

.approval_read_raw <- function(path, who, allow_absent = FALSE) {
  info <- .approval_assert_regular_path(path, who, allow_absent = allow_absent)
  if (is.null(info)) return(NULL)
  bytes <- readBin(path, "raw", info$size)
  # A second path check closes tested swaps that occur during a read. This is a
  # fail-closed behavioral check; base R does not provide an atomic no-follow
  # file-descriptor read here.
  .approval_assert_regular_path(path, who)
  bytes
}

.approval_gate_snapshot <- function(gate_file, who) {
  bytes <- .approval_read_raw(gate_file, who)
  txt <- rawToChar(bytes); Encoding(txt) <- "UTF-8"
  lines <- strsplit(txt, "\r\n|\n|\r")[[1]]
  list(bytes = bytes, lines = lines, digest = .approval_raw_digest(bytes))
}

.approval_gate_snapshot_assert <- function(who, gate_file, snap) {
  live <- .approval_gate_snapshot(gate_file, who)
  if (!identical(live$bytes, snap$bytes) || !identical(live$digest, snap$digest))
    stop(who, ": ", gate_file, " drifted after its guarded snapshot; nothing ",
         "was recorded.", call. = FALSE)
  invisible(TRUE)
}

.approval_ledger_snapshot_assert <- function(who, gate_file, snap) {
  live <- .approval_ledger_snapshot(gate_file, who)
  if (!identical(live$bytes, snap$bytes) || !identical(live$digest, snap$digest))
    stop(who, ": approval ledger drifted after its guarded snapshot; nothing ",
         "was changed or recorded.", call. = FALSE)
  invisible(TRUE)
}

.approval_ledger_snapshot <- function(gate_file, who) {
  path <- .ledger_path(gate_file)
  bytes <- .approval_read_raw(path, who, allow_absent = TRUE)
  list(path = path, bytes = bytes,
       digest = if (is.null(bytes)) .APPROVAL_LEDGER_ABSENT
                else .approval_raw_digest(bytes))
}

.approval_signature_layout <- function(lines, who) {
  bi <- which(lines == .GATE_RE$block_start)
  bj <- which(lines == .GATE_RE$block_end)
  body <- if (length(bi) == 1L && length(bj) == 1L && bj > bi)
    lines[-(bi:bj)] else lines
  rows <- list(
    approved = grep("^\\s*-\\s*\\[[ xX]\\]\\s*APPROVED(\\s+—|\\s*$)", body,
                    perl = TRUE),
    approved_with_changes = grep(
      "^\\s*-\\s*\\[[ xX]\\]\\s*APPROVED WITH CHANGES(\\s+—|\\s*$)",
      body, perl = TRUE),
    rejected = grep("^\\s*-\\s*\\[[ xX]\\]\\s*REJECTED(\\s+—|\\s*$)",
                    body, perl = TRUE),
    signed = grep(.GATE_RE$signed, body),
    date = grep(.GATE_RE$date, body),
    digest = grep(.GATE_RE$digest_signed, body))
  counts <- vapply(rows, length, integer(1))
  if (any(counts != 1L) || grepl("Date:", sub(.GATE_RE$signed, "", body[rows$signed]),
                                  fixed = TRUE))
    stop(who, ": expected exactly one standalone row for each of APPROVED, ",
         "APPROVED WITH CHANGES, REJECTED, Signed, Date, and Digest signed; ",
         "the decision/signature layout is ambiguous or unsupported.", call. = FALSE)
  # The generated block is excluded above. Convert body indices back to the
  # original vector so byte-mutation comparisons name exact line positions.
  keep <- if (length(bi) == 1L && length(bj) == 1L && bj > bi)
    setdiff(seq_along(lines), bi:bj) else seq_along(lines)
  lapply(rows, function(i) keep[i])
}

.approval_assert_candidate <- function(gate_file, bytes, who) {
  parts <- .split_record_bytes(bytes, who)
  if (!identical(.join_record_bytes(parts), bytes))
    stop(who, ": candidate line splitting did not reproduce its exact bytes.",
         call. = FALSE)
  layout <- .approval_signature_layout(parts$lines, who)
  parsed <- parse_gate_record(gate_file, lines = parts$lines)
  if (length(parsed$problems))
    stop(who, ": candidate is not a modern clean gate record:\n  - ",
         paste(parsed$problems, collapse = "\n  - "), call. = FALSE)
  if (!parsed$decision %in% .LEDGER_APPROVING)
    stop(who, ": candidate decision is '", parsed$decision,
         "', not an approval.", call. = FALSE)
  if (is.na(parsed$package_digest) ||
      !grepl("^canon2:sha256:[0-9a-f]{64}$", parsed$package_digest))
    stop(who, ": candidate lacks one modern canon2 Package-Digest.", call. = FALSE)
  suppressMessages(require_gate(gate_file, enforce_ledger = FALSE,
                                lines = parts$lines, bytes = bytes))
  list(bytes = bytes, lines = parts$lines, parts = parts, layout = layout,
       parsed = parsed, digest = .approval_raw_digest(bytes))
}

.approval_assert_normal_transition <- function(gate_file, base, candidate, who) {
  base_layout <- .approval_signature_layout(base$lines, who)
  base_parsed <- parse_gate_record(gate_file, lines = base$lines)
  if (length(base_parsed$problems) ||
      !identical(base_parsed$decision, "unsigned") ||
      !is.na(base_parsed$signer) || !is.na(base_parsed$date) ||
      nzchar(base_parsed$digest_signed) || is.na(base_parsed$package_digest) ||
      !grepl("^canon2:sha256:[0-9a-f]{64}$", base_parsed$package_digest))
    stop(who, ": normal base must be one modern, clean, unsigned record with ",
         "all four signature fields blank; signed, rejected, legacy, and ",
         "malformed bases are refused.", call. = FALSE)
  if (!identical(base$parts$eol, candidate$parts$eol) ||
      !identical(base$parts$trailing, candidate$parts$trailing) ||
      length(base$lines) != length(candidate$lines))
    stop(who, ": candidate must preserve the base record's line endings and ",
         "non-signature byte layout.", call. = FALSE)
  choice <- candidate$parsed$decision
  allowed <- unname(c(base_layout[[choice]], base_layout$signed,
                      base_layout$date, base_layout$digest))
  changed <- which(base$lines != candidate$lines)
  if (!identical(sort(changed), sort(allowed)))
    stop(who, ": candidate must differ from the confirmed base only in exactly ",
         "the four decision, Signed, Date, and Digest signed fields; refusing ",
         "a body or package replacement.", call. = FALSE)
  invisible(TRUE)
}

.approval_ledger_events <- function(snapshot, gate_file, who) {
  if (is.null(snapshot$bytes)) return(list())
  text <- tryCatch(rawToChar(snapshot$bytes), error = function(e)
    stop(who, ": approval ledger snapshot is not valid text: ",
         conditionMessage(e), call. = FALSE))
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  lines <- sub("\r$", "", lines)
  lines <- lines[nzchar(trimws(lines))]
  events <- tryCatch(lapply(lines, .json_parse_flat), error = function(e)
    stop(who, ": approval ledger cannot be parsed: ", conditionMessage(e),
         call. = FALSE))
  task <- .gate_task_key(gate_file)
  problems <- .verify_task_chain(Filter(
    function(e) identical(as.character(e$task), task), events))
  if (length(problems))
    stop(who, ": existing ledger chain for '", task, "' is broken:\n  - ",
         paste(problems, collapse = "\n  - "), call. = FALSE)
  events
}

.approval_completed_retry <- function(gate_file, candidate, ledger,
                                      expected_base_ledger, who) {
  bytes <- ledger$bytes
  if (is.null(bytes) || !length(bytes) || bytes[length(bytes)] != as.raw(0x0a))
    return(NULL)
  lf <- which(bytes == as.raw(0x0a))
  line_start <- if (length(lf) == 1L) 1L else lf[length(lf) - 1L] + 1L
  line_end <- lf[length(lf)] - 1L
  if (line_end < line_start) return(NULL)
  prefix <- if (line_start == 1L) raw(0) else bytes[seq_len(line_start - 1L)]
  prefix_digest <- if (!length(prefix)) .APPROVAL_LEDGER_ABSENT
                   else .approval_raw_digest(prefix)
  if (!identical(prefix_digest, expected_base_ledger)) return(NULL)
  line <- rawToChar(bytes[line_start:line_end])
  ev <- tryCatch(.json_parse_flat(line), error = function(e) NULL)
  if (is.null(ev)) return(NULL)
  task <- .gate_task_key(gate_file)
  matching <- identical(as.character(ev$analysis_id), .gate_analysis_id(gate_file)) &&
    identical(as.character(ev$task), task) &&
    identical(as.character(ev$decision), candidate$parsed$decision) &&
    identical(as.character(ev$record_digest), candidate$digest) &&
    identical(as.character(ev$package_digest), candidate$parsed$package_digest)
  if (!matching) return(NULL)
  events <- .approval_ledger_events(ledger, gate_file, who)
  if (!length(events) ||
      !identical(as.character(events[[length(events)]]$event_id),
                 as.character(ev$event_id))) return(NULL)
  ev
}

# Internal implementation accepts callbacks only so hermetic tests can stop at
# exact filesystem boundaries. The public function and production CLI never
# accept or pass them.
.approve_staged_gate_impl <- function(gate_file, candidate_bytes,
                                      expected_base_record, expected_base_ledger,
                                      check_store = TRUE, hooks = list()) {
  who <- "approve_staged_gate"
  if (!is.raw(candidate_bytes) || !length(candidate_bytes))
    stop(who, ": candidate_bytes must be a non-empty raw vector.", call. = FALSE)
  if (!is.character(expected_base_record) || length(expected_base_record) != 1L ||
      !grepl("^sha256:[0-9a-f]{64}$", expected_base_record))
    stop(who, ": expected_base_record must be one sha256 digest.", call. = FALSE)
  if (!is.character(expected_base_ledger) || length(expected_base_ledger) != 1L ||
      !(identical(expected_base_ledger, .APPROVAL_LEDGER_ABSENT) ||
        grepl("^sha256:[0-9a-f]{64}$", expected_base_ledger)))
    stop(who, ": expected_base_ledger must be 'absent' or one sha256 digest.",
         call. = FALSE)
  .approval_read_raw(gate_file, who)
  candidate <- .approval_assert_candidate(gate_file, candidate_bytes, who)

  ledger_path <- .ledger_path(gate_file)
  lock <- .release_lock_acquire(ledger_path, who = who)
  on.exit(.release_lock_free(lock), add = TRUE)
  if (is.function(hooks$after_lock)) hooks$after_lock(gate_file, ledger_path)

  # Revalidate candidate content and artifacts under the same lock that guards
  # the record transition and append. Candidate bytes are immutable in memory;
  # this second check closes freshness changes between the preliminary refusal
  # checks and lock acquisition.
  candidate <- .approval_assert_candidate(gate_file, candidate_bytes, who)
  live_snap <- .approval_gate_snapshot(gate_file, who)
  live_parts <- .split_record_bytes(live_snap$bytes, who)
  live <- c(live_snap, list(parts = live_parts))
  ledger <- .approval_ledger_snapshot(gate_file, who)
  .approval_ledger_events(ledger, gate_file, who)
  if (!is.null(ledger$bytes) && length(ledger$bytes) &&
      ledger$bytes[length(ledger$bytes)] != as.raw(0x0a))
    stop(who, ": existing ledger lacks its final line terminator; appending would ",
         "merge two events. Nothing was changed.", call. = FALSE)
  if (check_store) {
    if (!exists("require_fresh_store", mode = "function"))
      source(file.path(.approve_tools_dir(), "gate_package.R"))
    require_fresh_store(.gate_analysis_id(gate_file), .gate_task_key(gate_file))
  }

  record_is_base <- identical(live$digest, expected_base_record)
  record_is_candidate <- identical(live$digest, candidate$digest)
  ledger_is_base <- identical(ledger$digest, expected_base_ledger)

  if (record_is_candidate && !ledger_is_base) {
    completed <- .approval_completed_retry(
      gate_file, candidate, ledger, expected_base_ledger, who)
    if (!is.null(completed)) {
      if (is.function(hooks$before_completed_return))
        hooks$before_completed_return(gate_file, ledger_path)
      # A completed retry reports success only while both guarded paths still
      # equal the exact candidate-plus-one-event state matched above.
      .approval_gate_snapshot_assert(who, gate_file, live_snap)
      .approval_ledger_snapshot_assert(who, gate_file, ledger)
      message("Staged approval already completed for ", gate_file,
              " (event_id ", completed$event_id, "); no event appended")
      return(invisible(completed))
    }
  }

  if (record_is_base && ledger_is_base) {
    .approval_assert_normal_transition(gate_file, live, candidate, who)
    tmp <- tempfile(paste0(".", basename(gate_file), ".approval-staging-"),
                    tmpdir = dirname(gate_file))
    staged <- TRUE
    on.exit(if (staged && file.exists(tmp)) unlink(tmp), add = TRUE)
    writeBin(candidate$bytes, tmp)
    Sys.chmod(tmp, "0600")
    back <- .approval_read_raw(tmp, who)
    if (!identical(back, candidate$bytes))
      stop(who, ": staged candidate did not read back byte-for-byte; nothing ",
           "was changed.", call. = FALSE)
    if (is.function(hooks$after_stage)) hooks$after_stage(tmp, gate_file)
    if (is.function(hooks$before_rename)) hooks$before_rename(tmp, gate_file)
    # Re-establish every locked transaction precondition immediately before
    # replacement. These guarded equality checks are behavioral; they do not
    # make the following path-based rename OS-atomically no-follow.
    .approval_gate_snapshot_assert(who, gate_file, live_snap)
    .approval_ledger_snapshot_assert(who, gate_file, ledger)
    staged_now <- .approval_read_raw(tmp, who)
    if (!identical(staged_now, candidate$bytes) ||
        !identical(.approval_raw_digest(staged_now), candidate$digest))
      stop(who, ": staged candidate drifted before replacement; nothing was ",
           "changed or recorded.", call. = FALSE)
    if (!file.rename(tmp, gate_file))
      stop(who, ": candidate rename failed; live record and ledger are ",
           "unchanged.", call. = FALSE)
    staged <- FALSE
    if (is.function(hooks$after_rename)) hooks$after_rename(gate_file)
  } else if (!record_is_candidate || !ledger_is_base) {
    stop(who, ": live record/ledger state no longer matches the confirmed ",
         "transaction, and is not an exact interrupted or completed retry. ",
         "Nothing was recorded. Re-run preview.", call. = FALSE)
  }

  # Normal transition now reaches the same state as rename-complete / append-
  # incomplete recovery: candidate is live and the ledger equals the preview.
  live_candidate <- .approval_gate_snapshot(gate_file, who)
  if (!identical(live_candidate$bytes, candidate$bytes))
    stop(who, ": live record drifted after candidate replacement; no approval ",
         "event was appended. Retain the bundle and escalate.", call. = FALSE)
  suppressMessages(require_gate(gate_file, enforce_ledger = FALSE,
                                lines = candidate$lines, bytes = candidate$bytes))
  parsed <- parse_gate_record(gate_file, lines = candidate$lines)
  before_append <- if (is.function(hooks$before_append)) hooks$before_append else NULL
  rec <- .append_approval_under_lock(who, gate_file, live_candidate, parsed,
                                     expected_ledger = ledger,
                                     .before_append = before_append,
                                     guard_regular_gate = TRUE)
  message("Applied staged approval for ", gate_file,
          " (event_id ", rec$event_id, ")")
  invisible(rec)
}

approve_staged_gate <- function(gate_file, candidate_bytes,
                                expected_base_record, expected_base_ledger,
                                check_store = TRUE) {
  .approve_staged_gate_impl(gate_file, candidate_bytes, expected_base_record,
                            expected_base_ledger, check_store = check_store,
                            hooks = list())
}

.gate_nonempty_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
}

revoke_gate <- function(gate_file, reason, .after_snapshot = NULL) {
  if (missing(reason) || !.gate_nonempty_string(reason))
    stop("revoke_gate requires a non-empty reason.", call. = FALSE)
  # The same per-ledger lock as approve_gate() and release_gate(), taken before
  # the first ledger read and held through the append (batch 12, R11-P1-1).
  .ledger_lock <- .release_lock_acquire(.ledger_path(gate_file), who = "revoke_gate")
  on.exit(.release_lock_free(.ledger_lock), add = TRUE)
  # ONE snapshot, taken under the lock, parsed and hashed from the same read
  # (SME decision D14-1).
  snap <- .gate_snapshot(gate_file)
  parsed <- parse_gate_record(gate_file, lines = snap$lines)
  if (is.function(.after_snapshot)) .after_snapshot()
  events <- .read_ledger(.ledger_path(gate_file))
  task <- .gate_task_key(gate_file)
  task_events <- Filter(function(e) identical(as.character(e$task), task), events)
  chain_problems <- .verify_task_chain(task_events)
  if (length(chain_problems))
    stop("revoke_gate: existing ledger chain for '", task, "' is broken:\n  - ",
         paste(chain_problems, collapse = "\n  - "), call. = FALSE)
  latest <- .ledger_latest_for_task(events, task)
  ev <- .build_event(gate_file, "revoked", reason = reason, parsed = parsed,
                     record_digest = snap$digest)
  if (!is.null(latest)) ev$previous_event_id <- as.character(latest$event_id)
  .gate_snapshot_assert("revoke_gate", gate_file, snap)
  rec <- .append_event(gate_file, ev)
  message("Recorded revoked event for ", gate_file, " (event_id ", rec$event_id, ")")
  invisible(rec)
}

# --- release ----------------------------------------------------------------
# RELEASE AN APPROVED RECORD (round 6, finding 11). Re-versioning a signed gate
# used to mean hand-editing the markdown with `sed` — uncheck the decision box,
# blank Signed/Date/Digest signed — so that render_gate_from_store() would agree
# to rewrite it. That failed CLOSED (the old ledger digest stopped matching),
# but it left no event saying WHY the signed version was released, and the exact
# signed bytes were lost. The ledger then read approval-then-approval with
# nothing in between.
#
# release_gate() makes the release a first-class, ledgered, archived act:
#   1. verify the record is currently APPROVED and that its bytes still match
#      the latest ledger event for the gate (fail closed on any mismatch);
#   2. archive the exact released bytes, content-addressed, under
#      gates/<ID>/history/<task>.<full sha256 hex>.md — never overwritten
#      (the full digest, not a prefix: round 7, finding 17);
#   3. append a `release` event chained to that latest event, binding the OLD
#      record digest, the reason, the signer and the UTC timestamp;
#   4. rewrite the record clearing ONLY the decision checkbox, Signed, Date and
#      Digest signed, byte-for-byte identical otherwise, so the existing
#      renderer accepts it as unsigned. The rewrite is RAW-BYTE PRESERVING
#      (round 7, finding 17): the file is read as bytes, split on its own line
#      terminator (LF or CRLF, detected, never normalised), the trailing
#      terminator is kept or omitted exactly as found, and before writing the
#      function asserts that the round trip of the unmodified lines reproduces
#      the original bytes and that exactly the four cleared lines differ.
# The gate is then blocked by construction: an unsigned record fails the content
# check, and the latest ledger event is non-approving.

# Lines the release clears, and the placeholder each returns to. The values are
# the templates/gate-signoff.md placeholders, so a released record is
# indistinguishable from a freshly rendered one.
.RELEASE_PLACEHOLDERS <- c("Signed:" = "Signed: <!-- SME name -->",
                           "Date:"   = "Date: <!-- -->",
                           "Digest signed:" = "Digest signed: <!-- paste -->")

# Returns the record's lines with exactly the four decision fields cleared.
# Pure, so the byte-for-byte-otherwise property is testable without side effects.
.clear_signature_lines <- function(lines) {
  # Same box grammar as the strict parser (tools/gate.R .GATE_RE): pandoc
  # normalises "- [x]" to "-   [x]" when a record is round-tripped, and the
  # parser and release must accept the same spacing.
  box <- grep("^\\s*-\\s*\\[[xX]\\]\\s", lines)
  if (length(box) != 1L)
    stop("release_gate: expected exactly one checked decision box, found ",
         length(box), ".", call. = FALSE)
  lines[box] <- sub("\\[[xX]\\]", "[ ]", lines[box])
  signed_hit <- grep(.GATE_RE$signed, lines)
  digest_hit <- grep(.GATE_RE$digest_signed, lines)
  if (length(signed_hit) != 1L)
    stop("release_gate: expected exactly one Signed: line, found ",
         length(signed_hit), ".", call. = FALSE)
  if (length(digest_hit) != 1L)
    stop("release_gate: expected exactly one Digest signed: line, found ",
         length(digest_hit), ".", call. = FALSE)
  signed_line <- lines[signed_hit]
  indent <- sub("^(\\s*).*", "\\1", signed_line)
  combined_date <- grepl("Date:", sub(.GATE_RE$signed, "", signed_line), fixed = TRUE)
  lines[signed_hit] <- paste0(indent, "Signed: <!-- SME name -->",
                              if (combined_date) " Date: <!-- -->" else "")
  date_hit <- grep(.GATE_RE$date, lines)
  if (!combined_date) {
    if (length(date_hit) != 1L)
      stop("release_gate: expected exactly one Date: line, found ",
           length(date_hit), ".", call. = FALSE)
    date_indent <- sub("^(\\s*).*", "\\1", lines[date_hit])
    lines[date_hit] <- paste0(date_indent, "Date: <!-- -->")
  }
  digest_indent <- sub("^(\\s*).*", "\\1", lines[digest_hit])
  lines[digest_hit] <- paste0(digest_indent, "Digest signed: <!-- paste -->")
  lines
}

# Splits raw file bytes into lines on the file's own terminator and rebuilds
# them, so that rebuild(split(x)) == x for any LF- or CRLF-terminated text,
# with or without a final terminator. Mixed terminators are refused: there is
# no single faithful reconstruction of such a file.
.split_record_bytes <- function(bytes, who = "release_gate") {
  # Work on the raw vector itself: no locale translation, no encoding marks.
  lf <- which(bytes == as.raw(0x0a))
  cr_before <- lf[lf > 1L & bytes[pmax(lf - 1L, 1L)] == as.raw(0x0d)]
  crlf <- length(cr_before) > 0L
  if (crlf && length(cr_before) != length(lf))
    stop(who, ": the record mixes CRLF and LF line endings; refusing a ",
         "rewrite that cannot preserve its bytes.", call. = FALSE)
  eol <- if (crlf) as.raw(c(0x0d, 0x0a)) else as.raw(0x0a)
  n <- length(bytes)
  trailing <- length(lf) > 0L && lf[length(lf)] == n
  # line i spans (prev_end + 1) .. (lf_i - length(eol)); a trailing terminator
  # closes the last line, otherwise the tail after the last LF is the last line.
  starts <- c(1L, lf + 1L)
  ends <- c(lf - length(eol), n)
  if (trailing) { starts <- starts[-length(starts)]; ends <- ends[-length(ends)] }
  lines <- vapply(seq_along(starts), function(i) {
    if (ends[i] < starts[i]) return("")
    rawToChar(bytes[starts[i]:ends[i]])
  }, character(1))
  Encoding(lines) <- "UTF-8"
  list(lines = lines, eol = eol, trailing = trailing)
}
.join_record_bytes <- function(parts, lines = parts$lines) {
  # charToRaw() returns a string's stored bytes without translation, so the
  # join is byte-exact whatever the session locale.
  out <- raw(0)
  for (i in seq_along(lines)) {
    out <- c(out, charToRaw(lines[i]))
    if (i < length(lines) || parts$trailing) out <- c(out, parts$eol)
  }
  out
}
# Returns the record bytes with exactly the four decision lines cleared, having
# asserted (a) that the split/join round trip reproduces the input bytes and
# (b) that only the semantic decision/signature fields differ. Signer and date
# may share one accepted line, so a valid rewrite changes three or four lines.
.rewrite_signature_bytes <- function(bytes) {
  parts <- .split_record_bytes(bytes)
  if (!identical(.join_record_bytes(parts), bytes))
    stop("release_gate: internal check failed — the line split does not round-trip ",
         "the record bytes; refusing to rewrite.", call. = FALSE)
  cleared <- .clear_signature_lines(parts$lines)
  changed <- which(cleared != parts$lines)
  sig <- .gate_signature_fields(cleared)
  permitted <- grepl("^\\s*-\\s*\\[ \\]\\s|^\\s*Signed:|^\\s*Date:|^\\s*Digest signed:",
                     cleared[changed])
  if (!length(changed) %in% c(3L, 4L) || !all(permitted) ||
      !identical(.gate_decision_of(cleared), "unsigned") ||
      !is.na(sig$signer) || !is.na(sig$date) || nzchar(sig$digest_signed))
    stop("release_gate: internal check failed — the rewrite would change ",
         length(changed), " line(s) outside the supported decision/signature fields; ",
         "refusing to rewrite.", call. = FALSE)
  .join_record_bytes(parts, cleared)
}

# --- release serialisation (integrity batch 11, R10-P1-1) --------------------
# Two releasers could both pass the tail comparison and then both append,
# producing two events sharing one predecessor. The window is narrow but it is
# real, and a forked chain is exactly the defect the chain exists to prevent.
# Releases against one ledger are therefore serialised by an exclusive lock
# held from the FIRST tail read through the append and both renames.
#
# The lock is a DIRECTORY, not a file. R's file.create() truncates an existing
# file and returns TRUE, so it has no O_EXCL semantics; `file(path, "xb")` is
# not supported by this R (verified 2026-09-04, R 4.3.3: "Invalid argument").
# dir.create() IS atomic create-or-fail on every platform R supports -- it
# returns TRUE exactly once and FALSE (with a warning) for every other caller.
# `filelock` is not installed in this environment, so it is not depended on.
GATE_RELEASE_LOCK_STALE_SECONDS <- 15 * 60

.release_lock_path <- function(ledger) paste0(ledger, ".lock")

# Acquires the lock or STOPS. A held lock is never broken automatically: a
# stale lock and a live concurrent writer look identical from outside, and
# guessing wrong forks the chain. The refusal names the lock, its age and its
# holder so the SME can decide. `who` is the writer taking the lock -- every
# ledger writer (approve_gate, revoke_gate, release_gate) takes the same one
# (integrity batch 12, R11-P1-1).
#
# Cleanup on a failed acquisition (batch 12, R11-P2-2). dir.create() and the
# holder write are two steps, and a failure of the second used to leave the
# directory behind with the caller holding no path to clean up: every later
# writer was refused until someone removed the lock by hand. The cleanup below
# is armed the moment the directory exists and disarmed only once the holder
# record is written.
#
# The cleanup is VERIFIED (integrity batch 13, Codex round 12, R12-P2-2). The
# armed on.exit used to call .release_lock_remove() and ignore its answer, so
# when the directory could not be removed -- a parent made read-only between
# the create and the unlink, an unlink failure -- the error still said "the
# directory has been removed again" while a stale lock stood in every later
# writer's way. The message now reports what happened: removal confirmed, or
# the stale lock named with the manual step. `.on_created` is a TEST-ONLY
# injection point run once the directory exists, before the holder write, so
# the battery can make both steps fail; production callers never pass it.
.release_lock_acquire <- function(ledger, who = "release_gate", .on_created = NULL) {
  lock <- .release_lock_path(ledger)
  if (suppressWarnings(dir.create(lock, showWarnings = FALSE))) {
    armed <- TRUE
    cleanup <- list(done = NA)
    on.exit(if (armed) cleanup$done <- .release_lock_remove(lock), add = TRUE)
    if (is.function(.on_created)) .on_created(lock)
    holder <- file.path(lock, "holder")
    written <- tryCatch({
      writeLines(c(paste0("pid: ", Sys.getpid()),
                   paste0("host: ", Sys.info()[["nodename"]]),
                   paste0("writer: ", who),
                   paste0("acquired_utc: ",
                          format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))),
                 holder)
      TRUE
    }, error = function(e) conditionMessage(e), warning = function(w) conditionMessage(w))
    if (!isTRUE(written) || !file.exists(holder)) {
      # Run the cleanup HERE, verified, so the message can say what happened;
      # the on.exit cleanup is disarmed because this call did the work.
      armed <- FALSE
      removed <- .release_lock_remove(lock)
      reason <- if (isTRUE(written)) "the file does not exist after the write" else written
      if (isTRUE(removed))
        stop(who, ": the lock directory ", lock, " was created but its holder ",
             "record ", holder, " could not be written (", reason,
             "). The directory has been removed again so no stale lock is left ",
             "behind. Nothing was changed. ESCALATE to the SME.", call. = FALSE)
      stop(who, ": the lock directory ", lock, " was created but its holder ",
           "record ", holder, " could not be written (", reason,
           "), AND the directory could not be removed afterwards: a stale lock ",
           "with no holder record now stands at ", lock, " and every later ",
           "approve, revoke or release on ", ledger, " will be refused until it ",
           "is removed by hand (confirm no ledger writer is running, then ",
           "`rm -r '", lock, "'`). Nothing was changed in the ledger. ESCALATE ",
           "to the SME.", call. = FALSE)
    }
    armed <- FALSE
    return(lock)
  }
  holder <- if (file.exists(file.path(lock, "holder")))
    paste(readLines(file.path(lock, "holder"), warn = FALSE), collapse = "; ")
    else "<no holder record>"
  age <- suppressWarnings(as.numeric(
    difftime(Sys.time(), file.info(lock)$mtime, units = "secs")))
  stale <- is.finite(age) && age > GATE_RELEASE_LOCK_STALE_SECONDS
  stop(who, ": another ledger writer is holding the lock on ", ledger,
       " (lock: ", lock, "; holder: ", holder, "; age: ",
       if (is.finite(age)) sprintf("%.0f s", age) else "unknown", "). ",
       "Nothing was changed. ",
       if (stale)
         sprintf(paste0("This lock is older than the %d s staleness threshold ",
                        "and MAY be left over from a crashed writer. It is ",
                        "not broken automatically: a stale lock and a live ",
                        "concurrent writer are indistinguishable from here, ",
                        "and guessing wrong forks the approval chain. Confirm ",
                        "no approve, revoke or release is running, then ",
                        "remove the lock directory by hand. "),
                 GATE_RELEASE_LOCK_STALE_SECONDS)
       else "Wait for it to finish and retry. ",
       "ESCALATE to the SME.", call. = FALSE)
}

# Removes the lock directory, retrying once with the directory made writable
# (a holder write can fail because the directory itself is not writable, in
# which case its contents cannot be listed for the recursive unlink either).
.release_lock_remove <- function(lock) {
  unlink(lock, recursive = TRUE, force = TRUE)
  if (dir.exists(lock)) {
    suppressWarnings(Sys.chmod(lock, "700"))
    unlink(lock, recursive = TRUE, force = TRUE)
  }
  invisible(!dir.exists(lock))
}

# Releases the lock and VERIFIES the removal (batch 12, R11-P2-2). A silent
# unlink that left the directory in place would refuse every later writer with
# a message naming a holder that no longer exists.
.release_lock_free <- function(lock) {
  if (is.null(lock) || !nzchar(lock) || !dir.exists(lock)) return(invisible(NULL))
  .release_lock_remove(lock)
  if (dir.exists(lock))
    stop("the ledger lock directory ", lock, " still exists after release: it ",
         "could not be removed. Every later approve, revoke or release on this ",
         "ledger will be refused until it is removed by hand. ESCALATE to the ",
         "SME.", call. = FALSE)
  invisible(NULL)
}

# THE RESIDUAL WINDOW, named. Quoted verbatim into the refusal below, into
# docs/COPILOT-RUNBOOK.md and into the adapter's gate contract so the three
# cannot drift apart.
.release_named_residual_window <- function()
  paste0("A release is FAIL-LOUD, RECOVERABLE MULTI-STEP, not all-or-none. ",
         "The residual window is exactly this: between the ledger append and ",
         "the two renames that follow it, the ledger records the release ",
         "while the gate record on disk may still carry its signature and the ",
         "archive may not yet be in place. The append cannot be undone -- the ",
         "ledger is append-only, which is what makes the chain auditable -- so ",
         "the staged bytes are LEFT ON DISK, named, and the operator completes ",
         "or revokes the release by hand. Every release is serialised by an ",
         "exclusive lock, so the window is never entered by two releasers at ",
         "once. A release may also overwrite a concurrent editor save of the record: editing is unlocked, so a save made between the release's last comparison and its rename of the cleared record is replaced, and the ledger and archive stay consistent while that save is lost.")

release_gate <- function(gate_file, reason, signer, .after_snapshot = NULL) {
  if (missing(reason) || !.gate_nonempty_string(reason))
    stop("release_gate requires a non-empty --reason: the ledger must say why ",
         "the signed version was released.", call. = FALSE)
  if (missing(signer) || !.gate_nonempty_string(signer))
    stop("release_gate requires a --signer: a release is an SME act and the ",
         "ledger records who performed it.", call. = FALSE)
  if (.gate_reserved_signer(signer))
    stop("release_gate: --signer uses a reserved role label; enter the local ",
         "reviewer's name.", call. = FALSE)
  if (!file.exists(gate_file))
    stop("release_gate: no such gate record: ", gate_file, call. = FALSE)

  # (1) The record must currently BE an approval, and its content must pass the
  # same checks approve_gate applies before recording (ledger enforcement off:
  # this call is itself the ledger-extending step) EXCEPT the live-artifact
  # rehash: artifact drift is precisely why a signature gets released (the SME
  # undoes it so the package can be re-rendered, and render_gate_from_store
  # refuses an APPROVED record), and step (2) archives the exact released bytes
  # anyway. Parse, decision, digest binding and the ledger chain still hold.
  # VALIDATED FROM THE SNAPSHOT, NOT FROM AN INDEPENDENT RE-READ (integrity
  # batch 16, Codex round 15 R15-P3-1). This call used to run HERE, before the
  # lock and before the snapshot below, so release validated one read of the
  # live path and then archived, digested and cleared a different one. It now
  # runs after the lock and the single snapshot, against `snap$lines` /
  # `snap$bytes` -- the same bytes the event, the archive name and the archived
  # content are built from. See below.
  # THE LEDGER LOCK AND ONE SNAPSHOT, BOTH BEFORE THE FIRST READ THE EVENT WILL
  # DEPEND ON (SME decision D14-1, 2026-09-05; Codex round 13 R13-P1-2). The
  # release used to parse the record, compute `old_digest` from a second read,
  # read the archive bytes from a third, and take the lock only after all
  # three: a save in between could place different bytes under the old
  # digest's archive filename. The lock is now acquired first and the record is
  # read ONCE; the parse, the digest, the archive name and the archived bytes
  # all come from that single byte vector, and the live file is compared with
  # it again immediately before the append.
  ledger <- .ledger_path(gate_file)
  .release_lock <- .release_lock_acquire(ledger)
  on.exit(.release_lock_free(.release_lock), add = TRUE)
  snap <- .gate_snapshot(gate_file)
  # The content validation of step (1), now on the snapshot. Ledger enforcement
  # is off (this call is itself the ledger-extending step) and the live-artifact
  # rehash is off (artifact drift is precisely why a signature gets released,
  # and step (2) archives the exact released bytes anyway). Parse, decision and
  # digest binding still hold, and they hold over the bytes that get archived.
  suppressMessages(require_gate(gate_file, enforce_ledger = FALSE,
                                verify_artifacts = FALSE,
                                lines = snap$lines, bytes = snap$bytes))
  parsed <- parse_gate_record(gate_file, lines = snap$lines)
  if (is.function(.after_snapshot)) .after_snapshot()
  if (!parsed$decision %in% .LEDGER_APPROVING)
    stop("release_gate: the record is not currently APPROVED (decision '",
         parsed$decision, "'); there is no signature to release.", call. = FALSE)

  task <- .gate_task_key(gate_file)
  events <- .read_ledger(.ledger_path(gate_file))
  task_events <- Filter(function(e) identical(as.character(e$task), task), events)
  chain_problems <- .verify_task_chain(task_events)
  if (length(chain_problems))
    stop("release_gate: existing ledger chain for '", task, "' is broken:\n  - ",
         paste(chain_problems, collapse = "\n  - "), call. = FALSE)
  latest <- .ledger_latest_for_task(events, task)
  if (is.null(latest))
    stop("release_gate: '", task, "' has no ledger event; there is nothing to ",
         "release. Record the approval first.", call. = FALSE)

  old_digest <- snap$digest
  if (!identical(as.character(latest$record_digest), old_digest))
    stop("release_gate: the record digest (", old_digest, ") does not match the ",
         "latest ledger event's record_digest (", latest$record_digest,
         "). The record changed after it was ledgered; resolve that before ",
         "releasing.", call. = FALSE)
  if (!as.character(latest$decision) %in% .LEDGER_APPROVING)
    stop("release_gate: the latest ledger event for '", task, "' is '",
         as.character(latest$decision), "', not an approval; nothing to release.",
         call. = FALSE)
  if (!identical(as.character(latest$package_digest),
                 parsed$package_digest %||% NA_character_))
    stop("release_gate: the record's Package-Digest (", parsed$package_digest,
         ") does not match the latest ledger event's package_digest (",
         latest$package_digest, "). Fail closed.", call. = FALSE)

  # (2) Archive the exact released bytes, content-addressed. Never overwritten:
  # a name collision means identical bytes (the digest is the name) or a
  # corrupted archive, and neither is ours to resolve silently.
  hist_dir <- file.path(dirname(gate_file), "history")
  archive <- file.path(hist_dir, sprintf("%s.%s.md", task, sub("^sha256:", "", old_digest)))
  bytes <- snap$bytes
  # The cleared record is computed and asserted BEFORE any side effect. On
  # 2026-09-04 the rewrite failed on a record whose decision box the strict
  # parser accepted but this rewrite did not, after the archive had been
  # written and the release event appended: the ledger said "released" while
  # the record still carried the signature.
  #
  # RELEASE SEMANTICS: FAIL-LOUD, RECOVERABLE MULTI-STEP (SME decision 1,
  # 2026-09-04; round-10 finding P1-1). This is deliberately NOT all-or-none,
  # and the wording used to claim otherwise. The ledger is append-only -- that
  # is what makes the chain auditable -- so the release event cannot be
  # withdrawn once appended, and the two renames that follow it can fail
  # independently. See .release_named_residual_window() below for the residual
  # window, stated exactly.
  new_bytes <- .rewrite_signature_bytes(bytes)
  if (!dir.exists(hist_dir)) dir.create(hist_dir, recursive = TRUE)

  # Round 9, P2-8, revised in the fix round after review S1/S2.
  #
  # The first attempt staged all three writes -- archive, record AND ledger --
  # and renamed the ledger into place from a full in-memory copy. That silently
  # destroyed any line another process appended between the read and the
  # rename, and forked the chain besides, because previous_event_id had been
  # computed from the pre-append state. The ledger is append-only for exactly
  # this reason, so it keeps its O_APPEND write (.append_event) and is never
  # rewritten. Only the archive and the record -- both whole-file replacements
  # -- are staged.
  #
  # Order: stage archive and record; re-read the staged bytes and require them
  # to equal what was written; re-read the ledger and require it to be
  # unchanged since previous_event_id was computed; append the event
  # (O_APPEND); rename archive and record into place.
  #
  # Before the append, a failure leaves nothing behind. After the append, a
  # failed rename leaves the staged files ON DISK, names them, and escalates:
  # the ledger already records the release, so deleting the prepared bytes
  # would destroy the evidence the SME needs to finish by hand.
  # THE LOCK IS TAKEN EARLIER (integrity batch 14): at the top of this
  # function, before the record snapshot and therefore before anything is
  # staged and before the ledger is read for the tail comparison. It is held
  # through the append and both renames, and released on every exit path,
  # including the fail-after-append one: the staged bytes survive, the lock
  # does not, so the operator finishing by hand is not blocked by a lock
  # nobody holds.

  staged <- character(0)
  keep_staged <- FALSE
  on.exit(if (!keep_staged) unlink(staged[file.exists(staged)]), add = TRUE)
  stage <- function(dest, raw) {
    tmp <- paste0(dest, ".release-staging.", Sys.getpid(), ".tmp")
    if (file.exists(tmp)) unlink(tmp)
    writeBin(raw, tmp)
    staged <<- c(staged, tmp)
    back <- readBin(tmp, "raw", file.info(tmp)$size)
    if (!identical(back, raw))
      stop("release_gate: the bytes staged at ", tmp, " do not read back ",
           "equal to what was written; the filesystem did not store them ",
           "faithfully. Nothing was changed.", call. = FALSE)
    tmp
  }

  archive_needed <- !file.exists(archive)
  if (!archive_needed) {
    if (!identical(readBin(archive, "raw", file.info(archive)$size), bytes))
      stop("release_gate: ", archive, " exists with different bytes; refusing to ",
           "overwrite an archived record.", call. = FALSE)
  }
  archive_tmp <- if (archive_needed) stage(archive, bytes) else NA_character_
  record_tmp  <- stage(gate_file, new_bytes)

  # (3) The release event, chained to the approval it supersedes. The ledger's
  # last line is re-read here and compared with the state previous_event_id was
  # derived from; a ledger that moved under us is a refusal, not a merge.
  .ledger_tail <- function() {
    if (!file.exists(ledger)) return(character(0))
    ln <- readLines(ledger, warn = FALSE)
    if (!length(ln)) character(0) else ln[length(ln)]
  }
  tail_at_read <- .ledger_tail()
  events_now <- .read_ledger(ledger)
  latest_now <- .ledger_latest_for_task(events_now, task)
  if (is.null(latest_now) ||
      !identical(as.character(latest_now$event_id), as.character(latest$event_id)))
    stop("release_gate: the approval ledger for '", task, "' changed while the ",
         "release was being prepared (the latest event for this task is no ",
         "longer the one this release supersedes). Nothing was changed. ",
         "ESCALATE to the SME.", call. = FALSE)

  ev <- .build_event(gate_file, "release", reason = reason, parsed = parsed,
                     record_digest = snap$digest)
  ev$signer <- signer                 # the releasing SME, not the original signer
  ev$signed_at <- NA_character_       # a release is not a signature
  ev$previous_event_id <- as.character(latest_now$event_id)

  # Re-read immediately before the append: anything appended since the check
  # above would make this event's predecessor link stale.
  if (!identical(.ledger_tail(), tail_at_read))
    stop("release_gate: the approval ledger ", ledger, " gained a line while ",
         "this release was being staged; the release event would fork the ",
         "chain. Nothing was changed. ESCALATE to the SME.", call. = FALSE)

  # (4) Append (O_APPEND, never a rewrite), then commit the two staged files.
  .gate_snapshot_assert("release_gate", gate_file, snap)
  rec <- .append_event(gate_file, ev)

  fail_after_append <- function(what, dest) {
    keep_staged <<- TRUE
    stop("release_gate: the release event IS recorded in ", ledger, " (event_id ",
         rec$event_id, "), but the staged ", what, " could not be moved into ",
         "place at ", dest, ". The prepared bytes are LEFT ON DISK at: ",
         paste(staged[file.exists(staged)], collapse = ", "),
         ". Nothing has been deleted. Complete the move by hand and verify, or ",
         "revoke the release. ", .release_named_residual_window(),
         " ESCALATE to the SME.", call. = FALSE)
  }
  if (archive_needed && !file.rename(archive_tmp, archive))
    fail_after_append("archive", archive)
  if (!file.rename(record_tmp, gate_file))
    fail_after_append("cleared record", gate_file)
  staged <- character(0)

  message("Released ", gate_file, " (event_id ", rec$event_id,
          "; released bytes archived at ", archive, ")")
  invisible(rec)
}

# Full-ledger verification for an analysis (or a single approvals.jsonl path):
# recompute every event_id and check each task's chain. Returns
# list(ok, problems).
verify_approvals_ledger <- function(target = "gates") {
  paths <- if (file.exists(target) && grepl("approvals\\.jsonl$", target)) target
           else if (dir.exists(file.path("gates", target)))
             list.files(file.path("gates", target), pattern = "approvals\\.jsonl$",
                        full.names = TRUE, recursive = TRUE)
           else if (dir.exists(target))
             list.files(target, pattern = "approvals\\.jsonl$", full.names = TRUE,
                        recursive = TRUE)
           else character(0)
  problems <- character(0)
  event_count <- 0L
  for (p in paths) {
    events <- .read_ledger(p)
    event_count <- event_count + length(events)
    expected_analysis <- basename(dirname(p))
    pr <- .verify_approval_events(events, expected_analysis = expected_analysis)
    if (length(pr)) problems <- c(problems, sprintf("%s: %s", p, pr))
  }
  list(ok = length(problems) == 0L, problems = problems,
       ledger_count = as.integer(length(paths)), event_count = as.integer(event_count),
       empty = length(paths) == 0L)
}

# Append-only invariant: the committed base bytes of a ledger must be an EXACT
# PREFIX of the working-tree bytes (only appends, never edits/removals of past
# events). Pure function so CI (which reads the base via `git show`) and the
# test battery share it. Empty base (new file) is trivially a prefix.
ledger_append_only_ok <- function(base_text, work_text) {
  if (is.null(base_text) || length(base_text) == 0) return(TRUE)
  base_text <- paste0(paste(base_text, collapse = "\n"), "\n")
  work_text <- paste0(paste(work_text, collapse = "\n"), "\n")
  startsWith(work_text, base_text)
}

# --- CLI --------------------------------------------------------------------
.approve_invoked_as_script <- function() {
  fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  length(fa) > 0L && identical(basename(sub("^--file=", "", fa[1])),
                               "approve_gate.R")
}

if (.approve_invoked_as_script()) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0) {
    cat("usage: approve_gate.R approve|revoke|release|verify ...\n"); quit(status = 2L)
  }
  cmd <- args[1]
  if (cmd == "approve") {
    approve_gate(args[2]); quit(status = 0L)
  } else if (cmd == "revoke") {
    revoke_gate(args[2], args[3]); quit(status = 0L)
  } else if (cmd == "release") {
    # release <ID> <task> --reason "<text>" --signer <initials>
    # STRICT flag parsing (round 7, finding 17): exactly two positionals, then
    # flag/value pairs drawn from a closed set; an unknown flag, a duplicated
    # flag, a flag without a value (or whose value is itself a flag), or a
    # stray positional refuses with the usage status before anything runs.
    usage <- function(why) {
      cat("approve_gate.R release: ", why, "\n",
          'usage: approve_gate.R release <ID> <task> --reason "<text>" --signer <initials>\n',
          sep = "")
      quit(status = 2L)
    }
    if (length(args) < 3L || startsWith(args[2], "--") || startsWith(args[3], "--"))
      usage("<ID> and <task> are required positionals")
    allowed <- c("--reason", "--signer")
    flags <- list(); rest <- args[-(1:3)]
    while (length(rest)) {
      f <- rest[1]
      if (!startsWith(f, "--")) usage(paste0("unexpected argument '", f, "'"))
      if (!f %in% allowed) usage(paste0("unknown flag '", f, "'"))
      if (!is.null(flags[[f]])) usage(paste0("duplicate flag '", f, "'"))
      if (length(rest) < 2L || startsWith(rest[2], "--"))
        usage(paste0("flag '", f, "' requires a value"))
      flags[[f]] <- rest[2]; rest <- rest[-(1:2)]
    }
    for (f in allowed) if (is.null(flags[[f]])) usage(paste0("missing required flag '", f, "'"))
    # <task> is the pipeline task name; compose.R names the record with its
    # underscores dashed (data_assembly -> data-assembly.md), so accept either
    # form here.
    gf <- file.path("gates", args[2],
                    paste0(gsub("_", "-", sub("\\.md$", "", args[3])), ".md"))
    res <- tryCatch(release_gate(gf, reason = flags[["--reason"]], signer = flags[["--signer"]]),
                    error = function(e) e)
    if (inherits(res, "error")) { cat(conditionMessage(res), "\n", sep = ""); quit(status = 1L) }
    quit(status = 0L)
  } else if (cmd == "verify") {
    tgt <- if (length(args) >= 2) args[2] else "gates"
    if (length(args) >= 2 && !file.exists(tgt) && !file.exists(file.path("gates", tgt))) {
      cat("no such target: ", tgt, "\n", sep = "")
      quit(status = 2L)
    }
    res <- verify_approvals_ledger(tgt)
    inventory <- sprintf("%d ledger file(s), %d event(s) checked",
                         res$ledger_count, res$event_count)
    if (res$ok && isTRUE(res$empty)) {
      cat("approvals ledger EMPTY: ", inventory,
          "; no approval history was represented as verified\n", sep = "")
      quit(status = 0L)
    }
    if (res$ok) { cat("approvals ledger OK: ", inventory, "\n", sep = ""); quit(status = 0L) }
    cat("approvals ledger PROBLEMS:\n"); cat(paste0("  - ", res$problems), sep = "\n")
    cat("\n"); quit(status = 1L)
  } else {
    cat("unknown command: ", cmd, "\n", sep = ""); quit(status = 2L)
  }
}
