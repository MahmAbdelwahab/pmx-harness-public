# Signed errata ledger (integrity batch 4, item E).
#
# A signed gate record's bytes are never edited — the signature stays
# verifiable. The ledger records corrections to claims in signed records.
# gates/<ID>/errata.jsonl is the append-only, hash-chained overlay that makes
# such a correction VISIBLE to every reader of the gate: require_gate consults
# it before the legacy short-circuit and either emits a KNOWN SUPERSEDED notice
# (minor) or stops (material) until the SME re-signs with knowledge.
#
# The ledger primitives (JSON writer/scanner, canonical serialization, chain
# verification, append-only prefix rule) are SHARED with the approvals ledger
# (tools/gate.R, tools/approve_gate.R) — nothing here forks them.
#
# The tool records ONLY what the SME states: signer and date are arguments the
# SME supplies, never inferred from git, the environment, or the record.
#
# Run:
#   Rscript tools/errata.R record <gate_file> --claim "<exact claim text>" \
#       --correction "<what is true>" --evidence "<register row id or path>" \
#       --materiality minor|material --signer "<name>" --date "<date>"
#   Rscript tools/errata.R verify [<analysis-id> | <errata.jsonl>]   # all if omitted

.errata_tools_dir <- function() {
  fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(fa)) return(normalizePath(dirname(sub("^--file=", "", fa[1])), mustWork = FALSE))
  normalizePath("tools", mustWork = FALSE)
}

local({
  td <- .errata_tools_dir()
  if (!exists("parse_gate_record", inherits = TRUE)) source(file.path(td, "gate.R"))
  approval_helpers <- c("ledger_append_only_ok", ".release_lock_acquire",
                        ".release_lock_free", ".gate_snapshot",
                        ".gate_snapshot_assert")
  if (any(!vapply(approval_helpers, exists, logical(1), inherits = TRUE)))
    source(file.path(td, "approve_gate.R"))
})

# Event schema (field order is the canonical order for event_id):
#   schema_version, gate_path, record_digest, claim_sha256, claim_excerpt,
#   correction, evidence, materiality, signer, date, recorded_at_utc,
#   previous_event_id; event_id = sha256 over the canonical serialization of
#   every other field (.ERRATA_FIELDS in tools/gate.R).
.errata_to_json <- function(ev) {
  q <- function(v) if (is.null(v) || (length(v) == 1 && is.na(v))) "null"
                   else paste0('"', .json_escape(as.character(v)), '"')
  order <- c("schema_version", "event_id", .ERRATA_FIELDS[.ERRATA_FIELDS != "schema_version"])
  pairs <- vapply(order, function(f) {
    if (f == "schema_version") paste0('"schema_version":', as.integer(ev$schema_version))
    else paste0('"', f, '":', q(ev[[f]]))
  }, character(1))
  paste0("{", paste(pairs, collapse = ","), "}")
}

.claim_locator <- function(claim) {
  claim <- as.character(claim)
  excerpt <- if (nchar(claim) > 80) paste0(substr(claim, 1, 77), "...") else claim
  list(sha256 = paste0("sha256:", digest::digest(claim, algo = "sha256", serialize = FALSE)),
       excerpt = excerpt)
}

.nonempty_arg <- function(x, name) {
  if (is.null(x) || length(x) != 1 || is.na(x) || !nzchar(trimws(x)))
    stop("record_erratum: '", name, "' must be a non-empty string supplied by the SME.",
         call. = FALSE)
  trimws(x)
}

# Record one erratum against a SIGNED gate record. The record must currently be
# approved (an erratum against an unsigned draft is a fix, not an erratum); the
# event binds the record's current digest so a later re-version is visibly a
# different record. Returns the event invisibly.
record_erratum <- function(gate_file, claim, correction, evidence, materiality,
                           signer, date, .after_lock = NULL) {
  if (!file.exists(gate_file)) stop("gate record not found: ", gate_file, call. = FALSE)
  claim      <- .nonempty_arg(claim, "claim")
  correction <- .nonempty_arg(correction, "correction")
  evidence   <- .nonempty_arg(evidence, "evidence")
  signer     <- .nonempty_arg(signer, "signer")
  if (.gate_reserved_signer(signer))
    stop("record_erratum: signer uses a reserved role label; enter the local ",
         "reviewer's name.", call. = FALSE)
  date       <- .nonempty_arg(date, "date")
  materiality <- .nonempty_arg(materiality, "materiality")
  if (!materiality %in% .ERRATA_MATERIALITY)
    stop("record_erratum: materiality must be one of ",
         paste(.ERRATA_MATERIALITY, collapse = " | "), ", got '", materiality, "'.",
         call. = FALSE)
  path <- .errata_path(gate_file)
  ledger_lock <- .release_lock_acquire(path, who = "record_erratum")
  on.exit(.release_lock_free(ledger_lock), add = TRUE)
  if (is.function(.after_lock)) .after_lock()

  # One record snapshot under the ledger lock binds the checked approval state
  # and event digest to the same bytes. A concurrent editor is detected before
  # append by .gate_snapshot_assert().
  snap <- .gate_snapshot(gate_file)
  parsed <- parse_gate_record(gate_file, lines = snap$lines)
  if (!parsed$decision %in% .LEDGER_APPROVING)
    stop("record_erratum: ", gate_file, " is not an approved record (decision '",
         parsed$decision, "'); errata apply to signed records only.", call. = FALSE)

  all_events <- .read_ledger(path)
  chain_problems <- .verify_errata_events(
    all_events, expected_analysis = basename(dirname(normalizePath(gate_file,
                                                                  mustWork = FALSE))))
  if (length(chain_problems))
    stop("record_erratum: existing errata ledger is broken:\n  - ",
         paste(chain_problems, collapse = "\n  - "), call. = FALSE)
  events <- .errata_for_gate(gate_file, events = all_events)
  loc <- .claim_locator(claim)
  ev <- list(
    schema_version = 1L,
    gate_path = .gate_relpath(gate_file),
    record_digest = snap$digest,
    claim_sha256 = loc$sha256,
    claim_excerpt = loc$excerpt,
    correction = correction,
    evidence = evidence,
    materiality = materiality,
    signer = signer,
    date = date,
    recorded_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    previous_event_id = if (length(events))
      as.character(events[[length(events)]]$event_id) else NA_character_)
  ev$event_id <- .ledger_event_id(ev, .ERRATA_FIELDS)
  .gate_snapshot_assert("record_erratum", gate_file, snap)
  cat(.errata_to_json(ev), "\n", sep = "", file = path, append = TRUE)
  message("Recorded ", materiality, " erratum for ", gate_file, " (event_id ", ev$event_id, ")")
  invisible(ev)
}

# Verify every errata ledger under a target (an analysis id, a gates/<ID>
# directory, or one errata.jsonl): per-gate chains + required-field completeness
# + a known materiality. Returns list(ok, problems).
verify_errata_ledger <- function(target = "gates") {
  paths <- if (file.exists(target) && grepl("errata\\.jsonl$", target)) target
           else if (dir.exists(file.path("gates", target)))
             list.files(file.path("gates", target), pattern = "errata\\.jsonl$",
                        full.names = TRUE)
           else if (dir.exists(target))
             list.files(target, pattern = "errata\\.jsonl$", full.names = TRUE,
                        recursive = TRUE)
           else character(0)
  problems <- character(0)
  event_count <- 0L
  for (p in paths) {
    events <- .read_ledger(p)
    event_count <- event_count + length(events)
    pr <- .verify_errata_events(events, expected_analysis = basename(dirname(p)))
    if (length(pr)) problems <- c(problems, sprintf("%s: %s", p, pr))
  }
  list(ok = length(problems) == 0, problems = problems,
       ledger_count = as.integer(length(paths)), event_count = as.integer(event_count),
       empty = length(paths) == 0L)
}

# --- CLI --------------------------------------------------------------------
.errata_invoked_as_script <- function() {
  fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  length(fa) > 0 && grepl("errata\\.R$", sub("^--file=", "", fa[1]))
}

.errata_parse_flags <- function(args) {
  out <- list()
  i <- 1L
  while (i <= length(args)) {
    a <- args[i]
    if (startsWith(a, "--")) {
      key <- sub("^--", "", a)
      if (!key %in% c("claim", "correction", "evidence", "materiality", "signer", "date"))
        stop("unknown flag --", key, call. = FALSE)
      if (i + 1L > length(args)) stop("missing value for --", key, call. = FALSE)
      out[[key]] <- args[i + 1L]
      i <- i + 2L
    } else {
      out[["positional"]] <- c(out[["positional"]], a)
      i <- i + 1L
    }
  }
  out
}

if (.errata_invoked_as_script()) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0) {
    cat("usage: errata.R record <gate> --claim .. --correction .. --evidence ..",
        "--materiality minor|material --signer .. --date ..\n",
        "       errata.R verify [<analysis-id> | <errata.jsonl>]\n")
    quit(status = 2L)
  }
  cmd <- args[1]
  if (cmd == "record") {
    fl <- .errata_parse_flags(args[-1])
    gate <- fl[["positional"]][1]
    if (is.null(gate)) { cat("record: a gate file is required\n"); quit(status = 2L) }
    record_erratum(gate, claim = fl[["claim"]], correction = fl[["correction"]],
                   evidence = fl[["evidence"]], materiality = fl[["materiality"]],
                   signer = fl[["signer"]], date = fl[["date"]])
    quit(status = 0L)
  } else if (cmd == "verify") {
    tgt <- if (length(args) >= 2) args[2] else "gates"
    if (length(args) >= 2 && !file.exists(tgt) && !file.exists(file.path("gates", tgt))) {
      cat("no such target: ", tgt, "\n", sep = "")
      quit(status = 2L)
    }
    res <- verify_errata_ledger(tgt)
    if (length(args) >= 2 && res$ok && isTRUE(res$empty)) {
      cat("errata ledger EMPTY: 0 ledger files under ", tgt, "\n", sep = "")
      quit(status = 0L)
    }
    if (res$ok) { cat("errata ledger OK\n"); quit(status = 0L) }
    cat("errata ledger PROBLEMS:\n"); cat(paste0("  - ", res$problems), sep = "\n")
    cat("\n"); quit(status = 1L)
  } else {
    cat("unknown command: ", cmd, "\n", sep = ""); quit(status = 2L)
  }
}
