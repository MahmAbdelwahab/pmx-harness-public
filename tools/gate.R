# Gate enforcement (Layer 2, runtime-independent).
#
# A gate record is created from templates/gate-signoff.md. A stage's first
# target declares the upstream gate file as a dependency and calls
# require_gate() on it: unsigned gate -> hard stop, so no downstream target
# can run regardless of which runtime (Copilot, Claude Code, bare R) drives
# the pipeline.

# --- legacy allowlist (integrity batch 3a, item 1) --------------------------
# tools/legacy_gates.yaml freezes the exact (path, sha256) pairs permitted to
# take the digest-less legacy path, plus the one canon1 digest record. Parsed
# with base R (no yaml dependency) so require_gate stays sourceable in a
# dependency-light pipeline, the same discipline as read_method_manifest().
# Returns a list of entries, each list(kind, path, sha256).
#
# The parser is STRICT and fails closed (Codex round 6, finding 7). It stops —
# it does not warn and skip — on a duplicate path, a missing kind/path/sha256
# field, a kind outside {digestless, canon1}, or a sha256 that does not match
# the full "sha256:<64 lowercase hex>" grammar. Rationale: runtime lookup
# (.legacy_entry_for) returns the FIRST entry matching a path, so a second
# entry with the same kind+path but an attacker-chosen hash would silently take
# precedence, while a set-difference pin over (kind, path) would see no new
# pair and stay green. A malformed or partial entry is equally inadmissible in
# a trust anchor: there is no safe default for a frozen hash.
.LEGACY_GATE_KINDS <- c("digestless", "canon1")

.read_legacy_gates <- function(path = file.path("tools", "legacy_gates.yaml")) {
  if (!file.exists(path)) return(list())
  txt <- readLines(path, warn = FALSE)
  entries <- list()
  cur <- NULL
  n_started <- 0L
  bad <- function(msg) stop(sprintf("legacy gate allowlist %s: %s", path, msg),
                            call. = FALSE)
  flush <- function() {
    if (is.null(cur)) return(invisible(NULL))
    where <- sprintf("entry %d", n_started)
    for (f in c("kind", "path", "sha256")) {
      if (is.null(cur[[f]]) || !nzchar(cur[[f]]))
        bad(sprintf("%s has a missing or empty '%s' field", where, f))
    }
    if (!cur$kind %in% .LEGACY_GATE_KINDS)
      bad(sprintf("%s has kind '%s'; allowed kinds are %s", where, cur$kind,
                  paste(.LEGACY_GATE_KINDS, collapse = ", ")))
    if (!grepl("^sha256:[0-9a-f]{64}$", cur$sha256))
      bad(sprintf("%s (%s) has a malformed sha256 value '%s'; expected sha256:<64 lowercase hex>",
                  where, cur$path, cur$sha256))
    for (e in entries) {
      if (identical(e$path, cur$path))
        bad(sprintf("duplicate path '%s'; each record may appear at most once",
                    cur$path))
    }
    entries[[length(entries) + 1L]] <<- cur
    invisible(NULL)
  }
  val <- function(ln, key) trimws(sub(sprintf("^\\s*%s:\\s*", key), "", ln))
  for (ln in txt) {
    if (grepl("^\\s*-\\s*kind:", ln)) {
      flush()
      n_started <- n_started + 1L
      cur <- list(kind = trimws(sub("^\\s*-\\s*kind:\\s*", "", ln)))
    } else if (!is.null(cur) && grepl("^\\s*path:", ln)) {
      cur$path <- val(ln, "path")
    } else if (!is.null(cur) && grepl("^\\s*sha256:", ln)) {
      cur$sha256 <- val(ln, "sha256")
    }
  }
  flush()
  entries
}

# Repo-relative POSIX path of a gate file, so it can be matched against the
# allowlist keys (which are repo-relative). Falls back to the given path when
# it does not resolve under getwd().
.gate_relpath <- function(path) {
  root <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  real <- normalizePath(path, winslash = "/", mustWork = FALSE)
  pref <- paste0(root, "/")
  if (startsWith(real, pref)) substring(real, nchar(pref) + 1L) else path
}

.gate_sha256 <- function(path) paste0("sha256:", digest::digest(file = path, algo = "sha256"))

# sha256 of one artifact table cell, prefixed "sha256:", or the literal
# "MISSING". This is THE artifact digest: render_gate_package writes it into
# the table (gate_package.R::.gate_artifact_sha256 delegates here) and
# require_gate recomputes it from disk, so the two can never diverge.
#
# A cell names one or more paths separated by "; " (the convention the task
# modules use for a set of related files). Each path is a regular file or a
# directory; a directory stands for all regular files under it, recursively,
# in sorted order. A cell that expands to exactly one file digests that
# file's bytes. A cell that expands to several files digests the text
# "<path>\t<sha256>\n" per file in expansion order, so one drifted, added,
# or removed member changes the cell. Any absent path, unreadable file, or
# empty directory makes the whole cell MISSING: a visible defect the SME can
# see and refuse. Verification refuses an incomplete artifact.
#
# EVERY MEMBER MUST BE A REGULAR FILE (SME decision 5, 2026-09-04; round-10
# finding P2-3). The contract said "a regular file or a directory" but the
# expansion only distinguished dir.exists() from everything else satisfying
# file.exists(), so symlinks and special files passed. A symlink binds bytes
# that live outside the declared collection -- the artifact table would then
# describe a file the collection does not contain -- and a FIFO, socket or
# device can block the hash or return different bytes on each read. Any of
# them anywhere in a cell makes the whole cell MISSING, with the member named
# in the refusal reason.
#
# The type test is .gate_is_regular_file() below; see its comment for why
# base R cannot answer the question and POSIX `test -f` is used instead.
.gate_cell_reason <- new.env(parent = emptyenv())

# Is each path a REGULAR, non-symlink file? Vectorised, one shell per call.
#
# Base R cannot answer this. `file.info()` reports size, isdir, mode
# (permissions only), times and ownership -- and on this machine a FIFO and an
# empty regular file are indistinguishable across every one of those columns
# (verified 2026-09-04, R 4.3.3: both report size 0, isdir FALSE, mode 644).
# So `file.info()$isdir == FALSE` plus `Sys.readlink() == ""` excludes
# directories and symlinks but NOT a FIFO, socket or device, and
# digest::digest(file = <fifo>) blocks forever rather than erroring (which is
# why the hash itself is bounded since integrity batch 12; see
# .gate_digest_member()).
#
# POSIX `test -f` is the type test, and `test -L` excludes the symlink that
# `test -f` would otherwise follow. The call is VECTORISED over paths, but only
# the directory branch of the cell expansion (line ~195) passes more than one
# path at a time: `regular()` at line ~164 calls it with a single path, so an
# explicitly named member costs one `sh` process each. The earlier comment here
# claimed "one `sh` per cell, not per member", which the code does not do.
# Off Unix the shell is unavailable and the isdir/readlink pair is used, which
# is complete there for the file types that exist in that namespace.
#
# FAILS CLOSED: an unexpected number of answers, a shell that did not run, or
# a path carrying a control character (which would break the line-per-path
# correspondence) all report FALSE, which makes the cell MISSING.
.gate_is_regular_file <- function(paths) {
  if (!length(paths)) return(logical(0))
  if (any(grepl("[[:cntrl:]]", paths))) return(rep(FALSE, length(paths)))
  if (!identical(.Platform$OS.type, "unix")) {
    fi <- file.info(paths)
    return(!is.na(fi$isdir) & !fi$isdir & !nzchar(Sys.readlink(paths)))
  }
  script <- 'for p in "$@"; do if [ -f "$p" ] && [ ! -L "$p" ]; then echo 1; else echo 0; fi; done'
  out <- tryCatch(suppressWarnings(
    system2("sh", args = c("-c", shQuote(script), "sh", shQuote(paths)),
            stdout = TRUE, stderr = FALSE)),
    error = function(e) character(0))
  out <- trimws(as.character(out))
  out <- out[nzchar(out)]
  if (length(out) != length(paths)) return(rep(FALSE, length(paths)))
  out == "1"
}

# The reason the last digest of `cell` refused it, or "" when it did not.
#
# Integrity batch 12 (R11-P2-1). This used to re-run the EXPANSION only, so a
# cell refused by the digest step -- an unreadable regular file, a member whose
# type changed after the type check, a hash that hit the time limit -- came
# back with an empty reason: the expansion saw a regular file and had nothing
# to say. The reason is now the one recorded by the most recent
# .gate_artifact_cell_sha256() of the SAME cell; for any other cell the full
# digest path is run so the reason is always the digest path's own.
.gate_artifact_cell_reason <- function(cell) {
  if (identical(.gate_cell_reason$cell, cell) && !is.null(.gate_cell_reason$why))
    return(.gate_cell_reason$why)
  invisible(.gate_artifact_cell_sha256(cell))
  if (is.null(.gate_cell_reason$why)) "" else .gate_cell_reason$why
}

.gate_expand_artifact_cell <- function(cell) {
  no <- function(why) { .gate_cell_reason$why <- why; NULL }
  .gate_cell_reason$why <- ""
  # A regular, non-symlink file: not a directory, not a symlink, and its
  # file.info() row readable at all.
  regular <- function(p) isTRUE(.gate_is_regular_file(p))
  why_not <- function(p) {
    if (nzchar(Sys.readlink(p)[1]))
      return(sprintf("'%s' is a symlink (to '%s'); a signed artifact collection contains regular files only, so its members are physically inside the declared collection",
                     p, Sys.readlink(p)))
    sprintf("'%s' is not a regular file (a FIFO, socket, device or other special file cannot be digested reproducibly)", p)
  }
  if (length(cell) != 1 || is.na(cell) || !nzchar(trimws(cell)))
    return(no("the cell is empty"))
  parts <- trimws(strsplit(cell, ";", fixed = TRUE)[[1]])
  parts <- parts[nzchar(parts)]
  if (!length(parts)) return(no("the cell names no path"))
  files <- character(0)
  saw_dir <- FALSE
  for (p in parts) {
    if (!file.exists(p)) return(no(sprintf("'%s' does not exist", p)))
    # A symlink to a directory is a symlink, and is refused before the
    # directory branch can expand it.
    if (nzchar(Sys.readlink(p)[1])) return(no(why_not(p)))
    if (dir.exists(p)) {
      # Walk one level at a time. list.files(recursive = TRUE) follows a nested
      # symlink directory and returns only its ordinary descendants, hiding the
      # symlink hop from the member type check. Inspect every entry before
      # descending so every accepted descendant is physically inside `p`.
      walk <- function(d) {
        entries <- sort(list.files(d, recursive = FALSE, full.names = TRUE,
                                   all.files = TRUE, no.. = TRUE,
                                   include.dirs = TRUE))
        out <- character(0)
        for (entry in entries) {
          if (nzchar(Sys.readlink(entry)[1])) return(no(why_not(entry)))
          if (dir.exists(entry)) {
            child <- walk(entry)
            if (is.null(child)) return(NULL)
            out <- c(out, child)
          } else {
            if (!regular(entry)) return(no(why_not(entry)))
            out <- c(out, entry)
          }
        }
        out
      }
      inner <- walk(p)
      if (is.null(inner)) return(NULL)
      # Preserve the pre-existing canonical digest order after safe traversal.
      inner <- sort(inner)
      if (!length(inner))
        return(no(sprintf("'%s' is an empty directory", p)))
      files <- c(files, inner)
      saw_dir <- TRUE
    } else {
      if (!regular(p)) return(no(why_not(p)))
      files <- c(files, p)
    }
  }
  # A member path that carries a newline or any other control character would
  # forge or split rows of the "<path>\t<sha256>\n" manifest below.
  ctrl <- files[grepl("[[:cntrl:]]", files)]
  if (length(ctrl))
    return(no(sprintf("member path '%s' carries a control character",
                      gsub("[[:cntrl:]]", "?", ctrl[1]))))
  # Two cells that name the same bytes twice would otherwise be indistinguishable
  # from a genuine two-member collection.
  norm <- normalizePath(files, winslash = "/", mustWork = FALSE)
  if (anyDuplicated(norm))
    return(no(sprintf("member '%s' is named twice",
                      files[anyDuplicated(norm)])))
  attr(files, "directory_cell") <- saw_dir
  files
}

# --- bounded member digest (integrity batch 12, R11-P2-1) -------------------
# The type check (.gate_is_regular_file) and the hash used to be two separate
# operations with nothing between them: a FIFO swapped in after the check
# blocked digest::digest(file=) forever (setTimeLimit() does not interrupt a
# blocking open(2) inside the digest C code -- verified 2026-09-05, R 4.3.3:
# a 2 s elapsed limit on a FIFO with no writer was still blocked when an
# outer 30 s timeout killed the process), and a digest error was swallowed
# into a bare MISSING with no reason.
#
# One member is now digested in ONE bounded operation that returns a
# structured {digest, reason}: on Unix the hash is computed by the coreutils
# `sha256sum` under `timeout`, which the kernel kills when the limit expires
# whatever the file is doing; its output is the same hex digest::digest()
# produces (pinned by tools/tests/test_batch12.R against digest::digest on the
# same bytes), so a passing computation's result is unchanged. Off Unix the
# hash falls back to digest::digest() with its error retained as the reason.
# After the hash the type is checked AGAIN and a member that is no longer a
# regular non-symlink file is refused.
#
# THE LIMIT, and why 600 s. The largest artifacts a gate package digests are
# rendered PDFs, CSV/RDS tables and NONMEM output directories -- tens to a few
# hundred megabytes; sha256sum reads several hundred MB/s on this class of
# disk, so a legitimate member finishes in seconds. Ten minutes is two orders
# of magnitude of headroom for a slow network mount, while still turning "a
# FIFO blocks the release forever" into "the cell is MISSING with a reason
# within ten minutes". The FIFO fixture in the battery exercises the limit at
# 2 s; the constant is the production bound.
#
# SNAPSHOT SEMANTICS (Codex round 12, R12-P2-4; SME decision 4, 2026-09-05).
# An artifact digest attests THE BYTES READ AT ONE INSTANT. The type re-check
# after hashing catches a member swapped for a FIFO, a socket, a symlink or a
# directory; it does not, and cannot, catch a regular file replaced by another
# regular file between the read and a later consumer's read. Resisting that
# would mean binding hash and consumption to one open descriptor or an
# immutable snapshot for every member of every package, which the SME declined
# for the current scope. The digest is therefore a point-in-time record; the
# controls around the instant are the write guard on the tree, the append-only
# ledger that binds the digest to the signature, and require_gate()'s rehash
# at every downstream stage, which detects a later replacement as drift.
GATE_DIGEST_TIME_LIMIT_SECONDS <- 600

.gate_digest_member <- function(path, limit = GATE_DIGEST_TIME_LIMIT_SECONDS) {
  no <- function(why) list(digest = NA_character_, reason = why)
  if (!identical(.Platform$OS.type, "unix")) {
    d <- tryCatch(digest::digest(file = path, algo = "sha256"),
                  error = function(e) conditionMessage(e))
    if (!is.character(d) || length(d) != 1L || !grepl("^[0-9a-f]{64}$", d))
      return(no(sprintf("'%s' could not be digested: %s", path, paste(d, collapse = " "))))
    return(list(digest = d, reason = ""))
  }
  if (!nzchar(Sys.which("timeout")) || !nzchar(Sys.which("sha256sum")))
    return(no(sprintf("'%s' could not be digested: bounded hashing needs the coreutils `timeout` and `sha256sum` on PATH, and one of them is missing",
                      path)))
  out <- tryCatch(suppressWarnings(
    system2("timeout", c(shQuote(format(limit, scientific = FALSE)), "sha256sum", "--", shQuote(path)),
            stdout = TRUE, stderr = TRUE)),
    error = function(e) structure(conditionMessage(e), status = 1L))
  st <- attr(out, "status"); if (is.null(st)) st <- 0L
  if (identical(as.integer(st), 124L))
    return(no(sprintf("'%s' could not be digested within the %s s time limit: the read blocked (a FIFO, a socket or a stalled mount), so the member is refused",
                      path, format(limit, scientific = FALSE))))
  if (!identical(as.integer(st), 0L))
    return(no(sprintf("'%s' could not be digested: %s", path,
                      paste(trimws(as.character(out)), collapse = " "))))
  # GNU coreutils prints a leading backslash and escapes the name when the path
  # contains a backslash or a newline; the digest itself is unaffected, so the
  # marker is stripped before the hex is read.
  hex <- sub("^\\\\", "", sub("\\s.*$", "", trimws(as.character(out))[1]))
  if (is.na(hex) || !grepl("^[0-9a-f]{64}$", hex))
    return(no(sprintf("'%s' could not be digested: sha256sum returned no digest (%s)",
                      path, paste(trimws(as.character(out)), collapse = " "))))
  list(digest = hex, reason = "")
}

.gate_artifact_cell_sha256 <- function(cell) {
  .gate_cell_reason$cell <- cell
  files <- .gate_expand_artifact_cell(cell)
  if (is.null(files)) return("MISSING")
  no <- function(why) { .gate_cell_reason$why <- why; "MISSING" }
  shas <- character(length(files))
  for (i in seq_along(files)) {
    r <- .gate_digest_member(files[i])
    if (is.na(r$digest)) return(no(r$reason))
    # The type is re-checked AFTER hashing: a member that was a regular file
    # at expansion and is something else now was swapped under the gate.
    if (!isTRUE(.gate_is_regular_file(files[i])))
      return(no(sprintf("'%s' changed type between the type check and the digest (it is no longer a regular, non-symlink file); a member that changes under the gate is refused",
                        files[i])))
    shas[i] <- r$digest
  }
  # A cell that names a directory ALWAYS digests the member manifest, even
  # when the directory holds one file: the bytes-only branch did not bind the
  # member's PATH, so renaming the sole member of a signed directory left the
  # digest unchanged (round 9, P0-2). A cell that names exactly one regular
  # file still digests that file's bytes, unchanged.
  if (length(files) == 1L && !isTRUE(attr(files, "directory_cell")))
    return(paste0("sha256:", shas))
  manifest <- paste0(files, "\t", shas, "\n", collapse = "")
  paste0("sha256:", digest::digest(manifest, algo = "sha256", serialize = FALSE))
}

# Kept under its historical name for the verification path.
.gate_rehash_one <- function(path) .gate_artifact_cell_sha256(path)

# --- canon2 self-verifiable package digest (integrity batch 3b, item A2) -----
# canon2 hashes the EXACT RENDERED BYTES of the generated block so anyone
# holding the record can recompute the digest from the record alone (canon1's
# digest, by contrast, was over the in-memory package objects and could only be
# reproduced by re-running the tool that built it).
#
# EXCLUSION, defined precisely (do not change without versioning the prefix):
#   * Input is the block vector lines[bi:bj] — from the BEGIN marker line
#     through the END marker line, inclusive.
#   * The first line (BEGIN marker) and the last line (END marker) are dropped.
#   * From what remains, drop the single VOLATILE timestamp line (the italic
#     "_Generated from tool output at ..._", matched by its "^_Generated from
#     tool output at " prefix) and the digest line itself (matched by
#     "^Package-Digest:"). The digest cannot hash itself; the timestamp is not
#     part of the signed content.
#   * Every other line — blank lines included — is retained in order and joined
#     with a single LINE FEED ("\n"). The join is hashed as UTF-8 bytes
#     (serialize = FALSE), so the value is byte-reproducible on any platform.
# render_gate_package computes this over the same block it writes; require_gate
# and CI recompute it over the block they read. All three must agree.
.gate_canon2_payload <- function(block_lines) {
  n <- length(block_lines)
  inner <- if (n > 2) block_lines[-c(1L, n)] else character(0)
  drop <- grepl("^_Generated from tool output at ", inner) |
          grepl("^Package-Digest:", inner)
  paste(inner[!drop], collapse = "\n")
}

.gate_canon2_digest <- function(block_lines) {
  paste0("canon2:sha256:",
         digest::digest(.gate_canon2_payload(block_lines), algo = "sha256",
                        serialize = FALSE))
}

# --- approval ledger (integrity batch 3b tranche 2, item 1) ------------------
# gates/<ID>/approvals.jsonl is an append-only chain of approval events, one
# JSON object per line. These helpers READ and VERIFY the chain; appending is
# the sanctioned job of tools/approve_gate.R (run via Rscript, never an agent
# edit). gate.R stays dependency-light (base R + digest, no jsonlite — CI's
# full battery does not install it), so JSON is parsed by a hand-rolled scanner
# over the compact single-line objects approve_gate writes.
#
# The event schema (all fields required; reason may be null):
#   schema_version, event_id, analysis_id, task, previous_event_id, decision,
#   signer, signed_at, recorded_at_utc, package_revision, record_digest,
#   package_digest, artifact_manifest_digest, reason
# event_id = sha256 over the canonical serialization of every OTHER field
# (previous_event_id included) — that is the chain link.
.LEDGER_FIELDS <- c(
  "schema_version", "analysis_id", "task", "previous_event_id", "decision",
  "signer", "signed_at", "recorded_at_utc", "package_revision", "record_digest",
  "package_digest", "artifact_manifest_digest", "reason")
.LEDGER_APPROVING <- c("approved", "approved_with_changes")

.ledger_path <- function(gate_file) {
  file.path(dirname(gate_file), "approvals.jsonl")
}

.json_escape <- function(s) {
  s <- gsub("\\", "\\\\", s, fixed = TRUE)
  s <- gsub('"', '\\"', s, fixed = TRUE)
  s <- gsub("\n", "\\n", s, fixed = TRUE)
  s <- gsub("\r", "\\r", s, fixed = TRUE)
  s <- gsub("\t", "\\t", s, fixed = TRUE)
  s
}

# Minimal flat-JSON object scanner: values are string ("..."), null, or number.
.json_parse_flat <- function(txt) {
  chars <- strsplit(txt, "", fixed = TRUE)[[1]]
  n <- length(chars); i <- 1L
  skipws <- function() while (i <= n && chars[i] %in% c(" ", "\t", "\n", "\r")) i <<- i + 1L
  parse_string <- function() {
    if (i > n || chars[i] != '"') stop("invalid ledger JSON: expected string", call. = FALSE)
    i <<- i + 1L; out <- character(0)
    while (i <= n) {
      c <- chars[i]
      if (c == "\\") {
        if (i == n) stop("invalid ledger JSON: unterminated string", call. = FALSE)
        nx <- chars[i + 1L]
        if (!nx %in% c('"', "\\", "/", "b", "f", "n", "r", "t"))
          stop("invalid ledger JSON: unsupported string escape", call. = FALSE)
        out <- c(out, switch(nx, b = "\b", f = "\f", n = "\n", r = "\r", t = "\t", nx))
        i <<- i + 2L
      } else if (c == '"') {
        i <<- i + 1L
        return(paste(out, collapse = ""))
      } else {
        out <- c(out, c); i <<- i + 1L
      }
    }
    stop("invalid ledger JSON: unterminated string", call. = FALSE)
  }
  parse_value <- function() {
    skipws()
    if (i > n) stop("invalid ledger JSON: expected value", call. = FALSE)
    if (chars[i] == '"') return(parse_string())
    j <- i
    while (i <= n && !chars[i] %in% c(",", "}", " ", "\t", "\n", "\r")) i <<- i + 1L
    token <- if (i > j) paste(chars[j:(i - 1L)], collapse = "") else ""
    if (identical(token, "null")) return(NA_character_)
    if (!grepl("^-?(0|[1-9][0-9]*)(\\.[0-9]+)?([eE][+-]?[0-9]+)?$", token))
      stop("invalid ledger JSON: expected string, null or finite number", call. = FALSE)
    value <- as.numeric(token)
    if (!is.finite(value))
      stop("invalid ledger JSON: expected finite number", call. = FALSE)
    value
  }
  res <- list()
  skipws()
  if (i > n || chars[i] != "{") stop("invalid ledger JSON: expected opening brace", call. = FALSE)
  i <- i + 1L; skipws()
  if (i <= n && chars[i] != "}") repeat {
    key <- parse_string(); skipws()
    if (key %in% names(res)) stop("invalid ledger JSON: repeated key '", key, "'", call. = FALSE)
    if (i > n || chars[i] != ":") stop("invalid ledger JSON: expected colon", call. = FALSE)
    i <- i + 1L
    res[[key]] <- parse_value(); skipws()
    if (i <= n && chars[i] == "}") break
    if (i > n || chars[i] != ",") stop("invalid ledger JSON: expected comma or closing brace", call. = FALSE)
    i <- i + 1L; skipws()
  }
  if (i > n || chars[i] != "}") stop("invalid ledger JSON: expected closing brace", call. = FALSE)
  i <- i + 1L; skipws()
  if (i <= n) stop("invalid ledger JSON: trailing content after object", call. = FALSE)
  res
}

# Read a ledger file into a list of events (named lists). Blank lines skipped.
.read_ledger <- function(path) {
  if (!file.exists(path)) return(list())
  lines <- readLines(path, warn = FALSE)
  keep <- which(nzchar(trimws(lines)))
  lapply(keep, function(k) tryCatch(.json_parse_flat(lines[[k]]), error = function(e)
    stop(path, " line ", k, ": ", conditionMessage(e), call. = FALSE)))
}

# Canonical serialization of an event for event_id: every field except
# event_id, in fixed order, "field=escaped-value", NA/null -> the NUL token,
# joined by LF and hashed as UTF-8 bytes.
.ledger_canonical <- function(ev, fields = .LEDGER_FIELDS) {
  parts <- vapply(fields, function(f) {
    v <- ev[[f]]
    if (is.null(v) || (length(v) == 1 && is.na(v))) v <- "null"
    else v <- .json_escape(as.character(v))
    paste0(f, "=", v)
  }, character(1))
  paste(parts, collapse = "\n")
}

.ledger_event_id <- function(ev, fields = .LEDGER_FIELDS) {
  paste0("sha256:", digest::digest(.ledger_canonical(ev, fields), algo = "sha256",
                                   serialize = FALSE))
}

# Chain verification shared by the approvals ledger and the errata ledger
# (batch 4, item E: one set of ledger primitives, never forked). Every stored
# event_id must recompute over `fields`, and previous_event_id must link each
# event to the one before it in append order. `required` names the fields that
# must be non-null in every event (completeness, not only chain integrity).
.ledger_verify_chain <- function(events, fields = .LEDGER_FIELDS,
                                 required = character(0)) {
  problems <- character(0)
  prev <- NA_character_
  seen <- character(0)
  is_null <- function(v) is.null(v) || (length(v) == 1 && is.na(v)) ||
    (is.character(v) && length(v) == 1 && !nzchar(trimws(v)))
  for (k in seq_along(events)) {
    ev <- events[[k]]
    recomputed <- .ledger_event_id(ev, fields)
    stored_id <- if (is.null(ev$event_id) || length(ev$event_id) != 1L ||
                     is.na(ev$event_id)) "<missing>" else as.character(ev$event_id)
    if (!identical(stored_id, recomputed))
      problems <- c(problems, sprintf("event %d: event_id mismatch (stored %s, recomputed %s)",
                                      k, stored_id, recomputed))
    exp_prev <- if (k == 1) NA_character_ else prev
    got_prev <- if (is_null(ev$previous_event_id)) NA_character_
                else as.character(ev$previous_event_id)
    if (!identical(got_prev, exp_prev))
      problems <- c(problems, sprintf("event %d: previous_event_id %s does not chain to %s",
                                      k, if (is.na(got_prev)) "null" else got_prev,
                                      if (is.na(exp_prev)) "null" else exp_prev))
    if (stored_id %in% seen)
      problems <- c(problems, sprintf("event %d: duplicate event_id %s", k, stored_id))
    for (f in required) if (is_null(ev[[f]]))
      problems <- c(problems, sprintf("event %d: required field '%s' is empty", k, f))
    seen <- c(seen, stored_id)
    prev <- stored_id
  }
  problems
}

# Approval schema verification shared by runtime, writers, and CLI. The ledger
# is chained per task, so grouping happens here before the shared link verifier.
.LEDGER_DECISIONS <- c(.LEDGER_APPROVING, "revoked", "release")
.LEDGER_REQUIRED <- c("schema_version", "analysis_id", "task", "decision",
                      "recorded_at_utc", "package_revision", "record_digest")
.LEDGER_REQUIRED_APPROVING <- c("signer", "signed_at", "package_digest",
                                "artifact_manifest_digest")

.gate_reserved_signer <- function(signer) {
  if (!is.character(signer) || length(signer) != 1L || is.na(signer)) return(FALSE)
  normalized <- tolower(trimws(gsub("[\\h\\v]+", " ", signer, perl = TRUE)))
  normalized %in% c("sme", "the sme")
}

.verify_approval_events <- function(events, expected_analysis = NULL) {
  if (!length(events)) return(character(0))
  problems <- character(0)
  task_keys <- character(length(events))
  for (k in seq_along(events)) {
    ev <- events[[k]]
    missing_fields <- setdiff(c("event_id", .LEDGER_FIELDS), names(ev))
    if (length(missing_fields))
      problems <- c(problems, sprintf("event %d: missing schema field(s): %s", k,
                                      paste(missing_fields, collapse = ", ")))
    if (!identical(as.character(ev$schema_version), "1"))
      problems <- c(problems, sprintf("event %d: unsupported schema_version '%s'", k,
                                      paste(ev$schema_version, collapse = "")))
    decision <- if (is.null(ev$decision)) "" else as.character(ev$decision)
    if (length(decision) != 1L || !decision %in% .LEDGER_DECISIONS)
      problems <- c(problems, sprintf("event %d: unknown decision '%s'", k,
                                      paste(decision, collapse = "")))
    if (.gate_reserved_signer(ev$signer))
      problems <- c(problems, sprintf("event %d: signer uses a reserved role label", k))
    analysis <- if (is.null(ev$analysis_id)) "" else as.character(ev$analysis_id)
    if (!is.null(expected_analysis) &&
        (length(analysis) != 1L || !identical(analysis, expected_analysis)))
      problems <- c(problems, sprintf("event %d: analysis_id '%s' does not match '%s'",
                                      k, paste(analysis, collapse = ""), expected_analysis))
    task <- if (is.null(ev$task)) "" else as.character(ev$task)
    task_keys[k] <- if (length(task) == 1L) task else ""
    record_digest <- if (is.null(ev$record_digest)) "" else as.character(ev$record_digest)
    if (length(record_digest) != 1L ||
        !grepl("^sha256:[0-9a-f]{64}$", record_digest))
      problems <- c(problems, sprintf("event %d: malformed record_digest", k))
    prev <- ev$previous_event_id
    if (!is.null(prev) && !(length(prev) == 1L && is.na(prev)) &&
        (length(prev) != 1L || !grepl("^sha256:[0-9a-f]{64}$", as.character(prev))))
      problems <- c(problems, sprintf("event %d: malformed previous_event_id", k))
    if (decision %in% .LEDGER_APPROVING) {
      for (f in .LEDGER_REQUIRED_APPROVING) {
        v <- ev[[f]]
        if (is.null(v) || length(v) != 1L || is.na(v) || !nzchar(trimws(as.character(v))))
          problems <- c(problems, sprintf(
            "event %d: approving event has empty required field '%s'", k, f))
      }
    }
  }
  for (task in unique(task_keys)) {
    idx <- which(task_keys == task)
    problems <- c(problems, .ledger_verify_chain(events[idx], .LEDGER_FIELDS,
                                                  .LEDGER_REQUIRED))
  }
  unique(problems)
}

# Latest event (by append order) for a task, or NULL.
.ledger_latest_for_task <- function(events, task) {
  hit <- NULL
  for (ev in events) if (identical(as.character(ev$task), task)) hit <- ev
  hit
}

# --- artifacts-table parse (strict) -----------------------------------------
# Structured parse of the markdown artifacts table inside a generated block.
# Returns list(has_table, cols, rows) where rows is a list of named character
# vectors, or list(has_table, error=<message>) on a malformed table. The parse
# is STRICT: a row whose cell count does not match the header is an error, not a
# best-effort read. Duplicate-path / MISSING / malformed-sha checks are the
# caller's (they are refusals with row context), not this parser's.
.gate_parse_artifacts <- function(block) {
  a <- grep("^### Artifacts\\s*$", block)
  if (length(a) == 0) return(list(has_table = FALSE))
  rest <- block[(a[1] + 1L):length(block)]
  hdr_i <- which(grepl("^\\s*\\|", rest))
  if (length(hdr_i) == 0) return(list(has_table = FALSE))
  cells <- function(ln) {
    s <- trimws(ln)
    if (!startsWith(s, "|") || !endsWith(s, "|"))
      return(list(error = "row does not have outer pipe delimiters"))
    chars <- strsplit(substr(s, 2L, nchar(s) - 1L), "", fixed = TRUE)[[1]]
    out <- character(0); cur <- character(0); i <- 1L
    while (i <= length(chars)) {
      ch <- chars[i]
      if (identical(ch, "|")) {
        out <- c(out, paste(cur, collapse = "")); cur <- character(0)
      } else if (identical(ch, "\\")) {
        if (i == length(chars)) return(list(error = "dangling backslash escape"))
        i <- i + 1L; nx <- chars[i]
        decoded <- switch(nx, "\\" = "\\", "|" = "|", "r" = "\r", "n" = "\n",
                          NA_character_)
        if (is.na(decoded))
          return(list(error = sprintf("unsupported backslash escape '\\%s'", nx)))
        cur <- c(cur, decoded)
      } else {
        cur <- c(cur, ch)
      }
      i <- i + 1L
    }
    trimws(c(out, paste(cur, collapse = "")))
  }
  h <- hdr_i[1]
  cols <- cells(rest[h])
  if (is.list(cols) && !is.null(cols$error))
    return(list(has_table = TRUE, error = paste("header", cols$error)))
  # The separator row (|---|---|) must follow the header.
  if (h + 1L > length(rest) || !grepl("^\\s*\\|[-\\s|]+\\|?\\s*$", rest[h + 1L]))
    return(list(has_table = TRUE, error = "no separator row under the header"))
  rows <- list()
  i <- h + 2L
  while (i <= length(rest) && grepl("^\\s*\\|", rest[i])) {
    r <- cells(rest[i])
    if (is.list(r) && !is.null(r$error))
      return(list(has_table = TRUE,
                  error = sprintf("row %d: %s", length(rows) + 1L, r$error)))
    if (length(r) != length(cols))
      return(list(has_table = TRUE,
                  error = sprintf("row %d has %d cells, header has %d",
                                  length(rows) + 1L, length(r), length(cols))))
    names(r) <- cols
    rows[[length(rows) + 1L]] <- r
    i <- i + 1L
  }
  list(has_table = TRUE, cols = cols, rows = rows)
}


# --- one strict gate-record parser (integrity batch 4, item A) --------------
# EVERY consumer of a gate record's fields — require_gate, render_gate_package's
# envelope header, approve_gate, ci_check.R, the pilot review's gate-integrity
# check — reads them through parse_gate_record(). Before batch 4 each held its
# own grep set, and they had drifted: approve_gate demanded a column-zero
# "Signed:" while require_gate tolerated indentation, and the envelope header
# took the first "Analysis ID:" line wholesale, so a merged/reflowed header
# rendered the wrong identity into the digested envelope.
#
# Regex set, DEFINED ONCE:
#   identity   ^\s*Analysis ID:   ^\s*Stage:          (body lines outside the block)
#   decision   ^\s*-\s*\[[xX]\]\s*APPROVED( WITH CHANGES)? / REJECTED
#   signature  ^\s*Signed:   ^\s*Date:   ^\s*Digest signed:   (outside the block)
# Leading whitespace is tolerated on every field (markdown tooling re-indents
# lines it re-nests; an indented signature is still the SME's signature).
# Tolerance never weakens binding: the digest checks stay byte-exact.
#
# Identity is STRICT: the value after "Analysis ID:" / "Stage:" must be a
# single token ([A-Za-z0-9][A-Za-z0-9._-]*), carry no other header key, and no
# template-comment residue ("<!--" / "-->"). It is then checked against the
# parent directory name (analysis) and the file stem (task). A merged line is a
# PROBLEM, never a value.
#
# CANON CONSTRAINT: for a CLEAN record the extracted analysis_id / task are
# byte-identical to what the pre-batch-4 envelope grep produced, so no canon2
# digest of an existing clean record changes.
.GATE_RE <- list(
  analysis_id   = "^\\s*Analysis ID:",
  stage         = "^\\s*Stage:",
  signed        = "^\\s*Signed:",
  date          = "^\\s*Date:",
  digest_signed = "^\\s*Digest signed:",
  approved      = "^\\s*-\\s*\\[[xX]\\]\\s*APPROVED",
  approved_wc   = "^\\s*-\\s*\\[[xX]\\]\\s*APPROVED WITH CHANGES",
  rejected      = "^\\s*-\\s*\\[[xX]\\]\\s*REJECTED",
  identity_tok  = "^[A-Za-z0-9][A-Za-z0-9._-]*$",
  block_start   = "<!-- BEGIN GENERATED PACKAGE -->",
  block_end     = "<!-- END GENERATED PACKAGE -->",
  package_digest = "^Package-Digest:\\s*",
  envelope      = "^Envelope: analysis_id=")
.GATE_HEADER_KEYS <- c("Analysis ID:", "Stage:", "Date opened:", "Signed:",
                       "Date:", "Digest signed:")

# Strict single-field extraction of one identity line. Returns
# list(value = <token or NA>, problems = character).
.gate_identity_field <- function(lines, key, label) {
  ln <- grep(key, lines, value = TRUE)
  if (!length(ln))
    return(list(value = NA_character_, problems = paste0(label, " line missing")))
  raw <- sub(key, "", ln[1])
  problems <- character(0)
  if (grepl("<!--|-->", raw, fixed = FALSE))
    problems <- c(problems, paste0(label, " line carries template-comment residue"))
  other <- setdiff(.GATE_HEADER_KEYS, sub("^\\^\\\\s\\*", "", key))
  other <- other[other != sub("^\\^\\\\s\\*", "", key)]
  merged <- other[vapply(other, function(k) grepl(k, raw, fixed = TRUE), logical(1))]
  if (length(merged))
    problems <- c(problems, paste0(label, " line is merged with another header field (",
                                   paste(merged, collapse = ", "), ")"))
  # Legacy-compatible value: strip a trailing comment, trim. For a clean line
  # this is exactly the pre-batch-4 envelope value.
  value <- trimws(sub("<!--.*", "", raw))
  if (!nzchar(value)) {
    problems <- c(problems, paste0(label, " value is empty"))
    value <- NA_character_
  } else if (!grepl(.GATE_RE$identity_tok, value)) {
    problems <- c(problems, paste0(label, " value is not a single identifier token: '",
                                   value, "'"))
  }
  list(value = value, problems = unique(problems))
}

# Signature fields. signer: text after "Signed:" up to any "Date:" fragment,
# comment stripped; date: the "Date:" fragment of the Signed line, else the
# first standalone "Date:" line (not "Date opened:"); digest_signed: the value
# of "Digest signed:". All read OUTSIDE the generated block.
.gate_signature_fields <- function(sig_lines) {
  strip <- function(x) trimws(sub("<!--.*", "", x))
  sl <- grep(.GATE_RE$signed, sig_lines, value = TRUE)
  signer <- NA_character_; date <- NA_character_
  if (length(sl)) {
    rest <- sub(.GATE_RE$signed, "", sl[1])
    s <- strip(sub("Date:.*$", "", rest))
    if (nzchar(s)) signer <- s
    if (grepl("Date:", rest)) {
      d <- strip(sub(".*Date:\\s*", "", rest))
      if (nzchar(d)) date <- d
    }
  }
  if (is.na(date)) {
    dl <- grep(.GATE_RE$date, sig_lines, value = TRUE)
    if (length(dl)) {
      d <- strip(sub(.GATE_RE$date, "", dl[1]))
      if (nzchar(d)) date <- d
    }
  }
  fl <- grep(.GATE_RE$digest_signed, sig_lines, value = TRUE)
  digest_signed <- if (length(fl)) strip(sub(.GATE_RE$digest_signed, "", fl[1])) else ""
  list(signer = signer, date = date, digest_signed = digest_signed)
}

.gate_decision_of <- function(lines) {
  chk <- function(pat) any(grepl(pat, lines))
  if (chk(.GATE_RE$rejected)) return("rejected")
  if (chk(.GATE_RE$approved_wc)) return("approved_with_changes")
  if (chk(.GATE_RE$approved)) return("approved")
  "unsigned"
}

# The parser. Never stops on a malformed record: every defect lands in
# `problems` and the consumer decides what is a refusal.
#   analysis_id, task   strict identity tokens (NA when unextractable)
#   stage_line          the raw Stage: line (diagnostics)
#   decision            approved | approved_with_changes | rejected | unsigned
#   signer, date        SME signature fields (NA when empty/placeholder)
#   digest_signed       value of "Digest signed:" ("" when empty)
#   block               the generated block lines (character(0) when absent)
#   package_digest      value of the block's Package-Digest line (NA if none)
#   artifacts           .gate_parse_artifacts(block)
#   problems            character vector; empty for a clean record
# `lines` (integrity batch 14, SME decision D14-1): parse the caller's OWN
# snapshot of the record instead of reading the file again. An approval, a
# revocation and a release each read the record bytes once under the ledger
# lock and then parse, validate, hash and archive THAT byte vector, so the
# fields of the event and the digest of the event are provably the same bytes.
# The path is still used for the identity checks (analysis id against the
# directory, stage against the file stem), which are properties of where the
# record lives, not of its bytes.
parse_gate_record <- function(path, lines = NULL) {
  if (!file.exists(path)) stop("Gate record not found: ", path, call. = FALSE)
  if (is.null(lines)) lines <- readLines(path, warn = FALSE)
  bi <- which(lines == .GATE_RE$block_start)
  bj <- which(lines == .GATE_RE$block_end)
  has_block <- length(bi) == 1 && length(bj) == 1 && bj > bi
  block <- if (has_block) lines[bi:bj] else character(0)
  body  <- if (has_block) lines[-(bi:bj)] else lines
  problems <- character(0)

  id <- .gate_identity_field(body, .GATE_RE$analysis_id, "Analysis ID")
  st <- .gate_identity_field(body, .GATE_RE$stage, "Stage")
  problems <- c(problems, id$problems, st$problems)
  dir_name <- basename(dirname(normalizePath(path, mustWork = FALSE)))
  stem <- sub("\\.md$", "", basename(path))
  if (!is.na(id$value) && !identical(id$value, dir_name))
    problems <- c(problems, sprintf("Analysis ID '%s' does not match the parent directory '%s'",
                                    id$value, dir_name))
  if (!is.na(st$value) && !identical(st$value, stem))
    problems <- c(problems, sprintf("Stage '%s' does not match the file stem (task) '%s'",
                                    st$value, stem))

  decision <- .gate_decision_of(body)
  sig <- .gate_signature_fields(body)
  if (.gate_reserved_signer(sig$signer))
    problems <- c(problems, "Signed: uses a reserved role label; enter the local reviewer's name")
  if (decision %in% .LEDGER_APPROVING) {
    if (is.na(sig$signer)) problems <- c(problems, "approved decision with an empty signer")
    if (is.na(sig$date))   problems <- c(problems, "approved decision with an empty date")
  }

  pkg_line <- grep(.GATE_RE$package_digest, block, value = TRUE)
  package_digest <- if (length(pkg_line))
    trimws(sub(.GATE_RE$package_digest, "", pkg_line[1])) else NA_character_

  # Envelope consistency: the digested header must restate the body identity.
  env_line <- grep(.GATE_RE$envelope, block, value = TRUE)
  if (length(env_line) && !is.na(id$value) && !is.na(st$value)) {
    expected <- paste0("Envelope: analysis_id=", id$value, "; task=", st$value)
    if (!identical(env_line[1], expected))
      problems <- c(problems, "envelope header does not restate the record's identity")
  } else if (length(env_line)) {
    problems <- c(problems, "envelope header present but the identity cannot be verified")
  }

  list(analysis_id = id$value, task = st$value,
       stage_line = grep(.GATE_RE$stage, body, value = TRUE)[1],
       decision = decision, signer = sig$signer, date = sig$date,
       digest_signed = sig$digest_signed, block = block,
       package_digest = package_digest,
       artifacts = if (has_block) .gate_parse_artifacts(block) else list(has_table = FALSE),
       problems = unique(problems))
}

# Identity-class problems (the ones a re-versioned record fixes), as opposed to
# signature-completeness problems (which require_gate refuses outright).
.gate_identity_problems <- function(parsed) {
  grep("Analysis ID|Stage|envelope", parsed$problems, value = TRUE)
}

# --- shared content verification (integrity batch 3b, items A1/A2/A4) --------
# LIVE ARTIFACT REHASH + canon2 self-recompute, in ONE function called by both
# require_gate() (which stops on any problem) and ci_check.R's binding check
# (which reports FAIL). Returns character(0) when the record's rendered content
# is internally consistent with disk, or a character vector of problems.
#
# Rehash: parse the artifacts table; a duplicate path, a malformed sha256 cell,
# or a literal MISSING is a refusal naming the offending rows. Every listed
# regular file is rehashed on disk and compared to its recorded sha256; a
# mismatch names the drifted files (first 5). A table with NO sha256 column is
# legacy (the allowlist freezes its whole file) and is skipped.
#
# canon2: when the Package-Digest line carries the "canon2:" prefix, recompute
# the digest from the block bytes and require recomputed == Package-Digest.
# (require_gate and CI separately bind Package-Digest == Digest signed, so the
# three-way agreement is complete.) A record whose digest lacks the prefix
# (canon1) is left to the existing string-equality binding and skipped here.
.gate_verify_content <- function(gate_file, lines, rehash_artifacts = TRUE) {
  problems <- character(0)
  block_start <- "<!-- BEGIN GENERATED PACKAGE -->"
  block_end   <- "<!-- END GENERATED PACKAGE -->"
  bi <- which(lines == block_start)
  bj <- which(lines == block_end)
  if (!(length(bi) == 1 && length(bj) == 1 && bj > bi)) return(problems)
  block <- lines[bi:bj]

  generated <- grep("^_Generated from tool output at ", block, value = TRUE)
  digest_lines <- grep("^Package-Digest:", block, value = TRUE)
  if (length(generated) > 1L)
    problems <- c(problems, "generated timestamp line occurs more than once")
  if (length(digest_lines) > 1L)
    problems <- c(problems, "Package-Digest line occurs more than once")
  if (length(generated) && any(!grepl(paste0(
      "^_Generated from tool output at [0-9]{4}-[0-9]{2}-[0-9]{2} ",
      "[0-9]{2}:[0-9]{2}:[0-9]{2}(\\.[0-9]+)? UTC; values are read from fit and check objects, not transcribed\\._$"), generated)))
    problems <- c(problems, "malformed generated timestamp line")
  if (length(digest_lines) && any(!grepl("^Package-Digest: (canon2:)?sha256:[0-9a-f]{64}$", digest_lines)))
    problems <- c(problems, "malformed Package-Digest line")

  # --- live artifact rehash ---
  # Round 9, P2-8: the rehash and the canon2 self-recompute used to share one
  # on/off switch, so release_gate()'s verify_artifacts = FALSE silently
  # disabled BOTH. The canon2 recompute reads only the record's own bytes and
  # can never deadlock a release, so it always runs; only the disk rehash is
  # skippable.
  parsed <- if (rehash_artifacts) .gate_parse_artifacts(block) else list(has_table = FALSE)
  if (isTRUE(parsed$has_table) && !is.null(parsed$error)) {
    return(paste0("artifacts table malformed: ", parsed$error))
  }
  if (isTRUE(parsed$has_table) && "sha256" %in% parsed$cols &&
      !"path" %in% parsed$cols)
    problems <- c(problems, "artifacts table malformed: sha256 column without path column")
  if (isTRUE(parsed$has_table) && "sha256" %in% parsed$cols &&
      "path" %in% parsed$cols) {
    paths <- vapply(parsed$rows, function(r) unname(r[["path"]]), character(1))
    shas  <- vapply(parsed$rows, function(r) unname(r[["sha256"]]), character(1))
    dup <- unique(paths[duplicated(paths)])
    if (length(dup))
      problems <- c(problems, paste0("duplicate artifact path(s): ",
                                     paste(dup, collapse = ", ")))
    miss <- paths[shas == "MISSING"]
    if (length(miss)) {
      # Each MISSING row carries WHY it is missing (batch 11, R10-P2-3): an
      # absent path, an empty directory, and a symlink or special-file member
      # are different defects and the SME must be able to tell them apart.
      why <- vapply(miss, function(p) {
        r <- tryCatch(.gate_artifact_cell_reason(p), error = function(e) "")
        if (nzchar(r)) paste0(p, " (", r, ")") else p
      }, character(1), USE.NAMES = FALSE)
      problems <- c(problems, paste0("artifact row(s) render as MISSING: ",
                                     paste(why, collapse = "; ")))
    }
    malformed <- paths[!grepl("^sha256:[0-9a-f]{64}$", shas) & shas != "MISSING"]
    if (length(malformed))
      problems <- c(problems, paste0("malformed sha256 cell for row(s): ",
                                     paste(malformed, collapse = ", ")))
    # A structural defect is a refusal on its own; do not also rehash.
    if (length(problems) == 0) {
      disk <- vapply(paths, .gate_rehash_one, character(1))
      drift <- paths[disk != shas]
      if (length(drift)) {
        shown <- paste(utils::head(drift, 5L), collapse = ", ")
        more <- if (length(drift) > 5L) sprintf(" (+%d more)", length(drift) - 5L) else ""
        problems <- c(problems, paste0("artifact bytes drifted since signature: ",
                                       shown, more))
      }
    }
  }

  # --- canon2 self-recompute (unknown canon prefixes fail CLOSED) ---
  # canon2  -> recompute over the block bytes and require equality.
  # canon1  -> a bare "sha256:" digest predating canon2; string-bound by
  #            require_gate/CI, so nothing to recompute here. NOTE: if item-1
  #            backfill ledgered a canon1 record, the ledger's record_digest
  #            freezes its whole-file bytes, making it tamper-evident too.
  # anything else -> an UNKNOWN canon version; refuse and name it (fail closed),
  #            rather than silently skip a digest we cannot verify.
  pkg_line <- grep("^Package-Digest:\\s*", block, value = TRUE)
  if (length(pkg_line)) {
    pkg_val <- trimws(sub("^Package-Digest:\\s*", "", pkg_line[1]))
    if (grepl("^canon2:", pkg_val)) {
      recomputed <- .gate_canon2_digest(block)
      if (!identical(recomputed, pkg_val))
        problems <- c(problems, paste0(
          "canon2 digest does not match the rendered block bytes (recomputed ",
          recomputed, " vs Package-Digest ", pkg_val,
          "); the block was edited after signature."))
    } else if (!grepl("^sha256:", pkg_val)) {
      problems <- c(problems, paste0(
        "unknown Package-Digest canon version (cannot self-verify): ", pkg_val))
    }
  }
  problems
}

# Returns the allowlist entry whose path matches this gate file (by repo-
# relative path), or NULL. Path match is on the frozen key; hash is checked by
# the caller so an edited-but-listed record can be told apart from an off-list
# one.
.legacy_entry_for <- function(gate_file, entries = .read_legacy_gates()) {
  rel <- .gate_relpath(gate_file)
  for (e in entries) if (identical(e$path, rel)) return(e)
  NULL
}

# Returns the gate file path invisibly if the gate is signed; stops otherwise.
# Signed means: an "[x]" checkbox on the APPROVED or APPROVED WITH CHANGES
# line, and a "Signed:" line carrying a non-empty name. These markers are
# defined in templates/gate-signoff.md; do not change one without the other.
# enforce_ledger = FALSE runs every check EXCEPT the approval-ledger
# enforcement. approve_gate()/revoke_gate() use it to validate a record's
# content before RECORDING an event — a revoked gate must stay re-approvable,
# so the recording step cannot itself be blocked by the ledger it is about to
# extend.
# verify_artifacts = FALSE skips ONLY the live-artifact rehash inside
# .gate_verify_content; its canon2 self-recompute still runs, as do parse,
# decision box, digest binding, allowlist and ledger checks. (Before round 9
# this comment was untrue: FALSE skipped the whole function, canon2 included.) release_gate() alone uses it: artifact drift is
# the reason a signature gets released, and the release archives the exact
# released bytes, so refusing a drifted record there is a deadlock, not a guard.
# VALIDATING A SNAPSHOT (integrity batch 15, Codex round 14 R14-P1-3). The
# writing paths in tools/approve_gate.R build an event from ONE immutable
# snapshot of the record bytes, but full content validation used to re-read the
# path independently, so the bytes that were validated and the bytes that were
# parsed, hashed and archived could differ: an editor's save/undo could present
# a valid record to this function and a defective one to the event. `lines` and
# `bytes` let a caller hand over the snapshot it already read; every check below
# then runs on exactly those bytes. Omitting them reads the path, which is what
# every ordinary gate check does.
require_gate <- function(gate_file, enforce_ledger = TRUE, verify_artifacts = TRUE,
                         lines = NULL, bytes = NULL) {
  if (!file.exists(gate_file)) {
    stop("Gate record not found: ", gate_file,
         "\nCreate it from templates/gate-signoff.md and obtain SME sign-off.",
         call. = FALSE)
  }
  if (is.null(lines) != is.null(bytes))
    stop("require_gate: a snapshot is `lines` AND `bytes` from the same read; ",
         "supplying one without the other would validate one set of bytes and ",
         "hash another. Refused.", call. = FALSE)
  if (is.null(lines)) lines <- readLines(gate_file, warn = FALSE)
  # ONE parser (batch 4, item A): decision, signature and identity fields come
  # from parse_gate_record, never from a private grep.
  parsed <- parse_gate_record(gate_file, lines = lines)

  # Rejection outranks approval: if both boxes are somehow checked, the gate
  # still stops — ambiguity is never resolved in favour of proceeding.
  if (identical(parsed$decision, "rejected")) {
    stop("Gate REJECTED: ", gate_file,
         "\nRe-enter the stage named in the gate record before proceeding.",
         call. = FALSE)
  }
  if (!parsed$decision %in% .LEDGER_APPROVING || is.na(parsed$signer)) {
    stop("Gate not signed: ", gate_file,
         "\nSME approval ([x] APPROVED + Signed: <name>) is required before ",
         "downstream work runs.", call. = FALSE)
  }
  if (.gate_reserved_signer(parsed$signer))
    stop("Gate not signed: ", gate_file,
         "\nSigned: uses a reserved role label; enter the local reviewer's name.",
         call. = FALSE)
  if (is.na(parsed$date)) {
    stop("Gate signature has no date: ", gate_file,
         "\nAn approving signature must carry a Date: (on the Signed: line or ",
         "its own line). Refused.", call. = FALSE)
  }
  # Identity problems (merged/reflowed header, envelope mismatch) are REPORTED
  # here and inventoried by CI (pinned to the known records until they are
  # re-versioned); they do not weaken the digest binding below, which stays
  # byte-exact, so they are not a stop. render_gate_package refuses to create
  # new ones.
  idp <- .gate_identity_problems(parsed)
  if (length(idp))
    message("Gate identity fields malformed (re-version to fix; CI pins the known ",
            "records): ", gate_file, "\n  - ", paste(idp, collapse = "\n  - "))

  # --- signed errata (batch 4, item E) --------------------------------------
  # Consulted BEFORE the legacy short-circuit so an allowlisted record cannot
  # bypass a recorded correction. minor -> notice; material -> stop unless the
  # latest approving ledger event postdates the erratum (the SME re-signed with
  # knowledge of it).
  # In recording mode (enforce_ledger = FALSE, used by approve_gate) the notice
  # is still emitted but a material erratum does not stop: the SME's re-signature
  # is exactly the ledger event that resolves it, so the recording step cannot be
  # blocked by the condition it is about to clear.
  .gate_apply_errata(gate_file, enforce = enforce_ledger)

  # --- digest binding (finding C1) -----------------------------------------
  # A signature only means something if it binds to the PACKAGE that was signed.
  # The generated block ends with a "Package-Digest: sha256:<hex>" line and the
  # signature section carries a "Digest signed:" field the SME pastes it into.
  # require_gate here verifies only the BINDING (that the two match); it does
  # not recompute the digest from artifacts — self-consistency is CI's job.
  #
  # Markers are the same constants render_gate_package uses. Read them by grep
  # rather than sourcing gate_package.R (which pulls package deps) — they are
  # plain strings and cannot drift because both files are versioned together.
  block <- parsed$block
  pkg_line <- if (is.na(parsed$package_digest)) character(0) else parsed$package_digest

  # --- legacy allowlist gate (finding: digest-strip downgrade) --------------
  # A signed record's presence on the frozen allowlist is checked FIRST, so an
  # allowlisted record whose bytes drifted is refused whether or not it still
  # carries a Package-Digest line (an edited legacy record is no longer legacy).
  entry <- .legacy_entry_for(gate_file)
  if (!is.null(entry)) {
    file_hash <- if (is.null(bytes)) .gate_sha256(gate_file)
                 else paste0("sha256:", digest::digest(bytes, algo = "sha256",
                                                       serialize = FALSE))
    if (!identical(entry$sha256, file_hash)) {
      stop("Gate is on the legacy allowlist but its bytes have drifted: ",
           gate_file, "\nfrozen ", entry$sha256, " vs disk ", file_hash,
           ".\nAn edited legacy record is no longer legacy; the SME must ",
           "re-sign it under the current digest mechanism.", call. = FALSE)
    }
    if (enforce_ledger) {
      task <- sub("\\.md$", "", basename(gate_file))
      events <- .read_ledger(.ledger_path(gate_file))
      latest <- .ledger_latest_for_task(events, task)
      if (!is.null(latest) && !as.character(latest$decision) %in% .LEDGER_APPROVING) {
        stop("Gate blocked by the approval ledger: the latest event for '", task,
             "' is '", as.character(latest$decision), "' (a rejection/revocation outranks any older ",
             "approval): ", gate_file, "\nA NEW approving event must supersede it.",
             call. = FALSE)
      }
    }
    if (identical(entry$kind, "digestless")) {
      message("Gate legacy-unbound, on the frozen allowlist (no Package-Digest; ",
              "signature predates the digest mechanism): ", gate_file)
      return(invisible(gate_file))
    }
    # kind:canon1 -- the SME confirmed these reviewed bytes; accept without
    # re-deriving the digest binding. Falls through to the digest check below
    # only if the entry lacked a recognised kind.
    if (identical(entry$kind, "canon1")) {
      message("Gate canon1 (SME-confirmed reviewed bytes, on the frozen ",
              "allowlist): ", gate_file)
      return(invisible(gate_file))
    }
  }

  if (length(pkg_line) == 0) {
    # No digest line and NOT on the allowlist -> a digest-less signed record
    # that is either genuinely un-frozen or a digest-strip downgrade of a
    # bound record. Either way it is refused: the only digest-less records that
    # pass are the ones frozen in tools/legacy_gates.yaml.
    stop("digest-less signed record not in the legacy allowlist (possible ",
         "digest-strip downgrade): ", gate_file,
         "\nA signed gate record with no Package-Digest line passes only when ",
         "it is frozen in tools/legacy_gates.yaml by (path, sha256). If this ",
         "is a genuine pre-digest record, add it to the allowlist; otherwise a ",
         "NEW SME signature over the current package is required.",
         call. = FALSE)
  }

  pkg_digest <- parsed$package_digest
  # The Digest-signed field lives OUTSIDE the generated block (signature
  # section); the parser strips any HTML-comment placeholder.
  signed_val <- parsed$digest_signed

  if (!nzchar(signed_val) || !identical(signed_val, pkg_digest)) {
    stop("Gate signature does not cover this package: ", gate_file,
         "\nThe 'Digest signed:' field (", if (nzchar(signed_val)) signed_val
         else "<empty>", ") does not match the Package-Digest (", pkg_digest,
         ").\nThe package changed after signature, or the signature never ",
         "bound to it. A NEW SME signature over the current package is ",
         "required.", call. = FALSE)
  }

  # --- live artifact rehash + canon2 self-recompute (batch 3b, item A1/A2) ---
  # Runs AFTER the allowlist/canon1 short-circuit (which returned above) and
  # AFTER the digest binding: an allowlisted digestless/canon1 record never
  # reaches here (the allowlist already freezes its whole file). Every other
  # signed record is rehashed against disk and, when its digest is canon2,
  # recomputed from the block bytes. Shared with CI so the two never diverge.
  content_problems <- .gate_verify_content(gate_file, lines,
                                           rehash_artifacts = verify_artifacts)
  if (length(content_problems)) {
    stop("Gate content verification failed: ", gate_file, "\n  - ",
         paste(content_problems, collapse = "\n  - "),
         "\nThe signed package no longer matches the artifacts/bytes on disk. ",
         "A NEW SME signature over the current package is required.",
         call. = FALSE)
  }

  # --- approval-ledger enforcement (batch 3b tranche 2, item 1) -------------
  # Reached only by NON-allowlisted signed records (allowlisted records above
  # still refuse a latest non-approving event). The task key
  # is the gate filename stem — one record per stage, robust to messy Stage:
  # lines. The LATEST event for the task governs: an approving decision whose
  # record_digest and package_digest still match the live file passes; a
  # rejection/revocation refuses regardless of older approvals; a record with no
  # event yet passes with a one-line warning (TRANSITIONAL — CI inventories it).
  if (!enforce_ledger) return(invisible(gate_file))
  task <- sub("\\.md$", "", basename(gate_file))
  events <- .read_ledger(.ledger_path(gate_file))
  chain_problems <- .verify_approval_events(
    events, expected_analysis = basename(dirname(normalizePath(gate_file,
                                                               mustWork = FALSE))))
  if (length(chain_problems)) {
    stop("Gate approval ledger integrity failed: ", .ledger_path(gate_file),
         "\n  - ", paste(chain_problems, collapse = "\n  - "),
         "\nNo approval state was consumed.", call. = FALSE)
  }
  latest <- .ledger_latest_for_task(events, task)
  if (is.null(latest)) {
    message("Gate signed but not yet ledgered — run approve_gate(): ", gate_file)
  } else {
    dec <- as.character(latest$decision)
    if (!dec %in% .LEDGER_APPROVING) {
      stop("Gate blocked by the approval ledger: the latest event for '", task,
           "' is '", dec, "' (a rejection/revocation outranks any older ",
           "approval): ", gate_file, "\nA NEW approving event must supersede it.",
           call. = FALSE)
    }
    # THE SNAPSHOT'S BYTES, WHEN A SNAPSHOT WAS SUPPLIED (integrity batch 16,
    # Codex round 15 R15-P3-1). Everything above this point validates `lines`
    # -- the caller's snapshot when one was given -- while this comparison
    # re-read the live file, so a snapshot-validating caller with the default
    # `enforce_ledger = TRUE` could validate snapshot A's content and compare
    # live B's whole-record hash against the ledger. A snapshot argument means
    # "validate exactly these bytes"; it now means that here too.
    cur_record <- if (is.null(bytes)) .gate_sha256(gate_file)
                  else paste0("sha256:", digest::digest(bytes, algo = "sha256",
                                                        serialize = FALSE))
    if (!identical(as.character(latest$record_digest), cur_record)) {
      stop("Gate ledger is stale: the latest approving event's record_digest (",
           latest$record_digest, ") does not match the current file bytes (",
           cur_record, "): ", gate_file,
           "\nThe record changed after it was ledgered; re-approve it.",
           call. = FALSE)
    }
    if (!identical(as.character(latest$package_digest), pkg_digest)) {
      stop("Gate ledger package_digest (", latest$package_digest,
           ") does not match the record's Package-Digest (", pkg_digest, "): ",
           gate_file, "\nRe-approve the current package.", call. = FALSE)
    }
  }

  invisible(gate_file)
}


# --- signed errata: read side (integrity batch 4, item E) --------------------
# gates/<ID>/errata.jsonl is an append-only hash chain of SME-signed
# corrections to claims inside SIGNED records, keyed by gate path. The record's
# bytes are never touched (the signature stays verifiable); the erratum is the
# visible, independently signed overlay. Recording is tools/errata.R's job
# (Rscript, SME-supplied signer/date); this file only READS and ENFORCES.
.ERRATA_FIELDS <- c(
  "schema_version", "gate_path", "record_digest", "claim_sha256", "claim_excerpt",
  "correction", "evidence", "materiality", "signer", "date", "recorded_at_utc",
  "previous_event_id")
.ERRATA_REQUIRED <- c("gate_path", "record_digest", "claim_sha256", "claim_excerpt",
                      "correction", "evidence", "materiality", "signer", "date",
                      "recorded_at_utc")
.ERRATA_MATERIALITY <- c("minor", "material")

.errata_path <- function(gate_file) file.path(dirname(gate_file), "errata.jsonl")

# Gate identity used to match errata to a record: "<analysis dir>/<file>", so a
# fixture under tempdir() and a repo-relative gates/<ID>/<task>.md compare the
# same way.
.gate_key <- function(path) {
  file.path(basename(dirname(normalizePath(path, mustWork = FALSE))), basename(path))
}

.errata_for_gate <- function(gate_file, events = NULL) {
  if (is.null(events)) events <- .read_ledger(.errata_path(gate_file))
  key <- .gate_key(gate_file)
  Filter(function(e) identical(.gate_key(as.character(e$gate_path)), key), events)
}

.verify_errata_events <- function(events, expected_analysis = NULL) {
  if (!length(events)) return(character(0))
  problems <- character(0)
  gate_keys <- character(length(events))
  for (k in seq_along(events)) {
    ev <- events[[k]]
    missing_fields <- setdiff(c("event_id", .ERRATA_FIELDS), names(ev))
    if (length(missing_fields))
      problems <- c(problems, sprintf("event %d: missing schema field(s): %s", k,
                                      paste(missing_fields, collapse = ", ")))
    if (!identical(as.character(ev$schema_version), "1"))
      problems <- c(problems, sprintf("event %d: unsupported schema_version '%s'", k,
                                      paste(ev$schema_version, collapse = "")))
    if (.gate_reserved_signer(ev$signer))
      problems <- c(problems, sprintf("event %d: signer uses a reserved role label", k))
    materiality <- if (is.null(ev$materiality)) "" else as.character(ev$materiality)
    if (length(materiality) != 1L || !materiality %in% .ERRATA_MATERIALITY)
      problems <- c(problems, sprintf("event %d: unknown materiality '%s'", k,
                                      paste(materiality, collapse = "")))
    gate_path <- if (is.null(ev$gate_path)) "" else as.character(ev$gate_path)
    valid_path <- length(gate_path) == 1L && !is.na(gate_path) &&
      nzchar(trimws(gate_path))
    # Runtime consumption normalizes portable absolute/relative prefixes.
    # Verify that same identity so aliases cannot form independent chains.
    gate_keys[k] <- if (valid_path) .gate_key(gate_path) else ""
    if (!valid_path)
      problems <- c(problems, sprintf("event %d: gate_path must be one nonempty path", k))
    if (!is.null(expected_analysis) && valid_path &&
        !identical(basename(dirname(gate_keys[k])), expected_analysis))
      problems <- c(problems, sprintf("event %d: gate_path '%s' is outside analysis '%s'",
                                      k, gate_path, expected_analysis))
    for (f in c("record_digest", "claim_sha256")) {
      v <- if (is.null(ev[[f]])) "" else as.character(ev[[f]])
      if (length(v) != 1L || !grepl("^sha256:[0-9a-f]{64}$", v))
        problems <- c(problems, sprintf("event %d: malformed %s", k, f))
    }
    prev <- ev$previous_event_id
    if (!is.null(prev) && !(length(prev) == 1L && is.na(prev)) &&
        (length(prev) != 1L || !grepl("^sha256:[0-9a-f]{64}$", as.character(prev))))
      problems <- c(problems, sprintf("event %d: malformed previous_event_id", k))
  }
  for (gate_path in unique(gate_keys)) {
    idx <- which(gate_keys == gate_path)
    problems <- c(problems, .ledger_verify_chain(events[idx], .ERRATA_FIELDS,
                                                  .ERRATA_REQUIRED))
  }
  unique(problems)
}

# UTC timestamps written by approve_gate / errata.R ("%Y-%m-%d %H:%M:%S UTC")
# compare lexicographically; parse defensively anyway.
.utc_stamp <- function(x) {
  if (is.null(x) || is.na(x)) return(NA_real_)
  as.numeric(as.POSIXct(sub(" UTC$", "", as.character(x)), tz = "UTC",
                        tryFormats = c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M")))
}

.gate_apply_errata <- function(gate_file, enforce = TRUE) {
  all_errata <- .read_ledger(.errata_path(gate_file))
  chain_problems <- .verify_errata_events(
    all_errata, expected_analysis = basename(dirname(normalizePath(gate_file,
                                                                  mustWork = FALSE))))
  if (length(chain_problems)) {
    stop("Gate errata ledger integrity failed: ", .errata_path(gate_file),
         "\n  - ", paste(chain_problems, collapse = "\n  - "),
         "\nNo erratum materiality was consumed.", call. = FALSE)
  }
  errata <- .errata_for_gate(gate_file, events = all_errata)
  if (!length(errata)) return(invisible(NULL))
  n <- length(errata)
  message("KNOWN SUPERSEDED: ", n, if (n == 1) " erratum" else " errata",
          " recorded against this gate (see ", .errata_path(gate_file), "): ",
          gate_file)
  material <- Filter(function(e) identical(as.character(e$materiality), "material"), errata)
  if (!length(material) || !enforce) return(invisible(NULL))
  # The latest approving ledger event, if it postdates every material erratum,
  # is the SME's re-signature with knowledge; otherwise the gate stops.
  events <- .read_ledger(.ledger_path(gate_file))
  approval_problems <- .verify_approval_events(
    events, expected_analysis = basename(dirname(normalizePath(gate_file,
                                                               mustWork = FALSE))))
  if (length(approval_problems)) {
    stop("Gate approval ledger integrity failed while applying errata: ",
         .ledger_path(gate_file), "\n  - ",
         paste(approval_problems, collapse = "\n  - "),
         "\nNo approval timestamp was consumed.", call. = FALSE)
  }
  task <- sub("\\.md$", "", basename(gate_file))
  latest <- .ledger_latest_for_task(events, task)
  latest_ok <- !is.null(latest) && as.character(latest$decision) %in% .LEDGER_APPROVING
  latest_t <- if (latest_ok) .utc_stamp(latest$recorded_at_utc) else NA_real_
  newest_material <- max(vapply(material, function(e) .utc_stamp(e$recorded_at_utc), numeric(1)))
  if (is.na(latest_t) || is.na(newest_material) || !(latest_t > newest_material)) {
    stop("Gate blocked by a MATERIAL erratum: ", gate_file, "\n", length(material),
         " material erratum/errata are recorded in ", .errata_path(gate_file),
         " and no approving ledger event postdates them. The SME must re-sign the ",
         "record with knowledge of the correction (a new approving event) before ",
         "downstream work runs.", call. = FALSE)
  }
  invisible(NULL)
}
