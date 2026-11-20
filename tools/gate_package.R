# Gate package rendering (Layer 2).
#
# Numbers reaching a gate record are RENDERED from tool output, never
# transcribed by an agent (SME instruction, 2026-08-28: agent behaviour must
# be deterministic and tools-grounded). The agent writes only the prose
# sections — hypothesis, interpretation, concerns — which are passed in as
# text and never contain computed values.
#
# render_gate_package() rewrites the machine-generated block of a gate
# record between the markers below, leaving SME decision and prose intact.

GATE_BLOCK_START <- "<!-- BEGIN GENERATED PACKAGE -->"
GATE_BLOCK_END   <- "<!-- END GENERATED PACKAGE -->"

# Line, inside the generated block just above END, that carries the digest of
# the package content. tools/gate.R binds the SME signature to this value.
GATE_DIGEST_PREFIX <- "Package-Digest: "

# --- canonical package digest ----------------------------------------------
# gate_package_digest() returns a sha256 over a CANONICAL serialization of the
# package content (artifacts table, named tables in order, notes). It is the
# object the SME's signature binds to; the same objects must digest to the same
# value forever, so the serialization is fixed and free of anything volatile.
#
# CANON (do not change without versioning the digest):
#   * The serialized text is EXACTLY the markdown the reader sees, produced by
#     md_table() (which format()s every column with trim = TRUE). Digesting the
#     rendered cells means the SME signs precisely what is displayed.
#   * Field order is fixed: the artifacts table first, then each named table in
#     the order it appears in `tables` (its name line included), then the notes
#     lines. Sections are joined with a single "\n".
#   * NOTHING volatile participates: no generation timestamp, no file paths of
#     the record, no locale-dependent formatting (md_table uses base format()
#     with the default OutDec = "."; callers must not pass pre-formatted,
#     locale-variant strings).
# The hash is computed with serialize = FALSE over the joined string, so it is
# a hash of the bytes of the text, reproducible by anyone with the same objects.
gate_package_canonical <- function(artifacts, tables = list(),
                                   notes = character(0)) {
  parts <- md_table(artifacts)
  for (nm in names(tables)) {
    parts <- c(parts, paste0("### ", nm), md_table(tables[[nm]]))
  }
  if (length(notes)) parts <- c(parts, notes)
  paste(parts, collapse = "\n")
}

gate_package_digest <- function(artifacts, tables = list(),
                                notes = character(0)) {
  canon <- gate_package_canonical(artifacts, tables, notes)
  paste0("sha256:", digest::digest(canon, algo = "sha256", serialize = FALSE))
}

# sha256 of an artifact table cell, prefixed "sha256:", or the literal
# "MISSING" (absent path, unreadable file, empty directory): a visible defect
# in the package the SME can see and refuse, never a render-time error. The
# rule -- "; "-separated paths, directories expanded to their sorted regular
# files, a composite digest for several files -- lives in ONE place,
# gate.R::.gate_artifact_cell_sha256, so the render and the require_gate
# rehash cannot diverge.
.gate_artifact_sha256 <- function(path) {
  .gp_ensure_gate_tools()
  .gate_artifact_cell_sha256(path)
}

# --- typed deterministic cell rendering (integrity batch 3b tranche 2, item 2)
# The digest hashes the EXACT RENDERED BYTES of the table, so those bytes must be
# byte-identical across platforms and R option settings (digits, scipen, OutDec,
# LC_NUMERIC). format() is locale- and option-sensitive (OutDec = "," alone
# changes every decimal point), so it is replaced by a TYPED renderer that maps
# each supported column class to a fixed textual form independent of options:
#
#   double   -> integer-valued finite values render in plain integer form
#               (sprintf("%.0f"), so values beyond .Machine$integer.max are still
#               exact); every other finite value renders via sprintf("%.17g")
#               (full round-trip precision, locale-independent decimal point).
#               Negative zero is normalised to positive zero. Non-finite values
#               render deterministically as the fixed tokens NA / NaN / Inf /
#               -Inf (chosen over rejection so a legitimately non-finite computed
#               value still reaches the SME to see and dispose).
#   integer  -> as.character (NA -> NA token).
#   logical  -> "TRUE" / "FALSE" (NA -> NA token).
#   factor   -> its character levels (then escaped as character).
#   character-> passed through with explicit escaping (see .md_escape_cell).
#   anything else (list-columns, unsupported S4/S3 classes) -> stop().
#
# The NA token is a fixed literal so it never varies with getOption("na.print").
GATE_NA_TOKEN <- "NA"

# Escape map for character cells (comment is the spec):
#   backslash "\"  -> "\\"   (escaped FIRST so later escapes are not doubled)
#   pipe      "|"  -> "\|"   (a literal pipe would split the markdown cell)
#   CR        "\r" -> "\r"   (a bare CR would break the one-line-per-row layout)
#   LF        "\n" -> "\n"   (same; keeps every row on exactly one physical line)
.md_escape_cell <- function(x) {
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub("|", "\\|", x, fixed = TRUE)
  x <- gsub("\r", "\\r", x, fixed = TRUE)
  x <- gsub("\n", "\\n", x, fixed = TRUE)
  x
}

# Render one column (any supported class) to a deterministic character vector.
.md_render_col <- function(x, col = "<unnamed>") {
  if (inherits(x, c("Date", "POSIXt", "difftime")) || !is.null(dim(x)))
    stop(sprintf("column '%s' has a date/time class or dimensions; supply character input",
                 col), call. = FALSE)
  if (is.list(x) && !is.data.frame(x))
    stop(sprintf("column '%s' is a list-column; gate tables reject list-columns",
                 col), call. = FALSE)
  if (is.factor(x)) x <- as.character(x)
  if (is.logical(x)) {
    out <- ifelse(is.na(x), GATE_NA_TOKEN, ifelse(x, "TRUE", "FALSE"))
    return(out)
  }
  if (is.integer(x)) {
    return(ifelse(is.na(x), GATE_NA_TOKEN, as.character(x)))
  }
  if (is.double(x)) {
    out <- character(length(x))
    for (i in seq_along(x)) {
      v <- x[i]
      if (is.na(v) && !is.nan(v)) { out[i] <- GATE_NA_TOKEN; next }
      if (is.nan(v)) { out[i] <- "NaN"; next }
      if (is.infinite(v)) { out[i] <- if (v > 0) "Inf" else "-Inf"; next }
      if (v == 0) v <- 0                      # normalise negative zero
      out[i] <- if (v == round(v)) sprintf("%.0f", v) else sprintf("%.17g", v)
    }
    return(out)
  }
  if (is.character(x)) {
    return(ifelse(is.na(x), GATE_NA_TOKEN, .md_escape_cell(x)))
  }
  stop(sprintf("column '%s' has unsupported class %s for gate rendering",
               col, paste(class(x), collapse = "/")), call. = FALSE)
}

# Renders a data.frame as a markdown table (header, separator, one line per
# row) using the TYPED deterministic renderer above. Rejects blank or duplicate
# column names — a defect that would make the digested bytes ambiguous.
md_table <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  nm <- names(df)
  if (any(!nzchar(trimws(nm))))
    stop("gate table has a blank column name", call. = FALSE)
  if (anyDuplicated(nm))
    stop(sprintf("gate table has duplicate column name(s): %s",
                 paste(unique(nm[duplicated(nm)]), collapse = ", ")), call. = FALSE)
  cols <- lapply(seq_along(df), function(j) .md_render_col(df[[j]], nm[j]))
  c(paste0("| ", paste(.md_escape_cell(nm), collapse = " | "), " |"),
    paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|"),
    if (nrow(df) > 0)
      vapply(seq_len(nrow(df)), function(i)
        paste0("| ", paste(vapply(cols, `[[`, character(1), i), collapse = " | "),
               " |"), character(1))
    else character(0))
}

# --- evidence-note split (integrity batch 3b tranche 2, item 3) --------------
# MIGRATION RULE: `notes` accepts EITHER the legacy form — a plain character
# vector, rendered verbatim under "### Notes" INSIDE the digested envelope,
# exactly as before (every existing task module keeps working untouched) — OR a
# LIST that may mix plain character strings, structured evidence notes
# (evidence_note(); tools/evidence_notes.R) and authored notes (authored_note()).
# In the list form: plain strings and evidence notes render inside the digested
# envelope; authored notes render in a visibly separate "### Authored narrative"
# subsection placed OUTSIDE the generated-block markers (after END, before the
# SME decision), labelled as authored by the task's agent and NOT digested.
# Migrating a task module from the legacy vector to the split list is the OWNING
# specialist's later work, not done here.
.render_note_item <- function(x) {
  if (is.list(x) && identical(x$kind, "evidence"))
    return(paste0(x$label, " (from ", x$expression_text, "): ", x$value_rendered))
  if (is.character(x)) return(x)
  stop("unsupported note item inside the digested envelope; authored notes ",
       "must be authored_note() and render outside the block", call. = FALSE)
}
.split_notes <- function(notes) {
  if (is.character(notes))
    return(list(envelope = notes, authored = character(0)))
  envelope <- character(0); authored <- character(0)
  for (n in notes) {
    if (is.list(n) && identical(n$kind, "authored")) {
      authored <- c(authored, n$text)
    } else {
      envelope <- c(envelope, unlist(lapply(list(n), .render_note_item)))
    }
  }
  list(envelope = envelope, authored = authored)
}

# .gate_canon2_digest() lives in tools/gate.R. tar_source() and the CI battery
# source both files (gate.R sorts first), but an ad-hoc render — Rscript -e
# 'source("tools/gate_package.R"); render_gate_package(...)' — used to die with
# ".gate_canon2_digest not found". Resolve the dependency lazily: when the
# function is missing, source gate.R from the conventional locations relative
# to the working directory, else stop with the actual instruction.
.gp_ensure_gate_tools <- function() {
  if (exists(".gate_canon2_digest", mode = "function") &&
      exists("parse_gate_record", mode = "function")) return(invisible(TRUE))
  for (cand in c("tools/gate.R", "gate.R")) {
    if (file.exists(cand)) {
      source(cand)
      return(invisible(TRUE))
    }
  }
  stop("render_gate_package needs tools/gate.R loaded (.gate_canon2_digest / ",
       "parse_gate_record not found, and tools/gate.R is not visible from the working ",
       "directory). source(\"tools/gate.R\") first, or render from the repo ",
       "root.", call. = FALSE)
}

# artifacts: data.frame(artifact, path, source)
# tables: named list of data.frames rendered under their names
# notes: legacy character vector, or a mixed list (see .split_notes above)
# Returns the gate file path invisibly. Re-rendering is idempotent: an
# existing marker pair is replaced in place; on first render the block is
# inserted just above "## SME decision" (or appended when that heading is
# absent). Lines outside the marker pair — SME decision, prose, signature —
# are never rewritten.
# attestation (round 7, F10): render_gate_from_store passes the store's
# `environment_attestation` value (list with fingerprint, nonmem_provision,
# r_provision); rendered as one digested line so the signed record names the
# environment it was produced on. NULL (direct renders, pre-F10 stores) omits it.
render_gate_package <- function(gate_file, artifacts, tables = list(),
                                notes = character(0), manifest_digest = NULL,
                                attestation = NULL) {
  .gp_ensure_gate_tools()
  lines <- readLines(gate_file, warn = FALSE)

  # Reject blank or duplicate table names up front: a duplicate "### <name>"
  # heading would make the digested envelope ambiguous.
  tnm <- names(tables)
  if (length(tables) && (is.null(tnm) || any(!nzchar(trimws(tnm)))))
    stop("every entry in `tables` must carry a non-blank name", call. = FALSE)
  if (anyDuplicated(tnm))
    stop(sprintf("duplicate table name(s): %s",
                 paste(unique(tnm[duplicated(tnm)]), collapse = ", ")),
         call. = FALSE)
  split <- .split_notes(notes)

  # REVOCATION SEMANTIC (finding C1): an SME approval binds to the package that
  # was present when it was signed. Silently rewriting the package under a live
  # approval would let content change while the signature stands. Refuse; a
  # regenerated package is a NEW record version and the approval does not carry
  # over. (An unsigned record — no APPROVED box marked — re-renders freely.)
  if (any(grepl("^\\s*-\\s*\\[[xX]\\]\\s*APPROVED", lines))) {
    stop("Refusing to rewrite ", gate_file, ": its SME decision is already ",
         "marked APPROVED. Regeneration of a signed gate requires a NEW record ",
         "version — the approval does not carry over to a changed package ",
         "(revocation semantic, finding C1).", call. = FALSE)
  }

  # Bind the artifact content into the package: a sha256 column computed from
  # each artifact file on disk at render time. The column participates in the
  # digest, so changing any artifact changes the digest the SME signs.
  artifacts <- as.data.frame(artifacts, stringsAsFactors = FALSE)
  if (nrow(artifacts) > 0L && !"path" %in% names(artifacts))
    stop("artifacts table malformed: non-empty frame requires a path column", call. = FALSE)
  path_col <- if ("path" %in% names(artifacts)) artifacts[["path"]]
              else rep(NA_character_, nrow(artifacts))
  artifacts$sha256 <- vapply(path_col, .gate_artifact_sha256, character(1))

  # Canonical envelope header (item 2): analysis_id and task are bound INTO the
  # digested block, derived from the record's own "Analysis ID:" / "Stage:"
  # lines so the same package cannot be re-signed under a different identity; a
  # single line, part of the digest (it survives .gate_canon2_payload, which
  # only drops the timestamp and the digest line).
  #
  # Batch 4 (item A): the identity comes from the ONE strict parser, never from
  # a grep of body lines, and a record whose identity is malformed (merged or
  # reflowed header, template residue, mismatch with the directory / file stem)
  # stops rendering before it can write an invalid envelope.
  # For a clean record the envelope text is byte-identical to the pre-batch-4
  # form, so no canon2 digest of an existing clean record changes.
  parsed <- parse_gate_record(gate_file)
  # Only the HEADER identity lines gate a render. Envelope-consistency problems
  # describe the OLD generated block, which this render replaces wholesale — a
  # record whose envelope is stale must remain repairable by re-rendering.
  idp <- grep("envelope", .gate_identity_problems(parsed), value = TRUE, invert = TRUE)
  if (length(idp) || is.na(parsed$analysis_id) || is.na(parsed$task)) {
    stop("Refusing to render ", gate_file, ": the record's identity fields are ",
         "malformed and would produce a garbage envelope:\n  - ",
         paste(idp, collapse = "\n  - "),
         "\nFix the 'Analysis ID:' / 'Stage:' lines (one field per line, no ",
         "template residue, matching the gates/<ID>/<task>.md path) and render again.",
         call. = FALSE)
  }
  envelope_header <- paste0("Envelope: analysis_id=", parsed$analysis_id,
                            "; task=", parsed$task)
  # Optional manifest-set binding (batch 4, item D): render_gate_from_store
  # passes the sha256 over the method manifests the pipeline ran under; it is
  # rendered as its own digested line right after the envelope. Direct renders
  # (tests, legacy callers) pass NULL and the block is byte-identical to before.
  manifest_line <- if (!is.null(manifest_digest) && nzchar(manifest_digest))
    c(paste0("Method-manifests: ", manifest_digest), "") else character(0)
  # Environment attestation binding (round 7, F10): fingerprint + the two
  # provision modes, and nothing else from the probe — no path, no harness.yaml
  # value beyond the provision, reaches the record.
  attestation_line <- character(0)
  if (!is.null(attestation)) {
    fp <- attestation$fingerprint
    if (!is.character(fp) || length(fp) != 1L || !grepl("^sha256:[0-9a-f]{64}$", fp))
      stop("render_gate_package: `attestation` must carry a sha256:<hex64> fingerprint",
           call. = FALSE)
    attestation_line <- c(sprintf("Environment-attestation: %s (nonmem provision: %s; R provision: %s)",
                                  fp, attestation$nonmem_provision %||% "?",
                                  attestation$r_provision %||% "?"), "")
  }

  block <- c(
    GATE_BLOCK_START,
    "",
    paste0("_Generated from tool output at ",
           format(Sys.time(), tz = "UTC", usetz = TRUE),
           "; values are read from fit and check objects, not transcribed._"),
    "",
    envelope_header,
    "",
    manifest_line,
    attestation_line,
    "### Artifacts",
    "",
    md_table(artifacts),
    ""
  )
  for (nm in names(tables)) {
    block <- c(block, paste0("### ", nm), "", md_table(tables[[nm]]), "")
  }
  if (length(split$envelope))
    block <- c(block, "### Notes", "", split$envelope, "")

  # canon2 digest (integrity batch 3b, item A2): the digest hashes the EXACT
  # RENDERED BYTES of this block (excluding the volatile timestamp line and the
  # digest line itself — see tools/gate.R::.gate_canon2_payload), so any holder
  # of the record can recompute it and require_gate/CI verify self-consistency.
  # The digest line is assembled with a placeholder first, then the digest is
  # computed over the FINAL block (the placeholder line is excluded from the
  # payload regardless of its content), then substituted in place — so the bytes
  # hashed here are byte-identical to what require_gate reads back.
  # .gate_canon2_digest comes from tools/gate.R, sourced alongside this file by
  # tar_source("tools") and by every test/CI harness.
  placeholder <- paste0(GATE_DIGEST_PREFIX, "canon2:PENDING")
  block <- c(block, placeholder, "", GATE_BLOCK_END)
  pkg_digest <- .gate_canon2_digest(block)
  block[block == placeholder] <- paste0(GATE_DIGEST_PREFIX, pkg_digest)

  # Authored narrative (item 3): rendered OUTSIDE the generated block, right
  # after END and before the SME decision, clearly labelled as agent-authored
  # and not part of the digested evidence. It is NOT hashed.
  authored_section <- if (length(split$authored))
    c("", "### Authored narrative",
      "",
      "_Authored by the task's agent; NOT part of the digested evidence above._",
      "", split$authored) else character(0)
  tail_block <- c(block, authored_section)

  i <- which(lines == GATE_BLOCK_START)
  j <- which(lines == GATE_BLOCK_END)
  if (length(i) == 1 && length(j) == 1 && j > i) {
    # Replace the block AND any prior authored-narrative subsection that
    # immediately follows it (up to the SME decision heading), so re-rendering
    # stays idempotent.
    k <- grep("^## SME decision", lines)
    if (!length(k) || !any(k > j))
      stop("Refusing to rewrite ", gate_file, ": no '## SME decision' heading follows the END marker",
           call. = FALSE)
    k <- min(k[k > j])
    tail_after <- if (k <= length(lines)) c("", lines[k:length(lines)]) else character(0)
    lines <- c(lines[seq_len(i - 1)], tail_block, tail_after)
  } else {
    # insert before the SME decision section on first render
    k <- grep("^## SME decision", lines)
    k <- if (length(k)) k[1] else length(lines) + 1
    lines <- append(lines, c(tail_block, ""), after = k - 1)
  }
  writeLines(lines, gate_file)
  invisible(gate_file)
}


# --- render from store (integrity batch 4, item C) ---------------------------
# THE documented render path. A gate package is rendered from the `<task>_package`
# target of the analysis store, and ONLY from a store whose package target and
# every upstream dependency are current. Rendering from a stale store created a
# code/value hybrid on 2026-09-02 (the review record bound the current
# comparator source hash to result tables produced by the previous comparator),
# so staleness is a refusal here, never a warning.
#
# Staleness is decided by targets::tar_outdated in a callr subprocess (targets
# forbids its own API inside a running target, and the analysis script sources
# the harness into the session). The always-cued gate targets (compose.R,
# item A3) would mark every downstream target outdated on every call by
# construction, so the check runs the pipeline with the gate targets' cue reset
# to "thorough" — they are the only targets exempted, and only for this
# staleness question. `check_store = FALSE` is for tests of the renderer alone.
#
# The manifest-set digest (item D) is the sha256 over the bytes of the
# `method_manifests` file target's paths as recorded in the store — the
# manifests the pipeline actually ran under — and is bound into the block.
.pkg_target_for <- function(task) paste0(gsub("-", "_", task), "_package")

# Returns list(outdated = character, missing = logical, pkg_target) from a
# subprocess. `outdated` lists the package target and any outdated upstream
# target (gate_* excluded as explained above); empty means fresh.
.store_staleness <- function(analysis_id, task,
                             script = file.path("analyses", paste0(analysis_id, ".R")),
                             store = paste0("_targets_", analysis_id)) {
  pkg <- .pkg_target_for(task)
  if (!file.exists(script))
    stop("analysis script not found: ", script, " (needed to decide whether the ",
         "store is current).", call. = FALSE)
  if (!dir.exists(store))
    stop("targets store not found: ", store, " (the analysis has not been run ",
         "here; a gate package can only be rendered/approved from executed results).",
         call. = FALSE)
  wrapper <- tempfile("staleness_", fileext = ".R")
  writeLines(c(
    sprintf("pipe <- source(%s, local = FALSE)$value", shQuote(normalizePath(script))),
    "pipe <- lapply(pipe, function(t) {",
    "  # Every always-cued target is re-cued to 'thorough' for this question, not",
    "  # only gate_*. tar_outdated marks an always-cued target (and so every",
    "  # dependent) outdated by construction; compose.R's environment_attestation",
    "  # is always-cued and every target names it, so a store a complete tar_make",
    "  # had just finished read as OUTDATED forever. Always-cued targets are",
    "  # re-executed by every tar_make and their value was attested in the run",
    "  # that just completed; staleness must ask whether the RESULTS are current,",
    "  # which the thorough cue answers.",
    "  if (grepl('^gate_', t$settings$name) || identical(t$cue$mode, 'always'))",
    "    t$cue <- targets::tar_cue(mode = 'thorough')",
    "  t })",
    "pipe"), wrapper)
  # NOTE (found at first live use, 2026-09-02): do NOT pass callr_function =
  # NULL here. Inside a fresh non-interactive session, targets then evaluates
  # the script so that functions brought in by the script's own tar_source()
  # (e.g. `%||%` from tools/) are not visible when compose_pipeline() runs —
  # "could not find function %||%". Letting targets spawn its own subprocess
  # (the default) evaluates the script the same way tar_make does.
  res <- callr::r(function(wrapper, store, pkg) {
    nm <- vapply(targets::tar_manifest(script = wrapper, fields = "name")$name,
                 identity, character(1))
    if (!pkg %in% nm) return(list(missing = TRUE, outdated = character(0)))
    out <- targets::tar_outdated(names = tidyselect::all_of(pkg), script = wrapper,
                                 store = store, branches = FALSE,
                                 reporter = "silent")
    list(missing = FALSE, outdated = sort(setdiff(out, grep("^gate_", out, value = TRUE))))
  }, args = list(wrapper, normalizePath(store), pkg), wd = getwd(),
  show = FALSE, spinner = FALSE)
  unlink(wrapper)
  res$pkg_target <- pkg
  res
}

# Stops unless the package target and its upstream are current.
require_fresh_store <- function(analysis_id, task, ...) {
  st <- .store_staleness(analysis_id, task, ...)
  if (isTRUE(st$missing))
    stop("no package target '", st$pkg_target, "' in the pipeline of ", analysis_id,
         call. = FALSE)
  if (length(st$outdated)) {
    shown <- paste(utils::head(st$outdated, 8L), collapse = ", ")
    more <- if (length(st$outdated) > 8L)
      sprintf(" (+%d more)", length(st$outdated) - 8L) else ""
    stop("store is OUTDATED for '", st$pkg_target, "' of ", analysis_id, ": ",
         shown, more, "\nFor an open analysis, refresh its store using its configured script and store paths. ",
         "Preserve a closed analysis store as frozen evidence. Do not rebuild, render or sign it again. ",
         "Render and approve gate packages from current execution only.",
         call. = FALSE)
  }
  invisible(TRUE)
}

# sha256 over the concatenated bytes of the manifest files (sorted paths), or
# NULL when the store carries no method_manifests target (pre-batch-4 store).
.manifest_set_digest <- function(paths) {
  missing <- paths[!file.exists(paths)]
  if (length(missing))
    stop("method manifest path(s) not found: ", paste(missing, collapse = ", "), call. = FALSE)
  paths <- sort(paths)
  if (!length(paths)) return(NULL)
  payload <- paste(vapply(paths, function(p)
    paste0(p, "\n", digest::digest(file = p, algo = "sha256"), "\n"), character(1)),
    collapse = "")
  paste0("sha256:", digest::digest(payload, algo = "sha256", serialize = FALSE))
}

render_gate_from_store <- function(analysis_id, task,
                                   store = paste0("_targets_", analysis_id),
                                   script = file.path("analyses", paste0(analysis_id, ".R")),
                                   # Same rule as pipeline/compose.R gate_file_for():
                                   # underscores in the task name are dashed in the
                                   # record file name (data_assembly -> data-assembly.md).
                                   gate_file = file.path("gates", analysis_id,
                                                         paste0(gsub("_", "-", task), ".md")),
                                   check_store = TRUE) {
  .gp_ensure_gate_tools()
  pkg <- .pkg_target_for(task)
  if (check_store) require_fresh_store(analysis_id, task, script = script, store = store)
  if (!dir.exists(store)) stop("targets store not found: ", store, call. = FALSE)
  obj <- callr::r(function(store, pkg) {
    have <- targets::tar_meta(store = store, fields = "name")$name
    has_mm <- "method_manifests" %in% have
    has_att <- "environment_attestation" %in% have
    list(pkg = targets::tar_read_raw(pkg, store = store),
         manifests = if (has_mm) targets::tar_read_raw("method_manifests", store = store)
                     else character(0),
         attestation = if (has_att) targets::tar_read_raw("environment_attestation", store = store)
                       else NULL)
  }, args = list(normalizePath(store), pkg), wd = getwd(), show = FALSE, spinner = FALSE)
  p <- obj$pkg
  if (!is.list(p) || is.null(p$artifacts))
    stop("target '", pkg, "' is not a gate package (expected list(artifacts, tables, notes))",
         call. = FALSE)
  render_gate_package(gate_file, p$artifacts, p$tables %||% list(),
                      p$notes %||% character(0),
                      manifest_digest = .manifest_set_digest(obj$manifests),
                      attestation = obj$attestation)
}
if (!exists("%||%", mode = "function")) `%||%` <- function(a, b) if (is.null(a)) b else a
