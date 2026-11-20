#!/usr/bin/env Rscript
# Continuous-integration battery for pmx-harness.
#
# Converts the "run the checks when someone remembers" habit into a single
# deterministic command. Each check prints one PASS / FAIL / WARN line; the
# process exits non-zero iff any check FAILs. WARN never fails the build —
# machine drift (r-package mismatches) is SME-disposition territory per
# CLAUDE.md, not a CI gate.
#
# Before a full run on a fresh checkout, generate the synthetic example inputs
# with Rscript --vanilla analyses/synthetic_example.R --generate. Use --light for
# static checks or --ledger-only for the installed kit preflight. The local ledger modes accept --initial-main for
# a genuine parentless main, or --ledger-base <commit SHA> for a chosen base.

# NOTE ON SOURCING: this file lives in tools/, and analysis pipelines call
# tar_source("tools"), which sources every .R here. It must therefore be SAFE
# TO SOURCE: all executable statements (working-directory change, the check
# run, exit) are guarded behind .invoked_as_script() so that sourcing only
# defines functions and does nothing observable. Running it as
# `Rscript tools/ci_check.R` sets the flag and executes the battery.

.invoked_as_script <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg) == 0) return(FALSE)
  grepl("ci_check\\.R$", sub("^--file=", "", file_arg[1]))
}

# --- result accounting ------------------------------------------------------
.fail_count <- 0L

report <- function(status, name, detail = "") {
  if (identical(status, "FAIL")) .fail_count <<- .fail_count + 1L
  line <- sprintf("%-4s %s", status, name)
  if (nzchar(detail)) line <- paste0(line, " -- ", detail)
  cat(line, "\n", sep = "")
}

# Git output goes through a binary file because system2(stdout = TRUE) reads
# lines and would normalize committed ledger bytes.
.ci_git_capture <- function(args, binary = FALSE, allow_failure = FALSE) {
  out <- tempfile("ci-git-")
  on.exit(unlink(out))
  status <- suppressWarnings(system2("git", shQuote(c("--no-replace-objects", args)),
                                     stdout = out, stderr = FALSE,
                                     env = "GIT_GRAFT_FILE=/dev/null", timeout = 30))
  if (allow_failure) return(status)
  if (status != 0L) stop("Git could not read ", args[[1L]], call. = FALSE)
  if (binary) readBin(out, "raw", n = file.info(out)$size) else readLines(out, warn = FALSE)
}

.ci_commit_id <- function(id) {
  if (!is.character(id) || length(id) != 1L || is.na(id) ||
      !grepl("^[0-9a-f]{40}$", id) || identical(id, strrep("0", 40)))
    stop("Expected an exact nonzero commit SHA", call. = FALSE)
  if (!identical(.ci_git_capture(c("cat-file", "-t", id)), "commit"))
    stop("Expected a readable commit object", call. = FALSE)
  id
}

.ci_commit_parents <- function(sha) {
  commit <- .ci_git_capture(c("cat-file", "-p", sha))
  separator <- match("", commit)
  if (is.na(separator) || separator < 2L || !grepl("^tree [0-9a-f]{40}$", commit[[1L]]))
    stop("Invalid commit headers", call. = FALSE)
  sub("^parent ", "", grep("^parent ", head(commit, separator - 1L), value = TRUE))
}

.ci_require_ancestor <- function(base, head) {
  status <- .ci_git_capture(c("merge-base", "--is-ancestor", base, head), allow_failure = TRUE)
  if (status != 0L) stop("Comparison commit is not a verified ancestor of HEAD", call. = FALSE)
}

.ci_resolve_ledger_base <- function(initial_main = FALSE, explicit_base = NULL) {
  if (!identical(.ci_git_capture(c("rev-parse", "--is-shallow-repository")), "false"))
    stop("Ledger comparison requires a complete, non-shallow checkout", call. = FALSE)
  head <- .ci_commit_id(.ci_git_capture(c("rev-parse", "--verify", "HEAD")))
  parents <- .ci_commit_parents(head)
  action <- Sys.getenv("GITHUB_EVENT_NAME")
  hosted <- identical(Sys.getenv("GITHUB_ACTIONS"), "true") || nzchar(action)
  if (hosted) {
    if (initial_main || !is.null(explicit_base))
      stop("Local ledger options are unavailable for GitHub events", call. = FALSE)
    if (!action %in% c("push", "pull_request") ||
        !identical(.ci_commit_id(Sys.getenv("GITHUB_SHA")), head))
      stop("Unsupported GitHub event or event SHA differs from HEAD", call. = FALSE)
    event_file <- Sys.getenv("GITHUB_EVENT_PATH")
    if (!.ci_regular_file(event_file)) stop("GitHub event payload unavailable", call. = FALSE)
    event <- jsonlite::fromJSON(event_file, simplifyVector = FALSE)
    ref <- Sys.getenv("GITHUB_REF")
    if (action == "push") {
      if (!identical(ref, "refs/heads/main") || !identical(event$ref, ref) ||
          !identical(event$after, head) || !identical(event$deleted, FALSE) ||
          !identical(event$forced, FALSE))
        stop("Invalid main push identity or flags", call. = FALSE)
      initial_main <- identical(event$before, strrep("0", 40)) && identical(event$created, TRUE)
      if (initial_main && !identical(event$repository$default_branch, "main"))
        stop("Initial push must create the default main branch", call. = FALSE)
      if (!initial_main && !identical(event$created, FALSE))
        stop("Invalid main push creation flag or before SHA", call. = FALSE)
      base <- event$before
      if (!initial_main && is.null(base)) stop("Missing push comparison base", call. = FALSE)
    } else {
      pr <- event$pull_request
      number <- as.character(event$number)
      if (length(number) != 1L || !grepl("^[1-9][0-9]*$", number) ||
          !identical(ref, paste0("refs/pull/", number, "/merge")) ||
          !identical(pr$base$ref, "main") || length(parents) < 2L)
        stop("Invalid pull request merge identity or target", call. = FALSE)
      base <- .ci_commit_id(pr$base$sha)
      pr_head <- .ci_commit_id(pr$head$sha)
      if (identical(pr_head, head)) stop("Pull request head is not the event merge commit", call. = FALSE)
      .ci_require_ancestor(pr_head, head)
    }
  } else {
    base <- explicit_base
    if (initial_main && !identical(.ci_git_capture(c("symbolic-ref", "-q", "HEAD")), "refs/heads/main"))
      stop("The local initial baseline requires checked-out main", call. = FALSE)
    if (!initial_main && is.null(base)) {
      if (!length(parents)) stop("Missing comparison base; use --initial-main for a new main", call. = FALSE)
      base <- parents[[1L]]
    }
  }
  if (initial_main) {
    if (length(parents)) stop("Initial main requires a parentless commit", call. = FALSE)
    return(list(head = head, initial = TRUE))
  }
  base <- .ci_commit_id(base)
  if (identical(base, head)) stop("Comparison base must precede HEAD", call. = FALSE)
  .ci_require_ancestor(base, head)
  list(head = head, base = base, initial = FALSE)
}

.CI_LEDGER_NAMES <- c("approvals.jsonl", "errata.jsonl")

.ci_gate_tree <- function(sha) {
  bytes <- .ci_git_capture(c("ls-tree", "-rz", "--full-tree", sha, "--", "gates"), binary = TRUE)
  end <- which(bytes == as.raw(0))
  start <- c(0L, head(end, -1L)) + 1L
  entries <- vapply(seq_along(end), function(i) rawToChar(bytes[start[i]:(end[i] - 1L)]), character(1))
  meta <- sub("\t.*$", "", entries)
  paths <- sub("^[^\t]*\t", "", entries)
  if (any(!grepl("^100(644|755) blob [0-9a-f]{40}$", meta)))
    stop("The tracked gates tree contains a nonregular entry", call. = FALSE)
  stats::setNames(sub("^.* ", "", meta), paths)
}

.ci_gate_tree_live <- function(path = "gates") {
  if (!length(path)) return(character(0))
  link <- Sys.readlink(path)
  if (any(!is.na(link) & nzchar(link))) stop("The live gates tree contains a symlink", call. = FALSE)
  dirs <- path[dir.exists(path)]
  leaves <- setdiff(path[file.exists(path)], dirs)
  # R's file_test("-f") accepts FIFOs. Use the actual POSIX regular-file test
  # before reading any leaf as a ledger or a gate record.
  if (any(!vapply(leaves, .ci_regular_file, logical(1))))
    stop("The live gates tree contains a nonregular entry", call. = FALSE)
  inner <- unlist(lapply(dirs, list.files, all.files = TRUE, no.. = TRUE, full.names = TRUE), use.names = FALSE)
  c(leaves, .ci_gate_tree_live(inner))
}

.ci_regular_file <- function(path) {
  identical(system2("test", c("-f", shQuote(path)), timeout = 5), 0L)
}

.ci_read_gate_blob <- function(oid) .ci_git_capture(c("cat-file", "blob", oid), binary = TRUE)
.ci_read_gate_file <- function(path) readBin(path, "raw", n = file.info(path)$size)

.ci_record_is_signed <- function(bytes, gate) {
  con <- textConnection(rawToChar(bytes))
  on.exit(close(con))
  lines <- readLines(con, warn = FALSE)
  fields <- gate$.gate_signature_fields(lines)
  !identical(gate$.gate_decision_of(lines), "unsigned") || !is.na(fields$signer) ||
    !is.na(fields$date) || nzchar(fields$digest_signed)
}

.ci_check_ledger_history <- function(initial_main = FALSE, explicit_base = NULL) {
  comparison <- .ci_resolve_ledger_base(initial_main, explicit_base)
  tracked <- .ci_gate_tree(comparison$head)
  live <- .ci_gate_tree_live()
  tracked_ledgers <- names(tracked)[basename(names(tracked)) %in% .CI_LEDGER_NAMES]
  live_ledgers <- live[basename(live) %in% .CI_LEDGER_NAMES]
  if (comparison$initial) {
    gate <- new.env()
    source(file.path("tools", "gate.R"), local = gate)
    signed_count <- function(paths, read) {
      records <- paths[grepl("\\.md$", paths)]
      sum(vapply(records, function(path) .ci_record_is_signed(read(path), gate), logical(1)))
    }
    tracked_signed <- signed_count(names(tracked), function(path) .ci_read_gate_blob(tracked[[path]]))
    live_signed <- signed_count(live, .ci_read_gate_file)
    counts <- sprintf("tracked/live approval ledgers %d/%d, errata ledgers %d/%d, signed records %d/%d",
                      sum(basename(tracked_ledgers) == "approvals.jsonl"),
                      sum(basename(live_ledgers) == "approvals.jsonl"),
                      sum(basename(tracked_ledgers) == "errata.jsonl"),
                      sum(basename(live_ledgers) == "errata.jsonl"), tracked_signed, live_signed)
    if (any(c(length(tracked_ledgers), length(live_ledgers), tracked_signed, live_signed) > 0L))
      stop("Initial main has production history: ", counts, call. = FALSE)
    report("PASS", "initial main ledger baseline", paste("no parent to compare;", counts))
    return(invisible(NULL))
  }

  base <- .ci_gate_tree(comparison$base)
  base_ledgers <- names(base)[basename(names(base)) %in% .CI_LEDGER_NAMES]
  prefix <- function(original, candidate) identical(original, head(candidate, length(original)))
  for (path in union(base_ledgers, tracked_ledgers)) {
    if (!path %in% tracked_ledgers || !path %in% live_ledgers)
      stop("Committed ledger path removed: ", path, call. = FALSE)
    committed <- .ci_read_gate_blob(tracked[[path]])
    if ((path %in% base_ledgers && !prefix(.ci_read_gate_blob(base[[path]]), committed)) ||
        !prefix(committed, .ci_read_gate_file(path)))
      stop("Committed ledger byte prefix edited: ", path, call. = FALSE)
  }
  report("PASS", "approval and errata ledger append-only",
         sprintf("base %s; %d committed or live ledger path(s)", substr(comparison$base, 1L, 10L),
                 length(union(tracked_ledgers, live_ledgers))))
}

# Collect the R sources under the authored trees. pilot/ is included in the
# parse + stray-tag inventory (the review flagged its earlier exclusion) but
# NOT in composition — pilot analyses are historical and not re-composed.
r_sources <- function() {
  dirs <- c("pipeline", "tools", "analyses", "pilot",
            list.dirs("skills", recursive = FALSE, full.names = TRUE))
  files <- character(0)
  for (d in dirs) {
    if (!dir.exists(d)) next
    # For skills/*, restrict to reference/ subdirs; other dirs: all .R.
    if (startsWith(d, "skills")) {
      ref <- file.path(d, "reference")
      if (dir.exists(ref)) {
        files <- c(files, list.files(ref, pattern = "\\.R$", full.names = TRUE, recursive = TRUE))
      }
    } else {
      files <- c(files, list.files(d, pattern = "\\.R$", full.names = TRUE, recursive = TRUE))
    }
  }
  files <- unique(files)
  # Engine-generated PsN plot scripts land inside pilot run directories
  # (modelfit_dir1/, vpc_dir1/, bootstrap_dir1/). They are tool OUTPUT — their
  # auto-generated preamble carries template placeholders that are not valid R
  # and are overwritten on every run, so they are not authored harness code and
  # are excluded from the parse/stray inventory. The marker is content-based so
  # it generalises across PsN tools and directory names.
  keep <- vapply(files, function(f) {
    first <- tryCatch(readLines(f, n = 1, warn = FALSE), error = function(e) "")
    !length(first) || !grepl("AUTO-GENERATED PREAMBLE", first[1], fixed = TRUE)
  }, logical(1))
  files[keep]
}

# --- Check 1: parse every R source -----------------------------------------
check_parse <- function() {
  bad <- character(0)
  files <- r_sources()
  if (!length(files)) {
    report("FAIL", "R parse", "0 authored .R files found")
    return(invisible(NULL))
  }
  for (f in files) {
    ok <- tryCatch({ parse(file = f); TRUE },
                   error = function(e) { attr(bad, "e") <<- conditionMessage(e); FALSE })
    if (!isTRUE(ok)) bad <- c(bad, f)
  }
  if (length(bad) == 0) {
    report("PASS", sprintf("R parse (%d authored .R files)", length(files)))
  } else {
    report("FAIL", "R parse", paste(basename(bad), collapse = ", "))
  }
}

# --- Check 2: stray tool-call closing tags in R tails -----------------------
# Historical defect: a generation artifact left a literal tool-call closing tag
# at the end of an R file. The two target literals are assembled from pieces so
# this very file does not trip its own scan.
check_stray_tags <- function() {
  tag_a <- paste0("</", "invoke>")
  tag_b <- paste0("</", "content>")
  hits <- character(0)
  files <- r_sources()
  for (f in files) {
    txt <- tryCatch(readLines(f, warn = FALSE), error = function(e) character(0))
    if (any(grepl(tag_a, txt, fixed = TRUE)) || any(grepl(tag_b, txt, fixed = TRUE))) {
      hits <- c(hits, f)
    }
  }
  if (length(hits) == 0) {
    report("PASS", sprintf("no stray tool-call closing tags (%d files)", length(files)))
  } else {
    report("FAIL", "stray tool-call closing tags", paste(basename(hits), collapse = ", "))
  }
}

# --- Check 3: gate-record marker integrity ----------------------------------
# Marker constants come from tools/gate_package.R so the check can never drift
# from the renderer. They are read out with a plain grep rather than sourcing
# (sourcing pulls package deps); the constants are simple strings.
#
# Integrity invariant: a gate record carries EITHER no generated block (a
# hand-authored record predating render_gate_package, or one never rendered) OR
# exactly one well-formed BEGIN..END pair in order. The failure class this
# guards is a MANGLED block: an unbalanced marker, a duplicated block, or a
# crossed pair. Absence of a block is not corruption, so zero markers passes;
# only 1-vs-1-ordered or 0-vs-0 are accepted, everything else FAILs.
gate_markers <- function() {
  gp <- file.path("tools", "gate_package.R")
  txt <- readLines(gp, warn = FALSE)
  pick <- function(name) {
    ln <- grep(sprintf("^%s\\s*<-", name), txt, value = TRUE)[1]
    sub('.*"(.*)".*', "\\1", ln)
  }
  list(start = pick("GATE_BLOCK_START"), end = pick("GATE_BLOCK_END"))
}

# Live gate records: every gates/**/*.md EXCEPT the content-addressed archives
# under gates/<ID>/history/ (round 6, finding 11). An archived record holds the
# exact bytes of a RELEASED signature; it is history, not a live gate, and must
# not be inventoried as a signed record, digest-bound, or reported as
# unledgered. The live record at gates/<ID>/<task>.md is the only one that
# governs.
gate_record_files <- function() {
  f <- list.files("gates", pattern = "\\.md$", full.names = TRUE, recursive = TRUE)
  f[!grepl("(^|/)history/", f)]
}

check_gate_markers <- function() {
  mk <- gate_markers()
  if (!exists(".GATE_RE", inherits = TRUE)) source(file.path("tools", "gate.R"), local = TRUE)
  if (!identical(mk$start, .GATE_RE$block_start) || !identical(mk$end, .GATE_RE$block_end)) {
    report("FAIL", "gate-marker integrity", "gate_package.R marker constants do not match gate.R")
    return(invisible(NULL))
  }
  gate_files <- gate_record_files()
  bad <- character(0)
  for (f in gate_files) {
    txt <- readLines(f, warn = FALSE)
    starts <- which(txt == mk$start)
    ends <- which(txt == mk$end)
    ns <- length(starts); ne <- length(ends)
    ok <- (ns == 0 && ne == 0) || (ns == 1 && ne == 1 && starts < ends)
    if (!ok) bad <- c(bad, f)
  }
  if (length(bad) == 0) {
    report("PASS", sprintf("gate-marker integrity (%d records)", length(gate_files)))
  } else {
    report("FAIL", "gate-marker integrity",
           paste(sprintf("%s/%s", basename(dirname(bad)), basename(bad)), collapse = ", "))
  }
}

# --- Check 4: composition (tar_manifest, no execution) ----------------------
check_composition <- function() {
  ok_all <- TRUE
  detail <- character(0)
  analyses <- list.files("analyses", pattern = "\\.R$", full.names = TRUE)
  for (a in analyses) {
    # tar_manifest reads the pipeline definition only (no execution, no store
    # writes), so no store argument is needed or accepted by this targets API.
    res <- tryCatch({
      targets::tar_manifest(script = a)
      TRUE
    }, error = function(e) { detail <<- c(detail, sprintf("%s: %s", basename(a), conditionMessage(e))); FALSE })
    if (!isTRUE(res)) ok_all <- FALSE
  }
  if (ok_all) {
    report("PASS", sprintf("composition (%d analyses; tar_manifest per analysis)", length(analyses)))
  } else {
    report("FAIL", "composition", paste(detail, collapse = " | "))
  }
}

# --- Check 5: gate dominance (structural invariant, finding C2) --------------
# Composes each analysis pipeline and asserts verify_gate_dominance():
# every non-gate stage target that follows a task's gate must reference its
# preceding gate symbol in its command, so upstream approval dominates by
# DAG construction and not by a task module voluntarily wrapping gated().
# Run in a callr subprocess: sourcing an analysis in-process would re-enter
# tar_source("tools") -> this file and re-trigger the battery (this process
# was launched with --file=ci_check.R). Full mode only — it needs the
# composer, targets, and each analysis's setup.
check_gate_dominance <- function() {
  ok_all <- TRUE
  detail <- character(0)
  analyses <- list.files("analyses", pattern = "\\.R$", full.names = TRUE)
  for (a in analyses) {
    res <- tryCatch(
      callr::r(function(analysis) {
        env  <- new.env(parent = globalenv())
        pipe <- source(analysis, local = env)$value
        # verify_gate_dominance is defined by pipeline/compose.R, which the
        # analysis sources into the subprocess global environment.
        verify_gate_dominance(pipe)
        TRUE
      }, args = list(normalizePath(a)), wd = getwd()),
      error = function(e) {
        detail <<- c(detail, sprintf("%s: %s", basename(a), conditionMessage(e)))
        FALSE
      })
    if (!isTRUE(res)) ok_all <- FALSE
  }
  if (ok_all) {
    report("PASS", sprintf("gate dominance (%d analyses; every stage target chained to its gate)", length(analyses)))
  } else {
    report("FAIL", "gate dominance", paste(detail, collapse = " | "))
  }
}

# --- Check 6: self-gate consistency -----------------------------------------
# For every skill claiming a real 'Reviewed by:' signature, any reference/*.R
# it carries must parse. A signed GUIDANCE-only skill legitimately has no
# reference/ directory (its method is prose, not code) — that is not an
# inconsistency, so the check only asserts on reference code that exists.
check_self_gate <- function() {
  skill_docs <- list.files("skills", pattern = "^SKILL.*\\.md$",
                           full.names = TRUE, recursive = TRUE)
  bad <- character(0)
  n_refs <- 0L
  for (doc in skill_docs) {
    txt <- readLines(doc, warn = FALSE)
    signed <- any(grepl("^Reviewed by:\\s*\\S", txt))
    if (!signed) next
    ref_dir <- file.path(dirname(doc), "reference")
    refs <- if (dir.exists(ref_dir)) list.files(ref_dir, pattern = "\\.R$", full.names = TRUE) else character(0)
    # Guidance-only signed skill (no reference code) is consistent by definition.
    if (length(refs) == 0) next
    n_refs <- n_refs + length(refs)
    for (r in refs) {
      ok <- tryCatch({ parse(file = r); TRUE }, error = function(e) FALSE)
      if (!isTRUE(ok)) bad <- c(bad, sprintf("%s (ref %s unparseable)", doc, basename(r)))
    }
  }
  if (length(bad) == 0) {
    report("PASS", sprintf("self-gate consistency (%d signed-skill reference files)", n_refs))
  } else {
    report("FAIL", "self-gate consistency", paste(bad, collapse = " | "))
  }
}

# --- Check 6: r-packages lock (WARN, never FAIL) ----------------------------
check_r_lock <- function() {
  res <- tryCatch({
    source(file.path("tools", "r_lock.R"), local = new.env())
  }, error = function(e) e)
  if (inherits(res, "error")) {
    report("WARN", "r-packages lock", paste("could not source r_lock.R:", conditionMessage(res)))
    return(invisible())
  }
  env <- new.env(); source(file.path("tools", "r_lock.R"), local = env)
  mism <- tryCatch(env$verify_r_packages(), error = function(e) e)
  if (inherits(mism, "error")) {
    report("WARN", "r-packages lock", paste("verify_r_packages() error:", conditionMessage(mism)))
  } else if (length(mism) == 0 || (is.data.frame(mism) && nrow(mism) == 0)) {
    report("PASS", "r-packages lock (installed matches manifest)")
  } else {
    n <- if (is.data.frame(mism)) nrow(mism) else length(mism)
    report("WARN", "r-packages lock", sprintf("%d mismatch(es) for SME disposition", n))
  }
}

# --- Check 7: yaml validity -------------------------------------------------
check_yaml <- function() {
  targets_yaml <- c("config/engine.yaml", "harness.yaml.example")
  bad <- character(0)
  for (y in targets_yaml) {
    if (!file.exists(y)) { bad <- c(bad, sprintf("%s (missing)", y)); next }
    ok <- tryCatch({ yaml::read_yaml(y); TRUE }, error = function(e) { bad <<- c(bad, sprintf("%s: %s", y, conditionMessage(e))); FALSE })
  }
  if (length(bad) == 0) {
    report("PASS", "yaml validity (engine.yaml, harness.yaml.example)")
  } else {
    report("FAIL", "yaml validity", paste(bad, collapse = " | "))
  }
}

# --- Check 8: method-manifest v2 schema + digests match disk -----------------
# Every skills/*/MANIFEST.yaml passes validate_method_manifest() (round 7,
# finding 5): strict parse (no duplicate or unknown fields, no duplicate
# units), skill == directory, reference_path present and under the skill, every
# entry point a top-level function of that file (or the file name for a non-R
# template), sha256 grammar + equality with disk, reviewed_at an ISO date once
# reviewed_by is filled, each entry point in exactly one unit, and every
# per-unit depends: entry present and matching. FAIL names the manifest and
# the validator's own message. Full mode only.
check_manifest_digests <- function() {
  source(file.path("tools", "method_manifest.R"), local = TRUE)
  manifests <- list.files("skills", pattern = "^MANIFEST\\.yaml$",
                          full.names = TRUE, recursive = TRUE)
  bad <- character(0); n_units <- 0L
  for (mf in manifests) {
    skill <- basename(dirname(mf))
    m <- tryCatch(validate_method_manifest(mf), error = function(e) e)
    if (inherits(m, "error")) {
      bad <- c(bad, sprintf("%s: %s", skill, conditionMessage(m)))
      next
    }
    n_units <- n_units + length(m$units)
  }
  if (length(bad) == 0) {
    report("PASS", sprintf("method-manifest v2 schema + digests (%d manifest(s), %d unit(s))",
                           length(manifests), n_units))
  } else {
    report("FAIL", "method-manifest v2 schema + digests", paste(bad, collapse = " | "))
  }
}

# --- Check 8a: every manifested entry point calls its skill gate -------------
# Static audit (Codex round 8, finding 2): the manifest binds bytes and unit
# existence; audit_entrypoint_gating() parses each reference file and requires
# every declared entry point's body to call the skill's gate (a function that
# reaches require_validated_method). An ungated entry point of an APPROVED
# unit is a FAIL (the audit stops); of a draft unit a WARN.
check_entrypoint_gating <- function() {
  source(file.path("tools", "method_manifest.R"), local = TRUE)
  manifests <- list.files("skills", pattern = "^MANIFEST\\.yaml$",
                          recursive = TRUE, full.names = TRUE)
  bad <- character(0); warned <- character(0); n_eps <- 0L
  for (mf in manifests) {
    r <- tryCatch(
      withCallingHandlers(audit_entrypoint_gating(dirname(mf)),
                          warning = function(w) {
                            warned <<- c(warned, conditionMessage(w))
                            invokeRestart("muffleWarning")
                          }),
      error = function(e) e)
    if (inherits(r, "error")) bad <- c(bad, conditionMessage(r))
    else n_eps <- n_eps + nrow(r)
  }
  if (length(warned))
    report("WARN", "entry-point gating (draft units with ungated entry points)",
           paste(warned, collapse = " | "))
  if (length(bad) == 0) {
    report("PASS", sprintf("entry-point gating (%d manifested entry point(s) all call their gate)",
                           n_eps))
  } else {
    report("FAIL", "entry-point gating (approved units with ungated entry points)",
           paste(bad, collapse = " | "))
  }
}

# --- Check 8b: every reference/*.R is covered by a manifest unit -------------
# A reference file with no covering manifest unit is an UNGATED computing lane —
# exactly the overbroad/ungated surface integrity batch 3a closes. FAIL and name
# any reference file that no unit's reference_path points at. Full mode only.
check_manifest_coverage <- function() {
  source(file.path("tools", "method_manifest.R"), local = TRUE)
  manifests <- list.files("skills", pattern = "^MANIFEST\\.yaml$",
                          full.names = TRUE, recursive = TRUE)
  covered <- character(0)
  for (mf in manifests) {
    m <- tryCatch(read_method_manifest(mf), error = function(e) NULL)
    if (is.null(m)) next
    for (u in m$units) covered <- c(covered, u$reference_path)
  }
  covered <- unique(covered)
  refs <- list.files("skills", pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
  refs <- refs[grepl("/reference/", refs)]
  refs <- vapply(refs, .mm_relpath, character(1), USE.NAMES = FALSE)
  uncovered <- setdiff(refs, covered)
  if (length(uncovered) == 0) {
    report("PASS", sprintf("manifest coverage (%d reference file(s) all covered)",
                           length(refs)))
  } else {
    report("FAIL", "manifest coverage (uncovered reference files)",
           paste(uncovered, collapse = ", "))
  }
}

# --- Check 9: signed-gate digest binding + legacy allowlist (batch 3a) --------
# For every gate record carrying an APPROVED mark:
#   * WITH a Package-Digest line: the 'Digest signed:' field must equal it (FAIL
#     on mismatch), as before.
#   * WITHOUT a Package-Digest line (digest-less): it passes ONLY when it is on
#     the frozen legacy allowlist (tools/legacy_gates.yaml) AND its bytes still
#     hash to the frozen value. An off-list digest-less record, or an allowlisted
#     record whose hash drifted, is now a FAIL (was a WARN inventory before) —
#     this closes the digest-strip downgrade. On-list matches report as PASS
#     inventory.
check_gate_digest_binding <- function() {
  source(file.path("tools", "gate.R"), local = TRUE)
  entries <- .read_legacy_gates()
  gate_files <- gate_record_files()
  onlist <- character(0); bad <- character(0)
  for (f in gate_files) {
    lines <- readLines(f, warn = FALSE)
    parsed <- parse_gate_record(f)   # ONE parser (batch 4, item A)
    if (!parsed$decision %in% .LEDGER_APPROVING) next
    pkg_line <- if (is.na(parsed$package_digest)) character(0) else parsed$package_digest
    id <- sprintf("%s/%s", basename(dirname(f)), basename(f))
    entry <- .legacy_entry_for(f, entries)
    # Shared LIVE ARTIFACT REHASH + canon2 self-recompute (batch 3b, item A4):
    # the same .gate_verify_content require_gate runs, so CI and the runtime
    # gate never diverge. Skipped for ALLOWLISTED records exactly as require_gate
    # skips them: allowlist/canon1 short-circuits BEFORE the rehash there, the
    # frozen whole-file sha256 being their protection (a closed pilot's rendered
    # artifacts may legitimately have moved on). Off-list records are rehashed.
    if (is.null(entry)) {
      vprob <- .gate_verify_content(f, lines)
      if (length(vprob))
        bad <- c(bad, sprintf("%s (%s)", id, paste(vprob, collapse = "; ")))
    }
    if (length(pkg_line) == 0) {
      # digest-less: must be an allowlisted (path, sha256) match.
      if (is.null(entry)) {
        bad <- c(bad, sprintf("%s (digest-less, off allowlist)", id))
      } else if (!identical(entry$sha256, .gate_sha256(f))) {
        bad <- c(bad, sprintf("%s (allowlisted but hash drifted)", id))
      } else {
        onlist <- c(onlist, id)
      }
      next
    }
    # WITH digest: an allowlisted record whose bytes drifted is still a FAIL.
    if (!is.null(entry) && !identical(entry$sha256, .gate_sha256(f))) {
      bad <- c(bad, sprintf("%s (allowlisted but hash drifted)", id)); next
    }
    pkg <- parsed$package_digest
    val <- parsed$digest_signed
    if (!nzchar(val) || !identical(val, pkg))
      bad <- c(bad, id)
  }
  if (length(bad) == 0) {
    report("PASS", sprintf("signed-gate digest binding + legacy allowlist (%d records)", length(gate_files)))
  } else {
    report("FAIL", "signed-gate digest binding + legacy allowlist",
           paste(bad, collapse = ", "))
  }
  if (length(onlist)) {
    report("PASS", sprintf("legacy allowlist inventory (%d on-list digest-less record(s))",
                           length(onlist)))
  }
}

# --- Check 9b: legacy-allowlist pin (integrity batch 3b, item C3) ------------
# The stock public release carries no legacy trust entries.
# Compare the ordered kind, path and digest triples to catch trust changes.
EXPECTED_ALLOWLIST <- character(0)
check_allowlist_pin <- function() {
  source(file.path("tools", "gate.R"), local = TRUE)
  entries <- tryCatch(.read_legacy_gates(), error = function(e) e)
  if (inherits(entries, "error")) {
    report("FAIL", "legacy-allowlist pin (allowlist does not parse)",
           conditionMessage(entries))
    return(invisible(NULL))
  }
  got <- vapply(entries, function(e) paste0(e$kind, "\t", e$path, "\t", e$sha256),
                character(1))
  # ORDERED identity of the full triple vector: catches additions, removals,
  # duplicates, reorders and any sha256 change alike (the parser refuses
  # duplicates outright, so this is the second of two independent layers).
  if (identical(got, EXPECTED_ALLOWLIST)) {
    report("PASS", sprintf("legacy-allowlist pin (%d entries as pinned, kind+path+sha256 in order)",
                           length(EXPECTED_ALLOWLIST)))
    return(invisible(NULL))
  }
  show <- function(x) sub("\t", " ", sub("\t(sha256:[0-9a-f]{8})[0-9a-f]*$", " \\1…", x))
  kp <- function(x) sub("\t[^\t]*$", "", x)
  dups    <- unique(got[duplicated(got)])
  added   <- setdiff(got, EXPECTED_ALLOWLIST)
  removed <- setdiff(EXPECTED_ALLOWLIST, got)
  # An entry whose (kind, path) is pinned but whose sha256 differs is a
  # re-digested legacy record: name it as such.
  retag <- intersect(kp(added), kp(removed))
  added   <- added[!kp(added) %in% retag]
  removed <- removed[!kp(removed) %in% retag]
  detail <- c(if (length(retag)) paste0("sha256 changed for pinned entry: ",
                paste(sub("\t", " ", retag), collapse = ", ")),
              if (length(dups)) paste0("duplicated: ", paste(show(dups), collapse = ", ")),
              if (length(added)) paste0("unexpected: ", paste(show(added), collapse = ", ")),
              if (length(removed)) paste0("missing: ", paste(show(removed), collapse = ", ")))
  if (length(detail) == 0) {
    detail <- if (length(got) != length(EXPECTED_ALLOWLIST))
      sprintf("entry count %d does not match the pinned %d", length(got), length(EXPECTED_ALLOWLIST))
    else sprintf("entries reordered (first difference at position %d: %s)",
                 which(got != EXPECTED_ALLOWLIST)[1], show(got[which(got != EXPECTED_ALLOWLIST)[1]]))
  }
  report("FAIL", "legacy-allowlist pin (SME act — update the pin in the same change)",
         paste(detail, collapse = " | "))
}

# --- Check 10: binding semantics exercised on fixtures (findings C1) ----------
# Builds throwaway records in a tempdir and asserts the two invariants that the
# whole batch exists to enforce: (a) render_gate_package refuses to rewrite an
# APPROVED record and require_gate refuses a package whose digest binding does
# not match; (b) a legacy record with no digest line still passes. FAIL if any
# invariant does not hold.
check_binding_semantics <- function() {
  ok <- TRUE; why <- character(0)
  td <- tempfile("cibind"); dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  wd0 <- setwd(td); on.exit(setwd(wd0), add = TRUE)
  tools <- file.path(wd0, "tools")
  env <- new.env()
  source(file.path(tools, "gate_package.R"), local = env)
  source(file.path(tools, "gate.R"), local = env)

  writeLines("x", "a.csv")
  arts <- data.frame(artifact = "t", path = "a.csv", source = "s",
                     stringsAsFactors = FALSE)

  # Records live at gates/CI-001/<task>.md so the strict parser's identity
  # check (Analysis ID vs directory, Stage vs file stem) is satisfied.
  dir.create(file.path("gates", "CI-001"), recursive = TRUE)
  rec <- file.path("gates", "CI-001", "r.md")
  writeLines(c("# r", "", "Analysis ID: CI-001", "Stage: r", "", "## SME decision", "",
               "- [ ] APPROVED", "Digest signed: <!-- -->", "Signed: <!-- -->",
               "Date: <!-- -->"),
             rec)
  env$render_gate_package(rec, arts)
  ln <- readLines(rec)
  dg <- sub("^Package-Digest: ", "", grep("^Package-Digest:", ln, value = TRUE)[1])

  # (a1) signed + matching digest -> require_gate passes
  # A signed sibling record is rendered under its own stem (Stage: good) so the
  # strict identity check and the digested envelope agree.
  good_rec <- file.path("gates", "CI-001", "good.md")
  writeLines(c("# r", "", "Analysis ID: CI-001", "Stage: good", "", "## SME decision", "",
               "- [ ] APPROVED", "Digest signed: <!-- -->", "Signed: <!-- -->",
               "Date: <!-- -->"), good_rec)
  env$render_gate_package(good_rec, arts)
  good <- readLines(good_rec)
  dg <- sub("^Package-Digest: ", "", grep("^Package-Digest:", good, value = TRUE)[1])
  good[grep("^- \\[ \\] APPROVED", good)] <- "- [x] APPROVED"
  good[grep("^Digest signed:", good)] <- paste("Digest signed:", dg)
  good[grep("^Signed:", good)] <- "Signed: Local Fixture Reviewer"
  good[grep("^Date:", good)] <- "Date: 2026-09-02"
  writeLines(good, good_rec)
  r <- tryCatch({ suppressMessages(env$require_gate(good_rec)); TRUE },
                error = function(e) FALSE)
  if (!isTRUE(r)) { ok <- FALSE; why <- c(why, "matching-digest record refused") }

  # (a2) render refuses to rewrite an approved record
  r <- tryCatch({ env$render_gate_package(good_rec, arts); FALSE },
                error = function(e) TRUE)
  if (!isTRUE(r)) { ok <- FALSE; why <- c(why, "render did not refuse approved record") }

  # (a3) digest mismatch -> require_gate refuses
  bad <- good; bad[grep("^Digest signed:", bad)] <- "Digest signed: sha256:dead"
  bad[grep("^Stage:", bad)] <- "Stage: bad"
  writeLines(bad, file.path("gates", "CI-001", "bad.md"))
  r <- tryCatch({ suppressMessages(env$require_gate(file.path("gates", "CI-001", "bad.md"))); FALSE },
                error = function(e) TRUE)
  if (!isTRUE(r)) { ok <- FALSE; why <- c(why, "mismatched-digest record not refused") }

  # (b) digest-less record OFF the allowlist -> refused (digest-strip downgrade
  # defence; batch 3a). No legacy_gates.yaml exists in this tempdir, so nothing
  # is allowlisted and the record must be refused.
  writeLines(c("# r", "", "## SME decision", "", "- [x] APPROVED", "Signed: Local Fixture Reviewer",
               "Date: 2026-09-02"),
             "legacy.md")
  r <- tryCatch({ suppressMessages(env$require_gate("legacy.md")); FALSE },
                error = function(e) TRUE)
  if (!isTRUE(r)) { ok <- FALSE; why <- c(why, "off-list digest-less record not refused") }

  # (b2) digest-less record ON a frozen allowlist -> passes.
  dir.create("tools", showWarnings = FALSE)
  h <- paste0("sha256:", digest::digest(file = "legacy.md", algo = "sha256"))
  writeLines(c("entries:", "  - kind: digestless", "    path: legacy.md",
               paste0("    sha256: ", h)), file.path("tools", "legacy_gates.yaml"))
  r <- tryCatch({ suppressMessages(env$require_gate("legacy.md")); TRUE },
                error = function(e) FALSE)
  if (!isTRUE(r)) { ok <- FALSE; why <- c(why, "on-list digest-less record refused") }

  if (ok) report("PASS", "binding semantics (render refusal + require_gate binding)")
  else report("FAIL", "binding semantics", paste(why, collapse = " | "))
}

# Gate approval checks are independent of Git history and also run in the kit.
check_approvals_ledger <- function() {
  source(file.path("tools", "gate.R"), local = TRUE)
  source(file.path("tools", "approve_gate.R"), local = TRUE)
  result <- verify_approvals_ledger("gates")
  if (!result$ok) stop("Approval chain: ", paste(result$problems, collapse = " | "), call. = FALSE)
  ledgers <- list.files("gates", pattern = "^approvals\\.jsonl$", full.names = TRUE, recursive = TRUE)
  records <- gate_record_files()
  entries <- .read_legacy_gates()
  for (path in records) {
    p <- parse_gate_record(path)
    if (!p$decision %in% .LEDGER_APPROVING || !is.null(.legacy_entry_for(path, entries))) next
    latest <- .ledger_latest_for_task(.read_ledger(.ledger_path(path)), .gate_task_key(path))
    if (is.null(latest) || identical(latest$decision, "release") ||
        !identical(as.character(latest$record_digest), .gate_sha256(path)))
      stop("Signed gate lacks a matching latest ledger event: ", path, call. = FALSE)
  }
  uncommitted <- 0L
  for (path in ledgers) {
    events <- .read_ledger(path)
    uncommitted <- uncommitted + sum(vapply(events, function(event)
      identical(event$package_revision, "UNCOMMITTED"), logical(1)))
    tasks <- unique(vapply(events, function(event) as.character(event$task), character(1)))
    for (task in tasks) {
      latest <- .ledger_latest_for_task(events, task)
      if (!latest$decision %in% .LEDGER_APPROVING) next
      record <- file.path(dirname(path), paste0(task, ".md"))
      if (!record %in% records) stop("Latest approval is missing its gate record: ", path, call. = FALSE)
      p <- parse_gate_record(record)
      if (!identical(p$decision, latest$decision) ||
          !identical(as.character(latest$record_digest), .gate_sha256(record)) ||
          !identical(as.character(latest$package_digest), p$package_digest) ||
          !identical(p$digest_signed, p$package_digest) ||
          !identical(as.character(latest$artifact_manifest_digest), .gate_artifact_manifest_digest(p$block)) ||
          !identical(as.character(latest$signer), p$signer) ||
          !identical(as.character(latest$signed_at), p$date))
        stop("Latest approval does not bind the current gate: ", record, call. = FALSE)
    }
  }
  report("PASS", sprintf("approval-ledger chains and latest bindings (%d ledger file(s), %d event(s))",
                         result$ledger_count, result$event_count))
  report(if (uncommitted > 0L) "WARN" else "PASS",
         sprintf("approval package revision inventory (%d of %d events recorded before commit)",
                 uncommitted, result$event_count))
}

# Errata can bind a historical approved version even after it was released.
check_errata_ledger <- function() {
  source(file.path("tools", "gate.R"), local = TRUE)
  source(file.path("tools", "approve_gate.R"), local = TRUE)
  source(file.path("tools", "errata.R"), local = TRUE)
  result <- verify_errata_ledger("gates")
  if (!result$ok) stop("Errata chain: ", paste(result$problems, collapse = " | "), call. = FALSE)
  ledgers <- list.files("gates", pattern = "^errata\\.jsonl$", full.names = TRUE, recursive = TRUE)
  entries <- .read_legacy_gates()
  for (path in ledgers) {
    approvals <- .read_ledger(file.path(dirname(path), "approvals.jsonl"))
    for (erratum in .read_ledger(path)) {
      record <- file.path(dirname(path), basename(erratum$gate_path))
      task <- sub("\\.md$", "", basename(record))
      legacy <- .legacy_entry_for(record, entries)
      bound <- (!is.null(legacy) && identical(legacy$sha256, erratum$record_digest)) ||
        any(vapply(approvals, function(approval) {
          identical(approval$task, task) && approval$decision %in% .LEDGER_APPROVING &&
            identical(approval$record_digest, erratum$record_digest)
        }, logical(1)))
      if (!bound) stop("Erratum does not bind an approved gate version: ", record, call. = FALSE)
    }
  }
  report("PASS", sprintf("errata-ledger chains and approval bindings (%d ledger file(s), %d event(s))",
                         result$ledger_count, result$event_count))
}

check_ledger_preflight <- function(initial_main = FALSE, explicit_base = NULL) {
  tryCatch({
    .ci_check_ledger_history(initial_main, explicit_base)
    check_approvals_ledger()
    check_errata_ledger()
  }, error = function(e) report("FAIL", "production ledger preflight", conditionMessage(e)))
}

# Exercise the public verifier CLIs on disposable nonempty data. The library
# counts guard against a zero-event success, including the errata CLI's summary.
check_ledger_cli_fixtures <- function() {
  tools <- normalizePath("tools")
  source(file.path(tools, "gate.R"), local = TRUE)
  source(file.path(tools, "gate_package.R"), local = TRUE)
  source(file.path(tools, "approve_gate.R"), local = TRUE)
  source(file.path(tools, "errata.R"), local = TRUE)
  fixture <- tempfile("ci-ledger-")
  dir.create(fixture)
  original <- setwd(fixture)
  on.exit({ setwd(original); unlink(fixture, recursive = TRUE) })
  tryCatch({
    record <- file.path("gates", "LEDGER-FIXTURE", "eda.md")
    dir.create(dirname(record), recursive = TRUE)
    writeLines("synthetic evidence", "evidence.txt")
    writeLines(c("# Fixture", "", "Analysis ID: LEDGER-FIXTURE", "Stage: eda", "",
                 "## SME decision", "- [ ] APPROVED", "Digest signed: <!-- -->",
                 "Signed: <!-- -->", "Date: <!-- -->"), record)
    render_gate_package(record, data.frame(artifact = "Evidence", path = "evidence.txt", source = "fixture"))
    lines <- readLines(record)
    package <- sub("^Package-Digest: ", "", grep("^Package-Digest: ", lines, value = TRUE))
    lines[lines == "- [ ] APPROVED"] <- "- [x] APPROVED"
    lines[startsWith(lines, "Digest signed:")] <- paste("Digest signed:", package)
    lines[startsWith(lines, "Signed:")] <- "Signed: Local Fixture Reviewer"
    lines[startsWith(lines, "Date:")] <- "Date: 2000-01-01"
    writeLines(lines, record)
    suppressMessages(approve_gate(record, check_store = FALSE))
    suppressMessages(record_erratum(record, "Example claim", "Example correction",
                                    "evidence.txt", "minor", "Local Fixture Reviewer", "2000-01-02"))

    run_verifier <- function(script, target, expected) {
      out <- suppressWarnings(system2("Rscript", shQuote(c(file.path(tools, script), "verify", target)),
                                      stdout = TRUE, stderr = TRUE, timeout = 30))
      status <- attr(out, "status")
      status <- if (is.null(status)) 0L else status
      if (!identical(as.integer(status), expected) ||
          (expected == 0L && !any(grepl("ledger OK", out, fixed = TRUE))))
        stop(script, " verify failed the expected fixture outcome (exit ", status, ")", call. = FALSE)
    }
    for (kind in c("approvals", "errata")) {
      path <- file.path(dirname(record), paste0(kind, ".jsonl"))
      approval <- identical(kind, "approvals")
      verify <- if (approval) verify_approvals_ledger else verify_errata_ledger
      script <- if (approval) "approve_gate.R" else "errata.R"
      result <- verify(path)
      if (!result$ok || result$ledger_count != 1L || result$event_count != 1L)
        stop("Fixture must contain one valid ", kind, " event", call. = FALSE)
      run_verifier(script, path, 0L)
      original_lines <- readLines(path)
      writeLines(c(original_lines, "{"), path)
      run_verifier(script, path, 1L)
      # Keep valid JSON and the original event ID, but change a hashed field.
      changed <- sub("Local Fixture Reviewer", "Another Fixture Reviewer", original_lines, fixed = TRUE)
      if (identical(original_lines, changed)) stop("Fixture mutation made no change", call. = FALSE)
      writeLines(changed, path)
      run_verifier(script, path, 1L)
      other <- file.path("gates", "OTHER-FIXTURE", basename(path))
      dir.create(dirname(other), showWarnings = FALSE)
      writeLines(original_lines, other)
      run_verifier(script, other, 1L)
      unlink(other)
      writeLines(original_lines, path)
      report("PASS", paste(kind, "ledger verifier CLI fixtures"),
             "one nonempty event accepted; malformed, tampered and cross-bound events rejected")
    }
  }, error = function(e) report("FAIL", "ledger verifier CLI fixtures", conditionMessage(e)))
}

# The stock release has no exemptions for malformed signed records.
EXPECTED_ENVELOPE_MALFORMED <- character(0)
check_envelope_pin <- function() {
  source(file.path("tools", "gate.R"), local = TRUE)
  entries <- .read_legacy_gates()
  gate_files <- gate_record_files()
  malformed <- character(0); detail <- character(0)
  for (f in gate_files) {
    p <- parse_gate_record(f)
    if (!p$decision %in% .LEDGER_APPROVING) next
    if (!is.null(.legacy_entry_for(f, entries))) next
    idp <- .gate_identity_problems(p)
    if (length(idp)) {
      rel <- sprintf("gates/%s/%s", basename(dirname(f)), basename(f))
      malformed <- c(malformed, rel)
      detail <- c(detail, sprintf("%s: %s", rel, idp[1]))
    }
  }
  malformed <- sort(malformed); pinned <- sort(EXPECTED_ENVELOPE_MALFORMED)
  added <- setdiff(malformed, pinned); removed <- setdiff(pinned, malformed)
  if (length(added) == 0 && length(removed) == 0) {
    if (length(pinned))
      report("WARN", sprintf("envelope-malformed signed records PINNED (%d; re-version after re-run)",
                             length(pinned)), paste(detail, collapse = " | "))
    else
      report("PASS", sprintf("strict gate parser (%d records; no envelope-malformed signed records)", length(gate_files)))
  } else {
    report("FAIL", "envelope-malformed pin (SME act — re-version or update the pin in the same change)",
           paste(c(if (length(added)) paste0("unexpected: ", paste(added, collapse = ", ")),
                   if (length(removed)) paste0("missing: ", paste(removed, collapse = ", "))),
                 collapse = " | "))
  }
}

# --- Check 12: the battery table -------------------------------------------
# ONE INVENTORY FOR BOTH RUNNERS (SME decision D17-4; Codex round 16 R16-P2-6).
# Until integrity batch 17 the full local run invoked a hand-written list of
# `check_batch<N>_tests()` functions and the inventory that policed it matched
# only `test_batch<N>.R`, so tools/tests/test_sme_resign.sh -- shipped, passing,
# and registered in the hosted workflow -- was never run by
# `Rscript tools/ci_check.R`, and nothing said so. A developer running local CI
# saw success while a re-sign regression went unexercised.
#
# There is now ONE table. The full run invokes every entry in it, and
# check_battery_registration() compares the table against the batteries present
# on disk AND against the steps parsed out of .github/workflows/ci.yaml. A
# battery missing from either side is a FAIL naming it. The table is written out
# rather than globbed so that "removed from the local runner" is a reviewable
# diff and is caught by the inventory, exactly as "removed from the workflow" is.
#
.CI_BATTERIES <- list(
  list(file = "test_failopen_fixes.R", label = "fail-open fix regression battery"),
  list(file = "test_report_owner_review.R", label = "report owner-review battery", args = "--vanilla"),
  list(file = "test_review_results.R", label = "review remediation result contracts"),
  list(file = "test_review_runtime.R", label = "review remediation invocation provenance"),
  list(file = "test_review_integrity.R", label = "review remediation gate and audit integrity"),
  list(file = "test_review_environment.R", label = "review remediation environment contracts"),
  list(file = "test_batch4.R",           label = "integrity batch 4 battery"),
  list(file = "test_batch7.R",           label = "integrity batch 7 battery"),
  list(file = "test_batch8.R",           label = "integrity batch 8 battery"),
  list(file = "test_batch9.R",           label = "integrity batch 9 battery"),
  list(file = "test_batch10.R",          label = "integrity batch 10 battery"),
  list(file = "test_batch11.R",          label = "integrity batch 11 battery"),
  list(file = "test_batch12.R",          label = "integrity batch 12 battery"),
  list(file = "test_batch13.R",          label = "integrity batch 13 battery"),
  list(file = "test_batch14.R",          label = "integrity batch 14 battery"),
  list(file = "test_batch15.R",          label = "integrity batch 15 battery"),
  list(file = "test_batch16.R",          label = "integrity batch 16 battery"),
  list(file = "test_canon2_golden.R",    label = "canon2 golden vectors (rendering determinism + tamper)"),
  list(file = "test_covariate_task.R",   label = "covariate task frozen-input battery"),
  list(file = "test_eda_declarative.R",  label = "eda declarative battery",
       full_skip = "^SKIP: (yspec not installed; F8 overlay checks not run|yspec not installed; equivalence not run|yspec/pmtables not installed; end-to-end fixture run not performed)$"),
  list(file = "test_eda_external_spec.R", label = "eda external specification battery",
       full_skip = "^SKIP: yspec not installed; (spec-checked read.*not run|projection, label and role tests.*not run|mutation tests.*not run|specification-drift checks.*not run)$"),
  list(file = "test_env_confirm.R",      label = "environment confirmation battery"),
  list(file = "test_integrity.R",        label = "integrity battery"),
  list(file = "test_method_manifest.R",  label = "method-manifest battery"),
  list(file = "test_nonmem_base_model.R", label = "NONMEM base-model battery"),
  list(file = "test_baseline_fit.sh", label = "baseline fit guarded fixture battery"),
  list(file = "test_nonmem_image.sh",    label = "NONMEM image battery"),
  list(file = "test_note_text.R",        label = "note-text battery"),
  list(file = "test_protect_paths.sh",   label = "protect_paths guard battery"),
  list(file = "test_report_semantic.R",  label = "report-semantic battery",
       full_skip = paste0(
         "^SKIP  (C[0-5]b?: .*  \\((quarto or )?tectonic missing\\)|",
         "(R11 neutral reporting renders and passes semantics|",
         "R12 a dropped archived note blocks package creation|",
         "R13 the second page alone appears in the rendered report)  \\(tectonic missing\\))$")),
  list(file = "test_run_compute.R",      label = "run-compute battery"),
  list(file = "test_sign_approve_gate.R", label = "staged sign-approve battery"),
  list(file = "test_sme_resign.sh",      label = "SME re-sign utility battery"))

.ci_battery_files <- function() vapply(.CI_BATTERIES, function(b) b$file, character(1))

.ci_battery_runner <- function(f) if (grepl("\\.R$", f)) "Rscript" else "bash"

# Bounds emitted disclosure only; the subprocess capture remains unbounded.
# Digit masking does not remove alphabetic secrets or personal information.
.ci_failure_excerpt <- function(out) {
  truncated <- length(out) > 10L
  indices <- tail(seq_along(out), 10L)
  payload <- vapply(indices, function(i) {
    element <- out[[i]]
    if (nchar(element, type = "bytes") > 245L) truncated <<- TRUE
    # Mark bytes without transcoding, then bound before allocating raw bytes.
    Encoding(element) <- "bytes"
    bytes <- as.integer(charToRaw(substr(element, 1L, 245L)))
    bytes[bytes < 32L | bytes > 126L] <- 63L
    bytes[bytes >= 48L & bytes <= 57L] <- 100L
    rawToChar(as.raw(bytes))
  }, character(1), USE.NAMES = FALSE)
  if (!length(payload)) payload <- "(no child output)"
  paste0("  child | ", c("child output (masked)", payload,
                            if (truncated) "child output truncated" else "child output complete"))
}

# Runs every battery in the table, in a subprocess (each quit()s or exits with
# its own status). Full mode only.
check_batteries <- function() {
  for (b in .CI_BATTERIES) {
    tv <- file.path("tools", "tests", b$file)
    if (!file.exists(tv)) {
      report("FAIL", b$label, sprintf("%s is missing", tv))
      next
    }
    # A LAUNCH ERROR IS A FAILURE, NOT A CLEAN EXIT (Codex round 17 R17-P2-2,
    # integrity batch 18). system2() raises an R condition when it cannot start
    # the subprocess at all -- a missing interpreter, a fork failure. Round 17
    # caught that condition and returned an empty character vector, which
    # carries no `status` attribute; the next line read the absent attribute as
    # exit 0 and the battery that never ran was reported PASS. The condition is
    # now kept and reported. A missing status attribute on a NON-error return
    # still means exit 0, which is R's convention for a clean exit; an error
    # return no longer reaches that branch.
    launch_error <- NULL
    out <- tryCatch(suppressWarnings(
      system2(.ci_battery_runner(b$file), c(b$args, tv), stdout = TRUE, stderr = TRUE)),
      error = function(e) { launch_error <<- conditionMessage(e); NULL })
    if (!is.null(launch_error)) {
      report("FAIL", b$label,
             sprintf("%s could not be launched: %s", b$file, launch_error))
      next
    }
    st <- attr(out, "status")
    st <- if (is.null(st)) 0L else as.integer(st)
    # THE EXIT STATUS IS READ BEFORE THE SKIP LINE (R17-P2-2). A skip-enabled
    # battery that prints `SKIP ...` and then exits nonzero has failed; round 17
    # classified it as SKIP because it looked at the output first.
    if (!identical(st, 0L)) {
      report("FAIL", b$label, sprintf("%s exit %s", b$file, st))
      if (identical(b$file, "test_baseline_fit.sh")) {
        # Match the wrapper's full failure line and rebuild it from fixed fields.
        stages <- c("wrapper-path", "rscript-resolution", "library-admission",
                    "setup-receipt", "copy-setup", "copy-storage", "disk-reserve",
                    "original-cwd-guard", "recursive-wrapper-guard", "worker", "source-census")
        expected <- sprintf("FAIL baseline fixture stage=%s status=%d", stages, st)
        indices <- match(out, expected)
        stage <- c(stages[indices[!is.na(indices)]], "unreported")[[1L]]
        cat(sprintf("Baseline fixture diagnostic stage=%s status=%d\n", stage, st))
      } else if (identical(Sys.getenv("PMX_CI_FAILURE_DIAGNOSTICS", unset = ""), "masked")) {
        cat(.ci_failure_excerpt(out), sep = "\n")
      }
      next
    }
    skipped <- grep("^[[:space:]]*SKIP([[:space:]:]|$)", out, value = TRUE)
    if (!length(skipped)) {
      report("PASS", sprintf("%s (%s)", b$label, b$file))
      next
    }
    allowed <- !nzchar(Sys.getenv("PMX_CI_NO_SKIP")) && !is.null(b$full_skip) &&
      all(grepl(b$full_skip, skipped))
    if (allowed) report("SKIP", b$label, sprintf("%d validated-only case(s)", length(skipped)))
    else report("FAIL", b$label, sprintf("%d unexpected skipped case(s)", length(skipped)))
  }
}

# Compare the operational commands in parsed steps against each workflow's
# profile. Provisioning steps are listed by name; other run steps must match
# the exact sequence, including the interpreter, flags and full aggregate.
.ci_workflow_runs <- function(wf_path) {
  doc <- yaml::yaml.load_file(wf_path)
  if (!is.list(doc) || !is.list(doc$jobs) || !length(doc$jobs))
    stop("Missing workflow jobs")
  if (!is.null(doc$defaults)) stop("Unexpected workflow defaults")
  runs <- lapply(doc$jobs, function(job) {
    vapply(Filter(function(step) !is.null(step$run), job$steps), function(step) step$run, character(1))
  })
  list(runs = unlist(runs, use.names = FALSE), jobs = doc$jobs)
}

check_battery_registration <- function() {
  present <- sort(list.files(file.path("tools", "tests"), pattern = "^test_.*\\.(R|sh)$"))
  tabled <- .ci_battery_files()
  problems <- c(sprintf("%s missing from .CI_BATTERIES", setdiff(present, tabled)),
                sprintf("%s missing from tools/tests", setdiff(tabled, present)))
  if (length(tabled) != 33L || anyDuplicated(tabled))
    problems <- c(problems, "expected 33 distinct battery registrations")
  owner_review <- .CI_BATTERIES[tabled == "test_report_owner_review.R"]
  if (length(owner_review) != 1L || !identical(owner_review[[1L]]$args, "--vanilla"))
    problems <- c(problems, "owner-review registration must use --vanilla")
  if (any(!grepl("^test_[a-z0-9_]+\\.(R|sh)$", tabled)))
    problems <- c(problems, "battery table contains an invalid runner filename")

  workflows <- function() {
    light <- c("Rscript tools/ci_check.R --light", "bash tools/tests/test_protect_paths.sh",
               "bash copilot-adapter/tests/test_setup.sh", "python3 copilot-adapter/setup.py check")
    full <- c("Rscript --vanilla analyses/synthetic_example.R --generate",
              'R_LIBS_SITE="$R_LIBS_USER" Rscript tools/ci_check.R')
    jobs <- .ci_workflow_runs(file.path(".github", "workflows", "ci.yaml"))$jobs
    expected <- if (identical(names(jobs), "kit-battery")) {
      list(`kit-battery` = c("Rscript tools/ci_check.R --ledger-only", light))
    } else {
      list(`light-battery` = light, `full-battery` = full, `validated-stack` = full)
    }
    if (!setequal(names(jobs), names(expected)) || anyDuplicated(names(jobs)))
      stop("unexpected or missing CI job keys", call. = FALSE)
    setup <- c("Install lightweight CI dependencies", "Install system libraries for the CI R dependency closure",
               "Identify R runtime and runner image for cache",
               "Install poppler-utils (pdftotext for the rendered-report semantic battery)",
               "Install full-battery R dependencies", "Check composition and PDF extraction tools",
               "Install system libraries (CI R dependency closure and pdftotext)",
               "Install tectonic (official installer; binary lands in the working directory)",
               "Install the validated R stack (MPN snapshot 2025-09-17 + CRAN)")
    for (key in names(expected)) {
      job <- jobs[[key]]
      if (!is.null(job$`if`) || !is.null(job$defaults) || isTRUE(job$`continue-on-error`))
        stop(key, " job must be required", call. = FALSE)
      if (identical(key, "validated-stack") && !identical(job$env$PMX_CI_NO_SKIP, "1"))
        stop(key, " must set PMX_CI_NO_SKIP=1", call. = FALSE)
      operational <- Filter(function(step) !is.null(step$run) && !isTRUE(step$name %in% setup), job$steps)
      commands <- vapply(operational, function(step) {
        if (!is.null(step$`if`) || isTRUE(step$`continue-on-error`) ||
            !is.null(step$env) || !is.null(step$`working-directory`) || !is.null(step$shell))
          stop(key, " has a conditional or overridden operational step", call. = FALSE)
        step$run
      }, character(1))
      if (!identical(unname(commands), expected[[key]]))
        stop(key, " operational commands differ from the required profile", call. = FALSE)
      checkout <- Filter(function(step) identical(step$uses, "actions/checkout@v4"), job$steps)
      if (length(checkout) != 1L || !identical(checkout[[1L]]$with$`fetch-depth`, 0L) ||
          !identical(checkout[[1L]]$with$`persist-credentials`, FALSE) ||
          !is.null(checkout[[1L]]$with$ref))
        stop(key, " must check out the exact event with full history and no stored credentials", call. = FALSE)
    }
  }
  error <- tryCatch({ workflows(); NULL }, error = function(e) conditionMessage(e))
  problems <- c(problems, error)
  if (length(problems)) report("FAIL", "battery registration inventory", paste(problems, collapse = " | "))
  else report("PASS", sprintf("battery registration inventory (%d batteries and parsed workflow profile)", length(tabled)))
}

# --- Check 12c: references are loaded through the trusted loader only -------
# Integrity batch 13 (SME decision 1B). A reference implementation reaches an R
# session only through tools/load_reference.R, which authenticates the file's
# bytes and every declared dependency's bytes against the skill's MANIFEST.yaml
# BEFORE evaluating exactly those bytes. A direct `source()` / `sys.source()`
# of a skills/*/reference/*.R from harness code skips that authentication and
# is unsupported. This check is a grep over pipeline/ and analyses/ (light
# mode too): a source()/sys.source() call whose first argument is a literal
# reference path, a symbol bound in the same file to such a literal (whole or
# assembled with file.path("skills", ...)), or a symbol whose name contains
# "reference", is a FAIL naming the file and line. pilot/ is signed history
# and is NOT scanned; its forwarders are listed for the SME in the batch-13
# report.
check_reference_loading <- function(root = ".") {
  dirs <- file.path(root, c("pipeline", "analyses"))
  files <- character(0)
  for (d in dirs) if (dir.exists(d))
    files <- c(files, list.files(d, pattern = "\\.R$", full.names = TRUE, recursive = TRUE))
  ref_lit <- "skills/[^/\"']+/reference/"
  hits <- character(0)
  # Every USE of an unresolved-source exception, reported as a WARN below so an
  # exemption is visible in each CI run (D16-3).
  waivers <- character(0)

  # PARSE, DO NOT GREP (integrity batch 14, Codex round 13 R13-P2-3). The line
  # regex this replaced required the call and its argument on ONE line and the
  # parenthesis immediately after the name, so
  #   source(\n  "skills/.../gof.R",\n  local = TRUE\n)
  # and `source ("skills/.../gof.R")` and a named `file =` argument all walked
  # past it while the report claimed the files "source no skills/*/reference/*.R
  # directly". The check now walks the parse tree: any call whose head is
  # `source` or `sys.source`, bare or base::-qualified, is examined, and its
  # file argument is matched BY NAME (`file`) or positionally.
  is_load_head <- function(h) {
    if (is.name(h)) return(as.character(h) %in% c("source", "sys.source"))
    if (is.call(h) && length(h) == 3L && is.name(h[[1L]]) &&
        as.character(h[[1L]]) %in% c("::", ":::") && is.name(h[[3L]]))
      return(as.character(h[[3L]]) %in% c("source", "sys.source"))
    FALSE
  }
  # Files whose source() target this census may leave unresolved (SME decision
  # D15-2, 2026-09-06), keyed by the EXACT loading expression that was reviewed
  # (SME decision D16-3, 2026-09-06; Codex round 15, R15-P2-3).
  #
  # WHY PER EXPRESSION AND NOT PER FILE. Until integrity batch 16 this was a
  # vector of file paths and the exemption covered the whole file, so an
  # unrelated `source(Sys.getenv("METHOD_FILE"))` added to, say,
  # pipeline/tasks/simulation.R inherited the exception granted to that file's
  # reviewed targets file-target load. The justification above the list said
  # "that particular construction"; the implementation said "that file". The
  # allowlist now maps a file to the deparsed text of the loading expression(s)
  # the SME accepted; any OTHER unresolved load in the same file is a FAIL, and
  # every use of an exception is reported as a WARN naming the file, the line
  # and the expression, so an exemption is visible in every CI run rather than
  # silent.
  #
  # WHAT THE ACCEPTED CONSTRUCTIONS ARE. The eight files below source a TARGETS
  # FILE-TARGET SYMBOL (or, for compose.R, the composed task-module path; for
  # reporting.R, a path spliced as a literal from a declared location):
  # `source(da_assembly_code, local = TRUE)` inside a quoted target body, where
  # the symbol is a `format = "file"` target the pipeline resolves at run time.
  # The path is not a constant this census can fold and it never names a
  # skills/*/reference/*.R file -- those reach a task module through
  # load_reference(). The five pilot/ forwarders are NOT here: pilot/ is outside
  # the scanned set entirely and keeps its own WARN.
  #
  # Expression texts are compared after collapsing runs of whitespace, so
  # reindentation does not silently drop an exemption; any other edit to the
  # call does, which is the intent.
  .CI_UNRESOLVED_SOURCE_ALLOWLIST <- list(
    "pipeline/compose.R" = c(
      "sys.source(module, envir = env)"),
    "pipeline/_targets_full_example.R" = c(
      "source(mod_101_src, local = TRUE)",
      "source(mod_102_src, local = TRUE)"),
    "pipeline/tasks/base_model.R" = c(
      "source(bm_modeling_code, local = TRUE)"),
    "pipeline/tasks/data_assembly.R" = c(
      "source(da_assembly_code, local = TRUE)"),
    "pipeline/tasks/qualification.R" = c(
      "source(bm_modeling_code, local = TRUE)",
      "source(qual_code_file, local = TRUE)"),
    "pipeline/tasks/reporting.R" = c(
      "source(.(symbol_map_file), local = TRUE)",
      "source(.(forest_param), local = TRUE)"),
    "pipeline/tasks/review.R" = c(
      "source(review_checks_code, local = TRUE)"),
    "pipeline/tasks/simulation.R" = c(
      "source(sim_model_def_file, local = TRUE)",
      "source(sim_translation_file, local = TRUE)"))
  .norm_expr <- function(x) gsub("\\s+", " ", trimws(paste(deparse(x), collapse = " ")))
  # `file` is the first formal of both source() and sys.source().
  file_arg_of <- function(cl) {
    nms <- names(cl)
    if (!is.null(nms) && "file" %in% nms) return(cl[["file"]])
    rest <- as.list(cl)[-1L]
    if (!is.null(nms)) rest <- rest[nms[-1L] == "" | is.na(nms[-1L])]
    if (!length(rest)) return(NULL)
    rest[[1L]]
  }
  # A CONSTANT EVALUATOR FOR ORDINARY PATH CONSTRUCTORS (integrity batch 15,
  # Codex round 14 R14-P2-1). A path spelled `file.path("skills", ...)` or
  # `paste0("skills/diagnostics/reference/", "gof.R")`, with or without a `base::`
  # qualifier, is the same literal path written differently. Before this batch
  # only the bare `file.path` spelling was recognised, so both `base::file.path`
  # and `paste0` of literals escaped the census -- a regression against the
  # substring check this parsed census replaced. Nothing is evaluated here: the
  # function folds string literals in the parse tree and returns NA for anything
  # else, and an NA that reaches a source() call is reported (below), never
  # silently passed.
  head_name <- function(h) {
    if (is.name(h)) return(as.character(h))
    if (is.call(h) && length(h) == 3L && is.name(h[[1L]]) &&
        as.character(h[[1L]]) %in% c("::", ":::") && is.name(h[[3L]]))
      return(as.character(h[[3L]]))
    NA_character_
  }
  literal_of <- function(e) {
    if (is.character(e) && length(e) == 1L) return(e)
    if (is.call(e)) {
      hn <- head_name(e[[1L]])
      if (!is.na(hn) && hn %in% c("file.path", "paste0", "paste")) {
        parts <- as.list(e)[-1L]
        nms <- names(parts)
        if (!is.null(nms) && any(nzchar(nms))) return(NA_character_)
        if (!length(parts)) return(NA_character_)
        lits <- vapply(parts, function(x) {
          v <- literal_of(x)
          if (is.na(v)) NA_character_ else v
        }, character(1))
        if (anyNA(lits)) return(NA_character_)
        sep <- if (identical(hn, "file.path")) "/" else if (identical(hn, "paste")) " " else ""
        return(paste(lits, collapse = sep))
      }
    }
    NA_character_
  }

  for (f in files) {
    exprs <- tryCatch(parse(f, keep.source = TRUE), error = function(e) NULL)
    if (is.null(exprs)) {
      hits <- c(hits, sprintf("%s: does not parse; the reference-loading check cannot read it",
                              sub(paste0("^", root, "/"), "", f)))
      next
    }
    src <- readLines(f, warn = FALSE)
    # Symbols bound anywhere in this file to a reference path (whole, or
    # assembled from literals with file.path()).
    bound <- character(0)
    # Every symbol bound in this file to a resolvable constant path, not only
    # the ones naming a reference: a `source(p)` whose `p` resolves to an
    # ordinary path is resolved, not unresolved (R14-P2-1).
    #
    # ONE UNAMBIGUOUS BINDING, OR UNRESOLVED (integrity batch 16, Codex round 15
    # R15-P1-3; widened in fix round 1, review blocker B2). The collector used to
    # record a value only when resolution SUCCEEDED and to keep it across every
    # later assignment, so
    #   p <- "tools/helper.R"
    #   p <- Sys.getenv("METHOD_FILE")
    #   source(p)
    # resolved `p` to the obsolete literal and the load passed as an ordinary
    # path, when what actually executes is whatever METHOD_FILE names.
    #
    # WHAT COUNTS AS A BINDING, EXACTLY. Fix round 0 counted `<-`, `=` and `<<-`
    # only, while the comment claimed the general rule; a `for` loop index and an
    # `assign("p", ...)` therefore left the obsolete literal resolved, and both
    # failed in the PERMISSIVE direction. Every construct below is counted as a
    # binding of the named symbol:
    #   `<-`  `=`  `<<-`                    with a symbol on the left
    #   `for (nm in ...)`                    the loop index
    #   `assign("nm", ...)`                  including base:: / envir = forms
    #   `delayedAssign("nm", ...)`
    #   `makeActiveBinding("nm", ...)`
    #   `list2env(...)`                      names unknown: see below
    #   `f(nm) <- v`, `nm[i] <- v`, ...      functional replacement forms
    #   a formal of any function defined in the file
    # A symbol resolves only when the file binds it EXACTLY ONCE and that one
    # binding folds to a literal; a second binding of ANY of these kinds,
    # resolvable or not, makes it unresolved. `list2env()`, `attach()` and any
    # `assign()`/`delayedAssign()`/`makeActiveBinding()` whose first argument is
    # not a string literal bind a name this walk cannot name, so they poison
    # EVERY symbol in the file rather than none -- the refusing direction.
    #
    # WHAT IS STILL NOT COUNTED, said plainly rather than left implied. A
    # `source()` or `sys.source()` this file performs can itself bind file-level
    # symbols, and this walk does not follow it. Counting every loader as an
    # opaque binder was tried in fix round 1 and rejected: it makes EVERY
    # symbol-target `source()` in a loader file unresolved, including a symbol
    # assigned once to an ordinary literal, which is over-refusal rather than
    # precision. A load whose own target is unresolved is already a FAIL below,
    # and a load of a literal path is named in the report; a symbol rebound by a
    # sourced file is a residual this census does not close. Two further binder
    # spellings are not counted either: a replacement form whose innermost
    # target is a call rather than a symbol (`environment()$w <- ...`), and a
    # binding created by evaluating constructed code (`eval(str2lang("y <- ..."))`);
    # both fold a later source(y) to the stale file-level literal. They are the
    # same class as the source() residual and no repository file uses them.
    #
    # Counting is deliberately file-wide rather than top-level-only: a binding
    # inside a branch or a function body is equally capable of changing what a
    # later `source()` loads, and over-counting can only move a target towards
    # UNRESOLVED.
    n_assign <- list()
    bound_lit <- list()
    opaque_binder <- FALSE
    bind_of <- function(nm, rhs = NULL) {
      n_assign[[nm]] <<- (if (is.null(n_assign[[nm]])) 0L else n_assign[[nm]]) + 1L
      if (is.null(rhs)) return(invisible(NULL))
      lit <- literal_of(rhs)
      if (!is.na(lit)) bound_lit[[nm]] <<- lit
      if (!is.na(lit) && grepl(ref_lit, lit)) bound <<- c(bound, nm)
      invisible(NULL)
    }
    # The head of a call as a plain name, `base::f` included.
    .head_of <- function(h) {
      if (is.name(h)) return(as.character(h))
      if (is.call(h) && length(h) == 3L && is.name(h[[1L]]) &&
          as.character(h[[1L]]) %in% c("::", ":::") && is.name(h[[3L]]))
        return(as.character(h[[3L]]))
      NA_character_
    }
    collect <- function(e) {
      if (missing(e)) return(invisible(NULL))
      if (is.name(e) && !nzchar(as.character(e))) return(invisible(NULL))
      if (is.call(e)) {
        hn <- .head_of(e[[1L]])
        if (!is.na(hn)) {
          if (hn %in% c("<-", "=", "<<-") && length(e) >= 3L) {
            if (is.name(e[[2L]])) {
              bind_of(as.character(e[[2L]]), e[[3L]])
            } else if (is.call(e[[2L]])) {
              # A replacement form -- `names(x) <- v`, `x[[i]] <- v`,
              # `attr(x, "a") <- v` -- rebinds the symbol at the bottom of the
              # target expression. Value unknown to this walk, so no literal.
              tgt <- e[[2L]]
              while (is.call(tgt) && length(tgt) >= 2L) tgt <- tgt[[2L]]
              if (is.name(tgt) && nzchar(as.character(tgt))) bind_of(as.character(tgt))
            }
          } else if (hn == "for" && length(e) >= 3L && is.name(e[[2L]])) {
            bind_of(as.character(e[[2L]]))
          } else if (hn %in% c("assign", "delayedAssign", "makeActiveBinding") &&
                     length(e) >= 2L) {
            a1 <- e[[2L]]
            if (is.character(a1) && length(a1) == 1L) bind_of(a1)
            else opaque_binder <<- TRUE
          } else if (hn %in% c("list2env", "attach")) {
            # These bind a whole mapping whose names this walk cannot
            # enumerate, so they poison every symbol in the file.
            opaque_binder <<- TRUE
          } else if (hn == "function" && length(e) >= 2L &&
                     (is.pairlist(e[[2L]]) || is.null(e[[2L]]))) {
            # A FORMAL IS A BINDING OF THAT NAME. `read_code <- function(p) ...`
            # gives the file a second `p`, and the census must not resolve the
            # formal to a file-level literal (R15-P1-3, second scenario).
            for (fn in names(e[[2L]])) if (nzchar(fn)) bind_of(fn)
          }
        }
        for (i in seq_along(e)) collect(e[[i]])
      } else if (is.pairlist(e) || is.expression(e)) {
        for (i in seq_along(e)) collect(e[[i]])
      }
    }
    # A LITERAL COUNTS ONLY FROM A TOP-LEVEL SOLE ASSIGNMENT (integrity batch
    # 17, Codex round 16 R16-P1-2). `collect()` walks recursively, so before
    # this change the single assignment it counted could be anywhere -- inside a
    # function body that never runs, for instance. A file holding
    #     unused <- function() { p <- "tools/helper.R" }
    #     source(p)
    # then resolved `source(p)` to a harmless-looking literal, while at runtime
    # `p` resolves in the loading environment and can name anything. The
    # recursive count still decides UNIQUENESS (a nested rebinding must keep
    # poisoning the name); the top-level index decides whether there is a
    # file-level constant at all. This is the requirement
    # tools/method_manifest.R already applies to the manifest audit's root
    # binding index.
    top_names <- character(0)
    for (k in seq_along(exprs)) {
      te <- exprs[[k]]
      if (is.call(te) && length(te) >= 3L) {
        th <- .head_of(te[[1L]])
        if (!is.na(th) && th %in% c("<-", "=", "<<-") && is.name(te[[2L]]) &&
            nzchar(as.character(te[[2L]])))
          top_names <- c(top_names, as.character(te[[2L]]))
      }
    }
    for (k in seq_along(exprs)) collect(exprs[[k]])
    for (nm in names(bound_lit)) if (!nm %in% top_names) bound_lit[[nm]] <- NULL
    bound <- intersect(bound, top_names)
    # Drop every symbol the file binds more than once, and every symbol at all
    # when the file contains a binder whose target this walk cannot name.
    if (opaque_binder) {
      bound_lit <- list()
      bound <- character(0)
    } else {
      for (nm in names(n_assign)) if (!identical(n_assign[[nm]], 1L)) {
        bound_lit[[nm]] <- NULL
        bound <- setdiff(bound, nm)
      }
    }

    # Walk every call, carrying the top-level expression's line span so the
    # message can name the line the load is actually written on.
    #
    # FORMALS SHADOW FILE-LEVEL NAMES (integrity batch 16, R15-P1-3). A file
    # that binds `p <- "tools/helper.R"` at the top level and also defines
    # `read_code <- function(p) source(p)` has two different `p`s; resolving the
    # formal to the file-level literal claims to know what a wrapper loads when
    # it depends entirely on the caller. `shadow` carries the formal names of
    # every enclosing function definition, and a shadowed symbol is UNRESOLVED.
    examine <- function(e, lo, hi, shadow = character(0)) {
      if (missing(e)) return(invisible(NULL))
      if (is.name(e) && !nzchar(as.character(e))) return(invisible(NULL))
      if (is.call(e)) {
        if (is.name(e[[1L]]) && identical(as.character(e[[1L]]), "function") &&
            length(e) >= 2L && (is.pairlist(e[[2L]]) || is.null(e[[2L]]))) {
          fn <- names(e[[2L]])
          if (length(fn)) shadow <- unique(c(shadow, fn))
        }
        if (is_load_head(e[[1L]])) {
          arg <- file_arg_of(e)
          lit <- if (is.null(arg)) NA_character_ else literal_of(arg)
          shadowed <- !is.null(arg) && is.name(arg) && as.character(arg) %in% shadow
          if (is.na(lit) && !shadowed && !is.null(arg) && is.name(arg) &&
              !is.null(bound_lit[[as.character(arg)]]))
            lit <- bound_lit[[as.character(arg)]]
          why <- if (!is.na(lit) && grepl(ref_lit, lit)) "a literal reference path"
                 else if (!shadowed && !is.null(arg) && is.name(arg) &&
                          as.character(arg) %in% bound)
                   sprintf("the symbol `%s`, bound to a reference path in this file",
                           as.character(arg))
                 else if (!is.null(arg) && is.name(arg) &&
                          grepl("reference", as.character(arg), ignore.case = TRUE))
                   sprintf("the symbol `%s`, whose name says it is a reference",
                           as.character(arg))
                 else NA_character_
          rel <- sub(paste0("^", root, "/"), "", f)
          span <- seq.int(lo, min(hi, length(src)))
          at <- span[grepl("(^|[^A-Za-z0-9_.])(sys\\.)?source\\b", src[span])]
          at <- if (length(at)) at[1] else lo
          if (!is.na(why)) {
            hits <<- c(hits, sprintf("%s:%d sources %s; load it with load_reference(<skill_dir>, <reference_path>, envir = ...) (tools/load_reference.R)",
                                     rel, at, why))
          } else if (is.na(lit)) {
            # AN UNRESOLVED SOURCE TARGET IS A FAIL (SME decision D15-2,
            # 2026-09-06). Until batch 15 a source() whose argument the
            # census could not resolve contributed nothing and the check
            # printed an unconditional PASS, so a silent PASS read as
            # "authentication covered" when in fact the target was unknown.
            # The exemption is per EXPRESSION, not per file (D16-3), and every
            # use of one is reported.
            txt <- .norm_expr(e)
            allowed <- .CI_UNRESOLVED_SOURCE_ALLOWLIST[[rel]]
            if (!is.null(allowed) && txt %in% vapply(allowed, function(a)
                  gsub("\\s+", " ", trimws(a)), character(1))) {
              waivers <<- c(waivers, sprintf("%s:%d %s", rel, at, txt))
            } else {
              hits <<- c(hits, sprintf("%s:%d unresolved source target `%s`: the argument is not a constant path this check can fold, so whether it names a skills/*/reference/*.R file is unknown. Spell the path as literals (file.path()/paste0() of string literals, or a symbol assigned exactly once at the top level of this file), load it with load_reference(), or add THIS EXPRESSION to .CI_UNRESOLVED_SOURCE_ALLOWLIST in tools/ci_check.R with the SME's reason.",
                                       rel, at, txt))
            }
          }
        }
        for (i in seq_along(e)) examine(e[[i]], lo, hi, shadow)
      } else if (is.pairlist(e) || is.expression(e)) {
        for (i in seq_along(e)) examine(e[[i]], lo, hi, shadow)
      }
    }
    refs <- attr(exprs, "srcref")
    for (k in seq_along(exprs)) {
      r <- if (!is.null(refs) && length(refs) >= k) refs[[k]] else NULL
      lo <- if (is.null(r)) 1L else as.integer(r[1L])
      hi <- if (is.null(r)) length(src) else as.integer(r[3L])
      examine(exprs[[k]], lo, hi)
    }
  }
  hits <- unique(hits)
  waivers <- unique(waivers)
  for (w in waivers)
    report("WARN", "unresolved source target exempted by the SME allowlist", w)
  if (length(hits)) {
    for (h in hits) report("FAIL", "reference loading", h)
  } else {
    report("PASS", sprintf("reference loading (%d file(s) under pipeline/ and analyses/ hold no parsed source()/sys.source() call naming a skills/*/reference/*.R file)", length(files)))
  }

  # WARN, never FAIL (integrity batch 13 fix round 1, R13-P2-D). A task module
  # that CALLS reference_* functions but neither load_reference()s nor
  # source()s their reference receives them only because an analysis script
  # source()s a pilot/ forwarder that source()s the reference unauthenticated.
  # That is a live-lane gap the loader does not yet close (the forwarder is a
  # pilot/ file, read-only for the harness and an SME item), so it is surfaced
  # as a WARN naming the module AND the forwarder rather than passing silently.
  sym_re <- "reference_[A-Za-z0-9_]+"
  # reference symbol -> reference file (top-level definitions)
  sym_file <- list()
  for (rf in list.files(file.path(root, "skills"), pattern = "\\.R$",
                        full.names = TRUE, recursive = TRUE)) {
    if (!grepl("/reference/", rf)) next
    rl <- sub("#.*$", "", readLines(rf, warn = FALSE))
    for (i in seq_along(rl)) {
      m <- regmatches(rl[i], regexec(paste0("^\\s*(", sym_re, ")\\s*(<-|=)"), rl[i]))[[1]]
      if (length(m) == 3L) sym_file[[m[2]]] <- sub(paste0("^", root, "/"), "", rf)
    }
  }
  # reference file -> pilot forwarder that source()s it
  ref2fwd <- list()
  for (pf in list.files(file.path(root, "pilot"), pattern = "\\.R$",
                        full.names = TRUE, recursive = TRUE)) {
    pl <- sub("#.*$", "", readLines(pf, warn = FALSE))
    for (i in seq_along(pl)) {
      mm <- regmatches(pl[i], gregexpr("skills[/\"', ]+[A-Za-z0-9_-]+[/\"', ]+reference[/\"', ]+[A-Za-z0-9_.]+\\.R", pl[i]))[[1]]
      if (length(mm)) {
        # normalise both file.path("skills","x","reference","y.R") and the
        # literal "skills/x/reference/y.R" to a repository-relative path.
        parts <- regmatches(mm[1], gregexpr("[A-Za-z0-9_.-]+", mm[1]))[[1]]
        rp <- paste(parts, collapse = "/")
        ref2fwd[[rp]] <- sub(paste0("^", root, "/"), "", pf)
      }
    }
  }
  failed_files <- unique(sub(":.*$", "", hits))
  task_dir <- file.path(root, "pipeline", "tasks")
  if (dir.exists(task_dir)) {
    for (tf in list.files(task_dir, pattern = "\\.R$", full.names = TRUE)) {
      rel_tf <- sub(paste0("^", root, "/"), "", tf)
      if (rel_tf %in% failed_files) next                   # already a FAIL above
      tl <- sub("#.*$", "", readLines(tf, warn = FALSE))
      code <- paste(tl, collapse = "\n")
      if (grepl("load_reference\\(", code)) next           # module loads its own
      used <- unique(regmatches(code, gregexpr(paste0(sym_re, "\\s*\\("), code))[[1]])
      used <- sub("\\s*\\($", "", used)
      used <- used[used %in% names(sym_file)]
      if (!length(used)) next
      by_ref <- split(used, vapply(used, function(s) sym_file[[s]], character(1)))
      for (rp in names(by_ref)) {
        fwd <- ref2fwd[[rp]]
        if (is.null(fwd)) next
        report("WARN", "reference loading",
               sprintf("%s calls %s but does not load_reference() them; their reference %s reaches this task only through the pilot forwarder %s (source()d by the analysis, not authenticated by the loader). Live-lane gap; the forwarder is a pilot/ SME item.",
                       sub(paste0("^", root, "/"), "", tf),
                       paste(by_ref[[rp]], collapse = ", "), rp, fwd))
      }
    }
  }
  invisible(hits)
}

# --- Check 13: evidence-note migration inventory (batch 3b tranche 2, item 3)-
# WARN (never FAIL) inventory of task modules that still pass PLAIN-CHARACTER
# notes to render_gate_package rather than the evidence_note()/authored_note()
# split. Migration is the owning specialists' later work; this only surfaces
# what remains. Heuristic: a module that calls render_gate_package but never
# evidence_note/authored_note is unmigrated. Full mode only.
check_evidence_note_migration <- function() {
  # Task modules build a `*_package` payload with a `notes = ...` field that the
  # harness later renders via render_gate_package. A module that populates notes
  # but never uses evidence_note()/authored_note() still ships plain-character
  # notes — inventory it (WARN) as pending migration.
  mods <- list.files(file.path("pipeline", "tasks"), pattern = "\\.R$",
                     full.names = TRUE)
  unmigrated <- character(0)
  for (m in mods) {
    txt <- paste(readLines(m, warn = FALSE), collapse = "\n")
    if (grepl("notes\\s*=", txt) &&
        !grepl("evidence_note|authored_note", txt))
      unmigrated <- c(unmigrated, basename(m))
  }
  if (length(unmigrated))
    report("WARN", "evidence-note migration pending (plain-character notes)",
           paste(unmigrated, collapse = ", "))
  else
    report("PASS", "evidence-note migration (no plain-character task modules)")
}

# --- run (only when executed as a script; a no-op when sourced) -------------
.run_ci <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  light <- length(args) > 0L && identical(args[[1L]], "--light")
  ledger_only <- length(args) > 0L && identical(args[[1L]], "--ledger-only")
  if (light || ledger_only) args <- args[-1L]
  initial_main <- identical(args, "--initial-main")
  explicit_base <- if (length(args) == 2L && identical(args[[1L]], "--ledger-base")) args[[2L]] else NULL
  if ((light && length(args)) || (length(args) && !initial_main && is.null(explicit_base)))
    stop("Usage: Rscript tools/ci_check.R [--light | --ledger-only] [--initial-main | --ledger-base SHA]",
         call. = FALSE)

  # Repo root: this script lives in tools/ .
  file_arg <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
  root <- normalizePath(file.path(dirname(file_arg), ".."), mustWork = FALSE)
  if (is.na(root) || !nzchar(root)) root <- normalizePath(".", mustWork = FALSE)
  setwd(root)

  cat(sprintf("pmx-harness CI battery (%s mode)\n", if (light) "light" else if (ledger_only) "ledger" else "full"))
  cat("-----------------------------------------------\n")

  # Dependency-light subset — also the GitHub Actions set. The entry-point
  # gating audit and the manifest-coverage check are static parses (base R +
  # digest) and run here too (review round 8): an ungated approved entry
  # point or an uncovered reference file must fail the light job, not only
  # the full battery.
  if (!ledger_only) {
    check_parse()
    check_stray_tags()
    check_gate_markers()
    check_entrypoint_gating()
    check_manifest_coverage()
    check_reference_loading()
    check_battery_registration()
  }
  if (!light) check_ledger_preflight(initial_main, explicit_base)

  if (!light && !ledger_only) {
    wanted <- c("quarto", "pdftotext", if (nzchar(Sys.getenv("PMX_CI_NO_SKIP"))) "tectonic")
    missing <- wanted[!nzchar(Sys.which(wanted))]
    if (length(missing)) report("FAIL", "report and composition tools", paste(missing, collapse = ", "))
    check_composition()
    check_gate_dominance()
    check_self_gate()
    check_manifest_digests()
    check_gate_digest_binding()
    check_allowlist_pin()
    check_binding_semantics()
    check_envelope_pin()
    check_ledger_cli_fixtures()
    check_batteries()
    check_evidence_note_migration()
    check_r_lock()
    check_yaml()
  }

  cat("-----------------------------------------------\n")
  cat(sprintf("CI battery: %d FAIL\n", .fail_count))
  quit(status = if (.fail_count > 0L) 1L else 0L)
}

if (.invoked_as_script()) .run_ci()
