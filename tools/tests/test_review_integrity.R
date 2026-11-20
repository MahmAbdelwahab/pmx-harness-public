#!/usr/bin/env Rscript
# Review remediation regressions: gate, approval, errata, method authorization,
# and ledger-history controls. All records and signatures are synthetic fixtures
# under a disposable directory; no live gate, manifest, or history is changed.

.invoked_as_script <- function() {
  fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  length(fa) > 0L && grepl("test_review_integrity\\.R$", sub("^--file=", "", fa[1]))
}

.run_review_integrity_tests <- function() {
  args <- commandArgs(FALSE)
  file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
  TOOLS <- normalizePath(file.path(dirname(file_arg), ".."), mustWork = TRUE)

  suppressWarnings(suppressMessages({
    source(file.path(TOOLS, "gate.R"))
    source(file.path(TOOLS, "gate_package.R"))
    source(file.path(TOOLS, "method_manifest.R"))
    source(file.path(TOOLS, "approve_gate.R"))
    source(file.path(TOOLS, "errata.R"))
  }))

  .pass <- 0L
  .fail <- 0L
  ok <- function(cond, name) {
    cond <- tryCatch(isTRUE(cond), error = function(e) {
      cat(sprintf("      error: %s\n", conditionMessage(e)))
      FALSE
    })
    if (cond) {
      .pass <<- .pass + 1L
      cat(sprintf("PASS  %s\n", name))
    } else {
      .fail <<- .fail + 1L
      cat(sprintf("FAIL  %s\n", name))
    }
  }
  expect_stop <- function(expr, name, regexp = NULL) {
    e <- tryCatch({ force(expr); NULL }, error = function(e) e)
    hit <- !is.null(e) && (is.null(regexp) || grepl(regexp, conditionMessage(e)))
    if (!is.null(e) && !hit) cat(sprintf("      got: %s\n", conditionMessage(e)))
    if (is.null(e)) cat("      no error was raised\n")
    ok(hit, name)
  }

  root <- tempfile("review-integrity-")
  dir.create(root)
  old_wd <- setwd(root)
  on.exit({ setwd(old_wd); unlink(root, recursive = TRUE) }, add = TRUE)
  dir.create("tools")
  file.copy(file.path(TOOLS, c("gate.R", "gate_package.R", "method_manifest.R",
                              "approve_gate.R", "errata.R", "ci_check.R")),
            "tools", overwrite = TRUE)
  writeLines("fixture artifact", "artifact.txt")

  gate_header <- function(id, signature = c("Signed: Fixture", "Date: 2026-09-11")) {
    c(paste("Analysis ID:", id), "Stage: eda", "## SME decision",
      "- [ ] APPROVED", signature, "Digest signed:")
  }
  make_signed_gate <- function(id, signature = c("Signed: Fixture", "Date: 2026-09-11"),
                               artifact = "fixture") {
    dir.create(file.path("gates", id), recursive = TRUE)
    path <- file.path("gates", id, "eda.md")
    writeLines(gate_header(id, signature), path)
    render_gate_package(path, data.frame(artifact = artifact, path = "artifact.txt",
                                         stringsAsFactors = FALSE))
    lines <- readLines(path, warn = FALSE)
    lines[lines == "- [ ] APPROVED"] <- "- [x] APPROVED"
    digest <- parse_gate_record(path, lines = lines)$package_digest
    lines[grepl("^\\s*Digest signed:", lines)] <- paste("Digest signed:", digest)
    writeLines(lines, path)
    path
  }

  cat("\n--- I1: runtime verifies approval and errata chains ----------------------\n")
  f <- make_signed_gate("CHAIN")
  suppressMessages(approve_gate(f, check_store = FALSE))
  suppressMessages(revoke_gate(f, "fixture revocation"))
  ap <- .ledger_path(f)
  lines <- readLines(ap, warn = FALSE)
  lines[length(lines)] <- sub('"decision":"revoked"', '"decision":"approved"',
                              lines[length(lines)], fixed = TRUE)
  writeLines(lines, ap)
  expect_stop(suppressMessages(require_gate(f)),
              "I1: corrupt approval event cannot reopen a revoked gate",
              "approval ledger.*integrity|event_id mismatch|chain")

  f <- make_signed_gate("ERRCHAIN")
  suppressMessages(approve_gate(f, check_store = FALSE))
  suppressMessages(record_erratum(f, "fixture", "corrected", "fixture evidence",
                                  "material", "Fixture", "2026-09-11"))
  ep <- .errata_path(f)
  lines <- readLines(ep, warn = FALSE)
  lines <- sub('"materiality":"material"', '"materiality":"minor"', lines,
               fixed = TRUE)
  writeLines(lines, ep)
  expect_stop(suppressMessages(require_gate(f)),
              "I1: corrupt erratum cannot downgrade materiality",
              "errata ledger.*integrity|event_id mismatch|chain")

  alias_gate <- make_signed_gate("ERRALIAS")
  suppressMessages(approve_gate(alias_gate, check_store = FALSE))
  suppressMessages(record_erratum(alias_gate, "fixture", "minor correction",
                                  "fixture evidence", "minor", "Fixture", "2026-09-11"))
  alias_ledger <- .errata_path(alias_gate)
  alias_event <- .read_ledger(alias_ledger)[[1]]
  alias_event$gate_path <- normalizePath(alias_gate)
  alias_event$previous_event_id <- NA_character_
  alias_event$event_id <- .ledger_event_id(alias_event, .ERRATA_FIELDS)
  cat(.errata_to_json(alias_event), "\n", sep = "", file = alias_ledger, append = TRUE)
  expect_stop(suppressMessages(require_gate(alias_gate)),
              "I1: path aliases cannot fork the one canonical errata chain",
              "errata ledger.*integrity|chain")

  role_labels <- c(" sMe ", "THE   SME")
  for (i in seq_along(role_labels)) {
    label <- role_labels[[i]]
    f <- make_signed_gate(paste0("ROLE-", i))
    suppressMessages(approve_gate(f, check_store = FALSE))
    suppressMessages(record_erratum(f, "fixture", "minor edit", "fixture evidence",
                                    "minor", "Fixture", "2026-09-11"))
    ap <- .ledger_path(f)
    ep <- .errata_path(f)
    read_bytes <- function(path) readBin(path, "raw", file.info(path)$size)
    paths <- c(f, ap, ep)
    state <- lapply(paths, read_bytes)
    expect_stop(release_gate(f, "fixture release", label),
                "SIGN-ROLE-REFUSAL: release rejects a reserved signer", "reserved role label")
    expect_stop(record_erratum(f, "fixture", "minor edit", "fixture evidence",
                              "minor", label, "2026-09-11"),
                "SIGN-ROLE-REFUSAL: erratum writer rejects a reserved signer",
                "reserved role label")
    ok(identical(lapply(paths, read_bytes), state) &&
         identical(list.files(dirname(f)), c("approvals.jsonl", "eda.md", "errata.jsonl")),
       "SIGN-ROLE-REFUSAL: rejected writers leave record, ledgers and inventory intact")

    approval <- .read_ledger(ap)[[1L]]
    approval$signer <- label
    approval$event_id <- .ledger_event_id(approval)
    writeLines(.ledger_to_json(approval), ap)
    v <- verify_approvals_ledger(ap)
    ok(!v$ok && any(grepl("reserved role label", v$problems)) &&
         !any(grepl("event_id mismatch", v$problems)),
       "REVIEW-CHAIN: approval reader rejects a reserved signer with a valid event ID")
    writeBin(state[[2L]], ap)
    erratum <- .read_ledger(ep)[[1L]]
    erratum$signer <- label
    erratum$event_id <- .ledger_event_id(erratum, .ERRATA_FIELDS)
    writeLines(.errata_to_json(erratum), ep)
    v <- verify_errata_ledger(ep)
    ok(!v$ok && any(grepl("reserved role label", v$problems)) &&
         !any(grepl("event_id mismatch", v$problems)),
       "REVIEW-CHAIN: errata reader rejects a reserved signer with a valid event ID")
    unlink(dirname(f), recursive = TRUE)
  }

  cat("\n--- I2/I5: artifact locality and Markdown escape round-trip -------------\n")
  dir.create("outside")
  writeLines("outside", file.path("outside", "result.txt"))
  dir.create("collection")
  file.symlink(normalizePath("outside"), file.path("collection", "nested-link"))
  nested_digest <- .gate_artifact_cell_sha256("collection")
  nested_reason <- .gate_artifact_cell_reason("collection")
  ok(identical(nested_digest, "MISSING") && grepl("symlink", nested_reason, ignore.case = TRUE),
     "I2: nested symlink directory is MISSING with local reason")

  # A safe traversal must preserve the established sorted member digest.
  dir.create("ordered/a", recursive = TRUE)
  writeLines("nested", "ordered/a/child.txt")
  writeLines("sibling", "ordered/a.txt")
  old_order <- sort(c("ordered/a/child.txt", "ordered/a.txt"))
  old_hashes <- vapply(old_order, function(p)
    digest::digest(file = p, algo = "sha256"), character(1))
  old_manifest <- paste0(old_order, "\t", old_hashes, "\n", collapse = "")
  expected_digest <- paste0("sha256:", digest::digest(
    old_manifest, algo = "sha256", serialize = FALSE))
  ok(identical(.gate_artifact_cell_sha256("ordered"), expected_digest),
     "I2: ordinary nested directory retains pre-fix canonical digest ordering")

  escaped <- make_signed_gate("ESCAPE", artifact = "A | B \\ C")
  parsed <- parse_gate_record(escaped)$artifacts
  got <- if (isTRUE(parsed$has_table) && is.null(parsed$error) && length(parsed$rows))
    unname(parsed$rows[[1]][["artifact"]]) else NA_character_
  ok(identical(got, "A | B \\ C"),
     "I5: rendered pipe and backslash metadata parse byte-consistently")
  malformed <- .gate_parse_artifacts(c("### Artifacts", "| artifact | path |",
                                       "|---|---|", "| dangling\\ | x |"))
  ok(isTRUE(malformed$has_table) && !is.null(malformed$error),
     "I5: malformed escape sequence is refused")

  cat("\n--- I3: errata writer honors per-ledger lock ----------------------------\n")
  f <- make_signed_gate("ERRLOCK")
  lock <- paste0(.errata_path(f), ".lock")
  dir.create(lock, recursive = TRUE)
  writeLines(c("pid: fixture", "writer: fixture"), file.path(lock, "holder"))
  expect_stop(record_erratum(f, "fixture", "corrected", "fixture evidence", "minor",
                             "Fixture", "2026-09-11"),
              "I3: held errata-ledger lock refuses a competing writer",
              "another ledger writer|holding the lock")
  ok(!file.exists(.errata_path(f)),
     "I3: refused competing writer appends no erratum")
  unlink(lock, recursive = TRUE)

  cat("\n--- I6/I7: truthful verification and supported release forms -----------\n")
  dir.create(file.path("gates", "BADNEST"), recursive = TRUE)
  writeLines("{}", file.path("gates", "BADNEST", "approvals.jsonl"))
  vr <- verify_approvals_ledger("gates")
  ok(!vr$ok && identical(vr$ledger_count, 4L) && vr$event_count >= 1L,
     "I6: recursive default verification finds nested corrupt ledger and reports counts")
  unlink(file.path("gates", "BADNEST"), recursive = TRUE)

  empty_root <- tempfile("empty-gates-")
  dir.create(empty_root)
  empty <- verify_approvals_ledger(empty_root)
  ok(isTRUE(empty$empty) && identical(empty$ledger_count, 0L) &&
       identical(empty$event_count, 0L),
     "I6: empty inventory is reported explicitly, not as verified events")
  unlink(empty_root, recursive = TRUE)

  for (shape in c("indent", "combined")) {
    id <- paste0("RELEASE-", toupper(shape))
    sig <- if (shape == "indent") c("  Signed: Fixture", "  Date: 2026-09-11")
           else "Signed: Fixture Date: 2026-09-11"
    f <- make_signed_gate(id, sig)
    suppressMessages(approve_gate(f, check_store = FALSE))
    released <- tryCatch({
      suppressMessages(release_gate(f, "fixture release", "Fixture"))
      TRUE
    }, error = function(e) {
      cat(sprintf("      %s release error: %s\n", shape, conditionMessage(e)))
      FALSE
    })
    p <- parse_gate_record(f)
    ok(released && identical(p$decision, "unsigned") && is.na(p$signer) &&
         is.na(p$date) && !nzchar(p$digest_signed),
       paste0("I7: release clears ", shape, " signer/date form"))
  }

  cat("\n--- I8: inspection is separate from throwing execution authorization ---\n")
  d <- file.path("skills", "draft-fixture")
  p <- file.path(d, "reference", "method.R")
  dir.create(dirname(p), recursive = TRUE)
  writeLines(c(
    ".mm_trusted_private <- base::local({",
    "  e <- base::new.env(parent = base::baseenv())",
    '  base::sys.source("tools/method_manifest.R", envir = e, keep.source = FALSE)',
    "  base::lockEnvironment(e, bindings = TRUE)",
    "  e",
    "}, envir = base::new.env(parent = base::baseenv()))",
    ".method_gate <- function() base::get(\"require_validated_method\", envir = .mm_trusted_private)(",
    paste0('  skill_dir = "', d, '", calling_file = "', p,
           '", unit = "method")'),
    "method <- function(x) { .method_gate(); x + 1 }"), p)
  write_method_manifest(d, list(method = list(reference_path = p, entrypoints = "method",
                                               status = "draft")))
  query_exists <- exists("is_method_validated", mode = "function")
  ok(query_exists && isFALSE(suppressWarnings(is_method_validated(d, p, "method"))),
     "I8: non-executing inspection reports valid draft as FALSE")
  env <- new.env(parent = globalenv())
  sys.source(p, envir = env, keep.source = FALSE)
  computed <- FALSE
  expect_stop(suppressWarnings({ env$method(41); computed <- TRUE }),
              "I8: discarded draft authorization result throws before computation",
              "not execution-authorized|draft|SME")
  ok(!computed, "I8: draft method body did not compute")
  aud <- audit_entrypoint_gating(d)
  ok(isTRUE(aud$gated),
     "I8: trusted throwing authorization remains a certified gate")

  cat("\n--- RC1/RC2: exact ledger history and live bindings ----------------------\n")
  # The historic attack cases above are independent of the repository below.
  for (id in c("CHAIN", "ERRCHAIN", "ERRALIAS", "ESCAPE"))
    unlink(file.path("gates", id), recursive = TRUE)
  f <- file.path("gates", "ERRLOCK", "eda.md")
  suppressMessages(approve_gate(f, check_store = FALSE))
  suppressMessages(record_erratum(f, "fixture", "first correction", "fixture evidence",
                                  "minor", "Fixture", "2026-09-11"))
  git <- function(...) {
    out <- suppressWarnings(system2("git", shQuote(c(...)), stdout = TRUE, stderr = TRUE,
                                    timeout = 30))
    if (!is.null(attr(out, "status"))) stop(paste(out, collapse = "\n"))
    out
  }
  git("init", "-q", "-b", "main")
  git("config", "user.email", "fixture@example.invalid")
  git("config", "user.name", "Fixture Reviewer")
  git("config", "core.autocrlf", "false")
  empty_path <- file.path("gates", "EMPTY", "errata.jsonl")
  dir.create(dirname(empty_path))
  stopifnot(file.create(empty_path))
  git("add", "gates", "artifact.txt")
  git("commit", "-qm", "fixture ledgers")
  base <- git("rev-parse", "HEAD")
  git("branch", "topic")

  ci <- new.env(parent = globalenv())
  sys.source(file.path(TOOLS, "ci_check.R"), envir = ci, keep.source = FALSE)
  event_names <- c("GITHUB_ACTIONS", "GITHUB_EVENT_NAME", "GITHUB_EVENT_PATH",
                   "GITHUB_SHA", "GITHUB_REF")
  prior_event <- setNames(Sys.getenv(event_names, unset = NA_character_), event_names)
  on.exit({
    Sys.unsetenv(event_names)
    present <- !is.na(prior_event)
    if (any(present)) do.call(Sys.setenv, as.list(prior_event[present]))
  }, add = TRUE)
  Sys.unsetenv(event_names)
  history <- function(sha = base) ci$.ci_check_ledger_history(explicit_base = sha)
  read_bytes <- function(path) readBin(path, "raw", file.info(path)$size)
  approval_path <- .ledger_path(f)
  errata_path <- .errata_path(f)
  paths <- c(approval_path, errata_path)
  before <- lapply(paths, read_bytes)
  suppressMessages(revoke_gate(f, "fixture revocation"))
  suppressMessages(approve_gate(f, check_store = FALSE))
  suppressMessages(record_erratum(f, "fixture", "second correction", "fixture evidence",
                                  "minor", "Fixture", "2026-09-11"))
  git("add", "gates")
  git("commit", "-qm", "append both ledger chains")
  after <- lapply(paths, read_bytes)
  ok(all(mapply(function(old, new) length(new) > length(old) &&
                  identical(head(new, length(old)), old), before, after)) &&
       length(.read_ledger(approval_path)) == 3L &&
       length(.read_ledger(errata_path)) == 2L,
     "RC1: ordinary local transactions append to both nonempty ledgers")
  status <- capture.output({ history(); ci$check_approvals_ledger(); ci$check_errata_ledger() })
  ok(any(grepl("^PASS approval and errata ledger append-only", status)) &&
       any(grepl("^PASS approval-ledger chains", status)) &&
       any(grepl("^PASS errata-ledger chains", status)),
     "RC1: production history preflight accepts both append chains and their live bindings")

  for (i in seq_along(paths)) {
    path <- paths[[i]]
    bytes <- after[[i]]
    file.remove(path)
    expect_stop(history(), paste("RC1: live ledger removal refused", basename(path)),
                "Committed ledger path removed")
    text <- rawToChar(bytes)
    for (edited in list(charToRaw(sub("Fixture", "Changed", text, fixed = TRUE)),
                        charToRaw(gsub("\n", "\r\n", text, fixed = TRUE)),
                        head(bytes, -1L))) {
      stopifnot(!identical(edited, bytes))
      writeBin(edited, path)
      expect_stop(history(), paste("RC1: live byte prefix edit refused", basename(path)),
                  "Committed ledger byte prefix edited")
    }
    writeBin(bytes, path)
  }
  file.remove(empty_path)
  expect_stop(history(), "RC1: even an empty tracked ledger cannot disappear",
              "Committed ledger path removed")
  stopifnot(file.create(empty_path))
  expect_stop(history("bad base;still-invalid"), "RC2: malformed base fails before Git lookup",
              "exact nonzero commit SHA")
  expect_stop(history(strrep("f", 40)), "RC2: absent exact comparison object fails",
              "Git could not read cat-file|readable commit object")
  expect_stop(history(git("rev-parse", "HEAD:artifact.txt")),
              "RC2: a readable blob is refused as a comparison base", "readable commit object")

  signed <- read_bytes(f)
  writeBin(c(signed, charToRaw("changed after signing\n")), f)
  expect_stop(ci$check_approvals_ledger(),
              "REVIEW-CHAIN: changed latest record refuses current binding",
              "matching latest ledger event|does not bind the current gate")
  file.remove(f)
  expect_stop(ci$check_approvals_ledger(), "REVIEW-CHAIN: missing latest record refuses",
              "missing its gate record")
  writeBin(signed, f)
  errata <- .read_ledger(errata_path)
  errata[[2L]]$record_digest <- paste0("sha256:", strrep("0", 64))
  errata[[2L]]$event_id <- .ledger_event_id(errata[[2L]], .ERRATA_FIELDS)
  writeLines(vapply(errata, .errata_to_json, character(1)), errata_path)
  expect_stop(ci$check_errata_ledger(),
              "REVIEW-CHAIN: correct chain IDs cannot bind an unknown approved version",
              "does not bind an approved gate version")
  writeBin(after[[2L]], errata_path)

  # Execute the same history preflight in a cold process, then model an Actions
  # merge ref against a genuine two-parent commit from the disposable checkout.
  writeLines(c('source("tools/ci_check.R")',
               'check_ledger_preflight(explicit_base = commandArgs(TRUE)[1L])',
               'quit(status = if (.fail_count > 0L) 1L else 0L)'), "tools/cold-ci.R")
  cold <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
                                  c("--vanilla", "tools/cold-ci.R", base),
                                  stdout = TRUE, stderr = TRUE, timeout = 30))
  ok(is.null(attr(cold, "status")) &&
       any(grepl("^PASS approval and errata ledger append-only", cold)) &&
       any(grepl("^PASS approval-ledger chains", cold)) &&
       any(grepl("^PASS errata-ledger chains", cold)),
     "RC1/2: cold CI resolves both history and current chain checks")
  main <- git("rev-parse", "HEAD")
  git("switch", "-q", "topic")
  writeLines("independent change", "topic.txt")
  git("add", "topic.txt")
  git("commit", "-qm", "topic change")
  topic <- git("rev-parse", "HEAD")
  git("switch", "-q", "main")
  git("merge", "--no-ff", "-qm", "merge topic", "topic")
  merged <- git("rev-parse", "HEAD")
  ok(any(grepl("^PASS approval and errata ledger append-only",
              capture.output(history(main)))),
     "RC2: verified ancestry accepts a normal merge")
  event_path <- file.path(root, "event.json")
  event <- list(number = 17L, pull_request = list(base = list(ref = "main", sha = main),
                                                  head = list(sha = topic)))
  jsonlite::write_json(event, event_path, auto_unbox = TRUE)
  Sys.setenv(GITHUB_ACTIONS = "true", GITHUB_EVENT_NAME = "pull_request",
             GITHUB_SHA = merged, GITHUB_REF = "refs/pull/17/merge",
             GITHUB_EVENT_PATH = event_path)
  ok(any(grepl("^PASS approval and errata ledger append-only",
              capture.output(ci$.ci_check_ledger_history()))),
     "RC2: modeled pull request accepts verified base and topic ancestry")
  event$pull_request$head$sha <- merged
  jsonlite::write_json(event, event_path, auto_unbox = TRUE)
  expect_stop(ci$.ci_check_ledger_history(), "RC2: payload cannot call the merge commit its PR head",
              "not the event merge commit")
  Sys.unsetenv(event_names)

  git("rm", "-q", empty_path, errata_path)
  git("commit", "-qm", "remove committed ledgers")
  for (removed in c(empty_path, errata_path)) {
    expect_stop(history(merged), paste("RC1: committed deletion refuses", removed),
                paste0("Committed ledger path removed: ", removed))
    if (identical(removed, empty_path)) {
      dir.create(dirname(empty_path), showWarnings = FALSE)
      stopifnot(file.create(empty_path), file.info(empty_path)$size == 0L)
      git("add", empty_path)
      git("commit", "-qm", "restore only empty ledger")
    }
  }

  cat(sprintf("\nreview-integrity battery: %d passed, %d failed\n", .pass, .fail))
  if (.fail > 0L) quit(status = 1L)
  invisible(TRUE)
}

if (.invoked_as_script()) .run_review_integrity_tests()
