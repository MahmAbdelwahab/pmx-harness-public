#!/usr/bin/env Rscript
# Fail-open fix regressions. All fixtures live under tempdir(); sourcing this
# file defines helpers only, because pipelines source tools/ recursively.

.fo_invoked_as_script <- function() {
  fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  length(fa) > 0L && identical(basename(sub("^--file=", "", fa[1])),
                               "test_failopen_fixes.R")
}

.fo_run_tests <- function() {
  file_arg <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
  tools <- normalizePath(file.path(dirname(file_arg), ".."), mustWork = TRUE)
  passed <- 0L; failed <- 0L
  .fo_case <- function(label, expr) {
    value <- tryCatch(isTRUE(force(expr)), error = function(e) {
      cat("      error:", conditionMessage(e), "\n"); FALSE
    })
    if (value) { passed <<- passed + 1L; cat("PASS  ", label, "\n", sep = "")
    } else { failed <<- failed + 1L; cat("FAIL  ", label, "\n", sep = "") }
  }
  .fo_attempt <- function(expr) {
    tryCatch({ suppressWarnings(suppressMessages(force(expr))); NULL },
             error = function(e) conditionMessage(e))
  }
  .fo_read_raw <- function(path) readBin(path, "raw", file.info(path)$size)
  env <- globalenv()
  for (file in c("gate.R", "gate_package.R", "approve_gate.R", "errata.R"))
    suppressMessages(sys.source(file.path(tools, file), envir = env))
  env$.git_head <- function() "FIXTURE-NO-GIT"
  env$.gate_package_revision <- function(gate_file) "FIXTURE-NO-GIT"

  root <- tempfile("failopen-")
  dir.create(file.path(root, "tools"), recursive = TRUE)
  stopifnot(all(file.copy(file.path(tools, c("gate.R", "gate_package.R",
                                            "approve_gate.R", "errata.R")),
                         file.path(root, "tools"))))
  old <- setwd(root)
  on.exit(setwd(old), add = TRUE)
  writeLines("fixture artifact", "artifact.txt")
  .fo_make_gate <- function(id) {
    dir.create(file.path("gates", id), recursive = TRUE)
    path <- file.path("gates", id, "eda.md")
    writeLines(c(paste("Analysis ID:", id), "Stage: eda", "## SME decision",
                 "- [ ] APPROVED", "Signed: FIXTURE", "Date: 2026-11-14",
                 "Digest signed:"), path)
    suppressMessages(env$render_gate_package(
      path, data.frame(artifact = "fixture", path = "artifact.txt",
                       stringsAsFactors = FALSE)))
    lines <- readLines(path, warn = FALSE)
    lines[lines == "- [ ] APPROVED"] <- "- [x] APPROVED"
    digest <- env$parse_gate_record(path, lines = lines)$package_digest
    lines[grepl("^Digest signed:", lines)] <- paste("Digest signed:", digest)
    writeLines(lines, path)
    path
  }
  .fo_approve <- function(path) suppressMessages(env$approve_gate(path, check_store = FALSE))
  .fo_cli <- function(tool, target = character(0)) {
    output <- suppressWarnings(system2("timeout", c(
      "300", shQuote(file.path(R.home("bin"), "Rscript")),
      shQuote(file.path("tools", tool)), "verify", shQuote(target)),
      stdout = TRUE, stderr = TRUE))
    status <- attr(output, "status")
    list(status = if (is.null(status)) 0L else status, output = output)
  }

  # ==== G1: ledger writers and verify commands ==============================
  .fo_case("D-42: unterminated ledger refuses revocation before append", {
    path <- .fo_make_gate("G1-D42")
    .fo_approve(path)
    ledger <- env$.ledger_path(path)
    bytes <- .fo_read_raw(ledger)
    stopifnot(tail(bytes, 1L) == as.raw(10L))
    bytes <- bytes[-length(bytes)]
    writeBin(bytes, ledger)
    error <- .fo_attempt(env$revoke_gate(path, "fixture revocation"))
    !is.null(error) && grepl(ledger, error, fixed = TRUE) &&
      identical(.fo_read_raw(ledger), bytes)
  })
  .fo_case("D-42 control: terminated ledger takes approve and revoke; gate stops", {
    path <- .fo_make_gate("G1-D42-CONTROL")
    .fo_approve(path)
    before <- .fo_attempt(env$require_gate(path))
    suppressMessages(env$revoke_gate(path, "fixture revocation"))
    events <- env$.read_ledger(env$.ledger_path(path))
    error <- .fo_attempt(env$require_gate(path))
    is.null(before) && length(events) == 2L &&
      identical(events[[2]]$decision, "revoked") &&
      !is.null(error) && grepl("latest event.*revoked", error)
  })

  # NA is the confirming input. The other shapes check the same string contract
  # and require the designed refusal, rather than an incidental base R error.
  .fo_invalid_args <- function(kind) {
    invalid <- list(NA_character_, NULL, character(0), "", " \t", c("a", "b"), 1, list("x"))
    names(invalid) <- c("NA", "NULL", "zero length", "empty", "whitespace", "vector", "numeric", "list")
    results <- logical(length(invalid))
    for (i in seq_along(invalid)) {
      path <- .fo_make_gate(paste0("G1-D104-", kind, "-", i))
      .fo_approve(path)
      ledger <- env$.ledger_path(path)
      before <- .fo_read_raw(ledger)
      record <- .fo_read_raw(path)
      error <- if (kind == "revoke") .fo_attempt(env$revoke_gate(path, invalid[[i]]))
               else if (kind == "reason")
                 .fo_attempt(env$release_gate(path, invalid[[i]], "FIXTURE"))
               else .fo_attempt(env$release_gate(path, "fixture release", invalid[[i]]))
      pattern <- if (kind == "revoke") "revoke_gate requires a non-empty reason"
                 else if (kind == "reason") "release_gate requires a non-empty --reason"
                 else "release_gate requires a --signer"
      results[i] <- !is.null(error) && grepl(pattern, error, fixed = TRUE) &&
        identical(before, .fo_read_raw(ledger)) && identical(record, .fo_read_raw(path))
      if (!results[i]) cat("      ", kind, " ", names(invalid)[i], ": ",
                           if (is.null(error)) "returned without error" else error, "\n", sep = "")
    }
    all(results)
  }
  .fo_case("D-104 revoke: rejects NA and malformed reasons", .fo_invalid_args("revoke"))
  .fo_case("D-104 release: rejects NA and malformed reasons", .fo_invalid_args("reason"))
  .fo_case("D-104 release: rejects NA and malformed signers", .fo_invalid_args("signer"))
  .fo_case("D-104 control: release records reason and FIXTURE signer", {
    path <- .fo_make_gate("G1-D104-CONTROL")
    .fo_approve(path)
    suppressMessages(env$release_gate(path, "fixture release", "FIXTURE"))
    events <- env$.read_ledger(env$.ledger_path(path))
    event <- events[[length(events)]]
    identical(event$decision, "release") && identical(event$reason, "fixture release") &&
      identical(event$signer, "FIXTURE")
  })

  .fo_case("D-105: dotted-path revoke records normalized ID and blocks plain path", {
    path <- .fo_make_gate("G1-D105")
    .fo_approve(path)
    dotted <- file.path(dirname(path), ".", basename(path))
    suppressMessages(env$revoke_gate(dotted, "fixture revocation"))
    events <- env$.read_ledger(env$.ledger_path(path))
    event <- events[[length(events)]]
    error <- .fo_attempt(env$require_gate(path))
    identical(event$analysis_id, "G1-D105") &&
      !is.null(error) && grepl("latest event.*revoked", error)
  })
  .fo_case("D-105 control: plain-path revoke records plain ID and blocks gate", {
    path <- .fo_make_gate("G1-D105-CONTROL")
    .fo_approve(path)
    suppressMessages(env$revoke_gate(path, "fixture revocation"))
    events <- env$.read_ledger(env$.ledger_path(path))
    error <- .fo_attempt(env$require_gate(path))
    identical(events[[length(events)]]$analysis_id, "G1-D105-CONTROL") &&
      !is.null(error) && grepl("latest event.*revoked", error)
  })

  base <- '{"schema_version":1,"event_id":"sha256:aa"}'
  extra <- '{"decision":"revoked"}'
  .fo_case("D-107: characters added to committed final line return FALSE",
           identical(env$ledger_append_only_ok(base, paste0(base, extra)), FALSE))
  .fo_case("D-107 control: a true line append returns TRUE",
           isTRUE(env$ledger_append_only_ok(base, c(base, extra))))

  for (tool in c("approve_gate.R", "errata.R")) {
    .fo_case(paste0("D-102 ", tool, ": nonexistent explicit target exits 2"), {
      result <- .fo_cli(tool, "NO-SUCH-ANALYSIS")
      identical(result$status, 2L) &&
        "no such target: NO-SUCH-ANALYSIS" %in% result$output
    })
  }
  dir.create(file.path("gates", "G1-EMPTY"))
  for (tool in c("approve_gate.R", "errata.R")) {
    .fo_case(paste0("D-102 ", tool, ": existing empty targets exit 0 with EMPTY"), {
      results <- lapply(c("G1-EMPTY", "gates/G1-EMPTY", "artifact.txt"), function(target) {
        result <- .fo_cli(tool, target)
        expected <- if (tool == "errata.R")
          paste0("errata ledger EMPTY: 0 ledger files under ", target) else "approvals ledger EMPTY:"
        identical(result$status, 0L) && any(grepl(expected, result$output, fixed = TRUE))
      })
      all(unlist(results))
    })
  }
  .fo_case("D-102 control: no-target empty verify retains existing behavior", {
    empty <- tempfile("empty-verify-", tmpdir = root)
    dir.create(empty)
    dir.create(file.path(empty, "tools"))
    stopifnot(all(file.copy(file.path(root, "tools", c("gate.R", "approve_gate.R", "errata.R")),
                           file.path(empty, "tools"))))
    .fo_without_target <- function() {
      previous <- setwd(empty); on.exit(setwd(previous), add = TRUE)
      approval <- .fo_cli("approve_gate.R")
      errata <- .fo_cli("errata.R")
      identical(approval$status, 0L) && identical(errata$status, 0L) &&
        any(grepl("approvals ledger EMPTY:", approval$output, fixed = TRUE)) &&
        "errata ledger OK" %in% errata$output
    }
    .fo_without_target()
  })

  .fo_case("D-103: unknown --claimant flag is refused by name", {
    error <- .fo_attempt(env$.errata_parse_flags(c(
      "gates/FIXTURE/eda.md", "--claimant", "CLAIMANT-TEXT", "--correction", "c",
      "--evidence", "e", "--materiality", "minor", "--signer", "FIXTURE",
      "--date", "2026-11-14")))
    !is.null(error) && grepl("--claimant", error, fixed = TRUE)
  })
  .fo_case("D-103: unknown --bogus flag is refused by name", {
    error <- .fo_attempt(env$.errata_parse_flags(c("--bogus", "x")))
    !is.null(error) && grepl("--bogus", error, fixed = TRUE)
  })
  .fo_case("D-103 control: known flags record the same erratum fields", {
    path <- .fo_make_gate("G1-VERIFY")
    .fo_approve(path)
    flags <- env$.errata_parse_flags(c(
      path, "--claim", "fixture claim", "--correction", "fixture correction",
      "--evidence", "fixture evidence", "--materiality", "minor",
      "--signer", "FIXTURE", "--date", "2026-11-14"))
    suppressMessages(env$record_erratum(
      flags[["positional"]][1], claim = flags[["claim"]], correction = flags[["correction"]],
      evidence = flags[["evidence"]], materiality = flags[["materiality"]],
      signer = flags[["signer"]], date = flags[["date"]]))
    event <- env$.read_ledger(env$.errata_path(path))[[1]]
    identical(event$claim_excerpt, "fixture claim") &&
      identical(event$correction, "fixture correction") &&
      identical(event$evidence, "fixture evidence") && identical(event$materiality, "minor") &&
      identical(event$signer, "FIXTURE") && identical(event$date, "2026-11-14")
  })
  for (tool in c("approve_gate.R", "errata.R")) {
    .fo_case(paste0("D-102 control ", tool, ": real fixture ledger verifies with exit 0"), {
      ledger <- if (tool == "approve_gate.R") "approvals.jsonl" else "errata.jsonl"
      results <- lapply(c("G1-VERIFY", "gates/G1-VERIFY", file.path("gates", "G1-VERIFY", ledger)),
                        function(target) {
        result <- .fo_cli(tool, target)
        identical(result$status, 0L) && any(grepl("ledger OK", result$output, fixed = TRUE))
      })
      all(unlist(results))
    })
  }

  # ==== G2: gate verification and package rendering =========================
  for (.fo_bad in c('{"a":"1"}{"a":"2"}', '"a":"1"}', '{"a" "1"}',
                    '{"a":"1"', '{"a":"1","a":"2"}',
                    '{"a":1e999}', '{"a":1,}', '{,"a":1}',
                    '{"a":1 "b":2}', '{"a":"unterminated}', '{"a":01}')) {
    .fo_case(paste0("D-41: rejects malformed flat object ", .fo_bad),
             !is.null(.fo_attempt(env$.json_parse_flat(.fo_bad))))
  }
  .fo_case("D-41 existing-refusal control: true and nullx already stop", {
    !is.null(.fo_attempt(env$.json_parse_flat('{"a":true}'))) &&
      !is.null(.fo_attempt(env$.json_parse_flat('{"a":nullx}')))
  })
  .fo_case("D-41 control: ledger writers round-trip strings, null and integers", {
    .fo_path <- .fo_make_gate("G2-D41-CONTROL")
    .fo_approve(.fo_path)
    .fo_event <- env$.read_ledger(env$.ledger_path(.fo_path))[[1]]
    .fo_event$reason <- "quote \" slash \\ newline\nreturn\rtab\t"
    .fo_expected <- .fo_event
    .fo_expected$schema_version <- as.numeric(.fo_expected$schema_version)
    .fo_expected$reason <- .fo_event$reason
    .fo_approval <- env$.json_parse_flat(env$.ledger_to_json(.fo_event))
    .fo_erratum <- as.list(setNames(rep("fixture", length(env$.ERRATA_FIELDS)), env$.ERRATA_FIELDS))
    .fo_erratum$schema_version <- 1
    .fo_erratum$event_id <- "sha256:fixture"
    .fo_erratum$previous_event_id <- NA_character_
    .fo_order <- c("schema_version", "event_id", env$.ERRATA_FIELDS[env$.ERRATA_FIELDS != "schema_version"])
    identical(.fo_approval, .fo_expected[c("schema_version", "event_id", env$.LEDGER_FIELDS[env$.LEDGER_FIELDS != "schema_version"])]) &&
      identical(env$.json_parse_flat(env$.errata_to_json(.fo_erratum)), .fo_erratum[.fo_order]) &&
      identical(env$.json_parse_flat(" { } \t"), list()) &&
      identical(env$.json_parse_flat('{"x":-1.25e2,"y":null}'), list(x = -125, y = NA_character_))
  })

  for (.fo_kind in c("canon1", "digestless")) {
    .fo_case(paste0("D-40 ", .fo_kind, ": allowlisted revoked record stops"), {
      .fo_path <- .fo_make_gate(paste0("G2-D40-", .fo_kind))
      if (.fo_kind == "digestless") {
        .fo_lines <- readLines(.fo_path, warn = FALSE)
        writeLines(.fo_lines[!grepl("^Package-Digest:|^Digest signed:", .fo_lines)], .fo_path)
      }
      writeLines(c("entries:", paste0("  - kind: ", .fo_kind),
                   paste0("    path: ", env$.gate_relpath(.fo_path)),
                   paste0("    sha256: ", env$.gate_sha256(.fo_path))), "tools/legacy_gates.yaml")
      suppressMessages(env$revoke_gate(.fo_path, "fixture legacy revocation"))
      .fo_error <- .fo_attempt(env$require_gate(.fo_path))
      !is.null(.fo_error) && grepl("latest event.*revoked", .fo_error)
    })
    .fo_case(paste0("D-40 ", .fo_kind, " control: no events passes; enforcement off passes"), {
      .fo_off <- .fo_attempt(env$require_gate(.fo_path, enforce_ledger = FALSE))
      .fo_control <- .fo_make_gate(paste0("G2-D40-EMPTY-", .fo_kind))
      if (.fo_kind == "digestless") {
        .fo_lines <- readLines(.fo_control, warn = FALSE)
        writeLines(.fo_lines[!grepl("^Package-Digest:|^Digest signed:", .fo_lines)], .fo_control)
      }
      writeLines(c("entries:", paste0("  - kind: ", .fo_kind),
                   paste0("    path: ", env$.gate_relpath(.fo_control)),
                   paste0("    sha256: ", env$.gate_sha256(.fo_control))), "tools/legacy_gates.yaml")
      is.null(.fo_off) && is.null(.fo_attempt(env$require_gate(.fo_control)))
    })
  }
  .fo_case("D-40 control R1: allowlisted approve, revoke, approve records and passes", {
    .fo_path <- .fo_make_gate("G2-D40-REAPPROVE")
    writeLines(c("entries:", "  - kind: canon1", paste0("    path: ", env$.gate_relpath(.fo_path)),
                 paste0("    sha256: ", env$.gate_sha256(.fo_path))), "tools/legacy_gates.yaml")
    .fo_approve(.fo_path)
    suppressMessages(env$revoke_gate(.fo_path, "fixture legacy revocation"))
    .fo_approve(.fo_path)
    .fo_events <- env$.read_ledger(env$.ledger_path(.fo_path))
    length(.fo_events) == 3L && identical(.fo_events[[3]]$decision, "approved") &&
      is.null(.fo_attempt(env$require_gate(.fo_path)))
  })
  unlink("tools/legacy_gates.yaml")

  for (.fo_prefix in c("_Generated from tool output at ", "Package-Digest:")) {
    .fo_case(paste0("D-43: rejects duplicate ", .fo_prefix), {
      .fo_path <- .fo_make_gate(paste0("G2-D43-DUP-", if (.fo_prefix == "Package-Digest:") "DIGEST" else "TIME"))
      .fo_lines <- readLines(.fo_path, warn = FALSE)
      .fo_forged <- append(.fo_lines, paste0(.fo_prefix, " FORGED"),
                           after = grep("^Package-Digest:", .fo_lines)[1])
      .fo_problems <- env$.gate_verify_content(.fo_path, .fo_forged)
      any(grepl("more than once", .fo_problems, fixed = TRUE))
    })
  }
  .fo_case("D-43 exact form: altered single timestamp reports malformed line", {
    .fo_path <- .fo_make_gate("G2-D43-TIME-FORM")
    .fo_lines <- readLines(.fo_path, warn = FALSE)
    .fo_lines[grepl("^_Generated from tool output at ", .fo_lines)] <-
      "_Generated from tool output at FORGED: the SME never saw this line"
    any(grepl("malformed generated timestamp", env$.gate_verify_content(.fo_path, .fo_lines), fixed = TRUE))
  })
  .fo_case("D-43 exact form: altered single digest spacing reports malformed line", {
    .fo_path <- .fo_make_gate("G2-D43-DIGEST-FORM")
    .fo_lines <- readLines(.fo_path, warn = FALSE)
    .fo_lines <- sub("^Package-Digest: ", "Package-Digest:   ", .fo_lines)
    any(grepl("malformed Package-Digest", env$.gate_verify_content(.fo_path, .fo_lines), fixed = TRUE))
  })
  .fo_case("D-43 control: fresh signed record verifies; canon1 digest form accepted", {
    .fo_fractional <- function() {
      .fo_options <- options(digits.secs = 3)
      on.exit(options(.fo_options), add = TRUE)
      .fo_precise <- .fo_make_gate("G2-D43-FRACTIONAL")
      !length(env$.gate_verify_content(.fo_precise, readLines(.fo_precise, warn = FALSE)))
    }
    .fo_path <- .fo_make_gate("G2-D43-CONTROL")
    .fo_lines <- readLines(.fo_path, warn = FALSE)
    .fo_canon1 <- sub("^Package-Digest: canon2:", "Package-Digest: ", .fo_lines)
    !length(env$.gate_verify_content(.fo_path, .fo_lines)) &&
      !length(env$.gate_verify_content(.fo_path, .fo_canon1)) && .fo_fractional()
  })

  .fo_case("D-44 verify: sha256 artifacts table without path is malformed", {
    .fo_path <- .fo_make_gate("G2-D44-VERIFY")
    .fo_lines <- readLines(.fo_path, warn = FALSE)
    .fo_lines <- sub("| path |", "| file |", .fo_lines, fixed = TRUE)
    .fo_block <- .fo_lines[which(.fo_lines == env$GATE_BLOCK_START):which(.fo_lines == env$GATE_BLOCK_END)]
    .fo_lines[grepl("^Package-Digest:", .fo_lines)] <- paste0("Package-Digest: ", env$.gate_canon2_digest(.fo_block))
    any(grepl("artifacts table malformed", env$.gate_verify_content(.fo_path, .fo_lines), fixed = TRUE))
  })
  .fo_case("D-44 render: nonempty artifacts without path stops before writing", {
    .fo_path <- .fo_make_gate("G2-D44-RENDER")
    .fo_lines <- readLines(.fo_path, warn = FALSE)
    writeLines(sub("[x] APPROVED", "[ ] APPROVED", .fo_lines, fixed = TRUE), .fo_path)
    .fo_before <- .fo_read_raw(.fo_path)
    .fo_error <- .fo_attempt(env$render_gate_package(.fo_path, data.frame(artifact = "x", file = "artifact.txt")))
    !is.null(.fo_error) && grepl("path", .fo_error, fixed = TRUE) && identical(.fo_before, .fo_read_raw(.fo_path))
  })
  .fo_case("D-44 control: valid artifacts verify and zero-row render is unchanged", {
    .fo_path <- .fo_make_gate("G2-D44-CONTROL")
    .fo_clean <- !length(env$.gate_verify_content(.fo_path, readLines(.fo_path, warn = FALSE)))
    .fo_lines <- readLines(.fo_path, warn = FALSE)
    writeLines(sub("[x] APPROVED", "[ ] APPROVED", .fo_lines, fixed = TRUE), .fo_path)
    .fo_clean && is.null(.fo_attempt(env$render_gate_package(.fo_path, data.frame(artifact = character(0)))))
  })

  .fo_columns <- list(date = as.Date("2026-01-01"), time = as.POSIXct("2026-01-01", tz = "UTC"),
                      time_list = as.POSIXlt("2026-01-01", tz = "UTC"),
                      duration = as.difftime(1, units = "days"), matrix = matrix(1:4, 2),
                      character_matrix = matrix(c("a", "b"), 1))
  for (.fo_name in names(.fo_columns)) {
    .fo_case(paste0("D-110: rejects ", .fo_name, " column with character-input advice"), {
      .fo_error <- .fo_attempt(env$.md_render_col(.fo_columns[[.fo_name]], .fo_name))
      !is.null(.fo_error) && grepl(.fo_name, .fo_error, fixed = TRUE) &&
        grepl("character", .fo_error, fixed = TRUE)
    })
  }
  .fo_case("D-110 control: character dates and supported scalar types keep bytes", {
    identical(env$md_table(data.frame(d = "2026-01-01")),
              c("| d |", "|---|", "| 2026-01-01 |")) &&
      identical(env$.md_render_col(factor(c("a", NA))), c("a", env$GATE_NA_TOKEN)) &&
      identical(env$.md_render_col(c(TRUE, FALSE, NA)), c("TRUE", "FALSE", env$GATE_NA_TOKEN)) &&
      identical(env$.md_render_col(c(1L, NA_integer_)), c("1", env$GATE_NA_TOKEN)) &&
      identical(env$.md_render_col(c(1, 0.5, NA_real_, NaN, Inf, -Inf)),
                c("1", "0.5", env$GATE_NA_TOKEN, "NaN", "Inf", "-Inf"))
  })

  .fo_case("D-111: missing SME heading on rerender stops with file unchanged", {
    .fo_path <- .fo_make_gate("G2-D111")
    .fo_lines <- readLines(.fo_path, warn = FALSE)
    .fo_lines <- sub("[x] APPROVED", "[ ] APPROVED", .fo_lines, fixed = TRUE)
    .fo_lines[.fo_lines == "## SME decision"] <- "## SME Decision"
    writeLines(c(.fo_lines, "- [x] REJECTED", "SME prose: the fit is not acceptable."), .fo_path)
    .fo_before <- .fo_read_raw(.fo_path)
    .fo_error <- .fo_attempt(env$render_gate_package(.fo_path, data.frame(artifact = "fixture", path = "artifact.txt")))
    !is.null(.fo_error) && grepl("## SME decision", .fo_error, fixed = TRUE) &&
      identical(.fo_before, .fo_read_raw(.fo_path))
  })
  .fo_case("D-111 control: two rerenders preserve the decision tail bytes", {
    .fo_path <- .fo_make_gate("G2-D111-CONTROL")
    .fo_lines <- readLines(.fo_path, warn = FALSE)
    .fo_lines <- sub("[x] APPROVED", "[ ] APPROVED", .fo_lines, fixed = TRUE)
    writeLines(c(.fo_lines, "- [x] REJECTED", "SME prose: preserve this text."), .fo_path)
    .fo_tail <- function(path) {
      .fo_bytes <- rawToChar(.fo_read_raw(path))
      charToRaw(sub("(?s)^.*(?=## SME decision)", "", .fo_bytes, perl = TRUE))
    }
    .fo_before <- .fo_tail(.fo_path)
    .fo_results <- replicate(2, {
      suppressMessages(env$render_gate_package(.fo_path, data.frame(artifact = "fixture", path = "artifact.txt")))
      identical(.fo_before, .fo_tail(.fo_path))
    })
    all(.fo_results)
  })

  for (.fo_paths in list(c("artifact.txt", "gone.yaml"), "gone.yaml")) {
    .fo_case(paste0("D-112: missing manifest path stops for ", paste(.fo_paths, collapse = ",")), {
      .fo_error <- .fo_attempt(env$.manifest_set_digest(.fo_paths))
      !is.null(.fo_error) && grepl("gone.yaml", .fo_error, fixed = TRUE)
    })
  }
  .fo_case("D-112 control: existing manifest digest matches pristine function; empty is NULL", {
    .fo_manifest_before <- function(paths) {
      paths <- sort(paths[file.exists(paths)])
      if (!length(paths)) return(NULL)
      payload <- paste(vapply(paths, function(p)
        paste0(p, "\n", digest::digest(file = p, algo = "sha256"), "\n"), character(1)),
        collapse = "")
      paste0("sha256:", digest::digest(payload, algo = "sha256", serialize = FALSE))
    }
    writeLines("fixture: 1", "m1.yaml")
    .fo_paths <- c("m1.yaml", "artifact.txt")
    identical(env$.manifest_set_digest(.fo_paths), .fo_manifest_before(.fo_paths)) &&
      is.null(env$.manifest_set_digest(character(0)))
  })

  # Remediation round 1 (review F1, F2): the parser accepts every line the
  # ledger writers emit, in any locale, and names the line it refuses.
  .fo_reason <- paste0("esc", intToUtf8(27), " ff", intToUtf8(12), " bs",
                       intToUtf8(8), " caf", intToUtf8(233))
  .fo_case("D-41 F1: control and non-ASCII characters round-trip through the ledger writer", {
    .fo_path <- .fo_make_gate("G2-D41-F1-RT")
    .fo_approve(.fo_path)
    .fo_event <- env$.read_ledger(env$.ledger_path(.fo_path))[[1]]
    .fo_event$reason <- .fo_reason
    identical(env$.json_parse_flat(env$.ledger_to_json(.fo_event))$reason, .fo_reason)
  })
  .fo_case("D-41 F1: the writer's line parses to the same bytes under LC_ALL=C", {
    .fo_path <- .fo_make_gate("G2-D41-F1-C")
    .fo_approve(.fo_path)
    .fo_event <- env$.read_ledger(env$.ledger_path(.fo_path))[[1]]
    .fo_event$reason <- .fo_reason
    .fo_line <- file.path(getwd(), "f1-line.jsonl")
    .fo_out <- file.path(getwd(), "f1-out.bin")
    .fo_child <- file.path(getwd(), "f1-child.R")
    writeBin(c(charToRaw(enc2utf8(env$.ledger_to_json(.fo_event))), as.raw(10L)), .fo_line)
    writeLines(c(
      "if (!identical(Sys.getlocale('LC_CTYPE'), 'C')) quit(status = 3L)",
      "sys.source(file.path('tools', 'gate.R'), envir = globalenv())",
      sprintf("x <- .json_parse_flat(readLines(%s, warn = FALSE))", deparse(.fo_line)),
      sprintf("writeBin(charToRaw(x$reason), %s)", deparse(.fo_out))), .fo_child)
    .fo_output <- suppressWarnings(system2("env", c(
      "LC_ALL=C", "LANG=C", "timeout", "300",
      shQuote(file.path(R.home("bin"), "Rscript")), shQuote(.fo_child)),
      stdout = TRUE, stderr = TRUE))
    if (!is.null(attr(.fo_output, "status"))) cat("      child:", paste(tail(.fo_output, 2L), collapse = " | "), "\n")
    is.null(attr(.fo_output, "status")) && file.exists(.fo_out) &&
      identical(.fo_read_raw(.fo_out), charToRaw(enc2utf8(.fo_reason)))
  })
  .fo_case("D-41 F1 control: concatenated objects and an unsupported escape still stop", {
    !is.null(.fo_attempt(env$.json_parse_flat('{"a":"1"}{"a":"2"}'))) &&
      !is.null(.fo_attempt(env$.json_parse_flat('{"a":"\\u00e9"}')))
  })
  .fo_case("D-41 F2: a malformed ledger line is named by ledger path and line number", {
    .fo_path <- .fo_make_gate("G2-D41-F2")
    .fo_approve(.fo_path)
    .fo_ledger <- env$.ledger_path(.fo_path)
    stopifnot(length(readLines(.fo_ledger, warn = FALSE)) == 1L)
    cat('{"a":"1"}{"a":"2"}\n', file = .fo_ledger, append = TRUE)
    .fo_error <- .fo_attempt(env$.read_ledger(.fo_ledger))
    !is.null(.fo_error) && grepl(.fo_ledger, .fo_error, fixed = TRUE) &&
      grepl("line 2", .fo_error, fixed = TRUE) &&
      grepl("invalid ledger JSON: trailing content after object", .fo_error, fixed = TRUE)
  })


  # ==== G4: review evidence must be present before it can pass ===============
  for (.fo_file in c("method_manifest.R", "review_stage.R"))
    suppressMessages(sys.source(file.path(tools, .fo_file), envir = env))
  .fo_empty_manifest <- data.frame(name = character(0), command = character(0))
  .fo_manifest_lines <- c("# result: PASS", "# notes-expected: 3", "# notes-found: 3")
  .fo_census_name <- "Number census of the report sources"
  .fo_e7_name <- "Translation equivalence check (E7)"
  .fo_e7_finding <- "the simulation translation check is absent or outside its tolerance"
  .fo_decl_path <- "review-fixture.R"
  writeLines(c(
    "compose_pipeline(analysis_id = 'FIXTURE',",
    "  tasks = c('qualification', 'simulation', 'reporting', 'review'),",
    "  inputs = list(reporting = list(report_dir = 'report', store = '_targets_FIXTURE',",
    "    document = c(author = 'Fixture', date = '2026-11-14', version = 'draft'),",
    "    packages = 'qualification', number_census = list(patterns = character(0),",
    "      phrases = character(0), literals = character(0), functions = character(0)))))"),
    .fo_decl_path)
  .fo_decl <- env$rv_read_declaration(.fo_decl_path)
  dir.create("report")
  writeLines("Fixture report", "report/mar.qmd")
  .fo_report_sources <- "report/mar.qmd"
  .fo_packages <- function(e7 = data.frame(max_rel_err = 0, tolerance = 0.01),
                           heading = .fo_e7_name)
    list(simulation_package = list(tables = setNames(list(e7), heading)))
  .fo_audit <- function(pk) env$rv_validation_audit(
    .fo_empty_manifest, .fo_decl$tasks, character(0), pk, character(0))
  .fo_has <- function(outcomes, text, rule = NULL) {
    if (is.null(outcomes) || !nrow(outcomes)) return(FALSE)
    any(grepl(text, outcomes$finding, fixed = TRUE) &
          (is.null(rule) | outcomes$rule %in% rule))
  }
  .fo_promote <- function(tab, promoted = data.frame(
      target = "t", source = "report_source_files", selection = "sources"),
      extra = list(), reporting = .fo_decl$inputs$reporting) {
    if (nrow(promoted) && any(startsWith(promoted$source, "report_source_files")))
      extra <- c(extra, list(t = .fo_report_sources))
    tables <- c(list("Promoted values and their sources" = promoted),
                if (!is.null(tab)) setNames(list(tab), .fo_census_name))
    values <- c(extra, list(reporting_package = list(tables = tables)))
    env$rv_promotion_check(values$reporting_package$tables[["Promoted values and their sources"]],
                           values, character(0), reporting, .fo_manifest_lines)
  }
  .fo_no_promotions <- data.frame(target = character(0), source = character(0), selection = character(0))

  .fo_case("D-27: renamed translation error and tolerance columns file validation", {
    .fo_has(.fo_audit(.fo_packages(data.frame(max_relative_error = 0.5, tol = 0.01)))$outcomes,
            .fo_e7_finding, "validation")
  })
  .fo_case("D-27: absent or empty translation evidence files validation", {
    absent <- .fo_audit(list())
    empty <- .fo_audit(.fo_packages(data.frame(max_rel_err = numeric(0), tolerance = numeric(0))))
    .fo_has(absent$outcomes, .fo_e7_finding, "validation") &&
      .fo_has(empty$outcomes, .fo_e7_finding, "validation")
  })
  .fo_case("D-27: zero-row census files promotion", {
    .fo_r <- .fo_promote(data.frame(source = character(0), undeclared = integer(0)))
    isFALSE(.fo_r$promotions$match) && .fo_has(.fo_r$outcomes, "promotion t", "promotion") &&
      .fo_has(.fo_r$outcomes, "number census", "promotion")
  })
  .fo_case("D-27 control: correct passing columns file no verdict outcome", {
    .fo_a <- .fo_audit(.fo_packages())
    .fo_r <- .fo_promote(data.frame(source = "fixture", undeclared = 0L))
    !nrow(.fo_a$outcomes) &&
      !nrow(.fo_r$outcomes) && all(.fo_r$promotions$match)
  })
  .fo_case("D-27 control: correct failing columns file each verdict outcome", {
    .fo_a <- .fo_audit(.fo_packages(data.frame(max_rel_err = 0.5, tolerance = 0.01)))
    .fo_r <- .fo_promote(data.frame(source = "fixture", undeclared = 1L))
    .fo_has(.fo_a$outcomes, .fo_e7_finding, "validation") &&
      .fo_has(.fo_r$outcomes, "promotion t", "promotion")
  })
  .fo_case("D-27: NA and wrong-type verdict columns file outcomes", {
    .fo_bad <- list(data.frame(max_rel_err = NA_real_, tolerance = 0.01),
                    data.frame(max_rel_err = 0, tolerance = NA_real_),
                    data.frame(max_rel_err = "0", tolerance = "1"))
    all(vapply(.fo_bad, function(x)
      .fo_has(.fo_audit(.fo_packages(x))$outcomes, .fo_e7_finding, "validation"), logical(1))) &&
      all(vapply(list(NA_real_, "0"), function(x)
        .fo_has(.fo_promote(data.frame(undeclared = x))$outcomes, "promotion t"), logical(1)))
  })
  .fo_case("D-27: duplicate verdict columns file validation", {
    .fo_e7 <- data.frame(a = 0, b = 0, tolerance = 1)
    names(.fo_e7)[1:2] <- "max_rel_err"
    .fo_has(.fo_audit(.fo_packages(.fo_e7))$outcomes, .fo_e7_finding, "validation")
  })
  .fo_case("D-27: zero-row or duplicated census column is not a clean summary 0", {
    .fo_dup <- data.frame(a = 0L, b = 0L)
    names(.fo_dup) <- c("undeclared", "undeclared")
    all(vapply(list(data.frame(undeclared = integer(0)), .fo_dup), function(tab) {
      r <- .fo_promote(tab, .fo_no_promotions)
      !identical(r$numbers$value[1], "0") && isFALSE(r$numbers$match[1]) &&
        .fo_has(r$outcomes, "number census: sources with undeclared tokens", "promotion")
    }, logical(1)))
  })
  .fo_case("D-27: unnumbered translation heading passes or fails on its evidence", {
    heading <- "Translation equivalence check"
    good <- .fo_audit(.fo_packages(heading = heading))
    bad <- .fo_audit(.fo_packages(data.frame(max_rel_err = 0.5, tolerance = 0.01), heading))
    !nrow(good$outcomes) && .fo_has(bad$outcomes, .fo_e7_finding, "validation")
  })
  .fo_case("D-28: absent declared census NA is a mismatch and files promotion", {
    .fo_r <- .fo_promote(NULL, .fo_no_promotions)
    identical(.fo_r$numbers$check[1], "number census: sources with undeclared tokens") &&
      is.na(.fo_r$numbers$value[1]) && isFALSE(.fo_r$numbers$match[1]) &&
      .fo_has(.fo_r$outcomes, "number census", "promotion")
  })
  .fo_case("D-28 control: present zero census is a match", {
    .fo_r <- .fo_promote(data.frame(undeclared = 0L), .fo_no_promotions)
    isTRUE(.fo_r$numbers$match[1]) && !nrow(.fo_r$outcomes)
  })
  .fo_case("D-28 control: reporting without a census declaration has no census finding", {
    reporting <- .fo_decl$inputs$reporting
    reporting$number_census <- NULL
    .fo_r <- .fo_promote(NULL, .fo_no_promotions, reporting = reporting)
    !any(startsWith(.fo_r$numbers$check, "number census")) && !nrow(.fo_r$outcomes)
  })
  .fo_case("D-28: report promotion of qualification requires the bound stage package", {
    stage <- .fo_decl$inputs$reporting$packages
    source <- paste0(stage, "_package")
    promoted <- data.frame(target = "report_stage_packages", source = source,
                           selection = "Stage package")
    package <- list(analysis = .fo_decl$analysis_id, task = stage, notes = "fixture note")
    report <- list(report_stage_packages = setNames(list(package), stage))
    match <- .fo_promote(data.frame(undeclared = 0L), promoted,
                         c(report, setNames(list(package), source)))
    absent <- .fo_promote(data.frame(undeclared = 0L), promoted, report)
    package$notes <- "different note"
    changed <- .fo_promote(data.frame(undeclared = 0L), promoted,
                           c(report, setNames(list(package), source)))
    isTRUE(match$promotions$match) && !nrow(match$outcomes) &&
      isFALSE(absent$promotions$match) && isFALSE(changed$promotions$match) &&
      .fo_has(absent$outcomes, "promotion report_stage_packages", "promotion") &&
      .fo_has(changed$outcomes, "promotion report_stage_packages", "promotion")
  })

  for (.fo_kind in c("pdf", "deck")) {
    .fo_case(paste0("D-29 ", .fo_kind, ": empty extractions compare FALSE"), {
      .fo_empty <- if (.fo_kind == "pdf") character(0) else list()
      .fo_text <- if (.fo_kind == "pdf") "fixture 1" else list("fixture 1")
      isFALSE(env$rv_text_compare(.fo_empty, .fo_empty, .fo_kind)$outcome) &&
        isFALSE(env$rv_text_compare(.fo_empty, .fo_text, .fo_kind)$outcome) &&
        isFALSE(env$rv_text_compare(.fo_text, .fo_empty, .fo_kind)$outcome)
    })
    .fo_case(paste0("D-29 ", .fo_kind, " control: identical non-empty text compares TRUE"), {
      .fo_text <- if (.fo_kind == "pdf") "fixture 1" else list("fixture 1")
      isTRUE(env$rv_text_compare(.fo_text, .fo_text, .fo_kind)$outcome)
    })
  }
  # Exercise the existing local extraction functions without launching tools.
  .fo_extract <- function(name, reply) {
    .fo_e <- new.env(parent = env)
    .fo_e$system2 <- function(...) reply(...)
    .fo_e$spec <- list()
    .fo_defs <- as.list(body(env$rv_compare_files))[-1L]
    .fo_def <- Filter(function(x) is.call(x) && identical(x[[1L]], as.name("<-")) &&
                        identical(x[[2L]], as.name(name)), .fo_defs)
    stopifnot(length(.fo_def) == 1L)
    eval(.fo_def[[1L]], .fo_e)
    .fo_e[[name]]("fixture")
  }
  .fo_case("D-29 pdf wrapper: failed tool output is discarded", {
    .fo_r <- .fo_extract("pdf_lines", function(...) structure("fixture 1", status = 1L))
    identical(.fo_r$raw, character(0)) && identical(.fo_r$lines, character(0))
  })
  .fo_case("D-29 pdf wrapper control: successful output is retained", {
    .fo_r <- .fo_extract("pdf_lines", function(...) "fixture 1")
    identical(.fo_r$raw, "fixture 1") && identical(.fo_r$lines, "fixture 1")
  })
  .fo_case("D-29 deck wrapper: failed listing is discarded", {
    identical(.fo_extract("slides", function(...) structure("ppt/slides/slide1.xml", status = 1L)),
              character(0))
  })
  .fo_case("D-29 deck wrapper: failed listing with slide names is discarded when extraction succeeds", {
    .fo_r <- .fo_extract("slides", function(command, args, ...) {
      if (args[1] == "-Z1") structure("ppt/slides/slide1.xml", status = 1L)
      else "<a:t>fixture 1</a:t>"
    })
    length(.fo_r) == 0L
  })
  .fo_case("D-29 deck wrapper: failed slide extraction is discarded", {
    identical(.fo_extract("slides", function(command, args, ...) {
      if (args[1] == "-Z1") "ppt/slides/slide1.xml"
      else structure("<a:t>fixture 1</a:t>", status = 1L)
    }), character(0))
  })
  .fo_case("D-29 deck wrapper control: successful slide extraction is retained", {
    identical(.fo_extract("slides", function(command, args, ...) {
      if (args[1] == "-Z1") "ppt/slides/slide1.xml" else "<a:t>fixture 1</a:t>"
    }), list("fixture 1"))
  })

  .fo_heading <- data.frame(target = "t_absent", source = "x_package", selection = "Heading")
  .fo_case("D-30: absent target and heading are no match", {
    .fo_r <- .fo_promote(data.frame(undeclared = 0L), .fo_heading)
    isFALSE(.fo_r$promotions$match) && .fo_has(.fo_r$outcomes, "promotion t_absent", "promotion")
  })
  .fo_case("D-30: absent target and notes or declared field are no match", {
    all(vapply(c("notes", "document"), function(sel) {
      p <- data.frame(target = "t", source = if (sel == "notes") "x_package" else "the analysis declaration",
                      selection = sel)
      r <- .fo_promote(data.frame(undeclared = 0L), p)
      isFALSE(r$promotions$match) && .fo_has(r$outcomes, "promotion t", "promotion")
    }, logical(1)))
  })
  .fo_case("D-30 control: present identical heading passes; differing heading fails", {
    .fo_p <- list(x_package = list(tables = list(Heading = data.frame(x = 1))))
    .fo_good <- .fo_promote(data.frame(undeclared = 0L), .fo_heading,
                            c(list(t_absent = data.frame(x = 1)), .fo_p))
    .fo_bad <- .fo_promote(data.frame(undeclared = 0L), .fo_heading,
                           c(list(t_absent = data.frame(x = 2)), .fo_p))
    isTRUE(.fo_good$promotions$match) && !nrow(.fo_good$outcomes) &&
      isFALSE(.fo_bad$promotions$match) && .fo_has(.fo_bad$outcomes, "promotion t_absent")
  })
  for (.fo_selection in c("artifacts rows labelled: a; b", "Heading: the rows of run r1")) {
    .fo_case(paste0("D-30: empty selection fails for ", .fo_selection), {
      .fo_p <- data.frame(target = "t", source = "x_package", selection = .fo_selection)
      .fo_tab <- if (startsWith(.fo_selection, "artifacts"))
        data.frame(artifact = character(0), path = character(0)) else
          data.frame(run = character(0), value = numeric(0))
      .fo_pk <- if (startsWith(.fo_selection, "artifacts")) list(artifacts = .fo_tab) else
        list(tables = list(Heading = .fo_tab))
      .fo_r <- .fo_promote(data.frame(undeclared = 0L), .fo_p, list(t = .fo_tab, x_package = .fo_pk))
      isFALSE(.fo_r$promotions$match) && .fo_has(.fo_r$outcomes, "promotion t")
    })
    .fo_case(paste0("D-30 control: expected selection passes for ", .fo_selection), {
      .fo_p <- data.frame(target = "t", source = "x_package", selection = .fo_selection)
      .fo_tab <- if (startsWith(.fo_selection, "artifacts"))
        data.frame(artifact = c("a", "b"), path = c("a.png", "b.png")) else
          data.frame(run = "r1", value = 1)
      .fo_pk <- if (startsWith(.fo_selection, "artifacts")) list(artifacts = .fo_tab) else
        list(tables = list(Heading = .fo_tab))
      .fo_r <- .fo_promote(data.frame(undeclared = 0L), .fo_p, list(t = .fo_tab, x_package = .fo_pk))
      isTRUE(.fo_r$promotions$match) && !nrow(.fo_r$outcomes)
    })
  }
  .fo_case("D-30: labels a; b with only a in artifacts are no match", {
    .fo_p <- data.frame(target = "t", source = "x_package", selection = "artifacts rows labelled: a; b")
    .fo_tab <- data.frame(artifact = "a", path = "a.png")
    .fo_r <- .fo_promote(data.frame(undeclared = 0L), .fo_p,
                         list(t = .fo_tab, x_package = list(artifacts = .fo_tab)))
    isFALSE(.fo_r$promotions$match) && .fo_has(.fo_r$outcomes, "promotion t", "promotion")
  })
  .fo_case("D-30: absent target from report_source_files with a clean census is no match", {
    .fo_r <- .fo_promote(data.frame(undeclared = 0L),
                         data.frame(target = "t_absent", source = "report_source_files", selection = "x"))
    isFALSE(.fo_r$promotions$match) && .fo_has(.fo_r$outcomes, "promotion t_absent", "promotion")
  })
  .fo_case("D-30: NULL target and NULL source are no match in every guarded form", {
    .fo_p <- data.frame(target = paste0("t", 1:7),
      source = c("x_package", "x_package", "da_map_file", "the analysis declaration",
                 "x_package", "x_package", "x_package"),
      selection = c("Heading", "notes", "plan", "reference_regimen", "artifacts rows labelled: a",
                    "artifacts row labelled: a", "Heading: the rows of run r1"))
    .fo_values <- list(reporting_package = list(tables = setNames(
      list(data.frame(undeclared = 0L)), .fo_census_name)))
    .fo_r <- env$rv_promotion_check(.fo_p, .fo_values, NULL, list(), .fo_manifest_lines)
    identical(.fo_r$promotions$match, rep(FALSE, 7L)) &&
      all(vapply(.fo_p$target, function(t) .fo_has(.fo_r$outcomes, paste0("promotion ", t, " "), "promotion"),
                 logical(1)))
  })
  .fo_case("D-31: empty log files raw TeX log is empty", {
    .fo_r <- env$rv_render_census(character(0), character(0), "x", 0L, TRUE)
    .fo_has(.fo_r$outcomes, "raw TeX log is empty", "census_threshold")
  })
  .fo_case("D-31 control: nonempty clean log files no outcome", {
    !nrow(env$rv_render_census(character(0), "fixture clean log", "x", 0L, TRUE)$outcomes)
  })

  .fo_record <- .fo_make_gate("G4-D32")
  .fo_lines <- readLines(.fo_record, warn = FALSE)
  writeLines(sub("[x] APPROVED", "[ ] APPROVED", .fo_lines, fixed = TRUE), .fo_record)
  suppressMessages(env$render_gate_package(.fo_record,
    data.frame(artifact = "fixture", path = "artifact.txt"),
    manifest_digest = paste0("sha256:", strrep("b", 64)),
    attestation = list(fingerprint = paste0("sha256:", strrep("c", 64)),
                       nonmem_provision = "fixture", r_provision = "fixture")))
  .fo_lines <- readLines(.fo_record, warn = FALSE)
  .fo_lines <- sub("[ ] APPROVED", "[x] APPROVED", .fo_lines, fixed = TRUE)
  .fo_lines[grepl("^Digest signed:", .fo_lines)] <- paste("Digest signed:",
    env$parse_gate_record(.fo_record, lines = .fo_lines)$package_digest)
  writeLines(.fo_lines, .fo_record)
  .fo_approve(.fo_record)
  .fo_receipt <- paste("HEAD", strrep("a", 40))
  .fo_integrity <- function(records, receipt) env$rv_gate_integrity(
    records, env$.ledger_path(.fo_record), receipt, setNames(character(0), character(0)),
    "eda", "eda", "G4-D32")
  .fo_case("D-32: signed record without history files finding", {
    .fo_r <- .fo_integrity(.fo_record, .fo_receipt)
    .fo_has(.fo_r$outcomes, .fo_record, "not_evaluated") &&
      "domain 3 bullet 2" %in% .fo_r$outcomes$checklist_item
  })
  .fo_case("D-32: zero signed records files no signed record was examined", {
    .fo_has(.fo_integrity(character(0), .fo_receipt)$outcomes,
            "no signed record was examined", "gate_record")
  })
  .fo_case("D-32 control: every record has a history line and files nothing", {
    !nrow(.fo_integrity(.fo_record, c(.fo_receipt,
      paste("record", .fo_record, "commits 1 after-sign 0")))$outcomes)
  })

  dir.create(file.path("skills", "fxskill", "reference"), recursive = TRUE)
  .fo_ref <- "skills/fxskill/reference/fx.R"
  writeLines("fx_fn <- function() 1", .fo_ref)
  writeLines("# fxskill (fixture)", "skills/fxskill/SKILL.md")
  .fo_skill_manifest <- function(reviewer, reviewed_at) {
    writeLines(c("skill: fxskill", "generated_at: 2026-11-14T00:00:00Z",
      paste0('reviewed_by: "', reviewer, '"'), paste0('reviewed_at: "', reviewed_at, '"'),
      "units:", "  u1:", "    status: approved", paste0("    reference_path: ", .fo_ref),
      "    entrypoints: [fx_fn]", paste0("    sha256: ", env$.gate_sha256(.fo_ref)),
      "    evidence: skills/fxskill/SKILL.md", '    reason: "fixture"'), "skills/fxskill/MANIFEST.yaml")
    env$validate_method_manifest("skills/fxskill/MANIFEST.yaml")
  }
  .fo_skill_audit <- function(records = c(simulation = .fo_record)) env$rv_validation_audit(
    data.frame(name = "sim_out", command = paste0("fx_fn(source('", .fo_ref, "'))")),
    "simulation", records, .fo_packages(), "fxskill")
  .fo_skill_manifest("", "")
  .fo_case("D-33: unconfirmed manifest does not approve stored approved unit", {
    .fo_a <- .fo_skill_audit()
    .fo_has(.fo_a$outcomes, "skill fxskill: unit(s) not approved", "validation") &&
      .fo_a$table$statuses[.fo_a$table$skill == "fxskill"] != "approved"
  })
  .fo_case("D-33: unconfirmed manifest with no named unit does not read as all approved", {
    .fo_a <- env$rv_validation_audit(.fo_empty_manifest, "simulation", c(simulation = .fo_record),
                                     .fo_packages(), "fxskill")
    .fo_has(.fo_a$outcomes, "skill fxskill: no unit is named", "not_evaluated") &&
      identical(.fo_a$table$statuses[.fo_a$table$skill == "fxskill"], "not approved: u1")
  })
  .fo_case("D-96 reviewed_at: blank review beside dated first use files not_evaluated", {
    .fo_has(.fo_skill_audit()$outcomes, "reviewed_at", "not_evaluated")
  })
  .fo_case("D-96 struck-leg control: no dated first use files nothing new", {
    .fo_a <- .fo_skill_audit(c(simulation = NA_character_))
    !.fo_has(.fo_a$outcomes, "reviewed_at")
  })
  .fo_skill_manifest("FIXTURE", "2026-11-13")
  .fo_case("D-33/D-96 control: confirmed manifest keeps approved and predates use", {
    .fo_a <- .fo_skill_audit()
    .fo_row <- .fo_a$table[.fo_a$table$skill == "fxskill", ]
    identical(.fo_row$statuses, "approved") && isTRUE(.fo_row$reviewed_before_first_use) &&
      !.fo_has(.fo_a$outcomes, "skill fxskill")
  })
  for (.fo_inventory in list(NULL, data.frame(file = character(0), md5 = character(0)))) {
    .fo_case(paste0("D-96 inventory: ", if (is.null(.fo_inventory)) "NULL" else "zero rows",
                    " files not_evaluated"), {
      .fo_has(env$rv_inventory_check(.fo_inventory, character(0))$outcomes,
              "inventory", "not_evaluated")
    })
  }
  .fo_case("D-96 inventory control: present matching inventory files nothing", {
    .fo_inv <- data.frame(file = "artifact.txt", md5 = unname(tools::md5sum("artifact.txt")))
    !nrow(env$rv_inventory_check(.fo_inv, character(0))$outcomes)
  })
  .fo_crossdoc <- function(lines) env$rv_crossdoc(lines, c("Title", "2026-11-14", "\fpage 2"),
    "2026-11-14", 0L, "# result: PASS", c(assessment = "a", mar = "m", manifest = "s"))
  .fo_case("D-96 crossdoc: no MAR line files not_evaluated", {
    .fo_r <- .fo_crossdoc(character(0))
    isFALSE(.fo_r$table$consistent[1]) && .fo_has(.fo_r$outcomes, "MAR cross-reference", "not_evaluated")
  })
  .fo_case("D-96 crossdoc control: present MAR line files nothing", {
    .fo_r <- .fo_crossdoc("- MAR: fixture.pdf")
    isTRUE(.fo_r$table$consistent[1]) && !nrow(.fo_r$outcomes)
  })
  .fo_sources <- c("review_gate_integrity", "review_reproduction", "review_map_concordance",
                    "review_traceability", "review_validation")
  .fo_checklist <- env$rv_checklist(setNames(rep(list(list(skipped = FALSE)), length(.fo_sources)), .fo_sources))
  .fo_case("D-97: unknown checklist key stops and names it", {
    .fo_error <- .fo_attempt(env$rv_findings(env$rv_outcome("validation", "domain 4 bulet 1", "fixture", "x"),
                                           .fo_checklist))
    !is.null(.fo_error) && grepl("domain 4 bulet 1", .fo_error, fixed = TRUE)
  })
  .fo_case("D-97: empty-string checklist key stops", {
    .fo_error <- .fo_attempt(env$rv_findings(env$rv_outcome("validation", "", "fixture", "x"),
                                           .fo_checklist))
    !is.null(.fo_error) && grepl("unknown checklist_item key", .fo_error, fixed = TRUE)
  })
  .fo_case("D-97 control: known key marks row not met; NA stays outside checklist", {
    .fo_r <- env$rv_findings(rbind(env$rv_outcome("validation", "domain 4 bullet 1", "fixture", "x"),
                                  env$rv_outcome("census_threshold", NA_character_, "fixture", "x")), .fo_checklist)
    identical(.fo_r$checklist$outcome[startsWith(.fo_r$checklist$item, "domain 4 bullet 1:")], "not met") &&
      nrow(.fo_r$log) == 2L
  })


  # ==== G3: manifest audit and reference loader =============================
  stopifnot(all(file.copy(file.path(tools, c("method_manifest.R", "load_reference.R")), "tools")))
  .fo_mm <- new.env(parent = baseenv())
  sys.source("tools/method_manifest.R", envir = .fo_mm)
  .fo_lr <- new.env(parent = baseenv())
  sys.source("tools/load_reference.R", envir = .fo_lr)
  .fo_trusted <- c(
    '.mm_trusted_private <- base::local({',
    '  e <- base::new.env(parent = base::baseenv())',
    '  if (!base::file.exists("tools/method_manifest.R"))',
    '    base::stop("missing", call. = FALSE)',
    '  base::sys.source("tools/method_manifest.R", envir = e, keep.source = FALSE)',
    '  base::lockEnvironment(e, bindings = TRUE)', '  e',
    '}, envir = base::new.env(parent = base::baseenv()))')
  .fo_gate_file <- function(name, args, formal = "", prologue = character(), ep = ".alpha_gate()", bare = FALSE) {
    head <- if (bare) "require_validated_method" else 'base::get("require_validated_method", envir = private)'
    if (bare) return(sprintf('ep <- function(x) { require_validated_method(skill_dir = "skills/%s", calling_file = "skills/%s/reference/method.R", %s); x }', name, name, args))
    c(.fo_trusted, '.alpha_gate <- base::local({', '  private <- .mm_trusted_private',
      paste0('  function(', formal, ') {'), prologue,
      sprintf('    if (!%s(skill_dir = "skills/%s", calling_file = "skills/%s/reference/method.R", %s)) base::stop("no")', head, name, name, args),
      '    base::invisible(TRUE)', '  }', '}, envir = base::new.env())',
      sprintf('ep <- function(x) { %s; x }', ep))
  }
  .fo_method <- function(name, lines, ext = "R", status = "draft") {
    dir <- file.path("skills", name)
    dir.create(file.path(dir, "reference"), recursive = TRUE)
    path <- file.path(dir, "reference", paste0("method.", ext))
    writeLines(lines, path)
    writeLines(c(paste0("skill: ", name), "generated_at: 2026-11-14T00:00:00Z",
      'reviewed_by: "FIXTURE"', 'reviewed_at: "2026-11-14"', "units:", "  alpha:",
      paste0("    status: ", status), paste0("    reference_path: ", path),
      paste0("    entrypoints: [", if (ext == "R") "ep" else basename(path), "]"),
      paste0("    sha256: ", .fo_mm$.mm_file_sha256(path)), "    evidence: SKILL.md",
      '    reason: "fixture"'), file.path(dir, "MANIFEST.yaml"))
    list(dir = dir, path = path)
  }
  .fo_gated <- function(fixture) {
    result <- suppressWarnings(.fo_mm$audit_entrypoint_gating(fixture$dir))
    nrow(result) == 1L && isTRUE(result$gated[[1L]])
  }
  for (.fo_bare in c(FALSE, TRUE)) {
    .fo_kind <- if (.fo_bare) "bare" else "get"
    .fo_name <- paste0("g3-d21-", .fo_kind)
    .fo_case(paste0("D-21 ", .fo_kind, ": partial argument is refused"), {
      fixture <- .fo_method(.fo_name, .fo_gate_file(.fo_name, 'u = "beta", "alpha"', bare = .fo_bare))
      !.fo_gated(fixture)
    })
    .fo_name <- paste0(.fo_name, "-control")
    .fo_case(paste0("D-21 ", .fo_kind, " control: exact-name gate is credited"), {
      fixture <- .fo_method(.fo_name, .fo_gate_file(.fo_name, 'unit = "alpha"', bare = .fo_bare))
      .fo_gated(fixture)
    })
    if (.fo_bare) next
    .fo_name <- paste0("g3-d22-", .fo_kind)
    .fo_case(paste0("D-22 ", .fo_kind, ": reassigned forwarded unit is refused"), {
      fixture <- .fo_method(.fo_name, .fo_gate_file(.fo_name, 'unit = unit', "unit",
        '    unit <- "beta"', '.alpha_gate("alpha")', bare = .fo_bare))
      !.fo_gated(fixture)
    })
    .fo_name <- paste0(.fo_name, "-control")
    .fo_case(paste0("D-22 ", .fo_kind, " control: unchanged forwarded formal is credited"), {
      fixture <- .fo_method(.fo_name, .fo_gate_file(.fo_name, 'unit = unit', "unit",
        ep = '.alpha_gate("alpha")', bare = .fo_bare))
      .fo_gated(fixture)
    })
    # Each half of the rebound set alone: a for index is seen only by the
    # binding counts, a nested formal only by .mm_bound_names.
    .fo_case(paste0("D-22 ", .fo_kind, ": for-loop rebinding of forwarded unit is refused"), {
      .fo_name <- paste0("g3-d22-", .fo_kind, "-for")
      fixture <- .fo_method(.fo_name, .fo_gate_file(.fo_name, 'unit = unit', "unit",
        '    for (unit in "beta") NULL', '.alpha_gate("alpha")', bare = .fo_bare))
      !.fo_gated(fixture)
    })
    .fo_case(paste0("D-22 ", .fo_kind, ": nested formal named like forwarded unit is refused"), {
      .fo_name <- paste0("g3-d22-", .fo_kind, "-formal")
      fixture <- .fo_method(.fo_name, .fo_gate_file(.fo_name, 'unit = unit', "unit",
        '    .h <- function(unit) NULL', '.alpha_gate("alpha")', bare = .fo_bare))
      !.fo_gated(fixture)
    })
  }
  .fo_mm$Sys.time <- function() as.POSIXct("2026-11-14", tz = "UTC")
  .fo_clean <- .fo_method("g3-d93", "ep <- function(x) x")
  .fo_spec <- function(reason) list(alpha = list(reference_path = .fo_clean$path,
    entrypoints = "ep", status = "draft", evidence = "SKILL.md", reason = reason))
  .fo_mm$write_method_manifest(.fo_clean$dir, .fo_spec("fixture"))
  .fo_case("D-93 control: clean manifest round-trips with identical bytes", {
    path <- file.path(.fo_clean$dir, "MANIFEST.yaml")
    bytes <- .fo_read_raw(path)
    first <- .fo_mm$read_method_manifest(path)
    .fo_mm$write_method_manifest(.fo_clean$dir, .fo_spec("fixture"))
    identical(bytes, .fo_read_raw(path)) && identical(first, .fo_mm$read_method_manifest(path))
  })
  for (.fo_reason in c('see "note"', "see # note")) {
    .fo_case(paste0("D-93: reader-invalid reason preserves manifest: ", .fo_reason), {
      path <- file.path(.fo_clean$dir, "MANIFEST.yaml")
      bytes <- .fo_read_raw(path)
      error <- .fo_attempt(.fo_mm$write_method_manifest(.fo_clean$dir, .fo_spec(.fo_reason)))
      !is.null(error) && grepl("method manifest ", error, fixed = TRUE) &&
        grepl("contains", error, fixed = TRUE) && identical(bytes, .fo_read_raw(path))
    })
    # Reset the synthetic manifest so each failure is independent at baseline.
    .fo_mm$write_method_manifest(.fo_clean$dir, .fo_spec("fixture"))
  }
  .fo_case("D-93: a refused write leaves no temporary file", {
    .fo_attempt(.fo_mm$write_method_manifest(.fo_clean$dir, .fo_spec('see "note"')))
    identical(list.files(.fo_clean$dir, pattern = "^\\.MANIFEST-", all.files = TRUE),
              character(0))
  })
  .fo_mm$write_method_manifest(.fo_clean$dir, .fo_spec("fixture"))
  .fo_case("D-93: the temporary file is created beside the manifest", {
    seen <- new.env()
    .fo_mm$tempfile <- function(pattern = "file", tmpdir = tempdir(), fileext = "") {
      seen$tmpdir <- tmpdir
      base::tempfile(pattern, tmpdir, fileext)
    }
    error <- .fo_attempt(.fo_mm$write_method_manifest(.fo_clean$dir, .fo_spec("fixture")))
    rm("tempfile", envir = .fo_mm)
    is.null(error) && !is.null(seen$tmpdir) &&
      identical(normalizePath(seen$tmpdir), normalizePath(.fo_clean$dir))
  })
  .fo_ro <- .fo_method("g3-d93-ro", "ep <- function(x) x")
  .fo_ro_spec <- function(reason) list(alpha = list(reference_path = .fo_ro$path,
    entrypoints = "ep", status = "draft", evidence = "SKILL.md", reason = reason))
  .fo_case("D-93: a read-only manifest is not replaced", {
    path <- file.path(.fo_ro$dir, "MANIFEST.yaml")
    Sys.chmod(path, "0444")
    bytes <- .fo_read_raw(path)
    mode <- file.info(path)$mode
    error <- .fo_attempt(.fo_mm$write_method_manifest(.fo_ro$dir, .fo_ro_spec("changed")))
    after <- list(bytes = .fo_read_raw(path), mode = file.info(path)$mode)
    Sys.chmod(path, "0644")
    !is.null(error) && grepl(path, error, fixed = TRUE) &&
      grepl("read-only", error, fixed = TRUE) &&
      identical(bytes, after$bytes) && identical(mode, after$mode)
  })
  .fo_case("D-93 control: a writable manifest is replaced and a new one is created", {
    path <- file.path(.fo_ro$dir, "MANIFEST.yaml")
    .fo_mm$write_method_manifest(.fo_ro$dir, .fo_ro_spec("fixture"))
    bytes <- .fo_read_raw(path)
    .fo_mm$write_method_manifest(.fo_ro$dir, .fo_ro_spec("changed"))
    fresh <- file.path("skills", "g3-d93-new")
    dir.create(fresh)
    .fo_mm$write_method_manifest(fresh, .fo_ro_spec("fixture"))
    !identical(bytes, .fo_read_raw(path)) &&
      identical(.fo_mm$read_method_manifest(path)$units$alpha$reason, "changed") &&
      file.exists(file.path(fresh, "MANIFEST.yaml"))
  })
  .fo_code <- c("ep <- function(x) x", 'MARKER_D94 <- "evaluated at load"')
  .fo_case("D-94: non-R template is never evaluated", {
    fixture <- .fo_method("g3-d94-template", .fo_code, ext = "txt", status = "approved")
    .fo_mm$validate_method_manifest(file.path(fixture$dir, "MANIFEST.yaml"))
    target <- new.env(parent = baseenv())
    error <- .fo_attempt(.fo_lr$load_reference(fixture$dir, fixture$path, envir = target))
    !is.null(error) && grepl("template is never evaluated", error, fixed = TRUE) &&
      !exists("MARKER_D94", envir = target, inherits = FALSE)
  })
  for (.fo_ext in c("R.txt", "Rmd")) {
    .fo_case(paste0("D-94: path method.", .fo_ext, " is never evaluated"), {
      fixture <- .fo_method(paste0("g3-d94-", tolower(sub(".", "-", .fo_ext, fixed = TRUE))),
                            .fo_code, ext = .fo_ext, status = "approved")
      .fo_mm$validate_method_manifest(file.path(fixture$dir, "MANIFEST.yaml"))
      target <- new.env(parent = baseenv())
      error <- .fo_attempt(.fo_lr$load_reference(fixture$dir, fixture$path, envir = target))
      !is.null(error) && grepl("template is never evaluated", error, fixed = TRUE) &&
        !exists("MARKER_D94", envir = target, inherits = FALSE)
    })
  }
  .fo_case("D-94 control: R reference loads", {
    fixture <- .fo_method("g3-d94-r", .fo_code, status = "approved")
    target <- .fo_lr$load_reference(fixture$dir, fixture$path)
    identical(target$MARKER_D94, "evaluated at load") && identical(target$ep(1), 1)
  })


  # ==== G5: CI inventories, report census, QC and NONMEM refusals ===========
  .fo_g5 <- new.env(parent = globalenv())
  for (.fo_file in c("ci_check.R", "report_number_census.R", "record_selection.R",
                    "qc_battery.R", "nonmem_qualification.R", "nonmem_frames.R",
                    "nonmem_simulation.R", "qualification.R"))
    sys.source(file.path(tools, .fo_file), envir = .fo_g5)
  .fo_g5$`%||%` <- function(a, b) if (is.null(a)) b else a
  .fo_g5_root <- tempfile("g5-")
  dir.create(.fo_g5_root)
  .fo_g5_dir <- function(name) {
    path <- file.path(.fo_g5_root, name)
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    setwd(path)
    path
  }
  .fo_g5_output <- function(expr) {
    error <- NULL
    output <- capture.output(error <- .fo_attempt(force(expr)))
    if (!is.null(error)) stop(error)
    output
  }
  .fo_g5_status <- function(output, status, text) {
    any(startsWith(output, status) & grepl(text, output, fixed = TRUE)) &&
      (status == "PASS" || !any(startsWith(output, "PASS")))
  }
  .fo_gp <- readLines(file.path(tools, "gate_package.R"), warn = FALSE)
  .fo_start <- "<!-- BEGIN GENERATED PACKAGE -->"
  .fo_end <- "<!-- END GENERATED PACKAGE -->"
  .fo_marker_lines <- grepl("^GATE_BLOCK_(START|END)\\s*<-", .fo_gp)
  .fo_variants <- list(
    absent = .fo_gp[!.fo_marker_lines],
    single_quotes = ifelse(.fo_marker_lines, gsub('"', "'", .fo_gp, fixed = TRUE), .fo_gp),
    equals = ifelse(.fo_marker_lines, sub("<-", "=", .fo_gp, fixed = TRUE), .fo_gp),
    comment = ifelse(.fo_marker_lines, paste0(.fo_gp, ' # the "marker" constant'), .fo_gp))
  .fo_g5_markers <- function(name, lines, mangled = TRUE) {
    .fo_g5_dir(paste0("markers-", name))
    dir.create("tools"); dir.create("gates/FX", recursive = TRUE)
    file.copy(file.path(tools, "gate.R"), "tools/gate.R")
    writeLines(lines, "tools/gate_package.R")
    writeLines(c("# fixture", .fo_start, "a", if (mangled) .fo_start, "b", .fo_end),
               "gates/FX/eda.md")
    .fo_g5_output(.fo_g5$check_gate_markers())
  }
  for (.fo_v in names(.fo_variants)) {
    .fo_case(paste0("D-34 ", .fo_v, ": marker extraction fails closed"),
      .fo_g5_status(.fo_g5_markers(.fo_v, .fo_variants[[.fo_v]]), "FAIL", "gate-marker integrity"))
    .fo_case(paste0("D-34 ", .fo_v, " control: real markers pass with 1 record"),
      .fo_g5_status(.fo_g5_markers(paste0(.fo_v, "-control"), .fo_gp, FALSE), "PASS",
                    "gate-marker integrity (1 records)"))
  }
  .fo_g5_workflow <- function(name, lines) {
    .fo_g5_dir(paste0("workflow-", name))
    dir.create("tools/tests", recursive = TRUE)
    for (file in .fo_g5$.ci_battery_files()) writeLines("# fixture", file.path("tools/tests", file))
    if (!is.null(lines)) {
      dir.create(".github/workflows", recursive = TRUE)
      writeLines(lines, ".github/workflows/ci.yaml")
    }
    .fo_g5_output(.fo_g5$check_battery_registration())
  }
  .fo_wf <- readLines(file.path(tools, "..", ".github", "workflows", "ci.yaml"), warn = FALSE)
  .fo_case("D-35 absent: workflow fails without PASS", {
    .fo_g5_status(.fo_g5_workflow("absent", NULL), "FAIL", "battery registration inventory")
  })
  .fo_case("D-35 absent control: complete workflow passes", {
    .fo_g5_status(.fo_g5_workflow("absent-control", .fo_wf), "PASS", "parsed workflow profile")
  })
  .fo_case("D-35 invalid: parser message fails without PASS", {
    lines <- c("jobs:", "  fixture:", "    steps: [unclosed")
    parser <- tryCatch({ yaml::yaml.load(paste(lines, collapse = "\n")); "" },
                       error = function(e) conditionMessage(e))
    output <- .fo_g5_workflow("invalid", lines)
    .fo_g5_status(output, "FAIL", "battery registration inventory") &&
      nzchar(parser) && grepl(parser, paste(output, collapse = "\n"), fixed = TRUE)
  })
  .fo_case("D-35 invalid control: complete workflow passes", {
    .fo_g5_status(.fo_g5_workflow("invalid-control", .fo_wf), "PASS", "parsed workflow profile")
  })
  .fo_case("D-35 renamed: absent validated job fails without PASS", {
    lines <- sub("  validated-stack:", "  fixture-extra:", .fo_wf, fixed = TRUE)
    .fo_g5_status(.fo_g5_workflow("renamed", lines), "FAIL", "battery registration inventory")
  })
  .fo_case("D-35 renamed control: complete workflow passes", {
    .fo_g5_status(.fo_g5_workflow("renamed-control", .fo_wf), "PASS", "parsed workflow profile")
  })
  .fo_case("D-35 yaml missing: explicit parser failure without PASS", {
    # This is a child fixture process; only the local source environment is masked.
    parser <- .fo_g5$.ci_workflow_runs
    .fo_g5$.ci_workflow_runs <- function(...) stop("workflow parser unavailable", call. = FALSE)
    output <- tryCatch(.fo_g5_workflow("yaml-missing", .fo_wf),
                       finally = .fo_g5$.ci_workflow_runs <- parser)
    .fo_g5_status(output, "FAIL", "workflow parser unavailable")
  })
  .fo_case("D-35 yaml missing control: installed yaml checks both runners", {
    .fo_g5_status(.fo_g5_workflow("yaml-control", .fo_wf), "PASS", "parsed workflow profile")
  })
  .fo_owner_run <- function() {
    ci <- new.env(parent = globalenv())
    sys.source(file.path(tools, "ci_check.R"), envir = ci)
    ci$.CI_BATTERIES <- Filter(function(entry) identical(entry$file, "test_report_owner_review.R"), ci$.CI_BATTERIES)
    stopifnot(length(ci$.CI_BATTERIES) == 1L)
    writeLines("# fixture", "tools/tests/test_report_owner_review.R")
    ci$system2 <- function(command, args, ...) {
      ci$launched <- list(command = command, args = args)
      "OWNER_REVIEW_TESTS_COMPLETE"
    }
    output <- .fo_g5_output(ci$check_batteries())
    list(launched = ci$launched, output = output)
  }
  .fo_case("Registration: local owner-review launch includes --vanilla", {
    value <- .fo_owner_run()
    identical(value$launched$command, "Rscript") &&
      identical(value$launched$args, c("--vanilla", "tools/tests/test_report_owner_review.R"))
  })
  .fo_case("Registration control: successful owner-review child is reported PASS", {
    .fo_g5_status(.fo_owner_run()$output, "PASS", "test_report_owner_review.R")
  })
  .fo_g5_census <- function(rdir, files, texts = c(reference_regimen = "declared text")) {
    nc <- list(patterns = character(), phrases = character(),
      literals = if (rdir == "rep" && "rep/mar.qmd" %in% files) "mar.qmd|<top>|5" else character(),
      functions = character())
    prep <- .fo_g5$report_number_census_prepare(nc, rdir, files)
    command <- .fo_g5$report_number_census_command(rdir, prep, character(), texts)
    eval(command, envir = list2env(list(report_source_files = files), parent = .fo_g5))
  }
  .fo_g5_dir("census")
  dir.create("rep/sections", recursive = TRUE)
  writeLines(c("# Report", "", "```{r}", "x <- 5", "```"), "rep/mar.qmd")
  writeLines("A note with 7 numbers.", "rep/notes.md")
  for (.fo_kind in c("spelling", "md-only")) {
    .fo_case(paste0("D-36 ", .fo_kind, ": no qmd census stops"), {
      rdir <- if (.fo_kind == "spelling") "./rep" else "rep"
      files <- if (.fo_kind == "spelling") c("rep/mar.qmd", "rep/notes.md") else "rep/notes.md"
      error <- .fo_attempt(.fo_g5_census(rdir, files))
      !is.null(error) && grepl(paste0("no .qmd report source under '", rdir, "'"), error, fixed = TRUE)
    })
    .fo_case(paste0("D-36 ", .fo_kind, " control: qmd source is censused"), {
      value <- .fo_g5_census("rep", c("rep/mar.qmd", "rep/notes.md"))
      "mar.qmd" %in% value$summary$file && nrow(value$rows) > 0L
    })
  }
  .fo_helper <- function(call) c(paste0("sig3 <- function() ", call),
    "format_units <- function() NULL", "landscape <- function() NULL", "include_page <- function() NULL")
  .fo_bad_sources <- c('source("other.R")', 'source("other-file")', 'source(target)',
                      'source(paste0("tools/", "report_typography.R"))')
  for (.fo_call in .fo_bad_sources) {
    .fo_case(paste0(if (.fo_call == 'source("other.R")') "D-37 control: existing refusal of " else "D-37: helper refuses ", .fo_call), {
      writeLines(.fo_helper(.fo_call), "rep/sections/format.R")
      error <- .fo_attempt(.fo_g5_census("rep", c("rep/mar.qmd", "rep/sections/format.R")))
      !is.null(error) && grepl("call-source", error, fixed = TRUE)
    })
    .fo_case(paste0("D-37 control: typography source accepted beside ", .fo_call), {
      writeLines(.fo_helper('source("tools/report_typography.R")'), "rep/sections/format.R")
      value <- .fo_g5_census("rep", c("rep/mar.qmd", "rep/sections/format.R"))
      "sections/format.R" %in% value$summary$file
    })
  }
  # H2: parse helpers as data through the real census command; never source them.
  .fo_helper_text <- c(
    '.source_report_typography <- function(into) {',
    '  this_file <- NULL',
    '  for (i in rev(seq_len(sys.nframe()))) {',
    '    f <- sys.frame(i)$ofile',
    '    if (is.character(f) && length(f) == 1L && nzchar(f)) { this_file <- f; break }',
    '  }',
    '  candidates <- c(',
    '    if (!is.null(this_file) && file.exists(this_file))',
    '      file.path(dirname(normalizePath(this_file)), "..", "..", "..", "..",',
    '                "tools", "report_typography.R"),',
    '    "tools/report_typography.R",',
    '    if (requireNamespace("here", quietly = TRUE))',
    '      tryCatch(here::here("tools", "report_typography.R"), error = function(e) NULL))',
    '  hit <- candidates[file.exists(candidates)]',
    '  if (!length(hit)) stop("symbol-map.R: cannot locate tools/report_typography.R (the shared ",',
    '     "protect_dollars() definition); looked at: ", paste(candidates, collapse = ", "), call. = FALSE)',
    '  source(normalizePath(hit[[1]]), local = into)',
    '  invisible(hit[[1]])',
    '}')
  .fo_h2_census <- function(text, file = "symbol-map.R") {
    path <- file.path("rep/sections", file)
    writeLines(text, path)
    # Declare only numeric/operator tokens and numeric strings, across call contexts.
    # Source permission cannot be granted by literals and remains independently checked.
    pd <- utils::getParseData(parse(text = text, keep.source = TRUE))
    contexts <- unique(c("<top>", pd$text[pd$token == "SYMBOL_FUNCTION_CALL"]))
    tokens <- unique(pd$text[pd$token %in% c("NUM_CONST", "STR_CONST", "'+'", "'-'", "'*'", "'/'", "'^'", "SPECIAL")])
    literals <- if (length(tokens)) as.vector(outer(contexts, tokens, function(context, token)
      paste(paste0("sections/", file), context, token, sep = "|"))) else character()
    nc <- list(patterns = character(), phrases = character(),
      literals = c("mar.qmd|<top>|5", literals), functions = character())
    files <- c("rep/mar.qmd", path)
    prep <- .fo_g5$report_number_census_prepare(nc, "rep", files)
    command <- .fo_g5$report_number_census_command("rep", prep, character(), c(reference_regimen = "declared text"))
    eval(command, envir = list2env(list(report_source_files = files), parent = .fo_g5))
  }
  .fo_case("H2 local canonical helper: source text accepted without execution", {
    value <- .fo_h2_census(c(.fo_helper_text, 'stop("This census must only parse the helper")'))
    "sections/symbol-map.R" %in% value$summary$file
  })
  for (.fo_literal in c('source("report_typography.R")', 'source("tools/report_typography.R")',
                         'source(file = "tools/report_typography.R", local = TRUE)')) {
    .fo_case(paste0("H2 literal control: ", .fo_literal), {
      .fo_h2_census(.fo_helper(.fo_literal), "format.R")
      TRUE
    })
  }
  .fo_h2_bad <- list(
    arbitrary = 'source(target)',
    unrelated_literal = '{ allowed <- "tools/report_typography.R"; source(target) }',
    other_guard_lowercase = 'if (file.exists("tools/other.r")) source("tools/report_typography.R", local = TRUE)',
    other_guard_uppercase = 'if (file.exists("tools/other.R")) source("tools/report_typography.R", local = TRUE)',
    candidate = sub('"tools/report_typography.R"', '"other.R"', .fo_helper_text, fixed = TRUE),
    guard = sub('requireNamespace("here", quietly = TRUE)', 'requireNamespace("other", quietly = TRUE)', .fo_helper_text, fixed = TRUE),
    filter = sub('file.exists(candidates)', 'is.character(candidates)', .fo_helper_text, fixed = TRUE),
    index = sub('hit[[1]]', 'hit[[2]]', .fo_helper_text, fixed = TRUE),
    normalization = sub('source(normalizePath(hit[[1]]), local = into)', 'source(hit[[1]], local = into)', .fo_helper_text, fixed = TRUE),
    added_assignment = sub('source(normalizePath', 'hit <- "other.R"; source(normalizePath', .fo_helper_text, fixed = TRUE),
    renamed = sub('.source_report_typography <-', '.other <-', .fo_helper_text, fixed = TRUE),
    reordered = sub('hit <- candidates[file.exists(candidates)]', 'invisible(NULL); hit <- candidates[file.exists(candidates)]', .fo_helper_text, fixed = TRUE),
    wrong_local = sub('local = into', 'local = TRUE', .fo_helper_text, fixed = TRUE),
    outside = c(.fo_helper_text, 'source(normalizePath(hit[[1]]), local = into)'),
    malicious_second = c(.fo_helper_text, 'source(target)'),
    duplicate_file = 'source(file = "tools/report_typography.R", file = target)',
    duplicate_positional = 'source("tools/report_typography.R", target)',
    missing_file = 'source(file = )',
    missing_local = 'source("tools/report_typography.R", local = )',
    duplicate_local = 'source("tools/report_typography.R", local = TRUE, local = FALSE)',
    extra_argument = 'source("tools/report_typography.R", unexpected = target)')
  for (.fo_bad in names(.fo_h2_bad)) {
    .fo_case(paste0("H2 ", .fo_bad, ": specific call-source refusal"), {
      error <- .fo_attempt(.fo_h2_census(.fo_h2_bad[[.fo_bad]]))
      if (!is.null(error) && !grepl(":call-source:", error, fixed = TRUE))
        cat("      unexpected census refusal:", error, "\n")
      !is.null(error) && grepl(":call-source:", error, fixed = TRUE)
    })
  }
  .fo_case("H2 canonical helper in format.R: specific call-source refusal", {
    error <- .fo_attempt(.fo_h2_census(.fo_helper_text, "format.R"))
    !is.null(error) && grepl(":call-source:", error, fixed = TRUE)
  })
  .fo_case("H2 allowed guard control: typography literal remains accepted", {
    .fo_h2_census('if (file.exists("tools/report_typography.R")) source("tools/report_typography.R", local = TRUE)')
    TRUE
  })
  .fo_g5_dir("empty-counts")
  .fo_case("D-99 parse: zero R files fails", {
    .fo_g5_status(.fo_g5_output(.fo_g5$check_parse()), "FAIL", "R parse")
  })
  .fo_case("D-99 parse: one R file passes with count", {
    dir.create("tools"); writeLines("x <- NULL", "tools/fixture.R")
    .fo_g5_status(.fo_g5_output(.fo_g5$check_parse()), "PASS", "1")
  })
  .fo_case("D-99 parse control: well-formed R file still passes", {
    .fo_g5_status(.fo_g5_output(.fo_g5$check_parse()), "PASS", "R parse")
  })
  file.copy(file.path(tools, "gate.R"), "tools/gate.R")
  unlink("tools/legacy_gates.yaml")
  .fo_count_fns <- c("check_stray_tags", "check_composition", "check_gate_dominance",
                     "check_self_gate", "check_gate_digest_binding", "check_envelope_pin")
  for (.fo_fn in .fo_count_fns) {
    .fo_case(paste0("D-99 ", .fo_fn, ": PASS discloses its count"), {
      output <- .fo_g5_output(.fo_g5[[.fo_fn]]())
      any(startsWith(output, "PASS")) && all(grepl("[0-9]", output[startsWith(output, "PASS")]))
    })
    .fo_case(paste0("D-99 ", .fo_fn, " control: valid inventory still passes"), {
      output <- .fo_g5_output(.fo_g5[[.fo_fn]]())
      any(startsWith(output, "PASS")) && !any(startsWith(output, "FAIL"))
    })
  }

  # Part B: QC halts and the exclusion flag use exact, nonempty contracts.
  .fo_qc_inputs <- list(empty = data.frame(), zero_rows = data.frame(pass = logical()),
    missing_pass = data.frame(id = "x", ok = FALSE), null = NULL,
    numeric_pass = data.frame(pass = 1), missing_value = data.frame(pass = NA))
  for (.fo_q in names(.fo_qc_inputs)) {
    .fo_case(paste0("D-45 ", .fo_q, ": invalid QC frame stops"), {
      error <- .fo_attempt(.fo_g5$assert_qc_pass(.fo_qc_inputs[[.fo_q]]))
      !is.null(error) && grepl("QC", error, fixed = TRUE) && grepl("pass", error, fixed = TRUE)
    })
    .fo_case(paste0("D-45 ", .fo_q, " control: passing QC frame returns unchanged"), {
      qc <- data.frame(id = "x", description = "fixture", pass = TRUE, detail = "")
      identical(.fo_g5$assert_qc_pass(qc), qc)
    })
  }
  .fo_case("D-45 control: failing row retains QC battery failed message", {
    error <- .fo_attempt(.fo_g5$assert_qc_pass(data.frame(id = "x", description = "fixture", pass = FALSE, detail = "bad")))
    !is.null(error) && startsWith(error, "QC battery failed:\n") && grepl("[x] fixture", error, fixed = TRUE)
  })
  .fo_data <- data.frame(ID = c(1, 1), TIME = c(0, 1), EVID = c(1, 0), AMT = c(100, 0),
    DV = c(NA, 9), MDV = c(1, 0), NUM = 1:2, C = NA, BLQ = c(0, 0))
  .fo_expect <- function(lloq) list(n_subjects = 1, lloq = lloq, source_rows = 2, reconciled = TRUE)
  .fo_lloqs <- list(character = "10", infinite = Inf, vector = c(10, 20), empty = numeric())
  for (.fo_l in names(.fo_lloqs)) {
    .fo_case(paste0("D-127 ", .fo_l, ": invalid lloq stops"), {
      error <- .fo_attempt(.fo_g5$run_qc_battery(.fo_data, .fo_expect(.fo_lloqs[[.fo_l]])))
      !is.null(error) && grepl("lloq", error, fixed = TRUE)
    })
    .fo_case(paste0("D-127 ", .fo_l, " control: numeric lloq keeps BLQ result"), {
      qc <- .fo_g5$run_qc_battery(.fo_data, .fo_expect(10))
      identical(qc$pass[qc$id == "blq"], FALSE) && nrow(qc) == 12L
    })
  }
  .fo_case("D-127 control: NA lloq retains no-LLOQ check", {
    qc <- .fo_g5$run_qc_battery(.fo_data, .fo_expect(NA))
    identical(qc$pass[qc$id == "blq"], TRUE)
  })
  .fo_case("O-10b: a C-prefixed column cannot replace column C", {
    data <- .fo_data; names(data)[names(data) == "C"] <- "Cflag"
    error <- .fo_attempt(.fo_g5$run_qc_battery(data, .fo_expect(10)))
    !is.null(error) && grepl("column C", error, fixed = TRUE)
  })
  .fo_case("O-10b control: exact C column gives the same QC result", {
    data <- .fo_data; data$Cflag <- "exclude"
    identical(.fo_g5$run_qc_battery(data, .fo_expect(10)), .fo_g5$run_qc_battery(.fo_data, .fo_expect(10)))
  })

  # Part C: helper contracts, with no engine, external data or signed reference.
  .fo_subject <- data.frame(ID = c(1, 2), APGAR = c(7, 8))
  .fo_subject_data <- data.frame(ID = c(1, 2), WT = c(3.1, 2.9))
  for (.fo_col in c("NOPE", "WT")) {
    .fo_case(paste0("D-48 ", .fo_col, ": absent subject column stops"), {
      error <- .fo_attempt(.fo_g5$nq_attach_subject(.fo_subject_data, .fo_subject, .fo_col))
      !is.null(error) && grepl(.fo_col, error, fixed = TRUE)
    })
    .fo_case(paste0("D-48 ", .fo_col, " control: present subject column attaches"), {
      data <- .fo_g5$nq_attach_subject(.fo_subject_data, .fo_subject, "APGAR")
      identical(data$WT, .fo_subject_data$WT) && identical(data$APGAR, .fo_subject$APGAR)
    })
  }
  .fo_results <- list(list(record = list(meta = list(run = "102")),
    results = list(params = data.frame(parameter = "THETA3", estimate = 0.75, rse_pct = 40))))
  .fo_exponent <- function(bound) .fo_g5$nm_exponent_row(
    list(run = "102", parameter = "THETA3", rse_bound_pct = bound), .fo_results)
  .fo_bounds <- list(character = "5", infinite = Inf, missing = NA_real_, vector = c(5, 50))
  for (.fo_b in names(.fo_bounds)) {
    .fo_case(paste0("D-117 ", .fo_b, ": invalid exponent bound stops"), {
      error <- .fo_attempt(.fo_exponent(.fo_bounds[[.fo_b]]))
      !is.null(error) && grepl("rse_bound_pct", error, fixed = TRUE)
    })
    .fo_case(paste0("D-117 ", .fo_b, " control: numeric bound keeps comparison"), {
      value <- .fo_exponent(5)
      identical(value$within_bound, FALSE) && identical(value$rse_bound_pct, 5)
    })
  }
  .fo_g5_dir("handoffs")
  for (.fo_run in c("101", "102")) {
    dir.create(file.path("model", .fo_run), recursive = TRUE)
    for (.fo_ext in c("lst", "ext", "ctl"))
      writeLines(if (.fo_ext == "lst") "1NONMEM VERSION 7.5.1" else "fixture", file.path("model", .fo_run, paste0(.fo_run, ".", .fo_ext)))
    writeLines("{}", file.path("model", .fo_run, "bbi_config.json"))
  }
  writeLines("ID,TIME", "data.csv")
  .fo_result <- function(run, ofv) {
    frame <- data.frame(x = 1); attr(frame, "estimation") <- "$EST METHOD=1"
    list(record = list(meta = list(run = run), est = "$EST METHOD=1",
      run_start = as.POSIXct("2026-11-14 10:00:00", tz = "UTC")),
      results = list(heuristics = list(minimization_terminated = FALSE), ofv = ofv), frame = frame)
  }
  .fo_handoff <- function(prior, priors = list(prior_a = list(name = "prior_a", text = "declared prior"))) .fo_g5$nm_write_handoffs(
    comparisons = list(c1 = list(unit = "nested_lrt", parent = "101", child = "102", criterion_dofv = 3.84, df = 1, alpha = 0.05, prior = prior)),
    res_list = list(.fo_result("101", 100), .fo_result("102", 90)),
    streams = c(`101` = "model/101/101.ctl", `102` = "model/102/102.ctl"), data_file = "data.csv",
    weight_constants = data.frame(quantity = character(), value_kg = numeric()), exponent = NULL,
    priors = priors, model_dir = "model",
    out_dir = paste0("out-", prior), analysis_id = "FIXTURE", owner = "FIXTURE")
  .fo_case("D-118: undeclared comparison prior stops before writing", {
    error <- .fo_attempt(.fo_handoff("nope"))
    !is.null(error) && grepl("nope", error, fixed = TRUE) && !length(list.files("out-nope"))
  })
  .fo_case("D-118 control: declared prior writes three handoff files", {
    files <- .fo_handoff("prior_a")
    length(files) == 3L && all(file.exists(files)) &&
      grepl("prior_a", paste(readLines("out-prior_a/map-inputs-c1.yaml"), collapse = "\n"), fixed = TRUE)
  })
  # H1: positional declarations are accepted alongside named declarations.
  .fo_priors <- rep(list(list(name = "fixture", text = "declared prior")), 4L)
  .fo_case("H1: unnamed prior list accepts valid numeric index", {
    files <- .fo_handoff(3, .fo_priors)
    length(files) == 3L && all(file.exists(files))
  })
  .fo_invalid_priors <- list(out_of_range = 9, zero = 0, fractional = 1.5,
    missing = NA_real_, infinite = Inf, empty = numeric(), absent = NULL,
    missing_name = NA_character_, empty_name = "", missing_vector = c(1, NA_real_))
  for (.fo_ref in names(.fo_invalid_priors)) {
    .fo_case(paste0("H1 ", .fo_ref, ": invalid prior stops with comparison context"), {
      error <- .fo_attempt(.fo_handoff(.fo_invalid_priors[[.fo_ref]], .fo_priors))
      !is.null(error) && grepl("comparison c1", error, fixed = TRUE) && grepl("prior", error, fixed = TRUE)
    })
  }
  .fo_case("D-119: character numeric-constant column stops", {
    error <- .fo_attempt(.fo_g5$ns_constant(data.frame(quantity = "horizon", value = "48"), "horizon", "value"))
    !is.null(error) && grepl("value", error, fixed = TRUE)
  })
  .fo_case("D-119 control: numeric constants keep their numeric values", {
    identical(.fo_g5$ns_constant(data.frame(quantity = "horizon", value = 48), "horizon", "value"), 48) &&
      identical(.fo_g5$ns_constant(data.frame(quantity = "weight", value_kg = 3), "weight", "value_kg"), 3)
  })
  .fo_applicability <- function(values) .fo_g5$ns_applicability(
    data.frame(SCENARIO = "LD150-WT01-APGA", phase = "LD", dose_mg_per_kg = 150),
    horizon_h = 200, design_constants = data.frame(quantity = c("dose min", "dose max", "horizon"), value = values),
    dose_rows = list(LD = list(min = "dose min", max = "dose max")), horizon_row = "horizon", approved = list())
  .fo_case("D-119 applicability: character constants stop instead of comparing as text", {
    error <- .fo_attempt(.fo_applicability(c("10", "20", "48")))
    !is.null(error) && grepl("value", error, fixed = TRUE)
  })
  .fo_case("D-119 applicability control: numeric constants retain extrapolation refusal", {
    error <- .fo_attempt(.fo_applicability(c(10, 20, 48)))
    !is.null(error) && grepl("outside the observed design without recorded approval", error, fixed = TRUE)
  })
  .fo_design <- data.frame(ID = c(1, 1, 1, 2), TIME = c(0, 1, 2, 0), EVID = c(1, 0, 0, 1),
    AMT = c(10, 0, 0, 8), C = NA, PRED = c(0, 4, 5, 0), RATE = 0, ADDL = 0, II = 0, SS = 0,
    WT = c(3, 3, 3, 2.5))
  .fo_fitted <- function(data) .fo_g5$ns_fitted_design(data, 2, list(WT = "WT"), 1)
  for (.fo_col in c("RATE", "ADDL", "II", "SS", "TIME", "AMT")) {
    .fo_case(paste0("D-121 ", .fo_col, ": unsupported or missing design value stops"), {
      data <- .fo_design; data[[.fo_col]][1] <- if (.fo_col %in% c("TIME", "AMT")) NA else 1
      error <- .fo_attempt(.fo_fitted(data))
      !is.null(error) && grepl(.fo_col, error, fixed = TRUE)
    })
    .fo_case(paste0("D-121 ", .fo_col, " control: zero-filled supported design returns"), {
      value <- .fo_fitted(.fo_design)
      nrow(value$design) == 4L && value$n_dose_rows == 2L && !anyNA(value$design$AMT)
    })
  }
  # M1: excluded malformed rows never reach fitted-design validation.
  for (.fo_col in c("TIME", "AMT", "RATE")) {
    .fo_case(paste0("M1 ", .fo_col, ": excluded malformed row is dropped"), {
      extra <- .fo_design[1, , drop = FALSE]
      extra$C <- 1
      extra[[.fo_col]] <- if (.fo_col == "RATE") 1 else NA_real_
      value <- .fo_fitted(rbind(.fo_design, extra))
      identical(value, .fo_fitted(.fo_design))
    })
    .fo_case(paste0("M1 ", .fo_col, " control: retained malformed row stops"), {
      data <- .fo_design
      data[[.fo_col]][1] <- if (.fo_col == "RATE") 1 else NA_real_
      error <- .fo_attempt(.fo_fitted(data))
      !is.null(error) && grepl(.fo_col, error, fixed = TRUE)
    })
  }
  .fo_g5_dir("qualification")
  dir.create("skills/diagnostics/reference", recursive = TRUE)
  dir.create("out")
  for (.fo_member in c("gof", "npde")) {
    .fo_ref_fn <- if (.fo_member == "gof") "reference_gof_panel" else "reference_npde"
    .fo_reference <- file.path("skills/diagnostics/reference", paste0(.fo_member, ".R"))
    .fo_case(paste0("D-125 ", .fo_member, ": zero reference panels stop"), {
      writeLines(paste0(.fo_ref_fn, " <- function(...) list()"), .fo_reference)
      error <- .fo_attempt(.fo_g5$qualification_registry()[[.fo_member]]$fn(NULL,
        list(out_dir = "out", run = "fx", npde_n = 100, seed = 1)))
      !is.null(error) && grepl(.fo_member, error, fixed = TRUE) && grepl("panel", error, fixed = TRUE)
    })
    .fo_case(paste0("D-125 ", .fo_member, " control: one reference panel renders"), {
      writeLines(paste0(.fo_ref_fn, " <- function(...) list(function() NULL)"), .fo_reference)
      # Base graphics print method makes a synthetic panel without a plotting dependency.
      writeLines(c(paste0(.fo_ref_fn, " <- function(...) list(structure(list(), class = 'fo_panel'))"),
        "print.fo_panel <- function(x, ...) { graphics::plot.new(); invisible(x) }"), .fo_reference)
      value <- .fo_g5$qualification_registry()[[.fo_member]]$fn(NULL,
        list(out_dir = "out", run = "fx", npde_n = 100, seed = 1))
      identical(value$name, .fo_member) && file.exists(value$artifact) && file.size(value$artifact) > 0
    })
  }
  .fo_g5$reference_validated <- function(file) FALSE
  .fo_justify <- function(value) .fo_g5$qualification_battery(NULL, "low", disabled = "gof",
    justification = value, cfg = list(out_dir = "out"))
  .fo_justifications <- list(blank = "", whitespace = " \t", missing = NA_character_, empty = character(),
    vector = c("reason", "another"), numeric = 1)
  for (.fo_j in names(.fo_justifications)) {
    .fo_case(paste0("D-126 ", .fo_j, ": invalid disabled-member justification stops"), {
      error <- .fo_attempt(.fo_justify(.fo_justifications[[.fo_j]]))
      !is.null(error) && grepl("gof", error, fixed = TRUE) && grepl("justification", error, fixed = TRUE)
    })
    .fo_case(paste0("D-126 ", .fo_j, " control: written justification is retained"), {
      value <- .fo_justify("fixture reason")
      identical(value$justification, "fixture reason") && identical(value$disabled, "gof")
    })
  }
  .fo_case("D-126 control: NULL justification still stops", {
    error <- .fo_attempt(.fo_justify(NULL))
    !is.null(error) && grepl("requires a written justification", error, fixed = TRUE)
  })
  setwd(root)


  cat(sprintf("\nfail-open fix regression battery: %d passed, %d failed\n", passed, failed))
  invisible(failed)
}

if (.fo_invoked_as_script()) {
  .fo_failed <- .fo_run_tests()
  if (.fo_failed > 0L) quit(status = 1L)
}
