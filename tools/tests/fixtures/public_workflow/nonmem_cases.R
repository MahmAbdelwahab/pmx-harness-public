# Listed engine contracts exercised using generated records and an executable stub.
public_nonmem_cases <- function(repo) {
  root <- tempfile("public-nonmem-")
  dir.create(root)
  wd <- setwd(root)
  on.exit(setwd(wd), add = TRUE)
  ref <- "skills/nonmem-estimation/reference/bbr_estimation.R"
  manifest <- "skills/nonmem-estimation/MANIFEST.yaml"
  paths <- c(ref, manifest, "tools/method_manifest.R", "tools/review_stage.R")
  for (path in paths) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    stopifnot(file.copy(file.path(repo, path), path))
  }
  reference <- new.env(parent = globalenv())
  reference$"%||%" <- function(x, y) if (is.null(x)) y else x
  sys.source(ref, reference)
  draft <- readLines(manifest)
  approve <- function() {
    lines <- sub("^reviewed_by: .*$", 'reviewed_by: "Alex Rowan"', draft)
    lines <- sub("^reviewed_at: .*$", 'reviewed_at: "2026-11-20"', lines)
    writeLines(sub("status: draft$", "status: approved", lines), manifest)
  }
  refuses <- function(expr, why) {
    error <- tryCatch({ force(expr); NULL }, error = conditionMessage)
    if (is.null(error)) stop("expected refusal: ", why, call. = FALSE)
    if (!grepl(why, error, fixed = TRUE)) stop("expected ", why, "; got ", error, call. = FALSE)
  }
  dir.create("models")
  dir.create("engine-fixture/run", recursive = TRUE)
  writeLines("inert; never executed", "engine-fixture/run/fake-nmfe")
  bbi <- file.path(root, "bbi-stub")
  log <- file.path(root, "bbi-argv")
  write_bbi_stub <- function() {
    writeLines(c("#!/bin/sh", sprintf("printf '%%s\\n' \"$@\" > %s", shQuote(log)),
                 "[ \"$#\" -eq 1 ] && [ \"$1\" = version ] || exit 84",
                 "printf 'v3.4.0\\n'"), bbi)
    Sys.chmod(bbi, "0755")
  }
  write_bbi_stub()
  opts <- options("bbr.bbi_exe_path")
  on.exit(options(opts), add = TRUE)
  config <- list(engines = list(nonmem = list(provision = "native",
    native_executable = "fake-nmfe", nm_version_label = "fixture")),
    bbi = list(args = list(threads = 2L)))
  workspace <- function() reference$reference_nm_workspace("models", config,
    nonmem_home = file.path(root, "engine-fixture"), bbi_path = bbi)
  model <- file.path(root, "models", "run.mod")
  stats <- list(number_of_subjects = 3L, number_of_obs = 5L, number_of_data_records = 8L)
  counts <- reference$.reference_nm_counts
  parser <- reference$.reference_nm_termination_lines
  calls <- new.env(parent = emptyenv())
  joined <- data.frame(NUM = seq_len(5L), CWRES = seq_len(5L) / 10)
  calls$counts <- stats
  calls$ofv <- 9.5
  calls$join <- 0L
  calls$join_failure <- FALSE
  api <- list(
    model_summary = function(mod) list(ofv = list(list(ofv_no_constant = 81.5),
      list(ofv_no_constant = calls$ofv)), run_details = calls$counts),
    param_estimates = function(summary) list(parameter_names = "test_parameter",
      estimate = 3, stderr = 0.5, random_effect_sd = NA_real_,
      shrinkage = NA_real_, fixed = FALSE),
    get_model_path = function(mod) model,
    nm_join = function(mod, .join_col) {
      calls$join <- calls$join + 1L
      stopifnot(identical(.join_col, "NUM"))
      if (calls$join_failure) stop("inert join failure", call. = FALSE)
      joined
    })
  reference[["::"]] <- function(pkg, name) {
    package <- deparse(substitute(pkg))
    symbol <- deparse(substitute(name))
    if (identical(package, "bbr")) api[[symbol]] else getExportedValue(package, symbol)
  }
  dir_census <- function() sort(list.files(root, recursive = TRUE, all.files = TRUE))
  cases <- list(
    "NM-WORKSPACE: signed local configuration writes bbi.yaml after executable verification" = function() {
      before <- dir_census()
      refuses(workspace(), "not yet SME-validated")
      stopifnot(identical(dir_census(), before), !file.exists(log))
      approve()
      writeLines(c("#!/bin/sh", "exit 84"), bbi)
      refuses(workspace(), "did not report a version")
      stopifnot(identical(dir_census(), before), !file.exists(log))
      write_bbi_stub()
      out <- workspace()
      cfg <- yaml::read_yaml(out)
      stopifnot(identical(out, file.path("models", "bbi.yaml")),
                identical(readLines(log), "version"),
                identical(cfg$threads, 2L),
                identical(cfg$nonmem$fixture$executable, "fake-nmfe"),
                identical(cfg$nonmem$fixture$home, file.path(root, "engine-fixture")),
                identical(cfg$nm_version, "fixture"), identical(cfg$git, FALSE))
      snapshot <- list.files("models/engine-config-used", full.names = TRUE)
      stopifnot(length(snapshot) == 1L, identical(yaml::read_yaml(snapshot), config))
      writeLines(c(readLines(ref), "# changed fixture bytes"), ref)
      on.exit(file.copy(file.path(repo, ref), ref, overwrite = TRUE))
      before <- dir_census()
      refuses(workspace(), "digest drift")
      stopifnot(identical(dir_census(), before))
    },
    "NM-TERMINATION: success, failure and objective lines stay verbatim" = function() {
      listing <- file.path("models", "run.lst")
      for (status in c("SUCCESSFUL", "TERMINATED")) {
        lines <- c(paste("0MINIMIZATION", status), "  detail line", "#OBJV: 9.5")
        writeLines(c("fixture heading", lines, "other text"), listing)
        stopifnot(identical(parser(listing), lines),
          identical(reference$.reference_nm_termination_text(parser(listing)),
                    paste(lines, collapse = " | ")))
      }
      writeLines("0MINIMIZATION INCOMPLETE", listing)
      refuses(parser(listing), "no termination status")
      refuses(parser("models/absent.lst"), "could not be read")
    },
    "NM-RESULTS: the production entrypoint refuses bad paths and propagates join failure" = function() {
      approve()
      writeLines("$DATA ../data.csv IGNORE=@ ", model)
      result <- reference$reference_nm_results(model)
      stopifnot(identical(result$residuals, joined), identical(result$ofv, 9.5))
      n <- calls$join
      for (path in c("/fixture/data.csv", "C:/fixture/data.csv", "\\\\fixture\\data.csv")) {
        writeLines(paste("$DATA", path), model)
        refuses(reference$reference_nm_results(model), "$DATA is absolute")
      }
      writeLines("$PROBLEM No data", model)
      refuses(reference$reference_nm_results(model), "no $DATA record")
      stopifnot(identical(calls$join, n))
      writeLines("$DATA ../data.csv", model)
      calls$join_failure <- TRUE
      on.exit(calls$join_failure <- FALSE)
      refuses(reference$reference_nm_results(model), "nm_join failed (inert join failure)")
    },
    "NM-COUNTS: observed rows, records and subjects stay distinct through results" = function() {
      approve()
      writeLines("$DATA ../data.csv", model)
      result <- reference$reference_nm_results(model)
      stopifnot(identical(counts(stats), list(n_id = 3L, n_obs = 5L, n_records = 8L)),
                identical(result[c("n_id", "n_obs", "n_records")], counts(stats)))
      for (field in names(stats)) for (malformed in list(NULL, NA_integer_, 1:2)) {
        calls$counts <- stats
        calls$counts[[field]] <- malformed
        refuses(reference$reference_nm_results(model), paste("no usable", field))
      }
      calls$counts <- stats
      calls$ofv <- NA_real_
      refuses(reference$reference_nm_results(model), "required result field ofv")
    },
    "NM-CLEAN: copy guard refuses destinations equal to, inside or above the source" = function() {
      review <- new.env(parent = baseenv())
      sys.source("tools/review_stage.R", review)
      dir.create("copy-check/source/child", recursive = TRUE)
      src <- normalizePath("copy-check/source")
      marker <- file.path(src, "untouched.txt")
      writeLines("independent source witness", marker)
      before <- unname(tools::md5sum(marker))
      before_paths <- dir_census()
      for (dest in c(src, file.path(src, "child"), dirname(src))) {
        refuses(review$rv_copy_check(src, dest, src, "workspace", "nonmem", "untouched.txt"),
                "equal to, inside or containing the examined root")
      }
      stopifnot(identical(dir_census(), before_paths), identical(unname(tools::md5sum(marker)), before))
    })
  cases <- cases[c(2L, 5L, 1L, 3L, 4L)]
  failed <- character(0)
  for (name in names(cases)) {
    error <- tryCatch({ cases[[name]](); NULL }, error = conditionMessage)
    if (is.null(error)) cat("PASS ", name, "\n", sep = "") else {
      cat("FAIL ", name, ": ", error, "\n", sep = "")
      failed <- c(failed, name)
    }
  }
  invisible(length(failed) == 0L)
}
