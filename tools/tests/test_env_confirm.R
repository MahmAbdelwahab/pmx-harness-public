#!/usr/bin/env Rscript
# Environment-confirmation gate: check_environment() refuses to run until the
# SME has reviewed the probed harness.yaml and set environment_confirmed: true.
#
# Hermetic: every fixture lives under tempdir() and the working directory is
# changed into it, so the repository's own harness.yaml is never read, written
# or printed. Run with
#   Rscript tools/tests/test_env_confirm.R
# Exits non-zero if any assertion fails.
#
# SAFE TO SOURCE: guarded exactly like tools/tests/test_batch4.R, so
# tar_source("tools") defines and runs nothing.
.invoked_as_script <- function() {
  fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  length(fa) > 0 && grepl("test_env_confirm\\.R$", sub("^--file=", "", fa[1]))
}

.ac14_environment_cases <- function(TOOLS) {
  # Synthetic modules exercise the real composer without loading analysis declarations.
  compose_case <- function(tasks, scope = tasks) {
    e <- new.env(parent = globalenv())
    sys.source(file.path(TOOLS, "..", "pipeline", "compose.R"), e)
    e$`%||%` <- function(x, y) if (is.null(x)) y else x
    e$resolve_nonmem_provision <- function(...) "containerized"
    e$TASK_DIR <- tempfile("ac14-modules-")
    dir.create(e$TASK_DIR)
    for (tk in c("reporting", "review")) writeLines(
      sprintf('task_targets <- list(targets::tar_target_raw("synthetic_%s", quote(identity("engineering"))))', tk),
      file.path(e$TASK_DIR, paste0(tk, ".R")))
    p <- e$compose_pipeline("SYNTHETIC-ATTEST", tasks,
      attest_tasks = scope)
    p[[1L]]$command$expr[[1L]]
  }
  fixture <- function(code, tool = NULL, mode = "stdout", package_version = "fixture-a") {
    old_wd <- getwd()
    old_path <- Sys.getenv("PATH", unset = NA_character_)
    root <- tempfile("ac14-environment-"); dir.create(root)
    on.exit(setwd(old_wd), add = TRUE)
    on.exit(if (is.na(old_path)) Sys.unsetenv("PATH") else Sys.setenv(PATH = old_path), add = TRUE)
    setwd(root)
    writeLines(c("environment_confirmed: true", "r_provision: managed"), "harness.yaml")
    dir.create("bin")
    emit <- function(name, kind, version = paste(name, "fixture-a")) {
      body <- switch(kind, stdout = sprintf("printf '%%s\\n' '%s'", version),
        stderr = sprintf("printf '%%s\\n' '%s' >&2", version),
        nonzero = "printf 'failure text\\n'; exit 7", empty = "exit 0",
        whitespace = "printf '   \\n'", launch = "exit 0")
      path <- file.path("bin", name)
      writeLines(c(if (kind == "launch") "#!/absent/ac14-interpreter" else "#!/bin/sh",
                   sprintf("printf '%%s\\n' '%s' >> calls.log", name),
                   if (name %in% c("pdftotext", "unzip"))
                     'case "$1" in -v) ;; *) exit 9 ;; esac' else
                     'case "$1" in --version) ;; *) exit 9 ;; esac', body), path)
      Sys.chmod(path, "0755")
    }
    for (n in c("Rscript", "quarto", "tectonic", "pdftotext", "unzip"))
      if (!(identical(n, tool) && mode == "absent")) emit(n, if (identical(n, tool)) mode else "stdout")
    Sys.setenv(PATH = file.path(root, "bin"))
    e <- new.env(parent = globalenv())
    sys.source(file.path(TOOLS, "check_environment.R"), e)
    sys.source(file.path(TOOLS, "yaml_scalar.R"), e)
    e$resolve_nonmem_license <- function(...) ""
    e$resolve_nonmem_home <- function(...) NULL
    e$resolve_nonmem_image <- function(...) "synthetic"
    e$requireNamespace <- function(package, ...) !identical(package, "AC14MissingPackage")
    e$`::` <- function(pkg, name) {
      p <- as.character(substitute(pkg)); n <- as.character(substitute(name))
      if (identical(p, "utils") && identical(n, "packageVersion"))
        return(function(package) if (package == "PMXForest") package_version else "fixture-base")
      getExportedValue(p, n)
    }
    att <- function(...) e$attest_environment(require_nonmem = FALSE, require_psn = FALSE,
      nonmem_provision = "containerized", pkgs = "PMXForest", ...)
    code(e, att, emit)
  }
  refuses <- function(expr, name) {
    x <- tryCatch({ force(expr); NULL }, error = conditionMessage)
    !is.null(x) && grepl(name, x, fixed = TRUE)
  }
  cases <- list(
    "A-review planned-only" = function() stopifnot(isTRUE(compose_case(character(), "review")$require_review)),
    "A-review no-review" = function() stopifnot(is.null(compose_case(character())$require_review)),
    "A-scope truth-table" = function() {
      for (reporting in c(FALSE, TRUE)) for (review in c(FALSE, TRUE)) {
        scope <- c(if (reporting) "reporting", if (review) "review")
        cmd <- compose_case(character(), scope)
        stopifnot(identical(isTRUE(cmd$require_review), review),
                  identical(cmd, compose_case(character(), scope)))
      }
    },
    "A-legacy default-false" = function() fixture(function(e, att, emit) {
      a <- att(); b <- att(require_review = FALSE)
      calls <- readLines("calls.log", warn = FALSE)
      stopifnot(identical(a, b), identical(names(a$probe$executables), c("Rscript", "quarto", "tectonic")),
                !any(calls %in% c("pdftotext", "unzip")))
    }, tool = "pdftotext", mode = "nonzero"),
    "A-legacy nonzero" = function() fixture(function(e, att, emit) {
      emit("quarto", "nonzero")
      stopifnot(identical(e$.env_exe_probe("quarto")$version, "failure text"), !inherits(att(), "error"))
    }),
    "A-review flag-validation" = function() fixture(function(e, att, emit) {
      for (x in list(NULL, NA, 1L, "TRUE", c(TRUE, FALSE), logical(), matrix(TRUE, 1L, 1L)))
        stopifnot(refuses(att(require_review = x), "require_review"))
    }),
    "A-fingerprint package" = function() {
      # Isolate the package-only change in one environment.
      fixture(function(e, att, emit) {
        a <- att(require_review = TRUE)
        e$`::` <- function(pkg, name) {
          p <- as.character(substitute(pkg)); n <- as.character(substitute(name))
          if (p == "utils" && n == "packageVersion") return(function(package) "fixture-b")
          getExportedValue(p, n)
        }
        b <- att(require_review = TRUE)
        stopifnot(!identical(a$fingerprint, b$fingerprint), !identical(a$probe$r_packages$required, b$probe$r_packages$required))
      })
    },
    "A-forest missing-package" = function() fixture(function(e, att, emit) {
      e$requireNamespace <- function(package, ...) !identical(package, "PMXForest")
      stopifnot(refuses(att(require_review = TRUE), "PMXForest"))
    })
  )
  for (tool in c("pdftotext", "unzip")) local({
    n <- tool
    for (stream in c("stdout", "stderr")) local({
      s <- stream
      cases[[paste("A-tools", n, s)]] <<- function() fixture(function(e, att, emit) {
        helper <- e$.env_exe_probe(n, "-v", strict = TRUE)
        a <- att(require_review = TRUE)
        stopifnot(nzchar(helper$path), identical(helper$version, paste(n, "fixture-a")),
                  identical(a$probe$executables[[n]], helper),
                  sum(readLines("calls.log", warn = FALSE) == n) == 2L)
      }, tool = n, mode = s)
    })
    for (mode in c("absent", "launch", "nonzero", "empty", "whitespace")) local({
      m <- mode
      cases[[paste("A-tools", n, m)]] <<- function() fixture(function(e, att, emit) {
        stopifnot(refuses(e$.env_exe_probe(n, "-v", strict = TRUE), n),
                  refuses(att(require_review = TRUE), n))
      }, tool = n, mode = m)
    })
    cases[[paste("A-fingerprint", n)]] <<- function() fixture(function(e, att, emit) {
      a <- att(require_review = TRUE); b <- att(require_review = TRUE)
      stopifnot(identical(a, b))
      emit(n, "stdout", paste(n, "fixture-b"))
      c <- att(require_review = TRUE)
      stopifnot(!identical(a$fingerprint, c$fingerprint),
                identical(a$probe$r_packages, c$probe$r_packages))
    })
  })
  cases
}

.run_env_confirm_tests <- function() {

  args <- commandArgs(FALSE)
  file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
  TOOLS <- normalizePath(file.path(dirname(file_arg), ".."), mustWork = TRUE)
  source(file.path(TOOLS, "check_environment.R"))
  source(file.path(TOOLS, "engine_config.R"))

  failures <- 0L
  ok <- function(label, cond) {
    if (isTRUE(cond)) {
      cat("PASS ", label, "\n", sep = "")
    } else {
      cat("FAIL ", label, "\n", sep = "")
      failures <<- failures + 1L
    }
  }

  # The gate message the SME is meant to see, in fragments the test can pin
  # without re-typing the whole sentence.
  wanted <- c("harness.yaml is missing or not confirmed",
              "copilot-adapter/setup.py install",
              "environment_confirmed: true")

  err_of <- function() {
    tryCatch({
      check_environment(require_nonmem = FALSE, require_psn = FALSE,
                        pkgs = character(0))
      NULL
    }, error = function(e) conditionMessage(e))
  }

  sandbox <- file.path(tempdir(), "env-confirm-fixture")
  dir.create(sandbox, showWarnings = FALSE, recursive = TRUE)
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(sandbox)

  # --- 1. harness.yaml absent -------------------------------------------------
  if (file.exists("harness.yaml")) file.remove("harness.yaml")
  msg <- err_of()
  ok("1 absent harness.yaml stops", !is.null(msg))
  ok("1b message names the fix",
     !is.null(msg) && all(vapply(wanted, function(w) grepl(w, msg, fixed = TRUE),
                                 logical(1))))

  # --- 2. environment_confirmed: false ---------------------------------------
  writeLines(c("environment_confirmed: false", "r_provision: managed"),
             "harness.yaml")
  msg <- err_of()
  ok("2 unconfirmed harness.yaml stops", !is.null(msg))
  ok("2b message names the fix",
     !is.null(msg) && grepl("harness.yaml is missing or not confirmed", msg,
                            fixed = TRUE))

  # --- 3. a non-boolean value is not a confirmation ---------------------------
  writeLines(c("environment_confirmed: \"yes\"", "r_provision: managed"),
             "harness.yaml")
  ok("3 a non-TRUE value is not a confirmation", !is.null(err_of()))

  # --- 4. confirmed: the gate opens and the remaining probes run --------------
  writeLines(c("environment_confirmed: true", "r_provision: managed"),
             "harness.yaml")
  res <- tryCatch(
    check_environment(require_nonmem = FALSE, require_psn = FALSE,
                      pkgs = character(0)),
    error = function(e) e)
  ok("4 confirmed harness.yaml passes the gate",
     is.list(res) && identical(res$status, "ok"))

  # --- 5. resolve_nonmem_home(): harness.yaml value ---------------------------
  # The bbr native lane needs the site NONMEM install root. Resolution order
  # mirrors the licence exactly: NONMEM_HOME (environment) first, then the
  # nonmem_home: key in harness.yaml, else NULL. No path is ever invented.
  writeLines(c("environment_confirmed: true", "r_provision: managed",
               "nonmem_home: /opt/NONMEM/nm751"), "harness.yaml")
  Sys.unsetenv("NONMEM_HOME")
  ok("5 resolve_nonmem_home reads the harness.yaml key",
     identical(resolve_nonmem_home(), "/opt/NONMEM/nm751"))

  # --- 6. environment overrides the file (licence order) ----------------------
  Sys.setenv(NONMEM_HOME = "/opt/NONMEM/from-env")
  ok("6 NONMEM_HOME overrides the harness.yaml key",
     identical(resolve_nonmem_home(), "/opt/NONMEM/from-env"))
  Sys.unsetenv("NONMEM_HOME")

  # --- 7. absent everywhere is NULL, never a guess ----------------------------
  writeLines(c("environment_confirmed: true", "r_provision: managed"),
             "harness.yaml")
  ok("7 absent nonmem_home resolves to NULL", is.null(resolve_nonmem_home()))

  # --- 8. native provision requires an nmfe under <nonmem_home>/run/ ----------
  # bbi executes <nonmem_home>/run/<executable>, so that is what the check
  # verifies. A fake nmfe75 (an empty file) is enough: nothing executes it.
  model_dir <- file.path(sandbox, "model", "pk")
  dir.create(model_dir, showWarnings = FALSE, recursive = TRUE)
  writeLines("nonmem: {}", file.path(model_dir, "bbi.yaml"))
  fake_home <- file.path(sandbox, "nm751")
  dir.create(file.path(fake_home, "run"), showWarnings = FALSE, recursive = TRUE)
  file.create(file.path(fake_home, "run", "nmfe75"))

  native_err <- function() {
    tryCatch({
      check_environment(model_dir = model_dir, require_nonmem = TRUE,
                        require_psn = FALSE, nonmem_provision = "native",
                        pkgs = character(0))
      NULL
    }, error = function(e) conditionMessage(e))
  }

  writeLines(c("environment_confirmed: true", "r_provision: managed",
               paste0("nonmem_home: ", fake_home)), "harness.yaml")
  ok("8 native provision passes with an nmfe under <nonmem_home>/run/",
     is.null(native_err()))

  missing_home <- file.path(sandbox, "nm-absent")
  writeLines(c("environment_confirmed: true", "r_provision: managed",
               paste0("nonmem_home: ", missing_home)), "harness.yaml")
  msg <- native_err()
  ok("8b a nonmem_home without an nmfe fails", !is.null(msg))
  ok("8c the failure names the path it checked",
     !is.null(msg) && grepl(file.path(missing_home, "run"), msg, fixed = TRUE))

  # --- 9. unconfigured nonmem_home under native provision --------------------
  writeLines(c("environment_confirmed: true", "r_provision: managed"),
             "harness.yaml")
  msg <- native_err()
  ok("9 native provision without nonmem_home fails",
     !is.null(msg) && grepl("nonmem_home", msg, fixed = TRUE))

  # --- 10. containerized provision does not require nonmem_home --------------
  res <- tryCatch(
    check_environment(require_nonmem = FALSE, require_psn = FALSE,
                      nonmem_provision = "containerized", pkgs = character(0)),
    error = function(e) e)
  ok("10 containerized provision never asks for nonmem_home",
     is.list(res) && identical(res$status, "ok"))

  # --- 11. attest_environment(): a stable fingerprint (round 7, F10) ----
  # The always-cued pipeline target. Its VALUE must be stable across calls on
  # an unchanged machine (no timestamps in it, or every tar_make would
  # invalidate the whole DAG), carry a sha256 fingerprint of the probed state
  # plus the confirmation flag, and change when the probed state changes.
  writeLines(c("environment_confirmed: true", "r_provision: managed"),
             "harness.yaml")
  att <- function() attest_environment(require_nonmem = FALSE, require_psn = FALSE,
                                            nonmem_provision = "containerized",
                                            pkgs = character(0))
  a1 <- tryCatch(att(), error = function(e) e)
  ok("11 attestation returns on a confirmed environment", !inherits(a1, "error"))
  ok("11b fingerprint has the sha256:<hex64> grammar",
     !inherits(a1, "error") && grepl("^sha256:[0-9a-f]{64}$", a1$fingerprint))
  ok("11c the attestation names the provisions it probed",
     !inherits(a1, "error") && identical(a1$nonmem_provision, "containerized") &&
       identical(a1$r_provision, "managed") && isTRUE(a1$environment_confirmed))
  flat_names <- function(x, prefix = "") {
    if (!is.list(x)) return(prefix)
    unlist(lapply(names(x), function(n) flat_names(x[[n]], paste0(prefix, "/", n))))
  }
  ok("11d no timestamp field anywhere in the value",
     !inherits(a1, "error") &&
       !any(grepl("checked_at|time|date|stamp", flat_names(a1), ignore.case = TRUE)))
  a2 <- tryCatch(att(), error = function(e) e)
  ok("11e two calls on an unchanged machine give identical values",
     !inherits(a1, "error") && !inherits(a2, "error") && identical(a1, a2))
  ok("11f the probe records R version and platform",
     !inherits(a1, "error") && identical(a1$probe$r_version, as.character(getRversion())) &&
       identical(a1$probe$platform, R.version$platform))
  writeLines(c("environment_confirmed: true", "r_provision: site"), "harness.yaml")
  a3 <- tryCatch(att(), error = function(e) e)
  ok("11g a changed probed state (R provision) changes the fingerprint",
     !inherits(a3, "error") && !identical(a3$fingerprint, a1$fingerprint))
  writeLines(c("environment_confirmed: false", "r_provision: managed"), "harness.yaml")
  ok("11h an unconfirmed environment still refuses (check_environment semantic kept)",
     inherits(tryCatch(att(), error = function(e) e), "error"))
  writeLines(c("environment_confirmed: true", "r_provision: managed",
               "nonmem_license: /secret/license.lic"), "harness.yaml")
  a4 <- tryCatch(att(), error = function(e) e)
  ok("11i harness.yaml values other than the provisions never appear in the value",
     !inherits(a4, "error") && !any(grepl("/secret/license.lic",
                                         unlist(a4, use.names = FALSE), fixed = TRUE)))

  # --- 12. the composed package list probes what the tasks load ---------
  # compose_pipeline() splices the environment check's package vector into
  # the attestation target. It must name the packages the declared tasks and
  # their references actually load (ggplot2 and digest for an EDA) and never
  # riders nothing loads: mrggsave and mrgda used to be appended alongside
  # pmtables and yspec, and the hosted validated-stack job (2026-09-04)
  # stopped an EDA-only fixture pipeline with "R package not installed:
  # mrggsave" although no module, tool or reference calls it. Read from the
  # real public synthetic analysis manifest (composition only, nothing executes) with the
  # repository root current; the working directory is restored afterwards.
  REPO <- normalizePath(file.path(TOOLS, ".."), mustWork = TRUE)
  project <- tempfile("public-attestation-")
  dir.create(project)
  for (p in c("pipeline", "analyses", "config"))
    stopifnot(file.copy(file.path(REPO, p), project, recursive = TRUE))
  dir.create(file.path(project, "tools"))
  stopifnot(all(file.copy(list.files(TOOLS, pattern = "[.]R$", full.names = TRUE),
                         file.path(project, "tools"))))
  in_project <- function(expr) {
    wd <- setwd(project); on.exit(setwd(wd)); force(expr)
  }
  script <- file.path("analyses", "synthetic_example.R")
  in_project({
    status <- system2(file.path(R.home("bin"), "Rscript"),
                      c("--vanilla", shQuote(script), "--generate"), timeout = 15)
    stopifnot(identical(status, 0L))
  })
  att_cmd <- tryCatch(in_project({
    m <- suppressMessages(targets::tar_manifest(script = script, fields = c("name", "command")))
    m$command[m$name == "environment_attestation"]
  }), error = function(e) conditionMessage(e))
  # Both assertions require the real command: an error message or an empty
  # result must fail them, never pass the "absent" check by accident.
  is_cmd <- length(att_cmd) == 1L && grepl("attest_environment", att_cmd, fixed = TRUE)
  pkgs_in <- function(p) grepl(sprintf('"%s"', p), att_cmd, fixed = TRUE)
  ok("12 the public synthetic analysis attestation asks for the packages its tasks load (ggplot2, digest, pmplots, pmtables)",
     is_cmd && all(vapply(c("ggplot2", "digest", "pmplots", "pmtables"), pkgs_in, logical(1))))
  ok("12b it never asks for packages nothing loads (mrggsave, mrgda)",
     is_cmd && !any(vapply(c("mrggsave", "mrgda"), pkgs_in, logical(1))))

  # --- 13. attest_tasks fixes the attestation scope ------------------------
  # Every composed target names the attestation, whose package list follows
  # the tasks it is computed over. An analysis that appends tasks one gate at
  # a time declares the planned set in attest_tasks, so appending a task
  # changes neither the attestation command nor any signed stage. With the
  # argument omitted (the default, attest_tasks = tasks) a closed analysis
  # composes exactly what it composed before. Composition only, in memory,
  # with the repository root current; nothing executes.
  cmds_of <- function(pipe) {
    stats::setNames(lapply(pipe, function(t) t$command$expr[[1]]),
                    vapply(pipe, function(t) t$settings$name, character(1)))
  }
  planned <- c("data_assembly", "eda", "base_model", "covariate",
               "qualification", "simulation", "reporting", "review")
  da_in <- list(source = "examples/synthetic/dataset.csv",
                spec = "examples/synthetic/spec.yml", map = "examples/synthetic/plan.md",
                expect = list(n_subjects = 4L, source_rows = 12L, lloq = NA_real_))
  err_msg <- function(expr) tryCatch({ force(expr); "no error" },
                                     error = function(e) conditionMessage(e))
  r13 <- tryCatch(in_project({
    # compose.R and the task modules resolve %||% in the global environment
    # (tools/gate_package.R defines it in a pipeline session).
    if (!exists("%||%", envir = globalenv(), mode = "function")) {
      assign("%||%", function(a, b) if (is.null(a)) b else a, envir = globalenv())
    }
    source("pipeline/compose.R")
    one <- compose_pipeline(
      "TST-ATTEST", "data_assembly",
      list(engine = "nonmem", data_assembly = da_in), attest_tasks = planned)
    two <- compose_pipeline(
      "TST-ATTEST", c("data_assembly", "eda"),
      list(engine = "nonmem", data_assembly = da_in,
           eda = list(catalog = character(0))), attest_tasks = planned)
    unknown <- err_msg(compose_pipeline(
      "TST-ATTEST", "data_assembly", list(data_assembly = da_in),
      attest_tasks = c("data_assembly", "no_such_task")))
    omitted <- err_msg(compose_pipeline(
      "TST-ATTEST", c("data_assembly", "eda"),
      list(data_assembly = da_in, eda = list(catalog = character(0))),
      attest_tasks = "data_assembly"))
    # public synthetic analysis's own script, written twice to tempdir() through the same
    # deparse: once as declared (attest_tasks omitted) and once with
    # attest_tasks equal to its tasks; each composed by tar_manifest in the
    # default callr subprocess (the script's tar_source() helpers are needed
    # at composition, see test 12).
    ex <- parse(script, keep.source = FALSE)
    decl_att <- ex[[length(ex)]]
    decl_att$attest_tasks <- c("data_assembly", "eda")
    manifest_of <- function(decl, file) {
      writeLines(c(vapply(ex[-length(ex)], function(e) paste(deparse(e), collapse = "\n"),
                          character(1)),
                   deparse(decl)), file)
      m <- suppressMessages(targets::tar_manifest(
        script = file, fields = c("name", "command")))
      stats::setNames(m$command, m$name)
    }
    e_default <- manifest_of(ex[[length(ex)]], file.path(tempdir(), "synthetic-default.R"))
    e_att <- manifest_of(decl_att, file.path(tempdir(), "synthetic-attest.R"))
    list(one = cmds_of(one), two = cmds_of(two), unknown = unknown,
         omitted = omitted, e_default = as.list(e_default),
         e_att = as.list(e_att))
  }), error = function(e) conditionMessage(e))
  got13 <- is.list(r13)
  if (!got13) cat("      composition error: ", r13, "\n", sep = "")
  att13 <- function(x) x[["environment_attestation"]]
  ok("13 appending eda leaves the attestation command unchanged under the same attest_tasks",
     got13 && !is.null(att13(r13$one)) &&
       identical(att13(r13$one), att13(r13$two)))
  ok("13b the attestation is computed over attest_tasks, not tasks (NONMEM and the eda packages are asked for)",
     got13 && {
       a <- paste(deparse(att13(r13$one)), collapse = " ")
       grepl("require_nonmem = TRUE", a, fixed = TRUE) &&
         all(vapply(c("\"bbr\"", "\"mrgsolve\"", "\"pmplots\"", "\"pmtables\""),
                    grepl, logical(1), x = a, fixed = TRUE))
     })
  ok("13c an unknown name in attest_tasks stops at composition",
     got13 && grepl("attest_tasks", r13$unknown, fixed = TRUE) &&
       grepl("no_such_task", r13$unknown, fixed = TRUE))
  ok("13d attest_tasks that omits a declared task stops at composition",
     got13 && grepl("attest_tasks", r13$omitted, fixed = TRUE) &&
       grepl("eda", r13$omitted, fixed = TRUE))
  ok("13e public synthetic analysis composes the same attestation with attest_tasks omitted and equal to its tasks",
     got13 && !is.null(att13(r13$e_default)) &&
       identical(att13(r13$e_default), att13(r13$e_att)))
  ok("13f ... and the same command for every target",
     got13 && length(r13$e_default) > 0 &&
       identical(r13$e_default, r13$e_att))

  # --- 14. engine versions under the containerized provision (B0e) ----------
  # The attestation records the PsN
  # version line and two separately labelled NONMEM version fields (PsN's
  # registry and the image's SETUP75 line 2) and stops when a probe exits
  # nonzero, prints no version line, or the two NONMEM versions disagree.
  # No container starts here: every probe goes through a stub assigned over
  # .env_run_wrapper, and the licence path below is a fixture string that
  # names no file. The real runner is restored at the end.
  setwd(sandbox)
  real_runner <- .env_run_wrapper
  secret <- "/fixture/secret.lic"
  # The registry lines have the shape `psn -nm_versions` prints: the option
  # name padded to a column (B0e remediation round 1; the first live run had
  # this shape, the single-space stub did not). The recorded registry line is
  # the matched raw line trimmed at its ends only, so the inner padding stays.
  reg_line <- "default                   (/nonmem/nm751,7.5.1)"
  good <- list(
    execute = c("noise before", "PsN version: 5.7.1", "noise after"),
    psn = c("nm751                     (/nonmem/nm751,7.5.1)", reg_line,
            "The default is /nonmem/nm751,7.5.1"),
    grep = "# SETUP75 version 7.5.1 December 2021")
  # A stub runner: output and status per probe, keyed by the probe's command.
  stub <- function(out = good, status = list()) {
    function(args) list(lines = out[[args[1]]],
                        status = status[[args[1]]] %||% 0L)
  }
  # Runs .env_engine_versions() under a runner; returns the value or the
  # error message, plus every message and warning text raised on the way.
  run_versions <- function(runner) {
    assign(".env_run_wrapper", runner, envir = globalenv())
    seen <- character(0)
    v <- withCallingHandlers(
      tryCatch(.env_engine_versions(), error = function(e) e),
      message = function(m) { seen <<- c(seen, conditionMessage(m)); invokeRestart("muffleMessage") },
      warning = function(w) { seen <<- c(seen, conditionMessage(w)); invokeRestart("muffleWarning") })
    list(value = v, msg = if (inherits(v, "error")) conditionMessage(v) else "",
         seen = seen)
  }

  # (a) the fixture attestation of test 11 (engine not required) keeps its shape
  writeLines(c("environment_confirmed: true", "r_provision: managed"), "harness.yaml")
  a5 <- tryCatch(att(), error = function(e) e)
  ok("14a an attestation without the engine keeps its shape (no version fields)",
     !inherits(a5, "error") &&
       identical(names(a5$probe$container_image), c("name", "id")) &&
       !("psn_execute" %in% names(a5$probe$executables)) &&
       identical(names(a5$probe$nonmem), c("home", "nmfe")))

  # (b) the extractor returns exactly the matched line, trimmed, or ""
  ok("14b the extractor returns the PsN line from surrounding noise",
     identical(.env_version_line(c("x", "  PsN version: 5.7.1  ", "y"), "^PsN version:"),
               "PsN version: 5.7.1"))
  ok("14b2 the extractor returns \"\" when nothing matches",
     identical(.env_version_line(c("x", "y"), "^PsN version:"), ""))

  # (c) a failing wrapper that prints the licence path: stop, no echo
  leak <- run_versions(function(args)
    list(lines = paste("NONMEM license not found:", secret), status = 2L))
  ok("14c a nonzero probe stops, naming the probe and the status",
     startsWith(leak$msg, "PsN ") && grepl("status 2", leak$msg, fixed = TRUE))
  ok("14c2 the stop message never carries the captured output",
     nzchar(leak$msg) && !grepl(secret, leak$msg, fixed = TRUE) &&
       !grepl("license not found", leak$msg, fixed = TRUE))
  ok("14c3 no message or warning raised during the call carries the path",
     !any(grepl(secret, leak$seen, fixed = TRUE)))
  leak_nm <- run_versions(stub(
    out = modifyList(good, list(psn = paste("NONMEM license not found:", secret))),
    status = list(psn = 2L)))
  ok("14c4 a failing registry probe stops, named, without the path",
     startsWith(leak_nm$msg, "NONMEM registry ") && grepl("status 2", leak_nm$msg, fixed = TRUE) &&
       !grepl(secret, leak_nm$msg, fixed = TRUE) &&
       !any(grepl(secret, leak_nm$seen, fixed = TRUE)))

  # (d) a missing version line stops, naming the probe
  miss <- run_versions(stub(out = modifyList(good, list(execute = c("a", "b")))))
  ok("14d a PsN probe without a version line stops, naming PsN",
     startsWith(miss$msg, "PsN "))
  miss_reg <- run_versions(stub(out = modifyList(good, list(psn = "nm751                     (/nonmem/nm751,7.5.1)"))))
  ok("14d2 a registry without its default line stops, naming the registry probe",
     startsWith(miss_reg$msg, "NONMEM registry "))
  miss_set <- run_versions(stub(out = modifyList(good, list(grep = character(0)))))
  ok("14d3 a SETUP75 probe without its version line stops, naming it",
     startsWith(miss_set$msg, "NONMEM SETUP75 "))

  # (e) a version line with a nonzero status is still a stop
  late <- run_versions(stub(status = list(execute = 1L)))
  ok("14e a matched PsN line with status 1 stops, naming PsN and 1",
     startsWith(late$msg, "PsN ") && grepl("status 1", late$msg, fixed = TRUE))
  late_set <- run_versions(stub(status = list(grep = 1L)))
  ok("14e2 a matched SETUP75 line with status 1 stops",
     startsWith(late_set$msg, "NONMEM SETUP75 ") && grepl("status 1", late_set$msg, fixed = TRUE))

  # (f) check_environment: the licence note carries no path. PATH points at
  # an empty directory for the call so that no docker command can run.
  empty_bin <- file.path(sandbox, "empty-bin")
  dir.create(empty_bin, showWarnings = FALSE)
  old_path <- Sys.getenv("PATH"); old_lic <- Sys.getenv("NONMEM_LICENSE", NA)
  Sys.setenv(PATH = empty_bin); Sys.unsetenv("NONMEM_LICENSE")
  writeLines(c("environment_confirmed: true", "r_provision: managed",
               paste0("nonmem_license: ", secret)), "harness.yaml")
  cmsg <- tryCatch({
    check_environment(require_nonmem = TRUE, require_psn = TRUE,
                      nonmem_provision = "containerized", pkgs = character(0))
    ""
  }, error = function(e) conditionMessage(e))
  ok("14f the licence note says 'NONMEM license not found' without the path",
     grepl("NONMEM license not found", cmsg, fixed = TRUE) &&
       !grepl(secret, cmsg, fixed = TRUE))

  # (g) all probes succeed and agree: the three lines come back, labelled
  g <- run_versions(stub())
  ok("14g the PsN line and both NONMEM fields come back",
     identical(g$value, list(psn = "PsN version: 5.7.1",
                             nonmem_psn_registry = reg_line,
                             nonmem_setup75 = "# SETUP75 version 7.5.1 December 2021")))
  # (g2) the two NONMEM sources disagree: a stop naming both versions
  dis <- run_versions(stub(out = modifyList(good, list(grep = "# SETUP75 version 7.5.0 May 2021"))))
  ok("14g2 a registry/SETUP75 disagreement stops, naming both versions",
     grepl("disagree", dis$msg) && grepl("7.5.1", dis$msg, fixed = TRUE) &&
       grepl("7.5.0", dis$msg, fixed = TRUE))

  # (h) the fields land in the containerized attestation. check_environment is
  # replaced by a no-op for this call (its probes are tests 1-10 and 14f);
  # PATH is still the empty directory, so docker cannot run.
  real_check <- check_environment
  assign("check_environment", function(...) invisible(NULL), envir = globalenv())
  assign(".env_run_wrapper", stub(), envir = globalenv())
  writeLines(c("environment_confirmed: true", "r_provision: managed"), "harness.yaml")
  h <- tryCatch(attest_environment(require_nonmem = TRUE, require_psn = TRUE,
                                   nonmem_provision = "containerized",
                                   pkgs = character(0)), error = function(e) e)
  ok("14h the containerized attestation records the PsN line and both NONMEM fields",
     !inherits(h, "error") &&
       identical(h$probe$executables$psn_execute,
                 list(path = "tools/nonmem_exec.sh", version = "PsN version: 5.7.1")) &&
       identical(h$probe$container_image$nonmem_version_psn_registry,
                 reg_line) &&
       identical(h$probe$container_image$nonmem_version_setup75,
                 "# SETUP75 version 7.5.1 December 2021"))
  assign("check_environment", real_check, envir = globalenv())
  assign(".env_run_wrapper", real_runner, envir = globalenv())
  Sys.setenv(PATH = old_path)
  if (!is.na(old_lic)) Sys.setenv(NONMEM_LICENSE = old_lic)

  ac14 <- .ac14_environment_cases(TOOLS)
  for (label in names(ac14)) {
    result <- tryCatch({ ac14[[label]](); TRUE }, error = function(e) FALSE)
    ok(label, result)
  }

  cat(if (failures == 0L) "env-confirm tests: ALL PASS\n"
      else sprintf("env-confirm tests: %d FAILURE(S)\n", failures))
  invisible(failures)
}

if (.invoked_as_script()) {
  quit(status = if (.run_env_confirm_tests() == 0L) 0L else 1L)
}
