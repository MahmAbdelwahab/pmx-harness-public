#!/usr/bin/env Rscript
# Regression battery for review findings RC7, RC8, and RC3.
#
# All engine behavior is supplied by disposable shell stubs. Reference files
# execute through Fixture-approved manifests under tempdir(); production
# manifests, signed records, analysis outputs, and live engines are untouched.

.invoked_as_script <- function() {
  fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  length(fa) > 0 && grepl("test_review_runtime\\.R$", sub("^--file=", "", fa[1]))
}

.run_review_runtime_tests <- function() {
  args <- commandArgs(FALSE)
  file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
  tools_dir <- normalizePath(file.path(dirname(file_arg), ".."), mustWork = TRUE)
  source_root <- normalizePath(file.path(tools_dir, ".."), mustWork = TRUE)

  passed <- 0L
  failed <- 0L
  ok <- function(condition, name) {
    condition <- tryCatch(isTRUE(condition), error = function(e) {
      cat(sprintf("      error: %s\n", conditionMessage(e)))
      FALSE
    })
    if (condition) {
      passed <<- passed + 1L
      cat(sprintf("PASS  %s\n", name))
    } else {
      failed <<- failed + 1L
      cat(sprintf("FAIL  %s\n", name))
    }
  }
  capture_error <- function(expr) {
    tryCatch({ force(expr); NULL }, error = function(e) e)
  }
  expect_stop <- function(expr, name, pattern) {
    error <- capture_error(expr)
    if (!is.null(error) && !grepl(pattern, conditionMessage(error))) {
      cat(sprintf("      got: %s\n", conditionMessage(error)))
    }
    ok(!is.null(error) && grepl(pattern, conditionMessage(error)), name)
  }

  root <- tempfile("review-runtime-")
  dir.create(root)
  old_wd <- setwd(root)
  on.exit(setwd(old_wd), add = TRUE)

  dir.create("tools")
  file.copy(file.path(source_root, "tools", "method_manifest.R"),
            "tools/method_manifest.R", overwrite = TRUE)
  helper_source <- file.path(source_root, "tools", "psn_invocation.R")
  ok(file.exists(helper_source),
     "STAGE2 shared PsN invocation helper exists")
  if (file.exists(helper_source))
    file.copy(helper_source, "tools/psn_invocation.R", overwrite = TRUE)
  source("tools/method_manifest.R")
  sha <- function(path) .mm_file_sha256(path)
  production_manifests <- file.path(source_root, "skills",
                                    c("psn-uncertainty", "psn-simeval", "covariate-priors"),
                                    "MANIFEST.yaml")
  production_digests <- vapply(production_manifests, sha, character(1))

  copy_reference <- function(skill, file) {
    destination <- file.path("skills", skill, "reference")
    dir.create(destination, recursive = TRUE, showWarnings = FALSE)
    file.copy(file.path(source_root, "skills", skill, "reference", file),
              file.path(destination, file), overwrite = TRUE)
  }
  write_manifest <- function(skill, units, reviewed_by = "Alex Rowan") {
    lines <- c(
      "# Disposable runtime-test manifest",
      paste0("skill: ", skill),
      "generated_at: 2026-09-11T00:00:00Z",
      sprintf('reviewed_by: "%s"', reviewed_by),
      'reviewed_at: "2026-09-11"',
      "units:"
    )
    for (name in names(units)) {
      unit <- units[[name]]
      reference_path <- file.path("skills", skill, "reference", unit$file)
      lines <- c(
        lines,
        paste0("  ", name, ":"),
        paste0("    status: ", unit$status),
        paste0("    reference_path: ", reference_path),
        paste0("    entrypoints: [", paste(unit$entrypoints, collapse = ", "), "]"),
        paste0("    sha256: ", sha(reference_path)),
        "    evidence: Fixture",
        '    reason: "Disposable synthetic-engine regression fixture"'
      )
      if (length(unit$depends)) {
        lines <- c(lines, "    depends:")
        for (dependency in unit$depends) {
          lines <- c(lines, paste0("      - path: ", dependency),
                     paste0("        sha256: ", sha(dependency)))
        }
      }
    }
    writeLines(lines, file.path("skills", skill, "MANIFEST.yaml"))
  }
  source_reference <- function(skill, file) {
    environment <- new.env(parent = globalenv())
    sys.source(file.path("skills", skill, "reference", file), envir = environment)
    environment
  }

  copy_reference("psn-uncertainty", "psn_uncertainty.R")
  copy_reference("psn-simeval", "psn_simeval.R")
  copy_reference("covariate-priors", "scm_config.R")
  copy_reference("covariate-priors", "psn_covsearch.R")
  copy_reference("nonmem-estimation", "bbr_estimation.R")

  write_manifest("psn-uncertainty", list(
    bootstrap = list(status = "approved", file = "psn_uncertainty.R",
                     entrypoints = c("reference_psn_bootstrap",
                                     "reference_psn_bootstrap_precheck"),
                     depends = "tools/psn_invocation.R"),
    sir = list(status = "approved", file = "psn_uncertainty.R",
               entrypoints = "reference_psn_sir",
               depends = "tools/psn_invocation.R"),
    llp = list(status = "approved", file = "psn_uncertainty.R",
               entrypoints = "reference_psn_llp",
               depends = "tools/psn_invocation.R")
  ))
  write_manifest("psn-simeval", list(
    vpc = list(status = "approved", file = "psn_simeval.R",
               entrypoints = "reference_psn_vpc",
               depends = "tools/psn_invocation.R"),
    npc = list(status = "approved", file = "psn_simeval.R",
               entrypoints = "reference_psn_npc",
               depends = "tools/psn_invocation.R"),
    sse = list(status = "approved", file = "psn_simeval.R",
               entrypoints = "reference_psn_sse",
               depends = "tools/psn_invocation.R")
  ))
  write_manifest("covariate-priors", list(
    psn_covsearch = list(
      status = "approved", file = "psn_covsearch.R",
      entrypoints = c("reference_psn_scmplus", "reference_psn_boot_scm",
                      "reference_psn_xv_scm", "reference_psn_lasso",
                      "reference_psn_frem"),
      depends = c("skills/psn-uncertainty/reference/psn_uncertainty.R",
                  "skills/covariate-priors/reference/scm_config.R"))
  ))

  uncertainty <- source_reference("psn-uncertainty", "psn_uncertainty.R")
  simeval <- source_reference("psn-simeval", "psn_simeval.R")
  covsearch <- source_reference("covariate-priors", "psn_covsearch.R")
  estimation <- source_reference("nonmem-estimation", "bbr_estimation.R")

  bin_dir <- file.path(root, "stub bin")
  dir.create(bin_dir)
  stub <- file.path(bin_dir, "engine-stub")
  writeLines(c(
    "#!/usr/bin/env bash",
    "set -euo pipefail",
    "tool=$(basename \"$0\")",
    "if [ \"$tool\" = psn ] && [ \"${1:-}\" = -version ]; then echo 'PsN version: 5.7.1'; exit 0; fi",
    "capture=\"$PWD/argv-${tool}.txt\"",
    ": > \"$capture\"",
    "run_dir=",
    "for argument in \"$@\"; do",
    "  printf '<%s>\\n' \"$argument\" >> \"$capture\"",
    "  case \"$argument\" in -directory=*) run_dir=${argument#-directory=} ;; esac",
    "done",
    "[ -n \"$run_dir\" ] || run_dir=\"${tool}2\"",
    "mkdir -p \"$run_dir\"",
    "case \"${STUB_RESULT_MODE:-success}:$tool\" in",
    "  missing:*) exit 0 ;;",
    "  ambiguous:bootstrap)",
    "    mkdir -p \"$run_dir/a\" \"$run_dir/b\"",
    "    printf 'current-a\\n' > \"$run_dir/a/bootstrap_results.csv\"",
    "    printf 'current-b\\n' > \"$run_dir/b/bootstrap_results.csv\"",
    "    exit 0 ;;",
    "  stale:bootstrap)",
    "    printf 'current-stale\\n' > \"$run_dir/bootstrap_results.csv\"",
    "    touch -t 200001010000 \"$run_dir/bootstrap_results.csv\"",
    "    exit 0 ;;",
    "esac",
    "case \"$tool\" in",
    "  bootstrap)",
    "    printf 'current bootstrap artifact\\n' > \"$run_dir/bootstrap_results.csv\"",
    "    printf 'minimization_successful,THETA(1)\\n1,1\\n1,1.1\\n' > \"$run_dir/raw_results_model.csv\" ;;",
    "  vpc)",
    "    printf 'current vpc artifact\\n' > \"$run_dir/vpc_results.csv\"",
    "    printf 'TABLE NO. 1\\n' > \"$run_dir/vpctab1\" ;;",
    "  lasso)",
    "    printf 'relation,coefficient\\nCL:WT-2,0.1\\n' > \"$run_dir/lasso_coefficients.csv\"",
    "    printf 'sample,value\\n1,1\\n' > \"$run_dir/raw_results_model.csv\" ;;",
    "  scmplus)",
    "    printf 'synthetic search trail\\n' > \"$run_dir/scmlog.txt\"",
    "    printf 'sample,value\\n1,1\\n' > \"$run_dir/raw_results_model.csv\" ;;",
    "esac"
  ), stub)
  Sys.chmod(stub, "0755")
  for (tool in c("bootstrap", "vpc", "lasso", "scmplus", "psn")) {
    file.symlink(stub, file.path(bin_dir, tool))
  }

  empty_env <- file.path(root, "empty.env")
  writeLines("# Fixture environment", empty_env)
  original_path <- Sys.getenv("PATH")
  original_provision <- Sys.getenv("NONMEM_PROVISION", unset = NA_character_)
  original_env_file <- Sys.getenv("NONMEM_ENV_FILE", unset = NA_character_)
  on.exit({
    Sys.setenv(PATH = original_path)
    if (is.na(original_provision)) Sys.unsetenv("NONMEM_PROVISION")
    else Sys.setenv(NONMEM_PROVISION = original_provision)
    if (is.na(original_env_file)) Sys.unsetenv("NONMEM_ENV_FILE")
    else Sys.setenv(NONMEM_ENV_FILE = original_env_file)
    Sys.unsetenv(c("STUB_RESULT_MODE", "PMX_INVOCATION_ID"))
  }, add = TRUE)
  Sys.setenv(PATH = paste(bin_dir, original_path, sep = .Platform$path.sep),
             NONMEM_PROVISION = "native", NONMEM_ENV_FILE = empty_env)
  executor <- file.path(source_root, "tools", "nonmem_exec.sh")

  engine_config <- list(psn = list(
    threads = 2L, retries = 1L, tweak_inits = TRUE, picky = FALSE,
    submit = "slurm", prepend_flags = " -n 1", queue = "long queue",
    account = "project one", nodes = 2L, parafile = "para file.pnm"))
  write_run_config <- function(workdir) {
    writeLines(c(
      "engines:", "  nonmem:", "    provision: native",
      "psn:", "  threads: 2", "  retries: 1", "  submit: slurm",
      '  prepend_flags: " -n 1"', '  queue: "long queue"',
      '  account: "project one"', "  nodes: 2", '  parafile: "para file.pnm"'
    ), file.path(workdir, "run-config.yaml"))
    writeLines("$GENERAL", file.path(workdir, "para file.pnm"))
  }
  write_model <- function(workdir, name = "model one.mod") {
    writeLines(c("$PROBLEM Fixture", "$INPUT ID TIME DV", "$DATA data.csv"),
               file.path(workdir, name))
    name
  }

  # RC7: public bootstrap returns only artifacts from this invocation.
  stale_workdir <- file.path(root, "stale-bootstrap")
  dir.create(stale_workdir)
  write_run_config(stale_workdir)
  model <- write_model(stale_workdir, "model.mod")
  dir.create(file.path(stale_workdir, "bootstrap1"))
  writeLines("synthetic old artifact",
             file.path(stale_workdir, "bootstrap1", "bootstrap_results.csv"))
  writeLines("minimization_successful,THETA(1)\n1,9\n1,9",
             file.path(stale_workdir, "bootstrap1", "raw_results_old.csv"))
  bootstrap <- tryCatch(uncertainty$reference_psn_bootstrap(
    stale_workdir, model, samples = 500, seed = 20260911,
    engine_config = engine_config, model_risk = "medium",
    executor = executor, repo_root = root), error = function(e) e)
  ok(!inherits(bootstrap, "error") &&
       !grepl("bootstrap1", bootstrap$results_csv, fixed = TRUE) &&
       identical(readLines(bootstrap$results_csv), "current bootstrap artifact"),
     "RC7 public bootstrap binds results to the current invocation")
  ok(!inherits(bootstrap, "error") && file.exists(bootstrap$invocation_manifest),
     "RC7 bootstrap emits an invocation manifest")
  bootstrap_manifest <- if (!inherits(bootstrap, "error") &&
                              file.exists(bootstrap$invocation_manifest))
    readLines(bootstrap$invocation_manifest) else character(0)
  ok(any(grepl("^argv:$", bootstrap_manifest)) &&
       any(grepl("^config_snapshot: ", bootstrap_manifest)) &&
       any(grepl("^  results_csv: ", bootstrap_manifest)) &&
       any(grepl("^  raw_results_csv: ", bootstrap_manifest)),
     "RC7 invocation manifest identifies argv, config snapshot, and selected outputs")

  # Stage 2 finalization owns the established 0/1/>1 invocation-bound
  # config-snapshot lookup. Exercise it through the public bootstrap runner.
  snapshot_executor <- file.path(bin_dir, "snapshot-executor")
  writeLines(c(
    "#!/usr/bin/env bash", "set -euo pipefail",
    "workdir=$1; shift; tool=$1; shift; cd \"$workdir\"", "run_dir=",
    "for argument in \"$@\"; do case \"$argument\" in -directory=*) run_dir=${argument#-directory=} ;; esac; done",
    "mkdir -p \"$run_dir\"",
    "printf 'current bootstrap artifact\\n' > \"$run_dir/bootstrap_results.csv\"",
    "printf 'minimization_successful,THETA(1)\\n1,1\\n1,1.1\\n' > \"$run_dir/raw_results_model.csv\"",
    "case \"${STUB_SNAPSHOT_MODE:-zero}\" in",
    "  one) mkdir -p engine-config-used; printf 'fixture\\n' > \"engine-config-used/one-${PMX_INVOCATION_ID}.yaml\" ;;",
    "  multiple) mkdir -p engine-config-used; printf 'a\\n' > \"engine-config-used/a-${PMX_INVOCATION_ID}.yaml\"; printf 'b\\n' > \"engine-config-used/b-${PMX_INVOCATION_ID}.yaml\" ;;",
    "esac"
  ), snapshot_executor)
  Sys.chmod(snapshot_executor, "0755")
  snapshot_case <- function(mode) {
    workdir <- file.path(root, paste0("snapshot-", mode))
    dir.create(workdir)
    model <- write_model(workdir, "model.mod")
    Sys.setenv(STUB_SNAPSHOT_MODE = mode)
    on.exit(Sys.unsetenv("STUB_SNAPSHOT_MODE"), add = TRUE)
    uncertainty$reference_psn_bootstrap(
      workdir, model, samples = 500, seed = 20260911,
      engine_config = engine_config, model_risk = "medium",
      executor = snapshot_executor, repo_root = root)
  }
  no_snapshot <- tryCatch(snapshot_case("zero"), error = function(e) e)
  no_snapshot_lines <- if (!inherits(no_snapshot, "error"))
    readLines(no_snapshot$invocation_manifest) else character(0)
  ok(!inherits(no_snapshot, "error") &&
       "config_snapshot: not-emitted-by-custom-executor" %in% no_snapshot_lines,
     "STAGE2 zero matching config snapshots retains custom-executor sentinel")
  one_snapshot <- tryCatch(snapshot_case("one"), error = function(e) e)
  one_snapshot_lines <- if (!inherits(one_snapshot, "error"))
    readLines(one_snapshot$invocation_manifest) else character(0)
  ok(!inherits(one_snapshot, "error") &&
       length(grep("^config_snapshot: ", one_snapshot_lines)) == 1L &&
       !any(grepl("not-emitted-by-custom-executor", one_snapshot_lines, fixed = TRUE)),
     "STAGE2 one matching config snapshot is bound in public manifest")
  parser_marker <- file.path(root, "bootstrap-parser-touched")
  original_parser <- uncertainty$.psn_parse_bootstrap_results
  uncertainty$.psn_parse_bootstrap_results <- function(...) {
    file.create(parser_marker)
    stop("scientific parser reached", call. = FALSE)
  }
  multiple_snapshot_error <- capture_error(snapshot_case("multiple"))
  uncertainty$.psn_parse_bootstrap_results <- original_parser
  ok(!is.null(multiple_snapshot_error) &&
       grepl("matching engine-config snapshots; at most one",
             conditionMessage(multiple_snapshot_error)) &&
       !file.exists(parser_marker),
     "STAGE2 multiple config snapshots refuse before scientific parsing")

  # Missing, ambiguous, and stale current outputs fail even with old output nearby.
  for (mode in c("missing", "ambiguous", "stale")) {
    Sys.setenv(STUB_RESULT_MODE = mode)
    expect_stop(uncertainty$reference_psn_bootstrap(
      stale_workdir, model, samples = 500, seed = 20260911,
      engine_config = engine_config, model_risk = "medium",
      executor = executor, repo_root = root),
      paste0("RC7 bootstrap refuses ", mode, " current output"),
      "no bootstrap_results\\.csv|copies of bootstrap_results\\.csv|newer than")
  }
  Sys.unsetenv("STUB_RESULT_MODE")

  # RC8: public runners preserve each path/configured value as one argv item.
  spaced_workdir <- file.path(root, "work directory with spaces")
  dir.create(spaced_workdir)
  write_run_config(spaced_workdir)
  spaced_model <- write_model(spaced_workdir)
  bootstrap_spaced <- tryCatch(uncertainty$reference_psn_bootstrap(
    spaced_workdir, spaced_model, samples = 500, seed = 20260911,
    engine_config = engine_config, model_risk = "medium",
    executor = executor, repo_root = root), error = function(e) e)
  bootstrap_argv <- if (!inherits(bootstrap_spaced, "error"))
    readLines(file.path(spaced_workdir, "argv-bootstrap.txt")) else character(0)
  ok(!inherits(bootstrap_spaced, "error") &&
       "<model one.mod>" %in% bootstrap_argv &&
       "<-slurm_prepend_flags= -n 1>" %in% bootstrap_argv &&
       "<-parafile=para file.pnm>" %in% bootstrap_argv,
     "RC8 uncertainty public runner preserves spaced argv elements")

  vpc <- tryCatch(simeval$reference_psn_vpc(
    spaced_workdir, spaced_model, samples = 20, seed = 20260911,
    bin_rule = list(auto_bin = "auto"), engine_config = engine_config,
    executor = executor), error = function(e) e)
  if (inherits(vpc, "error"))
    cat(sprintf("      vpc error: %s\n", conditionMessage(vpc)))
  vpc_argv <- if (!inherits(vpc, "error"))
    readLines(file.path(spaced_workdir, "argv-vpc.txt")) else character(0)
  ok(!inherits(vpc, "error") && "<model one.mod>" %in% vpc_argv &&
       "<-slurm_prepend_flags= -n 1>" %in% vpc_argv &&
       "<-parafile=para file.pnm>" %in% vpc_argv,
     "RC8 simeval public runner preserves spaced argv elements")

  scm_config <- "config one.scm"
  writeLines(c("# GENERATED by skills/covariate-priors/reference/scm_config.R",
               "model = model one.mod", "[test_relations]", "CL = WT-2"),
             file.path(spaced_workdir, scm_config))
  covariate_run <- tryCatch(covsearch$reference_psn_scmplus(
    spaced_workdir, scm_config, engine_config = engine_config,
    executor = executor, repo_root = root),
    error = function(e) e)
  if (inherits(covariate_run, "error"))
    cat(sprintf("      covsearch error: %s\n", conditionMessage(covariate_run)))
  covariate_argv <- if (!inherits(covariate_run, "error"))
    readLines(file.path(spaced_workdir, "argv-scmplus.txt")) else character(0)
  ok(!inherits(covariate_run, "error") &&
       "<config one.scm>" %in% covariate_argv &&
       "<-slurm_prepend_flags= -n 1>" %in% covariate_argv,
     "RC8 covsearch public runner preserves spaced argv elements")
  ok(!inherits(vpc, "error") && file.exists(vpc$invocation_manifest) &&
       !inherits(covariate_run, "error") &&
       file.exists(covariate_run$invocation_manifest),
     "RC7 simeval and covsearch emit invocation manifests")

  # RC3 shell snapshot names remain distinct in a forced same-second fixture.
  fixed_bin <- file.path(root, "fixed-time-bin")
  dir.create(fixed_bin)
  writeLines(c("#!/usr/bin/env bash", "printf '20260911T120000Z\\n'"),
             file.path(fixed_bin, "date"))
  writeLines(c("#!/usr/bin/env bash", "exit 0"), file.path(fixed_bin, "true"))
  Sys.chmod(file.path(fixed_bin, c("date", "true")), "0755")
  snapshot_workdir <- file.path(root, "same-second")
  dir.create(snapshot_workdir)
  write_run_config(snapshot_workdir)
  Sys.setenv(PATH = paste(fixed_bin, original_path, sep = .Platform$path.sep))
  system2(executor, c(shQuote(snapshot_workdir), "true", "first-attempt"),
          stdout = TRUE, stderr = TRUE)
  system2(executor, c(shQuote(snapshot_workdir), "true", "second-attempt"),
          stdout = TRUE, stderr = TRUE)
  snapshots <- list.files(file.path(snapshot_workdir, "engine-config-used"),
                          full.names = TRUE)
  snapshot_argv <- unlist(lapply(snapshots, function(path)
    grep("^# argv:", readLines(path), value = TRUE)))
  ok(length(snapshots) == 2L && any(grepl("first-attempt", snapshot_argv)) &&
       any(grepl("second-attempt", snapshot_argv)),
     "RC3 same-second shell invocations preserve two snapshots")

  # RC3 bbr snapshot and pre-run inventory paths reserve unique names.
  bbr_workdir <- file.path(root, "bbr-same-second")
  dir.create(bbr_workdir)
  bbr_config <- file.path(bbr_workdir, "run-config.yaml")
  writeLines(c("bbi:", "  mode: local", "  args:", "    threads: 1"), bbr_config)
  resolved <- list(path = bbr_config, sha256 = sha(bbr_config),
                   cfg = list(bbi = list(mode = "local", args = list(threads = 1L))))
  estimation$Sys.time <- function() as.POSIXct("2026-09-11 12:00:00", tz = "UTC")
  first <- estimation$.reference_nm_fit_snapshot(
    bbr_workdir, file.path(bbr_workdir, "101"), "local", resolved)
  second <- estimation$.reference_nm_fit_snapshot(
    bbr_workdir, file.path(bbr_workdir, "101"), "local", resolved)
  ok(!identical(first, second) && file.exists(first) && file.exists(second),
     "RC3 same-second bbr submissions preserve two snapshots")
  inventory_stem <- "20260911T120000Z-101-pre-run-inventory"
  inventory_first <- estimation$.reference_nm_reserve_evidence_path(
    file.path(bbr_workdir, "engine-config-used"), inventory_stem, ".txt")
  inventory_second <- estimation$.reference_nm_reserve_evidence_path(
    file.path(bbr_workdir, "engine-config-used"), inventory_stem, ".txt")
  ok(!identical(inventory_first, inventory_second) &&
       file.exists(inventory_first) && file.exists(inventory_second),
     "RC3 same-second bbr pre-run inventories reserve distinct paths")

  # Reservation failures are operational errors, not endless collision retries.
  absent_parent <- file.path(root, "absent-reservation-parent")
  expect_stop(uncertainty$.psn_new_run_dir(absent_parent, "bootstrap"),
              "RC3/7 uncertainty reservation refuses absent parent promptly", "cannot reserve")
  expect_stop(simeval$.psn_new_run_dir(absent_parent, "vpc"),
              "RC3/7 simeval reservation refuses absent parent promptly", "cannot reserve")
  occupied_parent <- file.path(root, "evidence-parent-is-file")
  writeLines("fixture", occupied_parent)
  expect_stop(estimation$.reference_nm_reserve_evidence_path(
    occupied_parent, "snapshot", ".txt"),
    "RC3 bbr reservation refuses unusable evidence directory promptly", "cannot reserve")

  # U1 (units programme): sir and llp on a stub executor. The executor is
  # called as <executor> <workdir> <tool> <args>; STUB_U1_MODE selects the
  # files it writes. The results files carry the section titles and headers
  # verified on the stored EB1 runs (2026-09-28) with synthetic numbers; no
  # value of a real run appears here.
  u1_fixtures <- file.path(root, "u1-fixtures")
  dir.create(u1_fixtures)
  writeLines(c(
    "SIR run info",
    '"","          Date","         model","   PsN version","NONMEM version",',
    '"","2000-1-1","model.mod","v0.0.0","nm000",',
    "Summary statistics over resamples",
    '"","THETA(1) a","OMEGA(1,1) b",',
    '"center_estimate",1,2,',
    '"          mean",1,2,',
    "Quantiles (R type=2)",
    '"","THETA(1) a","OMEGA(1,1) b",',
    '"          2.5%",1,2,',
    " ",
    '"              ",',
    "Empirical sd-correlation matrix",
    '"","THETA(1) a","OMEGA(1,1) b",',
    '"THETA(1) a",1,0,',
    '"OMEGA(1,1) b",0,1,'
  ), file.path(u1_fixtures, "sir_results.csv"))
  writeLines(c(paste0("iteration,commandline.samples,attempted.samples,",
                      "successful.samples,commandline.resamples,actual.resamples,",
                      "requested.ratio,actual.ratio,negative.dOFV,minimum.sample.ofv"),
               "1,10,10,10,5,5,0.5,0.5,0,1",
               "2,10,10,10,5,5,0.5,0.5,0,1"),
             file.path(u1_fixtures, "summary_iterations.csv"))
  writeLines(c("a,b", "1,2"), file.path(u1_fixtures, "flat_results.csv"))
  writeLines(c(
    "confidence.intervals",
    paste0('"","         lower","maximum.likelihood.estimate","         upper",',
           '"interval.ratio","    near.bound","max.iterations",'),
    '"THETA(1) a",1,2,3,1,0,0,',
    '"THETA(2) b",1,2,3,1,0,0,'
  ), file.path(u1_fixtures, "llp_results.csv"))
  u1_executor <- file.path(bin_dir, "u1-executor")
  writeLines(c(
    "#!/usr/bin/env bash", "set -euo pipefail",
    "workdir=$1; shift; tool=$1; shift; cd \"$workdir\"", "run_dir=",
    ": > \"argv-$tool.txt\"",
    "for argument in \"$@\"; do printf '<%s>\\n' \"$argument\" >> \"argv-$tool.txt\"; case \"$argument\" in -directory=*) run_dir=${argument#-directory=} ;; esac; done",
    "mode=${STUB_U1_MODE:-layout}",
    "if [ \"$mode\" = nodir ]; then exit 0; fi",
    "mkdir -p \"$run_dir\"",
    "if [ \"$tool\" = llp ]; then cp \"$U1_FIXTURES/llp_results.csv\" \"$run_dir/\"; exit 0; fi",
    "cp \"$U1_FIXTURES/summary_iterations.csv\" \"$run_dir/\"",
    "case \"$mode\" in",
    "  flat) cp \"$U1_FIXTURES/flat_results.csv\" \"$run_dir/sir_results.csv\"",
    "        printf 'cov\\n' > \"$run_dir/model_sir.cov\" ;;",
    "  twocov) cp \"$U1_FIXTURES/sir_results.csv\" \"$run_dir/\"",
    "          mkdir -p \"$run_dir/a\" \"$run_dir/b\"",
    "          printf 'cov\\n' > \"$run_dir/a/model_sir.cov\"",
    "          printf 'cov\\n' > \"$run_dir/b/model_sir.cov\" ;;",
    "  *) cp \"$U1_FIXTURES/sir_results.csv\" \"$run_dir/\"",
    "     printf 'cov\\n' > \"$run_dir/model_sir.cov\"",
    "     printf 'cov\\n' > \"$run_dir/boxcox_covmatrix_iteration1.cov\" ;;",
    "esac"
  ), u1_executor)
  Sys.chmod(u1_executor, "0755")
  Sys.setenv(U1_FIXTURES = u1_fixtures)
  u1_config <- list(psn = list(threads = 1L, retries = 3L, nm_version = "nm000",
                               tweak_inits = TRUE, picky = FALSE, clean = 1L))
  u1_workdir <- function(label) {
    workdir <- file.path(root, paste0("u1-", label))
    dir.create(workdir)
    write_model(workdir, "model.mod")
    workdir
  }
  u1_marker <- function(workdir, tool) {
    marker <- list.files(workdir, pattern = paste0("^\\.", tool, "_run_.*\\.invocation\\.yaml$"),
                         all.files = TRUE, full.names = TRUE)
    if (length(marker) == 1L) readLines(marker) else character(0)
  }
  u1_sir <- function(mode) {
    workdir <- u1_workdir(paste0("sir-", mode))
    Sys.setenv(STUB_U1_MODE = mode)
    on.exit(Sys.unsetenv("STUB_U1_MODE"), add = TRUE)
    result <- tryCatch(uncertainty$reference_psn_sir(
      workdir, "model.mod", samples = c(10, 10), resamples = c(5, 5),
      seed = 20260830, engine_config = u1_config,
      executor = u1_executor, repo_root = root), error = function(e) e)
    list(result = result, marker = u1_marker(workdir, "sir"))
  }
  u1_message <- function(x) if (inherits(x, "error")) conditionMessage(x) else ""

  # (a) PF-1: the sir entry point returns; before U1 it stopped on the
  # unbound `psn` after its marker was finalized.
  sir_flat <- u1_sir("flat")
  if (inherits(sir_flat$result, "error"))
    cat(sprintf("      sir error: %s\n", u1_message(sir_flat$result)))
  ok(!inherits(sir_flat$result, "error") &&
       identical(names(sir_flat$result$record$operational),
                 c("threads", "retries", "mpi")) &&
       identical(sir_flat$result$record$proposal, "model covariance step"),
     "U1 (a) sir entry point returns its record (PF-1)")

  # (b) PF-2: two fresh files match the final selection pattern; the marker
  # is finalized first and the stop names the count.
  sir_two <- u1_sir("twocov")
  cat(sprintf("      sir marker %s; stop: %s\n",
              paste(grep("^state:", sir_two$marker, value = TRUE), collapse = " "),
              u1_message(sir_two$result)))
  ok(inherits(sir_two$result, "error") &&
       grepl("holds 2 copies of model_sir.cov", u1_message(sir_two$result), fixed = TRUE) &&
       "state: completed" %in% sir_two$marker && "argv:" %in% sir_two$marker,
     "U1 (b) sir finalizes its marker before an ambiguous selection stops (PF-2)")

  # (d) PF-2, critic G7: no run directory, no finalization; the selection's
  # own message and a marker that stays started.
  sir_nodir <- u1_sir("nodir")
  ok(inherits(sir_nodir$result, "error") &&
       grepl("was not created", u1_message(sir_nodir$result), fixed = TRUE) &&
       "state: started" %in% sir_nodir$marker &&
       !("state: completed" %in% sir_nodir$marker),
     "U1 (d) sir without a run directory keeps its marker started")

  # sir covariance selection and parser on the verified layout.
  sir_layout <- u1_sir("layout")
  if (inherits(sir_layout$result, "error"))
    cat(sprintf("      sir error: %s\n", u1_message(sir_layout$result)))
  sir_summary <- if (!inherits(sir_layout$result, "error")) sir_layout$result$summary else NULL
  ok(!is.null(sir_summary) &&
       identical(basename(sir_layout$result$sir_cov), "model_sir.cov"),
     "U1 sir selects <model>_sir.cov beside the Box-Cox covariance files")
  ok(!is.null(sir_summary) &&
       identical(attr(sir_summary, "sections"),
                 c("SIR run info", "Summary statistics over resamples",
                   "Quantiles (R type=2)", "Empirical sd-correlation matrix")) &&
       length(attr(sir_summary, "missing_sections")) == 0L &&
       length(sir_summary$section) == length(sir_summary$lines) &&
       identical(sir_summary$section[c(1, 4, 8, 13)], 1:4) &&
       nrow(sir_summary$iterations) == 2L &&
       length(attr(sir_summary, "missing_iteration_columns")) == 0L,
     "U1 sir parser recognizes the verified sections and iteration table")
  ok(!is.null(sir_summary) && "state: completed" %in% sir_layout$marker,
     "U1 sir marker completed on a clean run")
  flat_summary <- if (!inherits(sir_flat$result, "error")) sir_flat$result$summary else NULL
  ok(!is.null(flat_summary) && length(attr(flat_summary, "missing_sections")) == 4L,
     "U1 sir parser reports the sections a results file lacks")

  # (c) PF-12: llp passes the run-config through the shared builder.
  llp_workdir <- u1_workdir("llp")
  llp <- tryCatch(uncertainty$reference_psn_llp(
    llp_workdir, "model.mod", thetas = c(1, 2), engine_config = u1_config,
    executor = u1_executor, repo_root = root), error = function(e) e)
  if (inherits(llp, "error")) cat(sprintf("      llp error: %s\n", u1_message(llp)))
  llp_argv <- if (file.exists(file.path(llp_workdir, "argv-llp.txt")))
    readLines(file.path(llp_workdir, "argv-llp.txt")) else character(0)
  ok(all(c("<-tweak_inits>", "<-no-picky>", "<-clean=1>") %in% llp_argv),
     "U1 (c) llp argv carries -tweak_inits, -no-picky and -clean= (PF-12)")
  ok(identical(llp_argv[1:2], c("<model.mod>", "<-thetas=1,2>")) &&
       !any(grepl("^<-(omegas|sigmas|rse_)", llp_argv)),
     "U1 llp argv carries the declared selection only")
  llp_summary <- if (!inherits(llp, "error")) llp$summary else NULL
  ok(!is.null(llp_summary) &&
       identical(attr(llp_summary, "sections"), "confidence.intervals") &&
       length(attr(llp_summary, "missing_sections")) == 0L &&
       identical(llp_summary$section, rep(1L, 4L)) &&
       identical(names(llp$record$operational), c("threads", "retries")),
     "U1 llp parser recognizes the verified confidence.intervals section")
  ok(!is.null(llp_summary) && "state: completed" %in% u1_marker(llp_workdir, "llp"),
     "U1 llp marker completed")
  Sys.unsetenv("U1_FIXTURES")

  # U2 (units programme): npc and sse on a stub executor. The results files
  # carry the section titles verified on the stored EB1 runs (2026-09-28),
  # including the trailing blanks PsN writes after "NPC results"; every
  # number is synthetic and no value of a real run appears here. The sse
  # stub writes the per-sample file under the name PsN wrote in EB1
  # (raw_results_<model stem>.csv, top of the run directory) and, as PsN
  # does, a raw_results.csv of the simulation step in simulation_dir1/.
  u2_fixtures <- file.path(root, "u2-fixtures")
  dir.create(u2_fixtures)
  writeLines(c(
    "NPC run info",
    paste0('"","Date","observations","simulations","Modelfile","Simulation model",',
           '"Dependent variable","PsN version","NONMEM version",'),
    '"","2000-1-1",10,20,"model.mod","auto-generated","DV","0.0.0","nm000",',
    "",
    "NPC results  ",
    "10 observations out of 10",
    paste0('"","points below PI (count)","points below PI (%)",',
           '"outside CI for below PI","95% CI below: from (%)",',
           '"95% CI below: to (%)","points above PI (count)",',
           '"points above PI (%)","outside CI for above PI",',
           '"95% CI above: from (%)","95% CI above: to (%)",'),
    '"0% PI",1,1,"",1,1,1,1,"",1,1,',
    '"50% PI",1,1,"",1,1,1,1,"",1,1,',
    "",
    "NPC * (warning) statistics",
    paste0('"","Real data * count","Sim. data * count mean","Theoretical mean",',
           '"Sim. data * count median","Sim. data * count 95% CI from","to",'),
    '"",1,1,1,1,1,1,'
  ), file.path(u2_fixtures, "npc_results.csv"))
  writeLines(c(
    "SSE run info",
    paste0('"","Date","samples run","simulation model","PsN version",',
           '"NONMEM version","Sim. with uncertainty",'),
    '"","2000-1-1",2,"model.mod","v0.0.0","nm000","no",',
    "Reference/True values",
    '"","ofv","THETA1","OMEGA(1,1)",',
    '"Sim. model result",1,1,1,',
    "",
    "Simulation model",
    "Samples completed: 2",
    '"","ofv","TH_1","OM_1",',
    '"mean",1,1,1,',
    '"bias","",1,1,',
    '"standard error CI",',
    '"5%","",1,1,'
  ), file.path(u2_fixtures, "sse_results.csv"))
  writeLines(c("model,problem,subproblem,ofv", "1,1,1,1", "2,1,1,1"),
             file.path(u2_fixtures, "raw_results_per_sample.csv"))
  writeLines(c("model,problem,subproblem,ofv", "1,1,1,1"),
             file.path(u2_fixtures, "raw_results_simulation.csv"))
  writeLines(c("a,b", "1,2"), file.path(u2_fixtures, "flat_results.csv"))
  u2_executor <- file.path(bin_dir, "u2-executor")
  writeLines(c(
    "#!/usr/bin/env bash", "set -euo pipefail",
    "workdir=$1; shift; tool=$1; shift; cd \"$workdir\"", "run_dir=",
    "model=$1",
    ": > \"argv-$tool.txt\"",
    "for argument in \"$@\"; do printf '<%s>\\n' \"$argument\" >> \"argv-$tool.txt\"; case \"$argument\" in -directory=*) run_dir=${argument#-directory=} ;; esac; done",
    "mkdir -p \"$run_dir\"",
    "results=\"$U2_FIXTURES/${tool}_results.csv\"",
    "if [ \"${STUB_U2_MODE:-layout}\" = flat ]; then results=\"$U2_FIXTURES/flat_results.csv\"; fi",
    "cp \"$results\" \"$run_dir/${tool}_results.csv\"",
    "if [ \"$tool\" = sse ]; then",
    "  mkdir -p \"$run_dir/simulation_dir1\"",
    "  cp \"$U2_FIXTURES/raw_results_simulation.csv\" \"$run_dir/simulation_dir1/raw_results.csv\"",
    "  cp \"$U2_FIXTURES/raw_results_per_sample.csv\" \"$run_dir/raw_results_${model%.*}.csv\"",
    "fi"
  ), u2_executor)
  Sys.chmod(u2_executor, "0755")
  Sys.setenv(U2_FIXTURES = u2_fixtures)
  u2_run <- function(tool, label, mode = "layout", ...) {
    workdir <- file.path(root, paste0("u2-", tool, "-", label))
    dir.create(workdir)
    write_model(workdir, "model.mod")
    Sys.setenv(STUB_U2_MODE = mode)
    on.exit(Sys.unsetenv("STUB_U2_MODE"), add = TRUE)
    entry <- if (identical(tool, "npc")) simeval$reference_psn_npc
             else simeval$reference_psn_sse
    result <- tryCatch(entry(workdir, "model.mod", samples = 20, seed = 20260830,
                             engine_config = u1_config, executor = u2_executor, ...),
                       error = function(e) e)
    if (inherits(result, "error"))
      cat(sprintf("      %s error: %s\n", tool, conditionMessage(result)))
    argv_file <- file.path(workdir, paste0("argv-", tool, ".txt"))
    list(result = result,
         argv = if (file.exists(argv_file)) readLines(argv_file) else character(0),
         marker = u1_marker(workdir, tool))
  }
  u2_summary <- function(run) if (inherits(run$result, "error")) NULL else run$result$summary

  npc_run <- u2_run("npc", "layout")
  npc_summary <- u2_summary(npc_run)
  ok(!is.null(npc_summary) && isTRUE(npc_summary[["available"]]) &&
       identical(npc_summary[["sections"]],
                 c("NPC run info", "NPC results", "NPC * (warning) statistics")) &&
       length(npc_summary[["missing_sections"]]) == 0L &&
       length(npc_summary[["section"]]) == length(npc_summary[["lines"]]) &&
       identical(npc_summary[["section"]][c(1, 5, 11)], 1:3) &&
       is.null(npc_summary[["coverage"]]),
     "U2 npc parser recognizes the verified sections, coverage stays NULL")
  ok(!is.null(npc_summary) && "state: completed" %in% npc_run$marker &&
       identical(basename(npc_run$result[["results_csv"]]), "npc_results.csv"),
     "U2 npc marker completed and npc_results.csv selected")
  ok(identical(npc_run$argv[1:3], c("<model.mod>", "<-samples=20>", "<-seed=20260830>")) &&
       "<-tweak_inits>" %in% npc_run$argv &&
       !any(grepl("^<-(no-)?picky>$", npc_run$argv)) &&
       !any(grepl("^<-(auto_bin|bin_by_count|no_of_bins|bin_array|lloq|uloq)", npc_run$argv)),
     "U2 npc argv: declared tokens, no binning or censoring token, nothing for FALSE picky")
  npc_flat <- u2_summary(u2_run("npc", "flat", mode = "flat"))
  ok(!is.null(npc_flat) &&
       identical(npc_flat[["missing_sections"]],
                 c("NPC run info", "NPC results")),
     "U2 npc parser reports the sections a results file lacks")

  sse_run <- u2_run("sse", "layout", estimate_simulation = TRUE)
  sse_summary <- u2_summary(sse_run)
  ok(!is.null(sse_summary) && isTRUE(sse_summary[["available"]]) &&
       identical(sse_summary[["sections"]],
                 c("SSE run info", "Reference/True values", "Simulation model",
                   "Samples completed: 2")) &&
       length(sse_summary[["missing_sections"]]) == 0L &&
       length(sse_summary[["section"]]) == length(sse_summary[["lines"]]) &&
       identical(sse_summary[["section"]][c(1, 4, 8, 9)], 1:4),
     "U2 sse parser recognizes the verified sections")
  sse_flat <- u2_summary(u2_run("sse", "flat", mode = "flat", estimate_simulation = TRUE))
  ok(!is.null(sse_flat) &&
       identical(sse_flat[["missing_sections"]],
                 c("SSE run info", "Reference/True values", "Simulation model")),
     "U2 sse parser reports the sections a results file lacks")
  # PF-4: the per-sample file is raw_results_<model stem>.csv. Before U2 the
  # entry point asked for raw_results.csv, which matched only the simulation
  # step's file in simulation_dir1/ (EB1's marker recorded that path).
  sse_raw <- if (!inherits(sse_run$result, "error")) sse_run$result[["raw_results"]] else NA
  cat(sprintf("      sse per-sample selection: %s\n",
              if (is.na(sse_raw)) "NA" else
                sub(paste0(".*/", basename(sse_run$result[["run_dir"]]), "/"), "", sse_raw)))
  ok(identical(basename(sse_raw), "raw_results_model.csv") &&
       identical(basename(dirname(sse_raw)), basename(sse_run$result[["run_dir"]])) &&
       any(grepl("^  raw_results: .*/raw_results_model\\.csv\"$", sse_run$marker)),
     "U2 sse selects the per-sample raw_results_<model stem>.csv (PF-4)")
  ok("<-estimate_simulation>" %in% sse_run$argv &&
       !("<-no-estimate_simulation>" %in% sse_run$argv) &&
       !any(grepl("^<-(no-)?picky>$", sse_run$argv)) &&
       !any(grepl("^<-alternative_models=", sse_run$argv)),
     "U2 sse estimate_simulation = TRUE emits -estimate_simulation only")
  sse_false <- u2_run("sse", "nosim", estimate_simulation = FALSE)
  ok("<-no-estimate_simulation>" %in% sse_false$argv &&
       !("<-estimate_simulation>" %in% sse_false$argv),
     "U2 sse estimate_simulation = FALSE emits -no-estimate_simulation")
  sse_na <- u2_run("sse", "na", estimate_simulation = NA)
  ok(inherits(sse_na$result, "error") &&
       grepl("estimate_simulation must be TRUE or FALSE",
             conditionMessage(sse_na$result), fixed = TRUE) &&
       length(sse_na$marker) == 0L,
     "U2 sse refuses an estimate_simulation that is neither TRUE nor FALSE")
  Sys.unsetenv("U2_FIXTURES")

  # Fixtures never rewrite the production manifests (checked by digest, so
  # the check holds whether a production unit is draft or approved).
  ok(identical(vapply(production_manifests, sha, character(1)), production_digests),
     "Fixture approvals leave production manifests byte-identical")

  cat(sprintf("\nreview runtime tests: %d passed, %d failed\n", passed, failed))
  invisible(failed)
}

if (.invoked_as_script()) {
  quit(status = if (.run_review_runtime_tests() == 0L) 0L else 1L)
}
