#!/usr/bin/env Rscript
# Focused contract battery for the composed covariate task.
#
# All inputs are synthetic. Public repository files are copied through the
# explicit allowlist below into a disposable mini-repository; no workspace,
# credential, private-data, gate, or targets-store tree is copied.

.invoked_as_script <- function() {
  fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  length(fa) > 0L && grepl("test_covariate_task\\.R$", sub("^--file=", "", fa[1]))
}

.run_covariate_task_tests <- function() {
  args <- commandArgs(FALSE)
  file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
  TOOLS <- normalizePath(file.path(dirname(file_arg), ".."), mustWork = TRUE)
  REPO <- normalizePath(file.path(TOOLS, ".."), mustWork = TRUE)
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

  pass <- 0L
  fail <- 0L
  ok <- function(cond, name) {
    cond <- tryCatch(isTRUE(cond), error = function(e) {
      cat(sprintf("      error: %s\n", conditionMessage(e)))
      FALSE
    })
    if (cond) {
      pass <<- pass + 1L
      cat(sprintf("PASS  %s\n", name))
    } else {
      fail <<- fail + 1L
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

  for (pkg in c("targets", "yaml", "digest", "callr")) {
    if (!requireNamespace(pkg, quietly = TRUE))
      stop("test_covariate_task requires R package ", pkg, call. = FALSE)
  }
  suppressPackageStartupMessages(library(targets))

  public_allowlist <- c(
    "pipeline/compose.R",
    "pipeline/tasks/base_model.R",
    "pipeline/tasks/data_assembly.R",
    "config/engine.yaml",
    "tools/engine_config.R",
    "tools/yaml_scalar.R",
    "pipeline/tasks/covariate.R",
    "tools/check_environment.R",
    "tools/ci_check.R",
    "tools/evidence_notes.R",
    "tools/gate.R",
    "tools/gate_package.R",
    "tools/load_reference.R",
    "tools/nonmem_frames.R",
    "tools/method_manifest.R",
    "tools/psn_invocation.R",
    "skills/covariate-priors/MANIFEST.yaml",
    "skills/covariate-priors/reference/covariate_effect.R",
    "skills/covariate-priors/reference/covariate_search.R",
    "skills/covariate-priors/reference/nested_lrt.R",
    "skills/covariate-priors/reference/psn_covsearch.R",
    "skills/covariate-priors/reference/scm_config.R",
    "skills/psn-uncertainty/reference/psn_uncertainty.R"
  )
  missing_public <- public_allowlist[!file.exists(file.path(REPO, public_allowlist))]
  if (length(missing_public))
    stop("required public file absent: ", paste(missing_public, collapse = ", "),
         call. = FALSE)

  protected <- c(
    "skills/covariate-priors/MANIFEST.yaml",
    "skills/covariate-priors/reference/covariate_effect.R",
    "skills/covariate-priors/reference/covariate_search.R",
    "skills/covariate-priors/reference/nested_lrt.R",
    "skills/covariate-priors/reference/psn_covsearch.R",
    "skills/covariate-priors/reference/scm_config.R"
  )
  protected_sha_before <- vapply(file.path(REPO, protected), digest::digest,
                                 character(1), file = TRUE, algo = "sha256")

  FX <- tempfile("pmx-r2-covariate-")
  dir.create(FX)
  on.exit(unlink(FX, recursive = TRUE, force = TRUE), add = TRUE)
  for (rel in public_allowlist) {
    dst <- file.path(FX, rel)
    dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(file.path(REPO, rel), dst, overwrite = FALSE))
      stop("failed to copy allowlisted public file: ", rel, call. = FALSE)
  }
  copied <- sort(sub(paste0("^", FX, "/"), "",
                       list.files(FX, recursive = TRUE, full.names = TRUE)))
  ok(identical(copied, sort(public_allowlist)),
     "fixture contains exactly the explicit public-file allowlist")

  setwd(FX)
  source("tools/method_manifest.R")
  source("tools/load_reference.R")
  source("tools/evidence_notes.R")
  source("tools/gate_package.R")
  source("tools/check_environment.R")
  source("pipeline/compose.R")

  source("tools/nonmem_frames.R")
  source("tools/engine_config.R")
  manifest_file <- "skills/covariate-priors/MANIFEST.yaml"
  public_manifest <- readLines(manifest_file)
  approved <- public_manifest
  approved <- sub("^reviewed_by: .*$", 'reviewed_by: "Alex Rowan"', approved)
  approved <- sub("^reviewed_at: .*$", 'reviewed_at: "2026-11-20"', approved)
  for (unit in c("covariate_effect", "nested_lrt")) {
    row <- match(paste0("  ", unit, ":"), approved) + 1L
    stopifnot(identical(approved[row], "    status: draft"))
    approved[row] <- "    status: approved"
  }
  writeLines(approved, manifest_file)

  relative <- function(path) sub(paste0(FX, "/"), "", path, fixed = TRUE)
  sha <- function(path) digest::digest(file = path, algo = "sha256")
  write_case <- function(method_unit = "covariate_effect",
                         source = "external",
                         mutate_handoff = identity,
                         mutate_map = identity,
                         prefix = "case") {
    root <- tempfile(paste0(prefix, "-"), tmpdir = file.path(FX, "tmp"))
    dir.create(dirname(root), recursive = TRUE, showWarnings = FALSE)
    dir.create(root)
    model <- file.path(root, "parent-model.R")
    data <- file.path(root, "parent-data.csv")
    config <- file.path(root, "parent-run-config.yaml")
    map <- file.path(root, "map-inputs.yaml")
    handoff <- file.path(root, "frozen-handoff.yaml")
    writeLines("parent_model <- function() NULL", model)
    writeLines(c("ID,WT", "1,50", "2,100"), data)
    writeLines(c("engine: synthetic", "mode: no-execution"), config)
    map_obj <- if (identical(method_unit, "covariate_effect")) {
      list(
        method_unit = method_unit,
        priors = list(list(covariate = "WT", parameter = "CL",
                           direction = "increase", mechanism = "allometric theory")),
        criteria = list(band_lower = 0.80, band_upper = 1.25),
        arguments = list(wt_low = 50, wt_high = 100,
                         exponents = c(0.75, 1.0)))
    } else {
      list(
        method_unit = method_unit,
        priors = list(list(covariate = "APGR", parameter = "CL",
                           direction = "decrease", mechanism = "maturation proxy")),
        criteria = list(criterion_dofv = 3.84, df = 1, alpha = 0.05),
        arguments = list(ofv_parent = 100, ofv_child = 95))
    }
    map_obj <- mutate_map(map_obj)
    yaml::write_yaml(map_obj, map)
    handoff_obj <- list(
      schema_version = 1,
      source = source,
      method_unit = method_unit,
      producing_analysis_id = "SYN-PARENT",
      producing_run_id = "run-001",
      parent_fit_id = "fit-001",
      owner = "synthetic-fixture",
      created_at = "2026-09-12T00:00:00Z",
      engine = list(name = "synthetic-engine", version = "1.0"),
      model = list(path = model, sha256 = sha(model)),
      data = list(path = data, sha256 = sha(data)),
      run_config = list(path = config, sha256 = sha(config)),
      map_inputs = list(path = map, sha256 = sha(map)))
    handoff_obj <- mutate_handoff(handoff_obj)
    yaml::write_yaml(handoff_obj, handoff)
    list(root = root, handoff = handoff, model = model, data = data,
         config = config, map = map, method_unit = method_unit)
  }

  case_effect <- write_case("covariate_effect", prefix = "effect")
  task_input_effect <- list(method_unit = "covariate_effect",
                            external_input = relative(case_effect$handoff))
  module_env <- new.env(parent = globalenv())
  module_env$task_input <- task_input_effect
  module_env$analysis_id <- "R2-SYN"
  module_env$upstream_gate <- NULL
  module_env$task_targets <- NULL
  sys.source("pipeline/tasks/covariate.R", envir = module_env, keep.source = FALSE)
  ok(exists("covariate_task_compute", envir = module_env, inherits = FALSE) &&
       exists("covariate_reference_executor", envir = module_env, inherits = FALSE),
     "module exposes its production compute and fixed reference executor")
  compute <- get("covariate_task_compute", envir = module_env)
  production_executor <- get("covariate_reference_executor", envir = module_env)

  invoke <- function(x, task_input = list(method_unit = x$method_unit,
                                           external_input = x$handoff),
                     loader = load_reference,
                     executor = production_executor,
                     model = x$model, data = x$data, config = x$config,
                     map = x$map) {
    compute(
      analysis_id = "R2-SYN",
      task_input = task_input,
      covariate_input_file = x$handoff,
      covariate_model_file = model,
      covariate_data_file = data,
      covariate_run_config_file = config,
      covariate_map_inputs_file = map,
      reference_loader = loader,
      reference_executor = executor)
  }

  cat("\n--- approved helpers and exact package contract -------------------------\n")
  effect <- invoke(case_effect)
  ok(is.data.frame(effect$result) &&
       identical(effect$result$exponent, c(0.75, 1.0)) &&
       isTRUE(all.equal(effect$result$r, c(2^(1 - 0.75), 1))) &&
       identical(effect$result$within_band, c(TRUE, TRUE)),
     "covariate_effect executes approved helper with declared values")

  case_lrt <- write_case("nested_lrt", prefix = "lrt")
  lrt <- invoke(case_lrt)
  ok(is.list(lrt$result) && identical(lrt$result$dofv, 5) &&
       isTRUE(lrt$result$significant) && isTRUE(lrt$result$retained) &&
       identical(lrt$result$criterion_dofv, 3.84) &&
       identical(lrt$result$df, 1) && identical(lrt$result$alpha, 0.05),
     "nested_lrt executes approved helper with declared values")

  expected_keys <- c("analysis", "task", "artifacts", "tables", "notes")
  expected_tables <- c("Frozen input provenance", "MAP prior list",
                       "Declared criteria", "Method provenance",
                       "Computed evidence", "Diagnostics")
  expected_schemas <- list(
    "Frozen input provenance" = c("source", "role", "path", "declared_sha256",
      "observed_sha256", "producing_analysis_id", "producing_run_id",
      "parent_fit_id", "owner", "created_at", "engine_name", "engine_version"),
    "MAP prior list" = c("covariate", "parameter", "direction", "mechanism"),
    "Declared criteria" = c("criterion", "value"),
    "Method provenance" = c("unit", "status", "reference_path", "entrypoint",
      "reference_sha256", "dependency_path", "dependency_sha256"),
    "Computed evidence" = c("result_row", "field", "value"),
    "Diagnostics" = c("check", "status", "detail"))
  pkg <- effect$package
  ok(identical(names(pkg), expected_keys) && identical(names(pkg$tables), expected_tables),
     "package and table keys are exact and ordered")
  ok(all(vapply(expected_tables, function(nm)
    identical(names(pkg$tables[[nm]]), expected_schemas[[nm]]), logical(1))),
     "all six package table schemas are exact")
  expected_artifacts <- data.frame(
    artifact = c("frozen handoff", "parent model", "parent data",
                 "parent run configuration", "MAP inputs"),
    path = c(case_effect$handoff, case_effect$model, case_effect$data,
             case_effect$config, case_effect$map),
    source = c("covariate_input_file", "covariate_model_file",
               "covariate_data_file", "covariate_run_config_file",
               "covariate_map_inputs_file"), stringsAsFactors = FALSE)
  ok(identical(pkg$artifacts, expected_artifacts),
     "artifact table has exact five rows, paths, and target sources")
  observed <- pkg$tables[["Frozen input provenance"]]$observed_sha256
  ok(identical(unname(observed),
       unname(vapply(expected_artifacts$path, sha, character(1)))),
     "frozen provenance recomputes all five file digests")
  ok(identical(pkg$tables[["MAP prior list"]],
               data.frame(covariate = "WT", parameter = "CL",
                          direction = "increase", mechanism = "allometric theory",
                          stringsAsFactors = FALSE)),
     "MAP prior list is copied from executed frozen input")
  ok(identical(pkg$tables[["Declared criteria"]]$criterion,
               c("band_lower", "band_upper")) &&
       identical(pkg$tables[["Declared criteria"]]$value, c(0.80, 1.25)),
     "declared criteria are passed through without defaults")
  expected_notes <- list(
    authored_note("Agent interpretation status: PENDING. No interpretation has been supplied; computed evidence remains unchanged."),
    authored_note("SME disposition status: PENDING FORMAL SIGNATURE. No disposition has been supplied; this status cannot authorize execution or alter computed evidence."))
  ok(identical(pkg$notes, expected_notes),
     "package carries exact typed pending interpretation and disposition notes")
  diag <- pkg$tables[["Diagnostics"]]
  ok(any(diag$detail == "Approved pure-R reference helper executed.") &&
       any(diag$detail == "No R2 engine command was constructed or executed."),
     "diagnostics distinguish helper computation from upstream engine provenance")
  ok(!dir.exists(file.path(case_effect$root, "outputs")) &&
       !dir.exists(file.path(case_effect$root, "run")),
     "approved pure-R computation creates no output or engine run directory")

  cat("\n--- local guard refuses before loader and executor ----------------------\n")
  for (unit in c("covariate_search", "scm_config", "psn_covsearch", "unknown_unit")) {
    loader_called <- FALSE
    executor_called <- FALSE
    sentinel_loader <- function(...) { loader_called <<- TRUE; stop("LOADER REACHED") }
    sentinel_executor <- function(...) { executor_called <<- TRUE; stop("EXECUTOR REACHED") }
    expect_stop(
      invoke(case_effect,
             task_input = list(method_unit = unit,
                               external_input = relative(case_effect$handoff)),
             loader = sentinel_loader, executor = sentinel_executor),
      paste0("unit ", unit, " is refused by task-local allowlist"),
      "not approved for the R2 covariate task")
    ok(!loader_called && !executor_called,
       paste0("unit ", unit, " refusal precedes loader and executor"))
  }

  cat("\n--- frozen handoff and MAP refusals -------------------------------------\n")
  expect_guard_stop <- function(x, label, regexp, ...) {
    loader_called <- FALSE
    executor_called <- FALSE
    e <- tryCatch(invoke(x, loader = function(...) {
      loader_called <<- TRUE
      load_reference(...)
    }, executor = function(...) {
      executor_called <<- TRUE
      production_executor(...)
    }, ...), error = function(e) e)
    ok(inherits(e, "error") && grepl(regexp, conditionMessage(e)), label)
    ok(!loader_called && !executor_called, paste0(label, " precedes loader/executor"))
  }

  expect_guard_stop(
    case_effect, "common guard refuses a missing source declaration",
    "exactly one source",
    task_input = list(method_unit = "covariate_effect"))
  expect_guard_stop(
    case_effect, "common guard refuses simultaneous external and upstream routes",
    "exactly one source",
    task_input = list(method_unit = "covariate_effect",
                      external_input = case_effect$handoff,
                      upstream_target = "base_model_frozen_yaml"))

  bad_hash <- write_case(mutate_handoff = function(h) {
    h$model$sha256 <- paste(rep("0", 64), collapse = "")
    h
  }, prefix = "bad-hash")
  expect_guard_stop(bad_hash, "declared hash mismatch is refused", "digest mismatch")

  mutated <- write_case(prefix = "mutated")
  writeLines(c(readLines(mutated$model), "# changed after freeze"), mutated$model)
  expect_guard_stop(mutated, "post-freeze file mutation is refused", "digest mismatch")

  missing_prov <- write_case(mutate_handoff = function(h) {
    h$parent_fit_id <- NULL
    h
  }, prefix = "missing-provenance")
  expect_guard_stop(missing_prov, "missing provenance field is refused", "parent_fit_id")

  empty_priors <- write_case(mutate_map = function(m) { m$priors <- list(); m },
                             prefix = "empty-priors")
  expect_guard_stop(empty_priors, "empty prior list is refused", "non-empty prior list")

  malformed_prior <- write_case(mutate_map = function(m) {
    m$priors[[1]]$mechanism <- NULL
    m
  }, prefix = "bad-prior")
  expect_guard_stop(malformed_prior, "malformed prior row is refused", "exactly")

  bad_criteria <- write_case(mutate_map = function(m) {
    m$criteria$band_upper <- NULL
    m
  }, prefix = "bad-criteria")
  expect_guard_stop(bad_criteria, "missing declared criterion is refused", "criteria")

  bad_args <- write_case(mutate_map = function(m) {
    m$arguments$extra <- 9
    m
  }, prefix = "bad-arguments")
  expect_guard_stop(bad_args, "undeclared method argument is refused", "arguments")

  vector_criterion <- write_case(mutate_map = function(m) {
    m$criteria$band_lower <- c(0.80, 0.90)
    m
  }, prefix = "vector-criterion")
  expect_guard_stop(vector_criterion, "vector-valued criterion is refused before loading",
                    "single finite numeric")

  vector_scalar_argument <- write_case(mutate_map = function(m) {
    m$arguments$wt_low <- c(40, 50)
    m
  }, prefix = "vector-scalar-argument")
  expect_guard_stop(vector_scalar_argument,
                    "vector-valued scalar argument is refused before loading",
                    "single finite numeric")

  method_mismatch <- write_case(mutate_handoff = function(h) {
    h$method_unit <- "nested_lrt"
    h
  }, prefix = "method-mismatch")
  expect_guard_stop(method_mismatch, "task/handoff/MAP method mismatch is refused",
                    "method_unit")

  wrong_source <- write_case(source = "upstream", prefix = "wrong-source")
  expect_guard_stop(wrong_source, "non-external handoff route is refused", "source.*external")

  invalid_timestamp <- write_case(mutate_handoff = function(h) {
    h$created_at <- "2026-99-99T99:99:99Z"
    h
  }, prefix = "invalid-timestamp")
  expect_guard_stop(invalid_timestamp, "invalid creation timestamp is refused",
                    "created_at")

  duplicate_role <- write_case(mutate_handoff = function(h) {
    h$data <- h$model
    h
  }, prefix = "duplicate-role")
  expect_guard_stop(duplicate_role, "one file cannot fill duplicate frozen roles",
                    "duplicate-role", data = duplicate_role$model)

  non_file <- write_case(prefix = "non-file")
  dir.create(file.path(non_file$root, "not-a-file"))
  expect_guard_stop(non_file, "non-regular target path is refused", "regular file",
                    model = file.path(non_file$root, "not-a-file"))

  absent <- write_case(prefix = "absent")
  unlink(absent$data)
  expect_guard_stop(absent, "missing target file is refused", "regular file")

  cat("\n--- special-file refusals precede reads and digests ---------------------\n")
  case_field <- c(handoff = "handoff", model = "model", data = "data",
                  run_config = "config", map_inputs = "map")
  replace_case_path <- function(x, role, replacement) {
    x[[case_field[[role]]]] <- replacement
    if (!identical(role, "handoff")) {
      handoff <- yaml::read_yaml(x$handoff)
      handoff[[role]]$path <- replacement
      yaml::write_yaml(handoff, x$handoff)
    }
    x
  }

  for (role in names(case_field)) {
    linked <- write_case(prefix = paste0("symlink-", role))
    link <- file.path(linked$root, paste0(role, "-link"))
    if (!file.symlink(linked[[case_field[[role]]]], link))
      stop("failed to create synthetic symlink for role ", role, call. = FALSE)
    linked <- replace_case_path(linked, role, link)
    expect_guard_stop(
      linked, paste0("symlink frozen ", role, " path is refused"), "regular file")
  }

  if (!identical(.Platform$OS.type, "unix") || !nzchar(Sys.which("mkfifo")))
    stop("special-file battery requires Unix mkfifo", call. = FALSE)
  fifo_invoke <- function(x, marker) {
    tryCatch(callr::r(
      function(fixture, case, marker) {
        setwd(fixture)
        source("tools/method_manifest.R")
        source("tools/load_reference.R")
        source("tools/evidence_notes.R")
        source("tools/gate_package.R")
        source("pipeline/compose.R")
        env <- new.env(parent = globalenv())
        construction_handoff <- file.path(case$root, "frozen-handoff.yaml")
        env$task_input <- list(method_unit = case$method_unit,
                               external_input = sub(paste0(fixture, "/"), "", construction_handoff, fixed = TRUE))
        env$analysis_id <- "R2-SPECIAL"
        env$upstream_gate <- NULL
        env$task_targets <- NULL
        sys.source("pipeline/tasks/covariate.R", envir = env, keep.source = FALSE)
        tryCatch(env$covariate_task_compute(
          analysis_id = "R2-SPECIAL",
          task_input = list(method_unit = case$method_unit,
                            external_input = case$handoff),
          covariate_input_file = case$handoff,
          covariate_model_file = case$model,
          covariate_data_file = case$data,
          covariate_run_config_file = case$config,
          covariate_map_inputs_file = case$map,
          reference_loader = function(...) {
            writeLines("loader", marker)
            load_reference(...)
          },
          reference_executor = function(...) {
            writeLines("executor", marker)
            env$covariate_reference_executor(...)
          }), error = function(e) conditionMessage(e))
      }, args = list(fixture = FX, case = x, marker = marker),
      timeout = 5, spinner = FALSE),
      error = function(e) e)
  }

  for (role in names(case_field)) {
    fifo_case <- write_case(prefix = paste0("fifo-", role))
    fifo <- file.path(fifo_case$root, paste0(
      role, "-fifo", if (identical(role, "handoff")) ".yaml" else ""))
    status <- system2("mkfifo", fifo, stdout = FALSE, stderr = FALSE)
    if (!identical(status, 0L))
      stop("failed to create synthetic FIFO for role ", role, call. = FALSE)
    fifo_case <- replace_case_path(fifo_case, role, fifo)
    marker <- file.path(fifo_case$root, "reference-called")
    result <- fifo_invoke(fifo_case, marker)
    ok(is.character(result) && length(result) == 1L &&
         grepl("regular file", result),
       paste0("FIFO frozen ", role,
              " path is refused before a blocking read or digest"))
    ok(!file.exists(marker),
       paste0("FIFO frozen ", role,
              " refusal precedes loader and executor"))
  }

  cat("\n--- composition and generated gate ownership ----------------------------\n")
  pipe <- compose_pipeline(
    analysis_id = "R2-SYN", tasks = "covariate",
    inputs = list(covariate = task_input_effect))
  pipe_names <- vapply(pipe, function(t) t$settings$name, character(1))
  required_targets <- c("covariate_input_file", "covariate_model_file",
                        "covariate_data_file", "covariate_run_config_file",
                        "covariate_map_inputs_file", "covariate_result",
                        "covariate_package", "gate_covariate_file", "gate_covariate")
  ok(all(required_targets %in% pipe_names),
     "external-only covariate pipeline exposes five files, result, package, and generated gate")
  file_targets <- required_targets[seq_len(5)]
  ok(all(vapply(file_targets, function(nm)
    identical(pipe[[which(pipe_names == nm)]]$settings$format, "file"), logical(1))),
    "all five frozen inputs are format=file targets")
  syms <- function(nm) all.names(pipe[[which(pipe_names == nm)]]$command$expr[[1]])
  ok("covariate_result" %in% syms("covariate_package") &&
       all(c("covariate_package", "covariate_result", file_targets) %in%
             syms("gate_covariate")),
     "package and generated gate commands carry production dependency wiring")
  ok(isTRUE(tryCatch(verify_gate_dominance(pipe), error = function(e) FALSE)),
     "external-only covariate pipeline satisfies gate dominance")
  ok(!file.exists(file.path(FX, "gates", "R2-SYN", "covariate.md")),
     "composition does not render a covariate gate record")
  expect_stop(
    compose_pipeline("R2-MISSING", "covariate",
                     inputs = list(covariate = list(method_unit = "covariate_effect"))),
    "composer refuses a missing covariate source", "external_input")
  non_yaml <- file.path(case_effect$root, "handoff.txt")
  file.copy(case_effect$handoff, non_yaml)
  expect_stop(
    compose_pipeline("R2-NON-YAML", "covariate", inputs = list(
      covariate = list(method_unit = "covariate_effect", external_input = relative(non_yaml)))),
    "external covariate source must be a YAML handoff", "external_input")

  dir.create("models/diffs", recursive = TRUE)
  for (p in c("models/run-notes.md", "models/run-config.yaml", "models/101.mod",
              "models/diffs/101.diff", "plan.md")) writeLines("# Synthetic model input", p)
  base_input <- list(engine = "nonmem", model_dir = "models", map = "plan.md",
    seed = 42L, npde_nsim = 12L, run_budget = list(used = 0L, max = 3L),
    gof = list(cont_cov = "WT", cat_cov = "GROUP", strata = "GROUP"),
    runs = list(list(run = "101", step = "base model", based_on = "", retry_of = "",
      status = "launch", hypothesis = "Fixture model", etas = c(ETA1 = "CL"))))
  writeLines(c("ID,WT", "1,50", "2,75"), "assembly.csv")
  writeLines("# composition-only specification", "assembly.yml")
  da_input <- list(
    source = "assembly.csv", spec = "assembly.yml", map = "plan.md",
    expect = list(n_subjects = 2L, source_rows = 2L, lloq = NA_real_),
    weights = list(baseline = "assembly.csv", column = "WT", probs = c(0.05, 0.95),
                   quantile_type = 7L, labels = c("Median", "Low", "High")))
  comparison <- list(covariate_priors = list(list(covariate = "WT", parameter = "CL",
    direction = "increase", mechanism = "Synthetic fixture")), covariate_dir = "handoffs",
    covariate_comparisons = list(wt_ratio = list(unit = "covariate_effect", run = "101",
      band_lower = 0.8, band_upper = 1.25, prior = 1L, wt_points = c("Low", "High"),
      exponent_fixed = 0.75)))
  expect_stop(
    compose_pipeline("R2-DUAL", c("data_assembly", "base_model", "covariate"), inputs = list(
      data_assembly = da_input, base_model = base_input,
      covariate = task_input_effect)),
    "composer refuses simultaneous upstream and external covariate sources",
    "two declared input sources")

  cat("\n--- optional actual base-model producer construction --------------------\n")
  construct_base <- function(input) {
    e <- new.env(parent = globalenv())
    e$task_input <- input
    e$analysis_id <- "R2-BASE-CONSTRUCTION"
    e$upstream_gate <- "gate_data_assembly"
    e$task_targets <- NULL
    sys.source("pipeline/tasks/base_model.R", envir = e, keep.source = FALSE)
    setNames(e$task_targets, vapply(e$task_targets, function(t) t$settings$name, character(1)))
  }
  base_default <- construct_base(base_input)
  ok(!("base_model_frozen_yaml" %in% names(base_default)) &&
       !("base_model_frozen_yaml" %in% all.names(base_default$base_model_package$command$expr[[1]])),
     "COV-FROZEN: no undeclared base-model handoff target or package branch")
  base_with_handoff <- construct_base(c(base_input, comparison))
  producer <- base_with_handoff$base_model_frozen_yaml
  ok(identical(producer$settings$format, "file") &&
       all(c("nm_write_handoffs", "bm_ctl_101_file", "dataset_file") %in%
           all.names(producer$command$expr[[1]])) &&
       "base_model_frozen_yaml" %in% all.names(base_with_handoff$base_model_package$command$expr[[1]]),
     "COV-FROZEN: declared NONMEM comparison generates and packages its exact handoff")
  malformed <- comparison
  malformed$covariate_comparisons$wt_ratio$prior <- 2L
  expect_stop(construct_base(c(base_input, malformed)),
              "COV-FROZEN: absent prior index refuses at composition", "covariate_comparisons")

  # Supply only the recorded NONMEM result inputs. The real base-model target
  # writes all three files per comparison and the real covariate task reads them.
  fixture_ofv <- c("101" = 100, "102" = 95, "103" = 99)
  estimation <- "$ESTIMATION METHOD=1"
  for (run in names(fixture_ofv)) {
    dir <- file.path("models", run)
    dir.create(dir)
    if (run != "101") {
      writeLines(paste("$PROBLEM Synthetic run", run), file.path("models", paste0(run, ".mod")))
      writeLines(paste("Synthetic run", run), file.path("models", "diffs", paste0(run, ".diff")))
    }
    writeLines("(NONMEM) VERSION 7.5.0", file.path(dir, paste0(run, ".lst")))
    writeLines("Synthetic estimates", file.path(dir, paste0(run, ".ext")))
    writeLines("{}", file.path(dir, "bbi_config.json"))
    record <- list(meta = list(run = run), est = estimation,
                   run_start = as.POSIXct("2026-09-30 00:00:00", tz = "UTC"))
    result <- list(record = record,
                   frame = structure(data.frame(), estimation = estimation),
                   results = list(ofv = fixture_ofv[[run]],
                                  heuristics = list(minimization_terminated = FALSE)))
    saveRDS(result, file.path(dir, "result.rds"))
  }
  write_consumer <- function(path, input, producer, tamper_comparison = NULL) {
    command <- producer$command$expr[[1]]
    if (!is.null(tamper_comparison)) {
      map_file <- paste0("map-inputs-", tamper_comparison, ".yaml")
      command <- bquote({
        files <- .(command)
        cat("# changed after freeze\n", file = files[basename(files) == .(map_file)], append = TRUE)
        files
      })
    }
    src <- c(
      "library(targets)", "source('tools/method_manifest.R')",
      "source('tools/load_reference.R')", "source('tools/evidence_notes.R')",
      "source('tools/nonmem_frames.R')", "source('pipeline/compose.R')",
      paste0("task_input <- ", paste(deparse(input), collapse = " ")),
      "e <- new.env(parent=globalenv())", "e$task_input <- task_input",
      "e$analysis_id <- 'R2-UPSTREAM'", "e$upstream_gate <- NULL",
      "e$task_targets <- NULL",
      "sys.source('pipeline/tasks/covariate.R', envir=e, keep.source=FALSE)",
      "fixture_runs <- c('101', '102', '103')",
      "fit_inputs <- lapply(fixture_runs, function(run) list(",
      "  tar_target_raw(paste0('bm_ctl_', run, '_file'), call('identity', file.path('models', paste0(run, '.mod'))), format='file'),",
      "  tar_target_raw(paste0('bm_res_', run), call('readRDS', file.path('models', run, 'result.rds')))))",
      "producer_inputs <- c(unlist(fit_inputs, recursive=FALSE), list(",
      "  tar_target_raw('dataset_file', quote('assembly.csv'), format='file'),",
      "  tar_target_raw('da_weight_constants', quote(data.frame(quantity=c('Low', 'High'), value_kg=c(50, 100))))))",
      "producer <- tar_target_raw('base_model_frozen_yaml',",
      paste0("  quote(", paste(deparse(command), collapse = "\n"), "), format='file')"),
      "c(producer_inputs, list(producer), e$task_targets)")
    writeLines(src, path)
    path
  }

  cat("\n--- canonical upstream consumer edge and scheduler refusal --------------\n")
  upstream_input <- list(method_unit = "covariate_effect",
                         upstream_target = "base_model_frozen_yaml")
  single <- module_env$covariate_single_handoff
  ok(identical(single("fixture/frozen.yaml"), "fixture/frozen.yaml"),
     "single upstream route still accepts a legacy scalar handoff path")
  expect_stop(single(c("fixture/map-inputs-wt.yaml", "fixture/run-config-wt.yaml")),
              "single upstream route refuses a producer with no handoff", "0 handoff files")
  expect_stop(single(c("fixture/handoff-wt.yaml", "fixture/handoff-ht.yaml")),
              "single upstream route refuses an ambiguous producer", "2 handoff files")
  expect_stop(single(c("fixture/handoff-wt.yaml", "fixture/map-inputs-ht.yaml")),
              "single upstream route refuses another handoff's companion", "unexpected.*companion")
  upstream_pipe <- compose_pipeline(
    "R2-UPSTREAM", c("data_assembly", "base_model", "covariate"), inputs = list(
      data_assembly = da_input, base_model = c(base_input, comparison),
      covariate = upstream_input))
  upstream_names <- vapply(upstream_pipe, function(t) t$settings$name, character(1))
  upstream_input_cmd <- upstream_pipe[[which(upstream_names == "covariate_input_file")]]$
    command$expr[[1]]
  ok("base_model_frozen_yaml" %in% all.names(upstream_input_cmd) &&
       !any(c("get", "tar_read", "bm_records", "base_model_package") %in%
            all.names(upstream_input_cmd)),
     "upstream covariate input carries literal static producer edge only")
  ok(isTRUE(tryCatch(verify_gate_dominance(upstream_pipe), error = function(e) FALSE)),
     "actual producer/consumer composition satisfies gate dominance")
  for (bad_target in list(NULL, "", c("base_model_frozen_yaml", "other"),
                          "bm_records", "base_model_package", "handoffs/handoff-wt_ratio.yaml")) {
    bad_input <- list(method_unit = "covariate_effect")
    if (!is.null(bad_target)) bad_input$upstream_target <- bad_target
    expect_stop(
      compose_pipeline("R2-BAD-UPSTREAM", c("data_assembly", "base_model", "covariate"), inputs = list(
        data_assembly = da_input, base_model = c(base_input, comparison),
        covariate = bad_input)),
      paste0("upstream consumer refuses noncanonical producer declaration: ",
             paste(bad_target, collapse = ",")),
      "upstream_target")
  }

  upstream_script <- file.path(FX, "synthetic-upstream.R")
  upstream_store <- file.path(FX, "_targets_R2-UPSTREAM")
  write_consumer(upstream_script, upstream_input, producer)
  targets::tar_make(script = upstream_script, store = upstream_store,
                    names = tidyselect::all_of("covariate_package"), reporter = "silent")
  upstream_pkg <- targets::tar_read_raw("covariate_package", store = upstream_store)
  ok(identical(upstream_pkg$tables[["Frozen input provenance"]]$source,
               rep("upstream", 5L)) &&
       identical(upstream_pkg$artifacts$path,
                 c("handoffs/handoff-wt_ratio.yaml", "models/101.mod", "assembly.csv",
                   "handoffs/run-config-wt_ratio.yaml", "handoffs/map-inputs-wt_ratio.yaml")),
     "actual NONMEM handoff target executes the upstream consumer through the common guard")

  missing_script <- file.path(FX, "synthetic-upstream-missing-producer.R")
  missing_store <- file.path(FX, "_targets_R2-UPSTREAM-MISSING")
  observe <- file.path(FX, "loader-observe.txt")
  writeLines(c(
    "library(targets)", "source('tools/method_manifest.R')",
    "source('tools/load_reference.R')", "source('tools/evidence_notes.R')",
    "task_input <- list(method_unit='covariate_effect', upstream_target='base_model_frozen_yaml')",
    "e <- new.env(parent=globalenv())", "e$task_input <- task_input",
    "e$analysis_id <- 'R2-UPSTREAM-MISSING'", "e$upstream_gate <- NULL",
    "e$task_targets <- NULL",
    "source('pipeline/compose.R')",
    "sys.source('pipeline/tasks/covariate.R', envir=e, keep.source=FALSE)",
    "e$task_targets"), missing_script)
  old_test_root <- Sys.getenv("TEST_REPO_ROOT", unset = NA_character_)
  old_observe <- Sys.getenv("MM_GATE_OBSERVE", unset = NA_character_)
  Sys.setenv(TEST_REPO_ROOT = FX, MM_GATE_OBSERVE = observe)
  on.exit({
    if (is.na(old_test_root)) Sys.unsetenv("TEST_REPO_ROOT")
    else Sys.setenv(TEST_REPO_ROOT = old_test_root)
    if (is.na(old_observe)) Sys.unsetenv("MM_GATE_OBSERVE")
    else Sys.setenv(MM_GATE_OBSERVE = old_observe)
  }, add = TRUE)
  missing_error <- tryCatch({
    targets::tar_make(script = missing_script, store = missing_store,
                      names = tidyselect::all_of("covariate_package"), reporter = "silent")
    NULL
  }, error = function(e) e)
  ok(inherits(missing_error, "error") && !file.exists(observe),
     "missing producer is refused by scheduler before reference loader executes")

  wrong_route_case <- write_case("covariate_effect", source = "external",
                                 prefix = "upstream-wrong-source")
  expect_guard_stop(
    wrong_route_case,
    "upstream declaration rejects external-labelled frozen record in common guard",
    "source.*upstream",
    task_input = upstream_input)

  cat("\n--- fresh targets store and documented renderer -------------------------\n")
  analysis_script <- file.path(FX, "synthetic-covariate.R")
  store <- file.path(FX, "_targets_R2-SYN")
  script_lines <- c(
    "library(targets)",
    "source('tools/method_manifest.R')",
    "source('tools/load_reference.R')",
    "source('tools/evidence_notes.R')",
    "source('tools/gate_package.R')",
    "source('pipeline/compose.R')",
    sprintf("task_input <- list(method_unit='covariate_effect', external_input=%s)",
            deparse(relative(case_effect$handoff))),
    "e <- new.env(parent=globalenv())",
    "e$task_input <- task_input",
    "e$analysis_id <- 'R2-SYN'",
    "e$upstream_gate <- NULL",
    "e$task_targets <- NULL",
    "source('pipeline/compose.R')",
    "sys.source('pipeline/tasks/covariate.R', envir=e, keep.source=FALSE)",
    "e$task_targets")
  writeLines(script_lines, analysis_script)
  targets::tar_make(script = analysis_script, store = store, reporter = "silent")
  meta <- targets::tar_meta(store = store, fields = c("name", "error"))
  relevant_errors <- meta$error[meta$name %in% c("covariate_result", "covariate_package")]
  ok("covariate_package" %in% meta$name &&
       all(is.na(relevant_errors) | !nzchar(relevant_errors)),
     "fresh targets process executes production covariate result and package targets")
  gate_file <- file.path(FX, "gates", "R2-SYN", "covariate.md")
  dir.create(dirname(gate_file), recursive = TRUE)
  writeLines(c("# Covariate gate", "", "Analysis ID: R2-SYN", "Stage: covariate",
               "", "## SME decision", "", "- [ ] APPROVED", "- [ ] REVISE"),
             gate_file)
  render_gate_from_store("R2-SYN", "covariate", store = store,
                         script = analysis_script, gate_file = gate_file,
                         check_store = FALSE)
  rendered <- readLines(gate_file, warn = FALSE)
  rendered_text <- paste(rendered, collapse = "\n")
  rendered_paths <- relative(expected_artifacts$path)
  ok(all(vapply(rendered_paths, function(p) grepl(p, rendered_text, fixed = TRUE),
                logical(1))) &&
       all(vapply(expected_artifacts$path, function(p)
         grepl(paste0("sha256:", sha(p)), rendered_text, fixed = TRUE), logical(1))),
     "renderer reads executed store and hashes all five artifact paths")
  ok(all(vapply(expected_tables, function(nm)
    grepl(paste0("### ", nm), rendered_text, fixed = TRUE), logical(1))),
     "rendered gate contains all six package table headings")
  note_lines <- vapply(expected_notes, `[[`, character(1), "text")
  begin <- match(GATE_BLOCK_START, rendered)
  end <- match(GATE_BLOCK_END, rendered)
  authored <- match("### Authored narrative", rendered)
  ok(!is.na(begin) && !is.na(end) && !is.na(authored) && authored > end &&
       all(vapply(note_lines, function(x) any(rendered == x), logical(1))) &&
       !any(vapply(note_lines, function(x) any(rendered[begin:end] == x), logical(1))),
     "both typed pending notes render in authored narrative outside digested block")

  # The workflow profile and the registered battery inventory are checked together.
  reg <- new.env(parent = globalenv())
  sys.source(file.path(REPO, "tools", "ci_check.R"), envir = reg)
  setwd(REPO)
  invisible(capture.output(reg$check_battery_registration()))
  ok(reg$.fail_count == 0L && "test_covariate_task.R" %in% reg$.ci_battery_files(),
     "COV-FROZEN: both parsed CI jobs reach the registered aggregate battery")
  case_files <- list.files("tools/tests", pattern = "^test_.*[.](R|sh)$", full.names = TRUE)
  registry <- tempfile("covariate-registration-")
  dir.create(file.path(registry, "tools/tests"), recursive = TRUE)
  stopifnot(all(file.create(file.path(registry, case_files))))
  dir.create(file.path(registry, ".github/workflows"), recursive = TRUE)
  ci_path <- file.path(registry, ".github/workflows/ci.yaml")
  workflow <- reg$.ci_workflow_runs(file.path(REPO, ".github/workflows/ci.yaml"))
  workflow <- list(jobs = workflow$jobs)
  yaml::write_yaml(workflow, ci_path)
  setwd(registry)
  original_batteries <- reg$.CI_BATTERIES
  reg$.CI_BATTERIES <- Filter(function(x) x$file != "test_covariate_task.R", original_batteries)
  reg$.fail_count <- 0L
  invisible(capture.output(reg$check_battery_registration()))
  ok(reg$.fail_count == 1L, "COV-FROZEN: CI rejects omitted battery registration")
  reg$.CI_BATTERIES <- original_batteries
  j <- workflow$jobs$"validated-stack"$steps
  i <- which(vapply(j, function(step) identical(
    step$run, 'R_LIBS_SITE="$R_LIBS_USER" Rscript tools/ci_check.R'), logical(1)))
  stopifnot(length(i) == 1L)
  j[[i]]$run <- "Rscript tools/ci_check.R --light"
  workflow$jobs$"validated-stack"$steps <- j
  yaml::write_yaml(workflow, ci_path)
  reg$.fail_count <- 0L
  invisible(capture.output(reg$check_battery_registration()))
  ok(reg$.fail_count == 1L, "COV-FROZEN: CI rejects a missing validated aggregate")

  setwd(REPO)
  source("tools/method_manifest.R", local = TRUE)
  manifest <- validate_method_manifest("skills/covariate-priors/MANIFEST.yaml",
                                       "skills/covariate-priors")
  ok(length(manifest$units) == 5L && !nzchar(manifest$reviewed_by) &&
       all(vapply(manifest$units, function(unit) identical(unit$status, "draft"), logical(1))),
     "COV-FROZEN: public methods remain unsigned in the production manifest")
  protected_sha_after <- vapply(file.path(REPO, protected), digest::digest,
                                character(1), file = TRUE, algo = "sha256")
  ok(identical(protected_sha_before, protected_sha_after),
     "manifest and all five covariate reference bytes remain unchanged")

  # Part K exercises the invocation form.
  # Synthetic inputs only; every stub number below is a fixture value.
  cat("\n--- part K: invocation list ---------------------------------------------\n")
  setwd(FX)
  k_inv <- c(wt_v_fwd = "nested_lrt", wt_cl_fwd = "nested_lrt",
             wt_ratio = "covariate_effect")
  k_block <- list(upstream_target = "base_model_frozen_yaml", invocations = k_inv)
  k_base <- c(base_input, comparison)
  child_run <- c(wt_v_fwd = "102", wt_cl_fwd = "103", wt_ratio = "101")
  for (run in child_run[1:2]) {
    entry <- base_input$runs[[1]]
    entry$run <- run
    entry$based_on <- "101"
    k_base$runs <- c(k_base$runs, list(entry))
  }
  k_base$covariate_priors <- c(
    list(list(covariate = "WT", parameter = "V", direction = "increase",
              mechanism = "Synthetic fixture")), k_base$covariate_priors)
  k_nested <- lapply(seq_along(child_run[1:2]), function(i)
    list(unit = "nested_lrt", parent = "101", child = unname(child_run[i]),
         test = "forward", criterion_dofv = 3.84, df = 1, alpha = 0.05, prior = i))
  names(k_nested) <- names(child_run)[1:2]
  ratio <- k_base$covariate_comparisons$wt_ratio
  ratio$prior <- 2L
  k_base$covariate_comparisons <- c(k_nested, list(wt_ratio = ratio))
  k_compose <- function(block, tasks = c("data_assembly", "base_model", "covariate"),
                        id = "R2-INVOC") {
    inputs <- list(covariate = block)
    if ("base_model" %in% tasks) inputs <- c(list(data_assembly = da_input, base_model = k_base), inputs)
    compose_pipeline(id, tasks, inputs = inputs)
  }
  k_names_of <- function(pipe) vapply(pipe, function(t) t$settings$name, character(1))
  k_expected <- c(unlist(lapply(names(k_inv), function(n)
    paste0("covariate_", n, "_",
           c("handoff", "model", "data", "run_config", "map_inputs", "result")))),
    "covariate_package")
  k_file_names <- k_expected[!grepl("_result$|^covariate_package$", k_expected)]

  k_pipe <- tryCatch(k_compose(k_block), error = function(e) e)
  k_pipe_names <- if (inherits(k_pipe, "error")) character(0) else k_names_of(k_pipe)
  ok(identical(grep("^covariate_", k_pipe_names, value = TRUE), k_expected) &&
       all(c("gate_covariate_file", "gate_covariate") %in% k_pipe_names) &&
       length(k_file_names) == 15L &&
       all(vapply(k_file_names, function(nm)
         identical(k_pipe[[which(k_pipe_names == nm)]]$settings$format, "file"),
         logical(1))),
     "invocation list: K01 composes the 19 declared covariate targets and the gate")
  k_allowed_upstream <- c("base_model_frozen_yaml", "gate_base_model",
                          ENV_TARGET, MANIFEST_TARGET)
  ok(all(vapply(names(k_inv), function(n) {
    cmd <- k_pipe[[which(k_pipe_names == paste0("covariate_", n, "_handoff"))]]$
      command$expr[[1]]
    used <- intersect(all.names(cmd), k_pipe_names)
    "base_model_frozen_yaml" %in% used &&
      !length(setdiff(used, k_allowed_upstream)) &&
      !any(c("get", "tar_read", "bm_records", "base_model_package") %in%
             all.names(cmd))
  }, logical(1))),
     "invocation list: K02 each hand-off target reads base_model_frozen_yaml only")
  ok(isTRUE(tryCatch(verify_gate_dominance(k_pipe), error = function(e) FALSE)),
     "invocation list: K03 the invocation composition satisfies gate dominance")

  k_refuse <- list(
    list("unnamed", unname(k_inv), "invocations"),
    list("empty", character(0), "invocations"),
    list("a list", as.list(k_inv), "invocations"),
    list("a duplicate name", c(wt_v_fwd = "nested_lrt", wt_v_fwd = "nested_lrt"),
         "invocations"),
    list("a name failing the pattern", c(Wt_v_fwd = "nested_lrt"), "invocations"),
    list("a value NA", c(wt_v_fwd = NA_character_), "invocations"),
    list("a value empty", c(wt_v_fwd = ""), "invocations"),
    list("a value of two strings", list(wt_v_fwd = c("nested_lrt", "nested_lrt")),
         "invocations"),
    list("an unknown unit", c(wt_v_fwd = "scm_config"), "invocations"))
  for (r in k_refuse) {
    blk <- k_block
    blk["invocations"] <- list(r[[2]])
    expect_stop(k_compose(blk),
                paste0("invocation list: K04 module refuses invocations ", r[[1]]),
                r[[3]])
  }
  expect_stop(k_compose(c(k_block, list(method_unit = "nested_lrt"))),
              "invocation list: K04 module refuses a top-level method_unit",
              "method_unit")
  expect_stop(k_compose(k_block["invocations"]),
              "invocation list: K04 module refuses an absent upstream_target",
              "upstream_target")
  expect_stop(k_compose(modifyList(k_block, list(upstream_target = "bm_records"))),
              "invocation list: K04 module refuses a non-exact upstream_target",
              "upstream_target")
  expect_stop(k_compose(c(k_block, list(extra_field = TRUE))),
              "invocation list: K04 module refuses an unknown field by name",
              "extra_field")
  k_ext <- file.path(FX, "tmp", "k-external.yaml")
  expect_stop(k_compose(c(k_block, list(external_input = relative(k_ext)))),
              "invocation list: K04 composer refuses external_input with base_model",
              "two declared input sources")
  expect_stop(k_compose(c(k_block, list(external_input = relative(k_ext))), tasks = "covariate"),
              "invocation list: K04 module refuses external_input without the producer",
              "external_input")
  k_engine <- tryCatch(k_compose(c(k_block, list(engine = "nonmem"))),
                       error = function(e) e)
  ok(!inherits(k_engine, "error") &&
       identical(grep("^covariate_", k_names_of(k_engine), value = TRUE), k_expected),
     "invocation list: K04 engine key is accepted and composes the same 19 names")

  k_select <- module_env$covariate_select_handoff
  k_cover <- module_env$covariate_check_coverage
  k_paths <- c("/k/handoff-x.yaml", "/k/map-inputs-x.yaml", "/k/run-config-x.yaml",
               "/k/handoff-y.yaml")
  ok(identical(k_select(k_paths, "x"), "/k/handoff-x.yaml"),
     "invocation list: K05 selection returns the one basename match")
  expect_stop(k_select(k_paths, "z"),
              "invocation list: K05 selection stops on zero matches naming the invocation",
              "invocation 'z' matches 0 hand-off files")
  expect_stop(k_select(c(k_paths, "/other/handoff-x.yaml"), "x"),
              "invocation list: K05 selection stops on two matches",
              "invocation 'x' matches 2 hand-off files")
  expect_stop(k_cover(k_paths, "x"),
              "invocation list: K06 coverage names a hand-off without an invocation",
              "hand-offs without an invocation: y;")
  expect_stop(k_cover(k_paths, c("x", "y", "z")),
              "invocation list: K06 coverage names an invocation without a hand-off",
              "invocations without a hand-off: z")
  ok(isTRUE(k_cover(k_paths, c("y", "x"))),
     "invocation list: K06 coverage passes equal sets in a different order")

  k_producer <- function(dir, selected = names(k_inv)) {
    declaration <- k_base
    declaration$covariate_dir <- dir
    declaration$covariate_comparisons <- declaration$covariate_comparisons[selected]
    construct_base(declaration)$base_model_frozen_yaml
  }
  k_script <- function(name, input, producer, tamper_comparison = NULL)
    write_consumer(file.path(FX, paste0("k-", name, ".R")), input, producer,
                   tamper_comparison)
  k_run <- function(script, store, name = "covariate_package") tryCatch({
    targets::tar_make(script = script, store = store,
                      names = tidyselect::all_of(name), reporter = "silent")
    NULL
  }, error = function(e) e)
  k_meta_error <- function(store, name) {
    m <- tryCatch(targets::tar_meta(store = store, fields = c("name", "error")),
                  error = function(e) NULL)
    if (is.null(m) || !(name %in% m$name)) return(NA_character_)
    m$error[m$name == name][1]
  }

  k_out <- "k-invocations"
  k_main <- k_producer(k_out)
  k_main_script <- k_script("main", k_block, k_main)
  k_main_store <- file.path(FX, "_targets_R2-INVOC")
  k_main_error <- k_run(k_main_script, k_main_store)
  k_pkg <- tryCatch(targets::tar_read_raw("covariate_package", store = k_main_store),
                    error = function(e) NULL)
  k_tables <- k_pkg$tables
  k_art <- k_pkg$artifacts
  k_notes <- vapply(k_pkg$notes, function(x) x$text, character(1))
  k_manifest_names <- tryCatch(
    targets::tar_manifest(script = k_main_script, fields = "name")$name,
    error = function(e) character(0))
  ok(is.null(k_main_error) && is.list(k_pkg),
     "invocation list: K07 covariate_package builds from nine synthetic paths")
  ok(is.data.frame(k_art) && anyDuplicated(k_art$path) == 0L && nrow(k_art) == 11L,
     "invocation list: K07 artifacts are de-duplicated by path to 11 rows")
  ok(identical(names(k_tables)[1], "Invocation list") && length(k_tables) == 19L &&
       anyDuplicated(names(k_tables)) == 0L &&
       identical(k_tables[["Invocation list"]]$invocation, names(k_inv)),
     "invocation list: K07 tables open with the invocation list in declared order")
  # Every column of the invocation list (plan Section 3 decision 7) against the
  # synthetic producer's values, per invocation in declared order.
  k_list <- k_tables[["Invocation list"]]
  k_expected <- list(
    order = seq_along(k_inv), invocation = names(k_inv),
    method_unit = unname(k_inv),
    producing_run_id = unname(child_run),
    parent_fit_id = rep("101", length(k_inv)),
    handoff = file.path(k_out, paste0("handoff-", names(k_inv), ".yaml")))
  ok(is.data.frame(k_list) && identical(names(k_list), names(k_expected)),
     "invocation list: K07 invocation list has exactly the six specified columns")
  for (k_col in names(k_expected))
    ok(is.data.frame(k_list) && identical(k_list[[k_col]], k_expected[[k_col]]),
       paste0("invocation list: K07 column ", k_col,
              " equals the synthetic value per invocation"))
  ok(all(vapply(names(k_inv), function(n) identical(
    k_tables[[paste0(n, ": Frozen input provenance")]]$source, rep("upstream", 5L)),
    logical(1))),
     "invocation list: K07 every invocation reads the upstream route")
  ok(length(k_notes) > 0L && anyDuplicated(k_notes) == 0L,
     "invocation list: K07 each distinct note text occurs once")
  k_sources <- if (is.character(k_art$source))
    trimws(unlist(strsplit(k_art$source, ",", fixed = TRUE))) else character(0)
  ok(length(k_sources) > 0L && all(k_sources %in% k_manifest_names) &&
       identical(k_art$source[k_art$path == "models/101.mod"],
                 paste0("covariate_", names(k_inv), "_model", collapse = ", ")),
     "invocation list: K07 artifact sources name composed targets")

  k_bad_unit <- c(wt_v_fwd = "covariate_effect", wt_cl_fwd = "nested_lrt",
                  wt_ratio = "covariate_effect")
  k_unit_store <- file.path(FX, "_targets_R2-INVOC-UNIT")
  k_run(k_script("unit", modifyList(k_block, list(invocations = k_bad_unit)),
                 k_main), k_unit_store, "covariate_wt_v_fwd_result")
  ok(grepl("frozen handoff method_unit must match task_input$method_unit",
           k_meta_error(k_unit_store, "covariate_wt_v_fwd_result"), fixed = TRUE),
     "invocation list: K08 a hand-off unit unlike its invocation's unit is refused")

  k_tamper_store <- file.path(FX, "_targets_R2-INVOC-TAMPER")
  k_tamper_error <- k_run(k_script("tamper", k_block, k_producer("k-tampered"),
                                   tamper_comparison = "wt_cl_fwd"), k_tamper_store)
  ok(inherits(k_tamper_error, "error") &&
       grepl("digest mismatch for frozen map_inputs",
             k_meta_error(k_tamper_store, "covariate_wt_cl_fwd_result"), fixed = TRUE) &&
       !("covariate_package" %in% tryCatch(targets::tar_objects(store = k_tamper_store),
                                          error = function(e) character(0))),
     "invocation list: K09 a tampered MAP-input file stops that result and the package")

  # Plan decision 6 in the composed pipeline, not only in the check function:
  # an invocation list that skips a frozen hand-off stops covariate_package.
  k_skip_store <- file.path(FX, "_targets_R2-INVOC-SKIP")
  k_skip_error <- k_run(k_script(
    "skip", modifyList(k_block, list(invocations = k_inv[1:2])), k_main),
    k_skip_store)
  ok(inherits(k_skip_error, "error") &&
       grepl("hand-offs without an invocation: wt_ratio;",
             k_meta_error(k_skip_store, "covariate_package"), fixed = TRUE),
     "invocation list: K06 the composed package refuses a frozen hand-off the invocation list skips")

  k_gate <- file.path(FX, "gates", "R2-INVOC", "covariate.md")
  dir.create(dirname(k_gate), recursive = TRUE, showWarnings = FALSE)
  writeLines(c("# Covariate gate", "", "Analysis ID: R2-INVOC", "Stage: covariate",
               "", "## SME decision", "", "- [ ] APPROVED", "- [ ] REVISE"), k_gate)
  k_render <- tryCatch({
    render_gate_package(k_gate, k_pkg$artifacts, k_pkg$tables, k_pkg$notes)
    .gate_verify_content(k_gate, readLines(k_gate, warn = FALSE))
  }, error = function(e) e)
  ok(is.list(k_pkg) && identical(k_render, character(0)) &&
       any(readLines(k_gate, warn = FALSE) == "<!-- BEGIN GENERATED PACKAGE -->"),
     "invocation list: K10 the merged package renders and verifies without refusal")

  k_m <- names(k_inv)[2]
  k_legacy_store <- file.path(FX, "_targets_R2-INVOC-LEGACY")
  k_legacy_error <- k_run(k_script(
    "legacy", list(method_unit = k_inv[[k_m]], upstream_target = "base_model_frozen_yaml"),
    k_producer(k_out, k_m)),
    k_legacy_store)
  k_legacy_result <- tryCatch(
    targets::tar_read_raw("covariate_result", store = k_legacy_store),
    error = function(e) NULL)
  k_invoc_result <- tryCatch(
    targets::tar_read_raw(paste0("covariate_", k_m, "_result"), store = k_main_store),
    error = function(e) NULL)
  k_legacy_tables <- k_legacy_result$package$tables
  ok(is.null(k_legacy_error) && !is.null(k_legacy_result) && !is.null(k_invoc_result) &&
       identical(k_legacy_result$result, k_invoc_result$result),
     "invocation list: K11 legacy and invocation forms give an identical result")
  ok(length(k_legacy_tables) == 6L &&
       all(vapply(names(k_legacy_tables), function(t)
         identical(k_legacy_tables[[t]], k_tables[[paste0(k_m, ": ", t)]]),
         logical(1))),
     "invocation list: K11 the six tables equal the merged package's named tables")

  cat(sprintf("\n%d passed; %d failed\n", pass, fail))
  if (fail > 0L) quit(status = 1L)
  invisible(TRUE)
}

if (.invoked_as_script()) .run_covariate_task_tests()
