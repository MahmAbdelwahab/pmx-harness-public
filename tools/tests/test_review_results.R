#!/usr/bin/env Rscript
# Regression battery for review result contracts.
#
# Hermetic: approved method fixtures identify the reviewer as "Alex Rowan" and
# live under tempdir(). Engine-facing calls use small contract doubles; no
# NONMEM, PsN, nlmixr2 model, live manifest, gate, or analysis output is used.
#
# PMX_REVIEW_RESULTS_SOURCE_ROOT may point at a disposable source snapshot.
# Run with: Rscript tools/tests/test_review_results.R

.invoked_as_script <- function() {
  fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  length(fa) > 0L && grepl("test_review_results\\.R$", sub("^--file=", "", fa[1]))
}

.run_review_results_tests <- function() {
  args <- commandArgs(FALSE)
  file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
  TOOLS <- normalizePath(file.path(dirname(file_arg), ".."), mustWork = TRUE)
  REPO <- normalizePath(file.path(TOOLS, ".."), mustWork = TRUE)
  SRC <- Sys.getenv("PMX_REVIEW_RESULTS_SOURCE_ROOT", REPO)
  SRC <- normalizePath(SRC, mustWork = TRUE)
  cat(sprintf("review-results: copying reference/tool files from %s\n", SRC))

  src_file <- function(rel) {
    candidate <- file.path(SRC, rel)
    if (file.exists(candidate)) candidate else file.path(REPO, rel)
  }
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
    if (is.null(e)) cat("      no error was raised\n")
    if (!is.null(e) && !hit) cat(sprintf("      got: %s\n", conditionMessage(e)))
    ok(hit, name)
  }

  fixture <- tempfile("review-results-")
  dir.create(fixture)
  old_wd <- setwd(fixture)
  on.exit(setwd(old_wd), add = TRUE)
  dir.create("tools")
  for (f in c("method_manifest.R", "load_reference.R", "record_selection.R", "seed.R")) {
    stopifnot(file.copy(src_file(file.path("tools", f)), file.path("tools", f)))
  }
  source("tools/method_manifest.R")
  sha <- function(path) .mm_file_sha256(path)

  install_skill <- function(skill, files, units) {
    ref_dir <- file.path("skills", skill, "reference")
    dir.create(ref_dir, recursive = TRUE, showWarnings = FALSE)
    for (f in files) {
      stopifnot(file.copy(src_file(file.path("skills", skill, "reference", f)),
                          file.path(ref_dir, f), overwrite = TRUE))
    }
    lines <- c(
      "# result-review fixture",
      paste0("skill: ", skill),
      "generated_at: 2026-09-11T00:00:00Z",
      'reviewed_by: "Alex Rowan"',
      'reviewed_at: "2026-09-11"',
      "units:"
    )
    for (unit in names(units)) {
      x <- units[[unit]]
      ref <- file.path("skills", skill, "reference", x$ref)
      lines <- c(
        lines,
        sprintf("  %s:", unit),
        "    status: approved",
        sprintf("    reference_path: %s", ref),
        sprintf("    entrypoints: [%s]", paste(x$entrypoints, collapse = ", ")),
        sprintf("    sha256: %s", sha(ref)),
        "    evidence: SKILL.md",
        '    reason: "synthetic regression fixture"'
      )
    }
    writeLines(lines, file.path("skills", skill, "MANIFEST.yaml"))
  }
  set_unit_status <- function(skill, unit, status) {
    path <- file.path("skills", skill, "MANIFEST.yaml")
    lines <- readLines(path, warn = FALSE)
    unit_line <- which(lines == sprintf("  %s:", unit))[1]
    stopifnot(length(unit_line) == 1L)
    lines[unit_line + 1L] <- sprintf("    status: %s", status)
    writeLines(lines, path)
  }
  load_ref <- function(skill, file) {
    load_reference(file.path("skills", skill),
                   file.path("skills", skill, "reference", file))
  }
  source("tools/load_reference.R")

  # PM-1 and PM-5: actual approved diagnostic references in fixture manifests.
  install_skill("diagnostics", c("npde_tests.R", "qualification_verdict.R"), list(
    npde_tests = list(ref = "npde_tests.R", entrypoints = "reference_npde_global_tests"),
    qualification_verdict = list(
      ref = "qualification_verdict.R",
      entrypoints = "reference_qualification_verdict"
    )
  ))
  nt <- load_ref("diagnostics", "npde_tests.R")
  qv <- load_ref("diagnostics", "qualification_verdict.R")

  criteria <- list(
    rse_fixed_max_pct = 30,
    rse_iiv_max_pct = 50,
    cond_no_max = 1000,
    shrink_flag_pct = 30,
    vpc_coverage_min_pct = 80,
    npde_p_flag = 0.01,
    boot_converged_min_pct = 80,
    boot_median_max_pct = 15
  )
  refit <- list(
    parFixed = data.frame(parameter = "theta", rse_pct = 1,
                          shrink_sd_pct = NA_real_),
    cond_no_cov = 1,
    refit = list(converged = TRUE, message = "fixture")
  )
  vpc <- list(
    coverage_pooled = data.frame(pct_within = 100, n_within_band = 1, n_bins = 1),
    coverage_strat = data.frame(pct_within = 100)
  )
  boot <- list(
    fixed = data.frame(pct_diff_natural_scale = 1,
                       pct_diff_estimation_scale = 1, ci_finite = TRUE),
    omega = data.frame(parameter = "eta", rse_pct = 1, ci_finite = TRUE),
    success_rate_pct = 100
  )

  n <- 20L
  z <- as.numeric(scale(seq_len(n)))
  intended_p <- 0.0099999
  values <- z + stats::qt(1 - intended_p / 2, df = n - 1L) / sqrt(n)
  raw_p <- stats::t.test(values, mu = 0)$p.value
  npde <- nt$reference_npde_global_tests(values, criteria$npde_p_flag)
  verdict <- qv$reference_qualification_verdict(refit, npde, vpc, boot, criteria)
  q2 <- verdict[verdict$criterion == "Q2", , drop = FALSE]
  ok(identical(npde$p_value[1], raw_p) && isTRUE(npde$flagged[1]),
     "PM-1: NPDE returns full machine precision used for its flag")
  ok(nrow(q2) == 1L && identical(q2$disposition, "FLAG"),
     "PM-1: verdict Q2 agrees with NPDE at declared p-value boundary")
  # Pin the comparison at machine precision. Reuse one real test statistic
  # and place the fixture threshold immediately either side of its raw p-value;
  # exact equality is thereby representable without mocking statistical code.
  epsilon <- .Machine$double.eps * raw_p
  thresholds <- c(below = raw_p + epsilon, equal = raw_p, above = raw_p - epsilon)
  for (position in names(thresholds)) {
    threshold <- thresholds[[position]]
    boundary_npde <- nt$reference_npde_global_tests(values, threshold)
    boundary_criteria <- criteria
    boundary_criteria$npde_p_flag <- threshold
    boundary_verdict <- qv$reference_qualification_verdict(
      refit, boundary_npde, vpc, boot, boundary_criteria)
    expected_flag <- identical(position, "below")
    ok(identical(boundary_npde$p_value[1], raw_p) &&
         identical(boundary_npde$flagged[1], expected_flag) &&
         identical(boundary_verdict$disposition[boundary_verdict$criterion == "Q2"],
                   if (expected_flag) "FLAG" else "PASS"),
       paste("PM-1: public NPDE and verdict agree when p is", position, "threshold"))
  }
  ordinary_npde <- nt$reference_npde_global_tests(z, criteria$npde_p_flag)
  ordinary_verdict <- qv$reference_qualification_verdict(
    refit, ordinary_npde, vpc, boot, criteria
  )
  ordinary_q2 <- ordinary_verdict[ordinary_verdict$criterion == "Q2", , drop = FALSE]
  ok(!any(ordinary_npde$flagged) && identical(ordinary_q2$disposition, "PASS"),
     "PM-1/5 control: complete ordinary evidence still returns Q2 PASS")

  # Review follow-up: finite values must also belong to their declared domain.
  for (p_bad in c(-0.1, 1.1)) {
    expect_stop(qv$reference_qualification_verdict(
      refit, data.frame(p_value = p_bad), vpc, boot, criteria),
      paste("PM-5: out-of-domain NPDE probability refuses", p_bad))
  }
  bad_coverage <- vpc
  bad_coverage$coverage_pooled$pct_within <- 101
  expect_stop(qv$reference_qualification_verdict(
    refit, ordinary_npde, bad_coverage, boot, criteria),
    "PM-5: coverage above 100 percent refuses")
  for (counts in list(c(0, 0), c(2, 1), c(0.5, 1))) {
    bad_coverage <- vpc
    bad_coverage$coverage_pooled$n_within_band <- counts[1]
    bad_coverage$coverage_pooled$n_bins <- counts[2]
    expect_stop(qv$reference_qualification_verdict(
      refit, ordinary_npde, bad_coverage, boot, criteria),
      paste("PM-5: invalid coverage counts refuse", paste(counts, collapse = "/")))
  }
  bad_boot <- boot
  bad_boot$success_rate_pct <- 101
  expect_stop(qv$reference_qualification_verdict(
    refit, ordinary_npde, vpc, bad_boot, criteria),
    "PM-5: bootstrap success above 100 percent refuses")

  empty_npde <- data.frame(p_value = numeric())
  expect_stop(qv$reference_qualification_verdict(refit, empty_npde, vpc, boot, criteria),
              "PM-5: empty NPDE evidence is rejected", "NPDE|npde|empty|row")
  empty_pooled <- vpc
  empty_pooled$coverage_pooled <- empty_pooled$coverage_pooled[FALSE, ]
  expect_stop(qv$reference_qualification_verdict(refit, data.frame(p_value = 0.5),
                                                 empty_pooled, boot, criteria),
              "PM-5: empty pooled VPC evidence is rejected", "coverage_pooled|empty|row")
  empty_strat <- vpc
  empty_strat$coverage_strat <- empty_strat$coverage_strat[FALSE, ]
  expect_stop(qv$reference_qualification_verdict(refit, data.frame(p_value = 0.5),
                                                 empty_strat, boot, criteria),
              "PM-5: empty stratified VPC evidence is rejected", "coverage_strat|empty|row")
  incomplete_boot <- boot
  incomplete_boot$fixed <- data.frame(dummy = 1)
  expect_stop(qv$reference_qualification_verdict(refit, data.frame(p_value = 0.5),
                                                 vpc, incomplete_boot, criteria),
              "PM-5: incomplete bootstrap evidence is rejected", "boot\\$fixed|column")
  nonfinite_npde <- data.frame(p_value = NA_real_)
  expect_stop(qv$reference_qualification_verdict(refit, nonfinite_npde, vpc, boot, criteria),
              "PM-5: non-finite NPDE evidence is rejected", "p_value|finite")

  # PM-4 policy source: public methods and battery consume these same signed
  # minimum queries; exact boundaries are existing SME-reviewed values.
  install_skill("diagnostics", c("npde_tests.R", "qualification_verdict.R",
                                  "npde.R", "bootstrap.R"), list(
    npde_tests = list(ref = "npde_tests.R", entrypoints = "reference_npde_global_tests"),
    qualification_verdict = list(
      ref = "qualification_verdict.R", entrypoints = "reference_qualification_verdict"
    ),
    npde = list(ref = "npde.R",
                entrypoints = c("reference_npde", "reference_npde_minimum_nsim")),
    bootstrap = list(
      ref = "bootstrap.R",
      entrypoints = c("reference_bootstrap", "reference_bootstrap_minimum_nboot")
    )
  ))
  npde_policy <- load_ref("diagnostics", "npde.R")
  bootstrap_policy <- load_ref("diagnostics", "bootstrap.R")
  ok(identical(npde_policy$reference_npde_minimum_nsim(), 500),
     "PM-4: signed NPDE reference exposes existing 500 minimum")
  ok(identical(bootstrap_policy$reference_bootstrap_minimum_nboot("medium"), 500) &&
       identical(bootstrap_policy$reference_bootstrap_minimum_nboot("high"), 1000),
     "PM-2/4: signed bootstrap reference exposes existing risk floors")
  ok(identical(formals(npde_policy$reference_npde)$nsim, quote(NULL)) &&
       identical(formals(bootstrap_policy$reference_bootstrap)$nboot, quote(NULL)),
     "PM-4: public methods derive omitted counts from their authoritative policy query")

  # PM-2 and PM-4: real qualification public runner and registry selection;
  # capture only engine-facing member functions.
  `%||%` <- function(a, b) if (is.null(a)) b else a
  sys.source(src_file("tools/qualification.R"), envir = environment())
  original_registry <- qualification_registry
  original_member_config <- qualification_member_config

  # Integrity query contract: draft is reportable BLOCKED metadata, while
  # malformed/drifted state escapes the battery as a hard integrity error.
  member_ran <- FALSE
  qualification_registry <- function() list(
    npde = list(
      risk = "medium", simulation = TRUE, reference = "npde.R",
      fn = function(fit, cfg) {
        member_ran <<- TRUE
        list(name = "npde", artifact = NA_character_, detail = "ran")
      }
    )
  )
  require_validated_method <- function(...)
    stop("throwing runtime gate API called instead of metadata query", call. = FALSE)
  set_unit_status("diagnostics", "npde", "draft")
  blocked <- tryCatch(qualification_battery(
    NULL, "medium", cfg = list(out_dir = tempdir(), npde_n = 500)
  ), error = function(e) e)
  ok(is.list(blocked) && nrow(blocked$members) == 1L &&
       grepl("^BLOCKED:", blocked$members$detail) && !member_ran,
     "qualification metadata: valid draft method is BLOCKED without execution")
  write("# fixture drift", file = file.path("skills", "diagnostics", "reference", "npde.R"),
        append = TRUE)
  expect_stop(qualification_battery(
                NULL, "medium", cfg = list(out_dir = tempdir(), npde_n = 500)),
              "qualification metadata: method digest drift remains a hard error",
              "EDITED|mismatch|digest")
  stopifnot(file.copy(src_file("skills/diagnostics/reference/npde.R"),
                      file.path("skills", "diagnostics", "reference", "npde.R"),
                      overwrite = TRUE))
  set_unit_status("diagnostics", "npde", "approved")
  qualification_member_config <- function(...)
    stop("policy digest mismatch", call. = FALSE)
  expect_stop(qualification_battery(
                NULL, "medium", cfg = list(out_dir = tempdir(), npde_n = 500)),
              "qualification metadata: policy-resolution integrity error is not a FAILED row",
              "policy digest mismatch")
  qualification_member_config <- original_member_config

  captured <- new.env(parent = emptyenv())
  qualification_registry <- function() {
    reg <- original_registry()
    reg$gof$fn <- function(fit, cfg) {
      captured$gof <- cfg
      list(name = "gof", artifact = NA_character_, detail = "captured")
    }
    reg$vpc$fn <- function(fit, cfg) {
      captured$vpc <- cfg
      list(name = "vpc", artifact = NA_character_, detail = "captured")
    }
    reg$npde$fn <- function(fit, cfg) {
      captured$npde <- cfg
      list(name = "npde", artifact = NA_character_, detail = "captured")
    }
    reg$bootstrap$fn <- function(fit, cfg) {
      captured$bootstrap <- cfg
      list(name = "bootstrap", artifact = NA_character_, detail = "captured")
    }
    reg
  }
  reference_validated <- function(file) TRUE

  expect_stop(qualification_battery(NULL, "medum", cfg = list(out_dir = tempdir())),
              "PM-2: unknown MODEL_RISK is rejected", "MODEL_RISK|model_risk|risk")
  expect_stop(qualification_battery(NULL, character(0), cfg = list(out_dir = tempdir())),
              "PM-2: empty MODEL_RISK is rejected", "MODEL_RISK|model_risk|risk")
  expect_stop(qualification_battery(NULL, NA_character_, cfg = list(out_dir = tempdir())),
              "PM-2: missing MODEL_RISK is rejected", "MODEL_RISK|model_risk|risk")
  expect_stop(qualification_battery(NULL, "high", cfg = list(
                out_dir = tempdir(), model_risk = "medium")),
              "PM-2: conflicting duplicate MODEL_RISK is rejected", "conflict|MODEL_RISK|model_risk")

  high <- qualification_battery(
    NULL, "high", disabled = c("gof", "vpc"),
    justification = "fixture isolates engine-facing members",
    cfg = list(out_dir = tempdir())
  )
  ok(identical(high$model_risk, "high") &&
       identical(captured$npde$model_risk, "high") &&
       identical(captured$bootstrap$model_risk, "high"),
     "PM-2: one validated high risk reaches every mandatory member")
  ok(identical(captured$npde$npde_n, 500) &&
       identical(captured$bootstrap$boot_n, 1000),
     "PM-4: high-risk defaults satisfy existing NPDE/bootstrap policy floors")

  medium <- qualification_battery(
    NULL, "medium", disabled = c("gof", "vpc"),
    justification = "fixture isolates engine-facing members",
    cfg = list(out_dir = tempdir())
  )
  ok(identical(medium$model_risk, "medium") &&
       identical(captured$npde$npde_n, 500) &&
       identical(captured$bootstrap$boot_n, 500),
     "PM-4: medium-risk defaults satisfy existing NPDE/bootstrap policy floors")
  explicit_high <- qualification_battery(
    NULL, "high", disabled = c("gof", "vpc"),
    justification = "fixture isolates engine-facing members",
    cfg = list(out_dir = tempdir(), model_risk = "high", npde_n = 500, boot_n = 1000)
  )
  ok(identical(explicit_high$model_risk, "high") &&
       identical(captured$npde$npde_n, 500) &&
       identical(captured$bootstrap$boot_n, 1000),
     "PM-2/4 control: matching explicit risk and exact floor overrides remain valid")
  low <- qualification_battery(NULL, "low", cfg = list(out_dir = tempdir()))
  ok(identical(low$model_risk, "low") &&
       identical(low$members$member, c("gof", "vpc")) &&
       identical(captured$vpc$vpc_n, 500) &&
       is.null(captured$vpc$npde_n) && is.null(captured$vpc$boot_n),
     "PM-4 control: ordinary low-risk battery does not query unselected policies")
  qualification_registry <- original_registry
  low_bootstrap <- qualification_battery(
    NULL, "low", enabled = "bootstrap", disabled = c("gof", "vpc"),
    justification = "fixture isolates unsupported bootstrap risk before engine execution",
    cfg = list(out_dir = tempdir())
  )
  low_bootstrap_row <- low_bootstrap$members[
    low_bootstrap$members$member == "bootstrap", , drop = FALSE
  ]
  ok(nrow(low_bootstrap_row) == 1L && grepl("^FAILED:.*arg", low_bootstrap_row$detail),
     "PM-4 control: explicitly enabled low-risk bootstrap remains unsupported")

  # PM-3: real public callers against a contract double matching current vpc.R.
  contract_dir <- file.path(fixture, "vpc-contract")
  dir.create(contract_dir)
  writeLines(c(
    "reference_vpc_prepare <- function(obs, sim, id='ID', x='TIME', repl='REP', evid='EVID', categorical=FALSE) list(obs=obs, sim=sim)",
    "reference_vpc_attach_pred <- function(obs, sim) obs",
    "reference_vpc_continuous <- function(...) structure(list(ok=TRUE), class='fixture_vpc')",
    "reference_qpc <- function(vpc, alpha, weights, sharp_ref, interval_ref) list(qpc=data.frame(ok=TRUE))"
  ), file.path(contract_dir, "vpc.R"))
  DIAGNOSTICS_REFERENCE <- contract_dir
  vpc_simulate <- function(...) list(
    obs = data.frame(ID = 1, TIME = 1, DV = 1, EVID = 0),
    sim = data.frame(ID = 1, TIME = 1, DV = 1, EVID = 0, REP = 1)
  )
  vpc_cfg <- list(vpc_n = 200, seed = 1, binning = list(method = "ntile", nbins = 2),
                  stratify = NULL, out_dir = tempdir(), run = "fixture",
                  qpc_weights = NULL, sharp_ref = NULL, interval_ref = NULL)
  plain <- tryCatch(run_tidyvpc(NULL, vpc_cfg, pred_corr = FALSE, render = FALSE),
                    error = function(e) e)
  corrected <- tryCatch(run_tidyvpc(NULL, vpc_cfg, pred_corr = TRUE, render = FALSE),
                        error = function(e) e)
  quantified <- tryCatch(original_registry()$qpc$fn(NULL, vpc_cfg), error = function(e) e)
  ok(is.list(plain) && isTRUE(plain$vpc$ok) &&
       is.list(corrected) && isTRUE(corrected$vpc$ok) &&
       is.list(quantified) && identical(quantified$name, "qpc"),
     "PM-3: VPC, pcVPC, and qpc callers use current event-column contract")

  # PM-6: actual exposure reference refuses a grid whose lower limit is not 0.
  install_skill("simulation-standards", "exposure_metrics.R", list(
    exposure_metrics = list(
      ref = "exposure_metrics.R",
      entrypoints = c("reference_exposure_by_subject", "reference_exposure_summary",
                      "reference_profile_summary")
    )
  ))
  exposure <- load_ref("simulation-standards", "exposure_metrics.R")
  sim <- data.frame(SCENARIO = "single", ID = 1, time = c(1, 2),
                    CP = c(0.5, 0.25), CL = 1, V = 1)
  scenarios <- list(single = list(regimen = list(amt = 1, cmt = 1), n = 1))
  expect_stop(exposure$reference_exposure_by_subject(
                sim, scenarios, observed = "CP", time = "time",
                clearance = "CL", volume = "V", auc_tolerance = 1e-6),
              "PM-6: AUC(0-tlast) rejects nonzero-origin sampling grid",
              "zero|0|origin|AUC")
  sim_zero <- rbind(data.frame(SCENARIO = "single", ID = 1, time = 0,
                               CP = 0, CL = 1, V = 1), sim)
  exposure_ok <- exposure$reference_exposure_by_subject(
    sim_zero, scenarios, observed = "CP", time = "time",
    clearance = "CL", volume = "V", auc_tolerance = 1e-6
  )
  ok(is.data.frame(exposure_ok) && nrow(exposure_ok) == 1L &&
       identical(exposure_ok$tlast, 2),
     "PM-6 control: supported zero-origin grid still computes existing metrics")

  # PM-7: actual forest sampler keeps matrix shape for one covariance draw.
  install_skill("forest-plots", "forest.R", list(
    forest = list(
      ref = "forest.R",
      entrypoints = c("reference_forest_covstats", "reference_forest_covgrid",
                      "reference_forest_samples", "reference_forest_samples_nlmixr",
                      "reference_forest_verify", "reference_forest_parametric",
                      "reference_forest_empirical", "reference_forest_plot")
    )
  ))
  forest <- load_ref("forest-plots", "forest.R")
  vcov.review_result_fit <- function(object, ...) object$vc
  registerS3method("vcov", "review_result_fit", vcov.review_result_fit,
                   envir = asNamespace("stats"))
  fit <- structure(
    list(theta = c(a = 1, b = 2),
         vc = matrix(c(0.1, 0, 0, 0.2), 2,
                     dimnames = list(c("a", "b"), c("a", "b")))),
    class = "review_result_fit"
  )
  draws <- tryCatch(forest$reference_forest_samples_nlmixr(fit, n = 1, seed = 1),
                    error = function(e) e)
  ok(is.data.frame(draws) && nrow(draws) == 2L &&
       identical(names(draws), c("a", "b")) &&
       identical(as.numeric(draws[1, ]), unname(fit$theta)),
     "PM-7: single covariance draw returns estimate plus one named draw")
  draws_two <- forest$reference_forest_samples_nlmixr(fit, n = 2, seed = 1)
  ok(is.data.frame(draws_two) && nrow(draws_two) == 3L &&
       identical(names(draws_two), c("a", "b")),
     "PM-7 control: multi-draw covariance shape remains unchanged")

  cat(sprintf("\nresult correctness review battery: %d passed, %d failed\n",
              .pass, .fail))
  quit(status = if (.fail > 0L) 1L else 0L)
}

if (.invoked_as_script()) .run_review_results_tests()
