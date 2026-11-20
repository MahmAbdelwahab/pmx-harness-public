#!/usr/bin/env Rscript
# Integrity batch 8 test battery (Codex round 8 findings F9-F15).
#
# Hermetic, same pattern as tools/tests/test_batch7.R: every fixture lives
# under tempdir() laid out as <root>/skills/<skill>/reference/<file>.R +
# MANIFEST.yaml, with copies of the tools/ files a reference sources, and the
# working directory is moved there so repo-relative paths resolve exactly as
# they do in the repository. No live manifest, gate record, reference file,
# store or pilot output is touched.
#
# Source of the copied files: the repository by default; the environment
# variable PMX_BATCH8_SOURCE_ROOT overrides it (a HEAD export or a staged
# patch tree laid out like the repository), which is how the red (pre-fix)
# and green (post-fix) runs are produced. Every run states its source.
#
# Run with:  Rscript tools/tests/test_batch8.R
# Exits non-zero if any assertion fails.
#
# SAFE TO SOURCE: guarded behind .invoked_as_script() like the other
# batteries, so tar_source("tools") only defines and runs nothing.
.invoked_as_script <- function() {
  fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  length(fa) > 0 && grepl("test_batch8\\.R$", sub("^--file=", "", fa[1]))
}

.run_batch8_tests <- function() {

args <- commandArgs(FALSE)
file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
TOOLS <- normalizePath(file.path(dirname(file_arg), ".."), mustWork = TRUE)
REPO  <- normalizePath(file.path(TOOLS, ".."), mustWork = TRUE)
SRC   <- Sys.getenv("PMX_BATCH8_SOURCE_ROOT", REPO)
SRC   <- normalizePath(SRC, mustWork = TRUE)
cat(sprintf("batch8: copying reference/tool files from %s\n", SRC))

src_file <- function(rel) {
  p <- file.path(SRC, rel)
  if (file.exists(p)) p else file.path(REPO, rel)
}

.pass <- 0L; .fail <- 0L
ok <- function(cond, name) {
  cond <- tryCatch(isTRUE(cond), error = function(e) {
    cat(sprintf("      error: %s\n", conditionMessage(e))); FALSE })
  if (cond) { .pass <<- .pass + 1L; cat(sprintf("PASS  %s\n", name)) }
  else { .fail <<- .fail + 1L; cat(sprintf("FAIL  %s\n", name)) }
}
expect_stop <- function(expr, name, regexp = NULL) {
  e <- tryCatch({ force(expr); NULL }, error = function(e) e)
  hit <- !is.null(e) && (is.null(regexp) || grepl(regexp, conditionMessage(e)))
  if (!is.null(e) && !hit) cat(sprintf("      got: %s\n", conditionMessage(e)))
  if (is.null(e)) cat("      no error was raised\n")
  ok(hit, name)
}
expect_no_stop_matching <- function(expr, name, regexp) {
  e <- tryCatch({ force(expr); NULL }, error = function(e) e)
  bad <- !is.null(e) && grepl(regexp, conditionMessage(e))
  if (bad) cat(sprintf("      matched forbidden pattern: %s\n", conditionMessage(e)))
  ok(!bad, name)
}
quiet <- function(expr) suppressMessages(suppressWarnings(expr))

# --- fixture root ------------------------------------------------------------
root <- tempfile("batch8"); dir.create(root)
old_wd <- setwd(root); on.exit(setwd(old_wd), add = TRUE)
dir.create("tools")
for (f in c("method_manifest.R", "record_selection.R", "seed.R", "psn_invocation.R")) {
  s <- src_file(file.path("tools", f))
  if (file.exists(s)) file.copy(s, file.path("tools", f), overwrite = TRUE)
}
source("tools/method_manifest.R")
sha <- function(p) .mm_file_sha256(p)

install_skill <- function(skill, files, units, reviewed_by = "Fixture Reviewer",
                          reviewed_at = "2026-09-03") {
  d <- file.path("skills", skill, "reference")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  for (f in files) {
    s <- src_file(file.path("skills", skill, "reference", f))
    if (!file.exists(s)) return(NA_character_)
    file.copy(s, file.path(d, f), overwrite = TRUE)
  }
  lines <- c("# fixture", paste0("skill: ", skill),
             "generated_at: 2026-09-03T00:00:00Z",
             sprintf('reviewed_by: "%s"', reviewed_by),
             sprintf('reviewed_at: "%s"', reviewed_at), "units:")
  for (u in names(units)) {
    x <- units[[u]]
    ref <- file.path("skills", skill, "reference", x$ref)
    lines <- c(lines, sprintf("  %s:", u),
               sprintf("    status: %s", x$status),
               sprintf("    reference_path: %s", ref),
               sprintf("    entrypoints: [%s]", paste(x$entrypoints, collapse = ", ")),
               sprintf("    sha256: %s", sha(ref)),
               "    evidence: SKILL.md", '    reason: "fixture"')
    # A file the reference loads into its sealed validator object is a
    # dependency and is bound by its digest (validate_method_manifest refuses
    # a unit whose trusted loads are not all declared). Fixtures that install
    # such a reference declare them the same way write_method_manifest does.
    if (length(x$depends)) {
      lines <- c(lines, "    depends:")
      for (dp in x$depends)
        lines <- c(lines, sprintf("      - path: %s", dp),
                   sprintf("        sha256: %s", sha(dp)))
    }
  }
  p <- file.path("skills", skill, "MANIFEST.yaml"); writeLines(lines, p)
  p
}
load_ref <- function(skill, f) {
  e <- new.env(parent = globalenv())
  sys.source(file.path("skills", skill, "reference", f), envir = e)
  e
}
have_pkg <- function(p) requireNamespace(p, quietly = TRUE)

# ==== F9. diagnostics/vpc.R — data contract ================================
mf <- install_skill("diagnostics", "vpc.R", list(vpc = list(
  status = "approved", ref = "vpc.R",
  entrypoints = c("reference_vpc_prepare", "reference_vpc_attach_pred",
                  "reference_vpc_continuous", "reference_vpc_categorical",
                  "reference_vpc_bincheck", "reference_qpc"))))
ok(!is.na(mf), "F9: diagnostics/vpc.R present in source tree")
if (!is.na(mf) && have_pkg("rlang")) {
  vp <- load_ref("diagnostics", "vpc.R")

  # observation selection via harness_is_observation (EVID), NA excluded
  obs <- data.frame(ID = c(1, 1, 2), TIME = c(0, 1, 1),
                    DV = c(5, 6, 7), EVID = c(0, 0, 0))
  sim <- data.frame(ID = rep(c(1, 1, 2), 2), TIME = rep(c(0, 1, 1), 2),
                    DV = c(5, 6, 7, 5.1, 6.1, 7.1),
                    EVID = c(0, 0, 0, 0, 0, 0), REP = rep(1:2, each = 3))
  pr <- tryCatch(vp$reference_vpc_prepare(obs, sim, evid = "EVID"),
                 error = function(e) e)
  ok(is.list(pr) && !inherits(pr, "error") && nrow(pr$obs) == 3L && nrow(pr$sim) == 6L,
     "F9: reference_vpc_prepare keeps observation rows selected by EVID")

  # a dose row (EVID 1) and an NA-EVID row are both excluded (complete logical)
  obs2 <- rbind(obs,
                data.frame(ID = 2, TIME = 0, DV = 0, EVID = 1),      # dose
                data.frame(ID = 3, TIME = 2, DV = 9, EVID = NA))     # NA flag
  pr2 <- tryCatch(vp$reference_vpc_prepare(obs2, sim, evid = "EVID"),
                  error = function(e) e)
  ok(is.list(pr2) && !inherits(pr2, "error") && nrow(pr2$obs) == 3L &&
     !any(pr2$obs$EVID != 0),
     "F9: dose (EVID 1) and NA-EVID observed rows are excluded, not silently kept")

  # missing observation-flag column is a stop, never a silent no-op
  expect_stop(vp$reference_vpc_prepare(obs[c("ID", "TIME", "DV")], sim, evid = "EVID"),
              "F9: a missing EVID column stops (no silent MDV no-op)",
              regexp = "no 'EVID' column")

  # replicate completeness is validated when a replicate column is declared
  sim_ragged <- sim[-1, , drop = FALSE]   # replicate 1 now has 2 obs rows, not 3
  expect_stop(vp$reference_vpc_prepare(obs, sim_ragged, evid = "EVID"),
              "F9: ragged/incomplete replicates are a stop",
              regexp = "replicates are not complete")

  # attach_pred validates the ID/time key, not just the row count
  simp <- data.frame(ID = c(1, 1, 2), TIME = c(0, 1, 1), PRED = c(2, 3, 4), REP = 1)
  ok(identical(vp$reference_vpc_attach_pred(obs, simp)$PRED, c(2, 3, 4)),
     "F9: attach_pred attaches PRED when keys align")
  simp_wrongkey <- data.frame(ID = c(2, 2, 1), TIME = c(1, 0, 1),
                              PRED = c(2, 3, 4), REP = 1)   # same row count, wrong order
  expect_stop(vp$reference_vpc_attach_pred(obs, simp_wrongkey),
              "F9: attach_pred stops when keys diverge despite equal row counts",
              regexp = "does not match the observed records position for position")

  # binning method + nbins are declared, no silent ntile/9 default
  expect_stop(vp$apply_binning(NULL, list()),
              "F9: a missing binning method stops (no ntile default)",
              regexp = "method is a declared analysis-plan parameter")
  expect_stop(vp$apply_binning(NULL, list(method = "ntile")),
              "F9: a missing bin count stops (no default of 9)",
              regexp = "requires a declared bin count")

  # all-BLQ-bin policy is declared whenever censoring is active
  expect_stop(vp$reference_vpc_continuous(obs, sim, x = "TIME", yobs = "DV",
                ysim = "DV", repl = "REP", lloq = 5,
                binning_spec = list(method = "ntile", nbins = 3), seed = 1),
              "F9: censoring active + undeclared blq_bin_policy is a stop",
              regexp = "blq_bin_policy is a declared analysis-plan parameter")
  expect_no_stop_matching(vp$reference_vpc_continuous(obs, sim, x = "TIME",
                yobs = "DV", ysim = "DV", repl = "REP", lloq = 5,
                blq_bin_policy = "omit",
                binning_spec = list(method = "ntile", nbins = 3), seed = 1),
              "F9: a declared blq_bin_policy passes the policy guard",
              regexp = "blq_bin_policy is a declared")
}

# ==== F10. simulation-standards/uncertainty.R — estimate vs uncertainty ====
mf <- install_skill("simulation-standards", "uncertainty.R", list(uncertainty = list(
  status = "approved", ref = "uncertainty.R",
  entrypoints = c("reference_sim_uncertainty_draws", "reference_sim_install_check",
                  "reference_simulate_full", "reference_sim_full_summary"))))
ok(!is.na(mf), "F10: simulation-standards/uncertainty.R present in source tree")
if (!is.na(mf) && have_pkg("MASS")) {
  un <- load_ref("simulation-standards", "uncertainty.R")

  # DRAW 0 excluded from the uncertainty summary. Build a sim where, at one
  # time, DRAW 0 (estimate) sits AT the median of the uncertainty draws and,
  # at another time, far from it. Excluding DRAW 0 must leave the first
  # unchanged and change the second.
  mk <- function(draw, id, tm, cp) data.frame(SCENARIO = "s", DRAW = draw,
                                               ID = id, time = tm, CP = cp)
  # one subject per draw so IIV percentiles collapse to the value itself
  rows <- do.call(rbind, list(
    # time 1: draws 1,2,3 = 10,20,30 (median 20); DRAW 0 = 20 (equals median)
    mk(0, 1, 1, 20), mk(1, 1, 1, 10), mk(2, 1, 1, 20), mk(3, 1, 1, 30),
    # time 2: draws 1,2,3 = 10,20,30 (median 20); DRAW 0 = 200 (far off)
    mk(0, 1, 2, 200), mk(1, 1, 2, 10), mk(2, 1, 2, 20), mk(3, 1, 2, 30)))
  su <- tryCatch(un$reference_sim_full_summary(rows, observed = "CP",
                   iiv_probs = c(0.5, 0.9), unc_level = 0.5), error = function(e) e)
  ok(is.data.frame(su) && nrow(su) == 4L,
     "F10: full summary returns one row per scenario x time x percentile")
  if (is.data.frame(su)) {
    t1 <- su[su$time == 1 & su$iiv_pctile == 0.5, ]
    t2 <- su[su$time == 2 & su$iiv_pctile == 0.5, ]
    ok(isTRUE(all.equal(t1$estimate, 20)) && isTRUE(all.equal(t1$unc_median, 20)),
       "F10: estimate keeps DRAW 0; when DRAW 0 equals the draw median the summary is unchanged")
    ok(isTRUE(all.equal(t2$estimate, 200)) && isTRUE(all.equal(t2$unc_median, 20)),
       "F10: uncertainty median is over DRAW > 0 only (200 estimate does not pull the median off 20)")
  }
  # a summary with only DRAW 0 present is a stop (no uncertainty draws)
  only0 <- rbind(mk(0, 1, 1, 5), mk(0, 2, 1, 7))
  expect_stop(un$reference_sim_full_summary(only0, observed = "CP",
                iiv_probs = c(0.5, 0.9), unc_level = 0.5),
              "F10: no DRAW > 0 is a stop", regexp = "no DRAW > 0")

  # covariance draws: n_draws == 1 keeps the two-column data frame shape
  vcov.fb8fit <<- function(object, ...) object$vc
  fit_ok <- structure(list(theta = c(a = 1, b = 2),
                           vc = matrix(c(1, 0, 0, 1), 2,
                                       dimnames = list(c("a", "b"), c("a", "b")))),
                      class = "fb8fit")
  d1 <- tryCatch(quiet(un$reference_sim_uncertainty_draws(fit_ok, n_draws = 1,
                   seed = 1, source = "covariance")), error = function(e) e)
  ok(is.data.frame(d1) && nrow(d1) == 2L && ncol(d1) == 2L &&
     identical(names(d1), c("a", "b")),
     "F10: n_draws = 1 covariance path returns estimate + one draw, two columns (shape normalised)")

  # non-positive-semidefinite covariance is a stop before drawing
  fit_npsd <- fit_ok; fit_npsd$vc <- matrix(c(1, 2, 2, 1), 2,
                                            dimnames = list(c("a", "b"), c("a", "b")))
  expect_stop(quiet(un$reference_sim_uncertainty_draws(fit_npsd, n_draws = 5,
                seed = 1, source = "covariance")),
              "F10: a non-positive-semidefinite vcov is a stop",
              regexp = "positive semidefinite")
  fit_nonfin <- fit_ok; fit_nonfin$vc[1, 1] <- NA_real_
  expect_stop(quiet(un$reference_sim_uncertainty_draws(fit_nonfin, n_draws = 5,
                seed = 1, source = "covariance")),
              "F10: a non-finite vcov is a stop", regexp = "non-finite")

  # the install-check fail-closed guard: na.rm on the tolerance test is gone and
  # a non-finite comparison is a documented stop
  utxt <- paste(readLines(file.path("skills", "simulation-standards",
                                    "reference", "uncertainty.R"), warn = FALSE),
                collapse = "\n")
  ok(!grepl("any\\(rel > tolerance, na.rm = TRUE\\)", utxt) &&
     grepl("non-finite comparison value", utxt),
     "F10: install-check fails closed on non-finite (na.rm tolerance test removed)")
}

# ==== F11. covariate-priors draft units ====================================
mf <- install_skill("covariate-priors", "scm_config.R", list(scm_config = list(
  status = "approved", ref = "scm_config.R",
  entrypoints = c("reference_scm_config", "reference_scm_config_backward"))))
ok(!is.na(mf), "F11: covariate-priors/scm_config.R present in source tree")
# scm_config reads the declared house defaults from SKILL_SCM.md; supply it.
skf <- src_file("skills/covariate-priors/SKILL_SCM.md")
if (!is.na(mf) && file.exists(skf)) {
  file.copy(skf, file.path("skills", "covariate-priors", "SKILL_SCM.md"),
            overwrite = TRUE)
  sc <- load_ref("covariate-priors", "scm_config.R")

  ok(identical(sc$.scm_state_for("categorical", "none"), 1L) &&
     identical(sc$.scm_state_for("categorical", "linear"), 2L),
     "F11: categorical 'none' maps to state 1, 'linear' to state 2 (not both 2)")

  prior <- list(CL = data.frame(covariate = "WT", type = "continuous",
                                form = "power", stringsAsFactors = FALSE))
  # Integrity batch 10: both generators now require the control stream (PsN's
  # scm needs a TV<PARAM> line per searched parameter) and the site psn.conf
  # nm_version label. These assertions are about the p ceilings, so the
  # fixture simply satisfies the two new preconditions.
  ctl8 <- "f11-tv.ctl"
  writeLines(c("$PROBLEM F11", "$DATA d.csv", "$INPUT ID TIME DV WT", "$PK",
               "TVCL = EXP(THETA(1))", "CL = TVCL*EXP(ETA(1))",
               "$ERROR", "Y = F + F*EPS(1)"), ctl8)
  scmcfg <- function(...) sc$reference_scm_config("run1.mod", prior, "scmdir",
                                                  control_stream = ctl8,
                                                  nm_version = "nm751", ...)
  # p_forward at the house default and tighter are accepted
  cfg <- tryCatch(scmcfg(p_forward = 0.05), error = function(e) e)
  ok(is.character(cfg) && any(grepl("^p_forward=0.05$", cfg)),
     "F11: p_forward at the house default renders in C locale (p_forward=0.05)")
  ok(is.character(tryCatch(scmcfg(p_forward = 0.01), error = function(e) e)),
     "F11: a tighter p_forward (0.01) is accepted")
  # a looser p_forward is refused
  expect_stop(scmcfg(p_forward = 0.10),
              "F11: a looser p_forward (0.10 > 0.05) is a stop",
              regexp = "LOOSER than the house default")
  expect_stop(sc$reference_scm_config_backward("run1.mod", prior,
                list(CL = "WT-5"), "scmdir", control_stream = ctl8,
                nm_version = "nm751", p_backward = 0.05),
              "F11: a looser p_backward (0.05 > 0.01) is a stop",
              regexp = "LOOSER than the house default")
}

# psn_covsearch dedicated executor map.
# psn_covsearch.R loads the uncertainty lane and the scm config generator into
# its own sealed validator environment (batch 11 fix round 4), so both must be
# on disk BEFORE the manifest is written -- the manifest binds each of them by
# digest under depends:, exactly as write_method_manifest() derives it.
und <- src_file("skills/psn-uncertainty/reference/psn_uncertainty.R")
dir.create("skills/psn-uncertainty/reference", recursive = TRUE, showWarnings = FALSE)
if (file.exists(und)) file.copy(und, "skills/psn-uncertainty/reference/psn_uncertainty.R",
                                overwrite = TRUE)
mf2 <- install_skill("covariate-priors", c("scm_config.R", "psn_covsearch.R"),
  list(scm_config = list(status = "approved", ref = "scm_config.R",
                         entrypoints = c("reference_scm_config",
                                         "reference_scm_config_backward")),
       psn_covsearch = list(status = "approved", ref = "psn_covsearch.R",
         entrypoints = c("reference_psn_scmplus", "reference_psn_boot_scm",
                         "reference_psn_xv_scm", "reference_psn_lasso",
                         "reference_psn_frem"),
         depends = c("skills/psn-uncertainty/reference/psn_uncertainty.R",
                     "skills/covariate-priors/reference/scm_config.R"))))
ok(!is.na(mf2) && file.exists("skills/psn-uncertainty/reference/psn_uncertainty.R"),
   "F11: psn_covsearch.R present and uncertainty-lane dependency available")
if (!is.na(mf2) && file.exists("skills/psn-uncertainty/reference/psn_uncertainty.R")) {
  # a psn-uncertainty manifest so its bootstrap gate can resolve if reached
  install_skill("psn-uncertainty", "psn_uncertainty.R", list(
    bootstrap = list(status = "approved", ref = "psn_uncertainty.R",
                     entrypoints = c("reference_psn_bootstrap",
                                     "reference_psn_bootstrap_precheck"))),
    reviewed_by = "Fixture Reviewer")
  cv <- tryCatch(load_ref("covariate-priors", "psn_covsearch.R"),
                 error = function(e) e)
  ok(!inherits(cv, "error") && exists(".psn_covsearch_run", envir = cv),
     "F11: psn_covsearch.R defines its own .psn_covsearch_run executor")
  if (!inherits(cv, "error")) {
    ok(all(c("scmplus", "boot_scm", "xv_scm", "lasso", "frem") %in%
           names(cv$.PSN_COVSEARCH_TOOL_UNIT)),
       "F11: the covariate-search tool map covers scmplus/boot_scm/xv_scm/lasso/frem")
    wd <- file.path(root, "covwork"); dir.create(wd, showWarnings = FALSE)
    exec_ok <- file.path(root, "cov_exec.sh")
    writeLines(c("#!/bin/bash", 'echo "ran $2"', "exit 0"), exec_ok)
    Sys.chmod(exec_ok, "0755")
    r <- tryCatch(cv$.psn_covsearch_run(wd, "scmplus", character(0), exec_ok, root),
                  error = function(e) e)
    ok(!inherits(r, "error") && any(grepl("ran scmplus", r)),
       "F11: scmplus is ACCEPTED by the covariate-search executor (no 'unknown tool' rejection)")
    expect_stop(cv$.psn_covsearch_run(wd, "bootstrap", character(0), exec_ok, root),
                "F11: an uncertainty-only tool is rejected by the covariate-search map",
                regexp = "unknown PsN covariate-search tool")
  }
}

# ==== F12. guidance drift — arithmetic relocated to manifested draft units ==
# Independently constructed fixtures exercise boundary values and failure paths.
mfc <- install_skill("covariate-priors",
  c("covariate_effect.R", "nested_lrt.R"),
  list(covariate_effect = list(status = "approved", ref = "covariate_effect.R",
         entrypoints = "reference_covariate_exposure_ratio"),
       nested_lrt = list(status = "approved", ref = "nested_lrt.R",
         entrypoints = "reference_nested_lrt")))
ok(!is.na(mfc), "F12: covariate_effect.R and nested_lrt.R present in source tree")
if (!is.na(mfc)) {
  ce <- load_ref("covariate-priors", "covariate_effect.R")
  # A fourfold ratio gives hand-checkable power-law values and band edges.
  eff <- tryCatch(ce$reference_covariate_exposure_ratio(2, 8,
           exponents = c(0, 0.5, 1, 1.5),
           band_lower = 0.5, band_upper = 2), error = function(e) e)
  ok(is.data.frame(eff) &&
     isTRUE(all.equal(eff$r, c(4, 2, 1, 0.5),
                      tolerance = 1e-6)) &&
     identical(eff$within_band, c(FALSE, TRUE, TRUE, TRUE)),
     "F12a: exposure-ratio reference matches exact ratios and inclusive band boundaries")
  expect_stop(ce$reference_covariate_exposure_ratio(2, 8,
                exponents = 0.75), "F12a: band limits have no default",
              regexp = "band_lower and band_upper are declared")

  nl <- load_ref("covariate-priors", "nested_lrt.R")
  lrt <- tryCatch(nl$reference_nested_lrt(40, 35, criterion_dofv = 3.84,
                    df = 1L, alpha = 0.05), error = function(e) e)
  ok(is.list(lrt) && isTRUE(all.equal(lrt$dofv, 5)) &&
     isTRUE(lrt$significant) && isTRUE(lrt$retained),
     "F12c: nested-LRT reference returns the independently expected synthetic difference and decision")
  lrt_fail <- nl$reference_nested_lrt(NA_real_, 35, criterion_dofv = 3.84,
                                      df = 1L, alpha = 0.05)
  ok(is.na(lrt_fail$dofv) && !lrt_fail$significant && !lrt_fail$retained,
     "F12c: a failed parent (NA OFV) yields NA dofv and retained = FALSE (fail closed)")
  expect_stop(nl$reference_nested_lrt(40, 35, df = 1L, alpha = 0.05),
              "F12c: criterion_dofv has no default", regexp = "criterion_dofv is a declared")
}

mfw <- install_skill("data-assembly", "weight_constants.R", list(
  weight_constants = list(status = "approved", ref = "weight_constants.R",
                          entrypoints = "reference_weight_constants")))
ok(!is.na(mfw), "F12: data-assembly/weight_constants.R present in source tree")
if (!is.na(mfw)) {
  wc <- load_ref("data-assembly", "weight_constants.R")
  set.seed(1); v <- runif(16, 0.5, 4)
  r <- tryCatch(wc$reference_weight_constants(v, probs = c(0.05, 0.95),
                  quantile_type = 7), error = function(e) e)
  # Independent base-R calls specify the expected quantiles.
  ok(is.list(r) && identical(r$median, stats::median(v)) &&
     identical(unname(r$lower),
               unname(stats::quantile(v, 0.05, type = 7, names = FALSE))) &&
     identical(unname(r$upper),
               unname(stats::quantile(v, 0.95, type = 7, names = FALSE))),
     "F12d: weight-constants reference is stats::median + stats::quantile(type) (same arithmetic)")
  expect_stop(wc$reference_weight_constants(v, probs = c(0.05, 0.95)),
              "F12d: quantile_type has no default", regexp = "quantile_type is a declared")
}

# ==== F15. one seed helper: pinned RNG kinds =================================
sf <- src_file("tools/seed.R")
ok(file.exists("tools/seed.R"), "F15: tools/seed.R present in the fixture")
if (file.exists("tools/seed.R")) {
  se <- new.env(parent = globalenv()); sys.source("tools/seed.R", envir = se)
  rec <- se$harness_seed(20260828)
  ok(inherits(rec, "harness_seed") &&
     identical(rec$kind, "Mersenne-Twister") &&
     identical(rec$normal.kind, "Inversion") &&
     identical(rec$sample.kind, "Rejection"),
     "F15: harness_seed records the pinned RNG kinds for the gate package")
  # behaviour-preserving on this R: harness_seed(s) draws the same stream as
  # the bare set.seed(s) the references used before (current R defaults).
  se$harness_seed(20260828); a <- runif(5); b_sample <- sample(1:100, 5)
  set.seed(20260828); b <- runif(5); a_sample <- sample(1:100, 5)
  ok(identical(a, b) && identical(a_sample, b_sample),
     "F15: harness_seed reproduces the bare set.seed stream (no stored number changes)")
  # the kinds are actually pinned even from a non-default session
  RNGkind("Wichmann-Hill")
  se$harness_seed(1)
  ok(identical(RNGkind()[1], "Mersenne-Twister"),
     "F15: harness_seed pins the generator even when the session set another kind")
  # restore returns the prior state
  before <- RNGkind()
  r2 <- se$harness_seed(7); se$harness_seed_restore(r2)
  ok(identical(RNGkind(), before),
     "F15: harness_seed_restore restores the prior RNG kinds")
  # every seeding reference now routes through harness_seed, not bare set.seed
  refs <- c("skills/diagnostics/reference/vpc.R",
            "skills/diagnostics/reference/npde.R",
            "skills/diagnostics/reference/bootstrap.R",
            "skills/forest-plots/reference/forest.R",
            "skills/simulation-standards/reference/mrgsolve_sim.R",
            "skills/simulation-standards/reference/uncertainty.R")
  bare <- character(0); routed <- character(0)
  for (rf in refs) {
    p <- src_file(rf)
    if (!file.exists(p)) next
    txt <- readLines(p, warn = FALSE)
    if (any(grepl("^\\s*set\\.seed\\(seed\\)", txt))) bare <- c(bare, basename(rf))
    if (any(grepl("harness_seed\\(seed\\)", txt))) routed <- c(routed, basename(rf))
  }
  ok(length(bare) == 0L && length(routed) == 6L,
     "F15: all six seeding references route through harness_seed (no bare set.seed(seed) remains)")
}

# ==== B0u-d A3 item 9. the per-sample entry point of the bootstrap unit =====
# Plan revision 1.4 Section 5.3, part 2, item 9; erratum P-1.4-1. The public
# entry point wraps the private .psn_parse_raw_results unchanged, returns the
# raw minimization_successful column and the PsN flag columns beside it, and
# refuses a file whose `model` column does not mark row 1 as the original fit
# (marker 0) and every other row as a sample. Synthetic fixture file only.
set_unit_status <- function(skill, unit, status) {
  p <- file.path("skills", skill, "MANIFEST.yaml")
  l <- readLines(p); i <- which(l == sprintf("  %s:", unit))[1]
  l[i + 1L] <- sprintf("    status: %s", status); writeLines(l, p)
}
psn_ref <- "skills/psn-uncertainty/reference/psn_uncertainty.R"
if (file.exists(und)) file.copy(und, psn_ref, overwrite = TRUE)
mfp <- if (file.exists(psn_ref))
  install_skill("psn-uncertainty", "psn_uncertainty.R", list(
    bootstrap = list(status = "approved", ref = "psn_uncertainty.R",
                     entrypoints = c("reference_psn_bootstrap",
                                     "reference_psn_bootstrap_precheck",
                                     "reference_psn_bootstrap_samples"),
                     depends = "tools/psn_invocation.R"))) else NA_character_
ok(!is.na(mfp), "A3 item 9: psn_uncertainty.R installs with the per-sample entry point declared")
if (!is.na(mfp)) {
  pu <- load_ref("psn-uncertainty", "psn_uncertainty.R")
  ok(exists("reference_psn_bootstrap_samples", envir = pu, inherits = FALSE),
     "A3 item 9: reference_psn_bootstrap_samples is defined")
  hdr <- c("model", "problem", "subproblem", "covariance_step_run",
           "minimization_successful", "covariance_step_successful",
           "covariance_step_warnings", "estimate_near_boundary", "rounding_errors",
           "zero_gradients", "final_zero_gradients", "hessian_reset",
           "s_matrix_singular", "significant_digits", "condition_number", "ofv",
           "THETA(1) tcl: log CL, fixture", "OMEGA(1,1) IIV on CL, fixture")
  raw_row <- function(model, ms, theta, omega, rounding = 0)
    paste(c(model, 1, 0, 1, ms, 1, 0, 0, rounding, 0, 0, 0, 0, 3.2, 12.5, 100,
            theta, omega), collapse = ",")
  write_raw <- function(path, rows) {
    writeLines(c(paste0('"', hdr, '"', collapse = ","), rows), path)
    path
  }
  good <- write_raw(file.path(root, "raw_good.csv"), c(
    raw_row(0, 1, -5.1, 0.03), raw_row(1, 1, -5.0, 0.02), raw_row(2, 0, -4.9, 0.05, 1),
    raw_row(3, 1, -5.2, 0.04)))
  pat <- c(THETA1 = "^THETA\\(1\\)", OMEGA11 = "^OMEGA\\(1,1\\)")
  w <- tryCatch(pu$reference_psn_bootstrap_samples(good, pat), error = function(e) e)
  h <- pu$.psn_parse_raw_results(good, pat)
  if (inherits(w, "error")) cat("      got: ", conditionMessage(w), "\n")
  ok(!inherits(w, "error") && identical(w$estimates, h$estimates) &&
     identical(w$success, h$success) && identical(w$n_samples, h$n_samples),
     "A3 item 9: estimates, success and n_samples are the private helper's, unchanged")
  ok(!inherits(w, "error") && identical(w$minimization_successful, c(1L, 0L, 1L)),
     "A3 item 9 / P-1.4-1: the raw minimization_successful column is returned beside success")
  flag_cols <- c("covariance_step_run", "covariance_step_successful",
                 "covariance_step_warnings", "condition_number",
                 "estimate_near_boundary", "rounding_errors", "zero_gradients",
                 "final_zero_gradients", "hessian_reset", "s_matrix_singular")
  ok(!inherits(w, "error") && identical(names(w$flags), flag_cols) &&
     nrow(w$flags) == 3L && identical(w$flags$rounding_errors, c(0L, 1L, 0L)),
     "A3 item 9: the flag columns are returned for the sample rows only")
  bad_first <- write_raw(file.path(root, "raw_bad_first.csv"), c(
    raw_row(1, 1, -5.1, 0.03), raw_row(2, 1, -5.0, 0.02)))
  expect_stop(pu$reference_psn_bootstrap_samples(bad_first, pat),
              "A3 item 9: a first data row without the original-fit marker is a stop",
              "original fit")
  bad_sample <- write_raw(file.path(root, "raw_bad_sample.csv"), c(
    raw_row(0, 1, -5.1, 0.03), raw_row(0, 1, -5.0, 0.02)))
  expect_stop(pu$reference_psn_bootstrap_samples(bad_sample, pat),
              "A3 item 9: a sample row carrying the original-fit marker is a stop", "sample")
  no_flag <- file.path(root, "raw_no_flag.csv")
  writeLines(c(paste0('"', setdiff(hdr, "hessian_reset"), '"', collapse = ","),
               paste(c(0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 3.2, 12.5, 100, -5.1, 0.03),
                     collapse = ",")), no_flag)
  expect_stop(pu$reference_psn_bootstrap_samples(no_flag, pat),
              "A3 item 9: a file without a declared flag column is a stop", "hessian_reset")
  miss <- write_raw(file.path(root, "raw_missing_flag.csv"), c(
    raw_row(0, 1, -5.1, 0.03), raw_row(1, "", -5.0, 0.02), raw_row(2, 1, -4.9, 0.05)))
  wm <- tryCatch(pu$reference_psn_bootstrap_samples(miss, pat), error = function(e) e)
  ok(!inherits(wm, "error") && is.na(wm$minimization_successful[1]) && is.na(wm$success[1]),
     "A3 item 9: a missing success flag is returned as NA (unchanged helper behaviour) for item 2 to refuse")
  set_unit_status("psn-uncertainty", "bootstrap", "draft")
  expect_stop(pu$reference_psn_bootstrap_samples(good, pat),
              "A3 item 9: the entry point refuses while the bootstrap unit is draft",
              "not SME-validated")
}

cat(sprintf("\nintegrity batch 8 battery: %d passed, %d failed\n", .pass, .fail))
quit(status = if (.fail > 0L) 1L else 0L)

}  # end .run_batch8_tests

if (.invoked_as_script()) .run_batch8_tests()
