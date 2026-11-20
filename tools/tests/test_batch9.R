#!/usr/bin/env Rscript
# Integrity batch 9 test battery (Codex round 9 findings P1-3 through P2-9).
#
# Hermetic, same pattern as tools/tests/test_batch8.R: every fixture lives
# under tempdir() laid out as <root>/skills/<skill>/reference/<file>.R +
# MANIFEST.yaml, with copies of the tools/ files a reference sources, and the
# working directory is moved there so repo-relative paths resolve exactly as
# they do in the repository. No live manifest, gate record, reference file,
# store or pilot output is touched.
#
# Source of the copied files: the repository by default; the environment
# variable PMX_BATCH9_SOURCE_ROOT overrides it (a HEAD export or a staged
# patch tree laid out like the repository), which is how the red (pre-fix)
# and green (post-fix) runs are produced. Every run states its source.
#
# Run with:  Rscript tools/tests/test_batch9.R
# Exits non-zero if any assertion fails.
#
# SAFE TO SOURCE: guarded behind .invoked_as_script() like the other
# batteries, so tar_source("tools") only defines and runs nothing.
.invoked_as_script <- function() {
  fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  length(fa) > 0 && grepl("test_batch9\\.R$", sub("^--file=", "", fa[1]))
}

.run_batch9_tests <- function() {

args <- commandArgs(FALSE)
file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
TOOLS <- normalizePath(file.path(dirname(file_arg), ".."), mustWork = TRUE)
REPO  <- normalizePath(file.path(TOOLS, ".."), mustWork = TRUE)
SRC   <- Sys.getenv("PMX_BATCH9_SOURCE_ROOT", REPO)
SRC   <- normalizePath(SRC, mustWork = TRUE)
cat(sprintf("batch9: copying reference/tool files from %s\n", SRC))

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
root <- tempfile("batch9"); dir.create(root)
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
    # A reference's trusted loads are dependencies bound by digest (as in
    # test_batch8.R); B0u-d's npde_panels.R loads npde.R that way.
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


# ==== P1-3. psn-simeval — exactly one bin rule, exact artifact basename =====
mf <- install_skill("psn-simeval", "psn_simeval.R", list(
  vpc = list(status = "approved", ref = "psn_simeval.R",
             entrypoints = "reference_psn_vpc")))
ok(!is.na(mf), "P1-3: psn-simeval/psn_simeval.R present in source tree")
if (!is.na(mf)) {
  ps <- load_ref("psn-simeval", "psn_simeval.R")

  # The contract is EXACTLY one rule. Zero used to pass, which handed the
  # binning decision to the engine default -- an undeclared method.
  expect_stop(ps$.psn_check_bin_rule(list()),
              "P1-3: an empty bin_rule declares no method and is refused",
              "exactly one")
  expect_stop(ps$.psn_check_bin_rule(list(bin_by_count = FALSE)),
              "P1-3: bin_by_count = FALSE selects no rule and is refused",
              "exactly one")
  expect_stop(ps$.psn_check_bin_rule(list(no_of_bins = 9, bin_by_count = TRUE)),
              "P1-3: two rules are refused (unchanged)", "exactly one")
  ok(is.null(ps$.psn_check_bin_rule(list(no_of_bins = 9))),
     "P1-3: exactly one rule passes")
  # Integrity batch 11 (SME decision 2, 2026-09-04). This assertion used to
  # bless NULL, which meant "no declared rule" and handed the binning to the
  # engine default -- exactly the defect P1-3 was raised about. bin_rule is now
  # REQUIRED and NULL is refused by argument name; the house default METHOD is
  # PsN's own auto_bin with the value declared explicitly.
  expect_stop(ps$.psn_check_bin_rule(NULL),
              "P1-4: NULL bin_rule is refused, naming the argument",
              "bin_rule is a required")
  ok(is.null(ps$.psn_check_bin_rule(list(auto_bin = "auto"))),
     "P1-4: auto_bin is a recognised, declared rule")

  # The artifact lookup built a regex out of the filename, so the "." in
  # vpc_results.csv matched any character and a neighbouring file could be
  # read as this run's decisive artifact.
  rd <- file.path(tempdir(), paste0("psnrun-", as.integer(Sys.time())))
  dir.create(rd, recursive = TRUE, showWarnings = FALSE)
  writeLines("x", file.path(rd, "vpc_resultsXcsv"))
  run <- list(path = rd, since = Sys.time() - 3600)
  got <- tryCatch(ps$.psn_find_artifact(run, "vpc_results.csv", required = FALSE),
                  error = function(e) e)
  ok(!inherits(got, "error") && length(got) == 1L && is.na(got),
     "P1-3: a metacharacter near-match is not accepted as the artifact")
  writeLines("y", file.path(rd, "vpc_results.csv"))
  got2 <- tryCatch(ps$.psn_find_artifact(run, "vpc_results.csv", required = TRUE),
                   error = function(e) e)
  ok(!inherits(got2, "error") && basename(got2) == "vpc_results.csv",
     "P1-3: the exact basename is still found")
  unlink(rd, recursive = TRUE)
}

# ==== P1-4. diagnostics/vpc.R — no default binning, BLQ refusal ============
mf <- install_skill("diagnostics", "vpc.R", list(vpc = list(
  status = "approved", ref = "vpc.R",
  entrypoints = c("reference_vpc_prepare", "reference_vpc_attach_pred",
                  "reference_vpc_continuous", "reference_vpc_categorical",
                  "reference_vpc_bincheck", "reference_qpc"))))
ok(!is.na(mf), "P1-4: diagnostics/vpc.R present in source tree")
if (!is.na(mf)) {
  vp <- load_ref("diagnostics", "vpc.R")
  for (fn in c("reference_vpc_continuous", "reference_vpc_categorical")) {
    fml <- formals(vp[[fn]])
    ok("binning_spec" %in% names(fml) &&
       identical(fml$binning_spec, quote(expr = )),
       sprintf("P1-4: %s has no default binning_spec", fn))
  }
  # A missing spec is a refusal that names the parameter.
  expect_stop(vp$.vpc_check_binning_spec(NULL),
              "P1-4: an absent binning spec is refused by name", "binning_spec")
  ok(is.null(vp$.vpc_check_binning_spec(list(method = "ntile", nbins = 9))),
     "P1-4: an explicit binning spec still passes")
  # BLQ semantics for keep/omit are not SME-specified, so censoring refuses
  # rather than accepting an inert policy.
  blq <- data.frame(ID = 1, TIME = 1, DV = 0.1, BLQ = 1)
  for (pol in c("keep", "omit")) {
    expect_stop(vp$.vpc_check_blq_data(blq, "BLQ", pol),
                sprintf("P1-4: BLQ-flagged data refuse censoring under '%s'", pol),
                "not yet SME-specified")
  }
  none <- data.frame(ID = 1, TIME = 1, DV = 5, BLQ = 0)
  ok(is.null(vp$.vpc_check_blq_data(none, "BLQ", "keep")),
     "P1-4: data with no BLQ-flagged record are unaffected")
  ok(is.null(vp$.vpc_check_blq_data(none, NULL, "keep")),
     "P1-4: no declared BLQ column is unaffected")

  # Review S4: a factor/character/logical flag column must be read as a flag,
  # not coerced through its level codes.
  f0 <- data.frame(ID = 1:2, TIME = 1, DV = c(5, 6),
                   BLQ = factor(c("0", "0"), levels = c("0", "1")))
  ok(is.null(vp$.vpc_check_blq_data(f0, "BLQ", "keep")),
     "P1-4/S4: an all-zero FACTOR BLQ column is not mistaken for flagged data")
  f1 <- data.frame(ID = 1:2, TIME = 1, DV = c(5, 6),
                   BLQ = factor(c("0", "1"), levels = c("0", "1")))
  expect_stop(vp$.vpc_check_blq_data(f1, "BLQ", "keep"),
              "P1-4/S4: a factor BLQ column with a 1 is refused",
              "not yet SME-specified")
  l0 <- data.frame(ID = 1:2, TIME = 1, DV = c(5, 6), BLQ = c(FALSE, FALSE))
  ok(is.null(vp$.vpc_check_blq_data(l0, "BLQ", "keep")),
     "P1-4/S4: an all-FALSE logical BLQ column is unaffected")
  l1 <- data.frame(ID = 1:2, TIME = 1, DV = c(5, 6), BLQ = c(FALSE, TRUE))
  expect_stop(vp$.vpc_check_blq_data(l1, "BLQ", "keep"),
              "P1-4/S4: a logical BLQ column with a TRUE is refused",
              "not yet SME-specified")
  bx <- data.frame(ID = 1:2, TIME = 1, DV = c(5, 6), BLQ = c("0", "maybe"))
  expect_stop(vp$.vpc_check_blq_data(bx, "BLQ", "keep"),
              "P1-4/S4: a BLQ column carrying a non-flag value is refused",
              "not a flag")

  # Review S4: the lloq-only censoring path. With no BLQ column declared, an
  # observation below lloq is still a below-quantification-limit observation.
  lq <- data.frame(ID = 1:2, TIME = 1, DV = c(5, 0.1))
  expect_stop(vp$.vpc_check_blq_data(lq, NULL, "keep", yobs = "DV", lloq = 1),
              "P1-4/S4: an observation below lloq refuses censoring even with no BLQ column",
              "below lloq")
  ok(is.null(vp$.vpc_check_blq_data(lq, NULL, "keep", yobs = "DV", lloq = 0.01)),
     "P1-4/S4: no observation below lloq leaves censoring unaffected")
}

# ==== P1-5. simulation-standards/uncertainty.R — DRAW 0 by label ===========
mf <- install_skill("simulation-standards", "uncertainty.R", list(
  uncertainty = list(status = "approved", ref = "uncertainty.R",
                     entrypoints = "reference_sim_full_summary")))
ok(!is.na(mf), "P1-5: simulation-standards/uncertainty.R present in source tree")
if (!is.na(mf)) {
  un <- load_ref("simulation-standards", "uncertainty.R")
  mk <- function(draws, vals) data.frame(
    SCENARIO = "s", DRAW = rep(draws, each = 3), ID = rep(1:3, length(draws)),
    time = 1, CP = as.numeric(rbind(vals, vals, vals)))
  # Two percentiles: with a single percentile vapply() returns a vector rather
  # than the percentile x draw matrix this function has always indexed, which
  # is a pre-existing limitation of the reference and outside this batch.
  smry <- function(d) un$reference_sim_full_summary(d, observed = "CP",
                                                    iiv_probs = c(0.25, 0.5))
  s <- tryCatch(smry(mk(c(0, 1, 2, 3), c(10, 11, 9, 12))), error = function(e) e)
  ok(!inherits(s, "error") && isTRUE(all.equal(s$estimate[1], 10)),
     "P1-5: DRAW 0 is the point estimate when it is present")
  # Rows out of order: the positional rule read whichever group split() put
  # first, which is only DRAW 0 by luck of a numeric column.
  shuf <- mk(c(3, 1, 0, 2), c(12, 11, 10, 9))
  s2 <- tryCatch(smry(shuf), error = function(e) e)
  ok(!inherits(s2, "error") && isTRUE(all.equal(s2$estimate[1], 10)),
     "P1-5: the point estimate follows the DRAW 0 label, not group order")
  expect_stop(smry(mk(c(1, 2, 3), c(11, 9, 12))),
              "P1-5: a scenario/time without DRAW 0 is refused", "DRAW 0")
  expect_stop(smry(mk(c(0), c(10))),
              "P1-5: DRAW 0 with no uncertainty draw is refused",
              "no DRAW > 0")
  # Review S5: a non-finite uncertainty draw is a refusal naming it, never a
  # silent subset — dropping it would change what a passing computation returns.
  inf1 <- mk(c(0, 1, 2), c(10, 11, 12)); inf1$CP[inf1$DRAW == 1] <- Inf
  expect_stop(smry(inf1),
              "P1-5/S5: a non-finite DRAW > 0 quantile is refused, not dropped",
              "non-finite DRAW > 0")
  # An NA draw is refused too; quantile() refuses it before the summary does,
  # so only the refusal itself is pinned.
  nonfin <- mk(c(0, 1), c(10, 11)); nonfin$CP[nonfin$DRAW == 1] <- NA_real_
  expect_stop(smry(nonfin), "P1-5: an NA uncertainty draw is refused")
}

# ==== P1-6. NPDE global tests and VPC coverage as manifested units =========
# Both computations drove the approved qualification verdict's min_p and
# min_cov from unsigned pilot code. They are relocated verbatim into gated
# diagnostics reference units; the pilot only forwards.
for (spec in list(
      list(file = "npde_tests.R", unit = "npde_tests",
           ep = "reference_npde_global_tests"),
      list(file = "vpc_coverage.R", unit = "vpc_coverage",
           ep = "reference_vpc_coverage"))) {
  ok(file.exists(src_file(file.path("skills", "diagnostics", "reference", spec$file))),
     sprintf("P1-6: skills/diagnostics/reference/%s exists", spec$file))
}
mf <- install_skill("diagnostics", c("npde_tests.R", "vpc_coverage.R"), list(
  npde_tests = list(status = "approved", ref = "npde_tests.R",
                    entrypoints = "reference_npde_global_tests"),
  vpc_coverage = list(status = "approved", ref = "vpc_coverage.R",
                      entrypoints = "reference_vpc_coverage")))
ok(!is.na(mf), "P1-6: both new diagnostics references install as manifest units")
if (!is.na(mf)) {
  nt <- load_ref("diagnostics", "npde_tests.R")
  vc <- load_ref("diagnostics", "vpc_coverage.R")

  # Preserve the relocated arithmetic; p-values retain machine precision for
  # downstream threshold decisions (PM-1) and are rounded only for display.
  set.seed(11); vals <- stats::rnorm(60)
  ref <- local({
    n <- length(vals)
    tt <- stats::t.test(vals, mu = 0)
    x2 <- (n - 1) * stats::var(vals)
    p_var <- min(1, 2 * min(stats::pchisq(x2, df = n - 1),
                            stats::pchisq(x2, df = n - 1, lower.tail = FALSE)))
    data.frame(
      test = c("global mean = 0 (one-sample t)",
               "global variance = 1 (two-sided chi-squared)"),
      n = n,
      observed = signif(c(mean(vals), stats::var(vals)), 4),
      statistic = signif(c(unname(tt$statistic), x2), 4),
      p_value = c(tt$p.value, p_var),
      flag_threshold_p = 0.01,
      flagged = c(tt$p.value, p_var) < 0.01,
      stringsAsFactors = FALSE)
  })
  got <- tryCatch(nt$reference_npde_global_tests(vals, p_flag = 0.01),
                  error = function(e) e)
  ok(!inherits(got, "error") && isTRUE(all.equal(got, ref)),
     "P1-6: NPDE global tests preserve relocated arithmetic and full p-value precision")
  expect_stop(nt$reference_npde_global_tests(vals),
              "P1-6: p_flag is a declared parameter with no default", "p_flag")
  expect_stop(nt$reference_npde_global_tests(c(1, NA), p_flag = 0.01),
              "P1-6: a non-finite NPDE value is refused", "finite")
  expect_stop(nt$reference_npde_global_tests(1, p_flag = 0.01),
              "P1-6: a single NPDE value cannot support the tests", "at least")

  st <- data.frame(qname = rep(c("q5", "q50"), each = 4),
                   y  = c(1, 2, 3, 9, 1, 2, 3, 4),
                   lo = rep(0, 8), hi = rep(5, 8), stringsAsFactors = FALSE)
  cov_ref <- local({
    out <- do.call(rbind, lapply(split(st, st[, "qname", drop = FALSE], drop = TRUE),
      function(d) {
        row <- data.frame(percentile = as.character(d$qname[1]), n_bins = nrow(d),
                          n_within_band = sum(d$y >= d$lo & d$y <= d$hi),
                          n_below_band = sum(d$y < d$lo),
                          n_above_band = sum(d$y > d$hi), stringsAsFactors = FALSE)
        row$pct_within <- round(100 * row$n_within_band / row$n_bins, 1)
        row
      }))
    rownames(out) <- NULL; out
  })
  gotc <- tryCatch(vc$reference_vpc_coverage(st), error = function(e) e)
  ok(!inherits(gotc, "error") && isTRUE(all.equal(gotc, cov_ref)),
     "P1-6: reference_vpc_coverage reproduces the relocated bookkeeping exactly")
  sts <- cbind(st, stratum_col = rep(c("a", "b"), 4))
  gs <- tryCatch(vc$reference_vpc_coverage(sts, strata = "stratum_col"),
                 error = function(e) e)
  ok(!inherits(gs, "error") && "stratum" %in% names(gs) && nrow(gs) == 4L,
     "P1-6: the stratified form still returns one row per stratum x percentile")
  expect_stop(vc$reference_vpc_coverage(st[, c("qname", "y")]),
              "P1-6: a stats table without the band columns is refused", "lo")
}

# ==== P2-7. covariate-priors/scm_config.R — prior 'none' and reserved keys ==
mf <- install_skill("covariate-priors", "scm_config.R", list(
  scm_config = list(status = "approved", ref = "scm_config.R",
                    entrypoints = "reference_scm_config")))
ok(!is.na(mf), "P2-7: covariate-priors/scm_config.R present in source tree")
if (!is.na(mf)) {
  sc <- load_ref("covariate-priors", "scm_config.R")
  expect_stop(sc$.scm_check_extra_options(list(p_forward = "0.01")),
              "P2-7: a reserved key in extra_options is refused", "reserved")
  expect_stop(sc$.scm_check_extra_options(list(search_direction = "forward")),
              "P2-7: search_direction in extra_options is refused", "reserved")
  ok(is.null(sc$.scm_check_extra_options(list(threads = "4"))),
     "P2-7: a non-reserved extra option still passes")
  # Review S3: whitespace around and inside the key used to defeat the check.
  expect_stop(sc$.scm_check_extra_options(" search_direction = both "),
              "P2-7/S3: a reserved key written with spaces around '=' is refused",
              "reserved")
  expect_stop(sc$.scm_check_extra_options("p_forward\t=\t0.01"),
              "P2-7/S3: a reserved key separated by tabs is refused", "reserved")
  ok(is.null(sc$.scm_check_extra_options(" only_successful = 1 ")),
     "P2-7/S3: a spaced non-reserved option still passes")

  # A prior form of 'none' EXCLUDES the pair from [test_relations] (SME
  # decision 2026-09-04); it is not emitted with state 1.
  # The generator reads its p ceiling from the signed SKILL file, so the
  # fixture carries a copy of it.
  sk <- src_file("skills/covariate-priors/SKILL_SCM.md")
  if (file.exists(sk)) file.copy(sk, "skills/covariate-priors/SKILL_SCM.md",
                                 overwrite = TRUE)
  priors <- list(CL = data.frame(covariate = c("WT", "SEX"),
                                 type = c("continuous", "categorical"),
                                 form = c("power", "none"),
                                 stringsAsFactors = FALSE),
                 V  = data.frame(covariate = "WT", type = "continuous",
                                 form = "none", stringsAsFactors = FALSE))
  # Integrity batch 10: the generator now requires the control stream (PsN's
  # scm needs a TV<PARAM> line per searched parameter) and the site psn.conf
  # nm_version label. These assertions are about the 'none' exclusion, so the
  # fixture satisfies the two new preconditions for the searched parameter CL.
  ctl9 <- "p27-tv.ctl"
  writeLines(c("$PROBLEM P2-7", "$DATA d.csv", "$INPUT ID TIME DV WT SEX", "$PK",
               "TVCL = EXP(THETA(1))", "CL = TVCL*EXP(ETA(1))",
               "TVV = EXP(THETA(2))", "V = TVV*EXP(ETA(2))",
               "$ERROR", "Y = F + F*EPS(1)"), ctl9)
  cfg <- tryCatch(sc$reference_scm_config(prior_list = priors, model = "run101.mod",
                                          directory = "scm1",
                                          control_stream = ctl9,
                                          nm_version = "nm751"),
                  error = function(e) e)
  if (inherits(cfg, "error")) cat("      got: ", conditionMessage(cfg), "\n")
  ok(!inherits(cfg, "error"), "P2-7: the generator runs on a mixed prior list")
  if (!inherits(cfg, "error")) {
    tr <- cfg[(which(cfg == "[test_relations]") + 1L):length(cfg)]
    tr <- tr[seq_len(which(tr == "")[1] - 1L)]
    ok(any(grepl("^CL=", tr)) && !any(grepl("SEX", tr)),
       "P2-7: a categorical pair declared 'none' is excluded from [test_relations]")
    ok(!any(grepl("^V=", tr)),
       "P2-7: a parameter whose only pair is 'none' emits no test_relations line")
    ok(!any(grepl("categorical_covariates", cfg)),
       "P2-7: an excluded categorical is not declared as a searched covariate")
    ok(any(grepl("^continuous_covariates=WT$", cfg)),
       "P2-7: a tested continuous covariate is still declared")
  }
}

# ==== B0u-d. diagnostics units and the verdict interface ==========
# Plan revision 1.4 Section 5.3 (B0u-d parts 1 and 2), MAP amendments A3 and
# A3.1. Every input below is a declared synthetic fixture: no value comes from
# an analysis. Oracles are written here independently of the entry points
# (explicit formulas, never a call to the entry point under test). The
# section keeps its own pass/fail count so that its summary line is the
# deciding line of plan check V-q3.
.b0ud_fail_before <- .fail; .b0ud_pass_before <- .pass
for (f in c("vpc_strata.R", "bootstrap_agreement.R", "npde_panels.R"))
  ok(file.exists(src_file(file.path("skills", "diagnostics", "reference", f))),
     sprintf("B0u-d: skills/diagnostics/reference/%s exists", f))
BA_EPS <- c("reference_bootstrap_pct_diff", "reference_bootstrap_replicate_accounting",
            "reference_bootstrap_unsuccessful_counts", "reference_bootstrap_category_table",
            "reference_bootstrap_iiv_rse", "reference_bootstrap_fixed_rse",
            "reference_bootstrap_ci_finite", "reference_bootstrap_summaries",
            "reference_bootstrap_back_transform")
QV_EPS <- c("reference_qualification_verdict", "reference_qualification_verdict_covstep")
b0ud_units <- list(
  npde = list(status = "approved", ref = "npde.R",
              entrypoints = c("reference_npde", "reference_npde_minimum_nsim")),
  vpc_strata = list(status = "approved", ref = "vpc_strata.R",
                    entrypoints = "reference_weight_bands"),
  bootstrap_agreement = list(status = "approved", ref = "bootstrap_agreement.R",
                             entrypoints = BA_EPS),
  npde_panels = list(status = "approved", ref = "npde_panels.R",
                     entrypoints = "reference_npde_panels",
                     depends = "skills/diagnostics/reference/npde.R"),
  qualification_verdict = list(status = "approved", ref = "qualification_verdict.R",
                               entrypoints = QV_EPS))
b0ud_files <- c("npde.R", "vpc_strata.R", "bootstrap_agreement.R", "npde_panels.R",
                "qualification_verdict.R")
mf <- install_skill("diagnostics", b0ud_files, b0ud_units)
ok(!is.na(mf), "B0u-d: the five diagnostics references install as manifest units")
if (!is.na(mf)) {
  ok(isTRUE(tryCatch({ validate_method_manifest(mf); TRUE },
                     error = function(e) { cat("      got: ", conditionMessage(e), "\n"); FALSE })),
     "B0u-d: the fixture manifest with the three new units validates (depends declared)")

  # ---- vpc_strata (A2 item 3): the body of qual_wt_bands, unchanged -------
  vs <- load_ref("diagnostics", "vpc_strata.R")
  fd <- data.frame(ID = rep(1:9, each = 2),
                   WT = as.vector(rbind(10:18, rep(999, 9))))
  got_wb <- tryCatch(vs$reference_weight_bands(fd), error = function(e) e)
  labels <- c("WT <= 12.67 kg", "12.67 < WT <= 15.33 kg", "WT > 15.33 kg")
  ok(!inherits(got_wb, "error") &&
       isTRUE(all.equal(got_wb$cuts_kg, c(12 + 2/3, 15 + 1/3))) &&
       identical(got_wb$subject$ID, 1:9) &&
       identical(as.character(got_wb$subject$WTBAND), rep(labels, each = 3)) &&
       identical(got_wb$summary$n_subjects, rep(3L, 3)) &&
       identical(got_wb$summary$wt_min_kg, c(10, 13, 16)) &&
       identical(got_wb$summary$wt_max_kg, c(12, 15, 18)),
     "B0u-d vpc_strata: public fixture verifies subject deduplication, cuts and summaries")
  expect_stop(vs$reference_weight_bands(fd[, "ID", drop = FALSE]),
              "B0u-d vpc_strata: a fit_data without WT is refused", "WT")

  # ---- npde_panels (A2 item 4) ---------------------------------------------
  np <- tryCatch(load_ref("diagnostics", "npde_panels.R"), error = function(e) e)
  ok(!inherits(np, "error"), "B0u-d npde_panels: the reference loads with its npde.R dependency")
  if (!inherits(np, "error")) {
    set.seed(3)
    ndf <- data.frame(ID = rep(1:4, each = 3), TIME = rep(c(1, 4, 12), 4),
                      tad = rep(c(1, 4, 12), 4), PRED = stats::runif(12, 5, 20),
                      NPDE = stats::rnorm(12))
    expect_stop(np$reference_npde_panels(ndf, nsim = 500, engine = "NONMEM"),
                "B0u-d npde_panels: seed is mandatory", "seed")
    expect_stop(np$reference_npde_panels(ndf, nsim = 500, seed = 1),
                "B0u-d npde_panels: engine is mandatory", "engine")
    expect_stop(np$reference_npde_panels(ndf, nsim = 499, seed = 1, engine = "NONMEM"),
                "B0u-d npde_panels: nsim below the house minimum is refused", "nsim")
    expect_stop(np$reference_npde_panels(ndf[, c("ID", "TIME", "NPDE")], nsim = 500,
                                         seed = 1, engine = "NONMEM"),
                "B0u-d npde_panels: a frame without PRED is refused", "PRED")
    if (have_pkg("pmplots")) {
      pn <- tryCatch(np$reference_npde_panels(ndf, nsim = 500, seed = 7, engine = "NONMEM"),
                     error = function(e) e)
      ok(!inherits(pn, "error") &&
         identical(names(pn), c("npde_pred", "npde_time", "npde_tad", "npde_hist",
                                "npde_q", "record")) &&
         identical(pn$record, list(engine = "NONMEM", nsim = 500, seed = 7)),
         "B0u-d npde_panels: five panels and the record (engine, nsim, seed)")
      odf <- ndf; odf$TAD <- odf$tad
      same_layers <- function(a, b) {
        n <- length(a$layers)
        n == length(b$layers) && all(vapply(seq_len(n), function(i)
          identical(ggplot2::layer_data(a, i), ggplot2::layer_data(b, i)), logical(1)))
      }
      if (!inherits(pn, "error"))
        ok(same_layers(pn$npde_pred, pmplots::npde_pred(odf)) &&
           same_layers(pn$npde_time, pmplots::npde_time(odf)) &&
           same_layers(pn$npde_tad, pmplots::npde_tad(odf)) &&
           same_layers(pn$npde_hist, pmplots::npde_hist(odf)) &&
           same_layers(pn$npde_q, pmplots::npde_q(odf)),
           "B0u-d npde_panels: the five panels equal the pmplots calls layer by layer")
    }
  }

  # ---- bootstrap_agreement ---------------------------------------------------
  ba <- load_ref("diagnostics", "bootstrap_agreement.R")
  e_bt <- c(0.0065, 1.3, 2.5); m_bt <- c(0.0068, 1.21, 2.5)
  ok(identical(ba$reference_bootstrap_pct_diff(e_bt, m_bt),
               round(100 * abs(m_bt - e_bt) / abs(e_bt), 2)),
     "B0u-d A2 item 3: pct_diff is qualify.R line 294's expression")
  expect_stop(ba$reference_bootstrap_pct_diff(c(1, 0), c(1, 1)),
              "B0u-d A2 item 3: a zero point estimate is refused", "zero")
  expect_stop(ba$reference_bootstrap_pct_diff(1, c(1, 2)),
              "B0u-d A2 item 3: estimates and medians of different length are refused", "length")

  # A3 item 2: replicate accounting.
  acc <- tryCatch(ba$reference_bootstrap_replicate_accounting(c(1, 1, 0), n_samples = 3,
                                                              requested = 3),
                  error = function(e) e)
  ok(!inherits(acc, "error") && identical(acc$success, c(TRUE, TRUE, FALSE)) &&
     identical(acc$success_rate_pct, round(100 * 2 / 3, 1)) &&
     identical(acc$n_requested, 3L) && identical(acc$n_successful, 2L),
     "B0u-d A3 item 2: success rate over the requested count, rounded to one decimal")
  expect_stop(ba$reference_bootstrap_replicate_accounting(c(1, 1), n_samples = 2, requested = 3),
              "B0u-d A3 item 2: fewer parsed sample rows than requested is a stop", "requested")
  expect_stop(ba$reference_bootstrap_replicate_accounting(c(1, 1, 1, 1), n_samples = 4,
                                                          requested = 3),
              "B0u-d A3 item 2: more parsed sample rows than requested is a stop", "requested")
  expect_stop(ba$reference_bootstrap_replicate_accounting(c(1, 1), n_samples = 3, requested = 3),
              "B0u-d A3 item 2: a flag vector shorter than the parsed rows is a stop", "flag")
  expect_stop(ba$reference_bootstrap_replicate_accounting(c(1, NA, 1), n_samples = 3, requested = 3),
              "B0u-d A3 item 2: a missing success flag is a stop", "minimization_successful")
  expect_stop(ba$reference_bootstrap_replicate_accounting(c(1, 2, 1), n_samples = 3, requested = 3),
              "B0u-d A3 item 2: a success flag of 2 is a stop", "minimization_successful")
  expect_stop(ba$reference_bootstrap_replicate_accounting(c("1", "yes", "0"), n_samples = 3,
                                                          requested = 3),
              "B0u-d A3 item 2: a non-numeric success flag is a stop", "minimization_successful")
  expect_stop(ba$reference_bootstrap_replicate_accounting(c(1, 0.5, 1), n_samples = 3, requested = 3),
              "B0u-d A3 item 2: a fractional success flag is a stop", "minimization_successful")
  acc0 <- tryCatch(ba$reference_bootstrap_replicate_accounting(c(0, 0), n_samples = 2, requested = 2),
                   error = function(e) e)
  ok(!inherits(acc0, "error") && identical(acc0$success_rate_pct, 0),
     "B0u-d A3 item 2: zero successful replicates give a percentage of 0")
  accc <- tryCatch(ba$reference_bootstrap_replicate_accounting(c("1", "0"), n_samples = 2,
                                                               requested = 2),
                   error = function(e) e)
  ok(!inherits(accc, "error") && identical(accc$success, c(TRUE, FALSE)),
     "B0u-d A3 item 2: character flags \"1\" and \"0\" are accepted")

  # A3 item 3: unsuccessful replicates by PsN flag category.
  FLAGS <- c("estimate_near_boundary", "rounding_errors", "zero_gradients",
             "final_zero_gradients", "hessian_reset", "s_matrix_singular")
  fl <- as.data.frame(matrix(0L, nrow = 6, ncol = length(FLAGS),
                             dimnames = list(NULL, FLAGS)))
  fl$rounding_errors <- c(1L, 1L, 0L, 0L, 1L, 0L)
  fl$zero_gradients  <- c(0L, 2L, 0L, 0L, 0L, 1L)
  succ <- c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE)
  uc <- tryCatch(ba$reference_bootstrap_unsuccessful_counts(succ, fl), error = function(e) e)
  want <- as.data.frame(table(message = c("rounding_errors",
                                          "rounding_errors + zero_gradients",
                                          "no flag set")), stringsAsFactors = FALSE)
  names(want) <- c("replicate_message", "count")
  ok(!inherits(uc, "error") && identical(uc, want),
     "B0u-d A3 item 3: categories are the flags set, joined in the declared order; successful rows ignored")
  ok(!inherits(uc, "error") && sum(uc$count) == sum(!succ),
     "B0u-d A3 item 3: counts by category sum to the unsuccessful count")
  ok(!inherits(uc, "error") && "no flag set" %in% uc$replicate_message,
     "B0u-d A3 item 3: an unsuccessful replicate with no flag set is its own category")
  uc0 <- tryCatch(ba$reference_bootstrap_unsuccessful_counts(rep(TRUE, 6), fl),
                  error = function(e) e)
  ok(!inherits(uc0, "error") && nrow(uc0) == 0L &&
     identical(names(uc0), c("replicate_message", "count")),
     "B0u-d A3 item 3: no unsuccessful replicate gives an empty table")
  expect_stop(ba$reference_bootstrap_unsuccessful_counts(succ, fl[, -2]),
              "B0u-d A3 item 3: a missing declared flag column is refused", "rounding_errors")
  fl_na <- fl; fl_na$hessian_reset[1] <- NA
  expect_stop(ba$reference_bootstrap_unsuccessful_counts(succ, fl_na),
              "B0u-d A3 item 3: a missing flag value is refused", "hessian_reset")
  expect_stop(ba$reference_bootstrap_unsuccessful_counts(succ[-1], fl),
              "B0u-d A3 item 3: success and flag rows of different length are refused", "rows")
  msgs <- c("a", "b", "a", "c")
  mt <- as.data.frame(table(message = msgs), stringsAsFactors = FALSE)
  names(mt) <- c("replicate_message", "count")
  ok(identical(ba$reference_bootstrap_category_table(msgs), mt),
     "B0u-d A3 item 3: the tabulation is qualify.R lines 351-353's expression")

  # A3 items 4 and 5: bootstrap precision, three decimals.
  sdv <- c(0.0123456, 0.0456789); ev <- c(0.0291234, 0.0281234)
  ok(identical(ba$reference_bootstrap_iiv_rse(sdv, ev), round(100 * sdv / ev, 3)),
     "B0u-d A3 item 4: IIV bootstrap RSE is round(100 * sd / estimate, 3)")
  expect_stop(ba$reference_bootstrap_iiv_rse(0.01, 0),
              "B0u-d A3 item 4: a zero variance estimate is refused", "positive")
  expect_stop(ba$reference_bootstrap_iiv_rse(0.01, -0.02),
              "B0u-d A3 item 4: a negative variance estimate is refused", "positive")
  fe <- c(-5.0871234, 0.2745678)
  ok(identical(ba$reference_bootstrap_fixed_rse(sdv, fe), round(100 * sdv / abs(fe), 3)),
     "B0u-d A3 item 5: fixed-effect bootstrap RSE is round(100 * sd / abs(estimate), 3)")
  expect_stop(ba$reference_bootstrap_fixed_rse(0.01, 0),
              "B0u-d A3 item 5: a zero point estimate is refused", "zero")
  expect_stop(ba$reference_bootstrap_fixed_rse(c(0.01, NA), c(1, 2)),
              "B0u-d A3 item 5: a missing standard deviation is refused", "finite")

  # A3 item 6: interval finiteness.
  ok(identical(ba$reference_bootstrap_ci_finite(c(1, Inf, NA, 1), c(2, 3, 4, NaN)),
               c(TRUE, FALSE, FALSE, FALSE)),
     "B0u-d A3 item 6: an interval is finite only when both bounds are finite")
  expect_stop(ba$reference_bootstrap_ci_finite(1, c(1, 2)),
              "B0u-d A3 item 6: bounds of different length are refused", "length")

  # A3 item 8: back-transformation.
  ok(identical(ba$reference_bootstrap_back_transform(c(0, 1), "exp"), exp(c(0, 1))) &&
     identical(ba$reference_bootstrap_back_transform(c(0, 1), "identity"), c(0, 1)),
     "B0u-d A3 item 8: identity and exp")
  expect_stop(ba$reference_bootstrap_back_transform(1, "log"),
              "B0u-d A3 item 8: any other transform is a stop", "identity")

  # A3 items 7 and 8: summaries. Type 6 oracle written out: h = (n + 1) p,
  # order statistic floor(h) plus (h - floor(h)) times the gap, clamped.
  q6 <- function(x, p) {
    x <- sort(x); n <- length(x); h <- (n + 1) * p
    if (h <= 1) return(x[1]); if (h >= n) return(x[n])
    j <- floor(h); x[j] + (h - j) * (x[j + 1] - x[j])
  }
  sd1 <- function(x) sqrt(sum((x - mean(x))^2) / (length(x) - 1))
  est <- data.frame(THETA1 = log(c(1, 2, 4, 9, 50)), OMEGA11 = c(0.1, 0.2, 0.3, 0.4, 99))
  s5 <- c(TRUE, TRUE, TRUE, TRUE, FALSE)   # four successful: an even count
  tr <- c(THETA1 = "exp", OMEGA11 = "identity")
  sm <- tryCatch(ba$reference_bootstrap_summaries(est, s5, tr, ci = 0.95), error = function(e) e)
  x1 <- est$THETA1[s5]
  ok(!inherits(sm, "error") &&
     identical(sm$parameter, c("THETA1", "OMEGA11")) &&
     identical(sm$n_successful, c(4L, 4L)) &&
     isTRUE(all.equal(sm$median_natural[1], q6(exp(x1), 0.5), tolerance = 1e-14)) &&
     isTRUE(all.equal(sm$ci_lower_natural[1], q6(exp(x1), 0.025), tolerance = 1e-14)) &&
     isTRUE(all.equal(sm$ci_upper_natural[1], q6(exp(x1), 0.975), tolerance = 1e-14)) &&
     isTRUE(all.equal(sm$sd_estimation[1], sd1(x1), tolerance = 1e-14)) &&
     isTRUE(all.equal(sm$median_natural[2], q6(est$OMEGA11[s5], 0.5), tolerance = 1e-14)),
     "B0u-d A3 items 7-8: summaries equal the type-6 oracle over successful replicates")
  ok(!inherits(sm, "error") &&
     !isTRUE(all.equal(exp(q6(x1, 0.5)), q6(exp(x1), 0.5))) &&
     isTRUE(all.equal(sm$median_natural[1], q6(exp(x1), 0.5))) &&
     !isTRUE(all.equal(sm$median_natural[1], exp(q6(x1, 0.5)))),
     "B0u-d A3 item 7: on an even count the entry point transforms first, then summarises")
  expect_stop(ba$reference_bootstrap_summaries(est, rep(FALSE, 5), tr, ci = 0.95),
              "B0u-d A3 item 7: zero successful replicates: summaries stop", "two")
  expect_stop(ba$reference_bootstrap_summaries(est, c(TRUE, FALSE, FALSE, FALSE, FALSE), tr,
                                               ci = 0.95),
              "B0u-d A3 item 7: exactly one successful replicate: summaries stop", "two")
  s2 <- tryCatch(ba$reference_bootstrap_summaries(est, c(TRUE, TRUE, FALSE, FALSE, FALSE), tr,
                                                  ci = 0.95), error = function(e) e)
  ok(!inherits(s2, "error") && identical(s2$n_successful, c(2L, 2L)),
     "B0u-d A3 item 7: exactly two successful replicates: summaries computed")
  est_inf <- est; est_inf$THETA1[2] <- Inf
  expect_stop(ba$reference_bootstrap_summaries(est_inf, s5, tr, ci = 0.95),
              "B0u-d A3 item 7: a non-finite estimate in a successful replicate is a stop",
              "non-finite")
  est_uinf <- est; est_uinf$THETA1[5] <- NA
  ok(!inherits(tryCatch(ba$reference_bootstrap_summaries(est_uinf, s5, tr, ci = 0.95),
                        error = function(e) e), "error"),
     "B0u-d A3 item 7: an unsuccessful replicate's estimate is not read")
  expect_stop(ba$reference_bootstrap_summaries(est, s5, tr["THETA1"], ci = 0.95),
              "B0u-d A3 item 8: a parameter without a declared transform is refused", "OMEGA11")
  expect_stop(ba$reference_bootstrap_summaries(est, s5, c(THETA1 = "log", OMEGA11 = "identity"),
                                               ci = 0.95),
              "B0u-d A3 item 8: an undeclared transform form is refused", "identity")
  expect_stop(ba$reference_bootstrap_summaries(est, s5, tr),
              "B0u-d A3 item 7: the interval level is declared, no default", "ci")
  expect_stop(ba$reference_bootstrap_summaries(est, c(TRUE, NA, TRUE, TRUE, FALSE), tr, ci = 0.95),
              "B0u-d A3 item 7: a missing success value is refused", "success")

  # ---- qualification_verdict: the second entry point (A3 item 1) -----------
  qv <- load_ref("diagnostics", "qualification_verdict.R")
  crit <- list(rse_fixed_max_pct = 30, rse_iiv_max_pct = 50, cond_no_max = 1000,
               shrink_flag_pct = 30, vpc_coverage_min_pct = 80, npde_p_flag = 0.01,
               boot_converged_min_pct = 80, boot_median_max_pct = 15)
  estn <- list(params = data.frame(
                 parameter = c("THETA1", "THETA2", "OMEGA11", "OMEGA22", "SIGMA11"),
                 kind = c("fixed", "fixed", "iiv", "iiv", "other"),
                 rse_pct = c(5, 12, 40, 20, 8),
                 shrink_sd_pct = c(NA, NA, 15, 35, NA), stringsAsFactors = FALSE),
               minimization_successful = TRUE, covariance_step_successful = TRUE,
               condition_number = 42.7, termination_message = "MINIMIZATION SUCCESSFUL")
  bts <- list(fixed = data.frame(parameter = c("THETA1", "THETA2"), rse_pct = c(6, 14),
                                 pct_diff_natural_scale = c(1.2, 3.0),
                                 ci_finite = c(TRUE, TRUE), stringsAsFactors = FALSE),
              iiv = data.frame(parameter = c("OMEGA11", "OMEGA22"), rse_pct = c(45, 25),
                               ci_finite = c(TRUE, TRUE), stringsAsFactors = FALSE),
              success_rate_pct = 100)
  npt <- data.frame(test = c("mean", "variance"), p_value = c(0.4, 0.2))
  vpcx <- list(coverage_pooled = data.frame(pct_within = c(88.9, 100),
                                            n_within_band = c(8, 9), n_bins = c(9, 9)),
               coverage_strat = data.frame(stratum = c("a", "b"), pct_within = c(88.9, 100)))
  run_v <- function(e = estn, b = bts, n = npt, v = vpcx, cr = crit)
    tryCatch(qv$reference_qualification_verdict_covstep(e, b, n, v, cr),
             error = function(err) err)
  disp <- function(v, crit_name) v$disposition[v$criterion == crit_name]
  val <- function(v, crit_name) v$computed_value[v$criterion == crit_name]
  v0 <- run_v()
  if (inherits(v0, "error")) cat(sprintf("      got: %s\n", conditionMessage(v0)))
  ok(is.data.frame(v0) &&
     identical(v0$criterion, c("E1", "E2 (fixed effects, covariance step)",
       "E2 (fixed effects, bootstrap, reported alongside)",
       "E2 (fixed effects) sources agree: THETA1", "E2 (fixed effects) sources agree: THETA2",
       "E2 (IIV, covariance step)", "E2 (IIV, bootstrap, reported alongside)",
       "E2 (IIV) sources agree: OMEGA11", "E2 (IIV) sources agree: OMEGA22",
       "E3", "E4", "E5", "E6", "Q1 (coverage)", "Q1 (drift)", "Q2",
       "Q3 (convergence)", "Q3 (medians)", "Q3 (intervals)")),
     "B0u-d A3 item 1: the row set and order of the second entry point")
  ok(is.data.frame(v0) &&
     identical(v0$disposition, c("PASS", "PASS", "PASS", "AGREE", "AGREE", "PASS", "PASS",
                                 "AGREE", "AGREE", "PASS", "SME-VISUAL", "FLAG", "SME",
                                 "PASS", "SME-VISUAL", "PASS", "PASS", "PASS", "PASS")),
     "B0u-d A3 item 1: sources agree, PASS: every agreement row AGREE")
  ok(is.data.frame(v0) && identical(val(v0, "Q3 (medians)"), "max |diff| 3.00% natural scale"),
     "B0u-d A3 item 1: Q3 medians on the natural scale only (no estimation-scale figure)")
  ok(is.data.frame(v0) && identical(val(v0, "E2 (fixed effects, covariance step)"),
                                    "max RSE 12.00% (THETA2)") &&
     identical(val(v0, "E2 (IIV, bootstrap, reported alongside)"), "max omega RSE 45.00% (OMEGA11)"),
     "B0u-d A3 item 1: RSE rows keep the first entry point's value format; SIGMA (kind other) is not an E2 input")
  ok(is.data.frame(v0) && identical(disp(v0, "E6"), "SME") &&
     !grepl("[0-9]", val(v0, "E6")),
     "B0u-d A3 item 1: E6 carries no computed value; disposition SME")
  ok(is.data.frame(v0) &&
     !any(grepl("gates/|legacy signature|signed decision",
                unlist(v0[c("requirement", "computed_value", "basis")]), ignore.case = TRUE)),
     "B0u-d A3 item 1: output contains no inherited signature or gate reference")
  # agree FAIL: both sources over the bound
  e_f <- estn; e_f$params$rse_pct[2] <- 35
  b_f <- bts; b_f$fixed$rse_pct[2] <- 40
  vf <- run_v(e = e_f, b = b_f)
  ok(is.data.frame(vf) && identical(disp(vf, "E2 (fixed effects, covariance step)"), "FAIL") &&
     identical(disp(vf, "E2 (fixed effects, bootstrap, reported alongside)"), "FAIL") &&
     identical(disp(vf, "E2 (fixed effects) sources agree: THETA2"), "AGREE"),
     "B0u-d A3 item 1: sources agree, FAIL: both rows FAIL, agreement AGREE")
  # disagreement, fixed effects
  b_d <- bts; b_d$fixed$rse_pct[2] <- 35
  vd <- run_v(b = b_d)
  ok(is.data.frame(vd) && identical(disp(vd, "E2 (fixed effects, covariance step)"), "PASS") &&
     identical(disp(vd, "E2 (fixed effects, bootstrap, reported alongside)"), "FAIL") &&
     identical(disp(vd, "E2 (fixed effects) sources agree: THETA2"), "SME") &&
     identical(disp(vd, "E2 (fixed effects) sources agree: THETA1"), "AGREE") &&
     grepl("12.00", val(vd, "E2 (fixed effects) sources agree: THETA2")) &&
     grepl("35.00", val(vd, "E2 (fixed effects) sources agree: THETA2")),
     "B0u-d A3 item 1: a fixed-effect disagreement gives SME with both values shown")
  # disagreement, IIV
  e_d <- estn; e_d$params$rse_pct[4] <- 60
  vdi <- run_v(e = e_d)
  ok(is.data.frame(vdi) && identical(disp(vdi, "E2 (IIV, covariance step)"), "FAIL") &&
     identical(disp(vdi, "E2 (IIV, bootstrap, reported alongside)"), "PASS") &&
     identical(disp(vdi, "E2 (IIV) sources agree: OMEGA22"), "SME"),
     "B0u-d A3 item 1: an IIV disagreement gives SME")
  # all covariance IIV RSE missing: stop
  e_na <- estn; e_na$params$rse_pct[3:4] <- NA
  ok(inherits(run_v(e = e_na), "error") &&
     grepl("no omega RSE", conditionMessage(run_v(e = e_na))),
     "B0u-d A3 item 1: all covariance IIV RSE missing is a stop")
  # one missing: FAIL not evaluable, bootstrap row unchanged
  e_1 <- estn; e_1$params$rse_pct[3] <- NA
  v1 <- run_v(e = e_1)
  ok(is.data.frame(v1) && identical(disp(v1, "E2 (IIV, covariance step)"), "FAIL") &&
     grepl("RSE unavailable", val(v1, "E2 (IIV, covariance step)")) &&
     identical(v1[v1$criterion == "E2 (IIV, bootstrap, reported alongside)", ],
               v0[v0$criterion == "E2 (IIV, bootstrap, reported alongside)", ]) &&
     identical(disp(v1, "E2 (IIV) sources agree: OMEGA11"), "SME"),
     "B0u-d A3 item 1: one missing covariance IIV RSE gives FAIL; the bootstrap row is unchanged")
  # a bootstrap value never fills a covariance row and vice versa
  b_x <- bts; b_x$iiv$rse_pct <- c(1, 2)
  vx <- run_v(b = b_x)
  ok(is.data.frame(vx) && identical(val(vx, "E2 (IIV, covariance step)"),
                                    val(v0, "E2 (IIV, covariance step)")),
     "B0u-d A3 item 1: bootstrap values never reach a covariance row")
  # all bootstrap fixed RSE missing: stop (fail-closed rule on all four rows)
  b_na <- bts; b_na$fixed$rse_pct <- NA_real_
  ok(inherits(run_v(b = b_na), "error"),
     "B0u-d A3 item 1: all bootstrap fixed-effect RSE missing is a stop")
  # a missing criterion stops
  ok(inherits(run_v(cr = crit[setdiff(names(crit), "boot_median_max_pct")]), "error"),
     "B0u-d A3 item 1: a missing criterion is a stop")
  # condition number absent: E3 FAIL, never PASS
  e_cn <- estn; e_cn$condition_number <- NA_real_
  vcn <- run_v(e = e_cn)
  ok(is.data.frame(vcn) && identical(disp(vcn, "E3"), "FAIL") &&
     grepl("not evaluable", val(vcn, "E3")),
     "B0u-d A3 item 1: a missing condition number makes E3 FAIL")
  # non-finite interval bounds: ci_finite FALSE, Q3 intervals FAIL
  b_ci <- bts; b_ci$iiv$ci_finite[2] <- FALSE
  vci <- run_v(b = b_ci)
  ok(is.data.frame(vci) && identical(disp(vci, "Q3 (intervals)"), "FAIL"),
     "B0u-d A3 item 6: a non-finite interval bound makes Q3 intervals FAIL")
  # input checks carried over
  e_k <- estn; e_k$params$kind[5] <- "sigma"
  ok(inherits(run_v(e = e_k), "error"),
     "B0u-d A3 item 1: an undeclared parameter kind is refused")
  e_m <- estn; e_m$minimization_successful <- NA
  ok(inherits(run_v(e = e_m), "error"),
     "B0u-d A3 item 1: a missing minimization status is refused")
  b_sr <- bts; b_sr$success_rate_pct <- 101
  ok(inherits(run_v(b = b_sr), "error"),
     "B0u-d A3 item 1: a success rate outside [0, 100] is refused")
  v_ns <- vpcx; v_ns$coverage_strat$stratum <- NULL
  ok(inherits(run_v(v = v_ns), "error"),
     "B0u-d A3 item 1: a stratified coverage table without a stratum column is refused")
  # the unit gates
  set_unit_status <- function(skill, unit, status) {
    p <- file.path("skills", skill, "MANIFEST.yaml")
    l <- readLines(p); i <- which(l == sprintf("  %s:", unit))[1]
    l[i + 1L] <- sprintf("    status: %s", status); writeLines(l, p)
  }
  set_unit_status("diagnostics", "qualification_verdict", "draft")
  ok(inherits(run_v(), "error") && grepl("not SME-validated", conditionMessage(run_v())),
     "B0u-d: the second verdict entry point refuses while its unit is draft")
  set_unit_status("diagnostics", "bootstrap_agreement", "draft")
  expect_stop(ba$reference_bootstrap_summaries(est, s5, tr, ci = 0.95),
              "B0u-d: bootstrap_agreement refuses while its unit is draft", "not SME-validated")
  set_unit_status("diagnostics", "vpc_strata", "draft")
  expect_stop(vs$reference_weight_bands(fd),
              "B0u-d: vpc_strata refuses while its unit is draft", "not SME-validated")
  set_unit_status("diagnostics", "npde_panels", "draft")
  if (!inherits(np, "error"))
    expect_stop(np$reference_npde_panels(ndf, nsim = 500, seed = 1, engine = "NONMEM"),
                "B0u-d: npde_panels refuses while its unit is draft", "not SME-validated")
}
cat(sprintf("batch9 verdict-interface cases: %s (%d passed, %d failed)\n",
            if (.fail == .b0ud_fail_before) "ALL PASS" else "FAILURES",
            .pass - .b0ud_pass_before, .fail - .b0ud_fail_before))

# ==== P1-4 (cont). the harness layer carries no binning default =============
# tools/qualification.R supplied `cfg$binning %||% list(method = "ntile",
# nbins = 9)` -- the same silent default the SME removed from the public API,
# one layer above it. Removed under SME decision 2; pinned here so it cannot
# come back.
qf <- readLines(src_file("tools/qualification.R"), warn = FALSE)
ok(!any(grepl("ntile", qf)),
   "P1-4: tools/qualification.R carries no ntile binning default")
ok(!any(grepl("binning\\s*%\\|\\|", qf)),
   "P1-4: tools/qualification.R does not fall back when cfg$binning is absent")

# ==== P2-9. Independent callers retain the declared seed stream =============
source(src_file("tools/seed.R"))
seeded_draws <- function() {
  harness_seed(4242)
  c(stats::rnorm(5), stats::runif(5), sample(1000, 5))
}
RNGkind("Mersenne-Twister", "Inversion", "Rejection")
set.seed(4242)
expected <- c(stats::rnorm(5), stats::runif(5), sample(1000, 5))
RNGkind("L'Ecuyer-CMRG", "Box-Muller", "Rejection")
first <- seeded_draws()
RNGkind("Wichmann-Hill", "Box-Muller", "Rejection")
second <- seeded_draws()
ok(identical(first, expected) && identical(second, expected),
   "B9-FORWARD: a declared seed yields the same stream from distinct ambient RNG kinds")

cat(sprintf("\nintegrity batch 9 battery: %d passed, %d failed\n", .pass, .fail))
quit(status = if (.fail > 0L) 1L else 0L)

}  # end .run_batch9_tests

if (.invoked_as_script()) .run_batch9_tests()
