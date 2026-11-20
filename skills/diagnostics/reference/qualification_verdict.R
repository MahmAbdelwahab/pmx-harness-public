# REFERENCE IMPLEMENTATION — Qualification verdict table (MAP technical
# criteria applied as deterministic comparisons)
#
# AUTHORITY: skills/diagnostics/MANIFEST.yaml is the authority for the SME signature
# (root reviewed_by / reviewed_at) and for the status and digest of unit `qualification_verdict`.
# This header carries no status claim of its own -- read the manifest.
# Apply the caller-supplied qualification criteria to refit, NPDE, VPC
# and bootstrap evidence. Visual judgments remain for local review.
# Input contract (the shapes the qualification task already produces):
#   refit       list(parFixed = data.frame(parameter, rse_pct, shrink_sd_pct),
#                    cond_no_cov = numeric(1),
#                    refit = list(converged = logical, message = character))
#   npde_tests  data.frame with a p_value column (global mean/variance tests)
#   vpc         list(coverage_pooled = data.frame(pct_within, n_within_band,
#                    n_bins), coverage_strat = data.frame(pct_within))
#   boot        list(fixed = data.frame(pct_diff_natural_scale,
#                    pct_diff_estimation_scale, ci_finite),
#                    omega = data.frame(parameter, rse_pct, ci_finite),
#                    success_rate_pct = numeric(1))
#   criteria    named list; EVERY name in QUAL_VERDICT_CRITERIA is required and
#               must be a single finite number.
#
# Returns a data.frame(criterion, requirement, computed_value, disposition,
# basis), one row per MAP 4.3 criterion, in the fixed row order below.
#
# A second entry point, reference_qualification_verdict_covstep (end of this
# file, 2026-09-27), takes E2 precision from the engine's covariance step and
# reports the bootstrap alongside; its contract is described there. The first
# entry point is unchanged in behaviour: its three local helpers moved to file
# level (.qv_need, .qv_rse_verdict, .qv_row) with their bodies unchanged, so
# that both entry points share them.


QUAL_VERDICT_CRITERIA <- c("rse_fixed_max_pct", "rse_iiv_max_pct", "cond_no_max",
                           "shrink_flag_pct", "vpc_coverage_min_pct",
                           "npde_p_flag", "boot_converged_min_pct",
                           "boot_median_max_pct")

# Self-gate on the intended-use unit "qualification_verdict" (same pattern as
# the other diagnostics units).
# --- the private, locked validator environment (integrity batch 11, R10-P0-1)
# The self-gate below used to call whatever `require_validated_method` was
# visible on the search path, sourcing tools/method_manifest.R only if none
# was. A caller could therefore bind `require_validated_method <- function(...)
# TRUE` before sourcing this file and every draft unit computed. The validator
# is now loaded ONCE into a private environment whose parent is the base
# environment, and the environment is locked with its bindings; the gate is a
# closure over that environment, so nothing a caller binds -- before or after
# sourcing -- changes what the gate calls.
#
# THE HANDOVER, stated where the construction is written. Every executable
# name the construction reaches -- local, new.env, baseenv, file.exists, stop,
# sys.source and lockEnvironment, and since integrity batch 13 the `c` of
# every path vector, the `local` of every gate and accessor closure, and the
# heads of the pre-seal write -- is written with the exact `base::`
# qualification, and the block's own frame is supplied explicitly as
# `envir = base::new.env(parent = base::baseenv())`, so neither the frame nor
# its parent is the environment this file is sourced into. A caller who
# pre-binds a fake `sys.source`, `new.env`, `c` or `local` there is not
# consulted (integrity batches 12 and 13, Codex rounds 11 and 12). What
# remains is a caller who rebinds the `::` operator or R's syntax heads
# (`{`, `(`, `<-`, `if`, `for`, `!`, `function`) in that environment, or
# modifies the base namespace itself: the same class of act as modifying R.
# Same-process reflection -- a caller reaching a sealed environment through
# `environment(fn)`, `parent.env<-` or an active binding -- is out of scope
# of the method guard: its environment protects against ordinary
# rebinding, not against a hostile caller in the same process; the controls
# there are the trusted loader (tools/load_reference.R), the manifest
# digests, the write guard and the SME's review of the diff.
.mm_trusted_private <- base::local({
  e <- base::new.env(parent = base::baseenv())
  if (!base::file.exists("tools/method_manifest.R"))
    base::stop("this reference requires tools/method_manifest.R; references are ",
               "sourced with the repository root as the working directory.",
               call. = FALSE)
  base::sys.source("tools/method_manifest.R", envir = e, keep.source = FALSE)
  base::lockEnvironment(e, bindings = TRUE)
  e
}, envir = base::new.env(parent = base::baseenv()))

.diag_verdict_gate <- base::local({
  private <- .mm_trusted_private
  function() {
    if (!base::get("require_validated_method", envir = private)(skill_dir = "skills/diagnostics",
                                          calling_file = "skills/diagnostics/reference/qualification_verdict.R",
                                          unit = "qualification_verdict")) {
      base::stop("diagnostics unit 'qualification_verdict' is not SME-validated ",
           "(manifest unit draft/rejected, or the manifest has an empty ",
           "reviewed_by); the verdict table does not compute until the unit is ",
           "approved. ESCALATE to the SME.", call. = FALSE)
    }
    base::invisible(TRUE)
  }
}, envir = base::new.env())

# Every threshold declared, single, finite. Stops naming the first offender.
.qual_verdict_check_criteria <- function(criteria) {
  if (!is.list(criteria)) stop("criteria must be a named list", call. = FALSE)
  miss <- setdiff(QUAL_VERDICT_CRITERIA, names(criteria))
  if (length(miss))
    stop("criteria is missing declared threshold(s): ",
         paste(miss, collapse = ", "),
         ". Every threshold comes from the signed MAP; none has a default.",
         call. = FALSE)
  for (nm in QUAL_VERDICT_CRITERIA) {
    v <- criteria[[nm]]
    if (!is.numeric(v) || length(v) != 1L || !is.finite(v))
      stop("criteria$", nm, " must be a single finite number; got ",
           deparse(v), call. = FALSE)
  }
  invisible(TRUE)
}

# Shared helpers of both entry points, moved to file level unchanged from the
# first entry point's body (2026-09-27).
.qv_need <- function(x, what, columns = NULL) {
  if (is.null(x)) stop("verdict input is missing ", what, call. = FALSE)
  if (!is.null(columns)) {
    if (!is.data.frame(x) || !nrow(x))
      stop("verdict input ", what, " must be a non-empty data.frame.",
           call. = FALSE)
    missing_columns <- setdiff(columns, names(x))
    if (length(missing_columns))
      stop("verdict input ", what, " is missing column(s): ",
           paste(missing_columns, collapse = ", "), ".", call. = FALSE)
  }
}

# RSE reductions FAIL CLOSED (SME decision, integrity batch 7 phase 3). The
# relocated code took `max(rse, na.rm = TRUE)`, which is -Inf when every RSE
# is NA and then PASSes the criterion with no RSE at all. Now: an RSE vector
# that is entirely missing is a stop (the criterion cannot be evaluated at
# all — usually an absent covariance step); any single NA or non-finite RSE
# makes the criterion not evaluable and the disposition FAIL with the reason
# "RSE unavailable" naming the parameters. Never PASS, never a silent drop.
.qv_rse_verdict <- function(rse, names, limit, what) {
  if (is.null(rse) || length(rse) == 0 || all(is.na(rse)))
    stop("no ", what, " RSE values are available (all missing); the E2 ",
         "criterion cannot be evaluated. A missing covariance step is a ",
         "qualification failure, not a table entry. ESCALATE.", call. = FALSE)
  bad <- is.na(rse) | !is.finite(rse)
  if (any(bad)) {
    return(list(value = sprintf("RSE unavailable for: %s (criterion not evaluable)",
                                paste(names[bad], collapse = ", ")),
                disposition = "FAIL"))
  }
  list(value = sprintf("max %sRSE %.2f%% (%s)", if (identical(what, "omega")) "omega " else "",
                       max(rse), names[which.max(rse)]),
       disposition = if (max(rse) <= limit) "PASS" else "FAIL")
}

.qv_row <- function(criterion, requirement, value, disposition, basis) {
  data.frame(criterion = criterion, requirement = requirement,
             computed_value = value, disposition = disposition,
             basis = basis, stringsAsFactors = FALSE)
}

reference_qualification_verdict <- function(refit, npde_tests, vpc, boot, criteria) {
  .diag_verdict_gate()
  .qual_verdict_check_criteria(criteria)
  need <- .qv_need
  need(refit$parFixed, "refit$parFixed",
       c("parameter", "rse_pct", "shrink_sd_pct"))
  need(refit$cond_no_cov, "refit$cond_no_cov")
  need(refit$refit$converged, "refit$refit$converged")
  need(refit$refit$message, "refit$refit$message")
  need(npde_tests, "npde_tests", "p_value")
  need(vpc$coverage_pooled, "vpc$coverage_pooled",
       c("pct_within", "n_within_band", "n_bins"))
  need(vpc$coverage_strat, "vpc$coverage_strat", "pct_within")
  need(boot$fixed, "boot$fixed",
       c("pct_diff_natural_scale", "pct_diff_estimation_scale", "ci_finite"))
  need(boot$omega, "boot$omega", c("parameter", "rse_pct", "ci_finite"))
  need(boot$success_rate_pct, "boot$success_rate_pct")

  numeric_evidence <- list(
    "refit$cond_no_cov" = refit$cond_no_cov,
    "npde_tests$p_value" = npde_tests$p_value,
    "vpc$coverage_pooled$pct_within" = vpc$coverage_pooled$pct_within,
    "vpc$coverage_pooled$n_within_band" = vpc$coverage_pooled$n_within_band,
    "vpc$coverage_pooled$n_bins" = vpc$coverage_pooled$n_bins,
    "vpc$coverage_strat$pct_within" = vpc$coverage_strat$pct_within,
    "boot$fixed$pct_diff_natural_scale" = boot$fixed$pct_diff_natural_scale,
    "boot$fixed$pct_diff_estimation_scale" = boot$fixed$pct_diff_estimation_scale,
    "boot$success_rate_pct" = boot$success_rate_pct
  )
  for (what in names(numeric_evidence)) {
    x <- numeric_evidence[[what]]
    if (!is.numeric(x) || !length(x) || any(!is.finite(x)))
      stop("verdict input ", what, " must contain finite numeric evidence.",
           call. = FALSE)
  }
  # These are domains of the reported evidence, not acceptance thresholds.
  # Scientific criteria remain supplied by the signed MAP above.
  domain <- function(what, lower, upper) {
    x <- numeric_evidence[[what]]
    if (any(x < lower | x > upper))
      stop("verdict input ", what, " must lie in [", lower, ", ", upper,
           "].", call. = FALSE)
  }
  domain("npde_tests$p_value", 0, 1)
  for (what in c("vpc$coverage_pooled$pct_within",
                 "vpc$coverage_strat$pct_within", "boot$success_rate_pct"))
    domain(what, 0, 100)
  counts <- vpc$coverage_pooled
  if (any(counts$n_bins <= 0 | counts$n_bins != floor(counts$n_bins)) ||
      any(counts$n_within_band < 0 |
          counts$n_within_band != floor(counts$n_within_band) |
          counts$n_within_band > counts$n_bins))
    stop("verdict input coverage counts require positive integer n_bins and ",
         "integer n_within_band between zero and n_bins.", call. = FALSE)

  if (!is.logical(refit$refit$converged) || !length(refit$refit$converged) ||
      anyNA(refit$refit$converged))
    stop("verdict input refit$refit$converged must contain complete logical evidence.",
         call. = FALSE)
  for (what in c("fixed", "omega")) {
    ci <- boot[[what]]$ci_finite
    if (!is.logical(ci) || !length(ci) || anyNA(ci))
      stop("verdict input boot$", what,
           "$ci_finite must contain complete logical evidence.", call. = FALSE)
  }

  pf <- refit$parFixed
  cov_ok <- is.finite(refit$cond_no_cov)
  conv_ok <- all(refit$refit$converged)

  # RSE reductions fail closed: see .qv_rse_verdict above.
  rse_verdict <- .qv_rse_verdict
  e2_fixed <- rse_verdict(pf$rse_pct, pf$parameter, criteria$rse_fixed_max_pct, "fixed-effect")
  e2_iiv   <- rse_verdict(boot$omega$rse_pct, boot$omega$parameter,
                          criteria$rse_iiv_max_pct, "omega")
  shrink <- pf[!is.na(pf$shrink_sd_pct), c("parameter", "shrink_sd_pct")]
  high_shrink <- shrink[shrink$shrink_sd_pct > criteria$shrink_flag_pct, ]
  covp <- vpc$coverage_pooled
  # The MAP Q1 wording ("the 5th, 50th and 95th percentiles lie within the
  # bands in at least 80% of bins") admits two readings, so both are
  # computed and both appear in the verdict row: the STRICT per-percentile
  # reading (each displayed percentile must clear the minimum in its own
  # bins) and the POOLED reading (all percentile-bin combinations together).
  # The disposition uses the strict reading — the conservative one — and
  # never resolves the ambiguity in favour of passing.
  min_cov <- min(covp$pct_within)
  pooled_cov <- round(100 * sum(covp$n_within_band) / sum(covp$n_bins), 1)
  min_cov_strat <- min(vpc$coverage_strat$pct_within)
  min_p <- min(npde_tests$p_value)
  max_med_diff_nat <- max(boot$fixed$pct_diff_natural_scale)
  max_med_diff_est <- max(boot$fixed$pct_diff_estimation_scale)
  ci_all_finite <- all(boot$fixed$ci_finite) && all(boot$omega$ci_finite)

  row <- .qv_row
  rbind(
    row("E1", "successful minimization and covariance step",
        sprintf("converged = %s ('%s'); covariance step %s",
                conv_ok, refit$refit$message[1],
                if (cov_ok) "successful" else "ABSENT"),
        if (conv_ok && cov_ok) "PASS" else "FAIL",
        "verification refit (identical to signed run 101)"),
    row("E2 (fixed effects)",
        sprintf("all fixed-effect RSE <= %g%%", criteria$rse_fixed_max_pct),
        e2_fixed$value, e2_fixed$disposition,
        "verification refit parFixedDf"),
    row("E2 (IIV, deferred to Q3 bootstrap)",
        sprintf("all IIV variance-term RSE <= %g%%", criteria$rse_iiv_max_pct),
        e2_iiv$value, e2_iiv$disposition,
        "bootstrap SD / point estimate (closes base-model gate item 4)"),
    row("E3", sprintf("condition number < %g", criteria$cond_no_max),
        sprintf("covariance condition number %.1f", refit$cond_no_cov),
        if (cov_ok && refit$cond_no_cov < criteria$cond_no_max) "PASS"
        else "FAIL",
        "verification refit objDf"),
    row("E4", "no systematic trend in CWRES vs time / TAD / predictions",
        "CWRES panels rendered in the GOF artifact",
        "SME-VISUAL",
        "gof panel (reference gof.R); zero line and +/-2 guides per skill"),
    row("E5",
        sprintf("shrinkage reported per eta; > %g%% flagged",
                criteria$shrink_flag_pct),
        paste(sprintf("%s: %.1f%%", shrink$parameter, shrink$shrink_sd_pct),
              collapse = "; "),
        if (nrow(high_shrink)) "FLAG" else "PASS",
        "verification refit parFixedDf (flag = disclosure, not failure)"),
    row("E6", "typical values within published neonatal ranges",
        "review at the local gate",
        "SME-VISUAL", "typical values and the declared literature ranges"),
    row("Q1 (coverage)",
        sprintf("observed 5th/50th/95th inside 95%% bands in >= %g%% of bins",
                criteria$vpc_coverage_min_pct),
        sprintf("time VPC: strict per-percentile min %.1f%%, pooled over all percentile-bins %.1f%%; weight-stratified per-percentile min %.1f%%",
                min_cov, pooled_cov, min_cov_strat),
        if (min_cov >= criteria$vpc_coverage_min_pct) "PASS" else "FAIL",
        "tidyvpc stats; disposition on the STRICT per-percentile reading (both readings shown - see notes)"),
    row("Q1 (drift)",
        "no systematic drift across time or across the weight range",
        "per-bin band placement in VPC panels; miss sides in coverage tables",
        "SME-VISUAL",
        "vpc + weight-stratified vpc artifacts, coverage tables"),
    row("Q2",
        sprintf("NPDE global mean and variance tests; p < %g flagged",
                criteria$npde_p_flag),
        sprintf("min p = %.4g", min_p),
        if (min_p < criteria$npde_p_flag) "FLAG" else "PASS",
        "declared-code global tests on reference NPDE values"),
    row("Q3 (convergence)",
        sprintf(">= %g%% of replicates minimize successfully",
                criteria$boot_converged_min_pct),
        sprintf("success rate %.1f%%", boot$success_rate_pct),
        if (boot$success_rate_pct >= criteria$boot_converged_min_pct) "PASS"
        else "FAIL",
        "per-replicate engine cache records"),
    row("Q3 (medians)",
        sprintf("bootstrap medians within %g%% of point estimates (fixed effects)",
                criteria$boot_median_max_pct),
        sprintf("max |diff| %.2f%% natural scale (%.2f%% estimation scale)",
                max_med_diff_nat, max_med_diff_est),
        if (max_med_diff_nat <= criteria$boot_median_max_pct) "PASS"
        else "FAIL",
        "bootstrap summary vs point estimates; verdict on the natural (back-transformed) scale, both scales shown"),
    row("Q3 (intervals)",
        "95% percentile intervals finite and plausible",
        sprintf("all interval bounds finite = %s", ci_all_finite),
        if (ci_all_finite) "PASS" else "FAIL",
        "bootstrap percentile CIs; plausibility is SME judgment on the tables")
  )
}

# Covariance-step precision with the bootstrap summary alongside:
#   - E2 precision comes from the engine's covariance step (primary). The
#     bootstrap precision of the same parameters is reported alongside, at the
#     same bound, in its own row, and one "sources agree" row per parameter
#     (the MAP A3 row: "a 'sources agree' row per parameter") compares the
#     parameter's pass/fail in the two sources: AGREE when equal, SME when
#     they differ or either source cannot evaluate it (both values shown; the
#     SME decides). A covariance RSE is never filled from the bootstrap, and a
#     bootstrap value never appears in a covariance row.
#   - Parameters of kind `other` (for example a residual-error variance) are
#     not E2 inputs; E2 reads kind `fixed` and kind `iiv` only.
#   - E1 reads the engine's minimization and covariance-step status; E3 reads
#     the engine's condition number, and a missing one is FAIL "not evaluable"
#     (the fail-closed pattern of the RSE rows), never PASS.
#   - E6 carries no computed value; the SME judges it at the gate.
#   - Q3 medians are reported on the natural scale only (no estimation-scale
#     figure; the new contract carries no estimation-scale column).
#   - Requirement and basis texts cite the MAP criterion and bound only.
#
# Input contract
#   estimation  list(params = data.frame(parameter, kind, rse_pct, shrink_sd_pct),
#                    minimization_successful = logical(1),
#                    covariance_step_successful = logical(1),
#                    condition_number = numeric(1) (NA when not reported),
#                    termination_message = character)
#               kind is one of fixed, iiv, other
#   bootstrap   list(fixed = data.frame(parameter, rse_pct, pct_diff_natural_scale,
#                                       ci_finite),
#                    iiv = data.frame(parameter, rse_pct, ci_finite),
#                    success_rate_pct = numeric(1))
#   npde_tests  as for the first entry point
#   vpc         as for the first entry point; coverage_strat also has `stratum`
#   criteria    every name of QUAL_VERDICT_CRITERIA, none defaulted
reference_qualification_verdict_covstep <- function(estimation, bootstrap, npde_tests,
                                                    vpc, criteria) {
  .diag_verdict_gate()
  .qual_verdict_check_criteria(criteria)
  need <- .qv_need
  need(estimation$params, "estimation$params",
       c("parameter", "kind", "rse_pct", "shrink_sd_pct"))
  need(estimation$minimization_successful, "estimation$minimization_successful")
  need(estimation$covariance_step_successful, "estimation$covariance_step_successful")
  need(estimation$condition_number, "estimation$condition_number")
  need(estimation$termination_message, "estimation$termination_message")
  need(bootstrap$fixed, "bootstrap$fixed",
       c("parameter", "rse_pct", "pct_diff_natural_scale", "ci_finite"))
  need(bootstrap$iiv, "bootstrap$iiv", c("parameter", "rse_pct", "ci_finite"))
  need(bootstrap$success_rate_pct, "bootstrap$success_rate_pct")
  need(npde_tests, "npde_tests", "p_value")
  need(vpc$coverage_pooled, "vpc$coverage_pooled",
       c("pct_within", "n_within_band", "n_bins"))
  need(vpc$coverage_strat, "vpc$coverage_strat", c("stratum", "pct_within"))

  pars <- estimation$params
  if (!is.character(pars$kind) || anyNA(pars$kind) ||
      !all(pars$kind %in% c("fixed", "iiv", "other")))
    stop("verdict input estimation$params$kind must be one of fixed, iiv, other.",
         call. = FALSE)
  for (what in c("minimization_successful", "covariance_step_successful")) {
    x <- estimation[[what]]
    if (!is.logical(x) || length(x) != 1L || is.na(x))
      stop("verdict input estimation$", what, " must be a single TRUE or FALSE.",
           call. = FALSE)
  }
  cn <- estimation$condition_number
  if (!is.numeric(cn) || length(cn) != 1L)
    stop("verdict input estimation$condition_number must be a single number (NA ",
         "when the engine reported none).", call. = FALSE)
  if (!is.character(estimation$termination_message))
    stop("verdict input estimation$termination_message must be character.",
         call. = FALSE)
  numeric_evidence <- list(
    "npde_tests$p_value" = npde_tests$p_value,
    "vpc$coverage_pooled$pct_within" = vpc$coverage_pooled$pct_within,
    "vpc$coverage_pooled$n_within_band" = vpc$coverage_pooled$n_within_band,
    "vpc$coverage_pooled$n_bins" = vpc$coverage_pooled$n_bins,
    "vpc$coverage_strat$pct_within" = vpc$coverage_strat$pct_within,
    "bootstrap$fixed$pct_diff_natural_scale" = bootstrap$fixed$pct_diff_natural_scale,
    "bootstrap$success_rate_pct" = bootstrap$success_rate_pct
  )
  for (what in names(numeric_evidence)) {
    x <- numeric_evidence[[what]]
    if (!is.numeric(x) || !length(x) || any(!is.finite(x)))
      stop("verdict input ", what, " must contain finite numeric evidence.",
           call. = FALSE)
  }
  domain <- function(what, lower, upper) {
    x <- numeric_evidence[[what]]
    if (any(x < lower | x > upper))
      stop("verdict input ", what, " must lie in [", lower, ", ", upper,
           "].", call. = FALSE)
  }
  domain("npde_tests$p_value", 0, 1)
  for (what in c("vpc$coverage_pooled$pct_within",
                 "vpc$coverage_strat$pct_within", "bootstrap$success_rate_pct"))
    domain(what, 0, 100)
  counts <- vpc$coverage_pooled
  if (any(counts$n_bins <= 0 | counts$n_bins != floor(counts$n_bins)) ||
      any(counts$n_within_band < 0 |
          counts$n_within_band != floor(counts$n_within_band) |
          counts$n_within_band > counts$n_bins))
    stop("verdict input coverage counts require positive integer n_bins and ",
         "integer n_within_band between zero and n_bins.", call. = FALSE)
  for (what in c("fixed", "iiv")) {
    ci <- bootstrap[[what]]$ci_finite
    if (!is.logical(ci) || !length(ci) || anyNA(ci))
      stop("verdict input bootstrap$", what,
           "$ci_finite must contain complete logical evidence.", call. = FALSE)
  }

  fx <- pars[pars$kind == "fixed", ]
  iv <- pars[pars$kind == "iiv", ]
  rse_verdict <- .qv_rse_verdict
  e2_fixed_cov  <- rse_verdict(fx$rse_pct, fx$parameter, criteria$rse_fixed_max_pct,
                               "fixed-effect")
  e2_fixed_boot <- rse_verdict(bootstrap$fixed$rse_pct, bootstrap$fixed$parameter,
                               criteria$rse_fixed_max_pct, "fixed-effect")
  e2_iiv_cov    <- rse_verdict(iv$rse_pct, iv$parameter, criteria$rse_iiv_max_pct, "omega")
  e2_iiv_boot   <- rse_verdict(bootstrap$iiv$rse_pct, bootstrap$iiv$parameter,
                               criteria$rse_iiv_max_pct, "omega")

  row <- .qv_row
  # One row per parameter named by either source: AGREE when the parameter's
  # pass/fail at the bound is the same in both, SME otherwise.
  agree_rows <- function(label, cov_p, cov_r, boot_p, boot_r, limit) {
    one <- function(r) if (length(r) != 1L || !is.finite(r)) NA
                       else if (r <= limit) "PASS" else "FAIL"
    show <- function(r, d) if (is.na(d)) "not evaluable" else sprintf("%.2f%% %s", r, d)
    params <- unique(c(cov_p, boot_p))
    do.call(rbind, lapply(params, function(p) {
      rc <- cov_r[cov_p == p]; rb <- boot_r[boot_p == p]
      dc <- one(rc); db <- one(rb)
      row(sprintf("%s sources agree: %s", label, p),
          sprintf("covariance-step and bootstrap RSE agree on RSE <= %g%%", limit),
          sprintf("covariance step %s; bootstrap %s", show(rc, dc), show(rb, db)),
          if (!is.na(dc) && !is.na(db) && dc == db) "AGREE" else "SME",
          "MAP E2: disagreement is shown with both values and decided by the SME")
    }))
  }

  cov_ok <- estimation$covariance_step_successful
  conv_ok <- estimation$minimization_successful
  cn_ok <- is.finite(cn)
  shrink <- pars[!is.na(pars$shrink_sd_pct), c("parameter", "shrink_sd_pct")]
  high_shrink <- shrink[shrink$shrink_sd_pct > criteria$shrink_flag_pct, ]
  covp <- vpc$coverage_pooled
  min_cov <- min(covp$pct_within)
  pooled_cov <- round(100 * sum(covp$n_within_band) / sum(covp$n_bins), 1)
  min_cov_strat <- min(vpc$coverage_strat$pct_within)
  min_p <- min(npde_tests$p_value)
  max_med_diff_nat <- max(bootstrap$fixed$pct_diff_natural_scale)
  ci_all_finite <- all(bootstrap$fixed$ci_finite) && all(bootstrap$iiv$ci_finite)

  rbind(
    row("E1", "successful minimization and covariance step",
        sprintf("converged = %s ('%s'); covariance step %s",
                conv_ok, estimation$termination_message[1],
                if (cov_ok) "successful" else "not successful"),
        if (conv_ok && cov_ok) "PASS" else "FAIL",
        "MAP E1: engine run record (minimization and covariance-step status)"),
    row("E2 (fixed effects, covariance step)",
        sprintf("all fixed-effect RSE <= %g%%", criteria$rse_fixed_max_pct),
        e2_fixed_cov$value, e2_fixed_cov$disposition,
        "MAP E2: covariance-step standard error / |estimate|"),
    row("E2 (fixed effects, bootstrap, reported alongside)",
        sprintf("all fixed-effect RSE <= %g%%", criteria$rse_fixed_max_pct),
        e2_fixed_boot$value, e2_fixed_boot$disposition,
        "MAP E2: bootstrap SD / |estimate|, estimation scale, reported alongside"),
    agree_rows("E2 (fixed effects)", fx$parameter, fx$rse_pct,
               bootstrap$fixed$parameter, bootstrap$fixed$rse_pct,
               criteria$rse_fixed_max_pct),
    row("E2 (IIV, covariance step)",
        sprintf("all IIV variance-term RSE <= %g%%", criteria$rse_iiv_max_pct),
        e2_iiv_cov$value, e2_iiv_cov$disposition,
        "MAP E2: covariance-step standard error / estimate of the variance term"),
    row("E2 (IIV, bootstrap, reported alongside)",
        sprintf("all IIV variance-term RSE <= %g%%", criteria$rse_iiv_max_pct),
        e2_iiv_boot$value, e2_iiv_boot$disposition,
        "MAP E2: bootstrap SD / estimate of the variance term, reported alongside"),
    agree_rows("E2 (IIV)", iv$parameter, iv$rse_pct,
               bootstrap$iiv$parameter, bootstrap$iiv$rse_pct,
               criteria$rse_iiv_max_pct),
    row("E3", sprintf("condition number < %g", criteria$cond_no_max),
        if (cn_ok) sprintf("covariance condition number %.1f", cn)
        else "condition number unavailable (criterion not evaluable)",
        if (cov_ok && cn_ok && cn < criteria$cond_no_max) "PASS" else "FAIL",
        "MAP E3: engine-reported condition number"),
    row("E4", "no systematic trend in CWRES vs time / TAD / predictions",
        "CWRES panels rendered in the GOF artifact",
        "SME-VISUAL",
        "gof panel (reference gof.R); zero line and +/-2 guides per skill"),
    row("E5",
        sprintf("shrinkage reported per eta; > %g%% flagged",
                criteria$shrink_flag_pct),
        paste(sprintf("%s: %.1f%%", shrink$parameter, shrink$shrink_sd_pct),
              collapse = "; "),
        if (nrow(high_shrink)) "FLAG" else "PASS",
        "MAP E5: engine-reported shrinkage (flag = disclosure, not failure)"),
    row("E6", "typical values within the published ranges the MAP cites",
        "no computed value; judged at the gate",
        "SME", "MAP E6: SME judgment on the parameter table"),
    row("Q1 (coverage)",
        sprintf("observed 5th/50th/95th inside 95%% bands in >= %g%% of bins",
                criteria$vpc_coverage_min_pct),
        sprintf("time VPC: strict per-percentile min %.1f%%, pooled over all percentile-bins %.1f%%; weight-stratified per-percentile min %.1f%%",
                min_cov, pooled_cov, min_cov_strat),
        if (min_cov >= criteria$vpc_coverage_min_pct) "PASS" else "FAIL",
        "tidyvpc stats; disposition on the STRICT per-percentile reading (both readings shown - see notes)"),
    row("Q1 (drift)",
        "no systematic drift across time or across the weight range",
        "per-bin band placement in VPC panels; miss sides in coverage tables",
        "SME-VISUAL",
        "vpc + weight-stratified vpc artifacts, coverage tables"),
    row("Q2",
        sprintf("NPDE global mean and variance tests; p < %g flagged",
                criteria$npde_p_flag),
        sprintf("min p = %.4g", min_p),
        if (min_p < criteria$npde_p_flag) "FLAG" else "PASS",
        "declared-code global tests on reference NPDE values"),
    row("Q3 (convergence)",
        sprintf(">= %g%% of replicates minimize successfully",
                criteria$boot_converged_min_pct),
        sprintf("success rate %.1f%%", bootstrap$success_rate_pct),
        if (bootstrap$success_rate_pct >= criteria$boot_converged_min_pct) "PASS"
        else "FAIL",
        "MAP Q3: successful replicates over requested replicates"),
    row("Q3 (medians)",
        sprintf("bootstrap medians within %g%% of point estimates (fixed effects)",
                criteria$boot_median_max_pct),
        sprintf("max |diff| %.2f%% natural scale", max_med_diff_nat),
        if (max_med_diff_nat <= criteria$boot_median_max_pct) "PASS"
        else "FAIL",
        "MAP Q3: bootstrap medians vs point estimates on the natural (back-transformed) scale"),
    row("Q3 (intervals)",
        "95% percentile intervals finite and plausible",
        sprintf("all interval bounds finite = %s", ci_all_finite),
        if (ci_all_finite) "PASS" else "FAIL",
        "bootstrap percentile CIs; plausibility is SME judgment on the tables")
  )
}
