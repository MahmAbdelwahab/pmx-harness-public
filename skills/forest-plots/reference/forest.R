# REFERENCE IMPLEMENTATION — Covariate forest plots (PMXForest)
#
# AUTHORITY: skills/forest-plots/MANIFEST.yaml is the authority for the SME
# signature (root reviewed_by / reviewed_at) and for the per-unit status and
# digests. This header carries no status claim of its own -- read the manifest.
#
# PMXForest reference and vignettes: https://github.com/pharmetheus/PMXForest
#
# Contract: agents never edit this file at analysis time. The one
# analysis-specific piece — the paramFunction re-implementing the model's
# covariate equations — is authored per analysis, MUST pass
# reference_forest_verify() before any plot is produced, and its source is
# part of the gate package for SME review.

# ---------------------------------------------------------------------------
# Self-gate (integrity batch 3a). forest-plots had NO self-gate: its public
# functions computed unconditionally even though every other reference lane is
# gated. They are now gated on the single intended-use unit "forest" (the whole
# file is one signed method). The manifest binds this file's sha256; the gate
# computes only when skills/forest-plots/MANIFEST.yaml carries a confirmed root
# (non-empty reviewed_by) and this unit's status is approved. The legacy
# "Reviewed by:" grep of SKILL.md was retired in batch 3c and is no longer
# consulted.

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

forest_validated <- base::local({
  private <- .mm_trusted_private
  function() {
    base::get("require_validated_method", envir = private)(
      skill_dir    = "skills/forest-plots",
      calling_file = "skills/forest-plots/reference/forest.R",
      unit         = "forest")
  }
}, envir = base::new.env())

.forest_gate <- function() {
  if (!forest_validated()) {
    base::stop("forest-plots unit 'forest' is not SME-validated (either the manifest ",
         "unit is draft/rejected, or skills/forest-plots/MANIFEST.yaml has an ",
         "empty root reviewed_by); forest data/plots do not ",
         "compute until its unit is approved. ESCALATE to the SME.",
         call. = FALSE)
  }
  base::invisible(TRUE)
}

# ---------------------------------------------------------------------------
# Covariate grid and display-value support

# Summarize covariate distributions; continuous display values default to the
# 5th/95th percentiles of the observed data (no extrapolation).
reference_forest_covstats <- function(data, covariates,
                                      probs = c(0.05, 0.95),
                                      id = "ID", miss = -99) {
  .forest_gate()
  PMXForest::getCovStats(data, covariates, probs = probs,
                         idVar = id, missVal = miss)
}

# listCovs: continuous -> named numeric vector of display values;
# dummy-coded categorical -> named nested list, one entry per non-reference
# level. See skills/forest-plots/SKILL.md.
reference_forest_covgrid <- function(listCovs, miss = -99) {
  .forest_gate()
  PMXForest::createInputForestData(listCovs, iMiss = miss)
}

# ---------------------------------------------------------------------------
# Parameter uncertainty samples

# NONMEM / PsN path: .cov file -> mvnorm samples (symmetric intervals);
# PsN bootstrap or SIR raw_results -> asymmetric intervals. Row 1 of the
# result is always the final estimates.
reference_forest_samples <- function(uncertainty_file, ext_file, n = NULL) {
  .forest_gate()
  PMXForest::getSamples(uncertainty_file, extFile = ext_file, n = n)
}

# nlmixr2 path: no .ext/.cov files exist. Build the same samples structure
# from the fit object — final estimates in row 1, then n draws from the
# asymptotic covariance matrix (symmetric), or replicate estimates
# (asymmetric) when supplied.
#
# `bootstrap` takes a data.frame of per-replicate estimates with columns
# named like fit$theta — NOT the return value of nlmixr2extra::bootstrapFit
# (that returns the fit data; per-replicate estimates are cached under
# nlmixr2BootstrapCache_<fitName>_<md5>/ — extraction from that cache is
# verified at first real use).
#
# n = 200 is the house parameter-uncertainty sample count. Returns a plain
# data.frame shaped like getSamples() output: final estimates in row 1,
# then the draws. Stops only via stopifnot(!missing(seed)) — the seed is
# mandatory (one seed per analysis). Same construction as the covariance
# path of reference_sim_uncertainty_draws() in
# simulation-standards/reference/uncertainty.R; keep the two in step.
reference_forest_samples_nlmixr <- function(fit, n = 200, seed,
                                            bootstrap = NULL) {
  .forest_gate()
  stopifnot(!missing(seed))
  if (!exists("harness_seed", mode = "function")) source(file.path("tools", "seed.R"), local = TRUE)
  harness_seed(seed)
  theta <- fit$theta
  if (!is.null(bootstrap)) {
    draws <- as.data.frame(bootstrap)[, names(theta), drop = FALSE]
  } else {
    vc <- stats::vcov(fit)
    keep <- intersect(names(theta), colnames(vc))
    draws <- MASS::mvrnorm(n, mu = theta[keep], Sigma = vc[keep, keep])
    draws <- as.data.frame(matrix(draws, nrow = n,
                                  dimnames = list(NULL, keep)))
    # Thetas absent from vcov (fixed / not estimated) are held at their
    # final estimates rather than dropped.
    fixed <- setdiff(names(theta), keep)
    for (f in fixed) draws[[f]] <- theta[[f]]
    draws <- draws[, names(theta), drop = FALSE]
  }
  rbind(as.data.frame(as.list(theta)), draws)
}

# ---------------------------------------------------------------------------
# paramFunction verification — mandatory before any forest data is computed.
#
# Evaluates the analysis-authored paramFunction at the final estimates
# (samples row 1) for the reference subject and compares against expected
# typical values from the fitted model. Tolerance is relative.
#
# `expected` is a named list/vector of the fitted model's typical values
# (from executed fit output, never hand-typed). Returns invisibly TRUE;
# stops if the value count disagrees with `expected` or any relative
# error exceeds `tolerance` (default 1e-3, matching the simulation
# translation gate).
reference_forest_verify <- function(param_function, samples, expected,
                                    df_ref = NULL, miss = -99,
                                    tolerance = 1e-3, ...) {
  .forest_gate()
  thetas <- as.numeric(samples[1, ])
  if (is.null(df_ref)) {
    # Default reference subject: every covariate at the missing code
    # (all-missing reference, house default) — paramFunction must fall
    # back to typical values when it sees `miss`.
    nm <- names(expected)
    df_ref <- as.data.frame(as.list(stats::setNames(rep(miss, length(nm)), nm)))
  }
  got <- unlist(param_function(thetas, df_ref, ...))
  if (length(got) != length(expected)) {
    stop("paramFunction returned ", length(got), " values; ",
         length(expected), " expected quantities were declared.",
         call. = FALSE)
  }
  exp_v <- unlist(expected)
  # FAIL CLOSED (integrity batch 7, Codex round 8 finding 8): the tolerance
  # test below used `na.rm = TRUE`, so a paramFunction returning NA/NaN/Inf
  # for a quantity passed verification. A non-finite value is not "within
  # tolerance"; it is an unverifiable equation. Stop, naming the positions,
  # before any comparison — for the returned values and for `expected`.
  bad_got <- which(is.na(got) | !is.finite(got))
  if (length(bad_got)) {
    stop("paramFunction returned non-finite value(s) at the final estimates ",
         "at position(s) ", paste(bad_got, collapse = ", "),
         " (", paste(format(got[bad_got]), collapse = ", "), "); ",
         "verification cannot pass on a non-finite quantity. Fix the ",
         "paramFunction before producing any forest plot.", call. = FALSE)
  }
  bad_exp <- which(is.na(exp_v) | !is.finite(exp_v))
  if (length(bad_exp)) {
    stop("`expected` carries non-finite value(s) at position(s) ",
         paste(bad_exp, collapse = ", "),
         "; expected typical values come from executed fit output and must ",
         "all be finite.", call. = FALSE)
  }
  rel <- abs(got - exp_v) / pmax(abs(exp_v), .Machine$double.eps)
  if (any(rel > tolerance)) {
    bad <- which(rel > tolerance)
    stop("paramFunction failed verification at the final estimates:\n",
         paste(sprintf("  [%d] got %.6g, expected %.6g (rel. err %.2e)",
                       bad, got[bad], unlist(expected)[bad], rel[bad]),
               collapse = "\n"),
         "\nThe re-implemented equations disagree with the fitted model; ",
         "fix the paramFunction before producing any forest plot.",
         call. = FALSE)
  }
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# Forest data

# Parametric: effects from the model equations, one covariate row at a time.
#
# House primary method (SME 2026-08-28). probs = c(0.05, 0.95) encodes the
# 90%-interval house default; n_thetas (PMXForest noBaseThetas) is how
# many leading columns of `samples` are handed to param_function as
# thetas. Row 1 of `samples` (final estimates) gives the point estimate;
# the remaining rows give the interval. The seed is mandatory and set
# here because PMXForest may sample internally. Returns the forest
# data.frame consumed by reference_forest_plot().
reference_forest_parametric <- function(df_covs, row_labels,
                                        param_function, metric_names,
                                        n_thetas, samples,
                                        probs = c(0.05, 0.95),
                                        df_ref = NULL, seed, ...) {
  .forest_gate()
  stopifnot(!missing(seed))
  if (!exists("harness_seed", mode = "function")) source(file.path("tools", "seed.R"), local = TRUE)
  harness_seed(seed)
  PMXForest::getForestDFSCM(
    dfCovs           = df_covs,
    cdfCovsNames     = row_labels,
    functionList     = list(param_function),
    functionListName = metric_names,
    noBaseThetas     = n_thetas,
    dfParameters     = samples,
    probs            = probs,
    dfRefRow         = df_ref,
    ...)
}

# Empirical: covariate groups defined by expressions over the analysis
# dataset; observed covariate correlations retained.
#
# Companion to the parametric method (which is the house primary), useful
# when covariates are correlated and one-at-a-time rows would mislead.
# Same interval, seed, and return conventions as
# reference_forest_parametric().
reference_forest_empirical <- function(data, cov_expressions, row_labels,
                                       param_function, metric_names,
                                       n_thetas, samples,
                                       probs = c(0.05, 0.95),
                                       id = "ID", df_ref = NULL, seed, ...) {
  .forest_gate()
  stopifnot(!missing(seed))
  if (!exists("harness_seed", mode = "function")) source(file.path("tools", "seed.R"), local = TRUE)
  harness_seed(seed)
  PMXForest::getForestDFemp(
    dfData             = data,
    covExpressionsList = cov_expressions,
    cdfCovsNames       = row_labels,
    functionList       = list(param_function),
    functionListName   = metric_names,
    noBaseThetas       = n_thetas,
    dfParameters       = samples,
    probs              = probs,
    strID              = id,
    dfRefRow           = df_ref,
    ...)
}

# ---------------------------------------------------------------------------
# Rendering. ref_area is the clinical-relevance / no-effect band — the
# 0.8–1.25 default is a bioequivalence convention; the analysis plan declares
# the value actually used.
#
# Pure rendering: intervals come from the forest data.frame, nothing is
# recomputed here. Returns the PMXForest plot object for the gate package
# — forest plots are reporting-stage artifacts.
reference_forest_plot <- function(dfres, parameters,
                                  group_labels = NULL,
                                  ref_area = c(0.8, 1.25),
                                  relative = TRUE,
                                  xlab = NULL,
                                  table = TRUE,
                                  stacked = FALSE, ...) {
  .forest_gate()
  args <- list(dfres,
               parameters   = parameters,
               plotRelative = relative,
               ref_area     = ref_area,
               table        = table,
               stackedPlots = stacked, ...)
  if (!is.null(group_labels)) args$groupNameLabels <- group_labels
  if (!is.null(xlab))         args$xlb <- xlab
  do.call(PMXForest::forestPlot, args)
}
