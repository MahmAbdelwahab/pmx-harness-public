# REFERENCE IMPLEMENTATION — Parameter uncertainty propagation (simulation)
#
# AUTHORITY: skills/simulation-standards/MANIFEST.yaml is the authority for the SME
# signature (root reviewed_by / reviewed_at) and for the per-unit status and
# digests. This header carries no status claim of its own -- read the manifest.
#
# The full uncertainty tier propagates parameter uncertainty as well as IIV.
# It samples parameters with MASS::mvrnorm using the fit covariance and
# runs each draw with mrgsolve. Review across-draw summaries at the gate.
#
#   draws   <- reference_sim_uncertainty_draws(fit, n_draws, seed, ...)
#   check   <- reference_sim_install_check(mod, draws, install_fn, ...)
#   result  <- reference_simulate_full(mod, scenarios, seed, draws,
#                                      install_fn, ...)
#
# install_fn(mod, draw) is ANALYSIS-SPECIFIC numerics — it installs one
# theta draw into the compiled translation (the per-draw sibling of the
# translation's model_from_fit constructor) and lives with the analysis,
# never in this skill. reference_sim_install_check() is its mandatory
# verification: with draw 1 (the final estimates) installed, the typical
# profile must match the fit-installed model exactly. Same two controls as
# every other analysis-authored numeric: verify against the fit, review at
# the gate.
#
# Draw sources follow the forest-plots hierarchy (SME 2026-08-28):
# covariance matrix by default; bootstrap/SIR replicate estimates when the
# analysis already ran them. Covariance draws are THETA-ONLY (omega/sigma
# held at estimates); bootstrap replicates may carry omega columns —
# resolved by house decision D1 below.
#
# House decisions (SME, 2026-08-29):
#   D1 Uncertainty scope is source-dependent: covariance draws are
#      theta-only (the vcov covers fixed effects); bootstrap-sourced draws
#      install the replicate omega/sigma as well — the replicates carry
#      them and dropping them understates uncertainty. Recorded per run.
#      (Bootstrap-omega installation activates with the first
#      bootstrap-sourced full run, once the replicate omega column
#      convention is verified against a real nlmixr2extra cache.)
#   D2 Default n_draws = 200 (one house number for parameter-uncertainty
#      sampling, matching forest plots). The MAP may raise it.
#   D3 Summary convention: within each draw compute the IIV percentiles
#      (5/50/95 across subjects); across draws report each percentile's
#      median and 95% interval — estimation uncertainty kept separate
#      from population variability. reference_sim_full_summary()
#      implements this.


# Self-gate predicate, twin of sim_reference_validated() in mrgsolve_sim.R:
# TRUE only when skills/simulation-standards/MANIFEST.yaml has a non-empty
# root reviewed_by/reviewed_at and this unit's status: line reads
# "approved". The authority is that MANIFEST record, not the "Reviewed by:"
# comment above — that comment is a historical record the validator ignores.
# Signing is done by filling reviewed_by in the MANIFEST and approving the
# unit, never by editing the comment above.
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

uncertainty_reference_validated <- base::local({
  private <- .mm_trusted_private
  function() {
    # Integrity batch 2: delegates to the method-manifest self-gate core; binds
    # to this reference file's sha256 once the SME approves the manifest. The
    # DEPRECATED `legacy` closure (ignored by the validator since batch 3c) was
    # removed in integrity batch 13, when this file was next re-validated.
    base::get("require_validated_method", envir = private)(
      skill_dir    = "skills/simulation-standards",
      calling_file = "skills/simulation-standards/reference/uncertainty.R",
      unit         = "uncertainty")
  }
}, envir = base::new.env())

.uncertainty_gate <- function() {
  if (!uncertainty_reference_validated()) {
    base::stop("simulation-standards uncertainty propagation is not yet ",
         "SME-validated (unit uncertainty is draft/rejected, or ",
         "skills/simulation-standards/MANIFEST.yaml has an empty root ",
         "reviewed_by); the `full` tier does not run ",
         "until the manifest record is signed. ESCALATE to the SME.",
         call. = FALSE)
  }
  base::invisible(TRUE)
}

# ---------------------------------------------------------------------------
# Theta draws. Row 1 is always the final estimates (forest-samples pattern).

# Inputs: fit (nlmixr2 fit object), n_draws (house default 200, D2), seed
# (the analysis seed), source, and — required when source = "bootstrap" —
# a data.frame of per-replicate estimates.
# Covariance path: theta-only MASS::mvrnorm on vcov(fit) (D1); thetas
# absent from vcov (fixed / not estimated) are held at their final
# estimates rather than dropped.
# Bootstrap path: the replicate table is taken as-is after validating that
# every theta column is present — n_draws is IGNORED there; the replicate
# count is the draw count. Same construction as
# reference_forest_samples_nlmixr() in forest-plots; keep the two in step.
# Returns a data.frame with the final estimates bound as row 1 and a
# "record" attribute (source, n_draws, seed) destined for the run record.
reference_sim_uncertainty_draws <- function(fit, n_draws = 200, seed,
                                            source = c("covariance",
                                                       "bootstrap"),
                                            bootstrap = NULL) {
  .uncertainty_gate()
  stopifnot(!missing(seed))
  source <- match.arg(source)
  if (!exists("harness_seed", mode = "function")) source(file.path("tools", "seed.R"), local = TRUE)
  harness_seed(seed)
  theta <- fit$theta
  if (source == "bootstrap") {
    if (is.null(bootstrap)) {
      stop("source = 'bootstrap' requires `bootstrap`: a data.frame of ",
           "per-replicate estimates with columns named like fit$theta.",
           call. = FALSE)
    }
    missing_cols <- setdiff(names(theta), names(bootstrap))
    if (length(missing_cols)) {
      stop("bootstrap replicates lack theta column(s): ",
           paste(missing_cols, collapse = ", "), call. = FALSE)
    }
    draws <- as.data.frame(bootstrap)[, names(theta), drop = FALSE]
  } else {
    vc <- stats::vcov(fit)
    keep <- intersect(names(theta), colnames(vc))
    Sigma <- vc[keep, keep, drop = FALSE]
    # Validate the covariance before drawing (F10, Codex round 8): a
    # non-finite, non-symmetric or non-positive-semidefinite vcov would
    # otherwise produce silent NaNs or a cryptic mvrnorm error.
    if (any(!is.finite(as.matrix(Sigma))))
      stop("vcov(fit) has non-finite entries; cannot draw parameter ",
           "uncertainty. Refit or supply a finite covariance.", call. = FALSE)
    if (!isSymmetric(unname(as.matrix(Sigma)), tol = 1e-8))
      stop("vcov(fit) is not symmetric; cannot draw parameter uncertainty.",
           call. = FALSE)
    ev <- eigen(Sigma, symmetric = TRUE, only.values = TRUE)$values
    if (min(ev) < -1e-8 * max(abs(ev), 1))
      stop("vcov(fit) is not positive semidefinite (min eigenvalue ",
           format(min(ev), digits = 3), "); cannot draw parameter ",
           "uncertainty.", call. = FALSE)
    # MASS::mvrnorm returns a plain VECTOR when n_draws == 1; normalise to a
    # one-row matrix so the data-frame shape is invariant in the draw count.
    dr <- MASS::mvrnorm(n_draws, mu = theta[keep], Sigma = Sigma)
    dr <- matrix(dr, nrow = n_draws, dimnames = list(NULL, keep))
    draws <- as.data.frame(dr)
    fixed <- setdiff(names(theta), keep)
    for (f in fixed) draws[[f]] <- theta[[f]]
    draws <- draws[, names(theta), drop = FALSE]
  }
  out <- rbind(as.data.frame(as.list(theta)), draws)
  attr(out, "record") <- list(source = source, n_draws = nrow(out) - 1L,
                              seed = seed)
  out
}

# ---------------------------------------------------------------------------
# install_fn verification — mandatory before any full-tier scenario.
#
# With draw 1 (the final estimates) installed via the analysis-authored
# install_fn, the typical (zero-RE) profile of a probe regimen must match
# the reference model `mod` (already fit-installed and equivalence-checked
# by the translation's own gate). Relative tolerance.
#
# The 1e-8 default is deliberately far tighter than the translation gate's
# 1e-3: installing the exact numbers the model already carries should be
# an identity up to floating point, so any real discrepancy means
# install_fn maps a theta onto the wrong model parameter. Returns
# invisibly list(n, max_rel_err, tolerance); stops on failure.
reference_sim_install_check <- function(mod, draws, install_fn,
                                        probe = list(amt = 100, cmt = 1),
                                        end = 24, delta = 0.5,
                                        observed = "CP",
                                        tolerance = 1e-8) {
  .uncertainty_gate()
  sim_typical <- function(m) {
    ev <- mrgsolve::ev(amt = probe$amt, cmt = probe$cmt)
    mrgsolve::as.data.frame(mrgsolve::mrgsim(
      mrgsolve::zero_re(m), events = ev, end = end, delta = delta,
      obsonly = TRUE))[[observed]]
  }
  ref <- sim_typical(mod)
  got <- sim_typical(install_fn(mod, draws[1, , drop = FALSE]))
  rel <- abs(got - ref) / pmax(abs(ref), .Machine$double.eps)
  # Fail CLOSED on non-finite comparison values (F10, Codex round 8): the old
  # tolerance test dropped NA/NaN/Inf via na.rm, so an install_fn that
  # produced them passed verification silently. A non-finite comparison means
  # the check could not be evaluated — a stop, never a pass.
  if (any(!is.finite(rel))) {
    stop("install_fn verification produced non-finite comparison value(s) ",
         "(NA/NaN/Inf) at ", sum(!is.finite(rel)), " of ", length(rel),
         " probe points; the check cannot be evaluated. Fix install_fn ",
         "before any full-tier run.", call. = FALSE)
  }
  if (any(rel > tolerance)) {
    stop("install_fn failed verification: with the final estimates ",
         "(draws row 1) installed it does not reproduce the fit-installed ",
         "model (max rel err ", format(max(rel), digits = 3),
         " > ", tolerance, "). Fix install_fn before any full-tier run.",
         call. = FALSE)
  }
  invisible(list(n = length(ref), max_rel_err = max(rel),
                 tolerance = tolerance))
}

# ---------------------------------------------------------------------------
# Full-tier scenario simulation: for each theta draw, install and simulate
# the scenario with IIV. Returns the long simulation with a DRAW column and
# a run record; summarize with reference_sim_full_summary() (house
# decision D3).
#
# Scenario spec is the same shape as reference_simulate() in
# mrgsolve_sim.R ($regimen, $n, $grid or $times). Review note: per-draw
# output is not passed through reference_sim_physical_checks() here —
# whether that check belongs inside the draw loop is an open point for
# the SME to settle at signing.

reference_simulate_full <- function(mod, scenarios, seed, draws, install_fn,
                                    observed = "CP") {
  .uncertainty_gate()
  stopifnot(!missing(seed), is.list(scenarios),
            length(names(scenarios)) > 0, is.function(install_fn))
  if (any(!nzchar(names(scenarios)))) {
    stop("Every scenario must be named.", call. = FALSE)
  }
  if (!exists("harness_seed", mode = "function")) source(file.path("tools", "seed.R"), local = TRUE)
  harness_seed(seed)
  out <- lapply(names(scenarios), function(nm) {
    sc <- scenarios[[nm]]
    r <- sc$regimen
    ev_args <- list(amt = r$amt, cmt = r$cmt)
    if (!is.null(r$ii))   ev_args$ii   <- r$ii
    if (!is.null(r$addl)) ev_args$addl <- r$addl
    if (!is.null(r$rate)) ev_args$rate <- r$rate
    dose <- do.call(mrgsolve::ev, ev_args)
    per_draw <- lapply(seq_len(nrow(draws)), function(i) {
      # zero_re(..., "sigma") drops only RUV: each draw simulates
      # individual parameters with IIV, not noisy observations — the same
      # convention as the iiv tier in reference_simulate().
      m <- mrgsolve::zero_re(install_fn(mod, draws[i, , drop = FALSE]),
                             "sigma")
      sim_args <- list(m, events = dose, nid = sc$n %||% 1L,
                       obsonly = TRUE)
      if (!is.null(sc$times)) sim_args$tgrid <- sc$times
      else { g <- sc$grid; sim_args$start <- g$start %||% 0
             sim_args$end <- g$end; sim_args$delta <- g$delta }
      s <- mrgsolve::as.data.frame(do.call(mrgsolve::mrgsim, sim_args))
      s$DRAW <- i - 1L   # 0 = final estimates
      s
    })
    s <- do.call(rbind, per_draw)
    s$SCENARIO <- nm
    s
  })
  list(
    sim = do.call(rbind, out),
    record = list(engine = "mrgsolve",
                  version = as.character(utils::packageVersion("mrgsolve")),
                  seed = seed, uncertainty = "full",
                  draws = attr(draws, "record"),
                  scenarios = scenarios, observed = observed)
  )
}

# ---------------------------------------------------------------------------
# Summary across draws (house decision D3): within each draw, the IIV
# percentiles over subjects at each time; across draws, each percentile's
# median and 95% interval. Returns one row per scenario x time x percentile.

reference_sim_full_summary <- function(sim, observed = "CP",
                                       iiv_probs = c(0.05, 0.50, 0.95),
                                       unc_level = 0.95) {
  .uncertainty_gate()
  need <- c("SCENARIO", "DRAW", "ID", "time", observed)
  missing_cols <- setdiff(need, names(sim))
  if (length(missing_cols)) {
    stop("sim is missing column(s): ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }
  lo <- (1 - unc_level) / 2
  out <- list()
  for (sc in unique(sim$SCENARIO)) {
    s <- sim[sim$SCENARIO == sc, ]
    for (tm in unique(s$time)) {
      st <- s[s$time == tm, ]
      # vapply over split() yields an iiv_probs x draws matrix, one column per
      # DRAW, named by the DRAW value.
      per_draw <- vapply(split(st[[observed]], st$DRAW),
                         stats::quantile, numeric(length(iiv_probs)),
                         probs = iiv_probs, names = FALSE)
      # DRAW 0 is the point ESTIMATE and is NOT part of the uncertainty
      # distribution (F10, Codex round 8): the across-draw median and interval
      # are computed over DRAW > 0 only.
      #
      # Round 9, P1-5: DRAW 0 used to be taken positionally (`v[1]`, `v[-1]`)
      # on the argument that split() orders a numeric DRAW column numerically.
      # That is true only when DRAW 0 is actually present; with DRAW 0 absent
      # for a scenario/time, DRAW 1 silently became the reported estimate and
      # was struck from the uncertainty summary. The columns are now indexed
      # by their DRAW labels, and the pairing is required rather than assumed.
      draw_labels <- colnames(per_draw)
      if (is.null(draw_labels)) draw_labels <- names(split(st[[observed]], st$DRAW))
      is_zero <- draw_labels == "0"
      if (sum(is_zero) != 1L)
        stop("reference_sim_full_summary: scenario '", sc, "' at time ", tm,
             " has ", sum(is_zero), " DRAW 0 group(s); exactly one is ",
             "required — DRAW 0 is the point estimate.", call. = FALSE)
      for (i in seq_along(iiv_probs)) {
        v <- per_draw[i, ]
        est <- v[is_zero]
        unc <- v[!is_zero]                  # DRAW > 0 only
        if (!length(unc))
          stop("reference_sim_full_summary: scenario '", sc, "' at time ", tm,
               " has no DRAW > 0 — the uncertainty summary needs at least one ",
               "uncertainty draw beyond DRAW 0 (the estimate).", call. = FALSE)
        # Round 9 fix round (review S5). The first attempt FILTERED non-finite
        # draws out of `unc`. That silently changed what a passing computation
        # returned: a scenario/time whose per-draw quantiles included an Inf
        # previously fed it into median()/quantile() and returned a number, and
        # would now return a different number over a smaller set. The SME's
        # rule is refusals, never silent subsetting — a non-finite uncertainty
        # draw is a stop that names where it is.
        if (!all(is.finite(unc)))
          stop("reference_sim_full_summary: scenario '", sc, "' at time ", tm,
               " has ", sum(!is.finite(unc)), " non-finite DRAW > 0 quantile(s) ",
               "at percentile ", iiv_probs[i], " (draw(s): ",
               paste(names(unc)[!is.finite(unc)], collapse = ", "),
               "). An uncertainty summary is not computed over a value that is ",
               "not a number, and draws are never silently dropped. ",
               "ESCALATE to the SME.", call. = FALSE)
        out[[length(out) + 1L]] <- data.frame(
          SCENARIO = sc, time = tm, iiv_pctile = iiv_probs[i],
          estimate = unname(est),           # the DRAW 0 group
          unc_median = stats::median(unc),
          unc_lower = unname(stats::quantile(unc, lo)),
          unc_upper = unname(stats::quantile(unc, 1 - lo)))
      }
    }
  }
  do.call(rbind, out)
}

# Null-coalescing helper, duplicated from mrgsolve_sim.R so each reference
# file sources standalone (base R only ships %||% from 4.4).
`%||%` <- function(a, b) if (is.null(a)) b else a
