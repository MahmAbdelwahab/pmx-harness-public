# REFERENCE IMPLEMENTATION — Exposure metrics, unit identities and profile
# summaries on simulated output
#
# AUTHORITY: skills/simulation-standards/MANIFEST.yaml is the authority for the SME signature
# (root reviewed_by / reviewed_at) and for the status and digest of unit `exposure_metrics`.
# This header carries no status claim of its own -- read the manifest.
# Authored 2026-09-03 (integrity batch 7, Codex round 8 finding 4).
#
# WHY THIS FILE EXISTS. The single-dose exposure-metric set of house default 4
# (Cmax, AUC(0-tlast) by linear trapezoid, AUC(0-inf) = amt / CL_i), the two
# unit identities enforced on it (AUC(0-tlast) <= AUC(0-inf); Cmax <= amt/V_i)
# and the median [lower, upper] summaries — including the per-time
# concentration profile summary the deliverable figure draws — were computed
# inside pipeline/tasks/simulation.R with an inline tolerance (1 + 1e-6) and
# an implicit quantile type. Task modules never carry analysis numerics. The
# arithmetic is RELOCATED here verbatim. Two fail-closed input stops were
# ADDED that the task did not have: a scenario/subject with fewer than two
# grid points (no AUC is defined) and any non-finite metric value (the old
# code would have errored on `if (NA)` at the identity test; it now stops
# with a named reason). No definition, threshold or comparison changed. Every
# numeric choice is a DECLARED PARAMETER with no default: probabilities,
# quantile type, and the relative tolerance of the identity checks are passed
# by the caller from the analysis declaration and reviewed at the gate.
#
# Definitions (house default 4, single-dose set):
#   cmax        grid maximum of the observed column over the simulated interval
#   auc_0_tlast linear trapezoid of the observed column on the simulation
#               grid, to the grid end (tlast)
#   auc_0_inf   amt / CL_i, the subject's simulated apparent clearance — exact
#               for a linear one-compartment model; the caller confirms
#               applicability with the SME for any other structure
# A metric name outside this set is a stop, never an improvised definition.


EXPOSURE_METRICS_IMPLEMENTED <- c("cmax", "auc_0_tlast", "auc_0_inf")

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

sim_exposure_validated <- base::local({
  private <- .mm_trusted_private
  function() {
    base::get("require_validated_method", envir = private)(
      skill_dir    = "skills/simulation-standards",
      calling_file = "skills/simulation-standards/reference/exposure_metrics.R",
      unit         = "exposure_metrics")
  }
}, envir = base::new.env())

.sim_exposure_gate <- function() {
  if (!sim_exposure_validated()) {
    base::stop("simulation-standards unit 'exposure_metrics' is not SME-validated ",
         "(manifest unit draft/rejected, or the manifest has an empty ",
         "reviewed_by); exposure metrics do not compute until the unit is ",
         "approved. ESCALATE to the SME.", call. = FALSE)
  }
  base::invisible(TRUE)
}

.exposure_require_number <- function(x, what, len = 1L) {
  if (missing(x) || is.null(x) || !is.numeric(x) || length(x) != len || any(!is.finite(x)))
    stop(what, " must be declared as ", len, " finite number(s); none has a default.",
         call. = FALSE)
  invisible(TRUE)
}

.exposure_require_probs <- function(probs) {
  .exposure_require_number(probs, "probs", 2L)
  if (probs[1] >= probs[2] || any(probs < 0) || any(probs > 1))
    stop("probs must be two increasing probabilities in [0, 1]; got ",
         paste(probs, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}

.exposure_require_quantile_type <- function(quantile_type) {
  if (missing(quantile_type) || is.null(quantile_type) ||
      !is.numeric(quantile_type) || length(quantile_type) != 1L ||
      !quantile_type %in% 1:9)
    stop("quantile_type must be declared as one of 1..9 (stats::quantile type); ",
         "it has no default.", call. = FALSE)
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# reference_exposure_by_subject(sim, scenarios, observed, time, clearance,
#                               volume, auc_tolerance)
#
# sim         long simulated data.frame with columns SCENARIO, ID, `time`,
#             `observed`, `clearance`, `volume` (the reference_simulate output)
# scenarios   the named scenario list of the run record; each element's
#             $regimen$amt is the dose the metrics refer to
# observed    concentration column name (e.g. "CP")
# time        time column name (e.g. "time")
# clearance   subject apparent-clearance column (e.g. "CL")
# volume      subject volume column (e.g. "V"), for the mass-balance identity
# auc_tolerance  relative tolerance of the two identity checks; DECLARED,
#             no default. A violation is a stop: do not rescale anything to
#             quiet it; find the mismatch.
#
# Returns data.frame(SCENARIO, ID, tlast, cmax, auc_0_tlast, auc_0_inf), one
# row per scenario x subject.
reference_exposure_by_subject <- function(sim, scenarios, observed, time,
                                          clearance, volume, auc_tolerance) {
  .sim_exposure_gate()
  .exposure_require_number(auc_tolerance, "auc_tolerance")
  if (auc_tolerance < 0) stop("auc_tolerance must be >= 0", call. = FALSE)
  need <- c("SCENARIO", "ID", time, observed, clearance, volume)
  miss <- setdiff(need, names(sim))
  if (length(miss))
    stop("simulated output is missing column(s): ", paste(miss, collapse = ", "),
         call. = FALSE)
  if (!is.list(scenarios) || is.null(names(scenarios)))
    stop("scenarios must be the named scenario list of the run record", call. = FALSE)
  by_subject <- do.call(rbind, lapply(
    split(sim, list(sim$SCENARIO, sim$ID), drop = TRUE),
    function(d) {
      d <- d[order(d[[time]]), ]
      sc <- scenarios[[as.character(d$SCENARIO[1])]]
      if (is.null(sc) || is.null(sc$regimen$amt))
        stop("scenario '", d$SCENARIO[1], "' has no declared regimen$amt",
             call. = FALSE)
      amt <- sc$regimen$amt
      cp <- d[[observed]]
      tt <- d[[time]]
      if (length(tt) < 2L)
        stop("scenario '", d$SCENARIO[1], "', ID ", d$ID[1],
             ": fewer than two grid points; no AUC is defined.", call. = FALSE)
      if (!is.numeric(tt) || !isTRUE(tt[1] == 0))
        stop("scenario '", d$SCENARIO[1], "', ID ", d$ID[1],
             ": AUC(0-tlast) requires a sampling grid that starts at time 0.",
             call. = FALSE)
      data.frame(
        SCENARIO    = d$SCENARIO[1],
        ID          = d$ID[1],
        tlast       = max(tt),
        cmax        = max(cp),
        auc_0_tlast = sum(diff(tt) * (cp[-length(cp)] + cp[-1]) / 2),
        auc_0_inf   = amt / d[[clearance]][1],
        cmax_bound  = amt / d[[volume]][1],
        stringsAsFactors = FALSE)
    }))
  rownames(by_subject) <- NULL
  if (anyNA(by_subject[, c("cmax", "auc_0_tlast", "auc_0_inf", "cmax_bound")]) ||
      any(!is.finite(as.matrix(by_subject[, c("cmax", "auc_0_tlast", "auc_0_inf",
                                                "cmax_bound")]))))
    stop("non-finite exposure metric(s); the simulated output or the ",
         "clearance/volume columns are not usable. ESCALATE.", call. = FALSE)
  bad_auc <- by_subject$auc_0_tlast > by_subject$auc_0_inf * (1 + auc_tolerance)
  if (any(bad_auc)) {
    stop("Unit or time-scale inconsistency: AUC(0-tlast) exceeds ",
         "AUC(0-inf) for ", sum(bad_auc), " subject(s). Do not ",
         "rescale anything to quiet this; find the mismatch.",
         call. = FALSE)
  }
  bad_cmax <- by_subject$cmax > by_subject$cmax_bound * (1 + auc_tolerance)
  if (any(bad_cmax)) {
    stop("Mass-balance violation: Cmax exceeds amt/V for ",
         sum(bad_cmax), " subject(s); check dose units against the ",
         "declared amt unit.", call. = FALSE)
  }
  by_subject$cmax_bound <- NULL
  by_subject
}

# ---------------------------------------------------------------------------
# reference_exposure_summary(by_subject, metrics, units, probs, quantile_type)
#
# by_subject   output of reference_exposure_by_subject
# metrics      declared metric names (subset of EXPOSURE_METRICS_IMPLEMENTED)
# units        list(conc, time) — declared units, carried on every row
# probs        two probabilities for the interval columns (DECLARED)
# quantile_type  stats::quantile type 1..9 (DECLARED)
#
# Returns data.frame(SCENARIO, metric, unit, n, median, lower, upper, p_lower,
# p_upper), one row per scenario x metric.
reference_exposure_summary <- function(by_subject, metrics, units, probs,
                                       quantile_type) {
  .sim_exposure_gate()
  .exposure_require_probs(probs)
  .exposure_require_quantile_type(quantile_type)
  unknown <- setdiff(metrics, EXPOSURE_METRICS_IMPLEMENTED)
  if (length(unknown))
    stop("exposure metric(s) with no reference definition: ",
         paste(unknown, collapse = ", "), ". Implemented: ",
         paste(EXPOSURE_METRICS_IMPLEMENTED, collapse = ", "),
         ". Do not improvise a definition.", call. = FALSE)
  if (is.null(units$conc) || is.null(units$time))
    stop("units must declare conc and time", call. = FALSE)
  metric_units <- c(cmax        = units$conc,
                    auc_0_tlast = paste(units$conc, "*", units$time),
                    auc_0_inf   = paste(units$conc, "*", units$time))
  out <- do.call(rbind, lapply(
    split(by_subject, by_subject$SCENARIO),
    function(d) do.call(rbind, lapply(metrics, function(m) data.frame(
      SCENARIO = d$SCENARIO[1],
      metric   = m,
      unit     = metric_units[[m]],
      n        = nrow(d),
      median   = stats::median(d[[m]]),
      lower    = unname(stats::quantile(d[[m]], probs[1], type = quantile_type)),
      upper    = unname(stats::quantile(d[[m]], probs[2], type = quantile_type)),
      p_lower  = probs[1],
      p_upper  = probs[2],
      stringsAsFactors = FALSE)))))
  rownames(out) <- NULL
  out
}

# ---------------------------------------------------------------------------
# reference_profile_summary(sim, observed, time, probs, quantile_type)
#
# Per-time median and interval of the observed column across subjects, for
# the concentration-time deliverable figure. Same declared probs and quantile
# type as the metric summary so the figure and the table agree.
#
# Returns data.frame(time, lower, median, upper, n) ordered by time.
reference_profile_summary <- function(sim, observed, time, probs, quantile_type) {
  .sim_exposure_gate()
  .exposure_require_probs(probs)
  .exposure_require_quantile_type(quantile_type)
  miss <- setdiff(c(time, observed), names(sim))
  if (length(miss))
    stop("simulated output is missing column(s): ", paste(miss, collapse = ", "),
         call. = FALSE)
  out <- do.call(rbind, lapply(split(sim, sim[[time]]), function(x) data.frame(
    time   = x[[time]][1],
    lower  = unname(stats::quantile(x[[observed]], probs[1], type = quantile_type)),
    median = stats::median(x[[observed]]),
    upper  = unname(stats::quantile(x[[observed]], probs[2], type = quantile_type)),
    n      = nrow(x))))
  out <- out[order(out$time), ]
  rownames(out) <- NULL
  out
}
