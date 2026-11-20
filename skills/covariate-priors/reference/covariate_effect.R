# REFERENCE IMPLEMENTATION — covariate-effect exposure ratio and band test
#
# AUTHORITY: skills/covariate-priors/MANIFEST.yaml is the authority for the SME signature
# (root reviewed_by / reviewed_at) and for the status and digest of unit `covariate_effect`.
# This header carries no status claim of its own -- read the manifest.
#
# Validation record (SME fills in):
#   Reviewed by:
#   Date:
#   Source of truth: covariate-priors SKILL.md, "Body size — allometric
#     policy": under fixed or estimated allometry AUCss scales as
#     WT^(1 - theta), so the exposure ratio between two weight percentiles is
#     r = (WT_high / WT_low)^(1 - theta). This is a covariate-effect
#     magnitude, which is why it lives in covariate-priors.
#
# WHY THIS EXISTS (F12, Codex round 8). pipeline/tasks/reporting.R computed
# the QI2 exposure ratio and its consistency-band test inline:
#   r <- (wt95 / wt05)^(1 - theta); within_band <- r >= 0.80 & r <= 1.25
# That is analysis arithmetic authored outside a reference unit, contrary to
# the one rule (the LLM proposes, deterministic tools compute). The arithmetic
# is relocated here BYTE-FOR-BYTE; the band limits are DECLARED parameters (no
# 0.80/1.25 default), and the report task calls this function.
#
# The reference computes the numbers only; the calling task attaches the
# basis labels, the source weights and the exponent provenance.


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

.covariate_effect_gate <- base::local({
  private <- .mm_trusted_private
  function() {
    if (!base::get("require_validated_method", envir = private)(
          skill_dir    = "skills/covariate-priors",
          calling_file = "skills/covariate-priors/reference/covariate_effect.R",
          unit         = "covariate_effect")) {
      base::stop("covariate-priors unit 'covariate_effect' is not SME-validated ",
           "(manifest unit draft/rejected); the exposure-ratio band test does ",
           "not compute until the unit is approved. ESCALATE to the SME.",
           call. = FALSE)
    }
    base::invisible(TRUE)
  }
}, envir = base::new.env())

# Exposure ratio between two covariate values under an allometric (power)
# effect, with the consistency-band test.
#
#   wt_low, wt_high   the two covariate values (e.g. the 5th and 95th weight
#                     percentiles); wt_high > wt_low > 0.
#   exponents         numeric vector of allometric exponents theta (the primary
#                     MAP-fixed value and any sensitivity exponents).
#   band_lower,       the consistency band [band_lower, band_upper]; DECLARED
#   band_upper        parameters, no default. band_lower < band_upper, both > 0.
#
# Returns a data.frame with one row per exponent:
#   exponent, r = (wt_high/wt_low)^(1 - exponent),
#   within_band = r >= band_lower & r <= band_upper,
#   band_lower, band_upper.
reference_covariate_exposure_ratio <- function(wt_low, wt_high, exponents,
                                               band_lower, band_upper) {
  .covariate_effect_gate()
  if (!is.numeric(wt_low) || length(wt_low) != 1L || !is.finite(wt_low) ||
      wt_low <= 0)
    stop("wt_low must be a single positive number.", call. = FALSE)
  if (!is.numeric(wt_high) || length(wt_high) != 1L || !is.finite(wt_high) ||
      wt_high <= wt_low)
    stop("wt_high must be a single number greater than wt_low.", call. = FALSE)
  if (!is.numeric(exponents) || !length(exponents) || any(!is.finite(exponents)))
    stop("exponents must be a non-empty finite numeric vector.", call. = FALSE)
  if (missing(band_lower) || missing(band_upper) ||
      !is.numeric(band_lower) || !is.numeric(band_upper) ||
      length(band_lower) != 1L || length(band_upper) != 1L ||
      !is.finite(band_lower) || !is.finite(band_upper) ||
      band_lower <= 0 || band_upper <= band_lower)
    stop("band_lower and band_upper are declared parameters (no default): ",
         "supply 0 < band_lower < band_upper.", call. = FALSE)
  r <- (wt_high / wt_low)^(1 - exponents)
  data.frame(exponent = exponents,
             r = r,
             within_band = r >= band_lower & r <= band_upper,
             band_lower = band_lower,
             band_upper = band_upper,
             stringsAsFactors = FALSE)
}
