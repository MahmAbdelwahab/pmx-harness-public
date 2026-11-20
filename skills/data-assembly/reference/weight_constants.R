# REFERENCE IMPLEMENTATION — fixed weight constants (median + percentiles)
#
# AUTHORITY: skills/data-assembly/MANIFEST.yaml is the authority for the SME signature
# (root reviewed_by / reviewed_at) and for the status and digest of unit `weight_constants`.
# This header carries no status claim of its own -- read the manifest.
#
# Compute the median and two declared quantiles of a baseline covariate.
# The caller declares the probabilities and the quantile type.


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

.weight_constants_gate <- base::local({
  private <- .mm_trusted_private
  function() {
    if (!base::get("require_validated_method", envir = private)(
          skill_dir    = "skills/data-assembly",
          calling_file = "skills/data-assembly/reference/weight_constants.R",
          unit         = "weight_constants")) {
      base::stop("data-assembly unit 'weight_constants' is not SME-validated ",
           "(manifest unit draft/rejected); the weight constants do not compute ",
           "until the unit is approved. ESCALATE to the SME.", call. = FALSE)
    }
    base::invisible(TRUE)
  }
}, envir = base::new.env())

# Median and two percentiles of a numeric vector.
#
#   values          numeric vector (e.g. one baseline weight per subject).
#   probs           the two percentile probabilities; DECLARED, no default.
#                   length 2, 0 < probs[1] < probs[2] < 1.
#   quantile_type   the stats::quantile `type`; DECLARED, no default.
#
# Returns list(median, lower, upper, probs, quantile_type). The quantiles use
# stats::quantile(type = quantile_type, names = FALSE) — the same call the
# pilot used.
reference_weight_constants <- function(values, probs, quantile_type) {
  .weight_constants_gate()
  if (!is.numeric(values) || !length(values) || any(!is.finite(values)))
    stop("values must be a non-empty finite numeric vector.", call. = FALSE)
  if (missing(probs) || !is.numeric(probs) || length(probs) != 2L ||
      any(!is.finite(probs)) || !all(probs > 0 & probs < 1) ||
      probs[1] >= probs[2])
    stop("probs is a declared parameter (no default): two probabilities with ",
         "0 < probs[1] < probs[2] < 1.", call. = FALSE)
  if (missing(quantile_type) || !is.numeric(quantile_type) ||
      length(quantile_type) != 1L || !is.finite(quantile_type))
    stop("quantile_type is a declared parameter (no default).", call. = FALSE)
  q <- stats::quantile(values, probs = probs, type = quantile_type,
                       names = FALSE)
  list(median = stats::median(values), lower = q[1], upper = q[2],
       probs = probs, quantile_type = quantile_type)
}
