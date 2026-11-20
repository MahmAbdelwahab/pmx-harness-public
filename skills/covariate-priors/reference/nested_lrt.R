# REFERENCE IMPLEMENTATION — likelihood-ratio test between nested models
#
# AUTHORITY: skills/covariate-priors/MANIFEST.yaml is the authority for the SME signature
# (root reviewed_by / reviewed_at) and for the status and digest of unit `nested_lrt`.
# This header carries no status claim of its own -- read the manifest.
#
# The test: ΔOFV = OFV(parent) - OFV(child). Under the nested hypothesis
# -2 log-likelihood difference is chi-square with `df` degrees of freedom; the
# child is retained iff ΔOFV >= criterion_dofv (the chi-square quantile for the
# declared alpha and df). Significance is the sole retention criterion
# (covariate-priors SKILL.md).


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

.nested_lrt_gate <- base::local({
  private <- .mm_trusted_private
  function() {
    if (!base::get("require_validated_method", envir = private)(
          skill_dir    = "skills/covariate-priors",
          calling_file = "skills/covariate-priors/reference/nested_lrt.R",
          unit         = "nested_lrt")) {
      base::stop("covariate-priors unit 'nested_lrt' is not SME-validated (manifest ",
           "unit draft/rejected); the nested LRT does not compute until the ",
           "unit is approved. ESCALATE to the SME.", call. = FALSE)
    }
    base::invisible(TRUE)
  }
}, envir = base::new.env())

# Nested likelihood-ratio test.
#
#   ofv_parent, ofv_child   the two objective-function values. Pass NA for a
#                           failed/non-converged run; the test is then not
#                           evaluable and ΔOFV is NA (retained = FALSE).
#   criterion_dofv          the ΔOFV retention threshold (declared; the
#                           chi-square quantile for alpha and df). No default.
#   df, alpha               the declared degrees of freedom and significance
#                           level carried through for the record. No default.
#
# Returns a list: dofv, significant, retained, criterion_dofv, df, alpha.
# `significant` and `retained` are identical here (significance is the sole
# retention criterion); both are returned so a caller that later separates
# them does not have to recompute.
reference_nested_lrt <- function(ofv_parent, ofv_child, criterion_dofv,
                                 df, alpha) {
  .nested_lrt_gate()
  num1 <- function(x) is.numeric(x) && length(x) == 1L
  if (!num1(ofv_parent) || !num1(ofv_child))
    stop("ofv_parent and ofv_child must each be a single number (NA for a ",
         "failed run).", call. = FALSE)
  if (missing(criterion_dofv) || !num1(criterion_dofv) ||
      !is.finite(criterion_dofv))
    stop("criterion_dofv is a declared parameter (no default): the ΔOFV ",
         "retention threshold.", call. = FALSE)
  if (missing(df) || !num1(df) || !is.finite(df) ||
      missing(alpha) || !num1(alpha) || !is.finite(alpha))
    stop("df and alpha are declared parameters (no default).", call. = FALSE)
  dofv <- if (is.na(ofv_parent) || is.na(ofv_child)) NA_real_
          else ofv_parent - ofv_child
  significant <- !is.na(dofv) && dofv >= criterion_dofv
  list(dofv = dofv, significant = significant, retained = significant,
       criterion_dofv = criterion_dofv, df = df, alpha = alpha)
}
