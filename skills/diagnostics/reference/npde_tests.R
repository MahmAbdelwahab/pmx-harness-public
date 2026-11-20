# REFERENCE IMPLEMENTATION — NPDE global hypothesis tests
#
# AUTHORITY: skills/diagnostics/MANIFEST.yaml is the authority for the SME signature
# (root reviewed_by / reviewed_at) and for the status and digest of unit `npde_tests`.
# This header carries no status claim of its own -- read the manifest.
# Use a one-sample t-test of mean zero and a two-sided chi-squared test
# of variance one. Report flags using the explicitly supplied threshold.
#
# Input contract
#   values  numeric, the NPDE values of every observation record in the panel
#   p_flag  single finite number in (0, 1); the MAP's flag threshold
#
# Returns data.frame(test, n, observed, statistic, p_value, flag_threshold_p,
# flagged) with one row per global test, in the fixed order below.


# Self-gate on the intended-use unit "npde_tests" (same pattern as the other
# diagnostics units).
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

.diag_npde_tests_gate <- base::local({
  private <- .mm_trusted_private
  function() {
    if (!base::get("require_validated_method", envir = private)(skill_dir = "skills/diagnostics",
                                          calling_file = "skills/diagnostics/reference/npde_tests.R",
                                          unit = "npde_tests")) {
      base::stop("diagnostics unit 'npde_tests' is not SME-validated (manifest unit ",
           "draft/rejected, or the manifest has an empty reviewed_by); the ",
           "global NPDE tests do not compute until the unit is approved. ",
           "ESCALATE to the SME.", call. = FALSE)
    }
    base::invisible(TRUE)
  }
}, envir = base::new.env())

reference_npde_global_tests <- function(values, p_flag) {
  .diag_npde_tests_gate()
  if (missing(p_flag) || !is.numeric(p_flag) || length(p_flag) != 1L ||
      !is.finite(p_flag) || p_flag <= 0 || p_flag >= 1)
    stop("p_flag is a declared analysis-plan parameter with no default: it is ",
         "the MAP's flag threshold and must be a single finite number in ",
         "(0, 1).", call. = FALSE)
  if (!is.numeric(values) || !length(values) || !all(is.finite(values)))
    stop("values must be a numeric vector of finite NPDE values; a missing or ",
         "non-finite NPDE is a data defect, not a value to test around.",
         call. = FALSE)
  n <- length(values)
  if (n < 2L)
    stop("the global NPDE tests need at least 2 values; got ", n, ".",
         call. = FALSE)

  # Relocated verbatim from qual_npde_global_tests.
  tt <- stats::t.test(values, mu = 0)
  x2 <- (n - 1) * stats::var(values)   # H0: variance = 1
  p_var <- min(1, 2 * min(stats::pchisq(x2, df = n - 1),
                          stats::pchisq(x2, df = n - 1, lower.tail = FALSE)))
  data.frame(
    test = c("global mean = 0 (one-sample t)",
             "global variance = 1 (two-sided chi-squared)"),
    n = n,
    observed = signif(c(mean(values), stats::var(values)), 4),
    statistic = signif(c(unname(tt$statistic), x2), 4),
    # Keep machine-consumed p-values at full precision. Reporting code owns
    # display rounding; qualification compares this column to the MAP limit.
    p_value = c(tt$p.value, p_var),
    flag_threshold_p = p_flag,
    flagged = c(tt$p.value, p_var) < p_flag,
    stringsAsFactors = FALSE)
}
