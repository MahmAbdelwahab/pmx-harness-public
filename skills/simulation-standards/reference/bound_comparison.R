# REFERENCE IMPLEMENTATION -- Percentile-against-bound comparison for the
# simulation criteria
#
# AUTHORITY: skills/simulation-standards/MANIFEST.yaml is the authority for the SME signature
# (root reviewed_by / reviewed_at) and for the status and digest of unit `bound_comparison`.
# This header carries no status claim of its own -- read the manifest.
# Compare prespecified bounds and percentiles for local gate review.
#   verdict_lower  PASS when the lower percentile is at least the declared
#                  lower bound, a value equal to the bound included;
#                  otherwise FAIL.
#   verdict_upper  PASS when the upper percentile is at most the declared
#                  upper bound, a value equal to the bound included;
#                  otherwise FAIL.
#   verdict        PASS when both sides are PASS; otherwise FAIL.
# The comparison is on the numbers given, at full double precision: no
# tolerance, no rounding, no margin and no unit conversion. The declared
# bounds already carry the margin M of MAP 4.3.3 as a single number, so the
# unit does no arithmetic on a bound. Any missing, non-numeric, mismatched,
# empty or non-finite input, or a lower bound not below its upper bound,
# stops the whole call, naming the argument and the first offending
# position; no row gets a verdict. There is no default for any argument.
#
# WHAT THIS UNIT DOES NOT DECIDE: the decision rule of MAP 4.3.3 (c) (which
# dose pair is chosen), the representativeness objective O1, and any
# disposition under MAP 4.3.4. A disposition sits beside a computed verdict,
# never in its place, and a computed verdict is never rewritten.
#

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

sim_bound_validated <- base::local({
  private <- .mm_trusted_private
  function() {
    base::get("require_validated_method", envir = private)(
      skill_dir    = "skills/simulation-standards",
      calling_file = "skills/simulation-standards/reference/bound_comparison.R",
      unit         = "bound_comparison")
  }
}, envir = base::new.env())

.sim_bound_gate <- function() {
  if (!sim_bound_validated()) {
    base::stop("simulation-standards unit 'bound_comparison' is not SME-validated ",
         "(manifest unit draft/rejected, or the manifest has an empty ",
         "reviewed_by); the percentile-against-bound verdicts do not compute ",
         "until the unit is approved. ESCALATE to the SME.", call. = FALSE)
  }
  base::invisible(TRUE)
}

# ---------------------------------------------------------------------------
# reference_bound_verdict(lower, upper, bound_lower, bound_upper)
#
# lower        the lower percentile of each bound-table row
# upper        the upper percentile of each bound-table row
# bound_lower  the declared lower bound of that row's metric
# bound_upper  the declared upper bound of that row's metric
# Four numeric vectors of one common length n of at least 1; position i of
# each is row i of the bound table. The caller passes the table's columns as
# they are.
#
# Returns data.frame(verdict_lower, verdict_upper, verdict): n rows in input
# order, three character columns, each cell exactly "PASS" or "FAIL".
reference_bound_verdict <- function(lower, upper, bound_lower, bound_upper) {
  .sim_bound_gate()
  args <- c("lower", "upper", "bound_lower", "bound_upper")
  absent <- args[c(missing(lower), missing(upper), missing(bound_lower), missing(bound_upper))]
  if (length(absent))
    stop("reference_bound_verdict: argument '", absent[1L], "' is missing; it has no default.",
         call. = FALSE)
  x <- list(lower = lower, upper = upper, bound_lower = bound_lower,
            bound_upper = bound_upper)
  for (a in args)
    if (!is.numeric(x[[a]]) || !is.null(dim(x[[a]])))
      stop("reference_bound_verdict: argument '", a, "' must be a numeric vector.",
           call. = FALSE)
  n <- length(lower)
  if (n == 0L)
    stop("reference_bound_verdict: argument 'lower' has length 0; at least one ",
         "bound-table row is required.", call. = FALSE)
  for (a in args[-1L])
    if (length(x[[a]]) != n)
      stop("reference_bound_verdict: argument '", a, "' has a length different from ",
           "'lower'; the first position without a counterpart is ",
           min(length(x[[a]]), n) + 1L, ".", call. = FALSE)
  for (a in args) {
    bad <- which(!is.finite(x[[a]]))
    if (length(bad))
      stop("reference_bound_verdict: argument '", a, "' is NA, NaN or infinite at ",
           "position ", bad[1L], ".", call. = FALSE)
  }
  bad <- which(!(bound_lower < bound_upper))
  if (length(bad))
    stop("reference_bound_verdict: argument 'bound_lower' is not below 'bound_upper' ",
         "at position ", bad[1L], ".", call. = FALSE)
  pass_lower <- as.vector(lower >= bound_lower)
  pass_upper <- as.vector(upper <= bound_upper)
  data.frame(verdict_lower = ifelse(pass_lower, "PASS", "FAIL"),
             verdict_upper = ifelse(pass_upper, "PASS", "FAIL"),
             verdict = ifelse(pass_lower & pass_upper, "PASS", "FAIL"),
             stringsAsFactors = FALSE)
}
