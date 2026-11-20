# REFERENCE IMPLEMENTATION — VPC coverage bookkeeping
#
# AUTHORITY: skills/diagnostics/MANIFEST.yaml is the authority for the SME signature
# (root reviewed_by / reviewed_at) and for the status and digest of unit `vpc_coverage`.
# This header carries no status claim of its own -- read the manifest.
# For each observed percentile and optional stratum, count bins within
# the simulated confidence band and misses on each side. The band is
# inclusive of its endpoints. Missing required columns are an error.
#
# Input contract
#   stats_df  the tidyvpc stats table: columns qname, y, lo, hi (and the
#             stratum column when `strata` is given)
#   strata    NULL, or the name of one stratum column in stats_df
#
# Returns data.frame([stratum, ] percentile, n_bins, n_within_band,
# n_below_band, n_above_band, pct_within), one row per percentile x stratum.


# Self-gate on the intended-use unit "vpc_coverage".
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

.diag_vpc_coverage_gate <- base::local({
  private <- .mm_trusted_private
  function() {
    if (!base::get("require_validated_method", envir = private)(skill_dir = "skills/diagnostics",
                                          calling_file = "skills/diagnostics/reference/vpc_coverage.R",
                                          unit = "vpc_coverage")) {
      base::stop("diagnostics unit 'vpc_coverage' is not SME-validated (manifest unit ",
           "draft/rejected, or the manifest has an empty reviewed_by); VPC ",
           "coverage does not compute until the unit is approved. ESCALATE to ",
           "the SME.", call. = FALSE)
    }
    base::invisible(TRUE)
  }
}, envir = base::new.env())

reference_vpc_coverage <- function(stats_df, strata = NULL) {
  .diag_vpc_coverage_gate()
  if (!is.data.frame(stats_df) || !nrow(stats_df))
    stop("stats_df must be a non-empty tidyvpc stats table.", call. = FALSE)
  need <- c("qname", "y", "lo", "hi", strata)
  miss <- setdiff(need, names(stats_df))
  if (length(miss))
    stop("the VPC stats table is missing column(s): ",
         paste(miss, collapse = ", "),
         ". Coverage is counted against the simulated band, so every one is ",
         "required.", call. = FALSE)
  if (!is.null(strata) && length(strata) != 1L)
    stop("strata names exactly one stratum column, or is NULL.", call. = FALSE)

  # Relocated verbatim from qual_vpc_coverage.
  key <- stats_df[, c(strata, "qname"), drop = FALSE]
  out <- do.call(rbind, lapply(
    split(stats_df, key, drop = TRUE),
    function(d) {
      row <- data.frame(
        percentile = as.character(d$qname[1]),
        n_bins = nrow(d),
        n_within_band = sum(d$y >= d$lo & d$y <= d$hi),
        n_below_band = sum(d$y < d$lo),
        n_above_band = sum(d$y > d$hi),
        stringsAsFactors = FALSE)
      row$pct_within <- round(100 * row$n_within_band / row$n_bins, 1)
      if (!is.null(strata)) {
        row <- cbind(data.frame(stratum = as.character(d[[strata]][1]),
                                stringsAsFactors = FALSE), row)
      }
      row
    }))
  rownames(out) <- NULL
  out
}
