# REFERENCE IMPLEMENTATION — Subject-level weight bands for a stratified VPC
#
# AUTHORITY: skills/diagnostics/MANIFEST.yaml is the authority for the SME signature
# (root reviewed_by / reviewed_at) and for the status and digest of unit `vpc_strata`.
# This header carries no status claim of its own -- read the manifest.
# Compute subject-level weight bands for VPC strata from ID and WT.
# Quantile probabilities and quantile type are caller-overridable.
#
# Input contract
#   fit_data       data.frame with ID and WT (one WT per subject; the first row
#                  of each subject is used)
#   probs          cut probabilities (default: tertiles)
#   quantile_type  stats::quantile type (default 7, the house default)
#
# Returns list(subject = data.frame(ID, WTBAND), cuts_kg, quantile_type,
# summary = data.frame(weight_band, n_subjects, wt_min_kg, wt_max_kg)).


# Self-gate on the intended-use unit "vpc_strata": the private, locked
# validator environment of npde_tests.R (integrity batches 11 to 13; the
# construction is explained there).
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

.diag_vpc_strata_gate <- base::local({
  private <- .mm_trusted_private
  function() {
    if (!base::get("require_validated_method", envir = private)(skill_dir = "skills/diagnostics",
                                          calling_file = "skills/diagnostics/reference/vpc_strata.R",
                                          unit = "vpc_strata")) {
      base::stop("diagnostics unit 'vpc_strata' is not SME-validated (manifest unit ",
           "draft/rejected, or the manifest has an empty reviewed_by); the ",
           "weight bands do not compute until the unit is approved. ",
           "ESCALATE to the SME.", call. = FALSE)
    }
    base::invisible(TRUE)
  }
}, envir = base::new.env())

reference_weight_bands <- function(fit_data, probs = c(1/3, 2/3), quantile_type = 7) {
  .diag_vpc_strata_gate()
  if (!is.data.frame(fit_data) || !all(c("ID", "WT") %in% names(fit_data)))
    stop("fit_data must be a data.frame with ID and WT columns.", call. = FALSE)
  # Moved over unchanged from qual_wt_bands (qualify.R lines 169-182).
  s <- fit_data[!duplicated(fit_data$ID), c("ID", "WT")]
  cuts <- unname(stats::quantile(s$WT, probs, type = quantile_type))
  labels <- c(sprintf("WT <= %.2f kg", cuts[1]),
              sprintf("%.2f < WT <= %.2f kg", cuts[1], cuts[2]),
              sprintf("WT > %.2f kg", cuts[2]))
  band <- cut(s$WT, breaks = c(-Inf, cuts, Inf), labels = labels)
  summary <- do.call(rbind, lapply(labels, function(l) {
    w <- s$WT[band == l]
    data.frame(weight_band = l, n_subjects = length(w),
               wt_min_kg = min(w), wt_max_kg = max(w),
               stringsAsFactors = FALSE)
  }))
  list(subject = data.frame(ID = s$ID, WTBAND = band),
       cuts_kg = cuts, quantile_type = quantile_type, summary = summary)
}
