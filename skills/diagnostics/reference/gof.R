# REFERENCE IMPLEMENTATION — Goodness-of-fit panel
#
# AUTHORITY: skills/diagnostics/MANIFEST.yaml is the authority for the SME
# signature (root reviewed_by / reviewed_at) and for the per-unit status and
# digests. This header carries no status claim of its own -- read the manifest.
#
# Panel conventions follow MeRGE Expo 1 and use pmplots.
# Plot defaults:
#   D1 Panel membership: full MeRGE + ETA set — DV~PRED, DV~IPRED,
#      CWRES~PRED, CWRES~TIME, CWRES~TAD, CWRES QQ, |IWRES|~IPRED, plus
#      eta_pairs / eta_cont / eta_cat when eta and covariate names are
#      supplied (covariate panels join at the covariate stage via yspec
#      flags).
#   D2 DV~PRED and DV~IPRED rendered on BOTH linear and log-log scales,
#      always. Non-positive values counted per the eda-standards log-axis
#      audit before the log panels.
#   D3 LOESS smoother on all residual panels (pmplots default retained).
#   D4 Geometry: the plot-integrity rules in skills/diagnostics/SKILL.md
#      are binding — identity line (never regression), shared limits +
#      square aspect on observed-vs-predicted, zero line + ±2 guides on
#      residual panels.
#   D5 Stratification mirrors the VPC defaults: stratified DV~PRED and
#      CWRES~TIME companions by study and by dose group whenever more than
#      one is pooled.
#   D6 Individual prediction profiles: per-subject
#      DV + PRED + IPRED versus time, paginated — pmplots-native
#      dv_pred_ipred (the MeRGE Expo 1 panel), via
#      reference_gof_individual() below.
#   D7 EBE and covariate diagnostics include eta histograms
#      with per-eta shrinkage annotated on the panel
#      (reference_gof_ebe()), CWRES histogram joins the core panel set,
#      and individual-parameter-versus-covariate panels
#      (reference_gof_param_cov()). Shrinkage values are read from the
#      fit, never computed here; EBE-based panels are interpreted with
#      the shrinkage in view — that is why the annotation is mandatory.

# fit       nlmixr2 fit object (or any object with an as.data.frame method
#           yielding DV, PRED, IPRED, IWRES, CWRES, TIME, TAD columns)
# etas      character vector of eta column names (renders eta_pairs)
# cont_cov  continuous covariate columns for eta_cont (covariate stage)
# cat_cov   categorical covariate columns for eta_cat (covariate stage)
# strata    optional column name; renders stratified DV~PRED + CWRES~TIME
#
# Returns a named list of ggplot objects, one per panel. Entries beyond the
# eight-panel core are added only when their inputs are present (IWRES
# column, etas, covariate names, a usable strata column), so the panel count
# is data-dependent; whether the rendered set satisfies D1 is checked at the
# gate, not here. Missing required columns (DV, PRED, CWRES, ...) fail
# inside pmplots rather than being guarded in this function.
# ---------------------------------------------------------------------------
# Self-gate (integrity batch 3b, item C4). The diagnostics reference lanes
# executed UNGATED: validation lived in tools/qualification.R, but a task
# module that calls these functions directly bypassed that runner. Each public
# entry function now gates on its intended-use unit ("gof"), same pattern as
# forest.R. The manifest binds this file's sha256; the gate computes only
# when skills/diagnostics/MANIFEST.yaml carries a confirmed root (non-empty
# reviewed_by) and this unit's status is approved. The legacy "Reviewed by:"
# grep of SKILL.md was retired in batch 3c and is no longer consulted.
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

.gof_gate <- base::local({
  private <- .mm_trusted_private
  function() {
    if (!base::get("require_validated_method", envir = private)(skill_dir = "skills/diagnostics",
                                          calling_file = "skills/diagnostics/reference/gof.R",
                                          unit = "gof")) {
      base::stop("diagnostics unit 'gof' is not SME-validated (manifest unit ",
           "draft/rejected, or skills/diagnostics/MANIFEST.yaml has an empty root ",
           "reviewed_by); ",
           "GOF panels do not compute until the unit is approved. ESCALATE to ",
           "the SME.", call. = FALSE)
    }
    base::invisible(TRUE)
  }
}, envir = base::new.env())

reference_gof_panel <- function(fit, etas = NULL, cont_cov = NULL,
                                cat_cov = NULL, strata = NULL) {
  .gof_gate()
  df <- as.data.frame(fit)
  # nlmixr2 emits time-after-dose as lowercase `tad`; pmplots expects TAD.
  if (!"TAD" %in% names(df) && "tad" %in% names(df)) df$TAD <- df$tad
  out <- list(
    dv_pred        = pmplots::dv_pred(df),
    dv_pred_log    = pmplots::dv_pred(df, loglog = TRUE),
    dv_ipred       = pmplots::dv_ipred(df),
    dv_ipred_log   = pmplots::dv_ipred(df, loglog = TRUE),
    cwres_pred     = pmplots::cwres_pred(df),
    cwres_time     = pmplots::cwres_time(df),
    cwres_tad      = pmplots::cwres_tad(df),
    cwres_q        = pmplots::cwres_q(df),
    cwres_hist     = pmplots::cwres_hist(df)   # D7 (MeRGE core member)
  )
  # pmplots 0.5.1 exports no iwres helper, so the |IWRES|~IPRED panel (D1)
  # is built from the generic pm_scatter. The "col//label" strings are
  # pmplots' column//axis-label spec: ABSIWRES is a derived column whose
  # axis is labeled |IWRES|.
  if ("IWRES" %in% names(df)) {
    df$ABSIWRES <- abs(df$IWRES)
    out$absiwres_ipred <- pmplots::pm_scatter(df, x = "IPRED//IPRED",
                                              y = "ABSIWRES//|IWRES|")
  }
  if (!is.null(etas)) {
    # !duplicated(ID) keeps one row per subject: eta panels must weight each
    # subject once, not once per observation record.
    out$eta_pairs <- pmplots::eta_pairs(df[!duplicated(df$ID), ], etas)
    if (!is.null(cont_cov)) {
      out$eta_cont <- pmplots::eta_cont(df[!duplicated(df$ID), ],
                                        x = cont_cov, y = etas)
    }
    if (!is.null(cat_cov)) {
      out$eta_cat <- pmplots::eta_cat(df[!duplicated(df$ID), ],
                                      x = cat_cov, y = etas)
    }
  }
  # Stratified companions per D5. The guard requires the declared strata
  # column to exist with more than one level — i.e. more than one group is
  # actually pooled; a single-level column would only duplicate the pooled
  # panels. The stratum value is embedded in each list name so the gate
  # package can label the panels.
  if (!is.null(strata) && strata %in% names(df) &&
      length(unique(df[[strata]])) > 1) {
    for (lev in unique(df[[strata]])) {
      d <- df[df[[strata]] == lev, ]
      out[[paste0("dv_pred_", strata, "_", lev)]]    <- pmplots::dv_pred(d)
      out[[paste0("cwres_time_", strata, "_", lev)]] <- pmplots::cwres_time(d)
    }
  }
  out
}

# ---------------------------------------------------------------------------
# Individual prediction profiles (house decision D6, amendment 2026-08-30).
#
# pmplots::dv_pred_ipred — the MeRGE Expo 1 individual-profiles panel:
# per-subject DV points with PRED and IPRED lines versus time, faceted by
# subject, paginated. Returns the helper's list of page plots.
#
# fit           nlmixr2 fit object (or any object whose as.data.frame yields
#               ID, TIME, DV, PRED, IPRED)
# ids_per_page  subjects per page (default 9)
# log_y         also return log-scale pages (rows with DV <= 0 dropped for
#               those pages; count them per the eda-standards log-axis audit)
#
# NOTE: pmplots' paginated helpers assume ggplot2 is ATTACHED (they call
# ylab() etc. unqualified); callers library(ggplot2) before using this.
reference_gof_individual <- function(fit, ids_per_page = 9, log_y = FALSE) {
  .gof_gate()
  df <- as.data.frame(fit)
  need <- c("ID", "TIME", "DV", "PRED", "IPRED")
  missing_cols <- setdiff(need, names(df))
  if (length(missing_cols)) {
    stop("Individual-profile panel needs column(s): ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  out <- pmplots::dv_pred_ipred(df, id_per_plot = ids_per_page)
  names(out) <- paste0("ind_profiles_", seq_along(out))
  if (isTRUE(log_y)) {
    pos <- df[df$DV > 0, ]
    logs <- pmplots::dv_pred_ipred(pos, id_per_plot = ids_per_page,
                                   fun = function(p)
                                     p + ggplot2::scale_y_log10())
    names(logs) <- paste0("ind_profiles_log_", seq_along(logs))
    out <- c(out, logs)
  }
  out
}

# ---------------------------------------------------------------------------
# EBE / shrinkage diagnostics (house decision D7, amendment 2026-08-30).
#
# Eta histograms, one row per subject, with the per-eta shrinkage printed
# on the panel — EBE-based diagnostics are read with the shrinkage in view,
# so the annotation is mandatory: `shrinkage` is a named numeric vector
# (percent, names matching `etas`) READ FROM THE FIT by the caller (e.g.
# fit$parFixedDf or the run record), never computed here.
#
# Returns list(eta_hist = <plot>, shrinkage = <data.frame eta/shrinkage_pct>)
# — the table renders beside the panel in the gate package.
reference_gof_ebe <- function(fit, etas, shrinkage) {
  .gof_gate()
  stopifnot(length(etas) > 0, !missing(shrinkage),
            all(etas %in% names(shrinkage)))
  df <- as.data.frame(fit)
  one <- df[!duplicated(df$ID), ]
  labs <- sprintf("%s (shrinkage %.1f%%)", etas, shrinkage[etas])
  p <- pmplots::eta_hist(one, paste0(etas, "//", labs))
  if (is.list(p) && !inherits(p, "gg")) {
    p <- patchwork::wrap_plots(p)
  }
  list(eta_hist = p,
       shrinkage = data.frame(eta = etas,
                              shrinkage_pct = as.numeric(shrinkage[etas])))
}

# ---------------------------------------------------------------------------
# Individual-parameter versus covariate panels (house decision D7).
#
# Empirical-Bayes individual parameters (e.g. CL_i, V_i — columns already
# on the fit output) against covariates: scatter + smoother for continuous
# covariates, box/jitter for categorical. One row per subject.
#
# Serves EBEs (eta columns) with the same geometry (SME, 2026-08-30):
# pass eta columns in `params` (e.g. c(eta.cl = "EBE, clearance")) for
# eta-versus-covariate panels in this style. The two families read
# differently: trends in PARAMETER-vs-covariate mean the implemented
# structural relation is wrong or incomplete; trends in ETA-vs-covariate
# mean unexplained variability remains. Label panels so the reader knows
# which family they are looking at, and read both with the shrinkage
# annotation (reference_gof_ebe) in view.
#
# params    named character vector: column name -> axis label with unit
#           (e.g. c(CL = "Individual clearance (L/h)"))
# cont_cov  named character vector for continuous covariates, same shape
# cat_cov   named character vector for categorical covariates (factors or
#           coercible)
#
# Returns a named list of ggplot objects, one per parameter-covariate pair.
reference_gof_param_cov <- function(fit, params, cont_cov = NULL,
                                    cat_cov = NULL) {
  .gof_gate()
  stopifnot(length(params) > 0, !is.null(names(params)))
  df <- as.data.frame(fit)
  one <- df[!duplicated(df$ID), ]
  missing_cols <- setdiff(c(names(params), names(cont_cov), names(cat_cov)),
                          names(one))
  if (length(missing_cols)) {
    stop("Parameter/covariate column(s) not on the fit output: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  out <- list()
  for (p in names(params)) {
    for (cv in names(cont_cov)) {
      out[[paste0(p, "_vs_", cv)]] <- pmplots::pm_scatter(
        one, x = paste0(cv, "//", cont_cov[[cv]]),
        y = paste0(p, "//", params[[p]]))
    }
    for (cv in names(cat_cov)) {
      d <- one
      d[[cv]] <- factor(d[[cv]])
      out[[paste0(p, "_vs_", cv)]] <-
        ggplot2::ggplot(d, ggplot2::aes(x = .data[[cv]],
                                        y = .data[[p]])) +
        ggplot2::geom_boxplot(outlier.shape = NA) +
        ggplot2::geom_jitter(width = 0.1, alpha = 0.5) +
        ggplot2::labs(x = cat_cov[[cv]], y = params[[p]]) +
        ggplot2::theme_bw()
    }
  }
  out
}
