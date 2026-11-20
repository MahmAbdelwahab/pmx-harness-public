# REFERENCE IMPLEMENTATION — Normalized prediction distribution errors
#
# AUTHORITY: skills/diagnostics/MANIFEST.yaml is the authority for the SME
# signature (root reviewed_by / reviewed_at) and for the per-unit status and
# digests. This header carries no status claim of its own -- read the manifest.
#
# nlmixr2est::addNpde computes NPDE using the Brendel/Comets method.
# Plotting uses pmplots. Defaults:
#   D1 nsim minimum 500; the MAP may raise it, never go below.
#   D2 Decorrelation: nlmixr2 addNpde engine default (Cholesky), recorded
#      verbatim (method, nsim, seed) in the run record. Revisit only on
#      decorrelation artifacts.
#   D3 Mandatory panels: NPDE vs PRED, vs TIME, vs TAD, histogram with
#      N(0,1) overlay, QQ. Covariate NPDE panels (npde_cont/npde_cat) join
#      at the covariate stage.
#   D4 BLQ excluded from NPDE computation; the BLQ count/fraction is
#      stated in the panel caption. Censoring adequacy is judged by the
#      VPC %BLQ panel (always rendered when BLQ exist).
#
# Engine caveat (verified 2026-08-28): nlmixr2est::addNpde() fails with
# "parameter(s) are required for solving" on a fit loaded back from a
# targets store; it works on the in-session fit. Compute NPDE in the same
# target that runs the estimation, or refit before calling this.

# Computes NPDE on an in-session fit (engine caveat above) and renders the
# D3 mandatory panel set.
#
# fit   nlmixr2 fit object, in the same session that estimated it
# nsim  replicates for the NPDE reference distribution; default sits at the
#       D1 house floor
# seed  mandatory, no default — persisted in `record`
#
# Returns a list of the five D3 panels (ggplot objects) plus `record`, the
# verbatim run record D2 requires (engine, decorrelation method, nsim, seed,
# captured warnings). Stops when seed is missing or nsim is below the D1
# floor of 500 — it never lowers nsim to make progress.
# ---------------------------------------------------------------------------
# Self-gate (integrity batch 3b, item C4). The NPDE lane executed UNGATED; a
# task module calling it directly bypassed the qualification runner's
# validation. The public entry now gates on its intended-use unit ("npde"),
# same pattern as forest.R. The gate computes only when
# skills/diagnostics/MANIFEST.yaml carries a confirmed root (non-empty
# reviewed_by) and this unit's status is approved; the legacy "Reviewed by:"
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

.npde_gate <- base::local({
  private <- .mm_trusted_private
  function() {
    if (!base::get("require_validated_method", envir = private)(skill_dir = "skills/diagnostics",
                                          calling_file = "skills/diagnostics/reference/npde.R",
                                          unit = "npde")) {
      base::stop("diagnostics unit 'npde' is not SME-validated (manifest unit ",
           "draft/rejected, or skills/diagnostics/MANIFEST.yaml has an empty root ",
           "reviewed_by); ",
           "NPDE does not compute until the unit is approved. ESCALATE to the SME.",
           call. = FALSE)
    }
    base::invisible(TRUE)
  }
}, envir = base::new.env())

reference_npde_minimum_nsim <- function() {
  .npde_gate()
  500
}

reference_npde <- function(fit, nsim = NULL, seed) {
  .npde_gate()
  stopifnot(!missing(seed))
  minimum_nsim <- reference_npde_minimum_nsim()
  if (is.null(nsim)) nsim <- minimum_nsim
  if (nsim < minimum_nsim) {
    stop("nsim below the house minimum of ", minimum_nsim,
         " (SME decision 2026-08-28); ",
         "raise nsim or record a deviation.", call. = FALSE)
  }
  if (!exists("harness_seed", mode = "function")) source(file.path("tools", "seed.R"), local = TRUE)
  harness_seed(seed)
  warns <- character(0)
  # Warnings are captured verbatim for the D2 run record (decorrelation
  # fallbacks surface as warnings) and muffled so they cannot be lost in the
  # pipeline log; withCallingHandlers keeps evaluation going, unlike
  # tryCatch. updateObject = FALSE makes addNpde return an augmented copy
  # instead of mutating the caller's fit object.
  f <- withCallingHandlers(
    nlmixr2est::addNpde(fit, updateObject = FALSE, nsim = nsim),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  df <- as.data.frame(f)
  # nlmixr2 emits time-after-dose as lowercase `tad`; pmplots expects TAD.
  if (!"TAD" %in% names(df) && "tad" %in% names(df)) df$TAD <- df$tad
  list(
    npde_pred = pmplots::npde_pred(df),
    npde_time = pmplots::npde_time(df),
    npde_tad  = pmplots::npde_tad(df),
    npde_hist = pmplots::npde_hist(df),
    npde_q    = pmplots::npde_q(df),
    record    = list(engine = "nlmixr2 addNpde",
                     decorrelation = "engine default (Cholesky)",
                     nsim = nsim, seed = seed,
                     warnings = warns)  # decorrelation fallbacks surface here
  )
}
