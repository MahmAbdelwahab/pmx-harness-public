# REFERENCE IMPLEMENTATION — Bootstrap parameter uncertainty
#
# AUTHORITY: skills/diagnostics/MANIFEST.yaml is the authority for the SME
# signature (root reviewed_by / reviewed_at) and for the per-unit status and
# digests. This header carries no status claim of its own -- read the manifest.
#
# nlmixr2extra::bootstrapFit supplies the fit summary and the cached
# replicate results. Settings and output handling:
#   D1 nboot minimum by MODEL_RISK: 500 at medium, 1000 at high (bootstrap
#      is battery-mandated at medium+ only).
#   D2 Stratified resampling: no house default — declared per analysis by
#      the SME in the MAP (bootstrapFit `stratVar` passes it through).
#   D3 Non-converged replicates: excluded from CIs; the bootSummary
#      message/warnings are reported verbatim in the gate package; a
#      failure rate above 10% flags the bootstrap as unreliable and
#      ESCALATEs (usually a model-stability signal, not a bootstrap
#      problem).
#   D4 Confidence intervals: percentile (bootstrapFit default stdErrType).
#   D5 SIR is the declared alternative when bootstrap is computationally
#      infeasible or the failure rate exceeds the 10% flag; choice and
#      rationale recorded in the MAP.
#
# Engine notes (verified 2026-08-28):
#   - `fitName` is mandatory in this harness: without it bootstrapFit
#     derives its cache directory from the deparsed model function and
#     crashes ("the condition has length > 1").
#   - The replicate cache directory is machine-local scratch, git-ignored;
#     the audit artifact is the returned record, rendered into the gate
#     package.

# Runs the bootstrap and packages the results for the gate.
#
# fit         nlmixr2 fit object — MUTATED as a side effect: bootstrapFit
#             writes bootSummary and bootCovMatrix into fit$env (see source
#             of truth above); the results are read back from there
# nboot       replicates; when omitted, uses the D1 floor for `model_risk`;
#             an explicit value is checked against that same floor
# seed, name  mandatory, no defaults; `name` is the fitName the engine
#             requires (engine note above) and keys the replicate cache dir
# model_risk  selects the D1 floor: 500 at medium, 1000 at high
# strat_var   optional stratified-resampling column, declared per-MAP (D2)
# ci          interval level for the percentile CIs (D4)
#
# Returns a list: summary and cov_matrix from fit$env, the verbatim
# message/warnings D3 requires in the gate package, and a reproducibility
# record. It deliberately does NOT compute the replicate failure rate or
# apply the D3 10% escalation — that judgment happens at the gate, from the
# verbatim message/warnings. Per-replicate estimates stay in the local
# cache directory and are not returned. Stops when seed or name is missing,
# or nboot is below the D1 floor — it never lowers nboot to make progress.
# ---------------------------------------------------------------------------
# Self-gate (integrity batch 3b, item C4). The bootstrap lane executed UNGATED;
# a task module calling it directly bypassed the qualification runner's
# validation. The public entry now gates on its intended-use unit
# ("bootstrap"), same pattern as forest.R. The gate computes only when
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

.diag_bootstrap_gate <- base::local({
  private <- .mm_trusted_private
  function() {
    if (!base::get("require_validated_method", envir = private)(skill_dir = "skills/diagnostics",
                                          calling_file = "skills/diagnostics/reference/bootstrap.R",
                                          unit = "bootstrap")) {
      base::stop("diagnostics unit 'bootstrap' is not SME-validated (manifest unit ",
           "draft/rejected, or skills/diagnostics/MANIFEST.yaml has an empty root ",
           "reviewed_by); ",
           "bootstrap does not compute until the unit is approved. ESCALATE to ",
           "the SME.", call. = FALSE)
    }
    base::invisible(TRUE)
  }
}, envir = base::new.env())

reference_bootstrap_minimum_nboot <- function(model_risk = c("medium", "high")) {
  .diag_bootstrap_gate()
  model_risk <- match.arg(model_risk)
  c(medium = 500, high = 1000)[[model_risk]]
}

reference_bootstrap <- function(fit, nboot = NULL, seed, name,
                                model_risk = c("medium", "high"),
                                strat_var = NULL, ci = 0.95) {
  .diag_bootstrap_gate()
  stopifnot(!missing(seed), !missing(name))
  model_risk <- match.arg(model_risk)
  floor_n <- reference_bootstrap_minimum_nboot(model_risk)
  if (is.null(nboot)) nboot <- floor_n
  if (nboot < floor_n) {
    stop("nboot ", nboot, " is below the house minimum of ", floor_n,
         " for MODEL_RISK ", model_risk, " (SME decision 2026-08-28); ",
         "raise nboot or record a deviation.", call. = FALSE)
  }
  if (!exists("harness_seed", mode = "function")) source(file.path("tools", "seed.R"), local = TRUE)
  harness_seed(seed)
  # restart = FALSE keeps completed replicates already in the cache
  # directory (restart = TRUE deletes the cache and starts over). stratVar
  # is only added when declared so the engine's unstratified default applies
  # otherwise (D2). The bootstrapFit return value is deliberately discarded:
  # per the shape verification above, the results land in fit$env, not in
  # the return value.
  args <- list(fit = fit, nboot = nboot, restart = FALSE,
               fitName = name, ci = ci)
  if (!is.null(strat_var)) args$stratVar <- strat_var
  do.call(nlmixr2extra::bootstrapFit, args)

  e <- fit$env
  list(
    summary    = e$bootSummary,          # parFixedDf with CIs, omega, nboot
    cov_matrix = e$bootCovMatrix,
    message    = e$bootSummary$message,
    warnings   = e$bootSummary$warnings, # convergence bookkeeping, verbatim
    record     = list(nboot = nboot, seed = seed, ci = ci,
                      strat_var = strat_var %||% "none (per-MAP)",
                      engine = paste("nlmixr2extra",
                                     as.character(utils::packageVersion("nlmixr2extra"))))
  )
}

`%||%` <- function(a, b) if (is.null(a)) b else a
