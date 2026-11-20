# Stage 5 qualification battery (skills/diagnostics.md).
#
# Battery membership is configurable and risk-scaled (D4): the MAP declares
# MODEL_RISK, which selects the mandatory members; the MAP may additionally
# enable optional members by name. Simulation-based diagnostics (VPC, pcVPC,
# NPDE) are configurable members, not hardcoded — same registry discipline as
# the EDA plot tiers.
#
# Every member is a function(fit, cfg) returning a list(name, artifact, detail).
# Seeds are fixed and recorded (simulator/model-qualifier role rules).
#
# The numerics live in skills/diagnostics/reference/*.R (SME-validated
# reference implementations). This file owns only harness concerns: which
# members are mandatory at which MODEL_RISK, argument plumbing from the MAP,
# artifact handling, and the validation-status report. Agents select members;
# they never author the method.

DIAGNOSTICS_REFERENCE <- "skills/diagnostics/reference"

# Battery preset, above vpc.R's signed minimum of 200. NPDE/bootstrap defaults
# are queried from their signed references after validation (below), so their
# scientific floors have one authoritative source.
QUALIFICATION_VPC_DEFAULT <- 500
QUALIFICATION_MODEL_RISKS <- c("low", "medium", "high")

# Sources a reference file as an existence-and-parse check and returns its
# path invisibly; `local = TRUE` keeps the sourced definitions in this
# function's own frame, so battery members source the reference into their
# own scope at run time rather than relying on this loader. Stops when the
# file is absent.
load_diagnostic_reference <- function(file) {
  path <- file.path(DIAGNOSTICS_REFERENCE, file)
  if (!file.exists(path)) {
    stop("Missing reference implementation: ", path, call. = FALSE)
  }
  source(path, local = TRUE)$value
  invisible(path)
}

# TRUE only when the diagnostics manifest approves the intended-use unit for
# this reference file AND the file still hashes to the recorded digest. Uses
# the metadata query API because a valid draft/rejected unit is reportable as
# BLOCKED; malformed manifests and digest drift remain hard errors.
#
# Integrity batch 3a: this replaces the old comment-grep ("# Reviewed by:" in
# the reference file) with the per-unit method-manifest API. Each diagnostics
# reference file is one unit, keyed by its basename without the .R extension
# (gof.R -> unit "gof", vpc.R -> "vpc", ...). is_method_validated returns
# TRUE for an approved unit with a matching hash, FALSE for a draft/rejected
# unit or unconfirmed manifest, and HARD-STOPS on an integrity condition
# (missing manifest/unit or a tampered reference file) — the battery calls
# this BEFORE executing a member so an unvalidated method never runs.
reference_validated <- function(file) {
  if (!exists("is_method_validated", mode = "function"))
    source(file.path("tools", "method_manifest.R"))
  path <- file.path(DIAGNOSTICS_REFERENCE, file)
  if (!file.exists(path)) return(FALSE)
  unit <- sub("\\.R$", "", basename(file))
  isTRUE(is_method_validated(
    skill_dir    = "skills/diagnostics",
    calling_file = path,
    unit         = unit,
    # Transitional legacy closure: the diagnostics signature historically lived
    # as a "# Reviewed by:" COMMENT in the reference file itself (not SKILL.md),
    # so the pre-cutover fallback greps that comment to reproduce prior
    # behaviour exactly. Once the SME fills the manifest reviewed_by, the unit
    # status is authoritative and this closure is dead.
    legacy = function() {
      txt <- readLines(path, warn = FALSE)
      ln <- grep("^#\\s*Reviewed by:", txt, value = TRUE)
      length(ln) > 0 && nzchar(trimws(sub("^#\\s*Reviewed by:", "", ln[1])))
    }))
}

# Builds observed and simulated data from a fit, then runs the tidyvpc
# reference implementation. Simulation settings come from the analysis plan
# via cfg; the seed is recorded in the returned detail string.
# pred_corr = TRUE attaches model PRED to the observations and requests the
# prediction-corrected variant. Returns the standard member list(name,
# artifact, detail) — or, with render = FALSE, the raw tidyvpc object for
# members (qpc) that post-process it. The same cfg$seed drives both the
# simulation and tidyvpc, so the diagnostic reproduces from one recorded
# seed.
run_tidyvpc <- function(fit, cfg, pred_corr, render = TRUE) {
  source(file.path(DIAGNOSTICS_REFERENCE, "vpc.R"), local = TRUE)
  sim <- vpc_simulate(fit, n = cfg$vpc_n, seed = cfg$seed)
  prep <- reference_vpc_prepare(sim$obs, sim$sim, id = "ID", x = "TIME",
                                repl = "REP", evid = "EVID")
  obs <- prep$obs
  if (isTRUE(pred_corr)) obs <- reference_vpc_attach_pred(obs, prep$sim)

  v <- reference_vpc_continuous(
    obs, prep$sim, x = "TIME", yobs = "DV", ysim = "DV", repl = "REP",
    # No silent binning default here either (SME decision 2026-09-04, Codex
    # round 9 P1-4): the run-config's `binning` entry is the declaration, and
    # its absence is a refusal at the reference API, not a house default.
    binning_spec = cfg$binning,
    strata       = cfg$stratify,
    pred         = if (pred_corr) "PRED" else NULL,
    pred_corr    = pred_corr,
    log_scale_correction = cfg$log_scale_correction %||% FALSE,
    qpred        = cfg$qpred %||% c(0.05, 0.5, 0.95),
    conf.level   = cfg$conf_level %||% 0.95,
    seed         = cfg$seed)

  if (!render) return(list(vpc = v))
  nm   <- if (pred_corr) "pcvpc" else "vpc"
  path <- file.path(cfg$out_dir, paste0(nm, "-", cfg$run, ".pdf"))
  ggplot2::ggsave(path, plot(v), width = 7, height = 5)
  list(name = nm, artifact = path,
       detail = sprintf("%s via tidyvpc, n = %d replicates, %s binning, seed = %d",
                        if (pred_corr) "prediction-corrected VPC" else "VPC",
                        cfg$vpc_n,
                        cfg$binning$method,  # declared; the reference call above refused a NULL spec
                        cfg$seed))
}

# The battery registry. Each member declares: `risk` — the MODEL_RISK levels
# at which it is mandatory (empty vector = optional, enabled only by the
# plan); `simulation` — whether it is simulation-based; `reference` — the
# file under skills/diagnostics/reference whose validation status is
# reported per member; `fn(fit, cfg)` — the runner returning list(name,
# artifact, detail). Adding a member here is a harness change, not an
# analysis-time act.
qualification_registry <- function() {
  list(
    gof = list(
      risk = c("low", "medium", "high"), simulation = FALSE,
      reference = "gof.R",
      fn = function(fit, cfg) {
        source(file.path(DIAGNOSTICS_REFERENCE, "gof.R"), local = TRUE)
        panels <- reference_gof_panel(fit)
        if (!length(panels)) stop("qualification member gof: reference returned zero panels.", call. = FALSE)
        path <- file.path(cfg$out_dir, paste0("gof-final-", cfg$run, ".pdf"))
        grDevices::pdf(path, width = 7, height = 5.5)
        for (p in panels) print(p)
        grDevices::dev.off()
        list(name = "gof", artifact = path, detail = "standard GOF panel")
      }),

    vpc = list(
      risk = c("low", "medium", "high"), simulation = TRUE,
      reference = "vpc.R",
      fn = function(fit, cfg) run_tidyvpc(fit, cfg, pred_corr = FALSE)),

    pcvpc = list(
      risk = character(0), simulation = TRUE,   # optional; enable in the plan
      reference = "vpc.R",
      fn = function(fit, cfg) run_tidyvpc(fit, cfg, pred_corr = TRUE)),

    qpc = list(
      risk = character(0), simulation = TRUE,   # optional; enable in the plan
      reference = "vpc.R",
      fn = function(fit, cfg) {
        source(file.path(DIAGNOSTICS_REFERENCE, "vpc.R"), local = TRUE)
        v <- run_tidyvpc(fit, cfg, pred_corr = cfg$qpc_pred_corr %||% FALSE,
                          render = FALSE)
        q <- reference_qpc(v$vpc, alpha = cfg$qpc_alpha %||% 0.05,
                           weights      = cfg$qpc_weights,
                           sharp_ref    = cfg$sharp_ref,
                           interval_ref = cfg$interval_ref)
        path <- file.path(cfg$out_dir, paste0("qpc-", cfg$run, ".csv"))
        utils::write.csv(as.data.frame(q$qpc), path, row.names = FALSE)
        list(name = "qpc", artifact = path,
             detail = sprintf(
               "quantified prediction check, seed = %d%s", cfg$seed,
               if (is.null(cfg$sharp_ref)) "; self-normalized (not comparable across models)"
               else "; anchored to reference model"))
      }),

    npde = list(
      risk = c("medium", "high"), simulation = TRUE,
      reference = "npde.R",
      fn = function(fit, cfg) {
        source(file.path(DIAGNOSTICS_REFERENCE, "npde.R"), local = TRUE)
        panels <- reference_npde(fit, nsim = cfg$npde_n, seed = cfg$seed)
        if (!length(panels)) stop("qualification member npde: reference returned zero panels.", call. = FALSE)
        path <- file.path(cfg$out_dir, paste0("npde-", cfg$run, ".pdf"))
        grDevices::pdf(path, width = 7, height = 5.5)
        for (p in panels) print(p)
        grDevices::dev.off()
        list(name = "npde", artifact = path,
             detail = sprintf("NPDE panel, nsim = %d, seed = %d",
                              cfg$npde_n, cfg$seed))
      }),

    bootstrap = list(
      risk = c("medium", "high"), simulation = TRUE,
      reference = "bootstrap.R",
      fn = function(fit, cfg) {
        source(file.path(DIAGNOSTICS_REFERENCE, "bootstrap.R"), local = TRUE)
        # reference_bootstrap requires `name` (bootstrapFit's mandatory
        # fitName — the engine crashes without it) and returns the results
        # under $summary, not $bootSummary.
        b <- reference_bootstrap(fit, nboot = cfg$boot_n, seed = cfg$seed,
                                 name = paste0("fit_", cfg$run),
                                 model_risk = cfg$model_risk %||% "medium")
        path <- file.path(cfg$out_dir, paste0("bootstrap-", cfg$run, ".csv"))
        utils::write.csv(b$summary$parFixedDf %||% b$summary, path)
        list(name = "bootstrap", artifact = path,
             detail = sprintf("bootstrap, nboot = %d, seed = %d",
                              cfg$boot_n, cfg$seed))
      })
  )
}

# Fill only missing method-specific counts, after that method's manifest unit
# has validated. Explicit MAP values remain unchanged and the reference still
# enforces its floor at execution.
qualification_member_config <- function(member, cfg, model_risk) {
  if (identical(member, "npde") && is.null(cfg$npde_n)) {
    source(file.path(DIAGNOSTICS_REFERENCE, "npde.R"), local = TRUE)
    cfg$npde_n <- reference_npde_minimum_nsim()
  }
  if (identical(member, "bootstrap") &&
      model_risk %in% c("medium", "high") && is.null(cfg$boot_n)) {
    source(file.path(DIAGNOSTICS_REFERENCE, "bootstrap.R"), local = TRUE)
    cfg$boot_n <- reference_bootstrap_minimum_nboot(model_risk)
  }
  cfg
}

# model_risk selects mandatory members; enabled/disabled adjust explicitly.
# Disabling a risk-mandated member is a DEVIATE-level act: it errors unless
# `justification` is supplied, which is then recorded in the package.
# Returns list(model_risk, seed, members, disabled, justification); members
# is a data.frame with one row per executed member, including its reference
# file and sme_validated flag, so the gate package shows validation status
# alongside results. A member that errors is captured as a FAILED row —
# never dropped. The seed and VPC battery preset are fixed. Missing NPDE and
# bootstrap counts come from their validated signed reference policy queries.
qualification_battery <- function(fit, model_risk,
                                  enabled = character(0),
                                  disabled = character(0),
                                  justification = NULL,
                                  cfg = list()) {
  if (!is.character(model_risk) || length(model_risk) != 1L ||
      !nzchar(model_risk) || !model_risk %in% QUALIFICATION_MODEL_RISKS) {
    stop("MODEL_RISK must be exactly one of: ",
         paste(QUALIFICATION_MODEL_RISKS, collapse = ", "), ".",
         call. = FALSE)
  }
  if (!is.list(cfg)) stop("cfg must be a list.", call. = FALSE)
  if (!is.null(cfg$model_risk) && !identical(cfg$model_risk, model_risk)) {
    stop("cfg$model_risk conflicts with the top-level MODEL_RISK; declare risk ",
         "once through the model_risk argument.", call. = FALSE)
  }
  cfg <- utils::modifyList(
    list(out_dir = "pilot/model/outputs", run = "final", seed = 20260828,
         vpc_n = QUALIFICATION_VPC_DEFAULT),
    cfg)
  cfg$model_risk <- model_risk
  dir.create(cfg$out_dir, recursive = TRUE, showWarnings = FALSE)

  reg <- qualification_registry()
  mandatory <- names(Filter(function(m) model_risk %in% m$risk, reg))
  if (length(disabled) && (!is.character(justification) || length(justification) != 1L || is.na(justification) || !nzchar(trimws(justification)))) {
    stop("Disabling qualification members (", paste(disabled, collapse = ", "),
         ") requires a written justification recorded via DEVIATE.",
         call. = FALSE)
  }
  members <- setdiff(union(mandatory, enabled), disabled)
  unknown <- setdiff(members, names(reg))
  if (length(unknown)) {
    stop("Unknown qualification members: ", paste(unknown, collapse = ", "),
         call. = FALSE)
  }

  results <- lapply(members, function(m) {
    ref <- reg[[m]]$reference %||% NA_character_
    # Validation happens BEFORE execution and OUTSIDE tryCatch (integrity batch
    # 3a): an unvalidated method must never run. reference_validated() itself
    # hard-stops on an integrity condition (missing/tampered manifest); a draft
    # or rejected unit returns FALSE and the member is BLOCKED, not executed.
    validated <- if (is.na(ref)) NA else reference_validated(ref)
    if (isFALSE(validated)) {
      r <- list(name = m, artifact = NA_character_,
                detail = paste("BLOCKED: reference method", ref,
                               "is not SME-validated (unit draft/rejected)"))
    } else {
      # Resolve policy outside the execution catcher: a manifest/digest error
      # remains an integrity hard stop rather than becoming a FAILED result.
      member_cfg <- qualification_member_config(m, cfg, model_risk)
      r <- tryCatch(reg[[m]]$fn(fit, member_cfg),
                    error = function(e) list(name = m, artifact = NA_character_,
                                             detail = paste("FAILED:", conditionMessage(e))))
    }
    r$simulation_based <- reg[[m]]$simulation
    r$mandatory <- m %in% mandatory
    r$reference <- ref
    r$sme_validated <- validated
    r
  })

  list(
    model_risk = model_risk,
    seed = cfg$seed,
    members = do.call(rbind, lapply(results, function(r) data.frame(
      member = r$name, mandatory = r$mandatory,
      simulation_based = r$simulation_based,
      reference = r$reference, sme_validated = r$sme_validated,
      artifact = basename(r$artifact %||% NA), detail = r$detail,
      stringsAsFactors = FALSE))),
    disabled = disabled,
    justification = justification %||% NA_character_
  )
}
