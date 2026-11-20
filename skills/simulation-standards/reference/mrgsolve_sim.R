# REFERENCE IMPLEMENTATION — Simulation from a fitted model (mrgsolve)
#
# AUTHORITY: skills/simulation-standards/MANIFEST.yaml is the authority for the SME
# signature (root reviewed_by / reviewed_at) and for the per-unit status and
# digests. This header carries no status claim of its own -- read the manifest.
#
# This unit requires local manifest approval. mrgsolve documentation:
# https://mrgsolve.org/user-guide/
#
# Contract: agents never edit this file at analysis time. The one
# analysis-specific piece — the mrgsolve model text translating the fitted
# model — is authored per analysis, MUST pass
# reference_sim_translate_check() before any scenario is simulated, and its
# source is part of the gate package for SME review.


# Self-gate predicate: TRUE only when skills/simulation-standards/MANIFEST.yaml
# has a non-empty root reviewed_by/reviewed_at and this unit's status: line
# reads "approved". The authority is that MANIFEST record, not the
# "Reviewed by:" comment above — that comment is a historical record the
# validator ignores. Signing is done by filling reviewed_by in the MANIFEST
# and approving the unit, never by editing the comment above.
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

sim_reference_validated <- base::local({
  private <- .mm_trusted_private
  function() {
    # Integrity batch 2: delegates to the method-manifest self-gate core; binds
    # to this reference file's sha256 once the SME approves the manifest. The
    # DEPRECATED `legacy` closure (ignored by the validator since batch 3c) was
    # removed in integrity batch 13, when this file was next re-validated.
    base::get("require_validated_method", envir = private)(
      skill_dir    = "skills/simulation-standards",
      calling_file = "skills/simulation-standards/reference/mrgsolve_sim.R",
      unit         = "mrgsolve_sim")
  }
}, envir = base::new.env())

# Gate stop shared by every manifested entry point of this unit (integrity
# batch 7, Codex round 8 finding 4): compile, translation check and physical
# checks used to run ungated while only reference_simulate() consulted the
# manifest. An approved unit whose entry points do not gate is an authorized
# lane with the door open, so each now stops here first.
.sim_gate <- function() {
  if (!sim_reference_validated()) {
    base::stop("simulation-standards unit 'mrgsolve_sim' is not SME-validated ",
         "(manifest unit draft/rejected, or the manifest has an empty ",
         "reviewed_by); no simulation step runs until the unit is approved. ",
         "ESCALATE to the SME.", call. = FALSE)
  }
  base::invisible(TRUE)
}

# ---------------------------------------------------------------------------
# Compile

# Compiling is the first gate: a translation that does not compile is not a
# model. `code` is the mrgsolve model text authored for this analysis.
reference_sim_compile <- function(code, name = "sim_model") {
  .sim_gate()
  mrgsolve::mcode(name, code, quiet = TRUE)
}

# ---------------------------------------------------------------------------
# Translation equivalence check — mandatory before any scenario runs.
#
# Simulates the fitted model's own design (same subjects, doses, observation
# times) with all random effects zeroed, and compares the engine's typical
# prediction against the fit's population prediction (PRED). Relative
# tolerance. Hard stop on mismatch: the translation does not reproduce the
# model it claims to implement.
#
# design: data.frame with ID, TIME, EVID, AMT, CMT (dose rows EVID 1,
#   observation rows EVID 0) — the fitted dataset's design columns.
# pred:   data.frame with ID, TIME, PRED — population predictions from the
#   fit at the observation rows (e.g. as.data.frame(fit) for nlmixr2).
# observed: the $CAPTURE name of the concentration in the model ("CP").
# covariates: names of further design columns (for example "WT") carried
#   into the simulated data set, where mrgsolve uses a column named like a
#   $PARAM entry to update that parameter (help topic `data_set`, Details).
#   Default character(0): the data set is the one built before the amendment.
#
# Returns invisibly list(n, max_rel_err, tolerance) for the run record.
# Stops (never warns) on missing design columns, an undeclared capture
# name, observation rows with no simulated match, or any row whose
# relative error exceeds `tolerance` (house default 1e-3).
reference_sim_translate_check <- function(mod, design, pred,
                                          observed = "CP",
                                          tolerance = 1e-3,
                                          covariates = character(0)) {
  .sim_gate()
  if (!is.character(covariates) || anyNA(covariates)) {
    stop("covariates must be a character vector of design column names.",
         call. = FALSE)
  }
  need <- c("ID", "TIME", "EVID", "AMT", "CMT", covariates)
  missing_cols <- setdiff(need, names(design))
  if (length(missing_cols)) {
    stop("design is missing column(s): ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }
  d <- data.frame(ID = design$ID, time = design$TIME, evid = design$EVID,
                  amt = design$AMT, cmt = design$CMT)
  if (length(covariates)) d[covariates] <- design[covariates]
  d$amt[is.na(d$amt)] <- 0
  # -evid: at tied times the dose row (EVID 1) sorts before the observation
  # row, matching NONMEM/nlmixr2 record-order semantics for same-time events.
  d <- d[order(d$ID, d$time, -d$evid), ]

  # mrgsims coercion is an S4 method in the mrgsolve namespace; the bare
  # base generic does not dispatch on it under Rscript.
  typ <- mrgsolve::as.data.frame(mrgsolve::mrgsim_d(
    mrgsolve::zero_re(mod), d, obsonly = TRUE))
  if (!observed %in% names(typ)) {
    stop("Model captures no column named '", observed,
         "'; declare the concentration in $CAPTURE or pass `observed`.",
         call. = FALSE)
  }

  # Join on ID + time rounded to 10 significant digits so floating-point
  # noise in the time column cannot silently break the row matching.
  key_sim  <- paste(typ$ID, signif(typ$time, 10))
  key_pred <- paste(pred$ID, signif(pred$TIME, 10))
  m <- match(key_pred, key_sim)
  if (anyNA(m)) {
    stop(sum(is.na(m)), " observation row(s) in the fit have no matching ",
         "simulated row; design and pred disagree on times or IDs.",
         call. = FALSE)
  }
  got <- typ[[observed]][m]
  exp_pred <- pred$PRED
  rel <- abs(got - exp_pred) / pmax(abs(exp_pred), .Machine$double.eps)
  bad <- which(rel > tolerance)
  if (length(bad)) {
    show <- utils::head(bad, 5)
    stop("Translation failed equivalence with the fitted model at ",
         length(bad), " of ", length(rel), " observation rows:\n",
         paste(sprintf("  ID %s t=%g: engine %.6g, fit PRED %.6g (rel. err %.2e)",
                       pred$ID[show], pred$TIME[show], got[show],
                       exp_pred[show], rel[show]),
               collapse = "\n"),
         "\nThe mrgsolve model does not reproduce the fitted model; fix the ",
         "translation before simulating any scenario.", call. = FALSE)
  }
  invisible(list(n = length(rel), max_rel_err = max(rel), tolerance = tolerance))
}

# ---------------------------------------------------------------------------
# Physical checks on simulated output — the smoke gate.

# Sanity gate applied to every simulated scenario: stops on non-finite,
# negative, or all-zero values in the `observed` column. This catches
# failure modes the equivalence check cannot (it only exercises the fitted
# design), e.g. a scenario dosing into the wrong compartment or a solver
# blow-up on an extrapolated regimen. Returns invisibly TRUE; called per
# scenario inside reference_simulate().
reference_sim_physical_checks <- function(sim, observed = "CP") {
  .sim_gate()
  x <- sim[[observed]]
  if (is.null(x)) {
    stop("Simulated output has no column '", observed, "'.", call. = FALSE)
  }
  problems <- character(0)
  if (any(!is.finite(x))) {
    problems <- c(problems, paste0(sum(!is.finite(x)), " non-finite values"))
  }
  if (any(x < 0, na.rm = TRUE)) {
    problems <- c(problems, paste0(sum(x < 0, na.rm = TRUE), " negative values"))
  }
  if (all(x == 0, na.rm = TRUE)) {
    problems <- c(problems,
                  "all-zero output (drug never reaches the observed compartment)")
  }
  if (length(problems)) {
    stop("Physical checks failed on simulated output: ",
         paste(problems, collapse = "; "), ".", call. = FALSE)
  }
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# Scenario simulation — the gated entry point.
#
# scenarios: named list; each element a list with
#   $regimen  list(amt, cmt, ii = NULL, addl = NULL, rate = NULL,
#                  start = 0)
#   $n        subjects (1 for a typical-subject run)
#   $grid     list(start, end, delta) or $times numeric vector
#   Optional (each absent field leaves the call as
#   before):
#   $param    named list of model parameters for this scenario only, applied
#             with mrgsolve::param(.strict = TRUE), so a misspelt name stops
#             instead of being ignored (help topic `param`)
#   $regimen$amt_per_kg and $regimen$weight_param, in place of $regimen$amt:
#             the event amount is amt_per_kg times $param[[weight_param]],
#             formed here so the amount and the covariate come from one
#             declared weight. The record's scenarios carry the formed amount
#             as regimen$amt, beside amt_per_kg and weight_param, because the
#             exposure unit and the scenario table read regimen$amt.
#   $post_dose_obs  TRUE: at a time that holds both a dose and a grid point,
#             the observation is recorded after the dose, so for a bolus the
#             grid value there is dose over V (MAP A3 open question 8). It
#             uses mrgsolve's own record ordering, recsort = 3 ("3 and 4 put
#             those doses before padded observations at the same time", and
#             3 does the same for doses scheduled through addl; help topic
#             `mrgsim`, argument `recsort`). Every dose time must lie on the
#             grid, otherwise the scenario stops. FALSE or absent: today's
#             ordering, where the grid value at the dose time precedes it.
# uncertainty: "none" zeroes all random effects; "iiv" keeps OMEGA (and
#   SIGMA off — simulation of individual parameters, not noisy
#   observations); "full" hard-stops here (see note below).
# Returns list(sim = long data.frame with SCENARIO column, record = engine,
#   version, seed, settings) — the record goes in the gate package verbatim.
reference_simulate <- function(mod, scenarios, seed,
                               uncertainty = c("none", "iiv", "full"),
                               observed = "CP") {
  if (!sim_reference_validated()) {
    stop("simulation-standards reference implementation is not yet ",
         "SME-validated (unit mrgsolve_sim is draft/rejected, or ",
         "skills/simulation-standards/MANIFEST.yaml has an empty root ",
         "reviewed_by); no simulation runs until the unit is approved. ",
         "ESCALATE to the SME.", call. = FALSE)
  }
  uncertainty <- match.arg(uncertainty)
  # The separate uncertainty unit handles parameter uncertainty.
  # authorization; behavior unchanged.)
  if (uncertainty == "full") {
    stop("Uncertainty tier 'full' is not wired into reference_simulate(); ",
         "Use reference/uncertainty.R for parameter uncertainty ",
         "directly (draws -> install check -> reference_simulate_full) ",
         "per its header, or ESCALATE to the SME.", call. = FALSE)
  }
  stopifnot(!missing(seed), is.list(scenarios), length(names(scenarios)) > 0)
  if (any(!nzchar(names(scenarios)))) {
    stop("Every scenario must be named; names appear in outputs and the ",
         "gate package.", call. = FALSE)
  }
  # Every scenario is validated and its amount formed before anything runs.
  amounts <- lapply(names(scenarios), function(nm)
    .sim_scenario_amount(nm, scenarios[[nm]]))
  names(amounts) <- names(scenarios)
  if (!exists("harness_seed", mode = "function")) source(file.path("tools", "seed.R"), local = TRUE)
  harness_seed(seed)

  # zero_re(mod, "sigma") zeroes only RUV: OMEGA stays live, so the iiv
  # tier simulates between-subject variability without observation noise
  # (see the header's uncertainty note).
  m <- switch(uncertainty,
              none = mrgsolve::zero_re(mod),
              iiv  = mrgsolve::zero_re(mod, "sigma"))

  out <- lapply(names(scenarios), function(nm) {
    sc <- scenarios[[nm]]
    r <- sc$regimen
    ev_args <- list(amt = amounts[[nm]], cmt = r$cmt)
    if (!is.null(r$ii))    ev_args$ii   <- r$ii
    if (!is.null(r$addl))  ev_args$addl <- r$addl
    if (!is.null(r$rate))  ev_args$rate <- r$rate
    if (!is.null(r$start)) ev_args$time <- r$start
    dose <- do.call(mrgsolve::ev, ev_args)

    m_sc <- if (is.null(sc[["param"]])) m else mrgsolve::param(m, sc[["param"]], .strict = TRUE)
    sim_args <- list(m_sc, events = dose, nid = sc$n %||% 1L, obsonly = TRUE)
    if (isTRUE(sc$post_dose_obs)) sim_args$recsort <- 3L
    if (!is.null(sc$times)) {
      sim_args$tgrid <- sc$times
    } else {
      g <- sc$grid
      sim_args$start <- g$start %||% 0
      sim_args$end   <- g$end
      sim_args$delta <- g$delta
    }
    s <- mrgsolve::as.data.frame(do.call(mrgsolve::mrgsim, sim_args))
    reference_sim_physical_checks(s, observed = observed)
    s$SCENARIO <- nm
    s
  })

  # The record echoes the declared scenarios; a per-kilogram scenario also
  # carries the amount formed above as regimen$amt.
  echoed <- scenarios
  for (nm in names(scenarios)) {
    if (!is.null(scenarios[[nm]]$regimen$amt_per_kg))
      echoed[[nm]]$regimen$amt <- amounts[[nm]]
  }

  list(
    sim = do.call(rbind, out),
    record = list(
      engine      = "mrgsolve",
      version     = as.character(utils::packageVersion("mrgsolve")),
      seed        = seed,
      uncertainty = uncertainty,
      scenarios   = echoed,
      observed    = observed
    )
  )
}

# Validates one scenario's amended fields and returns its event amount:
# $regimen$amt unchanged when amt_per_kg is absent, otherwise amt_per_kg times
# the scenario's $param[[weight_param]]. Stops name the scenario.
.sim_scenario_amount <- function(nm, sc) {
  bad <- function(...) stop("scenario '", nm, "': ", ..., call. = FALSE)
  one_positive <- function(x)
    is.numeric(x) && length(x) == 1L && is.finite(x) && x > 0
  r <- sc$regimen
  p <- sc[["param"]]
  if (!is.null(p) && (!is.list(p) || is.null(names(p)) ||
                      any(!nzchar(names(p))) || anyDuplicated(names(p))))
    bad("param must be a named list of model parameters.")
  if (!is.null(sc$post_dose_obs) &&
      !(isTRUE(sc$post_dose_obs) || isFALSE(sc$post_dose_obs)))
    bad("post_dose_obs must be TRUE or FALSE.")
  if (isTRUE(sc$post_dose_obs)) {
    # ponytail: dose times from start, ii and addl only; a scenario that needs
    # another event shape (a data set, several ev objects) extends this list.
    dose_t <- (r$start %||% 0) + (r$ii %||% 0) * (0:(r$addl %||% 0))
    grid_t <- if (!is.null(sc$times)) sc$times else
      seq(sc$grid$start %||% 0, sc$grid$end, by = sc$grid$delta)
    off <- setdiff(signif(dose_t, 10), signif(grid_t, 10))
    if (length(off))
      bad("post_dose_obs needs every dose time on the sampling grid; dose ",
          "time(s) ", paste(off, collapse = ", "), " are not on the sampling grid.")
  }
  if (is.null(r$amt_per_kg)) {
    if (!is.null(r$weight_param))
      bad("weight_param is given without amt_per_kg.")
    return(r[["amt"]])
  }
  if (!is.null(r[["amt"]]))
    bad("give either amt or amt_per_kg, not both.")
  if (!one_positive(r$amt_per_kg))
    bad("amt_per_kg must be a single finite positive number.")
  wp <- r$weight_param
  if (!is.character(wp) || length(wp) != 1L || is.na(wp) || !nzchar(wp))
    bad("amt_per_kg needs weight_param, the name of the $param entry that ",
        "holds the weight.")
  if (!wp %in% names(p))
    bad("weight_param '", wp, "' names no $param entry of this scenario.")
  if (!one_positive(p[[wp]]))
    bad("$param entry '", wp, "' must be a single finite positive number.")
  r$amt_per_kg * p[[wp]]
}

# Null-coalescing helper: base R only ships %||% from 4.4; defined here so
# the file has no version or rlang dependency for it.
`%||%` <- function(a, b) if (is.null(a)) b else a
