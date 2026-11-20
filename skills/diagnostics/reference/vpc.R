# REFERENCE IMPLEMENTATION — Visual Predictive Check (tidyvpc)
#
# AUTHORITY: skills/diagnostics/MANIFEST.yaml is the authority for the SME
# signature (root reviewed_by / reviewed_at) and for the per-unit status and
# digests. This header carries no status claim of its own -- read the manifest.
#
# Built on tidyvpc (Certara). See the package vignettes:
# https://certara.github.io/tidyvpc/articles/tidyvpc_cont.html
# https://certara.github.io/tidyvpc/articles/tidyvpc_cat.html
# https://certara.github.io/tidyvpc/articles/tidyvpc_qpc.html
#   tidyvpc version this was written against: 1.6.0
#
# Contract: agents never edit this file at analysis time. They call these
# functions with parameters declared in the analysis plan. Changes to the
# method go through skill review, not through an analysis.
#
# API verified against the installed package (argument lists taken from the
# S3 methods, not from memory):
#   observed(data, x, yobs, pred, blq, lloq, alq, uloq)
#   simulated(o, data, ysim, xsim, repl)
#   censoring(o, blq, lloq, alq, uloq, data)
#   binning(o, bin, data, xbin = "xmedian", centers, breaks, nbins, altx,
#           stratum, by.strata = TRUE)
#   binless(o, optimize = TRUE, optimization.interval = c(0, 7),
#           loess.ypc, lambda, span, sp)
#   predcorrect(o, pred, data, log = FALSE, varcorr = FALSE)
#   stratify(o, formula, data, data.sim)
#   vpcstats(o, vpc.type = c("continuous", "categorical"),
#            qpred = c(0.05, 0.5, 0.95), conf.level = 0.95, quantile.type = 7)
#   qpcstats(o, alpha = 0.05, w = c(med_cov = .35, tail_cov = .2, mae = .15,
#            drift = .1, sharp = .1, interval = .1), sharp_ref, interval_ref)

# ---------------------------------------------------------------------------
# Continuous VPC
#
# obs        observed data (one row per observation record)
# sim        simulated data: replicates stacked, identified by `repl`
# x, yobs    observed independent and dependent variable column names
# ysim, repl simulated dependent variable and replicate identifier
# binning_spec  list describing the binning method; see reference_vpc_binning()
#               or list(method = "binless") for the LOESS/binless approach
# strata     one-sided formula, e.g. ~ STUDY or ~ STUDY + DOSE, or NULL
# pred       PRED column name; required when pred_corr = TRUE
# pred_corr  prediction correction (Bergstrand); `log` must match the scale
#            of yobs
# lloq/uloq  censoring limits; supply with blq/alq flags when applicable
# qpred      the percentiles the VPC displays
# conf.level confidence level of the simulated percentile intervals
# seed       mandatory, no default — every VPC run must be reproducible from
#            its declared seed
#
# Returns the tidyvpc object after vpcstats(): $stats carries the binned
# observed and simulated percentiles ready for the plotting layer. Stops
# when seed is missing or when pred_corr = TRUE cannot resolve PRED.
#
# Review notes:
# - `repl` is declared but not forwarded: tidyvpc::simulated() infers
#   replicate boundaries from row order, so `sim` must be complete
#   replicates stacked in observed-row order. reference_vpc_prepare
#   preserves that order (and sorts by `repl` in its categorical branch).
# - The qpred default encodes the house 5/50/95 percentiles; vpcstats' own
#   quantile.type = 7 default (API block above) matches the house choice.
# - Prediction correction (pcVPC) is reserved for heterogeneous designs per
#   the house defaults in SKILL_VPC.md; it is an analysis-plan declaration.
# - The house replicate minimum (>= 200) is enforced where the simulation is
#   produced, not here — this function accepts whatever `sim` it is given.
# ---------------------------------------------------------------------------
# Self-gate (integrity batch 3b, item C4). The diagnostics VPC lane executed
# UNGATED: a task module calling these functions directly bypassed the
# qualification runner's validation. Each public entry function now gates on
# its intended-use unit ("vpc"), same pattern as forest.R. The gate computes
# only when skills/diagnostics/MANIFEST.yaml carries a confirmed root (non-empty
# reviewed_by) and this unit's status is approved; the legacy "Reviewed by:"
# grep of SKILL_VPC.md was retired in batch 3c and is no longer consulted.
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

.vpc_gate <- base::local({
  private <- .mm_trusted_private
  function() {
    if (!base::get("require_validated_method", envir = private)(skill_dir = "skills/diagnostics",
                                          calling_file = "skills/diagnostics/reference/vpc.R",
                                          unit = "vpc")) {
      base::stop("diagnostics unit 'vpc' is not SME-validated (manifest unit ",
           "draft/rejected, or skills/diagnostics/MANIFEST.yaml has an empty root ",
           "reviewed_by); ",
           "VPC does not compute until the unit is approved. ESCALATE to the SME.",
           call. = FALSE)
    }
    base::invisible(TRUE)
  }
}, envir = base::new.env())

# Round 9, P1-4. The public VPCs used to default to ntile/9. A binning method
# is a scientific declaration -- it decides which observations are compared
# with which simulated percentiles -- so it has no default here; a call that
# does not declare one is refused by parameter name.
.vpc_check_binning_spec <- function(binning_spec) {
  if (missing(binning_spec) || is.null(binning_spec) || !length(binning_spec))
    stop("binning_spec is a declared analysis-plan parameter with no default: ",
         "the binning method decides which observations are compared with ",
         "which simulated percentiles. Supply binning_spec (for example ",
         "list(method = \"ntile\", nbins = 9)) from the MAP.", call. = FALSE)
  invisible(NULL)
}

# Round 9, P1-4. `blq_bin_policy` was validated and then never read: "keep"
# and "omit" followed identical code paths, so a declared scientific decision
# could not change the computed result. The SME has not yet specified what
# either value means for a bin containing only below-quantification-limit
# observations, so rather than accept an inert declaration the censoring path
# refuses whenever the data actually carry a BLQ-flagged record. Data with no
# BLQ record are unaffected.
#
# SME decision 4 (2026-09-04; round-10 finding, section C item 4): the refusal
# covers ALL censoring, not only the lower limit. The function activated policy
# validation for alq/uloq but its data-dependent refusal examined only
# BLQ/LLOQ, so an above-quantification-limit data set computed under an inert
# declaration -- the same defect the BLQ half was raised for. Both directions
# are now examined: a flag column (blq or alq) carrying a set flag, an
# observation below a finite lloq, and an observation above a finite uloq.
.vpc_check_blq_data <- function(data, blq, blq_bin_policy,
                                yobs = NULL, lloq = -Inf,
                                alq = NULL, uloq = Inf) {
  refuse <- function(n, what)
    stop("this data set carries ", n, " ", what, ", and the meaning of ",
         "blq_bin_policy = \"keep\" / \"omit\" for a bin containing only ",
         "below-quantification-limit observations is not yet SME-specified. ",
         "The skill is skills/diagnostics (SKILL_VPC.md) and the authorizing ",
         "record is skills/diagnostics/MANIFEST.yaml, unit 'vpc'. Censoring ",
         "does not compute until the SME specifies and approves the rule. ",
         "ESCALATE to the SME.", call. = FALSE)

  # Review S4. `as.numeric()` on a factor yields the LEVEL CODE, so a factor
  # column of all "0" counted as flagged and refused a data set with no BLQ
  # record at all. Flags are read through as.character() and compared with the
  # forms a flag column actually takes; a value that is none of them is a
  # refusal naming it, never a silent coercion.
  check_flag <- function(col, label) {
    if (is.null(col) || !nzchar(col) || is.null(data) || !col %in% names(data))
      return(invisible(NULL))
    flagged <- data[[col]]
    flagged <- flagged[!is.na(flagged)]
    if (!length(flagged)) return(invisible(NULL))
    chr <- trimws(as.character(flagged))
    known <- c("0", "1", "TRUE", "FALSE", "T", "F")
    bad <- setdiff(unique(chr), known)
    if (length(bad))
      stop("the ", label, " flag column '", col, "' carries value(s) that are ",
           "not a flag: ", paste(utils::head(bad, 5L), collapse = ", "),
           ". A censoring flag column is 0/1 or TRUE/FALSE; guessing what ",
           "another value means is not this function's call. ESCALATE to the ",
           "SME.", call. = FALSE)
    n <- sum(chr %in% c("1", "TRUE", "T"))
    if (n > 0) refuse(n, paste0(label, "-flagged observation(s)"))
    invisible(NULL)
  }
  check_flag(blq, "BLQ")
  check_flag(alq, "ALQ")

  # Review S4. The censoring branch also fires on lloq/alq/uloq alone. With no
  # flag column declared, this check used to return early and censoring
  # proceeded with blq_bin_policy still inert -- the SME's decision only half
  # enforced. An observation outside a declared quantification limit IS a
  # censored observation whether or not a flag column names it, in EITHER
  # direction (SME decision 4).
  if (!is.null(yobs) && !is.null(data) &&
      length(yobs) == 1L && yobs %in% names(data)) {
    v <- suppressWarnings(as.numeric(as.character(data[[yobs]])))
    v <- v[!is.na(v)]
    if (is.finite(lloq)) {
      n <- sum(v < lloq)
      if (n > 0) refuse(n, sprintf("observation(s) below lloq = %s", format(lloq)))
    }
    if (is.finite(uloq)) {
      n <- sum(v > uloq)
      if (n > 0) refuse(n, sprintf("observation(s) above uloq = %s", format(uloq)))
    }
  }
  invisible(NULL)
}

reference_vpc_continuous <- function(obs, sim,
                                     x, yobs, ysim, repl,
                                     binning_spec,
                                     strata = NULL,
                                     pred = NULL, pred_corr = FALSE,
                                     log_scale_correction = FALSE,
                                     blq = NULL, lloq = -Inf,
                                     alq = NULL, uloq = Inf,
                                     blq_bin_policy = NULL,
                                     qpred = c(0.05, 0.5, 0.95),
                                     conf.level = 0.95,
                                     seed) {
  .vpc_gate()
  stopifnot(!missing(seed))
  .vpc_check_binning_spec(if (missing(binning_spec)) NULL else binning_spec)
  if (!exists("harness_seed", mode = "function")) source(file.path("tools", "seed.R"), local = TRUE)
  harness_seed(seed)   # simulated data is supplied, but any resampling is seeded

  # All-BLQ-bin policy (F9, Codex round 8). When censoring is configured a bin
  # can contain only below-quantification-limit observations, and how such a
  # bin is treated (kept as a censored bin or omitted from the percentile
  # panel) changes what the VPC shows. That treatment is a scientific
  # decision, so it is a DECLARED analysis-plan parameter: whenever censoring
  # is active, blq_bin_policy must be declared ("keep" or "omit"); an
  # undeclared policy under active censoring is a stop rather than a silent
  # convention. (Per-bin detection of an actual all-BLQ bin needs a censored
  # fixture to verify against tidyvpc's stats and is left as an open item; the
  # conservative guard requires the declaration up front and never relaxes it.)
  censoring_active <- !is.null(blq) || is.finite(lloq) ||
    !is.null(alq) || is.finite(uloq)
  if (censoring_active) {
    if (is.null(blq_bin_policy) ||
        !identical(as.character(blq_bin_policy), as.character(blq_bin_policy)[1]) ||
        !as.character(blq_bin_policy)[1] %in% c("keep", "omit"))
      stop("blq_bin_policy is a declared analysis-plan parameter (no default) ",
           "whenever censoring is active: an all-BLQ bin must be handled by a ",
           "declared rule. Supply blq_bin_policy = \"keep\" or \"omit\".",
           call. = FALSE)
  }

  # tidyvpc's builder functions capture bare column names; rlang::sym() plus
  # !! injects the string parameters as column symbols so callers declare
  # columns as strings in the analysis plan.
  o <- tidyvpc::observed(obs, x = !!rlang::sym(x), yobs = !!rlang::sym(yobs))
  o <- tidyvpc::simulated(o, sim, ysim = !!rlang::sym(ysim))

  # censoring() also wants bare column names, and blq/alq are optional:
  # bquote() splices either NULL or the column as a name, and the eval
  # environment exposes the obs columns so the spliced name resolves.
  # Censoring is what feeds the %BLQ panel, always rendered when BLQ exist
  # (house default in SKILL_VPC.md).
  if (!is.null(blq) || is.finite(lloq) || !is.null(alq) || is.finite(uloq)) {
    .vpc_check_blq_data(obs, blq, blq_bin_policy, yobs = yobs, lloq = lloq,
                        alq = alq, uloq = uloq)
    o <- eval(bquote(tidyvpc::censoring(
                o,
                blq  = .(if (is.null(blq)) NULL else as.name(blq)),
                lloq = .(lloq),
                alq  = .(if (is.null(alq)) NULL else as.name(alq)),
                uloq = .(uloq))),
              envir = c(list(o = o), as.list(obs)))
  }

  if (!is.null(strata)) o <- tidyvpc::stratify(o, strata)

  if (isTRUE(pred_corr)) {
    if (is.null(pred)) {
      stop("pred_corr = TRUE requires the PRED column name.", call. = FALSE)
    }
    if (!pred %in% names(obs)) {
      stop("Prediction correction needs the population prediction in the ",
           "OBSERVED data: column '", pred, "' not found. Merge PRED from a ",
           "single simulation replicate onto the observed records first.",
           call. = FALSE)
    }
    # Same bquote/eval idiom as censoring: predcorrect() captures a bare
    # column name. `log` must match the scale of yobs (see param docs).
    o <- eval(bquote(tidyvpc::predcorrect(o, pred = .(as.name(pred)),
                                          log = .(log_scale_correction))),
              envir = c(list(o = o), as.list(obs)))
  }

  o <- apply_binning(o, binning_spec)

  tidyvpc::vpcstats(o, vpc.type = "continuous",
                    qpred = qpred, conf.level = conf.level)
}

# ---------------------------------------------------------------------------
# Categorical VPC — for binary or multi-category endpoints. The statistics are
# proportions per category rather than concentration percentiles, so `qpred`
# does not apply.
#
# Returns the tidyvpc object after vpcstats(vpc.type = "categorical");
# $stats holds per-category observed and simulated proportions. Stops when
# seed is missing. Same replicate-ordering requirement as the continuous
# VPC (`repl` not forwarded; see review notes above) — run
# reference_vpc_prepare with categorical = TRUE first, which applies the
# ordering the package requires.
reference_vpc_categorical <- function(obs, sim,
                                      x, yobs, ysim, repl,
                                      binning_spec,
                                      strata = NULL,
                                      conf.level = 0.95,
                                      seed) {
  .vpc_gate()
  stopifnot(!missing(seed))
  .vpc_check_binning_spec(if (missing(binning_spec)) NULL else binning_spec)
  if (!exists("harness_seed", mode = "function")) source(file.path("tools", "seed.R"), local = TRUE)
  harness_seed(seed)

  o <- tidyvpc::observed(obs, x = !!rlang::sym(x), yobs = !!rlang::sym(yobs))
  o <- tidyvpc::simulated(o, sim, ysim = !!rlang::sym(ysim))
  if (!is.null(strata)) o <- tidyvpc::stratify(o, strata)
  o <- apply_binning(o, binning_spec)

  tidyvpc::vpcstats(o, vpc.type = "categorical", conf.level = conf.level)
}

# ---------------------------------------------------------------------------
# Binning dispatch. Binning choice is a scientific decision recorded in the
# analysis plan, not an agent default: it determines what the VPC can and
# cannot show. `binless` fits smooth quantile curves instead of binning and
# avoids bin-edge artefacts; the vignettes recommend it when binning
# boundaries are arbitrary or the design is unbalanced.
# House default (SKILL_VPC.md) is a binned VPC; binless stays available as a
# declarable analysis-plan alternative. Unknown methods stop rather than
# fall through to a silent default.
apply_binning <- function(o, spec) {
  # Binning is a declared scientific decision, not an agent default (F9,
  # Codex round 8). The method and — for the ntile/pam/kmeans/jenks families
  # — the bin count must be present in the analysis-plan binning spec; a
  # missing value stops with the declared-parameter message rather than
  # silently defaulting to ntile/9.
  method <- spec$method
  if (is.null(method) || !nzchar(method))
    stop("Binning method is a declared analysis-plan parameter (no default): ",
         "supply binning_spec$method (one of binless, ntile, pam, kmeans, ",
         "jenks, centers, breaks, column).", call. = FALSE)
  count_bin <- function(bin) {
    if (is.null(spec$nbins))
      stop("Binning method '", bin, "' requires a declared bin count: ",
           "supply binning_spec$nbins (no default).", call. = FALSE)
    tidyvpc::binning(o, bin = bin, nbins = spec$nbins)
  }
  switch(
    method,
    binless = tidyvpc::binless(
      o,
      optimize              = spec$optimize %||% TRUE,
      optimization.interval = spec$optimization.interval %||% c(0, 7),
      loess.ypc             = spec$loess.ypc,
      lambda                = spec$lambda,
      span                  = spec$span,
      sp                    = spec$sp),
    ntile   = count_bin("ntile"),
    pam     = count_bin("pam"),
    kmeans  = count_bin("kmeans"),
    jenks   = count_bin("jenks"),
    centers = tidyvpc::binning(o, bin = "centers", centers = spec$centers),
    breaks  = tidyvpc::binning(o, bin = "breaks",  breaks  = spec$breaks),
    # bquote/as.name for the same reason as censoring above: binning()
    # captures a bare column name when binning on an existing variable.
    column  = eval(bquote(tidyvpc::binning(o, bin = .(as.name(spec$bin_variable)))),
                   envir = list(o = o), enclos = parent.frame()),  # bin on a
                   # column such as nominal time — the vignettes' default when
                   # a nominal time variable exists
    stop("Unknown binning method: ", method, call. = FALSE)
  )
}

# ---------------------------------------------------------------------------
# Quantified prediction check: reduces a VPC to scored metrics (median
# coverage, tail coverage, mean absolute error, drift, sharpness, interval
# score) so model comparison does not rest on visual impression alone. Weights
# and reference values are analysis-plan decisions.
reference_qpc <- function(vpc_obj, alpha = 0.05, weights = NULL,
                          sharp_ref = NULL, interval_ref = NULL) {
  .vpc_gate()
  # Built with do.call so an omitted `weights` falls back to qpcstats' own
  # documented default weight vector (API block above) instead of passing
  # NULL through.
  args <- list(o = vpc_obj, alpha = alpha,
               sharp_ref = sharp_ref, interval_ref = interval_ref)
  if (!is.null(weights)) args$w <- weights
  do.call(tidyvpc::qpcstats, args)
}

# ---------------------------------------------------------------------------
# Preprocessing required by every VPC (vignette rule 1): keep only observation
# records in both datasets, and — for a categorical VPC — enforce the ordering
# the package requires (observed by subject/x; simulated by replicate/subject/x).
#
# Observation selection uses the ONE harness classifier
# harness_is_observation() (tools/record_selection.R), where the NONMEM event
# convention (EVID == 0 is an observation, EVID == 1 a dose) is asserted for
# every consumer. Two reasons this replaces the old `MDV == 0` filter (F9,
# Codex round 8): (1) SKILL_VPC.md and the rest of the harness select
# observations by EVID, so a VPC filtering on MDV could disagree with QC, EDA
# and fitting on which records are observations; (2) `obs[obs$MDV == 0, ]` kept
# rows whose MDV was NA (the subscript is NA, not FALSE), silently retaining
# non-observation rows. harness_is_observation() is a complete logical:
# NA EVID is FALSE (excluded), not NA.
#
# The observation-flag column is declared (`evid`, default "EVID"); it must be
# present in both datasets — a VPC that cannot tell observations from doses is
# a stop, not a silent no-op.
#
# When a replicate column is declared and present in `sim`, the simulation must
# be COMPLETE replicates: after observation selection every replicate carries
# exactly as many observation rows as the observed data. tidyvpc infers
# replicate boundaries from row order (see review notes at the top of this
# file), so an incomplete or ragged replicate set would misalign silently.
reference_vpc_prepare <- function(obs, sim, id = "ID", x = "TIME",
                                  repl = "REP", evid = "EVID",
                                  categorical = FALSE) {
  .vpc_gate()
  if (!exists("harness_is_observation", mode = "function"))
    source(file.path("tools", "record_selection.R"), local = TRUE)
  for (nm in c("obs", "sim")) {
    d <- get(nm)
    if (!evid %in% names(d))
      stop("reference_vpc_prepare: the ", nm, " data has no '", evid,
           "' column; observation selection uses the NONMEM event convention ",
           "(harness_is_observation, tools/record_selection.R). Declare the ",
           "observation-flag column via `evid` and carry it in both datasets.",
           call. = FALSE)
  }
  obs <- obs[harness_is_observation(obs[[evid]]), , drop = FALSE]
  sim <- sim[harness_is_observation(sim[[evid]]), , drop = FALSE]
  if (repl %in% names(sim)) {
    per <- vapply(split(seq_len(nrow(sim)), sim[[repl]]), length, integer(1))
    if (length(per) && any(per != nrow(obs)))
      stop("reference_vpc_prepare: simulation replicates are not complete — ",
           "after observation selection the observed data has ", nrow(obs),
           " rows but replicate(s) carry {", paste(sort(unique(per)),
           collapse = ", "), "} observation rows. tidyvpc infers replicate ",
           "boundaries from row order, so every replicate must hold exactly ",
           "the observed observation rows.", call. = FALSE)
  }
  if (isTRUE(categorical)) {
    obs <- obs[order(obs[[id]], obs[[x]]), , drop = FALSE]
    sim <- sim[order(sim[[repl]], sim[[id]], sim[[x]]), , drop = FALSE]
  }
  list(obs = obs, sim = sim)
}

# Attaches PRED from one simulation replicate onto the observed records, the
# preprocessing step prediction correction requires.
#
# Alignment is validated on the ID/time KEY, not merely the row count (F9,
# Codex round 8): equal row counts do not prove the replicate's records line
# up with the observed records position for position, and predcorrect() pairs
# them by position. The first replicate's (id, x) key must equal the observed
# (id, x) key element for element; any divergence is a stop.
reference_vpc_attach_pred <- function(obs, sim, repl = "REP", pred = "PRED",
                                      id = "ID", x = "TIME") {
  .vpc_gate()
  first <- sim[sim[[repl]] == sim[[repl]][1], , drop = FALSE]
  if (nrow(first) != nrow(obs)) {
    stop("Cannot attach ", pred, ": one simulation replicate has ",
         nrow(first), " rows but the observed data has ", nrow(obs),
         ". Align the datasets (both filtered to observations) first.",
         call. = FALSE)
  }
  for (k in c(id, x)) {
    if (!k %in% names(obs) || !k %in% names(first))
      stop("Cannot attach ", pred, ": key column '", k, "' is absent from ",
           "the observed or the simulated data; exact key alignment requires ",
           "both.", call. = FALSE)
    if (!isTRUE(all.equal(obs[[k]], first[[k]], check.attributes = FALSE)))
      stop("Cannot attach ", pred, ": the first simulation replicate's '", k,
           "' key does not match the observed records position for position. ",
           "Equal row counts are not enough — the records must align on (",
           id, ", ", x, "). Sort both datasets into the same record order ",
           "first.", call. = FALSE)
  }
  obs[[pred]] <- first[[pred]]
  obs
}

# Bin-placement check: the vignettes advise inspecting bins BEFORE computing
# statistics. Returns the pre-vpcstats object, whose plot method renders bin
# placement only.
reference_vpc_bincheck <- function(obs, sim, x, yobs, ysim,
                                   binning_spec, strata = NULL) {
  .vpc_gate()
  o <- tidyvpc::observed(obs, x = !!rlang::sym(x), yobs = !!rlang::sym(yobs))
  o <- tidyvpc::simulated(o, sim, ysim = !!rlang::sym(ysim))
  if (!is.null(strata)) o <- tidyvpc::stratify(o, strata)
  apply_binning(o, binning_spec)
}

`%||%` <- function(a, b) if (is.null(a)) b else a
