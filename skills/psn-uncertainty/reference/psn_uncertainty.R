# REFERENCE IMPLEMENTATION — PsN uncertainty family (bootstrap, sir, llp).
#
# AUTHORITY: skills/psn-uncertainty/MANIFEST.yaml is the authority for the SME signature
# (root reviewed_by / reviewed_at) and for the per-unit status and digests (bootstrap, sir, llp).
# This header carries no status claim of its own -- read the manifest.
# Review and confirm the intended-use unit locally before running it.
#
# The one rule: this file COMPUTES; agents call it with declared
# parameters. It never invents paths — every path is a parameter. It never
# authors numerics — samples/resamples/stratify_on/seed and the
# theta/omega/sigma selection are MAP-declared and passed in. Operational
# flags (threads, retries, tweak_inits, picky, mpi) are read from a passed
# engine_config list (parsed config/engine.yaml), never hardcoded.
#
# Every PsN call goes through the provision dispatcher
# (tools/nonmem_exec.sh <workdir> <tool> <args>, the provision dispatcher);
# under the CONTAINERIZED provision the workdir MUST be
# inside a git repository (skills/nonmem-estimation/SKILL.md, control-stream
# requirements — not restated here). No config file is used: bootstrap,
# sir, and llp are invoked from the command line.
#
# Results-file parsing carries declared column expectations; any column not
# yet seen on a real run is flagged "unverified — confirm at first real
# run" rather than guessed.

# ---------------------------------------------------------------------------
# Self-gate. TRUE only when skills/psn-uncertainty/MANIFEST.yaml carries a
# confirmed root (non-empty reviewed_by) AND the called unit is status:
# approved with this file's current sha256 (same manifest mechanism as
# skills/simulation-standards/reference/uncertainty.R). While the manifest
# root is unsigned every entry point below refuses to run; filling the root
# reviewed_by/reviewed_at in the manifest and approving the unit IS the act
# of signing.
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

.mm_trusted_invocation <- base::local({
  d <- base::new.env(parent = base::baseenv())
  if (!base::file.exists("tools/psn_invocation.R"))
    base::stop("this reference requires tools/psn_invocation.R; references are ",
               "sourced with the repository root as the working directory.",
               call. = FALSE)
  base::sys.source("tools/psn_invocation.R", envir = d, keep.source = FALSE)
  base::lockEnvironment(d, bindings = TRUE)
  d
}, envir = base::new.env(parent = base::baseenv()))

.psn_new_run_dir <- base::local({
  sealed <- .mm_trusted_invocation
  function(...) base::get("psn_invocation_reserve", envir = sealed)(...)
}, envir = base::new.env())
.psn_find_artifact <- base::local({
  sealed <- .mm_trusted_invocation
  function(...) base::get("psn_invocation_find", envir = sealed)(...)
}, envir = base::new.env())
.psn_write_invocation_manifest <- base::local({
  sealed <- .mm_trusted_invocation
  function(...) base::get("psn_invocation_finalize", envir = sealed)(...)
}, envir = base::new.env())

psn_uncertainty_validated <- base::local({
  private <- .mm_trusted_private
  function(unit) {
    # Integrity batch 3a: delegates to the per-unit method-manifest self-gate
    # core. `unit` is the intended-use unit of the calling entry point
    # (bootstrap | sir | llp) -- the signed scope is bootstrap-only, so sir and
    # llp are draft units and refuse post-cutover while bootstrap passes; the
    # gate computes only when skills/psn-uncertainty/MANIFEST.yaml has a confirmed
    # root and the unit is approved (the legacy "Reviewed by:" grep was retired
    # in batch 3c).
    base::get("require_validated_method", envir = private)(
      skill_dir    = "skills/psn-uncertainty",
      calling_file = "skills/psn-uncertainty/reference/psn_uncertainty.R",
      unit         = unit)
  }
}, envir = base::new.env())

.psn_uncertainty_gate <- function(unit) {
  if (!psn_uncertainty_validated(unit)) {
    base::stop("psn-uncertainty unit '", unit, "' is not SME-validated (either the ",
         "manifest unit is draft/rejected, or skills/psn-uncertainty/MANIFEST.yaml ",
         "has an empty root reviewed_by); this lane ",
         "does not run until its unit is approved. ESCALATE to the SME.",
         call. = FALSE)
  }
  base::invisible(TRUE)
}

# `%||%` — NULL-coalesce, defined locally so the reference sources
# standalone (base R only ships %||% from 4.4).
`%||%` <- function(a, b) if (is.null(a)) b else a

# ---------------------------------------------------------------------------
# Operational flags -> PsN command-line arguments.
#
# Reads ONLY the operational psn block of config/engine.yaml (threads,
# retries, tweak_inits, picky, clean, model_subdir, grid submission,
# within-run MPI). Statistical flags are never sourced here. Flag names
# verified against the installed PsN 5.7.1 help (2026-08-31): -threads,
# -retries, -tweak_inits, -picky, -clean, -model_subdir, -run_on_sge /
# -run_on_slurm / -run_on_torque, -sge_prepend_flags /
# -slurm_prepend_flags / -torque_prepend_flags, -sge_queue /
# -slurm_partition / -torque_queue, -slurm_account, -nodes, -parafile.
# psn.conf is site-owned and never written; these explicit flags override
# its defaults so the invocation is auditable in the gate package.
#
# mpi_ok = FALSE (sir) DROPS the within-run MPI settings (nodes,
# parafile) with a message rather than emitting them: the sir user guide
# prohibits parallel (MPI) NONMEM — silent wrong-uncertainty failure.
# .psn_assert_no_mpi() separately refuses EXPLICIT caller-supplied MPI
# tokens; this drop covers the config-driven side.
#
# Returns a character vector of "-opt=val" / "-opt" / "-no-opt" tokens.
.psn_operational_args <- function(engine_config, mpi_ok = TRUE) {
  psn <- engine_config$psn
  if (is.null(psn)) {
    stop("config/engine.yaml is missing the psn: operational block; ",
         "the uncertainty lane needs threads/retries/tweak_inits/picky.",
         call. = FALSE)
  }
  args <- character(0)
  if (!is.null(psn$threads)) args <- c(args, paste0("-threads=", psn$threads))
  if (!is.null(psn$retries)) args <- c(args, paste0("-retries=", psn$retries))
  # NONMEM version label (psn.conf [nm_versions]); declared per run.
  if (!is.null(psn$nm_version)) args <- c(args, paste0("-nm_version=", psn$nm_version))
  if (!is.null(psn$tweak_inits)) {
    args <- c(args, if (isTRUE(psn$tweak_inits)) "-tweak_inits"
                    else "-no-tweak_inits")
  }
  if (!is.null(psn$picky)) {
    args <- c(args, if (isTRUE(psn$picky)) "-picky" else "-no-picky")
  }
  if (!is.null(psn$clean)) args <- c(args, paste0("-clean=", psn$clean))
  if (!is.null(psn$model_subdir)) {
    args <- c(args, if (isTRUE(psn$model_subdir)) "-model_subdir"
                    else "-no-model_subdir")
  }

  # Grid submission (operational routing; queue/account values are site
  # facts the SME put in the committed config deliberately).
  submit <- psn$submit %||% "local"
  if (!identical(submit, "local")) {
    grid <- match.arg(submit, c("sge", "slurm", "torque"))
    args <- c(args, paste0("-run_on_", grid))
    if (!is.null(psn$prepend_flags)) {
      args <- c(args, paste0("-", grid, "_prepend_flags=", psn$prepend_flags))
    }
    if (!is.null(psn$queue)) {
      queue_flag <- switch(grid, sge = "-sge_queue=",
                           slurm = "-slurm_partition=",
                           torque = "-torque_queue=")
      args <- c(args, paste0(queue_flag, psn$queue))
    }
    if (identical(grid, "slurm") && !is.null(psn$account)) {
      args <- c(args, paste0("-slurm_account=", psn$account))
    }
  }

  # Within-run NONMEM MPI. nodes > 1 requires a parafile; sir drops both.
  nodes <- psn$nodes %||% 1L
  if (nodes > 1 || !is.null(psn$parafile)) {
    if (!mpi_ok) {
      message("psn config sets nodes/parafile, but this tool prohibits ",
              "MPI NONMEM (sir user guide); dropping the within-run ",
              "parallel settings for this call.")
    } else {
      if (nodes > 1 && is.null(psn$parafile)) {
        stop("psn.nodes > 1 requires psn.parafile in config/engine.yaml ",
             "(the NONMEM MPI parafile, e.g. mpilinux8.pnm).", call. = FALSE)
      }
      args <- c(args, paste0("-nodes=", nodes))
      if (!is.null(psn$parafile)) {
        args <- c(args, paste0("-parafile=", psn$parafile))
      }
    }
  }
  args
}

# ---------------------------------------------------------------------------
# Executor invocation. Runs the provision dispatcher (tools/nonmem_exec.sh
# <workdir> <tool> <args>) via system2 and stops on a non-zero exit —
# containerized or native site install per config/engine.yaml. Returns the
# captured stdout invisibly. The executor path is a parameter (house default
# "tools/nonmem_exec.sh"); resolved relative to repo_root when relative.
# Tool-name -> intended-use unit map. The runner gates on the mapped unit
# BEFORE any system2, so this lane is safe even when .psn_run is called
# directly (not only through the public reference_psn_* entry points, which
# also gate). An unrecognised tool name is a HARD STOP — never an ungated run.
.PSN_UNCERTAINTY_TOOL_UNIT <- c(bootstrap = "bootstrap", sir = "sir",
                                llp = "llp")

.psn_run <- function(workdir, tool, args, executor, repo_root,
                     invocation = NULL) {
  unit <- unname(.PSN_UNCERTAINTY_TOOL_UNIT[tool])
  if (length(tool) != 1 || is.na(unit)) {
    stop(".psn_run: unknown PsN tool '", tool, "' — the runner gates on a ",
         "mapped intended-use unit before executing, and refuses an unmapped ",
         "tool rather than run ungated. Known tools: ",
         paste(names(.PSN_UNCERTAINTY_TOOL_UNIT), collapse = ", "), ".",
         call. = FALSE)
  }
  .psn_uncertainty_gate(unit)
  exec <- executor
  if (!grepl("^/", exec)) exec <- file.path(repo_root, exec)
  exec <- normalizePath(exec, mustWork = TRUE)
  if (!dir.exists(workdir)) {
    stop("workdir does not exist: ", workdir, call. = FALSE)
  }
  full <- c(normalizePath(workdir, mustWork = TRUE), tool, args)
  run_env <- if (is.null(invocation)) character(0) else
    paste0("PMX_INVOCATION_ID=", invocation$name)
  out <- suppressWarnings(system2(exec, shQuote(full), env = run_env,
                                  stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status")
  if (!is.null(status) && status != 0L) {
    stop("PsN ", tool, " failed (exit ", status, "):\n",
         paste(out, collapse = "\n"), call. = FALSE)
  }
  if (is.null(invocation)) return(invisible(out))
  list(status = status %||% 0L, stdout = out,
       cmd = paste(c(shQuote(exec), shQuote(full)), collapse = " "),
       argv = full)
}

# ---------------------------------------------------------------------------
# reference_psn_bootstrap_precheck(ctl_file)
#
# Scans a control stream for IGNORE/ACCEPT expressions that reference the
# ID column and HARD-STOPS if any is found. PsN renumbers individuals during
# bootstrap resampling, so an ID-based IGNORE/ACCEPT filters on the wrong
# individuals after renumbering — silently, with no error. A model needing
# ID-based record selection must have it baked into the analysis dataset
# before bootstrap, not expressed as a control-stream IGNORE/ACCEPT.
#
# ctl_file   path to the NONMEM control stream (.mod / .ctl).
# id_col     the ID column name as it appears in $INPUT (default "ID").
#
# Returns invisibly TRUE when clean; stops with an explanation on a hit.
reference_psn_bootstrap_precheck <- function(ctl_file, id_col = "ID") {
  # Gated (integrity batch 7): a manifested entry point of the approved
  # bootstrap unit; the entry-point gating audit requires every public
  # function of an approved unit to consult the gate.
  .psn_uncertainty_gate("bootstrap")
  if (!file.exists(ctl_file)) {
    stop("control stream not found: ", ctl_file, call. = FALSE)
  }
  lines <- readLines(ctl_file, warn = FALSE)

  # Restrict the scan to $DATA / $INFN records, where IGNORE/ACCEPT live.
  # A record runs from its $-tag to the next $-tag.
  starts <- grep("^\\s*\\$", lines)
  is_data <- grepl("^\\s*\\$(DATA|INFN)", lines[starts], ignore.case = TRUE)
  data_lines <- character(0)
  for (k in which(is_data)) {
    from <- starts[k]
    # record runs from its $-tag to the line before the next $-tag (or EOF
    # when $DATA/$INFN is the last record).
    nxt  <- starts[starts > from]
    to   <- if (length(nxt)) min(nxt) - 1L else length(lines)
    data_lines <- c(data_lines, lines[from:to])
  }
  if (!length(data_lines)) return(invisible(TRUE))

  # Strip inline comments (NONMEM comment char is ';').
  code <- sub(";.*$", "", data_lines)

  # IGNORE/ACCEPT expressions referencing the ID column. Matches
  # IGNORE=(ID.GT.10), ACCEPT=(ID.EQ.1), IGNORE(ID>4) and spacing variants.
  # \\b<id_col>\\b inside a parenthesised IGNORE/ACCEPT clause.
  clause <- grepl("(IGNORE|ACCEPT)\\s*[=(]", code, ignore.case = TRUE)
  hits <- code[clause & grepl(paste0("\\b", id_col, "\\b"), code)]

  if (length(hits)) {
    stop("bootstrap precheck FAILED: the control stream has IGNORE/ACCEPT ",
         "expression(s) referencing the ID column (", id_col, "):\n  ",
         paste(trimws(hits), collapse = "\n  "),
         "\nPsN renumbers individuals during bootstrap resampling, so an ",
         "ID-based IGNORE/ACCEPT filters the WRONG individuals after ",
         "renumbering — silently, with no error. Bake this selection into ",
         "the analysis dataset before bootstrap instead. (Related: the ",
         "dataset is not filtered on IGNORE/ACCEPT before bootstrapping, so ",
         "record-removing IGNOREs also shrink the samples.)", call. = FALSE)
  }
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# reference_psn_bootstrap(...)
#
# Runs PsN bootstrap through the containerized executor after the ID-based
# IGNORE/ACCEPT precheck. Builds the command vector, executes, and returns
# the decisive artifact paths plus a parsed summary.
#
# workdir      directory holding the control stream; MUST be inside a git
#              repo (container mount rule).
# ctl_file     control-stream filename (relative to workdir, or absolute).
# samples      MAP-declared number of bootstrap datasets. Checked against
#              the MODEL_RISK floor (bootstrap.R D1: 500 medium, 1000 high).
# seed         REQUIRED, MAP-declared. No default; reproducibility.
# engine_config parsed config/engine.yaml (list) — operational flags only.
# model_risk   selects the D1 floor for `samples`.
# stratify_on  MAP-declared stratified-resampling column; NULL = unstratified
#              (bootstrap.R D2: no house default).
# bca          MAP-declared; TRUE requests BCa CIs, else percentile (D4).
# dofv         MAP-declared; TRUE adds -dofv (delta-OFV via MAXEVAL=0).
# id_col       ID column for the precheck (default "ID").
# executor     containerized executor path (house default).
# repo_root    base for resolving relative executor path; getwd() in-pipeline.
#
# Returns list(results_csv, raw_results_csv, summary, record). `summary` is
# a data.frame parsed from bootstrap_results.csv with declared column
# expectations; unverified columns are flagged in the parser.
reference_psn_bootstrap <- function(workdir, ctl_file, samples, seed,
                                    engine_config,
                                    model_risk = c("medium", "high"),
                                    stratify_on = NULL, bca = FALSE,
                                    dofv = FALSE, id_col = "ID",
                                    executor = "tools/nonmem_exec.sh",
                                    repo_root = getwd()) {
  .psn_uncertainty_gate("bootstrap")
  stopifnot(!missing(seed), !missing(samples))
  model_risk <- match.arg(model_risk)

  # Do not author numerics; do enforce the declared floor. Never lower
  # samples to make progress (bootstrap.R D1).
  floor_n <- c(medium = 500, high = 1000)[[model_risk]]
  if (samples < floor_n) {
    stop("samples ", samples, " is below the house minimum of ", floor_n,
         " for MODEL_RISK ", model_risk, " (bootstrap.R D1); raise samples ",
         "or record a deviation in the MAP.", call. = FALSE)
  }

  ctl_path <- if (grepl("^/", ctl_file)) ctl_file
              else file.path(workdir, ctl_file)
  reference_psn_bootstrap_precheck(ctl_path, id_col = id_col)

  # Command vector. Method flags first (declared), then operational.
  args <- c(basename(ctl_file),
            paste0("-samples=", samples),
            paste0("-seed=", seed))
  if (!is.null(stratify_on)) args <- c(args, paste0("-stratify_on=", stratify_on))
  if (isTRUE(bca))  args <- c(args, "-bca")
  if (isTRUE(dofv)) args <- c(args, "-dofv")
  args <- c(args, .psn_operational_args(engine_config))

  run_dir <- .psn_new_run_dir(workdir, "bootstrap")
  args <- c(args, paste0("-directory=", run_dir$name))
  run <- .psn_run(workdir, "bootstrap", args, executor, repo_root,
                  invocation = run_dir)

  results_csv <- .psn_find_artifact(run_dir, "bootstrap_results.csv")
  raw_csv <- .psn_find_artifact(run_dir, "^raw_results.*\\.csv$", regex = TRUE)
  invocation_manifest <- .psn_write_invocation_manifest(
    run_dir, run, list(results_csv = results_csv, raw_results_csv = raw_csv))

  list(
    results_csv     = results_csv,
    raw_results_csv = raw_csv,
    run_dir         = run_dir$path,
    invocation_manifest = invocation_manifest,
    summary         = .psn_parse_bootstrap_results(results_csv),
    record = list(tool = "bootstrap", samples = samples, seed = seed,
                  model_risk = model_risk,
                  stratify_on = stratify_on %||% "none (per-MAP)",
                  bca = bca, dofv = dofv,
                  operational = engine_config$psn))
}

# ---------------------------------------------------------------------------
# reference_psn_sir(...)
#
# Runs PsN sir through the executor. REFUSES any MPI/parafile argument: the
# sir guide prohibits parallel (MPI) NONMEM — it silently produces wrong
# uncertainty. The harness runs non-MPI nmfe (config/engine.yaml
# psn.mpi: false); this refusal is defense in depth.
#
# sir needs a proposal distribution: the input model's $COVARIANCE step by
# default, or an external proposal via covmat_input / rawres_input. A model
# with neither cannot run sir — that is a precondition the MAP asserts, not
# a runtime fallback here.
#
# samples/resamples   MAP-declared per-iteration draw/retain series.
# seed                REQUIRED, MAP-declared.
# covmat_input/rawres_input  optional external proposal sources (paths).
# ...                 extra tokens are inspected for MPI/parafile and refused.
#
# Returns list(results_csv, sir_cov, iterations_csv, summary, record).
reference_psn_sir <- function(workdir, ctl_file, samples, resamples, seed,
                              engine_config, covmat_input = NULL,
                              rawres_input = NULL,
                              executor = "tools/nonmem_exec.sh",
                              repo_root = getwd(), ...) {
  .psn_uncertainty_gate("sir")
  stopifnot(!missing(seed), !missing(samples), !missing(resamples))

  extra <- list(...)
  .psn_assert_no_mpi(names(extra), unlist(extra))

  args <- c(basename(ctl_file),
            paste0("-samples=", paste(samples, collapse = ",")),
            paste0("-resamples=", paste(resamples, collapse = ",")),
            paste0("-seed=", seed))
  if (!is.null(covmat_input)) args <- c(args, paste0("-covmat_input=", covmat_input))
  if (!is.null(rawres_input)) args <- c(args, paste0("-rawres_input=", rawres_input))
  # Shared operational builder with mpi_ok = FALSE: sir gets the full
  # operational set (threads/retries/tweak_inits/picky are PsN COMMON
  # options governing the NONMEM runs sir performs, and grid submission
  # applies to sir like any tool) but the within-run MPI settings
  # (nodes/parafile) are DROPPED — the sir user guide prohibits MPI
  # NONMEM (silent wrong-uncertainty failure).
  psn <- engine_config$psn %||%
    stop("config/engine.yaml is missing the psn: block.", call. = FALSE)
  args <- c(args, .psn_operational_args(engine_config, mpi_ok = FALSE))

  run_dir <- .psn_new_run_dir(workdir, "sir")
  args <- c(args, paste0("-directory=", run_dir$name))
  run <- .psn_run(workdir, "sir", args, executor, repo_root,
                  invocation = run_dir)

  # Select the empirical covariance by model stem to exclude iteration matrices.
  model_stem <- sub("\\.[^.]*$", "", basename(ctl_file))
  found <- .psn_select_and_finalize(run_dir, run, list(
    results_csv    = "sir_results.csv",
    sir_cov        = paste0(model_stem, "_sir.cov"),
    iterations_csv = "summary_iterations.csv"))
  results_csv <- found$outputs$results_csv
  sir_cov <- found$outputs$sir_cov
  iterations_csv <- found$outputs$iterations_csv

  list(
    results_csv    = results_csv,
    sir_cov        = sir_cov,
    iterations_csv = iterations_csv,
    run_dir        = run_dir$path,
    invocation_manifest = found$invocation_manifest,
    summary        = .psn_parse_sir_results(results_csv, iterations_csv),
    record = list(tool = "sir", samples = samples, resamples = resamples,
                  seed = seed,
                  proposal = if (!is.null(rawres_input)) "rawres_input"
                             else if (!is.null(covmat_input)) "covmat_input"
                             else "model covariance step",
                  operational = list(threads = psn$threads,
                                     retries = psn$retries, mpi = FALSE)))
}

# Refuses MPI/parafile arguments (sir correctness requirement). Scans both
# argument names and values for parafile / mpi / nodes tokens.
.psn_assert_no_mpi <- function(arg_names, arg_values) {
  hay <- tolower(paste(c(arg_names, arg_values), collapse = " "))
  if (grepl("parafile|\\bmpi\\b|-nodes|parafile_", hay)) {
    stop("sir refuses MPI/parafile arguments: the sir user guide prohibits ",
         "parallel (MPI) NONMEM — it silently produces wrong uncertainty ",
         "estimates. The harness runs non-MPI nmfe (config/engine.yaml ",
         "psn.mpi: false). Remove the parafile/MPI argument.", call. = FALSE)
  }
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# reference_psn_llp(...)
#
# Runs PsN llp (log-likelihood profiling) through the executor. NO seed:
# llp is deterministic (verified against llp_userguide.tex, master,
# 2026-08-31 — no -seed option). Slow: profiles one parameter at a time;
# profile only the parameters that need a profile CI.
#
# thetas/omegas/sigmas   MAP-declared parameter-number selections (numeric
#                        vectors); at least one must be non-NULL.
# rse_thetas/rse_omegas/rse_sigmas  RSE (%) vectors, mandatory when no .lst
#                        file with SE estimates exists.
#
# Returns list(results_csv, summary, record).
reference_psn_llp <- function(workdir, ctl_file, thetas = NULL, omegas = NULL,
                              sigmas = NULL, engine_config,
                              rse_thetas = NULL, rse_omegas = NULL,
                              rse_sigmas = NULL,
                              executor = "tools/nonmem_exec.sh",
                              repo_root = getwd()) {
  .psn_uncertainty_gate("llp")
  if (is.null(thetas) && is.null(omegas) && is.null(sigmas)) {
    stop("llp needs an explicit parameter selection: at least one of ",
         "thetas / omegas / sigmas (MAP-declared).", call. = FALSE)
  }

  args <- basename(ctl_file)
  add_sel <- function(a, flag, v) if (is.null(v)) a
    else c(a, paste0("-", flag, "=", paste(v, collapse = ",")))
  args <- add_sel(args, "thetas", thetas)
  args <- add_sel(args, "omegas", omegas)
  args <- add_sel(args, "sigmas", sigmas)
  args <- add_sel(args, "rse_thetas", rse_thetas)
  args <- add_sel(args, "rse_omegas", rse_omegas)
  args <- add_sel(args, "rse_sigmas", rse_sigmas)

  psn <- engine_config$psn %||%
    stop("config/engine.yaml is missing the psn: block.", call. = FALSE)
  args <- c(args, .psn_operational_args(engine_config))

  run_dir <- .psn_new_run_dir(workdir, "llp")
  args <- c(args, paste0("-directory=", run_dir$name))
  run <- .psn_run(workdir, "llp", args, executor, repo_root,
                  invocation = run_dir)

  found <- .psn_select_and_finalize(run_dir, run,
                                    list(results_csv = "llp_results.csv"))
  results_csv <- found$outputs$results_csv
  list(
    results_csv = results_csv,
    run_dir = run_dir$path,
    invocation_manifest = found$invocation_manifest,
    summary     = .psn_parse_llp_results(results_csv),
    record = list(tool = "llp", thetas = thetas, omegas = omegas,
                  sigmas = sigmas,
                  rse = list(thetas = rse_thetas, omegas = rse_omegas,
                             sigmas = rse_sigmas),
                  operational = list(threads = psn$threads,
                                     retries = psn$retries)))
}

# Artifact selection for sir and llp with the invocation marker finalized
# first (PF-2, critic G7; units programme U1, 2026-09-28). Each selection
# runs under tryCatch. If the run directory exists, the marker is finalized
# with the artifacts found (finalization drops NA outputs) and the first
# selection error, if any, is then re-signalled unchanged. If the run
# directory was never created, finalization is skipped (it would stop on the
# missing directory) and the selection's own "was not created" error is
# re-signalled; the marker stays `state: started`. A stop inside
# finalization propagates as it is. `state: completed` therefore means the
# executor returned and the run directory existed, not that the artifacts
# are valid. The bootstrap entry point keeps its own order (selection before
# finalization); it is not routed through here.
#
# patterns  named list: output name -> exact artifact basename.
# Returns list(outputs, invocation_manifest).
.psn_select_and_finalize <- function(run_dir, run, patterns) {
  first_error <- NULL
  outputs <- lapply(patterns, function(pattern)
    tryCatch(.psn_find_artifact(run_dir, pattern), error = function(e) {
      if (is.null(first_error)) first_error <<- e
      NA_character_
    }))
  if (!dir.exists(run_dir$path)) stop(first_error)
  invocation_manifest <- .psn_write_invocation_manifest(run_dir, run, outputs)
  if (!is.null(first_error)) stop(first_error)
  list(outputs = outputs, invocation_manifest = invocation_manifest)
}

# ---------------------------------------------------------------------------
# Parsers. Each reads the decisive results CSV with DECLARED column
# expectations. The exact PsN column headers are unverified until a real
# run — the parser reads the CSV verbatim and does not rename or reshape;
# the "expected" columns below are documented for the gate, and a mismatch
# is reported rather than silently coerced.

# bootstrap_results.csv is a multi-section file with sections such as
# table — section title lines (diagnostic.means, means, bias,
# standard.error, percentile blocks, ...) each followed by their own
# quoted header row and value rows. A plain read.csv mangles it; the
# per-sample raw_results_<model>.csv is the flat, parse-friendly audit
# artifact (see .psn_parse_raw_results note below). Verified there too:
# parameter column headers embed the control stream's INLINE COMMENTS
# verbatim, commas included ("THETA(1) tcl: log CL at WT_ref ..."), so
# selection must be by pattern (^THETA\(1\), ^OMEGA\(1,1\)) with
# quote-aware parsing, never by fixed names. This function returns the
# raw section lines for the gate record rather than pretending the file
# is tabular.
.psn_parse_bootstrap_results <- function(path) {
  lines <- readLines(path, warn = FALSE)
  structure(list(lines = lines),
            sections = grep("^[a-z]", lines, value = TRUE),
            note = paste("Multi-section PsN results file; quantitative parsing uses",
                         "raw_results_<model>.csv"))
}

# raw_results_<model>.csv is the per-sample audit table. Row 1 is the original
# fit, bootstrap samples follow; `minimization_successful` carries the
# per-sample termination status; parameter columns are named with the
# control stream's inline comments embedded verbatim, so they are
# selected by REGEX on the parameter root (quote-aware read.csv handles
# the embedded commas). Returns list(estimates, success, n_samples).
#
# param_patterns  named character vector: result name -> regex matching
#                 the column start, e.g. c(THETA1 = "^THETA\\(1\\)").
.psn_parse_raw_results <- function(path, param_patterns) {
  df <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  nm <- names(df)
  cols <- vapply(param_patterns,
                 function(p) grep(p, nm, value = TRUE)[1], character(1))
  if (anyNA(cols)) {
    stop("raw_results: no column matches pattern(s) ",
         paste(names(param_patterns)[is.na(cols)], collapse = ", "),
         " — headers embed control-stream comments; check the patterns ",
         "against names(read.csv(path, check.names = FALSE)).",
         call. = FALSE)
  }
  est <- df[-1, cols, drop = FALSE]   # row 1 = original fit, not a sample
  names(est) <- names(param_patterns)
  list(estimates = est,
       success   = df$minimization_successful[-1] == 1,
       n_samples = nrow(df) - 1L)
}

# The bootstrap entrypoint locates and parses the per-sample results.
# nothing new. It adds refusals only:
#   - the file must carry a `model` column whose first data row is PsN's
#     original-fit marker 0 and whose other rows are sample markers (positive
#     whole numbers); the parity file of 2026-08-31 reads 0 then 1..200;
#   - every column of .PSN_SAMPLE_FLAG_COLUMNS and minimization_successful
#     must be present.
# It returns the helper's list(estimates, success, n_samples) unchanged, plus
# the RAW minimization_successful column of the sample rows (so that the
# replicate accounting of the diagnostics unit bootstrap_agreement validates
# the flag domain before any conversion to logical: the helper's `== 1` turns
# an invalid flag into FALSE, P-1.4-1) and the flag columns of the sample rows.
# ponytail: the file is read twice (here and in the helper) so that the
# helper's bytes stay unchanged; fold the reads together at the helper's next
# re-signature if the size of a per-sample file ever matters.
.PSN_SAMPLE_FLAG_COLUMNS <- c("covariance_step_run", "covariance_step_successful",
                              "covariance_step_warnings", "condition_number",
                              "estimate_near_boundary", "rounding_errors",
                              "zero_gradients", "final_zero_gradients",
                              "hessian_reset", "s_matrix_singular")

reference_psn_bootstrap_samples <- function(path, param_patterns) {
  .psn_uncertainty_gate("bootstrap")
  df <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  miss <- setdiff(c("model", "minimization_successful", .PSN_SAMPLE_FLAG_COLUMNS),
                  names(df))
  if (length(miss))
    stop("raw_results: missing column(s) ", paste(miss, collapse = ", "),
         "; the per-sample file does not have the layout verified on 2026-08-31.",
         call. = FALSE)
  if (nrow(df) < 2L)
    stop("raw_results: no sample rows after the original fit.", call. = FALSE)
  m <- df$model
  if (!is.numeric(m) || is.na(m[1]) || m[1] != 0)
    stop("raw_results: the first data row does not carry the original fit marker ",
         "(model 0); refusing to guess which row is the original fit.", call. = FALSE)
  s <- m[-1]
  if (anyNA(s) || any(s < 1 | s != floor(s)))
    stop("raw_results: every row after the first must carry a sample marker ",
         "(model a positive whole number).", call. = FALSE)
  c(.psn_parse_raw_results(path, param_patterns),
    list(minimization_successful = df$minimization_successful[-1],
         flags = df[-1, .PSN_SAMPLE_FLAG_COLUMNS, drop = FALSE]))
}

# sir_results.csv is a multi-section file with unquoted section title
# lines ("SIR run info", "Summary statistics over resamples", "Quantiles (R
# type=2)", "Empirical sd-correlation matrix"), each followed by a quoted
# header row with an empty first field and parameter columns that embed the
# control stream's inline comments, then quoted row-labelled rows; every
# row ends with a trailing comma, and a blank separator (" " and an empty
# quoted label) sits inside the quantile section. A plain read.csv stops on
# it. Returned as lines with a section index (0 before the first title), as
# the bootstrap parser returns its file; a missing title is reported in
# `missing_sections`, not coerced.
# summary_iterations.csv — a flat table, header VERIFIED on the same run;
# read as is, a missing column reported in `missing_iteration_columns`.
# Whether the iterations converged is the SME's judgement, not declared here.
.psn_parse_sir_results <- function(path, iterations_csv) {
  lines <- readLines(path, warn = FALSE)
  title <- grepl("^[A-Za-z][^,]*$", lines)
  iterations <- utils::read.csv(iterations_csv, check.names = FALSE,
                                stringsAsFactors = FALSE)
  structure(list(lines = lines, section = cumsum(title), iterations = iterations),
            sections = lines[title],
            missing_sections = setdiff(c("SIR run info",
                                         "Summary statistics over resamples",
                                         "Quantiles (R type=2)",
                                         "Empirical sd-correlation matrix"),
                                       lines[title]),
            missing_iteration_columns = setdiff(
              c("iteration", "commandline.samples", "attempted.samples",
                "successful.samples", "commandline.resamples", "actual.resamples",
                "requested.ratio", "actual.ratio", "negative.dOFV",
                "minimum.sample.ofv"),
              names(iterations)),
            note = "Multi-section PsN SIR results file; iterations table read as is")
}

# llp_results.csv has an unquoted confidence.intervals section title,
# then a quoted header row (empty first field, lower,
# maximum.likelihood.estimate, upper, interval.ratio, near.bound,
# max.iterations, trailing comma) and one quoted row per profiled parameter,
# labelled with the parameter column name including its inline comment. A
# plain read.csv stops on it ("more columns than column names"). Returned
# as lines with a section index, like the sir parser.
.psn_parse_llp_results <- function(path) {
  lines <- readLines(path, warn = FALSE)
  title <- grepl("^[A-Za-z][^,]*$", lines)
  structure(list(lines = lines, section = cumsum(title)),
            sections = lines[title],
            missing_sections = setdiff("confidence.intervals", lines[title]),
            note = "Multi-section PsN LLP results file")
}
