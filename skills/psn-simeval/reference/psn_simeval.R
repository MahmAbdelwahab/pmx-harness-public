# REFERENCE IMPLEMENTATION — PsN simulation-evaluation family (vpc / npc /
# sse) through the containerized provision.
#
# AUTHORITY: skills/psn-simeval/MANIFEST.yaml is the authority for the SME signature
# (root reviewed_by / reviewed_at) and for the per-unit status and digests (vpc, npc, sse).
# This header carries no status claim of its own -- read the manifest.
# Review and confirm the intended-use unit locally before running it.
# The PsN documentation describes the command and output formats.
#
# The one rule: this file COMPUTES; agents call it with declared
# parameters. It never invents paths — every path is a parameter. Method
# decisions (samples, bin rules, stratification, PI level, censoring,
# predcorr, alternative_models) come from the analysis declaration / MAP.
# Operational defaults (threads, retries, tweak_inits, picky) come from the
# passed engine_config list, read by the caller via
# yaml::read_yaml("config/engine.yaml") — same pattern as
# skills/nonmem-estimation/reference/bbr_estimation.R.
#
# Requires: a working NONMEM/PsN provision behind tools/nonmem_exec.sh
# (containerized wrapper or native site install per config/engine.yaml)
# and the git-repo mount precondition (see the estimation lane skill). R
# needs only base + utils here; results parsing uses utils::read.csv.

# `%||%` — NULL-coalesce (defined locally so the reference has no hard
# rlang dependency; mirrors bbr_estimation.R).
`%||%` <- function(a, b) if (is.null(a)) b else a

# Self-gate. TRUE only when skills/psn-simeval/MANIFEST.yaml carries a
# confirmed root (non-empty reviewed_by) AND the called unit is status:
# approved with this file's current sha256 (same manifest mechanism as
# psn_uncertainty.R and simulation-standards/reference/uncertainty.R). While
# the manifest root is unsigned every entry point refuses to run; filling the
# root reviewed_by/reviewed_at in the manifest and approving the unit IS the
# act of signing.
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
  function(run, filename, required = TRUE)
    base::get("psn_invocation_find", envir = sealed)(run, filename,
                                                      required = required)
}, envir = base::new.env())
.psn_write_invocation_manifest <- base::local({
  sealed <- .mm_trusted_invocation
  function(...) base::get("psn_invocation_finalize", envir = sealed)(...)
}, envir = base::new.env())

psn_simeval_validated <- base::local({
  private <- .mm_trusted_private
  function(unit) {
    # Integrity batch 3a: delegates to the per-unit method-manifest self-gate
    # core. `unit` is the intended-use unit of the calling entry point
    # (vpc | npc | sse) -- the signed scope is vpc-only, so npc and sse are
    # draft units and refuse post-cutover while vpc passes; the gate computes
    # only when skills/psn-simeval/MANIFEST.yaml has a confirmed root and the
    # unit is approved (the legacy "Reviewed by:" grep was retired in batch 3c).
    base::get("require_validated_method", envir = private)(
      skill_dir    = "skills/psn-simeval",
      calling_file = "skills/psn-simeval/reference/psn_simeval.R",
      unit         = unit)
  }
}, envir = base::new.env())

.psn_simeval_gate <- function(unit) {
  if (!psn_simeval_validated(unit)) {
    base::stop("psn-simeval unit '", unit, "' is not SME-validated (either the ",
         "manifest unit is draft/rejected, or skills/psn-simeval/MANIFEST.yaml ",
         "has an empty root reviewed_by); this lane does ",
         "not run until its unit is approved. ESCALATE to the SME.",
         call. = FALSE)
  }
  base::invisible(TRUE)
}

# ---------------------------------------------------------------------------
# Internal: translate the operational psn block of engine_config into the
# PsN single-line flags that apply to every tool call. METHOD flags are
# NEVER sourced here — they arrive as explicit function arguments from the
# MAP. Returns a character vector of "-flag" / "-flag=value" tokens.
.psn_operational_flags <- function(engine_config) {
  psn <- engine_config$psn %||% list()
  flags <- character(0)
  if (!is.null(psn$threads))     flags <- c(flags, paste0("-threads=", psn$threads))
  if (!is.null(psn$retries))     flags <- c(flags, paste0("-retries=", psn$retries))
  # NONMEM version label (psn.conf [nm_versions]); declared per run.
  if (!is.null(psn$nm_version))  flags <- c(flags, paste0("-nm_version=", psn$nm_version))
  if (isTRUE(psn$tweak_inits))   flags <- c(flags, "-tweak_inits")
  if (isTRUE(psn$picky))         flags <- c(flags, "-picky")
  if (!is.null(psn$clean))       flags <- c(flags, paste0("-clean=", psn$clean))
  if (!is.null(psn$model_subdir)) {
    flags <- c(flags, if (isTRUE(psn$model_subdir)) "-model_subdir"
                      else "-no-model_subdir")
  }

  # Grid submission (amendment 2026-08-31; flag names verified against the
  # installed PsN 5.7.1 help). psn.conf is site-owned and never written;
  # explicit flags keep the invocation auditable.
  submit <- psn$submit %||% "local"
  if (!identical(submit, "local")) {
    grid <- match.arg(submit, c("sge", "slurm", "torque"))
    flags <- c(flags, paste0("-run_on_", grid))
    if (!is.null(psn$prepend_flags)) {
      flags <- c(flags, paste0("-", grid, "_prepend_flags=", psn$prepend_flags))
    }
    if (!is.null(psn$queue)) {
      queue_flag <- switch(grid, sge = "-sge_queue=",
                           slurm = "-slurm_partition=",
                           torque = "-torque_queue=")
      flags <- c(flags, paste0(queue_flag, psn$queue))
    }
    if (identical(grid, "slurm") && !is.null(psn$account)) {
      flags <- c(flags, paste0("-slurm_account=", psn$account))
    }
  }

  # Within-run NONMEM MPI (vpc/npc/sse tolerate it; sir's prohibition is
  # the uncertainty lane's concern). nodes > 1 requires a parafile.
  nodes <- psn$nodes %||% 1L
  if (nodes > 1 || !is.null(psn$parafile)) {
    if (nodes > 1 && is.null(psn$parafile)) {
      stop("psn.nodes > 1 requires psn.parafile in config/engine.yaml ",
           "(the NONMEM MPI parafile, e.g. mpilinux8.pnm).", call. = FALSE)
    }
    flags <- c(flags, paste0("-nodes=", nodes))
    if (!is.null(psn$parafile)) {
      flags <- c(flags, paste0("-parafile=", psn$parafile))
    }
  }
  flags
}

# ---------------------------------------------------------------------------
# Internal: run a PsN tool through the container executor. Builds the full
# argv, execs system2, and returns the exit status plus captured output.
# workdir must be inside a git repository (mount rule — see the estimation
# lane skill); the container mounts the git toplevel so relative $DATA
# paths resolve. This function does NOT re-validate the git precondition
# per call — reference_nm_workspace() established it for the model tree;
# add an explicit check here if this family is ever used on a tree the
# estimation lane did not provision.
#
# executor  path to tools/nonmem_exec.sh, the provision dispatcher
#           (parameter, never hardcoded)
# workdir   directory PsN runs in (holds the control stream and its data)
# tool      PsN tool name: "vpc" | "npc" | "sse"
# args      character vector of already-assembled PsN arguments
#
# Returns list(status, stdout, cmd).
# Tool-name -> intended-use unit map. The runner gates on the mapped unit
# BEFORE any system2, so this lane is safe even when .psn_run is called
# directly (not only through the public reference_psn_* entry points, which
# also gate). An unrecognised tool name is a HARD STOP — never an ungated run.
.PSN_SIMEVAL_TOOL_UNIT <- c(vpc = "vpc", npc = "npc", sse = "sse")

.psn_run <- function(executor, workdir, tool, args, invocation = NULL) {
  unit <- unname(.PSN_SIMEVAL_TOOL_UNIT[tool])
  if (length(tool) != 1 || is.na(unit)) {
    stop(".psn_run: unknown PsN tool '", tool, "' — the runner gates on a ",
         "mapped intended-use unit before executing, and refuses an unmapped ",
         "tool rather than run ungated. Known tools: ",
         paste(names(.PSN_SIMEVAL_TOOL_UNIT), collapse = ", "), ".",
         call. = FALSE)
  }
  .psn_simeval_gate(unit)
  stopifnot(file.exists(executor), dir.exists(workdir))
  argv <- c(workdir, tool, args)
  # system2 warns on a nonzero exit; the stop below carries the status and
  # the output, so the warning is redundant noise and is muffled.
  run_env <- if (is.null(invocation)) character(0) else
    paste0("PMX_INVOCATION_ID=", invocation$name)
  out <- suppressWarnings(system2(executor, args = shQuote(argv), env = run_env,
                                  stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status") %||% 0L
  cmd <- paste(c(shQuote(basename(executor)), shQuote(argv)), collapse = " ")
  # FAIL CLOSED (integrity batch 7, Codex round 8 finding 5): a nonzero exit
  # used to be returned to the caller, which then went looking for the
  # newest results file anywhere under workdir — a failed run could report a
  # stale prior VPC. A nonzero exit is a stop, quoting the last output lines.
  if (!identical(as.integer(status), 0L)) {
    tail_out <- utils::tail(as.character(out), 20)
    stop("PsN ", tool, " exited with status ", status, " (", cmd, ").\n",
         "Last output lines:\n", paste("  ", tail_out, collapse = "\n"),
         "\nNo results are read from a failed run. Inspect the PsN log; ",
         "ESCALATE if the cause is not a declaration error.", call. = FALSE)
  }
  list(status = status, stdout = out, cmd = cmd, argv = argv)
}

# The binning rules this reference will emit. `auto_bin` is PsN's own
# automatic binning, and it is a DECLARED method here, not an unstated
# fallback: the generator emits an explicit -auto_bin=<value>, the value is
# declared in the MAP / run-config, and both the value and the PsN version are
# recorded in the run record.
#
# Flag syntax verified against the engine on 2026-09-04 (PsN 5.7.1,
# `tools/nonmem_exec.sh <scratch> vpc -help`):
#
#     -auto_bin=auto
#     -auto_bin=min,max
#     -auto_bin=N
#     -auto_bin=unique
#
# The same help text is internally inconsistent about the engine default --
# "Default is -auto_bin=2,10" in the body against "-auto_bin=auto   (default)"
# in the summary. That inconsistency is precisely why the value is declared
# rather than left to the engine.
PSN_VPC_BIN_RULES <- c("auto_bin", "bin_by_count", "no_of_bins", "bin_array")

# Exactly one binning rule, and one is REQUIRED (SME decision, 2026-09-04;
# round-10 finding P1-4). A bin_rule that sets more than one of the known
# fields, sets none, or names a field outside the set is a stop; NULL is a stop
# naming the argument. Binning decides which observations are compared with
# which simulated percentiles, so it is a scientific declaration with no
# default -- and "whatever PsN does" is not a declaration.
.psn_check_bin_rule <- function(bin_rule) {
  if (is.null(bin_rule))
    stop("bin_rule is a required, MAP-declared parameter of the PsN VPC and ",
         "has no default: the binning method decides which observations are ",
         "compared with which simulated percentiles. Declare exactly one of ",
         paste(PSN_VPC_BIN_RULES, collapse = ", "),
         "; the house default method is list(auto_bin = <value>) -- see ",
         "skills/psn-simeval/SKILL_PSN_SIMEVAL.md. ESCALATE to the SME if the ",
         "analysis plan does not state one.", call. = FALSE)
  known <- PSN_VPC_BIN_RULES
  unknown <- setdiff(names(bin_rule), known)
  if (length(unknown))
    stop("bin_rule has unknown field(s): ", paste(unknown, collapse = ", "),
         "; declare exactly one of ", paste(known, collapse = ", "), ".",
         call. = FALSE)
  set <- c(!is.null(bin_rule$auto_bin), isTRUE(bin_rule$bin_by_count),
           !is.null(bin_rule$no_of_bins), !is.null(bin_rule$bin_array))
  # Round 9, P1-3: zero used to pass as well as one. A bin_rule list that
  # selects nothing emits no explicit rule and leaves the binning to the
  # engine's own default -- an undeclared method deciding a reported figure.
  if (sum(set) != 1L)
    stop("bin_rule declares ", sum(set), " binning methods (",
         if (sum(set)) paste(known[set], collapse = ", ") else "none", "); ",
         "exactly one is allowed. The MAP declares which.", call. = FALSE)
  if (!is.null(bin_rule$auto_bin)) {
    v <- as.character(bin_rule$auto_bin)
    if (length(v) != 1L || !nzchar(v))
      stop("bin_rule$auto_bin must be a single non-empty value; PsN accepts ",
           "auto, N, min,max or unique (verified against PsN 5.7.1).",
           call. = FALSE)
    if (!grepl("^(auto|unique|[0-9]+(:[0-9]+)*|[0-9]+,[0-9]+(:[0-9]+,[0-9]+)*)$", v))
      stop("bin_rule$auto_bin = '", v, "' is not one of PsN's documented ",
           "forms: 'auto', 'unique', a bin count N (or N:N:N per stratum), or ",
           "min,max (or min,max:min,max per stratum). Verified against PsN ",
           "5.7.1 `vpc -help`.", call. = FALSE)
  }
  invisible(NULL)
}

# The PsN version the run actually used, read from the engine rather than from
# a configuration file, and recorded in the run record beside the binning rule
#: a declared engine default is only meaningful
# bound to the version that defines it.
# Seconds the probe may take before it is refused. See
# MM_VERSION_PROBE_TIMEOUT in bbr_estimation.R: the two probes are held to the
# same contract, and this file keeps its own constant because a reference file
# does not read another skill's reference code.
PSN_VERSION_PROBE_TIMEOUT <- 30

# Fix round 2 of batch 11 (independent review, finding F2-b). This repeated,
# one file away, the exact defect finding F2 was raised about and that
# .reference_nm_bbi_version() had just been fixed for: stdout and stderr were
# MERGED, so a stub writing "PsN version: ..." to stderr was accepted as the
# recorded engine version; the exit status was ignored, so a failing probe that
# happened to print the line was accepted; and there was no timeout, so a
# hanging executor blocked the caller indefinitely. stderr now goes to its own
# file and is never read for the value, the status is captured, the probe is
# bounded, and the version itself must be a version string.
PSN_VERSION_PATTERN <- "^[0-9]+(\\.[0-9]+)+$"

.psn_version <- function(executor, workdir) {
  errfile <- tempfile("psn-version-stderr")
  on.exit(unlink(errfile), add = TRUE)
  out <- tryCatch(suppressWarnings(
    system2(executor, args = shQuote(c(workdir, "psn", "-version")),
            stdout = TRUE, stderr = errfile,
            timeout = PSN_VERSION_PROBE_TIMEOUT)),
    error = function(e) structure(character(0), status = -1L))
  st <- attr(out, "status")
  if (!is.null(st) && identical(as.integer(st)[1], 124L))
    stop("`psn -version` did not return within the ", PSN_VERSION_PROBE_TIMEOUT,
         " second probe timeout (executor ", executor, "). An engine that cannot ",
         "say what it is within that budget is refused rather than waited on.",
         call. = FALSE)
  if (!is.null(st) && !identical(as.integer(st), 0L))
    stop("`psn -version` exited with exit status ", as.integer(st)[1],
         " (executor ", executor, "). The run record must carry the PsN version ",
         "alongside the bin rule, so a failed probe ",
         "is refused rather than recorded.", call. = FALSE)
  hit <- grep("PsN version", as.character(out), value = TRUE)
  # Fix round 1 of batch 11 (independent review). This used to return
  # NA_character_, and the run record then carried `psn_version: NA` silently.
  # SME decision 2 requires the PsN version to be RECORDED together with the bin
  # rule, so an unresolvable version is a stop, not a degraded field: a run
  # record that cannot say which engine produced it is not evidence.
  if (!length(hit))
    stop("`psn -version` did not report a PsN version line (executor ",
         executor, "). The run record must carry the PsN version alongside the ",
         "bin rule, so an unresolvable version is ",
         "refused here rather than recorded as NA.", call. = FALSE)
  v <- trimws(sub(".*PsN version:?", "", hit[1]))
  if (!nzchar(v))
    stop("`psn -version` reported an empty PsN version (executor ", executor,
         "). The run record must carry the PsN version alongside the bin rule ",
         "(SME decision 2, 2026-09-04).", call. = FALSE)
  if (!grepl(PSN_VERSION_PATTERN, v))
    stop("`psn -version` reported ", encodeString(substr(v, 1L, 60L), quote = "\""),
         " where a version string was expected (executor ", executor, "). A run ",
         "record field that is not a version is not evidence of which engine ran.",
         call. = FALSE)
  v
}


# ---------------------------------------------------------------------------
# reference_psn_vpc — Visual Predictive Check (engine-native alternative to
# the tidyvpc house default; see SKILL.md and its OPEN QUESTION on
# qualification-gate coverage parity).
#
# workdir        run directory, inside a git repo (mount rule)
# ctl_file       control stream filename (relative to workdir); must be
#                simulatable and have a successful estimation run (or supply
#                lst_file)
# samples        number of simulated datasets (MAP-declared; PsN min 20)
# seed           REQUIRED — reproducibility, recorded in the run record
# bin_rule       REQUIRED. A list declaring exactly ONE binning method:
#                list(auto_bin = "auto" | "unique" | N | "min,max") -- the
#                house default METHOD, PsN's own automatic binning with the
#                value declared explicitly -- or list(bin_by_count = TRUE),
#                list(no_of_bins = 9), or
#                list(bin_array = c(0, 1, 2, 4, 8, 12, 24)). MAP-declared;
#                NULL is refused by argument name.
# stratify_on    stratification column name, or NULL. MAP-declared.
# predcorr       prediction correction (pcVPC) — a METHOD decision, default
#                FALSE; TRUE emits -predcorr. MAP-declared.
# varcorr        variability correction; requires predcorr. Default FALSE.
# lloq / uloq    censoring limits, or NULL. Retain BQL rows in the dataset
#                (gotcha). MAP-declared.
# confidence_interval  PI coverage level; NULL leaves PsN's default (95).
# lst_file       explicit .lst when the fit is not the default; else NULL.
# engine_config  parsed config/engine.yaml (list) — operational flags only.
# executor       path to tools/nonmem_exec.sh, the provision dispatcher.
#
# Returns list(cmd, status, stdout, results_csv, vpctab, summary).
reference_psn_vpc <- function(workdir, ctl_file, samples, seed,
                              bin_rule, stratify_on = NULL,
                              predcorr = FALSE, varcorr = FALSE,
                              lloq = NULL, uloq = NULL,
                              confidence_interval = NULL,
                              lst_file = NULL,
                              engine_config,
                              executor = "tools/nonmem_exec.sh") {
  .psn_simeval_gate("vpc")
  if (missing(seed) || is.null(seed))
    stop("reference_psn_vpc: seed is required — every PsN vpc run must be ",
         "reproducible from its declared seed.", call. = FALSE)
  if (missing(samples) || is.null(samples))
    stop("reference_psn_vpc: samples is required (MAP-declared).", call. = FALSE)
  if (as.integer(samples) < 20)
    stop("reference_psn_vpc: PsN requires samples >= 20; got ", samples,
         ". The house reported-VPC floor is higher and MAP-declared.",
         call. = FALSE)
  if (isTRUE(varcorr) && !isTRUE(predcorr))
    stop("reference_psn_vpc: varcorr requires predcorr (PsN constraint).",
         call. = FALSE)

  args <- c(ctl_file,
            paste0("-samples=", as.integer(samples)),
            paste0("-seed=", seed))

  # Exactly one binning rule, and one is REQUIRED. The MAP declares which.
  .psn_check_bin_rule(bin_rule)
  if (!is.null(bin_rule$auto_bin))
    args <- c(args, paste0("-auto_bin=", as.character(bin_rule$auto_bin)))
  if (isTRUE(bin_rule$bin_by_count))
    args <- c(args, "-bin_by_count=1")
  if (!is.null(bin_rule$no_of_bins))
    args <- c(args, paste0("-no_of_bins=", bin_rule$no_of_bins))
  if (!is.null(bin_rule$bin_array))
    args <- c(args, paste0("-bin_array=",
                           paste(bin_rule$bin_array, collapse = ",")))

  if (!is.null(stratify_on))
    args <- c(args, paste0("-stratify_on=", stratify_on))
  if (isTRUE(predcorr)) args <- c(args, "-predcorr")
  if (isTRUE(varcorr))  args <- c(args, "-varcorr")
  if (!is.null(lloq))   args <- c(args, paste0("-lloq=", lloq))
  if (!is.null(uloq))   args <- c(args, paste0("-uloq=", uloq))
  if (!is.null(confidence_interval))
    args <- c(args, paste0("-confidence_interval=", confidence_interval))
  if (!is.null(lst_file))
    args <- c(args, paste0("-lst_file=", lst_file))

  args <- c(args, .psn_operational_flags(engine_config))

  run_dir <- .psn_new_run_dir(workdir, "vpc")
  args <- c(args, paste0("-directory=", run_dir$name))
  run <- .psn_run(executor, workdir, "vpc", args, invocation = run_dir)

  results_csv <- .psn_find_artifact(run_dir, "vpc_results.csv")
  vpctab <- list.files(run_dir$path, pattern = "^vpctab", recursive = TRUE,
                       full.names = TRUE)
  vpctab <- vpctab[file.info(vpctab)$mtime >= run_dir$since]
  invocation_manifest <- .psn_write_invocation_manifest(
    run_dir, run, list(results_csv = results_csv, vpctab = vpctab))

  # The run record carries the declared binning rule and the PsN version that
  # interpreted it. A declared engine default is
  # only meaningful bound to the version that defines it.
  list(cmd = run$cmd, status = run$status, stdout = run$stdout,
       run_dir = run_dir$path, start_marker = run_dir$marker,
       invocation_manifest = invocation_manifest,
       bin_rule = bin_rule,
       bin_rule_name = names(bin_rule)[
         names(bin_rule) %in% PSN_VPC_BIN_RULES][1],
       psn_version = .psn_version(executor, workdir),
       results_csv = results_csv,
       vpctab = if (length(vpctab)) vpctab else NA_character_,
       summary = .parse_vpc_results(results_csv))
}

# Parse vpc_results.csv into a coverage-oriented summary. The house
# qualification criteria are written against per-bin observed-vs-simulated
# coverage; PsN writes its own column layout, which the SME OPEN QUESTION
# in SKILL.md has not yet mapped to the house statistics.
# UNVERIFIED — confirm the actual column names at first real run and map
# them to the house coverage figures (or restrict PsN vpc to exploratory
# use per the resolved OPEN QUESTION). Returns the raw frame plus a note.
.parse_vpc_results <- function(results_csv) {
  if (is.na(results_csv) || !file.exists(results_csv))
    return(list(available = FALSE,
                note = "vpc_results.csv not found — did the run complete?"))
# vpc_results.csv is a multi-section file with distinct headers.
  # ("VPC run info", "VPC results", "Continuous data", "Diagnostics
  # VPC", "NPC results", per-bin blocks), each with its own header row.
  # Same layout family as PsN bootstrap_results.csv. A flat read.csv
  # mangles it; the raw lines and section index are returned instead,
  # and coverage extraction (if PsN vpc ever feeds a gate — see the
  # SKILL's OPEN QUESTION) must parse the "VPC results" section
  # explicitly. The vpctab file is the row-level artifact.
  raw <- tryCatch(readLines(results_csv, warn = FALSE),
                  error = function(e) NULL)
  if (!is.null(raw)) {
    return(list(available = TRUE,
                lines = raw,
                sections = grep("^[A-Za-z]", raw, value = TRUE),
                coverage = NULL,
                note = paste("multi-section PsN results file (verified",
                             "2026-08-31); coverage deliberately not",
                             "synthesized — see SKILL OPEN QUESTION")))
  }
  list(
    available = !is.null(raw),
    raw = raw,
    # Coverage mapping deliberately NOT computed: the parity of PsN's
    # coverage statistics with the house Q1 coverage rule is the unresolved
    # OPEN QUESTION (SKILL.md). Do not synthesize a coverage figure here.
    coverage = NULL,
    note = paste("unverified — confirm PsN vpc_results.csv column layout at",
                 "first real run; coverage parity with the house",
                 "qualification criteria is an unresolved OPEN QUESTION",
                 "(SKILL.md). Do not feed to a qualification gate until the",
                 "SME rules."))
}

# ---------------------------------------------------------------------------
# reference_psn_npc — Numerical Predictive Check. Same simulation as vpc,
# numerical output only (fraction of observations outside the PI per bin).
#
# Arguments mirror reference_psn_vpc where they overlap; npc has no
# predcorr/varcorr. Returns list(cmd, status, stdout, results_csv, summary).
reference_psn_npc <- function(workdir, ctl_file, samples, seed,
                              bin_rule = NULL, stratify_on = NULL,
                              lloq = NULL, uloq = NULL,
                              confidence_interval = NULL,
                              lst_file = NULL,
                              engine_config,
                              executor = "tools/nonmem_exec.sh") {
  .psn_simeval_gate("npc")
  if (missing(seed) || is.null(seed))
    stop("reference_psn_npc: seed is required.", call. = FALSE)
  if (missing(samples) || is.null(samples))
    stop("reference_psn_npc: samples is required (MAP-declared).", call. = FALSE)
  if (as.integer(samples) < 20)
    stop("reference_psn_npc: PsN requires samples >= 20; got ", samples, ".",
         call. = FALSE)

  args <- c(ctl_file,
            paste0("-samples=", as.integer(samples)),
            paste0("-seed=", seed))

  # Verified against the engine on 2026-09-04 (PsN 5.7.1,
  # `tools/nonmem_exec.sh <scratch> npc -h`): npc's option list contains NO
  # binning option at all -- no -auto_bin, -bin_by_count, -no_of_bins or
  # -bin_array. This generator used to construct those flags for npc, which
  # the engine would not have accepted. A bin_rule is therefore refused here
  # rather than translated: npc's binning is not a parameter of this lane.
  if (!is.null(bin_rule))
    stop("reference_psn_npc: PsN 5.7.1's npc accepts no binning option ",
         "(-auto_bin / -bin_by_count / -no_of_bins / -bin_array are absent ",
         "from `npc -h`), so a bin_rule cannot be honoured and is refused ",
         "rather than silently dropped. Declare the binning on the VPC lane, ",
         "or ESCALATE to the SME.", call. = FALSE)

  # Same engine check, same date: npc's option list carries --censor but
  # neither --lloq nor --uloq. Refused rather than emitted.
  if (!is.null(lloq) || !is.null(uloq))
    stop("reference_psn_npc: PsN 5.7.1's npc accepts neither -lloq nor -uloq ",
         "(they are absent from `npc -h`; npc's censoring option is -censor). ",
         "A censoring limit is refused here rather than emitted as a flag the ",
         "engine does not accept. ESCALATE to the SME.", call. = FALSE)

  if (!is.null(stratify_on))
    args <- c(args, paste0("-stratify_on=", stratify_on))
  if (!is.null(confidence_interval))
    args <- c(args, paste0("-confidence_interval=", confidence_interval))
  if (!is.null(lst_file))
    args <- c(args, paste0("-lst_file=", lst_file))

  args <- c(args, .psn_operational_flags(engine_config))

  run_dir <- .psn_new_run_dir(workdir, "npc")
  args <- c(args, paste0("-directory=", run_dir$name))
  run <- .psn_run(executor, workdir, "npc", args, invocation = run_dir)
  results_csv <- .psn_find_artifact(run_dir, "npc_results.csv")
  invocation_manifest <- .psn_write_invocation_manifest(
    run_dir, run, list(results_csv = results_csv))

  list(cmd = run$cmd, status = run$status, stdout = run$stdout,
       run_dir = run_dir$path, start_marker = run_dir$marker,
       invocation_manifest = invocation_manifest,
       results_csv = results_csv,
       summary = .parse_npc_results(results_csv))
}

# Parse npc_results.csv — the numerical predictive check reports, per PI
# level, the observations below and above the interval. This is a
# multi-section file like vpc_results.csv. Unquoted title lines ("NPC run
# info", "NPC results" written with trailing blanks, "NPC * (warning)
# statistics"), each followed by a quoted header row with an empty first
# field; an unquoted "<n> observations out of <m>" line under "NPC results";
# quoted rows ending in a trailing comma; blank separator lines. A flat
# read.csv mangles it, so the lines are returned with a section index (0
# before the first title) and the trimmed titles; a missing title is
# reported in `missing_sections`, not coerced. Coverage stays NULL: parity
# of PsN's NPC statistics with the house qualification criteria is the
# unresolved OPEN QUESTION (SKILL.md); do not feed a gate until the SME rules.
.parse_npc_results <- function(results_csv) {
  if (is.na(results_csv) || !file.exists(results_csv))
    return(list(available = FALSE,
                note = "npc_results.csv not found — did the run complete?"))
  lines <- readLines(results_csv, warn = FALSE)
  title <- grepl("^[A-Za-z][^,]*$", lines)
  list(available = TRUE, lines = lines, section = cumsum(title),
       sections = trimws(lines[title]),
       missing_sections = setdiff(c("NPC run info", "NPC results"),
                                  trimws(lines[title])),
       coverage = NULL,
       note = paste("multi-section PsN npc results file (verified 2026-09-28);",
                    "coverage deliberately not synthesized; qualification-gate",
                    "parity is the unresolved OPEN QUESTION (SKILL.md)."))
}

# ---------------------------------------------------------------------------
# reference_psn_sse — Stochastic Simulation and Estimation for study-design
# evaluation and power (NOT scenario simulation — that is the mrgsolve house
# path; see SKILL.md boundary). Simulates samples datasets from the true
# model and re-estimates alternative models on each.
#
# workdir             run directory, inside a git repo (mount rule)
# ctl_file            the "true" (simulation) control stream
# samples             number of simulated datasets (required, MAP-declared)
# seed                REQUIRED — reproducibility
# alternative_models  character vector of alternative control-stream
#                     filenames estimated on each simulated dataset (the
#                     power comparison); NULL for simulate-only. MAP-declared.
# estimate_simulation whether the simulation model is itself estimated on
#                     each dataset. TRUE emits -estimate_simulation; FALSE
#                     emits -no-estimate_simulation, because PsN estimates
#                     the simulation model by default ("Default set",
#                     PsN 5.7.1 `sse -h`), so emitting nothing on FALSE
#                     would still re-estimate (units programme U2, root's
#                     disposition pending the SME at signing). Any other
#                     value is refused. Default FALSE. MAP-declared.
# rawres_input        external parameter-vector file for simulation with
#                     uncertainty (e.g. bootstrap raw_results), or NULL.
# engine_config       parsed config/engine.yaml (operational flags only).
# executor            path to tools/nonmem_exec.sh (provision dispatcher).
#
# Returns list(cmd, status, stdout, results_csv, raw_results, summary).
reference_psn_sse <- function(workdir, ctl_file, samples, seed,
                              alternative_models = NULL,
                              estimate_simulation = FALSE,
                              rawres_input = NULL,
                              engine_config,
                              executor = "tools/nonmem_exec.sh") {
  .psn_simeval_gate("sse")
  if (missing(seed) || is.null(seed))
    stop("reference_psn_sse: seed is required.", call. = FALSE)
  if (missing(samples) || is.null(samples))
    stop("reference_psn_sse: samples is required (MAP-declared).", call. = FALSE)
  if (!isTRUE(estimate_simulation) && !isFALSE(estimate_simulation))
    stop("reference_psn_sse: estimate_simulation must be TRUE or FALSE ",
         "(MAP-declared); PsN estimates the simulation model unless told not to.",
         call. = FALSE)

  args <- c(ctl_file,
            paste0("-samples=", as.integer(samples)),
            paste0("-seed=", seed))

  if (!is.null(alternative_models))
    args <- c(args, paste0("-alternative_models=",
                           paste(alternative_models, collapse = ",")))
  args <- c(args, if (estimate_simulation) "-estimate_simulation"
                  else "-no-estimate_simulation")
  if (!is.null(rawres_input))
    args <- c(args, paste0("-rawres_input=", rawres_input))

  args <- c(args, .psn_operational_flags(engine_config))

  run_dir <- .psn_new_run_dir(workdir, "sse")
  args <- c(args, paste0("-directory=", run_dir$name))
  run <- .psn_run(executor, workdir, "sse", args, invocation = run_dir)
  results_csv <- .psn_find_artifact(run_dir, "sse_results.csv")
  # The root-level raw_results_<stem>.csv holds the per-sample results.
  raw_results <- .psn_find_artifact(
    run_dir, paste0("raw_results_", sub("\\.[^.]*$", "", basename(ctl_file)),
                    ".csv"), required = FALSE)
  invocation_manifest <- .psn_write_invocation_manifest(
    run_dir, run, list(results_csv = results_csv, raw_results = raw_results))

  list(cmd = run$cmd, status = run$status, stdout = run$stdout,
       run_dir = run_dir$path, start_marker = run_dir$marker,
       invocation_manifest = invocation_manifest,
       results_csv = results_csv, raw_results = raw_results,
       summary = .parse_sse_results(results_csv))
}

# Parse sse_results.csv — summary statistics of the re-estimations against
# the simulation model's values (bias, rmse, and power or type-I error when
# alternative models are declared). This file has multiple sections. Unquoted title lines ("SSE run info", "Reference/True
# values", "Simulation model", then "Samples completed: <n>"), quoted header
# rows with an empty first field, quoted row-labelled statistic rows ending
# in a trailing comma, a quoted "standard error CI" sub-block, a blank
# separator line. Returned as lines with a section index and the trimmed
# titles, like the npc parser; a missing title is reported in
# `missing_sections`. The sections an alternative-model design adds were not
# seen and are not asserted; read no power or bias figure by name until a
# declared design has been run through this parser.
.parse_sse_results <- function(results_csv) {
  if (is.na(results_csv) || !file.exists(results_csv))
    return(list(available = FALSE,
                note = "sse_results.csv not found — did the run complete?"))
  lines <- readLines(results_csv, warn = FALSE)
  title <- grepl("^[A-Za-z][^,]*$", lines)
  list(available = TRUE, lines = lines, section = cumsum(title),
       sections = trimws(lines[title]),
       missing_sections = setdiff(c("SSE run info", "Reference/True values",
                                    "Simulation model"), trimws(lines[title])),
       note = paste("multi-section PsN sse results file (verified 2026-09-28,",
                    "no alternative model); the power/bias rows depend on the",
                    "declared design and alternative models."))
}
