# REFERENCE IMPLEMENTATION — NONMEM estimation lane (bbr / bbi / shim /
# container).
#
# AUTHORITY: skills/nonmem-estimation/MANIFEST.yaml is the authority for the
# SME signature and for per-unit status and digests. This header carries no
# status claim of its own — read the manifest.
# The adapter validates the engine output and checks that the control
# stream and bbr data join can be used together.
#
# The one rule: this file COMPUTES; agents call it with declared
# parameters. It never invents paths — every path is a parameter or is
# read from config/engine.yaml. Method decisions (nm_version, parallel,
# threads-as-science, parafile) come from the analysis declaration / MAP,
# never from here or from config/engine.yaml.
#
# Requires: bbr (>= 1.14.2, MPN), readr pinned (read_table2), yaml. bbi
# 3.4.0, whose path is machine-local and comes from harness.yaml's
# bbi_path key (see .reference_nm_require_bbi).

# ---------------------------------------------------------------------------
# reference_nm_workspace(model_dir, engine_config)
#
# Writes bbi.yaml into model_dir from OPERATIONAL settings only, and
# validates the containerized provision's preconditions: the shim exists at
# <shim_home>/run/<shim_executable>, and model_dir is inside a Git repository
# or NONMEM_EXTRA_MOUNTS declares at least one nonempty colon-delimited token
# on a single line. That declaration acknowledges mounts, not input coverage;
# tokens remain opaque and their path usability is not checked here.
#
# bbr::bbi_init() is deliberately NOT used: `home` points at the shim, not
# a real NONMEM install, so the file is written directly.
#
# model_dir      directory that will hold the numbered control streams and
#                their output dirs; see containerized preconditions above.
# engine_config  parsed config/engine.yaml (list). Read by the caller via
#                yaml::read_yaml("config/engine.yaml") — passed in, not
#                read here, so the function stays path-free and testable.
# repo_root      base for resolving the relative shim_home in
#                config/engine.yaml. Pipelines run from the repo root
#                (root.dir is pinned there), so the default getwd() is
#                right in-pipeline; ad-hoc callers elsewhere pass the
#                repo root explicitly.
# nonmem_home    NATIVE provision only: the site's real NONMEM install
#                directory (bbi appends /run/<executable> — the standard
#                install layout). Machine-local, so it comes from
#                harness.yaml (git-ignored), never from config/engine.yaml.
#                Ignored under the containerized provision.
#
# Returns the path to the written bbi.yaml (invisibly).
reference_nm_workspace <- function(model_dir, engine_config,
                                   repo_root = getwd(),
                                   nonmem_home = NULL,
                                   bbi_path = NULL) {
  # GATED (integrity batch 7, Codex round 8 finding 7). Provisioning selects
  # the executable, threads, MPI behaviour and every operational default the
  # fit runs under; it is a manifested entry point of the approved unit and
  # gates like the others.
  .nonmem_estimation_gate()
  stopifnot(dir.exists(model_dir))

  # R10-P2-2: the workspace writes bbi.yaml for a bbi that must actually be
  # usable, so it resolves the executable through the ONE shared resolver
  # rather than assuming a later step will. Only harness.yaml's bbi_path key
  # is read; the file is never printed.
  if (is.null(bbi_path)) bbi_path <- .reference_nm_bbi_path_from_harness()
  .reference_nm_require_bbi(bbi_path)

  nm  <- engine_config$engines$nonmem
  bbi <- engine_config$bbi
  if (is.null(nm) || is.null(bbi)) {
    stop("config/engine.yaml is missing engines$nonmem or bbi; the ",
         "workspace cannot be provisioned without the operational block.",
         call. = FALSE)
  }
  provision <- nm$provision %||% "containerized"

  if (identical(provision, "containerized")) {
    # Shim precondition. bbi APPENDS /run/ to the nonmem `home`, so the
    # shim must resolve at <shim_home>/run/<shim_executable>. shim_home in
    # config/engine.yaml is repo-relative; absolute paths pass through.
    shim_home <- nm$shim_home
    if (!grepl("^/", shim_home)) shim_home <- file.path(repo_root, shim_home)
    shim_home <- normalizePath(shim_home, mustWork = TRUE)
    shim_path <- file.path(shim_home, "run", nm$shim_executable)
    if (!file.exists(shim_path)) {
      stop("NONMEM shim not found at ", shim_path,
           " — bbi appends /run/ to the nonmem home, so the shim must live ",
           "at <shim_home>/run/<executable>.", call. = FALSE)
    }

    # Containerized workspace precondition: Git membership or an explicit
    # effective mount declaration. Only failed Git checks consult the latter.
    in_git <- tryCatch(
      identical(system2("git",
                        c("-C", shQuote(model_dir), "rev-parse",
                          "--is-inside-work-tree"),
                        stdout = TRUE, stderr = FALSE), "true"),
      error = function(e) FALSE)
    if (!isTRUE(in_git)) {
      # A declaration acknowledges external mounts; it does not prove input
      # coverage or path usability. The wrapper consumes a single line.
      mounts <- Sys.getenv("NONMEM_EXTRA_MOUNTS", unset = "")
      tokens <- strsplit(mounts, ":", fixed = TRUE)[[1L]]
      tokens <- tokens[nzchar(tokens)]
      if (grepl("\n", mounts, fixed = TRUE) || length(tokens) == 0L) {
        stop("model_dir ", model_dir, " is not inside a git repository; move ",
             "the model tree into Git or set a single-line NONMEM_EXTRA_MOUNTS ",
             "declaration with a nonempty colon-delimited token. This ",
             "acknowledges mounts without validating input coverage.", call. = FALSE)
      }
    }
    home <- shim_home
    executable <- nm$shim_executable
  } else if (identical(provision, "native")) {
    # Native (regulated-environment) provision: bbi.yaml points straight
    # at the site's validated NONMEM install; no shim, no container, no
    # git-repo requirement. bbi appends /run/<executable>, which is the
    # standard NONMEM install layout, so `home` is the install root.
    if (is.null(nonmem_home)) {
      stop("provision is 'native' but nonmem_home was not supplied — pass ",
           "the site NONMEM install directory (machine-local; read it ",
           "from harness.yaml, never from config/engine.yaml).",
           call. = FALSE)
    }
    home <- normalizePath(nonmem_home, mustWork = TRUE)
    executable <- nm$native_executable %||% "nmfe75"
    nmfe <- file.path(home, "run", executable)
    if (!file.exists(nmfe)) {
      stop("native NONMEM executable not found at ", nmfe,
           " (bbi appends /run/ to the home directory; check nonmem_home ",
           "and the native_executable setting).", call. = FALSE)
    }
  } else {
    stop("Unknown NONMEM provision mode: ", provision,
         " (containerized|native).", call. = FALSE)
  }

  # OPERATIONAL bbi.yaml. nm_version selects the nonmem map entry; home is
  # the shim dir (containerized) or the site install (native); operational
  # args come from bbi$args.
  args <- bbi$args %||% list()
  cfg <- list(
    clean_lvl  = args$clean_lvl  %||% 1L,
    copy_lvl   = args$copy_lvl   %||% 0L,
    git        = FALSE,
    nm_version = nm$nm_version_label,
    overwrite  = args$overwrite  %||% FALSE,
    threads    = args$threads    %||% 1L,
    # Within-run NONMEM MPI (amendment 2026-08-31): parallel + parafile
    # mirror the PsN lane's nodes/parafile. parafile paths resolve
    # relative to the model directory; grid submission is NOT set here —
    # it is the .mode argument of submit_model() (engine_config$bbi$mode,
    # local|sge; bbi has no native slurm mode).
    parallel   = isTRUE(args$parallel),
    nonmem = stats::setNames(list(list(
      default    = TRUE,
      executable = executable,
      home       = home,
      nmqual     = FALSE)), nm$nm_version_label))
  if (!is.null(args$parafile)) cfg$parafile <- args$parafile  # unverified as a yaml key — confirm at first MPI run (CLI --parafile is verified bbi surface)

  out <- file.path(model_dir, "bbi.yaml")
  yaml::write_yaml(cfg, out)

  # Per-provisioning engine-config snapshot (audit trail, amendment
  # 2026-08-31): the bbr lane does not pass through tools/nonmem_exec.sh
  # (bbi execs the shim/nmfe directly), so the workspace writer freezes
  # the operational config it provisioned from — same convention as the
  # dispatcher's engine-config-used/ snapshots for PsN calls. bbi.yaml is
  # the derived operational file; this is the source it derived from.
  snap_dir <- file.path(model_dir, "engine-config-used")
  dir.create(snap_dir, showWarnings = FALSE)
  ts <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
  snap <- file.path(snap_dir, paste0(ts, "-bbr-workspace.yaml"))
  header <- c(
    "# Engine-config snapshot (written by reference_nm_workspace)",
    "# engine: nonmem (bbr/bbi estimation lane)",
    paste0("# invoked_at_utc: ", ts),
    paste0("# provision: ", provision),
    paste0("# model_dir: ", model_dir),
    "#")
  writeLines(c(header, yaml::as.yaml(engine_config)), snap)

  # R-package snapshot beside the engine snapshot (SME decision
  # 2026-08-31: reference lock is project-level, ACTUAL versions are
  # per-run evidence). Best-effort — absence of r_lock.R only skips it.
  if (file.exists(file.path(repo_root, "tools/r_lock.R"))) {
    local({
      source(file.path(repo_root, "tools/r_lock.R"), local = TRUE)
      abs_model_dir <- normalizePath(model_dir, mustWork = TRUE)
      # lock + harness.yaml resolve relative to the repo root
      owd <- setwd(repo_root); on.exit(setwd(owd))
      write_r_packages_used(abs_model_dir)
    })
  }

  invisible(out)
}

# `%||%` — NULL-coalesce (rlang provides one; defined locally so the
# reference has no hard rlang dependency).
`%||%` <- function(a, b) if (is.null(a)) b else a

# Self-gate. TRUE only when skills/nonmem-estimation/MANIFEST.yaml carries a
# confirmed root (non-empty reviewed_by) AND unit estimation is status:
# approved with this file's current sha256 (same manifest mechanism as the
# other draft-tier references: psn_uncertainty.R, psn_simeval.R,
# uncertainty.R). While the manifest root is unsigned the estimation entry
# points refuse to run; filling the root reviewed_by/reviewed_at in the
# manifest and approving the unit IS the act of signing.
# reference_nm_workspace is gated too (batch 7): provisioning
# writes bbi.yaml and selects the executable, threads and MPI settings the
# fit executes under, which is configuration bound to execution, not
# nothing.
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

nonmem_estimation_validated <- base::local({
  private <- .mm_trusted_private
  function() {
    # Integrity batch 2: the self-gate now delegates to the method-manifest core
    # (tools/method_manifest.R). Once the SME approves the manifest with a
    # non-empty approver, validation binds to the sha256 of this reference file;
    # the legacy "Reviewed by:" grep of SKILL.md was retired in batch 3c and is
    # no longer consulted.
    base::get("require_validated_method", envir = private)(
      skill_dir    = "skills/nonmem-estimation",
      calling_file = "skills/nonmem-estimation/reference/bbr_estimation.R",
      unit         = "estimation")
  }
}, envir = base::new.env())

.nonmem_estimation_gate <- function() {
  if (!nonmem_estimation_validated()) {
    base::stop("nonmem-estimation is not yet SME-validated (unit estimation is ",
         "draft/rejected, or skills/nonmem-estimation/MANIFEST.yaml has an empty ",
         "root reviewed_by); the NONMEM ",
         "estimation lane does not run until the manifest record is ",
         "signed. ESCALATE to the SME.", call. = FALSE)
  }
  base::invisible(TRUE)
}

# ONE CONFIG OBJECT PER RUN (integrity batch 7, Codex round 8 finding 7).
# The fit-time snapshot used to pick <model_dir>/run-config.yaml when present
# while bbi executed whatever bbi.yaml the workspace had been provisioned
# from earlier — the record could attest config A while NONMEM ran under
# config B. Now the operational config is resolved ONCE here, through
# tools/engine_config.R::load_engine_config (per-run run-config.yaml wins
# over config/engine.yaml; validate_compute_config() runs inside it), and
# reference_nm_fit() derives bbi.yaml, the submission mode and the snapshot
# from this same object. Returns list(cfg, path, sha256).
.reference_nm_resolve_config <- function(model_dir, repo_root = getwd()) {
  if (!exists("load_engine_config", mode = "function")) {
    ec <- file.path(repo_root, "tools", "engine_config.R")
    if (!file.exists(ec))
      stop("tools/engine_config.R not found under ", repo_root,
           "; the run configuration cannot be resolved.", call. = FALSE)
    source(ec, local = TRUE)
  }
  cfg <- load_engine_config(workdir = model_dir,
                            default = file.path(repo_root, "config", "engine.yaml"))
  path <- attr(cfg, "config_source")
  sha <- if (requireNamespace("digest", quietly = TRUE)) {
    digest::digest(file = path, algo = "sha256")
  } else {
    sub("\\s.*$", "", system2("sha256sum", shQuote(path), stdout = TRUE))
  }
  list(cfg = cfg, path = path, sha256 = sha)
}

# A file the run must have produced: exists and is newer than the run's
# start time. A stale file from a prior run satisfies neither existence-only
# checks nor the audit trail; it is a stop.
.reference_nm_require_fresh <- function(path, since, what) {
  if (length(path) != 1L || is.na(path) || !file.exists(path))
    stop(what, " was not written (", path, "); the run did not produce its ",
         "required output. Treat as a failed run.", call. = FALSE)
  mt <- file.info(path)$mtime
  if (is.na(mt) || mt < since)
    stop(what, " (", path, ") predates the run start (", format(since, tz = "UTC",
         usetz = TRUE), "); it belongs to a previous run and is not evidence ",
         "for this one.", call. = FALSE)
  invisible(path)
}

# The verbatim termination lines of a NONMEM listing, read from the listing
# BYTES rather than from a head/tail window.
#
# Read the complete listing because the termination block can occur
# outside the default bbr listing window.
#
# So the status is extracted by deterministic string matching over the whole
# listing: the "0MINIMIZATION SUCCESSFUL" / "0MINIMIZATION TERMINATED" block
# and the "#OBJV" line, verbatim, in file order. Nothing here interprets the
# result — a TERMINATED run yields its status just as a SUCCESSFUL one does,
# and the convergence verdict is formed elsewhere from run_heuristics. Absence
# of both is the refusal it already was: a run record with no termination
# status is a failed run, never a degraded field.
.reference_nm_termination_lines <- function(lst_path) {
  if (length(lst_path) != 1L || is.na(lst_path) || !nzchar(lst_path) ||
      !file.exists(lst_path))
    stop("the run listing (", lst_path %||% "<none>", ") could not be read; ",
         "the termination status is taken from the listing itself. Treat as ",
         "a failed run.", call. = FALSE)
  lines <- tryCatch(readLines(lst_path, warn = FALSE),
                    error = function(e) character(0))
  # The minimization block: the 0MINIMIZATION line and the indented detail
  # lines NONMEM writes under it (function evaluations, significant digits).
  hit <- grep("^\\s*0MINIMIZATION\\s+(SUCCESSFUL|TERMINATED)", lines)
  keep <- integer(0)
  for (i in hit) {
    keep <- c(keep, i)
    j <- i + 1L
    while (j <= length(lines) && grepl("^\\s+\\S", lines[j]) &&
             !grepl("^\\s*0", lines[j])) { keep <- c(keep, j); j <- j + 1L }
  }
  keep <- c(keep, grep("#OBJV", lines, fixed = TRUE))
  keep <- sort(unique(keep))
  if (!length(keep))
    stop("no termination status could be read from the listing ", lst_path,
         ": neither a 0MINIMIZATION SUCCESSFUL/TERMINATED line nor a #OBJV ",
         "line is present. Treat as a failed run.", call. = FALSE)
  lines[keep]
}

# Subject / observation / record counts from a bbr model_summary's run_details.
#
# Live run of 2026-09-04: number_of_subjects 59, number_of_data_records 744,
# number_of_obs 155. The reference used to assign number_of_data_records to
# n_obs, so a gate table would have reported 744 observations for a
# 155-observation dataset — 744 counts every record including dose rows. The
# record count is kept, under its own name. A summary missing any of the three
# is a stop: the field names are now verified, so an absent one means the
# summary is not what it claims to be, not that a default should be supplied.
.reference_nm_counts <- function(run_details) {
  pick <- function(field) {
    v <- run_details[[field]]
    if (is.null(v) || length(v) != 1L || is.na(v))
      stop("model_summary's run_details carries no usable ", field,
           "; the counts are verified fields of a real fit (live run ",
           "2026-09-04) and are never defaulted.", call. = FALSE)
    v
  }
  list(n_id      = pick("number_of_subjects"),
       n_obs     = pick("number_of_obs"),
       n_records = pick("number_of_data_records"))
}

# Is this control stream's data joinable by bbr::nm_join()?
#
# It is not, when $DATA is absolute — and the harness control streams use an
# absolute $DATA deliberately. bbr's get_data_path_from_json() computes
# fs::path_norm(file.path(output_dir, data_path_config)); for an absolute
# data path that yields <rundir>/<absolute path>, a file that cannot exist, and
# nm_join aborts with "Input data file does not exist or cannot be opened".
# The estimator and PsN may resolve relative paths from different
# directories. bbr expects a path relative to the run output directory.
#     directory.
# bbr 1.14.2 offers no way out at the call site: nm_join(), nm_tables() and
# nm_data() take neither an explicit data path nor a pre-read data frame, and
# nm_data() calls get_data_path(.mod) internally. So the lane REFUSES and
# reports it, rather than switching the control streams to relative paths
# (which would break the PsN lane) or silently returning a results record with
# no residuals.
.reference_nm_require_joinable_data <- function(ctl_path) {
  if (length(ctl_path) != 1L || is.na(ctl_path) || !file.exists(ctl_path))
    stop("the control stream (", ctl_path %||% "<none>", ") could not be read, ",
         "so its $DATA path cannot be checked against bbr's nm_join ",
         "requirement.", call. = FALSE)
  txt <- readLines(ctl_path, warn = FALSE)
  dat <- grep("^\\s*\\$DATA\\b", txt, value = TRUE)
  if (!length(dat))
    stop("the control stream ", ctl_path, " has no $DATA record; an ",
         "estimation model must name its dataset.", call. = FALSE)
  path <- sub("^\\s*\\$DATA\\s+", "", dat[1])
  path <- sub("\\s.*$", "", path)
  path <- gsub("^['\"]|['\"]$", "", path)
  # R10-P3-2. The test was `grepl("^/", path)` alone, which recognises only
  # POSIX absolute paths. A control stream is a portable text file and a
  # Windows-authored one carries `C:\data\pheno.csv` or `\\server\share\...`;
  # both are absolute and both hit the same bbr defect, so both are recognised.
  # PLATFORM NOTE: whether this harness RUNS on Windows is unverified -- the
  # engine is provisioned through a Linux container or a site install. The
  # broader test costs nothing and closes the gap for a control stream that
  # merely ORIGINATED on Windows, which needs no Windows R to happen.
  absolute <- grepl("^/", path) ||            # POSIX
              grepl("^[A-Za-z]:[/\\\\]", path) ||   # Windows drive
              grepl("^\\\\\\\\", path)              # Windows UNC
  if (absolute)
    stop("bbr::nm_join() cannot read this run's data and the results record ",
         "would have no residuals. $DATA is absolute (", path, "), and bbr ",
         "1.14.2's get_data_path_from_json() joins the recorded data path onto ",
         "the run output directory — file.path(<rundir>, <absolute path>) — so ",
         "it looks for a file that cannot exist. Two deliberate decisions ",
         "conflict: the harness writes an absolute $DATA for bbi/PsN cwd ",
         "parity (PsN rejects a quoted IGNORE sign), and bbr assumes the path ",
         "is output-directory-relative. nm_join(), nm_tables() and nm_data() ",
         "accept neither an explicit data path nor a pre-read data frame, so ",
         "there is no fix at this call site. Do NOT switch the control stream ",
         "to a relative $DATA — that breaks the PsN lane. Report this to the ",
         "SME as a finding (analyst runtime verification 2026-09-04, V1-D).",
         call. = FALSE)
  invisible(path)
}

# The bbi executable for this machine.
#
# bbr's default option value is the bare string "bbi", so on a machine where
# bbi is not on PATH bbi_version() returns "" and submit_model() fails. Nothing
# in the repository set options(bbr.bbi_exe_path) — not this file, not
# config/engine.yaml, not an .Rprofile (there is none). The path is
# machine-local, so it comes from harness.yaml's bbi_path key (git-ignored),
# exactly as nonmem_home does, and never from config/engine.yaml. The caller
# reads the key and passes the value, so this file stays path-free.
#
# `which` is injected so the resolution order can be tested without depending
# on what happens to be installed on the machine running the tests; `probe` is
# injected for the same reason, so the version probe can be exercised without a
# bbi installation.
#
# R10-P2-2. The resolver checked only file.exists() before installing the path
# as an executable, and the production entry point never resolved the path at
# all -- the live test read harness.yaml itself and passed the value by hand,
# so what the reference did on its own was untested. THE ONE SHARED RESOLVER
# now requires all four of:
#   - the path exists,
#   - it is not a directory,
#   - it is executable by this process (file.access(path, 1) == 0),
#   - `bbi version` runs and returns a non-empty version string,
# and it RETURNS the version so callers record it. tools/check_environment.R
# calls this same function; there is no second copy of the rule.
#
# Fix round 1 of batch 11 (independent review, finding 11.8). The probe merged
# stderr into stdout and ignored the exit status, so ANY executable passed:
# `/bin/sh version` fails with status 2 and prints "/bin/sh: 0: cannot open
# version: No such file" on stderr, and that sentence was taken as the first
# non-empty line and RECORDED AS THE ENGINE VERSION -- surfaced to the SME as
# `bbi_version` in tools/check_environment.R's environment fingerprint. A
# misconfigured bbi_path therefore produced a confidently reported, entirely
# fictitious engine identity, which is the one failure mode this harness exists
# to prevent. Every resolver test in the batteries injected `probe =`, so the
# real probe was never exercised.
#
# Three changes, each closing one step of that path:
#   - stderr is written to a temporary file, never merged into stdout, so an
#     error message can never become a version string;
#   - the exit status is captured and a non-zero status is a refusal;
#   - the surviving first non-empty line must LOOK like a version
#     (^v?<digits>.<digits>), so an executable that prints prose is refused
#     rather than recorded.
# The refusal path returns NA_character_ with a `why` attribute naming what the
# probe saw; the resolver quotes it. Injected probes that return a plain
# NA_character_ keep working unchanged.
# Fix round 2 of batch 11 (independent review, finding F2-c). The pattern used
# to be a PREFIX anchor and the whole line was recorded, so a stub printing
# "v1.2 is not bbi" was accepted and that entire string became the recorded
# engine version. Anchored at both ends: the trimmed first line is a version
# string or it is nothing.
MM_BBI_VERSION_PATTERN <- "^v?[0-9]+(\\.[0-9]+)+$"

# Seconds an engine-version probe may take before it is refused. An executable
# that cannot say what it is within this budget is not usable as an engine, and
# an unbounded probe hangs check_environment.R and the live battery for as long
# as the executable hangs (finding F2-a: 60 s demonstrated, unbounded in
# principle).
MM_VERSION_PROBE_TIMEOUT <- 30

.reference_nm_bbi_version <- function(path) {
  refuse <- function(why) structure(NA_character_, why = why)
  errfile <- tempfile("bbi-version-stderr")
  on.exit(unlink(errfile), add = TRUE)
  res <- tryCatch(suppressWarnings(
    system2(path, args = "version", stdout = TRUE, stderr = errfile,
            timeout = MM_VERSION_PROBE_TIMEOUT)),
    error = function(e) structure(character(0), status = -1L,
                                  msg = conditionMessage(e)))
  st <- attr(res, "status")
  # system2()'s timeout reports status 124, the same convention as timeout(1).
  if (!is.null(st) && identical(as.integer(st)[1], 124L))
    return(refuse(sprintf("`version` did not return within the %d second probe timeout",
                          MM_VERSION_PROBE_TIMEOUT)))
  if (!is.null(st) && !identical(as.integer(st), 0L))
    return(refuse(sprintf("`version` exited with exit status %s", as.integer(st)[1])))
  out <- trimws(as.character(res))
  out <- out[nzchar(out)]
  if (!length(out)) return(refuse("`version` produced no output on stdout"))
  # bbi prints its version on the first non-empty line, e.g. "v3.3.0".
  if (!grepl(MM_BBI_VERSION_PATTERN, out[1]))
    return(refuse(sprintf("`version` printed %s, which is not a version string",
                          encodeString(substr(out[1], 1L, 60L), quote = "\""))))
  out[1]
}

.reference_nm_require_bbi <- function(bbi_path = NULL, which = Sys.which,
                                      probe = .reference_nm_bbi_version) {
  resolved <- NULL
  if (!is.null(bbi_path) && length(bbi_path) == 1L && !is.na(bbi_path) &&
      nzchar(bbi_path)) {
    if (!file.exists(bbi_path))
      stop("the configured bbi_path (", bbi_path, ") does not exist. It is ",
           "read from harness.yaml (machine-local, git-ignored); correct the ",
           "key or install bbi with bbr::use_bbi().", call. = FALSE)
    if (isTRUE(file.info(bbi_path)$isdir))
      stop("the configured bbi_path (", bbi_path, ") is a directory, not the ",
           "bbi executable. Name the binary itself in harness.yaml's bbi_path ",
           "key.", call. = FALSE)
    if (!identical(unname(file.access(bbi_path, 1L)), 0L))
      stop("the configured bbi_path (", bbi_path, ") is not executable by this ",
           "process. bbr will hand it to system2() as a command; a file that ",
           "cannot be executed makes every submit_model() fail with an opaque ",
           "error. Correct the key or the file's mode.", call. = FALSE)
    resolved <- bbi_path
  } else {
    on_path <- tryCatch(unname(which("bbi")), error = function(e) "")
    if (length(on_path) == 1L && !is.na(on_path) && nzchar(on_path))
      resolved <- on_path
  }
  if (is.null(resolved))
    stop("the bbi executable was not found: no bbi_path was supplied and bbi is ",
         "not on PATH. bbr's default for this option is the bare string \"bbi\", ",
         "so leaving it unset makes bbi_version() empty and every submit_model() ",
         "fail. Set bbi_path: in harness.yaml (machine-local, git-ignored; see ",
         "harness.yaml.example), or install bbi with bbr::use_bbi().",
         call. = FALSE)
  version <- probe(resolved)
  if (is.na(version) || !nzchar(version)) {
    why <- attr(version, "why")
    if (is.null(why) || !nzchar(why)) why <- "`bbi version` produced no output"
    stop("bbi at ", resolved, " did not report a version: ", why, ". An ",
         "executable that cannot say what it is cannot be recorded in a run ",
         "record, so it is refused here rather than used and reported as an ",
         "unknown engine.", call. = FALSE)
  }
  options(bbr.bbi_exe_path = resolved)
  structure(resolved, bbi_version = version)
}

# The bbi_path key of harness.yaml (machine-local, git-ignored). ONLY that key
# is read, and the file's contents are never printed or returned: the rest of
# harness.yaml holds site paths and a NONMEM license location. Returns NULL
# when the file or the key is absent, so the PATH branch of the resolver can
# take over.
.reference_nm_bbi_path_from_harness <- function(
    path = file.path("harness.yaml")) {
  if (!file.exists(path)) return(NULL)
  ln <- tryCatch(readLines(path, warn = FALSE), error = function(e) character(0))
  # Anchored to the TOP LEVEL of the document (fix round 1 of batch 11): an
  # indented `bbi_path:` nested under some other mapping is a different key and
  # must not be picked up as this one.
  hit <- grep("^bbi_path\\s*:", ln, value = TRUE)
  if (!length(hit)) return(NULL)
  v <- trimws(sub("^bbi_path\\s*:\\s*", "", hit[1]))
  v <- sub("\\s+#.*$", "", v)
  v <- gsub("^['\"]|['\"]$", "", trimws(v))
  if (!nzchar(v) || identical(tolower(v), "null") || identical(v, "~"))
    return(NULL)
  v
}

# Verbatim termination text from the .lst tail; a missing or empty tail is a
# stop, never NA (the record message is audit evidence).
.reference_nm_termination_text <- function(lines) {
  lines <- as.character(lines)
  lines <- lines[!is.na(lines) & nzchar(trimws(lines))]
  if (length(lines) == 0)
    stop("no termination text could be read from the .lst tail; the run ",
         "record requires the verbatim termination lines. Treat as a failed ",
         "run.", call. = FALSE)
  paste(lines, collapse = " | ")
}

# The results record requires OFV and the subject/record counts; a NULL, NA
# or non-finite value is a stop, never a degraded NA field.
.reference_nm_require_counts <- function(ofv, n_id, n_obs) {
  chk <- function(x, what) {
    if (is.null(x) || length(x) != 1L || is.na(x) || !is.finite(as.numeric(x)))
      stop("required result field ", what, " is missing or non-finite (",
           deparse(x), "); the results record cannot degrade to NA.",
           call. = FALSE)
  }
  chk(ofv, "ofv"); chk(n_id, "n_id"); chk(n_obs, "n_obs")
  invisible(TRUE)
}

# Execution-time engine-config snapshot for the bbr submission (finding
# M7b). Freezes, beside the fit, the invocation line, model path,
# submission mode, the bbi mode/args in effect, the config source file and
# its sha256, and the bbr version — all read from the ONE resolved config
# object of the run (see .reference_nm_resolve_config). Mirrors the header
# format of tools/nonmem_exec.sh's snapshot_config so PsN and bbr lanes read
# alike.
.reference_nm_reserve_evidence_path <- function(directory, stem, extension) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(directory))
    stop("cannot reserve evidence: directory unavailable: ", directory, call. = FALSE)
  attempts <- 0L
  repeat {
    attempts <- attempts + 1L
    candidate <- tempfile(pattern = paste0(stem, "-"), tmpdir = directory,
                          fileext = extension)
    connection <- suppressWarnings(try(file(candidate, open = "wx"),
                                       silent = TRUE))
    if (!inherits(connection, "try-error")) {
      close(connection)
      return(candidate)
    }
    if (!file.exists(candidate) || attempts >= 100L)
      stop("cannot reserve evidence file: ", candidate, "; ",
           as.character(connection), call. = FALSE)
  }
}

.reference_nm_fit_snapshot <- function(model_dir, model_path, mode, resolved) {
  cfg_path <- resolved$path
  sha <- resolved$sha256
  cfg  <- resolved$cfg
  bbi  <- cfg$bbi
  args <- bbi$args

  bbr_ver <- tryCatch(as.character(utils::packageVersion("bbr")),
                      error = function(e) NA_character_)

  snap_dir <- file.path(model_dir, "engine-config-used")
  dir.create(snap_dir, showWarnings = FALSE)
  ts   <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
  snap <- .reference_nm_reserve_evidence_path(
    snap_dir, paste0(ts, "-bbr-fit"), ".yaml")

  header <- c(
    "# Engine-config snapshot (written by reference_nm_fit)",
    "# engine: nonmem (bbr/bbi estimation lane)",
    "# tool: bbr submit_model",
    paste0("# invoked_at_utc: ", ts),
    paste0("# model: ", model_path),
    paste0("# mode: ", mode),
    paste0("# config_source: ", normalizePath(cfg_path, mustWork = FALSE)),
    paste0("# config_sha256: ", sha),
    paste0("# r_packages: bbr ", bbr_ver),
    "#")
  body <- yaml::as.yaml(list(bbi = list(mode = bbi$mode %||% mode, args = args)))
  writeLines(c(header, body), snap)
  invisible(snap)
}

# ---------------------------------------------------------------------------
# reference_nm_fit(model_dir, run, based_on, hypothesis, mode, wait)
#
# Reads (or creates) the numbered model, submits it through the shim in
# LOCAL mode, and asserts the provenance record exists. Returns a
# normalized bundle carrying the model object, the run metadata, and the
# convergence verdict — the same meta/counts shape the nlmixr2 lane uses
# (tools/nlmixr_runs.R), so run_report.R and the gate tables consume both
# lanes identically.
#
# model_dir   directory holding the control stream <run>.mod (or .ctl)
# run         model id (string or number), e.g. "101"
# based_on    parent model id(s) for the run record; NULL for a reference
# hypothesis  free-text structure/hypothesis for the run record
# mode        submission mode fallback; the resolved config's bbi$mode wins
#             when set ("local" for the containerized provision — bbi's
#             Linux default is "sge" and fails off-grid)
# wait        must be TRUE: gated operation reads the run's outputs right
#             after submission, so an asynchronous submission would attest
#             files that do not yet exist (or stale ones). FALSE is a stop.
# repo_root   base for config/engine.yaml and tools/ (see reference_nm_workspace)
# nonmem_home native provision only (see reference_nm_workspace)
# bbi_path    the bbi executable. Machine-local, so it comes from
#             harness.yaml's bbi_path key (git-ignored), never from
#             config/engine.yaml. NULL falls back to bbi on PATH; when
#             neither is available the fit stops before writing anything.
#             bbr's own default is the bare string "bbi", which silently
#             yields an empty bbi_version() — hence the explicit resolution.
#
# Returns list(mod, meta, converged, message, config_json, config, run_start).
reference_nm_fit <- function(model_dir, run, based_on = NULL,
                             hypothesis = NA_character_,
                             mode = "local", wait = TRUE,
                             repo_root = getwd(), nonmem_home = NULL,
                             bbi_path = NULL) {
  .nonmem_estimation_gate()
  stopifnot(!missing(run))
  if (!isTRUE(wait)) {
    stop("reference_nm_fit: wait = TRUE is required in gated operation; the ",
         "run's outputs are read and attested immediately after submission, ",
         "which an asynchronous submission cannot support.", call. = FALSE)
  }
  # The bbi executable is machine-local: harness.yaml's bbi_path key, passed
  # in by the caller. Resolved BEFORE anything is written, so a machine with
  # no bbi fails here rather than half way through a submission — but AFTER
  # the argument contract above, so a caller's mistake is reported as its own
  # mistake rather than as an environment problem.
  # R10-P2-2: the entry point resolves the path ITSELF. It used to default to
  # NULL and never read harness.yaml, so the only thing that ever supplied a
  # bbi_path was the live test, by hand -- the production path was untested.
  # Only the bbi_path key is read; harness.yaml also holds site paths and a
  # NONMEM license location and is never printed.
  if (is.null(bbi_path)) bbi_path <- .reference_nm_bbi_path_from_harness()
  .reference_nm_require_bbi(bbi_path)

  # ONE config object for this run: bbi.yaml, the submission mode and the
  # snapshot all derive from it (round 8, finding 7).
  resolved <- .reference_nm_resolve_config(model_dir, repo_root)
  reference_nm_workspace(model_dir, resolved$cfg, repo_root = repo_root,
                         nonmem_home = nonmem_home)
  mode <- resolved$cfg$bbi$mode %||% mode

  base <- file.path(model_dir, as.character(run))
  yaml_sidecar <- paste0(base, ".yaml")

  # read_model when the sidecar exists (rerun / new session); new_model to
  # register a freshly authored control stream. bbr accepts .ctl or .mod
  # on disk; the harness standard is .mod.
  mod <- if (file.exists(yaml_sidecar)) {
    bbr::read_model(base)
  } else {
    m <- bbr::new_model(base, .description = hypothesis)
    # based_on is normally populated by copy_model_from at model creation
    # (inventory 1.1). For a hand-authored new_model the sidecar carries it
    # instead; bbr::add_based_on() is the expected setter but is NOT in the
    # verified inventory — confirm at first real run before relying on it.
    m
  }

  # Execution-time engine-config snapshot (finding M7b). The workspace
  # writer (reference_nm_workspace) freezes the config it PROVISIONED from,
  # but submission itself previously left no execution record — no
  # config_source, sha256, or argv at fit time. Freeze the operational
  # config in effect at submission, matching the conventions of
  # tools/nonmem_exec.sh's snapshot_config (invoked_at, argv, config_source,
  # config_sha256, R-packages line) and tools/engine_config.R's
  # load_engine_config (per-run run-config.yaml wins over the house default,
  # and its path is the recorded config_source).
  .reference_nm_fit_snapshot(model_dir, base, mode, resolved)

  # Pre-run artifact inventory and start marker (round 8, finding 7): every
  # output attested below must be NEWER than this instant, so a stale
  # bbi_config.json or .lst from a prior run can never satisfy the checks.
  run_start <- Sys.time()
  pre_inventory <- list.files(model_dir, recursive = TRUE, full.names = TRUE)
  inv_dir <- file.path(model_dir, "engine-config-used")
  dir.create(inv_dir, showWarnings = FALSE)
  inv_file <- .reference_nm_reserve_evidence_path(
    inv_dir,
    paste0(format(run_start, "%Y%m%dT%H%M%SZ", tz = "UTC"), "-",
           as.character(run), "-pre-run-inventory"),
    ".txt")
  writeLines(c(paste0("# pre-run inventory for run ", run, " at ",
                      format(run_start, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
               paste0("# config_source: ", resolved$path),
               paste0("# config_sha256: ", resolved$sha256),
               pre_inventory), inv_file)

  # Submit through the shim. .mode is the resolved config's; .wait = TRUE so
  # the gated pipeline sees a finished run. bbi_config.json is written by
  # bbi on the host after a successful run — its absence means bbi aborted
  # before NONMEM started, an explicit error state (never a warning).
  bbr::submit_model(mod, .mode = mode, .wait = TRUE)

  config_json <- bbr::get_config_path(mod)
  .reference_nm_require_fresh(config_json, run_start,
                              paste0("bbi_config.json for run ", run))
  # The listing (.lst) is the run's own output; bbr::build_path_from_model
  # is the inventory's path builder for run-relative artifacts. Verified
  # against a live fit for the .lst extension on 2026-09-04: the returned path
  # exists and is newer than the recorded run start, and both freshness checks
  # pass on a containerized NONMEM 7.5.1 run.
  lst_path <- tryCatch(bbr::build_path_from_model(mod, ".lst"),
                       error = function(e) NA_character_)
  .reference_nm_require_fresh(lst_path, run_start,
                              paste0(".lst listing for run ", run))

  # Convergence verdict from the summary heuristics: a clean run has every
  # run_heuristics flag FALSE (inventory section 3.1). The verbatim
  # termination line from the .lst is the audit source of truth; captured
  # here as the record message and REQUIRED (a missing tail is a stop).
  s <- bbr::model_summary(mod)
  heur <- s$run_heuristics
  if (!is.list(heur) || length(heur) == 0)
    stop("model_summary returned no run_heuristics for run ", run,
         "; the convergence verdict cannot be formed. Treat as a failed run.",
         call. = FALSE)
  converged <- !any(vapply(heur, isTRUE, logical(1)))
  # The termination block, grepped from the listing bytes. The head/tail
  # window is kept as supplementary context (start banner, #CPUT, Stop Time)
  # when bbr returns it, but it is NOT where the status lives — see
  # .reference_nm_termination_lines. .return = TRUE is required or tail_lst
  # returns invisible(NULL).
  window <- tryCatch(bbr::tail_lst(mod, .print = FALSE, .return = TRUE),
                     error = function(e) character(0))
  message_txt <- .reference_nm_termination_text(
    c(.reference_nm_termination_lines(lst_path), as.character(window)))

  list(
    mod  = mod,
    meta = list(run = as.character(run),
                based_on = based_on %||% "",
                hypothesis = hypothesis),
    converged = converged,
    message   = message_txt,
    config_json = config_json,
    config = list(source = resolved$path, sha256 = resolved$sha256, mode = mode),
    run_start = run_start,
    pre_run_inventory = inv_file)
}

# ---------------------------------------------------------------------------
# reference_nm_results(mod, join_col)
#
# The two-call results pattern. model_summary() -> param_estimates() for
# estimates/OFV/RSE/BSV/shrinkage; nm_join() for row-level residuals. Both
# are merged into the shaped list the harness run records use so the
# diagnostics and reporting lanes read NONMEM and nlmixr2 fits identically.
#
# mod       a bbi_nonmem_model object (from reference_nm_fit)
# join_col  row-index column for nm_join; harness convention "NUM" (must be
#           written to every $TABLE block in the control stream)
#
# Returns list(ofv, n_id, n_obs, params, shrinkage, residuals, heuristics).
# `params` mirrors tools/run_report.R columns (parameter, estimate,
# rse_pct, bsv_cv_pct, shrink_sd_pct); back_transform / CI are left to the
# reporting stage (house 3-sigfig, format-time) and flagged unverified.
reference_nm_results <- function(mod, join_col = "NUM") {
  .nonmem_estimation_gate()
  s <- bbr::model_summary(mod)

  # OFV — verified structure: s$ofv is a LIST of per-estimation-method
  # entries, each with ofv_no_constant (objective without the constant
  # term). The FINAL method's value is the reported OFV.
  ofv <- as.numeric(s$ofv[[length(s$ofv)]]$ofv_no_constant)

  # Parameter table. Verified param_estimates columns: parameter_names,
  # estimate, stderr, random_effect_sd, random_effect_sdse, fixed, diag,
  # shrinkage. RSE = stderr/abs(estimate); NA without $COV in the control
  # stream (verified). BSV as CV% from the omega SD is left to the
  # reporting convention; random_effect_sd carried through as-is here.
  pe <- bbr::param_estimates(s)
  params <- data.frame(
    parameter     = pe$parameter_names,
    estimate      = pe$estimate,
    stderr        = pe$stderr,
    rse_pct       = 100 * pe$stderr / abs(pe$estimate),
    random_effect_sd = pe$random_effect_sd,
    shrink_sd_pct = pe$shrinkage,   # SD-scale % per ETA/EPS row
    fixed         = pe$fixed,
    stringsAsFactors = FALSE)

  # Shrinkage detail (verified structure): one element per estimation
  # method, per sub-population. eta_sd is the SD-scale % vector.
  shr <- tryCatch(s$shrinkage_details[[1]][[1]],
                  error = function(e) NULL)
  shrinkage <- if (is.null(shr)) NULL else
    list(eta_sd = shr$eta_sd, eps_sd = shr$eps_sd,
         eta_bar = shr$eta_bar, pval = shr$pval)

  # Row-level residuals. nm_join merges $TABLE files with the input data on
  # join_col (default NUM). CWRES/NPDE/ETA columns appear only if requested
  # in $TABLE — their presence is a control-stream property, not enforced
  # here; the diagnostics lane fails on missing required columns.
  # nm_join errors with "No table files were found" when the control
  # stream has no $TABLE records (verified on the smoke model) — rethrown
  # here with the harness requirement attached, because an estimation
  # model without residual tables cannot pass the diagnostics lane.
  # An absolute $DATA and bbr's nm_join are mutually incompatible; refuse
  # before the call, naming both decisions, rather than after it with bbr's
  # own message about a path nobody wrote.
  .reference_nm_require_joinable_data(bbr::get_model_path(mod))

  residuals <- tryCatch(
    bbr::nm_join(mod, .join_col = join_col),
    error = function(e) {
      stop("nm_join failed (", conditionMessage(e), ") — the harness ",
           "requires every estimation control stream to carry $TABLE ",
           "records with a ", join_col, " row-index column (see ",
           "skills/nonmem-estimation/SKILL.md, control-stream ",
           "requirements).", call. = FALSE)
    })

  # Subject / observation / record counts from run_details. Field names
  # verified on the live fit of 2026-09-04 (59 / 155 / 744). REQUIRED (round 8,
  # finding 7): a missing count or OFV is a stop, never an NA that flows into
  # the gate tables. n_obs is the OBSERVATION count; the record count, which
  # includes dose rows, travels under its own name.
  cnt <- .reference_nm_counts(s$run_details)
  n_id <- cnt$n_id; n_obs <- cnt$n_obs; n_records <- cnt$n_records
  .reference_nm_require_counts(ofv, n_id, n_obs)

  list(
    ofv        = ofv,
    n_id       = n_id,
    n_obs      = n_obs,
    n_records  = n_records,
    params     = params,
    shrinkage  = shrinkage,
    residuals  = residuals,
    heuristics = s$run_heuristics)
}
