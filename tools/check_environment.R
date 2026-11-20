# Environment verification (decision D9).
#
# PsN and NONMEM are assumed site-provided; the harness never installs or
# configures them. Two provision modes (amendment 2026-08-30):
#   native        — PsN/NONMEM shell-callable on the host (original D9)
#   containerized — the locally built pmx-harness/nonmem image, invoked
#                   through tools/nonmem_container.sh with the license
#                   mounted at runtime (image built from site-held ICON
#                   media; never pulled from a registry, never committed)
# This check runs as the first target of every pipeline and fails fast
# with a clear message when the assumption does not hold, instead of
# dying mid-analysis.

# Verifies the runtime before any pipeline work runs. require_nonmem /
# require_psn are set by the composer from what the declared tasks actually
# need (only a non-nlmixr2 estimation engine pulls them in), and so is
# `pkgs`. Collects every problem before stopping, so one failed run reports
# the complete fix list. Returns list(checked_at, status = "ok") on
# success; stops otherwise.
`%||%` <- function(x, y) if (is.null(x)) y else x

if (!exists("read_reviewed_yaml", mode = "function") &&
    file.exists("tools/yaml_scalar.R")) {
  source("tools/yaml_scalar.R", local = TRUE)
}

check_environment <- function(model_dir = "model/pk",
                              require_nonmem = TRUE,
                              require_psn = TRUE,
                              nonmem_provision = c("containerized", "native"),
                              pkgs = c("targets", "bbr", "yspec", "mrgda",
                                       "pmplots", "pmtables", "mrggsave")) {
  nonmem_provision <- match.arg(nonmem_provision)

  # Environment confirmation (SME requirement 2026-09-03). In a managed
  # environment the engines arrive on the user's PATH; the harness must never
  # guess where they are. `copilot-adapter/setup.py install` probes this
  # machine and writes the detected R/NONMEM/PsN paths into harness.yaml with
  # environment_confirmed: false. Until the SME has read those values and set
  # the flag to true, no pipeline runs — this is the first check of the first
  # target of every pipeline, before any other probe.
  harness <- if (file.exists("harness.yaml")) read_reviewed_yaml("harness.yaml") else NULL
  confirmed <- if (is.null(harness)) FALSE else
    reviewed_yaml_scalar("harness.yaml", "environment_confirmed",
                         expected = "logical", required = TRUE,
                         document = harness)
  if (!isTRUE(confirmed)) {
    stop("harness.yaml is missing or not confirmed: run ",
         "`python3 copilot-adapter/setup.py install` (or `env`) to probe this ",
         "machine, review the detected R/NONMEM/PsN paths, and set ",
         "environment_confirmed: true. No pipeline runs before the SME ",
         "confirms the environment.", call. = FALSE)
  }

  # Machine-local resolvers (nonmem_home, nonmem_image) live in the tools
  # layer next door. In-pipeline tar_source("tools") has already defined
  # them; an ad-hoc caller that sourced only this file gets them here.
  if ((!exists("resolve_nonmem_home", mode = "function") ||
       !exists("resolve_nonmem_license", mode = "function")) &&
      file.exists("tools/engine_config.R")) {
    source("tools/engine_config.R", local = TRUE)
  }

  problems <- character(0)
  # `<<-` appends to `problems` in the enclosing frame: accumulate every
  # finding rather than failing on the first
  note <- function(msg) problems <<- c(problems, msg)

  if (nonmem_provision == "containerized" && (require_psn || require_nonmem)) {
    # Containerized mode: one wrapper serves both NONMEM and PsN; probe
    # the pieces the wrapper needs — docker, the local image, the wrapper
    # script, and the runtime-mounted license.
    if (!nzchar(Sys.which("docker"))) {
      note("docker not found on PATH (containerized NONMEM provision).")
    } else {
      # Image resolution, in order: NONMEM_IMAGE, then the machine-local
      # nonmem_image: key in harness.yaml, then the committed default —
      # the same order tools/nonmem_container.sh applies.
      img <- resolve_nonmem_image()
      have <- tryCatch(
        length(system2("docker", c("images", "-q", img), stdout = TRUE)) > 0,
        error = function(e) FALSE)
      if (!have) note(paste0("NONMEM container image not built: ", img,
                             " (build locally from site ICON media)."))
    }
    if (!file.exists("tools/nonmem_container.sh")) {
      note("tools/nonmem_container.sh missing (containerized executor).")
    }
    if (!file.exists("tools/nonmem_exec.sh")) {
      note("tools/nonmem_exec.sh missing (provision dispatcher).")
    }
    # License resolution, in order: NONMEM_LICENSE, then the machine-local
    # nonmem_license: key in harness.yaml. No path is ever invented — an
    # unconfigured license is reported as such, not as a missing file at some
    # developer's home directory.
    lic <- resolve_nonmem_license(harness)
    if (!nzchar(lic)) {
      note(paste0("NONMEM license not configured: set NONMEM_LICENSE or ",
                  "nonmem_license: in harness.yaml (the license mounts at ",
                  "runtime; see harness.yaml.example)."))
    } else if (!file.exists(lic)) {
      # No path in the note: this message reaches the pipeline log (B0e).
      note(paste0("NONMEM license not found at the configured path ",
                  "(set NONMEM_LICENSE or harness.yaml nonmem_license:)."))
    }
  }

  if (nonmem_provision == "containerized" && require_nonmem) {
    # bbr estimation lane: bbi drives the container through the nmfe
    # shim (bbi execs {home}/run/{executable}, so the shim lives under
    # run/). Both pieces are preconditions for submit_model().
    if (!file.exists("tools/nonmem_shim/run/nmfe75")) {
      note("tools/nonmem_shim/run/nmfe75 missing (bbi nmfe shim for the containerized provision).")
    }
    # bbi executable: the harness.yaml key, then the bbr option, then PATH.
    # No path is invented — this probe used to fall back to a guessed
    # ~/.local/share/bbi/bbi, which reported a specific path as "not found"
    # on a machine that had never been asked where bbi is (batch 10, V1-C).
    bbi <- .env_bbi_path(harness)
    if (!nzchar(bbi)) {
      note(paste0("bbi executable not found: set bbi_path: in harness.yaml ",
                  "(see harness.yaml.example) or put bbi on PATH ",
                  "(install with bbr::use_bbi())."))
    }
  }

  if (nonmem_provision == "native" && require_psn) {
    psn <- Sys.which("execute")   # PsN's core submission wrapper
    if (!nzchar(psn)) {
      note("PsN 'execute' not found on PATH (D9 assumes PsN is installed and shell-callable).")
    } else {
      v <- tryCatch(
        suppressWarnings(system2("execute", "-version", stdout = TRUE, stderr = TRUE)),
        error = identity)
      if (inherits(v, "error")) {
        note(paste0("PsN 'execute -version' failed to run: ", conditionMessage(v)))
      } else {
        status <- attr(v, "status") %||% 0L
        if (!identical(as.integer(status), 0L)) {
          diagnostic <- trimws(v[nzchar(trimws(v))])
          note(paste0("PsN 'execute -version' failed with status ", status,
                      if (length(diagnostic)) paste0(": ", paste(diagnostic, collapse = " | "))
                      else "."))
        }
      }
    }
  }

  if (nonmem_provision == "native" && require_nonmem) {
    # NONMEM itself is not probed directly; bbr's bbi.yaml carries its path
    # and is the practical precondition for estimation runs.
    bbi_yaml <- file.path(model_dir, "bbi.yaml")
    if (!file.exists(bbi_yaml)) {
      note(paste0("bbr configuration not found: ", bbi_yaml,
                  " (holds the NONMEM path; create with bbr::bbi_init())."))
    }
    # Site NONMEM install root. bbi executes <nonmem_home>/run/<executable>,
    # so the check is: the value is configured at all, and an nmfe7* exists
    # where bbi will look for it. Resolution order is NONMEM_HOME then the
    # harness.yaml key (resolve_nonmem_home); no path is ever invented and
    # nothing under the root is executed. Not required under the
    # containerized provision, where the bbi shim stands in for the install.
    nm_home <- resolve_nonmem_home()
    if (is.null(nm_home)) {
      note(paste0("NONMEM install root not configured: set NONMEM_HOME or ",
                  "nonmem_home: in harness.yaml (bbi executes ",
                  "<nonmem_home>/run/nmfe75; see harness.yaml.example)."))
    } else {
      run_dir <- file.path(nm_home, "run")
      found <- if (dir.exists(run_dir))
        list.files(run_dir, pattern = "^nmfe7") else character(0)
      if (length(found) == 0) {
        note(paste0("no nmfe7* executable under ", run_dir,
                    " (from nonmem_home = ", nm_home,
                    "; bbi executes <nonmem_home>/run/<executable>)."))
      }
    }
  }

  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      note(paste0("R package not installed: ", pkg))
    }
  }

  # R provision (amendment 2026-08-31): under "site" (harness.yaml,
  # regulated environments) the validated system library is verified
  # against the committed manifest — every mismatch is reported for SME
  # disposition, never auto-fixed. Under "managed" a mismatch is only
  # messaged (the dev machine may deliberately be ahead of the lock).
  if (file.exists("tools/r_lock.R")) {
    source("tools/r_lock.R", local = TRUE)
    mism <- verify_r_packages()
    if (length(mism) > 0) {
      if (identical(r_provision(), "site")) {
        for (m in mism) note(paste0("R manifest: ", m))
      } else {
        message("R manifest deviations (managed provision, informational):\n  - ",
                paste(mism, collapse = "\n  - "))
      }
    }
  }

  if (length(problems) > 0) {
    stop("Environment check failed:\n  - ",
         paste(problems, collapse = "\n  - "),
         "\nFix the environment before running the pipeline (see D9 in docs/ARCHITECTURE.md).",
         call. = FALSE)
  }

  list(checked_at = format(Sys.time(), tz = "UTC"), status = "ok")
}

# --- environment attestation (round 6 finding 6; round 7 finding 10) --------
# The always-cued pipeline target `environment_attestation` (composed by
# pipeline/compose.R and named by EVERY other target) evaluates this function.
# The function is named differently from the target on purpose: targets
# refuses a global object that shares a target's name ("Targets and globals
# must have unique names"), and tar_source("tools") puts this function in the
# global environment.
# It first runs check_environment() — so the environment_confirmed refusal and
# every probe above still apply — and then returns a STABLE description of the
# environment: the probed state plus the confirmation flag, and a sha256
# fingerprint over a canonical flattening of that state.
#
# Stability is the point. targets invalidates downstream targets when an
# upstream VALUE changes, so the value must carry nothing that varies between
# two runs on an unchanged machine: no timestamps, no run identifiers, no
# temp paths. Then downstream stays valid while nothing changed, and a change
# of R version, executable, image, lock state or provision invalidates every
# stage, package and gate — which is the desired behaviour (round 6, F6).
#
# What is recorded: R version, platform, OS and locale; the two provision
# modes; the resolved paths and version strings of the executables the declared
# tasks need (docker/bbi/PsN when NONMEM is required; quarto and tectonic
# always, since the reporting stage renders through them); the container image
# name and id under the containerized provision; whether a license is
# configured and present (never its path or contents); the r-packages lock
# result and the installed versions of the required packages; the library
# roots. The gate package binds only the fingerprint and the two provisions
# (tools/gate_package.R), so no harness.yaml value beyond the provision mode
# reaches a gate record, and nothing here is printed to the log.
# The bbi executable, resolved without inventing a path: the machine-local
# harness.yaml key `bbi_path`, then an explicitly set options(bbr.bbi_exe_path),
# then PATH. Returns "" when bbi cannot be located, so the caller reports "not
# found" rather than naming a path nobody configured. bbr's own default for the
# option is the bare string "bbi", which is a PATH lookup, not a real path, so
# it is only honoured here when it points at a file that exists.
# (Integrity batch 10, live-run finding V1-C.)
.env_bbi_path <- function(harness = NULL) {
  key <- as.character(harness[["bbi_path"]] %||% "")
  if (length(key) == 1L && nzchar(key) && file.exists(key)) return(key)
  opt <- as.character(getOption("bbr.bbi_exe_path", ""))
  if (length(opt) == 1L && nzchar(opt) && file.exists(opt)) return(opt)
  unname(Sys.which("bbi"))
}

.env_exe_probe <- function(name, args = "--version", strict = FALSE) {
  path <- unname(Sys.which(name))
  if (!nzchar(path)) {
    if (strict) stop("Required executable not found: ", name, call. = FALSE)
    return(list(path = "", version = ""))
  }
  out <- tryCatch(suppressWarnings(system2(path, args, stdout = TRUE, stderr = TRUE)),
                  error = function(e) {
                    if (strict) stop("Required executable failed to launch: ", name, call. = FALSE)
                    character(0)
                  })
  if (strict && !identical(as.integer(attr(out, "status") %||% 0L), 0L))
    stop("Required executable version probe failed: ", name, call. = FALSE)
  out <- out[nzchar(trimws(out))]
  if (strict && !length(out))
    stop("Required executable version probe returned no version: ", name, call. = FALSE)
  list(path = path, version = if (length(out)) trimws(out[1]) else "")
}

# --- Engine versions under the containerized provision --------------------
# Runs one command in the NONMEM image through the provision dispatcher, with
# the R session's tempdir() as the work directory (R removes it at exit).
# Both streams are captured and nothing captured is ever printed, messaged or
# put in a condition: the container script prints the licence path on stderr
# when the licence file is missing (tools/nonmem_container.sh line 46).
# Its own function so a test can replace it by assignment.
.env_run_wrapper <- function(args) {
  out <- tryCatch(
    suppressWarnings(system2("tools/nonmem_exec.sh", shQuote(c(tempdir(), args)),
                             stdout = TRUE, stderr = TRUE)),
    error = function(e) structure(character(0), status = 127L))
  list(lines = as.character(out), status = as.integer(attr(out, "status") %||% 0L))
}

# The first line matching `pattern`, trimmed, or "" when none matches.
.env_version_line <- function(lines, pattern) {
  hit <- grep(pattern, trimws(lines), value = TRUE)
  if (length(hit)) hit[1] else ""
}

# The PsN version line and two separately labelled NONMEM version fields:
# PsN's registry (`psn -nm_versions`, its `default` entry) and the image's
# declared version (`/nonmem/nm751/SETUP75` line 2). Stops, naming the probe and
# its exit status only, when a probe exits nonzero (even if a line matched),
# prints no version line, or the two NONMEM versions disagree.
.env_engine_versions <- function() {
  probe <- function(name, args, pattern) {
    r <- .env_run_wrapper(args)
    if (!isTRUE(r$status == 0))
      stop(name, " version probe failed with exit status ", r$status,
           " (output withheld).", call. = FALSE)
    line <- .env_version_line(r$lines, pattern)
    if (!nzchar(line))
      stop(name, " version probe printed no version line (exit status 0; ",
           "output withheld).", call. = FALSE)
    line
  }
  psn <- probe("PsN", c("execute", "-version"), "^PsN version: ")
  reg <- probe("NONMEM registry", c("psn", "-nm_versions"),
               "^default\\s+\\(.*,[0-9][0-9.]*\\)$")
  setup <- probe("NONMEM SETUP75",
                 c("grep", "-m1", "^# SETUP75 version ", "/nonmem/nm751/SETUP75"),
                 "^# SETUP75 version [0-9]")
  v_reg <- sub("^.*,([0-9][0-9.]*)\\)$", "\\1", reg)
  v_set <- sub("^# SETUP75 version ([0-9][0-9.]*).*$", "\\1", setup)
  if (!identical(v_reg, v_set))
    stop("NONMEM versions disagree: PsN registry ", v_reg, ", SETUP75 ", v_set,
         ".", call. = FALSE)
  list(psn = psn, nonmem_psn_registry = reg, nonmem_setup75 = setup)
}

# Canonical flattening of a nested list to "key=value" lines with sorted
# keys, for hashing. Atomic vectors are joined with "\x1f"; NULL and empty are
# written as empty values so a missing field and an empty one hash alike.
.env_flatten <- function(x, prefix = "") {
  if (is.list(x)) {
    if (!length(x)) return(paste0(prefix, "="))
    nm <- names(x)
    if (is.null(nm) || any(!nzchar(nm))) nm <- as.character(seq_along(x))
    o <- order(nm)
    unlist(lapply(o, function(i) .env_flatten(x[[i]], paste0(prefix, "/", nm[i]))),
           use.names = FALSE)
  } else {
    paste0(prefix, "=", paste(as.character(x), collapse = "\x1f"))
  }
}

attest_environment <- function(model_dir = "model/pk",
                                    require_nonmem = TRUE,
                                    require_psn = TRUE,
                                    nonmem_provision = c("containerized", "native"),
                                    pkgs = c("targets", "bbr", "yspec", "mrgda",
                                             "pmplots", "pmtables", "mrggsave"),
                                    require_review = FALSE) {
  if (!is.logical(require_review) || !is.null(dim(require_review)) ||
      length(require_review) != 1L || is.na(require_review))
    stop("require_review must be a non-missing logical scalar.", call. = FALSE)
  nonmem_provision <- match.arg(nonmem_provision)
  # The refusal (environment_confirmed not true) and every probe stop here
  # exactly as before; the attestation is built only for a passing check.
  check_environment(model_dir = model_dir, require_nonmem = require_nonmem,
                    require_psn = require_psn, nonmem_provision = nonmem_provision,
                    pkgs = pkgs)
  if ((!exists("resolve_nonmem_image", mode = "function") ||
       !exists("resolve_nonmem_license", mode = "function")) &&
      file.exists("tools/engine_config.R"))
    source("tools/engine_config.R", local = TRUE)
  harness <- if (file.exists("harness.yaml")) read_reviewed_yaml("harness.yaml") else NULL

  r_prov <- if (file.exists("tools/r_lock.R")) {
    source("tools/r_lock.R", local = TRUE); r_provision()
  } else as.character(harness[["r_provision"]] %||% "managed")
  lock <- if (exists("verify_r_packages", mode = "function")) sort(verify_r_packages())
          else "no lock file (tools/r_lock.R absent)"

  needs_engine <- require_nonmem || require_psn
  exes <- list(
    Rscript  = .env_exe_probe("Rscript", "--version"),
    quarto   = .env_exe_probe("quarto", "--version"),
    tectonic = .env_exe_probe("tectonic", "--version"))
  if (require_review) {
    exes$pdftotext <- .env_exe_probe("pdftotext", "-v", strict = TRUE)
    exes$unzip <- .env_exe_probe("unzip", "-v", strict = TRUE)
  }
  image <- list(name = "", id = "")
  if (needs_engine && nonmem_provision == "containerized") {
    exes$docker <- .env_exe_probe("docker", "--version")
    img <- resolve_nonmem_image()
    id <- tryCatch(suppressWarnings(system2("docker", c("images", "-q", img),
                                            stdout = TRUE, stderr = TRUE)),
                   error = function(e) character(0))
    image <- list(name = img, id = if (length(id)) trimws(id[1]) else "")
    v <- .env_engine_versions()
    exes$psn_execute <- list(path = "tools/nonmem_exec.sh", version = v$psn)
    image$nonmem_version_psn_registry <- v$nonmem_psn_registry
    image$nonmem_version_setup75 <- v$nonmem_setup75
  }
  if (require_nonmem && nonmem_provision == "containerized") {
    # R10-P2-2: ONE shared resolver. The rule for what counts as a usable bbi
    # (exists, not a directory, executable, and reports a version) lives in
    # skills/nonmem-estimation/reference/bbr_estimation.R and is called here
    # rather than restated -- this function used to accept any existing path
    # and still report environment_confirmed = TRUE with an empty version.
    # A refusal is reported as an empty version, which is what the caller
    # already treats as "not usable"; the fingerprint records the version.
    bbi_path <- .env_bbi_path(harness)
    bbi_ver <- ""
    if (nzchar(bbi_path)) {
      if (!exists(".reference_nm_require_bbi", mode = "function"))
        sys.source(file.path("skills", "nonmem-estimation", "reference",
                             "bbr_estimation.R"),
                   envir = environment(), keep.source = FALSE)
      r <- tryCatch(.reference_nm_require_bbi(bbi_path), error = function(e) e)
      bbi_ver <- if (inherits(r, "error")) "" else
        as.character(attr(r, "bbi_version") %||% "")
    }
    exes$bbi <- list(path = bbi_path,
                     version = if (length(bbi_ver) != 1L || is.na(bbi_ver)) ""
                               else bbi_ver)
  }
  if (require_psn && nonmem_provision == "native")
    exes$psn_execute <- .env_exe_probe("execute", "-version")
  nonmem <- list(home = "", nmfe = character(0))
  if (require_nonmem && nonmem_provision == "native") {
    nm_home <- resolve_nonmem_home()
    if (!is.null(nm_home)) {
      run_dir <- file.path(nm_home, "run")
      nonmem <- list(home = nm_home,
                     nmfe = if (dir.exists(run_dir))
                       sort(list.files(run_dir, pattern = "^nmfe7")) else character(0))
    }
  }
  lic <- resolve_nonmem_license(harness)
  license <- list(configured = nzchar(lic), present = nzchar(lic) && file.exists(lic))

  pkg_versions <- vapply(sort(unique(pkgs)), function(p)
    tryCatch(as.character(utils::packageVersion(p)), error = function(e) "MISSING"),
    character(1))

  probe <- list(
    schema_version = 1L,
    r_version = as.character(getRversion()),
    platform = R.version$platform,
    os = list(sysname = unname(Sys.info()[["sysname"]]),
              release = unname(Sys.info()[["release"]]),
              machine = unname(Sys.info()[["machine"]])),
    locale = list(collate = Sys.getlocale("LC_COLLATE"), ctype = Sys.getlocale("LC_CTYPE")),
    library_roots = .libPaths(),
    nonmem_provision = nonmem_provision,
    r_provision = r_prov,
    requires = list(nonmem = require_nonmem, psn = require_psn),
    executables = exes,
    container_image = image,
    nonmem = nonmem,
    license = license,
    r_packages = list(lock_check = if (length(lock)) lock else "clean",
                      required = as.list(pkg_versions)),
    environment_confirmed = TRUE)
  canon <- paste(.env_flatten(probe), collapse = "\n")
  list(
    schema_version = 1L,
    fingerprint = paste0("sha256:", digest::digest(canon, algo = "sha256", serialize = FALSE)),
    nonmem_provision = nonmem_provision,
    r_provision = r_prov,
    environment_confirmed = TRUE,
    probe = probe)
}
