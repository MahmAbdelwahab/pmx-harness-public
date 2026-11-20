# R package manifest for the site (regulated) R provision.
#
# In a regulated environment the system-wide R library is validated and
# authoritative — the harness must never install into or shadow it (the
# same ownership principle as the site NONMEM/PsN install and psn.conf).
# The harness therefore VERIFIES, and only verifies:
#
#   config/r-packages.lock   committed manifest: package, version,
#                            written deliberately from a validated dev
#                            machine (write_r_packages_lock()).
#   verify_r_packages()      compares the installed library against the
#                            manifest and returns every mismatch;
#                            called by check_environment() — under
#                            r_provision "site" a mismatch is a
#                            reported problem for SME disposition,
#                            never an auto-fix.
#
# The provision itself is a machine fact: `r_provision: managed|site`
# in harness.yaml (see harness.yaml.example).

R_PACKAGES_LOCK <- "config/r-packages.lock"

if (!exists("reviewed_yaml_scalar", mode = "function") &&
    file.exists("tools/yaml_scalar.R")) {
  source("tools/yaml_scalar.R", local = TRUE)
}

# The package set the harness requires — the union of
# setup_environment.R's install set and the engine packages from
# config/engine.yaml. Kept as a function so the lock and the verifier
# can never drift apart on WHAT to check.
harness_r_packages <- function() {
  c("targets", "here", "yaml",                        # pipeline
    "yspec", "mrgda", "lastdose", "mrgmisc",          # data assembly
    "pmtables", "pmplots", "mrggsave",                # EDA / diagnostics
    "bbr", "readr",                                   # NONMEM lane (readr pinned for bbr)
    "rxode2", "nlmixr2", "nlmixr2est",                # nlmixr2 engine
    "mrgsolve",                                       # simulation engine
    "tarchetypes", "quarto")                          # reporting
}

# Write the manifest from the CURRENT machine's installed versions.
# Run deliberately, on a development machine whose environment the SME
# considers the reference — regenerating casually defeats the purpose.
write_r_packages_lock <- function(path = R_PACKAGES_LOCK) {
  pkgs <- harness_r_packages()
  ver <- vapply(pkgs, function(p) {
    tryCatch(as.character(utils::packageVersion(p)),
             error = function(e) NA_character_)
  }, character(1))
  missing <- pkgs[is.na(ver)]
  if (length(missing)) {
    stop("Cannot write the manifest — packages not installed here: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  lines <- c(
    "# R package manifest (tools/r_lock.R::write_r_packages_lock).",
    paste0("# Written ", format(Sys.time(), "%Y-%m-%d %H:%M UTC", tz = "UTC"),
           " under R ", getRversion(), "."),
    "# Under r_provision 'site' the environment check verifies the",
    "# validated system library against these versions and reports",
    "# every mismatch for SME disposition. Never edited by hand.",
    paste0(pkgs, ": ", ver))
  writeLines(lines, path)
  invisible(path)
}

# Compare installed packages against the manifest. Returns a character
# vector of human-readable mismatch descriptions (empty = clean).
verify_r_packages <- function(path = R_PACKAGES_LOCK) {
  if (!file.exists(path)) {
    return(paste0("R package manifest not found: ", path,
                  " (write it on the reference machine with ",
                  "tools/r_lock.R::write_r_packages_lock())."))
  }
  lines <- readLines(path, warn = FALSE)
  parsed_names <- character(0)
  parsed_versions <- character(0)
  parsed_lines <- integer(0)
  out <- character(0)
  for (i in seq_along(lines)) {
    line <- trimws(lines[[i]])
    if (!nzchar(line) || startsWith(line, "#")) next
    fields <- strsplit(line, ":", fixed = TRUE)[[1L]]
    valid <- length(fields) == 2L
    name <- if (valid) trimws(fields[[1L]]) else ""
    version <- if (valid) trimws(fields[[2L]]) else ""
    valid <- valid && grepl("^[A-Za-z][A-Za-z0-9.]*$", name) &&
      nzchar(version) && !grepl("[[:space:]#]", version) &&
      !inherits(tryCatch(numeric_version(version, strict = TRUE), error = identity), "error")
    if (!valid) {
      out <- c(out, paste0("malformed manifest line ", i, ": ", line))
      next
    }
    parsed_names <- c(parsed_names, name)
    parsed_versions <- c(parsed_versions, version)
    parsed_lines <- c(parsed_lines, i)
  }

  duplicate_names <- unique(parsed_names[duplicated(parsed_names)])
  for (p in duplicate_names) {
    out <- c(out, paste0("duplicate manifest entry: ", p, " (lines ",
                         paste(parsed_lines[parsed_names == p], collapse = ", "), ")"))
  }
  keep <- !duplicated(parsed_names)
  want <- stats::setNames(parsed_versions[keep], parsed_names[keep])
  required <- harness_r_packages()
  missing <- setdiff(required, names(want))
  unexpected <- setdiff(names(want), required)
  if (length(missing)) out <- c(out, paste0("manifest missing: ", missing))
  if (length(unexpected)) {
    out <- c(out, paste0("unexpected manifest entry: ", unexpected))
  }

  comparable <- intersect(required, names(want))
  comparable <- setdiff(comparable, duplicate_names)
  for (p in comparable) {
    have <- tryCatch(as.character(utils::packageVersion(p)),
                     error = function(e) NA_character_)
    if (is.na(have)) {
      out <- c(out, paste0("package missing: ", p,
                           " (manifest ", want[[p]], ")"))
    } else if (!identical(have, want[[p]])) {
      out <- c(out, paste0("version mismatch: ", p, " installed ", have,
                           ", manifest ", want[[p]]))
    }
  }
  out
}

# Per-run capture: what versions ACTUALLY governed an execution — the R
# analogue of the engine-config-used/ snapshots (reference = the
# project-level lock; this is the run-level evidence). Writes
# <workdir>/engine-config-used/<UTC-timestamp>-r-packages.yaml with the
# installed version of every harness package, the R version, and the
# verification result against the lock at that instant.
write_r_packages_used <- function(workdir) {
  stopifnot(dir.exists(workdir))
  dir_ <- file.path(workdir, "engine-config-used")
  dir.create(dir_, showWarnings = FALSE)
  ts <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
  out <- file.path(dir_, paste0(ts, "-r-packages.yaml"))
  pkgs <- harness_r_packages()
  ver <- vapply(pkgs, function(p) {
    tryCatch(as.character(utils::packageVersion(p)),
             error = function(e) "MISSING")
  }, character(1))
  mism <- verify_r_packages()
  writeLines(c(
    "# R packages in effect at execution (written by write_r_packages_used)",
    paste0("# invoked_at_utc: ", ts),
    paste0("# r_version: ", getRversion()),
    paste0("# r_provision: ", r_provision()),
    paste0("# lock_check: ", if (length(mism) == 0) "clean"
           else paste0(length(mism), " deviation(s) — listed below")),
    if (length(mism)) paste0("#   ", mism),
    "#",
    paste0(pkgs, ": ", ver)), out)
  invisible(out)
}

# Resolve the R provision for this machine: harness.yaml r_provision,
# default "managed".
r_provision <- function(harness_yaml = "harness.yaml") {
  if (!file.exists(harness_yaml)) return("managed")
  prov <- reviewed_yaml_scalar(harness_yaml, "r_provision", expected = "character") %||%
    "managed"
  match.arg(prov, c("managed", "site"))
}

`%||%` <- function(a, b) if (is.null(a)) b else a
