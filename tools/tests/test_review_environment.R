#!/usr/bin/env Rscript

.invoked_as_script <- function() {
  fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  length(fa) > 0L && grepl("test_review_environment\\.R$", sub("^--file=", "", fa[1L]))
}

.run_review_environment_tests <- function() {
  args <- commandArgs(FALSE)
  file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1L])
  tools <- normalizePath(file.path(dirname(file_arg), ".."), mustWork = TRUE)
  repo <- normalizePath(file.path(tools, ".."), mustWork = TRUE)

  passed <- 0L
  failed <- 0L
  ok <- function(label, condition) {
    if (isTRUE(condition)) {
      cat("PASS  ", label, "\n", sep = "")
      passed <<- passed + 1L
    } else {
      cat("FAIL  ", label, "\n", sep = "")
      failed <<- failed + 1L
    }
  }
  error_of <- function(expr) tryCatch({ force(expr); NULL }, error = identity)

  sandbox <- tempfile("review-environment-")
  dir.create(sandbox, recursive = TRUE)
  on.exit(unlink(sandbox, recursive = TRUE, force = TRUE), add = TRUE)

  # I4: structural lock errors must never collapse to a clean result.
  lock_env <- new.env(parent = globalenv())
  sys.source(file.path(tools, "r_lock.R"), envir = lock_env)
  required <- lock_env$harness_r_packages()
  lock_problems <- function(lines) {
    path <- tempfile("r-lock-", tmpdir = sandbox)
    writeLines(lines, path)
    tryCatch(lock_env$verify_r_packages(path),
             error = function(e) paste0("unexpected verifier error: ", conditionMessage(e)))
  }

  empty <- lock_problems(c("# truncated lock", ""))
  ok("I4 empty lock reports every required package",
     length(empty) >= length(required) &&
       all(vapply(required, function(p) any(grepl(paste0("manifest missing: ", p), empty,
                                                 fixed = TRUE)), logical(1))))

  installed_target <- tryCatch(as.character(utils::packageVersion("targets")),
                               error = function(e) "0")
  partial <- lock_problems(paste0("targets: ", installed_target))
  omitted <- setdiff(required, "targets")
  ok("I4 partial lock reports every omitted required package",
     all(vapply(omitted, function(p) any(grepl(paste0("manifest missing: ", p), partial,
                                            fixed = TRUE)), logical(1))))

  duplicate <- lock_problems(c("targets: 1.0", "targets: 1.1"))
  ok("I4 duplicate lock entry refuses clean status",
     any(grepl("duplicate manifest entry: targets", duplicate, fixed = TRUE)))
  malformed <- lock_problems(c("targets 1.0", "yaml:", ": 1.0"))
  ok("I4 malformed and empty lock entries are reported",
     sum(grepl("malformed manifest line", malformed, fixed = TRUE)) == 3L)
  unexpected <- lock_problems(c("roguePackage: 1.0"))
  ok("I4 unexpected lock entry is reported",
     any(grepl("unexpected manifest entry: roguePackage", unexpected, fixed = TRUE)))

  exact_lines <- vapply(required, function(p) {
    version <- tryCatch(as.character(utils::packageVersion(p)), error = function(e) "0")
    paste0(p, ": ", version)
  }, character(1L))
  exact <- lock_problems(exact_lines)
  structural <- "manifest missing|unexpected manifest|duplicate manifest|malformed manifest"
  ok("I4 exact package inventory reaches installed-version disposition",
     !any(grepl(structural, exact)))

  # RC4: shared YAML API and CLI must use yaml's values and schema failures.
  helper <- file.path(tools, "yaml_scalar.R")
  helper_env <- new.env(parent = globalenv())
  helper_source <- error_of(sys.source(helper, envir = helper_env))
  ok("RC4 shared YAML helper sources", is.null(helper_source))

  resolver_adapter <- file.path(tools, "resolve_engine_config.R")
  resolver_adapter_env <- new.env(parent = globalenv())
  resolver_adapter_source <- error_of(sys.source(resolver_adapter,
                                                  envir = resolver_adapter_env))
  ok("SIMPLIFICATION resolver CLI is safe for recursive tool sourcing",
     is.null(resolver_adapter_source) &&
       exists(".run_resolve_engine_config_cli", envir = resolver_adapter_env,
              mode = "function", inherits = FALSE))

  yaml_file <- file.path(sandbox, "values.yaml")
  writeLines(c(
    "engines:",
    "  nonmem:",
    "    provision: \"native\" # reviewed",
    "psn:",
    "  threads: 4",
    "nonmem_image: 'local/nonmem:test' # reviewed image",
    "nonmem_license: \"/site/license file.lic\" # reviewed license"
  ), yaml_file)
  doc <- if (is.null(helper_source)) helper_env$read_reviewed_yaml(yaml_file) else NULL
  ok("RC4 read_reviewed_yaml parses a mapping with comments", is.list(doc))
  provision <- if (is.null(helper_source))
    helper_env$reviewed_yaml_scalar(yaml_file, c("engines", "nonmem", "provision"),
                                    expected = "character") else NULL
  ok("RC4 reviewed_yaml_scalar returns the R-consumed quoted scalar",
     identical(provision, "native"))
  threads <- if (is.null(helper_source))
    helper_env$reviewed_yaml_scalar(yaml_file, c("psn", "threads"),
                                    expected = "number") else NULL
  ok("RC4 reviewed_yaml_scalar preserves numeric scalar type", identical(threads, 4L))

  cli <- function(path, key, type = "character") {
    out <- suppressWarnings(system2("Rscript", c(helper, path, key, type),
                                    stdout = TRUE, stderr = TRUE))
    list(output = out, status = attr(out, "status") %||% 0L)
  }
  cli_image <- cli(yaml_file, "nonmem_image")
  ok("RC4 helper CLI prints one resolved scalar", cli_image$status == 0L &&
       identical(unname(cli_image$output), "local/nonmem:test"))

  missing <- cli(yaml_file, "missing_key")
  ok("RC4 optional missing key has empty success result",
     missing$status == 0L && length(missing$output) == 0L)
  null_file <- file.path(sandbox, "null.yaml")
  writeLines("nonmem_image: null", null_file)
  null <- cli(null_file, "nonmem_image")
  ok("RC4 optional null key has same empty result as missing",
     null$status == 0L && length(null$output) == 0L)

  wrong_file <- file.path(sandbox, "wrong.yaml")
  writeLines(c("nonmem_image:", "  nested: value"), wrong_file)
  wrong <- cli(wrong_file, "nonmem_image")
  ok("RC4 wrong-type key refuses with schema diagnostic", wrong$status != 0L &&
       any(grepl("must be a character scalar", wrong$output, fixed = TRUE)))
  duplicate_file <- file.path(sandbox, "duplicate.yaml")
  writeLines(c("nonmem_image: first", "nonmem_image: second"), duplicate_file)
  duplicate_yaml <- cli(duplicate_file, "nonmem_image")
  ok("RC4 duplicate YAML key refuses", duplicate_yaml$status != 0L &&
       any(grepl("Duplicate map key", duplicate_yaml$output, fixed = TRUE)))
  malformed_file <- file.path(sandbox, "malformed.yaml")
  writeLines("nonmem_image: [unterminated", malformed_file)
  malformed_yaml <- cli(malformed_file, "nonmem_image")
  ok("RC4 malformed YAML refuses", malformed_yaml$status != 0L)
  sequence_file <- file.path(sandbox, "sequence.yaml")
  writeLines("[]", sequence_file)
  sequence_yaml <- error_of(helper_env$read_reviewed_yaml(sequence_file))
  ok("RC4 configuration document root must be a mapping",
     inherits(sequence_yaml, "error") &&
       grepl("document root must be a mapping", conditionMessage(sequence_yaml), fixed = TRUE))

  check_env <- new.env(parent = globalenv())
  sys.source(file.path(tools, "check_environment.R"), envir = check_env)
  sys.source(file.path(tools, "engine_config.R"), envir = check_env)

  # Shared license resolution consumes the reviewed document already parsed by
  # each caller. Environment wins, the typed YAML scalar is next, and an
  # absent value remains explicitly unconfigured. The source file can vanish
  # after parsing without triggering another YAML read.
  license_config <- file.path(sandbox, "license-resolution.yaml")
  writeLines("nonmem_license: '/reviewed/site.lic'", license_config)
  license_document <- helper_env$read_reviewed_yaml(license_config)
  unlink(license_config)
  old_license <- Sys.getenv("NONMEM_LICENSE", unset = NA_character_)
  on.exit(if (is.na(old_license)) Sys.unsetenv("NONMEM_LICENSE") else
            Sys.setenv(NONMEM_LICENSE = old_license), add = TRUE)
  Sys.unsetenv("NONMEM_LICENSE")
  ok("SIMPLIFICATION license resolver uses parsed typed harness without rereading YAML",
     identical(check_env$resolve_nonmem_license(license_document),
               "/reviewed/site.lic"))
  Sys.setenv(NONMEM_LICENSE = "/environment/site.lic")
  ok("SIMPLIFICATION NONMEM_LICENSE precedes parsed harness",
     identical(check_env$resolve_nonmem_license(
       list(nonmem_license = c("invalid", "ignored"))),
       "/environment/site.lic"))
  Sys.unsetenv("NONMEM_LICENSE")
  ok("SIMPLIFICATION absent license remains empty",
     identical(check_env$resolve_nonmem_license(setNames(list(), character())), ""))
  invalid_license <- error_of(check_env$resolve_nonmem_license(
    list(nonmem_license = c("one", "two"))))
  ok("SIMPLIFICATION parsed harness license retains typed scalar refusal",
     inherits(invalid_license, "error") &&
       grepl("nonmem_license must be a character scalar",
             conditionMessage(invalid_license), fixed = TRUE))

  bad_license_root <- file.path(sandbox, "bad-license")
  dir.create(bad_license_root)
  writeLines(c("environment_confirmed: true", "nonmem_license: [one, two]"),
             file.path(bad_license_root, "harness.yaml"))
  bad_license_bin <- file.path(bad_license_root, "bin")
  dir.create(bad_license_bin)
  writeLines(c("#!/bin/sh", "exit 0"), file.path(bad_license_bin, "docker"))
  Sys.chmod(file.path(bad_license_bin, "docker"), "0755")
  previous_wd <- getwd()
  previous_path <- Sys.getenv("PATH")
  setwd(bad_license_root)
  Sys.setenv(PATH = paste(bad_license_bin, previous_path, sep = .Platform$path.sep))
  bad_license <- error_of(check_env$check_environment(
    require_nonmem = FALSE, require_psn = TRUE,
    nonmem_provision = "containerized", pkgs = character(0)))
  Sys.setenv(PATH = previous_path)
  setwd(previous_wd)
  ok("RC4 R preflight refuses a wrong-type license at the same scalar boundary",
     inherits(bad_license, "error") &&
       grepl("nonmem_license must be a character scalar",
             conditionMessage(bad_license), fixed = TRUE))

  # Container show-config is public, requires no Docker, and must agree with R.
  container_root <- file.path(sandbox, "container")
  dir.create(container_root)
  license <- file.path(container_root, "license file.lic")
  writeLines("fixture", license)
  writeLines(c("nonmem_image: 'local/nonmem:test' # reviewed image",
               paste0("nonmem_license: \"", license, "\" # reviewed license")),
             file.path(container_root, "harness.yaml"))
  container_out <- system2("bash", c(file.path(tools, "nonmem_container.sh"),
                                     "--show-config"), stdout = TRUE, stderr = TRUE,
                           env = c(paste0("REPO_ROOT=", container_root),
                                   "NONMEM_IMAGE=", "NONMEM_LICENSE="))
  ok("RC4 container show-config matches R for quotes and comments",
     identical(unname(container_out),
               c("image: local/nonmem:test (harness.yaml)",
                 paste0("license: ", license, " (harness.yaml)"))))

  # Native dispatcher public CLI reads quoted provision from per-run config.
  native_work <- file.path(sandbox, "native work")
  dir.create(native_work)
  writeLines(c("engines:", "  nonmem:",
               "    provision: \"native\" # reviewed"),
             file.path(native_work, "run-config.yaml"))
  native_out <- suppressWarnings(system2("bash", shQuote(c(file.path(tools, "nonmem_exec.sh"),
                                          native_work, "true")),
                                          stdout = TRUE, stderr = TRUE))
  ok("RC4 native dispatcher accepts quoted/commented reviewed provision",
     (attr(native_out, "status") %||% 0L) == 0L)
  snapshots <- list.files(file.path(native_work, "engine-config-used"), full.names = TRUE)
  ok("RC4 native dispatcher preserves config snapshot metadata", length(snapshots) == 1L &&
       any(grepl("# provision: native", readLines(snapshots[1L]), fixed = TRUE)))

  # Simplification compatibility: dispatcher historically works without an
  # engine YAML (environment/default resolution, no snapshot), and treats an
  # explicit empty provision exactly like a missing key.
  compatibility_root <- file.path(sandbox, "dispatcher-compatibility")
  compatibility_tools <- file.path(compatibility_root, "tools")
  compatibility_bin <- file.path(compatibility_root, "bin")
  dir.create(compatibility_tools, recursive = TRUE)
  dir.create(compatibility_bin)
  for (name in c("nonmem_exec.sh", "nonmem_container.sh", "resolved_context.sh",
                 "resolve_engine_config.R", "engine_config.R", "yaml_scalar.R")) {
    file.copy(file.path(tools, name), file.path(compatibility_tools, name))
  }
  Sys.chmod(file.path(compatibility_tools, c("nonmem_exec.sh", "nonmem_container.sh")), "0755")
  writeLines(c("#!/bin/sh", "exit 0"), file.path(compatibility_bin, "fixture-engine"))
  Sys.chmod(file.path(compatibility_bin, "fixture-engine"), "0755")
  compatibility_env <- file.path(compatibility_root, "empty.env")
  writeLines("# fixture", compatibility_env)
  run_compat <- function(work, extra_env = character(), args = c("fixture-engine")) {
    dir.create(work, showWarnings = FALSE)
    out <- suppressWarnings(system2(
      "bash", shQuote(c(file.path(compatibility_tools, "nonmem_exec.sh"), work, args)),
      stdout = TRUE, stderr = TRUE,
      env = c(paste0("PATH=", compatibility_bin, .Platform$path.sep, Sys.getenv("PATH")),
              paste0("TMPDIR=", compatibility_root),
              paste0("NONMEM_ENV_FILE=", compatibility_env), extra_env)))
    list(out = out, status = attr(out, "status") %||% 0L)
  }
  absent_work <- file.path(compatibility_root, "absent-config")
  absent <- run_compat(absent_work, "NONMEM_PROVISION=native")
  ok("SIMPLIFICATION absent engine YAML keeps environment provision and no snapshot",
     absent$status == 0L &&
       !dir.exists(file.path(absent_work, "engine-config-used")))

  empty_work <- file.path(compatibility_root, "empty-provision")
  dir.create(empty_work)
  writeLines(c("engines:", "  nonmem:", "    provision: ''"),
             file.path(empty_work, "run-config.yaml"))
  empty <- run_compat(empty_work, "NONMEM_PROVISION=native")
  ok("SIMPLIFICATION empty config provision behaves like a missing key",
     empty$status == 0L)

  unack_work <- file.path(compatibility_root, "unacknowledged")
  dir.create(unack_work)
  writeLines(c("engines:", "  nonmem:", "    provision: native"),
             file.path(unack_work, "run-config.yaml"))
  unack <- run_compat(unack_work, c("NONMEM_PROVISION=containerized",
                                    "NONMEM_PROVISION_OVERRIDE_ACK="))
  ok("SIMPLIFICATION unacknowledged provision override retains exit 3",
     identical(as.integer(unack$status), 3L))

  unknown_work <- file.path(compatibility_root, "unknown")
  dir.create(unknown_work)
  writeLines(c("engines:", "  nonmem:", "    provision: invented"),
             file.path(unknown_work, "run-config.yaml"))
  unknown <- run_compat(unknown_work, c("NONMEM_PROVISION=",
                                        "NONMEM_PROVISION_OVERRIDE_ACK="))
  ok("SIMPLIFICATION unknown provision retains exit 2",
     identical(as.integer(unknown$status), 2L))

  parafile_work <- file.path(compatibility_root, "missing-parafile")
  dir.create(parafile_work)
  writeLines(c("engines:", "  nonmem:", "    provision: native"),
             file.path(parafile_work, "run-config.yaml"))
  missing_parafile <- run_compat(
    parafile_work, c("NONMEM_PROVISION=", "NONMEM_PROVISION_OVERRIDE_ACK="),
    c("fixture-engine", "-parafile=missing.pnm"))
  ok("SIMPLIFICATION missing argv parafile retains exit 2",
     identical(as.integer(missing_parafile$status), 2L))
  dir.create(file.path(parafile_work, "directory.pnm"))
  directory_parafile <- run_compat(
    parafile_work, c("NONMEM_PROVISION=", "NONMEM_PROVISION_OVERRIDE_ACK="),
    c("fixture-engine", "-parafile=directory.pnm"))
  ok("SIMPLIFICATION directory argv parafile retains regular-file refusal and exit 2",
     identical(as.integer(directory_parafile$status), 2L))

  # P1 follow-up: one invocation must bind resolution, hash, and snapshot body
  # to one immutable read even when the reviewed source is atomically replaced
  # after the immutable capture is created.
  race_root <- file.path(sandbox, "config-race")
  race_bin <- file.path(race_root, "bin")
  race_tmp <- file.path(race_root, "tmp")
  race_work <- file.path(race_root, "race-work")
  dir.create(race_bin, recursive = TRUE)
  dir.create(race_tmp)
  dir.create(race_work)
  race_source <- file.path(race_work, "run-config.yaml")
  race_initial <- file.path(race_root, "initial.yaml")
  race_replacement <- file.path(race_root, "replacement.yaml")
  writeLines(c("engines:", "  nonmem:", "    provision: native",
               "psn:", "  threads: 2", "  nodes: 1",
               rep("# capture-race padding", 100000L)), race_initial)
  writeLines(c("engines:", "  nonmem:", "    provision: containerized",
               "psn:", "  threads: 9", "  nodes: 7"), race_replacement)
  file.copy(race_initial, race_source)
  real_rscript <- Sys.which("Rscript")
  writeLines(c(
    "#!/usr/bin/env bash",
    "set -euo pipefail",
    "\"$PMX_REAL_RSCRIPT\" \"$@\" & pid=$!",
    "while kill -0 \"$pid\" 2>/dev/null; do",
    "  capture=$(find \"$TMPDIR\" -maxdepth 1 -type f -name \"$PMX_CAPTURE_GLOB\" -print -quit)",
    "  if [ -n \"$capture\" ]; then",
    "    mode=$(stat -c %a \"$capture\")",
    "    [ -z \"${PMX_CAPTURE_MODE_FILE:-}\" ] || printf '%s\\n' \"$mode\" >> \"$PMX_CAPTURE_MODE_FILE\"",
    "    [ \"$mode\" = 400 ] || continue",
    "    if [ \"$PMX_RACE_MODE\" = source ]; then",
    "      cp \"$PMX_RACE_REPLACEMENT\" \"$PMX_RACE_SOURCE\"",
    "    else",
    "      sleep 0.02; chmod 0600 \"$capture\"; cp \"$PMX_RACE_REPLACEMENT\" \"$capture\"",
    "    fi",
    "    touch \"$PMX_RACE_MARKER\"; break",
    "  fi",
    "done",
    "set +e; wait \"$pid\"; status=$?",
    "exit \"$status\""
  ), file.path(race_bin, "Rscript"))
  writeLines(c(
    "#!/usr/bin/env bash",
    "set -euo pipefail",
    "printf '%s\\n' \"$@\" > \"$PMX_ENGINE_ARGV\"",
    "find \"$TMPDIR\" -mindepth 1 -maxdepth 1 -name 'pmx-*' | wc -l > \"$PMX_CAPTURE_COUNT\"",
    "exit 0"
  ), file.path(race_bin, "fake-engine"))
  Sys.chmod(file.path(race_bin, c("Rscript", "fake-engine")), "0755")
  race_marker <- file.path(race_root, "mutated")
  race_argv <- file.path(race_root, "argv")
  race_capture_count <- file.path(race_root, "capture-count")
  race_out <- suppressWarnings(system2(
    "bash",
    shQuote(c(file.path(tools, "nonmem_exec.sh"), race_work,
              "fake-engine", "-run", "value with space")),
    stdout = TRUE, stderr = TRUE,
    env = c(paste0("PATH=", race_bin, .Platform$path.sep, Sys.getenv("PATH")),
            paste0("TMPDIR=", race_tmp),
            paste0("PMX_REAL_RSCRIPT=", real_rscript),
            "PMX_RACE_MODE=source",
            "PMX_CAPTURE_GLOB=pmx-engine-config.*",
            paste0("PMX_RACE_MARKER=", race_marker),
            paste0("PMX_RACE_SOURCE=", race_source),
            paste0("PMX_RACE_REPLACEMENT=", race_replacement),
            paste0("PMX_ENGINE_ARGV=", race_argv),
            paste0("PMX_CAPTURE_MODE_FILE=", file.path(race_root, "capture-mode")),
            paste0("PMX_CAPTURE_COUNT=", race_capture_count))))
  race_status <- attr(race_out, "status") %||% 0L
  race_snapshots <- list.files(file.path(race_work, "engine-config-used"),
                               full.names = TRUE)
  race_snapshot <- if (length(race_snapshots) == 1L) race_snapshots[[1L]] else ""
  race_lines <- if (nzchar(race_snapshot)) readLines(race_snapshot) else character(0)
  race_yaml <- if (nzchar(race_snapshot))
    tryCatch(yaml::read_yaml(race_snapshot), error = identity) else NULL
  initial_hash <- digest::digest(file = race_initial, algo = "sha256", serialize = FALSE)
  ok("P1 dispatcher binds provision, compute fields, body, and hash to one config capture",
     race_status == 0L && length(race_snapshots) == 1L &&
       any(race_lines == "# provision: native") &&
       any(race_lines == paste0("# config_source: ", race_source)) &&
       any(race_lines == paste0("# config_sha256: ", initial_hash)) &&
       any(grepl("# compute_applied: threads=2 nodes=1", race_lines, fixed = TRUE)) &&
       is.list(race_yaml) && identical(race_yaml$engines$nonmem$provision, "native") &&
       identical(race_yaml$psn$threads, 2L))
  ok("P1 mutation occurred on original engine config after capture",
     file.exists(race_marker) &&
       identical(yaml::read_yaml(race_source)$engines$nonmem$provision,
                 "containerized"))
  ok("P1 configuration capture is owner-only from creation",
     all(readLines(file.path(race_root, "capture-mode")) %in% c("400", "600")) &&
       tail(readLines(file.path(race_root, "capture-mode")), 1L) == "400")
  ok("P1 dispatcher preserves exact argv and removes capture before engine exec",
     identical(readLines(race_argv), c("-run", "value with space")) &&
       identical(trimws(readLines(race_capture_count)), "0"))

  # A parser process must not be able to modify the captured evidence bytes.
  # The reviewer reproduction targets its argv[2], which is the original file
  # before this fix and the capture after it. Such an attempted write must
  # refuse the invocation rather than produce an internally mixed snapshot.
  capture_race <- file.path(sandbox, "capture-race")
  capture_bin <- file.path(capture_race, "bin")
  capture_tmp <- file.path(capture_race, "tmp")
  capture_work <- file.path(capture_race, "work")
  dir.create(capture_bin, recursive = TRUE)
  dir.create(capture_tmp)
  dir.create(capture_work)
  capture_source <- file.path(capture_work, "run-config.yaml")
  file.copy(race_initial, capture_source)
  file.copy(file.path(race_bin, "Rscript"), file.path(capture_bin, "Rscript"))
  capture_replacement <- file.path(capture_race, "replacement.yaml")
  writeLines(c("engines:", "  nonmem:", "    provision: native",
               "psn:", "  threads: 9", "  nodes: 1"), capture_replacement)
  capture_engine_marker <- file.path(capture_race, "engine-ran")
  writeLines(c("#!/usr/bin/env bash", "touch \"$PMX_ENGINE_MARKER\"", "exit 0"),
             file.path(capture_bin, "fake-engine"))
  Sys.chmod(file.path(capture_bin, c("Rscript", "fake-engine")), "0755")
  capture_out <- suppressWarnings(system2(
    "bash", shQuote(c(file.path(tools, "nonmem_exec.sh"), capture_work,
                       "fake-engine", "-run")),
    stdout = TRUE, stderr = TRUE,
    env = c(paste0("PATH=", capture_bin, .Platform$path.sep, Sys.getenv("PATH")),
            paste0("TMPDIR=", capture_tmp),
            paste0("PMX_REAL_RSCRIPT=", real_rscript),
            "PMX_RACE_MODE=capture",
            "PMX_CAPTURE_GLOB=pmx-engine-config.*",
            paste0("PMX_RACE_MARKER=", file.path(capture_race, "mutated")),
            paste0("PMX_RACE_SOURCE=", capture_source),
            paste0("PMX_RACE_REPLACEMENT=", capture_replacement),
            paste0("PMX_ENGINE_MARKER=", capture_engine_marker))))
  ok("P1 direct mutation of captured bytes refuses before engine and snapshot",
     identical(as.integer(attr(capture_out, "status") %||% 0L), 4L) &&
       any(grepl("captured engine configuration changed", capture_out, fixed = TRUE)) &&
       !file.exists(capture_engine_marker) &&
       length(list.files(file.path(capture_work, "engine-config-used"),
                         full.names = TRUE)) == 0L)
  ok("P1 failed capture-mutation path removes temporary config copy",
     length(list.files(capture_tmp, pattern = "^pmx-(engine|resolved)-config\\.")) == 0L)

  # Container wrapper must likewise derive image and license from one captured
  # harness.yaml, never from two independently mutable source reads.
  harness_race <- file.path(sandbox, "harness-race")
  harness_tmp <- file.path(harness_race, "tmp")
  dir.create(harness_race)
  dir.create(harness_tmp)
  license_initial <- file.path(harness_race, "initial license.lic")
  license_replacement <- file.path(harness_race, "replacement.lic")
  writeLines("fixture", license_initial)
  writeLines("fixture", license_replacement)
  harness_source <- file.path(harness_race, "harness.yaml")
  harness_replacement <- file.path(harness_race, "replacement.yaml")
  writeLines(c("nonmem_image: 'local/nonmem:initial'",
               paste0("nonmem_license: '", license_initial, "'")), harness_source)
  writeLines(c("nonmem_image: 'local/nonmem:replacement'",
               paste0("nonmem_license: '", license_replacement, "'")), harness_replacement)
  harness_marker <- file.path(harness_race, "mutated")
  harness_out <- suppressWarnings(system2(
    "bash", c(file.path(tools, "nonmem_container.sh"), "--show-config"),
    stdout = TRUE, stderr = TRUE,
    env = c(paste0("PATH=", race_bin, .Platform$path.sep, Sys.getenv("PATH")),
            paste0("TMPDIR=", harness_tmp),
            paste0("REPO_ROOT=", harness_race),
            paste0("PMX_REAL_RSCRIPT=", real_rscript),
            "PMX_RACE_MODE=source",
            "PMX_CAPTURE_GLOB=pmx-harness-config.*",
            paste0("PMX_RACE_MARKER=", harness_marker),
            paste0("PMX_RACE_SOURCE=", harness_source),
            paste0("PMX_RACE_REPLACEMENT=", harness_replacement),
            "NONMEM_IMAGE=", "NONMEM_LICENSE=")))
  ok("P1 container image and license resolve from one harness capture",
     (attr(harness_out, "status") %||% 0L) == 0L &&
       identical(unname(harness_out),
                 c("image: local/nonmem:initial (harness.yaml)",
                   paste0("license: ", license_initial, " (harness.yaml)"))) &&
       file.exists(harness_marker))
  ok("P1 container show-config removes its harness capture",
     length(list.files(harness_tmp, pattern = "^pmx-(harness|resolved)-config\\.")) == 0L)

  # RC5: public preflight must use subprocess status and retain diagnostics.
  psn_root <- file.path(sandbox, "psn")
  dir.create(psn_root)
  writeLines("environment_confirmed: true", file.path(psn_root, "harness.yaml"))
  bin <- file.path(psn_root, "bin")
  dir.create(bin)
  execute <- file.path(bin, "execute")
  old_path <- Sys.getenv("PATH")
  old_wd <- getwd()
  on.exit({ Sys.setenv(PATH = old_path); setwd(old_wd) }, add = TRUE)
  Sys.setenv(PATH = paste(bin, old_path, sep = .Platform$path.sep))
  setwd(psn_root)

  writeLines(c("#!/bin/sh", "echo 'fixture PsN broken' >&2", "exit 42"), execute)
  Sys.chmod(execute, "0755")
  failed_psn <- error_of(check_env$check_environment(
    require_nonmem = FALSE, require_psn = TRUE,
    nonmem_provision = "native", pkgs = character(0)))
  failed_msg <- if (inherits(failed_psn, "error")) conditionMessage(failed_psn) else ""
  ok("RC5 exit 42 cannot yield status ok", inherits(failed_psn, "error"))
  ok("RC5 failed PsN preflight retains exit status and diagnostic",
     grepl("status 42", failed_msg, fixed = TRUE) &&
       grepl("fixture PsN broken", failed_msg, fixed = TRUE))

  writeLines(c("#!/bin/sh", "echo 'PsN version 5.7.1'", "exit 0"), execute)
  Sys.chmod(execute, "0755")
  successful_psn <- error_of(check_env$check_environment(
    require_nonmem = FALSE, require_psn = TRUE,
    nonmem_provision = "native", pkgs = character(0)))
  ok("RC5 ordinary successful PsN version probe remains OK", is.null(successful_psn))

  cat(sprintf("review environment tests: %d passed, %d failed\n", passed, failed))
  invisible(failed)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

if (.invoked_as_script()) {
  quit(status = if (.run_review_environment_tests() == 0L) 0L else 1L)
}
