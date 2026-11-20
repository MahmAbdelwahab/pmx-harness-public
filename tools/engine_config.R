# Engine configuration for the NONMEM and PsN estimation tools.
#
# Three artifacts, three lifecycles:
#   config/engine.yaml            committed house default (operational).
#   <run>/run-config.yaml         PER-RUN config: authored by the
#                                 sub-agent BEFORE execution (seeded from
#                                 the house default), reviewed and
#                                 possibly edited by the SME at the gate.
#                                 Execution obeys this file when present —
#                                 tools/nonmem_exec.sh applies the same
#                                 precedence for provision and snapshots.
#   <run>/engine-config-used/     written AT execution: verbatim snapshot
#                                 of whichever config actually ran, with
#                                 argv, provision, and sha256 — the audit
#                                 proof that executed == reviewed.
#
# load_engine_config(workdir) returns the parsed config the run must
# use: workdir/run-config.yaml when present, else config/engine.yaml.
# The returned list carries attr "config_source" naming the file, so run
# records and gate packages can state which config governed.
`%||%` <- function(a, b) if (is.null(a)) b else a

if (!exists("reviewed_yaml_scalar", mode = "function") &&
    file.exists("tools/yaml_scalar.R")) {
  source("tools/yaml_scalar.R", local = TRUE)
}

# resolve_nonmem_home(harness) — the site NONMEM install root for the
# NATIVE provision (bbr/bbi lane only; PsN finds NONMEM through its own
# psn.conf). bbi executes <nonmem_home>/run/<executable>, so this is the
# value skills/nonmem-estimation/reference/bbr_estimation.R takes as its
# `nonmem_home =` argument.
#
# This is a MACHINE-LOCAL fact, so it lives in harness.yaml (git-ignored),
# never in config/engine.yaml. Resolution order mirrors the license
# resolution in tools/check_environment.R and tools/nonmem_container.sh
# exactly: NONMEM_HOME (environment) first, then the nonmem_home: key in
# harness.yaml. Returns NULL when neither is set — no path is ever
# invented and the filesystem is never searched for a NONMEM install; an
# unconfigured home is reported as such by the environment check.
resolve_nonmem_home <- function(harness = "harness.yaml") {
  home <- Sys.getenv("NONMEM_HOME", "")
  if (!nzchar(home) && file.exists(harness)) {
    value <- reviewed_yaml_scalar(harness, "nonmem_home", expected = "character")
    home <- as.character(value %||% "")
  }
  if (nzchar(home)) home else NULL
}

# resolve_nonmem_image(harness) — the local container image for the
# CONTAINERIZED provision. Same order as the license and the home:
# NONMEM_IMAGE (environment), then the nonmem_image: key in harness.yaml,
# then the committed default. tools/nonmem_container.sh applies the
# identical order in shell.
resolve_nonmem_image <- function(harness = "harness.yaml",
                                 default = "pmx-harness/nonmem:7.5.1") {
  img <- Sys.getenv("NONMEM_IMAGE", "")
  if (!nzchar(img) && file.exists(harness)) {
    value <- reviewed_yaml_scalar(harness, "nonmem_image", expected = "character")
    img <- as.character(value %||% "")
  }
  if (nzchar(img)) img else default
}

# resolve_nonmem_license(harness) — the NONMEM license path for containerized
# execution. `harness` is the reviewed document already parsed by the caller;
# this resolver never reads harness.yaml itself. An environment value takes
# precedence over the typed nonmem_license scalar. Empty means unconfigured.
resolve_nonmem_license <- function(harness = NULL) {
  license <- Sys.getenv("NONMEM_LICENSE", "")
  if (nzchar(license)) return(license)
  if (is.null(harness)) return("")

  value <- reviewed_yaml_scalar("harness.yaml", "nonmem_license",
                                expected = "character", document = harness)
  as.character(value %||% "")
}

load_engine_config <- function(workdir = NULL,
                               default = "config/engine.yaml") {
  path <- default
  if (!is.null(workdir)) {
    per_run <- file.path(workdir, "run-config.yaml")
    if (file.exists(per_run)) path <- per_run
  }
  if (!file.exists(path)) {
    stop("Engine config not found: ", path, call. = FALSE)
  }
  cfg <- yaml::read_yaml(path)
  attr(cfg, "config_source") <- normalizePath(path)
  # Compute settings are checked HERE, before any lane provisions a
  # workspace or calls an executor: a parafile that does not exist or a
  # bbi nodes/threads disagreement must stop the run before nmfe starts.
  validate_compute_config(cfg, workdir = workdir)
  cfg
}

# Harness default for the bbi-lane `nodes` key. config/engine.yaml has no
# bbi.args.nodes key (the SME owns that file's values); the draft seeds
# this value and names it as the default.
.bbi_nodes_default <- 1L

# validate_compute_config(cfg, workdir) — per-run compute consistency
# (SME request 2026-09-03). Stops, never warns, on:
#
#   * a parafile named in bbi.args or psn but absent on disk. Relative
#     paths resolve against the run folder (PsN: -parafile= is resolved
#     against the workdir; bbi: --parafile against the model directory,
#     which is the run folder the config was drafted in). The message
#     carries the resolved path.
#   * bbi.args.nodes > 1 while parallel is false — bbi only runs NONMEM
#     under MPI with --parallel, so the request could not take effect.
#   * bbi.args.parallel true with nodes != threads. bbi v3.4.0 has no
#     --nodes flag; `bbi nonmem run local --help` documents --threads as
#     "number of threads to execute with locally or nodes to execute on
#     in parallel". The harness therefore requires the two to agree so
#     the value bbi applies is the value the SME approved.
#   * bbi.args.parallel true with a parafile whose NODES= line disagrees
#     with nodes. Observed with bbi v3.4.0 --preview: bbi copies the
#     parafile into the run directory as <run>.pnm unchanged (NODES= is
#     not rewritten) and passes -parafile=<run>.pnm to nmfe; whether
#     --threads overrides NODES= at a real run is UNVERIFIED, so both
#     channels must carry the same number.
#
# Returns cfg invisibly.
validate_compute_config <- function(cfg, workdir = NULL) {
  base <- if (is.null(workdir)) getwd() else workdir
  resolve <- function(p) if (grepl("^/", p)) p else file.path(base, p)

  b <- cfg$bbi$args
  if (!is.null(b)) {
    threads  <- as.integer(b$threads %||% 1L)
    nodes    <- as.integer(b$nodes %||% .bbi_nodes_default)
    parallel <- isTRUE(b$parallel)
    parafile <- b$parafile
    if (!is.null(parafile) && !nzchar(parafile)) parafile <- NULL
    if (!is.null(parafile)) {
      pf <- resolve(parafile)
      if (!file.exists(pf)) {
        stop("bbi.args.parafile names a file that does not exist: ", pf,
             " (resolved against the run folder). Provide the parafile ",
             "or set parafile: null before the run starts.", call. = FALSE)
      }
    }
    if (nodes > 1L && !parallel) {
      stop("bbi.args.nodes = ", nodes, " but bbi.args.parallel is false: ",
           "bbi runs NONMEM under MPI only with parallel: true, so the ",
           "node count could not take effect. Set parallel: true (and a ",
           "parafile) or nodes: 1.", call. = FALSE)
    }
    if (parallel) {
      if (is.null(parafile)) {
        stop("bbi.args.parallel is true but no parafile is named; NONMEM ",
             "MPI requires one (e.g. mpilinux8.pnm).", call. = FALSE)
      }
      if (nodes != threads) {
        stop("bbi.args.nodes = ", nodes, " but bbi.args.threads = ", threads,
             ": bbi has no separate nodes flag — with parallel: true its ",
             "--threads value is the MPI node count (bbi nonmem run local ",
             "--help). Set threads and nodes to the same value.", call. = FALSE)
      }
      pnm <- readLines(resolve(parafile), warn = FALSE)
      m <- regmatches(pnm, regexpr("NODES\\s*=\\s*[0-9]+", pnm, ignore.case = TRUE))
      if (length(m)) {
        pn <- as.integer(sub(".*=\\s*", "", m[1]))
        if (!identical(pn, nodes)) {
          stop("bbi.args.nodes = ", nodes, " but the parafile ", parafile,
               " declares ", gsub("\\s", "", m[1]), ". bbi copies the parafile ",
               "into the run directory unchanged, so the two must agree.",
               call. = FALSE)
        }
      }
    }
  }

  p <- cfg$psn
  if (!is.null(p)) {
    parafile <- p$parafile
    if (!is.null(parafile) && !nzchar(parafile)) parafile <- NULL
    if (!is.null(parafile)) {
      pf <- resolve(parafile)
      if (!file.exists(pf)) {
        stop("psn.parafile names a file that does not exist: ", pf,
             " (resolved against the run folder, as PsN -parafile= is). ",
             "Provide the parafile or set parafile: null before the run ",
             "starts.", call. = FALSE)
      }
    }
    nodes <- as.integer(p$nodes %||% 1L)
    if (nodes > 1L && is.null(parafile)) {
      stop("psn.nodes = ", nodes, " requires psn.parafile (the NONMEM MPI ",
           "parafile, e.g. mpilinux8.pnm).", call. = FALSE)
    }
  }
  invisible(cfg)
}

# Resolve one NONMEM/PsN invocation from immutable source captures. This is the
# canonical boundary shared by the dispatcher and direct container wrapper.
# Shell remains responsible only for launching the resolved process.
resolve_dispatcher_config <- function(workdir, engine_config = NULL,
                                      harness = NULL, invocation_id,
                                      tool, args = character(),
                                      env = as.list(Sys.getenv(c(
                                        "NONMEM_PROVISION",
                                        "NONMEM_PROVISION_OVERRIDE_ACK",
                                        "NONMEM_IMAGE", "NONMEM_LICENSE",
                                        "NONMEM_ENV_FILE")))) {
  safe_scalar <- function(value, label) {
    if (length(value) != 1L || is.na(value) || grepl("[\r\n\t]", value)) {
      stop(label, " contains an unsupported scalar representation.", call. = FALSE)
    }
    value
  }
  workdir <- safe_scalar(as.character(workdir), "workdir")
  if (!dir.exists(workdir)) stop("Workdir does not exist: ", workdir, call. = FALSE)
  workdir <- normalizePath(workdir, mustWork = TRUE)
  if (!is.character(tool) || length(tool) != 1L || !nzchar(tool)) {
    stop("Invocation tool must be one non-empty string.", call. = FALSE)
  }
  tool <- safe_scalar(tool, "tool")
  if (!is.character(args) || anyNA(args)) {
    stop("Invocation arguments must be character strings.", call. = FALSE)
  }
  args <- vapply(seq_along(args), function(i) safe_scalar(args[[i]],
                                                          paste0("argument ", i)),
                 character(1L), USE.NAMES = FALSE)
  if (!is.null(engine_config)) engine_config <- safe_scalar(engine_config, "engine config path")
  if (!is.null(harness)) harness <- safe_scalar(harness, "harness path")
  env_value <- function(name) as.character(env[[name]] %||% "")
  capture_paths <- character()
  on.exit(unlink(capture_paths, force = TRUE), add = TRUE)
  capture <- function(path, prefix, required = FALSE) {
    if (is.null(path) || !nzchar(path) || !file.exists(path)) {
      if (required) stop("Engine config not found: ", path, call. = FALSE)
      return(NULL)
    }
    source <- normalizePath(path, mustWork = TRUE)
    copy <- tempfile(prefix, tmpdir = Sys.getenv("TMPDIR", tempdir()))
    capture_paths <<- c(capture_paths, copy)
    if (!file.create(copy)) stop("Unable to reserve configuration capture.", call. = FALSE)
    Sys.chmod(copy, "0600")
    if (!file.copy(source, copy, overwrite = TRUE, copy.mode = FALSE)) {
      stop("Unable to capture configuration: ", source, call. = FALSE)
    }
    bytes <- readBin(copy, "raw", n = file.info(copy)$size)
    sha256 <- digest::digest(bytes, algo = "sha256", serialize = FALSE)
    Sys.chmod(copy, "0400")
    list(source = source, capture = copy, bytes = bytes,
         sha256 = sha256)
  }
  engine <- capture(engine_config, "pmx-engine-config.", required = !is.null(engine_config))
  machine <- capture(harness, "pmx-harness-config.")
  parse_capture <- function(item) {
    if (is.null(item)) return(setNames(list(), character()))
    tryCatch(read_reviewed_yaml(item$capture), error = function(e) {
      stop(sub(item$capture, item$source, conditionMessage(e), fixed = TRUE),
           call. = FALSE)
    })
  }
  engine_doc <- parse_capture(engine)
  harness_doc <- parse_capture(machine)
  scalar <- function(item, document, key, expected = "character") {
    if (is.null(item)) return(NULL)
    reviewed_yaml_scalar(item$source, key, expected = expected, document = document)
  }

  config_provision <- scalar(engine, engine_doc, "engines.nonmem.provision")
  if (!is.null(config_provision) && !nzchar(config_provision)) config_provision <- NULL
  if (!is.null(config_provision))
    config_provision <- safe_scalar(config_provision, "engines.nonmem.provision")
  provision_env <- safe_scalar(env_value("NONMEM_PROVISION"), "NONMEM_PROVISION")
  provision <- config_provision %||% "containerized"
  provision_source <- if (is.null(config_provision))
    "default (no config provision)" else "config"
  displaced <- ""
  if (nzchar(provision_env)) {
    if (!is.null(config_provision) && !identical(provision_env, config_provision)) {
      if (!identical(env_value("NONMEM_PROVISION_OVERRIDE_ACK"), "1")) {
        stop("ERROR: NONMEM_PROVISION='", provision_env,
             "' differs from the reviewed config provision ('", config_provision,
             "' in ", engine$source, "). Set NONMEM_PROVISION_OVERRIDE_ACK=1 ",
             "to proceed with an explicitly recorded override.", call. = FALSE)
      }
      displaced <- config_provision
      provision_source <- "env-override-acknowledged"
    } else {
      provision_source <- if (is.null(config_provision))
        "env (no config provision present)" else "env (equals config)"
    }
    provision <- provision_env
  }
  if (!provision %in% c("containerized", "native")) {
    stop("Unknown NONMEM provision mode: ", provision,
         " (containerized|native)", call. = FALSE)
  }

  harness_value <- function(key) scalar(machine, harness_doc, key)
  resolve_value <- function(env_name, harness_key, default = "") {
    value <- safe_scalar(env_value(env_name), env_name)
    if (nzchar(value)) return(list(value = value, source = env_name))
    value <- safe_scalar(harness_value(harness_key) %||% "", harness_key)
    if (nzchar(value)) return(list(value = value, source = "harness.yaml"))
    list(value = default, source = if (nzchar(default)) "default" else "unset")
  }
  image <- resolve_value("NONMEM_IMAGE", "nonmem_image",
                         "pmx-harness/nonmem:7.5.1")
  license <- resolve_value("NONMEM_LICENSE", "nonmem_license")
  env_file <- resolve_value("NONMEM_ENV_FILE", "nonmem_env_file")

  compute_value <- function(key, expected = "character", default = "") {
    value <- scalar(engine, engine_doc, paste0("psn.", key), expected)
    safe_scalar(if (is.null(value)) default else as.character(value), paste0("psn.", key))
  }
  compute <- list(
    threads = compute_value("threads", "number", "1"),
    nodes = compute_value("nodes", "number", "1"),
    parafile = compute_value("parafile"), submit = compute_value("submit", default = "local"),
    queue = compute_value("queue"), account = compute_value("account"), argv = character())
  for (arg in args) {
    if (startsWith(arg, "-threads=")) compute$threads <- sub("^-threads=", "", arg)
    else if (startsWith(arg, "-nodes=")) compute$nodes <- sub("^-nodes=", "", arg)
    else if (startsWith(arg, "-parafile=")) compute$parafile <- sub("^-parafile=", "", arg)
    else if (startsWith(arg, "-run_on_")) compute$submit <- sub("^-run_on_", "", arg)
    else if (grepl("^-(sge_queue|slurm_partition|torque_queue)=", arg))
      compute$queue <- sub("^[^=]*=", "", arg)
    else if (startsWith(arg, "-slurm_account="))
      compute$account <- sub("^[^=]*=", "", arg)
    else next
    compute$argv <- c(compute$argv, arg)
  }
  compute$note <- ""
  if (identical(tool, "sir")) {
    compute$nodes <- "1"
    compute$parafile <- ""
    compute$note <- paste("sir prohibits MPI NONMEM (sir user guide);",
                          "nodes/parafile from the config were NOT applied")
  }
  for (arg in args[startsWith(args, "-parafile=")]) {
    parafile <- sub("^-parafile=", "", arg)
    path <- if (startsWith(parafile, "/")) parafile else file.path(workdir, parafile)
    if (!file_test("-f", path)) {
      stop("ERROR: parafile named in the arguments does not exist: ", path,
           " — stopping before the tool starts.", call. = FALSE)
    }
  }
  if (!is.null(engine)) validate_compute_config(engine_doc, workdir = workdir)

  for (item in Filter(Negate(is.null), list(engine, machine))) {
    current <- readBin(item$capture, "raw", n = file.info(item$capture)$size)
    if (!identical(current, item$bytes)) {
      label <- if (identical(item$source, engine$source %||% "")) "engine" else "harness"
      stop("ERROR: captured ", label,
           " configuration changed during resolution; refusing before execution.",
           call. = FALSE)
    }
  }

  list(schema_version = 1L, workdir = workdir,
       invocation_id = safe_scalar(as.character(invocation_id), "invocation_id"),
       tool = tool, args = args,
       config = if (is.null(engine)) NULL else engine[c("source", "sha256", "bytes")],
       harness = if (is.null(machine)) NULL else machine[c("source", "sha256")],
       provision = list(value = provision, source = provision_source,
                        displaced = displaced),
       image = image, license = license, env_file = env_file, compute = compute)
}

# draft_run_config(workdir, ...) seeds a per-run config for SME review.
# NOT a verbatim copy of the house default (SME feedback 2026-08-31:
# a full copy hides what is specific to the run): the draft is TRIMMED
# to the engine and lane the run actually uses, carries a header naming
# the run and its purpose, and applies run-specific operational
# overrides at draft time so the reviewer sees the deltas, not
# boilerplate. Values still seed from config/engine.yaml; anything not
# present in the per-run file falls back to the house default at load
# time ONLY via the sections included here — the file is the complete
# operational statement for the run.
#
# workdir    run folder.
# run        short label for the run configuration header.
# purpose    one-line purpose for the header.
# lane       which access paths the run uses: "psn", "bbr", or "both" —
#            only the matching operational blocks are emitted.
# overrides  named list of run-specific operational values, applied onto
#            the seeded blocks; names use section$key form via nested
#            lists, e.g. list(psn = list(clean = 2, threads = 4)).
#
# The SME edits the file directly (that IS the tweak mechanism) and
# approves it at the gate. Refuses to overwrite an existing per-run
# config — an SME-edited file must never be silently regenerated.
draft_run_config <- function(workdir, run = basename(workdir),
                             purpose = "engine run",
                             lane = c("psn", "bbr", "both"),
                             overrides = list(),
                             default = "config/engine.yaml") {
  stopifnot(dir.exists(workdir))
  lane <- match.arg(lane)
  out <- file.path(workdir, "run-config.yaml")
  if (file.exists(out)) {
    stop("run-config.yaml already exists in ", workdir,
         " — it may carry SME edits; delete it explicitly if a fresh ",
         "draft is intended.", call. = FALSE)
  }
  house <- yaml::read_yaml(default)

  # Trim to what this run uses — lane AND provision aware (SME feedback
  # 2026-08-31: a bbi-only key like nm_version_label in a PsN-lane
  # native run is misleading boilerplate). Provision may arrive as an
  # override; resolve it first so the key set matches the final value.
  nm_house <- house$engines$nonmem
  provision <- overrides$engines$nonmem$provision %||% nm_house$provision
  nm <- list(provision = provision, executor = nm_house$executor)
  if (identical(provision, "containerized")) nm$image <- nm_house$image
  if (lane %in% c("bbr", "both")) {
    # bbi-lane keys only: the bbi.yaml nonmem entry label plus the
    # provision-matching executable wiring.
    nm$nm_version_label <- nm_house$nm_version_label
    if (identical(provision, "containerized")) {
      nm$shim_home       <- nm_house$shim_home
      nm$shim_executable <- nm_house$shim_executable
    } else {
      nm$native_executable <- nm_house$native_executable
    }
  }
  cfg <- list(engines = list(nonmem = nm))
  if (lane %in% c("bbr", "both")) cfg$bbi <- house$bbi
  if (lane %in% c("psn", "both")) cfg$psn <- house$psn

  # Apply run-specific overrides (nested named lists onto the trimmed
  # config) so the reviewer sees final values, not a diff exercise.
  apply_over <- function(base, over) {
    for (nm in names(over)) {
      base[[nm]] <- if (is.list(over[[nm]]) && is.list(base[[nm]]))
        apply_over(base[[nm]], over[[nm]]) else over[[nm]]
    }
    base
  }
  cfg <- apply_over(cfg, overrides)

  # NONMEM-version discovery (PsN lane): the authoritative table is the
  # site psn.conf [nm_versions]; `psn -nm_versions` is the query that
  # prints it. Run it through the executor so the draft embeds the live
  # choices for the reviewer, and a configured label that is not in the
  # site table fails HERE, at draft time, not at execution. Discovery is
  # best-effort: if the provision cannot answer (no container/PATH yet),
  # the draft proceeds with a warning line instead.
  nm_versions_lines <- character(0)
  if (lane %in% c("psn", "both")) {
    listing <- tryCatch(
      suppressWarnings(system2(nm$executor, c(workdir, "psn", "-nm_versions"),
                               stdout = TRUE, stderr = TRUE)),
      error = function(e) NULL)
    labels <- if (!is.null(listing))
      sub("^([A-Za-z0-9_.-]+)\\s+\\(.*$", "\\1",
          grep("^[A-Za-z0-9_.-]+\\s+\\(", listing, value = TRUE))
    else character(0)
    if (length(labels)) {
      nm_versions_lines <- c(
        "# Site [nm_versions] labels (live `psn -nm_versions` at draft time):",
        paste0("#   ", grep("^[A-Za-z0-9_.-]+\\s+\\(", listing, value = TRUE)))
      chosen <- cfg$psn$nm_version
      if (!is.null(chosen) && !(chosen %in% labels)) {
        stop("psn.nm_version '", chosen, "' is not in the site psn.conf ",
             "[nm_versions] table (available: ",
             paste(labels, collapse = ", "), ").", call. = FALSE)
      }
    } else {
      nm_versions_lines <-
        "# (psn -nm_versions discovery unavailable at draft time — verify the nm_version label manually.)"
    }
  }

  header <- c(
    "# PER-RUN engine config — REVIEW BEFORE EXECUTION.",
    paste0("# Run:     ", run),
    paste0("# Purpose: ", purpose),
    paste0("# Lane:    ", lane,
           " (blocks below are trimmed to this run's access path)"),
    paste0("# Drafted ", format(Sys.time(), "%Y-%m-%d %H:%M UTC", tz = "UTC"),
           " from ", default, "; run-specific overrides applied: ",
           if (length(overrides)) paste(deparse(overrides), collapse = " ")
           else "none"),
    "# The SME edits this file directly and approves it at the gate;",
    "# execution obeys this file over the house default, and the",
    "# engine-config-used/ snapshots prove what actually ran.",
    "#")
  writeLines(c(header,
               .render_run_config(cfg, house, nm_versions_lines)),
             out)
  invisible(out)
}

# Hand-rendered, SELF-DOCUMENTING emission (SME feedback 2026-08-31: the
# draft must carry inline comments, house defaults, and live choices —
# yaml::as.yaml drops comments, so the known blocks are rendered
# explicitly). Values still come from the trimmed+overridden cfg; a
# value differing from the house default gets the house value noted.
.render_run_config <- function(cfg, house, nm_versions_lines) {
  yv <- function(x) {
    if (is.null(x)) "null"
    else if (is.logical(x)) tolower(as.character(x))
    else as.character(x)
  }
  # key line with inline comment; appends "house default: X" when the
  # run value differs from config/engine.yaml.
  # `always` forces the house default into the comment even when the run
  # value equals it — used for the COMPUTE keys (SME request 2026-09-03:
  # the reviewer must see the default and the effect on every draft).
  kv <- function(key, val, comment, house_val, indent = "  ", always = FALSE) {
    note <- comment
    if (!missing(house_val) && (always || !identical(val, house_val))) {
      note <- paste0(comment, " [house default: ", yv(house_val), "]")
    }
    sprintf("%s%s: %s%s", indent, key, yv(val),
            if (nzchar(note)) paste0("  # ", note) else "")
  }
  nm <- cfg$engines$nonmem
  hnm <- house$engines$nonmem
  hpsn <- house$psn
  lines <- c("engines:", "  nonmem:")
  lines <- c(lines,
    kv("provision", nm$provision,
       "containerized | native (native = site PsN/NONMEM from PATH)",
       hnm$provision, indent = "    "),
    kv("executor", nm$executor, "provision dispatcher", indent = "    "))
  if (!is.null(nm$image))
    lines <- c(lines, kv("image", nm$image, "container image", indent = "    "))
  if (!is.null(nm$nm_version_label))
    lines <- c(lines, kv("nm_version_label", nm$nm_version_label,
                         "bbi.yaml nonmem entry key (bbr lane)", indent = "    "))
  if (!is.null(nm$shim_home))
    lines <- c(lines,
      kv("shim_home", nm$shim_home, "bbi shim home (containerized bbr)", indent = "    "),
      kv("shim_executable", nm$shim_executable, "", indent = "    "))
  if (!is.null(nm$native_executable) && is.null(nm$shim_home))
    lines <- c(lines, kv("native_executable", nm$native_executable,
                         "nmfe under <nonmem_home>/run/ (bbr native; home from harness.yaml)",
                         indent = "    "))

  if (!is.null(cfg$bbi)) {
    b <- cfg$bbi; hb <- house$bbi
    lines <- c(lines, "", "bbi:",
      "  # COMPUTE (SME sets per run). bbi v3.4.0 has no separate nodes flag:",
      "  # --threads is \"number of threads to execute with locally or nodes to",
      "  # execute on in parallel\" (bbi nonmem run local --help). With",
      "  # parallel: true, threads MUST equal nodes and the parafile's NODES=",
      "  # line; load_engine_config stops otherwise. bbi copies the parafile",
      "  # into <run>/<run>.pnm and passes -parafile=<run>.pnm to nmfe.",
      kv("mode", b$mode,
         "submit_model .mode: local | sge (bbi has no slurm mode; on slurm run local inside sbatch)",
         hb$mode, always = TRUE),
      "  args:",
      kv("overwrite", b$args$overwrite, "", hb$args$overwrite, indent = "    "),
      kv("threads", b$args$threads,
         "concurrent local model runs; = MPI node count when parallel: true",
         hb$args$threads, indent = "    ", always = TRUE),
      kv("parallel", b$args$parallel,
         "true runs NONMEM under MPI (--parallel); requires parafile",
         hb$args$parallel, indent = "    ", always = TRUE),
      kv("parafile", b$args$parafile,
         "MPI parafile, relative to this run folder; must exist before the run (e.g. mpilinux8.pnm)",
         hb$args$parafile, indent = "    ", always = TRUE),
      kv("nodes", b$args$nodes %||% .bbi_nodes_default,
         "MPI node count as approved; bbi applies it via threads (see above), so keep equal to threads when parallel: true",
         hb$args$nodes %||% .bbi_nodes_default, indent = "    ", always = TRUE))
  }

  if (!is.null(cfg$psn)) {
    p <- cfg$psn
    lines <- c(lines, "", "psn:",
      "  # NONMEM version label from the site psn.conf [nm_versions];",
      sub("^# ", "  # ", nm_versions_lines),
      kv("nm_version", p$nm_version,
         "emitted as -nm_version=; explicit pin survives a psn.conf default repoint",
         hpsn$nm_version),
      kv("threads", p$threads, "concurrent MODEL runs (-threads); not NONMEM MPI", hpsn$threads, always = TRUE),
      # retries / tweak_inits / picky are METHOD-AFFECTING (finding M6c):
      # they change which minimum is reached or accepted, so every per-run
      # review must surface them — the suffix does that in the drafted file.
      kv("retries", p$retries,
         "estimation retries (-retries) [method-affecting: confirm at run review]",
         hpsn$retries),
      kv("tweak_inits", p$tweak_inits,
         "perturb initials on retry [method-affecting: confirm at run review]",
         hpsn$tweak_inits),
      kv("picky", p$picky,
         "strict termination checking [method-affecting: confirm at run review]",
         hpsn$picky),
      kv("clean", p$clean, "run-dir cleanup 0-3 (-clean); null = PsN default", hpsn$clean),
      kv("model_subdir", p$model_subdir, "false emits -no-model_subdir; null = PsN default",
         hpsn$model_subdir),
      kv("submit", p$submit, "local | sge | slurm | torque (-run_on_<grid>)", hpsn$submit, always = TRUE),
      kv("prepend_flags", p$prepend_flags, "-<grid>_prepend_flags value, e.g. \" -n 1\"",
         hpsn$prepend_flags),
      kv("queue", p$queue, "sge_queue / slurm_partition / torque_queue", hpsn$queue, always = TRUE),
      kv("account", p$account, "slurm_account", hpsn$account, always = TRUE),
      kv("nodes", p$nodes, "NONMEM MPI within run (-nodes=); >1 requires parafile; sir drops it", hpsn$nodes, always = TRUE),
      kv("parafile", p$parafile, "MPI parafile (-parafile=), relative to this run folder; must exist before the run", hpsn$parafile, always = TRUE),
      kv("mpi", p$mpi, "guard flag; sir drops nodes/parafile regardless", hpsn$mpi))
  }
  lines
}
