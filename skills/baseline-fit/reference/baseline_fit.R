# Inert candidate. The baseline unit must be confirmed before either export runs.
# Workspace custody is behavioral; the checked parent is reread after launch.
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

.baseline_gate <- base::local({
  private <- .mm_trusted_private
  function() {
    base::get("require_validated_method", envir = private)(
      skill_dir = "skills/baseline-fit",
      calling_file = "skills/baseline-fit/reference/baseline_fit.R", unit = "baseline")
  }
}, envir = base::new.env())

.baseline_fail <- function(...) stop("baseline-fit: ", ..., call. = FALSE)
.baseline_object <- function(x, fields, label) {
  if (!is.list(x) || is.data.frame(x) || is.null(names(x)) ||
      anyNA(names(x)) || anyDuplicated(names(x)) ||
      !setequal(names(x), fields)) .baseline_fail("invalid ", label, " fields")
  invisible(x)
}
.baseline_string <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x)) ||
      grepl("UNRESOLVED|[\r\n]", x)) .baseline_fail("required resolved string")
  x
}
.baseline_hash <- function(path) digest::digest(file = path, algo = "sha256")
.baseline_under <- function(path, root = getwd(), exists = TRUE) {
  .baseline_string(path)
  root <- normalizePath(root, mustWork = TRUE)
  p <- normalizePath(path, mustWork = exists)
  if (!startsWith(p, paste0(root, "/")) || identical(p, root))
    .baseline_fail("path outside artifact custody")
  p
}
.baseline_regular <- function(path) {
  if (!file.exists(path) || isTRUE(file.info(path)$isdir) ||
      nzchar(Sys.readlink(path))) .baseline_fail("required regular non-symlink artifact")
  normalizePath(path, mustWork = TRUE)
}
.baseline_record <- function(x, root = getwd()) {
  .baseline_object(x, c("path", "sha256"), "artifact")
  if (nzchar(Sys.readlink(.baseline_string(x$path)))) .baseline_fail("symlink artifact")
  p <- .baseline_regular(.baseline_under(x$path, root))
  if (!grepl("^[a-f0-9]{64}$", .baseline_string(x$sha256)) ||
      !identical(.baseline_hash(p), x$sha256)) .baseline_fail("artifact digest mismatch")
  p
}
.baseline_file <- function(path) list(path = path, sha256 = .baseline_hash(path))
.baseline_json <- function(path) {
  # simplifyVector=FALSE retains object names, including duplicate JSON keys.
  x <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  walk <- function(v) {
    if (is.null(v)) return(invisible(NULL))
    if (is.list(v)) {
      if (!is.null(names(v)) && (anyDuplicated(names(v)) || any(!nzchar(names(v)))))
        .baseline_fail("duplicate or empty JSON key")
      for (w in v) walk(w)
    } else if (length(v) != 1L || anyNA(v)) .baseline_fail("invalid JSON scalar")
  }
  walk(x)
  x
}
.baseline_binding <- function(model, data) {
  lines <- readLines(model, warn = FALSE)
  active <- grep("^[[:space:]]*\\$DATA([[:space:]]|$)", lines)
  first <- grep("$DATA", lines, fixed = TRUE)
  if (length(active) != 1L || !length(first) || first[[1L]] != active[[1L]] ||
      any(grepl("^[[:space:]]*\\$(INCLUDE|MSFI|ETAS|PHIS|ABBR)", lines)))
    .baseline_fail("unsupported DATA selection")
  # Use the same supported nmrec API as the pinned bbr control resolver.
  records <- tryCatch(nmrec::select_records(nmrec::read_ctl(model), "data"),
                      error = function(e) .baseline_fail("unsupported DATA parser syntax"))
  if (length(records) != 1L) .baseline_fail("DATA parser count disagreement")
  filename <- tryCatch(nmrec::get_record_option(records[[1L]], "filename"),
                       error = function(e) .baseline_fail("unsupported DATA parser filename"))
  if (is.null(filename)) .baseline_fail("DATA parser filename missing")
  fields <- strsplit(trimws(lines[[active]]), "[[:space:]]+")[[1L]]
  if (length(fields) < 2L || !grepl("^[A-Za-z0-9][A-Za-z0-9_.-]*$", fields[[2L]]) ||
      fields[[2L]] %in% c(".", "..")) .baseline_fail("unsupported DATA token")
  if (!identical(filename$value, fields[[2L]])) .baseline_fail("DATA parser token disagreement")
  resolved <- normalizePath(file.path(dirname(model), fields[[2L]]), mustWork = TRUE)
  if (!identical(resolved, normalizePath(data, mustWork = TRUE)))
    .baseline_fail("control DATA differs from approved data")
  list(token = fields[[2L]], base = dirname(model), resolved_path = resolved)
}
.baseline_identity <- function(x) {
  .baseline_object(x, c("analysis_id", "model", "data", "disposition", "data_binding",
                        "intended_use"), "identity")
  .baseline_string(x$analysis_id); .baseline_string(x$intended_use)
  model <- .baseline_record(x$model); data <- .baseline_record(x$data)
  .baseline_record(x$disposition)
  .baseline_object(x$data_binding, c("token", "base", "resolved_path"), "binding")
  if (!identical(x$data_binding, .baseline_binding(model, data)))
    .baseline_fail("input binding mismatch")
  invisible(x)
}
.baseline_equal <- function(a, b) {
  # Configuration equality ignores only the resolver's documented attribute.
  attr(a, "config_source") <- NULL
  identical(jsonlite::toJSON(a, auto_unbox = TRUE, null = "null", digits = NA),
            jsonlite::toJSON(b, auto_unbox = TRUE, null = "null", digits = NA))
}
.baseline_recipe <- function(spec) {
  .baseline_object(spec, c("invocation_root", "run_id", "recipe"), "run_spec")
  .baseline_string(spec$invocation_root); .baseline_string(spec$run_id)
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9_-]*$", spec$run_id)) .baseline_fail("invalid run id")
  p <- .baseline_record(spec$recipe)
  r <- .baseline_json(p)
  .baseline_object(r, c("schema", "versions", "bbi_executable", "engine_config",
                        "provisioning", "execution", "r_provenance"), "recipe")
  if (!identical(r$schema, 1L)) .baseline_fail("recipe schema")
  .baseline_object(r$versions, c("bbr", "nmrec", "bbi"), "versions")
  lapply(r$versions, .baseline_string)
  .baseline_object(r$engine_config, c("source", "body"), "engine_config")
  .baseline_record(r$engine_config$source)
  .baseline_object(r$provisioning, c("nonmem_home", "executable"), "provisioning")
  for (item in list(r$bbi_executable, r$provisioning$executable)) {
    .baseline_object(item, c("path", "sha256"), "executable identity")
    if (!startsWith(.baseline_string(item$path), "/") ||
        !grepl("^[a-f0-9]{64}$", .baseline_string(item$sha256)))
      .baseline_fail("explicit executable identity required")
  }
  .baseline_object(r$execution, c("mode", "wait", "child_dirs", "bbi_config_sha256"), "execution")
  if (!identical(r$execution$mode, "local") || !isTRUE(r$execution$wait) ||
      !isTRUE(r$execution$child_dirs) ||
      !grepl("^[a-f0-9]{64}$", .baseline_string(r$execution$bbi_config_sha256)))
    .baseline_fail("unsupported execution recipe")
  .baseline_object(r$r_provenance, "expected", "r_provenance")
  if (!.baseline_string(r$r_provenance$expected) %in% c("managed", "site"))
    .baseline_fail("invalid R provenance expectation")
  cfg <- r$engine_config$body
  .baseline_object(cfg, c("engines", "bbi"), "supported config body")
  .baseline_object(cfg$engines, "nonmem", "supported engines")
  .baseline_object(cfg$bbi, c("mode", "args"), "supported bbi config")
  nm <- cfg$engines$nonmem; args <- cfg$bbi$args
  if (!is.list(nm) || !.baseline_string(nm$provision) %in% c("native", "containerized") ||
      !identical(cfg$bbi$mode, "local")) .baseline_fail("incomplete engine body")
  .baseline_string(nm$nm_version_label)
  required <- c("clean_lvl", "copy_lvl", "threads", "nodes", "overwrite", "parallel", "parafile")
  .baseline_object(args, required, "supported operational args")
  for (key in c("clean_lvl", "copy_lvl", "threads", "nodes"))
    if (!is.integer(args[[key]]) || length(args[[key]]) != 1L || is.na(args[[key]]))
      .baseline_fail("operational integer required")
  if (!identical(args$overwrite, FALSE) || !is.logical(args$parallel) ||
      length(args$parallel) != 1L || is.na(args$parallel)) .baseline_fail("unsafe operational args")
  if (!is.null(args$parafile)) .baseline_under(.baseline_string(args$parafile))
  if (nm$provision == "native") {
    .baseline_object(nm, c("provision", "nm_version_label", "native_executable"), "native engine")
    .baseline_string(r$provisioning$nonmem_home); .baseline_string(nm$native_executable)
    if (!startsWith(r$provisioning$nonmem_home, "/") ||
        !identical(normalizePath(r$provisioning$nonmem_home, mustWork = TRUE), r$provisioning$nonmem_home))
      .baseline_fail("canonical native home required")
  } else {
    .baseline_object(nm, c("provision", "nm_version_label", "shim_home", "shim_executable"), "container engine")
    if (!is.null(r$provisioning$nonmem_home)) .baseline_fail("container home must be null")
    .baseline_string(nm$shim_home); .baseline_string(nm$shim_executable)
  }
  # These source versions have the traced local child-directory contract.
  if (!identical(r$versions$bbr, "1.14.2") || !identical(r$versions$bbi, "v3.4.0"))
    .baseline_fail("unsupported traced source versions")
  r
}
.baseline_packages <- function(model_dir, before, since, expected, fresh) {
  if (!is.character(expected) || length(expected) != 1L ||
      !expected %in% c("managed", "site")) .baseline_fail("invalid package expectation")
  paths <- list.files(file.path(model_dir, "engine-config-used"),
                      pattern = "-r-packages\\.yaml$", full.names = TRUE)
  if (length(paths) != 1L || length(intersect(paths, before)))
    .baseline_fail("missing, reused or ambiguous R snapshot")
  p <- .baseline_regular(.baseline_under(paths[[1L]], model_dir))
  fresh(p, since, "R package snapshot")
  lines <- readLines(p, warn = FALSE)
  if (!length(lines) || lines[[1L]] !=
      "# R packages in effect at execution (written by write_r_packages_used)")
    .baseline_fail("R snapshot producer marker")
  fields <- c("invoked_at_utc", "r_version", "r_provision", "lock_check")
  header <- stats::setNames(lapply(fields, function(key) {
    prefix <- paste0("# ", key, ": ")
    hit <- lines[startsWith(lines, prefix)]
    if (length(hit) != 1L) .baseline_fail("R snapshot header")
    .baseline_string(substring(hit, nchar(prefix) + 1L))
  }), fields)
  ts <- as.POSIXct(header$invoked_at_utc, format = "%Y%m%dT%H%M%SZ", tz = "UTC")
  if (is.na(ts) || format(ts, "%Y%m%dT%H%M%SZ", tz = "UTC") != header$invoked_at_utc ||
      !identical(header$r_provision, expected)) .baseline_fail("R snapshot mode or timestamp")
  list(path = p, sha256 = .baseline_hash(p), header = header)
}
.baseline_snapshot <- function(path, model_dir, model, config, resolved, since) {
  p <- .baseline_regular(.baseline_under(path, model_dir))
  fail <- function() .baseline_fail("fit snapshot relationship mismatch")
  if (!identical(dirname(p), file.path(model_dir, "engine-config-used"))) fail()
  lines <- readLines(p, warn = FALSE)
  if (!length(lines) || !identical(lines[[1L]],
      "# Engine-config snapshot (written by reference_nm_fit)")) fail()
  field <- function(key) {
    prefix <- paste0("# ", key, ": ")
    values <- lines[startsWith(lines, prefix)]
    if (length(values) != 1L) fail()
    substring(values, nchar(prefix) + 1L)
  }
  if (!identical(field("engine"), "nonmem (bbr/bbi estimation lane)") ||
      !identical(field("tool"), "bbr submit_model") ||
      !identical(field("model"), model) || !identical(field("mode"), "local") ||
      !identical(field("config_source"), config) ||
      !identical(field("config_sha256"), resolved$sha256)) fail()
  invoked <- field("invoked_at_utc")
  stamp <- as.POSIXct(invoked, format = "%Y%m%dT%H%M%SZ", tz = "UTC")
  # The existing producer records whole UTC seconds, while run_start is fractional.
  if (is.na(stamp) || format(stamp, "%Y%m%dT%H%M%SZ", tz = "UTC") != invoked ||
      as.numeric(stamp) < floor(as.numeric(since)) || stamp > Sys.time()) fail()
  body <- tryCatch(yaml::read_yaml(p), error = function(e) fail())
  if (!.baseline_equal(body, list(bbi = resolved$cfg$bbi))) fail()
  invisible(p)
}
.baseline_loaded <- function() {
  record <- attr(environment(.baseline_loaded), "load_reference")
  if (is.null(record)) .baseline_fail("trusted loader required")
  invisible(TRUE)
}
.baseline_old <- function() {
  e <- new.env(parent = baseenv())
  base::sys.source("tools/load_reference.R", envir = e)
  old <- e$load_reference("skills/nonmem-estimation", "skills/nonmem-estimation/reference/bbr_estimation.R")
  lockEnvironment(old, bindings = TRUE)
  old
}
.baseline_default_ops <- function() {
  old <- .baseline_old()
  ops <- new.env(parent = emptyenv())
  ops$identity <- function(r) {
    for (p in c("bbr", "nmrec"))
      if (!identical(as.character(utils::packageVersion(p)), r$versions[[p]]))
        .baseline_fail("loaded version mismatch")
    for (item in list(r$bbi_executable, r$provisioning$executable)) {
      .baseline_object(item, c("path", "sha256"), "executable")
      p <- .baseline_regular(.baseline_string(item$path))
      if (!startsWith(p, "/") || !identical(p, item$path) ||
          file.access(p, 1L) != 0L || !identical(.baseline_hash(p), item$sha256))
        .baseline_fail("executable identity mismatch")
    }
    version <- system2(r$bbi_executable$path, "version", stdout = TRUE, stderr = FALSE, timeout = 30)
    if (!is.null(attr(version, "status")) || length(version) != 1L ||
        !identical(trimws(version), r$versions$bbi)) .baseline_fail("bbi version mismatch")
    nm <- r$engine_config$body$engines$nonmem
    home <- if (nm$provision == "native") r$provisioning$nonmem_home else nm$shim_home
    exe <- if (nm$provision == "native") nm$native_executable else nm$shim_executable
    if (!identical(normalizePath(file.path(home, "run", exe), mustWork = TRUE),
                   r$provisioning$executable$path)) .baseline_fail("selected engine identity")
    invisible(TRUE)
  }
  ops$resolve <- old$.reference_nm_resolve_config
  ops$workspace <- old$reference_nm_workspace
  ops$snapshot <- old$.reference_nm_fit_snapshot
  ops$fresh <- old$.reference_nm_require_fresh
  ops$termination_lines <- old$.reference_nm_termination_lines
  ops$termination_text <- old$.reference_nm_termination_text
  ops$new_model <- function(path) bbr::new_model(path)
  ops$model_path <- function(mod) bbr::get_model_path(mod)
  ops$data_path <- function(mod) bbr::get_data_path(mod)
  ops$select <- function(bbi) options(bbr.bbi_exe_path = bbi)
  ops$restore <- function(previous) options(previous)
  ops$submit <- function(mod) bbr::submit_model(mod, .mode = "local", .wait = TRUE)
  ops$heuristics <- function(mod) bbr::model_summary(mod)$run_heuristics
  ops$parameters <- function(mod) bbr::param_estimates(bbr::model_summary(mod))
  lockEnvironment(ops, bindings = TRUE)
  ops
}
.baseline_factory <- function(operations) {
  if (!is.environment(operations) || !environmentIsLocked(operations) ||
      !all(vapply(ls(operations, all.names = TRUE), bindingIsLocked, logical(1), env = operations)))
    .baseline_fail("private operations must be locked")
  fit <- function(input_identity, run_spec) {
    .baseline_gate()
    .baseline_loaded()
    .baseline_identity(input_identity)
    r <- .baseline_recipe(run_spec)
    if (!identical(sub("\\.mod$", "", basename(input_identity$model$path)), run_spec$run_id))
      .baseline_fail("run id differs from model stem")
    operations$identity(r)
    previous_selection <- operations$select(r$bbi_executable$path)
    on.exit(operations$restore(previous_selection), add = TRUE)
    root <- normalizePath(getwd(), mustWork = TRUE)
    # Require an existing parent and an absent, simple child invocation directory.
    parent <- .baseline_under(dirname(run_spec$invocation_root), root)
    parent <- normalizePath(parent, mustWork = TRUE)
    base <- basename(run_spec$invocation_root)
    if (!grepl("^[A-Za-z0-9][A-Za-z0-9_-]*$", base)) .baseline_fail("invalid invocation path")
    invocation <- file.path(parent, base)
    if (!identical(invocation, run_spec$invocation_root))
      .baseline_fail("invocation_root must be the absolute canonical path")
    if (file.exists(invocation)) .baseline_fail("invocation already exists")
    dir.create(invocation)
    model_dir <- file.path(invocation, "model"); dir.create(model_dir)
    model <- file.path(model_dir, paste0(run_spec$run_id, ".mod"))
    data <- file.path(model_dir, input_identity$data_binding$token)
    config <- file.path(model_dir, "run-config.yaml")
    for (pair in list(c(input_identity$model$path, model), c(input_identity$data$path, data),
                     c(r$engine_config$source$path, config)))
      if (!file.copy(pair[[1L]], pair[[2L]], overwrite = FALSE)) .baseline_fail("staging failed")
    check_config <- function() {
      .baseline_record(r$engine_config$source)
      if (!identical(.baseline_hash(config), r$engine_config$source$sha256) ||
          !.baseline_equal(yaml::read_yaml(config), r$engine_config$body))
        .baseline_fail("staged config source/body mismatch")
    }
    check_config()
    resolved <- operations$resolve(model_dir, repo_root = root)
    if (!identical(normalizePath(resolved$path, mustWork = TRUE), config) ||
        !identical(resolved$sha256, r$engine_config$source$sha256) ||
        !.baseline_equal(resolved$cfg, r$engine_config$body)) .baseline_fail("resolver mismatch")
    since <- Sys.time()
    before <- list.files(file.path(model_dir, "engine-config-used"), full.names = TRUE)
    operations$workspace(model_dir, engine_config = resolved$cfg, repo_root = root,
                         nonmem_home = r$provisioning$nonmem_home, bbi_path = r$bbi_executable$path)
    bbi_config <- file.path(model_dir, "bbi.yaml")
    operations$fresh(bbi_config, since, "bbi config")
    if (!identical(.baseline_hash(bbi_config), r$execution$bbi_config_sha256))
      .baseline_fail("bbi config mismatch")
    packages <- .baseline_packages(model_dir, before, since, r$r_provenance$expected, operations$fresh)
    output <- file.path(model_dir, run_spec$run_id)
    clean <- function() {
      if (file.exists(output) || file.exists(file.path(model_dir, paste0(run_spec$run_id, ".ctl"))))
        .baseline_fail("stale output or ambiguous model suffix")
    }
    clean()
    mod <- operations$new_model(model)
    snapshot <- operations$snapshot(model_dir, model_path = model, mode = "local", resolved = resolved)
    operations$fresh(snapshot, since, "fit config snapshot")
    check <- function() {
      .baseline_gate()
      .baseline_identity(input_identity); .baseline_record(run_spec$recipe)
      check_config(); operations$identity(r); clean()
      .baseline_regular(model); .baseline_regular(data)
      if (!identical(.baseline_hash(model), input_identity$model$sha256) ||
          !identical(.baseline_hash(data), input_identity$data$sha256) ||
          !identical(normalizePath(operations$model_path(mod), mustWork = TRUE), model) ||
          !identical(normalizePath(operations$data_path(mod), mustWork = TRUE), data))
        .baseline_fail("checked model/data differs from approved inputs")
      .baseline_binding(model, data)
      .baseline_snapshot(snapshot, model_dir, model, config, resolved, since)
      if (!identical(.baseline_hash(bbi_config), r$execution$bbi_config_sha256) ||
          !identical(.baseline_hash(packages$path), packages$sha256) ||
          !identical(packages$header$r_provision, r$r_provenance$expected))
        .baseline_fail("final provenance drift")
      invisible(TRUE)
    }
    check()
    operations$submit(mod)
    listing <- file.path(output, paste0(run_spec$run_id, ".lst"))
    operations$fresh(listing, since, "listing")
    lines <- operations$termination_lines(listing)
    termination <- list(lines = lines, text = operations$termination_text(lines))
    list(mod = mod, input_identity = input_identity,
         identity_sha256 = digest::digest(input_identity, algo = "sha256"), run_spec = run_spec,
         checked_inputs = list(original_model = input_identity$model, original_data = input_identity$data,
                               staged_model = .baseline_file(model), staged_data = .baseline_file(data)),
         run_start = since, pre_run_inventory = before,
         config = list(source = r$engine_config$source, staged = .baseline_file(config),
                       bbi = .baseline_file(bbi_config), snapshot = .baseline_file(snapshot)),
         listing = .baseline_file(listing), termination = termination,
         heuristics = operations$heuristics(mod),
         execution_copy = list(status = "unavailable", reason = "Execution-copy transformation not observed; parent custody is behavioral."),
         r_packages = packages)
  }
  summary <- function(fit_receipt) {
    .baseline_gate()
    .baseline_loaded()
    f <- fit_receipt
    .baseline_object(f, c("mod", "input_identity", "identity_sha256", "run_spec", "checked_inputs",
                         "run_start", "pre_run_inventory", "config", "listing", "termination",
                         "heuristics", "execution_copy", "r_packages"), "fit receipt")
    .baseline_identity(f$input_identity)
    if (!identical(digest::digest(f$input_identity, algo = "sha256"), f$identity_sha256))
      .baseline_fail("receipt identity mismatch")
    .baseline_record(f$listing); .baseline_record(f$r_packages[c("path", "sha256")])
    .baseline_object(f$checked_inputs, c("original_model", "original_data", "staged_model", "staged_data"),
                     "checked inputs")
    lapply(f$checked_inputs, .baseline_record)
    .baseline_object(f$config, c("source", "staged", "bbi", "snapshot"), "config receipt")
    lapply(f$config, .baseline_record)
    r <- .baseline_recipe(f$run_spec)
    invocation <- .baseline_under(f$run_spec$invocation_root)
    if (!identical(invocation, f$run_spec$invocation_root) ||
        !identical(sub("\\.mod$", "", basename(f$input_identity$model$path)), f$run_spec$run_id))
      .baseline_fail("receipt invocation custody mismatch")
    model_dir <- file.path(invocation, "model")
    expected_model <- file.path(model_dir, paste0(f$run_spec$run_id, ".mod"))
    expected_data <- file.path(model_dir, f$input_identity$data_binding$token)
    if (!identical(f$checked_inputs$original_model, f$input_identity$model) ||
        !identical(f$checked_inputs$original_data, f$input_identity$data) ||
        !identical(f$checked_inputs$staged_model$path, expected_model) ||
        !identical(f$checked_inputs$staged_data$path, expected_data) ||
        !identical(f$checked_inputs$staged_model$sha256, f$input_identity$model$sha256) ||
        !identical(f$checked_inputs$staged_data$sha256, f$input_identity$data$sha256) ||
        !identical(normalizePath(operations$model_path(f$mod), mustWork = TRUE), expected_model) ||
        !identical(normalizePath(operations$data_path(f$mod), mustWork = TRUE), expected_data))
      .baseline_fail("receipt checked input relationship mismatch")
    .baseline_binding(expected_model, expected_data)
    if (!identical(f$config$source, r$engine_config$source) ||
        !identical(f$config$staged$path, file.path(model_dir, "run-config.yaml")) ||
        !identical(f$config$staged$sha256, r$engine_config$source$sha256) ||
        !.baseline_equal(yaml::read_yaml(f$config$staged$path), r$engine_config$body) ||
        !identical(f$config$bbi$path, file.path(model_dir, "bbi.yaml")) ||
        !identical(f$config$bbi$sha256, r$execution$bbi_config_sha256) ||
        !identical(dirname(f$config$snapshot$path), file.path(model_dir, "engine-config-used")) ||
        !identical(f$listing$path, file.path(model_dir, f$run_spec$run_id,
                                          paste0(f$run_spec$run_id, ".lst"))))
      .baseline_fail("receipt configuration relationship mismatch")
    .baseline_snapshot(f$config$snapshot$path, model_dir, expected_model,
                       f$config$staged$path,
                       list(sha256 = r$engine_config$source$sha256, cfg = r$engine_config$body),
                       f$run_start)
    actual_packages <- .baseline_packages(dirname(f$config$staged$path), character(),
                                          f$run_start, r$r_provenance$expected, operations$fresh)
    if (!identical(actual_packages, f$r_packages)) .baseline_fail("receipt package evidence mismatch")
    .baseline_object(f$termination, c("lines", "text"), "termination")
    if (!is.character(f$termination$lines) || !length(f$termination$lines) ||
        !identical(operations$termination_lines(f$listing$path), f$termination$lines) ||
        !identical(operations$termination_text(f$termination$lines), f$termination$text))
      .baseline_fail("missing or contradictory termination evidence")
    operations$identity(r)
    previous_selection <- operations$select(r$bbi_executable$path)
    on.exit(operations$restore(previous_selection), add = TRUE)
    p <- operations$parameters(f$mod)
    if (is.data.frame(p) && "parameter_names" %in% names(p) && !"parameter" %in% names(p))
      names(p)[names(p) == "parameter_names"] <- "parameter"
    if (!is.data.frame(p) || !all(c("parameter", "estimate", "fixed") %in% names(p)) ||
        anyNA(p$parameter) || anyDuplicated(p$parameter) || !is.logical(p$fixed))
      .baseline_fail("malformed raw parameter table")
    if (!"stderr" %in% names(p)) p$stderr <- NA_real_
    status <- if (!nrow(p) || all(is.na(p$stderr))) "unavailable" else
      if (anyNA(p$stderr)) "partially_available" else "available"
    list(parameters = p[, c("parameter", "estimate", "fixed", "stderr"), drop = FALSE],
         stderr_status = list(status = status, reason = "Raw supplied stderr fields only", evidence = f$listing),
         covariance_status = list(status = "unverified", reason = "No supported covariance reader", evidence = f$listing),
         termination = f$termination, heuristics = f$heuristics,
         evidence = list(listing = f$listing, config = f$config, r_packages = f$r_packages,
                         checked_inputs = f$checked_inputs, execution_copy = f$execution_copy),
         interpretation = "Conditional on fixed population inputs; termination and heuristics are retained independently.")
  }
  list(fit = fit, summary = summary)
}

reference_baseline_fit <- function(input_identity, run_spec) {
  .baseline_gate()
  .baseline_loaded()
  .baseline_factory(.baseline_default_ops())$fit(input_identity, run_spec)
}

reference_baseline_summary <- function(fit_receipt) {
  .baseline_gate()
  .baseline_loaded()
  .baseline_factory(.baseline_default_ops())$summary(fit_receipt)
}
