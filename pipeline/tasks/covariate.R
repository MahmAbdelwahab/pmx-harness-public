# Task module: covariate calculation over SME-frozen parent-fit handoffs.
#
# Two forms. Single: method_unit plus external_input or upstream_target, one
# frozen YAML record. Invocation: upstream_target plus a named `invocations`
# vector, one record per base_model_frozen_yaml hand-off, one merged package.
# Every input and method authorization is validated before a reference loads;
# no engine run, run directory, method choice, or interpretation of evidence.

covariate_reference_executor <- function(reference_environment, entrypoint,
                                         arguments, criteria) {
  fun <- get(entrypoint, envir = reference_environment, inherits = FALSE)
  do.call(fun, c(arguments, criteria))
}

# Runtime helper used only to turn a frozen handoff entry into a format=file
# target. The complete schema, route, digest, method, and manifest validation
# occurs in covariate_task_compute() before the reference loader or executor.
covariate_handoff_role_path <- function(handoff_file, role) {
  record <- tryCatch(yaml::read_yaml(handoff_file), error = function(e)
    stop("covariate: cannot read frozen handoff YAML: ", conditionMessage(e),
         call. = FALSE))
  entry <- record[[role]]
  path <- if (is.list(entry)) entry$path else NULL
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path))
    stop("covariate: frozen handoff entry '", role,
         "' must contain one non-empty path.", call. = FALSE)
  path
}

covariate_task_compute <- function(analysis_id, task_input,
                                   covariate_input_file,
                                   covariate_model_file,
                                   covariate_data_file,
                                   covariate_run_config_file,
                                   covariate_map_inputs_file,
                                   reference_loader,
                                   reference_executor) {
  refuse <- function(...)
    stop("covariate: ", ..., call. = FALSE)
  one_string <- function(x, label) {
    if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x))
      refuse(label, " must be one non-empty string.")
    x
  }
  exact_names <- function(x, expected, label) {
    if (!is.list(x) || is.null(names(x)) ||
        !identical(sort(names(x)), sort(expected))) {
      got <- if (is.list(x)) paste(names(x), collapse = ", ") else class(x)[1]
      refuse(label, " must contain exactly: ", paste(expected, collapse = ", "),
             "; got: ", got, ".")
    }
    invisible(TRUE)
  }
  is_regular_file <- function(path) {
    if (grepl("[[:cntrl:]]", path)) return(FALSE)
    if (!identical(.Platform$OS.type, "unix")) {
      info <- file.info(path)
      return(!is.na(info$isdir[[1]]) && !isTRUE(info$isdir[[1]]) &&
               !nzchar(Sys.readlink(path)[[1]]))
    }
    script <- paste(
      'if [ -f "$1" ] && [ ! -L "$1" ]; then echo 1; else echo 0; fi')
    answer <- tryCatch(suppressWarnings(system2(
      "sh", args = c("-c", shQuote(script), "sh", shQuote(path)),
      stdout = TRUE, stderr = FALSE)), error = function(e) character(0))
    answer <- trimws(as.character(answer))
    identical(answer[nzchar(answer)], "1")
  }
  regular_file <- function(path, label) {
    path <- one_string(path, label)
    # Match the artifact-boundary policy in tools/gate.R: on Unix, `test -f`
    # distinguishes regular files from FIFOs, devices, and sockets, while the
    # separate `! -L` test refuses symlinks that `test -f` would follow. An
    # unavailable or malformed type-test result fails closed before any read
    # or digest. Non-Unix falls back to the file types exposed by that
    # namespace, with both directories and links excluded.
    if (!isTRUE(is_regular_file(path)))
      refuse(label, " must name an existing regular file: ", path)
    path
  }
  file_sha <- function(path) digest::digest(file = path, algo = "sha256")
  read_yaml <- function(path, label) {
    tryCatch(yaml::read_yaml(path), error = function(e)
      refuse("cannot read ", label, " YAML '", path, "': ", conditionMessage(e)))
  }
  nonempty_scalar_fields <- function(x, fields, label) {
    for (field in fields) one_string(x[[field]], paste0(label, "$", field))
    invisible(TRUE)
  }

  methods <- list(
    covariate_effect = list(
      reference_path = "skills/covariate-priors/reference/covariate_effect.R",
      entrypoint = "reference_covariate_exposure_ratio",
      arguments = c("wt_low", "wt_high", "exponents"),
      criteria = c("band_lower", "band_upper")),
    nested_lrt = list(
      reference_path = "skills/covariate-priors/reference/nested_lrt.R",
      entrypoint = "reference_nested_lrt",
      arguments = c("ofv_parent", "ofv_child"),
      criteria = c("criterion_dofv", "df", "alpha")))

  unit <- one_string(task_input$method_unit, "task_input$method_unit")
  method <- methods[[unit]]
  if (is.null(method))
    refuse("method unit '", unit,
           "' is not approved for the R2 covariate task; allowed units are ",
           paste(names(methods), collapse = ", "), ".")

  has_external <- !is.null(task_input$external_input)
  has_upstream <- !is.null(task_input$upstream_target)
  if (has_external == has_upstream)
    refuse("declare exactly one source: external_input or upstream_target.")
  route <- if (has_external) "external" else "upstream"
  if (has_external) {
    declared <- one_string(task_input$external_input, "task_input$external_input")
    if (!identical(declared, covariate_input_file))
      refuse("external_input does not match covariate_input_file.")
  } else {
    upstream <- one_string(task_input$upstream_target, "task_input$upstream_target")
    if (!identical(upstream, "base_model_frozen_yaml"))
      refuse("task_input$upstream_target must be exactly 'base_model_frozen_yaml'.")
  }

  paths <- c(
    handoff = regular_file(covariate_input_file, "covariate_input_file"),
    model = regular_file(covariate_model_file, "covariate_model_file"),
    data = regular_file(covariate_data_file, "covariate_data_file"),
    run_config = regular_file(covariate_run_config_file,
                              "covariate_run_config_file"),
    map_inputs = regular_file(covariate_map_inputs_file,
                              "covariate_map_inputs_file"))
  canonical_paths <- normalizePath(paths, mustWork = TRUE)
  if (anyDuplicated(canonical_paths))
    refuse("duplicate-role frozen inputs are not allowed; handoff, model, data, ",
           "run_config, and map_inputs must be distinct regular files.")

  handoff <- read_yaml(paths[["handoff"]], "frozen handoff")
  handoff_fields <- c("schema_version", "source", "method_unit",
                      "producing_analysis_id", "producing_run_id",
                      "parent_fit_id", "owner", "created_at", "engine",
                      "model", "data", "run_config", "map_inputs")
  exact_names(handoff, handoff_fields, "frozen handoff")
  if (!is.numeric(handoff$schema_version) ||
      length(handoff$schema_version) != 1L ||
      is.na(handoff$schema_version) || handoff$schema_version != 1)
    refuse("frozen handoff schema_version must be 1.")
  if (!identical(handoff$source, route))
    refuse("frozen handoff source must be '", route, "' for this route.")
  if (!identical(handoff$method_unit, unit))
    refuse("frozen handoff method_unit must match task_input$method_unit.")
  nonempty_scalar_fields(
    handoff,
    c("producing_analysis_id", "producing_run_id", "parent_fit_id", "owner",
      "created_at"), "frozen handoff")
  if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$",
             handoff$created_at))
    refuse("frozen handoff$created_at must be an ISO-8601 UTC timestamp.")
  parsed_created_at <- suppressWarnings(as.POSIXct(
    handoff$created_at, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  if (is.na(parsed_created_at) ||
      !identical(format(parsed_created_at, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                 handoff$created_at))
    refuse("frozen handoff$created_at must be a valid ISO-8601 UTC timestamp.")
  exact_names(handoff$engine, c("name", "version"), "frozen handoff$engine")
  nonempty_scalar_fields(handoff$engine, c("name", "version"),
                         "frozen handoff$engine")

  roles <- c("model", "data", "run_config", "map_inputs")
  declared_sha <- character(length(roles))
  observed_sha <- character(length(roles))
  names(declared_sha) <- names(observed_sha) <- roles
  for (role in roles) {
    entry <- handoff[[role]]
    exact_names(entry, c("path", "sha256"), paste0("frozen handoff$", role))
    entry_path <- one_string(entry$path, paste0("frozen handoff$", role, "$path"))
    entry_sha <- one_string(entry$sha256,
                            paste0("frozen handoff$", role, "$sha256"))
    if (!grepl("^[0-9a-f]{64}$", entry_sha))
      refuse("frozen handoff$", role,
             "$sha256 must be 64 lowercase hexadecimal characters.")
    if (!identical(entry_path, paths[[role]]))
      refuse("frozen handoff$", role, "$path does not match the tracked ",
             "covariate_", role, "_file target.")
    actual <- file_sha(paths[[role]])
    if (!identical(entry_sha, actual))
      refuse("digest mismatch for frozen ", role, ": declared ", entry_sha,
             ", observed ", actual, ".")
    declared_sha[[role]] <- entry_sha
    observed_sha[[role]] <- actual
  }

  map <- read_yaml(paths[["map_inputs"]], "MAP inputs")
  exact_names(map, c("method_unit", "priors", "criteria", "arguments"),
              "MAP inputs")
  if (!identical(map$method_unit, unit))
    refuse("MAP inputs method_unit must match task_input and frozen handoff.")
  if (!is.list(map$priors) || !length(map$priors))
    refuse("MAP inputs must contain a non-empty prior list.")
  prior_fields <- c("covariate", "parameter", "direction", "mechanism")
  for (i in seq_along(map$priors)) {
    exact_names(map$priors[[i]], prior_fields,
                paste0("MAP inputs prior ", i))
    nonempty_scalar_fields(map$priors[[i]], prior_fields,
                           paste0("MAP inputs prior ", i))
  }
  exact_names(map$criteria, method$criteria,
              paste0(unit, " criteria"))
  exact_names(map$arguments, method$arguments,
              paste0(unit, " arguments"))
  declared_values <- c(map$arguments, map$criteria)
  for (name in names(declared_values)) {
    value <- declared_values[[name]]
    if (!is.numeric(value) || !length(value) || any(!is.finite(value)))
      refuse("declared value '", name,
             "' must be a non-empty finite numeric value or vector.")
  }
  scalar_values <- if (identical(unit, "covariate_effect"))
    c("wt_low", "wt_high", method$criteria) else
    c(method$arguments, method$criteria)
  for (name in scalar_values) {
    value <- declared_values[[name]]
    if (length(value) != 1L)
      refuse("declared value '", name, "' must be a single finite numeric value.")
  }

  manifest_path <- "skills/covariate-priors/MANIFEST.yaml"
  manifest <- validate_method_manifest(manifest_path, "skills/covariate-priors")
  manifest_unit <- manifest$units[[unit]]
  if (!nzchar(manifest$reviewed_by) || is.null(manifest_unit) ||
      !identical(manifest_unit$status, "approved"))
    refuse("method unit '", unit, "' is not approved in ", manifest_path, ".")
  if (!identical(manifest_unit$reference_path, method$reference_path) ||
      !identical(manifest_unit$entrypoints, method$entrypoint))
    refuse("method unit '", unit,
           "' does not match the R2 fixed reference map.")

  reference_environment <- reference_loader(
    "skills/covariate-priors", method$reference_path)
  load_receipt <- attr(reference_environment, "load_reference")
  if (!is.list(load_receipt) ||
      !identical(load_receipt$reference_path, method$reference_path))
    refuse("load_reference did not return the required digest receipt.")
  result <- reference_executor(reference_environment, method$entrypoint,
                               map$arguments, map$criteria)

  priors <- do.call(rbind, lapply(map$priors, function(x)
    as.data.frame(x[prior_fields], stringsAsFactors = FALSE)))
  rownames(priors) <- NULL
  criteria <- data.frame(
    criterion = names(map$criteria),
    value = as.numeric(unlist(map$criteria, use.names = FALSE)),
    stringsAsFactors = FALSE)
  dependencies <- load_receipt$depends
  if (!length(dependencies)) {
    dependency_path <- NA_character_
    dependency_sha256 <- NA_character_
  } else {
    dependency_path <- names(dependencies)
    dependency_sha256 <- unname(dependencies)
  }
  method_provenance <- data.frame(
    unit = rep(unit, length(dependency_path)),
    status = rep(manifest_unit$status, length(dependency_path)),
    reference_path = rep(method$reference_path, length(dependency_path)),
    entrypoint = rep(method$entrypoint, length(dependency_path)),
    reference_sha256 = rep(load_receipt$sha256, length(dependency_path)),
    dependency_path = dependency_path,
    dependency_sha256 = dependency_sha256,
    stringsAsFactors = FALSE)

  result_frame <- if (is.data.frame(result)) result else
    as.data.frame(result, stringsAsFactors = FALSE)
  scalar_text <- function(x) {
    if (is.logical(x)) return(ifelse(is.na(x), "NA", ifelse(x, "TRUE", "FALSE")))
    if (is.integer(x)) return(ifelse(is.na(x), "NA", as.character(x)))
    if (is.double(x)) return(vapply(x, function(v) {
      if (is.na(v) && !is.nan(v)) return("NA")
      if (is.nan(v)) return("NaN")
      if (is.infinite(v)) return(if (v > 0) "Inf" else "-Inf")
      if (v == 0) v <- 0
      if (v == round(v)) sprintf("%.0f", v) else sprintf("%.17g", v)
    }, character(1)))
    as.character(x)
  }
  computed <- do.call(rbind, lapply(seq_len(nrow(result_frame)), function(i)
    data.frame(result_row = rep(i, ncol(result_frame)),
               field = names(result_frame),
               value = unlist(lapply(result_frame[i, , drop = FALSE], scalar_text),
                              use.names = FALSE),
               stringsAsFactors = FALSE)))
  rownames(computed) <- NULL

  artifact_paths <- unname(paths)
  artifact_sources <- c("covariate_input_file", "covariate_model_file",
                        "covariate_data_file", "covariate_run_config_file",
                        "covariate_map_inputs_file")
  artifacts <- data.frame(
    artifact = c("frozen handoff", "parent model", "parent data",
                 "parent run configuration", "MAP inputs"),
    path = artifact_paths,
    source = artifact_sources,
    stringsAsFactors = FALSE)
  handoff_sha <- file_sha(paths[["handoff"]])
  provenance <- data.frame(
    source = rep(route, 5L),
    role = c("handoff", roles),
    path = artifact_paths,
    declared_sha256 = c(NA_character_, unname(declared_sha)),
    observed_sha256 = c(handoff_sha, unname(observed_sha)),
    producing_analysis_id = rep(handoff$producing_analysis_id, 5L),
    producing_run_id = rep(handoff$producing_run_id, 5L),
    parent_fit_id = rep(handoff$parent_fit_id, 5L),
    owner = rep(handoff$owner, 5L),
    created_at = rep(handoff$created_at, 5L),
    engine_name = rep(handoff$engine$name, 5L),
    engine_version = rep(handoff$engine$version, 5L),
    stringsAsFactors = FALSE)
  diagnostics <- data.frame(
    check = c("reference execution", "R2 engine command"),
    status = c("PASS", "NOT RUN"),
    detail = c("Approved pure-R reference helper executed.",
               "No R2 engine command was constructed or executed."),
    stringsAsFactors = FALSE)
  tables <- list(
    "Frozen input provenance" = provenance,
    "MAP prior list" = priors,
    "Declared criteria" = criteria,
    "Method provenance" = method_provenance,
    "Computed evidence" = computed,
    "Diagnostics" = diagnostics)
  notes <- list(
    authored_note("Agent interpretation status: PENDING. No interpretation has been supplied; computed evidence remains unchanged."),
    authored_note("SME disposition status: PENDING FORMAL SIGNATURE. No disposition has been supplied; this status cannot authorize execution or alter computed evidence."))
  package <- list(analysis = analysis_id, task = "covariate",
                  artifacts = artifacts, tables = tables, notes = notes)
  list(result = result, package = package)
}

# The NONMEM producer returns the handoff and its two tracked companions.
covariate_single_handoff <- function(paths) {
  if (!is.character(paths) || anyNA(paths) || any(!nzchar(paths)))
    stop("covariate: base_model_frozen_yaml must supply valid file paths.", call. = FALSE)
  files <- basename(paths)
  companion <- grepl("^(map-inputs|run-config)-.+\\.yaml$", files)
  # A legacy producer may supply a single handoff with a caller-chosen name.
  if (length(paths) == 1L && !companion) return(paths)
  handoffs <- paths[grepl("^handoff-.+\\.yaml$", files)]
  if (length(handoffs) != 1L)
    stop("covariate: base_model_frozen_yaml has ", length(handoffs),
         " handoff files; exactly one is required for a single method unit.", call. = FALSE)
  suffix <- sub("^handoff-", "", basename(handoffs))
  allowed <- file.path(dirname(handoffs), paste0(c("map-inputs-", "run-config-"), suffix))
  # Normalize the redundant current-directory prefix without resolving symlinks.
  others <- sub("^\\./", "", paths[paths != handoffs])
  if (anyDuplicated(others) || !all(others %in% sub("^\\./", "", allowed)))
    stop("covariate: unexpected or duplicate companion file in base_model_frozen_yaml.",
         call. = FALSE)
  handoffs
}

# The invocation form selects by the hand-off basename that the producer binds.
covariate_select_handoff <- function(handoffs, name) {
  hit <- handoffs[basename(handoffs) == paste0("handoff-", name, ".yaml")]
  if (length(hit) != 1L)
    stop("covariate: invocation '", name, "' matches ", length(hit),
         " hand-off files in base_model_frozen_yaml; exactly one is required.",
         call. = FALSE)
  hit
}

# The hand-off set and the invocation list must be equal, so that no frozen
# hand-off is skipped and none is invented.
covariate_check_coverage <- function(handoffs, names) {
  base <- basename(handoffs)
  found <- sub("^handoff-(.*)\\.yaml$", "\\1",
               base[grepl("^handoff-.*\\.yaml$", base)])
  listed <- function(x) if (length(x)) paste(x, collapse = ", ") else "none"
  if (anyDuplicated(found) || anyDuplicated(names) || !setequal(found, names))
    stop("covariate: the hand-off set of base_model_frozen_yaml must equal the ",
         "invocation list; hand-offs without an invocation: ",
         listed(setdiff(found, names)), "; invocations without a hand-off: ",
         listed(setdiff(names, found)), "; duplicated: ",
         listed(unique(c(found[duplicated(found)], names[duplicated(names)]))),
         ".", call. = FALSE)
  invisible(TRUE)
}

# Merges the per-invocation packages into one gate package. Labels and
# de-duplication only; no arithmetic.
covariate_packages_combine <- function(analysis_id, invocations, results,
                                       handoffs) {
  inv <- names(invocations)
  covariate_check_coverage(handoffs, inv)
  if (!identical(names(results), inv))
    stop("covariate: results must be named by the invocation list, in ",
         "declared order.", call. = FALSE)
  role_of <- c(covariate_input_file = "handoff", covariate_model_file = "model",
               covariate_data_file = "data",
               covariate_run_config_file = "run_config",
               covariate_map_inputs_file = "map_inputs")
  rows <- do.call(rbind, lapply(inv, function(n) {
    a <- results[[n]]$package$artifacts
    if (anyNA(role_of[a$source]))
      stop("covariate: unexpected artifact source in invocation '", n, "'.",
           call. = FALSE)
    a$source <- paste0("covariate_", n, "_", role_of[a$source])
    a$invocations <- rep(n, nrow(a))
    a
  }))
  joined <- function(column) vapply(artifacts$path, function(p)
    paste(rows[[column]][rows$path == p], collapse = ", "), character(1),
    USE.NAMES = FALSE)
  artifacts <- rows[!duplicated(rows$path), , drop = FALSE]
  artifacts$source <- joined("source")
  artifacts$invocations <- joined("invocations")
  rownames(artifacts) <- NULL

  handoff_row <- function(n, field) {
    p <- results[[n]]$package$tables[["Frozen input provenance"]]
    p[[field]][p$role == "handoff"]
  }
  one <- function(field) vapply(inv, handoff_row, character(1), field = field,
                                USE.NAMES = FALSE)
  invocation_list <- data.frame(
    order = seq_along(inv), invocation = inv,
    method_unit = unname(invocations),
    producing_run_id = one("producing_run_id"),
    parent_fit_id = one("parent_fit_id"),
    handoff = one("path"),
    stringsAsFactors = FALSE)
  tables <- c(list("Invocation list" = invocation_list),
              do.call(c, lapply(inv, function(n) {
                t <- results[[n]]$package$tables
                names(t) <- paste0(n, ": ", names(t))
                t
              })))
  notes <- c(unique(do.call(c, lapply(inv, function(n)
    results[[n]]$package$notes))),
    list(authored_note("Each frozen hand-off of base_model_frozen_yaml was evaluated by one invocation of the approved unit it names, in the declared order; the hand-off set and the invocation list were checked equal before this package was built.")))
  list(analysis = analysis_id, task = "covariate", artifacts = artifacts,
       tables = tables, notes = notes)
}

if (!is.null(task_input$invocations)) local({
  ti <- task_input
  refuse <- function(...) stop("covariate: ", ..., call. = FALSE)
  if (!is.null(ti$external_input))
    refuse("external_input is not allowed in the invocation form; it reads ",
           "base_model_frozen_yaml.")
  if (!is.null(ti$method_unit))
    refuse("method_unit is not allowed at the top level of the invocation ",
           "form; each invocation names its own unit.")
  unknown <- setdiff(names(ti), c("upstream_target", "invocations", "engine"))
  if (length(unknown))
    refuse(paste(unknown, collapse = ", "), " is not a field of the ",
           "invocation form; allowed: upstream_target, invocations, engine.")
  if (!identical(ti$upstream_target, "base_model_frozen_yaml"))
    refuse("upstream_target must be exactly 'base_model_frozen_yaml'.")
  inv <- ti$invocations
  if (!is.character(inv) || !length(inv) || is.null(names(inv)))
    refuse("invocations must be a non-empty named character vector.")
  inv_names <- names(inv)
  if (anyNA(inv_names) || anyDuplicated(inv_names) ||
      !all(grepl("^[a-z][a-z0-9_]*$", inv_names)))
    refuse("invocations names must be unique and match ^[a-z][a-z0-9_]*$.")
  units <- c("nested_lrt", "covariate_effect")
  if (anyNA(inv) || !all(inv %in% units))
    refuse("invocations values must each be one of: ",
           paste(units, collapse = ", "), ".")

  extractor <- covariate_handoff_role_path
  compute <- covariate_task_compute
  executor <- covariate_reference_executor
  selector <- covariate_select_handoff
  combine <- covariate_packages_combine
  roles <- c("model", "data", "run_config", "map_inputs")
  out <- list()
  for (n in inv_names) {
    sym <- function(role) as.name(paste0("covariate_", n, "_", role))
    out <- c(out, list(targets::tar_target_raw(
      paste0("covariate_", n, "_handoff"),
      bquote(.(selector)(base_model_frozen_yaml, .(n))), format = "file")))
    for (role in roles)
      out <- c(out, list(targets::tar_target_raw(
        paste0("covariate_", n, "_", role),
        bquote(.(extractor)(.(sym("handoff")), .(role))), format = "file")))
    out <- c(out, list(targets::tar_target_raw(
      paste0("covariate_", n, "_result"),
      bquote(.(compute)(
        analysis_id = .(analysis_id),
        task_input = .(list(method_unit = unname(inv[[n]]),
                            upstream_target = "base_model_frozen_yaml")),
        covariate_input_file = .(sym("handoff")),
        covariate_model_file = .(sym("model")),
        covariate_data_file = .(sym("data")),
        covariate_run_config_file = .(sym("run_config")),
        covariate_map_inputs_file = .(sym("map_inputs")),
        reference_loader = load_reference,
        reference_executor = .(executor))))))
  }
  results_call <- as.call(c(as.name("list"), stats::setNames(
    lapply(inv_names, function(n) as.name(paste0("covariate_", n, "_result"))),
    inv_names)))
  out <- c(out, list(targets::tar_target_raw("covariate_package", bquote(
    .(combine)(.(analysis_id), .(inv), .(results_call), base_model_frozen_yaml)))))
  composed <- vapply(out, function(t) t$settings$name, character(1))
  if (anyDuplicated(composed))
    refuse("composed target names must be unique; duplicated: ",
           paste(unique(composed[duplicated(composed)]), collapse = ", "), ".")
  task_targets <<- out
}) else local({
  ti <- task_input
  if (!is.character(ti$method_unit) || length(ti$method_unit) != 1L ||
      is.na(ti$method_unit) || !nzchar(ti$method_unit))
    stop("covariate: task_input$method_unit must be one non-empty string.",
         call. = FALSE)

  has_external <- !is.null(ti$external_input)
  has_upstream <- !is.null(ti$upstream_target)
  if (has_external == has_upstream)
    stop("covariate: declare exactly one source using external_input or ",
         "upstream_target.", call. = FALSE)
  if (has_external) {
    if (!is.character(ti$external_input) || length(ti$external_input) != 1L ||
        is.na(ti$external_input) || !nzchar(ti$external_input) ||
        !grepl("\\.ya?ml$", ti$external_input, ignore.case = TRUE))
      stop("covariate: external_input must name one non-empty frozen YAML file.",
           call. = FALSE)
    task_input_file("covariate", "external_input", ti$external_input, single = TRUE)
    input_expr <- call("identity", ti$external_input)
  } else {
    if (!is.character(ti$upstream_target) || length(ti$upstream_target) != 1L ||
        is.na(ti$upstream_target) ||
        !identical(ti$upstream_target, "base_model_frozen_yaml"))
      stop("covariate: upstream_target must be exactly ",
           "'base_model_frozen_yaml'.", call. = FALSE)
    input_expr <- bquote(.(covariate_single_handoff)(base_model_frozen_yaml))
  }

  extractor <- covariate_handoff_role_path
  compute <- covariate_task_compute
  executor <- covariate_reference_executor
  role_expr <- function(role)
    bquote(.(extractor)(covariate_input_file, .(role)))
  compute_expr <- bquote(.(compute)(
    analysis_id = .(analysis_id),
    task_input = .(ti),
    covariate_input_file = covariate_input_file,
    covariate_model_file = covariate_model_file,
    covariate_data_file = covariate_data_file,
    covariate_run_config_file = covariate_run_config_file,
    covariate_map_inputs_file = covariate_map_inputs_file,
    reference_loader = load_reference,
    reference_executor = .(executor)))

  task_targets <<- list(
    targets::tar_target_raw("covariate_input_file", input_expr, format = "file"),
    targets::tar_target_raw("covariate_model_file", role_expr("model"),
                            format = "file"),
    targets::tar_target_raw("covariate_data_file", role_expr("data"),
                            format = "file"),
    targets::tar_target_raw("covariate_run_config_file", role_expr("run_config"),
                            format = "file"),
    targets::tar_target_raw("covariate_map_inputs_file", role_expr("map_inputs"),
                            format = "file"),
    targets::tar_target_raw("covariate_result", compute_expr),
    targets::tar_target_raw("covariate_package", quote(covariate_result$package)))
})
