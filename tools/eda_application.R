# The public EDA application assembles declarations and packages method outputs.
# Record selection, statistics, drawing and validation live in the reference.
eda_application_reference <- function(path) {
  load_reference("skills/eda-standards", path)
}

eda_application_registry <- function() {
  entry <- function(kind, file, rows, ..., tier = "core", y_transform = "identity") {
    list(kind = kind, file = file, rows = rows, tier = tier, w = 8, h = 6,
         x_transform = "identity", y_transform = y_transform, ...)
  }
  profiles <- function(file, ..., y_transform = "identity", tier = "core",
                       y = "dv", colour = list(role = "dose_group", when_absent = "group")) {
    entry("profiles", file, "observations", x = "time", y = y, group = "id",
          colour = colour, y_transform = y_transform, tier = tier, ...)
  }
  bins <- list(rule = "equal_width", n = 10L)
  list(
    profiles_linear = profiles("profiles-linear.pdf",
      smooth = list(method = "loess", span = 1, degree = 1L)),
    profiles_log = profiles("profiles-log.pdf", y_transform = "log10"),
    individual_profiles = entry("individual_profiles", "individual-profiles.pdf",
      "observations", x = "time", y = "dv", facet = "id",
      markers = list(rows = "doses", x = "time"), page = list(per_page = 12L)),
    covariate_histograms = entry("histograms", "covariate-histograms.pdf",
      "subjects", variables = "continuous", bins = bins),
    covariate_pairs = entry("pairs", "covariate-pairs.pdf", "subjects",
      variables = "continuous", bins = bins,
      correlation = list(method = "pearson", use = "complete.obs", digits = 2L)),
    covariate_boxplots = entry("boxplots", "covariate-boxplots.pdf", "subjects",
      variables = "continuous", by = "categorical", requires = "categorical",
      box = list(quantile_type = 7L, whisker = 1.5)),
    dose_normalized = profiles("dose-normalized.pdf", tier = "catalog", y = "derived",
      derive = list(kind = "normalize", into = "EDA_NORMALIZED", numerator = "dv",
                    denominator = "dose", scale = 1, label = "{dv} per {dose}")),
    covariate_stratified = profiles("covariate-stratified.pdf", tier = "catalog",
      colour = list(role = "derived"),
      derive = list(kind = "quantile_bins", into = "EDA_STRATUM", role = "stratify",
                    probs = c(0, 1 / 3, 2 / 3, 1), breaks_rows = "subjects",
                    quantile_type = 7L)))
}

eda_application_role_flags <- function() {
  c(id = "eda_id", exclusion = "eda_exclusion", event = "eda_event",
    time = "eda_time", dv = "eda_dv", blq = "eda_blq",
    continuous = "eda_covariate_continuous", categorical = "eda_covariate_categorical",
    study = "eda_study", dose_group = "eda_dose_group",
    dose = "eda_dose", stratify = "eda_stratify")
}

eda_application_columns <- function(spec, registry, catalog) {
  flags <- eda_application_role_flags()
  # Use the roles in active derivation declarations, including custom entries.
  active <- Filter(function(p) identical(p$tier, "core"), registry)
  active <- c(active, registry[intersect(catalog, names(registry))])
  derived <- unlist(lapply(active, function(p) {
    c(p$derive$role, p$derive$numerator, p$derive$denominator)
  }), use.names = FALSE)
  required <- c("id", "exclusion", "event", "time", "dv", "blq", "continuous", derived)
  rules <- lapply(names(flags), function(role) {
    list(flag = flags[[role]], min = as.integer(role %in% required),
         max = if (role %in% c("continuous", "categorical")) Inf else 1L)
  })
  ys_resolve_roles(spec, stats::setNames(rules, names(flags)))
}

eda_application_view <- function(ref, input, spec, cols) {
  ref$reference_eda_view(input, id_col = cols$id, exclusion_col = cols$exclusion,
    event_col = cols$event, observation_values = ys_decode_values(spec, cols$event, "observation"),
    dose_values = ys_decode_values(spec, cols$event, "dose"))
}

eda_application_decodes <- function(spec) {
  cols <- intersect(names(spec$values), names(spec$decode))
  stats::setNames(lapply(cols, function(col) {
    list(value = spec$values[[col]], decode = spec$decode[[col]])
  }), cols)
}

eda_application_parameters <- function(component, parameters) {
  data.frame(component = rep(component, nrow(parameters)), parameters)
}

eda_application_tables <- function(ref, view, cols, spec, out_dir) {
  blq_values <- ys_decode_values(spec, cols$blq, "below LLOQ")
  inventory <- function(group = character()) {
    ref$reference_eda_inventory(view, blq_col = cols$blq, blq_values = blq_values,
                                group_cols = group)
  }
  summaries <- list(
    inventory = inventory(),
    inventory_by_study = if (length(cols$study)) inventory(cols$study),
    inventory_by_dose_group = if (length(cols$dose_group)) inventory(cols$dose_group),
    continuous_covariates = ref$reference_eda_continuous_summary(view, cols$continuous),
    categorical_covariates = if (length(cols$categorical))
      ref$reference_eda_categorical_summary(view, cols$categorical,
                                            decodes = eda_application_decodes(spec)))
  optional <- c(inventory_by_study = "eda_study", inventory_by_dose_group = "eda_dose_group",
                categorical_covariates = "eda_covariate_categorical")
  absent <- vapply(summaries, is.null, logical(1))
  status <- data.frame(deliverable = ref$EDA_CORE_TABLES,
    status = ifelse(absent, "inapplicable", "computed"),
    reason = ifelse(absent, paste0("The specification flags no column as `",
                                  optional[names(summaries)], "`."), ""),
    row.names = NULL)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  computed <- summaries[!absent]
  tables <- lapply(computed, `[[`, "table")
  files <- unlist(lapply(names(tables), function(nm) {
    csv <- file.path(out_dir, paste0(nm, ".csv"))
    tex <- paste0(nm, ".tex")
    utils::write.csv(tables[[nm]], csv, row.names = FALSE)
    pmtables::stable_save(pmtables::stable(tables[[nm]]), file = tex, dir = out_dir)
    c(csv, file.path(out_dir, tex))
  }), use.names = FALSE)
  list(inventory = tables$inventory, cont_summary = tables$continuous_covariates,
       cat_summary_display = if (absent[["categorical_covariates"]])
         status[names(absent) == "categorical_covariates", ] else tables$categorical_covariates,
       grouped_inventory = list("Data inventory by study" = tables$inventory_by_study,
                                "Data inventory by dose group" = tables$inventory_by_dose_group),
       status = status, files = files,
       parameters = do.call(rbind, lapply(names(computed), function(nm) {
         eda_application_parameters(nm, computed[[nm]]$parameters)
       })))
}

eda_application_active <- function(ref, registry, catalog) {
  index <- ref$reference_eda_registry_check(registry)
  if (anyDuplicated(index$plot) || any(index$file != basename(index$file)) ||
      any(grepl("\\\\", index$file)))
    stop("EDA registry entries must have unique names and use PDF basenames.", call. = FALSE)
  ref$reference_eda_registry_active(index, catalog)
}

eda_application_figures <- function(ref, view, cols, spec, registry, catalog, out_dir) {
  active <- eda_application_active(ref, registry, catalog)
  labels <- ifelse(nzchar(spec$unit), paste0(spec$short, " (", spec$unit, ")"), spec$short)
  names(labels) <- names(spec$short)
  flags <- eda_application_role_flags()
  decodes <- eda_application_decodes(spec)
  rendered <- data.frame(plot = character(), file = character(), path = character(),
    rows = character(), row_digest = character(), w = numeric(), h = numeric(),
    pages = integer(), sha256 = character())
  inapplicable <- data.frame(plot = character(), reason = character())
  notes <- inapplicable
  derived <- data.frame(plot = character(), transform = character(), column = character(),
                        declared = character(), computed = character())
  layers <- data.frame(plot = character())
  reconciliation <- data.frame(plot = character())
  parameters <- data.frame(component = character(), parameter = character(), value = character())
  for (nm in active) {
    entry <- registry[[nm]]
    built <- ref$reference_eda_build_figure(view, entry, nm, cols, labels, flags, decodes)
    if (length(built$notes))
      notes <- rbind(notes, data.frame(plot = nm, reason = built$notes))
    derived <- rbind(derived, built$derived)
    parameters <- rbind(parameters, eda_application_parameters(nm, built$parameters))
    if (!is.null(built$inapplicable)) {
      inapplicable <- rbind(inapplicable, data.frame(plot = nm, reason = built$inapplicable))
      next
    }
    path <- file.path(out_dir, entry$file)
    written <- ref$reference_eda_write_figure(built, entry, nm, path)
    checked <- ref$reference_eda_figure_check(view, built, entry, nm, path, written$sha256)
    rendered <- rbind(rendered, data.frame(
      plot = nm, file = entry$file, path = checked$file$path, rows = entry$rows,
      row_digest = checked$row_digest, w = checked$file$width_in, h = checked$file$height_in,
      pages = checked$file$pages, sha256 = checked$file$sha256))
    layers <- rbind(layers, data.frame(plot = nm, checked$layers))
    reconciliation <- rbind(reconciliation, data.frame(plot = nm, checked$reconciliation))
  }
  list(rendered = rendered, inapplicable = inapplicable, notes = notes, derived = derived,
       layers = layers, reconciliation = reconciliation, parameters = parameters)
}

eda_application_log_audits <- function(ref, view, cols, spec, registry, catalog) {
  active <- eda_application_active(ref, registry, catalog)
  requests <- list()
  for (nm in active) {
    entry <- registry[[nm]]
    if (any(vapply(entry$requires, function(role) !length(cols[[role]]), logical(1))))
      next
    for (axis in c("x", "y")) {
      if (!identical(entry[[paste0(axis, "_transform")]], "log10")) next
      column <- cols[[entry[[axis]]]]
      if (length(column) != 1L)
        stop("EDA log axis must resolve to one column for plot '", nm, "'.", call. = FALSE)
      requests[[length(requests) + 1L]] <- list(plot = nm, axis = axis, rows = entry$rows,
                                                column = column)
    }
  }
  ref$reference_eda_log_axis_audit_table(view, requests, blq_col = cols$blq,
    blq_values = ys_decode_values(spec, cols$blq, "below LLOQ"))
}

eda_application_method_parameters <- function(view, tables, figures) {
  rbind(eda_application_parameters("record selection", view$parameters),
        tables$parameters, figures$parameters)
}
