# Accept a project-supplied analysis-ready CSV and its specification. The
# project owns any upstream derivations, with their provenance in the plan.
local({
  ti <- task_input
  for (field in c("source", "spec", "map"))
    task_input_file("data_assembly", field, ti[[field]], single = TRUE)
  if (!is.list(ti$expect))
    stop("data_assembly: expect must declare n_subjects, source_rows and lloq.", call. = FALSE)
  for (field in c("n_subjects", "source_rows")) {
    value <- ti$expect[[field]]
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value) ||
        value <= 0 || value != floor(value))
      stop("data_assembly: expect$", field, " must be one positive count.", call. = FALSE)
  }
  lloq <- ti$expect$lloq
  if (!is.numeric(lloq) || length(lloq) != 1L || (!identical(lloq, NA_real_) && !is.finite(lloq)))
    stop("data_assembly: expect$lloq must be a finite number or NA_real_.", call. = FALSE)

  weights <- ti$weights
  optional <- list()
  if (!is.null(weights)) {
    if (!is.list(weights) || !setequal(names(weights), c("baseline", "column", "probs",
                                                         "quantile_type", "labels")) ||
        !is.character(weights$column) || length(weights$column) != 1L ||
        is.na(weights$column) || !nzchar(weights$column) ||
        !is.numeric(weights$probs) || length(weights$probs) != 2L ||
        anyNA(weights$probs) || !(0 < weights$probs[1] && weights$probs[1] < weights$probs[2] &&
                                 weights$probs[2] < 1) ||
        !is.numeric(weights$quantile_type) || length(weights$quantile_type) != 1L ||
        !weights$quantile_type %in% 1:9 || !is.character(weights$labels) ||
        length(weights$labels) != 3L || anyNA(weights$labels) ||
        any(!nzchar(weights$labels)) || anyDuplicated(weights$labels))
      stop("data_assembly: weights must declare baseline, column, probs, quantile_type and three labels.",
           call. = FALSE)
    task_input_file("data_assembly", "weights$baseline", weights$baseline, single = TRUE)
    optional <- list(
      targets::tar_target_raw("da_baseline_weights_file", call("identity", weights$baseline),
                              format = "file"),
      targets::tar_target_raw("da_weight_constants", bquote({
        base <- utils::read.csv(da_baseline_weights_file)
        ref <- load_reference("skills/data-assembly",
                              "skills/data-assembly/reference/weight_constants.R")
        value <- ref$reference_weight_constants(base[[.(weights$column)]],
                                                .(weights$probs), .(weights$quantile_type))
        data.frame(quantity = .(weights$labels), value_kg = c(value$median, value$lower, value$upper),
                   quantile_type = value$quantile_type)
      })))
  }
  if (!is.null(ti$design_constants)) {
    task_input_file("data_assembly", "design_constants", ti$design_constants, single = TRUE)
    optional <- c(optional, list(
      targets::tar_target_raw("da_design_constants_file", call("identity", ti$design_constants),
                              format = "file"),
      targets::tar_target_raw("da_design_constants", quote({
        design <- utils::read.csv(da_design_constants_file)
        if (!is.character(design$quantity) || !length(design$quantity) ||
            anyNA(design$quantity) || any(!nzchar(design$quantity)) || anyDuplicated(design$quantity) ||
            !is.numeric(design$value) || any(!is.finite(design$value)))
          stop("data_assembly: design_constants needs unique quantity labels and finite values.")
        design
      }))))
  }
  constants <- as.call(c(list(quote(list)),
    if (!is.null(weights)) alist("Weight constants" = da_weight_constants),
    if (!is.null(ti$design_constants)) alist("Declared design constants" = da_design_constants)))
  artifacts <- as.call(c(list(quote(c), quote(dataset_file), quote(da_spec_file), quote(da_map_file)),
    if (!is.null(weights)) list(quote(da_baseline_weights_file)),
    if (!is.null(ti$design_constants)) list(quote(da_design_constants_file))))
  task_targets <<- c(optional, list(
    targets::tar_target_raw("da_map_file", call("identity", ti$map), format = "file"),
    targets::tar_target_raw("da_spec_file", call("identity", ti$spec), format = "file"),
    targets::tar_target_raw("dataset_file", call("identity", ti$source), format = "file"),
    targets::tar_target_raw("dataset", quote({
      da_map_file
      read_external_dataset(dataset_file, da_spec_file)
    })),
    targets::tar_target_raw("da_qc", bquote({
      expected <- .(ti$expect)
      expected$reconciled <- nrow(dataset) == expected$source_rows
      expected$recon_detail <- sprintf("Read %d rows from the declared source file.", nrow(dataset))
      run_qc_battery(dataset, expect = expected, spec = da_spec_file)
    })),
    targets::tar_target_raw("da_qc_assert", quote(assert_qc_pass(da_qc))),
    targets::tar_target_raw("data_assembly_package", bquote({
      paths <- .(artifacts)
      hashes <- vapply(paths, function(path) digest::digest(file = path, algo = "sha256"), "")
      if (!identical(unname(hashes[1:2]),
                     c(attr(dataset, "external_source_sha256"), attr(dataset, "external_spec_sha256"))))
        stop("data_assembly: input bytes changed after the dataset was checked.", call. = FALSE)
      list(analysis = .(analysis_id), task = "data_assembly",
           tables = c(list("Dataset quality checks" = da_qc,
                           "Declared inputs" = data.frame(path = paths, sha256 = hashes, row.names = NULL)),
                      .(constants)),
           artifacts = data.frame(artifact = c("Analysis-ready dataset", "Dataset specification",
                                               "Analysis plan",
                                               .(if (!is.null(weights)) "Baseline weights"),
                                               .(if (!is.null(ti$design_constants)) "Design constants")),
                                  path = paths, source = "Declared project input"),
           notes = list(authored_note(paste(
             "The dataset was checked against its specification and the declared quality checks.",
             "The source and specification are tracked as file targets."))))
    }))
  ))
})
