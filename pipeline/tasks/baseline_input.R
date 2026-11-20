# Work targets only; approval and dominance belong to the composer.
local({
  ti <- task_input
  fields <- c("model", "data", "disposition", "intended_use")
  if (!is.list(ti) || anyDuplicated(names(ti)) || !setequal(names(ti), fields))
    stop("baseline_input requires exactly model, data, disposition and intended_use.", call. = FALSE)
  for (field in fields)
    if (!is.character(ti[[field]]) || length(ti[[field]]) != 1L || is.na(ti[[field]]) ||
        !nzchar(ti[[field]]) || grepl("UNRESOLVED", ti[[field]]))
      stop("baseline_input contains an unresolved field.", call. = FALSE)
  for (field in c("model", "data", "disposition"))
    task_input_file("baseline_input", field, ti[[field]], single = TRUE)
  task_targets <<- list(
    targets::tar_target_raw("baseline_model_file", call("identity", ti$model), format = "file"),
    targets::tar_target_raw("baseline_data_file", call("identity", ti$data), format = "file"),
    targets::tar_target_raw("baseline_disposition_file", call("identity", ti$disposition), format = "file"),
    targets::tar_target_raw("baseline_input_identity", bquote({
      ref <- load_reference("skills/baseline-fit", "skills/baseline-fit/reference/baseline_fit.R")
      model <- ref$.baseline_regular(ref$.baseline_under(baseline_model_file))
      data <- ref$.baseline_regular(ref$.baseline_under(baseline_data_file))
      disposition <- ref$.baseline_regular(ref$.baseline_under(baseline_disposition_file))
      identity <- list(analysis_id = .(analysis_id), model = ref$.baseline_file(model),
                       data = ref$.baseline_file(data), disposition = ref$.baseline_file(disposition),
                       data_binding = ref$.baseline_binding(model, data), intended_use = .(ti$intended_use))
      ref$.baseline_identity(identity)
      identity
    })),
    targets::tar_target_raw("baseline_identity_file", bquote({
      path <- file.path("baseline-identities", paste0(.(analysis_id), ".json"))
      dir.create(dirname(path), showWarnings = FALSE)
      jsonlite::write_json(baseline_input_identity, path, auto_unbox = TRUE, pretty = TRUE)
      path
    }), format = "file"),
    targets::tar_target_raw("baseline_input_package", bquote(list(
      analysis = .(analysis_id), task = "baseline_input", tables = list(),
      artifacts = data.frame(artifact = c("model", "data", "disposition", "identity"),
                             path = c(baseline_model_file, baseline_data_file,
                                      baseline_disposition_file, baseline_identity_file),
                             source = "Declared input bytes; identity binds intended use", stringsAsFactors = FALSE),
      notes = list(evidence_note("Declared intended use", baseline_input_identity$intended_use)))))
  )
})
