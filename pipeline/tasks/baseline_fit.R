local({
  ti <- task_input
  if (!is.list(ti) || anyDuplicated(names(ti)) || !identical(names(ti), "run_spec"))
    stop("baseline_fit requires exactly run_spec; external replacement is unsupported.", call. = FALSE)
  task_targets <<- list(
    targets::tar_target_raw("baseline_fit_receipt", bquote({
      ref <- load_reference("skills/baseline-fit", "skills/baseline-fit/reference/baseline_fit.R")
      ref$reference_baseline_fit(baseline_input_identity, .(ti$run_spec))
    })),
    targets::tar_target_raw("baseline_raw_summary", quote({
      ref <- load_reference("skills/baseline-fit", "skills/baseline-fit/reference/baseline_fit.R")
      ref$reference_baseline_summary(baseline_fit_receipt)
    })),
    targets::tar_target_raw("baseline_fit_package", bquote({
      f <- baseline_fit_receipt; s <- baseline_raw_summary
      paths <- c(f$listing$path, f$config$staged$path, f$config$bbi$path,
                 f$config$snapshot$path, f$r_packages$path,
                 f$checked_inputs$staged_model$path, f$checked_inputs$staged_data$path)
      list(analysis = .(analysis_id), task = "baseline_fit",
           tables = list("Raw conditional parameters" = s$parameters),
           artifacts = data.frame(artifact = basename(paths), path = paths,
                                  source = "Checked invocation evidence", stringsAsFactors = FALSE),
           notes = list(evidence_note("Conditional interpretation", s$interpretation),
                        evidence_note("Raw standard-error availability", s$stderr_status$status),
                        evidence_note("Covariance reader status", s$covariance_status$status)))
    }))
  )
})
