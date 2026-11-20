# Base model for a declared NONMEM project.
# Composition owns the approval and dependency edges.
if (identical(task_input$engine, "nonmem")) local({
  ti <- task_input
  refuse <- function(field, ...)
    stop("inputs$base_model$", field, ": ", ..., " Declare it in analyses/",
         analysis_id, ".R; the module does not invent settings.", call. = FALSE)
  str1 <- function(x) is.character(x) && length(x) == 1L && !is.na(x)
  count1 <- function(x) is.numeric(x) && length(x) == 1L && !is.na(x) &&
    x > 0 && x == round(x)

  # --- compose-time validation (Section 7.3); each stop names the field ---
  need <- c("engine", "model_dir", "map", "seed", "npde_nsim", "run_budget",
            "gof", "runs")
  missing_in <- setdiff(need, names(ti))
  if (length(missing_in)) refuse(paste(missing_in, collapse = ", "), "missing.")
  if (!count1(ti[["seed"]])) refuse("seed", "must be a single positive integer.")
  if (!count1(ti[["npde_nsim"]]))
    refuse("npde_nsim", "must be a single positive integer.")
  budget <- ti[["run_budget"]]
  if (!is.list(budget) || !all(c("used", "max") %in% names(budget)))
    refuse("run_budget", "must be list(used = , max = ) per MAP Section 5.")
  if (!(is.numeric(budget[["used"]]) && length(budget[["used"]]) == 1L &&
        is.finite(budget[["used"]]) && budget[["used"]] >= 0 &&
        budget[["used"]] == round(budget[["used"]])))
    refuse("run_budget$used", "must be a single non-negative whole number.")
  if (!count1(budget[["max"]]))
    refuse("run_budget$max", "must be a single positive whole number.")
  gof <- ti[["gof"]]
  if (!is.list(gof) || !all(vapply(c("cont_cov", "cat_cov", "strata"),
                                   function(f) str1(gof[[f]]), logical(1))))
    refuse("gof", "must be list(cont_cov = , cat_cov = , strata = ), one column name each.")
  runs <- ti[["runs"]]
  if (!is.list(runs) || !length(runs)) refuse("runs", "must be a non-empty list.")
  run_names <- vapply(runs, function(e)
    if (str1(e[["run"]])) e[["run"]] else NA_character_, character(1))
  if (anyNA(run_names) || !all(grepl("^[0-9]{3}$", run_names)))
    refuse("runs[[i]]$run", "every run name must be three digits.")
  if (anyDuplicated(run_names))
    refuse("runs[[i]]$run", "run names must be unique; ",
           paste(unique(run_names[duplicated(run_names)]), collapse = ", "),
           " is declared twice.")
  statuses <- c("launch", "stopped-before-estimation", "failed-counted")
  for (i in seq_along(runs)) {
    e <- runs[[i]]
    at <- function(f) paste0("runs (run ", run_names[i], ") ", f)
    if (!str1(e[["step"]]) || !nzchar(e[["step"]]))
      refuse(at("step"), "every entry carries its step, a non-empty string.")
    if (!str1(e[["based_on"]]) || !e[["based_on"]] %in% c("", run_names[-i]))
      refuse(at("based_on"), "must be empty or another declared run (not the entry's own).")
    if (!str1(e[["retry_of"]]) ||
        !e[["retry_of"]] %in% c("", run_names[seq_len(i - 1L)]))
      refuse(at("retry_of"), "must be empty or a run declared earlier in the list.")
    if (!str1(e[["status"]]) || !e[["status"]] %in% statuses)
      refuse(at("status"), "must be one of ", paste(statuses, collapse = ", "), ".")
    if (e[["status"]] != "launch" &&
        (!is.character(e[["evidence"]]) || !length(e[["evidence"]]) ||
         anyNA(e[["evidence"]]) || !all(nzchar(e[["evidence"]]))))
      refuse(at("evidence"), "a ", e[["status"]], " entry carries the verbatim ",
             "evidence lines of Section 13.")
    if (e[["status"]] == "launch") {
      if (!str1(e[["hypothesis"]]) || !nzchar(e[["hypothesis"]]))
        refuse(at("hypothesis"), "a launch entry carries its hypothesis.")
      et <- e[["etas"]]
      if (!is.character(et) || !length(et) || is.null(names(et)) ||
          any(!nzchar(names(et))) || anyDuplicated(names(et)))
        refuse(at("etas"), "must be a named, non-empty character vector with ",
               "unique names.")
    }
  }
  status_of <- vapply(runs, function(e) e[["status"]], character(1))
  launch <- run_names[status_of == "launch"]
  for (f in c("comparisons", "exponent", "covariate_comparisons")) {
    if (is.null(ti[[f]])) next
    items <- if (f == "exponent") list(ti[[f]]) else ti[[f]]
    named <- unlist(lapply(items, function(it)
      unlist(it[intersect(c("parent", "child", "run"), names(it))])))
    bad <- setdiff(named, launch)
    if (length(bad))
      refuse(f, "names run(s) that are not declared launch runs: ",
             paste(bad, collapse = ", "), ".")
  }
  # The B3b rule-table fields (Section 8); each refusal names the field.
  num1 <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x)
  has <- function(item, f) if (f %in% c("criterion_dofv", "df", "alpha", "rse_bound_pct"))
    num1(item[[f]]) else str1(item[[f]]) && nzchar(item[[f]])
  cmps <- ti[["comparisons"]]
  if (!is.null(cmps)) {
    if (!is.list(cmps) || !length(cmps) || is.null(names(cmps)) ||
        any(!nzchar(names(cmps))) || anyDuplicated(names(cmps)))
      refuse("comparisons", "must be a non-empty named list with unique comparison names.")
    for (nm in names(cmps)) {
      if (!is.list(cmps[[nm]]))
        refuse(paste0("comparisons (", nm, ")"), "each comparison is a list.")
      for (f in c("parent", "child", "criterion_dofv", "df", "alpha", "rule"))
        if (!has(cmps[[nm]], f))
          refuse(paste0("comparisons (", nm, ") ", f), "every comparison carries parent, ",
                 "child and rule (non-empty strings) and criterion_dofv, df and alpha ",
                 "(single finite numbers).")
      if (identical(cmps[[nm]][["parent"]], cmps[[nm]][["child"]]))
        refuse(paste0("comparisons (", nm, ")"), "parent and child are the same run; ",
               "a run cannot be compared with itself.")
    }
  }
  expo <- ti[["exponent"]]
  if (!is.null(expo) && !is.list(expo))
    refuse("exponent", "the exponent is a list.")
  if (!is.null(expo))
    for (f in c("run", "parameter", "rse_bound_pct"))
      if (!has(expo, f))
        refuse(paste("exponent", f), "the exponent carries run and parameter (non-empty ",
               "strings) and rse_bound_pct (a single finite number).")
  # The B3c hand-off fields (Section 9; design Section 5); each refusal names
  # the field. Comparison names become file names, so they are restricted.
  ccmps <- ti[["covariate_comparisons"]]
  if (!is.null(ccmps)) {
    if (!is.list(ccmps) || !length(ccmps) || is.null(names(ccmps)) ||
        !all(grepl("^[A-Za-z0-9_]+$", names(ccmps))) || anyDuplicated(names(ccmps)))
      refuse("covariate_comparisons", "must be a non-empty list with unique names of ",
             "letters, digits and underscores (they name the hand-off files).")
    if (!str1(ti[["covariate_dir"]]) || !nzchar(ti[["covariate_dir"]]))
      refuse("covariate_dir", "the hand-off directory, a non-empty string, is required ",
             "with covariate_comparisons.")
    pri <- ti[["covariate_priors"]]
    pf <- c("covariate", "parameter", "direction", "mechanism")
    if (!is.list(pri) || !length(pri) ||
        !all(vapply(pri, function(p) is.list(p) && identical(sort(names(p)), sort(pf)) &&
                      all(vapply(pf, function(f) str1(p[[f]]) && nzchar(p[[f]]), logical(1))),
                    logical(1))))
      refuse("covariate_priors", "the MAP 4.5 prior list: a non-empty list of rows, each ",
             "with exactly covariate, parameter, direction and mechanism (non-empty strings).")
    for (nm in names(ccmps)) {
      cc <- ccmps[[nm]]
      at <- function(f) paste0("covariate_comparisons (", nm, ")", f)
      if (!is.list(cc)) refuse(at(""), "each comparison is a list.")
      units <- list(nested_lrt = c("parent", "child", "test", "criterion_dofv", "df", "alpha"),
                    covariate_effect = c("run", "band_lower", "band_upper"))
      if (!str1(cc[["unit"]]) || !cc[["unit"]] %in% names(units))
        refuse(at(" unit"), "must be one of ", paste(names(units), collapse = ", "), ".")
      for (f in units[[cc[["unit"]]]])
        if (!(if (f %in% c("criterion_dofv", "df", "alpha", "band_lower", "band_upper"))
          num1(cc[[f]]) else has(cc, f)))
          refuse(at(paste0(" ", f)), "a ", cc[["unit"]], " comparison carries ",
                 paste(units[[cc[["unit"]]]], collapse = ", "), " and prior.")
      p <- cc[["prior"]]
      if (!is.numeric(p) || !length(p) || any(!is.finite(p)) || any(p != round(p)) ||
          any(p < 1) || any(p > length(pri)))
        refuse(at(" prior"), "must be one or more whole numbers indexing covariate_priors.")
      if (cc[["unit"]] == "nested_lrt") {
        if (!cc[["test"]] %in% c("forward", "backward"))
          refuse(at(" test"), "must be forward or backward.")
        if (identical(cc[["parent"]], cc[["child"]]))
          refuse(at(""), "parent and child are the same run; a run cannot be compared ",
                 "with itself.")
      } else {
        w <- cc[["wt_points"]]
        if (!is.character(w) || length(w) != 2L || anyNA(w) || !all(nzchar(w)))
          refuse(at(" wt_points"), "the two da_weight_constants quantities (low, high), ",
                 "for example c(\"WT05\", \"WT95\").")
        ef <- cc[["exponent_fixed"]]
        if (!is.null(ef) && !(is.numeric(ef) && length(ef) && all(is.finite(ef))))
          refuse(at(" exponent_fixed"), "must be one or more finite numbers.")
        if (is.null(ef) && !identical(expo[["run"]], cc[["run"]]))
          refuse(at(" exponent"), "without exponent_fixed the exponent is read from the ",
                 "declared exponent row, which must name run ", cc[["run"]], ".")
      }
    }
  }
  # The run-budget refusal (Section 13): launch and failed-counted entries count.
  nm_budget_stop(budget[["used"]],
                 sum(nm_counted(status_of)),
                 budget[["max"]])

  # --- targets (decision 4, B3a rows) ---
  model_dir <- ti[["model_dir"]]
  task_input_file("base_model", "map", ti[["map"]], single = TRUE)
  task_input_file("base_model", "model_dir/run-notes.md", file.path(model_dir, "run-notes.md"),
                  single = TRUE)
  task_input_file("base_model", "model_dir/run-config.yaml", file.path(model_dir, "run-config.yaml"),
                  single = TRUE)
  task_input_file("base_model", "runs", file.path(model_dir, paste0(run_names, ".mod")))
  task_input_file("base_model", "runs/diffs", file.path(model_dir, "diffs", paste0(run_names, ".diff")))
  failed <- run_names[status_of == "failed-counted"]
  for (ext in c(".lst", ".ext"))
    if (length(failed))
      task_input_file("base_model", "failed-counted runs", file.path(model_dir, failed,
                                                                     paste0(failed, ext)))
  out_dir <- file.path(model_dir, "outputs")
  sym <- function(...) as.name(paste0(...))
  file_target <- function(name, path)
    targets::tar_target_raw(name, call("identity", path), format = "file")
  row <- function(artifact, path, source)
    bquote(data.frame(artifact = .(artifact), path = .(path), source = .(source),
                      stringsAsFactors = FALSE))

  entry_targets <- list()
  artifact_rows <- list()
  for (e in runs) {
    r <- e[["run"]]
    st <- e[["status"]]
    ctl <- sym("bm_ctl_", r, "_file")
    dif <- sym("bm_diff_", r, "_file")
    entry_targets <- c(entry_targets, list(
      file_target(as.character(ctl), file.path(model_dir, paste0(r, ".mod"))),
      file_target(as.character(dif), file.path(model_dir, "diffs", paste0(r, ".diff")))))
    artifact_rows <- c(artifact_rows, list(
      row(paste0("control stream, run ", r, " (", st, ")"), ctl,
          "authored by the pmx-modeler before the run; file target"),
      row(paste0("stream diff against its parent, run ", r), dif,
          "authored by the pmx-modeler before the run; file target")))
    if (st == "failed-counted") {
      lst <- sym("bm_lst_", r, "_file")
      ext <- sym("bm_ext_", r, "_file")
      entry_targets <- c(entry_targets, list(
        file_target(as.character(lst), file.path(model_dir, r, paste0(r, ".lst"))),
        file_target(as.character(ext), file.path(model_dir, r, paste0(r, ".ext")))))
      artifact_rows <- c(artifact_rows, list(
        row(paste0("NONMEM listing, run ", r), lst, "written by NONMEM; file target"),
        row(paste0("NONMEM .ext, run ", r), ext, "written by NONMEM; file target")))
    }
    if (st == "launch") {
      fit <- sym("bm_fit_", r)
      res <- sym("bm_res_", r)
      gof_pdf <- sym("bm_gof_", r)
      entry_targets <- c(entry_targets, list(
        # The fit names its stream, the data file and the reference only.
        targets::tar_target_raw(as.character(fit), bquote({
          .(ctl)
          dataset_file
          load_reference("skills/nonmem-estimation", bm_nm_reference,
                         envir = environment())
          reference_nm_fit(.(model_dir), .(r), based_on = .(e[["based_on"]]),
                           hypothesis = .(e[["hypothesis"]]), wait = TRUE,
                           bbi_path = NULL)
        })),
        targets::tar_target_raw(as.character(res), bquote({
          load_reference("skills/nonmem-estimation", bm_nm_reference,
                         envir = environment())
          results <- reference_nm_results(.(fit)$mod)
          frame <- nm_observation_frame(
            results$residuals, .(file.path(model_dir, r, paste0(r, ".mod"))),
            results$n_obs, npde_nsim = .(ti[["npde_nsim"]]), seed = .(ti[["seed"]]))
          list(results = results, frame = frame,
               record = nm_run_record(.(fit), results, frame),
               no_bbr_heuristic_raised = .(fit)$converged)
        })),
        targets::tar_target_raw(as.character(gof_pdf), bquote({
          load_reference("skills/diagnostics", bm_gof_reference, envir = environment())
          frame <- .(res)$frame
          # pmplots' categorical panels take a factor, not a 0/1 number.
          frame[[.(gof$cat_cov)]] <- factor(frame[[.(gof$cat_cov)]])
          etas <- .(e[["etas"]])
          panels <- reference_gof_panel(frame, etas = names(etas),
                                        cont_cov = .(gof$cont_cov),
                                        cat_cov = .(gof$cat_cov),
                                        strata = .(gof$strata))
          ebe <- reference_gof_ebe(frame, etas = names(etas),
                                   shrinkage = nm_eta_shrinkage(.(res)$results$params, etas))
          nm_write_panels(c(panels, list(eta_hist = ebe$eta_hist)),
                          .(file.path(out_dir, paste0("gof-", r, ".pdf"))))
        }), format = "file")))
      run_dir <- bquote(file.path(.(model_dir), basename(dirname(.(fit)$config_json))))
      artifact_rows <- c(artifact_rows, list(
        row(paste0("NONMEM listing, run ", r),
            bquote(file.path(.(run_dir), .(paste0(r, ".lst")))),
            "written by NONMEM in the run directory the fit recorded"),
        row(paste0("NONMEM .ext, run ", r),
            bquote(file.path(.(run_dir), .(paste0(r, ".ext")))),
            "written by NONMEM in the run directory the fit recorded"),
        row(paste0("GOF and EBE panels, run ", r), gof_pdf,
            "target bm_gof (skills/diagnostics/reference/gof.R)")))
    }
  }
  res_list <- as.call(c(as.name("list"), lapply(launch, function(r) sym("bm_res_", r))))
  artifact_rows <- c(artifact_rows, list(
    row("run report (Markdown)", quote(bm_run_report), "target bm_run_report (tools/run_report.R)"),
    row("run notes (hypothesis and expected before each run, observed after)",
        quote(bm_run_notes_file), "authored by the pmx-modeler; file target"),
    row("bbi run configuration (SME-approved)", quote(bm_run_config_file),
        "approved by the SME before the first fit; file target"),
    row("NONMEM data file", quote(dataset_file), "data-assembly task (signed gate)")))

  # --- B3b rule tables (Section 8), composed only when declared, so the B3a
  # composition is unchanged otherwise. Each splices only the result targets
  # of the runs it names. The reference_nested_lrt call sits in the target
  # command: a tools/ function cannot see a reference loaded here.
  rule_targets <- list()
  extra_tables <- list()
  res_of <- function(rs) as.call(c(as.name("list"), lapply(rs, function(r) sym("bm_res_", r))))
  if (!is.null(cmps)) {
    rule_targets <- c(rule_targets, list(
      targets::tar_target_raw("bm_lrt_reference",
                              call("identity", "skills/covariate-priors/reference/nested_lrt.R"),
                              format = "file"),
      targets::tar_target_raw("bm_lrt", bquote({
        load_reference("skills/covariate-priors", bm_lrt_reference, envir = environment())
        res_list <- .(res_of(unique(unlist(lapply(cmps, function(x)
          c(x[["parent"]], x[["child"]]))))))
        cmps <- .(cmps)
        out <- do.call(rbind, lapply(names(cmps), function(nm) {
          cmp <- cmps[[nm]]
          ofv <- nm_lrt_inputs(cmp, res_list)
          unit <- reference_nested_lrt(ofv$ofv_parent, ofv$ofv_child, cmp[["criterion_dofv"]],
                                       cmp[["df"]], cmp[["alpha"]])
          data.frame(comparison = nm, parent = cmp[["parent"]], child = cmp[["child"]],
                     rule = cmp[["rule"]], unit, stringsAsFactors = FALSE)
        }))
        rownames(out) <- NULL
        out
      }))))
    extra_tables <- c(extra_tables, list(
      "Nested likelihood-ratio tests (reference_nested_lrt, skills/covariate-priors): residual error, W1, IIV covariance; dOFV and the declared criterion, NA for a failed run" =
        quote(bm_lrt)))
  }
  if (!is.null(expo)) {
    rule_targets <- c(rule_targets, list(
      targets::tar_target_raw("bm_exponent",
                              bquote(nm_exponent_row(.(expo), .(res_of(expo[["run"]])))))))
    extra_tables <- c(extra_tables, list(
      "Estimated weight exponent beside its declared RSE bound (MAP 4.1 W1 and E2); within_bound is a comparison, nothing is computed" =
        quote(bm_exponent)))
  }
  # --- B3c frozen hand-offs (Section 9), composed only when declared. The
  # command names the result targets of the runs the comparisons name, the
  # parents' stream targets, dataset_file and da_weight_constants; never a
  # fit target and never bm_map_file, so a regeneration re-runs no fit.
  if (!is.null(ccmps)) {
    named <- unique(unlist(lapply(ccmps, function(x)
      c(x[["parent"]], x[["child"]], x[["run"]]))))
    uses_expo <- any(vapply(ccmps, function(x) is.null(x[["exponent_fixed"]]) &&
                              identical(x[["unit"]], "covariate_effect"), logical(1)))
    stream_runs <- unique(vapply(ccmps, function(x)
      if (identical(x[["unit"]], "nested_lrt")) x[["parent"]] else x[["run"]], ""))
    streams <- as.call(c(as.name("c"), stats::setNames(
      lapply(stream_runs, function(r) sym("bm_ctl_", r, "_file")), stream_runs)))
    rule_targets <- c(rule_targets, list(
      targets::tar_target_raw("base_model_frozen_yaml", bquote(nm_write_handoffs(
        comparisons = .(ccmps), res_list = .(res_of(named)), streams = .(streams),
        data_file = dataset_file, weight_constants = da_weight_constants,
        exponent = .(if (uses_expo) expo), priors = .(ti[["covariate_priors"]]),
        model_dir = .(model_dir), out_dir = .(ti[["covariate_dir"]]),
        analysis_id = .(analysis_id),
        owner = "target base_model_frozen_yaml (pmx-modeler, base-model stage)")),
        format = "file")))
    k <- 0L
    for (nm in names(ccmps)) {
      artifact_rows <- c(artifact_rows, list(
        row(paste0("frozen covariate hand-off, ", nm), bquote(base_model_frozen_yaml[.(k + 1L)]),
            "target base_model_frozen_yaml (tools/nonmem_frames.R nm_write_handoffs)"),
        row(paste0("MAP inputs of the hand-off, ", nm), bquote(base_model_frozen_yaml[.(k + 2L)]),
            "target base_model_frozen_yaml (tools/nonmem_frames.R nm_write_handoffs)"),
        row(paste0("run configuration and provenance of the hand-off, ", nm),
            bquote(base_model_frozen_yaml[.(k + 3L)]),
            "target base_model_frozen_yaml (tools/nonmem_frames.R nm_write_handoffs)")))
      k <- k + 3L
    }
  }

  task_targets <<- c(list(
    targets::tar_target_raw("bm_map_file", call("identity", ti[["map"]]), format = "file"),
    targets::tar_target_raw("bm_nm_reference",
                            call("identity", "skills/nonmem-estimation/reference/bbr_estimation.R"),
                            format = "file"),
    targets::tar_target_raw("bm_gof_reference",
                            call("identity", "skills/diagnostics/reference/gof.R"),
                            format = "file")),
    entry_targets,
    list(
    targets::tar_target_raw("bm_records", as.call(c(as.name("list"), lapply(launch, function(r)
      call("$", sym("bm_res_", r), as.name("record")))))),
    targets::tar_target_raw("bm_params", bquote(nm_params_table(.(res_list)))),
    targets::tar_target_raw("bm_comparison", bquote(nm_comparison_table(.(res_list), .(runs)))),
    targets::tar_target_raw("bm_heuristics", bquote(nm_heuristics_table(.(res_list)))),
    targets::tar_target_raw("bm_log_axis_audit", as.call(c(as.name("rbind"), lapply(launch, function(r)
      bquote(nm_log_axis_audit(.(sym("bm_res_", r))$frame, .(r))))))),
    targets::tar_target_raw(
      "bm_run_report",
      bquote({
        out <- .(file.path(out_dir, "run-report.md"))
        dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
        run_report_render(bm_records, bm_params, dataset = dataset_file,
                          out_file = out,
                          title = .(paste0("Run report — ", analysis_id)))
        out
      }),
      format = "file"),
    file_target("bm_run_notes_file", file.path(model_dir, "run-notes.md")),
    file_target("bm_run_config_file", file.path(model_dir, "run-config.yaml")),
    targets::tar_target_raw("bm_budget", bquote(
      nm_budget_table(.(runs), .(budget[["used"]]), .(budget[["max"]])))),
    targets::tar_target_raw(
      "base_model_package",
      bquote({
        bm_map_file
        artifacts <- .(as.call(c(as.name("rbind"), artifact_rows)))
        tables <- list(
          "Run list and lineage: declared step, based on, retry of, status, counted, evidence, and the ETA column to OMEGA row mapping (etas)" =
            bm_budget$runs,
          "Candidate comparison: verbatim $ESTIMATION text, minimization_terminated (the failure field, SME word Q13), verbatim termination message, no bbr heuristic raised (the estimation unit's converged; not a convergence reading), OFV, subjects, observations" =
            bm_comparison,
          "bbr run heuristics by name per run, as returned" = bm_heuristics,
          "Parameters per run as the estimation unit returns them (estimate, SE, RSE, random-effect SD as bbr reports it, shrinkage, fixed flag); CV percent, back-transformed values and intervals are not computed in B3" =
            bm_params,
          "Log-axis audit (kept observation rows with non-positive DV, PRED or IPRED, which log-scale GOF panels drop)" =
            bm_log_axis_audit,
          "Estimation-run budget (MAP Section 5): used before this stage, counted, total and the hard stop" =
            bm_budget$totals, ..(extra_tables))
        notes <- list(
          authored_note("Numbers come from executed targets; nothing is transcribed by hand."),
          authored_note("The run report is Markdown and AIC is not computed (Q8)."),
          authored_note("The run report's dOFV line is the one subtraction the renderer has always made, parent OFV minus run OFV, with based_on the MAP parent (F8)."),
          authored_note("CV percent, back-transformed values, intervals and the condition number are not computed in B3 (SME word 2026-09-29, route (c)), and - in the run report means not computed."),
          authored_note("A run is failed only when bbr's minimization_terminated is TRUE (SME word Q13), which is also what NOT converged means in the run report; every other heuristic is shown by name, and bbr's help says FALSE \"can either mean it was not detected or it was not applicable for that run\"."),
          authored_note("For a declared fallback run (SAEM then METHOD=IMP EONLY=1, which performs no minimization) the minimization_terminated flag is not an E1 reading, and the SME judges that run from the verbatim termination message and the bm_heuristics table."),
          authored_note("NPDE settings were read back from each stream and checked."))
        list(analysis = .(analysis_id),
             task = "base_model",
             tables = tables,
             artifacts = artifacts,
             notes = notes)
      }, splice = TRUE))),
    rule_targets)
}) else {
  stop("base_model: engine must be explicitly nonmem for this task module.", call. = FALSE)
}
