# Qualification for a declared NONMEM project.
# Composition owns the approval and dependency edges.
if (identical(task_input$engine, "nonmem")) local({
  ti <- task_input
  refuse <- function(field, ...)
    stop("qualification: ", field, ": ", ..., " Declare it in analyses/", analysis_id,
         ".R; the module does not invent settings.", call. = FALSE)
  str1 <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  num1 <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x)
  whole1 <- function(x) num1(x) && x == floor(x)
  rel_path <- function(x) str1(x) && !startsWith(x, "/") &&
    !grepl("(^|/)\\.\\.(/|$)", x)

  # --- compose-time validation (plan Section 6.2 item 1) ---
  fields <- c("engine", "model_risk", "final_run", "workdir", "out_dir", "source_lst",
              "source_ext", "seed", "replicates", "ci", "criteria", "parameters", "vpc",
              "gof", "npde_engine")
  unknown <- setdiff(names(ti), fields)
  if (length(unknown)) refuse(paste(unknown, collapse = ", "), "unknown field(s).")
  absent <- setdiff(fields, names(ti))
  if (length(absent)) refuse(paste(absent, collapse = ", "), "missing.")
  if (!identical(ti$model_risk, "medium"))
    refuse("model_risk", "this branch runs the medium-tier battery (gof, vpc, npde, ",
           "bootstrap) only.")
  if (!str1(ti$final_run) || !grepl("^[0-9A-Za-z_]+$", ti$final_run))
    refuse("final_run", "one run name of letters, digits and underscores.")
  for (f in c("workdir", "out_dir", "source_lst", "source_ext"))
    if (!rel_path(ti[[f]])) refuse(f, "a repository-relative path without '..'.")
  reps <- ti$replicates
  floors <- c(vpc = 200, npde = 500, bootstrap = 500)
  if (!is.list(reps) || !setequal(names(reps), names(floors)))
    refuse("replicates", "list(vpc = , npde = , bootstrap = ) per MAP Section 5.")
  for (nm in names(floors))
    if (!whole1(reps[[nm]]) || reps[[nm]] < floors[[nm]])
      refuse(paste0("replicates$", nm), "a whole number at or above the MAP Section 5 ",
             "floor of ", floors[[nm]], "; floors may be raised, never lowered.")
  if (!whole1(ti$seed)) refuse("seed", "one whole number.")
  if (!num1(ti$ci) || ti$ci <= 0 || ti$ci >= 1) refuse("ci", "one number in (0, 1).")
  criteria_names <- c("rse_fixed_max_pct", "rse_iiv_max_pct", "cond_no_max",
                      "shrink_flag_pct", "vpc_coverage_min_pct", "npde_p_flag",
                      "boot_converged_min_pct", "boot_median_max_pct")
  crit <- ti$criteria
  if (!is.list(crit) || !setequal(names(crit), criteria_names) ||
      anyDuplicated(names(crit)) || !all(vapply(crit, num1, logical(1))))
    refuse("criteria", "exactly the thresholds ", paste(criteria_names, collapse = ", "),
           ", each one finite number.")
  pars <- ti$parameters
  if (!is.list(pars) || !length(pars) || is.null(names(pars)) || any(!nzchar(names(pars))) ||
      anyDuplicated(names(pars)))
    refuse("parameters", "a non-empty named list with unique parameter names.")
  for (nm in names(pars)) {
    p <- pars[[nm]]
    at <- function(f) paste0("parameters (", nm, ") ", f)
    if (!is.list(p)) refuse(at(""), "each parameter is a list.")
    if (!str1(p$kind) || !p$kind %in% c("fixed", "iiv", "other"))
      refuse(at("kind"), "one of fixed, iiv, other.")
    if (!str1(p$transform) || !p$transform %in% c("identity", "exp"))
      refuse(at("transform"), "one of identity, exp.")
    m <- p$psn
    if (!is.list(m)) refuse(at("psn"), "list(block = , position = , heading = ).")
    if (!str1(m$block) || !m$block %in% c("theta", "omega", "sigma"))
      refuse(at("psn$block"), "one of theta, omega, sigma.")
    if (!whole1(m$position) || m$position < 1)
      refuse(at("psn$position"), "a whole number, 1 or more.")
    if (!is.character(m$heading) || length(m$heading) != 1L)
      refuse(at("psn$heading"), "one string, or NA_character_.")
  }
  kinds <- vapply(pars, function(p) p$kind, "")
  if (!any(kinds == "fixed") || !any(kinds == "iiv"))
    refuse("parameters", "at least one fixed and one iiv parameter are required.")
  if (anyDuplicated(vapply(pars, function(p) paste(p$psn$block, p$psn$position), "")))
    refuse("parameters", "the (block, position) pairs of psn must be unique.")
  vpc <- ti$vpc
  if (!is.list(vpc)) refuse("vpc", "a list.")
  if (!is.list(vpc$psn_bin_rule) || length(vpc$psn_bin_rule) != 1L ||
      is.null(names(vpc$psn_bin_rule)))
    refuse("vpc$psn_bin_rule", "a list with exactly one named binning rule.")
  if (!is.character(vpc$npctab_columns) || !length(vpc$npctab_columns) ||
      anyNA(vpc$npctab_columns))
    refuse("vpc$npctab_columns", "the non-empty header PsN writes into its vpc tables.")
  if (!str1(vpc$evid)) refuse("vpc$evid", "one observation-flag column name.")
  for (f in c("binning_pooled", "binning_strata"))
    if (!is.list(vpc[[f]]) || !str1(vpc[[f]]$method))
      refuse(paste0("vpc$", f), "a list with a binning method.")
  oo <- vpc$observed_only
  if (!is.list(oo) || (length(oo) && (is.null(names(oo)) ||
                                      !all(names(oo) %in% c("WTBAND", "APGLOW")))))
    refuse("vpc$observed_only", "a named list, possibly empty, whose names are WTBAND or APGLOW.")
  for (nm in names(oo))
    if (!(is.character(oo[[nm]]) || is.numeric(oo[[nm]])) || length(oo[[nm]]) != 1L)
      refuse(paste0("vpc$observed_only (", nm, ")"), "one level of the stratum column.")
  # ponytail: the SME named no stratum at Q-B5-10, so the observed-points-only
  # panel and nq_coverage_rows are not built; a named stratum stops here
  # rather than being ignored. Ceiling: no sparse-stratum panel; upgrade path
  # is plan Decision 9 step 4 through a planner delta.
  if (length(oo))
    refuse("vpc$observed_only", "this branch implements the SME's Q-B5-10 answer 'Name ",
           "none'; a named stratum needs the observed-points-only panel, which is not built.")
  g <- ti$gof
  if (!is.list(g) || !whole1(g$ids_per_page) || g$ids_per_page < 1 ||
      !is.logical(g$log_y) || length(g$log_y) != 1L || is.na(g$log_y))
    refuse("gof", "list(ids_per_page = , log_y = , etas = , ebe_labels = , ebe_cont_cov = , ",
           "ebe_cat_cov = ).")
  named_chr <- function(x) is.character(x) && length(x) && !is.null(names(x)) &&
    all(nzchar(names(x))) && !anyDuplicated(names(x)) && !anyNA(x)
  for (f in c("etas", "ebe_labels", "ebe_cont_cov", "ebe_cat_cov"))
    if (!named_chr(g[[f]])) refuse(paste0("gof$", f), "a named character vector.")
  if (!setequal(names(g$ebe_labels), names(g$etas)))
    refuse("gof$ebe_labels", "the same names as gof$etas.")
  if (!str1(ti$npde_engine)) refuse("npde_engine", "one non-empty string.")

  # --- targets (plan Section 3 Decision 5) ---
  run <- ti$final_run
  wd <- ti$workdir
  od <- ti$out_dir
  task_input_file("qualification", "source_lst", ti$source_lst, single = TRUE)
  task_input_file("qualification", "source_ext", ti$source_ext, single = TRUE)
  for (file in c(paste0(run, c(".mod", ".lst", ".ext")), "run-config.yaml"))
    task_input_file("qualification", paste0("workdir/", file), file.path(wd, file),
                    single = TRUE)
  fit <- as.name(paste0("bm_fit_", run))
  res <- as.name(paste0("bm_res_", run))
  ctl <- as.name(paste0("bm_ctl_", run, "_file"))
  gof_pdf <- as.name(paste0("bm_gof_", run))
  out <- function(f) file.path(od, f)
  file_target <- function(name, path)
    targets::tar_target_raw(name, call("identity", path), format = "file")
  diag <- "skills/diagnostics/reference"
  ld <- function(skill, ref) bquote(load_reference(.(skill), .(as.name(ref)),
                                                   envir = environment()))

  task_targets <<- list(
    file_target("qual_ctl_file", file.path(wd, paste0(run, ".mod"))),
    file_target("qual_lst_file", file.path(wd, paste0(run, ".lst"))),
    file_target("qual_ext_file", file.path(wd, paste0(run, ".ext"))),
    file_target("qual_run_config_file", file.path(wd, "run-config.yaml")),
    file_target("qual_gof_reference", file.path(diag, "gof.R")),
    file_target("qual_vpc_reference", file.path(diag, "vpc.R")),
    file_target("qual_vpc_strata_reference", file.path(diag, "vpc_strata.R")),
    file_target("qual_vpc_coverage_reference", file.path(diag, "vpc_coverage.R")),
    file_target("qual_npde_panels_reference", file.path(diag, "npde_panels.R")),
    file_target("qual_npde_tests_reference", file.path(diag, "npde_tests.R")),
    file_target("qual_bootstrap_agreement_reference", file.path(diag, "bootstrap_agreement.R")),
    file_target("qual_verdict_reference", file.path(diag, "qualification_verdict.R")),
    file_target("qual_psn_vpc_reference", "skills/psn-simeval/reference/psn_simeval.R"),
    file_target("qual_psn_bootstrap_reference",
                "skills/psn-uncertainty/reference/psn_uncertainty.R"),

    # The PsN inputs are byte copies of the signed final-run files; both PsN
    # runs name this target, so neither runs on other bytes.
    targets::tar_target_raw("qual_inputs", bquote(list(
      digests = c(stream = nq_check_copy(qual_ctl_file, .(ctl)),
                  listing = nq_check_copy(qual_lst_file, .(ti$source_lst)),
                  parameter_file = nq_check_copy(qual_ext_file, .(ti$source_ext))),
      parameters = nq_check_parameters(.(names(pars)), .(res)$results$params$parameter)))),

    targets::tar_target_raw("qual_frame", bquote(nm_observation_frame(
      .(res)$results$residuals, qual_ctl_file, .(res)$results$n_obs,
      npde_nsim = .(reps$npde), seed = .(ti$seed)))),

    # E3 from bbr's model summary (Q-B5-8 (b)); the bbi path comes from the
    # estimation unit's own resolver.
    targets::tar_target_raw("qual_estimation", bquote({
      qual_inputs
      .(ld("skills/nonmem-estimation", "bm_nm_reference"))
      .reference_nm_require_bbi(.reference_nm_bbi_path_from_harness())
      nq_estimation_contract(.(res)$results, .(fit)$message,
                             nq_condition_number(.(fit)$mod), .(kinds), run = .(run))
    })),

    targets::tar_target_raw("qual_npde_tests", bquote({
      .(ld("skills/diagnostics", "qual_npde_tests_reference"))
      reference_npde_global_tests(qual_frame[["NPDE"]], p_flag = .(crit$npde_p_flag))
    })),

    targets::tar_target_raw("qual_npde_panels", bquote({
      .(ld("skills/diagnostics", "qual_npde_panels_reference"))
      p <- reference_npde_panels(qual_frame, .(reps$npde), .(ti$seed), .(ti$npde_engine))
      nm_write_panels(p[setdiff(names(p), "record")], .(out(paste0("npde-", run, ".pdf"))))
    }), format = "file"),

    targets::tar_target_raw("qual_ind_profiles", bquote({
      library(ggplot2)   # pmplots' paginated helpers call ggplot2 unqualified
      .(ld("skills/diagnostics", "qual_gof_reference"))
      nm_write_panels(reference_gof_individual(qual_frame, .(g$ids_per_page), .(g$log_y)),
                      .(out(paste0("individual-profiles-", run, ".pdf"))))
    }), format = "file"),

    # Q-B5-4 (b): the ETA-against-covariate panels through the gof unit's EBE
    # mode, each labelled with the engine's shrinkage for its ETA in the
    # unit's own histogram label format.
    targets::tar_target_raw("qual_ebe_panels", bquote({
      .(ld("skills/diagnostics", "qual_gof_reference"))
      shr <- nm_eta_shrinkage(.(res)$results$params, .(g$etas))
      labs <- sprintf("%s (shrinkage %.1f%%)", .(g$ebe_labels)[names(shr)], shr)
      nm_write_panels(reference_gof_param_cov(qual_frame, stats::setNames(labs, names(shr)),
                                              cont_cov = .(g$ebe_cont_cov),
                                              cat_cov = .(g$ebe_cat_cov)),
                      .(out(paste0("ebe-covariates-", run, ".pdf"))))
    }), format = "file"),

    targets::tar_target_raw("qual_vpc_run", bquote({
      qual_inputs
      qual_run_config_file
      .(ld("skills/psn-simeval", "qual_psn_vpc_reference"))
      reference_psn_vpc(.(wd), basename(qual_ctl_file), samples = .(reps$vpc),
                        seed = .(ti$seed), bin_rule = .(vpc$psn_bin_rule),
                        lst_file = basename(qual_lst_file),
                        engine_config = load_engine_config(.(wd)))
    })),

    # PsN simulates; the approved tidyvpc unit computes (A2 item 5). Strata
    # come from the final run's own frame, joined by ID.
    targets::tar_target_raw("qual_vpc", bquote({
      .(ld("skills/diagnostics", "qual_vpc_reference"))
      .(ld("skills/diagnostics", "qual_vpc_strata_reference"))
      .(ld("skills/diagnostics", "qual_vpc_coverage_reference"))
      tabs <- nq_vpc_tables(qual_vpc_run$run_dir, .(reps$vpc), .(vpc$npctab_columns))
      prep <- reference_vpc_prepare(tabs$obs, tabs$sim, id = "ID", x = "TIME",
                                    repl = "REP", evid = .(vpc$evid))
      bands <- reference_weight_bands(qual_frame)
      apgar <- qual_frame[!duplicated(qual_frame$ID), c("ID", "APGLOW")]
      with_strata <- function(d)
        nq_attach_subject(nq_attach_subject(d, bands$subject, "WTBAND"), apgar, "APGLOW")
      obs <- with_strata(prep$obs)
      sim <- with_strata(prep$sim)
      vpc_of <- function(binning, strata = NULL)
        reference_vpc_continuous(obs, sim, x = "TIME", yobs = "DV", ysim = "DV",
                                 repl = "REP", binning_spec = binning, strata = strata,
                                 seed = .(ti$seed))
      fits <- list(pooled = vpc_of(.(vpc$binning_pooled)),
                   weight = vpc_of(.(vpc$binning_strata), ~WTBAND),
                   apgar = vpc_of(.(vpc$binning_strata), ~APGLOW))
      stats <- lapply(fits, function(v) as.data.frame(v$stats))
      coverage_pooled <- reference_vpc_coverage(stats$pooled)
      coverage_weight <- reference_vpc_coverage(stats$weight, strata = "WTBAND")
      coverage_apgar <- reference_vpc_coverage(stats$apgar, strata = "APGLOW")
      paths <- .(c(pooled = out(paste0("vpc-", run, ".pdf")),
                   weight = out(paste0("vpc-wt-", run, ".pdf")),
                   apgar = out(paste0("vpc-apgar-", run, ".pdf")),
                   coverage = out(paste0("vpc-coverage-", run, ".csv"))))
      dir.create(.(od), recursive = TRUE, showWarnings = FALSE)
      titles <- .(c(pooled = paste0("VPC, run ", run, ", pooled over time"),
                    weight = paste0("VPC, run ", run, ", by subject weight tertile"),
                    apgar = paste0("VPC, run ", run, ", by Apgar group")))
      subtitle <- .(sprintf("PsN vpc simulation, %s datasets, seed %s; tidyvpc statistics",
                            format(reps$vpc), format(ti$seed, scientific = FALSE)))
      for (k in names(titles))
        ggplot2::ggsave(paths[[k]], plot(fits[[k]]) +
                          ggplot2::labs(title = titles[[k]], subtitle = subtitle),
                        width = if (k == "pooled") 7 else 9, height = 5)
      utils::write.csv(rbind(cbind(panel = "pooled time", stratum = "(all)", coverage_pooled),
                             cbind(panel = "weight tertile", coverage_weight),
                             cbind(panel = "Apgar group", coverage_apgar)),
                       paths[["coverage"]], row.names = FALSE)
      list(settings = list(samples = .(reps$vpc), seed = .(ti$seed), evid = .(vpc$evid),
                           binning_pooled = .(vpc$binning_pooled),
                           binning_strata = .(vpc$binning_strata)),
           bands = bands, stats = stats, coverage_pooled = coverage_pooled,
           coverage_weight = coverage_weight, coverage_apgar = coverage_apgar,
           artifacts = paths)
    })),

    targets::tar_target_raw("qual_bootstrap_run", bquote({
      qual_inputs
      qual_run_config_file
      .(ld("skills/psn-uncertainty", "qual_psn_bootstrap_reference"))
      reference_psn_bootstrap(.(wd), basename(qual_ctl_file), samples = .(reps$bootstrap),
                              seed = .(ti$seed), engine_config = load_engine_config(.(wd)),
                              model_risk = .(ti$model_risk), stratify_on = NULL,
                              bca = FALSE, dofv = FALSE)
    })),

    # The A3 chain (plan Decision 10); columns picked by position (A-1).
    targets::tar_target_raw("qual_bootstrap", bquote({
      .(ld("skills/psn-uncertainty", "qual_psn_bootstrap_reference"))
      .(ld("skills/diagnostics", "qual_bootstrap_agreement_reference"))
      pars <- .(pars)
      kinds <- vapply(pars, function(p) p$kind, "")
      tr <- vapply(pars, function(p) p$transform, "")
      raw <- qual_bootstrap_run$raw_results_csv
      structure_file <- file.path(qual_bootstrap_run$run_dir, "raw_results_structure")
      pat <- nq_psn_patterns(raw, structure_file, lapply(pars, function(p) p$psn))
      w <- reference_psn_bootstrap_samples(raw, pat)
      acc <- reference_bootstrap_replicate_accounting(w$minimization_successful, w$n_samples,
                                                      requested = .(reps$bootstrap))
      cats <- reference_bootstrap_unsuccessful_counts(acc$success, w$flags)
      s <- reference_bootstrap_summaries(w$estimates, acc$success, tr, ci = .(ti$ci))
      est <- nq_estimates(.(res)$results$params, names(pars))
      est_nat <- vapply(names(pars), function(p)
        reference_bootstrap_back_transform(est[[p]], tr[[p]]), numeric(1))
      col <- function(p, f) s[[f]][match(p, s$parameter)]
      fx <- names(kinds)[kinds == "fixed"]
      iv <- names(kinds)[kinds == "iiv"]
      rse <- c(stats::setNames(reference_bootstrap_fixed_rse(col(fx, "sd_estimation"),
                                                             unname(est[fx])), fx),
               stats::setNames(reference_bootstrap_iiv_rse(col(iv, "sd_estimation"),
                                                           unname(est[iv])), iv))
      pct <- stats::setNames(reference_bootstrap_pct_diff(unname(est_nat[fx]),
                                                          col(fx, "median_natural")), fx)
      cif <- stats::setNames(reference_bootstrap_ci_finite(col(names(pars), "ci_lower_natural"),
                                                           col(names(pars), "ci_upper_natural")),
                             names(pars))
      ptab <- nq_parameter_table(pars, est, est_nat,
                                 nq_estimates(.(res)$results$params, names(pars), "rse_pct"),
                                 s, rse, cif, attr(pat, "picked"))
      csv <- .(out(paste0("bootstrap-", run, ".csv")))
      dir.create(dirname(csv), recursive = TRUE, showWarnings = FALSE)
      utils::write.csv(ptab, csv, row.names = FALSE)
      c(nq_bootstrap_tables(kinds, rse, pct, cif, acc$success_rate_pct),
        list(accounting = data.frame(requested = acc$n_requested, parsed = w$n_samples,
                                     successful = acc$n_successful,
                                     success_rate_pct = acc$success_rate_pct),
             categories = cats, summaries = s, parameter_table = ptab,
             column_map = data.frame(parameter = names(pat), pattern = unname(pat),
                                     column = unname(attr(pat, "picked")),
                                     stringsAsFactors = FALSE),
             artifacts = list(raw_results = raw, structure = structure_file,
                              results = qual_bootstrap_run$results_csv, parameters = csv)))
    })),

    # The verdict's stratified coverage is the weight-tertile coverage only
    # (the covstep row reads "weight-stratified").
    targets::tar_target_raw("qual_verdicts", bquote({
      .(ld("skills/diagnostics", "qual_verdict_reference"))
      v <- reference_qualification_verdict_covstep(
        qual_estimation,
        list(fixed = qual_bootstrap$fixed, iiv = qual_bootstrap$iiv,
             success_rate_pct = qual_bootstrap$success_rate_pct),
        qual_npde_tests,
        list(coverage_pooled = qual_vpc$coverage_pooled, coverage_strat = qual_vpc$coverage_weight),
        .(crit))
      dir.create(.(od), recursive = TRUE, showWarnings = FALSE)
      utils::write.csv(v, .(out(paste0("verdict-table-", run, ".csv"))), row.names = FALSE)
      v
    })),

    targets::tar_target_raw("qualification_package", bquote({
      # Every qualification target is named here, so the store-staleness check
      # of the gate package sees all of them.
      qual_ctl_file; qual_lst_file; qual_ext_file; qual_run_config_file
      qual_gof_reference; qual_vpc_reference; qual_vpc_strata_reference
      qual_vpc_coverage_reference; qual_npde_panels_reference; qual_npde_tests_reference
      qual_bootstrap_agreement_reference; qual_verdict_reference
      qual_psn_vpc_reference; qual_psn_bootstrap_reference
      inv <- nq_invocation_table(list(vpc = qual_vpc_run, bootstrap = qual_bootstrap_run),
                                 load_engine_config(.(wd))$psn)
      vpc_inv <- inv[inv$tool == "vpc", ]
      boot_inv <- inv[inv$tool == "bootstrap", ]
      npde <- attr(qual_frame, "npde_settings")
      members <- nq_battery_members(.(ti$model_risk), list(
        gof = list(units = "diagnostics/gof",
                   artifact = paste(c(.(gof_pdf), qual_ind_profiles, qual_ebe_panels),
                                    collapse = "; "),
                   detail = paste("GOF and ETA panels of the final model from the estimation",
                                  "stage, individual profiles, ETA-against-covariate panels",
                                  "labelled with the engine's shrinkage per ETA")),
        vpc = list(units = c("psn-simeval/vpc", "diagnostics/vpc", "diagnostics/vpc_strata",
                             "diagnostics/vpc_coverage"),
                   artifact = paste(qual_vpc$artifacts, collapse = "; "),
                   detail = sprintf(paste("PsN vpc simulation, %s datasets, seed %s; tidyvpc",
                                          "statistics pooled and by stratum"),
                                    vpc_inv$samples, vpc_inv$seed)),
        npde = list(units = c("diagnostics/npde_panels", "diagnostics/npde_tests"),
                    artifact = qual_npde_panels,
                    detail = sprintf(paste("NPDE from the final run's $TABLE, ESAMPLE %s,",
                                           "SEED %s, read back from the stream"),
                                     npde$esample, npde$seed)),
        bootstrap = list(units = c("psn-uncertainty/bootstrap", "diagnostics/bootstrap_agreement"),
                         artifact = paste(unlist(qual_bootstrap$artifacts), collapse = "; "),
                         detail = sprintf(paste("PsN bootstrap, %s samples, seed %s,",
                                                "percentile intervals, no stratified",
                                                "resampling"),
                                          boot_inv$samples, boot_inv$seed))))
      inputs <- data.frame(
        run = .(run),
        file = c("stream", "listing", "parameter file", "run configuration"),
        copy = c(qual_ctl_file, qual_lst_file, qual_ext_file, qual_run_config_file),
        signed_original = c(.(ctl), .(ti$source_lst), .(ti$source_ext),
                            "none: approved for this stage"),
        sha256 = c(unname(qual_inputs$digests),
                   digest::digest(file = qual_run_config_file, algo = "sha256")),
        note = c(rep("copy checked equal to the signed original (nq_check_copy)", 3L),
                 "read by both PsN runs"),
        stringsAsFactors = FALSE)
      est <- qual_estimation$params
      estimation <- data.frame(
        parameter = est$parameter, kind = est$kind,
        rse_pct_covariance_step_estimation_scale = est$rse_pct,
        shrink_sd_pct_passed_to_E5 = est$shrink_sd_pct,
        engine_shrink_sd_pct = unname(nq_engine_shrinkage(.(res)$results$params, est$parameter)),
        minimization_successful = qual_estimation$minimization_successful,
        covariance_step_successful = qual_estimation$covariance_step_successful,
        condition_number = qual_estimation$condition_number,
        condition_number_producer = "bbr model_summary(), element condition_number",
        termination_message = paste(qual_estimation$termination_message, collapse = " "),
        stringsAsFactors = FALSE)
      accounting <- data.frame(
        quantity = c("development runs counted (bm_budget total)",
                     "development-run hard stop (bm_budget max)",
                     "VPC simulated datasets (PsN vpc command line)",
                     "bootstrap samples (PsN bootstrap command line)",
                     "NPDE samples (ESAMPLE read back from the final run's $TABLE)"),
        value = c(as.character(bm_budget$totals$total), as.character(bm_budget$totals$max),
                  vpc_inv$samples, boot_inv$samples, npde$esample),
        stringsAsFactors = FALSE)
      tables <- list(
        "Battery members (registry-selected, MODEL_RISK medium)" = members,
        "Final model inputs: the work-directory copies and their digests" = inputs,
        "Verdict table (MAP 4.3.2 E1 to E6, Q1 to Q3)" = qual_verdicts,
        "Estimation inputs to the verdict" = estimation,
        "Final model parameters with bootstrap intervals" = qual_bootstrap$parameter_table,
        "Bootstrap replicate accounting (Q3)" = qual_bootstrap$accounting,
        "Unsuccessful bootstrap replicates by PsN flag category (Q3)" = qual_bootstrap$categories,
        "VPC bin coverage, pooled time VPC (Q1)" = qual_vpc$coverage_pooled,
        "VPC bin coverage by subject weight tertile (Q1 drift evidence)" = qual_vpc$coverage_weight,
        "VPC bin coverage by Apgar group (Q1 drift evidence)" = qual_vpc$coverage_apgar,
        "Weight tertile bands (quantile type 7)" = qual_vpc$bands$summary,
        "NPDE global tests (Q2)" = qual_npde_tests,
        "NPDE settings read back from the final run's $TABLE" =
          data.frame(esample = npde$esample, seed = npde$seed, stream = npde$source,
                     stringsAsFactors = FALSE),
        "PsN invocations" = inv,
        "Run accounting (MAP Section 5)" = accounting)
      art <- function(artifact, path, source)
        data.frame(artifact = artifact, path = nq_relative_path(path), source = source,
                   stringsAsFactors = FALSE)
      artifacts <- rbind(
        art("final-run stream, PsN work-directory copy", qual_ctl_file,
            "file target qual_ctl_file; byte copy checked by qual_inputs"),
        art("final-run listing, PsN work-directory copy", qual_lst_file,
            "file target qual_lst_file; byte copy checked by qual_inputs"),
        art("final-run parameter file, PsN work-directory copy", qual_ext_file,
            "file target qual_ext_file; byte copy checked by qual_inputs"),
        art("PsN run configuration", qual_run_config_file, "file target qual_run_config_file"),
        art("GOF and ETA panels of the final model", .(gof_pdf),
            .(paste0("target bm_gof_", run, " (estimation stage)"))),
        art("individual profiles", qual_ind_profiles,
            "target qual_ind_profiles (diagnostics gof, reference_gof_individual)"),
        art("ETA-against-covariate panels with engine shrinkage", qual_ebe_panels,
            "target qual_ebe_panels (diagnostics gof, reference_gof_param_cov)"),
        art("NPDE panels", qual_npde_panels, "target qual_npde_panels (diagnostics npde_panels)"),
        art("VPC, pooled over time", qual_vpc$artifacts[["pooled"]], "target qual_vpc (diagnostics vpc)"),
        art("VPC by subject weight tertile", qual_vpc$artifacts[["weight"]],
            "target qual_vpc (diagnostics vpc, vpc_strata)"),
        art("VPC by Apgar group", qual_vpc$artifacts[["apgar"]], "target qual_vpc (diagnostics vpc)"),
        art("VPC bin coverage", qual_vpc$artifacts[["coverage"]],
            "target qual_vpc (diagnostics vpc_coverage)"),
        art("PsN vpc results", qual_vpc_run$results_csv, "target qual_vpc_run (psn-simeval vpc)"),
        art("PsN per-sample bootstrap file", qual_bootstrap$artifacts$raw_results,
            "target qual_bootstrap_run (psn-uncertainty bootstrap)"),
        art("PsN column map of the per-sample file", qual_bootstrap$artifacts$structure,
            "target qual_bootstrap_run (psn-uncertainty bootstrap)"),
        art("PsN bootstrap results", qual_bootstrap$artifacts$results,
            "target qual_bootstrap_run (psn-uncertainty bootstrap)"),
        art("final model parameters with bootstrap intervals", qual_bootstrap$artifacts$parameters,
            "target qual_bootstrap (diagnostics bootstrap_agreement)"),
        art("verdict table", .(out(paste0("verdict-table-", run, ".csv"))),
            "target qual_verdicts (diagnostics qualification_verdict)"))
      notes <- list(
        authored_note("Every number in the tables above is read from executed targets: the engine's results reader, the approved diagnostics and PsN units, and the files PsN wrote; nothing is transcribed by hand."),
        authored_note("Battery membership is mechanical: the declared model risk selects the mandatory members from the harness registry, and each unit's manifest status is read at run time."),
        authored_note("NPDE is the final run's own $TABLE column; its sample count and seed are read back from the stream and checked against the declaration."),
        authored_note("PsN simulates and the approved tidyvpc unit computes the VPC statistics; PsN's own vpc_results.csv is kept as the engine's artifact and feeds no verdict."),
        authored_note("PsN read the final estimates from the engine's parameter file beside the listing in its work directory and did not re-estimate the model; this stage composes no estimation target."),
        authored_note("Bootstrap evidence comes from PsN's per-sample file through the approved bootstrap units; its parameter columns are picked by position from PsN's own column map (raw_results_structure), not by heading."),
        authored_note("No bootstrap value enters a covariance-step row, and no covariance-step value enters a bootstrap row."),
        authored_note("The VPC coverage by Apgar group is drift evidence for the SME; the verdict's stratified coverage input is the weight-tertile coverage only."),
        authored_note("Run accounting: this stage composes no estimation target; PsN did not re-estimate the original model (the listing sat beside the stream); the bootstrap and VPC replicates are machinery under MAP Section 5."),
        evidence_note("VPC simulated datasets, from the PsN command line", vpc_inv$samples),
        evidence_note("VPC seed, from the PsN command line", vpc_inv$seed),
        evidence_note("Bootstrap samples, from the PsN command line", boot_inv$samples),
        evidence_note("Bootstrap seed, from the PsN command line", boot_inv$seed),
        evidence_note("NPDE samples, read back from the final run's $TABLE", npde$esample),
        evidence_note("NPDE seed, read back from the final run's $TABLE", npde$seed))
      list(analysis = .(analysis_id), task = "qualification", tables = tables,
           artifacts = artifacts, notes = notes)
    }))
  )
  stopifnot(!anyDuplicated(vapply(task_targets, function(t) t$settings$name, "")))
}) else {
  stop("qualification: engine must be explicitly nonmem for this task module.", call. = FALSE)
}
