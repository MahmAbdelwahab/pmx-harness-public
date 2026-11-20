# Per-run summary report — harness tool (deterministic; formats executed
# run records, computes nothing analytic itself except dOFV = parent OFV
# minus run OFV from the recorded lineage).
#
# Reads the base-model task targets (bm_records, bm_params) from an
# analysis store and renders one markdown card per run: structure
# hypothesis, lineage, dataset, counts, OFV / dOFV vs parent / AIC,
# convergence message, and the parameter table.
# The optional params columns back_transformed, ci_lower, ci_upper and
# bsv_cv_pct may be absent (the NONMEM path); an absent column or NA cell
# renders "-" and a fixed note line follows the table.
#
# Call run_report() with the analysis store and optional declared dataset and output paths.

# Render from in-memory objects — the form pipeline targets call (a target
# must not read its own store mid-build).
run_report_render <- function(records, params, dataset = NULL,
                              out_file = NULL, digits = 4,
                              title = "Run report") {

  ofv_by_run <- vapply(records, function(x) x$ofv, numeric(1))
  names(ofv_by_run) <- vapply(records, function(x) as.character(x$meta$run),
                              character(1))

  fmt <- function(x) formatC(x, digits = digits, format = "g")

  card <- function(x) {
    run <- as.character(x$meta$run)
    parent <- x$meta$based_on
    has_parent <- length(parent) == 1 && nzchar(as.character(parent)) &&
      as.character(parent) %in% names(ofv_by_run)
    dofv <- if (has_parent) {
      sprintf("%s (vs run %s)",
              fmt(ofv_by_run[[as.character(parent)]] - x$ofv), parent)
    } else "- (no parent)"

    p <- params[params$run == x$meta$run, , drop = FALSE]
    # Optional columns by exact name: `$` would partially match ci_lower_95.
    opt <- c("back_transformed", "ci_lower", "ci_upper", "bsv_cv_pct")
    ocell <- function(col, i) {
      v <- if (col %in% names(p)) p[[col]][i] else NA
      if (is.na(v)) "-" else fmt(v)
    }
    ptab <- c(
      "| Parameter | Estimate | RSE% | Natural scale | 95% CI | BSV CV% | Shrink(SD)% |",
      "|---|---|---|---|---|---|---|",
      vapply(seq_len(nrow(p)), function(i) {
        sprintf("| %s | %s | %s | %s | [%s, %s] | %s | %s |",
                p$parameter[i], fmt(p$estimate[i]), fmt(p$rse_pct[i]),
                ocell("back_transformed", i), ocell("ci_lower", i),
                ocell("ci_upper", i), ocell("bsv_cv_pct", i),
                ifelse(is.na(p$shrink_sd_pct[i]), "-", fmt(p$shrink_sd_pct[i])))
      }, character(1)))
    if (!all(opt %in% names(p))) {
      ptab <- c(ptab, "",
                paste("Natural scale, 95% CI and BSV CV% shown as - are not",
                      "computed for this run (no signed reference supplies",
                      "them); - here does not mean not applicable. NA in RSE%",
                      "means the engine returned no standard error."))
    }

    c(sprintf("## Run %s", run),
      "",
      sprintf("- **Structure / hypothesis**: %s", x$meta$hypothesis),
      sprintf("- **Based on**: %s",
              if (has_parent) sprintf("run %s", parent) else "- (reference)"),
      if (!is.null(dataset)) sprintf("- **Dataset**: %s", dataset),
      sprintf("- **Subjects / observations**: %d / %d", x$n_id, x$n_obs),
      sprintf("- **Estimation**: %s; %s", x$est,
              if (isTRUE(x$converged)) sprintf("converged (%s)", x$message)
              else sprintf("NOT converged (%s)", x$message)),
      sprintf("- **OFV**: %s | **dOFV**: %s | **AIC**: %s",
              fmt(x$ofv), dofv, fmt(x$aic)),
      "",
      ptab,
      "")
  }

  lines <- c(sprintf("# %s", title),
             sprintf("Generated %s from executed run records (bm_records, bm_params).",
                     format(Sys.time(), "%Y-%m-%d %H:%M %Z")),
             "",
             unlist(lapply(records, card)))
  if (!is.null(out_file)) {
    writeLines(lines, out_file)
    invisible(out_file)
  } else {
    lines
  }
}

# Ad-hoc form: read the base-model targets from a store and render.
run_report <- function(store, dataset = NULL, out_file = NULL, digits = 4) {
  run_report_render(
    records = targets::tar_read_raw("bm_records", store = store),
    params  = targets::tar_read_raw("bm_params", store = store),
    dataset = dataset, out_file = out_file, digits = digits,
    title   = sprintf("Run report — store %s", store))
}
