# NONMEM base-model shaping helpers.
#
# Functions only. Every name starts with nm_, no top-level code runs, and no
# helper computes an analysis number: they check, count, look up by exact name,
# compare with a declared bound and reshape what the estimation unit returned
# (the nested-LRT arithmetic is reference_nested_lrt's). A CV, a back-transformed
# value, an interval or a condition number is never computed here. Named by no
# method manifest.
#
# Callers: pipeline/tasks/base_model.R (NONMEM arm) and the NONMEM base-model
# battery (tools/tests/test_nonmem_base_model.R). harness_kept_record and
# harness_is_observation come from tools/record_selection.R.

# The control-stream records of `lines`, comment text after ";" removed and
# whitespace runs collapsed to one space: a data frame of keyword (upper case)
# and text (the whole record, keyword included, continuation lines joined).
nm_stream_records <- function(lines) {
  s <- sub(";.*$", "", lines)
  start <- grepl("^\\s*\\$[A-Za-z]", s)
  if (!any(start)) return(data.frame(keyword = character(0), text = character(0)))
  id <- cumsum(start)
  keep <- id > 0L
  text <- vapply(split(s[keep], id[keep]), function(x)
    trimws(gsub("\\s+", " ", paste(x, collapse = " "))), character(1))
  data.frame(keyword = toupper(sub("^(\\$[A-Za-z]+).*$", "\\1", text)),
             text = unname(text), stringsAsFactors = FALSE)
}

# The value of KEY= in one record's text, or NA when the option is absent.
nm_record_option <- function(text, key) {
  m <- regmatches(text, regexec(paste0("(^|\\s)", key, "\\s*=\\s*([^ ]+)"), text))[[1]]
  if (length(m)) m[3] else NA_character_
}

# The observation frame of one run (Section 7.2 steps 1 to 8). Returns the
# kept observation rows of `residuals` with every column and value unchanged,
# carrying the attributes npde_settings and estimation.
nm_observation_frame <- function(residuals, run_ctl, n_obs, npde_nsim, seed) {
  fail <- function(step, ...)
    stop("nm_observation_frame (", run_ctl, "), step ", step, ": ", ...,
         call. = FALSE)
  cols <- names(residuals)
  # 1. The only DV is the data DV (nm_join's duplicate repair, finding L4).
  dup <- cols[grepl("\\.DATA$", cols) | grepl("^DV\\.[0-9]+$", cols)]
  if (length(dup))
    fail(1, "duplicate-repaired column(s) ", paste(dup, collapse = ", "),
         "; the stream must table no DV, so the only DV is the data DV.")
  # 2. Required columns, nothing renamed.
  need <- c("ID", "TIME", "EVID", "C", "DV", "PRED", "IPRED", "CWRES", "TAD")
  miss <- setdiff(need, cols)
  if (length(miss)) fail(2, "missing column(s) ", paste(miss, collapse = ", "), ".")
  # 3. Kept observation records by the house classifier.
  keep <- harness_kept_record(residuals[["C"]]) & harness_is_observation(residuals[["EVID"]])
  frame <- residuals[keep, , drop = FALSE]
  # 4. Kept count equals the run's observation count.
  if (nrow(frame) != n_obs)
    fail(4, "kept observation rows (", nrow(frame), ") differ from n_obs (", n_obs, ").")
  # 5. TAD finite and not negative (finding L11).
  tad <- frame[["TAD"]]
  if (!all(is.finite(tad) & tad >= 0))
    fail(5, "TAD is missing, non-finite or negative on a kept observation row.")
  rec <- nm_stream_records(readLines(run_ctl, warn = FALSE))
  # 8. Record names in full (read before steps 6 and 7, whose readers match
  # full names only, so an abbreviation would otherwise read as absent).
  full <- c("$ESTIMATION", "$TABLE", "$COVARIANCE")
  short <- rec$keyword[substr(rec$keyword, 1, 4) %in% substr(full, 1, 4) &
                         !rec$keyword %in% full]
  if (length(short))
    fail(8, "abbreviated record name(s) ", paste(unique(short), collapse = ", "),
         "; Section 7.5 requires $ESTIMATION, $TABLE and $COVARIANCE in full.")
  # 6. NPDE settings read back from every $TABLE record that requests NPDE.
  tab <- rec$text[rec$keyword == "$TABLE"]
  req <- tab[grepl("(^|\\s)NPDE(\\s|$)", tab)]
  has_npde <- "NPDE" %in% names(frame)
  esample <- NA_character_; tseed <- NA_character_
  if (length(req)) {
    for (t in req) {
      esample <- nm_record_option(t, "ESAMPLE"); tseed <- nm_record_option(t, "SEED")
      if (is.na(esample) || is.na(tseed))
        fail(6, "a $TABLE record requests NPDE without both ESAMPLE= and SEED=.")
      if (!identical(as.numeric(esample), as.numeric(npde_nsim)) ||
          !identical(as.numeric(tseed), as.numeric(seed)))
        fail(6, "$TABLE ESAMPLE=", esample, " SEED=", tseed,
             " differ from the declared npde_nsim ", npde_nsim, " and seed ", seed, ".")
    }
    if (!has_npde) fail(6, "NPDE is requested in $TABLE but absent from the frame.")
    if (!all(is.finite(frame[["NPDE"]])))
      fail(6, "NPDE is not finite on every kept observation row.")
  } else if (has_npde) {
    fail(6, "the frame carries NPDE but no $TABLE record requests it (settings unknown).")
  }
  # 7. The $ESTIMATION records, verbatim, joined in stream order.
  est <- rec$text[rec$keyword == "$ESTIMATION"]
  if (!length(est)) fail(7, "the stream has no $ESTIMATION record.")
  attr(frame, "npde_settings") <- list(esample = esample, seed = tseed, source = run_ctl)
  attr(frame, "estimation") <- paste(est, collapse = "; ")
  frame
}

# The SME's failure field (Q13): bbr's minimization_terminated heuristic, read
# by exact name with [[ (never $, which matches partially).
nm_minimization_terminated <- function(heuristics, run) {
  if (!is.list(heuristics))
    stop("run ", run, ": heuristics is not a list.", call. = FALSE)
  if (!"minimization_terminated" %in% names(heuristics))
    stop("run ", run, ": heuristics carry no minimization_terminated flag.", call. = FALSE)
  v <- heuristics[["minimization_terminated"]]
  if (!is.logical(v) || length(v) != 1L || is.na(v))
    stop("run ", run, ": minimization_terminated is not one non-NA logical.", call. = FALSE)
  v
}

# The record run_report_render reads (decision 5).
nm_run_record <- function(fit, results, frame) {
  list(meta = fit$meta,
       est = attr(frame, "estimation"),
       message = fit$message,
       converged = !nm_minimization_terminated(results$heuristics, fit$meta$run),
       n_id = results$n_id,
       n_obs = results$n_obs,
       ofv = results$ofv,
       aic = NA_real_,
       run_start = fit$run_start)
}

# bm_params: each run's results$params with a leading run column.
nm_params_table <- function(res_list) {
  out <- do.call(rbind, lapply(res_list, function(r)
    cbind(data.frame(run = r$record$meta$run, stringsAsFactors = FALSE),
          r$results$params)))
  rownames(out) <- NULL
  out
}

# One declared run-list entry by name.
nm_entry <- function(runs, run) runs[[which(vapply(runs, `[[`, "", "run") == run)]]

# bm_comparison: one row per run. `runs` is the declaration's run list, the
# source of step and retry_of.
nm_comparison_table <- function(res_list, runs) {
  out <- do.call(rbind, lapply(res_list, function(r) {
    run <- r$record$meta$run
    e <- nm_entry(runs, run)
    data.frame(run = run, based_on = r$record$meta$based_on,
               retry_of = e[["retry_of"]], step = e[["step"]],
               estimation = r$record$est,
               minimization_terminated = nm_minimization_terminated(r$results$heuristics, run),
               message = r$record$message,
               no_bbr_heuristic_raised = r$no_bbr_heuristic_raised,
               ofv = r$results$ofv, n_id = r$results$n_id, n_obs = r$results$n_obs,
               stringsAsFactors = FALSE)
  }))
  rownames(out) <- NULL
  out
}

# bm_heuristics: every bbr heuristic by name, one row per run and name.
nm_heuristics_table <- function(res_list) {
  out <- do.call(rbind, lapply(res_list, function(r) {
    h <- r$results$heuristics
    data.frame(run = rep(r$record$meta$run, length(h)), heuristic = names(h),
               value = vapply(h, identity, logical(1), USE.NAMES = FALSE),
               stringsAsFactors = FALSE)
  }))
  rownames(out) <- NULL
  out
}

# The shrink_sd_pct of each declared OMEGA row, named by its ETA column.
nm_eta_shrinkage <- function(params, etas) {
  vapply(etas, function(row) {
    i <- which(params$parameter == row)
    if (length(i) != 1L)
      stop("nm_eta_shrinkage: parameter row ", row, " appears ", length(i),
           " times in the params table; exactly one is required.", call. = FALSE)
    params$shrink_sd_pct[i]
  }, numeric(1))
}

# The run statuses that count toward the run budget (Section 13).
nm_counted <- function(status) status %in% c("launch", "failed-counted")

# The compose-time budget refusal (Section 13).
nm_budget_stop <- function(used, counted, max) {
  whole <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x) &&
    x >= 0 && x == round(x)
  for (f in c("used", "counted", "max"))
    if (!whole(get(f, inherits = FALSE)))
      stop("nm_budget_stop: ", f, " must be a single non-negative whole number.",
           call. = FALSE)
  if (used + counted > max)
    stop("run budget exceeded: ", used + counted, " of ", max,
         " declared; ESCALATE to the SME", call. = FALSE)
  invisible(TRUE)
}

# bm_budget: the run list and lineage (one row per declared entry) and the
# budget totals.
nm_budget_table <- function(runs, used, max) {
  field <- function(e, f) if (is.null(e[[f]])) "" else paste(e[[f]], collapse = "; ")
  rows <- do.call(rbind, lapply(runs, function(e) data.frame(
    run = e[["run"]], step = field(e, "step"), based_on = field(e, "based_on"),
    retry_of = field(e, "retry_of"), status = e[["status"]],
    counted = nm_counted(e[["status"]]),
    evidence = field(e, "evidence"),
    etas = if (is.null(e[["etas"]])) "" else
      paste(names(e[["etas"]]), e[["etas"]], sep = " = ", collapse = "; "),
    stringsAsFactors = FALSE)))
  counted <- sum(rows$counted)
  list(runs = rows,
       totals = data.frame(used_before = used, counted = counted,
                           total = used + counted, max = max))
}

# Writes plot objects to one PDF. A plain list of plots (pmplots returns one
# for several etas) is printed element by element; ggmatrix is eta_pairs'
# class. Any other element stops, naming it, so no table reaches the log.
nm_write_panels <- function(panels, path) {
  is_plot <- function(x) inherits(x, c("ggplot", "patchwork", "ggmatrix"))
  nms <- names(panels)
  for (i in seq_along(panels)) {
    x <- panels[[i]]
    if (!is_plot(x) && !(is.list(x) && !is.object(x) && length(x) &&
                         all(vapply(x, is_plot, logical(1)))))
      stop("nm_write_panels: element ", if (is.null(nms)) i else nms[i],
           " is of class ", class(x)[1], ", not a plot.", call. = FALSE)
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  grDevices::pdf(path, width = 7, height = 5.5)
  on.exit(grDevices::dev.off())
  for (x in panels) if (is_plot(x)) print(x) else for (p in x) print(p)
  path
}

# Counts of kept rows a log-scale panel drops.
nm_log_axis_audit <- function(frame, run) {
  data.frame(run = run, kept_rows = nrow(frame),
             dv_nonpositive = sum(frame[["DV"]] <= 0, na.rm = TRUE),
             pred_nonpositive = sum(frame[["PRED"]] <= 0, na.rm = TRUE),
             ipred_nonpositive = sum(frame[["IPRED"]] <= 0, na.rm = TRUE),
             stringsAsFactors = FALSE)
}

# The two OFVs of one declared nested comparison (B3b, Section 8), as the
# result targets produced them. Both runs must be present, with identical
# verbatim $ESTIMATION text (stricter than a method-name comparison; a
# difference stops and goes to the SME). NA only for a run whose
# minimization_terminated is TRUE (SME word Q13); any other heuristic keeps
# the OFV. The test itself is reference_nested_lrt, called by the target.
nm_lrt_inputs <- function(cmp, res_list) {
  of <- function(run) {
    hit <- Filter(function(r) identical(as.character(r$record$meta$run), run), res_list)
    if (length(hit) != 1L)
      stop("nm_lrt_inputs: run ", run, " is not among the result targets supplied ",
           "(", length(hit), " matches).", call. = FALSE)
    hit[[1]]
  }
  parent <- cmp[["parent"]]; child <- cmp[["child"]]
  p <- of(parent); ch <- of(child)
  est <- function(r) attr(r[["frame"]], "estimation")
  if (!is.character(est(p)) || length(est(p)) != 1L || !identical(est(p), est(ch)))
    stop("nm_lrt_inputs: runs ", parent, " and ", child, " differ in their verbatim ",
         "$ESTIMATION text; the nested comparison is not made. ESCALATE to the SME.",
         call. = FALSE)
  ofv <- function(r, run)
    if (nm_minimization_terminated(r$results$heuristics, run)) NA_real_ else r$results$ofv
  list(ofv_parent = ofv(p, parent), ofv_child = ofv(ch, child))
}

# bm_exponent: the one params row of the declared run whose parameter equals
# spec$parameter exactly, beside the declared RSE bound. within_bound is a
# comparison (NA when rse_pct is NA); nothing is computed from an estimate.
nm_exponent_row <- function(spec, res_list) {
  if (!is.numeric(spec[["rse_bound_pct"]]) || length(spec[["rse_bound_pct"]]) != 1L || !is.finite(spec[["rse_bound_pct"]])) stop("nm_exponent_row: rse_bound_pct must be one finite number.", call. = FALSE)
  run <- spec[["run"]]; par <- spec[["parameter"]]
  hit <- Filter(function(r) identical(as.character(r$record$meta$run), run), res_list)
  if (length(hit) != 1L)
    stop("nm_exponent_row: run ", run, " is not among the result targets supplied ",
         "(", length(hit), " matches).", call. = FALSE)
  p <- hit[[1]]$results$params
  # Columns by exact name ([[ ]]); $ on a data frame partially matches.
  miss <- setdiff(c("parameter", "estimate", "rse_pct"), names(p))
  if (length(miss))
    stop("nm_exponent_row: run ", run, ": the params table lacks column(s) ",
         paste(miss, collapse = ", "), ".", call. = FALSE)
  i <- which(p[["parameter"]] == par)
  if (length(i) != 1L)
    stop("nm_exponent_row: run ", run, ", parameter ", par, " appears ", length(i),
         " times in the params table; exactly one is required.", call. = FALSE)
  data.frame(run = run, parameter = par, estimate = p[["estimate"]][i],
             rse_pct = p[["rse_pct"]][i], rse_bound_pct = spec[["rse_bound_pct"]],
             within_bound = p[["rse_pct"]][i] <= spec[["rse_bound_pct"]],
             stringsAsFactors = FALSE)
}

# A number for a YAML file: the shortest of 15, 16 or 17 significant digits
# that reads back as the identical double, written unquoted through yaml's
# "verbatim" class (yaml::as.yaml's own precision is 7 digits by default and
# would round an OFV; 17 digits prints 3.84 as 3.83999999999999986). A
# mantissa without a point gains ".0" so that yaml reads a float, never a
# string ("1e+05") or an integer. A non-numeric or non-finite value (NA, NaN,
# Inf) is a stop, so no file the consumer would refuse is written.
nm_yaml_number <- function(x) {
  if (!is.numeric(x) || any(!is.finite(x)))
    stop("a non-finite or non-numeric value cannot be written to a hand-off.",
         call. = FALSE)
  s <- vapply(x, function(v) {
    for (d in 15:17) {
      s <- sprintf(paste0("%.", d, "g"), v)
      if (as.numeric(s) == v) break
    }
    if (!grepl(".", s, fixed = TRUE)) s <- sub("^(-?[0-9]+)", "\\1.0", s)
    s
  }, character(1), USE.NAMES = FALSE)
  structure(s, class = "verbatim")
}

# The sha256 of one existing file, as the covariate module computes it.
nm_file_sha256 <- function(path) {
  if (!file.exists(path) || dir.exists(path))
    stop("file ", path, " does not exist.", call. = FALSE)
  digest::digest(file = path, algo = "sha256")
}

# The NONMEM version, read verbatim from the listing line carrying
# "(NONMEM) VERSION" (the .lst writes the program name in parentheses before
# VERSION); a listing without the line, or with lines naming different
# versions, is a stop. The version is never typed.
nm_nonmem_version <- function(lst) {
  if (!file.exists(lst)) stop("listing ", lst, " does not exist.", call. = FALSE)
  hit <- grep("NONMEM\\)? VERSION ", readLines(lst, warn = FALSE), value = TRUE)
  v <- unique(sub("^.*NONMEM\\)? VERSION +([^ ]+).*$", "\\1", hit))
  if (length(v) != 1L)
    stop("listing ", lst, " carries ", length(v), " distinct NONMEM VERSION ",
         "line values; exactly one is required.", call. = FALSE)
  if (!grepl("^[0-9][0-9A-Za-z.]*$", v))
    stop("listing ", lst, " carries a NONMEM VERSION line without a version ",
         "token.", call. = FALSE)
  v
}

# base_model_frozen_yaml (B3c, plan Section 9; design Section 5): per declared
# covariate comparison, in declared order, map-inputs-<name>.yaml,
# run-config-<name>.yaml and handoff-<name>.yaml under out_dir, with the field
# sets pipeline/tasks/covariate.R validates. Every value comes from a result
# target, a run file, the weight-constants target or the declaration; nothing
# is computed. Every comparison is checked before any file is written, so a
# refusal (failed run or missing OFV, differing $ESTIMATION text, a run not
# among the results, a listing without the NONMEM VERSION line) writes no file.
# Returns the written paths, three per comparison (hand-off, MAP inputs, run
# configuration). The same inputs give byte-identical files: fixed key order,
# no time but the child fit's recorded run_start, numbers by nm_yaml_number.
#   comparisons       the declaration's covariate_comparisons (named list)
#   res_list          the bm_res_<run> values of the runs the comparisons name
#   streams           named character vector, run -> control stream path
#                     (the bm_ctl_<run>_file values)
#   data_file         the NONMEM data file (dataset_file)
#   weight_constants  the da_weight_constants value (quantity, value_kg)
#   exponent          the declaration's exponent block, or NULL
#   priors            the declaration's covariate_priors (MAP 4.5 rows)
#   model_dir, out_dir, analysis_id, owner   declared constants
nm_write_handoffs <- function(comparisons, res_list, streams, data_file,
                              weight_constants, exponent, priors, model_dir,
                              out_dir, analysis_id, owner) {
  num <- nm_yaml_number
  ts <- function(x) format(x, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  result_of <- function(run) {
    hit <- Filter(function(r) identical(as.character(r$record$meta$run), run), res_list)
    if (length(hit) != 1L)
      stop("run ", run, " is not among the result targets supplied (", length(hit),
           " matches).", call. = FALSE)
    hit[[1]]
  }
  usable <- function(run) {
    r <- result_of(run)
    if (nm_minimization_terminated(r$results$heuristics, run))
      stop("run ", run, " failed (minimization_terminated TRUE, SME word Q13); ",
           "no hand-off is written. ESCALATE to the SME.", call. = FALSE)
    ofv <- r$results[["ofv"]]
    if (!is.numeric(ofv) || length(ofv) != 1L || !is.finite(ofv))
      stop("run ", run, " has no finite OFV; no hand-off is written.", call. = FALSE)
    r
  }
  file_entry <- function(path) list(path = path, sha256 = nm_file_sha256(path))
  run_entry <- function(run) {
    r <- usable(run)
    est <- r$record$est
    if (!is.character(est) || length(est) != 1L || is.na(est))
      stop("run ", run, " carries no recorded $ESTIMATION text; no hand-off is ",
           "written.", call. = FALSE)
    dir <- file.path(model_dir, run)
    list(run = run, directory = dir, estimation = r$record$est,
         files = list(lst = file_entry(file.path(dir, paste0(run, ".lst"))),
                      ext = file_entry(file.path(dir, paste0(run, ".ext"))),
                      bbi_config = file_entry(file.path(dir, "bbi_config.json"))))
  }
  weight <- function(label) {
    q <- weight_constants[["quantity"]]
    i <- which(sub(":.*$", "", q) == label)
    if (length(i) != 1L)
      stop("weight point ", label, " appears ", length(i), " times in the weight ",
           "constants; exactly one is required.", call. = FALSE)
    weight_constants[["value_kg"]][i]
  }
  stream_of <- function(run) {
    if (!run %in% names(streams))
      stop("run ", run, " has no control stream among those supplied.", call. = FALSE)
    streams[[run]]
  }

  # Everything is gathered and checked first; nothing is written until every
  # comparison has passed.
  plan <- lapply(names(comparisons), function(nm) tryCatch({
    cmp <- comparisons[[nm]]
    prior <- cmp[["prior"]]
    valid_prior <- length(prior) > 0L && !anyNA(prior) &&
      if (is.numeric(prior)) all(is.finite(prior) & prior == floor(prior) & prior >= 1 & prior <= length(priors))
      else is.character(prior) && all(nzchar(prior) & prior %in% names(priors))
    if (!valid_prior) stop("nm_write_handoffs: comparison ", nm,
      " has invalid prior reference(s) ", if (length(prior)) paste(prior, collapse = ", ") else "<empty>",
      "; use present prior names or whole-number indices within the prior list.", call. = FALSE)
    unit <- cmp[["unit"]]
    if (identical(unit, "nested_lrt")) {
      parent <- cmp[["parent"]]; child <- cmp[["child"]]
      runs <- list(parent = run_entry(parent), child = run_entry(child))
      ofv <- nm_lrt_inputs(cmp, res_list)
      criteria <- list(criterion_dofv = num(cmp[["criterion_dofv"]]),
                       df = num(cmp[["df"]]), alpha = num(cmp[["alpha"]]))
      arguments <- list(ofv_parent = num(ofv$ofv_parent), ofv_child = num(ofv$ofv_child))
    } else if (identical(unit, "covariate_effect")) {
      parent <- child <- cmp[["run"]]
      runs <- list(selected = run_entry(child))
      exps <- cmp[["exponent_fixed"]]
      if (is.null(exps)) {
        if (!identical(exponent[["run"]], child))
          stop("the estimated exponent is read from the declared exponent row, ",
               "which names run ", exponent[["run"]], ", not run ", child, ".",
               call. = FALSE)
        exps <- nm_exponent_row(exponent, res_list)$estimate
      }
      criteria <- list(band_lower = num(cmp[["band_lower"]]),
                       band_upper = num(cmp[["band_upper"]]))
      arguments <- list(wt_low = num(weight(cmp[["wt_points"]][1])),
                        wt_high = num(weight(cmp[["wt_points"]][2])),
                        exponents = num(exps))
    } else {
      stop("unit must be nested_lrt or covariate_effect.", call. = FALSE)
    }
    start <- result_of(child)$record$run_start
    if (!inherits(start, "POSIXct") || length(start) != 1L || is.na(start))
      stop("run ", child, " carries no recorded run_start.", call. = FALSE)
    list(
      name = nm, unit = unit, parent = parent, child = child, runs = runs,
      version = nm_nonmem_version(file.path(model_dir, child, paste0(child, ".lst"))),
      created_at = ts(start),
      model = file_entry(stream_of(parent)), data = file_entry(data_file),
      map = list(method_unit = unit, priors = unname(priors[cmp[["prior"]]]),
                 criteria = criteria, arguments = arguments))
  }, error = function(e)
    stop("nm_write_handoffs: comparison ", nm, ": ", conditionMessage(e), call. = FALSE)))

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  unlist(lapply(plan, function(p) {
    f <- file.path(out_dir, paste0(c("handoff-", "map-inputs-", "run-config-"),
                                   p$name, ".yaml"))
    yaml::write_yaml(p$map, f[2])
    yaml::write_yaml(list(comparison = p$name, method_unit = p$unit, runs = p$runs), f[3])
    yaml::write_yaml(list(
      schema_version = num(1), source = "upstream", method_unit = p$unit,
      producing_analysis_id = analysis_id, producing_run_id = p$child,
      parent_fit_id = p$parent, owner = owner, created_at = p$created_at,
      engine = list(name = "NONMEM", version = p$version),
      model = p$model, data = p$data,
      run_config = file_entry(f[3]), map_inputs = file_entry(f[2])), f[1])
    f
  }))
}
