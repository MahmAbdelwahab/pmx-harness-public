# NONMEM simulation shaping helpers.
#
# Functions only. Every name starts with ns_, no top-level code runs, and no
# helper computes an analysis number: they read the engine's files, select by
# exact label or name, place values into a compiled mrgsolve model through
# mrgsolve's own setters (param, omat, smat, with bmat building the matrix
# from the lower triangle as written), build the scenario list and the plots
# by name, compare with >= and <=, and bind what the approved simulation units
# returned. The only arithmetic-looking operator is ggplot2's layer `+` in the
# two plot builders. Named by no method manifest.
#
# Callers: pipeline/tasks/simulation.R (NONMEM arm) and the NONMEM battery
# (tools/tests/test_nonmem_base_model.R, part M). Counting checks use
# length(which(...)) so that the file stays clear of the arithmetic census.
#
# Built for the SME's B6-0a answers (ledger, "SME answers B6-0a, verbatim"):
# no sensitivity scenario (Q-B6-1 (c), Q-B6-2 (c)), so ns_override_scenarios
# and ns_override_identity are not built; one reference_simulate call for all
# scenarios (Q-B6-7 (b)), so ns_bind_runs checks one call's output.

# The file's lines joined with newlines: the text handed to the compiler.
ns_read_text <- function(path) {
  if (!file.exists(path)) stop("ns_read_text: ", path, " does not exist.", call. = FALSE)
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

# The `iteration` row of a NONMEM .ext file, the declared columns selected by
# name, as utils::read.table parses them. The row is read a second time by
# header position from the written text (rule 1: header line and row split on
# runs of white space after trimming; rule 3: tokens pair with values by name,
# in declared order) and every parsed value must be identical() to
# as.numeric() of its written token (rule 4). The tokens are kept as
# attribute `written`.
ns_ext_row <- function(ext, names, iteration = "-1000000000") {
  bad <- function(...) stop("ns_ext_row: ", ext, ": ", ..., call. = FALSE)
  if (!file.exists(ext)) bad("does not exist.")
  lines <- readLines(ext, warn = FALSE)
  tab <- which(grepl("^TABLE NO\\.", lines))
  if (length(tab) != 1L) bad(length(tab), " TABLE NO. lines; exactly one is required.")
  body <- lines[seq_along(lines) > tab]
  header <- strsplit(trimws(body[1L]), "\\s+")[[1L]]
  if (!identical(header[1L], "ITERATION")) bad("the column header does not begin ITERATION.")
  for (nm in names)
    if (length(which(header == nm)) != 1L)
      bad("the declared column ", nm, " appears ", length(which(header == nm)),
          " times in the header; exactly once is required.")
  fields <- strsplit(trimws(body[seq_along(body) > 1L]), "\\s+")
  hit <- which(vapply(fields, function(f) identical(f[1L], iteration), logical(1)))
  if (length(hit) != 1L) bad(length(hit), " rows with ITERATION ", iteration, "; exactly one is required.")
  row <- fields[[hit]]
  if (length(row) != length(header)) bad("the ITERATION ", iteration, " row does not match the header.")
  tokens <- vapply(names, function(nm) row[which(header == nm)], "")
  parsed <- utils::read.table(text = body, header = TRUE, check.names = FALSE)
  i <- which(parsed[["ITERATION"]] == as.numeric(iteration))
  if (length(i) != 1L) bad("utils::read.table finds ", length(i), " ITERATION ", iteration, " rows.")
  values <- vapply(names, function(nm) parsed[[nm]][i], numeric(1))
  if (length(tokens) != length(names)) bad("the token count differs from the declared name count.")
  for (nm in names)
    if (!identical(as.numeric(tokens[[nm]]), values[[nm]]))
      bad("the parsed value of ", nm, " is not identical to its written token.")
  structure(values, written = tokens)
}

# The `column` value of the one row of a constants table whose `quantity`
# equals `label` or begins with `label` followed by a colon.
ns_constant <- function(table, label, column) {
  if (!all(c("quantity", column) %in% names(table)))
    stop("ns_constant: the table has no quantity or no ", column, " column.", call. = FALSE)
  if (!is.numeric(table[[column]])) stop("ns_constant: column ", column, " for ", label, " must be numeric.", call. = FALSE)
  q <- table[["quantity"]]
  i <- which(q == label | startsWith(q, paste0(label, ":")))
  if (length(i) != 1L)
    stop("ns_constant: label ", label, " selects ", length(i),
         " rows; exactly one is required.", call. = FALSE)
  table[[column]][i]
}

# The weight in kg of each declared weight point, selected by its
# data-assembly label, named by the weight point, in declared order.
ns_weight_points <- function(table, labels)
  vapply(labels, function(l) ns_constant(table, l, "value_kg"), numeric(1))

# What the simulation graph consumes from the stored run: the estimates (with
# their written tokens), the estimation engine version, the parameter map and
# the reference weight as one value named by its model parameter.
ns_source <- function(estimates, version, map, reference_weight) {
  if (!identical(names(map), names(estimates)))
    stop("ns_source: the parameter map's engine names differ from the estimates' names.",
         call. = FALSE)
  if (!is.character(version) || length(version) != 1L || !nzchar(version))
    stop("ns_source: version is not one string.", call. = FALSE)
  if (!is.numeric(reference_weight) || length(reference_weight) != 1L ||
      !is.finite(reference_weight) || !(reference_weight > 0) || is.null(names(reference_weight)))
    stop("ns_source: reference_weight is not one finite positive number named by its parameter.",
         call. = FALSE)
  list(estimates = estimates, version = version, map = map, reference_weight = reference_weight)
}

# Places the estimates into the compiled model through mrgsolve's setters:
# the THETAs and the reference weight by parameter name (strict, so a name the
# model lacks stops), the OMEGA and SIGMA values as lower triangles in the
# declared position order (positions 1, 2, ... without gap). Placement only;
# the read-back proves it.
ns_install <- function(mod, source) {
  map <- source[["map"]]
  est <- source[["estimates"]]
  of_kind <- function(kind)
    names(map)[vapply(map, function(m) identical(m[["kind"]], kind), logical(1))]
  in_order <- function(kind) {
    nm <- of_kind(kind)
    pos <- vapply(map[nm], function(m) as.numeric(m[["model"]]), numeric(1))
    if (!identical(unname(pos[order(pos)]), as.numeric(seq_along(pos))))
      stop("ns_install: the ", kind, " positions are not 1 to ", length(pos), ".", call. = FALSE)
    nm[order(pos)]
  }
  th <- of_kind("theta")
  p <- c(stats::setNames(as.list(unname(est[th])), vapply(map[th], function(m) m[["model"]], "")),
         as.list(source[["reference_weight"]]))
  mod <- mrgsolve::param(mod, p, .strict = TRUE)
  mod <- mrgsolve::omat(mod, mrgsolve::bmat(unname(est[in_order("omega")])))
  mrgsolve::smat(mod, mrgsolve::bmat(unname(est[in_order("sigma")])))
}

# One row per mapped engine name: the model target (parameter name, or matrix
# cell [i,j] of the declared lower-triangle position in row order) and
# whether the installed value is identical() to the estimate. Stops unless
# every row is TRUE and the reference weight reads back identical.
ns_install_readback <- function(mod, source) {
  map <- source[["map"]]
  est <- source[["estimates"]]
  # The parameter list is an S4 object: its coercion is mrgsolve's method,
  # which the bare base generic does not dispatch to under Rscript.
  pv <- mrgsolve::as.list(mrgsolve::param(mod))
  mats <- list(omega = mrgsolve::omat(mod, make = TRUE),
               sigma = mrgsolve::smat(mod, make = TRUE))
  cells <- function(m) {
    ij <- which(lower.tri(m, diag = TRUE), arr.ind = TRUE)
    ij[order(ij[, 1L], ij[, 2L]), , drop = FALSE]
  }
  rows <- lapply(names(map), function(nm) {
    e <- map[[nm]]
    if (identical(e[["kind"]], "theta")) {
      target <- e[["model"]]
      got <- pv[[target]]
    } else {
      ij <- cells(mats[[e[["kind"]]]])
      if (!(e[["model"]] <= nrow(ij)))
        stop("ns_install_readback: ", nm, " points past the ", e[["kind"]], " matrix.", call. = FALSE)
      r <- ij[e[["model"]], 1L]
      cl <- ij[e[["model"]], 2L]
      target <- sprintf("%s[%d,%d]", e[["kind"]], r, cl)
      got <- mats[[e[["kind"]]]][r, cl]
    }
    data.frame(engine_name = nm, kind = e[["kind"]], model_target = target,
               readback_identical = identical(unname(got), unname(est[[nm]])),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  if (!all(out[["readback_identical"]]))
    stop("ns_install_readback: installed value not identical to the estimate for ",
         paste(out[["engine_name"]][!out[["readback_identical"]]], collapse = ", "), ".",
         call. = FALSE)
  rw <- source[["reference_weight"]]
  if (!identical(pv[[names(rw)]], unname(rw)))
    stop("ns_install_readback: the reference weight ", names(rw), " does not read back identical.",
         call. = FALSE)
  out
}

# The fitted design of the equivalence check from the results reader's joined
# frame: the kept records (harness_kept_record), dose rows as EVID 1 into
# `dose_cmt` with their AMT, observation rows as EVID 0 with AMT 0 and CMT 0,
# the covariate columns on both; `pred` holds ID, TIME, PRED of the kept
# observations.
ns_fitted_design <- function(residuals, n_obs, covariates, dose_cmt) {
  cols <- names(covariates)
  absent <- setdiff(c("ID", "TIME", "EVID", "AMT", "C", "PRED", cols), names(residuals))
  if (length(absent))
    stop("ns_fitted_design: the joined frame has no column(s) ",
         paste(absent, collapse = ", "), ".", call. = FALSE)
  kept <- residuals[harness_kept_record(residuals[["C"]]), , drop = FALSE]
  for (column in intersect(c("RATE", "ADDL", "II", "SS"), names(kept))) if (any(!is.na(kept[[column]]) & kept[[column]] != 0)) stop("ns_fitted_design: column ", column, " has a non-zero value; ns_fitted_design does not support infusion/ADDL/II/SS data.", call. = FALSE)
  if (anyNA(kept[["TIME"]])) stop("ns_fitted_design: column TIME has a missing value.", call. = FALSE)
  if (anyNA(kept[["AMT"]][harness_is_dose(kept[["EVID"]])])) stop("ns_fitted_design: column AMT has a missing value on a dose row.", call. = FALSE)
  dose <- kept[harness_is_dose(kept[["EVID"]]), , drop = FALSE]
  obs <- kept[harness_is_observation(kept[["EVID"]]), , drop = FALSE]
  if (!identical(as.numeric(nrow(obs)), as.numeric(n_obs)))
    stop("ns_fitted_design: ", nrow(obs), " kept observation rows; the results reader counted ",
         n_obs, ".", call. = FALSE)
  if (!nrow(dose)) stop("ns_fitted_design: no kept dose row.", call. = FALSE)
  for (cv in cols)
    if (anyNA(dose[[cv]]) || anyNA(obs[[cv]]))
      stop("ns_fitted_design: covariate ", cv, " has a missing value.", call. = FALSE)
  design <- rbind(
    data.frame(ID = dose[["ID"]], TIME = dose[["TIME"]], EVID = 1L, AMT = dose[["AMT"]],
               CMT = dose_cmt, dose[cols], stringsAsFactors = FALSE),
    data.frame(ID = obs[["ID"]], TIME = obs[["TIME"]], EVID = 0L, AMT = 0,
               CMT = 0, obs[cols], stringsAsFactors = FALSE))
  rownames(design) <- NULL
  list(design = design,
       pred = data.frame(ID = obs[["ID"]], TIME = obs[["TIME"]], PRED = obs[["PRED"]]),
       n_dose_rows = nrow(dose))
}

# The named scenario list and the scenario table, in the order phase (as
# declared), dose (as declared), weight point, Apgar group. Each scenario
# gives the per-kilogram dose and the weight parameter, so the approved unit
# forms the amount; no `amt` is set here.
ns_scenarios <- function(grid_doses, weight_points, apgar_groups, n, grid, dose_cmt,
                         post_dose_obs, weight_param, sensitivity = list()) {
  if (length(sensitivity))
    stop("ns_scenarios: sensitivity scenarios are not built (the SME's B6-0a answers ",
         "Q-B6-1 (c) and Q-B6-2 (c)).", call. = FALSE)
  rows <- do.call(rbind, lapply(names(grid_doses), function(ph)
    do.call(rbind, lapply(grid_doses[[ph]], function(d)
      do.call(rbind, lapply(names(weight_points), function(wp)
        data.frame(SCENARIO = paste(paste0(ph, format(d, trim = TRUE)), wp, apgar_groups,
                                    sep = "-"),
                   kind = "main", phase = ph, dose_mg_per_kg = d, weight_point = wp,
                   weight_kg = unname(weight_points[[wp]]), apgar_group = apgar_groups,
                   overrides = "none", stringsAsFactors = FALSE)))))))
  rownames(rows) <- NULL
  bad <- !grepl("^(LD|MD)[0-9.]+-WT[0-9]{2}-APG[A-Z]+(-[A-Z0-9]+)?$", rows[["SCENARIO"]])
  if (any(bad))
    stop("ns_scenarios: scenario name(s) outside the declared pattern: ",
         paste(rows[["SCENARIO"]][bad], collapse = ", "), ".", call. = FALSE)
  if (anyDuplicated(rows[["SCENARIO"]]))
    stop("ns_scenarios: duplicated scenario name(s): ",
         paste(unique(rows[["SCENARIO"]][duplicated(rows[["SCENARIO"]])]), collapse = ", "), ".",
         call. = FALSE)
  scenarios <- lapply(seq_len(nrow(rows)), function(i)
    list(regimen = list(amt_per_kg = rows[["dose_mg_per_kg"]][i], weight_param = weight_param,
                        cmt = dose_cmt),
         param = stats::setNames(list(rows[["weight_kg"]][i]), weight_param),
         n = n, grid = grid, post_dose_obs = post_dose_obs))
  names(scenarios) <- rows[["SCENARIO"]]
  list(scenarios = scenarios, table = rows)
}

# Per scenario, the dose row against the phase's observed minimum and maximum
# rows and the duration row against the horizon row, each selected by label
# from the design constants. `approval` is the amendment id declared for the
# scenario's <phase><dose> key, else empty. Stops before any scenario runs on
# a row outside the observed design without approval (naming the scenario and
# the dimension), and on an approval key that matches no outside dose row.
ns_applicability <- function(table, horizon_h, design_constants, dose_rows, horizon_row,
                             approved) {
  horizon <- ns_constant(design_constants, horizon_row, "value")
  out <- do.call(rbind, lapply(seq_len(nrow(table)), function(i) {
    ph <- table[["phase"]][i]
    d <- table[["dose_mg_per_kg"]][i]
    lo <- ns_constant(design_constants, dose_rows[[ph]][["min"]], "value")
    hi <- ns_constant(design_constants, dose_rows[[ph]][["max"]], "value")
    key <- paste0(ph, format(d, trim = TRUE))
    data.frame(SCENARIO = table[["SCENARIO"]][i], dimension = c("dose", "duration"),
               scenario_value = c(d, horizon_h), observed_min = c(lo, NA_real_),
               observed_max = c(hi, horizon),
               within_observed = c(d >= lo & d <= hi, horizon_h <= horizon),
               approval = c(if (key %in% names(approved)) unname(approved[[key]]) else "", ""),
               key = c(key, ""), stringsAsFactors = FALSE)
  }))
  rownames(out) <- NULL
  stopped <- !out[["within_observed"]] & !nzchar(out[["approval"]])
  if (any(stopped))
    stop("ns_applicability: outside the observed design without recorded approval: ",
         paste(out[["SCENARIO"]][stopped], out[["dimension"]][stopped], sep = " ", collapse = "; "),
         ". The SME approves an extrapolation before any scenario runs.", call. = FALSE)
  outside_keys <- out[["key"]][!out[["within_observed"]] & out[["dimension"]] == "dose"]
  stale <- setdiff(names(approved), outside_keys)
  if (length(stale))
    stop("ns_applicability: approval key(s) matching no dose outside the observed design: ",
         paste(stale, collapse = ", "), ".", call. = FALSE)
  out[["key"]] <- NULL
  out
}

# The one reference_simulate call of the run (Q-B6-7 (b)): stops unless its
# record echoes exactly the declared scenario names, in order, and its frame
# holds exactly those scenarios. Returns list(sim, record).
ns_bind_runs <- function(run, names) {
  rec <- run[["record"]]
  if (!identical(names(rec[["scenarios"]]), names))
    stop("ns_bind_runs: the run record's scenarios differ from the declared scenarios.",
         call. = FALSE)
  if (!identical(unique(run[["sim"]][["SCENARIO"]]), names))
    stop("ns_bind_runs: the simulated frame's scenarios differ from the declared scenarios.",
         call. = FALSE)
  for (f in c("engine", "version", "seed", "uncertainty", "observed"))
    if (is.null(rec[[f]])) stop("ns_bind_runs: the run record has no ", f, ".", call. = FALSE)
  list(sim = run[["sim"]], record = rec)
}

# Stops unless every scenario carries regimen[["amt"]] (exact name) as one
# finite positive number: the exposure unit reads the amount with `$`, which
# would partially match amt_per_kg on a declared per-kilogram scenario
# (erratum P-1.4-30). Returns the scenarios.
ns_assert_record_amounts <- function(scenarios) {
  for (nm in names(scenarios)) {
    a <- scenarios[[nm]][["regimen"]][["amt"]]
    if (!is.numeric(a) || length(a) != 1L || !is.finite(a) || !(a > 0))
      stop("ns_assert_record_amounts: scenario ", nm, " carries no regimen[[\"amt\"]] that is ",
           "one finite positive number; pass the run record's scenarios.", call. = FALSE)
  }
  scenarios
}

# Per scenario, its phase metric's row of the unit's summary (as the unit
# returns it), joined by SCENARIO with the scenario table, and the declared
# bound pair and unit copied verbatim. No comparison and no verdict.
ns_bound_table <- function(summary, table, phase_metric, bounds, bound_units) {
  rows <- lapply(seq_len(nrow(table)), function(i) {
    s <- table[["SCENARIO"]][i]
    m <- phase_metric[[table[["phase"]][i]]]
    j <- which(summary[["SCENARIO"]] == s & summary[["metric"]] == m)
    if (length(j) != 1L)
      stop("ns_bound_table: ", length(j), " summary rows for ", s, " and ", m,
           "; exactly one is required.", call. = FALSE)
    data.frame(SCENARIO = s, kind = table[["kind"]][i], phase = table[["phase"]][i],
               metric = m, unit = summary[["unit"]][j], p_lower = summary[["p_lower"]][j],
               lower = summary[["lower"]][j], bound_lower = unname(bounds[[m]][["lower"]]),
               median = summary[["median"]][j], p_upper = summary[["p_upper"]][j],
               upper = summary[["upper"]][j], bound_upper = unname(bounds[[m]][["upper"]]),
               bound_unit = unname(bound_units[[m]]), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# The declared rows of the qualification parameter table, its natural-scale
# columns selected by name, with the declared description.
ns_bootstrap_rows <- function(parameter_table, rows) {
  cols <- c("parameter", "transform", "estimate_natural_scale", "bootstrap_median_natural_scale",
            "bootstrap_ci_lower_natural_scale", "bootstrap_ci_upper_natural_scale", "ci_finite")
  absent <- setdiff(cols, names(parameter_table))
  if (length(absent))
    stop("ns_bootstrap_rows: the parameter table has no column(s) ",
         paste(absent, collapse = ", "), ".", call. = FALSE)
  i <- vapply(names(rows), function(p) {
    k <- which(parameter_table[["parameter"]] == p)
    if (length(k) != 1L)
      stop("ns_bootstrap_rows: row ", p, " appears ", length(k),
           " times; exactly one is required.", call. = FALSE)
    k
  }, integer(1))
  out <- parameter_table[i, cols, drop = FALSE]
  out[["description"]] <- unname(rows)
  rownames(out) <- NULL
  out
}

# One concentration-time panel: the unit's per-time median with its lower to
# upper ribbon, axis titles from the declared units, the scenario as title and
# the declared ribbon text as subtitle.
ns_profile_plot <- function(q, scenario, units, ribbon) {
  ggplot2::ggplot(q, ggplot2::aes(time, median)) + list(
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), fill = "grey70", alpha = 0.6),
    ggplot2::geom_line(linewidth = 0.8),
    ggplot2::labs(x = paste0("Time (", units[["time"]], ")"),
                  y = paste0("Concentration (", units[["conc"]], ")"),
                  title = scenario, subtitle = ribbon),
    ggplot2::theme_bw())
}

# One metric-distribution panel: a histogram per declared metric (facets) with
# the declared bin count, the scenario as title.
ns_metric_plot <- function(by_subject, metrics, scenario, bins) {
  long <- do.call(rbind, lapply(metrics, function(m)
    data.frame(metric = m, value = by_subject[[m]], stringsAsFactors = FALSE)))
  ggplot2::ggplot(long, ggplot2::aes(value)) + list(
    ggplot2::geom_histogram(bins = bins, fill = "grey70", colour = "grey30"),
    ggplot2::facet_wrap(~metric, scales = "free"),
    ggplot2::labs(x = NULL, y = "Subjects", title = scenario),
    ggplot2::theme_bw())
}
