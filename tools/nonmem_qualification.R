# NONMEM qualification shaping helpers.
#
# Functions only. Every name starts with nq_, no top-level code runs, and no
# helper computes an analysis number: they read files, check digests and
# shapes, select by exact name or by declared position, join by ID or by
# parameter name, rename and bind what the engine, PsN and the approved
# reference units returned. The one integer offset is the column index
# `start + position` in nq_psn_patterns (a header index, no data value). Named
# by no method manifest.
#
# Callers: pipeline/tasks/qualification.R (NONMEM branch) and the NONMEM
# battery (tools/tests/test_nonmem_base_model.R, part Q). Counting checks use
# length(which(...)) so that the file stays clear of the arithmetic census.

# The sha256 of `copy`, after checking that both files exist and that their
# digests are equal.
nq_check_copy <- function(copy, original) {
  for (f in c(copy, original))
    if (!file.exists(f)) stop("nq_check_copy: ", f, " does not exist.", call. = FALSE)
  d_copy <- digest::digest(file = copy, algo = "sha256")
  d_orig <- digest::digest(file = original, algo = "sha256")
  if (!identical(d_copy, d_orig))
    stop("nq_check_copy: ", copy, " is not a byte copy of ", original,
         " (sha256 differs); PsN would read other bytes than the signed ones.",
         call. = FALSE)
  d_copy
}

# Stops unless the declared parameter names equal the final run's parameter
# names as a set, naming the extra and the missing ones. Returns the declared
# names.
nq_check_parameters <- function(names, engine_names) {
  extra <- setdiff(names, engine_names)
  missing <- setdiff(engine_names, names)
  if (length(extra) || length(missing))
    stop("nq_check_parameters: declared parameter(s) not in the final run: ",
         if (length(extra)) paste(extra, collapse = ", ") else "none",
         "; final-run parameter(s) not declared: ",
         if (length(missing)) paste(missing, collapse = ", ") else "none", ".",
         call. = FALSE)
  names
}

# One column of the results reader's params frame (default `estimate`),
# selected by exact parameter name, as a vector named by `names`.
nq_estimates <- function(params, names, column = "estimate") {
  if (!column %in% names(params))
    stop("nq_estimates: the params frame has no column ", column, ".", call. = FALSE)
  vapply(names, function(p) {
    i <- which(params[["parameter"]] == p)
    if (length(i) != 1L)
      stop("nq_estimates: parameter ", p, " appears ", length(i),
           " times in the params frame; exactly one is required.", call. = FALSE)
    params[[column]][i]
  }, numeric(1))
}

# The condition number from bbr's model summary (Q-B5-8 (b)). The field is a
# list of one holding a named list whose `condition_number` element is a
# finite numeric scalar (P3b on run 209); that element is returned as read.
# NULL gives NA_real_; every other shape, and a non-finite value, stops.
nq_condition_number <- function(mod, reader = bbr::model_summary) {
  cn <- reader(mod)[["condition_number"]]
  if (is.null(cn)) return(NA_real_)
  inner <- if (is.list(cn) && !is.object(cn) && length(cn) == 1L) cn[[1L]] else NULL
  value <- if (is.list(inner) && !is.null(names(inner))) inner[["condition_number"]] else NULL
  if (!is.numeric(value) || length(value) != 1L || !is.finite(value))
    stop("nq_condition_number: model_summary()$condition_number is not a list of one ",
         "named list carrying one finite numeric condition_number; refusing to guess ",
         "the value.", call. = FALSE)
  value
}

# The covariance-step estimation list of reference_qualification_verdict_covstep.
# `kinds` is the declared kind per parameter, named by parameter.
nq_estimation_contract <- function(results, message, condition_number, kinds, run) {
  fail <- function(...) stop("nq_estimation_contract (run ", run, "): ", ..., call. = FALSE)
  p <- results[["params"]]
  absent <- setdiff(c("parameter", "rse_pct", "shrink_sd_pct"), names(p))
  if (length(absent))
    fail("the results' params frame has no column(s) ", paste(absent, collapse = ", "), ".")
  idx <- vapply(names(kinds), function(nm) {
    i <- which(p[["parameter"]] == nm)
    if (length(i) != 1L)
      fail("declared parameter ", nm, " appears ", length(i),
           " times in the results; exactly one is required.")
    i
  }, integer(1))
  aborted <- results[["heuristics"]][["covariance_step_aborted"]]
  if (!is.logical(aborted) || length(aborted) != 1L || is.na(aborted))
    fail("the heuristics carry no covariance_step_aborted flag that is one non-NA logical.")
  list(
    params = data.frame(parameter = names(kinds), kind = unname(kinds),
                        rse_pct = p[["rse_pct"]][idx],
                        shrink_sd_pct = ifelse(unname(kinds) == "iiv",
                                               p[["shrink_sd_pct"]][idx], NA_real_),
                        stringsAsFactors = FALSE),
    minimization_successful = !nm_minimization_terminated(results[["heuristics"]], run),
    covariance_step_successful = !aborted,
    condition_number = condition_number,
    termination_message = message)
}

# The engine's shrink_sd_pct as the results reader returns it, for every
# declared parameter, selected by name (table 4's engine column).
nq_engine_shrinkage <- function(params, names) nq_estimates(params, names, "shrink_sd_pct")

# The observed and simulated tables of one PsN vpc run: exactly
# m1/vpc_original.npctab.dta and m1/vpc_simulation.1.npctab.dta. Each file is
# split at its TABLE NO. lines; every block must carry exactly `columns`; the
# observed file has one block, the simulated file `samples` blocks of the
# observed size. REP, the ordinal of the simulated block, is added to the
# simulated rows.
nq_vpc_tables <- function(run_dir, samples, columns) {
  m1 <- file.path(run_dir, "m1")
  obs_file <- file.path(m1, "vpc_original.npctab.dta")
  sim_file <- file.path(m1, "vpc_simulation.1.npctab.dta")
  for (f in c(obs_file, sim_file))
    if (!file.exists(f)) stop("nq_vpc_tables: ", f, " does not exist.", call. = FALSE)
  others <- setdiff(list.files(m1, pattern = "^vpc_simulation\\.[0-9]+\\.npctab\\.dta$"),
                    basename(sim_file))
  if (length(others))
    stop("nq_vpc_tables: ", m1, " holds other simulation tables (",
         paste(others, collapse = ", "), "); only vpc_simulation.1 is read.", call. = FALSE)
  blocks <- function(path) {
    lines <- readLines(path, warn = FALSE)
    starts <- grepl("^TABLE NO\\.", lines)
    if (!any(starts))
      stop("nq_vpc_tables: ", path, " has no TABLE NO. line.", call. = FALSE)
    if (!starts[1L])
      stop("nq_vpc_tables: ", path, " does not begin with a TABLE NO. line.", call. = FALSE)
    block_of <- findInterval(seq_along(lines), which(starts))
    lapply(split(lines, block_of), function(b) {
      header <- strsplit(trimws(b[2L]), "\\s+")[[1L]]
      if (!identical(header, columns))
        stop("nq_vpc_tables: a block of ", path, " carries the columns ",
             paste(header, collapse = " "), "; the declared columns are ",
             paste(columns, collapse = " "), ".", call. = FALSE)
      body <- b[seq_along(b) > 2L]
      utils::read.table(text = body, col.names = columns, colClasses = "numeric")
    })
  }
  obs <- blocks(obs_file)
  sim <- blocks(sim_file)
  if (length(obs) != 1L)
    stop("nq_vpc_tables: the observed table holds ", length(obs),
         " blocks; exactly one is required.", call. = FALSE)
  if (length(sim) != samples)
    stop("nq_vpc_tables: the simulated table holds ", length(sim),
         " blocks; the declared sample count is ", samples, ".", call. = FALSE)
  if (!all(vapply(sim, nrow, integer(1)) == nrow(obs[[1L]])))
    stop("nq_vpc_tables: a simulated block differs in size from the observed table.",
         call. = FALSE)
  sim <- lapply(seq_along(sim), function(k) {
    s <- sim[[k]]
    s[["REP"]] <- k
    s
  })
  rows <- do.call(rbind, sim)
  rownames(rows) <- NULL
  list(obs = obs[[1L]], sim = rows)
}

# Adds `cols` to `df` from a one-row-per-subject frame, matched on ID. An NA
# in an attached column stops (it would form a silent NA stratum).
nq_attach_subject <- function(df, subject, cols) {
  if (length(setdiff(cols, names(subject)))) stop("nq_attach_subject: subject frame lacks column(s) ", paste(setdiff(cols, names(subject)), collapse = ", "), ".", call. = FALSE)
  if (anyDuplicated(subject[["ID"]]))
    stop("nq_attach_subject: the subject frame holds an ID twice.", call. = FALSE)
  i <- match(df[["ID"]], subject[["ID"]])
  if (anyNA(i))
    stop("nq_attach_subject: ID(s) without a subject row: ",
         paste(unique(df[["ID"]][is.na(i)]), collapse = ", "), ".", call. = FALSE)
  for (cl in cols) {
    df[[cl]] <- subject[[cl]][i]
    if (anyNA(df[[cl]]))
      stop("nq_attach_subject: the attached column ", cl, " carries NA.", call. = FALSE)
  }
  df
}

# Exact, anchored column patterns for reference_psn_bootstrap_samples, one per
# declared parameter, picked by position from PsN's column map. `parameters`
# is the named list of psn maps (block, position, heading). The header is read
# the way the PsN unit reads the file (quote-aware read.csv, check.names
# FALSE); the [0] section of raw_results_structure gives each block's
# zero-based start and length, so the one-based header index of position k is
# start + k. Returns the patterns with the picked names as attribute `picked`.
nq_psn_patterns <- function(raw_csv, structure, parameters) {
  hdr <- names(utils::read.csv(raw_csv, check.names = FALSE, nrows = 1))
  lines <- trimws(readLines(structure, warn = FALSE))
  s0 <- which(lines == "[0]")
  if (length(s0) != 1L)
    stop("nq_psn_patterns: ", structure, " has no single [0] section.", call. = FALSE)
  idx <- seq_along(lines)
  later <- which(idx > s0 & grepl("^\\[", lines))
  in0 <- idx > s0 & (if (length(later)) idx < later[1L] else TRUE)
  kv <- regmatches(lines[in0], regexec("^([A-Za-z_]+)=([0-9]+),([0-9]+)$", lines[in0]))
  kv <- kv[lengths(kv) == 4L]
  field <- vapply(kv, `[`, "", 2L)
  blocks <- vapply(parameters, function(m) m[["block"]], "")
  picked <- vapply(names(parameters), function(nm) {
    m <- parameters[[nm]]
    k <- which(field == m[["block"]])
    if (length(k) != 1L)
      stop("nq_psn_patterns: the [0] section has no single ", m[["block"]], " entry.",
           call. = FALSE)
    start <- as.integer(kv[[k]][3L])
    len <- as.integer(kv[[k]][4L])
    if (len != length(which(blocks == m[["block"]])))
      stop("nq_psn_patterns: the ", m[["block"]], " block holds ", len,
           " columns; the declaration names ", length(which(blocks == m[["block"]])),
           " parameters of that block.", call. = FALSE)
    position <- as.integer(m[["position"]])
    if (!position %in% seq_len(len))
      stop("nq_psn_patterns: ", nm, " position ", position, " is outside its block.",
           call. = FALSE)
    col <- start + position
    if (!col %in% seq_along(hdr))
      stop("nq_psn_patterns: ", nm, " points past the header.", call. = FALSE)
    heading <- m[["heading"]]
    if (!is.na(heading) && !grepl(heading, hdr[col]))
      stop("nq_psn_patterns: ", nm, " picks the column '", hdr[col],
           "', which does not match the declared heading ", heading, ".", call. = FALSE)
    hdr[col]
  }, "")
  if (anyDuplicated(picked))
    stop("nq_psn_patterns: the picked columns are not distinct: ",
         paste(unique(picked[duplicated(picked)]), collapse = ", "), ".", call. = FALSE)
  pat <- paste0("^", gsub("([][{}()|.^$*+?\\\\])", "\\\\\\1", picked), "$")
  names(pat) <- names(parameters)
  for (nm in names(pat)) {
    n_hit <- length(grep(pat[[nm]], hdr))
    if (n_hit != 1L)
      stop("nq_psn_patterns: the exact pattern for ", nm, " matches ", n_hit,
           " header columns; exactly one is required.", call. = FALSE)
  }
  structure(pat, picked = picked)
}

# The two bootstrap frames of the covstep contract and the success rate.
# `kinds` is named by parameter; `rse`, `pct_diff` and `ci_finite` are vectors
# named by parameter (rse for the fixed and iiv parameters, pct_diff for the
# fixed ones, ci_finite for every parameter).
nq_bootstrap_tables <- function(kinds, rse, pct_diff, ci_finite, success_rate_pct) {
  pick <- function(x, p, what) {
    miss <- setdiff(p, names(x))
    if (length(miss))
      stop("nq_bootstrap_tables: no ", what, " for ", paste(miss, collapse = ", "), ".",
           call. = FALSE)
    unname(x[p])
  }
  fx <- names(kinds)[kinds == "fixed"]
  iv <- names(kinds)[kinds == "iiv"]
  list(fixed = data.frame(parameter = fx, rse_pct = pick(rse, fx, "rse"),
                          pct_diff_natural_scale = pick(pct_diff, fx, "pct_diff"),
                          ci_finite = pick(ci_finite, fx, "ci_finite"),
                          stringsAsFactors = FALSE),
       iiv = data.frame(parameter = iv, rse_pct = pick(rse, iv, "rse"),
                        ci_finite = pick(ci_finite, iv, "ci_finite"),
                        stringsAsFactors = FALSE),
       success_rate_pct = success_rate_pct)
}

# The final-model parameter table (gate table 5), bound by parameter name:
# the estimate and its back-transform, the covariance-step RSE, the bootstrap
# summaries of reference_bootstrap_summaries, the bootstrap RSE where it is
# defined (NA otherwise), interval finiteness and the PsN column picked.
nq_parameter_table <- function(parameters, estimate, back_transformed, covariance_rse,
                               summaries, bootstrap_rse, ci_finite, psn_column) {
  p <- names(parameters)
  pick <- function(x, what) {
    miss <- setdiff(p, names(x))
    if (length(miss))
      stop("nq_parameter_table: no ", what, " for ", paste(miss, collapse = ", "), ".",
           call. = FALSE)
    unname(x[p])
  }
  i <- match(p, summaries[["parameter"]])
  if (anyNA(i))
    stop("nq_parameter_table: no bootstrap summary for ",
         paste(p[is.na(i)], collapse = ", "), ".", call. = FALSE)
  data.frame(
    parameter = p,
    kind = vapply(parameters, function(x) x[["kind"]], "", USE.NAMES = FALSE),
    transform = vapply(parameters, function(x) x[["transform"]], "", USE.NAMES = FALSE),
    estimate_estimation_scale = pick(estimate, "estimate"),
    estimate_natural_scale = pick(back_transformed, "back-transformed estimate"),
    rse_pct_covariance_step_estimation_scale = pick(covariance_rse, "covariance-step RSE"),
    bootstrap_median_natural_scale = summaries[["median_natural"]][i],
    bootstrap_ci_lower_natural_scale = summaries[["ci_lower_natural"]][i],
    bootstrap_ci_upper_natural_scale = summaries[["ci_upper_natural"]][i],
    bootstrap_sd_estimation_scale = summaries[["sd_estimation"]][i],
    bootstrap_rse_pct_estimation_scale = unname(bootstrap_rse[p]),
    ci_finite = pick(ci_finite, "ci_finite"),
    psn_column = pick(psn_column, "PsN column"),
    stringsAsFactors = FALSE)
}

# Gate table 1. Membership is mechanical: the members the registry makes
# mandatory at `model_risk` must equal the members given. `members` is a named
# list of list(units, artifact, detail); each unit is "<skill>/<unit>", and its
# status is read from the skill's MANIFEST.yaml at run time.
nq_battery_members <- function(model_risk, members) {
  reg <- qualification_registry()
  selected <- names(reg)[vapply(reg, function(m) model_risk %in% m[["risk"]], logical(1))]
  if (!setequal(selected, names(members)))
    stop("nq_battery_members: the registry selects ", paste(selected, collapse = ", "),
         " at MODEL_RISK ", model_risk, "; the members given are ",
         paste(names(members), collapse = ", "), ".", call. = FALSE)
  rows <- lapply(selected, function(m) {
    units <- members[[m]][["units"]]
    status <- vapply(units, function(u) {
      part <- strsplit(u, "/", fixed = TRUE)[[1L]]
      st <- read_method_manifest(file.path("skills", part[1L], "MANIFEST.yaml"))[["units"]][[
        part[2L]]][["status"]]
      if (!is.character(st) || length(st) != 1L)
        stop("nq_battery_members: no manifest status for unit ", u, ".", call. = FALSE)
      st
    }, "")
    data.frame(member = m, mandatory = model_risk %in% reg[[m]][["risk"]],
               units = paste(units, collapse = "; "),
               manifest_status = paste(units, status, sep = ": ", collapse = "; "),
               artifact = members[[m]][["artifact"]], detail = members[[m]][["detail"]],
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# Gate table 14: one row per PsN run, read from the files PsN wrote in the run
# directory (command.txt, version_and_option_info.txt). `runs` is a named list
# of the PsN units' run records (tool name -> record with run_dir);
# `operational` the psn block of the run configuration.
nq_invocation_table <- function(runs, operational) {
  ops <- unlist(operational)
  ops <- if (length(ops)) paste(names(ops), ops, sep = "=", collapse = "; ") else "none"
  rows <- lapply(names(runs), function(tool) {
    d <- runs[[tool]][["run_dir"]]
    cmd_file <- file.path(d, "command.txt")
    ver_file <- file.path(d, "version_and_option_info.txt")
    for (f in c(cmd_file, ver_file))
      if (!file.exists(f)) stop("nq_invocation_table: ", f, " does not exist.", call. = FALSE)
    cmd <- readLines(cmd_file, warn = FALSE)
    ver <- grep("^PsN version", readLines(ver_file, warn = FALSE), value = TRUE)
    if (!length(cmd) || length(ver) != 1L)
      stop("nq_invocation_table: ", d, " lacks its command line or its single PsN ",
           "version line.", call. = FALSE)
    tok <- strsplit(trimws(cmd[1L]), "\\s+")[[1L]]
    opt <- function(key) {
      v <- sub(paste0("^-", key, "="), "", grep(paste0("^-", key, "="), tok, value = TRUE))
      if (length(v) != 1L)
        stop("nq_invocation_table: the ", tool, " command line carries no single -", key,
             "= token.", call. = FALSE)
      v
    }
    data.frame(tool = tool, command = cmd[1L],
               psn_version = trimws(sub("^PsN version:?", "", ver)),
               run_dir = nq_relative_path(d), samples = opt("samples"), seed = opt("seed"),
               operational = ops, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# Paths relative to the repository root (the working directory of the
# pipeline), for artifact rows. A path outside the root stops.
nq_relative_path <- function(path) {
  root <- paste0(normalizePath(getwd(), mustWork = TRUE), .Platform$file.sep)
  full <- normalizePath(path, mustWork = TRUE)
  inside <- startsWith(full, root)
  if (!all(inside))
    stop("nq_relative_path: path(s) outside the repository root: ",
         paste(path[!inside], collapse = ", "), ".", call. = FALSE)
  vapply(full, function(x) sub(root, "", x, fixed = TRUE), "", USE.NAMES = FALSE)
}
