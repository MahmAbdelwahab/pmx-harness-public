# nlmixr2 run-record discipline (skills/model-management.md, nlmixr2 mode).
#
# Each candidate = a numbered model file in pilot/model/ defining one model
# function plus a `meta` list (run id, based_on, hypothesis / expected —
# written BEFORE the run). run_candidate() executes the fit deterministically
# and returns the full record; candidate_table() builds the comparison table
# presented at the base-model gate. Failed fits are kept as records, never
# dropped (modeler role rule 4).

# Executes one estimation run. Hard stop when the MAP run budget (D5) is
# already spent — the budget is raised only by the SME, never in code.
# Returns the full run record (meta, counts, OFV/AIC, convergence verdict,
# runtime); on error the record carries failed = TRUE plus the message, so
# a failed fit remains a first-class record for the gate table.
run_candidate <- function(model_fn, meta, data, est = "focei",
                          budget_used, budget_max) {
  if (budget_used >= budget_max) {
    stop("Estimation-run budget exhausted (", budget_used, "/", budget_max,
         ", MAP D5). ESCALATE to SME — do not raise the budget in code.",
         call. = FALSE)
  }
  # The fitting set is selected by THE harness kept-record rule
  # (tools/record_selection.R::harness_kept_record, Codex round 7, finding 2):
  # NA, "", whitespace-only and "." mean kept; any other C value is an
  # exclusion reason. Before 2026-09-03 this line kept `is.na(C)` only, so a
  # dataset whose blank cells arrived as "" was fitted on a different record
  # set than the EDA described and the QC battery checked.
  fit_data <- data[harness_kept_record(data$C), ]     # C-flagged records excluded
  t0 <- Sys.time()
  fit <- tryCatch(
    suppressWarnings(suppressMessages(
      nlmixr2est::nlmixr2(model_fn, fit_data, est = est,
                          control = list(print = 0)))),
    error = function(e) e)
  elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)

  failed <- inherits(fit, "error")
  list(
    meta      = meta,
    est       = est,
    failed    = failed,
    n_id      = length(unique(fit_data$ID[harness_is_observation(fit_data$EVID)])),
    n_obs     = sum(harness_is_observation(fit_data$EVID)),
    n_dose    = sum(harness_is_dose(fit_data$EVID)),
    message   = if (failed) NA_character_
                else paste(fit$message, collapse = " | "),
    error     = if (failed) conditionMessage(fit) else NA_character_,
    fit       = if (failed) NULL else fit,
    ofv       = if (failed) NA_real_ else as.numeric(fit$objDf$OBJF[1]),
    aic       = if (failed) NA_real_ else as.numeric(fit$objDf$AIC[1]),
    # Convergence is inferred from the optimizer message text; treat it as a
    # screen, not proof — the qualification battery is the real assessment.
    converged = if (failed) FALSE else
      !is.null(fit$message) && grepl("relative convergence|converged|Xtol|Normal exit",
                                     paste(fit$message, collapse = " "),
                                     ignore.case = TRUE),
    runtime_s = elapsed
  )
}

# Flattens run records into the base-model gate comparison table, one row
# per candidate. Failed runs appear with NA OFV/AIC (modeler rule: failures
# are presented, never filtered out).
candidate_table <- function(records) {
  do.call(rbind, lapply(records, function(r) data.frame(
    run        = r$meta$run,
    based_on   = r$meta$based_on %||% "",
    hypothesis = r$meta$hypothesis,
    est        = r$est,
    n_id       = r$n_id,
    n_obs      = r$n_obs,
    failed     = r$failed,
    converged  = r$converged,
    message    = r$message,
    ofv        = round(r$ofv, 2),
    aic        = round(r$aic, 2),
    runtime_s  = r$runtime_s,
    stringsAsFactors = FALSE)))
}

# Parameter summary with RSE and shrinkage for one record (gate package).
candidate_params <- function(record) {
  if (record$failed) return(NULL)
  f <- record$fit
  list(parFixed = f$parFixedDf, omega = f$omega,
       shrinkage = tryCatch(f$shrink, error = function(e) NULL))
}

# --- VPC simulation adapter (engine-specific plumbing, not method) ----------
# Produces the observed and simulated datasets that the tidyvpc reference
# implementation consumes. Which uncertainty is propagated is a method
# decision that belongs to the analysis plan; this adapter only reshapes what
# the engine returns into the columns tidyvpc expects (ID, TIME, DV, MDV,
# REP, and PRED when prediction correction is requested).
# The seed has no default on purpose: an unseeded simulation cannot be
# reproduced or audited, so a missing seed is an immediate stop.
vpc_simulate <- function(fit, n = 500, seed, pred = TRUE) {
  stopifnot(!missing(seed))
  s <- nlmixr2est::vpcSim(fit, n = n, seed = seed, pred = pred)
  names(s)[names(s) == "sim"]  <- "DV"
  names(s)[names(s) == "id"]   <- "ID"
  names(s)[names(s) == "time"] <- "TIME"
  names(s)[names(s) == "sim.id"] <- "REP"
  if ("pred" %in% names(s)) names(s)[names(s) == "pred"] <- "PRED"
  # vpcSim returns observation records only; carry the NONMEM event flag
  # (EVID == 0) so reference_vpc_prepare selects observations through the one
  # harness classifier (harness_is_observation) rather than on MDV. MDV is
  # retained for callers that still read it.
  s$MDV <- 0L
  s$EVID <- 0L
  # factor -> character -> integer round-trip recovers original numeric IDs
  s$ID  <- as.integer(as.character(s$ID))

  o <- as.data.frame(fit)
  obs <- data.frame(ID = as.integer(as.character(o$ID)),
                    TIME = o$TIME, DV = o$DV, MDV = 0L, EVID = 0L)
  if ("PRED" %in% names(o)) obs$PRED <- o$PRED

  list(obs = obs, sim = as.data.frame(s))
}
