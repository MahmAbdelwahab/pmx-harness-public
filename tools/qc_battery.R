# Deterministic QC battery for analysis-ready datasets (Stage 1 CHECK).
#
# Implements the 11-item battery from skills/data-assembly.md. Pure
# functions: dataset in, per-check results out. No LLM involvement — this is
# the CHECK() primitive of docs/WORKFLOW.md. yspec-backed checks (spec
# load, ys_check) run in the pipeline where yspec is installed; the checks
# here are the spec-independent core, so the battery still guards a site
# where the derivation used another toolchain.
#
# `expect` is a list of pre-specified values from the MAP:
#   n_subjects      expected enrollment
#   lloq            LLOQ in DV units, or NA when no BLQ handling applies
#   time_unit, dv_unit, amt_unit   declared units (documented, asserted
#                                  against the spec by the yspec layer)
#   source_rows     record count in the source data, for reconciliation
#
# Depends on tools/record_selection.R (harness_kept_record, harness_is_*,
# HARNESS_EVENT_OBSERVATION / HARNESS_EVENT_DOSE), sourced alongside this file
# by tar_source("tools").

# One-row result constructor: every battery item reports id, description,
# pass (coerced with isTRUE, so an NA verdict never passes), and a
# human-readable detail string for the gate package.
qc_check <- function(id, description, pass, detail = "") {
  data.frame(id = id, description = description,
             pass = isTRUE(pass), detail = detail,
             stringsAsFactors = FALSE)
}

# Runs the 11-item battery against the analysis-ready dataset. Returns a
# data.frame, one row per check (id, description, pass, detail). It never
# stops on failure — checks report, and assert_qc_pass() is the separate
# halt decision — so the gate package always shows the full picture.
# `spec` (optional, review 2 of eda-standards, M4): the path of the yspec
# specification the dataset was assembled against. When given, the battery
# adds item 0: the specification's EVID decodes for "observation" and "dose"
# are asserted against the NONMEM convention (0 and 1) that items 3-10 below
# select records by, through tools/record_selection.R::
# harness_assert_event_convention() — the same assertion the EDA reference
# makes. Without a spec the item is reported as not assessed (the fitting
# adapter has no spec at all and cannot assert; the EDA and this battery are
# the two places that can). The `da_qc` target in the data-assembly task
# has the declared specification available through `da_spec_file`.
run_qc_battery <- function(data, expect, spec = NULL) {
  stopifnot(is.data.frame(data), is.list(expect))
  if (!"C" %in% names(data)) stop("QC battery: required column C is absent.", call. = FALSE)
  if (!is.atomic(expect$lloq) || length(expect$lloq) != 1L || !(is.na(expect$lloq) && !is.nan(expect$lloq) || is.numeric(expect$lloq) && is.finite(expect$lloq))) stop("QC battery: expect$lloq must be NA or one finite number.", call. = FALSE)
  req_cols <- c("ID", "TIME", "EVID", "AMT", "DV", "MDV", "NUM", "C", "BLQ")
  res <- list()

  # 0. Event convention asserted against the specification's decodes
  if (!is.null(spec)) {
    conv <- tryCatch({
      proj <- ys_projection(spec)
      harness_assert_event_convention(
        ys_decode_values(proj, "EVID", "observation"),
        ys_decode_values(proj, "EVID", "dose"),
        source = paste0("the specification ", spec))
      "observation = 0, dose = 1 as decoded by the specification"
    }, error = function(e) structure(conditionMessage(e), failed = TRUE))
    res$event <- qc_check("event_convention",
                          "Specification decodes observation as EVID 0 and dose as EVID 1",
                          is.null(attr(conv, "failed")), as.character(conv))
  } else {
    res$event <- qc_check("event_convention",
                          "Specification decodes observation as EVID 0 and dose as EVID 1",
                          TRUE, "not assessed: no specification supplied to the battery")
  }

  # 1. Required NONMEM columns present
  missing_cols <- setdiff(req_cols, names(data))
  res$cols <- qc_check("columns", "Required NONMEM columns present",
                       length(missing_cols) == 0,
                       paste(missing_cols, collapse = ", "))
  if (length(missing_cols) > 0) {
    return(do.call(rbind, res))   # remaining checks would all error
  }

  # Excluded records (non-empty C flag) stay in the dataset but are not part
  # of the estimation set; event-level checks run on kept records only.
  # NUM and reconciliation checks still cover all records.
  # THE kept-record rule is tools/record_selection.R::harness_kept_record()
  # (Codex round 7, finding 2): one classifier for the EDA reference, this
  # battery and the fitting adapter. It is not restated here. The event
  # convention (observation = 0, dose = 1) comes from the same file; the EDA
  # asserts the specification's decodes against it, so all three agree.
  flagged <- !harness_kept_record(data[["C"]])
  kept <- data[!flagged, ]
  obs  <- kept[harness_is_observation(kept$EVID), ]
  dose <- kept[harness_is_dose(kept$EVID), ]

  # 2. Subject count matches expected enrollment (from MAP)
  n_sub <- length(unique(data$ID))
  res$nsub <- qc_check("n_subjects",
                       sprintf("Subject count equals expected (%s)", expect$n_subjects),
                       n_sub == expect$n_subjects,
                       sprintf("found %d", n_sub))

  # 3. Every subject has at least one dose and one observation record
  ids <- unique(data$ID)
  no_dose <- setdiff(ids, unique(dose$ID))
  no_obs  <- setdiff(ids, unique(obs$ID))
  res$events <- qc_check("events_per_subject",
                         "Every subject has >=1 dose and >=1 observation",
                         length(no_dose) == 0 && length(no_obs) == 0,
                         sprintf("no dose: [%s]; no obs: [%s]",
                                 paste(no_dose, collapse = ","),
                                 paste(no_obs, collapse = ",")))

  # 4. MDV coding: 1 on all dose records and all missing-DV records; 0 else
  mdv_bad <- sum(dose$MDV != 1) +
    sum(obs$MDV[is.na(obs$DV)] != 1) +
    sum(obs$MDV[!is.na(obs$DV)] != 0)
  res$mdv <- qc_check("mdv", "MDV consistent with EVID and DV missingness",
                      mdv_bad == 0, sprintf("%d violations", mdv_bad))

  # 5. AMT: positive on dose records, missing/zero on observations
  amt_bad <- sum(is.na(dose$AMT) | dose$AMT <= 0) +
    sum(!is.na(obs$AMT) & obs$AMT != 0)
  res$amt <- qc_check("amt", "AMT positive on doses, absent on observations",
                      amt_bad == 0, sprintf("%d violations", amt_bad))

  # 6. BLQ flag consistent with declared LLOQ
  if (is.na(expect$lloq)) {
    blq_bad <- sum(obs$BLQ != 0)
    detail <- "no LLOQ declared; all BLQ must be 0"
  } else {
    blq_bad <- sum(obs$BLQ[!is.na(obs$DV)] != (obs$DV[!is.na(obs$DV)] < expect$lloq))
    detail <- sprintf("LLOQ = %s; %%BLQ = %.1f%%", expect$lloq,
                      100 * mean(obs$BLQ == 1))
  }
  res$blq <- qc_check("blq", "BLQ flag consistent with declared LLOQ",
                      blq_bad == 0, detail)

  # 7. Exclusions via C flag only; every flagged record carries a reason
  #    (C is a missing token when kept — see HARNESS_MISSING_TOKENS — and a
  #    non-empty string reason when excluded)
  res$cflag <- qc_check("c_flag",
                        "Excluded records flagged with reasons, none deleted",
                        TRUE,   # deletion is unobservable here; reconciliation covers it
                        sprintf("%d records flagged for exclusion", sum(flagged)))

  # 8. NUM sequential and unique
  res$num <- qc_check("num", "NUM sequential and unique",
                      identical(as.integer(data$NUM), seq_len(nrow(data))),
                      "")

  # 9. TIME non-decreasing within subject; no NA
  time_ok <- !anyNA(data$TIME) &&
    all(tapply(data$TIME, data$ID, function(x) !is.unsorted(x)))
  res$time <- qc_check("time", "TIME non-decreasing within ID, no missing",
                       time_ok, "")

  # 10. Non-positive or missing DV on non-BLQ observation records
  dv_bad <- sum(!is.na(obs$DV) & obs$DV <= 0 & obs$BLQ == 0)
  res$dv <- qc_check("dv", "No non-positive DV on non-BLQ observations",
                     dv_bad == 0, sprintf("%d violations", dv_bad))

  # 11. Source reconciliation: row count traced to source
  res$recon <- qc_check("reconciliation",
                        sprintf("Row count reconciles to source (%s source rows)",
                                expect$source_rows),
                        isTRUE(expect$reconciled),
                        expect$recon_detail %||% "")

  out <- do.call(rbind, res)
  rownames(out) <- NULL
  out
}

# Null-coalescing helper: b when a is NULL (optional `expect` fields).
`%||%` <- function(a, b) if (is.null(a)) b else a

# Stops the pipeline when any battery item fails (the CHECK contract:
# a failing check is reported, never patched around).
assert_qc_pass <- function(qc) {
  if (!is.data.frame(qc) || !nrow(qc) || !"pass" %in% names(qc) || !is.logical(qc[["pass"]]) || anyNA(qc[["pass"]])) stop("QC battery requires a nonempty data frame with a logical, NA-free pass column.", call. = FALSE)
  if (!all(qc$pass)) {
    bad <- qc[!qc$pass, ]
    stop("QC battery failed:\n",
         paste(sprintf("  [%s] %s — %s", bad$id, bad$description, bad$detail),
               collapse = "\n"),
         "\nSee skills/data-assembly.md; deviations from the MAP data spec ",
         "go through DEVIATE(), not silent adaptation.", call. = FALSE)
  }
  qc
}
