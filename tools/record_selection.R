# Record-selection rule shared by readers, QC, EDA and fitting.
#
#   A record is KEPT when its exclusion flag is NA, empty (""), whitespace-only,
#   or the NONMEM missing token "." (after trimming surrounding whitespace).
#   Any other value is an exclusion REASON and the record is excluded.
#
# Why these four forms: read.csv(na.strings = ".") turns the token into NA; a
# blank cell in a character column arrives as ""; in-analysis assembly writes
# NA_character_; a reader that does not NA-code the token hands over a literal
# "."; and a hand-edited CSV can carry a stray space. All of them mean "no
# reason given", which is what "kept" means.
#
# EDA manifests bind this dependency by digest. Base R only.

# Missing tokens, compared AFTER trimws(). NA is handled separately (it is not
# a string). Defined once; every consumer takes this constant as its default.
HARNESS_MISSING_TOKENS <- c("", ".")

# The token(s) a CSV READER NA-codes on read (utils::read.csv(na.strings = )).
# The NONMEM convention writes "." for every missing value, numeric columns
# included, so the reader must NA-code it for numeric columns to parse. Blank
# cells are NOT listed: read.csv never NA-codes "" in character columns, which
# is exactly why the kept-record rule above accepts "" as well as NA.
HARNESS_READ_NA_STRINGS <- "."

# The rule as one sentence, quoted verbatim by the gate package and SKILL.md.
HARNESS_RECORD_SELECTION_RULE <- paste0(
  "kept when the exclusion flag is NA, empty, whitespace-only, or the NONMEM ",
  "missing token \".\" (compared after trimming); any other value is an ",
  "exclusion reason")

# TRUE for every record that is KEPT (not excluded). Vectorised over the
# exclusion-flag column; any atomic type is accepted and compared as character.
harness_kept_record <- function(x, tokens = HARNESS_MISSING_TOKENS) {
  if (!is.character(tokens))
    stop("harness_kept_record(): `tokens` must be a character vector of the ",
         "accepted missing tokens.", call. = FALSE)
  s <- trimws(as.character(x))
  is.na(x) | s %in% tokens
}

# The exclusion reason per record: NA_character_ for a kept record, the
# trimmed flag value otherwise.
harness_exclusion_reason <- function(x, tokens = HARNESS_MISSING_TOKENS) {
  kept <- harness_kept_record(x, tokens)
  out <- trimws(as.character(x))
  out[kept] <- NA_character_
  out
}

# Splits a data frame into the kept and the excluded records by the declared
# exclusion column, and tabulates the exclusion reasons. Returns
#   list(kept, excluded, reasons = data.frame(reason, n), kept_index (logical),
#        n_kept, n_excluded, rule, tokens)
# The frames keep their row order; nothing is dropped from `data` itself.
harness_select_records <- function(data, exclusion_col,
                                   tokens = HARNESS_MISSING_TOKENS) {
  if (!is.data.frame(data))
    stop("harness_select_records(): `data` must be a data frame.", call. = FALSE)
  if (!is.character(exclusion_col) || length(exclusion_col) != 1L ||
      !exclusion_col %in% names(data))
    stop("harness_select_records(): `exclusion_col` must name one column of ",
         "`data`; got ", paste(format(exclusion_col), collapse = ", "), ".",
         call. = FALSE)
  keep <- harness_kept_record(data[[exclusion_col]], tokens)
  reason <- harness_exclusion_reason(data[[exclusion_col]], tokens)
  reasons <- reason[!keep]
  tab <- if (length(reasons)) {
    lv <- sort(unique(reasons))
    data.frame(reason = lv, n = vapply(lv, function(r) sum(reasons == r), integer(1)),
               stringsAsFactors = FALSE)
  } else {
    data.frame(reason = character(0), n = integer(0), stringsAsFactors = FALSE)
  }
  rownames(tab) <- NULL
  list(kept       = data[keep, , drop = FALSE],
       excluded   = data[!keep, , drop = FALSE],
       reasons    = tab,
       kept_index = keep,
       n_kept     = sum(keep),
       n_excluded = sum(!keep),
       rule       = HARNESS_RECORD_SELECTION_RULE,
       tokens     = tokens)
}

# ---------------------------------------------------------------------------
# The NONMEM event convention (round 7, record-selection decision)
# ---------------------------------------------------------------------------
# The QC battery and the fitting adapter use EVID == 0 / EVID == 1 directly,
# while the EDA reads which event value means "observation" from the yspec
# decode annotation. Both are right only if the specification decodes
# observation as exactly 0 and dose as exactly 1. The harness ENFORCES that
# convention: the caller resolves the decoded
# values from the specification and asserts them here before any record is
# selected, so QC, EDA and fitting cannot disagree on what an observation is.
HARNESS_EVENT_OBSERVATION <- 0
HARNESS_EVENT_DOSE        <- 1

harness_assert_event_convention <- function(observation_values, dose_values,
                                            source = "the specification") {
  same <- function(v, want) {
    length(v) == 1L && !is.na(suppressWarnings(as.numeric(v))) &&
      isTRUE(all.equal(as.numeric(v), want))
  }
  if (!same(observation_values, HARNESS_EVENT_OBSERVATION))
    stop("Event convention: ", source, " decodes 'observation' as [",
         paste(format(observation_values), collapse = ", "), "], but the harness ",
         "requires exactly ", HARNESS_EVENT_OBSERVATION, " (NONMEM EVID). The QC ",
         "battery and the fitting adapter select observations by that value, so a ",
         "specification that decodes it differently would make QC, EDA and fitting ",
         "disagree. Reconcile the specification (or the dataset) rather than the ",
         "convention. ESCALATE to the SME.", call. = FALSE)
  if (!same(dose_values, HARNESS_EVENT_DOSE))
    stop("Event convention: ", source, " decodes 'dose' as [",
         paste(format(dose_values), collapse = ", "), "], but the harness ",
         "requires exactly ", HARNESS_EVENT_DOSE, " (NONMEM EVID). ESCALATE to ",
         "the SME.", call. = FALSE)
  invisible(TRUE)
}

harness_is_observation <- function(evid) {
  !is.na(evid) & as.numeric(evid) == HARNESS_EVENT_OBSERVATION
}
harness_is_dose <- function(evid) {
  !is.na(evid) & as.numeric(evid) == HARNESS_EVENT_DOSE
}
