# Deterministic EDA fixtures. Both specs describe the same fictional rows.
write_public_eda_inputs <- function(dir) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  id <- rep(seq_len(4L), each = 4L)
  time <- rep(0:3, 4L)
  dose <- time == 0
  excluded <- id == 4L & time == 3L
  blq <- id == 2L & time == 2L
  time[!dose] <- time[!dose] + id[!dose] / 10
  data <- data.frame(
    ID = id, TIME = time, EVID = as.integer(dose),
    AMT = ifelse(dose, 10, 0), DV = ifelse(dose, NA, 6 * id / (1 + time)),
    MDV = as.integer(dose), NUM = seq_along(id),
    C = ifelse(excluded, "excluded", NA_character_),
    BLQ = as.integer(blq),
    WT = 50 + 5 * id, DOSE = 5 * id)
  dataset <- file.path(dir, "dataset.csv")
  utils::write.csv(data, dataset, row.names = FALSE, na = ".")
  columns <- lapply(names(data), function(name) list(short = name, type = "numeric"))
  names(columns) <- names(data)
  columns$C$type <- "character"
  columns$EVID$values <- c(0L, 1L)
  columns$EVID$decode <- c("Observation", "Dose")
  columns$BLQ$values <- c(0L, 1L)
  columns$TIME$unit <- "h"
  columns$DV$unit <- "mg/L"
  columns$WT$unit <- "kg"
  setup <- list(description = "Synthetic assembly specification", primary_key = "NUM",
                use_internal_db = FALSE)
  base <- file.path(dir, "base-spec.yml")
  yaml::write_yaml(c(list(SETUP__ = setup), columns), base)
  setup$description <- "Synthetic EDA overlay"
  setup$flags <- list(eda_id = "ID", eda_time = "TIME", eda_dv = "DV",
                      eda_event = "EVID", eda_exclusion = "C", eda_dose = "DOSE",
                      eda_blq = "BLQ", eda_stratify = "WT",
                      eda_covariate_continuous = c("WT", "DOSE"))
  columns$BLQ$decode <- c("Quantified", "Below LLOQ")
  overlay <- file.path(dir, "eda-overlay.yml")
  writeLines(c(paste("# overlay-of:", base),
               paste0("# overlay-of-sha256: sha256:", digest::digest(file = base, algo = "sha256")),
               yaml::as.yaml(c(list(SETUP__ = setup), columns))), overlay)
  list(dataset = dataset, overlay = overlay, base = base)
}
