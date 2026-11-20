# Run from the project root with Rscript analyses/synthetic_example.R --generate.
# Then use targets::tar_make() with this file as its script.
if (sys.nframe() == 0L && identical(commandArgs(trailingOnly = TRUE), "--generate")) {
  path <- "examples/synthetic"
  files <- file.path(path, c("dataset.csv", "spec.yml", "plan.md"))
  if (any(file.exists(files)))
    stop("Synthetic example inputs already exist. Use an empty project copy to regenerate them.")
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  id <- rep(seq_len(4L), each = 3L)
  time <- rep(0:2, 4L)
  dose <- time == 0L
  time[!dose] <- time[!dose] + id[!dose] / 10
  data <- data.frame(ID = id, TIME = time, EVID = as.integer(dose),
                     AMT = ifelse(dose, 10, 0), DV = ifelse(dose, NA, (id + 2) / time),
                     MDV = as.integer(dose), NUM = seq_along(id), C = NA_character_,
                     BLQ = 0L, WT = 50 + 5 * id)
  utils::write.csv(data, files[1], row.names = FALSE, na = ".")
  columns <- list(
    ID = list(short = "Subject identifier"),
    TIME = list(short = "Time", unit = "h"),
    EVID = list(short = "Event", values = c(0L, 1L), decode = c("Observation", "Dose")),
    AMT = list(short = "Amount", unit = "mg"),
    DV = list(short = "Observation", unit = "mg/L"),
    MDV = list(short = "Missing observation flag", values = c(0L, 1L)),
    NUM = list(short = "Record index"),
    C = list(short = "Record exclusion flag", type = "character"),
    BLQ = list(short = "Below quantification flag", values = c(0L, 1L),
               decode = c("Quantifiable", "Below LLOQ")),
    WT = list(short = "Body weight", unit = "kg"))
  setup <- list(description = "Deterministic synthetic example", primary_key = "NUM",
                flags = list(eda_id = "ID", eda_time = "TIME", eda_dv = "DV",
                             eda_event = "EVID", eda_exclusion = "C", eda_dose = "AMT",
                             eda_blq = "BLQ", eda_covariate_continuous = "WT"))
  yaml::write_yaml(c(list(SETUP__ = setup), columns), files[2])
  writeLines(c("# Synthetic example plan", "",
               "Check the generated dataset against its specification and declared counts.",
               "The records are fictional and demonstrate the task and review workflow."),
             files[3])
  quit(save = "no")
}

library(targets)
tar_source("tools")
source("pipeline/compose.R")
tar_option_set(format = "rds")

compose_pipeline(
  analysis_id = "synthetic_example",
  tasks = c("data_assembly", "eda"),
  inputs = list(
    data_assembly = list(
      source = "examples/synthetic/dataset.csv",
      spec = "examples/synthetic/spec.yml",
      map = "examples/synthetic/plan.md",
      expect = list(n_subjects = 4L, source_rows = 12L, lloq = NA_real_)),
    eda = list(registry = eda_application_registry(), out_dir = "examples/synthetic/eda")))
