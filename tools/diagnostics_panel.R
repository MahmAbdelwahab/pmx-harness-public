# Per-candidate GOF panel (skills/diagnostics.md, per-run panel).
# Takes a run record from run_candidate(); renders the standard pmplots GOF
# set from the fit's output table. Part of the base-model gate package —
# the SME decision needs plots, not numbers alone (SME instruction,
# 2026-08-28).

# Renders the per-candidate GOF PDF and returns its path. Returns NULL for
# a failed run (no fit to plot — the failure stays visible in the candidate
# table). The plot set is the fixed standard panel; changing it is a
# harness change reviewed by the SME, not an analysis-time choice.
gof_panel <- function(record, out_dir = "pilot/model/outputs") {
  if (record$failed) return(NULL)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  df <- as.data.frame(record$fit)
  run <- record$meta$run

  plots <- list(
    dv_pred    = pmplots::dv_pred(df),
    dv_ipred   = pmplots::dv_ipred(df),
    cwres_pred = pmplots::cwres_pred(df),
    cwres_time = pmplots::cwres_time(df),
    cwres_q    = pmplots::cwres_q(df)
  )

  path <- file.path(out_dir, paste0("gof-", run, ".pdf"))
  grDevices::pdf(path, width = 7, height = 5.5)
  for (p in plots) print(p)
  grDevices::dev.off()
  path
}
