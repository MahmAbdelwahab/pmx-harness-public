#!/usr/bin/env Rscript
# Tests for the Codex round-7 EDA remediation (an SME decision:
# adopt all findings; no arbitrary R functions in the plot registry).
#
# Covered, in round-7 numbering:
#   F4  the reference's self-gate is NOT substitutable: a global
#       `require_validated_method <- function(...) TRUE` does not unlock a
#       draft unit. The numerics are exercised through a MANIFEST FIXTURE in a
#       tempdir (units approved with the current digest, reviewed_by
#       "test-fixture"), never through a stub and never by touching the real
#       manifest.
#   F2  one canonical exclusion classifier (tools/record_selection.R) shared by
#       the reference, the QC battery and the nlmixr2 fitting adapter; the
#       accepted missing tokens are defined once; the NONMEM event convention
#       (observation = 0, dose = 1) is validated against the spec's decodes.
#   F1/F6 the plot registry is declarative (no `fn`); every displayed
#       statistic (LOESS, histogram bins, correlations, box statistics, dose
#       markers) is computed by the reference and drawn with identity geoms;
#       every layer frame carries the row id and digests to the view it
#       declares; the registry index requires every active entry exactly once,
#       refuses duplicate and disabled renders, and verifies the rendered file
#       name, dimensions and page count against the declaration.
#   F7  the core deliverables the SKILL names exist in code: categorical
#       summary and boxplots (with an ENFORCED inapplicable state carrying the
#       reason when the specification flags no categorical covariate), the
#       inventory by study and dose group (same mechanism), .tex tables through
#       pmtables, linear concentration-time with dose-group colour and a
#       reference-computed LOESS, individual profiles with dose markers and
#       pagination.
#   F3  every method parameter is declared and emitted with the results.
#   F9  the log-axis audit classifies missing / zero / negative / non-finite
#       values by row id with BLQ status, per active plot, axis and row
#       selector, and is reconciled against ggplot_build() of the built plot.
#   F8  the EDA specification is a semantic OVERLAY bound to the base spec's
#       sha256; composition stops when the base spec drifted.
#   Numeric equivalence: the composed synthetic inventory, summary and log
#   audit match the authenticated direct calculations from the same input.
#
# Fixtures and stores live under tempdir(). Run with
#   Rscript tools/tests/test_eda_declarative.R
.invoked_as_script <- function() {
  fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  length(fa) > 0 && grepl("test_eda_declarative\\.R$", sub("^--file=", "", fa[1]))
}

.run_eda_declarative_tests <- function() {

args <- commandArgs(FALSE)
file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
TOOLS <- normalizePath(file.path(dirname(file_arg), ".."), mustWork = TRUE)
REPO  <- normalizePath(file.path(TOOLS, ".."), mustWork = TRUE)
setwd(REPO)
suppressWarnings(suppressMessages({
  source(file.path(TOOLS, "external.R"))
  source(file.path(TOOLS, "record_selection.R"))
  source(file.path(TOOLS, "qc_battery.R"))
  source(file.path(TOOLS, "gate_package.R"))
  source(file.path(TOOLS, "evidence_notes.R"))
  source(file.path(REPO, "pipeline", "compose.R"))
}))
if (!exists("%||%", envir = globalenv(), mode = "function")) {
  assign("%||%", function(a, b) if (is.null(a)) b else a, envir = globalenv())
}

.pass <- 0L; .fail <- 0L
ok <- function(cond, name) {
  cond <- tryCatch(isTRUE(cond), error = function(e) {
    cat(sprintf("      error: %s\n", conditionMessage(e))); FALSE })
  if (cond) { .pass <<- .pass + 1L; cat(sprintf("PASS  %s\n", name)) }
  else { .fail <<- .fail + 1L; cat(sprintf("FAIL  %s\n", name)) }
}
expect_stop <- function(expr, name, regexp = NULL) {
  e <- tryCatch({ force(expr); NULL }, error = function(e) e)
  hit <- !is.null(e) && (is.null(regexp) || grepl(regexp, conditionMessage(e)))
  if (!is.null(e) && !hit) cat(sprintf("      got: %s\n", conditionMessage(e)))
  if (is.null(e)) cat("      no error was raised\n")
  ok(hit, name)
}
quiet <- function(expr) suppressMessages(suppressWarnings(expr))
has_yspec <- requireNamespace("yspec", quietly = TRUE) &&
  !nzchar(Sys.getenv("PMX_TEST_NO_YSPEC"))
validated_only <- function(case, reason) {
  if (nzchar(Sys.getenv("PMX_CI_NO_SKIP"))) {
    .fail <<- .fail + 1L
    cat("FAIL ", case, ": ", reason, "\n", sep = "")
  } else {
    cat("SKIP: ", reason, "\n", sep = "")
  }
}
source(file.path(TOOLS, "tests", "fixtures", "public_workflow", "eda_inputs.R"),
       local = TRUE)

REF <- file.path("skills", "eda-standards", "reference", "eda.R")
REAL_MF <- file.path("skills", "eda-standards", "MANIFEST.yaml")
# The property wanted of the REAL manifest is that this test never writes it,
# whatever its status: its bytes are hashed here and compared at the end.
real_mf_sha_before <- digest::digest(file = REAL_MF, algo = "sha256")
source(file.path(TOOLS, "method_manifest.R"))
real_mf <- read_method_manifest(REAL_MF)

# A DRAFT fixture: a mini repository under tempdir() whose manifest carries
# the current digests with every unit draft and reviewed_by empty (what
# write_method_manifest() emits before an SME signs). The refusal tests run
# against it, so they hold whether or not the SME has approved the real one.
make_fixture <- function(root, status) {
  for (p in c(REF, "tools/method_manifest.R", "tools/record_selection.R")) {
    dir.create(file.path(root, dirname(p)), recursive = TRUE, showWarnings = FALSE)
    file.copy(p, file.path(root, p), overwrite = TRUE)
  }
  units <- lapply(real_mf$units, function(u)
    list(reference_path = u$reference_path, entrypoints = u$entrypoints,
         status = status, evidence = "TEST FIXTURE ONLY",
         reason = sprintf("test fixture: %s copy", status),
         depends = vapply(u$depends, function(d) d$path, character(1))))
  od <- setwd(root); on.exit(setwd(od))
  quiet(write_method_manifest(file.path(root, "skills", "eda-standards"), units))
  if (identical(status, "approved")) {
    mfp <- file.path(root, "skills", "eda-standards", "MANIFEST.yaml")
    l <- readLines(mfp)
    l <- sub('^reviewed_by: ""', 'reviewed_by: "test-fixture"', l)
    l <- sub('^reviewed_at: ""', 'reviewed_at: "2026-09-03"', l)
    writeLines(l, mfp)
  }
  invisible(root)
}
DRAFT <- make_fixture(tempfile("eda-draft"), "draft")
draft_mf <- read_method_manifest(file.path(DRAFT, "skills", "eda-standards", "MANIFEST.yaml"))
ok(identical(draft_mf$reviewed_by, "") &&
   all(vapply(draft_mf$units, function(u) identical(u$status, "draft"), logical(1))),
   "fixture: the DRAFT fixture manifest has empty reviewed_by and all units draft")

# ==== F4: the self-gate cannot be substituted ==============================
# A global stub is exactly what the old test suite used to unlock the
# numerics; it must be inert now. The stub is assigned in the GLOBAL
# environment and the reference is sourced afterwards (against the DRAFT
# fixture), so if the reference looked the validator up dynamically the stub
# would win.
assign("require_validated_method", function(...) TRUE, envir = globalenv())
src_env <- new.env(parent = globalenv())
od <- setwd(DRAFT)
sys.source(REF, envir = src_env, keep.source = TRUE)
setwd(od)
toy <- data.frame(C = NA_character_, ID = c(1, 1, 2, 2), TIME = c(0, 1, 0, 1),
                  EVID = c(1, 0, 1, 0), DV = c(NA, 1.5, NA, 2.5),
                  BLQ = c(0, 0, 0, 0), WT = c(70, 70, 80, 80),
                  stringsAsFactors = FALSE)
in_draft <- function(f, ...) { od <- setwd(DRAFT); on.exit(setwd(od)); f(...) }
expect_stop(quiet(in_draft(get("reference_eda_view", src_env), toy, "ID", "C", "EVID", 0, 1)),
            "F4: a global require_validated_method stub does NOT unlock a draft unit",
            "not SME-validated")
rm("require_validated_method", envir = globalenv())
mm_env <- get(".eda_private", envir = src_env)
ok(is.environment(mm_env) && environmentIsLocked(mm_env) &&
   bindingIsLocked("require_validated_method", mm_env),
   "F4: the validator lives in a private, locked environment inside the reference")
expect_stop(assign("require_validated_method", function(...) TRUE, envir = mm_env),
            "F4: the private binding cannot be reassigned",
            "locked")

# H1 (review 2): the row-set digest must be injective over row-id sets. With
# no separator {1, 2, 3} and {1, 23} collide.
dg <- get(".eda_digest", envir = src_env)
ok(!identical(dg(c("S", 1L, 2L, 3L)), dg(c("S", 1L, 23L))) &&
   !identical(dg(c("S", 11L, 12L)), dg(c("S", 1L, 112L))),
   "H1: row sets {1,2,3} / {1,23} and {11,12} / {1,112} digest differently")

# ==== the APPROVED manifest FIXTURE (test-only, never the real manifest) ====
# Same mini-repository shape with every unit `approved` at the CURRENT digests
# and reviewed_by "test-fixture", so the numerics run regardless of the real
# manifest's status. The reference resolves its paths against getwd(), so the
# fixture root is the working directory while its functions run.
FIX <- make_fixture(tempfile("eda-fixture"), "approved")
ok(all(vapply(real_mf$units, function(u) length(u$depends) >= 1L &&
              any(vapply(u$depends, function(d) identical(d$path, "tools/record_selection.R"),
                         logical(1))), logical(1))),
   "F2: every real unit declares tools/record_selection.R under depends")
ok(all(vapply(real_mf$units, function(u) identical(u$sha256, .mm_file_sha256(REF)),
              logical(1))),
   "fixture: every real unit digest equals the reference on disk")

# Run f with the fixture root as the working directory.
fx_env <- new.env(parent = globalenv())
od <- setwd(FIX)
sys.source(REF, envir = fx_env, keep.source = TRUE)
setwd(od)
R <- function(nm) {
  f <- get(nm, envir = fx_env, mode = "function")
  function(...) { od <- setwd(FIX); on.exit(setwd(od)); f(...) }
}
in_fix <- function(expr) { od <- setwd(FIX); on.exit(setwd(od)); force(expr) }

# ==== H2 (review 2): rebinding a helper in the sourcing environment STOPS ===
# Under BOTH source modes (the pilot runs with keep.source = TRUE), rebinding
# `.eda_kept`, `.eda_private` or `.eda_bins` after sourcing must be caught by
# the structural definitions digest / the gate's private-environment check.
h2_data <- data.frame(C = c(NA, "outlier", NA, NA), ID = c(1, 1, 2, 2),
                      TIME = c(0, 1, 0, 1), EVID = c(1, 0, 1, 0), DV = c(NA, 1, NA, 2),
                      WT = c(70, 70, 80, 80), stringsAsFactors = FALSE)
for (ks in c(TRUE, FALSE)) {
  mode <- if (ks) "keep.source = TRUE" else "keep.source = FALSE"
  fresh <- function() {
    e <- new.env(parent = globalenv())
    od <- setwd(FIX); on.exit(setwd(od))
    sys.source(REF, envir = e, keep.source = ks)
    e
  }
  call_in <- function(e, nm, ...) { od <- setwd(FIX); on.exit(setwd(od))
    get(nm, envir = e, mode = "function")(...) }
  e0 <- fresh()
  ok(is.list(tryCatch(call_in(e0, "reference_eda_view", h2_data, "ID", "C", "EVID", 0, 1),
                      error = function(err) { cat("      error:", conditionMessage(err), "\n"); NULL })),
     paste0("H2: untampered reference computes under ", mode))
  e1 <- fresh(); assign(".eda_kept", function(x, priv) rep(TRUE, length(x)), envir = e1)
  expect_stop(call_in(e1, "reference_eda_view", h2_data, "ID", "C", "EVID", 0, 1),
              paste0("H2: a rebound .eda_kept stops under ", mode),
              "do not match the definitions")
  e2 <- fresh()
  fake <- new.env(parent = baseenv())
  for (nm in ls(get(".eda_private", envir = e2), all.names = TRUE))
    assign(nm, get(nm, envir = get(".eda_private", envir = e2)), envir = fake)
  assign("harness_assert_event_convention", function(...) TRUE, envir = fake)
  assign(".eda_private", fake, envir = e2)
  expect_stop(call_in(e2, "reference_eda_view", h2_data, "ID", "C", "EVID", 7, 8),
              paste0("H2: a rebound .eda_private stops under ", mode),
              "rebound it after sourcing|do not match the definitions")
  e3 <- fresh()
  assign(".eda_bins", function(x, n, what) list(breaks = 0, index = rep(1L, length(x)),
                                                lower = 0, upper = 1, count = 1L,
                                                table = data.frame(bin = 1L, lower = 0, upper = 1, count = 1L)),
         envir = e3)
  v3 <- tryCatch(call_in(e0, "reference_eda_view", h2_data, "ID", "C", "EVID", 0, 1), error = function(err) NULL)
  hist_entry <- list(tier = "core", file = "h.pdf", w = 5, h = 4, kind = "histograms",
                     rows = "subjects", x_transform = "identity", y_transform = "identity",
                     variables = "covariate_continuous", bins = list(rule = "equal_width", n = 2L))
  expect_stop(call_in(e3, "reference_eda_build_figure", v3, hist_entry, "h",
                      list(id = "ID", covariate_continuous = "WT"), c(WT = "weight")),
              paste0("H2: a rebound .eda_bins stops under ", mode),
              "do not match the definitions")
}

# ==== F2: one canonical exclusion classifier ================================
cvals <- c(NA, "", " ", ".", "\t", "outlier", " . ", "x ")
kept <- harness_kept_record(cvals)
ok(identical(kept, c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE)),
   "F2: NA, empty, whitespace-only and \".\" (trimmed) are kept; a reason is not")
ok(identical(harness_exclusion_reason(cvals)[6], "outlier") &&
   is.na(harness_exclusion_reason(cvals)[1]),
   "F2: the exclusion reason is the trimmed flag value, NA when kept")
sel <- harness_select_records(data.frame(C = cvals, ID = seq_along(cvals),
                                         stringsAsFactors = FALSE), "C")
ok(nrow(sel$kept) == 6L && nrow(sel$excluded) == 2L &&
   identical(sel$reasons$reason, c("outlier", "x")) &&
   identical(sel$reasons$n, c(1L, 1L)),
   "F2: harness_select_records returns kept and excluded frames and a reason table")
# The QC battery selects with the same function (not a re-stated predicate).
qc_src <- readLines(file.path(TOOLS, "qc_battery.R"), warn = FALSE)
ok(any(grepl("harness_kept_record", qc_src)) &&
   !any(grepl('!= "\\."', qc_src)),
   "F2: tools/qc_battery.R delegates to harness_kept_record (no private predicate)")
nl_src <- readLines(file.path(TOOLS, "nlmixr_runs.R"), warn = FALSE)
ok(any(grepl("harness_kept_record", nl_src)) &&
   !any(grepl("is.na\\(data\\$C\\)", nl_src)),
   "F2: tools/nlmixr_runs.R selects the fitting set with harness_kept_record")
ext_src <- readLines(file.path(TOOLS, "external.R"), warn = FALSE)
ok(any(grepl("HARNESS_READ_NA_STRINGS", ext_src)) &&
   !any(grepl('na.strings = "\\."', ext_src)),
   "F2: tools/external.R reads with the shared missing-token constant")
ref_src <- readLines(REF, warn = FALSE)
ok(any(grepl("harness_kept_record", ref_src)),
   "F2: the reference's .eda_kept delegates to harness_kept_record")
# The EVID convention.
ok(isTRUE(harness_assert_event_convention(0, 1)),
   "F2: observation = 0 and dose = 1 satisfy the NONMEM event convention")
expect_stop(harness_assert_event_convention(c(0, 2), 1),
            "F2: an observation decode other than exactly 0 stops",
            "observation")
expect_stop(harness_assert_event_convention(0, 4),
            "F2: a dose decode other than exactly 1 stops",
            "dose")
ok(any(grepl("HARNESS_EVENT_OBSERVATION", qc_src)) && any(grepl("HARNESS_EVENT_DOSE", qc_src)),
   "F2: the QC battery uses the shared event constants, not literals")

# ==== the fixture dataset and its roles =====================================
fx_data <- data.frame(
  C    = c(NA, NA, "outlier", NA, NA, NA, NA, NA, NA, NA, NA, NA),
  ID   = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3),
  TIME = c(0, 1, 2, 4, 0, 1, 2, 4, 0, 1, 2, 4),
  EVID = c(1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0),
  DV   = c(NA, 0, 3.0, 1.0, NA, -1, 4.0, 2.0, NA, 2.5, 5.0, 2.0),
  BLQ  = c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  WT   = c(70, 70, 70, 70, 80, 80, 80, 80, 60, 60, 60, 60),
  AGE  = c(30, 30, 30, 30, 45, 45, 45, 45, 52, 52, 52, 52),
  SEX  = c(1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1),
  STUDY = c(101, 101, 101, 101, 101, 101, 101, 101, 102, 102, 102, 102),
  stringsAsFactors = FALSE)
cols <- list(id = "ID", exclusion = "C", event = "EVID", time = "TIME", dv = "DV",
             blq = "BLQ", covariate_continuous = c("WT", "AGE"),
             covariate_categorical = "SEX", study = "STUDY",
             dose_group = character(0))
flags <- c(id = "eda_id", exclusion = "eda_exclusion", event = "eda_event",
           time = "eda_time", dv = "eda_dv", blq = "eda_blq",
           covariate_continuous = "eda_covariate_continuous",
           covariate_categorical = "eda_covariate_categorical",
           study = "eda_study", dose_group = "eda_dose_group")
labels <- c(ID = "subject", TIME = "time (h)", DV = "concentration (mg/L)",
            WT = "weight (kg)", AGE = "age (y)", SEX = "sex", STUDY = "study")
decodes <- list(SEX = data.frame(value = c("1", "2"), decode = c("male", "female"),
                                 stringsAsFactors = FALSE))

v <- R("reference_eda_view")(fx_data, id_col = "ID", exclusion_col = "C",
                             event_col = "EVID", observation_values = 0,
                             dose_values = 1)
ok(all(c("analysis", "observations", "subjects", "doses", "excluded") %in% names(v$rows)) &&
   v$n_rows[["doses"]] == 3 && v$n_rows[["excluded"]] == 1 &&
   v$n_rows[["observations"]] == 8,
   "record_selection: the view carries doses and excluded selectors with counts")
ok(is.data.frame(v$exclusions) && identical(v$exclusions$reason, "outlier") &&
   v$exclusions$n == 1L,
   "record_selection: exclusion reasons are tabulated by the reference")
ok(is.data.frame(v$parameters) && all(c("parameter", "value") %in% names(v$parameters)) &&
   any(grepl("first kept record", v$parameters$value)) &&
   any(grepl("record_selection", v$parameters$parameter)),
   "F3: the view emits its method parameters (snapshot rule, dependency digest)")

# ==== F3 + F7: tables =======================================================
inv <- R("reference_eda_inventory")(v, blq_col = "BLQ", blq_values = 1,
                                    group_cols = "STUDY")
ok(nrow(inv$table) == 2L && identical(inv$table$n_subjects, c(2L, 1L)) &&
   identical(inv$table$n_excluded, c(1L, 0L)) &&
   identical(inv$table$n_records_read, c(8L, 4L)),
   "F7: the inventory groups by the declared column with group-specific exclusion counts")
inv0 <- R("reference_eda_inventory")(v, blq_col = "BLQ", blq_values = 1)
ok(inv0$table$n_excluded == 1L && inv0$table$n_records_read == 12L &&
   is.data.frame(inv0$parameters),
   "F7: the ungrouped inventory reports n_records_read and its parameters")
v_blqna <- v; v_blqna$rows$observations$BLQ[1] <- NA
expect_stop(R("reference_eda_inventory")(v_blqna, blq_col = "BLQ", blq_values = 1),
            "F3: a missing BLQ flag on an observation stops (declared policy)",
            "BLQ")

cs <- R("reference_eda_continuous_summary")(v, columns = c("WT", "AGE"),
                                            rows = "subjects", digits = 2)
ok(is.data.frame(cs$table) && cs$table$mean[cs$table$variable == "WT"] == 70 &&
   cs$table$n[1] == 3L,
   "summary_statistics: the continuous summary returns table + parameters")
pnames <- cs$parameters$parameter
ok(all(c("sd", "median", "missing values", "non-finite values", "rounding",
         "subject snapshot") %in% pnames),
   "F3: SD denominator, median convention, finite-value policy, rounding, snapshot are declared")
v_inf <- v; v_inf$rows$subjects$WT[1] <- Inf
expect_stop(R("reference_eda_continuous_summary")(v_inf, "WT"),
            "F3: a non-finite covariate value stops", "non-finite")
v_tv <- v; v_tv$rows$analysis$WT[2] <- 71
expect_stop(R("reference_eda_continuous_summary")(v_tv, "WT"),
            "F3: a covariate that varies within subject stops the subject snapshot",
            "varies within")
v_l3 <- v; v_l3$rows$analysis$WT[v_l3$rows$analysis$ID == 1][1] <- NA
v_l3$rows$subjects$WT[v_l3$rows$subjects$ID == 1] <- NA
expect_stop(R("reference_eda_continuous_summary")(v_l3, "WT"),
            "L3: a covariate missing on the first kept record but present later stops (declared)",
            "missing on the first kept record")

cat_s <- R("reference_eda_categorical_summary")(v, columns = "SEX", rows = "subjects",
                                                decodes = decodes, digits = 1)
ok(nrow(cat_s$table) == 2L && identical(cat_s$table$n, c(2L, 1L)) &&
   identical(cat_s$table$decode, c("male", "female")) &&
   isTRUE(all.equal(cat_s$table$percent, c(66.7, 33.3))),
   "F7: the categorical summary counts n and percent per decoded level")
expect_stop(R("reference_eda_categorical_summary")(v, columns = character(0)),
            "F7: an empty categorical column set is refused (inapplicable is the caller's state)",
            "inapplicable")

# ==== F9: the log-axis audit classifies by row id ===========================
la <- R("reference_eda_log_axis_audit")(v, value_col = "DV", rows = "observations",
                                        blq_col = "BLQ", blq_values = 1)
val <- function(id) la$audit$value[la$audit$id == id]
ok(val("n_zero") == 1 && val("n_negative") == 1 && val("n_missing") == 0 &&
   val("n_nonfinite") == 0 && val("n_dropped_by_log") == 2 &&
   val("n_nonpositive_and_blq") == 1,
   "F9: zero, negative, missing and non-finite are counted separately")
ok(is.data.frame(la$detail) && all(c("row_id", "value_class", "blq") %in% names(la$detail)) &&
   nrow(la$detail) == 2L && setequal(la$detail$value_class, c("zero", "negative")) &&
   identical(la$detail$blq[la$detail$value_class == "zero"], TRUE),
   "F9: the dropped rows are listed by row id with their class and BLQ status")
v_inf2 <- v; v_inf2$rows$observations$DV[4] <- Inf
la2 <- R("reference_eda_log_axis_audit")(v_inf2, "DV", blq_col = "BLQ", blq_values = 1)
ok(la2$audit$value[la2$audit$id == "n_nonfinite"] == 1 &&
   la2$audit$value[la2$audit$id == "n_dropped_by_log"] == 3,
   "F9: +Inf is a non-finite value a log axis drops")

# ==== F1/F6: the declarative registry =======================================
reg <- list(
  ct_lin = list(tier = "core", file = "ct-lin.pdf", w = 7, h = 5, kind = "profiles",
                rows = "observations", x_transform = "identity", y_transform = "identity",
                x = "time", y = "dv", group = "id",
                colour = list(role = "dose_group", when_absent = "group"),
                smooth = list(method = "loess", span = 0.9, degree = 1)),
  ct_log = list(tier = "core", file = "ct-log.pdf", w = 7, h = 5, kind = "profiles",
                rows = "observations", x_transform = "identity", y_transform = "log10",
                x = "time", y = "dv", group = "id",
                colour = list(role = "dose_group", when_absent = "group")),
  indiv = list(tier = "core", file = "indiv.pdf", w = 8, h = 7, kind = "individual_profiles",
               rows = "observations", x_transform = "identity", y_transform = "identity",
               x = "time", y = "dv", facet = "id",
               markers = list(rows = "doses", x = "time"),
               page = list(per_page = 2L)),
  hist = list(tier = "core", file = "hist.pdf", w = 7, h = 4, kind = "histograms",
              rows = "subjects", x_transform = "identity", y_transform = "identity",
              variables = "covariate_continuous",
              bins = list(rule = "equal_width", n = 2L)),
  pairs = list(tier = "core", file = "pairs.pdf", w = 6, h = 6, kind = "pairs",
               rows = "subjects", x_transform = "identity", y_transform = "identity",
               variables = "covariate_continuous",
               bins = list(rule = "equal_width", n = 2L),
               correlation = list(method = "pearson", use = "complete.obs", digits = 2L)),
  box = list(tier = "core", file = "box.pdf", w = 7, h = 5, kind = "boxplots",
             rows = "subjects", x_transform = "identity", y_transform = "identity",
             variables = "covariate_continuous", by = "covariate_categorical",
             requires = "covariate_categorical",
             box = list(quantile_type = 7L, whisker = 1.5)),
  strat = list(tier = "catalog", file = "strat.pdf", w = 7, h = 5, kind = "profiles",
               rows = "observations", x_transform = "identity", y_transform = "identity",
               x = "time", y = "dv", group = "id", colour = list(role = "derived"),
               derive = list(kind = "quantile_bins", role = "stratify",
                             probs = c(0, 0.5, 1), labels = c("low", "high"),
                             into = "EDA_STRATUM", breaks_rows = "subjects"))
)
rt <- R("reference_eda_registry_check")(reg)
ok(is.data.frame(rt) && nrow(rt) == 7L && all(c("kind", "file", "w", "h") %in% names(rt)),
   "F1: a declarative registry validates and tabulates")
with_fn <- reg; with_fn$ct_lin$fn <- function(d, cols, lab, der) NULL
expect_stop(R("reference_eda_registry_check")(with_fn),
            "F1: an entry carrying an R function (`fn`) is refused",
            "fn")
bad_kind <- reg; bad_kind$ct_lin$kind <- "whatever"
expect_stop(R("reference_eda_registry_check")(bad_kind),
            "F1: an unknown figure kind is refused", "kind")
bad_smooth <- reg; bad_smooth$ct_lin$smooth <- list(method = "gam")
expect_stop(R("reference_eda_registry_check")(bad_smooth),
            "F1: a smoother other than the declared loess with span and degree is refused",
            "smooth")
log_hist <- reg; log_hist$hist$y_transform <- "log10"
expect_stop(R("reference_eda_registry_check")(log_hist),
            "F1: a log axis on a non-profile kind is refused (not auditable)",
            "transform")

# Build every active figure on the fixture. `dose_group` is unresolved, so the
# colour falls back to the group role and says so; `covariate_categorical` IS
# resolved here, so the boxplots build.
out_dir <- file.path(FIX, "out"); dir.create(out_dir)
active <- R("reference_eda_registry_active")(rt, "strat")
cols_s <- cols; cols_s$stratify <- "WT"
builds <- lapply(active, function(nm)
  quiet(R("reference_eda_build_figure")(v, reg[[nm]], nm, cols_s, labels,
                                        flags = flags, decodes = decodes)))
names(builds) <- active
ok(all(vapply(builds, function(b) is.null(b$inapplicable), logical(1))),
   "F6: every active entry builds on the fixture (no inapplicable state)")
ok(all(vapply(builds, function(b) length(b$pages) >= 1L &&
              all(vapply(b$pages, function(p) inherits(p, "ggplot"), logical(1))),
              logical(1))),
   "F6: every build is one or more plain ggplot pages")
lin <- builds$ct_lin
ok(is.data.frame(lin$statistics$loess) && nrow(lin$statistics$loess) == 8L &&
   all(c("row_id", "x", "fitted") %in% names(lin$statistics$loess)) &&
   any(grepl("span", lin$parameters$parameter)),
   "F6: the LOESS smooth is computed by the reference at every row and its span/degree declared")
ok(any(grepl("dose_group", lin$notes)) && any(grepl("eda_dose_group", lin$notes)),
   "F7: an unresolved colour role is recorded with the flag the spec must gain")
ok(length(builds$indiv$pages) == 2L,
   "F7: individual profiles paginate at the declared subjects per page (3 subjects, 2 per page)")
ok(is.data.frame(builds$hist$statistics$bins) &&
   all(c("variable", "bin", "lower", "upper", "count") %in% names(builds$hist$statistics$bins)) &&
   sum(builds$hist$statistics$bins$count[builds$hist$statistics$bins$variable == "WT"]) == 3L,
   "F6: histogram bins and counts are computed by the reference")
ok(is.data.frame(builds$pairs$statistics$correlations) &&
   nrow(builds$pairs$statistics$correlations) == 1L &&
   all(c("x", "y", "r", "n") %in% names(builds$pairs$statistics$correlations)),
   "F6: pairs-plot correlations are computed by the reference (pmplots/GGally not used)")
ok(is.data.frame(builds$box$statistics$box) &&
   all(c("variable", "by", "level", "q1", "median", "q3", "whisker_low", "whisker_high", "n")
       %in% names(builds$box$statistics$box)),
   "F7: box statistics (quartiles, whiskers) are computed by the reference")
ok(is.data.frame(builds$strat$derived) && nrow(builds$strat$derived) == 1L &&
   grepl("cut points", builds$strat$derived$computed),
   "F6: a derive block's computed provenance is returned with the build")

# Every layer frame carries the row id and digests to the view the layer
# declares; a paginated figure's pages partition the view.
written <- lapply(active, function(nm)
  quiet(R("reference_eda_write_figure")(builds[[nm]], reg[[nm]], nm,
                                        file.path(out_dir, reg[[nm]]$file))))
names(written) <- active
ok(all(vapply(written, function(w) grepl("^sha256:[0-9a-f]{64}$", w$sha256) &&
              file.exists(w$path), logical(1))),
   "M1: the reference writes every figure PDF and returns the sha256 of the bytes")
checks <- lapply(active, function(nm)
  quiet(R("reference_eda_figure_check")(v, builds[[nm]], reg[[nm]], nm,
                                        written[[nm]]$path, sha256 = written[[nm]]$sha256)))
names(checks) <- active
ok(all(vapply(active, function(nm) identical(checks[[nm]]$file$sha256, written[[nm]]$sha256),
              logical(1))),
   "M1: the check reads the file back and confirms the written digest")
ok(all(vapply(checks, function(ch) identical(ch$row_digest,
              unname(v$digests[[reg[[ch$plot]]$rows]])), logical(1))),
   "F6: every figure's main row set digests to the view its entry declares")
ok(any(checks$indiv$layers$rows == "doses") &&
   all(checks$indiv$layers$ok),
   "F7: the dose-marker layer digests to the doses view, across pages")
ok(all(vapply(checks, function(ch) all(ch$reconciliation$ok), logical(1))),
   "F9: every built layer reconciles with ggplot_build (rows and non-finite counts)")
rec_log <- checks$ct_log$reconciliation
ok(all(rec_log$n_nonfinite_built[rec_log$rows == "observations"] == 2L) &&
   all(rec_log$n_expected_nonfinite[rec_log$rows == "observations"] == 2L),
   "F9: under log10 the built layers carry exactly the audit's dropped rows as non-finite")
ok(identical(checks$ct_log$file$pages, 1L) &&
   isTRUE(all.equal(checks$ct_log$file$width_in, 7)) &&
   isTRUE(all.equal(checks$ct_log$file$height_in, 5)),
   "F6: the rendered PDF's page size and page count are read back from the file")
ok(identical(checks$indiv$file$pages, 2L),
   "F7: the paginated PDF has one page per declared page")
# A FOREIGN PDF of the declared size (one blank page) is refused: its bytes
# are not the bytes the reference wrote.
foreign <- file.path(out_dir, "foreign", "ct-lin.pdf"); dir.create(dirname(foreign))
grDevices::pdf(foreign, width = 7, height = 5); plot.new(); grDevices::dev.off()
expect_stop(quiet(R("reference_eda_figure_check")(v, builds$ct_lin, reg$ct_lin, "ct_lin", foreign,
                                                  sha256 = written$ct_lin$sha256)),
            "M1: a foreign PDF of the declared size fails the figure check on its bytes",
            "not the file the reference wrote")
wrong_dims <- file.path(out_dir, "wrong.pdf")
grDevices::pdf(wrong_dims, width = 3, height = 3); quiet(print(builds$ct_lin$pages[[1]])); grDevices::dev.off()
expect_stop(quiet(R("reference_eda_figure_check")(v, builds$ct_lin, reg$ct_lin, "ct_lin", wrong_dims)),
            "F6: a PDF whose page size differs from the declaration is refused",
            "page size")
# A build whose LAYER data was tampered with (a filtered frame) is caught. The
# build is redone so the shared ggproto layer of `builds` is not mutated.
tampered <- quiet(R("reference_eda_build_figure")(v, reg$ct_lin, "ct_lin", cols_s, labels,
                                                  flags = flags, decodes = decodes))
tampered$pages[[1]]$layers[[2]]$data <- tampered$pages[[1]]$layers[[2]]$data[-1, , drop = FALSE]
expect_stop(quiet(R("reference_eda_figure_check")(v, tampered, reg$ct_lin, "ct_lin",
                                                  file.path(out_dir, "ct-lin.pdf"))),
            "F6: a page whose data is not the declared row set is refused",
            "row set")

# ==== the registry index: bijection, files, dimensions ======================
rendered <- do.call(rbind, lapply(active, function(nm) {
  ch <- checks[[nm]]; e <- reg[[nm]]
  data.frame(plot = nm, file = e$file, path = file.path(out_dir, e$file), rows = e$rows,
             row_digest = ch$row_digest, w = ch$file$width_in, h = ch$file$height_in,
             pages = ch$file$pages, sha256 = ch$file$sha256, stringsAsFactors = FALSE)
}))
idx <- R("reference_eda_registry_index")(rt, "strat", rendered, v$digests)
ok("sha256" %in% names(idx) && all(grepl("^sha256:", idx$sha256[idx$rendered])),
   "M1: the index carries the written digest of every rendered figure")
foreign_idx <- rendered; foreign_idx$path[foreign_idx$plot == "ct_lin"] <- foreign
expect_stop(R("reference_eda_registry_index")(rt, "strat", foreign_idx, v$digests),
            "M1: a foreign PDF of the declared size fails the registry index at packaging",
            "not the bytes the reference wrote")
ok(nrow(idx) == 7L && all(idx$status == "rendered"),
   "F6: with every active entry rendered once the index reports all rendered")
expect_stop(R("reference_eda_registry_index")(rt, "strat", rbind(rendered, rendered[1, ]), v$digests),
            "F6: a duplicate render is refused", "more than once")
expect_stop(R("reference_eda_registry_index")(rt, character(0), rendered, v$digests),
            "F6: rendering a disabled catalog entry is refused", "not enabled")
expect_stop(R("reference_eda_registry_index")(rt, "strat", rendered[-1, ], v$digests),
            "F7: a core entry that is neither rendered nor inapplicable-with-reason is refused",
            "neither rendered nor")
inapp <- data.frame(plot = "box", reason = "inapplicable: no column flagged eda_covariate_categorical",
                    stringsAsFactors = FALSE)
idx2 <- R("reference_eda_registry_index")(rt, "strat", rendered[rendered$plot != "box", ],
                                          v$digests, inapplicable = inapp)
ok(identical(idx2$status[idx2$plot == "box"], "inapplicable") &&
   grepl("eda_covariate_categorical", idx2$reason[idx2$plot == "box"]),
   "F7: an inapplicable core entry appears in the index with its reason")
expect_stop(R("reference_eda_registry_index")(rt, "strat", rendered, v$digests, inapplicable = inapp),
            "F7: an entry both rendered and declared inapplicable is refused", "both")
bad_file <- rendered; bad_file$file[1] <- "other.pdf"
expect_stop(R("reference_eda_registry_index")(rt, "strat", bad_file, v$digests),
            "F6: a rendered file name differing from the declaration is refused", "file")
bad_w <- rendered; bad_w$w[1] <- 3
expect_stop(R("reference_eda_registry_index")(rt, "strat", bad_w, v$digests),
            "F6: rendered dimensions differing from the declaration are refused", "dimension")

# The inapplicable state is produced by the BUILDER when a required role is
# unresolved, with the reason naming the flag.
cols_nocat <- cols_s; cols_nocat$covariate_categorical <- character(0)
b_box <- quiet(R("reference_eda_build_figure")(v, reg$box, "box", cols_nocat, labels,
                                               flags = flags, decodes = decodes))
ok(!is.null(b_box$inapplicable) && grepl("eda_covariate_categorical", b_box$inapplicable) &&
   length(b_box$pages) == 0L,
   "F7: boxplots with no categorical covariate are inapplicable, reason names the flag")

# Core deliverable status is enforced for tables as well as figures.
core_tables <- data.frame(
  deliverable = c("data inventory", "data inventory grouping by study",
                  "data inventory grouping by dose group", "continuous covariate summary",
                  "categorical covariate summary"),
  status = c("computed", "computed", "inapplicable", "computed", "inapplicable"),
  reason = c("", "", "inapplicable: no column flagged eda_dose_group", "",
             "inapplicable: no column flagged eda_covariate_categorical"),
  stringsAsFactors = FALSE)
st <- R("reference_eda_core_status")(figures = idx2, tables = core_tables)
ok(is.data.frame(st$table) && sum(st$table$status == "inapplicable") == 3L &&
   is.data.frame(st$counts) && st$counts$value[st$counts$id == "n_core_inapplicable"] == 3,
   "F7: the core-deliverable status table lists every core item with its state, counts by the reference")
no_reason <- core_tables; no_reason$reason[5] <- ""
expect_stop(R("reference_eda_core_status")(figures = idx2, tables = no_reason),
  "F7: an inapplicable deliverable without a reason is refused", "reason")
expect_stop(R("reference_eda_core_status")(figures = idx2, tables = core_tables[1, ]),
  "M2: a caller listing only one core table is refused (EDA_CORE_TABLES is canonical)",
  "core table\\(s\\) neither computed nor recorded inapplicable")
# L2: a boxplot level with no finite value is recorded, not skipped.
v_l2 <- v; v_l2$rows$subjects$WT[v_l2$rows$subjects$SEX == 2] <- NA
v_l2$rows$subjects$AGE[v_l2$rows$subjects$SEX == 2] <- NA
b_l2 <- quiet(R("reference_eda_build_figure")(v_l2, reg$box, "box", cols_s, labels,
                                              flags = flags, decodes = decodes))
ok(any(grepl("no finite", b_l2$notes)) &&
   any(b_l2$statistics$box$n == 0L & b_l2$statistics$box$level == "female"),
   "L2: a boxplot level with no finite value is a recorded note and a zero-n row")

# ==== F8: the generated overlay, and declared composition ==================
e2e <- tempfile("public-eda-composition-")
dir.create(e2e)
copy_in <- function(rel) {
  dest <- file.path(e2e, rel)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  stopifnot(file.copy(rel, dest))
}
for (p in c(REF, "pipeline/compose.R", "pipeline/tasks/eda.R",
            list.files("tools", pattern = "[.]R$", full.names = TRUE),
            "config/engine.yaml", "config/r-packages.lock")) copy_in(p)
stopifnot(file.copy(file.path(FIX, "skills", "eda-standards", "MANIFEST.yaml"),
                    file.path(e2e, "skills", "eda-standards")))
in_project <- function(expr) {
  wd <- setwd(e2e); on.exit(setwd(wd)); force(expr)
}
fixture <- in_project(write_public_eda_inputs("inputs"))
if (has_yspec) in_project({
  prov <- ys_overlay_provenance(fixture$overlay)
  ok(identical(prov$base_spec, fixture$base) &&
     identical(prov$base_spec_sha256, .mm_file_sha256(fixture$base)),
     "F8: the overlay records the base spec and its current digest")
  proj <- quiet(ys_projection(fixture$overlay))
  ok(identical(proj$overlay$base_spec_sha256, prov$base_spec_sha256),
     "F8: the EDA projection carries the base-spec digest")
  writeLines(c(readLines(fixture$base), "# changed base bytes"), fixture$base)
  expect_stop(compose_pipeline("EDA-SYNTH", "eda",
    list(eda = list(external_input = fixture$dataset, external_spec = fixture$overlay))),
    "F8: composition refuses a changed base specification", "base spec")
  write_public_eda_inputs("inputs")

  source("tools/load_reference.R", local = TRUE)
  source("tools/eda_application.R", local = TRUE)
  ref <- eda_application_reference(REF)
  registry <- eda_application_registry()
  cols <- eda_application_columns(proj, registry, c("dose_normalized", "covariate_stratified"))
  ok(identical(cols$dose, "DOSE") && identical(cols$event, "EVID") &&
     identical(as.integer(ys_decode_values(proj, cols$blq, "below LLOQ")), 1L),
     "F8: the application resolves the observed dose, event and BLQ from the overlay")
  da <- quiet(read_external_dataset(fixture$dataset, fixture$overlay))
  view <- eda_application_view(ref, da, proj, cols)
  inventory <- ref$reference_eda_inventory(view, blq_col = "BLQ", blq_values = 1)$table
  summary <- ref$reference_eda_continuous_summary(view, c("WT", "DOSE"), "subjects")$table
  audited <- ref$reference_eda_log_axis_audit(view, "DV", rows = "observations",
                                               blq_col = "BLQ", blq_values = 1)$audit
  ok(identical(as.integer(view$input_counts$value), c(16L, 11L, 4L)) &&
     all(inventory$n_subjects == 4L, inventory$n_obs == 11L,
         inventory$n_blq == 1L, inventory$n_excluded == 1L),
     "EDA-COMPOSE/F8: independent generated record, subject, observation and exclusion counts")
  ok(all(summary$n == 4L) &&
     identical(summary$median[match(c("WT", "DOSE"), summary$variable)], c(62.5, 12.5)) &&
     as.integer(audited$value[audited$id == "n_rows"]) == 11L,
     "EDA-COMPOSE/F8: generated covariate summaries and observation audit agree")
}) else validated_only("EDA-COMPOSE/F8", "yspec not installed; F8 overlay checks not run")

if (has_yspec && requireNamespace("pmtables", quietly = TRUE)) in_project({
  writeLines(c("environment_confirmed: true", "r_provision: managed"), "harness.yaml")
  tools <- c("external", "load_reference", "eda_application", "evidence_notes",
             "engine_config", "gate_package", "check_environment", "yaml_scalar")
  writeLines(c("library(targets)", sprintf("source('tools/%s.R')", tools),
               "source('pipeline/compose.R')",
               "tar_option_set(format = 'rds')",
               "registry <- eda_application_registry()",
               "registry$dose_normalized$file <- 'declared-dose-normalized.pdf'",
               "compose_pipeline('EDA-SYNTH', 'eda', list(eda = list(",
               "  external_input = 'inputs/dataset.csv', external_spec = 'inputs/eda-overlay.yml',",
               "  registry = registry, catalog = c('dose_normalized', 'covariate_stratified'),",
               "  out_dir = 'out')))"), "eda-analysis.R")
  result <- tryCatch({
    targets::tar_make(names = tidyselect::all_of("eda_package"), script = "eda-analysis.R",
                      store = "store", reporter = "silent")
    targets::tar_read(eda_package, store = "store")
  }, error = function(e) {
    cat("      ", conditionMessage(e), "\n", sep = "")
    NULL
  })
  ok(is.list(result), "EDA-COMPOSE: the approved local method reaches the composed EDA package")
  if (is.list(result)) {
    ok(all(file.exists(result$artifacts$path)),
       "EDA-COMPOSE: all published artifacts exist")
    outputs <- list.files("out")
    ok(setequal(outputs, c("inventory.tex", "inventory.csv", "continuous_covariates.tex",
             "continuous_covariates.csv", "profiles-linear.pdf", "profiles-log.pdf",
             "individual-profiles.pdf", "covariate-histograms.pdf", "covariate-pairs.pdf",
             "declared-dose-normalized.pdf", "covariate-stratified.pdf")),
       "EDA-COMPOSE: the applicable core and named catalog artifacts exist")
    direct_audit <- result$tables[["Log-axis audit per declared log axis (skills/eda-standards)"]]
    ok(identical(result$tables[["Data inventory (records not excluded; by declared grouping when the specification flags one)"]],
                 inventory) &&
       identical(result$tables[["Continuous covariate summary (one row per subject)"]], summary) &&
       identical(direct_audit$n_rows[direct_audit$plot == "profiles_log"],
                 audited$value[audited$id == "n_rows"]),
       "EDA-COMPOSE: the package agrees with the direct inventory, summary and log row count")
    dir.create("gates/EDA-SYNTH", recursive = TRUE)
    gate <- "gates/EDA-SYNTH/eda.md"
    writeLines(c("# EDA synthetic fixture", "Analysis ID: EDA-SYNTH", "Stage: eda",
      "<!-- BEGIN GENERATED PACKAGE -->", "<!-- END GENERATED PACKAGE -->", "",
      "## SME decision", "", "- [ ] APPROVED", "- [ ] APPROVED WITH CHANGES",
      "- [ ] REJECTED", "", "Comments / required changes:", "", "Signed:", "Date:",
      "Digest signed:"), gate)
    source("tools/gate.R", local = TRUE)
    render_gate_package(gate, result$artifacts, result$tables, result$notes)
    gate_txt <- paste(readLines(gate), collapse = "\n")
    has <- function(x) grepl(x, gate_txt, fixed = TRUE)
    ok(has("categorical covariate summary") && has("covariate_boxplots") &&
       has("inapplicable") && has("eda_covariate_categorical") &&
       has("grouping by study") && has("grouping by dose group") &&
       has("colour by role 'dose_group' inapplicable"),
       "EDA-COMPOSE/F7: missing optional roles remain visible in the gate package")
    ok(has("| all | 4 | 11 | 1 | 1 | 16 |") && has("smooth span") &&
       has("smooth degree") && has("sample standard deviation, denominator n - 1") &&
       has("R type 7") && has("kept-record rule") && has("profiles_log") &&
       has("base specification the overlay is bound to | inputs/base-spec.yml") &&
       has("base yspec specification the dataset was ASSEMBLED against"),
       "EDA-COMPOSE/F3/F8/F9: gate records inventory, parameters, log audit and binding")
  }
}) else validated_only("EDA-COMPOSE/E2E", "yspec/pmtables not installed; end-to-end fixture run not performed")

ok(identical(digest::digest(file = REAL_MF, algo = "sha256"), real_mf_sha_before),
   "fixture: the REAL manifest's bytes are unchanged by this test run (sha256 equal)")

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0) quit(status = 1)
invisible(TRUE)
}

if (.invoked_as_script()) .run_eda_declarative_tests()
