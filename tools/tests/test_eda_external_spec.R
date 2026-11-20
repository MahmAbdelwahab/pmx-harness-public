#!/usr/bin/env Rscript
# Tests for the "external dataset without a yspec specification is a STOP"
# rule (SME ruling, 2026-09-02).
#
# Covered:
#   (a) an analysis declaring inputs$eda$external_input with no
#       external_spec fails at COMPOSITION time, with the message that tells
#       the SME what to declare instead;
#   (b) with a matching spec the pipeline composes (tar_manifest) and
#       read_external_dataset(csv, spec) returns a data frame carrying
#       external_source, external_spec and external_spec_sha256;
#   (c) with a deliberately violated spec (a value outside a declared range)
#       read_external_dataset stops;
#   (d) the spec = NULL path is unchanged — one attribute, no spec
#       attributes, same rows and columns (other callers, e.g.
#       pipeline/tasks/simulation.R, still read without a spec);
#   (e) the eda_package target and its dependency structure;
#   (f) the in-analysis (data_assembly) dataset branch, and the data-assembly
#       module's optional inputs design_constants and gate_text.
#
# Extended 2026-09-03 for round-6 findings 2, 3, 4 and 12:
#   (g) the eda-standards method manifest gates the EDA numerics — every
#       reference entry point refuses while its unit is draft — and neither
#       the task module nor the analysis helper names a dataset column or
#       detects a log axis by deparsing a plot function;
#   (h) the reference numerics themselves, on a fixture (record selection and
#       row-set digests, inventory, covariate summary, log-axis census,
#       quantile bins, normalisation), exercised through a MANIFEST FIXTURE in
#       a tempdir (units approved with the current digests, reviewed_by
#       "test-fixture") — never through a validator stub, which the reference
#       no longer honours (round 7, F4), and never by touching the real
#       manifest;
#   (i) plot-registry validation and the per-figure row-set assertion,
#       including the decisive case a row COUNT check would pass: a different
#       row set of the same size;
#   (j) the yspec projection, spec-derived axis labels, decode-derived value
#       meanings, and the STOP when a column role is not annotated;
#   (k) MUTATION TESTS: altering the dataset, then the specification, in
#       place at the same path makes eda_input, eda_spec and eda_package
#       outdated in a fixture store.
#
# Fixtures and stores live under tempdir(). Composition creates no files (the
# gate record path is only a string at compose time). Run with
#   Rscript tools/tests/test_eda_external_spec.R
# Exits non-zero if any assertion fails.
#
# SAFE TO SOURCE: guarded exactly like tools/tests/test_batch4.R, so
# tar_source("tools") defines and runs nothing.
.invoked_as_script <- function() {
  fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  length(fa) > 0 && grepl("test_eda_external_spec\\.R$", sub("^--file=", "", fa[1]))
}

.run_eda_external_spec_tests <- function() {

args <- commandArgs(FALSE)
file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
TOOLS <- normalizePath(file.path(dirname(file_arg), ".."), mustWork = TRUE)
REPO  <- normalizePath(file.path(TOOLS, ".."), mustWork = TRUE)
# compose_pipeline() resolves task modules relative to the repository root.
setwd(REPO)
suppressWarnings(suppressMessages({
  source(file.path(TOOLS, "external.R"))
  source(file.path(REPO, "pipeline", "compose.R"))
}))
# compose.R and the task modules use %||%, which the harness defines in
# tools/gate_package.R; the composer resolves it in the global environment,
# so define it there when this file is run on its own.
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
source(file.path(TOOLS, "tests", "fixtures", "public_workflow", "eda_inputs.R"), local = TRUE)
# Visible skip. With PMX_CI_NO_SKIP=1 (the validated-stack CI job; Codex round
# 7, finding 14) a skip is counted as a FAILURE: a machine that claims the full
# stack must not go green by skipping.
skip <- function(why) {
  if (nzchar(Sys.getenv("PMX_CI_NO_SKIP", ""))) {
    .fail <<- .fail + 1L
    cat(sprintf("FAIL  would skip: %s (PMX_CI_NO_SKIP is set, so a skip is a failure)\n", why))
  } else cat("SKIP: ", why, "\n", sep = "")
}

# Parts (b-read) and (c) exercise yspec::ys_check for real. Hosted CI installs
# CRAN packages only, and yspec ships from MPN, so those parts are skipped
# there with a visible SKIP line rather than failing. Set PMX_TEST_NO_YSPEC=1
# to exercise the skip path on a machine that has yspec.
has_yspec <- requireNamespace("yspec", quietly = TRUE) &&
  !nzchar(Sys.getenv("PMX_TEST_NO_YSPEC"))

# --- fixtures: a tiny dataset and the spec it was assembled against ---------
project <- tempfile("public-eda-project-")
dir.create(project)
stopifnot(file.copy(file.path(REPO, "pipeline"), project, recursive = TRUE))
dir.create(file.path(project, "skills"))
stopifnot(file.copy(file.path(REPO, "skills", "eda-standards"),
                    file.path(project, "skills"), recursive = TRUE))
dir.create(file.path(project, "tools"))
dir.create(file.path(project, "config"))
for (p in c(list.files(TOOLS, pattern = "[.]R$", full.names = TRUE),
            file.path(REPO, "config", c("engine.yaml", "r-packages.lock")))) {
  subdir <- if (dirname(p) == TOOLS) "tools" else "config"
  stopifnot(file.copy(p, file.path(project, subdir)))
}
on.exit(setwd(REPO), add = TRUE)
setwd(project)
fx <- "fixture"; dir.create(fx)
csv_ok  <- file.path(fx, "toy-da.csv")
csv_bad <- file.path(fx, "toy-da-violating.csv")
spec    <- file.path(fx, "toy-spec.yml")

writeLines(c(
  "SETUP__:",
  "  description: toy dataset for the external-spec tests",
  "  use_internal_db: false",
  "ID:",
  "  short: subject identifier",
  "  type: numeric",
  "TIME:",
  "  short: time after first dose",
  "  unit: h",
  "  type: numeric",
  "  range: [0, 30]",
  "DV:",
  "  short: concentration",
  "  unit: mg/L",
  "  type: numeric"), spec)

writeLines(c("ID,TIME,DV",
             "1,0,0.5",
             "1,2,1.2",
             "2,0,0.4",
             "2,2,1.1"), csv_ok)
# Same shape, one TIME value outside the spec's declared [0, 30] range.
writeLines(c("ID,TIME,DV",
             "1,0,0.5",
             "1,999,1.2",
             "2,0,0.4",
             "2,2,1.1"), csv_bad)

# A temp analysis in the shape of analyses/*.R, declaring only the eda task.
# The generated files stay inside the disposable composition project.
write_analysis <- function(path, external_input, external_spec = NULL) {
  spec_line <- if (is.null(external_spec)) "" else
    sprintf("      external_spec = %s,", shQuote(external_spec, type = "cmd"))
  writeLines(c(
    "library(targets)",
    "tar_source(\"tools\")",
    "source(\"pipeline/compose.R\")",
    "tar_option_set(format = \"rds\")",
    "compose_pipeline(",
    "  analysis_id = \"TST-EDASPEC\",",
    "  tasks = \"eda\",",
    "  inputs = list(",
    "    eda = list(",
    sprintf("      external_input = %s,", shQuote(external_input, type = "cmd")),
    spec_line,
    "      catalog = character(0)",
    "    )",
    "  )",
    ")"), path)
  path
}

compose_eda <- function(external_input, external_spec = NULL) {
  eda_input <- list(external_input = external_input)
  if (!is.null(external_spec)) eda_input$external_spec <- external_spec
  compose_pipeline(analysis_id = "TST-EDASPEC", tasks = "eda",
                   inputs = list(eda = eda_input))
}

# ==== (a) external_input with no spec is a composition-time STOP ============
expect_stop(
  compose_eda(csv_ok),
  "a: external_input without external_spec stops at composition",
  "external_input declared without a yspec specification")

expect_stop(
  compose_eda(csv_ok, external_spec = ""),
  "a: an empty external_spec is refused the same way",
  "external_input declared without a yspec specification")

expect_stop(
  compose_eda(csv_ok, external_spec = NA_character_),
  "a: a missing (NA) external_spec is refused the same way",
  "external_input declared without a yspec specification")

# Two specs for one dataset is a declaration error with its own message,
# rather than a silently used first element.
expect_stop(
  compose_eda(csv_ok, external_spec = c(spec, spec)),
  "a: two external_spec entries are refused with their own message",
  "must name exactly one yspec file")

# The message must name the two ways out, so the SME/agent reading it knows
# what to do rather than only that something was missing.
msg_a <- tryCatch({ compose_eda(csv_ok); "" }, error = conditionMessage)
ok(grepl("inputs\\$eda\\$external_spec", msg_a) && grepl("data_assembly", msg_a),
   "a: the stop message names both remedies (declare a spec, or add data_assembly)")

# The same refusal must reach an analysis through tar_manifest(), not only a
# direct compose_pipeline() call.
an_nospec <- write_analysis(file.path(fx, "TST-EDASPEC-nospec.R"), csv_ok)
expect_stop(
  quiet(targets::tar_manifest(script = an_nospec)),
  "a: tar_manifest on such an analysis fails with the same message",
  "external_input declared without a yspec specification")

# ==== (b) with a matching spec: composes, and the read is spec-checked ======
pipe <- compose_eda(csv_ok, external_spec = spec)
target_names <- vapply(pipe, function(t) t$settings$name, character(1))
ok(all(c("eda_input", "eda_tables", "eda_figures") %in% target_names),
   "b: the eda task still composes its three stage targets")

# The dataset and the spec are FILE targets (round-6 finding 3), and the
# eda_input command consumes those TARGET SYMBOLS rather than literal paths,
# so a byte change at the same path invalidates the read. The literal paths
# live in the file targets' own commands, where tar_manifest still shows them.
fmt_of <- function(pipe, name) {
  nms <- vapply(pipe, function(t) t$settings$name, character(1))
  pipe[[which(nms == name)]]$settings$format
}
ok(identical(fmt_of(pipe, "eda_external_dataset_file"), "file") &&
   identical(fmt_of(pipe, "eda_external_spec_file"), "file"),
   "b: the dataset and the spec are format = \"file\" targets")
cmd <- paste(deparse(pipe[[which(target_names == "eda_input")]]$command$expr[[1]]),
             collapse = " ")
ok(grepl("eda_external_dataset_file", cmd, fixed = TRUE) &&
   grepl("eda_external_spec_file", cmd, fixed = TRUE) &&
   !grepl(basename(csv_ok), cmd, fixed = TRUE),
   "b: eda_input consumes the file-target symbols, not literal paths")
ds_cmd <- paste(deparse(pipe[[which(target_names == "eda_external_dataset_file")]]$command$expr[[1]]),
                collapse = " ")
sp_cmd <- paste(deparse(pipe[[which(target_names == "eda_external_spec_file")]]$command$expr[[1]]),
                collapse = " ")
ok(grepl(basename(csv_ok), ds_cmd, fixed = TRUE) &&
   grepl(basename(spec), sp_cmd, fixed = TRUE),
   "b: the file targets carry the declared dataset and spec paths")

an_spec <- write_analysis(file.path(fx, "TST-EDASPEC-spec.R"), csv_ok, spec)
man <- tryCatch(quiet(targets::tar_manifest(script = an_spec)),
                error = function(e) e)
ok(!inherits(man, "error") && all(c("eda_input", "eda_tables", "eda_figures",
                                    "gate_eda", "gate_eda_file") %in% man$name),
   "EDA-SPEC: tar_manifest composes the analysis (stage targets plus generated gate)")

if (has_yspec) {
d <- quiet(read_external_dataset(csv_ok, spec))
ok(is.data.frame(d) && nrow(d) == 4L && ncol(d) == 3L,
   "b: read_external_dataset returns the data frame (4 rows, 3 columns)")
ok(identical(attr(d, "external_source"), csv_ok),
   "b: external_source attribute records the dataset path")
ok(identical(attr(d, "external_spec"), spec),
   "b: external_spec attribute records the spec path")
ok(identical(attr(d, "external_spec_sha256"),
             digest::digest(file = spec, algo = "sha256")),
   "b: external_spec_sha256 is the sha256 of the spec file, recomputed here")

# ==== (c) a violated spec stops the read ===================================
expect_stop(
  quiet(read_external_dataset(csv_bad, spec)),
  "c: a value outside the spec's declared range stops the read")

msg_c <- tryCatch({ quiet(read_external_dataset(csv_bad, spec)); "" },
                  error = conditionMessage)
ok(grepl(basename(csv_bad), msg_c, fixed = TRUE) &&
   grepl(basename(spec), msg_c, fixed = TRUE),
   "c: the failure message names the dataset and the spec that rejected it")
ok(grepl("TIME", msg_c) && grepl("range", msg_c),
   "c: the yspec finding itself (column and reason) reaches the message")
} else {
  skip("yspec not installed; spec-checked read (b) and violated-spec stop (c) not run")
}

# ==== (d) spec = NULL behaviour unchanged ==================================
d0 <- read_external_dataset(csv_ok)
plain <- utils::read.csv(csv_ok, na.strings = ".", stringsAsFactors = FALSE)
ok(identical(as.data.frame(unclass(d0))[names(plain)], plain),
   "d: spec = NULL returns the same data as before")
ok(identical(attr(d0, "external_source"), csv_ok) &&
   is.null(attr(d0, "external_spec")) &&
   is.null(attr(d0, "external_spec_sha256")),
   "d: spec = NULL attaches external_source only, no spec attributes")
expect_stop(read_external_dataset(file.path(fx, "absent.csv")),
            "d: a missing dataset still stops with the original message",
            "External dataset not found")

# ==== (e) the eda_package gate-package target ==============================
# render_gate_from_store(<ID>, "eda") reads the `eda_package` target of the
# store, so the target must exist and must DEPEND on the stage's produced
# targets — otherwise the package could be built (and a gate signed) without
# the work it describes.
cmd_of <- function(pipe, name) {
  nms <- vapply(pipe, function(t) t$settings$name, character(1))
  pipe[[which(nms == name)]]$command$expr[[1]]
}
syms_of <- function(pipe, name) all.names(cmd_of(pipe, name))

pipe_e <- compose_eda(csv_ok, external_spec = spec)
names_e <- vapply(pipe_e, function(t) t$settings$name, character(1))
ok("eda_package" %in% names_e,
   "e: the eda task composes an eda_package target")

# Every produced target must be an ANCESTOR of eda_package in the composed
# DAG (directly or through another stage target): a package built without
# the work it describes is the defect this check exists to prevent.
ancestors_of <- function(pipe, target) {
  nms <- vapply(pipe, function(t) t$settings$name, character(1))
  seen <- character(0); frontier <- target
  while (length(frontier)) {
    step <- unlist(lapply(frontier, function(f)
      if (f %in% nms) intersect(syms_of(pipe, f), nms) else character(0)))
    frontier <- setdiff(unique(step), seen)
    seen <- unique(c(seen, frontier))
  }
  seen
}
anc_e <- ancestors_of(pipe_e, "eda_package")
ok(all(c("eda_input", "eda_tables", "eda_figures", "eda_provenance",
         "eda_figure_index", "eda_log_audit", "eda_application_file") %in% anc_e),
   "e: every produced target of the stage is an ancestor of eda_package")
application_cmd <- cmd_of(pipe_e, "eda_application_file")
ok(identical(fmt_of(pipe_e, "eda_application_file"), "file") &&
   identical(application_cmd[[length(application_cmd)]],
             quote(identity("tools/eda_application.R"))),
   "e: the public application source is tracked as a file")
ok(all(c("eda_tables", "eda_provenance", "eda_figure_index", "eda_log_audit")
       %in% syms_of(pipe_e, "eda_package")) &&
   "eda_figures" %in% syms_of(pipe_e, "eda_figure_index"),
   "e: the rendered figure set reaches the package through the registry index")

# The composer's own dominance invariant must hold for the composed pipeline
# (it also asserts the method-manifest edge on every stage target).
ok(isTRUE(tryCatch(verify_gate_dominance(pipe_e), error = function(e) {
     cat(sprintf("      error: %s\n", conditionMessage(e))); FALSE })),
   "e: the composed eda pipeline satisfies the gate-dominance invariant")

# The optional helper-code hook: declared, it becomes a hashed file target;
# undeclared, no such target exists.
ok(!("eda_code_file" %in% names_e),
   "e: no eda_code_file target when the analysis declares no code")
writeLines("# Fixture helper", file.path(fx, "eda-helpers.R"))
pipe_code <- compose_pipeline(
  analysis_id = "TST-EDASPEC", tasks = "eda",
  inputs = list(eda = list(external_input = csv_ok, external_spec = spec,
                           code = file.path(fx, "eda-helpers.R"))))
names_code <- vapply(pipe_code, function(t) t$settings$name, character(1))
ok("eda_code_file" %in% names_code &&
   identical(pipe_code[[which(names_code == "eda_code_file")]]$settings$format,
             "file"),
   "e: a declared inputs$eda$code becomes a file target (hashed at the gate)")
expect_stop(
  compose_pipeline(analysis_id = "TST-EDASPEC", tasks = "eda",
                   inputs = list(eda = list(external_input = csv_ok,
                                            external_spec = spec, catalog = 1))),
  "e: a non-character catalog declaration is refused at composition",
  "must be a character vector")

# ==== (f) the in-analysis dataset branch ===================================
# With a data_assembly task in the same analysis there are no external
# attributes: eda_input is the `dataset` target and the package still
# composes, with the provenance prose pointing at the data-assembly gate.
map <- file.path(fx, "map.md")
writeLines("# Synthetic analysis plan", map)
da_base <- list(source = csv_ok, spec = spec, map = map,
                expect = list(n_subjects = 2L, source_rows = 4L, lloq = NA_real_))
pipe_f <- compose_pipeline(
  analysis_id = "TST-EDADA",
  tasks = c("data_assembly", "eda"),
  inputs = list(
    data_assembly = da_base,
    eda = list(catalog = character(0))))
names_f <- vapply(pipe_f, function(t) t$settings$name, character(1))
ok(all(c("dataset", "eda_input", "eda_package") %in% names_f),
   "f: eda composes after data_assembly, with its package target")
ok("dataset" %in% syms_of(pipe_f, "eda_input"),
   "f: eda_input is the in-analysis `dataset` target, not an external read")
ok(!grepl("read_external_dataset",
          paste(deparse(cmd_of(pipe_f, "eda_input")), collapse = " ")),
   "f: no external read is composed when the dataset comes from data_assembly")
pkg_txt_f <- paste(deparse(cmd_of(pipe_f, "eda_package")), collapse = " ")
ok(grepl("in-analysis `dataset` target", pkg_txt_f, fixed = TRUE) &&
   grepl("data-assembly gate", pkg_txt_f, fixed = TRUE),
   "f: the package's provenance note names the in-analysis dataset and its gate")
ok(!grepl("spec_sha256", pkg_txt_f, fixed = TRUE),
   "f: no spec-hash evidence note is composed in that branch (there is no spec attribute)")
pkg_txt_e <- paste(deparse(cmd_of(pipe_e, "eda_package")), collapse = " ")
ok(grepl("ids[[\"dataset\"]]", pkg_txt_e, fixed = TRUE) &&
   grepl("ids[[\"spec\"]]", pkg_txt_e, fixed = TRUE) &&
   grepl("ids[[\"spec_sha256\"]]", pkg_txt_e, fixed = TRUE),
   "f: the external branch composes the dataset/spec/spec-hash evidence notes")
ok(grepl("external_source", paste(deparse(cmd_of(pipe_e, "eda_provenance")),
                                  collapse = " "), fixed = TRUE),
   "f: provenance is read off the data object's own attributes")
ok(isTRUE(tryCatch(verify_gate_dominance(pipe_f), error = function(e) {
     cat(sprintf("      error: %s\n", conditionMessage(e))); FALSE })),
   "f: the two-task pipeline satisfies the gate-dominance invariant")

# The public assembly package binds declared inputs and optional constants.
compose_da <- function(...) compose_pipeline("TST-EDADA", "data_assembly",
  inputs = list(data_assembly = c(da_base, list(...))))
names_of <- function(pipe) vapply(pipe, function(t) t$settings$name, character(1))
pipe_da <- compose_da()
ok(!("da_design_constants" %in% names_of(pipe_da)),
   "f: without design_constants there is no extra target")
design <- file.path(fx, "design.csv")
writeLines(c("quantity,value", "declared duration,6"), design)
pipe_dc <- compose_da(design_constants = design)
ok(identical(fmt_of(pipe_dc, "da_design_constants_file"), "file") &&
   "da_design_constants_file" %in% syms_of(pipe_dc, "da_design_constants") &&
   all(c("da_design_constants", "da_design_constants_file") %in%
       syms_of(pipe_dc, "data_assembly_package")),
   "f: declared design constants are a tracked input and a package table")
expect_stop(compose_da(design_constants = file.path(fx, "absent-design.csv")),
            "f: a missing design-constants file stops at composition", "design_constants")
dc_expr <- cmd_of(pipe_dc, "da_design_constants")
dc_env <- list2env(list(da_design_constants_file = design), parent = globalenv())
dc_env$environment_attestation <- dc_env$method_manifests <- TRUE
ok(identical(eval(dc_expr, dc_env)$quantity, "declared duration"),
   "f: the declared design table is read")
writeLines(c("quantity,value", "duration,6", "duration,8"), design)
expect_stop(eval(dc_expr, dc_env), "f: duplicate design quantities stop", "unique quantity")

# ==== (g) the method-manifest gate on the EDA numerics =====================
# Round-6 finding 2. Every EDA number now comes from
# skills/eda-standards/reference/eda.R, whose intended-use units are gated by
# skills/eda-standards/MANIFEST.yaml. While a unit is draft (or the manifest's
# reviewed_by is empty) its entry points must REFUSE to run, with the
# harness's standard "not SME-validated ... ESCALATE to the SME" stop. The
# refusals are asserted against a DRAFT FIXTURE (a tempdir mini-repository
# whose manifest carries the current digests, all units draft, reviewed_by
# empty), so they hold whether or not the SME has approved the real manifest.
source(file.path(TOOLS, "record_selection.R"))
source(file.path(TOOLS, "method_manifest.R"))

REAL_MF <- file.path(REPO, "skills", "eda-standards", "MANIFEST.yaml")
real_mf_sha_before <- digest::digest(file = REAL_MF, algo = "sha256")
mf <- read_method_manifest(REAL_MF)
expected_units <- c("record_selection", "summary_statistics", "log_axis_audit",
                    "display_transforms", "figure_construction",
                    "registry_validation")
ok(all(expected_units %in% names(mf$units)),
   "g: the eda-standards manifest declares one unit per intended use")
ok(all(vapply(mf$units, function(u) identical(u$sha256, .mm_file_sha256(
         file.path(REPO, u$reference_path))), logical(1))),
   "g: every unit's digest matches the reference file on disk")

toy <- data.frame(C = NA_character_, ID = c(1, 1, 2, 2), TIME = c(0, 1, 0, 1),
                  EVID = c(1, 0, 1, 0), DV = c(NA, 1.5, NA, 2.5),
                  BLQ = c(0, 0, 0, 0), WT = c(70, 70, 80, 80),
                  DOSE = c(300, 300, 320, 320), stringsAsFactors = FALSE)
gate_msg <- "not SME-validated"
DRAFT <- file.path(project, fx, "draft-fixture"); dir.create(DRAFT)
REFP <- file.path("skills", "eda-standards", "reference", "eda.R")
for (p in c(REFP, "tools/method_manifest.R", "tools/record_selection.R")) {
  dir.create(file.path(DRAFT, dirname(p)), recursive = TRUE, showWarnings = FALSE)
  file.copy(file.path(REPO, p), file.path(DRAFT, p), overwrite = TRUE)
}
draft_units <- lapply(mf$units, function(u)
  list(reference_path = u$reference_path, entrypoints = u$entrypoints,
       status = "draft", evidence = "TEST FIXTURE ONLY",
       reason = "test fixture: draft copy for exercising the refusals",
       depends = vapply(u$depends, function(d) d$path, character(1))))
od <- setwd(DRAFT)
quiet(write_method_manifest(file.path("skills", "eda-standards"), draft_units))
draft_mf <- read_method_manifest(file.path(DRAFT, "skills", "eda-standards", "MANIFEST.yaml"))
dr_env <- new.env(parent = globalenv())
sys.source(REFP, envir = dr_env, keep.source = TRUE)
setwd(od)
ok(identical(draft_mf$reviewed_by, "") &&
   all(vapply(draft_mf$units, function(u) identical(u$status, "draft"), logical(1))),
   "g: the DRAFT fixture manifest has empty reviewed_by and all units draft")
D <- function(nm) {
  f <- get(nm, envir = dr_env, mode = "function")
  function(...) { od <- setwd(DRAFT); on.exit(setwd(od)); f(...) }
}
expect_stop(quiet(D("reference_eda_view")(toy, "ID", "C", "EVID", 0, 1)),
            "g: record selection refuses while its unit is draft", gate_msg)
expect_stop(quiet(D("reference_eda_inventory")(list(rows = list()), "BLQ")),
            "g: summary statistics refuse while their unit is draft", gate_msg)
expect_stop(quiet(D("reference_eda_continuous_summary")(list(rows = list()), "WT")),
            "g: the covariate summary refuses while its unit is draft", gate_msg)
expect_stop(quiet(D("reference_eda_log_axis_audit")(list(rows = list()), "DV")),
            "g: the log-axis audit refuses while its unit is draft", gate_msg)
expect_stop(quiet(D("reference_eda_quantile_bins")(toy, "WT", c(0, 0.5, 1))),
            "g: display transforms refuse while their unit is draft", gate_msg)
expect_stop(quiet(D("reference_eda_registry_check")(list())),
            "g: registry validation refuses while its unit is draft", gate_msg)

# The task module and the analysis helper must be DECLARATIVE CALLERS: no
# record filter, no arithmetic and no hard-coded dataset column name.
mod_txt    <- readLines(file.path(REPO, "pipeline", "tasks", "eda.R"), warn = FALSE)
code_only <- function(txt) {
  txt <- txt[!grepl("^\\s*#", txt)]        # drop comment lines
  txt <- gsub('"[^"]*"', '""', txt)        # blank string literals (note prose)
  txt <- sub("#.*$", "", txt)              # drop trailing comments
  txt[nzchar(trimws(txt))]
}
dataset_cols <- c("C", "NUM", "ID", "TIME", "TAD", "EVID", "AMT", "DV", "MDV",
                "BLQ", "WT", "DOSE")
col_hits <- function(txt) {
  code <- code_only(txt)
  unlist(lapply(dataset_cols, function(cc)
    if (any(grepl(sprintf("\\b%s\\b", cc), code))) cc else NULL))
}
ok(length(col_hits(mod_txt)) == 0,
   "g: the task module names no dataset column (roles come from the spec)")
ok(!any(grepl("scale_[xy]_log10", code_only(mod_txt))),
   "g: the task module no longer detects log axes by deparsing plot functions")

# ==== (h) the numerics themselves, on a fixture ============================
# The numerics must not depend on the real manifest's status. To exercise
# them mechanically a MANIFEST FIXTURE is built under tempdir(): the reference,
# tools/method_manifest.R and tools/record_selection.R copied byte-for-byte,
# and a manifest whose units are `approved` with the CURRENT digests and
# reviewed_by "test-fixture". The reference resolves its paths against
# getwd(), so the fixture root is the working directory while its functions
# run. A validator stub would be inert (round 7, F4) and is not used. This
# changes nothing on disk in the repository; approving the real manifest
# remains the SME's act.
FIX <- file.path(project, fx, "manifest-fixture"); dir.create(FIX)
REFP <- file.path("skills", "eda-standards", "reference", "eda.R")
for (p in c(REFP, "tools/method_manifest.R", "tools/record_selection.R")) {
  dir.create(file.path(FIX, dirname(p)), recursive = TRUE, showWarnings = FALSE)
  file.copy(p, file.path(FIX, p), overwrite = TRUE)
}
fixture_units <- lapply(mf$units, function(u)
  list(reference_path = u$reference_path, entrypoints = u$entrypoints,
       status = "approved", evidence = "TEST FIXTURE ONLY",
       reason = "test fixture: approved copy for exercising the numerics",
       depends = vapply(u$depends, function(d) d$path, character(1))))
od <- setwd(FIX)
quiet(write_method_manifest(file.path("skills", "eda-standards"), fixture_units))
mf_lines <- readLines(file.path(FIX, "skills", "eda-standards", "MANIFEST.yaml"))
mf_lines <- sub('^reviewed_by: ""', 'reviewed_by: "test-fixture"', mf_lines)
mf_lines <- sub('^reviewed_at: ""', 'reviewed_at: "2026-09-03"', mf_lines)
writeLines(mf_lines, file.path(FIX, "skills", "eda-standards", "MANIFEST.yaml"))
fx_env <- new.env(parent = globalenv())
sys.source(REFP, envir = fx_env, keep.source = TRUE)
setwd(od)
R <- function(nm) {
  f <- get(nm, envir = fx_env, mode = "function")
  function(...) { od <- setwd(FIX); on.exit(setwd(od)); f(...) }
}

fx_data <- data.frame(
  C    = c(NA, NA, "outlier", NA, NA, NA, NA, NA),
  ID   = c(1, 1, 1, 1, 2, 2, 2, 2),
  TIME = c(0, 1, 2, 4, 0, 1, 2, 4),
  EVID = c(1, 0, 0, 0, 1, 0, 0, 0),
  DV   = c(NA, 0, 3.0, 1.0, NA, -1, 4.0, 2.0),
  BLQ  = c(0, 1, 0, 0, 0, 0, 0, 0),
  WT   = c(70, 70, 70, 70, 80, 80, 80, 80),
  DOSE = c(300, 300, 300, 300, 320, 320, 320, 320),
  stringsAsFactors = FALSE)

v <- R("reference_eda_view")(fx_data, id_col = "ID", exclusion_col = "C",
                             event_col = "EVID", observation_values = 0,
                             dose_values = 1)
ok(nrow(v$rows$analysis) == 7L && nrow(v$rows$observations) == 5L &&
   nrow(v$rows$subjects) == 2L,
   "h: the view selects 7 analysis records, 5 observations, 2 subjects")
ok(isTRUE(all.equal(as.numeric(v$input_counts$value), c(8, 8, 2))),
   "h: input counts report the dataset as read (8 rows, 8 columns, 2 subjects)")
# (on this fixture the doses and subjects views are the same records — the
# first kept record of each subject is its dose — so their digests coincide;
# the other four are distinct)
ok(all(c("analysis", "observations", "subjects", "doses", "excluded") %in% names(v$digests)) &&
   length(unique(v$digests[c("analysis", "observations", "subjects", "excluded")])) == 4L &&
   identical(unname(v$digests[["doses"]]), unname(v$digests[["subjects"]])),
   "h: each view carries its own row-set digest")
ok(identical(sort(v$rows$observations$EDA_ROW), c(2L, 4L, 6L, 7L, 8L)),
   "h: the stable row ids are the positions of the selected records")

ok(isTRUE(all.equal(as.numeric(v$n_rows[c("analysis", "observations",
                                          "subjects")]), c(7, 5, 2))),
   "h: the view returns its own per-view row counts (no caller uses nrow)")

# The kept-record rule (review finding H2): NA, empty, whitespace-only and the
# NONMEM missing token are all KEPT; anything else is an exclusion reason.
kept_fx <- data.frame(C = c(NA, "", " ", ".", "outlier", "\t"),
                      ID = 1:6, EVID = 0, DV = 1, stringsAsFactors = FALSE)
vk <- R("reference_eda_view")(kept_fx, "ID", "C", "EVID", 0, 1)
ok(identical(sort(vk$rows$analysis$ID), c(1L, 2L, 3L, 4L, 6L)),
   "h: NA, empty, whitespace-only and \".\" are kept; a reason is excluded")
ok(grepl("missing token", vk$selector$exclusion_rule, fixed = TRUE),
   "h: the selector states the rule verbatim, including the missing token")
# The same classifier tools/qc_battery.R and tools/nlmixr_runs.R call
# (tools/record_selection.R), so the three cannot drift apart.
qc_flagged <- !harness_kept_record(kept_fx$C)
ok(identical(sort(kept_fx$ID[!qc_flagged]), sort(vk$rows$analysis$ID)),
   "h: the reference and the harness kept-record classifier select the same records")
expect_stop(
  R("reference_eda_view")(data.frame(C = c("x", "y"), ID = 1:2, EVID = 0,
                                     DV = 1, stringsAsFactors = FALSE),
                          "ID", "C", "EVID", 0, 1),
  "h: a view in which every record is excluded stops instead of being empty",
  "every one of the 2 records is")

inv <- R("reference_eda_inventory")(v, blq_col = "BLQ", blq_values = 1)$table
ok(inv$n_subjects == 2 && inv$n_obs == 5 && inv$n_blq == 1 && inv$n_excluded == 1 &&
   inv$n_records_read == 8,
   "h: the inventory counts subjects, observations, BLQ, exclusions and records read")

cs <- R("reference_eda_continuous_summary")(v, columns = c("WT", "DOSE"),
                                            rows = "subjects", digits = 2)$table
ok(nrow(cs) == 2L && cs$n[1] == 2 && cs$mean[cs$variable == "WT"] == 75 &&
   cs$min[cs$variable == "DOSE"] == 300,
   "h: the covariate summary is computed one row per subject")

la <- R("reference_eda_log_axis_audit")(v, value_col = "DV")
val <- function(id) la$audit$value[la$audit$id == id]
ok(val("n_rows") == 5 && val("n_missing") == 0 && val("n_nonpositive") == 2 &&
   val("n_dropped_by_log") == 2,
   "h: the log-axis audit counts the rows a log axis would drop")
ok(identical(la$row_digest, unname(v$digests[["observations"]])),
   "h: the audit reports the digest of the very rows it audited")
ok(is.na(val("n_nonpositive_and_blq")),
   "h: with no BLQ column declared the cross-count is NA, not 0")
lb <- R("reference_eda_log_axis_audit")(v, value_col = "DV", blq_col = "BLQ",
                                        blq_values = 1)
valb <- function(id) lb$audit$value[lb$audit$id == id]
ok(valb("n_nonpositive") == 2 && valb("n_nonpositive_and_blq") == 1,
   "h: the BLQ cross-count reports how many non-positives are flagged BLQ")
expect_stop(R("reference_eda_log_axis_audit")(v, "DV", blq_col = "BLQ"),
            "h: a BLQ column without its declared values stops",
            "without `blq_values`")

# One audit per declared log axis (review finding M1), keyed by plot and axis.
at <- R("reference_eda_log_axis_audit_table")(
  v, list(list(plot = "p1", axis = "y", rows = "observations", column = "DV"),
          list(plot = "p2", axis = "x", rows = "analysis",     column = "TIME")),
  blq_col = "BLQ", blq_values = 1)
ok(nrow(at$table) == 2L && identical(at$table$axis, c("y", "x")) &&
   identical(at$table$rows, c("observations", "analysis")) &&
   at$n_requests == 2L,
   "h: the audit table has one row per declared log axis, on its own view")
ok(at$total_dropped == sum(at$table$n_dropped_by_log),
   "h: the table reports the total dropped, computed by the reference")
at0 <- R("reference_eda_log_axis_audit_table")(v, list())
ok(nrow(at0$table) == 0L && at0$total_dropped == 0 && at0$n_requests == 0L,
   "h: an analysis declaring no log axis gets an empty table, not a missing one")

bin_fx <- data.frame(WT = c(50, 60, 70, 80, 90, 100), stringsAsFactors = FALSE)
qb <- R("reference_eda_quantile_bins")(bin_fx, "WT", probs = c(0, 0.5, 1),
                                       labels = c("low", "high"))
ok(nrow(qb$breaks) == 3L && all(!is.na(qb$data$WT_BIN)) &&
   isTRUE(all.equal(qb$breaks$value, c(50, 75, 100))),
   "h: quantile binning returns its cut points and bins every row")
expect_stop(R("reference_eda_quantile_bins")(
              data.frame(WT = rep(70, 6)), "WT", probs = c(0, 1/3, 2/3, 1)),
            "h: indistinct quantiles stop rather than silently merging bins",
            "not distinct")
# breaks_from (review finding M3): the cut points may be taken from another
# frame; the DEFAULT is the rows being binned, so no reported number moves.
qb_def <- R("reference_eda_quantile_bins")(bin_fx, "WT", probs = c(0, 0.5, 1),
                                           labels = c("low", "high"))
qb_null <- R("reference_eda_quantile_bins")(bin_fx, "WT", probs = c(0, 0.5, 1),
                                            labels = c("low", "high"),
                                            breaks_from = NULL)
ok(identical(qb_def$breaks, qb_null$breaks) &&
   identical(qb_def$breaks_rows, "the rows being binned"),
   "h: breaks_from = NULL is the default and reproduces the previous behaviour")
qb_from <- R("reference_eda_quantile_bins")(
  bin_fx, "WT", probs = c(0, 0.5, 1), labels = c("low", "high"),
  breaks_from = data.frame(WT = c(50, 75, 100)))
ok(isTRUE(all.equal(qb_from$breaks$value, c(50, 75, 100))) &&
   qb_from$n_breaks_rows == 3L,
   "h: declared breaks_from takes the cut points from that frame and says so")
expect_stop(R("reference_eda_quantile_bins")(
              data.frame(WT = c(1, 2, 3, 100)), "WT", probs = c(0, 0.5, 1),
              labels = c("low", "high"),
              breaks_from = data.frame(WT = c(1, 2, 3))),
            "h: records outside the declared cut points stop",
            "fall outside the cut points")
expect_stop(R("reference_eda_quantile_bins")(
              data.frame(WT = c(1, 1, 1, 1, 9)), "WT",
              probs = c(0, 0.25, 0.5, 1),
              labels = c("a", "b", "c")),
            "h: a bin that would contain no record stops",
            "not distinct|would contain no record")

# L1: degenerate summaries stop instead of returning Inf.
v_empty <- v; v_empty$rows$subjects <- v$rows$subjects[0, , drop = FALSE]
expect_stop(R("reference_eda_continuous_summary")(v_empty, "WT"),
            "h: a summary over an empty view stops",
            "view is empty")
v_na <- v; v_na$rows$subjects$WT <- NA_real_
expect_stop(R("reference_eda_continuous_summary")(v_na, "WT"),
            "h: a covariate with no non-missing value stops",
            "no non-missing value")
nz <- R("reference_eda_normalize")(v$rows$observations, "DV", "DOSE",
                                   scale = 1000)
ok(isTRUE(all.equal(nz$data$DV_NORM[1], 0)) && nz$n_undefined == 0,
   "h: normalisation divides by the declared column and applies the scale")

# ==== (i) registry declaration and the row-set assertion ===================
good_reg <- list(
  a = list(tier = "core", file = "a.pdf", w = 5, h = 4, kind = "profiles",
           rows = "observations", x_transform = "identity", y_transform = "log10",
           x = "time", y = "dv", group = "id"),
  b = list(tier = "catalog", file = "b.pdf", w = 5, h = 4, kind = "histograms",
           rows = "subjects", x_transform = "identity", y_transform = "identity",
           variables = "covariate_continuous",
           bins = list(rule = "equal_width", n = 4L)))
rt <- R("reference_eda_registry_check")(good_reg)
ok(identical(rt$y_transform, c("log10", "identity")),
   "i: the registry table carries the DECLARED axis transform")
with_fn <- good_reg
with_fn$a$fn <- function(d, cols, lab, der) NULL
expect_stop(R("reference_eda_registry_check")(with_fn),
            "i: an entry carrying an R plotting function is refused (declarative registry)",
            "No arbitrary code")
bad_derive <- good_reg
bad_derive$a$derive <- list(bins = 8)
expect_stop(R("reference_eda_registry_check")(bad_derive),
            "i: a derive block with no kind is refused",
            "without a kind")
bad_rows <- good_reg
bad_rows$a$rows <- "whatever"
expect_stop(R("reference_eda_registry_check")(bad_rows),
            "i: an undeclared row selector is refused",
            "declares rows = 'whatever'")
dup <- good_reg; dup$b$file <- "a.pdf"
expect_stop(R("reference_eda_registry_check")(dup),
            "i: two entries writing the same file are refused",
            "share an output file name")
expect_stop(R("reference_eda_registry_active")(rt, "a"),
            "i: enabling a core entry as if it were catalog is refused",
            "not catalog tier")
expect_stop(R("reference_eda_registry_active")(rt, "nope"),
            "i: enabling an unknown catalog entry is refused",
            "not registry entries")

# Since review 2 (M1) the index compares the file on disk with the digest the
# reference returned when it wrote the figure, so entry `a` is built and
# written by the reference here.
built_a <- suppressMessages(suppressWarnings(R("reference_eda_build_figure")(
  v, good_reg$a, "a", list(id = "ID", time = "TIME", dv = "DV"),
  c(TIME = "time (h)", DV = "concentration"))))
out_i <- file.path(fx, "idx-out"); dir.create(out_i, showWarnings = FALSE)
written_a <- suppressMessages(suppressWarnings(R("reference_eda_write_figure")(
  built_a, good_reg$a, "a", file.path(out_i, "a.pdf"))))
rendered_ok <- data.frame(
  plot = "a", file = "a.pdf", path = written_a$path, rows = "observations",
  row_digest = unname(v$digests[["observations"]]), w = 5, h = 4,
  pages = as.integer(written_a$pages), sha256 = written_a$sha256,
  stringsAsFactors = FALSE)
idx <- R("reference_eda_registry_index")(rt, character(0), rendered_ok, v$digests)
ok(nrow(idx) == 2L && idx$rendered[idx$plot == "a"] &&
   !idx$rendered[idx$plot == "b"],
   "i: the index lists every entry with whether it rendered")

# The decisive check of round-6 finding 12: a figure drawn on a DIFFERENT row
# set of the SAME size must be refused, where the old nrow() tripwire passed.
other <- v$rows$observations
other$EDA_ROW <- rev(other$EDA_ROW) + 100L
rendered_bad <- rendered_ok
rendered_bad$row_digest <- R("reference_eda_rowset_digest")(v, other)
ok(nrow(other) == nrow(v$rows$observations),
   "i: the mismatched row set has the same number of rows")
expect_stop(R("reference_eda_registry_index")(rt, character(0), rendered_bad,
                                              v$digests),
            "i: a same-size but different row set is refused by the digest",
            "does not match the EDA view they declared")
rendered_stray <- rendered_ok; rendered_stray$plot <- "ghost"
expect_stop(R("reference_eda_registry_index")(rt, character(0), rendered_stray,
                                              v$digests),
            "i: a rendered figure with no registry entry is refused",
            "no plot-registry entry")

# ==== (i2) the row-set assertion on the BUILT PLOT (review finding H1) =====
# The old assertion digested the frame handed INTO the plotting function, so a
# function that subset its argument, or applied a transform the deparse scan
# did not match, passed. The assertion now reads the built ggplot.
#
# The ggplot2 internals this relies on are PINNED here against the installed
# version, so an upgrade that renames them fails loudly rather than silently
# disarming the check.
gp_ver <- as.character(utils::packageVersion("ggplot2"))
gp_probe <- suppressWarnings(
  ggplot2::ggplot(data.frame(x = 1:3, y = c(1, 10, 100)),
                  ggplot2::aes(x, y)) + ggplot2::geom_point())
gp_scaled <- gp_probe + ggplot2::scale_y_log10()
gp_sc <- gp_scaled$scales$get_scales("y")
gp_tr <- if (!is.null(gp_sc$transformation)) gp_sc$transformation else gp_sc$trans
ok(is.data.frame(gp_probe$data) && is.list(gp_probe$layers),
   sprintf("i2: ggplot2 %s exposes $data and $layers", gp_ver))
ok(is.null(gp_probe$scales$get_scales("y")) && !is.null(gp_sc) &&
   !is.null(gp_tr) && nzchar(gp_tr$name),
   sprintf("i2: ggplot2 %s exposes the y scale and its transformation name (%s)",
           gp_ver, if (is.null(gp_tr)) "<none>" else gp_tr$name))
ok(inherits(gp_probe$layers[[1]]$data, "waiver"),
   "i2: an inheriting layer carries waiver(), not a data frame")

plot_of <- function(f) suppressWarnings(f(v$rows$observations, NULL, NULL, NULL))
fn_clean <- function(d, cols, lab, der)
  ggplot2::ggplot(d, ggplot2::aes(TIME, DV)) + ggplot2::geom_point()
fn_filter <- function(d, cols, lab, der) {
  d <- d[d$DV > 1.5, ]
  ggplot2::ggplot(d, ggplot2::aes(TIME, DV)) + ggplot2::geom_point()
}
fn_reshape <- function(d, cols, lab, der) {
  long <- rbind(data.frame(EDA_ROW = d$EDA_ROW, v = d$DV),
                data.frame(EDA_ROW = d$EDA_ROW, v = d$WT))
  ggplot2::ggplot(long, ggplot2::aes(v)) + ggplot2::geom_histogram(bins = 3)
}
fn_trans <- function(d, cols, lab, der)
  ggplot2::ggplot(d, ggplot2::aes(TIME, DV)) + ggplot2::geom_point() +
    ggplot2::scale_y_continuous(breaks = c(1, 10), trans = "log10")
fn_coord <- function(d, cols, lab, der)
  ggplot2::ggplot(d, ggplot2::aes(TIME, DV)) + ggplot2::geom_point() +
    ggplot2::coord_trans(y = "log10")
fn_noid <- function(d, cols, lab, der)
  ggplot2::ggplot(d[, c("TIME", "DV")], ggplot2::aes(TIME, DV)) +
    ggplot2::geom_point()

ok(identical(R("reference_eda_plot_rowset_digest")(v, plot_of(fn_clean)),
             unname(v$digests[["observations"]])),
   "i2: a clean figure digests to its declared view")
ok(identical(R("reference_eda_plot_rowset_digest")(v, plot_of(fn_reshape)),
             unname(v$digests[["observations"]])),
   "i2: a reshaped figure (repeated row ids) still digests to the same row SET")
filt_dig <- R("reference_eda_plot_rowset_digest")(v, plot_of(fn_filter))
ok(!identical(filt_dig, unname(v$digests[["observations"]])),
   "i2: a figure that filtered its own data digests to something else")
rendered_filt <- rendered_ok; rendered_filt$row_digest <- filt_dig
expect_stop(R("reference_eda_registry_index")(rt, character(0), rendered_filt,
                                              v$digests),
            "i2: and the registry index refuses it",
            "does not match the EDA view they declared")
expect_stop(R("reference_eda_plot_rowset_digest")(v, plot_of(fn_trans)),
            "i2: a self-applied scale transform is refused structurally",
            "applied a 'log-10' transformation")
expect_stop(R("reference_eda_plot_rowset_digest")(v, plot_of(fn_coord)),
            "i2: a transforming coordinate system is refused",
            "transforming coordinate system")
expect_stop(R("reference_eda_plot_rowset_digest")(v, plot_of(fn_noid)),
            "i2: a plot frame without the row id cannot be traced and stops",
            "no column\\(s\\) EDA_ROW")
expect_stop(R("reference_eda_plot_rowset_digest")(v, v$rows$observations),
            "i2: handing it the data frame instead of the built plot stops",
            "must be a built figure")

# A composite figure (GGally ggmatrix) is no longer accepted anywhere: the
# pairs plot is built by the reference as a plain ggplot from computed
# correlations and bins (round 7, F1/F6; see tools/tests/test_eda_declarative.R).
expect_stop(R("reference_eda_plot_rowset_digest")(v, list(data = v$rows$subjects)),
            "i2: a non-ggplot composite object is refused",
            "must be a built figure")

# ==== (j) yspec semantics: roles, labels and decodes =======================
if (has_yspec) {
proj <- quiet(ys_projection(spec))
ok(identical(proj$columns, c("ID", "TIME", "DV")) &&
   identical(unname(proj$unit[["TIME"]]), "h"),
   "j: the projection carries the spec's columns and units")
ok(identical(ys_axis_label(proj, "TIME"), "time after first dose (h)"),
   "j: the axis label is built from the spec's short label and unit")

# The toy spec above carries NO SETUP__ flags, so role resolution must stop
# naming the role and the annotation the spec must gain — never fall back to
# a guessed column name.
msg_roles <- tryCatch({
  ys_resolve_roles(proj, list(id = list(flag = "eda_id", min = 1L, max = 1L)))
  "" }, error = conditionMessage)
ok(grepl("role 'id'", msg_roles) && grepl("eda_id", msg_roles) &&
   grepl("currently lists none", msg_roles),
   "j: an unannotated role stops, naming the role and the missing flag")

# The same spec WITH the flags resolves.
spec_flagged <- file.path(fx, "toy-spec-flagged.yml")
writeLines(append(readLines(spec),
                  c("  flags:", "    eda_id: [ID]", "    eda_time: [TIME]",
                    "    eda_dv: [DV]"),
                  after = grep("^  use_internal_db:", readLines(spec))),
           spec_flagged)
projf <- quiet(ys_projection(spec_flagged))
roles <- ys_resolve_roles(projf, list(id = list(flag = "eda_id"),
                                      time = list(flag = "eda_time"),
                                      dv = list(flag = "eda_dv")))
ok(identical(roles, list(id = "ID", time = "TIME", dv = "DV")),
   "j: annotated roles resolve to the columns the spec flags")
rtab <- ys_role_table(projf, roles)
ok(nrow(rtab) == 3L && all(c("role", "column", "short", "unit") %in% names(rtab)),
   "j: the resolved mapping renders as a table for the gate package")
expect_stop(ys_decode_values(projf, "DV", "observation"),
            "j: a value meaning with no values/decode annotation stops",
            "has no `values:` and no `decode:` annotation")
# The pilot case: a column that declares values but no decodes. The message
# must say which of the two is missing, not both.
spec_vals <- file.path(fx, "toy-spec-values.yml")
writeLines(c(readLines(spec_flagged), "BLQ:", "  short: below LLOQ flag",
             "  type: numeric", "  values: [0, 1]"), spec_vals)
projv <- quiet(ys_projection(spec_vals))
expect_stop(ys_decode_values(projv, "BLQ", "below LLOQ"),
            "j: a column with values but no decodes names the missing decode",
            "declares `values:` but has no `decode:` annotation")
} else {
  skip("yspec not installed; projection, label and role tests (j) not run")
}

# ==== (k) mutation tests: same-path byte changes invalidate the DAG ========
# Round-6 finding 3. A fixture store is built for eda_input only; the dataset
# and then the spec are altered IN PLACE, and tar_outdated() must list
# eda_input and eda_package each time. Use only the disposable fixture store.
if (has_yspec) {
mut_dir <- file.path(fx, "mutation"); dir.create(mut_dir, showWarnings = FALSE)
mut_fixture <- write_public_eda_inputs(mut_dir)
mut_csv <- mut_fixture$dataset
mut_spec <- mut_fixture$overlay
mut_store <- file.path(mut_dir, "store")
mut_an <- file.path(mut_dir, "MUT.R")
# The fixture root is disposable and has its own environment declaration.
writeLines(c("environment_confirmed: true", "r_provision: managed"), "harness.yaml")
writeLines(c(
  "library(targets)",
  "tar_source(\"tools\")",
  "source(\"pipeline/compose.R\")",
  "tar_option_set(format = \"rds\")",
  "compose_pipeline(analysis_id = \"TST-EDAMUT\", tasks = \"eda\",",
  "  inputs = list(eda = list(",
  sprintf("    external_input = %s,", shQuote(mut_csv, type = "cmd")),
  sprintf("    external_spec  = %s,", shQuote(mut_spec, type = "cmd")),
  "    catalog = character(0))))"), mut_an)

build_input <- function()
  quiet(targets::tar_make(names = tidyselect::all_of(c("eda_input", "eda_spec")),
                          script = mut_an, store = mut_store, reporter = "silent"))
# Since the round-7 environment attestation (pipeline/compose.R, ENV_TARGET
# always-cued and named in every stage command) tar_outdated() lists every
# stage target on every call, so it can no longer tell a same-path byte change
# from the attestation cue. The per-target reasons from tar_sitrep() can: a
# file target whose bytes changed reports `file`, and a consumer whose upstream
# changed reports `depend`.
sitrep <- function() {
  s <- quiet(targets::tar_sitrep(script = mut_an, store = mut_store))
  as.data.frame(s)
}
flag <- function(s, target, field) isTRUE(s[[field]][s$name == target])

built <- tryCatch({ build_input(); TRUE }, error = function(e) {
  cat(sprintf("      error: %s\n", conditionMessage(e))); FALSE })
ok(built, "k: the fixture store builds eda_input from the file targets")
s0 <- sitrep()
ok(!flag(s0, "eda_input", "file") && !flag(s0, "eda_input", "depend") &&
   !flag(s0, "eda_input", "command") &&
   !flag(s0, "eda_external_dataset_file", "file"),
   "k: eda_input and its file targets report no change immediately after the build")

# (k1) the DATASET's bytes change at the same path.
before <- digest::digest(file = mut_csv, algo = "sha256")
changed <- utils::read.csv(mut_csv, na.strings = ".")
changed$DV[2] <- changed$DV[2] + 1
utils::write.csv(changed, mut_csv, row.names = FALSE, na = ".")
ok(!identical(before, digest::digest(file = mut_csv, algo = "sha256")),
   "k1: the dataset bytes really changed (path unchanged)")
s1 <- sitrep()
ok(flag(s1, "eda_external_dataset_file", "file") && !flag(s1, "eda_external_spec_file", "file"),
   "k1: a same-path dataset change is a file change of the dataset file target (and of it alone)")
ok("eda_package" %in% quiet(targets::tar_outdated(script = mut_an, store = mut_store)),
   "k1: eda_package is outdated after the change")

build_input()
s1b <- sitrep()
ok(!flag(s1b, "eda_external_dataset_file", "file"),
   "k1: rebuilding clears it again")

# (k2) the SPEC's bytes change at the same path.
before_s <- digest::digest(file = mut_spec, algo = "sha256")
writeLines(c(readLines(mut_spec), "  # appended by the mutation test"), mut_spec)
ok(!identical(before_s, digest::digest(file = mut_spec, algo = "sha256")),
   "k2: the spec bytes really changed (path unchanged)")
s2 <- sitrep()
out2 <- quiet(targets::tar_outdated(script = mut_an, store = mut_store))
ok(flag(s2, "eda_external_spec_file", "file") && !flag(s2, "eda_external_dataset_file", "file") &&
   all(c("eda_input", "eda_spec", "eda_package") %in% out2),
   "k2: a same-path spec change is a file change of the spec file target; eda_input, eda_spec and eda_package are outdated")

# (k3) the cached hashes reach the object, and the package target names the
# file targets so the pre-packaging binding assertion has its inputs.
dmut <- quiet(read_external_dataset(mut_csv, mut_spec))
ok(identical(attr(dmut, "external_source_sha256"),
             digest::digest(file = mut_csv, algo = "sha256")) &&
   identical(attr(dmut, "external_spec_sha256"),
             digest::digest(file = mut_spec, algo = "sha256")),
   "k3: the data object carries both the dataset and the spec sha256")
mut_pipe <- compose_eda(mut_csv, mut_spec)
pkg_syms <- syms_of(mut_pipe, "eda_package")
ok(all(c("eda_external_dataset_file", "eda_external_spec_file", "eda_base_spec_file") %in% pkg_syms),
   "k3: eda_package depends on dataset, overlay and base file targets")
bind <- list(eda_input = targets::tar_read_raw("eda_input", store = mut_store),
             eda_spec = targets::tar_read_raw("eda_spec", store = mut_store),
             eda_external_dataset_file = mut_csv, eda_external_spec_file = mut_spec,
             eda_base_spec_file = mut_fixture$base)
bind[[ENV_TARGET]] <- targets::tar_read_raw(ENV_TARGET, store = mut_store)
bind[[MANIFEST_TARGET]] <- targets::tar_read_raw(MANIFEST_TARGET, store = mut_store)
expect_stop(eval(cmd_of(mut_pipe, "eda_package"), bind),
            "k3: eda_package refuses the stale cached specification before packaging",
            "cached EDA input does not match.*")
bind$eda_input <- dmut
bind$eda_spec <- quiet(ys_projection(mut_spec))
writeLines(c(readLines(mut_fixture$base), "# changed after composition"), mut_fixture$base)
expect_stop(eval(cmd_of(mut_pipe, "eda_package"), bind),
            "k3: eda_package refuses a stale base specification after composition",
            "cached EDA input does not match.*")
} else {
  skip("yspec not installed; mutation tests (k) not run")
}

# ==== (l) source identity and the reference path (review finding M4) =======
# The manifest hashes a constant path, which proves the FILE is the approved
# one but not that the functions RUNNING came from it. The reference now
# checks that too, and the task module no longer lets an analysis redirect
# the reference path.
mod_src <- readLines(file.path(REPO, "pipeline", "tasks", "eda.R"), warn = FALSE)
ok(!any(grepl("eda_reference\\s*<-\\s*ti\\$reference", mod_src)),
   "l: the task module no longer takes the reference path from the analysis")
expect_stop(
  compose_pipeline(analysis_id = "TST-EDASPEC", tasks = "eda",
                   inputs = list(eda = list(external_input = csv_ok,
                                            external_spec = spec,
                                            reference = "/tmp/mine.R"))),
  "l: declaring inputs$eda$reference is refused at composition",
  "is not accepted")

# A copy of the approved file, sourced into its own environment, must refuse
# to compute: the running definitions did not come from the manifest's path.
copy_path <- file.path(fx, "eda-copy.R")
file.copy(file.path(REPO, "skills", "eda-standards", "reference", "eda.R"),
          copy_path, overwrite = TRUE)
copy_env <- new.env(parent = globalenv())
sys.source(copy_path, envir = copy_env, keep.source = TRUE)
expect_stop(
  get("reference_eda_view", envir = copy_env)(toy, "ID", "C", "EVID", 0, 1),
  "l: a byte-identical copy sourced from elsewhere refuses to compute",
  "were sourced from")

# The structural fallback, used when source references are unavailable (the
# Rscript default): an ALTERED definition is caught even with no srcref.
alt_env <- new.env(parent = globalenv())
# Integrity batch 13: references reach a session only through the trusted
# loader (tools/load_reference.R), which authenticates the bytes it evaluates.
local({
  owd <- setwd(REPO); on.exit(setwd(owd), add = TRUE)
  sys.source(file.path(REPO, "tools", "load_reference.R"), envir = environment(), keep.source = FALSE)
  load_reference("skills/eda-standards", "skills/eda-standards/reference/eda.R", envir = alt_env)
})
# Integrity batch 14 (R13-P1-1): the identity closure no longer reads the
# reference file. The gate takes ONE byte snapshot, authenticates it against the
# manifest digest and hands it over, so these direct calls supply the same
# snapshot the gate would.
alt_snap <- local({
  owd <- setwd(REPO); on.exit(setwd(owd), add = TRUE)
  b <- readBin("skills/eda-standards/reference/eda.R", "raw",
               file.info("skills/eda-standards/reference/eda.R")$size)
  list(bytes = b, key = digest::digest(b, algo = "sha256", serialize = FALSE))
})
ok(isTRUE(tryCatch({ get(".eda_source_identity", envir = alt_env)(alt_snap$bytes, alt_snap$key); TRUE },
                   error = function(e) FALSE)),
   "l: unaltered definitions with no srcref pass the structural check")
assign(".eda_kept", function(x) rep(TRUE, length(x)), envir = alt_env)
expect_stop(get(".eda_source_identity", envir = alt_env)(alt_snap$bytes, alt_snap$key),
            "l: a tampered kept-record predicate is caught with no srcref",
            "do not match the definitions in")

# ==== (m) the two specifications for one dataset agree (finding M5) ========
if (has_yspec) {
overlay_fixture <- write_public_eda_inputs(file.path(fx, "overlay"))
base_spec <- overlay_fixture$base
eda_spec <- overlay_fixture$overlay
if (file.exists(base_spec) && file.exists(eda_spec)) {
  bs <- quiet(yspec::ys_load(base_spec))
  es <- quiet(yspec::ys_load(eda_spec))
  ok(identical(names(bs), names(es)),
     "m: both specifications declare the same columns in the same order")
  # The ONLY differences allowed are the enumerated additions: the SETUP__
  # flags block (which is metadata, not a column key) and a decode on the BLQ
  # column. Every other per-column key must be identical.
  allowed <- list(BLQ = "decode")
  drift <- character(0)
  for (cc in intersect(names(bs), names(es))) {
    b <- bs[[cc]]; e <- es[[cc]]
    # `col`, `dots`, `do_lookup` and `lookup` are yspec bookkeeping fields, not
    # column definitions; `allowed` names the enumerated additions.
    keys <- setdiff(union(names(b), names(e)),
                    c("col", "dots", "do_lookup", "lookup", allowed[[cc]]))
    for (k in keys)
      if (!identical(b[[k]], e[[k]]))
        drift <- c(drift, sprintf("%s$%s", cc, k))
  }
  ok(length(drift) == 0,
     sprintf("m: no per-column key drifted between the two specifications%s",
             if (length(drift)) paste0(" (", paste(drift, collapse = ", "), ")")
             else ""))
  ok(length(yspec::ys_flags(es)) > 0 && length(yspec::ys_flags(bs)) == 0,
     "m: the EDA specification adds the flags block the base one lacks")
  # SETUP__ keys (review 2 of eda-standards, L5): every key equal except the
  # two enumerated ones — `flags` (added) and `description` (the overlay
  # appends its own designation).
  # Compared on the DECLARED `SETUP__:` block of each file (yspec's get_meta
  # adds file-derived fields — spec_file, name, data_stem — that differ by
  # construction and are not declarations).
  mb <- yaml::read_yaml(base_spec)$SETUP__; me <- yaml::read_yaml(eda_spec)$SETUP__
  setup_keys <- setdiff(union(names(mb), names(me)), c("flags", "description"))
  setup_drift <- setup_keys[!vapply(setup_keys, function(k) identical(mb[[k]], me[[k]]), logical(1))]
  ok(length(setup_drift) == 0,
     sprintf("m: no SETUP__ key other than flags/description differs%s",
             if (length(setup_drift)) paste0(" (", paste(setup_drift, collapse = ", "), ")") else ""))
  ok(!identical(mb$description, me$description) &&
     grepl("overlay", as.character(me$description)),
     "m: the overlay's SETUP__ description names it an overlay (the one enumerated SETUP__ change)")
  ok(!is.null(es[["BLQ"]][["decode"]]) && is.null(bs[["BLQ"]][["decode"]]),
     "m: the EDA specification adds the BLQ decode the base one lacks")
} else {
  ok(FALSE, "EDA-SPEC: generated specification pair exists")
}
} else {
  skip("yspec not installed; specification-drift checks (m) not run")
}

ok(identical(digest::digest(file = REAL_MF, algo = "sha256"), real_mf_sha_before),
   "g: the REAL manifest's bytes are unchanged by this test run (sha256 equal)")

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0) quit(status = 1)
invisible(TRUE)
}

if (.invoked_as_script()) .run_eda_external_spec_tests()
