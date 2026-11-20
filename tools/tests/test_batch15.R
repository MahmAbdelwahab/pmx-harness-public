#!/usr/bin/env Rscript
# Integrity batch 15 battery (Codex round 14 findings, SME decisions 2026-09-06).
#
# Sections:
#   A  R14-P1-1  no dependency executes before its check: the gate compares
#                every declared dependency's digest BEFORE the identity check
#                evaluates anything, and the identity check skips the
#                dependency-loading constructions instead of running them.
#   B  R14-P1-2  policy data is part of method identity (D15-1): a top-level
#                non-function binding the reference owns cannot be rebound
#                after an honest load without the identity check refusing.
#   C  R14-P3-1  the inventory is compared with the definitions the
#                authenticated parse owns, not with a cardinality: a helper
#                added without inventorying it, and an inventory entry the file
#                no longer names, are both refusals.
#   D  queue O-8 the enclosing environment of every inventoried definition is
#                compared as well as its text, so a same-text lookalike built in
#                a hostile enclosure is refused and its fake never runs.
#   E  R14-P1-3  full approval validation runs on the snapshot the event is
#                built from, not on an independent re-read of the live path.
#   F  R14-P1-4  three more manifest-grammar refusals in BOTH readers, and the
#                raw dependency paths the two readers derive on the shipped
#                manifests are equal.
#   G  R14-P2-1  the CI reference-loading census folds ordinary constant path
#                constructors, and an unresolved source() target is a FAIL
#                (D15-2).
#
# Run with:  Rscript tools/tests/test_batch15.R
# Exits non-zero if any assertion fails.

# SAFE TO SOURCE: pipelines tar_source("tools") which recurses into
# tools/tests/; everything observable is guarded behind .invoked_as_script().
.invoked_as_script <- function() {
  fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  length(fa) > 0 && grepl("test_batch15\\.R$", sub("^--file=", "", fa[1]))
}

.run_batch15_tests <- function() {

args <- commandArgs(FALSE)
file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
TOOLS <- normalizePath(file.path(dirname(file_arg), ".."), mustWork = TRUE)
REPO  <- normalizePath(file.path(TOOLS, ".."), mustWork = TRUE)
source(file.path(TOOLS, "method_manifest.R"))

.pass <- 0L; .fail <- 0L
ok <- function(cond, name) {
  cond <- tryCatch(isTRUE(cond), error = function(e) {
    cat(sprintf("      error: %s\n", conditionMessage(e))); FALSE })
  if (cond) { .pass <<- .pass + 1L; cat(sprintf("PASS  %s\n", name)) }
  else { .fail <<- .fail + 1L; cat(sprintf("FAIL  %s\n", name)) }
}

old_wd <- getwd(); on.exit(setwd(old_wd), add = TRUE)

# --- the eda fixture --------------------------------------------------------
# A disposable copy of the repository-relative paths the eda reference is
# written for. Nothing here touches the live tree.
EDA_REF <- "skills/eda-standards/reference/eda.R"
eda_fixture <- function() {
  rt <- tempfile("b15eda"); dir.create(rt)
  dir.create(file.path(rt, "tools"))
  dir.create(file.path(rt, "skills", "eda-standards", "reference"), recursive = TRUE)
  for (f in c("method_manifest.R", "record_selection.R"))
    file.copy(file.path(REPO, "tools", f), file.path(rt, "tools", f), overwrite = TRUE)
  file.copy(file.path(REPO, EDA_REF), file.path(rt, EDA_REF), overwrite = TRUE)
  eda_reseal(rt, reviewed = TRUE)
  rt
}
eda_reseal <- function(rt, reviewed = FALSE) {
  p <- file.path(rt, "skills", "eda-standards", "MANIFEST.yaml")
  txt <- if (file.exists(p)) readLines(p, warn = FALSE)
         else readLines(file.path(REPO, "skills", "eda-standards", "MANIFEST.yaml"), warn = FALSE)
  old_ref <- sub("^\\s*sha256:\\s*", "", grep("^    sha256:", txt, value = TRUE)[1])
  old_dep <- sub("^\\s*sha256:\\s*", "", grep("^        sha256:", txt, value = TRUE)[1])
  txt <- gsub(old_ref, .mm_file_sha256(file.path(rt, EDA_REF)), txt, fixed = TRUE)
  txt <- gsub(old_dep, .mm_file_sha256(file.path(rt, "tools", "record_selection.R")),
              txt, fixed = TRUE)
  if (reviewed) {
    txt[grepl("^reviewed_by:", txt)] <- 'reviewed_by: "Fixture Reviewer"'
    txt[grepl("^reviewed_at:", txt)] <- 'reviewed_at: "2026-09-06"'
    txt[grepl("^    status:", txt)] <- "    status: approved"
  }
  writeLines(txt, p)
  invisible(TRUE)
}
VIEW <- list(selector_digest = "sha256:fixture", row_id_col = "ROWID")
ROWS <- data.frame(ROWID = 1:3)
BADROWS <- data.frame(OTHER = 1:3)

lr_env <- new.env(parent = globalenv())
sys.source(file.path(TOOLS, "load_reference.R"), envir = lr_env, keep.source = FALSE)
load_reference <- get("load_reference", envir = lr_env)

cat("\n--- A: R14-P1-1, no dependency executes before its check --------------------\n")

# The gate's dependency-drift comparison used to run AFTER the identity check,
# whose throwaway evaluation of the authenticated bytes re-ran the reference's
# `sys.source()` construction. An edited dependency's top level therefore
# executed during the very call that was about to refuse it.
rt <- eda_fixture(); setwd(rt)
Ea <- new.env(parent = baseenv())
load_reference("skills/eda-standards", EDA_REF, envir = Ea)
cat('base::file.create("MARKER_DEP")\n', file = file.path(rt, "tools", "record_selection.R"),
    append = TRUE)
ra <- tryCatch(Ea$reference_eda_rowset_digest(VIEW, ROWS), error = function(e) conditionMessage(e))
ok(is.character(ra) && grepl("changed on disk after the reference loaded it", ra, fixed = TRUE),
   "A: a dependency edited after the honest load is REFUSED on the next gated call")
ok(!file.exists(file.path(rt, "MARKER_DEP")),
   "A: the edited dependency's top level never ran (no marker)")

# Static: the identity closure skips, rather than evaluates, the constructions
# that load code from disk.
eda_src <- readLines(file.path(REPO, EDA_REF), warn = FALSE)
srcid <- eda_src[grep("^\\.eda_source_identity <- base::local", eda_src):
                 (grep("^\\.eda_srcid_env <- base::local", eda_src)[1] - 1L)]
srcid_code <- srcid[!grepl("^\\s*#", srcid)]
ok(any(grepl("if (loads_from_disk(e)) {", srcid_code, fixed = TRUE)) &&
     any(grepl("base::assign(nm, stub_env, envir = disk_env)", srcid_code, fixed = TRUE)),
   "A: the throwaway evaluation stubs a dependency-loading construction instead of running it")
gate_blk <- eda_src[grep("^\\.eda_gate <- base::local", eda_src):length(eda_src)]
gate_blk <- gate_blk[seq_len(grep("^\\}, envir", gate_blk)[1])]
gate_code <- gate_blk[!grepl("^\\s*#", gate_blk)]
i_dep <- grep('base::get("dependency_sha256", envir = deps)', gate_code, fixed = TRUE)[1]
i_srcid <- grep('base::get("srcid", envir = srcid_env)(disk_bytes, disk_key)',
                gate_code, fixed = TRUE)[1]
ok(!is.na(i_dep) && !is.na(i_srcid) && i_dep < i_srcid,
   "A: the dependency-drift comparison is written BEFORE the identity check in the gate")

setwd(old_wd)

cat("\n--- B: R14-P1-2 / D15-1, policy data is part of method identity -------------\n")

# EDA_CORE_TABLES is the required core-table set. It is ordinary mutable data,
# and the identity digest used to cover bindings of mode "function" only, so
# rebinding it narrowed what an approved method requires without changing one
# function object.
b_tables <- data.frame(deliverable = "data inventory", status = "computed",
                       reason = NA_character_, stringsAsFactors = FALSE)
b_figures <- data.frame(plot = "conc-time", tier = "core", status = "rendered",
                        reason = "", stringsAsFactors = FALSE)
rt <- eda_fixture(); setwd(rt)
Eb <- new.env(parent = baseenv())
load_reference("skills/eda-standards", EDA_REF, envir = Eb)
rb0 <- tryCatch(Eb$reference_eda_core_status(b_figures, b_tables),
                error = function(e) conditionMessage(e))
ok(is.character(rb0) && grepl("core table(s) neither computed nor", rb0, fixed = TRUE),
   "B/positive: with the approved constant, the four omitted core tables are named and refused")
assign("EDA_CORE_TABLES", "data inventory", envir = Eb)
rb <- tryCatch(Eb$reference_eda_core_status(b_figures, b_tables),
               error = function(e) conditionMessage(e))
ok(is.character(rb) && grepl("do not match the definitions in", rb, fixed = TRUE),
   "B: a rebound EDA_CORE_TABLES is an identity mismatch, not a narrowed core-table set")

# Every registry policy vector gets the same treatment, because the compared set
# is derived from the authenticated parse rather than from a list in this test.
for (nm in c("EDA_REGISTRY_FIELDS", "EDA_REGISTRY_TIERS", "EDA_REGISTRY_ROWS",
             "EDA_REGISTRY_TRANSFORMS", "EDA_FIGURE_KINDS", "EDA_DERIVE_KINDS",
             "EDA_CORRELATION_METHODS")) {
  setwd(old_wd); rt <- eda_fixture(); setwd(rt)
  Eb2 <- new.env(parent = baseenv())
  load_reference("skills/eda-standards", EDA_REF, envir = Eb2)
  assign(nm, "tampered", envir = Eb2)
  r <- tryCatch(Eb2$reference_eda_rowset_digest(VIEW, ROWS),
                error = function(e) conditionMessage(e))
  ok(is.character(r) && grepl("do not match the definitions in", r, fixed = TRUE),
     sprintf("B: a rebound %s is an identity mismatch", nm))
}
setwd(old_wd)

cat("\n--- C: R14-P3-1, the inventory names exactly what the file owns -------------\n")

c_case <- function(label, mutate) {
  setwd(old_wd); rt <- eda_fixture(); setwd(rt)
  writeLines(mutate(readLines(EDA_REF, warn = FALSE)), EDA_REF)
  eda_reseal(rt)
  E <- new.env(parent = baseenv())
  r <- tryCatch({ load_reference("skills/eda-standards", EDA_REF, envir = E)
                  E$reference_eda_rowset_digest(VIEW, ROWS) },
                error = function(e) conditionMessage(e))
  ok(is.character(r) &&
       grepl("does not name exactly the definitions", r, fixed = TRUE),
     sprintf("C: %s is REFUSED", label))
  setwd(old_wd)
}
c_case("a helper added to the file but not to the inventory",
       function(ln) c(ln, ".eda_extra_helper <- function(x) x"))
c_case("an inventory entry removed while the file still defines it",
       function(ln) sub('^(\\s*)"\\.eda_bins", "\\.eda_check_view",$', '\\1".eda_check_view",', ln))

cat("\n--- D: queue O-8, enclosing environments are compared as well as text -------\n")

# A helper rebuilt from its own deparsed text in an environment that binds one
# of its free names to a fake has identical TEXT and a different ENCLOSURE.
# This is the reference inspecting its own closures against its own
# authenticated bytes -- not a defence against a hostile caller's reflection,
# which stays out of scope (D15-4).
rt <- eda_fixture(); setwd(rt)
Ed <- new.env(parent = baseenv())
load_reference("skills/eda-standards", EDA_REF, envir = Ed)
d_txt <- paste(deparse(Ed$.eda_require_cols), collapse = "\n")
d_hostile <- new.env(parent = Ed)
assign("setdiff", function(...) { file.create("HIT_LOOKALIKE"); character(0) },
       envir = d_hostile)
assign(".eda_require_cols", eval(parse(text = d_txt), envir = d_hostile), envir = Ed)
ok(identical(paste(deparse(Ed$.eda_require_cols), collapse = "\n"), d_txt),
   "D/setup: the lookalike's deparsed text is identical to the approved helper's")
rd <- tryCatch(Ed$reference_eda_rowset_digest(VIEW, BADROWS),
               error = function(e) conditionMessage(e))
ok(is.character(rd) && grepl("do not match the definitions in", rd, fixed = TRUE),
   "D: a same-text helper in a hostile enclosure is REFUSED")
ok(!file.exists(file.path(rt, "HIT_LOOKALIKE")),
   "D: the hostile `setdiff` never ran (no marker)")
setwd(old_wd)

cat("\n--- E: R14-P1-3, full validation runs on the snapshot -----------------------\n")

ag <- new.env(parent = globalenv())
setwd(REPO)
suppressWarnings(suppressMessages({
  sys.source(file.path(TOOLS, "gate_package.R"), envir = ag, keep.source = FALSE)
  sys.source(file.path(TOOLS, "gate.R"), envir = ag, keep.source = FALSE)
  sys.source(file.path(TOOLS, "approve_gate.R"), envir = ag, keep.source = FALSE)
}))
ok(all(c("lines", "bytes") %in% names(formals(ag$require_gate))),
   "E: require_gate() takes the snapshot's lines and bytes")
e_probe <- tempfile(fileext = ".md"); writeLines("# Gate Sign-off Record", e_probe)
r <- tryCatch(ag$require_gate(e_probe, lines = "a"), error = function(e) conditionMessage(e))
ok(is.character(r) && grepl("`lines` AND `bytes` from the same read", r, fixed = TRUE),
   "E: lines without bytes is REFUSED (one read, or none)")
ok(any(grepl("lines = snap$lines, bytes = snap$bytes", deparse(ag$approve_gate), fixed = TRUE)),
   "E: approve_gate() hands the snapshot to the full validation under the lock")

gw <- tempfile("b15gate"); dir.create(file.path(gw, "gates", "TST-015"), recursive = TRUE)
setwd(gw)
writeLines("hello", "a.csv")
e_arts <- data.frame(artifact = "t1", path = "a.csv", source = "ta", stringsAsFactors = FALSE)
e_tabs <- list("Verdicts" = data.frame(criterion = "E1", value = 1.23, stringsAsFactors = FALSE))
grec <- file.path("gates", "TST-015", "eda.md")
writeLines(c("# Gate Sign-off Record", "", "Analysis ID: TST-015", "Stage: eda",
             "Date opened: 2026-09-06", "", "## SME decision", "",
             "- [ ] APPROVED — stage complete, next stage may start",
             "- [ ] APPROVED WITH CHANGES — changes listed below were applied and re-verified",
             "- [ ] REJECTED — returned to stage with instructions below", "",
             "Signed: <!-- SME name -->", "Date: <!-- -->", "Digest signed: <!-- paste -->"), grec)
ag$render_gate_package(grec, e_arts, e_tabs, c("a note"))
e_rendered <- readLines(grec)
gsign <- function(lines, digest = NULL) {
  dg <- sub("^Package-Digest: ", "", grep("^Package-Digest:", lines, value = TRUE)[1])
  i <- grep("^- \\[ \\] APPROVED —", lines)[1]
  lines[i] <- sub("\\[ \\]", "[x]", lines[i])
  lines[grep("^Digest signed:", lines)] <- paste("Digest signed:",
                                                 if (is.null(digest)) dg else digest)
  lines[grep("^Signed:", lines)] <- "Signed: Fixture Reviewer"
  lines[grep("^Date:", lines)] <- "Date: 2026-09-06"
  lines
}
# A: a valid signed record. B: approving, signed and dated, but its
# `Digest signed:` does not bind the package -- a record require_gate() refuses.
A <- gsign(e_rendered)
B <- gsign(e_rendered, digest = "sha256:0000000000000000")
gledger <- ag$.ledger_path(grec)
e_n <- function() if (file.exists(gledger)) length(readLines(gledger, warn = FALSE)) else 0L

writeLines(A, grec)
r <- tryCatch(suppressMessages(ag$require_gate(
        grec, enforce_ledger = FALSE, lines = B,
        bytes = charToRaw(paste0(paste(B, collapse = "\n"), "\n")))),
      error = function(e) conditionMessage(e))
ok(is.character(r) && grepl("Gate signature does not cover this package", r, fixed = TRUE),
   "E: with a valid record on the path, validation of a defective SNAPSHOT is refused")

# End to end: the record is A while the live-path validations run and B for the
# digest, the snapshot and the final drift assertion -- the editor save/undo the
# finding describes.
writeLines(A, grec); e_n0 <- e_n()
r2 <- tryCatch(suppressMessages(ag$approve_gate(
        grec, check_store = FALSE,
        .after_live_validation = function() writeLines(B, grec),
        .after_snapshot_read   = function() writeLines(A, grec),
        .after_snapshot        = function() writeLines(B, grec))),
      error = function(e) conditionMessage(e))
if (!is.character(r2)) cat(sprintf("      got: %s\n", paste(class(r2), collapse = " ")))
ok(is.character(r2) && grepl("Gate signature does not cover this package", r2, fixed = TRUE),
   "E: an approval whose snapshot never passed full validation is REFUSED")
ok(e_n() == e_n0, "E: the refused approval appended no ledger event")
ok(!dir.exists(ag$.release_lock_path(gledger)), "E: the lock is released after the refusal")

# Positive control: an unchanged valid record is still approved.
writeLines(A, grec)
ev <- tryCatch(suppressMessages(ag$approve_gate(grec, check_store = FALSE)),
               error = function(e) conditionMessage(e))
ok(is.list(ev) && identical(ev$signer, "Fixture Reviewer"),
   "E/positive: an unchanged record is approved from its snapshot")
# Revoke keeps its ability to revoke a defective record.
ok(!any(grepl("require_gate(", deparse(ag$revoke_gate), fixed = TRUE)),
   "E: revoke_gate() acquires no approval requirement (it must be able to revoke a defective record)")
setwd(old_wd)

cat("\n--- F: R14-P1-4, three more grammar refusals in both readers ----------------\n")

f_base <- readLines(file.path(REPO, "skills", "eda-standards", "MANIFEST.yaml"), warn = FALSE)
f_py <- function(lines) {
  tf <- tempfile(fileext = ".yaml"); writeLines(lines, tf)
  out <- suppressWarnings(system2("python3", c("-c", shQuote(paste0(
    "import sys; sys.path.insert(0, ", shQuote(file.path(REPO, ".claude", "hooks")), ");",
    "import protect_paths as pp;",
    "print(pp._manifest_grammar_problem(open(", shQuote(tf), ").read()))")))
    , stdout = TRUE, stderr = TRUE))
  paste(out, collapse = " ")
}
f_r <- function(lines) {
  tf <- tempfile(fileext = ".yaml"); writeLines(lines, tf)
  tryCatch({ read_method_manifest(tf); "" }, error = function(e) conditionMessage(e))
}
f_dep  <- grep("^      - path:", f_base)[1]
f_unit <- grep("^  [^ ].*:$", f_base)[1]
f_ep   <- grep("^    entrypoints:", f_base)[1]
f_cases <- list(
  "an inline # in a dependency path" = { v <- f_base; v[f_dep] <- "      - path: tools/site # helper.R"; v },
  "a unit key indented three spaces" = { v <- f_base; v[f_unit] <- paste0(" ", v[f_unit]); v },
  "a YAML alias inside the entrypoints list" = { v <- f_base
      v[f_ep] <- sub("[", "[*anchor, ", v[f_ep], fixed = TRUE); v })
for (nm in names(f_cases)) {
  ok(!identical(f_py(f_cases[[nm]]), "None"),
     sprintf("F: the write guard refuses %s", nm))
  ok(nzchar(f_r(f_cases[[nm]])),
     sprintf("F: tools/method_manifest.R refuses %s", nm))
}
ok(identical(f_py(f_base), "None") && !nzchar(f_r(f_base)),
   "F/positive: the shipped eda-standards manifest is accepted by both readers unchanged")

# The RAW dependency path scalars the two readers derive, before any
# normalization, on every shipped manifest.
f_manifests <- sort(Sys.glob(file.path(REPO, "skills", "*", "MANIFEST.yaml")))
f_r_paths <- sort(unique(unlist(lapply(f_manifests, function(m)
  unlist(lapply(read_method_manifest(m)$units, function(u)
    vapply(u$depends, function(d) d$path, character(1))))))))
f_py_out <- suppressWarnings(system2("python3", c("-c", shQuote(paste0(
  "import sys, glob, yaml; sys.path.insert(0, ", shQuote(file.path(REPO, ".claude", "hooks")), ");",
  "import protect_paths as pp; out=[];",
  "\nfor m in sorted(glob.glob(", shQuote(file.path(REPO, "skills", "*", "MANIFEST.yaml")), ")):",
  "\n    doc, prob = pp._manifest_document(open(m).read(), yaml)",
  "\n    assert prob is None, (m, prob)",
  "\n    for u in doc['units'].values():",
  "\n        for d in (u.get('depends') or []): out.append(d['path'])",
  "\nprint('\\n'.join(sorted(set(out))))"))), stdout = TRUE, stderr = TRUE))
f_py_paths <- sort(unique(f_py_out[nzchar(f_py_out)]))
if (!identical(f_r_paths, f_py_paths))
  cat(sprintf("      got: R=%s | py=%s\n", paste(f_r_paths, collapse = ","),
              paste(f_py_paths, collapse = ",")))
ok(length(f_r_paths) > 0L && identical(f_r_paths, f_py_paths),
   sprintf("F: both readers derive the same %d raw dependency path(s) from the shipped manifests",
           length(f_r_paths)))

cat("\n--- G: R14-P2-1 / D15-2, the CI source census ------------------------------\n")

# Isolate the census and generate only the paths needed by the control and probe.
g_rt <- tempfile("b15ci"); dir.create(g_rt)
dir.create(file.path(g_rt, "pipeline", "tasks"), recursive = TRUE)
dir.create(file.path(g_rt, "skills", "demo", "reference"), recursive = TRUE)
dir.create(file.path(g_rt, "pilot"))
writeLines("reference_demo <- function(x) x", file.path(g_rt, "skills", "demo", "reference", "method.R"))
writeLines('source("skills/demo/reference/method.R")', file.path(g_rt, "pilot", "fixture_forward.R"))
writeLines("evaluate <- function(x) reference_demo(x)", file.path(g_rt, "pipeline", "tasks", "fixture_task.R"))
writeLines(c('source(paste0("skills/demo/reference/", "method.R"))',
             'source(base::file.path("skills", "demo", "reference", "method.R"))',
             'q <- Sys.getenv("X")',
             'source(q)'),
           file.path(g_rt, "pipeline", "tasks", "b15_probe.R"))
g_ci <- new.env(parent = globalenv())
sys.source(file.path(TOOLS, "ci_check.R"), envir = g_ci)
setwd(g_rt)
g_out <- capture.output(g_ci$check_reference_loading())
setwd(old_wd)
g_ref <- grep("reference loading", g_out, value = TRUE)
ok(any(grepl("b15_probe.R:1 sources a literal reference path", g_ref, fixed = TRUE)),
   "G: paste0() of string literals naming a reference FAILs")
ok(any(grepl("b15_probe.R:2 sources a literal reference path", g_ref, fixed = TRUE)),
   "G: base::file.path() of string literals naming a reference FAILs")
ok(any(grepl("b15_probe.R:4 unresolved source target", g_ref, fixed = TRUE)),
   "G: a source() target that cannot be resolved FAILs as unresolved (D15-2)")
ok(any(grepl("pilot forwarder", g_ref, fixed = TRUE)) &&
     any(grepl("fixture_task.R.*fixture_forward.R", g_ref)),
   "B15-FORWARD: census names the local task and its unauthenticated forwarder")
ok(!any(grepl("^FAIL reference loading", g_ref) &
          !grepl("b15_probe.R", g_ref)),
   "G: no file other than the probe produces a reference-loading FAIL")
unlink(g_rt, recursive = TRUE)

cat(sprintf("\nintegrity batch 15 battery: %d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
invisible(TRUE)
}

if (.invoked_as_script()) .run_batch15_tests()
