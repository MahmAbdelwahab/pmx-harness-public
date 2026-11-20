#!/usr/bin/env Rscript
# Tests for tools/report_semantic.R — the mechanical semantic check on the
# RENDERED report.
#
# Why this battery exists (independent review round 6, finding 10): the gate
# digest proves WHICH bytes were signed, not that those bytes communicate the
# intended content. On 2026-09-02 an SME signed a Model Analysis Report whose
# appendices carried eleven deparsed `list(kind = ...)` blobs where archived
# review notes should have been. note_text() (tools/evidence_notes.R) fixed the
# serializer; nothing checked the PDF. This battery covers the check that does.
#
# Three groups:
#   A  expected_report_notes() — the canonical expected-text manifest, built
#      through the ONE serializer (note_text()) and through the report's own
#      declared render-time text transform when it has one.
#   B  the pure comparison core (check_semantics_text) against synthetic
#      extracted text: pass, missing note, reordered notes, structural
#      leakage, wrapped lines, whitespace runs, duplicated note.
#   C  end-to-end: a tiny Quarto document rendered with quarto + tectonic,
#      then extracted with pdftotext. Four cases — correct render passes; one
#      note removed fails; a `list(kind = ` blob fails; reordering fails.
#      Group C needs quarto, tectonic and pdftotext; each missing binary
#      prints a visible SKIP line and the skipped cases are counted as SKIPPED,
#      never as passes.
#
# Base R only (plus the repository's own tools). Run with
#   Rscript tools/tests/test_report_semantic.R
# Add --pure-only for groups A/B and the synthetic locale cases only. Groups
# C/D are excluded, never counted as passes. Full-stack CI cannot select it.
# Exits non-zero if any assertion fails.
#
# SAFE TO SOURCE: guarded behind .invoked_as_script() like the other batteries
# under tools/tests/, so tar_source("tools") defines and runs nothing.
.invoked_as_script <- function() {
  fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  length(fa) > 0 && grepl("test_report_semantic\\.R$", sub("^--file=", "", fa[1]))
}

## This function body is embedded in the engineering battery, not sourced there.
.run_locale_cases <- function(ok) {
  check <- function(name, expr) {
    value <- tryCatch(isTRUE(force(expr)), error = function(e) FALSE)
    ok(value, name)
  }
  raw_string <- function(prefix, bytes, suffix = "", label = "unknown") {
    x <- rawToChar(c(charToRaw(prefix), as.raw(bytes), charToRaw(suffix)))
    Encoding(x) <- label
    x
  }
  manifest <- function(note, heading = "Synthetic appendix") {
    sections <- setNames(list(list(notes = list(authored_note(note)))), heading)
    expected_report_notes(sections)
  }
  passes <- function(text, expected) {
    result <- check_semantics_text(text, expected, source = "<synthetic-locale>")
    inherits(result, "report_semantic_result") && isTRUE(result$pass)
  }
  refuses <- function(expr) {
    e <- tryCatch({ force(expr); NULL }, error = function(e) e)
    inherits(e, "error") && grepl("semantic text encoding", conditionMessage(e), fixed = TRUE)
  }
  stable <- "Synthetic stable content remains fully visible."
  expected <- manifest(stable)
  doc <- raw_string("Synthetic appendix\nSynthetic stable", c(0xc2, 0xa0),
                    "content remains fully visible.")
  check("LOC-DOCUMENT-UNKNOWN", passes(doc, expected))
  unknown_note <- raw_string("Synthetic stable", c(0xc2, 0xa0),
                             "content remains fully visible.")
  check("LOC-EXPECTED-UNKNOWN", {
    e <- manifest(unknown_note)
    identical(e$segments$text, "content remains fully visible.") &&
      passes(paste("Synthetic appendix", stable, sep = "\n"), e)
  })
  heading <- raw_string("Synthetic", c(0xc2, 0xa0), "appendix")
  check("LOC-HEADINGS", passes(paste("Synthetic appendix", stable, sep = "\n"),
                               manifest(stable, heading)))
  check("LOC-SPACE", identical(.rs_flatten(unknown_note), stable) &&
        identical(.rs_flatten_document(unknown_note), stable) &&
        identical(.rs_flatten(" alpha\r\n\tbeta "), "alpha beta") &&
        identical(.rs_flatten_document("alpha-\r\nbeta"), "alphabeta") &&
        identical(.rs_flatten_document("alpha\nbeta"), paste0("alpha", .RS_BREAK, "beta")))

  # Identical valid UTF-8 bytes, different declared meaning. Do not infer UTF-8
  # before honoring a declared Latin-1 label.
  unknown <- raw_string("alpha", c(0xc2, 0xa0), "beta")
  latin <- unknown
  Encoding(latin) <- "latin1"
  originals <- serialize(list(unknown, latin), NULL)
  check("LOC-ENCODINGS-LABEL-SETUP", Encoding(unknown) == "unknown" &&
        Encoding(latin) == "latin1" && identical(charToRaw(unknown), charToRaw(latin)) &&
        validUTF8(unknown))
  check("LOC-ENCODINGS", {
    u <- .rs_prepare_text(unknown)
    l <- .rs_prepare_text(latin)
    identical(utf8ToInt(u), c(97L, 108L, 112L, 104L, 97L, 160L, 98L, 101L, 116L, 97L)) &&
      identical(utf8ToInt(l), c(97L, 108L, 112L, 104L, 97L, 194L, 160L, 98L, 101L, 116L, 97L)) &&
      validUTF8(u) && validUTF8(l) &&
      identical(charToRaw(iconv(l, from = "UTF-8", to = "latin1", sub = NA)), charToRaw(latin)) &&
      identical(serialize(list(unknown, latin), NULL), originals)
  })
  check("LOC-ENCODINGS-PUBLIC", {
    unknown_long <- raw_string("Synthetic stable", c(0xc2, 0xa0), "content remains fully visible.")
    latin_long <- unknown_long
    Encoding(latin_long) <- "latin1"
    u <- manifest(unknown_long)
    l <- manifest(latin_long)
    identical(utf8ToInt(u$notes$shown), utf8ToInt("Synthetic stable\u00a0content remains fully visible.")) &&
      identical(utf8ToInt(l$notes$shown), utf8ToInt("Synthetic stable\u00c2\u00a0content remains fully visible.")) &&
      passes(paste0("Synthetic appendix\nSynthetic stable\u00a0content remains fully visible."), u) &&
      passes(paste0("Synthetic appendix\nSynthetic stable\u00c2\u00a0content remains fully visible."), l)
  })
  marked <- unknown_note
  Encoding(marked) <- "UTF-8"
  accented <- raw_string("accent", 0xe9, " prose", "latin1")
  check("LOC-ENCODINGS-MIXED", {
    mixed <- c(ascii = "ASCII", marked = marked, unknown = unknown_note, latin = accented)
    before <- serialize(mixed, NULL)
    out <- .rs_prepare_text(mixed)
    identical(names(out), names(mixed)) && all(validUTF8(out)) &&
      identical(utf8ToInt(out[[4L]]), utf8ToInt("accent\u00e9 prose")) &&
      identical(serialize(mixed, NULL), before)
  })
  check("LOC-ENCODINGS-PROVENANCE", {
    note <- authored_note(unknown_note)
    before <- serialize(note, NULL)
    calls <- 0L
    seen <- NULL
    transform <- function(x) {
      calls <<- calls + 1L
      seen <<- serialize(x, NULL)
      x
    }
    e <- expected_report_notes(list("Synthetic appendix" =
      list(notes = note, transform = transform)))
    identical(serialize(note, NULL), before) && calls == 1L &&
      identical(seen, serialize(unknown_note, NULL)) &&
      identical(charToRaw(e$notes$text), charToRaw(unknown_note)) &&
      identical(Encoding(e$notes$text), Encoding(unknown_note)) &&
      passes(paste("Synthetic appendix", stable, sep = "\n"), e)
  })
  invalid <- list(c(0x80), c(0xc2), c(0xc0, 0xaf), c(0xed, 0xa0, 0x80),
                  c(0xf4, 0x90, 0x80, 0x80), c(0xff))
  for (i in seq_along(invalid)) {
    bad <- raw_string("Synthetic stable", invalid[[i]], "content remains fully visible.", "UTF-8")
    check(paste0("LOC-INVALID-UTF8-", i), refuses(.rs_flatten_document(bad)))
    check(paste0("LOC-INVALID-EXPECTED-", i), refuses(manifest(bad)))
  }
  byte_text <- unknown_note
  Encoding(byte_text) <- "bytes"
  check("LOC-INVALID-BYTES-SETUP", identical(Encoding(byte_text), "bytes"))
  check("LOC-INVALID", identical(Encoding(byte_text), "bytes") &&
        refuses(.rs_flatten_document(byte_text)) && refuses(manifest(byte_text)))
  # Deleting invalid bytes between pieces of a stable word must never yield a
  # passing comparison. This additionally discriminates a deletion converter.
  poisoned <- raw_string("Synthetic appendix\nSynthetic sta", 0xff,
                         "ble content remains fully visible.", "UTF-8")
  check("LOC-NEGATIVE-INVALID-DELETION", refuses(check_semantics_text(poisoned, expected)))
  if (identical(Sys.getlocale("LC_CTYPE"), "C")) {
    bad_native <- raw_string("alpha", 0xff, "beta")
    check("LOC-INVALID-NATIVE-C", refuses(.rs_flatten(bad_native)))
  }
  for (label in c("unknown", "UTF-8", "latin1")) {
    base <- raw_string("Synthetic stable", if (label == "latin1") 0xa0 else c(0xc2, 0xa0),
                       "content remains fully visible.", label)
    e <- manifest(base)
    check(paste0("LOC-NEGATIVE-", label), {
      changed <- "Synthetic appendix\nSynthetic stable content remains completely hidden."
      duplicate <- paste("Synthetic appendix", stable, stable, sep = "\n")
      leaked <- paste("Synthetic appendix", stable, "list(kind = unexplained)", sep = "\n")
      !passes(changed, e) && !passes(duplicate, e) && !passes(leaked, e)
    })
  }
  e_order <- expected_report_notes(list(
    "Synthetic appendix" = list(notes = list(authored_note(stable),
      authored_note("Another independent synthetic statement remains present."))),
    "Other appendix" = list(notes = list(authored_note("Separate synthetic statement stays in its section.")))))
  check("LOC-NEGATIVE-ORDER-SECTION", {
    reordered <- paste("Synthetic appendix", "Another independent synthetic statement remains present.",
                       stable, "Other appendix", "Separate synthetic statement stays in its section.", sep = "\n")
    moved <- paste("Synthetic appendix", "Other appendix", stable,
                   "Another independent synthetic statement remains present.",
                   "Separate synthetic statement stays in its section.", sep = "\n")
    !passes(reordered, e_order) && !passes(moved, e_order)
  })
  check("LOC-EDGES", {
    x <- matrix(c(NA_character_, unknown_note), nrow = 1L,
                dimnames = list("row", c("missing", "text")))
    before <- serialize(x, NULL)
    out <- .rs_prepare_text(x)
    identical(dim(out), dim(x)) && identical(dimnames(out), dimnames(x)) &&
      is.na(out[[1L]]) && identical(serialize(x, NULL), before) &&
      identical(.rs_prepare_text(character()), character()) &&
      identical(.rs_flatten(character()), "") && identical(.rs_flatten_document(character()), "") &&
      identical(.rs_flatten(NA_character_), "NA") && identical(.rs_flatten_document(NA_character_), "NA")
  })
  restore_fixture <- function(error = FALSE) {
    old_cwd <- getwd()
    old_ctype <- Sys.getlocale("LC_CTYPE")
    old_env <- Sys.getenv("PMX_LOCALE_SYNTHETIC", unset = NA_character_)
    on.exit({
      setwd(old_cwd)
      Sys.setlocale("LC_CTYPE", old_ctype)
      if (is.na(old_env)) Sys.unsetenv("PMX_LOCALE_SYNTHETIC") else
        Sys.setenv(PMX_LOCALE_SYNTHETIC = old_env)
    }, add = TRUE)
    setwd(tempdir())
    stopifnot(nzchar(Sys.setlocale("LC_CTYPE", "C")))
    Sys.setenv(PMX_LOCALE_SYNTHETIC = "synthetic")
    if (error) .rs_flatten(poisoned) else .rs_flatten(unknown_note)
  }
  old <- list(getwd(), Sys.getlocale("LC_CTYPE"),
              Sys.getenv("PMX_LOCALE_SYNTHETIC", unset = NA_character_))
  check("LOC-RESTORE-SUCCESS", {
    restore_fixture()
    identical(old, list(getwd(), Sys.getlocale("LC_CTYPE"),
                         Sys.getenv("PMX_LOCALE_SYNTHETIC", unset = NA_character_)))
  })
  check("LOC-RESTORE-ERROR", {
    caught <- refuses(restore_fixture(TRUE))
    caught && identical(old, list(getwd(), Sys.getlocale("LC_CTYPE"),
                                  Sys.getenv("PMX_LOCALE_SYNTHETIC", unset = NA_character_)))
  })
}

# Exercise the shipped task, templates, and package from a declared project.
.declared_report_fixture <- function(repo, evaluate) {
  root <- tempfile("declared-report-")
  dir.create(root)
  old <- setwd(root)
  on.exit(setwd(old), add = TRUE)
  file.create(".here")
  sources <- c("pipeline/compose.R", "pipeline/tasks/reporting.R", "tools/evidence_notes.R",
               "tools/gate_package.R", "tools/report_semantic.R", "tools/report_typography.R",
               "tools/review_stage.R")
  for (path in sources) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    stopifnot(file.copy(file.path(repo, path), path))
  }
  dir.create("project-report")
  paths <- list.files(file.path(repo, "templates/report"), recursive = TRUE)
  for (path in paths) {
    dest <- file.path("project-report", path)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    stopifnot(file.copy(file.path(repo, "templates/report", path), dest))
  }
  notes <- c("A synthetic review recorded the provenance of the assembled inputs.",
             "An independent synthetic review confirmed the declared input columns.")
  report_dir <- "project-report"
  section <- file.path(report_dir, "sections/08-appendices.qmd")
  original <- readLines(section)
  stopifnot(sum(grepl("note_text(notes)", original, fixed = TRUE)) == 1L)

  # The analyst declares and tracks the multi-page source and embeds page two.
  # Distinct text in each source page makes a wrong page observable in the MAR.
  figure <- file.path(report_dir, "declared-figure.pdf")
  page_one <- "FIXTUREALPHAPAGE"
  page_two <- "FIXTUREBETAPAGE"
  grDevices::pdf(figure, width = 4, height = 2, useDingbats = FALSE)
  for (marker in c(page_one, page_two)) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, marker)
  }
  grDevices::dev.off()
  results <- file.path(report_dir, "sections/05-results.qmd")
  cat("\n\\includegraphics[page=2,width=0.7\\linewidth]{declared-figure.pdf}\n",
      file = results, append = TRUE)
  helper <- file.path(report_dir, "fixture-helper.R")
  writeLines("fixture_report_marker <- 'first bytes'", helper)
  declaration <- list(report_dir = report_dir, store = "_targets_report_fixture",
    document = c(author = "Alex Rowan", date = "2026-11-20", version = "1"),
    packages = "data_assembly", helpers = basename(c(helper, figure)))
  saveRDS(declaration, "declaration.rds")
  saveRDS(notes, "archived-notes.rds")
  writeLines(c(
    "library(targets)", "tar_source('tools')", "source('pipeline/compose.R')",
    "task <- new.env(parent = globalenv())",
    "task$task_input <- readRDS('declaration.rds')",
    "task$analysis_id <- 'REPORT-FIXTURE'",
    "task$upstream_tasks <- 'data_assembly'",
    "task$task_targets <- NULL",
    "sys.source('pipeline/tasks/reporting.R', task)",
    "c(list(tar_target(data_assembly_package,",
    "  list(notes = lapply(readRDS('archived-notes.rds'), authored_note)))), task$task_targets)"),
    "report-fixture.R")
  store <- declaration$store
  published <- file.path(store, "objects", "reporting_package")
  pdf <- file.path(report_dir, "mar.pdf")
  pptx <- file.path(report_dir, "deck.pptx")
  manifest <- file.path(report_dir, "mar-semantic-manifest.txt")
  build <- function(store_path = store) targets::tar_make(script = "report-fixture.R",
    store = store_path, names = tidyselect::all_of("reporting_package"), reporter = "silent")
  outdated <- function() targets::tar_outdated(script = "report-fixture.R", store = store)
  result <- tryCatch({ build(); NULL }, error = conditionMessage)
  evaluate("R11 neutral reporting renders and passes semantics", {
    if (!is.null(result)) stop(result, call. = FALSE)
    stopifnot(file.exists(published), file.exists(pdf), file.exists(pptx),
              any(readLines(manifest) == "# result: PASS"))
    package <- targets::tar_read(reporting_package, store = store)
    stopifnot(identical(package$analysis, "REPORT-FIXTURE"),
              identical(package$artifacts$path[1:2], c(pdf, pptx)),
              all(c(helper, section, figure, manifest) %in% package$artifacts$path),
              !any(c("report_mar", "report_deck") %in% outdated()))
    writeLines("fixture_report_marker <- 'changed bytes'", helper)
    stopifnot(all(c("report_mar", "report_deck", "reporting_package") %in% outdated()))
    build()
    stopifnot(any(readLines(manifest) == "# result: PASS"))
    TRUE
  })
  evaluate("R13 the second page alone appears in the rendered report", {
    if (!is.null(result)) stop(result, call. = FALSE)
    txt <- system2(Sys.which("pdftotext"), c("-layout", shQuote(pdf), "-"), stdout = TRUE)
    is.null(attr(txt, "status")) && any(grepl(page_two, txt, fixed = TRUE)) &&
      !any(grepl(page_one, txt, fixed = TRUE))
  })
  evaluate("R12 a dropped archived note blocks package creation", {
    if (!is.null(result)) stop(result, call. = FALSE)
    writeLines(sub("note_text(notes)", "note_text(notes[-1L])", original, fixed = TRUE), section)
    stopifnot(all(c("report_mar", "reporting_package") %in% outdated()))
    affected <- "_targets_report_missing_note"
    declaration$store <- affected
    saveRDS(declaration, "declaration.rds")
    failure <- tryCatch({ build(affected); NULL }, error = conditionMessage)
    metadata <- targets::tar_meta(store = affected, fields = c("name", "error"))
    stopifnot(!is.null(failure), identical(metadata$name[!is.na(metadata$error)], "report_semantic_check"),
              any(grepl("MISSING", readLines(manifest), fixed = TRUE)),
              !file.exists(file.path(affected, "objects", "reporting_package")))
    TRUE
  })
}

.run_report_semantic_tests <- function() {

.pure_only <- "--pure-only" %in% commandArgs(trailingOnly = TRUE)
if (.pure_only && nzchar(Sys.getenv("PMX_CI_NO_SKIP", "")))
  stop("--pure-only cannot be combined with PMX_CI_NO_SKIP", call. = FALSE)

args <- commandArgs(FALSE)
file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
TOOLS <- normalizePath(file.path(dirname(file_arg), ".."), mustWork = TRUE)
REPO  <- normalizePath(file.path(TOOLS, ".."), mustWork = TRUE)
source(file.path(TOOLS, "evidence_notes.R"))
# .md_render_col(), used by evidence_note()
source(file.path(TOOLS, "gate_package.R"))
source(file.path(TOOLS, "report_semantic.R"))

.pass <- 0L; .fail <- 0L; .skip <- 0L
ok <- function(cond, name) {
  cond <- tryCatch(isTRUE(cond), error = function(e) {
    cat(sprintf("      error: %s\n", conditionMessage(e))); FALSE })
  if (cond) { .pass <<- .pass + 1L; cat(sprintf("PASS  %s\n", name)) }
  else { .fail <<- .fail + 1L; cat(sprintf("FAIL  %s\n", name)) }
}
# PMX_CI_NO_SKIP=1 (the validated-stack CI job; Codex round 7, finding 14):
# a skip is a FAILURE. On a machine that claims the full stack, a missing
# dependency must not turn into a green run.
.no_skip <- nzchar(Sys.getenv("PMX_CI_NO_SKIP", ""))
skip <- function(name, why) {
  if (.no_skip) {
    .fail <<- .fail + 1L
    cat(sprintf("FAIL  %s  (would skip: %s; PMX_CI_NO_SKIP is set, so a skip is a failure)\n",
                name, why))
    return(invisible(FALSE))
  }
  .skip <<- .skip + 1L
  cat(sprintf("SKIP  %s  (%s)\n", name, why))
}
expect_stop <- function(expr, name, regexp = NULL) {
  e <- tryCatch({ force(expr); NULL }, error = function(e) e)
  hit <- !is.null(e) && (is.null(regexp) || grepl(regexp, conditionMessage(e)))
  if (!is.null(e) && !hit) cat(sprintf("      got: %s\n", conditionMessage(e)))
  ok(hit, name)
}

# Fixture workspace. Set REPORT_SEMANTIC_KEEP=<dir> to render the fixtures
# into a directory of your choosing and keep them afterwards, so a failure can
# be diagnosed against the actual PDF and its `pdftotext -layout` output.
keep <- Sys.getenv("REPORT_SEMANTIC_KEEP", "")
tmp <- if (nzchar(keep)) keep else
  file.path(tempdir(), paste0("report-semantic-", Sys.getpid()))
dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
if (!nzchar(keep)) on.exit(unlink(tmp, recursive = TRUE), add = TRUE) else
  cat("      fixtures kept in ", tmp, "\n", sep = "")

# --- the fixture notes -------------------------------------------------------
# Section 1 reproduces archived notes as written. Section 2 applies the
# declared typography and symbol transforms. The generated notes exercise
# punctuation, forced line breaks and characters that TeX can consume.
n_subjects <- 500L
seed_value <- 20260828L

verbatim_notes <- list(
  authored_note(paste(
    "Applicability: dose and duration are inside the fitted design; route and",
    "formulation match the fitted study (single oral dose), so no population",
    "extrapolation arises -- the simulated population is the fitted",
    "population itself.")),
  authored_note(paste(
    "Acceptance criteria, stated as recorded: convergence | covariance step |",
    "relative standard error at or below 30 per cent; the helper",
    "`read_external_dataset` reads the archived table, and the value shown as",
    "100% is a coverage fraction, not a percentage change.")),
  # The forced line break lives in a PLAIN legacy note string, not in a typed
  # note, and deliberately so. A plain string survives both note_text() and the
  # defect coercion below unchanged, so the blob fixture still renders and the
  # semantic check is actually exercised on it. Put the same break inside a
  # typed note and the defect deparses it to a literal backslash-n, which
  # pandoc passes through as raw TeX and the document fails to build - a
  # louder failure that stops the pipeline earlier, but one that tests the
  # renderer rather than the semantic check. Use both note representations.
  "A plain legacy note string, reproduced exactly as archived.  \nIts continuation after a forced line break, still one recorded note.",
  evidence_note("Subjects simulated per scenario", n_subjects),
  evidence_note("Random seed recorded for the scenario", seed_value))

transformed_notes <- list(
  authored_note(paste(
    "Diagnostic settings were read from the archived record rather than",
    "typed: the replicate count and the seed are quoted from the stored",
    "settings block (qual_vpc$settings$n and qual_vpc$settings$seed), so two",
    "dollar signs occur on this single recorded line.")),
  authored_note(paste(
    "Weight scaling was fixed at the planned allometric exponent, so CL_i",
    "and V_i follow (WT/WT_ref)^0.75 with n_id subjects and n_obs",
    "observations contributing to the fit.")),
  evidence_note("Subjects contributing to the archived fit", n_subjects))

symbol_map_file <- file.path(REPO, "tools", "report_typography.R")
source(symbol_map_file)
have_symbol_map <- TRUE
report_transform <- protect_dollars

make_expected <- function() {
  sections <- list()
  sections[["Appendix E.1 archived record (verbatim)"]] <-
    list(notes = verbatim_notes)
  if (have_symbol_map)
    sections[["Appendix E.2 archived record (typographically normalized)"]] <-
      list(notes = transformed_notes, transform = report_transform)
  expected_report_notes(sections)
}

# ============================================================================
# A. expected_report_notes(): the canonical expected-text manifest
# ============================================================================
cat("\n-- A. expected-text manifest -------------------------------------\n")

expected <- make_expected()
ok(inherits(expected, "report_expected_notes"),
   "A1: expected_report_notes() returns a typed manifest object")
vb_text <- note_text(verbatim_notes)
ok(identical(expected$notes$text[seq_along(vb_text)], vb_text),
   "A2: expected note text comes from note_text(), unchanged and in order")
ok(nrow(expected$notes) ==
     length(note_text(verbatim_notes)) +
     if (have_symbol_map) length(note_text(transformed_notes)) else 0L,
   "A3: one manifest row per note, every section")
ok(all(expected$segments$nchar > 0) && nrow(expected$segments) >= nrow(expected$notes),
   "A4: every note contributes at least one comparison segment")
ok(!any(grepl("[$]", expected$segments$text[expected$segments$section ==
                 "Appendix E.1 archived record (verbatim)"])),
   "A5: verbatim section carries no dollar sign (none in its source notes)")
if (have_symbol_map) {
  tr <- expected$notes$shown[expected$notes$section ==
          "Appendix E.2 archived record (typographically normalized)"]
  ok(any(grepl("\\$", tr, fixed = TRUE)),
     "A6: declared transform is applied to the expected text (dollars escaped)")
  seg2 <- expected$segments$text[expected$segments$section ==
            "Appendix E.2 archived record (typographically normalized)"]
  ok(any(grepl("$", seg2, fixed = TRUE)),
     "A7: literal dollar signs survive into the comparison segments")
  ok(!any(grepl("mathrm|allowbreak|\\\\theta", seg2)),
     "A8: LaTeX break markup the transform introduced is stripped, never compared as text")
} else {
  skip("A6-A8: declared-transform manifest", "symbol-map.R not found")
}
expect_stop(expected_report_notes(list(x = list(notes = list(42)))),
            "A9: an unsupported note item is an error, never a deparsed blob",
            "unsupported note item")
expect_stop(expected_report_notes(list(list(notes = verbatim_notes))),
            "A10: sections must be named", "NAMED list")

# ============================================================================
# B. the comparison core, against synthetic extracted text
# ============================================================================
cat("\n-- B. comparison core ---------------------------------------------\n")

# A synthetic "extraction": the notes as the report shows them, wrapped at a
# narrow width and indented like a blockquote, which is what pdftotext -layout
# produces for the MAR appendices.
shown_lines <- function(exp) exp$notes$shown
lay_out <- function(strings, width = 46) {
  paste(vapply(strings, function(s) {
    s <- gsub("[[:space:]]+", " ", s)
    paste0("    ", paste(strwrap(s, width = width), collapse = "\n    "))
  }, character(1)), collapse = "\n\n")
}
# Markup the transform introduced is typeset by the engine, not printed; the
# synthetic extraction therefore shows the STRIPPED form, which is exactly what
# expected_report_notes() records as `plain`.
# Matching is bound to sections (round 7, finding 13): every declared section
# name must appear as a heading line in the document, and a note is looked for
# only inside its own section. The synthetic documents therefore carry one
# heading line per section, followed by that section's notes.
with_sections <- function(plain, sections,
                          preamble = "Model Analysis Report\n\nAppendix E. Archived records\n\n",
                          headings = unique(sections)) {
  out <- preamble
  for (sn in headings) {
    body <- plain[sections == sn]
    out <- paste0(out, sn, "\n\n",
                  if (length(body)) paste0(lay_out(body), "\n\n") else "")
  }
  out
}
good_text <- paste0(with_sections(expected$notes$plain, expected$notes$section),
                    "End of report\n")

res <- check_semantics_text(good_text, expected, source = "<synthetic>")
ok(inherits(res, "report_semantic_result"), "B1: typed result returned")
ok(isTRUE(res$pass), "B2: correct extraction passes")
ok(res$n_notes == nrow(expected$notes) && res$n_found == res$n_notes,
   "B3: every expected note is found")
ok(is.null(res$first_failure), "B4: no first failure recorded on a pass")

drop_one <- 2L
missing_text <- with_sections(expected$notes$plain[-drop_one],
                              expected$notes$section[-drop_one],
                              headings = unique(expected$notes$section))
res_missing <- check_semantics_text(missing_text, expected, source = "<synthetic>")
ok(!isTRUE(res_missing$pass), "B5: a removed note fails the check")
ok(identical(res_missing$first_failure$status, "MISSING"),
   "B6: the failure is reported as MISSING")
ok(grepl(substr(expected$notes$text[drop_one], 1, 30),
         res_missing$message, fixed = TRUE),
   "B7: the failure message quotes the missing note")

ord <- seq_len(nrow(expected$notes)); ord[1:2] <- ord[2:1]
reordered_text <- with_sections(expected$notes$plain[ord],
                                expected$notes$section[ord],
                                headings = unique(expected$notes$section))
res_order <- check_semantics_text(reordered_text, expected, source = "<synthetic>")
ok(!isTRUE(res_order$pass), "B8: reordered notes fail the check")
ok(identical(res_order$first_failure$status, "OUT_OF_ORDER"),
   "B9: the failure is reported as OUT_OF_ORDER")

blob <- paste0("list(kind = \"evidence\", label = \"Subjects simulated per ",
               "scenario\", expression_text = \"n_subjects\", ",
               "value_rendered = \"500\")")
leak_text <- with_sections(c(expected$notes$plain[-5], blob),
                           c(expected$notes$section[-5], expected$notes$section[5]),
                           headings = unique(expected$notes$section))
res_leak <- check_semantics_text(leak_text, expected, source = "<synthetic>")
ok(!isTRUE(res_leak$pass), "B10: a deparsed list blob fails the check")
ok(any(res_leak$tripwires$accounted == FALSE),
   "B11: the structural-leakage tripwire fires on list(kind =")

dup_text <- with_sections(c(expected$notes$plain, expected$notes$plain[1]),
                          c(expected$notes$section, expected$notes$section[1]),
                          headings = unique(expected$notes$section))
res_dup <- check_semantics_text(dup_text, expected, source = "<synthetic>")
ok(!isTRUE(res_dup$pass), "B12: a note appearing twice fails the check")
ok(identical(res_dup$first_failure$status, "REPEATED"),
   "B13: the failure is reported as REPEATED")

# Line wrapping and whitespace runs are the ONLY normalization.
squashed <- gsub("\n", "\n        \t ", good_text)
ok(isTRUE(check_semantics_text(squashed, expected, source = "<s>")$pass),
   "B14: whitespace runs and indentation do not affect the result")
hyphenated <- sub("population itself", "popula-\n    tion itself", good_text,
                  fixed = TRUE)
ok(isTRUE(check_semantics_text(hyphenated, expected, source = "<s>")$pass),
   "B15: a word hyphenated across a line break still matches")

# A legitimate `c("` inside a reproduced expression is accounted for, not a
# tripwire failure when the archived expression contains that character sequence.
verdict_dispositions <- c("SME-VISUAL", "DISPOSED", "PASS", "FAIL")
acct <- expected_report_notes(list(
  "Reproduced expression" = list(notes = list(evidence_note(
    "Verdict rows carrying a computed comparison, disposition filtered",
    sum(!verdict_dispositions %in% c("SME-VISUAL", "DISPOSED")))))))
acct_text <- with_sections(acct$notes$plain, acct$notes$section, preamble = "Report\n\n")
res_acct <- check_semantics_text(acct_text, acct, source = "<s>")
ok(isTRUE(res_acct$pass),
   "B16: a reproduced expression containing c(\" does not fail the check")
ok(nrow(res_acct$tripwires) > 0 && all(res_acct$tripwires$accounted),
   "B17: that tripwire hit is recorded as accounted for by an expected note")

# Distinct notes can end with the same sentence. Count occurrences against
# the manifest's own use of each segment.
# The dash makes the shared tail a segment of its own, and the longest one, so
# it is the anchor of both notes and its count is what gets enforced.
shared_tail <- "reference implementation validation status, checked at run time: VALIDATED (signed record)"
shared <- expected_report_notes(list("Shared endings" = list(notes = list(
  authored_note(paste("The exploratory analysis code was checked -",
                      shared_tail)),
  authored_note(paste("The simulation engine wrapper was checked -",
                      shared_tail))))))
ok(sum(shared$segments$anchor & shared$segments$text == shared_tail) == 2L,
   "B18a: the shared tail is the anchor of both notes, expected twice")
shared_ok <- with_sections(shared$notes$plain, shared$notes$section, preamble = "Report\n\n")
ok(isTRUE(check_semantics_text(shared_ok, shared, source = "<s>")$pass),
   "B18: a segment two notes share is expected twice, and twice passes")
shared_bad <- with_sections(shared$notes$plain[1], shared$notes$section[1], preamble = "Report\n\n")
res_shared <- check_semantics_text(shared_bad, shared, source = "<s>")
ok(!isTRUE(res_shared$pass),
   "B19: dropping one of the two notes that share a segment still fails")

# Incidental phrases can appear both in an archived note and elsewhere.
incidental <- "on the prespecified comparison scale"
inc <- expected_report_notes(list("Incidental repetition" = list(notes = list(
  authored_note(paste0("Bootstrap medians were required to agree with the ",
                       "point estimates of the selected model - ", incidental,
                       " - and the criterion was met for every parameter."))))))
inc_text <- with_sections(inc$notes$plain, inc$notes$section,
                          preamble = paste0("The plan states ", incidental,
                   " for this comparison.\n\nThe appendix reproduces the plan: ",
                   incidental, ".\n\n"))
ok(isTRUE(check_semantics_text(inc_text, inc, source = "<s>")$pass),
   "B20: a non-anchor phrase the report repeats elsewhere is not a failure")
dup_anchor <- with_sections(rep(inc$notes$plain, 2), rep(inc$notes$section, 2),
                            preamble = "")
ok(!isTRUE(check_semantics_text(dup_anchor, inc, source = "<s>")$pass),
   "B21: the whole note appearing twice still fails (its anchor doubles)")

# A page number can appear inside a sentence split across two pages.
paged <- paste0("Model Analysis Report\n\n", expected$notes$section[1], "\n\n",
                substr(expected$notes$plain[1], 1, 60), "\n\n   39\n\f\n",
                substr(expected$notes$plain[1], 61,
                       nchar(expected$notes$plain[1])), "\n\n",
                with_sections(expected$notes$plain[-1], expected$notes$section[-1],
                              preamble = "",
                              headings = unique(expected$notes$section[-1])))
ok(isTRUE(check_semantics_text(paged, expected, source = "<s>")$pass),
   "B22: a page number between two lines of one note does not break the match")

# The tripwire looks for an R call, not for an English word before a bracket.
prose <- expected_report_notes(list("Prose" = list(notes = list(
  authored_note(paste("Covariate structure (pre-declared): body weight on",
                      "clearance and volume as the frozen prior list sets out,",
                      "with no further structure (none was added)."))))))
res_prose <- check_semantics_text(with_sections(prose$notes$plain, prose$notes$section,
                                                preamble = ""), prose,
                                  source = "<s>")
ok(isTRUE(res_prose$pass),
   "B23: the English word structure before a bracket is not a tripwire hit")
ok(sum(!res_prose$tripwires$accounted) == 0L,
   "B24: and no unaccounted tripwire is recorded for it")

# Section binding (round 7, finding 13). Matching used to run over the whole
# document, so a note that had migrated into the wrong appendix - or into the
# narrative - still counted as found. A note must now be found inside its own
# section's text, and every declared section heading must be located as a line
# of its own in the document.
ok(grepl("anchor segments exactly once and in order; supporting segments present",
         res$message, fixed = TRUE),
   "B25: the PASS message states the actual multiplicity rule, not \"exactly once\"")
if (have_symbol_map) {
  sec <- expected$notes$section
  swapped <- sec; swapped[sec == unique(sec)[1]][1] <- unique(sec)[2]
  # the first E.1 note is printed under the E.2 heading instead
  wrong_sec <- with_sections(expected$notes$plain[order(match(swapped, unique(sec)))],
                             sort(match(swapped, unique(sec))) |> (\(i) unique(sec)[i])(),
                             headings = unique(sec))
  res_ws <- check_semantics_text(wrong_sec, expected, source = "<s>")
  ok(!isTRUE(res_ws$pass) && identical(res_ws$first_failure$status, "OUT_OF_SECTION"),
     "B26: a note printed under another section's heading fails as OUT_OF_SECTION")
} else skip("B26: wrong-section note", "symbol-map.R not found")
no_heading <- sub(paste0(unique(expected$notes$section)[1], "\n\n"), "", good_text, fixed = TRUE)
res_nh <- check_semantics_text(no_heading, expected, source = "<s>")
ok(!isTRUE(res_nh$pass) && identical(res_nh$first_failure$status, "SECTION_NOT_FOUND") &&
     grepl(unique(expected$notes$section)[1], res_nh$message, fixed = TRUE),
   "B27: a section heading that cannot be located fails, quoting the heading")
# A table-of-contents entry is not the heading: it carries a dotted leader and
# a page number on the same line, so it is not a line consisting of the heading.
toc_text <- paste0("Contents\n\n",
                   paste0(unique(expected$notes$section), " . . . . . . . 38"), "\n\n",
                   collapse = "")
res_toc <- check_semantics_text(paste0(toc_text, good_text), expected, source = "<s>")
ok(isTRUE(res_toc$pass),
   "B28: a contents entry naming the section does not displace the heading")
res_toc_only <- check_semantics_text(paste0(toc_text, "\nbody without headings\n",
                                            lay_out(expected$notes$plain)),
                                     expected, source = "<s>")
ok(!isTRUE(res_toc_only$pass) && identical(res_toc_only$first_failure$status, "SECTION_NOT_FOUND"),
   "B29: a contents entry alone does not count as the heading")
# A heading printed mid-sentence inside prose is not the heading either.
prose_heading <- paste0("Model Analysis Report\n\nThe notes below sit in ",
                        unique(expected$notes$section)[1], " of the report.\n\n",
                        lay_out(expected$notes$plain))
res_ph <- check_semantics_text(prose_heading, expected, source = "<s>")
ok(!isTRUE(res_ph$pass) && identical(res_ph$first_failure$status, "SECTION_NOT_FOUND"),
   "B30: a heading mentioned inside a sentence is not a section heading")

# Manifest row-completeness (round 7, finding 13): the writer looped over the
# segments without braces, so only the LAST segment of each note reached the
# manifest. Every segment must appear exactly once.
man_path <- file.path(tmp, "synthetic-manifest.txt")
write_report_semantic_manifest(res, man_path)
man <- readLines(man_path)
seg_lines <- grep("^  segment [0-9]+: ", man, value = TRUE)
ok(length(seg_lines) == nrow(expected$segments),
   sprintf("B31: the manifest carries one line per comparison segment (%d of %d)",
           length(seg_lines), nrow(expected$segments)))
seg_key <- sub("^  segment ([0-9]+): .*$", "\\1", seg_lines)
note_blocks <- cumsum(grepl("^note: ", man))[grepl("^  segment [0-9]+: ", man)]
ok(!any(duplicated(paste(note_blocks, seg_key))),
   "B32: no segment is written twice within its note")
ok(all(vapply(seq_len(nrow(expected$segments)), function(i)
         any(grepl(paste0(" text=\"", expected$segments$text[i], "\""), man, fixed = TRUE)),
       logical(1))),
   "B33: every expected segment text is present in the manifest")
ok(any(grepl("^# occurrence rule: anchor segments exactly once and in order; supporting segments present", man)),
   "B34: the manifest header states the multiplicity rule")
res_m <- res; res_m$manifest_path <- man_path
ok(identical(report_semantic_manifest_file(res_m), man_path),
   "B36: report_semantic_manifest_file returns the written manifest path")
expect_stop(report_semantic_manifest_file(list(a = 1)),
            "B37: report_semantic_manifest_file refuses a non-result", "report_semantic_result")
res_m$manifest_path <- file.path(tmp, "nowhere.txt")
expect_stop(report_semantic_manifest_file(res_m),
            "B38: report_semantic_manifest_file refuses a missing manifest file", "does not exist")

.run_locale_cases(ok)
if (.pure_only) {
  cat("EXCLUDED C/D: pure-only selection; no render or extraction coverage\n")
  cat(sprintf("report-semantic selected pure suite: %d passed, %d failed, %d skipped\n",
              .pass, .fail, .skip))
  quit(status = if (.fail > 0L) 1L else 0L)
}

# ============================================================================
# C. end to end: quarto + tectonic render, pdftotext extraction
# ============================================================================
cat("\n-- C. end-to-end render and extraction -----------------------------\n")

have_quarto   <- nzchar(Sys.which("quarto"))
have_tectonic <- nzchar(Sys.which("tectonic"))
have_pdftotext <- nzchar(Sys.which("pdftotext"))

# The fixture document: the same shape as the MAR appendices — sourcing the
# repository's one serializer, reading the archived record, applying the
# report's declared transform where it has one, and emitting a blockquote.
write_fixture <- function(dir, mode = c("correct", "dropped", "blob", "reordered")) {
  mode <- match.arg(mode)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(list(verbatim = verbatim_notes, transformed = transformed_notes),
          file.path(dir, "notes.rds"))
  emit <- switch(
    mode,
    correct   = 'txt <- note_text(rec)',
    dropped   = 'txt <- note_text(rec)[-2]',
    reordered = 'txt <- note_text(rec)[c(2, 1, seq_along(note_text(rec))[-(1:2)])]',
    # The measured 2026-09-02 defect, reproduced exactly: the chunk pasted the
    # note LIST instead of calling note_text(), and paste0() coerced each typed
    # note to its data structure. Note this is paste0()/as.character()
    # coercion, not deparse(): deparse escapes the inner quotes, which become
    # undefined LaTeX control sequences and stop the document rendering
    # altogether. The real defect rendered cleanly and shipped - which is the
    # whole reason a mechanical check on the rendered document is needed.
    blob      = 'txt <- paste0(rec)')
  lines <- c(
    "---",
    'title: "Semantic-check fixture"',
    "format:",
    "  pdf:",
    "    pdf-engine: tectonic",
    "    documentclass: article",
    "    geometry: margin=1in",
    "---",
    "",
    "```{r setup}",
    "#| include: false",
    sprintf('source("%s")', file.path(TOOLS, "gate_package.R")),
    sprintf('source("%s")', file.path(TOOLS, "evidence_notes.R")),
    if (have_symbol_map) sprintf('source("%s")', symbol_map_file) else NULL,
    sprintf('notes <- readRDS("%s")', file.path(dir, "notes.rds")),
    "```",
    "",
    "## Appendix E.1 archived record (verbatim)",
    "",
    "```{r e1}",
    "#| echo: false",
    "#| results: asis",
    "rec <- notes$verbatim",
    emit,
    'cat(paste0("> ", gsub("\\n", "\\n> ", txt), collapse = "\\n\\n"), "\\n")',
    "```",
    if (have_symbol_map) c(
      "",
      "## Appendix E.2 archived record (typographically normalized)",
      "",
      "```{r e2}",
      "#| echo: false",
      "#| results: asis",
      "rec <- notes$transformed",
      emit,
      "txt <- protect_dollars(txt)",
      'cat(paste0("> ", gsub("\\n", "\\n> ", txt), collapse = "\\n\\n"), "\\n")',
      "```") else NULL)
  qmd <- file.path(dir, "fixture.qmd")
  writeLines(lines, qmd)
  qmd
}

render_fixture <- function(qmd, to = "pdf") {
  out <- suppressWarnings(system2("quarto", c("render", shQuote(qmd), "--to", to),
                                  stdout = TRUE, stderr = TRUE))
  f <- sub("\\.qmd$", if (identical(to, "pdf")) ".pdf" else ".txt", qmd)
  attr(f, "log") <- out
  f
}

# C0 is the render half on its own: it proves the fixture typesets through
# quarto + tectonic even on a machine that cannot extract the result.
if (have_quarto && have_tectonic) {
  d0 <- file.path(tmp, "render-probe")
  pdf0 <- render_fixture(write_fixture(d0, "correct"), "pdf")
  if (!file.exists(pdf0))
    cat(paste(utils::tail(attr(pdf0, "log"), 20), collapse = "\n"), "\n")
  ok(file.exists(pdf0) && file.size(pdf0) > 1000,
     "C0: the fixture renders to PDF through quarto + tectonic")
} else {
  skip("C0: the fixture renders to PDF through quarto + tectonic",
       "quarto or tectonic missing")
}

if (!have_quarto || !have_tectonic || !have_pdftotext) {
  why <- paste(c(if (!have_quarto) "quarto missing",
                 if (!have_tectonic) "tectonic missing",
                 if (!have_pdftotext) "pdftotext missing"), collapse = "; ")
  skip("C1: correct render passes the semantic check", why)
  skip("C2: a removed note fails on the rendered PDF", why)
  skip("C3: a deparsed list blob fails on the rendered PDF", why)
  skip("C4: reordered notes fail on the rendered PDF", why)
  skip("C5: manifest written next to the PDF, recording the outcome", why)
  skip("C5b: manifest is written on a FAILING run too", why)
  cat("      NOTE: group C is the only end-to-end coverage of the rendered\n",
      "     PDF. With it skipped, the check's extraction path is UNPROVEN on\n",
      "     this machine. Install poppler-utils (pdftotext) and re-run.\n",
      sep = "")
} else {
  expected_e2e <- make_expected()

  d1 <- file.path(tmp, "correct")
  pdf1 <- render_fixture(write_fixture(d1, "correct"))
  r1 <- NULL
  if (!file.exists(pdf1)) {
    cat(paste(utils::tail(attr(pdf1, "log"), 20), collapse = "\n"), "\n")
    ok(FALSE, "REPORT-PUBLIC C1: correct render passes the semantic check")
  } else {
    r1 <- check_report_semantics(pdf1, expected_e2e)
    if (!isTRUE(r1$pass)) cat("      ", r1$message, "\n", sep = "")
    ok(isTRUE(r1$pass), "REPORT-PUBLIC C1: correct render passes the semantic check")
  }

  # C5 is about the manifest, not about the verdict: it must land next to the
  # PDF and record whatever the outcome was, with one record per expected note.
  if (is.null(r1)) {
    ok(FALSE, "C5: manifest written next to the PDF, recording the outcome")
  } else {
    man <- if (file.exists(r1$manifest_path))
      readLines(r1$manifest_path) else character(0)
    ok(identical(r1$manifest_path, report_semantic_manifest_path(pdf1)) &&
         length(man) > 0 &&
         any(grepl("^# result: (PASS|FAIL)$", man)) &&
         any(man == paste0("# notes-expected: ", nrow(expected_e2e$notes))) &&
         sum(grepl("^note: ", man)) == nrow(expected_e2e$notes),
       "C5: manifest written next to the PDF, recording the outcome")
  }

  fail_results <- list()
  for (case in list(
        list(mode = "dropped",   name = "C2: a removed note fails on the rendered PDF"),
        list(mode = "blob",      name = "C3: a deparsed list blob fails on the rendered PDF"),
        list(mode = "reordered", name = "C4: reordered notes fail on the rendered PDF"))) {
    d <- file.path(tmp, case$mode)
    pdfx <- render_fixture(write_fixture(d, case$mode))
    if (!file.exists(pdfx)) {
      cat(paste(utils::tail(attr(pdfx, "log"), 20), collapse = "\n"), "\n")
      ok(FALSE, case$name)
      next
    }
    rx <- check_report_semantics(pdfx, expected_e2e)
    if (isTRUE(rx$pass)) cat("      unexpected PASS on the ", case$mode,
                             " fixture\n", sep = "")
    ok(!isTRUE(rx$pass), case$name)
    fail_results[[case$mode]] <- rx
    if (identical(case$mode, "blob"))
      ok(any(!rx$tripwires$accounted),
         "C3b: the structural-leakage tripwire fires on the rendered PDF")
  }

  # A failing run must leave its evidence on disk, or the failure cannot be
  # diagnosed without re-running the pipeline.
  rf <- fail_results[["dropped"]]
  if (is.null(rf)) {
    ok(FALSE, "C5b: manifest is written on a FAILING run too")
  } else {
    manf <- if (file.exists(rf$manifest_path))
      readLines(rf$manifest_path) else character(0)
    ok(length(manf) > 0 && any(grepl("^# result: FAIL$", manf)) &&
         any(grepl("^  segment .*status=MISSING", manf)) &&
         any(grepl("^# message: .*FAILED", manf)),
       "C5b: manifest is written on a FAILING run too")
  }

  # The extractor refuses to guess when the binary is absent.
  ok(is.character(report_semantic_extractor()) &&
       nzchar(report_semantic_extractor()),
     "C6: the extractor resolves to the pdftotext binary")
}

# The absent-binary contract holds regardless of this machine's tooling.
expect_stop(report_semantic_extractor(which = function(x) ""),
            "C7: a missing pdftotext is a hard stop, never a silent pass",
            "pdftotext")

# ============================================================================
# D. reduced-coverage substitute for group C: the same four cases, rendered
#    through the same document and the same pandoc typesetting, written to
#    plain text instead of PDF.
#
#    THIS IS NOT A REPLACEMENT FOR GROUP C. It exercises the R chunk, the one
#    serializer, the declared render-time transform, Markdown escaping and
#    pandoc's typographic substitutions (smart quotes, dashes, wrapping) —
#    which is where both measured defects lived — but it does NOT exercise
#    LaTeX, the PDF, or PDF text extraction. Group C remains the required
#    coverage; group D exists so the check is not entirely unexercised
#    end-to-end on a machine without poppler-utils.
# ============================================================================
cat("\n-- D. reduced-coverage end-to-end (pandoc plain, NOT the PDF) -------\n")

if (!have_quarto) {
  for (n in c("D1: correct render passes", "D2: a removed note fails",
              "D3: a deparsed list blob fails", "D4: reordered notes fail"))
    skip(n, "quarto missing")
} else {
  expected_d <- make_expected()
  run_plain <- function(mode) {
    d <- file.path(tmp, paste0("plain-", mode))
    txt <- render_fixture(write_fixture(d, mode), "plain")
    if (!file.exists(txt)) {
      cat(paste(utils::tail(attr(txt, "log"), 20), collapse = "\n"), "\n")
      return(NULL)
    }
    check_semantics_text(readLines(txt, warn = FALSE), expected_d,
                         source = txt)
  }
  rd1 <- run_plain("correct")
  if (!is.null(rd1) && !isTRUE(rd1$pass)) cat("      ", rd1$message, "\n", sep = "")
  ok(!is.null(rd1) && isTRUE(rd1$pass),
     "D1: correct render passes (pandoc plain, reduced coverage)")
  rd2 <- run_plain("dropped")
  ok(!is.null(rd2) && !isTRUE(rd2$pass) &&
       identical(rd2$first_failure$status, "MISSING"),
     "D2: a removed note fails (pandoc plain, reduced coverage)")
  rd3 <- run_plain("blob")
  ok(!is.null(rd3) && !isTRUE(rd3$pass),
     "D3: a deparsed list blob fails (pandoc plain, reduced coverage)")
  ok(!is.null(rd3) && any(!rd3$tripwires$accounted),
     "D3b: the structural-leakage tripwire fires on the rendered document")
  rd4 <- run_plain("reordered")
  ok(!is.null(rd4) && !isTRUE(rd4$pass) &&
       identical(rd4$first_failure$status, "OUT_OF_ORDER"),
     "D4: reordered notes fail (pandoc plain, reduced coverage)")
}

if (!have_quarto || !have_pdftotext) {
  ok(FALSE, "R11-R13 require Quarto and Poppler")
} else if (!have_tectonic) {
  for (name in c("R11 neutral reporting renders and passes semantics",
                "R12 a dropped archived note blocks package creation",
                "R13 the second page alone appears in the rendered report")) skip(name, "tectonic missing")
} else {
  .declared_report_fixture(REPO, function(name, assertions) ok(assertions, name))
}

cat(sprintf("\nreport-semantic battery: %d passed, %d failed, %d skipped\n",
            .pass, .fail, .skip))
quit(status = if (.fail > 0L) 1L else 0L)

}  # end .run_report_semantic_tests

if (.invoked_as_script()) .run_report_semantic_tests()
