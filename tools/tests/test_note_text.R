#!/usr/bin/env Rscript
# Tests for note_text() (tools/evidence_notes.R) — the one renderer the MAR
# appendices use to reproduce archived gate notes as prose.
#
# The load-bearing property is agreement with the gate record: an evidence note
# reproduced in the report must read exactly as the same note reads in the gate
# package, so the two are compared character-for-character against
# .render_note_item() (tools/gate_package.R), which is the authority.
#
# Base R only, no fixtures, no files written. Run with
#   Rscript tools/tests/test_note_text.R
# Exits non-zero if any assertion fails.
#
# SAFE TO SOURCE: guarded behind .invoked_as_script() like the other batteries
# under tools/tests/, so tar_source("tools") defines and runs nothing.
.invoked_as_script <- function() {
  fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  length(fa) > 0 && grepl("test_note_text\\.R$", sub("^--file=", "", fa[1]))
}

.run_note_text_tests <- function() {

args <- commandArgs(FALSE)
file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
TOOLS <- normalizePath(file.path(dirname(file_arg), ".."), mustWork = TRUE)
source(file.path(TOOLS, "evidence_notes.R"))
# .render_note_item() and .md_render_col() (the latter used by evidence_note())
source(file.path(TOOLS, "gate_package.R"))

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
  ok(hit, name)
}

# --- A: character passthrough (the legacy note form) ------------------------
chr <- c("first note", "second note")
ok(identical(note_text(chr), chr), "A1: character vector returned unchanged")
ok(identical(note_text(character(0)), character(0)),
   "A2: empty character vector returned unchanged")
ok(identical(note_text(list()), character(0)),
   "A3: empty list renders as an empty character vector")

# --- B: evidence rendering equals the gate renderer -------------------------
n_subjects <- 500L
ev <- evidence_note("Subjects simulated per scenario", n_subjects)
ok(identical(ev$kind, "evidence"), "B1: evidence_note() builds an evidence note")
ok(identical(note_text(ev), .render_note_item(ev)),
   "B2: single evidence note renders exactly as the gate renderer does")
ok(identical(note_text(ev),
             paste0(ev$label, " (from ", ev$expression_text, "): ",
                    ev$value_rendered)),
   "B3: evidence note renders label, expression and value in the gate's shape")

metrics <- c(1.5, 2.25)
ev2 <- evidence_note("Exposure metrics", signif(metrics, 3))
ok(identical(note_text(ev2), .render_note_item(ev2)),
   "B4: vector-valued evidence note matches the gate renderer")

# --- C: authored notes render as their text --------------------------------
au <- authored_note("No deviation from the approved plan arose.")
ok(identical(note_text(au), "No deviation from the approved plan arose."),
   "C1: authored note renders as its text, verbatim")
ok(identical(note_text(au), au$text), "C2: authored wording is unchanged")

# --- D: mixed list, in order -----------------------------------------------
mixed <- list(au, "a plain legacy string", ev)
ok(identical(note_text(mixed),
             c(au$text, "a plain legacy string", .render_note_item(ev))),
   "D1: mixed list renders one string per note, in order")
ok(identical(note_text(list(c("two", "elements"), au)),
             c("two", "elements", au$text)),
   "D2: a multi-element character item contributes all of its elements")
ok(is.character(note_text(mixed)) && length(note_text(mixed)) == 3L,
   "D3: result is a character vector, one element per note")

# --- E: unsupported input is an error, never a deparsed list ---------------
expect_stop(note_text(42), "E1: numeric input rejected", "note_text\\(\\) takes")
expect_stop(note_text(list(list(kind = "mystery", text = "x"))),
            "E2: unknown note kind rejected", "unsupported note item")
expect_stop(note_text(list(1L)), "E3: non-character item rejected",
            "unsupported note item")
rendered <- note_text(mixed)
ok(!any(grepl("list(kind", rendered, fixed = TRUE)),
   "E4: rendered output contains no deparsed note structure")

cat(sprintf("note_text battery: %d passed, %d failed\n", .pass, .fail))
quit(status = if (.fail > 0L) 1L else 0L)

}  # end .run_note_text_tests

if (.invoked_as_script()) .run_note_text_tests()
