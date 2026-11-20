# Expression-bound evidence notes (integrity batch 3b tranche 2, item 3).
#
# A gate "note" is either an AUTHORED narrative (prose the task's agent writes,
# never a computed number) or a piece of EVIDENCE (a value read from a target).
# The one rule everything serves — the LLM proposes, tools compute — means an
# evidence value must be produced by executed code, never transcribed. These
# constructors make that structural:
#
#   evidence_note(label, expr)
#     Captures `expr` UNEVALUATED for provenance, evaluates it in the caller's
#     environment (target context), renders the value through the SAME typed
#     deterministic renderer the gate tables use, and returns a structured note.
#     REFUSES a literal-only expression (a bare number or string): a literal is
#     an authored assertion masquerading as evidence, so it must reference at
#     least one symbol (in a task module that symbol is the upstream target the
#     value came from). Returns:
#         list(kind = "evidence", label, expression_text, value_rendered)
#
#   authored_note(text)
#     Prose authored by the agent, rendered OUTSIDE the digested envelope.
#     Returns list(kind = "authored", text).
#
# render_gate_package() (tools/gate_package.R) consumes a list mixing plain
# character strings, evidence notes, and authored notes; see its .split_notes.

# Render an evaluated value to a deterministic string using the gate table
# renderer (.md_render_col, defined in tools/gate_package.R and sourced
# alongside this file by tar_source("tools") and every test/CI harness). A
# vector value renders element-wise, joined with ", ".
.evidence_render_value <- function(value) {
  if (is.list(value) && !is.data.frame(value))
    stop("evidence_note value is a list; evidence must be an atomic value or ",
         "vector produced by a target", call. = FALSE)
  paste(.md_render_col(value, "<evidence>"), collapse = ", ")
}

evidence_note <- function(label, expr) {
  expr_q <- substitute(expr)
  syms <- all.vars(expr_q)
  if (length(syms) == 0)
    stop("evidence_note('", label, "', ...) refuses a literal-only expression: ",
         "evidence must reference at least one symbol (the target it came ",
         "from), never a bare authored number or string. Use authored_note() ",
         "for prose.", call. = FALSE)
  value <- eval(expr_q, envir = parent.frame())
  list(kind = "evidence",
       label = label,
       expression_text = paste(deparse(expr_q), collapse = " "),
       value_rendered = .evidence_render_value(value))
}

authored_note <- function(text) {
  if (!is.character(text) || length(text) != 1)
    stop("authored_note(text) takes a single character string", call. = FALSE)
  list(kind = "authored", text = text)
}

# --- note_text(): the one renderer for note lists read back out of the store --
#
# A promoted note target (e.g. report_sim_notes, report_da_notes) holds exactly
# what the task module shipped to its gate: since the 2026-09-01 migration that
# is a LIST mixing plain character strings, evidence notes and authored notes,
# not a character vector. Any consumer that reproduces those notes as prose —
# the MAR appendices are the current one — needs the same rendering the gate
# record shows, or the reader of the report and the reader of the gate see
# different text for the same archived note.
#
# note_text() is that single renderer. It accepts:
#   - a character vector (the legacy note form): returned UNCHANGED;
#   - one typed note (evidence or authored): a single string;
#   - a list mixing character strings and typed notes: one string per item, in
#     order (a character element contributes all of its elements).
# Anything else is an error — a silently deparsed list is the defect this
# function exists to remove (found 2026-09-03: MAR Appendix E rendered
# `list(kind = "evidence", ...)` as body text).
#
# Rendering is fixed, with no formatting choices of its own:
#   evidence -> "<label> (from <expression_text>): <value_rendered>"
#   authored -> <text>
# The evidence form MIRRORS .render_note_item() in tools/gate_package.R, which
# is the authority; tools/tests/test_note_text.R asserts the two agree
# character-for-character so the pair cannot drift apart. It is deliberately
# duplicated rather than delegated so that this file stands alone (a report
# chunk sources it without the gate machinery).
.note_text_item <- function(x) {
  if (is.character(x)) return(x)
  if (is.list(x) && identical(x$kind, "evidence"))
    return(paste0(x$label, " (from ", x$expression_text, "): ", x$value_rendered))
  if (is.list(x) && identical(x$kind, "authored")) return(x$text)
  stop("note_text() got an unsupported note item of class ",
       paste(class(x), collapse = "/"),
       "; a note is a character string, an evidence_note(), or an ",
       "authored_note()", call. = FALSE)
}

note_text <- function(notes) {
  if (is.character(notes)) return(notes)
  if (is.list(notes) && "kind" %in% names(notes)) return(.note_text_item(notes))
  if (!is.list(notes))
    stop("note_text() takes a character vector, a single typed note, or a ",
         "list of notes; got ", paste(class(notes), collapse = "/"),
         call. = FALSE)
  out <- unlist(lapply(notes, .note_text_item), use.names = FALSE)
  if (is.null(out)) character(0) else out
}
