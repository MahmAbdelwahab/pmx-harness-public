# Report-side typography helpers for REPRODUCED archived prose (MAR only).
#
# Scope: render-time text transforms that a report applies to store text it
# reproduces verbatim — the recorded review notes of signed stage records and
# the appended plan. They change how the archived characters are typeset; they
# never change the archived wording, and they never touch a stored value.
#
# Use the same rendering transform for the report and its semantic check.
# Projects can declare their own symbol normalization after this shared step.
#
# Deliberately NOT in tools/evidence_notes.R: that file is the ONE serializer
# for note lists, and its output is the archived truth the gate record and the
# report must agree on. A typography transform is a separate, declared stage
# applied AFTER serialization (the semantic check records `text` and `shown`
# separately for exactly this reason); folding it into the serializer would
# put render markup into the gate records.
#
# Base R only, so a report chunk can source this file without the gate
# machinery. No data access, no computation of analysis values.

# Escape dollar signs in archived prose so that a pair of dollar signs does
# not turn source text into a math span. Apply this before project-specific
# symbol normalization so its intended math markup is preserved.
#
# The break opportunity after each dollar sign inserts no character and
# changes no wording; it lets a long dotted expression wrap at its own
# boundaries instead of overflowing the text block.
#
# The replacement below is taken literally (fixed = TRUE does not process
# backslash escapes in the replacement), so it holds the exact two markup
# pieces wanted: an escaped dollar sign and a break opportunity.
protect_dollars <- function(text) {
  stopifnot(is.character(text))
  gsub("$", "\\$\\allowbreak{}", text, fixed = TRUE)
}
