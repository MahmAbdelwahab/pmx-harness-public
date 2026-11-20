# Semantic check on the rendered report.
#
# A gate digest proves which bytes were signed. Rendering can still lose a
# note, expose a deparsed list, or consume punctuation. Compare the rendered
# text with the notes as serialized for the archived record.
#
# This module is the mechanical oracle for that gap. It compares the text the
# report was SUPPOSED to carry — serialised through the one existing serializer,
# note_text() — against the text the finished PDF ACTUALLY carries, extracted
# with `pdftotext -layout`. The reporting task module runs it as a target
# (report_semantic_check) that stands between the render targets and
# reporting_package, so a report that lost its content cannot reach a gate
# record and therefore cannot be signed.
#
# WHAT IT IS NOT
# --------------
# It computes nothing scientific. It compares text. No analysis numeric is
# produced, transformed, or judged here; a number in a note is compared as the
# characters the serializer produced, never re-derived.
#
# DEPENDENCIES
# ------------
# Base R plus the `pdftotext` binary (poppler-utils). The binary is probed with
# Sys.which(); when it is absent the check STOPS with an instruction. It never
# degrades to a weaker oracle and never passes silently — an unprovable report
# is not an approved report.
#
# THE COMPARISON, AND WHY IT IS SHAPED THIS WAY
# ---------------------------------------------
# 1. Expected text comes from note_text() — the same renderer the gate record
#    uses — so the report and the gate cannot disagree about what a note says.
# 2. A report may declare a render-time TEXT TRANSFORM over reproduced prose.
#    For example, protect_dollars() can run before normalize_symbols().
#    The expected manifest applies the very
#    same function the report applies; it never re-implements it.
# 3. What survives typesetting is compared, and only that. The transform and
#    Markdown both introduce markup that the engine consumes rather than prints
#    (math spans, code-span backticks, LaTeX macros, backslash escapes); that
#    markup is stripped, leaving the printed characters. Then each note is cut
#    into SEGMENTS: maximal runs of characters whose printed form is stable
#    through pandoc and TeX. Characters excluded from segments, with the reason:
#      -   hyphen/dash      pandoc smart punctuation turns -- into an en dash
#      " '                  smart punctuation turns quotes into curly quotes
#      .. (two or more)     ... becomes a single ellipsis glyph
#      * [ ] < > # ~ ^ | \  Markdown/LaTeX active characters
#      ` $ { }              markup already handled above; a lone one is suspect
#      any non-ASCII        font substitution and extraction vary
#    Everything else — letters, digits, whitespace, and , ; : ( ) / % + = ? !
#    _ @ & . $ — is compared literally. The dollar sign is deliberately IN the
#    comparison because a swallowed `$` pair is exactly
#    what this check must catch.
# 4. Normalization on the extracted side is confined to line wrapping and runs
#    of whitespace. Nothing else is rewritten. Line wrapping needs care,
#    because a line break in a typeset document stands for one of three things
#    in the source and they are not interchangeable:
#      - ordinary word wrap: the source had a SPACE there;
#      - hyphenation: the engine INSERTED a hyphen that the source does not
#        have (repaired by joining, harmlessly, since a hyphen is never inside
#        a segment anyway);
#      - a discretionary break: the source had NOTHING there. The report's own
#        typography helper creates these deliberately - protect_dollars() puts
#        an \allowbreak after every dollar sign so a long dotted expression can
#        wrap at its own boundaries - and the engine then breaks the line in
#        the middle of a word with no character to show for it. Measured on the
#        fixture: "(qual_vpc$settings$n and qual_vpc$settings$" / "seed), so
#        two dollar signs ..." wraps immediately after a dollar sign.
#    Treating every break as a space therefore invents a space that is not in
#    the document and loses a correct note. So each line break is marked, runs
#    of horizontal whitespace collapse to one space, and a segment is matched
#    with a pattern in which a mark may stand anywhere (it carries no
#    character) and a source space may be realized as a mark. That is still
#    only line-wrapping and whitespace normalization; no character of content
#    is added, removed or rewritten on either side.
# 5. Every expected note must appear in its OWN SECTION of the document, and
#    the notes must appear in the order the report declares them. The
#    multiplicity rule is: each note's ANCHOR segment (its longest) appears
#    exactly as often as the expected set uses that text within the section,
#    and every supporting segment is present and in order; see the OCCURRENCE
#    RULE at check_semantics_text(). Section binding (round 7, finding 13):
#    each declared section name is located as a heading LINE of the extracted
#    text - a line consisting of the heading and nothing else, so a table-of-
#    contents entry (dotted leader and page number on the same line) or a
#    mention inside a sentence is not the heading - and a note is searched
#    for only between its heading and the next declared heading. A heading
#    that cannot be located is a failure that quotes the heading: the report's
#    declared section names must be the headings as the rendered document
#    prints them.
# 6. Structural leakage patterns (`list(kind =`, `c("`, `structure(`) are a
#    TRIPWIRE, not the proof. They are reported only where they fall OUTSIDE a
#    matched expected note, because archived evidence notes legitimately
#    reproduce R expressions that contain them. The proof is item 5; the
#    tripwire catches a shape the expected-text comparison has no reason to see.
#
# AUDITABILITY
# ------------
# Every run writes a manifest next to the PDF: the settings, every expected
# note, its comparison segments, and the position at which each was found. The
# check is therefore walkable by the review stage without re-running it.

# Minimum length of a comparison segment. Short runs recur in ordinary prose,
# and "appears exactly once" would then produce false alarms.
REPORT_SEMANTIC_MIN_SEGMENT <- 24L
# When a note yields no run this long, its longest run is used instead and the
# note is marked `weak` in the manifest. Below the floor the note is
# UNVERIFIABLE, which is a failure — never a silent pass.
REPORT_SEMANTIC_HARD_FLOOR <- 8L
# Placeholder written where markup was removed. It is itself an unstable
# character, so a segment can never span a removed region.
REPORT_SEMANTIC_GAP <- "§"

# Characters compared literally; see note 3 in the header.
.RS_STABLE <- "A-Za-z0-9 \t\n\r,;:()/%+=?!_@&.$"

# Structural-leakage tripwire patterns (perl regex, matched on the flattened
# extraction). Documented as a tripwire, not the proof.
# Written token by token so a line-break mark (see .rs_flatten_document) or a
# space between the tokens does not hide the shape.
.RS_TRIPWIRES <- c(
  "list[\u0002]*\\([\u0002]*kind[ \u0002]*=",
  "c[\u0002]*\\([\u0002]*[\"\u201c\u201d]",
  "structure[\u0002]*\\(")

# --- extraction --------------------------------------------------------------

# Resolves the pdftotext binary. `which` is injectable so the absent-binary
# contract is testable on a machine that has the binary.
report_semantic_extractor <- function(which = Sys.which) {
  p <- tryCatch(which("pdftotext"), error = function(e) "")
  if (length(p) != 1L || is.na(p) || !nzchar(p)) {
    stop("the rendered-report semantic check needs the `pdftotext` binary ",
         "(poppler-utils) and it is not on PATH. Install it (Debian/Ubuntu: ",
         "apt-get install poppler-utils) and re-run. The check does not fall ",
         "back to a weaker oracle and does not pass when it cannot read the ",
         "PDF: an unverifiable report is not an approved report.",
         call. = FALSE)
  }
  unname(p)
}

# Extracts the text of a PDF with `pdftotext -layout`, returned as one string.
report_pdf_text <- function(pdf, which = Sys.which) {
  bin <- report_semantic_extractor(which)
  if (!file.exists(pdf))
    stop("no such rendered report to check: ", pdf, call. = FALSE)
  out <- suppressWarnings(system2(bin, c("-layout", shQuote(pdf), "-"),
                                  stdout = TRUE, stderr = TRUE))
  st <- attr(out, "status")
  if (!is.null(st) && !identical(as.integer(st), 0L))
    stop("pdftotext failed on ", pdf, " (exit ", st, "): ",
         paste(utils::head(out, 5), collapse = " | "), call. = FALSE)
  paste(out, collapse = "\n")
}

# --- normalization and segmentation -----------------------------------------

# Prepare each text element before concatenation or regular expressions. An
# unknown label prefers valid UTF-8 bytes; a declared Latin-1 label keeps its
# own meaning. Conversions must survive an exact byte round-trip. Work on a
# local copy so serialized notes and the caller's attributes remain intact.
.rs_prepare_text <- function(x) {
  if (!is.character(x)) return(x)
  fail <- function() stop("semantic text encoding: invalid or non-text input",
                         call. = FALSE)
  convert <- function(value, from, to) {
    tryCatch(iconv(value, from = from, to = to, sub = NA),
             error = function(e) fail())
  }
  for (i in seq_along(x)) {
    value <- x[[i]]
    if (is.na(value)) next
    label <- Encoding(value)
    if (label == "bytes") fail()
    bytes <- charToRaw(value)
    if (all(as.integer(bytes) < 128L)) next
    view <- rawToChar(bytes)
    Encoding(view) <- "unknown"
    if (label == "UTF-8") {
      if (!isTRUE(validUTF8(view))) fail()
      Encoding(view) <- "UTF-8"
      x[[i]] <- view
    } else if (label == "unknown" && isTRUE(validUTF8(view))) {
      Encoding(view) <- "UTF-8"
      x[[i]] <- view
    } else {
      from <- if (label == "latin1") "latin1" else ""
      out <- convert(view, from, "UTF-8")
      if (is.na(out) || !isTRUE(validUTF8(out))) fail()
      back <- convert(out, "UTF-8", from)
      if (is.na(back) || !identical(charToRaw(back), bytes)) fail()
      x[[i]] <- out
    }
  }
  x
}

# Collapses runs of whitespace. Used on EXPECTED text (segments), where a line
# break always stands for a space.
.rs_flatten <- function(x) {
  x <- .rs_prepare_text(x)
  x <- paste(x, collapse = "\n")
  x <- gsub("\r\n", "\n", x, fixed = TRUE)
  x <- gsub("\r", "\n", x, fixed = TRUE)
  x <- gsub("\u00a0", " ", x, fixed = TRUE)   # no-break space is a space
  x <- gsub("[[:space:]]+", " ", x)
  trimws(x)
}

# Flattens the EXTRACTED document. Line breaks become marks rather than spaces,
# because a break may stand for a space (word wrap) or for nothing at all (a
# discretionary break); see header note 4. Hyphenation is repaired first: the
# engine inserted that hyphen, and a hyphen is never inside a segment, so
# joining can neither merge two segments nor hide a lost character.
.RS_BREAK <- "\u0002"
.rs_flatten_document <- function(x) {
  x <- .rs_prepare_text(x)
  x <- paste(x, collapse = "\n")
  x <- gsub("\r\n", "\n", x, fixed = TRUE)
  x <- gsub("\r", "\n", x, fixed = TRUE)
  x <- gsub("\u00a0", " ", x, fixed = TRUE)
  x <- gsub("\u00ad", "", x, fixed = TRUE)     # soft hyphen carries no text
  # Extraction can interleave a page number with a sentence that spans pages.
  # Drop a first or last line consisting only of a page number, optionally
  # decorated. Retain lines with words or an additional column.
  pages <- strsplit(x, "\f", fixed = TRUE)[[1]]
  x <- paste(vapply(pages, function(pg) {
    ln <- strsplit(pg, "\n", fixed = TRUE)[[1]]
    keep <- nzchar(trimws(ln))
    idx <- which(keep)
    if (length(idx)) {
      furniture <- grepl("^[[:space:][:punct:]]*[0-9ivxlcIVXLC]+[[:space:][:punct:]]*$",
                         ln[idx])
      drop <- integer(0)
      if (furniture[1]) drop <- c(drop, idx[1])
      if (length(idx) > 1L && furniture[length(idx)])
        drop <- c(drop, idx[length(idx)])
      if (length(drop)) ln <- ln[-drop]
    }
    paste(ln, collapse = "\n")
  }, character(1)), collapse = "\n")
  x <- gsub("-[ \t]*\n[ \t]*", "", x)          # end-of-line hyphenation
  x <- gsub("[ \t]*\n[ \t]*", .RS_BREAK, x)    # every other break: a mark
  x <- gsub("[ \t]+", " ", x)
  trimws(x)
}

# Line-break marks are internal; anything shown to a reader gets them back as
# spaces, and the patterns show them as <break>.
.rs_readable <- function(x) {
  x <- gsub(paste0("[ ", .RS_BREAK, "]*"), "<break>", x, fixed = TRUE)
  gsub(.RS_BREAK, " ", x, fixed = TRUE)
}

# Builds the search pattern for one expected segment against a marked document.
# Per header note 4: a source space may be realized as spaces and/or marks, and
# a mark may appear between any two characters because it carries none. Nothing
# else is made optional - every character of the segment must be present, in
# order, exactly as the serializer produced it.
.rs_needle_pattern <- function(needle) {
  ch <- strsplit(needle, "", fixed = TRUE)[[1]]
  if (!length(ch)) return("")
  meta <- c(".", "\\", "|", "(", ")", "[", "]", "^", "$", "*", "+", "?",
            "{", "}")
  esc <- function(c1) if (c1 %in% meta) paste0("\\", c1) else c1
  gap <- paste0("[", .RS_BREAK, "]*")
  out <- character(0)
  i <- 1L
  while (i <= length(ch)) {
    if (ch[i] == " ") {
      j <- i
      while (j <= length(ch) && ch[j] == " ") j <- j + 1L
      out <- c(out, paste0("[ ", .RS_BREAK, "]+"))
      i <- j
    } else {
      out <- c(out, esc(ch[i]))
      if (i < length(ch) && ch[i + 1L] != " ") out <- c(out, gap)
      i <- i + 1L
    }
  }
  paste(out, collapse = "")
}

# Removes markup the engine consumes rather than prints, leaving printed
# characters. Enumerated, in this order:
#   1. escaped dollar signs are set aside (they PRINT as a dollar sign)
#   2. math spans $...$ are removed (their content is typeset, not printed as
#      the source characters)
#   3. LaTeX macros, with an optional braced argument, are removed
#   4. remaining backslash escapes reduce to the escaped character
#   5. code-span backticks are removed, their content kept
#   6. the set-aside dollar signs are restored
.rs_strip_markup <- function(x) {
  x <- .rs_prepare_text(x)
  sentinel <- "\u0001"
  x <- gsub("\\\\\\$", sentinel, x)                       # \$  -> sentinel
  x <- gsub("\\$[^$]*\\$", .rs_prepare_text(REPORT_SEMANTIC_GAP), x) # $..$ -> gap
  x <- gsub("\\\\[A-Za-z]+(\\{[^}]*\\})?", "", x)         # \macro{...}
  x <- gsub("\\\\([^A-Za-z])", "\\1", x)                  # \% -> %
  x <- gsub("`", "", x, fixed = TRUE)
  gsub(sentinel, "$", x, fixed = TRUE)
}

# Cuts a printed string into maximal runs of typographically stable characters.
.rs_segments <- function(x) {
  x <- .rs_prepare_text(x)
  x <- gsub("\\.{2,}", .rs_prepare_text(REPORT_SEMANTIC_GAP), x) # ... -> ellipsis glyph
  x <- gsub(paste0("[^", .RS_STABLE, "]"), "\u0001", x)
  parts <- strsplit(x, "\u0001", fixed = TRUE)[[1]]
  parts <- vapply(parts, .rs_flatten, character(1), USE.NAMES = FALSE)
  parts[nzchar(parts)]
}

# --- the expected-text manifest ---------------------------------------------

.rs_section_spec <- function(x) {
  if (is.list(x) && !is.null(x$notes) && !("kind" %in% names(x)))
    return(list(notes = x$notes, transform = x$transform))
  list(notes = x, transform = NULL)
}

# Builds the canonical expected-text manifest for a report.
#
#   sections  a NAMED list, in the order the report presents them. Each element
#             is either the notes object itself (as promoted from a gate
#             package: a character vector, one typed note, or a list mixing
#             them) or a list(notes = <notes>, transform = <function|NULL>)
#             where `transform` is the report's own declared render-time text
#             transform for that section.
#
# Returns an object of class "report_expected_notes" carrying, per note: the
# text note_text() produced, the text after the declared transform, the printed
# form after markup stripping, and the comparison segments.
expected_report_notes <- function(sections,
                                  min_segment_chars = REPORT_SEMANTIC_MIN_SEGMENT,
                                  hard_floor = REPORT_SEMANTIC_HARD_FLOOR) {
  if (!is.list(sections) || !length(sections))
    stop("expected_report_notes() takes a non-empty list of report sections",
         call. = FALSE)
  nm <- names(sections)
  nm <- .rs_prepare_text(nm)
  if (is.null(nm) || any(is.na(nm)) || any(!nzchar(nm)))
    stop("expected_report_notes() takes a NAMED list: each name is the report ",
         "section that carries those notes, and the order is the order the ",
         "report presents them", call. = FALSE)

  notes_rows <- list(); seg_rows <- list(); id <- 0L
  for (s in seq_along(sections)) {
    spec <- .rs_section_spec(sections[[s]])
    # note_text() is the ONE serializer; it errors on an unsupported item
    # rather than deparsing it, which is the defect this check exists for.
    raw <- note_text(spec$notes)
    if (!is.character(raw))
      stop("note_text() did not return character for section '", nm[s], "'",
           call. = FALSE)
    shown <- if (is.null(spec$transform)) raw else {
      if (!is.function(spec$transform))
        stop("section '", nm[s], "': `transform` must be the function the ",
             "report itself applies, or NULL", call. = FALSE)
      out <- spec$transform(raw)
      if (!is.character(out) || length(out) != length(raw))
        stop("section '", nm[s], "': the declared render-time transform must ",
             "return one string per note", call. = FALSE)
      out
    }
    shown <- .rs_prepare_text(shown)
    plain <- .rs_strip_markup(shown)
    for (i in seq_along(raw)) {
      id <- id + 1L
      segs <- .rs_segments(plain[i])
      keep <- segs[nchar(segs) >= min_segment_chars]
      coverage <- "full"
      if (!length(keep)) {
        if (!length(segs) || max(nchar(segs)) < hard_floor) {
          coverage <- "unverifiable"
          keep <- character(0)
        } else {
          coverage <- "weak"
          keep <- segs[which.max(nchar(segs))]
        }
      }
      notes_rows[[length(notes_rows) + 1L]] <- data.frame(
        note_id = id, section = nm[s], note = i,
        text = raw[i], shown = shown[i], plain = plain[i],
        n_segments = length(keep), coverage = coverage,
        stringsAsFactors = FALSE)
      if (length(keep))
        seg_rows[[length(seg_rows) + 1L]] <- data.frame(
          note_id = id, section = nm[s], note = i,
          segment = seq_along(keep), text = keep, nchar = nchar(keep),
          # The note's ANCHOR is its longest run: the most distinctive thing it
          # says, and the one whose multiplicity is enforced. See the
          # OCCURRENCE RULE at check_semantics_text().
          anchor = seq_along(keep) == which.max(nchar(keep)),
          stringsAsFactors = FALSE)
    }
  }
  structure(list(
    notes = do.call(rbind, notes_rows),
    segments = if (length(seg_rows)) do.call(rbind, seg_rows) else
      data.frame(note_id = integer(0), section = character(0),
                 note = integer(0), segment = integer(0),
                 text = character(0), nchar = integer(0),
                 anchor = logical(0), stringsAsFactors = FALSE),
    settings = list(min_segment_chars = min_segment_chars,
                    hard_floor = hard_floor,
                    stable_characters = .RS_STABLE,
                    tripwires = .RS_TRIPWIRES)),
    class = "report_expected_notes")
}

# --- section location --------------------------------------------------------

# Pattern for one heading as a line of its own in the marked document: the
# heading's stable segments in order, with anything but a line break allowed
# between them (unstable characters, e.g. a dash), anchored at a line start
# (document start or a break mark) and a line end (a break mark or document
# end). Returns NULL when the heading has no stable segment at all, which is
# reported as unlocatable.
.rs_heading_pattern <- function(heading) {
  heading <- .rs_prepare_text(heading)
  segs <- .rs_segments(.rs_strip_markup(heading))
  if (!length(segs)) return(NULL)
  body <- paste(vapply(segs, .rs_needle_pattern, character(1)),
                collapse = paste0("[^", .RS_BREAK, "]*"))
  paste0("(?:^|", .RS_BREAK, ")[ ]*", body, "[ ]*(?:", .RS_BREAK, "|$)")
}

# Locates every declared section heading in the flattened document, in the
# declared order. Returns a data frame (section, start, end) where end is the
# position of the next heading (or one past the document), or a character
# string naming the first heading that could not be located.
.rs_locate_sections <- function(flat, sections) {
  starts <- integer(0); cursor <- 1L
  for (h in sections) {
    pat <- .rs_heading_pattern(h)
    if (is.null(pat)) return(h)
    hits <- gregexpr(pat, flat, perl = TRUE)[[1]]
    p <- as.integer(hits)
    if (length(p) == 1L && p[1] == -1L) return(h)
    p <- p[p >= cursor]
    if (!length(p)) return(h)
    starts <- c(starts, p[1]); cursor <- p[1] + 1L
  }
  data.frame(section = sections, start = starts,
             end = c(starts[-1], nchar(flat) + 1L), stringsAsFactors = FALSE)
}

# --- the comparison ----------------------------------------------------------

# The pure comparison core: asserts that every expected note appears in the
# rendered text, as often as the manifest expects it and in the declared order.
# Separated from extraction so it can be tested without a PDF, and so the
# extraction step has exactly one implementation.
#
# Enforce the occurrence count on the longest segment of a note. Distinct
# notes can share the same longest segment, so use the expected manifest's
# count for that segment. Other segments can recur in tables, narrative or
# appended documents. Require each expected segment to be present in order.
# Repeating an entire note increases its longest segment's count.
#
# Order is enforced by a single forward cursor: each segment is matched at the
# first occurrence at or after the previous match, and a segment that exists
# only before the cursor fails as OUT_OF_ORDER.
#
# SECTION BINDING (round 7, finding 13). Occurrences are counted, and matches
# accepted, only inside the note's own section window - from its declared
# heading to the next declared heading. A segment that exists in the document
# but not inside the window fails as OUT_OF_SECTION; a declared heading that
# cannot be located as a line of its own fails the whole check as
# SECTION_NOT_FOUND with the heading quoted, before any note is compared. The
# anchor multiplicity k is the expected set's use of that text WITHIN the
# section, so a phrase two sections legitimately share is not over-counted.
check_semantics_text <- function(text, expected, source = "<text>") {
  if (!inherits(expected, "report_expected_notes"))
    stop("check_semantics_text() takes the manifest from ",
         "expected_report_notes()", call. = FALSE)
  flat <- .rs_flatten_document(text)

  section_order <- unique(expected$notes$section)
  located <- .rs_locate_sections(flat, section_order)
  if (is.character(located)) {
    msg <- sprintf(paste("Rendered-report semantic check FAILED on %s: the",
                         "declared section heading \"%s\" could not be located",
                         "as a line of its own in the extracted text. The",
                         "section names passed to expected_report_notes() must",
                         "be the headings exactly as the rendered document",
                         "prints them."), source, located)
    return(structure(list(
      pass = FALSE, source = source,
      n_notes = nrow(expected$notes), n_found = 0L,
      first_failure = list(status = "SECTION_NOT_FOUND", note_id = NA_integer_,
                           section = located, text = located, segment = NA_character_),
      message = msg,
      summary = sprintf("FAIL - section heading \"%s\" not located; 0 of %d expected notes verified",
                        located, nrow(expected$notes)),
      findings = data.frame(note_id = integer(0), section = character(0), note = integer(0),
                            segment = integer(0), status = character(0),
                            position = integer(0), occurrences = integer(0),
                            expected_occurrences = integer(0), stringsAsFactors = FALSE),
      tripwires = data.frame(pattern = character(0), position = integer(0),
                             context = character(0), accounted = logical(0),
                             stringsAsFactors = FALSE),
      sections = data.frame(section = section_order, start = NA_integer_,
                            end = NA_integer_, stringsAsFactors = FALSE),
      expected = expected, extractor = NA_character_,
      manifest_path = NA_character_,
      checked_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")),
      class = "report_semantic_result"))
  }
  window_of <- function(sec) located[located$section == sec, , drop = FALSE]

  # Per distinct segment text: how many times the manifest expects it, and
  # every position at which the document actually carries it (with its matched
  # length, which varies with how many line-break marks fell inside it).
  all_needles <- vapply(expected$segments$text, .rs_flatten, character(1),
                        USE.NAMES = FALSE)
  needles <- unique(all_needles)
  wanted <- new.env(parent = emptyenv()); occ <- new.env(parent = emptyenv())
  len <- new.env(parent = emptyenv())
  # expected uses of a needle within one section
  wanted_in <- function(nd, sec)
    sum(all_needles == nd & expected$segments$section == sec)
  for (nd in needles) {
    assign(nd, sum(all_needles == nd), envir = wanted)
    hits <- gregexpr(.rs_needle_pattern(nd), flat, perl = TRUE)[[1]]
    p <- as.integer(hits)
    ml <- as.integer(attr(hits, "match.length"))
    if (length(p) == 1L && p[1] == -1L) { p <- integer(0); ml <- integer(0) }
    assign(nd, p, envir = occ)
    assign(nd, ml, envir = len)
  }

  findings <- list(); spans <- list(); cursor <- 0L
  first_failure <- NULL
  current_section <- NA_character_
  for (k in seq_len(nrow(expected$notes))) {
    nrow_k <- expected$notes[k, ]
    segs <- expected$segments[expected$segments$note_id == nrow_k$note_id, ,
                              drop = FALSE]
    win <- window_of(nrow_k$section)
    if (!identical(current_section, nrow_k$section)) {
      # entering a new section: the cursor moves to its heading
      current_section <- nrow_k$section
      cursor <- max(cursor, win$start)
    }
    if (identical(nrow_k$coverage, "unverifiable")) {
      findings[[length(findings) + 1L]] <- data.frame(
        note_id = nrow_k$note_id, section = nrow_k$section, note = nrow_k$note,
        segment = NA_integer_, status = "UNVERIFIABLE", position = NA_integer_,
        occurrences = NA_integer_, expected_occurrences = NA_integer_,
        stringsAsFactors = FALSE)
      if (is.null(first_failure))
        first_failure <- list(status = "UNVERIFIABLE", note_id = nrow_k$note_id,
                              section = nrow_k$section, text = nrow_k$text,
                              segment = NA_character_)
      next
    }
    for (j in seq_len(nrow(segs))) {
      needle <- .rs_flatten(segs$text[j])
      pos_all <- get(needle, envir = occ)
      mlen_all <- get(needle, envir = len)
      # only occurrences inside the note's own section window count
      in_win <- pos_all >= win$start & pos_all < win$end
      pos <- pos_all[in_win]; mlen <- mlen_all[in_win]
      want <- wanted_in(needle, nrow_k$section)
      at <- pos[pos >= cursor]
      is_anchor <- isTRUE(segs$anchor[j])
      status <- if (!length(pos)) {
                  if (length(pos_all)) "OUT_OF_SECTION" else "MISSING"
                }
                else if (is_anchor && length(pos) > want) "REPEATED"
                else if (!length(at)) {
                  if (is_anchor && length(pos) < want) "MISSING"
                  else "OUT_OF_ORDER"
                } else "FOUND"
      idx <- if (length(at)) which(pos >= cursor)[1] else
             if (length(pos)) 1L else NA_integer_
      here <- if (is.na(idx)) NA_integer_ else pos[idx]
      hlen <- if (is.na(idx)) NA_integer_ else mlen[idx]
      findings[[length(findings) + 1L]] <- data.frame(
        note_id = nrow_k$note_id, section = nrow_k$section, note = nrow_k$note,
        segment = segs$segment[j], status = status,
        position = here, occurrences = length(pos),
        expected_occurrences = want, stringsAsFactors = FALSE)
      if (identical(status, "FOUND")) {
        spans[[length(spans) + 1L]] <- c(here, here + hlen - 1L)
        cursor <- here + hlen
      } else if (is.null(first_failure)) {
        first_failure <- list(status = status, note_id = nrow_k$note_id,
                              section = nrow_k$section, text = nrow_k$text,
                              segment = needle)
      }
    }
  }
  findings <- if (length(findings)) do.call(rbind, findings) else
    data.frame(note_id = integer(0), section = character(0), note = integer(0),
               segment = integer(0), status = character(0),
               position = integer(0), occurrences = integer(0),
               expected_occurrences = integer(0), stringsAsFactors = FALSE)

  # Tripwire. Reported only outside a matched note span: archived evidence
  # notes legitimately reproduce R expressions containing these shapes.
  tw <- list()
  for (p in .RS_TRIPWIRES) {
    m <- gregexpr(p, flat, perl = TRUE)[[1]]
    if (length(m) == 1L && m[1] == -1L) next
    for (i in seq_along(m)) {
      st <- as.integer(m[i])
      accounted <- any(vapply(spans, function(s) st >= s[1] && st <= s[2],
                              logical(1)))
      tw[[length(tw) + 1L]] <- data.frame(
        pattern = .rs_readable(p), position = st,
        context = .rs_readable(substr(flat, max(1L, st - 20L), st + 60L)),
        accounted = accounted, stringsAsFactors = FALSE)
    }
  }
  tripwires <- if (length(tw)) do.call(rbind, tw) else
    data.frame(pattern = character(0), position = integer(0),
               context = character(0), accounted = logical(0),
               stringsAsFactors = FALSE)

  ok_notes <- unique(findings$note_id[findings$status == "FOUND"])
  bad_notes <- unique(findings$note_id[findings$status != "FOUND"])
  n_found <- length(setdiff(ok_notes, bad_notes))
  leak <- sum(!tripwires$accounted)
  if (is.null(first_failure) && leak > 0L) {
    lk <- tripwires[!tripwires$accounted, ][1, ]
    first_failure <- list(status = "STRUCTURAL_LEAKAGE", note_id = NA_integer_,
                          section = NA_character_, text = lk$context,
                          segment = lk$pattern)
  }
  pass <- is.null(first_failure)

  msg <- if (pass) {
    sprintf(paste("Rendered-report semantic check PASSED on %s: %d of %d",
                  "expected notes found within their own sections (%d section",
                  "heading(s) located), anchor segments exactly once and in",
                  "order; supporting segments present; %d structural-leakage",
                  "tripwire hit(s), all accounted for by an expected note."),
            source, n_found, nrow(expected$notes), nrow(located), nrow(tripwires))
  } else if (identical(first_failure$status, "STRUCTURAL_LEAKAGE")) {
    sprintf(paste("Rendered-report semantic check FAILED on %s: structural",
                  "leakage matching /%s/ appears outside every expected note.",
                  "Context: \"%s\". A note list reached the document without",
                  "passing through note_text()."),
            source, first_failure$segment, first_failure$text)
  } else {
    sprintf(paste("Rendered-report semantic check FAILED on %s: %d of %d",
                  "expected notes verified. First failure (%s) in section",
                  "\"%s\", note %s. Expected note text: \"%s\". Comparison",
                  "segment: \"%s\"."),
            source, n_found, nrow(expected$notes), first_failure$status,
            first_failure$section, format(first_failure$note_id),
            first_failure$text, first_failure$segment)
  }

  structure(list(
    pass = pass, source = source,
    n_notes = nrow(expected$notes), n_found = n_found,
    first_failure = first_failure, message = msg,
    summary = sprintf("%s - %d of %d expected notes found within their own sections (anchor segments exactly once and in order; supporting segments present); %d unaccounted structural-leakage hit(s)",
                      if (pass) "PASS" else "FAIL", n_found,
                      nrow(expected$notes), leak),
    findings = findings, tripwires = tripwires, sections = located,
    expected = expected, extractor = NA_character_,
    manifest_path = NA_character_,
    checked_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")),
    class = "report_semantic_result")
}

# Default manifest location: beside the PDF, named after it.
report_semantic_manifest_path <- function(pdf) {
  paste0(sub("\\.[Pp][Dd][Ff]$", "", pdf), "-semantic-manifest.txt")
}

# Writes the audit manifest: settings, every expected note, its comparison
# segments, and where each was found.
write_report_semantic_manifest <- function(result, path) {
  e <- result$expected
  f <- result$findings
  L <- c(
    "# Rendered-report semantic check manifest",
    "# generated-by: tools/report_semantic.R",
    paste0("# checked-at: ", result$checked_at),
    paste0("# source: ", result$source),
    paste0("# extractor: ", result$extractor),
    "# normalization: end-of-line hyphenation repaired; every other line break marked (a break may stand for a space or for nothing, e.g. an \\allowbreak after a dollar sign); runs of horizontal whitespace collapsed to one space; nothing else rewritten",
    paste0("# segmentation: maximal runs of typographically stable characters [",
           gsub("[\t\n\r]", "", e$settings$stable_characters),
           "]; minimum ", e$settings$min_segment_chars,
           " characters (fallback: longest run, floor ",
           e$settings$hard_floor, ")"),
    "# occurrence rule: anchor segments exactly once and in order; supporting segments present and in order. Each note is matched only inside its own section window (from its declared heading to the next); the anchor count is enforced against the number of times the expected set itself uses that text within the section",
    paste0("# tripwires (not the proof): ",
           paste(.rs_readable(e$settings$tripwires), collapse = "  ")),
    paste0("# result: ", if (isTRUE(result$pass)) "PASS" else "FAIL"),
    paste0("# notes-expected: ", result$n_notes),
    paste0("# notes-found: ", result$n_found),
    paste0("# tripwire-hits: ", nrow(result$tripwires),
           " (unaccounted: ", sum(!result$tripwires$accounted), ")"),
    paste0("# message: ", result$message),
    "")
  if (!is.null(result$sections) && nrow(result$sections)) {
    L <- c(L, "# section windows (position of the heading line in the flattened extraction)")
    for (i in seq_len(nrow(result$sections)))
      L <- c(L, paste0("section-heading: \"", result$sections$section[i], "\" start=",
                       result$sections$start[i], " end=", result$sections$end[i]))
    L <- c(L, "")
  }
  for (k in seq_len(nrow(e$notes))) {
    nk <- e$notes[k, ]
    fk <- f[f$note_id == nk$note_id, , drop = FALSE]
    status <- if (!nrow(fk)) "UNVERIFIABLE"
              else if (all(fk$status == "FOUND")) "FOUND"
              else paste(unique(fk$status[fk$status != "FOUND"]), collapse = ",")
    L <- c(L,
      paste0("section: ", nk$section),
      paste0("note: ", nk$note, " (id ", nk$note_id, ")"),
      paste0("coverage: ", nk$coverage),
      paste0("status: ", status),
      paste0("first-position: ",
             if (nrow(fk) && !all(is.na(fk$position))) min(fk$position, na.rm = TRUE) else "-"),
      paste0("text: ", gsub("[\r\n]+", " ", nk$text)))
    # One line per comparison segment. The loop body is braced: an unbraced
    # `for` here once bound only the `sel` assignment to the loop, so the
    # manifest carried a single (the last) segment per note (round 7,
    # finding 13). tools/tests/test_report_semantic.R B31-B33 pin the count.
    for (j in seq_len(nrow(fk))) {
      sel <- e$segments$note_id == nk$note_id &
             e$segments$segment == fk$segment[j]
      if (!any(sel)) next            # an UNVERIFIABLE note has no segments
      L <- c(L, paste0("  segment ", fk$segment[j], ": status=", fk$status[j],
                       " pos=", fk$position[j], " occurrences=",
                       fk$occurrences[j],
                       if (isTRUE(e$segments$anchor[sel]))
                         paste0(" anchor=TRUE expected-occurrences=",
                                fk$expected_occurrences[j])
                       else " anchor=FALSE",
                       " text=\"", e$segments$text[sel], "\""))
    }
    L <- c(L, "")
  }
  if (nrow(result$tripwires)) {
    L <- c(L, "# structural-leakage tripwire hits")
    for (i in seq_len(nrow(result$tripwires)))
      L <- c(L, paste0("tripwire: /", result$tripwires$pattern[i], "/ pos=",
                       result$tripwires$position[i], " accounted=",
                       result$tripwires$accounted[i], " context=\"",
                       result$tripwires$context[i], "\""))
    L <- c(L, "")
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(L, path)
  invisible(path)
}

# The manifest's path from a typed result, for the reporting task module to
# declare as a `format = "file"` target so the semantic manifest is a tracked,
# digested pipeline artifact (round 7, finding 13). Stops unless the argument
# is a report_semantic_result whose manifest exists on disk; the module's
# one-line declaration is
#   targets::tar_target_raw("report_semantic_manifest",
#                           quote(report_semantic_manifest_file(report_semantic_check)),
#                           format = "file")
report_semantic_manifest_file <- function(result) {
  if (!inherits(result, "report_semantic_result"))
    stop("report_semantic_manifest_file() takes a report_semantic_result ",
         "(the value of the report_semantic_check target)", call. = FALSE)
  p <- result$manifest_path
  if (is.null(p) || length(p) != 1L || is.na(p) || !nzchar(p))
    stop("the semantic result carries no manifest path; the manifest is written ",
         "by check_report_semantics(), not by the pure comparison", call. = FALSE)
  if (!file.exists(p))
    stop("the semantic manifest ", p, " does not exist on disk", call. = FALSE)
  p
}

# Extracts the PDF, runs the comparison, writes the manifest, returns the typed
# result. Does NOT stop on failure — assert_report_semantics() does that, so a
# caller can inspect the result and the manifest exists either way.
check_report_semantics <- function(pdf, expected, manifest_path = NULL,
                                   which = Sys.which) {
  bin <- report_semantic_extractor(which)
  text <- report_pdf_text(pdf, which)
  res <- check_semantics_text(text, expected, source = pdf)
  res$extractor <- paste(bin, "-layout")
  res$manifest_path <- if (is.null(manifest_path))
    report_semantic_manifest_path(pdf) else manifest_path
  write_report_semantic_manifest(res, res$manifest_path)
  res
}

# Pipeline entry point: check, and stop with the quoted first failure if the
# rendered report does not carry what it was supposed to carry.
assert_report_semantics <- function(pdf, expected, manifest_path = NULL,
                                    which = Sys.which) {
  res <- check_report_semantics(pdf, expected, manifest_path, which)
  if (!isTRUE(res$pass))
    stop(res$message, "\nManifest: ", res$manifest_path, call. = FALSE)
  res
}

print.report_semantic_result <- function(x, ...) {
  cat(x$message, "\n")
  if (!is.na(x$manifest_path)) cat("Manifest:", x$manifest_path, "\n")
  invisible(x)
}
