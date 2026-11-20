# Method manifests, schema v2 (Layer 2 self-gate; integrity batches 2 + 3a).
#
# WHY THIS EXISTS -----------------------------------------------------------
# The original self-gate for validated reference code was a grep of the
# skill's SKILL.md for a non-empty "Reviewed by:" line. The external
# architecture review (finding C3/M3) showed that gate is hollow: SKILL.md is
# mutable, so any non-empty name unlocked the method and nothing bound the
# signature to the *content* of the reference code it blessed. Batch 2 replaced
# the bare signature with a content binding: a MANIFEST.yaml recording a sha256
# per reference file plus an approver line the SME fills to sign.
#
# WHAT SCHEMA v2 ADDS (integrity batch 3a, item 2) ---------------------------
# The batch-2 manifest was FLAT: one approver + one sha256 per reference file
# authorized the whole file. But a signed scope is often NARROWER than a file:
# psn-uncertainty's signed scope is bootstrap-only while the file also exposes
# sir and llp; psn-simeval's is vpc-only while the file exposes npc and sse. A
# flat approval formalized OVERBROAD authorization. Schema v2 makes the unit of
# approval an INTENDED-USE UNIT, not a file:
#
#   skill:        <name>
#   generated_at: <UTC stamp>
#   reviewed_by:  ""          (the local reviewer enters their name)
#   reviewed_at:  "<date>"
#   units:
#     <unit>:
#       status:         approved | draft | rejected
#       reference_path: skills/<skill>/reference/<file>.R
#       entrypoints:    [fn1, fn2, ...]     (the public functions this unit gates)
#       sha256:         sha256:<hex>        (digest of reference_path)
#       evidence:       <pointer to the signed record>
#       reason:         "<why this status>"
#
# GATE SEMANTIC (require_validated_method) -----------------------------------
#   * root reviewed_by NON-EMPTY (post-cutover): unit statuses are load-bearing.
#       approved + hash match      -> TRUE
#       draft / rejected           -> FALSE (the domain self-gate then issues
#                                     its own intentional STOP)
#   * root reviewed_by EMPTY (unconfirmed): every unit is treated as draft ->
#       FALSE, with an awaiting-approval warning. The batch-2 transitional
#       fallback to a legacy "Reviewed by:" grep over SKILL*.md was RETIRED
#       (batch 3c, 2026-09-01) once every real manifest carried a filled
#       reviewed_by: the grep was a footgun — any signed SKILL*.md in the
#       directory could vouch for an unrelated draft unit.
#   * HARD STOP (integrity condition, never a plain FALSE) on: missing manifest,
#       malformed manifest, missing unit, or a hash drift of the reference file
#       against its recorded digest (an edited method).
#
# The manifest is protected by the write-protection hook once its root
# reviewed_by is non-empty (see .claude/hooks/protect_paths.py), the same class
# as signed reference code.
#
# STRICT VALIDATION (Codex round 7, finding 5) -------------------------------
# Parsing and validation used to be documentary: `skill`, `reference_path`,
# `entrypoints` and `reviewed_at` were read but never checked, and duplicate
# root fields or unit names were silently accepted (last one won). The reader
# is now strict (duplicate or unknown fields at either level are an error) and
# validate_method_manifest() enforces identity and content:
#   * `skill` equals the manifest's directory name;
#   * every `reference_path` exists and lies under skills/<skill>/;
#   * for an .R reference, every entry point is a top-level function defined
#     in that file (the file is parsed, `name <- function` / `name = function`
#     assignments collected); for a non-R reference (a control-stream
#     template) the single entry point is the file's own name, which is the
#     convention skills/reference-models declares for templates adopted by copy;
#   * `sha256` has the sha256:<64 lowercase hex> grammar and equals the file;
#   * `reviewed_at` is an ISO calendar date once `reviewed_by` is filled;
#   * a public entry point belongs to exactly one unit;
#   * an optional per-unit `depends:` list of {path, sha256} pairs names code
#     OUTSIDE the skill that the unit's method relies on (e.g. a shared
#     record-selection helper under tools/); each must exist and match, and
#     require_validated_method() checks them on every call so an edit to a
#     dependency is a hard stop exactly like an edit to the reference itself.
#       depends:
#         - path: tools/record_selection.R
#           sha256: sha256:<hex>

# --- hashing ---------------------------------------------------------------
# sha256 of a file's bytes, prefixed "sha256:". digest is a light, ubiquitous
# dependency already pulled in transitively; the prefix makes the algorithm
# explicit in every recorded and rendered hash.
.mm_file_sha256 <- function(path) {
  paste0("sha256:", digest::digest(file = path, algo = "sha256"))
}

# Repo-relative POSIX path when the file is under getwd(), else the path as
# given. Reference-file keys in the manifest are repo-relative so they match
# the constants the reference code passes to require_validated_method().
.mm_relpath <- function(path) {
  root <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  real <- normalizePath(path, winslash = "/", mustWork = FALSE)
  pref <- paste0(root, "/")
  if (startsWith(real, pref)) substring(real, nchar(pref) + 1L) else path
}

# --- manifest reader (base R, no yaml dependency) --------------------------
# Parses the v2 shape emitted by write_method_manifest() with base R, so the
# gate stays runnable inside dependency-light reference code. Returns
# list(skill, generated_at, reviewed_by, reviewed_at, units) where units is a
# named list; each unit is list(status, reference_path, entrypoints (chr),
# sha256, evidence, reason, depends (list of list(path, sha256))).
#
# STRICT (round 7, finding 5): a duplicate root field, an unknown root field, a
# duplicate unit name, a duplicate or unknown unit field, a malformed line
# inside the units block, or a missing units block is an ERROR, never a silent
# default. A manifest that does not parse is malformed, and a malformed
# manifest is an integrity stop for the gate.
.MM_ROOT_FIELDS <- c("skill", "generated_at", "reviewed_by", "reviewed_at", "units")
.MM_UNIT_FIELDS <- c("status", "reference_path", "entrypoints", "sha256",
                     "evidence", "reason", "depends")
.MM_UNIT_REQUIRED <- c("status", "reference_path", "entrypoints", "sha256",
                       "evidence", "reason")
.mm_unquote <- function(v) trimws(gsub('^"|"$', "", trimws(v)))


# ONE GRAMMAR, BOTH READERS (integrity batch 14, Codex round 13 R13-P2-2).
# .claude/hooks/protect_paths.py decides which tools/*.R files a manifest
# protects; this file decides which method a gate authorizes. They must agree
# on what a manifest SAYS, and general YAML does not give that: PyYAML decodes
# escapes in a double-quoted scalar while the R reader only strips the quotes,
# so `path: "tools/site\\helper.R"` named two different (both legal) Unix
# files to the two readers. The accepted language is therefore restricted to
# what both readers lex identically, and every scalar is checked BEFORE any
# normalization. Refused on both sides: a backslash anywhere in a scalar, a
# YAML anchor or alias, a merge key, a tag, and an inline sequence or mapping
# (the bracketed `entrypoints:` list is the one exception, lexed by
# parse_list() here and by the same rule there).
.MM_SCALAR_REFUSALS <- list(
  list(re = "\\\\", why = "a backslash: PyYAML decodes escapes inside a double-quoted scalar and this reader does not, so the two readers would name different files"),
  list(re = "^[&*]",   why = "a YAML anchor or alias, which this reader does not resolve"),
  list(re = "^!",      why = "a YAML tag, which this reader does not interpret"),
  list(re = "^[[{]",   why = "an inline sequence or mapping; only the bracketed entrypoints: list is inline"),
  # A `#` ANYWHERE IN A SCALAR (integrity batch 15, Codex round 14 R14-P1-4).
  # PyYAML lexes ` #` as the start of a comment and this reader keeps it as part
  # of the value, so `- path: tools/site # helper.R` -- a legal Unix filename --
  # named `tools/site # helper.R` to the R reader and `tools/site` to the write
  # guard, and the guard's textual cross-check (which excludes whitespace) did
  # not see the line either. The real `.R` dependency was then absent from the
  # guard's protection set. The character is refused outright in both readers
  # rather than assigned a meaning; no shipped manifest carries one in a value.
  list(re = "#", why = "a '#': PyYAML lexes it as the start of a comment and this reader keeps it in the value, so the two readers would name different files"),
  # A LEADING SINGLE QUOTE (integrity batch 16, Codex round 15 R15-P2-4).
  # PyYAML strips single quotes and .mm_unquote() removes double quotes only,
  # so `path: \'tools/x.R\'` denoted tools/x.R to
  # .claude/hooks/protect_paths.py and "\'tools/x.R\'" here -- one document,
  # two strings. Rather than teach this reader a second quoting style, which
  # would widen the shared grammar, the spelling is refused on both sides: a
  # scalar is written bare or double-quoted. No shipped manifest uses one.
  list(re = "^\'", why = "a leading single quote: PyYAML strips single quotes and this reader does not, so the two readers would name different strings; write the scalar bare or double-quoted"))
.mm_check_scalar <- function(v, what, bad) {
  raw <- trimws(v)
  for (r in .MM_SCALAR_REFUSALS)
    if (grepl(r$re, raw)) bad(what, " contains ", r$why, " ('", raw, "')")
  raw
}

read_method_manifest <- function(manifest_path) {
  txt <- readLines(manifest_path, warn = FALSE)
  bad <- function(...) stop("method manifest ", manifest_path, ": ", ...,
                            call. = FALSE)
  parse_list <- function(v) {
    v <- trimws(v)
    if (!grepl("^\\[.*\\]$", v)) bad("entrypoints must be a bracketed list, got '", v, "'")
    v <- sub("^\\[", "", sub("\\]$", "", v))
    if (!nzchar(trimws(v))) return(character(0))
    out <- trimws(strsplit(v, ",")[[1]])
    if (any(!nzchar(out))) bad("entrypoints list has an empty element")
    # THE LIST ELEMENTS ARE SCALARS TOO (integrity batch 15, R14-P1-4). Both
    # readers used to bypass their scalar refusals for the bracketed
    # entrypoints: list -- this one by splitting on commas, the write guard by
    # skipping the line -- so a YAML anchor, alias, tag or nested collection
    # inside the list was not refused as prohibited grammar by either. An
    # entry point is an R function name; nothing else belongs in the list.
    # Character membership rather than a bracket expression: `[` and `]` inside a
    # POSIX bracket expression are their own escaping problem, and this list must
    # mean exactly the characters it names.
    ep_bad_chars <- c("&", "*", "!", "{", "}", "[", "]", "#", "\\")
    bad_el <- out[vapply(out, function(x)
      any(strsplit(x, "")[[1]] %in% ep_bad_chars), logical(1))]
    if (length(bad_el))
      bad("entrypoints list element '", bad_el[1], "' carries YAML punctuation ",
          "(&, *, !, {, [, # or a backslash). An entry point is an R function ",
          "name; the two readers do not lex those characters the same way.")
    out
  }

  root <- list(); units <- list()
  cur_name <- NULL; cur <- NULL; cur_seen <- character(0)
  in_units <- FALSE; in_depends <- FALSE; dep <- NULL
  flush_dep <- function() {
    if (is.null(dep)) return(invisible(NULL))
    for (f in c("path", "sha256")) {
      if (is.null(dep[[f]]) || !nzchar(dep[[f]]))
        bad("unit '", cur_name, "' has a depends entry with a missing '", f, "' field")
    }
    cur$depends[[length(cur$depends) + 1L]] <<- dep
    dep <<- NULL
  }
  flush_unit <- function() {
    flush_dep()
    if (is.null(cur_name)) return(invisible(NULL))
    miss <- setdiff(.MM_UNIT_REQUIRED, cur_seen)
    if (length(miss))
      bad("unit '", cur_name, "' is missing field '", miss[1], "'")
    units[[cur_name]] <<- cur
    cur_name <<- NULL; cur <<- NULL; cur_seen <<- character(0)
    in_depends <<- FALSE
  }
  for (i in seq_along(txt)) {
    ln <- txt[i]
    if (grepl("^\\s*#", ln) || !nzchar(trimws(ln))) next
    if (!in_units) {
      m <- regmatches(ln, regexec("^([A-Za-z_][A-Za-z0-9_]*):\\s*(.*)$", ln))[[1]]
      if (length(m) != 3) bad("cannot parse line ", i, ": '", ln, "'")
      key <- m[2]
      if (!key %in% .MM_ROOT_FIELDS) bad("unknown root field '", key, "'")
      if (key %in% names(root) || (identical(key, "units") && in_units))
        bad("duplicate root field '", key, "'")
      if (identical(key, "units")) {
        if (nzchar(trimws(m[3]))) bad("units: must open a block, got '", ln, "'")
        root$units <- TRUE; in_units <- TRUE
      } else root[[key]] <- .mm_unquote(.mm_check_scalar(m[3], paste0("root field '", key, "'"), bad))
      next
    }
    # inside the units block
    if (grepl("^\\S", ln)) {
      # dedent to column 0: another root field after the units block
      flush_unit(); in_units <- FALSE
      m <- regmatches(ln, regexec("^([A-Za-z_][A-Za-z0-9_]*):\\s*(.*)$", ln))[[1]]
      if (length(m) != 3) bad("cannot parse line ", i, ": '", ln, "'")
      key <- m[2]
      if (!key %in% .MM_ROOT_FIELDS) bad("unknown root field '", key, "'")
      if (key %in% names(root)) bad("duplicate root field '", key, "'")
      root[[key]] <- .mm_unquote(.mm_check_scalar(m[3], paste0("root field '", key, "'"), bad))
      next
    }
    m <- regmatches(ln, regexec("^  ([^ :][^:]*):\\s*$", ln))[[1]]
    if (length(m) == 2) {                       # a unit header "  <name>:"
      flush_unit()
      cur_name <- trimws(m[2])
      if (cur_name %in% names(units)) bad("duplicate unit '", cur_name, "'")
      cur <- list(status = "", reference_path = "", entrypoints = character(0),
                  sha256 = "", evidence = "", reason = "", depends = list())
      next
    }
    if (is.null(cur_name)) bad("cannot parse line ", i, " inside units: '", ln, "'")
    f <- regmatches(ln, regexec("^    ([^ :]+):\\s*(.*)$", ln))[[1]]
    if (length(f) == 3) {                       # a unit field
      flush_dep(); in_depends <- FALSE
      key <- f[2]
      if (!key %in% .MM_UNIT_FIELDS)
        bad("unknown field '", key, "' in unit '", cur_name, "'")
      if (key %in% cur_seen)
        bad("duplicate field '", key, "' in unit '", cur_name, "'")
      cur_seen <- c(cur_seen, key)
      if (identical(key, "entrypoints")) cur$entrypoints <- parse_list(f[3])
      else if (identical(key, "depends")) {
        if (nzchar(trimws(f[3]))) bad("depends: must open a block in unit '", cur_name, "'")
        in_depends <- TRUE
      } else cur[[key]] <- .mm_unquote(.mm_check_scalar(f[3], paste0("field '", key, "' of unit '", cur_name, "'"), bad))
      next
    }
    if (in_depends) {
      d1 <- regmatches(ln, regexec("^      - (path|sha256):\\s*(.*)$", ln))[[1]]
      d2 <- regmatches(ln, regexec("^        (path|sha256):\\s*(.*)$", ln))[[1]]
      if (length(d1) == 3) {
        if (!identical(d1[2], "path"))
          bad("unit '", cur_name, "': a depends entry opens with '", d1[2],
              ":'; every entry is written `- path:` first and `sha256:` second, ",
              "so both readers pair the same digest with the same file ",
              "(integrity batch 14, R13-P2-2)")
        flush_dep(); dep <- list()
        dep[[d1[2]]] <- .mm_unquote(.mm_check_scalar(d1[3], paste0("depends path of unit '", cur_name, "'"), bad))
        next
      }
      if (length(d2) == 3 && !is.null(dep)) {
        if (!is.null(dep[[d2[2]]]))
          bad("unit '", cur_name, "' has a depends entry with a duplicate '", d2[2], "' field")
        dep[[d2[2]]] <- .mm_unquote(.mm_check_scalar(d2[3], paste0("depends ", d2[2], " of unit '", cur_name, "'"), bad))
        next
      }
    }
    bad("cannot parse line ", i, " in unit '", cur_name, "': '", ln, "'")
  }
  flush_unit()
  if (!isTRUE(root$units)) bad("no units: block (not schema v2)")
  list(
    skill = root$skill %||% "",
    generated_at = root$generated_at %||% "",
    reviewed_by = root$reviewed_by %||% "",
    reviewed_at = root$reviewed_at %||% "",
    units = units
  )
}

# --- strict validation (round 7, finding 5) --------------------------------
# Top-level function definitions in an R file: `name <- function(...)` and
# `name = function(...)` at the top level only. The file is PARSED, not
# grepped, so a definition inside a comment or a string does not count and a
# nested definition is not an entry point.
.mm_top_level_functions <- function(path) {
  exprs <- tryCatch(parse(path, keep.source = FALSE), error = function(e)
    stop("reference file ", path, " does not parse: ", conditionMessage(e), call. = FALSE))
  out <- character(0)
  for (e in exprs) {
    if (!is.call(e)) next
    op <- as.character(e[[1]])[1]
    if (!op %in% c("<-", "=", "<<-") || length(e) != 3L) next
    lhs <- e[[2]]; rhs <- e[[3]]
    if (!(is.name(lhs) || is.character(lhs))) next
    if (is.call(rhs) && identical(as.character(rhs[[1]])[1], "function"))
      out <- c(out, as.character(lhs))
  }
  unique(out)
}

.mm_is_iso_date <- function(x) {
  if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x)) return(FALSE)
  d <- tryCatch(as.Date(x, format = "%Y-%m-%d"), error = function(e) NA)
  !is.na(d) && identical(format(d, "%Y-%m-%d"), x)
}

# TRUE when a comment line of the reference file carries the PLACEHOLDER
# marker (round 8, finding 1). Comment lines only: a string or identifier
# containing the word inside real code is not a stub declaration.
.mm_has_placeholder_marker <- function(path) {
  txt <- readLines(path, warn = FALSE)
  any(grepl("^\\s*#.*\\bPLACEHOLDER\\b", txt))
}

# --- entry-point gating audit (Codex round 8, finding 2) --------------------
# The manifest binds a file's BYTES and the existence of a unit; nothing so
# far proved that each declared entry point actually consults the gate before
# computing. An approved unit with an ungated entry point is an authorized
# lane with the door open. This audit is static: the reference file is parsed
# (never sourced or executed) and every declared entry point's body must call
# the skill's gate.
#
# The GATE FUNCTIONS of a reference file are derived, not declared: a
# top-level function whose body calls require_validated_method() is a gate,
# and so is any top-level function whose body calls a gate (one level of
# wrapping, e.g. `.forest_gate()` -> `forest_validated()` ->
# require_validated_method(), is the house pattern). Declared entry points are
# never gate functions themselves, so an entry point that merely calls
# another entry point does not inherit its gate. An entry point is GATED only
# when the gate is its FIRST statement (see .mm_entrypoint_gated) and, when
# the gate takes a unit literal, that literal is the entry point's own unit.
#
# Return: a data.frame with one row per (unit, entrypoint) —
#   unit, status, entrypoint, gated (logical), gate_functions (the gate set of
#   the file, collapsed), trusted_loads (every file the reference sources into
#   the sealed object that holds the gate, collapsed) — for CI reporting and
#   tests. trusted_loads is a REPORT, not a gate: the gating expression is
#   `bad <- out[!out$gated, ]` alone, and this column is never read by it. The
#   audit can make the load set statically resolvable, repository-scoped and
#   visible; only the SME's review of the diff and the unit's manifest digest
#   can say the loaded files are the intended ones. What the trust rule does
#   and does not cover is set out in full above .mm_static_path(); read that
#   before trusting a summary of it, including this one.
# Severity: an ungated entry point of an APPROVED unit is a hard stop
# (integrity condition); of a draft or rejected unit a WARNING (the unit
# cannot run anyway, but the author is told). Non-R references (control-stream
# templates) have nothing to gate and are skipped.
.mm_call_heads <- function(e) {
  acc <- character(0)
  walk <- function(x) {
    if (is.call(x)) {
      h <- x[[1]]
      if (is.name(h)) {
        acc <<- c(acc, as.character(h))
      } else if (is.call(h) && as.character(h[[1]])[1] %in% c("::", ":::", "$", "@") &&
                 (is.name(h[[3]]) || is.character(h[[3]]))) {
        # pkg::fn(...) and env$fn(...) both name the function being called
        acc <<- c(acc, as.character(h[[3]]))
        walk(h[[2]])
      } else {
        walk(h)
      }
      for (i in seq_along(x)) if (i > 1L) walk(x[[i]])
    } else if (is.pairlist(x) || is.expression(x)) {
      lst <- as.list(x)
      for (i in seq_along(lst)) {
        # a formal with no default is the empty symbol; skip it without
        # binding it (binding it makes a "missing argument")
        if (is.symbol(lst[[i]]) && !nzchar(as.character(lst[[i]]))) next
        walk(lst[[i]])
      }
    }
  }
  walk(e)
  acc
}

# Named list of top-level bindings (name -> right-hand-side expression) in a
# parsed R file; same top-level rule as .mm_top_level_functions() but every
# binding is kept, not only `function` literals: a gate may be built by
# `local({ ...; function(unit) ... })` (eda-standards) and still be a gate.
.mm_top_level_bindings <- function(path) {
  exprs <- tryCatch(parse(path, keep.source = TRUE), error = function(e)
    stop("reference file ", path, " does not parse: ", conditionMessage(e), call. = FALSE))
  out <- list()
  for (e in exprs) {
    if (!is.call(e)) next
    op <- as.character(e[[1]])[1]
    if (!op %in% c("<-", "=", "<<-") || length(e) != 3L) {
      # An INDIRECT top-level assignment binds a top-level name just as plainly
      # (fix batch 11, R10-P0-2): base::assign("sys.source", f) shadows the
      # construction primitive exactly as `sys.source <- f` does, and the
      # primitive-shadow check reads this list.
      for (an in .mm_assign_call_names(e))
        if (length(e) >= 3L) out[[an]] <- e[[3]]
      next
    }
    lhs <- e[[2]]; rhs <- e[[3]]
    if (!(is.name(lhs) || is.character(lhs))) next
    out[[as.character(lhs)]] <- rhs
  }
  attr(out, "shadow") <- .mm_top_level_shadow_census(exprs)
  out
}

# --- the top-level shadow census (fix round 2 of batch 11, review F1-a) ------
#
# WHY THIS IS SEPARATE FROM THE MAP ABOVE. .mm_top_level_bindings() answers
# "what is bound to this top-level name?" and its value feeds the gate-chain
# trace, which needs exactly one right-hand side per name. This census answers
# a different question — "which names could this file have put in front of the
# sealed object's construction?" — and for that a name with no resolvable
# right-hand side still counts. The two must not be merged: widening the map
# would hand the tracer function-local right-hand sides it has no business
# following.
#
# WHAT THE PREVIOUS ROUND MISSED. The map matched each top-level statement AT
# ITS ROOT, so any wrapper hid the binder from it, and .mm_assign_call_names()
# returned the sentinel "<computed>" for a non-literal name, which then failed
# the membership test against MM_TRUST_CONSTRUCTION_PRIMITIVES exactly as a
# harmless name would. All three of these were proven at runtime to load a fake
# validator into the sealed object while the audit reported gated = TRUE:
#
#   if (TRUE) delayedAssign("sys.source", <fake>)
#   delayedAssign(paste0("sys.", "source"), <fake>)
#   makeActiveBinding(as.name(paste0("sys.", "source")), <fake>, environment())
#
# THE RULE. Walk the WHOLE expression tree of every top-level statement.
#   - `<-` / `=` / `<<-`: the base symbol of the target is bound. Inside a
#     function literal a plain `<-` / `=` is provably frame-local and is NOT
#     collected; `<<-` is, because it reaches outward.
#   - a `for` index binds in the frame the loop runs in, so it is collected
#     outside a function literal.
#   - assign / do.call / delayedAssign / makeActiveBinding / list2env are
#     collected EVERYWHERE, function literals included: their envir /
#     assign.env argument may name a caller frame, so being written inside a
#     function literal proves nothing about where they bind.
#   - a name that is not a static literal is recorded as "<computed>", which
#     the trust predicate treats as shadowing EVERY construction primitive and
#     the validator: the audit cannot rule out any name, so it rules out none.
#
# KNOWN BOUNDARY, stated in terms of what this census can SEE. It sees static
# bindings (`<-`, `=`, `<<-`, `for` indices) and binder CALLS whose head it can
# read as a name -- directly, through one `(`, or through one alias hop.
#
# Since fix round 4 of batch 11 this census is no longer the only rule over a
# reference file's top level. MM_REFLECTIVE_TOP_LEVEL and
# .mm_reflective_top_level_violations() refuse, outright and by name, any
# top-level statement that evaluates text, reaches a function reflectively, or
# calls a value whose head is not a bare or `::`-qualified symbol. Round 4
# closed five of the six cases the round-2 review proved open here -- `f <-
# get("assign")` (b3), `f <- match.fun("assign")` (b4), `list(a = assign)$a(...)`
# (b5), `(function() assign)()(...)` (b6), `eval(parse(text = ...))` (c4) --
# and, once psn_covsearch.R stopped using the form, `source("x.R", local =
# TRUE)` (c5) and `sys.source("x.R", envir = environment())` (c6) as well.
# Round 5 (fifth review, P1) closed the six top-level spellings that round had
# left accepted -- `do.call("source", ...)` with a literal what; `source` /
# `sys.source` passed as a VALUE to `Reduce`, `lapply`, `sapply`, `Map` (any
# call); a function literal handed as a DIRECT argument to a callee whose body
# sources -- and (fifth review, I4) the functional replacement spelling
# `` `parent.env<-`(.cache, e) ``, which now counts as a rebinding of `.cache`
# exactly as `parent.env(.cache) <- e` does. A top-level alias of a reflective
# name (`f <- source`) is refused by the same argument-position rule, so a
# chained alias of a REFLECTIVE function is closed too (scratchpad fixture
# f5/open.R, case z2).
#
# A binder whose BOUND NAME is computed (`nm <- paste0("sys.", "source");
# assign(nm, <fake>)`) is REFUSED and always was: the census records it as
# "<computed>", which shadows every construction primitive (f5/open.R, z1).
#
# Round 6 (sixth review, K1) closed three more spellings of the same
# argument-position class, each proven a runtime bypass in rev11f/attack.R:
# a CHARACTER literal equal to a listed name in argument position
# (`lapply(files, "source")`, `Map("sys.source", files, ...)`, and the same
# string written as a bound name into a private cache); a function literal
# wrapped in `(` in argument position (`Reduce((function(a, f) source(f)),
# files)`); and a function literal wrapped in `local()`, `identity()`,
# `invisible()`, `force()` or a single-expression `{` in argument position
# (`Reduce(local(function(a, f) source(f)), files)`). Statement-level
# definitions -- `x <- local(...)`, a bare `local({ ... })` block that only
# assigns -- stay exempt.
#
# WHAT THIS RULE IS. The top-level reflective rule is a BLACKLIST of named
# spellings over a free-form top level. It closes the spellings it names and
# nothing more: it does not close the class of "a load-time act that reaches
# a fake validator", and it cannot, because R lets a reflective act be
# spelled through any callee, any wrapper and any alias hop. Reviews have
# found a new accepted spelling in rounds 2, 4, 5 and 6; more are expected to
# be found, and each is added here when it is. No claim of completeness is
# made. What holds the class is not this rule but the controls around it: the
# PreToolUse write guard on skills/**/reference/*.R, the .harness-unlock
# window, the per-unit manifest digest that any edit to a reference file
# breaks, and the SME's review of the diff at approval time.
#
# WHAT IS KNOWN OPEN after round 6, each re-proven at runtime rather than
# asserted (scratchpad fixture f5/open.R against
# rev11c/runtime_probe.R, 2026-09-04: every case below returns 42, i.e. a fake
# validator really is reached, while the audit reports gated = TRUE):
#
#   (o1, a chained alias of a binder at the top level -- `f <- assign; g <- f;
#       g("sys.source", mkfake())` -- was open through round 5 and is CLOSED
#       by the round-6 string rule: the bound name is a listed name passed as
#       a character literal in argument position, and the computed-name
#       variant is refused as a computed argument. Re-proven refused in
#       rev11g/o1_reason.R, 2026-09-04. The two-hop alias itself is still not
#       resolved; it is harmless only while the name it binds is caught.)
#   o2  text evaluated as code inside the body of a NAMED function that the
#       file then calls:
#         boot <- function() eval(parse(text = ...), envir = parent.frame()); boot()
#   o3  a computed CALL HEAD inside such a body:
#         boot <- function() get("assign")("sys.source", <fake>, envir = parent.frame()); boot()
#   o4  another file loaded into a caller frame from inside such a body:
#         boot <- function() source("x.R", local = parent.frame()); boot()
#       in any of its bindings: a top-level `<-` (o4), a `<-` inside a
#       `local({ ... })` block that then calls it (o4b), or a `<-` inside an
#       `if` (o4c).
#
# (A multi-statement `local({ ... })` / `{ ... }` block in ARGUMENT position --
# `Reduce(local({ k <- 1; function(a, f) source(f) }), files)`, runtime-proven
# a bypass in f6/open6.R -- was closed by the round-6 addendum: the block is
# evaluated at load time, so every statement in it is walked names-only.)
#
# o2-o4 are the same act moved one layer down: the reflective rule
# deliberately does not descend into a function literal that is being DEFINED
# (the right-hand side of an assignment, or the value of a `{` / `(` / `local`
# block at statement level), because such a body is a definition rather than
# load-time code and is judged by the gate-chain rules; it descends into a
# literal handed to a callee -- bare or wrapped -- because the callee may run
# it at load time. These three are the cases KNOWN open; they are not the only
# ones open.
#
# Names bound in the census's frame by a top-level `f <- assign`-style alias:
# a plain binding whose right-hand side is a bare (possibly namespace-qualified)
# symbol naming one of MM_ASSIGN_CALL_BINDERS. Collected with the same scope
# rule as the census itself -- everywhere except a plain assignment inside a
# function literal, which is frame-local. One hop only: `g <- f; f <- assign`
# is not resolved, and neither is an alias reached through a list element or a
# computed name. Those are the documented boundary.
.mm_binder_aliases <- function(exprs) {
  out <- character(0)
  scan <- function(x, in_fn) {
    if (is.pairlist(x) || is.expression(x)) {
      lst <- as.list(x)
      for (el in lst) {
        if (is.symbol(el) && !nzchar(as.character(el))) next
        scan(el, in_fn)
      }
      return(invisible(NULL))
    }
    if (!is.call(x)) return(invisible(NULL))
    op <- as.character(x[[1]])[1]
    if (identical(op, "function") && length(x) >= 3L) {
      scan(x[[2]], in_fn); scan(x[[3]], TRUE)
      return(invisible(NULL))
    }
    if (op %in% c("<-", "=", "<<-") && length(x) == 3L) {
      if ((!in_fn || identical(op, "<<-")) && is.name(x[[2]])) {
        rhs <- x[[3]]
        nm <- if (is.name(rhs)) as.character(rhs)
              else if (is.call(rhs) && length(rhs) == 3L &&
                       as.character(rhs[[1]])[1] %in% c("::", ":::") &&
                       is.name(rhs[[3]])) as.character(rhs[[3]])
              else NA_character_
        if (!is.na(nm) && .mm_unqualify(nm) %in% MM_ASSIGN_CALL_BINDERS)
          out[[as.character(x[[2]])]] <<- .mm_unqualify(nm)
      }
      scan(x[[3]], in_fn)
      return(invisible(NULL))
    }
    for (i in seq_along(x)) {
      el <- tryCatch(x[[i]], error = function(e) NULL)
      if (!is.null(el)) tryCatch(scan(el, in_fn), error = function(e) NULL)
    }
    invisible(NULL)
  }
  for (e in exprs) tryCatch(scan(e, FALSE), error = function(err) NULL)
  out
}

# .mm_assign_call_names() with the call head normalised first: `(` peeled off
# the head, so `(assign)("sys.source", f)` is read as the assign() it is, and a
# known alias resolved to the binder it holds.
.mm_binder_call_names <- function(x, aliases = character(0)) {
  if (!is.call(x)) return(character(0))
  h <- x[[1]]
  while (is.call(h) && identical(as.character(h[[1]])[1], "(") && length(h) == 2L)
    h <- h[[2]]
  nm <- if (is.name(h)) as.character(h) else NA_character_
  if (!is.na(nm) && nm %in% names(aliases)) nm <- unname(aliases[[nm]])
  if (!is.na(nm)) {
    y <- x
    y[[1]] <- as.name(nm)
    return(.mm_assign_call_names(y))
  }
  .mm_assign_call_names(x)
}

# Every name this file binds, anywhere, by any form, with how many times: a
# tree-walk over all layers (function literal bodies included, because the
# question here is not "where does this bind" but "is this name bound more than
# once"), counting `<-` / `=` / `<<-` targets, `for` indices and the names of
# binder calls. Names the audit cannot resolve to a literal are not counted --
# they are already refused wholesale by .mm_top_level_shadow_violations(), which
# treats "<computed>" as shadowing every guarded name.
.mm_all_binding_counts <- function(exprs) {
  acc <- character(0)
  walk <- function(x) {
    if (is.pairlist(x) || is.expression(x)) {
      for (el in as.list(x)) {
        if (is.symbol(el) && !nzchar(as.character(el))) next
        tryCatch(walk(el), error = function(e) NULL)
      }
      return(invisible(NULL))
    }
    if (!is.call(x)) return(invisible(NULL))
    op <- as.character(x[[1]])[1]
    if (op %in% c("<-", "=", "<<-") && length(x) == 3L) {
      b <- tryCatch(.mm_assign_base_symbol(x[[2]]), error = function(e) NA_character_)
      if (!is.na(b)) acc <<- c(acc, b)
    } else if (identical(op, "for") && length(x) == 4L && is.name(x[[2]])) {
      acc <<- c(acc, as.character(x[[2]]))
    } else {
      # fix round 5 of batch 11: `` `f<-`(x, ...) `` rebinds x (see the helper)
      fr <- .mm_functional_replacement_base(x)
      if (!is.na(fr)) acc <<- c(acc, fr)
      an <- tryCatch(.mm_assign_call_names(x), error = function(e) character(0))
      acc <<- c(acc, an[!is.na(an) & nzchar(an) & an != "<computed>"])
    }
    for (i in seq_along(x)) {
      el <- tryCatch(x[[i]], error = function(e) NULL)
      if (!is.null(el)) tryCatch(walk(el), error = function(e) NULL)
    }
    invisible(NULL)
  }
  for (e in exprs) tryCatch(walk(e), error = function(err) NULL)
  table(acc)
}

# Is `x` the exact construction of a private, empty, statically parented
# environment -- `new.env(parent = emptyenv())` or `new.env(parent = baseenv())`
# with bare-symbol heads throughout and no other argument?
.mm_is_private_fresh_env_call <- function(x) {
  if (!.mm_is_bare_call_to(x, "new.env")) return(FALSE)
  a <- as.list(x)[-1]
  if (length(a) != 1L) return(FALSE)
  anm <- names(a)
  if (is.null(anm) || !identical(anm[1], "parent")) return(FALSE)
  p <- a[[1]]
  (.mm_is_bare_call_to(p, "emptyenv") || .mm_is_bare_call_to(p, "baseenv")) &&
    length(as.list(p)) == 1L
}

# Names bound to a FRESH new.env(): a binder call whose target environment is
# one of these provably writes into that object and not into the file's top
# level, so it cannot shadow the trusted construction. eda.R's
# `assign(key, d, envir = .eda_cache)` is the shipped case.
#
# Fix round 3 of batch 11 (review G1-a). The first version kept every symbol
# that appeared at root level as `X <- new.env(...)`, unconditionally and by
# head NAME. Three lines defeated it:
#
#   .cache <- new.env(parent = emptyenv())    # puts .cache in the fresh set
#   .cache <- environment()                   # ... and now it is the top level
#   if (TRUE) assign("sys.source", <fake>, envir = .cache)
#
# The census dropped the third statement because `.cache` was in the set, and
# the entry point then ran against a fake validator while the audit reported
# gated = TRUE. The exclusion now applies ONLY to a symbol that this file binds
# EXACTLY ONCE, anywhere, by any form, and whose single binding is a root-level
# `X <- new.env(parent = emptyenv())` or `X <- new.env(parent = baseenv())`
# written with bare-symbol heads. A cache that is genuinely private is bound
# once; a symbol that is bound twice is not an object the audit can reason
# about, so it is left in the census and judged there.
#
# WHICH CACHE SPELLING IS SUPPORTED, for the next reference-file author (fourth
# review, P3). "Bound once" counts `$` and `[[` element writes too, so a file
# that writes `.cache$key <- value` anywhere counts a second binding of `.cache`
# and loses this exclusion -- which then cascades into a refusal if the file
# also does a computed-name `assign` into the cache. The supported spelling is
# eda.R's: bind the cache once at root level and read and write it with
# `.cache[[key]]` / `assign(key, value, envir = .cache)`. Since fix round 4 the
# count also sees replacement forms (`parent.env(.cache) <- e`), so those are a
# second binding as well.
.mm_fresh_env_names <- function(exprs) {
  counts <- tryCatch(.mm_all_binding_counts(exprs), error = function(e) table(character(0)))
  out <- character(0)
  for (e in exprs) {
    if (!is.call(e) || length(e) != 3L) next
    if (!(as.character(e[[1]])[1] %in% c("<-", "=", "<<-"))) next
    if (!is.name(e[[2]])) next
    nm <- as.character(e[[2]])
    if (!.mm_is_private_fresh_env_call(e[[3]])) next
    n <- if (nm %in% names(counts)) as.integer(counts[[nm]]) else 0L
    if (identical(n, 1L)) out <- c(out, nm)
  }
  unique(out)
}

# The environment a binder call writes into, as an expression, or NULL when the
# call does not name one (in which case it writes into the calling frame).
.mm_binder_target_env <- function(x, binder) {
  formals_of <- list(assign = c("x", "value", "pos", "envir"),
                     delayedAssign = c("x", "value", "eval.env", "assign.env"),
                     makeActiveBinding = c("sym", "fun", "env"),
                     list2env = c("x", "envir"))
  fm <- formals_of[[binder]]
  if (is.null(fm)) return(NULL)
  a <- tryCatch(.mm_match_args(x, fm), error = function(e) list())
  a[[fm[length(fm)]]]
}

# Does this binder call name a fresh new.env() as its target environment?
.mm_binder_writes_into_fresh_env <- function(x, aliases, fresh) {
  if (!length(fresh)) return(FALSE)
  h <- x[[1]]
  while (is.call(h) && identical(as.character(h[[1]])[1], "(") && length(h) == 2L)
    h <- h[[2]]
  nm <- if (is.name(h)) as.character(h)
        else if (!is.na(.mm_call_head_name(x))) .mm_call_head_name(x)
        else return(FALSE)
  if (nm %in% names(aliases)) nm <- unname(aliases[[nm]])
  bare <- .mm_unqualify(nm)
  # `assign(..., inherits = TRUE)` does not necessarily write into the named
  # environment: it walks that environment's parents looking for an existing
  # binding, so the write can land anywhere on the chain (fix round 3 of batch
  # 11, review G1-a secondary). The exclusion says "this write goes into a
  # private cache and nowhere else", which is only true when `inherits` is
  # absent or a literal FALSE.
  if (identical(bare, "assign")) {
    ai <- tryCatch(.mm_match_args(x, c("x", "value", "pos", "envir", "inherits")),
                   error = function(e) list())
    inh <- ai$inherits
    if (!is.null(inh) && !(is.logical(inh) && length(inh) == 1L && identical(inh, FALSE)))
      return(FALSE)
  }
  target <- tryCatch(.mm_binder_target_env(x, bare), error = function(e) NULL)
  !is.null(target) && is.name(target) && as.character(target) %in% fresh
}

.mm_top_level_shadow_census <- function(exprs) {
  fresh <- tryCatch(.mm_fresh_env_names(exprs), error = function(e) character(0))
  aliases <- tryCatch(.mm_binder_aliases(exprs), error = function(e) character(0))
  nms <- character(0); stmts <- character(0)
  add <- function(new_names, stmt) {
    new_names <- new_names[!is.na(new_names) & nzchar(new_names)]
    if (!length(new_names)) return(invisible(NULL))
    txt <- tryCatch(paste(deparse(stmt), collapse = " "),
                    error = function(e) "<unparseable>")
    if (nchar(txt) > 160L) txt <- paste0(substr(txt, 1L, 157L), "...")
    nms <<- c(nms, new_names)
    stmts <<- c(stmts, rep(txt, length(new_names)))
    invisible(NULL)
  }
  walk <- function(x, in_fn) {
    if (is.pairlist(x) || is.expression(x)) {
      lst <- as.list(x)
      for (i in seq_along(lst)) {
        el <- lst[[i]]
        # a formal with no default is the empty symbol; walking it errors
        if (is.symbol(el) && !nzchar(as.character(el))) next
        walk(el, in_fn)
      }
      return(invisible(NULL))
    }
    if (!is.call(x)) return(invisible(NULL))
    op <- as.character(x[[1]])[1]
    if (identical(op, "function") && length(x) >= 3L) {
      walk(x[[2]], in_fn)          # formals' defaults
      walk(x[[3]], TRUE)           # the body: plain assignments are frame-local
      return(invisible(NULL))
    }
    if (op %in% c("<-", "=", "<<-") && length(x) == 3L) {
      if (!in_fn || identical(op, "<<-")) {
        base <- tryCatch(.mm_assign_base_symbol(x[[2]]),
                         error = function(e) NA_character_)
        add(if (is.na(base)) "<computed>" else base, x)
        # Fix round 4 of batch 11. .mm_assign_base_symbol() now peels ANY
        # replacement target down to its base symbol, which is what the
        # bound-once count needs. This census wants the OLDER, more
        # conservative reading as well: a replacement form other than a
        # `$` / `@` / `[[` chain runs an arbitrary `f<-` whose body the
        # audit has not read, so the statement stays "<computed>" here and
        # keeps refusing exactly as it did before that peel was widened.
        lhs <- x[[2]]
        if (is.call(lhs) &&
            !(as.character(lhs[[1]])[1] %in% c("$", "@", "[[")))
          add("<computed>", x)
      }
      walk(x[[3]], in_fn)
      return(invisible(NULL))
    }
    if (identical(op, "for") && length(x) == 4L) {
      if (!in_fn)
        add(if (is.name(x[[2]])) as.character(x[[2]]) else "<computed>", x)
      walk(x[[3]], in_fn); walk(x[[4]], in_fn)
      return(invisible(NULL))
    }
    # Fix round 5 of batch 11. The functional replacement spelling
    # `` `parent.env<-`(.cache, e) `` is the `parent.env(.cache) <- e` branch
    # above written as a call; it is collected under the same scope rule (a
    # plain rebinding inside a function literal is frame-local) and, like any
    # replacement form other than a `$` / `@` / `[[` chain, it runs an `f<-`
    # whose body the audit has not read, so it is also recorded as "<computed>".
    fr <- .mm_functional_replacement_base(x)
    if (!is.na(fr) && !in_fn) {
      add(fr, x)
      if (!(.mm_unqualify(.mm_call_head_name(x)) %in% c("$<-", "@<-", "[[<-")))
        add("<computed>", x)
    }
    an <- tryCatch(.mm_binder_call_names(x, aliases),
                   error = function(e) character(0))
    if (length(an) && !.mm_binder_writes_into_fresh_env(x, aliases, fresh))
      add(an, x)
    for (i in seq_along(x)) {
      el <- tryCatch(x[[i]], error = function(e) NULL)
      if (!is.null(el)) tryCatch(walk(el, in_fn), error = function(e) NULL)
    }
    invisible(NULL)
  }
  for (e in exprs) tryCatch(walk(e, FALSE), error = function(err) NULL)
  keep <- !duplicated(nms)
  list(names = nms[keep], statements = stmts[keep])
}

# The census carried by a .mm_top_level_bindings() map, in the shape the trust
# predicate wants. Falls back to the map's plain names when the attribute is
# absent (a caller that built the map some other way).
.mm_shadow_census_of <- function(fns) {
  if (is.null(fns)) return(list(names = character(0), statements = character(0)))
  if (is.list(fns) && !is.null(fns$names) && !is.null(fns$statements)) return(fns)
  if (is.character(fns))
    return(list(names = fns, statements = rep("<not recorded>", length(fns))))
  s <- attr(fns, "shadow")
  if (is.null(s)) return(list(names = names(fns),
                              statements = rep("<not recorded>", length(fns))))
  s
}

# The names in the census that shadow the trusted construction, each with the
# statement that binds it, as refusal sentences. "<computed>" shadows
# everything.
.mm_top_level_shadow_violations <- function(census) {
  if (is.null(census) || !length(census$names)) return(character(0))
  guarded <- c(MM_TRUST_CONSTRUCTION_PRIMITIVES, MM_VALIDATOR_NAME)
  out <- character(0)
  for (i in seq_along(census$names)) {
    nm <- census$names[i]
    if (identical(nm, "<computed>")) {
      out <- c(out, sprintf("the reference file binds a top-level name that the audit cannot resolve to a literal, so it cannot rule out that the name is one of the trusted construction's primitives (%s) or the validator: `%s`",
                            paste(guarded, collapse = ", "), census$statements[i]))
    } else if (nm %in% guarded) {
      out <- c(out, sprintf("the reference file binds '%s' at its top level, which the trusted construction calls, so the construction would not do what it reads as: `%s`",
                            nm, census$statements[i]))
    }
  }
  out
}

# Reflective and evaluating forms that a reference file's TOP LEVEL may not use
# (batch 11, fix round 4, SME decision 2026-09-04). Each of them either runs
# text as code, or reaches a function through a value the audit cannot read, so
# a top-level statement containing one puts what the file does at load time
# outside anything this audit can establish -- including which validator the
# trusted construction ends up holding. `source` is on the list as of the same
# decision; the one shipped file that used it (psn_covsearch.R) now loads its
# dependencies into its own sealed environment instead. The rule is never
# relaxed to fit a shipped file: the file is rewritten.
#
# `environment<-` is NOT reachable as a call head -- `environment(f) <- e`
# parses as `<-` with `environment(f)` on the left -- so it is matched on the
# assignment's left-hand side instead (fourth review, P3, which proved the
# unreachable spelling on a probe file).
MM_REFLECTIVE_TOP_LEVEL <- c("eval", "evalq", "eval.parent", "parse", "source",
                             "sys.source", "get", "get0", "mget", "match.fun",
                             "getFunction", "Recall")

# The walk, stated so it can be checked against the code:
#   - start from each TOP-LEVEL statement of the file;
#   - skip a top-level binding whose right-hand side is a trusted block: that
#     block has its own whitelist (.mm_trusted_block_statement_violations),
#     which is a positive enumeration and strictly stronger than this list;
#   - skip the body of a function literal that is being DEFINED: the
#     right-hand side of an assignment, or the value of a `{` / `(` / `local`
#     block. Such a body does not run when the file is sourced; it is a
#     definition, and it is judged by the gate-chain rules and by the binder
#     census, both of which walk into function bodies. A literal that IS the
#     head of its own call (`(function() ...)()`) does run, and is walked;
#   - (fix round 5 of batch 11, fifth review P1) a function literal that is a
#     DIRECT argument of any other call -- `Reduce(function(acc, f) source(f),
#     ...)` -- is handed to a callee that may invoke it during the top-level
#     statement, so its body is walked with the NAME LIST and the do.call()
#     literal-`what` check only (not the computed-head rule, which would refuse
#     ordinary `$` dispatch inside such a body). The `{` / `(` / `local` /
#     assignment exemption is what keeps eda.R's gate closure
#     (`local({ private <- ...; function() ... get(...) ... })`) a definition;
#   - (same round) a listed name appearing as a bare or `::`-qualified symbol
#     in ARGUMENT position -- `lapply(files, source)`, `Map(source, files)`,
#     `sapply(files, sys.source, envir = environment())` -- is refused: at the
#     top level it can only be there to be invoked by the callee;
#   - (same round) `do.call()` whose LITERAL `what` is a listed name is refused
#     exactly as the call spelled directly would be;
#   - (fix round 6 of batch 11, sixth review P1) a length-1 CHARACTER literal
#     equal to a listed name, bare or `pkg::name`, in ARGUMENT position --
#     `lapply(files, "source")`, `Map("sys.source", files, ...)` -- is refused
#     by the same rule as the symbol: match.fun() turns it into the function.
#     The same literal inside a walked callback body is a hit too;
#   - (same round) a function literal reached through `(`, single-expression
#     `{`, `local()`, `identity()`, `invisible()` or `force()` in ARGUMENT
#     position of a non-defining head -- `Reduce((function(a, f) source(f)),
#     files)`, `Reduce(local(function(a, f) source(f)), files)` -- is walked
#     names-only like the bare literal: the wrapper evaluates to the literal
#     and the callee runs it. A MULTI-statement `local({ ... })` / `{ ... }`
#     block in argument position (round-6 addendum) is walked names-only in
#     full, since it is evaluated at load time and its last expression is
#     handed to the callee. Statement-level wrappers (`x <- local(...)`, a
#     bare `local({ ... })` block that only defines) remain definitions;
#   - descend into every other argument, at every layer.
# Every hit names the statement, so the author can act on the refusal.
.mm_reflective_top_level_violations <- function(exprs, binds = list()) {
  if (is.null(exprs)) return(character(0))
  out <- character(0)
  say <- function(kind, x) {
    txt <- tryCatch(paste(deparse(x), collapse = " "),
                    error = function(e) "<unparseable>")
    if (nchar(txt) > 160L) txt <- paste0(substr(txt, 1L, 157L), "...")
    out <<- c(out, sprintf("the reference file's top level %s, which the audit cannot follow, so what this file binds and loads at source time cannot be established: `%s`",
                           kind, txt))
    invisible(NULL)
  }
  peel <- function(h) {
    while (is.call(h) && identical(as.character(h[[1]])[1], "(") && length(h) == 2L)
      h <- h[[2]]
    h
  }
  is_fn_lit <- function(x) is.call(x) && identical(as.character(x[[1]])[1], "function")
  # The listed name a symbol, a `::`-qualified symbol, or (fix round 6 of
  # batch 11, sixth review P1) a length-1 character literal denotes, else NA.
  # The apply family and match.fun() accept a string naming a function, so
  # `lapply(files, "source")` sources exactly as `lapply(files, source)` does;
  # the string form is judged by the same rule, bare or `pkg::name`.
  listed_symbol <- function(el) {
    if (is.character(el) && length(el) == 1L && !is.na(el)) {
      s <- .mm_unqualify(el)
      return(if (nzchar(s) && s %in% MM_REFLECTIVE_TOP_LEVEL) s else NA_character_)
    }
    if (is.name(el)) {
      s <- as.character(el)
      return(if (nzchar(s) && s %in% MM_REFLECTIVE_TOP_LEVEL) s else NA_character_)
    }
    if (is.call(el) && length(el) == 3L &&
        as.character(el[[1]])[1] %in% c("::", ":::") && is.name(el[[3]])) {
      s <- as.character(el[[3]])
      return(if (s %in% MM_REFLECTIVE_TOP_LEVEL) s else NA_character_)
    }
    NA_character_
  }
  # The literal `what` of a do.call() when it is a listed name, else NA.
  do_call_listed_what <- function(x) {
    a <- as.list(x)[-1]
    f1 <- if (!is.null(names(a)) && "what" %in% names(a)) a[["what"]]
          else if (length(a)) a[[1]] else NULL
    if (is.character(f1) && length(f1) == 1L &&
        .mm_unqualify(f1) %in% MM_REFLECTIVE_TOP_LEVEL) .mm_unqualify(f1)
    else NA_character_
  }
  # Heads whose function-literal argument is a DEFINITION (a value being bound
  # or returned), not a callback handed to a callee.
  DEFINING_HEADS <- c("{", "(", "local", "<-", "=", "<<-")
  # (fix round 6 of batch 11, sixth review P1) Wrappers that evaluate to their
  # single argument. In ARGUMENT position `Reduce((function(a, f) source(f)),
  # files)` and `Reduce(local(function(a, f) source(f)), files)` hand the
  # callee the same literal as the unwrapped spelling, so the wrapper is peeled
  # before the literal test. Only a wrapper with exactly one argument is
  # peeled here; a multi-statement `local({ ...; function() ... })` block in
  # argument position is handled below by walking the whole block names-only
  # (round-6 addendum).
  ARG_WRAPPERS <- c("(", "{", "local", "identity", "invisible", "force")
  # The wrapper's head, normalised: a bare symbol, or the exact
  # `base::<name>` spelling (integrity batch 13, R12-P1-3: `base::local(...)`
  # in argument position has the raw head `::`, so it was never peeled and its
  # function literal never walked, while `Reduce(base::local(function(acc, f)
  # base::source(f)), files)` runs the literal at source time exactly as the
  # bare form does). Normalised BEFORE unwrap_arg(), and only for the exact
  # base:: spelling -- `pkg::local` is not a wrapper of anything.
  wrapper_head <- function(el) {
    if (!is.call(el)) return(NA_character_)
    h <- el[[1]]
    if (is.name(h)) return(as.character(h))
    if (is.call(h) && length(h) == 3L && is.name(h[[1]]) &&
        identical(as.character(h[[1]]), "::") && is.name(h[[2]]) &&
        identical(as.character(h[[2]]), "base") && is.name(h[[3]]))
      return(as.character(h[[3]]))
    NA_character_
  }
  unwrap_arg <- function(el) {
    while (is.call(el) && length(el) == 2L &&
           wrapper_head(el) %in% ARG_WRAPPERS)
      el <- el[[2]]
    el
  }
  # Name-list-only walk over a function literal that a callee may invoke at
  # load time: every listed symbol anywhere in the body, and every do.call()
  # with a literal listed `what`. Nested literals are included, since the
  # callee's body may call them too. `stmt` is the top-level statement named
  # in the refusal.
  walk_names_only <- function(body, stmt, via) {
    hits <- character(0)
    rec <- function(y) {
      if (is.symbol(y) || is.character(y)) {
        s <- listed_symbol(y); if (!is.na(s)) hits <<- c(hits, s)
        return(invisible(NULL))
      }
      if (is.pairlist(y)) { for (el in as.list(y)) rec(el); return(invisible(NULL)) }
      if (!is.call(y)) return(invisible(NULL))
      if (identical(as.character(y[[1]])[1], "do.call")) {
        w <- do_call_listed_what(y); if (!is.na(w)) hits <<- c(hits, w)
      }
      for (i in seq_along(y)) {
        el <- tryCatch(y[[i]], error = function(e) NULL)
        if (is.null(el) || (is.symbol(el) && !nzchar(as.character(el)))) next
        tryCatch(rec(el), error = function(e) NULL)
      }
      invisible(NULL)
    }
    rec(body)
    if (length(hits))
      say(sprintf("passes %s() a function literal whose body names %s", via,
                  paste(unique(hits), collapse = ", ")), stmt)
    invisible(NULL)
  }
  walk <- function(x, stmt = x) {
    if (!is.call(x)) return(invisible(NULL))
    op <- as.character(x[[1]])[1]
    if (op %in% c("<-", "=", "<<-") && length(x) == 3L && is.call(x[[2]]) &&
        identical(as.character(x[[2]][[1]])[1], "environment"))
      say("replaces an environment (`environment(x) <- value`)", x)
    h <- peel(x[[1]])
    nm <- if (is.name(h)) as.character(h)
          else if (is.call(h) && length(h) == 3L &&
                   as.character(h[[1]])[1] %in% c("::", ":::") && is.name(h[[3]]))
            as.character(h[[3]])
          else NA_character_
    if (is.na(nm)) {
      say("calls a value whose head is neither a bare symbol nor a `::`-qualified symbol", x)
    } else {
      if (nm %in% MM_REFLECTIVE_TOP_LEVEL)
        say(sprintf("calls %s()", nm), x)
      if (identical(nm, "do.call")) {
        a <- as.list(x)[-1]
        f1 <- if (!is.null(names(a)) && "what" %in% names(a)) a[["what"]]
              else if (length(a)) a[[1]] else NULL
        if (!is.null(f1) && !(is.character(f1) && length(f1) == 1L))
          say("calls do.call() with a function it cannot read as a literal name", x)
        w <- do_call_listed_what(x)
        if (!is.na(w)) say(sprintf("calls do.call() with \"%s\" as its literal what", w), x)
      }
    }
    if (is_fn_lit(h)) walk(h[[3]], stmt)      # immediately invoked: it runs
    head_for_args <- if (is.na(nm)) op else nm
    for (i in seq_along(x)) {
      if (i == 1L) next
      el <- tryCatch(x[[i]], error = function(e) NULL)
      if (is.null(el) || (is.symbol(el) && !nzchar(as.character(el)))) next
      s <- listed_symbol(el)
      if (!is.na(s)) {
        how <- if (is.character(el)) sprintf("the character literal \"%s\"", el) else s
        if (head_for_args %in% c("<-", "=", "<<-"))
          say(sprintf("binds %s to a name (an alias of a reflective function)", how), stmt)
        else
          say(sprintf("passes %s as a value to %s()", how, head_for_args), stmt)
        next
      }
      lit <- unwrap_arg(el)
      if (is_fn_lit(lit)) {
        if (!(head_for_args %in% DEFINING_HEADS) && length(lit) >= 3L)
          walk_names_only(lit[[3]], stmt, head_for_args)   # a callback: the callee may run it now
        next                                    # otherwise a definition, judged elsewhere
      }
      # (round-6 addendum, o5) a multi-statement `local({ ... })` or `{ ... }`
      # block in ARGUMENT position of a non-defining head is evaluated at load
      # time, and the closure it returns is handed to the callee: every
      # statement of the block, the returned literal included, is walked
      # names-only. At statement level (`x <- local({ ... })`, a bare block
      # that only defines) the block stays a definition.
      if (!(head_for_args %in% DEFINING_HEADS) && is.call(el) &&
          wrapper_head(el) %in% c("local", "{")) {
        walk_names_only(el, stmt, head_for_args)
        next
      }
      tryCatch(walk(el, stmt), error = function(e) NULL)
    }
    invisible(NULL)
  }
  for (e in exprs) {
    if (is_fn_lit(e)) next
    if (is.call(e) && as.character(e[[1]])[1] %in% c("<-", "=", "<<-") &&
        length(e) == 3L) {
      b <- tryCatch(.mm_trusted_block_env(e[[3]]), error = function(err) NULL)
      if (!is.null(b)) {
        sv <- tryCatch(.mm_trusted_block_statement_violations(b$st, b$env_sym, binds),
                       error = function(err) "x")
        if (!length(sv)) next
      }
    }
    tryCatch(walk(e), error = function(err) NULL)
  }
  unique(out)
}

# The rule above applied to a file, for the callers that hold a path.
.mm_reflective_top_level_of_file <- function(path, binds = list()) {
  exprs <- tryCatch(parse(path, keep.source = FALSE), error = function(e) NULL)
  .mm_reflective_top_level_violations(exprs, binds)
}

.mm_is_function_literal <- function(rhs) {
  is.call(rhs) && identical(as.character(rhs[[1]])[1], "function")
}

# Names of every function called anywhere inside a binding's right-hand side.
# Used ONLY to derive the gate set (which bindings reach
# require_validated_method); entry points are judged by the first-statement
# rule below, never by this whole-body walk.
.mm_binding_calls <- function(rhs) unique(.mm_call_heads(rhs))

# FIRST-STATEMENT RULE (review round 8, MEDIUM 1). "Calls the gate somewhere"
# was foolable: `if (FALSE) gate()`, a call after return(), a call inside a
# nested function literal, a formal's default, or a lazy tail all contain the
# gate name without ever gating. An entry point is gated only if the FIRST
# statement of its body evaluates the gate: the body's `{` is unwrapped, the
# first statement taken, and the house wrappers peeled — `x <- gate(...)`,
# `if (!validated(...)) stop(...)`, `if (validated(...))`, `(gate())` — until a
# call remains whose head is a gate function. Formals' defaults and nested
# `function` literals are never inspected.
.mm_first_statement <- function(fn_expr) {
  if (length(fn_expr) < 3L) return(NULL)
  body <- fn_expr[[3]]
  if (is.call(body) && identical(as.character(body[[1]])[1], "{")) {
    if (length(body) < 2L) return(NULL)
    return(body[[2]])
  }
  body
}
.mm_gate_call_of <- function(stmt) {
  if (!is.call(stmt)) return(NULL)
  op <- as.character(stmt[[1]])[1]
  if (op %in% c("<-", "=", "<<-") && length(stmt) == 3L) return(.mm_gate_call_of(stmt[[3]]))
  if (identical(op, "if") && length(stmt) >= 3L) return(.mm_gate_call_of(stmt[[2]]))
  if (identical(op, "!") && length(stmt) == 2L) return(.mm_gate_call_of(stmt[[2]]))
  if (identical(op, "(") && length(stmt) == 2L) return(.mm_gate_call_of(stmt[[2]]))
  stmt
}
# --- gate-chain certification (round 9, P0-1) -------------------------------
# A binding used to count as a "gate" when `require_validated_method` occurred
# anywhere in its syntax tree. That certified dead code: a wrapper whose only
# mention of the validator was `if (FALSE) require_validated_method(...)`
# satisfied the audit while the method ran unvalidated. The gate set is no
# longer derived by a whole-body walk. Instead the audit follows an execution
# chain: from the entry point's first statement to the function it calls, from
# that function's own first gating-relevant statement to the next, and so on,
# until the chain reaches `require_validated_method(unit = "<this unit>")`.
# Any other terminus, a unit argument that is absent or not a character
# literal, or a cycle is a refusal that names the chain it walked.

# `base::local({ ...; function(...) ... })` (the house gate closure) and a bare
# `{ ... }` block both yield a function; the value is the block's last
# expression. Integrity batch 13 (Codex round 12, P0-2; SME decision 1A): the
# wrapper is recognised ONLY as the exact `base::local`. The bare `local` is an
# ordinary binding resolved through the environment the reference is sourced
# into, and a caller who pre-bound `local <- function(...) function(...) TRUE`
# received an always-TRUE gate from a file whose sealed construction was
# genuine (reproduced at run time on 681e8de: `real local : FALSE / fake local
# : TRUE` for scm_config's draft unit). A bare-local closure resolves to NULL
# here and is refused by name through .mm_bare_local_closures().
#
# The wrapper also names its frame explicitly: `base::local({ ... }, envir =
# base::new.env())`. local()'s DEFAULT envir is the expression `new.env()`
# substituted into the caller's frame, so a caller-shadowed `new.env` supplied
# the closure's frame -- and a frame whose parent is not this file's
# environment resolves `.mm_trusted_private` to whatever that parent holds
# (the batch-12 finding F1, reproduced on the closures by the batch-13 runtime
# fixture: `object '.mm_trusted_private' not found` under a fake new.env). The
# closure's frame must be parented on the file's own environment (it reads the
# file's bindings), so exactly `base::new.env()` with no arguments.
.mm_resolve_fn <- function(rhs) {
  seen <- 0L
  while (is.call(rhs) && seen < 8L) {
    op <- as.character(rhs[[1]])[1]
    if (.mm_is_base_qualified_call_to(rhs, "local") && length(rhs) >= 2L) {
      if (is.null(.mm_closure_wrapper_violation(rhs, "<closure>"))) { rhs <- rhs[[2]]; seen <- seen + 1L; next }
      return(NULL)
    }
    if (op == "{" && length(rhs) >= 2L) { rhs <- rhs[[length(rhs)]]; seen <- seen + 1L; next }
    break
  }
  if (.mm_is_function_literal(rhs)) rhs else NULL
}

# The name of the function a call invokes.
#
#   f(...)        -> "f"
#   env$f(...)    -> "f"        (element dispatch; the census decides whether
#                                that element name was built locally)
#   pkg::f(...)   -> "pkg::f"   QUALIFIED, deliberately (fix round 2, R7).
#
# Returning the bare name for a namespace-qualified call made
# `stats::require_validated_method(unit = "u")` identical to the validator and
# the audit credited it. Nothing outside this repository provides the harness's
# validator, and a chain member is a function defined in the reference file, so
# a qualified name can never legitimately terminate a chain; keeping the
# qualifier makes it fail the identity test instead of passing it.
.mm_call_head_name <- function(cl) {
  if (!is.call(cl)) return(NA_character_)
  h <- cl[[1]]
  if (is.name(h)) return(as.character(h))
  if (is.call(h) && length(h) == 3L &&
      (is.name(h[[3]]) || is.character(h[[3]]))) {
    op <- as.character(h[[1]])[1]
    if (op %in% c("::", ":::"))
      return(paste0(as.character(h[[2]])[1], op, as.character(h[[3]])[1]))
    if (op %in% c("$", "@")) return(as.character(h[[3]]))
  }
  NA_character_
}

# The bare name behind a possibly-qualified head: "pkg::f" -> "f".
.mm_unqualify <- function(nm) sub("^.*:::?", "", nm)

# Is `x` a call whose head is the BARE symbol `nm` -- not `obj$nm`, not
# `pkg::nm`, not `x[["nm"]]`, not a parenthesised or computed head?
#
# Fix round 3 of batch 11 (review G2-a). .mm_call_head_name() maps a `$` head
# to its member name by design, because for the gate chain `private$validate()`
# genuinely calls the member named `validate`. That is the wrong reading for
# the trusted construction, where the question is not "what is this member
# called" but "is this the base primitive". With the member reading,
# `new.env(parent = shim$baseenv())` and `shim$new.env(parent = baseenv())`
# both passed the construction clause, and the first of them produced a sealed
# object parented on R_GlobalEnv -- which any code in the calling session can
# write -- while the audit reported the block as gated. Wherever the audit
# means the base primitive itself, it asks this question instead.
.mm_is_bare_call_to <- function(x, nm) {
  is.call(x) && is.name(x[[1]]) && identical(as.character(x[[1]]), nm)
}

# Is `x` a call whose head is EXACTLY `base::<nm>` -- the `::` operator with
# the literal symbol `base` on its left and the bare symbol `nm` on its right?
#
# Integrity batch 12 (Codex round 11, P0-1; SME decision 1, 2026-09-05). The
# trusted construction used to call `local`, `new.env`, `baseenv`,
# `file.exists`, `stop`, `sys.source` and `lockEnvironment` as bare symbols,
# which resolve through the lookup chain of the environment the reference file
# is sourced into. A caller who pre-binds a fake `sys.source` in that
# environment -- one that writes `require_validated_method <- function(...)
# TRUE` into the `envir` it is handed -- installs its own validator into the
# object about to be sealed, with no repository byte changed. The audit could
# not see it because it examines the file's bindings, not the caller's.
#
# `base::name` resolves through the base NAMESPACE, which R seals at startup,
# so a binding in the sourcing environment is never consulted. The audit
# therefore admits exactly these qualified heads, each in its own position of
# the construction, and refuses everything else there: the bare spelling,
# `base:::name` (the internal accessor, a different operator), `pkg::name`,
# `"base"::name` (a string, not the symbol), `obj$name`, `obj[["name"]]`,
# `(base::name)(...)` and any computed head. Everywhere OUTSIDE the
# construction a `::`-qualified call keeps the treatment it had.
.mm_is_base_qualified_call_to <- function(x, nm) {
  if (!is.call(x)) return(FALSE)
  h <- x[[1]]
  is.call(h) && length(h) == 3L &&
    is.name(h[[1]]) && identical(as.character(h[[1]]), "::") &&
    is.name(h[[2]]) && identical(as.character(h[[2]]), "base") &&
    is.name(h[[3]]) && identical(as.character(h[[3]]), nm)
}

# The heads the constructions and their closures are written with, in the
# exact `base::` spelling the predicate above admits: the seven construction
# primitives (batch 12), and since integrity batch 13 (Codex round 12, P0-1;
# SME decision 1A) everything ELSE executable that a sealed construction or a
# gate/accessor closure reaches -- `c` for the path vectors, `local` for the
# closures, and the enumerated heads a pre-seal write may call (vapply,
# sapply, paste0, character, file.path, sprintf). `for`, `if`, `!`, `{`, `(`,
# `<-`, `function` and `::` itself are also ordinary bindings a sourcing
# environment CAN shadow (there is no unshadowable spelling in R; probe
# a local probe and the batch-12 review rebound all of them);
# they are not qualified here because qualifying `::` with `::` is circular.
# A reference file that binds any of them at its top level is refused through
# MM_TRUST_CONSTRUCTION_PRIMITIVES, and a CALLER that rebinds them is the
# documented residual stated in every construction block's handover comment:
# the residual is now `::` and the syntax heads, nothing else.
MM_TRUST_BASE_QUALIFIED_HEADS <- c("local", "new.env", "baseenv", "file.exists",
                                   "stop", "sys.source", "lockEnvironment",
                                   "c", "vapply", "sapply", "paste0", "character",
                                   "file.path", "sprintf")

# The value a pre-seal write may hold, when it is not a literal (integrity
# batch 13, decision 1A): a call whose EVERY head is one of these exact
# qualified spellings, whose symbols are the enclosing loop index, a formal of
# a function literal passed as FUN, or a root path constant of the file, and
# whose only unqualified head is the `function` literal itself. eda.R's
# dependency-digest write is
#   base::vapply(base::c("tools/record_selection.R"), function(f)
#     base::paste0("sha256:", digest::digest(file = f, algo = "sha256")),
#     base::character(1))
# `digest::digest` is on the list because base R has no sha256 and the write
# records the loaded bytes' digest; it resolves through the digest namespace,
# never through the sourcing environment, which is the property every entry
# here has. Nothing else is admitted, whatever it returns.
MM_TRUST_WRITE_VALUE_HEADS <- c("base::c", "base::vapply", "base::sapply",
                                "base::paste0", "base::character",
                                "base::file.path", "base::sprintf",
                                "digest::digest")

# Is `value` an admissible pre-seal write value? `idx` carries the enclosing
# loop indices, `formals` the formals of an enclosing function literal, `binds`
# the file's binding index (root path constants resolve through it).
.mm_trusted_write_value_ok <- function(value, idx = character(0), binds = list(),
                                       formals = character(0), depth = 0L) {
  if (depth > 8L) return(FALSE)
  if (is.null(value)) return(FALSE)
  if (is.atomic(value)) return(length(value) >= 1L && !is.function(value))
  if (is.name(value)) {
    nm <- as.character(value)
    if (!nzchar(nm)) return(FALSE)
    if (nm %in% idx || nm %in% formals) return(TRUE)
    return(!is.null(.mm_static_path_vector(value, binds)))
  }
  if (!is.call(value)) return(FALSE)
  h <- value[[1]]
  if (is.name(h) && identical(as.character(h), "function")) {
    fm <- if (is.null(value[[2]])) character(0) else names(as.list(value[[2]]))
    fl <- as.list(value[[2]])
    for (k in seq_along(fl))                                       # no defaults
      if (!(is.symbol(fl[[k]]) && !nzchar(as.character(fl[[k]])))) return(FALSE)
    return(.mm_trusted_write_value_ok(value[[3]], idx, binds, c(formals, fm), depth + 1L))
  }
  hq <- .mm_call_head_name(value)
  if (is.na(hq) || !hq %in% MM_TRUST_WRITE_VALUE_HEADS) return(FALSE)
  # The qualified head must be the `::` operator with two bare symbols, not a
  # string-qualified or `:::` spelling that happens to print alike.
  if (!(is.call(h) && length(h) == 3L && is.name(h[[1]]) &&
        identical(as.character(h[[1]]), "::") && is.name(h[[2]]) && is.name(h[[3]])))
    return(FALSE)
  al <- as.list(value)[-1]
  for (k in seq_along(al)) {
    if (is.symbol(al[[k]]) && !nzchar(as.character(al[[k]]))) return(FALSE)
    if (!.mm_trusted_write_value_ok(al[[k]], idx, binds, formals, depth + 1L)) return(FALSE)
  }
  TRUE
}

# Every call in `x` whose head names one of the seven construction primitives
# by any spelling OTHER than the exact `base::` qualification, as deparsed
# head text. Used to write a refusal that names the qualification requirement
# rather than the generic "not a permitted form".
.mm_unqualified_primitive_heads <- function(x) {
  acc <- character(0)
  walk <- function(y) {
    if (!is.call(y)) return(invisible(NULL))
    hq <- .mm_call_head_name(y)
    if (!is.na(hq)) {
      bare <- .mm_unqualify(hq)
      if (bare %in% MM_TRUST_BASE_QUALIFIED_HEADS &&
          !.mm_is_base_qualified_call_to(y, bare))
        acc <<- c(acc, paste(deparse(y[[1]]), collapse = ""))
    } else if (is.call(y[[1]])) {
      # A parenthesised or computed head that still spells a primitive.
      inner <- .mm_unparen(y[[1]])
      nm <- if (is.name(inner)) as.character(inner)
            else if (is.call(inner) && length(inner) == 3L &&
                     as.character(inner[[1]])[1] %in% c("::", ":::") && is.name(inner[[3]]))
              as.character(inner[[3]]) else NA_character_
      if (!is.na(nm) && nm %in% MM_TRUST_BASE_QUALIFIED_HEADS)
        acc <<- c(acc, paste(deparse(y[[1]]), collapse = ""))
    }
    for (i in seq_along(y)) if (!is.null(y[[i]])) tryCatch(walk(y[[i]]), error = function(e) NULL)
    invisible(NULL)
  }
  tryCatch(walk(x), error = function(e) NULL)
  unique(acc)
}

.mm_formal_names <- function(fn_expr) {
  f <- fn_expr[[2]]
  if (is.null(f)) character(0) else names(as.list(f))
}

# R's argument matching, reduced to what the audit needs: exact names first,
# then the remaining actuals positionally over the remaining formals.
.mm_match_args <- function(cl, formal_names) {
  actuals <- as.list(cl)[-1]
  nm <- names(actuals); if (is.null(nm)) nm <- rep("", length(actuals))
  out <- list()
  used <- rep(FALSE, length(actuals))
  for (i in seq_along(actuals)) {
    if (nzchar(nm[i]) && nm[i] %in% formal_names) { out[[nm[i]]] <- actuals[[i]]; used[i] <- TRUE }
  }
  free_formals <- setdiff(formal_names, c(names(out), "..."))
  k <- 1L
  for (i in seq_along(actuals)) {
    if (used[i] || nzchar(nm[i])) next
    if (k > length(free_formals)) break
    out[[free_formals[k]]] <- actuals[[i]]; k <- k + 1L
  }
  out
}

# A character literal, either written out or reachable through one level of
# forwarding (`.gate("vpc")` -> `validated(unit)` -> `..., unit = unit`).
.mm_literal_value <- function(arg, bindings) {
  if (is.character(arg) && length(arg) == 1L) return(arg)
  if (is.name(arg)) {
    v <- bindings[[as.character(arg)]]
    if (is.character(v) && length(v) == 1L) return(v)
  }
  NA_character_
}

# The candidate gate calls inside a wrapper, in execution order.
#
# The chain follows only statements at the TOP LEVEL of the wrapper's body, so
# it never enters an `if`/`for`/`while` body, a nested `function` literal, or a
# formal's default -- which is what makes `if (FALSE) require_validated_method()`
# unreachable to it. Real house gates do work before validating (the lazy
# `source()` of this file, eda-standards' source-identity and dependency-digest
# checks), so preceding top-level statements may be stepped over; a statement
# that is stepped over must not contain `return`, because an early return would
# let the wrapper exit before the gate call it is credited with.
.mm_wrapper_statements <- function(fn_expr) {
  if (length(fn_expr) < 3L) return(list())
  body <- fn_expr[[3]]
  if (is.call(body) && identical(as.character(body[[1]])[1], "{"))
    as.list(body)[-1] else list(body)
}
.mm_has_return <- function(e) "return" %in% .mm_call_heads(e)

# --- structural anti-shadowing (round 9 fix round, review B1) ---------------
# The step-over rule below let a stepped-over statement REBIND the validator,
# or any function in the credited chain, to a no-op: the audit credited the
# next call as the gate while R's lexical scoping resolved it to the local
# no-op at runtime. Five fixtures demonstrated it (AA/BB/CC/DD/FF), including
# one that shadowed a MID-CHAIN helper rather than the validator, so a fix that
# special-cases the validator's name is not enough. Three structural rules
# close it, none of them by name-matching a call:
#
#   (1) a reference file that BINDS `require_validated_method` anywhere -- top
#       level, inside any function body, by `<-` / `=` / `<<-` / `assign()`, or
#       as a formal -- is refused outright. The validator is a name a reference
#       may call and must never define.
#   (2) a stepped-over statement may not contain a call that can rebind a name
#       dynamically (`assign`, `eval`, `source`, `environment<-`, ...), which is
#       what a static name scan cannot see.
#   (3) every name in the credited chain must be bound EXACTLY ONCE in the
#       file, by its single top-level definition. A chain member rebound
#       anywhere else -- in another function's body, or as a formal -- is
#       refused.

MM_VALIDATOR_NAME <- "require_validated_method"

# Calls that can bind a name at run time, invisibly to a static scan.
MM_REBINDING_CALLS <- c("assign", "eval", "evalq", "eval.parent", "source",
                        "sys.source", "attach", "list2env", "delayedAssign",
                        "makeActiveBinding", "environment<-", "body<-",
                        "formals<-", "local", "parse", "substitute")

# The innermost symbol a (possibly replacement-function) assignment targets:
# `x <- v` -> "x"; `names(x) <- v` -> "x"; `e$f <- v` -> "f".
.mm_assign_target <- function(lhs) {
  while (is.call(lhs)) {
    op <- as.character(lhs[[1]])[1]
    if (op %in% c("$", "@") && length(lhs) == 3L) { lhs <- lhs[[3]]; next }
    if (length(lhs) < 2L) return(NA_character_)
    lhs <- lhs[[2]]
  }
  if (is.name(lhs) || is.character(lhs)) as.character(lhs)[1] else NA_character_
}

# EVERY name an assignment's left-hand side binds: the base symbol AND every
# `$` / `[[` element key along the way. Fix round 2 (R5): `e[["rvm"]] <- f`
# was recorded only as a binding of `e`, so installing the validator into a
# fresh environment and dispatching off it was invisible to the census.
.mm_assign_names <- function(lhs) {
  acc <- character(0)
  cur <- lhs
  while (is.call(cur) && length(cur) >= 3L) {
    op <- as.character(cur[[1]])[1]
    if (op %in% c("$", "@", "[[")) {
      key <- cur[[3]]
      if (is.name(key) || (is.character(key) && length(key) == 1L))
        acc <- c(acc, as.character(key)[1])
      cur <- cur[[2]]
      next
    }
    break
  }
  base <- .mm_assign_target(lhs)
  if (!is.na(base)) acc <- c(acc, base)
  unique(acc[nzchar(acc)])
}

# The base symbol a replacement target dispatches off: `e$x` and `e[["x"]]` and
# `e$a$b` all have base symbol `e`. NA when the target is not rooted in a plain
# symbol. Distinct from .mm_assign_target(), which reports the MEMBER name.
#
# Fix round 4 of batch 11 (fourth review, P2). The peel used to be limited to
# `$`, `@` and `[[`, so ANY other replacement form -- `parent.env(.cache) <- e`,
# `environment(f) <- e`, `attr(.cache, "x") <- v`, `body(f) <- ...` -- returned
# NA and was therefore not counted as a rebinding of `.cache` / `f` by
# .mm_all_binding_counts(). A cache re-parented that way inside a named function
# body kept its bound-once exclusion. Every replacement call `g(x, ...) <- v`
# targets its FIRST argument, so the peel is now general: walk down argument one
# until a symbol is reached. `names(x)[1] <- v` peels `[` then `names` to `x`;
# a target rooted in something other than a symbol still returns NA.
.mm_assign_base_symbol <- function(lhs) {
  cur <- lhs
  while (is.call(cur) && length(cur) >= 2L) cur <- cur[[2]]
  if (is.name(cur)) as.character(cur) else NA_character_
}

# Fix round 5 of batch 11 (fifth review, I4 boundary). `parent.env(.cache) <- e`
# and `` `parent.env<-`(.cache, e) `` are the same act -- the second is the
# replacement function called directly -- but the second is a CALL, not an
# assignment, so the peel above was never handed its target and the cache kept
# its bound-once exclusion. A call whose (possibly `::`-qualified) head name
# ends in `<-`, other than the assignment operators themselves, rebinds the base
# symbol of its first argument. NA when the call is not of that shape.
.mm_functional_replacement_base <- function(x) {
  if (!is.call(x) || length(x) < 2L) return(NA_character_)
  h <- tryCatch(.mm_call_head_name(x), error = function(e) NA_character_)
  if (is.na(h)) return(NA_character_)
  bare <- .mm_unqualify(h)
  if (bare %in% c("<-", "<<-", "=") || !grepl("<-$", bare)) return(NA_character_)
  tryCatch(.mm_assign_base_symbol(x[[2]]), error = function(e) NA_character_)
}

# The name a call BINDS when it is one of the INDIRECT assignment forms the
# census used to miss (fix batch 11, review R10-P0-2): `assign("x", v)` written
# plainly or namespace-qualified (`base::assign`), and `do.call("assign",
# list("x", v))` / `do.call(assign, ...)`. The `` `<-`(x, v) `` functional form
# needs nothing here — its call head IS "<-", so the ordinary assignment branch
# of every census already sees it.
#
# Fix round 1 of batch 11 (independent review R2.1, R2.2). Three further forms
# bind in the calling frame by default and were invisible to this census:
# `delayedAssign("sys.source", f)`, `makeActiveBinding("sys.source", f,
# environment())` and `list2env(list(sys.source = f), environment())`. All three
# were demonstrated end to end: the audit reported `gated = TRUE` with an honest
# `trusted_loads` while the real validator was never loaded. `list2env` was
# already named in MM_TRUST_CONSTRUCTION_PRIMITIVES, so the file knew the call
# mattered and the census did not look for it.
#
# This blacklist governs the file's FREE-FORM TOP LEVEL, where no whitelist of
# statement forms is possible. Inside the trusted block the rule is now a
# whitelist (.mm_trusted_block_statement_ok), which closes the class rather than
# one member of it; the two are independent and both apply.
MM_ASSIGN_CALL_BINDERS <- c("assign", "do.call", "delayedAssign",
                            "makeActiveBinding", "list2env")

# Returns the literal name(s); "<computed>" when the form is an assignment but
# the name is not a static literal (`base::assign(paste0("sys.", "source"),
# ...)` -- an unresolvable name is still a binding and must not be silently
# dropped); or character(0) when the call binds nothing. `list2env` can bind
# more than one name at once, which is why this returns a vector.
.mm_assign_call_names <- function(x) {
  if (!is.call(x)) return(character(0))
  h <- .mm_call_head_name(x)
  if (is.na(h)) return(character(0))
  bare <- .mm_unqualify(h)
  nm_of <- function(a) if (is.character(a) && length(a) == 1L) a else "<computed>"
  if (identical(bare, "delayedAssign") || identical(bare, "makeActiveBinding")) {
    a <- as.list(x)[-1]
    if (!length(a)) return(character(0))
    anm <- names(a)
    formal1 <- if (identical(bare, "delayedAssign")) "x" else "sym"
    first <- if (!is.null(anm) && formal1 %in% anm) {
      a[[which(anm == formal1)[1]]]
    } else {
      pos <- if (is.null(anm)) seq_along(a) else which(!nzchar(anm))
      if (length(pos)) a[[pos[1]]] else NULL
    }
    if (is.null(first)) return("<computed>")
    return(nm_of(first))
  }
  if (identical(bare, "list2env")) {
    a <- as.list(x)[-1]
    if (!length(a)) return(character(0))
    anm <- names(a)
    lst <- if (!is.null(anm) && "x" %in% anm) {
      a[[which(anm == "x")[1]]]
    } else {
      pos <- if (is.null(anm)) seq_along(a) else which(!nzchar(anm))
      if (length(pos)) a[[pos[1]]] else NULL
    }
    if (is.null(lst) || !is.call(lst)) return("<computed>")
    lh <- .mm_call_head_name(lst)
    if (is.na(lh) || !.mm_unqualify(lh) %in% c("list", "alist"))
      return("<computed>")
    parts <- as.list(lst)[-1]
    pnm <- names(parts)
    if (!length(parts)) return(character(0))
    if (is.null(pnm) || !all(nzchar(pnm))) return("<computed>")
    return(pnm)
  }
  if (identical(bare, "assign")) {
    a <- as.list(x)[-1]
    if (!length(a)) return(NA_character_)
    anm <- names(a)
    first <- if (!is.null(anm) && "x" %in% anm) {
      a[[which(anm == "x")[1]]]
    } else {
      pos <- if (is.null(anm)) seq_along(a) else which(!nzchar(anm))
      if (length(pos)) a[[pos[1]]] else NULL
    }
    if (is.null(first)) return("<computed>")
    return(nm_of(first))
  }
  if (identical(bare, "do.call")) {
    a <- as.list(x)[-1]
    if (!length(a)) return(character(0))
    what <- a[[1]]
    wnm <- if (is.character(what) && length(what) == 1L) what
           else if (is.name(what)) as.character(what) else NA_character_
    if (is.na(wnm) || !.mm_unqualify(wnm) %in% c("assign", "<-", "<<-", "="))
      return(character(0))
    argl <- if (length(a) >= 2L) a[[2]] else NULL
    if (is.call(argl) && !is.na(.mm_call_head_name(argl)) &&
        .mm_unqualify(.mm_call_head_name(argl)) %in% c("list", "alist")) {
      parts <- as.list(argl)[-1]
      if (length(parts)) return(nm_of(parts[[1]]))
    }
    return("<computed>")
  }
  character(0)
}

# Single-name compatibility view of the census: the first name bound, or NA when
# the call binds nothing. Call sites that can carry more than one name use
# .mm_assign_call_names() directly.
.mm_assign_call_name <- function(x) {
  nms <- .mm_assign_call_names(x)
  if (!length(nms)) NA_character_ else nms[1]
}

# Constructors whose NAMED ARGUMENTS become element names of the value they
# build. Fix round 2 (R1, R6): `list(require_validated_method = function(...) TRUE)`
# followed by `p$require_validated_method(...)` bound the validator's name in
# every sense that matters at the call site, and the census did not see it.
# Restricted to these constructors on purpose: recording the named arguments of
# every call would sweep in ordinary parameter names such as `unit =`.
MM_ELEMENT_CONSTRUCTORS <- c("list", "c", "structure", "alist", "data.frame",
                             "list2env", "new.env", "setNames", "modifyList")

# Every name bound ANYWHERE in an expression tree, with multiplicity: by
# assignment, by assign() with a literal name, and by being a formal of any
# function literal. Nested function bodies and formals' defaults are walked.
.mm_bound_names <- function(e) {
  acc <- character(0)
  walk <- function(x) {
    if (is.call(x)) {
      op <- as.character(x[[1]])[1]
      if (op %in% c("<-", "=", "<<-") && length(x) == 3L) {
        acc <<- c(acc, .mm_assign_names(x[[2]]))
      } else if (length(an <- .mm_assign_call_names(x))) {
        acc <<- c(acc, an)
      } else if (op %in% MM_ELEMENT_CONSTRUCTORS) {
        anm <- names(as.list(x))
        if (!is.null(anm)) acc <<- c(acc, anm[nzchar(anm)])
      } else if (identical(op, "function") && length(x) >= 3L) {
        fm <- x[[2]]
        if (!is.null(fm)) acc <<- c(acc, names(as.list(fm)))
      }
      for (i in seq_along(x)) if (!is.null(x[[i]])) walk(x[[i]])
    } else if (is.pairlist(x)) {
      lst <- as.list(x)
      acc <<- c(acc, names(lst))
      for (v in lst) if (!missing(v) && !is.null(v)) tryCatch(walk(v), error = function(e) NULL)
    }
    invisible(NULL)
  }
  tryCatch(walk(e), error = function(e) NULL)
  acc[nzchar(acc)]
}

# Bound-name census for a whole reference file.
.mm_file_bound_names <- function(path) {
  exprs <- tryCatch(parse(path, keep.source = FALSE), error = function(e) NULL)
  if (is.null(exprs)) return(character(0))
  unlist(lapply(as.list(exprs), .mm_bound_names))
}

# Calls that REACH another call by name, so the callee is a value rather than a
# syntactic head and `.mm_call_heads()` cannot see it. A gate prologue has no
# business calling any of them, so in a stepped-over statement they are refused
# outright (fix round 2, R2/R3).
MM_INDIRECT_CALLS <- c("do.call", "eval", "evalq", "eval.parent", "Recall")

# Higher-order calls that take a function to apply. They are refused only when
# something in the statement mentions a rebinding name -- as a symbol or as a
# string literal -- which is how `Reduce(function(a, b) do.call("assign", ...))`
# smuggled an assignment past a head-only scan (fix round 2, R4).
MM_APPLY_CALLS <- c("Reduce", "Map", "Filter", "Find", "Position", "lapply",
                    "sapply", "vapply", "mapply", "apply", "tapply", "rapply",
                    "outer", "by")

# TRUE when a statement contains a call that could rebind a name dynamically.
.mm_has_rebinding_call <- function(e) {
  heads <- .mm_call_heads(e)
  if (any(MM_REBINDING_CALLS %in% heads)) return(TRUE)
  if (any(MM_INDIRECT_CALLS %in% heads)) return(TRUE)
  if (any(MM_APPLY_CALLS %in% heads)) {
    mentioned <- c(heads,
                   vapply(.mm_literals_of(e), function(x) x, character(1)))
    if (any(MM_REBINDING_CALLS %in% mentioned) ||
        any(MM_INDIRECT_CALLS %in% mentioned)) return(TRUE)
  }
  FALSE
}

# The rebinding-capable names a statement actually mentions, for the message.
.mm_rebinding_names <- function(e) {
  mentioned <- c(.mm_call_heads(e),
                 vapply(.mm_literals_of(e), function(x) x, character(1)))
  unique(intersect(mentioned, c(MM_REBINDING_CALLS, MM_INDIRECT_CALLS)))
}


# The ONE statement a wrapper may step over despite calling `source()`: the
# canonical lazy loader every house gate opens with,
#   if (!exists("require_validated_method", mode = "function"))
#     source(file.path("tools", "method_manifest.R"))
# It is recognised structurally -- an `exists()` guard on the validator's own
# name, whose body sources a literal path ending in method_manifest.R -- so a
# `source()` of anything else, or a bare `source()` with no guard, is still
# refused by rule (2).
.mm_is_lazy_loader <- function(stmt) {
  if (!is.call(stmt) || !identical(as.character(stmt[[1]])[1], "if") ||
      length(stmt) < 3L) return(FALSE)
  cond <- .mm_gate_call_of(stmt[[2]])
  if (is.null(cond) || !is.call(cond) ||
      !identical(.mm_call_head_name(cond), "exists")) return(FALSE)
  args <- as.list(cond)[-1]
  if (!length(args) || !is.character(args[[1]]) ||
      !identical(args[[1]], MM_VALIDATOR_NAME)) return(FALSE)
  lits <- Filter(function(x) is.character(x) && length(x) == 1L,
                 as.list(.mm_literals_of(stmt[[3]])))
  calls <- .mm_call_heads(stmt[[3]])
  identical(setdiff(calls, c("source", "file.path")), character(0)) &&
    any(vapply(lits, function(x) grepl("method_manifest\\.R$", x), logical(1)))
}

# Every character literal in an expression tree (used only by the loader shape
# check above).
.mm_literals_of <- function(e) {
  acc <- list()
  walk <- function(x) {
    if (is.character(x) && length(x) == 1L) acc[[length(acc) + 1L]] <<- x
    else if (is.call(x) || is.pairlist(x))
      for (i in seq_along(x)) if (!is.null(x[[i]])) tryCatch(walk(x[[i]]), error = function(e) NULL)
    invisible(NULL)
  }
  tryCatch(walk(e), error = function(e) NULL)
  acc
}

# --- trusted dispatch bases (fix round 3, final check F1-F8) ---------------
#
# Two rounds of extending the bound-name census closed twelve shapes and
# invited six more: `setNames(list(f), "rvm")` puts the element name in a
# POSITIONAL argument, `structure(list(f), names = "rvm")` puts it in the VALUE
# of an argument, and `names(p) <-`, `attr(p, "names") <-`, the functional
# `` `[[<-`(list(), "rvm", f) `` and a variable `p[[k]] <- ` are four more
# routes to the same place. Each made the audit certify an entry point that
# performed no validation at run time. Enumerating routes cannot end: R has
# more ways to put a name on an object than any list can hold.
#
# So the census stops being the discriminator for `$` dispatch. What separates
# the shipped pattern from every attack is not HOW the element name got there
# but WHERE THE OBJECT CAME FROM. There is exactly one construction in this
# repository that legitimately holds the validator as an element
# (skills/eda-standards/reference/eda.R:113-125):
#
#   .eda_private <- local({
#     e <- new.env(parent = baseenv())
#     sys.source("tools/method_manifest.R", envir = e)   # loaded, not built
#     lockEnvironment(e, bindings = TRUE)                 # and sealed
#     e
#   })
#
# A `$` dispatch to the validator's name -- or to a chain member's, or an entry
# point's -- is credited ONLY when its base is a symbol that traces to a
# top-level binding of that construction, and nothing anywhere in the file
# rebinds or mutates any symbol on that trace. Every other `$` dispatch to such
# a name is refused, naming the object, whatever route the name took. A `[[`
# dispatch is not resolvable to a name statically at all and was already
# refused by `.mm_call_head_name()`.
#
# The census (rules 1 and 3) is kept as an additional, independent refusal: it
# costs nothing and still catches the plain shapes early with a clearer message.

# Every binding of every name in a parsed file, mapped to the right-hand sides
# it was bound to. `names(p) <- v`, `attr(p, "names") <- v` and `p[[k]] <- v`
# all count as bindings of `p`, so a mutated object has more than one.
.mm_binding_index <- function(path) {
  exprs <- tryCatch(parse(path, keep.source = FALSE), error = function(e) NULL)
  if (is.null(exprs)) return(list())
  idx <- list()
  add <- function(nm, rhs) {
    if (is.na(nm) || !nzchar(nm)) return(invisible(NULL))
    idx[[nm]] <<- c(idx[[nm]], list(rhs))
  }
  walk <- function(x) {
    if (is.call(x)) {
      op <- as.character(x[[1]])[1]
      if (op %in% c("<-", "=", "<<-") && length(x) == 3L) {
        for (nm in .mm_assign_names(x[[2]])) add(nm, x[[3]])
      } else if (length(an <- .mm_assign_call_names(x)) && length(x) >= 3L) {
        for (a1 in an) add(a1, x[[3]])
      }
      for (i in seq_along(x)) if (!is.null(x[[i]])) tryCatch(walk(x[[i]]), error = function(e) NULL)
    } else if (is.pairlist(x)) {
      lst <- as.list(x)
      for (nm in names(lst)) add(nm, NULL)          # a formal is a binding too
    }
    invisible(NULL)
  }
  for (e in as.list(exprs)) tryCatch(walk(e), error = function(e) NULL)
  # ROOT-ONLY index of plain assignments (integrity batch 12, R11-P0-3). The
  # recursive index above answers "is this name bound more than once anywhere",
  # which is what the bound-once rules need. It must NOT be what a path
  # constant is resolved through: a function-local `DEPS <- "..."` in an
  # otherwise unrelated helper was the whole file's only binding of DEPS, so
  # .mm_static_path_vector() called it "a top-level constant" and trusted its
  # right-hand side -- while at run time the trusted block's `for (f in DEPS)`
  # read whatever the sourcing environment held. A path constant is credited
  # only from THIS index: exactly one plain `name <- value` / `name = value`
  # statement at the file's root.
  root <- list()
  # Each root binding also carries its STATEMENT ORDINAL (integrity batch 13,
  # R12-P1-2): a path constant is credited only when its single root binding
  # precedes the construction that reads it, because at run time a
  # construction written first reads whatever the sourcing environment holds
  # under that name, not the honest vector bound later in the file.
  root_order <- list()
  exprs_l <- as.list(exprs)
  for (i in seq_along(exprs_l)) {
    e <- exprs_l[[i]]
    if (!is.call(e)) next
    op <- as.character(e[[1]])[1]
    if (!op %in% c("<-", "=") || length(e) != 3L) next
    if (!is.name(e[[2]])) next
    nm <- as.character(e[[2]])
    if (!nzchar(nm)) next
    root[[nm]] <- c(root[[nm]], list(e[[3]]))
    root_order[[nm]] <- c(root_order[[nm]], i)
  }
  attr(idx, "root") <- root
  attr(idx, "root_order") <- root_order
  idx
}

# Is this right-hand side a closure built with the bare `local(...)` -- or any
# spelling of `local` other than the exact `base::local` (`base:::local`,
# `(base::local)`, `pkg::local`)? Integrity batch 13, R12-P0-2.
.mm_is_bare_local_rhs <- function(rhs) {
  rhs <- .mm_unparen(rhs)
  if (!is.call(rhs)) return(FALSE)
  if (.mm_is_base_qualified_call_to(rhs, "local")) return(FALSE)
  hd <- .mm_unparen(rhs[[1]])
  if (is.name(hd)) return(identical(as.character(hd), "local"))
  if (is.call(hd) && length(hd) == 3L && (is.name(hd[[3]]) || is.character(hd[[3]])))
    return(identical(as.character(hd[[3]]), "local"))
  FALSE
}

# Does this `base::local(...)` yield a function (a closure), as opposed to the
# sealed construction, whose block's value is a bare symbol?
.mm_local_yields_function <- function(rhs) {
  if (!is.call(rhs) || length(rhs) < 2L) return(FALSE)
  body <- rhs[[2]]
  if (is.call(body) && identical(as.character(body[[1]])[1], "{")) {
    st <- as.list(body)[-1]
    if (!length(st)) return(FALSE)
    body <- st[[length(st)]]
  }
  .mm_is_function_literal(body)
}

# The closure-wrapper rule, as a refusal sentence or NULL (integrity batch 13,
# R12-P0-2): a gate or accessor closure is written exactly
# `X <- base::local({ ... }, envir = base::new.env())`. The bare `local` is the
# caller's; a `base::local` with the default envir takes its frame from the
# caller's `new.env` (see .mm_resolve_fn).
.mm_closure_wrapper_violation <- function(rhs, nm) {
  # A bare-local block whose value is not a function is a construction
  # look-alike and is judged (and refused) by the construction rules.
  if (.mm_is_bare_local_rhs(rhs) && .mm_local_yields_function(.mm_unparen(rhs)))
    return(sprintf("`%s` is built with the bare `local({ ... })`; a gate or accessor closure must be written `%s <- base::local({ ... }, envir = base::new.env())` so that the wrapper resolves through the sealed base namespace and not through the environment this file is sourced into, where a caller's `local` returns whatever closure it likes (integrity batch 13, R12-P0-2)",
                   nm, nm))
  u <- .mm_unparen(rhs)
  if (!.mm_is_base_qualified_call_to(u, "local")) return(NULL)
  if (!.mm_local_yields_function(u)) return(NULL)          # a construction, judged elsewhere
  a <- as.list(u)[-1]
  anm <- names(a); if (is.null(anm)) anm <- rep("", length(a))
  ok <- length(a) == 2L && identical(anm[2], "envir") &&
    .mm_is_base_qualified_call_to(a[[2]], "new.env") && length(as.list(a[[2]])) == 1L
  if (ok) return(NULL)
  sprintf("`%s` is a `base::local({ ... })` closure without the explicit frame `envir = base::new.env()`; local()'s default envir is `new.env()` evaluated in the frame this file is sourced into, so a caller's fake `new.env` supplies the closure's frame and its parent decides what `.mm_trusted_private` resolves to; write `%s <- base::local({ ... }, envir = base::new.env())` (integrity batch 13, R12-P0-2)",
          nm, nm)
}

# Heads a gate-chain function may call unqualified: the syntax the R parser
# fixes at parse time and that has no `base::` spelling worth writing, listed
# once here. Integrity batch 13 fix round 1 (R13-P0-A): the list is trimmed to
# exactly these seven. Every earlier member -- `$`, `[[`, `@`, `==`, `&&`,
# `<`, `+`, `[`, `return`, ... -- WAS shadowable through the environment this
# file is sourced into (`$` in particular defeated all 20 gates: a fake `$`
# bound in the sourcing frame turned `private$require_validated_method(...)`
# into a caller-supplied accessor). Those spellings are refused inside a
# gate-chain body now; a gate reaches the validator only as
# `base::get("require_validated_method", envir = <sealed>)(...)` and a
# dependency accessor only as `base::get("<name>", envir = <sealed>)(...)`.
# `::` is not on the list because qualifying `::` with `::` is circular; it and
# these seven heads are the documented residual in every handover comment.
MM_GATE_SYNTAX_HEADS <- c("{", "(", "if", "for", "!", "<-", "function")

# Every call head inside `x` that is a BARE symbol not defined at this file's
# top level and not a syntax head, as deparsed text. Only `::`- and
# `:::`-qualified heads resolve through a namespace the sourcing environment
# cannot shadow, so only those are treated as not bare. Integrity batch 13 fix
# round 1 (R13-P0-A): a `$`/`[[`/`@` dispatch head is NO LONGER exempt -- those
# operators resolve through the environment this file is sourced into, so a
# fake `$`/`[[`/`@` bound there reaches into whatever object it likes; a
# gate-chain body must spell every reach into a sealed object as
# `base::get("<name>", envir = <sealed>)`.
.mm_unqualified_gate_heads <- function(x, fns = character(0)) {
  acc <- character(0)
  walk <- function(y) {
    if (is.call(y)) {
      h <- y[[1]]
      if (is.name(h)) {
        nm <- as.character(h)
        if (nzchar(nm) && !nm %in% MM_GATE_SYNTAX_HEADS && !nm %in% fns) acc <<- c(acc, nm)
      } else if (is.call(h)) {
        op <- as.character(h[[1]])[1]
        if (!op %in% c("::", ":::")) acc <<- c(acc, paste(deparse(h), collapse = ""))
      }
      for (i in seq_along(y)) if (i > 1L) tryCatch(walk(y[[i]]), error = function(e) NULL)
    } else if (is.pairlist(y)) {
      lst <- as.list(y)
      for (i in seq_along(lst)) {
        if (is.symbol(lst[[i]]) && !nzchar(as.character(lst[[i]]))) next
        tryCatch(walk(lst[[i]]), error = function(e) NULL)
      }
    }
    invisible(NULL)
  }
  tryCatch(walk(x), error = function(e) NULL)
  unique(acc)
}

# Every top-level binding of the file whose value is a bare-local closure, as
# a refusal sentence naming the binding, the file and the fix (integrity
# batch 13, R12-P0-2). A gate closure so built is the caller's `local`, not
# base's, however genuine the sealed construction beside it.
.mm_bare_local_closures <- function(path) {
  exprs <- tryCatch(parse(path, keep.source = FALSE), error = function(e) NULL)
  if (is.null(exprs)) return(character(0))
  out <- character(0)
  for (e in as.list(exprs)) {
    if (!is.call(e)) next
    op <- as.character(e[[1]])[1]
    if (!op %in% c("<-", "=", "<<-") || length(e) != 3L) next
    if (!(is.name(e[[2]]) || is.character(e[[2]]))) next
    nm <- as.character(e[[2]])
    cw <- .mm_closure_wrapper_violation(e[[3]], nm)
    if (is.null(cw)) next
    out <- c(out, sprintf("%s: %s", path, cw))
  }
  out
}

# Every root path constant a sealed construction reads that is not bound
# exactly once at the file's root BEFORE the construction statement, as a
# refusal sentence naming both statements (integrity batch 13, R12-P1-2).
.mm_root_order_violations <- function(path, binds) {
  exprs <- tryCatch(parse(path, keep.source = FALSE), error = function(e) NULL)
  if (is.null(exprs)) return(character(0))
  root_order <- attr(binds, "root_order")
  if (is.null(root_order)) return(character(0))
  out <- character(0)
  exprs_l <- as.list(exprs)
  for (i in seq_along(exprs_l)) {
    e <- exprs_l[[i]]
    if (!is.call(e)) next
    op <- as.character(e[[1]])[1]
    if (!op %in% c("<-", "=") || length(e) != 3L || !is.name(e[[2]])) next
    b <- tryCatch(.mm_trusted_block_env(e[[3]]), error = function(err) NULL)
    if (is.null(b)) next
    calls <- tryCatch(.mm_loads_into(b$st, b$env_sym), error = function(err) list())
    if (!length(calls)) next
    used <- unique(unlist(lapply(calls, function(cl)
      if (is.null(cl$expr)) character(0) else all.names(cl$expr))))
    used <- intersect(used, names(root_order))
    for (nm in used) {
      ords <- root_order[[nm]]
      if (length(ords) != 1L) {
        out <- c(out, sprintf("the path constant `%s` read by the construction `%s` (statement %d) is bound %d times at the root of this file (statements %s); a path constant is bound exactly once, before the construction that reads it (integrity batch 13, R12-P1-2)",
                              nm, as.character(e[[2]]), i, length(ords), paste(ords, collapse = ", ")))
      } else if (ords >= i) {
        out <- c(out, sprintf("the path constant `%s` (statement %d) is bound AFTER the construction `%s` (statement %d) that reads it; at source time the construction would read whatever the sourcing environment holds under that name, so the constant must precede the construction (integrity batch 13, R12-P1-2)",
                              nm, ords, as.character(e[[2]]), i))
      }
    }
  }
  unique(out)
}

# The binding index of a reference file with every per-file attribute the
# trust predicate reads carried on it: `attaches` (the file attaches a
# package), `reflective` (its top level uses a reflective or evaluating form)
# and, since integrity batch 12, `blocks` (a load-bearing block that qualifies
# as neither the validator environment nor a dependency environment). Every
# caller that judges a file's trusted loads -- the gating audit, manifest
# validation and manifest generation -- builds `binds` here so they cannot
# disagree about what was checked.
.mm_audit_binds <- function(ref, fns = NULL) {
  if (is.null(fns)) fns <- .mm_top_level_bindings(ref)
  binds <- .mm_binding_index(ref)
  attr(binds, "attaches") <- .mm_file_attaches(ref)
  attr(binds, "reflective") <- .mm_reflective_top_level_of_file(ref, binds)
  # Integrity batch 13: `bare_local` (a closure built with the caller's
  # `local`, R12-P0-2) and `order` (a path constant bound after, or more than
  # once around, the construction that reads it, R12-P1-2). Both are computed
  # BEFORE `blocks`, whose predicate reads them.
  attr(binds, "bare_local") <- .mm_bare_local_closures(ref)
  attr(binds, "order") <- .mm_root_order_violations(ref, binds)
  attr(binds, "blocks") <- .mm_trusted_block_role_violations(binds, names(fns),
                                                              .mm_shadow_census_of(fns))
  binds
}

# Is this right-hand side the shipped construction?
#
# Fix round 4. The first version asked whether `new.env`, `sys.source`,
# `lockEnvironment`, the path literal and `bindings = TRUE` APPEARED anywhere
# in the expression. That is a presence scan, and it certified three decoys: a
# block that loads the real thing into a discarded environment `d` and returns
# a fake (G11, G12), and a block whose entire construction sits in a dead
# `if (FALSE)` branch (G13). All three were demonstrated to perform no
# validation at run time. G13 is this batch's founding defect -- a construction
# inside `if (FALSE)` credited as real -- recurring in the predicate that
# decides whether an OBJECT is trustworthy: the gate chain had learned not to
# look inside `if` bodies, and this predicate had not.
#
# The evidence is now bound to the value the block RETURNS. The right-hand side
# must be a `local({ ... })` or `{ ... }` block (nested blocks are unwrapped)
# in which:
#
#   (1) the block's LAST expression is a bare symbol -- call it `e`;
#   (2) `e <- new.env(...)` is a TOP-LEVEL statement of the block, naming that
#       same symbol;
#   (3) `lockEnvironment(e, bindings = TRUE)` is a TOP-LEVEL statement of the
#       block, naming that same symbol, and appearing after (2);
#   (4) a `sys.source(<path>, envir = e)` names that same symbol somewhere in
#       the block, with a literal naming tools/method_manifest.R reachable in
#       the block;
#   (5) no TOP-LEVEL statement after the lock assigns to `e`.
#
# Clause (4) deliberately does NOT require the `sys.source` to be a top-level
# statement or its path to be a literal: in the shipped construction
# (skills/eda-standards/reference/eda.R:113-126) it sits inside a `for` loop
# and its path is the loop variable, with the literal in the vector being
# looped over. Clause (5) permits eda's `e$dependency_sha256 <- ...`, which
# writes into the environment BEFORE it is sealed.
# Fix round 5 (review B8). Unwrapping a block whose value is another block used
# to DISCARD every statement of the outer layer, so nothing ever judged it --
# while at run time an enclosing local() sits on the construction's lookup chain
# exactly as the reference file's top level does. The discarded statements are
# now carried out on the "enclosing" attribute so the caller can refuse a
# construction wrapped in a layer that binds anything.
# Integrity batch 12 (R11-P0-1). The wrapper is recognised in BOTH spellings,
# `local({ ... })` and `base::local({ ... })`, so that a block written with the
# bare wrapper is still judged as a block and refused with a reason naming the
# qualification requirement rather than falling through to "built in this
# file". Whether a bare `local` was seen on the way in is carried on the
# `bare_wrapper` attribute; the predicate refuses it.
#
# THE WRAPPER'S OWN FRAME (integrity batch 12, found while writing the runtime
# fixture for decision 1). base::local() is `function(expr, envir = new.env())
# eval.parent(substitute(eval(quote(expr), envir)))`: its DEFAULT `envir` is
# the expression `new.env()`, substituted into the call and evaluated in the
# CALLER's frame -- the environment this file is sourced into. A caller who
# pre-binds a fake `new.env` there therefore supplies the block's own frame
# even when every head in the block is `base::`-qualified, and a frame carrying
# an active binding for `e` hands the load one environment and the seal
# another (probe a local probe, 2026-09-05: the returned object
# held the fake validator). So the wrapper must be written
# `base::local({ ... }, envir = base::new.env())` -- the frame created by
# base's new.env(), with no arguments -- and the default is refused. The
# violation, if any, is carried on the `wrapper_violation` attribute.
.mm_trusted_block_statements <- function(rhs, depth = 0L, enclosing = list(),
                                         bare_wrapper = FALSE, wrapper_violation = NULL) {
  if (depth > 4L || is.null(rhs) || !is.call(rhs)) return(NULL)
  is_local <- .mm_is_bare_call_to(rhs, "local") || .mm_is_base_qualified_call_to(rhs, "local")
  if (is_local && length(rhs) >= 2L) {
    wv <- wrapper_violation
    if (.mm_is_base_qualified_call_to(rhs, "local") && is.null(wv)) {
      a <- as.list(rhs)[-1]
      anm <- names(a); if (is.null(anm)) anm <- rep("", length(a))
      # Integrity batch 13 (Codex round 12, P0-1): the frame's PARENT is the
      # sourcing environment unless it is named, and the block's own
      # statements (`for`, a pre-seal write) resolve through that frame. The
      # one admitted spelling names the parent: `base::new.env(parent =
      # base::baseenv())`, exactly one argument, written with the name.
      ne <- if (length(a) >= 2L) a[[2]] else NULL
      ne_args <- if (length(a) == 2L && is.call(ne)) as.list(ne)[-1] else list()
      ne_nm <- names(ne_args); if (is.null(ne_nm)) ne_nm <- rep("", length(ne_args))
      envir_ok <- length(a) == 2L && identical(anm[2], "envir") &&
        .mm_is_base_qualified_call_to(ne, "new.env") && length(ne_args) == 1L &&
        identical(ne_nm[1], "parent") &&
        .mm_is_base_qualified_call_to(ne_args[[1]], "baseenv") &&
        length(as.list(ne_args[[1]])) == 1L
      if (!envir_ok)
        wv <- "the block's wrapper must be written exactly `base::local({ ... }, envir = base::new.env(parent = base::baseenv()))`: local()'s default `envir` is the expression `new.env()` evaluated in the frame this file is sourced into, and `base::new.env()` without a named parent is parented on that same frame, so the block's own statements would resolve through whatever a caller pre-bound there (integrity batches 12 and 13)"
    }
    return(.mm_trusted_block_statements(rhs[[2]], depth + 1L, enclosing,
                                        bare_wrapper || .mm_is_bare_call_to(rhs, "local"), wv))
  }
  op <- as.character(rhs[[1]])[1]
  if (identical(op, "{")) {
    st <- as.list(rhs)[-1]
    if (!length(st)) return(NULL)
    last <- st[[length(st)]]
    # A block whose value is another block: unwrap and judge that one, keeping
    # this layer's other statements as the enclosing context.
    if (is.call(last) && (as.character(last[[1]])[1] %in% c("local", "{") ||
                          .mm_is_base_qualified_call_to(last, "local"))) {
      inner <- .mm_trusted_block_statements(last, depth + 1L,
                                            c(enclosing, st[-length(st)]), bare_wrapper,
                                            wrapper_violation)
      if (!is.null(inner)) return(inner)
    }
    attr(st, "enclosing") <- enclosing
    attr(st, "bare_wrapper") <- bare_wrapper
    attr(st, "wrapper_violation") <- wrapper_violation
    return(st)
  }
  NULL
}

# Calls that attach a package or environment to the search path. In a reference
# file that builds a trusted object these have no legitimate use, and their
# payload lives outside the repository where nothing the audit can resolve
# reaches it (fix round 5, review K1).
# Does this file make an attach call anywhere? Parsed, not grepped, so a call
# in a string or a comment does not count (fix round 5, K1).
.mm_file_attaches <- function(path) {
  exprs <- tryCatch(parse(path, keep.source = FALSE), error = function(e) NULL)
  if (is.null(exprs)) return(FALSE)
  found <- FALSE
  walk <- function(x) {
    if (!is.call(x)) return(invisible(NULL))
    h <- .mm_call_head_name(x)
    if (!is.na(h) && .mm_unqualify(h) %in% MM_TRUST_ATTACH_CALLS) found <<- TRUE
    for (k in seq_along(x)) if (!is.null(x[[k]])) tryCatch(walk(x[[k]]), error = function(e) NULL)
    invisible(NULL)
  }
  for (e in as.list(exprs)) tryCatch(walk(e), error = function(e) NULL)
  found
}

# Only calls that modify the SEARCH PATH belong here: the search path is on the
# construction's lookup chain, so a package attached before the construction can
# export `sys.source` and redirect the load. `requireNamespace()` and
# `loadNamespace()` load a namespace and return, leaving the search path
# untouched, so they cannot reach that chain; they were removed in integrity
# batch 11 (batch-10 review S5). `requireNamespace(..., quietly = TRUE)` is this
# repository's idiom for an optional dependency and is used by
# skills/nonmem-estimation/reference/bbr_estimation.R, which builds a trusted
# object.
MM_TRUST_ATTACH_CALLS <- c("library", "require", "attach", "attachNamespace")

# Names bound by an ENCLOSING layer of the trusted construction, plus any
# attach call made there. Any of these is a refusal: an enclosing layer can
# redefine the very primitives the construction is built from, which is how
# K5-redirect made the load fetch a different file while `trusted_loads` still
# reported the honest one.
# Names bound by a statement INSIDE the trusted block (fix batch 11, review
# R10-P0-2). The block's own frame is the INNERMOST link on the construction's
# lookup chain, so a fake `new.env` or `sys.source` bound one line above the
# construction shadows the primitive exactly as an enclosing layer does — and
# until this batch no rule looked at these statements at all. The block may
# therefore bind only the symbol it returns.
#
# Two structural allowances, neither able to shadow anything the construction
# calls:
#   - a `for` loop's index variable, because the enumerated loads may be
#     written as a loop over a literal vector (eda.R writes them that way);
#   - a formal of a function literal, which binds only inside that function's
#     own frame and is never on the construction's chain.
# The member name of a pre-seal `e$<name> <-` write is not a binding of the
# block either; it is judged by the position-4 write rules instead.
#
# SINCE FIX ROUND 1 OF THIS BATCH THIS IS NO LONGER THE PRIMARY RULE. It is a
# blacklist of binder CALL NAMES, and a new binder form slipped through it in
# three consecutive review rounds. The primary rule is now the positive
# enumeration in .mm_trusted_block_statement_ok(): a statement of the block must
# be one of seven permitted forms, and anything else is refused whether or not
# it binds a name this census knows about. Both rules run; this one is kept
# because it is the finer-grained of the two and reports which name was bound.
.mm_trusted_block_bindings <- function(st, env_sym) {
  acc <- character(0)
  collect <- function(x, skip) {
    if (!is.call(x)) return(invisible(NULL))
    op <- as.character(x[[1]])[1]
    if (op %in% c("<-", "=", "<<-") && length(x) == 3L) {
      base_nm <- tryCatch(.mm_assign_base_symbol(x[[2]]),
                          error = function(e) NA_character_)
      if (!identical(base_nm, env_sym)) {
        nms <- tryCatch(.mm_assign_names(x[[2]]), error = function(e) character(0))
        if (!length(nms)) nms <- "<binding>"
        acc <<- c(acc, setdiff(nms, skip))
      }
      collect(x[[3]], skip)
      return(invisible(NULL))
    }
    if (identical(op, "for") && length(x) == 4L) {
      # Fix round 1 of batch 11. The allowance used to be unconditional, so
      # `for (sys.source in list(<fake>)) NULL` shadowed a construction
      # primitive inside the very allowance this batch introduced. An index
      # that shadows a construction primitive, or the validator, is still
      # collected.
      idx <- if (is.name(x[[2]])) as.character(x[[2]]) else character(0)
      shadowing <- intersect(idx, c(MM_TRUST_CONSTRUCTION_PRIMITIVES,
                                    MM_VALIDATOR_NAME, env_sym))
      acc <<- c(acc, shadowing)
      idx <- setdiff(idx, shadowing)
      collect(x[[3]], skip)
      collect(x[[4]], c(skip, idx))
      return(invisible(NULL))
    }
    an <- tryCatch(.mm_assign_call_names(x), error = function(e) character(0))
    acc <<- c(acc, setdiff(an, c(skip, env_sym)))
    for (i in seq_along(x))
      if (!is.null(x[[i]])) tryCatch(collect(x[[i]], skip), error = function(e) NULL)
    invisible(NULL)
  }
  for (s in st) tryCatch(collect(s, character(0)), error = function(e) NULL)
  unique(acc[nzchar(acc)])
}

# --- the trusted block's permitted statement forms (fix round 1, batch 11) ---
#
# WHY A WHITELIST. The rule above is a blacklist: it enumerates the CALLS that
# bind a name, and refuses a block that binds anything other than the symbol it
# returns. Three consecutive review rounds each found a binder form the
# enumeration had not thought of -- `base::assign`, then `do.call(assign, ...)`,
# then `delayedAssign` / `makeActiveBinding` / `list2env` -- and each one was a
# demonstrated end-to-end bypass, with the audit reporting `gated = TRUE` and an
# honest `trusted_loads` while the real validator was never loaded. A blacklist
# of binder names is incomplete by construction: R has no closed set of ways to
# create a binding, and a new one is a silent regression rather than a refusal.
#
# The rule below is therefore POSITIVE. A statement of the trusted block must be
# one of the seven forms the construction actually needs; every other statement
# is a refusal naming the statement, whether or not anyone has worked out what
# it does. The forms are:
#
#   (a) `<e> <- base::new.env(parent = base::baseenv())`   the construction
#   (b) `base::sys.source(<path>, envir = <e>, ...)`       an enumerated load
#   (c) `for (<idx> in <literal vector or ROOT constant>) { ... }`
#       whose body contains only (b) and (d) statements, and whose index is
#       neither a construction primitive, nor the validator, nor <e>
#   (d) `if (!base::file.exists(<path>)) base::stop(<literals and <idx>>,
#       call. = FALSE)` -- the pre-load check, which binds nothing
#   (e) `<e>$<literal> <- <value>` / `<e>[["<literal>"]] <- <value>`
#       the pre-seal write, judged in full by position 4 of
#       .mm_trusted_env_uses()
#   (f) `base::lockEnvironment(<e>, bindings = TRUE)`    the seal
#   (g) the final bare `<e>`                             the returned value
#
# Since integrity batch 12 (SME decision 1, 2026-09-05) every primitive in
# (a), (b), (d) and (f) -- and the `base::local({ ... }, envir =
# base::new.env())` wrapper around the block -- is written with the EXACT
# `base::` qualification, tested by .mm_is_base_qualified_call_to(); the bare
# spelling that every reference used before is refused, with a message naming
# the requirement. `<path>` is the same set clause (4) resolves: a character
# literal, file.path() of character literals, the index of an enclosing `for`
# in form (c), or a ROOT constant of the file (R11-P0-3: resolved through the
# root-only index, never the recursive one).
#
# Two ROLES of block are admitted (SME decision 2, R11-P0-2): the VALIDATOR
# environment, whose loads are exactly tools/method_manifest.R, and a
# DEPENDENCY environment, whose loads are the file's declared dependencies
# named by a root constant and never the validator file. eda.R and
# psn_covsearch.R ship both; the other 18 references ship the validator block
# alone (byte-identical). eda.R's dependency block needs (a), (c) wrapping (d)
# and (b), (e), (f), (g); the standard block needs (a), (d), (b), (f), (g).
# Nothing else has to fit, which is what makes a whitelist affordable at all.
#
# WHAT THIS DOES NOT REACH, stated so the next round does not rediscover it.
# The whitelist governs the STATEMENTS OF THE BLOCK. It says nothing about the
# file's free-form top level, which remains a blacklist
# (MM_TRUST_CONSTRUCTION_PRIMITIVES read from .mm_top_level_bindings(), whose
# census is .mm_assign_call_names()), nor about the CONTENTS of a resolved load.
# The third handover of batch 11 -- a caller that shadows a base primitive in
# the environment the file is sourced into -- is closed for the seven
# primitives by the `base::` qualification (batch 12), which resolves through
# the sealed base namespace. What it does NOT close, verified by probe
# (a local probe, 2026-09-05): the `::` operator itself is an
# ordinary binding resolved through the same chain, so a caller who binds `::`
# in the sourcing environment receives every qualified call; and base::local()'s
# DEFAULT `envir` is evaluated in the caller's frame, which is why the wrapper
# must pass `envir = base::new.env()` explicitly. The `::` residual is the same
# class of act as modifying R and is stated in every reference's handover
# comment.

# Is this expression a path the trusted block may load from -- the same set
# clause (4) resolves: a character literal, file.path() of character literals,
# the index of an enclosing `for`, or a top-level constant of the file that
# .mm_static_path_vector() can resolve (eda.R's EDA_DEPENDENCY_FILES shape).
.mm_trusted_path_expr_ok <- function(e, idx, binds) {
  if (is.name(e)) {
    if (as.character(e) %in% idx) return(TRUE)
    return(!is.null(.mm_static_path_vector(e, binds)))
  }
  if (!is.na(.mm_static_path(e))) return(TRUE)
  !is.null(.mm_static_path_vector(e, binds))
}

# Form (b): an enumerated load into the returned symbol. The call must be
# EXACTLY `base::sys.source` (integrity batch 12, R11-P0-1; before that the
# bare `sys.source`, fix round 3 of batch 11, review G2-a: `.mm_call_head_name()`
# reads `shim$sys.source(...)` as `sys.source`, which is the wrong reading here
# for the same reason it is wrong for the construction), every named argument
# must be one of its formals, and every argument other than `file` and `envir`
# must be a literal flag.
.mm_trusted_stmt_is_load <- function(x, env_sym, idx, binds) {
  if (!.mm_is_base_qualified_call_to(x, "sys.source")) return(FALSE)
  formals_ok <- c("file", "envir", "chdir", "keep.source")
  actuals <- as.list(x)[-1]
  anm <- names(actuals); if (is.null(anm)) anm <- rep("", length(actuals))
  if (any(nzchar(anm) & !anm %in% formals_ok)) return(FALSE)
  if (length(actuals) > length(formals_ok)) return(FALSE)
  a <- .mm_match_args(x, formals_ok)
  if (is.null(a$envir) || !is.name(a$envir) ||
      !identical(as.character(a$envir), env_sym)) return(FALSE)
  if (is.null(a$file) || !.mm_trusted_path_expr_ok(a$file, idx, binds)) return(FALSE)
  for (k in setdiff(names(a), c("file", "envir"))) {
    v <- a[[k]]
    if (!(is.logical(v) && length(v) == 1L && !is.na(v))) return(FALSE)
  }
  TRUE
}

# Form (d): `if (!base::file.exists(<path>)) base::stop(<literals and indices>,
# call. = FALSE)`. There is no `else` branch, the condition is exactly that
# negated call, and every argument of the stop() is a character literal, an
# enclosing loop index, or `call. = FALSE`. The statement binds nothing by
# construction.
#
# Integrity batch 12 (R11-P0-4). The two heads used to be read with
# .mm_call_head_name(), which maps `shim$file.exists(...)` to `file.exists`,
# so an object-dispatched check passed the whitelist and ran a helper of the
# author's choosing at load time. Both heads are now the exact `base::`
# qualified spelling (decision 1), tested with .mm_is_base_qualified_call_to().
.mm_trusted_stmt_is_check <- function(x, idx, binds) {
  if (!is.call(x) || !identical(as.character(x[[1]])[1], "if")) return(FALSE)
  if (length(x) != 3L) return(FALSE)
  cond <- x[[2]]
  if (!is.call(cond) || !identical(as.character(cond[[1]])[1], "!") ||
      length(cond) != 2L) return(FALSE)
  fe <- cond[[2]]
  if (!.mm_is_base_qualified_call_to(fe, "file.exists")) return(FALSE)
  fa <- as.list(fe)[-1]
  if (length(fa) != 1L || !.mm_trusted_path_expr_ok(fa[[1]], idx, binds)) return(FALSE)
  body <- x[[3]]
  if (is.call(body) && identical(as.character(body[[1]])[1], "{")) {
    b <- as.list(body)[-1]
    if (length(b) != 1L) return(FALSE)
    body <- b[[1]]
  }
  if (!.mm_is_base_qualified_call_to(body, "stop")) return(FALSE)
  args <- as.list(body)[-1]
  anm <- names(args); if (is.null(anm)) anm <- rep("", length(args))
  if (!length(args)) return(FALSE)
  for (k in seq_along(args)) {
    v <- args[[k]]
    if (identical(anm[k], "call.")) {
      if (!identical(v, FALSE)) return(FALSE)
      next
    }
    if (nzchar(anm[k])) return(FALSE)
    if (is.character(v) && length(v) == 1L) next
    if (is.name(v) && as.character(v) %in% idx) next
    return(FALSE)
  }
  TRUE
}

# Is this statement one of the permitted forms? `idx` carries the indices of the
# enclosing `for`, if any; `binds` is the file's top-level binding index, used to
# resolve a `for` sequence written as a top-level constant.
.mm_trusted_block_statement_ok <- function(x, env_sym, binds, idx = character(0),
                                           in_loop = FALSE) {
  if (is.name(x)) return(identical(as.character(x), env_sym))   # (g)
  if (!is.call(x)) return(FALSE)
  op <- as.character(x[[1]])[1]

  # (a) and (e): assignment forms.
  if (op %in% c("<-", "=") && length(x) == 3L) {
    lhs <- x[[2]]
    if (is.name(lhs) && identical(as.character(lhs), env_sym))
      return(.mm_is_base_qualified_call_to(x[[3]], "new.env"))   # (a)
    if (is.call(lhs) && length(lhs) >= 3L &&
        as.character(lhs[[1]])[1] %in% c("$", "[[") &&
        is.name(lhs[[2]]) && identical(as.character(lhs[[2]]), env_sym)) {
      ix <- lhs[[3]]                                            # (e)
      name_ok <- (identical(as.character(lhs[[1]])[1], "$") && is.name(ix)) ||
                 (is.character(ix) && length(ix) == 1L)
      # ... and the VALUE is a literal or a call built only from the
      # enumerated qualified heads (integrity batch 13, decision 1A): the
      # write is executed in the block's frame at source time, so a bare
      # `vapply` or `paste0` there was the caller's.
      return(name_ok && .mm_trusted_write_value_ok(x[[3]], idx, binds))
    }
    return(FALSE)
  }

  if (.mm_trusted_stmt_is_load(x, env_sym, idx, binds)) return(TRUE)   # (b)
  if (.mm_trusted_stmt_is_check(x, idx, binds)) return(TRUE)           # (d)

  # (f) the seal, written exactly `base::lockEnvironment` (batch 12).
  if (.mm_is_base_qualified_call_to(x, "lockEnvironment")) {
    a <- .mm_match_args(x, c("env", "bindings"))
    return(!is.null(a$env) && is.name(a$env) &&
             identical(as.character(a$env), env_sym) &&
             isTRUE(a$bindings))
  }

  # (c) a `for` over a statically resolvable vector. Not nested: one level is
  # all eda.R needs, and a nested loop would need its own sequence rule.
  if (identical(op, "for") && length(x) == 4L && !in_loop) {
    if (!is.name(x[[2]])) return(FALSE)
    iname <- as.character(x[[2]])
    if (iname %in% c(MM_TRUST_CONSTRUCTION_PRIMITIVES, MM_VALIDATOR_NAME,
                     env_sym)) return(FALSE)
    if (is.null(.mm_static_path_vector(x[[3]], binds))) return(FALSE)
    body <- x[[4]]
    stmts <- if (is.call(body) && identical(as.character(body[[1]])[1], "{"))
               as.list(body)[-1] else list(body)
    if (!length(stmts)) return(FALSE)
    for (s in stmts)
      if (!.mm_trusted_block_statement_ok(s, env_sym, binds, c(idx, iname),
                                          in_loop = TRUE)) return(FALSE)
    return(TRUE)
  }

  FALSE
}

# Every statement of the block that is not a permitted form, each named.
.mm_trusted_block_statement_violations <- function(st, env_sym, binds = list()) {
  bad <- character(0)
  for (i in seq_along(st)) {
    if (i == length(st)) next            # the final bare symbol: clause (1)
    s <- st[[i]]
    if (!.mm_trusted_block_statement_ok(s, env_sym, binds)) {
      txt <- tryCatch(paste(deparse(s), collapse = " "),
                      error = function(e) "<unparseable>")
      if (nchar(txt) > 160L) txt <- paste0(substr(txt, 1L, 157L), "...")
      # Name the qualification requirement when that is what the statement
      # gets wrong (integrity batch 12, decision 1): the bare `sys.source(...)`
      # that every reference used to write is the refusal an author will meet
      # first, and "not a permitted form" does not tell them what to write.
      unq <- .mm_unqualified_primitive_heads(s)
      if (length(unq)) {
        bad <- c(bad, sprintf("the trusted block calls `%s`, but every construction primitive must be written with the exact `base::` qualification (%s) so that it resolves through the sealed base namespace and not through the environment this file is sourced into: `%s`",
                              unq[1], paste0("base::", MM_TRUST_BASE_QUALIFIED_HEADS, collapse = ", "), txt))
        next
      }
      # A `for` whose sequence does not resolve: name the path rule and, when
      # the sequence or its root constant is a bare `c(...)`/`file.path(...)`,
      # the qualification to write (integrity batch 13).
      if (is.call(s) && identical(as.character(s[[1]])[1], "for") && length(s) == 4L &&
          is.null(.mm_static_path_vector(s[[3]], binds))) {
        bad <- c(bad, sprintf("the trusted block's `for` iterates `%s`, which does not resolve to a literal path vector: the sequence must be a root constant of this file bound exactly once to `base::c(...)` of string literals, `base::file.path(...)` of string literals, or a single string literal%s: `%s`",
                              paste(deparse(s[[3]]), collapse = " "), .mm_path_hint(s[[3]], binds), txt))
        next
      }
      # A pre-seal write of the right shape whose VALUE is not admissible
      # (integrity batch 13): name the enumerated heads.
      if (is.call(s) && as.character(s[[1]])[1] %in% c("<-", "=") && length(s) == 3L &&
          is.call(s[[2]]) && length(s[[2]]) >= 3L &&
          as.character(s[[2]][[1]])[1] %in% c("$", "[[") &&
          is.name(s[[2]][[2]]) && identical(as.character(s[[2]][[2]]), env_sym)) {
        bad <- c(bad, sprintf("the pre-seal write's value must be a literal, or a call whose every head is one of %s, whose symbols are the loop index, a formal of the function literal passed as FUN, or a root path constant of this file, and whose only unqualified head is that `function` literal; anything else executes through the environment this file is sourced into (integrity batch 13, decision 1A): `%s`",
                              paste(MM_TRUST_WRITE_VALUE_HEADS, collapse = ", "), txt))
        next
      }
      bad <- c(bad, sprintf("the trusted block contains a statement that is not one of the permitted construction forms (the base::new.env() construction, a base::sys.source() into it, a `for` over a literal vector of such loads, an `if (!base::file.exists(...)) base::stop(...)` check, a pre-seal `%s$<name> <-` write, the base::lockEnvironment() seal, and the final bare symbol): `%s`",
                            env_sym, txt))
    }
  }
  bad
}

.mm_enclosing_bindings <- function(st) {
  encl <- attr(st, "enclosing")
  if (is.null(encl) || !length(encl)) return(character(0))
  acc <- character(0)
  for (x in encl) {
    if (!is.call(x)) next
    op <- as.character(x[[1]])[1]
    if (op %in% c("<-", "=", "<<-") && length(x) == 3L) {
      nms <- tryCatch(.mm_assign_names(x[[2]]), error = function(e) character(0))
      acc <- c(acc, if (length(nms)) nms else "<binding>")
    } else if (op %in% MM_TRUST_ATTACH_CALLS) {
      acc <- c(acc, paste0(op, "()"))
    }
  }
  unique(acc[nzchar(acc)])
}

# --- clause (4): tie the loaded path to its call, and enumerate every load ---
# Integrity batch 10, closing the batch-9 review's Final check 3 residual.
# Clause (4) used to be two independent presence checks — SOME sys.source
# targets the returned environment, and SOME literal in the block names
# tools/method_manifest.R — with nothing tying the literal to that call and
# nothing constraining any OTHER file loaded into the object before it was
# sealed. Four demonstrated shapes passed: a decoy literal beside a load of a
# different file (K6), the qualifying load parked in a dead branch (K7), and a
# qualifying load followed by a second one (K10, and K11 via base source()).
#
# THE INVARIANT (fix round 4, review N9). State it once, in its own terms, so
# the next gap shows up as an ABSENCE rather than as a demonstrated bypass:
#
#     NOTHING MAY ENTER THE SEALED OBJECT'S LOOKUP CHAIN THAT THE AUDIT HAS
#     NOT RESOLVED AND JUDGED.
#
# The validator's closure IS the sealed object, so an unqualified call inside
# it resolves: call frame -> e -> parent(e). That chain has exactly three
# contributors, and each one is judged here:
#
#   (a) what the LOADS put in    -> .mm_resolve_trusted_loads(): every load is
#                                   enumerated, statically resolved, and
#                                   repository-relative.
#   (b) what a PRE-SEAL WRITE puts in -> position 4: literal name, non-function
#                                   value, and the name may not collide with
#                                   the validator, this file's bindings, a
#                                   loaded file's bindings, or baseenv().
#   (c) the PARENT               -> clause 2 requires new.env(parent = baseenv())
#                                   exactly, so the tail of the chain is base
#                                   and nothing else.
#
# Rounds 1-5 each closed one face of this and left the next one open, because
# the rule was written against demonstrated shapes rather than against the
# chain. B5 was (a) and half of (b); B6 was the rest of (b); B7 was (c); B8 was
# the CONSTRUCTION's own chain -- an enclosing local() can redefine the
# primitives the object is built from, and the unwrapper used to discard those
# statements before any rule saw them.
#
# WHAT THIS GUARANTEES, AND WHERE IT HANDS OVER (review N11). Guaranteed: the
# sealed object's lookup chain is exactly (the resolved loads) + baseenv(); the
# construction is performed by unshadowed primitives, in a file that attaches
# no package and with no enclosing layer that binds anything; and every
# pre-seal write is a literal-named, non-colliding, non-function value.
#
# NOT guaranteed, and not guaranteeable here: the CONTENTS of the resolved
# loads. A load is sourced INTO the object, so a loaded file's top level IS the
# object, and a dependency that binds a base name shadows it. The one case that
# needs no name list is refused (a dependency defining the validator); the rest
# is the handover, and it is the right one -- an accomplice must be a file in
# this repository, so it appears in `git diff`, sits under the write guard, and
# is named to the SME by the `trusted_loads` column at approval.
#
# tools/method_manifest.R itself is where the audit necessarily stops: that
# file IS the validator and IS this audit, and no static rule can bootstrap
# trust in the file it lives in. Its controls are the write guard, the
# .harness-unlock window and the SME's review of the diff -- the same three
# that governed this batch's own edits to it.
#
# SAME-PROCESS REFLECTION IS OUT OF SCOPE (Codex round 12, R12-P2-3; SME
# decision 3, 2026-09-05). A locked, base-parented R environment protects
# against ORDINARY rebinding: a caller who assigns over a name, sources into
# the sourcing environment, or pre-binds a fake before the reference loads.
# It does not protect against a hostile caller in the same R process using
# reflection -- `environment(fn)$sealed`, `parent.env<-`, an active binding,
# `unlockBinding` on a namespace -- because R offers no in-process boundary
# against code that can already run arbitrary R. The controls for that
# caller are the trusted loader (tools/load_reference.R: authenticated bytes
# are the only bytes evaluated), the manifest digests, the write guard and
# the SME's review of the diff; a claim that "callers cannot change what an
# accessor reaches" is made only for ordinary rebinding, never for
# reflection. docs/PORTABILITY.md states the same boundary for readers who
# never open this file.
#
# WHAT THE RULE BELOW ACTUALLY COVERS, stated exactly, because the batch-9
# review's whole point was that this boundary be written down rather than
# rediscovered a round later. Three parts:
#
#   (i)  .mm_trusted_env_uses() — every USE of the returned symbol in the block
#        must sit in one of five allowed positions (construction, enumerated
#        load, seal, pre-seal `e$<name> <-` write, final bare symbol).
#        Deny-by-default: anything else is refused whether or not it has been
#        thought of. This is what catches list2env/assign/eval/local/
#        `environment<-`/aliasing, none of which are "loads" at all.
#   (ii) .mm_loads_into() — every source()/sys.source() targeting the symbol is
#        enumerated, including inside a `for` body AND its sequence expression,
#        and namespace-qualified spellings are unqualified for the census and
#        then refused on their own account.
#   (iii).mm_resolve_trusted_loads() — every enumerated path must resolve
#        statically AND be repository-relative (including the qualifying one);
#        at least one must be tools/method_manifest.R; any other must come from
#        a top-level literal constant.
#   (iv) .mm_trusted_block_statement_ok() — fix round 1 of batch 11. Every
#        STATEMENT of the block must be one of the seven permitted construction
#        forms, enumerated positively where that function is defined. (i)
#        inverted the rule for USES of the returned symbol; (iv) inverts it for
#        the statements themselves, which is what closes the binder-form class
#        that three rounds of blacklisting failed to close.
#
# The boundary this does NOT reach, and cannot: the audit cannot know that a
# resolvable, repository-relative path is benign, nor prove that an arbitrary
# value expression written into the object before the seal is not a function.
# It can only make the load set statically resolvable, repository-scoped and
# reportable. Past that point the controls are the reference file's manifest
# digest and the SME's review of the diff — which is why the resolved list is
# reported in the audit output rather than merely consumed here.
#
# Fix round, from the independent review: (i) and the qualified-call and
# for-sequence and harness-relative holes were all demonstrated at run time
# against the first cut of this rule, with the real validator in scope. The
# first cut's own comment claimed it resolved "EVERY load", and it did not.
# Be suspicious of a comment in this file that claims completeness.

# The primitives the trusted construction itself calls. The block is evaluated
# with the reference file's top level on its chain, so a top-level binding of
# any of these redefines how the object is built or sealed (fix round 4, B7).
#
# Integrity batch 12. Since decision 1 the construction calls its primitives
# through `base::`, so a top-level binding of `sys.source` in the reference
# file no longer changes what the construction does -- but `::` itself, and
# the syntax primitives (`{`, `(`, `<-`, `if`, `for`, `!`, `function`), are
# ordinary bindings R resolves through the same chain (probe
# a local probe: a `::` bound in the sourcing environment
# received `base::paste`). A reference file that binds any of them at its top
# level is refused; the original seven stay on the list as an independent
# refusal.
#
# Integrity batch 13 (decision 1A). The names a construction or a closure now
# executes beyond the seven -- `c`, `vapply`, `sapply`, `paste0`, `character`,
# `file.path`, `sprintf` -- join the list: a reference file that binds one of
# them at its top level is refused, as an independent refusal beside the
# qualification requirement inside the block.
MM_TRUST_CONSTRUCTION_PRIMITIVES <- c("new.env", "baseenv", "sys.source",
                                      "source", "lockEnvironment", "local",
                                      "file.exists", "assign", "list2env",
                                      "stop", "::", "{", "(", "<-", "=", "if",
                                      "for", "!", "function",
                                      "c", "vapply", "sapply", "paste0",
                                      "character", "file.path", "sprintf")

MM_TRUST_QUALIFYING_LOAD <- "(^|/)tools/method_manifest\\.R$"

# A path expression resolved to the single literal path it denotes: a character
# literal, or file.path() of character literals. NA when not statically
# resolvable (an interpolated path such as paste0("tools/", nm, ".R") is not).
.mm_static_path <- function(e) {
  if (is.character(e) && length(e) == 1L) return(e)
  # Integrity batch 13 (decision 1A): `file.path` is an executable name read
  # at source time, so only the exact `base::file.path(...)` spelling resolves.
  if (.mm_is_base_qualified_call_to(e, "file.path")) {
    parts <- as.list(e)[-1]
    if (length(parts) &&
        all(vapply(parts, function(p) is.character(p) && length(p) == 1L, logical(1))))
      return(paste(unlist(parts), collapse = "/"))
  }
  NA_character_
}

# A path expression resolved to the vector of paths it denotes, each tagged
# with its provenance: "literal" for a path written in the block itself,
# "toplevel" for one reached through a top-level constant of the file. Handles
# c(...) and one level of indirection through `binds`. NULL when any element is
# not statically resolvable — an unresolvable load is never credited.
.mm_static_path_vector <- function(e, binds, allow_indirect = TRUE) {
  if (is.name(e)) {
    if (!allow_indirect) return(NULL)
    nm <- as.character(e)
    rhss <- binds[[nm]]
    # A top-level constant is credited only when it is bound exactly once in
    # the whole file; a rebound name says nothing about its value here.
    if (is.null(rhss) || length(rhss) != 1L) return(NULL)
    # ... AND that one binding is a plain root assignment (integrity batch 12,
    # R11-P0-3). The recursive index above counts a function-local `DEPS <-`
    # as the file's single binding of DEPS, which it is -- but it is not the
    # binding the trusted block reads at run time. The root-only index carried
    # on `binds` by .mm_binding_index() is the only source a path constant may
    # be resolved from; a `binds` built without it resolves nothing.
    root <- attr(binds, "root")
    if (is.null(root)) return(NULL)
    rr <- root[[nm]]
    if (is.null(rr) || length(rr) != 1L) return(NULL)
    inner <- .mm_static_path_vector(rr[[1]], binds, allow_indirect = FALSE)
    if (is.null(inner)) return(NULL)
    inner$src <- rep("toplevel", length(inner$path))
    return(inner)
  }
  # Integrity batch 13 (Codex round 12, P0-1). The vector's `c` is executed
  # in the environment this file is sourced into; a stateful fake `c()` that
  # answers its first call with an attacker's path redirected eda.R's
  # dependency load while the audit reported the honest vector. Only the exact
  # `base::c(...)` resolves; the bare spelling resolves to NULL here, and the
  # refusal that follows names the qualification (.mm_path_hint).
  if (.mm_is_base_qualified_call_to(e, "c")) {
    parts <- as.list(e)[-1]
    if (!length(parts)) return(NULL)
    acc <- list(path = character(0), src = character(0))
    for (p in parts) {
      sub <- .mm_static_path_vector(p, binds, allow_indirect = allow_indirect)
      if (is.null(sub)) return(NULL)
      acc$path <- c(acc$path, sub$path); acc$src <- c(acc$src, sub$src)
    }
    return(acc)
  }
  p <- .mm_static_path(e)
  if (is.na(p)) return(NULL)
  list(path = p, src = "literal")
}

# A sentence naming the qualification requirement when a path expression (or
# the root constant it names) is written with a bare `c(...)` or
# `file.path(...)`; "" otherwise. Appended to the resolver's refusal so an
# author meets the fix, not only the fact (integrity batch 13).
.mm_path_hint <- function(e, binds) {
  target <- e
  if (is.name(e)) {
    root <- attr(binds, "root")
    rr <- if (is.null(root)) NULL else root[[as.character(e)]]
    if (!is.null(rr) && length(rr) == 1L) target <- rr[[1]]
  }
  if (!is.call(target)) return("")
  hq <- .mm_call_head_name(target)
  if (is.na(hq)) return("")
  bare <- .mm_unqualify(hq)
  if (bare %in% c("c", "file.path") && !.mm_is_base_qualified_call_to(target, bare))
    return(sprintf("; a path vector read by a sealed construction must be written exactly `base::%s(...)` (or be a single string literal), because the bare `%s` resolves through the environment this file is sourced into and a stateful fake redirects the load (integrity batch 13, R12-P0-1)",
                   bare, hq))
  ""
}

# Is this a dependency path the harness could legitimately ship alongside the
# validator: repository-relative, and not escaping the repository root?
.mm_is_harness_relative <- function(p) {
  !grepl("^([A-Za-z]:)?[/\\\\]", p) && !grepl("(^|/)\\.\\.(/|$)", p) &&
    !grepl("^~", p)
}

# Every source()/sys.source() in the block that loads into `env_sym`, with each
# call's path expression and the for-loop sequence it iterates, if any.
# Returns a list of list(expr = <path expression>, loop = TRUE/FALSE).
.mm_loads_into <- function(st, env_sym) {
  acc <- list()
  walk <- function(x, loops) {
    if (!is.call(x)) return(invisible(NULL))
    hq <- .mm_call_head_name(x)
    # Fix round (review B2). The head name deliberately KEEPS its namespace
    # qualifier, which is right for the validator-identity test but wrong for a
    # load census: `base::sys.source(...)` does exactly what `sys.source(...)`
    # does, and matching on the qualified spelling made it invisible to the
    # enumeration. The census unquallifies; the qualified spelling is then
    # refused on its own account, consistent with batch 9's rule that a
    # namespace-qualified call has no legitimate place in this construction.
    h <- if (is.na(hq)) hq else .mm_unqualify(hq)
    if (identical(h, "for") && length(x) == 4L && is.name(x[[2]])) {
      loops <- c(loops, stats::setNames(list(x[[3]]), as.character(x[[2]])))
      # Fix round (review S1). The SEQUENCE expression is walked too, with the
      # OUTER loop bindings: a source() placed in it used to be invisible
      # because only the body was descended into.
      tryCatch(walk(x[[3]], loops), error = function(e) NULL)
      tryCatch(walk(x[[4]], loops), error = function(e) NULL)
      return(invisible(NULL))
    }
    if (identical(h, "sys.source") || identical(h, "source")) {
      a <- if (identical(h, "sys.source"))
             .mm_match_args(x, c("file", "envir", "chdir", "keep.source"))
           else .mm_match_args(x, c("file", "local", "echo"))
      target <- if (identical(h, "sys.source")) a$envir else a$local
      if (!is.null(target) && is.name(target) &&
          identical(as.character(target), env_sym)) {
        fx <- a$file
        # Integrity batch 12 (decision 1): the ONE admitted spelling of a load
        # is `base::sys.source(...)`. The flag keeps its historical name; it
        # now means "not written as base::sys.source" -- the bare spelling,
        # `base:::`, another package, `source(local = )` and `obj$sys.source`
        # all set it, and the resolver refuses on it.
        qualified <- !.mm_is_base_qualified_call_to(x, "sys.source")
        # A bare loop variable stands for every element of its sequence.
        if (!is.null(fx) && is.name(fx) && as.character(fx) %in% names(loops))
          acc[[length(acc) + 1L]] <<- list(expr = loops[[as.character(fx)]],
                                           qualified = qualified, head = hq)
        else
          acc[[length(acc) + 1L]] <<- list(expr = fx, qualified = qualified,
                                           head = hq)
      }
    }
    for (i in seq_along(x))
      if (!is.null(x[[i]])) tryCatch(walk(x[[i]], loops), error = function(e) NULL)
    invisible(NULL)
  }
  for (x in st) tryCatch(walk(x, list()), error = function(e) NULL)
  acc
}

# Every USE of the trusted symbol in the block, judged deny-by-default.
#
# Fix round (review S2). Enumerating source() calls answers "which files are
# loaded", but the question the gate actually depends on is "what gets into the
# object before it is sealed". Those are not the same: clause 5 only refuses a
# REBINDING of the symbol after the lock, while mutation of the environment's
# CONTENTS before the lock is deliberately allowed, because eda.R writes its
# dependency digests that way. `list2env(as.list(d), envir = e)` walks straight
# through that allowance, and so do assign(envir=), eval(envir=), local(envir=),
# `environment<-`, and aliasing the object into another symbol and loading into
# that. Enumerating those functions by name would be another "does X appear"
# blacklist, and this file has been burned by that shape three times.
#
# So the rule is inverted: EVERY occurrence of the symbol must sit in one of a
# small closed set of positions, and anything else is refused whether or not
# anyone has thought of it yet:
#   1. `e <- new.env(...)`                      the construction
#   2. `sys.source(..., envir = e)` / `source(..., local = e)`   an enumerated load
#   3. `lockEnvironment(e, bindings = TRUE)`    the seal
#   4. `e$<name> <- <value>` before the seal    the eda.R digest write
#   5. the block's final bare `e`               the returned value
#
# W10 is the worked example of why the inversion earns its keep: `p <- e;
# sys.source("dep.R", envir = p)` is refused without the audit modelling
# aliasing at all, because the symbol simply appears somewhere that is not on
# the list.
#
# POSITION 4, exactly (fix round 2, after two rounds of it being written more
# permissively than the one write it exists to permit). All four must hold:
#   (a) the target is `e$<name>` or `e[["<name>"]]` -- the binding name is
#       written LITERALLY. A computed index (`e[[nm]] <-`) is refused: it
#       cannot be judged, and reading it with as.character() yields the index
#       variable's spelling rather than its value, which is how W4/W8/W9 wrote
#       over `require_validated_method` itself.
#   (b) the write is before the seal.
#   (c) the name is neither the validator, nor any top-level binding of this
#       file -- which covers entry points and gate-chain members, since both
#       are top-level bindings -- nor any top-level binding of a file LOADED
#       into the object, nor a binding of baseenv() (the object's parent). (c) is the one that matters most: the validator's
#       closure IS `e`, so a write onto a loaded name rewrites the code the
#       gate runs. `e$.mm_file_sha256 <- ...` turned the manifest digest-drift
#       hard stop into TRUE from inside the reference file (fix round 3, B5),
#       defeating the very control the batch-9 review designated as the
#       backstop past the audit's reach.
#   (d) the value is not functionish: not a function literal (parentheses
#       unwrapped first), not an extraction out of another object, and not a
#       call to a function-returning helper on the list below.
#
# What (d) CANNOT do, stated so the next round does not rediscover it: decide
# an arbitrary call. `eda.R` writes `e$dependency_sha256 <- vapply(...)`, so
# refusing every call-valued write would refuse the only shipped use of this
# construction in the repository. A call that returns a function and is not on
# the list is therefore still accepted as a value -- but by (a) and (c) it can
# never land on the validator, on any name this file defines, or on any name
# the loaded files define -- which is what made W8/W9 and G1/G2/G3 exploitable.
#
# What bounds the remaining residual is worth naming precisely, because it is
# NOT position 4 (review note N7). A position-4 write of a NEW name, colliding
# with nothing, may hold a function. That binding is inert: it cannot be
# reached as the gate, because the gate-chain rule credits a chain member only
# when it is a function DEFINED IN THE REFERENCE FILE, so
# `.priv$sneaky(...)` is refused as a gate by that independent control. Do not
# assume position 4 is carrying that weight.
MM_TRUST_ALLOWED_WRITE_LIMIT <- c("function", "\\", "match.fun", "as.function",
                                  "getFromNamespace", "get", "get0", "mget",
                                  "Recall")

# Fix round 2 (review B4). `(function() TRUE)` is a call whose head is `(`, so
# the literal test used to be false and the value was credited as a
# non-function. Parentheses are unwrapped first; `\\(x)` parses to the same
# `function` node, so the shorthand is covered by the same test once it is.
#
# An extraction (`src$x`, `src[["x"]]`, `obj@x`) is also refused as a value: it
# lifts something out of another object, and what that object holds cannot be
# read from this file. That is the W9 shape.
#
# The LIMIT of this test, stated because the first cut of position 4 claimed
# more than it delivered: it CANNOT decide an arbitrary call. `eda.R` writes
# `vapply(...)`, so refusing every call-valued write would refuse the one
# shipped use of this construction in the repository. A call that returns a
# function and is not on the list below is therefore still accepted here, and
# is bounded by the literal-name rule (it cannot land on the validator or on
# any name this file binds), the manifest digest, and the SME's diff review.
.mm_unparen <- function(e) {
  for (i in seq_len(8L)) {
    if (is.call(e) && length(e) == 2L && identical(as.character(e[[1]])[1], "(")) e <- e[[2]]
    else break
  }
  e
}

.mm_expr_is_functionish <- function(e) {
  e <- .mm_unparen(e)
  if (is.function(e)) return(TRUE)
  if (!is.call(e)) return(FALSE)
  if (identical(as.character(e[[1]])[1], "function")) return(TRUE)
  if (as.character(e[[1]])[1] %in% c("$", "[[", "@")) return(TRUE)
  h <- .mm_call_head_name(e)
  !is.na(h) && .mm_unqualify(h) %in% MM_TRUST_ALLOWED_WRITE_LIMIT
}

# Does the block write any binding into the object before it is sealed? Used
# only to scope the "loaded file unreadable" refusal to the blocks whose
# verdict actually depends on reading it (fix round 3).
.mm_has_preseal_write <- function(st, env_sym) {
  found <- FALSE
  walk <- function(x) {
    if (!is.call(x)) return(invisible(NULL))
    if (as.character(x[[1]])[1] %in% c("<-", "=", "<<-") && length(x) == 3L) {
      lhs <- x[[2]]
      if (is.call(lhs) && length(lhs) >= 3L &&
          as.character(lhs[[1]])[1] %in% c("$", "[[") &&
          is.name(lhs[[2]]) && identical(as.character(lhs[[2]]), env_sym))
        found <<- TRUE
    }
    for (k in seq_along(x)) if (!is.null(x[[k]])) tryCatch(walk(x[[k]]), error = function(e) NULL)
    invisible(NULL)
  }
  for (x in st) tryCatch(walk(x), error = function(e) NULL)
  found
}

# The top-level binding names of every file loaded into the trusted object.
#
# Fix round 3 (review B5). `e` is filled by sys.source()ing these files, and the
# validator's closure IS `e`, so every helper the validator calls resolves
# there: require_validated_method() -> validate_method_manifest() ->
# .mm_file_sha256(). A pre-seal write onto any of those names rewrites the
# integrity check from inside the reference file, with no accomplice file and
# no digest change to the loaded file itself. That defeats the DIGEST BACKSTOP,
# which is what the batch-9 review designated as the control past the audit's
# reach -- so it is not a boundary to document, it is a hole to close.
#
# A file that does not exist at audit time contributes no names; a reference
# that sys.source()s a missing file fails at run time regardless.
.mm_load_binding_names <- function(paths) {
  acc <- character(0)
  for (p in paths) {
    if (!length(p) || is.na(p) || !nzchar(p) || !file.exists(p)) next
    acc <- c(acc, tryCatch(names(.mm_top_level_bindings(p)),
                           error = function(e) character(0)))
  }
  unique(acc[nzchar(acc)])
}

.mm_trusted_env_uses <- function(st, env_sym, lock_i, fns = character(0),
                                 load_names = character(0)) {
  bad <- character(0)
  is_sym <- function(x) is.name(x) && identical(as.character(x), env_sym)
  walk <- function(x, i) {
    if (is.name(x)) {
      if (is_sym(x))
        bad <<- c(bad, sprintf("'%s' is referred to outside the construction, the enumerated load(s), the seal and a pre-seal `%s$<name> <-` write",
                               env_sym, env_sym))
      return(invisible(NULL))
    }
    if (!is.call(x)) return(invisible(NULL))
    hq <- .mm_call_head_name(x)
    h  <- if (is.na(hq)) hq else .mm_unqualify(hq)
    skip <- function(except) {
      for (k in seq_along(x)[-1])
        if (!is.null(x[[k]]) && !identical(x[[k]], except)) tryCatch(walk(x[[k]], i), error = function(e) NULL)
    }
    if (!is.na(h) && h %in% c("<-", "=", "<<-") && length(x) == 3L) {
      lhs <- x[[2]]
      # 1. e <- new.env(...): the RHS is still judged, so `e <- new.env(parent = e)` is caught.
      if (is_sym(lhs)) { tryCatch(walk(x[[3]], i), error = function(e) NULL); return(invisible(NULL)) }
      # 4. e$name <- value / e[["name"]] <- value, before the seal.
      #
      # Fix round 2 (review B3). The binding NAME must be written literally.
      # `e$foo` carries the name as a symbol and `e[["foo"]]` as a string, and
      # both are the name itself; `e[[nm]]` carries an EXPRESSION, and the
      # first cut resolved it with as.character(), which returns the index
      # VARIABLE'S SPELLING ("nm") rather than its value. Both guards below
      # were then comparing against the wrong string, so a reference could
      # write any binding under any name -- including the validator's -- and
      # copy a whole second environment in a loop one binding at a time (W4,
      # W8, W9, all of which defeated the real validator at run time).
      #
      # A computed index cannot be judged, so it is not credited. This makes
      # position 4 exactly the eda.R shape and nothing else.
      if (is.call(lhs) && length(lhs) >= 3L &&
          as.character(lhs[[1]])[1] %in% c("$", "[[") && is_sym(lhs[[2]])) {
        idx <- lhs[[3]]
        literal <- (identical(as.character(lhs[[1]])[1], "$") && is.name(idx)) ||
                   (is.character(idx) && length(idx) == 1L)
        if (!literal) {
          bad <<- c(bad, sprintf("a binding is written into '%s' under a computed name (`%s`); the name of a pre-seal write must be written literally, so that the audit can see which binding it lands on",
                                 env_sym, paste(deparse(idx), collapse = " ")))
          tryCatch(walk(x[[3]], i), error = function(e) NULL)
          return(invisible(NULL))
        }
        nm <- as.character(idx)[1]
        if (!is.na(lock_i) && i > lock_i)
          bad <<- c(bad, sprintf("a binding is written into '%s' after it is sealed", env_sym))
        if (identical(nm, MM_VALIDATOR_NAME))
          bad <<- c(bad, sprintf("'%s' is written into '%s' by this file; the validator must come from tools/method_manifest.R, never from the reference",
                                 MM_VALIDATOR_NAME, env_sym))
        # Entry points and gate-chain members are top-level bindings of the
        # reference file, so refusing every name this file already binds covers
        # both without enumerating them separately.
        if (nm %in% fns)
          bad <<- c(bad, sprintf("'%s' is written into '%s' but is already a top-level binding of this reference file; a pre-seal write must not shadow a name the file defines (an entry point or a gate-chain member)",
                                 nm, env_sym))
        if (nm %in% load_names)
          bad <<- c(bad, sprintf("'%s' is written into '%s' but is a top-level binding of a file loaded into it; the validator's closure IS '%s', so overwriting a name that came from the loaded file rewrites the code the gate itself runs (e.g. .mm_file_sha256, which the digest check calls)",
                                 nm, env_sym, env_sym))
        # Fix round 4 (review B6). The chain does not stop at the loaded files.
        # '%s' is parented on baseenv(), and every function sys.source() puts
        # into it closes over it, so an unqualified call inside the validator
        # resolves frame -> e -> baseenv(). require_validated_method()
        # expresses EVERY refusal as a call to stop(); shadowing stop() turns
        # all of them into no-ops, and `stop` is a binding of no loaded file.
        if (exists(nm, envir = baseenv(), inherits = FALSE))
          bad <<- c(bad, sprintf("'%s' is written into '%s' but is a binding of baseenv(), which is '%s''s parent; the validator's unqualified calls resolve through it, so this write would replace a base function the gate itself runs (stop(), which every refusal in the validator calls, is the sharpest case)",
                                 nm, env_sym, env_sym))
        if (.mm_expr_is_functionish(x[[3]]))
          bad <<- c(bad, sprintf("a function value is written into '%s'; only non-function values (the dependency-digest shape) may be added before the seal",
                                 env_sym))
        tryCatch(walk(x[[3]], i), error = function(e) NULL)
        return(invisible(NULL))
      }
    }
    # 3. the seal.
    if (identical(h, "lockEnvironment")) {
      a <- .mm_match_args(x, c("env", "bindings"))
      if (!is.null(a$env) && is_sym(a$env)) { skip(a$env); return(invisible(NULL)) }
    }
    # 2. an enumerated load.
    if (!is.na(h) && h %in% c("sys.source", "source")) {
      a <- if (identical(h, "sys.source"))
             .mm_match_args(x, c("file", "envir", "chdir", "keep.source"))
           else .mm_match_args(x, c("file", "local", "echo"))
      target <- if (identical(h, "sys.source")) a$envir else a$local
      if (!is.null(target) && is_sym(target)) { skip(target); return(invisible(NULL)) }
    }
    for (k in seq_along(x))
      if (!is.null(x[[k]])) tryCatch(walk(x[[k]], i), error = function(e) NULL)
    invisible(NULL)
  }
  # 5. the block's value is the bare symbol, already required by clause (1).
  for (i in seq_len(max(length(st) - 1L, 0L)))
    tryCatch(walk(st[[i]], i), error = function(e) NULL)
  unique(bad)
}

# Resolve every load into `env_sym` and judge the resolved set.
# Returns list(ok, loads, why). Accepted when: every load resolves statically,
# at least one resolves to tools/method_manifest.R, and every OTHER resolved
# path came from a top-level literal vector of harness-relative paths (the
# eda.R EDA_DEPENDENCY_FILES shape) rather than from a literal written inside
# the block.

# The statement index of `lockEnvironment(env_sym, bindings = TRUE)`, or NA.
# Recomputed here rather than threaded through, so that all three callers of
# the resolver judge the same block the same way.
.mm_lock_index <- function(st, env_sym) {
  for (i in seq_along(st)) {
    x <- st[[i]]
    if (!.mm_is_base_qualified_call_to(x, "lockEnvironment")) next
    a <- .mm_match_args(x, c("env", "bindings"))
    if (!is.null(a$env) && is.name(a$env) &&
        identical(as.character(a$env), env_sym)) return(i)
  }
  NA_integer_
}

# `role` (integrity batch 12, R11-P0-2; SME decision 2). A file may build TWO
# sealed objects: the VALIDATOR environment, which loads tools/method_manifest.R
# and nothing else, and a DEPENDENCY environment, which loads the file's
# declared dependencies and never the validator file. They used to share one
# environment, so a dependency sourced after tools/method_manifest.R could
# overwrite `require_validated_method` or `.mm_file_sha256` before the seal --
# and the gate then ran the dependency's code with the manifest digest checked
# by the dependency's helper. The validator environment never receives a
# dependency binding; selected dependency functions are exposed through
# closures over the dependency environment.
.mm_resolve_trusted_loads <- function(st, env_sym, binds, fns = character(0),
                                      role = c("validator", "dependency")) {
  role <- match.arg(role)
  no <- function(why) list(ok = FALSE, loads = character(0), why = why)
  calls <- .mm_loads_into(st, env_sym)
  if (!length(calls))
    return(no(sprintf("nothing is source()d into '%s'", env_sym)))
  paths <- character(0); srcs <- character(0)
  for (cl in calls) {
    if (isTRUE(cl$qualified))
      return(no(sprintf("'%s' loads into '%s', but a load into a trusted object must be written exactly `base::sys.source(...)`: the bare spelling resolves through the environment this file is sourced into, and any other qualification, `source()`, or an object dispatch is not base's sys.source (integrity batch 12, decision 1)",
                        cl$head, env_sym)))
    if (is.null(cl$expr)) return(no("a load into the trusted object has no file argument"))
    r <- .mm_static_path_vector(cl$expr, binds)
    if (is.null(r))
      return(no(sprintf("the path loaded into '%s' by `%s` cannot be resolved statically; every file loaded into the trusted object must be nameable at audit time%s",
                        env_sym, paste(deparse(cl$expr), collapse = " "),
                        .mm_path_hint(cl$expr, binds))))
    paths <- c(paths, r$path); srcs <- c(srcs, r$src)
  }
  # Fix round (review B1). EVERY path must be repository-relative, including
  # the qualifying one. The pattern below matches any path ENDING in
  # tools/method_manifest.R, so "/tmp/attacker/tools/method_manifest.R" and
  # "../../../attacker/tools/method_manifest.R" qualified just as happily as
  # the real thing, and the harness-relative test was skipped for exactly the
  # path it most needed to cover. That shape is worse than the residual the
  # batch-9 review accepted: it needs no accomplice file inside the repository,
  # so the write guard, the manifest digests and the SME's diff review -- the
  # designated backstop past the audit's reach -- never see it.
  for (i in seq_along(paths)) {
    if (!.mm_is_harness_relative(paths[i]))
      return(no(sprintf("'%s' is loaded into '%s' but is not a repository-relative path; a trusted object loads only files inside this repository, where the write guard, the manifest digests and the SME's review of the diff can see them",
                        paths[i], env_sym)))
  }
  qual <- grepl(MM_TRUST_QUALIFYING_LOAD, paths)
  if (identical(role, "validator")) {
    if (!any(qual))
      return(no(sprintf("none of the file(s) loaded into '%s' (%s) is tools/method_manifest.R, so the object does not hold this repository's validator",
                        env_sym, paste(paths, collapse = ", "))))
    if (!all(qual))
      return(no(sprintf("the validator environment '%s' loads only tools/method_manifest.R; '%s' is a dependency and belongs in a separate, second locked environment whose functions are reached through closures, never in the environment the validator runs in (integrity batch 12, R11-P0-2)",
                        env_sym, paths[!qual][1])))
  } else {
    if (any(qual))
      return(no(sprintf("the dependency environment '%s' loads tools/method_manifest.R; the validator is loaded only into its own environment, which loads nothing else (integrity batch 12, R11-P0-2)",
                        env_sym)))
  }
  # Fix round 5 (review L1). A load is sourced INTO the object, so the loaded
  # file's top level IS the object: a dependency that defines the validator
  # shadows the real one. One condition over the load bindings already
  # computed, and no legitimate dependency can want that name. The QUALIFYING
  # load is exempt for the obvious reason -- tools/method_manifest.R defines
  # require_validated_method, which is the entire point of loading it.
  for (i in seq_along(paths)) {
    if (qual[i]) next
    if (MM_VALIDATOR_NAME %in% .mm_load_binding_names(paths[i]))
      return(no(sprintf("the dependency '%s' loaded into '%s' defines %s(); a load is sourced INTO the object, so this would shadow the real validator with one from a file that is not tools/method_manifest.R",
                        paths[i], env_sym, MM_VALIDATOR_NAME)))
  }
  # Integrity batch 13 (decision 1A). The block's frame is now parented on
  # base, so a root constant of the file is NOT visible to the block at run
  # time (R resolves frame -> base -> global -> search path, never the
  # environment the file is sourced into). A dependency path is therefore
  # written INSIDE the block as `base::c(...)` of string literals or a single
  # string literal; the batch-10 requirement that it come from a top-level
  # constant is withdrawn for that reason and no other -- a root constant is
  # still resolved statically (root-only, bound once, preceding the block) so
  # the audit's verdict on such a file is the runtime's ("object not found"
  # at source time), never a silent pass. `srcs` is kept for the report.
  # The use check runs LAST, because it needs the resolved load list: a pre-seal
  # write may not land on any name the loaded files define (fix round 3, B5).
  uses <- .mm_trusted_env_uses(st, env_sym, .mm_lock_index(st, env_sym), fns,
                               .mm_load_binding_names(unique(paths)))
  if (length(uses)) return(no(uses[1]))

  # "Could not check" is not a pass -- but only where a check was actually
  # needed. The B5 rule compares a pre-seal write against the bindings of the
  # loaded files, so an unreadable load disables it. A block with NO pre-seal
  # write has nothing to shadow and does not need those bindings, so it is not
  # penalised for a file the caller's working directory cannot see. Scoping the
  # refusal this way keeps the audit's verdict from depending on filesystem
  # state except where the verdict genuinely turns on it.
  if (.mm_has_preseal_write(st, env_sym)) {
    missing <- unique(paths)[!vapply(unique(paths), file.exists, logical(1))]
    if (length(missing))
      return(no(sprintf("'%s' writes a binding into '%s' before the seal, but the loaded file '%s' cannot be read at audit time, so the audit cannot tell whether that write shadows a name the loaded file defines. Run the audit from the repository root.",
                        env_sym, env_sym, missing[1])))
  }

  list(ok = TRUE, loads = unique(paths), why = "")
}

# The new.env() construction clause, as a refusal sentence or NULL when the
# block's construction is the one permitted shape. Shared by the predicate and
# by the reason reporter so the two can never disagree about why a block was
# refused.
.mm_trusted_construction_index <- function(st, env_sym) {
  is_plain_assign_to <- function(x, sym) {
    is.call(x) && as.character(x[[1]])[1] %in% c("<-", "=", "<<-") &&
      length(x) == 3L && is.name(x[[2]]) && identical(as.character(x[[2]]), sym)
  }
  for (i in seq_along(st))
    if (is_plain_assign_to(st[[i]], env_sym) &&
        .mm_is_base_qualified_call_to(st[[i]][[3]], "new.env")) return(i)
  NA_integer_
}

# The same search with the head requirement dropped, used only to write a
# refusal that names what the file actually wrote. A construction whose head is
# `shim$new.env`, `pkg::new.env` or a computed value is NOT the construction;
# this locates it so the refusal can say which statement was rejected.
.mm_trusted_construction_lookalike <- function(st, env_sym) {
  for (i in seq_along(st)) {
    x <- st[[i]]
    if (!is.call(x) || !(as.character(x[[1]])[1] %in% c("<-", "=", "<<-")) ||
        length(x) != 3L || !is.name(x[[2]]) ||
        !identical(as.character(x[[2]]), env_sym)) next
    rhs <- x[[3]]
    if (is.call(rhs) && identical(.mm_unqualify(.mm_call_head_name(rhs)), "new.env"))
      return(i)
  }
  NA_integer_
}

.mm_trusted_construction_violation <- function(st, env_sym) {
  new_i <- .mm_trusted_construction_index(st, env_sym)
  if (is.na(new_i)) {
    look <- .mm_trusted_construction_lookalike(st, env_sym)
    if (!is.na(look)) {
      lt <- tryCatch(paste(deparse(st[[look]]), collapse = " "),
                     error = function(e) "<unparseable>")
      if (nchar(lt) > 160L) lt <- paste0(substr(lt, 1L, 157L), "...")
      return(sprintf("the construction's head must be exactly `base::new.env`, so that it is base's new.env() reached through the sealed base namespace and not a function of that name reached through the sourcing environment, an object or another namespace: `%s`",
                     lt))
    }
    return(sprintf("the block has no top-level `%s <- base::new.env(parent = base::baseenv())` construction", env_sym))
  }
  txt <- tryCatch(paste(deparse(st[[new_i]]), collapse = " "),
                  error = function(e) "<unparseable>")
  if (nchar(txt) > 160L) txt <- paste0(substr(txt, 1L, 157L), "...")
  new_args <- as.list(st[[new_i]][[3]])[-1]
  anm <- names(new_args)
  if (length(new_args) != 1L || is.null(anm) || !identical(anm[1], "parent"))
    return(sprintf("the construction must be written `%s <- base::new.env(parent = base::baseenv())` -- exactly one argument, written with the name `parent`. R's formals are new.env(hash, parent, size), so a positional argument does not mean what it reads as: `%s`",
                   env_sym, txt))
  parent_arg <- new_args[[1]]
  if (!.mm_is_base_qualified_call_to(parent_arg, "baseenv") ||
      length(as.list(parent_arg)) != 1L)
    return(sprintf("the construction's `parent` must be the literal call `base::baseenv()` written with exactly that head and no arguments, so the sealed object's chain is exactly (its loads, base): `%s`",
                   txt))
  NULL
}

.mm_is_trusted_private_rhs <- function(rhs, binds = list(), fns = character(0),
                                       shadow = NULL) {
  .mm_is_trusted_rhs(rhs, binds, fns, shadow, role = "validator")
}

# The same structural predicate for the DEPENDENCY environment (integrity
# batch 12, R11-P0-2): every rule below applies unchanged; only the load set
# differs -- no tools/method_manifest.R, no file that defines the validator,
# every path from a root literal constant of this file.
.mm_is_trusted_dependency_rhs <- function(rhs, binds = list(), fns = character(0),
                                          shadow = NULL) {
  .mm_is_trusted_rhs(rhs, binds, fns, shadow, role = "dependency")
}

.mm_is_trusted_rhs <- function(rhs, binds = list(), fns = character(0),
                               shadow = NULL, role = c("validator", "dependency")) {
  role <- match.arg(role)
  st <- .mm_trusted_block_statements(rhs)
  if (is.null(st) || length(st) < 3L) return(FALSE)
  # The wrapper itself is a construction primitive: `local` resolves through
  # the sourcing environment exactly as `sys.source` does, so the block must be
  # `base::local({ ... }, envir = base::new.env())` (integrity batch 12,
  # decision 1, and the wrapper-frame finding at .mm_trusted_block_statements).
  if (isTRUE(attr(st, "bare_wrapper"))) return(FALSE)
  if (!is.null(attr(st, "wrapper_violation"))) return(FALSE)

  # (1) the block's value is a bare symbol
  last <- st[[length(st)]]
  if (!is.name(last)) return(FALSE)
  env_sym <- as.character(last)

  # (2) `env_sym <- new.env(...)` as a TOP-LEVEL statement
  # Fix round 4 (review B7). The PARENT is the third contributor to the sealed
  # object's lookup chain and was unconstrained. `new.env()` inside local({})
  # takes parent.frame(), so the chain runs block -> reference file -> global,
  # and the reference file's own top-level bindings then shadow base for
  # everything loaded into the object -- with no pre-seal write anywhere, so
  # the position-4 whitelist never looks at it. Requiring parent = baseenv()
  # makes the chain exactly (loads, base), both of which ARE judged. eda.R:114
  # already writes precisely this, so the constraint pins the one shipped shape
  # and costs nothing.
  #
  # Fix round 2 of batch 11 (review F1-b). The clause used to match the
  # construction's arguments against c("parent", "hash", "size"); R's formals
  # are new.env(hash, parent, size), so a POSITIONAL new.env(baseenv()) was
  # read by the audit as parent = baseenv() while R set hash = baseenv() and
  # left parent = parent.frame(). The audit reported gated = TRUE for a sealed
  # object whose chain ran block -> reference file -> global, which is exactly
  # the invariant this clause exists to hold. Rather than repair the formal
  # order and keep guessing at positional matching, the construction must now
  # be written in the one shape whose meaning is unambiguous: exactly one
  # argument, named `parent`, equal to the literal call baseenv(). Every
  # shipped reference file already writes it that way.
  new_i <- .mm_trusted_construction_index(st, env_sym)
  if (!is.null(.mm_trusted_construction_violation(st, env_sym))) return(FALSE)

  # The construction ITSELF is evaluated in the local({}) block, whose parent is
  # the reference file's top level -- so the file can shadow the primitives the
  # construction calls. A top-level `sys.source <- function(...) NULL` makes the
  # load a no-op; a top-level `lockEnvironment <- function(...) NULL` leaves the
  # object unsealed. Neither is a pre-seal write, so nothing else here looks at
  # it.
  #
  # Scoped DELIBERATELY to the primitives the construction calls, and NOT to
  # "any name a loaded file binds": eda.R and tools/method_manifest.R BOTH
  # define `%||%` at top level, so the broader rule would refuse the only
  # shipped use of this construction in the repository. eda.R binds none of the
  # names below.
  if (length(intersect(fns, MM_TRUST_CONSTRUCTION_PRIMITIVES))) return(FALSE)

  # ... and the same rule applied to the FULL top-level census, which since fix
  # round 2 of batch 11 walks the whole expression tree of every top-level
  # statement rather than matching it at the root, and treats a name the audit
  # cannot resolve to a literal as shadowing everything. `fns` above is the
  # binding MAP's names, which by construction only sees root-level plain
  # assignments; the census sees `if (TRUE) delayedAssign("sys.source", ...)`
  # and `delayedAssign(paste0("sys.", "source"), ...)` as well. Both are kept:
  # the map's names are what the load resolver reasons about, the census is
  # what the shadow rule reasons about.
  if (length(.mm_top_level_shadow_violations(.mm_shadow_census_of(shadow))))
    return(FALSE)

  # ... and no ENCLOSING layer of the construction may bind anything at all
  # (fix round 5, B8). eda.R -- the only shipped use of this construction --
  # has no enclosing layer, so the blanket form costs nothing and needs no list
  # of which names would have been dangerous.
  if (length(.mm_enclosing_bindings(st))) return(FALSE)

  # ... nor may any statement INSIDE the block bind a name other than the
  # symbol it returns (fix batch 11, R10-P0-2). The enclosing-layer rule above
  # and the top-level primitive rule below both left the block's OWN frame --
  # the innermost link on the construction's chain -- unexamined, so Codex's
  # counterexample bound fake primitives one line above the construction.
  if (length(.mm_trusted_block_bindings(st, env_sym))) return(FALSE)

  # ... and, since fix round 1 of this batch, every statement of the block must
  # be one of the permitted construction FORMS. The binding census above is a
  # blacklist and had failed three review rounds running; this is the positive
  # enumeration that closes the class. Both apply.
  if (length(.mm_trusted_block_statement_violations(st, env_sym, binds)))
    return(FALSE)

  # A reference file that builds a trusted object may not attach a package: an
  # attached package is on the CONSTRUCTION's chain, so one exporting
  # sys.source would redirect the load exactly as an enclosing binding does,
  # and its payload is outside anything the audit can resolve (K1). The flag is
  # computed once per file in audit_entrypoint_gating and carried on `binds`.
  if (isTRUE(attr(binds, "attaches"))) return(FALSE)

  # ... and the file's top level may not use a reflective or evaluating form,
  # nor call a value whose head the audit cannot read as a name (fix round 4 of
  # batch 11, SME decision 2026-09-04). Those statements run when the file is
  # sourced, in the very frame the construction's block is nested in, and their
  # effect is outside every census here. Computed once per file in
  # audit_entrypoint_gating() and carried on `binds`, the same way `attaches`
  # is, so the predicate does not have to be handed the path.
  if (length(attr(binds, "reflective"))) return(FALSE)

  # ... and no closure of the file may be built with the bare `local`, and
  # every path constant a construction reads must precede it, bound once
  # (integrity batch 13, R12-P0-2 and R12-P1-2; computed by .mm_audit_binds()).
  if (length(attr(binds, "bare_local"))) return(FALSE)
  if (length(attr(binds, "order"))) return(FALSE)

  # ... and no OTHER load-bearing block of the file may be of an unqualified
  # shape (integrity batch 12, R11-P0-2). A block that loads into the
  # environment it returns is either the validator environment or a dependency
  # environment; one that is neither -- a bare-wrapped construction, a
  # dependency environment loading the validator file, a validator environment
  # loading a dependency -- used to drop silently out of the trusted_loads
  # report while the gate stayed credited. Computed once per file by
  # .mm_audit_binds() and carried on `binds` like `reflective`.
  if (length(attr(binds, "blocks"))) return(FALSE)

  # (3) `base::lockEnvironment(env_sym, bindings = TRUE)` as a TOP-LEVEL statement
  lock_i <- NA_integer_
  for (i in seq_along(st)) {
    x <- st[[i]]
    if (!.mm_is_base_qualified_call_to(x, "lockEnvironment")) next
    a <- .mm_match_args(x, c("env", "bindings"))
    if (!("env" %in% names(a)) || !is.name(a$env) ||
        !identical(as.character(a$env), env_sym)) next
    if (!("bindings" %in% names(a)) || !isTRUE(a$bindings)) next
    lock_i <- i; break
  }
  if (is.na(lock_i) || lock_i <= new_i) return(FALSE)

  # (5) nothing at the top level rebinds the environment after the lock
  if (lock_i < length(st))
    for (i in seq.int(lock_i + 1L, length(st))) {
      x <- st[[i]]
      if (is.call(x) && as.character(x[[1]])[1] %in% c("<-", "=", "<<-") &&
          length(x) == 3L && env_sym %in% .mm_assign_names(x[[2]])) return(FALSE)
      if (is.call(x) && identical(as.character(x[[1]])[1], "assign")) return(FALSE)
    }

  # (4) every load into env_sym resolves and matches the ROLE: the validator
  # environment loads exactly tools/method_manifest.R; a dependency environment
  # loads only dependencies named by a root literal constant of this file, none
  # of which defines the validator. The path may be written whole
  # ("tools/method_manifest.R") or assembled with file.path().
  isTRUE(.mm_resolve_trusted_loads(st, env_sym, binds, fns, role = role)$ok)
}

# The env_sym / statements pair of a block, or NULL when the block is not of
# the trusted shape at all. Shared by the predicate and the load report so the
# two never disagree about which symbol the block returns.
.mm_trusted_block_env <- function(rhs) {
  st <- .mm_trusted_block_statements(rhs)
  if (is.null(st) || length(st) < 3L) return(NULL)
  last <- st[[length(st)]]
  if (!is.name(last)) return(NULL)
  list(st = st, env_sym = as.character(last))
}

# The files this block loads into the object it returns, as the audit reports
# them. Returns character(0) when the block is not of the trusted shape or its
# load set does not qualify — a block that is not trusted has no trusted loads
# to report, and its refusal reason is carried by .mm_trusted_dispatch_base(),
# not by this reporter. The only caller filters with
# .mm_is_trusted_private_rhs() first, so in practice the empty branch is
# reached only if that predicate and this resolver ever disagree.
.mm_trusted_loads_of_rhs <- function(rhs, binds = list(), fns = character(0),
                                     role = c("validator", "dependency")) {
  role <- match.arg(role)
  b <- .mm_trusted_block_env(rhs)
  if (is.null(b)) return(character(0))
  r <- .mm_resolve_trusted_loads(b$st, b$env_sym, binds, fns, role = role)
  if (isTRUE(r$ok)) r$loads else character(0)
}

# Every trusted-object load in a reference file, for the audit's report column
# and for `depends:` derivation. Scans the file's top-level bindings for the
# constructions the trust predicate accepts -- the validator environment and,
# since integrity batch 12, a dependency environment -- and collects what each
# of them loads. The validator's own file is reported first.
.mm_trusted_loads_of_file <- function(binds, fns = character(0), shadow = NULL) {
  acc <- character(0)
  for (nm in names(binds)) {
    rhss <- binds[[nm]]
    if (is.null(rhss) || length(rhss) != 1L) next
    if (.mm_is_trusted_private_rhs(rhss[[1]], binds, fns, shadow))
      acc <- c(acc, .mm_trusted_loads_of_rhs(rhss[[1]], binds, fns, role = "validator"))
    else if (.mm_is_trusted_dependency_rhs(rhss[[1]], binds, fns, shadow))
      acc <- c(acc, .mm_trusted_loads_of_rhs(rhss[[1]], binds, fns, role = "dependency"))
  }
  acc <- unique(acc)
  c(acc[grepl(MM_TRUST_QUALIFYING_LOAD, acc)], acc[!grepl(MM_TRUST_QUALIFYING_LOAD, acc)])
}

# Why a block of the trusted SHAPE is not a trusted object, as one sentence, or
# NULL when it is. Shared by .mm_trusted_dispatch_base() (the gate's refusal)
# and .mm_trusted_block_role_violations() (the per-file refusal) so the two
# never disagree about the reason.
.mm_trusted_block_why <- function(nm, rhs, binds, fns, census,
                                  role = c("validator", "dependency")) {
  role <- match.arg(role)
  b <- .mm_trusted_block_env(rhs)
  if (is.null(b)) return(NULL)
  if (isTRUE(attr(b$st, "bare_wrapper")))
    return(sprintf("'%s' is not a trusted object: its block is wrapped in the bare `local(...)`, but the wrapper is a construction primitive and must be written `base::local({ ... }, envir = base::new.env())` so that it resolves through the sealed base namespace (integrity batch 12, decision 1)", nm))
  if (!is.null(attr(b$st, "wrapper_violation")))
    return(sprintf("'%s' is not a trusted object: %s", nm, attr(b$st, "wrapper_violation")))
  # Name the offending statement when the block has the trusted SHAPE but
  # holds a statement form the whitelist does not permit (fix round 1).
  sv <- .mm_trusted_block_statement_violations(b$st, b$env_sym, binds)
  if (length(sv))
    return(sprintf("'%s' is not a trusted object: %s", nm, sv[1]))
  # The top-level shadow census and the new.env() clause each name their
  # own offender rather than falling through to the generic shape sentence
  # (fix round 2 of batch 11): both refusals are about a statement the
  # author wrote, and an author cannot act on "is built in this file".
  tv <- .mm_top_level_shadow_violations(census)
  if (length(tv))
    return(sprintf("'%s' is not a trusted object: %s", nm, tv[1]))
  # A statement violation in ANY load-bearing block of the file is reported
  # before the file-level verdicts (integrity batch 13): the reflective walker
  # only exempts a block of the trusted shape when its statements pass, so a
  # dependency block with a bare `c` or `vapply` is ALSO a reflective hit, and
  # the statement-level sentence -- which names the qualification to write --
  # is the one an author can act on.
  osv <- .mm_other_block_statement_violations(binds, nm)
  if (length(osv))
    return(sprintf("'%s' is not a trusted object: %s", nm, osv[1]))
  rv <- attr(binds, "reflective")
  if (length(rv))
    return(sprintf("'%s' is not a trusted object: %s", nm, rv[1]))
  bl <- attr(binds, "bare_local")
  if (length(bl))
    return(sprintf("'%s' is not a trusted object: %s", nm, bl[1]))
  ov <- attr(binds, "order")
  if (length(ov))
    return(sprintf("'%s' is not a trusted object: %s", nm, ov[1]))
  cv <- .mm_trusted_construction_violation(b$st, b$env_sym)
  if (!is.null(cv))
    return(sprintf("'%s' is not a trusted object: %s", nm, cv))
  lr <- .mm_resolve_trusted_loads(b$st, b$env_sym, binds, fns, role = role)
  if (!isTRUE(lr$ok))
    return(sprintf("'%s' is not a trusted object: %s", nm, lr$why))
  bv <- attr(binds, "blocks")
  if (length(bv))
    return(sprintf("'%s' is not a trusted object: %s", nm, bv[1]))
  NULL
}

# The first statement violation of any OTHER load-bearing block of the file,
# prefixed with that block's name (integrity batch 13; see .mm_trusted_block_why).
.mm_other_block_statement_violations <- function(binds, except = "") {
  for (nm in names(binds)) {
    if (identical(nm, except)) next
    for (rhs in binds[[nm]]) {
      b <- tryCatch(.mm_trusted_block_env(rhs), error = function(e) NULL)
      if (is.null(b)) next
      if (!length(tryCatch(.mm_loads_into(b$st, b$env_sym), error = function(e) list()))) next
      sv <- tryCatch(.mm_trusted_block_statement_violations(b$st, b$env_sym, binds),
                     error = function(e) character(0))
      if (length(sv)) return(sprintf("the block '%s' of this file is refused: %s", nm, sv[1]))
    }
  }
  character(0)
}

# Every LOAD-BEARING block of the file -- a block of the trusted shape that
# source()s something into the symbol it returns -- that qualifies as neither
# the validator environment nor a dependency environment, each as a refusal
# sentence (integrity batch 12, R11-P0-2). Deny-by-default over the whole
# file: such a block is never left unjudged, so it cannot drop out of the
# trusted_loads report while the gate stays credited. A load-bearing block
# bound more than once is refused for that reason alone.
.mm_trusted_block_role_violations <- function(binds, fns = character(0), shadow = NULL) {
  out <- character(0)
  census <- .mm_shadow_census_of(shadow)
  for (nm in names(binds)) {
    rhss <- binds[[nm]]
    for (rhs in rhss) {
      b <- tryCatch(.mm_trusted_block_env(rhs), error = function(e) NULL)
      if (is.null(b)) next
      if (!length(tryCatch(.mm_loads_into(b$st, b$env_sym), error = function(e) list()))) next
      if (length(rhss) != 1L) {
        out <- c(out, sprintf("'%s' loads into the environment it returns but is bound %d times in this file; a trusted object is bound once", nm, length(rhss)))
        next
      }
      if (.mm_is_trusted_private_rhs(rhs, binds, fns, shadow)) next
      if (.mm_is_trusted_dependency_rhs(rhs, binds, fns, shadow)) next
      # Report the validator-role reason unless the block is plainly a
      # dependency environment (it loads no tools/method_manifest.R).
      loads <- tryCatch(.mm_resolve_trusted_loads(b$st, b$env_sym, binds, fns, role = "validator"),
                        error = function(e) list(ok = FALSE))
      role <- if (!isTRUE(loads$ok) && grepl("none of the file\\(s\\) loaded", loads$why %||% ""))
                "dependency" else "validator"
      why <- .mm_trusted_block_why(nm, rhs, binds, fns, census, role = role)
      out <- c(out, if (is.null(why))
        sprintf("'%s' loads into the environment it returns but qualifies as neither the validator environment (loading exactly tools/method_manifest.R) nor a dependency environment", nm)
        else why)
    }
  }
  unique(out)
}

# The base expression of a `$` / `@` / `[[` dispatch, or NULL.
.mm_dispatch_base <- function(cl) {
  if (!is.call(cl)) return(NULL)
  h <- cl[[1]]
  if (is.call(h) && length(h) == 3L &&
      as.character(h[[1]])[1] %in% c("$", "@", "[[")) return(h[[2]])
  NULL
}

# Integrity batch 13 fix round 1 (R13-P0-A). A call written
# `base::get("<name>", envir = <sym>)(...)` -- the result of a base::get is
# invoked. This is the ONLY spelling a gate-chain function may use to reach the
# validator, and a dependency accessor to reach a sealed dependency, because
# base::get resolves through the base namespace (which R seals at startup) and
# takes its environment from the bare symbol handed to `envir =`, not from any
# operator the sourcing environment can shadow. Returns list(name, base) -- the
# looked-up name as a string and the environment symbol -- or NULL when `cl` is
# not exactly that shape (the inner call is base::get with a one-element
# character first argument and a bare-symbol `envir`).
.mm_base_get_dispatch <- function(cl) {
  if (!is.call(cl)) return(NULL)
  g <- cl[[1]]
  if (!.mm_is_base_qualified_call_to(g, "get")) return(NULL)
  a <- .mm_match_args(g, c("x", "pos", "envir", "mode", "inherits"))
  nm <- a[["x"]]; ev <- a[["envir"]]
  if (is.null(nm) || !(is.character(nm) && length(nm) == 1L)) return(NULL)
  if (is.null(ev) || !is.name(ev)) return(NULL)
  list(name = as.character(nm), base = ev)
}

MM_TRUST_TRACE_MAX <- 4L

# Does `base` trace to a top-level binding of the shipped construction, with
# every symbol on the trace bound exactly once in the whole file?
.mm_trusted_dispatch_base <- function(base, binds, fns) {
  no <- function(why) list(ok = FALSE, why = why)
  census <- .mm_shadow_census_of(fns)
  if (!is.name(base))
    return(no("the dispatch base is an expression, not a symbol, so what it holds cannot be established from the file"))
  nm <- as.character(base)
  for (step in seq_len(MM_TRUST_TRACE_MAX)) {
    rhss <- binds[[nm]]
    if (is.null(rhss) || !length(rhss))
      return(no(sprintf("'%s' is not bound in this reference file", nm)))
    if (length(rhss) != 1L)
      return(no(sprintf("'%s' is bound %d time(s); a trusted dispatch base is bound once and never mutated", nm, length(rhss))))
    rhs <- rhss[[1]]
    if (.mm_is_trusted_private_rhs(rhs, binds, names(fns), census))
      return(list(ok = TRUE, why = ""))
    # One alias hop: `private <- .eda_private`.
    if (is.name(rhs)) { nm <- as.character(rhs); next }
    # When the block has the trusted SHAPE but its loads do not qualify, say
    # which load failed rather than repeating the generic shape description —
    # the load set is the part of clause (4) an author is most likely to get
    # wrong, and the most likely place for a decoy (integrity batch 10). The
    # reason is written by .mm_trusted_block_why(), shared with the per-file
    # role census (integrity batch 12).
    why <- .mm_trusted_block_why(nm, rhs, binds, names(fns), census, role = "validator")
    if (!is.null(why)) return(no(why))
    return(no(sprintf("'%s' is built in this file rather than loaded: a trusted dispatch base is a base::new.env() filled by base::sys.source()ing tools/method_manifest.R and sealed with base::lockEnvironment(bindings = TRUE)", nm)))
  }
  no(sprintf("the dispatch base could not be traced to a top-level binding within %d steps", MM_TRUST_TRACE_MAX))
}

MM_MAX_GATE_CHAIN <- 8L

# Walks the chain from `cl` and reports whether it terminates in this unit's
# own `require_validated_method`. `fns` are the reference file's top-level
# bindings. Within a wrapper the candidate statements are tried in execution
# order and the first one whose subchain reaches the validator wins; a
# candidate that does not is stepped over under the `return` guard above.
.mm_gate_chain <- function(cl, unit, fns, bindings = list(),
                           visited = character(0), chain = character(0),
                           bound = character(0), eps = character(0),
                           binds = list()) {
  no <- function(reason, ch = chain) list(ok = FALSE, chain = ch,
    reason = sprintf("gate chain %s: %s", paste(ch, collapse = " -> "), reason))
  if (length(chain) >= MM_MAX_GATE_CHAIN)
    return(no(sprintf("no gate within %d calls", MM_MAX_GATE_CHAIN)))
  if (is.null(cl) || !is.call(cl)) return(no("statement is not a call"))

  # Integrity batch 13 fix round 1 (R13-P0-A / R13-P0-B). The batch-13 terminus
  # is `base::get("require_validated_method", envir = <sealed>)(...)`, whose head
  # is itself a call, so it is recognised here before the `$`-dispatch reading of
  # .mm_call_head_name(). The sealed environment is trusted by the same trace as
  # a `$` dispatch base was; skill_dir and calling_file must be STRING LITERALS
  # (a symbol -- even one that resolves to a literal today -- can be rebound in
  # the sourcing frame after the file is sourced and redirect the gate to a
  # forged manifest); unit is a literal or a one-hop forward of the gate
  # closure's own formal.
  gd <- .mm_base_get_dispatch(cl)
  if (!is.null(gd)) {
    h <- gd$name
    chain <- c(chain, h)
    if (identical(h, "require_validated_method") || h %in% names(fns) || h %in% eps) {
      tr <- .mm_trusted_dispatch_base(gd$base, binds, fns)
      if (!isTRUE(tr$ok))
        return(no(sprintf("the gate is reached by base::get() off '%s', which is not a trusted object: %s",
                          paste(deparse(gd$base), collapse = " "), tr$why), chain))
    }
    if (identical(h, "require_validated_method")) {
      formals_ok <- c("skill_dir", "calling_file", "unit", "legacy")
      actuals <- as.list(cl)[-1]
      anm <- names(actuals); if (is.null(anm)) anm <- rep("", length(actuals))
      if (any(nzchar(anm) & !anm %in% formals_ok))
        return(no("require_validated_method has an argument that is not an exact formal name", chain))
      args <- .mm_match_args(cl, formals_ok)
      for (a in c("skill_dir", "calling_file")) {
        if (!a %in% names(args))
          return(no(sprintf("require_validated_method is called without a %s argument", a), chain))
        if (!(is.character(args[[a]]) && length(args[[a]]) == 1L))
          return(no(sprintf("the %s argument is not a string literal; skill_dir and calling_file must be written out as string literals so a top-level constant rebound in the sourcing frame after this file is sourced cannot redirect the gate (integrity batch 13, R13-P0-B)", a), chain))
      }
      # THIS skill and THIS file, not merely "a string literal" (integrity
      # batch 14, Codex round 13 R13-P1-3). Before this, the audit checked the
      # skill_dir and calling_file arguments for TYPE and compared only the
      # unit name, so a DRAFT reference could name another skill's APPROVED
      # reference of the same unit name in its gate literals: the audit
      # credited the chain, and at run time the validator read the other
      # skill's manifest and hashed the other skill's file, letting the draft
      # implementation compute under someone else's approval. The expected
      # values travel on `binds` (as the trust predicate's file path does)
      # rather than through four call layers.
      exp_skill <- base::attr(binds, "expect_skill_dir")
      exp_ref   <- base::attr(binds, "expect_reference_path")
      if (!is.null(exp_skill) && !identical(.mm_relpath(args$skill_dir), .mm_relpath(exp_skill)))
        return(no(sprintf("the gate authorizes skill_dir '%s', but this file is audited as part of '%s'; a gate must name the skill whose manifest and approval it computes under (integrity batch 14, R13-P1-3)",
                          args$skill_dir, exp_skill), chain))
      if (!is.null(exp_ref) && !identical(.mm_relpath(args$calling_file), .mm_relpath(exp_ref)))
        return(no(sprintf("the gate names calling_file '%s', but the manifest binds this unit to '%s'; a gate must name the file under audit, not another file of the same unit name (integrity batch 14, R13-P1-3)",
                          args$calling_file, exp_ref), chain))
      if (!"unit" %in% names(args))
        return(no("require_validated_method is called without a unit argument", chain))
      lit <- .mm_literal_value(args$unit, bindings)
      if (is.na(lit))
        return(no("the unit argument is not a character literal", chain))
      if (!identical(lit, unit))
        return(no(sprintf("the gate authorizes unit '%s', not '%s'", lit, unit), chain))
      return(list(ok = TRUE, chain = chain, reason = ""))
    }
    return(no(sprintf("'%s' is reached through base::get(); base::get dispatch is the batch-13 spelling for the validator terminus and dependency accessors, not for reaching a gate-chain member (name a chain member by its bare top-level symbol)", h), chain))
  }

  # A base::get(...)() terminus whose dispatch did NOT resolve (.mm_base_get_dispatch
  # returned NULL): name exactly what disqualified it -- a non-literal accessed
  # name, or an `envir` that is not the bare captured symbol -- rather than
  # falling through to the generic ":: is not a gate" message below, which names
  # base::get and hides the real defect. Refusal unchanged; text only (integrity
  # batch 13 fix round 2).
  if (is.call(cl) && is.call(cl[[1]]) && .mm_is_base_qualified_call_to(cl[[1]], "get")) {
    a <- .mm_match_args(cl[[1]], c("x", "pos", "envir", "mode", "inherits"))
    gnm <- a[["x"]]; gev <- a[["envir"]]
    chain <- c(chain, "base::get")
    if (is.null(gnm) || !(is.character(gnm) && length(gnm) == 1L))
      return(no("the base::get() terminus names its target with an expression, not a string literal; the accessed name must be a literal string so a symbol rebound in the sourcing frame cannot redirect the gate (integrity batch 13)", chain))
    if (is.null(gev) || !is.name(gev))
      return(no(sprintf("the base::get() terminus for '%s' has an `envir` that is an expression, not the captured private symbol; the sealed environment must be named by the bare symbol it was bound to at construction (integrity batch 13)",
                        as.character(gnm)), chain))
  }

  h <- .mm_call_head_name(cl)
  if (is.na(h)) return(no("the called function cannot be named statically"))
  chain <- c(chain, h)

  # Fix round 3. A `$` dispatch whose element name is the validator, a chain
  # member or an entry point is credited only when the object it dispatches off
  # was LOADED and SEALED, not built in this file. This replaces enumerating
  # the routes by which an element name can reach an object.
  dbase <- .mm_dispatch_base(cl)
  if (!is.null(dbase) &&
      (identical(h, "require_validated_method") || h %in% names(fns) || h %in% eps)) {
    tr <- .mm_trusted_dispatch_base(dbase, binds, fns)
    if (!isTRUE(tr$ok))
      return(no(sprintf("the gate is reached by dispatch off '%s', which is not a trusted object: %s",
                        paste(deparse(dbase), collapse = " "), tr$why), chain))
  }

  # Fix round 2 (R7). A `::` / `:::`-qualified call can never legitimately be
  # the terminus (no package provides the harness validator) nor a chain member
  # (a chain member is defined in this reference file). Naming one is either a
  # mistake or an attempt to borrow the audit's trust for a foreign function.
  if (grepl(":::?", h)) {
    bare <- .mm_unqualify(h)
    if (identical(bare, MM_VALIDATOR_NAME) || bare %in% names(fns) || bare %in% eps)
      return(no(sprintf("'%s' reaches a namespace-qualified function named like the validator or a chain member; the validator is this repository's and a chain member is defined in this file, so neither is ever reached through :: or :::", h), chain))
    return(no(sprintf("'%s' is a namespace-qualified call, which cannot be a gate in this reference file", h), chain))
  }

  if (identical(h, "require_validated_method")) {
    formals_ok <- c("skill_dir", "calling_file", "unit", "legacy")
    actuals <- as.list(cl)[-1]
    anm <- names(actuals); if (is.null(anm)) anm <- rep("", length(actuals))
    if (any(nzchar(anm) & !anm %in% formals_ok))
      return(no("require_validated_method has an argument that is not an exact formal name", chain))
    args <- .mm_match_args(cl, formals_ok)
    if (!"unit" %in% names(args))
      return(no("require_validated_method is called without a unit argument", chain))
    lit <- .mm_literal_value(args$unit, bindings)
    if (is.na(lit))
      return(no("the unit argument is not a character literal", chain))
    if (!identical(lit, unit))
      return(no(sprintf("the gate authorizes unit '%s', not '%s'", lit, unit), chain))
    return(list(ok = TRUE, chain = chain, reason = ""))
  }
  if (h %in% visited) return(no("the chain cycles", chain))
  # A chain may not pass THROUGH a manifested entry point. An entry point is a
  # public surface with its own unit and its own gate; crediting one entry
  # point's gate to another hides which unit authorized what, and would let a
  # bare `ep2 <- function(x) ep1(x)` inherit ep1's approval. (The pre-round-9
  # audit expressed this by excluding entry points from the gate set;
  # tools/tests/test_batch7.R pins it with `ep_via_ep`.)
  if (h %in% eps)
    return(no(sprintf("'%s' is itself a manifested entry point; an entry point is not gated by calling another entry point", h), chain))
  if (!h %in% names(fns))
    return(no(sprintf("'%s' is not a function defined in this reference file", h), chain))
  # Rule (3): a credited chain member must be bound exactly once in the file --
  # by the top-level definition the audit read. Any other binding (in another
  # function's body, or as a formal) can shadow it at the call site.
  nb <- sum(bound == h)
  if (nb != 1L)
    return(no(sprintf("'%s' is rebound %d time(s) in this reference file; a chain member must have exactly one top-level definition and must never be shadowed", h, nb), chain))
  callee <- .mm_resolve_fn(fns[[h]])
  if (is.null(callee)) {
    cw <- .mm_closure_wrapper_violation(fns[[h]], h)
    if (!is.null(cw)) return(no(cw, chain))
    return(no(sprintf("'%s' is not a function literal", h), chain))
  }
  # Every executable name inside a gate-chain function is base::-qualified
  # (integrity batch 13, decision 1A): a bare `stop` in a gate body resolves
  # through the environment this file is sourced into, so a caller's
  # `stop <- function(...) NULL` turned every refusal into a pass.
  uh <- .mm_unqualified_gate_heads(fns[[h]], names(fns))
  if (length(uh))
    return(no(sprintf("'%s' calls `%s` unqualified; every executable name inside a gate-chain function must be written `base::<name>` (or be a function this file defines at its top level, or one of the parser-fixed syntax heads {, (, if, for, !, <-, function), because a bare name -- including a `$`/`[[`/`@` operator -- resolves through the environment this file is sourced into; reach a sealed object as `base::get(\"<name>\", envir = <sealed>)` (integrity batch 13, R13-P0-A)",
                      h, paste(uh, collapse = "`, `")), chain))

  matched <- .mm_match_args(cl, .mm_formal_names(callee))
  inner <- Filter(Negate(is.na),
                  lapply(matched, .mm_literal_value, bindings = bindings))
  # A forwarded formal is a literal only while the wrapper never rebinds it.
  rebound <- c(names(.mm_all_binding_counts(list(callee[[3]]))),
               .mm_bound_names(callee[[3]]))
  inner <- inner[!names(inner) %in% rebound]
  visited <- c(visited, h)
  first_fail <- NULL
  for (st in .mm_wrapper_statements(callee)) {
    c2 <- .mm_gate_call_of(st)
    if (!is.null(c2) && is.call(c2) && !is.na(.mm_call_head_name(c2))) {
      v <- .mm_gate_chain(c2, unit, fns, inner, visited, chain, bound, eps, binds)
      if (isTRUE(v$ok)) return(v)
      # A shadowing refusal is decisive and outranks any earlier, vaguer
      # failure from the same wrapper: it names the actual defect.
      if (grepl("is rebound|binds `", v$reason)) return(v)
      if (is.null(first_fail)) first_fail <- v
      # The step-over below is about to accept this statement as a benign
      # prologue even though the chain recursion did not credit it. A prologue
      # that is a file-defined helper with an UNQUALIFIED executable head is not
      # refused here (a static audit cannot tell a benign prologue from a
      # security-critical one -- integrity batch 13 fix round 2 reverted the
      # attempt to make it decisive, because the accepted `.side()` prologue
      # shape is structurally identical). But the audit must not be SILENT about
      # it: the runtime `base::` qualification of the member, not this audit, is
      # what defends the chain, and a maintainer reading a clean audit could
      # believe otherwise. Report it at WARN level (integrity batch 13 fix round
      # 3, N2). Names defined at this file's top level are exempt (the head
      # resolves to a file function, not the sourcing frame).
      hn2 <- .mm_call_head_name(c2)
      if (!is.na(hn2) && hn2 %in% names(fns)) {
        uh2 <- .mm_unqualified_gate_heads(fns[[hn2]], names(fns))
        if (length(uh2))
          warning(sprintf("method manifest: gate-chain prologue '%s' (stepped over before the '%s' gate reaches its validator) calls `%s` unqualified; the audit does not refuse a stepped-over prologue, so the RUNTIME base:: qualification of '%s' -- not this static audit -- is what defends the chain (integrity batch 13 fix round 3, N2)",
                          hn2, h, paste(uh2, collapse = "`, `"), hn2), call. = FALSE)
      }
    }
    # From here the statement is being STEPPED OVER. Two guards on what may be
    # stepped over: it must not be able to return early (an exit before the
    # gate), and it must not be able to rebind a name (rule 2) -- a stepped-over
    # `assign()` / `eval()` / `source()` can install a no-op under the very name
    # the next statement is about to be credited for.
    if (.mm_has_return(st))
      return(no(sprintf("'%s' can return before it reaches a gate call", h), chain))
    if (.mm_has_rebinding_call(st) && !.mm_is_lazy_loader(st))
      return(no(sprintf("'%s' can rebind a name before it reaches a gate call (a stepped-over statement calls one of: %s)", h, paste(.mm_rebinding_names(st), collapse = ", ")), chain))
  }
  if (!is.null(first_fail)) return(first_fail)
  no(sprintf("'%s' reaches no gate call", h), chain)
}

# An entry point is gated only if its FIRST statement starts a chain that ends
# in this unit's own validator. The lazy-loader step-over applies inside gate
# wrappers, never to the entry point itself.
.mm_entrypoint_gated <- function(fn_expr, fns, unit, bound = character(0),
                                 eps = character(0), binds = list()) {
  no <- function(reason) list(gated = FALSE, reason = reason, chain = character(0))
  # Rule (1): the validator is a name a reference CALLS and must never DEFINE.
  # Any binding of it anywhere in the file -- top level, inside a function body,
  # by assign(), or as a formal -- can shadow the real one at the call site, so
  # the whole file is refused rather than any one entry point.
  if (MM_VALIDATOR_NAME %in% bound)
    return(no(sprintf("this reference file binds `%s` (%d binding(s)); a reference may call the validator but must never define, alias or shadow it",
                      MM_VALIDATOR_NAME, sum(bound == MM_VALIDATOR_NAME))))
  fn <- .mm_resolve_fn(fn_expr)
  if (is.null(fn)) return(no("not a function literal"))
  stmt <- .mm_first_statement(fn)
  if (is.null(stmt)) return(no("empty body"))
  cl <- .mm_gate_call_of(stmt)
  if (is.null(cl) || !is.call(cl) || is.na(.mm_call_head_name(cl)))
    return(no("first statement is not a gate call"))
  v <- .mm_gate_chain(cl, unit, fns, bound = bound, eps = eps, binds = binds)
  list(gated = v$ok, reason = v$reason, chain = v$chain)
}

audit_entrypoint_gating <- function(skill_dir,
                                    manifest_path = file.path(skill_dir, "MANIFEST.yaml")) {
  m <- read_method_manifest(manifest_path)
  rows <- list()
  for (u in names(m$units)) {
    unit <- m$units[[u]]
    ref <- unit$reference_path
    if (!grepl("\\.[Rr]$", ref) || !file.exists(ref)) next
    fns <- .mm_top_level_bindings(ref)
    bound <- .mm_file_bound_names(ref)
    # The binding index with its per-file attributes: `attaches` (does this
    # reference file attach a package? fix round 5, K1), `reflective` (does
    # its top level use a reflective or evaluating form? fix round 4 of batch
    # 11) and `blocks` (does it hold a load-bearing block that is neither the
    # validator nor a dependency environment? integrity batch 12). Carried on
    # `binds` so the trust predicate can refuse without the path being
    # threaded through four call layers; built by .mm_audit_binds() so the
    # audit, validation and generation judge the same thing.
    binds <- .mm_audit_binds(ref, fns)
    # What the gate literals must name: THIS skill directory and THIS
    # reference file (integrity batch 14, R13-P1-3).
    attr(binds, "expect_skill_dir") <- skill_dir
    attr(binds, "expect_reference_path") <- ref
    # What this reference file loads into the object(s) that hold the gate.
    # Reported so the SME sees it at approval: past the audit's reach, the
    # controls are the manifest digest and the SME's review of the diff.
    loads <- paste(.mm_trusted_loads_of_file(binds, names(fns),
                                             .mm_shadow_census_of(fns)),
                   collapse = ", ")
    all_eps <- unique(unlist(lapply(m$units, function(x) x$entrypoints)))
    # A file that builds ANY closure with the bare `local` has every entry
    # point refused, naming the closure, the file and the fix (integrity
    # batch 13, R12-P0-2): an accessor so built is not on the gate chain, yet
    # the caller's `local` decides what it forwards to.
    bare_local <- attr(binds, "bare_local")
    for (ep in unit$entrypoints) {
      verdict <- if (length(bare_local))
                   list(gated = FALSE, reason = bare_local[1], chain = character(0))
                 else if (ep %in% names(fns))
                   .mm_entrypoint_gated(fns[[ep]], fns, u, bound,
                                        setdiff(all_eps, ep), binds)
                 else list(gated = FALSE, reason = "not defined in the reference file",
                           chain = character(0))
      rows[[length(rows) + 1L]] <- data.frame(
        unit = u, status = unit$status, reference_path = ref, entrypoint = ep,
        gated = verdict$gated, reason = verdict$reason,
        gate_functions = paste(verdict$chain, collapse = " -> "),
        trusted_loads = loads,
        stringsAsFactors = FALSE)
    }
  }
  out <- if (length(rows)) do.call(rbind, rows) else
    data.frame(unit = character(0), status = character(0),
               reference_path = character(0), entrypoint = character(0),
               gated = logical(0), reason = character(0), gate_functions = character(0),
               trusted_loads = character(0),
               stringsAsFactors = FALSE)
  bad <- out[!out$gated, , drop = FALSE]
  if (nrow(bad)) {
    describe <- function(b) sprintf("%s::%s (unit '%s', %s; %s)", basename(b$reference_path),
                                    b$entrypoint, b$unit, b$status, b$reason)
    hard <- bad[bad$status == "approved", , drop = FALSE]
    soft <- bad[bad$status != "approved", , drop = FALSE]
    if (nrow(soft))
      warning("method manifest ", manifest_path, ": draft/rejected entry point(s) ",
              "do not call the skill gate: ",
              paste(describe(soft), collapse = "; "), call. = FALSE)
    if (nrow(hard))
      stop("method manifest ", manifest_path, ": APPROVED entry point(s) do not ",
           "call the skill gate (the unit is authorized but the door is open): ",
           paste(describe(hard), collapse = "; "),
           ". Gate every manifested entry point at the top of its body.",
           call. = FALSE)
  }
  invisible(out)
}

# Parses AND validates a manifest against the strict contract in the header.
# Returns the parsed manifest (as read_method_manifest) on success; stops with
# the first violation otherwise. `skill_dir` defaults to the manifest's own
# directory; reference and dependency paths are resolved against getwd(), which
# is the repository root for every caller (pipelines, ci_check, the gate).
# THE STATIC CENSUS CACHE (integrity batch 15, batch 13/14 queue item). Every
# gated entry-point call validates its skill's whole manifest, and for each unit
# that means parse-walking the reference file twice: once for its top-level
# function names and once for the trusted-load census. eda-standards has six
# units over one ~2500-line file, so a gated eda call paid that walk twelve
# times for bytes that had not changed.
#
# WHAT IS CACHED, EXACTLY (corrected in integrity batch 16, Codex round 15
# R15-P2-1; the previous wording claimed more than was true). ONLY the
# reference's top-level FUNCTION NAMES are memoised. That census is a pure
# function of the reference file's bytes, whose sha256 is part of the key, so a
# hit cannot weaken a refusal.
#
# THE TRUSTED-LOAD CENSUS IS ALSO CACHED, BUT ITS KEY CARRIES THE FILESYSTEM.
# That census is NOT a pure function of the two hashed files: it consults the
# top-level bindings of each file the reference loads, and its verdict depends
# on whether a loaded file exists at all. Under the two-part key a validation
# run made while an undeclared dependency was ABSENT concluded "not a trusted
# load", cached that, and kept passing after the dependency was created -- while
# a fresh census would have discovered the load and refused it as undeclared.
# The key therefore also carries the sha256 (or "<missing>") of every path the
# census can read; see .mm_census_fs_key() below.
#
# The cache is keyed on the sha256 of the MANIFEST, the sha256 of the REFERENCE
# -- both recomputed from disk by validate_method_manifest() on every call,
# before the cache is consulted -- and that filesystem term. It lives for the
# life of the R session, in a base-parented environment; there is no disk
# cache.
.mm_census_cache <- base::new.env(parent = base::baseenv())

# THE FILESYSTEM TERM OF THE KEY (integrity batch 16, R15-P2-1; rewritten in fix
# round 1, review blocker B3). Every path the trusted-load census can read is
# named as a character constant in the reference itself: the census follows the
# file's own load expressions and, for each, asks .mm_load_binding_names() for
# that file's top-level names -- a file that does not exist contributes none.
# This takes the SUPERSET of those paths -- every character constant anywhere in
# the reference's PARSE TREE that ends in `.R` or `.r` -- plus this validator,
# and folds each one's sha256, or "<missing>", into the key. Creating, deleting
# or editing any of them changes the key and forces a fresh census, which is
# exactly the scenario the finding describes: validate with an undeclared
# dependency absent (not a trusted load), create it, validate again (a trusted
# load, and undeclared -- refused). The superset is deliberate: over-including a
# path can only invalidate the cache more often, never less.
#
# WHY THE PARSE TREE AND NOT A REGEX OVER THE TEXT. Fix round 0 matched
# `"[^"]+\.[Rr]"` against the file's lines, which sees DOUBLE-quoted literals
# only. R accepts single-quoted strings and the trusted-load census reads them,
# so a dependency written `base::sys.source('tools/x.R', ...)` was invisible to
# the key and reproduced the stale-cache defect in full -- the finding's own
# scenario, behind a comment that claimed every string literal. Parsing removes
# the whole class: `base::parse()` yields the constant, whatever quoting,
# escaping or line breaking produced it. Nothing is evaluated; the walk reads
# constants out of the tree. If the file does not parse, the key falls back to
# the sha256 of its bytes prefixed `<unparsed>`, which is a DIFFERENT key from
# any parsed one and so can only force a recomputation.
.mm_char_constants <- function(e, acc = base::character(0)) {
  if (base::is.character(e)) return(base::c(acc, e))
  if (base::is.call(e) || base::is.pairlist(e) || base::is.expression(e) ||
      base::is.list(e)) {
    l <- base::as.list(e)
    for (i in base::seq_along(l)) {
      # A call can carry the empty symbol (a missing argument); comparing it
      # through single-bracket indexing never binds it to a variable.
      if (!base::identical(base::`[`(l, i), base::list(base::quote(expr = ))))
        acc <- .mm_char_constants(base::`[[`(l, i), acc)
    }
  }
  acc
}
# EVERY PATH THE CENSUS CAN RESOLVE, NOT EVERY STRING THAT LOOKS LIKE ONE
# (integrity batch 17, Codex round 16 R16-P2-1). The trusted-load census reads
# a dependency path through `.mm_static_path()`, which resolves a character
# literal AND `base::file.path()` of literals. Collecting bare character
# constants missed the assembled form: a reference declaring
# `base::file.path("tools", "probe_dep.R")` put `probe_dep.R` in the cache key
# while the census read `tools/probe_dep.R`, so creating the dependency left
# the key unchanged and the "not a trusted load" verdict cached from the run
# made while it was absent kept passing. This walk applies the census's own
# resolver to every node, so the key names the paths the census actually reads.
.mm_static_path_constants <- function(e, acc = base::character(0)) {
  p <- base::tryCatch(.mm_static_path(e), error = function(x) NA_character_)
  if (!base::is.na(p)) acc <- base::c(acc, p)
  if (base::is.call(e) || base::is.pairlist(e) || base::is.expression(e)) {
    l <- base::as.list(e)
    for (i in base::seq_along(l)) {
      if (!base::identical(base::`[`(l, i), base::list(base::quote(expr = ))))
        acc <- .mm_static_path_constants(base::`[[`(l, i), acc)
    }
  }
  acc
}
# THE CANDIDATE LIST IS CACHED, THE FILESYSTEM STATE IS NOT. The set of
# character constants in a reference is a pure function of that file's bytes,
# whose sha256 keys this cache, so parsing it once per byte state is sound --
# and necessary: every gated call validates its skill's whole manifest, so
# without this the ~2800-line eda.R was re-parsed twelve times per call and the
# warm cost of a gated call went from 0.217 s to 0.750 s (measured, fix round
# 1). What must NOT be cached is the hashing below: that is the whole point of
# the filesystem term, and it runs on every call.
#
# WHAT INCLUDING `tools/method_manifest.R` IN THE KEY DOES AND DOES NOT DO (SME
# decision D17-3, integrity batch 17). Its hash is in the candidate list so that
# an edit to this validator invalidates every cached census -- a cache-freshness
# term, nothing more. It does NOT authenticate this file against an approved
# digest, and no such identity exists: the SME's decision is that the validator's
# trust root remains the protected-diff boundary enforced by
# .claude/hooks/protect_paths.py, which is also why the declaration exemption
# below removes "tools/method_manifest.R" from the paths a reference must declare.
.mm_fs_cand_cache <- base::new.env(parent = base::baseenv())
.mm_census_fs_cands <- function(ref, ref_sha) {
  hit <- base::`[[`(.mm_fs_cand_cache, ref_sha)
  if (!base::is.null(hit)) return(hit)
  ex <- base::tryCatch(base::parse(ref, keep.source = FALSE), error = function(e) NULL)
  val <- if (base::is.null(ex)) NULL else {
    lits <- .mm_static_path_constants(ex)
    lits <- lits[base::grepl("\\.[Rr]$", lits)]
    cand <- base::unique(base::c(lits, "tools/method_manifest.R"))
    base::sort(cand[base::nzchar(cand)])
  }
  base::`[[<-`(.mm_fs_cand_cache, ref_sha, base::list(cand = val))
  base::list(cand = val)
}
.mm_census_fs_key <- function(ref) {
  ref_sha <- .mm_file_sha256(ref)
  cand <- base::`[[`(.mm_census_fs_cands(ref, ref_sha), "cand")
  if (base::is.null(cand)) return(base::paste0("<unparsed>=", ref_sha))
  base::paste(base::vapply(cand, function(p)
    base::paste0(p, "=", if (base::file.exists(p)) .mm_file_sha256(p) else "<missing>"),
    base::character(1)), collapse = "|")
}

.mm_census_of <- function(ref, key) {
  hit <- base::`[[`(.mm_census_cache, key)
  if (!base::is.null(hit)) return(hit)
  tlb <- .mm_top_level_bindings(ref)
  val <- base::list(
    defined = .mm_top_level_functions(ref),
    trusted = tryCatch(.mm_trusted_loads_of_file(.mm_audit_binds(ref, tlb), names(tlb),
                                                 .mm_shadow_census_of(tlb)),
                       error = function(e) character(0)))
  base::`[[<-`(.mm_census_cache, key, val)
  val
}

.mm_reserved_reviewer <- function(reviewer) {
  normalized <- tolower(trimws(gsub("[\\h\\v]+", " ", reviewer, perl = TRUE)))
  normalized %in% c("sme", "the sme")
}

validate_method_manifest <- function(manifest_path,
                                     skill_dir = dirname(manifest_path)) {
  m <- read_method_manifest(manifest_path)
  manifest_sha <- .mm_file_sha256(manifest_path)
  bad <- function(...) stop("method manifest ", manifest_path, ": ", ...,
                            call. = FALSE)
  skill <- basename(normalizePath(skill_dir, mustWork = FALSE))
  if (!identical(m$skill, skill))
    bad("skill: ", m$skill, " does not equal the directory name '", skill, "'")
  if (length(m$units) == 0) bad("units: block is empty")
  if (nzchar(m$reviewed_by)) {
    if (.mm_reserved_reviewer(m$reviewed_by))
      bad("reviewed_by uses a reserved role label; enter the local reviewer's name")
    if (!nzchar(m$reviewed_at))
      bad("reviewed_by is filled but reviewed_at is empty; a confirmation carries its date")
    if (!.mm_is_iso_date(m$reviewed_at))
      bad("reviewed_at '", m$reviewed_at, "' is not an ISO calendar date (YYYY-MM-DD)")
  } else if (nzchar(m$reviewed_at) && !.mm_is_iso_date(m$reviewed_at)) {
    bad("reviewed_at '", m$reviewed_at, "' is not an ISO calendar date (YYYY-MM-DD)")
  }
  under <- paste0("skills/", skill, "/")
  sha_ok <- function(x) grepl("^sha256:[0-9a-f]{64}$", x)
  ep_owner <- list()
  for (u in names(m$units)) {
    unit <- m$units[[u]]
    if (!unit$status %in% c("approved", "draft", "rejected"))
      bad("unit '", u, "' has bad status '", unit$status, "'")
    ref <- unit$reference_path
    if (!nzchar(ref)) bad("unit '", u, "' has an empty reference_path")
    if (!startsWith(ref, under) || grepl("(^|/)\\.\\.(/|$)", ref))
      bad("unit '", u, "' reference_path ", ref, " is not under ", under)
    if (!file.exists(ref)) bad("unit '", u, "' reference_path ", ref, " does not exist")
    if (!sha_ok(unit$sha256))
      bad("unit '", u, "' has a malformed sha256 '", unit$sha256,
          "'; expected sha256:<64 lowercase hex>")
    actual <- .mm_file_sha256(ref)
    if (!identical(actual, unit$sha256))
      bad("unit '", u, "': ", basename(ref), " digest drift (manifest ", unit$sha256,
          " vs disk ", actual, ")")
    if (length(unit$entrypoints) == 0) bad("unit '", u, "' has no entrypoints")
    census_key <- paste0(manifest_sha, "|", actual, "|", .mm_census_fs_key(ref))
    if (grepl("\\.[Rr]$", ref)) {
      defined <- .mm_census_of(ref, census_key)$defined
      for (ep in unit$entrypoints) {
        if (!ep %in% defined)
          bad("unit '", u, "' entry point '", ep, "' is not a top-level function ",
              "defined in ", ref)
      }
      # APPROVED PLACEHOLDER (Codex round 8, finding 1). An approved unit whose
      # reference file declares itself unwritten is an approval of nothing:
      # the SME signed a method that does not exist. The marker is a comment
      # line carrying the word PLACEHOLDER (the convention every stub in this
      # repository uses: "# STATUS: PLACEHOLDER, NOT WRITTEN"). Hard stop for
      # approved units only; a draft placeholder is the honest authoring state.
      if (identical(unit$status, "approved") && .mm_has_placeholder_marker(ref))
        bad("unit '", u, "' is approved but ", ref, " carries a PLACEHOLDER ",
            "marker: approved unit points at unwritten method. Reset the unit ",
            "to draft; approval waits for the authored, validated method")
    } else {
      if (!identical(unit$entrypoints, basename(ref)))
        bad("unit '", u, "' is a non-R reference; its single entry point must be ",
            "the file name '", basename(ref), "', got [",
            paste(unit$entrypoints, collapse = ", "), "]")
    }
    for (ep in unit$entrypoints) ep_owner[[ep]] <- c(ep_owner[[ep]], u)
    # R10-P1-5. A file the reference LOADS into its sealed validator object is
    # ON that object's lookup chain, so its contents decide what the gate calls
    # -- exactly as the reference file's own contents do. The audit reports
    # those loads in `trusted_loads`; the manifest must BIND them, or later
    # drift in a loaded helper leaves the approved manifest valid and the
    # running method silently different from the signed one. The write guard
    # protects a fixed core set plus skills/*/reference/*.R, not arbitrary
    # tools/*.R helpers, so the digest here is the binding control.
    #
    # tools/method_manifest.R is the one exception, and necessarily: it IS the
    # validator and IS this check. Its controls are the write guard, the
    # .harness-unlock window and the SME's review of the diff.
    if (grepl("\\.[Rr]$", ref)) {
      tl <- .mm_census_of(ref, census_key)$trusted
      tl <- setdiff(tl, "tools/method_manifest.R")
      declared <- if (length(unit$depends))
        vapply(unit$depends, function(d) d$path, character(1)) else character(0)
      undeclared <- setdiff(tl, declared)
      if (length(undeclared))
        bad("unit '", u, "' loads ", paste(undeclared, collapse = ", "),
            " into its sealed validator object but does not declare it under ",
            "depends:; a trusted load is a dependency and is bound by its ",
            "digest. Regenerate the manifest with write_method_manifest()")
    }
    for (dp in unit$depends) {
      if (grepl("(^|/)\\.\\.(/|$)", dp$path) || startsWith(dp$path, "/"))
        bad("unit '", u, "' depends on ", dp$path, ", which is not a repo-relative path")
      if (!file.exists(dp$path))
        bad("unit '", u, "' depends on ", dp$path, ", which does not exist")
      if (!sha_ok(dp$sha256))
        bad("unit '", u, "' depends entry ", dp$path, " has a malformed sha256 '",
            dp$sha256, "'")
      da <- .mm_file_sha256(dp$path)
      if (!identical(da, dp$sha256))
        bad("unit '", u, "' depends on ", dp$path, ": digest drift (manifest ",
            dp$sha256, " vs disk ", da, ")")
    }
  }
  multi <- Filter(function(x) length(x) > 1L, ep_owner)
  if (length(multi)) {
    ep <- names(multi)[1]
    bad("entry point '", ep, "' is bound to more than one unit (units ",
        paste(multi[[1]], collapse = ", "), "); each public entry point belongs ",
        "to exactly one unit")
  }
  invisible(m)
}

# --- manifest writer -------------------------------------------------------
# Generates/updates skills/<name>/MANIFEST.yaml in schema v2.
#
# `units_spec` is a named list, one element per intended-use unit:
#   list(<unit> = list(reference_path = "skills/.../reference/x.R",
#                      entrypoints = c("fn1", "fn2"),
#                      status = "approved" | "draft" | "rejected",
#                      evidence = "<pointer>",
#                      reason = "<derivation reason>"))
# The sha256 is computed here from reference_path on disk.
#
# PRESERVATION (item 2): regeneration keeps an existing unit's status and the
# root reviewed_by UNLESS the reference file's content hash changed for that
# unit. A hash drift resets that unit to `draft` AND clears root reviewed_by
# (with a message) -- this prevents a silent self-blessing where a method is
# edited but keeps its old approval. A brand-new unit, or a skill with no prior
# v2 manifest, takes the status given in `units_spec`. Returns the manifest
# path invisibly.
write_method_manifest <- function(skill_dir, units_spec) {
  name <- basename(normalizePath(skill_dir, mustWork = FALSE))
  manifest_path <- file.path(skill_dir, "MANIFEST.yaml")

  old <- if (file.exists(manifest_path)) tryCatch(read_method_manifest(manifest_path),
                                                  error = function(e) NULL) else NULL
  old_units <- if (!is.null(old)) old$units else list()
  reviewed_by <- if (!is.null(old)) old$reviewed_by else ""
  reviewed_at <- if (!is.null(old)) old$reviewed_at else ""

  drift <- FALSE
  resolved <- list()
  for (u in names(units_spec)) {
    spec <- units_spec[[u]]
    ref <- spec$reference_path
    if (!file.exists(ref)) {
      stop("write_method_manifest: reference file for unit '", u, "' not found: ",
           ref, call. = FALSE)
    }
    new_hash <- .mm_file_sha256(ref)
    ou <- old_units[[u]]
    if (!is.null(ou) && nzchar(ou$status)) {
      if (identical(ou$sha256, new_hash)) {
        status <- ou$status                       # preserve existing status
      } else {
        status <- "draft"                         # hash drift resets to draft
        drift <- TRUE
        message("method_manifest: unit '", u, "' of skill '", name,
                "' drifted (reference hash changed); status reset to draft.")
      }
    } else {
      status <- spec$status %||% "draft"           # new unit -> spec status
    }
    # Per-unit dependencies (round 7, finding 5): repo-relative paths of code
    # outside the skill the unit relies on; each is digested here from disk.
    # R10-P1-5: a trusted load IS a dependency, so the generator derives them
    # from the reference rather than trusting the caller's spec to list them.
    # validate_method_manifest() refuses a unit whose trusted loads are not all
    # declared here, so the two can never disagree.
    dep_paths <- as.character(spec$depends %||% character(0))
    if (grepl("\\.[Rr]$", ref)) {
      tl <- tryCatch({
        tlb <- .mm_top_level_bindings(ref)
        .mm_trusted_loads_of_file(.mm_audit_binds(ref, tlb), names(tlb),
                                  .mm_shadow_census_of(tlb))
      }, error = function(e) character(0))
      dep_paths <- union(dep_paths, setdiff(tl, "tools/method_manifest.R"))
    }
    deps <- list()
    for (dp in dep_paths) {
      if (!file.exists(dp))
        stop("write_method_manifest: dependency of unit '", u, "' not found: ", dp,
             call. = FALSE)
      deps[[length(deps) + 1L]] <- list(path = .mm_relpath(dp), sha256 = .mm_file_sha256(dp))
    }
    resolved[[u]] <- list(status = status,
                          reference_path = .mm_relpath(ref),
                          entrypoints = spec$entrypoints %||% character(0),
                          sha256 = new_hash,
                          evidence = spec$evidence %||% "see SKILL.md",
                          reason = spec$reason %||% "",
                          depends = deps)
  }
  # REGENERATION CLEARS ROOT (integrity batch 3b, item C2; round-4 HIGH 3).
  # ANY regeneration that changes the manifest's unit content clears the root
  # reviewed_by/reviewed_at, not only a reference-hash drift: adding a new unit,
  # changing an entrypoint list, a reference_path, evidence or reason all reopen
  # the SME confirmation. Only a BYTE-IDENTICAL rewrite (the resolved unit set
  # equals the prior one field-for-field) preserves the signature. This kills
  # the new-unit-inherits-signature and metadata-swap blessing paths: a new or
  # altered unit can never ride in under the old root sign-off.
  content_changed <- !is.null(old) && length(old_units) > 0 &&
    !.mm_units_identical(old_units, resolved)
  if ((drift || content_changed) && nzchar(reviewed_by)) {
    reason <- if (drift) "a unit's reference hash drifted"
              else "the manifest's unit content changed"
    message("method_manifest: clearing root reviewed_by of skill '", name,
            "' because ", reason, "; the SME must re-confirm.")
    reviewed_by <- ""
    reviewed_at <- ""
  }
  if (.mm_reserved_reviewer(reviewed_by))
    stop("write_method_manifest: reviewed_by uses a reserved role label; ",
         "enter the local reviewer's name.", call. = FALSE)

  q <- function(s) paste0('"', gsub('"', '\\\\"', s), '"')
  lines <- c(
    sprintf("# MANIFEST.yaml -- method manifest (schema v2) for skill '%s'.", name),
    "# Generated by tools/method_manifest.R::write_method_manifest.",
    "# reviewed_by is EMPTY until the SME confirms the derived unit statuses;",
    "# while empty every unit is treated as draft by the self-gate (the legacy",
    "# signature fallback was retired in batch 3c, 2026-09-01). Fill",
    "# reviewed_by + reviewed_at to make the per-unit statuses + digests below",
    "# authoritative.",
    sprintf("skill: %s", name),
    sprintf("generated_at: %s",
            format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
    sprintf("reviewed_by: %s", q(reviewed_by)),
    sprintf("reviewed_at: %s", q(reviewed_at)),
    "units:"
  )
  for (u in names(resolved)) {
    r <- resolved[[u]]
    lines <- c(lines,
               sprintf("  %s:", u),
               sprintf("    status: %s", r$status),
               sprintf("    reference_path: %s", r$reference_path),
               sprintf("    entrypoints: [%s]", paste(r$entrypoints, collapse = ", ")),
               sprintf("    sha256: %s", r$sha256))
    if (length(r$depends)) {
      lines <- c(lines, "    depends:")
      for (dp in r$depends)
        lines <- c(lines, sprintf("      - path: %s", dp$path),
                   sprintf("        sha256: %s", dp$sha256))
    }
    lines <- c(lines,
               sprintf("    evidence: %s", r$evidence),
               sprintf("    reason: %s", q(r$reason)))
  }
  # The rename below would replace a read-only file, so refuse it here, as the
  # direct write did before (G3 remediation, QA probe L1).
  if (file.exists(manifest_path) && file.access(manifest_path, 2L) != 0L)
    stop("write_method_manifest: method manifest ", manifest_path,
         " is read-only; it is not replaced", call. = FALSE)
  temporary <- tempfile(".MANIFEST-", tmpdir = dirname(manifest_path))
  on.exit(unlink(temporary), add = TRUE)
  writeLines(lines, temporary)
  read_method_manifest(temporary)
  if (!file.rename(temporary, manifest_path))
    stop("write_method_manifest: could not replace ", manifest_path, call. = FALSE)
  invisible(manifest_path)
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# Field-for-field equality of two resolved unit sets (unit names + every unit
# field). Used to tell a byte-identical regeneration (preserve the root
# signature) from a substantive one (clear it). entrypoints compare as ordered
# character vectors; all other fields as scalars.
.mm_units_identical <- function(a, b) {
  if (!identical(sort(names(a)), sort(names(b)))) return(FALSE)
  flds <- c("status", "reference_path", "entrypoints", "sha256",
            "evidence", "reason")
  dep_key <- function(d) vapply(d %||% list(), function(x) paste(x$path, x$sha256),
                                character(1))
  for (u in names(a)) {
    for (f in flds) {
      if (!identical(a[[u]][[f]], b[[u]][[f]])) return(FALSE)
    }
    # depends is signed content too: adding, dropping or re-digesting one
    # reopens the confirmation.
    if (!identical(unname(dep_key(a[[u]]$depends)), unname(dep_key(b[[u]]$depends))))
      return(FALSE)
  }
  TRUE
}

# --- self-gate core --------------------------------------------------------
# `is_method_validated()` is the non-executing inspection API. It returns TRUE
# only for a confirmed approved unit, FALSE only for a structurally valid
# unconfirmed/draft/rejected unit, and hard-stops on every integrity defect.
# `require_validated_method()` is the execution authorization API: it delegates
# all inspection to that query, then throws on FALSE. Entrypoint certification
# recognizes only the throwing API, so discarding its return value is safe.
#
#   skill_dir     directory of the skill (contains SKILL.md, reference/)
#   calling_file  the reference .R file this gate protects (repo-relative,
#                 matching its unit's reference_path)
#   unit          the intended-use unit the calling entry point belongs to
#   legacy        DEPRECATED and ignored (batch 3c, 2026-09-01). It fed the
#                 retired transitional fallback; the parameter is kept so the
#                 signed reference files that still pass a closure keep calling
#                 without edits (editing them would invalidate their unit
#                 digests). Remove it from callers whenever their skill is next
#                 re-validated for other reasons.
# TEST-ONLY OBSERVATION OF A PASSED GATE (integrity batch 16, Codex round 15
# R15-P2-5). The default-loader battery in tools/tests/test_batch14.R INFERRED
# that a gate had run and passed from the shape of the error a shallow call
# produced -- "unused argument", "argument is missing". Both are raised by R's
# own argument matching BEFORE the body executes, and a gate-wrapper prologue
# can raise an accepted input error before reaching the validator, so an
# accepted outcome was not evidence of a passed gate. This records the fact
# directly.
#
# INERT IN EVERY REAL INVOCATION. It writes nothing unless BOTH TEST_REPO_ROOT
# and MM_GATE_OBSERVE are set in the environment -- the same convention the
# other test-only hooks in this repository use. It observes; it never decides:
# no refusal depends on it, and a failure to write is swallowed.
.mm_observe_gate_pass <- function(calling_file, unit) {
  if (!nzchar(Sys.getenv("TEST_REPO_ROOT"))) return(invisible(NULL))
  path <- Sys.getenv("MM_GATE_OBSERVE")
  if (!nzchar(path)) return(invisible(NULL))
  tryCatch(cat(sprintf("%s|%s\n", calling_file, unit), file = path, append = TRUE),
           error = function(e) NULL)
  invisible(NULL)
}

is_method_validated <- function(skill_dir, calling_file, unit,
                                legacy = NULL) {
  if (missing(unit) || is.null(unit) || !nzchar(unit)) {
    stop("is_method_validated: a unit name is required (schema v2). ",
         "The calling entry point must declare its intended-use unit.",
         call. = FALSE)
  }
  manifest_path <- file.path(skill_dir, "MANIFEST.yaml")

  if (!file.exists(manifest_path)) {
    stop("method manifest missing for skill ", skill_dir, " (", manifest_path,
         "): schema v2 requires a per-unit manifest. Regenerate it with ",
         "write_method_manifest().", call. = FALSE)
  }
  # STRICT (round 7, finding 5): the manifest must parse under the strict
  # reader AND pass validate_method_manifest() — identity, entry points,
  # digests, dependencies, date. Any violation is an integrity hard stop with
  # the validator's own message, never a plain FALSE.
  m <- tryCatch(validate_method_manifest(manifest_path, skill_dir), error = function(e) e)
  if (inherits(m, "error")) {
    stop(conditionMessage(m), " -- the manifest is malformed (schema v2); ",
         "regenerate it with write_method_manifest() and have the SME re-confirm.",
         call. = FALSE)
  }
  u <- m$units[[unit]]
  if (is.null(u)) {
    stop("method manifest ", manifest_path, " has no unit '", unit, "' -- the ",
         "manifest does not cover this entry point. The SME must regenerate ",
         "and re-confirm it.", call. = FALSE)
  }

  # Hash binding is enforced in EVERY state: an edited reference file is a hard
  # stop whether or not the SME has confirmed the manifest.
  actual <- .mm_file_sha256(calling_file)
  if (!identical(u$sha256, actual)) {
    stop("method file ", calling_file, " (unit '", unit, "') was EDITED after ",
         "its manifest digest was recorded (mismatch: manifest ", u$sha256,
         " vs disk ", actual, "). The record no longer covers this file; the ",
         "SME must regenerate and re-confirm the manifest.", call. = FALSE)
  }

  # Dependencies outside the skill (depends:) are bound the same way: an
  # edited dependency is a hard stop in every state.
  for (dp in u$depends) {
    da <- .mm_file_sha256(dp$path)
    if (!identical(dp$sha256, da)) {
      stop("dependency ", dp$path, " of method unit '", unit, "' (", calling_file,
           ") was EDITED after its manifest digest was recorded (mismatch: manifest ",
           dp$sha256, " vs disk ", da, "). The SME must regenerate and re-confirm ",
           "the manifest.", call. = FALSE)
    }
  }

  if (nzchar(trimws(m$reviewed_by))) {
    # root reviewed_by is filled -> unit statuses are authoritative.
    return(invisible(isTRUE(identical(u$status, "approved"))))
  }

  # reviewed_by empty -> the derived unit statuses are not yet SME-confirmed;
  # every unit is treated as draft. (The transitional fallback to a legacy
  # SKILL*.md signature grep was retired in batch 3c — see the header.)
  warning("method manifest ", manifest_path, " is awaiting SME confirmation ",
          "(empty reviewed_by) -- unit '", unit, "' is treated as draft ",
          "until the SME fills reviewed_by.", call. = FALSE)
  invisible(FALSE)
}

require_validated_method <- function(skill_dir, calling_file, unit,
                                     legacy = NULL) {
  passed <- is_method_validated(skill_dir = skill_dir,
                                calling_file = calling_file,
                                unit = unit,
                                legacy = legacy)
  if (!isTRUE(passed)) {
    stop("Method file ", calling_file,
         " is not yet SME-validated (not SME-validated for execution): unit '",
         unit, "' in ", file.path(skill_dir, "MANIFEST.yaml"),
         " is not execution-authorized because it is draft, rejected, or awaiting ",
         "SME confirmation. Loading for inspection remains allowed; computation ",
         "is blocked until the SME approves the exact method bytes.", call. = FALSE)
  }
  .mm_observe_gate_pass(calling_file, unit)
  invisible(TRUE)
}
