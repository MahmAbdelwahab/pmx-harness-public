# Shared report number-census declaration checks and command construction.
# Definitions only: loading this file never reads report inputs.
report_number_census_prepare <- function(number_census, report_dir, source_files) {
  refuse <- function(field, ...)
    stop("reporting: ", field, ": ", ..., call. = FALSE)
    census_classes <- c("patterns", "phrases", "literals", "functions")
    if (!is.list(number_census) || is.null(names(number_census)) ||
        !identical(sort(names(number_census)), sort(census_classes)))
      refuse("number_census", "must be a list with exactly patterns, phrases, literals and functions")
    for (k in census_classes) {
      v <- number_census[[k]]
      if (!is.character(v) || anyNA(v) || !all(nzchar(v)) || anyDuplicated(v))
        refuse("number_census", k, ": must be a character vector of unique non-empty strings")
    }
    for (p in number_census[["patterns"]])
      if (!isTRUE(tryCatch({ grepl(p, "", perl = TRUE); TRUE },
                           error = function(e) FALSE, warning = function(w) FALSE)))
        refuse("number_census", "patterns: '", p, "' is not a valid Perl regular expression")
    # A literals entry is <file>|<context>|<text>; <file> is a report source
    # relative to the report directory; <context> may be `#|` (a whole option
    # line or an !expr token).
    source_rel <- sub("^/", "", sub(report_dir, "", source_files, fixed = TRUE))
    lit <- number_census[["literals"]]
    lit_ok <- grepl("^[^|]+[|](#[|]|[^|]+)[|].+$", lit) & sub("[|].*$", "", lit) %in% source_rel
    if (!all(lit_ok))
      refuse("number_census", "literals: not of the form <file>|<context>|<text> with <file> ",
             "a report source: ", paste(lit[!lit_ok], collapse = "; "))
    # Decision 10 rule 7 item 6 (revision 1.2): names that may never be
    # declared, and (item 5) the subset that a helper file may not name.
    deny_helper <- c("glue", "glue_data", "str_glue", "eval", "evalq", "parse", "str2lang",
                     "do.call", "match.fun", "get", "mget", "Reduce", "sys.source",
                     "tar_read_raw", "tar_load", "tar_load_raw")
    deny <- c("sum", "mean", "median", "min", "max", "range", "quantile", "diff", "cumsum",
              "cumprod", "prod", "nrow", "NROW", "ncol", "NCOL", "length", "lengths", "table",
              "tabulate", "abs", "sqrt", "exp", "log", "log2", "log10", "log1p", "expm1",
              "round", "signif", "ceiling", "floor", "trunc", "sd", "var", "cor", "seq",
              "seq_len", "seq_along", "rank", "colSums", "rowSums", "colMeans", "rowMeans",
              "scale", "nchar", "which", "which.max", "which.min", "as.numeric", "as.integer",
              "as.double", deny_helper)
    # Ruling G5: in the .qmd files the readers are denied as values too (as
    # calls they keep rulings A3 and A4); the helpers may not read files.
    helper_readers <- c("readRDS", "load", "readLines", "scan", "read.csv", "read.table",
                        "read.delim")
    # Ruling G12: the file readers are denied as values in the .qmd files too.
    deny_value <- c("tar_read", "source", "sys.source", helper_readers)
    # Ruling G1: a name is compared with surrounding quotes or backticks removed.
    unquote <- function(x) gsub("^[\"'`]|[\"'`]$", "", x, perl = TRUE)
    denied <- intersect(unquote(number_census[["functions"]]), c(deny, helper_readers))
    if (length(denied))
      refuse("number_census", "functions: on the deny list: ", paste(denied, collapse = ", "))
  list(number_census = number_census, deny = deny, deny_helper = deny_helper,
       helper_readers = helper_readers, deny_value = deny_value)
}

report_number_census_command <- function(report_dir, prepared, readable, declared_texts) {
  number_census <- prepared$number_census
  deny <- prepared$deny
  deny_helper <- prepared$deny_helper
  helper_readers <- prepared$helper_readers
  deny_value <- prepared$deny_value
    bquote({
      report_source_files
      rdir <- .(report_dir)
      nc <- .(number_census)
      deny <- .(deny)
      deny_helper <- c(.(deny_helper), .(helper_readers))
      deny_value <- .(deny_value)
      readable <- .(readable)
      texts <- .(declared_texts)
      unquote <- function(x) gsub("^[\"'`]|[\"'`]$", "", x, perl = TRUE)
      # Every pattern on source text is Perl (or fixed): under a C locale the
      # default engine rewrites a non-ASCII byte as <xx>, which holds digits.
      # A digit is an ASCII digit or any Unicode number character (ruling G2).
      digit <- "[0-9]|\\p{N}"
      mask <- function(x) gsub(digit, "d", x, perl = TRUE)
      acc <- new.env()
      acc$rows <- list()
      add <- function(file, where, kind, context, text, ok) {
        if (length(text))
          acc$rows <- c(acc$rows, list(data.frame(
            file = file, where = where, kind = kind, context = context, shape = mask(text),
            status = ifelse(ok, "declared", "undeclared"), stringsAsFactors = FALSE)))
        invisible(NULL)
      }
      # Text units: phrases first (fixed strings), then the patterns in their
      # declared order (ruling A2); any digit left is undeclared.
      cleared <- function(u) {
        for (p in nc$phrases) u <- gsub(p, " ", u, fixed = TRUE)
        for (p in nc$patterns) u <- gsub(p, " ", u, perl = TRUE)
        !grepl(digit, u, perl = TRUE)
      }
      units <- function(file, where, kind, text, whole)
        add(file, where, kind, kind, trimws(text), whole | cleared(text))
      # The innermost enclosing call: the first ancestor expr whose first child
      # is an expr holding a SYMBOL_FUNCTION_CALL; otherwise <top>. Built once
      # per parse: parent, first child and called name, keyed by id.
      context_finder <- function(pd) {
        o <- pd[order(pd$parent, pd$line1, pd$col1), , drop = FALSE]
        fc <- o[!duplicated(o$parent), , drop = FALSE]
        first_id <- stats::setNames(as.character(fc$id), fc$parent)
        first_token <- stats::setNames(fc$token, fc$parent)
        fcall <- pd[pd$token == "SYMBOL_FUNCTION_CALL", , drop = FALSE]
        called <- stats::setNames(fcall$text, fcall$parent)
        up <- stats::setNames(pd$parent, pd$id)
        function(ids) vapply(ids, function(id) {
          p <- up[[as.character(id)]]
          while (p > 0L) {
            k <- as.character(p)
            if (identical(first_token[[k]], "expr") && first_id[[k]] %in% names(called))
              return(called[[first_id[[k]]]])
            p <- up[[k]]
          }
          "<top>"
        }, "")
      }
      head_of <- function(e) {
        h <- e[[1L]]
        if (is.name(h)) return(as.character(h))
        if (is.call(h) && identical(length(h), 3L) &&
            as.character(h[[1L]]) %in% c("::", ":::")) return(as.character(h[[3L]]))
        ""
      }
      # The call a SYMBOL_FUNCTION_CALL token heads, as text.
      call_text <- function(pd, id)
        utils::getParseText(pd, pd$parent[pd$id == pd$parent[pd$id == id]])
      as_call <- function(txt) tryCatch(str2lang(txt), error = function(e) NULL)
      # Rule 7 item 6 and ruling A3: the two declared forms of a source call.
      source_ok <- function(cl) {
        if (!is.call(cl) || !identical(length(cl), 2L) || !is.null(names(cl))) return(FALSE)
        a <- cl[[2L]]
        if (!is.call(a) || !is.null(names(a))) return(FALSE)
        (identical(head_of(a), "file.path") && identical(length(a), 4L) &&
           identical(a[[3L]], "sections") && is.character(a[[4L]]) &&
           a[[4L]] %in% c("format.R", "symbol-map.R")) ||
          (identical(head_of(a), "here") && identical(length(a), 3L) &&
             identical(a[[2L]], "tools") && identical(a[[3L]], "evidence_notes.R"))
      }
      # Item 3 (b) and ruling A4: tar_read reads a promoted name only.
      tar_read_ok <- function(cl)
        is.call(cl) && length(cl) > 1L && is.name(cl[[2L]]) &&
          as.character(cl[[2L]]) %in% readable
      # Item 3 (c): kable never rounds.
      kable_ok <- function(txt) {
        spd <- tryCatch(utils::getParseData(parse(text = txt, keep.source = TRUE,
                                                  encoding = "UTF-8")),
                        error = function(e) NULL)
        !is.null(spd) && !any(spd$token == "SYMBOL_SUB" & spd$text %in% c("digits", "format.args"))
      }
      # Item 5 and ruling A5: a helper's source call loads only the shared
      # typography file.
      # Exact source-shape exception for the shipped symbol-map helper. This
      # does not establish runtime target identity under rebinding or symlinks.
      canonical_typography <- quote(
        .source_report_typography <- function(into) {
          this_file <- NULL
          for (i in rev(seq_len(sys.nframe()))) {
            f <- sys.frame(i)$ofile
            if (is.character(f) && length(f) == 1L && nzchar(f)) { this_file <- f; break }
          }
          candidates <- c(
            if (!is.null(this_file) && file.exists(this_file))
              file.path(dirname(normalizePath(this_file)), "..", "..", "..", "..",
                        "tools", "report_typography.R"),
            "tools/report_typography.R",
            if (requireNamespace("here", quietly = TRUE))
              tryCatch(here::here("tools", "report_typography.R"),
                       error = function(e) NULL))
          hit <- candidates[file.exists(candidates)]
          if (!length(hit))
            stop("symbol-map.R: cannot locate tools/report_typography.R (the shared ",
                 "protect_dollars() definition); looked at: ",
                 paste(candidates, collapse = ", "), call. = FALSE)
          source(normalizePath(hit[[1]]), local = into)
          invisible(hit[[1]])
        }
      )
      helper_source_ok <- function(pd, id, file) {
        cl <- as_call(call_text(pd, id))
        if (!is.call(cl) || length(cl) < 2L) return(FALSE)
        args <- as.list(cl)[-1L]
        complete_args <- vapply(seq_along(args), function(i) tryCatch({
          value <- args[[i]]
          !identical(value, quote(expr = ))
        }, error = function(e) FALSE), logical(1))
        if (!all(complete_args)) return(FALSE)
        arg_names <- names(args)
        if (is.null(arg_names)) arg_names <- rep("", length(args))
        # There is one file argument and at most one explicitly named local
        # argument. Reject duplicates, partial names, and ambiguous positionals.
        if (any(!arg_names %in% c("", "file", "local")) ||
            sum(arg_names == "local") > 1L ||
            sum(arg_names %in% c("", "file")) != 1L) return(FALSE)
        file_arg <- tryCatch(args[[which(arg_names %in% c("", "file"))]],
                             error = function(e) NULL)
        # Preserve the original guard/enclosing-expression restriction as well
        # as checking the actual source argument. An unrelated allowed string
        # cannot authorize a computed target, or an unrelated R-file guard.
        top <- id
        seen <- integer()
        repeat {
          row <- which(pd$id == top)
          if (length(row) != 1L || top %in% seen) return(FALSE)
          seen <- c(seen, top)
          parent <- pd$parent[row]
          if (parent <= 0L) break
          top <- parent
        }
        tpd <- tryCatch(utils::getParseData(parse(text = utils::getParseText(pd, top),
          keep.source = TRUE, encoding = "UTF-8")), error = function(e) NULL)
        if (is.null(tpd)) return(FALSE)
        strings <- unquote(tpd$text[tpd$token == "STR_CONST"])
        if (!all(strings[grepl("[.]R$", strings, ignore.case = TRUE, perl = TRUE)] %in%
                 c("report_typography.R", "tools/report_typography.R"))) return(FALSE)
        if (is.character(file_arg) && length(file_arg) == 1L &&
            file_arg %in% c("report_typography.R", "tools/report_typography.R")) return(TRUE)
        if (!identical(file, "sections/symbol-map.R") ||
            !identical(cl, quote(source(normalizePath(hit[[1]]), local = into)))) return(FALSE)
        # Follow this source token's unique parent chain to its own assignment;
        # another matching function elsewhere in the file cannot grant permission.
        seen <- integer()
        while (id > 0L) {
          row <- which(pd$id == id)
          if (length(row) != 1L || id %in% seen) return(FALSE)
          seen <- c(seen, id)
          if (identical(pd$token[row], "expr") &&
              identical(as_call(utils::getParseText(pd, id)), canonical_typography)) return(TRUE)
          id <- pd$parent[row]
        }
        FALSE
      }
      # Item 3 on one parse; `where` maps token lines to locations; `forced`
      # is the context of an !expr value (item 4); `helper` applies item 5.
      check_code <- function(file, pd, where, forced = NULL, helper = FALSE) {
        tk <- pd[pd$terminal, , drop = FALSE]
        context_of <- context_finder(pd)
        ctx <- function(i) if (is.null(forced)) context_of(tk$id[i]) else rep(forced, length(i))
        lit <- function(i, text) paste(file, ctx(i), text, sep = "|") %in% nc$literals
        # Ruling G11: R decodes an escape inside a backtick-quoted name, so
        # the token text does not show the name it resolves to.
        esc <- which(tk$token %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL") &
                       grepl("\\", tk$text, fixed = TRUE))
        if (length(esc))
          add(file, where(tk$line1[esc]), "symbol-escape", ctx(esc), tk$text[esc], FALSE)
        for (i in which(tk$token == "SYMBOL_FUNCTION_CALL")) {
          fn <- unquote(tk$text[i])
          kind <- "call"
          ok <- helper || fn %in% nc$functions
          if (helper && fn %in% c("tar_read", "library", deny_helper)) {
            kind <- "call-helper"; ok <- FALSE
          } else if (helper && identical(fn, "source")) {
            if (!helper_source_ok(pd, tk$id[i], file)) { kind <- "call-source"; ok <- FALSE }
          } else if (!helper && fn %in% deny) {
            kind <- "call-denied"; ok <- FALSE
          } else if (!helper && identical(fn, "source")) {
            if (!source_ok(as_call(call_text(pd, tk$id[i])))) { kind <- "call-source"; ok <- FALSE }
          } else if (!helper && identical(fn, "tar_read")) {
            if (!tar_read_ok(as_call(call_text(pd, tk$id[i])))) { kind <- "call-tar_read"; ok <- FALSE }
          } else if (!helper && identical(fn, "kable")) {
            if (!kable_ok(call_text(pd, tk$id[i]))) { kind <- "call-kable"; ok <- FALSE }
          }
          add(file, where(tk$line1[i]), kind, ctx(i), fn, ok)
        }
        # Item 3 (a): a symbol named like a denied function, unless it is the
        # right operand of `$` or `@` (an element lookup). In the .qmd files
        # the readers count too (ruling G5).
        dn <- if (helper) deny_helper else c(deny, deny_value)
        for (i in which(tk$token == "SYMBOL" & unquote(tk$text) %in% dn)) {
          sib <- pd[pd$parent == tk$parent[i], , drop = FALSE]
          lookup <- any(sib$token %in% c("'$'", "'@'") &
                          (sib$line1 < tk$line1[i] |
                             (sib$line1 == tk$line1[i] & sib$col1 < tk$col1[i])))
          if (!lookup) add(file, where(tk$line1[i]), "symbol-denied", ctx(i), tk$text[i], FALSE)
        }
        nums <- which((tk$token == "NUM_CONST" & grepl(digit, tk$text, perl = TRUE)) |
                        tk$token %in% c("'+'", "'-'", "'*'", "'/'", "'^'") |
                        (tk$token == "SPECIAL" & !tk$text %in% c("%in%", "%>%")))
        if (length(nums))
          add(file, where(tk$line1[nums]),
              ifelse(tk$token[nums] == "NUM_CONST", "number", "operator"),
              ctx(nums), tk$text[nums], lit(nums, tk$text[nums]))
        for (i in which(tk$token == "STR_CONST")) {
          txt <- utils::getParseText(pd, tk$id[i])
          bare <- unquote(txt)
          if (grepl("^[rR][\"']", txt, perl = TRUE)) {
            # Ruling G10: a raw string constant keeps its delimiters in the
            # token text, so the quote strip cannot see a deny name in it.
            add(file, where(tk$line1[i]), "string-raw", ctx(i), txt, FALSE)
          } else if (bare %in% dn) {
            # A denied name as a string is declarable only as a column or
            # label name inside c(...) (item 3 (a)); a reader name never is.
            add(file, where(tk$line1[i]), "string-denied", ctx(i), txt,
                !helper && !bare %in% deny_value && identical(ctx(i), "c") && lit(i, txt))
          } else if (grepl(digit, txt, perl = TRUE)) {
            if (lit(i, txt)) add(file, where(tk$line1[i]), "string", ctx(i), txt, TRUE)
            else units(file, where(tk$line1[i]), "string-text", txt, FALSE)
          }
        }
        invisible(NULL)
      }
      # NULL when the text does not parse; an empty frame when it holds no code.
      parse_data <- function(text = NULL, file = NULL) {
        ex <- tryCatch(
          if (is.null(file)) parse(text = text, keep.source = TRUE, encoding = "UTF-8")
          else parse(file, keep.source = TRUE, encoding = "UTF-8"),
          error = function(e) NULL)
        if (is.null(ex)) return(NULL)
        pd <- utils::getParseData(ex)
        if (is.null(pd))
          pd <- data.frame(line1 = integer(0), col1 = integer(0), id = integer(0),
                           parent = integer(0), token = character(0), terminal = logical(0),
                           text = character(0), stringsAsFactors = FALSE)
        pd
      }

      census_qmd <- function(path, file) {
        L <- readLines(path, warn = FALSE, encoding = "UTF-8")
        at <- seq_along(L)
        line_no <- as.character(at)
        # R chunks (rule 7 item 2): from an R header to the next bare fence.
        opener <- grepl("^\\s*```+\\s*\\{r[ ,}]", L, perl = TRUE)
        closer <- grepl("^\\s*```+\\s*$", L, perl = TRUE)
        in_r <- logical(length(L))
        on <- FALSE
        for (i in at) {
          if (on) {
            in_r[i] <- TRUE
            if (closer[i]) on <- FALSE
          } else if (opener[i]) {
            in_r[i] <- TRUE
            on <- TRUE
          }
        }
        # Pre-scan (item 1, revision 1.2), never declarable: (i) chunk headers
        # other than an R header with at most a label or a raw latex block;
        # (ii) options that hide code from purl or substitute text (`eval` and
        # `engine` with any value, ruling G4); (iii) inline spans of other
        # engines.
        hdr <- grepl("^\\s*```+\\s*\\{", L, perl = TRUE)
        hdr_ok <- grepl("^\\s*```+\\s*\\{r( +[A-Za-z0-9_-]+)?\\}\\s*$", L, perl = TRUE) |
          grepl("^\\s*```+\\s*\\{=latex\\}\\s*$", L, perl = TRUE)
        add(file, line_no[hdr & !hdr_ok], "chunk-header", "prescan", L[hdr & !hdr_ok], FALSE)
        opt <- in_r & grepl("^\\s*#[|]", L, perl = TRUE)
        opt_name <- gsub("-", ".", sub("^\\s*#[|]\\s*([^:\\s]+).*$", "\\1", L, perl = TRUE),
                         fixed = TRUE)
        hidden <- opt & opt_name %in% c("purl", "code", "file", "ref.label", "child",
                                        "opts.label", "engine.path", "eval", "engine")
        add(file, line_no[hidden], "chunk-option", "prescan", L[hidden], FALSE)
        for (i in at[!in_r]) {
          eng <- regmatches(L[i], gregexpr("`[{][A-Za-z0-9_.-]+[}] ", L[i], perl = TRUE))[[1]]
          eng <- eng[eng != "`{r} "]
          add(file, rep(line_no[i], length(eng)), "inline-engine", "prescan", eng, FALSE)
        }
        # Inline R spans (item 2), parsed alone; the text unit keeps a space.
        span_re <- "`r +[^`]+`|`[{]r[}] +[^`]+`"
        text_L <- L
        for (i in at[!in_r & grepl(span_re, L, perl = TRUE)]) {
          for (sp in regmatches(L[i], gregexpr(span_re, L[i], perl = TRUE))[[1]]) {
            code <- sub("^`(r|[{]r[}]) +", "", sub("`$", "", sp, perl = TRUE), perl = TRUE)
            spd <- parse_data(text = code)
            if (is.null(spd)) add(file, line_no[i], "inline-parse", "inline", sp, FALSE)
            else check_code(file, spd, function(l) line_no[i])
          }
          text_L[i] <- gsub(span_re, " ", L[i], perl = TRUE)
        }
        # Ruling G3: an inline R opener left after the one-line spans are
        # removed (a span across a line break, or `r# ...) is never declarable.
        for (i in at[!in_r]) {
          op <- regmatches(text_L[i], gregexpr("`([{]r[}]|r)([ \t#]|$)", text_L[i], perl = TRUE))[[1]]
          add(file, rep(line_no[i], length(op)), "inline-open", "prescan", op, FALSE)
        }
        # !expr option values are code (item 3 (d), item 4), context `#|`.
        ex <- opt & grepl("^\\s*#[|]\\s*[^:]+:\\s*!expr\\b", L, perl = TRUE)
        for (i in at[ex]) {
          v <- sub("^\\s*#[|]\\s*[^:]+:\\s*!expr\\s*", "", L[i], perl = TRUE)
          spd <- parse_data(text = v)
          if (is.null(spd)) add(file, line_no[i], "expr-parse", "#|", L[i], FALSE)
          else check_code(file, spd, function(l) line_no[i], forced = "#|")
        }
        # Text units (item 4): every line outside R chunks and every other `#|`
        # line; a line equal to a whole-line literal of its file is declared.
        tl <- !in_r & nzchar(trimws(text_L))
        units(file, line_no[tl], "text", text_L[tl],
              paste(file, "text", trimws(L[tl]), sep = "|") %in% nc$literals)
        ol <- opt & !ex
        units(file, line_no[ol], "option", L[ol],
              paste(file, "#|", trimws(L[ol]), sep = "|") %in% nc$literals)
        # Chunk code through knitr::purl (item 1). knitr::purl evaluates an
        # !expr option value (U18: an undefined function made purl drop the
        # chunk), so those lines are left out of the copy; item 4 censused them.
        tmp <- tempfile(fileext = ".Rmd")
        out <- tempfile(fileext = ".R")
        writeLines(L[!ex], tmp, useBytes = TRUE)
        done <- tryCatch({
          knitr::purl(tmp, output = out, documentation = 1L, quiet = TRUE)
          TRUE
        }, error = function(e) FALSE)
        if (!done) return(add(file, "purl", "purl", "purl", file, FALSE))
        P <- readLines(out, warn = FALSE, encoding = "UTF-8")
        heads <- grep("^## ----", P)
        label <- trimws(sub(",.*$", "", sub("^## ----(.*?)-*$", "\\1", P[heads], perl = TRUE),
                            perl = TRUE))
        label[!nzchar(label)] <- "chunk"
        where <- function(l) {
          k <- findInterval(l, heads)
          ifelse(k > 0L, paste0(label[pmax(k, 1L)], " line ", l - heads[pmax(k, 1L)]),
                 paste0("line ", l))
        }
        pd <- parse_data(file = out)
        if (is.null(pd)) return(add(file, "purl", "parse", "purl", file, FALSE))
        check_code(file, pd, where)
      }

      census_helper <- function(path, file) {
        pd <- parse_data(file = path)
        if (is.null(pd)) return(add(file, "file", "parse", "helper", file, FALSE))
        check_code(file, pd, function(l) as.character(l), helper = TRUE)
        # Rule 3: format.R defines exactly the four formatter functions.
        if (identical(file, "sections/format.R")) {
          ex <- parse(path, keep.source = FALSE, encoding = "UTF-8")
          nm <- vapply(as.list(ex), function(e)
            if (is.call(e) && as.character(e[[1L]]) %in% c("<-", "=") && is.name(e[[2L]]))
              as.character(e[[2L]]) else "", "")
          add(file, "top level", "top-level", "<top>", paste(sort(nm), collapse = ", "),
              identical(sort(nm), sort(c("sig3", "format_units", "landscape", "include_page"))))
        }
      }

      files <- report_source_files[startsWith(report_source_files, paste0(rdir, "/"))]
      if (!any(grepl("[.]qmd$", files, perl = TRUE))) stop("no .qmd report source under '", rdir, "'", call. = FALSE)
      rel <- sub("^/", "", sub(rdir, "", files, fixed = TRUE), perl = TRUE)
      for (k in seq_along(files)) {
        if (grepl("[.]qmd$", rel[k], perl = TRUE)) census_qmd(files[k], rel[k])
        else if (rel[k] %in% c("sections/format.R", "sections/symbol-map.R"))
          census_helper(files[k], rel[k])
      }
      units("<declaration>", names(texts), "declared-text", texts, rep(FALSE, length(texts)))

      rows <- do.call(rbind, acc$rows)
      text_kinds <- c("text", "option", "string-text", "declared-text")
      censused <- unique(rows$file)
      summary <- data.frame(
        file = censused,
        tokens = vapply(censused, function(f) sum(rows$file == f & !rows$kind %in% text_kinds), 0L),
        calls = vapply(censused, function(f) sum(rows$file == f & startsWith(rows$kind, "call")), 0L),
        text_units = vapply(censused, function(f) sum(rows$file == f & rows$kind %in% text_kinds), 0L),
        undeclared = vapply(censused, function(f) sum(rows$file == f & rows$status == "undeclared"), 0L),
        row.names = NULL, stringsAsFactors = FALSE)
      declared <- data.frame(class = rep(names(nc), lengths(nc)), entry = unname(unlist(nc)),
                             stringsAsFactors = FALSE)
      bad <- rows[rows$status == "undeclared", , drop = FALSE]
      if (nrow(bad))
        stop("reporting: number census: typed numbers, calls or operators the declaration ",
             "does not cover (file:where:kind:context:shape, digits masked):\n",
             paste(bad$file, bad$where, bad$kind, bad$context, bad$shape, sep = ":",
                   collapse = "\n"), call. = FALSE)
      list(rows = rows, summary = summary, declared = declared)
    })
}
