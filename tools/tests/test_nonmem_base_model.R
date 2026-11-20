#!/usr/bin/env Rscript
# Synthetic NONMEM, qualification, simulation, reporting and review checks.
# Generated data and disposable fixture projects exercise public task declarations,
# independent local method approval, package provenance, and fail-closed behavior.
# Runtime tests use inert NONMEM and PsN executables. Simulation tests compile only
# the generated model. The release does not contain any study data or decisions.

.invoked_as_script <- function() {
  fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  length(fa) > 0L && grepl("test_nonmem_base_model\\.R$", sub("^--file=", "", fa[1]))
}

# Public audit and runner tests use generic receipts and declared tasks.
.ac14_review_cases <- function(TOOLS) {
  e <- new.env(parent = globalenv())
  sys.source(file.path(TOOLS, "review_stage.R"), e)
  snapshot <- function() list(
    dataset = data.frame(engineering = "control"),
    environment_attestation = list(
      fingerprint = paste0("sha256:", strrep("a", 64L)),
      probe = list(environment_confirmed = TRUE,
                   r_packages = list(required = list(targets = "fixture-version")),
                   executables = list(pdftotext = list(path = "fake", version = "fixture-version")))))
  compare <- function(a, b) e$rv_compare_stores(a, b, list(fixed = character(), patterns = character()))
  attestation_rows <- function(result) {
    rows <- result$table[startsWith(result$table$item, "environment_attestation: "), , drop = FALSE]
    stopifnot(identical(rows$item, paste0("environment_attestation: ", c("fingerprint", "probe"))))
    rows
  }
  cases <- list(
    "A-audit attestation agrees" = function() {
      r <- compare(snapshot(), snapshot())
      stopifnot(all(attestation_rows(r)$match), nrow(r$outcomes) == 0L)
    },
    "A-audit-mismatch whole-attestation" = function() {
      a <- snapshot()
      b <- a
      b$environment_attestation$probe$executables$pdftotext$version <- "changed"
      r <- compare(a, b)
      stopifnot(identical(attestation_rows(r)$match, c(TRUE, FALSE)),
                identical(r$outcomes$rule, "attestation"))
      b <- a
      b$environment_attestation$fingerprint <- paste0("sha256:", strrep("b", 64L))
      r <- compare(a, b)
      stopifnot(identical(attestation_rows(r)$match, c(FALSE, TRUE)),
                identical(r$outcomes$rule, "attestation"))
      b <- a
      b$environment_attestation$probe$environment_confirmed <- FALSE
      r <- compare(a, b)
      stopifnot(identical(attestation_rows(r)$match, c(TRUE, FALSE)),
                identical(r$outcomes$rule, "attestation"))
      b <- a
      b$environment_attestation$probe$r_packages$required$targets <- "changed"
      r <- compare(a, b)
      stopifnot(identical(attestation_rows(r)$match, c(TRUE, FALSE)),
                identical(r$outcomes$rule, "attestation"))
    },
    "A-audit attestation absent or unreadable" = function() {
      a <- snapshot()
      for (field in c("fingerprint", "probe")) {
        b <- a
        b$environment_attestation[[field]] <- NULL
        r <- compare(a, b)
        stopifnot(identical(attestation_rows(r)$match, c("fingerprint", "probe") != field),
                  identical(r$outcomes$rule, "attestation"))
      }
      b <- a
      b["environment_attestation"] <- list(NULL)
      attr(b, "read_errors") <- c(environment_attestation = "fixture receipt unreadable")
      r <- compare(a, b)
      stopifnot(identical(attestation_rows(r)$match, c(FALSE, FALSE)),
                identical(r$outcomes$rule, rep("attestation", 2L)),
                all(grepl("fresh: fixture receipt unreadable", attestation_rows(r)$detail, fixed = TRUE)))
      a$environment_attestation <- NULL
      r <- compare(a, a)
      stopifnot(identical(attestation_rows(r)$match, c(FALSE, FALSE)),
                identical(r$outcomes$rule, rep("attestation", 2L)),
                all(grepl("analysis: absent or NULL; fresh: absent or NULL", attestation_rows(r)$detail, fixed = TRUE)))
    },
    "A-audit declared translation evidence" = function() {
      audit <- function(table = NULL, tasks = "simulation") {
        packages <- list(simulation_package = list(tables = list("Translation equivalence check" = table)))
        e$rv_validation_audit(data.frame(name = character(), command = character()),
                              tasks, character(), packages, character(), root = tempdir())
      }
      good <- data.frame(max_rel_err = 0.25, tolerance = 0.25)
      bad <- data.frame(max_rel_err = 0.5, tolerance = 0.25)
      stopifnot(nrow(audit(good)$outcomes) == 0L, nrow(audit(tasks = "reporting")$outcomes) == 0L)
      for (table in list(NULL, good["tolerance"], bad)) {
        o <- audit(table)$outcomes
        stopifnot(identical(o$rule, "validation"), identical(o$checklist_item, "domain 4 bullet 3"),
                  identical(o$evidence_path, "simulation_package"),
                  grepl("absent or outside its tolerance", o$finding, fixed = TRUE))
      }
    }
  )
  for (explicit in c(FALSE, TRUE)) local({
    ex <- explicit
    cases[[paste("A-runner", if (ex) "explicit" else "omitted")]] <<- function() {
      base <- tempfile("review-runner-"); dir.create(base)
      old <- getwd(); on.exit(setwd(old)); setwd(base)
      dir.create("modules"); dir.create("derived")
      tasks <- c("data_assembly", "reporting", "review")
      for (task in tasks) writeLines(
        sprintf('task_targets <- list(targets::tar_target_raw("synthetic_%s", quote(identity("engineering"))))', task),
        file.path("modules", paste0(task, ".R")))
      inputs <- list(reporting = list(report_dir = "report", store = "reports", title = "Fixture report"))
      call <- call("compose_pipeline", analysis_id = "SYNTHETIC-RUNNER", tasks = tasks, inputs = inputs)
      if (ex) call$attest_tasks <- tasks
      writeLines(deparse(call), "original.R")
      decl <- e$rv_read_declaration("original.R")
      der <- e$rv_derive_runner(decl, "derived")
      e$rv_write_runner(decl, der, "derived", "synthetic-epoch")
      reread <- e$rv_read_declaration("derived/runner.R")
      cc <- reread$compose_call
      stopifnot(identical(reread$attest_tasks, tasks),
                identical(reread$inputs$reporting,
                          list(report_dir = "derived/report", store = "derived/store", title = "Fixture report")))
      ce <- new.env(parent = globalenv())
      sys.source(file.path(TOOLS, "..", "pipeline", "compose.R"), ce)
      sys.source(file.path(TOOLS, "check_environment.R"), ce)
      ce$TASK_DIR <- "modules"
      ce$resolve_nonmem_provision <- function(...) "containerized"
      # Inspect the real composed command without executing the runner or any stage.
      full <- eval(cc, ce)[[1L]]$command$expr[[1L]]
      original <- eval(decl$compose_call, ce)[[1L]]$command$expr[[1L]]
      cc$tasks <- character()
      reduced <- eval(cc, ce)[[1L]]$command$expr[[1L]]
      stopifnot(identical(full, original), identical(full, reduced),
                identical(full[[1L]], as.name("attest_environment")),
                identical(full$pkgs, c("targets", "yaml", "digest", "yspec")),
                isTRUE(full$require_review), isFALSE(full$require_nonmem), isFALSE(full$require_psn))
      # An unconfirmed working directory must stop before any executable probe.
      err <- tryCatch({eval(full, ce); NULL}, error = function(e) conditionMessage(e))
      stopifnot(length(err) == 1L, grepl("No pipeline runs before the SME confirms the environment", err, fixed = TRUE))
    }
  })
  cases
}

.r15_fixture_definitions <- function(RX) {
  r_write <- function (rel, lines, root = RX)
{
    p <- file.path(root, rel)
    dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
    writeLines(lines, p, useBytes = TRUE)
    invisible(p)
}
  fence <- "```"
  chunk <- function (label, ...)
c(paste0(fence, "{r ", label, "}"), ..., fence, "")
  sections <- c("01-executive-summary.qmd", "02-introduction.qmd", "03-objectives.qmd",
"04-data-and-methods.qmd", "05-results.qmd", "06-discussion.qmd",
"07-conclusions.qmd", "08-appendices.qmd")
  literals <- c("mar.qmd|text|{{< include sections/01-executive-summary.qmd >}}",
"mar.qmd|text|{{< include sections/02-introduction.qmd >}}",
"mar.qmd|text|{{< include sections/03-objectives.qmd >}}", "mar.qmd|text|{{< include sections/04-data-and-methods.qmd >}}",
"mar.qmd|text|{{< include sections/05-results.qmd >}}", "mar.qmd|text|{{< include sections/06-discussion.qmd >}}",
"mar.qmd|text|{{< include sections/07-conclusions.qmd >}}", "mar.qmd|text|{{< include sections/08-appendices.qmd >}}",
"sections/05-results.qmd|include_page|2", "sections/05-results.qmd|#||#| fig-width: 7",
"sections/format.R|signif|3", "sections/format.R|formatC|3")
  helpers <- list(`sections/symbol-map.R` = c("normalize_symbols <- function(text) gsub(\"theta_fix\", \"theta-fix\", text, fixed = TRUE)",
"tool_files <- c(\"tools/other_tool.R\")", "source(\"tools/report_typography.R\", local = TRUE)"
), `sections/format.R` = c("sig3 <- function(x) formatC(signif(x, 3), format = \"fg\", digits = 3, flag = \"#\")",
"format_units <- function(u) u", "landscape <- function(tab) tab",
"include_page <- function(path, page, caption, label) {", "  cat(\"\\n\\\\begin{figure}[h]\\n\\\\centering\\n\\\\includegraphics[page=\", page,",
"      \",width=\\\\linewidth]{\", normalizePath(path), \"}\\n\\\\caption{\", caption,",
"      \"}\\\\label{fig:\", label, \"}\\n\\\\end{figure}\\n\\n\", sep = \"\")",
"}"))
  list(r_write = r_write, chunk = chunk, R_SECTIONS = sections,
       R_CENSUS = list(literals = literals), R_SRC = helpers)
}


.r15_collect <- function(cases) {
  cat("R15 selected cases: ", paste(names(cases), collapse = ", "), "\n", sep = "")
  errors <- vapply(names(cases), function(id) {
    error <- tryCatch({ cases[[id]](); "" }, error = conditionMessage)
    cat(if (nzchar(error)) paste0("FAIL R15 (", id, "): ", error, "\n")
        else paste0("PASS R15 (", id, ")\n"))
    error
  }, "")
  names(errors) <- names(cases)
  if (any(nzchar(errors)))
    stop(paste0("R15 (", names(errors)[nzchar(errors)], "): ",
                errors[nzchar(errors)], collapse = "\n"), call. = FALSE)
  invisible(TRUE)
}

.r15_cases <- function(fixture) {
  r_census <- fixture$r_census
  RX <- fixture$RX
  R_SRC_REL <- fixture$R_SRC_REL
  R_CENSUS <- fixture$R_CENSUS
  r15 <- fixture$r15
  r_chunk_with <- fixture$r_chunk_with
  r_append <- fixture$r_append
  r15_expect <- fixture$r15_expect
  rblock <- fixture$rblock
  rmodule <- fixture$rmodule
  r05 <- "sections/05-results.qmd"
  r01 <- "sections/01-executive-summary.qmd"
  cases <- list(
    clean = function() {
        res <- r_census(RX)
        stopifnot(identical(res, fixture$direct_census()),
                  identical(names(res), c("rows", "summary", "declared")),
                  !any(res$rows$status == "undeclared"),
                  identical(names(res$summary), c("file", "tokens", "calls", "text_units", "undeclared")),
                  all(res$summary$undeclared == 0L),
                  all(c(R_SRC_REL, "<declaration>") %in% res$summary$file),
                  identical(nrow(res$declared), length(unlist(R_CENSUS))),
                  identical(names(res$declared), c("class", "entry")))
    },
    a = function() {
        r15_expect(r15("a", r_chunk_with(r05, "#| fig-cap: \"Concentration over the first 336 h\"", "x <- TRUE")),
                   r05, "option")
    },
    b = function() {
        msg <- r15("b", r_append(r01, "Inline `r 17`."))
        r15_expect(msg, r01, "number")
        if (!grepl(":number:<top>:dd(\n|$)", msg)) stop("R15 (b): the shape is not masked: ", msg)
    },
    c = function() {
        r15_expect(r15("c", r_append(r01, "Inline `r nrow(report_x)`.")), r01, "call-denied")
    },
    d = function() {
        r15_expect(r15("d", r_chunk_with(r05, "#| echo: false", "cat(\"\\\\caption{Exposure over 336 h}\")")),
                   r05, "string-text")
    },
    e = function() {
        r15_expect(r15("e", function(s) { s[["mar.qmd"]] <- append(s[["mar.qmd"]], "subtitle: \"Day 3\"", 2); s }),
                   "mar.qmd", "text")
    },
    f = function() {
        doc <- rblock()$document
        doc[["author"]] <- "Fixture author naming 12 subjects."
        tt <- rmodule(rblock(document = doc), all = TRUE)
        msg <- tryCatch({ r_census(RX, tt); NULL }, error = conditionMessage)
        if (is.null(msg)) stop("R15 (f): the census did not stop")
        r15_expect(msg, "<declaration>", "declared-text")
    },
    g = function() {
        r15_expect(r15("g", r_append("sections/06-discussion.qmd", "", "```{=latex}", "\\vspace{2mm}", "```")),
                   "sections/06-discussion.qmd", "text")
    },
    h = function() {
        r15_expect(r15("h", r_chunk_with(r05, "#| echo: false", "#| results: asis",
                                         "include_page(fig$path, 3, \"Other page.\", \"other\")")),
                   r05, "number")
    },
    i = function() {
        r15_expect(r15("i", r_chunk_with(r05, "x <- 1.5", "y <- x * 2")), r05, "operator")
    },
    j = function() {
        r15_expect(r15("j", r_append("sections/format.R", "extra_helper <- function(x) x")),
                   "sections/format.R", "top-level")
    },
    k = function() {
        r15_expect(r15("k", r_append(r05, "", "```{r, fig.cap=\"Concentration over 336 h\"}", "x <- TRUE", "```")),
                   r05, "chunk-header")
    },
    l = function() {
        r15_expect(r15("l", r_chunk_with(r05, "#| fig-cap: !expr paste(nrow(report_x), \"subjects\")", "x <- TRUE")),
                   r05, "call-denied")
    },
    m = function() {
        r15_expect(r15("m", r_chunk_with(r05, "#| purl: false", "x <- TRUE")), r05, "chunk-option")
    },
    n = function() {
        r15_expect(r15("n", r_chunk_with(r05, "y <- vapply(x, nrow, 1L)")), r05, "symbol-denied")
    },
    o = function() {
        r15_expect(r15("o", r_chunk_with(r05, "source(\"sections/other.R\")")), r05, "call-source")
    },
    p = function() {
        r15_expect(r15("p", r_chunk_with(r05, "q <- tar_read(qual_bootstrap, store = params$store)")),
                   r05, "call-tar_read")
    },
    q = function() {
        r15_expect(r15("q", r_chunk_with(r05, "knitr::kable(x, digits = 3)")), r05, "call-kable")
    },
    A3 = function() {
        r15_expect(r15("A3", r_chunk_with(r05, "source(here::here(\"tools\", \"other.R\"))")),
                   r05, "call-source")
    },
    A5 = function() {
        r15_expect(r15("A5", function(s) {
          s[["sections/symbol-map.R"]][3] <-
            "if (file.exists(\"tools/other.R\")) source(\"tools/report_typography.R\", local = TRUE)"
          s }), "sections/symbol-map.R", "call-source")
    },
    sentinel = function() invisible(TRUE))
        g9 <- list(
          G1 = function() r15_expect(r15("G1", r_chunk_with(r05, "y <- list(`nrow`)")),
                                     r05, "symbol-denied"),
          G2 = function() r15_expect(r15("G2", r_append("sections/03-objectives.qmd",
                                                        "Fixture objective number \uff11 of the report.")),
                                     "sections/03-objectives.qmd", "text"),
          G3 = function() r15_expect(r15("G3", r_append(r01, "Inline `r nrow(", "report_x)` here.")),
                                     r01, "inline-open"),
          G4 = function() r15_expect(r15("G4", r_chunk_with(r05, "#| eval: false", "x <- nrow(report_x)")),
                                     r05, "chunk-option"),
          G5 = function() r15_expect(r15("G5", r_chunk_with(r05, "q <- list(tar_read)")),
                                     r05, "symbol-denied"),
          r = function() r15_expect(r15("r", r_chunk_with(r05, "y <- undeclared_fn(TRUE)")), r05, "call"),
          s = function() r15_expect(r15("s", r_chunk_with(r05, "y <- list(\"length\")")),
                                    r05, "string-denied"),
          # Ruling G10: a raw string constant hides a deny name from the quote strip.
          G10 = function() r15_expect(r15("G10", r_chunk_with(r05, "y <- list(r\"(length)\")")),
                                      r05, "string-raw"),
          # Ruling G11: an escape inside a backtick-quoted name resolves to a
          # deny name the token text does not show.
          G11 = function() r15_expect(r15("G11", r_chunk_with(r05, "y <- list(`len\\x67th`)")),
                                      r05, "symbol-escape"),
          # Ruling G12: a file reader passed as a value in a .qmd file.
          G12 = function() r15_expect(r15("G12", r_chunk_with(r05, "q <- list(readRDS)")),
                                      r05, "symbol-denied"))
  row <- function(msg, file, kind, context, shape) {
    r15_expect(msg, file, kind)
    suffix <- paste0(":", kind, ":", context, ":", shape)
    if (!any(endsWith(strsplit(msg, "\n", fixed = TRUE)[[1L]], suffix)))
      stop("R15 diagnostic context or masked shape differs: ", msg)
  }
  extra <- list(
    reference_explicit = function() {
      text <- c("Section 7", "Section 7.2", "Section 7, Section 8",
                "Section 7 and Section 8", "Section 7 to Section 8")
      for (i in seq_along(text)) {
        d <- file.path(fixture$FX, "r15", paste0("reference-explicit-", i))
        fixture$r_sources(d, src = r_append("sections/03-objectives.qmd", text[i])(fixture$R_SRC))
        res <- r_census(d)
        stopifnot(!any(res$rows$status == "undeclared"))
      }
    },
    reference_continuation = function() {
      text <- c("Section 7, 937", "Section 7.2, 937",
                "Section 7 and 937", "Section 7 to 937")
      for (i in seq_along(text))
        r15_expect(r15(paste0("reference-continuation-", i),
                       r_append("sections/03-objectives.qmd", text[i])), "sections/03-objectives.qmd", "text")
    },
    reader_declaration_bare = function() {
      readers <- c("readRDS", "load", "readLines", "scan", "read.csv", "read.table", "read.delim")
      for (reader in readers) {
        nc <- R_CENSUS
        nc$functions <- c(nc$functions, reader)
        block <- fixture$rblock(number_census = nc)
        msg <- tryCatch({ fixture$rmodule(block, all = TRUE); NULL }, error = conditionMessage)
        if (is.null(msg))
          stop("reader_declaration_bare: missing prepare-time refusal for ", reader)
        stopifnot(identical(msg, paste0(
          "reporting: number_census: functions: on the deny list: ", reader)))
      }
    },
    reader_declaration_quotes = function() {
      values <- c("'readRDS'", "\"readLines\"", "`read.csv`")
      expected <- c(
        "reporting: number_census: functions: on the deny list: readRDS",
        "reporting: number_census: functions: on the deny list: readLines",
        "reporting: number_census: functions: on the deny list: read.csv")
      for (k in seq_along(values)) {
        nc <- R_CENSUS
        nc$functions <- c(nc$functions, values[k])
        block <- fixture$rblock(number_census = nc)
        msg <- tryCatch({ fixture$rmodule(block, all = TRUE); NULL }, error = conditionMessage)
        if (is.null(msg))
          stop("reader_declaration_quotes: missing prepare-time refusal for ", values[k])
        stopifnot(identical(msg, expected[k]))
      }
    },
    reader_declaration_mixed = function() {
      nc <- R_CENSUS
      nc$functions <- c(nc$functions, "toupper", "read.table", "scan", "'read.table'", "load")
      stopifnot(!anyDuplicated(nc$functions))
      block <- fixture$rblock(number_census = nc)
      msg <- tryCatch({ fixture$rmodule(block, all = TRUE); NULL }, error = conditionMessage)
      if (is.null(msg)) stop("reader_declaration_mixed: missing prepare-time refusal")
      stopifnot(identical(msg,
        "reporting: number_census: functions: on the deny list: read.table, scan, load"))
    },
    reader_source_control = function() {
      nc <- R_CENSUS
      stopifnot(all(c("source", "tar_read", "paste") %in% nc$functions),
                !anyDuplicated(nc$functions),
                "source(file.path(\"rpt\", \"sections\", \"format.R\"))" %in%
                  fixture$R_SRC[["mar.qmd"]],
                "source(here::here(\"tools\", \"evidence_notes.R\"))" %in%
                  fixture$R_SRC[["mar.qmd"]])
      tt <- fixture$rmodule(fixture$rblock(number_census = nc), all = TRUE)
      res <- r_census(RX, tt)
      calls <- res$rows[res$rows$kind == "call", , drop = FALSE]
      for (name in c("source", "tar_read")) {
        relevant <- calls[calls$file == "mar.qmd" & calls$shape == name, , drop = FALSE]
        stopifnot(nrow(relevant) > 0L, all(relevant$status == "declared"))
      }
      # Each source form must emit its own call row at the nested path call's location.
      for (form in c("file.path", "here")) {
        paths <- calls[calls$file == "mar.qmd" & calls$shape == form, , drop = FALSE]
        stopifnot(nrow(paths) > 0L, all(paths$status == "declared"))
        for (where in paths$where) {
          relevant <- calls[calls$file == "mar.qmd" & calls$where == where &
                              calls$shape == "source", , drop = FALSE]
          stopifnot(nrow(relevant) > 0L, all(relevant$status == "declared"))
        }
      }
      stopifnot(!any(res$rows$status == "undeclared"))
    },
    expr_option_nonexecution = function() {
      sentinel <- file.path(fixture$FX, "r15", "expr-option", "r15-expression-option-sentinel")
      stopifnot(!file.exists(sentinel))
      row(r15("expr-option", r_chunk_with(r05,
        "#| fig-cap: !expr file.create(\"r15-expression-option-sentinel\")", "x <- TRUE")),
        r05, "call", "#|", "file.create")
      stopifnot(!file.exists(sentinel))
    },
    helper_reader_call = function() row(
      r15("helper-reader-call", r_append("sections/symbol-map.R", "x <- readRDS(path)")),
      "sections/symbol-map.R", "call-helper", "readRDS", "readRDS"),
    helper_reader_value = function() row(
      r15("helper-reader-value", r_append("sections/symbol-map.R", "x <- list(readRDS)")),
      "sections/symbol-map.R", "symbol-denied", "list", "readRDS"),
    reader_string = function() {
      nc <- R_CENSUS
      nc$literals <- c(nc$literals, paste0(r05, "|c|\"readRDS\""),
                       paste0(r05, "|c|\"length\""))
      tt <- rmodule(rblock(number_census = nc), all = TRUE)
      row(r15("reader-string", r_chunk_with(r05, "x <- c(\"length\", \"readRDS\")"), tt),
          r05, "string-denied", "c", "\"readRDS\"")
    },
    ordinary_denied_string = function() {
      nc <- R_CENSUS
      nc$literals <- c(nc$literals, paste0(r05, "|c|\"length\""))
      d <- file.path(fixture$FX, "r15", "ordinary-string")
      fixture$r_sources(d, src = r_chunk_with(r05, "x <- c(\"length\")")(fixture$R_SRC))
      r_census(d, rmodule(rblock(number_census = nc), all = TRUE))
    },
    source_lowercase = function() row(r15("source-lowercase", r_append(
      "sections/symbol-map.R",
      "if (file.exists(\"tools/other.r\")) source(\"tools/report_typography.R\", local = TRUE)")),
      "sections/symbol-map.R", "call-source", "source", "source"),
    source_uppercase = function() row(r15("source-uppercase", r_append(
      "sections/symbol-map.R",
      "if (file.exists(\"tools/other.R\")) source(\"tools/report_typography.R\", local = TRUE)")),
      "sections/symbol-map.R", "call-source", "source", "source"),
    typography_source_control = function() r_census(RX),
    unicode_string = function() row(r15("unicode-string", r_chunk_with(r05, "x <- \"\u2163\"")),
      r05, "string-text", "string-text", "\"d\""),
    backtick_call = function() row(r15("backtick-call", r_chunk_with(r05, "x <- `nrow`(report_x)")),
      r05, "call-denied", "`nrow`", "nrow"),
    backtick_declaration = function() {
      nc <- R_CENSUS
      nc$functions <- c(nc$functions, "`nrow`")
      msg <- tryCatch({ rmodule(rblock(number_census = nc), all = TRUE); NULL },
                      error = conditionMessage)
      stopifnot(identical(msg, "reporting: number_census: functions: on the deny list: nrow"))
    })
  c(cases[names(cases) != "sentinel"], g9, extra)
}

.r15_setup <- function(REPO, FX) {
  REPO <- normalizePath(REPO, mustWork = TRUE)
  parent <- normalizePath(dirname(FX), mustWork = TRUE)
  if (!identical(parent, dirname(FX)) || !startsWith(FX, "/") ||
      startsWith(FX, paste0(REPO, "/")) || startsWith(REPO, paste0(FX, "/")) ||
      identical(FX, REPO)) stop("R15 fixture destination overlaps sources or traverses a symlink")
  if (file.exists(FX)) stop("R15 fixture destination already exists")
  dir.create(FX, recursive = TRUE)
  RX <- file.path(FX, "fixture")
  dir.create(RX)
  input_census_path <- file.path(FX, "input-census.rds")
  stopifnot(!file.exists(input_census_path))
  input_censuses <- list()
  saveRDS(input_censuses, input_census_path)
  record_inputs <- function(label) {
    stopifnot(!nzchar(Sys.readlink(RX)),
              identical(normalizePath(RX, mustWork = TRUE), RX))
    pending <- RX
    paths <- character()
    while (length(pending)) {
      entries <- list.files(pending[1L], all.files = TRUE, full.names = TRUE,
                            no.. = TRUE)
      pending <- pending[-1L]
      stopifnot(!any(nzchar(Sys.readlink(entries))))
      info <- file.info(entries)
      stopifnot(!anyNA(info$isdir))
      pending <- c(pending, entries[info$isdir])
      paths <- c(paths, entries[!info$isdir])
    }
    paths <- sort(paths)
    stopifnot(all(startsWith(normalizePath(paths, mustWork = TRUE),
                             paste0(RX, "/"))))
    hashes <- tools::md5sum(paths)
    stopifnot(!anyNA(hashes))
    names(hashes) <- substring(paths, nchar(RX) + 2L)
    input_censuses[[label]] <<- hashes
    saveRDS(input_censuses, input_census_path)
    invisible(hashes)
  }
  defs <- .r15_fixture_definitions(RX)
  chunk <- defs$chunk
  sections <- defs$R_SECTIONS
  helpers <- c("sections/symbol-map.R", "sections/format.R")
  R_SRC_REL <- c("mar.qmd", "deck.qmd", file.path("sections", sections), helpers)
  r_source_files <- function()
    c(file.path("rpt", R_SRC_REL), "tools/evidence_notes.R", "tools/report_typography.R")
  R_CENSUS <- list(
    patterns = c("\\b20[0-9]{2}-[0-9]{2}-[0-9]{2}\\b", "\\bSection [0-9]+(\\.[0-9]+)*",
                 "\\bversion [0-9]+(\\.[0-9]+)*"),
    phrases = "30 mg/kg",
    literals = defs$R_CENSUS$literals,
    functions = c("source", "file.path", "here", "tar_read", "library",
                  "include_page", "cat", "paste", "c", "list", "kable"))
  R_SRC <- as.list(stats::setNames(paste("# Fixture", letters[seq_along(sections)]),
                                  file.path("sections", sections)))
  R_SRC[["mar.qmd"]] <- c(
    "---", 'title: "Fixture report"', "format: pdf", "---", "",
    chunk("setup", "#| include: false", "library(targets)",
          'source(file.path("rpt", "sections", "format.R"))',
          'source(here::here("tools", "evidence_notes.R"))',
          "packages <- tar_read(report_stage_packages, store = params$store)"),
    sprintf("{{< include sections/%s >}}", sections))
  R_SRC[["deck.qmd"]] <- c("---", 'title: "Fixture deck"', "format: pptx", "---", "", "# Slide")
  R_SRC[["sections/02-introduction.qmd"]] <- c(
    "# Introduction", "", "Fixture decision dated 2026-08-30.")
  R_SRC[["sections/05-results.qmd"]] <- c(
    "# Results", "",
    chunk("figure-options", "#| include: false", "#| echo: !expr c(FALSE)", "#| fig-width: 7",
          '#| fig-cap: "Fixture dose of 30 mg/kg"', "x <- TRUE"),
    chunk("figure-page", "#| echo: false", "#| results: asis",
          'include_page(fig$path, 2, "Fixture page of the figure file.", "fixture-page")'))
  R_SRC[helpers] <- defs$R_SRC[helpers]
  r_sources <- function(root, src = R_SRC)
    for (rel in names(src)) defs$r_write(file.path("rpt", rel), src[[rel]], root = root)
  rblock <- function(number_census = R_CENSUS,
                     document = c(author = "Fixture author", version = "version 1.0",
                                  date = "2026-10-04"))
    list(report_dir = "rpt", store = "_targets_fixture", document = document,
         packages = "simulation", helpers = helpers, number_census = number_census)

  files <- c("pipeline/compose.R", "pipeline/tasks/reporting.R", "tools/evidence_notes.R",
             "tools/report_typography.R", "tools/review_stage.R", "tools/report_number_census.R")
  for (file in files) {
    from <- file.path(REPO, file)
    if (!startsWith(normalizePath(from, mustWork = TRUE), paste0(REPO, "/")) ||
        nzchar(Sys.readlink(from))) stop("R15 source traverses a symlink")
    to <- file.path(RX, file)
    dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
    stopifnot(file.copy(from, to, overwrite = FALSE))
  }
  record_inputs("copied-before-source")
  renv <- new.env(parent = globalenv())
  for (file in setdiff(files, "pipeline/tasks/reporting.R"))
    sys.source(file.path(RX, file), envir = renv)
  r_sources(RX)
  record_inputs("generated-after-writes")
  RMODULE <- file.path(RX, "pipeline/tasks/reporting.R")
  rmodule <- function(ti, all = FALSE) {
    me <- new.env(parent = renv)
    me$task_input <- ti
    me$analysis_id <- "FIXTURE"
    me$upstream_tasks <- "simulation"
    me$upstream_gate <- "gate_simulation"
    me$task_targets <- NULL
    old <- setwd(RX)
    on.exit(setwd(old))
    sys.source(RMODULE, envir = me)
    if (all) return(me$task_targets)
    vapply(me$task_targets, function(t) t$settings$name, character(1))
  }
  r_targets <- NULL
  rtargets <- function() {
    if (is.null(r_targets)) r_targets <<- rmodule(rblock(), all = TRUE)
    r_targets
  }
  rcmd <- function(name, tt = rtargets())
    tt[[which(vapply(tt, function(x) x$settings$name, "") == name)]]$command$expr[[1]]
  eval_census <- function(cmd, dir, source_files) {
    old <- setwd(dir)
    on.exit(setwd(old))
    eval(cmd, list(report_source_files = source_files), enclos = renv)
  }
  r_census <- function(dir, tt = rtargets()) {
    sources <- eval(rcmd("report_source_files", tt))
    eval_census(rcmd("report_number_census", tt), dir, sources)
  }
  direct_census <- function() {
    sources <- r_source_files()
    ti <- rblock()
    prepared <- renv$report_number_census_prepare(ti$number_census, ti$report_dir, sources)
    cmd <- renv$report_number_census_command(ti$report_dir, prepared, "report_stage_packages",
                                            ti$document)
    eval_census(cmd, RX, sources)
  }
  # One R15 mutation: a copy of the fixture sources with `edit` applied (a
  # function of the source list), then the census; returns the stop message.
  r15 <- function(case, edit, tt = rtargets()) {
    d <- file.path(FX, "r15", case)
    r_sources(d, src = edit(R_SRC))
    msg <- tryCatch({ r_census(d, tt); NULL }, error = function(e) conditionMessage(e))
    if (is.null(msg)) stop("R15 ", case, ": the census did not stop")
    msg
  }
  r_append <- function(rel, ...) function(s) { s[[rel]] <- c(s[[rel]], ...); s }
  r_chunk_with <- function(rel, ...) r_append(rel, "", chunk("mutation", ...))
  # Expects a census stop naming `file` and `kind`, with masked shapes.
  r15_expect <- function(msg, file, kind) {
    if (!grepl(paste0("(^|\n)", file, ":[^\n]*:", kind, ":"), msg, perl = TRUE))
      stop("census message lacks ", file, " ", kind, ": ", msg)
    invisible(TRUE)
  }
  environment()
}

.r15_wiring <- function(fixture) {
  tt <- fixture$rtargets()
  names <- vapply(tt, function(t) t$settings$name, "")
  target <- tt[[match("report_number_census", names)]]
  wrapped <- fixture$renv$wrap_command_with_gate(target, "gate_simulation")
  stopifnot(sum(names == "report_number_census") == 1L,
            "gate_simulation" %in% all.names(wrapped$command$expr[[1L]]),
            "report_source_files" %in% all.names(fixture$rcmd("report_number_census", tt)))
  actual_sources <- eval(fixture$rcmd("report_source_files", tt))
  helper <- "tools/report_number_census.R"
  stopifnot(identical(actual_sources, fixture$r_source_files()),
            !"report_number_census" %in% fixture$rmodule(fixture$rblock(number_census = NULL)),
            !"report_number_census" %in% all.names(fixture$rcmd("reporting_package",
              fixture$rmodule(fixture$rblock(number_census = NULL), all = TRUE))),
            "report_number_census" %in% all.names(fixture$rcmd("reporting_package", tt)),
            "report_source_files" %in% all.names(fixture$rcmd("report_code_inventory", tt)),
            !helper %in% actual_sources)
  # The private module tracked the census implementation as a separate file
  # target. Preserve its wiring assertions for target sets that expose it.
  file_id <- "report_number_census_helper_file"
  if (file_id %in% names) {
    file_target <- tt[[match(file_id, names)]]
    cmd <- file_target$command$expr[[1L]]
    stopifnot(identical(file_target$settings$format, "file"),
              "gate_simulation" %in% all.names(cmd),
              file_id %in% all.names(fixture$rcmd("report_code_inventory", tt)),
              identical(eval(cmd, list(gate_simulation = NULL), enclos = fixture$renv),
                        helper))
  }
  for (name in c("report_mar", "report_deck")) {
    cmd <- fixture$rcmd(name, tt)
    inputs <- eval(as.list(cmd)[["input"]])
    stopifnot(!helper %in% inputs,
              all(c("rpt/sections/format.R", "rpt/sections/symbol-map.R",
                    "tools/evidence_notes.R", "tools/report_typography.R") %in% inputs))
  }
  cat("PASS R15 real construction wiring\n")
  invisible(TRUE)
}

.run_r15_selected <- function(wiring = FALSE) {
  fixture <- .r15_setup(normalizePath("."), tempfile("r15-selected-", tmpdir = dirname(tempdir())))
  # Setup and construction must succeed before independent case accounting.
  fixture$rtargets()
  if (wiring) .r15_wiring(fixture) else .r15_collect(.r15_cases(fixture))
  invisible(TRUE)
}
.run_nonmem_base_model_tests <- function() {
  file_arg <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
  TOOLS <- normalizePath(file.path(dirname(file_arg), ".."), mustWork = TRUE)
  env <- new.env()
  sys.source(file.path(TOOLS, "run_report.R"), envir = env)
  sys.source(file.path(TOOLS, "record_selection.R"), envir = env)
  frames_file <- file.path(TOOLS, "nonmem_frames.R")
  if (file.exists(frames_file)) sys.source(frames_file, envir = env)
  COMPOSER <- file.path(TOOLS, "..", "pipeline", "compose.R")
  sys.source(COMPOSER, envir = env)
  MODULE <- file.path(TOOLS, "..", "pipeline", "tasks", "base_model.R")

  HEADER <- "| Parameter | Estimate | RSE% | Natural scale | 95% CI | BSV CV% | Shrink(SD)% |"
  NOTE <- paste("Natural scale, 95% CI and BSV CV% shown as - are not computed",
                "for this run (no signed reference supplies them); - here does",
                "not mean not applicable. NA in RSE% means the engine returned",
                "no standard error.")
  record <- function(aic = 110) {
    list(meta = list(run = 1, based_on = "", hypothesis = "h"),
         n_id = 2L, n_obs = 3L, est = "FOCE", converged = TRUE, message = "ok",
         ofv = 100, aic = aic)
  }
  full <- function() {
    data.frame(run = 1, parameter = c("CL", "V"), estimate = c(1.5, 2),
               rse_pct = c(10, 20), back_transformed = c(2, 3),
               ci_lower = c(1.25, 2.5), ci_upper = c(3.5, 4),
               bsv_cv_pct = c(30, NA_real_), shrink_sd_pct = c(12.5, NA_real_),
               stringsAsFactors = FALSE)
  }
  render <- function(params, aic = 110) env$run_report_render(list(record(aic)), params)
  # The cells of a parameter row, split on the table separators.
  cells <- function(line) trimws(strsplit(sub("^\\| (.*) \\|$", "\\1", line), " | ", fixed = TRUE)[[1]])
  row_of <- function(lines, name) lines[startsWith(lines, sprintf("| %s |", name))]

  cases <- list(
    T1 = function() {
      out <- render(data.frame(run = 1, parameter = c("CL", "V"), estimate = c(1.5, 2),
                               rse_pct = c(10, 20), shrink_sd_pct = c(12.5, 5),
                               stringsAsFactors = FALSE))
      stopifnot(sum(out == HEADER) == 1L)
      for (nm in c("CL", "V")) {
        cl <- cells(row_of(out, nm))
        stopifnot(length(cl) == 7L, cl[4] == "-", cl[5] == "[-, -]", cl[6] == "-")
      }
      stopifnot(sum(out == NOTE) == 1L,
                which(out == NOTE) == which(startsWith(out, "| V |")) + 2L,
                out[which(out == NOTE) - 1L] == "")
    },
    T2 = function() {
      out <- render(transform(full(), bsv_cv_pct = c(30, 40), shrink_sd_pct = c(12.5, 5)))
      # formatC(digits = 4, format = "g") pads each number to width 5.
      expected <- c(
        "# Run report", "", "## Run 1", "",
        "- **Structure / hypothesis**: h",
        "- **Based on**: - (reference)",
        "- **Subjects / observations**: 2 / 3",
        "- **Estimation**: FOCE; converged (ok)",
        "- **OFV**:   100 | **dOFV**: - (no parent) | **AIC**:   110",
        "",
        HEADER,
        "|---|---|---|---|---|---|---|",
        "| CL |   1.5 |    10 |     2 | [ 1.25,   3.5] |    30 |  12.5 |",
        "| V |     2 |    20 |     3 | [  2.5,     4] |    40 |     5 |",
        "")
      stopifnot(identical(out[-2], expected), !any(out == NOTE))
    },
    T3 = function() {
      p <- full()
      p$back_transformed[2] <- NA_real_
      p$ci_lower[2] <- NA_real_
      p$ci_upper[2] <- NA_real_
      cl <- cells(row_of(render(p), "V"))
      stopifnot(cl[4] == "-", cl[5] == "[-, -]", cl[6] == "-", cl[7] == "-")
    },
    T4 = function() {
      out <- render(full(), aic = NA_real_)
      stopifnot(sum(endsWith(out, "| **AIC**:    NA")) == 1L)
    },
    T5 = function() {
      p <- full()
      names(p)[names(p) == "ci_lower"] <- "ci_lower_95"
      names(p)[names(p) == "ci_upper"] <- "ci_upper_95"
      for (nm in c("CL", "V")) stopifnot(cells(row_of(render(p), nm))[5] == "[-, -]")
    })

  # ---- Part A (B3a) --------------------------------------------------------
  FX <- tempfile("pmx-b3a-")
  dir.create(FX)
  # Expects `expr` to stop with a message matching `regexp`.
  stops <- function(expr, regexp) {
    msg <- tryCatch({ force(expr); NULL }, error = function(e) conditionMessage(e))
    if (is.null(msg)) stop("no error was raised (expected: ", regexp, ")")
    if (!grepl(regexp, msg)) stop("wrong error: ", msg, " (expected: ", regexp, ")")
    invisible(TRUE)
  }
  # A joined residual frame: two doses, three kept observations, one flagged.
  resid <- function() {
    data.frame(NUM = 1:6, ID = c(1, 1, 1, 2, 2, 2), TIME = c(0, 1, 2, 0, 1, 2),
               EVID = c(1, 0, 0, 1, 0, 0), C = c(".", ".", "C", ".", ".", "."),
               DV = c(0, 10, 9, 0, 8, 7), PRED = c(0, 9, 8, 0, 7, 6),
               IPRED = c(0, 9.5, 8.5, 0, 7.5, 6.5), CWRES = c(0, 0.1, 0.2, 0, -0.1, 0.3),
               TAD = c(0, 1, 2, 0, 1, 2), NPDE = c(0, 0.5, -0.5, 0, 0.2, 1.1),
               stringsAsFactors = FALSE)
  }
  STREAM <- c(
    "$PROBLEM fixture",
    "; a comment naming SEED=1 is ignored",
    "$INPUT C NUM ID TIME EVID DV",
    "$ESTIMATION METHOD=1   INTERACTION ; first record",
    "$ESTIMATION METHOD=IMP",
    "    EONLY=1   ; the second continues",
    "$COVARIANCE",
    "$TABLE NUM TAD PRED IPRED CWRES NPDE",
    "       ESAMPLE=500 SEED=20260927 NOPRINT FILE=x.tab",
    "$TABLE NUM ONEHEADER FILE=y.tab")
  stream <- function(lines = STREAM) {
    p <- tempfile("run", tmpdir = FX, fileext = ".mod")
    writeLines(lines, p)
    p
  }
  frame <- function(r = resid(), lines = STREAM, n_obs = 3L, nsim = 500, seed = 20260927)
    env$nm_observation_frame(r, stream(lines), n_obs, nsim, seed)
  heur <- function(mt = FALSE, eta = TRUE)
    list(covariance_step_aborted = FALSE, eta_pval_significant = eta,
         minimization_terminated = mt, prderr = FALSE)
  params <- function() {
    data.frame(parameter = c("THETA1", "OMEGA(1,1)", "OMEGA(2,2)", "SIGMA(1,1)"),
               estimate = c(1.5, 0.1, 0.2, 0.05), stderr = c(0.1, NA_real_, 0.02, 0.01),
               rse_pct = c(6.5, NA_real_, 10, 20), random_effect_sd = c(NA_real_, 0.3, 0.4, 0.2),
               shrink_sd_pct = c(NA_real_, 12.5, 30, 5), fixed = c(FALSE, FALSE, FALSE, FALSE),
               stringsAsFactors = FALSE)
  }
  stub_fit <- function(run) {
    list(meta = list(run = run, based_on = "", hypothesis = "h"), converged = FALSE,
         message = "MINIMIZATION SUCCESSFUL",
         run_start = as.POSIXct("2026-09-29 10:00:00", tz = "UTC"))
  }
  stub_res <- function(run, h = heur()) {
    results <- list(ofv = 100, n_id = 2L, n_obs = 3L, params = params(), heuristics = h)
    f <- frame()
    list(results = results, frame = f,
         record = env$nm_run_record(stub_fit(run), results, f),
         no_bbr_heuristic_raised = FALSE)
  }
  entry <- function(run, status = "launch", based_on = "", retry_of = "", ...) {
    e <- list(run = run, step = "1", based_on = based_on, retry_of = retry_of,
              status = status, ...)
    if (status == "launch" && is.null(e$etas)) e$etas <- c(ET1 = "OMEGA(1,1)")
    if (status == "launch" && is.null(e$hypothesis)) e$hypothesis <- "h"
    e
  }
  decl <- function(runs = list(entry("201")), ...) {
    d <- list(engine = "nonmem", model_dir = "m", map = "map.md", seed = 20260927,
              npde_nsim = 500, run_budget = list(used = 0, max = 20),
              gof = list(cont_cov = "WT", cat_cov = "APGLOW", strata = "APGLOW"),
              runs = runs)
    x <- list(...)
    d[names(x)] <- x
    d[!vapply(d, is.null, logical(1))]
  }
  launches <- function(n) lapply(sprintf("%03d", 200 + seq_len(n)), entry)
  # The module sourced as compose.R sources it; returns the target names, or
  # the target objects themselves when all is TRUE.
  module <- function(ti, all = FALSE) {
    me <- new.env(parent = env)
    me$task_input <- ti
    me$analysis_id <- "B3A-FIX"
    me$upstream_gate <- "gate_eda"
    me$task_targets <- NULL   # so the module's <<- binds here, as in test_covariate_task.R
    old <- setwd(AX)
    on.exit(setwd(old), add = TRUE)
    sys.source(MODULE, envir = me)
    if (all) return(me$task_targets)
    vapply(me$task_targets, function(t) t$settings$name, character(1))
  }
  # A disposable mini-repository holding the module and the helper file.
  AX <- file.path(FX, "repo")
  dir.create(file.path(AX, "pipeline", "tasks"), recursive = TRUE)
  dir.create(file.path(AX, "tools"))
  stopifnot(file.copy(COMPOSER, file.path(AX, "pipeline", "compose.R")),
            file.copy(MODULE, file.path(AX, "pipeline", "tasks", "base_model.R")))
  base_files <- function(root, runs) {
    dir.create(file.path(root, "m/diffs"), recursive = TRUE, showWarnings = FALSE)
    writeLines("# Synthetic analysis plan", file.path(root, "map.md"))
    writeLines("# Synthetic run notes", file.path(root, "m/run-notes.md"))
    writeLines("psn: {}", file.path(root, "m/run-config.yaml"))
    for (run in runs) {
      writeLines("$PROBLEM Synthetic stream", file.path(root, "m", paste0(run, ".mod")))
      writeLines("# Synthetic diff", file.path(root, "m/diffs", paste0(run, ".diff")))
    }
  }
  base_files(AX, 201:221)
  dir.create(file.path(AX, "m/202"))
  writeLines("Synthetic failed listing", file.path(AX, "m/202/202.lst"))
  writeLines("Synthetic failed estimates", file.path(AX, "m/202/202.ext"))
  if (file.exists(frames_file))
    file.copy(frames_file, file.path(FX, "repo", "tools", "nonmem_frames.R"))
  compose <- function(d, fn = targets::tar_manifest, ...) {
    rds <- tempfile("decl", tmpdir = file.path(FX, "repo"), fileext = ".rds")
    saveRDS(d, rds)
    script <- sub("\\.rds$", ".R", rds)
    writeLines(c(
      "fx_env <- environment()",
      "source('pipeline/compose.R', local = fx_env)",
      "source('tools/nonmem_frames.R', local = fx_env)",
      "me <- new.env(parent = fx_env)",
      sprintf("me$task_input <- readRDS('%s')", basename(rds)),
      "me$analysis_id <- 'B3A-FIX'",
      "me$upstream_gate <- 'gate_eda'",
      "me$task_targets <- NULL",
      "sys.source('pipeline/tasks/base_model.R', envir = me)",
      "me$task_targets"), script)
    old <- setwd(file.path(FX, "repo"))
    on.exit(setwd(old))
    suppressMessages(fn(script = basename(script), callr_function = NULL,
                        envir = new.env(parent = globalenv()), ...))
  }
  B3A_NAMES <- c("base_model_package", "bm_budget", "bm_comparison", "bm_ctl_201_file",
                 "bm_diff_201_file", "bm_fit_201", "bm_gof_201", "bm_gof_reference",
                 "bm_heuristics", "bm_log_axis_audit", "bm_map_file", "bm_nm_reference",
                 "bm_params", "bm_records", "bm_res_201", "bm_run_config_file",
                 "bm_run_notes_file", "bm_run_report")
  ESCALATE <- "run budget exceeded: 21 of 20 declared; ESCALATE to the SME"

  cases <- c(cases, list(
    A01_frame_passes = function() {
      r <- resid()
      out <- frame(r)
      keep <- r$C != "C" & r$EVID == 0
      stopifnot(identical(structure(out, npde_settings = NULL, estimation = NULL),
                          r[keep, , drop = FALSE]),
                identical(attr(out, "estimation"),
                          "$ESTIMATION METHOD=1 INTERACTION; $ESTIMATION METHOD=IMP EONLY=1"),
                identical(attr(out, "npde_settings")[c("esample", "seed")],
                          list(esample = "500", seed = "20260927")))
    },
    A02_step1_duplicate_dv = function() {
      r <- resid(); r$DV.DATA <- r$DV
      stops(frame(r), "step 1: .*DV\\.DATA")
      r <- resid(); r$DV.1 <- r$DV
      stops(frame(r), "step 1: .*DV\\.1")
    },
    A03_step2_missing_column = function() {
      stops(frame(resid()[names(resid()) != "TAD"]), "step 2: missing column\\(s\\) TAD")
    },
    A04_step4_count = function() stops(frame(n_obs = 4L), "step 4"),
    A05_step5_tad = function() {
      r <- resid(); r$TAD[2] <- -1
      stops(frame(r), "step 5")
      r <- resid(); r$TAD[5] <- NA
      stops(frame(r), "step 5")
    },
    A06_step6_continuation_seed_read = function() stops(frame(seed = 1), "step 6: .*SEED=20260927"),
    A07_step6_seed_in_comment_ignored = function() {
      s <- STREAM
      s[9] <- "       ESAMPLE=500 NOPRINT FILE=x.tab ; SEED=20260927"
      stops(frame(lines = s), "step 6: .*without both")
    },
    A08_step6_esample_differs = function() stops(frame(nsim = 1000), "step 6: .*ESAMPLE=500"),
    A09_step6_requested_absent = function()
      stops(frame(resid()[names(resid()) != "NPDE"]), "step 6: NPDE is requested"),
    A10_step6_present_not_requested = function() {
      s <- STREAM
      s[8:9] <- c("$TABLE NUM TAD PRED IPRED CWRES", "       NOPRINT FILE=x.tab")
      stops(frame(lines = s), "step 6: .*no \\$TABLE record requests it")
    },
    A11_step6_npde_not_finite = function() {
      r <- resid(); r$NPDE[6] <- NA
      stops(frame(r), "step 6: NPDE is not finite")
    },
    A12_step7_no_estimation = function()
      stops(frame(lines = STREAM[-(4:6)]), "step 7: .*no \\$ESTIMATION"),
    A13_step8_abbreviations = function() {
      s <- STREAM; s[4:5] <- sub("^\\$ESTIMATION", "$EST", s[4:5])
      stops(frame(lines = s), "step 8: .*\\$EST")
      s <- STREAM; s[c(8, 10)] <- sub("^\\$TABLE", "$TAB", s[c(8, 10)])
      stops(frame(lines = s), "step 8: .*\\$TAB")
    },
    A14_minimization_terminated = function() {
      stopifnot(identical(env$nm_minimization_terminated(heur(FALSE, TRUE), "201"), FALSE),
                identical(env$nm_minimization_terminated(heur(TRUE), "201"), TRUE))
      stops(env$nm_minimization_terminated(list(prderr = FALSE), "201"),
            "run 201: .*no minimization_terminated")
      stops(env$nm_minimization_terminated(list(minimization_terminated_extra = TRUE), "201"),
            "run 201: .*no minimization_terminated")
      stops(env$nm_minimization_terminated(list(minimization_terminated = NA), "201"),
            "not one non-NA logical")
    },
    A15_run_record = function() {
      rec <- env$nm_run_record(stub_fit("201"), list(heuristics = heur(FALSE, TRUE),
                                                    n_id = 2L, n_obs = 3L, ofv = 100), frame())
      stopifnot(isTRUE(rec$converged), identical(rec$aic, NA_real_),
                identical(rec$run_start, stub_fit("201")$run_start),
                identical(rec$est, attr(frame(), "estimation")),
                identical(rec$message, "MINIMIZATION SUCCESSFUL"))
    },
    A16_params_table_and_render = function() {
      rl <- list(stub_res("201"), stub_res("202"))
      p <- env$nm_params_table(rl)
      stopifnot(identical(names(p)[1], "run"), identical(p$run, rep(c("201", "202"), each = 4)))
      for (col in names(params()))
        stopifnot(identical(p[[col]], c(params()[[col]], params()[[col]])))
      out <- env$run_report_render(lapply(rl, `[[`, "record"), p)
      stopifnot(sum(out == HEADER) == 2L, sum(out == NOTE) == 2L,
                sum(grepl("converged \\(MINIMIZATION SUCCESSFUL\\)", out)) == 2L)
    },
    A17_heuristics_and_comparison = function() {
      rl <- list(stub_res("201", heur(FALSE, TRUE)), stub_res("202", heur(TRUE, FALSE)))
      h <- env$nm_heuristics_table(rl)
      stopifnot(nrow(h) == 8L, identical(h$run, rep(c("201", "202"), each = 4)),
                identical(h$heuristic, rep(names(heur()), 2)),
                identical(h$value, unname(c(unlist(heur(FALSE, TRUE)), unlist(heur(TRUE, FALSE))))))
      cmp <- env$nm_comparison_table(rl, list(entry("201"), entry("202", based_on = "201")))
      stopifnot(identical(cmp$minimization_terminated, c(FALSE, TRUE)),
                identical(cmp$step, c("1", "1")), !"condition_number" %in% names(cmp))
    },
    A18_eta_shrinkage = function() {
      stopifnot(identical(env$nm_eta_shrinkage(params(), c(ET1 = "OMEGA(1,1)", ET2 = "OMEGA(2,2)")),
                          c(ET1 = 12.5, ET2 = 30)))
      stops(env$nm_eta_shrinkage(params(), c(ET3 = "OMEGA(3,3)")), "OMEGA\\(3,3\\) appears 0 times")
      stops(env$nm_eta_shrinkage(rbind(params(), params()), c(ET1 = "OMEGA(1,1)")),
            "appears 2 times")
    },
    A19_write_panels = function() {
      g <- ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y))
      path <- file.path(FX, "out", "gof.pdf")
      stops(env$nm_write_panels(list(dv_pred = g, shrinkage = data.frame(a = 1)), path),
            "element shrinkage is of class data.frame")
      stopifnot(!file.exists(path),
                identical(env$nm_write_panels(list(a = g, b = g, eta_cont = list(g, g)), path), path),
                file.exists(path))
    },
    A20_budget_stop = function() {
      stopifnot(isTRUE(env$nm_budget_stop(0, 20, 20)))
      stops(env$nm_budget_stop(0, 21, 20), ESCALATE)
    },
    A21_module_names_and_budget_table = function() {
      stopifnot(identical(sort(module(decl())), sort(B3A_NAMES)))
      n <- module(decl(list(entry("201"), entry("202", "failed-counted", evidence = "e"),
                            entry("203", "stopped-before-estimation", evidence = "e"))))
      stopifnot(all(c("bm_lst_202_file", "bm_ext_202_file", "bm_ctl_203_file",
                      "bm_diff_203_file") %in% n), !any(c("bm_fit_202", "bm_fit_203") %in% n))
      b <- env$nm_budget_table(list(entry("201"), entry("202", "stopped-before-estimation",
                                                        evidence = c("a", "b"))), 0, 20)
      stopifnot(identical(b$runs$counted, c(TRUE, FALSE)), identical(b$totals$total, 1),
                identical(b$runs$etas, c("ET1 = OMEGA(1,1)", "")))
    },
    A22_compose_refusals = function() {
      bad <- list(
        model_dir = decl(model_dir = NULL),
        "runs:" = decl(runs = list()),
        "\\$run:" = decl(runs = list(entry("20"))),
        "unique" = decl(runs = list(entry("201"), entry("201"))),
        based_on = decl(runs = list(entry("201", based_on = "199"))),
        retry_of = decl(runs = list(entry("201", retry_of = "202"), entry("202"))),
        status = decl(runs = list(entry("201", "done"))),
        evidence = decl(runs = list(entry("201"), entry("202", "failed-counted"))),
        hypothesis = decl(runs = list(entry("201", hypothesis = ""))),
        etas = decl(runs = list(entry("201", etas = c("OMEGA(1,1)")))),
        etas = decl(runs = list(entry("201", etas = c(ET1 = "OMEGA(1,1)", ET1 = "OMEGA(2,2)")))),
        comparisons = decl(runs = list(entry("201"), entry("202", "stopped-before-estimation",
                                                           evidence = "e")),
                           comparisons = list(x = list(parent = "201", child = "202"))),
        exponent = decl(exponent = list(run = "209", parameter = "THETA3")),
        covariate_comparisons = decl(covariate_comparisons = list(
          w = list(unit = "covariate_effect", run = "205"))),
        npde_nsim = decl(npde_nsim = 0.5),
        seed = decl(seed = -1),
        run_budget = decl(run_budget = list(used = 0)),
        gof = decl(gof = list(cont_cov = "WT", cat_cov = "APGLOW")))
      for (i in seq_along(bad)) stops(module(bad[[i]]), names(bad)[i])
      # failed-counted counts, stopped-before-estimation does not.
      stops(module(decl(runs = list(entry("201"), entry("202", "failed-counted", evidence = "e")),
                        run_budget = list(used = 19, max = 20))), "21 of 20")
      module(decl(runs = list(entry("201"), entry("202", "stopped-before-estimation", evidence = "e")),
                  run_budget = list(used = 19, max = 20)))
    },
    A23_manifest_names_and_budget_limit = function() {
      m <- compose(decl())
      stopifnot(identical(sort(m$name), sort(B3A_NAMES)))
      stopifnot(nrow(compose(decl(runs = launches(20)))) > 0L)
      stops(compose(decl(runs = launches(21))), ESCALATE)
    },
    A24_fit_command_stable_on_append = function() {
      m1 <- compose(decl(), fields = c("name", "command"))
      m2 <- compose(decl(runs = list(entry("201"), entry("202", based_on = "201"))),
                    fields = c("name", "command"))
      stopifnot(identical(m1$command[m1$name == "bm_fit_201"], m2$command[m2$name == "bm_fit_201"]))
      e <- compose(decl(runs = list(entry("201"), entry("202", based_on = "201"))),
                   fn = targets::tar_network, targets_only = TRUE, outdated = FALSE)$edges
      into_fit <- e$from[e$to == "bm_fit_201"]
      stopifnot(setequal(into_fit, c("bm_ctl_201_file", "bm_nm_reference")),
                !any(grepl("budget", into_fit)))
    },
    A25_map_file_reaches_no_fit_or_result = function() {
      e <- compose(decl(), fn = targets::tar_network, targets_only = TRUE, outdated = FALSE)$edges
      d <- "bm_map_file"
      repeat {
        nx <- unique(c(d, e$to[e$from %in% d]))
        if (length(nx) == length(d)) break
        d <- nx
      }
      stopifnot(identical(sort(d), c("base_model_package", "bm_map_file")))
    },
    # Remediation round 1 (code review findings 1 and 2, note 9).
    A26_budget_stop_inputs = function() {
      stopifnot(isTRUE(env$nm_budget_stop(0, 20, 20)))
      stops(env$nm_budget_stop(0, 21, 20), ESCALATE)
      stops(env$nm_budget_stop(-100, 1, 20), "nm_budget_stop: used must be")
      stops(env$nm_budget_stop("0", 1, 20), "nm_budget_stop: used must be")
      stops(env$nm_budget_stop(NA_real_, 1, 20), "nm_budget_stop: used must be")
      stops(env$nm_budget_stop(0.5, 1, 20), "nm_budget_stop: used must be")
      stops(env$nm_budget_stop(0, 1, Inf), "nm_budget_stop: max must be")
    },
    A27_compose_budget_fields = function() {
      stops(module(decl(run_budget = list(used = "0", max = 20))), "run_budget\\$used")
      stops(module(decl(run_budget = list(used = -1, max = 20))), "run_budget\\$used")
      stops(module(decl(run_budget = list(used = 0, max = 0))), "run_budget\\$max")
      stops(module(decl(run_budget = list(used = 0, max = 2.5))), "run_budget\\$max")
    },
    A28_compose_step_required = function() {
      no_step <- entry("201"); no_step$step <- NULL
      stops(module(decl(runs = list(no_step))), "\\(run 201\\) step")
      no_step$step <- ""
      stops(module(decl(runs = list(no_step))), "\\(run 201\\) step")
    },
    A29_compose_based_on_not_self = function()
      stops(module(decl(runs = list(entry("201", based_on = "201")))), "\\(run 201\\) based_on")))

  # ---- Part B (B3b) ----
  # nm_lrt_inputs and nm_exponent_row on stub results, the approved nested-LRT
  # unit on what nm_lrt_inputs returns, and the bm_lrt and bm_exponent targets
  # composed from fixture declarations. Stub numbers only.
  REPO <- normalizePath(file.path(TOOLS, ".."), mustWork = TRUE)
  sys.source(file.path(TOOLS, "load_reference.R"), envir = env)
  approve_fixture_method <- function(root, skill, units = NULL) {
    path <- file.path(root, "skills", skill, "MANIFEST.yaml")
    if (is.null(units)) units <- names(yaml::read_yaml(path)$units)
    lines <- readLines(path)
    for (unit in units) {
      header <- which(lines == paste0("  ", unit, ":"))
      stopifnot(length(header) == 1L)
      status <- header + 1L
      stopifnot(lines[status] %in% c("    status: draft", "    status: approved"))
      lines[status] <- "    status: approved"
    }
    lines <- sub("^reviewed_by: .*$", 'reviewed_by: "Alex Rowan"', lines)
    lines <- sub("^reviewed_at: .*$", 'reviewed_at: "2026-11-20"', lines)
    writeLines(lines, path)
  }
  # A stub result whose heuristics and OFV are replaced after the record is
  # built (nm_run_record itself reads minimization_terminated).
  res_with <- function(run, h = heur(), ofv = 100) {
    r <- stub_res(run)
    r$results$heuristics <- h
    r$results$ofv <- ofv
    r
  }
  cmp <- function(parent = "201", child = "202", ...) {
    x <- list(parent = parent, child = child, criterion_dofv = 3.84, df = 1,
              alpha = 0.05, rule = "MAP 4.1 W1")
    y <- list(...)
    x[names(y)] <- y
    x[!vapply(x, is.null, logical(1))]
  }
  # The approved unit, loaded as the bm_lrt target loads it.
  lrt <- function(inputs, c = cmp()) {
    old <- setwd(MR)
    on.exit(setwd(old))
    env$load_reference("skills/covariate-priors",
                       "skills/covariate-priors/reference/nested_lrt.R",
                       envir = environment())
    reference_nested_lrt(inputs$ofv_parent, inputs$ofv_child, c$criterion_dofv,
                         c$df, c$alpha)
  }
  two_runs <- function(...)
    decl(runs = list(entry("201"), entry("202", based_on = "201")), ...)
  CMPS <- list(w1 = cmp())
  EXPO <- list(run = "202", parameter = "THETA3", rse_bound_pct = 30)

  cases <- c(cases, list(
    B01_lrt_mixed_method_stops = function() {
      child <- res_with("202")
      attr(child$frame, "estimation") <- "$ESTIMATION METHOD=SAEM"
      stops(env$nm_lrt_inputs(cmp(), list(res_with("201"), child)),
            "201 and 202 .*\\$ESTIMATION")
      # The stop names the runs, never an OFV.
      msg <- tryCatch(env$nm_lrt_inputs(cmp(), list(res_with("201", ofv = 123.5), child)),
                      error = conditionMessage)
      stopifnot(!grepl("123", msg))
    },
    B02_lrt_missing_run_stops = function()
      stops(env$nm_lrt_inputs(cmp(child = "203"), list(res_with("201"), res_with("202"))),
            "run 203"),
    B03_lrt_other_heuristic_keeps_ofv = function() {
      # (i) eta_pval_significant TRUE, minimization_terminated FALSE: not failed.
      p <- res_with("201", heur(FALSE, TRUE), ofv = 100)
      ch <- res_with("202", heur(FALSE, TRUE), ofv = 95.5)
      stopifnot(identical(env$nm_minimization_terminated(ch$results$heuristics, "202"), FALSE))
      out <- env$nm_lrt_inputs(cmp(), list(p, ch))
      stopifnot(identical(names(out), c("ofv_parent", "ofv_child")),
                identical(out$ofv_parent, p$results$ofv),
                identical(out$ofv_child, ch$results$ofv))
      u <- lrt(out)
      stopifnot(!is.na(u$dofv))
    },
    B04_lrt_terminated_passes_na = function() {
      # (ii) minimization_terminated TRUE: failed, NA passed, dofv NA.
      ch <- res_with("202", heur(TRUE, FALSE), ofv = 95.5)
      stopifnot(identical(env$nm_minimization_terminated(ch$results$heuristics, "202"), TRUE))
      out <- env$nm_lrt_inputs(cmp(), list(res_with("201"), ch))
      stopifnot(identical(out$ofv_child, NA_real_), identical(out$ofv_parent, 100))
      u <- lrt(out)
      stopifnot(is.na(u$dofv), identical(u$significant, FALSE))
    },
    B05_lrt_heuristics_name_exact = function() {
      # (iii) no minimization_terminated name, or only a longer name that $
      # would partially match: stop naming the run.
      for (h in list(list(prderr = FALSE, eta_pval_significant = TRUE),
                     list(minimization_terminated_extra = FALSE)))
        stops(env$nm_lrt_inputs(cmp(), list(res_with("201"), res_with("202", h))),
              "run 202: .*no minimization_terminated")
    },
    B06_exponent_row_exact = function() {
      out <- env$nm_exponent_row(list(run = "202", parameter = "THETA1", rse_bound_pct = 30),
                                 list(res_with("201"), res_with("202")))
      stopifnot(identical(names(out), c("run", "parameter", "estimate", "rse_pct",
                                        "rse_bound_pct", "within_bound")),
                nrow(out) == 1L, identical(out$run, "202"), identical(out$parameter, "THETA1"),
                identical(out$estimate, 1.5), identical(out$rse_pct, 6.5),
                identical(out$rse_bound_pct, 30), identical(out$within_bound, TRUE))
      r <- res_with("202")
      r$results$params$rse_pct[1] <- 45
      stopifnot(identical(env$nm_exponent_row(list(run = "202", parameter = "THETA1",
                                                   rse_bound_pct = 30), list(r))$within_bound,
                          FALSE))
      r$results$params$rse_pct[1] <- NA_real_
      stopifnot(identical(env$nm_exponent_row(list(run = "202", parameter = "THETA1",
                                                   rse_bound_pct = 30), list(r))$within_bound,
                          NA))
    },
    B07_exponent_row_stops = function() {
      spec <- list(run = "202", parameter = "THETA1", rse_bound_pct = 30)
      r <- res_with("202")
      r$results$params$parameter[1] <- "THETA10"   # a longer name never matches THETA1
      stops(env$nm_exponent_row(spec, list(r)), "run 202.*THETA1 appears 0 times")
      r <- res_with("202")
      r$results$params <- rbind(r$results$params, r$results$params)
      stops(env$nm_exponent_row(spec, list(r)), "run 202.*THETA1 appears 2 times")
      stops(env$nm_exponent_row(spec, list(res_with("201"))), "run 202")
    },
    B08_compose_names_with_and_without = function() {
      stopifnot(identical(sort(compose(decl())$name), sort(B3A_NAMES)))
      without <- compose(two_runs(), fields = c("name", "command"))
      with <- compose(two_runs(comparisons = CMPS, exponent = EXPO),
                      fields = c("name", "command"))
      stopifnot(all(without$name %in% with$name),
                setequal(setdiff(with$name, without$name),
                         c("bm_lrt", "bm_exponent", "bm_lrt_reference")))
      # B3a's commands are unchanged when neither field is declared.
      for (n in without$name)
        stopifnot(identical(without$command[without$name == n] == with$command[with$name == n],
                            n != "base_model_package"))
      lrt_cmd <- with$command[with$name == "bm_lrt"]
      stopifnot(grepl("load_reference(\"skills/covariate-priors\", bm_lrt_reference",
                      lrt_cmd, fixed = TRUE),
                grepl("reference_nested_lrt(", lrt_cmd, fixed = TRUE),
                grepl("nm_lrt_inputs(", lrt_cmd, fixed = TRUE))
      pkg <- with$command[with$name == "base_model_package"]
      stopifnot(grepl("bm_lrt", pkg, fixed = TRUE), grepl("bm_exponent", pkg, fixed = TRUE))
      # The two table titles, verbatim as the module declares them.
      stopifnot(
        grepl(paste0("Nested likelihood-ratio tests (reference_nested_lrt, ",
                     "skills/covariate-priors): residual error, W1, IIV covariance; ",
                     "dOFV and the declared criterion, NA for a failed run"), pkg, fixed = TRUE),
        grepl(paste0("Estimated weight exponent beside its declared RSE bound ",
                     "(MAP 4.1 W1 and E2); within_bound is a comparison, nothing is ",
                     "computed"), pkg, fixed = TRUE))
      # bm_lrt rows follow the declaration order of the comparisons (names
      # deliberately not sorted), evaluated on stub results.
      three <- decl(runs = list(entry("201"), entry("202", based_on = "201"),
                                entry("203", based_on = "202")),
                    comparisons = list(z2 = cmp("202", "203"), a1 = cmp("201", "202")))
      tt <- module(three, all = TRUE)
      lrt_t <- tt[[which(vapply(tt, function(t) t$settings$name, "") == "bm_lrt")]]
      run_env <- new.env(parent = env)
      run_env$bm_res_201 <- res_with("201", ofv = 100)
      run_env$bm_res_202 <- res_with("202", ofv = 95)
      run_env$bm_res_203 <- res_with("203", ofv = 94)
      run_env$bm_lrt_reference <- "skills/covariate-priors/reference/nested_lrt.R"
      old <- setwd(MR)
      out <- tryCatch(eval(lrt_t$command$expr, new.env(parent = run_env)),
                      finally = setwd(old))
      stopifnot(identical(out$comparison, c("z2", "a1")),
                identical(out$parent, c("202", "201")), identical(out$child, c("203", "202")))
      # Each table composes with its own field only.
      n <- module(two_runs(exponent = EXPO))
      stopifnot("bm_exponent" %in% n, !any(c("bm_lrt", "bm_lrt_reference") %in% n))
      n <- module(two_runs(comparisons = CMPS))
      stopifnot(all(c("bm_lrt", "bm_lrt_reference") %in% n), !"bm_exponent" %in% n)
    },
    B09_compose_refusals = function() {
      stops(module(two_runs(comparisons = list(w1 = cmp(child = "209")))),
            "comparisons: names run\\(s\\) .*209")
      stops(module(two_runs(comparisons = list(w1 = cmp(alpha = NULL)))),
            "comparisons \\(w1\\) alpha")
      stops(module(two_runs(comparisons = list(w1 = cmp(rule = "")))),
            "comparisons \\(w1\\) rule")
      stops(module(two_runs(comparisons = list(cmp()))), "comparisons: .*named")
      stops(module(two_runs(exponent = EXPO[c("run", "parameter")])),
            "exponent rse_bound_pct")
      stops(module(two_runs(exponent = replace(EXPO, "parameter", list(NULL)))),
            "exponent parameter")
    },
    B10_compose_entry_shape_refusals = function() {
      # A comparison or the exponent that is not a list refuses naming its
      # field, not with R's own subscript error; a run is never compared with
      # itself.
      stops(module(two_runs(comparisons = list(w1 = "201 vs 202"))),
            "inputs\\$base_model\\$comparisons \\(w1\\): .*list")
      stops(module(two_runs(exponent = "THETA3")),
            "inputs\\$base_model\\$exponent: .*list")
      stops(module(two_runs(comparisons = list(w1 = cmp(child = "201")))),
            "inputs\\$base_model\\$comparisons \\(w1\\): .*compared with itself")
    },
    B11_exponent_row_columns_exact = function() {
      # Columns are read by exact name: a decoy column whose name extends a
      # missing one is never read in its place.
      spec <- list(run = "202", parameter = "THETA1", rse_bound_pct = 30)
      r <- res_with("202")
      names(r$results$params)[names(r$results$params) == "rse_pct"] <- "rse_pct_extra"
      r$results$params$rse_pct_extra <- 99
      stops(env$nm_exponent_row(spec, list(r)), "run 202.*lacks column.*rse_pct")
      r <- res_with("202")
      names(r$results$params)[names(r$results$params) == "estimate"] <- "estimate_sd"
      stops(env$nm_exponent_row(spec, list(r)), "run 202.*lacks column.*estimate")
    }))

  # ---- Part C (B3c) ----
  # A disposable mini-repository holding the covariate module, the manifest
  # validator and the covariate-priors skill files it checks (the
  # test_covariate_task.R allowlist pattern), plus stub run files. Stub
  # numbers only; nothing under pilot/ is read.
  MR <- file.path(FX, "cov-repo")
  for (rel in c("pipeline/tasks/covariate.R", "tools/method_manifest.R",
                "tools/psn_invocation.R",
                "skills/covariate-priors/MANIFEST.yaml",
                "skills/covariate-priors/reference/covariate_effect.R",
                "skills/covariate-priors/reference/covariate_search.R",
                "skills/covariate-priors/reference/nested_lrt.R",
                "skills/covariate-priors/reference/psn_covsearch.R",
                "skills/covariate-priors/reference/scm_config.R",
                "skills/psn-uncertainty/reference/psn_uncertainty.R")) {
    dir.create(dirname(file.path(MR, rel)), recursive = TRUE, showWarnings = FALSE)
    stopifnot(file.copy(file.path(REPO, rel), file.path(MR, rel)))
  }
  approve_fixture_method(MR, "covariate-priors", c("nested_lrt", "covariate_effect"))
  VERSION_LINE <- "1NONLINEAR MIXED EFFECTS MODEL PROGRAM (NONMEM) VERSION 7.5.1"
  # Stub run files for runs 201 to 203 under the mini-repository's model_dir
  # "m"; the .lst of `no_version` lacks the version line.
  run_files <- function(no_version = character(0)) {
    for (r in c("201", "202", "203")) {
      d <- file.path(MR, "m", r)
      dir.create(d, recursive = TRUE, showWarnings = FALSE)
      writeLines(c("stub listing", if (!r %in% no_version) VERSION_LINE, "end"),
                 file.path(d, paste0(r, ".lst")))
      writeLines(c("TABLE NO.     1", "ITERATION THETA1 OBJ", "0 1.5 100"),
                 file.path(d, paste0(r, ".ext")))
      writeLines(paste0("{\"run\": \"", r, "\"}"), file.path(d, "bbi_config.json"))
      writeLines(c("$PROBLEM stub", "$ESTIMATION METHOD=1"), file.path(MR, "m", paste0(r, ".mod")))
    }
    dir.create(file.path(MR, "d"), showWarnings = FALSE)
    writeLines(c("ID,TIME,DV", "1,0,.", "1,1,10"), file.path(MR, "d", "data.csv"))
  }
  run_files()
  PRIORS <- list(
    list(covariate = "WT", parameter = "CL", direction = "increase", mechanism = "allometry"),
    list(covariate = "APGR", parameter = "V", direction = "decrease", mechanism = "stub"))
  WTC <- data.frame(quantity = c("WT_ref: median", "WT05: 5% percentile", "WT95: 95% percentile"),
                    value_kg = c(1.3, 0.9, 2.5), stringsAsFactors = FALSE)
  EXPO_C <- list(run = "202", parameter = "THETA1", rse_bound_pct = 30)
  CCMPS <- list(
    apgv_fwd = list(unit = "nested_lrt", parent = "201", child = "202", test = "forward",
                    criterion_dofv = 3.84, df = 1, alpha = 0.05, prior = 2),
    wt_ratio = list(unit = "covariate_effect", run = "202", band_lower = 0.8,
                    band_upper = 1.25, prior = 1, wt_points = c("WT05", "WT95")),
    wt_fixed = list(unit = "covariate_effect", run = "201", band_lower = 0.8,
                    band_upper = 1.25, prior = 1, wt_points = c("WT05", "WT95"),
                    exponent_fixed = 0.75))
  CHILD_START <- as.POSIXct("2026-09-29 11:22:33", tz = "UTC")
  c_res <- function(child = res_with("202", ofv = 95)) {
    child$record$run_start <- CHILD_START
    list(res_with("201"), child, res_with("203"))
  }
  # One write from the mini-repository root; relative paths as in the
  # pipeline. Returns the written paths.
  write_c <- function(cmps = CCMPS, rl = c_res(), out = "cov", wtc = WTC) {
    old <- setwd(MR)
    on.exit(setwd(old))
    env$nm_write_handoffs(
      comparisons = cmps, res_list = rl,
      streams = c("201" = "m/201.mod", "202" = "m/202.mod", "203" = "m/203.mod"),
      data_file = "d/data.csv", weight_constants = wtc, exponent = EXPO_C,
      priors = PRIORS, model_dir = "m", out_dir = out, analysis_id = "B3C-FIX",
      owner = "target base_model_frozen_yaml (pmx-modeler, base-model stage)")
  }
  # A refusal: stops matching `regexp` and leaves `out` without any file.
  refuses <- function(regexp, ...) {
    out <- basename(tempfile("refuse-", tmpdir = MR))
    stops(write_c(..., out = out), regexp)
    stopifnot(length(list.files(file.path(MR, out), all.files = TRUE, no.. = TRUE)) == 0L)
  }
  # The consumer's validation (check C3 form): covariate.R sourced once with
  # the upstream route, then covariate_task_compute per hand-off with a stub
  # loader and an executor that stops with the sentinel.
  SENTINEL <- "C3 sentinel: validation reached the executor"
  consume <- function(handoffs) {
    old <- setwd(MR)
    on.exit(setwd(old))
    ce <- new.env(parent = env)
    sys.source("tools/method_manifest.R", envir = ce)
    me <- new.env(parent = ce)
    me$task_input <- list(method_unit = "nested_lrt", upstream_target = "base_model_frozen_yaml")
    me$analysis_id <- "B3C-FIX"
    me$upstream_gate <- NULL
    me$task_targets <- NULL
    sys.source("pipeline/tasks/covariate.R", envir = me, keep.source = FALSE)
    loader <- function(skill_dir, reference_path)
      structure(new.env(), load_reference = list(reference_path = reference_path))
    executor <- function(...) stop(SENTINEL, call. = FALSE)
    vapply(handoffs, function(h) {
      unit <- yaml::read_yaml(h)$method_unit
      msg <- tryCatch(me$covariate_task_compute(
        analysis_id = "B3C-FIX",
        task_input = list(method_unit = unit, upstream_target = "base_model_frozen_yaml"),
        covariate_input_file = h,
        covariate_model_file = me$covariate_handoff_role_path(h, "model"),
        covariate_data_file = me$covariate_handoff_role_path(h, "data"),
        covariate_run_config_file = me$covariate_handoff_role_path(h, "run_config"),
        covariate_map_inputs_file = me$covariate_handoff_role_path(h, "map_inputs"),
        reference_loader = loader, reference_executor = executor),
        error = conditionMessage)
      if (identical(msg, SENTINEL)) "accepted" else paste("refused:", msg)
    }, character(1))
  }
  # A two-run declaration with covariate comparisons; `...` overrides or (with
  # NULL) removes a field.
  two_runs_c <- function(cc = CCMPS["apgv_fwd"], ...) {
    x <- list(covariate_comparisons = cc, covariate_dir = "pilot/x/covariate",
              covariate_priors = PRIORS)
    y <- list(...)
    x[names(y)] <- y
    do.call(decl, c(list(runs = list(entry("201"), entry("202", based_on = "201"))), x))
  }

  cases <- c(cases, list(
    C01_producer_passes_consumer_validation = function() {
      paths <- write_c()
      expect <- unlist(lapply(names(CCMPS), function(nm)
        file.path("cov", paste0(c("handoff-", "map-inputs-", "run-config-"), nm, ".yaml"))))
      stopifnot(identical(paths, expect), all(file.exists(file.path(MR, paths))))
      verdict <- consume(file.path("cov", paste0("handoff-", names(CCMPS), ".yaml")))
      if (!all(verdict == "accepted")) stop(paste(verdict, collapse = " | "))
    },
    C02_handoff_fields_and_sources = function() {
      write_c()
      rd <- function(f) yaml::read_yaml(file.path(MR, "cov", f))
      h <- rd("handoff-apgv_fwd.yaml")
      stopifnot(identical(names(h), c("schema_version", "source", "method_unit",
                                      "producing_analysis_id", "producing_run_id",
                                      "parent_fit_id", "owner", "created_at", "engine",
                                      "model", "data", "run_config", "map_inputs")),
                identical(h$producing_run_id, "202"), identical(h$parent_fit_id, "201"),
                identical(h$engine, list(name = "NONMEM", version = "7.5.1")),
                identical(h$model$path, "m/201.mod"), identical(h$data$path, "d/data.csv"),
                identical(h$model$sha256, digest::digest(file = file.path(MR, "m/201.mod"),
                                                         algo = "sha256")))
      m <- rd("map-inputs-apgv_fwd.yaml")
      stopifnot(identical(m$priors, PRIORS[2]),
                identical(m$criteria, list(criterion_dofv = 3.84, df = 1, alpha = 0.05)),
                identical(m$arguments, list(ofv_parent = 100, ofv_child = 95)))
      rc <- rd("run-config-apgv_fwd.yaml")
      stopifnot(identical(names(rc$runs), c("parent", "child")),
                identical(rc$runs$child$directory, "m/202"),
                identical(rc$runs$child$estimation, attr(frame(), "estimation")),
                identical(names(rc$runs$child$files), c("lst", "ext", "bbi_config")),
                identical(rc$runs$parent$files$ext$sha256,
                          digest::digest(file = file.path(MR, "m/201/201.ext"), algo = "sha256")))
      w <- rd("map-inputs-wt_ratio.yaml")
      stopifnot(identical(w$arguments, list(wt_low = 0.9, wt_high = 2.5, exponents = 1.5)),
                identical(w$criteria, list(band_lower = 0.8, band_upper = 1.25)),
                identical(w$priors, PRIORS[1]),
                identical(rd("map-inputs-wt_fixed.yaml")$arguments$exponents, 0.75),
                identical(names(rd("run-config-wt_ratio.yaml")$runs), "selected"))
    },
    C03_created_at_is_child_run_start = function() {
      before <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      write_c()
      h <- yaml::read_yaml(file.path(MR, "cov", "handoff-apgv_fwd.yaml"))
      stopifnot(identical(h$created_at, format(CHILD_START, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
                h$created_at != before)
    },
    C04_two_writes_byte_identical = function() {
      p <- file.path(MR, write_c())
      first <- lapply(p, function(f) readBin(f, "raw", file.info(f)$size))
      Sys.sleep(1.1)
      p2 <- file.path(MR, write_c())
      second <- lapply(p2, function(f) readBin(f, "raw", file.info(f)$size))
      stopifnot(identical(p, p2), identical(first, second))
    },
    C05_refuses_failed_run = function()
      refuses("comparison apgv_fwd: .*run 202.*minimization_terminated",
              rl = c_res(res_with("202", heur(TRUE, FALSE), ofv = 95))),
    C06_refuses_missing_ofv = function() {
      refuses("comparison apgv_fwd: .*run 202.*OFV", rl = c_res(res_with("202", ofv = NA_real_)))
      ch <- res_with("202"); ch$results$ofv <- NULL
      refuses("comparison apgv_fwd: .*run 202.*OFV", rl = c_res(ch))
    },
    C07_refuses_mixed_methods = function() {
      ch <- res_with("202", ofv = 95)
      attr(ch$frame, "estimation") <- "$ESTIMATION METHOD=SAEM"
      ch$record$est <- "$ESTIMATION METHOD=SAEM"
      refuses("comparison apgv_fwd: .*\\$ESTIMATION", rl = c_res(ch))
    },
    C08_refuses_undeclared_run = function() {
      bad <- CCMPS; bad$apgv_fwd$child <- "209"
      refuses("comparison apgv_fwd: .*209", cmps = bad)
      bad <- CCMPS; bad$wt_ratio$run <- "209"
      refuses("comparison wt_ratio: .*209", cmps = bad)
    },
    C09_refuses_missing_version_line = function() {
      run_files(no_version = "202")
      on.exit(run_files())
      refuses("comparison apgv_fwd: .*NONMEM VERSION", cmps = CCMPS["apgv_fwd"])
    },
    C10_refuses_before_any_write = function() {
      # The last comparison fails, so nothing is written for the first ones.
      bad <- CCMPS; bad$wt_fixed$run <- "209"
      refuses("comparison wt_fixed: ", cmps = bad)
    },
    C11_compose_frozen_yaml_with_and_without = function() {
      without <- compose(two_runs(), fields = c("name", "command"))
      with <- compose(two_runs_c(), fields = c("name", "command"))
      stopifnot(identical(setdiff(with$name, without$name), "base_model_frozen_yaml"),
                all(without$name %in% with$name),
                identical(sort(compose(decl())$name), sort(B3A_NAMES)))
      for (n in without$name)
        stopifnot(identical(without$command[without$name == n] == with$command[with$name == n],
                            n != "base_model_package"))
      cmd <- with$command[with$name == "base_model_frozen_yaml"]
      stopifnot(!grepl("bm_fit_", cmd, fixed = TRUE), !grepl("bm_map_file", cmd, fixed = TRUE),
                grepl("nm_write_handoffs(", cmd, fixed = TRUE),
                grepl("dataset_file", cmd, fixed = TRUE),
                grepl("pilot/x/covariate", cmd, fixed = TRUE))
      e <- compose(two_runs_c(), fn = targets::tar_network, targets_only = TRUE,
                   outdated = FALSE)$edges
      stopifnot(setequal(e$from[e$to == "base_model_frozen_yaml"],
                         c("bm_res_201", "bm_res_202", "bm_ctl_201_file")))
      pkg <- with$command[with$name == "base_model_package"]
      stopifnot(lengths(regmatches(pkg, gregexpr("base_model_frozen_yaml[", pkg,
                                                 fixed = TRUE))) == 3L)
      three <- compose(two_runs_c(CCMPS, exponent = EXPO_C), fields = c("name", "command"))
      pkg <- three$command[three$name == "base_model_package"]
      stopifnot(lengths(regmatches(pkg, gregexpr("base_model_frozen_yaml[", pkg,
                                                 fixed = TRUE))) == 9L)
      tt <- module(two_runs_c(), all = TRUE)
      fy <- tt[[which(vapply(tt, function(t) t$settings$name, "") == "base_model_frozen_yaml")]]
      stopifnot(identical(fy$settings$format, "file"))
    },
    C12_compose_refusals = function() {
      cc <- function(nm = "apgv_fwd", ...) {
        x <- CCMPS[nm]; y <- list(...)
        x[[1]][names(y)] <- y
        x[[1]] <- x[[1]][!vapply(x[[1]], is.null, logical(1))]
        x
      }
      bad <- list(
        "covariate_comparisons \\(apgv_fwd\\) unit" = two_runs_c(cc(unit = NULL)),
        "covariate_comparisons \\(apgv_fwd\\) unit" = two_runs_c(cc(unit = "scm")),
        "covariate_comparisons \\(apgv_fwd\\) test" = two_runs_c(cc(test = "sideways")),
        "covariate_comparisons \\(apgv_fwd\\) alpha" = two_runs_c(cc(alpha = NULL)),
        "covariate_comparisons \\(apgv_fwd\\) prior" = two_runs_c(cc(prior = NULL)),
        "covariate_comparisons \\(apgv_fwd\\) prior" = two_runs_c(cc(prior = 3)),
        "covariate_comparisons \\(apgv_fwd\\): .*compared with itself" =
          two_runs_c(cc(child = "201")),
        "covariate_comparisons \\(wt_ratio\\) band_upper" =
          two_runs_c(cc("wt_ratio", band_upper = NULL)),
        "covariate_comparisons \\(wt_ratio\\) wt_points" =
          two_runs_c(cc("wt_ratio", wt_points = "WT05")),
        "covariate_comparisons \\(wt_ratio\\) exponent" = two_runs_c(cc("wt_ratio")),
        "covariate_comparisons \\(wt_fixed\\) exponent_fixed" =
          two_runs_c(cc("wt_fixed", exponent_fixed = "0.75")),
        "covariate_comparisons: .*names" = two_runs_c(setNames(CCMPS["apgv_fwd"], "../x")),
        "covariate_comparisons \\(apgv_fwd\\): .*list" = two_runs_c(list(apgv_fwd = "201 vs 202")),
        "covariate_dir" = two_runs_c(covariate_dir = NULL),
        "covariate_priors" = two_runs_c(covariate_priors = list(PRIORS[[1]]["covariate"])))
      for (i in seq_along(bad)) stops(module(bad[[i]]), names(bad)[i])
      # The estimated exponent is read from the declared exponent row of the
      # same run; with it declared, wt_ratio composes.
      module(two_runs_c(cc("wt_ratio"), exponent = EXPO_C))
      stops(module(two_runs_c(cc("wt_ratio"), exponent = replace(EXPO_C, "run", "201"))),
            "covariate_comparisons \\(wt_ratio\\) exponent")
    },
    C13_yaml_numbers_round_trip = function() {
      x <- c(3.84, 1, 0.1 + 0.2, 1e5, -2.5e-7, 1234.56789012345)
      s <- env$nm_yaml_number(x)
      stopifnot(identical(unclass(s)[1:2], c("3.84", "1.0")))
      back <- yaml::yaml.load(yaml::as.yaml(list(v = s)))$v
      stopifnot(is.double(back), identical(back, x))
    },
    # Review F-1: a non-finite number is refused before any write, both from
    # the estimated exponent and from the weight constants.
    C14_refuses_non_finite_number = function() {
      for (bad in c(NA_real_, Inf)) {
        ch <- res_with("202", ofv = 95)
        p <- ch$results$params
        p[["estimate"]][p[["parameter"]] == "THETA1"] <- bad
        ch$results$params <- p
        refuses("comparison wt_ratio: .*non-finite or non-numeric", rl = c_res(ch))
        w <- WTC; w$value_kg[3] <- bad
        refuses("comparison wt_ratio: .*non-finite or non-numeric", wtc = w)
      }
    },
    # Review F-2: a covariate_effect run without its $ESTIMATION text is a stop,
    # never a null in the run configuration.
    C15_refuses_missing_estimation_text = function() {
      rl <- c_res()
      rl[[1]]$record$est <- NULL
      refuses("comparison wt_fixed: .*run 201 carries no recorded \\$ESTIMATION",
              cmps = CCMPS["wt_fixed"], rl = rl)
      rl[[1]]$record$est <- NA_character_
      refuses("comparison wt_fixed: .*run 201 carries no recorded \\$ESTIMATION",
              cmps = CCMPS["wt_fixed"], rl = rl)
    },
    # Review F-3 and F-5: two distinct version lines, and a version line whose
    # token is missing or not a version, are stops.
    C16_refuses_bad_version_lines = function() {
      on.exit(run_files())
      lst <- file.path(MR, "m", "202", "202.lst")
      for (v in list(c(VERSION_LINE, sub("7.5.1", "7.4.4", VERSION_LINE, fixed = TRUE)),
                     sub("7.5.1", "   ", VERSION_LINE, fixed = TRUE),
                     sub("7.5.1", "beta", VERSION_LINE, fixed = TRUE))) {
        writeLines(c("stub listing", v, "end"), lst)
        refuses("comparison apgv_fwd: .*NONMEM VERSION", cmps = CCMPS["apgv_fwd"])
      }
    },
    # Review F-5: a failed parent run and a failed covariate_effect run.
    C17_refuses_failed_parent_and_effect_run = function() {
      rl <- c_res()
      rl[[1]] <- res_with("201", heur(TRUE, FALSE))
      refuses("comparison apgv_fwd: .*run 201.*minimization_terminated", rl = rl)
      refuses("comparison wt_ratio: .*run 202.*minimization_terminated",
              cmps = CCMPS["wt_ratio"], rl = c_res(res_with("202", heur(TRUE, FALSE), ofv = 95)))
    }))

  # ---- Part Q (B5) ----
  # The shaping file is sourced into its own environment (parent: the part A
  # environment, which holds tools/nonmem_frames.R); while it is absent every
  # case below errors, which is the red state of the order of work.
  QX <- file.path(FX, "q-repo")
  QMODULE <- file.path(REPO, "pipeline", "tasks", "qualification.R")
  nq_file <- file.path(TOOLS, "nonmem_qualification.R")
  qenv <- new.env(parent = env)
  if (file.exists(nq_file)) sys.source(nq_file, envir = qenv)
  # Gate rendering for Q11, from the same tools files the pipeline sources.
  genv <- new.env(parent = globalenv())
  for (f in c("gate.R", "gate_package.R", "evidence_notes.R"))
    sys.source(file.path(TOOLS, f), envir = genv)
  # The mini-repository: the composer, both task modules, every top-level
  # tools/*.R and the four skills the branch loads, copied whole.
  q_copy <- function(rel) {
    dir.create(dirname(file.path(QX, rel)), recursive = TRUE, showWarnings = FALSE)
    stopifnot(file.copy(file.path(REPO, rel), file.path(QX, rel), overwrite = TRUE))
  }
  for (rel in c("pipeline/compose.R", "pipeline/tasks/base_model.R",
                "pipeline/tasks/data_assembly.R",
                "pipeline/tasks/qualification.R",
                file.path("tools", basename(Sys.glob(file.path(TOOLS, "*.R")))),
                unlist(lapply(c("diagnostics", "psn-simeval", "psn-uncertainty",
                                "nonmem-estimation"), function(s)
                  file.path("skills", s, list.files(file.path(REPO, "skills", s),
                                                    recursive = TRUE))))))
    q_copy(rel)
  base_files(QX, c(201, 202, 301))
  dir.create(file.path(QX, "m/201"))
  dir.create(file.path(QX, "q"))
  for (rel in c("m/201/201.lst", "m/201/201.ext", "q/201.mod", "q/201.lst", "q/201.ext"))
    writeLines("Synthetic composition input", file.path(QX, rel))
  writeLines("psn: {}", file.path(QX, "q/run-config.yaml"))
  approve_fixture_method(QX, "diagnostics", c("bootstrap", "bootstrap_agreement", "gof",
    "npde", "npde_panels", "npde_tests", "qualification_verdict", "vpc", "vpc_coverage", "vpc_strata"))
  approve_fixture_method(QX, "psn-simeval", "vpc")
  approve_fixture_method(QX, "psn-uncertainty", "bootstrap")
  approve_fixture_method(QX, "nonmem-estimation", "estimation")

  Q_NAMES <- c("qual_ctl_file", "qual_lst_file", "qual_ext_file", "qual_run_config_file",
               "qual_gof_reference", "qual_vpc_reference", "qual_vpc_strata_reference",
               "qual_vpc_coverage_reference", "qual_npde_panels_reference",
               "qual_npde_tests_reference", "qual_bootstrap_agreement_reference",
               "qual_verdict_reference", "qual_psn_vpc_reference",
               "qual_psn_bootstrap_reference", "qual_inputs", "qual_frame",
               "qual_estimation", "qual_npde_tests", "qual_npde_panels",
               "qual_ind_profiles", "qual_ebe_panels", "qual_vpc_run", "qual_vpc",
               "qual_bootstrap_run", "qual_bootstrap", "qual_verdicts",
               "qualification_package")
  Q_FILES <- c(Q_NAMES[1:14], "qual_npde_panels", "qual_ind_profiles", "qual_ebe_panels")
  # The fifteen table names of Section 8.1, in order.
  Q_TABLES <- c(
    "Battery members (registry-selected, MODEL_RISK medium)",
    "Final model inputs: the work-directory copies and their digests",
    "Verdict table (MAP 4.3.2 E1 to E6, Q1 to Q3)",
    "Estimation inputs to the verdict",
    "Final model parameters with bootstrap intervals",
    "Bootstrap replicate accounting (Q3)",
    "Unsuccessful bootstrap replicates by PsN flag category (Q3)",
    "VPC bin coverage, pooled time VPC (Q1)",
    "VPC bin coverage by subject weight tertile (Q1 drift evidence)",
    "VPC bin coverage by Apgar group (Q1 drift evidence)",
    "Weight tertile bands (quantile type 7)",
    "NPDE global tests (Q2)",
    "NPDE settings read back from the final run's $TABLE",
    "PsN invocations",
    "Run accounting (MAP Section 5)")
  FORBIDDEN <- c("SYNTHETIC-AUX", "SYNTHETIC-REVIEW", "run 101", "qual101", "nlmixr2")
  # Synthetic parameter map; its columns exist in the synthetic per-sample file.
  Q_PARAMS <- list(
    THETA1 = list(kind = "fixed", transform = "exp",
                  psn = list(block = "theta", position = 1, heading = "^THETA\\(1\\)")),
    THETA2 = list(kind = "fixed", transform = "exp",
                  psn = list(block = "theta", position = 2, heading = "^THETA\\(2\\)")),
    "OMEGA(1,1)" = list(kind = "iiv", transform = "identity",
                        psn = list(block = "omega", position = 1, heading = "^OMEGA\\(1,1\\)")),
    "OMEGA(2,1)" = list(kind = "other", transform = "identity",
                        psn = list(block = "omega", position = 2, heading = "^OMEGA\\(2,1\\)$")),
    "OMEGA(2,2)" = list(kind = "iiv", transform = "identity",
                        psn = list(block = "omega", position = 3, heading = NA_character_)),
    "SIGMA(1,1)" = list(kind = "other", transform = "identity",
                        psn = list(block = "sigma", position = 1, heading = "^SIGMA\\(1,1\\)")))
  Q_KINDS <- vapply(Q_PARAMS, `[[`, "", "kind")
  # A qualification block naming the fixture's run 201; `...` overrides or
  # (with NULL) removes a field.
  qblock <- function(...) {
    q <- list(engine = "nonmem", model_risk = "medium", final_run = "201",
              workdir = "q", out_dir = "q/outputs",
              source_lst = "m/201/201.lst", source_ext = "m/201/201.ext",
              seed = 20260927, replicates = list(vpc = 200, npde = 500, bootstrap = 500),
              ci = 0.95,
              criteria = list(rse_fixed_max_pct = 30, rse_iiv_max_pct = 50,
                              cond_no_max = 1000, shrink_flag_pct = 30,
                              vpc_coverage_min_pct = 80, npde_p_flag = 0.01,
                              boot_converged_min_pct = 80, boot_median_max_pct = 15),
              parameters = Q_PARAMS,
              vpc = list(psn_bin_rule = list(auto_bin = "auto"),
                         npctab_columns = c("ID", "MDV", "TIME", "DV"), evid = "MDV",
                         binning_pooled = list(method = "ntile", nbins = 9),
                         binning_strata = list(method = "ntile", nbins = 4),
                         observed_only = list()),
              gof = list(ids_per_page = 9, log_y = FALSE,
                         etas = c(ET1 = "OMEGA(1,1)", ET2 = "OMEGA(2,2)"),
                         ebe_labels = c(ET1 = "EBE, clearance", ET2 = "EBE, volume"),
                         ebe_cont_cov = c(WT = "Body weight (kg)"),
                         ebe_cat_cov = c(APGLOW = "Apgar group")),
              npde_engine = "NONMEM $TABLE NPDE")
    x <- list(...)
    q[names(x)] <- x
    q[!vapply(q, is.null, logical(1))]
  }
  # The module sourced as compose.R sources it.
  qmodule <- function(ti, all = FALSE) {
    me <- new.env(parent = qenv)
    me$task_input <- ti
    me$analysis_id <- "B5A-FIX"
    me$upstream_gate <- "gate_base_model"
    me$task_targets <- NULL
    old <- setwd(QX)
    on.exit(setwd(old), add = TRUE)
    sys.source(QMODULE, envir = me)
    if (all) return(me$task_targets)
    vapply(me$task_targets, function(t) t$settings$name, character(1))
  }
  cmd_text <- function(targets, name) {
    t <- targets[[which(vapply(targets, function(x) x$settings$name, "") == name)]]
    paste(deparse(t$command$expr[[1]]), collapse = " ")
  }
  # The full composition through compose_pipeline, in a fresh R process at the
  # mini-repository root (base model with runs 201 and 202, then the
  # qualification block).
  q_compose <- function(qb) {
    r <- callr::r(function(repo, base, qb) tryCatch({
      setwd(repo)
      for (f in list.files("tools", pattern = "\\.R$", full.names = TRUE)) source(f)
      source("pipeline/compose.R")
      p <- compose_pipeline("B5A-FIX", c("base_model", "qualification"),
                            inputs = list(base_model = base, qualification = qb))
      list(names = vapply(p, function(t) t$settings$name, ""),
           formats = vapply(p, function(t) t$settings$format, ""),
           commands = lapply(p, function(t) t$command$expr[[1]]),
           dominance = tryCatch(verify_gate_dominance(p), error = function(e) conditionMessage(e)))
    }, error = function(e) list(error = conditionMessage(e))),
    args = list(repo = QX, qb = qb,
                base = decl(runs = list(entry("201"), entry("202", based_on = "201")),
                            external_input = "d/data.csv")))
    if (!is.null(r$error)) stop("compose_pipeline: ", r$error)
    r
  }
  q_composed <- NULL
  composed <- function() {
    if (is.null(q_composed)) q_composed <<- q_compose(qblock())
    q_composed
  }

  # Synthetic PsN and run files. Every number is a test constant.
  HAZARD <- "OMEGA(2,1) clearance-volume covariance, then V"
  PAR_COLS <- c("THETA(1) tcl, log CL", "THETA(2) tv, log V", "OMEGA(1,1) IIV CL, variance",
                "OMEGA(2,1)", HAZARD, "SIGMA(1,1) proportional, variance")
  STUB_EST <- c(THETA1 = -5, THETA2 = 0.3, "OMEGA(1,1)" = 0.04, "OMEGA(2,1)" = 0.01,
                "OMEGA(2,2)" = 0.03, "SIGMA(1,1)" = 0.015)
  # The per-sample file in the parity layout (non-parameter columns in the
  # parity order, then the parameter columns, then decoy se columns) and its
  # raw_results_structure. `n` sample rows follow the original-fit row.
  write_raw <- function(dir, n, structure = NULL, dup_name = FALSE) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    set.seed(7)
    rows <- n + 1L
    fail <- c(FALSE, seq_len(n) %% 50L == 0L)
    df <- data.frame(model = 0:n, problem = 1, subproblem = 1, covariance_step_run = 0,
                     minimization_successful = as.integer(!fail),
                     covariance_step_successful = 0, covariance_step_warnings = 0,
                     estimate_near_boundary = 0, rounding_errors = as.integer(fail),
                     zero_gradients = 0, final_zero_gradients = 0, hessian_reset = 0,
                     s_matrix_singular = 0, significant_digits = 3.2,
                     condition_number = NA_real_, est_methods = "FOCE_I",
                     model_run_time = "0:00:01", subprob_est_time = 0.1,
                     subprob_cov_time = 0, ofv = 100 + stats::rnorm(rows),
                     check.names = FALSE, stringsAsFactors = FALSE)
    for (i in seq_along(PAR_COLS))
      df[[PAR_COLS[i]]] <- STUB_EST[[i]] * (1 + 0.05 * stats::rnorm(rows))
    for (i in seq_along(PAR_COLS)) df[[paste0("se", PAR_COLS[i])]] <- 0.01
    if (dup_name) df <- cbind(df, stats::setNames(df[PAR_COLS[1]], PAR_COLS[1]))
    path <- file.path(dir, "raw_results_201.csv")
    utils::write.csv(df, path, row.names = FALSE, na = "")
    if (is.null(structure))
      structure <- c("[0]", "model=0,1", "problem=1,1", "ofv=19,1", "theta=20,2",
                     "omega=22,3", "sigma=25,1", "setheta=26,2", "", "[1]",
                     "model=0,1", "theta=20,2", "omega=22,3", "sigma=25,1")
    writeLines(structure, file.path(dir, "raw_results_structure"))
    writeLines(c("diagnostic.means", "\"\",\"x\"", "\"a\",1"),
               file.path(dir, "bootstrap_results.csv"))
    path
  }
  # A NONMEM table file of `blocks` (data frames), one TABLE NO. line each.
  npctab <- function(blocks, path, header = "ID MDV TIME DV") {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines(unlist(lapply(blocks, function(b) c(
      "TABLE NO.     1", paste0(" ", header),
      apply(as.matrix(b), 1, function(r) paste0(" ", paste(sprintf("%12.4E", r), collapse = " ")))))),
      path)
  }
  # The observed records of the end-to-end fixture: 12 subjects, a dose record
  # and four observations each.
  N_ID <- 12L
  q_obs <- function() {
    do.call(rbind, lapply(seq_len(N_ID), function(i) data.frame(
      ID = i, MDV = c(1, 0, 0, 0, 0), TIME = c(0, c(1, 2, 4, 8) + i / 10),
      DV = c(0, 10 * exp(-c(1, 2, 4, 8) / 10) * (1 + i / 50)))))
  }
  q_residuals <- function() {
    o <- q_obs()
    data.frame(NUM = seq_len(nrow(o)), ID = o$ID, TIME = o$TIME, EVID = o$MDV,
               C = ".", DV = o$DV, PRED = o$DV * 0.95, IPRED = o$DV * 1.02,
               CWRES = stats::qnorm(seq_len(nrow(o)) / (nrow(o) + 1)),
               TAD = o$TIME, NPDE = stats::qnorm(rev(seq_len(nrow(o))) / (nrow(o) + 1)),
               WT = 1 + o$ID / 10, APGLOW = o$ID %% 2,
               ET1 = (o$ID - 6) / 20, ET2 = (6 - o$ID) / 30, stringsAsFactors = FALSE)
  }
  q_params <- function() {
    data.frame(parameter = c("SIGMA(1,1)", "THETA1", "OMEGA(2,2)", "THETA2", "OMEGA(1,1)",
                             "OMEGA(2,1)"),
               estimate = unname(STUB_EST[c("SIGMA(1,1)", "THETA1", "OMEGA(2,2)", "THETA2",
                                            "OMEGA(1,1)", "OMEGA(2,1)")]),
               stderr = 0.01, rse_pct = c(11, 3, 25, 4, 60, 70),
               random_effect_sd = NA_real_, shrink_sd_pct = c(9, NA, 20, NA, 35, NA),
               fixed = FALSE, stringsAsFactors = FALSE)
  }
  q_heur <- function(mt = FALSE, aborted = FALSE)
    list(covariance_step_aborted = aborted, minimization_terminated = mt, prderr = FALSE)
  q_results <- function(h = q_heur())
    list(ofv = 100, n_id = N_ID, n_obs = 48L, params = q_params(), heuristics = h,
         residuals = q_residuals())
  Q_CN <- 12.5
  Q_STREAM <- STREAM
  # The end-to-end set-up in the mini-repository, keyed by store name; `n`
  # bootstrap sample rows (the declared 500, or one fewer for Q13).
  q_e2e <- function(store, n = 500L) {
    old <- setwd(QX)
    on.exit(setwd(old), add = TRUE)
    dir.create("m/201", recursive = TRUE, showWarnings = FALSE)
    dir.create("m/outputs", recursive = TRUE, showWarnings = FALSE)
    writeLines(Q_STREAM, "m/201.mod")
    writeLines(c("stub listing", "MINIMIZATION SUCCESSFUL"), "m/201/201.lst")
    writeLines(c("TABLE NO.     1: First Order Conditional Estimation",
                 " ITERATION    THETA1       THETA2       SIGMA(1,1)   OMEGA(1,1)   OBJ",
                 "  0  -5.0E+00  3.0E-01  1.5E-02  4.0E-02  100"), "m/201/201.ext")
    writeLines("%PDF-1.4 stub", "m/outputs/gof-201.pdf")
    dir.create("q", showWarnings = FALSE)
    for (f in c("201.mod", "201.lst", "201.ext"))
      stopifnot(file.copy(file.path("m", if (f == "201.mod") f else file.path("201", f)),
                          file.path("q", f), overwrite = TRUE))
    writeLines(c("psn:", "  nm_version: nm751", "  clean: 1", "  retries: 0", "  threads: 1"),
               "q/run-config.yaml")
    dir.create("fx/bin", recursive = TRUE, showWarnings = FALSE)
    saveRDS(qblock(), "fx/decl.rds")
    saveRDS(list(mod = structure(list(), class = "nq_fixture_mod"),
                 message = "MINIMIZATION SUCCESSFUL", meta = list(run = "201")),
            "fx/fit.rds")
    saveRDS(list(results = q_results()), "fx/res.rds")
    saveRDS(env$nm_budget_table(list(entry("201")), 0, 20), "fx/budget.rds")
    # PsN vpc outputs: the observed table and `vpc` simulated blocks.
    set.seed(11)
    o <- q_obs()
    npctab(list(o), "fx/vpc/m1/vpc_original.npctab.dta")
    npctab(lapply(seq_len(200), function(k)
      transform(o, DV = DV * exp(stats::rnorm(nrow(o), 0, 0.2)))),
      "fx/vpc/m1/vpc_simulation.1.npctab.dta")
    writeLines(c("VPC run info", "\"a\",1"), "fx/vpc/vpc_results.csv")
    write_raw(file.path("fx", paste0("bootstrap-", n)), n)
    # The fake executor: `psn -version` prints the version line; vpc and
    # bootstrap copy the synthetic outputs into the reserved -directory and
    # write PsN's command.txt and version_and_option_info.txt.
    writeLines(c(
      "#!/usr/bin/env Rscript",
      "a <- commandArgs(TRUE)",
      "if (identical(a[2], \"psn\")) { cat(\"PsN version: 5.7.1\\n\"); quit(status = 0) }",
      "rest <- a[-(1:2)]",
      "out <- file.path(a[1], sub(\"^-directory=\", \"\", grep(\"^-directory=\", rest, value = TRUE)))",
      sprintf("src <- file.path(%s, if (a[2] == \"vpc\") \"vpc\" else Sys.getenv(\"Q_BOOT_FIXTURE\"))",
              deparse(file.path(QX, "fx"))),
      "for (f in list.files(src, recursive = TRUE)) {",
      "  dir.create(dirname(file.path(out, f)), recursive = TRUE, showWarnings = FALSE)",
      "  file.copy(file.path(src, f), file.path(out, f))",
      "}",
      "writeLines(paste(c(a[2], rest), collapse = \" \"), file.path(out, \"command.txt\"))",
      "writeLines(c(\"PsN version: 5.7.1\", \"Run started: x\", \"Run finished: x\"),",
      "           file.path(out, \"version_and_option_info.txt\"))"),
      "tools/nonmem_exec.sh")
    Sys.chmod("tools/nonmem_exec.sh", "755")
    writeLines(c("#!/bin/sh", "echo v3.4.0"), "fx/bin/bbi")
    Sys.chmod("fx/bin/bbi", "755")
    script <- paste0("q-", store, ".R")
    writeLines(c(
      "library(targets)",
      "tar_source('tools')",
      "source('pipeline/compose.R')",
      "registerS3method('model_summary', 'nq_fixture_mod', function(.mod, ...)",
      sprintf("  list(condition_number = list(list(method = 'fixture', condition_number = %s))),", Q_CN),
      "  envir = asNamespace('bbr'))",
      "tar_option_set(format = 'rds')",
      "e <- new.env(parent = globalenv())",
      "e$task_input <- readRDS('fx/decl.rds')",
      "e$analysis_id <- 'B5A-FIX'",
      "e$upstream_gate <- NULL",
      "e$task_targets <- NULL",
      "sys.source('pipeline/tasks/qualification.R', envir = e)",
      "c(list(",
      "  tar_target_raw('bm_ctl_201_file', quote('m/201.mod'), format = 'file'),",
      "  tar_target_raw('bm_nm_reference', quote('skills/nonmem-estimation/reference/bbr_estimation.R'), format = 'file'),",
      "  tar_target_raw('bm_gof_201', quote('m/outputs/gof-201.pdf'), format = 'file'),",
      "  tar_target_raw('bm_fit_201', quote(readRDS('fx/fit.rds'))),",
      "  tar_target_raw('bm_res_201', quote(readRDS('fx/res.rds'))),",
      "  tar_target_raw('bm_budget', quote(readRDS('fx/budget.rds')))),",
      "  e$task_targets)"), script)
    old_path <- Sys.getenv("PATH")
    Sys.setenv(PATH = paste(file.path(QX, "fx", "bin"), old_path, sep = ":"),
               Q_BOOT_FIXTURE = paste0("bootstrap-", n))
    on.exit(Sys.setenv(PATH = old_path), add = TRUE)
    err <- tryCatch({
      targets::tar_make(script = script, store = store,
                        names = tidyselect::all_of("qualification_package"),
                        reporter = "silent")
      NULL
    }, error = function(e) e)
    list(store = file.path(QX, store), script = file.path(QX, script), error = err)
  }
  q_main <- NULL
  main <- function() {
    if (is.null(q_main)) q_main <<- q_e2e("_targets_q_main")
    if (!is.null(q_main$error)) stop("end-to-end run failed: ", conditionMessage(q_main$error))
    q_main
  }
  qread <- function(name) targets::tar_read_raw(name, store = main()$store)
  # A PsN vpc run directory for Q05.
  vpc_dir <- function(n_sim = 3L, rows = 3L, header_sim = "ID MDV TIME DV",
                      extra = FALSE, drop = NULL, no_table = FALSE) {
    d <- tempfile("vpcdir", tmpdir = FX)
    ob <- data.frame(ID = c(1, 1, 2), MDV = c(1, 0, 0), TIME = c(0, 1, 1), DV = c(0, 5, 6))
    npctab(list(ob), file.path(d, "m1", "vpc_original.npctab.dta"))
    sims <- lapply(seq_len(n_sim), function(k) {
      s <- ob[seq_len(if (k == n_sim) rows else 3L), ]
      s$DV <- 10 * k + seq_len(nrow(s))
      if (header_sim != "ID MDV TIME DV") s$NPDE <- 0
      s
    })
    npctab(sims, file.path(d, "m1", "vpc_simulation.1.npctab.dta"), header = header_sim)
    if (extra) npctab(sims, file.path(d, "m1", "vpc_simulation.2.npctab.dta"))
    if (!is.null(drop)) unlink(file.path(d, "m1", drop))
    if (no_table) {
      f <- file.path(d, "m1", "vpc_original.npctab.dta")
      writeLines(readLines(f)[-1], f)
    }
    d
  }
  COLS <- c("ID", "MDV", "TIME", "DV")

  cases <- c(cases, list(
    "qualification nonmem: Q01 composed names, formats, edges and splices" = function() {
      c1 <- composed()
      qn <- c1$names[grepl("^qual", c1$names) | c1$names == "qualification_package"]
      stopifnot(identical(qn, Q_NAMES),
                all(c("gate_qualification_file", "gate_qualification") %in% c1$names))
      fmt <- stats::setNames(c1$formats, c1$names)
      stopifnot(all(fmt[Q_FILES] == "file"), all(fmt[setdiff(Q_NAMES, Q_FILES)] != "file"))
      syms <- stats::setNames(lapply(c1$commands, all.names), c1$names)
      for (n in c("qual_inputs", "qual_frame", "qual_estimation")) {
        runs <- grep("^bm_(res|fit|ctl)_", syms[[n]], value = TRUE)
        stopifnot(length(runs) > 0L, all(runs %in% c("bm_res_201", "bm_fit_201", "bm_ctl_201_file")))
      }
      # The copy check: three nq_check_copy calls in qual_inputs, and both PsN
      # runs name qual_inputs.
      stopifnot(sum(syms[["qual_inputs"]] == "nq_check_copy") == 3L,
                "qual_inputs" %in% syms[["qual_vpc_run"]],
                "qual_inputs" %in% syms[["qual_bootstrap_run"]])
      stopifnot(all(setdiff(Q_NAMES, "qualification_package") %in% syms[["qualification_package"]]))
      for (n in c("qual_vpc_run", "qual_bootstrap_run")) {
        txt <- paste(deparse(c1$commands[[which(c1$names == n)]]), collapse = " ")
        for (bad in c("THETA", "OMEGA", "SIGMA", "criteria", "rse_fixed", "ntile", "nbins",
                      "observed_only", "npctab", "MDV", "ebe_", "ids_per_page", "Body weight"))
          if (grepl(bad, txt, fixed = TRUE)) stop(n, " splices ", bad)
      }
    },
    "qualification nonmem: Q02 gate dominance holds on the Q01 composition" = function()
      stopifnot(isTRUE(composed()$dominance)),
    "qualification nonmem: Q03 compose-time refusals name the field" = function() {
      fields <- c("engine", "model_risk", "final_run", "workdir", "out_dir", "source_lst",
                  "source_ext", "seed", "replicates", "ci", "criteria", "parameters", "vpc",
                  "gof", "npde_engine")
      for (f in setdiff(fields, "engine"))
        stops(qmodule(do.call(qblock, stats::setNames(list(NULL), f))), paste0("qualification: ", f))
      pset <- function(p, field, value) { x <- Q_PARAMS; x[[p]][[field]] <- value; x }
      psn <- function(p, field, value) { x <- Q_PARAMS; x[[p]]$psn[[field]] <- value; x }
      crit <- qblock()$criteria
      vp <- function(...) { v <- qblock()$vpc; x <- list(...); v[names(x)] <- x; v }
      gp <- function(...) { g <- qblock()$gof; x <- list(...); g[names(x)] <- x; g }
      bad <- list(
        list("qualification: gof: ", qblock(gof = gp(ids_per_page = 0))),
        list("qualification: gof\\$etas", qblock(gof = gp(etas = "OMEGA(1,1)"))),
        list("qualification: gof\\$ebe_labels", qblock(gof = gp(ebe_labels = c(ET1 = "a", ET3 = "b")))),
        list("qualification: vpc\\$binning_pooled", qblock(vpc = vp(binning_pooled = list(nbins = 9)))),
        list("qualification: vpc\\$binning_strata", qblock(vpc = vp(binning_strata = list(nbins = 4)))),
        list("qualification: vpc\\$evid", qblock(vpc = vp(evid = ""))),
        list("qualification: npde_engine", qblock(npde_engine = "")),
        list("qualification: out_dir", qblock(out_dir = "q/../outputs")),
        list("qualification: source_ext", qblock(source_ext = "/m/201/201.ext")),
        list("qualification: extra", qblock(extra = 1)),
        list("qualification: model_risk", qblock(model_risk = "high")),
        list("qualification: final_run", qblock(final_run = "2 01")),
        list("qualification: replicates.*vpc", qblock(replicates = list(vpc = 199, npde = 500, bootstrap = 500))),
        list("qualification: replicates.*npde", qblock(replicates = list(vpc = 200, npde = 499, bootstrap = 500))),
        list("qualification: replicates.*bootstrap", qblock(replicates = list(vpc = 200, npde = 500, bootstrap = 499))),
        list("qualification: seed", qblock(seed = 1.5)),
        list("qualification: ci", qblock(ci = 1)),
        list("qualification: criteria", qblock(criteria = crit[-1])),
        list("qualification: criteria", qblock(criteria = c(crit, list(extra_pct = 1)))),
        list("qualification: parameters.*kind", qblock(parameters = pset("THETA1", "kind", "random"))),
        list("qualification: parameters.*transform", qblock(parameters = pset("THETA1", "transform", "log"))),
        list("qualification: parameters", qblock(parameters = c(Q_PARAMS, Q_PARAMS[1]))),
        list("qualification: parameters.*fixed", qblock(parameters = Q_PARAMS[Q_KINDS != "fixed"])),
        list("qualification: parameters.*iiv", qblock(parameters = Q_PARAMS[Q_KINDS != "iiv"])),
        list("qualification: parameters.*block", qblock(parameters = psn("THETA1", "block", "eta"))),
        list("qualification: parameters.*position", qblock(parameters = psn("THETA1", "position", 0))),
        list("qualification: parameters.*position", qblock(parameters = psn("THETA1", "position", 1.5))),
        list("qualification: parameters.*block, position", qblock(parameters = psn("THETA2", "position", 1))),
        list("qualification: parameters.*heading", qblock(parameters = psn("THETA1", "heading", 1))),
        list("qualification: parameters.*heading", qblock(parameters = psn("THETA1", "heading", c("a", "b")))),
        list("qualification: vpc.*npctab_columns", qblock(vpc = vp(npctab_columns = character(0)))),
        list("qualification: vpc.*observed_only", qblock(vpc = vp(observed_only = list(AGE = 1)))),
        list("qualification: vpc.*observed_only", qblock(vpc = vp(observed_only = list(APGLOW = c(0, 1))))),
        list("qualification: vpc.*observed_only", qblock(vpc = vp(observed_only = list(APGLOW = list(0))))),
        list("qualification: vpc.*observed_only.*Q-B5-10", qblock(vpc = vp(observed_only = list(APGLOW = 1)))),
        list("qualification: vpc.*psn_bin_rule", qblock(vpc = vp(psn_bin_rule = list(auto_bin = "auto", no_of_bins = 5)))),
        list("qualification: workdir", qblock(workdir = "../q")),
        list("qualification: source_lst", qblock(source_lst = "m/../../x.lst")))
      for (b in bad) stops(qmodule(b[[2]]), b[[1]])
    },
    "qualification nonmem: Q04 the declared route requires the NONMEM engine" = function() {
      missing_engine <- qblock()
      missing_engine$engine <- NULL
      stops(qmodule(missing_engine), "engine must be explicitly nonmem")
      stopifnot(identical(qmodule(qblock()), Q_NAMES))
    },
    "qualification nonmem: Q05 nq_vpc_tables" = function() {
      t <- qenv$nq_vpc_tables(vpc_dir(), 3L, COLS)
      stopifnot(identical(names(t$sim), c(COLS, "REP")), identical(names(t$obs), COLS),
                nrow(t$obs) == 3L, nrow(t$sim) == 9L,
                identical(as.numeric(t$sim$REP), rep(c(1, 2, 3), each = 3)),
                all(t$sim$REP == floor(t$sim$DV / 10)))
      stops(qenv$nq_vpc_tables(vpc_dir(n_sim = 2L), 3L, COLS), "nq_vpc_tables")
      stops(qenv$nq_vpc_tables(vpc_dir(rows = 2L), 3L, COLS), "nq_vpc_tables")
      stops(qenv$nq_vpc_tables(vpc_dir(header_sim = "ID MDV TIME DV NPDE"), 3L, COLS), "nq_vpc_tables")
      stops(qenv$nq_vpc_tables(vpc_dir(extra = TRUE), 3L, COLS), "vpc_simulation.2")
      stops(qenv$nq_vpc_tables(vpc_dir(drop = "vpc_simulation.1.npctab.dta"), 3L, COLS), "nq_vpc_tables")
      stops(qenv$nq_vpc_tables(vpc_dir(no_table = TRUE), 3L, COLS), "TABLE NO")
    },
    "qualification nonmem: Q06 nq_attach_subject" = function() {
      df <- data.frame(ID = c(2, 1, 2), x = 1:3)
      sub <- data.frame(ID = c(1, 2), W = c("a", "b"), stringsAsFactors = FALSE)
      stopifnot(identical(qenv$nq_attach_subject(df, sub, "W")$W, c("b", "a", "b")))
      stops(qenv$nq_attach_subject(data.frame(ID = 3), sub, "W"), "nq_attach_subject")
      stops(qenv$nq_attach_subject(df, rbind(sub, sub), "W"), "nq_attach_subject")
      # An NA in the attached column stops (B5a code review L-3).
      sub_na <- data.frame(ID = c(1, 2), W = c("a", NA), stringsAsFactors = FALSE)
      stops(qenv$nq_attach_subject(df, sub_na, "W"), "nq_attach_subject: .*W.*NA")
    },
    "qualification nonmem: Q07 nq_estimation_contract and nq_engine_shrinkage" = function() {
      out <- qenv$nq_estimation_contract(q_results(), "MINIMIZATION SUCCESSFUL", Q_CN, Q_KINDS,
                                         run = "201")
      p <- q_params()
      i <- match(names(Q_KINDS), p$parameter)
      stopifnot(identical(names(out$params), c("parameter", "kind", "rse_pct", "shrink_sd_pct")),
                identical(out$params$parameter, names(Q_KINDS)),
                identical(out$params$kind, unname(Q_KINDS)),
                identical(out$params$rse_pct, p$rse_pct[i]),
                identical(out$params$shrink_sd_pct,
                          ifelse(Q_KINDS == "iiv", p$shrink_sd_pct[i], NA_real_) |> unname()),
                identical(out$minimization_successful, TRUE),
                identical(out$covariance_step_successful, TRUE),
                identical(out$condition_number, Q_CN),
                identical(out$termination_message, "MINIMIZATION SUCCESSFUL"))
      r <- q_results(q_heur(mt = TRUE, aborted = TRUE))
      out <- qenv$nq_estimation_contract(r, "m", NA_real_, Q_KINDS, run = "201")
      stopifnot(identical(out$minimization_successful, FALSE),
                identical(out$covariance_step_successful, FALSE))
      for (h in list(list(covariance_step = FALSE, minimization_terminated = FALSE),
                     list(covariance_step_aborted_extra = FALSE, minimization_terminated = FALSE),
                     list(covariance_step_aborted = NA, minimization_terminated = FALSE)))
        stops(qenv$nq_estimation_contract(q_results(h), "m", 1, Q_KINDS, run = "201"),
              "run 201.*covariance_step_aborted")
      stops(qenv$nq_estimation_contract(q_results(), "m", 1, c(Q_KINDS, THETA9 = "fixed"),
                                        run = "201"), "run 201.*THETA9")
      # A params frame without a column the contract reads stops, naming it.
      for (cl in c("rse_pct", "shrink_sd_pct")) {
        r <- q_results()
        r$params[[cl]] <- NULL
        stops(qenv$nq_estimation_contract(r, "m", 1, Q_KINDS, run = "201"),
              paste0("nq_estimation_contract \\(run 201\\): .*", cl))
      }
      stopifnot(identical(qenv$nq_engine_shrinkage(p, names(Q_KINDS)),
                          stats::setNames(p$shrink_sd_pct[i], names(Q_KINDS))))
    },
    "qualification nonmem: Q08 nq_condition_number on stub readers (Q-B5-8 (b))" = function() {
      rd <- function(cn) function(mod) {
        stopifnot(identical(mod, "MOD"))
        list(other = 1, condition_number = cn)
      }
      real <- list(list(method = "fixture", condition_number = Q_CN))
      stopifnot(identical(qenv$nq_condition_number("MOD", reader = rd(real)), Q_CN),
                identical(qenv$nq_condition_number("MOD", reader = rd(NULL)), NA_real_),
                identical(qenv$nq_condition_number("MOD", reader = function(mod) list(other = 1)),
                          NA_real_))
      for (bad in list(Q_CN, list(method = "fixture", condition_number = Q_CN), "12.5",
                       list(list(method = "fixture")), list(Q_CN),
                       list(list(method = "fixture", condition_number = "12.5")),
                       list(list(method = "fixture", condition_number = c(1, 2))), real[c(1, 1)],
                       list(list(method = "fixture", condition_number = Inf)),
                       list(list(method = "fixture", condition_number = NA_real_)),
                       list(list(method = "fixture", condition_number = NaN))))
        stops(qenv$nq_condition_number("MOD", reader = rd(bad)), "nq_condition_number")
    },
    "qualification nonmem: Q09 nq_check_copy, nq_check_parameters, nq_estimates" = function() {
      a <- tempfile("copy", tmpdir = FX); b <- tempfile("orig", tmpdir = FX)
      writeLines(c("x", "y"), a); writeLines(c("x", "y"), b)
      stopifnot(identical(qenv$nq_check_copy(a, b), digest::digest(file = b, algo = "sha256")))
      writeLines(c("x", "z"), a)
      stops(qenv$nq_check_copy(a, b), "nq_check_copy")
      stops(qenv$nq_check_copy(tempfile(tmpdir = FX), b), "nq_check_copy")
      qenv$nq_check_parameters(c("A", "B"), c("B", "A"))
      stops(qenv$nq_check_parameters(c("A", "B", "C"), c("B", "A")), "not in the final run: C")
      stops(qenv$nq_check_parameters(c("A"), c("B", "A")), "not declared: B")
      p <- q_params()
      stopifnot(identical(qenv$nq_estimates(p, c("THETA2", "SIGMA(1,1)")),
                          c(THETA2 = 0.3, "SIGMA(1,1)" = 0.015)),
                identical(qenv$nq_estimates(p, "OMEGA(1,1)", "rse_pct"), c("OMEGA(1,1)" = 60)))
      stops(qenv$nq_estimates(p, "THETA9"), "THETA9")
      stops(qenv$nq_estimates(rbind(p, p), "THETA1"), "THETA1")
    },
    "qualification nonmem: Q10 end to end through the approved units" = function() {
      pkg <- qread("qualification_package")
      v <- qread("qual_verdicts")
      stopifnot(nrow(v) == 19L)
      qv <- qread("qual_vpc")
      stopifnot(nrow(qv$coverage_weight) > 0L,
                all(qv$coverage_weight$stratum %in% qv$bands$summary$weight_band))
      tt <- qmodule(qblock(), all = TRUE)
      vc <- cmd_text(tt, "qual_verdicts")
      stopifnot(grepl("coverage_strat = qual_vpc$coverage_weight", vc, fixed = TRUE),
                !grepl("coverage_apgar", vc, fixed = TRUE))
      wt_row <- v$computed_value[v$criterion == "Q1 (coverage)"]
      stopifnot(grepl(sprintf("weight-stratified per-percentile min %.1f%%",
                              min(qv$coverage_weight$pct_within)), wt_row, fixed = TRUE))
      pt <- pkg$tables[["Final model parameters with bootstrap intervals"]]
      stopifnot(identical(pt$parameter, names(Q_PARAMS)),
                identical(pt$psn_column[pt$parameter == "OMEGA(2,2)"], HAZARD),
                identical(pt$psn_column[pt$parameter == "OMEGA(2,1)"], "OMEGA(2,1)"))
      for (p in names(Q_PARAMS)[vapply(Q_PARAMS, `[[`, "", "transform") == "exp"])
        stopifnot(identical(pt$estimate_natural_scale[pt$parameter == p], exp(STUB_EST[[p]])))
      b <- qread("qual_bootstrap")
      stopifnot(identical(b$fixed$parameter, c("THETA1", "THETA2")),
                identical(b$iiv$parameter, c("OMEGA(1,1)", "OMEGA(2,2)")))
      med <- b$summaries$median_natural[match(b$fixed$parameter, b$summaries$parameter)]
      nat <- exp(STUB_EST[b$fixed$parameter])
      stopifnot(isTRUE(all.equal(b$fixed$pct_diff_natural_scale,
                                 round(100 * abs(med - nat) / abs(nat), 2), check.attributes = FALSE)))
      stopifnot(identical(names(pkg$tables), Q_TABLES), anyDuplicated(names(pkg$tables)) == 0L)
      # Table 14 carries the declared replicate counts and seed as PsN's
      # command lines recorded them, and table 15 repeats the counts.
      inv <- pkg$tables[["PsN invocations"]]
      qb <- qblock()
      stopifnot(identical(inv$tool, c("vpc", "bootstrap")),
                identical(inv$samples, as.character(c(qb$replicates$vpc, qb$replicates$bootstrap))),
                identical(inv$seed, rep(as.character(qb$seed), 2L)),
                identical(pkg$tables[["Run accounting (MAP Section 5)"]]$value[3:4], inv$samples))
      art <- pkg$artifacts
      stopifnot(nrow(art) == 18L, anyDuplicated(art$path) == 0L, !any(startsWith(art$path, "/")),
                all(file.exists(file.path(QX, art$path))))
      ins <- qread("qual_inputs")
      stopifnot(identical(unname(ins$digests),
                          vapply(file.path(QX, c("m/201.mod", "m/201/201.lst", "m/201/201.ext")),
                                 function(f) digest::digest(file = f, algo = "sha256"), "",
                                 USE.NAMES = FALSE)))
      txt <- c(names(pkg$tables), unlist(lapply(pkg$tables, function(t) unlist(lapply(t, as.character)))),
               unlist(pkg$notes), unlist(lapply(art, as.character)))
      for (f in FORBIDDEN) if (any(grepl(f, txt, fixed = TRUE))) stop("generated text carries ", f)
    },
    "qualification nonmem: Q11 the package renders and verifies" = function() {
      pkg <- qread("qualification_package")
      gate <- file.path(QX, "gates", "B5A-FIX", "qualification.md")
      dir.create(dirname(gate), recursive = TRUE, showWarnings = FALSE)
      writeLines(c("# Qualification gate", "", "Analysis ID: B5A-FIX", "Stage: qualification",
                   "", "## SME decision", "", "- [ ] APPROVED", "- [ ] REVISE"), gate)
      old <- setwd(QX)
      on.exit(setwd(old))
      genv$render_gate_package(gate, pkg$artifacts, pkg$tables, pkg$notes)
      stopifnot(identical(genv$.gate_verify_content(gate, readLines(gate, warn = FALSE)),
                          character(0)))
    },
    "qualification nonmem: Q12 nq_psn_patterns picks by position" = function() {
      d <- tempfile("raw", tmpdir = FX)
      f <- write_raw(d, 3L)
      hdr <- names(utils::read.csv(f, check.names = FALSE, nrows = 1))
      maps <- lapply(Q_PARAMS, `[[`, "psn")
      s <- file.path(d, "raw_results_structure")
      p <- qenv$nq_psn_patterns(f, s, maps)
      stopifnot(identical(names(p), names(Q_PARAMS)),
                identical(unname(attr(p, "picked")), hdr[21:26]),
                identical(unname(attr(p, "picked")[["OMEGA(2,2)"]]), HAZARD),
                identical(unname(attr(p, "picked")[["OMEGA(2,1)"]]), "OMEGA(2,1)"),
                all(vapply(p, function(x) length(grep(x, hdr)), 1L) == 1L))
      bad_len <- tempfile("raw", tmpdir = FX)
      write_raw(bad_len, 3L, structure = c("[0]", "theta=20,2", "omega=22,2", "sigma=25,1"))
      stops(qenv$nq_psn_patterns(file.path(bad_len, "raw_results_201.csv"),
                                 file.path(bad_len, "raw_results_structure"), maps), "omega")
      m2 <- maps; m2$THETA1$heading <- "^THETA\\(9\\)"
      stops(qenv$nq_psn_patterns(f, s, m2), "THETA1")
      m3 <- maps; m3[["OMEGA(2,2)"]]$position <- 2
      stops(qenv$nq_psn_patterns(f, s, m3), "nq_psn_patterns: .*distinct")
      no0 <- tempfile("raw", tmpdir = FX)
      write_raw(no0, 3L, structure = c("[1]", "theta=20,2", "omega=22,3", "sigma=25,1"))
      stops(qenv$nq_psn_patterns(file.path(no0, "raw_results_201.csv"),
                                 file.path(no0, "raw_results_structure"), maps), "\\[0\\]")
      dup <- tempfile("raw", tmpdir = FX)
      write_raw(dup, 3L, dup_name = TRUE)
      stops(qenv$nq_psn_patterns(file.path(dup, "raw_results_201.csv"),
                                 file.path(dup, "raw_results_structure"), maps), "THETA1")
    },
    "qualification nonmem: Q13 one sample row fewer stops the accounting" = function() {
      r <- q_e2e("_targets_q_fewer", n = 499L)
      e <- targets::tar_meta(store = r$store, fields = c("name", "error"))
      msg <- e$error[e$name == "qual_bootstrap"]
      # A failure names which targets hold an error and what tar_make raised
      # (AC8): the case once failed with nothing but its condition.
      if (!(inherits(r$error, "error") && length(msg) == 1L &&
            grepl("do not reconcile", msg, fixed = TRUE))) {
        bad <- !is.na(e$error)
        stop(gsub("\\s*\n\\s*", " ", useBytes = TRUE, paste0(
          "tar_make error ", inherits(r$error, "error"), ", qual_bootstrap rows ",
          length(msg), "; targets with an error: ",
          if (any(bad)) paste(e$name[bad], e$error[bad], sep = ": ", collapse = " | ")
          else "none",
          "; tar_make said: ",
          if (inherits(r$error, "error")) conditionMessage(r$error) else "nothing")))
      }
    },
    "qualification nonmem: Q14 EBE panels carry the engine shrinkage per ETA" = function() {
      tt <- qmodule(qblock(), all = TRUE)
      t <- tt[[which(vapply(tt, function(x) x$settings$name, "") == "qual_ebe_panels")]]
      ev <- new.env(parent = qenv)
      ev$qual_frame <- qread("qual_frame")
      ev$bm_res_201 <- list(results = q_results())
      ev$qual_gof_reference <- "skills/diagnostics/reference/gof.R"
      got <- NULL
      ev$nm_write_panels <- function(panels, path) { got <<- panels; path }
      old <- setwd(QX)
      on.exit(setwd(old))
      eval(t$command$expr[[1]], ev)
      g <- qblock()$gof
      for (e in names(g$etas)) {
        want <- sprintf("%s (shrinkage %.1f%%)", g$ebe_labels[[e]],
                        q_params()$shrink_sd_pct[q_params()$parameter == g$etas[[e]]])
        for (cv in c("WT", "APGLOW"))
          stopifnot(identical(ggplot2::get_labs(got[[paste0(e, "_vs_", cv)]])$y, want))
      }
      stopifnot(file.exists(file.path(QX, qread("qual_ebe_panels"))))
    },
    "qualification nonmem: Q15 arithmetic census of the shaping file" = function() {
      pat <- "\\b(exp|log|sqrt|quantile|median|mean|sd|var|round|signif|sum|abs|max|min|prod|cumsum|cumprod|log10|log2|log1p|expm1|floor|ceiling|trunc|pmin|pmax|diff|range|rowSums|colSums|rowMeans|colMeans)\\("
      lines <- readLines(nq_file, warn = FALSE)
      hits <- grep(pat, lines, perl = TRUE)
      if (length(hits)) stop("census hits at line(s) ", paste(hits, collapse = ", "))
      # The operator census (V-Q6): outside strings and comments the only
      # arithmetic operator is the declared header offset `start + position`.
      pd <- utils::getParseData(parse(nq_file, keep.source = TRUE))
      pd <- pd[pd$terminal, ]
      ops <- pd$text[pd$token %in% c("'+'", "'-'", "'*'", "'/'", "'^'") |
                       (pd$token == "SPECIAL" & pd$text != "%in%")]
      if (!identical(ops, "+")) stop("operator census: ", paste(ops, collapse = " "))
    },
    "qualification nonmem: Q16 gate-binding helpers bind by name and refuse defects" = function() {
      root <- tempfile("q16", tmpdir = FX)
      dir.create(root)
      root <- normalizePath(root)
      old <- setwd(root)
      on.exit(setwd(old))
      # (a) nq_invocation_table on synthetic PsN run directories.
      run <- function(name, cmd = "vpc 201.mod -samples=7 -seed=99 -directory=x",
                      ver = c("PsN version: 5.7.1", "Run started: x")) {
        d <- file.path(root, "runs", name)
        dir.create(d, recursive = TRUE)
        if (!is.null(cmd)) writeLines(cmd, file.path(d, "command.txt"))
        if (!is.null(ver)) writeLines(ver, file.path(d, "version_and_option_info.txt"))
        list(run_dir = d)
      }
      inv <- qenv$nq_invocation_table(
        list(vpc = run("v"), bootstrap = run("b", cmd = "bootstrap 201.mod -seed=5 -samples=11")),
        list(nm_version = "nm751", clean = 1))
      stopifnot(identical(inv$tool, c("vpc", "bootstrap")),
                identical(inv$samples, c("7", "11")), identical(inv$seed, c("99", "5")),
                identical(inv$psn_version, c("5.7.1", "5.7.1")),
                identical(inv$run_dir, c("runs/v", "runs/b")),
                identical(inv$command[2], "bootstrap 201.mod -seed=5 -samples=11"),
                all(inv$operational == "nm_version=nm751; clean=1"),
                identical(qenv$nq_invocation_table(list(vpc = run("n")), list())$operational,
                          "none"))
      for (b in list(run("c1", cmd = NULL), run("c2", cmd = character(0)),
                     run("c3", cmd = "vpc 201.mod -samples=7"),
                     run("c4", cmd = "vpc 201.mod -seed=99"),
                     run("c5", cmd = "vpc 201.mod -samples=7 -seed=99 -seed=98"),
                     run("c6", ver = NULL), run("c7", ver = "Run started: x"),
                     run("c8", ver = c("PsN version: 5.7.1", "PsN version: 5.7.2"))))
        stops(qenv$nq_invocation_table(list(vpc = b), list()), "nq_invocation_table")
      # (b) nq_relative_path: inside the working directory gives the relative
      # path; outside, including a sibling whose name extends the root's, stops.
      dir.create(file.path(root, "in", "deep"), recursive = TRUE)
      writeLines("x", file.path(root, "in", "deep", "f.txt"))
      stopifnot(identical(qenv$nq_relative_path(file.path(root, "in", "deep", "f.txt")),
                          "in/deep/f.txt"),
                identical(qenv$nq_relative_path(c("in/deep/f.txt", "in")),
                          c("in/deep/f.txt", "in")))
      sib <- paste0(root, "x")
      dir.create(sib)
      writeLines("x", file.path(sib, "g.txt"))
      writeLines("x", file.path(FX, "q16-outside.txt"))
      for (p in list(file.path(sib, "g.txt"), file.path(FX, "q16-outside.txt"),
                     c("in/deep/f.txt", file.path(FX, "q16-outside.txt"))))
        stops(qenv$nq_relative_path(p), "nq_relative_path: .*outside")
      # (c) nq_battery_members against the registry and the copied manifests.
      setwd(QX)
      benv <- new.env(parent = qenv)
      for (f in c("method_manifest.R", "qualification.R"))
        sys.source(file.path(TOOLS, f), envir = benv)
      bm <- qenv$nq_battery_members
      environment(bm) <- benv
      mem <- function(units) list(units = units, artifact = "a.pdf", detail = "d")
      ok <- list(gof = mem("diagnostics/gof"), vpc = mem(c("psn-simeval/vpc", "diagnostics/vpc")),
                 npde = mem("diagnostics/npde_tests"), bootstrap = mem("psn-uncertainty/bootstrap"))
      t1 <- bm("medium", ok[c(4, 2, 1, 3)])
      stopifnot(identical(t1$member, c("gof", "vpc", "npde", "bootstrap")), all(t1$mandatory),
                identical(t1$units[2], "psn-simeval/vpc; diagnostics/vpc"),
                grepl("^psn-simeval/vpc: [^;]+; diagnostics/vpc: [^;]+$", t1$manifest_status[2]),
                identical(t1$artifact, rep("a.pdf", 4L)))
      stops(bm("medium", ok[-3]), "nq_battery_members: .*npde")
      stops(bm("medium", c(ok, list(pcvpc = mem("diagnostics/vpc")))), "nq_battery_members: .*pcvpc")
      no_unit <- ok
      no_unit$gof$units <- "diagnostics/no_such_unit"
      stops(bm("medium", no_unit), "nq_battery_members: no manifest status for unit diagnostics/no_such_unit")
      # (d) nq_bootstrap_tables binds by parameter name, in the kinds order.
      k <- c(A = "fixed", B = "iiv", C = "other", D = "fixed")
      rse <- c(D = 4, B = 2, A = 1)
      pdf_ <- c(D = 6, A = 5)
      cf <- c(C = TRUE, B = FALSE, A = TRUE, D = TRUE)
      bt <- qenv$nq_bootstrap_tables(k, rse, pdf_, cf, 99)
      stopifnot(identical(bt$fixed, data.frame(parameter = c("A", "D"), rse_pct = c(1, 4),
                                               pct_diff_natural_scale = c(5, 6),
                                               ci_finite = c(TRUE, TRUE))),
                identical(bt$iiv, data.frame(parameter = "B", rse_pct = 2, ci_finite = FALSE)),
                identical(bt$success_rate_pct, 99))
      stops(qenv$nq_bootstrap_tables(k, rse[-1], pdf_, cf, 99), "nq_bootstrap_tables: no rse for D")
      stops(qenv$nq_bootstrap_tables(k, rse, pdf_[-2], cf, 99), "nq_bootstrap_tables: no pct_diff for A")
      stops(qenv$nq_bootstrap_tables(k, rse, pdf_, cf[-2], 99), "nq_bootstrap_tables: no ci_finite for B")
      # (e) nq_parameter_table binds every column by parameter name.
      pars <- list(A = list(kind = "fixed", transform = "exp"),
                   B = list(kind = "iiv", transform = "identity"))
      sm <- data.frame(parameter = c("B", "A"), median_natural = c(20, 10),
                       ci_lower_natural = c(19, 9), ci_upper_natural = c(21, 11),
                       sd_estimation = c(0.2, 0.1))
      pt_args <- list(parameters = pars, estimate = c(B = 2, A = 1),
                      back_transformed = c(B = 2, A = 3), covariance_rse = c(B = 7, A = 6),
                      summaries = sm, bootstrap_rse = c(A = 8), ci_finite = c(B = FALSE, A = TRUE),
                      psn_column = c(B = "colB", A = "colA"))
      pt <- do.call(qenv$nq_parameter_table, pt_args)
      stopifnot(identical(pt$parameter, c("A", "B")), identical(pt$kind, c("fixed", "iiv")),
                identical(pt$transform, c("exp", "identity")),
                identical(pt$estimate_estimation_scale, c(1, 2)),
                identical(pt$estimate_natural_scale, c(3, 2)),
                identical(pt$rse_pct_covariance_step_estimation_scale, c(6, 7)),
                identical(pt$bootstrap_median_natural_scale, c(10, 20)),
                identical(pt$bootstrap_ci_lower_natural_scale, c(9, 19)),
                identical(pt$bootstrap_ci_upper_natural_scale, c(11, 21)),
                identical(pt$bootstrap_sd_estimation_scale, c(0.1, 0.2)),
                identical(pt$bootstrap_rse_pct_estimation_scale, c(8, NA_real_)),
                identical(pt$ci_finite, c(TRUE, FALSE)), identical(pt$psn_column, c("colA", "colB")))
      for (d in list(list("estimate", c(B = 2), "no estimate for A"),
                     list("summaries", sm[1, ], "no bootstrap summary for A"),
                     list("covariance_rse", c(A = 6), "no covariance-step RSE for B"),
                     list("psn_column", c(A = "colA"), "no PsN column for B"))) {
        a <- pt_args
        a[[d[[1]]]] <- d[[2]]
        stops(do.call(qenv$nq_parameter_table, a), paste0("nq_parameter_table: ", d[[3]]))
      }
    }))

  # ---- Part M (B6) ----
  # The shaping file is sourced into its own environment (parent: the part Q
  # environment); while it is absent, or while the module has no NONMEM arm,
  # every case below fails or errors, which is the red state of the order of
  # work. The trusted loader is sourced there too: the arm reads the exposure
  # unit's implemented metrics at compose time, as the closed arm does.
  ns_file <- file.path(TOOLS, "nonmem_simulation.R")
  SMODULE <- file.path(REPO, "pipeline", "tasks", "simulation.R")
  senv <- new.env(parent = qenv)
  sys.source(file.path(TOOLS, "load_reference.R"), envir = senv)
  if (file.exists(ns_file)) sys.source(ns_file, envir = senv)
  for (rel in c("pipeline/tasks/simulation.R", "tools/nonmem_simulation.R",
                file.path("skills", "simulation-standards",
                          list.files(file.path(REPO, "skills", "simulation-standards"),
                                     recursive = TRUE))))
    if (file.exists(file.path(REPO, rel))) q_copy(rel)
  approve_fixture_method(QX, "simulation-standards")

  # The seventeen module targets of Decision 5 under the B6-0a answers, in
  # compose order (no sim_exponent_ext_file, no sim_override_check); AC12
  # (2026-10-26) added the file target sim_comparison_reference.
  M_NAMES <- c("sim_translation_file", "sim_stream_file", "sim_listing_file", "sim_ext_file",
               "sim_mrgsolve_reference", "sim_exposure_reference", "sim_comparison_reference",
               "sim_source", "sim_design",
               "sim_scenarios", "sim_applicability", "sim_translation_check", "sim_results",
               "sim_exposure_metrics", "sim_bounds", "sim_figures", "simulation_package")
  M_FILES <- c(M_NAMES[1:7], "sim_figures")
  M_CLOSED <- c("sim_model_def_file", "sim_data_file", "sim_translation_file", "sim_fit",
                "sim_translation_check", "sim_applicability", "sim_results",
                "sim_exposure_metrics", "sim_figures", "simulation_package")
  # Section 9.1 headings, tables 1 to 11 without 4b (Q-B6-2 (c), Q-B6-8, Q-B6-10).
  M_TABLES <- c("Run record", "Final model source", "Parameters installed in the translation",
                "Translation equivalence check (E7)", "Weight points", "Scenario definitions",
                "Applicability against the observed design", "Applicability statements",
                "Exposure metrics", "Percentile against bound",
                "Bootstrap intervals for typical clearance and volume")
  # G2 (whole words) plus the Decision 18 names; G7 on column-name words.
  M_G2 <- "\\b(SYNTHETIC-EDA|SYNTHETIC-AUX|SYNTHETIC-REVIEW|sim_101|nlmixr2|oral|run 101|run 209|5th-95th)\\b"
  M_G7 <- "^(verdicts?|pass(ed|es)?|fail(ed|s)?)$"
  # AC12 (2026-10-26, plan D-7): G7 narrowed. The G7 words occur only as these
  # three exact column names of the table "Percentile against bound", computed
  # by the approved bound_comparison unit; the D-3 note replaces the old one.
  M_VERDICT_COLS <- c("verdict_lower", "verdict_upper", "verdict")
  M_BOUND_NOTE <- "computed by the approved bound_comparison unit"
  M_OLD_NOTE <- "the verdicts are the SME's"
  M_MAP <- list(
    THETA1 = list(kind = "theta", model = "THETA1", scale = "log; fixture"),
    THETA2 = list(kind = "theta", model = "THETA2", scale = "log; fixture"),
    "OMEGA(1,1)" = list(kind = "omega", model = 1, scale = "variance"),
    "OMEGA(2,1)" = list(kind = "omega", model = 2, scale = "covariance"),
    "OMEGA(2,2)" = list(kind = "omega", model = 3, scale = "variance"),
    "SIGMA(1,1)" = list(kind = "sigma", model = 1, scale = "variance"))
  # Synthetic .ext: the header lists SIGMA(1,1) before the OMEGA columns, as
  # the engine writes it. The values are the part Q test constants STUB_EST,
  # written as tokens; the 0 and -1000000001 rows carry other test constants.
  EXT_COLS <- c("THETA1", "THETA2", "SIGMA(1,1)", "OMEGA(1,1)", "OMEGA(2,1)", "OMEGA(2,2)")
  M_TOKENS <- sprintf("%.5E", STUB_EST[EXT_COLS])
  names(M_TOKENS) <- EXT_COLS
  ext_row <- function(it, v = STUB_EST)
    paste0("  ", it, "  ", paste(sprintf("%.5E", v[EXT_COLS]), collapse = "  "), "    100.5")
  EXT_LINES <- c("TABLE NO.     1: synthetic fixture",
                 paste(c(" ITERATION", EXT_COLS, "OBJ"), collapse = "    "),
                 ext_row("0", STUB_EST * 2), ext_row("-1000000000"),
                 ext_row("-1000000001", STUB_EST / 10))
  m_ext_file <- function(lines = EXT_LINES) {
    p <- tempfile("ext", tmpdir = FX, fileext = ".ext")
    writeLines(lines, p)
    p
  }
  M_WTREF <- 1.3   # fixture reference weight, kg
  M_WC <- data.frame(quantity = c("WT_ref: fixture median", "WT05: fixture low",
                                  "WT95: fixture high"),
                     value_kg = c(M_WTREF, 0.9, 2.6), stringsAsFactors = FALSE)
  M_DC <- data.frame(quantity = c("fixture first dose: minimum", "fixture first dose: maximum",
                                  "fixture later dose: minimum", "fixture later dose: maximum",
                                  "fixture horizon"),
                     value = c(8, 15, 1, 6, 48), stringsAsFactors = FALSE)
  # A simulation block naming the fixture's run 301; `...` overrides or
  # (with NULL) removes a field. Every number is a test constant.
  sblock <- function(...) {
    s <- list(
      engine = "nonmem",
      source = list(run = "301", stream = "sm/301.mod", listing = "sm/301/301.lst",
                    parameter_file = "sm/301/301.ext"),
      translation = "s/sim-301.cpp", parameter_map = M_MAP, covariates = c(WT = "WT"),
      reference_weight = list(param = "WTREF", constant = "WT_ref"),
      captures = list(observed = "CP", clearance = "CL", volume = "V"),
      tolerance = 1e-3, units = list(time = "h", amt = "mg", conc = "mg/L"),
      grid_doses = list(LD = c(10, 20), MD = c(2, 4)),
      weight_points = c(WT05 = "WT05", WT50 = "WT_ref", WT95 = "WT95"),
      apgar_groups = "APGREF", dose_cmt = 1, n = 500,
      grid = list(start = 0, end = 24, delta = 1), post_dose_obs = TRUE,
      uncertainty = "iiv", seed = 20260927, metrics = c("cmax", "auc_0_inf"),
      phase_metric = c(LD = "cmax", MD = "auc_0_inf"), summary_probs = c(0.1, 0.9),
      quantile_type = 7L, auc_tolerance = 1e-3,
      bounds = list(cmax = c(lower = 1, upper = 99), auc_0_inf = c(lower = 10, upper = 9999)),
      bound_units = c(cmax = "mg/L", auc_0_inf = "mg*h/L"),
      applicability = list(
        dose_rows = list(LD = c(min = "fixture first dose: minimum",
                                max = "fixture first dose: maximum"),
                         MD = c(min = "fixture later dose: minimum",
                                max = "fixture later dose: maximum")),
        horizon_row = "fixture horizon", approved_extrapolations = c(LD20 = "A9"),
        statements = c(route = "Fixture route statement.",
                       population = "Fixture population statement.")),
      bootstrap_rows = c(THETA1 = "fixture typical clearance",
                         THETA2 = "fixture typical volume"),
      sensitivity = list(), figures = list(bins = 30, ribbon = "fixture ribbon text"),
      out_dir = "s/outputs")
    x <- list(...)
    s[names(x)] <- x
    s[!vapply(s, is.null, logical(1))]
  }
  # The module sourced as compose.R sources it, from the repository root (the
  # arm loads the exposure unit at compose time).
  smodule <- function(ti, all = FALSE) {
    me <- new.env(parent = senv)
    me$task_input <- ti
    me$analysis_id <- "B6A-FIX"
    me$upstream_gate <- "gate_qualification"
    me$task_targets <- NULL
    old <- setwd(QX)
    on.exit(setwd(old))
    sys.source(SMODULE, envir = me)
    if (all) return(me$task_targets)
    vapply(me$task_targets, function(t) t$settings$name, character(1))
  }
  # The full composition through compose_pipeline in a fresh R process at the
  # mini-repository root: base model, the part Q qualification block, then
  # the simulation block.
  m_compose <- function(sb) {
    r <- callr::r(function(repo, base, qb, sb, da) tryCatch({
      setwd(repo)
      for (f in list.files("tools", pattern = "\\.R$", full.names = TRUE)) source(f)
      source("pipeline/compose.R")
      p <- compose_pipeline("B6A-FIX", c("data_assembly", "base_model", "qualification", "simulation"),
                            inputs = list(data_assembly = da, base_model = base, qualification = qb,
                                          simulation = sb))
      list(names = vapply(p, function(t) t$settings$name, ""),
           formats = vapply(p, function(t) t$settings$format, ""),
           commands = lapply(p, function(t) t$command$expr[[1]]),
           dominance = tryCatch(verify_gate_dominance(p), error = function(e) conditionMessage(e)))
    }, error = function(e) list(error = conditionMessage(e))),
    args = list(repo = QX, qb = qblock(), sb = sb,
                base = decl(runs = list(entry("201"), entry("202", based_on = "201"),
                                        entry("301", based_on = "202"))),
                da = list(source = "assembly.csv", spec = "assembly.yaml", map = "map.md",
                          expect = list(n_subjects = 2L, source_rows = 2L, lloq = NA_real_),
                          weights = list(baseline = "assembly.csv", column = "WT",
                                         probs = c(0.05, 0.95), quantile_type = 7L,
                                         labels = c("Median", "Low", "High")),
                          design_constants = "design.csv")))
    if (!is.null(r$error)) stop("compose_pipeline: ", r$error)
    r
  }
  writeLines(c("ID,WT", "1,1.2", "2,1.4"), file.path(QX, "assembly.csv"))
  writeLines("# Synthetic specification", file.path(QX, "assembly.yaml"))
  utils::write.csv(M_DC, file.path(QX, "design.csv"), row.names = FALSE)
  m_composed <- NULL
  mcomposed <- function() {
    if (is.null(m_composed)) m_composed <<- m_compose(sblock())
    m_composed
  }
  # The fixture translation: the parameter interface of Decision 8 under
  # Q-B6-2 (c) (THETA1, THETA2, WT, WTREF, a 2-by-2 OMEGA block, one SIGMA,
  # captures CP, CL, V); placeholders only, installed by ns_install.
  m_cpp <- function(cl_exponent = "0.75") paste(c(
    "$PROB synthetic fixture translation (battery part M)",
    "$THETA", "0", "0",
    "$PARAM WT = 1, WTREF = 1",
    "$CMT CENT",
    "$MAIN",
    "double LWT = log(WT / WTREF);",
    sprintf("double CL = exp(THETA1 + %s * LWT + ETA(1));", cl_exponent),
    "double V = exp(THETA2 + 1.0 * LWT + ETA(2));",
    "$ODE", "dxdt_CENT = -(CL / V) * CENT;",
    "$OMEGA @block", "0", "0 0",
    "$SIGMA", "0",
    "$TABLE", "double CP = CENT / V;",
    "$CAPTURE CP CL V"), collapse = "\n")
  # The fixture run's joined frame: six subjects, a bolus at time 0 and five
  # observations each; PRED is the one-compartment typical prediction of the
  # fixture translation at the written tokens (test arithmetic, not the
  # module's). Subject 1's C-flagged observation carries a PRED no model
  # gives, so E7 fails unless the exclusion holds.
  m_resid <- function() {
    th <- as.numeric(M_TOKENS[c("THETA1", "THETA2")])
    wt <- c(0.9, 1.1, 1.3, 1.6, 2, 2.6)
    tt <- c(0, 1, 4, 8, 12, 24)
    do.call(rbind, lapply(seq_along(wt), function(i) {
      cl <- exp(th[1]) * (wt[i] / M_WTREF)^0.75
      v <- exp(th[2]) * wt[i] / M_WTREF
      flag <- c(".", ".", ".", if (i == 1L) "C" else ".", ".", ".")
      pred <- c(0, 25 / v * exp(-cl / v * tt[-1]))
      pred[flag == "C"] <- 999
      data.frame(ID = i, TIME = tt, EVID = c(1, 0, 0, 0, 0, 0), AMT = c(25, 0, 0, 0, 0, 0),
                 C = flag, PRED = pred, WT = wt[i], stringsAsFactors = FALSE)
    }))
  }
  M_NOBS <- 29L
  M_QB <- list(parameter_table = data.frame(
    parameter = c("THETA1", "THETA2", "OMEGA(1,1)"), kind = c("fixed", "fixed", "iiv"),
    transform = c("exp", "exp", "identity"), estimate_natural_scale = c(0.0067, 1.35, 0.04),
    bootstrap_median_natural_scale = c(0.0068, 1.34, 0.041),
    bootstrap_ci_lower_natural_scale = c(0.006, 1.2, 0.02),
    bootstrap_ci_upper_natural_scale = c(0.0075, 1.5, 0.06),
    ci_finite = TRUE, stringsAsFactors = FALSE))
  # The end-to-end set-up in the mini-repository, keyed by store name.
  m_inputs <- function(sb = sblock(), cpp = m_cpp()) {
    old <- setwd(QX)
    on.exit(setwd(old), add = TRUE)
    for (d in c("sm/301", "s", "sx")) dir.create(d, recursive = TRUE, showWarnings = FALSE)
    writeLines(c("$PROBLEM synthetic fixture stream", "$INPUT ID TIME AMT DV WT"), "sm/301.mod")
    writeLines(c("synthetic listing",
                 "1NONLINEAR MIXED EFFECTS MODEL PROGRAM (NONMEM) VERSION 7.9.9"),
               "sm/301/301.lst")
    writeLines(EXT_LINES, "sm/301/301.ext")
    writeLines(cpp, sb$translation)
  }
  m_inputs()
  m_e2e <- function(store, sb = sblock(), cpp = m_cpp()) {
    m_inputs(sb, cpp)
    old <- setwd(QX)
    on.exit(setwd(old), add = TRUE)
    rds <- paste0("sx/decl-", store, ".rds")
    saveRDS(sb, rds)
    saveRDS(list(results = list(residuals = m_resid(), n_obs = M_NOBS)), "sx/res.rds")
    saveRDS(M_WC, "sx/wc.rds")
    saveRDS(M_DC, "sx/dc.rds")
    saveRDS(M_QB, "sx/qb.rds")
    script <- paste0("s-", store, ".R")
    writeLines(c(
      "library(targets)",
      "tar_source('tools')", "source('pipeline/compose.R')",
      "tar_option_set(format = 'rds')",
      "e <- new.env(parent = globalenv())",
      sprintf("e$task_input <- readRDS('%s')", rds),
      "e$analysis_id <- 'B6A-FIX'",
      "e$upstream_gate <- NULL",
      "e$task_targets <- NULL",
      "sys.source('pipeline/tasks/simulation.R', envir = e)",
      "c(list(",
      "  tar_target_raw('bm_res_301', quote(readRDS('sx/res.rds'))),",
      "  tar_target_raw('da_weight_constants', quote(readRDS('sx/wc.rds'))),",
      "  tar_target_raw('da_design_constants', quote(readRDS('sx/dc.rds'))),",
      "  tar_target_raw('qual_bootstrap', quote(readRDS('sx/qb.rds')))),",
      "  e$task_targets)"), script)
    err <- tryCatch({
      targets::tar_make(script = script, store = store,
                        names = tidyselect::all_of("simulation_package"), reporter = "silent")
      NULL
    }, error = function(e) e)
    list(store = file.path(QX, store), error = err)
  }
  m_main <- NULL
  smain <- function() {
    if (is.null(m_main)) m_main <<- m_e2e("_targets_m_main")
    if (!is.null(m_main$error)) {
      e <- tryCatch(targets::tar_meta(store = m_main$store, fields = c("name", "error")),
                    error = function(x) data.frame(name = character(0), error = character(0)))
      e <- e[!is.na(e$error), ]
      stop("end-to-end run failed: ", paste(e$name, e$error, sep = ": ", collapse = "; "))
    }
    m_main
  }
  sread <- function(name) targets::tar_read_raw(name, store = smain()$store)
  m_units <- function(unit = "mrgsolve_sim") {
    senv$load_reference("skills/simulation-standards",
      file.path("skills/simulation-standards/reference", paste0(unit, ".R")))
  }
  # A synthetic per-kilogram scenario list as ns_scenarios builds it.
  m_scen <- function()
    senv$ns_scenarios(list(LD = c(15, 20, 25, 30), MD = c(2, 3, 4, 5)),
                      c(WT05 = 0.9, WT50 = 1.3, WT95 = 2.6), "APGREF", n = 500,
                      grid = list(start = 0, end = 24, delta = 1), dose_cmt = 1,
                      post_dose_obs = TRUE, weight_param = "WT")

  cases <- c(cases, list(
    "simulation nonmem: M01 composed names, formats, edges and splices" = function() {
      c1 <- mcomposed()
      sn <- c1$names[grepl("^sim_", c1$names) | c1$names == "simulation_package"]
      stopifnot(identical(sn, M_NAMES),
                all(c("gate_simulation_file", "gate_simulation") %in% c1$names),
                !"sim_override_check" %in% c1$names, !"sim_exponent_ext_file" %in% c1$names)
      fmt <- stats::setNames(c1$formats, c1$names)
      stopifnot(all(fmt[M_FILES] == "file"), all(fmt[setdiff(M_NAMES, M_FILES)] != "file"))
      syms <- stats::setNames(lapply(c1$commands, all.names), c1$names)
      stopifnot(identical(unique(grep("^bm_", syms[["sim_design"]], value = TRUE)), "bm_res_301"),
                all(c(setdiff(M_NAMES, "simulation_package"), "qual_bootstrap") %in%
                      syms[["simulation_package"]]),
                all(c("sim_translation_check", "sim_applicability") %in% syms[["sim_results"]]),
                all(c("sim_comparison_reference", "reference_bound_verdict") %in%
                      syms[["sim_bounds"]]))
      txt <- paste(deparse(c1$commands[[which(c1$names == "sim_results")]]), collapse = " ")
      for (bad in c("9999", "Fixture route statement", "fixture ribbon text", "cmax",
                    "auc_0_inf", "fixture first dose", "A9", "quantile", "bins"))
        if (grepl(bad, txt, fixed = TRUE)) stop("sim_results splices ", bad)
      # The parameter_map's scale text (a table 3 label) is spliced into
      # simulation_package, not into sim_source (Decision 14, CR-2).
      cmd_txt <- function(n) paste(deparse(c1$commands[[which(c1$names == n)]]), collapse = " ")
      stopifnot(!grepl("log; fixture", cmd_txt("sim_source"), fixed = TRUE),
                !grepl("covariance", cmd_txt("sim_source"), fixed = TRUE),
                grepl("log; fixture", cmd_txt("simulation_package"), fixed = TRUE))
    },
    "simulation nonmem: M02 gate dominance holds on the M01 composition" = function()
      stopifnot(isTRUE(mcomposed()$dominance)),
    "simulation nonmem: M03 compose-time refusals name the field" = function() {
      fields <- names(sblock())
      for (f in setdiff(fields, "engine"))
        stops(smodule(do.call(sblock, stats::setNames(list(NULL), f))),
              paste0("simulation: ", f, ": "))
      pm <- function(p, field, value) { x <- M_MAP; x[[p]][[field]] <- value; x }
      ap <- function(...) { a <- sblock()$applicability; x <- list(...); a[names(x)] <- x; a }
      bad <- list(
        list("simulation: extra", sblock(extra = 1)),
        list("simulation: uncertainty", sblock(uncertainty = "full")),
        list("simulation: uncertainty", sblock(uncertainty = "none")),
        list("simulation: n: ", sblock(n = 499)),
        list("simulation: grid", sblock(grid = list(start = 1, end = 24, delta = 1))),
        list("simulation: grid_doses", sblock(grid_doses = list(LD = 10, MD = 2, XD = 1))),
        list("simulation: grid_doses", sblock(grid_doses = list(LD = numeric(0), MD = 2))),
        list("simulation: grid_doses", sblock(grid_doses = list(LD = c(20, 10), MD = 2))),
        list("simulation: parameter_map", sblock(parameter_map = pm("THETA1", "kind", "eta"))),
        list("simulation: parameter_map", sblock(parameter_map = pm("THETA2", "model", "THETA1"))),
        list("simulation: metrics", sblock(metrics = c("cmax", "auc_x"))),
        list("simulation: phase_metric",
             sblock(phase_metric = c(LD = "cmax", MD = "auc_0_tlast"))),
        list("simulation: bounds", sblock(bounds = list(cmax = c(lower = 5, upper = 5),
                                                        auc_0_inf = c(lower = 1, upper = 2)))),
        list("simulation: applicability", sblock(applicability = ap(
          approved_extrapolations = c(LD99 = "A9")))),
        list("simulation: applicability", sblock(applicability = ap(
          approved_extrapolations = c(LD20 = "B9")))),
        list("simulation: apgar_groups", sblock(apgar_groups = "low")),
        list("simulation: translation", sblock(translation = "s/../sim.cpp")),
        list("simulation: source", sblock(source = list(run = "301", stream = "../a.mod",
                                                         listing = "b", parameter_file = "c"))),
        list("simulation: out_dir", sblock(out_dir = "/abs/out")),
        list("simulation: weight_points", sblock(weight_points = c(WT05 = "WT05", WT95 = "WT95"))),
        list("simulation: weight_points", sblock(weight_points = c(WT05 = "WT_ref",
                                                                   WT50 = "WT_ref",
                                                                   WT95 = "WT95"))),
        list("simulation: covariates", sblock(covariates = c(BW = "BW"))),
        list("simulation: sensitivity", sblock(sensitivity = list(exponent = list()))),
        # One defect per remaining value guard (QA-1), each failing only its
        # own field.
        list("simulation: tolerance: ", sblock(tolerance = 1)),
        list("simulation: units: ", sblock(units = list(time = "h", amt = "mg"))),
        list("simulation: reference_weight: ", sblock(reference_weight = list(param = "WTREF"))),
        list("simulation: captures: ", sblock(captures = list(observed = "CP", clearance = "CL"))),
        list("simulation: dose_cmt: ", sblock(dose_cmt = 0)),
        list("simulation: post_dose_obs: ", sblock(post_dose_obs = NA)),
        list("simulation: seed: ", sblock(seed = 1.5)),
        list("simulation: summary_probs: ", sblock(summary_probs = c(0.9, 0.1))),
        list("simulation: quantile_type: ", sblock(quantile_type = 10L)),
        list("simulation: auc_tolerance: ", sblock(auc_tolerance = -1)),
        list("simulation: bound_units: ", sblock(bound_units = c(cmax = "mg/L"))),
        list("simulation: bootstrap_rows: ", sblock(bootstrap_rows = "fixture row")),
        list("simulation: figures: ", sblock(figures = list(bins = 0, ribbon = "fixture"))))
      for (b in bad) stops(smodule(b[[2]]), b[[1]])
    },
    "simulation nonmem: M04 a declared R model uses the no-engine route" = function() {
      writeLines(c("fixture_model <- function() NULL", "fixture_metadata <- list()"),
                 file.path(QX, "m04-model.R"))
      writeLines(c("ID,TIME,DV", "1,0,0"), file.path(QX, "m04-data.csv"))
      writeLines("fixture_translation <- NULL", file.path(QX, "m04-translation.R"))
      closed <- list(model = "m04-model.R", model_object = "fixture_model",
                     metadata_object = "fixture_metadata", data = "m04-data.csv", est = "focei",
                     refit_budget = list(used = 0, max = 1), translation = "m04-translation.R",
                     tolerance = 1e-3, uncertainty = "none", seed = 1, out_dir = "o",
                     scenarios = list(a = list(regimen = list(amt = 1, cmt = 1), n = 1,
                                               grid = list(start = 0, end = 1, delta = 1))),
                     units = list(time = "h", conc = "mg/L"), metrics = "cmax",
                     summary_probs = c(0.1, 0.9), quantile_type = 7, auc_tolerance = 1e-3)
      stopifnot(identical(smodule(closed), M_CLOSED), identical(smodule(sblock()), M_NAMES))
    },
    "simulation nonmem: M05 ns_ext_row selects by name and keeps the written tokens" = function() {
      v <- senv$ns_ext_row(m_ext_file(), names(M_MAP))
      stopifnot(identical(names(v), names(M_MAP)),
                identical(as.numeric(v), as.numeric(M_TOKENS[names(M_MAP)])),
                identical(attr(v, "written"), M_TOKENS[names(M_MAP)]))
      stops(senv$ns_ext_row(m_ext_file(c(EXT_LINES, EXT_LINES)), names(M_MAP)),
            "ns_ext_row: .*2 TABLE NO")
      stops(senv$ns_ext_row(m_ext_file(EXT_LINES[-4]), names(M_MAP)),
            "ns_ext_row: .*0 rows with ITERATION -1000000000")
      stops(senv$ns_ext_row(m_ext_file(c(EXT_LINES, EXT_LINES[4])), names(M_MAP)),
            "ns_ext_row: .*2 rows with ITERATION -1000000000")
      stops(senv$ns_ext_row(m_ext_file(), c(names(M_MAP), "THETA3")), "ns_ext_row: .*THETA3")
      stops(senv$ns_ext_row(m_ext_file(sub("ITERATION", "ITER", EXT_LINES)), names(M_MAP)),
            "ns_ext_row: .*begin ITERATION")
      stops(senv$ns_ext_row(file.path(FX, "no-such.ext"), names(M_MAP)),
            "ns_ext_row: .*does not exist")
    },
    "simulation nonmem: M06 ns_fitted_design keeps the run's own records" = function() {
      r <- data.frame(ID = c(1, 1, 1, 1, 2, 2, 2), TIME = c(0, 0, 1, 2, 0, 1, 2),
                      EVID = c(1, 1, 0, 0, 1, 0, 0), C = c(".", "C", ".", "C", ".", ".", "."),
                      AMT = c(10, 99, 0, 0, 20, 0, 0), PRED = c(0, 0, 5, 4, 0, 6, 5),
                      WT = c(1.1, 1.1, 1.1, 1.1, 2.2, 2.2, 2.2), stringsAsFactors = FALSE)
      d <- senv$ns_fitted_design(r, 3L, c(WT = "WT"), 1)
      ds <- d$design
      stopifnot(identical(d$n_dose_rows, 2L), identical(ds$AMT[ds$EVID == 1], c(10, 20)),
                all(ds$CMT[ds$EVID == 1] == 1), !99 %in% ds$AMT,
                identical(length(which(ds$EVID == 0)), 3L), all(ds$AMT[ds$EVID == 0] == 0),
                identical(ds$WT, c(1.1, 2.2, 1.1, 2.2, 2.2)),
                identical(names(d$pred), c("ID", "TIME", "PRED")),
                identical(d$pred$PRED, c(5, 6, 5)))
      stops(senv$ns_fitted_design(r, 4L, c(WT = "WT"), 1), "ns_fitted_design: 3 kept observation")
      stops(senv$ns_fitted_design(r, 3L, c(BW = "BW"), 1), "ns_fitted_design: .*BW")
      r2 <- r; r2$WT[6] <- NA
      stops(senv$ns_fitted_design(r2, 3L, c(WT = "WT"), 1), "ns_fitted_design: covariate WT")
      stops(senv$ns_fitted_design(r[r$EVID == 0, ], 3L, c(WT = "WT"), 1),
            "ns_fitted_design: no kept dose row")
    },
    "simulation nonmem: M07 ns_constant and ns_weight_points select by label" = function() {
      wc <- rbind(M_WC, data.frame(quantity = c("WT_refx: decoy", "WT10"), value_kg = c(7, 8)))
      stopifnot(identical(senv$ns_constant(wc, "WT_ref", "value_kg"), M_WTREF),
                identical(senv$ns_constant(wc, "WT10", "value_kg"), 8),
                identical(senv$ns_weight_points(wc, c(WT05 = "WT05", WT50 = "WT_ref",
                                                      WT95 = "WT95")),
                          c(WT05 = 0.9, WT50 = M_WTREF, WT95 = 2.6)))
      stops(senv$ns_constant(wc, "WT20", "value_kg"), "ns_constant: label WT20 selects 0 rows")
      stops(senv$ns_constant(rbind(wc, wc[2, ]), "WT05", "value_kg"),
            "ns_constant: label WT05 selects 2 rows")
    },
    "simulation nonmem: M08 ns_scenarios builds the declared grid" = function() {
      s <- m_scen()
      want <- paste(rep(c(paste0("LD", c(15, 20, 25, 30)), paste0("MD", 2:5)), each = 3),
                    c("WT05", "WT50", "WT95"), "APGREF", sep = "-")
      stopifnot(identical(names(s$scenarios), want), identical(s$table$SCENARIO, want),
                all(grepl("^(LD|MD)[0-9.]+-WT[0-9]{2}-APG[A-Z]+(-[A-Z0-9]+)?$", want)))
      wp <- c(WT05 = 0.9, WT50 = 1.3, WT95 = 2.6)
      for (i in seq_along(want)) {
        sc <- s$scenarios[[i]]
        stopifnot(is.null(sc[["regimen"]][["amt"]]),
                  identical(sc[["regimen"]][["amt_per_kg"]], s$table$dose_mg_per_kg[i]),
                  identical(sc[["regimen"]][["weight_param"]], "WT"),
                  identical(sc[["param"]][["WT"]], wp[[s$table$weight_point[i]]]),
                  identical(sc[["n"]], 500), isTRUE(sc[["post_dose_obs"]]))
      }
      stops(senv$ns_scenarios(list(LD = c(15, 15)), wp, "APGREF", 500, list(), 1, TRUE, "WT"),
            "ns_scenarios: duplicated")
      stops(senv$ns_scenarios(list(LD = 15), wp, "low", 500, list(), 1, TRUE, "WT"),
            "ns_scenarios: .*pattern")
      stops(senv$ns_scenarios(list(LD = 15), wp, "APGREF", 500, list(), 1, TRUE, "WT",
                              sensitivity = list(x = 1)), "ns_scenarios: sensitivity")
    },
    "simulation nonmem: M09 ns_applicability" = function() {
      rows <- sblock()$applicability$dose_rows
      tab <- data.frame(SCENARIO = c("LD10-WT05-APGREF", "LD20-WT05-APGREF", "MD2-WT05-APGREF"),
                        phase = c("LD", "LD", "MD"), dose_mg_per_kg = c(10, 20, 2),
                        stringsAsFactors = FALSE)
      app <- function(h = 24, approved = c(LD20 = "A9"))
        senv$ns_applicability(tab, h, M_DC, rows, "fixture horizon", approved)
      a <- app()
      dose <- a[a$dimension == "dose", ]
      stopifnot(nrow(a) == 6L, identical(dose$within_observed, c(TRUE, FALSE, TRUE)),
                identical(dose$approval, c("", "A9", "")),
                all(a$within_observed[a$dimension == "duration"]),
                identical(dose$observed_max, c(15, 15, 6)))
      stops(app(approved = character(0)), "ns_applicability: .*LD20-WT05-APGREF dose")
      stops(app(h = 72), "ns_applicability: .*duration")
      stops(app(approved = c(LD20 = "A9", LD10 = "A10")), "ns_applicability: approval .*LD10")
    },
    "simulation nonmem (C): M10 ns_install and ns_install_readback on the fixture translation" =
      function() {
        old <- setwd(QX)
        on.exit(setwd(old))
        ms <- m_units()
        mod <- ms$reference_sim_compile(m_cpp(), "m10_fixture")
        src <- senv$ns_source(senv$ns_ext_row(m_ext_file(), names(M_MAP)), "7.9.9", M_MAP,
                              c(WTREF = M_WTREF))
        mod <- senv$ns_install(mod, src)
        rb <- senv$ns_install_readback(mod, src)
        om <- as.matrix(mrgsolve::omat(mod, make = TRUE))
        stopifnot(nrow(rb) == 6L, all(rb$readback_identical),
                  identical(rb$model_target, c("THETA1", "THETA2", "omega[1,1]", "omega[2,1]",
                                               "omega[2,2]", "sigma[1,1]")),
                  identical(unname(om[2, 1]), unname(src$estimates[["OMEGA(2,1)"]])))
        swapped <- src
        swapped$map[["OMEGA(1,1)"]]$model <- 3
        swapped$map[["OMEGA(2,2)"]]$model <- 1
        stops(senv$ns_install_readback(mod, swapped),
              "ns_install_readback: .*OMEGA\\(1,1\\), OMEGA\\(2,2\\)")
        gap <- src
        gap$map[["OMEGA(2,2)"]]$model <- 4
        stops(senv$ns_install(mod, gap), "ns_install: the omega positions are not 1 to 3")
      },
    "simulation nonmem (C): M11 end to end through the approved units" = function() {
      pkg <- sread("simulation_package")
      sb <- sblock()
      sc <- sread("sim_scenarios")
      res <- sread("sim_results")
      n_sc <- length(sb$weight_points) * length(unlist(sb$grid_doses))
      stopifnot(length(sc$scenarios) == n_sc, identical(names(res$record$scenarios),
                                                        names(sc$scenarios)))
      for (nm in names(res$record$scenarios)) {
        a <- res$record$scenarios[[nm]][["regimen"]][["amt"]]
        stopifnot(is.numeric(a), length(a) == 1L, a > 0)
      }
      tc <- sread("sim_translation_check")
      stopifnot(identical(nrow(tc$readback), 6L), isTRUE(all(tc$readback$readback_identical)),
                tc$n == M_NOBS, is.finite(tc$max_rel_err))
      bt <- pkg$tables[["Percentile against bound"]]
      stopifnot(identical(bt$SCENARIO, names(sc$scenarios)),
                identical(bt$metric, unname(sb$phase_metric[sc$table$phase])))
      ap <- pkg$tables[["Applicability against the observed design"]]
      stopifnot(identical(ap$approval[ap$dimension == "dose"],
                          ifelse(grepl("^LD20-", names(sc$scenarios)), "A9", "")))
      stopifnot(identical(pkg$tables[["Bootstrap intervals for typical clearance and volume"]]$parameter,
                          c("THETA1", "THETA2")),
                identical(names(pkg$tables), M_TABLES), anyDuplicated(names(pkg$tables)) == 0L)
      art <- pkg$artifacts
      stopifnot(nrow(art) == 11L, anyDuplicated(art$path) == 0L, !any(startsWith(art$path, "/")),
                all(file.exists(file.path(QX, art$path))))
      txt <- c(names(pkg$tables), unlist(lapply(pkg$tables, function(t) unlist(lapply(t, as.character)))),
               unlist(pkg$notes), unlist(lapply(art, as.character)))
      hit <- grep(M_G2, txt, perl = TRUE, value = TRUE)
      if (length(hit)) stop("generated text carries a G2 word: ", hit[1])
      cols <- unlist(lapply(names(pkg$tables), function(h) {
        nm <- names(pkg$tables[[h]])
        if (identical(h, "Percentile against bound")) setdiff(nm, M_VERDICT_COLS) else nm
      }))
      words <- unlist(strsplit(cols, "_", fixed = TRUE))
      if (any(grepl(M_G7, words, ignore.case = TRUE))) stop("a column name carries a G7 word")
      if (any(grepl(M_OLD_NOTE, unlist(pkg$notes), fixed = TRUE)))
        stop("a note says the verdicts are the SME's")
      stopifnot(identical(utils::tail(names(bt), 3L), M_VERDICT_COLS),
                all(unlist(bt[M_VERDICT_COLS]) %in% c("PASS", "FAIL")))
      bu <- local({
        old <- setwd(QX)
        on.exit(setwd(old))
        m_units("bound_comparison")$reference_bound_verdict(
          bt$lower, bt$upper, bt$bound_lower, bt$bound_upper)
      })
      stopifnot(identical(bt[M_VERDICT_COLS], bu))
    },
    "simulation nonmem (C): M12 the package renders and verifies" = function() {
      pkg <- sread("simulation_package")
      gate <- file.path(QX, "gates", "B6A-FIX", "simulation.md")
      dir.create(dirname(gate), recursive = TRUE, showWarnings = FALSE)
      writeLines(c("# Simulation gate", "", "Analysis ID: B6A-FIX", "Stage: simulation",
                   "", "## SME decision", "", "- [ ] APPROVED", "- [ ] REVISE"), gate)
      old <- setwd(QX)
      on.exit(setwd(old))
      genv$render_gate_package(gate, pkg$artifacts, pkg$tables, pkg$notes)
      stopifnot(identical(genv$.gate_verify_content(gate, readLines(gate, warn = FALSE)),
                          character(0)))
    },
    "simulation nonmem (C): M13 E7 refusal end to end" = function() {
      r <- m_e2e("_targets_m_e7", sblock(translation = "s/sim-301-e7.cpp"),
                 cpp = m_cpp(cl_exponent = "0.5"))
      e <- targets::tar_meta(store = r$store, fields = c("name", "error"))
      msg <- e$error[e$name == "sim_translation_check"]
      stopifnot(inherits(r$error, "error"), length(msg) == 1L,
                grepl("Translation failed equivalence", msg, fixed = TRUE),
                !any(file.exists(file.path(r$store, "objects",
                                          c("sim_results", "simulation_package")))))
    },
    "simulation nonmem: M15 arithmetic and name census of the shaping file" = function() {
      pat <- "\\b(exp|log|sqrt|quantile|median|mean|sd|var|round|signif|sum|abs|max|min|prod|cumsum|cumprod|log10|log2|log1p|expm1|floor|ceiling|trunc|pmin|pmax|diff|range|rowSums|colSums|rowMeans|colMeans)\\("
      lines <- readLines(ns_file, warn = FALSE)
      hits <- grep(pat, lines, perl = TRUE)
      if (length(hits)) stop("census hits at line(s) ", paste(hits, collapse = ", "))
      # Operators outside strings and comments: only ggplot2's layer `+`, once
      # in each plot builder, on the line that starts the plot.
      pd <- utils::getParseData(parse(ns_file, keep.source = TRUE))
      pd <- pd[pd$terminal, ]
      op <- pd$token %in% c("'+'", "'-'", "'*'", "'/'", "'^'") |
        (pd$token == "SPECIAL" & pd$text != "%in%")
      if (!identical(pd$text[op], c("+", "+")) ||
          !all(grepl("ggplot2::ggplot(", lines[pd$line1[op]], fixed = TRUE)))
        stop("operator census: ", paste(pd$text[op], pd$line1[op], collapse = "; "))
      # Functions only, every name prefixed ns_ (no top-level code, no
      # operator definition).
      for (e in as.list(parse(ns_file, keep.source = FALSE)))
        if (!(is.call(e) && identical(e[[1L]], as.name("<-")) && is.name(e[[2L]]) &&
              startsWith(as.character(e[[2L]]), "ns_") && is.call(e[[3L]]) &&
              identical(e[[3L]][[1L]], as.name("function"))))
          stop("top-level expression is not an ns_ function definition: ",
               paste(deparse(e)[1L], collapse = ""))
    },
    "simulation nonmem: M16 ns_assert_record_amounts reads the exact field" = function() {
      s <- m_scen()$scenarios
      stops(senv$ns_assert_record_amounts(s), "ns_assert_record_amounts: scenario LD15-WT05-APGREF")
      echo <- s
      for (nm in names(echo)) echo[[nm]][["regimen"]][["amt"]] <- 20
      stopifnot(identical(senv$ns_assert_record_amounts(echo), echo))
      echo[[3]][["regimen"]][["amt"]] <- NA_real_
      stops(senv$ns_assert_record_amounts(echo), "ns_assert_record_amounts: scenario LD15-WT95")
    },
    "simulation nonmem: M18 sim_translation_check stops on a non-finite maximum relative error" =
      function() {
        tt <- smodule(sblock(), all = TRUE)
        t <- tt[[which(vapply(tt, function(x) x$settings$name, "") == "sim_translation_check")]]
        ev <- new.env(parent = senv)
        ev$load_reference <- function(...) invisible(NULL)
        ev$reference_sim_compile <- function(code, name) "MOD"
        ev$ns_read_text <- function(path) "text"
        ev$ns_install <- function(mod, source) mod
        ev$ns_install_readback <- function(mod, source) data.frame(readback_identical = TRUE)
        ev$sim_translation_file <- "t.cpp"
        ev$sim_mrgsolve_reference <- "r.R"
        ev$sim_source <- list()
        ev$sim_design <- list(design = 1, pred = 2, n_dose_rows = 3L)
        ev$reference_sim_translate_check <- function(...)
          list(n = 2L, max_rel_err = NaN, tolerance = 1e-3)
        stops(eval(t$command$expr[[1]], ev), "sim_translation_check: .*not finite")
        ev$reference_sim_translate_check <- function(...)
          list(n = 2L, max_rel_err = Inf, tolerance = 1e-3)
        stops(eval(t$command$expr[[1]], ev), "sim_translation_check: .*not finite")
        ev$reference_sim_translate_check <- function(...) list(n = 2L, tolerance = 1e-3)
        stops(eval(t$command$expr[[1]], ev), "sim_translation_check: .*not finite")
        ev$reference_sim_translate_check <- function(...)
          list(n = 2L, max_rel_err = 0, tolerance = 1e-3)
        out <- eval(t$command$expr[[1]], ev)
        stopifnot(identical(out$n_dose_rows, 3L), isTRUE(out$readback$readback_identical))
      },
    "simulation nonmem: M19 stop branches of ns_bind_runs, ns_bound_table, ns_bootstrap_rows, ns_source and ns_read_text" =
      function() {
        sc <- c("s1", "s2")
        rec <- list(scenarios = list(s1 = 1, s2 = 2), engine = "fixture", version = "0",
                    seed = 1, uncertainty = "iiv", observed = "CP")
        run <- list(sim = data.frame(SCENARIO = c("s1", "s1", "s2")), record = rec)
        stopifnot(identical(senv$ns_bind_runs(run, sc), run))
        stops(senv$ns_bind_runs(run, c("s1", "s3")), "ns_bind_runs: the run record's scenarios")
        r2 <- run
        r2$sim <- data.frame(SCENARIO = c("s2", "s1"))
        stops(senv$ns_bind_runs(r2, sc), "ns_bind_runs: the simulated frame's scenarios")
        r3 <- run
        r3$record$seed <- NULL
        stops(senv$ns_bind_runs(r3, sc), "ns_bind_runs: the run record has no seed")

        tab <- data.frame(SCENARIO = sc, kind = "grid", phase = c("LD", "MD"),
                          stringsAsFactors = FALSE)
        sm <- data.frame(SCENARIO = sc, metric = c("cmax", "auc_0_inf"), unit = "u",
                         p_lower = 0.1, lower = 1, median = 2, p_upper = 0.9, upper = 3,
                         stringsAsFactors = FALSE)
        pmet <- c(LD = "cmax", MD = "auc_0_inf")
        bnd <- list(cmax = c(lower = 5, upper = 50), auc_0_inf = c(lower = 7, upper = 70))
        bu <- c(cmax = "u1", auc_0_inf = "u2")
        bt <- senv$ns_bound_table(sm, tab, pmet, bnd, bu)
        stopifnot(identical(bt$metric, unname(pmet)), identical(bt$bound_lower, c(5, 7)),
                  identical(bt$bound_upper, c(50, 70)), identical(bt$bound_unit, c("u1", "u2")))
        stops(senv$ns_bound_table(sm[1, ], tab, pmet, bnd, bu),
              "ns_bound_table: 0 summary rows for s2 and auc_0_inf")
        stops(senv$ns_bound_table(rbind(sm, sm[1, ]), tab, pmet, bnd, bu),
              "ns_bound_table: 2 summary rows for s1 and cmax")

        pt <- M_QB$parameter_table
        br <- senv$ns_bootstrap_rows(pt, c(THETA2 = "fixture b", THETA1 = "fixture a"))
        stopifnot(identical(br$parameter, c("THETA2", "THETA1")),
                  identical(br$description, c("fixture b", "fixture a")))
        stops(senv$ns_bootstrap_rows(pt[setdiff(names(pt), "ci_finite")], c(THETA1 = "x")),
              "ns_bootstrap_rows: the parameter table has no column\\(s\\) ci_finite")
        stops(senv$ns_bootstrap_rows(pt, c(THETA9 = "x")), "ns_bootstrap_rows: row THETA9 appears 0")
        stops(senv$ns_bootstrap_rows(rbind(pt, pt[1, ]), c(THETA1 = "x")),
              "ns_bootstrap_rows: row THETA1 appears 2")

        est <- c(THETA1 = 1, THETA2 = 2)
        map <- M_MAP[c("THETA1", "THETA2")]
        s <- senv$ns_source(est, "7.9.9", map, c(WTREF = M_WTREF))
        stopifnot(identical(s$map, map), identical(s$reference_weight, c(WTREF = M_WTREF)))
        stops(senv$ns_source(est, "7.9.9", M_MAP, c(WTREF = M_WTREF)),
              "ns_source: the parameter map's engine names")
        stops(senv$ns_source(est, "", map, c(WTREF = M_WTREF)), "ns_source: version is not one")
        stops(senv$ns_source(est, "7.9.9", map, M_WTREF), "ns_source: reference_weight is not one")

        p <- tempfile("txt", tmpdir = FX)
        writeLines(c("line a", "line b"), p)
        stopifnot(identical(senv$ns_read_text(p), "line a\nline b"))
        stops(senv$ns_read_text(file.path(FX, "no-such.cpp")), "ns_read_text: .*does not exist")
      },
    "simulation nonmem: M20 simulation_package reads the exact amount and the spliced scale" =
      function() {
        tt <- smodule(sblock(), all = TRUE)
        cmd <- function(n)
          tt[[which(vapply(tt, function(x) x$settings$name, "") == n)]]$command$expr[[1]]
        ev <- new.env(parent = senv)
        ev$ns_ext_row <- function(ext, names) stats::setNames(as.numeric(seq_along(names)), names)
        ev$nm_nonmem_version <- function(listing) "7.9.9"
        ev$sim_ext_file <- "x.ext"
        ev$sim_listing_file <- "x.lst"
        ev$da_weight_constants <- M_WC
        src <- eval(cmd("sim_source"), ev)
        stopifnot(all(vapply(src$map, function(e) identical(names(e), c("kind", "model")),
                             logical(1))))
        sc <- c("LD10-WT05-APGREF", "MD2-WT05-APGREF")
        scen <- function(amt) lapply(stats::setNames(c(10, 2), sc), function(d)
          list(regimen = c(list(amt_per_kg = d, weight_param = "WT"), amt), n = 500))
        ev$sim_source <- src
        ev$sim_mrgsolve_reference <- "r.R"
        ev$sim_exposure_reference <- "e.R"
        ev$sim_comparison_reference <- "c.R"
        ev$sim_design <- list()
        ev$sim_stream_file <- "x.mod"
        ev$sim_translation_file <- "x.cpp"
        ev$sim_results <- list(record = list(engine = "fixture", version = "0", seed = 1,
                                             uncertainty = "iiv", observed = "CP",
                                             scenarios = scen(list(amt = 7))))
        ev$sim_scenarios <- list(table = data.frame(
          SCENARIO = sc, kind = "grid", phase = c("LD", "MD"), dose_mg_per_kg = c(10, 2),
          weight_point = "WT05", weight_kg = 0.9, apgar_group = "APGREF", overrides = "",
          stringsAsFactors = FALSE))
        ev$sim_translation_check <- list(
          readback = data.frame(engine_name = names(M_MAP), model_target = "t",
                                readback_identical = TRUE, stringsAsFactors = FALSE),
          n = 2L, n_dose_rows = 1L, max_rel_err = 0, tolerance = 1e-3)
        ev$sim_applicability <- data.frame(dimension = "dose")
        ev$sim_exposure_metrics <- list(summary = data.frame(metric = "cmax"),
                                        files = c("a.csv", "b.csv"))
        ev$sim_bounds <- list(table = data.frame(metric = "cmax"), file = "c.csv")
        ev$sim_figures <- "f.pdf"
        ev$qual_bootstrap <- M_QB
        ev$nq_relative_path <- function(path) path
        ev$authored_note <- function(text) text
        ev$evidence_note <- function(label, expr) label
        pkg <- eval(cmd("simulation_package"), ev)
        stopifnot(identical(pkg$tables[["Scenario definitions"]]$amount_formed_by_unit, c(7, 7)),
                  identical(pkg$tables[["Parameters installed in the translation"]]$scale,
                            unname(vapply(M_MAP, function(e) e[["scale"]], ""))))
        # AC12 (D-3): the note says the verdict columns are computed by the
        # approved unit, and the old note is gone.
        stopifnot(sum(grepl(M_BOUND_NOTE, unlist(pkg$notes), fixed = TRUE)) == 1L,
                  !any(grepl(M_OLD_NOTE, unlist(pkg$notes), fixed = TRUE)))
        # A record without the exact amt field: `$amt` would partially match
        # amt_per_kg and fill the amount column; the exact read must stop.
        ev$sim_results$record$scenarios <- scen(NULL)
        stops(eval(cmd("simulation_package"), ev), "length 1")
      },
    "simulation nonmem: M21 sim_bounds binds the verdict columns the unit returns" =
      function() {
        # The wiring only: the loader is called with the new file target, the
        # unit receives the table's four columns in order, its columns are bound
        # last and unchanged, and the CSV carries them. The unit's rule is
        # section W of test_batch7.R; the real unit on this path is M11 (C).
        sb <- sblock()
        tt <- smodule(sb, all = TRUE)
        cmd <- function(n)
          tt[[which(vapply(tt, function(x) x$settings$name, "") == n)]]$command$expr[[1]]
        # The declared out_dir is repository-relative, so the command runs in a
        # fresh directory under FX; it refuses any other place (destructive
        # fixtures never point at the live tree).
        wd <- file.path(FX, "m21")
        dir.create(wd, recursive = TRUE, showWarnings = FALSE)
        old <- setwd(wd)
        on.exit(setwd(old), add = TRUE)
        here <- normalizePath(getwd())
        tmp <- normalizePath(tempdir())
        if (!startsWith(here, paste0(tmp, "/")) || identical(here, REPO) ||
            startsWith(here, paste0(REPO, "/")))
          stop("M21: the working directory is not a disposable directory under tempdir()")
        calls <- new.env()
        calls$load <- list()
        calls$unit <- list()
        # Literal fixture verdicts: the stub compares nothing.
        verdicts <- data.frame(verdict_lower = c("PASS", "FAIL"), verdict_upper = c("FAIL", "PASS"),
                               verdict = c("FAIL", "FAIL"), stringsAsFactors = FALSE)
        ev <- new.env(parent = senv)
        ev$sim_comparison_reference <- "c.R"
        ev$load_reference <- function(skill_dir, ref, envir) {
          calls$load <- c(calls$load, list(ref))
          assign("reference_bound_verdict", function(lower, upper, bound_lower, bound_upper) {
            calls$unit <- c(calls$unit, list(list(lower, upper, bound_lower, bound_upper)))
            verdicts
          }, envir = envir)
          invisible(NULL)
        }
        # Two scenarios read by the real ns_bound_table. In each row the six
        # numbers p_lower, lower, bound_lower, p_upper, upper and bound_upper
        # are pairwise distinct (the bounds are sblock()'s: cmax 1 and 99,
        # auc_0_inf 10 and 9999), so passing the probabilities for the
        # percentiles, or swapping the bounds or the percentiles, changes what
        # the stub receives.
        sc <- c("LD10-WT05-APGREF", "MD2-WT05-APGREF")
        ev$sim_scenarios <- list(table = data.frame(SCENARIO = sc, kind = "grid",
                                                    phase = c("LD", "MD"),
                                                    stringsAsFactors = FALSE))
        ev$sim_exposure_metrics <- list(summary = data.frame(
          SCENARIO = sc, metric = c("cmax", "auc_0_inf"), unit = c("mg/L", "mg*h/L"),
          p_lower = c(0.1, 0.1), lower = c(5, 20), median = c(30, 300),
          p_upper = c(0.9, 0.9), upper = c(50, 500), stringsAsFactors = FALSE))
        res <- eval(cmd("sim_bounds"), ev)
        tab0 <- senv$ns_bound_table(ev$sim_exposure_metrics$summary, ev$sim_scenarios$table,
                                    sb$phase_metric, sb$bounds, sb$bound_units)
        # Detached assertion view: preserve the actual wiring fixture unchanged.
        m21_values <- tab0[c("p_lower", "lower", "bound_lower", "p_upper", "upper", "bound_upper")]
        # AC14-M21-ASSERTION
        stopifnot(all(apply(m21_values, 1L, function(row) anyDuplicated(row) == 0L)))
        stopifnot(length(calls$load) == 1L,
                  identical(calls$load[[1]], ev$sim_comparison_reference),
                  length(calls$unit) == 1L,
                  identical(calls$unit[[1]],
                            list(res$table$lower, res$table$upper, res$table$bound_lower,
                                 res$table$bound_upper)),
                  identical(calls$unit[[1]],
                            list(tab0$lower, tab0$upper, tab0$bound_lower, tab0$bound_upper)),
                  ncol(tab0) == 13L,
                  identical(names(res$table), c(names(tab0), M_VERDICT_COLS)),
                  identical(res$table[names(tab0)], tab0),
                  identical(res$table[M_VERDICT_COLS], verdicts))
        csv <- utils::read.csv(file.path(wd, res$file), stringsAsFactors = FALSE)
        stopifnot(ncol(csv) == 16L, identical(names(csv), names(res$table)),
                  identical(csv[M_VERDICT_COLS], verdicts))
      }))

  # ---- Part R ----
  # Use the shipped report sources inside a disposable declared project.
  RX <- file.path(FX, "r-repo")
  r_write <- function(rel, lines) {
    path <- file.path(RX, rel)
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines(lines, path, useBytes = TRUE)
    invisible(path)
  }
  r_copy <- function(source, dest = source) {
    dest <- file.path(RX, dest)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    stopifnot(file.copy(file.path(REPO, source), dest))
  }
  r_write(".here", character())
  for (rel in c("pipeline/compose.R", "pipeline/tasks/reporting.R", "tools/review_stage.R",
                "tools/engine_config.R", "tools/report_number_census.R",
                "tools/method_manifest.R", "tools/seed.R",
                "tools/evidence_notes.R", "tools/report_typography.R")) r_copy(rel)
  r_sections <- file.path("sections", c(
    "01-executive-summary.qmd", "02-introduction.qmd", "03-objectives.qmd",
    "04-data-and-methods.qmd", "05-results.qmd", "06-discussion.qmd",
    "07-conclusions.qmd", "08-appendices.qmd"))
  r_docs <- c("mar.qmd", "deck.qmd", r_sections)
  for (rel in r_docs) r_copy(file.path("templates/report", rel), file.path("rpt", rel))
  r_helpers <- c("sections/fixture-format.R", "sections/fixture-symbols.R")
  r_shared <- c("tools/evidence_notes.R", "tools/report_typography.R")
  for (rel in r_helpers) r_write(file.path("rpt", rel), "# Declared fixture helper")
  r_write("rpt/undeclared.R", "# This file has no report dependency")
  r_sources <- c(file.path("rpt", c(r_docs, r_helpers)), r_shared)
  r_id <- "REPORT-FIXTURE"
  r_stages <- c("data_assembly", "simulation")
  r_declaration <- list(report_dir = "rpt", store = "_targets_report_fixture",
                        document = c(author = "Fixture author", version = "2.1",
                                     date = "2026-10-04"),
                        packages = r_stages, helpers = r_helpers)
  for (stage in r_stages)
    r_write(file.path("pipeline/tasks", paste0(stage, ".R")), sprintf(
      "task_targets <- list(targets::tar_target_raw('%s_package', quote(list())))", stage))

  renv <- new.env(parent = genv)
  for (rel in c("pipeline/compose.R", "tools/review_stage.R", "tools/report_number_census.R",
                r_shared))
    sys.source(file.path(RX, rel), envir = renv)

  rtargets <- function(declaration = r_declaration, id = r_id) {
    me <- new.env(parent = renv)
    me$task_input <- declaration
    me$analysis_id <- id
    me$upstream_tasks <- r_stages
    me$task_targets <- NULL
    old <- setwd(RX)
    on.exit(setwd(old))
    sys.source("pipeline/tasks/reporting.R", envir = me)
    me$task_targets
  }
  r_targets <- NULL
  r_default_targets <- function() {
    if (is.null(r_targets)) r_targets <<- rtargets()
    r_targets
  }
  rcmd <- function(name, tt = r_default_targets())
    tt[[match(name, vapply(tt, function(t) t$settings$name, ""))]]$command$expr[[1L]]
  r_eval <- function(name, binds = list(), tt = r_default_targets()) {
    old <- setwd(RX)
    on.exit(setwd(old))
    eval(rcmd(name, tt), binds, enclos = renv)
  }
  # Reuse a single composer run for the positive cases that inspect its DAG.
  r_composed <- NULL
  rcomposed <- function() {
    if (!is.null(r_composed)) return(r_composed)
    saveRDS(r_declaration, file.path(RX, "report-declaration.rds"))
    r_write("report-network.R", c(
      "source('pipeline/compose.R')", "source('tools/review_stage.R')",
      "source('tools/engine_config.R')",
      "compose_pipeline('REPORT-FIXTURE', c('data_assembly', 'simulation', 'reporting'),",
      "                 list(reporting = readRDS('report-declaration.rds')))"))
    r_composed <<- callr::r(function(root) {
      setwd(root)
      p <- source("report-network.R")$value
      net <- targets::tar_network(script = "report-network.R", targets_only = TRUE,
                                  callr_function = NULL)
      list(names = vapply(p, function(t) t$settings$name, ""),
           formats = vapply(p, function(t) t$settings$format, ""),
           commands = lapply(p, function(t) t$command$expr[[1L]]),
           edges = net$edges, dominance = verify_gate_dominance(p))
    }, args = list(root = RX), timeout = 45)
    r_composed
  }
  r_has_edge <- function(from, to, net = rcomposed()$edges)
    any(net$from == from & net$to == to)
  r_render_call <- function(cmd) {
    if (identical(cmd[[1L]], quote(`{`))) cmd <- cmd[[length(cmd)]]
    as.list(cmd)
  }
  r_render_params <- function(cmd)
    eval(as.list(r_render_call(cmd)$args)$execute_params)

  # Synthetic sample and reference-estimate inputs for the retained methods.
  RF_EST <- c(THETA1 = -2.3, THETA2 = 0, "OMEGA(1,1)" = 0.04, "OMEGA(2,1)" = 0.01,
              "OMEGA(2,2)" = 0.03, "SIGMA(1,1)" = 0.015)
  RF_WTREF <- 1.3
  # The per-sample file follows the PsN layout. The extreme file has one
  # synthetic draw with an exaggerated volume parameter.
  rf_boot <- function(rel, extreme = FALSE) {
    d <- file.path(RX, rel)
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
    shift <- c(0, 0.02, -0.02, 0.01)
    df <- data.frame(model = 0:3, problem = 1, subproblem = 1, covariance_step_run = 0,
                     minimization_successful = c(1, 1, 0, 1), ofv = c(100, 101, 102, 99))
    for (i in seq_along(PAR_COLS)) df[[PAR_COLS[i]]] <- RF_EST[[i]] + shift
    if (extreme) df[[PAR_COLS[2]]][4] <- 800
    utils::write.csv(df, file.path(d, "raw_results_fx.csv"), row.names = FALSE)
    writeLines(c("[0]", "model=0,1", "problem=1,1", "ofv=5,1", "theta=6,2", "omega=8,3",
                 "sigma=11,1", "", "[1]", "model=0,1", "theta=6,2", "omega=8,3", "sigma=11,1"),
               file.path(d, "raw_results_structure"))
    writeLines(c("TABLE NO.     1: synthetic fixture",
                 paste(c(" ITERATION", EXT_COLS, "OBJ"), collapse = "    "),
                 ext_row("-1000000000", RF_EST)), file.path(d, "fx.ext"))
  }
  rf_ready <- FALSE
  rf_setup <- function() {
    if (rf_ready) return(invisible(TRUE))
    # Load the shipped forest and simulation reference methods.
    for (s in c("forest-plots", "simulation-standards"))
      for (rel in file.path("skills", s, list.files(file.path(REPO, "skills", s), recursive = TRUE))) {
        dir.create(dirname(file.path(RX, rel)), recursive = TRUE, showWarnings = FALSE)
        stopifnot(file.copy(file.path(REPO, rel), file.path(RX, rel), overwrite = TRUE))
      }
    for (skill in c("forest-plots", "simulation-standards")) approve_fixture_method(RX, skill)
    rf_boot("rfx/forest/boot")
    rf_boot("rfx/forest/boot-extreme", extreme = TRUE)
    rf_ready <<- TRUE
  }
  with_forest_methods <- function(expr) {
    rf_setup()
    old <- setwd(RX)
    on.exit(setwd(old))
    forest <- senv$load_reference("skills/forest-plots", "skills/forest-plots/reference/forest.R")
    eval(substitute(expr), list(forest = forest), enclos = parent.frame())
  }
  cases <- c(cases, list(
    "reporting nonmem: R01 the declared report composes behind both upstream stages" = function() {
      p <- rcomposed()
      expected <- c("report_source_files", "report_stage_packages", "report_code_inventory",
                    "report_mar", "report_deck", "report_semantic_check",
                    "report_semantic_manifest", "reporting_package")
      i <- match("gate_simulation", p$names)
      stopifnot(identical(p$names[seq.int(i + 1L, length(p$names))],
                          c(expected, "gate_reporting_file", "gate_reporting")),
                identical(unname(p$formats[match(c("report_source_files",
                  "report_semantic_manifest"), p$names)]), c("file", "file")),
                all(vapply(paste0(r_stages, "_package"), r_has_edge, logical(1),
                           to = "report_stage_packages")))
    },
    "reporting nonmem: R02 the upstream gate dominates every report target" = function() {
      p <- rcomposed()
      report_targets <- p$names[seq.int(match("gate_simulation", p$names) + 1L,
                                        match("gate_reporting_file", p$names) - 1L)]
      stopifnot(isTRUE(p$dominance), r_has_edge("gate_data_assembly", "gate_simulation"),
                all(vapply(report_targets, function(n) r_has_edge("gate_simulation", n),
                           logical(1))),
                r_has_edge("reporting_package", "gate_reporting"))
    },
    "reporting nonmem: R03 the declaration selects stages in declared order" = function() {
      decl <- r_declaration
      decl$packages <- rev(r_stages)
      a <- list(analysis = "assembly fixture", tables = list(Records = data.frame(n = 4L)))
      b <- list(analysis = "simulation fixture", artifacts = data.frame(path = "fixture.pdf"))
      binds <- list(data_assembly_package = a, simulation_package = b)
      stopifnot(identical(r_eval("report_stage_packages", binds,
                                rtargets(decl, id = "SECOND-REPORT-FIXTURE")),
                          list(simulation = b, data_assembly = a)))
      decl$packages <- "data_assembly"
      stopifnot(identical(r_eval("report_stage_packages", binds["data_assembly_package"],
                                rtargets(decl)), list(data_assembly = a)))
    },
    "reporting nonmem: R04 an arbitrary analysis ID reaches the declared reporting route" = function() {
      id <- "ANOTHER-REPORT-FIXTURE"
      tt <- rtargets(id = id)
      for (name in c("report_mar", "report_deck"))
        stopifnot(identical(r_render_params(rcmd(name, tt))$analysis_id, id))
      decl <- r_declaration
      decl$packages <- "qualification"
      stops(rtargets(decl, id), "reporting: packages: must name one or more unique preceding tasks")
    },
    "reporting nonmem: R05 declaration refusals name missing, invalid and unsupported fields" = function() {
      for (field in c("report_dir", "store", "document", "packages")) {
        decl <- r_declaration
        decl[[field]] <- NULL
        stops(rtargets(decl), paste0("reporting: ", field, ":"))
      }
      for (field in c("extra", "final_run", "reference_regimen", "deviation_log", "forest")) {
        decl <- r_declaration
        decl[[field]] <- "unsupported"
        stops(rtargets(decl), paste0("reporting: ", field, ": unsupported field"))
      }
      refuse <- function(field, values) {
        for (value in values) {
          decl <- r_declaration
          decl[[field]] <- value
          stops(rtargets(decl), paste0("reporting: ", field, ":"))
        }
      }
      refuse("store", list("", NA_character_, c("one", "two")))
      refuse("document", list(c(author = "Fixture", version = "1", date = "2026-02-30"),
                              c(author = "Fixture", version = "", date = "2026-10-04"),
                              c(author = "Fixture", date = "2026-10-04")))
      refuse("packages", list(character(), c("simulation", "simulation"), NA_character_))
      refuse("helpers", list(c(r_helpers[1L], r_helpers[1L]), "../outside.R", "/tmp/helper.R",
                             "", NA_character_))
      census <- list(patterns = "^known$", phrases = character(), literals = character(),
                     functions = "source")
      decl <- r_declaration
      decl$number_census <- census[names(census) != "functions"]
      stops(rtargets(decl), "reporting: number_census: must be a list with exactly")
      for (bad in list(list(patterns = "(unclosed"),
                       list(literals = "undeclared.R|text|1"), list(functions = "nrow"))) {
        decl$number_census <- utils::modifyList(census, bad)
        stops(rtargets(decl), paste0("reporting: number_census: ", names(bad), ":"))
      }
    },
    "reporting nonmem: R06 only existing declared project files reach report sources" = function() {
      stopifnot(identical(r_eval("report_source_files"), r_sources))
      for (name in c("undeclared-missing.R", paste0(r_helpers[1L], " "), "sections")) {
        decl <- r_declaration
        decl$helpers <- name
        stops(rtargets(decl), "reporting: helpers must name an existing project-relative file")
      }
      decl <- r_declaration
      decl$report_dir <- "rpt/../rpt"
      decl$helpers <- NULL
      stops(rtargets(decl), "reporting: report_dir must name an existing project-relative file")
      for (rel in r_docs[-length(r_docs)])
        r_copy(file.path("templates/report", rel), file.path("incomplete-report", rel))
      decl$report_dir <- "incomplete-report"
      stops(rtargets(decl), "reporting: report_dir must name an existing project-relative file")
      stopifnot(!file.exists(file.path(RX, "incomplete-report", tail(r_docs, 1L))))
    },
    "reporting nonmem: R07 promotion preserves archived typed notes and their order" = function() {
      evidence_count <- 4L
      notes <- list(renv$authored_note("The fixture reviewed the declared inputs."),
                    renv$evidence_note("Inputs reviewed", evidence_count),
                    "A second review confirmed the source.")
      pkgs <- r_eval("report_stage_packages", list(
        data_assembly_package = list(notes = notes[1:2]),
        simulation_package = list(notes = notes[3L])))
      actual <- unlist(lapply(pkgs, function(p) p$notes), recursive = FALSE, use.names = FALSE)
      stopifnot(identical(actual, notes),
                identical(renv$note_text(actual),
                          c("The fixture reviewed the declared inputs.",
                            "Inputs reviewed (from evidence_count): 4",
                            "A second review confirmed the source.")))
    },
    "reporting nonmem: R08 inventory records the declared source paths, byte sizes and digests" = function() {
      files <- r_eval("report_source_files")
      index <- r_eval("report_code_inventory", list(report_source_files = files))
      helper <- match("rpt/sections/fixture-format.R", files)
      expected_bytes <- nchar("# Declared fixture helper\n", type = "bytes")
      stopifnot(identical(names(index), c("file", "bytes", "md5")),
                identical(index$file, r_sources), is.integer(index$bytes),
                identical(index$bytes[helper], expected_bytes),
                identical(index$md5, unname(tools::md5sum(file.path(RX, r_sources)))),
                !anyNA(index$md5), is.character(genv$md_table(index)))
    },
    "reporting nonmem: R09 the semantic target refuses a report without archived notes" = function() {
      stops(r_eval("report_semantic_check", list(
        report_deck = "rpt/deck.pptx", report_source_files = r_sources,
        report_stage_packages = list(data_assembly = list(notes = list()),
                                     simulation = list(notes = NULL)))),
        "reporting: the selected packages carry no review notes")
    },
    "reporting nonmem: R10 the reporting package carries declared provenance and typed evidence" = function() {
      manifest <- "rpt/mar-semantic-manifest.txt"
      semantic <- list(summary = "PASS: fixture", n_notes = 3L, manifest_path = manifest)
      sources <- r_eval("report_source_files")
      index <- r_eval("report_code_inventory", list(report_source_files = sources))
      pkg <- r_eval("reporting_package", list(
        report_mar = c("rpt/mar.qmd", "rpt/mar.pdf"),
        report_deck = c("rpt/deck.qmd", "rpt/deck.pptx"),
        report_source_files = sources, report_semantic_manifest = manifest,
        report_semantic_check = semantic, report_code_inventory = index))
      stopifnot(identical(pkg$analysis, r_id), identical(pkg$task, "reporting"),
                identical(names(pkg$tables),
                          c("Promoted values and their sources",
                            "Code inventory (gate traceability; not rendered in the MAR)")),
                identical(pkg$tables[[1L]], data.frame(
                  target = rep("report_stage_packages", 2L),
                  source = c("data_assembly_package", "simulation_package"),
                  selection = rep("Stage package", 2L))),
                identical(pkg$tables[[2L]], index),
                identical(pkg$artifacts$path, c("rpt/mar.pdf", "rpt/deck.pptx", sources, manifest)),
                anyDuplicated(pkg$artifacts$path) == 0L, !any(startsWith(pkg$artifacts$path, "/")),
                identical(vapply(pkg$notes, function(n) n$kind, ""), c("evidence", "evidence")),
                identical(vapply(pkg$notes, function(n) n$expression_text, ""),
                          c("report_semantic_check$summary", "report_semantic_check$n_notes")),
                identical(vapply(pkg$notes, function(n) n$value_rendered, ""), c("PASS: fixture", "3")))
      for (table in pkg$tables) {
        stopifnot(!any(vapply(table, is.list, logical(1))))
        genv$md_table(table)
        words <- unlist(strsplit(names(table), "_", fixed = TRUE))
        stopifnot(!any(grepl("^(verdicts?|pass(ed|es)?|fail(ed|s)?)$", words, ignore.case = TRUE)))
      }
    },
    "reporting nonmem: R14 only declared and shared helpers are inputs to both renders" = function() {
      p <- rcomposed()
      cmd <- stats::setNames(p$commands, p$names)
      sources <- eval(cmd$report_source_files[[length(cmd$report_source_files)]])
      shared <- c(file.path("rpt", r_helpers), r_shared)
      mar <- r_render_call(cmd$report_mar)
      deck <- r_render_call(cmd$report_deck)
      stopifnot(identical(sources, r_sources),
                identical(as.list(mar$args)$input, "rpt/mar.qmd"),
                identical(as.list(deck$args)$input, "rpt/deck.qmd"),
                setequal(eval(mar$input), c(file.path("rpt", r_sections), shared)),
                setequal(eval(deck$input), shared))
    },
    "reporting nonmem: R15 the number census passes the clean fixture and stops on each mutation" =
      function() {
        fixture <- .r15_setup(REPO, file.path(FX, "public-r15"))
        .r15_collect(.r15_cases(fixture))
      },
    "reporting nonmem: R16 both renders receive declared document metadata and precede semantic approval" =
      function() {
        p <- rcomposed()
        cmd <- stats::setNames(p$commands, p$names)
        expected <- list(analysis_id = r_id, store = r_declaration$store,
                         date = r_declaration$document[["date"]])
        for (name in c("report_mar", "report_deck"))
          stopifnot(identical(r_render_params(cmd[[name]]), expected),
                    r_has_edge(name, "report_semantic_check"),
                    r_has_edge(name, "reporting_package"))
        stopifnot(r_has_edge("report_stage_packages", "report_semantic_check"),
                  r_has_edge("report_source_files", "report_semantic_check"),
                  r_has_edge("report_semantic_check", "report_semantic_manifest"),
                  r_has_edge("report_semantic_manifest", "reporting_package"))
      },
    "forest methods: RF03 the retained sample reader and final-estimate verifier" = function() {
      with_forest_methods({
        root <- "rfx/forest/boot"
        samples <- forest$reference_forest_samples(file.path(root, "raw_results_fx.csv"),
                                                   file.path(root, "fx.ext"))
        raw <- utils::read.csv(file.path(root, "raw_results_fx.csv"), check.names = FALSE)
        stopifnot(any(raw$minimization_successful == 0), any(raw$model == 0),
                  identical(nrow(samples), sum(raw$ofv != 0)))
        current <- function(draw, cov) draw[1:2]
        stopifnot(isTRUE(forest$reference_forest_verify(current, samples,
          unname(RF_EST[1:2]), df_ref = data.frame(WT = RF_WTREF))))
      })
    },
    "forest methods: RF04 swapped parameter placement fails the retained install check" = function() {
      with_forest_methods({
        model <- m_units()$reference_sim_compile(m_cpp(), "rf04_fixture")
        draw <- data.frame(THETA1 = RF_EST[["THETA1"]], THETA2 = RF_EST[["THETA2"]])
        install_draw <- function(mod, d) mrgsolve::param(mod, as.list(d), .strict = TRUE)
        model <- install_draw(model, draw)
        check <- m_units("uncertainty")$reference_sim_install_check
        stopifnot(check(model, draw, install_draw)$n > 0L)
        swapped <- function(mod, d) install_draw(mod, setNames(d[2:1], names(d)))
        stops(check(model, draw, swapped), "install_fn failed verification")
      })
    },
    "forest methods: RF05 missing-indicator weight cannot reach simulation" = function() {
      with_forest_methods({
        simulate <- m_units()$reference_simulate
        no_guard <- function(draw, cov) {
          simulate(stop("invalid weight reached simulation"), list(fixture = list(regimen = list(amt_per_kg = 2,
            weight_param = "WT", cmt = 1), param = list(WT = cov$WT))), seed = 42)
        }
        stops(forest$reference_forest_verify(no_guard, data.frame(theta = 1), 1,
                  df_ref = data.frame(WT = -99)), "'WT' must be a single finite positive number")
      })
    },
    "forest methods: RF06 an AUC without halving fails retained verification" = function() {
      with_forest_methods({
        samples <- data.frame(theta = c(3, 4))
        trapezoid <- function(draw, cov) sum(c(draw[1], draw[1] * 2)) / 2
        doubled <- function(draw, cov) 2 * trapezoid(draw, cov)
        ref <- data.frame(WT = RF_WTREF)
        stopifnot(isTRUE(forest$reference_forest_verify(trapezoid, samples, 4.5, df_ref = ref)))
        stops(forest$reference_forest_verify(doubled, samples, 4.5, df_ref = ref),
              "paramFunction failed verification")
      })
    },
    "forest methods: RF09 retained physical checks refuse invalid simulated draws" = function() {
      with_forest_methods({
        path <- "rfx/forest/boot-extreme/raw_results_fx.csv"
        before <- unname(tools::md5sum(path))
        check <- m_units()$reference_sim_physical_checks
        stopifnot(isTRUE(check(data.frame(CP = c(0, 2, 1)))))
        for (invalid in list(c(0, Inf), c(0, -1), c(0, 0)))
          stops(check(data.frame(CP = invalid)), "non-finite|negative|all-zero")
        stopifnot(identical(unname(tools::md5sum(path)), before))
      })
    }))

  # ---- Part V (B8) ----
  # The rv_ functions load into an environment above the gate tools; before
  # tools/review_stage.R exists every case stops at its first rv() lookup.
  RV_FILE <- file.path(TOOLS, "review_stage.R")
  RV_MODULE <- file.path(REPO, "pipeline", "tasks", "review.R")
  venv <- new.env(parent = genv)
  sys.source(file.path(REPO, "pipeline", "compose.R"), envir = venv)
  sys.source(file.path(TOOLS, "method_manifest.R"), envir = venv)
  if (file.exists(RV_FILE)) sys.source(RV_FILE, envir = venv)
  rv <- function(n) get(n, envir = venv, inherits = FALSE)
  v10_guard <- function(src, dst) {
    canon <- function(x) normalizePath(x, winslash = "/", mustWork = FALSE)
    src <- canon(src); dst <- canon(dst)
    tmp <- canon(tempdir()); fx <- canon(FX); repo <- canon(REPO)
    inside <- function(x, p) startsWith(x, paste0(p, "/"))
    if (!inside(fx, tmp) || !inside(src, fx) || !inside(dst, fx) ||
        identical(src, dst) || inside(src, dst) || inside(dst, src) ||
        identical(src, repo) || identical(dst, repo) || inside(src, repo) || inside(dst, repo))
      stop("recipe roots must be disjoint disposable fixture children")
    invisible(TRUE)
  }
  # The test copies use an argv-only rsync with explicit fixture exclusions.
  # A root-relative store glob preserves ordinary nested source.
  v_copy <- function(src, dst) {
    v10_guard(src, dst)
    dir.create(dst, recursive = TRUE, showWarnings = FALSE)
    exclude <- c(".git", "harness.yaml", ".mcp.json", ".token_seed", ".env",
                 ".harness-unlock", "software", "__pycache__", "*.pyc", "/_targets*")
    args <- c("-a", as.vector(rbind("--exclude", exclude)), paste0(src, "/"), paste0(dst, "/"))
    status <- system2("rsync", vapply(args, shQuote, ""), stdout = TRUE, stderr = TRUE)
    if (!is.null(attr(status, "status"))) stop("fixture copy failed: ", paste(status, collapse = "\n"))
  }
  v10_copy <- function(tag) {
    src <- file.path(FX, paste0("v10-recipe-", tag, "-src"))
    dst <- file.path(FX, paste0("v10-recipe-", tag, "-dst"))
    v10_guard(src, dst)
    ordinary <- c("pipeline/_targets_full_example.R", "tools/source.R")
    private <- c(".git/config", "harness.yaml", ".mcp.json", ".token_seed", ".env",
                 ".harness-unlock", "software/sentinel")
    omitted <- c("_targets/objects/sentinel", "_targets_fixture/sentinel",
                 "tools/standalone.pyc", "tools/__pycache__/payload.txt", private)
    for (rel in c(ordinary, omitted)) vw(src, rel, "fixture harmless bytes")
    v_copy(src, dst)
    list(src = src, dst = dst, private = private)
  }
  v10_equal_file <- function(f, rel) {
    a <- file.path(f$src, rel); b <- file.path(f$dst, rel)
    file.exists(b) && identical(readBin(a, "raw", n = file.info(a)$size),
                                readBin(b, "raw", n = file.info(b)$size))
  }
  v10_digest <- function(tag, rel, ignored) {
    root <- file.path(FX, paste0("v10-digest-", tag))
    # One bytecode form per independent fixture; no opposing filter can mask it.
    v10_guard(root, file.path(FX, paste0("v10-digest-", tag, "-unused")))
    vw(root, rel, "abcd")
    before <- rv("rv_tree_digest")(root, c("tools", "pipeline"))
    vw(root, rel, "abce") # Same-size ordinary source changes must remain visible.
    after <- rv("rv_tree_digest")(root, c("tools", "pipeline"))
    out <- file.path(FX, paste0("v10-digest-", tag, "-out"))
    err <- tryCatch({rv("rv_tree_check")(before, after, out, "copy-tree", "fixture"); NULL},
                    error = function(e) conditionMessage(e))
    if (ignored) stopifnot(!rel %in% before$path, identical(before, after), is.null(err), !dir.exists(out))
    else stopifnot(rel %in% before$path, !identical(before, after), !is.null(err),
                   file.exists(file.path(out, "copy-tree-before.tsv")),
                   file.exists(file.path(out, "copy-tree-after.tsv")))
  }
  V_WS <- "_targets_review_review_fixture"
  V_STAMP <- "re:(bootstrap|vpc)_run_[0-9]{8}T[0-9]{6}Z_[0-9a-f]{13,14}(?![0-9a-f])"
  V_VOL <- c(
    "re:(?:Mon|Tue|Wed|Thu|Fri|Sat|Sun) [A-Z][a-z]{2} +[0-9]{1,2} [0-9]{2}:[0-9]{2}:[0-9]{2} UTC [0-9]{4}" = "fixture timestamp",
    "re:(Current Date:) +[0-9]+ [A-Z]{3} [0-9]{4}$" = "fixture date",
    "re:(Days until program expires :) +[0-9]+$" = "fixture expiration",
    "re:(Elapsed +(?:estimation|covariance|postprocess|finaloutput) +time in seconds:) +[0-9]+[.][0-9]+" = "fixture runtime",
    "re:(#CPUT: Total CPU Time in Seconds,) +[0-9]+[.][0-9]+" = "fixture CPU time",
    "re:(Generated )[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2} UTC" = "fixture generation time",
    "re:(^created_at: )'[0-9TZ:-]+'$" = "fixture creation time",
    "re:(^<meta name=\"date\" content=\")[0-9]{4}-[0-9]{2}-[0-9]{2}(?=\" />$)" = "fixture HTML metadata",
    "re:(^<h[14] class=\"date\">)[0-9]{4}-[0-9]{2}-[0-9]{2}(?=</h[14]>$)" = "fixture HTML title date",
    "re:(^ +sha256: )[0-9a-f]{64}$" = "fixture digest",
    stats::setNames("fixture run stamp", V_STAMP))
  V_NAMES <- c("review_map_file", "review_assessment_file", "review_checkout_file",
               "review_gate_records", "review_gate_integrity", "review_traceability",
               "review_validation", "review_crossdoc", "review_reproduction",
               "review_findings", "review_package")
  vw <- function(root, rel, lines) {
    p <- file.path(root, rel)
    dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
    writeLines(lines, p)
    invisible(p)
  }
  # Sets (or, for NA, unsets) environment variables while `expr` runs.
  v_env <- function(vars, expr) {
    old <- Sys.getenv(names(vars), unset = NA, names = TRUE)
    set <- function(v) for (n in names(v)) if (is.na(v[[n]])) Sys.unsetenv(n) else
      do.call(Sys.setenv, stats::setNames(list(v[[n]]), n))
    on.exit(set(old), add = TRUE)
    set(vars)
    force(expr)
  }
  v_block <- function(...) {
    b <- list(assessment = "examples/review-fixture/assessment.md",
              map = "examples/review-fixture/plan.md", out_dir = "examples/review-fixture/review",
              store = "_targets_reviewed", checkout = "examples/review-fixture/checkout.txt",
              checkout_permitted = c("untracked:re:^gates/review_fixture/sign-" = "fixture signing bundles"),
              volatile = c(accessed_at = "fixture access stamp", V_VOL),
              expected_changes = c("analyses/review_fixture.R" = "fixture expected change"))
    utils::modifyList(b, list(...))
  }
  v_decl <- function(review = v_block(), reporting_store = "_targets_reviewed", da = list(),
                     id = "review_fixture") {
    inputs <- list(
      data_assembly = c(list(out_dir = "examples/review-fixture/da",
                             model_dir = "examples/review-fixture/model"), da),
      reporting = list(report_dir = "examples/review-fixture/report", store = reporting_store,
                       document = c(author = "Alex Rowan", date = "2026-10-04", version = "1"),
                       packages = "data_assembly", helpers = character()),
      review = review)
    c("library(targets)", "tar_source(\"tools\")", "source(\"pipeline/compose.R\")",
      "tar_option_set(format = \"rds\")",
      deparse(call("compose_pipeline", analysis_id = id, tasks = c("data_assembly", "reporting", "review"),
                   inputs = inputs), width.cutoff = 500L))
  }
  v_record <- function(id, stage, decision = "approved", date = "2026-10-01") {
    box <- function(d, label) paste0("- [", if (identical(decision, d)) "x" else " ", "] ", label)
    c("# Fixture record", paste0("Analysis ID: ", id), paste0("Stage: ", stage), "Date opened: 2026-10-01", "",
      box("approved", "APPROVED - stage complete"),
      box("approved_with_changes", "APPROVED WITH CHANGES - changes applied"),
      box("rejected", "REJECTED - returned to stage"), "",
      if (decision %in% c("approved", "rejected")) c("Signed: FX", paste0("Date: ", date)) else c("Signed:", "Date:"))
  }
  # The examined fixture root: the composer, the review module and every
  # top-level tools/*.R copied whole; a fixture data-assembly module with
  # three literal inputs and one computed file; the declaration, plan,
  # inputs, a generated run directory and one signed stage record.
  v_base <- function(x, decl = v_decl()) {
    for (rel in c("pipeline/compose.R", "pipeline/tasks/review.R", "pipeline/tasks/reporting.R",
                  file.path("tools", basename(Sys.glob(file.path(TOOLS, "*.R")))))) {
      dir.create(dirname(file.path(x, rel)), recursive = TRUE, showWarnings = FALSE)
      stopifnot(file.copy(file.path(REPO, rel), file.path(x, rel), overwrite = TRUE))
    }
    vw(x, "pipeline/tasks/data_assembly.R", c(
      "# Fixture producer module (battery part V): three literal inputs under the",
      "# declared locations and one computed file under out_dir.",
      "task_targets <- list(",
      "  targets::tar_target_raw('da_spec_file', call('identity', file.path(task_input$out_dir, 'spec.yaml')), format = 'file'),",
      "  targets::tar_target_raw('da_diff_file', call('identity', file.path(task_input$model_dir, 'x.diff')), format = 'file'),",
      "  targets::tar_target_raw('da_notes_file', call('identity', file.path(task_input$model_dir, 'notes')), format = 'file'),",
      "  targets::tar_target_raw('da_out', bquote({ da_spec_file; p <- .(file.path(task_input$out_dir, 'out.csv')); writeLines('x', p); p }), format = 'file'))"))
    vw(x, "analyses/review_fixture.R", decl)
    vw(x, "examples/review-fixture/plan.md", c("# Fixture plan", "", "## 6. Amendments and deviations", "",
                                      "D-1 recorded here.", "", "## 7. Open decisions", ""))
    vw(x, "examples/review-fixture/assessment.md", "- MAR: fixture/mar.pdf")
    report <- file.path(x, "examples/review-fixture")
    stopifnot(file.copy(file.path(REPO, "templates/report"), report, recursive = TRUE))
    vw(x, "examples/review-fixture/da/spec.yaml", "fixture: spec")
    vw(x, "examples/review-fixture/model/x.diff", "--- a/fixture.mod")
    vw(x, "examples/review-fixture/model/notes", "fixture notes")
    vw(x, "examples/review-fixture/model/201/201.lst", "a generated run file")
    vw(x, "gates/review_fixture/data-assembly.md", v_record("review_fixture", "data-assembly"))
    vw(x, "gates/review_fixture/reporting.md", v_record("review_fixture", "reporting"))
    vw(x, "gates/review_fixture/review.md", v_record("review_fixture", "review", decision = "draft"))
    vw(x, "examples/review-fixture/checkout.txt", "HEAD fixture")
    vw(x, ".gitignore", c("_targets_*/", "harness.yaml"))
    invisible(x)
  }
  # One examined root and one private copy per case (every setup needs a new
  # workspace). `edit(x)` changes the examined root before the copy; `store`
  # builds a fixture analysis store whose one computed file target writes
  # under out_dir; `git` is "repo" (a committed fixture repository), "plain"
  # (an empty .git directory) or "none"; `edit_copy(c)` changes the copy
  # after the receipt.
  # `store_extra` adds target lines (each starting with a comma) to the store.
  v_case <- function(tag, edit = NULL, git = "repo", harness = TRUE, store = FALSE,
                     edit_copy = NULL, decl = v_decl(), store_extra = character(0)) {
    x <- file.path(FX, paste0("vx-", tag)); cc <- file.path(FX, paste0("vc-", tag))
    v_base(x, decl)
    if (!is.null(edit)) edit(normalizePath(x))
    if (store) {
      vw(x, "_targets_fx.R", c("library(targets)", "list(tar_target_raw('da_out', quote({",
                               "  p <- 'examples/review-fixture/da/out.csv'; writeLines('x', p); p }), format = 'file')",
                               store_extra, ")"))
      (function() {
        old <- setwd(x)
        on.exit(setwd(old))
        targets::tar_make(script = "_targets_fx.R", store = "_targets_reviewed", reporter = "silent")
      })()
    } else {
      vw(x, "examples/review-fixture/da/out.csv", "x")
    }
    v_copy(x, cc)
    if (harness) file.create(file.path(cc, "harness.yaml"))   # an empty fixture file stands for it
    head <- strrep("0", 40)
    if (identical(git, "repo")) {
      g <- function(...) suppressWarnings(system2("git", c("-C", shQuote(cc), ...), stdout = TRUE, stderr = TRUE))
      g("init", "-q"); g("add", "--", ".gitignore")
      g("-c", "user.email=fixture@example.invalid", "-c", "user.name=Fixture", "-c", "commit.gpgsign=false", "commit", "-q", "-m", "fixture")
      head <- trimws(g("rev-parse", "HEAD"))[1]
    } else if (identical(git, "plain")) {
      dir.create(file.path(cc, ".git"))
    }
    receipt <- c(paste("HEAD", head), "status ?? gates/review_fixture/sign-fixture/",
                 "record gates/review_fixture/data-assembly.md commits 1 after-sign 0")
    vw(x, "examples/review-fixture/checkout.txt", receipt)
    vw(cc, "examples/review-fixture/checkout.txt", receipt)
    if (!is.null(edit_copy)) edit_copy(cc)
    list(x = normalizePath(x), c = normalizePath(cc), head = head)
  }
  V_LIVE <- file.path(FX, "v-live")
  dir.create(V_LIVE, showWarnings = FALSE)
  V_LIVE <- normalizePath(V_LIVE)
  v_setup <- function(case, live = case$x, dry = FALSE, copy = case$c, extra = c()) {
    old <- setwd(case$x)
    on.exit(setwd(old), add = TRUE)
    vars <- c(PMX_REVIEW_COPY = copy, PMX_REVIEW_REPRODUCTION = NA, REPO_ROOT = NA)
    vars[names(extra)] <- extra
    v_env(vars,
          rv("rv_reproduce_setup")("review_fixture", live_root = live, dry = dry))
  }
  v_files <- function(root) sort(list.files(root, recursive = TRUE, all.files = TRUE, include.dirs = FALSE))
  # Content digests by relative path, so a rewritten existing file is seen (m-8).
  v_digests <- function(root) { f <- v_files(root); stats::setNames(unname(tools::md5sum(file.path(root, f))), f) }
  # The successful setup (V05 and V18 share it): the .diff input holds the
  # live root's path (an inert-class fact); nothing else holds a root path.
  v_ok <- NULL
  vok <- function() {
    if (is.null(v_ok)) {
      case <- v_case("ok", edit = function(x) vw(x, "examples/review-fixture/model/x.diff",
                                                c("--- a/fixture.mod", paste0("+++ ", V_LIVE, "/fixture.mod"))),
                     store = TRUE)
      bx <- v_digests(case$x); bc <- v_digests(case$c)
      s <- v_setup(case, live = V_LIVE)
      v_ok <<- list(case = case, s = s, before_x = bx, after_x = v_digests(case$x), before_c = bc,
                    after_c = v_digests(case$c))
    }
    v_ok
  }
  # The review module sourced as compose.R sources it, from a fixture root.
  v_mroot <- NULL
  vmroot <- function() {
    if (is.null(v_mroot)) v_mroot <<- normalizePath(v_base(file.path(FX, "vm")))
    v_mroot
  }
  vmod <- function(ti, id = "review_fixture", all = FALSE, root = vmroot()) {
    me <- new.env(parent = venv)
    me$task_input <- ti
    me$analysis_id <- id
    me$upstream_gate <- "gate_reporting"
    me$upstream_tasks <- c("data_assembly", "reporting")
    me$task_targets <- NULL
    old <- setwd(root)
    on.exit(setwd(old))
    sys.source(RV_MODULE, envir = me)
    if (all) return(me$task_targets)
    vapply(me$task_targets, function(t) t$settings$name, character(1))
  }
  vcmd <- function(tt, name) tt[[which(vapply(tt, function(t) t$settings$name, "") == name)]]$command$expr[[1]]
  v_refuses <- function(ti, regexp) v_env(c(PMX_REVIEW_REPRODUCTION = NA), stops(vmod(ti), regexp))
  # A spec of the setup's form for synthetic roots.
  v_spec <- function(ex = "/fx/ex", cp = "/fx/copy", ws = "_targets_review_X", store = "_targets_X")
    list(fixed = stats::setNames(c(file.path(ex, store), store, paste0(ex, "/"), "", ex),
                                 c(file.path(cp, ws, "store"), file.path(ws, "store"),
                                   paste0(cp, "/", ws, "/"), paste0(ws, "/"), cp)),
         patterns = sub("^re:", "", V_STAMP), token = "<volatile>", counter = new.env())
  # A signed fixture record with a canon2 generated block (V13).
  v13_record <- function(id, stage, decision = "approved", edit_after = FALSE) {
    inner <- c("_Generated from tool output at 2026-10-01 00:00:00 UTC; values are read from fit and check objects, not transcribed._",
               paste0("Envelope: analysis_id=", id, "; task=", stage),
               "Method-manifests: sha256:fixture", "Environment-attestation: sha256:fixture (fixture)",
               "fixture content line")
    block <- c("<!-- BEGIN GENERATED PACKAGE -->", inner, "Package-Digest: pending",
               "<!-- END GENERATED PACKAGE -->")
    block[length(block) - 1L] <- paste("Package-Digest:", genv$.gate_canon2_digest(block))
    if (edit_after) block[6] <- "fixture content line, edited after signature"
    r <- v_record(id, stage, decision)
    c(r[1:5], block, "", r[-(1:5)])
  }
  v_event <- function(id, task, record_file, digest = NULL) {
    ev <- list(schema_version = "1", analysis_id = id, task = task, previous_event_id = NA,
               decision = "approved", signer = "FX", signed_at = "2026-10-01",
               recorded_at_utc = "2026-10-01T00:00:00Z", package_revision = "1",
               record_digest = if (is.null(digest)) genv$.gate_sha256(record_file) else digest,
               package_digest = "canon2:sha256:fixture", artifact_manifest_digest = "sha256:fixture",
               reason = NA)
    ev$event_id <- genv$.ledger_event_id(ev)
    f <- c("schema_version", "event_id", genv$.LEDGER_FIELDS[-1])
    paste0("{", paste(vapply(f, function(k) sprintf('"%s":%s', k,
      if (is.na(ev[[k]])) "null" else sprintf('"%s"', genv$.json_escape(ev[[k]]))), ""), collapse = ","), "}")
  }

  cases <- c(cases, list(
    "review (V): V01 a declared analysis composes review targets and references only its preceding gate records" = function() {
      tt <- v_env(c(PMX_REVIEW_REPRODUCTION = NA), vmod(v_block(), all = TRUE))
      nm <- vapply(tt, function(t) t$settings$name, "")
      stopifnot(setequal(nm, V_NAMES), length(nm) == length(V_NAMES), !any(grepl("^gate_", nm)))
      recs <- eval(vcmd(tt, "review_gate_records")[[2]])
      stopifnot(identical(recs, c("gates/review_fixture/data-assembly.md", "gates/review_fixture/reporting.md")),
                identical(tt[[which(nm == "review_gate_records")]]$settings$format, "file"),
                all(c("gate_reporting", "review_checkout_file") %in% all.names(vcmd(tt, "review_reproduction"))))
      root <- normalizePath(v_base(file.path(FX, "vm-other")))
      vw(root, "analyses/another_fixture.R", v_decl(id = "another_fixture"))
      other <- vmod(v_block(), id = "another_fixture", root = root, all = TRUE)
      stopifnot(identical(eval(vcmd(other, "review_gate_records")[[2]]),
                          sub("review_fixture", "another_fixture", recs, fixed = TRUE)))
    },
    "review (V): V02 public declaration refusals name their inputs and reproduction bypasses cross-block checks" = function() {
      drop <- function(f) { b <- v_block(); b[[f]] <- NULL; b }
      v_refuses(c(v_block(), list(extra = 1)), "review: extra: unsupported field")
      for (field in c("assessment", "map", "checkout")) {
        v_refuses(drop(field), paste0("review: ", field, ": required declaration field"))
        for (path in list(c("a", "b"), "/abs/input", "../input", "examples/absent.md"))
          v_refuses(do.call(v_block, stats::setNames(list(path), field)),
                    paste0("review: ", field, " must name an existing project-relative file"))
      }
      for (field in c("out_dir", "store"))
        for (path in c("/abs/review", "../review", "examples/../review"))
          v_refuses(do.call(v_block, stats::setNames(list(path), field)),
                    "review: out_dir and store: must be project-relative paths")
      v_refuses(v_block(out_dir = "."), "review: out_dir: must name a dedicated")
      v_refuses(v_block(checkout_permitted = "x"), "review: checkout_permitted: must be a named")
      for (key in c("staged:re:^a", "untracked:re:a", "untracked:re:^("))
        v_refuses(v_block(checkout_permitted = stats::setNames("fixture", key)),
                  "review: checkout_permitted: requires")
      v_refuses(v_block(volatile = c(accessed_at = "")), "review: volatile: must be a named")
      v_refuses(v_block(volatile = c("re:(" = "r")), "review: volatile: invalid Perl pattern")
      v_refuses(v_block(volatile = c("file:/abs/x" = "r")), "review: volatile: invalid project path")
      v_refuses(v_block(expected_changes = c("examples/absent.R" = "r")), "review: expected_changes must name")
      for (path in c("examples/review-fixture/da", "examples/review-fixture", "tools"))
        v_refuses(v_block(out_dir = path), "review: out_dir: overlaps another declared location or harness input")
      b <- v_block(store = "_targets_other")
      v_refuses(b, "review: store: must match the reporting store")
      stopifnot(setequal(v_env(c(PMX_REVIEW_REPRODUCTION = "1"), vmod(b)), V_NAMES))
      stopifnot(setequal(v_env(c(PMX_REVIEW_REPRODUCTION = "1"), vmod(v_block(out_dir = "tools"))), V_NAMES))
      v_env(c(PMX_REVIEW_REPRODUCTION = "1"),
            stops(vmod(v_block(assessment = "examples/absent.md")), "review: assessment must name"))
      root <- normalizePath(v_base(file.path(FX, "vm-dry"), decl = v_decl(da = list(scratch_dir = "examples/scratch"))))
      v_env(c(PMX_REVIEW_REPRODUCTION = NA),
            stops(vmod(v_block(), root = root), "'data_assembly/scratch_dir' looks like a location"))
      stopifnot(setequal(v_env(c(PMX_REVIEW_REPRODUCTION = "1"), vmod(v_block(), root = root)), V_NAMES))
    },
    "review (V): V03 the derivation prefixes every location, maps every store to the workspace store, keeps the rest" = function() {
      d <- file.path(FX, "v03")
      inputs <- list(a = list(out_dir = "pilot/a/outputs", store = "_targets_X"),
                     b = list(out_dir = "pilot/b/outputs", model_dir = "pilot/m", source_lst = "pilot/m/1/1.lst"),
                     c = list(out_dir = "pilot/c/outputs", report_dir = "pilot/r", covariate_dir = "pilot/cv",
                              workdir = "pilot/w", store = "_targets_X", nested = list(workdir = "pilot/w2")))
      vw(d, "analyses/X.R", c("library(targets)",
        deparse(call("compose_pipeline", analysis_id = "X", tasks = c("a", "b", "c"), inputs = inputs),
                width.cutoff = 500L)))
      decl <- rv("rv_read_declaration")(file.path(d, "analyses/X.R"))
      der <- rv("rv_derive_runner")(decl, "_targets_review_X")
      i <- der$inputs
      stopifnot(identical(i$a$out_dir, "_targets_review_X/pilot/a/outputs"),
                identical(i$b$out_dir, "_targets_review_X/pilot/b/outputs"),
                identical(i$c$out_dir, "_targets_review_X/pilot/c/outputs"),
                identical(i$b$model_dir, "_targets_review_X/pilot/m"),
                identical(i$c$report_dir, "_targets_review_X/pilot/r"),
                identical(i$c$covariate_dir, "_targets_review_X/pilot/cv"),
                identical(i$c$workdir, "_targets_review_X/pilot/w"),
                identical(i$c$nested$workdir, "_targets_review_X/pilot/w2"),
                identical(i$a$store, "_targets_review_X/store"), identical(i$c$store, i$a$store),
                identical(der$store, i$a$store), identical(i$b$source_lst, "pilot/m/1/1.lst"),
                nrow(der$locations) == 10L)
      run <- rv("rv_spawn_command")("/fx/copy", "tar_make", "_targets_review_X")
      stopifnot(grepl(sprintf('store = "%s"', der$store), run, fixed = TRUE))
      bad <- decl; bad$inputs$b$scratch_dir <- "pilot/s"
      stops(rv("rv_derive_runner")(bad, "_targets_review_X"), "'b/scratch_dir' looks like a location")
      bad <- decl; bad$inputs$c$workdir <- "/abs/w"
      stops(rv("rv_derive_runner")(bad, "_targets_review_X"), "'c/workdir' is absolute or contains")
    },
    "review (V): V04 the write census hits written paths, their ancestors and root literals, nothing else" = function() {
      roots <- c(examined = "/fx/ex", live = "/fx/live", copy = "/fx/copy")
      cmd <- c(w = '{ writeLines("x", "pilot/a/b/out.csv") }',
               anc = 'dir.create("pilot/a/b", recursive = TRUE)',
               pre = 'writeLines("x", "_targets_review_X/pilot/a/b/out.csv")',
               ex = 'readLines("/fx/ex/tools/x.R")',
               lv = 'readLines("/fx/live/tools/x.R")',
               cp = 'readLines("/fx/copy/pilot/z.csv")',
               cpw = 'readLines("/fx/copy/_targets_review_X/pilot/z.csv")',
               id = '{ gate_x; identity(c("pilot/a/in.csv", "pilot/q/in.csv")) }')
      h <- rv("rv_write_census")(cmd, "pilot/a/b/out.csv", "_targets_review_X", roots)
      stopifnot(setequal(h$target, c("w", "anc", "ex", "lv", "cp")),
                any(h$literal == "pilot/a/b" & h$target == "anc"),
                identical(h$class[h$target == "w"], "path written by a computed file target"),
                identical(h$class[h$target == "cp"], "absolute, under the copy root outside the workspace"))
      stopifnot(nrow(rv("rv_write_census")(cmd[c("pre", "cpw", "id")], "pilot/a/b/out.csv",
                                           "_targets_review_X", roots)) == 0L)
    },
    "review (V): V05 inputs: only literal inputs are copied; a refused class holding a root path stops; .diff is a fact" = function() {
      ok <- vok()
      ws <- file.path(ok$case$c, V_WS)
      stopifnot(file.exists(file.path(ws, "examples/review-fixture/da/spec.yaml")),
                file.exists(file.path(ws, "examples/review-fixture/model/x.diff")),
                file.exists(file.path(ws, "examples/review-fixture/model/notes")),
                !file.exists(file.path(ws, "examples/review-fixture/model/201")),
                sum(startsWith(ok$s$inputs$target, "da_")) == 3L,
                identical(ok$s$inert$file, "examples/review-fixture/model/x.diff"),
                identical(ok$s$inert$root, "live root"),
                any(grepl("inert-class input holding the live root", ok$s$record$quantity)))
      stops(v_setup(v_case("v05-missing", edit = function(x) file.remove(file.path(x, "examples/review-fixture/da/spec.yaml")))),
            "missing required input examples/review-fixture/da/spec.yaml")
      stops(v_setup(v_case("v05-yaml-ex", edit = function(x) vw(x, "examples/review-fixture/da/spec.yaml", paste0("root: ", x)))),
            "refused class holds the examined( and live)? root.*\\.yaml")
      stops(v_setup(v_case("v05-yaml-live", edit = function(x) vw(x, "examples/review-fixture/da/spec.yaml", paste0("root: ", V_LIVE))),
                    live = V_LIVE), "refused class holds the live root.*\\.yaml")
      stops(v_setup(v_case("v05-bare", edit = function(x) vw(x, "examples/review-fixture/model/notes", x))),
            "refused class holds the examined( and live)? root.*no extension")
      # A literal input path with a '..' component would leave the workspace (m-2).
      fm <- data.frame(name = c("ok_file", "gate_file", "up_file"), format = "file",
                       command = c(sprintf('{ g; { h; identity("%s/pilot/m/a.csv") } }', V_WS),
                                   'identity("gates/review_fixture/data-assembly.md")',
                                   sprintf('identity("%s/pilot/m/../../x.csv")', V_WS)),
                       stringsAsFactors = FALSE)
      inp <- rv("rv_input_files")(fm[1:2, ], V_WS)
      stopifnot(identical(inp$source, "pilot/m/a.csv"), identical(inp$dest, file.path(V_WS, "pilot/m/a.csv")))
      stops(rv("rv_input_files")(fm, V_WS), "input path 'pilot/m/../../x.csv' .*'\\.\\.'")
      # The same refusal in pass-1 mode (locations given, examined-form paths).
      stops(rv("rv_input_files")(data.frame(name = "up_file", format = "file", command = 'identity("pilot/m/../../x.csv")'),
                                 V_WS, "pilot/m"), "input path 'pilot/m/../../x.csv' .*'\\.\\.'")
    },
    "review (V): V06 rv_engine_caches needs both arguments, scopes by engine and deletes nothing" = function() {
      x <- file.path(FX, "v06x"); cc <- file.path(FX, "v06c")
      dir.create(x); dir.create(file.path(cc, ".git"), recursive = TRUE)
      dir.create(file.path(cc, "nlmixr2BootstrapCache_x"))
      file.create(file.path(cc, "harness.yaml")); vw(cc, ".gitignore", "_targets_*/")
      stops(rv("rv_engine_caches")(), "root and engines are both required")
      stops(rv("rv_engine_caches")(cc), "root and engines are both required")
      stopifnot(identical(rv("rv_engine_caches")(cc, "nonmem"), character(0)),
                identical(basename(rv("rv_engine_caches")(cc, "nlmixr2")), "nlmixr2BootstrapCache_x"))
      v_env(c(REPO_ROOT = NA), stops(rv("rv_copy_check")(x, normalizePath(cc), x, V_WS, "nlmixr2", character(0)),
                                     "an engine cache of a declared engine is present"))
      stopifnot(dir.exists(file.path(cc, "nlmixr2BootstrapCache_x")))
    },
    "review (V): V07 the recursion guard stubs the target, refuses setup and stops the runner prologue" = function() {
      tt <- v_env(c(PMX_REVIEW_REPRODUCTION = NA), vmod(v_block(), all = TRUE))
      ev <- new.env(parent = venv)
      ev$review_checkout_file <- "fixture"; ev$gate_reporting <- NULL
      out <- v_env(c(PMX_REVIEW_REPRODUCTION = "1"), eval(vcmd(tt, "review_reproduction"), ev))
      stopifnot(isTRUE(out$skipped), all(c("record", "comparison", "files", "text", "census", "outcomes") %in% names(out)))
      v_env(c(PMX_REVIEW_REPRODUCTION = "1"),
            stops(rv("rv_reproduce_setup")("review_fixture", live_root = "/fx/live"), "called inside a reproduction"))
      d <- file.path(FX, "v07"); dir.create(d)
      vw(d, "analyses/X.R", c("library(targets)", "compose_pipeline(analysis_id = 'X', tasks = 'a', inputs = list(a = list(out_dir = 'o')))"))
      decl <- rv("rv_read_declaration")(file.path(d, "analyses/X.R"))
      runner <- rv("rv_write_runner")(decl, rv("rv_derive_runner")(decl, "_targets_review_X"), d, "1791072000")
      ex <- parse(runner)
      stopifnot(any(grepl('Sys.setenv(SOURCE_DATE_EPOCH = "1791072000")', readLines(runner), fixed = TRUE)),
                file.exists(file.path(d, "inputs.rds")))
      v_env(c(PMX_REVIEW_REPRODUCTION = "1"), stops(eval(ex[[1]], new.env()), "Recursion guard"))
    },
    "review (V): V08 the store comparator normalizes both sides, drops volatile names, blanks expected changes" = function() {
      pkg <- function(ws = "", cp = "/fx/ex", stamp = "20261003T065219Z_b11484528f45d", acc = "t1", val = 1, md5 = "m1", md5y = "y1", nv = 1)
        list(x_package = list(analysis = "FX", task = "x",
               tables = list(t1 = data.frame(a = c(val, 2), accessed_at = c(acc, "t2"),
                                             path = c(paste0(ws, "pilot/m/bootstrap_run_", stamp, "/a.csv"),
                                                      paste0(cp, "/", ws, "pilot/m/q.csv")),
                                             stringsAsFactors = FALSE)),
               artifacts = data.frame(artifact = "a", path = paste0(ws, "pilot/m/a.csv")),
               notes = list(genv$authored_note("fixture note"), genv$evidence_note("fixture value", nv))),
             reporting_package = list(analysis = "FX", task = "reporting",
               tables = list("Promoted values and their sources" = data.frame(target = "report_x", source = "x_package", selection = "t1"),
                             "Code inventory (gate traceability; not rendered in the MAR)" =
                               data.frame(file = c("analyses/X.R", "tools/y.R"), bytes = c(1, 2), md5 = c(md5, md5y))),
               artifacts = data.frame(artifact = "r", path = paste0(ws, "pilot/r/mar.pdf")), notes = list()),
             review_package = list(analysis = "FX", task = "review", tables = list(), artifacts = NULL, notes = list()),
             dataset = data.frame(ID = 1:2), report_x = data.frame(v = 1),
             environment_attestation = list(fingerprint = "f", probe = list(lib = paste0(cp, "/lib"))))
      a <- pkg()
      b <- pkg(ws = "_targets_review_X/", cp = "/fx/copy", stamp = "20261010T010101Z_0123456789abc", acc = "t9")
      vol <- stats::setNames(c("time", "stamp"), c("accessed_at", V_STAMP))
      ec <- c("analyses/X.R" = "expected")
      r <- rv("rv_compare_stores")(a, b, v_spec(), vol, ec)
      stopifnot(all(r$table$match %in% c(TRUE, NA)), nrow(r$outcomes) == 0L,
                r$excluded$removed[r$excluded$entry == "accessed_at"] >= 2L,
                identical(r$excluded$removed[r$excluded$entry == V_STAMP], 1L),   # one analysis cell (m-6)
                any(grepl("typed notes compared", r$table$detail)),
                is.na(r$table$match[grepl("^review_package", r$table$item)]))
      r <- rv("rv_compare_stores")(a, pkg(ws = "_targets_review_X/", cp = "/fx/copy", val = 7, md5 = "m2"),
                                   v_spec(), vol, ec)
      bad <- r$table[r$table$match %in% FALSE, ]
      stopifnot(nrow(bad) == 1L, grepl("x_package: tables\\[t1\\]", bad$item), grepl("'1' became '7'", bad$detail),
                identical(r$outcomes$rule, "comparison_mismatch"))
      r <- rv("rv_compare_stores")(a, pkg(ws = "_targets_review_X/", cp = "/fx/copy", md5y = "y2"), v_spec(), vol, ec)
      stopifnot(sum(r$table$match %in% FALSE) == 1L, any(grepl("Code inventory", r$table$item[r$table$match %in% FALSE])))
      r <- rv("rv_compare_stores")(a, pkg(ws = "_targets_review_X/", cp = "/fx/copy", acc = "t9"), v_spec(), character(0), ec)
      stopifnot(sum(r$table$match %in% FALSE) >= 1L)
      sp <- v_spec()
      stopifnot(identical(rv("rv_normalize")(c("/fx/copy/_targets_review_X/pilot/q", "_targets_review_X/pilot/q",
                                               "/fx/copy/_targets_review_X/store/x", "/fx/copy/z"), sp),
                          c("/fx/ex/pilot/q", "pilot/q", "/fx/ex/_targets_X/x", "/fx/ex/z")))
    },
    "review (V): V08b the comparator matches equal infinities; any other infinite or missing difference is a mismatch with a preview" = function() {
      cmp <- rv("rv_compare_value")
      df <- function(v) data.frame(k = c("a", "b"), rse_pct = c(v, 2), stringsAsFactors = FALSE)
      # Equal infinities (fixed parameters carry rse_pct Inf on both sides, L3c).
      for (v in c(Inf, -Inf)) {
        r <- cmp("fx", df(v), df(v))
        stopifnot(isTRUE(r$match), isTRUE(r$exact), identical(r$max_abs_diff, 0))
        r <- cmp("fx", c(v, 1, NA), c(v, 1, NA))
        stopifnot(isTRUE(r$match), isTRUE(r$exact), identical(r$max_abs_diff, 0))
      }
      # Every other pair holding an infinity or a missing value is a mismatch
      # whose preview names both cells.
      tx <- function(v) if (is.na(v)) "NA" else as.character(v)
      for (p in list(c(Inf, 3), c(Inf, -Inf), c(NA, Inf), c(Inf, NA), c(-Inf, 3))) {
        want <- sprintf("'%s' became '%s'", tx(p[1]), tx(p[2]))
        r <- cmp("fx", df(p[1]), df(p[2]))
        stopifnot(isFALSE(r$match), isFALSE(r$exact), grepl(want, r$detail, fixed = TRUE))
        r <- cmp("fx", c(p[1], 1), c(p[2], 1))
        stopifnot(isFALSE(r$match), isFALSE(r$exact), grepl(want, r$detail, fixed = TRUE))
      }
    },
    "review (V): V08c declared fixture volatile entries pass the block's refusals and replace only the volatile part" = function() {
      vol <- v_block()$volatile
      stopifnot(setequal(v_env(c(PMX_REVIEW_REPRODUCTION = NA), vmod(v_block(volatile = vol))), V_NAMES))
      n <- function(x) rv("rv_normalize")(x, list(patterns = sub("^re:", "", grep("^re:", names(vol), value = TRUE))))
      h1 <- strrep("a1", 32L); h2 <- strrep("0f", 32L)
      # Each row: two synthetic lines that differ only in the volatile part, and
      # the one form both must take (the label kept, the varying part a token).
      rows <- list(
        c("Tue Sep 29 07:12:33 UTC 2026", "Sun Oct  4 18:09:10 UTC 2026", "<volatile>"),
        c("x (MINIMIZATION SUCCESSFUL | Wed Sep 30 01:02:03 UTC 2026)", "x (MINIMIZATION SUCCESSFUL | Sun Oct 11 11:22:33 UTC 2026)",
          "x (MINIMIZATION SUCCESSFUL | <volatile>)"),
        c("Current Date:       29 SEP 2026", "Current Date:        4 OCT 2026", "Current Date:<volatile>"),
        c("Days until program expires : 123", "Days until program expires :  45", "Days until program expires :<volatile>"),
        c(" Elapsed estimation  time in seconds:     0.12", " Elapsed estimation  time in seconds:    10.34",
          " Elapsed estimation  time in seconds:<volatile>"),
        c(" Elapsed covariance  time in seconds:     0.01", " Elapsed covariance  time in seconds:     0.02",
          " Elapsed covariance  time in seconds:<volatile>"),
        c(" Elapsed postprocess time in seconds:     0.00", " Elapsed postprocess time in seconds:     0.03",
          " Elapsed postprocess time in seconds:<volatile>"),
        c(" Elapsed finaloutput time in seconds:     0.05", " Elapsed finaloutput time in seconds:     0.06",
          " Elapsed finaloutput time in seconds:<volatile>"),
        c(" #CPUT: Total CPU Time in Seconds,        0.123", " #CPUT: Total CPU Time in Seconds,        1.456",
          " #CPUT: Total CPU Time in Seconds,<volatile>"),
        c("Generated 2026-09-29 07:12 UTC from executed run records (bm_records, bm_params).",
          "Generated 2026-10-11 08:09 UTC from executed run records (bm_records, bm_params).",
          "Generated <volatile> from executed run records (bm_records, bm_params)."),
        c("created_at: '2026-09-29T07:12:33Z'", "created_at: '2026-10-11T08:09:10Z'", "created_at: <volatile>"),
        c('<meta name="date" content="2026-09-29" />', '<meta name="date" content="2026-10-11" />',
          '<meta name="date" content="<volatile>" />'),
        c('<h1 class="date">2026-09-29</h1>', '<h1 class="date">2026-10-11</h1>', '<h1 class="date"><volatile></h1>'),
        c('<h4 class="date">2026-09-29</h4>', '<h4 class="date">2026-10-11</h4>', '<h4 class="date"><volatile></h4>'),
        c(paste0("  sha256: ", h1), paste0("  sha256: ", h2), "  sha256: <volatile>"),
        c(paste0("        sha256: ", h1), paste0("        sha256: ", h2), "        sha256: <volatile>"),
        c(paste("converged (0MINIMIZATION SUCCESSFUL |  NO. OF FUNCTION EVALUATIONS USED:      123 |",
                "NO. OF SIG. DIGITS IN FINAL EST.:  3.4 |  #OBJV:****      111.111       **** |",
                "Tue Sep 29 07:12:33 UTC 2026 | $PROBLEM synthetic run"),
          paste("converged (0MINIMIZATION SUCCESSFUL |  NO. OF FUNCTION EVALUATIONS USED:      123 |",
                "NO. OF SIG. DIGITS IN FINAL EST.:  3.4 |  #OBJV:****      111.111       **** |",
                "Sun Oct 11 08:09:10 UTC 2026 | $PROBLEM synthetic run"),
          paste("converged (0MINIMIZATION SUCCESSFUL |  NO. OF FUNCTION EVALUATIONS USED:      123 |",
                "NO. OF SIG. DIGITS IN FINAL EST.:  3.4 |  #OBJV:****      111.111       **** |",
                "<volatile> | $PROBLEM synthetic run")))
      for (r in rows)
        if (!identical(n(r[1:2]), r[c(3L, 3L)]))
          stop("volatile row not normalized as declared: '", r[3], "' (got '", paste(n(r[1:2]), collapse = "' and '"), "')")
      # Controls: content that is not volatile stays byte for byte.
      ctrl <- c(" NO. OF FUNCTION EVALUATIONS USED:      123", " NO. OF SIG. DIGITS IN FINAL EST.:  3.4",
                " #OBJV:********************************************      111.111       *****",
                " $ESTIMATION METHOD=1 INTERACTION MAXEVAL=9999 PRINT=5 NOABORT",
                " TH 1   1.11E+00   2.22E-01", "ITERATION  -1000000000  1.1E+00  2.2E-01  3.3E-01",
                paste0("# config_sha256: ", h1), paste0("  sha256: ", h1, " # trailing"),
                paste0("Method-manifests: sha256:", h1), "  path: examples/review-fixture/model/201/201.lst",
                "Date: 2026-10-01", "Signed at 2026-10-01T00:00:00Z", "created_at: 2026-10-01",
                "Elapsed time in seconds: 1.0", "<p class=\"date\">2026-09-29</p>",
                "<h4 class=\"title\">2026-09-29</h4>")
      stopifnot(identical(n(ctrl), ctrl))
    },
    "review (V): V08d a declared fixture stamp pattern takes a PsN run-directory suffix of 13 or 14 hex characters" = function() {
      # Both fixture suffix lengths normalize without changing the tool name.
      vol <- v_block()$volatile
      n <- function(x) rv("rv_normalize")(x, list(patterns = sub("^re:", "", grep("^re:", names(vol), value = TRUE))))
      for (tool in c("bootstrap", "vpc")) {
        x <- paste0("examples/review-fixture/qualification/", tool, "_run_20261003T065219Z_",
                    c("0123456789abc", "0123456789abcd"), "/raw.csv")
        want <- paste0("examples/review-fixture/qualification/", tool, "<volatile>/raw.csv")   # group 1 keeps the tool (N-3)
        if (!identical(n(x), c(want, want)))
          stop("stamp not normalized on both sides: got '", paste(n(x), collapse = "' and '"), "'")
      }
      ctrl <- c("examples/review-fixture/qualification/vpc_run_20261003T065219Z_0123456/raw.csv",
                "examples/review-fixture/qualification/vpc_results.csv")
      stopifnot(identical(n(ctrl), ctrl))
    },
    "review (V): V09 the errored-set rule and the outcome: complete, unexpected errors, killed and partial runs" = function() {
      g <- file.path(FX, "v09")
      vw(g, "gates/review_fixture/reporting.md", v_record("review_fixture", "reporting"))
      vw(g, "gates/review_fixture/review.md", v_record("review_fixture", "review", decision = "unsigned"))
      expected <- rv("rv_expected_errors")(g, "review_fixture", c("reporting", "review"))
      stopifnot(identical(expected, "gate_review"))
      edges <- data.frame(from = c("x1", "gate_reporting", "r1"), to = c("gate_reporting", "r1", "gate_review"))
      meta <- function(err = list(), names = c("x1", "gate_reporting", "r1", "gate_review"))
        data.frame(name = names, type = "stem",
                   error = vapply(names, function(n) if (is.null(err[[n]])) NA_character_ else err[[n]], ""),
                   stringsAsFactors = FALSE)
      ok <- rv("rv_run_outcome")(1L, meta(list(gate_review = "Gate not signed: gates/review_fixture/review.md")), expected, edges)
      stopifnot(ok$complete, ok$equal, nrow(ok$outcomes) == 0L)
      # Targets outside the ancestors and descendants: recorded built or not (m-9).
      o <- rv("rv_run_outcome")(1L, meta(list(gate_review = "Gate not signed: x"),
                                         c("x1", "gate_reporting", "r1", "gate_review", "z1")),
                                expected, edges, outside = c("z1", "z2"))
      stopifnot(identical(o$record$value[o$record$quantity == "targets outside the ancestors and descendants not built"], "1"))
      o <- rv("rv_run_outcome")(1L, meta(list(gate_reporting = "Gate not signed: x"), c("x1", "gate_reporting")), expected, edges)
      stopifnot(o$complete, !o$equal, identical(o$outcomes$rule, "errored_set"))
      o <- rv("rv_run_outcome")(1L, meta(list(x1 = "an analysis error"), "x1"), expected, edges)
      stopifnot(o$complete, !o$equal, identical(o$outcomes$rule, "errored_set"), grepl("x1", o$outcomes$owner))
      o <- rv("rv_run_outcome")(137L, meta(names = c("x1", "gate_reporting")), expected, edges)
      stopifnot(!o$complete, identical(o$outcome, "incomplete"), nrow(o$outcomes) == 0L)   # a stop, no finding (QA F-2)
      o <- rv("rv_run_outcome")(1L, meta(names = c("x1", "gate_reporting")), expected, edges)
      stopifnot(!o$complete, nrow(o$outcomes) == 0L)
      o <- rv("rv_run_outcome")(1L, meta(), expected, edges)
      stopifnot(!o$complete, nrow(o$outcomes) == 0L)
    },
    "review (V): V10 the tree digest stops on a change inside the census roots and ignores review/ and bytecode" = function() {
      roots <- c("examples/review-fixture", "gates/review_fixture", "pilot/review_fixture-*.md", "analyses/review_fixture.R",
                 "pipeline", "tools", "skills")
      ex <- "examples/review-fixture/review"
      mk <- function(tag) {
        r <- file.path(FX, paste0("v10-", tag))
        vw(r, "examples/review-fixture/a.txt", "abcd"); vw(r, "examples/review-fixture/plan.md", "plan")
        vw(r, "examples/review-fixture/review/r.txt", "r"); vw(r, "tools/t.R", "t <- 1")
        vw(r, "pipeline/_targets_x.R", "x"); vw(r, "tools/x/__pycache__/y.pyc", "b")
        r
      }
      check <- function(tag, change, stop_expected) {
        r <- mk(tag)
        before <- rv("rv_tree_digest")(r, roots, ex)
        change(r)
        after <- rv("rv_tree_digest")(r, roots, ex)
        od <- file.path(FX, paste0("v10-out-", tag))
        msg <- tryCatch({ rv("rv_tree_check")(before, after, od, "live-tree", "the fixture tree"); NULL },
                        error = function(e) conditionMessage(e))
        if (stop_expected) stopifnot(!is.null(msg), file.exists(file.path(od, "live-tree-before.tsv")),
                                     file.exists(file.path(od, "live-tree-after.tsv")))
        else stopifnot(is.null(msg), !dir.exists(od))
      }
      check("pilot", function(r) vw(r, "examples/review-fixture/a.txt", "abce"), TRUE)   # same size
      check("tools", function(r) vw(r, "tools/t.R", "t <- 2"), TRUE)
      check("map", function(r) vw(r, "examples/review-fixture/plan.md", "plan!"), TRUE)
      check("nested", function(r) vw(r, "pipeline/_targets_x.R", "y"), TRUE)
      check("review", function(r) vw(r, "examples/review-fixture/review/r.txt", "s"), FALSE)
      check("pyc", function(r) vw(r, "tools/x/__pycache__/y.pyc", "c"), FALSE)
      check("none", function(r) NULL, FALSE)
      d <- rv("rv_tree_digest")(mk("list"), roots, ex)
      stopifnot("pipeline/_targets_x.R" %in% d$path, !any(grepl("__pycache__|review/", d$path)))
    },
    "review (V): V10-recipe-nested fixture copy retains nested source" = function() {
      f <- v10_copy("nested")
      stopifnot(v10_equal_file(f, "pipeline/_targets_full_example.R"),
                "pipeline/_targets_full_example.R" %in% rv("rv_tree_digest")(f$dst, "pipeline")$path)
    },
    "review (V): V10-recipe-root-store fixture copy omits only root stores" = function() {
      f <- v10_copy("root-store")
      stopifnot(!dir.exists(file.path(f$dst, "_targets")), !dir.exists(file.path(f$dst, "_targets_fixture")),
                v10_equal_file(f, "pipeline/_targets_full_example.R"))
    },
    "review (V): V10-recipe-pyc fixture copy omits standalone bytecode" = function() {
      f <- v10_copy("pyc"); stopifnot(!file.exists(file.path(f$dst, "tools/standalone.pyc")))
    },
    "review (V): V10-recipe-cache fixture copy omits non-bytecode cache payload" = function() {
      f <- v10_copy("cache"); stopifnot(!file.exists(file.path(f$dst, "tools/__pycache__/payload.txt")))
    },
    "review (V): V10-recipe-private fixture copy omits each private sentinel" = function() {
      f <- v10_copy("private")
      for (rel in f$private) if (file.exists(file.path(f$dst, rel))) stop("private sentinel copied: ", rel)
    },
    "review (V): V10-recipe-source fixture copy retains source and detects same-size changes" = function() {
      f <- v10_copy("source"); stopifnot(v10_equal_file(f, "tools/source.R"))
      before <- rv("rv_tree_digest")(f$dst, "tools")
      vw(f$dst, "tools/source.R", "fixture harmless bytez")
      after <- rv("rv_tree_digest")(f$dst, "tools")
      out <- file.path(FX, "v10-recipe-source-out")
      msg <- tryCatch({rv("rv_tree_check")(before, after, out, "copy-tree", "fixture"); NULL},
                      error = function(e) conditionMessage(e))
      stopifnot(!identical(before, after), !is.null(msg),
                file.exists(file.path(out, "copy-tree-before.tsv")), file.exists(file.path(out, "copy-tree-after.tsv")))
    },
    "review (V): V10-digest-pyc standalone bytecode is independently ignored" = function() {
      v10_digest("pyc", "tools/standalone.pyc", TRUE)
    },
    "review (V): V10-digest-cache non-bytecode cache payload is independently ignored" = function() {
      v10_digest("cache", "tools/__pycache__/payload.txt", TRUE)
    },
    "review (V): V10-digest-source nested ordinary source remains visible" = function() {
      v10_digest("source", "pipeline/_targets_full_example.R", FALSE)
    },
    "review (V): V10-recipe-guard fixture copy rejects overlapping or non-fixture roots before launch" = function() {
      src <- file.path(FX, "v10-overlap")
      stopifnot(inherits(tryCatch(v10_guard(REPO, file.path(FX, "unsafe")), error = identity), "error"),
                inherits(tryCatch(v10_guard(src, src), error = identity), "error"),
                inherits(tryCatch(v10_guard(src, file.path(src, "inside")), error = identity), "error"),
                !file.exists(src))
    },
    "review (V): V11 the R-F11 text rule accepts only the recorded legend reordering; the deck rule is strict" = function() {
      keys <- "Point estimate Confidence interval Reference subject Reference area"
      p1 <- c("Title page", "Results 1.25 and 3.5")
      p2 <- c("Forest", keys, "AUC 1.01 [0.95, 1.08]", "Cmax 0.98 [0.90, 1.05]", "end")
      mk <- function(pages) c(unlist(lapply(seq_along(pages), function(i) {
        l <- pages[[i]]; if (i > 1L) l[1] <- paste0("\f", l[1]); l })), "\f")
      tc <- function(a, b) rv("rv_text_compare")(mk(a), mk(b), "pdf")$outcome
      re1 <- p2; re1[2] <- "Reference subject Point estimate Reference area Confidence interval"
      re2 <- c(p2[1], "Reference area Confidence interval", "Point estimate Reference subject", p2[3:5])
      swap <- p2[c(1, 2, 4, 3, 5)]
      word <- p2; word[2] <- "Legend Point estimate Confidence interval Reference subject Reference area"
      dec <- p1; dec[2] <- "Results 1.25 and 3.6"
      extra <- c(p1, "an extra line")
      stopifnot(isTRUE(tc(list(p1, p2), list(p1, p2))), isTRUE(tc(list(p1, p2), list(p1, re1))),
                isTRUE(tc(list(p1, p2), list(p1, re2))), isFALSE(tc(list(p1, p2), list(p1, swap))),
                isFALSE(tc(list(p1, p2), list(p1, word))), isFALSE(tc(list(p1, p2), list(dec, p2))),
                isFALSE(tc(list(p1, p2), list(extra, re1))), isFALSE(tc(list(p1, p2), list(p1, p2, p1))))
      dk <- function(a, b) rv("rv_text_compare")(a, b, "deck")$outcome
      s <- list(c("Title", "1.0"), c("Results", "AUC"))
      stopifnot(isTRUE(dk(s, s)), isFALSE(dk(s, list(c("Title", "1.0"), c("Results", "Cmax")))),
                isFALSE(dk(s, s[1])))
    },
    "review (V): V12 the census parse counts per leg; undefined references judged on the log leg alone" = function() {
      stream <- c("warning: texput.tex:100: Overfull \\hbox (25.5pt too wide) in paragraph at lines 100--101",
                  "warning: texput.tex:50: Overfull \\hbox (25.5pt too wide) in paragraph at lines 50--51",
                  "warning: texput.tex:60: Overfull \\vbox (3.0pt too high) has occurred",
                  "warning: Missing character: There is no X in font", "Output created: mar.pdf")
      log <- c("This is TeX", "Overfull \\hbox (25.5pt too wide) in paragraph at lines 50--51",
               "Overfull \\vbox (3.0pt too high) has occurred while \\output is active",
               "Missing character: There is no X in font",
               "LaTeX Warning: Reference `fig:x' on page 1 undefined on input line 5.")
      allowed <- "fixture::allowed\\_symbol"
      tex <- c("CL\\_i in the text", "\\texttt{CL\\_i} as code", paste(allowed, "value"))
      ct <- rv("rv_render_census")(stream, log, tex, 0L, TRUE, allow = allowed)$table
      j <- function(t, q) t$judged[startsWith(t$quantity, q)]
      stopifnot(j(ct, "overfull hbox over 20 pt") == 1, ct$stream[startsWith(ct$quantity, "overfull hbox over")] == 1,
                j(ct, "overfull vbox") == 1, j(ct, "missing glyph") == 1, j(ct, "undefined reference") == 1,
                ct$stream[startsWith(ct$quantity, "undefined")] == 0, j(ct, "escaped symbol") == 1,
                j(ct, "render completed") == 1, j(ct, "engine exit") == 0)
      ct2 <- rv("rv_render_census")(c(stream, "LaTeX Warning: Reference `y' undefined"),
                                    log[1:4], tex, 0L, TRUE, allow = allowed)
      stopifnot(j(ct2$table, "undefined reference") == 0,
                setequal(ct2$outcomes$rule, "census_threshold"))
    },
    "review (V): V13 gate integrity: unsigned, rejected, edited-after-signature and ledger-mismatch records" = function() {
      g <- file.path(FX, "v13")
      f <- function(s) file.path(g, "gates", "FXA", paste0(s, ".md"))
      vw(g, "gates/FXA/alpha.md", v13_record("FXA", "alpha"))
      vw(g, "gates/FXA/beta.md", v_record("FXA", "beta", decision = "unsigned"))
      vw(g, "gates/FXA/gamma.md", v13_record("FXA", "gamma", decision = "rejected"))
      vw(g, "gates/FXA/delta.md", v13_record("FXA", "delta", edit_after = TRUE))
      vw(g, "gates/FXA/omega.md", v13_record("FXA", "omega"))
      ledger <- vw(g, "gates/FXA/approvals.jsonl", c(v_event("FXA", "alpha", f("alpha")),
                                                    v_event("FXA", "delta", f("delta")),
                                                    v_event("FXA", "omega", f("omega"), paste0("sha256:", strrep("0", 64)))))
      r <- rv("rv_gate_integrity")(f(c("alpha", "beta", "gamma", "delta", "omega")), ledger, character(0),
                                   c("untracked:re:^x" = "r"), "alpha", "alpha", "FXA")
      oc <- r$outcomes[r$outcomes$rule == "gate_record", ]
      on <- function(s) oc$finding[grepl(s, oc$finding, fixed = TRUE)]
      stopifnot(!length(on("alpha.md")), !any(grepl("alpha", oc$finding)),
                any(grepl("not approved \\(decision unsigned\\)", on("beta.md"))),
                any(grepl("not approved \\(decision rejected\\)", on("gamma.md"))),
                any(grepl("content problem", on("delta.md"))),
                any(grepl("latest event for omega", oc$finding)),
                r$records$content_problems[basename(r$records$record) == "delta.md"] > 0L,
                identical(r$ledger$value[r$ledger$quantity == "approvals ledger events"], "3"),
                grepl("^[0-9a-f]{64}$", r$ledger$value[r$ledger$quantity == "approvals ledger sha256"]))
    },
    "review (V): V14 each row of the severity table follows from its outcome" = function() {
      sev <- c(errored_set = "critical", attestation = "critical", comparison_mismatch = "critical",
               file_mismatch = "critical", text_rule = "critical", gate_record = "critical",
               promotion = "critical", validation = "critical", census_threshold = "major",
               concordance = "major", inventory_undeclared = "major", section6_differs = "major",
               checkout = "major", history_after_sign = "major (reading item)", registry = "major",
               not_evaluated = "major", inventory_expected = "minor (expected)",
               section6_pending = "minor (expected)", crossref_pending = "minor (expected)",
               outside_attestation = "minor (recorded limitation)")
      cl <- rv("rv_checklist")(stats::setNames(rep(list(list(outcomes = NULL)), 6),
        c("review_gate_integrity", "review_reproduction", "review_traceability", "review_map_concordance",
          "review_validation", "review_crossdoc")))
      f <- rv("rv_findings")(rv("rv_outcome")(names(sev), "domain 3 bullet 1", paste("fixture", names(sev)), "e"), cl)
      stopifnot(identical(f$log$severity, unname(sev)), nrow(f$log) == 20L,
                identical(f$log$owner[f$log$finding == "fixture inventory_undeclared"], "owning module"))
      stops(rv("rv_findings")(rv("rv_outcome")("unknown_rule", NA, "x", "e"), cl), "unknown rule")
    },
    "review (V): V15 promotion check: table and notes exact; an unknown form is not evaluated, never a match" = function() {
      tab <- data.frame(a = 1:2)
      vals <- list(x_package = list(tables = list(Heading = tab), notes = list("n1")),
                   t1 = tab, t2 = list("n1"), t3 = "anything",
                   reporting_package = list(tables = list("Number census of the report sources" =
                                                            data.frame(file = "f", undeclared = 0))))
      pr <- data.frame(target = c("t1", "t2", "t3"), source = c("x_package", "x_package", "somewhere"),
                       selection = c("Heading", "notes", "magic"), stringsAsFactors = FALSE)
      man <- c("# result: PASS", "# notes-expected: 2", "# notes-found: 2")
      r <- rv("rv_promotion_check")(pr, vals, character(0), list(), man)
      stopifnot(identical(r$promotions$match, c(TRUE, TRUE, NA)),
                identical(r$promotions$form[3], "not evaluated"),
                identical(r$outcomes$rule, "not_evaluated"), all(r$numbers$match))
      vals$t1 <- data.frame(a = c(1L, 3L))
      r <- rv("rv_promotion_check")(pr[1, ], vals, character(0), list(), man)
      stopifnot(identical(r$outcomes$rule, "promotion"))
    },
    "review (V): V16 validation audit: reviewed before first use passes; after first use or a draft unit is critical" = function() {
      skill <- "skills/data-assembly"
      ref <- file.path(skill, "reference/weight_constants.R")
      mkroot <- function(tag, draft = FALSE, reviewed = TRUE) {
        r <- file.path(FX, paste0("v16-", tag))
        dir.create(file.path(r, "skills"), recursive = TRUE)
        dir.create(file.path(r, "tools"))
        stopifnot(file.copy(file.path(REPO, skill), file.path(r, "skills"), recursive = TRUE),
                  file.copy(file.path(TOOLS, "method_manifest.R"), file.path(r, "tools")))
        if (reviewed) va_review(r, "data-assembly")
        if (!draft) va_status(r, "data-assembly", "approved")
        normalizePath(r)
      }
      man <- data.frame(name = c("da_x", "gate_data_assembly"),
                        command = c(sprintf("{\n    load_reference('%s', '%s')\n}", skill, ref),
                                    "{\n    gate_data_assembly_file\n}"),
                        stringsAsFactors = FALSE)
      audit <- function(root, date) {
        rec <- vw(root, "gates/FXA/data-assembly.md", v_record("FXA", "data-assembly", date = date))
        old <- setwd(root); on.exit(setwd(old))
        rv("rv_validation_audit")(man, "data_assembly", c(data_assembly = rec), list(), "data-assembly")
      }
      method <- function(root) {
        old <- setwd(root); on.exit(setwd(old))
        loaded <- env$load_reference(skill, ref)
        evidence <- attr(loaded, "load_reference")
        stopifnot(identical(evidence$units, "weight_constants"),
                  identical(evidence$reference_path, ref),
                  identical(evidence$sha256,
                            rv("read_method_manifest")(file.path(skill, "MANIFEST.yaml"))$units$weight_constants$sha256))
        loaded$reference_weight_constants(1:5, c(0.25, 0.75), 7L)$median
      }
      ok_root <- mkroot("ok")
      ok <- audit(ok_root, "2026-10-01")
      row <- va_row(ok, "data-assembly")
      stopifnot(nrow(ok$outcomes) == 0L, identical(row$units, "weight_constants"),
                identical(row$statuses, "approved"), isTRUE(row$reviewed_before_first_use),
                identical(row$validation_problems, 0L), identical(method(ok_root), 3L))
      late <- audit(mkroot("late"), "2026-09-01")
      stopifnot(any(late$outcomes$rule == "validation" & grepl("after its first use", late$outcomes$finding)))
      draft_root <- mkroot("draft", draft = TRUE)
      dr <- audit(draft_root, "2026-10-01")
      stopifnot(any(dr$outcomes$rule == "validation" & grepl("not approved", dr$outcomes$finding)))
      stops(method(draft_root), "not SME-validated")

      unsigned_root <- mkroot("unsigned", reviewed = FALSE)
      unsigned <- audit(unsigned_root, "2026-10-01")
      stopifnot(identical(va_row(unsigned, "data-assembly")$statuses, "draft"),
                any(unsigned$outcomes$rule == "not_evaluated" & grepl("reviewed_at is blank", unsigned$outcomes$finding)),
                any(unsigned$outcomes$rule == "validation" & grepl("not approved", unsigned$outcomes$finding)))
      stops(method(unsigned_root), "not SME-validated")

      cat("\n# Fixture byte drift\n", file = file.path(ok_root, ref), append = TRUE)
      drift <- audit(ok_root, "2026-10-01")
      stopifnot(identical(va_row(drift, "data-assembly")$validation_problems, 1L),
                any(drift$outcomes$rule == "validation" & grepl("manifest does not validate", drift$outcomes$finding)))
      stops(method(ok_root), "bytes .* do not match the digest")
    },
    "review (V): V17 review helpers define rv_ functions without defining targets or loading code directly" = function() {
      ex <- parse(RV_FILE, keep.source = FALSE)
      ok <- vapply(ex, function(e) is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
                     is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function")), logical(1))
      stopifnot(all(ok))
      nm <- vapply(ex, function(e) as.character(e[[2]]), "")
      stopifnot(all(startsWith(nm, "rv_")), !anyDuplicated(nm))
      heads <- function(e) {
        if (is.call(e)) c(if (is.name(e[[1]])) as.character(e[[1]]), unlist(lapply(as.list(e), heads)))
        else if (is.pairlist(e)) unlist(lapply(as.list(e), heads))
      }
      calls <- unique(unlist(lapply(ex, function(e) heads(e[[3]]))))
      stopifnot(!any(c("source", "sys.source", "load_reference", "require_gate") %in% calls))
      tt <- v_env(c(PMX_REVIEW_REPRODUCTION = NA), vmod(v_block(), all = TRUE))
      r <- new.env(parent = venv)
      pb <- rv("rv_read_declaration")(file.path(vmroot(), "analyses/review_fixture.R"))$inputs$reporting
      r$task_input <- pb
      r$analysis_id <- "review_fixture"
      r$upstream_tasks <- "data_assembly"
      local({
        old <- setwd(vmroot())
        on.exit(setwd(old))
        sys.source("pipeline/tasks/reporting.R", envir = r)
      })
      ordinary <- r$task_targets
      target_names <- vapply(c(tt, ordinary), function(t) t$settings$name, "")
      report_symbols <- unique(unlist(lapply(ordinary, function(t) all.names(t$command$expr[[1]]))))
      stopifnot(!any(nm %in% target_names), !any(nm %in% report_symbols))
    },
    "review (V): V18 copy refusals; dry reports; setup writes only under the copy's workspace; the spawn strings" = function() {
      b <- v_case("v18")
      stops(v_setup(b, copy = NA), "PMX_REVIEW_COPY is unset or empty")
      stops(v_setup(b, copy = "relative/copy"), "is not an absolute path")
      stops(v_setup(b, copy = b$x), "equal to, inside or containing the examined root")
      stops(v_setup(b, copy = file.path(b$x, "examples")), "equal to, inside or containing the examined root")
      stops(v_setup(b, copy = dirname(b$x)), "equal to, inside or containing the examined root")
      stops(v_setup(b, extra = c(REPO_ROOT = "/somewhere")), "REPO_ROOT is set")
      stops(v_setup(v_case("v18-nogit", git = "none")), "the copy has no .git directory")
      stops(v_setup(v_case("v18-noharness", harness = FALSE)), "the copy holds no harness.yaml")
      stops(v_setup(v_case("v18-store", decl = v_decl(reporting_store = "_targets_other"))),
            "store: the review block's store differs")
      differ <- function(cc) cat("# changed in the copy\n", file = file.path(cc, "tools", "compose_marker.R"))
      stops(v_setup(v_case("v18-differ", edit_copy = differ)), "census roots of the copy differ")
      plain <- v_case("v18-plain", git = "plain")
      stopifnot(identical(rv("rv_copy_head")(plain$c)$head, "HEAD unreadable"))
      stops(v_setup(plain), "HEAD is unreadable")
      # A readable copy HEAD that differs from the receipt's HEAD line (QA F-4);
      # the receipt is rewritten identically in both roots so the census holds.
      hd <- v_case("v18-head")
      for (r in c(hd$x, hd$c))
        vw(r, "examples/review-fixture/checkout.txt", c(paste("HEAD", strrep("1", 40)),
           "status ?? gates/review_fixture/sign-fixture/", "record gates/review_fixture/data-assembly.md commits 1 after-sign 0"))
      stops(v_setup(hd), "HEAD differs from the HEAD line of the checkout receipt")
      dry <- v_case("v18-dry", edit_copy = differ, harness = FALSE, store = TRUE)
      s <- v_setup(dry, dry = TRUE)
      stopifnot(isFALSE(s$summary$census_roots_equal), "tools/compose_marker.R" %in% s$summary$differing_paths,
                isFALSE(s$summary$harness_yaml_in_copy), identical(s$summary$copy_head, dry$head),
                isTRUE(s$summary$derived_store_identical), s$summary$write_census_hits == 0L,
                isTRUE(s$summary$names_equal), isTRUE(s$summary$commands_equal),
                isTRUE(s$summary$tree_unchanged), identical(s$summary$expected_errors, "gate_review"))
      ok <- vok()
      stopifnot(identical(ok$before_x, ok$after_x),
                identical(ok$after_c[names(ok$before_c)], ok$before_c),
                all(startsWith(setdiff(names(ok$after_c), names(ok$before_c)), paste0(V_WS, "/"))),
                file.exists(file.path(ok$case$c, V_WS, "runner.R")),
                !dir.exists(file.path(ok$case$x, "examples/review-fixture/review")),
                identical(ok$s$expected_errors, "gate_review"),
                identical(ok$s$record$value[ok$s$record$quantity == "copy HEAD equal to the receipt's"], "TRUE"))
      sc <- rv("rv_spawn_command")
      cmd <- sc("/fx/copy root", "tar_make", V_WS)
      stopifnot(startsWith(cmd, "cd '/fx/copy root' && "),
                grepl(paste0('script = "', V_WS, '/runner.R"'), cmd, fixed = TRUE),
                startsWith(sc("/fx/copy root", "census", V_WS, "review_fixture", "1791072000"),
                           paste0("cd '/fx/copy root/", V_WS, "/census/report' && ")),
                startsWith(sc("/fx/copy root", "tectonic", V_WS, tectonic = "/usr/bin/tectonic"),
                           paste0("cd '/fx/copy root/", V_WS, "/census/report' && ")))
    },
    "review (V): V19 file comparison: pairing by label and stamp, expansion by the gate's function, classes" = function() {
      A <- file.path(FX, "v19a"); C <- file.path(FX, "v19c"); W <- "_targets_review_X"
      dir.create(A); dir.create(C); A <- normalizePath(A); C <- normalizePath(C)
      both <- function(rel, a, b = a, fresh_rel = rel) { vw(A, rel, a); vw(C, file.path(W, fresh_rel), b) }
      s1 <- "bootstrap_run_20261003T065219Z_b11484528f45d"; s2 <- "bootstrap_run_20261010T101010Z_0123456789abc"
      both("pilot/m/r.ext", c("TABLE", "1 0.5"))
      both("pilot/m/bad.ext", c("TABLE", "1 0.5"), c("TABLE", "1 0.6"))
      both("pilot/m/r.lst", c("run", paste0("dir ", s1)), c("run", paste0("dir ", s2)))
      vw(A, "pilot/m/b.bin", "aaaa"); vw(C, file.path(W, "pilot/m/b.bin"), "aaab")
      both("pilot/m/vol.yaml", "a: 1", "a: 2")
      vw(A, "pilot/m/only.csv", "x")
      both("pilot/m/command.txt", paste0("psn ", A, "/pilot/m/x.mod"), paste0("psn ", C, "/", W, "/pilot/m/x.mod"))
      both(file.path("pilot/q", s1, "raw.csv"), "1,2", fresh_rel = file.path("pilot/q", s2, "raw.csv"))
      both("pilot/d/one.csv", "1"); both("pilot/d/two.csv", "2"); vw(C, file.path(W, "pilot/d/three.csv"), "3")
      both("pilot/s/a.csv", "a"); both("pilot/s/b.csv", "b")
      vw(C, "pilot/up/u.ext", "u"); vw(C, file.path(W, "pilot/up/u.ext"), "u")   # (e) reads the copy
      art <- function(ws) data.frame(
        artifact = c("ext", "bad", "lst", "bin", "vol", "only", "cmd", "stamped", "dir", "pair", "lonely"),
        path = paste0(ws, c("pilot/m/r.ext", "pilot/m/bad.ext", "pilot/m/r.lst", "pilot/m/b.bin",
                            "pilot/m/vol.yaml", "pilot/m/only.csv", "pilot/m/command.txt",
                            paste0("pilot/q/", if (nzchar(ws)) s2 else s1), "pilot/d",
                            paste0(ws, "pilot/s/a.csv; ", ws, "pilot/s/b.csv"), "pilot/m/r.ext")),
        source = "fixture", stringsAsFactors = FALSE)
      an <- list(x_package = list(artifacts = art("")))
      fr <- list(x_package = list(artifacts = art(paste0(W, "/"))[-11, ]))
      an$x_package$artifacts$path[10] <- "pilot/s/a.csv; pilot/s/b.csv"
      fr$x_package$artifacts$path[10] <- paste0(W, "/pilot/s/a.csv; ", W, "/pilot/s/b.csv")
      # A review package in the analysis snapshot is skipped, never paired (QA F-5).
      an$review_package <- list(artifacts = data.frame(artifact = "rv", path = "pilot/m/r.ext", source = "fixture",
                                                       stringsAsFactors = FALSE))
      sp <- v_spec(ex = A, cp = C, ws = W)
      r <- rv("rv_compare_files")(an, fr, c(examined = A, copy = C), "pilot", W, sp,
                                  volatile = c("file:pilot/m/vol.yaml" = "fixture"), upstream = "pilot/up/u.ext")
      t <- r$table
      m <- function(it, f = NULL) { k <- t$item == paste0("x_package / ", it); if (!is.null(f)) k <- k & t$file == f; t$match[k] }
      stopifnot(isTRUE(m("ext")), isFALSE(m("bad")), isTRUE(m("lst")), isFALSE(m("bin")),
                identical(t$class[t$item == "x_package / bin"], "sha256"),
                is.na(m("vol")), grepl("analysis .*; fresh ", t$detail[t$item == "x_package / vol"]),
                isFALSE(m("only")), isTRUE(m("cmd")),
                isTRUE(m("stamped", "pilot/q/bootstrap<volatile>/raw.csv")),
                isTRUE(m("dir", "pilot/d/one.csv")), isFALSE(m("dir", "pilot/d/three.csv")),
                isTRUE(m("pair", "pilot/s/a.csv")), isTRUE(m("pair", "pilot/s/b.csv")),
                isFALSE(m("lonely")), isTRUE(t$match[t$item == "declared upstream file"]),
                !any(startsWith(t$item, "review_package")),
                !length(ls(sp$counter)),   # the file comparison counts nothing (m-6)
                all(r$outcomes$rule == "file_mismatch"))
      # The stamp token keeps the tool name: a bootstrap and a vpc name stay apart (N-3).
      nm2 <- rv("rv_normalize")(c(file.path("pilot/q", s1), sub("bootstrap", "vpc", file.path("pilot/q", s1))), v_spec())
      stopifnot(!identical(nm2[1], nm2[2]))
      # Declared upstream files: an NA leaf of the declaration never becomes a path (M-1).
      inp <- list(a = list(out_dir = "pilot/a", src = "pilot/up/u.ext", heading = NA, note = "pilot/up/two words", n = 3),
                  b = list(model_dir = "pilot/up"))
      stopifnot(identical(rv("rv_upstream_files")(inp, "pilot/up"), "pilot/up/u.ext"))
      # An artifact row under no remapped location whose fresh partner lies in the
      # workspace (a fixture figure) is compared under its rule (M-2).
      mkpdf <- function(f, txt) {
        dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
        grDevices::pdf(f); graphics::plot.new(); graphics::text(0.5, 0.5, txt); invisible(grDevices::dev.off())
      }
      fo <- function(ws) list(reporting_package = list(artifacts = data.frame(
        artifact = "explicit.pdf", path = paste0(ws, "pilot/f/outputs/explicit.pdf"), source = "fixture",
        stringsAsFactors = FALSE)))
      fcmp <- function() rv("rv_compare_files")(fo(""), fo(paste0(W, "/")), c(examined = A, copy = C), "pilot/m", W,
                                                v_spec(ex = A, cp = C, ws = W), rf11 = "explicit.pdf")
      mkpdf(file.path(A, "pilot/f/outputs/explicit.pdf"), "Fixture value 1.01")
      mkpdf(file.path(C, W, "pilot/f/outputs/explicit.pdf"), "Fixture value 1.01")
      rf <- fcmp()
      stopifnot(identical(rf$table$class, "pdf, R-F11 text rule"), isTRUE(rf$table$match))
      mkpdf(file.path(C, W, "pilot/f/outputs/explicit.pdf"), "Fixture value 1.02")
      rf <- fcmp()
      stopifnot(isFALSE(rf$table$match), identical(rf$outcomes$rule, "text_rule"))
      # A fresh row in the workspace, under no location, with no analysis partner
      # is reported (QA F-1).
      fx <- fo(paste0(W, "/"))
      fx$reporting_package$artifacts <- rbind(fx$reporting_package$artifacts, data.frame(
        artifact = "extra.pdf", path = paste0(W, "/pilot/f/outputs/extra.pdf"), source = "fixture"))
      rf <- rv("rv_compare_files")(fo(""), fx, c(examined = A, copy = C), "pilot/m", W, v_spec(ex = A, cp = C, ws = W))
      stopifnot(identical(rf$table$detail[rf$table$item == "reporting_package / extra.pdf"],
                          "fresh row with no analysis partner"))
    },
    "review (V): V19b PDF text is normalized before comparison: a workspace-prefixed path and a CPU time line match (F12, F13)" = function() {
      A <- file.path(FX, "v19ba"); C <- file.path(FX, "v19bc"); W <- "_targets_review_X"
      dir.create(A); dir.create(C); A <- normalizePath(A); C <- normalizePath(C)
      mkpdf <- function(f, l1, l2) {
        dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
        grDevices::pdf(f); graphics::plot.new()
        # Left-aligned, as the MAR bullet lines are: pdftotext layout columns stay put.
        graphics::text(0, 0.7, l1, adj = 0); graphics::text(0, 0.3, l2, adj = 0); invisible(grDevices::dev.off())
      }
      for (nm in c("mar.pdf", "plot.pdf")) {
        mkpdf(file.path(A, "pilot/f/report", nm), "Artifact pilot/f/outputs/a.pdf",
              "#CPUT: Total CPU Time in Seconds, 0.123")
        mkpdf(file.path(C, W, "pilot/f/report", nm), paste0("Artifact ", W, "/pilot/f/outputs/a.pdf"),
              "#CPUT: Total CPU Time in Seconds, 1.456")
      }
      fo <- function(ws) list(reporting_package = list(artifacts = data.frame(
        artifact = c("mar.pdf", "plot.pdf"), path = paste0(ws, "pilot/f/report/", c("mar.pdf", "plot.pdf")),
        source = "fixture", stringsAsFactors = FALSE)))
      # Apply the declared fixture patterns to both text comparison paths.
      sp <- v_spec(ex = A, cp = C, ws = W)
      sp$patterns <- sub("^re:", "", grep("^re:", names(v_block()$volatile), value = TRUE))
      t <- rv("rv_compare_files")(fo(""), fo(paste0(W, "/")), c(examined = A, copy = C), "pilot/m", W, sp, rf11 = "mar.pdf")$table
      stopifnot(identical(t$class, c("pdf, R-F11 text rule", "pdf, identical pdftotext text and pages")))
      if (!all(t$match %in% TRUE)) stop("normalized PDF text did not match: ", paste(t$detail, collapse = "; "))
    },
    "review (V): V20 checkout receipt lines are classified by subject and path part" = function() {
      perm <- c("modified:re:^docs/fixture-ledger\\.md$" = "ledger",
                "untracked:re:^gates/SYNTHETIC-REVIEW/sign-" = "bundles")
      rcpt <- c(paste("HEAD", strrep("a", 40)), "status  M docs/fixture-ledger.md",
                "status ?? docs/fixture-ledger.md", "status ?? gates/SYNTHETIC-REVIEW/sign-x/",
                "status  M pilot/other.R", "status R  a.R -> b.R",
                "record gates/SYNTHETIC-REVIEW/eda.md commits 2 after-sign 1")
      gi <- function(r) rv("rv_gate_integrity")(character(0), file.path(FX, "v20-absent.jsonl"), r, perm,
                                               "eda", "eda", "SYNTHETIC-REVIEW")
      r <- gi(rcpt)
      st <- r$checkout[r$checkout$kind == "status", ]
      stopifnot(identical(st$class, c(names(perm)[1], NA, names(perm)[2], NA, NA)),
                identical(st$code, c(" M", "??", "??", " M", "R ")),
                identical(st$path[5], "a.R; b.R"), identical(r$head, strrep("a", 40)),
                sum(r$outcomes$rule == "checkout") == 3L,
                sum(r$outcomes$rule == "history_after_sign") == 1L)
      for (bad in list(rcpt[-1], c("HEAD xyz", rcpt[-1]), character(0)))
        stopifnot(any(gi(bad)$outcomes$rule == "checkout" & grepl("HEAD|empty", gi(bad)$outcomes$finding)))
    },
    "review (V): V21 the checklist has the 18 skill rows; a row without an outcome is not evaluated and major" = function() {
      tg <- c("review_gate_integrity", "review_reproduction", "review_traceability", "review_map_concordance",
              "review_validation", "review_crossdoc")
      full <- stats::setNames(rep(list(list(outcomes = rv("rv_outcome")())), 6), tg)
      cl <- rv("rv_checklist")(full)
      stopifnot(nrow(cl) == 18L, identical(cl$skill_line, sort(cl$skill_line)), all(nzchar(cl$evaluated_by)),
                !any(cl$outcome == "not evaluated"), sum(cl$outcome == "reading item") == 4L)
      part <- full; part["review_validation"] <- list(NULL)
      f <- rv("rv_findings")(rv("rv_outcome")(), rv("rv_checklist")(part))
      stopifnot(sum(f$checklist$outcome == "not evaluated") == 3L, nrow(f$log) == 3L,
                all(f$log$severity == "major"), all(startsWith(f$log$checklist_item, "domain 4")),
                all(nzchar(f$checklist$finding[f$checklist$outcome == "not evaluated"])))
    },
    "review (V): V22 the composed findings and package commands retain typed evidence on stub inputs" = function() {
      tt <- v_env(c(PMX_REVIEW_REPRODUCTION = NA), vmod(v_block(), all = TRUE))
      d <- file.path(FX, "v22"); dir.create(d)
      old <- setwd(d); on.exit(setwd(old))
      ev <- new.env(parent = venv); ev$gate_reporting <- NULL
      oc <- list(outcomes = rv("rv_outcome")())
      for (n in c("review_gate_integrity", "review_traceability", "review_validation",
                  "review_crossdoc")) ev[[n]] <- oc
      ev$review_reproduction <- rv("rv_reproduction_stub")()
      f <- eval(vcmd(tt, "review_findings"), ev)
      stopifnot(file.exists(file.path(d, "examples/review-fixture/review/findings-log.csv")), nrow(f$checklist) == 18L)
      ev$review_findings <- f
      ev$review_gate_integrity <- list(checkout = data.frame(a = 1), records = data.frame(a = 1),
        ledger = data.frame(quantity = c("approvals ledger events", "approvals ledger sha256"),
                            value = c("3", strrep("a", 64)), stringsAsFactors = FALSE))
      ev$review_traceability <- list(promotions = data.frame(a = 1), numbers = data.frame(a = 1), inventory = data.frame(a = 1))
      for (n in c("review_validation", "review_crossdoc")) ev[[n]] <- list(table = data.frame(a = 1))
      ev$review_checkout_file <- v_block()$checkout
      ev$review_assessment_file <- v_block()$assessment
      ev$review_map_file <- v_block()$map
      p <- eval(vcmd(tt, "review_package"), ev)
      stopifnot(identical(p$analysis, "review_fixture"), identical(p$task, "review"),
                identical(p$artifacts$path, c(v_block()$checkout, v_block()$assessment,
                  v_block()$map, file.path(v_block()$out_dir, "findings-log.csv"))),
                identical(p$tables[["Findings"]], f$log),
                identical(p$tables[["Checklist"]], f$checklist),
                identical(p$tables[["Approvals ledger"]], ev$review_gate_integrity$ledger),
                identical(p$notes[[1]]$kind, "authored"),
                grepl("findings", p$notes[[1]]$text, fixed = TRUE))
      ev$review_reproduction$skipped <- FALSE
      ev$review_reproduction$files <- data.frame(class = "fixture comparison", match = TRUE)
      p <- eval(vcmd(tt, "review_package"), ev)
      stopifnot(all(c("tree-before.tsv", "tree-after.tsv", "copy-tree-after.tsv",
                      "reproduction-record.csv", "file-comparison.csv", "text-comparison.csv",
                      "tar_make.log") %in% p$artifacts$artifact),
                identical(p$tables[["Artifact comparison"]], ev$review_reproduction$files))
      # The reproduction command stops on an incomplete fresh run (QA F-3): the
      # setup and the spawn are replaced in the evaluation environment, the
      # outcome rule is the real one, and a killed child (status 137) is incomplete.
      ev$rv_inside_reproduction <- function() FALSE
      ev$rv_reproduce_setup <- function(...) list(
        expected_errors = "gate_review", edges = data.frame(from = "r1", to = "gate_review"),
        outside = character(0), tree_before = data.frame(path = character(0)),
        record = data.frame(quantity = character(0), value = character(0)))
      ev$rv_reproduce_run <- function(s) list(
        exit_status = 137L, meta = data.frame(name = character(0), type = character(0), error = character(0)),
        log = file.path(d, "absent.log"), start = "s", end = "e")
      stops(eval(vcmd(tt, "review_reproduction"), ev), "the fresh run is incomplete")
    },
    "review (V): V24 setup: a written gate record or method manifest is a recorded read; a written non-input stops" = function() {
      # The fixture module gains two commands naming a method manifest and a
      # parameter file by literal; the store tracks the gate record (as
      # report_record_files does), the manifest (as method_manifests does)
      # and, in the second case, writes the parameter file.
      edit <- function(x) {
        vw(x, "method-records/fx/MANIFEST.yaml", "name: fx")
        cat("task_targets <- c(task_targets, list(",
            "  targets::tar_target_raw('da_prior', quote(readLines('method-records/fx/MANIFEST.yaml'))),",
            "  targets::tar_target_raw('da_param', quote(readLines('examples/review-fixture/derived/param.R')))))",
            sep = "\n", file = file.path(x, "pipeline/tasks/data_assembly.R"), append = TRUE)
      }
      tracked <- c(", tar_target_raw('rec_files', quote('gates/review_fixture/data-assembly.md'), format = 'file')",
                   ", tar_target_raw('method_manifests', quote('method-records/fx/MANIFEST.yaml'), format = 'file')")
      s <- v_setup(v_case("v24-read", edit = edit, store = TRUE, store_extra = tracked), live = V_LIVE)
      cr <- s$census_reads
      stopifnot(nrow(cr) > 0L, all(cr$class == "path written by a computed file target"),
                any(cr$target == "gate_data_assembly_file" & cr$literal == "gates/review_fixture/data-assembly.md"),
                any(cr$target == "da_prior" & cr$literal == "method-records/fx/MANIFEST.yaml"),
                !any(cr$target == "da_param"),
                sum(grepl("^write census: tracked input, read", s$record$quantity)) == nrow(cr))
      param <- ", tar_target_raw('fx_param', quote({ dir.create('examples/review-fixture/derived', recursive = TRUE, showWarnings = FALSE); writeLines('x', 'examples/review-fixture/derived/param.R'); 'examples/review-fixture/derived/param.R' }), format = 'file')"
      stops(v_setup(v_case("v24-write", edit = edit, store = TRUE, store_extra = c(tracked, param)), live = V_LIVE),
            "write census hit: target da_param literal 'examples/review-fixture/derived/param.R' \\(path written by a computed file target; 1 hit\\(s\\)\\)")
    },
    "review (V): V25 rv_identity_paths unwraps nested blocks to the last expression" = function() {
      ip <- rv("rv_identity_paths")
      stopifnot(identical(ip('{ a; { b; identity("x/p.R") } }'), "x/p.R"),
                identical(ip('{ { identity(c("x", "y")) } }'), c("x", "y")),
                is.null(ip('{ a; { b; f("x/p.R") } }')),
                is.null(ip('{ a; { identity("x"); b } }')))
    },
    "review (V): V26 setup copies a gated identity input under a derived sibling of a declared location" = function() {
      # The module derives examples/review-fixture/derived from dirname(model_dir), as
      # reporting.R derives a sibling input from dirname(report_dir),
      # and gates it in a nested block, as reporting.R's gated() does (Q-B8-18).
      add <- function(x) cat("task_targets <- c(task_targets, list(",
        "  targets::tar_target_raw('da_param_file', bquote({ da_spec_file; identity(.(file.path(dirname(task_input$model_dir), 'derived', 'param.R'))) }), format = 'file')))",
        sep = "\n", file = file.path(x, "pipeline/tasks/data_assembly.R"), append = TRUE)
      s <- v_setup(v_case("v26-ok", edit = function(x) { add(x); vw(x, "examples/review-fixture/derived/param.R", "f <- 1") },
                          store = TRUE), live = V_LIVE)
      r <- s$inputs[s$inputs$target == "da_param_file", , drop = FALSE]
      stopifnot(identical(r$source, "examples/review-fixture/derived/param.R"),
                identical(r$dest, file.path(V_WS, "examples/review-fixture/derived/param.R")),
                file.exists(file.path(s$copy, r$dest)), sum(startsWith(s$inputs$target, "da_")) == 4L)
      stops(v_setup(v_case("v26-missing", edit = add, store = TRUE), live = V_LIVE),
            "missing required input examples/review-fixture/derived/param.R")
    },
    "review (V): V27 setup copies before composing: a definition-time path assertion in a module passes; a sibling asserted at definition time stops" = function() {
      # tarchetypes' tar_quarto_raw asserts its sources when the target is
      # defined (plan 14.4 item 6); the fixture module makes the same call.
      assert <- function(x, p) cat(sprintf("targets::tar_assert_path(%s)", p), sep = "\n",
                                   file = file.path(x, "pipeline/tasks/data_assembly.R"), append = TRUE)
      s <- v_setup(v_case("v27-ok", edit = function(x) assert(x, "file.path(task_input$out_dir, 'spec.yaml')"),
                          store = TRUE), live = V_LIVE)
      stopifnot(file.exists(file.path(s$ws, "examples/review-fixture/da/spec.yaml")), sum(startsWith(s$precopy$target, "da_")) == 3L)
      # The stated limit: a sibling under no location, asserted at definition
      # time, is not in the workspace when the derived pipeline is composed.
      sib <- function(x) {
        cat("task_targets <- c(task_targets, list(",
            "  targets::tar_target_raw('da_param_file', bquote({ da_spec_file; identity(.(file.path(dirname(task_input$model_dir), 'derived', 'param.R'))) }), format = 'file')))",
            sep = "\n", file = file.path(x, "pipeline/tasks/data_assembly.R"), append = TRUE)
        vw(x, "examples/review-fixture/derived/param.R", "f <- 1")
        assert(x, "file.path(dirname(task_input$model_dir), 'derived', 'param.R')")
      }
      stops(v_setup(v_case("v27-sibling", edit = sib, store = TRUE), live = V_LIVE),
            "does not compose after the pre-copy.*missing files: .*derived/param\\.R")
    },
    "review (V): V28 pass 1 skips declared kept values; a pre-copied path the derived pipeline does not read stops" = function() {
      decl <- v_decl(da = list(listing = "examples/review-fixture/model/201/201.lst"))
      target <- function(line) function(x) cat("task_targets <- c(task_targets, list(", line, sep = "\n",
                                                file = file.path(x, "pipeline/tasks/data_assembly.R"), append = TRUE)
      # Three kept values of three kinds; the .mod one is also a path the derived
      # pipeline reads under model_dir (the run-209 model shape): pass 1 skips it,
      # pass 2 copies it back.
      kept <- v_decl(da = list(listing = "examples/review-fixture/model/201/201.lst", ext = "examples/review-fixture/model/201/201.ext",
                               stream = "examples/review-fixture/model/201.mod"))
      s <- v_setup(v_case("v28-kept", decl = kept, store = TRUE,
                          edit = function(x) {
                            target(paste0("  targets::tar_target_raw('da_listing_file', call('identity', task_input$listing), format = 'file'),\n",
                                          "  targets::tar_target_raw('da_ext_kept', call('identity', task_input$ext), format = 'file'),\n",
                                          "  targets::tar_target_raw('da_mod_file', call('identity', file.path(task_input$model_dir, '201.mod')), format = 'file')))"))(x)
                            vw(x, "examples/review-fixture/model/201.mod", "a run model")
                            vw(x, "examples/review-fixture/model/201/201.ext", "a kept fixture input")
                          }),
                   live = V_LIVE)
      stopifnot(!file.exists(file.path(s$ws, "examples/review-fixture/model/201")),
                !"da_listing_file" %in% c(s$precopy$target, s$inputs$target), sum(startsWith(s$precopy$target, "da_")) == 3L,
                !"da_ext_kept" %in% c(s$precopy$target, s$inputs$target),
                "da_mod_file" %in% s$inputs$target, !"da_mod_file" %in% s$precopy$target,
                file.exists(file.path(s$ws, "examples/review-fixture/model/201.mod")))
      formed <- target("  targets::tar_target_raw('da_ext_file', call('identity', sub('[.]lst$', '.ext', task_input$listing)), format = 'file')))")
      stops(v_setup(v_case("v28-formed", decl = decl, store = TRUE,
                           edit = function(x) { formed(x); vw(x, "examples/review-fixture/model/201/201.ext", "a generated run file") }),
                    live = V_LIVE),
            "pre-copied input examples/review-fixture/model/201/201\\.ext \\(target da_ext_file\\) is not an input of the derived pipeline")
    },
    "review (V): V29 a column: entry masks declared columns of a rectangular CSV, counted per file and per entry; without one the table is today's" = function() {
      A <- file.path(FX, "v29a"); C <- file.path(FX, "v29c"); W <- "_targets_review_X"
      dir.create(A); dir.create(C); A <- normalizePath(A); C <- normalizePath(C)
      F <- "pilot/fx/qualification/raw_results_fx.csv"
      hdr <- "model,THETA1,THETA2,model_run_time,subprob_est_time,subprob_cov_time"
      put <- function(a, b) { vw(A, F, c(hdr, a)); vw(C, file.path(W, F), c(hdr, b)) }
      put(c("1,e1,e2,t01,s01,c01", "2,e3,e4,t02,s02,c02"), c("1,e1,e2,t11,s11,c11", "2,e3,e4,t12,s12,c12"))
      art <- function(ws) list(x_package = list(artifacts = data.frame(artifact = "raw", path = paste0(ws, F),
                                                                       source = "fixture", stringsAsFactors = FALSE)))
      cmp <- function(vol) rv("rv_compare_files")(art(""), art(paste0(W, "/")), c(examined = A, copy = C), "pilot", W,
                                                  v_spec(ex = A, cp = C, ws = W), volatile = vol)
      # 1. Regression: with no column: entry the table and outcomes are today's.
      today <- list(item = "x_package / raw", file = F, class = "text, line by line after normalization",
                    match = FALSE, detail = "3 and 3 lines; first differing line 2")
      today_out <- list(rule = "file_mismatch", checklist_item = "domain 3 bullet 3",
                        finding = paste0("file comparison mismatch: x_package / raw: ", F,
                                         " (text, line by line after normalization; 3 and 3 lines; first differing line 2)"),
                        evidence_path = "file-comparison.csv",
                        owner = "owning stage of pilot/fx/qualification/raw_results_fx.csv")
      for (vol in list(character(0), c("re:^never-matches-fx$" = "fixture", "file:pilot/fx/other.csv" = "fixture"))) {
        r <- cmp(vol)
        stopifnot(identical(as.list(r$table), today), identical(as.list(r$outcomes), today_out),
                  !any(grepl("declared columns excluded|column:", r$table$class)))
      }
      # 2. The three timing columns declared: the file matches, with its counts;
      # one live summary row per entry.
      cols <- c("model_run_time", "subprob_est_time", "subprob_cov_time")
      vol <- stats::setNames(rep("fixture timing column", 3), paste0("column:", cols, "@^pilot/fx/qualification/raw_results_"))
      counted <- "analysis header lines 1, cells replaced 2; fresh header lines 1, cells replaced 2"
      r <- cmp(vol)
      fr <- r$table[r$table$item == "x_package / raw", ]
      stopifnot(isTRUE(fr$match),
                identical(fr$class, "text, line by line after normalization, declared columns excluded"),
                identical(fr$detail, paste0("3 lines identical", paste0("; column ", cols, ": ", counted, collapse = ""))),
                nrow(r$outcomes) == 0L)
      er <- r$table[r$table$item == "volatile column entry", ]
      stopifnot(identical(er$file, names(vol)), all(er$class == "column: volatile entry (listed, not judged)"),
                all(is.na(er$match)), all(er$detail == paste0("matched files 1; ", counted)))
      # 3. A differing cell outside the declared columns still mismatches.
      put(c("1,e1,e2,t01,s01,c01", "2,e3,e4,t02,s02,c02"), c("1,e1,e9,t11,s11,c11", "2,e3,e4,t12,s12,c12"))
      r <- cmp(vol)
      fr <- r$table[r$table$item == "x_package / raw", ]
      stopifnot(isFALSE(fr$match), startsWith(fr$detail, "3 and 3 lines; first differing line 2; column model_run_time: "),
                identical(r$outcomes$rule, "file_mismatch"), identical(r$outcomes$owner, "owning stage of pilot/fx/qualification/raw_results_fx.csv"))
      # 4. The two helpers directly.
      stopifnot(identical(rv("rv_volatile_columns")(vol),
                          data.frame(entry = names(vol), column = cols, pattern = "^pilot/fx/qualification/raw_results_",
                                     stringsAsFactors = FALSE)))
      z <- rv("rv_volatile_columns")(c("re:x" = "a", "file:y" = "b", bare = "c"))
      stopifnot(identical(nrow(z), 0L), identical(names(z), c("entry", "column", "pattern")),
                all(vapply(z, is.character, logical(1))))
      m <- rv("rv_mask_columns")(c(hdr, "1,e1,e2,t01,s01,c01", "2,e3,e4,t02,s02,c02"), c("subprob_cov_time", "model_run_time"))
      stopifnot(identical(m$lines, c(hdr, "1,e1,e2,<volatile>,s01,<volatile>", "2,e3,e4,<volatile>,s02,<volatile>")),
                identical(m$counts, data.frame(column = c("subprob_cov_time", "model_run_time"), header_lines = c(1L, 1L),
                                               cells = c(2L, 2L), stringsAsFactors = FALSE)))
      m0 <- rv("rv_mask_columns")(c(hdr, "1,e1,e2,t01,s01,c01"), character(0))
      stopifnot(identical(m0$lines, c(hdr, "1,e1,e2,t01,s01,c01")), identical(nrow(m0$counts), 0L),
                identical(names(m0$counts), c("column", "header_lines", "cells")))
      # 5. A column: entry on a file that also matches a semantic manifest: the
      # suffix is appended to the digit-masked class and the mismatch keeps the
      # text_rule routing (plan note C2-1; code review CR1-1).
      SM <- "pilot/fx/report/fx-semantic-manifest.txt"
      vw(A, SM, c("key,stamp", "alpha,sa")); vw(C, file.path(W, SM), c("key,stamp", "beta,sb"))
      sm <- function(ws) list(x_package = list(artifacts = data.frame(artifact = "sm", path = paste0(ws, SM),
                                                                      source = "fixture", stringsAsFactors = FALSE)))
      r <- rv("rv_compare_files")(sm(""), sm(paste0(W, "/")), c(examined = A, copy = C), "pilot", W,
                                  v_spec(ex = A, cp = C, ws = W), volatile = c("column:stamp@^pilot/fx/report/" = "fixture"))
      fr <- r$table[r$table$file == SM, ]
      stopifnot(isFALSE(fr$match), endsWith(fr$class, ", declared columns excluded"),
                grepl("digits masked", fr$class, fixed = TRUE), identical(r$outcomes$rule, "text_rule"))
    },
    "review (V): V30 column: entries on sectioned CSVs: the body ends at a field-count change, an empty line or a broken quoted line; quoted fields keep the index" = function() {
      A <- file.path(FX, "v30a"); C <- file.path(FX, "v30c"); W <- "_targets_review_X"
      dir.create(A); dir.create(C); A <- normalizePath(A); C <- normalizePath(C)
      BS <- "pilot/fx/qualification/bootstrap_results.csv"; VP <- "pilot/fx/qualification/vpc_results.csv"
      # A quoted field with a comma in the header, a doubled quote and a quoted
      # comma in the body: a plain comma split would shift the index by one.
      bhdr <- 'id,"name, with comma",note,model.run.time,subprob.est.time,next.col'
      bs <- function(t1, t2, nxt = "N", later = "q1")
        c("diagnostic.means", bhdr, paste0('1,"say ""hi""",plain,', t1, ",", t2, ',"', nxt, ', 1"'),
          "standard.errors", 'id,"name, with comma",note,other.a,other.b,other.c,other.d',
          paste0('1,"say ""hi""",plain,', later, ",x,y,z"))
      vp <- function(d, later = "L1") c("VPC run info", '"Date","Run, info",modelfile', paste0(d, ',"r, 1",m.mod'), "",
                                        paste0(later, ',"r, 1",m.mod'))
      put <- function(a_bs, b_bs, a_vp, b_vp) {
        vw(A, BS, a_bs); vw(C, file.path(W, BS), b_bs); vw(A, VP, a_vp); vw(C, file.path(W, VP), b_vp)
      }
      art <- function(ws) list(x_package = list(artifacts = data.frame(artifact = c("bs", "vpc"), path = paste0(ws, c(BS, VP)),
                                                                       source = "fixture", stringsAsFactors = FALSE)))
      vol <- c("column:model.run.time@^pilot/fx/qualification/bootstrap_results" = "fixture",
               "column:subprob.est.time@^pilot/fx/qualification/bootstrap_results" = "fixture",
               "column:Date@^pilot/fx/qualification/vpc_results" = "fixture")
      cmp <- function() rv("rv_compare_files")(art(""), art(paste0(W, "/")), c(examined = A, copy = C), "pilot", W,
                                               v_spec(ex = A, cp = C, ws = W), volatile = vol)
      row_of <- function(r, f) r$table[r$table$file == f, ]
      one <- "analysis header lines 1, cells replaced 1; fresh header lines 1, cells replaced 1"
      # The declared cells differ on both files: excluded and counted on both sides.
      put(bs("T1", "T2"), bs("T7", "T8"), vp("D1"), vp("D2"))
      r <- cmp()
      stopifnot(isTRUE(row_of(r, BS)$match), isTRUE(row_of(r, VP)$match), nrow(r$outcomes) == 0L,
                identical(row_of(r, BS)$detail, paste0("6 lines identical; column model.run.time: ", one,
                                                       "; column subprob.est.time: ", one)),
                identical(row_of(r, VP)$detail, paste0("5 lines identical; column Date: ", one)))
      # A cell at the declared position in a later section still mismatches.
      put(bs("T1", "T2"), bs("T7", "T8", later = "q2"), vp("D1"), vp("D2", later = "L2"))
      r <- cmp()
      stopifnot(isFALSE(row_of(r, BS)$match), grepl("first differing line 6", row_of(r, BS)$detail),
                isFALSE(row_of(r, VP)$match), grepl("first differing line 5", row_of(r, VP)$detail))
      # A cell in the column next to the declared one still mismatches.
      put(bs("T1", "T2"), bs("T7", "T8", nxt = "M"), vp("D1"), vp("D2"))
      r <- cmp()
      stopifnot(isFALSE(row_of(r, BS)$match), grepl("first differing line 3", row_of(r, BS)$detail),
                isTRUE(row_of(r, VP)$match))
      # A body line broken inside a quoted field ends the body: compared, not masked.
      broken <- function(t1) c(bhdr, '1,"say ""hi""",plain,"broken', paste0(' field",', t1, ',T2,"N, 1"'))
      put(broken("T1"), broken("T7"), vp("D1"), vp("D2"))
      r <- cmp()
      b <- row_of(r, BS)
      stopifnot(isFALSE(b$match), identical(b$class, "text, line by line after normalization"),
                identical(b$detail, "3 and 3 lines; first differing line 3"),
                identical(r$table$detail[r$table$file == names(vol)[1]],
                          "matched files 1; analysis header lines 1, cells replaced 0; fresh header lines 1, cells replaced 0"))
      # A quote opens a quoted field only at a field start (C2-4); trimws and one
      # pair of quotes are removed from a header field.
      m <- rv("rv_mask_columns")(c("a,b,c", 'x"y,z"w,1', 'p"q,1,2'), "b")
      stopifnot(identical(m$lines, c("a,b,c", 'x"y,<volatile>,1', 'p"q,1,2')), identical(m$counts$cells, 1L))
      m <- rv("rv_mask_columns")(c(' "b"\t,a', "1,2"), "b")
      stopifnot(identical(m$lines, c(' "b"\t,a', "<volatile>,2")), identical(m$counts$header_lines, 1L))
      # A one-column file: the empty line ends the body even though a field
      # count cannot tell it apart; the line after it is not masked (QA1-1).
      m <- rv("rv_mask_columns")(c("Date", "D1", "", "L1"), "Date")
      stopifnot(identical(m$lines, c("Date", "<volatile>", "", "L1")), identical(m$counts$cells, 1L))
      # A header naming the column at two positions: both cells are replaced
      # and counted (QA2-2; plan D-4 rule 2, every such field position).
      m <- rv("rv_mask_columns")(c("a,a", "1,2"), "a")
      stopifnot(identical(m$counts$cells, 2L))
      # Two entries naming the same column match the same file: the column is
      # masked and reported once in the file detail, and each entry is credited
      # with the file's counts (QA1-3; plan D-1 distinct columns, 3.3 entry row).
      put(bs("T1", "T2"), bs("T7", "T8"), vp("D1"), vp("D2"))
      r <- rv("rv_compare_files")(art(""), art(paste0(W, "/")), c(examined = A, copy = C), "pilot", W,
                                  v_spec(ex = A, cp = C, ws = W),
                                  volatile = c(vol, "column:Date@^pilot/fx/qualification/vpc_" = "fixture"))
      stopifnot(isTRUE(row_of(r, VP)$match), identical(row_of(r, VP)$detail, paste0("5 lines identical; column Date: ", one)),
                identical(r$table$detail[r$table$item == "volatile column entry"][3:4],
                          rep(paste0("matched files 1; ", one), 2L)))
    },
    "review (V): V31 column: entries: compose-time refusals; a dead entry is a critical outcome row owned by the declaration; the store comparison ignores them" = function() {
      # 1. The five compose-time refusals, each naming the entry; a valid entry composes.
      base <- v_block()$volatile
      bad <- list(c("column:model_run_time", "entry 'column:model_run_time' has no '@'"),
                  c("column:@^pilot/fx/", "entry 'column:@\\^pilot/fx/' has an empty column name"),
                  c("column:model_run_time@", "entry 'column:model_run_time@' has an empty file pattern"),
                  c("column:model_run_time@pilot/fx/", "entry 'column:model_run_time@pilot/fx/' has a file pattern not anchored with \\^"),
                  c("column:model_run_time@^pilot/fx/(",
                    "entry 'column:model_run_time@\\^pilot/fx/\\(' has a file pattern that is not a valid Perl regular expression"))
      for (b in bad)
        v_refuses(v_block(volatile = c(base, stats::setNames("fixture", b[1]))), paste0("^volatile: ", b[2]))
      ok <- v_env(c(PMX_REVIEW_REPRODUCTION = NA),
                  vmod(v_block(volatile = c(base, "column:model_run_time@^pilot/fx/" = "fixture"))))
      stopifnot(setequal(ok, V_NAMES))
      # 2. Every dead-entry cause gives a critical row owned by the declaration.
      A <- file.path(FX, "v31a"); C <- file.path(FX, "v31c"); W <- "_targets_review_X"
      dir.create(A); dir.create(C); A <- normalizePath(A); C <- normalizePath(C)
      Q <- "pilot/fx/qualification/"
      same <- function(rel, lines) { vw(A, paste0(Q, rel), lines); vw(C, file.path(W, paste0(Q, rel)), lines) }
      same("raw_results_fx.csv", c("model,model_run_time", "1,t1"))
      same("nobody_empty.csv", c("a,lonely_col", "", "1,2"))
      same("nobody_eof.csv", "a,eof_col")
      same("listed.csv", c("a,model_run_time", "1,t1"))
      same("blob.bin", c("a,model_run_time", "1,t1"))
      files <- c("raw_results_fx.csv", "nobody_empty.csv", "nobody_eof.csv", "listed.csv", "blob.bin")
      dead <- c("column:absent_col@^pilot/fx/qualification/raw_results_",     # no header names the column
                "column:model_run_time@^pilot/fx/nomatch/",                    # no compared file matches
                "column:lonely_col@^pilot/fx/qualification/nobody_empty",      # header, then an empty line
                "column:eof_col@^pilot/fx/qualification/nobody_eof",           # header, then the end of the file
                "column:model_run_time@^pilot/fx/qualification/listed",        # matches only a file: entry
                "column:model_run_time@^pilot/fx/qualification/blob")          # matches only a sha256-class file
      vol <- c(stats::setNames(rep("fixture", length(dead)), dead), "file:pilot/fx/qualification/listed.csv" = "fixture")
      art <- function(ws) list(x_package = list(artifacts = data.frame(artifact = files, path = paste0(ws, Q, files),
                                                                       source = "fixture", stringsAsFactors = FALSE)))
      r <- rv("rv_compare_files")(art(""), art(paste0(W, "/")), c(examined = A, copy = C), "pilot", W,
                                  v_spec(ex = A, cp = C, ws = W), volatile = vol)
      er <- r$table[r$table$item == "volatile column entry", ]
      zero <- function(f, h) sprintf("matched files %d; analysis header lines %d, cells replaced 0; fresh header lines %d, cells replaced 0", f, h, h)
      stopifnot(identical(er$file, dead), all(er$class == "column: volatile entry, excluded nothing"), all(er$match %in% FALSE),
                identical(er$detail, c(zero(1L, 0L), zero(0L, 0L), zero(1L, 1L), zero(1L, 1L), zero(0L, 0L), zero(0L, 0L))),
                identical(r$outcomes$rule, rep("file_mismatch", 6L)),
                all(r$outcomes$owner == "root (review declaration)"),
                identical(r$outcomes$finding, paste0("file comparison mismatch: volatile column entry: ", dead,
                                                     " (column: volatile entry, excluded nothing; ", er$detail, ")")),
                all(r$table$match[r$table$item != "volatile column entry"] %in% c(TRUE, NA)))
      # 3. Through the findings log: one critical row owned by the declaration.
      cl <- rv("rv_checklist")(stats::setNames(rep(list(list(outcomes = NULL)), 6),
        c("review_gate_integrity", "review_reproduction", "review_traceability", "review_map_concordance",
          "review_validation", "review_crossdoc")))
      f <- rv("rv_findings")(r$outcomes[1, ], cl)
      stopifnot(nrow(f$log) == 1L, identical(f$log$severity, "critical"), identical(f$log$owner, "root (review declaration)"),
                grepl(dead[1], f$log$finding, fixed = TRUE))
      # 4. The store comparison treats a column: name as no element name.
      ent <- "column:model_run_time@^pilot/fx/"
      snap <- function(v) list(
        x_package = list(analysis = "FX", task = "x",
                         tables = list(t1 = stats::setNames(data.frame(1, v, stringsAsFactors = FALSE), c("a", ent))),
                         artifacts = data.frame(artifact = "a", path = "pilot/fx/a.csv"), notes = list()),
        dataset = data.frame(ID = 1), environment_attestation = list(fingerprint = "f", probe = list(lib = "l")))
      s <- rv("rv_compare_stores")(snap("v1"), snap("v2"), v_spec(), c(stats::setNames("fixture", ent), accessed_at = "fixture"))
      sb <- s$table[s$table$match %in% FALSE, ]
      stopifnot(!ent %in% s$excluded$entry, "accessed_at" %in% s$excluded$entry, nrow(sb) == 1L,
                grepl("x_package: tables\\[t1\\]", sb$item))
      # 6. A header on one side only: cells replaced summed over both sides is
      # at least one, so the entry is live, not dead, whichever side holds the
      # header (QA1-2; plan 3.3 live and dead entry, AC-9).
      vw(A, paste0(Q, "onesided_a.csv"), c("a,stamp_col", "1,x"))
      vw(C, file.path(W, paste0(Q, "onesided_a.csv")), c("a,b", "1,x"))
      vw(A, paste0(Q, "onesided_f.csv"), c("a,b", "1,x"))
      vw(C, file.path(W, paste0(Q, "onesided_f.csv")), c("a,stamp_col", "1,x"))
      one <- c("onesided_a.csv", "onesided_f.csv")
      ent1 <- paste0("column:stamp_col@^pilot/fx/qualification/", sub("[.]csv$", "", one))
      art1 <- function(ws) list(x_package = list(artifacts = data.frame(artifact = one, path = paste0(ws, Q, one),
                                                                        source = "fixture", stringsAsFactors = FALSE)))
      r1 <- rv("rv_compare_files")(art1(""), art1(paste0(W, "/")), c(examined = A, copy = C), "pilot", W,
                                   v_spec(ex = A, cp = C, ws = W), volatile = stats::setNames(rep("fixture", 2L), ent1))
      er1 <- r1$table[r1$table$item == "volatile column entry", ]
      stopifnot(identical(er1$file, ent1), all(er1$class == "column: volatile entry (listed, not judged)"), all(is.na(er1$match)),
                identical(er1$detail, c("matched files 1; analysis header lines 1, cells replaced 1; fresh header lines 0, cells replaced 0",
                                        "matched files 1; analysis header lines 0, cells replaced 0; fresh header lines 1, cells replaced 1")),
                !"root (review declaration)" %in% r1$outcomes$owner)
      # The two file rows: a cell replaced on either side marks the class and
      # adds the per-side column note (QA2-1; plan 3.3 file row, AC-7).
      fr1 <- r1$table[match(paste0(Q, one), r1$table$file), ]
      stopifnot(all(fr1$class == "text, line by line after normalization, declared columns excluded"),
                identical(fr1$detail, paste0("2 and 2 lines; first differing line 1; column stamp_col: ",
                  c("analysis header lines 1, cells replaced 1; fresh header lines 0, cells replaced 0",
                    "analysis header lines 0, cells replaced 0; fresh header lines 1, cells replaced 1"))))
    }))

  # ---- Part V, AC2 ----
  # Wrap-aware volatile patterns in PDF text: rv_join_volatile and its use in
  # the two PDF branches of rv_compare_files. Each case opens with
  # rv_compare_files on fixture PDFs, so before the helper exists it fails on
  # a behavioural assertion, never on a missing object (AC-11). Fixture text
  # is synthetic; non-ASCII characters are written as escapes.
  # Fixture PDFs as V19b makes them: one page per element of `pages`,
  # left-aligned lines 0.03 apart (adjacent pdftotext layout lines); `xs`
  # gives per-line left offsets, one vector per page.
  vj_pdf <- function(f, pages, xs = NULL) {
    dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
    grDevices::pdf(f)
    for (k in seq_along(pages)) {
      graphics::plot.new()
      x <- if (is.null(xs)) rep(0, length(pages[[k]])) else xs[[k]]
      for (i in seq_along(pages[[k]]))
        graphics::text(x[i], 0.9 - 0.03 * (i - 1L), pages[[k]][i], adj = 0, xpd = NA)
    }
    invisible(grDevices::dev.off())
  }
  # Pick one fixture pattern by the start of its text.
  vj_pats <- function() sub("^re:", "", grep("^re:", names(v_block()$volatile), value = TRUE))
  vj_p <- function(head) {
    p <- vj_pats()
    p <- p[startsWith(p, head)]
    stopifnot(length(p) == 1L)
    p
  }
  # rv_compare_files on fixture files of the reporting package: PDFs `pdfs`
  # (analysis pages `an`, fresh pages `fr`) and text files `text` (a named
  # list of list(an = <lines>, fr = <lines>)), with the stage's spec and the
  # fixture patterns.
  vj_cmp <- function(tag, an, fr, pdfs = c("mar.pdf", "plot.pdf"), an_x = NULL, fr_x = NULL,
                     volatile = character(0), text = list()) {
    A <- file.path(FX, paste0(tag, "a")); C <- file.path(FX, paste0(tag, "c")); W <- "_targets_review_X"
    dir.create(A); dir.create(C); A <- normalizePath(A); C <- normalizePath(C)
    for (nm in pdfs) {
      vj_pdf(file.path(A, "pilot/f/report", nm), an, an_x)
      vj_pdf(file.path(C, W, "pilot/f/report", nm), fr, fr_x)
    }
    for (nm in names(text)) {
      vw(A, file.path("pilot/f/report", nm), text[[nm]]$an)
      vw(C, file.path(W, "pilot/f/report", nm), text[[nm]]$fr)
    }
    files <- c(pdfs, names(text))
    fo <- function(ws) list(reporting_package = list(artifacts = data.frame(
      artifact = files, path = paste0(ws, "pilot/f/report/", files), source = "fixture",
      stringsAsFactors = FALSE)))
    sp <- v_spec(ex = A, cp = C, ws = W)
    sp$patterns <- vj_pats()
    rv("rv_compare_files")(fo(""), fo(paste0(W, "/")), c(examined = A, copy = C), "pilot/m", W, sp,
                           volatile = volatile, rf11 = "mar.pdf")
  }
  # Character fixtures pass rv_normalize with the same spec first, as the PDF
  # path does (plan Section 7); vj_item asserts the lines and the counts of
  # one item exactly and returns the result with its patterns, for the
  # second pass of V33 item 4.
  vj_sp <- function(pats) list(fixed = character(0), patterns = pats, token = "<volatile>")
  vj_run <- function(lines, pats) {
    sp <- vj_sp(pats)
    rv("rv_join_volatile")(rv("rv_normalize")(lines, sp), sp)
  }
  vj_check <- function(id, r, lines, counts) {
    if (!identical(r$lines, lines) || !identical(unname(r$counts), as.integer(counts)))
      stop(id, ": lines ", paste(encodeString(r$lines, quote = "'"), collapse = ", "),
           "; counts ", paste(r$counts, collapse = ", "))
    invisible(r)
  }
  vj_item <- function(id, lines, pats, want, counts) {
    r <- vj_run(lines, pats)
    vj_check(id, r, want, counts)
    list(r = r, pats = pats)
  }
  # Runs `expr` under LC_CTYPE en_US.UTF-8 (the switch asserted), then restores.
  vj_utf8 <- function(expr) {
    old <- Sys.getlocale("LC_CTYPE")
    on.exit(Sys.setlocale("LC_CTYPE", old), add = TRUE)
    stopifnot(nzchar(Sys.setlocale("LC_CTYPE", "en_US.UTF-8")))
    force(expr)
  }
  VJ_ROWS <- c("pages 1 and 1; decimals equal TRUE; differing lines 0 and 0", "text identical")

  cases <- c(cases, list(
    "review (V): V32 a wrapped listing stamp and CPU label in PDF text are joined across one line break and counted per side in the detail (AC2 S1)" = function() {
      P1 <- vj_p("(?:Mon"); P5 <- vj_p("(#CPUT")
      one <- function(d, t, x) list(c(paste0("Run record | Tue Oct ", d, " ", t, " UTC 2026 | $PROBLEM fixture"),
                                      paste0("#CPUT: Total CPU Time in Seconds, ", x, " | Stop")))
      wrap <- function(d, t, x, w = "fixture") list(c(paste0("Run record | Tue Oct ", d),
                                                      paste0(t, " UTC 2026 | $PROBLEM ", w),
                                                      "#CPUT: Total CPU Time in", paste0("Seconds, ", x, " | Stop")))
      # 1. Stamp and label each on one line: today's rows, no joined part (E3, E8).
      t1 <- vj_cmp("v32o", one("13", "10:11:12", "0.123"), one("14", "11:12:13", "4.567"))$table
      stopifnot(identical(t1$class, c("pdf, R-F11 text rule", "pdf, identical pdftotext text and pages")),
                identical(t1$match, c(TRUE, TRUE)), identical(t1$detail, VJ_ROWS))
      # 2. Wrapped: both rows match and name both spanning entries per side (E1, E4, E9).
      t2 <- vj_cmp("v32w", wrap("13", "10:11:12", "0.123"), wrap("14", "11:12:13", "4.567"))$table
      if (!identical(t2$match, c(TRUE, TRUE)))
        stop("wrapped stamp and CPU label rows do not match: ", paste(t2$detail, collapse = " / "))
      suf <- paste0("; joined across one line break, re:", c(P1, P5), ": analysis 1, fresh 1", collapse = "")
      stopifnot(identical(t2$class, t1$class), identical(t2$detail, paste0(VJ_ROWS, suf)))
      # 3. A word after the stamp on the second line differs: still a mismatch (E10).
      t3 <- vj_cmp("v32d", wrap("13", "10:11:12", "0.123"), wrap("14", "11:12:13", "4.567", "other"))$table
      stopifnot(identical(t3$match, c(FALSE, FALSE)))
      # 4. Direct: as many lines out as in; counts named by the patterns, in order.
      pats <- vj_pats(); sp <- vj_sp(pats)
      f <- file.path(FX, "v32wa", "pilot/f/report/mar.pdf")
      l <- rv("rv_normalize")(suppressWarnings(system2("pdftotext", c("-layout", shQuote(f), "-"),
                                                       stdout = TRUE, stderr = FALSE)), sp)
      r <- rv("rv_join_volatile")(l, sp)
      stopifnot(length(r$lines) == length(l), identical(names(r$counts), pats),
                identical(unname(r$counts), as.integer(pats %in% c(P1, P5))))
    },
    "review (V): V33 the join bound: page guard, one-space fold, start and end tests, guard G-1 put-back, ceilings, idempotence (AC2 S2)" = function() {
      P1 <- vj_p("(?:Mon"); P5 <- vj_p("(#CPUT"); P6 <- vj_p("(Generated ")
      # 0. End to end first: two chained CPU spans, the middle line indented (E1, M08).
      three <- function(x1, x2) list(c("Phase one #CPUT: Total CPU Time in",
                                       paste0("Seconds, ", x1, " | Phase two #CPUT: Total CPU Time in"),
                                       paste0("Seconds, ", x2, " | Stop")))
      xs <- list(c(0, 0.1, 0))
      t0 <- vj_cmp("v33e", three("0.123", "0.456"), three("7.891", "2.345"), an_x = xs, fr_x = xs)$table
      if (!identical(t0$match, c(TRUE, TRUE)))
        stop("two chained CPU spans do not match: ", paste(t0$detail, collapse = " / "))
      stopifnot(identical(t0$detail, paste0(VJ_ROWS, "; joined across one line break, re:", P5,
                                            ": analysis 2, fresh 2")))
      ST <- "(Stamp:) [0-9]+"; SF <- "(Stamp:)\\s*[0-9]+"
      d8 <- strrep("\u2014", 8L)
      out <- list()
      # 1. Joined, each line asserted exactly.
      out$a <- vj_item("1a", c("Run #CPUT: Total CPU Time in", "   Seconds, 0.123 | Stop"), P5,
                       c("Run #CPUT: Total CPU Time in Seconds,<volatile>", "    | Stop"), 1)
      out$b <- vj_item("1b", c("Run #CPUT: Total CPU Time", "  in Seconds, 0.123 caf\u00e9"), P5,
                       c("Run #CPUT: Total CPU Time in Seconds,<volatile>", "   caf\u00e9"), 1)
      out$c <- vj_item("1c", c("\u2014\u2014 #CPUT: Total CPU Time in", "Seconds, 0.123 | Stop"), P5,
                       c("\u2014\u2014 #CPUT: Total CPU Time in Seconds,<volatile>", " | Stop"), 1)
      d_in <- c(paste0(d8, " Run #CPUT: Total CPU Time in"), "Seconds, 0.123 | Stop")
      out$d <- vj_utf8(vj_item("1d", d_in, P5, c(paste0(d8, " Run #CPUT: Total CPU Time in Seconds,<volatile>"),
                                                 " | Stop"), 1))
      # 1k: the 1d fixture unmarked, as system2 gives PDF text, without the
      # locale switch: the same bytes as 1d (compared as bytes, plan C3-2).
      k_in <- vapply(d_in, function(x) rawToChar(charToRaw(x)), "", USE.NAMES = FALSE)
      rk <- vj_run(k_in, P5)
      stopifnot(identical(lapply(rk$lines, charToRaw), lapply(out$d$r$lines, charToRaw)),
                identical(unname(rk$counts), 1L))
      out$k <- list(r = rk, pats = P5)
      out$e <- vj_item("1e", c("a Stamp:", "12 b Stamp:", "34 c"), ST,
                       c("a Stamp:<volatile>", " b Stamp:<volatile>", " c"), 2)
      out$f <- vj_item("1f", c("Run | Tue Oct 13", "10:11:12 UTC 2026 | #CPUT: Total CPU Time in", "Seconds, 0.123 | Stop"),
                       c(P1, P5), c("Run | <volatile>", " | #CPUT: Total CPU Time in Seconds,<volatile>", " | Stop"), c(1, 1))
      out$g <- vj_item("1g", c("a Stamp:   ", "   12 rest"), ST, c("a Stamp:<volatile>", "    rest"), 1)
      out$h <- vj_item("1h", c("ab X", "12 rest"), "(X) [0-9]+", c("ab X<volatile>", " rest"), 1)
      out$i <- vj_item("1i", c("a Stamp:", "7 rest"), "(Stamp:) [0-9]", c("a Stamp:<volatile>", " rest"), 1)
      out$j <- vj_item("1j", c("page one", "\fa Stamp:", "12 rest"), SF,
                       c("page one", "\fa Stamp:<volatile>", " rest"), 1)
      # 2. Not joined: the lines as rv_normalize left them, count 0 (2k excepted).
      same <- function(id, lines, pats, want = lines) vj_item(id, lines, pats, want, rep(0L, length(pats)))
      same("2a", c("x Stamp:", "\f12 rest"), SF)
      same("2b", c("a Stamp:", "", "12 rest"), ST)
      same("2c", c("Run | Tue Oct", "13 10:11:12", "UTC 2026 | x"), P1)
      same("2d", c("Run | Tue Oct 13 10:1", "1:12 UTC 2026 | x"), P1)
      same("2e", c("Generated 2026-10-01 10:11 UTC", "from executed run records"), P6,
           c("Generated <volatile>", "from executed run records"))
      same("2f", c("a Stamp:", "12 rest"), "(?<=Stamp:) [0-9]+")
      same("2g", c("a Stamp:", "b rest"), "(Stamp:) ")
      out$h2 <- same("2h", c("x Label: abc", "def | rest"), "(Label:).*", c("x Label:<volatile>", "def | rest"))
      out$i2 <- same("2i", c("x T: aa", "U: bb", "12 rest"), c("(T:).*", "U: [a-z]+ [0-9]+"),
                     c("x T:<volatile>", "U: bb", "12 rest"))
      out$i3 <- same("2i one pattern", c("x T: aa", "U: bb", "12 rest"), "(T:).*|U: [a-z]+ [0-9]+",
                     c("x T:<volatile>", "U: bb", "12 rest"))
      out$j2 <- same("2j", c("x T: aa", "U: bb | V: cc", "12 rest"), c("(T:)[^|]*", "(V:) [a-z]+ [0-9]+"),
                     c("x T:<volatile>", "U: bb | V: cc", "12 rest"))
      vj_item("2k", c("a Stamp:", "\fP", "12 rest"), c("\\s*P [0-9]+", "(Stamp:) [^ ]+"),
              c("a Stamp:", "<volatile>", " rest"), c(1, 0))
      # 3. Ceiling (5) characterised: an open tail up to its delimiter; the
      # order of overlapping patterns.
      out$a3 <- vj_item("3a", c("x Label:", "abc def | rest"), "(Label:) [^|]*", c("x Label:<volatile>", "| rest"), 1)
      out$b3 <- vj_item("3b", c("x A", "B C y"), c("A B", "B C"), c("x A", "<volatile> y"), c(0, 0))
      # 4. A second pass joins nothing and changes nothing (2k excluded: its
      # first pass deletes a form feed); no pattern, no change and no count.
      for (id in names(out)) {
        r2 <- rv("rv_join_volatile")(out[[id]]$r$lines, vj_sp(out[[id]]$pats))
        if (any(r2$counts != 0L) || !identical(r2$lines, out[[id]]$r$lines)) stop("second pass changed item ", id)
      }
      r0 <- rv("rv_join_volatile")(c("a Stamp:", "12 rest"), vj_sp(character(0)))
      stopifnot(identical(r0$lines, c("a Stamp:", "12 rest")), length(r0$counts) == 0L)
      # 5. A stamp split across a page break of a fixture PDF is not joined.
      t5 <- vj_cmp("v33p", list("Run | Tue Oct 13", "10:11:12 UTC 2026 | x"),
                   list("Run | Tue Oct 14", "11:12:13 UTC 2026 | x"), pdfs = "mar.pdf")$table
      stopifnot(isFALSE(t5$match), !grepl("joined across", t5$detail, fixed = TRUE))
    },
    "review (V): V34 honesty and scope: asymmetric wraps counted per side, PDF text only, file: entries first, text_rule routing kept (AC2 S3)" = function() {
      P1 <- vj_p("(?:Mon"); P4 <- vj_p("(Elapsed"); P5 <- vj_p("(#CPUT")
      an <- list(c("Run | Tue Oct 13", "10:11:12 UTC 2026 | $PROBLEM fixture"))
      fr <- list(c("Run | Tue Oct 14 11:12:13 UTC 2026 |", "$PROBLEM fixture"))
      suf <- function(n, m) paste0("; joined across one line break, re:", P1, ": analysis ", n, ", fresh ", m)
      # 1. End to end first: the analysis side wrapped, the fresh side not; the
      # separator lands on different lines, so both rows mismatch, and the
      # detail carries the one-sided count (E7).
      r1 <- vj_cmp("v34a", an, fr)
      stopifnot(identical(r1$table$match, c(FALSE, FALSE)))
      if (!all(endsWith(r1$table$detail, suf(1, 0))))
        stop("an asymmetric wrap does not show its per-side counts: ", paste(r1$table$detail, collapse = " / "))
      # 2. The mirror (E2, E7).
      t2 <- vj_cmp("v34b", fr, an)$table
      stopifnot(identical(t2$match, c(FALSE, FALSE)), all(endsWith(t2$detail, suf(0, 1))))
      # 3. A re-flow to equal text matches; the asymmetry is in the counts only.
      a3 <- vj_check("3 analysis", vj_run(c("Run | Tue Oct 13", "10:11:12 UTC 2026"), P1), c("Run | <volatile>", ""), 1)
      b3 <- vj_check("3 fresh", vj_run(c("Run | Tue Oct 14 11:12:13 UTC 2026", ""), P1), c("Run | <volatile>", ""), 0)
      stopifnot(isTRUE(rv("rv_text_compare")(a3$lines, b3$lines, "pdf")$outcome))
      # 4. A text-class file with the same wrapped lines is not joined (E5).
      t4 <- vj_cmp("v34t", NULL, NULL, pdfs = character(0), text = list("run.lst" = list(
        an = an[[1]], fr = c("Run | Tue Oct 14", "11:12:13 UTC 2026 | $PROBLEM fixture"))))$table
      stopifnot(identical(t4$class, "text, line by line after normalization"), isFALSE(t4$match),
                identical(t4$detail, "2 and 2 lines; first differing line 1"))
      # 5. A PDF named by a file: entry is listed, not judged, not joined (E6).
      t5 <- vj_cmp("v34f", an, fr, pdfs = "mar.pdf", volatile = c("file:pilot/f/report/mar.pdf" = "fixture"))$table
      stopifnot(identical(t5$class, "file: volatile entry (listed, not judged)"), is.na(t5$match),
                grepl("^analysis [0-9a-f]{64}; fresh [0-9a-f]{64}$", t5$detail))
      # 6. A mismatching rf11 PDF with joins keeps its text_rule routing (E8).
      o <- r1$outcomes[grepl("report/mar.pdf (pdf, R-F11 text rule; ", r1$outcomes$finding, fixed = TRUE), ]
      stopifnot(nrow(o) == 1L, identical(o$rule, "text_rule"), identical(o$owner, "reporting (report-writer)"),
                grepl("joined across one line break", o$finding, fixed = TRUE))
      # 7. The declared patterns on the three measured span shapes: one join each.
      pats <- vj_pats()
      r7 <- vj_run(c("Run | Tue Oct 13", "10:11:12 UTC 2026 | x", "\u2013\u2013 Elapsed",
                     "estimation time in seconds: 1.23 | y", "#CPUT: Total CPU Time in", "Seconds, 0.45 | z"), pats)
      stopifnot(identical(unname(r7$counts), as.integer(pats %in% c(P1, P4, P5))), length(r7$lines) == 6L)
    }))

  # ---- Part V, AC4 ---------
  # The validation audit judges only the units the pipeline used (V35 to
  # V37). Each fixture root under FX copies skill directories from the
  # repository and sets every unit status and reviewed_at in the copy only,
  # so the cases do not depend on the live statuses. Records are written as
  # v_record writes them, with fixture lines before the decision box.
  # va_status sets one unit's status, or every unit's when `unit` is NULL.
  va_status <- function(r, skill, status, unit = NULL) {
    f <- file.path(r, "skills", skill, "MANIFEST.yaml")
    l <- readLines(f)
    if (is.null(unit)) {
      l <- sub("^    status: .*$", paste0("    status: ", status), l)
    } else {
      i <- match(paste0("  ", unit, ":"), l)
      l[i + which(startsWith(l[-seq_len(i)], "    status:"))[1]] <- paste0("    status: ", status)
    }
    writeLines(l, f)
  }
  va_review <- function(r, skill) {
    f <- file.path(r, "skills", skill, "MANIFEST.yaml")
    l <- sub("^reviewed_by: .*$", 'reviewed_by: "Alex Rowan"', readLines(f))
    writeLines(sub("^reviewed_at: .*$", 'reviewed_at: "2026-09-28"', l), f)
  }
  va_root <- function(tag, extra = character(0)) {
    r <- file.path(FX, paste0("ac4-", tag))
    dir.create(file.path(r, "skills"), recursive = TRUE)
    for (s in c("reference-models", "data-assembly", extra)) {
      stopifnot(file.copy(file.path(REPO, "skills", s), file.path(r, "skills"), recursive = TRUE))
      va_review(r, s)
      if (s != "reference-models") va_status(r, s, "approved")
    }
    va_status(r, "reference-models", "approved", "onecmt_iv")
    for (u in c("onecmt_oral", "twocmt_iv", "twocmt_oral")) va_status(r, "reference-models", "draft", u)
    normalizePath(r)
  }
  VA_TASKS <- c("data_assembly", "base_model", "covariate")
  VA_RECS <- c(data_assembly = "gates/FXA/data-assembly.md", base_model = "gates/FXA/base-model.md",
               covariate = "gates/FXA/covariate.md")
  VA_DATES <- c(data_assembly = "2026-10-01", base_model = "2026-10-02", covariate = "2026-10-03")
  VA_L <- "Disclosure: skills/reference-models, unit twocmt_oral, status draft"
  VA_OK <- c("$PROBLEM a", ";  Source: skills/reference-models, unit onecmt_iv")
  VA_DR <- c("$PROBLEM b", ";  Source: skills/reference-models, unit twocmt_oral")
  va_s <- function(line) c("$PROBLEM fixture", line)
  # One record: `extra` before the decision box, `after` directly after it.
  va_rec <- function(r, task, extra = character(0), after = character(0)) {
    v <- v_record("FXA", sub("_", "-", task), date = VA_DATES[[task]])
    vw(r, VA_RECS[[task]], c(v[1:5], extra, v[6:8], after, v[9:length(v)]))
  }
  # The fixture manifest: a prose command naming skills/reference-models (none
  # when `note` is NULL), one file target per input with its gate symbol (a
  # gate_data_assembly block is task base_model, gate_base_model covariate),
  # and two gate targets.
  va_man <- function(files, gate_of, note) {
    n <- if (is.null(note)) character(0) else "bm_note"
    data.frame(name = c(n, sprintf("bm_ctl_file%d", seq_along(files)), "gate_data_assembly", "gate_base_model"),
               command = c(if (length(n)) sprintf("{\n    gate_data_assembly\n    list(h = \"%s\")\n}", note),
                           sprintf("{\n    %s\n    identity(\"%s\")\n}", gate_of, files),
                           "{\n    x\n}", "{\n    y\n}"),
               format = c(if (length(n)) "rds", rep("file", length(files)), "rds", "rds"),
               stringsAsFactors = FALSE)
  }
  # Runs the audit on a fresh fixture root with the working directory at the
  # root, as V16 does. `files` (path = lines) are written under the root;
  # `inputs` (default their names; "<ROOT>" stands for the root) are the file
  # targets; `bm` (lines, or a function of the root) and `cov` go into the
  # base_model and covariate records; `prep(root)` runs before the files.
  va <- function(tag, files = list(), inputs = as.character(names(files)),
                 gate_of = rep("gate_data_assembly", length(inputs)), bm = character(0),
                 bm_after = character(0), cov = character(0),
                 note = "in the house form of skills/reference-models", extra = character(0),
                 prep = NULL, recs = VA_RECS) {
    r <- va_root(tag, extra)
    if (!is.null(prep)) prep(r)
    for (p in names(files)) vw(r, p, files[[p]])
    if (is.function(bm)) bm <- bm(r)
    va_rec(r, "data_assembly")
    va_rec(r, "base_model", bm, bm_after)
    va_rec(r, "covariate", cov)
    m <- va_man(sub("<ROOT>", r, inputs, fixed = TRUE), gate_of, note)
    old <- setwd(r); on.exit(setwd(old))
    rv("rv_validation_audit")(m, VA_TASKS, recs, list(), "data-assembly")
  }
  va_b2 <- function(a) a$outcomes[a$outcomes$checklist_item %in% "domain 4 bullet 2", , drop = FALSE]
  va_rules <- function(a, skill = "reference-models") {
    o <- va_b2(a)
    o$rule[startsWith(o$finding, paste0("skill ", skill, ":"))]
  }
  va_row <- function(a, skill = "reference-models") a$table[a$table$skill == skill, , drop = FALSE]
  # The audit alone through rv_checklist and rv_findings: the log rows and the
  # checklist outcome of domain 4 bullet 2.
  va_log <- function(a) {
    f <- rv("rv_findings")(a$outcomes, rv("rv_checklist")(list(review_validation = a)))
    list(log = f$log[f$log$checklist_item == "domain 4 bullet 2", , drop = FALSE],
         row = f$checklist$outcome[startsWith(f$checklist$item, "domain 4 bullet 2:")])
  }
  # Expects exactly the bullet 2 rules `rule` for `skill` from va(tag, ...).
  va_want <- function(rule, tag, ..., skill = "reference-models") {
    got <- va_rules(va(tag, ...), skill)
    if (!identical(got, rule))
      stop(tag, ": expected ", paste(rule, collapse = ", "), ", got ",
           if (length(got)) paste(got, collapse = ", ") else "(none)")
    invisible(TRUE)
  }

  cases <- c(cases, list(
    "review (V): V35 validation audit: only the units the pipeline used are judged; a template unit counts when a literal input cites it (AC4 S1)" = function() {
      # 1. A prose mention of the skill, a stream citing the approved unit,
      # three draft units: no bullet 2 finding and the row met.
      a <- va("v35-1", list("model/1.mod" = VA_OK))
      lg <- va_log(a)
      stopifnot(nrow(va_b2(a)) == 0L, identical(va_row(a)$units, "onecmt_iv"),
                identical(va_row(a)$statuses, "approved"), identical(va_row(a)$first_use, "2026-10-02"),
                nrow(lg$log) == 0L, identical(lg$row, "met"))
      # 2. The MANIFEST form.
      a <- va("v35-2", list("model/1.mod" = va_s("; see skills/reference-models/MANIFEST.yaml, unit `onecmt_iv`")))
      stopifnot(nrow(va_b2(a)) == 0L, identical(va_row(a)$units, "onecmt_iv"))
      # 3. Two citations on one line: one unit, one table entry.
      a <- va("v35-3", list("model/1.mod" = va_s(paste(";  skills/reference-models, unit onecmt_iv and",
                                                       "skills/reference-models/MANIFEST.yaml, unit `onecmt_iv`"))))
      stopifnot(nrow(va_b2(a)) == 0L, identical(va_row(a)$units, "onecmt_iv"))
      # 3b. Two citations of different units on one line: both are read (the
      # draft unit twocmt_oral is judged, not only the first citation).
      a <- va("v35-3b", list("model/1.mod" = va_s(paste(";  skills/reference-models, unit onecmt_iv and",
                                                        "skills/reference-models, unit twocmt_oral"))))
      stopifnot(identical(va_row(a)$units, "onecmt_iv, twocmt_oral"), identical(va_rules(a), "validation"),
                grepl("not approved: twocmt_oral (draft)", va_b2(a)$finding, fixed = TRUE))
      # 4. The same stream named by two file targets of one task: one unit,
      # and for a cited draft unit one row (no duplicate task).
      a <- va("v35-4", list("model/1.mod" = VA_OK), inputs = c("model/1.mod", "model/1.mod"))
      stopifnot(nrow(va_b2(a)) == 0L, identical(va_row(a)$units, "onecmt_iv"))
      a <- va("v35-4b", list("model/1.mod" = VA_DR), inputs = c("model/1.mod", "model/1.mod"))
      stopifnot(identical(va_rules(a), "validation"), identical(va_row(a)$units, "twocmt_oral"))
      # 5. Widened citation forms, each alone in the stream.
      f5 <- c(no_comma = ";  Source: skills/reference-models unit onecmt_iv",
              capital = ";  Source: skills/reference-models, Unit onecmt_iv",
              path = ";  Source: skills/reference-models/reference/onecmt_iv.ctl")
      for (k in names(f5)) {
        a <- va(paste0("v35-5-", k), list("model/1.mod" = va_s(f5[[k]])))
        if (nrow(va_b2(a)) != 0L || !identical(va_row(a)$units, "onecmt_iv"))
          stop("V35 item 5 form ", k, ": units '", va_row(a)$units, "', bullet 2 rows ", nrow(va_b2(a)))
      }
      # 6. Partial citation: the exact form for onecmt_iv, no comma for twocmt_oral.
      a <- va("v35-6", list("model/1.mod" = VA_OK,
                            "model/2.mod" = va_s(";  Source: skills/reference-models unit twocmt_oral")))
      lg <- va_log(a)
      stopifnot(identical(va_row(a)$units, "onecmt_iv, twocmt_oral"), identical(va_rules(a), "validation"),
                grepl("not approved: twocmt_oral (draft)", va_b2(a)$finding, fixed = TRUE),
                identical(lg$log$severity, "critical"), identical(lg$row, "not met"))
    },
    "review (V): V36 validation audit: a non-approved used unit is judged flagged or silent from the signed record of the task that used it (AC4 S2)" = function() {
      d <- list("model/1.mod" = VA_DR)
      # 1. The disclosure line in the base_model record: disclosed, minor, met.
      a <- va("v36-1", d, bm = VA_L)
      lg <- va_log(a)
      stopifnot(identical(va_rules(a), "validation_disclosed"),
                grepl("unit(s) not approved: twocmt_oral (draft), used by task base_model and flagged",
                      va_b2(a)$finding, fixed = TRUE),
                identical(lg$log$severity, "minor (recorded limitation)"),
                identical(lg$log$owner, "owner of skills/reference-models and the SME"), identical(lg$row, "met"))
      # 2. No line anywhere: silent, critical, not met.
      a <- va("v36-2", d)
      lg <- va_log(a)
      stopifnot(identical(va_rules(a), "validation"),
                endsWith(va_b2(a)$finding, "with no flag in its signed record"),
                identical(lg$log$severity, "critical"), identical(lg$row, "not met"))
      # 3. The line only in a later task's record. 4. The line naming another unit.
      va_want("validation", "v36-3", d, cov = VA_L)
      va_want("validation", "v36-4", d, bm = sub("twocmt_oral", "twocmt_iv", VA_L, fixed = TRUE))
      # 5. Accepted variants.
      ok5 <- list(marker = "- Disclosure: skills/reference-models, unit `twocmt_oral`, status `draft`.",
                  rejected = "Disclosure: skills/reference-models, unit twocmt_oral, status rejected",
                  spaces = "   Disclosure: skills/reference-models, unit twocmt_oral, status unvalidated",
                  star_tab = paste0("*\t", VA_L),
                  unbalanced = "Disclosure: skills/reference-models, unit `twocmt_oral, status draft",
                  fence = c("```", VA_L, "```"),
                  trailing_blanks = paste0(VA_L, "   "),
                  trailing_tab = paste0(VA_L, intToUtf8(9L)))
      for (k in names(ok5)) va_want("validation_disclosed", paste0("v36-5-", k), d, bm = ok5[[k]])
      # 6. Near-miss lines.
      no6 <- list(approved = sub("status draft", "status approved", VA_L, fixed = TRUE),
                  text_after = paste0(VA_L, " (not used)"),
                  text_before = paste0("We did not write ", VA_L),
                  wrapped = c("Disclosure: skills/reference-models, unit twocmt_oral,", "status draft"),
                  lower = sub("Disclosure:", "disclosure:", VA_L, fixed = TRUE),
                  two_spaces = sub("Disclosure: ", "Disclosure:  ", VA_L, fixed = TRUE))
      for (k in names(no6)) va_want("validation", paste0("v36-6-", k), d, bm = no6[[k]])
      # 7. The unit is captured whole. 8. Prose is not read.
      va_want("validation", "v36-7", d, bm = sub("twocmt_oral", "twocmt_oral2", VA_L, fixed = TRUE))
      va_want("validation", "v36-8", d, bm = c("The stream adapts the skills/reference-models unit",
                                               "`twocmt_oral`, a draft template (disclosed)."))
      # 9. Two tasks use the unit; only the base_model record holds the line.
      a <- va("v36-9", list("model/1.mod" = VA_DR, "model/2.mod" = VA_DR),
              gate_of = c("gate_data_assembly", "gate_base_model"), bm = VA_L)
      o <- va_b2(a)
      stopifnot(nrow(o) == 2L, identical(o$rule[grepl("used by task base_model", o$finding)], "validation_disclosed"),
                identical(o$rule[grepl("used by task covariate", o$finding)], "validation"))
      # 10. `records` without the task. 11. The task's record file is absent.
      va_want("validation", "v36-10", d, bm = VA_L, recs = VA_RECS[c("data_assembly", "covariate")])
      r11 <- VA_RECS
      r11[["base_model"]] <- "gates/FXA/absent.md"
      va_want("validation", "v36-11", d, bm = VA_L, recs = r11)
      # 12. The C1-1 fixtures in the base_model record.
      c12 <- list(
        c1 = list(bm = "The stream adapts skills/reference-models unit twocmt_oral; no draft template was used."),
        c2 = list(bm = function(r) readLines(file.path(r, "skills/reference-models/reference/twocmt_oral.ctl"))[1:14]),
        c3 = list(bm = "Of skills/reference-models, twocmt_iv is draft and was not used; the stream adapts twocmt_oral."),
        c4 = list(bm_after = "Comments: the stream adapts skills/reference-models unit twocmt_oral as planned."),
        c5 = list(bm = paste("skills/reference-models unit twocmt_oral is approved;",
                             "it is not draft, not rejected and not unvalidated.")))
      for (k in names(c12)) do.call(va_want, c(list("validation", paste0("v36-12-", k), d), c12[[k]]))
      # 13. C1-1 e1: the unit name inside the skill name.
      nm <- function(r) va_status(r, "nonmem-estimation", "draft", "estimation")
      nmd <- list("model/1.mod" = va_s(";  Source: skills/nonmem-estimation, unit estimation"))
      va_want("validation", "v36-13", nmd, bm = "Runs used skills/nonmem-estimation as in the draft MAP.",
              extra = "nonmem-estimation", prep = nm, skill = "nonmem-estimation")
      va_want("validation_disclosed", "v36-13b", nmd,
              bm = "Disclosure: skills/nonmem-estimation, unit estimation, status draft",
              extra = "nonmem-estimation", prep = nm, skill = "nonmem-estimation")
      # 14. C1-1 e2: a unit name shared across skills.
      dg <- function(r) va_status(r, "diagnostics", "draft", "vpc")
      va_want("validation", "v36-14", list("model/1.mod" = "$PROBLEM fixture"),
              bm = "skills/diagnostics produced the plots; psn-simeval unit vpc is draft and was not run.",
              note = "plots with skills/diagnostics/reference/vpc.R", extra = c("diagnostics", "psn-simeval"),
              prep = dg, skill = "diagnostics")
      va_want("validation", "v36-14b", list("model/1.mod" = "$PROBLEM fixture"),
              bm = "Disclosure: skills/psn-simeval, unit vpc, status draft",
              note = "plots with skills/diagnostics/reference/vpc.R", extra = c("diagnostics", "psn-simeval"),
              prep = dg, skill = "diagnostics")
      # 15. rv_findings direct: the new rule with no owner; the other 20 rules
      # keep the V14 severities.
      sev <- c(errored_set = "critical", attestation = "critical", comparison_mismatch = "critical",
               file_mismatch = "critical", text_rule = "critical", gate_record = "critical",
               promotion = "critical", validation = "critical", census_threshold = "major",
               concordance = "major", inventory_undeclared = "major", section6_differs = "major",
               checkout = "major", history_after_sign = "major (reading item)", registry = "major",
               not_evaluated = "major", inventory_expected = "minor (expected)",
               section6_pending = "minor (expected)", crossref_pending = "minor (expected)",
               outside_attestation = "minor (recorded limitation)",
               validation_disclosed = "minor (recorded limitation)")
      cl <- rv("rv_checklist")(stats::setNames(rep(list(list(outcomes = NULL)), 6),
        c("review_gate_integrity", "review_reproduction", "review_traceability", "review_map_concordance",
          "review_validation", "review_crossdoc")))
      f <- rv("rv_findings")(rv("rv_outcome")(names(sev), "domain 3 bullet 1", paste("fixture", names(sev)), "e"), cl)
      stopifnot(identical(f$log$severity, unname(sev)), nrow(f$log) == 21L,
                identical(f$log$owner[f$log$finding == "fixture validation_disclosed"], "the skill's owner and the SME"))
      # 16. HTML comment lines in the base_model record.
      h16 <- list(a = c("<!-- hidden note", VA_L, "-->"), b = c("<!-- note", "still note -->", VA_L),
                  c = c("<!-- generated -->", VA_L), d = c("<!-- a --> <!-- b", VA_L, "-->"),
                  e = c("<!-- never closed", VA_L), f = paste0("<!-- ", VA_L, " -->"))
      w16 <- c(a = "validation", b = "validation_disclosed", c = "validation_disclosed", d = "validation",
               e = "validation", f = "validation")
      for (k in names(h16)) va_want(w16[[k]], paste0("v36-16", k), d, bm = h16[[k]])
    },
    "review (V): V37 validation audit: unidentified units are not evaluated, never assumed used; scope of the citation read (AC4 S3)" = function() {
      # 1. A prose mention only, no stream: not evaluated, major, not met.
      a <- va("v37-1")
      lg <- va_log(a)
      stopifnot(identical(va_rules(a), "not_evaluated"),
                grepl("(onecmt_oral, twocmt_iv, twocmt_oral)", va_b2(a)$finding, fixed = TRUE),
                identical(lg$log$severity, "major"), identical(lg$row, "not met"),
                identical(va_row(a)$units, "none identified"),
                identical(va_row(a)$statuses, "not approved: onecmt_oral, twocmt_iv, twocmt_oral"),
                !any(va_b2(a)$rule == "validation"))
      # 2. data-assembly, a registry skill with no path named, all approved.
      stopifnot(identical(va_row(a, "data-assembly")$units, "none identified"),
                identical(va_row(a, "data-assembly")$statuses, "all units approved"),
                !any(startsWith(a$outcomes$finding, "skill data-assembly:")))
      # 3. A cited unit the manifest lacks.
      a <- va("v37-3", list("model/1.mod" = va_s(";  Source: skills/reference-models, unit threecmt_iv")))
      lg <- va_log(a)
      stopifnot(identical(va_rules(a), "validation"),
                grepl("threecmt_iv (not in manifest)", va_b2(a)$finding, fixed = TRUE),
                identical(va_row(a)$statuses, "not in manifest"), identical(lg$log$severity, "critical"))
      # 4. A skill cited only in a stream (covariate's) joins with its first use.
      a <- va("v37-4", list("model/1.mod" = VA_OK), gate_of = "gate_base_model", note = NULL)
      stopifnot(identical(va_row(a)$units, "onecmt_iv"), identical(va_row(a)$first_use, "2026-10-03"))
      # 5. Binary, directory, missing and unreadable inputs beside a citation.
      lk <- file.path(FX, "ac4-v37-5", "model", "locked.txt")
      on.exit(if (file.exists(lk)) Sys.chmod(lk, "644"), add = TRUE)
      p5 <- function(r) {
        dir.create(file.path(r, "model", "adir"), recursive = TRUE)
        writeBin(as.raw(c(0x00, 0xff, 0x0a, 0xc3, 0x28, 0x0a)), file.path(r, "model", "bin.dat"))
        writeLines(";  skills/reference-models, unit twocmt_oral", file.path(r, "model", "locked.txt"))
        Sys.chmod(file.path(r, "model", "locked.txt"), "000")
        if (file.access(file.path(r, "model", "locked.txt"), 4) != -1L)
          stop("V37 item 5: the mode 000 file is readable by this user, so the case cannot exercise it")
      }
      a <- va("v37-5", list("model/1.mod" = VA_OK),
              inputs = c("model/1.mod", "model/bin.dat", "model/adir", "model/none", "model/locked.txt"), prep = p5)
      stopifnot(identical(va_row(a)$units, "onecmt_iv"), nrow(va_b2(a)) == 0L)
      # 5b. A citation on a line that also holds invalid UTF-8 bytes (0xc3
      # 0x28) is still read: the citation match works on bytes. Without that
      # the unit is lost only under a UTF-8 locale.
      p5b <- function(r) {
        dir.create(file.path(r, "model"), recursive = TRUE)
        writeBin(c(charToRaw(";  skills/reference-models, unit onecmt_iv "), as.raw(c(0xc3, 0x28, 0x0a))),
                 file.path(r, "model", "cite.dat"))
      }
      a <- va("v37-5b", inputs = "model/cite.dat", prep = p5b)
      if (!identical(va_row(a)$units, "onecmt_iv") || nrow(va_b2(a)) != 0L)
        stop("V37 item 5b: units '", va_row(a)$units, "', bullet 2 rows ", nrow(va_b2(a)))
      # 6. A citation in a file target of a task with no record.
      a <- va("v37-6", list("model/1.mod" = VA_OK, "model/2.mod" = VA_DR),
              gate_of = c("gate_data_assembly", "gate_base_model"), recs = VA_RECS[c("data_assembly", "base_model")])
      stopifnot(identical(va_rules(a), "validation"), grepl("used by task covariate", va_b2(a)$finding, fixed = TRUE))
      # 7. Review timing and draft status on the copied public method.
      man16 <- data.frame(name = c("da_x", "gate_data_assembly"),
                          command = c(paste("{\n    environment_attestation\n    method_manifests\n",
                                            "   load_reference(\"skills/data-assembly\", \"skills/data-assembly/reference/weight_constants.R\")\n}"),
                                      "{\n    environment_attestation\n    gate_data_assembly_file\n}"),
                          stringsAsFactors = FALSE)
      audit16 <- function(tag, date, draft = FALSE) {
        r <- file.path(FX, paste0("ac4-v16-", tag))
        dir.create(file.path(r, "skills"), recursive = TRUE)
        stopifnot(file.copy(file.path(REPO, "skills", "data-assembly"), file.path(r, "skills"), recursive = TRUE))
        va_review(r, "data-assembly")
        va_status(r, "data-assembly", if (draft) "draft" else "approved")
        r <- normalizePath(r)
        rec <- vw(r, "gates/FXA/data-assembly.md", v_record("FXA", "data-assembly", date = date))
        old <- setwd(r); on.exit(setwd(old))
        rv("rv_validation_audit")(man16, "data_assembly", c(data_assembly = rec), list(), "data-assembly")
      }
      ok <- audit16("ok", "2026-10-01")
      late <- audit16("late", "2026-09-01")
      dr <- audit16("draft", "2026-10-01", draft = TRUE)
      stopifnot(!any(ok$outcomes$rule == "validation"),
                isTRUE(ok$table$reviewed_before_first_use[ok$table$skill == "data-assembly"]),
                any(late$outcomes$rule == "validation" & grepl("after its first use", late$outcomes$finding)),
                any(dr$outcomes$rule == "validation" & grepl("not approved", dr$outcomes$finding) &
                      grepl("with no flag in its signed record", dr$outcomes$finding, fixed = TRUE)))
      # 8. The header comment of rv_validation_audit.
      src <- readLines(RV_FILE)
      i <- grep("^rv_validation_audit <- function", src)
      j <- i - 1L
      while (j > 0L && startsWith(src[j], "#")) j <- j - 1L
      hdr <- paste(sub("^# ?", "", src[(j + 1L):(i - 1L)]), collapse = " ")
      for (w in c("cited", "not evaluated", "flagged", "Disclosure: skills/<skill>, unit <unit>, status", "ponytail:"))
        if (!grepl(w, hdr, fixed = TRUE)) stop("V37 item 8: the rv_validation_audit header lacks '", w, "'")
      # 9. Only this analysis's stage record paths in `records` are skipped;
      # every other input is read. `records` root-relative, as the pipeline
      # passes it.
      two <- c("gate_data_assembly", "gate_base_model")
      chk <- function(part, a, units, rules) {
        o <- va_b2(a)
        if (!identical(va_row(a)$units, units) || !identical(sort(o$rule), sort(rules)))
          stop("V37 item 9 (", part, "): units '", paste(va_row(a)$units, collapse = "; "), "', rules '",
               paste(o$rule, collapse = ", "), "'")
        o
      }
      o <- chk("a", va("v37-9a", list("model/1.mod" = VA_DR), inputs = c("model/1.mod", "gates/FXA/base-model.md"),
                       gate_of = two, bm = VA_L), "twocmt_oral", "validation_disclosed")
      stopifnot(grepl("used by task base_model", o$finding, fixed = TRUE))
      chk("b", va("v37-9b", list("model/1.mod" = VA_DR), inputs = c("model/1.mod", "./gates/FXA/base-model.md"),
                  gate_of = two, bm = VA_L), "twocmt_oral", "validation_disclosed")
      # (c) to (l), (n), (o): model/1.mod cites onecmt_iv; the second input
      # (attributed by its gate symbol) cites twocmt_oral; no record holds the line.
      nine <- list(
        c = list("skills/reference-models/work/2.mod", "gate_data_assembly", VA_DR),
        d = list("skills/reference-models/reference/fx_adapted.ctl", "gate_data_assembly", VA_DR),
        e = list("gates/FXA/work/2.mod", "gate_data_assembly", VA_DR),
        f = list("gates/../notes2.md", "gate_base_model", VA_DR),
        g = list("gates/./notes2.md", "gate_base_model", VA_DR),
        h = list("gates/FXA/notes2.md", "gate_base_model", VA_DR),
        i = list("gates/OTHER/notes2.md", "gate_base_model", VA_DR),
        j = list("gates/OTHER/base-model.md", "gate_data_assembly", c("# Fixture record", VA_L)),
        k = list("gates/FXA/work2.mod", "gate_base_model", VA_DR),
        l = list("gates/FXA/history/base-model.abc.md", "gate_data_assembly", c("# Fixture record", VA_L)),
        n = list("<ROOT>/notes2.md", "gate_base_model", VA_DR),
        o = list(".//notes2.md", "gate_base_model", VA_DR))
      for (k in names(nine)) {
        p <- nine[[k]]
        fl <- list("model/1.mod" = VA_OK)
        fl[[if (startsWith(p[[1]], "<ROOT>/")) "notes2.md" else p[[1]]]] <- p[[3]]
        chk(k, va(paste0("v37-9", k), fl, inputs = c("model/1.mod", p[[1]]), gate_of = c("gate_data_assembly", p[[2]])),
            "onecmt_iv, twocmt_oral", "validation")
      }
      # (m) The base_model value of `records` written with a "." segment, and
      # the covariate file target naming that string: read, the safe direction.
      rm9 <- VA_RECS
      rm9[["base_model"]] <- "gates/./FXA/base-model.md"
      o <- chk("m", va("v37-9m", list("model/1.mod" = VA_DR), inputs = c("model/1.mod", "gates/./FXA/base-model.md"),
                       gate_of = two, bm = VA_L, recs = rm9), "twocmt_oral", c("validation_disclosed", "validation"))
      stopifnot(identical(o$rule[grepl("used by task base_model", o$finding)], "validation_disclosed"),
                identical(o$rule[grepl("used by task covariate", o$finding)], "validation"))
    }))

  # AC5 (reviewer R-2): skill names taken from commands, and the cell of a
  # skill without a method manifest. Synthetic skill directories only; the
  # fixture roots live under FX.
  va5_root <- function(tag, skills = list(), copy = character(0)) {
    r <- file.path(FX, paste0("ac5-", tag))
    dir.create(file.path(r, "skills"), recursive = TRUE)
    for (s in copy) stopifnot(file.copy(file.path(REPO, "skills", s), file.path(r, "skills"), recursive = TRUE))
    for (s in names(skills)) if (!is.null(skills[[s]])) vw(r, file.path("skills", s, "SKILL.md"), skills[[s]])
      else dir.create(file.path(r, "skills", s))
    normalizePath(r)
  }
  # One stage command (`cmd`) of task data_assembly, its gate target, and the
  # audit run with the working directory at the root, as V16 does.
  va5 <- function(r, cmd = "{\n    x\n}", registry = character(0)) {
    man <- data.frame(name = c("da_x", "gate_data_assembly"),
                      command = c(cmd, "{\n    environment_attestation\n    gate_data_assembly_file\n}"),
                      stringsAsFactors = FALSE)
    rec <- vw(r, "gates/FXA/data-assembly.md", v_record("FXA", "data-assembly", date = "2026-10-01"))
    old <- setwd(r); on.exit(setwd(old))
    rv("rv_validation_audit")(man, "data_assembly", c(data_assembly = rec), list(), registry)
  }
  VA5_OK <- c("# Skill: fx-guide", "", "Status: VALIDATED 2026-08-29 (fixture scope).")
  VA5_GO <- "Status: guidance-only skill - it gates no method reference unit and therefore"

  cases <- c(cases, list(
    "review (V): V38 validation audit: a skill name taken from a command ends before a full stop (AC5 S1)" = function() {
      note <- function(s) sprintf("{\n    list(h = \"%s\")\n}", s)
      # 1. A SKILL.md-only skill named at the end of a sentence and in the
      # registry: one row, the skill's own, with its validation line.
      a <- va5(va5_root("v38-1", list("fx-guide" = VA5_OK)), note("thresholds from skills/fx-guide."), "fx-guide")
      stopifnot(identical(a$table$skill, "fx-guide"),
                identical(va_row(a, "fx-guide")$statuses, VA5_OK[3]))
      # 2. The same sentence with no registry skill: the row is still the skill's.
      a <- va5(va5_root("v38-2", list("fx-guide" = VA5_OK)), note("thresholds from skills/fx-guide."))
      stopifnot(identical(a$table$skill, "fx-guide"), identical(va_row(a, "fx-guide")$statuses, VA5_OK[3]))
      # 3. A skill with a method manifest named by its reference path and at the
      # end of a sentence: one MANIFEST.yaml row, its unit found.
      cmd <- paste0("{\n    load_reference(\"skills/data-assembly\", ",
                    "\"skills/data-assembly/reference/weight_constants.R\")\n    list(h = \"see skills/data-assembly.\")\n}")
      a <- va5(va5_root("v38-3", copy = "data-assembly"), cmd, "data-assembly")
      stopifnot(identical(a$table$skill, "data-assembly"), identical(va_row(a, "data-assembly")$record, "MANIFEST.yaml"),
                identical(va_row(a, "data-assembly")$units, "weight_constants"))
      # 4. A file name under skills/ reads as its skill; 5. digits, "_" and "-"
      # stay in a name.
      a <- va5(va5_root("v38-4", list("fx-guide" = VA5_OK)), note("see skills/fx-guide.md for the rule"))
      stopifnot(identical(a$table$skill, "fx-guide"))
      a <- va5(va5_root("v38-5", list("fx_guide-2" = VA5_OK)), note("see skills/fx_guide-2, line 3"))
      stopifnot(identical(a$table$skill, "fx_guide-2"))
      # 6. A skill with a method manifest named only at the end of a sentence
      # (no registry skill, no reference path), one unit not approved: the
      # mention is a use, so the skill gets its MANIFEST.yaml row and its
      # units are not evaluated (the stricter direction of the fix).
      r <- va5_root("v38-6", copy = "data-assembly")
      stopifnot(file.rename(file.path(r, "skills", "data-assembly"), file.path(r, "skills", "fx-meth")))
      f <- file.path(r, "skills", "fx-meth", "MANIFEST.yaml")
      writeLines(gsub("skills/data-assembly/", "skills/fx-meth/",
                      sub("^skill: data-assembly$", "skill: fx-meth", readLines(f)), fixed = TRUE), f)
      va_review(r, "fx-meth")
      va_status(r, "fx-meth", "draft")
      a <- va5(r, note("see skills/fx-meth."))
      stopifnot(identical(a$table$skill, "fx-meth"), identical(va_row(a, "fx-meth")$record, "MANIFEST.yaml"),
                identical(a$outcomes$rule[startsWith(a$outcomes$finding, "skill fx-meth")], "not_evaluated"))
      # 7. A mention of a skill with no directory keeps its row and the
      # fallback, the signal that the mention is stale (Q3 (c) rejected).
      a <- va5(va5_root("v38-7"), note("see skills/fx-gone."))
      stopifnot(identical(a$table$skill, "fx-gone"),
                identical(va_row(a, "fx-gone")$statuses, "no validation line found"))
    },
    "review (V): V39 validation audit: a guidance-only skill shows its Status line, not a missing validation line (AC5 S2)" = function() {
      one <- function(tag, lines) {
        a <- va5(va5_root(tag, list("fx-guide" = lines)), registry = "fx-guide")
        r <- va_row(a, "fx-guide")
        if (nrow(r) != 1L || !identical(r$record, "validation record in SKILL.md"))
          stop(tag, ": expected one SKILL.md row for fx-guide")
        if (any(startsWith(a$outcomes$finding, "skill fx-guide")))
          stop(tag, ": a SKILL.md-only skill gave an outcome")
        r$statuses
      }
      # 1. The guidance-only Status line is the cell.
      stopifnot(identical(one("v39-1", c("# Skill: fx-guide", "", VA5_GO, "carries no method manifest.")), VA5_GO))
      # 2. The first matching line in file order is the cell.
      stopifnot(identical(one("v39-2", c("# Skill: fx-guide", VA5_GO, "", "Date: 2026-01-01")), VA5_GO))
      # 3. "Status: guidance-only" not at the start of a line is not read.
      stopifnot(identical(one("v39-3", c("# Skill: fx-guide", "Former Status: guidance-only (withdrawn).")),
                          "no validation line found"))
      # 4. No validation line and no Status line, and no SKILL.md: the fallback.
      stopifnot(identical(one("v39-4", c("# Skill: fx-guide", "Status: draft")), "no validation line found"),
                identical(one("v39-4b", NULL), "no validation line found"))
      # 5. A validated skill's cell is unchanged.
      stopifnot(identical(one("v39-5", VA5_OK), VA5_OK[3]))
    }))


  # AC6 (reviewer R-3): synthetic snapshots of the rv_store_values form.
  # `null` names the values read as NULL; `errs` the names whose read error
  # the attribute "read_errors" carries ("FX read error <name>").
  v6_snap <- function(null = character(0), errs = null) {
    s <- list(x_package = list(analysis = "FX", task = "x", tables = list(t1 = data.frame(a = c(1, 2))),
                               artifacts = data.frame(artifact = "a", path = "pilot/m/a.csv"), notes = list()),
              reporting_package = list(analysis = "FX", task = "reporting",
                tables = list("Promoted values and their sources" =
                                data.frame(target = "report_x", source = "x_package", selection = "t1")),
                artifacts = NULL, notes = list()),
              review_package = list(analysis = "FX", task = "review", tables = list(), artifacts = NULL,
                                    notes = list()),
              dataset = data.frame(ID = 1:2), report_x = data.frame(v = 1),
              environment_attestation = list(fingerprint = "f", probe = list(lib = "/lib")))
    for (n in null) s[n] <- list(NULL)
    attr(s, "read_errors") <- stats::setNames(sprintf("FX read error %s", errs), errs)
    s
  }
  v6_cmp <- function(a, b) rv("rv_compare_stores")(a, b, v_spec(), character(0))
  v6_row <- function(r, item) r$table[r$table$item == item, , drop = FALSE]
  V6_DS <- "dataset (analysis-ready data, all cells)"
  V6_GONE <- "absent or unreadable in one of the stores"
  v6_both <- function(nm, why = paste("FX read error", nm))
    sprintf("%s (analysis: %s; fresh: %s)", V6_GONE, why, why)

  cases <- c(cases, list(
    "review (V): V40 the store comparator: a value unreadable or absent in both stores is a mismatch quoting both reasons (AC6 S1)" = function() {
      both <- function(nm) v6_cmp(v6_snap(nm), v6_snap(nm))
      # 1. The dataset.
      r <- both("dataset"); x <- v6_row(r, V6_DS)
      stopifnot(nrow(x) == 1L, isFALSE(x$match), isFALSE(x$exact), is.na(x$max_abs_diff),
                identical(x$detail, v6_both("dataset")),
                identical(r$outcomes$rule, "comparison_mismatch"),
                identical(r$outcomes$owner, "owning stage: data_assembly"))
      # 2. A promoted target.
      r <- both("report_x"); x <- v6_row(r, "promoted target report_x")
      stopifnot(nrow(x) == 1L, isFALSE(x$match), identical(x$detail, v6_both("report_x")),
                identical(r$outcomes$rule, "comparison_mismatch"),
                identical(r$outcomes$owner, "owning stage: reporting"))
      # 3. The environment attestation: both rows, rule attestation, so the
      # reproduction record's "attestation fingerprint equal" reads FALSE.
      r <- both("environment_attestation")
      x <- r$table[startsWith(r$table$item, "environment_attestation: "), , drop = FALSE]
      stopifnot(identical(x$item, c("environment_attestation: fingerprint", "environment_attestation: probe")),
                identical(x$match, c(FALSE, FALSE)), all(x$detail == v6_both("environment_attestation")),
                identical(r$outcomes$rule, c("attestation", "attestation")),
                !isTRUE(r$table$match[r$table$item == "environment_attestation: fingerprint"]))
      # 4. A package: the existing guard now quotes the read errors.
      r <- both("x_package"); x <- v6_row(r, "x_package")
      stopifnot(nrow(x) == 1L, isFALSE(x$match), identical(x$detail, v6_both("x_package")),
                identical(r$outcomes$rule, "comparison_mismatch"))
      # 5. NULL in both snapshots without a read error (an absent target; a
      # snapshot without the attribute at all): a mismatch, reason "absent or NULL".
      a <- v6_snap("dataset"); attr(a, "read_errors") <- NULL
      x <- v6_row(v6_cmp(a, a), V6_DS)
      stopifnot(isFALSE(x$match), identical(x$detail, v6_both("dataset", "absent or NULL")))
      # 6. The attestation readable in both stores, one compared field absent
      # from both (owner ruling C1-1 (a)): that row is a mismatch, reason
      # "absent or NULL", rule attestation; the other field's row still
      # matches. The field absent on one side only keeps rv_compare_value's row.
      for (f in c("fingerprint", "probe")) {
        a <- v6_snap(); a$environment_attestation[[f]] <- NULL
        r <- v6_cmp(a, a)
        x <- r$table[startsWith(r$table$item, "environment_attestation: "), , drop = FALSE]
        k <- x$item == paste0("environment_attestation: ", f)
        stopifnot(sum(k) == 1L, identical(x$match, !k),
                  identical(x$detail[k], v6_both(f, "absent or NULL")),
                  identical(r$outcomes$rule, "attestation"), identical(r$outcomes$owner, "root (environment)"))
        x <- v6_row(v6_cmp(a, v6_snap()), paste0("environment_attestation: ", f))
        stopifnot(isFALSE(x$match), startsWith(x$detail, "mismatch: class NULL length 0 vs class "))
        # The sides swapped (C2-2): the field absent on the fresh side only.
        x <- v6_row(v6_cmp(v6_snap(), a), paste0("environment_attestation: ", f))
        stopifnot(isFALSE(x$match), startsWith(x$detail, "mismatch: class "),
                  endsWith(x$detail, " vs class NULL length 0"))
      }
      # 7. A promoted target named NA or "", with a read error of that name
      # (C1-3, C2-1): a row, not a stop; both sides read "absent or NULL".
      for (tn in c(NA_character_, "")) {
        a <- v6_snap()
        a$reporting_package$tables[[1]] <- data.frame(target = c("report_x", tn), source = "x_package",
                                                      selection = "t1")
        attr(a, "read_errors") <- stats::setNames(paste("FX read error", tn), tn)
        r <- v6_cmp(a, a); x <- v6_row(r, paste0("promoted target ", tn))
        stopifnot(nrow(x) == 1L, isFALSE(x$match), identical(x$detail, v6_both(tn, "absent or NULL")),
                  identical(r$outcomes$rule, "comparison_mismatch"))
      }
    },
    "review (V): V41 the store comparator: a one-sided unreadable value names its side; review_package stays not compared; every other row is unchanged (AC6 S2)" = function() {
      base <- v6_cmp(v6_snap(), v6_snap())
      # 1. Unreadable in the analysis store only.
      r <- v6_cmp(v6_snap("dataset"), v6_snap())
      x <- v6_row(r, V6_DS)
      stopifnot(isFALSE(x$match), identical(x$detail, paste0(V6_GONE, " (analysis: FX read error dataset)")),
                identical(r$outcomes$rule, "comparison_mismatch"))
      # 2. Every other row is unchanged; no row is added or removed.
      keep <- base$table$item != V6_DS
      stopifnot(nrow(r$table) == nrow(base$table), identical(r$table[keep, ], base$table[keep, ]),
                all(base$table$match %in% c(TRUE, NA)), nrow(base$outcomes) == 0L,
                length(attr(v6_snap(), "read_errors")) == 0L)
      # 3. Unreadable in the fresh store only (a package).
      x <- v6_row(v6_cmp(v6_snap(), v6_snap("x_package")), "x_package")
      stopifnot(isFALSE(x$match), identical(x$detail, paste0(V6_GONE, " (fresh: FX read error x_package)")))
      # 4. review_package unreadable in the fresh store stays not compared.
      r <- v6_cmp(v6_snap(), v6_snap("review_package"))
      stopifnot(is.na(v6_row(r, "review_package (review task package)")$match), nrow(r$outcomes) == 0L,
                identical(r$table, base$table))
      # 5. The analysis side is still normalized when the fresh side is
      # unreadable: the re: count of "Excluded from comparison" is unchanged.
      st <- function(s) {
        s$dataset$p <- c("pilot/m/bootstrap_run_20261003T065219Z_b11484528f45d/a.csv", "q")
        s
      }
      vol <- stats::setNames("stamp", V_STAMP)
      e0 <- rv("rv_compare_stores")(st(v6_snap()), st(v6_snap()), v_spec(), vol)$excluded
      e1 <- rv("rv_compare_stores")(st(v6_snap()), v6_snap("dataset"), v_spec(), vol)$excluded
      stopifnot(identical(e0$removed, 1L), identical(e1, e0))
      # 6. The attestation unreadable in the analysis store, the fresh one
      # without a fingerprint: that row compared NULL against NULL (TRUE)
      # before the guard; now both rows are mismatches, rule attestation.
      b <- v6_snap(); b$environment_attestation$fingerprint <- NULL
      r <- v6_cmp(v6_snap("environment_attestation"), b)
      x <- v6_row(r, "environment_attestation: fingerprint")
      stopifnot(isFALSE(x$match),
                identical(x$detail, paste0(V6_GONE, " (analysis: FX read error environment_attestation)")),
                identical(r$outcomes$rule, c("attestation", "attestation")))
    }))

  # ---- Part V, AC7 ----
  # PDF graphical content: page rasters in the identical-text branch of
  # rv_compare_files (V42) and the disclosure note of the review package (V43).
  # Fixture figures: one panel per element of `ys` (fixed axes, so the tick
  # labels, the only text, do not depend on the points), drawn by
  # grDevices::pdf; with `lab`, each page is bare: the line `lab` and the
  # points only.
  v7_fig <- function(f, ys, col = "black", lab = NULL) {
    dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
    grDevices::pdf(f, width = 5, height = 4)
    for (y in ys) {
      if (is.null(lab)) {
        graphics::plot(1:4, y, xlim = c(0, 5), ylim = c(0, 10), col = col, pch = 19,
                       xlab = "Fixture x", ylab = "Fixture y", main = "Fixture panel")
      } else {
        # A bare page with one left-aligned line, as vj_pdf draws: the layout
        # columns stay put when only a stamp in the line differs.
        graphics::plot.new(); graphics::text(0, 0.9, lab, adj = 0, xpd = NA)
        graphics::points(1:4 / 5, y / 10, col = col, pch = 19)
      }
    }
    invisible(grDevices::dev.off())
  }
  V7_Y <- c(1, 2, 3, 4)
  # rv_compare_files on one figure `nm` of a fixture eda package: analysis
  # panels `an`, fresh panels `fr`; `an_lab` and `fr_lab` as in v7_fig.
  v7_cmp <- function(tag, an, fr, nm = "fig.pdf", col = "black", an_lab = NULL, fr_lab = NULL, rf11 = character(0)) {
    A <- file.path(FX, paste0(tag, "a")); C <- file.path(FX, paste0(tag, "c")); W <- "_targets_review_X"
    dir.create(A); dir.create(C); A <- normalizePath(A); C <- normalizePath(C)
    v7_fig(file.path(A, "pilot/x/eda", nm), an, lab = an_lab)
    v7_fig(file.path(C, W, "pilot/x/eda", nm), fr, col = col, lab = fr_lab)
    fo <- function(ws) list(eda_package = list(artifacts = data.frame(
      artifact = "fixture figure", path = paste0(ws, "pilot/x/eda/", nm), source = "fixture",
      stringsAsFactors = FALSE)))
    rv("rv_compare_files")(fo(""), fo(paste0(W, "/")), c(examined = A, copy = C), "pilot/x", W,
                           v_spec(ex = A, cp = C, ws = W), rf11 = rf11)
  }
  # v7_cmp with a fixture pdftoppm first on PATH (PATH restored on return):
  # the fixture runs the sh lines `body`, with $r its output root (the last
  # argument).
  v7_shim <- function(tag, body, ...) {
    bin <- file.path(FX, paste0(tag, "bin")); dir.create(bin)
    writeLines(c("#!/bin/sh", "# Fixture pdftoppm (AC7 V42).", "for a; do r=$a; done", body),
               file.path(bin, "pdftoppm"))
    Sys.chmod(file.path(bin, "pdftoppm"), "0755")
    old <- Sys.getenv("PATH"); on.exit(Sys.setenv(PATH = old))
    Sys.setenv(PATH = paste(bin, old, sep = ":"))
    v7_cmp(tag, ...)
  }
  # Stops, naming the row it saw, unless the match column is exactly `want`
  # (plan K-4: every match assertion of V42 names the row).
  v7_match <- function(r, want, item)
    if (!identical(r$table$match, want))
      stop("item ", item, " row: ", paste(r$table$class, r$table$match, r$table$detail,
                                          sep = " / ", collapse = " | "))
  V7_CLS <- "pdf, identical pdftotext text, pages and page rasters"
  V7_NONE <- "text identical; page rasters unavailable (pdftoppm failed or rendered no page)"

  cases <- c(cases, list(
    "review (V): V42 a PDF outside the R-F11 pair with identical text is compared as page rasters, without a tolerance; a failed render is a mismatch (AC7 S1)" = function() {
      # 1. The same figure drawn twice: one row, the raster class, a match.
      r <- v7_cmp("v42a", list(V7_Y), list(V7_Y))
      v7_match(r, TRUE, 1)
      stopifnot(nrow(r$table) == 1L, identical(r$table$class, V7_CLS),
                identical(r$table$detail, "text identical; page rasters identical at 150 DPI, 1 pages"),
                nrow(r$outcomes) == 0L)
      # 2. Every point moved, the text unchanged: a mismatch of the owning stage.
      r <- v7_cmp("v42b", list(V7_Y), list(V7_Y + 1))
      v7_match(r, FALSE, 2)
      stopifnot(identical(r$table$class, V7_CLS),
                identical(r$table$detail, "text identical; page rasters differ at 150 DPI on 1 of 1 pages"),
                identical(r$outcomes$rule, "file_mismatch"),
                identical(r$outcomes$owner, "owning stage of pilot/x/eda/fig.pdf"))
      # 3. Two pages, the colour changed on both, then the points on page 2 only.
      r <- v7_cmp("v42c", list(V7_Y, V7_Y), list(V7_Y, V7_Y), col = "red")
      v7_match(r, FALSE, "3a")
      stopifnot(identical(r$table$detail, "text identical; page rasters differ at 150 DPI on 2 of 2 pages"))
      r <- v7_cmp("v42d", list(V7_Y, V7_Y), list(V7_Y, rev(V7_Y)))
      v7_match(r, FALSE, "3b")
      stopifnot(identical(r$table$detail, "text identical; page rasters differ at 150 DPI on 1 of 2 pages"))
      # 4. Text equal only after normalization (a run stamp of the fixture
      # pattern V_STAMP on the page, equal in length on both sides so the
      # layout columns stay put): the text-only class, a match, rasters not
      # compared although they differ (the disclosed limit).
      r <- v7_cmp("v42e", list(V7_Y), list(V7_Y + 1), an_lab = "Run vpc_run_20261001T000000Z_aaaaaaaaaaaaa",
                  fr_lab = "Run vpc_run_20261002T000000Z_bbbbbbbbbbbbb")
      v7_match(r, TRUE, 4)
      stopifnot(identical(r$table$class, "pdf, identical pdftotext text and pages"),
                identical(r$table$detail, "text identical"))
      # 5. A pdftoppm that writes a page and exits 1: the failure is a
      # mismatch, never equal rasters.
      r <- v7_shim("v42f", c("printf fixture > \"$r-1.pgm\"", "exit 1"), list(V7_Y), list(V7_Y))
      v7_match(r, FALSE, 5)
      stopifnot(identical(r$table$class, V7_CLS), identical(r$table$detail, V7_NONE),
                identical(r$outcomes$rule, "file_mismatch"))
      # 6. The R-F11 pair keeps its text rule: a fixture figure with every
      # point moved still matches (the disclosed limit).
      r <- v7_cmp("v42g", list(V7_Y), list(V7_Y + 1), nm = "explicit.pdf", rf11 = "explicit.pdf")
      v7_match(r, TRUE, 6)
      stopifnot(identical(r$table$class, "pdf, R-F11 text rule"))
      # 7. Different text: the text-only class and a mismatch, as before.
      r <- v7_cmp("v42h", list(V7_Y), list(V7_Y), an_lab = "Fixture label one", fr_lab = "Fixture label two")
      v7_match(r, FALSE, 7)
      stopifnot(identical(r$table$class, "pdf, identical pdftotext text and pages"),
                identical(r$table$detail, "text differs"))
      # 8. A pdftoppm that exits 0 and renders no page: a mismatch too. The
      # fixture records its arguments: grey, at the explicit 150 DPI.
      argf <- file.path(FX, "v42i.args")
      r <- v7_shim("v42i", c(paste0("printf '%s\\n' \"$@\" > ", shQuote(argf)), "exit 0"), list(V7_Y), list(V7_Y))
      v7_match(r, FALSE, 8)
      stopifnot(identical(r$table$detail, V7_NONE), identical(r$outcomes$rule, "file_mismatch"),
                identical(readLines(argf, n = 3L), c("-gray", "-r", "150")))
      # 9. A pdftoppm that exits 0 with one page for the analysis side and two
      # for the fresh side: a mismatch naming both page counts.
      r <- v7_shim("v42j", c("case \"$*\" in */v42ja/*) printf a > \"$r-1.pgm\" ;;",
                             "*) printf a > \"$r-1.pgm\"; printf b > \"$r-2.pgm\" ;; esac", "exit 0"),
                   list(V7_Y), list(V7_Y))
      v7_match(r, FALSE, 9)
      stopifnot(identical(r$table$detail, "text identical; page rasters differ: 1 and 2 pages"),
                identical(r$outcomes$rule, "file_mismatch"))
      # 10. No page raster is left in tempdir().
      stopifnot(!length(list.files(tempdir(), "^rv-raster-")))
    },
    "review (V): V43 the public reproduction uses the default PDF comparison and reports its classes" = function() {
      tt <- v_env(c(PMX_REVIEW_REPRODUCTION = NA), vmod(v_block(), all = TRUE))
      find_call <- function(x, name) {
        if (!is.call(x)) return(NULL)
        if (identical(x[[1L]], as.name(name))) return(x)
        for (a in as.list(x)[-1L]) {
          hit <- find_call(a, name)
          if (!is.null(hit)) return(hit)
        }
        NULL
      }
      cmp <- find_call(vcmd(tt, "review_reproduction"), "rv_compare_files")
      pkg <- find_call(vcmd(tt, "review_package"), "list")
      stopifnot(is.call(cmp), is.null(cmp$rf11) || identical(cmp$rf11, quote(character(0))),
                identical(pkg$tables[["Artifact comparison"]], quote(r$files)),
                identical(pkg$tables[["Report text comparison"]], quote(r$text)))
    }))

  cases <- c(cases, list(
    "review (V): V44 declared simulation settings match run records and every scenario count" = function() {
      sim <- data.frame(
        declared = c("simulation/seed", "simulation/n", "simulation/uncertainty"),
        item = c("simulation run seed", "simulation subjects per scenario", "simulation uncertainty tier"),
        map_citation = "Fixture simulation declaration",
        accessor = c('x$tables[["Run record"]]$seed',
                     'with(x$tables[["Exposure metrics"]], n[match(x$tables[["Scenario definitions"]]$SCENARIO, SCENARIO)])',
                     'x$tables[["Run record"]]$uncertainty_tier'),
        target = "simulation_package", compare = "equal", owner = "simulation (pmx-simulator)")
      # The three rows on stub packages: a declared seed 101, 7 subjects per
      # scenario, tier "iiv"; two declared scenarios of two metrics each.
      inputs <- list(simulation = list(seed = 101, n = 7, uncertainty = "iiv"))
      pkg <- function(rec = data.frame(seed = 101, uncertainty_tier = "iiv"), n = rep(7, 4)) {
        tb <- list("Exposure metrics" = data.frame(SCENARIO = rep(c("S1", "S1", "S2", "S2"), length.out = length(n)),
                                                   metric = rep(c("cmax", "auc_0_inf"), length.out = length(n)),
                                                   n = n),
                   "Scenario definitions" = data.frame(SCENARIO = c("S1", "S2")))
        tb[["Run record"]] <- rec
        list(simulation_package = list(tables = tb))
      }
      run <- function(values) {
        r <- rv("rv_concordance")(sim, inputs, values, character(0), "A4", "A4", "simulation")
        a <- r$table[r$table$part == "(a) settings", ]
        list(match = stats::setNames(a$match, sub("^simulation/", "", a$declared_field)), a = a,
             o = r$outcomes[r$outcomes$rule %in% c("concordance", "not_evaluated"), ])
      }
      seen <- function(r, item) stop("item ", item, ": match ", paste(r$match, collapse = " "),
                                     "; outcomes ", paste(r$o$rule, collapse = " "))
      # 2. All three equal: TRUE, no concordance or not-evaluated outcome.
      r <- run(pkg())
      if (!identical(r$match, c(seed = TRUE, n = TRUE, uncertainty = TRUE)) || nrow(r$o)) seen(r, 2)
      stopifnot(identical(r$a$executed, c("101", "7", "iiv")))
      # 3. The run record's seed differs: that row only, a mismatch routed to the simulator.
      r <- run(pkg(rec = data.frame(seed = 102, uncertainty_tier = "iiv")))
      if (!identical(r$match, c(seed = FALSE, n = TRUE, uncertainty = TRUE))) seen(r, 3)
      stopifnot(identical(r$o$rule, "concordance"), identical(r$o$owner, "simulation (pmx-simulator)"),
                identical(r$o$finding, "concordance mismatch: simulation run seed"))
      # 4. One scenario counted one subject fewer: the subjects row only.
      r <- run(pkg(n = c(7, 7, 6, 6)))
      if (!identical(r$match, c(seed = TRUE, n = FALSE, uncertainty = TRUE))) seen(r, 4)
      stopifnot(identical(r$o$rule, "concordance"))
      # 5. A different tier in the run record: the tier row only.
      r <- run(pkg(rec = data.frame(seed = 101, uncertainty_tier = "none")))
      if (!identical(r$match, c(seed = TRUE, n = TRUE, uncertainty = FALSE))) seen(r, 5)
      stopifnot(identical(r$o$rule, "concordance"))
      # 6. No run record table: seed and tier not evaluated, never TRUE.
      r <- run(pkg(rec = NULL))
      if (!identical(r$match, c(seed = NA, n = TRUE, uncertainty = NA))) seen(r, 6)
      stopifnot(identical(r$o$rule, c("not_evaluated", "not_evaluated")))
      # 7. An exposure-metrics table with no row: every declared scenario lacks
      # its count, so the subjects row is a mismatch.
      r <- run(pkg(n = numeric(0)))
      if (!identical(r$match, c(seed = TRUE, n = FALSE, uncertainty = TRUE))) seen(r, 7)
      stopifnot(identical(r$o$rule, "concordance"))
      # 8. One declared scenario absent from the exposure metrics (S1 only): a
      # mismatch, although every count present equals the declaration.
      r <- run(pkg(n = c(7, 7)))
      if (!identical(r$match, c(seed = TRUE, n = FALSE, uncertainty = TRUE))) seen(r, 8)
      stopifnot(identical(r$o$rule, "concordance"))
      # 9. Either table the subjects row reads is absent: not evaluated, never TRUE.
      for (tab in c("Exposure metrics", "Scenario definitions")) {
        p <- pkg()
        p$simulation_package$tables[[tab]] <- NULL
        r <- run(p)
        if (!identical(r$match, c(seed = TRUE, n = NA, uncertainty = TRUE))) seen(r, paste0("9 (", tab, ")"))
        stopifnot(identical(r$o$rule, "not_evaluated"))
      }
    }))

  cases <- c(cases, list(
    "review (V): V45 the cross-document date row reads the MAR title page only: a line of page 1 that is exactly the declared date (AC10 S1)" = function() {
      # Fixture text in the shape of pdftotext -layout output: every page ends
      # with a form feed, so the first line of each later page starts with
      # one. A fixture date, not an analysis value.
      d <- "2031-02-03"
      tp <- function(date_line = d) c("                Model Analysis Report - FX-001", "                Version 1.0", "",
                                      "                Fixture author", "", paste0("                ", date_line), "", "",
                                      "Table of contents", "1 Introduction . . . . . . . . . . . . . . . 2")
      body <- c("\f1 Introduction", "Body text of page 2.", "\fAppendix", "Body text of page 3.", "\f")
      section <- "E. Recorded review notes"
      manifest <- c(paste0("section: ", section, " / fixture"), "# result: PASS")
      cx <- function(mar, assessment = "- MAR: fixture/mar.pdf", evidence = manifest) {
        r <- rv("rv_crossdoc")(assessment, mar, d, 1L, evidence,
                               c(assessment = "A", mar = "M", manifest = "F"), section_prefix = section)
        r$o <- r$outcomes[r$outcomes$rule %in% "concordance", ]
        r
      }
      seen <- function(r, item) stop("item ", item, ": consistent ", paste(r$table$consistent, collapse = " "),
                                     "; outcomes ", paste(r$outcomes$rule, collapse = " "))
      fails <- function(r, item) {
        if (!identical(r$table$consistent, c(TRUE, FALSE, TRUE))) seen(r, item)
        stopifnot(identical(r$o$finding, "the MAR title page (page 1) has no line that reads the declared document date"),
                  identical(r$o$owner, "reporting (pmx-report-writer)"), identical(r$o$evidence_path, "M"))
      }
      # 1. The title block sets the date alone on its line (indented): the row
      # holds, no outcome; the three check labels are unchanged.
      r <- cx(c(tp(), body))
      if (!identical(r$table$consistent, c(TRUE, TRUE, TRUE)) || nrow(r$outcomes)) seen(r, 1)
      stopifnot(identical(r$table$check, c("assessment table MAR cross-reference pending (close-out)",
                                           "MAR title-page date equals the declared document date",
                                           "Archived notes reproduced in the MAR match the expected count and semantic result")))
      for (assessment in c("- MAR: to be delivered", "No cross-reference")) {
        r <- cx(c(tp(), body), assessment)
        stopifnot(identical(r$table$consistent, c(FALSE, TRUE, TRUE)),
                  identical(r$outcomes$evidence_path, "A"),
                  identical(r$outcomes$rule, if (startsWith(assessment, "- MAR:")) "crossref_pending" else "not_evaluated"))
      }
      for (m in list("# result: PASS", c(manifest[1], "# result: FAIL"), c(manifest, manifest[1]))) {
        r <- cx(c(tp(), body), evidence = m)
        stopifnot(identical(r$table$consistent, c(TRUE, TRUE, FALSE)),
                  identical(r$outcomes$rule, "concordance"),
                  identical(r$outcomes$evidence_path, "F"))
      }
      tt <- v_env(c(PMX_REVIEW_REPRODUCTION = NA), vmod(v_block(), all = TRUE))
      expr <- vcmd(tt, "review_crossdoc")
      stopifnot(all(c("review_assessment_file", "reporting_package", "report_semantic_manifest",
                      "report_semantic_check") %in% all.names(expr)),
                grepl(section, paste(deparse(expr), collapse = " "), fixed = TRUE))
      # 2. The title page shows another date; the declared date occurs only on
      # page 3, in a body line (a deviation-log entry) or alone on its line (a
      # signature date): the row fails.
      for (ln in c(paste0("1. Date: ", d, ". Change: fixture."), paste0("      ", d)))
        fails(cx(c(tp("2031-02-04"), body[1:3], ln, body[4:5])), 2)
      # 3. The declared date occurs on page 1 only inside a line with other
      # text: a contents entry, a prefix ("Date: ") or a suffix ("(draft)").
      for (ln in c(paste("A1 amendment of", d, ". . . . 4"), paste0("Date: ", d), paste(d, "(draft)")))
        fails(cx(c(tp("2031-02-04"), ln, body)), 3)
      # 4. Text without a form feed has no identifiable title page.
      fails(cx(tp()), 4)
      # 5. No MAR text (the MAR absent).
      fails(cx(character(0)), 5)
      # 6. The declared date alone on a line of page 2, the page after the
      # title page: it is not on the title page.
      fails(cx(c(tp("2031-02-04"), body[1], paste0("      ", d), body[2:5])), 6)
      # 7. A one-page MAR (the text ends with its one form feed), a title date
      # line followed by spaces (trimmed on both sides), and a title date that
      # recurs in the body: the row holds, no outcome.
      for (mar in list(c(tp(), "\f"), c(tp(paste0(d, "   ")), body), c(tp(), body[1], paste0("1. Date: ", d, "."), body[3:5]))) {
        r <- cx(mar)
        if (!identical(r$table$consistent, c(TRUE, TRUE, TRUE)) || nrow(r$outcomes)) seen(r, 7)
      }
    }))

  cases <- c(cases, list(
    "review (V): V46 the excluded-from-comparison table gives each expected change the count of its own blanked rows, not the total (AC11 S1)" = function() {
      # Synthetic snapshots of the rv_store_values form (v6_snap). The code
      # inventory lists fx/a.R, fx/b.R, fx/z.R and a row without a file; a
      # table of x_package also has file and md5 columns and lists fx/a.R
      # again (`extra` more times). With fx/a.R, fx/b.R and fx/c.R (never
      # listed) declared, the analysis side holds 2, 1 and 0 of their rows.
      ci <- "Code inventory (gate traceability; not rendered in the MAR)"
      inv <- function(s, extra = 0L) {
        s$reporting_package$tables[[ci]] <- data.frame(file = c("fx/a.R", "fx/b.R", "fx/z.R", NA),
                                                       bytes = c(1, 2, 3, 4), md5 = c("m1", "m2", "m3", "m4"),
                                                       stringsAsFactors = FALSE)
        s$x_package$tables$t2 <- data.frame(file = rep("fx/a.R", 1L + extra), md5 = "n1", stringsAsFactors = FALSE)
        s
      }
      ec <- c("fx/a.R" = "fx reason a", "fx/b.R" = "fx reason b", "fx/c.R" = "fx reason c")
      vol <- stats::setNames("fx time", "accessed_at")
      cmp <- function(a, b, e) rv("rv_compare_stores")(a, b, v_spec(), vol, e)
      ecr <- function(r) r$excluded[r$excluded$kind == "expected change: md5 and bytes of its code inventory row", ,
                                    drop = FALSE]
      item <- function(i, r, ...) {
        ok <- c(...)
        if (!all(ok)) stop(sprintf("item %d: check %s failed; expected-change removed %s", i,
                                   paste(which(!ok), collapse = ","), paste(ecr(r)$removed, collapse = " ")),
                           call. = FALSE)
      }
      # 1. Three entries, the same snapshot on both sides: 2, 1 and 0 in the
      # declared order; entries, reasons and the volatile row unchanged; the
      # table keeps plain row names; every row matches, no outcome.
      r <- cmp(inv(v6_snap()), inv(v6_snap()), ec)
      e <- ecr(r)
      item(1, r, identical(e$removed, c(2L, 1L, 0L)), identical(e$entry, names(ec)), identical(e$reason, unname(ec)),
           identical(rownames(r$excluded), as.character(seq_len(nrow(r$excluded)))),
           identical(r$excluded[seq_len(nrow(r$excluded) - 3L), ], cmp(inv(v6_snap()), inv(v6_snap()), character(0))$excluded),
           all(r$table$match %in% c(TRUE, NA)), nrow(r$outcomes) == 0L)
      # 2. The declared order is kept: the entries reversed, the counts reversed.
      r <- cmp(inv(v6_snap()), inv(v6_snap()), rev(ec))
      item(2, r, identical(ecr(r)$removed, c(0L, 1L, 2L)), identical(ecr(r)$entry, rev(names(ec))))
      # 3. Counted on the analysis side only (m-6): two more fx/a.R rows in the
      # fresh store leave the counts as they are (that table is the one
      # mismatch); the same two rows in the analysis store count.
      r <- cmp(inv(v6_snap()), inv(v6_snap(), extra = 2L), ec)
      item(3, r, identical(ecr(r)$removed, c(2L, 1L, 0L)), sum(r$table$match %in% FALSE) == 1L)
      r <- cmp(inv(v6_snap(), extra = 2L), inv(v6_snap()), ec)
      item(3, r, identical(ecr(r)$removed, c(4L, 1L, 0L)))
      # 4. Blanking is unchanged: md5 and bytes of every listed row differ on
      # the fresh side and still match; an unlisted file's md5 is a mismatch.
      f <- inv(v6_snap())
      f$reporting_package$tables[[ci]]$md5[1:2] <- c("q1", "q2")
      f$reporting_package$tables[[ci]]$bytes[1:2] <- c(8, 9)
      f$x_package$tables$t2$md5 <- "q3"
      r <- cmp(inv(v6_snap()), f, ec)
      item(4, r, all(r$table$match %in% c(TRUE, NA)), identical(ecr(r)$removed, c(2L, 1L, 0L)))
      f$reporting_package$tables[[ci]]$md5[3] <- "q4"
      r <- cmp(inv(v6_snap()), f, ec)
      item(4, r, sum(r$table$match %in% FALSE) == 1L, any(grepl(ci, r$table$item[r$table$match %in% FALSE], fixed = TRUE)))
      # 5. One entry: its own count.
      r <- cmp(inv(v6_snap()), inv(v6_snap()), ec[2])
      item(5, r, identical(ecr(r)$removed, 1L))
      # 6. No table with file and md5 columns: every count is 0.
      r <- cmp(v6_snap(), v6_snap(), ec[1:2])
      item(6, r, identical(ecr(r)$removed, c(0L, 0L)))
    }))

  cases <- c(cases, .ac14_review_cases(file.path(REPO, "tools")))
  failed <- character(0)
  for (id in names(cases)) {
    err <- tryCatch({ cases[[id]](); NULL }, error = function(e) conditionMessage(e))
    if (is.null(err)) {
      cat(sprintf("PASS %s\n", id))
    } else {
      cat(sprintf("FAIL %s: %s\n", id, err))
      failed <- c(failed, id)
    }
  }
  if (length(failed) > 0L) {
    cat(sprintf("nonmem base-model tests: FAILED %s\n", paste(failed, collapse = ", ")))
    quit(status = 1L)
  }
  cat("nonmem base-model tests: ALL PASS\n")
  invisible(TRUE)
}

.run_grid_selected <- function() {
  # This selector deliberately precedes the default battery's setup. Its only
  # subprocesses are the two real store readers below; fixture builds are local.
  payload <- normalizePath(getwd(), mustWork = TRUE)
  work <- tempfile("public-grid-fixture-", tmpdir = dirname(tempdir()))
  dir.create(work)
  e <- new.env(parent = globalenv())
  for (f in c("gate.R", "gate_package.R", "evidence_notes.R", "method_manifest.R", "review_stage.R"))
    sys.source(file.path(payload, "tools", f), envir = e)
  sys.source(file.path(payload, "pipeline", "compose.R"), envir = e)

  # Record the installed sequence implementation used by the allocation cases.
  saveRDS(list(version = R.version.string, sha256 = digest::digest(body(base::seq.default), algo = "sha256")),
          file.path(work, "installed-seq-source.rds"))

  grid <- list(start = 0, end = 1, delta = 0.25)
  times <- seq(from = grid$start, to = grid$end, by = grid$delta)
  frame <- do.call(rbind, lapply(c("alpha", "beta"), function(sc) {
    ids <- if (sc == "alpha") c(7, 31) else c(19, 85)
    do.call(rbind, lapply(ids, function(id)
      data.frame(SCENARIO = sc, ID = id, TIME = times)))
  }))
  frame <- frame[rev(seq_len(nrow(frame))), , drop = FALSE]
  rownames(frame) <- NULL
  pkg <- list(tables = list("Scenario definitions" =
    data.frame(SCENARIO = c("alpha", "beta"), n = 2L, grid = "0 to 1 by 0.25"),
    "Exposure metrics" = data.frame(SCENARIO = c("alpha", "beta"), n = 2L),
    "Run record" = data.frame(seed = 123L, uncertainty_tier = "fixture")),
    sentinel = "simulation sentinel")
  clean <- list(simulation_package = pkg, sim_results = list(sim = frame),
    reporting_package = list(tables = list("Deviation log as rendered" = data.frame(number = "A1"))))
  broken <- clean
  selected <- frame$SCENARIO == "alpha" & frame$ID == 7
  broken$sim_results$sim <- frame[!(selected & frame$TIME == 0.5), , drop = FALSE]
  store_ok <- file.path(work, "success-store")
  store_missing <- file.path(work, "missing-store")
  script_ok <- file.path(work, "success.R")
  script_missing <- file.path(work, "missing.R")
  # The disk scratch is inside the checkout. Anchor default config/root
  # discovery locally before loading a targets script or spawning a reader.
  old <- setwd(work)
  on.exit(setwd(old), add = TRUE)
  config <- file.path(work, "_targets.yaml")
  writeLines(c("main:", "  script: success.R", "  store: success-store"), config)
  writeLines(character(0), file.path(work, ".here"))
  old_config <- Sys.getenv("TAR_CONFIG", unset = NA_character_)
  Sys.setenv(TAR_CONFIG = config)
  on.exit(if (is.na(old_config)) Sys.unsetenv("TAR_CONFIG") else
    Sys.setenv(TAR_CONFIG = old_config), add = TRUE)
  saveRDS(broken, file.path(work, "synthetic.rds"))
  writeLines(c(sprintf("fixture <- readRDS(%s)", dQuote(file.path(work, "synthetic.rds"), q = FALSE)),
    "list(targets::tar_target_raw('reporting_package', quote(fixture$reporting_package)),",
    "targets::tar_target_raw('simulation_package', quote(fixture$simulation_package)),",
    "targets::tar_target_raw('sim_results', quote(fixture$sim_results)))"), script_ok)
  writeLines(c(sprintf("fixture <- readRDS(%s)", dQuote(file.path(work, "synthetic.rds"), q = FALSE)),
    "list(targets::tar_target_raw('reporting_package', quote(fixture$reporting_package)),",
    "targets::tar_target_raw('simulation_package', quote(fixture$simulation_package)))"), script_missing)
  targets::tar_make(script = script_ok, store = store_ok, callr_function = NULL,
                    reporter = "silent", envir = new.env(parent = globalenv()))
  targets::tar_make(script = script_missing, store = store_missing, callr_function = NULL,
                    reporter = "silent", envir = new.env(parent = globalenv()))

  inputs <- list(simulation = list(grid = grid, n = 2L, seed = 123L))
  rows <- data.frame(item = c("simulation: grid start", "simulation: grid end", "simulation: grid step",
                            "simulation: recorded seed"),
                     map_citation = "Synthetic grid declaration",
                     declared = c("simulation/grid/start", "simulation/grid/end", "simulation/grid/delta",
                                  "simulation/seed"),
                     target = c(rep("sim_results", 3L), "simulation_package"),
                     accessor = c(rep("NULL", 3L), "x$tables[['Run record']]$seed"),
                     compare = "equal", owner = "simulation (pmx-simulator)")
  grid_rows <- rows[seq_len(3L), ]
  read_store <- function(store) {
    extra <- "sim_results"
    list(store = store, extra = extra, value = e$rv_store_values(store, extra = extra))
  }
  reads <- lapply(c(store_ok, store_missing), read_store)
  wire <- e$rv_concordance(grid_rows, inputs, reads[[1L]]$value, character(), "A1", "A1", "simulation")
  absent <- reads[[2L]]$value
  saveRDS(reads, file.path(work, "two-reader-receipt.rds"))
  stopifnot(length(reads) == 2L)
  snapshot <- reads[[1L]]$value
  clean <- snapshot; clean$sim_results <- list(sim = frame)
  compare <- function(v = clean, g = grid, rr = grid_rows, n = inputs$simulation$n) {
    inp <- inputs; inp$simulation$grid <- g; inp$simulation$n <- n
    e$rv_concordance(rr, inp, v, c("## 2. Deviations", "## 3. Next"), "A1", "A1", "simulation")
  }
  matches <- function(r) unname(r$table$match[match(grid_rows$item, r$table$item)])
  unknown <- function(r) {
    stopifnot(all(is.na(matches(r))), any(r$outcomes$rule == "not_evaluated"),
      all(nzchar(r$outcomes$finding[r$outcomes$rule == "not_evaluated"])))
  }
  perturb <- function(kind) {
    v <- clean; f <- v$sim_results$sim
    k <- which(f$SCENARIO == "alpha" & f$ID == 7)
    if (kind == "missing-interior") f <- f[-k[which(f$TIME[k] == 0.5)], , drop = FALSE]
    if (kind == "extra") { a <- f[k[1L], ]; a$TIME <- 0.125; f <- rbind(f, a) }
    if (kind == "duplicate") f <- rbind(f, f[k[which(f$TIME[k] == 0.5)], , drop = FALSE])
    if (kind == "first") f$TIME[k[which.min(f$TIME[k])]] <- -0.25
    if (kind == "last") f$TIME[k[which.max(f$TIME[k])]] <- 1.25
    v$sim_results$sim <- f; v
  }
  cases <- list()
  cases[["G-clean"]] <- function() {
    r <- compare(); stopifnot(identical(matches(r), rep(TRUE, 3L)),
      identical(clean$simulation_package, pkg), identical(r$table$map_citation[seq_len(3L)], grid_rows$map_citation))
    a <- compare(rr = rows); b <- compare(perturb("missing-interior"), rr = rows)
    keep <- !a$table$item %in% grid_rows$item
    stopifnot(identical(a$table[keep, ], b$table[keep, ]))
  }
  cases[["G-read-wire/missing-interior"]] <- function() {
    stopifnot(!inherits(wire, "error"), identical(reads[[1L]]$extra, "sim_results"),
      identical(snapshot$sim_results, broken$sim_results), identical(snapshot$simulation_package, pkg),
      identical(matches(wire), c(TRUE, TRUE, FALSE)))
  }
  for (kind in c("missing-interior", "extra", "duplicate", "first", "last")) local({
    kk <- kind
    cases[[paste0("G-one-group/", kk)]] <<- function() {
      r <- compare(perturb(kk))
      expected <- switch(kk, first = c(FALSE, TRUE, FALSE), last = c(TRUE, FALSE, FALSE), c(TRUE, TRUE, FALSE))
      stopifnot(identical(matches(r), expected), any(r$outcomes$rule == "concordance"),
        all(r$outcomes$owner[r$outcomes$rule == "concordance"] == "simulation (pmx-simulator)"))
    }
  })
  cases[["G-exact/fractional-storage-names"]] <- function() {
    stopifnot(all(matches(compare())))
    v <- clean; v$sim_results$sim$TIME <- as.integer(v$sim_results$sim$TIME * 4)
    stopifnot(all(matches(compare(v, list(start = 0L, end = 4L, delta = 1L)))))
    g <- lapply(grid, function(x) setNames(x, "incidental"))
    stopifnot(all(matches(compare(g = g))))
  }
  cases[["G-exact/unequal-interior"]] <- function() {
    v <- clean; k <- which(v$sim_results$sim$TIME == 0.5)[1L]
    v$sim_results$sim$TIME[k] <- 0.5 + .Machine$double.eps
    stopifnot(identical(matches(compare(v)), c(TRUE, TRUE, FALSE)))
  }
  cases[["G-exact/omitted-endpoint"]] <- function() {
    v <- clean; v$sim_results$sim <- v$sim_results$sim[v$sim_results$sim$TIME <= 0.75, ]
    stopifnot(identical(matches(compare(v, list(start = 0, end = 0.9, delta = 0.25))), c(TRUE, FALSE, TRUE)))
  }
  cases[["G-evidence/read-errors"]] <- function() {
    stopifnot(identical(reads[[2L]]$extra, "sim_results"), "sim_results" %in% names(attr(absent, "read_errors")))
    unknown(compare(absent))
  }
  invalid <- list(absent = NULL, atomic = 1, nonframe = list(TIME = 0), empty = frame[FALSE, ])
  for (name in names(invalid)) local({
    nn <- name; bad <- invalid[name]
    cases[[paste0("G-evidence/", nn)]] <<- function() {
      v <- clean; v$sim_results <- if (nn == "absent") NULL else list(sim = bad[[1L]])
      unknown(compare(v))
    }
  })
  for (kind in c("column", "duplicate-column", "identifier", "nonnumeric", "NA", "infinite", "missing-scenario", "unexpected-scenario", "count", "metadata", "declared-count")) local({
    kk <- kind
    cases[[paste0("G-evidence/", kk)]] <<- function() {
      v <- clean; f <- v$sim_results$sim
      if (kk == "column") f$TIME <- NULL
      if (kk == "duplicate-column") { f$extra <- f$TIME; names(f)[ncol(f)] <- "TIME" }
      if (kk == "identifier") f$ID[] <- NA_real_
      if (kk == "nonnumeric") f$TIME <- as.character(f$TIME)
      if (kk == "NA") f$TIME[] <- NA_real_
      if (kk == "infinite") f$TIME[] <- Inf
      if (kk == "missing-scenario") f <- f[f$SCENARIO == "alpha", ]
      if (kk == "unexpected-scenario") f$SCENARIO[] <- "unknown"
      if (kk == "count") f <- f[!(f$SCENARIO == "alpha" & f$ID == 7), ]
      if (kk == "metadata") v$simulation_package$tables[["Scenario definitions"]] <- NULL
      v$sim_results$sim <- f; r <- compare(v)
      if (kk == "declared-count") r <- compare(v, n = NA_real_)
      stopifnot(any(is.na(matches(r))), any(r$outcomes$rule == "not_evaluated"), !any(matches(r) %in% FALSE))
    }
  })
  for (kind in c("atomic-grid", "zero", "wrong-sign", "nonfinite", "nonscalar", "classed", "string", "complex", "duplicate-grid-field")) local({
    kk <- kind
    cases[[paste0("G-evidence/", kk)]] <<- function() {
      g <- grid
      if (kk == "atomic-grid") g <- unlist(g)
      if (kk == "zero") g$delta <- 0
      if (kk == "wrong-sign") g$delta <- -0.25
      if (kk == "nonfinite") g$delta <- Inf
      if (kk == "nonscalar") g$delta <- c(0.25, 0.5)
      if (kk == "classed") g$delta <- structure(0.25, class = "fixture")
      if (kk == "string") g$delta <- "0.25"
      if (kk == "complex") g$delta <- 0.25 + 1i
      if (kk == "duplicate-grid-field") g <- c(g, list(delta = 0.25))
      unknown(compare(g = g))
    }
  })
  # Instrument only literal construction dispatch, leaving the base oracle
  # intact. The receipts prove construction was reached and its actual size.
  constructions <- list()
  e$seq <- function(...) {
    result <- base::seq(...)
    constructions[[length(constructions) + 1L]] <<- length(result)
    result
  }
  bounds <- list(
    huge = list(g = list(start = 0, end = 1e8, delta = 1), time = c(0, 1e8), match = FALSE, construct = FALSE),
    fallback = list(g = list(start = -1e308, end = 1e308, delta = 1e308), time = c(-1e308, 0, 1e308), match = TRUE, construct = TRUE),
    fallback_unknown = list(g = list(start = -1e308, end = 1e308, delta = 1), time = c(-1e308, 1e308), match = NA, construct = FALSE),
    ceiling = list(g = list(start = 0, end = .Machine$integer.max + 1, delta = 1), time = c(0, 1), match = NA, construct = FALSE),
    equal = list(g = list(start = 2, end = 2, delta = 1), time = 2, match = TRUE, construct = TRUE),
    relative = list(g = list(start = 1e10, end = 1e10 + 1e-4, delta = 1e-10), time = 1e10, match = TRUE, construct = TRUE),
    decreasing = list(g = list(start = 1, end = 0, delta = -0.25), time = times, match = FALSE, construct = TRUE),
    boundary_inside = list(g = list(start = 0, end = 3, delta = 1), time = c(0, 3), match = FALSE, construct = TRUE),
    boundary_outside = list(g = list(start = 0, end = 3.001, delta = 1), time = c(0, 3.001), match = FALSE, construct = FALSE),
    fuzz = list(g = list(start = 0, end = 3 - 5e-11, delta = 1), time = c(0, 1, 2, 3 - 5e-11), match = TRUE, construct = TRUE),
    clamp_negative = list(g = list(start = 3, end = 5e-11, delta = -1), time = c(5e-11, 1, 2, 3), match = FALSE, construct = TRUE),
    representation = list(g = list(start = 0, end = .Machine$integer.max, delta = 1), time = c(0, 1), match = FALSE, construct = FALSE))
  for (name in names(bounds)) local({
    nn <- name; b <- bounds[[nn]]
    cases[[paste0("G-seq-bound/", nn)]] <<- function() {
      constructions <<- list()
      v <- clean
      v$sim_results$sim <- do.call(rbind, lapply(split(frame, interaction(frame$SCENARIO, frame$ID, drop = TRUE)), function(f)
        data.frame(SCENARIO = f$SCENARIO[1L], ID = f$ID[1L], TIME = b$time)))
      r <- compare(v, b$g)
      saveRDS(list(case = nn, lengths = constructions, match = matches(r)), file.path(work, paste0("allocation-", nn, ".rds")))
      stopifnot(identical(matches(r)[3L], b$match), identical(length(constructions) > 0L, b$construct),
        all(unlist(constructions) <= length(b$time) + 2))
    }
  })
  cases[["G-mixed"]] <- function() {
    v <- perturb("missing-interior"); v$sim_results$sim$TIME[1L] <- NA_real_
    r <- compare(v)
    stopifnot(identical(matches(r)[3L], FALSE), all(c("concordance", "not_evaluated") %in% r$outcomes$rule))
  }
  for (state in c("TRUE", "FALSE", "NA", "mixed")) local({
    ss <- state
    cases[[paste0("G-package/", ss)]] <<- function() {
      r <- compare()
      if (ss == "FALSE") r <- compare(perturb("missing-interior"))
      if (ss == "NA") r <- compare(absent)
      if (ss == "mixed") {
        v <- perturb("missing-interior"); v$sim_results$sim$TIME[1L] <- NA_real_; r <- compare(v)
      }
      sentinel <- data.frame(sentinel = "untouched")
      value <- list(table = sentinel, outcomes = e$rv_outcome())
      vals <- setNames(rep(list(value), 6L), c("review_gate_integrity", "review_reproduction", "review_traceability",
        "review_map_concordance", "review_validation", "review_crossdoc"))
      vals$review_map_concordance <- r
      initial <- e$rv_checklist(vals)
      independent <- e$rv_outcome("crossref_pending", NA_character_, "independent sentinel", "fixture")
      finding <- e$rv_findings(rbind(r$outcomes, independent), initial)
      at <- startsWith(initial$item, "domain 2 bullet 3:")
      stopifnot(identical(initial$outcome[at], "met"), any(finding$log$finding == "independent sentinel"))
      if (ss != "TRUE") stopifnot(identical(finding$checklist$outcome[at], "not met"))
      if (ss == "mixed") stopifnot(all(c("concordance", "not_evaluated") %in% r$outcomes$rule))
    }
  })
  failed <- character(0)
  writeLines(names(cases), file.path(work, "case-inventory.txt"))
  for (name in names(cases)) {
    err <- tryCatch({ cases[[name]](); NULL }, error = conditionMessage)
    if (is.null(err)) cat("PASS ", name, "\n", sep = "") else {
      cat("FAIL ", name, ": ", err, "\n", sep = ""); failed <- c(failed, name)
    }
  }
  if (length(failed)) quit(status = 1L)
  cat("grid tests: ALL PASS\n")
  invisible(TRUE)
}

# Run the generated public witnesses before any older composed fixtures.
.run_public_nonmem_cases <- function() {
  file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
  repo <- normalizePath(file.path(dirname(file), "../.."), mustWork = TRUE)
  env <- new.env(parent = globalenv())
  sys.source(file.path(repo, "tools/tests/fixtures/public_workflow/nonmem_cases.R"), env)
  if (!env$public_nonmem_cases(repo)) quit(status = 1L)
}

if (.invoked_as_script()) {
  args <- commandArgs(trailingOnly = TRUE)
  if (identical(args, "--grid-only")) .run_grid_selected()
  else if (identical(args, "--public-fixtures-only")) .run_public_nonmem_cases()
  else if (!length(args)) {
    .run_public_nonmem_cases()
    .run_nonmem_base_model_tests()
  }
  else if (identical(args, "--r15-only")) .run_r15_selected()
  else if (identical(args, "--r15-wiring-only")) .run_r15_selected(wiring = TRUE)
  else stop("Unknown test_nonmem_base_model.R argument", call. = FALSE)
}
