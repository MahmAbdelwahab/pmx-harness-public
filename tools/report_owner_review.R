# Prepare a pending owner handoff; never inspect PDF content or approve a report.
ror_absolute <- function(path, label) {
  if (length(path) != 1L || is.na(path) || !nzchar(path) || !startsWith(path, "/"))
    stop(label, " must be an absolute path", call. = FALSE)
}

ror_file <- function(path, label) {
  ror_absolute(path, label)
  # Requires POSIX paths, compatible /bin/sh with builtin test, and R timeout support.
  # This metadata check follows symlinks; private-parent scope excludes path races.
  status <- tryCatch(
    system2("/bin/sh",
      c("-c", shQuote('test -f "$1"', type = "sh"),
        "report-owner-review-file", shQuote(path, type = "sh")),
      wait = TRUE, timeout = 5L),
    warning = function(w) stop("Unsupported runtime for regular-file validation: ",
                               conditionMessage(w), call. = FALSE),
    error = function(e) stop("Unsupported runtime for regular-file validation: ",
                             conditionMessage(e), call. = FALSE))
  if (!identical(status, 0L) && !identical(status, 1L))
    stop("Unsupported runtime for regular-file validation: unexpected status", call. = FALSE)
  if (!identical(status, 0L) || file.access(path, 4L) != 0L)
    stop(label, " must be an existing readable regular file", call. = FALSE)
  normalizePath(path, mustWork = TRUE)
}

ror_markdown <- function(text) {
  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  text <- gsub(">", "&gt;", text, fixed = TRUE)
  text <- gsub("`", "&#96;", text, fixed = TRUE)
  text <- gsub("|", "&#124;", text, fixed = TRUE)
  text <- gsub("*", "&#42;", text, fixed = TRUE)
  text <- gsub("_", "&#95;", text, fixed = TRUE)
  text <- gsub("[", "&#91;", text, fixed = TRUE)
  gsub("[\r\n]", " ", text)
}

report_owner_review <- function(artifacts_csv, output_dir) {
  artifacts_csv <- ror_file(artifacts_csv, "artifacts_csv")
  ror_absolute(output_dir, "output_dir")
  x <- tryCatch(read.csv(artifacts_csv, check.names = FALSE, colClasses = "character",
                        na.strings = NULL, fill = FALSE, row.names = NULL, stringsAsFactors = FALSE),
                error = function(e) stop("Invalid inventory: ", conditionMessage(e), call. = FALSE))
  optional <- c(analysis_id = "unspecified", stage = "reporting", owner = "owner",
                counterpart_required = "FALSE", counterpart_path = "", expectations_path = "",
                evidence_path = "", comments_record_path = "", extraction_status = "not_requested",
                extraction_evidence_path = "")
  allowed <- c("report_id", "report_path", names(optional))
  if (anyDuplicated(names(x)) || any(!names(x) %in% allowed) ||
      !all(c("report_id", "report_path") %in% names(x)))
    stop("Invalid inventory header: require report_id/report_path and known unique headers", call. = FALSE)
  if (!nrow(x)) stop("Inventory must contain reports", call. = FALSE)
  if (any(!nzchar(trimws(x$report_id))) || anyDuplicated(x$report_id))
    stop("report_id must be nonempty and unique", call. = FALSE)
  for (name in names(optional)) {
    if (!name %in% names(x)) x[[name]] <- rep(optional[[name]], nrow(x))
    blank <- !nzchar(trimws(x[[name]]))
    x[[name]][blank] <- optional[[name]]
  }
  if (any(!x$counterpart_required %in% c("TRUE", "FALSE")))
    stop("counterpart_required must be TRUE or FALSE", call. = FALSE)
  if (any(!x$extraction_status %in% c("not_requested", "supplied", "failed", "empty")))
    stop("Invalid extraction_status", call. = FALSE)
  paths <- c("report_path", "counterpart_path", "expectations_path", "evidence_path",
             "comments_record_path", "extraction_evidence_path")
  hashes <- as.data.frame(matrix("", nrow(x), length(paths), dimnames = list(NULL, paths)), stringsAsFactors = FALSE)
  if (!requireNamespace("digest", quietly = TRUE)) stop("Existing digest package is required", call. = FALSE)
  for (i in seq_len(nrow(x))) {
    if (x$counterpart_required[i] == "TRUE" && !nzchar(x$counterpart_path[i]))
      stop("Required counterpart_path is not supplied", call. = FALSE)
    for (name in paths) {
      path <- x[[name]][i]
      if (name == "report_path" || nzchar(path)) {
        x[[name]][i] <- ror_file(path, name)
        hashes[[name]][i] <- digest::digest(file = x[[name]][i], algo = "sha256")
      }
    }
    state <- x$extraction_status[i]
    path <- x$extraction_evidence_path[i]
    if (state == "not_requested" && nzchar(path))
      stop("not_requested cannot have extraction_evidence_path", call. = FALSE)
    if (state %in% c("supplied", "empty")) {
      if (!nzchar(path)) stop(state, " requires extraction_evidence_path", call. = FALSE)
      size <- file.info(path)$size
      if (state == "supplied" && size == 0) stop("supplied evidence must be nonempty", call. = FALSE)
      if (state == "empty" && size != 0) stop("empty evidence must have zero length", call. = FALSE)
    }
  }
  # Private-parent scope: exclusive creation, not protection against concurrent mutation.
  link <- Sys.readlink(output_dir)
  if (file.exists(output_dir) || (!is.na(link) && nzchar(link)))
    stop("output_dir already exists; never reuse it", call. = FALSE)
  parent <- dirname(output_dir)
  if (!dir.exists(parent)) stop("output_dir parent must already exist", call. = FALSE)
  output_dir <- file.path(normalizePath(parent, mustWork = TRUE), basename(output_dir))
  if (!dir.create(output_dir, recursive = FALSE, showWarnings = FALSE))
    stop("Cannot create output_dir: ", output_dir, call. = FALSE)
  findings <- data.frame(id = character(), checklist_item = character(), domain = character(),
                         severity = character(), owner = character(), finding = character(),
                         evidence_path = character(), resolution = character(), verified_by = character(),
                         status = character(), stringsAsFactors = FALSE)
  checklist <- data.frame(item = character(), evaluated_by = character(), outcome = character(),
                          finding = character(), report_id = character(), evaluation_status = character(),
                          match = logical(), stringsAsFactors = FALSE)
  text <- c("# Owner report handoff", "", "OWNER EVALUATION REQUIRED", "",
            paste0("Pending owner evaluations: ", nrow(x)),
            paste0("Caller-declared extraction gaps: ", sum(x$extraction_status %in% c("failed", "empty"))),
            "All reports: evaluation_status=not_evaluated; match=NA. Open major findings remain.",
            "Referenced evidence is listed and hashed, never executed or scientifically certified.", "")
  for (i in seq_len(nrow(x))) {
    id <- sprintf("F-%03d", nrow(findings) + 1L)
    findings[nrow(findings) + 1L, ] <- list(id, "rendered-report", "reporting", "major", x$owner[i],
      paste("OWNER EVALUATION REQUIRED:", x$report_id[i]), x$report_path[i],
      "open (see record Comments)", "open (see record Comments)", "open")
    checklist[nrow(checklist) + 1L, ] <- list("rendered-report", "owner required", "not_evaluated",
                                            id, x$report_id[i], "not_evaluated", NA)
    text <- c(text, paste0("## Report: ", ror_markdown(x$report_id[i])),
              paste0("Analysis: ", ror_markdown(x$analysis_id[i]), "; stage: ", ror_markdown(x$stage[i]),
                     "; owner: ", ror_markdown(x$owner[i])),
              paste0("Owner evaluation required; finding ", id, "; evaluation_status=not_evaluated; match=NA."),
              paste0("Extraction status (caller-declared): ", x$extraction_status[i]))
    for (name in paths) {
      value <- if (nzchar(x[[name]][i])) paste0(ror_markdown(x[[name]][i]), " ; SHA-256 ", hashes[[name]][i]) else "not supplied"
      text <- c(text, paste0(gsub("_", " ", name, fixed = TRUE), ": ", value))
    }
    if (!nzchar(x$comments_record_path[i]))
      text <- c(text, "Existing reporting/review record is not yet identified. Root must place the later disposition in that record's Comments when available; no gate or Comments record is created here.")
    if (!nzchar(x$expectations_path[i]) || !nzchar(x$evidence_path[i]))
      text <- c(text, "Review limitation: expectations or structured evidence not supplied. Name actual evidence used and unresolved gaps in the human disposition.")
    if (x$extraction_status[i] %in% c("failed", "empty")) {
      extraction_id <- sprintf("F-%03d", nrow(findings) + 1L)
      findings[nrow(findings) + 1L, ] <- list(extraction_id, "extraction-evidence", "reporting", "major", x$owner[i],
        paste("Caller-declared extraction", x$extraction_status[i], "for", x$report_id[i]),
        x$extraction_evidence_path[i], "open (see record Comments)", "open (see record Comments)", "open")
      text <- c(text, paste0("Open major extraction finding: ", extraction_id, ". Manual review may proceed; extraction does not confer acceptance."))
    }
    text <- c(text, "")
  }
  text <- c(text, "## Owner reading and disposition", "",
    "Inspect expected sections, archived Comments/notes and their order; missing text, serialized structures or leaked markup; statements and numerics against structured evidence; figures, captions, labels and legends against sources/counterparts; pagination, clipping, blank/missing pages, symbols/fonts and cross-references.",
    "Include report and counterpart digests in the response. Changed bytes require fresh review. Supplied extraction evidence never resolves owner evaluation.", "",
    "Record this template in the existing record Comments outside generated tables:",
    "Owner identity and date: ...", "Report ID/path/SHA-256 and counterpart identity/digest where relevant: ...",
    "Actual pages/sections/checks inspected: ...", "Evidence paths used: ...", "Limitations and omissions: ...",
    "Finding IDs: ...", "Disposition (accepted/adverse/pending) and reasons: ...",
    "Owning stage/role and verification evidence for resolved items: ...", "",
    "Wrong numbers, broken traceability or reproduction disagreement are critical findings routed to the owning scientific/reporting stage, with rollback/re-entry proposal if a gated artifact is invalidated. Resolve every major/critical finding under the existing process before SME signature.",
    "Owner acceptance does not replace personal SME review, independent reproduction, structured/non-PDF checks or gate signature. Completeness of personal review is behavioral. These pending snapshots are not updated by parsing Comments.")
  tryCatch({
    write.csv(findings, file.path(output_dir, "findings-log.csv"), row.names = FALSE, na = "NA")
    write.csv(checklist, file.path(output_dir, "checklist.csv"), row.names = FALSE, na = "NA")
    writeLines(text, file.path(output_dir, "handoff.md"), useBytes = TRUE)
  }, warning = function(w) stop("Incomplete retained directory: ", output_dir, "; ", conditionMessage(w), call. = FALSE),
     error = function(e) stop("Incomplete retained directory: ", output_dir, "; ", conditionMessage(e), call. = FALSE))
  invisible(list(output_dir = output_dir, handoff = file.path(output_dir, "handoff.md"),
                 findings = file.path(output_dir, "findings-log.csv"), checklist = file.path(output_dir, "checklist.csv"),
                 pending = nrow(x), findings_count = nrow(findings)))
}

ror_cli <- function(args = commandArgs(trailingOnly = TRUE)) {
  usage <- "Usage: Rscript --vanilla tools/report_owner_review.R --artifacts ABS --output-dir ABS"
  if (length(args) != 4L || !identical(sort(args[c(1L, 3L)]), sort(c("--artifacts", "--output-dir"))))
    stop(usage, call. = FALSE)
  values <- setNames(args[c(2L, 4L)], args[c(1L, 3L)])
  result <- report_owner_review(values[["--artifacts"]], values[["--output-dir"]])
  cat("OWNER_REVIEW_CREATED ", result$output_dir, "\n", sep = "")
  invisible(result)
}

if (sys.nframe() == 0L) {
  tryCatch(ror_cli(), error = function(e) {
    message(conditionMessage(e))
    quit(status = 1L)
  })
}
