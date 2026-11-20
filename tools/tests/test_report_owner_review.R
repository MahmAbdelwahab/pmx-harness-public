# Run only as the Rscript entry point; recursive sourcing defines nothing.
if (base::sys.nframe() == 0L && base::local({
  fa <- base::grep("^--file=", base::commandArgs(FALSE), value = TRUE)
  base::length(fa) > 0L && base::identical(
    base::basename(base::sub("^--file=", "", fa[[1L]])), "test_report_owner_review.R")
})) base::local({
# Synthetic public-interface proof. Run from the project root.
check <- function(ok, label) {
  if (!isTRUE(ok)) stop(paste("FAIL", label), call. = FALSE)
  cat("OK", label, "\n")
}
helper <- "tools/report_owner_review.R"
check(file.exists(helper), "missing-feature-report-owner-review")
source(helper, local = TRUE)
check(is.function(report_owner_review), "public-callable")
check(requireNamespace("digest", quietly = TRUE), "existing-digest-dependency")
root <- tempfile("report-owner-review-")
check(!file.exists(root), "fresh-synthetic-root")
dir.create(root)
put <- function(name, text) {
  p <- file.path(root, name)
  writeLines(text, p, useBytes = TRUE)
  p
}
report <- put("opaque space'\"$`;*?[].pdf", "opaque synthetic report bytes")
pair <- put("counterpart.txt", "synthetic counterpart")
evidence <- put("evidence.txt", "caller supplied extraction")
empty <- file.path(root, "empty.txt")
file.create(empty)
inventory <- function(name, x) {
  p <- file.path(root, paste0(name, ".csv"))
  write.csv(x, p, row.names = FALSE, na = "")
  p
}
minimal <- data.frame(report_id = "synthetic", report_path = report)
inv <- inventory("minimal", minimal)
out <- function(name) file.path(root, name)
read_output <- function(p) {
  check(identical(sort(list.files(p)), sort(c("handoff.md", "findings-log.csv", "checklist.csv"))), "exact-three-outputs")
  list(text = paste(readLines(file.path(p, "handoff.md")), collapse = "\n"),
       findings = read.csv(file.path(p, "findings-log.csv"), stringsAsFactors = FALSE),
       checklist = read.csv(file.path(p, "checklist.csv"), stringsAsFactors = FALSE))
}
pending <- function(x, count) {
  check(grepl("OWNER EVALUATION REQUIRED", x$text, fixed = TRUE) &&
        grepl(paste0("Pending owner evaluations: ", count), x$text, fixed = TRUE), "visible-pending-count")
  check(!grepl("\\bPASS\\b", x$text), "no-overall-pass")
  check(all(x$findings$severity == "major") && all(x$findings$status == "open"), "open-major-findings")
  check(all(x$checklist$evaluation_status == "not_evaluated") &&
        all(is.na(x$checklist$match)) && all(x$checklist$finding %in% x$findings$id), "pending-checklist-linkage")
}
before <- vapply(c(inv, report, pair, evidence, empty), digest::digest, "", file = TRUE, algo = "sha256")
old_path <- Sys.getenv("PATH")
tryCatch({
  Sys.setenv(PATH = "")
  check(all(Sys.which(c("pdftotext", "pdftoppm")) == ""), "pdf-tools-unavailable-after-startup")
  report_owner_review(inv, out("minimal-output"))
  check(identical(before, vapply(c(inv, report, pair, evidence, empty), digest::digest, "", file = TRUE, algo = "sha256")), "quoted-filename-empty-path-inputs-unchanged")
}, finally = Sys.setenv(PATH = old_path))
x <- read_output(out("minimal-output"))
pending(x, 1)
check(all(vapply(c("unspecified", "not supplied", "not yet identified"), grepl, logical(1), x = x$text, fixed = TRUE)), "omitted-context-visible")
check(grepl(digest::digest(file = report, algo = "sha256"), x$text, fixed = TRUE), "independent-report-digest")
bad <- function(name, data, pattern, destination = out(paste0(name, "-output"))) {
  p <- inventory(name, data)
  message <- tryCatch({report_owner_review(p, destination); "NO ERROR"}, error = conditionMessage)
  check(grepl(pattern, message, ignore.case = TRUE), name)
  check(!file.exists(file.path(destination, "handoff.md")), paste0(name, "-no-handoff"))
}
# Root prepares exactly one private FIFO before the suite; never open its content.
fifo <- file.path(getwd(), "synthetic-input.fifo")
rejected <- tryCatch({report_owner_review(fifo, out("fifo-inventory-output")); "NO ERROR"},
                     error = conditionMessage)
check(grepl("regular file", rejected, fixed = TRUE), "fifo-inventory-rejected")
check(!file.exists(out("fifo-inventory-output")) &&
      !file.exists(file.path(out("fifo-inventory-output"), "handoff.md")), "fifo-inventory-no-output-or-handoff")
bad("fifo-report", transform(minimal, report_path = fifo), "regular file")
check(!file.exists(out("fifo-report-output")), "fifo-report-no-output")
bad("fifo-context", transform(minimal, evidence_path = fifo), "regular file")
check(!file.exists(out("fifo-context-output")), "fifo-context-no-output")
# Metadata rejection only: neither read nor hash this device.
bad("device-report", transform(minimal, report_path = "/dev/null"), "regular file")
check(!file.exists(out("device-report-output")), "device-report-no-output")
full <- minimal
full$counterpart_required <- "TRUE"
full$counterpart_path <- pair
full$expectations_path <- evidence
full$evidence_path <- evidence
full$comments_record_path <- evidence
full$extraction_status <- "supplied"
full$extraction_evidence_path <- evidence
report_owner_review(inventory("full", full), out("full-output"))
pending(read_output(out("full-output")), 1)
bad("required-counterpart", transform(minimal, counterpart_required = "TRUE"), "counterpart")
bad("missing-report", transform(minimal, report_path = out("absent.pdf")), "report_path")
bad("duplicate-id", rbind(minimal, minimal), "unique")
bad("unknown-header", transform(minimal, unknown = "x"), "header")
dup <- minimal
names(dup) <- c("report_id", "report_id")
bad("duplicate-header", dup, "header")
bad("malformed-flag", transform(minimal, counterpart_required = "maybe"), "counterpart_required")
bad("missing-context", transform(minimal, evidence_path = out("absent")), "evidence_path")
bad("nonregular-context", transform(minimal, evidence_path = root), "regular")
bad("nonregular-report", transform(minimal, report_path = root), "regular")
bad("relative-report", transform(minimal, report_path = "relative.pdf"), "absolute")
bad("supplied-empty", transform(minimal, extraction_status = "supplied", extraction_evidence_path = empty), "nonempty")
bad("contradictory-extraction", transform(minimal, extraction_evidence_path = evidence), "not_requested")
for (state in c("failed", "empty")) {
  d <- transform(minimal, extraction_status = state,
                 extraction_evidence_path = if (state == "empty") empty else "")
  report_owner_review(inventory(state, d), out(paste0(state, "-output")))
  z <- read_output(out(paste0(state, "-output")))
  pending(z, 1)
  check(nrow(z$findings) == 2L && grepl(state, z$text, fixed = TRUE), paste0(state, "-distinct-major"))
}
sentinel_dir <- out("existing")
dir.create(sentinel_dir)
sentinel <- file.path(sentinel_dir, "sentinel")
writeLines("preserve", sentinel)
bad("existing-destination", minimal, "exists", sentinel_dir)
check(identical(readLines(sentinel), "preserve"), "sentinel-preserved")
bad("input-output-collision", minimal, "exists", report)
check(identical(before, vapply(c(inv, report, pair, evidence, empty), digest::digest, "", file = TRUE, algo = "sha256")), "all-inputs-unchanged")
writeLines("changed synthetic report bytes", report)
report_owner_review(inv, out("changed-output"))
changed <- read_output(out("changed-output"))
check(grepl(digest::digest(file = report, algo = "sha256"), changed$text, fixed = TRUE) &&
      !identical(before[[2]], digest::digest(file = report, algo = "sha256")), "changed-report-digest")
# Standard permissions, no fault-injection framework. Root must run unprivileged.
locked <- out("locked-parent")
dir.create(locked)
Sys.chmod(locked, "0555")
tryCatch(bad("unwritable-destination", minimal, "(create|incomplete|writ)", file.path(locked, "new")),
         finally = Sys.chmod(locked, "0755"))
# Exactly two normal-startup child CLI commands; no utilities or services.
rscript <- file.path(R.home("bin"), "Rscript")
cli_log <- out("cli-success.log")
status <- system2(rscript, c("--vanilla", shQuote(normalizePath(helper)), "--artifacts", shQuote(inv), "--output-dir", shQuote(out("cli-output"))), stdout = cli_log, stderr = cli_log, timeout = 15)
check(identical(status, 0L), "documented-cli-success")
pending(read_output(out("cli-output")), 1)
check(any(grepl("OWNER_REVIEW_CREATED", readLines(cli_log), fixed = TRUE)), "completion-receipt")
invalid_log <- out("cli-invalid.log")
status <- suppressWarnings(system2(rscript, c("--vanilla", shQuote(normalizePath(helper)), "--unknown"), stdout = invalid_log, stderr = invalid_log, timeout = 15))
check(status != 0L && !any(grepl("OWNER_REVIEW_CREATED", readLines(invalid_log), fixed = TRUE)), "invalid-cli-nonzero-no-completion")
for (p in c("roles/reviewer.md", ".claude/agents/pmx-reviewer.md")) {
  text <- paste(readLines(p), collapse = "\n")
  check(all(vapply(c("report_owner_review.R", "--artifacts", "--output-dir", "handoff.md", "findings-log.csv", "checklist.csv", "Comments", "SME", "legacy", "read-only", "MistralOcrClient.process_pdf"), grepl, logical(1), x = text, fixed = TRUE)), "documented-owner-workflow")
}
check(identical(digest::digest(file = "skills/review-checklist/SKILL.md", algo = "sha256"),
                "0145983f803f7964b0f2e85cffe61c5d0c7de41bbd4eec1e5ee642a0eb4a7640"),
      "exact-public-checklist-bytes")
# Local synthetic writer warning; exercise the actual sourced CLI without a child.
# This simulates partial output, not an operating-system disk or close failure.
warning_env <- new.env(parent = globalenv())
sys.source(helper, envir = warning_env)
warning_env$writeLines <- function(text, con, useBytes = FALSE) {
  base::writeLines("incomplete synthetic handoff", con, useBytes = useBytes)
  warning("synthetic writer warning", call. = FALSE)
}
warning_destination <- out("writer-warning-output")
warning_result <- NULL
warning_stdout <- capture.output({
  warning_error <- tryCatch({
    warning_result <- suppressWarnings(warning_env$ror_cli(
      c("--artifacts", inv, "--output-dir", warning_destination)))
    "NO ERROR"
  }, error = conditionMessage)
})
check(grepl("Incomplete retained directory: ", warning_error, fixed = TRUE) &&
      grepl(warning_destination, warning_error, fixed = TRUE) &&
      grepl("synthetic writer warning", warning_error, fixed = TRUE), "writer-warning-incomplete-error")
check(dir.exists(warning_destination) &&
      identical(readLines(file.path(warning_destination, "handoff.md")),
                "incomplete synthetic handoff"), "writer-warning-retains-partial-directory")
check(is.null(warning_result), "writer-warning-no-success-return")
check(!any(grepl("OWNER_REVIEW_CREATED", warning_stdout, fixed = TRUE)),
      "writer-warning-no-completion-receipt")
cat("OWNER_REVIEW_TESTS_COMPLETE\n")
})
