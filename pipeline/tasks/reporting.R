# Render declared report sources from prior, gate-reviewed stage packages.
local({
  ti <- task_input
  refuse <- function(field, message) stop("reporting: ", field, ": ", message, call. = FALSE)
  report_dir <- ti$report_dir
  if (!is.character(report_dir) || length(report_dir) != 1L || is.na(report_dir) ||
      !nzchar(report_dir) || !dir.exists(report_dir))
    refuse("report_dir", "must name a project directory containing mar.qmd and deck.qmd")
  if (!rv_inside_reproduction() &&
      (normalizePath(report_dir) == normalizePath("templates/report", mustWork = FALSE) ||
       startsWith(normalizePath(report_dir), paste0(normalizePath("templates/report", mustWork = FALSE), "/"))))
    refuse("report_dir", "copy the shipped templates to a project directory before rendering")
  allowed <- c("report_dir", "store", "document", "packages", "helpers", "number_census")
  for (field in setdiff(names(ti), allowed)) refuse(field, "unsupported field")
  if (!is.character(ti$store) || length(ti$store) != 1L || is.na(ti$store) || !nzchar(ti$store))
    refuse("store", "must name the analysis store")
  doc <- ti$document
  if (!is.character(doc) || !identical(sort(names(doc)), c("author", "date", "version")) ||
      anyNA(doc) || !all(nzchar(doc)) || !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", doc[["date"]]) ||
      is.na(as.Date(doc[["date"]], format = "%Y-%m-%d")))
    refuse("document", "must specify author, version and a valid YYYY-MM-DD date")
  stages <- ti$packages
  if (!is.character(stages) || !length(stages) || anyNA(stages) || anyDuplicated(stages) ||
      !all(stages %in% upstream_tasks))
    refuse("packages", "must name one or more unique preceding tasks")

  mar <- file.path(report_dir, "mar.qmd")
  deck <- file.path(report_dir, "deck.qmd")
  sections <- file.path(report_dir, "sections", c(
    "01-executive-summary.qmd", "02-introduction.qmd", "03-objectives.qmd",
    "04-data-and-methods.qmd", "05-results.qmd", "06-discussion.qmd",
    "07-conclusions.qmd", "08-appendices.qmd"))
  helpers <- ti$helpers
  if (!is.null(helpers)) {
    if (!is.character(helpers) || anyNA(helpers) || anyDuplicated(helpers) ||
        any(!nzchar(helpers) | grepl("(^|/)[.][.](/|$)|^[/~]|^[A-Za-z]:", helpers)))
      refuse("helpers", "must be unique paths relative to report_dir")
    if (length(helpers)) {
      helpers <- file.path(report_dir, helpers)
      task_input_file("reporting", "helpers", helpers)
    }
  }
  task_input_file("reporting", "report_dir", c(mar, deck, sections))
  shared <- c("tools/evidence_notes.R", "tools/report_typography.R")
  sources <- unique(c(mar, deck, sections, helpers, shared))
  package_names <- paste0(stages, "_package")
  promotion <- as.call(c(list(quote(list)), stats::setNames(lapply(package_names, as.name), stages)))
  promoted <- data.frame(target = "report_stage_packages", source = package_names,
                         selection = "Stage package", stringsAsFactors = FALSE)
  params <- bquote(list(analysis_id = .(analysis_id), store = .(ti$store), date = .(doc[["date"]])))

  optional <- list()
  if (!is.null(ti$number_census)) {
    census <- report_number_census_prepare(ti$number_census, report_dir, sources)
    optional <- list(targets::tar_target_raw("report_number_census",
      report_number_census_command(report_dir, census, "report_stage_packages", doc)))
  }
  census_tables <- if (length(optional)) quote(list(
    "Number census of the report sources" = report_number_census$summary,
    "Declared typed numbers" = report_number_census$declared)) else quote(list())

  task_targets <<- c(list(
    targets::tar_target_raw("report_source_files", call("identity", sources), format = "file"),
    targets::tar_target_raw("report_stage_packages", promotion),
    targets::tar_target_raw("report_code_inventory", quote(data.frame(
      file = report_source_files, bytes = as.integer(file.size(report_source_files)),
      md5 = unname(tools::md5sum(report_source_files)), stringsAsFactors = FALSE))),
    tarchetypes::tar_quarto_raw("report_mar", path = mar, working_directory = ".",
      extra_files = c(sections, helpers, shared), execute_params = params),
    tarchetypes::tar_quarto_raw("report_deck", path = deck, working_directory = ".",
      extra_files = c(helpers, shared), execute_params = params),
    targets::tar_target_raw("report_semantic_check", quote({
      report_deck
      report_source_files
      notes <- unlist(lapply(report_stage_packages, function(pkg) pkg$notes), recursive = FALSE)
      if (!length(notes)) stop("reporting: the selected packages carry no review notes.", call. = FALSE)
      expected <- expected_report_notes(list(
        "E. Recorded review notes" = list(notes = notes, transform = protect_dollars)))
      assert_report_semantics(report_mar[grepl("mar[.]pdf$", report_mar)], expected)
    })),
    targets::tar_target_raw("report_semantic_manifest",
      quote(report_semantic_manifest_file(report_semantic_check)), format = "file")
  ), optional, list(targets::tar_target_raw("reporting_package", bquote({
    rendered <- c(report_mar[basename(report_mar) == "mar.pdf"],
                  report_deck[basename(report_deck) == "deck.pptx"])
    stopifnot(identical(basename(rendered), c("mar.pdf", "deck.pptx")))
    paths <- c(rendered, report_source_files, report_semantic_manifest)
    list(analysis = .(analysis_id), task = "reporting",
         artifacts = data.frame(artifact = basename(paths), path = paths,
                                source = c("PDF render", "Deck render", rep("Report source",
                                  length(report_source_files)), "Semantic check manifest")),
         tables = c(list("Promoted values and their sources" = .(promoted),
                        "Code inventory (gate traceability; not rendered in the MAR)" = report_code_inventory),
                    .(census_tables)),
         notes = list(evidence_note("Rendered-report semantic check", report_semantic_check$summary),
                      evidence_note("Archived notes matched", report_semantic_check$n_notes)))
  }))))
})
