# Review the declared analysis, its signed stage gates and the reproduced report.
local({
  ti <- task_input
  refuse <- function(field, ...) stop("review: ", field, ": ", ..., call. = FALSE)
  fields <- c("assessment", "map", "out_dir", "store", "checkout", "checkout_permitted",
              "volatile", "expected_changes")
  for (field in setdiff(names(ti), fields)) refuse(field, "unsupported field")
  for (field in setdiff(fields, names(ti))) refuse(field, "required declaration field")
  for (field in c("assessment", "map", "checkout"))
    task_input_file("review", field, ti[[field]], single = TRUE)
  out_dir <- ti$out_dir
  store <- ti$store
  if (!is.character(out_dir) || length(out_dir) != 1L || is.na(out_dir) ||
      !nzchar(out_dir) || identical(out_dir, "."))
    refuse("out_dir", "must name a dedicated project output directory")
  if (!"reporting" %in% upstream_tasks)
    refuse("reporting", "must be a preceding task")
  for (field in c("checkout_permitted", "volatile", "expected_changes")) {
    value <- ti[[field]]
    if (!is.character(value) || anyNA(value) || any(!nzchar(value)) ||
        (length(value) && (is.null(names(value)) || anyNA(names(value)) ||
                           any(!nzchar(names(value))) || anyDuplicated(names(value)))))
      refuse(field, "must be a named character vector of nonempty reasons or character(0)")
  }
  for (key in names(ti$checkout_permitted)) {
    pattern <- sub("^(untracked|modified):re:", "", key)
    if (identical(key, pattern) || !startsWith(pattern, "^") ||
        !isTRUE(tryCatch({ grepl(pattern, "", perl = TRUE); TRUE },
                         error = function(e) FALSE, warning = function(w) FALSE)))
      refuse("checkout_permitted", "requires untracked:re: or modified:re: with an anchored pattern")
  }
  if (length(ti$expected_changes))
    task_input_file("review", "expected_changes", names(ti$expected_changes))
  rv_volatile_columns(ti$volatile)
  for (key in names(ti$volatile)) {
    if (startsWith(key, "re:") &&
        !isTRUE(tryCatch({ grepl(substring(key, 4L), "", perl = TRUE); TRUE },
                         error = function(e) FALSE, warning = function(w) FALSE)))
      refuse("volatile", "invalid Perl pattern in ", key)
    if (startsWith(key, "file:") && !rv_project_path(substring(key, 6L)))
      refuse("volatile", "invalid project path in ", key)
  }
  if (!rv_project_path(out_dir) || !rv_project_path(store))
    refuse("out_dir and store", "must be project-relative paths")

  decl_file <- file.path("analyses", paste0(analysis_id, ".R"))
  if (!rv_inside_reproduction()) {
    decl <- rv_read_declaration(decl_file)
    if (!identical(decl$analysis_id, analysis_id))
      refuse("analysis_id", "must match the declaration filename")
    if (!identical(store, decl$inputs$reporting$store))
      refuse("store", "must match the reporting store")
    locations <- rv_location_fields(decl$inputs)
    others <- c(locations$value[locations$path != "review/out_dir"], ti$map, ti$checkout,
                ti$assessment, "templates", "pipeline", "tools", "skills", "gates", "analyses")
    out <- sub("/+$", "", out_dir)
    for (path in sub("/+$", "", others))
      if (identical(path, out) || startsWith(path, paste0(out, "/")) ||
          startsWith(out, paste0(path, "/")))
        refuse("out_dir", "overlaps another declared location or harness input: ", path)
  }
  stages <- upstream_tasks
  records <- unname(vapply(stages, function(t) gate_file_for(analysis_id, t), ""))
  ledger <- file.path("gates", analysis_id, "approvals.jsonl")
  gate_dep <- if (is.null(upstream_gate)) NULL else as.name(upstream_gate)
  gated <- function(expr) as.call(c(quote(`{`), gate_dep, list(expr)))
  task_targets <<- list(
    targets::tar_target_raw("review_map_file", call("identity", ti$map), format = "file"),
    targets::tar_target_raw("review_assessment_file", call("identity", ti$assessment), format = "file"),
    targets::tar_target_raw("review_checkout_file", call("identity", ti$checkout), format = "file"),
    targets::tar_target_raw("review_gate_records", call("identity", records), format = "file"),
    targets::tar_target_raw("review_gate_integrity", bquote(
      rv_gate_integrity(stats::setNames(review_gate_records, .(stages)), .(ledger),
                        readLines(review_checkout_file, warn = FALSE), .(ti$checkout_permitted),
                        c(.(stages), "review"), names(task_registry()), .(analysis_id),
                        checkout_path = review_checkout_file))),
    targets::tar_target_raw("review_traceability", bquote({
      decl <- rv_read_declaration(.(decl_file))
      v <- rv_store_values(.(store))
      rp <- v[["reporting_package"]]
      man <- rp$artifacts$path[grepl("-semantic-manifest[.]txt$", rp$artifacts$path)]
      tr <- rv_promotion_check(rp$tables[["Promoted values and their sources"]], v,
                               readLines(review_map_file, warn = FALSE), decl$inputs$reporting,
                               if (length(man) == 1L && file.exists(man)) readLines(man, warn = FALSE) else character(0))
      inv <- rv_inventory_check(rp$tables[["Code inventory (gate traceability; not rendered in the MAR)"]],
                                .(ti$expected_changes))
      list(promotions = tr$promotions, numbers = tr$numbers, inventory = inv$table,
           outcomes = rbind(tr$outcomes, inv$outcomes))
    })),
    targets::tar_target_raw("review_validation", bquote({
      decl <- rv_read_declaration(.(decl_file))
      man <- rv_manifest(.(decl_file), wd = ".")
      v <- rv_store_values(.(store))
      reg <- task_registry()
      rv_validation_audit(man$manifest, decl$tasks, stats::setNames(review_gate_records, .(stages)), v,
                          unname(vapply(reg[intersect(decl$tasks, names(reg))], function(r) r$skill, "")))
    })),
    targets::tar_target_raw("review_crossdoc", bquote({
      decl <- rv_read_declaration(.(decl_file))
      art <- reporting_package$artifacts
      mar <- art$path[basename(art$path) == "mar.pdf"]
      manifest <- report_semantic_manifest
      mar_lines <- if (length(mar) == 1L && file.exists(mar))
        suppressWarnings(system2("pdftotext", c("-layout", shQuote(mar), "-"), stdout = TRUE)) else character(0)
      rv_crossdoc(readLines(review_assessment_file, warn = FALSE), mar_lines,
                  decl$inputs$reporting$document[["date"]], report_semantic_check$n_notes,
                  readLines(manifest, warn = FALSE),
                  c(assessment = review_assessment_file, mar = paste(mar, collapse = "; "),
                    manifest = manifest), section_prefix = "E. Recorded review notes")
    })),
    targets::tar_target_raw("review_reproduction", gated(bquote({
      review_checkout_file
      if (rv_inside_reproduction()) {
        rv_reproduction_stub()
      } else {
        od <- .(out_dir)
        s <- rv_reproduce_setup(.(analysis_id), live_root = normalizePath("."))
        run <- rv_reproduce_run(s)
        oc <- rv_run_outcome(run$exit_status, run$meta, s$expected_errors, s$edges, s$outside)
        dir.create(od, recursive = TRUE, showWarnings = FALSE)
        if (file.exists(run$log)) file.copy(run$log, file.path(od, "tar_make.log"), overwrite = TRUE)
        tsv <- function(x, f) utils::write.table(x, file.path(od, f), sep = "\t", row.names = FALSE,
                                                 quote = FALSE)
        tsv(s$tree_before, "tree-before.tsv")
        record <- rbind(s$record, oc$record, data.frame(
          quantity = c("start (UTC)", "end (UTC)"), value = c(run$start, run$end),
          stringsAsFactors = FALSE))
        utils::write.csv(record, file.path(od, "reproduction-record.csv"), row.names = FALSE)
        if (!oc$complete)
          stop("review: the fresh run is incomplete (exit status ", run$exit_status, "; expected ",
               "errors present with 'Gate not signed': ", all(s$expected_errors %in% oc$errored),
               "; ancestors neither built nor errored: ", length(oc$unbuilt_ancestors),
               "); a stop, not a finding (Decision 12). Log: ", file.path(od, "tar_make.log"),
               call. = FALSE)
        tree_after <- rv_tree_digest(s$examined, s$roots, s$exclude)
        tsv(tree_after, "tree-after.tsv")
        rv_tree_check(s$tree_before, tree_after, od, "live-tree", "the live analysis tree")
        copy_after <- rv_tree_digest(s$copy, s$roots, s$exclude)
        tsv(copy_after, "copy-tree-after.tsv")
        rv_tree_check(s$tree_before, copy_after, od, "copy-tree", "the copy's census roots")
        cmp <- rv_compare_stores(.(store), s$fresh_store, s$spec, s$volatile, s$expected_changes)
        locs <- unique(s$derived$locations$value[s$derived$locations$field != "store"])
        upstream <- rv_upstream_files(s$decl$inputs, locs)
        fc <- rv_compare_files(cmp$snapshots$analysis, cmp$snapshots$fresh,
                               c(examined = s$examined, copy = s$copy), locs, s$ws_rel, s$spec,
                               s$volatile, upstream, rf11 = character(0))
        # Render census (Decision 15): a copy of the fresh report directory,
        # the stream leg through quarto with the TeX capture shim, then the
        # log leg through tectonic, each spawned with the cd first.
        cdir <- file.path(s$ws, "census")
        for (d in c("report", "shim", "engine")) dir.create(file.path(cdir, d), recursive = TRUE, showWarnings = FALSE)
        src <- file.path(s$copy, s$report_dir)
        file.copy(list.files(src, full.names = TRUE, all.files = TRUE, no.. = TRUE),
                  file.path(cdir, "report"), recursive = TRUE, copy.date = TRUE)
        real <- unname(Sys.which("tectonic"))
        captured <- file.path(cdir, "shim", "captured.tex")
        writeLines(c("#!/bin/sh",
                     "# Written by the review census: keeps the TeX quarto pipes to the engine, then",
                     "# forwards it to the real engine unchanged.",
                     "if [ -t 0 ]; then", paste0("  exec ", shQuote(real), " \"$@\""), "else",
                     paste0("  cat > ", shQuote(captured)),
                     paste0("  exec ", shQuote(real), " \"$@\" < ", shQuote(captured)), "fi"),
                   file.path(cdir, "shim", "tectonic"))
        Sys.chmod(file.path(cdir, "shim", "tectonic"), "0755")
        suppressWarnings(system2("sh", c("-c", shQuote(rv_spawn_command(
          s$copy, "census", s$ws_rel, analysis_id = .(analysis_id), epoch = s$epoch,
          document_date = s$decl$inputs$reporting$document[["date"]])))))
        ex <- suppressWarnings(system2("sh", c("-c", shQuote(rv_spawn_command(s$copy, "tectonic", s$ws_rel)))))
        rd <- function(f) if (file.exists(f)) readLines(f, warn = FALSE) else character(0)
        stream <- rd(file.path(cdir, "stream.txt"))
        texlog <- rd(file.path(cdir, "engine", "texput.log"))
        census <- rv_render_census(stream, texlog, rd(captured), as.integer(ex),
                                   any(grepl("Output created", stream, fixed = TRUE)))
        writeLines(stream, file.path(od, "census-stream.txt"))
        writeLines(texlog, file.path(od, "census-texput.log"))
        att <- cmp$table$match[cmp$table$item == "environment_attestation: fingerprint"]
        record <- rbind(record, data.frame(quantity = c(
          "attestation fingerprint equal", "live tree unchanged", "copy unchanged",
          "comparison mismatches", "file comparison mismatches", "text rules held"),
          value = c(as.character(isTRUE(att)), "TRUE", "TRUE",
                    as.character(sum(cmp$table$match %in% FALSE)),
                    as.character(sum(fc$table$match %in% FALSE)),
                    as.character(is.null(fc$text) || all(fc$text$outcome %in% TRUE))),
          stringsAsFactors = FALSE))
        text <- if (is.null(fc$text)) data.frame(item = "none", rule = NA_character_, outcome = NA,
                                                 stringsAsFactors = FALSE) else fc$text
        utils::write.csv(record, file.path(od, "reproduction-record.csv"), row.names = FALSE)
        utils::write.csv(cmp$table, file.path(od, "reproduction-comparison.csv"), row.names = FALSE)
        utils::write.csv(cmp$excluded, file.path(od, "excluded-from-comparison.csv"), row.names = FALSE)
        utils::write.csv(fc$table, file.path(od, "file-comparison.csv"), row.names = FALSE)
        utils::write.csv(text, file.path(od, "text-comparison.csv"), row.names = FALSE)
        list(skipped = FALSE, record = record, comparison = cmp$table, excluded = cmp$excluded,
             files = fc$table, text = text, census = census$table,
             outcomes = rbind(oc$outcomes, cmp$outcomes, fc$outcomes, census$outcomes),
             workspace = s$ws)
      }
    }))),

    targets::tar_target_raw("review_findings", bquote({
      checks <- list(review_gate_integrity = review_gate_integrity,
                     review_reproduction = review_reproduction,
                     review_traceability = review_traceability,
                     review_validation = review_validation,
                     review_crossdoc = review_crossdoc)
      path <- file.path(.(out_dir), "findings-log.csv")
      findings <- rv_findings(do.call(rbind, lapply(checks, function(x) x$outcomes)),
                              rv_checklist(checks), path)
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      utils::write.csv(findings$log, path, row.names = FALSE)
      findings
    })),
    targets::tar_target_raw("review_package", bquote({
      r <- review_reproduction
      g <- review_gate_integrity
      paths <- c(review_checkout_file, review_assessment_file, review_map_file,
                 file.path(.(out_dir), c(if (!r$skipped) c(
                   "reproduction-record.csv", "reproduction-comparison.csv", "excluded-from-comparison.csv",
                   "file-comparison.csv", "text-comparison.csv", "census-stream.txt", "census-texput.log",
                   "tree-before.tsv", "tree-after.tsv", "copy-tree-after.tsv", "tar_make.log"),
                   "findings-log.csv")))
      list(analysis = .(analysis_id), task = "review",
           artifacts = data.frame(artifact = basename(paths), path = paths, source = "Review evidence"),
           tables = list("Findings" = review_findings$log, "Checklist" = review_findings$checklist,
                         "Checkout" = g$checkout, "Gate integrity" = g$records,
                         "Approvals ledger" = g$ledger, "Reproduction" = r$record,
                         "Store comparison" = r$comparison, "Artifact comparison" = r$files,
                         "Report text comparison" = r$text, "Render census" = r$census,
                         "Promotions" = review_traceability$promotions, "Number traceability" = review_traceability$numbers,
                         "Code inventory" = review_traceability$inventory,
                         "Validation status" = review_validation$table,
                         "Cross-document consistency" = review_crossdoc$table),
           notes = list(authored_note("Review the computed findings and the reading items before signing.")))
    }))
  )
})
