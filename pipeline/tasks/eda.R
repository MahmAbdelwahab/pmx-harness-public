# Task module: EDA
# Authored by the exploratory-analyst sub-agent, consuming
# skills/eda-standards/. Reviewed by the SME as a diff at the eda gate.
#
# Contract: define `task_targets`; never define gate targets. Available from
# the composer: `task_input` (analysis-declared settings), `analysis_id`,
# `upstream_gate` (name of the previous task's gate, or NULL).
#
# THIS MODULE COMPUTES NOTHING (round-6 findings 2 and 12; round-7 finding 1)
# Every number it carries to the gate is produced by
# skills/eda-standards/reference/eda.R under that skill's method manifest,
# called through the public EDA application or a declared local helper. There
# is no record filter, no count, no comparison against a threshold and no
# rounding below: the module wires targets, splices analysis declarations
# into commands, and assembles the package from what the reference returned.
# The counts the package quotes (rows read, subjects, deliverables produced
# or inapplicable, figures rendered) are read off reference results
# (reference_eda_view, reference_eda_core_status), not taken with nrow() or
# sum() here. Two assertions remain, both integrity checks over hashes: the
# cached input hashes must equal the live file-target hashes before
# packaging, and — when the specification is a semantic overlay — the base
# specification on disk must still carry the sha256 the overlay records.
#
# What this module wires together, in dependency order:
#   1. eda_external_dataset_file / eda_external_spec_file /
#      eda_base_spec_file — the external dataset, its yspec specification
#                         and (when the specification is an OVERLAY, round-7
#                         finding 8) the base specification it is bound to,
#                         all as format = "file" targets.
#   2. eda_spec         — a plain-list projection of the yspec specification
#                         (columns, short labels, units, declared values and
#                         decodes, ranges, the SETUP__ flags that carry column
#                         roles, and the overlay provenance).
#   3. eda_input        — the analysis dataset, either the in-analysis
#                         `dataset` target or the external file read and
#                         spec-checked by tools/external.R.
#   4. eda_columns      — the resolved column ROLES (required roles stop when
#                         unannotated; optional roles — categorical
#                         covariates, study, dose group — resolve to nothing
#                         and make their deliverables INAPPLICABLE with the
#                         reason, never silently absent).
#   5. eda_role_map     — the resolved mapping as a table for the gate.
#   6. eda_view         — THE canonical EDA view: selected records with a
#                         stable row id, the row selectors (analysis /
#                         observations / doses / subjects / excluded), the
#                         record-selection rule as declared values, the
#                         exclusion reasons, one row-set digest per selector,
#                         and the method parameters applied.
#   7. eda_code_file    — OPTIONAL: the analysis's EDA helper file(s).
#   8. eda_reference_file / eda_dependency_file — the eda-standards reference
#                         and its manifest dependency tools/record_selection.R
#                         as file targets, so editing either invalidates the
#                         results (the manifest digest check then hard-stops).
#   9. eda_tables       — the standard summary tables (CSV and .tex through
#                         pmtables), the categorical summary or its
#                         inapplicable state, the table-deliverable status and
#                         the method parameters.
#  10. eda_figures      — every active registry entry built by the reference
#                         from its DECLARATION, written, and checked from the
#                         built object and the file: rendered rows,
#                         inapplicable entries with reasons, display-transform
#                         provenance, figure parameters, layer digests, the
#                         ggplot_build reconciliation and the statistics drawn.
#  11. eda_provenance   — where the dataset came from and how big it is.
#  12. eda_figure_index — every registry entry with tier, kind, enabled,
#                         status (rendered / inapplicable / disabled), reason,
#                         declared transforms and row set, and the assertion
#                         that each rendered figure's row-set digest, file
#                         name and dimensions equal the declaration.
#  13. eda_log_audit    — one log-axis audit per log axis the registry
#                         declares, on that entry's row selector and axis
#                         column, classifying dropped values by row id with
#                         BLQ status.
#  14. eda_core_status  — every core deliverable (tables and figures) with
#                         its state; an inapplicable state carries its reason
#                         or the target stops.
#  15. eda_method_parameters — every method parameter the reference applied.
#  16. eda_package      — the gate-package payload rendered into
#                         gates/<ID>/eda.md by tools/gate_package.R.

local({
  ti <- task_input

  external <- !is.null(ti$external_input)
  overlay <- NULL

  if (external) {
    task_input_file("eda", "external_input", ti$external_input, single = TRUE)
    # A yspec specification is REQUIRED with an external dataset (SME ruling,
    # 2026-09-02); refused HERE, at composition, so the analysis stops before
    # a store is written.
    spec <- ti$external_spec
    if (is.null(spec) || !length(spec) || !is.character(spec) ||
        is.na(spec[[1]]) || !nzchar(spec[[1]])) {
      stop("eda: external_input declared without a yspec specification. ",
           "Declare inputs$eda$external_spec = '<path>.yml' (the spec the ",
           "dataset is checked against — the assembly spec, or a semantic ",
           "overlay on it) or add a data_assembly task to this analysis.",
           call. = FALSE)
    }
    if (length(spec) != 1L) {
      stop("eda: inputs$eda$external_spec must name exactly one yspec file; ",
           length(spec), " were declared (", paste(spec, collapse = ", "),
           "). One dataset is checked against one spec.", call. = FALSE)
    }
    # SEMANTIC OVERLAY (round-7 finding 8). A specification that declares
    # `# overlay-of:` / `# overlay-of-sha256:` in its header is not the
    # assembly spec; it is bound to one. The base spec on disk must still have
    # the recorded bytes, or composition stops: an overlay of a spec that has
    # changed no longer describes the dataset's assembly.
    task_input_file("eda", "external_spec", spec, single = TRUE)
    overlay <- ys_overlay_check(spec)
  }

  catalog <- ti$catalog %||% character(0)
  if (!is.character(catalog)) {
    stop("eda: inputs$eda$catalog must be a character vector of plot-registry ",
         "entry names (the catalog-tier plots this analysis enables).",
         call. = FALSE)
  }

  code_files <- ti$code
  if (!is.null(code_files) &&
      (!is.character(code_files) || !length(code_files) ||
       any(is.na(code_files)) || any(!nzchar(code_files)))) {
    stop("eda: inputs$eda$code, when declared, must be a character vector of ",
         "existing file paths (the analysis's EDA helper code).", call. = FALSE)
  }
  if (!is.null(code_files)) task_input_file("eda", "code", code_files)
  registry <- ti$registry
  if (!is.null(registry) && (!is.list(registry) || !length(registry) ||
      is.null(names(registry)) || anyNA(names(registry)) ||
      any(!nzchar(names(registry))) || anyDuplicated(names(registry)) ||
      !is.null(code_files))) {
    stop("eda: inputs$eda$registry must be a named plot declaration list; ",
         "use it with the public EDA application or declare local code.",
         call. = FALSE)
  }

  out_dir <- ti$out_dir
  if (!is.null(out_dir) &&
      (!is.character(out_dir) || length(out_dir) != 1L || is.na(out_dir) ||
       !nzchar(out_dir))) {
    stop("eda: inputs$eda$out_dir, when declared, must be a single directory ",
         "path (where the EDA tables and figures are written).", call. = FALSE)
  }

  # The eda-standards reference implementation and its manifest dependency.
  # The paths are FIXED, not declarable (review finding M4): the approved
  # method is the one the manifest names, and nothing else.
  eda_reference  <- file.path("skills", "eda-standards", "reference", "eda.R")
  eda_dependency <- file.path("tools", "record_selection.R")
  if (!is.null(ti$reference)) {
    stop("eda: inputs$eda$reference is not accepted. The eda-standards ",
         "reference implementation is the file skills/eda-standards/",
         "MANIFEST.yaml names; an analysis cannot redirect it. Remove the ",
         "declaration.", call. = FALSE)
  }

  origin <- if (external) {
    paste0("external input declared in analyses/", analysis_id,
           ".R (inputs$eda$external_input), read and spec-checked by ",
           "tools/external.R read_external_dataset()")
  } else {
    "in-analysis target `dataset`, produced by the data_assembly task of this pipeline"
  }
  spec_kind <- if (is.null(overlay)) {
    if (external) "the specification declared for the dataset (no overlay header: taken as the assembly spec)"
    else "the data_assembly task's own specification"
  } else {
    paste0("a SEMANTIC OVERLAY (EDA role flags and decodes) on the base spec ",
           overlay$base_spec, ", bound to its sha256 ", overlay$base_spec_sha256)
  }

  gate_dep <- if (is.null(upstream_gate)) NULL else as.name(upstream_gate)
  gated <- function(expr) as.call(c(quote(`{`), gate_dep, list(expr)))

  if (is.null(code_files)) {
    registry_expr <- if (is.null(registry)) quote(eda_application_registry()) else
      bquote(identity(.(registry)))
    public_out_dir <- out_dir %||% file.path("outputs", analysis_id, "eda")
    columns_expr <- bquote(eda_application_columns(eda_spec, eda_registry, .(catalog)))
    view_expr <- quote(eda_application_view(eda_application_reference(eda_reference_file),
                                            eda_input, eda_spec, eda_columns))
    tables_expr <- bquote(eda_application_tables(
      eda_application_reference(eda_reference_file), eda_view, eda_columns, eda_spec,
      .(public_out_dir)))
    figures_expr <- bquote(eda_application_figures(
      eda_application_reference(eda_reference_file), eda_view, eda_columns, eda_spec,
      eda_registry, .(catalog), .(public_out_dir)))
    audit_expr <- bquote(eda_application_log_audits(
      eda_application_reference(eda_reference_file), eda_view, eda_columns, eda_spec,
      eda_registry, .(catalog)))
    parameters_expr <- quote(eda_application_method_parameters(eda_view, eda_tables, eda_figures))
  } else {
    registry_expr <- quote({ eda_code_file; eda_plot_registry() })
    columns_expr <- bquote({ eda_code_file; eda_resolve_columns(eda_spec, enabled = .(catalog)) })
    view_expr <- quote({ eda_code_file; make_eda_view(eda_input, eda_spec, eda_columns) })
    local_output <- if (is.null(out_dir)) list() else list(out_dir = out_dir)
    tables_expr <- as.call(c(as.list(quote(make_eda_tables(eda_view, eda_columns, eda_spec))),
                             local_output))
    figures_expr <- as.call(c(as.list(bquote(make_eda_figures(
      eda_view, eda_columns, eda_spec, enabled_catalog = .(catalog)))), local_output))
    tables_expr <- bquote({ eda_code_file; .(tables_expr) })
    figures_expr <- bquote({ eda_code_file; .(figures_expr) })
    audit_expr <- bquote({
      eda_code_file
      make_eda_log_audits(eda_view, eda_columns, eda_spec, enabled_catalog = .(catalog))
    })
    parameters_expr <- quote({
      eda_code_file
      make_eda_method_parameters(eda_view, eda_tables, eda_figures)
    })
  }

  dataset_expr <- if (external) {
    quote(read_external_dataset(eda_external_dataset_file,
                                eda_external_spec_file))
  } else {
    quote(dataset)
  }

  spec_projection_expr <- if (external) {
    if (is.null(overlay)) quote(ys_projection(eda_external_spec_file))
    else quote({ eda_base_spec_file; ys_projection(eda_external_spec_file) })
  } else {
    quote(ys_projection(da_spec_file))
  }

  # Input-hash binding, asserted before the package is assembled (round-6
  # finding 3; overlay binding round-7 finding 8).
  hash_binding_expr <- if (external) {
    base_check <- if (is.null(overlay)) quote(NULL) else quote({
      live_base <- paste0("sha256:", digest::digest(file = eda_base_spec_file, algo = "sha256"))
      if (!identical(eda_spec$overlay$base_spec_sha256, live_base))
        drift <- c(drift, sprintf(
          "base specification %s: the overlay records %s, file on disk %s",
          eda_base_spec_file, eda_spec$overlay$base_spec_sha256, live_base))
      base_rows <- data.frame(
        item = "base specification sha256 (overlay header and file target agree)",
        value = live_base, stringsAsFactors = FALSE)
    })
    bquote({
      live_ds   <- digest::digest(file = eda_external_dataset_file, algo = "sha256")
      live_spec <- digest::digest(file = eda_external_spec_file, algo = "sha256")
      cached_ds   <- attr(eda_input, "external_source_sha256")
      cached_spec <- attr(eda_input, "external_spec_sha256")
      drift <- character(0)
      base_rows <- NULL
      if (!identical(cached_ds, live_ds))
        drift <- c(drift, sprintf(
          "dataset %s: cached sha256 %s, file on disk %s",
          eda_external_dataset_file, cached_ds, live_ds))
      if (!identical(cached_spec, live_spec))
        drift <- c(drift, sprintf(
          "specification %s: cached sha256 %s, file on disk %s",
          eda_external_spec_file, cached_spec, live_spec))
      if (!identical(eda_spec$spec_sha256, live_spec))
        drift <- c(drift, sprintf(
          "specification projection: recorded sha256 %s, file on disk %s",
          eda_spec$spec_sha256, live_spec))
      .(base_check)
      if (length(drift))
        stop("eda_package: the cached EDA input does not match the files the ",
             "pipeline tracks:\n  - ", paste(drift, collapse = "\n  - "),
             "\nA package built now would bind current input hashes beside ",
             "derived results computed from different bytes. Re-run the EDA ",
             "stage.", call. = FALSE)
      rbind(data.frame(
        item = c("external dataset sha256 (file target and cached object agree)",
                 "yspec specification sha256 (file target, cached object and projection agree)"),
        value = c(live_ds, live_spec),
        stringsAsFactors = FALSE), base_rows)
    })
  } else {
    quote(data.frame(
      item = "input hash binding",
      value = paste0("not applicable: the dataset is the in-analysis ",
                     "`dataset` target, bound by the data-assembly gate"),
      stringsAsFactors = FALSE))
  }

  input_artifacts_expr <- if (external) {
    spec_artifact <- if (is.null(overlay))
      "yspec specification the dataset was checked against"
    else "yspec specification the dataset was checked against: a SEMANTIC OVERLAY (EDA roles and decodes) on the base spec below, not the assembly spec"
    base_artifact <- if (is.null(overlay)) quote(NULL) else quote(data.frame(
      artifact = "base yspec specification the dataset was ASSEMBLED against (the overlay's header records its sha256)",
      path = eda_base_spec_file,
      source = "declared in the overlay header (# overlay-of); file target eda_base_spec_file; sha256 checked at composition and before packaging",
      stringsAsFactors = FALSE))
    bquote(rbind(data.frame(
      artifact = c("external analysis dataset as read (checked against its spec at read time)",
                   .(spec_artifact)),
      path = c(eda_external_dataset_file, eda_external_spec_file),
      source = c("declared inputs$eda$external_input; file target eda_external_dataset_file, read by target eda_input",
                 "declared inputs$eda$external_spec; file target eda_external_spec_file, ys_check run inside target eda_input"),
      stringsAsFactors = FALSE), .(base_artifact)))
  } else {
    quote(NULL)
  }

  code_artifacts_expr <- if (is.null(code_files)) {
    quote(NULL)
  } else {
    quote(data.frame(
      artifact = paste0("EDA helper code: ", basename(eda_code_file)),
      path = eda_code_file,
      source = "declared inputs$eda$code; sourced by the analysis script",
      stringsAsFactors = FALSE))
  }

  provenance_notes_expr <- if (external) {
    bquote(list(
      evidence_note(
        "External dataset read for this EDA, as recorded on the data object itself",
        eda_provenance$ids[["dataset"]]),
      evidence_note(
        "sha256 of that dataset file, taken when it was read",
        eda_provenance$ids[["dataset_sha256"]]),
      evidence_note(
        "yspec specification the dataset was checked against, as recorded on the data object",
        eda_provenance$ids[["spec"]]),
      evidence_note(
        "sha256 of that specification file, taken when the dataset was read",
        eda_provenance$ids[["spec_sha256"]]),
      authored_note(.(paste0(
        "Dataset provenance: the dataset was supplied from outside this analysis and was checked against the yspec specification above by yspec::ys_check() at read time, inside tools/external.R read_external_dataset(). That specification is ",
        spec_kind,
        ". A dataset that does not conform stops the eda_input target, so the tables and figures in this package exist only for a spec-conformant dataset. The dataset, the specification",
        if (is.null(overlay)) "" else " and the base specification",
        " are format = \"file\" targets of this pipeline, so a byte change at the same path invalidates the read and everything downstream of it; the input-hash binding table above is the assertion, made before this package was assembled, that the hashes cached on the data object still equal the hashes of the files the pipeline tracks",
        if (is.null(overlay)) "." else ", and that the base specification on disk still has the sha256 the overlay header records (also checked when the pipeline was composed).")))))
  } else {
    quote(list(
      authored_note("Dataset provenance: the dataset is the in-analysis `dataset` target produced by the data_assembly task of this same pipeline. Its source, assembly code, specification, definition file and QC battery are the artifacts of the data-assembly gate record, and the composed gate edge means this EDA task ran only after that gate was signed; they are not restated here. The external-source rows of the provenance table are therefore empty by construction.")))
  }

  task_targets <<- list(

    # 1. External inputs as FILE targets.
    if (!external) NULL else
      targets::tar_target_raw("eda_external_dataset_file",
                              bquote(identity(.(ti$external_input))),
                              format = "file"),
    if (!external) NULL else
      targets::tar_target_raw("eda_external_spec_file",
                              bquote(identity(.(ti$external_spec[[1]]))),
                              format = "file"),
    if (is.null(overlay)) NULL else
      targets::tar_target_raw("eda_base_spec_file",
                              bquote(identity(.(overlay$base_spec))),
                              format = "file"),

    # The reference implementation and its dependency as hashed dependencies.
    targets::tar_target_raw("eda_reference_file",
                            bquote(identity(.(eda_reference))), format = "file"),
    targets::tar_target_raw("eda_dependency_file",
                            bquote(identity(.(eda_dependency))), format = "file"),
    targets::tar_target_raw("eda_application_file",
                            quote(identity("tools/eda_application.R")), format = "file"),

    # 2. The specification projection: metadata only.
    targets::tar_target_raw("eda_spec", gated(spec_projection_expr)),

    # 3. The dataset.
    targets::tar_target_raw("eda_input", dataset_expr),
    targets::tar_target_raw("eda_registry", gated(registry_expr)),

    # 4. Column roles resolved from the specification.
    targets::tar_target_raw(
      "eda_columns",
      gated(columns_expr)),

    # 5. The resolved mapping as a reviewable table.
    targets::tar_target_raw(
      "eda_role_map",
      gated(quote(ys_role_table(eda_spec, eda_columns)))),

    # 6. THE canonical EDA view.
    targets::tar_target_raw(
      "eda_view",
      gated(bquote({
        eda_reference_file; eda_dependency_file; eda_application_file
        .(view_expr)
      }))),

    if (is.null(code_files)) NULL else
      targets::tar_target_raw("eda_code_file",
                              bquote(identity(.(code_files))), format = "file"),

    # 9. Summary tables (CSV + .tex), status and parameters.
    targets::tar_target_raw("eda_tables", gated(tables_expr)),

    # 10. Figures built from their declarations, written and checked.
    targets::tar_target_raw("eda_figures", gated(figures_expr)),

    # 11. Provenance and size, read off the object and the view.
    targets::tar_target_raw(
      "eda_provenance",
      gated(bquote({
        d <- eda_input
        att <- function(nm) {
          v <- attr(d, nm)
          if (is.null(v) || !length(v) || is.na(v[[1]]) ||
              !nzchar(as.character(v[[1]]))) NA_character_
          else as.character(v[[1]])
        }
        ids <- c(dataset        = att("external_source"),
                 dataset_sha256 = att("external_source_sha256"),
                 spec           = att("external_spec"),
                 spec_sha256    = att("external_spec_sha256"))
        ov <- eda_spec$overlay
        source_tab <- data.frame(
          item = c("dataset origin (composed declaration)",
                   "external dataset path recorded on the data",
                   "sha256 of that dataset file at read time",
                   "yspec specification recorded on the data",
                   "sha256 of that specification file at read time",
                   "what that specification is",
                   "base specification the overlay is bound to",
                   "base specification sha256 recorded in the overlay header"),
          value = c(.(origin), unname(ids), .(spec_kind),
                    if (is.null(ov)) NA_character_ else ov$base_spec,
                    if (is.null(ov)) NA_character_ else ov$base_spec_sha256),
          stringsAsFactors = FALSE)
        list(source = source_tab, counts = eda_view$input_counts, ids = ids)
      }))),

    # 12. The plot registry as a table, with the row-set, file and dimension
    # assertions and the enforced inapplicable state.
    targets::tar_target_raw(
      "eda_figure_index",
      gated(bquote({
        ref <- eda_application_reference(eda_reference_file)
        ref$reference_eda_registry_index(
          registry_table  = ref$reference_eda_registry_check(eda_registry),
          enabled_catalog = .(catalog),
          rendered        = eda_figures$rendered,
          view_digests    = eda_view$digests,
          inapplicable    = eda_figures$inapplicable)
      }))),

    # 13. Log-axis audits, one per declared log axis, from the SAME view.
    targets::tar_target_raw(
      "eda_log_audit",
      gated(audit_expr)),

    # 14. Core-deliverable status (tables and figures), enforced.
    targets::tar_target_raw(
      "eda_core_status",
      gated(quote({
        ref <- eda_application_reference(eda_reference_file)
        ref$reference_eda_core_status(figures = eda_figure_index, tables = eda_tables$status)
      }))),

    # 15. Every method parameter the reference applied.
    targets::tar_target_raw(
      "eda_method_parameters",
      gated(parameters_expr)),

    # 16. Gate-package payload.
    targets::tar_target_raw(
      "eda_package",
      gated(bquote({
        binding <- .(hash_binding_expr)
        selector <- data.frame(
          item = names(unlist(eda_view$selector)),
          value = unname(unlist(eda_view$selector)),
          stringsAsFactors = FALSE)
        digests <- data.frame(
          view = names(eda_view$digests),
          rows = unname(eda_view$n_rows[names(eda_view$digests)]),
          digest = unname(eda_view$digests),
          stringsAsFactors = FALSE)
        tables <- list(
          "Dataset provenance" = eda_provenance$source,
          "Input hash binding, asserted before packaging" = binding,
          "Record counts, dataset as read" = eda_provenance$counts,
          "Column roles resolved from the yspec specification" = eda_role_map,
          "EDA record-selection rule (the row-selector identity)" = selector,
          "EDA views and their row-set digests" = digests,
          "Exclusion reasons found (records excluded, never dropped)" = eda_view$exclusions,
          "Core deliverables and their status (inapplicable states carry the specification's reason)" =
            eda_core_status$table,
          "Data inventory (records not excluded; by declared grouping when the specification flags one)" =
            eda_tables$inventory,
          "Continuous covariate summary (one row per subject)" = eda_tables$cont_summary,
          "Categorical covariate summary (one row per subject) or its inapplicable state" =
            eda_tables$cat_summary_display,
          "Method parameters applied by the reference (skills/eda-standards)" =
            eda_method_parameters,
          "Plot registry: tier, kind, status, declared transform and row set, file, dimensions" =
            eda_figure_index,
          "Figure notes (partially applicable declarations)" = eda_figures$notes,
          "Display transforms: declared parameters and computed values" =
            eda_figures$derived,
          "Figure layers: view drawn, rows and digest (union over pages)" =
            eda_figures$layers,
          "Figure reconciliation against ggplot_build(): rows and non-finite positions per layer" =
            eda_figures$reconciliation,
          "Log-axis audit per declared log axis (skills/eda-standards)" =
            eda_log_audit$table,
          "Log-axis audit: dropped values by row id with BLQ status" =
            eda_log_audit$detail)
        tables <- c(tables, Filter(Negate(is.null), eda_tables$grouped_inventory))
        artifacts <- rbind(
          .(input_artifacts_expr),
          data.frame(
            artifact = c("eda-standards reference implementation (the approved method these numbers came from)",
                         "record-selection classifier the reference depends on (manifest depends:)",
                         "public EDA application"),
            path = c(eda_reference_file, eda_dependency_file, eda_application_file),
            source = c("skills/eda-standards/reference/eda.R, gated by skills/eda-standards/MANIFEST.yaml",
                       "tools/record_selection.R, digest recorded under every unit's depends: in the manifest",
                       "tools/eda_application.R, file target eda_application_file"),
            stringsAsFactors = FALSE),
          data.frame(
            artifact = paste0("EDA summary table: ", basename(eda_tables$files)),
            path = eda_tables$files,
            source = "target eda_tables (reference_eda_inventory, reference_eda_continuous_summary, reference_eda_categorical_summary; .tex rendered by pmtables::stable from the same values)",
            stringsAsFactors = FALSE),
          data.frame(
            artifact = if (nrow(eda_figures$rendered)) paste0("figure: ", eda_figures$rendered$plot)
                       else character(),
            path = eda_figures$rendered$path,
            source = if (nrow(eda_figures$rendered)) paste0("target eda_figures (plot-registry entry ",
                            eda_figures$rendered$plot,
                            ", built by reference_eda_build_figure, checked by reference_eda_figure_check)")
                     else character(),
            stringsAsFactors = FALSE),
          .(code_artifacts_expr))
        notes <- c(
          .(provenance_notes_expr),
          list(
            authored_note("Method authorization (house rule: the LLM proposes, deterministic tools compute): every count, summary statistic, covariate bin, smooth, histogram bin, correlation, box statistic, dose marker and log-axis census in this package is produced by skills/eda-standards/reference/eda.R, whose intended-use units are gated by skills/eda-standards/MANIFEST.yaml. The reference and its declared dependency tools/record_selection.R are listed as artifacts above and are file dependencies of this pipeline, so an edit to either invalidates these results and the manifest digest check refuses to run until the SME re-confirms it. The plot registry carries no R function: each figure is a declaration the reference builds. This task module and the analysis helper contain declarations and calls only."),
            evidence_note("Rows in the dataset as read",
                          with(eda_provenance$counts, value[id == "rows"])),
            evidence_note("Subjects (unique ID) in the dataset as read",
                          with(eda_provenance$counts, value[id == "subjects"])),
            evidence_note("Column roles the yspec specification resolved to",
                          paste0(eda_role_map$role, " = ", eda_role_map$column)),
            authored_note("Column semantics (skills/eda-standards): the table above is the mapping the EDA actually used. Which column is the subject identifier, the exclusion flag, the event identifier, the time axis, the observed value, the BLQ flag, the continuous and categorical covariates, the study and the dose group is read from the specification's SETUP__ flags; the axis labels on every figure are built from each column's short label and unit; which event value means an observation or a dose, and which BLQ value means below LLOQ, come from those columns' decode annotations, and the event values are asserted against the NONMEM convention (observation = 0, dose = 1) so the QC battery, this EDA and the fitting adapter select the same records. A required role the specification does not annotate stops the task; an optional role it does not annotate (categorical covariates, study, dose group) makes the deliverables that need it INAPPLICABLE, recorded in the status table above with the flag the specification would need."),
            evidence_note("Subjects in the EDA views (records not excluded)",
                          eda_tables$inventory$n_subjects),
            evidence_note("Observation records in the EDA views",
                          eda_tables$inventory$n_obs),
            evidence_note("Records excluded by the exclusion flag and therefore outside the EDA views",
                          eda_tables$inventory$n_excluded),
            evidence_note("Observations flagged BLQ in the EDA views",
                          eda_tables$inventory$n_blq),
            evidence_note("Row-set digest of the observation view every figure and the audit were computed on",
                          eda_view$digests[["observations"]]),
            authored_note("Row-set identity (skills/eda-standards): the tables, every figure and every log-axis audit are computed from ONE canonical view object, whose record-selection rule is stated verbatim in the table above. Each figure is checked from the BUILT PLOT OBJECT: every layer's data frame carries the row id, the union of the row ids over all pages of a layer must digest to the view the layer declares (a dose-marker layer to the doses view, the profile layers to the observations view), pages must partition the view, the axis scales must carry exactly the declared transform, and ggplot_build() of every page must carry exactly the frame's rows and exactly the non-finite positions the reference predicts. The registry index refuses to build unless every active entry is rendered exactly once or recorded as inapplicable with a reason, and unless the rendered file name, page size and row-set digest equal the declaration."),
            authored_note("Record selection (skills/eda-standards): a record is kept when its exclusion flag is missing, empty, whitespace-only, or the NONMEM missing token \".\" (compared after trimming); any other value is an exclusion reason. The rule is defined once, in tools/record_selection.R, and applied by the EDA reference, the QC battery and the fitting adapter, so the three describe, check and fit the same record set. The exclusion reasons found are tabulated above; nothing is dropped. A view with no kept records stops the task rather than producing an empty EDA."),
            authored_note("Exclusion and outlier policy (roles/exploratory-analyst, skills/eda-standards): nothing is dropped by this task. Records already flagged for exclusion upstream are excluded from the EDA views by the record-selection rule above and are counted in the evidence note above; any outlier candidate seen in the figures is flagged for the SME in the agent summary of this record, never removed from the dataset."),
            evidence_note("Core deliverables (tables and figures) in this analysis",
                          with(eda_core_status$counts, value[id == "n_core"])),
            evidence_note("Core deliverables produced (computed or rendered)",
                          with(eda_core_status$counts, value[id == "n_core_produced"])),
            evidence_note("Core deliverables INAPPLICABLE for this dataset, each with the specification's reason in the status table (a documented state, not a deviation)",
                          with(eda_core_status$counts, value[id == "n_core_inapplicable"])),
            evidence_note("Inapplicable deliverables and their reasons",
                          with(eda_core_status$table,
                               paste0(deliverable[status == "inapplicable"], ": ",
                                      reason[status == "inapplicable"]))),
            authored_note("Inapplicable core deliverables (skills/eda-standards, round-7 finding 7): a core deliverable that this dataset cannot support — the categorical-covariate summary and boxplots when the specification flags no categorical covariate, the inventory grouping by study or dose group when it flags neither — is recorded as inapplicable with that reason, both in the status table above and in the registry index. The code paths exist and run whenever the specification flags the role; the state is enforced by the reference, which refuses a core entry that is neither produced nor recorded with a reason. A dose-group colour on the concentration-time figures falls back to colour by subject and says so in the figure notes."),
            evidence_note("Log axes audited, one per axis a registry entry declares",
                          eda_log_audit$n_requests),
            evidence_note("Rows a log axis drops, summed over the audited axes",
                          eda_log_audit$total_dropped),
            evidence_note("Plot-registry entries that DECLARE a log axis, to which that audit applies",
                          unique(eda_log_audit$table$plot)),
            authored_note("Log-axis audit (skills/eda-standards, 2026-08-28; round-7 finding 9): a log scale removes missing, zero, negative and non-finite values with a warning that never reaches this package, and the figure still looks normal without them. The table above is that census run ONCE PER LOG AXIS THE REGISTRY DECLARES, on that entry's own row selector and axis column, with the dropped values classified and listed by row id with their BLQ status. The same classification predicts the non-finite positions ggplot_build() must carry for every layer of every rendered figure, and the reconciliation table above shows that prediction against the built object; a mismatch stops the figure. A non-positive value with no matching exclusion-flag or BLQ accounting is a data question for the assembly stage, not a plotting detail."),
            authored_note("Figure construction (skills/eda-standards, round-7 findings 1 and 6): every displayed statistic — the LOESS smooth on the linear concentration-time figure (declared span and degree, evaluated at every observation row), the histogram bins and counts, the pairs-plot correlations, the box quartiles and whiskers, the dose-event markers, the quantile cut points of the stratified view — is computed by the reference from the registry declaration and drawn with identity geoms from frames that carry the row id. pmplots::pairs_plot and GGally are not used: they compute the displayed correlations and densities inside the plotting call, outside the approved method. The method-parameters table above lists every parameter each figure and table applied."),
            evidence_note("Figures rendered for this analysis",
                          with(eda_core_status$counts, value[id == "n_figures_rendered"])),
            evidence_note("Catalog-tier figures enabled for this analysis",
                          with(eda_core_status$counts, value[id == "n_catalog_enabled"])),
            authored_note("Plot tiers (skills/eda-standards): core entries are always rendered or recorded as inapplicable with a reason; catalog entries render only when the analysis declaration enables them, and enabling one mid-analysis is additive rather than a deviation. The registry table above lists EVERY entry with its tier, kind and status."),
            authored_note("Every number in the tables and evidence notes above is read from executed target output; nothing is transcribed by hand and nothing is computed in this module."),
            authored_note("Observations memo (roles/exploratory-analyst): the computed observations at this gate are the tables and evidence notes above. The reading of the FIGURES - profile shape, apparent nonlinearity, covariate correlation, outlier candidates - is written into the agent summary of this record after the figures for this run exist, separating observation from interpretation and labelling both; it is not authored in this module, because it is not a computed value and must not be pre-written.")))
        list(analysis = .(analysis_id),
             task = "eda",
             tables = tables,
             artifacts = artifacts,
             notes = notes)
      })))
  )

  task_targets <<- Filter(Negate(is.null), task_targets)
})
