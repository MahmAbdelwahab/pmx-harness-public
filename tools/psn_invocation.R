# Shared operational primitives for PsN references. Scientific gates,
# arguments, prechecks, and result parsing remain in each reference.

psn_invocation_reserve <- function(workdir, tool) {
  if (!dir.exists(workdir))
    stop("cannot reserve invocation: workdir does not exist: ", workdir, call. = FALSE)
  attempts <- 0L
  timestamp <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
  repeat {
    name <- basename(tempfile(pattern = sprintf("%s_run_%s_", tool, timestamp),
                              tmpdir = workdir))
    marker <- file.path(workdir, paste0(".", name, ".invocation.yaml"))
    attempts <- attempts + 1L
    connection <- suppressWarnings(try(file(marker, open = "wx"), silent = TRUE))
    if (!inherits(connection, "try-error")) break
    if (!file.exists(marker) || attempts >= 100L)
      stop("cannot reserve invocation marker: ", marker, "; ",
           as.character(connection), call. = FALSE)
  }
  writeLines(c(paste0("invocation_id: ", name), paste0("tool: ", tool),
               paste0("started_utc: ", timestamp), paste0("run_dir: ", name),
               "state: started"), connection)
  close(connection)
  list(name = name, path = file.path(workdir, name), marker = marker,
       tool = tool, since = file.info(marker)$mtime)
}

psn_invocation_find <- function(run, pattern, regex = FALSE, required = TRUE) {
  if (!dir.exists(run$path)) {
    if (required)
      stop("PsN run directory ", run$path, " was not created; the invocation ",
           "left no results in its own directory.", call. = FALSE)
    return(NA_character_)
  }
  hits <- list.files(run$path, recursive = TRUE, full.names = TRUE)
  hits <- hits[if (regex) grepl(pattern, basename(hits)) else basename(hits) == pattern]
  fresh <- hits[file.info(hits)$mtime >= run$since]
  if (!length(fresh)) {
    if (required)
      stop("PsN invocation ", run$name, " wrote no ", pattern,
           " newer than its start marker; results elsewhere under the ",
           "workdir belong to another invocation and are not read.", call. = FALSE)
    return(NA_character_)
  }
  if (length(fresh) > 1L)
    stop("PsN invocation ", run$name, " holds ", length(fresh), " copies of ",
         pattern, "; the selected artifact must be unique.", call. = FALSE)
  fresh[[1L]]
}

psn_invocation_finalize <- function(run, execution, outputs) {
  snapshots <- list.files(file.path(dirname(run$marker), "engine-config-used"),
                          full.names = TRUE)
  snapshots <- snapshots[grepl(run$name, basename(snapshots), fixed = TRUE)]
  if (length(snapshots) > 1L)
    stop("PsN ", run$tool, " invocation ", run$name, " produced ",
         length(snapshots), " matching engine-config snapshots; at most one ",
         "invocation-bound snapshot is allowed.", call. = FALSE)
  output_paths <- unlist(outputs, use.names = TRUE)
  output_paths <- output_paths[!is.na(output_paths) & nzchar(output_paths)]
  lines <- c(paste0("invocation_id: ", run$name), paste0("tool: ", run$tool),
             paste0("started_utc: ", format(run$since, "%Y-%m-%dT%H:%M:%SZ",
                                             tz = "UTC")),
             paste0("run_dir: ", encodeString(
               normalizePath(run$path, mustWork = TRUE), quote = '"')),
             "state: completed", "argv:",
             paste0("  - ", encodeString(execution$argv, quote = '"')),
             paste0("config_snapshot: ", if (!length(snapshots))
               "not-emitted-by-custom-executor" else encodeString(
                 normalizePath(snapshots[[1L]], mustWork = TRUE), quote = '"')),
             "outputs:")
  if (length(output_paths)) {
    lines <- c(lines, paste0("  ", names(output_paths), ": ",
                             encodeString(normalizePath(output_paths, mustWork = TRUE),
                                          quote = '"')))
  } else lines <- c(lines, "  none: true")
  temporary <- paste0(run$marker, ".tmp")
  writeLines(lines, temporary)
  if (!file.rename(temporary, run$marker))
    stop("could not finalize PsN invocation manifest: ", run$marker, call. = FALSE)
  run$marker
}
