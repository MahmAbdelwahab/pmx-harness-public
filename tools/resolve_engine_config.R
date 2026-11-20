#!/usr/bin/env Rscript

# Typed command boundary for NONMEM/PsN runner configuration. Output is a
# private fixed-file protocol; shell consumers never evaluate generated text.
.run_resolve_engine_config_cli <- function(argv = commandArgs(trailingOnly = TRUE)) {
file_arg <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1L])
tools_dir <- normalizePath(dirname(file_arg), mustWork = TRUE)
sys.source(file.path(tools_dir, "yaml_scalar.R"), envir = environment())
sys.source(file.path(tools_dir, "engine_config.R"), envir = environment())
separator <- match("--", argv)
if (is.na(separator) || separator == length(argv)) {
  stop("Usage: resolve_engine_config.R --workdir DIR --engine-config FILE ",
       "--harness FILE --handoff DIR --invocation ID ",
       "-- TOOL [ARG ...]", call. = FALSE)
}
options <- argv[seq_len(separator - 1L)]
command <- argv[(separator + 1L):length(argv)]
if (length(options) %% 2L != 0L) stop("Resolver options require values.", call. = FALSE)
values <- setNames(as.list(options[seq(2L, length(options), by = 2L)]),
                   options[seq(1L, length(options), by = 2L)])
allowed <- c("--workdir", "--engine-config", "--harness", "--handoff", "--invocation")
if (anyDuplicated(names(values)) || !setequal(names(values), allowed)) {
  stop("Resolver options must contain each supported key exactly once.", call. = FALSE)
}
handoff <- values[["--handoff"]]
handoff_info <- file.info(handoff)
if (!dir.exists(handoff) || nzchar(Sys.readlink(handoff)) ||
    !identical(as.character(handoff_info$mode), "700") ||
    !identical(handoff_info$uname, unname(Sys.info()[["user"]])) ||
    length(list.files(handoff, all.files = TRUE, no.. = TRUE)) != 0L) {
  stop("Handoff directory must be owned, mode 0700, non-symlink, and empty: ",
       handoff, call. = FALSE)
}

resolved <- resolve_dispatcher_config(
  workdir = values[["--workdir"]],
  engine_config = if (nzchar(values[["--engine-config"]]))
    values[["--engine-config"]] else NULL,
  harness = if (nzchar(values[["--harness"]])) values[["--harness"]] else NULL,
  invocation_id = values[["--invocation"]], tool = command[[1L]],
  args = command[-1L])

context <- c(
  "env-file" = resolved$env_file$value,
  "image" = resolved$image$value,
  "image-source" = resolved$image$source,
  "license" = resolved$license$value,
  "license-source" = resolved$license$source,
  "provision" = resolved$provision$value,
  "schema-version" = as.character(resolved$schema_version))
published <- file.path(handoff, names(context))
handoff_complete <- FALSE
on.exit(if (!handoff_complete) unlink(published, force = TRUE), add = TRUE)
for (name in names(context)) {
  writeLines(context[[name]], file.path(handoff, name), useBytes = TRUE)
}
Sys.chmod(published, "0600")

if (!is.null(resolved$config)) {
  snapshot_dir <- file.path(resolved$workdir, "engine-config-used")
  dir.create(snapshot_dir, recursive = TRUE, showWarnings = FALSE)
  timestamp <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
  safe_tool <- gsub("[^A-Za-z0-9._-]", "_", resolved$tool)
  safe_invocation <- gsub("[^A-Za-z0-9._-]", "_", resolved$invocation_id)
  snapshot_tmp <- tempfile(paste0(".", timestamp, "-", safe_tool, "-",
                                  safe_invocation, "."), tmpdir = snapshot_dir)
  on.exit(unlink(snapshot_tmp, force = TRUE), add = TRUE)
  engine <- if (startsWith(resolved$tool, "nmfe"))
    paste0("nonmem (direct nmfe: ", resolved$tool, ")") else
    paste0("nonmem (PsN ", resolved$tool, ")")
  display_argv <- paste(c(resolved$tool, resolved$args), collapse = " ")
  argv_items <- paste0("# argv_item: ",
                       vapply(c(resolved$tool, resolved$args), encodeString,
                              character(1L), quote = '"'))
  compute <- resolved$compute
  metadata <- c(
    "# Engine-config snapshot (written by tools/resolve_engine_config.R)",
    paste0("# engine: ", engine), paste0("# invoked_at_utc: ", timestamp),
    paste0("# invocation_id: ", resolved$invocation_id),
    paste0("# provision: ", resolved$provision$value),
    paste0("# provision_source: ", resolved$provision$source))
  if (nzchar(resolved$provision$displaced)) {
    metadata <- c(metadata,
      paste0("# provision_override: acknowledged env override ",
             "(NONMEM_PROVISION_OVERRIDE_ACK=1) displaced reviewed-config ",
             "value: ", resolved$provision$displaced))
  }
  metadata <- c(
    metadata, paste0("# argv: ", display_argv), argv_items,
    if (nzchar(compute$note)) paste0("# compute_note: ", compute$note),
    paste0("# compute_applied: threads=", compute$threads,
           " nodes=", compute$nodes,
           " parafile=", if (nzchar(compute$parafile)) compute$parafile else "none",
           " submit=", if (nzchar(compute$submit)) compute$submit else "local",
           " queue=", if (nzchar(compute$queue)) compute$queue else "none",
           " account=", if (nzchar(compute$account)) compute$account else "none"),
    paste0("# compute_argv:", if (length(compute$argv))
      paste0(" ", paste(compute$argv, collapse = " ")) else " (none)"),
    paste0("# image: ", resolved$image$value, " (", resolved$image$source, ")"),
    paste0("# config_source: ", resolved$config$source),
    paste0("# config_sha256: ", resolved$config$sha256), "#")
  connection <- file(snapshot_tmp, open = "wb")
  writeLines(metadata, connection, useBytes = TRUE)
  writeBin(resolved$config$bytes, connection)
  close(connection)
  Sys.chmod(snapshot_tmp, "0600")
  snapshot <- file.path(snapshot_dir,
                        paste0(timestamp, "-", safe_tool, "-", safe_invocation,
                               ".", basename(snapshot_tmp), ".yaml"))
  if (!file.rename(snapshot_tmp, snapshot)) stop("Unable to publish engine snapshot.", call. = FALSE)
}
handoff_complete <- TRUE
invisible(NULL)
}

resolver_file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(resolver_file_arg) > 0L &&
    grepl("resolve_engine_config\\.R$",
          sub("^--file=", "", resolver_file_arg[[1L]]))) {
  tryCatch(.run_resolve_engine_config_cli(), error = function(e) {
    message("Error: ", conditionMessage(e))
    status <- if (grepl("captured .* configuration changed", conditionMessage(e))) 4L
      else if (grepl("NONMEM_PROVISION=.*differs from the reviewed", conditionMessage(e))) 3L
      else if (grepl("Unknown NONMEM provision mode|parafile named in the arguments",
                     conditionMessage(e))) 2L else 1L
    quit(status = status)
  })
}
rm(resolver_file_arg)
