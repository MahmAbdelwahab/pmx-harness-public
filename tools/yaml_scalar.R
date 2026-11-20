# Narrow reviewed-configuration reader shared by R and shell entry points.
# Uses the harness's existing yaml dependency, including its duplicate-key
# refusal, instead of reimplementing YAML quoting and comment rules in shell.

read_reviewed_yaml <- function(path) {
  if (!file.exists(path)) return(NULL)
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("R package 'yaml' is required to read reviewed configuration: ", path,
         call. = FALSE)
  }
  value <- tryCatch(
    yaml::read_yaml(path),
    error = function(e) stop("Invalid YAML configuration ", path, ": ",
                             conditionMessage(e), call. = FALSE))
  if (is.null(value)) return(stats::setNames(list(), character(0)))
  if (!is.list(value) || is.null(names(value))) {
    stop("Invalid YAML configuration ", path,
         ": document root must be a mapping.", call. = FALSE)
  }
  value
}

reviewed_yaml_scalar <- function(path, keys, expected = "character",
                                 required = FALSE, document = NULL) {
  expected <- match.arg(expected, c("character", "logical", "number", "scalar"))
  if (length(keys) == 1L && grepl(".", keys, fixed = TRUE)) {
    keys <- strsplit(keys, ".", fixed = TRUE)[[1L]]
  }
  if (!length(keys) || any(!nzchar(keys))) {
    stop("YAML scalar path must contain one or more non-empty keys.", call. = FALSE)
  }
  if (is.null(document)) document <- read_reviewed_yaml(path)
  label <- paste(keys, collapse = ".")
  value <- document
  for (key in keys) {
    if (is.null(value)) break
    if (!is.list(value) || is.null(names(value))) {
      stop("Invalid YAML configuration ", path, ": ", label,
           " traverses a non-mapping value.", call. = FALSE)
    }
    index <- match(key, names(value))
    if (is.na(index)) {
      value <- NULL
      break
    }
    value <- value[[index]]
  }
  if (is.null(value)) {
    if (isTRUE(required)) {
      stop("Invalid YAML configuration ", path, ": required key ", label,
           " is missing or null.", call. = FALSE)
    }
    return(NULL)
  }

  scalar <- length(value) == 1L && !is.list(value) && !is.object(value) &&
    !is.na(value)
  type_ok <- switch(expected,
                    character = is.character(value),
                    logical = is.logical(value),
                    number = is.integer(value) || is.double(value),
                    scalar = is.atomic(value))
  if (!scalar || !type_ok ||
      (is.character(value) && grepl("[\r\n]", value))) {
    type_name <- switch(expected, number = "numeric", expected)
    stop("Invalid YAML configuration ", path, ": ", label,
         " must be a ", type_name, " scalar.", call. = FALSE)
  }
  value
}

.yaml_scalar_file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(.yaml_scalar_file_arg) > 0L &&
    grepl("yaml_scalar\\.R$", sub("^--file=", "", .yaml_scalar_file_arg[1L]))) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 2L || length(args) > 4L) {
    stop("Usage: Rscript tools/yaml_scalar.R <file> <dot.path> [character|logical|number|scalar] [optional|required]",
         call. = FALSE)
  }
  mode <- if (length(args) >= 4L) match.arg(args[[4L]], c("optional", "required"))
          else "optional"
  value <- reviewed_yaml_scalar(args[[1L]], args[[2L]],
                                expected = if (length(args) >= 3L) args[[3L]] else "character",
                                required = identical(mode, "required"))
  if (!is.null(value)) {
    if (is.logical(value)) value <- if (value) "true" else "false"
    writeLines(as.character(value))
  }
}
rm(.yaml_scalar_file_arg)
