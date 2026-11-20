# Reads a dataset produced outside the current analysis (an earlier analysis,
# or an SME-supplied file). The path is recorded in the gate package so the
# provenance of an external input is explicit.
#
# MISSING-TOKEN NORMALISATION (Codex round 7, finding 2). The reader NA-codes
# the NONMEM missing token on read with the SHARED constant
# HARNESS_READ_NA_STRINGS from tools/record_selection.R, the same file that
# defines the kept-record rule every consumer applies. Blank cells in
# character columns still arrive as "" (read.csv never NA-codes them), which
# is why the kept-record rule accepts "" as well as NA — the rule is stated
# once, there, and never restated here. Stops when the file does not exist;
# returns the data.frame with an external_source attribute recording
# provenance for the gate package.
#
# `spec` is the yspec specification the dataset is CHECKED against (SME
# ruling, 2026-09-02: an external dataset reaching the EDA task without one is
# refused at composition — see pipeline/tasks/eda.R). It may be the assembly
# spec itself or a SEMANTIC OVERLAY on it (see ys_overlay_provenance() below);
# which one it is, is recorded by the caller, never assumed here. When given:
#   * the spec is loaded with yspec::ys_load() and the data frame is checked
#     against it with yspec::ys_check(). The check is COMPUTED here, so a
#     dataset that does not match its spec stops the target that reads it
#     rather than producing plots and tables over unverified columns;
#   * the yspec findings (the column and the reason it failed) are carried
#     into the error message, because ys_check reports them as messages that
#     would otherwise never reach the log the SME reads;
#   * two further attributes are attached next to external_source:
#     external_spec (the spec path) and external_spec_sha256 (the sha256 of
#     the spec file's bytes), so the gate package can state WHICH spec the
#     dataset was verified against and detect a spec that changed afterwards.
# `spec = NULL` keeps the original behaviour exactly: read, attach
# external_source, return. Other callers rely on it — pipeline/tasks/
# simulation.R reads the fitting dataset that way in three places.
if (!exists("HARNESS_READ_NA_STRINGS", mode = "character")) {
  # tools/record_selection.R is sourced alongside this file by
  # tar_source("tools"); when external.R is sourced on its own, load it from
  # the same directory so the constant is the shared one, never a local copy.
  .ext_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) NULL)
  .rs <- if (!is.null(.ext_dir) && file.exists(file.path(.ext_dir, "record_selection.R")))
    file.path(.ext_dir, "record_selection.R") else file.path("tools", "record_selection.R")
  if (file.exists(.rs)) source(.rs)
  rm(.ext_dir, .rs)
}
read_external_dataset <- function(path, spec = NULL) {
  if (!file.exists(path)) stop("External dataset not found: ", path, call. = FALSE)
  d <- utils::read.csv(path, na.strings = HARNESS_READ_NA_STRINGS,
                       stringsAsFactors = FALSE)
  attr(d, "external_source") <- path
  # The DATASET's own sha256, recorded next to its path (round-6 finding 3).
  # The spec hash was already recorded; the dataset hash was not, so a cached
  # data object could not be compared against the file it claimed to come
  # from. The consumer asserts the two agree before packaging.
  attr(d, "external_source_sha256") <- digest::digest(file = path, algo = "sha256")
  if (is.null(spec) || !nzchar(spec)) return(d)

  if (!file.exists(spec))
    stop("yspec specification not found: ", spec,
         " (declared as the spec for external dataset ", path, ")", call. = FALSE)
  ys <- yspec::ys_load(spec)

  # ys_check() reports each finding through message() and then stops with a
  # generic "Please review messages and re-check." Collect the messages so
  # the stop below names the actual defect.
  findings <- character(0)
  res <- withCallingHandlers(
    tryCatch(yspec::ys_check(d, ys), error = function(e) e),
    message = function(m) {
      findings <<- c(findings, sub("\n$", "", conditionMessage(m)))
      invokeRestart("muffleMessage")
    })
  failed <- inherits(res, "error") || !isTRUE(res)
  if (failed) {
    detail <- findings[nzchar(trimws(findings))]
    stop("External dataset ", path, " does not conform to its yspec ",
         "specification ", spec, ":\n  ",
         paste(if (length(detail)) detail else conditionMessage(res),
               collapse = "\n  "),
         call. = FALSE)
  }

  attr(d, "external_spec") <- spec
  attr(d, "external_spec_sha256") <- digest::digest(file = spec, algo = "sha256")
  d
}

# ---------------------------------------------------------------------------
# yspec metadata: a plain-list projection, and role resolution
# ---------------------------------------------------------------------------
# Round-6 finding 4: the EDA task claimed that units, labels, decodes and
# covariate roles came from the yspec specification, but ys_load()/ys_check()
# ran and the spec object was then discarded — axis labels and covariate
# column names were hard-coded in the analysis helper. They agreed with the
# pilot spec, but the agreement was neither derived nor checked.
#
# These helpers close that gap. They read metadata only: no subject-level
# value is touched, nothing is computed from data, so they are harness
# tooling rather than an analysis method. The functions that DO compute over
# records take the resolved column names as declared parameters
# (skills/eda-standards/reference/eda.R).
#
# A yspec object itself is not a good targets value (it carries environments
# and a package class whose serialisation is not part of yspec's contract),
# so ys_projection() returns a plain list: the columns, their short labels,
# units, types, declared values and decodes, ranges, the continuous/discrete
# split yspec derives, the primary key, and the SETUP__ `flags` blocks that
# carry column ROLES.

# Returns a plain list projection of a yspec specification file:
#   spec_path, spec_sha256, description
#   columns    character vector, in spec order
#   short      named character (column -> short label)
#   unit       named character (column -> unit, "" when the spec declares none)
#   type       named character
#   continuous, discrete   character vectors (yspec's own derivation)
#   values     named list (column -> declared values, absent when undeclared)
#   decode     named list (column -> declared decodes, parallel to values)
#   range      named list
#   primary_key character
#   flags      named list (flag name -> the columns carrying it)
ys_projection <- function(spec_path) {
  if (!file.exists(spec_path))
    stop("yspec specification not found: ", spec_path, call. = FALSE)
  ys <- yspec::ys_load(spec_path)
  meta <- yspec::get_meta(ys)
  cols <- names(ys)
  fld <- function(f, default = "") {
    v <- vapply(cols, function(c) {
      x <- ys[[c]][[f]]
      if (is.null(x) || !length(x)) default else as.character(x)[1]
    }, character(1))
    stats::setNames(v, cols)
  }
  lst <- function(f) {
    out <- lapply(cols, function(c) ys[[c]][[f]])
    names(out) <- cols
    out[!vapply(out, function(x) is.null(x) || !length(x), logical(1))]
  }
  lgl <- function(f) {
    keep <- vapply(cols, function(c) isTRUE(ys[[c]][[f]]), logical(1))
    cols[keep]
  }
  flags <- meta$flags
  if (is.null(flags)) flags <- list()

  list(spec_path   = spec_path,
       spec_sha256 = digest::digest(file = spec_path, algo = "sha256"),
       overlay     = ys_overlay_provenance(spec_path),
       description = if (is.null(meta$description)) "" else as.character(meta$description)[1],
       columns     = cols,
       short       = fld("short"),
       unit        = fld("unit"),
       type        = fld("type"),
       continuous  = lgl("continuous"),
       discrete    = lgl("discrete"),
       values      = lst("values"),
       decode      = lst("decode"),
       range       = lst("range"),
       primary_key = if (is.null(meta$primary_key)) character(0) else as.character(meta$primary_key),
       flags       = lapply(flags, as.character))
}

# SEMANTIC OVERLAY PROVENANCE (Codex round 7, finding 8). A specification file
# that carries EDA role annotations on top of an immutable base (assembly)
# spec declares that relation in its header:
#   # overlay-of: <repo-relative path of the base spec>
#   # overlay-of-sha256: sha256:<hex of the base spec's bytes>
# Returns list(base_spec, base_spec_sha256) or NULL when the file declares no
# base. It reads the header only; it does not check the base (the EDA task
# does, at composition and again before packaging).
ys_overlay_provenance <- function(spec_path) {
  if (!file.exists(spec_path))
    stop("yspec specification not found: ", spec_path, call. = FALSE)
  txt <- readLines(spec_path, warn = FALSE)
  head <- txt[seq_len(min(length(txt), 40L))]
  pick <- function(key) {
    ln <- grep(sprintf("^#\\s*%s:", key), head, value = TRUE)
    if (!length(ln)) return(NULL)
    trimws(sub(sprintf("^#\\s*%s:\\s*", key), "", ln[1]))
  }
  base <- pick("overlay-of"); sha <- pick("overlay-of-sha256")
  if (is.null(base) && is.null(sha)) return(NULL)
  if (is.null(base) || is.null(sha) || !nzchar(base) || !nzchar(sha))
    stop("yspec specification ", spec_path, " declares an overlay header with ",
         "only one of `overlay-of:` / `overlay-of-sha256:`; both are required.",
         call. = FALSE)
  if (!grepl("^sha256:[0-9a-f]{64}$", sha))
    stop("yspec specification ", spec_path, ": overlay-of-sha256 '", sha,
         "' is not of the form sha256:<64 lowercase hex>.", call. = FALSE)
  list(base_spec = base, base_spec_sha256 = sha)
}

# Checks that the base spec an overlay names still has the recorded bytes.
# Returns the provenance list invisibly; stops otherwise (or when the overlay
# declares no base and `required = TRUE`).
ys_overlay_check <- function(spec_path, required = FALSE) {
  prov <- ys_overlay_provenance(spec_path)
  if (is.null(prov)) {
    if (required)
      stop("yspec specification ", spec_path, " declares no `overlay-of:` base ",
           "spec.", call. = FALSE)
    return(invisible(NULL))
  }
  if (!file.exists(prov$base_spec))
    stop("The EDA specification ", spec_path, " is an overlay of the base spec ",
         prov$base_spec, ", which does not exist.", call. = FALSE)
  actual <- paste0("sha256:", digest::digest(file = prov$base_spec, algo = "sha256"))
  if (!identical(actual, prov$base_spec_sha256))
    stop("The EDA specification ", spec_path, " is a semantic overlay bound to the ",
         "base spec ", prov$base_spec, " at ", prov$base_spec_sha256,
         ", but that base spec on disk is ", actual, ". The overlay no longer ",
         "describes the specification the dataset was assembled against; ",
         "re-derive the overlay from the current base spec (and record its ",
         "sha256) or restore the base spec. ESCALATE to the SME.", call. = FALSE)
  invisible(prov)
}

# The axis label for a column, built from the spec: "<short> (<unit>)", or
# just "<short>" when the spec declares no unit. Stops when the column has no
# `short:` annotation rather than falling back to the bare column name — a
# NONMEM column name is not a label.
ys_axis_label <- function(projection, col) {
  if (!col %in% projection$columns)
    stop("ys_axis_label(): column '", col, "' is not in the specification ",
         projection$spec_path, ".", call. = FALSE)
  short <- projection$short[[col]]
  if (!nzchar(short))
    stop("ys_axis_label(): column '", col, "' has no `short:` annotation in ",
         projection$spec_path, ", so no axis label can be derived from the ",
         "specification. Add `short:` (and `unit:` where the column has one) ",
         "to that column.", call. = FALSE)
  unit <- projection$unit[[col]]
  if (nzchar(unit)) paste0(short, " (", unit, ")") else short
}

# The values of a column whose declared decode matches `decode_label` — e.g.
# which EVID value means "observation". Stops when the column carries no
# values/decode annotation, naming the column and what is missing, rather
# than assuming the NONMEM convention.
ys_decode_values <- function(projection, col, decode_label) {
  if (!col %in% projection$columns)
    stop("ys_decode_values(): column '", col, "' is not in the specification ",
         projection$spec_path, ".", call. = FALSE)
  vals <- projection$values[[col]]
  dec  <- projection$decode[[col]]
  if (is.null(vals) || is.null(dec)) {
    missing_ann <- if (is.null(vals) && is.null(dec))
      "has no `values:` and no `decode:` annotation"
    else if (is.null(dec))
      "declares `values:` but has no `decode:` annotation"
    else "declares `decode:` but has no `values:` annotation"
    stop("ys_decode_values(): column '", col, "' ", missing_ann, " in ",
         projection$spec_path, ", so the value meaning '", decode_label,
         "' cannot be derived from the specification. Add the missing ",
         "annotation to that column, as a `decode:` list parallel to its ",
         "`values:` list.", call. = FALSE)
  }
  if (length(vals) != length(dec))
    stop("ys_decode_values(): column '", col, "' declares ", length(vals),
         " value(s) and ", length(dec), " decode(s) in ", projection$spec_path,
         "; they must be parallel.", call. = FALSE)
  hit <- tolower(trimws(as.character(dec))) == tolower(trimws(decode_label))
  if (!any(hit))
    stop("ys_decode_values(): no value of column '", col, "' decodes to '",
         decode_label, "' in ", projection$spec_path, " (declared decodes: ",
         paste(dec, collapse = ", "), ").", call. = FALSE)
  vals[hit]
}

# Resolves column ROLES from the specification's SETUP__ `flags` blocks.
#
# `required` is a named list, one element per role the caller needs:
#   list(<role> = list(flag = "<yspec flag name>", min = 1L, max = 1L))
# `min`/`max` are the number of columns the role admits (max = Inf for an
# open set such as "the continuous covariates"). A role whose flag is absent
# from the spec, empty, or outside its cardinality is a STOP that names the
# role, the flag the spec must carry, and the columns it currently lists —
# never a fallback to a guessed column name.
#
# Returns a named list: role -> character vector of columns.
ys_resolve_roles <- function(projection, required) {
  if (!is.list(required) || !length(required) || is.null(names(required)))
    stop("ys_resolve_roles(): `required` must be a named list of role ",
         "declarations.", call. = FALSE)
  flags <- projection$flags
  out <- list()
  problems <- character(0)
  for (role in names(required)) {
    spec <- required[[role]]
    flag <- spec$flag
    mn <- if (is.null(spec$min)) 1L else spec$min
    mx <- if (is.null(spec$max)) 1L else spec$max
    cols <- flags[[flag]]
    if (is.null(cols)) cols <- character(0)
    cols <- cols[nzchar(cols)]
    absent <- setdiff(cols, projection$columns)
    if (length(absent)) {
      problems <- c(problems, sprintf(
        "role '%s': flag `%s` lists column(s) not in the specification: %s",
        role, flag, paste(absent, collapse = ", ")))
      next
    }
    if (length(cols) < mn || length(cols) > mx) {
      want <- if (is.finite(mx) && mn == mx) sprintf("exactly %d column(s)", mn)
              else if (is.finite(mx)) sprintf("between %d and %d columns", mn, mx)
              else sprintf("at least %d column(s)", mn)
      problems <- c(problems, sprintf(
        "role '%s': the specification must annotate %s under `SETUP__: flags: %s:`; it currently lists %s",
        role, want, flag,
        if (length(cols)) paste(cols, collapse = ", ") else "none"))
      next
    }
    out[[role]] <- cols
  }
  if (length(problems))
    stop("The yspec specification ", projection$spec_path, " does not carry ",
         "the column-role annotations this analysis needs:\n  - ",
         paste(problems, collapse = "\n  - "),
         "\nAdd the missing `SETUP__: flags:` entries to the specification. ",
         "Column roles are never inferred from column names or short labels.",
         call. = FALSE)
  out
}

# The resolved mapping as a table for the gate package: one row per role,
# with the column, its short label, unit and declared decodes.
ys_role_table <- function(projection, roles) {
  rows <- lapply(names(roles), function(r) {
    cols <- roles[[r]]
    if (!length(cols))
      return(data.frame(role = r, column = NA_character_, short = NA_character_,
                        unit = NA_character_, decode = NA_character_,
                        stringsAsFactors = FALSE))
    do.call(rbind, lapply(cols, function(c) {
      dec <- projection$decode[[c]]
      val <- projection$values[[c]]
      data.frame(role = r, column = c,
                 short = projection$short[[c]],
                 unit = projection$unit[[c]],
                 decode = if (is.null(dec) || is.null(val)) NA_character_
                          else paste(sprintf("%s = %s", val, dec), collapse = "; "),
                 stringsAsFactors = FALSE)
    }))
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
