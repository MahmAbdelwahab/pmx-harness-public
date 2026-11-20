# Deterministic verification for a declared analysis and its checked reproduction.

rv_outcome <- function(rule = character(0), checklist_item = NA_character_,
                       finding = character(0), evidence_path = NA_character_,
                       owner = NA_character_) {
  n <- length(rule)
  if (!n) return(data.frame(rule = character(0), checklist_item = character(0),
                            finding = character(0), evidence_path = character(0),
                            owner = character(0), stringsAsFactors = FALSE))
  data.frame(rule = rule, checklist_item = rep_len(as.character(checklist_item), n),
             finding = rep_len(as.character(finding), n),
             evidence_path = rep_len(as.character(evidence_path), n),
             owner = rep_len(as.character(owner), n), stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
# Declaration, location fields, derivation, runner (Decisions 4 and 6)
# ---------------------------------------------------------------------------

# Parses analyses/<ID>.R and extracts its one compose_pipeline() call; the
# arguments are evaluated in a child of baseenv() (they are data literals).
# attest_tasks carried.
rv_read_declaration <- function(file) {
  exprs <- parse(file, keep.source = FALSE, encoding = "UTF-8")
  is_compose <- vapply(exprs, function(e)
    is.call(e) && identical(e[[1L]], as.name("compose_pipeline")), logical(1))
  if (sum(is_compose) != 1L)
    stop("review: expected exactly one compose_pipeline() call in ", file,
         "; found ", sum(is_compose), call. = FALSE)
  cc <- exprs[[which(is_compose)]]
  stub <- function(analysis_id, tasks, inputs = list(), attest_tasks = tasks) NULL
  args <- as.list(match.call(stub, cc))[-1L]
  sandbox <- new.env(parent = baseenv())
  ev <- function(a) if (is.null(a)) NULL else eval(a, sandbox)
  tasks <- ev(args$tasks)
  list(file = file, exprs = exprs, compose_index = which(is_compose),
       compose_call = cc, analysis_id = ev(args$analysis_id), tasks = tasks,
       inputs = ev(args$inputs),
       attest_tasks = if (is.null(args$attest_tasks)) tasks else ev(args$attest_tasks))
}

# Every location field of the declared inputs, found by walking the blocks
# recursively. The location fields are out_dir, report_dir, model_dir,
# covariate_dir, workdir and store; another field whose name ends in "dir" or
# in "path" (after an underscore or as the whole name) stops, because the
# derivation would leave it pointing at the examined tree; so does a location
# that is not one relative path without "..". Returns one row per field:
# path (block and field joined by "/"), field, value.
rv_project_path <- function(path) {
  is.character(path) && length(path) == 1L && !is.na(path) && nzchar(path) &&
    !grepl("(^|/)[.][.](/|$)|^[/~]|^[A-Za-z]:|^([.]/?)+$", path) &&
    !grepl(intToUtf8(92), path, fixed = TRUE)
}

rv_location_fields <- function(inputs) {
  known <- c("out_dir", "report_dir", "model_dir", "covariate_dir", "workdir", "store")
  rows <- list()
  walk <- function(x, trail) {
    if (!is.list(x)) return(invisible(NULL))
    nm <- names(x)
    for (k in seq_along(x)) {
      f <- if (is.null(nm) || is.na(nm[k])) "" else nm[k]
      here <- paste(c(trail, if (nzchar(f)) f else as.character(k)), collapse = "/")
      if (f %in% known) {
        v <- x[[k]]
        if (!is.character(v) || length(v) != 1L || is.na(v) || !nzchar(v))
          stop("review: location field '", here, "' must be one non-empty string",
               call. = FALSE)
        if (!rv_project_path(v))
          stop("review: location field '", here, "' is absolute or contains '..'",
               call. = FALSE)
        rows[[length(rows) + 1L]] <<- data.frame(path = here, field = f, value = v,
                                                 stringsAsFactors = FALSE)
      } else if (nzchar(f) && grepl("dir$|(^|_)path$", f)) {
        stop("review: field '", here, "' looks like a location but is not one of ",
             paste(known, collapse = ", "), "; the derivation cannot remap it",
             call. = FALSE)
      } else {
        walk(x[[k]], here)
      }
    }
  }
  walk(inputs, character(0))
  out <- if (length(rows)) do.call(rbind, rows) else
    data.frame(path = character(0), field = character(0), value = character(0),
               stringsAsFactors = FALSE)
  attr(out, "fields") <- known
  out
}

# The one store rule and the mirror prefix (Decision 6): every store field
# becomes <ws_rel>/store, every other location <ws_rel>/<value>, both relative
# to the copy root; nothing else is rewritten. The rebuilt call reads the
# derived inputs from <ws_rel>/inputs.rds (written by rv_write_runner), so the
# derived values reach compose_pipeline unchanged by any deparse. Ported from
rv_derive_runner <- function(decl, ws_rel) {
  loc <- rv_location_fields(decl$inputs)
  known <- attr(loc, "fields")
  store <- file.path(ws_rel, "store")
  rewrite <- function(x) {
    if (!is.list(x)) return(x)
    nm <- names(x)
    for (k in seq_along(x)) {
      f <- if (is.null(nm) || is.na(nm[k])) "" else nm[k]
      if (identical(f, "store")) x[[k]] <- store
      else if (f %in% known) x[[k]] <- file.path(ws_rel, x[[k]])
      else if (is.list(x[[k]])) x[[k]] <- rewrite(x[[k]])
    }
    x
  }
  cc <- decl$compose_call
  cc$attest_tasks <- decl$attest_tasks
  cc$inputs <- call("readRDS", file.path(ws_rel, "inputs.rds"))
  derived <- ifelse(loc$field == "store", store, file.path(ws_rel, loc$value))
  list(inputs = rewrite(decl$inputs), locations = loc,
       location_map = stats::setNames(derived, loc$value),
       modified_call = cc, original_call = decl$compose_call, store = store)
}

# Writes <ws_dir>/inputs.rds and <ws_dir>/runner.R: a prologue (recursion
# guard, then SOURCE_DATE_EPOCH) and the declaration's own top-level
# expressions with the compose_pipeline() call replaced. Ported from
rv_write_runner <- function(decl, derived, ws_dir, epoch) {
  exprs <- decl$exprs
  exprs[[decl$compose_index]] <- derived$modified_call
  saveRDS(derived$inputs, file.path(ws_dir, "inputs.rds"))
  lines <- c(
    "# Generated by the review task: fresh-store reproduction runner.",
    sprintf("# Derived from the committed declaration %s: every store field moved to", decl$file),
    "# the workspace store and every other location prefixed with the workspace",
    "# path in the reproduction workspace; every other value is",
    "# carried from the parsed declaration through inputs.rds. Run from the copy root.",
    "",
    "# Recursion guard (a prior analysis finding R12): a reproduction must not reproduce",
    "# itself. Hard stop if the marker is already set (depth check), then export it.",
    "if (nzchar(Sys.getenv(\"PMX_REVIEW_REPRODUCTION\"))) {",
    "  stop(\"Recursion guard: PMX_REVIEW_REPRODUCTION is already set - \",",
    "       \"a reproduction is attempting to reproduce itself.\")",
    "}",
    "Sys.setenv(PMX_REVIEW_REPRODUCTION = \"1\")",
    sprintf("Sys.setenv(SOURCE_DATE_EPOCH = \"%s\")", epoch),
    "")
  for (e in exprs) lines <- c(lines, deparse(e, width.cutoff = 500L), "")
  path <- file.path(ws_dir, "runner.R")
  writeLines(lines, path)
  invisible(path)
}

# The recursion guard's read (a prior analysis finding R12).
rv_inside_reproduction <- function() nzchar(Sys.getenv("PMX_REVIEW_REPRODUCTION"))

# What review_reproduction returns inside a reproduction: the element names of
# the real result, values explicit about why nothing was spawned.
rv_reproduction_stub <- function() {
  msg <- "skipped: recursion guard - a reproduction does not reproduce itself"
  list(skipped = TRUE,
       record = data.frame(quantity = "reproduction", value = msg, stringsAsFactors = FALSE),
       comparison = data.frame(item = "reproduction", match = NA, exact = NA,
                               max_abs_diff = NA_real_, detail = msg,
                               stringsAsFactors = FALSE),
       excluded = data.frame(entry = "none", kind = "none", reason = msg, removed = NA_integer_,
                             stringsAsFactors = FALSE),
       files = data.frame(item = "reproduction", file = NA_character_, class = NA_character_,
                          match = NA, detail = msg, stringsAsFactors = FALSE),
       text = data.frame(item = "reproduction", rule = NA_character_, outcome = NA,
                         detail = msg, stringsAsFactors = FALSE),
       census = data.frame(quantity = "render-acceptance census", stream = NA_real_,
                           log = NA_real_, judged = NA_real_, threshold = NA_real_,
                           within = NA, note = msg, stringsAsFactors = FALSE),
       outcomes = rv_outcome(),
       workspace = NA_character_)
}

# ---------------------------------------------------------------------------
# The copy (Decision 5)
# ---------------------------------------------------------------------------

# sha256 of every regular file under the given roots of `base` (globs
# allowed), minus the `exclude` prefixes and minus bytecode caches (a path
# with a __pycache__ component or a .pyc suffix; R2-F1). Sorted table of
# relative path and sha256.
rv_tree_digest <- function(base, roots, exclude = character(0)) {
  b <- normalizePath(base, winslash = "/", mustWork = TRUE)
  files <- character(0)
  for (r in roots) {
    for (h in Sys.glob(file.path(b, r))) {
      if (!startsWith(normalizePath(h, winslash = "/", mustWork = TRUE), paste0(b, "/")))
        stop("review: census root escapes its tree: ", r, call. = FALSE)
      if (dir.exists(h)) {
        files <- c(files, list.files(h, recursive = TRUE, all.files = TRUE,
                                     full.names = TRUE, no.. = TRUE))
      } else if (file.exists(h)) {
        files <- c(files, h)
      }
    }
  }
  files <- unique(files)
  rel <- substring(files, nchar(b) + 2L)
  keep <- !grepl("(^|/)__pycache__(/|$)", rel) & !grepl("\\.pyc$", rel)
  for (x in sub("/+$", "", exclude)) keep <- keep & rel != x & !startsWith(rel, paste0(x, "/"))
  files <- files[keep]; rel <- rel[keep]
  resolved <- normalizePath(files, winslash = "/", mustWork = TRUE)
  escaped <- !startsWith(resolved, paste0(b, "/"))
  if (any(escaped))
    stop("review: census file escapes its tree: ", rel[which(escaped)[1]], call. = FALSE)
  o <- order(rel)
  data.frame(path = rel[o],
             sha256 = vapply(files[o], function(f) digest::digest(file = f, algo = "sha256"),
                             character(1), USE.NAMES = FALSE),
             stringsAsFactors = FALSE)
}

# Compares two digest tables of the same roots. On a difference it writes
# both tables and the differing paths into `out_dir` under `name` and stops
# (Decision 8 (b) and (c): a stop, critical, routed to root).
rv_tree_check <- function(before, after, out_dir, name, what) {
  if (identical(before, after)) return(invisible(TRUE))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  key <- function(d) paste(d$path, d$sha256)
  differ <- sort(unique(c(before$path[!key(before) %in% key(after)],
                          after$path[!key(after) %in% key(before)])))
  utils::write.table(before, file.path(out_dir, paste0(name, "-before.tsv")),
                     sep = "\t", row.names = FALSE, quote = FALSE)
  utils::write.table(after, file.path(out_dir, paste0(name, "-after.tsv")),
                     sep = "\t", row.names = FALSE, quote = FALSE)
  writeLines(differ, file.path(out_dir, paste0(name, "-difference.txt")))
  stop("review: ", what, " changed during the reproduction (", length(differ),
       " path(s), first: ", differ[1], "); both digest tables and the difference are in ",
       out_dir, ". A stop, routed to root (Decision 8).", call. = FALSE)
}

# The copy's own HEAD and porcelain line count, read with Git read-only
# (precedent: bbr_estimation.R lines 99-103, approve_gate.R lines 66 and 79;
# --no-optional-locks so status does not refresh the copy's index).
rv_copy_head <- function(copy_root) {
  git <- function(...) suppressWarnings(tryCatch(
    system2("git", c("--no-optional-locks", "--git-dir", shQuote(file.path(copy_root, ".git")),
                     "--work-tree", shQuote(copy_root), ...), stdout = TRUE, stderr = FALSE),
    error = function(e) structure(character(0), status = 127L)))
  h <- git("rev-parse", "HEAD")
  if (!is.null(attr(h, "status")) || length(h) != 1L || !grepl("^[0-9a-f]{40}$", h))
    return(list(head = "HEAD unreadable", porcelain_lines = NA_integer_))
  s <- git("status", "--porcelain")
  list(head = h, porcelain_lines = if (is.null(attr(s, "status"))) length(s) else NA_integer_)
}

# The directories under `root` matching the cache patterns of the declared
# engines (nlmixr2: ^nlmixr2BootstrapCache_; nonmem: none). Never deletes.
# Both arguments are mandatory. Ported from review_clear_engine_caches
rv_engine_caches <- function(root, engines) {
  if (missing(root) || missing(engines))
    stop("rv_engine_caches: root and engines are both required", call. = FALSE)
  patterns <- list(nlmixr2 = "^nlmixr2BootstrapCache_", nonmem = character(0))
  p <- unlist(patterns[intersect(engines, names(patterns))])
  if (!length(p)) return(character(0))
  d <- list.dirs(root, full.names = TRUE, recursive = FALSE)
  hit <- rep(FALSE, length(d))
  for (x in p) hit <- hit | grepl(x, basename(d))
  d[hit]
}

# The copy refusals of Decision 5 and the census-roots digest equality. With
# dry = TRUE (L3-dry) the absent harness.yaml and the differing census roots
# are reported instead of refused; every other refusal stops. Returns the
# facts for the reproduction record.
rv_copy_check <- function(examined, copy, live_root, ws_rel, engines, roots,
                          exclude = character(0), dry = FALSE) {
  halt <- function(...) stop("review: copy refusal: ", ..., call. = FALSE)
  if (!is.character(copy) || length(copy) != 1L || is.na(copy) || !nzchar(copy))
    halt("PMX_REVIEW_COPY is unset or empty; root's run names the private copy in it")
  if (!startsWith(copy, "/")) halt("PMX_REVIEW_COPY '", copy, "' is not an absolute path")
  if (!dir.exists(copy)) halt("PMX_REVIEW_COPY '", copy, "' is not a directory")
  cn <- normalizePath(copy, mustWork = TRUE)
  en <- normalizePath(examined, mustWork = TRUE)
  ln <- normalizePath(live_root, mustWork = FALSE)
  related <- function(a, b) identical(a, b) || startsWith(a, paste0(b, "/")) ||
    startsWith(b, paste0(a, "/"))
  if (related(cn, en)) halt("the copy '", cn, "' is equal to, inside or containing the examined root")
  if (related(cn, ln)) halt("the copy '", cn, "' is equal to, inside or containing the live root")
  if (!dir.exists(file.path(cn, ".git"))) halt("the copy has no .git directory")
  harness <- file.exists(file.path(cn, "harness.yaml"))
  if (!harness && !dry) halt("the copy holds no harness.yaml (the owner places it, W7b)")
  if (nzchar(Sys.getenv("REPO_ROOT")))
    halt("REPO_ROOT is set; it would redirect the container's harness.yaml (F-15)")
  gi <- file.path(cn, ".gitignore")
  rules <- if (file.exists(gi)) trimws(readLines(gi, warn = FALSE)) else character(0)
  rules <- rules[nzchar(rules) & !startsWith(rules, "#") & !startsWith(rules, "!")]
  ignored <- any(vapply(rules, function(r)
    grepl(utils::glob2rx(sub("^/", "", sub("/+$", "", r))), ws_rel), logical(1)))
  if (!ignored) halt("no rule of the copy's .gitignore matches the workspace name ", ws_rel)
  caches <- rv_engine_caches(cn, engines)
  if (length(caches)) halt("an engine cache of a declared engine is present: ", basename(caches[1]))
  if (file.exists(file.path(cn, ws_rel))) halt("the workspace ", file.path(cn, ws_rel), " exists; every run uses a new copy")
  da <- rv_tree_digest(en, roots, exclude)
  db <- rv_tree_digest(cn, roots, exclude)
  key <- function(d) paste(d$path, d$sha256)
  differ <- sort(unique(c(da$path[!key(da) %in% key(db)], db$path[!key(db) %in% key(da)])))
  if (length(differ) && !dry)
    halt("the census roots of the copy differ from the examined root (", length(differ),
         " path(s), first: ", differ[1], ")")
  list(copy = cn, examined = en, live = ln, harness_present = harness,
       roots_equal = !length(differ), differing = differ,
       extra_mounts_set = nzchar(Sys.getenv("NONMEM_EXTRA_MOUNTS")), tree = da)
}

# ---------------------------------------------------------------------------
# Store and manifest reads (callr subprocesses; a prior analysis R13)
# ---------------------------------------------------------------------------

# Metadata of one store: name, type, error, format and path (a list column).
rv_store_meta <- function(store) {
  callr::r(function(store) {
    m <- targets::tar_meta(store = store)
    keep <- intersect(c("name", "type", "error", "format", "path"), names(m))
    as.data.frame(m[, keep])
  }, args = list(store = store))
}

# The values of one store: every *_package target, dataset,
# environment_attestation, every target named in the reporting package's
# table "Promoted values and their sources", and `extra`. A target that cannot
# be read is NULL, its message in the attribute "read_errors". Ported from
rv_store_values <- function(store, extra = character(0)) {
  callr::r(function(store, extra) {
    m <- targets::tar_meta(store = store)
    real <- m$name[m$type %in% c("stem", "pattern")]
    want <- unique(c(sort(grep("_package$", real, value = TRUE)),
                     intersect(c("dataset", "environment_attestation"), real), extra))
    out <- list()
    errs <- character(0)
    read <- function(nm) {
      v <- try(targets::tar_read_raw(nm, store = store), silent = TRUE)
      if (inherits(v, "try-error")) {
        errs[nm] <<- conditionMessage(attr(v, "condition"))
        out[nm] <<- list(NULL)
      } else {
        out[nm] <<- list(v)
      }
    }
    for (nm in want) read(nm)
    pr <- out[["reporting_package"]]$tables[["Promoted values and their sources"]]$target
    for (nm in setdiff(pr, names(out))) read(nm)
    attr(out, "read_errors") <- errs
    out
  }, args = list(store = store, extra = extra))
}

# The manifest (every field) and the network edges of a targets script, each
# read in its own fresh subprocess whose working directory is `wd`: a fresh
# global environment holds only what the script sources, as under tar_make,
# and the derived runner's recursion guard sees the script run once per
# process.
rv_manifest <- function(script, wd) {
  read <- function(script, wd, what) {
    old <- setwd(wd)
    on.exit(setwd(old))
    if (identical(what, "manifest"))
      return(as.data.frame(targets::tar_manifest(script = script, fields = tidyselect::everything(),
                                                 callr_function = NULL, envir = globalenv())))
    as.data.frame(targets::tar_network(script = script, targets_only = TRUE,
                                       callr_function = NULL, envir = globalenv())$edges)
  }
  list(manifest = callr::r(read, args = list(script = script, wd = wd, what = "manifest")),
       edges = callr::r(read, args = list(script = script, wd = wd, what = "edges")))

}
# ---------------------------------------------------------------------------
# Inputs and the write guard (Decisions 7 and 8 (a))
# ---------------------------------------------------------------------------

# The paths of a literal input file target: the LAST expression of the
# command, taken repeatedly while it is a `{` block (the composer prefixes
# dependency names, root's P2; a module's gate wrapper nests a second block,
# identity(c(<character literals>)). NULL for any other command.
rv_identity_paths <- function(command) {
  e <- tryCatch(parse(text = command, keep.source = FALSE)[[1L]], error = function(err) NULL)
  if (is.null(e)) return(NULL)
  while (is.call(e) && identical(e[[1L]], as.name("{")) && length(e) > 1L) e <- e[[length(e)]]
  if (!is.call(e) || !identical(e[[1L]], as.name("identity")) || length(e) != 2L) return(NULL)
  a <- e[[2L]]
  literal <- is.character(a) ||
    (is.call(a) && identical(a[[1L]], as.name("c")) &&
       all(vapply(as.list(a)[-1L], is.character, logical(1))))
  if (!literal) return(NULL)
  as.character(eval(a, baseenv()))
}

# The literal input file targets of the DERIVED manifest whose paths the
# moved"): dest is the path as given, source the same path in the copy with
# the workspace prefix removed. The derivation remaps declared locations only,
# so a literal under the prefix is derived from one by construction, including
# a sibling a module forms itself (reporting.R's forest parameter file under
# dirname(report_dir)). A copied path with a '..' component is refused: its
# destination could leave the workspace (Decision 7).
# With `locations` (pass 1 of plan 14.4 item 6, before the derived pipeline is
# composed): the identity inputs of the EXAMINED manifest that equal or lie
# under a declared non-store location value, except a path in `kept`: every
# character value of the declaration other than a location (only exact path
# equality matters, so non-path strings are inert; the run-209 listing is one).
rv_input_files <- function(manifest, ws_rel, locations = NULL, kept = character(0)) {
  pre <- paste0(ws_rel, "/")
  loc <- sub("/+$", "", as.character(locations))
  fm <- manifest[manifest$format %in% "file", , drop = FALSE]
  rows <- lapply(seq_len(nrow(fm)), function(i) {
    p <- as.character(rv_identity_paths(fm$command[i]))
    if (is.null(locations)) {
      p <- substring(p[startsWith(p, pre)], nchar(pre) + 1L)
    } else {
      under <- vapply(p, function(x) any(x == loc | startsWith(x, paste0(loc, "/"))), logical(1))
      p <- p[under & !p %in% kept]
    }
    if (!length(p)) return(NULL)
    up <- p[grepl("(^|/)[.][.](/|$)", p)]
    if (length(up))
      stop("review: input path '", up[1], "' (target ", fm$name[i], ") has a '..' component; ",
           "an input must stay inside the workspace (Decision 7)", call. = FALSE)
    data.frame(target = fm$name[i], source = p, dest = paste0(pre, p), stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(data.frame(target = character(0), source = character(0),
                                       dest = character(0), stringsAsFactors = FALSE))
  out <- do.call(rbind, rows)
  out[!duplicated(out$dest), , drop = FALSE]
}

# "Approve pattern and .diff as the only inert class (Recommended)"). An input
# of one of these classes that holds a root's absolute path is a recorded
# fact; every other class, and a file without an extension, is refused
# (Decision 7). The one place the list lives.
rv_input_classes <- function() ".diff"

# Static write census of the derived commands (Decision 8 (a)). A hit is a
# string constant that (i) as a relative path equals a path written by a
# computed file target of the analysis store, or an ancestor directory of one
# in the project, and does not lie under the workspace; or (ii) is an
# absolute path under the examined root, under the live root, or under the
# copy root outside the workspace. `roots` is c(examined = , live = , copy = ).
rv_write_census <- function(commands, written, ws_rel, roots) {
  ancestors <- unique(unlist(lapply(written, function(p) {
    d <- character(0)
    x <- dirname(p)
    while (!x %in% c(".", "/", "")) { d <- c(d, x); x <- dirname(x) }
    d
  })))
  leaves <- function(x) {
    if (is.character(x)) return(x)
    if (is.call(x) || is.pairlist(x) || is.expression(x) || is.list(x))
      return(unlist(lapply(as.list(x), leaves)))
    character(0)
  }
  under <- function(s, r) identical(s, r) || startsWith(s, paste0(r, "/"))
  ws_abs <- file.path(roots[["copy"]], ws_rel)
  rows <- list()
  for (nm in names(commands)) {
    e <- tryCatch(parse(text = commands[[nm]], keep.source = FALSE), error = function(err) NULL)
    for (s in unique(leaves(e))) {
      if (is.na(s) || !nzchar(s)) next
      cls <- NA_character_
      if (startsWith(s, "/")) {
        if (under(s, roots[["examined"]])) cls <- "absolute, under the examined root"
        else if (under(s, roots[["live"]])) cls <- "absolute, under the live root"
        else if (under(s, roots[["copy"]]) && !under(s, ws_abs)) cls <- "absolute, under the copy root outside the workspace"
      } else {
        r <- sub("/+$", "", sub("^\\./", "", s))
        if (!under(r, ws_rel)) {
          if (r %in% written) cls <- "path written by a computed file target"
          else if (r %in% ancestors) cls <- "ancestor directory of a written path"
        }
      }
      if (!is.na(cls)) rows[[length(rows) + 1L]] <- data.frame(target = nm, literal = s,
                                                               class = cls, stringsAsFactors = FALSE)
    }
  }
  if (!length(rows)) return(data.frame(target = character(0), literal = character(0),
                                       class = character(0), stringsAsFactors = FALSE))
  do.call(rbind, rows)
}

# ---------------------------------------------------------------------------
# Setup, spawn and outcome (Decisions 5 to 12)
# ---------------------------------------------------------------------------

# The gate targets whose record in the copy does not approve at setup (an
# absent record included): the errored set the fresh run must show.
rv_expected_errors <- function(root, analysis_id, tasks) {
  unsigned <- vapply(tasks, function(tk) {
    f <- file.path(root, gate_file_for(analysis_id, tk))
    d <- tryCatch(parse_gate_record(f)$decision, error = function(e) NA_character_)
    !isTRUE(d %in% c("approved", "approved_with_changes"))
  }, logical(1))
  paste0("gate_", tasks[unsigned])
}

# Pure: the shell command of the spawn (kind "tar_make", Decision 11) or of
# the census legs (kinds "census" and "tectonic", Decision 15). Every path is
# shell-quoted and the cd comes first (the integrity batch 15 lesson).
rv_spawn_command <- function(copy_root, kind, ws_rel, analysis_id = NULL, epoch = NULL,
                             rscript = file.path(R.home("bin"), "Rscript"),
                             tectonic = unname(Sys.which("tectonic")),
                             document_date = NULL) {
  q <- shQuote
  ws <- file.path(copy_root, ws_rel)
  census <- file.path(ws, "census")
  switch(kind,
    tar_make = paste0(
      "cd ", q(copy_root), " && exec ", q(rscript), " -e ",
      q(sprintf('targets::tar_make(script = "%s", store = "%s")',
                file.path(ws_rel, "runner.R"), file.path(ws_rel, "store"))),
      " > ", q(file.path(ws, "tar_make.log")), " 2>&1"),
    census = paste0(
      "cd ", q(file.path(census, "report")), " && SOURCE_DATE_EPOCH=", q(epoch),
      " PATH=", q(file.path(census, "shim")), ":\"$PATH\" quarto render mar.qmd --to pdf",
      " --execute-dir ", q(copy_root),
      " -P ", q(paste0("store:", file.path(ws_rel, "store"))),
      " -P ", q(paste0("analysis_id:", analysis_id)),
      if (!is.null(document_date)) paste0(" -P ", q(paste0("date:", document_date))),
      " > ", q(file.path(census, "stream.txt")), " 2>&1"),
    tectonic = paste0(
      "cd ", q(file.path(census, "report")), " && ", q(tectonic), " --keep-logs --outdir ",
      q(file.path(census, "engine")), " - < ", q(file.path(census, "shim", "captured.tex")),
      " > ", q(file.path(census, "engine-stream.txt")), " 2>&1"),
    stop("rv_spawn_command: unknown kind '", kind, "'", call. = FALSE))
}

# Everything before the spawn (Section 7.2 item 3): guard, preconditions,
# live-tree digest before, copy check, the copy's HEAD against the receipt,
# workspace, derivation, inputs with the input path check, runner, write
# census. Writes only under <copy>/<ws_rel>/; its facts and digest tables are
# returned to the target body, which alone writes into the review out_dir.
# dry = TRUE (L3-dry) reports the three items Decision 5 names instead of
# stopping on them, adds the comparison of the derived and the examined
# manifests, and stops before any spawn. Ported from review_reproduce_setup
rv_reproduce_setup <- function(analysis_id, live_root, dry = FALSE) {
  if (missing(live_root) || !is.character(live_root) || length(live_root) != 1L ||
      !startsWith(live_root, "/"))
    stop("rv_reproduce_setup: live_root must be the live repository's absolute path",
         call. = FALSE)
  if (rv_inside_reproduction())
    stop("review: rv_reproduce_setup called inside a reproduction (PMX_REVIEW_REPRODUCTION ",
         "is set); a reproduction does not reproduce itself", call. = FALSE)
  if (!is.character(analysis_id) || length(analysis_id) != 1L || is.na(analysis_id) ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9_-]*$", analysis_id))
    stop("review: analysis_id must be a declaration filename stem.", call. = FALSE)
  examined <- normalizePath(".", mustWork = TRUE)
  ws_rel <- paste0("_targets_review_", analysis_id)
  decl_file <- file.path("analyses", paste0(analysis_id, ".R"))
  decl <- rv_read_declaration(decl_file)
  if (!identical(decl$analysis_id, analysis_id))
    stop("review: ", decl_file, " declares '", decl$analysis_id, "', not '", analysis_id, "'",
         call. = FALSE)
  rb <- decl$inputs[["review"]]
  pb <- decl$inputs[["reporting"]]
  if (!is.list(rb)) stop("review: ", decl_file, " has no review block", call. = FALSE)
  if (!is.list(pb)) stop("review: ", decl_file, " has no reporting block", call. = FALSE)
  if (!rv_project_path(rb$store) || !identical(rb$store, pb$store))
    stop("review: store: the review block's store differs from the reporting block's in ",
         decl_file, call. = FALSE)
  der <- rv_derive_runner(decl, ws_rel)
  dirs <- der$locations$value[der$locations$field != "store" & der$locations$path != "review/out_dir"]
  preliminary <- unique(c(decl_file, rb$assessment, rb$map, rb$checkout,
                          names(rb$expected_changes), dirs, file.path("gates", analysis_id),
                          "pipeline", "tools", "skills", "config"))
  exclude <- c(rb$out_dir, rb$store)
  if (!all(vapply(c(preliminary, exclude), function(path)
    rv_project_path(path) && !grepl("[*?\\[]", path), logical(1))))
    stop("review: declared copy roots and exclusions must be literal project paths.", call. = FALSE)
  engines <- unique(c(if (is.character(decl$inputs$engine)) decl$inputs$engine,
                      unlist(lapply(Filter(is.list, decl$inputs), function(b) b[["engine"]]))))
  if (!length(engines)) engines <- "nlmixr2"
  cc <- rv_copy_check(examined, Sys.getenv("PMX_REVIEW_COPY"), live_root, ws_rel, engines,
                      preliminary, exclude, dry)
  man_ex <- rv_manifest(decl_file, wd = cc$copy)
  files <- man_ex$manifest$format %in% "file" & !grepl("^gate_.*_file$", man_ex$manifest$name)
  identity <- unique(as.character(unlist(lapply(man_ex$manifest$command[files], rv_identity_paths))))
  meta <- rv_store_meta(file.path(examined, rb$store))
  artifacts <- unique(as.character(unlist(meta$path[
    meta$format %in% "file" & is.na(meta$error) &
      !grepl("^gate_|^review_|^method_manifests$", meta$name)])))
  required <- unique(c(decl_file, rb$assessment, rb$map, rb$checkout,
                       names(rb$expected_changes), identity, artifacts))
  for (path in required) {
    if (!rv_project_path(path) || grepl("[*?\\[]", path))
      stop("review: required input must be a literal project path: ", path, call. = FALSE)
    if (!file.exists(path))
      stop("review: missing required input ", path, call. = FALSE)
  }
  roots <- unique(c(required, preliminary))
  cc <- rv_copy_check(examined, cc$copy, live_root, ws_rel, engines, roots, exclude, dry)
  tree_before <- cc$tree   # the examined census roots, digested once by the copy check
  copy <- cc$copy
  live <- cc$live
  receipt <- file.path(examined, rb$checkout)
  receipt_head <- if (file.exists(receipt))
    sub("^HEAD ", "", grep("^HEAD ", readLines(receipt, warn = FALSE), value = TRUE)) else character(0)
  ch <- rv_copy_head(copy)
  head_equal <- length(receipt_head) == 1L && identical(ch$head, receipt_head)
  if (!dry && identical(ch$head, "HEAD unreadable"))
    stop("review: copy refusal: the copy's HEAD is unreadable", call. = FALSE)
  if (!dry && !head_equal)
    stop("review: copy refusal: the copy's HEAD differs from the HEAD line of the checkout ",
         "receipt ", rb$checkout, call. = FALSE)
  ws <- file.path(copy, ws_rel)
  for (path in required) {
    candidate <- file.path(copy, path)
    if (!file.exists(candidate))
      stop("review: missing required input ", path, " in the copy", call. = FALSE)
    if (!startsWith(normalizePath(candidate), paste0(copy, "/")))
      stop("review: required input escapes the copy: ", path, call. = FALSE)
  }
  dir.create(ws)
  # Inputs (Decision 7, Q-B8-18, plan 14.4 item 6): copied from their path in
  # the copy to the workspace in two passes around the derived composition; a
  # missing source stops; generated run directories, sidecars and bbi.yaml are
  # not identity inputs and are never copied; nothing copied is deleted.
  copy_in <- function(rows) for (i in seq_len(nrow(rows))) {
    src <- file.path(copy, rows$source[i])
    if (!file.exists(src) || dir.exists(src))
      stop("review: missing input ", rows$source[i], " (target ", rows$target[i],
           ") in the copy", call. = FALSE)
    dest <- file.path(copy, rows$dest[i])
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(src, dest, copy.date = TRUE))
      stop("review: could not copy the input ", rows$source[i], call. = FALSE)
  }
  # Pass 1, before composing: a module may assert an input when its target is
  # defined (tarchetypes' tar_quarto_raw does).
  locs <- unique(der$locations$value[der$locations$field != "store"])
  kept <- setdiff(rapply(decl$inputs, function(x) x, classes = "character", how = "unlist"),
                  der$locations$value)
  precopy <- rv_input_files(man_ex$manifest, ws_rel, locs, kept)
  copy_in(precopy)
  date <- pb$document[["date"]]
  epoch <- format(as.numeric(as.POSIXct(paste(date, "00:00:00"), tz = "UTC")), scientific = FALSE)
  rv_write_runner(decl, der, ws, epoch)
  man_dr <- tryCatch(rv_manifest(file.path(ws_rel, "runner.R"), wd = copy), error = function(e)
    stop("review: the derived pipeline does not compose after the pre-copy (Decision 7): ",
         conditionMessage(if (inherits(e$parent, "condition")) e$parent else e), call. = FALSE))
  inputs <- rv_input_files(man_dr$manifest, ws_rel)
  stray <- precopy[!precopy$dest %in% inputs$dest, , drop = FALSE]
  if (nrow(stray))
    stop("review: pre-copied input ", stray$source[1], " (target ", stray$target[1],
         ") is not an input of the derived pipeline; a file no derived target reads would ",
         "sit in the workspace (Decision 7)", call. = FALSE)
  # Pass 2: inputs only the derived manifest names.
  copy_in(inputs[!inputs$dest %in% precopy$dest, , drop = FALSE])
  # Input path check (Decision 7, root ruling 0.A.2): the examined and the
  # live root searched as fixed byte strings in every copied input.
  check_roots <- unique(c(examined = examined, live = live))
  root_names <- ifelse(check_roots == examined, "examined root", "live root")
  root_names[check_roots == examined & check_roots == live] <- "examined and live root"
  hits <- list()
  for (i in seq_len(nrow(inputs))) {
    dest <- file.path(copy, inputs$dest[i])
    bytes <- readBin(dest, "raw", n = max(1, file.size(dest)))
    for (k in seq_along(check_roots)) {
      if (length(grepRaw(charToRaw(check_roots[k]), bytes, fixed = TRUE))) {
        ext <- tolower(tools::file_ext(inputs$source[i]))
        cls <- if (nzchar(ext)) paste0(".", ext) else "(no extension)"
        hits[[length(hits) + 1L]] <- data.frame(file = inputs$source[i], class = cls,
                                                root = root_names[k], stringsAsFactors = FALSE)
      }
    }
  }
  hits <- if (length(hits)) do.call(rbind, hits) else
    data.frame(file = character(0), class = character(0), root = character(0),
               stringsAsFactors = FALSE)
  inert <- hits[hits$class %in% rv_input_classes(), , drop = FALSE]
  refused <- hits[!hits$class %in% rv_input_classes(), , drop = FALSE]
  if (nrow(refused))
    stop("review: an input of a refused class holds the ", refused$root[1],
         "'s absolute path: ", refused$file[1], " (class ", refused$class[1], "; ",
         nrow(refused), " such input(s)); Decision 7", call. = FALSE)
  # The write census of the derived commands.
  id_paths <- lapply(man_ex$manifest$command, rv_identity_paths)
  identity_names <- man_ex$manifest$name[!vapply(id_paths, is.null, logical(1))]
  computed <- meta[meta$format %in% "file" & !meta$name %in% identity_names, , drop = FALSE]
  written <- unique(unlist(computed$path))
  # Reads (Q-B8-17): the identity-input paths of the examined pipeline and the
  # paths the compose-level method_manifests target tracks (MANIFEST_TARGET,
  # pipeline/compose.R line 28; a tracker that writes nothing).
  reads <- unique(c(unlist(id_paths), unlist(meta$path[meta$name %in% "method_manifests"])))
  census <- rv_write_census(stats::setNames(man_dr$manifest$command, man_dr$manifest$name),
                            written, ws_rel, c(examined = examined, live = live, copy = copy))
  rd <- census$class %in% "path written by a computed file target" &
    sub("/+$", "", sub("^\\./", "", census$literal)) %in% reads
  census_reads <- census[rd, , drop = FALSE]
  census <- census[!rd, , drop = FALSE]
  if (nrow(census))
    stop("review: write census hit: target ", census$target[1], " literal '", census$literal[1],
         "' (", census$class[1], "; ", nrow(census), " hit(s)); Decision 8 (a)", call. = FALSE)
  expected <- rv_expected_errors(copy, analysis_id, decl$tasks)
  edges <- man_dr$edges
  reach <- function(start, from, to) {
    seen <- character(0); frontier <- start
    repeat {
      nxt <- setdiff(from[to %in% frontier], c(seen, start))
      if (!length(nxt)) break
      seen <- c(seen, nxt); frontier <- nxt
    }
    seen
  }
  anc <- reach(expected, edges$from, edges$to)
  desc <- reach(expected, edges$to, edges$from)
  outside <- setdiff(man_dr$manifest$name, c(anc, expected, desc))
  # Normalization (Decision 13): the workspace prefix (absolute and relative)
  # and the copy root map to the examined root, the fresh store to the
  # analysis store; the declared re: patterns become one token on both sides.
  from <- c(file.path(copy, ws_rel, "store"), file.path(ws_rel, "store"),
            paste0(copy, "/", ws_rel, "/"), paste0(ws_rel, "/"), copy)
  to <- c(file.path(examined, rb$store), rb$store, paste0(examined, "/"), "", examined)
  spec <- list(fixed = stats::setNames(to, from),
               patterns = sub("^re:", "", grep("^re:", names(rb$volatile), value = TRUE)),
               token = "<volatile>", counter = new.env())
  record <- data.frame(quantity = c(
    "copy root (PMX_REVIEW_COPY)", "examined root", "live root",
    "HEAD from the checkout receipt", "the copy's own HEAD", "copy HEAD equal to the receipt's",
    "the copy's porcelain line count (recorded, not judged)",
    "census roots of the copy equal to the examined root at setup",
    "REPO_ROOT unset", "NONMEM_EXTRA_MOUNTS set", "inert input classes in force",
    "inputs copied into the workspace", "inputs copied before the derived composition",
    "inert-class inputs holding a root path",
    "declared tasks composed", "runner derived from the committed declaration",
    "SOURCE_DATE_EPOCH", "expected errored set (unsigned gates in the copy at setup)",
    "ancestors of the expected errored set", "targets outside the ancestors and descendants",
    "pdftotext version"),
    value = c(copy, examined, live, paste(receipt_head, collapse = "; "), ch$head,
              as.character(head_equal), as.character(ch$porcelain_lines),
              as.character(cc$roots_equal), as.character(!nzchar(Sys.getenv("REPO_ROOT"))),
              as.character(cc$extra_mounts_set), paste(rv_input_classes(), collapse = ", "),
              as.character(nrow(inputs)), as.character(nrow(precopy)), as.character(nrow(inert)),
              paste(decl$tasks, collapse = ", "), decl_file, epoch,
              paste(expected, collapse = ", "), as.character(length(anc)),
              as.character(length(outside)),
              tryCatch(suppressWarnings(system2("pdftotext", "-v", stdout = TRUE, stderr = TRUE))[1],
                       error = function(e) "unavailable")),
    stringsAsFactors = FALSE)
  if (nrow(inert)) record <- rbind(record, data.frame(
    quantity = paste0("inert-class input holding the ", inert$root, "'s path (fact, not refused)"),
    value = paste0(inert$file, " (", inert$class, ")"), stringsAsFactors = FALSE))
  if (nrow(census_reads)) record <- rbind(record, data.frame(
    quantity = paste0("write census: tracked input, read (fact, not a stop): target ", census_reads$target),
    value = census_reads$literal, stringsAsFactors = FALSE))
  out <- list(analysis_id = analysis_id, examined = examined, live = live, copy = copy,
              ws_rel = ws_rel, ws = ws, decl = decl, derived = der, roots = roots,
              exclude = exclude, tree_before = tree_before, inputs = inputs, precopy = precopy,
              inert = inert, census_reads = census_reads, expected_errors = expected, edges = edges,
              ancestors = anc, outside = outside, epoch = epoch, spec = spec,
              volatile = rb$volatile, expected_changes = rb$expected_changes,
              store = rb$store, fresh_store = file.path(copy, ws_rel, "store"),
              report_dir = der$inputs$reporting$report_dir, record = record)
  if (dry) {
    # Whitespace is dropped on both sides: the manifest deparses each command
    # and wraps it by width, so a longer path literal moves a line break.
    norm <- function(x) gsub("[[:space:]]+", "", rv_normalize(x, list(fixed = spec$fixed)))
    ce <- stats::setNames(norm(man_ex$manifest$command), man_ex$manifest$name)
    cd <- stats::setNames(norm(man_dr$manifest$command), man_dr$manifest$name)
    names_equal <- setequal(names(ce), names(cd))
    out$summary <- list(
      examined_root = examined, live_root = live,
      census_roots_equal = cc$roots_equal, differing_paths = cc$differing,
      harness_yaml_in_copy = cc$harness_present, copy_head = ch$head,
      receipt_head = receipt_head, head_equal = head_equal,
      derived_store_identical = identical(der$inputs$reporting$store, der$store) &&
        identical(der$inputs$review$store, der$store),
      write_census_hits = nrow(census), inputs_copied = nrow(inputs),
      inputs_precopied = nrow(precopy),
      refused_class_hits_examined = sum(refused$root != "live root"),
      refused_class_hits_live = sum(refused$root != "examined root"),
      inert_class_hits = inert, ancestors = length(anc), outside = length(outside),
      names_equal = names_equal,
      commands_equal = names_equal && identical(ce[sort(names(ce))], cd[sort(names(ce))]),
      tree_unchanged = identical(tree_before, rv_tree_digest(examined, roots, exclude)),
      expected_errors = expected)
  }
  out
}

# The spawn of Decision 11 with the command string from rv_spawn_command; the
# child's exit status and the fresh store's metadata (read in a subprocess).
rv_reproduce_run <- function(setup) {
  cmd <- rv_spawn_command(setup$copy, "tar_make", setup$ws_rel)
  start <- Sys.time()
  status <- suppressWarnings(system2("sh", c("-c", shQuote(cmd))))
  end <- Sys.time()
  meta <- tryCatch(rv_store_meta(setup$fresh_store), error = function(e)
    data.frame(name = character(0), type = character(0), error = character(0),
               stringsAsFactors = FALSE))
  list(exit_status = as.integer(status), meta = meta,
       log = file.path(setup$ws, "tar_make.log"),
       start = format(start, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
       end = format(end, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
}

# Pure: the outcome of Decision 12. Let A be the ancestors of the expected
# errored set in the derived network. The run is incomplete (a stop) when the
# exit status is not 1, or when no target outside the expected set errored and
# either an expected target did not error with "Gate not signed" or a target
# of A is neither built nor errored. Otherwise it is complete and the
# errored-set comparison applies; an unexpected errored target is the critical
# finding of Decision 17. `outside` (the targets outside the ancestors and
# descendants, from setup) is recorded built or not: the count not built.
rv_run_outcome <- function(exit_status, meta, expected, edges, outside = character(0)) {
  stems <- meta[meta$type %in% c("stem", "pattern"), , drop = FALSE]
  err <- !is.na(stems$error) & nzchar(stems$error)
  errored <- stems$name[err]
  built <- stems$name[!err]
  anc <- character(0); frontier <- expected
  repeat {
    nxt <- setdiff(edges$from[edges$to %in% frontier], c(anc, expected))
    if (!length(nxt)) break
    anc <- c(anc, nxt); frontier <- nxt
  }
  gate_msg <- stems$error[match(expected, stems$name)]
  gate_ok <- length(expected) > 0L && all(expected %in% errored) &&
    all(grepl("Gate not signed", gate_msg, fixed = TRUE))
  unexpected <- setdiff(errored, expected)
  unbuilt_a <- setdiff(anc, c(built, errored))
  incomplete <- !identical(as.integer(exit_status), 1L) ||
    (!length(unexpected) && (!gate_ok || length(unbuilt_a) > 0L))
  outcome <- if (incomplete) "incomplete" else "complete"
  equal <- !incomplete && setequal(errored, expected)
  first <- if (length(unexpected)) unexpected[1] else NA_character_
  outcomes <- if (!incomplete && !equal)
    rv_outcome("errored_set", "procedure step 2",
               paste0("the errored set of the fresh run differs from the expected set; ",
                      "unexpected: ", paste(unexpected, collapse = ", "), "; expected not errored: ",
                      paste(setdiff(expected, errored), collapse = ", "),
                      "; ancestors left unbuilt: ", length(unbuilt_a)),
               "reproduction-record.csv",
               paste0("owning stage of ", first)) else rv_outcome()
  record <- data.frame(quantity = c(
    "child exit status", "outcome (complete or incomplete)", "expected errored set",
    "observed errored set", "errored set equals expected", "targets built without error",
    "ancestors of the expected set neither built nor errored",
    "targets outside the ancestors and descendants not built"),
    value = c(as.character(exit_status), outcome, paste(expected, collapse = ", "),
              paste(errored, collapse = ", "), as.character(equal), as.character(length(built)),
              as.character(length(unbuilt_a)), as.character(length(setdiff(outside, built)))),
    stringsAsFactors = FALSE)
  list(outcome = outcome, complete = !incomplete, equal = equal, errored = errored,
       expected = expected, unexpected = unexpected, unbuilt_ancestors = unbuilt_a,
       ancestors = anc, record = record, outcomes = outcomes)
}

# ---------------------------------------------------------------------------
# Comparison (Decisions 13 and 14)
# ---------------------------------------------------------------------------

# Type-aware, idempotent normalization: character data has the fixed
# replacements of spec$fixed applied longest first (names are the from-strings)
# and every Perl pattern of spec$patterns replaced by spec$token, preceded by
# the pattern's first capture group when it has one (so the stamp pattern's
# tool name, bootstrap or vpc, survives; N-3); lists and data frames recurse
# with class and attributes kept; every other type is returned untouched.
# When spec$counter is given, the elements (cells, lines, names) a pattern
# changed are counted in it; callers pass it on one side only (m-6).
# lines 719-779); the relative ../ variants are not needed because every
# fresh path is relative to the copy root, the fresh run's working directory.
rv_normalize <- function(x, spec) {
  if (is.list(x)) {
    if (length(x)) x[] <- lapply(x, rv_normalize, spec = spec)
    return(x)
  }
  if (!is.character(x)) return(x)
  fx <- spec$fixed
  if (length(fx)) for (i in order(-nchar(names(fx)))) {
    if (nzchar(names(fx)[i])) x <- gsub(names(fx)[i], fx[[i]], x, fixed = TRUE)
  }
  token <- if (is.null(spec$token)) "<volatile>" else spec$token
  for (p in spec$patterns) {
    if (is.environment(spec$counter)) {
      n <- sum(grepl(p, x, perl = TRUE))
      prev <- if (exists(p, envir = spec$counter, inherits = FALSE)) get(p, envir = spec$counter) else 0
      assign(p, prev + n, envir = spec$counter)
    }
    grouped <- grepl("(", gsub("\\\\.|[(][?]", "", p), fixed = TRUE)
    x <- gsub(p, if (grouped) paste0("\\1", token) else token, x, perl = TRUE)
  }
  x
}

# One comparison row: exact equality, numeric maximum absolute difference and
# a detail with the first differing pairs (up to 5). Typed notes compare
# element by element (finding N1). Ported from review_compare_value,
# numeric tolerance 1e-8 of the signed a prior analysis comparator is kept.
# ponytail: one absolute tolerance for every store value, disclosed in the
# package's method note; per-quantity tolerances only if the SME asks.
rv_compare_value <- function(item, a, b, ignore = character(0)) {
  row <- function(match, exact, max_diff, detail)
    data.frame(item = item, match = match, exact = exact, max_abs_diff = max_diff,
               detail = detail, stringsAsFactors = FALSE)
  preview <- function(x, y, n = 5L, width = 60L) {
    cx <- as.character(x); cy <- as.character(y)
    d <- which(is.na(cx) != is.na(cy) | (!is.na(cx) & !is.na(cy) & cx != cy))
    d <- utils::head(d, n)
    tr <- function(v) ifelse(is.na(v), "NA", ifelse(nchar(v) > width,
                                                     paste0(substr(v, 1, width), "..."), v))
    paste0("{", paste0("'", tr(cx[d]), "' became '", tr(cy[d]), "'", collapse = "; "), "}")
  }
  is_notes <- function(x) {
    if (!is.list(x) || is.data.frame(x) || !length(x)) return(FALSE)
    ok <- vapply(x, function(e) (is.character(e) && length(e) == 1L) ||
                   (is.list(e) && is.character(e$kind) && length(e$kind) == 1L), logical(1))
    typed <- vapply(x, function(e) is.list(e) && is.character(e$kind) && length(e$kind) == 1L &&
                      e$kind %in% c("authored", "evidence"), logical(1))
    all(ok) && any(typed)
  }
  note_id <- function(note, i) {
    if (!is.list(note)) return(sprintf("note[%d] (plain)", i))
    kind <- if (is.character(note$kind) && length(note$kind) == 1L) note$kind else "untyped"
    if (is.character(note$label) && length(note$label) == 1L)
      sprintf("note[%d] (%s: %s)", i, kind, note$label) else sprintf("note[%d] (%s)", i, kind)
  }
  if (is.data.frame(a) && is.data.frame(b)) {
    a <- a[, setdiff(names(a), ignore), drop = FALSE]
    b <- b[, setdiff(names(b), ignore), drop = FALSE]
    if (!identical(dim(a), dim(b)) || !identical(names(a), names(b)))
      return(row(FALSE, FALSE, NA_real_, sprintf("structure mismatch: %dx%d vs %dx%d",
                                                 nrow(a), ncol(a), nrow(b), ncol(b))))
    rownames(a) <- NULL; rownames(b) <- NULL
    max_diff <- 0
    mism <- character(0)
    for (nm in names(a)) {
      x <- a[[nm]]; y <- b[[nm]]
      if (is.numeric(x) && is.numeric(y)) {
        d <- abs(x - y)
        d[(is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & x == y)] <- 0   # equal infinities: Inf - Inf is NaN
        if (anyNA(d)) { mism[nm] <- paste0(nm, " ", preview(x, y)); next }
        max_diff <- max(max_diff, d)
        if (any(d > 1e-8)) mism[nm] <- paste0(nm, " ", preview(x, y))
      } else if (is.list(x) || is.list(y)) {
        if (!identical(x, y)) mism[nm] <- paste0(nm, " (list column differs)")
      } else if (!identical(as.character(x), as.character(y))) {
        mism[nm] <- paste0(nm, " ", preview(x, y))
      }
    }
    match <- !length(mism)
    return(row(match, identical(a, b), max_diff,
               if (match) sprintf("%d rows, %d cols compared%s", nrow(a), ncol(a),
                                  if (length(ignore)) paste0(" (ignored: ",
                                                             paste(ignore, collapse = ", "), ")") else "")
               else paste("mismatch in:", paste(mism, collapse = " | "))))
  }
  if (is_notes(a) && is_notes(b)) {
    if (length(a) != length(b))
      return(row(FALSE, FALSE, NA_real_, sprintf("note-count mismatch: %d notes vs %d notes",
                                                 length(a), length(b))))
    diffs <- character(0)
    for (i in seq_along(a)) {
      if (identical(a[[i]], b[[i]])) next
      x <- a[[i]]; y <- b[[i]]
      fd <- if (is.list(x) && is.list(y)) {
        d <- character(0)
        for (fn in union(names(x), names(y))) {
          fx <- if (is.null(x[[fn]])) NA_character_ else as.character(x[[fn]])
          fy <- if (is.null(y[[fn]])) NA_character_ else as.character(y[[fn]])
          if (identical(fx, fy)) next
          d <- c(d, if (length(fx) == length(fy)) paste0(fn, " ", preview(fx, fy))
                 else sprintf("%s (length %d vs %d)", fn, length(fx), length(fy)))
        }
        if (length(d)) paste(d, collapse = ", ") else "structure differs"
      } else preview(as.character(x), as.character(y))
      diffs <- c(diffs, paste0(note_id(x, i), ": ", fd))
    }
    match <- !length(diffs)
    return(row(match, identical(a, b), NA_real_,
               if (match) sprintf("%d typed notes compared", length(a))
               else paste("mismatch in:", paste(utils::head(diffs, 5L), collapse = " | "))))
  }
  if (is.numeric(a) && is.numeric(b) && length(a) == length(b) && length(a)) {
    d <- abs(a - b)
    d[(is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)] <- 0   # equal infinities: Inf - Inf is NaN
    if (anyNA(d)) return(row(FALSE, FALSE, NA_real_, paste("numeric mismatch", preview(a, b))))
    md <- max(d)
    return(row(md <= 1e-8, identical(a, b), md,
               if (md <= 1e-8) sprintf("length %d", length(a)) else paste("numeric mismatch", preview(a, b))))
  }
  same <- identical(a, b)
  row(same, same, NA_real_,
      if (same) paste("class", class(a)[1])
      else if (is.character(a) && is.character(b) && length(a) == length(b)) paste("mismatch", preview(a, b))
      else sprintf("mismatch: class %s length %s vs class %s length %s",
                   class(a)[1], length(a), class(b)[1], length(b)))
}

# The store comparison (Decision 13 (a) to (d)): every *_package present in
# either store except review_package, the dataset, every promoted target, and
# the environment attestation's fingerprint and probe. Both sides pass through
# the same rv_normalize; the volatile names are dropped by exact column or
# element name; the md5 and bytes cells of the code inventory rows whose file
# is in expected_changes are blanked on both sides. `analysis` and `fresh` are
# store paths or snapshots from rv_store_values. Returns the comparison table,
# the "Excluded from comparison" table, the outcome rows and the snapshots.
# In the excluded table each expected change's `removed` is the number of
# analysis-side rows of its own file that were blanked (R-9), not the total.
rv_compare_stores <- function(analysis, fresh, spec, volatile = character(0),
                              expected_changes = character(0)) {
  sa <- if (is.character(analysis)) rv_store_values(analysis) else analysis
  sb <- if (is.character(fresh)) rv_store_values(fresh) else fresh
  vn <- names(volatile)[!grepl("^(re|file|column):", names(volatile))]
  removed <- stats::setNames(integer(length(vn)), vn)
  blanked <- integer(length(expected_changes))
  drop <- function(x, count) {
    if (is.data.frame(x)) {
      hit <- intersect(names(x), vn)
      if (count) for (h in hit) removed[h] <<- removed[h] + nrow(x)
      if (length(hit)) x <- x[, setdiff(names(x), hit), drop = FALSE]
      if (all(c("file", "md5") %in% names(x)) && length(expected_changes)) {
        ex <- x$file %in% names(expected_changes)
        if (count) blanked <<- blanked + vapply(names(expected_changes),
                                                function(f) sum(x$file %in% f), integer(1))
        x$md5[ex] <- NA
        if ("bytes" %in% names(x)) x$bytes[ex] <- NA
      }
      return(x)
    }
    if (is.list(x) && length(x)) {
      if (!is.null(names(x))) {
        hit <- intersect(names(x), vn)
        if (count) for (h in hit) removed[h] <<- removed[h] + 1L
        if (length(hit)) x <- x[!names(x) %in% hit]
      }
      if (length(x)) x[] <- lapply(x, drop, count = count)
    }
    x
  }
  # The re: counts come from the analysis side only (count = TRUE), once per value.
  quiet <- spec; quiet$counter <- NULL
  prep <- function(x, count) drop(rv_normalize(x, if (count) spec else quiet), count)
  stage_of <- function(pk) paste0("owning stage: ", sub("_package$", "", pk))
  rows <- list()
  outs <- list()
  add <- function(r, owner, rule = "comparison_mismatch") {
    rows[[length(rows) + 1L]] <<- r
    if (isFALSE(r$match))
      outs[[length(outs) + 1L]] <<- rv_outcome(rule, "procedure step 3",
                                               paste0("reproduction comparison mismatch: ", r$item),
                                               "reproduction-comparison.csv", owner)
  }
  # R-3: a value that is NULL in either snapshot was not read (rv_store_values
  # keeps its message in the attribute "read_errors") or is absent, and a
  # compared field NULL in both readable values is absent. Either row is a
  # mismatch quoting each side's reason; package fields compare as before.
  unread <- function(item, nm, absent_both = FALSE) {
    why <- function(s, side) {
      if (!is.null(s[[nm]])) return(NULL)
      e <- attr(s, "read_errors")
      paste0(side, ": ", if (!is.na(nm) && nzchar(nm) && nm %in% names(e)) e[[nm]] else "absent or NULL")
    }
    w <- c(why(sa, "analysis"), why(sb, "fresh"))
    if (!length(w) && absent_both) w <- c("analysis: absent or NULL", "fresh: absent or NULL")
    if (length(w)) data.frame(item = item, match = FALSE, exact = FALSE, max_abs_diff = NA_real_,
                              detail = paste0("absent or unreadable in one of the stores (",
                                              paste(w, collapse = "; "), ")"), stringsAsFactors = FALSE)
  }
  # Both sides are prepared first, so the analysis side's volatile counts do
  # not depend on whether the fresh side was read.
  vrow <- function(item, nm, x, y) {
    force(x); force(y)
    u <- unread(item, nm, is.null(x) && is.null(y))
    if (is.null(u)) rv_compare_value(item, x, y) else u
  }
  pk_a <- grep("_package$", names(sa), value = TRUE)
  pk_b <- grep("_package$", names(sb), value = TRUE)
  for (pk in sort(union(pk_a, pk_b))) {
    if (identical(pk, "review_package")) {
      rows[[length(rows) + 1L]] <- data.frame(
        item = "review_package (review task package)", match = NA, exact = NA,
        max_abs_diff = NA_real_, detail = paste(
          "not compared - excluded by design: holds the reproduction record, stubbed",
          "inside a reproduction (recursion guard, R12)"), stringsAsFactors = FALSE)
      next
    }
    a <- sa[[pk]]; b <- sb[[pk]]
    if (!is.null(u <- unread(pk, pk))) {
      add(u, stage_of(pk))
      next
    }
    a <- prep(a, TRUE); b <- prep(b, FALSE)
    add(rv_compare_value(paste0(pk, ": identity (analysis, task)"),
                         c(a$analysis, a$task), c(b$analysis, b$task)), stage_of(pk))
    add(rv_compare_value(paste0(pk, ": table-name set"), names(a$tables), names(b$tables)),
        stage_of(pk))
    add(rv_compare_value(paste0(pk, ": notes"), a$notes, b$notes), stage_of(pk))
    add(rv_compare_value(paste0(pk, ": artifacts"), a$artifacts, b$artifacts), stage_of(pk))
    for (tn in intersect(names(a$tables), names(b$tables)))
      add(rv_compare_value(paste0(pk, ": tables[", tn, "]"), a$tables[[tn]], b$tables[[tn]]),
          stage_of(pk))
  }
  add(vrow("dataset (analysis-ready data, all cells)", "dataset",
            prep(sa[["dataset"]], TRUE), prep(sb[["dataset"]], FALSE)),
      "owning stage: data_assembly")
  promoted <- sa[["reporting_package"]]$tables[["Promoted values and their sources"]]$target
  for (nm in setdiff(promoted, c(pk_a, "dataset")))
    add(vrow(paste0("promoted target ", nm), nm, prep(sa[[nm]], TRUE),
             prep(sb[[nm]], FALSE)), "owning stage: reporting")
  ea <- sa[["environment_attestation"]]; eb <- sb[["environment_attestation"]]
  add(vrow("environment_attestation: fingerprint", "environment_attestation",
            ea$fingerprint, eb$fingerprint), "root (environment)", "attestation")
  add(vrow("environment_attestation: probe", "environment_attestation",
            prep(ea$probe, TRUE), prep(eb$probe, FALSE)), "root (environment)", "attestation")
  table <- do.call(rbind, rows)
  rownames(table) <- NULL
  # ponytail: re: counts cover store cells only; count file lines and names in
  # rv_compare_files and merge them here if the SME asks for them.
  re <- grep("^re:", names(volatile), value = TRUE)
  re_count <-vapply(sub("^re:", "", re), function(p)
    if (is.environment(spec$counter) && exists(p, envir = spec$counter, inherits = FALSE))
      as.integer(get(p, envir = spec$counter)) else 0L, integer(1))
  excluded <- data.frame(
    entry = c(vn, re, names(expected_changes)),
    kind = c(rep("volatile, exact column or element name", length(vn)),
             rep(paste("volatile, re: pattern replaced by one token on both sides (removed: the",
                       "analysis store cells it changed; file lines and names are not counted)"), length(re)),
             rep("expected change: md5 and bytes of its code inventory row", length(expected_changes))),
    reason = c(unname(volatile[vn]), unname(volatile[re]), unname(expected_changes)),
    removed = c(unname(removed), unname(re_count), unname(blanked)),
    stringsAsFactors = FALSE)
  list(table = table, excluded = excluded,
       outcomes = if (length(outs)) do.call(rbind, outs) else rv_outcome(),
       snapshots = list(analysis = sa, fresh = sb))
}

# (Recommended)"). kind "pdf": `a` and `b` are pdftotext -layout outputs as
# lines (pages separated by form feeds). (i) equal page counts; (ii) the
# multiset of decimal tokens identical; (iii) after collapsing runs of spaces
# either no line differs, or the differing lines form one block of at most two
# adjacent lines on each side, on one page, whose words with the four legend
# keys removed are identical in order and in which each key appears once on
# both sides. kind "deck": `a` and `b` are lists, one character vector of
# <a:t> text runs per slide; equal slide counts and identical runs per slide.
rv_text_compare <- function(a, b, kind = c("pdf", "deck")) {
  kind <- match.arg(kind)
  if (kind == "deck") {
    same_n <- length(a) == length(b)
    same <- length(a) > 0L && length(b) > 0L && same_n && all(mapply(identical, a, b))
    return(data.frame(rule = "deck: slide text runs identical per slide",
                      pages_a = length(a), pages_b = length(b), decimals_equal = NA,
                      differing_lines_a = NA_integer_, differing_lines_b = NA_integer_,
                      page = NA_integer_, outcome = same, stringsAsFactors = FALSE))
  }
  pages <- function(x) {
    p <- strsplit(paste(x, collapse = "\n"), "\f", fixed = TRUE)[[1]]
    if (length(p) && !nzchar(trimws(p[length(p)]))) p <- p[-length(p)]
    lapply(p, function(s) gsub(" +", " ", strsplit(s, "\n", fixed = TRUE)[[1]]))
  }
  pa <- pages(a); pb <- pages(b)
  dec <- function(x) sort(unlist(regmatches(x, gregexpr("[0-9]+([.][0-9]+)?", x))))
  dec_ok <- identical(dec(unlist(pa)), dec(unlist(pb)))
  same_pages <- length(pa) == length(pb)
  na <- NA_integer_; nb <- NA_integer_; pg <- NA_integer_
  block_ok <- FALSE
  if (same_pages) {
    diffp <- which(!mapply(identical, pa, pb))
    if (!length(diffp)) {
      block_ok <- TRUE; na <- 0L; nb <- 0L
    } else if (length(diffp) == 1L) {
      pg <- diffp
      x <- pa[[pg]]; y <- pb[[pg]]
      pre <- 0L
      while (pre < min(length(x), length(y)) && identical(x[pre + 1L], y[pre + 1L])) pre <- pre + 1L
      suf <- 0L
      while (suf < min(length(x), length(y)) - pre &&
             identical(x[length(x) - suf], y[length(y) - suf])) suf <- suf + 1L
      ba <- x[seq.int(pre + 1L, length.out = length(x) - pre - suf)]
      bb <- y[seq.int(pre + 1L, length.out = length(y) - pre - suf)]
      na <- length(ba); nb <- length(bb)
      keys <- c("Point estimate", "Confidence interval", "Reference subject", "Reference area")
      ta <- paste(ba, collapse = " "); tb <- paste(bb, collapse = " ")
      count <- function(t, k) sum(gregexpr(k, t, fixed = TRUE)[[1]] > 0L)
      keys_once <- all(vapply(keys, function(k) count(ta, k) == 1L && count(tb, k) == 1L, logical(1)))
      words <- function(t) {
        for (k in keys) t <- gsub(k, " ", t, fixed = TRUE)
        w <- strsplit(trimws(t), " +")[[1]]
        w[nzchar(w)]
      }
      block_ok <- na %in% 1:2 && nb %in% 1:2 && keys_once && identical(words(ta), words(tb))
    }
  }
  data.frame(rule = "R-F11: pages, decimal multiset, one legend block of at most two lines",
             pages_a = length(pa), pages_b = length(pb), decimals_equal = dec_ok,
             differing_lines_a = na, differing_lines_b = nb, page = pg,
             outcome = length(a) > 0L && length(b) > 0L && same_pages && dec_ok && block_ok,
             stringsAsFactors = FALSE)
}

# The declared upstream files of Decision 13 (e): every string leaf of the
# declared inputs that lies under a remapped location (`locations`), is not
# itself a location value and holds no whitespace. NA and empty leaves are
# dropped first: a declared NA is no path (M-1).
rv_upstream_files <- function(inputs, locations) {
  leaves <- as.character(unlist(inputs, use.names = FALSE))
  leaves <- leaves[!is.na(leaves) & nzchar(leaves)]
  lf <- rv_location_fields(inputs)
  unique(leaves[!leaves %in% lf$value & !grepl("[[:space:]]", leaves) &
                  vapply(leaves, function(x) any(startsWith(x, paste0(locations, "/"))), logical(1))])
}

# The column: entries of the review declaration's volatile vector
# (`column:<column name>@<file pattern>`, split at the first @; owner ruling
# in the order of `volatile`: entry, column, pattern. Stops, naming the entry,
# when the @ is missing, the column name or the pattern is empty, or the
# pattern is not a valid Perl regular expression anchored with ^. The review
# module's compose-time check and rv_compare_files both read entries here.
rv_volatile_columns <- function(volatile) {
  e <- grep("^column:", names(volatile), value = TRUE)
  body <- substring(e, 8L)
  col <- sub("@.*$", "", body)
  pat <- sub("^[^@]*@", "", body)
  for (i in seq_along(e)) {
    why <- if (!grepl("@", body[i], fixed = TRUE)) "has no '@' between the column name and the file pattern"
      else if (!nzchar(col[i])) "has an empty column name"
      else if (!nzchar(pat[i])) "has an empty file pattern"
      else if (!startsWith(pat[i], "^")) "has a file pattern not anchored with ^"
      else if (inherits(tryCatch(suppressWarnings(grepl(pat[i], "", perl = TRUE)), error = function(x) x), "error"))
        "has a file pattern that is not a valid Perl regular expression"
    if (!is.null(why)) stop("volatile: entry '", e[i], "' ", why, call. = FALSE)
  }
  data.frame(entry = as.character(e), column = col, pattern = pat, stringsAsFactors = FALSE)
}

# Masks the declared columns of one side of a CSV-shaped file (plan D-1, D-4).
# `lines` are the raw lines, `cols` distinct column names. A line splits at
# commas; a double quote opens a quoted field only as the first character of
# a field, and a quoted field may hold commas and doubled quotes; each field
# keeps its raw text. A line with an odd number of quotes, or a quoted field
# not followed by a comma or the line end, has no field count. A header line
# for a column holds a field equal to it after trimws and the removal of one
# pair of surrounding quotes; every such position is excluded. Its body is
# the lines after it up to, not including, the first line that is empty, has
# another field count or none, or is itself a header line for the column. In
# each body line the excluded fields become <volatile>; every other byte is
# kept, header lines included. Returns the lines and, per column, the header
# lines found and the cells replaced.
rv_mask_columns <- function(lines, cols) {
  counts <- data.frame(column = as.character(cols), header_lines = integer(length(cols)),
                       cells = integer(length(cols)), stringsAsFactors = FALSE)
  if (!length(cols)) return(list(lines = lines, counts = counts))
  split <- function(l) {
    if (lengths(regmatches(l, gregexpr("\"", l, fixed = TRUE))) %% 2L) return(NULL)
    f <- character(0)
    repeat {
      m <- regmatches(l, regexpr("^(\"([^\"]|\"\")*\"|[^,\"][^,]*|)(,|$)", l, perl = TRUE))
      if (!length(m)) return(NULL)
      more <- endsWith(m, ",")
      f <- c(f, if (more) substr(m, 1L, nchar(m) - 1L) else m)
      if (!more) return(f)
      l <- substring(l, nchar(m) + 1L)
    }
  }
  fl <- lapply(lines, split)
  done <- logical(length(lines))
  for (k in seq_along(cols)) {
    pos <- lapply(fl, function(f) which(sub("^\"(.*)\"$", "\\1", trimws(f)) == cols[k]))
    head <- lengths(pos) > 0L
    counts$header_lines[k] <- sum(head)
    # ponytail: two ceilings. (1) An adjacent section of the same field count
    # with no separator line is read as body. (2) A body cell whose text equals
    # the column name starts a second header, so later cells are masked at that
    # line's index; the entry row then counts one more header line on that
    # side, which is the evidence (code review CR1-2). Upgrade path if a
    # declared file has either shape: an explicit body-line limit per entry,
    # or a header line that may not follow a body line of its own section.
    for (h in which(head)) {
      n <- length(fl[[h]])
      j <- h + 1L
      while (j <= length(lines) && nzchar(lines[j]) && length(fl[[j]]) == n && !head[j]) {
        fl[[j]][pos[[h]]] <- "<volatile>"
        counts$cells[k] <- counts$cells[k] + length(pos[[h]])
        done[j] <- TRUE
        j <- j + 1L
      }
    }
  }
  lines[done] <- vapply(fl[done], paste, character(1), collapse = ",")
  list(lines = lines, counts = counts)
}

# Wrap-aware volatile patterns for PDF text (BETA-QUEUE after-close row 2,
# pdftotext lines after rv_normalize; reads spec$patterns and spec$token. For
# each pattern in declared order and each adjacent pair of lines (i, i+1),
# unless line i+1 as it came in holds a form feed (a span never crosses a
# page): line i without trailing spaces and line i+1 without leading spaces
# are joined by one space, and a match that starts on line i and ends at
# least one character into line i+1 is replaced as rv_normalize replaces it
# (group 1 when the pattern has one, then the token); line i keeps the text
# before the match, line i+1 its leading spaces and the text after the match,
# so the number of lines is unchanged. Guard G-1: a join whose pattern also
# matches line i alone at the same start is made but marked, and after the
# last pattern every run of lines connected by joins that holds a marked join
# is put back to its input lines and not counted; so each output line is the
# line of the same helper without G-1, or the input line. Returns the lines
# and the joins counted per pattern (named by the pattern, in order).
# ponytail: ceilings (D-3). (1) A break inside a token (no space at the wrap)
# is not joined. (2) A span over two or more breaks is not joined. (3) A
# match wholly on one line whose lookaround text lies on the other, or that
# begins at the fold, is not joined. (4) Sides wrapped at different points
# show in the per-side counts; the outcome follows the remaining text. (5)
# The helper cannot tell a layout wrap from a real line break: (5a) the head
# of line i+1 that the pattern's tail accepts is removed either way; (5b) an
# open tail takes line i+1 up to its delimiter or whole; (5c) the fold is one
# space where the layout held several; (5d) the joins run after every
# one-line replacement, so overlapping patterns can give another result than
# the unwrapped text. (6) A wrap after a match that line i already completes
# is not joined and fails closed as today; G-1 puts back the whole run of
# such a join. Upgrade paths, each only when a measured case or the SME needs
# it: a second join with the empty string; a join of up to N lines; accepting
# a match that ends on line i when the pattern does not match line i alone;
# a per-entry opt-in that limits the reach of (5) to named patterns; for
# (5d), normalizing and joining pattern by pattern.
rv_join_volatile <- function(lines, spec) {
  token <- if (is.null(spec$token)) "<volatile>" else spec$token
  pats <- as.character(spec$patterns)
  input <- lines
  n <- length(lines)
  page <- grepl("\f", input, fixed = TRUE)   # read on the input lines only (D-2 item 1)
  link <- veto <- logical(n)                 # link[i]: lines i and i+1 joined by some pattern
  at <- by <- integer(0)                     # line i and pattern index of each join made
  for (q in seq_along(pats)) {
    p <- pats[q]
    grouped <- grepl("(", gsub("\\\\.|[(][?]", "", p), fixed = TRUE)
    for (i in seq_len(max(0L, n - 1L))) {
      if (page[i + 1L]) next
      a <- sub(" +$", "", lines[i]); b <- sub("^ +", "", lines[i + 1L])
      j <- paste(a, b)
      m <- gregexpr(p, j, perl = TRUE)[[1L]]
      st <- as.integer(m); en <- st + attr(m, "match.length") - 1L
      k <- which(st > 0L & st <= nchar(a) & en >= nchar(a) + 2L)
      if (!length(k)) next
      k <- k[1L]
      if (st[k] %in% gregexpr(p, a, perl = TRUE)[[1L]]) veto[i] <- TRUE
      cs <- attr(m, "capture.start")
      g <- if (!grouped || is.null(cs)) "" else
        substr(j, cs[k, 1L], cs[k, 1L] + attr(m, "capture.length")[k, 1L] - 1L)
      lead <- strrep(" ", nchar(lines[i + 1L]) - nchar(b))
      lines[i] <- paste0(substr(j, 1L, st[k] - 1L), g, token)
      lines[i + 1L] <- paste0(lead, substring(j, en[k] + 1L))
      link[i] <- TRUE; at <- c(at, i); by <- c(by, q)
    }
  }
  run <- cumsum(c(TRUE, !link))[seq_len(n)]
  back <- run %in% run[veto]
  lines[back] <- input[back]
  list(lines = lines, counts = stats::setNames(tabulate(by[!back[at]], length(pats)), pats))
}

# The file comparison (Decision 13 (e) and (f)). (e) each declared upstream
# path (`upstream`, relative) in the copy against <copy>/<ws>/<path>. (f) every
# artifact row of every package of the analysis snapshot whose cell names a
# path under a remapped location, or whose fresh row of the same label lies
# under the workspace (the forest figure; M-2), paired with the fresh row of the same
# package and label (by the normalized cell when a label is not unique);
# each cell expanded to its files by the gate's own .gate_expand_artifact_cell
# (tools/gate.R line 173) with the working directory set to that side's root;
# files paired on normalized names and compared by extension class: .pdf by
# pdftotext text after rv_normalize (rv_text_compare for the names in `rf11`,
# identical text otherwise; F12), the PDF text first passed through
# rv_join_volatile, so a declared re: pattern also matches across one line
# break on one page, each such join counted per pattern and side in the row
# .pptx by slide text, the text classes line by line after rv_normalize (digits
# masked for names matching `masked`), every other extension by sha256. A
# `file:` volatile entry is listed with both digests and not judged. An
# unpaired row or file is a mismatch. A `column:` volatile entry masks its
# column (rv_mask_columns) on both sides of every text-class file whose
# normalized name matches its pattern, before rv_normalize and the digit
# masking; a file with a replaced cell says so in its class and counts in
# its detail, and each entry gets a summary row, a mismatch owned by the
# declaration when it replaced no cell (owner ruling Q2; AC1 plan 3.3).
# A .pdf outside `rf11` whose text is identical before normalization is also
# ponytail: the R-F11 pair, and a PDF whose text matches only after
# normalization, compare by text alone, so a purely visual change there
# passes (disclosed in the review package notes); upgrade path: rasters
# with the volatile regions masked, if the SME asks for it.
rv_compare_files <- function(analysis, fresh, roots, locations, ws_rel, spec,
                             volatile = character(0), upstream = character(0),
                             rf11 = character(0),
                             masked = "-semantic-manifest[.]txt$") {
  text_classes <- c("csv", "tsv", "txt", "lst", "ext", "mod", "ctl", "yaml", "yml", "md",
                    "html", "tex", "log", "r", "qmd", "json", "cpp", "diff", "scss")
  vfiles <- sub("^file:", "", grep("^file:", names(volatile), value = TRUE))
  vcols <- rv_volatile_columns(volatile)
  # One row per column entry: matched files, then header lines and cells
  # replaced on the analysis and the fresh side.
  vtally <- matrix(0L, nrow(vcols), 5L, dimnames = list(NULL, c("files", "ha", "hb", "ca", "cb")))
  locations <- sub("/+$", "", locations)
  spec$counter <- NULL   # the excluded-table counts are the store comparison's (m-6)
  sha <- function(f) digest::digest(file = f, algo = "sha256")
  # One PDF side: list(lines, counts) of rv_join_volatile on the normalized
  # text, and raw, the text before normalization (AC7 D-1).
  pdf_lines <- function(f) {
    raw <- suppressWarnings(system2("pdftotext", c("-layout", shQuote(f), "-"), stdout = TRUE, stderr = FALSE))
    if (!is.null(attr(raw, "status"))) raw <- character(0)
    c(rv_join_volatile(rv_normalize(raw, spec), spec), list(raw = raw))
  }
  # One PDF side's page rasters (AC7 D-1): the sha256 of each page pdftoppm
  # renders in grey at `dpi`, in page order; NULL when pdftoppm exits
  # non-zero or renders no page, so a failure never reads equal. The
  # resolution is passed explicitly (SME ruling Q3 (a): 150 DPI, pdftoppm's
  # documented default), so a poppler release cannot move the bound.
  # ponytail: a whole document is rendered into tempdir() per side before
  # hashing; upgrade path: render and hash page by page with -f and -l if a
  # large PDF outside rf11 appears.
  dpi <- 150L
  rasters <- function(f) {
    root <- tempfile("rv-raster-")
    st <- suppressWarnings(system2("pdftoppm", c("-gray", "-r", dpi, shQuote(f), shQuote(root)),
                                   stdout = FALSE, stderr = FALSE))
    p <- list.files(dirname(root), paste0("^", basename(root), "-[0-9]+[.]pgm$"), full.names = TRUE)
    on.exit(unlink(p))
    if (!identical(as.integer(st), 0L) || !length(p)) return(NULL)
    vapply(p, sha, character(1), USE.NAMES = FALSE)
  }
  # The detail suffix (plan D-4): each pattern joined on either side, in
  # pattern order, with both sides' counts; empty when nothing was joined.
  joined <- function(ja, jb) {
    k <- which(ja$counts > 0L | jb$counts > 0L)
    if (!length(k)) return("")
    paste0("; joined across one line break, re:", names(ja$counts)[k], ": analysis ", ja$counts[k],
           ", fresh ", jb$counts[k], collapse = "")
  }
  slides <- function(f) {
    l <- suppressWarnings(system2("unzip", c("-Z1", shQuote(f)), stdout = TRUE, stderr = FALSE))
    if (!is.null(attr(l, "status"))) return(character(0))
    s <- grep("^ppt/slides/slide[0-9]+\\.xml$", l, value = TRUE)
    s <- s[order(as.integer(gsub("[^0-9]", "", basename(s))))]
    runs <- vector("list", length(s))
    for (i in seq_along(s)) {
      xml <- suppressWarnings(system2("unzip", c("-p", shQuote(f), shQuote(s[i])),
                                      stdout = TRUE, stderr = FALSE))
      if (!is.null(attr(xml, "status"))) return(character(0))
      xml <- paste(xml, collapse = "\n")
      m <- regmatches(xml, gregexpr("<a:t>[^<]*</a:t>", xml))[[1]]
      runs[[i]] <- gsub("^<a:t>|</a:t>$", "", m)
    }
    runs
  }
  rows <- list(); trows <- list()
  row <- function(item, file, class, match, detail)
    rows[[length(rows) + 1L]] <<- data.frame(item = item, file = file, class = class,
                                             match = match, detail = detail,
                                             stringsAsFactors = FALSE)
  compare_one <- function(item, name, fa, fb) {
    ext <- tolower(tools::file_ext(name))
    if (name %in% vfiles) {
      return(row(item, name, "file: volatile entry (listed, not judged)", NA,
                 paste0("analysis ", sha(fa), "; fresh ", sha(fb))))
    }
    if (ext == "pdf") {
      ja <- pdf_lines(fa); jb <- pdf_lines(fb)
      if (basename(name) %in% rf11) {
        r <- rv_text_compare(ja$lines, jb$lines, "pdf")
        trows[[length(trows) + 1L]] <<- cbind(data.frame(item = name, stringsAsFactors = FALSE), r)
        return(row(item, name, "pdf, R-F11 text rule", r$outcome,
                   paste0(sprintf("pages %d and %d; decimals equal %s; differing lines %s and %s",
                                  r$pages_a, r$pages_b, r$decimals_equal, r$differing_lines_a,
                                  r$differing_lines_b), joined(ja, jb))))
      }
      same <- identical(ja$lines, jb$lines)
      # AC7 D-1: text identical before normalization, so the pages are also
      # compared as rasters, without a tolerance. Text equal only after
      # normalization keeps the text-only class (disclosed in the notes).
      if (same && identical(ja$raw, jb$raw)) {
        ra <- rasters(fa); rb <- rasters(fb)
        ok <- !is.null(ra) && !is.null(rb) && identical(ra, rb)
        what <- if (is.null(ra) || is.null(rb)) "page rasters unavailable (pdftoppm failed or rendered no page)"
          else if (ok) sprintf("page rasters identical at %d DPI, %d pages", dpi, length(ra))
          else if (length(ra) != length(rb)) sprintf("page rasters differ: %d and %d pages", length(ra), length(rb))
          else sprintf("page rasters differ at %d DPI on %d of %d pages", dpi, sum(ra != rb), length(ra))
        return(row(item, name, "pdf, identical pdftotext text, pages and page rasters", ok,
                   paste0("text identical; ", what, joined(ja, jb))))
      }
      return(row(item, name, "pdf, identical pdftotext text and pages", same,
                 paste0(if (same) "text identical" else "text differs", joined(ja, jb))))
    }
    if (ext == "pptx") {
      r <- rv_text_compare(slides(fa), slides(fb), "deck")
      trows[[length(trows) + 1L]] <<- cbind(data.frame(item = name, stringsAsFactors = FALSE), r)
      return(row(item, name, "pptx, slide text runs", r$outcome,
                 sprintf("slides %d and %d; byte-identical %s (a fact, not a check)",
                         r$pages_a, r$pages_b, identical(sha(fa), sha(fb)))))
    }
    if (ext %in% text_classes) {
      k <- which(vapply(vcols$pattern, grepl, logical(1), x = name, perl = TRUE, USE.NAMES = FALSE))
      cols <- unique(vcols$column[k])
      ma <- rv_mask_columns(readLines(fa, warn = FALSE), cols)
      mb <- rv_mask_columns(readLines(fb, warn = FALSE), cols)
      for (i in k) {
        ci <- match(vcols$column[i], cols)
        vtally[i, ] <<- vtally[i, ] + c(1L, ma$counts$header_lines[ci], mb$counts$header_lines[ci],
                                        ma$counts$cells[ci], mb$counts$cells[ci])
      }
      la <- rv_normalize(ma$lines, spec)
      lb <- rv_normalize(mb$lines, spec)
      cls <- "text, line by line after normalization"
      if (grepl(masked, name)) {
        la <- gsub("[0-9]", "d", la); lb <- gsub("[0-9]", "d", lb)
        cls <- "text, line by line with digits masked"
      }
      # Appended, never replaced, so a semantic manifest keeps its text_rule routing (C2-1).
      excl <- sum(ma$counts$cells, mb$counts$cells) > 0L
      if (excl) cls <- paste0(cls, ", declared columns excluded")
      note <- if (!excl) "" else
        paste0("; column ", cols, ": analysis header lines ", ma$counts$header_lines, ", cells replaced ",
               ma$counts$cells, "; fresh header lines ", mb$counts$header_lines, ", cells replaced ",
               mb$counts$cells, collapse = "")
      same <- identical(la, lb)
      if (grepl(masked, name))
        trows[[length(trows) + 1L]] <<- data.frame(
          item = name, rule = "semantic manifest, digit-masked lines", pages_a = NA_integer_,
          pages_b = NA_integer_, decimals_equal = NA, differing_lines_a = NA_integer_,
          differing_lines_b = NA_integer_, page = NA_integer_, outcome = same,
          stringsAsFactors = FALSE)
      first <- if (same) NA else {
        n <- max(length(la), length(lb))
        which(vapply(seq_len(n), function(i) !identical(la[i], lb[i]), logical(1)))[1]
      }
      return(row(item, name, cls, same, paste0(
                 if (same) sprintf("%d lines identical", length(la))
                 else sprintf("%d and %d lines; first differing line %d", length(la), length(lb), first), note)))
    }
    same <- identical(sha(fa), sha(fb))
    row(item, name, "sha256", same, if (same) "sha256 equal" else "sha256 differs")
  }
  # (e) declared upstream files
  for (p in upstream) {
    fa <- file.path(roots[["copy"]], p); fb <- file.path(roots[["copy"]], ws_rel, p)
    if (!file.exists(fb)) row("declared upstream file", p, "missing", FALSE, "fresh counterpart missing")
    else compare_one("declared upstream file", p, fa, fb)
  }
  # (f) package artifact files
  expand <- function(cell, root) {
    old <- setwd(root)
    on.exit(setwd(old))
    f <- .gate_expand_artifact_cell(cell)
    if (is.null(f)) structure(character(0), why = .gate_cell_reason$why) else as.character(f)
  }
  under_loc <- function(cell) {
    parts <- trimws(strsplit(cell, ";", fixed = TRUE)[[1]])
    any(vapply(parts, function(x) any(x == locations | startsWith(x, paste0(locations, "/"))),
               logical(1)))
  }
  # A fresh cell under the workspace (relative or under the copy root) was
  # written by the fresh run, wherever its declared location: the forest
  # figure, under no location field, is one (M-2).
  wpre <- c(paste0(ws_rel, "/"), paste0(roots[["copy"]], "/", ws_rel, "/"))
  in_ws <- function(cell) {
    parts <- trimws(strsplit(cell, ";", fixed = TRUE)[[1]])
    isTRUE(any(startsWith(parts, wpre[1]) | startsWith(parts, wpre[2])))
  }
  for (pk in sort(grep("_package$", names(analysis), value = TRUE))) {
    if (identical(pk, "review_package")) next
    aa <- analysis[[pk]]$artifacts
    fb_art <- fresh[[pk]]$artifacts
    if (!is.data.frame(aa) || !nrow(aa)) next
    if (!is.data.frame(fb_art)) fb_art <- data.frame(artifact = character(0), path = character(0))
    fresh_ws <- vapply(fb_art$path, in_ws, logical(1), USE.NAMES = FALSE)
    keep <- vapply(aa$path, under_loc, logical(1)) | aa$artifact %in% fb_art$artifact[fresh_ws]
    aa <- aa[keep, , drop = FALSE]
    fnorm <- rv_normalize(fb_art$path, spec)
    fresh_used <- rep(FALSE, nrow(fb_art))
    for (i in seq_len(nrow(aa))) {
      lab <- aa$artifact[i]
      item <- paste0(pk, " / ", lab)
      cand <- which(fb_art$artifact == lab & !fresh_used)
      j <- if (sum(aa$artifact == lab) == 1L && length(cand) == 1L) cand else
        cand[fnorm[cand] == rv_normalize(aa$path[i], spec)][1]
      if (!length(j) || is.na(j)) { row(item, aa$path[i], "pairing", FALSE, "no fresh row of the same package and label"); next }
      fresh_used[j] <- TRUE
      fa <- expand(aa$path[i], roots[["examined"]])
      fb <- expand(fb_art$path[j], roots[["copy"]])
      if (!length(fa) || !length(fb)) {
        row(item, aa$path[i], "expansion", FALSE, paste0("the cell expands to no file (",
            if (!length(fa)) paste0("analysis: ", attr(fa, "why")) else paste0("fresh: ", attr(fb, "why")), ")"))
        next
      }
      na <- rv_normalize(fa, spec); nb <- rv_normalize(fb, spec)
      for (n in setdiff(na, nb)) row(item, n, "pairing", FALSE, "file on the analysis side only")
      for (n in setdiff(nb, na)) row(item, n, "pairing", FALSE, "file on the fresh side only")
      for (n in intersect(na, nb))
        compare_one(item, n, file.path(roots[["examined"]], fa[match(n, na)]),
                    file.path(roots[["copy"]], fb[match(n, nb)]))
    }
    for (j in which(!fresh_used & (vapply(fnorm, under_loc, logical(1)) | fresh_ws)))
      row(paste0(pk, " / ", fb_art$artifact[j]), fb_art$path[j], "pairing", FALSE,
          "fresh row with no analysis partner")
  }
  # One summary row per column: entry; one that replaced no cell is dead (Q2).
  for (i in seq_len(nrow(vcols))) {
    live <- vtally[i, "ca"] + vtally[i, "cb"] > 0L
    row("volatile column entry", vcols$entry[i],
        if (live) "column: volatile entry (listed, not judged)" else "column: volatile entry, excluded nothing",
        if (live) NA else FALSE,
        sprintf("matched files %d; analysis header lines %d, cells replaced %d; fresh header lines %d, cells replaced %d",
                vtally[i, "files"], vtally[i, "ha"], vtally[i, "ca"], vtally[i, "hb"], vtally[i, "cb"]))
  }
  table <- if (length(rows)) do.call(rbind, rows) else
    data.frame(item = character(0), file = character(0), class = character(0),
               match = logical(0), detail = character(0), stringsAsFactors = FALSE)
  text <- if (length(trows)) do.call(rbind, trows) else NULL
  bad <- table[!is.na(table$match) & !table$match, , drop = FALSE]
  owner_of <- function(f) paste0("owning stage of ", f)
  outs <- rv_outcome(
    ifelse(bad$class %in% c("pdf, R-F11 text rule", "pptx, slide text runs") |
             grepl("digits masked", bad$class), "text_rule", "file_mismatch"),
    "domain 3 bullet 3",
    paste0("file comparison mismatch: ", bad$item, ": ", bad$file, " (", bad$class, "; ", bad$detail, ")"),
    "file-comparison.csv",
    ifelse(startsWith(bad$class, "column: volatile entry"), "root (review declaration)",
    ifelse(bad$class %in% c("pdf, R-F11 text rule", "pptx, slide text runs") |
             grepl("digits masked", bad$class), "reporting (report-writer)",
           vapply(bad$file, owner_of, character(1), USE.NAMES = FALSE))))
  list(table = table, text = text, outcomes = outs)
}

# ---------------------------------------------------------------------------
# Render census (Decision 15)
# ---------------------------------------------------------------------------

# Parses the two legs of the render census and the captured TeX. Stream leg:
# overfull hbox lines carry "texput.tex:<line>:" and "(<w>pt"; passes split
# where the line number decreases; the final pass counts. Log leg: the kept
# .log is the final pass (P8 receipt); "^Overfull \hbox (<w>pt too wide)",
# "^Overfull \vbox", "^Missing character"; undefined reference: lines with
# "undefined" and "reference" or "citation" (case-insensitive). The undefined
# class is judged on the log leg alone ("stream blind"); the other classes on
# the worse leg. Escaped-symbol literals on the captured TeX (allowlist
# the caller-specific allowlist). Thresholds: reporting-standards SKILL.md lines 118-124.
# Ported from review_render_census and review_escaped_symbol_census
rv_render_census <- function(stream, log, tex = character(0), engine_exit = NA_integer_,
                             completed = NA, allow = character(0)) {
  num <- function(x) suppressWarnings(as.numeric(x))
  ov <- grep("Overfull \\\\hbox", stream, value = TRUE)
  ln <- suppressWarnings(as.integer(sub("^.*texput\\.tex:([0-9]+):.*$", "\\1", ov)))
  pt <- num(sub("^.*\\(([0-9.]+)pt.*$", "\\1", ov))
  keep <- !is.na(ln) & !is.na(pt)
  ln <- ln[keep]; pt <- pt[keep]
  s_over20 <- 0; s_worst <- 0
  if (length(ln)) {
    pass <- cumsum(c(TRUE, diff(ln) < 0))
    fin <- pass == max(pass)
    s_over20 <- sum(pt[fin] > 20); s_worst <- max(pt[fin])
  }
  lov <- grep("^Overfull \\\\hbox \\(([0-9.]+)pt too wide\\)", log, value = TRUE)
  lpt <- num(sub("^Overfull \\\\hbox \\(([0-9.]+)pt too wide\\).*$", "\\1", lov))
  l_over20 <- sum(lpt > 20, na.rm = TRUE)
  l_worst <- if (length(lpt)) max(lpt, na.rm = TRUE) else 0
  undefined <- function(x) sum(grepl("undefined", x, ignore.case = TRUE) &
                                 grepl("reference|citation", x, ignore.case = TRUE))
  esc <- if (!length(tex)) NA_real_ else {
    t <- paste(tex, collapse = "\n")
    for (env in c("verbatim", "Verbatim", "Shaded", "lstlisting"))
      t <- gsub(sprintf("(?s)\\\\begin\\{%s\\}.*?\\\\end\\{%s\\}", env, env), "", t, perl = TRUE)
    h <- gregexpr("[A-Za-z][A-Za-z0-9]*\\\\_[A-Za-z0-9]+", t, perl = TRUE)[[1]]
    if (h[1] == -1L) 0 else {
      lens <- attr(h, "match.length")
      sum(vapply(seq_along(h), function(k) {
        tok <- substr(t, h[k], h[k] + lens[k] - 1L)
        back <- substr(t, max(1L, h[k] - 80L), h[k] - 1L)
        in_code <- regexpr("\\\\texttt\\{[^{}]*$", back, perl = TRUE) > 0
        allowed <- any(vapply(allow, grepl, logical(1), x = paste0(back, tok), fixed = TRUE))
        !in_code && !allowed
      }, logical(1)))
    }
  }
  q <- c("render completed (deliverable path, quarto --to pdf)", "engine exit status (log leg)",
         "overfull hbox over 20 pt, final pass", "overfull vbox", "missing glyph",
         "undefined reference (log leg alone; stream blind)",
         "escaped symbol literals, captured TeX (allowlisted excluded)",
         "worst overfull hbox, final pass (pt; recorded, not judged)",
         "captured TeX sha256 (a value; recorded, not judged)")
  s <- c(as.numeric(completed), NA, s_over20, sum(grepl("Overfull \\\\vbox", stream)),
         sum(grepl("Missing character", stream)), undefined(stream), NA, s_worst, NA)
  l <- c(NA, as.numeric(engine_exit), l_over20, sum(grepl("^Overfull \\\\vbox", log)),
         sum(grepl("^Missing character", log)), undefined(log), NA, l_worst, NA)
  judged <- c(as.numeric(completed), as.numeric(engine_exit), max(s[3], l[3]), max(s[4], l[4]),
              max(s[5], l[5]), l[6], esc, max(s[8], l[8]), NA)
  threshold <- c(1, 0, 0, 0, 0, 0, 0, NA, NA)
  within <- ifelse(is.na(threshold), NA, !is.na(judged) & judged == threshold)
  note <- c(rep("", 8), if (length(tex)) digest::digest(paste(tex, collapse = "\n"),
                                                        algo = "sha256", serialize = FALSE) else "no TeX captured")
  table <- data.frame(quantity = q, stream = s, log = l, judged = judged, threshold = threshold,
                      within = within, note = note, stringsAsFactors = FALSE)
  bad <- table[!is.na(table$within) & !table$within, , drop = FALSE]
  outcomes <- rv_outcome(rep("census_threshold", nrow(bad)), NA_character_,
                             paste0("render census: ", bad$quantity, " is ", bad$judged,
                                    " against the threshold ", bad$threshold),
                             "census-texput.log")
  if (!length(log)) outcomes <- rbind(outcomes, rv_outcome("census_threshold", NA_character_,
    "render census: raw TeX log is empty", "census-texput.log"))
  list(table = table, outcomes = outcomes)
}

# ---------------------------------------------------------------------------
# Checklist domains (Decision 16)
# ---------------------------------------------------------------------------

# Domain 1 and the checkout: per signed record the parsed decision, signer and
# date (parse_gate_record), the content problems (.gate_verify_content:
# canon2 self-recompute and live artifact rehash), the generated block and its
# three header lines once each, the REJECTED lines outside the template's
# unchecked box (rejection rounds, a reading item); the approvals ledger read
# by path (sha256, event count, chain via .verify_approval_events, the latest
# event per task approving with its record_digest equal to the record's whole
# bytes as .gate_sha256 computes them); the registry coverage; and the
# checkout receipt (HEAD line, status lines classified by checkout_permitted,
# per-record history census). Ported from review_signature_state and
rv_gate_integrity <- function(records, ledger, receipt, permitted, declared_tasks,
                              registry_tasks, analysis_id, checkout_path = "checkout receipt") {
  stage <- function(f) sub("\\.md$", "", basename(f))
  outs <- list()
  add <- function(rule, item, finding, evidence, owner = NA_character_)
    outs[[length(outs) + 1L]] <<- rv_outcome(rule, item, finding, evidence, owner)
  events <- .read_ledger(ledger)
  if (!length(records))
    add("gate_record", "domain 1 bullet 1", "no signed record was examined", ledger)
  rec <- do.call(rbind, lapply(records, function(f) {
    if (!file.exists(f)) {
      add("gate_record", "domain 1 bullet 1", paste0("signed record absent: ", f), f,
          paste0("owning stage: ", stage(f)))
      return(data.frame(record = f, decision = NA_character_, signer = NA_character_,
                        date = NA_character_, identity_problems = NA_integer_,
                        content_problems = NA_integer_, generated_block = FALSE,
                        header_lines_once = FALSE, rejected_lines = NA_integer_,
                        ledger_latest_approving = NA, ledger_record_digest_equal = NA,
                        stringsAsFactors = FALSE))
    }
    lines <- readLines(f, warn = FALSE)
    p <- parse_gate_record(f, lines)
    cp <- .gate_verify_content(f, lines)
    bl <- p$block
    once <- function(rx) sum(grepl(rx, bl)) == 1L
    header <- once("^Package-Digest:") && once("^Environment-attestation:") &&
      once("^Method-manifests:") && once("^_Generated from tool output at ")
    bi <- which(lines == "<!-- BEGIN GENERATED PACKAGE -->"); bj <- which(lines == "<!-- END GENERATED PACKAGE -->")
    outside <- if (length(bi) == 1L && length(bj) == 1L) lines[-(bi:bj)] else lines
    rej <- sum(grepl("REJECTED", outside, fixed = TRUE) & !grepl("^\\s*-\\s*\\[ \\]\\s*REJECTED", outside))
    latest <- .ledger_latest_for_task(events, stage(f))
    approving <- !is.null(latest) && isTRUE(as.character(latest$decision) %in% c("approved", "approved_with_changes"))
    dig_eq <- !is.null(latest) && identical(as.character(latest$record_digest), .gate_sha256(f))
    dec <- if (length(p$decision) == 1L) p$decision else NA_character_
    own <- paste0("owning stage: ", stage(f))
    if (!isTRUE(dec %in% c("approved", "approved_with_changes")))
      add("gate_record", "domain 1 bullet 1", paste0("record not approved (decision ", dec, "): ", f), f, own)
    if (is.na(p$signer) || is.na(p$date))
      add("gate_record", "domain 1 bullet 1", paste0("record without signer or date: ", f), f, own)
    if (length(p$problems))
      add("gate_record", "domain 1 bullet 1", paste0("record identity problem(s): ", f, ": ",
                                                    paste(p$problems, collapse = "; ")), f, own)
    if (length(cp))
      add("gate_record", "domain 3 bullet 2", paste0("record content problem(s): ", f, ": ",
                                                    paste(cp, collapse = "; ")), f, own)
    if (!length(bl) || !header)
      add("gate_record", "domain 1 bullet 3", paste0("generated block or its header lines missing: ", f), f, own)
    if (!approving || !dig_eq)
      add("gate_record", "domain 1 bullet 1", paste0("ledger: the latest event for ", stage(f),
          " is not approving or its record_digest differs from the record bytes"), ledger, own)
    data.frame(record = f, decision = dec, signer = as.character(p$signer), date = as.character(p$date),
               identity_problems = length(p$problems), content_problems = length(cp),
               generated_block = length(bl) > 0L, header_lines_once = header, rejected_lines = rej,
               ledger_latest_approving = approving, ledger_record_digest_equal = dig_eq,
               stringsAsFactors = FALSE)
  }))
  chain <- .verify_approval_events(events, expected_analysis = analysis_id)
  if (length(chain))
    add("gate_record", "domain 1 bullet 1", paste0("approvals ledger chain problem(s): ",
                                                  paste(chain, collapse = "; ")), ledger, "root")
  ledger_facts <- data.frame(
    quantity = c("approvals ledger path", "approvals ledger sha256", "approvals ledger events",
                 "approvals ledger chain problems"),
    value = c(ledger, if (file.exists(ledger)) digest::digest(file = ledger, algo = "sha256") else "absent",
              as.character(length(events)), as.character(length(chain))), stringsAsFactors = FALSE)
  # Registry coverage: a registry task neither declared nor out of scope by a
  # signed record of its own.
  missing_tasks <- setdiff(registry_tasks, declared_tasks)
  for (tk in missing_tasks) {
    f <- gate_file_for(analysis_id, tk)
    d <- tryCatch(parse_gate_record(f)$decision, error = function(e) NA_character_)
    if (!isTRUE(d %in% c("approved", "approved_with_changes")))
      add("registry", "domain 1 bullet 4", paste0("registry task '", tk,
          "' is neither declared nor marked out of scope by a signed record"), "pipeline/compose.R")
  }
  # Checkout receipt (procedure step 1; Q-B8-16).
  heads <- sub("^HEAD ", "", grep("^HEAD ", receipt, value = TRUE))
  head_ok <- length(heads) == 1L && grepl("^[0-9a-f]{40}$", heads)
  st <- sub("^status ", "", grep("^status ", receipt, value = TRUE))
  code <- substr(st, 1L, 2L)
  pathpart <- substring(st, 4L)
  subj <- sub(":re:.*$", "", names(permitted))
  pat <- sub("^[a-z]+:re:", "", names(permitted))
  cls <- vapply(seq_along(st), function(i) {
    admit <- (subj == "untracked" & code[i] == "??") | (subj == "modified" & code[i] == " M")
    hit <- which(admit & vapply(pat, function(p) grepl(p, pathpart[i], perl = TRUE), logical(1)))
    if (length(hit)) names(permitted)[hit[1]] else NA_character_
  }, character(1))
  co <- data.frame(kind = c("HEAD", rep("status", length(st))),
                   code = c(NA_character_, code),
                   path = c(if (length(heads)) paste(heads, collapse = "; ") else NA_character_,
                            ifelse(grepl(" -> ", pathpart, fixed = TRUE),
                                   gsub(" -> ", "; ", pathpart, fixed = TRUE), pathpart)),
                   class = c(if (head_ok) "40-hex hash" else "missing or malformed", cls),
                   commits = NA_integer_, after_sign = NA_integer_, stringsAsFactors = FALSE)
  rx <- "^record (.+) commits ([0-9]+) after-sign ([0-9]+)$"
  hl <- grep(rx, receipt, value = TRUE)
  if (length(hl)) co <- rbind(co, data.frame(
    kind = "record", code = NA_character_, path = sub(rx, "\\1", hl), class = NA_character_,
    commits = as.integer(sub(rx, "\\2", hl)), after_sign = as.integer(sub(rx, "\\3", hl)),
    stringsAsFactors = FALSE))
  rc <- checkout_path
  for (f in setdiff(records, co$path[co$kind == "record"]))
    add("not_evaluated", "domain 3 bullet 2", paste0("signed record has no history census line: ", f), rc)
  if (!length(receipt)) add("checkout", "procedure step 1", "the checkout receipt is empty", rc, "root")
  else if (!head_ok) add("checkout", "procedure step 1", "the checkout receipt has no single HEAD line with a 40-hex hash", rc, "root")
  for (i in which(is.na(cls)))
    add("checkout", "procedure step 1", paste0("status line outside the permitted classes: code '",
        code[i], "', path ", pathpart[i]), rc, "root")
  for (i in which(co$kind == "record" & co$after_sign > 0L))
    add("history_after_sign", "domain 3 bullet 2", paste0("signed record with commits after its signing commit: ",
        co$path[i], " (", co$after_sign[i], ")"), rc)
  list(records = rec, ledger = ledger_facts, checkout = co,
       registry = data.frame(task = registry_tasks, declared = registry_tasks %in% declared_tasks,
                             stringsAsFactors = FALSE),
       head = if (head_ok) heads else NA_character_,
       rejection_rounds = sum(rec$rejected_lines, na.rm = TRUE),
       outcomes = if (length(outs)) do.call(rbind, outs) else rv_outcome())
}

# A verdict column must be present exactly once in a nonempty table.
rv_col <- function(d, key) {
  if (!is.data.frame(d) || nrow(d) == 0L || sum(names(d) == key, na.rm = TRUE) != 1L)
    return(NULL)
  d[[which(names(d) == key)]]
}

# Domain 3 (a) and (b): every row of the reporting package's promoted table
# against its source, by the selection's form, each rule written from
# reading pipeline/tasks/reporting.R lines 803-880 (the promotion list) and
# 1332-1366 (the command forms): a heading of a *_package source (exact
# identical table); "notes" (identical notes); "<heading>: the rows of run
# <id>"; "artifacts row labelled: <label>"; "artifacts rows labelled: <l1>;
# ..."; the plan file (every line); the six records (the path list); the
# declaration fields reference_regimen, document and deviation_log; the
# Comments blocks (each block contained in its record, signer and date
# lines equal); the recorded tables (names, and each table equal to its
# promotion with double cells as the gate record writes them); the number
# census (evaluated by (b)). A form no rule covers is "not evaluated"
# (major). (b) the number census table (undeclared 0 per source) and the
# semantic manifest (# result: PASS, notes expected equal to found).
rv_promotion_check <- function(promoted, values, map_lines, reporting, manifest_lines = character(0)) {
  outs <- list()
  rows <- list()
  pk_tables <- function(s) values[[s]]$tables
  cmp <- function(t, v, e) !is.null(v) && !is.null(e) && rv_compare_value(t, v, e)$match
  rd <- values[["report_review_decisions"]]
  for (i in seq_len(nrow(promoted))) {
    t <- promoted$target[i]; s <- promoted$source[i]; sel <- promoted$selection[i]
    v <- values[[t]]
    form <- NA_character_; ok <- NA
    if (identical(sel, "Stage package") && grepl("_package$", s)) {
      form <- "stage package"
      stage <- sub("_package$", "", s)
      ok <- !is.null(values[[s]]) && identical(v[[stage]], values[[s]])
    } else if (identical(sel, "notes") && grepl("_package$", s)) {
      form <- "notes"; ok <- !is.null(values[[s]]$notes) && identical(v, values[[s]]$notes)
    } else if (startsWith(sel, "artifacts rows labelled: ") && grepl("_package$", s)) {
      form <- "figure rows"
      labs <- strsplit(sub("^artifacts rows labelled: ", "", sel), "; ", fixed = TRUE)[[1]]
      a <- values[[s]]$artifacts
      e <- if (is.data.frame(a)) a[a$artifact %in% labs, c("artifact", "path"), drop = FALSE] else NULL
      ok <- !is.null(e) && nrow(e) > 0L && nrow(e) == length(labs) && cmp(t, v, e)
    } else if (startsWith(sel, "artifacts row labelled: ") && grepl("_package$", s)) {
      form <- "artifacts row"
      lab <- sub("^artifacts row labelled: ", "", sel)
      a <- values[[s]]$artifacts
      e <- if (is.data.frame(a)) a[a$artifact == lab, c("artifact", "path"), drop = FALSE] else NULL
      ok <- !is.null(e) && nrow(e) == 1L && cmp(t, v, e)
    } else if (grepl(": the rows of run ", sel, fixed = TRUE) && grepl("_package$", s)) {
      form <- "rows of a run"
      h <- sub(": the rows of run .*$", "", sel); run <- sub("^.*: the rows of run ", "", sel)
      x <- pk_tables(s)[[h]]
      e <- if (is.data.frame(x)) x[x$run %in% run, , drop = FALSE] else NULL
      ok <- !is.null(e) && nrow(e) > 0L && cmp(t, v, e)
    } else if (grepl("_package$", s)) {
      form <- "table heading"
      ok <- !is.null(pk_tables(s)[[sel]]) && identical(v, pk_tables(s)[[sel]])
    } else if (identical(s, "da_map_file")) {
      form <- "plan file, every line"; ok <- !is.null(map_lines) && identical(v, map_lines)
    } else if (identical(s, "the six signed stage records")) {
      form <- "record paths"; ok <- identical(unname(v), strsplit(sel, "; ", fixed = TRUE)[[1]])
    } else if (identical(s, "the analysis declaration") && sel %in% c("reference_regimen", "document")) {
      form <- "declared field"; ok <- !is.null(reporting[[sel]]) && identical(v, reporting[[sel]])
    } else if (identical(s, "the analysis declaration") && identical(sel, "deviation_log")) {
      form <- "deviation log"
      dl <- reporting$deviation_log
      r <- v$rows
      ok <- !is.null(dl) && is.data.frame(r) && identical(r$number, names(dl)) &&
        all(vapply(names(dl), function(n) all(vapply(names(dl[[n]]), function(f)
          identical(r[[f]][r$number == n], unname(dl[[n]][[f]])), logical(1))), logical(1)))
    } else if (identical(s, "report_record_files")) {
      form <- "Comments blocks"
      ok <- is.list(v) && length(v) > 0L && all(vapply(v, function(e) {
        if (!file.exists(e$record)) return(FALSE)
        l <- readLines(e$record, warn = FALSE, encoding = "UTF-8")
        txt <- paste(l, collapse = "\n")
        all(vapply(e$blocks, grepl, logical(1), x = txt, fixed = TRUE)) &&
          paste0("Signed: ", e$signer) %in% l && paste0("Date: ", e$date) %in% l
      }, logical(1)))
    } else if (identical(s, "the promotions above")) {
      form <- "recorded tables"
      nm <- strsplit(sel, ", ", fixed = TRUE)[[1]]
      ok <- is.list(v) && identical(names(v), nm) && all(vapply(nm, function(n) {
        x <- values[[n]]
        if (!is.data.frame(x)) return(FALSE)
        for (cn in names(x)) if (is.double(x[[cn]])) x[[cn]] <- .md_render_col(x[[cn]], cn)
        isTRUE(cmp(n, v[[n]], x))
      }, logical(1)))
    } else if (startsWith(s, "report_source_files")) {
      form <- "number census (evaluated by the number census row)"
      tab <- values[["reporting_package"]]$tables[["Number census of the report sources"]]
      und <- rv_col(tab, "undeclared")
      ok <- is.numeric(und) && !anyNA(und) && all(und == 0)
    }
    if (is.null(v)) ok <- FALSE
    if (is.na(form)) {
      outs[[length(outs) + 1L]] <- rv_outcome("not_evaluated", "domain 3 bullet 1",
        paste0("promotion ", t, ": no rule covers the selection form (source ", s, ")"),
        "findings-log.csv")
    } else if (!isTRUE(ok)) {
      outs[[length(outs) + 1L]] <- rv_outcome("promotion", "domain 3 bullet 1",
        paste0("promotion ", t, " differs from its source ", s, " (", form, ")"),
        "findings-log.csv")
    }
    rows[[i]] <- data.frame(target = t, source = s, form = if (is.na(form)) "not evaluated" else form,
                            match = if (is.na(form)) NA else isTRUE(ok), stringsAsFactors = FALSE)
  }
  promotions <- if (length(rows)) do.call(rbind, rows) else
    data.frame(target = character(0), source = character(0), form = character(0), match = logical(0))
  tab <- values[["reporting_package"]]$tables[["Number census of the report sources"]]
  und <- rv_col(tab, "undeclared")
  und <- if (is.numeric(und) && !anyNA(und)) sum(und != 0) else NA
  res <- sub("^# result: ", "", grep("^# result: ", manifest_lines, value = TRUE))
  ne <- sub("^# notes-expected: ", "", grep("^# notes-expected: ", manifest_lines, value = TRUE))
  nf <- sub("^# notes-found: ", "", grep("^# notes-found: ", manifest_lines, value = TRUE))
  numbers <- data.frame(
    check = c("number census: sources with undeclared tokens", "semantic check: # result line",
              "semantic check: notes expected equal notes found"),
    value = c(as.character(und), if (length(res)) res[1] else "absent",
              as.character(length(ne) == 1L && identical(ne, nf))),
    expected = c("0", "PASS", "TRUE"), stringsAsFactors = FALSE)
  if (is.null(reporting$number_census) && is.null(tab))
    numbers <- numbers[-1L, , drop = FALSE]
  numbers$match <- !is.na(numbers$value) & numbers$value == numbers$expected
  for (i in which(!numbers$match))
    outs[[length(outs) + 1L]] <- rv_outcome("promotion", "domain 3 bullet 1",
      paste0(numbers$check[i], ": ", numbers$value[i], " (expected ", numbers$expected[i], ")"),
      "findings-log.csv", "reporting")
  list(promotions = promotions, numbers = numbers,
       outcomes = if (length(outs)) do.call(rbind, outs) else rv_outcome())
}

# Domain 3 (c): the code inventory's stored md5 against the file on disk
# (relative to `root`); a mismatch on an expected_changes path is marked
# expected (minor), any other is major. Ported from review_inventory_check
rv_inventory_check <- function(inventory, expected_changes, root = ".") {
  if (is.null(inventory) || (is.data.frame(inventory) && nrow(inventory) == 0L))
    return(list(table = data.frame(file = character(0), exists = logical(0),
      md5_stored = character(0), md5_disk = character(0), match = logical(0),
      expected_change = logical(0), stringsAsFactors = FALSE),
      outcomes = rv_outcome("not_evaluated", "domain 3 bullet 1", "code inventory is absent or empty",
        "findings-log.csv")))
  f <- inventory$file
  abs <- file.path(root, f)
  ex <- file.exists(abs)
  md5 <- rep(NA_character_, length(f))
  md5[ex] <- unname(tools::md5sum(abs[ex]))
  match <- ex & !is.na(md5) & md5 == inventory$md5
  expected <- f %in% names(expected_changes)
  table <- data.frame(file = f, exists = ex, md5_stored = inventory$md5, md5_disk = md5,
                      match = match, expected_change = expected, stringsAsFactors = FALSE)
  outs <- rv_outcome(
    ifelse(expected[!match], "inventory_expected", "inventory_undeclared"), "domain 3 bullet 1",
    paste0("code inventory: stored md5 differs from disk: ", f[!match],
           ifelse(expected[!match], " (declared expected change)", "")),
    "findings-log.csv",
    ifelse(expected[!match], NA_character_, paste0("owning module of ", f[!match])))
  list(table = table, outcomes = outs)
}

# Domain 2: (a) one row per item of `rows` (item, map_citation, declared: a
# path into the declared inputs joined by "/", target, accessor: an R
# expression on x, the target's value, compare: "equal" or "at_most", owner);
# the declared value read from the parsed declaration by path, never typed;
# the executed value from the accessor; an item with no executed value is
# "not evaluated" (major). (b) the deviation identifiers absent from the MAP's
# Section 6 text (between "## 6." and "## 7.") against map_section6_pending:
# equal is the expected close-out finding (minor), different is major. (c)
# every declared task has a package in the store.
rv_concordance <- function(rows, inputs, values, map_lines, deviation_ids, pending, tasks) {
  outs <- list()
  # Evidence preparation is local to the existing three grid rows. All other
  # comparators retain their historical accessors and outcome routing.
  grid_evidence <- function() {
    result <- list(start = logical(0), end = logical(0), sequence = logical(0))
    reasons <- character(0)
    note <- function(reason) reasons <<- c(reasons, reason)
    scalar <- function(x) typeof(x) %in% c("integer", "double") && !is.object(x) && is.null(dim(x)) &&
      length(x) == 1L && is.finite(x)
    grid <- if (is.list(inputs$simulation)) inputs$simulation$grid else NULL
    usable <- is.list(grid) && !is.object(grid) && !is.null(names(grid)) && !anyNA(names(grid)) &&
      all(vapply(c("start", "end", "delta"),
        function(k) sum(names(grid) == k) == 1L && scalar(grid[[k]]), logical(1)))
    if (!usable || grid$delta == 0) {
      note("grid declaration is not a list of finite plain numeric scalars with nonzero delta")
      return(list(result = result, reasons = reasons))
    }
    from <- as.double(grid$start); to <- as.double(grid$end); by <- as.double(grid$delta)
    del <- to - from
    q <- if (is.finite(del)) del/by else to/by - from/by
    if (!is.finite(q) || q < 0 || q > .Machine$integer.max) {
      note("grid quotient has invalid direction or exceeds the installed sequence representation")
      return(list(result = result, reasons = reasons))
    }
    collapsed <- from == to || (is.finite(del) &&
      abs(del)/max(abs(to), abs(from)) < 100 * .Machine$double.eps)
    if (!collapsed) {
      integer_branch <- all(vapply(grid[c("start", "end", "delta")], is.integer, logical(1)))
      adjusted <- if (integer_branch) q else q + 1e-10
      index <- trunc(adjusted)
      vector_limit <- if (.Machine$sizeof.pointer >= 8L) 2^52 else .Machine$integer.max
      if (!is.finite(adjusted) || adjusted > .Machine$integer.max || index < 0 ||
          index > .Machine$integer.max || index + 1 > vector_limit) {
        note("grid scalar index or length is not representable by the installed sequence source")
        return(list(result = result, reasons = reasons))
      }
    }
    package <- values[["simulation_package"]]
    metadata <- if (is.list(package) && is.list(package$tables)) package$tables[["Scenario definitions"]] else NULL
    count <- if (is.list(inputs$simulation)) inputs$simulation$n else NULL
    if (!is.data.frame(metadata) || anyNA(names(metadata)) ||
        sum(names(metadata) == "SCENARIO") != 1L ||
        !is.atomic(metadata$SCENARIO) || is.object(metadata$SCENARIO) ||
        !length(metadata$SCENARIO) || anyNA(metadata$SCENARIO) ||
        any(!nzchar(trimws(as.character(metadata$SCENARIO)))) ||
        !scalar(count) || count < 1 || count != trunc(count)) {
      note("scenario metadata or declared distinct-ID count is unavailable or unusable")
      return(list(result = result, reasons = reasons))
    }
    scenarios <- unique(as.character(metadata$SCENARIO))
    raw <- values[["sim_results"]]
    frame <- if (is.list(raw)) raw$sim else NULL
    read_errors <- attr(values, "read_errors")
    if ("sim_results" %in% names(read_errors)) note("sim_results could not be read from the store")
    required <- c("SCENARIO", "ID", "TIME")
    if (!is.data.frame(frame) || !nrow(frame) || anyNA(names(frame)) ||
        !all(vapply(required, function(k) sum(names(frame) == k) == 1L, logical(1)))) {
      note("sim_results$sim is absent, empty or lacks unique SCENARIO/ID/TIME columns")
      return(list(result = result, reasons = reasons))
    }
    identifier <- function(x) typeof(x) %in% c("character", "integer", "double") && !is.object(x) && is.null(dim(x))
    if (!identifier(frame$SCENARIO) || !identifier(frame$ID) ||
        !typeof(frame$TIME) %in% c("integer", "double") || is.object(frame$TIME) || !is.null(dim(frame$TIME))) {
      note("observed identifiers or times do not have usable plain types")
      return(list(result = result, reasons = reasons))
    }
    sc <- as.character(frame$SCENARIO); id <- as.character(frame$ID)
    named <- !is.na(sc) & nzchar(trimws(sc)) & !is.na(id) & nzchar(trimws(id))
    if (is.numeric(frame$SCENARIO)) named <- named & is.finite(frame$SCENARIO)
    if (is.numeric(frame$ID)) named <- named & is.finite(frame$ID)
    if (any(!named)) note("observations with missing or unusable group identifiers")
    if (any(named & !sc %in% scenarios)) note("observations include unexpected scenarios")
    for (scenario in scenarios) {
      in_scenario <- named & sc == scenario
      ids <- unique(id[in_scenario])
      if (!length(ids)) note("expected scenario has no identified observations")
      if (length(ids) != count) note("scenario distinct-ID count differs from the declared count")
      for (subject in ids) {
        index <- which(in_scenario & id == subject)
        if (!all(is.finite(frame$TIME[index]))) {
          note("a scenario/ID group contains nonfinite times")
          next
        }
        observed <- sort(frame$TIME[index])
        result$start <- c(result$start, observed[1L] == from)
        result$end <- c(result$end, observed[length(observed)] == to)
        m <- length(observed)
        if (!collapsed && q > m + 1) {
          result$sequence <- c(result$sequence, FALSE)
        } else {
          # The scalar proof above bounds this sole oracle construction to
          # observed group length plus two, including source fuzz/clamping.
          expected <- tryCatch(seq(from = grid$start, to = grid$end, by = grid$delta),
                               error = function(err) NULL)
          if (is.null(expected)) {
            note("literal sequence construction could not evaluate the declaration")
          } else {
            result$sequence <- c(result$sequence,
              length(observed) == length(expected) &&
                identical(unname(as.double(observed)), unname(as.double(expected))))
          }
        }
      }
    }
    list(result = result, reasons = unique(reasons))
  }
  grid <- NULL
  grid_items <- c("simulation: grid start", "simulation: grid end", "simulation: grid step")
  get_decl <- function(path) {
    x <- inputs
    for (k in strsplit(path, "/", fixed = TRUE)[[1]]) {
      if (is.null(x) || is.null(names(x)) || !k %in% names(x)) return(NULL)
      x <- x[[k]]
    }
    x
  }
  out <- do.call(rbind, lapply(seq_len(nrow(rows)), function(i) {
    if (rows$item[i] %in% grid_items) {
      if (is.null(grid)) grid <<- grid_evidence()
      key <- c("start", "end", "sequence")[match(rows$item[i], grid_items)]
      checks <- grid$result[[key]]
      failed <- any(checks %in% FALSE)
      unknown <- length(grid$reasons) > 0L || !length(checks)
      m <- if (failed) FALSE else if (unknown) NA else TRUE
      if (unknown) outs[[length(outs) + 1L]] <<- rv_outcome("not_evaluated", "domain 2 bullet 3",
        paste0("concordance item '", rows$item[i], "': ", paste(grid$reasons, collapse = "; ")),
        "findings-log.csv")
      if (failed) outs[[length(outs) + 1L]] <<- rv_outcome("concordance", "domain 2 bullet 3",
        paste0("concordance mismatch: ", rows$item[i], " (per SCENARIO/ID observations; declared time-unit basis)"),
        "findings-log.csv", rows$owner[i])
      declared <- if (is.list(inputs$simulation) && is.list(inputs$simulation$grid))
        inputs$simulation$grid[[c("start", "end", "delta")[match(rows$item[i], grid_items)]]] else NULL
      return(data.frame(part = "(a) settings", item = rows$item[i], map_citation = rows$map_citation[i],
        declared_field = rows$declared[i], declared = if (is.null(declared)) NA_character_ else paste(declared, collapse = ", "),
        source = rows$target[i], executed = paste0("per SCENARIO/ID observations in declared time units: ",
          if (failed) "deviation" else if (unknown) "not evaluated" else "exact match"),
        match = m, stringsAsFactors = FALSE))
    }
    d <- get_decl(rows$declared[i])
    e <- tryCatch(eval(parse(text = rows$accessor[i], keep.source = FALSE)[[1]],
                       list(x = values[[rows$target[i]]]), baseenv()),
                  error = function(err) NULL)
    m <- if (is.null(d) || !length(e)) NA else {
      dn <- suppressWarnings(as.numeric(d)); en <- suppressWarnings(as.numeric(e))
      numeric <- !anyNA(dn) && !anyNA(en)
      if (identical(rows$compare[i], "at_most")) numeric && all(en <= dn)
      else if (numeric) length(dn) == 1L && all(en == dn)
      else identical(unique(as.character(e)), as.character(d))
    }
    if (is.na(m)) outs[[length(outs) + 1L]] <<- rv_outcome("not_evaluated", "domain 2 bullet 3",
      paste0("concordance item '", rows$item[i], "': no executed value"), "findings-log.csv")
    else if (!m) outs[[length(outs) + 1L]] <<- rv_outcome("concordance", "domain 2 bullet 3",
      paste0("concordance mismatch: ", rows$item[i]), "findings-log.csv", rows$owner[i])
    data.frame(part = "(a) settings", item = rows$item[i], map_citation = rows$map_citation[i],
               declared_field = rows$declared[i],
               declared = if (is.null(d)) NA_character_ else paste(d, collapse = ", "),
               source = rows$target[i],
               executed = if (!length(e)) NA_character_ else paste(unique(as.character(e)), collapse = ", "),
               match = m, stringsAsFactors = FALSE)
  }))
  s6a <- grep("^## 6\\.", map_lines); s7 <- grep("^## 7\\.", map_lines)
  s6 <- if (length(s6a) == 1L && length(s7) == 1L && s7 > s6a) map_lines[(s6a + 1L):(s7 - 1L)] else character(0)
  present <- vapply(deviation_ids, function(id)
    any(grepl(paste0("(?<![A-Za-z0-9])", gsub("([.-])", "\\\\\\1", id), "(?![0-9])"), s6, perl = TRUE)),
    logical(1))
  absent <- deviation_ids[!present]
  equal <- setequal(absent, pending)
  outs[[length(outs) + 1L]] <- if (equal)
    rv_outcome("section6_pending", "domain 2 bullet 4", paste0("MAP Section 6 pending entries ",
      paste(absent, collapse = ", "), ", as declared; routed to the close-out MAP install"), "declared analysis plan")
  else rv_outcome("section6_differs", "domain 2 bullet 4", paste0("MAP Section 6 pending set (",
      paste(absent, collapse = ", "), ") differs from map_section6_pending (", paste(pending, collapse = ", "), ")"),
      "declared analysis plan")
  pk <- paste0(tasks, "_package")
  have <- pk %in% names(values) & !vapply(pk, function(p) is.null(values[[p]]), logical(1))
  for (p in pk[!have]) outs[[length(outs) + 1L]] <- rv_outcome("concordance", "domain 2 bullet 2",
    paste0("declared task without a package in the store: ", p), "findings-log.csv")
  table <- rbind(out,
    data.frame(part = "(b) MAP Section 6", item = paste0("deviation ", deviation_ids, " in MAP Section 6"),
               map_citation = "MAP Section 6", declared_field = "review/map_section6_pending",
               declared = ifelse(deviation_ids %in% pending, "pending", "present"),
               source = "reporting_package: Deviation log as rendered",
               executed = ifelse(present, "present", "pending"),
               match = (deviation_ids %in% pending) == !present, stringsAsFactors = FALSE),
    data.frame(part = "(c) packages", item = paste0("package of task ", tasks), map_citation = "",
               declared_field = "tasks", declared = "declared", source = pk,
               executed = ifelse(have, "present", "absent"), match = have, stringsAsFactors = FALSE))
  rownames(table) <- NULL
  list(table = table, outcomes = do.call(rbind, outs))
}

# Domain 4. The skills used: every skills/<name> string (a name of
# letters, digits, "_" and "-") in the commands of
# the examined manifest, every skill a unit citation or a unit's reference
# path names in a literal input file, and the registry skill of each declared
# task. A unit is used by a task when a command of that task names the unit's
# reference path, or when a literal input file of one of the task's file
# targets (rv_identity_paths) names the reference path or cites the unit as
# "skills/<skill>[/MANIFEST.yaml][,] unit [`]<unit>" ("unit" in any case;
# the forms the control-stream templates and their adapted streams carry).
# A literal input is read by the path as written: an absolute or "~" path as
# written after path.expand, any other path joined to `root`. One kind of
# input is not read: a path that, after one leading "./" is removed for this
# comparison only, equals one of `records` (this analysis's stage record
# paths as the pipeline builds them) and has no "." or ".." segment, because
# its disclosure lines are flags, not uses. A file that cannot be read gives
# no lines. No other unit counts as used: a skill with no unit identified is
# judged only when one of its units is not approved, and then as not
# evaluated (the units used are unknown), never as a use. Per skill with a
# MANIFEST.yaml (relative to `root`): the units used, their statuses (a cited
# unit the manifest lacks reads "not in manifest" and is judged as not
# approved), reviewed_by and reviewed_at, the strict validation (problem
# count), and whether reviewed_at is on or before the first use (the date of
# the first signed record of a task that uses the skill; dates compared as
# dates). A used unit that is not approved was flagged at the time of use
# when the signed record of the task that used it holds the line
# "Disclosure: skills/<skill>, unit <unit>, status <draft|rejected|unvalidated>"
# (optional leading spaces or tabs and one list marker, optional backticks
# around the unit and the status, an optional full stop, nothing else on the
# line; a line inside an HTML comment does not count). A flagged use is a
# disclosed use (validation_disclosed, minor); an unflagged use is silent
# (validation, critical); one row per unit and task. Prose is not read as a
# flag. ponytail: citation read of literal inputs and a fixed disclosure
# line (a citation wrapped across lines, an unreadable input and an uncited
# adoption beside a cited one are not seen); replace with the adoption record
# of SME decision 5 when adopt_reference_model exists. Per skill without a
# manifest: its first SKILL.md validation line, or its "Status:
# guidance-only" line. The two analysis-authored numerics
# checks (the simulation translation check, the forest verification) read by
# heading.
rv_validation_audit <- function(manifest, tasks, records, packages, registry_skills,
                                root = ".") {
  outs <- list()
  add <- function(rule, item, finding, evidence, owner = NA_character_)
    outs[[length(outs) + 1L]] <<- rv_outcome(rule, item, finding, evidence, owner)
  task_of <- function(cmd) {
    e <- tryCatch(parse(text = cmd, keep.source = FALSE)[[1L]], error = function(err) NULL)
    if (is.call(e) && identical(e[[1L]], as.name("{")))
      for (k in seq_along(e)[-1L]) {
        s <- e[[k]]
        if (is.name(s) && grepl("^gate_", as.character(s))) {
          prev <- sub("^gate_", "", as.character(s))
          return(tasks[match(prev, tasks) + 1L])
        }
      }
    tasks[1]
  }
  stage_rows <- manifest[!grepl("^gate_", manifest$name), , drop = FALSE]
  use <- do.call(rbind, lapply(seq_len(nrow(stage_rows)), function(i) {
    sk <- unique(sub("^skills/", "", regmatches(stage_rows$command[i],
                                                 gregexpr("skills/[A-Za-z0-9_-]+", stage_rows$command[i]))[[1]]))
    if (!length(sk)) return(NULL)
    data.frame(skill = sk, task = task_of(stage_rows$command[i]), command = stage_rows$command[i],
               stringsAsFactors = FALSE)
  }))
  if (is.null(use)) use <- data.frame(skill = character(0), task = character(0), command = character(0))
  # The lines of every literal input of a file target, with the target's task
  # (`p` the path as written, `q` the path for the record skip only).
  ft <- stage_rows[if (is.null(stage_rows$format)) FALSE else stage_rows$format %in% "file", , drop = FALSE]
  inp <- do.call(rbind, lapply(seq_len(nrow(ft)), function(i) {
    p <- as.character(unlist(rv_identity_paths(ft$command[i])))
    q <- sub("^[.]/", "", p)
    p <- path.expand(p[!(q %in% records & !grepl("(^|/)[.][.]?(/|$)", q))])
    p <- ifelse(startsWith(p, "/"), p, file.path(root, p))
    l <- unlist(lapply(p, function(f)
      tryCatch(suppressWarnings(readLines(f, warn = FALSE)), error = function(e) character(0))))
    if (length(l)) data.frame(task = task_of(ft$command[i]), line = l, stringsAsFactors = FALSE)
  }))
  if (is.null(inp)) inp <- data.frame(task = character(0), line = character(0))
  # The (skill, unit, task) uses the inputs give: citations, then reference paths.
  cite_re <- "skills/([A-Za-z0-9_-]+)(/MANIFEST[.]yaml)?,?[ \t]+(?i:unit)[ \t]+`?([A-Za-z0-9_]+)"
  hit <- regmatches(inp$line, gregexpr(cite_re, inp$line, perl = TRUE, useBytes = TRUE))
  h <- unlist(hit)
  cite <- data.frame(skill = sub(cite_re, "\\1", h, perl = TRUE, useBytes = TRUE),
                     unit = sub(cite_re, "\\3", h, perl = TRUE, useBytes = TRUE),
                     task = rep(inp$task, lengths(hit)), stringsAsFactors = FALSE)
  for (s in unique(sub("^skills/(.*)/$", "\\1", unlist(regmatches(inp$line,
         gregexpr("skills/[A-Za-z0-9_-]+/", inp$line, useBytes = TRUE)))))) {
    mf <- file.path(root, "skills", s, "MANIFEST.yaml")
    if (!file.exists(mf)) next
    m <- read_method_manifest(mf)
    for (u in names(m$units)) {
      tk <- unique(inp$task[grepl(m$units[[u]]$reference_path, inp$line, fixed = TRUE, useBytes = TRUE)])
      if (length(tk)) cite <- rbind(cite, data.frame(skill = s, unit = u, task = tk, stringsAsFactors = FALSE))
    }
  }
  skills <- sort(unique(c(use$skill, cite$skill, registry_skills)))
  rec_date <- function(tk) {
    f <- records[tk]
    if (is.na(f)) return(NA_character_)
    tryCatch(parse_gate_record(f)$date, error = function(e) NA_character_)
  }
  # Whether the signed record of task `tk` holds the disclosure line for
  # skill `s` and unit `u` outside an HTML comment: a line is inside one when
  # the last comment marker before it opened one.
  disc_re <- "^[ \t]*(?:[-*][ \t]+)?Disclosure: skills/([A-Za-z0-9_.-]+), unit `?([A-Za-z0-9_]+)`?, status `?(?:draft|rejected|unvalidated)`?[.]?[ \t]*$"
  flagged <- function(tk, s, u) {
    f <- records[tk]
    if (is.na(f) || !file.exists(f)) return(FALSE)
    l <- readLines(f, warn = FALSE)
    mk <- regmatches(l, gregexpr("<!--|-->", l, useBytes = TRUE))
    end <- vapply(mk, function(x) if (length(x)) x[length(x)] == "<!--" else NA, logical(1))
    k <- cummax(ifelse(is.na(end), 0L, seq_along(end)))
    l <- l[!c(FALSE, end)[c(0L, k[-length(k)]) + 1L] & grepl(disc_re, l, perl = TRUE, useBytes = TRUE)]
    paste(s, u) %in% sub(disc_re, "\\1 \\2", l, perl = TRUE, useBytes = TRUE)
  }
  rows <- lapply(skills, function(s) {
    tks <- unique(c(use$task[use$skill == s], cite$task[cite$skill == s]))
    dates <- vapply(tks, rec_date, character(1))
    first <- if (length(dates) && any(!is.na(dates))) as.character(min(as.Date(dates[!is.na(dates)]))) else NA_character_
    mf <- file.path(root, "skills", s, "MANIFEST.yaml")
    if (file.exists(mf)) {
      m <- read_method_manifest(mf)
      us <- use[use$skill == s, , drop = FALSE]
      cs <- cite[cite$skill == s, , drop = FALSE]
      ut <- unique(rbind(cs[, c("unit", "task")], do.call(rbind, lapply(names(m$units), function(u) {
        tk <- us$task[grepl(m$units[[u]]$reference_path, us$command, fixed = TRUE)]
        if (length(tk)) data.frame(unit = u, task = tk, stringsAsFactors = FALSE)
      }))))
      units <- c(intersect(names(m$units), ut$unit), setdiff(ut$unit, names(m$units)))
      st <- vapply(units, function(u) if (u %in% names(m$units)) m$units[[u]]$status else "not in manifest",
                   character(1))
      all_st <- vapply(m$units, function(u) u$status, character(1))
      if (!nzchar(trimws(m$reviewed_by))) {
        st[] <- "draft"
        all_st[] <- "draft"
      }
      prob <- tryCatch({ validate_method_manifest(mf); 0L }, error = function(e) 1L)
      before <- if (is.na(first) || !nzchar(m$reviewed_at)) NA else
        as.Date(m$reviewed_at) <= as.Date(first)
      owner <- paste0("owner of skills/", s, " and the SME")
      if (!is.na(first) && !nzchar(m$reviewed_at))
        add("not_evaluated", "domain 4 bullet 1", paste0("skill ", s, ": reviewed_at is blank before its dated first use"), mf, owner)
      if (!length(units) && !all(all_st == "approved"))
        add("not_evaluated", "domain 4 bullet 2", paste0("skill ", s, ": no unit is named by its reference path ",
            "or cited in a literal input, and unit(s) not approved exist (", paste(names(all_st)[all_st != "approved"],
            collapse = ", "), "): the units used are not identified"), mf, owner)
      for (u in units[st != "approved"]) for (tk in ut$task[ut$unit == u]) {
        if (flagged(tk, s, u))
          add("validation_disclosed", "domain 4 bullet 2", paste0("skill ", s, ": unit(s) not approved: ", u,
              " (", st[[u]], "), used by task ", tk, " and flagged in its signed record"), mf, owner)
        else
          add("validation", "domain 4 bullet 2", paste0("skill ", s, ": unit(s) not approved: ", u,
              " (", st[[u]], "), used by task ", tk, " with no flag in its signed record"), mf, owner)
      }
      if (isFALSE(before))
        add("validation", "domain 4 bullet 1", paste0("skill ", s, ": reviewed_at after its first use"), mf, owner)
      if (prob > 0L)
        add("validation", "domain 4 bullet 1", paste0("skill ", s, ": the method manifest does not validate"), mf, owner)
      data.frame(skill = s, record = "MANIFEST.yaml",
                 units = if (length(units)) paste(units, collapse = ", ") else "none identified",
                 statuses = if (length(units)) paste(st, collapse = ", ") else if (all(all_st == "approved"))
                   "all units approved" else paste0("not approved: ", paste(names(all_st)[all_st != "approved"],
                   collapse = ", ")), reviewed_by = m$reviewed_by,
                 reviewed_at = m$reviewed_at, validation_problems = prob, first_use = first,
                 reviewed_before_first_use = before, stringsAsFactors = FALSE)
    } else {
      sm <- file.path(root, "skills", s, "SKILL.md")
      l <- if (file.exists(sm)) readLines(sm, warn = FALSE) else character(0)
      dl <- grep("VALIDATED [0-9]{4}-[0-9]{2}-[0-9]{2}|^Date: [0-9]{4}-[0-9]{2}-[0-9]{2}|^Status: guidance-only", l, value = TRUE)
      data.frame(skill = s, record = "validation record in SKILL.md", units = "",
                 statuses = if (length(dl)) dl[1] else "no validation line found",
                 reviewed_by = "", reviewed_at = "", validation_problems = NA_integer_,
                 first_use = first, reviewed_before_first_use = NA, stringsAsFactors = FALSE)
    }
  })
  skills_table <- do.call(rbind, rows)
  if ("simulation" %in% tasks) {
    tables <- packages[["simulation_package"]]$tables
    e7 <- tables[["Translation equivalence check (E7)"]]
    if (is.null(e7)) e7 <- tables[["Translation equivalence check"]]
    err <- rv_col(e7, "max_rel_err")
    tol <- rv_col(e7, "tolerance")
    ok <- is.numeric(err) && is.numeric(tol) && !anyNA(err) && !anyNA(tol) && all(err <= tol)
    if (!ok) add("validation", "domain 4 bullet 3",
                 "the simulation translation check is absent or outside its tolerance",
                 "simulation_package", "simulation (pmx-simulator) and the SME")
  }
  list(table = skills_table, outcomes = if (length(outs)) do.call(rbind, outs) else rv_outcome())
}

# Cross-document rows: the assessment table's MAR cross-reference (pending
# until close-out: minor, expected), the MAR title-page date equal to the
# declared document date (a line of the title page that reads exactly that
# date), and the Comments blocks reproduced in the MAR (the semantic
# manifest's F-section note count against the reporting package's decisions
# table, and its result line).
# The title page is page 1 of the pdftotext text: the lines before the first
# form feed (pdftotext ends every page with one, so the first line of each
# later page starts with it). Text without a form feed has no identifiable
# title page and fails. The date must stand alone on its line, as the title
# block sets it; a date inside a contents entry, or one of the body's dates
# (deviation log, Comments blocks), does not count (reviewer R-6, AC10).
rv_crossdoc <- function(assessment_lines, mar_lines, declared_date, decisions_n, manifest_lines,
                        evidence = c(assessment = "", mar = "", manifest = ""), section_prefix = "F.") {
  outs <- list()
  pending <- any(grepl("^- MAR: to be delivered", assessment_lines))
  present <- any(grepl("^- MAR:", assessment_lines))
  if (!present) outs[[length(outs) + 1L]] <- rv_outcome("not_evaluated", NA_character_,
    "assessment table: the MAR cross-reference is absent", evidence[["assessment"]])
  ff <- grepl("\f", mar_lines, fixed = TRUE)
  date_ok <- any(ff) && any(trimws(mar_lines[cumsum(ff) == 0L]) == declared_date)
  nf <- sum(startsWith(manifest_lines, paste0("section: ", section_prefix)))
  res <- identical(sub("^# result: ", "", grep("^# result: ", manifest_lines, value = TRUE)), "PASS")
  f_ok <- nf == decisions_n && res
  if (pending) outs[[length(outs) + 1L]] <- rv_outcome("crossref_pending", NA_character_,
    "assessment table: the MAR cross-reference still reads 'to be delivered'; updated at close-out",
    evidence[["assessment"]])
  if (!date_ok) outs[[length(outs) + 1L]] <- rv_outcome("concordance", NA_character_,
    "the MAR title page (page 1) has no line that reads the declared document date", evidence[["mar"]],
    "reporting (pmx-report-writer)")
  if (!f_ok) outs[[length(outs) + 1L]] <- rv_outcome("concordance", NA_character_,
    "the Archived notes reproduced in the MAR differ from the expected count or semantic result",
    evidence[["manifest"]], "reporting (pmx-report-writer)")
  list(table = data.frame(
    check = c("assessment table MAR cross-reference pending (close-out)",
              "MAR title-page date equals the declared document date",
              "Archived notes reproduced in the MAR match the expected count and semantic result"),
    consistent = c(present && !pending, date_ok, f_ok),
    evidence = unname(evidence[c("assessment", "mar", "manifest")]), stringsAsFactors = FALSE),
    outcomes = if (length(outs)) do.call(rbind, outs) else rv_outcome())
}

# ---------------------------------------------------------------------------
# Checklist and findings (Decisions 16 and 17)
# ---------------------------------------------------------------------------

# The 18 checklist rows of skills/review-checklist/SKILL.md (procedure steps
# 1 to 4, lines 29-38; the bullets of domains 1 to 4, lines 43-71), each with
# its evaluating target or reading item. `values` is the named list of the
# check target values; a row whose evaluating target produced no outcome
# (absent, or the reproduction stubbed) reads "not evaluated".
rv_checklist <- function(values) {
  key <- c(paste("procedure step", 1:4), paste("domain 1 bullet", 1:4),
           paste("domain 2 bullet", 1:4), paste("domain 3 bullet", 1:3),
           paste("domain 4 bullet", 1:3))
  text <- c("clean checkout state confirmed, analysis commits identified",
            "fresh store re-blocks at each gate exactly as the originals did",
            "every gate-package number compared with the fresh store; any mismatch critical",
            "the rerun record goes in the review gate package",
            "every stage gate present, signed, dated; verdict markers intact",
            "rejection rounds preserved verbatim",
            "generated packages present between markers, with generation timestamps",
            "out-of-scope stages marked so by a signed record",
            "everything executed was declared or is a recorded amendment or deviation",
            "everything declared was executed or explicitly descoped",
            "seeds, replicate counts and settings in outputs match the declared values",
            "every deviation names its trigger and its approval",
            "every number in packages and reports traces to a target in a store",
            "rendered blocks state their source; nothing inside the markers altered (history)",
            "figures referenced by gates exist at the recorded paths and match the store outputs",
            "every reference implementation used carries a validation record signed before use",
            "any unvalidated use was flagged at the time",
            "analysis-authored numerics passed their mandatory equivalence checks")
  line <- c(29L, 31L, 34L, 37L, 43L, 44L, 45L, 46L, 50L, 52L, 53L, 55L, 58L, 60L, 62L, 66L, 69L, 71L)
  source <- c("review_gate_integrity", "review_reproduction", "review_reproduction", "review_reproduction",
              rep("review_gate_integrity", 4), rep("review_map_concordance", 4),
              "review_traceability", "review_gate_integrity", "review_reproduction",
              rep("review_validation", 3))
  by <- c("review_gate_integrity (checkout receipt)", "review_reproduction", "review_reproduction",
          "review_package (the reproduction record)", "review_gate_integrity",
          "reading item (pmx-reviewer, SME), with the rejection count of review_gate_integrity",
          "review_gate_integrity", "review_gate_integrity (registry coverage)",
          "reading item (pmx-reviewer, SME), with review_map_concordance (b)",
          "reading item (pmx-reviewer, SME), with review_map_concordance (c)",
          "review_map_concordance (a)",
          "reading item (pmx-reviewer, SME), with review_map_concordance (b)",
          "review_traceability and review_reproduction; sampled re-tracing a reading item",
          "review_gate_integrity (canon2 and the history census)",
          "review_gate_integrity (rehash) and review_reproduction (file comparison)",
          "review_validation", "review_validation", "review_validation")
  reading <- key %in% c("domain 1 bullet 2", "domain 2 bullet 1", "domain 2 bullet 2", "domain 2 bullet 4")
  absent <- vapply(source, function(s) is.null(values[[s]]) || isTRUE(values[[s]]$skipped), logical(1))
  data.frame(item = paste0(key, ": ", text), skill_line = line, evaluated_by = by,
             outcome = ifelse(absent, "not evaluated", ifelse(reading, "reading item", "met")),
             finding = "", stringsAsFactors = FALSE)
}

# (Recommended)"). `outcomes` are rv_outcome rows; every checklist row "not
# evaluated" adds a major row. Returns the log and the checklist with each
# row's finding ids, and "not met" where a critical or major finding names the
# row.
rv_findings <- function(outcomes, checklist, findings_path = "findings-log.csv") {
  rules <- data.frame(rule = c(
    "errored_set", "attestation", "comparison_mismatch", "file_mismatch", "text_rule", "gate_record",
    "promotion", "validation", "census_threshold", "concordance", "inventory_undeclared",
    "section6_differs", "checkout", "history_after_sign", "registry", "not_evaluated",
    "inventory_expected", "section6_pending", "crossref_pending", "outside_attestation",
    "validation_disclosed"),
    severity = c(rep("critical", 8), rep("major", 5), "major (reading item)", "major", "major",
                 "minor (expected)", "minor (expected)", "minor (expected)", "minor (recorded limitation)",
                 "minor (recorded limitation)"),
    owner = c("owning stage of the first errored target", "root (environment)", "owning stage of the package",
              "owning stage of the file", "reporting (report-writer)", "owning stage", "reporting",
              "the skill's owner and the SME", "report-writer", "owning stage", "owning module",
              "root and reporting", "root", "root and SME", "root and SME", "coderx and reviewer",
              "root (close-out record)", "root and SME (close-out MAP install)",
              "pmx-midd-strategist (close-out)", "harness owner (after close)",
              "the skill's owner and the SME"),
    stringsAsFactors = FALSE)
  key <- sub(":.*$", "", checklist$item)
  unknown <- setdiff(outcomes$checklist_item[!is.na(outcomes$checklist_item)], key)
  if (length(unknown)) stop("rv_findings: unknown checklist_item key(s): ",
                            paste(unknown, collapse = ", "), call. = FALSE)
  ne <- which(checklist$outcome == "not evaluated")
  outcomes <- rbind(outcomes, rv_outcome(rep("not_evaluated", length(ne)), key[ne],
    paste0("checklist item '", checklist$item[ne], "' was not evaluated: its evaluating target produced no outcome"),
    findings_path))
  unknown <- setdiff(outcomes$rule, rules$rule)
  if (length(unknown)) stop("rv_findings: unknown rule(s): ", paste(unknown, collapse = ", "), call. = FALSE)
  r <- rules[match(outcomes$rule, rules$rule), ]
  domain <- ifelse(is.na(outcomes$checklist_item), "outside the checklist (render census, cross-document)",
            ifelse(startsWith(outcomes$checklist_item, "procedure"), "reproduction procedure",
            ifelse(startsWith(outcomes$checklist_item, "domain 1"), "1. gate-record integrity",
            ifelse(startsWith(outcomes$checklist_item, "domain 2"), "2. MAP-vs-execution concordance",
            ifelse(startsWith(outcomes$checklist_item, "domain 3"), "3. number traceability",
                   "4. validation-status audit")))))
  log <- data.frame(id = if (nrow(outcomes)) sprintf("F%02d", seq_len(nrow(outcomes))) else character(0),
                    checklist_item = ifelse(is.na(outcomes$checklist_item), "", outcomes$checklist_item),
                    domain = domain, severity = r$severity,
                    owner = ifelse(is.na(outcomes$owner), r$owner, outcomes$owner),
                    finding = outcomes$finding, evidence_path = outcomes$evidence_path,
                    resolution = rep("open (see record Comments)", nrow(outcomes)),
                    verified_by = rep("open (see record Comments)", nrow(outcomes)),
                    status = rep("open", nrow(outcomes)), stringsAsFactors = FALSE)
  for (i in seq_along(key)) {
    ids <- log$id[log$checklist_item == key[i]]
    checklist$finding[i] <- paste(ids, collapse = "; ")
    serious <- any(log$checklist_item == key[i] & !startsWith(log$severity, "minor"))
    if (identical(checklist$outcome[i], "met") && serious) checklist$outcome[i] <- "not met"
  }
  list(log = log, checklist = checklist)
}
