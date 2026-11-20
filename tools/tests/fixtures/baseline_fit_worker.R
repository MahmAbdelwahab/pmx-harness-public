# Run only as the Rscript entry point; recursive sourcing defines nothing.
if (base::sys.nframe() == 0L && base::local({
  fa <- base::grep("^--file=", base::commandArgs(FALSE), value = TRUE)
  base::length(fa) > 0L && base::identical(
    base::basename(base::sub("^--file=", "", fa[[1L]])), "baseline_fit_worker.R")
})) base::local({
# This worker must never create its own containment marker.
refuse <- function(...) stop("baseline fixture: ", ..., call. = FALSE)
copy_root <- normalizePath(getwd(), mustWork = TRUE)
marker <- file.path(copy_root, ".baseline-fixture-copy")
if (!startsWith(copy_root, "/tmp/pmx-harness-baseline-") ||
    !file.exists(marker) || nzchar(Sys.readlink(marker))) refuse("external wrapper copy required")
marker_lines <- readLines(marker, warn = FALSE)
if (length(marker_lines) != 3L || !startsWith(marker_lines[[1L]], "original_root=") ||
    marker_lines[[2L]] != paste0("copy_root=", copy_root) ||
    marker_lines[[3L]] != paste0("nonce=", basename(copy_root))) refuse("marker mismatch")
original_root <- substring(marker_lines[[1L]], nchar("original_root=") + 1L)
if (copy_root == original_root || startsWith(copy_root, paste0(original_root, "/")) ||
    startsWith(original_root, paste0(copy_root, "/"))) refuse("copy ancestry")
guard <- function(path) {
  if (normalizePath(getwd(), mustWork = TRUE) != copy_root ||
      !identical(readLines(marker, warn = FALSE), marker_lines)) refuse("containment drift")
  p <- normalizePath(dirname(path), mustWork = TRUE)
  link <- Sys.readlink(path)
  if (!(p == copy_root || startsWith(p, paste0(copy_root, "/"))) ||
      (!is.na(link) && nzchar(link))) refuse("fixture write escape")
  invisible(path)
}
worker <- normalizePath("tools/tests/fixtures/baseline_fit_worker.R", mustWork = TRUE)
if (!startsWith(worker, paste0(copy_root, "/"))) refuse("worker source escape")
for (pkg in c("yaml", "digest", "jsonlite", "targets", "nmrec"))
  if (!requireNamespace(pkg, quietly = TRUE)) refuse("required public package unavailable: ", pkg)
write_fixture <- function(path, lines) {
  guard(path)
  # Keep synthetic creation after the measured start despite filesystem rounding.
  Sys.sleep(0.02)
  writeLines(lines, path)
  invisible(path)
}
dir_fixture <- function(path) { guard(path); if (!dir.create(path)) refuse("fixture dir exists"); path }
pass <- function(label) cat("PASS ", label, "\n", sep = "")
fails <- character()
test <- function(label, action) {
  result <- tryCatch({ action(); TRUE }, error = function(e) {
    fails <<- c(fails, label)
    detail <- gsub("[0-9]", "d", conditionMessage(e))
    detail <- gsub("SYNTHETIC_SENTINEL_NEVER_REPORT", "[omitted fixture field]", detail, fixed = TRUE)
    guard("failure-details.log")
    cat(label, ": ", detail, "\n", sep = "", file = "failure-details.log", append = TRUE)
    FALSE
  })
  cat(if (result) "PASS " else "FAIL ", label, "\n", sep = "")
  invisible(result)
}
must_refuse <- function(expr, cause = NULL) {
  value <- tryCatch({ force(expr); NULL }, error = base::identity)
  if (!inherits(value, "error")) refuse("expected refusal")
  if (!is.null(cause) && !grepl(cause, conditionMessage(value), fixed = TRUE))
    refuse("wrong refusal cause: ", conditionMessage(value))
}
sha <- function(p) digest::digest(file = p, algo = "sha256")
source_paths <- readLines("copy-files.txt", warn = FALSE)
initial_hashes <- stats::setNames(vapply(source_paths, sha, character(1)), source_paths)

# Generate only a DRAFT manifest, snapshot it, then separately confirm fiction.
mm <- new.env(parent = baseenv())
base::sys.source("tools/method_manifest.R", envir = mm)
deps <- c("tools/method_manifest.R", "tools/load_reference.R", "tools/engine_config.R",
          "tools/yaml_scalar.R", "tools/r_lock.R", "config/r-packages.lock",
          "skills/nonmem-estimation/reference/bbr_estimation.R", "skills/nonmem-estimation/MANIFEST.yaml")
guard("skills/baseline-fit/MANIFEST.yaml")
mm$write_method_manifest("skills/baseline-fit", list(baseline = list(
  reference_path = "skills/baseline-fit/reference/baseline_fit.R",
  entrypoints = c("reference_baseline_fit", "reference_baseline_summary"),
  status = "draft", evidence = "Guarded synthetic wiring proof only; no numerical validation",
  reason = "Inert candidate awaiting owner-led method review", depends = deps)))
draft <- readLines("skills/baseline-fit/MANIFEST.yaml", warn = FALSE)
write_fixture("DRAFT-MANIFEST.yaml", draft)
loader <- new.env(parent = baseenv()); base::sys.source("tools/load_reference.R", envir = loader)
ref <- loader$load_reference("skills/baseline-fit", "skills/baseline-fit/reference/baseline_fit.R")
test("draft fit refuses before operations", function() must_refuse(ref$reference_baseline_fit(NULL, NULL)))
test("draft summary independently refuses", function() must_refuse(ref$reference_baseline_summary(NULL)))
test("manifest static self-gate census", function() mm$validate_method_manifest("skills/baseline-fit/MANIFEST.yaml"))
test("unauthenticated source independently refuses", function() {
  raw <- new.env(parent = baseenv())
  base::sys.source("skills/baseline-fit/reference/baseline_fit.R", envir = raw)
  must_refuse(raw$reference_baseline_fit(NULL, NULL))
  must_refuse(raw$reference_baseline_summary(NULL))
})
# This fictitious confirmation is confined to the guarded copy. The preserved
# draft candidate above is the only manifest eligible for installation.
confirmed <- draft
confirmed <- sub('^reviewed_by:.*$', 'reviewed_by: "Alex Rowan"', confirmed)
confirmed <- sub('^reviewed_at:.*$', paste0('reviewed_at: "', format(Sys.time(), "%Y-%m-%d", tz = "UTC"), '"'), confirmed)
confirmed <- sub('^    status: draft$', '    status: approved', confirmed)
write_fixture("skills/baseline-fit/MANIFEST.yaml", confirmed)
test("BASELINE-PUBLIC: disposable local confirmation validates self-gates", function() {
  mm$validate_method_manifest("skills/baseline-fit/MANIFEST.yaml")
  stopifnot(isTRUE(mm$is_method_validated("skills/baseline-fit",
    "skills/baseline-fit/reference/baseline_fit.R", "baseline")))
})
test("both exports pass fixture gate before structural refusal", function() {
  fit_error <- tryCatch(ref$reference_baseline_fit(NULL, NULL), error = base::identity)
  summary_error <- tryCatch(ref$reference_baseline_summary(NULL), error = base::identity)
  stopifnot(inherits(fit_error, "error"), grepl("identity fields", conditionMessage(fit_error), fixed = TRUE),
            inherits(summary_error, "error"), grepl("fit receipt fields", conditionMessage(summary_error), fixed = TRUE))
})
for (path in c("skills/baseline-fit/reference/baseline_fit.R", "tools/yaml_scalar.R",
               "skills/nonmem-estimation/reference/bbr_estimation.R")) {
  test(paste("authenticated byte drift refuses", path), function() {
    original <- readLines(path, warn = FALSE)
    on.exit(write_fixture(path, original), add = TRUE)
    write_fixture(path, c(original, "# synthetic drift"))
    must_refuse(loader$load_reference("skills/baseline-fit", "skills/baseline-fit/reference/baseline_fit.R"))
  })
}

fixture_root <- dir_fixture(file.path(copy_root, "synthetic"))
inputs_root <- dir_fixture(file.path(fixture_root, "inputs"))
runs_root <- dir_fixture(file.path(fixture_root, "runs"))
model <- write_fixture(file.path(inputs_root, "baseline.mod"),
                       c("$PROBLEM synthetic wiring", "$DATA data.txt IGNORE=@", "; later $DATA comment is allowed"))
data <- write_fixture(file.path(inputs_root, "data.txt"), c("synthetic fixture only"))
disposition <- write_fixture(file.path(inputs_root, "disposition.txt"), "Synthetic disposition; no scientific interpretation")
config_source <- file.path(inputs_root, "engine.yaml")
body <- list(engines = list(nonmem = list(provision = "native", nm_version_label = "SYNTHETIC",
                                        native_executable = "synthetic-engine")),
             bbi = list(mode = "local", args = list(clean_lvl = 1L, copy_lvl = 1L, threads = 1L,
                         nodes = 1L, overwrite = FALSE, parallel = FALSE, parafile = NULL)))
guard(config_source); yaml::write_yaml(body, config_source)
synthetic_bbi <- write_fixture(file.path(inputs_root, "synthetic-bbi"), "inert; never executed")
synthetic_engine <- write_fixture(file.path(inputs_root, "synthetic-engine"), "inert; never executed")
recipe_path <- file.path(inputs_root, "recipe.json")
bbi_bytes <- "synthetic bbi configuration; never consumed by an engine"
bbi_template <- write_fixture(file.path(inputs_root, "expected-bbi.yaml"), bbi_bytes)
recipe <- list(schema = 1L, versions = list(bbr = "1.14.2", nmrec = "synthetic", bbi = "v3.4.0"),
               bbi_executable = list(path = synthetic_bbi, sha256 = sha(synthetic_bbi)),
               engine_config = list(source = list(path = config_source, sha256 = sha(config_source)), body = body),
               provisioning = list(nonmem_home = inputs_root,
                                   executable = list(path = synthetic_engine, sha256 = sha(synthetic_engine))),
               execution = list(mode = "local", wait = TRUE, child_dirs = TRUE, bbi_config_sha256 = sha(bbi_template)),
               r_provenance = list(expected = "managed"))
write_recipe <- function(value = recipe) {
  guard(recipe_path); jsonlite::write_json(value, recipe_path, auto_unbox = TRUE, null = "null", pretty = TRUE)
  list(path = recipe_path, sha256 = sha(recipe_path))
}
identity <- list(analysis_id = "synthetic-baseline", model = ref$.baseline_file(model),
                 data = ref$.baseline_file(data), disposition = ref$.baseline_file(disposition),
                 data_binding = ref$.baseline_binding(model, data), intended_use = "Synthetic wiring proof only")
counter <- new.env(parent = emptyenv()); counter$submits <- 0L; counter$checked <- NULL
counter$mode <- "managed"; counter$mutation <- ""; counter$residual <- 0L
recorder <- new.env(parent = baseenv())
recorder$setNames <- stats::setNames
base::sys.source("tools/r_lock.R", envir = recorder)
old <- loader$load_reference("skills/nonmem-estimation", "skills/nonmem-estimation/reference/bbr_estimation.R")
synthetic_config <- function(mode) {
  if (!mode %in% c("managed", "site")) refuse("invalid synthetic mode")
  path <- file.path(copy_root, "harness.yaml")
  guard(path)
  yaml::write_yaml(list(r_provision = mode, unrelated_fixture_sentinel = "SYNTHETIC_SENTINEL_NEVER_REPORT"), path)
}
ops <- new.env(parent = emptyenv())
ops$identity <- function(r) {
  if (!identical(r$versions, recipe$versions)) refuse("fake version mismatch")
  for (artifact in list(r$bbi_executable, r$provisioning$executable)) {
    if (!identical(artifact$sha256, sha(artifact$path))) refuse("fake executable mismatch")
  }
}
ops$resolve <- function(model_dir, repo_root) {
  path <- file.path(model_dir, "run-config.yaml")
  if (counter$mutation == "resolved-source") path <- config_source
  cfg <- yaml::read_yaml(path)
  list(cfg = cfg, path = normalizePath(path), sha256 = sha(path))
}
ops$workspace <- function(model_dir, engine_config, repo_root, nonmem_home, bbi_path) {
  stopifnot(identical(repo_root, copy_root), identical(bbi_path, synthetic_bbi),
            identical(nonmem_home, inputs_root))
  write_fixture(file.path(model_dir, "bbi.yaml"), bbi_bytes)
  synthetic_config(counter$mode)
  # Actual unchanged metadata producer, fixed cwd, fixed known synthetic config.
  guard(file.path(model_dir, "engine-config-used"))
  package_path <- recorder$write_r_packages_used(model_dir)
  if (counter$mutation == "packages-missing") {
    guard(package_path); guard(paste0(package_path, ".evidence"))
    stopifnot(file.rename(package_path, paste0(package_path, ".evidence")))
  }
  if (counter$mutation == "packages-stale") Sys.setFileTime(package_path, Sys.time() - 30)
  if (counter$mutation == "packages-ambiguous")
    write_fixture(file.path(dirname(package_path), "other-r-packages.yaml"), readLines(package_path))
  if (counter$mutation == "packages-malformed")
    write_fixture(package_path, sub("# r_provision: managed", "# r_provision:", readLines(package_path), fixed = TRUE))
  if (counter$mutation == "packages-duplicate")
    write_fixture(package_path, c(readLines(package_path), "# r_provision: managed"))
  if (counter$mutation == "stale-bbi") Sys.setFileTime(file.path(model_dir, "bbi.yaml"), Sys.time() - 60)
  if (counter$mutation == "bbi-config") write_fixture(file.path(model_dir, "bbi.yaml"), "drift")
}
ops$snapshot <- function(model_dir, model_path, mode, resolved) {
  Sys.sleep(0.02)
  p <- old$.reference_nm_fit_snapshot(model_dir, model_path, mode, resolved)
  if (startsWith(counter$mutation, "snapshot-")) {
    lines <- readLines(p)
    variant <- sub("snapshot-", "", counter$mutation, fixed = TRUE)
    lines <- switch(variant,
      producer = sub("written by reference_nm_fit", "written by unrelated", lines, fixed = TRUE),
      model = sub(paste0("# model: ", model_path), paste0("# model: ", disposition), lines, fixed = TRUE),
      source = sub(paste0("# config_source: ", resolved$path), paste0("# config_source: ", config_source), lines, fixed = TRUE),
      digest = sub(paste0("# config_sha256: ", resolved$sha256), paste0("# config_sha256: ", sha(disposition)), lines, fixed = TRUE),
      mode = sub("# mode: local", "# mode: sge", lines, fixed = TRUE),
      timestamp = sub("^# invoked_at_utc:.*", "# invoked_at_utc: invalid", lines),
      old = sub("^# invoked_at_utc:.*", paste0("# invoked_at_utc: ", format(Sys.time() - 120, "%Y%m%dT%H%M%SZ", tz = "UTC")), lines),
      duplicate = c(lines, "# mode: local"),
      body = sub("threads: 1", "threads: 2", lines, fixed = TRUE))
    write_fixture(p, lines)
  }
  if (counter$mutation == "stale-snapshot") Sys.setFileTime(p, Sys.time() - 60)
  p
}
ops$fresh <- old$.reference_nm_require_fresh
ops$termination_lines <- old$.reference_nm_termination_lines
ops$termination_text <- old$.reference_nm_termination_text
ops$new_model <- function(path) {
  mod <- new.env(parent = emptyenv()); mod$path <- path
  write_fixture(sub("\\.mod$", ".yaml", path), "synthetic constructor metadata")
  if (counter$mutation == "suffix") write_fixture(sub("\\.mod$", ".ctl", path), "ambiguous")
  if (counter$mutation == "original") write_fixture(data, "changed synthetic original")
  if (counter$mutation == "original-model") write_fixture(model, "changed synthetic original")
  if (counter$mutation == "disposition") write_fixture(disposition, "changed disposition")
  if (counter$mutation == "recipe") write_fixture(recipe_path, "changed recipe")
  if (counter$mutation == "config-source") write_fixture(config_source, "changed config")
  if (counter$mutation == "provision") write_fixture(synthetic_engine, "changed provision")
  if (counter$mutation == "staged-data") write_fixture(file.path(dirname(path), "data.txt"), "changed staged data")
  if (counter$mutation == "staged-config") write_fixture(file.path(dirname(path), "run-config.yaml"), "changed config")
  if (counter$mutation == "stale-config") Sys.setFileTime(file.path(dirname(path), "bbi.yaml"), Sys.time() - 60)
  if (counter$mutation == "staged") write_fixture(path, "changed staged model")
  if (counter$mutation == "stale") dir_fixture(sub("\\.mod$", "", path))
  if (counter$mutation == "packages-drift") {
    p <- list.files(file.path(dirname(path), "engine-config-used"), pattern = "-r-packages\\.yaml$", full.names = TRUE)
    write_fixture(p, c(readLines(p), "# synthetic post-capture drift"))
  }
  mod
}
ops$model_path <- function(mod) mod$path
ops$data_path <- function(mod) {
  counter$checked <- mod
  if (counter$mutation == "accessor") return(disposition)
  file.path(dirname(mod$path), "data.txt")
}
ops$select <- function(bbi) { previous <- counter$bbi; counter$bbi <- bbi; previous }
ops$restore <- function(previous) { counter$bbi <- previous; invisible(NULL) }
ops$submit <- function(mod) {
  stopifnot(identical(mod, counter$checked), identical(counter$bbi, synthetic_bbi))
  counter$submits <- counter$submits + 1L
  output <- dir_fixture(sub("\\.mod$", "", mod$path))
  write_fixture(file.path(output, "baseline.lst"), "0MINIMIZATION SUCCESSFUL")
}
ops$heuristics <- function(mod) list(minimization_successful = FALSE, note = "Synthetic disagreement retained")
ops$parameters <- function(mod) data.frame(parameter_names = c("SYNTHETIC_A", "SYNTHETIC_B"),
                                          estimate = c(1, 2), fixed = c(FALSE, TRUE), stderr = c(0.1, NA_real_))
ops$residual <- function(...) { counter$residual <- counter$residual + 1L; refuse("residual join forbidden") }
lockEnvironment(ops, bindings = TRUE)
api <- ref$.baseline_factory(ops)
serial <- 0L
spec <- function(r = recipe) {
  serial <<- serial + 1L
  list(invocation_root = file.path(runs_root, paste0("case", serial)), run_id = "baseline", recipe = write_recipe(r))
}
negative <- function(label, identity_value = identity, recipe_value = recipe, mutation = "", spec_edit = NULL, cause = NULL) {
  test(label, function() {
    before <- counter$submits; counter$mutation <- mutation
    original_bytes <- lapply(c(model, data, disposition, config_source, synthetic_engine), readLines, warn = FALSE)
    on.exit({
      counter$mutation <- ""
      for (i in seq_along(original_bytes)) write_fixture(c(model, data, disposition, config_source, synthetic_engine)[[i]], original_bytes[[i]])
    }, add = TRUE)
    run <- spec(recipe_value)
    if (is.function(spec_edit)) run <- spec_edit(run)
    must_refuse(api$fit(identity_value, run), cause)
    stopifnot(counter$submits == before)
  })
}
negative("relative invocation root", spec_edit = function(x) {
  x$invocation_root <- substring(x$invocation_root, nchar(copy_root) + 2L)
  x
}, cause = "invocation_root must be the absolute canonical path")
test("relative invocation root creates no directory", function() {
  run <- spec()
  run$invocation_root <- substring(run$invocation_root, nchar(copy_root) + 2L)
  before <- counter$submits
  must_refuse(api$fit(identity, run), "invocation_root must be the absolute canonical path")
  stopifnot(!dir.exists(run$invocation_root), counter$submits == before)
})
test("configuration values beyond four digits compare unequal", function() {
  stopifnot(identical(ref$.baseline_equal(list(a = 1.000001), list(a = 1.000002)), FALSE))
})
test("equal configurations compare equal", function() {
  stopifnot(isTRUE(ref$.baseline_equal(list(a = 1.000001), list(a = 1.000001))))
})
for (helper in c("r_lock.R", "yaml_scalar.R")) {
  test(paste("O-11 base-parented helper", helper), function() {
    isolated <- new.env(parent = baseenv())
    base::sys.source(file.path("tools", helper), envir = isolated)
    path <- file.path(inputs_root, paste0(helper, ".txt"))
    write_fixture(path, if (helper == "r_lock.R") "stats: 1.0.0" else "# empty mapping")
    error <- tryCatch({
      if (helper == "r_lock.R") isolated$verify_r_packages(path)
      else stopifnot(identical(isolated$read_reviewed_yaml(path), stats::setNames(list(), character(0))))
      NULL
    }, error = function(e) conditionMessage(e))
    if (!is.null(error)) cat("O-11 receipt: ", helper, ": ", error, "\n", sep = "")
    stopifnot(is.null(error))
  })
}
for (variant in c("producer", "model", "source", "digest", "mode", "timestamp", "old", "duplicate", "body"))
  negative(paste("fit snapshot relationship", variant), mutation = paste0("snapshot-", variant),
           cause = "fit snapshot relationship")
for (variant in c("missing", "stale", "ambiguous", "malformed", "duplicate", "drift")) {
  cause <- switch(variant, missing = "missing, reused or ambiguous R snapshot",
    stale = "predates the run start", ambiguous = "missing, reused or ambiguous R snapshot",
    malformed = "R snapshot header", duplicate = "R snapshot header", drift = "final provenance drift")
  negative(paste("fit package snapshot", variant), mutation = paste0("packages-", variant), cause = cause)
}
broken <- recipe; broken$r_provenance$expected <- "invalid"
negative("fit package invalid expectation cause", recipe_value = broken, cause = "invalid R provenance expectation")
broken <- recipe; broken$r_provenance$expected <- NULL
negative("fit package missing expectation cause", recipe_value = broken, cause = "invalid r_provenance fields")
# No execution result is inferred from the presence of these assertions.
for (field in names(identity)) {
  broken <- identity; broken[[field]] <- NULL
  negative(paste("missing identity", field), broken)
}
broken <- identity; broken$unknown <- TRUE; negative("unknown identity field", broken)
broken <- identity; names(broken)[[2L]] <- names(broken)[[1L]]; negative("duplicate identity field", broken)
broken <- identity; broken$model$sha256 <- paste(rep("0", 64), collapse = ""); negative("identity hash mismatch", broken)
for (field in names(recipe)) {
  broken <- recipe; broken[field] <- NULL
  negative(paste("missing recipe", field), recipe_value = broken)
}
for (key in c("mode", "wait", "child_dirs", "bbi_config_sha256")) {
  broken <- recipe; broken$execution[key] <- NULL
  negative(paste("missing execution", key), recipe_value = broken)
}
broken <- recipe; broken$unknown <- TRUE; negative("unknown recipe field", recipe_value = broken)
broken <- recipe; broken$execution$mode <- "sge"; negative("nonlocal execution", recipe_value = broken)
broken <- recipe; broken$execution$child_dirs <- FALSE; negative("unsupported child behavior", recipe_value = broken)
broken <- recipe; broken$engine_config$body$bbi$args$overwrite <- TRUE; negative("overwrite forbidden", recipe_value = broken)
broken <- recipe; broken$r_provenance$expected <- "invalid"; negative("invalid R expectation", recipe_value = broken)
broken <- recipe; broken$r_provenance$expected <- "site"; negative("actual recorder mode mismatch", recipe_value = broken)
broken <- recipe; broken$bbi_executable$sha256 <- paste(rep("0", 64), collapse = ""); negative("executable mismatch", recipe_value = broken)
broken <- recipe; broken$versions$nmrec <- "drift"; negative("loaded version mismatch", recipe_value = broken)
broken <- recipe; broken$engine_config$body$bbi$args$threads <- 2L; negative("config source body mismatch", recipe_value = broken)
for (mutation in c("resolved-source", "bbi-config", "suffix", "staged", "stale", "accessor"))
  negative(paste("pre-submit", mutation), mutation = mutation)
for (mutation in c("original", "original-model", "disposition", "recipe", "config-source"))
  negative(paste("targeted drift", mutation), mutation = mutation, cause = "artifact digest mismatch")
for (mutation in c("staged-data", "staged"))
  negative(paste("targeted drift", mutation), mutation = mutation, cause = "checked model/data differs")
negative("targeted staged config drift", mutation = "staged-config", cause = "staged config source/body mismatch")
negative("targeted provision drift", mutation = "provision", cause = "fake executable mismatch")
negative("targeted stale provisioning config", mutation = "stale-bbi", cause = "predates the run start")
negative("targeted stale snapshot", mutation = "stale-snapshot", cause = "predates the run start")
negative("targeted suffix ambiguity", mutation = "suffix", cause = "stale output or ambiguous model suffix")
negative("targeted stale output", mutation = "stale", cause = "stale output or ambiguous model suffix")
negative("targeted parser accessor disagreement", mutation = "accessor", cause = "checked model/data differs")
negative("invocation escape", spec_edit = function(x) {x$invocation_root <- "/tmp/unowned-baseline-output"; x})
negative("recipe digest mismatch", spec_edit = function(x) {x$recipe$sha256 <- paste(rep("0", 64), collapse = ""); x})
negative("duplicate recipe JSON key", spec_edit = function(x) {
  lines <- readLines(recipe_path, warn = FALSE)
  write_fixture(recipe_path, c('{"schema": 1,', substring(paste(lines, collapse = "\n"), 2L)))
  x$recipe$sha256 <- sha(recipe_path); x
})
negative("missing staged config source", spec_edit = function(x) {
  r <- recipe; r$engine_config$source$path <- file.path(inputs_root, "absent.yaml")
  x$recipe <- write_recipe(r); x
})
test("DATA comment-first refuses", function() {
  p <- write_fixture(file.path(inputs_root, "comment.mod"), c("; first $DATA bad", "$DATA data.txt"))
  must_refuse(ref$.baseline_binding(p, data))
})
for (variant in c("absolute", "multiple", "quoted", "multiline", "extra", "lowercase")) test(paste("targeted DATA", variant), function() {
  bytes <- readLines(model)
  on.exit(write_fixture(model, bytes), add = TRUE)
  changed <- switch(variant,
    absolute = c("$PROBLEM synthetic", paste("$DATA", data)),
    multiple = c("$PROBLEM synthetic", "$DATA data.txt", "$DATA data.txt"),
    quoted = c("$PROBLEM synthetic", '$DATA "data.txt"'),
    multiline = c("$PROBLEM synthetic", "$DATA", "data.txt"),
    extra = c("$PROBLEM synthetic", "$DATA data.txt", "$MSFI extra"),
    lowercase = c("$PROBLEM synthetic", "$data data.txt"))
  write_fixture(model, changed)
  other <- identity; other$model <- ref$.baseline_file(model)
  before <- counter$submits
  must_refuse(api$fit(other, spec()), "unsupported DATA")
  stopifnot(counter$submits == before)
})
test("targeted symlink identity refuses", function() {
  p <- file.path(inputs_root, "linked-data.txt"); guard(p)
  stopifnot(file.symlink(data, p))
  other <- identity; other$data <- list(path = p, sha256 = sha(data))
  before <- counter$submits
  must_refuse(api$fit(other, spec()), "symlink artifact")
  stopifnot(counter$submits == before)
})
test("targeted marker mismatch refuses", function() {
  on.exit(writeLines(marker_lines, marker), add = TRUE)
  writeLines(c(marker_lines, "synthetic mismatch"), marker)
  must_refuse(guard(file.path(inputs_root, "escape.txt")), "containment drift")
})
test("targeted write path escape refuses", function() {
  must_refuse(guard(file.path(copy_root, "..", "escape.txt")), "fixture write escape")
})
test("DATA active followed by comment accepted", function() ref$.baseline_binding(model, data))
test("snapshot missing refuses", function() {
  empty <- dir_fixture(file.path(fixture_root, "snapshot-empty"))
  must_refuse(ref$.baseline_packages(empty, character(), Sys.time(), "managed", ops$fresh))
})
for (mode in c("managed", "site")) test(paste("real metadata producer", mode), function() {
  work <- dir_fixture(file.path(fixture_root, paste0("recorder-", mode)))
  synthetic_config(mode); start <- Sys.time(); recorder$write_r_packages_used(work)
  receipt <- ref$.baseline_packages(work, character(), start, mode, ops$fresh)
  stopifnot(identical(receipt$header$r_provision, mode),
            !any(grepl("SYNTHETIC_SENTINEL_NEVER_REPORT", readLines(receipt$path), fixed = TRUE)),
            !grepl("SYNTHETIC_SENTINEL_NEVER_REPORT", jsonlite::toJSON(receipt), fixed = TRUE))
  must_refuse(ref$.baseline_packages(work, character(), Sys.time() + 30, mode, ops$fresh))
  must_refuse(ref$.baseline_packages(work, receipt$path, start, mode, ops$fresh))
  must_refuse(ref$.baseline_packages(work, character(), start, "invalid", ops$fresh))
  write_fixture(receipt$path, c("# malformed producer", "# unrelated header"))
  must_refuse(ref$.baseline_packages(work, character(), start, mode, ops$fresh))
})
test("ambiguous snapshot refuses", function() {
  work <- dir_fixture(file.path(fixture_root, "recorder-ambiguous"))
  synthetic_config("managed"); start <- Sys.time(); p <- recorder$write_r_packages_used(work)
  write_fixture(file.path(dirname(p), "other-r-packages.yaml"), readLines(p))
  must_refuse(ref$.baseline_packages(work, character(), start, "managed", ops$fresh))
})
test("duplicate snapshot header refuses", function() {
  work <- dir_fixture(file.path(fixture_root, "recorder-duplicate"))
  synthetic_config("managed"); start <- Sys.time(); p <- recorder$write_r_packages_used(work)
  write_fixture(p, c(readLines(p), "# r_provision: managed"))
  must_refuse(ref$.baseline_packages(work, character(), start, "managed", ops$fresh))
})
counter$mode <- "managed"
fit <- NULL
test("positive checked same-object submit exactly once", function() {
  before <- counter$submits
  fit <<- api$fit(identity, spec())
  stopifnot(counter$submits == before + 1L, identical(fit$mod, counter$checked))
})
if (!is.null(fit)) {
  test("raw conditional summary partial uncertainty", function() {
    s <- api$summary(fit)
    stopifnot(identical(s$parameters$estimate, c(1, 2)), identical(s$parameters$fixed, c(FALSE, TRUE)),
              identical(s$stderr_status$status, "partially_available"),
              identical(s$covariance_status$status, "unverified"),
              identical(s$heuristics$minimization_successful, FALSE), counter$residual == 0L)
  })
  test("receipt identity contradiction refuses", function() {
    f <- fit; f$identity_sha256 <- "mismatch"; must_refuse(api$summary(f))
  })
  test("termination contradiction refuses", function() {
    f <- fit; f$termination$text <- "contradiction"; must_refuse(api$summary(f))
  })
  for (key in c("original_model", "original_data", "staged_model", "staged_data")) test(paste("valid artifact contradiction", key), function() {
    f <- fit; f$checked_inputs[[key]] <- ref$.baseline_file(disposition)
    must_refuse(api$summary(f), "receipt checked input relationship mismatch")
  })
  for (key in c("source", "staged", "bbi", "snapshot")) test(paste("valid config contradiction", key), function() {
    f <- fit; f$config[[key]] <- ref$.baseline_file(disposition)
    must_refuse(api$summary(f), "receipt configuration relationship mismatch")
  })
  test("internally consistent wrong termination refuses", function() {
    f <- fit; f$termination$lines <- "0MINIMIZATION TERMINATED"
    f$termination$text <- ops$termination_text(f$termination$lines)
    must_refuse(api$summary(f), "missing or contradictory termination evidence")
  })
  test("valid listing substitution refuses", function() {
    p <- write_fixture(file.path(inputs_root, "other.lst"), "0MINIMIZATION SUCCESSFUL")
    f <- fit; f$listing <- ref$.baseline_file(p)
    must_refuse(api$summary(f), "receipt configuration relationship mismatch")
  })
  test("malformed receipt run spec refuses", function() {
    f <- fit; f$run_spec$callback <- function() refuse("must never run")
    must_refuse(api$summary(f), "invalid run_spec fields")
  })
  test("receipt invocation substitution refuses", function() {
    f <- fit; f$run_spec$invocation_root <- runs_root
    must_refuse(api$summary(f), "receipt checked input relationship mismatch")
  })
  for (variant in c("model", "source", "digest", "body", "mode", "timestamp", "old", "duplicate", "producer"))
    test(paste("summary same-directory snapshot relationship", variant), function() {
      p <- fit$config$snapshot$path
      lines <- readLines(p)
      staged <- fit$config$staged$path
      changed <- switch(variant,
        producer = sub("written by reference_nm_fit", "written by unrelated", lines, fixed = TRUE),
        model = sub(paste0("# model: ", fit$checked_inputs$staged_model$path), paste0("# model: ", model), lines, fixed = TRUE),
        source = sub(paste0("# config_source: ", staged), paste0("# config_source: ", config_source), lines, fixed = TRUE),
        digest = sub(paste0("# config_sha256: ", fit$config$staged$sha256), paste0("# config_sha256: ", sha(disposition)), lines, fixed = TRUE),
        body = sub("threads: 1", "threads: 2", lines, fixed = TRUE),
        mode = sub("# mode: local", "# mode: sge", lines, fixed = TRUE),
        timestamp = sub("^# invoked_at_utc:.*", "# invoked_at_utc: invalid", lines),
        old = sub("^# invoked_at_utc:.*", paste0("# invoked_at_utc: ", format(Sys.time() - 120, "%Y%m%dT%H%M%SZ", tz = "UTC")), lines),
        duplicate = c(lines, "# mode: local"))
      other <- write_fixture(file.path(dirname(p), paste0("unrelated-", variant, ".yaml")), changed)
      f <- fit; f$config$snapshot <- ref$.baseline_file(other)
      must_refuse(api$summary(f), "fit snapshot relationship")
    })
  test("snapshot byte drift refuses summary", function() {
    p <- fit$r_packages$path; bytes <- readLines(p)
    on.exit(write_fixture(p, bytes), add = TRUE)
    write_fixture(p, c(bytes, "# synthetic drift")); must_refuse(api$summary(fit))
  })
  for (variant in c("missing", "available", "malformed")) test(paste("raw stderr", variant), function() {
    other <- new.env(parent = emptyenv())
    for (name in ls(ops, all.names = TRUE)) other[[name]] <- ops[[name]]
    other$parameters <- switch(variant,
      missing = function(mod) data.frame(parameter = "SYNTHETIC", estimate = 1, fixed = FALSE),
      available = function(mod) data.frame(parameter = "SYNTHETIC", estimate = 1, fixed = FALSE, stderr = 0.1),
      malformed = function(mod) data.frame(estimate = 1))
    lockEnvironment(other, bindings = TRUE)
    summarizer <- ref$.baseline_factory(other)$summary
    if (variant == "malformed") must_refuse(summarizer(fit)) else {
      s <- summarizer(fit)
      stopifnot(s$stderr_status$status == if (variant == "missing") "unavailable" else "available")
    }
  })
}

# Production closures and public summary, with inert executables in this copy only.
production_cases <- function() {
  saved_options <- options()
  on.exit(options(saved_options), add = TRUE)
  prior_selection <- getOption("bbr.bbi_exe_path")
  on.exit(options(bbr.bbi_exe_path = prior_selection), add = TRUE)
  if (!requireNamespace("bbr", quietly = TRUE)) refuse("public bbr unavailable")
  # The wrapper only forwards canonical, admitted user-library directories.
  approved_libraries <- strsplit(Sys.getenv("R_LIBS_USER"), .Platform$path.sep, fixed = TRUE)[[1L]]
  for (pkg in c("bbr", "nmrec")) {
    loaded <- normalizePath(getNamespaceInfo(asNamespace(pkg), "path"), mustWork = TRUE)
    stopifnot(dirname(loaded) %in% approved_libraries)
    cat("LOADED_PACKAGE=", pkg, " VERSION=", as.character(utils::packageVersion(pkg)),
        " PATH=", loaded, "\n", sep = "")
  }
  stopifnot(!isTRUE(getOption("bbr.DEV_no_min_version")))
  prod <- ref$.baseline_default_ops()
  root <- dir_fixture(file.path(fixture_root, "production-api"))
  selected_log <- file.path(root, "selected.log")
  decoy_log <- file.path(root, "decoy.log")
  engine_log <- file.path(root, "engine.log")
  response <- file.path(root, "response.json")
  selected <- file.path(root, "selected-bbi")
  decoy <- file.path(root, "decoy-bbi")
  script <- c("#!/bin/bash", "set -eu",
    paste0("printf '%s|%s|%s\\n' \"$0\" \"$PWD\" \"$*\" >> ", shQuote(selected_log)),
    'case "$*" in',
    "  version) printf '%s\\n' 'v3.4.0' ;;",
    paste0("  'nonmem summary baseline --json') while IFS= read -r line || [[ -n $line ]]; do printf '%s\\n' \"$line\"; done < ", shQuote(response), " ;;"),
    "  *) exit 91 ;;", "esac")
  write_fixture(selected, script); Sys.chmod(selected, "0755")
  write_fixture(decoy, c("#!/bin/bash", paste0("printf '%s|%s|%s\\n' \"$0\" \"$PWD\" \"$*\" >> ", shQuote(decoy_log)), "exit 92"))
  Sys.chmod(decoy, "0755")
  engine_dir <- dir_fixture(file.path(inputs_root, "run"))
  engine <- write_fixture(file.path(engine_dir, "synthetic-engine"),
    c("#!/bin/bash", paste0("printf 'forbidden\\n' >> ", shQuote(engine_log)), "exit 93"))
  Sys.chmod(engine, "0755")
  r <- recipe
  r$versions$nmrec <- as.character(utils::packageVersion("nmrec"))
  r$bbi_executable <- ref$.baseline_file(selected)
  r$provisioning$executable <- ref$.baseline_file(engine)
  f <- fit
  f$run_spec$recipe <- write_recipe(r)
  on.exit(write_recipe(recipe), add = TRUE)
  model_path <- f$checked_inputs$staged_model$path
  metadata <- sub("\\.mod$", ".yaml", model_path)
  guard(metadata); guard(paste0(metadata, ".synthetic-evidence"))
  stopifnot(file.rename(metadata, paste0(metadata, ".synthetic-evidence")))
  f$mod <- prod$new_model(model_path)
  stopifnot(identical(prod$model_path(f$mod), model_path),
            identical(prod$data_path(f$mod), f$checked_inputs$staged_data$path))
  output <- normalizePath(bbr::get_output_dir(f$mod), mustWork = TRUE)
  write_response <- function(kind = "numeric") {
    # Pinned bbr aaa.R BBI_NULL_NUM is the exact sentinel below.
    stderr <- switch(kind, numeric = list(theta = c(0.1, 0.2)),
      sentinel = list(theta = c(0.1, -999999999)), null = NULL, missing = NULL)
    parameters <- list(estimates = list(theta = c(1, 2)), fixed = list(theta = c(0, 1)))
    if (kind != "missing") parameters["std_err"] <- list(stderr)
    payload <- list(run_details = list(estimation_method = I(c("FOCE")), only_sim = FALSE),
      run_heuristics = list(minimization_successful = FALSE),
      parameter_names = list(theta = c("THETA1", "THETA2")), parameters_data = list(parameters))
    if (kind == "error") payload <- list(error_msg = "synthetic extraction rejected")
    guard(response)
    jsonlite::write_json(payload, response, auto_unbox = TRUE, null = "null", pretty = TRUE,
                         always_decimal = TRUE)
    # Explicit array wrappers are required even for a single method.
    invisible(NULL)
  }
  assert_logs <- function() {
    stopifnot(!file.exists(decoy_log), !file.exists(engine_log))
    lines <- readLines(selected_log, warn = FALSE)
    summary_lines <- lines[grepl("|nonmem summary ", lines, fixed = TRUE)]
    stopifnot(length(summary_lines) > 0L,
      all(summary_lines == paste(selected, output, "nonmem summary baseline --json", sep = "|")),
      all(lines == paste(selected, copy_root, "version", sep = "|") |
          lines == paste(selected, output, "version", sep = "|") |
          lines == paste(selected, output, "nonmem summary baseline --json", sep = "|")))
  }
  write_response()
  test("production identity and parser fixture", function() {
    prod$identity(r)
    previous <- prod$select(selected); on.exit(prod$restore(previous), add = TRUE)
    p <- prod$parameters(f$mod)
    stopifnot(identical(p$fixed, c(FALSE, TRUE)), identical(p$stderr, c(0.1, 0.2)),
              identical(p$parameter_names, c("THETA1", "THETA2")))
    assert_logs()
  })
  test("production heuristics supported export and restoration", function() {
    options(bbr.bbi_exe_path = decoy)
    h <- local({
      previous <- prod$select(selected); on.exit(prod$restore(previous), add = TRUE)
      prod$heuristics(f$mod)
    })
    stopifnot(identical(h$minimization_successful, FALSE), identical(getOption("bbr.bbi_exe_path"), decoy))
  })
  test("production public summary selects recipe executable", function() {
    options(bbr.bbi_exe_path = decoy)
    s <- tryCatch(ref$reference_baseline_summary(f), error = function(e) {
      if (file.exists(decoy_log)) refuse("summary selection regression: decoy invoked")
      stop(e)
    })
    stopifnot(identical(getOption("bbr.bbi_exe_path"), decoy),
      identical(s$parameters$parameter, c("THETA1", "THETA2")),
      identical(s$parameters$fixed, c(FALSE, TRUE)), identical(s$parameters$stderr, c(0.1, 0.2)),
      identical(s$stderr_status$status, "available"), identical(s$heuristics$minimization_successful, FALSE))
    assert_logs()
  })
  for (kind in c("sentinel", "null", "missing", "numeric")) test(paste("production raw stderr", kind), function() {
    write_response(kind)
    options(bbr.bbi_exe_path = NULL)
    s <- ref$reference_baseline_summary(f)
    stopifnot(!"bbr.bbi_exe_path" %in% names(options()),
      identical(s$parameters$fixed, c(FALSE, TRUE)), nrow(s$parameters) == 2L,
      identical(s$stderr_status$status, switch(kind, sentinel = "partially_available",
        null = "unavailable", missing = "unavailable", numeric = "available")))
    if (kind == "numeric") stopifnot(identical(s$parameters$stderr, c(0.1, 0.2)))
    if (kind == "sentinel") stopifnot(identical(s$parameters$stderr, c(0.1, NA_real_)))
    if (kind %in% c("null", "missing")) stopifnot(all(is.na(s$parameters$stderr)))
    assert_logs()
  })
  test("production extraction error restores selection", function() {
    write_response("error"); options(bbr.bbi_exe_path = decoy)
    must_refuse(ref$reference_baseline_summary(f), "synthetic extraction rejected")
    stopifnot(identical(getOption("bbr.bbi_exe_path"), decoy)); assert_logs()
  })
  test("production identity fails before selection and extraction", function() {
    options(bbr.bbi_exe_path = decoy)
    before <- readLines(selected_log, warn = FALSE)
    broken <- r; broken$versions$nmrec <- "wrong"
    bad <- f; bad$run_spec$recipe <- write_recipe(broken)
    on.exit(write_recipe(r), add = TRUE)
    must_refuse(ref$reference_baseline_summary(bad), "loaded version mismatch")
    stopifnot(identical(getOption("bbr.bbi_exe_path"), decoy),
              identical(before, readLines(selected_log, warn = FALSE)))
    assert_logs()
  })
}
test("production API setup", production_cases)

# Evaluate actual composed commands; approval is a synthetic byte-bound receipt.
# Use the same source scope as the normal pipeline declaration.
compose <- globalenv()
compose$`%||%` <- function(a, b) if (is.null(a)) b else a
base::sys.source("pipeline/compose.R", envir = compose)
test("composed gate dominance and fake attestor selectors", function() {
  synthetic_engine_dir <- dir_fixture(file.path(copy_root, "config-fixture"))
  # The composer source's fixed config/engine.yaml is generated only in this copy.
  guard("config/engine.yaml"); yaml::write_yaml(body, "config/engine.yaml")
  run <- spec()
  project_path <- function(path) substring(path, nchar(copy_root) + 2L)
  pipe <- compose$compose_pipeline("synthetic-baseline", c("baseline_input", "baseline_fit"),
    inputs = list(engine = "nonmem", baseline_input = list(model = project_path(model), data = project_path(data),
      disposition = project_path(disposition), intended_use = identity$intended_use), baseline_fit = list(run_spec = run)))
  stopifnot(compose$verify_gate_dominance(pipe))
  att <- pipe[[1L]]$command$expr[[1L]]
  env <- new.env(parent = globalenv()); env$identity <- base::identity
  env$attest_environment <- function(...) list(...)
  captured <- eval(att, env)
  stopifnot(isTRUE(captured$require_nonmem), isTRUE(captured$require_psn),
            identical(captured$nonmem_provision, "native"),
            identical(captured$pkgs, c("targets", "yaml", "digest", "bbr")))
  by_name <- stats::setNames(pipe, vapply(pipe, function(t) t$settings$name, character(1)))
  command <- by_name$baseline_fit_receipt$command$expr[[1L]]
  env$environment_attestation <- captured; env$method_manifests <- "synthetic"
  facade <- new.env(parent = ref)
  # Environment dollar lookup does not inherit these authenticated helpers.
  for (name in c(".baseline_regular", ".baseline_under", ".baseline_file",
                 ".baseline_binding", ".baseline_identity")) facade[[name]] <- ref[[name]]
  facade$reference_baseline_fit <- function(input_identity, run_spec) {
    ref$.baseline_gate(); api$fit(input_identity, run_spec)
  }
  facade$reference_baseline_summary <- function(fit_receipt) {
    ref$.baseline_gate(); api$summary(fit_receipt)
  }
  lockEnvironment(facade, bindings = TRUE)
  env$load_reference <- function(skill, path) {
    stopifnot(skill == "skills/baseline-fit", path == "skills/baseline-fit/reference/baseline_fit.R")
    facade
  }
  env$evidence_note <- function(label, value) list(label = label, value = value)
  for (name in c("baseline_model_file", "baseline_data_file", "baseline_disposition_file",
                 "baseline_input_identity", "baseline_identity_file", "baseline_input_package"))
    env[[name]] <- eval(by_name[[name]]$command$expr[[1L]], env)
  stopifnot(identical(env$baseline_input_identity, identity),
            identical(env$baseline_input_package$artifacts$path,
                      c(project_path(model), project_path(data), project_path(disposition), env$baseline_identity_file)))
  approval <- new.env(parent = emptyenv()); approval$signed <- FALSE; approval$hashes <- NULL
  env$require_gate <- function(path) {
    if (!isTRUE(approval$signed)) refuse("synthetic approval missing or unsigned")
    package <- if (path == "synthetic-input-gate") env$baseline_input_package else env$baseline_fit_package
    current <- vapply(package$artifacts$path, sha, character(1))
    if (!identical(current, approval$hashes)) refuse("synthetic approval artifact mismatch")
    TRUE
  }
  env$gate_baseline_input_file <- "synthetic-input-gate"
  input_gate <- by_name$gate_baseline_input$command$expr[[1L]]
  before <- counter$submits
  must_refuse(eval(command, env)); stopifnot(counter$submits == before)
  must_refuse(eval(input_gate, env)); stopifnot(counter$submits == before)
  approval$signed <- TRUE; approval$hashes <- "mismatch"
  must_refuse(eval(input_gate, env)); stopifnot(counter$submits == before)
  approval$hashes <- vapply(env$baseline_input_package$artifacts$path, sha, character(1))
  env$gate_baseline_input <- eval(input_gate, env)
  receipt <- eval(command, env)
  stopifnot(counter$submits == before + 1L, identical(receipt$mod, counter$checked))
  gate <- by_name$gate_baseline_fit$command$expr[[1L]]
  env$baseline_fit_receipt <- receipt
  env$baseline_raw_summary <- eval(by_name$baseline_raw_summary$command$expr[[1L]], env)
  env$baseline_fit_package <- eval(by_name$baseline_fit_package$command$expr[[1L]], env)
  env$gate_baseline_fit_file <- "synthetic-output-gate"
  approval$signed <- FALSE; must_refuse(eval(gate, env))
  approval$signed <- TRUE
  approval$hashes <- vapply(env$baseline_fit_package$artifacts$path, sha, character(1))
  stopifnot(isTRUE(eval(gate, env)))
})
test("external replacement refuses", function() {
  must_refuse(compose$compose_pipeline("synthetic-baseline", "baseline_fit",
    inputs = list(engine = "nonmem", baseline_fit = list(external_input = "synthetic"))))
})
for (engine in list(NULL, "nlmixr2", c("nonmem", "nlmixr2"))) test("wrong or default engine refuses", function() {
  before <- counter$submits
  must_refuse(compose$compose_pipeline("synthetic-baseline", c("baseline_input", "baseline_fit"),
    inputs = list(engine = engine, baseline_input = list(model = model, data = data,
      disposition = disposition, intended_use = identity$intended_use), baseline_fit = list(run_spec = spec()))))
  stopifnot(counter$submits == before)
})
test("missing baseline input task refuses", function() {
  must_refuse(compose$compose_pipeline("synthetic-baseline", "baseline_fit",
                                      inputs = list(engine = "nonmem", baseline_fit = list(run_spec = spec()))))
})
test("source originals immutable", function() {
  stopifnot(identical(sha(model), identity$model$sha256), identical(sha(data), identity$data$sha256))
})
test("protected copied sources unchanged", function() {
  paths <- setdiff(source_paths, "skills/baseline-fit/MANIFEST.yaml")
  stopifnot(identical(unname(vapply(paths, sha, character(1))), unname(initial_hashes[paths])))
})
write_fixture("skills/baseline-fit/MANIFEST.yaml", draft)
test("installed candidate is draft and unconfirmed", function() {
  m <- mm$read_method_manifest("DRAFT-MANIFEST.yaml")
  stopifnot(identical(m$units$baseline$status, "draft"), !nzchar(m$reviewed_by),
            identical(draft, readLines("skills/baseline-fit/MANIFEST.yaml")))
})
cat("FAILED_CASES=", length(fails), "\n", sep = "")
if (length(fails)) quit(status = 1L)
pass("baseline guarded inert wiring contract")
})
