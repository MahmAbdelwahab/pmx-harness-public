#!/usr/bin/env Rscript
# Integrity batch 11 battery (Codex round 10 findings, SME decisions 2026-09-04).
#
# Sections:
#   A  S5          requireNamespace()/loadNamespace() do not attach, so they
#                  must not disqualify a file that builds a trusted object;
#                  library()/require()/attach()/attachNamespace() still do.
#   B  R10-P0-2    a statement INSIDE the trusted block that binds any name
#                  other than the returned symbol is a refusal, and the binding
#                  census sees base::assign, do.call("assign", ...) and the
#                  `<-`() functional form.
#   C  R10-P0-1    every manifested entry point in the repository reaches its
#                  gate through the sealed private validator, statically.
#   D  R10-P0-1    ... and at run time: a caller that pre-binds a fake
#                  `require_validated_method` before sourcing a reference still
#                  reaches the real validator loaded into the sealed object.
#   E  R10-P1-5    a trusted load other than tools/method_manifest.R must be a
#                  manifest dependency of the unit, with its digest.
#   F  fix round 2 the top-level shadow census walks the whole expression tree
#                  and a computed binder name shadows everything (F1-a); the
#                  trusted construction is new.env(parent = baseenv()) with
#                  exactly that one named argument (F1-b); the engine-version
#                  probes are bounded and anchored (F2-a/b/c).
#
# Run with:  Rscript tools/tests/test_batch11.R
# Exits non-zero if any assertion fails.

# SAFE TO SOURCE: pipelines tar_source("tools") which recurses into
# tools/tests/; everything observable is guarded behind .invoked_as_script().
.invoked_as_script <- function() {
  fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  length(fa) > 0 && grepl("test_batch11\\.R$", sub("^--file=", "", fa[1]))
}

.run_batch11_tests <- function() {

args <- commandArgs(FALSE)
file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
TOOLS <- normalizePath(file.path(dirname(file_arg), ".."), mustWork = TRUE)
REPO  <- normalizePath(file.path(TOOLS, ".."), mustWork = TRUE)
source(file.path(TOOLS, "method_manifest.R"))

.pass <- 0L; .fail <- 0L
ok <- function(cond, name) {
  cond <- tryCatch(isTRUE(cond), error = function(e) {
    cat(sprintf("      error: %s\n", conditionMessage(e))); FALSE })
  if (cond) { .pass <<- .pass + 1L; cat(sprintf("PASS  %s\n", name)) }
  else { .fail <<- .fail + 1L; cat(sprintf("FAIL  %s\n", name)) }
}

# --- fixture root -----------------------------------------------------------
root <- tempfile("b11fix"); dir.create(root)
old_wd <- setwd(root); on.exit(setwd(old_wd), add = TRUE)
dir.create("tools")
writeLines(c("keep_rows <- function(d) d", "helper <- 1"), "tools/record_selection.R")
file.copy(file.path(REPO, "tools", "method_manifest.R"),
          "tools/method_manifest.R", overwrite = TRUE)
file.copy(file.path(REPO, "tools", "psn_invocation.R"),
          "tools/psn_invocation.R", overwrite = TRUE)
sha <- function(p) .mm_file_sha256(p)

mkgate <- function(name, body_lines, ep_lines, unit = "alpha") {
  d <- file.path("skills", name)
  dir.create(file.path(d, "reference"), recursive = TRUE, showWarnings = FALSE)
  p <- file.path(d, "reference", "method.R")
  # Integrity batch 14 (R13-P1-3): a gate's skill_dir / calling_file literals
  # must name the skill and file under audit. The shared templates above are
  # written with placeholders; bind them to this fixture's own paths here.
  body_lines <- gsub('skill_dir = "s"', paste0('skill_dir = "', d, '"'), body_lines, fixed = TRUE)
  body_lines <- gsub('calling_file = "f"', paste0('calling_file = "', p, '"'), body_lines, fixed = TRUE)
  body_lines <- gsub('calling_file = "x"', paste0('calling_file = "', p, '"'), body_lines, fixed = TRUE)
  ep_lines <- gsub('skill_dir = "s"', paste0('skill_dir = "', d, '"'), ep_lines, fixed = TRUE)
  ep_lines <- gsub('calling_file = "f"', paste0('calling_file = "', p, '"'), ep_lines, fixed = TRUE)
  ep_lines <- gsub('calling_file = "x"', paste0('calling_file = "', p, '"'), ep_lines, fixed = TRUE)
  writeLines(c(body_lines, ep_lines), p)
  writeLines(c("# fixture", paste0("skill: ", name),
               "generated_at: 2026-09-04T00:00:00Z",
               'reviewed_by: "Fixture Reviewer"', 'reviewed_at: "2026-09-01"', "units:",
               paste0("  ", unit, ":"), "    status: approved",
               paste0("    reference_path: ", p), "    entrypoints: [ep]",
               paste0("    sha256: ", sha(p)), "    evidence: SKILL.md",
               '    reason: "signed"'),
             file.path(d, "MANIFEST.yaml"))
  d
}
gated_of <- function(d) {
  r <- tryCatch(suppressWarnings(audit_entrypoint_gating(d)),
                error = function(e) conditionMessage(e))
  if (is.character(r)) list(gated = FALSE, reason = r)
  else list(gated = isTRUE(r$gated[1]), reason = r$reason[1])
}

# The construction every reference now carries, as fixture lines.
CONSTRUCTION <- c(
  '.mm_trusted_private <- base::local({',
  '  e <- base::new.env(parent = base::baseenv())',
  '  if (!base::file.exists("tools/method_manifest.R"))',
  '    base::stop("missing", call. = FALSE)',
  '  base::sys.source("tools/method_manifest.R", envir = e, keep.source = FALSE)',
  '  base::lockEnvironment(e, bindings = TRUE)',
  '  e',
  '}, envir = base::new.env(parent = base::baseenv()))')
GATE <- c(
  '.alpha_gate <- base::local({',
  '  private <- .mm_trusted_private',
  '  function() {',
  '    if (!base::get("require_validated_method", envir = private)(skill_dir = "s", calling_file = "f",',
  '                                          unit = "alpha")) base::stop("no")',
  '    base::invisible(TRUE)',
  '  }',
  '}, envir = base::new.env())')
EP <- c('ep <- function(x) { .alpha_gate(); x }')

cat("\n--- A: S5, requireNamespace/loadNamespace do not attach ---------------\n")

for (call_line in c('invisible(requireNamespace("digest", quietly = TRUE))',
                    'invisible(loadNamespace("digest"))')) {
  nm <- sub("\\(.*", "", sub("^invisible\\(", "", call_line))
  d <- mkgate(paste0("s5_ok_", nm), c(call_line, CONSTRUCTION, GATE), EP)
  ok(gated_of(d)$gated,
     sprintf("A/S5 %s() in a file that builds a trusted object is allowed", nm))
}
for (call_line in c('invisible(library(stats))', 'invisible(require(stats))',
                    'invisible(attach(list()))',
                    'invisible(attachNamespace("stats"))')) {
  nm <- sub("\\(.*", "", sub("^invisible\\(", "", call_line))
  d <- mkgate(paste0("s5_no_", nm), c(call_line, CONSTRUCTION, GATE), EP)
  ok(!gated_of(d)$gated,
     sprintf("A/S5 %s() in such a file is still refused", nm))
}

cat("\n--- B: R10-P0-2, shadowing inside the trusted block --------------------\n")

# Codex's counterexample: fakes bound INSIDE the block, before the
# construction. Every one of these is a plain statement of the trusted block,
# which no rule looked at before this batch.
b_inblock <- list(
  "B/new.env"         = '  new.env <- function(...) globalenv()',
  "B/sys.source"      = '  sys.source <- function(...) invisible(NULL)',
  "B/lockEnvironment" = '  lockEnvironment <- function(...) invisible(NULL)',
  "B/file.exists"     = '  file.exists <- function(...) TRUE',
  "B/unrelated name"  = '  helper <- 1')
for (nm in names(b_inblock)) {
  d <- mkgate(paste0("b11_", gsub("[^A-Za-z0-9]", "", nm)),
              c('.mm_trusted_private <- base::local({',
                b_inblock[[nm]],
                '  e <- base::new.env(parent = base::baseenv())',
                '  base::sys.source("tools/method_manifest.R", envir = e, keep.source = FALSE)',
                '  base::lockEnvironment(e, bindings = TRUE)',
                '  e',
                '}, envir = base::new.env(parent = base::baseenv()))', GATE), EP)
  ok(!gated_of(d)$gated,
     sprintf("%s bound inside the trusted block is refused", nm))
}

# The same shapes through the qualified / functional assignment forms the
# binding census did not recognise.
b_qualified <- list(
  "B/base::assign"   = '  base::assign("sys.source", function(...) NULL)',
  "B/do.call assign" = '  do.call("assign", list("sys.source", function(...) NULL))',
  "B/do.call symbol" = '  do.call(assign, list("sys.source", function(...) NULL))',
  "B/`<-`() form"    = '  `<-`(sys.source, function(...) NULL)',
  "B/paste0 name"    = '  base::assign(paste0("sys.", "source"), function(...) NULL)')
for (nm in names(b_qualified)) {
  d <- mkgate(paste0("b11q_", gsub("[^A-Za-z0-9]", "", nm)),
              c('.mm_trusted_private <- base::local({',
                b_qualified[[nm]],
                '  e <- base::new.env(parent = base::baseenv())',
                '  base::sys.source("tools/method_manifest.R", envir = e, keep.source = FALSE)',
                '  base::lockEnvironment(e, bindings = TRUE)',
                '  e',
                '}, envir = base::new.env(parent = base::baseenv()))', GATE), EP)
  ok(!gated_of(d)$gated, sprintf("%s inside the trusted block is refused", nm))
}

# ... and at the file's top level, where the same forms previously slipped past
# the primitive-shadow check because the census only knew literal `assign`.
d <- mkgate("b11top_baseassign",
            c('base::assign("sys.source", function(...) invisible(NULL))',
              CONSTRUCTION, GATE), EP)
ok(!gated_of(d)$gated,
   "B/base::assign at the file's top level shadows a construction primitive and is refused")

# --- fix round 1 of batch 11 (independent review R2.1-R2.3, F1) -------------
# The in-block rule was a BLACKLIST of binder call names, and a new binder form
# slipped through it in three consecutive review rounds. It is now a WHITELIST
# of permitted statement forms: a statement of the trusted block must be the
# construction, an enumerated sys.source load, a `for` over a literal vector
# whose body holds only loads and file-existence checks, a file-existence check,
# a pre-seal `e$<name> <-` write, the seal, or the final bare symbol. Every
# other statement form is refused whether or not it binds anything.
#
# The five cases below are the reviewer's runtime-confirmed bypasses
# (scratchpad/rev11/{b112,exploit,more,forindex,top}.R), each of which reported
# `gated = TRUE` with an honest `trusted_loads` while the real validator was
# never loaded.
b_whitelist <- list(
  "B/delayedAssign"      = '  delayedAssign("sys.source", function(...) invisible(NULL))',
  "B/makeActiveBinding"  = '  makeActiveBinding("sys.source", function() function(...) NULL, environment())',
  "B/list2env"           = '  list2env(list(sys.source = function(...) NULL), environment())',
  "B/for-index shadow"   = '  for (sys.source in list(function(...) NULL)) NULL',
  "B/bare call"          = '  print(1)',
  "B/for over non-literal" = '  for (f in getOption("x")) base::sys.source(f, envir = e, keep.source = FALSE)')
for (nm in names(b_whitelist)) {
  d <- mkgate(paste0("b11w_", gsub("[^A-Za-z0-9]", "", nm)),
              c('.mm_trusted_private <- base::local({',
                b_whitelist[[nm]],
                '  e <- base::new.env(parent = base::baseenv())',
                '  base::sys.source("tools/method_manifest.R", envir = e, keep.source = FALSE)',
                '  base::lockEnvironment(e, bindings = TRUE)',
                '  e',
                '}, envir = base::new.env(parent = base::baseenv()))', GATE), EP)
  ok(!gated_of(d)$gated,
     sprintf("%s is not a permitted trusted-block statement form and is refused", nm))
}

# A sys.source into an environment that is NOT the returned symbol is not an
# enumerated load, so it is not a permitted statement either.
d <- mkgate("b11w_othersource",
            c('.mm_trusted_private <- base::local({',
              '  e <- base::new.env(parent = base::baseenv())',
              '  base::sys.source("tools/record_selection.R", envir = globalenv(), keep.source = FALSE)',
              '  base::sys.source("tools/method_manifest.R", envir = e, keep.source = FALSE)',
              '  base::lockEnvironment(e, bindings = TRUE)',
              '  e',
              '}, envir = base::new.env(parent = base::baseenv()))', GATE), EP)
ok(!gated_of(d)$gated,
   "B/sys.source into an environment other than the returned symbol is refused")

# The three new binder forms at the file's TOP level, where the primitive-shadow
# rule at MM_TRUST_CONSTRUCTION_PRIMITIVES reads .mm_top_level_bindings() and so
# depends on the same census.
b_top <- list(
  "B/top delayedAssign"     = 'delayedAssign("sys.source", function(...) invisible(NULL))',
  "B/top makeActiveBinding" = 'makeActiveBinding("sys.source", function() function(...) NULL, environment())',
  "B/top list2env"          = 'list2env(list(sys.source = function(...) NULL), environment())')
for (nm in names(b_top)) {
  d <- mkgate(paste0("b11wt_", gsub("[^A-Za-z0-9]", "", nm)),
              c(b_top[[nm]], CONSTRUCTION, GATE), EP)
  ok(!gated_of(d)$gated,
     sprintf("%s at the file's top level shadows a construction primitive and is refused", nm))
}

# Positive controls: the shipped shapes must still pass.
d <- mkgate("b11_pos_plain", c(CONSTRUCTION, GATE), EP)
ok(gated_of(d)$gated, "B/positive: the batch-11 construction is trusted")

# eda.R's shape: a for loop over a literal vector binds its index, and the
# pre-seal write's value expression binds a formal. Neither can shadow the
# construction, and both must keep passing.
# Since integrity batch 12 (decision 2) the loop and the pre-seal write live in
# the SECOND, dependency environment; the validator loads only its own file.
d <- mkgate("b11_pos_eda", c(
  'DEPS <- base::c("tools/record_selection.R")',
  CONSTRUCTION,
  '.mm_trusted_deps <- base::local({',
  '  d <- base::new.env(parent = base::baseenv())',
  '  for (f in DEPS) {',
  '    if (!base::file.exists(f)) base::stop("missing ", f, call. = FALSE)',
  '    base::sys.source(f, envir = d, keep.source = FALSE)',
  '  }',
  '  d$dependency_sha256 <- base::vapply(DEPS, function(f) base::paste0("sha256:", f), base::character(1))',
  '  base::lockEnvironment(d, bindings = TRUE)',
  '  d',
  '}, envir = base::new.env(parent = base::baseenv()))', GATE), EP)
ok(gated_of(d)$gated,
   "B/positive: eda.R's loop index and pre-seal write formal are not shadowing")

cat("\n--- C: R10-P0-1, every repository entry point gates through the seal ---\n")

setwd(old_wd)
manifests <- sort(Sys.glob("skills/*/MANIFEST.yaml"))
# baseline-fit (2026-11-14): the eleventh shipped manifest is a draft unit.
ok(length(manifests) == 11L,
   sprintf("C: eleven skill manifests are audited (found %d)", length(manifests)))
rows <- do.call(rbind, lapply(manifests, function(p)
  tryCatch(suppressWarnings(audit_entrypoint_gating(dirname(p))),
           error = function(e) { cat(sprintf("      %s\n", conditionMessage(e))); NULL })))
# Includes the NPDE and bootstrap minimum-count policy query entry points.
# B0u-d (2026-09-27, diagnostics fixture): three diagnostics
# references were added (vpc_strata.R, bootstrap_agreement.R, npde_panels.R)
# and the diagnostics and psn-uncertainty manifests gained 13 entry points.
# AC12 (2026-10-26, bound-comparison coverage):
# one simulation-standards reference was added (bound_comparison.R) and the
# simulation-standards manifest gained 1 entry point.
# baseline-fit (2026-11-14): the new draft manifest declares 2 entry points.
ok(!is.null(rows) && nrow(rows) == 90L,
   sprintf("C: the census is 90 entry points (got %s)",
           if (is.null(rows)) "audit error" else nrow(rows)))
if (!is.null(rows)) {
  ungated <- rows[!rows$gated, , drop = FALSE]
  # AC12 (2026-10-26): 87/88 since bound_comparison.R's entry point was added.
  # baseline-fit (2026-11-14): 89/90; both new entry points are gated.
  ok(nrow(ungated) == 1L && identical(ungated$unit[1], "covariate_search"),
     sprintf("C: 89/90 gated; the only exception is the covariate_search placeholder (got %d ungated: %s)",
             nrow(ungated), paste(ungated$unit, collapse = ", ")))
  # Every gated entry point's chain must terminate in the sealed private
  # object, which the audit reports by naming tools/method_manifest.R as a
  # trusted load of the file.
  gated <- rows[rows$gated, , drop = FALSE]
  no_seal <- unique(gated$reference_path[
    !grepl("tools/method_manifest\\.R", gated$trusted_loads)])
  ok(length(no_seal) == 0L,
     sprintf("C: every gated entry point's file builds a sealed validator (%d without)",
             length(no_seal)))
  if (length(no_seal)) for (p in no_seal) cat(sprintf("      %s\n", p))
}
# The weak gate is gone from every reference file.
weak <- Sys.glob("skills/*/reference/*.R")
weak <- weak[vapply(weak, function(p)
  any(grepl('exists\\("require_validated_method"', readLines(p, warn = FALSE))),
  logical(1))]
ok(length(weak) == 0L,
   sprintf("C: no reference file still gates on a caller-visible validator (%d do)",
           length(weak)))
if (length(weak)) for (p in weak) cat(sprintf("      %s\n", p))
setwd(root)

cat("\n--- D: R10-P0-1 at run time -------------------------------------------\n")

# A copy of the repository whose tools/method_manifest.R defines the validator
# as an unconditional stop. Any gate that reaches the REAL validator raises it;
# a gate that resolves the caller's fake returns quietly.
rt <- tempfile("b11rt"); dir.create(rt)
dir.create(file.path(rt, "tools"))
mm <- readLines(file.path(REPO, "tools", "method_manifest.R"), warn = FALSE)
writeLines(c(mm,
             'require_validated_method <- function(...) base::stop("REAL VALIDATOR RAN", call. = FALSE)'),
           file.path(rt, "tools", "method_manifest.R"))
for (f in c("record_selection.R", "engine_config.R", "seed.R", "psn_invocation.R"))
  if (file.exists(file.path(REPO, "tools", f)))
    file.copy(file.path(REPO, "tools", f), file.path(rt, "tools", f))

# file, gate symbol, and the unit argument when the gate takes one.
RT <- list(
  list("skills/covariate-priors/reference/covariate_effect.R", ".covariate_effect_gate", NULL),
  list("skills/covariate-priors/reference/nested_lrt.R",       ".nested_lrt_gate",       NULL),
  list("skills/covariate-priors/reference/psn_covsearch.R",    ".psn_covsearch_gate",    NULL),
  list("skills/covariate-priors/reference/scm_config.R",       ".scm_config_gate",       NULL),
  list("skills/data-assembly/reference/weight_constants.R",    ".weight_constants_gate", NULL),
  list("skills/diagnostics/reference/bootstrap.R",             ".diag_bootstrap_gate",   NULL),
  list("skills/diagnostics/reference/gof.R",                   ".gof_gate",              NULL),
  list("skills/diagnostics/reference/npde.R",                  ".npde_gate",             NULL),
  list("skills/diagnostics/reference/npde_tests.R",            ".diag_npde_tests_gate",  NULL),
  list("skills/diagnostics/reference/qualification_verdict.R", ".diag_verdict_gate",     NULL),
  list("skills/diagnostics/reference/vpc_coverage.R",          ".diag_vpc_coverage_gate", NULL),
  list("skills/diagnostics/reference/vpc.R",                   ".vpc_gate",              NULL),
  list("skills/forest-plots/reference/forest.R",               ".forest_gate",           NULL),
  list("skills/nonmem-estimation/reference/bbr_estimation.R",  ".nonmem_estimation_gate", NULL),
  list("skills/psn-simeval/reference/psn_simeval.R",           ".psn_simeval_gate",      "vpc"),
  list("skills/psn-uncertainty/reference/psn_uncertainty.R",   ".psn_uncertainty_gate",  "bootstrap"),
  list("skills/simulation-standards/reference/exposure_metrics.R", ".sim_exposure_gate", NULL),
  list("skills/simulation-standards/reference/mrgsolve_sim.R", ".sim_gate",              NULL),
  list("skills/eda-standards/reference/eda.R",                 ".eda_gate",              "summary_statistics"))
# uncertainty.R's gate is exercised through the same shape; listed separately
# because its gate takes no argument but its file is large.
RT[[length(RT) + 1L]] <- list("skills/simulation-standards/reference/uncertainty.R",
                              ".uncertainty_gate", NULL)
# AC12 (2026-10-26, plan Section 8, P-2): the new simulation-standards
# gate meets the same run-time attacks as the other three.
RT[[length(RT) + 1L]] <- list("skills/simulation-standards/reference/bound_comparison.R",
                              ".sim_bound_gate", NULL)

for (f in vapply(RT, `[[`, character(1), 1)) {
  dir.create(file.path(rt, dirname(f)), recursive = TRUE, showWarnings = FALSE)
  file.copy(file.path(REPO, f), file.path(rt, f), overwrite = TRUE)
}
# Integrity batch 14: the eda gate authenticates the reference bytes against
# skills/eda-standards/MANIFEST.yaml before the identity check parses them, so
# the fixture tree needs that manifest. The validator itself is still the
# fake/real one this section installs; only the digest binding is read here.
{
  edaf <- file.path(rt, "skills/eda-standards/reference/eda.R")
  writeLines(c("# fixture", "skill: eda-standards",
               "generated_at: 2026-09-05T00:00:00Z", 'reviewed_by: ""', 'reviewed_at: ""',
               "units:", "  summary_statistics:", "    status: draft",
               "    reference_path: skills/eda-standards/reference/eda.R",
               "    entrypoints: [reference_eda_inventory]",
               paste0("    sha256: ", .mm_file_sha256(edaf)),
               "    evidence: skills/eda-standards/SKILL.md", '    reason: "fixture"'),
             file.path(rt, "skills", "eda-standards", "MANIFEST.yaml"))
}
setwd(rt)
for (spec in RT) {
  path <- spec[[1]]; gate <- spec[[2]]; unit <- spec[[3]]
  env <- new.env(parent = globalenv())
  # The attack: a fake validator visible to the reference as it is sourced.
  assign("require_validated_method", function(...) TRUE, envir = env)
  loaded <- tryCatch({ sys.source(path, envir = env, keep.source = FALSE); TRUE },
                     error = function(e) conditionMessage(e))
  if (!isTRUE(loaded)) {
    ok(FALSE, sprintf("D: %s sources (%s)", basename(path), loaded))
    next
  }
  g <- tryCatch(get(gate, envir = env), error = function(e) NULL)
  if (is.null(g)) { ok(FALSE, sprintf("D: %s defines %s", basename(path), gate)); next }
  msg <- tryCatch({
    if (is.null(unit)) g() else g(unit)
    "<no error: the fake validator authorized the call>"
  }, error = function(e) conditionMessage(e))
  ok(grepl("REAL VALIDATOR RAN", msg),
     sprintf("D: %s reaches the real validator despite a pre-bound fake", basename(path)))
  if (!grepl("REAL VALIDATOR RAN", msg)) cat(sprintf("      got: %s\n", msg))
}
setwd(root)

cat("\n--- E: R10-P1-5, a trusted load is a manifest dependency ---------------\n")

# A reference that loads tools/record_selection.R into its sealed object but
# does not declare it under the unit's depends.
d <- file.path("skills", "e_nodep")
dir.create(file.path(d, "reference"), recursive = TRUE, showWarnings = FALSE)
p <- file.path(d, "reference", "method.R")
# (batch 12, decision 2: the dependency loads into its own second environment.)
writeLines(c('DEPS <- base::c("tools/record_selection.R")',
             CONSTRUCTION,
             '.mm_trusted_deps <- base::local({',
             '  d <- base::new.env(parent = base::baseenv())',
             '  for (f in DEPS) {',
             '    if (!base::file.exists(f)) base::stop("missing ", f, call. = FALSE)',
             '    base::sys.source(f, envir = d, keep.source = FALSE)',
             '  }',
             '  base::lockEnvironment(d, bindings = TRUE)',
             '  d',
             '}, envir = base::new.env(parent = base::baseenv()))', GATE, EP), p)
mk_e <- function(dep_lines) {
  writeLines(c("# fixture", "skill: e_nodep", "generated_at: 2026-09-04T00:00:00Z",
               'reviewed_by: "Fixture Reviewer"', 'reviewed_at: "2026-09-01"', "units:",
               "  alpha:", "    status: approved",
               paste0("    reference_path: ", p), "    entrypoints: [ep]",
               paste0("    sha256: ", sha(p)), "    evidence: SKILL.md",
               '    reason: "signed"', dep_lines),
             file.path(d, "MANIFEST.yaml"))
}
mk_e(character(0))
e <- tryCatch({ validate_method_manifest(file.path(d, "MANIFEST.yaml")); NULL },
              error = function(e) conditionMessage(e))
ok(!is.null(e) && grepl("record_selection\\.R", e),
   "E: a trusted load absent from depends is refused, naming the load")
if (!is.null(e)) cat(sprintf("      %s\n", e))

mk_e(c("    depends:", "      - path: tools/record_selection.R",
       paste0("        sha256: ", sha("tools/record_selection.R"))))
e <- tryCatch({ validate_method_manifest(file.path(d, "MANIFEST.yaml")); NULL },
              error = function(e) conditionMessage(e))
ok(is.null(e), "E: the same unit with the load declared under depends validates")
if (!is.null(e)) cat(sprintf("      %s\n", e))

cat("\n--- F: R10-P1-3 / R10-P2-1, SCM reserved keys and the TV check ---------\n")

expect_stop <- function(expr, name, regexp = NULL) {
  e <- tryCatch({ force(expr); NULL }, error = function(e) e)
  hit <- !is.null(e) && (is.null(regexp) || grepl(regexp, conditionMessage(e)))
  if (!is.null(e) && !hit) cat(sprintf("      got: %s\n", conditionMessage(e)))
  if (is.null(e)) cat("      no error was raised\n")
  ok(hit, name)
}
# The generators gate on the covariate-priors `scm_config` unit, which is draft
# in the repository. A fixture skill with an APPROVED manifest lets the gate
# pass so the checks under test are actually reached -- without it every
# assertion below would be satisfied by the gate's own refusal.
sc_dir <- file.path("skills", "covariate-priors", "reference")
dir.create(sc_dir, recursive = TRUE, showWarnings = FALSE)
file.copy(file.path(REPO, "skills/covariate-priors/reference/scm_config.R"),
          file.path(sc_dir, "scm_config.R"), overwrite = TRUE)
# The p-ceiling check reads the house default out of the signed skill file.
file.copy(file.path(REPO, "skills/covariate-priors/SKILL_SCM.md"),
          file.path("skills", "covariate-priors", "SKILL_SCM.md"),
          overwrite = TRUE)
writeLines(c("# fixture", "skill: covariate-priors",
             "generated_at: 2026-09-04T00:00:00Z",
             'reviewed_by: "Fixture Reviewer"', 'reviewed_at: "2026-09-04"', "units:",
             "  scm_config:", "    status: approved",
             "    reference_path: skills/covariate-priors/reference/scm_config.R",
             "    entrypoints: [reference_scm_config, reference_scm_config_backward]",
             paste0("    sha256: ", sha(file.path(sc_dir, "scm_config.R"))),
             "    evidence: SKILL_SCM.md", '    reason: "fixture"'),
           file.path("skills", "covariate-priors", "MANIFEST.yaml"))
sc <- new.env(parent = globalenv())
sys.source(file.path(sc_dir, "scm_config.R"), envir = sc, keep.source = FALSE)
ok(isTRUE(tryCatch({ sc$.scm_config_gate(); TRUE }, error = function(e) FALSE)),
   "F: the fixture manifest approves scm_config, so the gate is not what refuses below")

ctl_dir <- tempfile("b11ctl"); dir.create(ctl_dir)
mk_ctl <- function(nm, pk) {
  p <- file.path(ctl_dir, nm)
  writeLines(c("$PROBLEM x", "$INPUT ID TIME DV WT", "$PK", pk,
               "$ERROR Y = F", "$THETA 1"), p)
  p
}
ctl_ok      <- mk_ctl("ok.ctl",   c("TVCL = EXP(THETA(1))", "CL = TVCL*EXP(ETA(1))"))
ctl_notv    <- mk_ctl("notv.ctl", c("CL = EXP(THETA(1)+ETA(1))"))
# R10-P2-1: TVCL assigned but never consumed by CL. This passed the old
# presence regex, and covariate code inserted around TVCL would not reach CL.
ctl_unused  <- mk_ctl("unused.ctl", c("TVCL = EXP(THETA(1))",
                                      "CL = EXP(THETA(1))*EXP(ETA(1))"))
# A near-miss name: TVCLX is assigned and consumed, TVCL is not.
ctl_prefix  <- mk_ctl("prefix.ctl", c("TVCLX = EXP(THETA(1))",
                                      "CLX = TVCLX*EXP(ETA(1))"))

ok(isTRUE(sc$.scm_check_tv_parameterization(ctl_ok, "CL")),
   "F/TV: a TV-parameterised $PK that consumes its typical value passes")
expect_stop(sc$.scm_check_tv_parameterization(ctl_notv, "CL"),
            "F/TV: no TVCL assignment is refused", "no TVCL")
expect_stop(sc$.scm_check_tv_parameterization(ctl_unused, "CL"),
            "F/TV: TVCL assigned but not used by CL is refused",
            "does not use TVCL")
expect_stop(sc$.scm_check_tv_parameterization(ctl_prefix, "CL"),
            "F/TV: TVCLX does not satisfy the TVCL precondition (literal match)",
            "no TVCL")
for (bad in c("C.L", "CL*", "1CL", "CL)")) {
  expect_stop(sc$.scm_check_tv_parameterization(ctl_ok, bad),
              sprintf("F/TV: the identifier '%s' is refused, not matched approximately", bad),
              "not valid NONMEM identifiers")
}
# The old check built a regex from the name; "C." would have matched "TVCL".
ok(!isTRUE(tryCatch(sc$.scm_check_tv_parameterization(ctl_ok, "C."),
                    error = function(e) FALSE)),
   "F/TV: a regex metacharacter cannot match a different typical-value line")
# Neither refusal advertises the [code] alternative any more (decision 3).
for (spec in list(list(ctl_notv, "CL"), list(ctl_unused, "CL"))) {
  m <- tryCatch({ sc$.scm_check_tv_parameterization(spec[[1]], spec[[2]]); "" },
                error = function(e) conditionMessage(e))
  ok(nzchar(m) && !grepl("\\[code\\]", m),
     sprintf("F/TV: the refusal for %s does not promise a [code] alternative",
             basename(spec[[1]])))
}

# R10-P1-3: every reserved key, through BOTH public generators.
priors <- list(CL = data.frame(covariate = "WT", type = "continuous",
                               form = "power", state = 2L,
                               stringsAsFactors = FALSE))
# Exactly SCM_RESERVED_OPTIONS, read from the reference so the battery cannot
# drift from the list it is testing. (`test_relations` is deliberately not on
# it: the generator emits a `[test_relations]` SECTION, not an option line.)
reserved <- paste0(sc$SCM_RESERVED_OPTIONS, "=x")
ok(setequal(sub("=x$", "", reserved), sc$SCM_RESERVED_OPTIONS) &&
   length(reserved) == 12L,
   sprintf("F/reserved: all %d reserved keys are exercised", length(reserved)))
fwd <- function(extra) sc$reference_scm_config(
  model = "101.ctl", prior_list = priors, directory = "scm1",
  control_stream = ctl_ok, nm_version = "nm751", extra_options = extra)
bwd <- function(extra) sc$reference_scm_config_backward(
  model = "101.ctl", prior_list = priors,
  included_relations = list(CL = "WT-2"), directory = "scm2",
  control_stream = ctl_ok, nm_version = "nm751", extra_options = extra)
for (kv in reserved) {
  key <- sub("=.*", "", kv)
  expect_stop(fwd(kv),
              sprintf("F/reserved: forward refuses extra_options '%s'", key),
              sprintf("reserved key '%s'", key))
  expect_stop(bwd(kv),
              sprintf("F/reserved: backward refuses extra_options '%s'", key),
              sprintf("reserved key '%s'", key))
}
# A non-reserved extra option still passes through both generators.
ok(is.character(tryCatch(fwd("linearize=1"), error = function(e) e)),
   "F/reserved: forward still accepts a non-reserved extra option")
ok(is.character(tryCatch(bwd("linearize=1"), error = function(e) e)),
   "F/reserved: backward still accepts a non-reserved extra option")

cat("\n--- G: R10-P2-3, a signed artifact collection holds regular files only -\n")

gt <- new.env(parent = globalenv())
sys.source(file.path(REPO, "tools", "gate.R"), envir = gt, keep.source = FALSE)

gd <- tempfile("b11gate"); dir.create(gd)
plain <- file.path(gd, "plain.csv"); writeLines("a,b", plain)
ok(grepl("^sha256:[0-9a-f]{64}$", gt$.gate_artifact_cell_sha256(plain)),
   "G/positive: a regular file still digests to its bytes")

# A symlink member (SME decision 5: rejected outright).
link <- file.path(gd, "link.csv"); file.symlink(plain, link)
ok(file.exists(link), "G: the symlink fixture was created")
ok(identical(gt$.gate_artifact_cell_sha256(link), "MISSING"),
   "G: a symlink member makes the cell MISSING")
ok(grepl("symlink", gt$.gate_artifact_cell_reason(link)),
   "G: the refusal reason names the symlink and its target")

# A directory containing a symlink: the whole cell is MISSING, not just that
# member silently dropped or silently followed.
sdir <- file.path(gd, "withlink"); dir.create(sdir)
writeLines("x", file.path(sdir, "real.csv"))
file.symlink(plain, file.path(sdir, "pointer.csv"))
ok(identical(gt$.gate_artifact_cell_sha256(sdir), "MISSING"),
   "G: a directory containing a symlink makes the cell MISSING")
ok(grepl("pointer\\.csv", gt$.gate_artifact_cell_reason(sdir)),
   "G: the refusal names the offending member inside the directory")

# A symlink to a DIRECTORY is a symlink, refused before the directory branch
# can expand it.
cleandir <- file.path(gd, "clean"); dir.create(cleandir)
writeLines("y", file.path(cleandir, "one.csv"))
dlink <- file.path(gd, "dirlink"); file.symlink(cleandir, dlink)
ok(identical(gt$.gate_artifact_cell_sha256(dlink), "MISSING"),
   "G: a symlink to a directory makes the cell MISSING")

# A FIFO member. Public Linux CI requires mkfifo and both FIFO probes.
fifo <- file.path(gd, "pipe.fifo")
made <- suppressWarnings(system2("mkfifo", shQuote(fifo), stdout = NULL,
                                 stderr = NULL, timeout = 10)) == 0L
if (isTRUE(made) && file.exists(fifo)) {
  ok(identical(gt$.gate_artifact_cell_sha256(fifo), "MISSING"),
     "G: a FIFO member makes the cell MISSING (never hashed, never blocked on)")
  ok(grepl("not a regular file", gt$.gate_artifact_cell_reason(fifo)),
     "G: the FIFO refusal names it as a special file")
  fdir <- file.path(gd, "withfifo"); dir.create(fdir)
  writeLines("z", file.path(fdir, "real.csv"))
  inner_fifo <- file.path(fdir, "inner.fifo")
  stopifnot(system2("mkfifo", shQuote(inner_fifo), stdout = NULL, stderr = NULL,
                    timeout = 10) == 0L, file.exists(inner_fifo))
  ok(identical(gt$.gate_artifact_cell_sha256(fdir), "MISSING"),
     "G: a directory containing a FIFO makes the cell MISSING")
} else {
  ok(FALSE, "G: required mkfifo prerequisite failed")
}

# The reasons for the pre-existing MISSING causes are still distinguishable.
ok(grepl("does not exist", gt$.gate_artifact_cell_reason(file.path(gd, "nope.csv"))),
   "G: an absent path is still MISSING, with its own reason")
empty <- file.path(gd, "empty"); dir.create(empty)
ok(grepl("empty directory", gt$.gate_artifact_cell_reason(empty)),
   "G: an empty directory is still MISSING, with its own reason")

cat("\n--- H: decision 4, the VPC censoring refusal covers BOTH directions ----\n")

vp <- new.env(parent = globalenv())
local({
  wprev <- setwd(REPO); on.exit(setwd(wprev), add = TRUE)
  sys.source("skills/diagnostics/reference/vpc.R", envir = vp,
             keep.source = FALSE)
})
chk <- vp$.vpc_check_blq_data
msg <- function(...) tryCatch({ chk(...); "" },
                              error = function(e) conditionMessage(e))

d_clean <- data.frame(DV = c(1, 2, 3), BLQ = 0L, ALQ = 0L)
d_blq   <- data.frame(DV = c(1, 2, 3), BLQ = c(1L, 0L, 0L), ALQ = 0L)
d_alq   <- data.frame(DV = c(1, 2, 3), BLQ = 0L, ALQ = c(0L, 0L, 1L))

ok(!nzchar(msg(d_clean, "BLQ", "keep", yobs = "DV", lloq = 0, alq = "ALQ",
                uloq = 10)),
   "H/positive: a data set with no censored record computes")
ok(grepl("BLQ-flagged", msg(d_blq, "BLQ", "keep", yobs = "DV", lloq = -Inf)),
   "H: a BLQ-flagged record is still refused")
ok(grepl("ALQ-flagged", msg(d_alq, NULL, "keep", yobs = "DV", alq = "ALQ")),
   "H: an ALQ-flagged record is refused (decision 4)")
ok(grepl("below lloq", msg(d_clean, NULL, "keep", yobs = "DV", lloq = 2)),
   "H: an observation below a finite lloq is still refused")
ok(grepl("above uloq", msg(d_clean, NULL, "keep", yobs = "DV", uloq = 2)),
   "H: an observation above a finite uloq is refused (decision 4)")
ok(grepl("not a flag", msg(data.frame(DV = 1, ALQ = "maybe"), NULL, "keep",
                           yobs = "DV", alq = "ALQ")),
   "H: a non-flag value in the ALQ column is refused naming it")
# The uncensored control has no limits or flags.
ok(!nzchar(msg(d_clean, NULL, NULL, yobs = "DV")),
   "H: with no limits and no flag column, nothing is refused")

cat("\n--- I: R10-P3-2 / R10-P2-2, absolute $DATA and the bbi resolver -------\n")

nm <- new.env(parent = globalenv())
local({
  wprev <- setwd(REPO); on.exit(setwd(wprev), add = TRUE)
  sys.source("skills/nonmem-estimation/reference/bbr_estimation.R", envir = nm,
             keep.source = FALSE)
})
ctl_of <- function(data_line) {
  p <- tempfile(fileext = ".ctl")
  writeLines(c("$PROBLEM x", paste("$DATA", data_line), "$INPUT ID TIME DV"), p)
  p
}
for (abs_path in c("/data/observations.csv", "C:/data/observations.csv",
                   "C:\\data\\observations.csv", "\\\\server\\share\\observations.csv")) {
  e <- tryCatch({ nm$.reference_nm_require_joinable_data(ctl_of(abs_path)); NULL },
                error = function(e) conditionMessage(e))
  ok(!is.null(e) && grepl("\\$DATA is absolute", e),
     sprintf("I: the absolute $DATA '%s' is refused", abs_path))
}
e <- tryCatch({ nm$.reference_nm_require_joinable_data(
                  ctl_of("../data/observations.csv IGNORE=C")); NULL },
              error = function(e) conditionMessage(e))
ok(is.null(e), "I/positive: a relative $DATA still passes")

# The harness.yaml key reader takes ONLY bbi_path and never returns anything
# else from the file. Exercised on a fixture, never on the SME's file.
hy <- file.path(tempfile("b11hy")); dir.create(hy)
hf <- file.path(hy, "harness.yaml")
writeLines(c("environment_confirmed: true",
             "nonmem_license: /secret/nonmem.lic",
             'bbi_path: "/opt/bbi/bbi"   # machine-local',
             "data_root: /secret/study"), hf)
got <- nm$.reference_nm_bbi_path_from_harness(hf)
ok(identical(got, "/opt/bbi/bbi"),
   "I: the reader returns the bbi_path value, comment and quotes stripped")
ok(length(got) == 1L && !any(grepl("secret", got)),
   "I: the reader returns nothing else from harness.yaml")
writeLines(c("environment_confirmed: true"), hf)
ok(is.null(nm$.reference_nm_bbi_path_from_harness(hf)),
   "I: a harness.yaml with no bbi_path key yields NULL, not an error")
ok(is.null(nm$.reference_nm_bbi_path_from_harness(file.path(hy, "absent.yaml"))),
   "I: an absent harness.yaml yields NULL, not an error")

cat("\n--- J: fix round 1 hygiene ---------------------------------------------\n")

# .psn_version(): decision 2 requires the PsN version to be RECORDED together
# with the bin rule. A probe that fails used to yield NA_character_, and the run
# record then carried NA silently -- a required field degrading to "unknown"
# without anybody being told. It now refuses.
ps <- new.env(parent = globalenv())
sys.source(file.path(REPO, "skills", "psn-simeval", "reference",
                     "psn_simeval.R"), envir = ps, keep.source = FALSE)
e <- tryCatch(ps$.psn_version(file.path(REPO, "tools", "no-such-executor"),
                              tempdir()),
              error = function(e) e)
ok(inherits(e, "error"),
   "J: .psn_version REFUSES when the probe cannot report a version")
ok(inherits(e, "error") && grepl("PsN version", conditionMessage(e), fixed = TRUE),
   "J: the refusal names the version line it could not find")

# SKILL_SCM.md must document the narrowing the TV check introduced: a
# single-line conditional assignment `IF (COND) CL = TVCL*...` is refused,
# because the text left of the first `=` is not exactly the parameter name.
scm_doc <- readLines(file.path(REPO, "skills", "covariate-priors", "SKILL_SCM.md"),
                     warn = FALSE)
ok(any(grepl("IF (COND) CL = TVCL", scm_doc, fixed = TRUE)),
   "J: SKILL_SCM.md documents that a single-line conditional assignment is refused")
ok(any(grepl("ENDIF", scm_doc, fixed = TRUE)),
   "J: SKILL_SCM.md names the multi-line IF/THEN/ENDIF form that does satisfy the check")

# The dead helper the review found in .scm_check_tv_parameterization().
scm_src <- readLines(file.path(REPO, "skills", "covariate-priors", "reference",
                               "scm_config.R"), warn = FALSE)
ok(!any(grepl("rhs_of", scm_src, fixed = TRUE)),
   "J: the dead rhs_of() helper is gone from scm_config.R")

# tools/gate.R's comment must describe what the code does: only the directory
# branch is vectorised, so an explicitly named member costs one `sh` each.
gate_src <- readLines(file.path(REPO, "tools", "gate.R"), warn = FALSE)
ok(!any(grepl("One `sh` process per cell, not per member", gate_src, fixed = TRUE)),
   "J: the inaccurate `one sh per cell` comment is gone from tools/gate.R")

cat("\n--- F: fix round 2, the top-level shadow census and the construction ---\n")

# F1-a. The top-level binder blacklist used to match each top-level statement
# AT ITS ROOT, and to record a non-literal name as the sentinel "<computed>",
# which then matched no construction primitive. Both were proven at runtime to
# load a fake validator into the sealed object while the audit reported
# gated = TRUE. The census now walks the whole expression tree of every
# top-level statement and treats "<computed>" as shadowing everything.
FAKE <- 'function(file, envir, ...) NULL'
f1a <- list(
  "if (TRUE) delayedAssign(literal)" =
    sprintf('if (TRUE) delayedAssign("sys.source", %s)', FAKE),
  "delayedAssign(computed name)" =
    sprintf('delayedAssign(paste0("sys.", "source"), %s)', FAKE),
  "makeActiveBinding(computed name)" =
    sprintf('makeActiveBinding(as.name(paste0("sys.", "source")), function() %s, environment())', FAKE),
  "local({ delayedAssign(new.env) })" =
    sprintf('local({ delayedAssign("new.env", %s) })', FAKE),
  "for (i in 1) makeActiveBinding(lockEnvironment)" =
    sprintf('for (i in 1) makeActiveBinding("lockEnvironment", function() %s, environment())', FAKE),
  "while (FALSE) delayedAssign(sys.source)" =
    sprintf('while (FALSE) delayedAssign("sys.source", %s)', FAKE),
  "{ delayedAssign(sys.source) }" =
    sprintf('{ delayedAssign("sys.source", %s) }', FAKE),
  "(assign)(sys.source), `(` around the head" =
    sprintf('(assign)("sys.source", %s)', FAKE),
  "f <- assign; f(sys.source), one alias hop" =
    sprintf('f <- assign; f("sys.source", %s)', FAKE),
  "an env alias that is not a fresh new.env" =
    sprintf('.cache <- environment(); assign("sys.source", %s, envir = .cache)', FAKE))
for (nm in names(f1a)) {
  d <- mkgate(paste0("f1a_", gsub("[^A-Za-z0-9]", "", nm)),
              c(f1a[[nm]], CONSTRUCTION, GATE), EP)
  ok(!gated_of(d)$gated,
     sprintf("F/F1-a a top-level binder written as `%s` is refused", nm))
}

# ... and the refusal names the statement, not just the shape.
d <- mkgate("f1a_msg", c(sprintf('if (TRUE) delayedAssign("sys.source", %s)', FAKE),
                         CONSTRUCTION, GATE), EP)
ok(grepl("delayedAssign", gated_of(d)$reason, fixed = TRUE),
   "F/F1-a the refusal quotes the offending top-level statement")

# The census must not over-refuse. A plain top-level binding, a function whose
# locals are frame-local, and a write into a FRESH new.env() (eda.R's
# `assign(key, d, envir = .eda_cache)`) all stay trusted.
f1ok <- list(
  "a harmless top-level binding" = 'harmless <- 1',
  "locals inside a function literal" = 'helper <- function(x) { sys.source <- x; sys.source }',
  "a write into a fresh new.env cache" =
    '.eda_cache <- new.env(parent = emptyenv())\nputc <- function(k, v) assign(k, v, envir = .eda_cache)')
for (nm in names(f1ok)) {
  d <- mkgate(paste0("f1ok_", gsub("[^A-Za-z0-9]", "", nm)),
              c(strsplit(f1ok[[nm]], "\n")[[1]], CONSTRUCTION, GATE), EP)
  ok(gated_of(d)$gated,
     sprintf("F/F1-a %s does not shadow the construction", nm))
}

# F1-b. R's formals are new.env(hash, parent, size), so the old audit read a
# POSITIONAL new.env(baseenv()) as parent = baseenv() while R set hash and left
# parent = parent.frame() -- the sealed object's chain then ran block ->
# reference file -> global, which is exactly the invariant the clause exists to
# hold. The construction must now carry exactly one argument, named `parent`.
ctors <- c("new.env(baseenv())", "new.env(TRUE, baseenv())",
           "new.env(parent = baseenv(), hash = TRUE)",
           "new.env(size = 1L, parent = baseenv())",
           "new.env(parent = globalenv())")
for (ct in ctors) {
  body <- CONSTRUCTION
  body[2] <- paste0("  e <- ", ct)
  d <- mkgate(paste0("f1b_", gsub("[^A-Za-z0-9]", "", ct)), c(body, GATE), EP)
  ok(!gated_of(d)$gated,
     sprintf("F/F1-b the construction written `%s` is refused", ct))
}
d <- mkgate("f1b_positive", c(CONSTRUCTION, GATE), EP)
ok(gated_of(d)$gated,
   "F/F1-b positive control: base::new.env(parent = base::baseenv()) is still trusted")
d <- mkgate("f1b_msg", c(local({ b <- CONSTRUCTION; b[2] <- "  e <- base::new.env(base::baseenv())"; b }),
                        GATE), EP)
ok(grepl("exactly one argument", gated_of(d)$reason, fixed = TRUE),
   "F/F1-b the refusal says the construction takes exactly one named argument")

# Every shipped reference file writes the named form; nothing here was bought
# by editing them.
shipped <- unlist(lapply(list.files(file.path(REPO, "skills"), pattern = "\\.R$",
                                    recursive = TRUE, full.names = TRUE),
                         function(f) grep("base::new.env(parent = base::baseenv())",
                                          readLines(f, warn = FALSE),
                                          fixed = TRUE, value = TRUE)))
ok(length(shipped) >= 19,
   sprintf("F/F1-b all shipped constructions use the named form (%d found)",
           length(shipped)))

# The engine-version probes are bounded and their patterns anchored at both
# ends, so a line that merely STARTS with a version is not recorded as one.
bbr_src <- readLines(file.path(REPO, "skills", "nonmem-estimation", "reference",
                               "bbr_estimation.R"), warn = FALSE)
ok(any(grepl("timeout = MM_VERSION_PROBE_TIMEOUT", bbr_src, fixed = TRUE)),
   "F/F2-a the bbi version probe is bounded by a timeout")
ok(any(grepl('MM_BBI_VERSION_PATTERN <- "^v?[0-9]+(\\\\.[0-9]+)+$"', bbr_src, fixed = TRUE)),
   "F/F2-c the bbi version pattern is anchored at both ends")
psn_src <- readLines(file.path(REPO, "skills", "psn-simeval", "reference",
                               "psn_simeval.R"), warn = FALSE)
ok(any(grepl("timeout = PSN_VERSION_PROBE_TIMEOUT", psn_src, fixed = TRUE)),
   "F/F2-b the PsN version probe is bounded by a timeout")
# Scoped to .psn_version()'s own body: .psn_run() merges the two streams on
# purpose, because it quotes the tail of the output in its failure message.
psn_probe <- local({
  i <- grep("^.psn_version <- function", psn_src)[1]
  j <- grep("^reference_psn_vpc <- function", psn_src)[1]
  psn_src[i:(j - 1L)]
})
ok(any(grepl("stderr = errfile", psn_probe, fixed = TRUE)) &&
     !any(grepl("stdout = TRUE, stderr = TRUE", psn_probe, fixed = TRUE)),
   "F/F2-b the PsN probe keeps stderr out of the recorded value")
ok(any(grepl("exited with exit status", psn_probe, fixed = TRUE)),
   "F/F2-b the PsN probe refuses on a nonzero exit status")
ok(any(grepl("PSN_VERSION_PATTERN", psn_probe, fixed = TRUE)),
   "F/F2-b the PsN probe requires the reported value to be a version string")


cat("\n--- G: fix round 3, bare-symbol heads and the bound-once cache ---\n")

# G2-a. .mm_call_head_name() maps a `$` head to its MEMBER name, which is the
# right reading for `private$validate()` in the gate chain and the wrong one
# for the trusted construction: with it, `new.env(parent = shim$baseenv())`
# passed the clause and produced a sealed object parented on R_GlobalEnv --
# writable by any code in the calling session -- while the audit reported the
# block as gated. Both the construction and its `parent` must now be written
# with bare-symbol heads.
g2a <- c("new.env(parent = shim$baseenv())", "shim$new.env(parent = base::baseenv())",
         "new.env(parent = base::baseenv())", "new.env(parent = (baseenv()))",
         "new.env(parent = shim[[\"baseenv\"]]())")
SHIM <- 'shim <- list(baseenv = function() globalenv(), new.env = function(parent) base::new.env(parent = parent))'
for (ct in g2a) {
  body <- CONSTRUCTION
  body[2] <- paste0("  e <- ", ct)
  d <- mkgate(paste0("g2a_", gsub("[^A-Za-z0-9]", "", ct)), c(SHIM, body, GATE), EP)
  ok(!gated_of(d)$gated,
     sprintf("G/G2-a the construction written `%s` is refused", ct))
}
d <- mkgate("g2a_positive", c(SHIM, CONSTRUCTION, GATE), EP)
ok(gated_of(d)$gated,
   "G/G2-a positive control: base::new.env(parent = base::baseenv()) is still trusted")
d <- mkgate("g2a_msg_parent",
            c(SHIM, local({ b <- CONSTRUCTION
                            b[2] <- "  e <- base::new.env(parent = shim$baseenv())"; b }),
              GATE), EP)
ok(grepl("base::baseenv()", gated_of(d)$reason, fixed = TRUE),
   "G/G2-a the refusal for a dispatched `parent` names the `base::baseenv()` requirement")
d <- mkgate("g2a_msg_head",
            c(SHIM, local({ b <- CONSTRUCTION
                            b[2] <- "  e <- shim$new.env(parent = base::baseenv())"; b }),
              GATE), EP)
ok(grepl("base::new.env", gated_of(d)$reason, fixed = TRUE),
   "G/G2-a the refusal for a dispatched construction names the `base::new.env` requirement")

# The same reading was used to recognise the block's sys.source() load, so a
# `shim$sys.source(...)` was accepted as the enumerated load.
d <- mkgate("g2a_load",
            c(SHIM2 <- 'shim2 <- list(sys.source = function(...) NULL)',
              local({ b <- CONSTRUCTION
                      b[4] <- '  shim2$sys.source("tools/method_manifest.R", envir = e, keep.source = FALSE)'
                      b }), GATE), EP)
ok(!gated_of(d)$gated,
   "G/G2-a a load written `shim2$sys.source(...)` is not the enumerated load")

# G1-a. The fresh-new.env exclusion kept a STATIC symbol set, so rebinding the
# cache symbol to the file's own top level put the top level behind an
# exclusion meant for a private cache. The exclusion now applies only to a
# symbol bound exactly once in the whole file, whose single binding is a
# root-level new.env(parent = emptyenv()) or new.env(parent = baseenv()).
FAKE3 <- 'function(file, envir, ...) NULL'
g1a <- list(
  "the cache symbol is rebound to environment()" = c(
    '.cache <- new.env(parent = emptyenv())', '.cache <- environment()',
    sprintf('if (TRUE) assign("lockEnvironment", %s, envir = .cache)', FAKE3)),
  "the cache symbol is rebound inside a function body" = c(
    '.cache <- new.env(parent = emptyenv())',
    'reset <- function() .cache <- environment()',
    sprintf('if (TRUE) assign("lockEnvironment", %s, envir = .cache)', FAKE3)),
  "the cache symbol is rebound by a `for` index" = c(
    '.cache <- new.env(parent = emptyenv())',
    'for (.cache in list(environment())) NULL',
    sprintf('if (TRUE) assign("lockEnvironment", %s, envir = .cache)', FAKE3)),
  "the cache symbol is rebound by a binder call" = c(
    '.cache <- new.env(parent = emptyenv())', 'assign(".cache", environment())',
    sprintf('if (TRUE) assign("lockEnvironment", %s, envir = .cache)', FAKE3)),
  "the cache has a non-static parent" = c(
    '.cache <- new.env(parent = environment())',
    sprintf('if (TRUE) assign("lockEnvironment", %s, envir = .cache)', FAKE3)),
  "the cache has no parent argument" = c(
    '.cache <- new.env()',
    sprintf('if (TRUE) assign("lockEnvironment", %s, envir = .cache)', FAKE3)),
  "the cache is built through a dispatched new.env" = c(
    'shim <- list(new.env = function(parent) base::new.env(parent = parent))',
    '.cache <- shim$new.env(parent = emptyenv())',
    sprintf('if (TRUE) assign("lockEnvironment", %s, envir = .cache)', FAKE3)),
  "the excluded write carries inherits = TRUE" = c(
    '.cache <- new.env(parent = emptyenv())',
    sprintf('if (TRUE) assign("lockEnvironment", %s, envir = .cache, inherits = TRUE)', FAKE3)),
  "the excluded write carries a computed inherits" = c(
    '.cache <- new.env(parent = emptyenv())',
    sprintf('if (TRUE) assign("lockEnvironment", %s, envir = .cache, inherits = !FALSE)', FAKE3)))
for (nm in names(g1a)) {
  d <- mkgate(paste0("g1a_", gsub("[^A-Za-z0-9]", "", nm)),
              c(g1a[[nm]], CONSTRUCTION, GATE), EP)
  ok(!gated_of(d)$gated,
     sprintf("G/G1-a the fresh-cache exclusion does not apply when %s", nm))
}

# ... and it still applies to a cache that is genuinely private: bound once, at
# root level, with a static parent. eda.R's `.eda_cache` is exactly this shape.
g1ok <- list(
  "parent = emptyenv(), written in a function" = c(
    '.cache <- new.env(parent = emptyenv())',
    'putc <- function(k, v) assign(k, v, envir = .cache)'),
  "parent = baseenv(), written in a function" = c(
    '.cache <- base::new.env(parent = base::baseenv())',
    'putc <- function(k, v) assign(k, v, envir = .cache)'),
  # (fix round 6) the written name is a construction primitive that is not a
  # reflective name: since round 6 the character literal "sys.source" in
  # argument position is itself refused by the reflective rule (pinned in
  # section H/K1), so this fixture binds "lockEnvironment" to keep exercising
  # the private-cache exclusion rather than that refusal.
  "an excluded write with an explicit inherits = FALSE" = c(
    '.cache <- new.env(parent = emptyenv())',
    sprintf('if (TRUE) assign("lockEnvironment", %s, envir = .cache, inherits = FALSE)', FAKE3)))
for (nm in names(g1ok)) {
  d <- mkgate(paste0("g1ok_", gsub("[^A-Za-z0-9]", "", nm)),
              c(g1ok[[nm]], CONSTRUCTION, GATE), EP)
  ok(gated_of(d)$gated,
     sprintf("G/G1-a a private cache with %s is still excluded", nm))
}

# `.eda_cache` was removed from eda.R in integrity batch 15 (queue O-11): it had
# no reader after batch 14 fix round 1 and was kept only because this assertion
# pinned its line, which is a test holding dead code in place rather than a
# check of anything. The synthetic G1-a cases above already exercise every shape
# of the private-cache exclusion; nothing was relaxed by dropping the pin.

# --- H. fix round 4: reflective forms at a reference file's top level -------
# The SME's decision of 2026-09-04: refuse, at a reference file's top level,
# every reflective / evaluating form no shipped file uses, then rewrite the one
# shipped file that used source() so that source() can join the list too. Each
# case below was proven at runtime (fixture scratchpad/f4/i1.R, round-4 red
# pass) to reach a fake validator while the audit still reported gated = TRUE.
h_refuse <- list(
  "an alias built by get()" = c(
    'f <- get("assign")', sprintf('if (TRUE) f("sys.source", %s)', FAKE3)),
  "an alias built by match.fun()" = c(
    'f <- match.fun("assign")', sprintf('if (TRUE) f("sys.source", %s)', FAKE3)),
  "a binder reached through a list element" = c(
    sprintf('if (TRUE) list(a = assign)$a("sys.source", %s)', FAKE3)),
  "a binder reached through an immediately-invoked function literal" = c(
    sprintf('if (TRUE) (function() assign)()("sys.source", %s)', FAKE3)),
  "text evaluated as code" = c(
    'eval(parse(text = "sys.source <- function(file, envir, ...) NULL"))'),
  "a ::-qualified reflective call" = c(
    'base::eval(base::parse(text = "sys.source <- function(file, envir, ...) NULL"))'),
  "do.call() with a what the audit cannot read as a literal" = c(
    'f <- assign', sprintf('if (TRUE) do.call(f, list("sys.source", %s))', FAKE3)),
  "source(local = TRUE)" = c('source("nonexistent-fixture.R", local = TRUE)'),
  "base::sys.source(envir = environment())" = c(
    'base::sys.source("nonexistent-fixture.R", envir = environment())'),
  "an environment replaced with environment<-" = c(
    'g <- function() NULL', 'environment(g) <- environment()'))
for (nm in names(h_refuse)) {
  d <- mkgate(paste0("h11_", gsub("[^A-Za-z0-9]", "", nm)),
              c(h_refuse[[nm]], CONSTRUCTION, GATE), EP)
  ok(!gated_of(d)$gated,
     sprintf("H %s at the reference file's top level is refused", nm))
}

# ... and the forms that must NOT move: a reflective call inside a named
# function body is a definition, judged by the gate-chain rules, and the
# shipped construction itself must still pass.
h_keep <- list(
  "get() inside a named function body" = c(
    'lookup <- function(n) get(n, envir = baseenv())'),
  "do.call() with a literal what inside a body" = c(
    'noop <- function() do.call("paste0", list("a", "b"))'),
  "a local({}) block whose value is a function" = c(
    'helper <- base::local({ k <- 1; function() k }, envir = base::new.env())'))
for (nm in names(h_keep)) {
  d <- mkgate(paste0("h11ok_", gsub("[^A-Za-z0-9]", "", nm)),
              c(h_keep[[nm]], CONSTRUCTION, GATE), EP)
  ok(gated_of(d)$gated, sprintf("H %s is still accepted", nm))
}

# H/I4-P2: a replacement-function rebinding of the cache symbol counts as a
# rebinding, so the bound-once exclusion no longer applies to it. Written
# inside a function body that the file then calls, this was neither counted
# nor refused before fix round 4 (fourth review, case m5).
d <- mkgate("h11_replacement_rebind", c(
  '.cache <- new.env(parent = emptyenv())',
  '.mut <- function() parent.env(.cache) <- environment()',
  '.mut()',
  sprintf('if (TRUE) assign("sys.source", %s, envir = .cache)', FAKE3),
  CONSTRUCTION, GATE), EP)
ok(!gated_of(d)$gated,
   "H a replacement-form rebinding of the cache defeats the bound-once exclusion")
ok(is.na(.mm_assign_base_symbol(quote(f()))) &&
     identical(.mm_assign_base_symbol(quote(parent.env(.cache))), ".cache") &&
     identical(.mm_assign_base_symbol(quote(e[["x"]])), "e"),
   "H .mm_assign_base_symbol peels any replacement target to its base symbol")

# --- H/J1 (fix round 5, fifth review P1). Six top-level spellings were
# accepted by the round-4 rule and each reached a fake validator at runtime
# (rev11e/i1_attack.R a07, a09-a13; re-proven red in scratchpad/f5/j1.R): a
# listed name passed as a VALUE in argument position, do.call() with a LITERAL
# `what` that is a listed name, and a function literal handed as a DIRECT
# argument to a callee (not to `{` / `(` / `local` / an assignment) whose body
# names a listed function. All are load-time acts at the file's top level.
h_refuse5 <- list(
  "do.call() with a literal listed what" = c(
    'do.call("source", list("nonexistent-fixture.R", local = TRUE))'),
  "do.call(what = <literal listed name>, args = ...)" = c(
    'do.call(what = "sys.source", args = list("nonexistent-fixture.R", envir = environment()))'),
  "source passed as a value to Reduce()" = c(
    'Reduce(source, list("nonexistent-fixture.R", FALSE))'),
  "a function-literal argument whose body sources" = c(
    'Reduce(function(acc, f) source(f), "nonexistent-fixture.R", NULL)'),
  "source passed as a value to lapply()" = c(
    'lapply("nonexistent-fixture.R", source)'),
  "a ::-qualified listed name passed as a value" = c(
    'lapply("nonexistent-fixture.R", base::source)'),
  "sys.source passed as a value to sapply()" = c(
    'sapply("nonexistent-fixture.R", sys.source, envir = environment())'),
  "source passed as a value to Map()" = c(
    'Map(source, "nonexistent-fixture.R")'),
  "a function-literal argument whose body defines and calls a sourcing function" = c(
    'Reduce(function(acc, f) { h <- function() source(f); h() }, "nonexistent-fixture.R", NULL)'),
  "a function-literal argument whose body do.call()s a literal listed what" = c(
    'Reduce(function(acc, f) do.call("source", list(f)), "nonexistent-fixture.R", NULL)'))
for (nm in names(h_refuse5)) {
  d <- mkgate(paste0("h11j1_", gsub("[^A-Za-z0-9]", "", nm)),
              c(h_refuse5[[nm]], CONSTRUCTION, GATE), EP)
  ok(!gated_of(d)$gated,
     sprintf("H/J1 %s at the reference file's top level is refused", nm))
}
# ... and the load-time functional forms that carry no listed name stay
# accepted: the rule is a name list over argument position and direct
# function-literal arguments, not a ban on apply-family calls.
h_keep5 <- list(
  "lapply() over a harmless function literal" = c(
    'k <- lapply(1:3, function(v) v + 1)'),
  "sapply() with file.exists passed as a value" = c(
    'k <- sapply(c("tools/method_manifest.R"), file.exists)'),
  "do.call() with a literal what that is not listed" = c(
    'k <- do.call("paste0", list("a", "b"))'),
  "vapply() with is.character passed as a value" = c(
    'k <- vapply(list("a"), is.character, logical(1))'))
for (nm in names(h_keep5)) {
  d <- mkgate(paste0("h11j1ok_", gsub("[^A-Za-z0-9]", "", nm)),
              c(h_keep5[[nm]], CONSTRUCTION, GATE), EP)
  ok(gated_of(d)$gated, sprintf("H/J1 %s at the top level is still accepted", nm))
}

# --- H/K1 (fix round 6, sixth review P1). Three more top-level spellings of
# the same class were accepted by the round-5 rule and each reached a fake
# validator at runtime (rev11f/attack.R: a_string, c_paren_wrapped,
# c_local_head): a CHARACTER literal equal to a listed name in argument
# position (the apply family runs match.fun() on it, so `lapply(files,
# "source")` sources); a function literal wrapped in `(` in argument position
# (the `(` exemption was meant for statement-level definitions, not for an
# argument handed to a callee); and a function literal wrapped in `local()`,
# `identity()`, `invisible()`, `force()` or a single-expression `{` in argument
# position (the wrapper evaluates to the literal, which the callee then runs).
h_refuse6 <- list(
  "a character literal \"source\" as the FUN of lapply()" = c(
    'lapply("nonexistent-fixture.R", "source")'),
  "a character literal \"base::source\" as the FUN of lapply()" = c(
    'lapply("nonexistent-fixture.R", "base::source")'),
  "a character literal \"sys.source\" as the f of Map()" = c(
    'Map("sys.source", "nonexistent-fixture.R", MoreArgs = list(envir = environment()))'),
  "a paren-wrapped function-literal argument whose body sources" = c(
    'Reduce((function(acc, f) source(f)), "nonexistent-fixture.R", NULL)'),
  "a paren-wrapped function-literal argument with a braced body that sources" = c(
    'vapply("nonexistent-fixture.R", (function(f) { source(f); TRUE }), logical(1))'),
  "a local()-wrapped function-literal argument whose body sources" = c(
    'Reduce(local(function(acc, f) source(f)), "nonexistent-fixture.R", NULL)'),
  "an identity()-wrapped function-literal argument whose body sources" = c(
    'Reduce(identity(function(acc, f) source(f)), "nonexistent-fixture.R", NULL)'),
  "a walked callback whose body passes the character literal \"source\" to lapply()" = c(
    'Reduce(function(acc, f) lapply(f, "source"), "nonexistent-fixture.R", NULL)'),
  "the character literal \"sys.source\" as the name written into a private cache" = c(
    '.cache <- new.env(parent = emptyenv())',
    'if (TRUE) assign("sys.source", function(file, envir, ...) NULL, envir = .cache, inherits = FALSE)'),
  # (round-6 addendum, o5) a multi-statement block in argument position is
  # evaluated at load time; every statement is walked names-only.
  "a multi-statement local({ ... }) argument whose last expression is a function literal that sources" = c(
    'Reduce(local({ k <- 1; function(acc, f) source(f) }), "nonexistent-fixture.R", NULL)'),
  "a multi-statement { ... } argument whose last expression is a function literal that sources" = c(
    'Reduce({ x <- 1; function(acc, f) source(f) }, "nonexistent-fixture.R", NULL)'))
for (nm in names(h_refuse6)) {
  d <- mkgate(paste0("h11k1_", gsub("[^A-Za-z0-9]", "", nm)),
              c(h_refuse6[[nm]], CONSTRUCTION, GATE), EP)
  ok(!gated_of(d)$gated,
     sprintf("H/K1 %s at the reference file's top level is refused", nm))
}
# ... and the same shapes carrying no listed name stay accepted: a character
# literal is refused only when it spells a listed name, and a wrapped literal
# only when its body names one.
h_keep6 <- list(
  "lapply() with the character literal \"sum\" as FUN" = c(
    'k <- lapply(list(1:3), "sum")'),
  "lapply() over a paren-wrapped harmless function literal" = c(
    'k <- lapply(1:3, (function(v) v + 1))'),
  "sapply() with the character literal \"file.exists\" as FUN" = c(
    'k <- sapply(c("tools/method_manifest.R"), "file.exists")'),
  "Reduce() over a multi-statement local({ ... }) block whose closure names nothing listed" = c(
    'k <- Reduce(local({ k <- 2; function(a, b) a + b * k }), 1:3)'))
for (nm in names(h_keep6)) {
  d <- mkgate(paste0("h11k1ok_", gsub("[^A-Za-z0-9]", "", nm)),
              c(h_keep6[[nm]], CONSTRUCTION, GATE), EP)
  ok(gated_of(d)$gated, sprintf("H/K1 %s at the top level is still accepted", nm))
}

# H/J2 (fix round 5, fifth review I4 boundary). The functional replacement
# spelling `` `parent.env<-`(.cache, e) `` is a call, not an assignment, so the
# round-4 peel never saw it; it re-parents the cache in place exactly as
# `parent.env(.cache) <- e` does. A call whose head name ends in `<-` now binds
# the base symbol of its first argument, at top level and inside a body.
j2_cnt <- function(txt) {
  t <- .mm_all_binding_counts(parse(text = txt, keep.source = FALSE))
  n <- t[".cache"]; if (is.na(n)) 0L else as.integer(n)
}
ok(j2_cnt(c(".cache <- new.env(parent = emptyenv())",
            "`parent.env<-`(.cache, environment())")) >= 2L &&
     j2_cnt(c(".cache <- new.env(parent = emptyenv())",
              ".mut <- function() base::`parent.env<-`(.cache, environment())",
              ".mut()")) >= 2L &&
     j2_cnt(c(".cache <- new.env(parent = emptyenv())",
              "k <- identical(.cache, .cache)")) == 1L,
   "H/J2 a functional `f<-`(x, ...) call counts as a rebinding of x; an ordinary call does not")
for (where in c("top level", "named function body")) {
  stmt <- if (where == "top level") "`parent.env<-`(.cache, environment())"
          else c(".mut <- function() `parent.env<-`(.cache, environment())", ".mut()")
  d <- mkgate(paste0("h11j2_", gsub("[^A-Za-z0-9]", "", where)), c(
    '.cache <- new.env(parent = emptyenv())', stmt,
    sprintf('if (TRUE) assign("sys.source", %s, envir = .cache)', FAKE3),
    CONSTRUCTION, GATE), EP)
  ok(!gated_of(d)$gated,
     sprintf("H/J2 a functional replacement re-parenting of the cache at the %s defeats the bound-once exclusion", where))
}

# The rule is never relaxed to fit a shipped file: the file is rewritten. No
# shipped reference file has a top-level reflective form.
h_files <- Sys.glob(file.path(REPO, "skills", "*", "reference", "*.R"))
h_hits <- unlist(lapply(h_files, function(f)
  .mm_reflective_top_level_violations(parse(f, keep.source = FALSE),
                                      .mm_binding_index(f))))
# AC12 (2026-10-26, bound-comparison coverage):
# one simulation-standards reference was added (bound_comparison.R) and the
# simulation-standards manifest gained 1 entry point.
# baseline-fit (2026-11-14): one draft reference was added (baseline_fit.R).
ok(length(h_files) == 26L && length(h_hits) == 0L,
   sprintf("H all %d shipped reference files are free of top-level reflective forms",
           length(h_files)))

cat(sprintf("\nintegrity batch 11 battery: %d passed, %d failed\n", .pass, .fail))
if (.fail > 0) quit(status = 1)
invisible(TRUE)
}

if (.invoked_as_script()) .run_batch11_tests()
