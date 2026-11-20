#!/usr/bin/env Rscript
# Integrity batch 7 test battery (Codex round 8 findings F1-F8).
#
# Hermetic: every fixture lives under tempdir() laid out as
# <root>/skills/<skill>/reference/<file>.R + MANIFEST.yaml, with a copy of
# tools/method_manifest.R (and tools/engine_config.R where needed), and the
# working directory is moved there so repo-relative paths resolve exactly as
# they do in the repository. No live manifest, gate record or reference file
# is touched. PsN "executors" are throwaway shell scripts under the fixture.
#
# Source of the copied files: the repository by default; the environment
# variable PMX_BATCH7_SOURCE_ROOT overrides it so the battery can be run
# against a staged patch tree before protected files are unlocked (the
# staged tree is laid out like the repository). Every run states its source.
#
# Run with:  Rscript tools/tests/test_batch7.R
# Exits non-zero if any assertion fails.
#
# SAFE TO SOURCE: guarded behind .invoked_as_script() like the other
# batteries, so tar_source("tools") only defines and runs nothing.
.invoked_as_script <- function() {
  fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  length(fa) > 0 && grepl("test_batch7\\.R$", sub("^--file=", "", fa[1]))
}

.run_batch7_tests <- function() {

args <- commandArgs(FALSE)
file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
TOOLS <- normalizePath(file.path(dirname(file_arg), ".."), mustWork = TRUE)
REPO  <- normalizePath(file.path(TOOLS, ".."), mustWork = TRUE)
SRC   <- Sys.getenv("PMX_BATCH7_SOURCE_ROOT", REPO)
SRC   <- normalizePath(SRC, mustWork = TRUE)
cat(sprintf("batch7: copying reference/tool files from %s\n", SRC))

# A file from the source tree, falling back to the repository when the staged
# tree does not carry it (a staged tree holds only the files it changes).
src_file <- function(rel) {
  p <- file.path(SRC, rel)
  if (file.exists(p)) p else file.path(REPO, rel)
}

.pass <- 0L; .fail <- 0L
ok <- function(cond, name) {
  cond <- tryCatch(isTRUE(cond), error = function(e) {
    cat(sprintf("      error: %s\n", conditionMessage(e))); FALSE })
  if (cond) { .pass <<- .pass + 1L; cat(sprintf("PASS  %s\n", name)) }
  else { .fail <<- .fail + 1L; cat(sprintf("FAIL  %s\n", name)) }
}
expect_stop <- function(expr, name, regexp = NULL) {
  e <- tryCatch({ force(expr); NULL }, error = function(e) e)
  hit <- !is.null(e) && (is.null(regexp) || grepl(regexp, conditionMessage(e)))
  if (!is.null(e) && !hit) cat(sprintf("      got: %s\n", conditionMessage(e)))
  if (is.null(e)) cat("      no error was raised\n")
  ok(hit, name)
}
quiet <- function(expr) suppressMessages(suppressWarnings(expr))
warnings_of <- function(expr) {
  w <- character(0)
  withCallingHandlers(expr, warning = function(c) {
    w <<- c(w, conditionMessage(c)); invokeRestart("muffleWarning") })
  w
}

# --- fixture root ------------------------------------------------------------
root <- tempfile("batch7"); dir.create(root)
old_wd <- setwd(root); on.exit(setwd(old_wd), add = TRUE)
dir.create("tools")
file.copy(src_file("tools/method_manifest.R"), "tools/method_manifest.R")
file.copy(src_file("tools/psn_invocation.R"), "tools/psn_invocation.R")
source("tools/method_manifest.R")
sha <- function(p) .mm_file_sha256(p)

# Copy a skill's reference file(s) from the source tree into the fixture and
# write a schema-v2 manifest for the given units. `units` is a named list of
# list(status, ref (basename), entrypoints). Returns the manifest path.
install_skill <- function(skill, files, units, reviewed_by = "Fixture Reviewer",
                          reviewed_at = "2026-09-03") {
  d <- file.path("skills", skill, "reference")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  for (f in files) {
    s <- src_file(file.path("skills", skill, "reference", f))
    if (!file.exists(s)) return(NA_character_)
    file.copy(s, file.path(d, f), overwrite = TRUE)
  }
  write_fixture_manifest(skill, units, reviewed_by, reviewed_at)
}
write_fixture_manifest <- function(skill, units, reviewed_by = "Fixture Reviewer",
                                   reviewed_at = "2026-09-03") {
  lines <- c("# fixture", paste0("skill: ", skill),
             "generated_at: 2026-09-03T00:00:00Z",
             sprintf('reviewed_by: "%s"', reviewed_by),
             sprintf('reviewed_at: "%s"', reviewed_at), "units:")
  for (u in names(units)) {
    x <- units[[u]]
    ref <- file.path("skills", skill, "reference", x$ref)
    lines <- c(lines, sprintf("  %s:", u),
               sprintf("    status: %s", x$status),
               sprintf("    reference_path: %s", ref),
               sprintf("    entrypoints: [%s]", paste(x$entrypoints, collapse = ", ")),
               sprintf("    sha256: %s", sha(ref)),
               "    evidence: SKILL.md", '    reason: "fixture"')
    if (length(x$depends)) {
      lines <- c(lines, "    depends:",
                 unlist(lapply(x$depends, function(dep)
                   c(sprintf("      - path: %s", dep),
                     sprintf("        sha256: %s", sha(dep))))))
    }
  }
  p <- file.path("skills", skill, "MANIFEST.yaml"); writeLines(lines, p); p
}
set_unit_status <- function(skill, unit, status) {
  p <- file.path("skills", skill, "MANIFEST.yaml")
  l <- readLines(p); i <- which(l == sprintf("  %s:", unit))[1]
  l[i + 1L] <- sprintf("    status: %s", status); writeLines(l, p)
}
# Source a fixture reference file into a fresh environment whose parent is the
# global env (so require_validated_method from tools/ is visible).
load_ref <- function(skill, f) {
  e <- new.env(parent = globalenv())
  sys.source(file.path("skills", skill, "reference", f), envir = e)
  e
}

# ==== A. F1: approved unit pointing at a PLACEHOLDER reference ===============
dir.create("skills/stub/reference", recursive = TRUE)
writeLines(c("# REFERENCE IMPLEMENTATION -- covariate search",
             "# STATUS: PLACEHOLDER, NOT WRITTEN.",
             "reference_stub <- function(...) stop('not authored')"),
           "skills/stub/reference/stub.R")
mf <- write_fixture_manifest("stub", list(u = list(status = "approved", ref = "stub.R",
                                                   entrypoints = "reference_stub")))
expect_stop(validate_method_manifest(mf),
            "A: approved unit whose reference carries a PLACEHOLDER marker is a hard stop",
            regexp = "approved unit points at unwritten method")
set_unit_status("stub", "u", "draft")
ok(!inherits(tryCatch(validate_method_manifest(mf), error = function(e) e), "error"),
   "A: the same placeholder as a draft unit validates (honest authoring state)")
writeLines(c("# real method", "reference_stub <- function(x) paste('PLACEHOLDER', x)"),
           "skills/stub/reference/stub.R")
mf <- write_fixture_manifest("stub", list(u = list(status = "approved", ref = "stub.R",
                                                   entrypoints = "reference_stub")))
ok(!inherits(tryCatch(validate_method_manifest(mf), error = function(e) e), "error"),
   "A: the word PLACEHOLDER inside code (not a comment) is not a stub marker")
# and the gate core refuses the approved placeholder too (it validates first)
writeLines(c("# STATUS: PLACEHOLDER, NOT WRITTEN.",
             "reference_stub <- function(...) stop('not authored')"),
           "skills/stub/reference/stub.R")
mf <- write_fixture_manifest("stub", list(u = list(status = "approved", ref = "stub.R",
                                                   entrypoints = "reference_stub")))
expect_stop(require_validated_method("skills/stub", "skills/stub/reference/stub.R", "u"),
            "A: require_validated_method hard-stops on the approved placeholder",
            regexp = "unwritten method")

# ==== B. F2: entry-point gating audit ========================================
ok(exists("audit_entrypoint_gating", mode = "function"),
   "B: audit_entrypoint_gating() exists in tools/method_manifest.R")
dir.create("skills/gated/reference", recursive = TRUE)
writeLines(c(
  ".mm_trusted_private <- base::local({ e <- base::new.env(parent = base::baseenv())",
  "  base::sys.source('tools/method_manifest.R', envir = e, keep.source = FALSE)",
  "  base::lockEnvironment(e, bindings = TRUE); e }, envir = base::new.env(parent = base::baseenv()))",
  ".g_validated <- function() base::get(\"require_validated_method\", envir = .mm_trusted_private)('skills/gated',",
  "  'skills/gated/reference/m.R', 'u')",
  ".g_gate <- function() { if (!.g_validated()) base::stop('no'); base::invisible(TRUE) }",
  "ep_gated <- function(x) { .g_gate(); x + 1 }",
  "ep_wrapped <- function(x) { if (!.g_validated()) base::stop('no'); x }",
  "ep_open <- function(x) x * 2",
  "ep_via_ep <- function(x) ep_gated(x)"),
  "skills/gated/reference/m.R")
mf <- write_fixture_manifest("gated", list(
  u = list(status = "approved", ref = "m.R",
           entrypoints = c("ep_gated", "ep_wrapped", "ep_open", "ep_via_ep"))))
expect_stop(audit_entrypoint_gating("skills/gated"),
            "B: approved unit with ungated entry points is a hard stop naming them",
            regexp = "ep_open.*ep_via_ep|ep_via_ep.*ep_open")
e <- tryCatch(audit_entrypoint_gating("skills/gated"), error = function(e) conditionMessage(e))
ok(!grepl("ep_gated \\(|ep_wrapped \\(", e),
   "B: the gated entry points (direct gate, wrapped validated()) are not named")
set_unit_status("gated", "u", "draft")
r <- NULL
w <- tryCatch(warnings_of(r <- audit_entrypoint_gating("skills/gated")),
              error = function(e) { cat(sprintf("      error: %s\n", conditionMessage(e)));
                                    character(0) })
ok(length(w) == 1L && grepl("ep_open", w) && grepl("draft", w),
   "B: the same unit as draft is a WARN, not a stop")
ok(is.data.frame(r) && identical(r$entrypoint[r$gated], c("ep_gated", "ep_wrapped")) &&
   identical(r$entrypoint[!r$gated], c("ep_open", "ep_via_ep")),
   "B: audit table marks gated/ungated per entry point; an entry point calling another entry point is not gated")

# First-statement rule (review round 8, MEDIUM 1): a gate name that appears
# anywhere but as the first evaluated statement is NOT a gate. The five probe
# shapes the reviewer used, plus the wrong-unit literal, must audit ungated;
# the two house shapes (`x <- gate(...)`, `if (!validated()) stop()`) gated.
dir.create("skills/probes/reference", recursive = TRUE, showWarnings = FALSE)
writeLines(c(
  ".mm_trusted_private <- base::local({ e <- base::new.env(parent = base::baseenv())",
  "  base::sys.source('tools/method_manifest.R', envir = e, keep.source = FALSE)",
  "  base::lockEnvironment(e, bindings = TRUE); e }, envir = base::new.env(parent = base::baseenv()))",
  ".p_validated <- function(unit) base::get(\"require_validated_method\", envir = .mm_trusted_private)('skills/probes',",
  "  'skills/probes/reference/m.R', unit)",
  ".p_gate <- function(unit) { if (!.p_validated(unit)) base::stop('no'); base::invisible(TRUE) }",
  "ep_dead <- function(x) { if (FALSE) .p_gate('u'); x }",
  "ep_after_return <- function(x) { return(x); .p_gate('u') }",
  "ep_nested <- function(x) { f <- function() .p_gate('u'); x }",
  "ep_default <- function(x, g = .p_gate('u')) x",
  "ep_tail <- function(x) { x; .p_gate('u') }",
  "ep_wrong_unit <- function(x) { .p_gate('other'); x }",
  "ep_assign <- function(x) { p <- .p_gate('u'); x }",
  "ep_if <- function(x) { if (!.p_validated('u')) base::stop('no'); x }",
  "ep_first <- function(x) { .p_gate('u'); x }"),
  "skills/probes/reference/m.R")
probe_eps <- c("ep_dead", "ep_after_return", "ep_nested", "ep_default", "ep_tail",
               "ep_wrong_unit", "ep_assign", "ep_if", "ep_first")
mf <- write_fixture_manifest("probes", list(u = list(status = "draft", ref = "m.R",
                                                     entrypoints = probe_eps)))
r <- NULL
w <- tryCatch(warnings_of(r <- audit_entrypoint_gating("skills/probes")),
              error = function(e) { cat(sprintf("      error: %s\n", conditionMessage(e)));
                                    character(0) })
gated_of <- function(ep) is.data.frame(r) && isTRUE(r$gated[r$entrypoint == ep])
ok(!gated_of("ep_dead"), "B1: a gate call inside `if (FALSE)` does not count")
ok(!gated_of("ep_after_return"), "B1: a gate call after return() does not count")
ok(!gated_of("ep_nested"), "B1: a gate call inside a nested function literal does not count")
ok(!gated_of("ep_default"), "B1: a gate call in a formal's default does not count")
ok(!gated_of("ep_tail"), "B1: a gate call that is not the first statement (lazy tail) does not count")
ok(!gated_of("ep_wrong_unit"),
   "B1: a gate literal naming another unit does not gate this unit's entry point")
ok(gated_of("ep_first") && gated_of("ep_assign") && gated_of("ep_if"),
   "B1: first-statement gate call, `x <- gate()` and `if (!validated()) stop()` are gated")
ok(length(w) == 1L && grepl("ep_dead", w) && grepl("ep_wrong_unit", w) && !grepl("ep_first", w),
   "B1: the draft warning names exactly the ungated probes")
set_unit_status("probes", "u", "approved")
expect_stop(audit_entrypoint_gating("skills/probes"),
            "B1: the same probes under an approved unit are a hard stop",
            regexp = "ep_dead.*ep_after_return.*ep_nested.*ep_default.*ep_tail.*ep_wrong_unit")
# gate built by local({ ... }) with base::get(\"require_validated_method\", envir = env)(...) (eda pattern)
dir.create("skills/gated2/reference", recursive = TRUE, showWarnings = FALSE)
# This mirrors the SHIPPED eda-standards pattern (eda.R:113-125): the private
# environment is filled by sys.source() into a locked new.env(), so the
# reference file never NAMES the validator -- it only dispatches off the
# environment. Round 9 fix round 2: the fixture used to build the private
# object as `list(require_validated_method = require_validated_method)`, which
# is syntactically indistinguishable from the shadowing attack
# `list(require_validated_method = function(...) TRUE)` and is now refused for
# that reason. The assertion is unchanged; only the construction moved to the
# one the harness actually ships.
writeLines(c(
  ".priv <- base::local({ e <- base::new.env(parent = base::baseenv())",
  "  base::sys.source('tools/method_manifest.R', envir = e, keep.source = FALSE)",
  "  base::lockEnvironment(e, bindings = TRUE); e }, envir = base::new.env(parent = base::baseenv()))",
  ".lg <- base::local({ p <- .priv; function(unit) {",
  "  if (!base::get(\"require_validated_method\", envir = p)('skills/gated2', 'skills/gated2/reference/m.R', unit))",
  "    base::stop('no'); base::invisible(TRUE) } }, envir = base::new.env())",
  "ep_a <- function(x) { .lg('u'); x }"),
  "skills/gated2/reference/m.R")
mf <- write_fixture_manifest("gated2", list(u = list(status = "approved", ref = "m.R",
                                                     entrypoints = "ep_a")))
r <- tryCatch(audit_entrypoint_gating("skills/gated2"), error = function(e) e)
ok(is.data.frame(r) && all(r$gated) && grepl(".lg", r$gate_functions[1], fixed = TRUE),
   "B: a gate closure built with local() and base::get(\"require_validated_method\", envir = env)() is recognised")

# ... and the ambiguous form it replaced is refused, because nothing in the
# file distinguishes aliasing the real validator from installing a no-op.
dir.create("skills/gated3/reference", recursive = TRUE, showWarnings = FALSE)
writeLines(c(
  ".priv <- list(require_validated_method = require_validated_method)",
  ".lg <- function(unit) {",
  "  if (!base::get(\"require_validated_method\", envir = .priv)('skills/gated3', 'skills/gated3/reference/m.R', unit))",
  "    stop('no'); invisible(TRUE) }",
  "ep_a <- function(x) { .lg('u'); x }"),
  "skills/gated3/reference/m.R")
mf <- write_fixture_manifest("gated3", list(u = list(status = "draft", ref = "m.R",
                                                     entrypoints = "ep_a")))
r3 <- tryCatch(suppressWarnings(audit_entrypoint_gating("skills/gated3")),
               error = function(e) e)
ok(inherits(r3, "error") || !any(r3$gated),
   "B: naming the validator as a list element is refused (indistinguishable from a shadow)")

# The audit against the real reference files (copied from the source tree):
# diagnostics and forest-plots were already fully gated; mrgsolve_sim (F4) and
# bbr_estimation (F7) must be gated by this batch.
audit_real <- function(skill, files, units) {
  mf <- install_skill(skill, files, units)
  if (is.na(mf)) return(structure(list(message = "reference file missing"), class = "error"))
  tryCatch(audit_entrypoint_gating(file.path("skills", skill)), error = function(e) e)
}
r <- audit_real("forest-plots", "forest.R", list(forest = list(
  status = "approved", ref = "forest.R",
  entrypoints = c("reference_forest_covstats", "reference_forest_covgrid",
                  "reference_forest_samples", "reference_forest_samples_nlmixr",
                  "reference_forest_verify", "reference_forest_parametric",
                  "reference_forest_empirical", "reference_forest_plot"))))
ok(is.data.frame(r) && nrow(r) == 8L && all(r$gated),
   "B: forest.R — all eight entry points call the forest gate")
r <- audit_real("simulation-standards", "mrgsolve_sim.R", list(mrgsolve_sim = list(
  status = "approved", ref = "mrgsolve_sim.R",
  entrypoints = c("reference_sim_compile", "reference_sim_translate_check",
                  "reference_sim_physical_checks", "reference_simulate"))))
if (inherits(r, "error")) cat(sprintf("      got: %s\n", conditionMessage(r)))
ok(is.data.frame(r) && nrow(r) == 4L && all(r$gated),
   "B/F4: mrgsolve_sim.R — compile, translate_check, physical_checks and simulate all gate")
r <- audit_real("nonmem-estimation", "bbr_estimation.R", list(estimation = list(
  status = "approved", ref = "bbr_estimation.R",
  entrypoints = c("reference_nm_workspace", "reference_nm_fit", "reference_nm_results"))))
if (inherits(r, "error")) cat(sprintf("      got: %s\n", conditionMessage(r)))
ok(is.data.frame(r) && nrow(r) == 3L && all(r$gated),
   "B/F7: bbr_estimation.R — workspace, fit and results all gate")
r <- audit_real("psn-simeval", "psn_simeval.R", list(
  vpc = list(status = "approved", ref = "psn_simeval.R", entrypoints = "reference_psn_vpc",
             depends = "tools/psn_invocation.R"),
  npc = list(status = "draft", ref = "psn_simeval.R", entrypoints = "reference_psn_npc",
             depends = "tools/psn_invocation.R"),
  sse = list(status = "draft", ref = "psn_simeval.R", entrypoints = "reference_psn_sse",
             depends = "tools/psn_invocation.R")))
ok(is.data.frame(r) && nrow(r) == 3L && all(r$gated),
   "B: psn_simeval.R — vpc/npc/sse gate through .psn_simeval_gate")

# ==== C. F3: qualification verdict as a manifested diagnostics unit ==========
mf <- install_skill("diagnostics", "qualification_verdict.R", list(
  qualification_verdict = list(status = "approved", ref = "qualification_verdict.R",
                               entrypoints = "reference_qualification_verdict")))
ok(!is.na(mf), "C: skills/diagnostics/reference/qualification_verdict.R exists in the source tree")
if (!is.na(mf)) {
  qv <- load_ref("diagnostics", "qualification_verdict.R")
  criteria <- list(rse_fixed_max_pct = 30, rse_iiv_max_pct = 50, cond_no_max = 1000,
                   shrink_flag_pct = 30, vpc_coverage_min_pct = 80, npde_p_flag = 0.01,
                   boot_converged_min_pct = 80, boot_median_max_pct = 15)
  # Fictional fixed effects have finite RSE. The residual parameter has no eta.
  refit <- list(parFixed = data.frame(parameter = c("tcl", "tv", "prop.sd"),
                                      rse_pct = c(3.1, 12.5, 8.0),
                                      shrink_sd_pct = c(20, 35, NA),
                                      stringsAsFactors = FALSE),
                cond_no_cov = 42.7,
                refit = list(converged = TRUE, message = "relative convergence (4)"))
  npde_tests <- data.frame(test = c("mean", "variance"), p_value = c(0.4, 0.005))
  vpc <- list(coverage_pooled = data.frame(pct_within = c(88.9, 77.8, 100),
                                           n_within_band = c(8, 7, 9), n_bins = c(9, 9, 9)),
              coverage_strat = data.frame(pct_within = c(66.7, 88.9)))
  boot <- list(fixed = data.frame(pct_diff_natural_scale = c(1.2, 16.0),
                                  pct_diff_estimation_scale = c(0.3, 2.1),
                                  ci_finite = c(TRUE, TRUE)),
               omega = data.frame(parameter = c("om.cl", "om.v"), rse_pct = c(58.0, 26.7),
                                  ci_finite = c(TRUE, TRUE), stringsAsFactors = FALSE),
               success_rate_pct = 100)
  v <- tryCatch(qv$reference_qualification_verdict(refit, npde_tests, vpc, boot, criteria),
                error = function(e) e)
  if (inherits(v, "error")) cat(sprintf("      got: %s\n", conditionMessage(v)))
  ok(is.data.frame(v) && nrow(v) == 13L &&
     identical(v$disposition,
               c("PASS", "PASS", "FAIL", "PASS", "SME-VISUAL", "FLAG", "SME-VISUAL",
                 "FAIL", "SME-VISUAL", "FLAG", "PASS", "FAIL", "PASS")),
     "C: the verdict table reproduces the relocated dispositions row for row")
  ok(grepl("max RSE 12.50% \\(tv\\)", v$computed_value[2]) &&
     grepl("strict per-percentile min 77.8%, pooled over all percentile-bins 88.9%", v$computed_value[8]),
     "C: computed-value strings match the relocated arithmetic (max RSE name, strict/pooled coverage)")
  expect_stop(qv$reference_qualification_verdict(refit, npde_tests, vpc, boot,
                                                 criteria[setdiff(names(criteria), "cond_no_max")]),
              "C: a missing threshold is a stop (no default)", regexp = "cond_no_max")
  bad <- criteria; bad$npde_p_flag <- NA_real_
  expect_stop(qv$reference_qualification_verdict(refit, npde_tests, vpc, boot, bad),
              "C: a non-finite threshold is a stop", regexp = "npde_p_flag")
  # SME decision (phase 3): RSE reductions fail closed. One NA fixed-effect
  # RSE makes E2 (fixed effects) FAIL with the reason "RSE unavailable"; an
  # all-NA vector is a stop. Same for the omega RSEs behind E2 (IIV).
  refit_na <- refit; refit_na$parFixed$rse_pct <- c(3.1, NA, NA)
  v_na <- tryCatch(qv$reference_qualification_verdict(refit_na, npde_tests, vpc, boot, criteria),
                   error = function(e) e)
  if (inherits(v_na, "error")) cat(sprintf("      got: %s\n", conditionMessage(v_na)))
  ok(is.data.frame(v_na) && identical(v_na$disposition[2], "FAIL") &&
     grepl("RSE unavailable", v_na$computed_value[2]) && grepl("tv", v_na$computed_value[2]),
     "C2: a missing fixed-effect RSE makes E2 FAIL with 'RSE unavailable' naming the parameter (never PASS via na.rm)")
  refit_all_na <- refit; refit_all_na$parFixed$rse_pct <- NA_real_
  expect_stop(qv$reference_qualification_verdict(refit_all_na, npde_tests, vpc, boot, criteria),
              "C2: an all-NA fixed-effect RSE vector is a stop, not a -Inf PASS",
              regexp = "no .*RSE")
  boot_na <- boot; boot_na$omega$rse_pct[1] <- NA_real_
  v_bna <- tryCatch(qv$reference_qualification_verdict(refit, npde_tests, vpc, boot_na, criteria),
                    error = function(e) e)
  if (inherits(v_bna, "error")) cat(sprintf("      got: %s\n", conditionMessage(v_bna)))
  ok(is.data.frame(v_bna) && identical(v_bna$disposition[3], "FAIL") &&
     grepl("RSE unavailable", v_bna$computed_value[3]),
     "C2: a missing omega RSE makes E2 (IIV) FAIL with 'RSE unavailable'")
  set_unit_status("diagnostics", "qualification_verdict", "draft")
  expect_stop(qv$reference_qualification_verdict(refit, npde_tests, vpc, boot, criteria),
              "C: the unit gates — draft status refuses to compute",
              regexp = "qualification_verdict.*not SME-validated")
  # Exercise a disposable caller through the shipped authenticated loader.
  forwarder <- file.path(root, "qualification_fixture.R")
  writeLines(c(
    'run_verdict <- function(refit, npde_tests, vpc, boot, criteria) {',
    '  method <- load_reference("skills/diagnostics", "skills/diagnostics/reference/qualification_verdict.R")',
    '  method$reference_qualification_verdict(refit, npde_tests, vpc, boot, criteria)',
    '}'), forwarder)
  forward <- new.env(parent = globalenv())
  sys.source(src_file("tools/load_reference.R"), envir = forward)
  sys.source(forwarder, envir = forward)
  expect_stop(forward$run_verdict(refit, npde_tests, vpc, boot, criteria),
              "B7-GATE: thin authenticated forwarder refuses draft computation",
              regexp = "qualification_verdict.*not SME-validated")
  set_unit_status("diagnostics", "qualification_verdict", "approved")
  ok(identical(forward$run_verdict(refit, npde_tests, vpc, boot, criteria), v),
     "B7-GATE: approved forwarder reproduces the independently exercised verdict")
  cat("\n# fixture mutation\n", file = "skills/diagnostics/reference/qualification_verdict.R", append = TRUE)
  expect_stop(forward$run_verdict(refit, npde_tests, vpc, boot, criteria),
              "B7-GATE: thin authenticated forwarder rejects reference digest drift",
              regexp = "do not match the digest")
  file.copy(src_file("skills/diagnostics/reference/qualification_verdict.R"),
            "skills/diagnostics/reference/qualification_verdict.R", overwrite = TRUE)

}

# ==== D. F4: gated simulation entry points; exposure metrics in a reference ==
mf <- install_skill("simulation-standards", "mrgsolve_sim.R", list(mrgsolve_sim = list(
  status = "draft", ref = "mrgsolve_sim.R",
  entrypoints = c("reference_sim_compile", "reference_sim_translate_check",
                  "reference_sim_physical_checks", "reference_simulate"))))
ms <- load_ref("simulation-standards", "mrgsolve_sim.R")
expect_stop(ms$reference_sim_physical_checks(data.frame(CP = c(1, 2, 3))),
            "D: reference_sim_physical_checks refuses under a draft unit (gated)",
            regexp = "not SME-validated")
expect_stop(ms$reference_sim_compile("$PARAM CL = 1", name = "x"),
            "D: reference_sim_compile refuses under a draft unit (gated, before compiling)",
            regexp = "not SME-validated")
expect_stop(ms$reference_sim_translate_check(NULL, data.frame(), data.frame()),
            "D: reference_sim_translate_check refuses under a draft unit (gated, before column checks)",
            regexp = "not SME-validated")

mf <- install_skill("simulation-standards", c("mrgsolve_sim.R", "exposure_metrics.R"), list(
  mrgsolve_sim = list(status = "draft", ref = "mrgsolve_sim.R",
                      entrypoints = c("reference_sim_compile", "reference_sim_translate_check",
                                      "reference_sim_physical_checks", "reference_simulate")),
  exposure_metrics = list(status = "approved", ref = "exposure_metrics.R",
                          entrypoints = c("reference_exposure_by_subject",
                                          "reference_exposure_summary",
                                          "reference_profile_summary"))))
ok(!is.na(mf), "D: skills/simulation-standards/reference/exposure_metrics.R exists in the source tree")
if (!is.na(mf)) {
  em <- load_ref("simulation-standards", "exposure_metrics.R")
  sim <- rbind(
    data.frame(SCENARIO = "s1", ID = 1, time = c(0, 1, 2), CP = c(0, 4, 2), CL = 2, V = 10),
    data.frame(SCENARIO = "s1", ID = 2, time = c(0, 1, 2), CP = c(0, 2, 1), CL = 4, V = 10))
  scen <- list(s1 = list(regimen = list(amt = 100, cmt = 1), n = 2))
  bs <- tryCatch(em$reference_exposure_by_subject(sim, scen, observed = "CP", time = "time",
                                                  clearance = "CL", volume = "V",
                                                  auc_tolerance = 1e-6),
                 error = function(e) e)
  if (inherits(bs, "error")) cat(sprintf("      got: %s\n", conditionMessage(bs)))
  # hand-computed: trapezoid on (0,4,2) over unit steps = 2 + 3 = 5; (0,2,1) = 1 + 1.5 = 2.5
  ok(is.data.frame(bs) && nrow(bs) == 2L &&
     isTRUE(all.equal(bs$auc_0_tlast, c(5, 2.5))) &&
     isTRUE(all.equal(bs$auc_0_inf, c(50, 25))) &&
     isTRUE(all.equal(bs$cmax, c(4, 2))) && all(bs$tlast == 2) &&
     identical(setdiff(names(bs), c("SCENARIO", "ID", "tlast", "cmax", "auc_0_tlast", "auc_0_inf")),
               character(0)),
     "D: by-subject metrics reproduce the relocated definitions (trapezoid AUC, amt/CL, grid Cmax)")
  su <- em$reference_exposure_summary(bs, metrics = c("cmax", "auc_0_tlast"),
                                      units = list(conc = "mg/L", time = "h"),
                                      probs = c(0.05, 0.95), quantile_type = 7L)
  ok(nrow(su) == 2L && su$unit[2] == "mg/L * h" && su$n[1] == 2 &&
     isTRUE(all.equal(su$median, c(3, 3.75))) &&
     isTRUE(all.equal(su$lower, c(unname(quantile(c(4, 2), 0.05, type = 7)),
                                  unname(quantile(c(5, 2.5), 0.05, type = 7))))) &&
     all(su$p_lower == 0.05) && all(su$p_upper == 0.95),
     "D: summary carries declared units, declared probabilities and the declared quantile type")
  expect_stop(em$reference_exposure_summary(bs, metrics = "cmax",
                                            units = list(conc = "mg/L", time = "h"),
                                            probs = c(0.05, 0.95)),
              "D: quantile_type has no default", regexp = "quantile_type")
  expect_stop(em$reference_exposure_summary(bs, metrics = "cmax",
                                            units = list(conc = "mg/L", time = "h"),
                                            quantile_type = 7L),
              "D: probs has no default", regexp = "probs")
  expect_stop(em$reference_exposure_summary(bs, metrics = c("cmax", "auc_0_tau"),
                                            units = list(conc = "mg/L", time = "h"),
                                            probs = c(0.05, 0.95), quantile_type = 7L),
              "D: an undefined metric is a stop, never an improvised definition",
              regexp = "auc_0_tau")
  expect_stop(em$reference_exposure_by_subject(sim, scen, observed = "CP", time = "time",
                                               clearance = "CL", volume = "V"),
              "D: auc_tolerance has no default", regexp = "auc_tolerance")
  bad_sim <- sim; bad_sim$CL[bad_sim$ID == 1] <- 40   # AUC(0-inf) = 2.5 < AUC(0-tlast) = 5
  expect_stop(em$reference_exposure_by_subject(bad_sim, scen, observed = "CP", time = "time",
                                               clearance = "CL", volume = "V",
                                               auc_tolerance = 1e-6),
              "D: the AUC identity is enforced (AUC(0-tlast) > AUC(0-inf) stops)",
              regexp = "AUC\\(0-tlast\\) exceeds")
  pr <- em$reference_profile_summary(sim, observed = "CP", time = "time",
                                     probs = c(0.05, 0.95), quantile_type = 7L)
  ok(nrow(pr) == 3L && isTRUE(all.equal(pr$median, c(0, 3, 1.5))) && all(pr$n == 2) &&
     identical(names(pr), c("time", "lower", "median", "upper", "n")),
     "D: per-time profile summary from the reference (median, declared interval, n)")
  set_unit_status("simulation-standards", "exposure_metrics", "draft")
  expect_stop(em$reference_exposure_by_subject(sim, scen, observed = "CP", time = "time",
                                               clearance = "CL", volume = "V",
                                               auc_tolerance = 1e-6),
              "D: the exposure_metrics unit gates — draft status refuses to compute",
              regexp = "exposure_metrics.*not SME-validated")
  # the task module no longer carries the arithmetic
  ttxt <- paste(readLines(src_file("pipeline/tasks/simulation.R"), warn = FALSE), collapse = "\n")
  ok(!grepl("diff\\(tt\\)", ttxt) && !grepl("amt / d\\$CL", ttxt) &&
     !grepl("1 \\+ 1e-6", ttxt) && grepl("reference_exposure_by_subject", ttxt) &&
     grepl("reference_profile_summary", ttxt) && grepl("quantile_type", ttxt),
     "D: pipeline/tasks/simulation.R calls the reference with declared parameters and computes no metric")
  # Review round 8, MEDIUM 2: the summary parameters come from the analysis
  # declaration, not from the task module.
  ok(!grepl("summary_probs\\s*<-\\s*c\\(", ttxt) && !grepl("quantile_type\\s*<-\\s*7L", ttxt) &&
     !grepl("auc_tolerance\\s*<-\\s*1e-6", ttxt) &&
     grepl("ti\\$summary_probs", ttxt) && grepl("ti\\$quantile_type", ttxt) &&
     grepl("ti\\$auc_tolerance", ttxt) && grepl("EXPOSURE_METRICS_IMPLEMENTED", ttxt),
     "D2: the task reads summary_probs / quantile_type / auc_tolerance from task_input and the metric set from the reference")
  declaration <- file.path(root, "simulation_spec.R")
  writeLines("list(simulation = list(summary_probs = c(0.1, 0.9), quantile_type = 8L, auc_tolerance = 0.01))",
             declaration)
  submitted <- dget(declaration)$simulation
  set_unit_status("simulation-standards", "exposure_metrics", "approved")
  nondefault <- em$reference_exposure_summary(bs, "cmax", list(conc = "mg/L", time = "h"),
                                               submitted$summary_probs, submitted$quantile_type)
  ok(identical(nondefault$lower, 2) && identical(nondefault$upper, 4) &&
       identical(nondefault$p_lower, 0.1) && identical(nondefault$p_upper, 0.9),
     "D2: declared probabilities and quantile method change the reported interval")
  near_bound <- sim
  near_bound$CL[near_bound$ID == 1] <- 20.1
  compute_exposure <- function(tolerance) em$reference_exposure_by_subject(
    near_bound, scen, observed = "CP", time = "time", clearance = "CL", volume = "V",
    auc_tolerance = tolerance)
  ok(is.data.frame(compute_exposure(submitted$auc_tolerance)),
     "D2: the declaration supplies sufficient tolerance for a near-bound fixture")
  expect_stop(compute_exposure(0), "D2: zero tolerance refuses the same fixture",
              regexp = "AUC\\(0-tlast\\) exceeds")
  set_unit_status("simulation-standards", "exposure_metrics", "draft")

}

# ==== E. F5: PsN failure is a stop; artifacts bound to the invocation ========
mf <- install_skill("psn-simeval", "psn_simeval.R", list(
  vpc = list(status = "approved", ref = "psn_simeval.R", entrypoints = "reference_psn_vpc",
             depends = "tools/psn_invocation.R"),
  npc = list(status = "draft", ref = "psn_simeval.R", entrypoints = "reference_psn_npc",
             depends = "tools/psn_invocation.R"),
  sse = list(status = "draft", ref = "psn_simeval.R", entrypoints = "reference_psn_sse",
             depends = "tools/psn_invocation.R")))
ps <- load_ref("psn-simeval", "psn_simeval.R")
wd <- file.path(root, "psnwork"); dir.create(wd)
writeLines("$PROBLEM x", file.path(wd, "run1.mod"))
# a stale artifact from an earlier run, elsewhere under the workdir
dir.create(file.path(wd, "vpc_dir1")); writeLines("STALE", file.path(wd, "vpc_dir1", "vpc_results.csv"))
Sys.sleep(1.1)
mk_exec <- function(name, body) {
  p <- file.path(root, name)
  writeLines(c("#!/bin/bash", 'workdir="$1"; shift; tool="$1"; shift', body), p)
  Sys.chmod(p, "0755"); p
}
exec_fail <- mk_exec("exec_fail.sh", c('echo "PsN starting"', 'echo "NMTRAN error: data file missing"',
                                        'echo "aborting"', "exit 3"))
exec_silent <- mk_exec("exec_silent.sh", c('echo "done"', "exit 0"))
exec_ok <- mk_exec("exec_ok.sh", c(
  # Fix round 1 of integrity batch 11: .psn_version() now REFUSES when the probe
  # reports no version, because SME decision 2 requires the version to be
  # recorded in the run record alongside the bin rule. A stub executor that
  # stands in for PsN therefore has to answer `psn -version` like PsN does.
  'if [ "$tool" = "psn" ] && [ "$1" = "-version" ]; then echo "PsN version: 5.7.1"; exit 0; fi',
  'for a in "$@"; do case "$a" in -directory=*) d="${a#-directory=}";; esac; done',
  '[ -n "$d" ] || { echo "no -directory given"; exit 9; }',
  'mkdir -p "$workdir/$d"', 'echo "VPC results" > "$workdir/$d/vpc_results.csv"',
  'echo "ok"', "exit 0"))
ec <- list(psn = list())
expect_stop(ps$reference_psn_vpc(wd, "run1.mod", samples = 20, seed = 1, engine_config = ec,
                                 executor = exec_fail,
                                 bin_rule = list(auto_bin = "auto")),
            "E: a nonzero PsN exit is a stop quoting the last output lines",
            regexp = "status 3.*NMTRAN error: data file missing")
expect_stop(ps$reference_psn_vpc(wd, "run1.mod", samples = 20, seed = 1, engine_config = ec,
                                 executor = exec_silent,
                                 bin_rule = list(auto_bin = "auto")),
            "E: a zero exit that leaves no artifact inside the run directory is a stop (the stale vpc_results.csv elsewhere is not read)",
            regexp = "no vpc_results.csv inside its run directory|run directory .* was not created")
r <- tryCatch(ps$reference_psn_vpc(wd, "run1.mod", samples = 20, seed = 1, engine_config = ec,
                                   executor = exec_ok,
                                   bin_rule = list(auto_bin = "auto")), error = function(e) e)
if (inherits(r, "error")) cat(sprintf("      got: %s\n", conditionMessage(r)))
ok(is.list(r) && !inherits(r, "error") && !is.na(r$results_csv) &&
   startsWith(normalizePath(r$results_csv), normalizePath(r$run_dir)) &&
   !grepl("vpc_dir1", r$results_csv) && file.exists(r$start_marker) &&
   grepl("-directory=", r$cmd),
   "E: a successful run reads only the artifact inside its own run directory (start marker recorded, -directory passed)")
r2 <- tryCatch(ps$reference_psn_vpc(wd, "run1.mod", samples = 20, seed = 1, engine_config = ec,
                                    executor = exec_ok,
                                   bin_rule = list(auto_bin = "auto")), error = function(e) e)
ok(!inherits(r2, "error") && !identical(r2$run_dir, r$run_dir),
   "E: every invocation gets its own run directory")
expect_stop(ps$reference_psn_vpc(wd, "run1.mod", samples = 20, seed = 1, engine_config = ec,
                                 executor = exec_ok,
                                 bin_rule = list(bin_by_count = TRUE, no_of_bins = 9)),
            "E: more than one binning rule is a stop (documented exactly-one contract enforced)",
            regexp = "exactly one")
expect_stop(ps$reference_psn_vpc(wd, "run1.mod", samples = 20, seed = 1, engine_config = ec,
                                 executor = exec_ok, bin_rule = list(bins = 9)),
            "E: an unknown bin_rule field is a stop", regexp = "unknown field")

# ==== F. F6: PsN bootstrap floors and public validation status ==============
sk <- readLines(src_file("skills/psn-uncertainty/SKILL.md"), warn = FALSE)
ok(any(grepl("requires local approval", sk, fixed = TRUE)) &&
     !any(grepl("Reviewed by:|samples successful", sk)),
   "F: public guidance requires local approval and carries no inherited validation")
mf <- install_skill("psn-uncertainty", "psn_uncertainty.R", list(
  bootstrap = list(status = "approved", ref = "psn_uncertainty.R",
    entrypoints = c("reference_psn_bootstrap_precheck", "reference_psn_bootstrap",
                    "reference_psn_bootstrap_samples"),
    depends = "tools/psn_invocation.R")))
if (is.na(mf)) stop("PsN uncertainty fixture reference missing")
unc <- load_ref("psn-uncertainty", "psn_uncertainty.R")
for (risk in c(medium = 500L, high = 1000L)) {
  model_risk <- if (risk == 500L) "medium" else "high"
  expect_stop(unc$reference_psn_bootstrap(wd, "unused.ctl", risk - 1L, seed = 1L,
                                         engine_config = list(), model_risk = model_risk),
              "F: below-minimum request refuses before invoking an engine",
              regexp = paste("below the house minimum of", risk))
  expect_stop(unc$reference_psn_bootstrap(wd, "unused.ctl", risk, seed = 1L,
                                         engine_config = list(), model_risk = model_risk),
              "F: at-minimum request reaches control-stream validation",
              regexp = "control stream|not found|does not exist")
}

# ==== G. F7: NONMEM workspace gated; one config per run; fail-closed outputs =
mf <- install_skill("nonmem-estimation", "bbr_estimation.R", list(estimation = list(
  status = "draft", ref = "bbr_estimation.R",
  entrypoints = c("reference_nm_workspace", "reference_nm_fit", "reference_nm_results"))))
bb <- load_ref("nonmem-estimation", "bbr_estimation.R")
mdir <- file.path(root, "models"); dir.create(mdir)
expect_stop(bb$reference_nm_workspace(mdir, list(engines = list(nonmem = list()), bbi = list())),
            "G: reference_nm_workspace refuses under a draft unit (gated)",
            regexp = "not yet SME-validated")
set_unit_status("nonmem-estimation", "estimation", "approved")
expect_stop(bb$reference_nm_fit(mdir, "101", wait = FALSE),
            "G: wait = FALSE is a stop in gated operation (before any submission)",
            regexp = "wait = TRUE is required")
ok(exists(".reference_nm_require_fresh", envir = bb) &&
   exists(".reference_nm_termination_text", envir = bb) &&
   exists(".reference_nm_require_counts", envir = bb) &&
   exists(".reference_nm_resolve_config", envir = bb),
   "G: fail-closed helpers exist (fresh-output, termination text, counts, config resolution)")
if (exists(".reference_nm_require_fresh", envir = bb)) {
  stale <- file.path(root, "stale.json"); writeLines("{}", stale)
  Sys.sleep(1.1); t0 <- Sys.time()
  expect_stop(bb$.reference_nm_require_fresh(stale, t0, "bbi_config.json"),
              "G: an output older than the run start is a stop (stale artifact)",
              regexp = "predates the run start")
  expect_stop(bb$.reference_nm_require_fresh(file.path(root, "none.json"), t0, "bbi_config.json"),
              "G: a missing required output is a stop", regexp = "was not written")
  Sys.sleep(1.1); fresh <- file.path(root, "fresh.json"); writeLines("{}", fresh)
  ok(identical(bb$.reference_nm_require_fresh(fresh, t0, "x"), fresh),
     "G: an output newer than the run start passes")
  expect_stop(bb$.reference_nm_termination_text(character(0)),
              "G: a missing termination tail is a stop, never NA",
              regexp = "no termination text")
  ok(identical(bb$.reference_nm_termination_text(c("MINIMIZATION SUCCESSFUL", "")),
               "MINIMIZATION SUCCESSFUL"),
     "G: termination text is the verbatim non-empty tail")
  expect_stop(bb$.reference_nm_require_counts(123.4, NA_integer_, 100),
              "G: a missing subject count is a stop", regexp = "n_id")
  expect_stop(bb$.reference_nm_require_counts(NULL, 10, 100),
              "G: a missing OFV is a stop", regexp = "ofv")
  ok(isTRUE(bb$.reference_nm_require_counts(123.4, 10L, 100L)),
     "G: finite OFV and counts pass")
}
if (exists(".reference_nm_resolve_config", envir = bb) &&
    requireNamespace("yaml", quietly = TRUE)) {
  file.copy(src_file("tools/engine_config.R"), "tools/engine_config.R")
  file.copy(src_file("tools/yaml_scalar.R"), "tools/yaml_scalar.R")
  dir.create("config", showWarnings = FALSE)
  yaml::write_yaml(list(bbi = list(mode = "local", args = list(threads = 1L))), "config/engine.yaml")
  rc <- file.path(mdir, "run-config.yaml")
  yaml::write_yaml(list(bbi = list(mode = "local", args = list(threads = 4L))), rc)
  res <- tryCatch(bb$.reference_nm_resolve_config(mdir, repo_root = root), error = function(e) e)
  if (inherits(res, "error")) cat(sprintf("      got: %s\n", conditionMessage(res)))
  ok(is.list(res) && !inherits(res, "error") &&
     identical(normalizePath(res$path), normalizePath(rc)) &&
     grepl("^[0-9a-f]{64}$", res$sha256) && identical(res$cfg$bbi$args$threads, 4L),
     "G: the run config resolves ONCE to the per-run run-config.yaml with its sha256")
  # native workspace derived from the resolved object: bbi.yaml carries its threads
  nmh <- file.path(root, "nm75"); dir.create(file.path(nmh, "run"), recursive = TRUE)
  writeLines("#!/bin/bash", file.path(nmh, "run", "nmfe75"))
  cfg <- list(engines = list(nonmem = list(provision = "native", nm_version_label = "nm75")),
              bbi = list(mode = "local", args = list(threads = 4L)))
  # Integrity batch 11 (R10-P2-2): the workspace writes bbi.yaml for a bbi that
  # must actually be usable, so it resolves the executable through the shared
  # resolver. A stub that is executable and reports a version satisfies it
  # without a bbi installation.
  fake_bbi <- file.path(root, "bbi-stub")
  writeLines(c("#!/bin/sh", 'echo "v3.4.0"'), fake_bbi)
  Sys.chmod(fake_bbi, "0755")
  out <- tryCatch(bb$reference_nm_workspace(mdir, cfg, repo_root = root,
                                            nonmem_home = nmh,
                                            bbi_path = fake_bbi),
                  error = function(e) e)
  if (inherits(out, "error")) cat(sprintf("      got: %s\n", conditionMessage(out)))
  by <- if (!inherits(out, "error")) yaml::read_yaml(out) else NULL
  ok(!is.null(by) && identical(as.integer(by$threads), 4L) && identical(by$nm_version, "nm75"),
     "G: bbi.yaml is derived from the passed config object (threads, nm_version)")
}
btxt <- paste(readLines(src_file("skills/nonmem-estimation/reference/bbr_estimation.R"),
                        warn = FALSE), collapse = "\n")
ok(!grepl("stays ungated", btxt) && !grepl("error = function\\(e\\) NA_character_\\)  # unverified", btxt),
   "G: the stale 'stays ungated' comment and the NA_character_ swallow are gone")

# AC14: the public workspace seam, with invented operational fixtures only.
(function() {
  controls <- c("PATH", "NONMEM_EXTRA_MOUNTS", "PMX_E_GIT_LOG",
                "PMX_E_GIT_OUTPUT", "PMX_E_GIT_STATUS")
  save_env <- function() Sys.getenv(controls, unset = NA_character_)
  restore_env <- function(saved) {
    for (key in names(saved)) {
      if (is.na(saved[[key]])) Sys.unsetenv(key)
      else do.call(Sys.setenv, stats::setNames(list(saved[[key]]), key))
    }
  }
  saved <- save_env()
  on.exit({ restore_env(saved)
            set_unit_status("nonmem-estimation", "estimation", "approved") }, add = TRUE)
  if (!requireNamespace("yaml", quietly = TRUE)) {
    ok(FALSE, "G-E-ack: yaml is required for the workspace cases")
  } else {
    fixture <- file.path(root, "mount-ack-fixture")
    dir.create(fixture)
    bin <- file.path(fixture, "bin"); dir.create(bin)
    git_log <- file.path(fixture, "git.log")
    fake_git <- file.path(bin, "git")
    # This fake never invokes or delegates to real Git.
    writeLines(c("#!/bin/sh", 'printf "%s\\n" "$@" >> "$PMX_E_GIT_LOG"',
                 'printf "%s\\n" "$PMX_E_GIT_OUTPUT"',
                 'exit "$PMX_E_GIT_STATUS"'), fake_git)
    Sys.chmod(fake_git, "0755")
    bbi_stub <- file.path(bin, "bbi")
    writeLines(c("#!/bin/sh", 'printf "v3.4.0\\n"'), bbi_stub)
    Sys.chmod(bbi_stub, "0755")
    shim <- file.path(fixture, "shim")
    dir.create(file.path(shim, "run"), recursive = TRUE)
    writeLines("#!/bin/sh", file.path(shim, "run", "invented-shim"))
    native <- file.path(fixture, "native")
    dir.create(file.path(native, "run"), recursive = TRUE)
    writeLines("#!/bin/sh", file.path(native, "run", "invented-native"))
    config <- list(engines = list(nonmem = list(provision = "containerized",
                     shim_home = shim, shim_executable = "invented-shim",
                     native_executable = "invented-native", nm_version_label = "synthetic")),
                   bbi = list(mode = "local", args = list(threads = 3L, clean_lvl = 2L)))
    Sys.setenv(PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
               PMX_E_GIT_LOG = git_log)
    run_case <- function(label, mounts = NA_character_, git = "false", status = "0",
                         pass = FALSE, cfg = config, nh = native, draft = FALSE,
                         expect_git = TRUE, preexisting = FALSE, error_pattern = NULL) {
      before <- save_env()
      on.exit({ restore_env(before)
                set_unit_status("nonmem-estimation", "estimation", "approved") }, add = TRUE)
      if (is.na(mounts)) Sys.unsetenv("NONMEM_EXTRA_MOUNTS")
      else Sys.setenv(NONMEM_EXTRA_MOUNTS = mounts)
      Sys.setenv(PMX_E_GIT_OUTPUT = git, PMX_E_GIT_STATUS = status)
      writeLines(character(), git_log)
      md <- tempfile("model-", tmpdir = fixture); dir.create(md)
      output <- file.path(md, "bbi.yaml")
      snapshots <- file.path(md, "engine-config-used")
      if (preexisting) {
        writeLines("sentinel", output)
        dir.create(snapshots)
        writeLines("sentinel", file.path(snapshots, "prior.txt"))
      }
      listing_before <- list.files(md, all.files = TRUE, recursive = TRUE, no.. = TRUE)
      snapshot_existed <- dir.exists(snapshots)
      bytes_before <- lapply(listing_before, function(p) readBin(file.path(md, p), "raw", n = 100000L))
      if (draft) set_unit_status("nonmem-estimation", "estimation", "draft")
      result <- tryCatch(quiet(bb$reference_nm_workspace(md, cfg, repo_root = fixture,
                              nonmem_home = nh, bbi_path = bbi_stub)), error = function(e) e)
      succeeded <- !inherits(result, "error")
      ok(identical(succeeded, pass), label)
      if (!pass && !is.null(error_pattern)) {
        ok(!succeeded && grepl(error_pattern, conditionMessage(result)),
           paste0(label, " refusal reason"))
      }
      observed <- readLines(git_log, warn = FALSE)
      ok(if (expect_git) identical(observed, c("-C", md, "rev-parse", "--is-inside-work-tree"))
         else length(observed) == 0L, paste0(label, " Git ordering/arguments"))
      if (pass) {
        by <- tryCatch(if (succeeded && identical(result, output)) yaml::read_yaml(output)
                       else NULL, error = function(e) NULL)
        expected_home <- if (identical(cfg$engines$nonmem$provision, "native")) nh else shim
        expected_exec <- if (identical(cfg$engines$nonmem$provision, "native")) "invented-native" else "invented-shim"
        ok(!is.null(by) && identical(by$nonmem$synthetic$home, expected_home) &&
             identical(by$nonmem$synthetic$executable, expected_exec) &&
             identical(as.integer(by$threads), 3L) && identical(as.integer(by$clean_lvl), 2L) &&
             identical(by$git, FALSE) && dir.exists(snapshots) &&
             length(list.files(snapshots, pattern = "bbr-workspace.yaml$")) == 1L,
           paste0("G-E-outputs: ", label, " operational YAML and snapshot"))
      } else {
        listing_after <- list.files(md, all.files = TRUE, recursive = TRUE, no.. = TRUE)
        bytes_after <- lapply(listing_after, function(p) readBin(file.path(md, p), "raw", n = 100000L))
        ok(identical(snapshot_existed, dir.exists(snapshots)) &&
             identical(listing_before, listing_after) && identical(bytes_before, bytes_after),
           paste0("G-E-outputs: ", label, " refusal preserves outputs"))
      }
    }
    run_case("G-E-ack: declared non-Git workspace", "/declared", pass = TRUE)
    run_case("G-E-empty-parts: effective tokens", ":/declared::/other:", pass = TRUE)
    for (value in c(":", "::"))
      run_case(paste0("G-E-empty-parts: delimiter-only ", value), value)
    run_case("G-E-absent: unset", preexisting = TRUE)
    run_case("G-E-absent: empty", "")
    for (value in c("relative", "   ", "/invented-nonexistent", "/same:/same"))
      run_case(paste0("G-E-opaque: token ", encodeString(value)), value, pass = TRUE)
    for (value in c("\n/declared", "/declared\n/other", "/declared\n"))
      run_case(paste0("G-E-line: LF ", encodeString(value)), value)
    for (value in c(NA_character_, "", "::", "\n/declared"))
      run_case(paste0("G-E-git: declaration ", encodeString(value)), value, git = "true", pass = TRUE)
    for (mode in list(c("false", "1"), c("unexpected", "0"))) {
      run_case(paste0("G-E-git-error: absent status ", mode[2], " output ", mode[1]),
               git = mode[1], status = mode[2])
      run_case(paste0("G-E-git-error: acknowledgment status ", mode[2], " output ", mode[1]),
               "/declared", git = mode[1], status = mode[2], pass = TRUE)
    }
    missing_exec <- config; missing_exec$engines$nonmem$shim_executable <- "absent"
    run_case("G-E-shim: missing executable", "/declared", cfg = missing_exec,
             expect_git = FALSE, error_pattern = "shim not found")
    missing_home <- config; missing_home$engines$nonmem$shim_home <- file.path(fixture, "absent-home")
    run_case("G-E-shim: unresolvable home", "/declared", cfg = missing_home, expect_git = FALSE)
    run_case("G-E-gate: draft refuses", "/declared", draft = TRUE, expect_git = FALSE,
             error_pattern = "not yet SME-validated")
    native_cfg <- config; native_cfg$engines$nonmem$provision <- "native"
    for (value in c(NA_character_, "::", "\n/declared"))
      run_case(paste0("G-E-native: declaration ", encodeString(value)), value,
               cfg = native_cfg, pass = TRUE, expect_git = FALSE)
    run_case("G-E-native: missing home", "/declared", cfg = native_cfg,
             nh = NULL, expect_git = FALSE, error_pattern = "nonmem_home was not supplied")
    native_bad <- native_cfg; native_bad$engines$nonmem$native_executable <- "absent"
    run_case("G-E-native: missing executable", "/declared", cfg = native_bad,
             expect_git = FALSE, error_pattern = "native NONMEM executable not found")
    # Check both unset and set-empty restoration after success and caught errors.
    for (value in c(NA_character_, "")) {
      if (is.na(value)) Sys.unsetenv("NONMEM_EXTRA_MOUNTS")
      else Sys.setenv(NONMEM_EXTRA_MOUNTS = value)
      for (fail in c(FALSE, TRUE)) {
        prior <- save_env()
        tryCatch((function() {
          saved_inner <- save_env(); on.exit(restore_env(saved_inner))
          run_case("G-E-environment: nested workspace", "/declared", pass = TRUE)
          Sys.setenv(NONMEM_EXTRA_MOUNTS = "changed", PMX_E_GIT_STATUS = "9")
          if (fail) stop("deliberate fixture error")
        })(), error = function(e) NULL)
        ok(identical(prior, save_env()), "G-E-environment: exact restoration including unset/empty")
      }
    }
  }
})()

# ==== H. F8: forest verifier fails closed on NA / non-finite =================
mf <- install_skill("forest-plots", "forest.R", list(forest = list(
  status = "approved", ref = "forest.R",
  entrypoints = c("reference_forest_covstats", "reference_forest_covgrid",
                  "reference_forest_samples", "reference_forest_samples_nlmixr",
                  "reference_forest_verify", "reference_forest_parametric",
                  "reference_forest_empirical", "reference_forest_plot"))))
fo <- load_ref("forest-plots", "forest.R")
samples <- data.frame(tcl = 1.5, tv = 20)
pf_ok  <- function(thetas, df, ...) list(CL = thetas[1], V = thetas[2])
pf_na  <- function(thetas, df, ...) list(CL = thetas[1], V = NA_real_)
pf_inf <- function(thetas, df, ...) list(CL = Inf, V = thetas[2])
pf_bad <- function(thetas, df, ...) list(CL = thetas[1] * 2, V = thetas[2])
ok(isTRUE(fo$reference_forest_verify(pf_ok, samples, expected = list(CL = 1.5, V = 20))),
   "H: a matching paramFunction verifies")
expect_stop(fo$reference_forest_verify(pf_na, samples, expected = list(CL = 1.5, V = 20)),
            "H: a paramFunction returning NA for one quantity is a stop naming position 2",
            regexp = "non-finite value.*position\\(s\\) 2")
expect_stop(fo$reference_forest_verify(pf_inf, samples, expected = list(CL = 1.5, V = 20)),
            "H: a paramFunction returning Inf is a stop naming position 1",
            regexp = "non-finite value.*position\\(s\\) 1")
expect_stop(fo$reference_forest_verify(pf_ok, samples, expected = list(CL = 1.5, V = NA)),
            "H: a non-finite `expected` value is a stop naming its position",
            regexp = "expected.*position\\(s\\) 2")
expect_stop(fo$reference_forest_verify(pf_bad, samples, expected = list(CL = 1.5, V = 20)),
            "H: a finite mismatch still fails verification",
            regexp = "failed verification")

# ==== S. B0u-s: the amended mrgsolve_sim unit ====================
# Plan revision 1.4 Section 5.3, bullet B0u-s; MAP amendment A2 item 6 and item
# 13, A3 open question 8; erratum P-1.4-3. Covariate columns on the
# translation check, per-scenario $param, per-kilogram amounts formed from one
# declared weight and echoed into the record, and the explicit post-dose
# observation. Every refusal fires before mrgsolve is called, so those cases
# run with a NULL model and compile nothing. The cases after them compile a
# fixture model and run only when PMX_BATCH7_COMPILE=1 (lane rules forbid
# compilers in lanes; root runs them on a private copy).
mf <- install_skill("simulation-standards", c("mrgsolve_sim.R", "exposure_metrics.R"), list(
  mrgsolve_sim = list(status = "approved", ref = "mrgsolve_sim.R",
                      entrypoints = c("reference_sim_compile", "reference_sim_translate_check",
                                      "reference_sim_physical_checks", "reference_simulate")),
  exposure_metrics = list(status = "approved", ref = "exposure_metrics.R",
                          entrypoints = c("reference_exposure_by_subject",
                                          "reference_exposure_summary",
                                          "reference_profile_summary"))))
file.copy(src_file("tools/seed.R"), "tools/seed.R", overwrite = TRUE)
ms <- load_ref("simulation-standards", "mrgsolve_sim.R")
em <- load_ref("simulation-standards", "exposure_metrics.R")

s_design <- data.frame(ID = 1, TIME = c(0, 1), EVID = c(1, 0), AMT = c(10, 0), CMT = c(1, 0))
expect_stop(ms$reference_sim_translate_check(NULL, s_design, data.frame(), covariates = "WT"),
            "S: a declared covariate column missing from the design stops",
            regexp = "missing column\\(s\\): WT")
expect_stop(ms$reference_sim_translate_check(NULL, s_design, data.frame(), covariates = 1),
            "S: covariates must be column names",
            regexp = "covariates must be")

s_grid <- list(start = 0, end = 24, delta = 1)
s_sc <- function(regimen, ...) list(regimen = regimen, n = 1L, grid = s_grid, ...)
s_refuse <- function(sc, name, rx)
  expect_stop(ms$reference_simulate(NULL, list(a = sc), seed = 1), name, regexp = rx)
s_refuse(s_sc(list(amt = 60, amt_per_kg = 20, weight_param = "WT", cmt = 1), param = list(WT = 3)),
         "S: amt and amt_per_kg together stop", "either amt or amt_per_kg")
s_refuse(s_sc(list(amt_per_kg = 20, cmt = 1), param = list(WT = 3)),
         "S: amt_per_kg without weight_param stops", "needs weight_param")
s_refuse(s_sc(list(amt_per_kg = 20, weight_param = "BW", cmt = 1), param = list(WT = 3)),
         "S: a weight_param naming no $param entry stops", "names no \\$param entry")
s_refuse(s_sc(list(amt_per_kg = 20, weight_param = "WT", cmt = 1)),
         "S: a weight_param with no $param at all stops", "names no \\$param entry")
s_refuse(s_sc(list(amt_per_kg = NA_real_, weight_param = "WT", cmt = 1), param = list(WT = 3)),
         "S: a non-finite amt_per_kg stops", "amt_per_kg must be a single finite positive number")
s_refuse(s_sc(list(amt_per_kg = c(10, 20), weight_param = "WT", cmt = 1), param = list(WT = 3)),
         "S: an amt_per_kg of length two stops", "amt_per_kg must be a single finite positive number")
s_refuse(s_sc(list(amt_per_kg = -20, weight_param = "WT", cmt = 1), param = list(WT = 3)),
         "S: a negative amt_per_kg stops", "amt_per_kg must be a single finite positive number")
s_refuse(s_sc(list(amt_per_kg = 20, weight_param = "WT", cmt = 1), param = list(WT = Inf)),
         "S: a non-finite weight stops", "'WT' must be a single finite positive number")
s_refuse(s_sc(list(amt_per_kg = 20, weight_param = "WT", cmt = 1), param = list(WT = 0)),
         "S: a zero weight stops", "'WT' must be a single finite positive number")
s_refuse(s_sc(list(amt = 60, weight_param = "WT", cmt = 1), param = list(WT = 3)),
         "S: weight_param without amt_per_kg stops", "weight_param is given without amt_per_kg")
s_refuse(s_sc(list(amt = 60, cmt = 1), param = list(3)),
         "S: an unnamed $param stops", "param must be a named list")
s_refuse(s_sc(list(amt = 60, cmt = 1), post_dose_obs = "yes"),
         "S: post_dose_obs other than TRUE or FALSE stops", "post_dose_obs must be TRUE or FALSE")
s_refuse(s_sc(list(amt = 60, cmt = 1, start = 0.5), post_dose_obs = TRUE),
         "S: post_dose_obs with a dose time off the grid stops", "not on the sampling grid")
s_refuse(list(regimen = list(amt = 60, cmt = 1, ii = 12, addl = 1), n = 1L,
              times = c(0, 6, 18), post_dose_obs = TRUE),
         "S: post_dose_obs with an addl dose off the times stops", "not on the sampling grid")

if (!identical(Sys.getenv("PMX_BATCH7_COMPILE"), "1")) {
  cat("NOTE  S/compile: the mrgsolve cases were not run (they compile a model; ",
      "root sets PMX_BATCH7_COMPILE=1 on a private copy)\n", sep = "")
} else if (!requireNamespace("mrgsolve", quietly = TRUE)) {
  ok(FALSE, "S/compile: PMX_BATCH7_COMPILE=1 but mrgsolve is not installed")
} else {
  # Fixture model: one compartment, intravenous bolus into CENT, weight on CL
  # and V. The three values below are FIXTURE values declared here, not
  # estimates; every expectation is an R expression over them. Realistic rates
  # (erratum P-1.3-2): placeholder rates of 1 underflow a long grid.
  S_TVCL <- 0.005                 # fixture, L/h per kg
  S_TVV  <- 1                     # fixture, L per kg
  S_WT   <- c(a = 1.5, b = 3.5)   # fixture weights, kg
  S_DOSE_PER_KG <- 20             # fixture, mg/kg
  s_code <- paste0(
    "$PARAM WT = 1, TVCL = ", S_TVCL, ", TVV = ", S_TVV, "\n",
    "$CMT CENT\n",
    "$OMEGA 0\n$SIGMA 0\n",
    "$MAIN\ndouble CL = TVCL * WT;\ndouble V = TVV * WT;\n",
    "$ODE\ndxdt_CENT = -(CL / V) * CENT;\n",
    "$TABLE\ncapture CP = CENT / V;\n",
    "$CAPTURE CL V\n")
  s_mod <- quiet(ms$reference_sim_compile(s_code, name = "b0us_fixture"))
  s_same <- function(x, y) {
    rownames(x) <- NULL; rownames(y) <- NULL; identical(x, y)
  }
  s_exposure <- function(res)
    em$reference_exposure_by_subject(res$sim, res$record$scenarios, observed = "CP",
                                     time = "time", clearance = "CL", volume = "V",
                                     auc_tolerance = 1e-6)

  # Legacy call: no new field; output equals a direct mrgsolve call, and the
  # record echoes the declared scenarios unchanged.
  s_legacy <- list(a = s_sc(list(amt = 10, cmt = 1)))
  r0 <- ms$reference_simulate(s_mod, s_legacy, seed = 1)
  d0 <- mrgsolve::as.data.frame(mrgsolve::mrgsim(
    mrgsolve::zero_re(s_mod), events = mrgsolve::ev(amt = 10, cmt = 1), nid = 1L,
    obsonly = TRUE, start = 0, end = 24, delta = 1))
  ok(s_same(r0$sim[names(d0)], d0) && identical(r0$record$scenarios, s_legacy),
     "S: a legacy call is a plain mrgsolve run and echoes the declared scenarios")
  ok(identical(r0$sim$CP[r0$sim$time == 0], 0),
     "S: without post_dose_obs the time-0 grid value precedes the dose (row e)")
  r0f <- ms$reference_simulate(s_mod, list(a = s_sc(list(amt = 10, cmt = 1),
                                                     post_dose_obs = FALSE)), seed = 1)
  ok(identical(r0f$sim, r0$sim), "S: post_dose_obs = FALSE is today's output")

  # Covariates on the translation check: the design's WT reaches the model.
  s_des <- data.frame(ID = c(1, 1, 1, 2, 2, 2), TIME = c(0, 2, 6, 0, 2, 6),
                      EVID = c(1, 0, 0, 1, 0, 0), AMT = c(30, 0, 0, 70, 0, 0),
                      CMT = c(1, 0, 0, 1, 0, 0),
                      WT = rep(unname(S_WT), each = 3))
  s_obs <- s_des[s_des$EVID == 0, ]
  s_ke <- S_TVCL / S_TVV   # CL / V, the same for every weight in this fixture
  s_pred <- data.frame(ID = s_obs$ID, TIME = s_obs$TIME,
                       PRED = s_des$AMT[match(s_obs$ID, s_des$ID)] / (S_TVV * s_obs$WT) *
                              exp(-s_ke * s_obs$TIME))
  chk <- tryCatch(ms$reference_sim_translate_check(s_mod, s_des, s_pred, covariates = "WT"),
                  error = function(e) e)
  if (inherits(chk, "error")) cat(sprintf("      got: %s\n", conditionMessage(chk)))
  ok(is.list(chk) && !inherits(chk, "error") && chk$n == nrow(s_obs),
     "S: covariate translation within 1e-3 (design WT carried into the check)")
  expect_stop(ms$reference_sim_translate_check(s_mod, s_des, s_pred),
              "S: the same check without covariates is refused (model default WT)",
              regexp = "Translation failed equivalence")

  # Per-scenario $param against separate runs on models set with param().
  s_two <- list(a = s_sc(list(amt = 10, cmt = 1), param = list(WT = S_WT[["a"]])),
                b = s_sc(list(amt = 10, cmt = 1), param = list(WT = S_WT[["b"]])))
  r2 <- ms$reference_simulate(s_mod, s_two, seed = 1)
  sep <- lapply(c("a", "b"), function(nm)
    ms$reference_simulate(mrgsolve::param(s_mod, WT = S_WT[[nm]]),
                          stats::setNames(list(s_sc(list(amt = 10, cmt = 1))), nm),
                          seed = 1)$sim)
  ok(s_same(r2$sim[r2$sim$SCENARIO == "a", ], sep[[1]]) &&
     s_same(r2$sim[r2$sim$SCENARIO == "b", ], sep[[2]]),
     "S: per-scenario parameters identical to separate runs")
  ok(!identical(r2$sim$CP[r2$sim$SCENARIO == "a"], r2$sim$CP[r2$sim$SCENARIO == "b"]),
     "S: the two scenarios' parameters differ in the output")
  # An unknown per-scenario parameter name stops (param(.strict = TRUE));
  # without .strict mrgsolve would drop the name silently and the run succeed.
  expect_stop(ms$reference_simulate(s_mod, list(a = s_sc(list(amt = 10, cmt = 1),
                                                         param = list(KA = 1))), seed = 1),
              "S/compile: an unknown per-scenario parameter name stops",
              regexp = "not a model parameter")

  # Per-kilogram amounts against explicit amounts written as R expressions.
  s_kg <- list(a = s_sc(list(amt_per_kg = S_DOSE_PER_KG, weight_param = "WT", cmt = 1),
                        param = list(WT = S_WT[["a"]])),
               b = s_sc(list(amt_per_kg = S_DOSE_PER_KG, weight_param = "WT", cmt = 1),
                        param = list(WT = S_WT[["b"]])))
  s_ex <- list(a = s_sc(list(amt = S_DOSE_PER_KG * S_WT[["a"]], cmt = 1),
                        param = list(WT = S_WT[["a"]])),
               b = s_sc(list(amt = S_DOSE_PER_KG * S_WT[["b"]], cmt = 1),
                        param = list(WT = S_WT[["b"]])))
  rk <- ms$reference_simulate(s_mod, s_kg, seed = 1)
  rx <- ms$reference_simulate(s_mod, s_ex, seed = 1)
  ok(identical(rk$sim, rx$sim), "S: per-kilogram amounts identical to explicit amounts")
  ok(identical(rk$record$scenarios$a$regimen$amt, s_ex$a$regimen$amt) &&
     identical(rk$record$scenarios$b$regimen$amt, s_ex$b$regimen$amt) &&
     identical(rk$record$scenarios$a$regimen$amt_per_kg, S_DOSE_PER_KG) &&
     identical(rk$record$scenarios$a$regimen$weight_param, "WT"),
     "S: resolved amount in the record, amt_per_kg and weight_param kept beside it")
  ok(identical(s_exposure(rk), s_exposure(rx)),
     "S: exposure metrics identical to explicit amounts (AUC(0-inf) reads the echoed amount)")

  # Post-dose observation: cmax equals dose over V for the bolus.
  s_pd <- list(a = s_sc(list(amt_per_kg = S_DOSE_PER_KG, weight_param = "WT", cmt = 1),
                        param = list(WT = S_WT[["a"]]), post_dose_obs = TRUE))
  rp <- ms$reference_simulate(s_mod, s_pd, seed = 1)
  bp <- s_exposure(rp)
  s_bound <- (S_DOSE_PER_KG * S_WT[["a"]]) / (S_TVV * S_WT[["a"]])
  ok(nrow(rp$sim) == nrow(r0$sim) && abs(bp$cmax / s_bound - 1) <= 1e-3,
     "S: post-dose cmax equals dose over V (relative 1e-3), grid unchanged")
  ok(max(rk$sim$CP[rk$sim$SCENARIO == "a"]) < bp$cmax,
     "S: the same scenario without post_dose_obs misses the peak")
}

# ==== W. AC12: bound comparison as a manifested simulation-standards unit =====
# bound-comparison coverage:
# the verdict columns of the percentile-against-bound table, computed by the
# unit through its own gate. Every expected verdict below is a literal chosen
# by construction of the fixture numbers; no case re-implements the
# comparison. The fixture bounds are 1 and 4, powers of two, so the double
# one step below 1 is exactly 1 - 2^-53 and the double one step above 4 is
# exactly 4 + 2^-50 (the spacing of doubles in [1, 2) is 2^-52, in [0.5, 1)
# it is 2^-53 and in [4, 8) it is 2^-50).
mf <- install_skill("simulation-standards", "bound_comparison.R", list(
  bound_comparison = list(status = "approved", ref = "bound_comparison.R",
                          entrypoints = "reference_bound_verdict")))
ok(!is.na(mf), "W: W1 skills/simulation-standards/reference/bound_comparison.R exists in the source tree")
if (!is.na(mf)) {
  bc <- load_ref("simulation-standards", "bound_comparison.R")
  ok(is.function(bc$reference_bound_verdict) &&
       identical(names(formals(bc$reference_bound_verdict)),
                 c("lower", "upper", "bound_lower", "bound_upper")) &&
       all(!nzchar(as.character(formals(bc$reference_bound_verdict)))),
     "W: W1 the file loads; reference_bound_verdict(lower, upper, bound_lower, bound_upper) has no defaults")
  bv <- function(lower, upper, bound_lower, bound_upper)
    bc$reference_bound_verdict(lower, upper, bound_lower, bound_upper)
  W_COLS <- c("verdict_lower", "verdict_upper", "verdict")
  w_frame <- function(vl, vu, v)
    data.frame(verdict_lower = vl, verdict_upper = vu, verdict = v, stringsAsFactors = FALSE)

  # W2: four rows, each percentile inside its bounds.
  w2 <- bv(c(2, 3, 1.5, 20), c(3, 3.5, 2, 30), c(1, 1, 1, 10), c(4, 4, 4, 40))
  ok(is.data.frame(w2) && identical(names(w2), W_COLS) && nrow(w2) == 4L &&
       all(vapply(w2, is.character, logical(1))) &&
       identical(w2, w_frame(rep("PASS", 4), rep("PASS", 4), rep("PASS", 4))),
     "W: W2 all rows inside their bounds: three character columns named verdict_lower, verdict_upper, verdict, every cell PASS, one row per input")
  # W3: the upper percentile above its upper bound.
  ok(identical(bv(2, 5, 1, 4), w_frame("PASS", "FAIL", "FAIL")),
     "W: W3 an upper percentile above its upper bound reads PASS, FAIL, FAIL")
  # W4: the lower percentile below its lower bound.
  ok(identical(bv(0.5, 3, 1, 4), w_frame("FAIL", "PASS", "FAIL")),
     "W: W4 a lower percentile below its lower bound reads FAIL, PASS, FAIL")
  # W5: both sides outside.
  ok(identical(bv(0.5, 5, 1, 4), w_frame("FAIL", "FAIL", "FAIL")),
     "W: W5 both percentiles outside their bounds read FAIL, FAIL, FAIL")
  # W6: equality passes on both sides; one representable double beyond fails.
  below1 <- 1 - 2^-53
  above4 <- 4 + 2^-50
  ok(below1 < 1 && (below1 + 2^-53) == 1 && above4 > 4 && (above4 - 2^-50) == 4,
     "W: W6 fixture construction: 1 - 2^-53 and 4 + 2^-50 are the neighbouring doubles of the bounds")
  ok(identical(bv(1, 4, 1, 4), w_frame("PASS", "PASS", "PASS")),
     "W: W6 a percentile equal to its bound passes, on both sides (inclusive)")
  ok(identical(bv(c(below1, 2), c(3, above4), c(1, 1), c(4, 4)),
               w_frame(c("FAIL", "PASS"), c("PASS", "FAIL"), c("FAIL", "FAIL"))),
     "W: W6 one representable double beyond each bound fails (exact, no tolerance)")
  # W7: the rows of W2 to W6 in one call, then permuted.
  w7_in <- list(lower = c(2, 3, 1.5, 20, 2, 0.5, 0.5, 1, below1, 2),
                upper = c(3, 3.5, 2, 30, 5, 3, 5, 4, 3, above4),
                bound_lower = c(1, 1, 1, 10, 1, 1, 1, 1, 1, 1),
                bound_upper = c(4, 4, 4, 40, 4, 4, 4, 4, 4, 4))
  w7_out <- w_frame(c("PASS", "PASS", "PASS", "PASS", "PASS", "FAIL", "FAIL", "PASS", "FAIL", "PASS"),
                    c("PASS", "PASS", "PASS", "PASS", "FAIL", "PASS", "FAIL", "PASS", "PASS", "FAIL"),
                    c("PASS", "PASS", "PASS", "PASS", "FAIL", "FAIL", "FAIL", "PASS", "FAIL", "FAIL"))
  w7_perm <- c(9L, 5L, 1L, 10L, 7L, 3L, 6L, 2L, 8L, 4L)
  ok(identical(do.call(bv, w7_in), w7_out),
     "W: W7 the rows of W2 to W6 in one call read their own verdicts")
  w7_p <- do.call(bv, lapply(w7_in, function(v) v[w7_perm]))
  w7_exp <- w7_out[w7_perm, , drop = FALSE]
  rownames(w7_exp) <- NULL
  ok(identical(w7_p, w7_exp), "W: W7 permuted rows give the permuted verdicts (input order kept)")
  # W8: NA, NaN, Inf and -Inf in each argument stop, naming the argument and
  # the position (position 2 of three rows).
  w8_ok <- list(lower = c(2, 2, 2), upper = c(3, 3, 3), bound_lower = c(1, 1, 1),
                bound_upper = c(4, 4, 4))
  for (a in names(w8_ok)) for (bad in list(NA_real_, NaN, Inf, -Inf)) {
    w8 <- w8_ok
    w8[[a]][2] <- bad
    expect_stop(do.call(bv, w8),
                sprintf("W: W8 %s in %s stops, naming the argument and position 2",
                        format(bad), a),
                regexp = sprintf("argument '%s' is NA, NaN or infinite at position 2", a))
  }
  # W9: shape refusals.
  expect_stop(bv(c(2, 2, 2), c(3, 3), c(1, 1, 1), c(4, 4, 4)),
              "W: W9 a length different from lower stops, naming the argument",
              regexp = "argument 'upper' has a length different from 'lower'; the first position without a counterpart is 3")
  expect_stop(bv(numeric(0), numeric(0), numeric(0), numeric(0)),
              "W: W9 length 0 stops", regexp = "argument 'lower' has length 0")
  expect_stop(bv(c(2, 2), c("3", "3"), c(1, 1), c(4, 4)),
              "W: W9 a character argument stops", regexp = "argument 'upper' must be a numeric vector")
  expect_stop(bv(c(2, 2), c(3, 3), c(1, 4), c(4, 4)),
              "W: W9 a lower bound equal to its upper bound stops, naming the position",
              regexp = "argument 'bound_lower' is not below 'bound_upper' at position 2")
  expect_stop(bv(c(2, 2), c(3, 3), c(1, 5), c(4, 4)),
              "W: W9 a lower bound above its upper bound stops, naming the position",
              regexp = "argument 'bound_lower' is not below 'bound_upper' at position 2")
  expect_stop(bc$reference_bound_verdict(c(2, 2), c(3, 3), c(1, 1)),
              "W: W9 a missing argument stops, naming it",
              regexp = "argument 'bound_upper' is missing; it has no default")
  # W10: a draft unit refuses.
  set_unit_status("simulation-standards", "bound_comparison", "draft")
  expect_stop(bv(2, 3, 1, 4), "W: W10 the bound_comparison unit gates: draft status refuses to compute",
              regexp = "bound_comparison.*not SME-validated")
  # W11: an empty root reviewed_by refuses, with the unit approved.
  write_fixture_manifest("simulation-standards", list(
    bound_comparison = list(status = "approved", ref = "bound_comparison.R",
                            entrypoints = "reference_bound_verdict")), reviewed_by = "")
  expect_stop(bv(2, 3, 1, 4), "W: W11 an empty root reviewed_by refuses to compute",
              regexp = "bound_comparison.*not SME-validated")
  # W11: the task module calls the unit and compares no bound itself (the
  # section D pattern).
  ttxt <- paste(readLines(src_file("pipeline/tasks/simulation.R"), warn = FALSE), collapse = "\n")
  ok(grepl("reference_bound_verdict", ttxt, fixed = TRUE) &&
       !grepl("bound_(lower|upper)[]\"')]*[[:space:]]*(<=|>=|<|>|==|!=)", ttxt) &&
       !grepl("(<=|>=|<|>|==|!=)[[:space:]]*[[:alnum:]_.$[\"']*bound_(lower|upper)", ttxt),
     "W: W11 pipeline/tasks/simulation.R calls reference_bound_verdict and applies no comparison operator to bound_lower or bound_upper")
}

cat(sprintf("\nintegrity batch 7 battery: %d passed, %d failed\n", .pass, .fail))
quit(status = if (.fail > 0L) 1L else 0L)

}  # end .run_batch7_tests

if (.invoked_as_script()) .run_batch7_tests()
