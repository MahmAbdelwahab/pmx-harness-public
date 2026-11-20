#!/usr/bin/env Rscript
# Method-manifest strictness battery (Codex round 7, finding 5).
#
# tools/method_manifest.R used to parse `skill`, `reference_path`,
# `entrypoints` and `reviewed_at` without validating any of them, and accepted
# duplicate root fields and duplicate unit names. This battery pins the strict
# contract: every fixture lives in a tempdir laid out as <root>/skills/<name>/,
# and the working directory is moved there so repo-relative reference paths
# resolve exactly as they do in the repository.
#
# Run with:  Rscript tools/tests/test_method_manifest.R
# Exits non-zero if any assertion fails.

# SAFE TO SOURCE: pipelines tar_source("tools") which recurses into
# tools/tests/; everything observable is guarded behind .invoked_as_script().
.invoked_as_script <- function() {
  fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  length(fa) > 0 && grepl("test_method_manifest\\.R$", sub("^--file=", "", fa[1]))
}

.run_method_manifest_tests <- function() {

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
expect_stop <- function(expr, name, regexp = NULL) {
  e <- tryCatch({ force(expr); NULL }, error = function(e) e)
  hit <- !is.null(e) && (is.null(regexp) || grepl(regexp, conditionMessage(e)))
  if (!is.null(e) && !hit) cat(sprintf("      got: %s\n", conditionMessage(e)))
  if (is.null(e)) cat("      no error was raised\n")
  ok(hit, name)
}

# --- fixture root -----------------------------------------------------------
root <- tempfile("mmfix"); dir.create(root)
old_wd <- setwd(root); on.exit(setwd(old_wd), add = TRUE)
dir.create("tools")
writeLines(c("keep_rows <- function(d) d", "helper <- 1"), "tools/record_selection.R")
# The real validator file, so that fixtures which sys.source() it into a
# trusted object are judged against its ACTUAL 66 top-level bindings. Without
# it the B5 shadowing check has nothing to compare against and silently passes
# — which is how the first cut of that check looked green (fix round 3).
file.copy(file.path(old_wd, "tools", "method_manifest.R"),
          "tools/method_manifest.R", overwrite = TRUE)
sha <- function(p) .mm_file_sha256(p)

# A valid two-unit skill. `f` and `g` are top-level function definitions; `h`
# is a non-function binding and must NOT count as an entry point.
mk_skill <- function(name = "demo") {
  d <- file.path("skills", name); dir.create(file.path(d, "reference"), recursive = TRUE)
  writeLines(c("f <- function() 1", "g = function(x) x", "h <- 42",
               "local({ nested <- function() 0 })"),
             file.path(d, "reference", "method.R"))
  d
}
manifest_lines <- function(skill = "demo", ref = "skills/demo/reference/method.R",
                           units = NULL, reviewed_by = '""', reviewed_at = '""',
                           extra_root = character(0), sha_value = NULL) {
  sha_value <- sha_value %||% sha(ref)
  if (is.null(units)) units <- c(
    "  alpha:", "    status: approved", paste0("    reference_path: ", ref),
    "    entrypoints: [f]", paste0("    sha256: ", sha_value),
    "    evidence: SKILL.md", '    reason: "signed"',
    "  beta:", "    status: draft", paste0("    reference_path: ", ref),
    "    entrypoints: [g]", paste0("    sha256: ", sha_value),
    "    evidence: SKILL.md", '    reason: "not yet"')
  c("# fixture", paste0("skill: ", skill), "generated_at: 2026-09-03T00:00:00Z",
    paste0("reviewed_by: ", reviewed_by), paste0("reviewed_at: ", reviewed_at),
    extra_root, "units:", units)
}
write_manifest <- function(lines, skill = "demo") {
  p <- file.path("skills", skill, "MANIFEST.yaml"); writeLines(lines, p); p
}

# ==== A. a well-formed manifest validates ==================================
d <- mk_skill()
mf <- write_manifest(manifest_lines())
v <- tryCatch(validate_method_manifest(mf), error = function(e) e)
ok(!inherits(v, "error") && identical(sort(names(v$units)), c("alpha", "beta")),
   "A: a well-formed two-unit manifest validates and returns the parsed manifest")
ok(isFALSE(suppressWarnings(is_method_validated(d, "skills/demo/reference/method.R", "alpha"))),
   "A: unconfirmed manifest inspection -> approved-in-spec unit is FALSE")
write_manifest(manifest_lines(reviewed_by = '"Fixture Reviewer"', reviewed_at = '"2026-09-01"'))
ok(isTRUE(require_validated_method(d, "skills/demo/reference/method.R", "alpha")),
   "A: confirmed manifest with a valid date -> approved unit TRUE")

role_ref <- file.path(d, "reference", "method.R")
for (reviewer in c(" sMe ", "THE   SME")) {
  write_manifest(manifest_lines(reviewed_by = paste0('"', reviewer, '"'),
                                reviewed_at = '"2026-09-01"'))
  expect_stop(validate_method_manifest(mf),
              "MM-ROLE-REFUSAL: digest-valid reviewed unit rejects reserved label",
              "reserved role label")
  expect_stop(require_validated_method(d, role_ref, "alpha"),
              "MM-ROLE-REFUSAL: execution authorization rejects reserved label",
              "reserved role label")
  prior <- readBin(mf, "raw", file.info(mf)$size)
  spec <- list(alpha = list(reference_path = role_ref, entrypoints = "f", status = "approved",
                            evidence = "SKILL.md", reason = "signed"),
               beta = list(reference_path = role_ref, entrypoints = "g", status = "draft",
                           evidence = "SKILL.md", reason = "not yet"))
  expect_stop(write_method_manifest(d, spec),
              "MM-ROLE-REFUSAL: unchanged regeneration refuses to retain reserved label",
              "reserved role label")
  ok(identical(readBin(mf, "raw", file.info(mf)$size), prior),
     "MM-ROLE-REFUSAL: rejected regeneration changes no manifest bytes")
}
write_manifest(manifest_lines(reviewed_by = '"Fixture SME Reviewer"', reviewed_at = '"2026-09-01"'))
ok(isTRUE(require_validated_method(d, role_ref, "alpha")),
   "MM-LOAD: ordinary local reviewer name containing SME is accepted")

# ==== B. duplicate and unknown fields are refused ==========================
write_manifest(manifest_lines(extra_root = 'reviewed_by: "ZZ"'))
expect_stop(validate_method_manifest(mf), "B: duplicate root field refused", "duplicate root field 'reviewed_by'")
write_manifest(manifest_lines(extra_root = "approver: Fixture Reviewer"))
expect_stop(validate_method_manifest(mf), "B: unknown root field refused", "unknown root field 'approver'")
ref <- "skills/demo/reference/method.R"
u_dup <- c("  alpha:", "    status: approved", paste0("    reference_path: ", ref),
           "    entrypoints: [f]", paste0("    sha256: ", sha(ref)),
           "    evidence: SKILL.md", '    reason: "a"',
           "  alpha:", "    status: draft", paste0("    reference_path: ", ref),
           "    entrypoints: [g]", paste0("    sha256: ", sha(ref)),
           "    evidence: SKILL.md", '    reason: "b"')
write_manifest(manifest_lines(units = u_dup))
expect_stop(validate_method_manifest(mf), "B: duplicate unit name refused", "duplicate unit 'alpha'")
u_dupf <- c("  alpha:", "    status: approved", "    status: draft",
            paste0("    reference_path: ", ref), "    entrypoints: [f, g]",
            paste0("    sha256: ", sha(ref)), "    evidence: SKILL.md", '    reason: "a"')
write_manifest(manifest_lines(units = u_dupf))
expect_stop(validate_method_manifest(mf), "B: duplicate unit field refused", "duplicate field 'status' in unit 'alpha'")
u_unk <- c("  alpha:", "    status: approved", paste0("    reference_path: ", ref),
           "    entrypoints: [f, g]", paste0("    sha256: ", sha(ref)),
           "    evidence: SKILL.md", '    reason: "a"', "    approved_by: Fixture Reviewer")
write_manifest(manifest_lines(units = u_unk))
expect_stop(validate_method_manifest(mf), "B: unknown unit field refused", "unknown field 'approved_by' in unit 'alpha'")
u_miss <- c("  alpha:", "    status: approved", paste0("    reference_path: ", ref),
            "    entrypoints: [f, g]", "    evidence: SKILL.md", '    reason: "a"')
write_manifest(manifest_lines(units = u_miss))
expect_stop(validate_method_manifest(mf), "B: missing required unit field refused", "missing field 'sha256'")
write_manifest(c("skill: demo", "generated_at: x", 'reviewed_by: ""', 'reviewed_at: ""'))
expect_stop(validate_method_manifest(mf), "B: a manifest without a units block is refused", "units")

# ==== C. identity: skill name, reference path, entry points ================
write_manifest(manifest_lines(skill = "other"))
expect_stop(validate_method_manifest(mf), "C: skill field must equal the directory name", "skill: other.*directory.*demo")
write_manifest(manifest_lines(ref = "skills/demo/reference/missing.R", sha_value = sha(ref)))
expect_stop(validate_method_manifest(mf), "C: reference_path must exist", "does not exist")
dir.create("skills/elsewhere/reference", recursive = TRUE)
writeLines("f <- function() 1\ng <- function() 2", "skills/elsewhere/reference/method.R")
write_manifest(manifest_lines(ref = "skills/elsewhere/reference/method.R"))
expect_stop(validate_method_manifest(mf), "C: reference_path must be under the skill", "not under skills/demo/")
write_manifest(manifest_lines(ref = "tools/record_selection.R"))
expect_stop(validate_method_manifest(mf), "C: a reference_path outside skills/ is refused", "not under skills/demo/")
u_ep <- c("  alpha:", "    status: approved", paste0("    reference_path: ", ref),
          "    entrypoints: [f, h]", paste0("    sha256: ", sha(ref)),
          "    evidence: SKILL.md", '    reason: "a"')
write_manifest(manifest_lines(units = u_ep))
expect_stop(validate_method_manifest(mf), "C: a non-function binding is not an entry point", "entry point 'h'.*not a top-level function")
u_ep2 <- c("  alpha:", "    status: approved", paste0("    reference_path: ", ref),
           "    entrypoints: [f, nested]", paste0("    sha256: ", sha(ref)),
           "    evidence: SKILL.md", '    reason: "a"')
write_manifest(manifest_lines(units = u_ep2))
expect_stop(validate_method_manifest(mf), "C: a nested function is not a top-level entry point", "entry point 'nested'")
u_ep3 <- c("  alpha:", "    status: approved", paste0("    reference_path: ", ref),
           "    entrypoints: []", paste0("    sha256: ", sha(ref)),
           "    evidence: SKILL.md", '    reason: "a"')
write_manifest(manifest_lines(units = u_ep3))
expect_stop(validate_method_manifest(mf), "C: an empty entrypoints list is refused", "no entrypoints")
u_two <- c("  alpha:", "    status: approved", paste0("    reference_path: ", ref),
           "    entrypoints: [f, g]", paste0("    sha256: ", sha(ref)),
           "    evidence: SKILL.md", '    reason: "a"',
           "  beta:", "    status: draft", paste0("    reference_path: ", ref),
           "    entrypoints: [g]", paste0("    sha256: ", sha(ref)),
           "    evidence: SKILL.md", '    reason: "b"')
write_manifest(manifest_lines(units = u_two))
expect_stop(validate_method_manifest(mf), "C: an entry point bound to two units is refused", "entry point 'g'.*units alpha, beta")

# ==== D. digest grammar and equality =======================================
write_manifest(manifest_lines(sha_value = "sha256:abc"))
expect_stop(validate_method_manifest(mf), "D: malformed sha256 refused", "malformed sha256")
write_manifest(manifest_lines(sha_value = sub("^sha256:", "", sha(ref))))
expect_stop(validate_method_manifest(mf), "D: sha256 without the prefix refused", "malformed sha256")
write_manifest(manifest_lines(sha_value = paste0("sha256:", paste(rep("0", 64), collapse = ""))))
expect_stop(validate_method_manifest(mf), "D: sha256 that does not match the file refused", "digest drift")

# ==== E. reviewed_at is a date once reviewed_by is filled ==================
write_manifest(manifest_lines(reviewed_by = '"Fixture Reviewer"', reviewed_at = '""'))
expect_stop(validate_method_manifest(mf), "E: filled reviewed_by with empty reviewed_at refused", "reviewed_at")
write_manifest(manifest_lines(reviewed_by = '"Fixture Reviewer"', reviewed_at = '"yesterday"'))
expect_stop(validate_method_manifest(mf), "E: non-date reviewed_at refused", "reviewed_at 'yesterday'")
write_manifest(manifest_lines(reviewed_by = '"Fixture Reviewer"', reviewed_at = '"2026-02-30"'))
expect_stop(validate_method_manifest(mf), "E: impossible calendar date refused", "reviewed_at '2026-02-30'")
write_manifest(manifest_lines(reviewed_by = '"Fixture Reviewer"', reviewed_at = '"2026-09-01"'))
ok(!inherits(tryCatch(validate_method_manifest(mf), error = function(e) e), "error"),
   "E: ISO date accepted")
write_manifest(manifest_lines(reviewed_by = '"Fixture Reviewer"', reviewed_at = '"2026-09-01"'))
expect_stop(require_validated_method(d, ref, "alpha", legacy = NULL) && {
              write_manifest(manifest_lines(reviewed_by = '"Fixture Reviewer"', reviewed_at = '"bad"'))
              require_validated_method(d, ref, "alpha") },
            "E: require_validated_method hard-stops on a malformed manifest (not FALSE)",
            "reviewed_at")

# ==== F. per-unit depends: {path, sha256} ===================================
dep_sha <- sha("tools/record_selection.R")
u_dep <- function(dsha = dep_sha, dpath = "tools/record_selection.R") c(
  "  alpha:", "    status: approved", paste0("    reference_path: ", ref),
  "    entrypoints: [f]", paste0("    sha256: ", sha(ref)),
  "    depends:", paste0("      - path: ", dpath), paste0("        sha256: ", dsha),
  "    evidence: SKILL.md", '    reason: "a"')
write_manifest(manifest_lines(units = u_dep(), reviewed_by = '"Fixture Reviewer"', reviewed_at = '"2026-09-01"'))
vd <- tryCatch(validate_method_manifest(mf), error = function(e) e)
ok(!inherits(vd, "error") && length(vd$units$alpha$depends) == 1L &&
     identical(vd$units$alpha$depends[[1]]$path, "tools/record_selection.R"),
   "F: a depends entry that exists and matches parses and validates")
ok(isTRUE(require_validated_method(d, ref, "alpha")),
   "F: require_validated_method TRUE when the dependency matches")
writeLines(c("keep_rows <- function(d) d[1, ]", "helper <- 1"), "tools/record_selection.R")
expect_stop(validate_method_manifest(mf), "F: a drifted dependency is refused", "depends.*record_selection.R.*digest drift")
expect_stop(require_validated_method(d, ref, "alpha"),
            "F: require_validated_method hard-stops on a drifted dependency", "record_selection.R")
writeLines(c("keep_rows <- function(d) d", "helper <- 1"), "tools/record_selection.R")
write_manifest(manifest_lines(units = u_dep(dpath = "tools/nowhere.R")))
expect_stop(validate_method_manifest(mf), "F: a missing dependency file is refused", "nowhere.R.*does not exist")
write_manifest(manifest_lines(units = u_dep(dsha = "sha256:zz")))
expect_stop(validate_method_manifest(mf), "F: a malformed dependency sha256 is refused", "malformed sha256")
u_dep_bad <- c("  alpha:", "    status: approved", paste0("    reference_path: ", ref),
  "    entrypoints: [f]", paste0("    sha256: ", sha(ref)),
  "    depends:", "      - path: tools/record_selection.R",
  "    evidence: SKILL.md", '    reason: "a"')
write_manifest(manifest_lines(units = u_dep_bad))
expect_stop(validate_method_manifest(mf), "F: a depends entry without sha256 is refused", "depends.*sha256")

# ==== G. non-R reference files (control-stream templates) ===================
dn <- file.path("skills", "ctl"); dir.create(file.path(dn, "reference"), recursive = TRUE)
writeLines("$PROBLEM x", file.path(dn, "reference", "onecmt.ctl"))
cref <- "skills/ctl/reference/onecmt.ctl"
u_ctl <- function(ep) c("  onecmt:", "    status: draft", paste0("    reference_path: ", cref),
                        paste0("    entrypoints: [", ep, "]"), paste0("    sha256: ", sha(cref)),
                        "    evidence: SKILL.md", '    reason: "template"')
mfc <- write_manifest(manifest_lines(skill = "ctl", units = u_ctl("onecmt.ctl")), skill = "ctl")
ok(!inherits(tryCatch(validate_method_manifest(mfc), error = function(e) e), "error"),
   "G: a non-R reference whose single entry point is its own file name validates")
write_manifest(manifest_lines(skill = "ctl", units = u_ctl("run_model")), skill = "ctl")
expect_stop(validate_method_manifest(mfc), "G: a non-R reference with any other entry point is refused", "file name")

# ==== H. the writer emits depends and treats it as signed content ==========
dw <- mk_skill("writer")
spec <- list(u = list(reference_path = "skills/writer/reference/method.R", entrypoints = "f",
                      status = "approved", evidence = "SKILL.md", reason = "x",
                      depends = "tools/record_selection.R"))
mfw <- write_method_manifest(dw, spec)
wl <- readLines(mfw)
ok(any(grepl("^    depends:$", wl)) && any(grepl("^      - path: tools/record_selection.R$", wl)) &&
     any(grepl(paste0("^        sha256: ", sha("tools/record_selection.R"), "$"), wl)),
   "H: write_method_manifest emits the depends block with the file digest")
ok(!inherits(tryCatch(validate_method_manifest(mfw), error = function(e) e), "error"),
   "H: the written manifest validates")
t <- readLines(mfw); t[grep("^reviewed_by:", t)] <- 'reviewed_by: "Fixture Reviewer"'
t[grep("^reviewed_at:", t)] <- 'reviewed_at: "2026-09-01"'; writeLines(t, mfw)
suppressMessages(write_method_manifest(dw, spec))
ok(identical(read_method_manifest(mfw)$reviewed_by, "Fixture Reviewer"),
   "H: byte-identical regeneration with depends preserves the root signature")
spec2 <- spec; spec2$u$depends <- character(0)
suppressMessages(write_method_manifest(dw, spec2))
ok(nchar(read_method_manifest(mfw)$reviewed_by) == 0,
   "H: dropping a dependency clears the root signature")

# ==== J. gate-chain certification (round 9, P0-1) ===========================
# A binding used to become a "gate" if `require_validated_method` appeared
# anywhere in its syntax tree, so a wrapper whose only mention of it was dead
# code (`if (FALSE) require_validated_method(...)`) certified an entry point
# that never validated anything. The gate set is now derived by following the
# call chain from the entry point's first statement to a terminus that must be
# `require_validated_method` with a unit argument equal to the unit's name.
# Integrity batch 13: a bare `require_validated_method(...)` call resolves
# through the environment the reference is sourced into and is refused by the
# gate-chain head rule, so every fixture dispatches off the sealed validator
# object; mkgate() prepends the construction unless the fixture builds its own.
MM_FIXTURE_SEALED <- c(
  ".mm_trusted_private <- base::local({ e <- base::new.env(parent = base::baseenv())",
  "  base::sys.source('tools/method_manifest.R', envir = e, keep.source = FALSE)",
  "  base::lockEnvironment(e, bindings = TRUE); e }, envir = base::new.env(parent = base::baseenv()))")
mkgate <- function(name, body_lines, ep_lines, unit = "alpha") {
  d <- file.path("skills", name)
  dir.create(file.path(d, "reference"), recursive = TRUE, showWarnings = FALSE)
  p <- file.path(d, "reference", "method.R")
  if (!any(grepl("\\.mm_trusted_private <-|\\.priv <-", c(body_lines, ep_lines))) &&
      any(grepl("\\.mm_trusted_private", c(body_lines, ep_lines))))
    body_lines <- c(MM_FIXTURE_SEALED, body_lines)
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
# An approved-but-ungated entry point is a hard stop, so the verdict is read
# either from the returned frame (when the audit passes) or from the refusal
# message (when it does not).
gated_of <- function(d) {
  r <- tryCatch(suppressWarnings(audit_entrypoint_gating(d)),
                error = function(e) conditionMessage(e))
  if (is.character(r)) list(gated = FALSE, reason = r)
  else list(gated = isTRUE(r$gated[1]), reason = r$reason[1])
}

# The house pattern .x_gate() -> x_validated(...) -> require_validated_method()
d <- mkgate("gchain_house", c(
  'alpha_validated <- function() {',
  '  base::get("require_validated_method", envir = .mm_trusted_private)(skill_dir = "skills/gchain_house",',
  '                          calling_file = "x", unit = "alpha")',
  '}',
  '.alpha_gate <- function() {',
  '  if (!alpha_validated()) base::stop("no")',
  '  base::invisible(TRUE)',
  '}'),
  c('ep <- function(x) { .alpha_gate(); x }'))
r <- gated_of(d)
ok(r$gated, "J: house pattern .gate() -> validated() -> require_validated_method is accepted")

# Dead-code wrapper: the gate name is present but never executed.
d <- mkgate("gchain_dead", c(
  '.alpha_gate <- function() {',
  '  if (FALSE) base::get("require_validated_method", envir = .mm_trusted_private)(skill_dir = "s", calling_file = "x", unit = "alpha")',
  '  invisible(TRUE)',
  '}'),
  c('ep <- function(x) { .alpha_gate(); x }'))
r <- gated_of(d)
ok(!r$gated && grepl("chain", r$reason),
   "J: dead-code wrapper (if (FALSE) require_validated_method) is refused")

# A wrapper may do work before validating -- the lazy source() of
# tools/method_manifest.R, and eda-standards' source-identity and
# dependency-digest checks, all run first -- so a preceding statement is
# stepped over. What must hold is that the validator is reached
# unconditionally: the walk never enters an `if`/`for`/`while` body or a
# nested function literal, and it refuses any stepped-over statement that
# could return early.
d <- mkgate("gchain_prologue", c(
  '.side <- function() invisible(TRUE)',
  '.alpha_gate <- function() {',
  '  .side()',
  '  base::get("require_validated_method", envir = .mm_trusted_private)(skill_dir = "s", calling_file = "x", unit = "alpha")',
  '}'),
  c('ep <- function(x) { .alpha_gate(); x }'))
r <- gated_of(d)
ok(r$gated, "J: a wrapper prologue before an unconditional gate call is accepted")

# The same shape with an early exit is a bypass, not a prologue.
d <- mkgate("gchain_early", c(
  '.alpha_gate <- function() {',
  '  if (base::Sys.getenv("SKIP") == "1") return(base::invisible(TRUE))',
  '  base::get("require_validated_method", envir = .mm_trusted_private)(skill_dir = "s", calling_file = "x", unit = "alpha")',
  '}'),
  c('ep <- function(x) { .alpha_gate(); x }'))
r <- gated_of(d)
ok(!r$gated && grepl("return", r$reason),
   "J: a wrapper that can return before the gate call is refused")

# A wrapper that never reaches a gate call at all.
d <- mkgate("gchain_none", c(
  '.alpha_gate <- function() base::invisible(TRUE)'),
  c('ep <- function(x) { .alpha_gate(); x }'))
r <- gated_of(d)
ok(!r$gated && grepl("invisible", r$reason),
   "J: a wrapper that reaches no gate call is refused (the chain names its terminus)")

# Non-literal unit: the terminus is reached but the unit cannot be read.
d <- mkgate("gchain_dyn", c(
  '.alpha_gate <- function() {',
  '  u <- get("alpha_name")',
  '  base::get("require_validated_method", envir = .mm_trusted_private)(skill_dir = "s", calling_file = "x", unit = u)',
  '}'),
  c('ep <- function(x) { .alpha_gate(); x }'))
r <- gated_of(d)
ok(!r$gated, "J: non-literal unit argument is refused")

# Missing unit argument.
d <- mkgate("gchain_nounit", c(
  '.alpha_gate <- function() {',
  '  base::get("require_validated_method", envir = .mm_trusted_private)(skill_dir = "s", calling_file = "x")',
  '}'),
  c('ep <- function(x) { .alpha_gate(); x }'))
r <- gated_of(d)
ok(!r$gated, "J: absent unit argument is refused")

# Wrong unit literal: another unit's approval must not authorize this one.
d <- mkgate("gchain_wrong", c(
  '.alpha_gate <- function() {',
  '  base::get("require_validated_method", envir = .mm_trusted_private)(skill_dir = "s", calling_file = "x", unit = "beta")',
  '}'),
  c('ep <- function(x) { .alpha_gate(); x }'))
r <- gated_of(d)
ok(!r$gated && grepl("beta", r$reason),
   "J: a gate naming another unit is refused")

# A cycle terminates with a refusal rather than recursing forever.
d <- mkgate("gchain_cycle", c(
  '.a <- function() .b()',
  '.b <- function() .a()'),
  c('ep <- function(x) { .a(); x }'))
r <- gated_of(d)
ok(!r$gated && grepl("cycle", r$reason), "J: a cyclic gate chain is refused")

# Literals propagate through a forwarding wrapper (the psn-* house pattern:
# .gate("vpc") -> validated(unit) -> require_validated_method(unit = unit)).
d <- mkgate("gchain_fwd", c(
  'alpha_validated <- function(unit) {',
  '  base::get("require_validated_method", envir = .mm_trusted_private)(skill_dir = "s", calling_file = "x", unit = unit)',
  '}',
  '.alpha_gate <- function(unit) {',
  '  if (!alpha_validated(unit)) base::stop("no")',
  '}'),
  c('ep <- function(x) { .alpha_gate("alpha"); x }'))
r <- gated_of(d)
ok(r$gated, "J: a literal unit forwarded through a wrapper's formal is accepted")

# The lazy-loader preamble (`if (!exists(...)) source(...)`) that every gate
# carried before batch 11 is REFUSED since integrity batch 13: `exists` and
# `source` are bare executable names inside a gate-chain function, and the
# sealed construction replaced the preamble in every shipped file.
d <- mkgate("gchain_lazy", c(
  '.alpha_gate <- function() {',
  '  if (!exists("require_validated_method", mode = "function"))',
  '    source(file.path("tools", "method_manifest.R"))',
  '  if (!base::get("require_validated_method", envir = .mm_trusted_private)(skill_dir = "s", calling_file = "x", unit = "alpha"))',
  '    base::stop("no")',
  '}'),
  c('ep <- function(x) { .alpha_gate(); x }'))
r <- gated_of(d)
ok(!r$gated && grepl("`exists`", r$reason, fixed = TRUE),
   "J: the pre-batch-11 lazy source() preamble before the gate call is refused (bare `exists`/`source` in a gate body; batch 13)")

# --- J: validator shadowing (round 9 fix round, review B1) -----------------
# The step-over rule let a stepped-over statement REBIND the validator, or any
# function in the credited chain, to a no-op. The audit then credited the next
# call as the gate while R's lexical scoping resolved it to the local no-op at
# runtime. These are the reviewer's fixtures AA, BB, CC, DD and FF, verbatim.
d <- mkgate("gshadow_AA", c(
  'require_validated_method <- function(...) TRUE',
  '.g <- function() base::get("require_validated_method", envir = .mm_trusted_private)(unit = "alpha")'),
  c('ep <- function(x) { .g(); x }'))
r <- gated_of(d)
ok(!r$gated && grepl("binds `require_validated_method`", r$reason),
   "J/B1 AA: a reference file that binds require_validated_method at top level is refused")

d <- mkgate("gshadow_BB", c(
  '.noop <- function(...) TRUE',
  '.g <- function() { require_validated_method <- .noop',
  '                   base::get("require_validated_method", envir = .mm_trusted_private)(unit = "alpha") }'),
  c('ep <- function(x) { .g(); x }'))
r <- gated_of(d)
ok(!r$gated && grepl("binds `require_validated_method`", r$reason),
   "J/B1 BB: a local alias rebinding the validator inside a wrapper is refused")

d <- mkgate("gshadow_CC", c(
  '.g <- function() { require_validated_method <- function(...) base::invisible(TRUE)',
  '                   base::get("require_validated_method", envir = .mm_trusted_private)(unit = "alpha") }'),
  c('ep <- function(x) { .g(); x }'))
r <- gated_of(d)
ok(!r$gated && grepl("binds `require_validated_method`", r$reason),
   "J/B1 CC: a validator shadow declared inside a function body is refused")

d <- mkgate("gshadow_DD", c(
  '.alpha_gate <- function() { alpha_validated <- function(u) TRUE',
  '                            alpha_validated("alpha") }',
  'alpha_validated <- function(unit) base::get("require_validated_method", envir = .mm_trusted_private)(unit = unit)'),
  c('ep <- function(x) { .alpha_gate(); x }'))
r <- gated_of(d)
ok(!r$gated && grepl("rebound", r$reason),
   "J/B1 DD: rebinding a MID-CHAIN function (not the validator) is refused")

d <- mkgate("gshadow_FF", c(
  '.g <- function() { .h() }',
  '.h <- function() { require_validated_method <- function(...) TRUE',
  '                   base::get("require_validated_method", envir = .mm_trusted_private)(unit = "alpha") }'),
  c('ep <- function(x) { .g(); x }'))
r <- gated_of(d)
ok(!r$gated && grepl("binds `require_validated_method`", r$reason),
   "J/B1 FF: a two-level chain whose inner function shadows the validator is refused")

# The same shadow shapes reached through assign() and through a formal.
d <- mkgate("gshadow_assign", c(
  '.g <- function() { assign("require_validated_method", function(...) TRUE)',
  '                   base::get("require_validated_method", envir = .mm_trusted_private)(unit = "alpha") }'),
  c('ep <- function(x) { .g(); x }'))
r <- gated_of(d)
ok(!r$gated, "J/B1: assign()-ing the validator name is refused")

d <- mkgate("gshadow_formal", c(
  'alpha_validated <- function(unit) base::get("require_validated_method", envir = .mm_trusted_private)(unit = unit)',
  '.alpha_gate <- function(alpha_validated = function(u) TRUE) alpha_validated("alpha")'),
  c('ep <- function(x) { .alpha_gate(); x }'))
r <- gated_of(d)
ok(!r$gated && grepl("rebound", r$reason),
   "J/B1: a formal shadowing a chain member is refused")

# Dynamic rebinding a static name scan cannot see: a stepped-over statement
# may not call assign/eval/source/environment<- and friends.
d <- mkgate("gshadow_eval", c(
  '.g <- function() { eval(parse(text = "require_validated_method <- function(...) TRUE"))',
  '                   base::get("require_validated_method", envir = .mm_trusted_private)(unit = "alpha") }'),
  c('ep <- function(x) { .g(); x }'))
r <- gated_of(d)
ok(!r$gated, "J/B1: eval(parse(...)) before the gate call is refused")

d <- mkgate("gshadow_source", c(
  '.g <- function() { base::source("other.R")',
  '                   base::get("require_validated_method", envir = .mm_trusted_private)(skill_dir = "s", calling_file = "x", unit = "alpha") }'),
  c('ep <- function(x) { .g(); x }'))
r <- gated_of(d)
ok(!r$gated && grepl("rebind", r$reason),
   "J/B1: a source() call stepped over before the gate is refused")

# The runtime demonstration the reviewer used: with the real validator in
# scope, the shadowed wrapper never runs it. Pinned so the audit's verdict and
# the runtime behaviour are asserted together.
shadow_env <- new.env(parent = globalenv())
assign("require_validated_method", function(...) base::stop("REAL VALIDATOR RAN"),
       envir = shadow_env)
eval(quote(.g <- function() { require_validated_method <- function(...) TRUE
                              base::get("require_validated_method", envir = .mm_trusted_private)(unit = "alpha") }),
     envir = shadow_env)
ran <- tryCatch({ eval(quote(.g()), envir = shadow_env); FALSE },
                error = function(e) grepl("REAL VALIDATOR RAN", conditionMessage(e)))
ok(isFALSE(ran),
   "J/B1: runtime — a locally shadowed wrapper does NOT run the real validator (the audit must refuse it)")

# --- J: dispatch and indirection residues (fix round 2, re-review) --------
# The bound-name census closed BINDING the validator and REBINDING a chain
# member. It did not close DISPATCHING through a locally constructed object,
# nor `assign` reached through `do.call`, nor a namespace-qualified terminus.
# All three were demonstrated at runtime by the reviewer (R1/R2/R5 returned
# without the real validator running while the audit reported the unit gated).
d <- mkgate("gdisp_R1", c(
  '.g <- function() { p <- list(require_validated_method = function(...) TRUE)',
  '                   base::get("require_validated_method", envir = p)(unit = "alpha") }'),
  c('ep <- function(x) { .g(); x }'))
r <- gated_of(d)
ok(!r$gated, "J/R1: $ dispatch off a locally built list naming the validator is refused")

# R2 and R4 stayed refused across integrity batch 11, but the refusal moved
# EARLIER and got sharper: the binding census now recognises the indirect
# assignment forms (base::assign, do.call("assign", ...)), so the whole file is
# refused for binding the validator's name rather than one wrapper being
# refused for rebinding before its gate. Both wordings are accepted here.
d <- mkgate("gdisp_R2", c(
  '.g <- function() { do.call("assign", list("require_validated_method", function(...) TRUE))',
  '                   base::get("require_validated_method", envir = .mm_trusted_private)(skill_dir = "s", calling_file = "x", unit = "alpha") }'),
  c('ep <- function(x) { .g(); x }'))
r <- gated_of(d)
ok(!r$gated && grepl("rebind|binds `require_validated_method`", r$reason),
   "J/R2: do.call(\"assign\", ...) stepped over before the gate is refused")

d <- mkgate("gdisp_R3", c(
  '.g <- function() { do.call("eval", list(quote(require_validated_method <- function(...) TRUE)))',
  '                   base::get("require_validated_method", envir = .mm_trusted_private)(skill_dir = "s", calling_file = "x", unit = "alpha") }'),
  c('ep <- function(x) { .g(); x }'))
r <- gated_of(d)
ok(!r$gated, "J/R3: do.call(\"eval\", ...) stepped over before the gate is refused")

d <- mkgate("gdisp_R4", c(
  '.g <- function() { Reduce(function(a, b) do.call("assign", list("require_validated_method", function(...) TRUE)), 1)',
  '                   base::get("require_validated_method", envir = .mm_trusted_private)(skill_dir = "s", calling_file = "x", unit = "alpha") }'),
  c('ep <- function(x) { .g(); x }'))
r <- gated_of(d)
ok(!r$gated && grepl("rebind|binds `require_validated_method`", r$reason),
   "J/R4: an apply/Reduce call hiding assign behind a string is refused")

d <- mkgate("gdisp_R5", c(
  '.g <- function() { e <- new.env(); e[["require_validated_method"]] <- function(...) TRUE',
  '                   base::get("require_validated_method", envir = e)(unit = "alpha") }'),
  c('ep <- function(x) { .g(); x }'))
r <- gated_of(d)
ok(!r$gated, "J/R5: [[ ]]-assigning the validator into an environment then dispatching is refused")

d <- mkgate("gdisp_R6", c(
  '.mk <- function() structure(list(require_validated_method = function(...) TRUE))',
  '.g <- function() { p <- .mk(); base::get("require_validated_method", envir = p)(unit = "alpha") }'),
  c('ep <- function(x) { .g(); x }'))
r <- gated_of(d)
ok(!r$gated, "J/R6: structure(list(...)) built by a helper then $ dispatched is refused")

d <- mkgate("gdisp_R7", c(
  '.g <- function() stats::require_validated_method(unit = "alpha")'),
  c('ep <- function(x) { .g(); x }'))
r <- gated_of(d)
ok(!r$gated && grepl("namespace|::", r$reason),
   "J/R7: a ::-qualified terminus is refused (no package provides the harness validator)")

d <- mkgate("gdisp_R7b", c(
  'alpha_validated <- function() base::get("require_validated_method", envir = .mm_trusted_private)(skill_dir = "s", calling_file = "x", unit = "alpha")',
  '.alpha_gate <- function() somepkg:::alpha_validated()'),
  c('ep <- function(x) { .alpha_gate(); x }'))
r <- gated_of(d)
ok(!r$gated, "J/R7b: a :::-qualified call to a chain member's name is refused")

# Round 2 asserted here that a `$` dispatch off a bare `local({ new.env() })`
# passed, on the reasoning that the element name was bound nowhere in the file.
# Fix round 3 supersedes that: an object BUILT in the file is never a trusted
# dispatch base however its elements got there, and only the shipped
# sys.source + lockEnvironment construction is (see the J/H5 assertions).
d <- mkgate("gdisp_private", c(
  '.priv <- local({ p <- new.env(); p })',
  '.alpha_gate <- function() {',
  '  if (!base::get("require_validated_method", envir = .priv)(skill_dir = "s", calling_file = "x", unit = "alpha"))',
  '    base::stop("no")',
  '}'),
  c('ep <- function(x) { .alpha_gate(); x }'))
r <- gated_of(d)
ok(!r$gated && grepl("trusted", r$reason),
   "J/R1-6: a $ dispatch off a bare new.env() built in the file is refused")

# --- J: $ dispatch is allowlisted, not enumerated (fix round 3) ------------
# Two rounds of extending the bound-name census closed twelve shapes and
# invited six more (setNames, structure(names=), names<-, a variable [[k]] <-,
# the functional `[[<-`, attr(,"names")<-), each demonstrated at runtime. R has
# more routes for putting a name on an object than a list can hold, so the
# census stopped being the discriminator. What separates the shipped pattern
# from every attack is not HOW the name got there but WHERE THE OBJECT CAME
# FROM: a `$` dispatch to the validator is credited only when its base traces
# to a top-level binding that sys.source()s method_manifest.R into a locked
# new.env(). Everything else is refused, whatever route the name took.
f_dispatch <- list(
  "F1 setNames(list(f), \"rvm\") then $" = c(
    '.g <- function() { p <- setNames(list(function(...) TRUE), "require_validated_method")',
    '                   base::get("require_validated_method", envir = p)(unit = "alpha") }'),
  "F2 structure(list(f), names = \"rvm\") then $" = c(
    '.g <- function() { p <- structure(list(function(...) TRUE), names = "require_validated_method")',
    '                   base::get("require_validated_method", envir = p)(unit = "alpha") }'),
  "F3 names(p) <- \"rvm\" then $" = c(
    '.g <- function() { p <- list(function(...) TRUE); names(p) <- "require_validated_method"',
    '                   base::get("require_validated_method", envir = p)(unit = "alpha") }'),
  "F5 a variable [[k]] <- then $ dispatch" = c(
    '.g <- function() { k <- "require_validated_method"; p <- list(); p[[k]] <- function(...) TRUE',
    '                   base::get("require_validated_method", envir = p)(unit = "alpha") }'),
  "F7 `[[<-`(list(), \"rvm\", f) functional form" = c(
    '.g <- function() { p <- `[[<-`(list(), "require_validated_method", function(...) TRUE)',
    '                   base::get("require_validated_method", envir = p)(unit = "alpha") }'),
  "F8 attr(p, \"names\") <- \"rvm\"" = c(
    '.g <- function() { p <- list(function(...) TRUE); attr(p, "names") <- "require_validated_method"',
    '                   base::get("require_validated_method", envir = p)(unit = "alpha") }'))
for (nm in names(f_dispatch)) {
  d <- mkgate(paste0("gf_", gsub("[^A-Za-z0-9]", "", substr(nm, 1, 2))),
              f_dispatch[[nm]], c('ep <- function(x) { .g(); x }'))
  r <- gated_of(d)
  ok(!r$gated, sprintf("J/%s is refused", nm))
}

# Two shapes that were already refused, pinned so the new rule cannot loosen
# them: a `[[` dispatch whose key is a variable, and an env built by a helper.
d <- mkgate("gf_F4", c(
  '.g <- function() { k <- "require_validated_method"; p <- list(); p[[k]] <- function(...) TRUE',
  '                   p[[k]](unit = "alpha") }'),
  c('ep <- function(x) { .g(); x }'))
ok(!gated_of(d)$gated, "J/F4 a [[ ]] dispatch keyed by a variable is refused")
d <- mkgate("gf_F10", c(
  '.mk <- function() { e <- new.env(); assign("require_validated_method", function(...) TRUE, envir = e); e }',
  '.g <- function() { e <- .mk(); base::get("require_validated_method", envir = e)(unit = "alpha") }'),
  c('ep <- function(x) { .g(); x }'))
ok(!gated_of(d)$gated, "J/F10 an environment returned by a helper is not a trusted dispatch base")

# THE shape that must pass: the shipped construction (eda.R:113-125) — a
# top-level binding whose new.env() is filled by sys.source()ing
# method_manifest.R and then locked.
d <- mkgate("gf_H5", c(
  '.priv <- base::local({ e <- base::new.env(parent = base::baseenv())',
  '  base::sys.source(base::file.path("tools", "method_manifest.R"), envir = e)',
  '  base::lockEnvironment(e, bindings = TRUE); e }, envir = base::new.env(parent = base::baseenv()))',
  '.g <- function() base::get("require_validated_method", envir = .priv)(skill_dir = "s", calling_file = "f", unit = "alpha")'),
  c('ep <- function(x) { .g(); x }'))
ok(gated_of(d)$gated,
   "J/H5 the shipped sys.source + lockEnvironment construction is a trusted dispatch base")

# ... and the same shape reached through one alias, which is exactly how
# .eda_gate holds it (`private <- .eda_private` inside the gate's local()).
d <- mkgate("gf_H5alias", c(
  '.priv <- base::local({ e <- base::new.env(parent = base::baseenv())',
  '  base::sys.source(base::file.path("tools", "method_manifest.R"), envir = e)',
  '  base::lockEnvironment(e, bindings = TRUE); e }, envir = base::new.env(parent = base::baseenv()))',
  '.g <- base::local({ private <- .priv; function(unit) {',
  '  if (!base::get("require_validated_method", envir = private)(skill_dir = "s", calling_file = "f", unit = unit))',
  '    base::stop("no"); base::invisible(TRUE) } }, envir = base::new.env())'),
  c('ep <- function(x) { .g("alpha"); x }'))
ok(gated_of(d)$gated,
   "J/H5 the alias form (private <- .priv inside the gate's local) is still trusted")

# Mutating the trusted object anywhere in the file withdraws the trust.
d <- mkgate("gf_H5mut", c(
  '.priv <- base::local({ e <- base::new.env(parent = base::baseenv())',
  '  base::sys.source(base::file.path("tools", "method_manifest.R"), envir = e)',
  '  base::lockEnvironment(e, bindings = TRUE); e }, envir = base::new.env(parent = base::baseenv()))',
  '.tamper <- function() .priv$require_validated_method <- function(...) TRUE',
  '.g <- function() base::get("require_validated_method", envir = .priv)(skill_dir = "s", calling_file = "f", unit = "alpha")'),
  c('ep <- function(x) { .g(); x }'))
ok(!gated_of(d)$gated,
   "J/H5 a trusted base that is re-assigned anywhere in the file is refused")

# Dropping either half of the construction drops the trust.
for (variant in list(
      list(nm = "no lockEnvironment", body = c(
        '.priv <- base::local({ e <- base::new.env(parent = base::baseenv())',
        '  base::sys.source(file.path("tools", "method_manifest.R"), envir = e); e }, envir = base::new.env(parent = base::baseenv()))')),
      list(nm = "no sys.source of method_manifest.R", body = c(
        '.priv <- base::local({ e <- base::new.env(parent = base::baseenv())',
        '  base::sys.source("evil.R", envir = e)',
        '  base::lockEnvironment(e, bindings = TRUE); e }, envir = base::new.env(parent = base::baseenv()))')))) {
  d <- mkgate(paste0("gf_H5x", gsub("[^A-Za-z0-9]", "", variant$nm)),
              c(variant$body,
                '.g <- function() base::get("require_validated_method", envir = .priv)(skill_dir = "s", calling_file = "f", unit = "alpha")'),
              c('ep <- function(x) { .g(); x }'))
  ok(!gated_of(d)$gated,
     sprintf("J/H5 the construction with %s is NOT trusted", variant$nm))
}

# --- J: the trust predicate binds its evidence to the returned value -------
# Fix round 4. `.mm_is_trusted_private_rhs()` was a PRESENCE scan: it walked the
# whole expression for `new.env`, `sys.source`, `lockEnvironment` and the path
# literal, so nothing tied the three calls to each other or to the value the
# block actually returns. A decoy could load the real thing into a discarded
# environment, or park the whole construction in a dead `if (FALSE)` branch,
# and return a fake. The sharpest decoy is this batch's own founding defect --
# a construction inside `if (FALSE)` credited as real -- recurring in the
# predicate that decides whether an OBJECT is trustworthy.
g_decoys <- list(
  "G11 the real load is discarded and a fake env returned" = c(
    '.priv <- base::local({',
    '  d <- base::new.env(parent = base::baseenv()); base::sys.source("tools/method_manifest.R", envir = d)',
    '  base::lockEnvironment(d, bindings = TRUE)',
    '  as.environment(setNames(list(function(...) TRUE), "require_validated_method")) }, envir = base::new.env(parent = base::baseenv()))'),
  "G12 a trusted-looking block that returns a list" = c(
    '.priv <- base::local({',
    '  d <- base::new.env(parent = base::baseenv()); base::sys.source("tools/method_manifest.R", envir = d)',
    '  base::lockEnvironment(d, bindings = TRUE)',
    '  structure(list(function(...) TRUE), names = "require_validated_method") }, envir = base::new.env(parent = base::baseenv()))'),
  "G13 the whole construction inside if (FALSE)" = c(
    '.priv <- local({ if (FALSE) { e <- new.env()',
    '    base::sys.source("tools/method_manifest.R", envir = e); base::lockEnvironment(e, bindings = TRUE) }',
    '  setNames(list(function(...) TRUE), "require_validated_method") })'))
for (nm in names(g_decoys)) {
  d <- mkgate(paste0("gg_", gsub("[^A-Za-z0-9]", "", substr(nm, 1, 3))),
              c(g_decoys[[nm]],
                '.g <- function() base::get("require_validated_method", envir = .priv)(unit = "alpha")'),
              c('ep <- function(x) { .g(); x }'))
  r <- gated_of(d)
  ok(!r$gated && grepl("trusted", r$reason), sprintf("J/%s is refused", nm))
}

# Re-assigning the environment at the block's top level AFTER the lock is a
# decoy too; before the lock (as eda.R does with e$dependency_sha256) is not.
d <- mkgate("gg_afterlock", c(
  '.priv <- base::local({ e <- base::new.env(parent = base::baseenv())',
  '  base::sys.source("tools/method_manifest.R", envir = e)',
  '  base::lockEnvironment(e, bindings = TRUE)',
  '  e <- setNames(list(function(...) TRUE), "require_validated_method")',
  '  e }, envir = base::new.env(parent = base::baseenv()))',
  '.g <- function() base::get("require_validated_method", envir = .priv)(unit = "alpha")'),
  c('ep <- function(x) { .g(); x }'))
ok(!gated_of(d)$gated,
   "J/G a block that re-assigns the environment after the lock is refused")

d <- mkgate("gg_beforelock", c(
  '.priv <- base::local({ e <- base::new.env(parent = base::baseenv())',
  '  base::sys.source("tools/method_manifest.R", envir = e)',
  '  e$dependency_sha256 <- "sha256:00"',
  '  base::lockEnvironment(e, bindings = TRUE)',
  '  e }, envir = base::new.env(parent = base::baseenv()))',
  '.g <- function() base::get("require_validated_method", envir = .priv)(skill_dir = "s", calling_file = "f", unit = "alpha")'),
  c('ep <- function(x) { .g(); x }'))
ok(gated_of(d)$gated,
   "J/G writing into the environment BEFORE the lock is still trusted (the eda.R shape)")

# Positive controls that must survive the tightening.
d <- mkgate("gg_G14", c(
  '.priv <- base::local({ base::local({ e <- base::new.env(parent = base::baseenv())',
  '  base::sys.source("tools/method_manifest.R", envir = e); base::lockEnvironment(e, bindings = TRUE); e }, envir = base::new.env(parent = base::baseenv())) }, envir = base::new.env(parent = base::baseenv()))',
  '.g <- function() base::get("require_validated_method", envir = .priv)(skill_dir = "s", calling_file = "f", unit = "alpha")'),
  c('ep <- function(x) { .g(); x }'))
ok(gated_of(d)$gated, "J/G14 a nested local() wrapper around the construction still passes")

# The full shipped shape, transcribed from skills/eda-standards/reference/eda.R
# :113-126 -- the sys.source sits inside a for loop and its path is the loop
# variable, so the rule must not demand a top-level or literal-path sys.source.
# (Integrity batch 12, decision 2: the loop and the pre-seal write now live in
# the SECOND, dependency environment; the validator loads only its own file.)
d <- mkgate("gg_edashape", c(
  'DEPS <- base::c("tools/record_selection.R")',
  '.priv <- base::local({',
  '  e <- base::new.env(parent = base::baseenv())',
  '  base::sys.source("tools/method_manifest.R", envir = e)',
  '  base::lockEnvironment(e, bindings = TRUE)',
  '  e }, envir = base::new.env(parent = base::baseenv()))',
  '.deps <- base::local({',
  '  d <- base::new.env(parent = base::baseenv())',
  '  for (f in DEPS) {',
  '    if (!base::file.exists(f)) base::stop("missing")',
  '    base::sys.source(f, envir = d, keep.source = FALSE)',
  '  }',
  '  d$dependency_sha256 <- "sha256:00"',
  '  base::lockEnvironment(d, bindings = TRUE)',
  '  d',
  '}, envir = base::new.env(parent = base::baseenv()))',
  '.g <- base::local({ private <- .priv; function(unit) {',
  '  if (!base::get("require_validated_method", envir = private)(skill_dir = "s", calling_file = "f", unit = unit))',
  '    base::stop("no"); base::invisible(TRUE) } }, envir = base::new.env())'),
  c('ep <- function(x) { .g("alpha"); x }'))
ok(gated_of(d)$gated,
   "J/G the full eda.R shape (for-loop sys.source, non-literal path, alias hop) passes")

# Re-sourcing INTO the trusted object after it is sealed always failed closed
# at run time (R stops with "cannot add bindings to a locked environment"), and
# until fix round 4 of batch 11 the audit let it through. It is now refused
# statically as well: a top-level sys.source() is one of the reflective forms
# MM_REFLECTIVE_TOP_LEVEL names, so the file no longer reaches run time. Pinned
# in the strict direction.
d <- mkgate("gg_G9", c(
  '.priv <- base::local({ e <- base::new.env(parent = base::baseenv())',
  '  base::sys.source("tools/method_manifest.R", envir = e)',
  '  base::lockEnvironment(e, bindings = TRUE); e }, envir = base::new.env(parent = base::baseenv()))',
  'base::sys.source("tools/evil.R", envir = .priv)',
  '.g <- function() base::get("require_validated_method", envir = .priv)(skill_dir = "s", calling_file = "f", unit = "alpha")'),
  c('ep <- function(x) { .g(); x }'))
ok(!gated_of(d)$gated,
   "J/G9 re-sourcing into the sealed object is refused at the top level (fix round 4) and still fails closed at run time")

# --- J: clause (4) ties the path to its call and enumerates EVERY load -----
# Integrity batch 10, from the batch-9 review's Final check 3 residual. Clause
# (4) used to be two independent presence checks: SOME sys.source targets the
# returned environment, and SOME literal in the block names
# tools/method_manifest.R. Nothing tied the literal to that call, and nothing
# constrained any OTHER file loaded into the object before it was sealed. So a
# reference file could exhibit the literal as a decoy and load an accomplice
# file instead, or load the real validator and then overwrite it.
#
# The rule now: every source()/sys.source() targeting the returned environment
# must have a statically resolvable path; at least one must resolve to
# tools/method_manifest.R; and every OTHER resolved path must come from a
# top-level literal vector of harness-relative paths (the eda.R dependency
# shape), never from a bare literal written inside the block.
k_decoys <- list(
  "K6 the literal is a decoy and another file is what actually loads" = c(
    '.priv <- base::local({',
    '  e <- base::new.env(parent = base::baseenv())',
    '  DOC <- "tools/method_manifest.R"',
    '  base::sys.source("tools/evil.R", envir = e)',
    '  base::lockEnvironment(e, bindings = TRUE)',
    '  e }, envir = base::new.env(parent = base::baseenv()))'),
  "K7 the qualifying load sits in a dead branch and another file loads for real" = c(
    '.priv <- base::local({',
    '  e <- base::new.env(parent = base::baseenv())',
    '  if (FALSE) base::sys.source("tools/method_manifest.R", envir = e)',
    '  base::sys.source("tools/evil.R", envir = e)',
    '  base::lockEnvironment(e, bindings = TRUE)',
    '  e }, envir = base::new.env(parent = base::baseenv()))'),
  "K10 a qualifying load followed by a second, unrelated load into the same object" = c(
    '.priv <- base::local({',
    '  e <- base::new.env(parent = base::baseenv())',
    '  base::sys.source("tools/method_manifest.R", envir = e)',
    '  base::sys.source("tools/evil.R", envir = e)',
    '  base::lockEnvironment(e, bindings = TRUE)',
    '  e }, envir = base::new.env(parent = base::baseenv()))'),
  "K11 the same, with base source(local = ) rather than sys.source" = c(
    '.priv <- base::local({',
    '  e <- base::new.env(parent = base::baseenv())',
    '  base::sys.source("tools/method_manifest.R", envir = e)',
    '  source("tools/evil.R", local = e)',
    '  base::lockEnvironment(e, bindings = TRUE)',
    '  e }, envir = base::new.env(parent = base::baseenv()))'),
  "K12 a load whose path cannot be resolved statically" = c(
    '.priv <- base::local({',
    '  e <- base::new.env(parent = base::baseenv())',
    '  base::sys.source("tools/method_manifest.R", envir = e)',
    '  base::sys.source(paste0("tools/", nm, ".R"), envir = e)',
    '  base::lockEnvironment(e, bindings = TRUE)',
    '  e }, envir = base::new.env(parent = base::baseenv()))'))
for (nm in names(k_decoys)) {
  d <- mkgate(paste0("gk_", gsub("[^A-Za-z0-9]", "", substr(nm, 1, 3))),
              c(k_decoys[[nm]],
                '.g <- function() base::get("require_validated_method", envir = .priv)(skill_dir = "s", calling_file = "f", unit = "alpha")'),
              c('ep <- function(x) { .g(); x }'))
  r <- gated_of(d)
  ok(!r$gated && grepl("trusted", r$reason), sprintf("J/%s is refused", nm))
}

# Positive control: a second load IS allowed when its path comes from a
# top-level literal vector of harness-relative paths -- exactly what eda.R's
# EDA_DEPENDENCY_FILES is. Written here without the for loop so the direct
# one-level constant resolution is pinned on its own.
d <- mkgate("gk_toplevelconst", c(
  'DEPS <- "tools/record_selection.R"',
  '.priv <- base::local({',
  '  e <- base::new.env(parent = base::baseenv())',
  '  base::sys.source("tools/method_manifest.R", envir = e)',
  '  base::lockEnvironment(e, bindings = TRUE)',
  '  e }, envir = base::new.env(parent = base::baseenv()))',
  '.deps <- base::local({',
  '  d <- base::new.env(parent = base::baseenv())',
  '  base::sys.source(DEPS, envir = d)',
  '  base::lockEnvironment(d, bindings = TRUE)',
  '  d }, envir = base::new.env(parent = base::baseenv()))',
  '.g <- function() base::get("require_validated_method", envir = .priv)(skill_dir = "s", calling_file = "f", unit = "alpha")'),
  c('ep <- function(x) { .g(); x }'))
ok(gated_of(d)$gated,
   "J/K a second load resolved through a top-level literal constant is still trusted")

# ... and it is NOT allowed when the top-level constant names an absolute path
# or escapes the repository, since neither is a harness-relative dependency.
for (bad in c('"/etc/evil.R"', '"../../evil.R"')) {
  d <- mkgate(paste0("gk_badconst", gsub("[^A-Za-z0-9]", "", bad)), c(
    paste0('DEPS <- ', bad),
  '.priv <- base::local({',
  '  e <- base::new.env(parent = base::baseenv())',
  '  base::sys.source("tools/method_manifest.R", envir = e)',
  '  base::lockEnvironment(e, bindings = TRUE)',
  '  e }, envir = base::new.env(parent = base::baseenv()))',
    '.deps <- base::local({',
    '  d <- base::new.env(parent = base::baseenv())',
    '  base::sys.source(DEPS, envir = d)',
    '  base::lockEnvironment(d, bindings = TRUE)',
    '  d }, envir = base::new.env(parent = base::baseenv()))',
    '.g <- function() base::get("require_validated_method", envir = .priv)(skill_dir = "s", calling_file = "f", unit = "alpha")'),
    c('ep <- function(x) { .g(); x }'))
  ok(!gated_of(d)$gated,
     sprintf("J/K a top-level constant naming %s is not a harness-relative dependency", bad))
}

# The audit REPORTS what it resolved, so the SME sees at approval exactly which
# files were loaded into the object that holds the gate. Past this point the
# control is not the audit -- it is the reference file's manifest digest and the
# SME's review of the diff (batch-9 review, Final check 3 section 3).
d <- mkgate("gk_report", c(
  'DEPS <- base::c("tools/record_selection.R")',
  '.priv <- base::local({',
  '  e <- base::new.env(parent = base::baseenv())',
  '  base::sys.source("tools/method_manifest.R", envir = e)',
  '  base::lockEnvironment(e, bindings = TRUE)',
  '  e }, envir = base::new.env(parent = base::baseenv()))',
  '.deps <- base::local({',
  '  d <- base::new.env(parent = base::baseenv())',
  '  for (f in DEPS) base::sys.source(f, envir = d)',
  '  base::lockEnvironment(d, bindings = TRUE)',
  '  d }, envir = base::new.env(parent = base::baseenv()))',
  '.g <- function() base::get("require_validated_method", envir = .priv)(skill_dir = "s", calling_file = "f", unit = "alpha")'),
  c('ep <- function(x) { .g(); x }'))
r <- suppressWarnings(audit_entrypoint_gating(d))
ok("trusted_loads" %in% names(r),
   "J/K the audit frame carries a trusted_loads column")
ok(grepl("tools/method_manifest.R", r$trusted_loads[1], fixed = TRUE) &&
     grepl("tools/record_selection.R", r$trusted_loads[1], fixed = TRUE),
   "J/K the audit reports every file resolved into the trusted object")

# --- J: batch-10 fix round, from the independent review --------------------
# The first cut of the enumeration had four holes. Each fixture below is the
# reviewer's, and each was demonstrated at run time with the real validator in
# scope as stop("REAL VALIDATOR RAN") -- i.e. they were not theoretical: no
# validation happened and the entry point returned.
#
# B1 is the sharpest, and it is worse than the boundary the batch-9 review
# accepted. Every earlier bypass needed an accomplice file INSIDE the
# repository, which the write guard, the manifest digests and the SME's diff
# review all see. A qualifying path that is absolute or climbs out of the tree
# needs no accomplice in the repository at all: the reference file is the whole
# attack, and the loaded "validator" is outside every one of those controls.
r_decoys <- list(
  "B1/R6 the qualifying load is an absolute path outside the repository" = c(
    '.priv <- base::local({',
    '  e <- base::new.env(parent = base::baseenv())',
    '  base::sys.source("/tmp/attacker/tools/method_manifest.R", envir = e)',
    '  base::lockEnvironment(e, bindings = TRUE)',
    '  e }, envir = base::new.env(parent = base::baseenv()))'),
  "B1/R7 the qualifying load climbs out of the repository" = c(
    '.priv <- base::local({',
    '  e <- base::new.env(parent = base::baseenv())',
    '  base::sys.source("../../../attacker/tools/method_manifest.R", envir = e)',
    '  base::lockEnvironment(e, bindings = TRUE)',
    '  e }, envir = base::new.env(parent = base::baseenv()))'),
  "B2/R5 a namespace-qualified load is enumerated, not invisible" = c(
    '.priv <- base::local({',
    '  e <- base::new.env(parent = base::baseenv())',
    '  base::sys.source("tools/method_manifest.R", envir = e)',
    '  base::sys.source("tools/evil.R", envir = e)',
    '  base::lockEnvironment(e, bindings = TRUE)',
    '  e }, envir = base::new.env(parent = base::baseenv()))'),
  "B2/R5b a `base:::`-qualified load is a refusal even when it is the ONLY load (batch 12: `base::` is the one admitted spelling)" = c(
    '.priv <- base::local({',
    '  e <- base::new.env(parent = base::baseenv())',
    '  base:::sys.source("tools/method_manifest.R", envir = e)',
    '  base::lockEnvironment(e, bindings = TRUE)',
    '  e }, envir = base::new.env(parent = base::baseenv()))'),
  "S1/R8b a load hidden in a for loop's SEQUENCE expression" = c(
    '.priv <- base::local({',
    '  e <- base::new.env(parent = base::baseenv())',
    '  base::sys.source("tools/method_manifest.R", envir = e)',
    '  for (i in seq_len({ base::sys.source("tools/evil.R", envir = e); 1L })) NULL',
    '  base::lockEnvironment(e, bindings = TRUE)',
    '  e }, envir = base::new.env(parent = base::baseenv()))'),
  "S2/R3 a second environment copied in wholesale with list2env" = c(
    '.priv <- base::local({',
    '  e <- base::new.env(parent = base::baseenv())',
    '  base::sys.source("tools/method_manifest.R", envir = e)',
    '  d <- base::new.env(parent = base::baseenv())',
    '  base::sys.source("tools/evil.R", envir = d)',
    '  list2env(as.list(d), envir = e)',
    '  base::lockEnvironment(e, bindings = TRUE)',
    '  e }, envir = base::new.env(parent = base::baseenv()))'),
  "S2 assign() into the object" = c(
    '.priv <- base::local({',
    '  e <- base::new.env(parent = base::baseenv())',
    '  base::sys.source("tools/method_manifest.R", envir = e)',
    '  assign("helper", function(...) TRUE, envir = e)',
    '  base::lockEnvironment(e, bindings = TRUE)',
    '  e }, envir = base::new.env(parent = base::baseenv()))'),
  "S2 eval() into the object" = c(
    '.priv <- base::local({',
    '  e <- base::new.env(parent = base::baseenv())',
    '  base::sys.source("tools/method_manifest.R", envir = e)',
    '  eval(quote(helper <- function(...) TRUE), envir = e)',
    '  base::lockEnvironment(e, bindings = TRUE)',
    '  e }, envir = base::new.env(parent = base::baseenv()))'),
  "S2 local() evaluated in the object" = c(
    '.priv <- base::local({',
    '  e <- base::new.env(parent = base::baseenv())',
    '  base::sys.source("tools/method_manifest.R", envir = e)',
    '  local(helper <- function(...) TRUE, envir = e)',
    '  base::lockEnvironment(e, bindings = TRUE)',
    '  e }, envir = base::new.env(parent = base::baseenv()))'),
  "S2 a load reaching the object through a variable alias" = c(
    '.priv <- base::local({',
    '  e <- base::new.env(parent = base::baseenv())',
    '  base::sys.source("tools/method_manifest.R", envir = e)',
    '  alias <- e',
    '  base::sys.source("tools/evil.R", envir = alias)',
    '  base::lockEnvironment(e, bindings = TRUE)',
    '  e }, envir = base::new.env(parent = base::baseenv()))'),
  "S2 environment<- reparenting the object" = c(
    '.priv <- base::local({',
    '  e <- base::new.env(parent = base::baseenv())',
    '  base::sys.source("tools/method_manifest.R", envir = e)',
    '  environment(e) <- globalenv()',
    '  base::lockEnvironment(e, bindings = TRUE)',
    '  e }, envir = base::new.env(parent = base::baseenv()))'),
  "S2 a function value written into the object before the lock" = c(
    '.priv <- base::local({',
    '  e <- base::new.env(parent = base::baseenv())',
    '  base::sys.source("tools/method_manifest.R", envir = e)',
    '  e$helper <- function(...) TRUE',
    '  base::lockEnvironment(e, bindings = TRUE)',
    '  e }, envir = base::new.env(parent = base::baseenv()))'))
for (nm in names(r_decoys)) {
  d <- mkgate(paste0("gr_", gsub("[^A-Za-z0-9]", "", substr(nm, 1, 8))),
              c(r_decoys[[nm]],
                '.g <- function() base::get("require_validated_method", envir = .priv)(skill_dir = "s", calling_file = "f", unit = "alpha")'),
              c('ep <- function(x) { .g(); x }'))
  r <- gated_of(d)
  ok(!r$gated && grepl("trusted", r$reason), sprintf("J/%s is refused", nm))
}

# Positive controls that must survive the tightening. The eda.R shape writes a
# NON-function value into the object before the lock (a character vector of
# dependency digests) and that stays legitimate -- it is the whole reason
# mutation before the seal is allowed at all.
d <- mkgate("gr_edanonfn", c(
  'DEPS <- base::c("tools/record_selection.R")',
  '.priv <- base::local({',
  '  e <- base::new.env(parent = base::baseenv())',
  '  base::sys.source("tools/method_manifest.R", envir = e)',
  '  base::lockEnvironment(e, bindings = TRUE)',
  '  e }, envir = base::new.env(parent = base::baseenv()))',
  '.deps <- base::local({',
  '  d <- base::new.env(parent = base::baseenv())',
  '  for (f in DEPS) {',
  '    if (!base::file.exists(f)) base::stop("missing")',
  '    base::sys.source(f, envir = d, keep.source = FALSE)',
  '  }',
  '  d$dependency_sha256 <- base::vapply(DEPS, function(f) base::paste0("sha256:", f), base::character(1))',
  '  base::lockEnvironment(d, bindings = TRUE)',
  '  d',
  '}, envir = base::new.env(parent = base::baseenv()))',
  '.g <- function() base::get("require_validated_method", envir = .priv)(skill_dir = "s", calling_file = "f", unit = "alpha")'),
  c('ep <- function(x) { .g(); x }'))
ok(gated_of(d)$gated,
   "J/S2 the real eda.R shape (non-function write before the lock) still passes")

# The fixtures above deliberately write a NEUTRAL name, so that only the
# deny-by-default rule can refuse them. Writing the validator's own name is
# refused too -- by the older rule that a reference may call the validator but
# never define, alias or shadow it. Both routes are pinned so that neither can
# quietly stop covering this.
d <- mkgate("gr_validatorname", c(
  '.priv <- base::local({',
  '  e <- base::new.env(parent = base::baseenv())',
  '  base::sys.source("tools/method_manifest.R", envir = e)',
  '  e$require_validated_method <- function(...) TRUE',
  '  base::lockEnvironment(e, bindings = TRUE)',
  '  e }, envir = base::new.env(parent = base::baseenv()))',
  '.g <- function() base::get("require_validated_method", envir = .priv)(skill_dir = "s", calling_file = "f", unit = "alpha")'),
  c('ep <- function(x) { .g(); x }'))
ok(!gated_of(d)$gated,
   "J/S2 writing the validator's own name into the object is refused")

# --- J: fix round 2, position 4 of the whitelist ---------------------------
# The re-review found position 4 written more permissively than the eda.R write
# it exists to permit. Both of its guards were defeated by the same one
# character: `e$name <-` became `e[[nm]] <-`, and the code then read the INDEX
# VARIABLE'S SPELLING ("nm") instead of its value, so the validator's own name
# could be written under it. W4/W8/W9 all defeated the real validator at run
# time. Position 4 is now exactly the eda.R shape: a literal binding name, a
# non-function value, before the seal, and never the validator or any name this
# file already binds.
w_decoys <- list(
  "B3/W4 a second environment copied in one binding at a time through e[[nm]]" = c(
    '.priv <- base::local({',
    '  e <- base::new.env(parent = base::baseenv())',
    '  base::sys.source("tools/method_manifest.R", envir = e)',
    '  d <- base::new.env(parent = base::baseenv())',
    '  base::sys.source("tools/dep.R", envir = d)',
    '  for (nm in names(d)) e[[nm]] <- d[[nm]]',
    '  base::lockEnvironment(e, bindings = TRUE)',
    '  e }, envir = base::new.env(parent = base::baseenv()))'),
  "B3/W8 the validator overwritten through a computed index" = c(
    '.priv <- base::local({',
    '  e <- base::new.env(parent = base::baseenv())',
    '  base::sys.source("tools/method_manifest.R", envir = e)',
    '  nm <- "require_validated_method"',
    '  e[[nm]] <- (function(...) TRUE)',
    '  base::lockEnvironment(e, bindings = TRUE)',
    '  e }, envir = base::new.env(parent = base::baseenv()))'),
  "B3/W9 the same with no syntactic function literal anywhere" = c(
    '.priv <- base::local({',
    '  e <- base::new.env(parent = base::baseenv())',
    '  base::sys.source("tools/method_manifest.R", envir = e)',
    '  nm  <- "require_validated_method"',
    '  src <- list(x = function(...) TRUE)',
    '  e[[nm]] <- src$x',
    '  base::lockEnvironment(e, bindings = TRUE)',
    '  e }, envir = base::new.env(parent = base::baseenv()))'),
  "B4/W7 a PARENTHESISED function literal is still a function literal" = c(
    '.priv <- base::local({',
    '  e <- base::new.env(parent = base::baseenv())',
    '  base::sys.source("tools/method_manifest.R", envir = e)',
    '  e$helper <- (function(...) TRUE)',
    '  base::lockEnvironment(e, bindings = TRUE)',
    '  e }, envir = base::new.env(parent = base::baseenv()))'),
  "B4 the backslash lambda shorthand is a function literal too" = c(
    '.priv <- base::local({',
    '  e <- base::new.env(parent = base::baseenv())',
    '  base::sys.source("tools/method_manifest.R", envir = e)',
    '  e$helper <- \\(...) TRUE',
    '  base::lockEnvironment(e, bindings = TRUE)',
    '  e }, envir = base::new.env(parent = base::baseenv()))'),
  "B3 a value lifted out of another object by extraction" = c(
    '.priv <- base::local({',
    '  e <- base::new.env(parent = base::baseenv())',
    '  base::sys.source("tools/method_manifest.R", envir = e)',
    '  src <- list(x = function(...) TRUE)',
    '  e$helper <- src$x',
    '  base::lockEnvironment(e, bindings = TRUE)',
    '  e }, envir = base::new.env(parent = base::baseenv()))'),
  "B3 a write under a name this file already binds" = c(
    'helper <- function() TRUE',
    '.priv <- base::local({',
    '  e <- base::new.env(parent = base::baseenv())',
    '  base::sys.source("tools/method_manifest.R", envir = e)',
    '  e$helper <- "x"',
    '  base::lockEnvironment(e, bindings = TRUE)',
    '  e }, envir = base::new.env(parent = base::baseenv()))'))
for (nm in names(w_decoys)) {
  d <- mkgate(paste0("gw_", gsub("[^A-Za-z0-9]", "", substr(nm, 1, 8))),
              c(w_decoys[[nm]],
                '.g <- function() base::get("require_validated_method", envir = .priv)(skill_dir = "s", calling_file = "f", unit = "alpha")'),
              c('ep <- function(x) { .g(); x }'))
  r <- gated_of(d)
  ok(!r$gated, sprintf("J/%s is refused", nm))
}

# Positive control, transcribed from the REAL eda.R:121-123 rather than
# simplified to a string literal. Its value is a vapply() call, so the rule
# cannot refuse call-valued writes outright without refusing the one shipped
# use of this construction in the repository.
d <- mkgate("gw_edareal", c(
  'DEPS <- base::c("tools/record_selection.R")',
  '.priv <- base::local({',
  '  e <- base::new.env(parent = base::baseenv())',
  '  base::sys.source("tools/method_manifest.R", envir = e)',
  '  base::lockEnvironment(e, bindings = TRUE)',
  '  e }, envir = base::new.env(parent = base::baseenv()))',
  '.deps <- base::local({',
  '  d <- base::new.env(parent = base::baseenv())',
  '  for (f in DEPS) {',
  '    if (!base::file.exists(f)) base::stop("missing")',
  '    base::sys.source(f, envir = d, keep.source = FALSE)',
  '  }',
  '  d$dependency_sha256 <- base::vapply(DEPS, function(f)',
  '    base::paste0("sha256:", digest::digest(file = f, algo = "sha256")), base::character(1))',
  '  base::lockEnvironment(d, bindings = TRUE)',
  '  d',
  '}, envir = base::new.env(parent = base::baseenv()))',
  '.g <- function() base::get("require_validated_method", envir = .priv)(skill_dir = "s", calling_file = "f", unit = "alpha")'),
  c('ep <- function(x) { .g(); x }'))
ok(gated_of(d)$gated,
   "J/B3 the REAL eda.R position-4 write (a vapply call value) still passes")

# A [[ ]] index that IS a character literal is the same thing as $ and stays
# allowed; only a computed index is refused.
d <- mkgate("gw_litindex", c(
  '.priv <- base::local({',
  '  e <- base::new.env(parent = base::baseenv())',
  '  base::sys.source("tools/method_manifest.R", envir = e)',
  '  e[["dependency_sha256"]] <- "sha256:00"',
  '  base::lockEnvironment(e, bindings = TRUE)',
  '  e }, envir = base::new.env(parent = base::baseenv()))',
  '.g <- function() base::get("require_validated_method", envir = .priv)(skill_dir = "s", calling_file = "f", unit = "alpha")'),
  c('ep <- function(x) { .g(); x }'))
ok(gated_of(d)$gated,
   "J/B3 a [[ ]] write with a character-literal name is still trusted")

# --- J: fix round 3, B5 — position 4 versus the LOADED file's own bindings --
# The final check found the sharpest shape yet, and it defeats the backstop
# rather than the audit. `e` is filled by sys.source("tools/method_manifest.R",
# envir = e), so it holds all 66 top-level objects of that file, and the
# validator's closure IS `e` — so every helper it calls resolves there:
#
#   require_validated_method() -> validate_method_manifest() -> .mm_file_sha256()
#
# None of those names is the validator's own and none is a top-level binding of
# the REFERENCE file, so the round-2 name guards did not cover them. One line
# rewrote the digest check from inside the reference, turning the digest-drift
# hard stop into TRUE. That stop is exactly what the batch-9 review designated
# as the control past the audit's reach, so this had to be closed here.
g_shadow <- list(
  "B5/G2 the digest helper the validator calls is overwritten" = c(
    '.priv <- base::local({',
    '  e <- base::new.env(parent = base::baseenv())',
    '  base::sys.source("tools/method_manifest.R", envir = e)',
    '  e$.mm_file_sha256 <- (function() function(...) "x")()',
    '  base::lockEnvironment(e, bindings = TRUE)',
    '  e }, envir = base::new.env(parent = base::baseenv()))'),
  "B5/G1 the manifest validator itself is overwritten" = c(
    '.priv <- base::local({',
    '  e <- base::new.env(parent = base::baseenv())',
    '  base::sys.source("tools/method_manifest.R", envir = e)',
    '  e$validate_method_manifest <- (function() function(...) TRUE)()',
    '  base::lockEnvironment(e, bindings = TRUE)',
    '  e }, envir = base::new.env(parent = base::baseenv()))'),
  "B5/G2-rt the same helper, closing over the real one" = c(
    '.priv <- base::local({',
    '  e <- base::new.env(parent = base::baseenv())',
    '  base::sys.source("tools/method_manifest.R", envir = e)',
    '  real <- "sha256:00"',
    '  e$.mm_file_sha256 <- (function(h) function(...) h)(real)',
    '  base::lockEnvironment(e, bindings = TRUE)',
    '  e }, envir = base::new.env(parent = base::baseenv()))'),
  "B5/G3 a CONSTANT of the loaded file is overwritten" = c(
    '.priv <- base::local({',
    '  e <- base::new.env(parent = base::baseenv())',
    '  base::sys.source("tools/method_manifest.R", envir = e)',
    '  e$MM_VALIDATOR_NAME <- "nope"',
    '  base::lockEnvironment(e, bindings = TRUE)',
    '  e }, envir = base::new.env(parent = base::baseenv()))'),
  "B5 a binding of a DEPENDENCY load is overwritten" = c(
    'DEPS <- base::c("tools/record_selection.R")',
    '.priv <- base::local({',
    '  e <- base::new.env(parent = base::baseenv())',
    '  for (f in c("tools/method_manifest.R", DEPS)) base::sys.source(f, envir = e)',
    '  e$keep_rows <- "x"',
    '  base::lockEnvironment(e, bindings = TRUE)',
    '  e }, envir = base::new.env(parent = base::baseenv()))'))
for (nm in names(g_shadow)) {
  d <- mkgate(paste0("gs_", gsub("[^A-Za-z0-9]", "", substr(nm, 1, 8))),
              c(g_shadow[[nm]],
                '.g <- function() base::get("require_validated_method", envir = .priv)(skill_dir = "s", calling_file = "f", unit = "alpha")'),
              c('ep <- function(x) { .g(); x }'))
  ok(!gated_of(d)$gated, sprintf("J/%s is refused", nm))
}

# --- J: fix round 4 — the sealed object's LOOKUP CHAIN ---------------------
# Final check 2. B5 asked "is this name defined in a file I loaded?" The
# question the run time asks is "is this name reachable from the sealed
# object's lookup chain?" Those differ by the whole of baseenv(), and `e`'s
# parent IS baseenv(). require_validated_method() expresses EVERY one of its
# refusals as a call to stop(), so shadowing stop() turns them all into
# no-ops -- and `stop` is not a binding of any loaded file, so the round-3
# guard did not see it.
#
# The invariant, stated once: NOTHING MAY ENTER THE SEALED OBJECT'S LOOKUP
# CHAIN THAT THE AUDIT HAS NOT RESOLVED AND JUDGED. That chain has exactly
# three contributors -- (a) what the loads put in, (b) what a pre-seal write
# puts in, (c) the parent -- and each is judged: (a) by the load resolution,
# (b) by position 4, (c) by requiring parent = baseenv().
chain_decoys <- list(
  "B6/I3-stop a pre-seal write shadows stop(), which every refusal calls" = c(
    '.priv <- base::local({',
    '  e <- base::new.env(parent = base::baseenv())',
    '  base::sys.source("tools/method_manifest.R", envir = e)',
    '  e$stop <- (function() function(...) invisible(NULL))()',
    '  base::lockEnvironment(e, bindings = TRUE)',
    '  e }, envir = base::new.env(parent = base::baseenv()))'),
  "B6 a pre-seal write shadows another base function" = c(
    '.priv <- base::local({',
    '  e <- base::new.env(parent = base::baseenv())',
    '  base::sys.source("tools/method_manifest.R", envir = e)',
    '  e$identical <- "x"',
    '  base::lockEnvironment(e, bindings = TRUE)',
    '  e }, envir = base::new.env(parent = base::baseenv()))'),
  "B7/J1 the object is parented on globalenv() rather than baseenv()" = c(
    '.priv <- base::local({',
    '  e <- new.env(parent = globalenv())',
    '  base::sys.source("tools/method_manifest.R", envir = e)',
    '  base::lockEnvironment(e, bindings = TRUE)',
    '  e }, envir = base::new.env(parent = base::baseenv()))'),
  "B7/J2 the object is built with a bare new.env()" = c(
    '.priv <- local({',
    '  e <- new.env()',
    '  base::sys.source("tools/method_manifest.R", envir = e)',
    '  base::lockEnvironment(e, bindings = TRUE)',
    '  e })'),
  "B7/J3 a bare new.env() plus a top-level stop() in the reference file" = c(
    'stop <- function(...) invisible(NULL)',
    '.priv <- local({',
    '  e <- new.env()',
    '  base::sys.source("tools/method_manifest.R", envir = e)',
    '  base::lockEnvironment(e, bindings = TRUE)',
    '  e })'),
  "B7 an emptyenv() parent is not baseenv() either" = c(
    '.priv <- base::local({',
    '  e <- new.env(parent = emptyenv())',
    '  base::sys.source("tools/method_manifest.R", envir = e)',
    '  base::lockEnvironment(e, bindings = TRUE)',
    '  e }, envir = base::new.env(parent = base::baseenv()))'),
  "B7 the reference file shadows a primitive the construction itself calls" = c(
    'sys.source <- function(...) invisible(NULL)',
    '.priv <- base::local({',
    '  e <- base::new.env(parent = base::baseenv())',
    '  base::sys.source("tools/method_manifest.R", envir = e)',
    '  base::lockEnvironment(e, bindings = TRUE)',
    '  e }, envir = base::new.env(parent = base::baseenv()))'),
  "B7 the reference file shadows lockEnvironment, so the seal is a no-op" = c(
    'lockEnvironment <- function(...) invisible(NULL)',
    '.priv <- base::local({',
    '  e <- base::new.env(parent = base::baseenv())',
    '  base::sys.source("tools/method_manifest.R", envir = e)',
    '  base::lockEnvironment(e, bindings = TRUE)',
    '  e }, envir = base::new.env(parent = base::baseenv()))'))
for (nm in names(chain_decoys)) {
  d <- mkgate(paste0("gc_", gsub("[^A-Za-z0-9]", "", substr(nm, 1, 10))),
              c(chain_decoys[[nm]],
                '.g <- function() base::get("require_validated_method", envir = .priv)(skill_dir = "s", calling_file = "f", unit = "alpha")'),
              c('ep <- function(x) { .g(); x }'))
  ok(!gated_of(d)$gated, sprintf("J/%s is refused", nm))
}

# The reviewer's I1 control: a FORMAL of the validator is bound in the call
# frame, which shadows `e`, so writing that name is unreachable and stays
# allowed. Refusing it would be over-tightening.
d <- mkgate("gc_formal", c(
  '.priv <- base::local({',
  '  e <- base::new.env(parent = base::baseenv())',
  '  base::sys.source("tools/method_manifest.R", envir = e)',
  '  e$calling_file <- "anything"',
  '  base::lockEnvironment(e, bindings = TRUE)',
  '  e }, envir = base::new.env(parent = base::baseenv()))',
  '.g <- function() base::get("require_validated_method", envir = .priv)(skill_dir = "s", calling_file = "f", unit = "alpha")'),
  c('ep <- function(x) { .g(); x }'))
ok(gated_of(d)$gated,
   "J/B6 writing a FORMAL of the validator is still trusted (the call frame shadows e)")

# eda.R binds `%||%` at top level and so does tools/method_manifest.R. A
# BLANKET "the reference file may not bind any name a load binds" rule would
# refuse the one shipped construction in the repository, so the rule is scoped
# to the primitives the construction itself calls. This pins that eda.R's own
# top-level %||% stays acceptable.
d <- mkgate("gc_pctpct", c(
  '`%||%` <- function(a, b) if (is.null(a)) b else a',
  '.priv <- base::local({',
  '  e <- base::new.env(parent = base::baseenv())',
  '  base::sys.source("tools/method_manifest.R", envir = e)',
  '  base::lockEnvironment(e, bindings = TRUE)',
  '  e }, envir = base::new.env(parent = base::baseenv()))',
  '.g <- function() base::get("require_validated_method", envir = .priv)(skill_dir = "s", calling_file = "f", unit = "alpha")'),
  c('ep <- function(x) { .g(); x }'))
ok(gated_of(d)$gated,
   "J/B7 a top-level %||% (which method_manifest.R also binds) is still trusted")

# --- J: fix round 5 — the ENCLOSING layers of the construction -------------
# Final check 3. The round-4 primitive-shadow rule is applied to the reference
# file's top level, but .mm_trusted_block_statements() unwraps a block whose
# value is another block and judges only the INNERMOST one -- so every
# statement of an enclosing layer was discarded before any rule saw it. An
# enclosing local() is on the construction's lookup chain exactly as the file's
# top level is, so the same redirect the round-4 rule refuses at top level
# passes when moved one layer out, and `trusted_loads` then REPORTS
# tools/method_manifest.R for a construction that loads tools/evil.R. A report
# that is confidently wrong is worse than no report.
encl_decoys <- list(
  "B8/K2-lock an enclosing layer shadows lockEnvironment, so the seal is a no-op" = c(
    '.priv <- base::local({',
    '  lockEnvironment <- function(...) base::invisible(NULL)',
    '  local({',
    '    e <- base::new.env(parent = base::baseenv())',
    '    base::sys.source("tools/method_manifest.R", envir = e)',
    '    base::lockEnvironment(e, bindings = TRUE)',
    '    e }, envir = base::new.env(parent = base::baseenv()))',
    '}, envir = base::new.env())'),
  "B8/K2-sys.source an enclosing layer shadows sys.source, so the load is a no-op" = c(
    '.priv <- base::local({',
    '  sys.source <- function(...) base::invisible(NULL)',
    '  local({',
    '    e <- base::new.env(parent = base::baseenv())',
    '    base::sys.source("tools/method_manifest.R", envir = e)',
    '    base::lockEnvironment(e, bindings = TRUE)',
    '    e }, envir = base::new.env(parent = base::baseenv()))',
    '}, envir = base::new.env())'),
  "B8/K2-newenv an enclosing layer shadows new.env, so the parent is not base" = c(
    '.priv <- base::local({',
    '  new.env <- function(...) globalenv()',
    '  local({',
    '    e <- base::new.env(parent = base::baseenv())',
    '    base::sys.source("tools/method_manifest.R", envir = e)',
    '    base::lockEnvironment(e, bindings = TRUE)',
    '    e }, envir = base::new.env(parent = base::baseenv()))',
    '}, envir = base::new.env())'),
  "B8/K2-stop an enclosing layer binds stop" = c(
    '.priv <- base::local({',
    '  stop <- function(...) base::invisible(NULL)',
    '  local({',
    '    e <- base::new.env(parent = base::baseenv())',
    '    base::sys.source("tools/method_manifest.R", envir = e)',
    '    base::lockEnvironment(e, bindings = TRUE)',
    '    e }, envir = base::new.env(parent = base::baseenv()))',
    '}, envir = base::new.env())'),
  "B8/K5-redirect an enclosing sys.source redirects the load, and trusted_loads misreports it" = c(
    '.priv <- base::local({',
    '  sys.source <- function(file, envir, ...) base::sys.source("tools/evil.R", envir)',
    '  local({',
    '    e <- base::new.env(parent = base::baseenv())',
    '    base::sys.source("tools/method_manifest.R", envir = e)',
    '    base::lockEnvironment(e, bindings = TRUE)',
    '    e }, envir = base::new.env(parent = base::baseenv()))',
    '}, envir = base::new.env())'),
  "B8 an enclosing layer that binds anything at all" = c(
    '.priv <- base::local({',
    '  harmless <- 42',
    '  local({',
    '    e <- base::new.env(parent = base::baseenv())',
    '    base::sys.source("tools/method_manifest.R", envir = e)',
    '    base::lockEnvironment(e, bindings = TRUE)',
    '    e }, envir = base::new.env(parent = base::baseenv()))',
    '})'),
  "K1 library() at top level of a file that builds a trusted object" = c(
    'library(stats)',
    '.priv <- base::local({',
    '  e <- base::new.env(parent = base::baseenv())',
    '  base::sys.source("tools/method_manifest.R", envir = e)',
    '  base::lockEnvironment(e, bindings = TRUE)',
    '  e }, envir = base::new.env(parent = base::baseenv()))'),
  "K1 require() likewise" = c(
    'require(stats)',
    '.priv <- base::local({',
    '  e <- base::new.env(parent = base::baseenv())',
    '  base::sys.source("tools/method_manifest.R", envir = e)',
    '  base::lockEnvironment(e, bindings = TRUE)',
    '  e }, envir = base::new.env(parent = base::baseenv()))'))
for (nm in names(encl_decoys)) {
  d <- mkgate(paste0("ge_", gsub("[^A-Za-z0-9]", "", substr(nm, 1, 11))),
              c(encl_decoys[[nm]],
                '.g <- function() base::get("require_validated_method", envir = .priv)(skill_dir = "s", calling_file = "f", unit = "alpha")'),
              c('ep <- function(x) { .g(); x }'))
  ok(!gated_of(d)$gated, sprintf("J/%s is refused", nm))
}

# L1: a DEPENDENCY load that defines the validator shadows the real one, since
# a load is sourced INTO e and so the loaded file's top level IS e. One
# condition over the load bindings the audit already computes; no legitimate
# dependency can want that name. The QUALIFYING load must stay exempt --
# tools/method_manifest.R binds require_validated_method, which is the point.
writeLines(c("require_validated_method <- function(...) TRUE"), "tools/dep_evil.R")
d <- mkgate("ge_L1dep", c(
  'DEPS <- base::c("tools/dep_evil.R")',
  '.priv <- base::local({',
  '  e <- base::new.env(parent = base::baseenv())',
  '  for (f in c("tools/method_manifest.R", DEPS)) base::sys.source(f, envir = e)',
  '  base::lockEnvironment(e, bindings = TRUE)',
  '  e }, envir = base::new.env(parent = base::baseenv()))',
  '.g <- function() base::get("require_validated_method", envir = .priv)(skill_dir = "s", calling_file = "f", unit = "alpha")'),
  c('ep <- function(x) { .g(); x }'))
ok(!gated_of(d)$gated,
   "J/L1 a dependency load that binds require_validated_method is refused")

# Positive control: the real eda.R shape has NO enclosing layer and no
# library() call, so none of the above touches it.
d <- mkgate("ge_edaencl", c(
  'DEPS <- base::c("tools/record_selection.R")',
  '.priv <- base::local({',
  '  e <- base::new.env(parent = base::baseenv())',
  '  base::sys.source("tools/method_manifest.R", envir = e)',
  '  base::lockEnvironment(e, bindings = TRUE)',
  '  e }, envir = base::new.env(parent = base::baseenv()))',
  '.deps <- base::local({',
  '  d <- base::new.env(parent = base::baseenv())',
  '  for (f in DEPS) {',
  '    if (!base::file.exists(f)) base::stop("missing")',
  '    base::sys.source(f, envir = d, keep.source = FALSE)',
  '  }',
  '  d$dependency_sha256 <- base::vapply(DEPS, function(f) base::paste0("sha256:", f), base::character(1))',
  '  base::lockEnvironment(d, bindings = TRUE)',
  '  d',
  '}, envir = base::new.env(parent = base::baseenv()))',
  '.g <- function() base::get("require_validated_method", envir = .priv)(skill_dir = "s", calling_file = "f", unit = "alpha")'),
  c('ep <- function(x) { .g(); x }'))
ok(gated_of(d)$gated,
   "J/B8 the real eda.R shape (no enclosing layer, no library()) still passes")

# An entry point is not gated by calling another entry point (the pre-round-9
# audit excluded entry points from the gate set; test_batch7.R pins it too).
d <- mkgate("gchain_viaep", c(
  '.alpha_gate <- function() base::get("require_validated_method", envir = .mm_trusted_private)(skill_dir = "s", calling_file = "x", unit = "alpha")',
  'ep <- function(x) { .alpha_gate(); x }'),
  c('ep2 <- function(x) ep(x)'))
writeLines(sub("entrypoints: \\[ep\\]", "entrypoints: [ep, ep2]",
               readLines("skills/gchain_viaep/MANIFEST.yaml")),
           "skills/gchain_viaep/MANIFEST.yaml")
r <- tryCatch(suppressWarnings(audit_entrypoint_gating("skills/gchain_viaep")),
              error = function(e) conditionMessage(e))
ok(is.character(r) && grepl("ep2", r) && grepl("manifested entry point", r),
   "J: an entry point that merely calls another entry point is refused")

# --- J: the attack surface that is already refused, pinned -----------------
# Twenty-two shapes the reviewer confirmed are correctly refused. Pinning them
# means the B1 hardening cannot be traded for a regression here.
gate_ok <- c('alpha_validated <- function() base::get("require_validated_method", envir = .mm_trusted_private)(',
             '  skill_dir = "s", calling_file = "x", unit = "alpha")')
attacks <- list(
  "tryCatch around the gate"        = 'ep <- function(x) { tryCatch(alpha_validated()); x }',
  "try() around the gate"           = 'ep <- function(x) { try(alpha_validated(), silent = TRUE); x }',
  "suppressWarnings around the gate"= 'ep <- function(x) { suppressWarnings(alpha_validated()); x }',
  "base::invisible() around the gate"     = 'ep <- function(x) { base::invisible(alpha_validated()); x }',
  "switch() dispatch to the gate"   = 'ep <- function(x) { switch(x, alpha_validated()); x }',
  "local({}) wrapping the gate"     = 'ep <- function(x) { local({ alpha_validated() }); x }',
  "on.exit() deferring the gate"    = 'ep <- function(x) { on.exit(alpha_validated()); x }',
  "do.call the validator"           = 'ep <- function(x) { do.call(require_validated_method, list(unit = "alpha")); x }',
  "repeat{} around the gate"        = 'ep <- function(x) { repeat { alpha_validated() }; x }',
  "while(FALSE) the gate"           = 'ep <- function(x) { while (FALSE) alpha_validated(); x }',
  "nested function literal holds it"= 'ep <- function(x) { f <- function() alpha_validated(); x }',
  "&& short-circuit"                = 'ep <- function(x) { TRUE && alpha_validated(); x }',
  "%||% chain"                      = 'ep <- function(x) { NULL %||% alpha_validated(); x }',
  "if (FALSE) the gate"             = 'ep <- function(x) { if (FALSE) alpha_validated(); x }',
  "early return before the gate"    = 'ep <- function(x) { return(x); alpha_validated() }')
refused <- character(0)
for (nm in names(attacks)) {
  d <- mkgate(paste0("gatk_", gsub("[^A-Za-z0-9]", "", nm)), gate_ok, attacks[[nm]])
  if (!isTRUE(gated_of(d)$gated)) refused <- c(refused, nm)
}
ok(setequal(refused, names(attacks)),
   sprintf("J: all %d entry-point evasion shapes are refused (%d refused)",
           length(attacks), length(refused)))

# Accepted shapes that must keep working.
d <- mkgate("gacc_positional", c(
  sprintf('.alpha_gate <- function() base::get("require_validated_method", envir = .mm_trusted_private)("%s", "%s", "alpha")',
          "skills/gacc_positional", "skills/gacc_positional/reference/method.R")),
  c('ep <- function(x) { .alpha_gate(); x }'))
ok(gated_of(d)$gated,
   "J: the positional base::get(\"require_validated_method\", envir = .mm_trusted_private)(skill_dir, calling_file, unit) form is accepted")

# ==== I. every real manifest passes the strict rules ========================
setwd(REPO)
real <- list.files("skills", "^MANIFEST\\.yaml$", recursive = TRUE, full.names = TRUE)
bad <- character(0)
for (m in real) {
  e <- tryCatch({ validate_method_manifest(m); NULL }, error = function(e) conditionMessage(e))
  if (!is.null(e)) bad <- c(bad, paste0(basename(dirname(m)), ": ", e))
}
if (length(bad)) cat(paste0("      ", bad, collapse = "\n"), "\n")
# Coverage is derived, not pinned: every skill whose reference/ directory
# holds at least one file must carry a manifest (integrity batch 8 added
# skills/data-assembly, the tenth; a hard-coded 9 would fail every time a
# competency gains a reference). The strict-rule validation above covers the
# content of each one.
with_refs <- Filter(function(d) length(list.files(file.path(d, "reference"))) > 0,
                    list.dirs("skills", recursive = FALSE))
ok(setequal(dirname(real), with_refs) && length(bad) == 0L,
   sprintf("I: all %d real manifests validate under the strict rules (one per skill with reference files: %d)",
           length(real), length(with_refs)))

# ==== K. Stage 2 regenerated PsN dependency records ========================
uncertainty_manifest <- read_method_manifest("skills/psn-uncertainty/MANIFEST.yaml")
simeval_manifest <- read_method_manifest("skills/psn-simeval/MANIFEST.yaml")
covariate_manifest <- read_method_manifest("skills/covariate-priors/MANIFEST.yaml")
helper <- "tools/psn_invocation.R"
dependency_paths <- function(unit)
  vapply(unit$depends, function(dependency) dependency$path, character(1L))
ok(file.exists(helper) &&
     all(vapply(uncertainty_manifest$units, function(unit)
       helper %in% dependency_paths(unit), logical(1L))) &&
     all(vapply(simeval_manifest$units, function(unit)
       helper %in% dependency_paths(unit), logical(1L))),
   "K/STAGE2 uncertainty and simulation units bind the shared helper digest")
covariance_dependencies <- dependency_paths(covariate_manifest$units$psn_covsearch)
ok(identical(covariance_dependencies,
             c("skills/psn-uncertainty/reference/psn_uncertainty.R",
               "skills/covariate-priors/reference/scm_config.R",
               helper)) &&
     identical(covariate_manifest$units$psn_covsearch$sha256,
               .mm_file_sha256("skills/covariate-priors/reference/psn_covsearch.R")),
   "K/STAGE2 covariate search binds two source dependencies plus the transitive helper and retains its source digest")

# The covariate-search gate does not call the uncertainty gate before using
# uncertainty's invocation closures. Its own manifest must therefore bind the
# helper transitively. Prove that at the gate boundary in a disposable tree.
binding_fixture <- tempfile("mm-stage2-binding-")
dir.create(file.path(binding_fixture, "tools"), recursive = TRUE)
for (file in c("method_manifest.R", "load_reference.R", "psn_invocation.R"))
  file.copy(file.path(REPO, "tools", file), file.path(binding_fixture, "tools", file))
dir.create(file.path(binding_fixture, "skills"))
for (skill in c("psn-uncertainty", "covariate-priors"))
  file.copy(file.path(REPO, "skills", skill), file.path(binding_fixture, "skills"),
            recursive = TRUE)
binding_manifest <- file.path(binding_fixture, "skills", "covariate-priors", "MANIFEST.yaml")
binding_lines <- readLines(binding_manifest)
binding_lines[grep("^reviewed_by:", binding_lines)] <- 'reviewed_by: "Fixture SME"'
binding_lines[grep("^reviewed_at:", binding_lines)] <- 'reviewed_at: "2026-09-11"'
binding_header <- grep("^  psn_covsearch:$", binding_lines)
binding_status <- binding_header +
  which(grepl("^    status:", binding_lines[(binding_header + 1L):length(binding_lines)]))[1L]
binding_lines[binding_status] <- "    status: approved"
writeLines(binding_lines, binding_manifest)
cat('\nbase::writeLines("ran", "HELPER_EXECUTED")\n',
    file = file.path(binding_fixture, helper), append = TRUE)
setwd(binding_fixture)
loader_env <- new.env(parent = baseenv())
sys.source("tools/load_reference.R", envir = loader_env, keep.source = FALSE)
binding_error <- tryCatch({
  binding_env <- loader_env$load_reference(
    "skills/covariate-priors",
    "skills/covariate-priors/reference/psn_covsearch.R")
  binding_env$.psn_covsearch_gate()
  NULL
}, error = function(e) e)
ok(inherits(binding_error, "error") &&
     grepl("psn_invocation.R", conditionMessage(binding_error), fixed = TRUE) &&
     grepl("do not match", conditionMessage(binding_error), fixed = TRUE) &&
     !file.exists("HELPER_EXECUTED"),
   "K/STAGE2 supported loader refuses a drifted covariate-search helper before its top level runs")
setwd(REPO)

# Approval state is workflow state, not a permanent repository invariant. Pin
# regeneration behavior in a disposable, initially confirmed fixture so this
# battery passes both before and after legitimate SME confirmation.
stage2_fixture <- tempfile("mm-stage2-regen-")
dir.create(file.path(stage2_fixture, "tools"), recursive = TRUE)
for (file in c("method_manifest.R", "psn_invocation.R"))
  file.copy(file.path(REPO, "tools", file), file.path(stage2_fixture, "tools", file))
dir.create(file.path(stage2_fixture, "skills"))
for (skill in c("psn-uncertainty", "psn-simeval", "covariate-priors"))
  file.copy(file.path(REPO, "skills", skill), file.path(stage2_fixture, "skills"),
            recursive = TRUE)
setwd(stage2_fixture)
set_fixture_manifest <- function(skill, statuses) {
  path <- file.path("skills", skill, "MANIFEST.yaml")
  lines <- readLines(path)
  lines[grep("^reviewed_by:", lines)] <- 'reviewed_by: "Fixture SME"'
  lines[grep("^reviewed_at:", lines)] <- 'reviewed_at: "2026-09-11"'
  for (unit in names(statuses)) {
    header <- grep(paste0("^  ", unit, ":$"), lines)
    status_line <- header + which(grepl("^    status:", lines[(header + 1L):length(lines)]))[1L]
    lines[status_line] <- paste0("    status: ", statuses[[unit]])
  }
  writeLines(lines, path)
}
set_fixture_manifest("psn-uncertainty",
  c(bootstrap = "approved", sir = "draft", llp = "draft"))
set_fixture_manifest("psn-simeval",
  c(vpc = "approved", npc = "draft", sse = "draft"))
set_fixture_manifest("covariate-priors",
  c(covariate_search = "draft", psn_covsearch = "draft", scm_config = "draft",
    covariate_effect = "approved", nested_lrt = "approved"))
fixture_manifests <- file.path("skills",
  c("psn-uncertainty", "psn-simeval", "covariate-priors"), "MANIFEST.yaml")
ok(all(vapply(fixture_manifests, function(path) {
       manifest <- validate_method_manifest(path)
       identical(manifest$reviewed_by, "Fixture SME")
     }, logical(1L))),
   "K/STAGE2 controlled regeneration fixture starts confirmed and digest-valid")

cat("\n# controlled helper dependency mutation\n", file = "tools/psn_invocation.R",
    append = TRUE)
cat("\n# controlled reference mutation\n",
    file = "skills/psn-uncertainty/reference/psn_uncertainty.R", append = TRUE)
cat("\n# controlled reference mutation\n",
    file = "skills/psn-simeval/reference/psn_simeval.R", append = TRUE)
regenerate_fixture_manifest <- function(skill_dir) {
  manifest <- read_method_manifest(file.path(skill_dir, "MANIFEST.yaml"))
  spec <- lapply(manifest$units, function(unit) list(
    reference_path = unit$reference_path, entrypoints = unit$entrypoints,
    status = unit$status, evidence = unit$evidence, reason = unit$reason,
    depends = vapply(unit$depends, function(dependency) dependency$path,
                     character(1L))))
  suppressMessages(write_method_manifest(skill_dir, spec))
}
for (skill_dir in dirname(fixture_manifests)) regenerate_fixture_manifest(skill_dir)
regenerated <- lapply(fixture_manifests, read_method_manifest)
names(regenerated) <- c("uncertainty", "simeval", "covariate")
ok(all(vapply(regenerated, function(manifest)
       !nzchar(manifest$reviewed_by) && !nzchar(manifest$reviewed_at), logical(1L))) &&
     all(vapply(regenerated$uncertainty$units, function(unit)
       identical(unit$status, "draft"), logical(1L))) &&
     all(vapply(regenerated$simeval$units, function(unit)
       identical(unit$status, "draft"), logical(1L))) &&
     identical(regenerated$covariate$units$covariate_effect$status, "approved") &&
     identical(regenerated$covariate$units$nested_lrt$status, "approved") &&
     all(vapply(regenerated$covariate$units[
       c("covariate_search", "psn_covsearch", "scm_config")],
       function(unit) identical(unit$status, "draft"), logical(1L))),
   "K/STAGE2 controlled drift clears roots and derives required unit statuses")
ok(all(vapply(c(regenerated$uncertainty$units, regenerated$simeval$units),
       function(unit) identical(unit$depends[[1L]]$sha256,
         .mm_file_sha256("tools/psn_invocation.R")), logical(1L))) &&
     identical(regenerated$covariate$units$psn_covsearch$depends[[1L]]$sha256,
       .mm_file_sha256("skills/psn-uncertainty/reference/psn_uncertainty.R")),
   "K/STAGE2 controlled regeneration derives changed helper and transitive digests")
setwd(REPO)
setwd(root)

# --- RM: reference-model templates (units programme U6) --------------------
# The four .ctl templates are not sourced by any battery. These cases read
# them and skills/reference-models/SKILL.md as text from the repository root. The
# subroutines per template come from the SKILL.md starter-set table, so the
# table and the templates cannot drift apart silently.
setwd(REPO)
rm_dir <- file.path("skills", "reference-models", "reference")
rm_skill <- readLines(file.path("skills", "reference-models", "SKILL.md"), warn = FALSE)
rm_units <- c("onecmt_iv", "onecmt_oral", "twocmt_iv", "twocmt_oral")
rm_manifest <- read_method_manifest(file.path(dirname(rm_dir), "MANIFEST.yaml"))
ok(setequal(names(rm_manifest$units), rm_units) &&
     identical(rm_manifest$reviewed_by, "") && identical(rm_manifest$reviewed_at, ""),
   "RM/public: exactly four reference units with no recorded review")
# ADVAN -> scaling compartment and TRANS4 volume names (central first).
rm_scale <- c(ADVAN1 = "S1", ADVAN2 = "S2", ADVAN3 = "S1", ADVAN4 = "S2")
rm_vols  <- list(ADVAN1 = "V", ADVAN2 = "V", ADVAN3 = c("V1", "V2"),
                 ADVAN4 = c("V2", "V3"))
rm_code <- function(x) gsub("\\s+", " ", trimws(sub(";.*$", "", x)))
rm_rec  <- function(code, rec) which(startsWith(code, rec))
# The contiguous comment lines directly above line i.
rm_above <- function(x, i) {
  j <- i - 1L
  while (j >= 1L && grepl("^\\s*;", x[j])) j <- j - 1L
  if (j + 1L <= i - 1L) x[(j + 1L):(i - 1L)] else character(0)
}
for (u in rm_units) {
  filename <- paste0(u, ".ctl")
  path <- file.path(rm_dir, filename)
  x <- readLines(path, warn = FALSE)
  code <- rm_code(x)
  comments <- sub("^[^;]*;", "", x[grepl(";", x)])
  # Match the starter-set table's reference path for this unit.
  row <- grep(paste0("^\\| `", u, "` \\| `reference/"), rm_skill, value = TRUE)
  subr <- if (length(row) == 1L) trimws(strsplit(row, "|", fixed = TRUE)[[1]][5]) else "none"
  advan <- sub(" .*", "", subr)
  if (!advan %in% names(rm_scale)) advan <- "ADVAN1"  # the subroutines case has already failed
  i_in <- rm_rec(code, "$INPUT"); i_da <- rm_rec(code, "$DATA")
  ok(length(i_in) == 1L && length(i_da) == 1L && i_in < i_da,
     sprintf("RM/%s input record precedes the data record", u))
  ok(length(i_da) == 1L && startsWith(code[i_da], "$DATA ../analysis-ready.csv "),
     sprintf("RM/%s names the project-generated analysis-ready input", u))
  ok(length(row) == 1L && sum(startsWith(code, "$SUB")) == 1L &&
       sum(code == paste("$SUBROUTINES", subr)) == 1L,
     sprintf("RM/%s subroutines equal the SKILL table (%s)", u, subr))
  scal <- grep("^S[0-9]+ ?=", code, value = TRUE)
  vols <- rm_vols[[advan]]
  ok(length(scal) == 1L && identical(scal, paste(rm_scale[[advan]], "=", vols[1])),
     sprintf("RM/%s scales the central compartment with %s", u, rm_scale[[advan]]))
  defined <- sub(" ?=.*", "", grep("^[A-Z][A-Z0-9_]* ?=", code, value = TRUE))
  wrong <- setdiff(c("V", "V1", "V2", "V3"), vols)
  ok(all(vols %in% defined) && !any(wrong %in% defined),
     sprintf("RM/%s volume names match %s", u, advan))
  pk <- code[seq(rm_rec(code, "$PK"), rm_rec(code, "$ERROR"))]
  noth <- gsub("THETA\\([0-9]+\\)", "", pk)
  etas <- sort(unique(as.integer(unlist(regmatches(noth,
            gregexpr("(?<=ETA\\()[0-9]+", noth, perl = TRUE))))))
  mu_ok <- vapply(etas, function(n) {
    mu <- grep(sprintf("^MU_%d = ", n), pk, value = TRUE)
    length(mu) == 1L && grepl("THETA", mu) && !grepl("ETA\\(", sub("THETA", "", mu)) &&
      sum(grepl(sprintf("^[A-Z0-9]+ = EXP\\(MU_%d \\+ ETA\\(%d\\)\\)$", n, n), pk)) == 1L
  }, logical(1L))
  params <- grep("^(CL|V|V1|V2|V3|Q|KA) = ", pk, value = TRUE)
  ok(length(etas) >= 1L && all(mu_ok) && all(grepl("= EXP\\(", params)),
     sprintf("RM/%s THETAs are mu-referenced on the log scale", u))
  ok(sum(grepl("^\\$ESTIMATION METHOD=1 INTERACTION( |$)", code)) == 1L &&
       sum(startsWith(code, "$EST")) == 1L,
     sprintf("RM/%s estimation is METHOD=1 INTERACTION", u))
  ok(sum(code == "$COVARIANCE") == 1L, sprintf("RM/%s covariance record present", u))
  tab <- strsplit(grep("^\\$TABLE", code, value = TRUE), " ")
  ok(length(tab) == 1L && identical(tab[[1]][2], "NUM") && !("DV" %in% tab[[1]]),
     sprintf("RM/%s table has NUM first and no DV", u))
  marked <- vapply(c("$INPUT", "$DATA", "$THETA", "$TABLE"), function(r)
    any(grepl("ADAPT-HERE", rm_above(x, which(startsWith(code, r))[1L]))), logical(1L))
  ok(all(marked), sprintf("RM/%s ADAPT-HERE marks input, data, parameters and table", u))
  inits <- x[grepl("^\\s*\\(?-?[0-9.]+\\)?\\s*;", x)]
  ok(length(inits) >= 3L && all(grepl("PLACEHOLDER", inits)),
     sprintf("RM/%s every initial carries a PLACEHOLDER label", u))
  ok(!any(grepl("\\$[A-Za-z]", comments)),
     sprintf("RM/%s no record keyword inside a comment", u))
  declarations <- gsub("[[:space:]]+", " ", trimws(comments))
  status <- paste0("Status: see skills/reference-models/MANIFEST.yaml, unit `", u, "`.")
  unit <- rm_manifest$units[[u]]
  ok(sum(startsWith(code, "$PROBLEM")) == 1L &&
       startsWith(code[1L], paste0("$PROBLEM REFERENCE TEMPLATE ", u, " - ")) &&
       status %in% declarations && paste0("Based on: ", path, " (template)") %in% declarations &&
       any(grepl("A template is `draft` until", declarations, fixed = TRUE)) &&
       "ADAPTATION CONTRACT" %in% declarations &&
       identical(unit$status, "draft") && identical(unit$evidence, "") &&
       identical(unit$reference_path, path) && identical(unit$entrypoints, filename) &&
       identical(unit$sha256, .mm_file_sha256(path)) &&
       !any(grepl("[[:xdigit:]]{64}|\\.zip\\b", comments, ignore.case = TRUE)),
     sprintf("RM/%s declares its draft identity, local provenance and adaptation contract", u))
}

# Public starters declare the project-generated input. An independently
# authored adaptation replaces only the data record of a disposable copy.
tpl <- readLines(file.path(rm_dir, "onecmt_iv.ctl"), warn = FALSE)
data_line <- which(startsWith(rm_code(tpl), "$DATA"))
ok(length(data_line) == 1L &&
     startsWith(rm_code(tpl)[data_line], "$DATA ../analysis-ready.csv "),
   "RM/public: shipped template names the project-generated analysis-ready input")
adapted <- tpl
adapted[data_line] <- sub("../analysis-ready.csv", "./generated-observations.csv",
                         tpl[data_line], fixed = TRUE)
fixture <- tempfile("adapted-model-", fileext = ".ctl")
writeLines(adapted, fixture)
got <- readLines(fixture)
ok(identical(got[-data_line], tpl[-data_line]) &&
     identical(sub("./generated-observations.csv", "../analysis-ready.csv",
                   got[data_line], fixed = TRUE), tpl[data_line]),
   "RM/public: disposable data-path adaptation preserves every other template line")
setwd(root)

cat(sprintf("\nmethod-manifest battery: %d passed, %d failed\n", .pass, .fail))
quit(status = if (.fail > 0L) 1L else 0L)

}  # end .run_method_manifest_tests

if (.invoked_as_script()) .run_method_manifest_tests()
