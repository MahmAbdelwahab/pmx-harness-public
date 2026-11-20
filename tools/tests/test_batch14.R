#!/usr/bin/env Rscript
# Integrity batch 14 battery (Codex round 13 findings, SME decisions 2026-09-05).
#
# Sections:
#   A  R13-P0-1  the eda source-identity chain cannot be skipped: a missing or
#                non-function inventoried definition is a refusal, the
#                structural comparison runs unconditionally, and the captured
#                inventory is complete.
#   C  R13-P0-2  the loader resolves nothing through the caller: operator and
#                accessor fakes in the environment it was sourced into cannot
#                change which expressions it evaluates.
#   I  R13-P2-4  the persistent regression checks: the prologue WARN is asserted
#                positively on a fixture and negatively on the shipped
#                references, and a hostile load whose running implementation is
#                tampered is REFUSED.
#   H  R13-P2-2  one manifest grammar, both readers: every refused construct is
#                refused by tools/method_manifest.R AND by the write guard.
#   G  R13-P2-3  the CI check parses, it does not grep: multiline, extra
#                whitespace, named-argument and sys.source spellings of a direct
#                reference load all FAIL.
#   F  R13-P1-2  one record snapshot per ledger event (SME decision D14-1):
#                approve, revoke and release build their event from the bytes
#                they parsed and validated, and refuse if the live record drifts
#                before the event is committed.
#   E  R13-P1-3  a gate's literal skill_dir / calling_file must name THIS skill
#                and THIS file, not another skill's approved unit of the same
#                name.
#   D  R13-P3-1 / R13-P2-1  header census (no reference claims its own status)
#                and non-base call-head census (every reference is callable
#                through the default, base-parented loader environment).
#   B  R13-P1-1  no re-execution of unauthenticated bytes during a gated call:
#                the disk-side identity comes from one byte snapshot, the
#                manifest digest is checked against that snapshot before it is
#                parsed, and the cache is keyed on the bytes actually parsed.
#
# Run with:  Rscript tools/tests/test_batch14.R
# Exits non-zero if any assertion fails.

# SAFE TO SOURCE: pipelines tar_source("tools") which recurses into
# tools/tests/; everything observable is guarded behind .invoked_as_script().
.invoked_as_script <- function() {
  fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  length(fa) > 0 && grepl("test_batch14\\.R$", sub("^--file=", "", fa[1]))
}

.run_batch14_tests <- function() {

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

old_wd <- getwd(); on.exit(setwd(old_wd), add = TRUE)

# --- the eda fixture --------------------------------------------------------
# A disposable copy of the repository-relative paths the eda reference is
# written for: the reference at its manifest path, the manifest with the
# digests of the copied bytes, and the declared dependency. Nothing here
# touches the live tree.
eda_fixture <- function() {
  rt <- tempfile("b14eda"); dir.create(rt)
  dir.create(file.path(rt, "tools"))
  dir.create(file.path(rt, "skills", "eda-standards", "reference"), recursive = TRUE)
  for (f in c("method_manifest.R", "record_selection.R"))
    file.copy(file.path(REPO, "tools", f), file.path(rt, "tools", f), overwrite = TRUE)
  file.copy(file.path(REPO, "skills", "eda-standards", "reference", "eda.R"),
            file.path(rt, "skills", "eda-standards", "reference", "eda.R"), overwrite = TRUE)
  txt <- readLines(file.path(REPO, "skills", "eda-standards", "MANIFEST.yaml"), warn = FALSE)
  old_ref <- sub("^\\s*sha256:\\s*", "", grep("^    sha256:", txt, value = TRUE)[1])
  old_dep <- sub("^\\s*sha256:\\s*", "", grep("^        sha256:", txt, value = TRUE)[1])
  new_ref <- .mm_file_sha256(file.path(rt, "skills", "eda-standards", "reference", "eda.R"))
  new_dep <- .mm_file_sha256(file.path(rt, "tools", "record_selection.R"))
  txt <- gsub(old_ref, new_ref, txt, fixed = TRUE)
  txt <- gsub(old_dep, new_dep, txt, fixed = TRUE)
  # The fixture owns its own approval state: these tests are about the identity
  # chain and the byte snapshot, not about what the SME has signed in the live
  # tree, whose statuses reset to draft whenever a reference is regenerated.
  txt[grepl("^reviewed_by:", txt)] <- 'reviewed_by: "Fixture Reviewer"'
  txt[grepl("^reviewed_at:", txt)] <- 'reviewed_at: "2026-09-05"'
  txt[grepl("^    status:", txt)] <- "    status: approved"
  writeLines(txt, file.path(rt, "skills", "eda-standards", "MANIFEST.yaml"))
  rt
}
# The manifest digests must be refreshed after the fixture's own reference
# bytes are rewritten by a scenario that edits the file on disk.
eda_reseal <- function(rt) {
  p <- file.path(rt, "skills", "eda-standards", "MANIFEST.yaml")
  txt <- readLines(p, warn = FALSE)
  old_ref <- sub("^\\s*sha256:\\s*", "", grep("^    sha256:", txt, value = TRUE)[1])
  new_ref <- .mm_file_sha256(file.path(rt, "skills", "eda-standards", "reference", "eda.R"))
  writeLines(gsub(old_ref, new_ref, txt, fixed = TRUE), p)
  invisible(new_ref)
}
EDA_REF <- "skills/eda-standards/reference/eda.R"
eda_src <- readLines(file.path(REPO, EDA_REF), warn = FALSE)
VIEW <- list(selector_digest = "sha256:fixture", row_id_col = "ROWID")
ROWS <- data.frame(ROWID = 1:3)

lr_env <- new.env(parent = globalenv())
sys.source(file.path(TOOLS, "load_reference.R"), envir = lr_env, keep.source = FALSE)
load_reference <- get("load_reference", envir = lr_env)

cat("\n--- A: R13-P0-1, the eda source-identity chain cannot skip itself -----------\n")

# Scenario 0 (positive control): the honest fixture computes.
rt <- eda_fixture(); setwd(rt)
E <- new.env(parent = baseenv())
r0 <- tryCatch({ load_reference("skills/eda-standards", EDA_REF, envir = E)
                 E$reference_eda_rowset_digest(VIEW, ROWS) },
               error = function(e) conditionMessage(e))
if (!is.character(r0) || !grepl("^sha256:", r0)) cat(sprintf("      got: %s\n", paste(r0, collapse = " ")))
ok(is.character(r0) && grepl("^sha256:", r0),
   "A/positive: the honest eda reference computes a row-set digest through its gate")

# Scenario 1: remove `reference_eda_view` from the loaded environment and swap
# `.eda_digest` for a fake that writes a marker. Nothing on disk changed, and
# no sealed environment was reopened -- this is ordinary rebinding.
setwd(old_wd); rt <- eda_fixture(); setwd(rt)
E1 <- new.env(parent = baseenv())
load_reference("skills/eda-standards", EDA_REF, envir = E1)
rm("reference_eda_view", envir = E1)
assign(".eda_digest", function(x) { file.create("MARKER_S1"); "sha256:forged" }, envir = E1)
r1 <- tryCatch(E1$reference_eda_rowset_digest(VIEW, ROWS), error = function(e) conditionMessage(e))
if (!is.character(r1) || !grepl("ESCALATE", r1)) cat(sprintf("      got: %s\n", paste(r1, collapse = " ")))
ok(is.character(r1) && grepl("ESCALATE", r1),
   "A/1: a missing inventoried entry point is REFUSED, not a reason to skip the comparison")
ok(!file.exists("MARKER_S1"), "A/1: the substituted `.eda_digest` never ran (no marker)")

# Scenario 2: bind a fake `c()` that empties the two inventory vectors before
# the reference is evaluated into the environment, remove it afterwards, then
# swap `.eda_digest`. An empty captured inventory would make both sides of the
# structural comparison digest nothing.
setwd(old_wd); rt <- eda_fixture(); setwd(rt)
E2 <- new.env(parent = baseenv())
assign("c", function(...) {
  a <- list(...)
  if (length(a) && is.character(a[[1]]) &&
      a[[1]][1] %in% c("reference_eda_view", ".eda_gate")) return(character(0))
  do.call(base::c, a)
}, envir = E2)
r2load <- tryCatch({ load_reference("skills/eda-standards", EDA_REF, envir = E2); TRUE },
                   error = function(e) conditionMessage(e))
if (isTRUE(r2load)) {
  rm("c", envir = E2)
  assign(".eda_digest", function(x) { file.create("MARKER_S2"); "sha256:forged" }, envir = E2)
  r2 <- tryCatch(E2$reference_eda_rowset_digest(VIEW, ROWS), error = function(e) conditionMessage(e))
} else r2 <- r2load
if (!is.character(r2) || !grepl("ESCALATE", r2)) cat(sprintf("      got: %s\n", paste(r2, collapse = " ")))
ok(is.character(r2) && grepl("ESCALATE", r2),
   "A/2: a fake `c()` cannot empty the captured inventory (the load refuses, or the swap is caught)")
ok(!file.exists("MARKER_S2"), "A/2: the substituted `.eda_digest` never ran (no marker)")

cat("\n--- C: R13-P0-2, the trusted loader resolves nothing through the caller ----\n")

# The loader is sourced into an environment that already binds hostile `[[`,
# `$`, `get`, `eval`, `parse` and `seq_along`. The `[[` fake substitutes a
# side-effecting expression whenever it is handed an expression vector; the
# `$` fake substitutes the manifest reader. Neither is reflection: both are
# ordinary bindings in the environment the loader's own body would resolve
# through if it had no controlled enclosure.
setwd(old_wd); rt <- eda_fixture(); setwd(rt)
H <- new.env(parent = globalenv())
assign("[[", function(x, ...) {
  if (is.expression(x)) { file.create("MARKER_LR_bracket"); return(quote(base::file.create("MARKER_LR_EVAL"))) }
  base::`[[`(x, ...)
}, envir = H)
assign("$", function(x, name) {
  name <- as.character(substitute(name))
  if (is.environment(x) && identical(name, "read_method_manifest")) {
    file.create("MARKER_LR_dollar")
    return(function(...) stop("forged manifest reader"))
  }
  base::get(name, envir = as.environment(x))
}, envir = H)
assign("get", function(...) { file.create("MARKER_LR_get"); base::get(...) }, envir = H)
assign("eval", function(...) { file.create("MARKER_LR_eval"); base::eval(...) }, envir = H)
assign("parse", function(...) { file.create("MARKER_LR_parse"); base::parse(...) }, envir = H)
assign("seq_along", function(...) { file.create("MARKER_LR_seq"); integer(0) }, envir = H)
sys.source(file.path(TOOLS, "load_reference.R"), envir = H, keep.source = FALSE)
hostile_loader <- get("load_reference", envir = H)
Eh <- new.env(parent = baseenv())
rc <- tryCatch({ hostile_loader("skills/eda-standards", EDA_REF, envir = Eh); TRUE },
               error = function(e) conditionMessage(e))
if (!isTRUE(rc)) cat(sprintf("      got: %s\n", paste(rc, collapse = " ")))
ok(isTRUE(rc), "C: the loader completes with hostile operator bindings in its sourcing environment")
ok(!file.exists("MARKER_LR_EVAL"),
   "C: the substituted expression was never evaluated (no marker)")
# `eval` and `parse` markers are not asserted absent: base::sys.source(), which
# the FIXTURE uses to bring the loader into the hostile environment, itself
# calls `eval(exprs[i], envir)` and so trips a fake bound there. That is the
# fixture's own mechanism, not the loader's body. The operators the loader's
# body would have resolved through its sourcing environment before this fix --
# `[[`, `$`, `get`, `seq_along` -- must never be reached.
ok(!file.exists("MARKER_LR_bracket") && !file.exists("MARKER_LR_dollar") &&
     !file.exists("MARKER_LR_get") && !file.exists("MARKER_LR_seq"),
   "C: no fake `[[`, `$`, `get` or `seq_along` was reached by the loader's body")
# The definitions evaluated are exactly the parse of the bytes on disk.
Eclean <- new.env(parent = baseenv())
honest <- tryCatch({ load_reference("skills/eda-standards", EDA_REF, envir = Eclean); TRUE },
                   error = function(e) conditionMessage(e))
names_h <- if (isTRUE(honest)) sort(ls(Eclean, all.names = TRUE)) else character(0)
names_x <- if (isTRUE(rc)) sort(ls(Eh, all.names = TRUE)) else character(0)
# DEPARSED DEFINITIONS, NOT ONLY NAMES (integrity batch 15, Codex round 14
# R14-P3-1). Equal name sets and equal load-record digests do not by themselves
# establish that the two loads evaluated the same definitions; the definitions
# are therefore compared as text, source references removed so the comparison is
# of the code rather than of where it was read from.
defs_text <- function(env, nms) vapply(nms, function(n) {
  v <- get(n, envir = env, inherits = FALSE)
  if (is.function(v)) paste(deparse(utils::removeSource(v)), collapse = "\n")
  else paste(deparse(v), collapse = "\n")
}, character(1))
ok(length(names_h) > 0 && identical(names_h, names_x),
   sprintf("C: the hostile-frame load evaluated the same definitions as an honest load (%d names)",
           length(names_h)))
ok(length(names_h) > 0 && identical(names_h, names_x) &&
     identical(defs_text(Eclean, names_h), defs_text(Eh, names_x)),
   "C: the two loads' definitions are identical as deparsed text, not merely equally named")
ok(isTRUE(rc) && is.character(attr(Eh, "load_reference")$sha256) &&
     identical(attr(Eh, "load_reference")$sha256, attr(Eclean, "load_reference")$sha256),
   "C: both loads record the digest of the same bytes")
# Static: the loader is built in a controlled, base-parented enclosure and its
# body qualifies every extraction and replacement operator.
lr_src <- readLines(file.path(TOOLS, "load_reference.R"), warn = FALSE)
lr_code <- lr_src[!grepl("^\\s*#", lr_src)]
ok(any(grepl("^load_reference <- base::local\\(\\{", lr_code)) &&
     any(grepl("envir = base::new.env(parent = base::baseenv())", lr_code, fixed = TRUE)),
   "C: load_reference() is constructed with base::local() in a base-parented environment")
ok(!any(grepl("[^:a-zA-Z._`]\\$", lr_code)) && !any(grepl("[^:`]\\[\\[", lr_code)),
   "C: no unqualified `$` or `[[` remains in the loader's code")

cat("\n--- I: R13-P2-4, persistent regression checks ------------------------------\n")
setwd(old_wd)
irt <- tempfile("b14warn"); dir.create(irt)
dir.create(file.path(irt, "tools"))
file.copy(file.path(TOOLS, "method_manifest.R"), file.path(irt, "tools", "method_manifest.R"))
setwd(irt)
mkw <- function(name, body) {
  d <- file.path("skills", name)
  dir.create(file.path(d, "reference"), recursive = TRUE, showWarnings = FALSE)
  p <- file.path(d, "reference", "method.R")
  writeLines(body, p)
  writeLines(c("# fixture", paste0("skill: ", name), "generated_at: 2026-09-05T00:00:00Z",
               'reviewed_by: "Fixture Reviewer"', 'reviewed_at: "2026-09-01"', "units:", "  alpha:",
               "    status: approved", paste0("    reference_path: ", p),
               "    entrypoints: [ep]", paste0("    sha256: ", .mm_file_sha256(p)),
               "    evidence: SKILL.md", '    reason: "fixture"'),
             file.path(d, "MANIFEST.yaml"))
  list(dir = d, path = p)
}
prol <- mkw("i_prologue", c(
  '.mm_trusted_private <- base::local({',
  '  e <- base::new.env(parent = base::baseenv())',
  '  if (!base::file.exists("tools/method_manifest.R"))',
  '    base::stop("missing", call. = FALSE)',
  '  base::sys.source("tools/method_manifest.R", envir = e, keep.source = FALSE)',
  '  base::lockEnvironment(e, bindings = TRUE)',
  '  e',
  '}, envir = base::new.env(parent = base::baseenv()))',
  '.side <- function() invisible(TRUE)',
  '.alpha_gate <- function() {',
  '  .side()',
  sprintf('  base::get("require_validated_method", envir = .mm_trusted_private)(skill_dir = "%s", calling_file = "%s", unit = "alpha")',
          "skills/i_prologue", "skills/i_prologue/reference/method.R"),
  '}',
  'ep <- function(x) { .alpha_gate(); x }'))
warns <- character(0)
res <- withCallingHandlers(audit_entrypoint_gating(prol$dir),
         warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") })
ok(isTRUE(res$gated[1]) && identical(read_method_manifest(file.path(prol$dir, "MANIFEST.yaml"))$units$alpha$status, "approved"),
   "B14-AUDIT: the explicitly approved disposable prologue fixture is still gated")
ok(any(grepl("gate-chain prologue", warns, fixed = TRUE)) &&
     any(grepl("unqualified", warns, fixed = TRUE)),
   "I: the prologue WARN fires on a fixture whose stepped-over prologue calls an unqualified name")
if (!length(warns)) cat("      got: no warnings at all\n")
# The same warning must NOT fire on any shipped reference.
setwd(REPO)
shipped_warns <- character(0)
# THE AUDIT MUST COMPLETE BEFORE ITS SILENCE MEANS ANYTHING (integrity batch 15,
# Codex round 14 R14-P3-1). This loop swallowed every error with
# `error = function(e) NULL`, so an audit that FAILED counted as an audit that
# raised no prologue warning. Completion is now asserted separately, per skill.
shipped_audit_fail <- character(0)
shipped_mpaths <- sort(Sys.glob("skills/*/MANIFEST.yaml"))
for (mpath in shipped_mpaths)
  withCallingHandlers(
    tryCatch(audit_entrypoint_gating(dirname(mpath)),
             error = function(e) shipped_audit_fail <<- c(shipped_audit_fail,
               paste0(dirname(mpath), ": ", substr(conditionMessage(e), 1, 120)))),
    warning = function(w) { shipped_warns <<- c(shipped_warns, conditionMessage(w));
                            invokeRestart("muffleWarning") })
if (length(shipped_audit_fail)) cat(sprintf("      got: %s\n", paste(shipped_audit_fail, collapse = " | ")))
ok(length(shipped_mpaths) > 0L && length(shipped_audit_fail) == 0L,
   sprintf("I: the entry-point gating audit COMPLETED for all %d shipped skills", length(shipped_mpaths)))
prologue_warns <- grep("gate-chain prologue", shipped_warns, fixed = TRUE, value = TRUE)
ok(length(prologue_warns) == 0L,
   sprintf("I: no shipped reference raises the prologue WARN (%d raised)", length(prologue_warns)))
if (length(prologue_warns)) for (w in prologue_warns) cat("      ", substr(w, 1, 160), "\n")

# A hostile load whose RUNNING implementation is tampered must be REFUSED, not
# merely reach the real validator (test_batch13.R:636 judged success by
# "REAL VALIDATOR RAN" alone, which a tampered implementation also achieves).
setwd(old_wd); rt <- eda_fixture(); setwd(rt)
Et <- new.env(parent = baseenv())
load_reference("skills/eda-standards", EDA_REF, envir = Et)
assign(".eda_kept", function(...) { file.create("MARKER_TAMPER"); TRUE }, envir = Et)
rt1 <- tryCatch(Et$reference_eda_rowset_digest(VIEW, ROWS), error = function(e) conditionMessage(e))
if (!is.character(rt1) || !grepl("ESCALATE", rt1)) cat(sprintf("      got: %s\n", paste(rt1, collapse = " ")))
ok(is.character(rt1) && grepl("do not match the", rt1, fixed = TRUE) && grepl("ESCALATE", rt1),
   "I: a tampered running implementation is REFUSED even though the real validator would have run")
ok(!file.exists("MARKER_TAMPER"), "I: the tampered helper never ran (no marker)")
setwd(old_wd)

cat("\n--- H: R13-P2-2, one manifest grammar, both readers ------------------------\n")
setwd(REPO)
hrt <- tempfile("b14yaml"); dir.create(hrt)
py_problem <- function(path) {
  cmd <- sprintf("import importlib.util;spec=importlib.util.spec_from_file_location('pp','%s');m=importlib.util.module_from_spec(spec);spec.loader.exec_module(m);print(m._manifest_document(open('%s').read(), __import__('yaml'))[1])",
                 file.path(REPO, ".claude", "hooks", "protect_paths.py"), path)
  out <- suppressWarnings(system2("python3", c("-c", shQuote(cmd)), stdout = TRUE, stderr = TRUE))
  paste(out, collapse = " ")
}
# A fixture that does not parse as YAML would be refused as a parse error rather
# than on the grammar rule under test, so every grammar variant that is meant to
# be well-formed YAML is checked here first.
py_parses <- function(path) {
  cmd <- sprintf("import yaml;yaml.safe_load(open('%s').read());print('PARSES')", path)
  out <- suppressWarnings(system2("python3", c("-c", shQuote(cmd)), stdout = TRUE, stderr = TRUE))
  any(grepl("PARSES", out, fixed = TRUE))
}
base_manifest <- c(
  "# fixture", "skill: h", "generated_at: 2026-09-05T00:00:00Z",
  'reviewed_by: "Fixture Reviewer"', 'reviewed_at: "2026-09-01"', "units:", "  alpha:",
  "    status: approved", "    reference_path: skills/h/reference/method.R",
  "    entrypoints: [ep]",
  "    sha256: sha256:0000000000000000000000000000000000000000000000000000000000000000",
  "    depends:", "      - path: tools/dep.R",
  "        sha256: sha256:1111111111111111111111111111111111111111111111111111111111111111",
  "    evidence: SKILL.md", '    reason: "fixture"')
variants <- list(
  "a backslash in a dependency path" =
    sub("      - path: tools/dep.R", '      - path: "tools/site\\\\helper.R"', base_manifest, fixed = TRUE),
  "an anchor on a scalar" = sub("^skill: h$", "skill: &s h", base_manifest),
  "an alias as a scalar" = sub("^    evidence: SKILL.md$", "    evidence: *s", base_manifest),
  "a YAML tag" = sub("^    status: approved$", "    status: !!str approved", base_manifest),
  "an inline empty depends" = c(base_manifest[1:11], "    depends: []", base_manifest[15:16]),
  "an inline mapping" = sub("^    evidence: SKILL.md$", "    evidence: {a: b}", base_manifest),
  "a merge key" = append(base_manifest, "    <<: *s", after = 7L),
  "a dependency entry with sha256 first, as the FIRST entry" =
    c(base_manifest[1:12],
      "      - sha256: sha256:1111111111111111111111111111111111111111111111111111111111111111",
      "        path: tools/dep.R", base_manifest[15:16]),
  "a dependency entry with sha256 first, as the SECOND entry" =
    c(base_manifest[1:14],
      "      - sha256: sha256:2222222222222222222222222222222222222222222222222222222222222222",
      "        path: tools/two.R", base_manifest[15:16]))
# The three variants below are well-formed YAML: their refusal must come from the
# shared grammar rule, named by the same sentence in both readers, not from a
# parse failure. The pattern is matched against BOTH readers' messages.
grammar_sentence <- c(
  "an inline empty depends" = "depends: must open a block",
  "a dependency entry with sha256 first, as the FIRST entry" = "a depends entry opens with",
  "a dependency entry with sha256 first, as the SECOND entry" = "a depends entry opens with")
mp <- file.path(hrt, "MANIFEST.yaml")
writeLines(base_manifest, mp)
ok(is.null(tryCatch({ read_method_manifest(mp); NULL }, error = function(e) conditionMessage(e))) &&
     identical(py_problem(mp), "None"),
   "H/positive: the baseline fixture manifest is accepted by both readers")
for (nm in names(variants)) {
  writeLines(variants[[nm]], mp)
  r_err <- tryCatch({ read_method_manifest(mp); NA_character_ }, error = function(e) conditionMessage(e))
  p_err <- py_problem(mp)
  if (is.na(r_err) || identical(p_err, "None"))
    cat(sprintf("      got: R=%s | guard=%s\n", r_err, p_err))
  ok(!is.na(r_err) && !identical(p_err, "None"),
     sprintf("H: %s is refused by BOTH readers", nm))
  if (nm %in% names(grammar_sentence)) {
    pat <- grammar_sentence[[nm]]
    if (!py_parses(mp)) cat("      got: the fixture does not parse as YAML\n")
    ok(py_parses(mp),
       sprintf("H: the `%s` fixture is well-formed YAML, so the refusal is the grammar rule", nm))
    if (!(!is.na(r_err) && grepl(pat, r_err, fixed = TRUE) &&
          grepl(pat, p_err, fixed = TRUE)))
      cat(sprintf("      got: R=%s | guard=%s\n", r_err, p_err))
    ok(!is.na(r_err) && grepl(pat, r_err, fixed = TRUE) && grepl(pat, p_err, fixed = TRUE),
       sprintf("H: both readers refuse `%s` with the grammar sentence \"%s\"", nm, pat))
  }
}
# baseline-fit (2026-11-14): the eleventh shipped manifest is a draft unit.
# The eleven shipped manifests still validate under both.
shipped <- sort(Sys.glob("skills/*/MANIFEST.yaml"))
r_ok <- sum(vapply(shipped, function(p) is.na(tryCatch({ read_method_manifest(p); NA_character_ },
                                                      error = function(e) conditionMessage(e))), logical(1)))
p_ok <- sum(vapply(shipped, function(p) identical(py_problem(p), "None"), logical(1)))
ok(length(shipped) == 11L && r_ok == 11L && p_ok == 11L,
   sprintf("H: the eleven shipped manifests are accepted by both readers (R %d/11, guard %d/11)", r_ok, p_ok))
setwd(old_wd)

cat("\n--- G: R13-P2-3, parse (not grep) for direct reference loads ---------------\n")
setwd(old_wd)
ci <- new.env(parent = globalenv())
suppressWarnings(suppressMessages(sys.source(file.path(TOOLS, "ci_check.R"), envir = ci, keep.source = FALSE)))
grt <- tempfile("b14ci"); dir.create(file.path(grt, "pipeline", "tasks"), recursive = TRUE)
run_check <- function(body) {
  writeLines(body, file.path(grt, "pipeline", "tasks", "t.R"))
  out <- utils::capture.output(ci$check_reference_loading(grt))
  paste(out, collapse = "\n")
}
REF <- '"skills/diagnostics/reference/gof.R"'
cases <- list(
  "one line"        = sprintf("source(%s, local = TRUE)", REF),
  "multiline"       = sprintf("source(\n  %s,\n  local = TRUE\n)", REF),
  "space before ("  = sprintf("source (%s)", REF),
  "named argument"  = sprintf("source(local = TRUE, file = %s)", REF),
  "sys.source"      = sprintf("sys.source(%s, envir = new.env())", REF),
  "base:: qualified"= sprintf("base::source(%s)", REF),
  "file.path"       = 'source(file.path("skills", "diagnostics", "reference", "gof.R"))',
  "local symbol"    = sprintf("p <- %s\nsource(p, local = TRUE)", REF))
for (nm in names(cases)) {
  out <- run_check(cases[[nm]])
  if (!grepl("FAIL", out)) cat(sprintf("      got: %s\n", out))
  ok(grepl("FAIL", out), sprintf("G: a direct reference load written as %s FAILS", nm))
}
ok(grepl("t\\.R:", run_check(cases[["multiline"]])),
   "G: the refusal names the file and a line number")
clean <- run_check(c('env <- load_reference("skills/diagnostics",',
                     '                      "skills/diagnostics/reference/gof.R")',
                     'source("tools/seed.R", local = TRUE)'))
ok(grepl("PASS", clean) && !grepl("FAIL", clean),
   "G/positive: load_reference() and a non-reference source() are accepted")
setwd(old_wd)

cat("\n--- F: R13-P1-2, one record snapshot per ledger event (D14-1) --------------\n")
setwd(old_wd)
ag <- new.env(parent = globalenv())
suppressWarnings(suppressMessages({
  sys.source(file.path(TOOLS, "gate_package.R"), envir = ag, keep.source = FALSE)
  sys.source(file.path(TOOLS, "gate.R"), envir = ag, keep.source = FALSE)
  sys.source(file.path(TOOLS, "approve_gate.R"), envir = ag, keep.source = FALSE)
}))
gw <- tempfile("b14gate"); dir.create(file.path(gw, "gates", "TST-014"), recursive = TRUE)
setwd(gw)
writeLines("hello", "a.csv")
arts <- data.frame(artifact = "t1", path = "a.csv", source = "ta", stringsAsFactors = FALSE)
tables <- list("Verdicts" = data.frame(criterion = "E1", value = 1.23, stringsAsFactors = FALSE))
grec <- file.path("gates", "TST-014", "eda.md")
writeLines(c("# Gate Sign-off Record", "", "Analysis ID: TST-014", "Stage: eda",
             "Date opened: 2026-09-05", "", "## SME decision", "",
             "- [ ] APPROVED — stage complete, next stage may start",
             "- [ ] APPROVED WITH CHANGES — changes listed below were applied and re-verified",
             "- [ ] REJECTED — returned to stage with instructions below", "",
             "Signed: <!-- SME name -->", "Date: <!-- -->", "Digest signed: <!-- paste -->"), grec)
ag$render_gate_package(grec, arts, tables, c("a note"))
rendered <- readLines(grec)
gsign <- function(lines, signer = "Signed: Fixture Reviewer", date = "Date: 2026-09-05") {
  dg <- sub("^Package-Digest: ", "", grep("^Package-Digest:", lines, value = TRUE)[1])
  i <- grep("^- \\[ \\] APPROVED —", lines)[1]
  lines[i] <- sub("\\[ \\]", "[x]", lines[i])
  lines[grep("^Digest signed:", lines)] <- paste("Digest signed:", dg)
  lines[grep("^Signed:", lines)] <- signer
  lines[grep("^Date:", lines)] <- date
  lines
}
gledger <- ag$.ledger_path(grec)
nlines <- function() if (file.exists(gledger)) length(readLines(gledger, warn = FALSE)) else 0L

ok(".after_snapshot" %in% names(formals(ag$approve_gate)),
   "F: approve_gate() takes a `.after_snapshot` injection point (test-only, run after the record snapshot is taken)")
ok(".after_snapshot" %in% names(formals(ag$revoke_gate)),
   "F: revoke_gate() takes a `.after_snapshot` injection point")
ok(".after_snapshot" %in% names(formals(ag$release_gate)),
   "F: release_gate() takes a `.after_snapshot` injection point")
ok(!("gate_file" %in% names(formals(ag$.build_event))) ||
     "record_digest" %in% names(formals(ag$.build_event)),
   "F: .build_event() is given the snapshot's digest rather than hashing the record again")
bsrc <- deparse(ag$.build_event)
ok(!any(grepl(".gate_sha256(gate_file)", bsrc, fixed = TRUE)),
   "F: .build_event() does not re-hash the gate file")

# APPROVE: the live record is rewritten after the snapshot is taken and
# validated. The approval must refuse, naming the drift, and append nothing.
writeLines(gsign(rendered), grec)
n0 <- nlines()
r <- tryCatch(suppressMessages(ag$approve_gate(grec, check_store = FALSE,
        .after_snapshot = function() writeLines(gsign(rendered, signer = "Signed: XX"), grec))),
      error = function(e) conditionMessage(e))
if (!is.character(r)) cat(sprintf("      got: %s\n", paste(class(r), collapse = " ")))
ok(is.character(r) && grepl("drift", r, ignore.case = TRUE),
   "F: an approval whose record drifts after the snapshot is REFUSED, naming the drift")
ok(nlines() == n0, "F: the refused approval appended no ledger event")
ok(!dir.exists(ag$.release_lock_path(gledger)), "F: the lock is released after the refusal")

# APPROVE, honest: the event's fields and digest come from the same snapshot.
writeLines(gsign(rendered), grec)
ev <- tryCatch(suppressMessages(ag$approve_gate(grec, check_store = FALSE)),
               error = function(e) conditionMessage(e))
ok(is.list(ev) && identical(ev$signer, "Fixture Reviewer") && identical(ev$record_digest, ag$.gate_sha256(grec)),
   "F/positive: an unchanged record is approved from one snapshot (signer and digest agree)")

# REVOKE: same protocol.
n0 <- nlines()
r <- tryCatch(suppressMessages(ag$revoke_gate(grec, reason = "test",
        .after_snapshot = function() writeLines(gsign(rendered, signer = "Signed: XX"), grec))),
      error = function(e) conditionMessage(e))
ok(is.character(r) && grepl("drift", r, ignore.case = TRUE),
   "F: a revocation whose record drifts after the snapshot is REFUSED")
ok(nlines() == n0, "F: the refused revocation appended no ledger event")

# RELEASE: same protocol; nothing archived either.
writeLines(gsign(rendered), grec)
n0 <- nlines()
hist_dir <- file.path(dirname(grec), "history")
r <- tryCatch(suppressMessages(ag$release_gate(grec, reason = "test", signer = "Fixture Reviewer",
        .after_snapshot = function() writeLines(gsign(rendered, signer = "Signed: XX"), grec))),
      error = function(e) conditionMessage(e))
ok(is.character(r) && grepl("drift", r, ignore.case = TRUE),
   "F: a release whose record drifts after the snapshot is REFUSED")
ok(nlines() == n0, "F: the refused release appended no ledger event")
ok(!dir.exists(hist_dir) || length(list.files(hist_dir)) == 0L,
   "F: the refused release archived nothing")
setwd(old_wd)

cat("\n--- E: R13-P1-3, literal gate arguments must name THIS skill and THIS file --\n")
setwd(old_wd)
ert <- tempfile("b14lit"); dir.create(ert)
dir.create(file.path(ert, "tools"))
file.copy(file.path(TOOLS, "method_manifest.R"), file.path(ert, "tools", "method_manifest.R"))
setwd(ert)
mk <- function(name, gate_skill, gate_file, status) {
  d <- file.path("skills", name)
  dir.create(file.path(d, "reference"), recursive = TRUE, showWarnings = FALSE)
  p <- file.path(d, "reference", "method.R")
  writeLines(c(
    '.mm_trusted_private <- base::local({',
    '  e <- base::new.env(parent = base::baseenv())',
    '  if (!base::file.exists("tools/method_manifest.R"))',
    '    base::stop("missing", call. = FALSE)',
    '  base::sys.source("tools/method_manifest.R", envir = e, keep.source = FALSE)',
    '  base::lockEnvironment(e, bindings = TRUE)',
    '  e',
    '}, envir = base::new.env(parent = base::baseenv()))',
    '.alpha_gate <- base::local({',
    '  private <- .mm_trusted_private',
    '  function() {',
    sprintf('    if (!base::get("require_validated_method", envir = private)(skill_dir = "%s", calling_file = "%s",',
            gate_skill, gate_file),
    '                                          unit = "alpha")) base::stop("no")',
    '    base::invisible(TRUE)',
    '  }',
    '}, envir = base::new.env())',
    'ep <- function(x) { .alpha_gate(); x }'), p)
  writeLines(c("# fixture", paste0("skill: ", name), "generated_at: 2026-09-05T00:00:00Z",
               'reviewed_by: "Fixture Reviewer"', 'reviewed_at: "2026-09-01"', "units:", "  alpha:",
               paste0("    status: ", status), paste0("    reference_path: ", p),
               "    entrypoints: [ep]", paste0("    sha256: ", .mm_file_sha256(p)),
               "    evidence: SKILL.md", '    reason: "fixture"'),
             file.path(d, "MANIFEST.yaml"))
  list(dir = d, path = p)
}
good <- mk("e_approved", "skills/e_approved", "skills/e_approved/reference/method.R", "approved")
gated_of <- function(d) {
  r <- tryCatch(suppressWarnings(audit_entrypoint_gating(d)), error = function(e) conditionMessage(e))
  if (is.character(r)) list(gated = FALSE, reason = r) else list(gated = isTRUE(r$gated[1]), reason = r$reason[1])
}
g1 <- gated_of(good$dir)
ok(isTRUE(g1$gated), "E/positive: a gate naming its own skill and its own file is gated")
# The same unit name in a second, DRAFT skill, whose gate literals point at the
# approved skill's manifest and file.
bad <- mk("e_draft", "skills/e_approved", "skills/e_approved/reference/method.R", "draft")
g2 <- gated_of(bad$dir)
if (isTRUE(g2$gated)) cat("      got: gated with reason ", g2$reason, "\n")
ok(!isTRUE(g2$gated) && grepl("R13-P1-3", g2$reason, fixed = TRUE),
   "E: a gate whose skill_dir literal names another skill is REFUSED")
bad2 <- mk("e_draft2", "skills/e_draft2", "skills/e_approved/reference/method.R", "draft")
g3 <- gated_of(bad2$dir)
ok(!isTRUE(g3$gated) && grepl("calling_file", g3$reason, fixed = TRUE),
   "E: a gate whose calling_file literal names another file is REFUSED")
setwd(old_wd)

cat("\n--- D: R13-P3-1 and R13-P2-1, the two reference censuses -------------------\n")
setwd(REPO)
all_refs <- sort(Sys.glob("skills/*/reference/*.R"))
PLACEHOLDER <- "skills/covariate-priors/reference/covariate_search.R"
sealed <- setdiff(all_refs, PLACEHOLDER)
# B0u-d (2026-09-27, diagnostics fixture): three diagnostics
# references were added (vpc_strata.R, bootstrap_agreement.R, npde_panels.R).
# AC12 (2026-10-26, bound-comparison coverage):
# one simulation-standards reference was added (bound_comparison.R) and the
# simulation-standards manifest gained 1 entry point.
# baseline-fit (2026-11-14): one sealed draft reference was added.
ok(length(all_refs) == 26L && length(sealed) == 25L,
   sprintf("D: the census covers the 25 sealed references and the 1 placeholder (%d found)",
           length(all_refs)))
# A DEFINED, CASE-INSENSITIVE CONVENTION (integrity batch 15, Codex round 14
# R14-P3-1). The census matched the literals "STATUS:" and "SME-VALIDATED", so a
# header written `# Status: SME validated` escaped both and a reference could
# claim its own approval state in prose. The forbidden forms are now the word
# `status` followed by a colon, and the words `sme validated` however spelled,
# matched without regard to case, hyphenation or spacing.
# The convention is a HEADER line: a comment whose content opens with the word
# `status` and a colon, or with an SME-validated claim (with or without that
# `status:` prefix). Prose elsewhere in a comment, and the refusal messages the
# gates carry in their bodies, are not claims and are not matched.
STATUS_CLAIM_RE <- "^[[:space:]]*#[[:space:]]*status[[:space:]]*:"
SME_CLAIM_RE    <- "^[[:space:]]*#[[:space:]]*(status[[:space:]]*:[[:space:]]*)?sme[-[:space:]]*validated"
claims_status <- function(f) any(grepl(STATUS_CLAIM_RE, readLines(f, warn = FALSE),
                                       ignore.case = TRUE))
claims_sme <- function(f) any(grepl(SME_CLAIM_RE, readLines(f, warn = FALSE),
                                    ignore.case = TRUE))
status_files <- all_refs[vapply(all_refs, claims_status, logical(1))]
ok(identical(status_files, "skills/covariate-priors/reference/covariate_search.R"),
   sprintf("D: only the covariate_search placeholder states a status: header, in any case (%s)",
           paste(basename(status_files), collapse = ", ")))
ok(!any(vapply(sealed, claims_sme, logical(1))),
   "D: no reference header claims SME validated, in any case or spacing; the manifest is the authority")
# The forbidden variants, on fixtures: the literal spellings the census used to
# look for and the ones that escaped it.
for (variant in c("# STATUS: SME-VALIDATED", "# Status: SME validated",
                  "#  status : sme-validated", "# status: draft",
                  "# STATUS: PLACEHOLDER, NOT WRITTEN")) {
  ftmp <- tempfile(fileext = ".R"); writeLines(c(variant, "f <- function() 1"), ftmp)
  ok(claims_status(ftmp),
     sprintf("D: the census sees a status header written `%s`", variant))
  unlink(ftmp)
}
for (variant in c("# SME-VALIDATED", "# SME validated", "# sme  validated",
                  "# Status: SME validated")) {
  ftmp <- tempfile(fileext = ".R"); writeLines(c(variant, "f <- function() 1"), ftmp)
  ok(claims_sme(ftmp),
     sprintf("D: the census sees an SME-validated claim written `%s`", variant))
  unlink(ftmp)
}
# Every source() inside a sealed reference is local = TRUE, so the helpers it
# brings in land in the calling frame rather than in the global environment,
# which a base-parented loader environment cannot reach (D14-2).
bad_source <- character(0)
for (f in sealed) {
  ln <- readLines(f, warn = FALSE); ln <- ln[!grepl("^\\s*#", ln)]
  for (h in grep("(^|[^a-zA-Z0-9._])source\\(", ln, value = TRUE))
    if (!grepl("local = TRUE", h, fixed = TRUE)) bad_source <- c(bad_source, paste0(basename(f), ": ", trimws(h)))
}
ok(length(bad_source) == 0L,
   sprintf("D: every source() in a sealed reference carries local = TRUE (%d violations)", length(bad_source)))
if (length(bad_source)) for (b in bad_source) cat("      ", b, "\n")
# The unqualified non-base call-head census. A head that base cannot resolve
# and the file does not bind is unavailable when the reference is loaded
# through the default loader environment.
EXEMPT <- c(".", "harness_seed", "harness_is_observation", "load_engine_config",
            "write_r_packages_used")
heads <- new.env(parent = emptyenv()); bound <- new.env(parent = emptyenv())
note <- function(env, k) assign(k, TRUE, envir = env)
walk <- function(e, file) {
  if (missing(e)) return(invisible(NULL))
  if (is.name(e) && !nzchar(as.character(e))) return(invisible(NULL))
  if (is.call(e)) {
    h <- e[[1L]]
    if (is.name(h)) {
      nm <- as.character(h)
      if (!exists(nm, envir = baseenv(), inherits = FALSE)) note(heads, paste0(file, "\037", nm))
      if (nm %in% c("<-", "=", "<<-") && length(e) >= 3L && is.name(e[[2L]]))
        note(bound, paste0(file, "\037", as.character(e[[2L]])))
      if (nm == "function" && length(e) >= 2L && is.pairlist(e[[2L]]))
        for (fnm in names(e[[2L]])) note(bound, paste0(file, "\037", fnm))
    }
  }
  if (is.call(e) || is.pairlist(e) || is.expression(e))
    for (i in seq_along(e)) walk(e[[i]], file)
}
for (f in sealed) walk(parse(f, keep.source = FALSE), f)
keys <- setdiff(ls(heads, all.names = TRUE), ls(bound, all.names = TRUE))
residual <- sort(unique(setdiff(sub("^.*\037", "", keys), EXEMPT)))
ok(length(residual) == 0L,
   sprintf("D: no unqualified non-base call head remains in the 20 references (%s)",
           if (length(residual)) paste(residual, collapse = ", ") else "0 residual"))
# The same census applied to the loader itself (integrity batch 14, R13-P0-2).
# tools/load_reference.R is not a manifested reference and has no gate chain,
# so the audit's gate-head census does not reach it; the call-head census does,
# and the loader is constructed in a base-parented enclosure, so any head base
# cannot resolve would be a defect exactly as it is in a reference.
lheads <- new.env(parent = emptyenv()); lbound <- new.env(parent = emptyenv())
heads <- lheads; bound <- lbound
walk(parse(file.path(TOOLS, "load_reference.R"), keep.source = FALSE), "loader")
lres <- sort(setdiff(sub("^.*\037", "", setdiff(ls(lheads, all.names = TRUE),
                                                 ls(lbound, all.names = TRUE))), EXEMPT))
ok(length(lres) == 0L,
   sprintf("D: tools/load_reference.R has no unqualified non-base call head either (%s)",
           if (length(lres)) paste(lres, collapse = ", ") else "0 residual"))
setwd(old_wd)

# No definition in eda.R re-reads and re-executes the reference by PATH. The
# construction block at the top of the file sources its declared dependencies
# once, before the entry points exist; everything after the inventory works from
# the one authenticated byte snapshot. `.eda_disk_defs_digest()` was the last
# definition that digested the file by path and then sys.source()d it with no
# manifest check, and it is gone (fix round 1, observation O-3).
eda_lines <- readLines(file.path(REPO, "skills", "eda-standards", "reference", "eda.R"))
inv_line <- grep("^EDA_ENTRYPOINTS <- base::c\\(", eda_lines)[1L]
late_sys_source <- grep("sys\\.source\\(", eda_lines)
late_sys_source <- late_sys_source[late_sys_source > inv_line &
                                     !grepl("^\\s*#", eda_lines[late_sys_source])]
if (length(late_sys_source))
  cat(sprintf("      got: sys.source at line(s) %s\n", paste(late_sys_source, collapse = ", ")))
ok(length(late_sys_source) == 0L,
   "D/O-3: no sys.source() of the reference path survives below the inventory in eda.R")
dd <- grep("\\.eda_disk_defs_digest", eda_lines)
if (length(dd)) cat(sprintf("      got: .eda_disk_defs_digest at line(s) %s\n",
                            paste(dd, collapse = ", ")))
ok(length(dd) == 0L,
   "D/O-3: the dead unauthenticated `.eda_disk_defs_digest` and its inventory entry are removed together")

# O-5. Under the isolated default environment an S3 method the CALLER defined in
# its own global environment is not dispatched: the reference's frame does not
# reach globalenv(), and UseMethod() looks in the calling frame and the defining
# function's environment, neither of which is the caller's globalenv here. This
# is a property of D14-2's isolation, not of the `stats::` qualification, and the
# loader's comment block must say so rather than read as if qualification alone
# makes every call work.
o5_env <- new.env(parent = baseenv())
assign("vcov.b14o5", function(object, ...) "CALLER METHOD RAN", envir = globalenv())
o5_obj <- structure(list(), class = "b14o5")
assign("probe_o5", eval(parse(text = "function(x) stats::vcov(x)"), envir = o5_env), envir = o5_env)
o5 <- tryCatch(o5_env$probe_o5(o5_obj), error = function(e) conditionMessage(e))
if (!grepl("no applicable method", o5, fixed = TRUE)) cat(sprintf("      got: %s\n", o5))
ok(is.character(o5) && grepl("no applicable method", o5, fixed = TRUE),
   "D/O-5: an S3 method living only in the caller's globalenv is NOT dispatched from a base-parented frame")
rm("vcov.b14o5", envir = globalenv())
# ANCHORED ON TEXT, NOT ON LINE NUMBERS (integrity batch 15, queue O-12). This
# pin read lines [40:110]; any edit above the block silently moved the pin off
# the text it was meant to hold, so the assertion could pass or fail for reasons
# unrelated to the comment. The whole file is searched instead -- the claim is
# that the loader RECORDS this consequence somewhere in its contract, and the
# two phrases together are specific enough that only that block satisfies them.
loader_block <- paste(readLines(file.path(TOOLS, "load_reference.R")), collapse = "\n")
ok(grepl("no applicable method", loader_block, fixed = TRUE) &&
     grepl("S3 method", loader_block, fixed = TRUE),
   "D/O-5: the loader's comment block records that caller-defined S3 methods are not dispatched")

cat("\n--- B: R13-P1-1, no re-execution of unauthenticated bytes in a gated call ---\n")

# The reference is loaded honestly, then the file on disk gains a top-level
# side effect. The next gated call must refuse on the digest the manifest binds
# and must never evaluate the changed bytes.
setwd(old_wd); rt <- eda_fixture(); setwd(rt)
E3 <- new.env(parent = baseenv())
r3a <- tryCatch({ load_reference("skills/eda-standards", EDA_REF, envir = E3)
                  E3$reference_eda_rowset_digest(VIEW, ROWS) },
                error = function(e) conditionMessage(e))
ok(is.character(r3a) && grepl("^sha256:", r3a),
   "B/positive: the honest fixture computes before the file is changed")
cat('base::file.create("MARKER_S3")\n', file = EDA_REF, append = TRUE)
r3 <- tryCatch(E3$reference_eda_rowset_digest(VIEW, ROWS), error = function(e) conditionMessage(e))
if (!is.character(r3) || !grepl("ESCALATE", r3)) cat(sprintf("      got: %s\n", paste(r3, collapse = " ")))
ok(is.character(r3) && grepl("are not the bytes", r3, fixed = TRUE) && grepl("ESCALATE", r3),
   "B: a gated call REFUSES bytes the manifest does not bind, naming both digests")
ok(!file.exists("MARKER_S3"),
   "B: the appended top-level side effect never ran (no marker)")

# The refusal was the manifest digest, not merely "the file changed": reseal
# the fixture manifest to the changed bytes and the same call proceeds, which
# is what shows the authentication -- and not some incidental failure -- is
# what stopped the previous call. The marker appears only now, when the bytes
# carrying it are the bytes the manifest binds.
eda_reseal(rt)
r4 <- tryCatch(E3$reference_eda_rowset_digest(VIEW, ROWS), error = function(e) conditionMessage(e))
ok(is.character(r4) && grepl("^sha256:", r4),
   "B: with the manifest resealed on the changed bytes, the same call computes")
ok(file.exists("MARKER_S3"),
   "B: the changed top level ran only once its digest was authenticated (marker now present)")

# Static: the identity closure never sys.source()s the reference path again.
srcid <- eda_src[grep("^\\.eda_source_identity <- base::local", eda_src):
                 (grep("^\\.eda_srcid_env <- base::local", eda_src)[1] - 1L)]
srcid_code <- srcid[!grepl("^\\s*#", srcid)]
gate_blk <- eda_src[grep("^\\.eda_gate <- base::local", eda_src):length(eda_src)]
gate_blk <- gate_blk[seq_len(grep("^\\}, envir", gate_blk)[1])]
gate_code <- gate_blk[!grepl("^\\s*#", gate_blk)]
# The pin names CALLS, not the bare name: since integrity batch 15 the closure
# carries a syntactic walker whose refusal list contains the STRING
# "sys.source", which is the opposite of sourcing the path.
ok(!any(grepl("sys.source(", srcid_code, fixed = TRUE)) &&
     !any(grepl("readBin(", srcid_code, fixed = TRUE)) &&
     any(grepl("base::readBin(ref_file", gate_code, fixed = TRUE)) &&
     any(grepl('base::get("srcid", envir = srcid_env)(disk_bytes, disk_key)', gate_code, fixed = TRUE)),
   "B: the gate reads the bytes once and hands that snapshot to the identity check, which never sys.source()s the path")
setwd(old_wd)

# The inventories are built with base::c() so a rebound `c` cannot reach them.
ok(any(grepl("^EDA_ENTRYPOINTS <- base::c\\(", eda_src)) &&
     any(grepl("^EDA_INTERNALS <- base::c\\(", eda_src)),
   "A: both inventory vectors are constructed with base::c()")
setwd(old_wd)

cat("\n--- K: every sealed reference loads and runs through the DEFAULT loader (O-4) --\n")

# Item 14.6 / D14-2. The supported interface is `load_reference()` with `envir`
# omitted: a fresh environment whose parent is baseenv(). A reference that leaves
# a non-base call unqualified loads fine and only fails when the call is reached,
# so loading is not enough -- every declared entry point of every sealed
# reference is CALLED here, and any "could not find function" is a failure.
#
# The fixture is a disposable copy of the repository whose manifests are resealed
# on the copied bytes and approved, so the gates compute rather than refusing on
# the live tree's signature state (which resets to draft on every regeneration).
# A gate refusal is therefore itself a failure here and is reported as such, not
# swallowed.
k_fixture <- function() {
  rt <- tempfile("b14loadall"); dir.create(rt)
  st <- system2("rsync", c("-a", "--exclude", ".git", "--exclude", "_targets_*",
                           "--exclude", "harness.yaml", "--exclude", ".harness-unlock",
                           "--exclude", "scratchpad", paste0(REPO, "/"), paste0(rt, "/")))
  if (!identical(st, 0L)) stop("could not build the load-all fixture (rsync exit ", st, ")")
  for (mp in Sys.glob(file.path(rt, "skills", "*", "MANIFEST.yaml"))) {
    ln <- readLines(mp, warn = FALSE); ref <- NA_character_; dep <- NA_character_
    for (i in seq_along(ln)) {
      if (grepl("^    reference_path:", ln[i])) ref <- trimws(sub("^    reference_path:", "", ln[i]))
      if (grepl("^      - path:", ln[i])) dep <- trimws(sub("^      - path:", "", ln[i]))
      if (grepl("^    sha256:", ln[i]) && !is.na(ref))
        ln[i] <- paste0("    sha256: ", .mm_file_sha256(file.path(rt, ref)))
      if (grepl("^        sha256:", ln[i]) && !is.na(dep))
        ln[i] <- paste0("        sha256: ", .mm_file_sha256(file.path(rt, dep)))
    }
    ln[grepl("^reviewed_by:", ln)] <- 'reviewed_by: "Fixture Reviewer"'
    ln[grepl("^reviewed_at:", ln)] <- 'reviewed_at: "2026-09-05"'
    # THE PLACEHOLDER STAYS DRAFT (integrity batch 15, Codex round 14 R14-P2-2).
    # This fixture used to approve EVERY unit, including the documented
    # covariate_search placeholder. validate_method_manifest() refuses an
    # approved placeholder and validation covers the WHOLE manifest, so all four
    # of covariate-priors' real references then failed on that defect -- and the
    # call battery below, which recorded an error only when it matched "could
    # not find function" or "ESCALATE", swallowed the failure and still printed
    # its claim. Only the units this battery intends to exercise are approved.
    placeholder <- rep(FALSE, length(ln))
    unit_start <- grep("^  [^ ].*:$", ln)
    for (u in unit_start) {
      nxt <- unit_start[unit_start > u]
      stop_at <- if (length(nxt)) nxt[1] - 1L else length(ln)
      blk <- seq.int(u, stop_at)
      if (any(grepl("covariate_search\\.R", ln[blk]))) placeholder[blk] <- TRUE
    }
    ln[grepl("^    status:", ln) & !placeholder] <- "    status: approved"
    writeLines(ln, mp)
  }
  # baseline-fit (2026-11-14): its dependency is another skill's manifest.
  # Bind that dependency only after all fixture approvals and reseals finish.
  for (mp in Sys.glob(file.path(rt, "skills", "*", "MANIFEST.yaml"))) {
    ln <- readLines(mp, warn = FALSE); dep <- NA_character_
    for (i in seq_along(ln)) {
      if (grepl("^      - path:", ln[i])) dep <- trimws(sub("^      - path:", "", ln[i]))
      if (grepl("^        sha256:", ln[i]) && !is.na(dep) &&
          grepl("/MANIFEST[.]yaml$", dep))
        ln[i] <- paste0("        sha256: ", .mm_file_sha256(file.path(rt, dep)))
    }
    writeLines(ln, mp)
  }
  rt
}
krt <- k_fixture()
setwd(krt)
# OBSERVE THE GATE, DO NOT INFER IT (integrity batch 16, Codex round 15
# R15-P2-5). The accepted-error inventory below is kept -- it is still a useful
# hit detector -- but it is no longer the evidence that a gate ran and passed.
# require_validated_method() appends one line per PASSED gate to the file named
# by MM_GATE_OBSERVE (test-only: inert unless TEST_REPO_ROOT is also set), and
# the assertion after the loop requires an observed pass for every approved
# entry point the battery exercised.
k_obs <- tempfile("b16gateobs")
Sys.setenv(TEST_REPO_ROOT = krt, MM_GATE_OBSERVE = k_obs)
on.exit(Sys.unsetenv(c("TEST_REPO_ROOT", "MM_GATE_OBSERVE")), add = TRUE)
k_seen <- character(0)
# THE OBSERVATION MUST NAME THE ENTRY POINT UNDER TEST (integrity batch 17,
# Codex round 16 R16-P2-2). Round 16's rule was "the observer file grew", which
# credits the exercised entry point for a gate some OTHER unit passed: a
# wrapper whose prologue calls another approved unit and then raises an accepted
# input error before reaching its own gate was recorded as having passed it.
# The records carry `<reference_path>|<unit>`; crediting now requires that pair.
k_new_records <- function(before, after)
  if (length(after) > length(before)) after[(length(before) + 1L):length(after)] else character(0)
k_credited <- function(new_records, rp, units)
  length(units) > 0L && any(new_records %in% paste0(rp, "|", units))
k_loader_env <- new.env(parent = globalenv())
sys.source(file.path(krt, "tools", "load_reference.R"), envir = k_loader_env, keep.source = FALSE)
k_load <- get("load_reference", envir = k_loader_env)
# Two argument sets per entry point: no arguments at all, and a small data frame
# with a column name. Both reach the body past R's own argument matching, which
# is where an unqualified head would be resolved.
K_ARGS <- list(list(), list(data.frame(a = 1), "a"))
# The refusals a shallow call is EXPECTED to produce: R's own argument matching
# and the references' own declared input contracts. Every one of them is
# evidence that the entry point's body was reached -- which means the gate at
# the top of that body ran and PASSED. Anything outside this list is a hit.
K_ARG_VALIDATION <- c(
  # R's own argument matching.
  "argument \"[^\"]+\" is missing, with no default",
  "unused argument",
  # stopifnot() input contracts.
  "is not TRUE$",
  # The references' own declared input contracts, and R's own errors on the
  # deliberately invalid inputs this shallow battery supplies -- named one by
  # one so that a message NOT on this list is a hit. This is an INVENTORY of
  # what a no-argument / one-column-frame call currently reaches, not an
  # endorsement of each message: several below are R complaining about junk
  # input rather than a reference validating it.
  #
  # WHAT THIS SET DOES AND DOES NOT CLOSE (integrity batch 15 fix round 1,
  # review blocker B1). It closes the "any error" hole: a message outside it is
  # a hit, so a malformed-manifest refusal or an unrelated defect can no longer
  # pass unremarked. It does NOT and CANNOT by itself keep an unresolved name
  # out, because several of these messages are OUTER wrappers whose inner cause
  # is arbitrary -- `error in evaluating the argument 'x' in selecting a method
  # for function 'as.data.frame': could not find function "mrgsim"` matches an
  # accepted pattern and hides exactly the failure item 15.6 exists to catch.
  # The two decisive tokens are therefore tested SEPARATELY and unconditionally,
  # in K_ALWAYS_HIT below; this list can never override them.
  "a does not have a .ext extension",
  "auc_tolerance must be declared as",
  "column PRED is required in the data set",
  "design is missing column\\(s\\)",
  # Narrowed to the inner message it was written for: as an outer wrapper it
  # matched any cause at all, including an unresolved name (blocker B1).
  "unable to find an inherited method for function .zero_re.",
  "In argument: `PARAMETER %in% parameters`",
  "incorrect number of dimensions",
  "Individual-profile panel needs column\\(s\\)",
  "invalid 'file' argument",
  "invalid filename argument",
  "llp needs an explicit parameter selection",
  "Must use existing variables",
  "no applicable method for '(qpcstats|model_summary)'",
  "ofv_parent and ofv_child must each be a single number",
  "p_flag is a declared analysis-plan parameter with no default",
  "probs must be declared as",
  "reference_psn_(npc|sse|vpc): seed is required",
  "reference_vpc_prepare: the obs data has no 'EVID' column",
  "scm config file not found",
  "sim is missing column\\(s\\)",
  "subscript out of bounds",
  "the bbi executable was not found",
  "the model file a.cpp does not exist",
  "the VPC stats table is missing column\\(s\\)",
  "values must be a non-empty finite numeric vector",
  "wt_low must be a single positive number",
  # B0u-d (2026-09-27): the input contracts of the three new diagnostics
  # references and of reference_psn_bootstrap_samples, as a no-argument or
  # one-column-frame call reaches them; the last is R's read.csv on a frame.
  "fit_data must be a data.frame with ID and WT columns",
  "must be a non-empty vector of finite numbers",
  "n_samples must be a single positive whole number",
  "success must be a logical vector without missing values",
  "interval bounds must be numeric",
  "ci is the declared interval level",
  "^x must be numeric",
  "seed is mandatory: the engine run's seed is recorded",
  "'file' must be a character string or connection",
  # AC12 (2026-10-26, bound-comparison coverage, plan D-1): the input contract
  # of reference_bound_verdict as both shallow calls reach it (the no-argument
  # call names 'lower', the one-column-frame call names 'bound_lower').
  "^reference_bound_verdict: argument '[a-z_]+' is missing; it has no default\\.$",
  # baseline-fit (2026-11-14): the one-column-frame call reaches its identity
  # input contract after its own baseline gate has passed (observed below).
  "^baseline-fit: invalid identity fields$")
# ALWAYS A HIT, WHATEVER K_ARG_VALIDATION SAYS (integrity batch 15 fix round 1,
# review blocker B1). An unresolved name and a gate/integrity refusal are the
# two outcomes this battery exists to detect. They are matched anywhere in the
# message -- including inside an outer wrapper raised while evaluating an
# argument to a generic -- and the accepted-argument-validation set is not
# consulted for them.
K_ALWAYS_HIT <- c("could not find function", "ESCALATE")
k_files <- 0L; k_eps <- 0L; k_hits <- character(0)
k_exercised <- character(0); k_loadfail <- character(0)
for (mp in sort(Sys.glob("skills/*/MANIFEST.yaml"))) {
  km <- read_method_manifest(mp)
  for (rp in unique(vapply(km$units, function(u) u$reference_path, character(1)))) {
    if (!grepl("\\.R$", rp)) next          # the four .ctl reference models
    if (grepl("covariate_search\\.R$", rp)) next   # the documented placeholder
    ke <- tryCatch(k_load(dirname(mp), rp), error = function(err) conditionMessage(err))
    if (is.character(ke)) { k_loadfail <- c(k_loadfail, paste(rp, "::", substr(ke, 1, 120))); next }
    k_files <- k_files + 1L
    kunits <- km$units[vapply(km$units, function(u) identical(u$reference_path, rp), logical(1))]
    # WHICH UNIT DECLARES WHICH ENTRY POINT (integrity batch 17, Codex round 16
    # R16-P2-2). The observer records `<reference_path>|<unit>`; crediting an
    # entry point requires the pair for the unit that DECLARES it, so an
    # observation produced by some other unit's gate cannot stand in for it.
    kmap <- list()
    for (un in names(kunits))
      for (ep0 in kunits[[un]]$entrypoints) kmap[[ep0]] <- unique(c(kmap[[ep0]], un))
    keps <- names(kmap)
    # eda.R's gate re-digests every definition on each call; one argument set per
    # entry point is enough to reach every body there and keeps this section from
    # dominating the battery's running time.
    argsets <- if (grepl("eda\\.R$", rp)) K_ARGS[1] else K_ARGS
    for (ep in keps) {
      if (!exists(ep, envir = ke, inherits = FALSE)) {
        k_hits <- c(k_hits, paste(rp, ep, ":: declared entry point is not defined")); next
      }
      k_eps <- k_eps + 1L
      k_exercised <- c(k_exercised, paste0(rp, "::", ep))
      kf <- get(ep, envir = ke)
      for (a in argsets) {
        k_before <- if (file.exists(k_obs)) readLines(k_obs, warn = FALSE) else character(0)
        kmsg <- tryCatch({ do.call(kf, a); "" }, error = function(err) conditionMessage(err))
        if (nzchar(Sys.getenv("B15_K_DEBUG")) && nzchar(kmsg))
          cat(sprintf("      K-DEBUG %s %s :: %s\n", rp, ep, substr(kmsg, 1, 160)))
        # TWO INDEPENDENT TESTS (integrity batch 15, Codex round 14 R14-P2-2;
        # narrowed in fix round 1 for review blocker B1).
        #   1. An unresolved name or a refusal naming the SME is a hit
        #      unconditionally -- K_ARG_VALIDATION is not consulted, so no
        #      accepted pattern can mask one.
        #   2. Otherwise, any message outside the named accepted set is a hit.
        #      The battery calls each entry point with no arguments and with a
        #      one-column data frame, so the expected outcome is success or one
        #      of the named refusals.
        always <- nzchar(kmsg) && any(vapply(K_ALWAYS_HIT, function(tok)
          grepl(tok, kmsg, fixed = TRUE), logical(1)))
        if (always || (nzchar(kmsg) && !any(vapply(K_ARG_VALIDATION, function(pat)
              grepl(pat, kmsg), logical(1)))))
          k_hits <- c(k_hits, paste(rp, ep, "::", substr(kmsg, 1, 160)))
        k_after <- if (file.exists(k_obs)) readLines(k_obs, warn = FALSE) else character(0)
        if (k_credited(k_new_records(k_before, k_after), rp, kmap[[ep]]))
          k_seen <- c(k_seen, paste0(rp, "::", ep))
      }
    }
  }
}
setwd(old_wd)
Sys.unsetenv(c("TEST_REPO_ROOT", "MM_GATE_OBSERVE"))
k_seen <- unique(k_seen)
k_exercised <- unique(k_exercised)
k_unobserved <- setdiff(k_exercised, k_seen)
if (length(k_unobserved))
  cat(sprintf("      no observed gate pass for: %s\n",
              paste(k_unobserved, collapse = " | ")))
ok(length(k_exercised) > 0L && length(k_unobserved) == 0L,
   sprintf("K: every one of the %d exercised approved entry points was observed returning from a PASSED gate (R15-P2-5)",
           length(k_exercised)))
if (length(k_loadfail)) cat(sprintf("      got: %s\n", paste(k_loadfail, collapse = " | ")))
ok(length(k_loadfail) == 0L,
   sprintf("K: all sealed R references load through the DEFAULT loader (%d loaded, %d failed)",
           k_files, length(k_loadfail)))
# AC12 (2026-10-26, bound-comparison coverage):
# one simulation-standards reference was added (bound_comparison.R) and the
# simulation-standards manifest gained 1 entry point.
# baseline-fit (2026-11-14): the default loader must also load the new draft.
ok(k_files == 25L,
   sprintf("K: the census covered all twenty-five sealed R references (%d)", k_files))
if (length(k_hits)) cat(sprintf("      got: %s\n", paste(k_hits, collapse = "\n      got: ")))
ok(length(k_hits) == 0L,
   sprintf("K: every declared entry point (%d) runs with no unresolved name and no gate refusal (%d hits)",
           k_eps, length(k_hits)))

# THE NEGATIVE FIXTURE FOR THE ATTRIBUTION RULE (integrity batch 17, Codex
# round 16 R16-P2-2). `reference_attr_wrap` belongs to unit_wrap. Its prologue
# calls `reference_attr_a`, which passes unit_a's gate, and then raises an error
# that is in the accepted set above -- so the wrapper's own gate is never
# reached. One observer record is appended and it names unit_a. The rule must
# not credit unit_wrap for it.
kn_rt <- tempfile("b17attr")
dir.create(file.path(kn_rt, "tools"), recursive = TRUE)
dir.create(file.path(kn_rt, "skills", "attrprobe", "reference"), recursive = TRUE)
file.copy(file.path(TOOLS, "method_manifest.R"), file.path(kn_rt, "tools"))
setwd(kn_rt)
KN <- new.env(parent = globalenv())
sys.source("tools/method_manifest.R", envir = KN, keep.source = FALSE)
kn_ref <- "skills/attrprobe/reference/attr.R"
writeLines(c(
  'reference_attr_a <- function() {',
  '  require_validated_method(skill_dir = "skills/attrprobe",',
  '    calling_file = "skills/attrprobe/reference/attr.R", unit = "unit_a")',
  '  invisible(TRUE)',
  '}',
  'reference_attr_wrap <- function(x) {',
  '  reference_attr_a()',
  '  if (missing(x)) stop("argument \\"x\\" is missing, with no default", call. = FALSE)',
  '  require_validated_method(skill_dir = "skills/attrprobe",',
  '    calling_file = "skills/attrprobe/reference/attr.R", unit = "unit_wrap")',
  '  invisible(TRUE)',
  '}'), kn_ref)
kn_sha <- KN$.mm_file_sha256(kn_ref)
writeLines(c("skill: attrprobe", "generated_at: 2026-09-06T00:00:00Z",
  'reviewed_by: "Fixture Reviewer"', 'reviewed_at: "2026-09-06"', "units:",
  "  unit_a:", "    status: approved", paste0("    reference_path: ", kn_ref),
  "    entrypoints: [reference_attr_a]", paste0("    sha256: ", kn_sha),
  "    evidence: skills/attrprobe/SKILL.md", '    reason: "fixture"',
  "  unit_wrap:", "    status: approved", paste0("    reference_path: ", kn_ref),
  "    entrypoints: [reference_attr_wrap]", paste0("    sha256: ", kn_sha),
  "    evidence: skills/attrprobe/SKILL.md", '    reason: "fixture"'),
  "skills/attrprobe/MANIFEST.yaml")
kn_obs <- tempfile("b17gateobs")
Sys.setenv(TEST_REPO_ROOT = kn_rt, MM_GATE_OBSERVE = kn_obs)
KE <- new.env(parent = globalenv())
assign("require_validated_method", KN$require_validated_method, envir = KE)
sys.source(kn_ref, envir = KE, keep.source = FALSE)
kn_before <- if (file.exists(kn_obs)) readLines(kn_obs, warn = FALSE) else character(0)
kn_msg <- tryCatch({ KE$reference_attr_wrap(); "" }, error = function(e) conditionMessage(e))
kn_after <- if (file.exists(kn_obs)) readLines(kn_obs, warn = FALSE) else character(0)
Sys.unsetenv(c("TEST_REPO_ROOT", "MM_GATE_OBSERVE"))
kn_new <- k_new_records(kn_before, kn_after)
setwd(old_wd)
unlink(kn_rt, recursive = TRUE)
ok(any(vapply(K_ARG_VALIDATION, function(p) grepl(p, kn_msg), logical(1))),
   "K-neg: the wrapper's error is one this battery accepts, so the hit detector stays silent")
ok(identical(kn_new, paste0(kn_ref, "|unit_a")),
   "K-neg: exactly one observer record was appended and it names the OTHER unit")
ok(!k_credited(kn_new, kn_ref, "unit_wrap"),
   "K-neg: the attribution rule does NOT credit unit_wrap for unit_a's gate pass (R16-P2-2)")
ok(k_credited(kn_new, kn_ref, "unit_a"),
   "K-neg: the same rule DOES credit unit_a, whose gate actually passed")

# The calls above stop at each entry point's argument validation, which is
# shallow, so they do not reach the deep call sites this batch qualified. These
# do, with valid inputs, through the same default loader: `stats::quantile` and
# `stats::median` in the uncertainty summary, `stats::median` and `utils::head`
# in the eda continuous summary. Un-qualifying any of them turns these into
# "could not find function".
setwd(krt)
k_un <- k_load("skills/simulation-standards", "skills/simulation-standards/reference/uncertainty.R")
k_mk <- function(draw, id, tm, cp) data.frame(SCENARIO = "s", DRAW = draw, ID = id, time = tm, CP = cp)
k_rows <- do.call(rbind, list(k_mk(0,1,1,20), k_mk(1,1,1,10), k_mk(2,1,1,20), k_mk(3,1,1,30),
                              k_mk(0,1,2,200), k_mk(1,1,2,10), k_mk(2,1,2,20), k_mk(3,1,2,30)))
k_su <- tryCatch(k_un$reference_sim_full_summary(k_rows, observed = "CP",
                                                 iiv_probs = c(0.5, 0.9), unc_level = 0.5),
                 error = function(e) conditionMessage(e))
if (!is.data.frame(k_su)) cat(sprintf("      got: %s\n", substr(paste(k_su, collapse = " "), 1, 200)))
ok(is.data.frame(k_su) && all(c("unc_median", "unc_lower", "unc_upper") %in% names(k_su)),
   "K/valid: reference_sim_full_summary() computes through the default loader (the stats::quantile and stats::median sites run)")
k_ed <- k_load("skills/eda-standards", "skills/eda-standards/reference/eda.R")
# WT IS PRESENT ON EVERY RECORD AND CONSTANT WITHIN SUBJECT (integrity batch 15
# fix round 1, review blocker B2). The earlier frame left WT missing on the dose
# record, so `.eda_first_value` refused at eda.R:1028 and neither
# `stats::median` (:1049) nor `utils::head` (:1033) ever ran -- while the
# assertion claimed both sites were reached. This frame reaches the summary's
# own body; the varying frame below reaches the `utils::head` site.
k_d <- data.frame(ID = c(1,1,2,2,3,3), TIME = c(0,1,0,1,0,1), EVID = c(1,0,1,0,1,0),
                  DV = c(NA,1,NA,2,NA,3), WT = c(70,70,80,80,90,90), C = NA_character_,
                  stringsAsFactors = FALSE)
k_dv <- k_d; k_dv$WT <- c(70,71,80,81,90,91)   # WT varies within subject
k_v <- tryCatch(k_ed$reference_eda_view(k_d, "ID", "C", "EVID", 0, 1), error = function(e) conditionMessage(e))
k_vv <- tryCatch(k_ed$reference_eda_view(k_dv, "ID", "C", "EVID", 0, 1), error = function(e) conditionMessage(e))
# The call is spelled to the declared signature (view, columns, rows, digits).
# It used to pass a list and a named vector into `rows` and `digits`, so it
# stopped at `match.arg()` with "'arg' must be NULL or a character vector" and
# never reached the stats::median / utils::head sites this assertion claims to
# exercise -- which the assertion's own vacuous acceptance hid (integrity batch
# 15, Codex round 14 R14-P2-2).
k_cs <- if (is.list(k_v))
  tryCatch(k_ed$reference_eda_continuous_summary(k_v, "WT", rows = "subjects"),
           error = function(e) conditionMessage(e)) else "the view did not build"
# The fixture data deliberately carries a covariate that is absent on the first
# kept record of every subject, which is the utils::head first-value guard: the
# reference REFUSES with its own message. That refusal is the guard running, and
# it is asserted as such -- what must never appear is a name-lookup failure.
# THE ACCEPTED OUTCOME IS NAMED, AND THE VALUE IS ASSERTED (integrity batch 15,
# Codex round 14 R14-P2-2; tightened in fix round 1 for review blocker B2).
# This assertion used to accept ANY character result that did not say "could not
# find function" -- including its own fallback "the view did not build" -- and
# then, after the first tightening, still accepted the first-value refusal,
# which stops BEFORE the two sites the assertion names. The summary must now
# RETURN, and the returned median must be the value stats::median computes for
# the fixture (the three subject weights 70, 80, 90 -> 80); un-qualifying
# `stats::median` in the reference turns this into "could not find function".
if (!is.list(k_cs)) cat(sprintf("      got: %s\n", substr(paste(k_cs, collapse = " "), 1, 200)))
ok(is.list(k_v) && !identical(k_cs, "the view did not build"),
   "K/valid: the eda view builds, so the continuous summary is actually reached")
k_tab <- if (is.list(k_cs) && !is.data.frame(k_cs)) k_cs[[1]] else k_cs
ok(is.data.frame(k_tab) && identical(k_tab$variable, "WT") &&
     identical(k_tab$n, 3L) && identical(k_tab$median, 80) &&
     identical(k_tab$min, 70) && identical(k_tab$max, 90),
   "K/valid: reference_eda_continuous_summary() RETURNS the subject summary, so its stats::median site ran (median 80 over 70/80/90)")
# The utils::head site at eda.R:1046 is inside the within-subject-variation
# refusal, so it is reached by a frame whose covariate varies -- not by the
# succeeding call above. Both sites are therefore exercised, each by the input
# that reaches it.
k_cv <- if (is.list(k_vv))
  tryCatch(k_ed$reference_eda_continuous_summary(k_vv, "WT", rows = "subjects"),
           error = function(e) conditionMessage(e)) else "the varying view did not build"
if (!is.character(k_cv)) cat(sprintf("      got: %s\n", paste(class(k_cv), collapse = " ")))
ok(is.character(k_cv) && grepl("varies within subject for 3 subject(s) (1, 2, 3)", k_cv, fixed = TRUE),
   "K/valid: a within-subject-varying covariate refuses and NAMES the subjects, so the utils::head site ran")
setwd(old_wd)
unlink(krt, recursive = TRUE)

cat("\n--- J: the identity-chain rebindings of batch 13 fix round 3 (O-1) ---------\n")

# The batch-13 fix-round-3 shape, persisted. The APPROVED bytes sit at the
# manifest path; a TAMPERED COPY of the reference -- one inventoried internal,
# `.eda_kept`, neutered so that every record is kept and a marker file is
# written when it runs -- is sourced from a DIFFERENT path into the environment;
# then one ordinary post-source rebinding of a name the identity chain used to
# read is applied, and an approved entry point is called. Every case must be
# REFUSED and the neutered helper must never run. Both `keep.source` modes are
# exercised: with source references kept, the srcfile comparison also has
# something to say; without them, only the structural comparison does.
j_fixture <- function() {
  rt <- eda_fixture()
  p <- file.path(rt, EDA_REF)
  ln <- readLines(p, warn = FALSE)
  i <- grep("^\\.eda_kept <- function", ln)[1L]
  ln[i] <- paste0(".eda_kept <- function(x, priv) { base::file.create(\"MARKER_J\"); ",
                  "base::rep(TRUE, base::length(x)) }  # NEUTERED")
  writeLines(ln, file.path(rt, "tampered-eda.R"))
  rt
}
j_cases <- list(
  "no rebinding at all (the tampered copy alone)" = function(E) invisible(NULL),
  "EDA_REFERENCE_FILE points at the tampered copy" =
    function(E) assign("EDA_REFERENCE_FILE", "tampered-eda.R", envir = E),
  "`.eda_digest` returns a constant" =
    function(E) assign(".eda_digest", function(x) "sha256:const", envir = E),
  "`.eda_fn_text` returns the same text for every definition" =
    function(E) assign(".eda_fn_text", function(f) "same", envir = E),
  "both inventory vectors are emptied" =
    function(E) { assign("EDA_ENTRYPOINTS", character(0), envir = E)
                  assign("EDA_INTERNALS", character(0), envir = E) },
  "`.eda_source_identity` is replaced by a no-op" =
    function(E) assign(".eda_source_identity", function(...) invisible(TRUE), envir = E),
  "`.eda_defs_digest` returns a constant" =
    function(E) assign(".eda_defs_digest", function(env) "d", envir = E),
  # `.eda_disk_defs_digest` was removed in fix round 1 (observation O-3). Binding
  # the name post-source must therefore also change nothing, which is what this
  # case asserts -- the identity check never resolved it through this frame.
  "`.eda_disk_defs_digest` is bound post-source" =
    function(E) assign(".eda_disk_defs_digest", function(...) "d", envir = E))
for (ks in c(FALSE, TRUE)) for (nm in names(j_cases)) {
  setwd(old_wd); jrt <- j_fixture(); setwd(jrt)
  Ej <- new.env(parent = globalenv())
  lo <- tryCatch({ sys.source("tampered-eda.R", envir = Ej, keep.source = ks); "sourced" },
                 error = function(e) conditionMessage(e))
  if (!identical(lo, "sourced")) {
    cat(sprintf("      got: the tampered copy did not load: %s\n", substr(lo, 1, 120)))
    ok(FALSE, sprintf("J[keep.source=%s]: %s is REFUSED", ks, nm))
    ok(FALSE, sprintf("J[keep.source=%s]: %s -- the neutered helper never ran", ks, nm))
    setwd(old_wd); next
  }
  j_cases[[nm]](Ej)
  rj <- tryCatch(Ej$reference_eda_rowset_digest(VIEW, ROWS), error = function(e) conditionMessage(e))
  if (!(is.character(rj) && grepl("ESCALATE", rj)))
    cat(sprintf("      got: %s\n", substr(paste(rj, collapse = " "), 1, 160)))
  ok(is.character(rj) && grepl("ESCALATE", rj),
     sprintf("J[keep.source=%s]: %s is REFUSED", ks, nm))
  ok(!file.exists("MARKER_J"),
     sprintf("J[keep.source=%s]: %s -- the neutered helper never ran (no marker)", ks, nm))
  setwd(old_wd)
}
setwd(old_wd)

cat(sprintf("\nintegrity batch 14 battery: %d passed, %d failed\n", .pass, .fail))
if (.fail > 0) quit(status = 1)
invisible(TRUE)
}

if (.invoked_as_script()) .run_batch14_tests()
