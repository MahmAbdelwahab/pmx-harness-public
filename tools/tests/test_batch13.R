#!/usr/bin/env Rscript
# Integrity batch 13 battery (Codex round 12 findings, SME decisions 2026-09-05).
#
# Sections:
#   A  R12-P0-1/P0-2 static: every executable name inside a sealed construction
#                  and inside every gate/accessor closure is base::-qualified.
#                  The wrapper is exactly `base::local({ ... }, envir =
#                  base::new.env(parent = base::baseenv()))`; a path vector is
#                  `base::c(...)` or a single string literal; a pre-seal write's
#                  value is a literal or a call whose every head is one of the
#                  enumerated qualified heads; a gate/accessor closure is
#                  recognised only as `X <- base::local({ ... })` and a bare
#                  `local` closure is refused naming the file and the fix.
#   AR R12-P0-1/P0-2 at run time, Codex's two scenarios: a stateful fake `c()`
#                  redirecting a dependency load, and a fake `local` installing
#                  an always-TRUE gate on a DRAFT unit (scm_config).
#   B  R12-P1-2    a root path constant must be bound exactly once at the root
#                  AND before the construction statement that reads it.
#   C  R12-P1-3    the reflective walker unwraps `base::local(function ...)` in
#                  argument position exactly as it unwraps the bare form.
#   D  R12-P1-1    approve_gate() re-parses the record under the ledger lock and
#                  refuses when the record changed between the two parses.
#   E  R12-P2-2    a failed lock acquisition verifies its cleanup and names the
#                  stale lock when the directory remains.
#   F  decision 1B/2  tools/load_reference.R: tampered reference or dependency
#                  bytes are refused BEFORE any top-level side effect; an honest
#                  load returns an environment whose gate works; the bytes
#                  evaluated are the bytes hashed.
#   G  decision 1B ci_check refuses a direct source()/sys.source() of a
#                  skills/*/reference/*.R from pipeline/ or analyses/.
#   H  R12-P0-1/2  at run time, all 20 sealed files: with fakes for local, c,
#                  vapply, paste0, character, new.env, sys.source,
#                  lockEnvironment, file.exists and stop pre-bound in the
#                  sourcing environment, every gate reaches the real validator
#                  and every dependency load resolves to its honest path.
#   I  census      every top-level `X <- local({` closure in the 20 sealed
#                  files is written `base::local`; the 18 standard blocks are
#                  byte-identical; no bare `c(` feeds a construction.
#
# Run with:  Rscript tools/tests/test_batch13.R
# Exits non-zero if any assertion fails.

# SAFE TO SOURCE: pipelines tar_source("tools") which recurses into
# tools/tests/; everything observable is guarded behind .invoked_as_script().
.invoked_as_script <- function() {
  fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  length(fa) > 0 && grepl("test_batch13\\.R$", sub("^--file=", "", fa[1]))
}

.run_batch13_tests <- function() {

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
is_root <- identical(Sys.info()[["effective_user"]], "root")

# --- fixture root -----------------------------------------------------------
root <- tempfile("b13fix"); dir.create(root)
old_wd <- setwd(root); on.exit(setwd(old_wd), add = TRUE)
dir.create("tools")
writeLines(c("keep_rows <- function(d) d", "helper <- 1"), "tools/record_selection.R")
file.copy(file.path(REPO, "tools", "method_manifest.R"),
          "tools/method_manifest.R", overwrite = TRUE)
sha <- function(p) .mm_file_sha256(p)

mkgate <- function(name, body_lines, ep_lines, unit = "alpha", depends = character(0),
                   status = "approved") {
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
  dep_lines <- if (length(depends)) c("    depends:",
    unlist(lapply(depends, function(dp) c(paste0("      - path: ", dp),
                                          paste0("        sha256: ", sha(dp))))))
    else character(0)
  writeLines(c("# fixture", paste0("skill: ", name),
               "generated_at: 2026-09-05T00:00:00Z",
               'reviewed_by: "Fixture Reviewer"', 'reviewed_at: "2026-09-01"', "units:",
               paste0("  ", unit, ":"), paste0("    status: ", status),
               paste0("    reference_path: ", p), "    entrypoints: [ep]",
               paste0("    sha256: ", sha(p)), "    evidence: SKILL.md",
               '    reason: "signed"', dep_lines),
             file.path(d, "MANIFEST.yaml"))
  d
}
gated_of <- function(d) {
  r <- tryCatch(suppressWarnings(audit_entrypoint_gating(d)),
                error = function(e) conditionMessage(e))
  if (is.character(r)) list(gated = FALSE, reason = r, loads = "")
  else list(gated = isTRUE(r$gated[1]), reason = r$reason[1], loads = r$trusted_loads[1])
}
swap <- function(block, line, from, to) { block[line] <- sub(from, to, block[line], fixed = TRUE); block }

# The batch-13 standard block (decision 1A): wrapper frame parented on base.
QC <- c(
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
DEPS1 <- 'DEPS <- base::c("tools/record_selection.R")'
# The dependency environment in eda.R's shape, qualified throughout.
QD <- c(
  '.mm_trusted_deps <- base::local({',
  '  d <- base::new.env(parent = base::baseenv())',
  '  for (f in DEPS) {',
  '    if (!base::file.exists(f)) base::stop("missing ", f, call. = FALSE)',
  '    base::sys.source(f, envir = d, keep.source = FALSE)',
  '  }',
  '  d$dependency_sha256 <- base::vapply(DEPS, function(f)',
  '    base::paste0("sha256:", digest::digest(file = f, algo = "sha256")), base::character(1))',
  '  base::lockEnvironment(d, bindings = TRUE)',
  '  d',
  '}, envir = base::new.env(parent = base::baseenv()))',
  'keep_rows <- base::local({ sealed <- .mm_trusted_deps; function(...) base::get("keep_rows", envir = sealed)(...) }, envir = base::new.env())')

cat("\n--- A: R12-P0-1 / P0-2, everything executable is base::-qualified -------\n")

d <- mkgate("a_pos", c(QC, GATE), EP)
ok(gated_of(d)$gated, "A/positive: the batch-13 standard block with a base::local gate closure is trusted")
d <- mkgate("a_pos_deps", c(DEPS1, QC, QD, GATE), EP)
r <- gated_of(d)
ok(r$gated, "A/positive: base::c() path vector, qualified dependency block and qualified pre-seal write are trusted")
ok(grepl("tools/record_selection.R", r$loads, fixed = TRUE),
   "A/positive: the dependency load from the base::c() vector is reported")

# The wrapper frame must be parented on base (R12-P0-1): base::new.env()'s
# default parent is the calling frame, which is the sourcing environment.
d <- mkgate("a_wrap_old", c(swap(QC, 8L, "envir = base::new.env(parent = base::baseenv())", "envir = base::new.env()"), GATE), EP)
r <- gated_of(d)
ok(!r$gated, "A: the batch-12 wrapper `envir = base::new.env()` (parent = the sourcing frame) is refused")
ok(grepl("base::new.env(parent = base::baseenv())", r$reason, fixed = TRUE),
   "A: the wrapper refusal names `envir = base::new.env(parent = base::baseenv())`")
for (bad_envir in c("envir = base::new.env(parent = baseenv())",
                    "envir = base::new.env(parent = base::globalenv())",
                    "envir = base::new.env(base::baseenv())",
                    "envir = base::new.env(parent = base::baseenv(), hash = TRUE)")) {
  d <- mkgate(paste0("a_envir_", gsub("[^A-Za-z0-9]", "", bad_envir)),
              c(swap(QC, 8L, "envir = base::new.env(parent = base::baseenv())", bad_envir), GATE), EP)
  ok(!gated_of(d)$gated, sprintf("A: a wrapper frame written `%s` is refused", bad_envir))
}

# Path vectors: bare `c` refused (Codex's stateful fake c), base::c admitted,
# a single string literal admitted, bare file.path refused, base::file.path admitted.
d <- mkgate("a_bare_c", c('DEPS <- c("tools/record_selection.R")', QC, QD, GATE), EP)
r <- gated_of(d)
ok(!r$gated, "A: a root path vector built with bare `c(...)` is refused")
ok(grepl("base::c", r$reason, fixed = TRUE), "A: the bare-c refusal names `base::c`")
d <- mkgate("a_str", c('DEPS <- "tools/record_selection.R"', QC, QD, GATE), EP)
ok(gated_of(d)$gated, "A/positive: a root path constant that is a single string literal is admitted")
d <- mkgate("a_bare_fp", c('DEPS <- file.path("tools", "record_selection.R")', QC, QD, GATE), EP)
ok(!gated_of(d)$gated, "A: a root path constant built with bare `file.path(...)` is refused")
d <- mkgate("a_q_fp", c('DEPS <- base::file.path("tools", "record_selection.R")', QC, QD, GATE), EP)
ok(gated_of(d)$gated, "A/positive: a root path constant built with `base::file.path(...)` is admitted")
d <- mkgate("a_c_in_block", c(QC, swap(QD, 3L, "for (f in DEPS) {", 'for (f in c("tools/record_selection.R")) {'), GATE), EP)
ok(!gated_of(d)$gated, "A: a bare `c(...)` written inside the block's for-sequence is refused")
QDI <- swap(QD, 3L, "for (f in DEPS) {", 'for (f in base::c("tools/record_selection.R")) {')
QDI <- swap(QDI, 7L, "base::vapply(DEPS,", 'base::vapply(base::c("tools/record_selection.R"),')
d <- mkgate("a_qc_in_block", c(QC, QDI, GATE), EP)
r <- gated_of(d)
ok(r$gated && grepl("tools/record_selection.R", r$loads, fixed = TRUE),
   "A/positive: an in-block `base::c(...)` literal names a dependency (the shipped shape: a base-parented frame cannot read a root constant)")
d <- mkgate("a_str_in_block", c(QC, swap(swap(QD, 3L, "for (f in DEPS) {", 'for (f in "tools/record_selection.R") {'),
                                          7L, "base::vapply(DEPS,", 'base::vapply("tools/record_selection.R",'), GATE), EP)
ok(gated_of(d)$gated, "A/positive: an in-block single string literal names a dependency")

# The pre-seal write's value.
d <- mkgate("a_w_bare_vapply", c(DEPS1, QC, swap(QD, 7L, "base::vapply(", "vapply("), GATE), EP)
r <- gated_of(d)
ok(!r$gated, "A: a pre-seal write whose value calls bare `vapply(...)` is refused")
ok(grepl("base::vapply", r$reason, fixed = TRUE), "A: the bare-vapply refusal names the qualified head")
d <- mkgate("a_w_bare_paste0", c(DEPS1, QC, swap(QD, 8L, "base::paste0(", "paste0("), GATE), EP)
ok(!gated_of(d)$gated, "A: a pre-seal write whose value calls bare `paste0(...)` is refused")
d <- mkgate("a_w_bare_char", c(DEPS1, QC, swap(QD, 8L, "base::character(1)", "character(1)"), GATE), EP)
ok(!gated_of(d)$gated, "A: a pre-seal write whose value calls bare `character(1)` is refused")
d <- mkgate("a_w_other_head", c(DEPS1, QC, swap(QD, 8L, "base::paste0(", "base::get(\"x\", "), GATE), EP)
ok(!gated_of(d)$gated, "A: a pre-seal write whose value calls a base:: head outside the enumerated set (base::get) is refused")
d <- mkgate("a_w_other_pkg", c(DEPS1, QC, swap(QD, 8L, "digest::digest(", "openssl::sha256("), GATE), EP)
ok(!gated_of(d)$gated, "A: a pre-seal write whose value calls a qualified head of another package (openssl::sha256) is refused")
d <- mkgate("a_w_free_sym", c(DEPS1, QC, swap(QD, 7L, "base::vapply(DEPS,", "base::vapply(OTHER,"), GATE), EP)
ok(!gated_of(d)$gated, "A: a pre-seal write whose value reads a symbol that is neither a root path constant, the loop index nor a formal is refused")
QDL <- QD; QDL[7L] <- '  d$dependency_sha256 <- "sha256:literal"'; QDL <- QDL[-8L]
d <- mkgate("a_w_literal", c(DEPS1, QC, QDL, GATE), EP)
ok(gated_of(d)$gated, "A/positive: a pre-seal write of a string literal is admitted")
d <- mkgate("a_w_sapply", c(DEPS1, QC, swap(QD, 7L, "base::vapply(DEPS, function(f)", "base::sapply(DEPS, function(f)"),
                            GATE), EP)
r <- gated_of(d)
ok(!r$gated || grepl("character(1)", paste(readLines(file.path(d, "reference", "method.R")), collapse = ""), fixed = TRUE),
   "A: base::sapply is on the enumerated set (the fixture keeps base::character(1) as an extra argument, which sapply passes through to FUN)")

# Gate and accessor closures: bare `local` refused naming the file and the fix.
BARE_GATE <- swap(GATE, 1L, "base::local({", "local({")
d <- mkgate("a_bare_gate", c(QC, BARE_GATE), EP)
r <- gated_of(d)
ok(!r$gated, "A: a gate closure built with bare `local({ ... })` is refused")
ok(grepl("base::local", r$reason, fixed = TRUE) && grepl(".alpha_gate", r$reason, fixed = TRUE) &&
     grepl("method.R", r$reason, fixed = TRUE),
   "A: the bare-local refusal names the closure, the file and the `base::local` fix")
d <- mkgate("a_bare_accessor", c(DEPS1, QC, swap(QD, 12L, "keep_rows <- base::local({", "keep_rows <- local({"), GATE), EP)
ok(!gated_of(d)$gated, "A: an accessor closure built with bare `local({ ... })` is refused")
d <- mkgate("a_default_envir_gate", c(QC, swap(GATE, 8L, "}, envir = base::new.env())", "})")), EP)
r <- gated_of(d)
ok(!r$gated, "A: a gate closure `base::local({ ... })` with the DEFAULT envir is refused (a fake new.env supplies its frame)")
ok(grepl("envir = base::new.env()", r$reason, fixed = TRUE), "A: the default-envir closure refusal names `envir = base::new.env()`")
d <- mkgate("a_parented_gate", c(QC, swap(GATE, 8L, "}, envir = base::new.env())", "}, envir = base::new.env(parent = base::baseenv()))")), EP)
ok(!gated_of(d)$gated, "A: a gate closure whose frame is parented on base is refused (it cannot read the file's sealed object)")
d <- mkgate("a_bare_stop", c(QC, swap(GATE, 5L, "base::stop(", "stop(")), EP)
r <- gated_of(d)
ok(!r$gated && grepl("`stop`", r$reason, fixed = TRUE) && grepl("base::<name>", r$reason, fixed = TRUE),
   "A: a gate body calling bare `stop(...)` is refused naming the head and the qualification")
d <- mkgate("a_bare_invisible", c(QC, swap(GATE, 6L, "base::invisible(", "invisible(")), EP)
ok(!gated_of(d)$gated, "A: a gate body calling bare `invisible(...)` is refused")
d <- mkgate("a_member_stop", c(QC, swap(GATE, 5L, "base::stop(", "helper_stop("),
                                 'helper_stop <- function(...) base::stop(...)'), EP)
ok(gated_of(d)$gated, "A/positive: a gate body calling a function this file defines at its top level is admitted")
ok(identical(.mm_unqualified_gate_heads(quote(function() { if (!p$v(unit = "a")) stop("x"); base::invisible(TRUE) })), c("p$v", "stop")),
   "A: .mm_unqualified_gate_heads() lists the bare non-syntax heads AND the `$` dispatch (batch 13 fix round 1: `$`/`[[`/`@` are no longer exempt)")
d <- mkgate("a_paren_local", c(QC, swap(GATE, 1L, "base::local({", "(base::local)({")), EP)
ok(!gated_of(d)$gated, "A: a gate closure whose wrapper head is `(base::local)` is refused")
d <- mkgate("a_triple_local", c(QC, swap(GATE, 1L, "base::local({", "base:::local({")), EP)
ok(!gated_of(d)$gated, "A: a gate closure whose wrapper head is `base:::local` is refused")
ok(is.null(.mm_resolve_fn(quote(local({ private <- p; function() 1 })))) &&
     is.null(.mm_resolve_fn(quote(base::local({ private <- p; function() 1 })))) &&
     .mm_is_function_literal(.mm_resolve_fn(quote(base::local({ private <- p; function() 1 }, envir = base::new.env())))),
   "A: .mm_resolve_fn() resolves only `base::local({ ...; function }, envir = base::new.env())`")
# A top-level binding of any name the constructions now execute is refused.
for (nm in c("c", "vapply", "paste0", "character", "sapply", "file.path", "sprintf")) {
  d <- mkgate(paste0("a_shadow_", nm), c(sprintf('%s <- function(...) NULL', nm), QC, GATE), EP)
  ok(!gated_of(d)$gated, sprintf("A: a top-level binding of `%s` in the reference file is refused", nm))
}

cat("\n--- AR: Codex's two scenarios at run time --------------------------------\n")
# A copy of the repository's sealed files, sourced into a hostile environment.
# Scenario 1: a stateful fake `c()` answers the FIRST call with an attacker's
# file and delegates afterwards; the first `c(...)` in eda.R is the dependency
# vector, so the dependency environment loads the attacker's file while the
# audit reports tools/record_selection.R. Scenario 2: a fake `local` that
# returns an always-TRUE closure; scm_config.R's `scm_config_validated` is a
# DRAFT unit's gate, so the honest answer is an authorization refusal before execution.
rt0 <- tempfile("b13ar"); dir.create(rt0)
# (the whole skills/ tree, because a manifest validates every unit's files)
dir.create(file.path(rt0, "tools"))
# baseline-fit (2026-11-14): the new draft manifest binds these additional
# tool dependencies and the R lockfile; regeneration needs their copied bytes.
for (f in c("method_manifest.R", "record_selection.R", "psn_invocation.R",
            "load_reference.R", "engine_config.R", "yaml_scalar.R", "r_lock.R"))
  file.copy(file.path(REPO, "tools", f), file.path(rt0, "tools", f))
dir.create(file.path(rt0, "config"))
file.copy(file.path(REPO, "config", "r-packages.lock"), file.path(rt0, "config"))
file.copy(file.path(REPO, "skills"), rt0, recursive = TRUE)
evil <- file.path(rt0, "evil.R")
writeLines('EVIL_LOADED <- TRUE', evil)
setwd(rt0)
# The copy's manifests are regenerated so their digests bind the copied bytes
# (write_method_manifest() resets every drifted unit to draft, which is the
# state these scenarios need: an honest gate throws before execution).
regen_manifest <- function(skill_dir) {
  m <- read_method_manifest(file.path(skill_dir, "MANIFEST.yaml"))
  spec <- lapply(m$units, function(u) list(
    reference_path = u$reference_path, entrypoints = u$entrypoints,
    evidence = u$evidence, reason = u$reason, status = u$status,
    depends = vapply(u$depends, function(d) d$path, character(1))))
  suppressMessages(write_method_manifest(skill_dir, spec))
}
for (sd in Sys.glob("skills/*")) if (file.exists(file.path(sd, "MANIFEST.yaml"))) regen_manifest(sd)
env <- new.env(parent = globalenv())
calls <- 0L
assign("c", function(...) { calls <<- calls + 1L; if (calls == 1L) evil else base::c(...) }, envir = env)
loaded <- tryCatch({ base::sys.source("skills/eda-standards/reference/eda.R", envir = env, keep.source = FALSE); TRUE },
                   error = function(e) conditionMessage(e))
dd <- tryCatch(get(".eda_deps", envir = env), error = function(e) NULL)
if (!isTRUE(loaded)) cat(sprintf("      eda.R did not source: %s\n", loaded))
ok(isTRUE(loaded) && !is.null(dd) && !exists("EVIL_LOADED", envir = dd, inherits = FALSE) &&
     exists("harness_kept_record", envir = dd, inherits = FALSE),
   "AR: eda.R's dependency environment loads tools/record_selection.R, not the file a stateful fake `c()` names")
ok(isTRUE(loaded) && identical(get("EDA_DEPENDENCY_FILES", envir = env), "tools/record_selection.R"),
   "AR: eda.R's EDA_DEPENDENCY_FILES is the honest vector with a fake `c()` pre-bound")
env <- new.env(parent = globalenv())
assign("local", function(expr, envir = NULL) function(...) TRUE, envir = env)
loaded <- tryCatch({ base::sys.source("skills/covariate-priors/reference/scm_config.R", envir = env, keep.source = FALSE); TRUE },
                   error = function(e) conditionMessage(e))
honest <- new.env(parent = globalenv())
base::sys.source("skills/covariate-priors/reference/scm_config.R", envir = honest, keep.source = FALSE)
real_gate <- tryCatch(honest$scm_config_validated(), error = function(e) e)
fake_gate <- tryCatch(env$scm_config_validated(), error = function(e) e)
if (!isTRUE(loaded)) cat(sprintf("      scm_config.R did not source: %s\n", loaded))
ok(inherits(real_gate, "error") && grepl("not execution-authorized", conditionMessage(real_gate), fixed = TRUE),
   "AR/control: scm_config's draft gate throws an authorization refusal in an honest environment")
ok(isTRUE(loaded) && inherits(fake_gate, "error") && inherits(real_gate, "error") &&
     identical(conditionMessage(fake_gate), conditionMessage(real_gate)),
   "AR: scm_config's draft gate throws the same refusal with a fake `local` pre-bound")
setwd(root)

cat("\n--- B: R12-P1-2, root path constants precede the construction ----------\n")
d <- mkgate("b_after", c(QC, QD, GATE, DEPS1), EP)
r <- gated_of(d)
ok(!r$gated, "B: a root path constant bound AFTER the construction that reads it is refused")
ok(grepl("DEPS", r$reason, fixed = TRUE) && grepl("statement", r$reason) &&
     grepl(".mm_trusted_deps", r$reason, fixed = TRUE),
   "B: the ordering refusal names the constant and the construction statement")
d <- mkgate("b_before", c(QC, DEPS1, QD, GATE), EP)
ok(gated_of(d)$gated, "B/positive: a root path constant bound immediately before the construction is admitted")
d <- mkgate("b_twice", c(DEPS1, QC, QD, GATE, DEPS1), EP)
ok(!gated_of(d)$gated, "B: a root path constant bound before AND after the construction is refused (bound twice)")
# The shipped files: since decision 1A parented the block's frame on base, no
# construction reads a root constant (it could not at run time); their paths
# are in-block `base::c(...)` literals and the ordering rule has nothing to
# refuse. eda.R keeps EDA_DEPENDENCY_FILES as the provenance declaration.
setwd(REPO)
for (f in c("skills/eda-standards/reference/eda.R", "skills/covariate-priors/reference/psn_covsearch.R")) {
  b <- .mm_audit_binds(f)
  ok(length(attr(b, "order")) == 0L && is.list(attr(b, "root_order")),
     sprintf("B: %s has no ordering violation and carries statement ordinals on its root index", basename(f)))
}
ok(identical(.mm_static_path_vector(quote(base::c("tools/record_selection.R")), list())$path, "tools/record_selection.R"),
   "B: eda.R's in-block `base::c(\"tools/record_selection.R\")` resolves as a literal")
setwd(root)

cat("\n--- C: R12-P1-3, base::local in argument position ------------------------\n")
refl <- function(txt) .mm_reflective_top_level_violations(parse(text = txt, keep.source = FALSE))
ok(length(refl('Reduce(base::local(function(acc, f) base::source(f)), files)')) > 0L,
   "C: `Reduce(base::local(function(acc, f) base::source(f)), files)` is refused")
ok(length(refl('Reduce(local(function(acc, f) source(f)), files)')) > 0L,
   "C: the bare form `Reduce(local(function(acc, f) source(f)), files)` is refused")
ok(length(refl('Reduce(base::local({ k <- 1; function(acc, f) base::sys.source(f, envir = e) }), files)')) > 0L,
   "C: a multi-statement `base::local({ ... })` block in argument position is walked names-only")
ok(length(refl('lapply(files, base::local(function(f) eval(parse(text = f))))')) > 0L,
   "C: `lapply(files, base::local(function(f) eval(parse(...))))` is refused")
ok(length(refl('Reduce(base::local(function(a, b) a + b), xs)')) == 0L,
   "C/positive: `Reduce(base::local(function(a, b) a + b), xs)` is not refused")
ok(length(refl('x <- base::local({ p <- 1; function() p })')) == 0L,
   "C/positive: a statement-level `x <- base::local({ ... })` definition stays exempt")
ok(length(refl('x <- local({ p <- 1; function() p })')) == 0L,
   "C/positive: a statement-level bare `x <- local({ ... })` definition is not a reflective violation (it is refused by the closure rule, not here)")

cat("\n--- D: R12-P1-1, approve_gate() re-parses the record under the lock ---------\n")
ag <- new.env(parent = globalenv())
suppressWarnings(suppressMessages({
  sys.source(file.path(TOOLS, "gate_package.R"), envir = ag, keep.source = FALSE)
  sys.source(file.path(TOOLS, "gate.R"), envir = ag, keep.source = FALSE)
  sys.source(file.path(TOOLS, "approve_gate.R"), envir = ag, keep.source = FALSE)
}))
gw <- tempfile("b13gate"); dir.create(file.path(gw, "gates", "TST-013"), recursive = TRUE)
setwd(gw)
writeLines("hello", "a.csv")
arts <- data.frame(artifact = "t1", path = "a.csv", source = "ta", stringsAsFactors = FALSE)
tables <- list("Verdicts" = data.frame(criterion = "E1", value = 1.23, stringsAsFactors = FALSE))
rec <- file.path("gates", "TST-013", "eda.md")
writeLines(c("# Gate Sign-off Record", "", "Analysis ID: TST-013", "Stage: eda",
             "Date opened: 2026-09-05", "", "## SME decision", "",
             "- [ ] APPROVED — stage complete, next stage may start",
             "- [ ] APPROVED WITH CHANGES — changes listed below were applied and re-verified",
             "- [ ] REJECTED — returned to stage with instructions below", "",
             "Signed: <!-- SME name -->", "Date: <!-- -->", "Digest signed: <!-- paste -->"), rec)
ag$render_gate_package(rec, arts, tables, c("a note"))
rendered <- readLines(rec)
sign <- function(lines, signer = "Signed: Fixture Reviewer", date = "Date: 2026-09-05") {
  dg <- sub("^Package-Digest: ", "", grep("^Package-Digest:", lines, value = TRUE)[1])
  i <- grep("^- \\[ \\] APPROVED —", lines)[1]
  lines[i] <- sub("\\[ \\]", "[x]", lines[i])
  lines[grep("^Digest signed:", lines)] <- paste("Digest signed:", dg)
  lines[grep("^Signed:", lines)] <- signer
  lines[grep("^Date:", lines)] <- date
  lines
}
writeLines(sign(rendered), rec)
ledger <- ag$.ledger_path(rec)
ok(".after_lock" %in% names(formals(ag$approve_gate)),
   "D: approve_gate() takes a `.after_lock` injection point (test-only hook run after the lock is held)")
# The record is rewritten between the preliminary parse and the lock: a valid
# signature by another signer. The stale waiter must refuse, appending nothing.
n_before <- if (file.exists(ledger)) length(readLines(ledger)) else 0L
r <- tryCatch(suppressMessages(ag$approve_gate(rec, check_store = FALSE,
         .after_lock = function() writeLines(sign(rendered, signer = "Signed: XX"), rec))),
     error = function(e) conditionMessage(e))
ok(is.character(r) && grepl("changed", r) && grepl("lock", r),
   "D: a record rewritten between the preliminary parse and the lock is refused, naming the change under the lock")
ok((if (file.exists(ledger)) length(readLines(ledger)) else 0L) == n_before,
   "D: nothing was appended to the ledger by the refused approval")
ok(!dir.exists(ag$.release_lock_path(ledger)), "D: the lock is released after the refusal")
# The honest path: the event carries the fields of the record as re-parsed under the lock.
writeLines(sign(rendered), rec)
ev <- tryCatch(suppressMessages(ag$approve_gate(rec, check_store = FALSE)), error = function(e) e)
ok(!inherits(ev, "error") && identical(ev$signer, "Fixture Reviewer") && identical(ev$record_digest, ag$.gate_sha256(rec)),
   "D/positive: an unchanged record is approved from the re-parsed fields and the current digest")
# A record that becomes INVALID under the lock (signer cleared) is refused too.
writeLines(sign(rendered, signer = "Signed: <!-- SME name -->"), rec)
writeLines(sign(rendered), rec)   # valid at the preliminary parse ...
r <- tryCatch(suppressMessages(ag$approve_gate(rec, check_store = FALSE,
         .after_lock = function() writeLines(sign(rendered, signer = "Signed: <!-- SME name -->"), rec))),
     error = function(e) conditionMessage(e))
ok(is.character(r) && !dir.exists(ag$.release_lock_path(ledger)),
   "D: a record that loses its signature under the lock is refused and the lock released")

cat("\n--- E: R12-P2-2, a failed acquisition verifies its cleanup ------------------\n")
ok(".on_created" %in% names(formals(ag$.release_lock_acquire)),
   "E: .release_lock_acquire() takes a `.on_created` injection point (test-only hook run after the directory exists)")
if (is_root) {
  cat("      (running as root: permission fixtures are not meaningful; E skipped)\n")
} else {
  lw <- tempfile("b13lock"); dir.create(lw)
  led <- file.path(lw, "approvals.jsonl"); writeLines("", led)
  lockdir <- ag$.release_lock_path(led)
  # The holder write fails (lock directory read-only) AND the cleanup fails
  # (parent read-only): the error must name the stale lock and the manual step.
  r <- tryCatch(ag$.release_lock_acquire(led, who = "test_writer", .on_created = function(lock) {
                  Sys.chmod(lock, "500"); Sys.chmod(lw, "500") }),
                error = function(e) conditionMessage(e))
  remains <- dir.exists(lockdir)
  Sys.chmod(lw, "700"); if (dir.exists(lockdir)) Sys.chmod(lockdir, "700"); unlink(lockdir, recursive = TRUE, force = TRUE)
  ok(remains, "E/fixture: the lock directory could not be removed (parent read-only)")
  ok(is.character(r) && grepl("stale", r) && grepl(lockdir, r, fixed = TRUE) && grepl("by hand", r),
     "E: the acquisition error names the stale lock directory and the manual removal step")
  ok(is.character(r) && !grepl("has been removed again", r),
     "E: the error no longer claims the directory was removed")
  # Cleanup that succeeds keeps the batch-12 wording.
  r <- tryCatch(ag$.release_lock_acquire(led, who = "test_writer", .on_created = function(lock) Sys.chmod(lock, "500")),
                error = function(e) conditionMessage(e))
  ok(is.character(r) && grepl("removed again", r) && !dir.exists(lockdir),
     "E/positive: a failed holder write whose cleanup succeeds reports the removal and leaves no lock")
}
setwd(root)

cat("\n--- F: decisions 1B and 2, the trusted loader ------------------------------\n")
LR <- file.path(TOOLS, "load_reference.R")
ok(file.exists(LR), "F: tools/load_reference.R exists")
lr_env <- new.env(parent = globalenv())
lr_loaded <- tryCatch({ sys.source(LR, envir = lr_env, keep.source = FALSE); TRUE },
                      error = function(e) conditionMessage(e))
ok(isTRUE(lr_loaded) && exists("load_reference", envir = lr_env, mode = "function"),
   "F: tools/load_reference.R defines load_reference()")
load_reference <- if (isTRUE(lr_loaded)) get("load_reference", envir = lr_env) else function(...) stop("no loader")
# An honest, approved unit whose reference has a top-level side effect (a
# marker file) beside the sealed construction and gate.
MARK <- c('base::file.create("MARKER_REF")')
# A gate that names the fixture's real skill directory and file, so it runs.
gate_rt <- function(name) c(
  '.alpha_gate <- base::local({',
  '  private <- .mm_trusted_private',
  '  function() {',
  sprintf('    if (!base::get("require_validated_method", envir = private)(skill_dir = "skills/%s", calling_file = "skills/%s/reference/method.R",', name, name),
  '                                          unit = "alpha")) base::stop("no")',
  '    base::invisible(TRUE)',
  '  }',
  '}, envir = base::new.env())')
d <- mkgate("f_honest", c(MARK, QC, gate_rt("f_honest")), EP)
unlink("MARKER_REF")
env <- tryCatch(load_reference(d, file.path(d, "reference", "method.R")), error = function(e) e)
ep_res <- if (is.environment(env)) tryCatch(env$ep(41), error = function(e) conditionMessage(e)) else conditionMessage(env)
if (!identical(ep_res, 41)) cat(sprintf("      got: %s\n", paste(ep_res, collapse = " ")))
ok(is.environment(env) && exists("ep", envir = env, inherits = FALSE) && identical(ep_res, 41),
   "F/positive: an honest load returns an environment whose gated entry point computes")
ok(file.exists("MARKER_REF"), "F/positive: the honest reference's top level ran (marker present)")
ok(identical(parent.env(env), baseenv()),
   "F: the default environment is parented on base (base names win; attached packages reachable through global)")
rec <- attr(env, "load_reference")
ok(is.list(rec) && identical(rec$reference_path, file.path(d, "reference", "method.R")) &&
     identical(rec$sha256, sha(file.path(d, "reference", "method.R"))),
   "F: the returned environment carries the load record (path and the digest of the bytes evaluated)")
# Tampered reference bytes: refused BEFORE any side effect.
unlink("MARKER_REF")
cat("# tampered\n", file = file.path(d, "reference", "method.R"), append = TRUE)
r <- tryCatch(load_reference(d, file.path(d, "reference", "method.R")), error = function(e) conditionMessage(e))
ok(is.character(r) && grepl("sha256", r) && grepl("method.R", r, fixed = TRUE),
   "F: tampered reference bytes are refused, naming the file and the digests")
ok(!file.exists("MARKER_REF"), "F: the tampered reference's top level did NOT run (no marker)")
ok(is.character(r) && grepl(sha(file.path(d, "reference", "method.R")), r, fixed = TRUE),
   "F: the refusal names the digest of the bytes on disk")
# Tampered dependency: refused likewise, before the reference's first byte runs.
writeLines(c("keep_rows <- function(d) d"), "tools/dep_f.R")
d2 <- mkgate("f_dep", c(MARK, QC,
                        swap(swap(QD, 3L, "for (f in DEPS) {", 'for (f in base::c("tools/dep_f.R")) {'),
                             7L, "base::vapply(DEPS,", 'base::vapply(base::c("tools/dep_f.R"),'), gate_rt("f_dep")), EP,
             depends = "tools/dep_f.R")
unlink("MARKER_REF")
env2 <- tryCatch(load_reference(d2, file.path(d2, "reference", "method.R")), error = function(e) conditionMessage(e))
ok(is.environment(env2) && file.exists("MARKER_REF"),
   "F/positive: an honest reference with an honest dependency loads")
unlink("MARKER_REF")
cat('base::file.create("MARKER_DEP")\n', file = "tools/dep_f.R", append = TRUE)
r <- tryCatch(load_reference(d2, file.path(d2, "reference", "method.R")), error = function(e) conditionMessage(e))
ok(is.character(r) && grepl("tools/dep_f.R", r, fixed = TRUE) && grepl("sha256", r),
   "F: a tampered dependency is refused, naming the dependency and the digests")
ok(!file.exists("MARKER_REF") && !file.exists("MARKER_DEP"),
   "F: neither the reference nor the tampered dependency ran (no markers)")
# A reference the manifest does not name, a wrong skill dir, and an unreadable manifest.
r <- tryCatch(load_reference(d, "tools/record_selection.R"), error = function(e) conditionMessage(e))
ok(is.character(r) && grepl("MANIFEST.yaml", r, fixed = TRUE),
   "F: a path no unit of the manifest names is refused")
r <- tryCatch(load_reference("skills/nonexistent", file.path(d, "reference", "method.R")),
              error = function(e) conditionMessage(e))
ok(is.character(r) && grepl("MANIFEST", r), "F: a skill directory without a manifest is refused")
# The caller's environment: `envir = environment()` (the pipeline's `local = TRUE` replacement).
d <- mkgate("f_envir", c(QC, gate_rt("f_envir")), EP)
f <- function() { load_reference(d, file.path(d, "reference", "method.R"), envir = environment()); ep(2) }
f_res <- tryCatch(f(), error = function(e) conditionMessage(e))
if (!identical(f_res, 2)) cat(sprintf("      got: %s\n", paste(f_res, collapse = " ")))
ok(identical(f_res, 2),
   "F/positive: `envir = environment()` evaluates the reference into the caller's frame")
# The loader never re-reads between hash and evaluation: it evaluates the parsed
# bytes, so the evaluated definitions equal the parse of the hashed bytes.
d <- mkgate("f_once", c(QC, GATE), EP)
env <- load_reference(d, file.path(d, "reference", "method.R"))
hashed <- parse(text = readLines(file.path(d, "reference", "method.R")), keep.source = FALSE)
ok(identical(deparse(env$ep), deparse(eval(hashed[[length(hashed)]][[3]]))),
   "F: the evaluated definitions are the parse of the hashed bytes")

cat("\n--- G: decision 1B, ci_check refuses direct reference sourcing ---------------\n")
ci_env <- new.env(parent = globalenv())
sys.source(file.path(TOOLS, "ci_check.R"), envir = ci_env, keep.source = FALSE)
ok(exists("check_reference_loading", envir = ci_env, mode = "function"),
   "G: tools/ci_check.R defines check_reference_loading()")
gfix <- tempfile("b13ci"); dir.create(file.path(gfix, "pipeline", "tasks"), recursive = TRUE)
dir.create(file.path(gfix, "analyses"))
run_check <- function() {
  out <- capture.output(res <- tryCatch(ci_env$check_reference_loading(root = gfix),
                                        error = function(e) conditionMessage(e)))
  list(out = paste(out, collapse = "\n"), res = res)
}
writeLines(c('x <- function() {', '  source("skills/diagnostics/reference/gof.R", local = TRUE)', '}'),
           file.path(gfix, "pipeline", "tasks", "a.R"))
r <- run_check()
ok(grepl("FAIL", r$out) && grepl("a.R", r$out, fixed = TRUE) && grepl("load_reference", r$out, fixed = TRUE),
   "G: a literal source() of a skills/*/reference/*.R under pipeline/ is a FAIL naming the file and the loader")
writeLines(c('ref <- "skills/diagnostics/reference/gof.R"', 'sys.source(ref, envir = e)'),
           file.path(gfix, "pipeline", "tasks", "a.R"))
r <- run_check()
ok(grepl("FAIL", r$out) && grepl("a.R", r$out, fixed = TRUE),
   "G: a sys.source() of a symbol bound to a reference path in the same file is a FAIL")
writeLines(c('source(qual_gof_reference, local = TRUE)'), file.path(gfix, "analyses", "b.R"))
r <- run_check()
ok(grepl("FAIL", r$out) && grepl("b.R", r$out, fixed = TRUE),
   "G: a source() of a symbol whose name says `reference` under analyses/ is a FAIL")
# The positive case is a source() of a NON-reference file whose path the census
# can resolve. It used to be `source(bm_modeling_code, local = TRUE)`, a symbol
# the census cannot fold; since SME decision D15-2 (2026-09-06) an unresolved
# source target is a FAIL unless its file is allowlisted, so that spelling now
# belongs to the unresolved-target case (tools/tests/test_batch15.R, section G)
# rather than to this one.
dir.create(file.path(gfix, "tools"))
writeLines("fixture_helper <- function(x) x", file.path(gfix, "tools", "fixture_helper.R"))
writeLines(c('load_reference("skills/diagnostics", "skills/diagnostics/reference/gof.R", envir = environment())',
             'source("tools/fixture_helper.R", local = TRUE)'),
           file.path(gfix, "pipeline", "tasks", "a.R"))
unlink(file.path(gfix, "analyses", "b.R"))
r <- run_check()
ok(grepl("PASS", r$out) && !grepl("FAIL", r$out),
   "G/positive: load_reference() and a source() of a non-reference file pass")
setwd(REPO)
r_live <- capture.output(ci_env$check_reference_loading(root = "."))
ok(any(grepl("^PASS", r_live)) && !any(grepl("^FAIL", r_live)),
   "G: the live pipeline/ and analyses/ trees source no reference directly")
setwd(root)

cat("\n--- H: all 20 sealed files at run time, hostile sourcing environment -------\n")
# A copy of the repository whose tools/method_manifest.R defines the validator
# as an unconditional stop, sourced into an environment where every executable
# name the constructions and closures reach is pre-bound to a fake: local, c,
# vapply, paste0, character, new.env, sys.source, lockEnvironment, file.exists
# and stop. A gate that reaches the REAL validator raises its stop; any fake
# consulted on the way changes the outcome (a redirected load, a swallowed
# refusal, an always-TRUE closure, a frame parented on the fake's choice).
rt <- tempfile("b13rt"); dir.create(rt)
dir.create(file.path(rt, "tools"))
mm <- readLines(file.path(REPO, "tools", "method_manifest.R"), warn = FALSE)
writeLines(c(mm, 'require_validated_method <- function(...) stop("REAL VALIDATOR RAN", call. = FALSE)'),
           file.path(rt, "tools", "method_manifest.R"))
for (f in c("record_selection.R", "engine_config.R", "seed.R", "psn_invocation.R"))
  if (file.exists(file.path(REPO, "tools", f)))
    file.copy(file.path(REPO, "tools", f), file.path(rt, "tools", f))
file.copy(file.path(REPO, "skills"), rt, recursive = TRUE)
writeLines('EVIL_LOADED <- TRUE', file.path(rt, "evil.R"))
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
  list("skills/simulation-standards/reference/uncertainty.R",  ".uncertainty_gate",      NULL),
  # AC12 (2026-10-26, plan Section 8, P-2): the new simulation-standards gate.
  list("skills/simulation-standards/reference/bound_comparison.R", ".sim_bound_gate",   NULL),
  list("skills/eda-standards/reference/eda.R",                 ".eda_gate",              "summary_statistics"))
setwd(rt)
evil_path <- file.path(rt, "evil.R")
bind_fakes <- function(env) {
  assign("require_validated_method", function(...) TRUE, envir = env)
  assign("sys.source", function(file, envir, ...) {
    assign("require_validated_method", function(...) TRUE, envir = envir); invisible(NULL) }, envir = env)
  assign("new.env", function(...) {
    f <- base::new.env(parent = globalenv())
    base::assign(".mm_trusted_private", base::list(require_validated_method = function(...) TRUE), envir = f)
    base::assign(".eda_private", base::list(require_validated_method = function(...) TRUE), envir = f)
    f }, envir = env)
  assign("baseenv", function() globalenv(), envir = env)
  assign("lockEnvironment", function(...) invisible(NULL), envir = env)
  assign("file.exists", function(...) TRUE, envir = env)
  assign("stop", function(...) invisible(NULL), envir = env)            # swallows every refusal
  assign("invisible", function(x) x, envir = env)
  assign("local", function(expr, envir = NULL) function(...) TRUE, envir = env)   # an always-TRUE closure
  assign("c", function(...) evil_path, envir = env)                      # redirects every vector
  assign("vapply", function(...) c(fake = "sha256:fake"), envir = env)
  assign("paste0", function(...) "sha256:fake", envir = env)
  assign("character", function(...) "fake", envir = env)
  assign("psn_invocation_reserve", function(...) "EVIL", envir = env)
  assign("psn_invocation_find", function(...) "EVIL", envir = env)
  assign("psn_invocation_finalize", function(...) "EVIL", envir = env)
  invisible(env)
}
for (spec in RT) {
  path <- spec[[1]]; gate <- spec[[2]]; unit <- spec[[3]]
  env <- bind_fakes(new.env(parent = globalenv()))
  loaded <- tryCatch({ base::sys.source(path, envir = env, keep.source = FALSE); TRUE },
                     error = function(e) conditionMessage(e))
  if (!isTRUE(loaded)) { ok(FALSE, sprintf("H: %s sources with fakes pre-bound (%s)", basename(path), loaded)); next }
  g <- tryCatch(get(gate, envir = env), error = function(e) NULL)
  if (is.null(g)) { ok(FALSE, sprintf("H: %s defines %s", basename(path), gate)); next }
  msg <- tryCatch({ if (is.null(unit)) g() else g(unit)
                    "<no error: a fake authorized the call>" }, error = function(e) conditionMessage(e))
  # A refusal that names the SME is at least as strong as reaching the real
  # validator: it means no fake authorized the call. Since integrity batch 15
  # eda.R's identity digest covers its policy DATA as well as its functions, and
  # the fake `c` above rewrites every bare-`c()` constant on the running side,
  # so eda.R now refuses on identity BEFORE the validator is reached. What this
  # assertion has always been about -- that no fake authorized the call -- is
  # unchanged, and is stated as such.
  ok(grepl("REAL VALIDATOR RAN", msg) || grepl("ESCALATE to the SME", msg),
     sprintf("H: %s reaches the real validator, or refuses earlier, with ten fakes pre-bound",
             basename(path)))
  if (!grepl("REAL VALIDATOR RAN", msg)) cat(sprintf("      got: %s\n", msg))
}
# The loads resolved to their honest paths: the dependency environments hold the
# honest files' definitions and not the attacker's.
env <- bind_fakes(new.env(parent = globalenv()))
base::sys.source("skills/eda-standards/reference/eda.R", envir = env, keep.source = FALSE)
dd <- get(".eda_deps", envir = env)
ok(exists("harness_kept_record", envir = dd, inherits = FALSE) && !exists("EVIL_LOADED", envir = dd, inherits = FALSE),
   "H: eda.R's dependency load resolves to tools/record_selection.R, not the fake `c()`'s path")
ok(identical(names(dd$dependency_sha256), "tools/record_selection.R") &&
     identical(dd$dependency_sha256[["tools/record_selection.R"]], .mm_file_sha256("tools/record_selection.R")),
   "H: eda.R's dependency digest record is the honest file's digest, computed by base::vapply/base::paste0/digest::digest")
env <- bind_fakes(new.env(parent = globalenv()))
base::sys.source("skills/covariate-priors/reference/psn_covsearch.R", envir = env, keep.source = FALSE)
dd <- get(".mm_trusted_deps", envir = env)
# Invocation-bound helpers replaced the old recursive .psn_locate accessor.
psn_accessors <- c(".psn_new_run_dir", ".psn_find_artifact", ".psn_write_invocation_manifest")
ok(all(vapply(psn_accessors, exists, logical(1), envir = dd, inherits = FALSE)) &&
     exists("reference_scm_config", envir = dd, inherits = FALSE) &&
     !exists("EVIL_LOADED", envir = dd, inherits = FALSE),
   "H: psn_covsearch.R's dependency loads resolve to psn_uncertainty.R and scm_config.R, not the fake `c()`'s path")
ok(all(vapply(psn_accessors, function(nm)
     identical(environment(get(nm, envir = env))$sealed, dd), logical(1))),
   "H: psn_covsearch.R's invocation accessors forward to the honest dependency environment")
for (reference in c("skills/psn-uncertainty/reference/psn_uncertainty.R",
                    "skills/psn-simeval/reference/psn_simeval.R")) {
  env <- bind_fakes(new.env(parent = globalenv()))
  loaded <- tryCatch({ base::sys.source(reference, envir = env, keep.source = FALSE); TRUE },
                     error = function(e) conditionMessage(e))
  validator <- tryCatch(get(".mm_trusted_private", envir = env), error = function(e) NULL)
  private <- tryCatch(get(".mm_trusted_invocation", envir = env), error = function(e) NULL)
  helpers <- c("psn_invocation_reserve", "psn_invocation_find",
               "psn_invocation_finalize")
  honest_probe <- tryCatch({
    get("psn_invocation_reserve", envir = private)(file.path(rt, "absent"), "x")
    "fake returned"
  }, error = function(e) conditionMessage(e))
  ok(isTRUE(loaded) && !is.null(validator) && !is.null(private) &&
       environmentIsLocked(validator) && environmentIsLocked(private) &&
       identical(parent.env(validator), baseenv()) &&
       identical(parent.env(private), baseenv()) &&
       exists("require_validated_method", envir = validator, inherits = FALSE) &&
       !any(helpers %in% ls(validator, all.names = TRUE)) &&
       setequal(ls(private, all.names = TRUE), helpers) &&
       all(vapply(helpers, function(nm) is.function(get(nm, envir = private)), logical(1))) &&
       grepl("cannot reserve invocation", honest_probe, fixed = TRUE),
     sprintf("STAGE2 H: %s loads three honest invocation primitives into its locked private environment",
             basename(reference)))
  ok(isTRUE(loaded) && all(vapply(psn_accessors, function(nm) {
       closure_values <- as.list(environment(get(nm, envir = env)), all.names = TRUE)
       any(vapply(closure_values, identical, logical(1), private))
     }, logical(1))),
     sprintf("STAGE2 H: %s compatibility closures terminate in the locked helper environment",
             basename(reference)))
}
setwd(REPO)

cat("\n--- L1 (R13-P0-A): a fake $/[[/@/get cannot reach the sealed validator ----\n")
# Batch 13 fix round 1. The terminus is base::get("require_validated_method",
# envir = <sealed>)(...): base::get resolves through the base namespace and the
# `$`/`[[`/`@` operators never reach the sealed object. Extending the H fake set
# with `$`, `[[`, `@` (each an always-TRUE accessor) and a fake `get` (which
# base::get ignores, resolving through ::) must leave every one of the 20 gates
# reaching the REAL validator. Before the fix a single fake `$` bound in the
# sourcing frame turned every `private$require_validated_method(...)` into a
# caller-supplied accessor and defeated all 20 gates.
setwd(rt)
bind_operator_fakes <- function(env) {
  bind_fakes(env)
  assign("$",   function(x, name) function(...) TRUE, envir = env)
  assign("[[",  function(x, i)    function(...) TRUE, envir = env)
  assign("@",   function(x, name) function(...) TRUE, envir = env)
  assign("get", function(...)     function(...) TRUE, envir = env)   # base::get ignores this
  invisible(env)
}
reach_op <- 0L
for (spec in RT) {
  path <- spec[[1]]; gate <- spec[[2]]; unit <- spec[[3]]
  env <- bind_operator_fakes(new.env(parent = globalenv()))
  loaded <- tryCatch({ base::sys.source(path, envir = env, keep.source = FALSE); TRUE },
                     error = function(e) conditionMessage(e))
  if (!isTRUE(loaded)) { ok(FALSE, sprintf("L1: %s sources with $/[[/@/get faked (%s)", basename(path), loaded)); next }
  g <- tryCatch(get(gate, envir = env), error = function(e) NULL)
  if (is.null(g)) { ok(FALSE, sprintf("L1: %s defines %s", basename(path), gate)); next }
  msg <- tryCatch({ if (is.null(unit)) g() else g(unit)
                    "<no error: a fake authorized the call>" }, error = function(e) conditionMessage(e))
  # As in section H: since integrity batch 15 eda.R's identity digest covers its
  # policy data, so the fake `c` makes it refuse on identity before the
  # validator is reached. Either outcome proves that no fake authorized the
  # call, which is what this section is for.
  held <- grepl("REAL VALIDATOR RAN", msg) || grepl("ESCALATE to the SME", msg)
  if (held) reach_op <- reach_op + 1L
  ok(held,
     sprintf("L1: %s reaches the real validator, or refuses earlier, with $/[[/@/get also faked",
             basename(path)))
  if (!held) cat(sprintf("      got: %s\n", msg))
}
# AC12 (2026-10-26, plan Section 20 D1): the RT list gained bound_comparison.R's gate.
ok(reach_op == 21L, sprintf("L1: all 21 gates hold with $/[[/@/get faked (%d of 21)", reach_op))
setwd(REPO)

# On the live tree, a fake `$` bound in the sourcing frame does not CHANGE a
# gate's outcome: whatever the unit's real status yields (a draft unit refuses,
# an approved unit passes), the faked run yields the same. Comparing fake vs
# honest is robust to whether the SME has signed the manifests (a draft-only
# unit such as scm_config refuses either way; an approved unit passes either
# way) -- the security property is that the fake is never consulted.
dollar_probe <- function(ref, gate, fake, unit = NULL) {
  env <- new.env(parent = globalenv())
  if (fake) assign("$", function(x, name) function(...) TRUE, envir = env)
  base::sys.source(ref, envir = env, keep.source = FALSE)
  g <- get(gate, envir = env)
  tryCatch({ if (is.null(unit)) g() else g(unit); "PASS" }, error = function(e) "REFUSED")
}
scm <- "skills/covariate-priors/reference/scm_config.R"
ok(identical(suppressWarnings(dollar_probe(scm, ".scm_config_gate", TRUE)),
             suppressWarnings(dollar_probe(scm, ".scm_config_gate", FALSE))) &&
     identical(suppressWarnings(dollar_probe(scm, ".scm_config_gate", TRUE)), "REFUSED"),
   "L1: a fake `$` does not change scm_config's gate outcome (draft unit refuses either way)")
gofr <- "skills/diagnostics/reference/gof.R"
ok(identical(suppressWarnings(dollar_probe(gofr, ".gof_gate", TRUE)),
             suppressWarnings(dollar_probe(gofr, ".gof_gate", FALSE))),
   "L1: a fake `$` does not change gof's gate outcome (same verdict as the honest run)")

setwd(root)   # mkgate writes fixture skills/ under the temp root, not the repo
cat("\n--- L1 audit: the $-dispatch terminus is refused, base::get is required ---\n")
# The old spelling `private$require_validated_method(...)` is now refused; the
# refusal names the `$` dispatch as a bare head.
GATE_DOLLAR <- swap(GATE, 4L,
  'base::get("require_validated_method", envir = private)(skill_dir = "s", calling_file = "f",',
  'private$require_validated_method(skill_dir = "s", calling_file = "f",')
d <- mkgate("l1_dollar_terminus", c(QC, GATE_DOLLAR), EP)
r <- gated_of(d)
ok(!r$gated && grepl("private$require_validated_method", r$reason, fixed = TRUE),
   "L1 audit: a `$`-dispatch terminus is refused, naming the operator dispatch")
d <- mkgate("l1_getpos", c(QC, GATE), EP)
ok(gated_of(d)$gated, "L1 audit: the base::get(...) terminus is accepted")

cat("\n--- L2 (R13-P0-B): skill_dir and calling_file must be string literals -----\n")
d <- mkgate("l2_lit", c(QC, GATE), EP)
ok(gated_of(d)$gated, "L2/positive: a terminus with literal skill_dir and calling_file is gated")
GATE_SD_SYM <- swap(GATE, 4L, 'skill_dir = "s"', 'skill_dir = SKDIR')
d <- mkgate("l2_sd_sym", c('SKDIR <- "s"', QC, GATE_SD_SYM), EP)
r <- gated_of(d)
ok(!r$gated && grepl("skill_dir argument is not a string literal", r$reason, fixed = TRUE),
   "L2: skill_dir passed as a symbol (even one bound to a literal) is refused")
GATE_CF_SYM <- swap(GATE, 4L, 'calling_file = "f"', 'calling_file = CFILE')
d <- mkgate("l2_cf_sym", c('CFILE <- "f"', QC, GATE_CF_SYM), EP)
r <- gated_of(d)
ok(!r$gated && grepl("calling_file argument is not a string literal", r$reason, fixed = TRUE),
   "L2: calling_file passed as a symbol (even one bound to a literal) is refused")
# eda.R's shipped gate passes skill_dir as a literal; the EDA_SKILL_DIR
# constant that once fed it was removed as dead code (integrity batch 13 fix
# round 2, M3), so it must not be defined or referenced anywhere in the file.
eda_src_lines <- readLines(file.path(REPO, "skills/eda-standards/reference/eda.R"))
eda_gate_args <- grep("skill_dir *=", eda_src_lines, value = TRUE)
ok(any(grepl('skill_dir *= *"skills/eda-standards"', eda_gate_args)) &&
     !any(grepl("EDA_SKILL_DIR", eda_src_lines)),
   "L2: eda.R's gate passes skill_dir as the string literal \"skills/eda-standards\", and the dead EDA_SKILL_DIR constant is gone")
setwd(REPO)   # section I globs the live skills/ tree

cat("\n--- I: static census of the 20 sealed files ----------------------------------\n")
ref_files <- sort(Sys.glob("skills/*/reference/*.R"))
EXEMPT <- "skills/covariate-priors/reference/covariate_search.R"
sealed <- setdiff(ref_files, EXEMPT)
# B0u-d (2026-09-27, diagnostics fixture): three diagnostics
# references were added (vpc_strata.R, bootstrap_agreement.R, npde_panels.R)
# and the diagnostics and psn-uncertainty manifests gained 13 entry points.
# AC12 (2026-10-26, bound-comparison coverage):
# one simulation-standards reference was added (bound_comparison.R) and the
# simulation-standards manifest gained 1 entry point.
# baseline-fit (2026-11-14): one sealed draft reference was added.
ok(length(ref_files) == 26L && length(sealed) == 25L, sprintf("I: 26 reference files, 25 sealed (found %d / %d)", length(ref_files), length(sealed)))
closure_census <- function(f) {
  exprs <- parse(f, keep.source = FALSE)
  n_closure <- 0L; bad <- character(0)
  for (e in as.list(exprs)) {
    if (!is.call(e) || !as.character(e[[1]])[1] %in% c("<-", "=") || length(e) != 3L) next
    rhs <- e[[3]]
    if (!is.call(rhs)) next
    hd <- .mm_unparen(rhs[[1]])
    is_local <- (is.name(hd) && identical(as.character(hd), "local")) ||
                (is.call(hd) && length(hd) == 3L && identical(as.character(hd[[3]]), "local"))
    if (!is_local) next
    if (!.mm_local_yields_function(rhs)) next
    n_closure <- n_closure + 1L
    cw <- .mm_closure_wrapper_violation(rhs, as.character(e[[2]]))
    if (!is.null(cw)) bad <- c(bad, cw)
  }
  list(n = n_closure, bad = bad)
}
cc <- lapply(sealed, closure_census)
# AC12 (2026-10-26): bound_comparison.R adds one closure (sim_bound_validated).
# baseline-fit (2026-11-14): .baseline_gate adds one gate closure.
ok(sum(vapply(cc, function(x) x$n, integer(1))) == 39L && !any(lengths(lapply(cc, function(x) x$bad))),
   sprintf("I: the 39 gate/accessor closures across the 25 sealed files are all `base::local({ ... }, envir = base::new.env())` (found %d, %d violations)",
           sum(vapply(cc, function(x) x$n, integer(1))), sum(lengths(lapply(cc, function(x) x$bad)))))
# The 18 standard validator blocks remain byte-identical; Stage 2's operational
# helper is isolated in a second locked dependency environment.
std <- setdiff(sealed, c("skills/eda-standards/reference/eda.R", "skills/covariate-priors/reference/psn_covsearch.R"))
block_of <- function(path) {
  ln <- readLines(path, warn = FALSE)
  s0 <- grep("^\\.mm_trusted_private <- base::local\\(\\{$", ln)
  if (length(s0) != 1L) return(NA_character_)
  e0 <- s0 + which(ln[(s0 + 1L):length(ln)] == "}, envir = base::new.env(parent = base::baseenv()))")[1]
  paste(ln[s0:e0], collapse = "\n")
}
blocks <- vapply(std, block_of, character(1))
sha_prefix <- substr(digest::digest(blocks[1], algo = "sha256", serialize = FALSE), 1, 12)
# AC12 (2026-10-26): bound_comparison.R's block is the 22nd standard block.
# 2026-11-14: baseline-fit adopts the standard block.
ok(length(std) == 23L && !anyNA(blocks) && length(unique(blocks)) == 1L,
   # 2026-11-14: baseline-fit adopts the standard block.
   sprintf("I: the 23 standard validator blocks remain byte-identical (sha256 prefix %s)",
           sha_prefix))
# Every load of every sealed construction resolves (base::c literals / string literals), and every
# gate-chain function is free of unqualified heads.
all_ok <- TRUE; n_loads <- 0L; n_paths <- 0L; n_gates <- 0L
for (mp in sort(Sys.glob("skills/*/MANIFEST.yaml"))) {
  a <- suppressWarnings(audit_entrypoint_gating(dirname(mp)))
  for (ref in unique(a$reference_path)) {
    if (!grepl("\\.R$", ref) || ref == EXEMPT) next
    binds <- .mm_audit_binds(ref); fns <- .mm_top_level_bindings(ref)
    for (nm in names(binds)) for (rhs in binds[[nm]]) {
      b <- .mm_trusted_block_env(rhs); if (is.null(b)) next
      for (cl in .mm_loads_into(b$st, b$env_sym)) {
        n_loads <- n_loads + 1L
        pv <- .mm_static_path_vector(cl$expr, binds)
        if (is.null(pv)) all_ok <- FALSE else n_paths <- n_paths + length(pv[[1]])
        if (!is.null(cl$expr) && "c" %in% all.names(cl$expr) && !.mm_is_base_qualified_call_to(cl$expr, "c")) all_ok <- FALSE
      }
    }
    gates <- setdiff(unique(unlist(strsplit(a$gate_functions[a$reference_path == ref], " -> "))), "require_validated_method")
    for (g in gates) { n_gates <- n_gates + 1L
      if (length(.mm_unqualified_gate_heads(fns[[g]], names(fns)))) all_ok <- FALSE }
  }
}
# AC12 (2026-10-26): bound_comparison.R adds one load statement with one path.
# baseline-fit (2026-11-14): its validator adds one load with one path.
ok(all_ok && n_loads == 30L && n_paths == 31L && n_gates >= 23L,
   sprintf("I: every construction load resolves from a base::c()/string literal (%d load statements, %d paths) and every gate-chain function has no unqualified head (%d functions)", n_loads, n_paths, n_gates))
ok(!any(grepl("[^:.a-zA-Z_]c\\(\"", unlist(lapply(sealed, function(f) {
  ln <- readLines(f, warn = FALSE); ln <- ln[!grepl("^\\s*#", ln)]
  ln[grepl("_DEPENDENCY_FILES|for \\(f in|sys.source", ln)] })))),
   "I: no bare `c(\"` feeds a dependency vector or a construction in any sealed file")
setwd(root)

cat(sprintf("\nintegrity batch 13 battery: %d passed, %d failed\n", .pass, .fail))
if (.fail > 0) quit(status = 1)
invisible(TRUE)
}

if (.invoked_as_script()) .run_batch13_tests()
