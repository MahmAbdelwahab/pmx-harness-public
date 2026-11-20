#!/usr/bin/env Rscript
# Integrity batch 12 battery (Codex round 11 findings, SME decisions 2026-09-05).
#
# Sections:
#   A  R11-P0-1    the trusted construction's primitives are written with the
#                  exact `base::` qualification (base::local, base::new.env,
#                  base::baseenv, base::file.exists, base::stop,
#                  base::sys.source, base::lockEnvironment), each admitted only
#                  in its own position; bare, `base:::`, `pkg::`, `$`, `[[`,
#                  `(`-wrapped and computed heads are refused.
#   B  R11-P0-2    the validator lives in its own locked environment that loads
#                  ONLY tools/method_manifest.R; dependencies load into a second
#                  locked environment; the audit reports both and derives
#                  `depends:` from the second; a dependency file that defines
#                  the validator or the digest helper cannot reach the gate.
#   C  R11-P0-3    a path constant is resolved through a ROOT-ONLY binding
#                  index: exactly one plain root assignment to a literal
#                  character vector, or nothing.
#   D  R11-P0-4    whitelist form (d) requires the bare-qualified heads
#                  `base::file.exists` and `base::stop`; `shim$file.exists`,
#                  `shim$stop` and `(file.exists)(f)` are refused.
#   E  R11-P2-1    gate.R digests a member in one bounded operation returning
#                  {digest, reason}; the type is re-checked after hashing.
#   F  R11-P2-2    the release lock cleans up after itself when the holder
#                  record cannot be written, and .release_lock_free() stops
#                  loudly when the directory remains.
#   G  R11-P3-2    per-file construction census over the 21 reference files:
#                  covariate_search.R exempt, exactly one qualified validator
#                  construction and one qualified validator load per remaining
#                  file, the 18 standard blocks byte-identical.
#   H  R11-P0-1/2  at run time: with fake sys.source/new.env/lockEnvironment/
#                  local/file.exists/baseenv pre-bound in the sourcing
#                  environment, every sealed reference still reaches the real
#                  validator; a dependency that defines the validator does not
#                  reach the gate of eda.R or psn_covsearch.R.
#
# R11-P1-1 (approve/revoke under the ledger lock) is pinned in
# tools/tests/test_batch4.R section J3, where the ledger fixtures live.
# R11-P1-2 / P3-1 (the hook without PyYAML; quoted paths) are pinned in
# tools/tests/test_protect_paths.sh.
#
# Run with:  Rscript tools/tests/test_batch12.R
# Exits non-zero if any assertion fails.

# SAFE TO SOURCE: pipelines tar_source("tools") which recurses into
# tools/tests/; everything observable is guarded behind .invoked_as_script().
.invoked_as_script <- function() {
  fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  length(fa) > 0 && grepl("test_batch12\\.R$", sub("^--file=", "", fa[1]))
}

.run_batch12_tests <- function() {

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
root <- tempfile("b12fix"); dir.create(root)
old_wd <- setwd(root); on.exit(setwd(old_wd), add = TRUE)
dir.create("tools")
writeLines(c("keep_rows <- function(d) d", "helper <- 1"), "tools/record_selection.R")
writeLines(c("require_validated_method <- function(...) TRUE"), "tools/bad_dep.R")
writeLines(c(".mm_file_sha256 <- function(path) \"sha256:fake\""), "tools/bad_sha.R")
file.copy(file.path(REPO, "tools", "method_manifest.R"),
          "tools/method_manifest.R", overwrite = TRUE)
sha <- function(p) .mm_file_sha256(p)

mkgate <- function(name, body_lines, ep_lines, unit = "alpha", depends = character(0)) {
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
               paste0("  ", unit, ":"), "    status: approved",
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
validate_msg <- function(d) tryCatch({ validate_method_manifest(file.path(d, "MANIFEST.yaml")); "" },
                                     error = function(e) conditionMessage(e))

# The qualified standard block (decision 1) and the house gate/entry point.
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
# The qualified dependency environment (decision 2), eda.R's loop shape.
QD <- c(
  '.mm_trusted_deps <- base::local({',
  '  d <- base::new.env(parent = base::baseenv())',
  '  for (f in DEPS) {',
  '    if (!base::file.exists(f)) base::stop("missing ", f, call. = FALSE)',
  '    base::sys.source(f, envir = d, keep.source = FALSE)',
  '  }',
  '  d$dependency_sha256 <- base::vapply(DEPS, function(f) base::paste0("sha256:", f), base::character(1))',
  '  base::lockEnvironment(d, bindings = TRUE)',
  '  d',
  '}, envir = base::new.env(parent = base::baseenv()))',
  'keep_rows <- base::local({ sealed <- .mm_trusted_deps; function(...) base::get("keep_rows", envir = sealed)(...) }, envir = base::new.env())')
swap <- function(block, line, from, to) { block[line] <- sub(from, to, block[line], fixed = TRUE); block }

cat("\n--- A: R11-P0-1, base::-qualified construction primitives --------------\n")

d <- mkgate("a_pos", c(QC, GATE), EP)
ok(gated_of(d)$gated, "A/positive: the base::-qualified standard block is trusted")
ok(grepl("tools/method_manifest.R", gated_of(d)$loads, fixed = TRUE),
   "A/positive: the qualified block reports tools/method_manifest.R as its trusted load")

# The predicate itself.
ok(.mm_is_base_qualified_call_to(quote(base::sys.source(f, envir = e)), "sys.source") &&
     !.mm_is_base_qualified_call_to(quote(sys.source(f, envir = e)), "sys.source") &&
     !.mm_is_base_qualified_call_to(quote(base:::sys.source(f, envir = e)), "sys.source") &&
     !.mm_is_base_qualified_call_to(quote(utils::sys.source(f, envir = e)), "sys.source") &&
     !.mm_is_base_qualified_call_to(quote(shim$sys.source(f, envir = e)), "sys.source") &&
     !.mm_is_base_qualified_call_to(quote((base::sys.source)(f, envir = e)), "sys.source") &&
     !.mm_is_base_qualified_call_to(quote("base"::sys.source(f, envir = e)), "sys.source") &&
     !.mm_is_base_qualified_call_to(quote(base::"sys.source"(f, envir = e)), "sys.source") &&
     !.mm_is_base_qualified_call_to(quote(base::sys.source(f, envir = e)), "source"),
   "A: .mm_is_base_qualified_call_to() admits exactly `base::<symbol>` with the literal symbol base on the left")

# Each of the seven heads, written bare, is refused in its own position.
# The wrapper's own frame: base::local()'s DEFAULT envir is `new.env()`
# evaluated in the sourcing frame, so a caller-shadowed new.env supplies the
# block's frame (probe scratchpad/b12/probe_colon.R). The explicit
# `envir = base::new.env()` is required and the default is refused.
d <- mkgate("a_default_envir", c(swap(QC, 8L, "}, envir = base::new.env(parent = base::baseenv()))", "})"), GATE), EP)
r <- gated_of(d)
ok(!r$gated, "A: base::local({ ... }) with the DEFAULT envir is refused")
ok(grepl("envir = base::new.env(parent = base::baseenv())", r$reason, fixed = TRUE),
   "A: the default-envir refusal names the explicit `envir = base::new.env(parent = base::baseenv())` requirement (batch 13 wording)")
for (bad_envir in c("envir = new.env()", "envir = base::new.env(parent = globalenv())",
                    "envir = environment()", "envir = shim$new.env()")) {
  d <- mkgate(paste0("a_envir_", gsub("[^A-Za-z0-9]", "", bad_envir)),
              c('shim <- list(new.env = function() globalenv())',
                swap(QC, 8L, "envir = base::new.env(parent = base::baseenv())", bad_envir), GATE), EP)
  ok(!gated_of(d)$gated, sprintf("A: a wrapper frame written `%s` is refused", bad_envir))
}

bare <- list(
  "local"           = list(1L, "base::local(",          "local("),
  "new.env"         = list(2L, "base::new.env(",        "new.env("),
  "baseenv"         = list(2L, "base::baseenv()",       "baseenv()"),
  "file.exists"     = list(3L, "base::file.exists(",    "file.exists("),
  "stop"            = list(4L, "base::stop(",           "stop("),
  "sys.source"      = list(5L, "base::sys.source(",     "sys.source("),
  "lockEnvironment" = list(6L, "base::lockEnvironment(", "lockEnvironment("))
for (nm in names(bare)) {
  s <- bare[[nm]]
  d <- mkgate(paste0("a_bare_", nm), c(swap(QC, s[[1]], s[[2]], s[[3]]), GATE), EP)
  ok(!gated_of(d)$gated, sprintf("A: the bare, unqualified `%s` inside the block is refused", nm))
}
d <- mkgate("a_bare_msg", c(swap(QC, 5L, "base::sys.source(", "sys.source("), GATE), EP)
ok(grepl("base::sys.source", gated_of(d)$reason, fixed = TRUE),
   "A: the refusal of a bare sys.source() names the `base::sys.source` qualification requirement")

# `base:::` (the internal-namespace spelling) is refused for every head.
for (nm in names(bare)) {
  s <- bare[[nm]]
  d <- mkgate(paste0("a_triple_", nm),
              c(swap(QC, s[[1]], s[[2]], sub("^base::", "base:::", s[[2]])), GATE), EP)
  ok(!gated_of(d)$gated, sprintf("A: `base:::%s` inside the block is refused", nm))
}

# Other qualifiers, object dispatch, parenthesised and string-qualified heads.
others <- list(
  "utils::sys.source"       = swap(QC, 5L, "base::sys.source(", "utils::sys.source("),
  "base::eval as the load"  = swap(QC, 5L, 'base::sys.source("tools/method_manifest.R", envir = e, keep.source = FALSE)',
                                            'base::eval(quote(1), envir = e)'),
  "shim$sys.source"         = c('shim <- list(sys.source = function(...) NULL)',
                                swap(QC, 5L, "base::sys.source(", "shim$sys.source(")),
  "(base::sys.source)(...)" = swap(QC, 5L, "base::sys.source(", "(base::sys.source)("),
  "\"base\"::sys.source"    = swap(QC, 5L, "base::sys.source(", "\"base\"::sys.source("),
  "shim$new.env"            = c('shim <- list(new.env = function(parent) base::new.env(parent = parent))',
                                swap(QC, 2L, "base::new.env(", "shim$new.env(")),
  "shim$baseenv"            = c('shim <- list(baseenv = function() globalenv())',
                                swap(QC, 2L, "base::baseenv()", "shim$baseenv()")),
  "shim[[\"lockEnvironment\"]]" = c('shim <- list(lockEnvironment = function(...) NULL)',
                                    swap(QC, 6L, "base::lockEnvironment(", 'shim[["lockEnvironment"]](')),
  "base::local wrapped in (" = swap(QC, 1L, "base::local(", "(base::local)("))
for (nm in names(others)) {
  d <- mkgate(paste0("a_other_", gsub("[^A-Za-z0-9]", "", nm)), c(others[[nm]], GATE), EP)
  ok(!gated_of(d)$gated, sprintf("A: a construction written with `%s` is refused", nm))
}

# A qualified head is admitted only in its own position.
misplaced <- list(
  "base::sys.source as the construction" = swap(QC, 2L, "base::new.env(parent = base::baseenv())",
                                                 'base::sys.source("tools/method_manifest.R")'),
  "base::new.env as the load"            = swap(QC, 5L, "base::sys.source(", "base::new.env("),
  "base::lockEnvironment as the load"    = swap(QC, 5L, "base::sys.source(", "base::lockEnvironment("),
  "base::new.env as the seal"            = swap(QC, 6L, "base::lockEnvironment(", "base::new.env("),
  "base::stop as the check"              = swap(QC, 3L, "base::file.exists(", "base::stop("),
  "base::file.exists as the stop"        = swap(QC, 4L, "base::stop(", "base::file.exists("),
  "base::local as the construction"      = swap(QC, 2L, "base::new.env(parent = base::baseenv())",
                                                 "base::local(base::new.env(parent = base::baseenv()))"),
  "base::baseenv as the construction"    = swap(QC, 2L, "base::new.env(parent = base::baseenv())",
                                                 "base::baseenv()"))
for (nm in names(misplaced)) {
  d <- mkgate(paste0("a_mis_", gsub("[^A-Za-z0-9]", "", nm)), c(misplaced[[nm]], GATE), EP)
  ok(!gated_of(d)$gated, sprintf("A: %s is refused (each head is admitted only in its own position)", nm))
}

# Qualification is required INSIDE the block only; at the top level `::` stays
# refused as before, and a top-level shadow of a primitive is still refused.
d <- mkgate("a_top_qualified", c('k <- base::sys.source', QC, GATE), EP)
ok(!gated_of(d)$gated, "A: a top-level `base::sys.source` alias is still refused")
d <- mkgate("a_top_shadow", c('sys.source <- function(...) NULL', QC, GATE), EP)
ok(!gated_of(d)$gated, "A: a top-level binding of a construction primitive is still refused")

cat("\n--- B: R11-P0-2, validator and dependencies in separate environments ---\n")

DEPS1 <- 'DEPS <- base::c("tools/record_selection.R")'
d <- mkgate("b_pos", c(DEPS1, QC, QD, GATE), EP)
r <- gated_of(d)
ok(r$gated, "B/positive: a validator environment plus a separate dependency environment is trusted")
ok(grepl("tools/method_manifest.R", r$loads, fixed = TRUE) &&
     grepl("tools/record_selection.R", r$loads, fixed = TRUE),
   "B: trusted_loads reports both the validator load and the dependency load")
ok(grepl("record_selection\\.R", validate_msg(d)) && grepl("depends", validate_msg(d)),
   "B: the dependency environment's load must be declared under depends: (derivation unchanged)")
d <- mkgate("b_pos_dep", c(DEPS1, QC, QD, GATE), EP, depends = "tools/record_selection.R")
ok(!nzchar(validate_msg(d)), "B: the same unit with the dependency declared validates")
# write_method_manifest() derives the dependency from the second environment.
spec <- list(alpha = list(reference_path = file.path(d, "reference", "method.R"),
                          entrypoints = "ep", evidence = "SKILL.md", reason = "x"))
suppressMessages(write_method_manifest(d, spec))
m <- read_method_manifest(file.path(d, "MANIFEST.yaml"))
ok(identical(vapply(m$units$alpha$depends, function(x) x$path, character(1)),
             "tools/record_selection.R"),
   "B: write_method_manifest() derives depends: from the dependency environment's loads")

# A dependency load INTO THE VALIDATOR ENVIRONMENT is refused (the old eda.R shape, qualified).
old_eda <- c(
  '.mm_trusted_private <- base::local({',
  '  e <- base::new.env(parent = base::baseenv())',
  '  for (f in base::c("tools/method_manifest.R", DEPS)) {',
  '    if (!base::file.exists(f)) base::stop("missing ", f, call. = FALSE)',
  '    base::sys.source(f, envir = e, keep.source = FALSE)',
  '  }',
  '  base::lockEnvironment(e, bindings = TRUE)',
  '  e',
  '}, envir = base::new.env(parent = base::baseenv()))')
d <- mkgate("b_val_dep", c(DEPS1, old_eda, GATE), EP)
r <- gated_of(d)
ok(!r$gated, "B: a dependency loaded into the validator environment is refused")
ok(grepl("only tools/method_manifest.R", r$reason, fixed = TRUE),
   "B: the refusal says the validator environment loads only tools/method_manifest.R")
# The dependency environment may not load the validator file.
d <- mkgate("b_dep_val", c('DEPS <- base::c("tools/method_manifest.R", "tools/record_selection.R")',
                           QC, QD, GATE), EP)
ok(!gated_of(d)$gated, "B: a dependency environment that loads tools/method_manifest.R is refused")
# The dependency environment may not load a file that defines the validator.
d <- mkgate("b_dep_defines_validator", c('DEPS <- base::c("tools/bad_dep.R")', QC, QD, GATE), EP)
r <- gated_of(d)
ok(!r$gated && grepl("require_validated_method", r$reason, fixed = TRUE),
   "B: a dependency file that defines require_validated_method() is refused by the audit")
# The gate may not dispatch off the dependency environment.
d <- mkgate("b_gate_off_deps", c(DEPS1, QC, QD,
                                 sub(".mm_trusted_private", ".mm_trusted_deps", GATE, fixed = TRUE)), EP)
ok(!gated_of(d)$gated, "B: a gate dispatching off the dependency environment is refused")
# A block that loads into the symbol it returns but is neither a valid
# validator nor a valid dependency environment refuses the whole file, so it
# cannot drop silently out of the trusted_loads report.
d <- mkgate("b_orphan_block", c('DEPS <- base::c("tools/record_selection.R")', QC,
                                swap(QD, 5L, "envir = d", "envir = d, chdir = 2"), GATE), EP)
ok(!gated_of(d)$gated,
   "B: a load-bearing block that qualifies as neither environment refuses the file")

cat("\n--- C: R11-P0-3, root-only path constants ------------------------------\n")

# Codex's scenario: the only binding of DEPS is function-local.
d <- mkgate("c_local_const", c('helper <- function() DEPS <- "tools/record_selection.R"',
                               QC, QD, GATE), EP)
ok(!gated_of(d)$gated,
   "C: a path constant whose only binding is function-local is refused")
binds <- .mm_binding_index(file.path(d, "reference", "method.R"))
ok(is.null(.mm_static_path_vector(quote(DEPS), binds)),
   "C: .mm_static_path_vector() does not resolve a function-local binding")
# Root constant plus a function-local rebinding: refused (bound more than once).
d <- mkgate("c_root_and_local", c(DEPS1, 'helper <- function() DEPS <- "x.R"', QC, QD, GATE), EP)
ok(!gated_of(d)$gated,
   "C: a root path constant that is also bound inside a function is refused")
# A root constant that is not a literal character vector.
d <- mkgate("c_root_computed", c('DEPS <- getOption("deps")', QC, QD, GATE), EP)
ok(!gated_of(d)$gated, "C: a root path constant that is not a literal vector is refused")
d <- mkgate("c_root_paste", c('DEPS <- paste0("tools/", "record_selection.R")', QC, QD, GATE), EP)
ok(!gated_of(d)$gated, "C: a root path constant built by paste0() is refused")
# A root constant bound by `=` is a plain root assignment too.
d <- mkgate("c_root_eq", c('DEPS = base::c("tools/record_selection.R")', QC, QD, GATE), EP)
ok(gated_of(d)$gated, "C/positive: a root constant written with `=` still resolves")
# The two shipped root-literal shapes.
binds <- .mm_binding_index(file.path(REPO, "skills", "eda-standards", "reference", "eda.R"))
ok(identical(.mm_static_path_vector(quote(EDA_DEPENDENCY_FILES), binds)$path,
             "tools/record_selection.R"),
   "C: eda.R's EDA_DEPENDENCY_FILES resolves through the root-only index")
# Since integrity batch 13 psn_covsearch.R names its dependencies as an in-block
# `base::c(...)` literal (its frame is parented on base and cannot read a root
# constant); the resolved load set is asserted through the audit report instead.
psn_loads <- local({ setwd(REPO); on.exit(setwd(root))
  r <- suppressWarnings(audit_entrypoint_gating("skills/covariate-priors"))
  r$trusted_loads[r$reference_path == "skills/covariate-priors/reference/psn_covsearch.R"][1] })
ok(grepl("skills/psn-uncertainty/reference/psn_uncertainty.R", psn_loads, fixed = TRUE) &&
     grepl("skills/covariate-priors/reference/scm_config.R", psn_loads, fixed = TRUE),
   "C: psn_covsearch.R's in-block dependency literal resolves to both dependencies in the audit report")

cat("\n--- D: R11-P0-4, form (d) requires bare-qualified heads ----------------\n")

SHIMD <- 'shim <- list(file.exists = function(...) TRUE, stop = function(...) NULL)'
dforms <- list(
  "shim$file.exists(...)" = swap(QC, 3L, "base::file.exists(", "shim$file.exists("),
  "shim$stop(...)"        = swap(QC, 4L, "base::stop(", "shim$stop("),
  "(file.exists)(f)"      = swap(QC, 3L, "base::file.exists(", "(file.exists)("),
  "(base::file.exists)(f)" = swap(QC, 3L, "base::file.exists(", "(base::file.exists)("),
  "shim[[\"stop\"]](...)" = swap(QC, 4L, "base::stop(", 'shim[["stop"]]('))
for (nm in names(dforms)) {
  d <- mkgate(paste0("d_", gsub("[^A-Za-z0-9]", "", nm)), c(SHIMD, dforms[[nm]], GATE), EP)
  ok(!gated_of(d)$gated, sprintf("D: a pre-load check written `%s` is refused", nm))
}

cat("\n--- E: R11-P2-1, one bounded digest operation with a reason -----------\n")

gt <- new.env(parent = globalenv())
sys.source(file.path(REPO, "tools", "gate.R"), envir = gt, keep.source = FALSE)
gd <- tempfile("b12gate"); dir.create(gd)
plain <- file.path(gd, "plain.csv"); writeLines("a,b", plain)
m1 <- gt$.gate_digest_member(plain)
ok(is.list(m1) && identical(m1$digest, digest::digest(file = plain, algo = "sha256")) &&
     identical(m1$reason, ""),
   "E: .gate_digest_member() returns {digest, reason} and the digest equals digest::digest(file=)")
ok(is.numeric(gt$GATE_DIGEST_TIME_LIMIT_SECONDS) && gt$GATE_DIGEST_TIME_LIMIT_SECONDS == 600,
   "E: the bounded-hashing limit is a named constant of 600 s")
ok(grepl("^sha256:[0-9a-f]{64}$", gt$.gate_artifact_cell_sha256(plain)),
   "E/positive: a regular file still digests to its bytes through the cell path")
# A regular but unreadable file: MISSING with the read error as the reason.
if (is_root) {
  cat("      (running as root; the unreadable-file case cannot be exercised)\n")
} else {
  unread <- file.path(gd, "unreadable.csv"); writeLines("secret", unread)
  Sys.chmod(unread, "000")
  ok(identical(gt$.gate_artifact_cell_sha256(unread), "MISSING"),
     "E: an unreadable regular file renders MISSING")
  why <- gt$.gate_artifact_cell_reason(unread)
  ok(grepl("ermission denied|cannot read|could not be digested", why),
     sprintf("E: the MISSING reason carries the read error (got: %s)", why))
  Sys.chmod(unread, "644")
}
# A FIFO is hashed under the time limit and comes back MISSING with a reason.
fifo <- file.path(gd, "pipe.csv")
mk <- suppressWarnings(system2("mkfifo", shQuote(fifo), stdout = FALSE, stderr = FALSE))
if (!identical(as.integer(mk), 0L)) {
  cat("      (mkfifo unavailable; the FIFO case cannot be exercised)\n")
} else {
  t0 <- Sys.time()
  m2 <- gt$.gate_digest_member(fifo, limit = 2)
  el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  ok(is.na(m2$digest) && grepl("2 s|time limit|exceeded", m2$reason) && el < 15,
     sprintf("E: digesting a FIFO returns within the limit with a reason (%.1f s: %s)", el, m2$reason))
}
# The type is re-checked after hashing: a member that changes type between the
# expansion and the digest is refused, with a reason that says so.
real_regular <- gt$.gate_is_regular_file
calls <- 0L
assign(".gate_is_regular_file", function(paths) {
  calls <<- calls + 1L
  if (calls >= 2L) rep(FALSE, length(paths)) else real_regular(paths)
}, envir = gt)
sw <- gt$.gate_artifact_cell_sha256(plain)
why <- gt$.gate_artifact_cell_reason(plain)
assign(".gate_is_regular_file", real_regular, envir = gt)
ok(identical(sw, "MISSING") && grepl("changed|no longer a regular file", why),
   sprintf("E: a member whose type changes after the type check is MISSING with a reason (got: %s)", why))

cat("\n--- F: R11-P2-2, lock acquisition cleanup -----------------------------\n")

ag <- new.env(parent = globalenv())
for (f in c("gate_package.R", "gate.R", "approve_gate.R"))
  sys.source(file.path(REPO, "tools", f), envir = ag, keep.source = FALSE)
ld <- tempfile("b12lock"); dir.create(ld)
ledger <- file.path(ld, "approvals.jsonl"); writeLines(character(0), ledger)
lock <- ag$.release_lock_path(ledger)
if (is_root) {
  cat("      (running as root; the unwritable-lock cases cannot be exercised)\n")
} else {
  old_umask <- Sys.umask("777")
  e <- tryCatch(ag$.release_lock_acquire(ledger), error = function(e) e)
  Sys.umask(old_umask)
  ok(inherits(e, "error"), "F: acquiring a lock whose holder record cannot be written stops")
  ok(inherits(e, "error") && grepl("holder", conditionMessage(e)),
     "F: the failure names the holder record it could not write")
  ok(!dir.exists(lock), "F: the lock directory is removed when the holder record cannot be written")
  # A lock that cannot be removed is reported loudly, never silently left behind.
  # The lock's PARENT is made read-only, so the directory itself cannot be
  # unlinked whatever is done to its contents or its own mode.
  h <- ag$.release_lock_acquire(ledger)
  Sys.chmod(ld, "555")
  e2 <- tryCatch(ag$.release_lock_free(h), error = function(e) e)
  Sys.chmod(ld, "755"); unlink(h, recursive = TRUE, force = TRUE)
  ok(inherits(e2, "error") && grepl("still exists|could not be removed", conditionMessage(e2)),
     "F: .release_lock_free() stops loudly when the lock directory remains")
}
h <- ag$.release_lock_acquire(ledger)
ok(dir.exists(h) && file.exists(file.path(h, "holder")), "F/positive: a normal acquisition records its holder")
ag$.release_lock_free(h)
ok(!dir.exists(h), "F/positive: a normal release removes the lock")

cat("\n--- G: R11-P3-2, per-file construction census --------------------------\n")

setwd(REPO)
ref_files <- sort(Sys.glob("skills/*/reference/*.R"))
# Three additional diagnostics
# references were added (vpc_strata.R, bootstrap_agreement.R, npde_panels.R)
# and the diagnostics and psn-uncertainty manifests gained 13 entry points.
# Bound comparisons:
# one simulation-standards reference was added (bound_comparison.R) and the
# simulation-standards manifest gained 1 entry point.
# baseline-fit (2026-11-14): one draft reference was added (baseline_fit.R).
ok(length(ref_files) == 26L, sprintf("G: 26 reference files are enumerated (found %d)", length(ref_files)))
EXEMPT <- "skills/covariate-priors/reference/covariate_search.R"
ok(EXEMPT %in% ref_files && .mm_has_placeholder_marker(EXEMPT),
   "G: covariate_search.R is the explicit exemption and carries the PLACEHOLDER marker")
count_qualified_mm_loads <- function(path) {
  exprs <- parse(path, keep.source = FALSE); n <- 0L
  walk <- function(x) {
    if (!is.call(x)) return(invisible(NULL))
    if (.mm_is_base_qualified_call_to(x, "sys.source")) {
      a <- .mm_match_args(x, c("file", "envir", "chdir", "keep.source"))
      if (is.character(a$file) && identical(a$file, "tools/method_manifest.R")) n <<- n + 1L
    }
    for (i in seq_along(x)) if (!is.null(x[[i]])) tryCatch(walk(x[[i]]), error = function(e) NULL)
  }
  for (e in exprs) tryCatch(walk(e), error = function(err) NULL)
  n
}
constructions_of <- function(path) {
  tlb <- .mm_top_level_bindings(path); binds <- .mm_binding_index(path)
  attr(binds, "attaches") <- .mm_file_attaches(path)
  attr(binds, "reflective") <- .mm_reflective_top_level_of_file(path, binds)
  attr(binds, "blocks") <- .mm_trusted_block_role_violations(binds, names(tlb), .mm_shadow_census_of(tlb))
  census <- .mm_shadow_census_of(tlb)
  v <- 0L; dep <- 0L
  for (nm in names(tlb)) {
    rhs <- tlb[[nm]]
    if (!is.call(rhs) || !.mm_is_base_qualified_call_to(rhs, "local")) next
    if (.mm_is_trusted_private_rhs(rhs, binds, names(tlb), census)) v <- v + 1L
    else if (.mm_is_trusted_dependency_rhs(rhs, binds, names(tlb), census)) dep <- dep + 1L
  }
  list(validator = v, dependency = dep, loads = count_qualified_mm_loads(path))
}
bad_v <- character(0); bad_l <- character(0)
for (f in setdiff(ref_files, EXEMPT)) {
  c3 <- constructions_of(f)
  if (c3$validator != 1L) bad_v <- c(bad_v, sprintf("%s (%d)", f, c3$validator))
  if (c3$loads != 1L) bad_l <- c(bad_l, sprintf("%s (%d)", f, c3$loads))
}
# baseline-fit (2026-11-14): the two pass labels below read 25/25 (were 24/24).
ok(length(bad_v) == 0L,
   sprintf("G: every non-exempt reference file has EXACTLY ONE qualified validator construction (%s)",
           if (length(bad_v)) paste(bad_v, collapse = ", ") else "25/25"))
ok(length(bad_l) == 0L,
   sprintf("G: every non-exempt reference file has EXACTLY ONE qualified load of tools/method_manifest.R (%s)",
           if (length(bad_l)) paste(bad_l, collapse = ", ") else "25/25"))
c_ex <- constructions_of(EXEMPT)
ok(c_ex$validator == 0L && c_ex$loads == 0L, "G: the exempt placeholder has no construction and no validator load")
NONSTANDARD <- c("skills/eda-standards/reference/eda.R",
                 "skills/covariate-priors/reference/psn_covsearch.R")
MULTI <- c(NONSTANDARD,
           "skills/psn-simeval/reference/psn_simeval.R",
           "skills/psn-uncertainty/reference/psn_uncertainty.R",
           "skills/diagnostics/reference/npde_panels.R")
for (f in MULTI) {
  c3 <- constructions_of(f)
  ok(c3$dependency == 1L, sprintf("G: %s has exactly one dependency environment", basename(f)))
}
for (f in setdiff(ref_files, c(EXEMPT, MULTI)))
  if (constructions_of(f)$dependency != 0L) ok(FALSE, sprintf("G: %s has an unexpected dependency environment", f))
# The 23 standard blocks are byte-identical, and so are their handover comments.
# 2026-11-14: baseline-fit adopts the standard block.
# AC12 (2026-10-26, plan Sections 8 and 9.4): bound_comparison.R added the 22nd.
std <- setdiff(ref_files, c(EXEMPT, NONSTANDARD))
block_of <- function(path) {
  ln <- readLines(path, warn = FALSE)
  s <- grep("^\\.mm_trusted_private <- base::local\\(\\{$", ln)
  if (length(s) != 1L) return(NA_character_)
  e <- s + which(ln[(s + 1L):length(ln)] == "}, envir = base::new.env(parent = base::baseenv()))")[1]
  paste(ln[s:e], collapse = "\n")
}
comment_of <- function(path) {
  ln <- readLines(path, warn = FALSE)
  s <- grep("^\\.mm_trusted_private <- base::local\\(\\{$", ln)
  if (length(s) != 1L) return(NA_character_)
  h <- grep("^# THE HANDOVER, stated where the construction is written", ln)
  h <- h[h < s]
  if (length(h) != 1L) return(NA_character_)
  paste(ln[h:(s - 1L)], collapse = "\n")
}
blocks <- vapply(std, block_of, character(1))
# 2026-11-14: baseline-fit adopts the standard block.
ok(length(std) == 23L && !anyNA(blocks) && length(unique(blocks)) == 1L,
   sprintf("G: the %d standard blocks are byte-identical (%d distinct)", length(std), length(unique(blocks))))
comments <- vapply(std, comment_of, character(1))
ok(!anyNA(comments) && length(unique(comments)) == 1L &&
     grepl("base namespace", comments[1]) && grepl("`::` operator", comments[1], fixed = TRUE) &&
     grepl("envir = base::new.env(parent = base::baseenv())", comments[1], fixed = TRUE) &&
     grepl("# of the method guard: its environment protects against ordinary\n# rebinding, not against a hostile caller in the same process",
           comments[1], fixed = TRUE) &&
     !grepl("write guard, the .harness-unlock window and the SME's review", comments[1]),
   # 2026-11-14: baseline-fit adopts the standard block.
   "G: the 23 handover comments are identical and state the base-namespace closure")
ok(!any(grepl("[^:]sys.source\\(|^sys.source\\(", unlist(lapply(setdiff(ref_files, EXEMPT), function(f) {
  ln <- readLines(f, warn = FALSE); ln[!grepl("^\\s*#", ln)] })))),
   "G: no non-exempt reference file calls an unqualified sys.source() outside a comment")
setwd(root)

cat("\n--- H: R11-P0-1 / P0-2 at run time ------------------------------------\n")

# A copy of the repository whose tools/method_manifest.R defines the validator
# as an unconditional stop, whose tools/record_selection.R and scm_config.R
# define a permissive validator and a fake digest helper (the P0-2 attack),
# sourced into an environment where every construction primitive is pre-bound
# to a fake (the P0-1 attack). A gate that reaches the REAL validator raises
# the stop; one that resolves any fake returns quietly.
rt <- tempfile("b12rt"); dir.create(rt)
dir.create(file.path(rt, "tools"))
mm <- readLines(file.path(REPO, "tools", "method_manifest.R"), warn = FALSE)
writeLines(c(mm, 'require_validated_method <- function(...) base::stop("REAL VALIDATOR RAN", call. = FALSE)'),
           file.path(rt, "tools", "method_manifest.R"))
for (f in c("record_selection.R", "engine_config.R", "seed.R", "psn_invocation.R"))
  if (file.exists(file.path(REPO, "tools", f)))
    file.copy(file.path(REPO, "tools", f), file.path(rt, "tools", f))
FAKE_DEP <- c('require_validated_method <- function(...) TRUE',
              '.mm_file_sha256 <- function(path) "sha256:fake"')
cat(FAKE_DEP, file = file.path(rt, "tools", "record_selection.R"), sep = "\n", append = TRUE)
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
# psn_covsearch.R's dependency scm_config.R carries the same fake pair.
cat(FAKE_DEP, file = file.path(rt, "skills/covariate-priors/reference/scm_config.R"),
    sep = "\n", append = TRUE)
setwd(rt)
fake_hits <- new.env(parent = emptyenv())
bind_fakes <- function(env) {
  fake_hits$n <- 0L
  assign("require_validated_method", function(...) TRUE, envir = env)
  assign("sys.source", function(file, envir, ...) {
    fake_hits$n <- fake_hits$n + 1L
    assign("require_validated_method", function(...) TRUE, envir = envir)
    invisible(NULL) }, envir = env)
  # The fake new.env returns a frame carrying an ACTIVE BINDING for `e`: the
  # first reads (the load and the seal) see one environment, later reads see
  # another that holds a permissive validator. Against base::local()'s default
  # envir this installed the fake (probe scratchpad/b12/probe_colon.R).
  assign("new.env", function(...) {
    fake_hits$n <- fake_hits$n + 1L
    f <- base::new.env(parent = globalenv())
    reads <- 0L; real <- NULL
    fake_env <- base::new.env(parent = base::baseenv())
    base::assign("require_validated_method", function(...) TRUE, envir = fake_env)
    base::makeActiveBinding("e", function(v) {
      if (!missing(v)) { real <<- v; return(invisible(NULL)) }
      reads <<- reads + 1L
      if (reads <= 3L) real else fake_env
    }, f)
    f }, envir = env)
  assign("baseenv", function() { fake_hits$n <- fake_hits$n + 1L; globalenv() }, envir = env)
  assign("lockEnvironment", function(...) { fake_hits$n <- fake_hits$n + 1L; invisible(NULL) }, envir = env)
  assign("file.exists", function(...) { fake_hits$n <- fake_hits$n + 1L; TRUE }, envir = env)
  # A spy `local`: behaves as base::local so the gate closures (which use the
  # bare spelling legitimately) still build, and counts how often it ran.
  assign("local", function(expr, envir = base::new.env()) {
    fake_hits$n <- fake_hits$n + 1L
    base::eval.parent(base::substitute(base::eval(quote(expr), envir))) }, envir = env)
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
  ok(grepl("REAL VALIDATOR RAN", msg),
     sprintf("H: %s reaches the real validator with fake construction primitives pre-bound", basename(path)))
  if (!grepl("REAL VALIDATOR RAN", msg)) cat(sprintf("      got: %s\n", msg))
}
# The two multi-load files: the fake pair in the dependency did not reach the gate
# (asserted above through REAL VALIDATOR RAN), and each closure sees its own `%||%`.
env <- new.env(parent = globalenv())
base::sys.source("skills/covariate-priors/reference/psn_covsearch.R", envir = env, keep.source = FALSE)
v <- get(".mm_trusted_private", envir = env); dd <- get(".mm_trusted_deps", envir = env)
ok(grepl("REAL VALIDATOR RAN", paste(deparse(get("require_validated_method", envir = v)), collapse = "")) &&
     identical(get("require_validated_method", envir = dd)(), TRUE),
   "H: psn_covsearch.R's validator environment holds the real validator while the dependency environment holds the fake")
ok(grepl("length(a) == 0", paste(deparse(get("%||%", envir = v)), collapse = ""), fixed = TRUE),
   "H: psn_covsearch.R's validator environment sees tools/method_manifest.R's `%||%`")
ok(!grepl("length(a) == 0", paste(deparse(get("%||%", envir = dd)), collapse = ""), fixed = TRUE),
   "H: psn_covsearch.R's dependency environment sees psn_uncertainty.R's `%||%`")
ok(identical(environment(get("%||%", envir = env))$sealed, dd),
   "H: psn_covsearch.R's `%||%` accessor forwards to the dependency environment")
ok(!identical(get(".mm_file_sha256", envir = v)("tools/method_manifest.R"), "sha256:fake"),
   "H: psn_covsearch.R's validator environment keeps the real .mm_file_sha256")
env <- new.env(parent = globalenv())
base::sys.source("skills/eda-standards/reference/eda.R", envir = env, keep.source = FALSE)
v <- get(".eda_private", envir = env); dd <- get(".eda_deps", envir = env)
ok(grepl("REAL VALIDATOR RAN", paste(deparse(get("require_validated_method", envir = v)), collapse = "")) &&
     identical(get("require_validated_method", envir = dd)(), TRUE),
   "H: eda.R's validator environment holds the real validator while the dependency environment holds the fake")
ok(!identical(get(".mm_file_sha256", envir = v)("tools/method_manifest.R"), "sha256:fake"),
   "H: eda.R's validator environment keeps the real .mm_file_sha256")
ok(grepl("length(a) == 0", paste(deparse(get("%||%", envir = v)), collapse = ""), fixed = TRUE) &&
     !exists("%||%", envir = dd, inherits = FALSE),
   "H: eda.R's validator environment sees tools/method_manifest.R's `%||%`; the dependency environment defines none")
ok(exists("dependency_sha256", envir = dd, inherits = FALSE) &&
     !exists("dependency_sha256", envir = v, inherits = FALSE),
   "H: eda.R's pre-seal dependency digests live in the dependency environment that consumes them")
setwd(root)

cat(sprintf("\nintegrity batch 12 battery: %d passed, %d failed\n", .pass, .fail))
if (.fail > 0) quit(status = 1)
invisible(TRUE)
}

if (.invoked_as_script()) .run_batch12_tests()
