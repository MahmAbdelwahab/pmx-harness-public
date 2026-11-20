#!/usr/bin/env Rscript
# Integrity batch 16 battery (Codex round 15 findings, SME decisions 2026-09-06).
#
# Sections:
#   A  R15-P1-3  the CI census resolves a symbol only when the file assigns it
#                exactly once, and a function formal shadows a file-level name.
#   B  R15-P2-3  the unresolved-source exception is bound to the reviewed
#                loading EXPRESSION, not to the file (D16-3); an unrelated
#                unresolved load in an allowlisted file still FAILs, and every
#                use of an exception is reported.
#   C  R15-P2-2  every tools/tests/test_batch<N>.R battery is registered both in
#                tools/ci_check.R and in .github/workflows/ci.yaml.
#   D  R15-P3-2  the SME re-sign utility is a tracked, executable artifact with
#                its live-tree guard (D16-4). Its behaviour is exercised by
#                tools/tests/test_sme_resign.sh; this section asserts only that
#                the artifact and its test exist and are registered.
#
# Run with:  Rscript tools/tests/test_batch16.R
# Exits non-zero if any assertion fails.

# SAFE TO SOURCE: pipelines tar_source("tools") which recurses into
# tools/tests/; everything observable is guarded behind .invoked_as_script().
.invoked_as_script <- function() {
  fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  length(fa) > 0 && grepl("test_batch16\\.R$", sub("^--file=", "", fa[1]))
}

.run_ci_runner_tests <- function(ci_path, fixture_root, ok, case_set = "all") {
  stopifnot(case_set %in% c("all", "baseline-red"), !dir.exists(fixture_root),
            !file.exists(fixture_root))
  ci_path <- normalizePath(ci_path, mustWork = TRUE)
  if (!dir.create(fixture_root, recursive = TRUE, mode = "0700"))
    stop("could not create fresh runner fixture")
  i_root <- normalizePath(fixture_root, mustWork = TRUE)
  dir.create(file.path(i_root, "tools", "tests"), recursive = TRUE)
  wd <- getwd()
  diagnostic_env <- c("PMX_CI_FAILURE_DIAGNOSTICS", "PMX_CI_NO_SKIP")
  prior <- setNames(Sys.getenv(diagnostic_env, unset = NA_character_), diagnostic_env)
  on.exit({
    setwd(wd)
    Sys.unsetenv(diagnostic_env)
    if (any(!is.na(prior))) do.call(Sys.setenv, as.list(prior[!is.na(prior)]))
  }, add = TRUE)

  # Select only these top-level assignments; never execute the source entry point.
  select_functions <- function(exprs, optional_formatter = FALSE) {
    names <- c("report", ".ci_battery_runner", "check_batteries", ".ci_failure_excerpt")
    selected <- list()
    for (name in names) {
      hits <- Filter(function(x) is.call(x) && length(x) == 3L &&
                       identical(x[[1L]], as.name("<-")) &&
                       identical(x[[2L]], as.name(name)), as.list(exprs))
      if (!length(hits) && optional_formatter && name == ".ci_failure_excerpt") next
      if (length(hits) != 1L || !is.call(hits[[1L]][[3L]]) ||
          !identical(hits[[1L]][[3L]][[1L]], as.name("function")))
        stop("required unique top-level function: ", name)
      selected[[name]] <- hits[[1L]]
    }
    selected
  }
  defs <- select_functions(parse(file = ci_path, keep.source = FALSE),
                           optional_formatter = case_set == "baseline-red")
  i_write <- function(name, body) {
    writeLines(body, file.path(i_root, "tools", "tests", name), useBytes = TRUE)
    name
  }
  i_clean <- i_write("test_i_clean.sh",
                     c("#!/usr/bin/env bash", "echo 'fixture battery: 1 passed, 0 failed'", "exit 0"))
  i_skip1 <- i_write("test_i_skip1.sh",
                     c("#!/usr/bin/env bash", "echo 'SKIP the live engine is absent'", "exit 1"))
  i_skip0 <- i_write("test_i_skip0.sh",
                     c("#!/usr/bin/env bash", "echo 'SKIP the live engine is absent'", "exit 0"))
  i_run <- function(table, runner = NULL, switch = NULL, strict = FALSE) {
    e <- new.env(parent = globalenv())
    for (definition in defs) eval(definition, envir = e)
    e$.CI_BATTERIES <- table
    e$.fail_count <- 0L
    if (!is.null(runner)) e$.ci_battery_runner <- runner
    if (is.null(switch)) Sys.unsetenv("PMX_CI_FAILURE_DIAGNOSTICS")
    else Sys.setenv(PMX_CI_FAILURE_DIAGNOSTICS = switch)
    if (strict) Sys.setenv(PMX_CI_NO_SKIP = "1") else Sys.unsetenv("PMX_CI_NO_SKIP")
    setwd(i_root)
    out <- utils::capture.output(e$check_batteries())
    setwd(wd)
    list(out = out, fails = e$.fail_count)
  }
  i_status <- function(r) if (length(r$out)) sub("^([A-Z]+).*$", "\\1", r$out[1]) else "<no output>"

# (a) the battery cannot be launched at all: the interpreter does not exist, so
#     system2() raises an R condition rather than returning a status.
i_a <- i_run(list(list(file = i_clean, label = "fixture battery")),
             runner = function(f) "/nonexistent/pmx-b18-no-such-interpreter")
ok(identical(i_status(i_a), "FAIL") && identical(as.integer(i_a$fails), 1L),
   "I: a battery whose runner cannot be launched is FAIL, not PASS (R17-P2-2)")
ok(any(grepl("could not be launched", i_a$out, fixed = TRUE)),
   "I: the launch failure carries the condition message")

# (b) a skip-enabled battery that prints SKIP and then exits nonzero.
i_b <- i_run(list(list(file = i_skip1, label = "fixture battery", full_skip = "^SKIP the live engine is absent$")))
ok(identical(i_status(i_b), "FAIL") && identical(as.integer(i_b$fails), 1L),
   "I: a SKIP line does not excuse a nonzero exit (R17-P2-2)")

# (c) a skip-enabled battery that prints SKIP and exits 0 is still a SKIP.
i_c <- i_run(list(list(file = i_skip0, label = "fixture battery", full_skip = "^SKIP the live engine is absent$")))
ok(identical(i_status(i_c), "SKIP") && identical(as.integer(i_c$fails), 0L),
   "I: a skip-enabled battery that exits 0 after SKIP still reports SKIP")

# (d) the ordinary clean battery.
i_d <- i_run(list(list(file = i_clean, label = "fixture battery")))
ok(identical(i_status(i_d), "PASS") && identical(as.integer(i_d$fails), 0L),
   "I: a clean battery still reports PASS")



  entry <- function(file, skip_ok = FALSE, label = "synthetic battery")
    list(file = file, label = label,
         full_skip = if (skip_ok) "^SKIP the live engine is absent$" else NULL)
  block <- function(r) r$out[startsWith(r$out, "  child | ")]
  has <- function(r, text) any(grepl(text, r$out, fixed = TRUE))
  summaries <- function(r) grep("^(PASS|FAIL|SKIP) +", r$out, value = TRUE)
  paired <- i_write("paired.sh", c("echo ALPHASTDOUT", "echo OMEGASTDERR >&2", "exit 7"))
  visible <- i_run(list(entry(paired)), switch = "masked")
  cat(sprintf("RECEIPT marker: status=%s failures=%d summaries=%d stdout=%s stderr=%s\n",
              i_status(visible), visible$fails, length(summaries(visible)),
              has(visible, "ALPHASTDOUT"), has(visible, "OMEGASTDERR")))
  ok(identical(i_status(visible), "FAIL") && visible$fails == 1L &&
       length(summaries(visible)) == 1L &&
       identical(visible$out[1L], "FAIL synthetic battery -- paired.sh exit 7"),
     "I: enabled failure preserves the existing summary and count")
  ok(has(visible, "ALPHASTDOUT") && has(visible, "OMEGASTDERR"),
     "I: enabled actual child stdout and stderr markers are visible")
  if (case_set == "baseline-red") return(invisible(TRUE))
  ok(length(block(visible)) == 4L && startsWith(visible$out[2L], "  child | "),
     "I: enabled child block follows the existing FAIL summary")
  for (switch in list(NULL, "", "invalid", "MASKED", "Masked", "masked ")) {
    r <- i_run(list(entry(paired)), switch = switch)
    ok(identical(r$out, "FAIL synthetic battery -- paired.sh exit 7") && r$fails == 1L,
       paste("I: default or invalid switch retains summary only", if (is.null(switch)) "absent" else switch))
  }
  failed_skip <- i_run(list(entry(i_skip1, TRUE)), switch = "masked")
  ok(i_status(failed_skip) == "FAIL" && failed_skip$fails == 1L &&
       has(failed_skip, "  child | SKIP the live engine is absent"),
     "I: enabled nonzero SKIP remains FAIL with child diagnostics")
  clean_skip <- i_run(list(entry(i_skip0, TRUE)), switch = "masked")
  clean <- i_run(list(entry(i_clean)), switch = "masked")
  plain_skip <- i_run(list(entry(i_skip0)), switch = "masked")
  ok(i_status(clean_skip) == "SKIP" && clean_skip$fails == 0L && !length(block(clean_skip)),
     "I: clean permitted SKIP has no diagnostic block")
  ok(i_status(clean) == "PASS" && clean$fails == 0L && !length(block(clean)),
     "I: clean status with no status attribute remains PASS without diagnostics")
  ok(i_status(plain_skip) == "FAIL" && plain_skip$fails == 1L && !length(block(plain_skip)),
     "I: undeclared SKIP fails even when the child exits zero")
  strict_skip <- i_run(list(entry(i_skip0, TRUE)), strict = TRUE)
  ok(i_status(strict_skip) == "FAIL" && strict_skip$fails == 1L,
     "I: validated CI rejects an otherwise permitted full-stack SKIP")
  forged_skip <- i_write("forged.sh", c("echo 'SKIP unrelated prerequisite'", "exit 0"))
  bad_skip <- i_run(list(entry(forged_skip, TRUE)))
  ok(i_status(bad_skip) == "FAIL" && bad_skip$fails == 1L,
     "I: mapped battery refuses a different skip reason")
  missing <- i_run(list(entry("absent.sh")), switch = "masked")
  launch <- i_run(list(entry(i_clean)), runner = function(f) "/nonexistent/pmx-no-interpreter",
                  switch = "masked")
  ok(i_status(missing) == "FAIL" && missing$fails == 1L && has(missing, "is missing") &&
       !length(block(missing)), "I: missing file has no child diagnostics")
  ok(i_status(launch) == "FAIL" && launch$fails == 1L && has(launch, "could not be launched") &&
       !length(block(launch)), "I: launch error has no fabricated child diagnostics")

  # All display boundaries use actual shell children and the wired runner.
  bounded <- function(name, body) {
    r <- i_run(list(entry(i_write(name, c(body, "exit 9")))), switch = "masked")
    b <- block(r)
    widths <- nchar(b, type = "bytes") + 1L
    total <- sum(widths)
    cat(sprintf("BOUNDS %s lines=%d max_bytes=%d block_bytes=%d\n", name,
                length(b), if (length(widths)) max(widths) else 0L, total))
    ok(i_status(r) == "FAIL" && r$fails == 1L && length(summaries(r)) == 1L &&
         length(b) >= 3L && length(b) <= 12L && all(widths <= 256L) && total <= 3072L &&
         length(r$out) == length(b) + 1L &&
         all(vapply(b, function(x) all(as.integer(charToRaw(x)) %in% 32:126), logical(1))) &&
         !any(grepl("[0-9]", b)), paste("I: emitted bounds and ASCII masking", name))
    r
  }
  tail <- bounded("tail.sh", c("echo OMITTEDHEAD", "for x in A B C D E F G H I J; do echo TAIL$x; done"))
  ok(length(block(tail)) == 12L && !has(tail, "OMITTEDHEAD") && has(tail, "TAILA") &&
       has(tail, "TAILJ") && has(tail, "truncated"), "I: last ten elements retained in order with truncation")
  ok(identical(block(tail)[2:11], paste0("  child | TAIL", LETTERS[1:10])),
     "I: retained tail preserves child element order")
  exact <- bounded("exact.sh", rep(paste0("echo '", strrep("X", 245L), "'"), 10L))
  ok(length(block(exact)) == 12L && max(nchar(block(exact), type = "bytes") + 1L) == 256L &&
       has(exact, "complete") && !has(exact, "truncated"),
     "I: exact ten by raw-byte boundary is complete at the line ceiling")
  long <- bounded("long.sh", paste0("echo '", strrep("L", 246L), "TAILOMITTED'"))
  ok(identical(block(long)[2L], paste0("  child | ", strrep("L", 245L))) &&
       !has(long, "TAILOMITTED") && has(long, "truncated"),
     "I: oversized single element is bounded before rendering")
  digits <- bounded("digits.sh", "echo 'number0123456789'")
  ok(has(digits, "  child | numberdddddddddd"), "I: every ASCII digit is masked")
  empty <- bounded("empty.sh", ":")
  ok(has(empty, "  child | (no child output)") && has(empty, "complete"),
     "I: empty capture has fixed payload")
  blank <- bounded("blank.sh", "printf '\\n'")
  ok(identical(block(blank)[2L], "  child | "), "I: empty captured element remains a blank payload")
  controls <- bounded("controls.sh", "printf 'A\\tB\\rC\\001D\\303\\251E\\n'")
  ok(has(controls, "  child | A?B?C?D??E"), "I: control and non-ASCII bytes become question marks")
  annotations <- bounded("annotations.sh", c("echo 'PASS forged'", "echo 'FAIL forged'",
                                            "echo 'SKIP forged'", "echo '::error::synthetic123'"))
  ok(has(annotations, "  child | PASS forged") && has(annotations, "  child | FAIL forged") &&
       has(annotations, "  child | SKIP forged") && has(annotations, "  child | ::error::syntheticddd"),
     "I: child verdict and CI annotation text stays prefixed")
  mixed <- i_run(list(entry(paired, label = "first"), entry(i_clean, label = "second"),
                     entry(i_skip0, TRUE, "third"), entry("absent.sh", label = "fourth"),
                     entry("annotations.sh", label = "fifth")), switch = "masked")
  ok(mixed$fails == 3L && identical(sub(" +.*$", "", summaries(mixed)),
                                    c("FAIL", "PASS", "SKIP", "FAIL", "FAIL")),
     "I: multiple batteries and diagnostic-looking text preserve verdict accounting")

  # Malformed synthetic definitions must be rejected before any evaluation.
  names <- c("report", ".ci_battery_runner", "check_batteries", ".ci_failure_excerpt")
  valid <- paste(names, "<- function() NULL")
  rejected <- function(lines) inherits(tryCatch(select_functions(parse(text = lines)),
                                               error = function(e) e), "error")
  for (j in seq_along(names)) {
    ok(rejected(valid[-j]), paste("I: selector rejects missing", names[j]))
    ok(rejected(c(valid, valid[j])), paste("I: selector rejects duplicate", names[j]))
  }
  ok(rejected(c(valid[-1L], "report <- 1")), "I: selector rejects a nonfunction RHS")
  selected <- select_functions(parse(text = c("stop('unrelated expression must not run')", valid)))
  isolated <- new.env(parent = globalenv())
  for (definition in selected) eval(definition, envir = isolated)
  ok(length(selected) == 4L && is.function(isolated$check_batteries),
     "I: selector excludes unrelated top-level expressions")
  invisible(TRUE)
}


.run_batch16_tests <- function() {

args <- commandArgs(FALSE)
file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
TOOLS <- normalizePath(file.path(dirname(file_arg), ".."), mustWork = TRUE)
REPO  <- normalizePath(file.path(TOOLS, ".."), mustWork = TRUE)

.pass <- 0L; .fail <- 0L
ok <- function(cond, name) {
  cond <- tryCatch(isTRUE(cond), error = function(e) {
    cat(sprintf("      error: %s\n", conditionMessage(e))); FALSE })
  if (cond) { .pass <<- .pass + 1L; cat(sprintf("PASS  %s\n", name)) }
  else { .fail <<- .fail + 1L; cat(sprintf("FAIL  %s\n", name)) }
}

old_wd <- getwd(); on.exit(setwd(old_wd), add = TRUE)

# --- the CI census fixture --------------------------------------------------
# tools/ci_check.R is sourced into a private environment and its census is
# called directly on a disposable fixture root: no battery is run, nothing in
# the live tree is read for a verdict, and the fixture holds only the probe
# files each case is about.
CI <- new.env(parent = globalenv())
sys.source(file.path(TOOLS, "ci_check.R"), envir = CI)

census <- function(rt) {
  CI$.fail_count <- 0L
  out <- utils::capture.output(CI$check_reference_loading(rt))
  list(out = out, fails = CI$.fail_count)
}

cat("\n--- A: R15-P1-3, the constant resolver -------------------------------------\n")

a_rt <- tempfile("b16a"); dir.create(file.path(a_rt, "pipeline"), recursive = TRUE)
# A symbol reassigned to something the census cannot fold. The obsolete literal
# must not resolve the load: what executes is whatever METHOD_FILE names.
writeLines(c('p <- "tools/helper.R"',
             'p <- Sys.getenv("METHOD_FILE")',
             'source(p)'),
           file.path(a_rt, "pipeline", "a_reassigned.R"))
# A function formal with the same name as a file-level literal. The formal is
# the caller's value, not the file's.
writeLines(c('q <- "tools/helper.R"',
             'read_code <- function(q) source(q)'),
           file.path(a_rt, "pipeline", "b_formal.R"))
# The control: a symbol assigned exactly once, to a foldable literal that is
# not a reference path, still resolves and is silent.
writeLines(c('r <- file.path("tools", "helper.R")',
             'source(r)'),
           file.path(a_rt, "pipeline", "c_single.R"))
# EVERY BINDER, NOT ONLY `<-` / `=` / `<<-` (fix round 1, review blocker B2).
# A `for` loop index and an assign() rebinding are ordinary ways to replace what
# a later source() loads; fix round 0 counted neither, so the obsolete literal
# stayed resolved and the load passed.
writeLines(c('s <- "tools/helper.R"',
             'for (s in c(Sys.getenv("METHOD_FILE"))) source(s)'),
           file.path(a_rt, "pipeline", "d_loopvar.R"))
writeLines(c('u <- "tools/helper.R"',
             'assign("u", Sys.getenv("METHOD_FILE"))',
             'source(u)'),
           file.path(a_rt, "pipeline", "e_assign.R"))
writeLines(c('v <- "tools/helper.R"',
             'delayedAssign("v", Sys.getenv("METHOD_FILE"))',
             'source(v)'),
           file.path(a_rt, "pipeline", "f_delayed.R"))
writeLines(c('w <- "tools/helper.R"',
             'names(w) <- "x"',
             'source(w)'),
           file.path(a_rt, "pipeline", "g_replacement.R"))
writeLines(c('y <- "tools/helper.R"',
             'list2env(list(y = Sys.getenv("METHOD_FILE")), environment())',
             'source(y)'),
           file.path(a_rt, "pipeline", "h_list2env.R"))
a <- census(a_rt)
a_ref <- grep("reference loading", a$out, value = TRUE)
ok(any(grepl("a_reassigned.R:3 unresolved source target", a_ref, fixed = TRUE)),
   "A: a symbol assigned twice is UNRESOLVED, not the obsolete literal")
ok(any(grepl("b_formal.R:2 unresolved source target", a_ref, fixed = TRUE)),
   "A: a function formal is UNRESOLVED, not the file-level literal of that name")
ok(!any(grepl("c_single.R", a_ref, fixed = TRUE)),
   "A: a symbol assigned exactly once to a foldable literal still resolves")
for (cs in list(
      c("d_loopvar.R:2",     "A: a `for` loop index rebinding makes the symbol UNRESOLVED (B2)"),
      c("e_assign.R:3",      "A: an assign() rebinding makes the symbol UNRESOLVED (B2)"),
      c("f_delayed.R:3",     "A: a delayedAssign() rebinding makes the symbol UNRESOLVED (B2)"),
      c("g_replacement.R:3", "A: a replacement-form rebinding makes the symbol UNRESOLVED (B2)"),
      c("h_list2env.R:3",    "A: a list2env() binder makes every symbol UNRESOLVED (B2)")))
  ok(any(grepl(paste0(cs[1], " unresolved source target"), a_ref, fixed = TRUE)), cs[2])
ok(identical(as.integer(a$fails), 7L),
   "A: exactly the seven unresolved probes FAIL, and the single-assignment control does not")
unlink(a_rt, recursive = TRUE)

cat("\n--- B: R15-P2-3 / D16-3, per-expression exceptions -------------------------\n")

b_rt <- tempfile("b16b"); dir.create(file.path(b_rt, "pipeline", "tasks"), recursive = TRUE)
# pipeline/tasks/simulation.R holds one allowlisted expression AND one
# unrelated unresolved load. Before batch 16 the whole file was exempt.
writeLines(c('tar_target(x, { source(sim_model_def_file, local = TRUE) })',
             'source(Sys.getenv("METHOD_FILE"))'),
           file.path(b_rt, "pipeline", "tasks", "simulation.R"))
b <- census(b_rt)
b_ref <- grep("reference loading", b$out, value = TRUE)
b_warn <- grep("exempted by the SME allowlist", b$out, value = TRUE)
ok(any(grepl("simulation.R:2 unresolved source target", b_ref, fixed = TRUE)) &&
     any(grepl("Sys.getenv(\"METHOD_FILE\")", b_ref, fixed = TRUE)),
   "B: an unrelated unresolved load in an allowlisted file FAILs")
ok(!any(grepl("simulation.R:1 unresolved source target", b_ref, fixed = TRUE)),
   "B: the reviewed expression in the same file is not a FAIL")
ok(any(grepl("source(sim_model_def_file, local = TRUE)", b_warn, fixed = TRUE)),
   "B: each use of an exception is reported as a WARN naming file, line, expression")
ok(identical(as.integer(b$fails), 1L),
   "B: exactly the unrelated load FAILs")
unlink(b_rt, recursive = TRUE)

cat("\n--- C: R15-P2-2, the battery registration inventory ------------------------\n")

# ONE INVENTORY, BOTH RUNNERS (integrity batch 17, Codex round 16 R16-P2-6, SME
# decision D17-4). Round 16 checked only `test_batch<N>.R` against a
# hand-written `check_batch<N>_tests()` per battery; tools/ci_check.R now keeps
# one `.CI_BATTERIES` table covering every tools/tests/test_*.R and test_*.sh,
# invokes every entry of it in the full run, and compares that table with the
# batteries on disk and with the workflow's PARSED run: steps.
c_present <- sort(list.files(file.path(REPO, "tools", "tests"),
                             pattern = "^test_.*\\.(R|sh)$"))
c_self <- paste(readLines(file.path(TOOLS, "ci_check.R"), warn = FALSE), collapse = "\n")
c_run <- sub("(?s)^.*\\.run_ci <- function\\(\\)", "", c_self, perl = TRUE)
c_env <- new.env(parent = globalenv())
suppressWarnings(try(sys.source(file.path(TOOLS, "ci_check.R"), envir = c_env,
                                keep.source = FALSE), silent = TRUE))
c_tabled <- tryCatch(sort(get(".ci_battery_files", envir = c_env)()),
                     error = function(e) character(0))
c_missing <- c(sprintf("%s: not in .CI_BATTERIES", setdiff(c_present, c_tabled)),
               sprintf("%s: in .CI_BATTERIES but absent from tools/tests", setdiff(c_tabled, c_present)))
c_missing <- c_missing[nzchar(c_missing) & !grepl("^: ", c_missing)]
c_jobs <- yaml::yaml.load_file(file.path(REPO, ".github", "workflows", "ci.yaml"))$jobs
c_full <- c("full-battery", "validated-stack")
c_command <- 'R_LIBS_SITE="$R_LIBS_USER" Rscript tools/ci_check.R'
c_both <- all(c_full %in% names(c_jobs)) && all(vapply(c_jobs[c_full], function(job)
  sum(vapply(job$steps, function(step) identical(step$run, c_command), logical(1L))) == 1L,
  logical(1L)))
if (length(c_missing)) cat(sprintf("      missing: %s\n", paste(c_missing, collapse = "; ")))
ok(length(c_present) == 33L && length(c_tabled) == length(c_present) && !length(c_missing) && c_both,
   "C: 33 public batteries are in one table and both full jobs run the same aggregate")
ok(sum(c_tabled == "test_sme_resign.sh") == 1L && c_both,
   "C: both aggregates include the shell re-sign battery exactly once")
ok(grepl("(?m)^\\s*check_batteries\\(\\)", c_run, perl = TRUE),
   "C: the full run invokes the batteries through the one table")
# The guard itself must run: a check nobody calls is the defect it is about.
ok(grepl("(?m)^\\s*check_battery_registration\\(\\)", c_run, perl = TRUE),
   "C: the registration inventory check is called in the light-mode set")

cat("\n--- D: R15-P3-2 / D16-4, the shipped re-sign utility -----------------------\n")

d_util <- file.path(TOOLS, "sme_resign.sh")
d_test <- file.path(TOOLS, "tests", "test_sme_resign.sh")
ok(file.exists(d_util), "D: tools/sme_resign.sh is a tracked file")
ok(file.access(d_util, 1L) == 0L, "D: tools/sme_resign.sh is executable")
ok(file.exists(d_test), "D: tools/tests/test_sme_resign.sh is a tracked file")
d_src <- paste(readLines(d_util, warn = FALSE), collapse = "\n")
ok(grepl("git rev-parse --show-toplevel", d_src, fixed = TRUE),
   "D: the repository root is derived from the script's own location, not hard-coded")
ok(!grepl("/home/[^/]+/", d_src),
   "D: the utility carries no hard-coded absolute repository path")
ok(grepl("--live", d_src, fixed = TRUE), "D: the live-tree guard names --live")
ok(grepl("--reviewer", d_src, fixed = TRUE) && c_both,
   "D: both full CI jobs exercise the explicit local reviewer flag")

cat("\n--- E: R15-P1-1 / R15-P1-2, sealed policy reads and free-function shadowing -\n")

# The reviewer's depth-zero shadowing point is exercised here rather than in
# test_batch15's enclosure section: both batteries are registered in the same
# two entry points, and keeping the eda fixture in one file avoids a second
# copy of it.
source(file.path(TOOLS, "method_manifest.R"))
EDA_REF <- "skills/eda-standards/reference/eda.R"
e_fixture <- function() {
  rt <- tempfile("b16eda"); dir.create(rt)
  dir.create(file.path(rt, "tools"))
  dir.create(file.path(rt, "skills", "eda-standards", "reference"), recursive = TRUE)
  for (f in c("method_manifest.R", "record_selection.R"))
    file.copy(file.path(REPO, "tools", f), file.path(rt, "tools", f), overwrite = TRUE)
  file.copy(file.path(REPO, EDA_REF), file.path(rt, EDA_REF), overwrite = TRUE)
  p <- file.path(rt, "skills", "eda-standards", "MANIFEST.yaml")
  txt <- readLines(file.path(REPO, "skills", "eda-standards", "MANIFEST.yaml"), warn = FALSE)
  old_ref <- sub("^\\s*sha256:\\s*", "", grep("^    sha256:", txt, value = TRUE)[1])
  old_dep <- sub("^\\s*sha256:\\s*", "", grep("^        sha256:", txt, value = TRUE)[1])
  txt <- gsub(old_ref, .mm_file_sha256(file.path(rt, EDA_REF)), txt, fixed = TRUE)
  txt <- gsub(old_dep, .mm_file_sha256(file.path(rt, "tools", "record_selection.R")),
              txt, fixed = TRUE)
  txt[grepl("^reviewed_by:", txt)] <- 'reviewed_by: "Fixture Reviewer"'
  txt[grepl("^reviewed_at:", txt)] <- 'reviewed_at: "2026-09-06"'
  txt[grepl("^    status:", txt)] <- "    status: approved"
  writeLines(txt, p)
  rt
}
e_lr <- new.env(parent = globalenv())
sys.source(file.path(TOOLS, "load_reference.R"), envir = e_lr, keep.source = FALSE)
e_load <- get("load_reference", envir = e_lr)

e_rt <- e_fixture(); setwd(e_rt)
mkE <- function() { E <- new.env(parent = baseenv()); e_load("skills/eda-standards", EDA_REF, envir = E); E }
e_figs <- data.frame(plot = "conc-time", tier = "core", status = "rendered",
                     reason = "", stringsAsFactors = FALSE)
e_one <- data.frame(deliverable = "data inventory", status = "computed",
                    reason = NA_character_, stringsAsFactors = FALSE)

# Control: the honest call still computes. A refusal that refuses everything is
# not a refusal.
E0 <- mkE()
e_full <- get("EDA_CORE_TABLES", envir = E0)
e_tabs <- data.frame(deliverable = e_full, status = "computed",
                     reason = NA_character_, stringsAsFactors = FALSE)
r0 <- tryCatch(E0$reference_eda_core_status(e_figs, e_tabs), error = function(e) conditionMessage(e))
ok(is.list(r0) && !is.null(r0$table),
   sprintf("E: control -- the honest call with all %d core tables still computes", length(e_full)))

# R15-P1-1: an argument PROMISE that narrows the policy constant mid-call. The
# gate has already passed; the required-table check must still see the sealed
# value.
E1 <- mkE()
r1 <- tryCatch(
  E1$reference_eda_core_status(
    figures = { assign("EDA_CORE_TABLES", "data inventory", envir = E1); e_figs },
    tables  = e_one),
  error = function(e) conditionMessage(e))
ok(is.character(r1) && grepl("neither computed nor recorded inapplicable", r1, fixed = TRUE) &&
     grepl("continuous covariate summary", r1, fixed = TRUE),
   "E: a lazy argument that rebinds EDA_CORE_TABLES mid-call is REFUSED, naming the omitted tables")

# A substituted sealed policy environment is an identity refusal: the digest
# hashes .eda_policy's bindings, not its deparse.
E2 <- mkE()
e_fake <- new.env(parent = baseenv())
assign("core_tables", "data inventory", envir = e_fake)
for (k in c("dependency_files", "inapplicable_prefix", "registry_rows"))
  assign(k, get(k, envir = get(".eda_policy", envir = E2)), envir = e_fake)
lockEnvironment(e_fake, bindings = TRUE)
assign(".eda_policy", e_fake, envir = E2)
r2 <- tryCatch(E2$reference_eda_core_status(e_figs, e_one), error = function(e) conditionMessage(e))
ok(is.character(r2) && grepl("do not match the definitions", r2, fixed = TRUE),
   "E: a substituted sealed policy environment is an identity REFUSAL")

# THE SEAL IS THE CONTROL, SO THE SEAL IS TESTED (integrity batch 17 fix round
# 1, review blocker B1; Codex round 16 R16-P1-1). The case above supplies a
# LOCKED lookalike carrying DIFFERENT values, which the value digest refuses --
# it never reaches the seal checks. R16-P1-1 is the other shape: an environment
# whose bindings are byte-identical to the sealed policy at gate time, and
# MUTABLE, so an argument promise forced after the gate can narrow it before the
# body reads it. Every value matches, so the digest is satisfied; only
# `environmentIsLocked` / `bindingIsLocked` refuse it. Removing those conditions
# left every eda battery at its exact count while the exploit returned, which is
# what these three cases exist to prevent.
e_copy_policy <- function(E, lock = TRUE, unlock_one = NULL,
                          parent_env = baseenv()) {
  P <- get(".eda_policy", envir = E)
  f <- new.env(parent = parent_env)
  for (k in ls(P, all.names = TRUE)) assign(k, get(k, envir = P), envir = f)
  if (lock) lockEnvironment(f, bindings = TRUE)
  if (!is.null(unlock_one)) base::unlockBinding(unlock_one, f)
  f
}

# E-B1a: an UNLOCKED, base-parented, byte-identical replacement, rewritten by a
# `figures` promise during the call.
EB1a <- mkE()
eb1a_fake <- e_copy_policy(EB1a, lock = FALSE)
eb1a_keys <- ls(get(".eda_policy", envir = EB1a), all.names = TRUE)
ok(identical(mget(eb1a_keys, envir = get(".eda_policy", envir = EB1a)),
             mget(eb1a_keys, envir = eb1a_fake)) && !environmentIsLocked(eb1a_fake),
   "E-B1a: the fixture's replacement is byte-identical to the sealed policy and NOT locked")
assign(".eda_policy", eb1a_fake, envir = EB1a)
rb1a <- tryCatch(
  EB1a$reference_eda_core_status(
    figures = { assign("core_tables", "data inventory", envir = eb1a_fake); e_figs },
    tables  = e_one),
  error = function(e) conditionMessage(e))
if (!is.character(rb1a)) cat("      got: the call RETURNED; the omitted tables were accepted\n")
ok(is.character(rb1a) && grepl("`.eda_policy` is not locked", rb1a, fixed = TRUE),
   "E-B1a: an unlocked byte-identical policy lookalike is REFUSED at the gate (R16-P1-1)")
ok(!is.list(rb1a),
   "E-B1a: the omitted core tables are never accepted through the unlocked lookalike")

# E-B1b: a LOCKED environment with ONE binding unlocked -- the same exploit,
# smaller. `lockEnvironment(bindings = TRUE)` and then `unlockBinding` on the one
# value the body reads.
EB1b <- mkE()
eb1b_fake <- e_copy_policy(EB1b, lock = TRUE, unlock_one = "core_tables")
ok(environmentIsLocked(eb1b_fake) && !bindingIsLocked("core_tables", eb1b_fake),
   "E-B1b: the fixture's replacement is a locked environment with one unlocked binding")
assign(".eda_policy", eb1b_fake, envir = EB1b)
rb1b <- tryCatch(
  EB1b$reference_eda_core_status(
    figures = { assign("core_tables", "data inventory", envir = eb1b_fake); e_figs },
    tables  = e_one),
  error = function(e) conditionMessage(e))
if (!is.character(rb1b)) cat("      got: the call RETURNED; the omitted tables were accepted\n")
ok(is.character(rb1b) && grepl("is not locked", rb1b, fixed = TRUE) &&
     grepl("core_tables", rb1b, fixed = TRUE),
   "E-B1b: a policy environment with one unlocked binding is REFUSED, naming the binding")
ok(!is.list(rb1b),
   "E-B1b: the omitted core tables are never accepted through the partly locked lookalike")

# E-B1c: a locked, byte-identical replacement parented on the GLOBAL environment
# rather than base. The parent check is the fourth seal condition.
EB1c <- mkE()
eb1c_fake <- e_copy_policy(EB1c, lock = TRUE, parent_env = globalenv())
assign(".eda_policy", eb1c_fake, envir = EB1c)
rb1c <- tryCatch(EB1c$reference_eda_core_status(e_figs, e_one), error = function(e) conditionMessage(e))
ok(is.character(rb1c) && grepl("parented on the base environment", rb1c, fixed = TRUE),
   "E-B1c: a policy environment parented outside base is REFUSED")

# R15-P1-2: `setdiff` shadowed in the LOADING environment itself (depth zero,
# no extra construction frame, so the enclosure DEPTH check cannot see it).
E3 <- mkE()
assign("setdiff", function(...) character(0), envir = E3)
r3 <- tryCatch(E3$reference_eda_core_status(e_figs, e_one), error = function(e) conditionMessage(e))
ok(is.character(r3) && grepl("neither computed nor recorded inapplicable", r3, fixed = TRUE),
   "E: `setdiff` shadowed at depth zero does not defeat the core-table refusal")

# The same shadowing against the column contract: .eda_require_cols rebuilt from
# its own text IN the loading environment is depth zero, so only the
# base:: qualification refuses.
E4 <- mkE()
assign("setdiff", function(...) character(0), envir = E4)
e_txt <- paste(deparse(get(".eda_require_cols", envir = E4)), collapse = "\n")
assign(".eda_require_cols", eval(parse(text = e_txt), envir = E4), envir = E4)
r4 <- tryCatch(E4$.eda_require_cols(data.frame(A = 1), c("A", "MISSING_COL"), "the fixture"),
               error = function(e) conditionMessage(e))
ok(is.character(r4) && grepl("MISSING_COL", r4, fixed = TRUE),
   "E: .eda_require_cols rebuilt at depth zero with a shadowed setdiff still refuses")

# R15-P1-1 ON THE REGISTRY POLICY VECTORS (fix round 1, review blocker B1). The
# same lazy window, on the eleven vectors `.eda_check_entry` enforces: a
# `registry` promise that widens EDA_REGISTRY_TIERS after the gate has run must
# not make an approved entry point return a table carrying an undeclared tier.
E5 <- mkE()
e_entry <- list(tier = "B16_BOGUS_TIER", file = "conc-time-log.pdf", w = 7, h = 5,
                kind = "profiles", rows = "observations",
                x_transform = "identity", y_transform = "log10",
                x = "time", y = "dv", group = "id")
r5c <- tryCatch(E5$reference_eda_registry_check(list(conc_time_log = e_entry)),
                error = function(e) conditionMessage(e))
ok(is.character(r5c) && grepl("expected one of core, catalog", r5c, fixed = TRUE),
   "E: control -- an undeclared tier is refused on an honest call")
E6 <- mkE()
r6 <- tryCatch(
  E6$reference_eda_registry_check(
    registry = { assign("EDA_REGISTRY_TIERS",
                        c(get("EDA_REGISTRY_TIERS", envir = E6), "B16_BOGUS_TIER"),
                        envir = E6)
                 list(conc_time_log = e_entry) }),
  error = function(e) conditionMessage(e))
ok(is.character(r6) && grepl("expected one of core, catalog", r6, fixed = TRUE),
   "E: a lazy argument that widens EDA_REGISTRY_TIERS mid-call is REFUSED (B1)")
# Every policy vector a gated call can reach is in the sealed environment, and
# the two helpers that enforce them refuse without it.
E7 <- mkE()
e_keys <- sort(ls(get(".eda_policy", envir = E7), all.names = TRUE))
ok(identical(e_keys, sort(c("core_tables", "dependency_files", "inapplicable_prefix",
                            "registry_rows", "registry_fields", "registry_tiers",
                            "registry_transforms", "figure_kinds", "derive_kinds",
                            "marker_rows", "smooth_methods", "bin_rules",
                            "correlation_methods", "correlation_use"))),
   sprintf("E: the sealed policy environment holds all %d policy vectors (B1)", length(e_keys)))
r7 <- tryCatch(E7$.eda_check_entry("x", e_entry, "not an environment"),
               error = function(e) conditionMessage(e))
ok(is.character(r7) && grepl("without the sealed policy", r7, fixed = TRUE),
   "E: .eda_check_entry() refuses without the sealed policy environment")
r8 <- tryCatch(E7$.eda_role_col("x", list(), list(), "nm", NULL, "not an environment"),
               error = function(e) conditionMessage(e))
ok(is.character(r8) && grepl("without the sealed policy", r8, fixed = TRUE),
   "E: .eda_role_col() refuses without the sealed policy environment")

# The comment now describes the enclosure check as a DEPTH check and says what
# it does not compare (the reviewer's second point).
e_src <- paste(readLines(file.path(REPO, EDA_REF), warn = FALSE), collapse = "\n")
ok(grepl("A DEPTH CHECK", e_src, fixed = TRUE) &&
     grepl("IT DOES NOT compare the IDENTITY of any enclosing environment", e_src, fixed = TRUE),
   "E: the enclosure comment is worded as a depth check and states what it does not compare")

setwd(old_wd)
unlink(e_rt, recursive = TRUE)

cat("\n--- F: R15-P3-1, release validates its own snapshot ------------------------\n")

ag <- new.env(parent = globalenv())
setwd(REPO)
suppressWarnings(suppressMessages({
  sys.source(file.path(TOOLS, "gate_package.R"), envir = ag, keep.source = FALSE)
  sys.source(file.path(TOOLS, "gate.R"), envir = ag, keep.source = FALSE)
  sys.source(file.path(TOOLS, "approve_gate.R"), envir = ag, keep.source = FALSE)
}))

# Static: the release path takes its snapshot FIRST and validates THAT.
f_rel <- deparse(ag$release_gate)
f_i_snap <- grep("snap <- .gate_snapshot(gate_file)", f_rel, fixed = TRUE)[1]
f_i_req <- grep("lines = snap$lines, bytes = snap$bytes", f_rel, fixed = TRUE)[1]
ok(!is.na(f_i_snap) && !is.na(f_i_req) && f_i_req > f_i_snap,
   "F: release_gate() validates the locked snapshot, after taking it")
ok(!any(grepl("require_gate(gate_file, enforce_ledger = FALSE, verify_artifacts = FALSE)",
              f_rel, fixed = TRUE)),
   "F: release_gate() no longer validates an independent re-read of the live path")

f_wd <- tempfile("b16gate"); dir.create(file.path(f_wd, "gates", "TST-016"), recursive = TRUE)
setwd(f_wd)
writeLines("hello", "a.csv")
f_arts <- data.frame(artifact = "t1", path = "a.csv", source = "ta", stringsAsFactors = FALSE)
f_tabs <- list("Verdicts" = data.frame(criterion = "F1", value = 1.23, stringsAsFactors = FALSE))
f_rec <- file.path("gates", "TST-016", "eda.md")
writeLines(c("# Gate Sign-off Record", "", "Analysis ID: TST-016", "Stage: eda",
             "Date opened: 2026-09-06", "", "## SME decision", "",
             "- [ ] APPROVED — stage complete, next stage may start",
             "- [ ] APPROVED WITH CHANGES — changes listed below were applied and re-verified",
             "- [ ] REJECTED — returned to stage with instructions below", "",
             "Signed: <!-- SME name -->", "Date: <!-- -->", "Digest signed: <!-- paste -->"), f_rec)
ag$render_gate_package(f_rec, f_arts, f_tabs, c("a note"))
f_rendered <- readLines(f_rec)
f_sign <- function(lines) {
  dg <- sub("^Package-Digest: ", "", grep("^Package-Digest:", lines, value = TRUE)[1])
  i <- grep("^- \\[ \\] APPROVED —", lines)[1]
  lines[i] <- sub("\\[ \\]", "[x]", lines[i])
  lines[grep("^Digest signed:", lines)] <- paste("Digest signed:", dg)
  lines[grep("^Signed:", lines)] <- "Signed: Fixture Reviewer"
  lines[grep("^Date:", lines)] <- "Date: 2026-09-06"
  lines
}
A <- f_sign(f_rendered)
# B: content-valid (same package digest, same signature) but DIFFERENT BYTES.
B <- A; B[grep("^Date opened:", B)] <- "Date opened: 2026-09-07"
writeLines(A, f_rec)
f_ev <- tryCatch(suppressMessages(ag$approve_gate(f_rec, check_store = FALSE)),
                 error = function(e) conditionMessage(e))
ok(is.list(f_ev) && identical(f_ev$signer, "Fixture Reviewer"),
   "F: control -- the valid record is approved and ledgered")

# THE LEDGER RECORD-HASH COMPARISON USES THE SUPPLIED BYTES. The live path still
# holds A, whose digest the ledger carries; the caller supplies B. Before this
# batch require_gate() re-hashed the live file here and passed.
f_bytes <- charToRaw(paste0(paste(B, collapse = "\n"), "\n"))
f_r <- tryCatch(suppressMessages(ag$require_gate(f_rec, lines = B, bytes = f_bytes)),
                error = function(e) conditionMessage(e))
ok(is.character(f_r) && grepl("Gate ledger is stale", f_r, fixed = TRUE),
   "F: with a snapshot supplied, the ledger record-hash is compared against THOSE bytes")
# Positive control: the matching snapshot still validates.
f_bytesA <- charToRaw(paste0(paste(A, collapse = "\n"), "\n"))
f_ok <- tryCatch({ suppressMessages(ag$require_gate(f_rec, lines = A, bytes = f_bytesA)); "OK" },
                 error = function(e) conditionMessage(e))
if (!identical(f_ok, "OK")) cat(sprintf("      got: %s\n", substr(f_ok, 1, 200)))
ok(identical(f_ok, "OK"), "F: the matching snapshot still passes ledger enforcement")

# Release of the unchanged record still works, from its own snapshot.
f_rel_ev <- tryCatch({ suppressMessages(ag$release_gate(f_rec, reason = "batch 16 fixture",
                                                        signer = "Fixture Reviewer")); "OK" },
                     error = function(e) conditionMessage(e))
if (!identical(f_rel_ev, "OK")) cat(sprintf("      got: %s\n", substr(f_rel_ev, 1, 250)))
ok(identical(f_rel_ev, "OK"),
   "F: an unchanged approved record is still releasable from its own snapshot")
# Revoke keeps its ability to revoke a defective record.
ok(!any(grepl("require_gate(", deparse(ag$revoke_gate), fixed = TRUE)),
   "F: revoke_gate() acquires no approval requirement (revoke unchanged)")
setwd(old_wd)
unlink(f_wd, recursive = TRUE)

cat("\n--- G: R15-P2-1, the census cache key carries the filesystem ---------------\n")

# A validation run made while an undeclared dependency was ABSENT concluded
# "not a trusted load" and cached that verdict under a key made of two file
# digests. Creating the dependency changed neither, so the stale entry kept
# passing while a fresh census would have refused the undeclared load.
g_rt <- tempfile("b16cache")
dir.create(file.path(g_rt, "tools"), recursive = TRUE)
dir.create(file.path(g_rt, "skills", "probe", "reference"), recursive = TRUE)
file.copy(file.path(TOOLS, "method_manifest.R"), file.path(g_rt, "tools"))
setwd(g_rt)
G <- new.env(parent = globalenv())
sys.source("tools/method_manifest.R", envir = G, keep.source = FALSE)
G_REF <- "skills/probe/reference/probe.R"
writeLines(c(
  '.probe_deps <- base::local({',
  '  d <- base::new.env(parent = base::baseenv())',
  '  for (f in base::c("tools/probe_dep.R")) {',
  '    if (!base::file.exists(f))',
  '      base::stop("probe: required file ", f, " is missing.", call. = FALSE)',
  '    base::sys.source(f, envir = d, keep.source = FALSE)',
  '  }',
  '  d$dependency_sha256 <- base::vapply(base::c("tools/probe_dep.R"), function(f)',
  '    base::paste0("sha256:", digest::digest(file = f, algo = "sha256")),',
  '    base::character(1))',
  '  base::lockEnvironment(d, bindings = TRUE)',
  '  d',
  '}, envir = base::new.env(parent = base::baseenv()))',
  'reference_probe <- function() base::invisible(TRUE)'), G_REF)
writeLines(c("skill: probe", "generated_at: 2026-09-06T00:00:00Z",
             'reviewed_by: "Fixture Reviewer"', 'reviewed_at: "2026-09-06"', "units:", "  probe:",
             "    status: approved",
             paste0("    reference_path: ", G_REF),
             "    entrypoints: [reference_probe]",
             paste0("    sha256: ", G$.mm_file_sha256(G_REF)),
             "    evidence: skills/probe/SKILL.md", '    reason: "fixture"'),
           "skills/probe/MANIFEST.yaml")
g_try <- function() tryCatch({ G$validate_method_manifest("skills/probe/MANIFEST.yaml"); "PASSED" },
                             error = function(e) conditionMessage(e))
g1 <- g_try()
ok(identical(g1, "PASSED"),
   "G: control -- with the dependency file absent the construction is not a trusted load")
writeLines("probe_helper <- function() TRUE", "tools/probe_dep.R")
g2 <- g_try()
ok(is.character(g2) && grepl("does not declare it under depends:", g2, fixed = TRUE),
   "G: creating the dependency invalidates the cached census; the undeclared load is REFUSED")
setwd(old_wd)
unlink(g_rt, recursive = TRUE)
ok(any(grepl(".mm_census_fs_key(ref)",
             readLines(file.path(TOOLS, "method_manifest.R"), warn = FALSE), fixed = TRUE)),
   "G: the census key carries a filesystem term")

# THE SAME SCENARIO WITH A SINGLE-QUOTED DEPENDENCY PATH (fix round 1, review
# blocker B3). The key's filesystem term used to be a regex over the text that
# matched double-quoted literals only, so this reproduced the stale-cache defect
# in full. It is now a walk over the parse tree's character constants.
g2_rt <- tempfile("b16cachesq")
dir.create(file.path(g2_rt, "tools"), recursive = TRUE)
dir.create(file.path(g2_rt, "skills", "probe", "reference"), recursive = TRUE)
file.copy(file.path(TOOLS, "method_manifest.R"), file.path(g2_rt, "tools"))
setwd(g2_rt)
G2 <- new.env(parent = globalenv())
sys.source("tools/method_manifest.R", envir = G2, keep.source = FALSE)
writeLines(c(
  '.probe_deps <- base::local({',
  '  d <- base::new.env(parent = base::baseenv())',
  "  for (f in base::c('tools/probe_dep.R')) {",
  '    if (!base::file.exists(f))',
  '      base::stop("probe: required file ", f, " is missing.", call. = FALSE)',
  '    base::sys.source(f, envir = d, keep.source = FALSE)',
  '  }',
  "  d$dependency_sha256 <- base::vapply(base::c('tools/probe_dep.R'), function(f)",
  '    base::paste0("sha256:", digest::digest(file = f, algo = "sha256")),',
  '    base::character(1))',
  '  base::lockEnvironment(d, bindings = TRUE)',
  '  d',
  '}, envir = base::new.env(parent = base::baseenv()))',
  'reference_probe <- function() base::invisible(TRUE)'), G_REF)
writeLines(c("skill: probe", "generated_at: 2026-09-06T00:00:00Z",
             'reviewed_by: "Fixture Reviewer"', 'reviewed_at: "2026-09-06"', "units:", "  probe:",
             "    status: approved",
             paste0("    reference_path: ", G_REF),
             "    entrypoints: [reference_probe]",
             paste0("    sha256: ", G2$.mm_file_sha256(G_REF)),
             "    evidence: skills/probe/SKILL.md", '    reason: "fixture"'),
           "skills/probe/MANIFEST.yaml")
ok(grepl("probe_dep", G2$.mm_census_fs_key(G_REF), fixed = TRUE),
   "G: the filesystem term sees a SINGLE-quoted dependency path (B3)")
g2_try <- function() tryCatch({ G2$validate_method_manifest("skills/probe/MANIFEST.yaml"); "PASSED" },
                              error = function(e) conditionMessage(e))
ok(identical(g2_try(), "PASSED"),
   "G: control -- single-quoted, dependency absent, not a trusted load")
writeLines("probe_helper <- function() TRUE", "tools/probe_dep.R")
ok(grepl("does not declare it under depends:", g2_try(), fixed = TRUE),
   "G: single-quoted -- creating the dependency invalidates the cache and REFUSES (B3)")
setwd(old_wd)
unlink(g2_rt, recursive = TRUE)

cat("\n--- H: R15-P2-4, one grammar -- every indentation level and one quoting ----\n")

# The full per-manifest, per-unit dependency MAPPING (path and sha256) as each
# reader derives it, compared entry by entry. The batch-15 test collapsed every
# dependency of every manifest into one unique set of paths, which cannot see a
# dependency reassigned from one unit to another, and dropped the digests.
h_r <- character(0)
for (mp in sort(Sys.glob(file.path(REPO, "skills", "*", "MANIFEST.yaml")))) {
  m <- read_method_manifest(mp)
  sk <- basename(dirname(mp))
  for (nm in names(m$units)) for (d in m$units[[nm]]$depends)
    h_r <- c(h_r, sprintf("%s|%s|%s|%s", sk, nm, d$path, d$sha256))
}
h_r <- sort(h_r)
h_py <- suppressWarnings(system2("python3", c("-c", shQuote(paste0(
  "import sys, glob, os, yaml; sys.path.insert(0, ", shQuote(file.path(REPO, ".claude", "hooks")), ");",
  "import protect_paths as pp; out=[];",
  "\nfor m in sorted(glob.glob(", shQuote(file.path(REPO, "skills", "*", "MANIFEST.yaml")), ")):",
  "\n    doc, prob = pp._manifest_document(open(m).read(), yaml)",
  "\n    assert prob is None, (m, prob)",
  "\n    sk = os.path.basename(os.path.dirname(m))",
  "\n    for nm, u in doc['units'].items():",
  "\n        for d in (u.get('depends') or []):",
  "\n            out.append('|'.join([sk, nm, d['path'], d['sha256']]))",
  "\nprint('\\n'.join(sorted(out)))"))), stdout = TRUE, stderr = TRUE))
h_py <- sort(h_py[nzchar(h_py)])
if (!identical(h_r, h_py))
  cat(sprintf("      R=%s\n      py=%s\n", paste(h_r, collapse = " ; "),
              paste(h_py, collapse = " ; ")))
ok(length(h_r) > 0L && identical(h_r, h_py),
   sprintf("H: both readers derive the same %d (skill, unit, path, sha256) dependency row(s)",
           length(h_r)))

# Per refused construct, in BOTH readers. Each document differs from the
# control by exactly the construct named.
h_base <- c("skill: probe", "generated_at: 2026-09-06T00:00:00Z",
            'reviewed_by: "Fixture Reviewer"', 'reviewed_at: "2026-09-06"', "units:", "  probe:",
            "    status: approved",
            "    reference_path: skills/probe/reference/probe.R",
            "    entrypoints: [reference_probe]",
            paste0("    sha256: sha256:", strrep("0", 64)),
            "    evidence: skills/probe/SKILL.md", '    reason: "fixture"',
            "    depends:", "      - path: tools/probe_dep.R",
            paste0("        sha256: sha256:", strrep("0", 64)))
h_r_verdict <- function(lines) {
  f <- tempfile(fileext = ".yaml"); writeLines(lines, f)
  v <- tryCatch({ read_method_manifest(f); "ACCEPTED" }, error = function(e) "REFUSED")
  unlink(f); v
}
h_py_verdict <- function(lines) {
  f <- tempfile(fileext = ".yaml"); writeLines(lines, f)
  o <- suppressWarnings(system2("python3", c("-c", shQuote(paste0(
    "import sys; sys.path.insert(0, ", shQuote(file.path(REPO, ".claude", "hooks")), ");",
    "import protect_paths as pp;",
    "print('REFUSED' if pp._manifest_grammar_problem(open(", shQuote(f), ").read()) else 'ACCEPTED')"))),
    stdout = TRUE, stderr = TRUE))
  unlink(f); o[length(o)]
}
# BOTH COMPLETE READERS (integrity batch 17, Codex round 16 R16-P2-3). The
# verdict the guard acts on is `_manifest_document()`: the strict YAML load, the
# shared grammar and the schema together. Comparing only the grammar lexer with
# the R reader compared half of one reader with all of the other, which is how a
# document the R reader accepts (root fields written after the units block)
# could be refused here unnoticed, and how a document the grammar lexer accepts
# on its own (a unit field at column zero, which the schema then refuses for a
# missing `status`) looked like a disagreement. Each case below now names the
# verdict expected from the grammar lexer AND from the complete reader; the
# complete reader is the one required to agree with read_method_manifest().
h_py_doc_verdict <- function(lines) {
  f <- tempfile(fileext = ".yaml"); writeLines(lines, f)
  o <- suppressWarnings(system2("python3", c("-c", shQuote(paste0(
    "import sys, yaml; sys.path.insert(0, ", shQuote(file.path(REPO, ".claude", "hooks")), ");",
    "import protect_paths as pp;",
    "d, p = pp._manifest_document(open(", shQuote(f), ").read(), yaml);",
    "print('ACCEPTED' if p is None else 'REFUSED')"))),
    stdout = TRUE, stderr = TRUE))
  unlink(f); o[length(o)]
}
h_cases <- list(
  list("the shipped shape",             function(l) l, "ACCEPTED"),
  list("a unit field at five spaces",   function(l) sub("^    status:", "     status:", l), "REFUSED"),
  list("a depends opener at eight",     function(l) sub("^      - path:", "        - path:", l), "REFUSED"),
  list("a depends sha256 at ten",       function(l) sub("^        sha256:", "          sha256:", l), "REFUSED"),
  list("depends: at two spaces",        function(l) sub("^    depends:$", "  depends:", l), "REFUSED"),
  list("a single-quoted depends path",  function(l) sub("- path: tools/probe_dep.R",
                                                        "- path: 'tools/probe_dep.R'", l, fixed = TRUE), "REFUSED"),
  list("a single-quoted reason",        function(l) sub('reason: "fixture"', "reason: 'fixture'", l, fixed = TRUE), "REFUSED"),
  # Review observation O1: every indentation check in the hook was guarded by
  # `in_units`, and the unit-field check accepted column zero, so these two were
  # accepted there and refused by the R reader.
  list("a root field indented two",     function(l) sub("^reviewed_by:", "  reviewed_by:", l), "REFUSED"),
  # A unit field at column zero ENDS the units block in both readers now (batch
  # 17): the R reader flushes the unit and refuses it for a missing `status`;
  # the hook reads the same line as a root field and then refuses the unit
  # FIELDS that follow it, which are now indented inside no unit.
  list("a unit field at column zero",   function(l) sub("^    status:", "status:", l), "REFUSED"),
  # R16-P2-3 itself: root fields written AFTER the completed units block. The R
  # reader accepts this ordering (tools/method_manifest.R resets `in_units` on a
  # column-zero key), so the hook must too.
  list("root fields after the units block",
       function(l) c(l[!grepl("^reviewed_(by|at):", l)], l[grepl("^reviewed_(by|at):", l)]),
       "ACCEPTED"))
for (cs in h_cases) {
  lines <- cs[[2]](h_base)
  want_gram <- if (length(cs) >= 4L) cs[[4]] else cs[[3]]
  vr <- h_r_verdict(lines); vp <- h_py_verdict(lines); vd <- h_py_doc_verdict(lines)
  if (!(identical(vr, cs[[3]]) && identical(vd, cs[[3]]) && identical(vp, want_gram)))
    cat(sprintf("      %s: R=%s py_doc=%s py_gram=%s (wanted %s / grammar %s)\n",
                cs[[1]], vr, vd, vp, cs[[3]], want_gram))
  ok(identical(vr, cs[[3]]) && identical(vd, cs[[3]]),
     sprintf("H: %s -- both COMPLETE readers say %s", cs[[1]], cs[[3]]))
  ok(identical(vp, want_gram),
     sprintf("H: %s -- the hook's grammar lexer says %s", cs[[1]], want_gram))
}

cat("\n--- I: CI battery runner semantics and bounded diagnostics ---------------\n")
.run_ci_runner_tests(file.path(TOOLS, "ci_check.R"),
                     tempfile("b16_battery_fixture_", tmpdir = tempdir()), ok)

cat(sprintf("\nintegrity batch 16 battery: %d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
invisible(TRUE)
}

if (.invoked_as_script()) .run_batch16_tests()
