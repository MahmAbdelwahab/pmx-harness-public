#!/usr/bin/env Rscript
# Run-compute and bbi-debuggability test battery (SME request 2026-09-03).
#
# Exercises, hermetically under tempdir() and with STUB executables only
# (never docker, never a real nmfe or PsN):
#   Part A  draft_run_config() emits compute keys for BOTH lanes, each with
#           an inline comment naming the house default and the effect.
#   Part B  load_engine_config() stops when a parafile is named but absent
#           (path in the message) or the bbi nodes/threads/parallel triple
#           is inconsistent; consistent configs load.
#   Part C  the bbi shim tools/nonmem_shim/run/nmfe75 captures the nmfe
#           console to <run dir>/OUTPUT, appends to harness-run.log, rotates
#           an earlier OUTPUT instead of overwriting it, relays the exit
#           status, and stops before the backend when -parafile= names a
#           missing file.
#   Part D  tools/nonmem_exec.sh (native provision, stub tools on PATH)
#           records the applied compute values in the engine-config-used
#           snapshot, notes the sir MPI drop, and stops on a missing parafile.
#   Part E  the psn-uncertainty sir rule (drop nodes/parafile) still holds.
#
# Run with:  Rscript tools/tests/test_run_compute.R
# Exits non-zero if any assertion fails.

# SAFE TO SOURCE: pipelines tar_source("tools") which recurses into
# tools/tests/; everything observable is guarded behind .invoked_as_script().
.invoked_as_script <- function() {
  fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  length(fa) > 0 && grepl("test_run_compute\\.R$", sub("^--file=", "", fa[1]))
}

.run_run_compute_tests <- function() {
options(warn = -1)  # stubs exit non-zero on purpose; system2 warnings are noise

args <- commandArgs(FALSE)
file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
TOOLS <- normalizePath(file.path(dirname(file_arg), ".."), mustWork = TRUE)
ROOT  <- normalizePath(file.path(TOOLS, ".."), mustWork = TRUE)
source(file.path(TOOLS, "engine_config.R"))

.pass <- 0L; .fail <- 0L
ok <- function(cond, name) {
  if (isTRUE(cond)) { .pass <<- .pass + 1L; cat(sprintf("PASS  %s\n", name)) }
  else { .fail <<- .fail + 1L; cat(sprintf("FAIL  %s\n", name)) }
}
err_msg <- function(expr) tryCatch({ force(expr); NULL },
                                   error = function(e) conditionMessage(e))
has <- function(lines, pattern) any(grepl(pattern, lines, fixed = TRUE))

sandbox <- tempfile("run-compute-"); dir.create(sandbox)
old_wd <- getwd(); on.exit(setwd(old_wd), add = TRUE)

# --- fixture house config ----------------------------------------------------
# Trimmed to the keys under test; values deliberately differ from the
# committed config where it helps prove the draft reads the FIXTURE.
fixture_cfg <- file.path(sandbox, "engine.yaml")
writeLines(c(
  "engines:",
  "  nonmem:",
  "    provision: native",
  "    executor: tools/nonmem_exec.sh",
  "    image: pmx-harness/nonmem:7.5.1",
  "    nm_version_label: docker75",
  "    shim_home: tools/nonmem_shim",
  "    shim_executable: nmfe75",
  "    native_executable: nmfe75",
  "bbi:",
  "  mode: local",
  "  args:",
  "    overwrite: false",
  "    threads: 1",
  "    parallel: false",
  "    parafile: null",
  "psn:",
  "  nm_version: null",
  "  threads: 1",
  "  retries: 3",
  "  tweak_inits: true",
  "  picky: false",
  "  clean: null",
  "  model_subdir: null",
  "  submit: local",
  "  prepend_flags: null",
  "  queue: null",
  "  account: null",
  "  nodes: 1",
  "  parafile: null",
  "  mpi: false"), fixture_cfg)

# ==== Part 0: canonical dispatcher resolver =================================
# One typed result must bind effective values, provenance, compute metadata,
# argv, hash, and reviewed bytes to one immutable engine/harness capture.
resolver_harness <- file.path(sandbox, "harness.yaml")
resolver_license <- file.path(sandbox, "license file.lic")
writeLines("fixture", resolver_license)
writeLines(c("nonmem_image: 'fixture/nonmem:one'",
             paste0("nonmem_license: '", resolver_license, "'"),
             "nonmem_env_file: '/site/profile with spaces.sh'"),
           resolver_harness)
resolved <- tryCatch(
  resolve_dispatcher_config(
    workdir = sandbox, engine_config = fixture_cfg,
    harness = resolver_harness, invocation_id = "unit-invocation",
    tool = "execute", args = c("model 101.mod", "-threads=7", "literal;$value"),
    env = list(NONMEM_PROVISION = "native",
               NONMEM_PROVISION_OVERRIDE_ACK = "",
               NONMEM_IMAGE = "", NONMEM_LICENSE = "", NONMEM_ENV_FILE = "")),
  error = identity)
ok(is.list(resolved) && !inherits(resolved, "error"),
   "0 canonical dispatcher resolver exists and returns a typed record")
ok(is.list(resolved) && identical(resolved$schema_version, 1L) &&
     identical(resolved$provision$value, "native") &&
     identical(resolved$provision$source, "env (equals config)") &&
     identical(resolved$image$value, "fixture/nonmem:one") &&
     identical(resolved$image$source, "harness.yaml") &&
     identical(resolved$license$value, resolver_license) &&
     identical(resolved$env_file$value, "/site/profile with spaces.sh"),
   "0 resolver applies existing environment/config/default precedence with provenance")
ok(is.list(resolved) && identical(resolved$compute$threads, "7") &&
     identical(resolved$compute$nodes, "1") &&
     identical(resolved$args, c("model 101.mod", "-threads=7", "literal;$value")),
   "0 resolver preserves exact argv and derives effective compute metadata")
ok(is.list(resolved) &&
     identical(resolved$config$source, normalizePath(fixture_cfg)) &&
     identical(resolved$config$sha256,
               digest::digest(file = fixture_cfg, algo = "sha256", serialize = FALSE)) &&
     identical(resolved$config$bytes, readBin(fixture_cfg, "raw", n = file.info(fixture_cfg)$size)),
   "0 resolver binds original source, SHA-256, and reviewed bytes")

override <- tryCatch(
  resolve_dispatcher_config(
    sandbox, fixture_cfg, resolver_harness, "override", "execute", character(),
    env = list(NONMEM_PROVISION = "containerized",
               NONMEM_PROVISION_OVERRIDE_ACK = "1",
               NONMEM_IMAGE = "env/image:two", NONMEM_LICENSE = resolver_license,
               NONMEM_ENV_FILE = "")),
  error = identity)
ok(is.list(override) && identical(override$provision$value, "containerized") &&
     identical(override$provision$source, "env-override-acknowledged") &&
     identical(override$provision$displaced, "native"),
   "0 resolver preserves acknowledged provision displacement evidence")
unacknowledged <- err_msg(resolve_dispatcher_config(
  sandbox, fixture_cfg, resolver_harness, "refuse", "execute", character(),
  env = list(NONMEM_PROVISION = "containerized",
             NONMEM_PROVISION_OVERRIDE_ACK = "",
             NONMEM_IMAGE = "", NONMEM_LICENSE = "", NONMEM_ENV_FILE = "")))
ok(!is.null(unacknowledged) && grepl("NONMEM_PROVISION_OVERRIDE_ACK=1", unacknowledged,
                                    fixed = TRUE),
   "0 resolver refuses an unacknowledged provision override")

resolver_cli <- file.path(TOOLS, "resolve_engine_config.R")
handoff <- file.path(sandbox, "resolved-context")
dir.create(handoff, mode = "0700")
cli_out <- suppressWarnings(system2(
  "Rscript",
  shQuote(c(resolver_cli, "--workdir", sandbox,
            "--engine-config", fixture_cfg, "--harness", resolver_harness,
            "--handoff", handoff,
            "--invocation", "cli-invocation", "--", "execute",
            "model 101.mod", "-threads=7", "literal;$value")),
  stdout = TRUE, stderr = TRUE))
cli_status <- attr(cli_out, "status") %||% 0L
expected_context_files <- c("env-file", "image", "image-source", "license",
                            "license-source", "provision", "schema-version")
ok(cli_status == 0L && identical(sort(list.files(handoff)), expected_context_files) &&
     identical(readLines(file.path(handoff, "schema-version")), "1") &&
     identical(readLines(file.path(handoff, "image")), "fixture/nonmem:one"),
   "0 CLI writes a complete private fixed-file protocol without evaluating values")
cli_snapshots <- list.files(file.path(sandbox, "engine-config-used"), full.names = TRUE)
cli_lines <- if (length(cli_snapshots) == 1L) readLines(cli_snapshots) else character()
ok(length(cli_snapshots) == 1L &&
     any(cli_lines == "# argv: execute model 101.mod -threads=7 literal;$value") &&
     sum(grepl("^# argv_item: ", cli_lines)) == 4L &&
     any(cli_lines == paste0("# config_source: ", normalizePath(fixture_cfg))) &&
     identical(yaml::read_yaml(cli_snapshots)$engines$nonmem$provision, "native"),
   "0 CLI atomically binds exact argv metadata and reviewed body in durable evidence")
ok(!any(grepl("^# (license|native_env_file|harness_source|harness_sha256):",
              cli_lines)),
   "0 durable evidence excludes private harness, license, and native env-file metadata")

injection_work <- file.path(sandbox, "injection-work"); dir.create(injection_work)
injection_context <- file.path(sandbox, "injection-context"); dir.create(injection_context)
injection <- suppressWarnings(system2(
  "Rscript",
  shQuote(c(resolver_cli, "--workdir", injection_work,
            "--engine-config", fixture_cfg, "--harness", resolver_harness,
            "--handoff", injection_context,
            "--invocation", "injection", "--", "execute\n# forged: yes")),
  stdout = TRUE, stderr = TRUE))
ok((attr(injection, "status") %||% 0L) != 0L &&
     length(list.files(injection_context, all.files = TRUE, no.. = TRUE)) == 0L &&
     !dir.exists(file.path(injection_work, "engine-config-used")),
   "0 CR/LF dynamic metadata refuses before handoff or durable evidence")

multiline_harness <- file.path(sandbox, "multiline-harness.yaml")
writeLines(c("nonmem_image: |", "  local/nonmem:first", "  forged"), multiline_harness)
multiline_work <- file.path(sandbox, "multiline-work"); dir.create(multiline_work)
multiline_context <- file.path(sandbox, "multiline-context"); dir.create(multiline_context)
multiline <- suppressWarnings(system2(
  "Rscript",
  shQuote(c(resolver_cli, "--workdir", multiline_work,
            "--engine-config", fixture_cfg, "--harness", multiline_harness,
            "--handoff", multiline_context,
            "--invocation", "multiline", "--", "execute")),
  stdout = TRUE, stderr = TRUE))
ok((attr(multiline, "status") %||% 0L) != 0L &&
     length(list.files(multiline_context, all.files = TRUE, no.. = TRUE)) == 0L &&
     !dir.exists(file.path(multiline_work, "engine-config-used")),
   "0 multiline reviewed scalar refuses before handoff or durable evidence")

insecure_work <- file.path(sandbox, "insecure-work"); dir.create(insecure_work)
insecure_context <- file.path(sandbox, "insecure-context"); dir.create(insecure_context)
Sys.chmod(insecure_context, "0755")
insecure <- suppressWarnings(system2(
  "Rscript",
  shQuote(c(resolver_cli, "--workdir", insecure_work,
            "--engine-config", fixture_cfg, "--harness", resolver_harness,
            "--handoff", insecure_context,
            "--invocation", "insecure", "--", "execute")),
  stdout = TRUE, stderr = TRUE))
ok((attr(insecure, "status") %||% 0L) != 0L &&
     length(list.files(insecure_context, all.files = TRUE, no.. = TRUE)) == 0L &&
     !dir.exists(file.path(insecure_work, "engine-config-used")),
   "0 CLI refuses a non-private handoff directory before publication")

# ==== Part A: draft carries compute keys for both lanes ======================
wdA <- file.path(sandbox, "draftA"); dir.create(wdA)
suppressWarnings(draft_run_config(wdA, run = "T 101", purpose = "test",
                                  lane = "both", default = fixture_cfg))
draft <- readLines(file.path(wdA, "run-config.yaml"))
bbi_start <- grep("^bbi:", draft); psn_start <- grep("^psn:", draft)
bbi_block <- draft[bbi_start:(psn_start - 1)]
psn_block <- draft[psn_start:length(draft)]

for (k in c("mode", "threads", "parallel", "parafile", "nodes")) {
  line <- grep(sprintf("^\\s+%s:", k), bbi_block, value = TRUE)
  ok(length(line) == 1, sprintf("A bbi draft carries `%s`", k))
  ok(length(line) == 1 && grepl("house default", line, fixed = TRUE),
     sprintf("A bbi `%s` names its house default", k))
}
nodes_line <- grep("^\\s+nodes:", bbi_block, value = TRUE)
ok(length(nodes_line) == 1 && grepl("threads", nodes_line, fixed = TRUE),
   "A bbi `nodes` comment states how it maps onto bbi (via threads)")

for (k in c("threads", "nodes", "parafile", "submit", "queue", "account")) {
  line <- grep(sprintf("^\\s+%s:", k), psn_block, value = TRUE)
  ok(length(line) == 1 && grepl("house default", line, fixed = TRUE),
     sprintf("A psn `%s` names its house default", k))
}
for (k in c("retries", "tweak_inits", "picky")) {
  line <- grep(sprintf("^\\s+%s:", k), psn_block, value = TRUE)
  ok(length(line) == 1 && grepl("method-affecting", line, fixed = TRUE),
     sprintf("A psn `%s` still flagged method-affecting", k))
}
parsed <- yaml::read_yaml(file.path(wdA, "run-config.yaml"))
ok(identical(parsed$bbi$args$nodes, 1L) && identical(parsed$bbi$args$threads, 1L) &&
   identical(parsed$psn$nodes, 1L) && identical(parsed$psn$retries, 3L),
   "A drafted values seed from the fixture house config")

# ==== Part B: load_engine_config stops on bad compute settings ===============
write_run_cfg <- function(dir, bbi_lines = character(0), psn_lines = character(0)) {
  dir.create(dir, showWarnings = FALSE)
  writeLines(c("engines:", "  nonmem:", "    provision: native",
               "    executor: tools/nonmem_exec.sh",
               "bbi:", "  mode: local", "  args:", "    overwrite: false",
               bbi_lines,
               "psn:", "  threads: 1", "  retries: 3", psn_lines),
             file.path(dir, "run-config.yaml"))
  dir
}
wdB1 <- write_run_cfg(file.path(sandbox, "B1"),
  bbi_lines = c("    threads: 4", "    parallel: true",
                "    parafile: absent.pnm", "    nodes: 4"))
m <- err_msg(load_engine_config(wdB1, default = fixture_cfg))
ok(!is.null(m) && grepl(file.path(wdB1, "absent.pnm"), m, fixed = TRUE),
   "B bbi parafile named but absent stops, path in message")

wdB2 <- write_run_cfg(file.path(sandbox, "B2"),
  psn_lines = c("  nodes: 2", "  parafile: gone.pnm"))
m <- err_msg(load_engine_config(wdB2, default = fixture_cfg))
ok(!is.null(m) && grepl(file.path(wdB2, "gone.pnm"), m, fixed = TRUE),
   "B psn parafile named but absent stops, path in message")

wdB3 <- write_run_cfg(file.path(sandbox, "B3"),
  bbi_lines = c("    threads: 1", "    parallel: true",
                "    parafile: ok.pnm", "    nodes: 4"))
writeLines(c("$GENERAL", "NODES=4 PARSE_TYPE=2"), file.path(wdB3, "ok.pnm"))
m <- err_msg(load_engine_config(wdB3, default = fixture_cfg))
ok(!is.null(m) && grepl("threads", m) && grepl("nodes", m),
   "B bbi parallel with nodes != threads stops (bbi carries nodes via threads)")

wdB4 <- write_run_cfg(file.path(sandbox, "B4"),
  bbi_lines = c("    threads: 4", "    parallel: true",
                "    parafile: ok.pnm", "    nodes: 4"))
writeLines(c("$GENERAL", "NODES=8 PARSE_TYPE=2"), file.path(wdB4, "ok.pnm"))
m <- err_msg(load_engine_config(wdB4, default = fixture_cfg))
ok(!is.null(m) && grepl("NODES=8", m, fixed = TRUE),
   "B bbi nodes disagreeing with the parafile NODES= line stops")

wdB5 <- write_run_cfg(file.path(sandbox, "B5"),
  bbi_lines = c("    threads: 4", "    parallel: true",
                "    parafile: ok.pnm", "    nodes: 4"),
  psn_lines = c("  nodes: 4", "  parafile: ok.pnm"))
writeLines(c("$GENERAL", "NODES=4 PARSE_TYPE=2"), file.path(wdB5, "ok.pnm"))
cfg5 <- tryCatch(load_engine_config(wdB5, default = fixture_cfg), error = function(e) NULL)
ok(is.list(cfg5) && identical(cfg5$bbi$args$nodes, 4L),
   "B consistent compute settings with an existing parafile load")

wdB6 <- write_run_cfg(file.path(sandbox, "B6"),
  bbi_lines = c("    threads: 1", "    parallel: false", "    nodes: 2"))
m <- err_msg(load_engine_config(wdB6, default = fixture_cfg))
ok(!is.null(m) && grepl("parallel", m),
   "B bbi nodes > 1 without parallel: true stops")

# ==== Part C: shim writes OUTPUT + harness-run.log, rotates, relays status ===
shim <- file.path(TOOLS, "nonmem_shim", "run", "nmfe75")
stub <- file.path(sandbox, "stub_backend.sh")
writeLines(c("#!/usr/bin/env bash",
             "# Stub backend: records the call, prints to both streams, exits",
             "# with STUB_EXIT (default 0). Never touches docker or nmfe.",
             "echo \"invoked $*\" >> \"$(dirname \"$0\")/stub-calls.log\"",
             "echo \"STUB NMFE STDOUT args=$*\"",
             "echo \"STUB NMFE STDERR\" >&2",
             "exit ${STUB_EXIT:-0}"), stub)
Sys.chmod(stub, "0755")
run_shim <- function(rundir, args, exit = 0L) {
  system2(shim, args, stdout = TRUE, stderr = TRUE,
          env = c(paste0("NONMEM_SHIM_BACKEND=", stub),
                  paste0("STUB_EXIT=", exit)),
          wait = TRUE) -> out
  st <- attr(out, "status"); if (is.null(st)) st <- 0L
  list(out = out, status = st)
}
rdC <- file.path(sandbox, "runC"); dir.create(rdC)
setwd(rdC)
r1 <- run_shim(rdC, c("101.ctl", "101.lst", "-maxlim=2"))
setwd(old_wd)
ok(identical(as.integer(r1$status), 0L), "C shim relays exit 0 from the backend")
ok(file.exists(file.path(rdC, "OUTPUT")), "C shim writes <run dir>/OUTPUT")
outp <- if (file.exists(file.path(rdC, "OUTPUT"))) readLines(file.path(rdC, "OUTPUT")) else character(0)
ok(has(outp, "STUB NMFE STDOUT") && has(outp, "STUB NMFE STDERR"),
   "C OUTPUT captures both stdout and stderr of nmfe")
ok(file.exists(file.path(rdC, "harness-run.log")), "C shim writes <run dir>/harness-run.log")
logp <- if (file.exists(file.path(rdC, "harness-run.log"))) readLines(file.path(rdC, "harness-run.log")) else character(0)
ok(has(logp, "argv:") && has(logp, "101.ctl 101.lst -maxlim=2"),
   "C harness-run.log records the nmfe argument line")
ok(has(logp, "provision:") && has(logp, "exit_status: 0") &&
   has(logp, "started_utc:") && has(logp, "finished_utc:"),
   "C harness-run.log records provision, timestamps and exit status")
ok(has(logp, stub), "C harness-run.log names the backend actually executed")
ok(has(logp, paste0("run_dir: ", rdC)), "C harness-run.log records the run directory")
calls <- readLines(file.path(sandbox, "stub-calls.log"))
ok(length(calls) == 1 && grepl(paste0(rdC, " nmfe75 101.ctl 101.lst -maxlim=2"), calls[1], fixed = TRUE),
   "C backend receives <run dir> nmfe75 <args> exactly as before")

# second attempt: rotation + append + non-zero relay
setwd(rdC)
r2 <- run_shim(rdC, c("101.ctl", "101.lst", "-maxlim=2"), exit = 7L)
setwd(old_wd)
ok(identical(as.integer(r2$status), 7L), "C shim relays a non-zero backend exit status")
rotated <- list.files(rdC, pattern = "^OUTPUT\\.")
ok(length(rotated) == 1, "C second attempt rotates the earlier OUTPUT with a timestamp suffix")
ok(length(readLines(file.path(rdC, "OUTPUT"))) == length(outp),
   "C current OUTPUT holds only the latest attempt")
logp2 <- readLines(file.path(rdC, "harness-run.log"))
ok(sum(grepl("exit_status:", logp2)) == 2 && has(logp2, "exit_status: 7"),
   "C harness-run.log appends across attempts")
ok(has(logp2, paste0("rotated_previous_output: ", file.path(rdC, rotated[1]))),
   "C harness-run.log names the rotated OUTPUT")

# missing parafile: stop BEFORE the backend
rdC2 <- file.path(sandbox, "runC2"); dir.create(rdC2)
setwd(rdC2)
r3 <- run_shim(rdC2, c("102.ctl", "102.lst", "-parafile=102.pnm", "-maxlim=2"))
setwd(old_wd)
ok(!identical(as.integer(r3$status), 0L), "C missing parafile is a non-zero stop")
ok(any(grepl(file.path(rdC2, "102.pnm"), r3$out, fixed = TRUE)),
   "C missing-parafile message names the resolved path")
ok(length(readLines(file.path(sandbox, "stub-calls.log"))) == 2,
   "C backend NOT invoked when the parafile is missing")
ok(file.exists(file.path(rdC2, "harness-run.log")) &&
   has(readLines(file.path(rdC2, "harness-run.log")), "parafile"),
   "C the stop is itself recorded in harness-run.log")

# present parafile passes through
writeLines(c("$GENERAL", "NODES=2"), file.path(rdC2, "102.pnm"))
setwd(rdC2)
r4 <- run_shim(rdC2, c("102.ctl", "102.lst", "-parafile=102.pnm", "-maxlim=2"))
setwd(old_wd)
ok(identical(as.integer(r4$status), 0L), "C present parafile passes through to the backend")

# ==== Part D: dispatcher snapshot records applied compute; parafile stop =====
exec_sh <- file.path(TOOLS, "nonmem_exec.sh")
bindir <- file.path(sandbox, "bin"); dir.create(bindir)
for (tool in c("execute", "sir")) {
  writeLines(c("#!/usr/bin/env bash",
               sprintf("echo \"%s-stub $*\" >> \"$(dirname \"$0\")/tool-calls.log\"", tool),
               "exit 0"), file.path(bindir, tool))
  Sys.chmod(file.path(bindir, tool), "0755")
}
# An explicit (empty) env file keeps the dispatcher from sourcing a
# machine-local harness.yaml nonmem_env_file, which may reset PATH and
# hide the stub tools behind a real site PsN.
env_file <- file.path(sandbox, "empty.env"); writeLines("# no-op", env_file)
run_exec <- function(wd, args) {
  out <- system2(exec_sh, c(wd, args), stdout = TRUE, stderr = TRUE,
                 env = c(paste0("PATH=", bindir, ":", Sys.getenv("PATH")),
                         "NONMEM_PROVISION=native",
                         paste0("NONMEM_ENV_FILE=", env_file)))
  st <- attr(out, "status"); if (is.null(st)) st <- 0L
  list(out = out, status = st)
}
snap_lines <- function(wd) {
  f <- list.files(file.path(wd, "engine-config-used"), full.names = TRUE)
  f <- f[order(file.info(f)$mtime, f)]
  readLines(f[length(f)])
}

wdD <- file.path(sandbox, "D"); dir.create(wdD)
writeLines(c("engines:", "  nonmem:", "    provision: native",
             "    executor: tools/nonmem_exec.sh",
             "bbi:", "  mode: local", "  args:", "    threads: 1",
             "psn:", "  threads: 3", "  retries: 3", "  submit: slurm",
             "  queue: batch", "  account: pmx", "  nodes: 2",
             "  parafile: mpilinux8.pnm"),
           file.path(wdD, "run-config.yaml"))
writeLines(c("$GENERAL", "NODES=2"), file.path(wdD, "mpilinux8.pnm"))
rD <- run_exec(wdD, c("execute", "run101.mod", "-threads=3", "-nodes=2",
                      "-parafile=mpilinux8.pnm", "-run_on_slurm"))
ok(identical(as.integer(rD$status), 0L), "D dispatcher runs the stub execute")
sn <- snap_lines(wdD)
ok(has(sn, "# compute_applied: threads=3 nodes=2 parafile=mpilinux8.pnm submit=slurm queue=batch account=pmx"),
   "D snapshot records the applied PsN compute values")
ok(has(sn, "# compute_argv:") && has(sn, "-nodes=2") && has(sn, "-parafile=mpilinux8.pnm"),
   "D snapshot records the compute tokens present in argv")

rS <- run_exec(wdD, c("sir", "run101.mod", "-samples=100", "-threads=3"))
ok(identical(as.integer(rS$status), 0L), "D dispatcher runs the stub sir")
snS <- snap_lines(wdD)
ok(has(snS, "# compute_applied: threads=3 nodes=1 parafile=none"),
   "D sir snapshot records that MPI was NOT applied (nodes=1, no parafile)")
ok(has(snS, "sir") && has(snS, "MPI"),
   "D sir snapshot states the reason: sir prohibits MPI NONMEM")

wdD2 <- file.path(sandbox, "D2"); dir.create(wdD2)
file.copy(file.path(wdD, "run-config.yaml"), wdD2)
rD2 <- run_exec(wdD2, c("execute", "run101.mod", "-nodes=2", "-parafile=mpilinux8.pnm"))
ok(!identical(as.integer(rD2$status), 0L), "D dispatcher stops when argv names a missing parafile")
ok(any(grepl(file.path(wdD2, "mpilinux8.pnm"), rD2$out, fixed = TRUE)),
   "D dispatcher stop names the resolved parafile path")
calls <- readLines(file.path(bindir, "tool-calls.log"))
ok(!any(grepl("D2", calls)) && length(calls) == 2,
   "D tool NOT invoked when the parafile is missing")

# explicit -sir with MPI tokens must never be accepted silently: the sir
# refusal lives in the reference script (Part E); here the dispatcher must
# still record what it was asked. A -parafile= for sir is a stop only through
# the reference script, so no dispatcher assertion beyond the record.

# ==== Part E: psn-uncertainty sir rule still drops MPI =======================
ue <- new.env()
source(file.path(ROOT, "skills", "psn-uncertainty", "reference", "psn_uncertainty.R"),
       local = ue)
cfgE <- list(psn = list(threads = 3L, nodes = 4L, parafile = "mpilinux8.pnm",
                        retries = 1L, tweak_inits = TRUE, picky = FALSE))
a_full <- ue$.psn_operational_args(cfgE, mpi_ok = TRUE)
a_sir  <- suppressMessages(ue$.psn_operational_args(cfgE, mpi_ok = FALSE))
ok(any(grepl("^-nodes=4$", a_full)) && any(grepl("^-parafile=mpilinux8.pnm$", a_full)),
   "E non-sir tools emit -nodes/-parafile from config")
ok(!any(grepl("nodes|parafile", a_sir)) && any(grepl("^-threads=3$", a_sir)),
   "E sir drops nodes/parafile but keeps threads")

cat(sprintf("\nrun-compute tests: %d passed, %d failed\n", .pass, .fail))
invisible(.fail)
}

if (.invoked_as_script()) {
  quit(status = if (.run_run_compute_tests() == 0L) 0L else 1L)
}
