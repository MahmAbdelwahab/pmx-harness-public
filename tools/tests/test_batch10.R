#!/usr/bin/env Rscript
# Integrity batch 10 test battery — the live-engine findings.
#
# Every assertion here descends from a defect that a green test battery did
# not catch, because the batteries stub the engine and the defects only
# appear when NONMEM or PsN actually runs (analyst runtime-verification
# report, 2026-09-04, findings V1-B..E and V2-A..D). The lesson is recorded
# in the battery itself: a lane that is only ever exercised against a stub
# can be uniformly green and still have never completed once.
#
# Hermetic, same pattern as tools/tests/test_batch9.R: every fixture lives
# under tempdir() laid out as <root>/skills/<skill>/reference/<file>.R +
# MANIFEST.yaml, with copies of the tools/ files a reference sources, and the
# working directory is moved there so repo-relative paths resolve exactly as
# they do in the repository. No live manifest, gate record, reference file,
# store, pilot output or harness.yaml is read or touched.
#
# Source of the copied files: the repository by default; the environment
# variable PMX_BATCH10_SOURCE_ROOT overrides it (a HEAD export or a staged
# patch tree laid out like the repository), which is how the red (pre-fix)
# and green (post-fix) runs are produced. Every run states its source.
#
# Run with:  Rscript tools/tests/test_batch10.R
# Exits non-zero if any assertion fails.
#
# SAFE TO SOURCE: guarded behind .invoked_as_script() like the other
# batteries, so tar_source("tools") only defines and runs nothing.
.invoked_as_script <- function() {
  fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  length(fa) > 0 && grepl("test_batch10\\.R$", sub("^--file=", "", fa[1]))
}

.run_batch10_tests <- function() {

args <- commandArgs(FALSE)
file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
TOOLS <- normalizePath(file.path(dirname(file_arg), ".."), mustWork = TRUE)
REPO  <- normalizePath(file.path(TOOLS, ".."), mustWork = TRUE)
SRC   <- Sys.getenv("PMX_BATCH10_SOURCE_ROOT", REPO)
SRC   <- normalizePath(SRC, mustWork = TRUE)
cat(sprintf("batch10: copying reference/tool files from %s\n", SRC))

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

# --- fixture root ------------------------------------------------------------
root <- tempfile("batch10"); dir.create(root)
old_wd <- setwd(root); on.exit(setwd(old_wd), add = TRUE)
dir.create("tools")
for (f in c("method_manifest.R", "record_selection.R", "seed.R")) {
  s <- src_file(file.path("tools", f))
  if (file.exists(s)) file.copy(s, file.path("tools", f), overwrite = TRUE)
}
source("tools/method_manifest.R")
sha <- function(p) .mm_file_sha256(p)

install_skill <- function(skill, files, units, reviewed_by = "Fixture Reviewer",
                          reviewed_at = "2026-09-04") {
  d <- file.path("skills", skill, "reference")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  for (f in files) {
    s <- src_file(file.path("skills", skill, "reference", f))
    if (!file.exists(s)) return(NA_character_)
    file.copy(s, file.path(d, f), overwrite = TRUE)
  }
  lines <- c("# fixture", paste0("skill: ", skill),
             "generated_at: 2026-09-04T00:00:00Z",
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
  }
  p <- file.path("skills", skill, "MANIFEST.yaml"); writeLines(lines, p)
  p
}
load_ref <- function(skill, f) {
  e <- new.env(parent = globalenv())
  sys.source(file.path("skills", skill, "reference", f), envir = e)
  e
}

# ==== 10.1 bbr lane — skills/nonmem-estimation/reference/bbr_estimation.R ====
mf <- install_skill("nonmem-estimation", "bbr_estimation.R", list(
  estimation = list(status = "approved", ref = "bbr_estimation.R",
                    entrypoints = c("reference_nm_workspace", "reference_nm_fit",
                                    "reference_nm_results"))))
ok(!is.na(mf), "10.1: nonmem-estimation/bbr_estimation.R present in source tree")
if (!is.na(mf)) {
  nm <- load_ref("nonmem-estimation", "bbr_estimation.R")

  # ---- V1-B. The termination status is read from the listing, not the tail ---
  # bbr::tail_lst(.print = FALSE) returns invisible NULL unless .return = TRUE,
  # so the reference always received length 0 and refused every real fit. And
  # A termination status deep in a generated listing is outside a short tail window.
  lst_ok <- file.path(tempdir(), "b10-ok.lst")
  writeLines(c("Fixture listing start",
               "$PROBLEM Synthetic termination fixture",
               rep("  filler", 300),
               "0MINIMIZATION SUCCESSFUL",
               " NO. OF FUNCTION EVALUATIONS USED:      27",
               rep("  filler", 40),
               " #OBJV:*****      42.250       *****",
               "Stop Time:", "Fixture listing end"), lst_ok)

  got <- tryCatch(nm$.reference_nm_termination_lines(lst_ok), error = function(e) e)
  ok(is.character(got) && any(grepl("^0MINIMIZATION SUCCESSFUL", got)),
     "10.1a: the termination block is read from the listing bytes, not the tail window")
  ok(is.character(got) && any(grepl("#OBJV", got)),
     "10.1a: the #OBJV line is carried with the termination status")

  lst_term <- file.path(tempdir(), "b10-term.lst")
  writeLines(c(rep("  filler", 40), "0MINIMIZATION TERMINATED",
               " #OBJV:*****      56.750       *****"), lst_term)
  got2 <- tryCatch(nm$.reference_nm_termination_lines(lst_term), error = function(e) e)
  ok(is.character(got2) && any(grepl("^0MINIMIZATION TERMINATED", got2)),
     "10.1a: a TERMINATED run yields its verbatim status rather than an error")

  # Absence of both is the refusal it already is — a run record without a
  # termination status is a failed run, never a degraded field.
  lst_none <- file.path(tempdir(), "b10-none.lst")
  writeLines(c("Fixture listing start", "$PROBLEM x", "Stop Time:"), lst_none)
  expect_stop(nm$.reference_nm_termination_lines(lst_none),
              "10.1a: a listing with neither a MINIMIZATION line nor #OBJV is refused",
              "termination")
  expect_stop(nm$.reference_nm_termination_lines(file.path(tempdir(), "b10-absent.lst")),
              "10.1a: a missing listing is refused", "listing")

  # The reference no longer calls tail_lst without .return, which is what made
  # the value invisible in the first place.
  txt <- readLines(src_file("skills/nonmem-estimation/reference/bbr_estimation.R"),
                   warn = FALSE)
  # Code lines only: the surrounding commentary necessarily quotes the broken
  # call it is explaining, and a comment is not a call.
  code <- grep("^\\s*#", txt, value = TRUE, invert = TRUE)
  tl <- grep("tail_lst\\(", code, value = TRUE)
  ok(length(tl) == 0 || all(grepl("\\.return\\s*=\\s*TRUE", tl)),
     "10.1a: every tail_lst() call passes .return = TRUE")

  # ---- V1-E. n_obs is the observation count, not the record count -----------
  # Generated counts are distinct to catch substitution between fields.
  rd <- list(number_of_subjects = 3, number_of_data_records = 12,
             number_of_obs = 7)
  cnt <- tryCatch(nm$.reference_nm_counts(rd), error = function(e) e)
  ok(!inherits(cnt, "error") && identical(as.numeric(cnt$n_obs), 7),
     "10.1b: n_obs is number_of_obs in the generated summary")
  ok(!inherits(cnt, "error") && identical(as.numeric(cnt$n_records), 12),
     "10.1b: the record count is kept under its own name, n_records (12)")
  ok(!inherits(cnt, "error") && identical(as.numeric(cnt$n_id), 3),
     "10.1b: n_id is number_of_subjects (3)")
  expect_stop(nm$.reference_nm_counts(list(number_of_subjects = 3,
                                           number_of_data_records = 12)),
              "10.1b: a summary without number_of_obs is refused, never defaulted",
              "number_of_obs")

  # The "unverified — confirm at first real run" hedges the live run retired.
  ok(!any(grepl("unverified — confirm at first real run", txt, fixed = TRUE)),
     "10.1b: the retired 'confirm at first real run' hedges are gone")
  ok(is.character(got) && any(grepl("#OBJV:*****      42.250", got, fixed = TRUE)),
     "10.1b: a generated objective line is preserved verbatim")

  # ---- V1-D. an absolute $DATA and bbr's nm_join are incompatible -----------
  # bbr's get_data_path_from_json does file.path(output_dir, data_path), which
  # for an absolute $DATA yields <rundir>/<abs path> — a file that cannot
  # exist. The harness control streams use an absolute $DATA deliberately (PsN
  # and bbi resolve relative data paths from different directories). Neither
  # decision may be quietly reversed here, so the lane REFUSES and names both.
  ctl_abs <- file.path(tempdir(), "b10-abs.ctl")
  writeLines(c("$PROBLEM x", "$DATA /home/site/data/observations.csv IGNORE=C",
               "$INPUT C NUM ID TIME DV"), ctl_abs)
  expect_stop(nm$.reference_nm_require_joinable_data(ctl_abs),
              "10.1c: an absolute $DATA is refused before nm_join is called",
              "nm_join")
  e <- tryCatch(nm$.reference_nm_require_joinable_data(ctl_abs), error = function(e) e)
  ok(inherits(e, "error") && grepl("get_data_path_from_json", conditionMessage(e)),
     "10.1c: the refusal names the bbr defect by function")
  ok(inherits(e, "error") && grepl("PsN", conditionMessage(e)),
     "10.1c: the refusal names the PsN/bbi parity decision it conflicts with")

  ctl_rel <- file.path(tempdir(), "b10-rel.ctl")
  writeLines(c("$PROBLEM x", "$DATA ../data/observations.csv IGNORE=C",
               "$INPUT C NUM ID TIME DV"), ctl_rel)
  ok(is.null(tryCatch(nm$.reference_nm_require_joinable_data(ctl_rel),
                      error = function(e) e)) ||
       !inherits(tryCatch(nm$.reference_nm_require_joinable_data(ctl_rel),
                          error = function(e) e), "error"),
     "10.1c: a relative $DATA is joinable and passes")

  # ---- V1-C. options(bbr.bbi_exe_path) is set nowhere -----------------------
  # bbr's default is the bare string "bbi", so bbi_version() returns "" and any
  # submit_model() fails on a machine where bbi is not on PATH. The executable
  # is machine-local, so it comes from harness.yaml (git-ignored), never from
  # config/engine.yaml — the same rule nonmem_home already follows.
  # Integrity batch 11 (R10-P2-2). This block used to create a PLAIN,
  # non-executable file with writeLines() and assert it was accepted as the
  # bbi executable, pinning the missing executable check. The resolver now
  # requires a non-directory, executable file whose `bbi version` returns a
  # version string; the probe is injected so no bbi installation is needed.
  probe_ok <- function(path) "v3.4.0"
  fake_bbi <- file.path(tempdir(), "b10-bbi"); writeLines("#!/bin/sh", fake_bbi)
  expect_stop(nm$.reference_nm_require_bbi(fake_bbi, which = function(x) "",
                                           probe = probe_ok),
              "10.1d: a non-executable plain file is REFUSED as the bbi executable",
              "not executable")
  Sys.chmod(fake_bbi, "0755")
  got <- tryCatch(nm$.reference_nm_require_bbi(fake_bbi, which = function(x) "",
                                               probe = probe_ok),
                  error = function(e) e)
  ok(!inherits(got, "error") && identical(as.character(got), fake_bbi),
     "10.1d: an explicit, executable bbi_path is used as the executable")
  ok(!inherits(got, "error") &&
       identical(attr(got, "bbi_version"), "v3.4.0"),
     "10.1d: the resolver returns the recorded bbi version")
  ok(identical(getOption("bbr.bbi_exe_path"), fake_bbi),
     "10.1d: the resolved executable is installed into options(bbr.bbi_exe_path)")

  bbi_dir <- file.path(tempdir(), "b10-bbi-dir")
  dir.create(bbi_dir, showWarnings = FALSE)
  expect_stop(nm$.reference_nm_require_bbi(bbi_dir, which = function(x) "",
                                           probe = probe_ok),
              "10.1d: a directory named as bbi_path is refused", "is a directory")
  expect_stop(nm$.reference_nm_require_bbi(fake_bbi, which = function(x) "",
                                           probe = function(path) NA_character_),
              "10.1d: an executable whose `bbi version` says nothing is refused",
              "did not report a version")

  got <- tryCatch(nm$.reference_nm_require_bbi(NULL,
                                               which = function(x) "/usr/bin/bbi",
                                               probe = probe_ok),
                  error = function(e) e)
  ok(!inherits(got, "error") && identical(as.character(got), "/usr/bin/bbi"),
     "10.1d: bbi on PATH is accepted when no key is configured")

  expect_stop(nm$.reference_nm_require_bbi(NULL, which = function(x) ""),
              "10.1d: no bbi_path and no bbi on PATH is refused", "bbi_path")
  e <- tryCatch(nm$.reference_nm_require_bbi(NULL, which = function(x) ""),
                error = function(e) e)
  ok(inherits(e, "error") && grepl("harness.yaml", conditionMessage(e), fixed = TRUE),
     "10.1d: the refusal names harness.yaml as the source of the key")
  expect_stop(nm$.reference_nm_require_bbi(file.path(tempdir(), "b10-no-such-bbi"),
                                           which = function(x) ""),
              "10.1d: a configured bbi_path that does not exist is refused",
              "bbi_path")

  # ---- fix round 1 of batch 11: the REAL probe, not an injected one ---------
  # Every case above injects `probe =`, so .reference_nm_bbi_version() itself
  # was never exercised by any test. It merged stderr into stdout and ignored
  # the exit status, so /bin/sh passed as bbi and the shell's error message
  # ("/bin/sh: 0: cannot open version: No such file") was recorded as the engine
  # version and surfaced to the SME in the environment fingerprint. The three
  # cases below call the resolver with its DEFAULT probe.
  if (file.exists("/bin/sh")) {
    e <- tryCatch(nm$.reference_nm_require_bbi("/bin/sh", which = function(x) ""),
                  error = function(e) e)
    ok(inherits(e, "error"),
       "11/F2: /bin/sh is REFUSED by the real version probe (it is not bbi)")
    if (inherits(e, "error")) {
      msg <- conditionMessage(e)
      ok(grepl("/bin/sh", msg, fixed = TRUE),
         "11/F2: the refusal names the configured path")
      ok(grepl("exit status", msg, fixed = TRUE),
         "11/F2: the refusal names the exit status the probe saw")
      ok(!grepl("cannot open version", msg, fixed = TRUE),
         "11/F2: the shell's error text is not carried into the message as a version")
    } else {
      ok(FALSE, "11/F2: the refusal names the configured path")
      ok(FALSE, "11/F2: the refusal names the exit status the probe saw")
      ok(FALSE, "11/F2: the shell's error text is not carried into the message as a version")
    }
  }

  # An executable that prints a version string is accepted; one that prints
  # something that is not a version is refused rather than recorded.
  stub_ok <- file.path(tempdir(), "b11-bbi-ok")
  writeLines(c("#!/bin/sh", "echo v9.9.9"), stub_ok); Sys.chmod(stub_ok, "0755")
  got <- tryCatch(nm$.reference_nm_require_bbi(stub_ok, which = function(x) ""),
                  error = function(e) e)
  ok(!inherits(got, "error") && identical(attr(got, "bbi_version"), "v9.9.9"),
     "11/F2: an executable reporting `v9.9.9` is accepted and the version recorded")

  stub_junk <- file.path(tempdir(), "b11-bbi-junk")
  writeLines(c("#!/bin/sh", "echo garbage"), stub_junk); Sys.chmod(stub_junk, "0755")
  expect_stop(nm$.reference_nm_require_bbi(stub_junk, which = function(x) ""),
              "11/F2: an executable printing `garbage` is refused, not recorded as the version",
              "did not report a version")
}

# tools/check_environment.R probes the same key rather than inventing a path.
ce <- readLines(src_file("tools/check_environment.R"), warn = FALSE)
ok(any(grepl("harness\\$bbi_path|harness\\[\\[\"bbi_path\"\\]\\]", ce)),
   "10.1d: tools/check_environment.R probes the harness.yaml bbi_path key")
ok(!any(grepl('\\.local/share/bbi/bbi', grep("^\\s*#", ce, value = TRUE, invert = TRUE))),
   "10.1d: tools/check_environment.R invents no bbi install path of its own")

# harness.yaml.example documents the key (the SME's own harness.yaml is never
# read by this battery, or by anything else in the repository's tests).
hx <- readLines(src_file("harness.yaml.example"), warn = FALSE)
ok(any(grepl("^\\s*#?\\s*bbi_path\\s*:", hx)),
   "10.1d: harness.yaml.example documents the bbi_path key")
sk <- readLines(src_file("skills/nonmem-estimation/SKILL.md"), warn = FALSE)
ok(any(grepl("bbi_path", sk, fixed = TRUE)),
   "10.1d: SKILL.md documents where the bbi executable comes from")

# ==== 10.2 SCM lane — skills/covariate-priors/reference/scm_config.R =========
mf <- install_skill("covariate-priors", "scm_config.R", list(
  scm_config = list(status = "approved", ref = "scm_config.R",
                    entrypoints = c("reference_scm_config",
                                    "reference_scm_config_backward"))))
ok(!is.na(mf), "10.2: covariate-priors/scm_config.R present in source tree")
if (!is.na(mf)) {
  sk <- src_file("skills/covariate-priors/SKILL_SCM.md")
  if (file.exists(sk)) file.copy(sk, "skills/covariate-priors/SKILL_SCM.md",
                                 overwrite = TRUE)
  sc <- load_ref("covariate-priors", "scm_config.R")

  priors <- list(CL = data.frame(covariate = c("WT", "APGR"),
                                 type = c("continuous", "continuous"),
                                 form = c("power", "none"),
                                 stringsAsFactors = FALSE),
                 V  = data.frame(covariate = "WT", type = "continuous",
                                 form = "power", stringsAsFactors = FALSE))

  # A control stream in the TVxx form PsN's scm requires (V2-B).
  ctl_tv <- "b10-tv.ctl"
  writeLines(c("$PROBLEM fixture", "$DATA data.csv", "$INPUT C ID TIME DV WT APGR",
               "$PK", "LWT  = LOG(WT/1.30)", "TVCL = EXP(THETA(1)+0.75*LWT)",
               "CL   = TVCL*EXP(ETA(1))", "TVV  = EXP(THETA(2)+1.0*LWT)",
               "V    = TVV*EXP(ETA(2))", "S1   = V", "$ERROR", "Y = F + F*EPS(1)"),
             ctl_tv)
  # The harness's own parity control stream: algebraically equivalent, and not
  # scm-usable, because scm has no TVxx to insert the covariate code around.
  ctl_notv <- "b10-notv.ctl"
  writeLines(c("$PROBLEM fixture", "$DATA data.csv", "$INPUT C ID TIME DV WT APGR",
               "$PK", "LWT = LOG(WT/1.30)", "CL  = EXP(THETA(1)+0.75*LWT+ETA(1))",
               "V   = EXP(THETA(2)+1.0*LWT+ETA(2))", "S1  = V",
               "$ERROR", "Y = F + F*EPS(1)"), ctl_notv)

  # ---- V2-A. nm_version has no default; it is the site psn.conf label -------
  # reference_scm_config(nm_version = 7) produced `nm_version=7`, which PsN
  # refuses at option parsing: 'No NONMEM version with name "7" defined in
  # psn.conf'. The fixture supplies local labels explicitly.
  fml <- names(formals(sc$reference_scm_config))
  ok("nm_version" %in% fml,
     "10.2a: reference_scm_config still takes nm_version")
  ok(identical(formals(sc$reference_scm_config)$nm_version, quote(expr = )),
     "10.2a: nm_version has no default in reference_scm_config")
  ok(identical(formals(sc$reference_scm_config_backward)$nm_version, quote(expr = )),
     "10.2a: nm_version has no default in reference_scm_config_backward")
  expect_stop(sc$reference_scm_config(model = "101.ctl", prior_list = priors,
                                      directory = "scm1", control_stream = ctl_tv,
                                      nm_version = ""),
              "10.2a: an empty nm_version is refused", "nm_version")
  expect_stop(sc$reference_scm_config(model = "101.ctl", prior_list = priors,
                                      directory = "scm1", control_stream = ctl_tv,
                                      nm_version = NULL),
              "10.2a: a NULL nm_version is refused", "nm_version")

  cfg <- tryCatch(sc$reference_scm_config(model = "101.ctl", prior_list = priors,
                                          directory = "scm1",
                                          control_stream = ctl_tv,
                                          nm_version = "nm751"),
                  error = function(e) e)
  if (inherits(cfg, "error")) cat("      got: ", conditionMessage(cfg), "\n")
  ok(!inherits(cfg, "error"), "10.2a: the generator runs with an explicit site label")
  ok(!inherits(cfg, "error") && any(cfg == "nm_version=nm751"),
     "10.2a: the site psn.conf label is emitted verbatim")

  # ---- V2-D. do_not_drop covers every covariate on the prior list -----------
  # scm marks covariates a candidate does not use as =DROP in $INPUT. The
  # harness control stream consumes WT structurally (LWT = LOG(WT/1.30)), so a
  # candidate that dropped WT failed NMTRAN with "208 UNDEFINED VARIABLE" and
  # PsN recorded it as FAILED 999 and carried on with exit status 0 — a lost
  # candidate model, silently. Under the exclusion decision this could not bite
  # in the run observed, which was fortunate rather than designed.
  if (!inherits(cfg, "error")) {
    dnd <- grep("^do_not_drop=", cfg, value = TRUE)
    ok(length(dnd) == 1L, "10.2b: exactly one do_not_drop line is emitted")
    if (length(dnd) == 1L) {
      cov <- strsplit(sub("^do_not_drop=", "", dnd), ",", fixed = TRUE)[[1]]
      ok(all(c("WT", "APGR") %in% cov),
         "10.2b: every covariate on the prior list is protected from DROP")
      ok("APGR" %in% cov,
         "10.2b: an EXCLUDED covariate is protected too — it is still in $INPUT")
    }
  }
  expect_stop(sc$reference_scm_config(model = "101.ctl", prior_list = priors,
                                      directory = "scm1", control_stream = ctl_tv,
                                      nm_version = "nm751",
                                      extra_options = list(do_not_drop = "WT")),
              "10.2b: extra_options setting the reserved do_not_drop is refused",
              "reserved")

  # ---- V2-B. the TVxx precondition is checked, not assumed ------------------
  expect_stop(sc$reference_scm_config(model = "101.ctl", prior_list = priors,
                                      directory = "scm1",
                                      control_stream = ctl_notv,
                                      nm_version = "nm751"),
              "10.2c: a control stream with no TVCL for a searched parameter is refused",
              "TVCL")
  e <- tryCatch(sc$reference_scm_config(model = "101.ctl", prior_list = priors,
                                        directory = "scm1",
                                        control_stream = ctl_notv,
                                        nm_version = "nm751"),
                error = function(e) e)
  # Integrity batch 11 (SME decision 3, 2026-09-04): TV-parameterised $PK is
  # the ONLY route. The [code] promise was removed from the refusal because the
  # API could not honour it -- this check runs before extra_code is considered
  # and [code] is appended only afterwards. The refusal now names the
  # reparameterisation and nothing else.
  ok(inherits(e, "error") && !grepl("\\[code\\]", conditionMessage(e)) &&
     grepl("Reparameterize \\$PK into TV<PARAM> form", conditionMessage(e)),
     "10.2c: the refusal names reparameterisation only, not a [code] alternative")
  expect_stop(sc$reference_scm_config(model = "101.ctl", prior_list = priors,
                                      directory = "scm1",
                                      control_stream = "b10-no-such.ctl",
                                      nm_version = "nm751"),
              "10.2c: an unreadable control stream is refused, never skipped",
              "cannot be read")

  # The check reads the $PK block only: a TVCL appearing in a comment or in
  # $ERROR is not a typical-value assignment scm can insert around.
  ctl_decoy <- "b10-decoy.ctl"
  writeLines(c("$PROBLEM fixture", "$DATA data.csv", "$INPUT C ID TIME DV WT APGR",
               "; TVCL = EXP(THETA(1))", "$PK",
               "CL = EXP(THETA(1)+ETA(1))", "TVV = EXP(THETA(2))",
               "V  = TVV*EXP(ETA(2))", "$ERROR", "TVCL = 1", "Y = F + F*EPS(1)"),
             ctl_decoy)
  expect_stop(sc$reference_scm_config(model = "101.ctl", prior_list = priors,
                                      directory = "scm1",
                                      control_stream = ctl_decoy,
                                      nm_version = "nm751"),
              "10.2c: a TVCL in a comment or in $ERROR does not satisfy the precondition",
              "TVCL")

  # SKILL_SCM.md states the precondition, so the analyst meets it at the MAP
  # rather than at the first failed scm run.
  sm <- readLines(src_file("skills/covariate-priors/SKILL_SCM.md"), warn = FALSE)
  ok(any(grepl("TV", sm)) && any(grepl("typical", sm, ignore.case = TRUE)),
     "10.2c: SKILL_SCM.md carries the TVxx precondition")
  ok(any(grepl("nm751|psn\\.conf", sm)),
     "10.2a: SKILL_SCM.md says nm_version is the site psn.conf label")
}

# ==== 10.4 stale DRAFT headers ==============================================
# The SME's decision is to correct a stale header only at a natural re-sign.
# bbr_estimation.R is re-signed by 10.1 anyway, so its header is corrected
# here; the other six each cost a signature and are left alone. This asserts
# the decision in both directions so neither half drifts.
bh <- readLines(src_file("skills/nonmem-estimation/reference/bbr_estimation.R"),
                warn = FALSE)[1:12]
ok(!any(grepl("STATUS: DRAFT", bh)),
   "10.4: bbr_estimation.R no longer declares DRAFT in its header")
ok(any(grepl("MANIFEST\\.yaml", bh)),
   "10.4: bbr_estimation.R's header points at the manifest as the status authority")

cat(sprintf("\nintegrity batch 10 battery: %d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
invisible(TRUE)
}

if (.invoked_as_script()) .run_batch10_tests()
