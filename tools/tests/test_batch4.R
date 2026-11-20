#!/usr/bin/env Rscript
# Integrity batch 4 test battery (one strict gate parser, approval hardening,
# render-from-store, manifest dependencies, signed errata).
#
# Hermetic: every fixture lives under tempdir(); no file under gates/** is
# touched. Run with
#   Rscript tools/tests/test_batch4.R
# Exits non-zero if any assertion fails.
#
# SAFE TO SOURCE: guarded behind .invoked_as_script() exactly like
# tools/tests/test_integrity.R, so tar_source("tools") only defines and runs
# nothing.
.invoked_as_script <- function() {
  fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  length(fa) > 0 && grepl("test_batch4\\.R$", sub("^--file=", "", fa[1]))
}

.run_batch4_tests <- function() {

suppressWarnings(suppressMessages({
  args <- commandArgs(FALSE)
  file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
  TOOLS <- normalizePath(file.path(dirname(file_arg), ".."), mustWork = TRUE)
  REPO  <- normalizePath(file.path(TOOLS, ".."), mustWork = TRUE)
  source(file.path(TOOLS, "gate_package.R"))
  source(file.path(TOOLS, "gate.R"))
  source(file.path(TOOLS, "approve_gate.R"))
  if (file.exists(file.path(TOOLS, "errata.R"))) source(file.path(TOOLS, "errata.R"))
}))

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
  ok(hit, name)
}
quiet <- function(expr) suppressMessages(suppressWarnings(expr))
messages_of <- function(expr) {
  msgs <- character(0)
  withCallingHandlers(expr, message = function(m) {
    msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") })
  msgs
}

# --- fixture repo: <wd>/gates/TST-001/eda.md, artifacts at the repo root ----
wd <- tempfile("batch4"); dir.create(wd)
old <- setwd(wd)
dir.create(file.path(wd, "gates", "TST-001"), recursive = TRUE)
writeLines("hello", "a.csv"); writeLines("world", "b.csv")
arts <- data.frame(artifact = c("t1", "t2"), path = c("a.csv", "b.csv"),
                   source = c("ta", "tb"), stringsAsFactors = FALSE)
tables <- list("Verdicts" = data.frame(criterion = "E1", value = 1.23,
                                        stringsAsFactors = FALSE))

# A gate record body in the template's shape (clean identity lines).
template_body <- function(id = "TST-001", stage = "eda",
                          id_line = NULL, stage_line = NULL) {
  c("# Gate Sign-off Record", "",
    id_line %||% paste0("Analysis ID: ", id),
    stage_line %||% paste0("Stage: ", stage),
    "Date opened: 2026-09-02", "",
    "## SME decision", "",
    "- [ ] APPROVED — stage complete, next stage may start",
    "- [ ] APPROVED WITH CHANGES — changes listed below were applied and re-verified",
    "- [ ] REJECTED — returned to stage with instructions below", "",
    "Signed: <!-- SME name -->", "Date: <!-- -->",
    "Digest signed: <!-- paste -->")
}
sign <- function(lines, signer_line = "Signed: Fixture Reviewer", date_line = "Date: 2026-09-02",
                 box = "APPROVED") {
  dg <- sub("^Package-Digest: ", "", grep("^Package-Digest:", lines, value = TRUE)[1])
  pat <- if (box == "APPROVED") "^- \\[ \\] APPROVED —" else paste0("^- \\[ \\] ", box)
  lines[grep(pat, lines)[1]] <- sub("\\[ \\]", "[x]", lines[grep(pat, lines)[1]])
  lines[grep("^Digest signed:", lines)] <- paste("Digest signed:", dg)
  lines[grep("^Signed:", lines)] <- signer_line
  lines[grep("^Date:", lines)] <- date_line
  lines
}

# ==== A. one strict parser ===================================================
rec <- file.path("gates", "TST-001", "eda.md")
writeLines(template_body(), rec)
render_gate_package(rec, arts, tables, c("a note"))
rendered <- readLines(rec)

# CANON CONSTRAINT: the envelope line for a clean record is byte-identical to
# the pre-batch-4 form (no digest of an existing clean record may change).
ok("Envelope: analysis_id=TST-001; task=eda" %in% rendered,
   "A: envelope line for a clean record is byte-identical to the legacy form")

p_unsigned <- parse_gate_record(rec)
ok(identical(p_unsigned$analysis_id, "TST-001") && identical(p_unsigned$task, "eda") &&
   identical(p_unsigned$decision, "unsigned") && length(p_unsigned$problems) == 0,
   "A: clean unsigned record parses (identity from body, decision unsigned, no problems)")

writeLines(sign(rendered), rec)
p <- parse_gate_record(rec)
ok(identical(p$decision, "approved") && identical(p$signer, "Fixture Reviewer") &&
   identical(p$date, "2026-09-02") && length(p$problems) == 0 &&
   grepl("^canon2:sha256:", p$package_digest) &&
   identical(p$digest_signed, p$package_digest),
   "A: clean signed record parses (decision, signer, date, digests)")
ok(isTRUE(p$artifacts$has_table) && length(p$artifacts$rows) == 2,
   "A: parser exposes the artifacts table via the shared strict table parser")

# signature on one line ("Signed: Fixture Reviewer Date: 02.09.2026") and indented fields
one_line <- sign(rendered, signer_line = "  Signed: Fixture Reviewer Date: 02.09.2026",
                 date_line = "")
rec2 <- file.path("gates", "TST-001", "eda2.md")
writeLines(one_line, rec2)
p2 <- parse_gate_record(rec2)
ok(identical(p2$signer, "Fixture Reviewer") && identical(p2$date, "02.09.2026"),
   "A: leading whitespace tolerated; Date: fragment on the Signed line read")
ok(any(grepl("task", p2$problems)) ,
   "A: file stem 'eda2' vs Stage 'eda' mismatch is a problem")

# merged/reflowed identity line
merged <- paste0("Analysis ID: TST-001 Stage: eda covariate-model \\| qualification",
                 " \\| simulation \\| reporting \\| review --\\> Date opened: 2026-09-01")
rec3 <- file.path("gates", "TST-001", "eda.md")
body3 <- template_body(id_line = merged, stage_line = NULL)
body3 <- body3[body3 != "Stage: eda" & body3 != "Date opened: 2026-09-02"]
writeLines(body3, rec3)
p3 <- parse_gate_record(rec3)
ok(length(p3$problems) > 0 && any(grepl("Analysis ID", p3$problems)),
   "A: merged identity line (review.md:5 shape) is flagged as a problem")
expect_stop(render_gate_package(rec3, arts, tables),
            "A: render refuses a record whose identity fields are malformed",
            "identity|Analysis ID|Stage")

# absent identity lines -> refuse render (no more analysis_id=NA envelopes)
rec4 <- file.path("gates", "TST-001", "eda.md")
writeLines(c("# rec", "", "## SME decision", "", "- [ ] APPROVED",
             "Signed: <!-- -->", "Date: <!-- -->", "Digest signed: <!-- -->"), rec4)
expect_stop(render_gate_package(rec4, arts, tables),
            "A: render refuses a record with no Analysis ID / Stage lines",
            "identity|Analysis ID|Stage")

# directory mismatch is a problem
dir.create(file.path("gates", "OTHER-9"))
rec5 <- file.path("gates", "OTHER-9", "eda.md")
writeLines(sign(rendered), rec5)
p5 <- parse_gate_record(rec5)
ok(any(grepl("directory|OTHER-9", p5$problems)),
   "A: Analysis ID vs parent directory mismatch is a problem")

# envelope drift: identity body lines fine but the Envelope line disagrees
rec6 <- file.path("gates", "TST-001", "eda.md")
env_bad <- sign(rendered)
env_bad[grep("^Envelope:", env_bad)] <- "Envelope: analysis_id=TST-001; task=NA"
writeLines(env_bad, rec6)
p6 <- parse_gate_record(rec6)
ok(any(grepl("[Ee]nvelope", p6$problems)),
   "A: envelope line disagreeing with the body identity is a problem")

# approved with empty signer / empty date
writeLines(sign(rendered, signer_line = "Signed: <!-- SME name -->"), rec6)
p7 <- parse_gate_record(rec6)
ok(identical(p7$decision, "approved") && any(grepl("signer", p7$problems)),
   "A: approved with empty signer is a problem")
expect_stop(require_gate(rec6), "A: require_gate refuses approved record with empty signer",
            "not signed|signer")
writeLines(sign(rendered, date_line = "Date: <!-- -->"), rec6)
p8 <- parse_gate_record(rec6)
ok(any(grepl("date", p8$problems)), "A: approved with empty date is a problem")
expect_stop(require_gate(rec6), "A: require_gate refuses approved record with empty date",
            "date")

# clean record passes every consumer identically
writeLines(sign(rendered), rec)
ok(!is.null(tryCatch(quiet(require_gate(rec)), error = function(e) NULL)),
   "A: clean signed record passes require_gate")
# A malformed-identity signed record still passes require_gate (message only:
# the two live malformed records are pinned in CI until re-versioned).
writeLines(env_bad, rec6)
msgs <- messages_of(tryCatch(require_gate(rec6), error = function(e) NULL))
ok(any(grepl("identity", msgs)),
   "A: require_gate reports (does not stop on) an identity problem of a signed record")

# ==== B. approve_gate hardening ==============================================
# indented Signed: line (previously required column zero) now records.
setwd(old); setwd(wd)
writeLines(sign(rendered, signer_line = "   Signed: Fixture Reviewer", date_line = "Date: 2026-09-02"), rec)
ev <- tryCatch(quiet(approve_gate(rec, check_store = FALSE)), error = function(e) e)
ok(!inherits(ev, "error") && identical(ev$signer, "Fixture Reviewer") && identical(ev$signed_at, "2026-09-02"),
   "B: approve_gate records signer/date from the shared parser (indented Signed:)")

# empty date -> refused before any event is written
n_before <- length(readLines(file.path("gates", "TST-001", "approvals.jsonl")))
writeLines(sign(rendered, date_line = "Date: <!-- -->"), rec)
expect_stop(quiet(approve_gate(rec, check_store = FALSE)),
            "B: approve_gate refuses when the date is empty", "date")
ok(length(readLines(file.path("gates", "TST-001", "approvals.jsonl"))) == n_before,
   "B: refused approval appended nothing")

# legacy DIGESTLESS record (frozen in the allowlist, no Package-Digest line):
# require_gate accepts it on the legacy path, but the ledger must never receive
# an approving event with an empty package_digest (that event would break the
# task chain). approve_gate refuses BEFORE writing, regardless of check_store.
# Hermetic: the allowlist lives in the temp fixture's own tools/ directory.
lrec <- file.path("gates", "TST-001", "legacy.md")
lbody <- template_body(stage = "legacy")
lbody[grep("^- \\[ \\] APPROVED —", lbody)[1]] <-
  sub("\\[ \\]", "[x]", lbody[grep("^- \\[ \\] APPROVED —", lbody)[1]])
lbody[grep("^Signed:", lbody)] <- "Signed: Fixture Reviewer"
lbody[grep("^Date:", lbody)] <- "Date: 2026-08-30"
writeLines(lbody, lrec)
dir.create(file.path(wd, "tools"), showWarnings = FALSE)
expect_stop(require_gate(lrec),
            "B4-LEGACY: no local allowlist refuses the digestless fixture",
            "legacy|allowlist|no Package-Digest")
writeLines(c("entries:", "  - kind: digestless", paste0("    path: ", lrec),
             paste0("    sha256: ", .gate_sha256(lrec))),
           file.path(wd, "tools", "legacy_gates.yaml"))
ok(!is.null(tryCatch(quiet(require_gate(lrec)), error = function(e) NULL)) &&
   is.na(parse_gate_record(lrec)$package_digest),
   "B: fixture legacy digestless record passes require_gate and parses with no Package-Digest")
n_before <- length(readLines(file.path("gates", "TST-001", "approvals.jsonl")))
expect_stop(quiet(approve_gate(lrec, check_store = FALSE)),
            "B: approve_gate refuses a legacy digestless record (no Package-Digest)",
            "legacy|digest")
ok(length(readLines(file.path("gates", "TST-001", "approvals.jsonl"))) == n_before,
   "B: refused legacy digestless approval appended nothing to the ledger")
unlink(file.path(wd, "tools"), recursive = TRUE)

# verify: an event with a null signer fails completeness (chain intact)
ledger_dir <- file.path(wd, "gates", "TST-001")
ok(verify_approvals_ledger(ledger_dir)$ok, "B: clean ledger verifies")
evs <- .read_ledger(file.path(ledger_dir, "approvals.jsonl"))
bad_ev <- evs[[1]]; bad_ev$signer <- NA_character_
bad_ev$event_id <- .ledger_event_id(bad_ev)   # chain link recomputed -> only completeness catches it
dir.create(file.path(wd, "gates", "BAD-001"))
writeLines(.ledger_to_json(bad_ev), file.path(wd, "gates", "BAD-001", "approvals.jsonl"))
vr <- verify_approvals_ledger(file.path(wd, "gates", "BAD-001"))
ok(!vr$ok && any(grepl("signer", vr$problems)),
   "B: verify flags an approving event whose signer is null (required-field completeness)")

# outdated *_package target -> approval refused (targets fixture)
tdir <- file.path(wd, "trepo"); dir.create(file.path(tdir, "analyses"), recursive = TRUE)
dir.create(file.path(tdir, "gates", "TGT-001"), recursive = TRUE)
setwd(tdir)
writeLines("hello", "a.csv")
# The fixture carries an `environment_attestation` target with a fixed value so
# render-from-store can be shown to bind the fingerprint (round 7, F10).
FIX_FP <- paste0("sha256:", paste(rep("ab", 32), collapse = ""))
writeLines(c("library(targets)",
             "list(",
             sprintf("  tar_target(environment_attestation, list(fingerprint = '%s', nonmem_provision = 'containerized', r_provision = 'managed', environment_confirmed = TRUE)),", FIX_FP),
             "  tar_target(x, 1),",
             "  tar_target(eda_package, list(artifacts = data.frame(artifact = 'a', path = 'a.csv', source = 'x', stringsAsFactors = FALSE),",
             "                               tables = list(T = data.frame(v = x)), notes = 'n'))",
             ")"), file.path("analyses", "TGT-001.R"))
quiet(callr::r(function() targets::tar_make(script = "analyses/TGT-001.R",
                                            store = "_targets_TGT-001", reporter = "silent"),
               wd = getwd()))
ok(dir.exists("_targets_TGT-001"), "B: fixture store built")
trec <- file.path("gates", "TGT-001", "eda.md")
writeLines(template_body(id = "TGT-001", stage = "eda"), trec)

# ==== C. render-from-store ===================================================
render_gate_from_store("TGT-001", "eda")
tl <- readLines(trec)
ok(any(grepl("^Package-Digest: canon2:sha256:", tl)) && any(tl == "| T |") == FALSE &&
   any(grepl("^### T$", tl)),
   "C: render_gate_from_store renders the stored package into the gate record")
ok(any(grepl("^Method-manifests: sha256:[0-9a-f]{64}$", tl)) ||
   !any(grepl("^Method-manifests:", tl)),
   "C: manifest-set digest line, when present, is well-formed")
att_line <- grep("^Environment-attestation: ", tl, value = TRUE)
ok(length(att_line) == 1L && grepl(FIX_FP, att_line, fixed = TRUE) &&
     grepl("nonmem provision: containerized", att_line, fixed = TRUE) &&
     grepl("R provision: managed", att_line, fixed = TRUE),
   "C: the gate package binds the environment-attestation fingerprint and provisions")
bs <- grep("BEGIN GENERATED PACKAGE", tl); be <- grep("END GENERATED PACKAGE", tl)
ok(length(bs) == 1L && length(be) == 1L && which(tl == att_line) > bs && which(tl == att_line) < be,
   "C: the attestation line sits inside the digested block")
writeLines(sign(tl), trec)
ev2 <- tryCatch(quiet(approve_gate(trec)), error = function(e) e)
ok(!inherits(ev2, "error"), "B: approve_gate passes the fresh-store check when nothing is outdated")

# invalidate: edit the script -> package target outdated -> approve refused,
# render_gate_from_store refused
writeLines(sub("tar_target\\(x, 1\\)", "tar_target(x, 2)",
               readLines(file.path("analyses", "TGT-001.R"))), file.path("analyses", "TGT-001.R"))
writeLines(c(sign(tl), ""), trec)   # change bytes so idempotence is not what refuses
expect_stop(quiet(approve_gate(trec)),
            "B: approve_gate refuses when the *_package target is OUTDATED in its store",
            "OUTDATED|outdated")
writeLines(template_body(id = "TGT-001", stage = "eda"), trec)
expect_stop(quiet(render_gate_from_store("TGT-001", "eda")),
            "C: render_gate_from_store refuses a stale store (package or upstream outdated)",
            "OUTDATED|outdated")
setwd(wd)

# ==== D. method manifests as pipeline dependencies (compose-level) ==========
synthetic_project <- tempfile("batch4-synthetic-")
dir.create(synthetic_project)
for (p in c("pipeline", "analyses", "config"))
  stopifnot(file.copy(file.path(REPO, p), synthetic_project, recursive = TRUE))
dir.create(file.path(synthetic_project, "tools"))
stopifnot(all(file.copy(list.files(TOOLS, pattern = "[.]R$", full.names = TRUE),
                       file.path(synthetic_project, "tools"))))
(function() {
  previous <- setwd(synthetic_project)
  on.exit(setwd(previous))
  status <- system2(file.path(R.home("bin"), "Rscript"),
                    c("--vanilla", "analyses/synthetic_example.R", "--generate"), timeout = 15)
  stopifnot(identical(status, 0L))
})()
comp <- tryCatch(callr::r(function(repo) {
  setwd(repo)
  env <- new.env(parent = globalenv())
  pipe <- source("analyses/synthetic_example.R", local = env)$value
  source("pipeline/compose.R", local = env)
  nm <- vapply(pipe, function(t) t$settings$name, character(1))
  stage <- pipe[!grepl("^gate_", nm) & !nm %in% c("environment_attestation", "method_manifests")]
  has_dep <- vapply(stage, function(t) "method_manifests" %in% all.names(t$command$expr[[1]]), logical(1))
  mm <- pipe[nm == "method_manifests"]
  fmt <- if (length(mm)) mm[[1]]$settings$format else NA_character_
  dom_ok <- tryCatch(env$verify_gate_dominance(pipe), error = function(e) conditionMessage(e))
  # strip the manifest edge from one stage target -> verifier must refuse
  idx <- which(!grepl("^gate_", nm) & !nm %in% c("environment_attestation", "method_manifests"))[2]
  t <- pipe[[idx]]
  inner <- t$command$expr[[1]]
  # inner is { environment_attestation; method_manifests; <gate>; cmd } (or
  # without the gate): drop the method_manifests symbol only
  parts <- as.list(inner)
  stripped <- as.call(parts[!vapply(parts, function(x) identical(x, as.name("method_manifests")), logical(1))])
  pipe2 <- pipe
  pipe2[[idx]] <- targets::tar_target_raw(t$settings$name, stripped, format = t$settings$format)
  dom_bad <- tryCatch(env$verify_gate_dominance(pipe2), error = function(e) conditionMessage(e))
  list(has_mm = length(mm) == 1, fmt = fmt, all_dep = all(has_dep), n = length(stage),
       dom_ok = dom_ok, dom_bad = dom_bad)
}, args = list(synthetic_project)), error = function(e) e)
ok(!inherits(comp, "error") && isTRUE(comp$has_mm) && identical(comp$fmt, "file"),
   "D: compose declares one file target `method_manifests` over skills/*/MANIFEST.yaml")
ok(!inherits(comp, "error") && isTRUE(comp$all_dep),
   sprintf("D: every stage target (%s) carries the method_manifests dependency",
           if (inherits(comp, "error")) "?" else comp$n))
ok(!inherits(comp, "error") && isTRUE(comp$dom_ok),
   "D: verify_gate_dominance passes on the composed pipeline")
ok(!inherits(comp, "error") && is.character(comp$dom_bad) && grepl("method_manifests", comp$dom_bad),
   "D: verify_gate_dominance refuses a stage target lacking the method_manifests edge")
setwd(wd)

# ==== E. signed errata =======================================================
# fresh gate + ledgered approval to hang errata on
edir <- file.path(wd, "erepo"); dir.create(file.path(edir, "gates", "ERR-001"), recursive = TRUE)
setwd(edir)
writeLines("hello", "a.csv"); writeLines("world", "b.csv")
erec <- file.path("gates", "ERR-001", "eda.md")
writeLines(template_body(id = "ERR-001", stage = "eda"), erec)
render_gate_package(erec, arts, tables, c("claim: shrinkage exceeds 30% in every candidate"))
writeLines(sign(readLines(erec)), erec)
quiet(approve_gate(erec, check_store = FALSE))
ok(!is.null(tryCatch(quiet(require_gate(erec)), error = function(e) NULL)),
   "E: ledgered fixture gate passes before any erratum")

claim <- "shrinkage exceeds 30% in every candidate"
e1 <- record_erratum(erec, claim = claim,
                     correction = "run 106 tcl shrinkage is 26.2%; the universal claim is false",
                     evidence = "findings-log.csv N2", materiality = "minor",
                     signer = "Fixture Reviewer", date = "2026-09-02")
epath <- file.path("gates", "ERR-001", "errata.jsonl")
ok(file.exists(epath) && length(readLines(epath)) == 1L, "E: record_erratum appends exactly one event")
ok(identical(e1$claim_sha256, paste0("sha256:", digest::digest(claim, algo = "sha256", serialize = FALSE))) &&
   nzchar(e1$claim_excerpt) && identical(e1$record_digest, .gate_sha256(erec)),
   "E: erratum binds the claim text hash, an excerpt, and the approved record digest")
ok(verify_errata_ledger(file.path("gates", "ERR-001"))$ok, "E: errata chain verifies")
msgs <- messages_of(require_gate(erec))
ok(any(grepl("KNOWN SUPERSEDED: 1 erratum", msgs)),
   "E: minor erratum -> require_gate passes with a KNOWN SUPERSEDED message")

# signer/date are never inferred
expect_stop(record_erratum(erec, claim = claim, correction = "x", evidence = "y",
                           materiality = "minor", signer = "", date = "2026-09-02"),
            "E: record_erratum refuses an empty signer", "signer")
expect_stop(record_erratum(erec, claim = claim, correction = "x", evidence = "y",
                           materiality = "severe", signer = "Fixture Reviewer", date = "2026-09-02"),
            "E: record_erratum refuses an unknown materiality", "materiality")

# material erratum -> STOP
Sys.sleep(1.1)   # recorded_at_utc has 1 s resolution; the erratum must postdate the approval
e2 <- record_erratum(erec, claim = "12 of 12", correction = "9 rows, 4 bins each",
                     evidence = "target report_vpc_coverage_wt", materiality = "material",
                     signer = "Fixture Reviewer", date = "2026-09-02")
expect_stop(quiet(require_gate(erec)),
            "E: material erratum -> require_gate STOPs", "material|erratum")
ok(verify_errata_ledger(file.path("gates", "ERR-001"))$ok, "E: two-event chain verifies")

# SME re-signs with knowledge (bytes change -> new approving event postdates it) -> passes
Sys.sleep(1.1)
el <- readLines(erec)
el[grep("^Date:", el)] <- "Date: 2026-09-03"
writeLines(el, erec)
quiet(approve_gate(erec, check_store = FALSE))
msgs2 <- messages_of(tryCatch(require_gate(erec), error = function(e) NULL))
ok(any(grepl("KNOWN SUPERSEDED: 2 errata", msgs2)) &&
   !is.null(tryCatch(quiet(require_gate(erec)), error = function(e) NULL)),
   "E: approving event postdating the material erratum -> passes with the notice")

# tamper -> verify fails; append-only primitive shared with approvals
jl <- readLines(epath)
jl[1] <- sub('"materiality":"minor"', '"materiality":"material"', jl[1], fixed = TRUE)
dir.create(file.path(wd, "etamper"))
writeLines(jl, file.path(wd, "etamper", "errata.jsonl"))
ok(!verify_errata_ledger(file.path(wd, "etamper"))$ok,
   "E: edited erratum event is caught (event_id mismatch)")
setwd(wd)

# ==== F. CI: strict-parser WARN inventory pinned at the known records ========
# The pin list lives in ci_check.R (EXPECTED_ENVELOPE_MALFORMED); the real-repo
# assertion reads it rather than hard-coding paths, so a record leaving the pin
# after repair must still agree with the pinned inventory.
cienv <- new.env(); source(file.path(TOOLS, "ci_check.R"), local = cienv)
run_pin <- function(root) {
  wprev <- setwd(root); on.exit(setwd(wprev))
  cienv$.fail_count <- 0L
  out <- capture.output(cienv$check_envelope_pin())
  list(out = out, fail = cienv$.fail_count)
}
r_real <- run_pin(REPO)
pinned <- cienv$EXPECTED_ENVELOPE_MALFORMED
ok(r_real$fail == 0L && (length(pinned) == 0L ||
   (any(grepl("^WARN", r_real$out)) &&
    all(vapply(pinned, function(pth) any(grepl(pth, r_real$out, fixed = TRUE)), logical(1))))),
   "F: real repo -> WARN inventory names exactly the pinned records, 0 FAIL")
# Use a disposable fixture with no inherited gate or trust records.
pinfix <- file.path(wd, "pinfix"); dir.create(file.path(pinfix, "tools"), recursive = TRUE)
file.copy(file.path(REPO, "tools", "gate.R"), file.path(pinfix, "tools", "gate.R"))
ok(run_pin(pinfix)$fail == 0L, "B4-LEGACY: empty fixture matches the public pin")
pin_dir <- file.path(pinfix, "gates", "PIN-001")
dir.create(pin_dir, recursive = TRUE)
pin_rec <- file.path(pin_dir, "review.md")
writeLines(template_body(id = "PIN-001", stage = "review"), pin_rec)
render_gate_package(pin_rec, arts, tables, c("pinned malformed fixture"))
pin_txt <- sign(readLines(pin_rec))
pin_txt <- sub("^Analysis ID: PIN-001$", "Analysis ID: PIN-001 Stage: review Date opened: 2026-09-02", pin_txt)
pin_txt <- pin_txt[!grepl("^Stage: review$|^Date opened: 2026-09-02$", pin_txt)]
writeLines(pin_txt, pin_rec)
saved_pin <- cienv$EXPECTED_ENVELOPE_MALFORMED
cienv$EXPECTED_ENVELOPE_MALFORMED <- c(saved_pin, "gates/PIN-001/review.md")
ok(run_pin(pinfix)$fail == 0L, "F: a pinned malformed record present -> 0 FAIL (WARN only)")
file.remove(pin_rec)
ok(run_pin(pinfix)$fail == 1L, "F: a pinned record disappearing -> pin mismatch FAIL")
cienv$EXPECTED_ENVELOPE_MALFORMED <- saved_pin
unlink(pin_dir, recursive = TRUE)
# A separately generated signed record with a reflow-merged header.
dir.create(file.path(pinfix, "gates", "SNEAK-001"))
writeLines(c("# Gate", "", "Analysis ID: SNEAK-001 Stage: reporting Date opened: 2026-09-02", "",
             GATE_BLOCK_START, "Envelope: analysis_id=SNEAK-001 Stage: reporting; task=NA",
             "Package-Digest: canon2:sha256:0000000000000000000000000000000000000000000000000000000000000000",
             GATE_BLOCK_END, "", "## SME decision", "",
             "- [x] APPROVED — stage complete", "Signed: Fixture Reviewer", "Date: 2026-09-02",
             "Digest signed: canon2:sha256:0000000000000000000000000000000000000000000000000000000000000000"),
           file.path(pinfix, "gates", "SNEAK-001", "reporting.md"))
ok(run_pin(pinfix)$fail == 1L, "F: an additional malformed signed record -> FAIL")

setwd(old)
cat("-----------------------------------------------\n")
# ==== Part G: store-freshness check on a pipeline that tar_source()s helpers ==
# Regression for the first live use (2026-09-02): with callr_function = NULL
# inside a fresh subprocess, targets evaluated the analysis script so that
# functions brought in by its own tar_source() were invisible to the pipeline
# definition ("could not find function %||%"). The fixture mirrors the real
# analysis-script shape: library(targets); tar_source(<dir>); a helper defined
# ONLY in the sourced dir is needed to build the target list.
gwd <- file.path(wd, "fresh"); dir.create(file.path(gwd, "analyses"), recursive = TRUE)
dir.create(file.path(gwd, "helpers"))
writeLines(c("`%||%` <- function(a, b) if (is.null(a)) b else a",
             "mk <- function(nm) targets::tar_target_raw(nm, quote(1 + 1))"),
           file.path(gwd, "helpers", "h.R"))
writeLines(c("library(targets)", "tar_source('helpers')",
             "nm <- NULL %||% 'demo_package'", "list(mk(nm))"),
           file.path(gwd, "analyses", "FRESH-001.R"))
old_wd <- setwd(gwd)
callr::r(function() targets::tar_make(script = "analyses/FRESH-001.R",
                                       store = "_targets_FRESH-001", reporter = "silent"),
         wd = gwd, show = FALSE)
st <- tryCatch(.store_staleness("FRESH-001", "demo"), error = function(e) e)
ok(!inherits(st, "error") && identical(st$outdated, character(0)),
   "G: staleness check composes a tar_source()-dependent script (fresh store -> nothing outdated)")
# touch the helper's target definition -> the package target is outdated
writeLines(c("`%||%` <- function(a, b) if (is.null(a)) b else a",
             "mk <- function(nm) targets::tar_target_raw(nm, quote(1 + 2))"),
           file.path(gwd, "helpers", "h.R"))
expect_stop(require_fresh_store("FRESH-001", "demo"),
            "G: changed target definition -> store OUTDATED refusal", "OUTDATED")
setwd(old_wd)

# ==== Part G2: always-cued upstream targets do not make a fresh store stale ==
# Regression for the first live render attempt after integrity batch 5: the
# composer's `environment_attestation` target is tar_cue(mode = "always") and
# every other target depends on it. tar_outdated marks an always-cued target
# outdated by construction, and with it every dependent, so a store that a
# complete tar_make had just finished was reported OUTDATED for the package
# target forever, and no gate package could be rendered or approved from any
# store. The staleness wrapper must re-cue EVERY always-cued target (not only
# gate_*) to "thorough", so the question asked is whether the RESULTS are
# current. A genuine change downstream must still be reported.
g2wd <- file.path(wd, "fresh_always"); dir.create(file.path(g2wd, "analyses"), recursive = TRUE)
write_g2 <- function(cmd) writeLines(c(
  "library(targets)",
  "list(",
  "  tar_target(attest, 'fingerprint', cue = tar_cue(mode = 'always')),",
  sprintf("  tar_target(demo_package, %s)", cmd),
  ")"), file.path(g2wd, "analyses", "ALWAYS-001.R"))
write_g2("paste(attest, 1 + 1)")
old_wd <- setwd(g2wd)
callr::r(function() targets::tar_make(script = "analyses/ALWAYS-001.R",
                                       store = "_targets_ALWAYS-001", reporter = "silent"),
         wd = g2wd, show = FALSE)
st2 <- tryCatch(.store_staleness("ALWAYS-001", "demo"), error = function(e) e)
ok(!inherits(st2, "error") && identical(st2$outdated, character(0)),
   "G2: always-cued upstream target does not mark a freshly built store OUTDATED")
write_g2("paste(attest, 1 + 2)")
expect_stop(require_fresh_store("ALWAYS-001", "demo"),
            "G2: changed downstream command -> store OUTDATED refusal still fires", "OUTDATED")
setwd(old_wd)

# ==== H. environment attestation dominates the DAG (round 6 F6, round 7 F10) =
# A targets DAG has no "first target". The composer therefore emits ONE
# always-cued `environment_attestation` target whose value is a stable
# fingerprint of the probed environment, and rewrites EVERY other target -
# stage, package, gate file, gate, method_manifests - to name it. Because the
# value is stable, downstream stays valid while nothing changed and is
# invalidated exactly when the environment changes.
envdom <- tryCatch(callr::r(function(repo) {
  setwd(repo)
  env <- new.env(parent = globalenv())
  pipe <- source("analyses/synthetic_example.R", local = env)$value
  source("pipeline/compose.R", local = env)
  nm <- vapply(pipe, function(t) t$settings$name, character(1))
  deps <- lapply(pipe, function(t) all.names(t$command$expr[[1]]))
  names(deps) <- nm
  att_idx <- which(nm == "environment_attestation")
  others <- setdiff(seq_along(nm), att_idx)
  gate_idx  <- which(grepl("^gate_", nm) & !grepl("_file$", nm))
  stage_idx <- which(!grepl("^gate_", nm) & !nm %in% c("environment_attestation", "method_manifests"))
  cue_mode <- if (length(att_idx) == 1L) pipe[[att_idx]]$cue$mode else NA_character_
  no_env_check <- !"env_check" %in% nm
  dom_ok <- tryCatch(env$verify_gate_dominance(pipe), error = function(e) conditionMessage(e))
  strip_env <- function(t, cue = t$cue) {
    inner <- t$command$expr[[1]]
    keep <- as.list(inner)
    keep <- keep[!vapply(keep, function(x) identical(x, as.name("environment_attestation")), logical(1))]
    targets::tar_target_raw(t$settings$name, as.call(keep), format = t$settings$format, cue = cue)
  }
  pipe_g <- pipe; pipe_g[[gate_idx[1]]] <- strip_env(pipe[[gate_idx[1]]])
  dom_bad_gate <- tryCatch(env$verify_gate_dominance(pipe_g), error = function(e) conditionMessage(e))
  pipe_s <- pipe; pipe_s[[stage_idx[2]]] <- strip_env(pipe[[stage_idx[2]]])
  dom_bad_stage <- tryCatch(env$verify_gate_dominance(pipe_s), error = function(e) conditionMessage(e))
  # the DAG as targets itself sees it
  net <- targets::tar_network(script = "analyses/synthetic_example.R", targets_only = TRUE,
                              reporter = "silent")
  edges <- net$edges
  fed <- unique(edges$to[edges$from == "environment_attestation"])
  list(n_att = length(att_idx), cue_mode = cue_mode, no_env_check = no_env_check,
       all_dep = all(vapply(deps[others], function(d) "environment_attestation" %in% d, logical(1))),
       n_others = length(others), n_gates = length(gate_idx), n_stage = length(stage_idx),
       dom_ok = dom_ok, dom_bad_gate = dom_bad_gate, dom_bad_stage = dom_bad_stage,
       net_missing = setdiff(setdiff(net$vertices$name, "environment_attestation"), fed))
}, args = list(synthetic_project)), error = function(e) e)
if (inherits(envdom, "error")) cat("      H error: ", conditionMessage(envdom), "\n", sep = "")
ok(!inherits(envdom, "error") && envdom$n_att == 1L && identical(envdom$cue_mode, "always"),
   "H: compose emits exactly one always-cued `environment_attestation` target")
ok(!inherits(envdom, "error") && isTRUE(envdom$no_env_check),
   "H: the old env_check target is gone")
ok(!inherits(envdom, "error") && isTRUE(envdom$all_dep),
   sprintf("H: every other target (%s: %s stage, %s gates, files, manifests) names environment_attestation",
           if (inherits(envdom, "error")) "?" else envdom$n_others,
           if (inherits(envdom, "error")) "?" else envdom$n_stage,
           if (inherits(envdom, "error")) "?" else envdom$n_gates))
ok(!inherits(envdom, "error") && length(envdom$net_missing) == 0L,
   sprintf("H: tar_network shows an edge from environment_attestation to every other vertex%s",
           if (!inherits(envdom, "error") && length(envdom$net_missing))
             paste0(" (missing: ", paste(envdom$net_missing, collapse = ", "), ")") else ""))
ok(!inherits(envdom, "error") && isTRUE(envdom$dom_ok),
   "H: verify_gate_dominance passes on the composed pipeline")
ok(!inherits(envdom, "error") && is.character(envdom$dom_bad_gate) &&
     grepl("environment_attestation", envdom$dom_bad_gate),
   "H: verify_gate_dominance refuses a gate target lacking the attestation edge")
ok(!inherits(envdom, "error") && is.character(envdom$dom_bad_stage) &&
     grepl("environment_attestation", envdom$dom_bad_stage),
   "H: verify_gate_dominance refuses a stage target lacking the attestation edge")
setwd(wd)

# ==== I. explicit EDA input source (round 6, finding 4) ====================
# The composer required an external input only when the upstream task was
# ABSENT, while the EDA module takes the external branch whenever
# external_input is present. An analysis could therefore include data_assembly
# and silently have EDA read a different dataset. Declaring both is now a
# composition-time STOP naming both sources; declaring neither still stops.
FIX_BOTH <- file.path(wd, "AMBIG-001.R")
writeLines(c(
  '# Fixture analysis: declares an upstream data_assembly task AND an external',
  '# EDA input. Two sources of truth for the same dataset -> composition STOP.',
  'compose_pipeline(',
  '  analysis_id = "AMBIG-001",',
  '  tasks = c("data_assembly", "eda"),',
  '  inputs = list(',
  '    data_assembly = list(source = "generated/observations.csv"),',
  '    eda = list(external_input = "somewhere/else.csv",',
  '               external_spec  = "somewhere/else.yml")))'), FIX_BOTH)
setwd(REPO)
srcrule <- tryCatch(callr::r(function(repo, fixture) {
  setwd(repo)
  env <- new.env(parent = globalenv())
  source("pipeline/compose.R", local = env)
  both <- tryCatch(source(fixture, local = env)$value,
                   error = function(e) conditionMessage(e))
  neither <- tryCatch(
    env$compose_pipeline("TST-002", c("eda"), inputs = list(eda = list())),
    error = function(e) conditionMessage(e))
  list(both = both, neither = neither)
}, args = list(REPO, FIX_BOTH)), error = function(e) e)
ok(!inherits(srcrule, "error") && is.character(srcrule$both) &&
     grepl("data_assembly", srcrule$both) && grepl("external_input", srcrule$both),
   "I: an analysis declaring BOTH data_assembly and eda external_input is refused, naming both")
ok(!inherits(srcrule, "error") && is.character(srcrule$neither) &&
     grepl("external_input", srcrule$neither),
   "I: an analysis declaring NEITHER source is still refused")
setwd(wd)

# ==== J. approve_gate release (round 6, finding 11) =========================
# Releasing an approved record used to be freehand `sed`: uncheck the box,
# clear Signed/Date/Digest signed, let the renderer rewrite. It failed closed
# but left no event explaining WHY the signed version was released, and the
# previous bytes were lost. release_gate() makes it a ledgered, archived act.
rdir <- file.path(wd, "relrepo"); dir.create(file.path(rdir, "gates", "REL-001"), recursive = TRUE)
setwd(rdir)
writeLines("hello", "a.csv"); writeLines("world", "b.csv")
rrec <- file.path("gates", "REL-001", "eda.md")
writeLines(template_body(id = "REL-001", stage = "eda"), rrec)
render_gate_package(rrec, arts, tables, c("a note"))
writeLines(sign(readLines(rrec)), rrec)
approve_ev <- quiet(approve_gate(rrec, check_store = FALSE))
signed_bytes <- readLines(rrec, warn = FALSE)
old_digest <- .gate_sha256(rrec)

expect_stop(quiet(release_gate(rrec, signer = "Fixture Reviewer")),
            "J: release refuses without a reason", "reason")
expect_stop(quiet(release_gate(rrec, reason = "re-version under canon2")),
            "J: release refuses without a signer", "signer")

rel <- tryCatch(quiet(release_gate(rrec, reason = "re-version under canon2",
                                   signer = "Fixture Reviewer")),
                error = function(e) e)
if (inherits(rel, "error")) cat("      release error: ", conditionMessage(rel), "\n", sep = "")
ok(!inherits(rel, "error") && identical(as.character(rel$decision), "release"),
   "J: release appends a 'release' event")
ok(!inherits(rel, "error") &&
     identical(as.character(rel$previous_event_id), as.character(approve_ev$event_id)),
   "J: the release event chains to the approving event it supersedes")
ok(!inherits(rel, "error") && identical(as.character(rel$record_digest), old_digest),
   "J: the release event binds the OLD record digest")
ok(!inherits(rel, "error") && identical(as.character(rel$signer), "Fixture Reviewer") &&
     grepl("canon2", as.character(rel$reason)),
   "J: the release event carries the signer and the reason")

# The archive holds the exact released bytes, content-addressed.
hist_dir <- file.path("gates", "REL-001", "history")
hist <- list.files(hist_dir, full.names = TRUE)
ok(length(hist) == 1L &&
     identical(readLines(hist[1], warn = FALSE), signed_bytes),
   "J: the exact released markdown bytes are archived under history/")
ok(length(hist) == 1L &&
     identical(basename(hist[1]), paste0("eda.", sub("^sha256:", "", old_digest), ".md")),
   "J: the archive name carries the FULL sha256 of the released record (round 7, finding 17)")

# EXACTLY four lines change: the decision checkbox, Signed, Date, Digest signed.
now_bytes <- readLines(rrec, warn = FALSE)
diff_idx <- which(now_bytes != signed_bytes)
ok(length(now_bytes) == length(signed_bytes) && length(diff_idx) == 4L,
   sprintf("J: exactly four lines change on release (got %d)", length(diff_idx)))
ok(all(grepl("^(- \\[ \\] APPROVED|Signed:|Date:|Digest signed:)", now_bytes[diff_idx])),
   "J: the changed lines are the checkbox, Signed, Date and Digest signed")
ok(isTRUE(verify_approvals_ledger(file.path("gates", "REL-001", "approvals.jsonl"))$ok),
   "J: the ledger still verifies after the release event")
ok(inherits(tryCatch(quiet(require_gate(rrec)), error = function(e) e), "error"),
   "J: the released gate is blocked")

# Byte preservation (round 7, finding 17). The rewrite used to go through
# readLines()/writeLines(), which normalises line endings and appends a final
# newline. A record signed WITHOUT a trailing newline, carrying non-ASCII text,
# must come back with exactly the four cleared lines changed and every other
# byte identical - including the absence of the trailing newline.
brec <- file.path("gates", "REL-001", "simulation.md")
writeLines(template_body(id = "REL-001", stage = "simulation"), brec)
render_gate_package(brec, arts, tables, c("a note with an em dash \u2014 and a caf\u00e9"))
writeLines(sign(readLines(brec)), brec)
sb <- readBin(brec, "raw", file.info(brec)$size)
if (sb[length(sb)] == as.raw(0x0a)) sb <- sb[-length(sb)]     # strip the final newline
writeBin(sb, brec)
quiet(approve_gate(brec, check_store = FALSE))
before <- readBin(brec, "raw", file.info(brec)$size)
relb <- tryCatch(quiet(release_gate(brec, reason = "byte test", signer = "Fixture Reviewer")),
                 error = function(e) e)
after <- readBin(brec, "raw", file.info(brec)$size)
ok(!inherits(relb, "error") && after[length(after)] != as.raw(0x0a),
   "J: release does not append a trailing newline the record did not have")
split_lines <- function(r) strsplit(rawToChar(r), "\n", fixed = TRUE)[[1]]
lb <- split_lines(before); la <- split_lines(after)
dl <- which(lb != la)
ok(length(lb) == length(la) && length(dl) == 4L &&
     all(grepl("^(- \\[ \\] APPROVED|Signed:|Date:|Digest signed:)", la[dl])),
   sprintf("J: exactly the four decision lines differ byte-wise (got %d)", length(dl)))
ok(identical(charToRaw(paste(lb[-dl], collapse = "\n")),
             charToRaw(paste(la[-dl], collapse = "\n"))),
   "J: every byte outside the four cleared lines is identical (non-ASCII preserved)")
ok(length(after) == length(before) - sum(nchar(lb[dl], type = "bytes")) +
     sum(nchar(la[dl], type = "bytes")),
   "J: the byte delta equals the sum of the four line-content deltas")
# CRLF records keep their line endings.
crec <- file.path("gates", "REL-001", "eda2.md")
writeLines(template_body(id = "REL-001", stage = "eda2"), crec)
render_gate_package(crec, arts, tables, c("crlf note"))
writeLines(sign(readLines(crec)), crec)
ctxt <- gsub("\n", "\r\n", rawToChar(readBin(crec, "raw", file.info(crec)$size)), fixed = TRUE)
writeBin(charToRaw(ctxt), crec)
quiet(approve_gate(crec, check_store = FALSE))
relc <- tryCatch(quiet(release_gate(crec, reason = "crlf", signer = "Fixture Reviewer")), error = function(e) e)
cafter <- rawToChar(readBin(crec, "raw", file.info(crec)$size))
ok(!inherits(relc, "error") && !grepl("[^\r]\n", cafter) && grepl("\r\n", cafter, fixed = TRUE),
   "J: a CRLF record is rewritten with CRLF line endings only")

# Strict CLI flags (round 7, finding 17): duplicate or unknown flags refuse
# with the usage exit code BEFORE anything is touched.
cli <- function(...) {
  suppressWarnings(system2("Rscript", c(file.path(REPO, "tools", "approve_gate.R"), ...),
                           stdout = TRUE, stderr = TRUE))
}
st <- function(out) { s <- attr(out, "status"); if (is.null(s)) 0L else as.integer(s) }
ok(st(cli("release", "REL-001", "eda", "--reason", "x", "--reason", "y", "--signer", "Fixture Reviewer")) == 2L,
   "J: CLI refuses a duplicated --reason flag with the usage status")
ok(st(cli("release", "REL-001", "eda", "--reason", "x", "--signer", "Fixture Reviewer", "--force")) == 2L,
   "J: CLI refuses an unknown flag with the usage status")
ok(st(cli("release", "REL-001", "eda", "--reason", "--signer", "Fixture Reviewer")) == 2L,
   "J: CLI refuses a flag whose value is another flag")
ok(st(cli("release", "REL-001", "eda", "--reason", "x", "--signer", "Fixture Reviewer", "extra")) == 2L,
   "J: CLI refuses a stray positional argument")

# Releasing an already-unsigned record is refused.
expect_stop(quiet(release_gate(rrec, reason = "again", signer = "Fixture Reviewer")),
            "J: release refuses a record that is not currently APPROVED",
            "not currently approved|APPROVED")

# Record bytes that no longer match the latest ledger event are refused: the
# release must bind the exact version the SME signed.
# re-signed with different bytes, and NOT re-ledgered
writeLines(sign(readLines(rrec), date_line = "Date: 2026-09-03"), rrec)
expect_stop(quiet(release_gate(rrec, reason = "drifted", signer = "Fixture Reviewer")),
            "J: release refuses when the record digest does not match the ledger",
            "digest")

# ==== J2. release grammar and atomicity ===
# The strict parser accepts a pandoc-normalised decision box ("-   [x]"), so
# approve_gate() ledgered such a record; release_gate() then archived the bytes
# and appended the release event before its own rewrite refused the box, and
# the record kept its signature while the ledger said "released". Two rules:
# the rewrite uses the parser's box grammar, and nothing is written until the
# cleared bytes have been computed and asserted.
prec <- file.path("gates", "REL-001", "pandoc.md")
writeLines(template_body(id = "REL-001", stage = "pandoc"), prec)
render_gate_package(prec, arts, tables, c("pandoc-spaced box"))
pl <- sign(readLines(prec))
pl <- sub("^- \\[x\\] ", "-   [x] ", pl)
writeLines(pl, prec)
quiet(approve_gate(prec, check_store = FALSE))
relp <- tryCatch(quiet(release_gate(prec, reason = "pandoc box", signer = "Fixture Reviewer")),
                 error = function(e) e)
pafter <- readLines(prec)
ok(!inherits(relp, "error") && any(grepl("^-   \\[ \\] APPROVED", pafter)) &&
     !any(grepl("^Signed: Fixture Reviewer", pafter)),
   "J2: a record whose checked box carries pandoc spacing is released and cleared")

# Atomicity: a record the rewrite must refuse (mixed CRLF/LF terminators, which
# no single byte-faithful rewrite can reproduce) leaves no archive, no ledger
# event and no record change behind.
mrec <- file.path("gates", "REL-001", "mixed.md")
writeLines(template_body(id = "REL-001", stage = "mixed"), mrec)
render_gate_package(mrec, arts, tables, c("mixed terminators"))
writeLines(sign(readLines(mrec)), mrec)
mtxt <- rawToChar(readBin(mrec, "raw", file.info(mrec)$size))
mtxt <- sub("\n", "\r\n", mtxt, fixed = TRUE)                # exactly one CRLF line
writeBin(charToRaw(mtxt), mrec)
quiet(approve_gate(mrec, check_store = FALSE))
ledger_before <- readLines(file.path("gates", "REL-001", "approvals.jsonl"))
mbytes_before <- readBin(mrec, "raw", file.info(mrec)$size)
archives_before <- list.files(file.path("gates", "REL-001", "history"))
expect_stop(quiet(release_gate(mrec, reason = "mixed", signer = "Fixture Reviewer")),
            "J2: release refuses a record it cannot rewrite byte-faithfully",
            "CRLF|refusing")
ok(identical(readLines(file.path("gates", "REL-001", "approvals.jsonl")), ledger_before),
   "J2: a refused release appends no ledger event")
ok(identical(list.files(file.path("gates", "REL-001", "history")), archives_before),
   "J2: a refused release writes no archive")
ok(identical(readBin(mrec, "raw", file.info(mrec)$size), mbytes_before),
   "J2: a refused release leaves the record bytes untouched")

# ==== J3. release durability (round 9, P2-8; revised after review S1/S2) ====
# The archive write, the ledger append and the record rewrite were three
# sequential side effects: a failure after the first left an archive behind,
# and a failure after the second left a ledger saying "released" over a record
# still carrying its signature. Archive and record are now staged and renamed;
# the ledger keeps its O_APPEND write and is never rewritten.
arec <- file.path("gates", "REL-001", "atomic.md")
writeLines(template_body(id = "REL-001", stage = "atomic"), arec)
render_gate_package(arec, arts, tables, c("atomic release"))
writeLines(sign(readLines(arec)), arec)
quiet(approve_gate(arec, check_store = FALSE))
a_hist            <- file.path("gates", "REL-001", "history")
a_ledger_path     <- file.path("gates", "REL-001", "approvals.jsonl")
dir.create(a_hist, recursive = TRUE, showWarnings = FALSE)
a_ledger_before   <- readLines(a_ledger_path)
a_bytes_before    <- readBin(arec, "raw", file.info(arec)$size)
a_archives_before <- list.files(a_hist)
a_env  <- environment(release_gate)

# --- (a) failure BEFORE the ledger append leaves nothing behind -------------
a_real_json <- get(".ledger_to_json", envir = a_env)
assign(".ledger_to_json",
       function(ev) stop("injected serialisation failure", call. = FALSE),
       envir = a_env)
a_rel <- tryCatch(quiet(release_gate(arec, reason = "atomicity", signer = "Fixture Reviewer")),
                  error = function(e) e)
assign(".ledger_to_json", a_real_json, envir = a_env)
ok(inherits(a_rel, "error"),
   "J3: a release that fails before the ledger append refuses")
ok(identical(list.files(a_hist), a_archives_before),
   "J3: a pre-append failure leaves NO archive behind")
ok(identical(readLines(a_ledger_path), a_ledger_before),
   "J3: a pre-append failure appends no ledger event")
ok(identical(readBin(arec, "raw", file.info(arec)$size), a_bytes_before),
   "J3: a pre-append failure leaves the record signed")
ok(!length(list.files(dirname(arec), pattern = "release-staging")),
   "J3: a pre-append failure leaves no staging file behind")

# --- (b) the rename window: the append succeeded, a rename then fails -------
# This is the window the previous test claimed to cover and did not. The
# record's rename is forced to fail by replacing file.rename in release_gate's
# environment for the record destination only.
a_real_rename <- base::file.rename
assign("file.rename", function(from, to) {
  if (grepl("atomic\\.md$", to)) return(FALSE)
  a_real_rename(from, to)
}, envir = a_env)
w_ledger_before   <- readLines(a_ledger_path)
w_archives_before <- list.files(a_hist)
w_rel <- tryCatch(quiet(release_gate(arec, reason = "rename window", signer = "Fixture Reviewer")),
                  error = function(e) e)
rm("file.rename", envir = a_env)
w_ledger_after   <- readLines(a_ledger_path)
w_archives_after <- list.files(a_hist)
w_staged <- list.files(dirname(arec), pattern = "release-staging", full.names = TRUE)
ok(inherits(w_rel, "error") && grepl("IS recorded", conditionMessage(w_rel)),
   "J3: a rename that fails after the append reports that the event IS recorded")
ok(inherits(w_rel, "error") && grepl("LEFT ON DISK", conditionMessage(w_rel)) &&
     grepl("release-staging", conditionMessage(w_rel)),
   "J3: the failure names the staged files it left on disk")
ok(length(w_staged) >= 1L,
   "J3: the staged bytes are NOT deleted after the append (the evidence survives)")
ok(length(w_archives_after) == length(w_archives_before) + 1L,
   "J3: the archive of the released bytes is in place after the append")
ok(length(w_ledger_after) == length(w_ledger_before) + 1L,
   "J3: exactly one release event was appended")
ok(identical(readBin(arec, "raw", file.info(arec)$size), a_bytes_before),
   "J3: the record whose rename failed still holds its original bytes")
unlink(w_staged)

# --- (c) a concurrent ledger append is preserved, or the release refuses ----
# The ledger is append-only. Rewriting it from an in-memory copy destroyed any
# line another process appended while the release was being staged; the append
# is now O_APPEND and the ledger is re-read before it.
crec3 <- file.path("gates", "REL-001", "concurrent.md")
writeLines(template_body(id = "REL-001", stage = "concurrent"), crec3)
render_gate_package(crec3, arts, tables, c("concurrent append"))
writeLines(sign(readLines(crec3)), crec3)
quiet(approve_gate(crec3, check_store = FALSE))
c_before <- readLines(a_ledger_path)
c_line   <- '{"schema":"pmx-gate-ledger/1","note":"concurrent writer"}'
c_real_json <- get(".ledger_to_json", envir = a_env)
assign(".ledger_to_json", function(ev) {
  cat(c_line, "\n", sep = "", file = a_ledger_path, append = TRUE)  # another writer
  c_real_json(ev)
}, envir = a_env)
c_rel <- tryCatch(quiet(release_gate(crec3, reason = "concurrent", signer = "Fixture Reviewer")),
                  error = function(e) e)
assign(".ledger_to_json", c_real_json, envir = a_env)
c_after <- readLines(a_ledger_path)
ok(any(c_after == c_line),
   "J3: a line appended by another writer during the release SURVIVES")
ok(length(c_after) == length(c_before) + 2L ||
     (inherits(c_rel, "error") && length(c_after) == length(c_before) + 1L),
   "J3: the release either appends alongside the concurrent line or refuses; it never overwrites")

# --- (d) two releasers cannot both enter the window (batch 11, R10-P1-1) ----
# Two releasers could both pass the tail comparison and then both append,
# producing two release events sharing one predecessor. Releases against one
# ledger are now serialised by an exclusive lock held from the first tail read
# through the append and both renames. The lock is taken here directly, which
# is exactly what a release already inside the window holds.
lrec <- file.path("gates", "REL-001", "locked.md")
writeLines(template_body(id = "REL-001", stage = "locked"), lrec)
render_gate_package(lrec, arts, tables, c("lock contention"))
writeLines(sign(readLines(lrec)), lrec)
quiet(approve_gate(lrec, check_store = FALSE))
l_ledger_before <- readLines(a_ledger_path)
l_bytes_before  <- readBin(lrec, "raw", file.info(lrec)$size)
held <- .release_lock_acquire(a_ledger_path)
ok(dir.exists(held) && file.exists(file.path(held, "holder")),
   "J3: the release lock is an atomically created directory recording its holder")
l_rel <- tryCatch(quiet(release_gate(lrec, reason = "second releaser", signer = "Fixture Reviewer")),
                  error = function(e) e)
ok(inherits(l_rel, "error") &&
     grepl("is holding the lock", conditionMessage(l_rel)),
   "J3: a second releaser is REFUSED while the lock is held")
ok(inherits(l_rel, "error") && grepl("holder", conditionMessage(l_rel)) &&
     grepl("age", conditionMessage(l_rel)),
   "J3: the lock refusal names the lock's holder and its age")
ok(identical(readLines(a_ledger_path), l_ledger_before),
   "J3: the refused releaser appended NO ledger event")
ok(identical(readBin(lrec, "raw", file.info(lrec)$size), l_bytes_before),
   "J3: the refused releaser left the record signed and unchanged")
ok(!length(list.files(dirname(lrec), pattern = "release-staging")),
   "J3: the refused releaser staged nothing")
# --- (e) approve and revoke honour the same lock (batch 12, R11-P1-1) --------
# approve_gate() and revoke_gate() used to read the ledger and append with no
# lock at all, so either could slip a same-task event between a release's
# tail comparison and its append and fork the chain. All three writers now
# take the same per-ledger lock before their first ledger read.
xrec <- file.path("gates", "REL-001", "xlock.md")
writeLines(template_body(id = "REL-001", stage = "xlock"), xrec)
render_gate_package(xrec, arts, tables, c("approve under lock"))
writeLines(sign(readLines(xrec)), xrec)
x_ledger_before <- readLines(a_ledger_path)
x_app <- tryCatch(quiet(approve_gate(xrec, check_store = FALSE)), error = function(e) e)
ok(inherits(x_app, "error") && grepl("holding the lock", conditionMessage(x_app)) &&
     grepl("holder", conditionMessage(x_app)),
   "J3: approve_gate() is REFUSED while the ledger lock is held, naming the holder")
x_rev <- tryCatch(quiet(revoke_gate(lrec, reason = "under lock")), error = function(e) e)
ok(inherits(x_rev, "error") && grepl("holding the lock", conditionMessage(x_rev)) &&
     grepl("holder", conditionMessage(x_rev)),
   "J3: revoke_gate() is REFUSED while the ledger lock is held, naming the holder")
ok(identical(readLines(a_ledger_path), x_ledger_before),
   "J3: neither refused writer appended a ledger event")
.release_lock_free(held)
ok(!dir.exists(held), "J3: releasing the lock removes it")
x_app2 <- tryCatch(quiet(approve_gate(xrec, check_store = FALSE)), error = function(e) e)
ok(!inherits(x_app2, "error"), "J3: with the lock free approve_gate() succeeds")
x_rev2 <- tryCatch(quiet(revoke_gate(xrec, reason = "serialised after approval")),
                   error = function(e) e)
ok(!inherits(x_rev2, "error"), "J3: with the lock free revoke_gate() succeeds after the approval")
x_ev <- Filter(function(e) identical(as.character(e$task), "xlock"), .read_ledger(a_ledger_path))
ok(length(x_ev) == 2L && identical(as.character(x_ev[[1]]$decision), "approved") &&
     identical(as.character(x_ev[[2]]$decision), "revoked") &&
     identical(as.character(x_ev[[2]]$previous_event_id), as.character(x_ev[[1]]$event_id)) &&
     length(.verify_task_chain(x_ev)) == 0L,
   "J3: approve then revoke chain correctly when serialised through the lock")
ok(!dir.exists(.release_lock_path(a_ledger_path)),
   "J3: approve and revoke leave no lock behind")

# With the lock free, the same release now succeeds -- the refusal was the
# lock, not the record.
l_rel2 <- tryCatch(quiet(release_gate(lrec, reason = "second releaser", signer = "Fixture Reviewer")),
                   error = function(e) e)
ok(!inherits(l_rel2, "error"),
   "J3: with the lock free the same release succeeds (the lock was the only obstacle)")
ok(!dir.exists(.release_lock_path(a_ledger_path)),
   "J3: a successful release does not leave its lock behind")

# A stale lock is REPORTED as possibly stale and is never broken automatically.
held2 <- .release_lock_acquire(a_ledger_path)
Sys.setFileTime(held2, Sys.time() - (GATE_RELEASE_LOCK_STALE_SECONDS + 60))
s_rel <- tryCatch(.release_lock_acquire(a_ledger_path), error = function(e) e)
ok(inherits(s_rel, "error") && grepl("staleness threshold", conditionMessage(s_rel)),
   "J3: a lock older than the staleness threshold is reported as possibly stale")
ok(inherits(s_rel, "error") && grepl("remove the lock", conditionMessage(s_rel)) &&
     dir.exists(held2),
   "J3: a stale lock is NOT broken automatically; the refusal tells the SME to remove it")
.release_lock_free(held2)

# canon2 self-recompute runs on the release path. verify_artifacts = FALSE is
# meant to skip ONLY the live-artifact rehash, but it used to disable the whole
# content check, so a record whose generated block had been edited after
# signature could still be released as if it were the signed package.
crec2 <- file.path("gates", "REL-001", "canon2.md")
writeLines(template_body(id = "REL-001", stage = "canon2"), crec2)
render_gate_package(crec2, arts, tables, c("canon2 always"))
writeLines(sign(readLines(crec2)), crec2)
quiet(approve_gate(crec2, check_store = FALSE))
cl2 <- readLines(crec2)
note_i <- grep("canon2 always", cl2)[1]
cl2[note_i] <- sub("canon2 always", "canon2 tampered", cl2[note_i])
writeLines(cl2, crec2)
expect_stop(quiet(release_gate(crec2, reason = "tampered", signer = "Fixture Reviewer")),
            "J3: release refuses a record whose canon2 digest no longer recomputes",
            "canon2")
setwd(wd)

# Keep the release diagnostic and the public operator instructions aligned.
# The adapter's install battery exercises the same generated instructions.
resid <- .release_named_residual_window()
for (doc in c("docs/COPILOT-RUNBOOK.md",
              "copilot-adapter/profile/instructions/gates.md",
              ".github/instructions/gates.instructions.md")) {
  p <- file.path(REPO, doc)
  txt <- if (file.exists(p))
    paste(readLines(p, warn = FALSE), collapse = " ") else ""
  txt <- gsub("[[:space:]]+", " ", txt)
  ok(grepl(resid, txt, fixed = TRUE),
     sprintf("J3: %s carries the residual-window sentence, identical modulo whitespace", doc))
}

# ==== K. release while the signed artifacts have drifted ====================
# Regression: release_gate() ran the full
# require_gate() content check, which rehashes the live artifacts against the
# signed digests. Drift is exactly WHY a release happens (the SME undoes the
# signature so the package can be re-rendered), and render_gate_from_store
# refuses to touch an APPROVED record, so a drifted approved record could be
# neither released nor re-rendered. The release must keep every identity and
# integrity check (parse, decision, digest binding, ledger chain, archive) but
# must not fail on live-artifact drift; the default require_gate() still does.
kdir <- file.path(wd, "driftrepo"); dir.create(file.path(kdir, "gates", "DRF-001"), recursive = TRUE)
setwd(kdir)
writeLines("hello", "a.csv"); writeLines("world", "b.csv")
krec <- file.path("gates", "DRF-001", "eda.md")
writeLines(template_body(id = "DRF-001", stage = "eda"), krec)
render_gate_package(krec, arts, tables, c("a note"))
writeLines(sign(readLines(krec)), krec)
k_approve <- quiet(approve_gate(krec, check_store = FALSE))
k_signed_bytes <- readLines(krec, warn = FALSE)
k_old_digest <- .gate_sha256(krec)
writeLines("hello, drifted", "a.csv")           # artifact bytes drift after signature

expect_stop(quiet(require_gate(krec)),
            "K: default require_gate refuses the drifted approved record", "drifted")
krel <- tryCatch(quiet(release_gate(krec, reason = "artifacts drifted; re-render",
                                    signer = "Fixture Reviewer")),
                 error = function(e) e)
if (inherits(krel, "error")) cat("      release error: ", conditionMessage(krel), "\n", sep = "")
ok(!inherits(krel, "error") && identical(as.character(krel$decision), "release"),
   "K: release succeeds while the signed artifacts have drifted")
ok(!inherits(krel, "error") &&
     identical(as.character(krel$previous_event_id), as.character(k_approve$event_id)) &&
     identical(as.character(krel$record_digest), k_old_digest),
   "K: the drift release chains to the approval and binds the old record digest")
k_hist <- list.files(file.path("gates", "DRF-001", "history"), full.names = TRUE)
ok(length(k_hist) == 1L && identical(readLines(k_hist[1], warn = FALSE), k_signed_bytes),
   "K: the exact released bytes are archived under history/")
k_now <- readLines(krec, warn = FALSE)
k_diff <- which(k_now != k_signed_bytes)
ok(length(k_now) == length(k_signed_bytes) && length(k_diff) == 4L &&
     all(grepl("^(- \\[ \\] APPROVED|Signed:|Date:|Digest signed:)", k_now[k_diff])),
   "K: exactly the four decision fields are cleared")
ok(isTRUE(verify_approvals_ledger(file.path("gates", "DRF-001", "approvals.jsonl"))$ok),
   "K: the ledger chain verifies after the drift release")
expect_stop(quiet(require_gate(krec)),
            "K: default require_gate still refuses the released (now unsigned) record", "not signed")
# The archived signed copy, placed back beside its drifted artifacts, is still
# refused by the default require_gate: the release relaxed nothing for consumers.
k_copy <- file.path("gates", "DRF-001", "eda_signed_copy.md")
file.copy(k_hist[1], k_copy)
expect_stop(quiet(require_gate(k_copy)),
            "K: default require_gate still refuses the archived signed record against drifted artifacts",
            "drifted")
setwd(wd)

# ---- L: render_gate_from_store resolves the record path by the compose rule ----
# pipeline/compose.R gate_file_for() dashes underscores in the task name
# (data_assembly -> gates/<ID>/data-assembly.md). The render default used the
# raw task name and so looked for data_assembly.md, which surfaced on the first
# comparison. Task names without underscores also retain their spelling.
l_default <- eval(formals(render_gate_from_store)$gate_file,
                  list(analysis_id = "SYNTHETIC-001", task = "data_assembly"))
ok(identical(l_default, file.path("gates", "SYNTHETIC-001", "data-assembly.md")),
   "L: render_gate_from_store default record path dashes underscores like compose.R")
ok(identical(eval(formals(render_gate_from_store)$gate_file,
                  list(analysis_id = "SYNTHETIC-001", task = "eda")),
             file.path("gates", "SYNTHETIC-001", "eda.md")),
   "L: render_gate_from_store default record path unchanged for dash-free tasks")

# ---- M: artifact cells with several files or a directory are digest-bound ----
# SME decision 2026-09-04: a "; "-joined cell
# or a directory cell used to render MISSING (and require_gate refuses any
# MISSING row), which deadlocked every task module that groups related files.
m_dir <- file.path(tempdir(), paste0("m-art-", as.integer(Sys.time())))
dir.create(file.path(m_dir, "sub"), recursive = TRUE)
writeLines("alpha", file.path(m_dir, "a.txt"))
writeLines("beta",  file.path(m_dir, "b.txt"))
writeLines("gamma", file.path(m_dir, "sub", "c.txt"))
m_wd <- setwd(m_dir)
m_single <- .gate_artifact_cell_sha256("a.txt")
ok(identical(m_single, paste0("sha256:", digest::digest(file = "a.txt", algo = "sha256"))),
   "M: a single-file cell digests the file bytes exactly as before")
m_multi <- .gate_artifact_cell_sha256("a.txt; b.txt")
ok(grepl("^sha256:[0-9a-f]{64}$", m_multi) && !identical(m_multi, m_single),
   "M: a '; '-joined cell renders one well-formed composite digest")
ok(!identical(m_multi, .gate_artifact_cell_sha256("b.txt; a.txt")),
   "M: the composite digest binds member order")
m_dircell <- .gate_artifact_cell_sha256(".")
ok(grepl("^sha256:[0-9a-f]{64}$", m_dircell),
   "M: a directory cell digests its regular files recursively")
ok(identical(.gate_artifact_cell_sha256("."), m_dircell),
   "M: the directory digest is reproducible")
writeLines("beta changed", file.path(m_dir, "b.txt"))
ok(!identical(.gate_artifact_cell_sha256("a.txt; b.txt"), m_multi),
   "M: one drifted member changes the composite digest")
ok(!identical(.gate_artifact_cell_sha256("."), m_dircell),
   "M: one drifted member changes the directory digest")
ok(identical(.gate_artifact_cell_sha256("a.txt; missing.txt"), "MISSING"),
   "M: one absent member makes the whole cell MISSING")
dir.create(file.path(m_dir, "empty"))
ok(identical(.gate_artifact_cell_sha256("empty"), "MISSING"),
   "M: an empty directory is MISSING")
ok(identical(.gate_artifact_sha256("a.txt; b.txt"), .gate_rehash_one("a.txt; b.txt")),
   "M: render-side and verify-side digests are the same function")

# Round 9, P0-2. A directory cell used to expand with all.files = FALSE, so a
# hidden member was outside the signature entirely; and a directory holding
# exactly one file fell through the single-file branch, which digests bytes
# only, so renaming the sole member left the signed digest unchanged. A cell
# that names a directory now ALWAYS digests a member manifest, whatever the
# member count, and hidden files are members.
m_hid <- file.path(tempdir(), paste0("m-hid-", as.integer(Sys.time())))
dir.create(m_hid, recursive = TRUE)
h_wd <- setwd(m_hid)
dir.create("d")
writeLines("visible", file.path("d", "v.txt"))
writeLines("secret",  file.path("d", ".hidden"))
h_before <- .gate_artifact_cell_sha256("d")
writeLines("secret changed", file.path("d", ".hidden"))
ok(!identical(.gate_artifact_cell_sha256("d"), h_before),
   "M: a changed hidden member changes the directory digest")

dir.create("one")
writeLines("only", file.path("one", "a.txt"))
o_before <- .gate_artifact_cell_sha256("one")
ok(!identical(o_before, paste0("sha256:", digest::digest(file = file.path("one", "a.txt"),
                                                         algo = "sha256"))),
   "M: a one-member directory digests a manifest, not the member's bytes")
file.rename(file.path("one", "a.txt"), file.path("one", "b.txt"))
ok(!identical(.gate_artifact_cell_sha256("one"), o_before),
   "M: renaming the sole member of a directory changes the digest")

writeLines("dup", "dup.txt")
ok(identical(.gate_artifact_cell_sha256("dup.txt; dup.txt"), "MISSING"),
   "M: a duplicated member path makes the cell MISSING")
ok(identical(.gate_artifact_cell_sha256("dup.txt; ./dup.txt"), "MISSING"),
   "M: the same file named two ways is still a duplicate member")
setwd(h_wd)
unlink(m_hid, recursive = TRUE)
setwd(m_wd)
unlink(m_dir, recursive = TRUE)

# ---- N: the release CLI resolves an underscored task name to the dashed record ----
n_src <- readLines(file.path(REPO, "tools", "approve_gate.R"))
n_line <- grep('gf <- file.path("gates", args[2],', n_src, fixed = TRUE)
ok(length(n_line) == 1L && any(grepl('gsub("_", "-"', n_src[n_line + 0:1], fixed = TRUE)),
   "N: approve_gate.R release CLI dashes underscores in <task> like compose.R gate_file_for")

cat(sprintf("batch4 battery: %d passed, %d failed\n", .pass, .fail))
quit(status = if (.fail > 0L) 1L else 0L)

}  # end .run_batch4_tests

if (.invoked_as_script()) .run_batch4_tests()
