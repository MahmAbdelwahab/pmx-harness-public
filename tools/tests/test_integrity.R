#!/usr/bin/env Rscript
# Integrity batch 2 test battery (digest binding + method manifests).
#
# Exercises, with hermetic fixtures in a tempdir, the semantics added in
# integrity batch 2:
#   Part A  gate_package_digest() determinism; render_gate_package() adds an
#           artifact sha256 column and a Package-Digest line; render refuses to
#           overwrite an APPROVED record; require_gate() binds the SME signature
#           to the package digest and grandfathers pre-digest signed records.
#   Part B  write_method_manifest() emits per-reference digests;
#           require_validated_method() enforces approver + file digest, treats
#           every unit of an unconfirmed manifest (empty reviewed_by) as draft
#           (the legacy-signature fallback was retired in batch 3c), and
#           hard-stops on a missing manifest or a digest mismatch of an
#           APPROVED manifest (tampered method file).
#
# No real repo file is touched: every fixture lives under tempdir(). Run with
#   Rscript tools/tests/test_integrity.R
# Exits non-zero if any assertion fails.

# SAFE TO SOURCE: pipelines call tar_source("tools"), which recurses into
# tools/tests/ and sources every .R here. This file must therefore do NOTHING
# observable when merely sourced — all execution is guarded behind
# .invoked_as_script(), exactly as tools/ci_check.R is. Running it as
# `Rscript tools/tests/test_integrity.R` sets the flag and runs the battery.
.invoked_as_script <- function() {
  fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  length(fa) > 0 && grepl("test_integrity\\.R$", sub("^--file=", "", fa[1]))
}

.run_integrity_tests <- function() {

suppressWarnings(suppressMessages({
  # tools/ is two levels up from tools/tests/ .
  args <- commandArgs(FALSE)
  file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
  TOOLS <- normalizePath(file.path(dirname(file_arg), ".."), mustWork = TRUE)
  source(file.path(TOOLS, "gate_package.R"))
  source(file.path(TOOLS, "gate.R"))
  source(file.path(TOOLS, "method_manifest.R"))
  source(file.path(TOOLS, "approve_gate.R"))
}))

.pass <- 0L; .fail <- 0L
ok <- function(cond, name) {
  if (isTRUE(cond)) { .pass <<- .pass + 1L; cat(sprintf("PASS  %s\n", name)) }
  else { .fail <<- .fail + 1L; cat(sprintf("FAIL  %s\n", name)) }
}
expect_stop <- function(expr, name, regexp = NULL) {
  e <- tryCatch({ force(expr); NULL }, error = function(e) e)
  hit <- !is.null(e) && (is.null(regexp) || grepl(regexp, conditionMessage(e)))
  ok(hit, name)
}
`%+%` <- function(a, b) paste0(a, b)

# --- fixtures ---------------------------------------------------------------
artifacts <- data.frame(
  artifact = c("table one", "figure two"),
  path = c("a.csv", "missing.pdf"),
  source = c("target_a", "target_b"),
  stringsAsFactors = FALSE
)
tables <- list("Verdicts" = data.frame(criterion = "E1", value = 1.23,
                                        stringsAsFactors = FALSE))
notes <- c("a note line")

# ==== Part A: canonical digest ==============================================
d1 <- gate_package_digest(artifacts, tables, notes)
d2 <- gate_package_digest(artifacts, tables, notes)
ok(identical(d1, d2), "digest deterministic over identical objects")
ok(grepl("^sha256:[0-9a-f]{64}$", d1), "digest carries sha256:<hex64> shape")
art2 <- artifacts; art2$source[1] <- "target_z"
ok(!identical(d1, gate_package_digest(art2, tables, notes)),
   "digest changes when an artifact field changes")

# ==== Part A: render adds sha256 column + Package-Digest =====================
wd <- tempfile("pkgdir"); dir.create(wd)
writeLines("hello", file.path(wd, "a.csv"))
old <- setwd(wd)
# Batch 4: render refuses a record without clean identity lines, and an
# approving signature needs a date, so fixtures carry both. Records live under
# gates/TST-001/<stem>.md so the strict parser's directory / file-stem checks
# hold; `hdr(stem)` writes the identity + decision skeleton.
gdir <- file.path(wd, "gates", "TST-001"); dir.create(gdir, recursive = TRUE)
hdr <- function(stem) c("# rec", "", "Analysis ID: TST-001", paste0("Stage: ", stem), "",
                        "## SME decision", "",
                        "- [ ] APPROVED — stage complete",
                        "Digest signed: <!-- paste -->", "Signed: <!-- -->",
                        "Date: <!-- -->")
# A rendered-and-signed body is re-stemmed for a sibling record: the Stage line
# and the digested Envelope line must both name the new stem, so the block is
# re-rendered from scratch rather than string-edited.
rec <- file.path(gdir, "rec.md")
writeLines(hdr("rec"), rec)
render_gate_package(rec, artifacts, tables, notes)
lines <- readLines(rec)
ok(any(grepl("^\\| sha256 \\|", lines)) ||
   any(grepl("sha256", lines[grep("### Artifacts", lines) + 2])),
   "artifacts table gained a sha256 column")
ok(any(grepl("MISSING", lines)), "missing artifact file renders as MISSING")
# Integrity batch 3b, item A2: the digest is now canon2 (a self-verifiable
# hash over the rendered block bytes), carrying the "canon2:sha256:" prefix.
ok(any(grepl("^Package-Digest: canon2:sha256:[0-9a-f]{64}$", lines)),
   "generated block ends with a canon2 Package-Digest line")

# ==== Part A: render refuses to overwrite an APPROVED record =================
approved_rec <- file.path(gdir, "approved.md")
writeLines(c("# rec", "", "Analysis ID: TST-001", "Stage: approved", "",
             GATE_BLOCK_START, "old", GATE_BLOCK_END, "",
             "## SME decision", "",
             "- [x] APPROVED — stage complete", "Signed: Fixture Reviewer", "Date: 2026-09-02"), approved_rec)
expect_stop(render_gate_package(approved_rec, artifacts, tables, notes),
            "render refuses to rewrite an APPROVED record", "APPROVED|new record")

# ==== Part A: require_gate digest binding ===================================
# The require_gate LIVE ARTIFACT REHASH (batch 3b, item A1) refuses a record
# whose artifacts table carries a MISSING row, so the binding fixtures below use
# an ALL-PRESENT artifacts set (both files exist on disk) rendered into rec_ok.
writeLines("world", file.path(wd, "b.csv"))
artifacts_ok <- data.frame(
  artifact = c("table one", "table two"),
  path = c("a.csv", "b.csv"),
  source = c("target_a", "target_b"),
  stringsAsFactors = FALSE)
# Every signed fixture below is a distinct record; each is rendered under its
# own stem so identity + envelope stay consistent.
render_signed <- function(stem, arts, sign = TRUE) {
  f <- file.path(gdir, paste0(stem, ".md"))
  writeLines(hdr(stem), f)
  render_gate_package(f, arts, tables, notes)
  b <- readLines(f)
  if (sign) {
    dg <- sub("^Package-Digest: ", "", grep("^Package-Digest:", b, value = TRUE)[1])
    b[grep("^- \\[ \\] APPROVED", b)] <- "- [x] APPROVED — stage complete"
    b[grep("^Digest signed:", b)] <- paste("Digest signed:", dg)
    b[grep("^Signed:", b)] <- "Signed: Fixture Reviewer"
    b[grep("^Date:", b)] <- "Date: 2026-09-02"
    writeLines(b, f)
  }
  f
}
rec_ok <- file.path(gdir, "rec_ok.md")
writeLines(hdr("rec_ok"), rec_ok)
render_gate_package(rec_ok, artifacts_ok, tables, notes)
lines <- readLines(rec_ok)

pkg_digest <- sub("^Package-Digest: ", "",
                  grep("^Package-Digest:", lines, value = TRUE)[1])
ok(grepl("^canon2:sha256:[0-9a-f]{64}$", pkg_digest),
   "rendered digest is canon2 (self-verifiable block hash)")
# a signed record whose Digest-signed matches the block digest AND whose
# artifacts all hash to their recorded value -> pass
signed_ok <- render_signed("signed_ok", artifacts_ok)
body <- readLines(signed_ok)
ok(!is.null(tryCatch(suppressMessages(require_gate(signed_ok)), error = function(e) NULL)),
   "require_gate passes signed record whose digest binding + rehash match")

# ==== Part A: LIVE ARTIFACT REHASH (batch 3b, item A1) ======================
# drift one artifact file on disk after signature -> require_gate refuses,
# naming the drifted file.
drift_rec <- render_signed("drift", artifacts_ok)
saved_b <- readLines(file.path(wd, "b.csv"))
writeLines("world-EDITED", file.path(wd, "b.csv"))
expect_stop(require_gate(drift_rec),
            "require_gate refuses when a listed artifact drifted on disk",
            "drifted|content verification")
writeLines(saved_b, file.path(wd, "b.csv"))   # restore for later fixtures

# a MISSING artifact row is a refusal (the render fixture with missing.pdf)
missing_rec <- render_signed("missing_signed", artifacts)   # missing.pdf artifact
expect_stop(require_gate(missing_rec),
            "require_gate refuses a signed record with a MISSING artifact row",
            "MISSING|content verification")

# ==== Part A: canon2 self-recompute (batch 3b, item A2) =====================
# hand-edit a byte inside the generated block (a table value) without touching
# the digest line -> canon2 recompute no longer matches -> refuse.
canon2_rec <- render_signed("canon2_edit", artifacts_ok)
ebody <- readLines(canon2_rec)
# flip a visible cell in the Verdicts table (value 1.23 -> 9.99); the digest
# line is untouched, so only the recompute can catch it.
ebody <- sub("1.23", "9.99", ebody, fixed = TRUE)
writeLines(ebody, canon2_rec)
expect_stop(require_gate(canon2_rec),
            "require_gate refuses a hand-edited block (canon2 recompute)",
            "canon2|content verification")

# tamper: digest-signed no longer matches the block -> refuse
signed_bad <- render_signed("signed_bad", artifacts_ok)
body2 <- readLines(signed_bad)
body2[grep("^Digest signed:", body2)] <- "Digest signed: sha256:deadbeef"
writeLines(body2, signed_bad)
expect_stop(require_gate(signed_bad),
            "require_gate refuses when digest binding mismatches",
            "package changed|signature|digest")

# approved but no Digest-signed value (placeholder left) -> refuse
signed_nofield <- render_signed("signed_nofield", artifacts_ok)
body3 <- readLines(signed_nofield)
body3[grep("^Digest signed:", body3)] <- "Digest signed: <!-- paste -->"
writeLines(body3, signed_nofield)
expect_stop(require_gate(signed_nofield),
            "require_gate refuses approved record lacking digest field",
            "package changed|signature|digest")

# ==== Part A2: legacy allowlist semantics (integrity batch 3a) ===============
# A digest-less signed record OFF the allowlist is refused (digest-strip
# downgrade defence); ON the allowlist with a matching hash it passes; an
# allowlisted record whose bytes drifted is refused.
dir.create(file.path(wd, "tools"), showWarnings = FALSE)
legacy_rec <- file.path(wd, "legacy.md")
writeLines(c("# rec", "", "## SME decision", "",
             "- [x] APPROVED — stage complete", "Signed: Fixture Reviewer", "Date: 2026-09-01"), legacy_rec)

# no allowlist yet -> off-list digest-less record refused
expect_stop(require_gate(legacy_rec),
            "digest-less record OFF the allowlist is refused (downgrade defence)",
            "digest-strip|allowlist")

# freeze it on the allowlist -> passes
lg <- file.path(wd, "tools", "legacy_gates.yaml")
h_legacy <- paste0("sha256:", digest::digest(file = legacy_rec, algo = "sha256"))
writeLines(c("entries:", "  - kind: digestless", "    path: legacy.md",
             paste0("    sha256: ", h_legacy)), lg)
ok(!is.null(tryCatch(suppressMessages(require_gate(legacy_rec)),
                     error = function(e) NULL)),
   "digest-less record ON the frozen allowlist passes")

# edit the allowlisted record -> hash drift -> refused
writeLines(c("# rec (edited)", "", "## SME decision", "",
             "- [x] APPROVED — stage complete", "Signed: Fixture Reviewer", "Date: 2026-09-01"), legacy_rec)
expect_stop(require_gate(legacy_rec),
            "allowlisted record whose bytes drifted is refused",
            "drift|no longer legacy")
file.remove(lg)  # clear the allowlist for the manifest section below

# ==== Part B: method manifest schema v2 =====================================
skill_dir <- file.path(wd, "skills", "demo")
ref_dir <- file.path(skill_dir, "reference")
dir.create(ref_dir, recursive = TRUE)
writeLines(c("title", "Reviewed by: Fixture Reviewer"), file.path(skill_dir, "SKILL.md"))
ref_file <- file.path(ref_dir, "method.R")
writeLines("f <- function() 1\ng <- function() 2", ref_file)

spec <- list(
  alpha = list(reference_path = ref_file, entrypoints = c("f"),
               status = "approved", evidence = "SKILL.md", reason = "signed"),
  beta  = list(reference_path = ref_file, entrypoints = c("g"),
               status = "draft", evidence = "SKILL.md", reason = "not yet"))
write_method_manifest(skill_dir, spec)
manifest <- file.path(skill_dir, "MANIFEST.yaml")
ok(file.exists(manifest), "write_method_manifest emits MANIFEST.yaml")
m <- read_method_manifest(manifest)
ok(nchar(m$reviewed_by) == 0, "fresh manifest has an empty root reviewed_by")
ok(length(m$units) == 2 &&
   grepl("^sha256:[0-9a-f]{64}$", m$units$alpha$sha256),
   "manifest records a sha256 per unit")
ok(identical(m$units$alpha$status, "approved") &&
   identical(m$units$beta$status, "draft"),
   "unit statuses written from the spec")

# empty reviewed_by -> every unit treated as draft (FALSE), regardless of the
# unit's derived status. (The transitional legacy-signature fallback was
# retired in batch 3c: a signed SKILL*.md must never vouch for an unconfirmed
# manifest's units.)
ok(isFALSE(suppressWarnings(is_method_validated(skill_dir, ref_file, "alpha"))),
   "unconfirmed manifest (empty reviewed_by): approved-in-spec unit still FALSE")
ok(isFALSE(suppressWarnings(is_method_validated(skill_dir, ref_file, "beta"))),
   "unconfirmed manifest (empty reviewed_by): draft unit FALSE")

# confirm the manifest (fill reviewed_by) -> unit statuses become load-bearing
# (round 7, finding 5: a confirmation carries its date, so reviewed_at is
# filled alongside reviewed_by in every fixture.)
mtxt <- readLines(manifest)
mtxt[grep("^reviewed_by:", mtxt)] <- 'reviewed_by: "Fixture Reviewer"'
mtxt[grep("^reviewed_at:", mtxt)] <- 'reviewed_at: "2026-09-01"'
writeLines(mtxt, manifest)
ok(isTRUE(require_validated_method(skill_dir, ref_file, "alpha")),
   "post-cutover: approved unit + matching hash -> TRUE")
ok(isFALSE(is_method_validated(skill_dir, ref_file, "beta")),
   "post-cutover inspection: draft unit -> FALSE")
expect_stop(require_validated_method(skill_dir, ref_file, "beta"),
            "post-cutover execution authorization: draft unit -> hard stop",
            "not execution-authorized")

# missing unit -> integrity hard stop
expect_stop(require_validated_method(skill_dir, ref_file, "gamma"),
            "missing unit -> integrity hard stop", "no unit|does not cover")

# tamper the reference file -> digest mismatch -> hard stop (even for alpha)
writeLines("f <- function() 99  # edited\ng <- function() 2", ref_file)
expect_stop(require_validated_method(skill_dir, ref_file, "alpha"),
            "edited reference file -> hard stop (tamper)",
            "EDITED|mismatch|regenerate")

# missing manifest entirely -> integrity hard stop
skill2 <- file.path(wd, "skills", "nomani")
dir.create(file.path(skill2, "reference"), recursive = TRUE)
r2 <- file.path(skill2, "reference", "m.R"); writeLines("h<-1", r2)
expect_stop(require_validated_method(skill2, r2, "any"),
            "missing manifest -> integrity hard stop", "missing")

# ==== Part B2: regeneration preserves statuses; hash drift resets to draft ===
# Regenerating with the SAME reference content preserves the confirmed status
# and root reviewed_by (no silent downgrade); editing the reference content
# resets the drifted unit to draft AND clears root reviewed_by.
skill3 <- file.path(wd, "skills", "regen")
dir.create(file.path(skill3, "reference"), recursive = TRUE)
r3 <- file.path(skill3, "reference", "k.R"); writeLines("k <- function() 1", r3)
spec3 <- list(only = list(reference_path = r3, entrypoints = c("k"),
                          status = "approved", evidence = "SKILL.md", reason = "signed"))
write_method_manifest(skill3, spec3)
mf3 <- file.path(skill3, "MANIFEST.yaml")
mt3 <- readLines(mf3); mt3[grep("^reviewed_by:", mt3)] <- 'reviewed_by: "Fixture Reviewer"'
mt3[grep("^reviewed_at:", mt3)] <- 'reviewed_at: "2026-09-01"'
writeLines(mt3, mf3)

# regenerate, content unchanged -> status + reviewed_by preserved
suppressMessages(write_method_manifest(skill3, spec3))
mr <- read_method_manifest(mf3)
ok(identical(mr$units$only$status, "approved") && identical(mr$reviewed_by, "Fixture Reviewer"),
   "regeneration with unchanged content preserves status + reviewed_by")

# edit the reference content, regenerate -> unit resets to draft, reviewed_by cleared
writeLines("k <- function() 2  # edited", r3)
suppressMessages(write_method_manifest(skill3, spec3))
md <- read_method_manifest(mf3)
ok(identical(md$units$only$status, "draft") && nchar(md$reviewed_by) == 0,
   "hash drift resets unit to draft and clears root reviewed_by")

# ==== Part B2b: shipped unsigned input and local approval on a copy =========
# Batch 11 finding 11.6 covers regeneration of a multi-unit manifest with a
# real reference and a declared dependency. Copy the shipped EDA skill and
# dependency so the same bytes support both the public and approved cases.
REPO <- normalizePath(file.path(TOOLS, ".."), mustWork = TRUE)
b2b <- file.path(wd, "shipped-regen")
dir.create(file.path(b2b, "tools"), recursive = TRUE)
dir.create(file.path(b2b, "skills"), recursive = TRUE)
stopifnot(file.copy(file.path(TOOLS, "record_selection.R"), file.path(b2b, "tools")),
          file.copy(file.path(REPO, "skills", "eda-standards"),
                    file.path(b2b, "skills"), recursive = TRUE))
b2b_skill <- file.path(b2b, "skills", "eda-standards")
b2b_mf <- file.path(b2b_skill, "MANIFEST.yaml")
b2b_before <- read_method_manifest(b2b_mf)
manifest_spec <- function(m) lapply(m$units, function(u)
  list(reference_path = u$reference_path, entrypoints = u$entrypoints,
       status = u$status, evidence = u$evidence, reason = u$reason,
       depends = vapply(u$depends %||% list(), function(d) d$path, character(1))))
old_b2b <- setwd(b2b)
is_unsigned <- function(m)
  identical(m$reviewed_by, "") && identical(m$reviewed_at, "") &&
    length(m$units) > 0L &&
    all(vapply(m$units, function(u) identical(u$status, "draft"), logical(1)))
ok(is_unsigned(b2b_before),
   "shipped EDA manifest: nonempty draft units with no root confirmation")
suppressMessages(write_method_manifest(b2b_skill, manifest_spec(b2b_before)))
b2b_after <- read_method_manifest(b2b_mf)
ok(is_unsigned(b2b_after) && identical(b2b_after$units, b2b_before$units),
   "shipped EDA manifest: unchanged regeneration preserves unsigned draft units and their digests")

# Create a fresh approval candidate with the same references, dependencies,
# and entrypoints, then record local confirmation on the writer's exact bytes.
stopifnot(file.rename(b2b_mf, file.path(b2b, "unsigned-regenerated.yaml")))
approved_spec <- lapply(manifest_spec(b2b_after), function(u) {
  u$status <- "approved"
  u$evidence <- "SKILL.md"
  u$reason <- "Local fixture review"
  u
})
suppressMessages(write_method_manifest(b2b_skill, approved_spec))
reviewer <- "Fixture Reviewer"
review_date <- "2026-09-01"
approved_lines <- readLines(b2b_mf)
approved_lines <- sub("^reviewed_by: .*", sprintf('reviewed_by: "%s"', reviewer), approved_lines)
approved_lines <- sub("^reviewed_at: .*", sprintf('reviewed_at: "%s"', review_date), approved_lines)
writeLines(approved_lines, b2b_mf)
b2b_approved <- validate_method_manifest(b2b_mf)
ok(all(vapply(names(b2b_approved$units), function(u)
     is_method_validated(b2b_skill, b2b_approved$units[[u]]$reference_path, u), logical(1))),
   "locally confirmed EDA fixture: every unit passes the manifest gate")
manifest_bytes <- function(path)
  sub("(?m)^generated_at:[^\n]*", "generated_at:", readChar(path, file.size(path), useBytes = TRUE),
      perl = TRUE)
approved_bytes <- manifest_bytes(b2b_mf)
suppressMessages(write_method_manifest(b2b_skill, manifest_spec(b2b_approved)))
b2b_after <- read_method_manifest(b2b_mf)
ok(identical(b2b_after$reviewed_by, reviewer),
   "locally confirmed EDA fixture: unchanged regeneration preserves the root signature")
ok(identical(b2b_after$reviewed_at, review_date),
   "locally confirmed EDA fixture: unchanged regeneration preserves reviewed_at")
ok(identical(b2b_after$units, b2b_approved$units),
   "locally confirmed EDA fixture: unchanged regeneration preserves all unit metadata and digests")
ok(identical(manifest_bytes(b2b_mf), approved_bytes),
   "locally confirmed EDA fixture: regeneration preserves manifest bytes apart from generated_at")
setwd(old_b2b)

# ==== Part B3: regeneration-clears-root on ANY content change (batch 3b C2) ==
# Adding a new unit, or changing a unit's metadata (entrypoints/reason/...)
# WITHOUT touching the reference file's bytes, clears the root reviewed_by — a
# byte-identical rewrite is the ONLY regeneration that preserves it. Kills the
# new-unit-inherits-signature and metadata-swap blessing paths (round-4 HIGH 3).
skill4 <- file.path(wd, "skills", "clearroot")
dir.create(file.path(skill4, "reference"), recursive = TRUE)
r4 <- file.path(skill4, "reference", "m.R"); writeLines("a <- function() 1", r4)
r4b <- file.path(skill4, "reference", "n.R"); writeLines("b <- function() 2", r4b)
spec4 <- list(u1 = list(reference_path = r4, entrypoints = c("a"),
                        status = "approved", evidence = "SKILL.md", reason = "signed"))
write_method_manifest(skill4, spec4)
mf4 <- file.path(skill4, "MANIFEST.yaml")
fill_root <- function(mf) {
  t <- readLines(mf); t[grep("^reviewed_by:", t)] <- 'reviewed_by: "Fixture Reviewer"'
  t[grep("^reviewed_at:", t)] <- 'reviewed_at: "2026-09-01"'; writeLines(t, mf)
}

# (a) byte-identical rewrite preserves root
fill_root(mf4)
suppressMessages(write_method_manifest(skill4, spec4))
ok(identical(read_method_manifest(mf4)$reviewed_by, "Fixture Reviewer"),
   "byte-identical regeneration preserves root reviewed_by")

# (b) metadata change (reason) with SAME file content clears root
fill_root(mf4)
spec4b <- spec4; spec4b$u1$reason <- "re-scoped by an agent"
suppressMessages(write_method_manifest(skill4, spec4b))
ok(nchar(read_method_manifest(mf4)$reviewed_by) == 0,
   "metadata change (unchanged file) clears root reviewed_by (metadata-swap defence)")

# (c) adding a NEW unit clears root (new-unit-inherits-signature defence)
fill_root(mf4)
spec4c <- spec4b
spec4c$u2 <- list(reference_path = r4b, entrypoints = c("b"),
                  status = "approved", evidence = "SKILL.md", reason = "sneaked in")
suppressMessages(write_method_manifest(skill4, spec4c))
mc <- read_method_manifest(mf4)
ok(nchar(mc$reviewed_by) == 0 && !is.null(mc$units$u2),
   "adding a new unit clears root reviewed_by (new-unit cannot inherit a signature)")

# ==== Part C: dotted-runner unknown-tool hard stop (batch 3b, item C1) =======
# The internal .psn_run gates on a tool->unit map and REFUSES an unmapped tool
# rather than run system2 ungated — safe even when called directly.
# Sourced with the repository root as the working directory: since integrity
# batch 11 every reference builds its sealed validator environment AT LOAD
# TIME, and that construction resolves tools/method_manifest.R repo-relative
# exactly as the reference headers say. The assertions below run back in the
# fixture directory.
se <- new.env(); ue <- new.env()
local({
  wprev <- setwd(old); on.exit(setwd(wprev), add = TRUE)
  source(file.path("skills", "psn-simeval", "reference", "psn_simeval.R"),
         local = se)
  source(file.path("skills", "psn-uncertainty", "reference",
                   "psn_uncertainty.R"), local = ue)
})
expect_stop(se$.psn_run("exec", wd, "frobnicate", character(0)),
            "psn-simeval .psn_run refuses an unknown tool name", "unknown PsN tool")
expect_stop(ue$.psn_run(wd, "frobnicate", character(0), "exec", old),
            "psn-uncertainty .psn_run refuses an unknown tool name", "unknown PsN tool")

# ==== Part C: legacy-allowlist pin violation (batch 3b, item C3) =============
# check_allowlist_pin FAILs when the on-disk allowlist gains an entry the pin
# does not list. Fixtured in a tempdir with a copy of gate.R + a tampered
# legacy_gates.yaml; the real check function is sourced from tools/ci_check.R.
cienv <- new.env(); source(file.path(old, "tools", "ci_check.R"), local = cienv)
pindir <- file.path(wd, "pinfix"); dir.create(file.path(pindir, "tools"), recursive = TRUE)
file.copy(file.path(old, "tools", "gate.R"), file.path(pindir, "tools", "gate.R"))
ok(identical(cienv$EXPECTED_ALLOWLIST, character(0)),
   "INTEGRITY-LEGACY: public driver begins with an empty pin")
ok(!file.exists(file.path(old, "tools", "legacy_gates.yaml")),
   "INTEGRITY-LEGACY: public snapshot ships no legacy allowlist")
pinned <- vapply(c("legacy-a", "legacy-b"), function(stem) {
  path <- file.path("gates", "PIN-FIXTURE", paste0(stem, ".md"))
  dir.create(dirname(file.path(pindir, path)), recursive = TRUE, showWarnings = FALSE)
  writeLines(c("Analysis ID: PIN-FIXTURE", paste("Stage:", stem),
               "## SME decision", "- [x] APPROVED", "Signed: Fixture Reviewer",
               "Date: 2026-09-02"), file.path(pindir, path))
  paste("digestless", path,
        paste0("sha256:", digest::digest(file = file.path(pindir, path), algo = "sha256")),
        sep = "\t")
}, character(1), USE.NAMES = FALSE)
FIXTURE_SHA <- paste0("sha256:", paste(rep("0", 64), collapse = ""))
write_allowlist <- function(items) {
  ln <- c("entries:")
  for (it in items) {
    kp <- strsplit(it, "\t", fixed = TRUE)[[1]]
    ln <- c(ln, sprintf("  - kind: %s", kp[1]), sprintf("    path: %s", kp[2]),
            paste0("    sha256: ", if (length(kp) >= 3L) kp[3] else FIXTURE_SHA))
  }
  writeLines(ln, file.path(pindir, "tools", "legacy_gates.yaml"))
}
run_pin <- function() {
  wprev <- setwd(pindir); on.exit(setwd(wprev))
  cienv$.fail_count <- 0L
  out <- capture.output(cienv$check_allowlist_pin())
  list(out = out, fail = cienv$.fail_count)
}
ok(run_pin()$fail == 0L, "INTEGRITY-LEGACY: missing allowlist matches the public empty pin")
write_allowlist(pinned)
ok(run_pin()$fail == 1L, "INTEGRITY-LEGACY: public pin refuses local legacy additions")
cienv$EXPECTED_ALLOWLIST <- pinned
write_allowlist(pinned)
ok(run_pin()$fail == 0L, "allowlist pin PASSES on the exact pinned set")
write_allowlist(c(pinned, "digestless\tgates/SNEAK-001/extra.md\t" %+% FIXTURE_SHA))
ok(run_pin()$fail == 1L, "allowlist pin FAILs on an unexpected added entry")
write_allowlist(pinned[-1])
ok(run_pin()$fail == 1L, "allowlist pin FAILs on a removed entry")
# Order is pinned too: runtime lookup is first-match, so order is part of the
# anchor's meaning.
write_allowlist(rev(pinned))
ok(run_pin()$fail == 1L, "allowlist pin FAILs when the entries are reordered")
# The coordinated attack (round 7, finding 11): edit an allowlisted legacy
# record's bytes AND update its frozen sha256 in the allowlist so the binding
# check stays green. The (kind, path) set is unchanged; only the sha differs.
# The pin must refuse it.
retagged <- pinned
target <- strsplit(pinned[[1L]], "\t", fixed = TRUE)[[1L]][2L]
cat("changed record bytes\n", file = file.path(pindir, target), append = TRUE)
changed_sha <- paste0("sha256:", digest::digest(file = file.path(pindir, target), algo = "sha256"))
retagged[1] <- sub("\tsha256:[0-9a-f]{64}$", paste0("\t", changed_sha), retagged[1])
write_allowlist(retagged)
rp <- run_pin()
ok(rp$fail == 1L && any(grepl("sha256|digest", rp$out)),
   "allowlist pin FAILs when one entry's frozen sha256 changes (coordinated gate-byte + allowlist edit)")

# --- Part C2: duplicate / malformed allowlist entries (round 6, finding 7) ---
# The attack: prepend a SECOND entry with the same kind+path as an existing one
# but a different frozen sha256. Runtime lookup takes the first match, so the
# attacker's hash wins; a set-difference pin sees no new (kind, path) pair and
# stays green. Both layers must now refuse it.
write_allowlist(c(pinned[1], pinned))
ok(run_pin()$fail == 1L, "allowlist pin FAILs on a duplicated (kind, path) entry")
dupfile <- file.path(pindir, "tools", "legacy_gates.yaml")
expect_stop(.read_legacy_gates(dupfile),
            "allowlist parser STOPS on a duplicate path", "duplicate")

mal <- file.path(wd, "legacy_malformed.yaml")
writeLines(c("entries:", "  - kind: digestless", "    path: gates/X/a.md",
             "    sha256: sha256:0"), mal)
expect_stop(.read_legacy_gates(mal),
            "allowlist parser STOPS on a malformed sha256", "sha256")
writeLines(c("entries:", "  - kind: digestless", "    path: gates/X/a.md"), mal)
expect_stop(.read_legacy_gates(mal),
            "allowlist parser STOPS on a missing sha256 field", "missing")
writeLines(c("entries:", "  - kind: bogus", "    path: gates/X/a.md",
             paste0("    sha256: ", FIXTURE_SHA)), mal)
expect_stop(.read_legacy_gates(mal),
            "allowlist parser STOPS on an unknown kind", "kind")
writeLines(c("entries:", "  - kind: digestless",
             paste0("    sha256: ", FIXTURE_SHA)), mal)
expect_stop(.read_legacy_gates(mal),
            "allowlist parser STOPS on a missing path field", "missing")

# ==== Part D: approval ledger (batch 3b tranche 2, item 1) ==================
# Batch 4: the record's Analysis ID must match its directory, so the fixture
# analysis lives at <ledger root>/gates/TST-001/eda.md with artifacts at the root.
lroot <- file.path(wd, "ledger"); ldir <- file.path(lroot, "gates", "TST-001")
dir.create(ldir, recursive = TRUE)
file.copy(file.path(wd, "a.csv"), file.path(lroot, "a.csv"))
file.copy(file.path(wd, "b.csv"), file.path(lroot, "b.csv"))
arts_l <- data.frame(artifact = c("t1", "t2"), path = c("a.csv", "b.csv"),
                     source = c("ta", "tb"), stringsAsFactors = FALSE)
lrec <- file.path(ldir, "eda.md")
old2 <- setwd(lroot)  # so artifact rehash resolves a.csv/b.csv relative paths

writeLines(c("Analysis ID: TST-001", "Stage: eda", "", "## SME decision", "",
             "- [ ] APPROVED — stage complete",
             "Digest signed: <!-- -->", "Signed: <!-- -->"), lrec)
render_gate_package(lrec, arts_l)
ll <- readLines(lrec)
ldg <- sub("^Package-Digest: ", "", grep("^Package-Digest:", ll, value = TRUE)[1])
ll[grep("^- \\[ \\] APPROVED", ll)] <- "- [x] APPROVED — stage complete"
ll[grep("^Digest signed:", ll)] <- paste("Digest signed:", ldg)
ll[grep("^Signed:", ll)] <- "Signed: Fixture Reviewer Date: 2026-09-01"
writeLines(ll, lrec)

# before approve_gate: require_gate passes but WARNS "signed but not yet ledgered"
warned <- FALSE
withCallingHandlers(require_gate(lrec),
  message = function(m) if (grepl("not yet ledgered", conditionMessage(m))) warned <<- TRUE)
ok(warned, "unledgered signed record passes require_gate with a warning")

# approve_gate appends exactly one event; verify_approvals_ledger is clean
ev1 <- suppressMessages(approve_gate(lrec, check_store = FALSE))  # no targets store in this fixture
lg <- readLines(file.path(ldir, "approvals.jsonl"))
ok(length(lg) == 1L, "approve_gate appends exactly one ledger line")
ok(verify_approvals_ledger(ldir)$ok, "verify_approvals_ledger passes on a fresh chain")

# event_id recomputation
ev_read <- .read_ledger(file.path(ldir, "approvals.jsonl"))[[1]]
ok(identical(as.character(ev_read$event_id), .ledger_event_id(ev_read)),
   "stored event_id recomputes from the event fields")

# after approve_gate: require_gate passes WITHOUT the warning
warned2 <- FALSE
withCallingHandlers(require_gate(lrec),
  message = function(m) if (grepl("not yet ledgered", conditionMessage(m))) warned2 <<- TRUE)
ok(!warned2, "ledgered record passes require_gate with no unledgered warning")

# idempotence: re-recording the identical event is refused
expect_stop(approve_gate(lrec, check_store = FALSE), "approve_gate is idempotent (refuses a duplicate)",
            "idempotence|identical")

# revocation: require_gate then refuses until superseded
suppressMessages(revoke_gate(lrec, "SME withdrew approval"))
expect_stop(require_gate(lrec),
            "revoked latest event refuses require_gate", "revocation|rejection|ledger")
ok(verify_approvals_ledger(ldir)$ok, "chain still verifies after a revocation event")

# re-approval supersedes the revocation -> passes again
suppressMessages(approve_gate(lrec, check_store = FALSE))
ok(!is.null(tryCatch(require_gate(lrec), error = function(e) NULL)),
   "a new approving event supersedes the revocation")

# event_id tamper is caught by verify_approvals_ledger
jl <- readLines(file.path(ldir, "approvals.jsonl"))
jl[1] <- sub('"decision":"approved"', '"decision":"rejected"', jl[1], fixed = TRUE)
tdir <- file.path(wd, "tamper"); dir.create(tdir)
writeLines(jl, file.path(tdir, "approvals.jsonl"))
ok(!verify_approvals_ledger(tdir)$ok,
   "verify_approvals_ledger catches an edited event (event_id mismatch)")

# append-only prefix invariant
base <- c("line1", "line2")
ok(ledger_append_only_ok(base, c(base, "line3")),
   "append-only OK when committed bytes are a prefix")
ok(!ledger_append_only_ok(base, c("line1", "EDITED")),
   "append-only violation caught when a committed line is altered")
ok(ledger_append_only_ok(character(0), c("line1")),
   "append-only trivially OK for a brand-new ledger (empty base)")

setwd(old2)

setwd(old)
cat("-----------------------------------------------\n")
cat(sprintf("integrity battery: %d passed, %d failed\n", .pass, .fail))
quit(status = if (.fail > 0L) 1L else 0L)

}  # end .run_integrity_tests

if (.invoked_as_script()) .run_integrity_tests()
