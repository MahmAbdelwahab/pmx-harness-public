#!/usr/bin/env Rscript
# canon2 rendering-determinism golden vectors (integrity batch 3b tranche 2,
# item 2). The package digest hashes the EXACT RENDERED BYTES of the generated
# block, so those bytes must be byte-identical no matter what R print/locale
# options are in effect when a task module renders. This battery renders and
# digests a FIXED set of input objects under a matrix of hostile option
# settings and asserts every digest is identical; then it flips a single byte
# and asserts the change is caught.
#
# SAFE TO SOURCE: guarded behind .invoked_as_script() exactly like
# tools/ci_check.R, so tar_source("tools") only defines and runs nothing.
#   Rscript tools/tests/test_canon2_golden.R

.invoked_as_script <- function() {
  fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  length(fa) > 0 && grepl("test_canon2_golden\\.R$", sub("^--file=", "", fa[1]))
}

.run_golden <- function() {
suppressWarnings(suppressMessages({
  args <- commandArgs(FALSE)
  file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
  TOOLS <- normalizePath(file.path(dirname(file_arg), ".."), mustWork = TRUE)
  source(file.path(TOOLS, "gate_package.R"))
  source(file.path(TOOLS, "gate.R"))
}))

.pass <- 0L; .fail <- 0L
ok <- function(cond, name) {
  if (isTRUE(cond)) { .pass <<- .pass + 1L; cat(sprintf("PASS  %s\n", name)) }
  else { .fail <<- .fail + 1L; cat(sprintf("FAIL  %s\n", name)) }
}

# --- fixed input objects ----------------------------------------------------
# Deliberately exercises: integer-valued doubles, non-integer doubles needing
# full precision, negative zero, large/small exponents, non-finite values, NA
# across classes, logicals, and characters carrying pipe / backslash / newline.
artifacts <- data.frame(
  artifact = c("alpha", "beta|gamma", "delta\\eps"),
  path = c("a.csv", "b.csv", "c.csv"),
  source = c("t_a", "t_b", "t_c"),
  stringsAsFactors = FALSE)
tables <- list(
  "Numeric edge cases" = data.frame(
    int_valued = c(1.0, 100.0, -0.0, 1e12),
    precise    = c(0.1, 1.23, 1/3, 2.5),
    exponents  = c(1e-300, 1e300, 6.022e23, 1.6e-19),
    nonfinite  = c(NA_real_, NaN, Inf, -Inf),
    flag       = c(TRUE, FALSE, NA, TRUE),
    label      = c("x", "y|z", "line1\nline2", NA_character_),
    stringsAsFactors = FALSE),
  "Integers" = data.frame(i = c(1L, 2L, NA_integer_), stringsAsFactors = FALSE))
notes <- c("a note line", "another | with a pipe")

digest_of <- function() {
  # Digest the full canonical package serialization (uses md_table throughout).
  gate_package_digest(artifacts, tables, notes)
}

# --- option matrix ----------------------------------------------------------
baseline <- digest_of()
ok(grepl("^sha256:[0-9a-f]{64}$", baseline), "baseline digest well-formed")

run_under <- function(setter, label) {
  saved <- setter$set()
  on.exit(setter$restore(saved))
  d <- digest_of()
  ok(identical(d, baseline), sprintf("digest invariant under %s", label))
}

run_under(list(set = function() { o <- options(digits = 3); o },
               restore = function(s) options(s)), "digits=3")
run_under(list(set = function() { o <- options(scipen = 999); o },
               restore = function(s) options(s)), "scipen=999")
run_under(list(set = function() { o <- options(digits = 22); o },
               restore = function(s) options(s)), "digits=22")
run_under(list(set = function() { o <- options(OutDec = ","); o },
               restore = function(s) options(s)), "OutDec=comma")
run_under(list(
  set = function() {
    old <- Sys.getlocale("LC_NUMERIC")
    set <- suppressWarnings(Sys.setlocale("LC_NUMERIC", "de_DE.UTF-8"))
    if (!nzchar(set)) suppressWarnings(Sys.setlocale("LC_NUMERIC", "de_DE"))
    old
  },
  restore = function(old) suppressWarnings(Sys.setlocale("LC_NUMERIC", old))),
  "LC_NUMERIC variation (best-effort; identical if settable)")

# --- non-finite / negative-zero deterministic tokens ------------------------
nf <- md_table(data.frame(v = c(NA_real_, NaN, Inf, -Inf, -0.0), stringsAsFactors = FALSE))
ok(all(c("| NA |", "| NaN |", "| Inf |", "| -Inf |", "| 0 |") %in% nf),
   "non-finite + negative-zero render as fixed deterministic tokens")

# --- large/small exponent exactness -----------------------------------------
# 1e-300 is non-integer -> %.17g ("1e-300"); 1e300 is integer-valued -> plain
# integer form (%.0f, a 301-digit exact integer). Both are option-independent.
ex1 <- md_table(data.frame(v = c(1e-300), stringsAsFactors = FALSE))
ex2 <- md_table(data.frame(v = c(1e300), stringsAsFactors = FALSE))
ok(any(ex1 == "| 1e-300 |"), "small exponent renders via %.17g deterministically")
ok(any(grepl("^\\| 1000000000000000[0-9]+ \\|$", ex2)),
   "large integer-valued double renders in exact plain integer form")

# --- one-byte tamper is caught ----------------------------------------------
canon <- gate_package_canonical(artifacts, tables, notes)
d0 <- paste0("sha256:", digest::digest(canon, algo = "sha256", serialize = FALSE))
tampered <- sub("alpha", "alphb", canon, fixed = TRUE)   # one-byte flip
d1 <- paste0("sha256:", digest::digest(tampered, algo = "sha256", serialize = FALSE))
ok(identical(d0, baseline), "canonical digest equals package digest")
ok(!identical(d0, d1), "single-byte tamper changes the digest (caught)")

cat("-----------------------------------------------\n")
cat(sprintf("canon2 golden battery: %d passed, %d failed\n", .pass, .fail))
quit(status = if (.fail > 0L) 1L else 0L)
}

if (.invoked_as_script()) .run_golden()
