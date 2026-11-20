#!/usr/bin/env Rscript
# Focused battery for the staged gate-sign and approval transaction.
# Every mutable fixture lives under tempdir(); no repository gate is touched.

.invoked_as_script <- function() {
  fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  length(fa) > 0L && identical(basename(sub("^--file=", "", fa[1])),
                               "test_sign_approve_gate.R")
}

.run_sign_approve_tests <- function() {
  options(warn = 1)
  file_arg <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
  TOOLS <- normalizePath(file.path(dirname(file_arg), ".."), mustWork = TRUE)
  REPO <- normalizePath(file.path(TOOLS, ".."), mustWork = TRUE)
  passed <- 0L; failed <- 0L
  ok <- function(condition, label) {
    value <- tryCatch(isTRUE(condition), error = function(e) {
      cat("      error:", conditionMessage(e), "\n"); FALSE
    })
    if (value) { passed <<- passed + 1L; cat("PASS ", label, "\n", sep = "")
    } else { failed <<- failed + 1L; cat("FAIL ", label, "\n", sep = "") }
  }
  expect_stop <- function(expr, label, pattern = NULL) {
    err <- tryCatch({ force(expr); NULL }, error = identity)
    hit <- inherits(err, "error") &&
      (is.null(pattern) || grepl(pattern, conditionMessage(err), ignore.case = TRUE))
    if (inherits(err, "error") && !hit) cat("      got:", conditionMessage(err), "\n")
    ok(hit, label)
  }
  digest_raw <- function(bytes) paste0(
    "sha256:", digest::digest(bytes, algo = "sha256", serialize = FALSE))
  read_raw <- function(path) readBin(path, "raw", file.info(path)$size)
  write_raw <- function(path, bytes) writeBin(bytes, path)
  file_hash <- function(path) if (file.exists(path)) digest_raw(read_raw(path)) else "absent"
  inventory <- function(root) list(
    gates = sort(list.files(file.path(root, "gates"), pattern = "\\.md$",
                            recursive = TRUE, full.names = FALSE)),
    approvals = sort(list.files(file.path(root, "gates"), pattern = "^approvals\\.jsonl$",
                                recursive = TRUE, full.names = FALSE)),
    errata = sort(list.files(file.path(root, "gates"), pattern = "^errata\\.jsonl$",
                             recursive = TRUE, full.names = FALSE)))
  mode_of <- function(path) as.integer(file.info(path)$mode) %% 512L
  with_wd <- function(path, expr) {
    old <- setwd(path); on.exit(setwd(old), add = TRUE); force(expr)
  }

  approval_source <- paste(readLines(file.path(TOOLS, "approve_gate.R"), warn = FALSE),
                           collapse = "\n")
  command_path <- file.path(TOOLS, "sign_approve_gate.R")
  cat("--- A: required public surfaces and source safety ---\n")
  api_exists <- grepl("approve_staged_gate\\s*<-\\s*function", approval_source, perl = TRUE)
  command_exists <- file.exists(command_path)
  ok(api_exists, "A: approve_staged_gate() public API exists")
  ok(command_exists, "A: sign_approve_gate.R command exists")
  if (!api_exists || !command_exists) {
    cat(sprintf("\n%d passed, %d failed\n", passed, failed))
    return(invisible(failed))
  }

  suppressWarnings(suppressMessages({
    source(file.path(TOOLS, "gate_package.R"))
    source(file.path(TOOLS, "gate.R"))
    source(file.path(TOOLS, "approve_gate.R"))
    source(command_path)
  }))
  ok(exists("approve_staged_gate", mode = "function"),
     "A: approve_gate.R is source-safe from this battery")
  ok(exists("sign_approve_main", mode = "function"),
     "A: sign_approve_gate.R is source-safe")

  template_body <- function(id = "TST-001", stage = "eda") c(
    "# Gate Sign-off Record", "", paste0("Analysis ID: ", id),
    paste0("Stage: ", stage), "Date opened: 2026-09-11", "", "## SME decision", "",
    "- [ ] APPROVED — stage complete, next stage may start",
    "- [ ] APPROVED WITH CHANGES — changes listed below were applied and re-verified",
    "- [ ] REJECTED — returned to stage with instructions below", "",
    "Signed: <!-- SME name -->", "Date: <!-- -->", "Digest signed: <!-- paste -->", "",
    "SME note: retain this byte-for-byte")
  make_fixture <- function(eol = "\n") {
    root <- tempfile("sign-approve-")
    dir.create(file.path(root, "gates", "TST-001"), recursive = TRUE)
    writeLines("synthetic artifact", file.path(root, "artifact.txt"), useBytes = TRUE)
    gate <- file.path(root, "gates", "TST-001", "eda.md")
    writeLines(template_body(), gate, useBytes = TRUE)
    with_wd(root, render_gate_package(
      gate,
      data.frame(artifact = "synthetic", path = "artifact.txt", source = "fixture",
                 stringsAsFactors = FALSE),
      list("Synthetic" = data.frame(value = "fixture", stringsAsFactors = FALSE)),
      "fixture only"))
    if (identical(eol, "\r\n")) {
      lines <- readLines(gate, warn = FALSE)
      write_raw(gate, charToRaw(paste0(paste(lines, collapse = eol), eol)))
    }
    list(root = root, gate = gate, ledger = file.path(dirname(gate), "approvals.jsonl"))
  }
  preview <- function(fx, decision = "approved", signer = "Fixture Reviewer",
                      date = "2026-09-11", name = "approval-bundle") {
    out <- file.path(dirname(fx$gate), name)
    res <- with_wd(fx$root, .sign_approve_preview(
      fx$gate, decision = decision, signer = signer, date = date,
      out = out, check_store = FALSE))
    list(result = res, out = out)
  }
  apply_bundle <- function(fx, p, token = p$result$confirm_token, gate = fx$gate) {
    with_wd(fx$root, .sign_approve_apply(
      p$out, gate = gate, confirm = token, check_store = FALSE))
  }
  no_write_state <- function(fx) list(record = file_hash(fx$gate), ledger = file_hash(fx$ledger))
  is_symlink <- function(path) {
    link <- Sys.readlink(path)
    length(link) == 1L && !is.na(link) && nzchar(link)
  }

  cat("\n--- B: preview content, privacy, and inventories ---\n")
  fx <- make_fixture("\r\n")
  base <- read_raw(fx$gate); inv_before <- inventory(fx$root)
  shown <- capture.output(p <- preview(fx, decision = "approved_with_changes",
                                      signer = "SME-1", date = "2026-09-11"),
                          type = "output")
  candidate <- read_raw(file.path(p$out, "candidate.bin"))
  request <- .sign_approve_read_request(p$out)
  parsed_candidate <- parse_gate_record(fx$gate,
                                        lines = .split_record_bytes(candidate, "test")$lines)
  ok(identical(parsed_candidate$decision, "approved_with_changes") &&
       identical(parsed_candidate$signer, "SME-1") &&
       identical(parsed_candidate$date, "2026-09-11"),
     "B: preview applies explicit decision, signer, and date")
  ok(identical(parsed_candidate$digest_signed, parsed_candidate$package_digest) &&
       identical(request$package_digest, parsed_candidate$package_digest),
     "B: option B copies displayed Package-Digest into Digest signed")
  bp <- .split_record_bytes(base, "test"); cp <- .split_record_bytes(candidate, "test")
  changed <- which(bp$lines != cp$lines)
  ok(length(changed) == 4L &&
       all(grepl("^(- \\[x\\] APPROVED WITH CHANGES|Signed:|Date:|Digest signed:)",
                 cp$lines[changed])) &&
       identical(bp$eol, cp$eol) && identical(bp$trailing, cp$trailing),
     "B: preview changes exactly four signature fields and preserves CRLF/trailing EOL")
  ok(identical(sort(list.files(p$out)), c("candidate.bin", "request.json")) &&
       mode_of(p$out) == strtoi("700", base = 8L) &&
       all(vapply(file.path(p$out, c("candidate.bin", "request.json")), mode_of,
                  integer(1)) == strtoi("600", base = 8L)),
     "B: bundle has exactly two private payloads with 0700/0600 modes")
  ok(identical(inv_before, inventory(fx$root)),
     "B: retained bundle changes no gate, approval-ledger, or errata inventory")
  ok(all(vapply(c(request$destination, request$base_record, request$base_ledger,
                  request$candidate, request$package_digest, p$result$confirm_token),
                function(x) any(grepl(x, shown, fixed = TRUE)), logical(1))) &&
       any(grepl("SME note: retain this byte-for-byte", shown, fixed = TRUE)),
     "B: preview displays full candidate and every bound transaction field")

  bad <- make_fixture()
  expect_stop(preview(bad, decision = "rejected"), "B: unsupported decision refuses", "decision")
  expect_stop(preview(bad, signer = ""), "B: empty signer refuses", "signer")
  expect_stop(preview(bad, date = "11/09/2026"), "B: non-ISO date refuses", "date")
  duplicate <- readLines(bad$gate, warn = FALSE)
  duplicate <- append(duplicate, "Signed: duplicate", after = grep("^Signed:", duplicate))
  writeLines(duplicate, bad$gate, useBytes = TRUE)
  expect_stop(preview(bad, name = "duplicate-bundle"),
              "B: ambiguous signature layout refuses", "exactly one|ambiguous")
  wrong_id <- make_fixture(); wrong <- readLines(wrong_id$gate, warn = FALSE)
  wrong[grep("^Stage:", wrong)] <- "Stage: other"; writeLines(wrong, wrong_id$gate, useBytes = TRUE)
  expect_stop(preview(wrong_id), "B: identity-problem record refuses", "identity|Stage")
  collision <- make_fixture(); dir.create(file.path(dirname(collision$gate), "approval-bundle"))
  expect_stop(preview(collision), "B: existing output path refuses without overwrite", "exists|new")
  if (.Platform$OS.type == "unix") {
    link_fx <- make_fixture(); target <- tempfile("bundle-target-"); dir.create(target)
    file.symlink(target, file.path(dirname(link_fx$gate), "approval-bundle"))
    expect_stop(preview(link_fx), "B: symlink output path refuses", "symlink|exists")
    gate_link_fx <- make_fixture()
    gate_link <- file.path(dirname(gate_link_fx$gate), "linked.md")
    file.symlink(gate_link_fx$gate, gate_link)
    linked <- gate_link_fx; linked$gate <- gate_link
    expect_stop(preview(linked, name = "linked-bundle"),
                "B: symlink gate input refuses", "symlink")
  }

  cat("\n--- C: untrusted bundle and token refusals ---\n")
  tampered <- make_fixture(); tp <- preview(tampered); before <- no_write_state(tampered)
  cand_path <- file.path(tp$out, "candidate.bin")
  write_raw(cand_path, c(read_raw(cand_path), charToRaw("tamper")))
  expect_stop(apply_bundle(tampered, tp), "C: changed candidate bytes refuse", "candidate|hash")
  ok(identical(before, no_write_state(tampered)), "C: changed candidate writes nothing")
  req_fx <- make_fixture(); rp <- preview(req_fx); before <- no_write_state(req_fx)
  req_path <- file.path(rp$out, "request.json"); write_raw(req_path, c(read_raw(req_path), charToRaw(" ")))
  expect_stop(apply_bundle(req_fx, rp), "C: non-canonical changed request refuses", "request")
  ok(identical(before, no_write_state(req_fx)), "C: changed request writes nothing")
  token_fx <- make_fixture(); kp <- preview(token_fx); before <- no_write_state(token_fx)
  expect_stop(apply_bundle(token_fx, kp,
                          token = paste0("sha256:", paste(rep("0", 64), collapse = ""))),
              "C: wrong confirmation token refuses", "confirm|token")
  ok(identical(before, no_write_state(token_fx)), "C: wrong token writes nothing")
  dest_fx <- make_fixture(); dp <- preview(dest_fx); other <- make_fixture(); before <- no_write_state(dest_fx)
  expect_stop(apply_bundle(dest_fx, dp, gate = other$gate),
              "C: wrong destination refuses", "destination|gate")
  ok(identical(before, no_write_state(dest_fx)), "C: wrong destination writes nothing")
  extra_fx <- make_fixture(); ep <- preview(extra_fx); before <- no_write_state(extra_fx)
  writeLines("extra", file.path(ep$out, "extra.txt"))
  expect_stop(apply_bundle(extra_fx, ep), "C: extra bundle payload refuses", "exactly|payload")
  ok(identical(before, no_write_state(extra_fx)), "C: extra payload writes nothing")

  cat("\n--- D: locked transition and conservative refusals ---\n")
  clean <- make_fixture(); clean_p <- preview(clean)
  clean_candidate <- read_raw(file.path(clean_p$out, "candidate.bin"))
  clean_event <- apply_bundle(clean, clean_p); clean_lines <- readLines(clean$ledger, warn = FALSE)
  ok(identical(read_raw(clean$gate), clean_candidate) && length(clean_lines) == 1L &&
       identical(clean_event$record_digest, digest_raw(clean_candidate)),
     "D: clean apply replaces record and appends exactly one matching event")
  ok(isTRUE(verify_approvals_ledger(clean$ledger)$ok), "D: clean staged ledger verifies")

  body_fx <- make_fixture(); body_p <- preview(body_fx)
  body_candidate <- read_raw(file.path(body_p$out, "candidate.bin"))
  parts <- .split_record_bytes(body_candidate, "test")
  parts$lines[grep("^Date opened:", parts$lines)] <- "Date opened: 2026-09-10"
  body_candidate <- .join_record_bytes(parts, parts$lines); before <- no_write_state(body_fx)
  expect_stop(with_wd(body_fx$root, approve_staged_gate(
                body_fx$gate, body_candidate, body_p$result$request$base_record,
                body_p$result$request$base_ledger, check_store = FALSE)),
              "D: self-consistent body replacement refuses", "only|four|signature")
  ok(identical(before, no_write_state(body_fx)), "D: body replacement writes nothing")

  identity_fx <- make_fixture(); identity_p <- preview(identity_fx)
  other_gate <- file.path(dirname(identity_fx$gate), "other.md")
  other_lines <- readLines(identity_fx$gate, warn = FALSE)
  other_lines[grep("^Stage:", other_lines)] <- "Stage: other"
  writeLines(other_lines, other_gate, useBytes = TRUE)
  identity_ledger <- file.path(dirname(other_gate), "approvals.jsonl")
  identity_before <- list(record = file_hash(other_gate), ledger = file_hash(identity_ledger))
  expect_stop(with_wd(identity_fx$root, approve_staged_gate(
                other_gate, read_raw(file.path(identity_p$out, "candidate.bin")),
                file_hash(other_gate), "absent", check_store = FALSE)),
              "D: candidate validated against final destination identity refuses",
              "identity|Stage|task")
  ok(identical(identity_before,
               list(record = file_hash(other_gate), ledger = file_hash(identity_ledger))),
     "D: wrong destination identity writes nothing")

  drift_fx <- make_fixture(); drift_p <- preview(drift_fx); before_ledger <- file_hash(drift_fx$ledger)
  writeLines(c(readLines(drift_fx$gate, warn = FALSE), "concurrent edit"), drift_fx$gate)
  changed_record <- file_hash(drift_fx$gate)
  expect_stop(apply_bundle(drift_fx, drift_p), "D: record drift refuses", "record|drift|state")
  ok(identical(file_hash(drift_fx$gate), changed_record) &&
       identical(file_hash(drift_fx$ledger), before_ledger),
     "D: record drift preserves changed record and ledger")

  wait_fx <- make_fixture(); wait_p <- preview(wait_fx); wait_ledger <- file_hash(wait_fx$ledger)
  expect_stop(with_wd(wait_fx$root, .approve_staged_gate_impl(
    wait_fx$gate, read_raw(file.path(wait_p$out, "candidate.bin")),
    wait_p$result$request$base_record, wait_p$result$request$base_ledger,
    check_store = FALSE,
    hooks = list(after_lock = function(gate, ...) {
      writeLines(c(readLines(gate, warn = FALSE), "edit while lock waiter resumes"), gate)
    }))), "D: record changed at the lock-wait boundary refuses", "record|state|drift")
  ok(identical(file_hash(wait_fx$ledger), wait_ledger) &&
       any(grepl("edit while lock waiter resumes", readLines(wait_fx$gate, warn = FALSE),
                 fixed = TRUE)),
     "D: lock-wait drift appends nothing and preserves concurrent edit")

  if (.Platform$OS.type == "unix") {
    lock_link_fx <- make_fixture(); lock_link_p <- preview(lock_link_fx)
    lock_link_target <- file.path(dirname(lock_link_fx$gate), "lock-link-target.md")
    write_raw(lock_link_target, read_raw(lock_link_fx$gate))
    lock_link_target_before <- file_hash(lock_link_target)
    expect_stop(with_wd(lock_link_fx$root, .approve_staged_gate_impl(
      lock_link_fx$gate, read_raw(file.path(lock_link_p$out, "candidate.bin")),
      lock_link_p$result$request$base_record,
      lock_link_p$result$request$base_ledger, check_store = FALSE,
      hooks = list(after_lock = function(gate, ...) {
        unlink(gate)
        if (!file.symlink(lock_link_target, gate)) stop("fixture symlink failed")
      }))), "D: symlink swap at lock-wait boundary refuses", "symlink|regular")
    ok(is_symlink(lock_link_fx$gate) &&
         identical(file_hash(lock_link_target), lock_link_target_before) &&
         !file.exists(lock_link_fx$ledger),
       "D: lock-wait symlink refusal preserves target and appends no event")
  }

  ledger_fx <- make_fixture(); ledger_p <- preview(ledger_fx); base_record_hash <- file_hash(ledger_fx$gate)
  writeLines("foreign ledger bytes", ledger_fx$ledger); ledger_hash <- file_hash(ledger_fx$ledger)
  expect_stop(apply_bundle(ledger_fx, ledger_p), "D: ledger-only drift refuses", "ledger|chain")
  ok(identical(file_hash(ledger_fx$gate), base_record_hash) &&
       identical(file_hash(ledger_fx$ledger), ledger_hash),
     "D: ledger-only drift writes nothing")

  signed_base <- make_fixture(); sbp <- preview(signed_base)
  write_raw(signed_base$gate, read_raw(file.path(sbp$out, "candidate.bin")))
  alt <- .sign_candidate_bytes(read_raw(signed_base$gate), "approved", "OTHER", "2026-09-11",
                               signed_base$gate)
  sb_state <- no_write_state(signed_base)
  expect_stop(with_wd(signed_base$root, approve_staged_gate(
                signed_base$gate, alt, file_hash(signed_base$gate), "absent",
                check_store = FALSE)),
              "D: signed normal base refuses direct API", "unsigned|normal base")
  ok(identical(sb_state, no_write_state(signed_base)), "D: signed normal-base refusal writes nothing")

  rejected <- make_fixture(); rlines <- readLines(rejected$gate, warn = FALSE)
  hit <- grep("^- \\[ \\] REJECTED", rlines)
  rlines[hit] <- sub("\\[ \\]", "[x]", rlines[hit]); writeLines(rlines, rejected$gate, useBytes = TRUE)
  rej_candidate <- .sign_candidate_bytes(read_raw(rejected$gate), "approved", "Fixture Reviewer", "2026-09-11",
                                         rejected$gate)
  rej_state <- no_write_state(rejected)
  expect_stop(with_wd(rejected$root, approve_staged_gate(
                rejected$gate, rej_candidate, file_hash(rejected$gate), "absent",
                check_store = FALSE)),
              "D: rejected normal base refuses direct API", "rejected|unsigned|normal base")
  ok(identical(rej_state, no_write_state(rejected)), "D: rejected normal-base refusal writes nothing")

  stale <- make_fixture(); stale_p <- preview(stale); stale_state <- no_write_state(stale)
  expect_stop(with_wd(stale$root, approve_staged_gate(
                stale$gate, read_raw(file.path(stale_p$out, "candidate.bin")),
                stale_p$result$request$base_record,
                stale_p$result$request$base_ledger, check_store = TRUE)),
              "D: production freshness is enforced by staged API", "analysis script|store")
  ok(identical(stale_state, no_write_state(stale)), "D: stale-store refusal writes nothing")

  held <- make_fixture(); held_p <- preview(held); held_state <- no_write_state(held)
  lock <- paste0(held$ledger, ".lock"); dir.create(lock); writeLines("writer: fixture", file.path(lock, "holder"))
  expect_stop(apply_bundle(held, held_p), "D: held lock refuses", "holding the lock")
  ok(identical(held_state, no_write_state(held)), "D: held-lock refusal writes nothing")
  unlink(lock, recursive = TRUE)
  stale_lock <- make_fixture(); slp <- preview(stale_lock); sl_state <- no_write_state(stale_lock)
  lock <- paste0(stale_lock$ledger, ".lock"); dir.create(lock); writeLines("writer: fixture", file.path(lock, "holder"))
  Sys.setFileTime(lock, Sys.time() - GATE_RELEASE_LOCK_STALE_SECONDS - 5)
  expect_stop(apply_bundle(stale_lock, slp), "D: stale lock refuses without breaking it", "older than|not broken")
  ok(dir.exists(lock) && identical(sl_state, no_write_state(stale_lock)),
     "D: stale lock remains and transaction writes nothing")
  unlink(lock, recursive = TRUE)

  for (label in c(" sMe ", "THE   SME")) {
    role <- make_fixture()
    untouched <- no_write_state(role)
    expect_stop(preview(role, signer = label),
                "SIGN-ROLE-REFUSAL: preview refuses reserved signer", "reserved role label")
    ok(identical(no_write_state(role), untouched) &&
         !file.exists(file.path(dirname(role$gate), "approval-bundle")),
       "SIGN-ROLE-REFUSAL: refused preview creates no bundle or ledger")
    valid <- preview(role, signer = "Fixture Reviewer")
    candidate <- .split_record_bytes(read_raw(file.path(valid$out, "candidate.bin")), "test")
    candidate$lines[grep("^Signed:", candidate$lines)] <- paste0("Signed: ", label)
    forged <- .join_record_bytes(candidate)
    p_role <- parse_gate_record(role$gate, lines = candidate$lines)
    ok(identical(p_role$decision, "approved") &&
         identical(p_role$package_digest, p_role$digest_signed),
       "SIGN-ROLE-REFUSAL: staged refusal fixture has a bound approving package")
    expect_stop(with_wd(role$root, approve_staged_gate(
      role$gate, forged, valid$result$request$base_record, valid$result$request$base_ledger,
      check_store = FALSE)), "SIGN-ROLE-REFUSAL: staged apply rejects a reserved signer",
      "reserved role label")
    ok(identical(no_write_state(role), untouched),
       "SIGN-ROLE-REFUSAL: refused staged apply leaves both files untouched")
    write_raw(role$gate, forged)
    forged_state <- no_write_state(role)
    expect_stop(with_wd(role$root, require_gate(role$gate)),
                "SIGN-ROLE-REFUSAL: runtime rejects a reserved signer", "reserved role label")
    expect_stop(with_wd(role$root, approve_gate(role$gate, check_store = FALSE)),
                "SIGN-ROLE-REFUSAL: direct approval rejects a reserved signer", "reserved role label")
    ok(identical(no_write_state(role), forged_state),
       "SIGN-ROLE-REFUSAL: direct approval writes no gate or ledger bytes")
  }

  cat("\n--- E: interruption, failure, and exact retry semantics ---\n")
  rename_fx <- make_fixture(); rename_p <- preview(rename_fx); rename_state <- no_write_state(rename_fx)
  expect_stop(with_wd(rename_fx$root, .approve_staged_gate_impl(
    rename_fx$gate, read_raw(file.path(rename_p$out, "candidate.bin")),
    rename_p$result$request$base_record, rename_p$result$request$base_ledger,
    check_store = FALSE, hooks = list(before_rename = function(...) stop("injected rename failure")))),
    "E: injected rename failure refuses", "rename failure")
  ok(identical(rename_state, no_write_state(rename_fx)), "E: rename failure changes neither file")

  staged_fx <- make_fixture(); staged_p <- preview(staged_fx); staged_state <- no_write_state(staged_fx)
  expect_stop(with_wd(staged_fx$root, .approve_staged_gate_impl(
    staged_fx$gate, read_raw(file.path(staged_p$out, "candidate.bin")),
    staged_p$result$request$base_record, staged_p$result$request$base_ledger,
    check_store = FALSE, hooks = list(after_stage = function(...) stop("injected staging interruption")))),
    "E: interruption after staging refuses", "staging interruption")
  ok(identical(staged_state, no_write_state(staged_fx)),
     "E: interruption before rename changes neither file")

  record_stage_fx <- make_fixture(); record_stage_p <- preview(record_stage_fx)
  record_stage_candidate <- read_raw(file.path(record_stage_p$out, "candidate.bin"))
  record_stage_bytes <- NULL
  expect_stop(with_wd(record_stage_fx$root, .approve_staged_gate_impl(
    record_stage_fx$gate, record_stage_candidate,
    record_stage_p$result$request$base_record,
    record_stage_p$result$request$base_ledger, check_store = FALSE,
    hooks = list(after_stage = function(tmp, gate) {
      record_stage_bytes <<- c(read_raw(gate), charToRaw("concurrent body edit\n"))
      write_raw(gate, record_stage_bytes)
    }))), "E: regular live-record drift after staging refuses before rename",
    "record|drift|snapshot")
  ok(!is.null(record_stage_bytes) &&
       identical(read_raw(record_stage_fx$gate), record_stage_bytes) &&
       !file.exists(record_stage_fx$ledger),
     "E: after-stage record refusal preserves concurrent bytes and appends no event")

  ledger_stage_fx <- make_fixture(); ledger_stage_p <- preview(ledger_stage_fx)
  ledger_stage_base <- read_raw(ledger_stage_fx$gate)
  ledger_stage_bytes <- charToRaw("foreign concurrent ledger bytes\n")
  expect_stop(with_wd(ledger_stage_fx$root, .approve_staged_gate_impl(
    ledger_stage_fx$gate, read_raw(file.path(ledger_stage_p$out, "candidate.bin")),
    ledger_stage_p$result$request$base_record,
    ledger_stage_p$result$request$base_ledger, check_store = FALSE,
    hooks = list(after_stage = function(tmp, gate) {
      write_raw(ledger_stage_fx$ledger, ledger_stage_bytes)
    }))), "E: ledger drift after staging refuses before rename", "ledger|drift")
  ok(identical(read_raw(ledger_stage_fx$gate), ledger_stage_base) &&
       identical(read_raw(ledger_stage_fx$ledger), ledger_stage_bytes),
     "E: after-stage ledger refusal preserves concurrent ledger and base record")

  temp_stage_fx <- make_fixture(); temp_stage_p <- preview(temp_stage_fx)
  temp_stage_base <- read_raw(temp_stage_fx$gate)
  temp_stage_tamper <- charToRaw("tampered staged bytes\n")
  expect_stop(with_wd(temp_stage_fx$root, .approve_staged_gate_impl(
    temp_stage_fx$gate, read_raw(file.path(temp_stage_p$out, "candidate.bin")),
    temp_stage_p$result$request$base_record,
    temp_stage_p$result$request$base_ledger, check_store = FALSE,
    hooks = list(after_stage = function(tmp, gate) write_raw(tmp, temp_stage_tamper)))),
    "E: staged temporary-file tamper refuses before rename", "staged|candidate|drift")
  ok(identical(read_raw(temp_stage_fx$gate), temp_stage_base) &&
       !file.exists(temp_stage_fx$ledger),
     "E: staged-temp refusal preserves base record and appends no event")

  after_rename_fx <- make_fixture(); arp <- preview(after_rename_fx)
  after_rename_candidate <- read_raw(file.path(arp$out, "candidate.bin"))
  expect_stop(with_wd(after_rename_fx$root, .approve_staged_gate_impl(
    after_rename_fx$gate, after_rename_candidate,
    arp$result$request$base_record, arp$result$request$base_ledger,
    check_store = FALSE,
    hooks = list(after_rename = function(...) stop("injected post-rename interruption")))),
    "E: interruption immediately after rename is fail-loud", "post-rename interruption")
  ok(identical(read_raw(after_rename_fx$gate), after_rename_candidate) &&
       !file.exists(after_rename_fx$ledger),
     "E: post-rename interruption leaves candidate visible and ledger unchanged")
  apply_bundle(after_rename_fx, arp)
  ok(length(readLines(after_rename_fx$ledger, warn = FALSE)) == 1L,
     "E: post-rename interruption retries through candidate-equal branch")

  if (.Platform$OS.type == "unix") {
    rename_link_fx <- make_fixture(); rename_link_p <- preview(rename_link_fx)
    rename_link_candidate <- read_raw(file.path(rename_link_p$out, "candidate.bin"))
    rename_link_target <- file.path(dirname(rename_link_fx$gate), "rename-link-target.md")
    write_raw(rename_link_target, rename_link_candidate)
    rename_link_target_before <- file_hash(rename_link_target)
    expect_stop(with_wd(rename_link_fx$root, .approve_staged_gate_impl(
      rename_link_fx$gate, rename_link_candidate,
      rename_link_p$result$request$base_record,
      rename_link_p$result$request$base_ledger, check_store = FALSE,
      hooks = list(after_rename = function(gate, ...) {
        unlink(gate)
        if (!file.symlink(rename_link_target, gate)) stop("fixture symlink failed")
      }))), "E: candidate-equal symlink swap after rename refuses", "symlink|regular")
    ok(is_symlink(rename_link_fx$gate) &&
         identical(file_hash(rename_link_target), rename_link_target_before) &&
         !file.exists(rename_link_fx$ledger),
       "E: after-rename symlink refusal preserves target and appends no event")
  }

  append_fx <- make_fixture(); append_p <- preview(append_fx)
  append_candidate <- read_raw(file.path(append_p$out, "candidate.bin"))
  expect_stop(with_wd(append_fx$root, .approve_staged_gate_impl(
    append_fx$gate, append_candidate,
    append_p$result$request$base_record, append_p$result$request$base_ledger,
    check_store = FALSE, hooks = list(before_append = function(...) stop("injected append failure")))),
    "E: append failure is fail-loud", "append failure")
  ok(identical(read_raw(append_fx$gate), append_candidate) && !file.exists(append_fx$ledger),
     "E: append failure leaves signed unledgered record visible")
  retry_event <- apply_bundle(append_fx, append_p)
  ok(length(readLines(append_fx$ledger, warn = FALSE)) == 1L &&
       identical(retry_event$record_digest, digest_raw(append_candidate)),
     "E: retained-bundle retry appends once after rename-complete interruption")
  completed_bytes <- read_raw(append_fx$ledger); completed_event <- apply_bundle(append_fx, append_p)
  ok(identical(read_raw(append_fx$ledger), completed_bytes) &&
       identical(completed_event$event_id, retry_event$event_id),
     "E: exact completed retry succeeds without another append")

  completed_record_fx <- make_fixture(); completed_record_p <- preview(completed_record_fx)
  completed_record_candidate <- read_raw(file.path(completed_record_p$out, "candidate.bin"))
  apply_bundle(completed_record_fx, completed_record_p)
  completed_record_ledger <- read_raw(completed_record_fx$ledger)
  completed_record_drift <- NULL
  expect_stop(with_wd(completed_record_fx$root, .approve_staged_gate_impl(
    completed_record_fx$gate, completed_record_candidate,
    completed_record_p$result$request$base_record,
    completed_record_p$result$request$base_ledger, check_store = FALSE,
    hooks = list(before_completed_return = function(gate, ledger_path) {
      completed_record_drift <<- c(read_raw(gate),
                                   charToRaw("concurrent completed-record edit\n"))
      write_raw(gate, completed_record_drift)
    }))), "E: completed retry refuses final record drift", "record|drift|snapshot")
  ok(!is.null(completed_record_drift) &&
       identical(read_raw(completed_record_fx$gate), completed_record_drift) &&
       identical(read_raw(completed_record_fx$ledger), completed_record_ledger),
     "E: completed-retry record refusal preserves concurrent bytes and ledger")

  completed_ledger_fx <- make_fixture(); completed_ledger_p <- preview(completed_ledger_fx)
  completed_ledger_candidate <- read_raw(file.path(completed_ledger_p$out, "candidate.bin"))
  apply_bundle(completed_ledger_fx, completed_ledger_p)
  completed_ledger_record <- read_raw(completed_ledger_fx$gate)
  completed_ledger_drift <- NULL
  expect_stop(with_wd(completed_ledger_fx$root, .approve_staged_gate_impl(
    completed_ledger_fx$gate, completed_ledger_candidate,
    completed_ledger_p$result$request$base_record,
    completed_ledger_p$result$request$base_ledger, check_store = FALSE,
    hooks = list(before_completed_return = function(gate, ledger_path) {
      completed_ledger_drift <<- c(read_raw(ledger_path),
                                   charToRaw("concurrent completed-ledger edit\n"))
      write_raw(ledger_path, completed_ledger_drift)
    }))), "E: completed retry refuses final ledger drift", "ledger|drift")
  ok(!is.null(completed_ledger_drift) &&
       identical(read_raw(completed_ledger_fx$gate), completed_ledger_record) &&
       identical(read_raw(completed_ledger_fx$ledger), completed_ledger_drift),
     "E: completed-retry ledger refusal preserves record and concurrent bytes")

  with_wd(append_fx$root, revoke_gate(append_fx$gate, "fixture revocation"))
  revoked_state <- no_write_state(append_fx)
  expect_stop(apply_bundle(append_fx, append_p),
              "E: later revocation makes retained bundle retry refuse", "ledger|state|retry")
  ok(identical(revoked_state, no_write_state(append_fx)),
     "E: revocation refusal preserves record and ledger")

  if (.Platform$OS.type == "unix") {
    append_link_fx <- make_fixture(); append_link_p <- preview(append_link_fx)
    append_link_candidate <- read_raw(file.path(append_link_p$out, "candidate.bin"))
    append_link_target <- file.path(dirname(append_link_fx$gate), "append-link-target.md")
    write_raw(append_link_target, append_link_candidate)
    append_link_target_before <- file_hash(append_link_target)
    expect_stop(with_wd(append_link_fx$root, .approve_staged_gate_impl(
      append_link_fx$gate, append_link_candidate,
      append_link_p$result$request$base_record,
      append_link_p$result$request$base_ledger, check_store = FALSE,
      hooks = list(before_append = function(gate, ...) {
        unlink(gate)
        if (!file.symlink(append_link_target, gate)) stop("fixture symlink failed")
      }))), "E: symlink swap immediately before append refuses", "symlink|regular")
    ok(is_symlink(append_link_fx$gate) &&
         identical(file_hash(append_link_target), append_link_target_before) &&
         !file.exists(append_link_fx$ledger),
       "E: pre-append symlink refusal preserves target and appends no event")
  }

  existing <- make_fixture(); first <- preview(existing, name = "first-bundle")
  apply_bundle(existing, first)
  with_wd(existing$root, release_gate(existing$gate, "fixture re-version", "Fixture Reviewer"))
  second <- preview(existing, signer = "MM2", name = "second-bundle")
  apply_bundle(existing, second)
  existing_ledger <- read_raw(existing$ledger)
  existing_retry <- apply_bundle(existing, second)
  ok(length(readLines(existing$ledger, warn = FALSE)) == 3L &&
       identical(read_raw(existing$ledger), existing_ledger) &&
       identical(existing_retry$signer, "MM2"),
     "E: completed retry recognizes exact non-empty ledger prefix plus one event")
  ordinary_before <- length(readLines(clean$ledger, warn = FALSE))
  expect_stop(with_wd(clean$root, approve_gate(clean$gate, check_store = FALSE)),
              "E: ordinary approve_gate duplicate remains a refusal", "idempotence|identical latest")
  ok(length(readLines(clean$ledger, warn = FALSE)) == ordinary_before,
     "E: ordinary duplicate refusal appends nothing")

  cat("\n--- F: CLI strictness and CI registration parser ---\n")
  cli_fx <- make_fixture(); cli_out <- file.path(dirname(cli_fx$gate), "cli-bundle")
  cli_signer <- shQuote("Fixture Reviewer")
  cli_before <- no_write_state(cli_fx)
  cli <- with_wd(cli_fx$root, suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    c(command_path, "preview", cli_fx$gate, "--decision", "approved",
      "--signer", cli_signer, "--date", "2026-09-11", "--out", cli_out),
    stdout = TRUE, stderr = TRUE)))
  cli_status <- attr(cli, "status"); if (is.null(cli_status)) cli_status <- 0L
  ok(cli_status == 1L && any(grepl("analysis script not found:", cli, fixed = TRUE)) &&
       !file.exists(cli_out) && identical(no_write_state(cli_fx), cli_before),
     "F: CLI preview always enforces freshness and creates no stale bundle")
  cli_bad <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
                     c(command_path, "preview", cli_fx$gate, "--decision", "approved",
                       "--decision", "approved", "--signer", cli_signer,
                       "--date", "2026-09-11", "--out", cli_out),
                     stdout = TRUE, stderr = TRUE))
  bad_status <- attr(cli_bad, "status"); if (is.null(bad_status)) bad_status <- 0L
  ok(bad_status == 2L && any(grepl("duplicate", cli_bad, ignore.case = TRUE)),
     "F: CLI parser refuses duplicated options before workflow code")

  cli_ok <- make_fixture(); dir.create(file.path(cli_ok$root, "analyses"))
  writeLines(c(
    "library(targets)",
    "list(",
    "  tar_target(artifact_file, 'artifact.txt', format = 'file'),",
    "  tar_target(eda_package, list(",
    "    artifacts = data.frame(artifact = 'synthetic', path = artifact_file, source = 'fixture', stringsAsFactors = FALSE),",
    "    tables = list(Synthetic = data.frame(value = 'fixture', stringsAsFactors = FALSE)),",
    "    notes = 'fixture only'))",
    ")"), file.path(cli_ok$root, "analyses", "TST-001.R"), useBytes = TRUE)
  with_wd(cli_ok$root, suppressMessages(targets::tar_make(
    script = file.path("analyses", "TST-001.R"), store = "_targets_TST-001",
    reporter = "silent")))
  cli_ok_out <- file.path(dirname(cli_ok$gate), "cli-ok-bundle")
  cli_preview <- with_wd(cli_ok$root, system2(
    file.path(R.home("bin"), "Rscript"),
    c(command_path, "preview", cli_ok$gate, "--decision", "approved_with_changes",
      "--signer", "CLI-SME", "--date", "2026-09-11", "--out", cli_ok_out),
    stdout = TRUE, stderr = TRUE))
  preview_status <- attr(cli_preview, "status"); if (is.null(preview_status)) preview_status <- 0L
  token_line <- grep("^confirm-token: ", cli_preview, value = TRUE)
  cli_token <- if (length(token_line) == 1L) sub("^confirm-token: ", "", token_line) else ""
  cli_apply <- if (preview_status == 0L && nzchar(cli_token)) with_wd(
    cli_ok$root, system2(file.path(R.home("bin"), "Rscript"),
      c(command_path, "apply", cli_ok_out, "--gate", cli_ok$gate, "--confirm", cli_token),
      stdout = TRUE, stderr = TRUE)) else structure("preview failed", status = 1L)
  apply_status <- attr(cli_apply, "status"); if (is.null(apply_status)) apply_status <- 0L
  cli_parsed <- parse_gate_record(cli_ok$gate)
  ok(preview_status == 0L && apply_status == 0L &&
       identical(cli_parsed$decision, "approved_with_changes") &&
       identical(cli_parsed$signer, "CLI-SME") &&
       length(readLines(cli_ok$ledger, warn = FALSE)) == 1L,
     "F: real CLI preview/apply succeeds against a fresh synthetic store")

  ci_env <- new.env(parent = globalenv()); sys.source(file.path(TOOLS, "ci_check.R"), envir = ci_env)
  tabled <- vapply(get(".CI_BATTERIES", envir = ci_env), `[[`, character(1), "file")
  wf_path <- file.path(REPO, ".github", "workflows", "ci.yaml")
  wf <- yaml::yaml.load_file(wf_path)
  aggregate <- 'R_LIBS_SITE="$R_LIBS_USER" Rscript tools/ci_check.R'
  has_aggregate <- function(job) {
    sum(vapply(job$steps, function(step) identical(step$run, aggregate), logical(1L))) == 1L
  }
  full <- c("full-battery", "validated-stack")
  ok(sum(tabled == "test_sign_approve_gate.R") == 1L &&
       all(vapply(wf$jobs[full], has_aggregate, logical(1L))),
     "F: local focused battery runs through the aggregate in both full jobs")
  scratch <- tempfile("sign-workflow-")
  dir.create(file.path(scratch, "tools", "tests"), recursive = TRUE)
  dir.create(file.path(scratch, ".github", "workflows"), recursive = TRUE)
  for (name in tabled) writeLines("# fixture", file.path(scratch, "tools", "tests", name))
  wf$jobs[[full[[2L]]]]$steps <- Filter(function(step) !identical(step$run, aggregate),
                                        wf$jobs[[full[[2L]]]]$steps)
  yaml::write_yaml(wf, file.path(scratch, ".github", "workflows", "ci.yaml"))
  report <- with_wd(scratch, capture.output(ci_env$check_battery_registration()))
  ok(any(grepl("^FAIL battery registration inventory.*validated-stack operational commands", report)),
     "F: production registration check detects removal of the validated aggregate")

  cat(sprintf("\n%d passed, %d failed\n", passed, failed))
  invisible(failed)
}

if (.invoked_as_script()) {
  failures <- .run_sign_approve_tests()
  quit(status = if (failures) 1L else 0L)
}
