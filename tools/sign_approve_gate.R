#!/usr/bin/env Rscript
# Preview and apply one explicit SME gate-sign and approval transaction.
# Preview never edits the gate. Apply delegates the locked replacement and
# ledger append to approve_staged_gate().

.sign_approve_tools_dir <- function() {
  fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  own <- fa[basename(sub("^--file=", "", fa)) == "sign_approve_gate.R"]
  if (length(own))
    return(normalizePath(dirname(sub("^--file=", "", own[1])), mustWork = TRUE))
  normalizePath("tools", mustWork = TRUE)
}

local({
  td <- .sign_approve_tools_dir()
  if (!exists("require_gate", mode = "function")) source(file.path(td, "gate.R"))
  if (!exists("render_gate_package", mode = "function"))
    source(file.path(td, "gate_package.R"))
  if (!exists("approve_staged_gate", mode = "function"))
    source(file.path(td, "approve_gate.R"))
})

.SIGN_APPROVE_VERSION <- 1L
.SIGN_APPROVE_FIELDS <- c("format_version", "destination", "base_record",
                          "base_ledger", "candidate", "package_digest")

.sign_approve_sha256 <- function(bytes) paste0(
  "sha256:", digest::digest(bytes, algo = "sha256", serialize = FALSE))

.sign_approve_normal_gate <- function(path, who) {
  .approval_read_raw(path, who)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

.sign_approve_request_json <- function(request) {
  q <- function(x) paste0('"', .json_escape(as.character(x)), '"')
  paste0(
    '{"format_version":', as.integer(request$format_version),
    ',"destination":', q(request$destination),
    ',"base_record":', q(request$base_record),
    ',"base_ledger":', q(request$base_ledger),
    ',"candidate":', q(request$candidate),
    ',"package_digest":', q(request$package_digest), "}\n")
}

# Domain-separated, tagged and byte-length-prefixed encoding. No delimiter can
# move bytes between fields because both tag and value carry their byte length.
.sign_approve_token <- function(request) {
  out <- c(charToRaw("pmx-harness/sign-approve-gate"), as.raw(0),
           charToRaw(as.character(.SIGN_APPROVE_VERSION)), as.raw(0))
  for (field in .SIGN_APPROVE_FIELDS) {
    value <- if (identical(field, "format_version"))
      as.character(as.integer(request[[field]])) else as.character(request[[field]])
    tag <- charToRaw(field); val <- charToRaw(value)
    out <- c(out, charToRaw(paste0(length(tag), ":")), tag,
             charToRaw(paste0(length(val), ":")), val)
  }
  .sign_approve_sha256(out)
}

.sign_approve_validate_request <- function(request) {
  if (!identical(sort(names(request)), sort(.SIGN_APPROVE_FIELDS)))
    stop("apply: request.json has missing or unexpected fields.", call. = FALSE)
  if (!is.numeric(request$format_version) || length(request$format_version) != 1L ||
      is.na(request$format_version) ||
      as.integer(request$format_version) != .SIGN_APPROVE_VERSION)
    stop("apply: unsupported request format_version.", call. = FALSE)
  for (field in setdiff(.SIGN_APPROVE_FIELDS, "format_version"))
    if (!is.character(request[[field]]) || length(request[[field]]) != 1L ||
        is.na(request[[field]]) || !nzchar(request[[field]]))
      stop("apply: request field '", field, "' is missing or malformed.", call. = FALSE)
  if (!grepl("^sha256:[0-9a-f]{64}$", request$base_record) ||
      !(identical(request$base_ledger, .APPROVAL_LEDGER_ABSENT) ||
        grepl("^sha256:[0-9a-f]{64}$", request$base_ledger)) ||
      !grepl("^sha256:[0-9a-f]{64}$", request$candidate) ||
      !grepl("^canon2:sha256:[0-9a-f]{64}$", request$package_digest))
    stop("apply: request digest fields are malformed.", call. = FALSE)
  request$format_version <- .SIGN_APPROVE_VERSION
  request
}

.sign_approve_read_request <- function(bundle) {
  path <- file.path(bundle, "request.json")
  bytes <- .approval_read_raw(path, "apply")
  text <- tryCatch(rawToChar(bytes), error = function(e)
    stop("apply: request.json is not valid text: ", conditionMessage(e), call. = FALSE))
  request <- tryCatch(.json_parse_flat(text), error = function(e)
    stop("apply: request.json cannot be parsed: ", conditionMessage(e), call. = FALSE))
  request <- .sign_approve_validate_request(request)
  if (!identical(bytes, charToRaw(.sign_approve_request_json(request))))
    stop("apply: request.json is not the exact canonical request written by preview; ",
         "refusing changed or ambiguous metadata.", call. = FALSE)
  request
}

.sign_approve_private_write <- function(path, bytes) {
  link <- Sys.readlink(path)
  if (file.exists(path) || (length(link) && !is.na(link[1]) && nzchar(link[1])))
    stop("preview: refusing to overwrite bundle payload ", path, ".", call. = FALSE)
  writeBin(bytes, path)
  Sys.chmod(path, "0600")
  back <- .approval_read_raw(path, "preview")
  if (!identical(back, bytes))
    stop("preview: bundle payload did not read back byte-for-byte: ", path,
         call. = FALSE)
  mode <- as.integer(file.info(path)$mode) %% 512L
  if (!identical(mode, strtoi("600", base = 8L)))
    stop("preview: bundle payload is not mode 0600: ", path, call. = FALSE)
  invisible(path)
}

.sign_candidate_bytes <- function(base_bytes, decision, signer, date, gate_file) {
  if (!decision %in% .LEDGER_APPROVING)
    stop("preview: --decision must be approved or approved_with_changes.", call. = FALSE)
  if (!is.character(signer) || length(signer) != 1L || is.na(signer) ||
      !nzchar(trimws(signer)) || grepl("[\r\n]", signer) ||
      grepl("Date:|<!--|-->", signer))
    stop("preview: --signer must be one non-empty line.", call. = FALSE)
  if (.gate_reserved_signer(signer))
    stop("preview: --signer uses a reserved role label; enter the local ",
         "reviewer's name.", call. = FALSE)
  if (!is.character(date) || length(date) != 1L || is.na(date) ||
      !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", date))
    stop("preview: --date must use YYYY-MM-DD.", call. = FALSE)
  parsed_date <- suppressWarnings(as.Date(date))
  if (is.na(parsed_date) || format(parsed_date, "%Y-%m-%d") != date)
    stop("preview: --date is not a valid calendar date.", call. = FALSE)

  parts <- .split_record_bytes(base_bytes, "preview")
  if (!identical(.join_record_bytes(parts), base_bytes))
    stop("preview: record byte split did not round-trip.", call. = FALSE)
  layout <- .approval_signature_layout(parts$lines, "preview")
  parsed <- parse_gate_record(gate_file, lines = parts$lines)
  if (is.na(parsed$package_digest) ||
      !grepl("^canon2:sha256:[0-9a-f]{64}$", parsed$package_digest))
    stop("preview: record lacks one modern canon2 Package-Digest.", call. = FALSE)
  lines <- parts$lines
  choice <- layout[[decision]]
  lines[choice] <- sub("\\[[ xX]\\]", "[x]", lines[choice])
  indent <- function(line) sub("^(\\s*).*", "\\1", line)
  lines[layout$signed] <- paste0(indent(lines[layout$signed]), "Signed: ", signer)
  lines[layout$date] <- paste0(indent(lines[layout$date]), "Date: ", date)
  lines[layout$digest] <- paste0(indent(lines[layout$digest]),
                                 "Digest signed: ", parsed$package_digest)
  .join_record_bytes(parts, lines)
}

.sign_approve_bundle_path <- function(out, gate_file) {
  if (!is.character(out) || length(out) != 1L || is.na(out) || !nzchar(out))
    stop("preview: --out must name a new sibling bundle directory.", call. = FALSE)
  link <- Sys.readlink(out)
  if (file.exists(out) || (length(link) && !is.na(link[1]) && nzchar(link[1])))
    stop("preview: --out already exists or is a symlink; a new path is required: ",
         out, call. = FALSE)
  parent <- normalizePath(dirname(out), winslash = "/", mustWork = TRUE)
  gate_parent <- normalizePath(dirname(gate_file), winslash = "/", mustWork = TRUE)
  name <- basename(out)
  if (!identical(parent, gate_parent) || name %in% c(".", "..", "history") ||
      grepl("\\.md$|^(approvals|errata)\\.jsonl$", name, ignore.case = TRUE))
    stop("preview: --out must be a non-gate sibling directory inside ", gate_parent,
         ".", call. = FALSE)
  file.path(parent, name)
}

.sign_approve_preview <- function(gate_file, decision, signer, date, out,
                                  check_store = TRUE) {
  gate <- .sign_approve_normal_gate(gate_file, "preview")
  base_snap <- .approval_gate_snapshot(gate, "preview")
  base_parts <- .split_record_bytes(base_snap$bytes, "preview")
  base <- c(base_snap, list(parts = base_parts))
  candidate_bytes <- .sign_candidate_bytes(base_snap$bytes, decision, signer, date, gate)
  candidate <- .approval_assert_candidate(gate, candidate_bytes, "preview")
  if (!identical(candidate$parsed$decision, decision) ||
      !identical(candidate$parsed$signer, signer) ||
      !identical(candidate$parsed$date, date))
    stop("preview: candidate did not preserve the explicit SME inputs exactly.",
         call. = FALSE)
  .approval_assert_normal_transition(gate, base, candidate, "preview")
  ledger <- .approval_ledger_snapshot(gate, "preview")
  .approval_ledger_events(ledger, gate, "preview")
  if (check_store) require_fresh_store(.gate_analysis_id(gate), .gate_task_key(gate))

  bundle <- .sign_approve_bundle_path(out, gate)
  request <- list(
    format_version = .SIGN_APPROVE_VERSION,
    destination = gate,
    base_record = base_snap$digest,
    base_ledger = ledger$digest,
    candidate = candidate$digest,
    package_digest = candidate$parsed$package_digest)
  token <- .sign_approve_token(request)

  if (!dir.create(bundle, mode = "0700", showWarnings = FALSE))
    stop("preview: could not create private bundle directory ", bundle, ".",
         call. = FALSE)
  keep <- FALSE
  on.exit(if (!keep && dir.exists(bundle)) unlink(bundle, recursive = TRUE), add = TRUE)
  Sys.chmod(bundle, "0700")
  if ((as.integer(file.info(bundle)$mode) %% 512L) != strtoi("700", base = 8L))
    stop("preview: bundle directory is not mode 0700: ", bundle, call. = FALSE)
  .sign_approve_private_write(file.path(bundle, "candidate.bin"), candidate_bytes)
  .sign_approve_private_write(file.path(bundle, "request.json"),
                              charToRaw(.sign_approve_request_json(request)))
  keep <- TRUE

  cat("bundle: ", bundle, "\n",
      "destination: ", request$destination, "\n",
      "base-record: ", request$base_record, "\n",
      "base-ledger: ", request$base_ledger, "\n",
      "Package-Digest: ", request$package_digest, "\n",
      "candidate: ", request$candidate, "\n",
      "confirm-token: ", token, "\n",
      "--- candidate.bin ---\n", sep = "")
  cat(rawToChar(candidate_bytes))
  if (!length(candidate_bytes) || candidate_bytes[length(candidate_bytes)] != as.raw(0x0a))
    cat("\n")
  cat("--- end candidate.bin ---\n")
  invisible(list(bundle = bundle, request = request, confirm_token = token))
}

.sign_approve_apply <- function(bundle, gate, confirm, check_store = TRUE) {
  if (!is.character(bundle) || length(bundle) != 1L || is.na(bundle) ||
      !dir.exists(bundle) || (!is.na(Sys.readlink(bundle)[1]) &&
                              nzchar(Sys.readlink(bundle)[1])))
    stop("apply: bundle must be an existing non-symlink directory.", call. = FALSE)
  bundle <- normalizePath(bundle, winslash = "/", mustWork = TRUE)
  if ((as.integer(file.info(bundle)$mode) %% 512L) != strtoi("700", base = 8L))
    stop("apply: bundle directory is not private mode 0700.", call. = FALSE)
  if (!identical(sort(list.files(bundle, all.files = TRUE, no.. = TRUE)),
                 c("candidate.bin", "request.json")))
    stop("apply: bundle must contain exactly candidate.bin and request.json payloads.",
         call. = FALSE)
  for (name in c("candidate.bin", "request.json")) {
    path <- file.path(bundle, name)
    .approval_read_raw(path, "apply")
    if ((as.integer(file.info(path)$mode) %% 512L) != strtoi("600", base = 8L))
      stop("apply: bundle payload is not private mode 0600: ", path, call. = FALSE)
  }
  destination <- .sign_approve_normal_gate(gate, "apply")
  request <- .sign_approve_read_request(bundle)
  if (!identical(destination, request$destination))
    stop("apply: supplied --gate destination does not match the confirmed request.",
         call. = FALSE)
  candidate_bytes <- .approval_read_raw(file.path(bundle, "candidate.bin"), "apply")
  candidate_hash <- .sign_approve_sha256(candidate_bytes)
  if (!identical(candidate_hash, request$candidate))
    stop("apply: candidate.bin hash differs from the confirmed candidate hash.",
         call. = FALSE)
  token <- .sign_approve_token(request)
  if (!is.character(confirm) || length(confirm) != 1L || is.na(confirm) ||
      !identical(confirm, token))
    stop("apply: --confirm token does not match candidate and transaction context.",
         call. = FALSE)
  candidate <- .approval_assert_candidate(destination, candidate_bytes, "apply")
  if (!identical(candidate$parsed$package_digest, request$package_digest))
    stop("apply: candidate Package-Digest differs from the confirmed request.",
         call. = FALSE)
  approve_staged_gate(destination, candidate_bytes, request$base_record,
                      request$base_ledger, check_store = check_store)
}

.sign_approve_usage <- function(message) {
  structure(list(message = message, call = NULL),
            class = c("sign_approve_usage", "error", "condition"))
}

.sign_approve_parse <- function(args, command, positionals, flags) {
  if (!length(args) || !identical(args[1], command))
    stop(.sign_approve_usage(paste0(command, ": internal command parse mismatch")))
  rest <- args[-1]; values <- list()
  for (name in positionals) {
    if (!length(rest) || startsWith(rest[1], "--"))
      stop(.sign_approve_usage(paste0(command, ": missing ", name)))
    values[[name]] <- rest[1]; rest <- rest[-1]
  }
  while (length(rest)) {
    flag <- rest[1]
    if (!startsWith(flag, "--") || !flag %in% flags)
      stop(.sign_approve_usage(paste0(command, ": unknown argument '", flag, "'")))
    if (!is.null(values[[flag]]))
      stop(.sign_approve_usage(paste0(command, ": duplicate flag '", flag, "'")))
    if (length(rest) < 2L || startsWith(rest[2], "--"))
      stop(.sign_approve_usage(paste0(command, ": flag '", flag, "' requires a value")))
    values[[flag]] <- rest[2]; rest <- rest[-(1:2)]
  }
  for (flag in flags) if (is.null(values[[flag]]))
    stop(.sign_approve_usage(paste0(command, ": missing required flag '", flag, "'")))
  values
}

sign_approve_main <- function(args = commandArgs(trailingOnly = TRUE)) {
  if (!length(args))
    stop(.sign_approve_usage("missing preview or apply command"))
  if (identical(args[1], "preview")) {
    x <- .sign_approve_parse(args, "preview", "gate_file",
                             c("--decision", "--signer", "--date", "--out"))
    return(.sign_approve_preview(x$gate_file, x[["--decision"]], x[["--signer"]],
                                 x[["--date"]], x[["--out"]], check_store = TRUE))
  }
  if (identical(args[1], "apply")) {
    x <- .sign_approve_parse(args, "apply", "bundle", c("--gate", "--confirm"))
    return(.sign_approve_apply(x$bundle, x[["--gate"]], x[["--confirm"]],
                               check_store = TRUE))
  }
  stop(.sign_approve_usage(paste0("unknown command '", args[1], "'")))
}

.sign_approve_invoked_as_script <- function() {
  fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  length(fa) > 0L && identical(basename(sub("^--file=", "", fa[1])),
                               "sign_approve_gate.R")
}

if (.sign_approve_invoked_as_script()) {
  result <- tryCatch({ sign_approve_main(); NULL }, error = identity)
  if (inherits(result, "error")) {
    cat(conditionMessage(result), "\n", sep = "")
    if (inherits(result, "sign_approve_usage")) {
      cat("usage:\n",
          "  sign_approve_gate.R preview <gate-file> --decision <approved|approved_with_changes> --signer <text> --date <YYYY-MM-DD> --out <bundle-directory>\n",
          "  sign_approve_gate.R apply <bundle-directory> --gate <gate-file> --confirm <token>\n",
          sep = "")
      quit(status = 2L)
    }
    quit(status = 1L)
  }
  quit(status = 0L)
}
