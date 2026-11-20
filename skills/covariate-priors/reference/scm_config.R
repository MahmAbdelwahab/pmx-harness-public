# REFERENCE IMPLEMENTATION — PsN scm configuration generator
#
# AUTHORITY: skills/covariate-priors/MANIFEST.yaml is the authority for the SME signature
# (root reviewed_by / reviewed_at) and for the status and digest of unit `scm_config`.
# This header carries no status claim of its own -- read the manifest.
# PsN configuration reference: https://github.com/UUPharmacometrics/PsN/tree/master/doc
#
# The structural control (SKILL_SCM.md): the scm config is GENERATED from
# the MAP's frozen prior list — [test_relations] is that list verbatim, so
# a covariate not on the list cannot enter the search. Agents never
# hand-write configs at analysis time; the generated config is a gate
# artifact reviewed before scm runs.
#
# prior_list: named list, one element per parameter, each a data.frame
#   with columns:
#     covariate  column name in the dataset
#     type       "continuous" | "categorical"
#     form       "power" (house default) | "linear" | "exponential" |
#                "none" — the declared functional form for the PAIR;
#                departures from the house default must match the MAP.
# Example:
#   list(CL = data.frame(covariate = c("WT","EGFR"),
#                        type = "continuous",
#                        form = c("power","power")),
#        V  = data.frame(covariate = "SEXF", type = "categorical",
#                        form = "linear"))
#
# PsN state numbers: 1 none, 2 linear, 4 exponential, 5 power (house
# continuous default); categorical uses 1,2. State 3 (piecewise) and
# custom states are per-analysis MAP declarations, added via `extra_code`.

SCM_CONFIG_SKILL_FILE <- "skills/covariate-priors/SKILL_SCM.md"

# ---------------------------------------------------------------------------
# Self-gate (integrity batch 3a): the scm config generator COMPUTES the config
# a covariate search runs on, so its computing entry points are gated the same
# way the psn-covsearch and simulation lanes are. `unit` is fixed to
# "scm_config"; the signed scope of covariate-priors is covariate_search-only,
# so scm_config is a draft unit and refuses post-cutover: the gate computes
# only when skills/covariate-priors/MANIFEST.yaml carries a confirmed root
# (non-empty reviewed_by) and unit scm_config is approved. The legacy
# "Reviewed by:" grep of SKILL_SCM.md was retired in batch 3c and is no longer
# consulted.
# --- the private, locked validator environment (integrity batch 11, R10-P0-1)
# The self-gate below used to call whatever `require_validated_method` was
# visible on the search path, sourcing tools/method_manifest.R only if none
# was. A caller could therefore bind `require_validated_method <- function(...)
# TRUE` before sourcing this file and every draft unit computed. The validator
# is now loaded ONCE into a private environment whose parent is the base
# environment, and the environment is locked with its bindings; the gate is a
# closure over that environment, so nothing a caller binds -- before or after
# sourcing -- changes what the gate calls.
#
# THE HANDOVER, stated where the construction is written. Every executable
# name the construction reaches -- local, new.env, baseenv, file.exists, stop,
# sys.source and lockEnvironment, and since integrity batch 13 the `c` of
# every path vector, the `local` of every gate and accessor closure, and the
# heads of the pre-seal write -- is written with the exact `base::`
# qualification, and the block's own frame is supplied explicitly as
# `envir = base::new.env(parent = base::baseenv())`, so neither the frame nor
# its parent is the environment this file is sourced into. A caller who
# pre-binds a fake `sys.source`, `new.env`, `c` or `local` there is not
# consulted (integrity batches 12 and 13, Codex rounds 11 and 12). What
# remains is a caller who rebinds the `::` operator or R's syntax heads
# (`{`, `(`, `<-`, `if`, `for`, `!`, `function`) in that environment, or
# modifies the base namespace itself: the same class of act as modifying R.
# Same-process reflection -- a caller reaching a sealed environment through
# `environment(fn)`, `parent.env<-` or an active binding -- is out of scope
# of the method guard: its environment protects against ordinary
# rebinding, not against a hostile caller in the same process; the controls
# there are the trusted loader (tools/load_reference.R), the manifest
# digests, the write guard and the SME's review of the diff.
.mm_trusted_private <- base::local({
  e <- base::new.env(parent = base::baseenv())
  if (!base::file.exists("tools/method_manifest.R"))
    base::stop("this reference requires tools/method_manifest.R; references are ",
               "sourced with the repository root as the working directory.",
               call. = FALSE)
  base::sys.source("tools/method_manifest.R", envir = e, keep.source = FALSE)
  base::lockEnvironment(e, bindings = TRUE)
  e
}, envir = base::new.env(parent = base::baseenv()))

scm_config_validated <- base::local({
  private <- .mm_trusted_private
  function() {
    base::get("require_validated_method", envir = private)(
      skill_dir    = "skills/covariate-priors",
      calling_file = "skills/covariate-priors/reference/scm_config.R",
      unit         = "scm_config")
  }
}, envir = base::new.env())

.scm_config_gate <- function() {
  if (!scm_config_validated()) {
    base::stop("covariate-priors unit 'scm_config' is not SME-validated (either the ",
         "manifest unit is draft/rejected, or skills/covariate-priors/MANIFEST.yaml ",
         "has an empty root reviewed_by); the scm config ",
         "generator does not run until its unit is approved. ESCALATE to the ",
         "SME.", call. = FALSE)
  }
  base::invisible(TRUE)
}

# Maps a declared (type, form) pair to its PsN state number (catalog in the
# header). Categorical pairs accept only "linear" or "none" — house rule,
# enforced here so a MAP typo cannot silently request an unsupported state.
# Returns an integer scalar; stops on any form outside the declared set.
# C-locale numeric rendering for machine-consumed config values (F11, F15,
# Codex round 8). base::format() honours getOption("OutDec") and can emit a
# locale decimal comma, corrupting a PsN option line. formatC() always uses
# the C locale (decimal point), so a config generated under any locale is
# byte-identical. "g" with high precision preserves the declared value.
.scm_fmt_num <- function(x) formatC(x, format = "g", digits = 15, width = 1)

# The house default significance level declared in SKILL_SCM.md, read from the
# skill (not hardcoded here) so the ceiling below tracks the signed guidance.
# The declaration lives in the options table as a row like
#   | `p_forward=` | 0.05 | house default ... MAP may tighten, never loosen |
.scm_house_p <- function(param, path = SCM_CONFIG_SKILL_FILE) {
  if (!file.exists(path))
    stop("scm_config: cannot read the house default for ", param,
         "; SKILL file missing: ", path, call. = FALSE)
  txt <- readLines(path, warn = FALSE)
  row <- grep(sprintf("\\|\\s*`%s=`\\s*\\|", param), txt, value = TRUE)
  if (length(row) != 1L)
    stop("scm_config: could not find exactly one declared house default for ",
         param, " in ", path, call. = FALSE)
  cells <- strsplit(row[1], "\\|")[[1]]
  val <- suppressWarnings(as.numeric(trimws(cells[3])))
  if (!is.finite(val))
    stop("scm_config: the declared house default for ", param, " in ", path,
         " is not a finite number.", call. = FALSE)
  val
}

# The significance level may TIGHTEN (smaller p) but never LOOSEN (larger p)
# relative to the house default (SKILL_SCM.md, "MAP may tighten, never
# loosen"). A looser threshold admits more relations and is refused (F11).
.scm_check_p_ceiling <- function(param, value) {
  house <- .scm_house_p(param)
  if (value > house)
    stop(param, " = ", .scm_fmt_num(value), " is LOOSER than the house ",
         "default (", .scm_fmt_num(house), ", SKILL_SCM.md): a covariate ",
         "search may tighten the significance level (smaller p) but never ",
         "loosen it. Lower ", param, " to ", .scm_fmt_num(house), " or ",
         "below, or escalate to change the declared default.", call. = FALSE)
  invisible(TRUE)
}

.scm_state_for <- function(type, form) {
  if (type == "categorical") {
    if (!form %in% c("linear", "none")) {
      stop("Categorical covariate form must be 'linear' (fractional ",
           "change per level, house default) or 'none'; got '", form, "'.",
           call. = FALSE)
    }
    # PsN state numbers (header catalog): 'none' is state 1, 'linear' is
    # state 2. The old code mapped BOTH to state 2 (F11, Codex round 8),
    # which silently promoted a declared 'none' pair into a tested linear
    # relation. 'none' must map to state 1.
    return(if (form == "none") 1L else 2L)
  }
  switch(form,
         power       = 5L,
         linear      = 2L,
         exponential = 4L,
         none        = 1L,
         stop("Unknown functional form '", form, "'. Declared forms: ",
              "power, linear, exponential, none.", call. = FALSE))
}

# Round 9, P2-7. `extra_options` was appended verbatim after the options the
# generator emits itself, so a second `p_forward=` or `search_direction=` line
# could contradict a validated declaration and PsN's resolution of the
# duplicate is unknown. The keys this generator owns are refused in
# extra_options; they are declared through the function's own parameters.
SCM_RESERVED_OPTIONS <- c("model", "directory", "search_direction", "p_forward",
                          "p_backward", "continuous_covariates",
                          "categorical_covariates", "missing_data_token",
                          "do_not_drop", "logit", "parallel_states",
                          "nm_version")
.scm_check_extra_options <- function(extra_options) {
  if (is.null(extra_options) || !length(extra_options)) return(invisible(NULL))
  keys <- names(extra_options)
  if (is.null(keys)) keys <- rep("", length(extra_options))
  # An unnamed entry is a raw "key=value" line.
  raw <- vapply(as.character(unlist(extra_options)), function(x) x, character(1),
                USE.NAMES = FALSE)
  for (i in seq_along(keys)) {
    # Round 9 fix round (review S3): trimws() strips OUTER whitespace only, so
    # splitting before trimming left the inner space in " search_direction = both "
    # and the reserved comparison missed it. Whitespace is removed from the key
    # entirely -- around it and inside it -- before the comparison; a PsN option
    # line is ordinarily written either way.
    k <- if (nzchar(keys[i])) keys[i] else sub("=.*$", "", raw[i])
    k <- gsub("[[:space:]]", "", k)
    if (k %in% SCM_RESERVED_OPTIONS)
      stop("extra_options carries the reserved key '", k, "', which this ",
           "generator emits itself from a validated parameter; a duplicate option line would ",
           "contradict the declared search and PsN's resolution of the ",
           "duplicate is not established. Declare it through the function ",
           "parameter instead.", call. = FALSE)
  }
  invisible(NULL)
}

# Structural check of the frozen prior list before any config is written:
# each element must be a data.frame with covariate/type/form columns and
# every type continuous or categorical. Stops on the first violation;
# returns TRUE invisibly. The list's content (which covariates) is never
# second-guessed here — it is the MAP's, verbatim.
.scm_validate_prior_list <- function(prior_list) {
  stopifnot(is.list(prior_list), length(names(prior_list)) > 0)
  for (par in names(prior_list)) {
    df <- prior_list[[par]]
    need <- c("covariate", "type", "form")
    if (!is.data.frame(df) || !all(need %in% names(df))) {
      stop("prior_list$", par, " must be a data.frame with columns: ",
           paste(need, collapse = ", "), call. = FALSE)
    }
    bad <- setdiff(unique(df$type), c("continuous", "categorical"))
    if (length(bad)) {
      stop("prior_list$", par, ": unknown covariate type(s): ",
           paste(bad, collapse = ", "), call. = FALSE)
    }
  }
  invisible(TRUE)
}

# Supply the NONMEM version label configured in the site psn.conf.
# The project declares the label in run-config.yaml.
.scm_check_nm_version <- function(nm_version) {
  if (is.null(nm_version) || length(nm_version) != 1L || is.na(nm_version) ||
      !nzchar(trimws(as.character(nm_version)))) {
    stop("nm_version is required and must be a non-empty site psn.conf label ",
         "(e.g. \"nm751\"), not a NONMEM version number. PsN resolves it ",
         "against psn.conf and refuses an unknown name at option parsing; run ",
         "`psn -nm_versions` to list this site's labels, and record the chosen ",
         "one in the analysis run-config.yaml.", call. = FALSE)
  }
  invisible(TRUE)
}

# PsN's scm inserts covariate code around a TYPICAL-VALUE variable named
# TV<PARAM>. A control stream that computes the parameter directly is not
# scm-usable: scm stops with "Could not determine a good place to add the
# covariate relation. i.e. No TVCL was found" (live PsN 5.7.1, 2026-09-04).
# The harness's own parity control stream is exactly that shape — it writes
# `CL = EXP(THETA(1)+0.75*LWT+ETA(1))` with no TVCL — so this precondition is
# not hypothetical, and the generator checked nothing.
#
# The check is a string check on the $PK block only. A TVCL in a comment or in
# $ERROR is not a typical-value assignment scm can work with, and crediting it
# would be precisely the "does X appear somewhere" presence scan this
# repository has been burned by. TV-parameterised $PK is the ONLY route the
# generator accepts (SME decision 3, 2026-09-04): the refusal used to advertise
# an explicit [code] section as an alternative, but this check runs before
# extra_code is considered and [code] is appended only afterwards, so the
# promise was one the API could not honour. `extra_code` remains what it is —
# an appended section — and is not a substitute for the TV precondition.
.scm_check_tv_parameterization <- function(control_stream, parameters) {
  if (is.null(control_stream) || length(control_stream) != 1L ||
      is.na(control_stream) || !nzchar(control_stream) ||
      !file.exists(control_stream)) {
    shown <- if (is.null(control_stream)) "<none>" else
      paste(as.character(control_stream), collapse = ", ")
    stop("the control_stream (", shown, ") cannot be ",
         "read. PsN's scm requires a TV<PARAM> typical-value line per searched ",
         "parameter, and that precondition is checked against the actual ",
         "control stream, never assumed.", call. = FALSE)
  }
  txt <- readLines(control_stream, warn = FALSE)
  # Strip NONMEM comments (everything from an unquoted ';') before looking for
  # assignments, then keep only the $PK block.
  txt <- sub(";.*$", "", txt)
  starts <- grep("^\\s*\\$", txt)
  pk <- integer(0)
  for (i in starts) {
    if (!grepl("^\\s*\\$PK\\b", txt[i], ignore.case = TRUE)) next
    nxt <- starts[starts > i]
    end <- if (length(nxt)) nxt[1] - 1L else length(txt)
    pk <- c(pk, seq.int(i, end))
  }
  if (!length(pk)) {
    stop("the control stream ", control_stream, " has no $PK block, so no ",
         "TV<PARAM> typical-value line can be found for the searched ",
         "parameter(s). PsN's scm cannot insert covariate code into it.",
         call. = FALSE)
  }
  block <- txt[pk]
  # R10-P2-1, part 1: validate the identifier before it is used at all. The
  # check below matches literally, but a parameter name is also going to be
  # written into a NONMEM variable name, so anything that is not a NONMEM-shaped
  # identifier is a stop rather than something to escape and hope about.
  bad_id <- parameters[!grepl("^[A-Za-z][A-Za-z0-9_]*$", parameters)]
  if (length(bad_id))
    stop("searched parameter name(s) ", paste(bad_id, collapse = ", "),
         " are not valid NONMEM identifiers (^[A-Za-z][A-Za-z0-9_]*$). A ",
         "parameter name becomes the variable TV<PARAM> in the control ",
         "stream; a name that cannot be one is refused rather than matched ",
         "approximately.", call. = FALSE)

  # R10-P2-1, part 2. The old check was the unescaped presence regex
  # `^\s*TV<p>\s*=`, so a name carrying a regex metacharacter could match a
  # different variable, and `TVCL = ...` passed even when CL never consumed
  # TVCL. Two string checks on the parsed $PK lines, no interpretation of the
  # arithmetic: the block must ASSIGN TV<P>, and the assignment of <P> itself
  # must USE TV<P>. That is the shape scm reparameterizes around
  # (TVCL = EXP(...); CL = TVCL*EXP(ETA(1))).
  lhs_is <- function(line, nm)
    identical(trimws(sub("=.*$", "", line)), nm) && grepl("=", line, fixed = TRUE)
  # A whole-word occurrence of `nm`, found by literal splitting rather than by
  # a regex built from the name.
  uses <- function(txt, nm) {
    toks <- strsplit(txt, "[^A-Za-z0-9_]+")[[1]]
    nm %in% toks
  }
  assigns_tv <- vapply(parameters, function(p)
    any(vapply(block, lhs_is, logical(1), nm = paste0("TV", p))), logical(1))
  consumes_tv <- vapply(parameters, function(p)
    any(vapply(block[vapply(block, lhs_is, logical(1), nm = p)],
               uses, logical(1), nm = paste0("TV", p))), logical(1))

  missing <- parameters[!assigns_tv]
  if (length(missing)) {
    stop("the control stream ", control_stream, " has no TV", missing[1],
         " assignment in its $PK block, so PsN's scm has nowhere to add the ",
         "covariate relation for ", missing[1], " and stops with \"Could not ",
         "determine a good place to add the covariate relation. i.e. No TV",
         missing[1], " was found\". Searched parameter(s) without a ",
         "typical-value line: ", paste(missing, collapse = ", "),
         ". Reparameterize $PK into TV<PARAM> form (algebraically identical: ",
         "TVCL = EXP(...); CL = TVCL*EXP(ETA(1))). See SKILL_SCM.md.",
         call. = FALSE)
  }
  # SME decision 3 (2026-09-04): TV-parameterised $PK is the ONLY route. The
  # old refusal advertised "or supply an explicit [code] section", which the
  # API could not honour -- this check runs before extra_code is considered and
  # [code] is appended only afterwards. `extra_code` remains what it is, an
  # appended section; it is not a substitute for the TV precondition, and the
  # promise has been removed from both refusals.
  unconsumed <- parameters[assigns_tv & !consumes_tv]
  if (length(unconsumed)) {
    p <- unconsumed[1]
    stop("the control stream ", control_stream, " assigns TV", p, " in its ",
         "$PK block but the assignment of ", p, " does not use TV", p,
         ", so the typical-value line is not on the path that computes ", p,
         " and covariate code inserted around TV", p, " would not reach it. ",
         "Searched parameter(s) with an unconsumed typical value: ",
         paste(unconsumed, collapse = ", "),
         ". Reparameterize $PK into TV<PARAM> form (TV", p, " = EXP(...); ",
         p, " = TV", p, "*EXP(ETA(1))). See SKILL_SCM.md.", call. = FALSE)
  }
  invisible(TRUE)
}

# Forward-search config. Returns the config text (character vector, one
# line each); the caller writes it to file and attaches it to the gate
# package.
# House defaults encoded in the signature: p_forward = 0.05 and missing
# token -99. Stops (via the validator) on a malformed prior list. Line
# order below is load-bearing: PsN requires every single-line option to
# precede the first [bracket] section, so extra_options is emitted before
# [test_relations].
#
# control_stream  path to the control stream this config will run against.
#                 Required, and actually read: PsN's scm needs a TV<PARAM>
#                 typical-value line per searched parameter, and the harness's
#                 own parity control stream does not have one.
# nm_version      the site's psn.conf label (e.g. "nm751"), required — see
#                 .scm_check_nm_version. Not a NONMEM version number.
# do_not_drop     covariates to protect from $INPUT DROP, ADDITIONAL to every
#                 covariate on the prior list, which is protected
#                 automatically. Setting do_not_drop through extra_options is
#                 refused; it is a reserved key.
reference_scm_config <- function(model, prior_list,
                                 directory,
                                 control_stream,
                                 nm_version,
                                 p_forward = 0.05,
                                 missing_token = -99,
                                 do_not_drop = character(0),
                                 logit = character(0),
                                 parallel_states = 0L,
                                 extra_options = character(0),
                                 extra_code = character(0)) {
  .scm_config_gate()
  .scm_validate_prior_list(prior_list)
  .scm_check_p_ceiling("p_forward", p_forward)
  .scm_check_extra_options(extra_options)
  .scm_check_nm_version(nm_version)
  all_pairs <- do.call(rbind, lapply(names(prior_list), function(p) {
    cbind(parameter = p, prior_list[[p]])
  }))
  # Round 9, P2-7 (SME decision 2026-09-04): every pair is routed through
  # .scm_state_for(), and a pair whose declared prior form is 'none' is
  # EXCLUDED from [test_relations] rather than emitted with state 1. The
  # generator previously emitted every declared covariate as a tested
  # relation, so the mapping existed but nothing called it.
  all_pairs$scm_state <- vapply(seq_len(nrow(all_pairs)),
    function(i) .scm_state_for(all_pairs$type[i], all_pairs$form[i]), integer(1))
  tested <- all_pairs[all_pairs$scm_state != 1L, , drop = FALSE]
  cont <- unique(tested$covariate[tested$type == "continuous"])
  cat_ <- unique(tested$covariate[tested$type == "categorical"])

  # PsN's TVxx precondition, checked against the control stream this config
  # will be run against, for every parameter actually searched.
  .scm_check_tv_parameterization(control_stream, unique(tested$parameter))

  # Round 10, V2-D. scm marks every covariate a given candidate does not use
  # as =DROP in that candidate's $INPUT. When the structural model consumes a
  # covariate outside any covariate relation — the harness base model does,
  # with the fixed allometry LWT = LOG(WT/1.30) — the dropped candidate fails
  # NMTRAN with "208 UNDEFINED VARIABLE", and PsN records it as FAILED 999,
  # does not restart it, and CONTINUES THE SEARCH with exit status 0. A
  # candidate model is lost silently. do_not_drop existed as a parameter and
  # nothing ever tied it to the covariates the model consumes, so every
  # covariate on the prior list is now protected — including one excluded by a
  # prior of 'none', which is still present in $INPUT and still droppable.
  do_not_drop <- unique(c(as.character(do_not_drop),
                          as.character(all_pairs$covariate)))

  # House valid_states: continuous none->power; categorical none->linear.
  # Per-pair departures from the house default are pinned in [code], not
  # by widening the global state list (SKILL_SCM.md).
  code_lines <- character(0)
  for (i in seq_len(nrow(tested))) {
    r <- tested[i, ]
    if (r$type == "continuous" && !r$form %in% c("power", "none")) {
      code_lines <- c(code_lines,
                      sprintf("%s:%s-5=%s", r$parameter, r$covariate, r$form))
    }
  }

  lines <- c(
    "; GENERATED by skills/covariate-priors/reference/scm_config.R —",
    "; do not edit by hand. [test_relations] is the MAP's frozen prior",
    "; list; the config is a gate artifact (SKILL_SCM.md).",
    sprintf("model=%s", model),
    sprintf("directory=%s", directory),
    "search_direction=forward",
    sprintf("p_forward=%s", .scm_fmt_num(p_forward)),
    if (length(cont)) sprintf("continuous_covariates=%s",
                              paste(cont, collapse = ",")),
    if (length(cat_)) sprintf("categorical_covariates=%s",
                              paste(cat_, collapse = ",")),
    sprintf("missing_data_token=%s", missing_token),
    if (length(do_not_drop)) sprintf("do_not_drop=%s",
                                     paste(do_not_drop, collapse = ",")),
    if (length(logit)) sprintf("logit=%s", paste(logit, collapse = ",")),
    sprintf("parallel_states=%d", as.integer(parallel_states)),
    sprintf("nm_version=%s", nm_version),
    extra_options,
    "",
    "[test_relations]",
    unlist(lapply(unique(tested$parameter), function(p) {
      sprintf("%s=%s", p,
              paste(tested$covariate[tested$parameter == p], collapse = ","))
    })),
    "",
    "[valid_states]",
    "continuous = 1,5",
    "categorical = 1,2",
    if (length(code_lines) || length(extra_code)) c("", "[code]",
                                                    code_lines, extra_code)
  )
  # Skipped conditional entries return NULL and vanish inside c(); the NA
  # filter is a final guard so no literal "NA" line reaches the config file.
  lines[!is.na(lines)]
}

# Backward-elimination config from a forward result.
# included_relations: named list parameter -> character vector of
#   "COV-state" tokens the forward step retained (e.g. c("WT-5","EGFR-5")),
#   read from the forward scm logfile by the analyst and reviewed at the
#   gate together with that logfile.
# code_pins: character vector pinning non-default parameterizations the
#   included relations used (e.g. "CL:AGE-2=power"), per the backward
#   template's requirement.
# House default p_backward = 0.01. Hard stops: a parameter not on the prior
# list, or a token naming a covariate off that parameter's prior list —
# either means the tokens were mis-transcribed from the forward logfile, so
# the config is refused rather than repaired. control_stream and nm_version
# are required and carry the same meaning as in the forward generator.
reference_scm_config_backward <- function(model, prior_list,
                                          included_relations,
                                          directory,
                                          control_stream,
                                          nm_version,
                                          p_backward = 0.01,
                                          missing_token = -99,
                                          code_pins = character(0),
                                          extra_options = character(0)) {
  .scm_config_gate()
  .scm_validate_prior_list(prior_list)
  .scm_check_p_ceiling("p_backward", p_backward)
  .scm_check_nm_version(nm_version)
  # R10-P1-3. The backward generator ran the p and version checks but never the
  # reserved-key check, then appended extra_options verbatim -- so
  # `extra_options = "search_direction=forward"` or `"p_backward=0.05"` could
  # contradict the validated declaration this function is generating. Checked
  # BEFORE any line is constructed, exactly as the forward generator does.
  .scm_check_extra_options(extra_options)
  stopifnot(is.list(included_relations),
            length(names(included_relations)) > 0)
  unknown_par <- setdiff(names(included_relations), names(prior_list))
  if (length(unknown_par)) {
    stop("included_relations name parameter(s) not on the prior list: ",
         paste(unknown_par, collapse = ", "), call. = FALSE)
  }
  for (p in names(included_relations)) {
    # strip the "-<state>" suffix to recover bare covariate names for the
    # prior-list membership check
    covs <- sub("-[0-9]+$", "", included_relations[[p]])
    off_list <- setdiff(covs, prior_list[[p]]$covariate)
    if (length(off_list)) {
      stop("included_relations$", p, " carries covariate(s) not on the ",
           "prior list for ", p, ": ", paste(off_list, collapse = ", "),
           ". The forward result cannot contain them; check the tokens.",
           call. = FALSE)
    }
  }
  all_pairs <- do.call(rbind, lapply(names(prior_list), function(p) {
    cbind(parameter = p, prior_list[[p]])
  }))
  cont <- unique(all_pairs$covariate[all_pairs$type == "continuous"])
  cat_ <- unique(all_pairs$covariate[all_pairs$type == "categorical"])

  # Same two preconditions as the forward step: scm reparameterizes around
  # TV<PARAM> in the backward direction too, and every covariate on the prior
  # list is protected from $INPUT DROP.
  .scm_check_tv_parameterization(control_stream, names(included_relations))
  do_not_drop <- unique(as.character(all_pairs$covariate))

  lines <- c(
    "; GENERATED by skills/covariate-priors/reference/scm_config.R —",
    "; backward step from the reviewed forward result (SKILL_SCM.md).",
    sprintf("model=%s", model),
    sprintf("directory=%s", directory),
    "search_direction=backward",
    sprintf("p_backward=%s", .scm_fmt_num(p_backward)),
    if (length(cont)) sprintf("continuous_covariates=%s",
                              paste(cont, collapse = ",")),
    if (length(cat_)) sprintf("categorical_covariates=%s",
                              paste(cat_, collapse = ",")),
    sprintf("missing_data_token=%s", missing_token),
    if (length(do_not_drop)) sprintf("do_not_drop=%s",
                                     paste(do_not_drop, collapse = ",")),
    sprintf("nm_version=%s", nm_version),
    extra_options,
    "",
    "[test_relations]",
    vapply(names(prior_list), function(p) {
      sprintf("%s=%s", p, paste(prior_list[[p]]$covariate, collapse = ","))
    }, character(1)),
    "",
    "[valid_states]",
    "continuous = 1,5",
    "categorical = 1,2",
    if (length(code_pins)) c("", "[code]", code_pins),
    "",
    "[included_relations]",
    vapply(names(included_relations), function(p) {
      sprintf("%s=%s", p, paste(included_relations[[p]], collapse = ","))
    }, character(1))
  )
  lines[!is.na(lines)]
}
