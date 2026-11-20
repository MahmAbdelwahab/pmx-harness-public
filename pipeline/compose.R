# Pipeline composition (Layer 2, harness-owned).
#
# An analysis is NOT assumed to be a full MIDD lifecycle. It declares the
# tasks it actually needs — "EDA and simulation" is a complete analysis if
# that is what was asked for — and the harness composes a pipeline from
# those task modules only.
#
# Division of ownership:
#   harness (this file)  gate targets, chaining, validation. Generated, never
#                        agent-authored, so the human-in-the-loop guarantee
#                        does not depend on an agent remembering it.
#   sub-agent            the task modules in pipeline/tasks/<task>.R — the
#                        actual work, grown as the analysis develops and
#                        reviewed by the SME as a diff at the task's gate.
#
# A task module returns a list of targets and declares its dependencies on
# other tasks. It must not create gate targets; the composer does that.
#
# Engineering reference for authoring task modules and extending this
# composer (branching, batching, target factories, tar_manifest checks):
# .claude/skills/targets-pipelines/SKILL.md (vendored, MIT; see
# docs/reference/README.md for attribution and scope).

TASK_DIR <- "pipeline/tasks"

# Validate a declared project input before constructing its file target.
task_input_file <- function(task, field, paths, single = FALSE) {
  valid <- is.character(paths) && length(paths) && !anyNA(paths) &&
    (!single || length(paths) == 1L) &&
    all(nzchar(paths)) && !any(grepl("(^|/)[.][.](/|$)|^[/~]|^[A-Za-z]:", paths)) &&
    !any(grepl(intToUtf8(92), paths, fixed = TRUE))
  if (!valid || any(!file_test("-f", paths)))
    stop(task, ": ", field, " must name an existing project-relative file: ",
         paste(paths, collapse = ", "), call. = FALSE)
  root <- paste0(normalizePath("."), "/")
  if (any(!startsWith(normalizePath(paths, winslash = "/", mustWork = TRUE), root)))
    stop(task, ": ", field, " must stay within the project root.", call. = FALSE)
  invisible(paths)
}

# Name of the composed file target over skills/*/MANIFEST.yaml (batch 4, item
# D). Every stage target's command names it; verify_gate_dominance asserts it.
MANIFEST_TARGET <- "method_manifests"

# Name of the environment-attestation target (round 6 finding 6; round 7
# finding 10). Always cued; its value is a STABLE fingerprint of the probed
# environment (tools/check_environment.R::attest_environment). EVERY other
# composed target — stage, package, gate file, gate, method_manifests — names
# it in its command, so the environment dominates the whole DAG; because the
# value is stable, downstream stays valid while nothing changed and is
# invalidated exactly when the environment changes. verify_gate_dominance
# asserts the edge for every non-attestation target. (The earlier `env_check`
# target — default cue, gates only — is replaced, not aliased.)
ENV_TARGET <- "environment_attestation"

# Registry of known tasks: which role owns it, which skill it consumes, and
# which other tasks it requires when they are present in the analysis.
# `needs` feeds the environment check: package names are literal;
# "estimation_engine" / "simulation_engine" are resolved to packages in
# compose_pipeline() based on inputs$engine.
task_registry <- function() {
  list(
    baseline_input = list(role = "data-assembler", skill = "baseline-fit", requires = character(0),
                          needs = character(0)),
    baseline_fit   = list(role = "modeler", skill = "baseline-fit", requires = "baseline_input",
                          needs = "estimation_engine"),
    data_assembly = list(role = "data-assembler",      skill = "data-assembly",       requires = character(0),
                         needs = "yspec"),
    eda           = list(role = "exploratory-analyst", skill = "eda-standards",       requires = "data_assembly",
                         needs = c("pmtables", "pmplots")),
    base_model    = list(role = "modeler",             skill = "model-management",    requires = "data_assembly",
                         needs = "estimation_engine"),
    covariate     = list(role = "covariate-analyst",   skill = "covariate-priors",    requires = "base_model",
                         needs = "estimation_engine"),
    qualification = list(role = "model-qualifier",     skill = "diagnostics",         requires = "base_model",
                         needs = c("estimation_engine", "pmplots")),
    simulation    = list(role = "simulator",           skill = "simulation-standards",requires = character(0),
                         needs = c("simulation_engine", "estimation_engine")),
    reporting     = list(role = "report-writer",       skill = "reporting-standards", requires = character(0),
                         needs = c("tarchetypes", "quarto")),
    review        = list(role = "reviewer",            skill = "review-checklist",    requires = character(0),
                         needs = character(0))
  )
}

# Canonical gate-record location: gates/<analysis>/<task>.md, with
# underscores dashed in the file name.
gate_file_for <- function(analysis_id, task) {
  file.path("gates", analysis_id, paste0(gsub("_", "-", task), ".md"))
}

# Resolves the configured NONMEM provision (finding M6b) from the house
# engine config so the environment check probes what the machine actually
# uses. engines.nonmem.provision in config/engine.yaml is the operational
# default (a per-run run-config.yaml may override it at execution, but the
# pipeline-level environment check reflects the committed house default).
# Falls back to "containerized" when the file or key is absent, matching
# tools/nonmem_exec.sh and check_environment()'s own default.
resolve_nonmem_provision <- function(default = "config/engine.yaml") {
  p <- NULL
  if (file.exists(default)) {
    cfg <- tryCatch(yaml::read_yaml(default), error = function(e) NULL)
    p <- cfg$engines$nonmem$provision
  }
  if (is.null(p) || !nzchar(p)) "containerized" else p
}

# Composes the pipeline for one analysis.
#   analysis_id  identifier, also the gates/ subdirectory
#   tasks        character vector of task names, in the order the analysis
#                runs them (declared in the analysis scope file)
#   inputs       named list passed to each task module as `task_input`
#   attest_tasks tasks the environment check is computed over; default
#                `tasks`. An analysis that appends tasks gate by gate names
#                every planned task here so the attestation, which every
#                target names, does not change when a task is appended.
#                Must include every declared task.
# Returns a list of targets: environment_attestation first, then each task's own targets
# followed by two generated ones — gate_<task>_file (the gate record as a
# file target, so editing/signing it invalidates the gate) and gate_<task>,
# which calls require_gate() and blocks everything downstream until signed.
compose_pipeline <- function(analysis_id, tasks, inputs = list(),
                             attest_tasks = tasks) {
  reg <- task_registry()
  # Baseline submission has one explicit backend and an actual upstream package.
  if ("baseline_fit" %in% tasks) {
    if (!identical(inputs$engine, "nonmem"))
      stop("baseline_fit requires explicit inputs$engine = nonmem.", call. = FALSE)
    if (!"baseline_input" %in% tasks || !is.null(inputs$baseline_fit$external_input))
      stop("baseline_fit requires the baseline_input task without external replacement.", call. = FALSE)
  }
  unknown <- setdiff(tasks, names(reg))
  if (length(unknown)) {
    stop("Unknown tasks: ", paste(unknown, collapse = ", "),
         "\nKnown tasks: ", paste(names(reg), collapse = ", "), call. = FALSE)
  }
  unknown_att <- setdiff(attest_tasks, names(reg))
  if (length(unknown_att)) {
    stop("Unknown tasks in attest_tasks: ", paste(unknown_att, collapse = ", "),
         "\nKnown tasks: ", paste(names(reg), collapse = ", "), call. = FALSE)
  }
  omitted_att <- setdiff(tasks, attest_tasks)
  if (length(omitted_att)) {
    stop("attest_tasks omits declared task(s): ",
         paste(omitted_att, collapse = ", "),
         ". The environment check must cover every task the analysis runs.",
         call. = FALSE)
  }
  if (!is.null(inputs$base_model$covariate_comparisons) && "base_model" %in% tasks &&
      !"weights" %in% names(inputs$data_assembly))
    stop("base_model: covariate_comparisons requires data_assembly$weights.", call. = FALSE)
  if ("simulation" %in% tasks && identical(inputs$simulation$engine, "nonmem") &&
      (!all(c("data_assembly", "base_model", "qualification") %in% tasks) ||
       !all(c("weights", "design_constants") %in% names(inputs$data_assembly))))
    stop("simulation: the NONMEM path requires data_assembly with weights and design_constants, ",
         "base_model, and qualification.", call. = FALSE)

  # Dependencies are enforced only among tasks actually included: a task
  # whose prerequisite is absent from this analysis must say where its input
  # comes from (an earlier analysis, or an SME-supplied artifact).
  #
  # EXACTLY ONE SOURCE (round 6, finding 4). The rule is symmetric: a task
  # whose prerequisite is absent MUST declare external_input, and a task whose
  # prerequisite is PRESENT must NOT. Previously only the first half was
  # checked, while task modules select the external branch whenever
  # external_input happens to be set — so an analysis could include
  # data_assembly and silently have EDA read a different dataset. Both
  # declarations at once is an ambiguity the composer cannot resolve, so it
  # stops and names both sources for the SME to choose between.
  for (tk in tasks) {
    missing_dep <- setdiff(reg[[tk]]$requires, tasks)
    present_dep <- intersect(reg[[tk]]$requires, tasks)
    has_external <- !is.null(inputs[[tk]]$external_input)
    if (length(missing_dep) && !has_external) {
      stop("Task '", tk, "' requires ", paste(missing_dep, collapse = ", "),
           ", which this analysis does not include. Declare ",
           "inputs$", tk, "$external_input to state where its input comes from.",
           call. = FALSE)
    }
    if (length(present_dep) && has_external) {
      stop("Task '", tk, "' has two declared input sources and the composer ",
           "cannot choose between them: this analysis includes the upstream ",
           "task(s) ", paste(present_dep, collapse = ", "),
           ", AND declares inputs$", tk, "$external_input = ",
           paste(format(inputs[[tk]]$external_input), collapse = " "),
           ". Name exactly one source: drop the external_input to consume the ",
           "upstream task's output, or drop the upstream task from `tasks` to ",
           "read the external dataset.", call. = FALSE)
    }
  }

  # The environment check covers only what the declared tasks actually need:
  # an EDA-and-simulation analysis must not demand NONMEM or PsN. Computed
  # over attest_tasks (default: tasks).
  needs <- unique(unlist(lapply(attest_tasks, function(tk) reg[[tk]]$needs)))

  # Engine resolution (finding M6). Analyses declare the estimation engine
  # inside a task's input block (e.g. inputs$base_model$engine), not only at
  # top level, so a per-task NONMEM declaration must NOT be overridden by a
  # nlmixr2 top-level default. Documented precedence, per task:
  #   1. inputs[[task]]$engine  — the task's own declaration wins for it.
  #   2. inputs$engine          — top-level default for tasks without one.
  #   3. "nlmixr2"              — final default.
  # The environment check must satisfy EVERY engine any declared task will
  # use, so the package/provision needs below are the UNION across all
  # declared engines, never a single top-level choice.
  top_engine <- inputs$engine %||% "nlmixr2"
  task_engine <- function(tk) {
    e <- inputs[[tk]]$engine
    if (is.null(e)) return(top_engine)
    if (length(unique(e)) > 1L) {
      stop("Task '", tk, "' declares conflicting engines (",
           paste(unique(e), collapse = ", "),
           "); a task must resolve to exactly one estimation engine.",
           call. = FALSE)
    }
    e[[1]]
  }
  # Engines actually reached: only tasks that need an estimation engine.
  est_tasks <- Filter(function(tk) "estimation_engine" %in% reg[[tk]]$needs, attest_tasks)
  engines_used <- unique(vapply(est_tasks, task_engine, character(1)))

  # Packages the composed pipeline actually loads. digest: every tool hashes
  # with it. ggplot2: the eda-standards reference and the diagnostics
  # references build figures with it. mrgda and mrggsave used to ride along
  # with yspec and pmtables here although no task module, tool or reference
  # calls them; the hosted validated-stack job (2026-09-04) stopped an
  # EDA-only fixture pipeline with "R package not installed: mrggsave". The
  # check probes what the declared tasks use, nothing more and nothing less.
  pkgs <- c("targets", "yaml", "digest")
  if ("yspec" %in% needs)    pkgs <- c(pkgs, "yspec")
  if ("pmplots" %in% needs)  pkgs <- c(pkgs, "pmplots", "ggplot2")
  if ("pmtables" %in% needs) pkgs <- c(pkgs, "pmtables")
  if ("estimation_engine" %in% needs) {
    # Union of package needs across every engine any declared task uses.
    for (e in engines_used) {
      pkgs <- c(pkgs, if (identical(e, "nlmixr2")) c("nlmixr2est", "rxode2")
                      else "bbr")
    }
  }
  # Simulation runs on mrgsolve regardless of the estimation engine (house
  # default, SME 2026-08-28); its task also needs the estimation engine to
  # reach the fitted model for the translation-equivalence check.
  if ("simulation_engine" %in% needs) pkgs <- c(pkgs, "mrgsolve", "ggplot2")
  needs_review <- "review" %in% attest_tasks
  # NONMEM/PsN are required iff any declared estimation engine is non-nlmixr2.
  needs_nonmem <- any(engines_used != "nlmixr2")

  # Provision resolution (finding M6b): the environment check must probe
  # what the machine actually uses. The configured NONMEM provision lives in
  # config/engine.yaml (engines.nonmem.provision); resolve it at compose time
  # and pass it EXPLICITLY, so check_environment probes docker/image/license
  # (containerized) or PsN-on-PATH (native) accordingly rather than falling
  # back to its containerized default.
  nonmem_provision <- resolve_nonmem_provision()

  attestation_command <- bquote(attest_environment(require_nonmem   = .(needs_nonmem),
                                   require_psn       = .(needs_nonmem),
                                   nonmem_provision  = .(nonmem_provision),
                                   pkgs              = .(unique(pkgs))))
  if (needs_review) attestation_command$require_review <- TRUE

  # bquote() splices the compose-time decisions (engine flags, package list,
  # resolved provision) into the target command, so the composed pipeline is
  # self-contained and inspectable via tar_manifest().
  out <- list(targets::tar_target_raw(
    ENV_TARGET,
    attestation_command,
    # ALWAYS CUED: the probe runs on every tar_make. Its VALUE is stable on an
    # unchanged machine, so re-running it invalidates nothing downstream.
    cue = targets::tar_cue(mode = "always")),
    # METHOD MANIFESTS AS PIPELINE DEPENDENCIES (integrity batch 4, item D).
    # One file target over every skills/*/MANIFEST.yaml, named inside the
    # command of EVERY stage target below (the same dominance wrap as the
    # gate edge), so a manifest change — a revocation in particular —
    # invalidates every downstream result instead of leaving a cached method
    # result live until something else happens to re-execute it. Coarse by
    # design (any manifest invalidates every stage); per-skill wiring is a
    # later refinement. Compiler-invariant: no task module is edited.
    targets::tar_target_raw(
      MANIFEST_TARGET,
      # names the attestation too: every non-attestation target does
      as.call(list(quote(`{`), as.name(ENV_TARGET),
                   quote(sort(Sys.glob(file.path("skills", "*", "MANIFEST.yaml")))))),
      format = "file"))
  prev_gate <- NULL

  for (i in seq_along(tasks)) {
    tk <- tasks[[i]]
    module <- file.path(TASK_DIR, paste0(tk, ".R"))
    if (!file.exists(module)) {
      stop("Task module not found: ", module,
           "\nThe ", reg[[tk]]$role, " sub-agent authors it, consuming skills/",
           reg[[tk]]$skill, "/.", call. = FALSE)
    }
    # Modules run in a fresh child of globalenv() with task_input,
    # analysis_id and upstream_gate pre-bound — no leakage between tasks.
    env <- new.env(parent = globalenv())
    env$task_input <- inputs[[tk]]
    env$analysis_id <- analysis_id
    env$upstream_gate <- prev_gate
    env$upstream_tasks <- tasks[seq_len(i - 1L)]
    sys.source(module, envir = env)
    if (!exists("task_targets", envir = env)) {
      stop("Task module ", module, " must define `task_targets`.", call. = FALSE)
    }
    stage_targets <- get("task_targets", envir = env)
    validate_task_module(tk, module, stage_targets)

    # GATE DOMINANCE AS A COMPILER INVARIANT (finding C2). A module's
    # gated() wrapper is voluntary — a module that omits it would pass
    # validate_task_module() and run before upstream approval. So the
    # composer makes dominance STRUCTURAL: every stage target of a task
    # that follows another task has its command rewritten to reference the
    # preceding gate symbol, creating the DAG edge by static inspection
    # regardless of what the module authored. A braced block returns the
    # original command's value unchanged, and format="file" targets still
    # return their path, so target VALUES are untouched — only the
    # dependency structure gains the gate edge. Existing gated() wrappers
    # become redundant but harmless (duplicate DAG edges dedup). The first
    # task has no preceding gate, so its targets are left unwrapped.
    # The manifest edge is added to EVERY task's targets, including the first
    # task's (which has no preceding gate).
    stage_targets <- lapply(stage_targets, wrap_command_with_gate, prev_gate)

    gate_name      <- paste0("gate_", tk)
    gate_file_name <- paste0("gate_", tk, "_file")
    produced <- vapply(stage_targets, function(t) t$settings$name, character(1))

    # ALWAYS-CUE THE GATES (integrity batch 3b, item A3). require_gate now
    # performs a LIVE ARTIFACT REHASH + canon2 self-recompute, so it must run on
    # EVERY tar_make — not only when the gate record file changes — to catch an
    # artifact that drifted on disk while the signed gate record stayed put.
    # tar_cue(mode = "always") is applied to the two GATE targets ONLY (the file
    # target and the require_gate target); the stage targets keep their default
    # cue so ordinary invalidation/caching of the actual work is unchanged.
    out <- c(out, stage_targets, list(
      targets::tar_target_raw(gate_file_name,
                              as.call(list(quote(`{`), as.name(ENV_TARGET),
                                           call("identity", gate_file_for(analysis_id, tk)))),
                              format = "file",
                              cue = targets::tar_cue(mode = "always")),
      # Builds the literal call { environment_attestation; artifact1; ...; prev_gate;
      # require_gate(file) }. targets discovers dependencies by static code
      # inspection, so naming each artifact and the previous gate inside the
      # command is what chains the DAG: nothing downstream runs until this
      # gate — and every gate before it — is signed.
      #
      # THE ATTESTATION DOMINATES THE GATES (round 6 F6, round 7 F10). Being
      # the first element of the composed list means nothing in a DAG:
      # tar_make(names = "eda_package") would never run it. Naming it in EVERY
      # generated gate command (and, via wrap_command_with_gate, in every stage
      # target) makes the environment attestation an ancestor of every
      # signature point, so no gate can be satisfied on an unconfirmed or
      # changed environment regardless of which target the caller selects.
      targets::tar_target_raw(
        gate_name,
        # the gate requires the environment check, this task's own artifacts
        # and the previous gate
        as.call(c(quote(`{`),
                  lapply(c(ENV_TARGET, produced, prev_gate), as.name),
                  list(call("require_gate", as.name(gate_file_name))))),
        cue = targets::tar_cue(mode = "always")
      )
    ))
    prev_gate <- gate_name
  }
  out
}

# Refuses a task module that tries to own gate mechanics.
# Two refusals: (1) any target named gate_* — the composer owns those names;
# (2) any textual require_gate( in the module source, which catches calls
# the target-name scan cannot see (inside expressions or helper functions).
# This is the structural guarantee that agents cannot author or pre-satisfy
# gate enforcement.
validate_task_module <- function(task, module, stage_targets) {
  names_used <- vapply(stage_targets, function(t) t$settings$name, character(1))
  offending <- grep("^gate_", names_used, value = TRUE)
  if (length(offending)) {
    stop("Task module ", module, " defines gate targets (",
         paste(offending, collapse = ", "),
         "). Gates are harness-owned and generated by compose_pipeline(); ",
         "task modules define work only.", call. = FALSE)
  }
  txt <- readLines(module, warn = FALSE)
  if (any(grepl("require_gate\\s*\\(", txt))) {
    stop("Task module ", module, " calls require_gate() directly. ",
         "Gate enforcement is composed, not authored.", call. = FALSE)
  }
  invisible(TRUE)
}

# Rewrites one stage target's command to depend on `gate_sym` (the
# preceding gate) without changing the value the target produces.
#
# The command becomes `{ gate_sym; <original command> }`: naming the gate
# symbol first inside a braced block creates the static-inspection DAG edge
# targets uses for dependency discovery, while the block's value is that of
# its last expression — the original command — so the target's value is
# unchanged. format="file" targets keep returning their path for the same
# reason. The target is rebuilt via tar_target_raw so its command hash and
# dependency graph are recomputed cleanly (rather than mutating internals);
# all other settings (format, pattern, iteration, error, memory,
# deployment, priority, resources, storage, retrieval, cue, packages,
# library, description, repository, garbage_collection) are carried over
# verbatim from the original target.
# Batch 4 (item D): the block also names the method-manifest file target —
# `{ environment_attestation; method_manifests; gate_sym; <original command> }`
# — so a manifest edit invalidates the target. Round 7 (F10): the block names
# the environment-attestation target first, so an environment change
# invalidates every stage target. gate_sym = NULL (first task) yields
# `{ environment_attestation; method_manifests; <original command> }`.
wrap_command_with_gate <- function(target, gate_sym) {
  # command$expr is an expression() of length 1; its single element is the
  # authored command language.
  inner <- target$command$expr[[1]]
  deps <- c(list(as.name(ENV_TARGET), as.name(MANIFEST_TARGET)),
            if (!is.null(gate_sym)) list(as.name(gate_sym)) else list())
  wrapped <- as.call(c(quote(`{`), deps, list(inner)))
  s <- target$settings
  targets::tar_target_raw(
    name        = s$name,
    command     = wrapped,
    pattern     = s$pattern,
    packages    = target$command$packages,
    library     = target$command$library,
    format      = s$format,
    repository  = s$repository,
    iteration   = s$iteration,
    error       = s$error,
    memory      = s$memory,
    garbage_collection = s$garbage_collection,
    deployment  = s$deployment,
    priority    = s$priority,
    resources   = s$resources,
    storage     = s$storage,
    retrieval   = s$retrieval,
    cue         = target$cue,
    description = s$description)
}

# Dominance verifier (finding C2). Given a composed pipeline (list of
# targets from compose_pipeline), asserts that EVERY non-gate stage target
# that follows a task's gate references its preceding gate symbol in its
# command — i.e. the structural dominance edge is present — that every stage
# target names the method-manifest file target, and (round 7, F10) that EVERY
# non-attestation target — stage, package, gate file, gate, method_manifests —
# names the environment-attestation target, which must exist exactly once and
# be always-cued. The first task's targets legitimately precede any gate and
# are exempt from the gate edge only.
# Returns TRUE on success; stops with the offending targets otherwise.
# Wired as a full-mode ci_check.R check (it needs the composer + targets).
verify_gate_dominance <- function(pipeline) {
  names_of <- vapply(pipeline, function(t) t$settings$name, character(1))
  is_gate  <- grepl("^gate_", names_of)
  gate_syms <- names_of[is_gate & !grepl("_file$", names_of)]

  offending <- character(0)
  if (sum(names_of == MANIFEST_TARGET) != 1L)
    offending <- c(offending, sprintf("pipeline lacks the single '%s' file target",
                                      MANIFEST_TARGET))
  att_idx <- which(names_of == ENV_TARGET)
  if (length(att_idx) != 1L) {
    offending <- c(offending, sprintf("pipeline lacks the single '%s' target",
                                      ENV_TARGET))
  } else if (!identical(pipeline[[att_idx]]$cue$mode, "always")) {
    offending <- c(offending, sprintf("%s is not always-cued (cue mode '%s')",
                                      ENV_TARGET, pipeline[[att_idx]]$cue$mode))
  }

  # Establish, by scanning in composed order, which gate precedes each
  # target. The attestation and the first task's targets have no preceding gate.
  prev_gate <- NULL
  for (i in seq_along(pipeline)) {
    nm <- names_of[i]
    if (nm == ENV_TARGET) next
    syms <- all.names(pipeline[[i]]$command$expr[[1]])
    # Round 7, F10: every non-attestation target names the attestation.
    if (!(ENV_TARGET %in% syms))
      offending <- c(offending, sprintf("%s (missing edge to %s)", nm, ENV_TARGET))
    if (nm == MANIFEST_TARGET) next
    if (grepl("^gate_", nm)) {
      # A gate target (or its _file) — the require_gate gate updates the
      # running "preceding gate" for subsequent tasks.
      if (!grepl("_file$", nm)) prev_gate <- nm
      next
    }
    if (!(MANIFEST_TARGET %in% syms))
      offending <- c(offending, sprintf("%s (missing edge to %s)", nm, MANIFEST_TARGET))
    # A stage target. If a gate precedes it, its command must name that gate.
    if (!is.null(prev_gate)) {
      if (!(prev_gate %in% syms)) {
        offending <- c(offending, sprintf("%s (missing edge to %s)", nm, prev_gate))
      }
    }
  }
  if (length(offending)) {
    stop("Gate-dominance invariant violated — targets without a structural edge ",
         "to their preceding gate / the method-manifest target / the ",
         "environment attestation:\n  - ",
         paste(offending, collapse = "\n  - "), call. = FALSE)
  }
  invisible(TRUE)
}
