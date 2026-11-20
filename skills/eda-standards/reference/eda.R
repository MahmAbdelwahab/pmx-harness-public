# REFERENCE IMPLEMENTATION — Exploratory data analysis (EDA) numerics and
# figure construction
#
# AUTHORITY: skills/eda-standards/MANIFEST.yaml is the authority for the SME signature
# (root reviewed_by / reviewed_at) and for the per-unit status and digests (six units).
# This header carries no status claim of its own -- read the manifest.
# Every entry point below refuses to run while its unit is not `approved`
# under a locally confirmed manifest. The plot registry is declarative.
#
# WHAT LIVES HERE, AND WHY -------------------------------------------------
# Six intended-use units, one per manifest unit:
#
#   record_selection    Which records the EDA views describe. The kept-record
#                       rule is the harness-wide classifier in
#                       tools/record_selection.R (a manifest DEPENDENCY of
#                       every unit here), the event filter comes from the
#                       specification's decodes and is asserted against the
#                       NONMEM convention, and the one-row-per-subject rule is
#                       declared and validated. This unit also fixes the
#                       ROW-SET IDENTITY: a stable row id on every input record
#                       and a digest per view (round-6 finding 12).
#
#   summary_statistics  The data inventory (subjects, observations, BLQ,
#                       excluded, records read) per declared grouping with
#                       group-specific denominators; the continuous-covariate
#                       summary; the categorical-covariate summary. Every
#                       estimator's convention (SD denominator, median, missing
#                       and non-finite policy, rounding, BLQ policy, subject
#                       snapshot) is a declared parameter emitted with the
#                       result (round 7, finding 3).
#
#   log_axis_audit      The census skills/eda-standards requires before any
#                       log-scale figure, per active plot, axis and row
#                       selector: every value a log axis drops, classified as
#                       missing / zero / negative / non-finite by row id with
#                       its BLQ status (round 7, finding 9).
#
#   display_transforms  Values computed for a figure and read off it: quantile
#                       bins of a covariate (declared quantile type and
#                       interval closure), normalisation of one column by
#                       another.
#
#   figure_construction Builds every EDA figure FROM A DECLARATION (round 7,
#                       findings 1 and 6). The registry carries no R function:
#                       an entry declares a kind, roles, transforms and
#                       parameters, and this unit computes every statistic the
#                       figure displays — LOESS smooth, histogram bins and
#                       counts, pairs-plot correlations, box statistics, dose
#                       markers, pagination — and draws them with identity
#                       geoms from frames that carry the row id.
#
#   registry_validation The registry's declared shape; the render-time checks
#                       taken from the BUILT PLOT (every layer's data digests to
#                       the view it declares, the scales carry exactly the
#                       declared transform, ggplot_build() carries the rows and
#                       the non-finite values the audit predicts, the PDF has
#                       the declared page size and page count); the index that
#                       requires every active entry exactly once and an
#                       enforced `inapplicable` state with a reason for any core
#                       deliverable that cannot be produced.
#
# THE COLUMN CONTRACT ------------------------------------------------------
# No function here knows any column name. Every column is a declared
# parameter, and the analysis resolves those parameters from the dataset's
# yspec specification (tools/external.R: ys_projection() and
# ys_resolve_roles()). Roles used below, with the yspec flag each comes from:
#
#   id            subject identifier                          eda_id
#   exclusion     record-exclusion / comment flag             eda_exclusion
#   event         event type identifier                       eda_event
#   time          independent variable                        eda_time
#   dv            observed dependent variable                 eda_dv
#   blq           below-LLOQ flag                             eda_blq
#   covariate_continuous / covariate_categorical               eda_covariate_*
#   study, dose_group  inventory grouping / profile colour    eda_study,
#                                                             eda_dose_group
#   stratify, dose  catalog figures                           eda_stratify,
#                                                             eda_dose
#
# WHAT THESE FUNCTIONS DO NOT DO ------------------------------------------
# They never read a dataset path and never drop a record from the dataset:
# excluded records are reported with their reasons and remain in the object
# the caller holds. Outlier candidates are flagged by the analyst against the
# analysis plan, never removed here. They write NO file, with ONE exception
# (review 2, M1, decision B): the figure PDFs. The file hashed in the gate
# must be the file the reference proved, so reference_eda_write_figure()
# writes the built pages itself and returns the sha256 of the written bytes;
# reference_eda_figure_check() and reference_eda_registry_index() compare that
# digest to the file on disk. A caller that prints its own PDF has nothing to
# hand the index.

EDA_REFERENCE_FILE <- "skills/eda-standards/reference/eda.R"
# The manifest that binds this file. The source-identity check reads the
# digest it declares for EDA_REFERENCE_FILE and refuses to parse disk bytes
# that do not match (integrity batch 14, R13-P1-1).
EDA_MANIFEST_FILE <- "skills/eda-standards/MANIFEST.yaml"

# Code outside this skill that the methods here rely on. Each is recorded in
# MANIFEST.yaml under every unit's `depends:` with its sha256, so an edit to it
# is a hard stop exactly like an edit to this file. This constant is the
# DECLARATION the provenance table reports (reference_eda_registry_index);
# the sealed dependency construction below names the same path as an in-block
# literal, because its frame is parented on base and cannot read a binding of
# this file (integrity batch 13, decision 1A). The two must agree; the
# manifest's `depends:` is derived from the construction's loads.
EDA_DEPENDENCY_FILES <- base::c("tools/record_selection.R")

# --- the private, locked validator environment (round 7, finding 4) ---------
# The self-gate used to call whatever `require_validated_method` was visible
# on the search path, sourcing tools/method_manifest.R only if none was. A
# caller could therefore define `require_validated_method <- function(...)
# TRUE` and every draft unit computed. The validator is now loaded ONCE into a
# private environment whose parent is the base environment (so nothing on the
# search path is visible to it), and the environment is locked with its
# bindings. .eda_gate() is a closure over that environment, so rebinding any
# name in the global environment — or even rebinding `.eda_private` itself
# afterwards — changes nothing the gate calls.
#
# THE VALIDATOR ENVIRONMENT LOADS ONLY tools/method_manifest.R (integrity
# batch 12, Codex round 11 P0-2). Until batch 12 this file's dependencies were
# sourced into the same environment AFTER the validator, so a dependency that
# defined `require_validated_method` or `.mm_file_sha256` replaced the real
# one before the seal, and every gated call then ran the dependency's code
# with the manifest digest checked by the dependency's helper. The
# dependencies now load into a SECOND locked environment (.eda_deps, below);
# the gate captures the first, the entry points reach the dependency
# functions through the second, and neither environment ever holds a binding
# from the other's files.
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
# A caller who rebinds the gate symbol itself (`.eda_gate`) in the frame this
# file was loaded into bypasses every check below, because the entry points
# then call the caller's closure instead of the gate; nothing in this file
# defends against that, whichever loading shape is used. The controls against
# it are the ones named at the end of this paragraph, not a construction here.
# Same-process reflection -- a caller reaching a sealed environment through
# `environment(fn)`, `parent.env<-` or an active binding -- is out of scope
# of the method guard: its environment protects against ordinary
# rebinding, not against a hostile caller in the same process; the controls
# there are the trusted loader (tools/load_reference.R), the manifest
# digests, the write guard and the SME's review of the diff.
.eda_private <- base::local({
  e <- base::new.env(parent = base::baseenv())
  if (!base::file.exists("tools/method_manifest.R"))
    base::stop("eda-standards reference: required file tools/method_manifest.R ",
               "is missing (the reference is sourced with the repository root ",
               "as the working directory).", call. = FALSE)
  base::sys.source("tools/method_manifest.R", envir = e, keep.source = FALSE)
  base::lockEnvironment(e, bindings = TRUE)
  e
}, envir = base::new.env(parent = base::baseenv()))

# --- the private, locked dependency environment (integrity batch 12) ---------
# tools/record_selection.R is loaded here, never into the validator's
# environment. Its digest at load time is written into this environment before
# the seal; .eda_gate() re-hashes the file on every gated call and compares.
# Every entry point reaches the record-selection functions and constants
# through the value .eda_gate() returns, which is this environment.
#
# WHAT THE DIGEST WRITE IS, AND IS NOT (integrity batch 13, SME decision 2).
# `dependency_sha256` is a RECORD of the bytes this construction loaded, taken
# after the load; it is not the authentication of those bytes, because a
# hash taken after execution cannot stop a top-level side effect that has
# already run. Authentication before execution is the trusted loader's job:
# tools/load_reference.R checks this file's digest and every declared
# dependency's digest against the manifest BEFORE the first byte of this
# file is evaluated, and evaluates exactly the bytes it hashed. The window
# between the loader's check and this construction's own re-read of the
# dependency from disk is the stated residual (a TOCTOU on the repository's
# own files), held by the write guard and by the digest the gate re-checks.
.eda_deps <- base::local({
  d <- base::new.env(parent = base::baseenv())
  for (f in base::c("tools/record_selection.R")) {
    if (!base::file.exists(f))
      base::stop("eda-standards reference: required file ", f, " is missing (the ",
                 "reference is sourced with the repository root as the working ",
                 "directory).", call. = FALSE)
    base::sys.source(f, envir = d, keep.source = FALSE)
  }
  d$dependency_sha256 <- base::vapply(base::c("tools/record_selection.R"), function(f)
    base::paste0("sha256:", digest::digest(file = f, algo = "sha256")),
    base::character(1))
  base::lockEnvironment(d, bindings = TRUE)
  d
}, envir = base::new.env(parent = base::baseenv()))

# The definitions this file owns. Entry points are mirrored by MANIFEST.yaml;
# the internals are inventoried too, so an edit to a helper's TEXT is detected
# as well as an edit to an entry point's. What the structural comparison at
# `.eda_source_identity` compares is exactly that: the deparsed text of each
# inventoried definition (`.eda_fn_text`), running side against disk side. It
# does NOT compare the enclosing environment of a definition, so a same-text
# closure rebuilt in an environment that binds one of its free names to a fake
# passes the comparison: the fake is reached and the identity check still says
# the definitions match (integrity batch 14 review, observation O-8). Comparing
# enclosing environments as well as text is queued for a later batch by SME
# decision of 2026-09-05; it is not an accepted residual.
EDA_ENTRYPOINTS <- base::c(
  "reference_eda_view", "reference_eda_rowset_digest",
  "reference_eda_inventory", "reference_eda_continuous_summary",
  "reference_eda_categorical_summary",
  "reference_eda_log_axis_audit", "reference_eda_log_axis_audit_table",
  "reference_eda_quantile_bins", "reference_eda_normalize",
  "reference_eda_build_figure", "reference_eda_write_figure",
  "reference_eda_registry_check", "reference_eda_registry_active",
  "reference_eda_registry_index", "reference_eda_plot_rowset_digest",
  "reference_eda_figure_check", "reference_eda_core_status")
EDA_INTERNALS <- base::c(".eda_gate", ".eda_digest", ".eda_require_cols", ".eda_kept",
                   ".eda_exclusion_reason", ".eda_fn_text",
                   ".eda_log_class", ".eda_nonfinite_after", ".eda_layer_plan",
                   ".eda_role_col", ".eda_label", ".eda_pdf_geometry",
                   ".eda_scale_transform_name", ".eda_check_entry",
                   ".eda_bins", ".eda_check_view",
                   ".eda_source_identity", ".eda_first_value", "%||%")

# The separator is the ASCII unit separator "\037", which cannot occur in a
# row id or a selector field; with no separator {1, 2, 3} and {1, 23} digest
# identically (review 2, H1 -- a regression from HEAD, now covered by a test).
# Defined here, ahead of the source-identity construction, because that closure
# captures this function object at load (integrity batch 13 fix round 3).
.eda_digest <- function(x) {
  base::paste0("sha256:", digest::digest(base::paste(base::as.character(x), collapse = "\037"),
                                   algo = "sha256"))
}

.eda_fn_text <- function(f) {
  if (base::exists("removeSource", envir = base::asNamespace("utils"),
                   mode = "function"))
    f <- utils::removeSource(f)
  base::paste(base::deparse(f), collapse = "\n")
}

# SOURCE IDENTITY (pre-approval review finding M4; review 2, H2). The manifest
# hashes the file at a constant path; this proves the functions now RUNNING
# match that file. The structural digest of every definition in the entry-point
# and internals inventory is compared against the file on disk in BOTH source
# modes (before review 2 it was skipped whenever source references were kept,
# which is the mode the pilot runs in). The srcfile comparison is an ADDITIONAL
# check when a source reference exists, never an early return.
#
# Integrity batch 13 fix round 3 (R13C-2). This closure does not read ONE name
# from the sourcing frame at call time. Fix round 2 sealed only the closure's
# own body and still delegated to the plain top-level `.eda_defs_digest` and to
# a since-removed disk-side digest helper, which in turn read `.eda_digest`,
# `.eda_fn_text`,
# `EDA_ENTRYPOINTS`/`EDA_INTERNALS` and `EDA_REFERENCE_FILE` from the sourcing
# frame; any one of five ordinary post-source rebindings collapsed both sides of
# the comparison to the same value and let a MODIFIED copy of eda.R compute
# under the approved entry (reviewer fixture srcid3.R, five GATE PASSED). Now
# everything the check depends on is captured HERE, at construction: `ref_file`
# and `inv` (the sorted inventory) as VALUES, and the digest primitive and text
# helper as the function OBJECTS they were at load, held in a base-parented,
# locked `helpers` environment and reached through `base::get(..., envir =
# helpers)`. The digest is computed inline (`digest_of`), over `inv`, for both
# the running definitions and a fresh re-source of `ref_file`; a rebinding of
# `.eda_digest`, `.eda_fn_text`, `EDA_ENTRYPOINTS`, `EDA_INTERNALS` or
# `EDA_REFERENCE_FILE` in the sourcing frame changes nothing this closure uses.
# `env` is still the environment that sourced this file (the parent of this
# closure's own environment), which is where the RUNNING entry points live.
.eda_source_identity <- base::local({
  ref_file <- EDA_REFERENCE_FILE
  inv <- base::sort(base::c(EDA_ENTRYPOINTS, EDA_INTERNALS))
  # The captured inventory must be COMPLETE (integrity batch 14, R13-P0-1).
  # The two inventory vectors are built with `base::c()`, so a caller cannot
  # reach them by rebinding `c`; this check refuses anyway if what arrives here
  # is a degenerate inventory, because an empty inventory would let both sides
  # of the structural comparison below digest nothing.
  #
  # This is a SHAPE check only. Until integrity batch 15 it also asserted the
  # cardinality 37, which is not the same claim as "the inventory names every
  # definition this file owns": an approved revision that added a helper and
  # left the vectors alone would still count 37 and leave the new helper
  # outside identity checking (Codex round 14, R14-P3-1). The completeness
  # claim is now made where it can actually be settled -- in the closure below,
  # against the set of definitions the AUTHENTICATED parse owns.
  if (!base::is.character(inv) || !base::length(inv) || base::anyNA(inv) ||
      !base::all(base::nzchar(inv)) || !base::identical(inv, base::unique(inv)))
    base::stop("eda-standards: the source-identity inventory captured at load is not a ",
               "non-empty vector of distinct names (", base::length(inv),
               " captured). The structural comparison cannot run against an incomplete ",
               "inventory. ESCALATE to the SME.", call. = FALSE)
  helpers <- base::new.env(parent = base::baseenv())
  base::assign("digest_fn", .eda_digest, envir = helpers)
  base::assign("fn_text", .eda_fn_text, envir = helpers)
  base::lockEnvironment(helpers, bindings = TRUE)
  # A construction-local cache for the disk-side digest, keyed by the reference
  # file's sha256. Re-sourcing and re-digesting the ~2000-line reference on every
  # gated entry-point call is otherwise a heavy per-call cost. The cache is
  # WRITTEN in the returned closure's body (a `base::assign` in a function body,
  # not a pre-seal write in a sealed construction block), so the trusted-construction
  # census does not (and should not) apply. It is a base-parented env captured
  # here, unreachable from the sourcing frame (integrity batch 13 fix round 3).
  cache <- base::new.env(parent = base::baseenv())
  # Does this expression load code from disk anywhere in its tree? Used to skip
  # the dependency-loading constructions during the throwaway evaluation of the
  # authenticated bytes (integrity batch 15, R14-P1-1). It is a purely syntactic
  # walk over a parsed expression: it never evaluates anything, and it recognises
  # exactly the NINE spellings named below.
  #
  # `load` AND `readRDS` JOINED THE LIST in integrity batch 16 (Codex round 15,
  # additional check), and their effect is NOT the same as a code load's --
  # wording corrected in fix round 1, review observation O3, where the earlier
  # sentence called it "the same class of skip". A stubbed CODE load binds names
  # that are excluded from the comparison anyway, so stubbing it changes nothing
  # that is compared. A stubbed DATA load binds a name the comparison DOES cover
  # by value: the running side holds the loaded object and the disk side holds
  # the empty stub environment, the digests differ, and every gated call of such
  # a reference REFUSES on identity. That is fail-closed, and deliberately so --
  # a reference must not read a data file at its top level -- but it is a
  # permanent refusal, not a skip, and an author who adds one will see the
  # identity message rather than a quiet pass. No shipped reference contains a
  # bare `load` or `readRDS` (verified: `grep -n "\bload\b\|readRDS"` over
  # this file returns comment lines only), so nothing is affected today.
  #
  # WHAT THIS COVERS, EXACTLY (corrected in integrity batch 15 fix round 1,
  # review observation O1; the earlier wording claimed the opposite of the
  # truth). A spelling OUTSIDE the nine -- `base::eval(base::parse(file = ...))`,
  # say -- is NOT recognised, so it is EVALUATED here rather than skipped: the
  # walk errs towards EVALUATION, not towards refusal. Together with the gate's
  # dependency-drift comparison, which now runs first, the pair leaves no
  # ordering in which a DECLARED dependency loaded by one of the nine
  # recognised spellings executes unchecked. A QUALIFICATION ON THE ORDERING
  # (Codex round 15): the gate's early dependency comparison uses the manifest's
  # declared `depends:` digests, so declaring an additional data load puts it
  # under that comparison; a load of a file the manifest does not declare is
  # outside it either way. It says nothing about an
  # UNDECLARED file, and nothing about a load written some other way: such a
  # file is outside the manifest's `depends:` digests, so the gate does not
  # re-hash it, and it would execute during this comparison. Nothing in the
  # harness yet REQUIRES a reference's disk loads to be declared
  # (tools/method_manifest.R records that its trusted-loads census is a report,
  # not a gate); making that a requirement is an SME item. No shipped reference
  # loads code by an unrecognised spelling.
  loads_from_disk <- function(e) {
    if (base::is.symbol(e))
      base::as.character(e) %in% base::c("source", "sys.source", "library", "require",
                                         "requireNamespace", "loadNamespace", "attach",
                                         "load", "readRDS")
    else if (base::is.call(e) || base::is.pairlist(e) || base::is.expression(e)) {
      # `as.list()` rather than `[[`: a call can carry a missing argument (the
      # empty symbol), and indexing one out of a call is itself an error.
      hit <- FALSE
      l <- base::as.list(e)
      for (i in base::seq_along(l)) {
        # The empty symbol is compared through single-bracket indexing and
        # never bound to a variable: a variable bound to it counts as a missing
        # argument, and touching it is an error.
        if (!base::identical(base::`[`(l, i), base::list(base::quote(expr = ))))
          if (loads_from_disk(base::`[[`(l, i))) hit <- TRUE
      }
      hit
    } else FALSE
  }
  # ENCLOSURE DEPTH (integrity batch 15, queue O-8; SME decision "go with b",
  # 2026-09-05). A definition's deparsed text does not say which environment it
  # closes over, so a same-text helper rebuilt in an environment that binds one
  # of its free names to a fake used to pass the comparison and still reach the
  # fake (reviewer fixture probe rebuilt `.eda_require_cols`
  # in an env binding `setdiff`). `enclosure_depth` reports how many
  # `parent.env` steps separate a definition's enclosing environment from the
  # environment the file was loaded into: 0 for an ordinary top-level
  # definition, 1 for one built in a load-time `base::local()` construction
  # frame, NA if the loading environment is not on that chain at all. The depth
  # enters the digest, so the running side and the authenticated disk side must
  # agree definition by definition; the lookalike's extra hostile frame shows up
  # as depth 1 where the file owns depth 0.
  #
  # WHAT THIS IS, EXACTLY: A DEPTH CHECK (wording corrected in integrity batch
  # 16, Codex round 15 R15-P1-2; the earlier wording invited more to be read
  # into it than it does).
  #
  # IT COMPARES one integer per definition -- how many `parent.env` steps
  # separate that definition's enclosing environment from the environment the
  # file was loaded into -- and it compares that integer between the running
  # side and the authenticated disk side.
  #
  # IT DOES NOT compare the IDENTITY of any enclosing environment, and it does
  # not authenticate the BINDINGS those environments hold. Two different
  # environments at the same depth are indistinguishable to it. In particular a
  # same-text helper rebuilt IN the loading environment itself has depth 0,
  # exactly what the file owns, so this check cannot refuse it -- if that
  # rebuilt helper reaches a shadowed free name, the defence is
  # `base::`-qualification of the policy-critical call, not this check. What
  # this check does refuse is a lookalike built in an EXTRA frame (the reviewer
  # fixture probe rebuilt `.eda_require_cols` in an env
  # binding `setdiff`), which shows up as depth 1 where the file owns depth 0.
  #
  # It is the reference inspecting its OWN closures against its own
  # authenticated bytes. It is NOT a defence against a hostile caller's
  # reflection in the same process -- reaching a sealed environment through
  # `environment(fn)`, `parent.env<-` or an active binding stays out of scope by
  # SME decision (2026-09-05, restated as D15-4 on 2026-09-06 and D16-5).
  enclosure_depth <- function(fn, top) {
    e <- base::environment(fn)
    n <- 0L
    out <- NA_integer_
    while (n <= 16L && !base::is.null(e) && base::is.na(out)) {
      if (base::identical(e, top)) out <- n
      else if (base::identical(e, base::emptyenv())) e <- NULL
      else { e <- base::parent.env(e); n <- n + 1L }
    }
    out
  }
  # `digest_of` is a construction-local closure (bound in this block's frame, so
  # a bare call to it resolves lexically HERE, never through the sourcing frame)
  # and uses only nested `if`/`else` -- no `return` -- so nothing in its control
  # flow resolves through the sourcing frame either.
  #
  # POLICY DATA IS PART OF METHOD IDENTITY (integrity batch 15, R14-P1-2; SME
  # decision D15-1, 2026-09-06). The digest used to cover bindings of mode
  # "function" only, so `EDA_CORE_TABLES` and the registry policy vectors could
  # be rebound after an honest load and change what an approved method requires
  # without changing one function object. `data_names` is the set of top-level
  # non-function bindings the AUTHENTICATED parse owns (it is derived from the
  # disk expressions by the caller of `digest_of`, never from a running-side
  # vector a caller could shrink), and their deparsed VALUES enter the digest
  # alongside the function texts.
  digest_of <- function(defs_env, data_names) {
    base::get("digest_fn", envir = helpers)(base::c(
      base::vapply(inv, function(nm) {
        if (base::exists(nm, envir = defs_env, mode = "function", inherits = FALSE)) {
          f <- base::get(nm, envir = defs_env, mode = "function")
          base::paste0(nm, "=", base::get("fn_text", envir = helpers)(f),
                       "\037enclosure=", enclosure_depth(f, defs_env))
        } else base::paste0(nm, "=<absent>")
      }, base::character(1)),
      base::vapply(data_names, function(nm) {
        if (base::exists(nm, envir = defs_env, inherits = FALSE)) {
          v <- base::get(nm, envir = defs_env, inherits = FALSE)
          # AN ENVIRONMENT DEPARSES TO "<environment>" AND WOULD CARRY NO
          # CONTENT INTO THE DIGEST (integrity batch 16, R15-P1-1 / D16-1). The
          # sealed policy environment `.eda_policy` is a top-level non-function
          # binding whose VALUES are the policy the gated bodies enforce, so a
          # substituted lookalike -- a locked, base-parented environment holding
          # a narrowed EDA_CORE_TABLES -- would be invisible to a deparse. Its
          # bindings are digested by name and value instead. Nothing here
          # evaluates a binding: it reads values already in the environment.
          #
          # ONLY `.eda_policy`, AND WHY NOT EVERY ENVIRONMENT. The other
          # top-level environment bindings (`.eda_private`, `.eda_deps`) are
          # built by constructions that load code from disk, and those
          # constructions are SKIPPED on the authenticated-disk side and bound
          # to an empty sealed stub instead (integrity batch 15, R14-P1-1).
          # Their contents therefore cannot be compared: the disk side has
          # none. `.eda_policy` is built from this file's own constants, so both
          # sides construct it identically and its contents ARE comparable.
          if (base::is.environment(v) && base::identical(nm, ".eda_policy")) {
            # THE SEAL IS PART OF THE IDENTITY (integrity batch 17, Codex round
            # 16 R16-P1-1). Digesting the values alone accepted an unlocked
            # lookalike holding the right values, which a promise forced later
            # in the same call could then rewrite. An unsealed policy
            # environment is refused here rather than digested.
            if (!base::environmentIsLocked(v))
              base::stop("eda-standards: `.eda_policy` is not a locked environment; ",
                   "the policy this reference enforces could be rewritten while a ",
                   "gated call runs. ESCALATE to the SME.", call. = FALSE)
            for (k in base::ls(v, all.names = TRUE))
              if (!base::bindingIsLocked(k, v))
                base::stop("eda-standards: the binding `", k, "` in `.eda_policy` is ",
                     "not locked. ESCALATE to the SME.", call. = FALSE)
            ks <- base::sort(base::ls(v, all.names = TRUE))
            base::paste0(nm, "=<env:", base::paste(base::vapply(ks, function(k)
              base::paste0(k, "=", base::paste(base::deparse(
                base::get(k, envir = v, inherits = FALSE)), collapse = "\n")),
              base::character(1)), collapse = "\036"), ">")
          } else
            base::paste0(nm, "=", base::paste(base::deparse(v), collapse = "\n"))
        } else base::paste0(nm, "=<absent>")
      }, base::character(1))))
  }
  # `disk_bytes` / `disk_key` are the ONE byte snapshot the gate read and
  # authenticated against the manifest before calling this closure (integrity
  # batch 14, R13-P1-1). This closure never reads the file itself: it parses
  # exactly the bytes it was handed, so nothing unauthenticated is ever
  # evaluated, and the cache is keyed on the digest of those same bytes.
  function(disk_bytes, disk_key) {
    env <- base::parent.env(base::environment(base::sys.function()))
    # EVERY inventoried definition must be present, and be a function, in the
    # environment the reference was loaded into. Until integrity batch 14 the
    # whole structural comparison sat inside a check that `reference_eda_view`
    # existed, and the closure returned TRUE otherwise; deleting that one
    # binding therefore disabled the identity check, after which any other
    # definition could be substituted undetected (Codex round 13, R13-P0-1). A
    # missing or non-function inventoried name is now a REFUSAL, and the
    # comparison below runs unconditionally.
    for (nm in inv) {
      if (!base::exists(nm, envir = env, mode = "function", inherits = FALSE))
        base::stop("eda-standards: the inventoried definition `", nm, "` is absent from ",
             "the environment the reference was loaded into, or is not a function. ",
             "An incomplete environment cannot satisfy the source-identity check; ",
             "the approved file defines all ", base::length(inv), " of them. ",
             "ESCALATE to the SME.", call. = FALSE)
    }
    fn <- base::get("reference_eda_view", envir = env, mode = "function")
    sf <- base::attr(base::body(fn), "srcfile")
    if (!base::is.null(sf)) {
      fname <- base::get("filename", envir = sf)
      if (base::is.character(fname)) {
        if (base::nzchar(fname)) {
          if (!base::identical(
                base::normalizePath(fname, winslash = "/", mustWork = FALSE),
                base::normalizePath(ref_file, winslash = "/", mustWork = FALSE)))
            base::stop("eda-standards: the reference functions now running were sourced ",
                 "from '", fname, "', not from the manifest's reference_path ",
                 ref_file, ". The manifest authorizes THAT file; a copy ",
                 "of it cannot compute under its approval. ESCALATE to the SME.",
                 call. = FALSE)
        }
      }
    }
    if (!base::is.raw(disk_bytes) || !base::is.character(disk_key) ||
        base::length(disk_key) != 1L)
      base::stop("eda-standards: the source-identity check was called without the ",
           "authenticated byte snapshot the gate takes. ESCALATE to the SME.",
           call. = FALSE)
    disk <- base::`[[`(cache, disk_key)
    if (base::is.null(disk)) {
      disk_env <- base::new.env(parent = base::baseenv())
      disk_txt <- base::rawToChar(disk_bytes)
      base::Encoding(disk_txt) <- "UTF-8"
      disk_exprs <- base::parse(text = disk_txt, keep.source = FALSE)
      # NOTHING IS SOURCED FROM DISK WHILE THE COMPARISON RUNS (integrity batch
      # 15, Codex round 14 R14-P1-1). This throwaway evaluation used to execute
      # every top-level expression of the authenticated reference, including the
      # two `base::local()` constructions that `base::sys.source()` a dependency
      # off the working tree. On a cold identity check that re-ran a dependency
      # whose bytes had not been checked yet -- an edited helper's top level
      # executed before the gate's dependency-drift comparison could refuse it.
      # A dependency-loading construction is now SKIPPED and its name bound to a
      # sealed empty environment instead. That is sound for this comparison: the
      # digest covers each definition's deparsed text and enclosure depth, and a
      # closure's deparse does not depend on the VALUES its construction frame
      # captured, so the stub changes nothing that is compared. The authentic
      # dependency bytes are checked by the gate, before this closure is called.
      stub_env <- base::new.env(parent = base::baseenv())
      base::lockEnvironment(stub_env, bindings = TRUE)
      for (k in base::seq_along(disk_exprs)) {
        e <- base::`[[`(disk_exprs, k)
        nm <- if (base::is.call(e) && base::length(e) == 3L &&
                  base::is.symbol(base::`[[`(e, 1L)) &&
                  base::as.character(base::`[[`(e, 1L)) %in% base::c("<-", "=") &&
                  base::is.symbol(base::`[[`(e, 2L)))
                base::as.character(base::`[[`(e, 2L)) else NA_character_
        if (loads_from_disk(e)) {
          if (base::is.na(nm))
            base::stop("eda-standards: the approved reference has a top-level expression ",
                 "that loads code from disk outside an assignment, so the identity ",
                 "comparison cannot evaluate the file without executing it. ",
                 "ESCALATE to the SME.", call. = FALSE)
          if (nm %in% inv)
            base::stop("eda-standards: the inventoried definition `", nm, "` is built by a ",
                 "construction that loads code from disk. An inventoried definition ",
                 "cannot be stubbed out of the identity comparison. ESCALATE to the SME.",
                 call. = FALSE)
          base::assign(nm, stub_env, envir = disk_env)
        } else base::eval(e, envir = disk_env)
      }
      # The definitions the AUTHENTICATED parse actually owns (integrity batch
      # 15, R14-P3-1). `disk_env` is base-parented and holds nothing but this
      # file's own top-level bindings, so its function bindings ARE the owned
      # definition set. Comparing that set with the captured inventory settles
      # completeness in both directions -- a helper added without inventorying
      # it, and an inventoried name the file no longer defines -- where the
      # former cardinality assertion settled neither.
      owned <- base::sort(base::Filter(function(n)
        base::exists(n, envir = disk_env, mode = "function", inherits = FALSE),
        base::ls(disk_env, all.names = TRUE)))
      if (!base::identical(owned, inv)) {
        extra <- base::setdiff(owned, inv)
        gone  <- base::setdiff(inv, owned)
        base::stop("eda-standards: the source-identity inventory does not name exactly the ",
             "definitions ", ref_file, " owns. Not inventoried: ",
             base::paste(base::c(extra, "<none>")[base::seq_len(base::max(1L, base::length(extra)))],
                         collapse = ", "),
             ". Inventoried but not defined: ",
             base::paste(base::c(gone, "<none>")[base::seq_len(base::max(1L, base::length(gone)))],
                         collapse = ", "),
             ". A definition outside the inventory is outside identity checking. ",
             "ESCALATE to the SME.", call. = FALSE)
      }
      # The top-level non-function bindings the authenticated parse owns: the
      # method's policy data (D15-1). Taken from `disk_env`, never from a
      # running-side vector, so a caller cannot shrink the set that is compared.
      data_names <- base::sort(base::Filter(function(n)
        !base::exists(n, envir = disk_env, mode = "function", inherits = FALSE),
        base::ls(disk_env, all.names = TRUE)))
      disk <- base::list(digest = digest_of(disk_env, data_names),
                         data_names = data_names)
      # Base-qualified in-place write (environments are reference-semantic, so
      # this mutates `cache` and the return is discarded); writing bare
      # `cache[[disk_key]] <-` would call a `[[<-` resolved through the
      # sourcing frame, where it could be shadowed to poison the cached
      # disk-side digest.
      base::`[[<-`(cache, disk_key, disk)
    }
    if (!base::identical(digest_of(env, base::`[[`(disk, "data_names")),
                         base::`[[`(disk, "digest")))
      base::stop("eda-standards: the reference functions now running do not match the ",
           "definitions in ", ref_file, ", the manifest's ",
           "reference_path. Something other than the approved file is ",
           "computing. ESCALATE to the SME.", call. = FALSE)
    base::invisible(TRUE)
  }
}, envir = base::new.env())

# The sealed handle the gate calls the source-identity check through (integrity
# batch 13 fix round 3, R13C-2). The gate must NOT call `.eda_source_identity()`
# by its bare top-level symbol: that name resolves through the sourcing frame,
# where a caller could rebind it to a no-op after the file is sourced (reviewer
# fixture srcid3.R). This environment holds the closure OBJECT as it was at load
# (bound by a pre-seal write inside this block; the trusted-construction
# census does NOT examine this block, so only the runtime identity check below
# defends it), parent = base, locked. The gate captures it at construction, checks
# its identity, lock and parent before use, and invokes the check as
# `base::get("srcid", envir = <this>)()`, so a rebinding of the top-level symbol
# cannot substitute a different closure.
.eda_srcid_env <- base::local({
  e <- base::new.env(parent = base::baseenv())
  base::assign("srcid", .eda_source_identity, envir = e)
  base::lockEnvironment(e, bindings = TRUE)
  e
}, envir = base::new.env())

# The self-gate: a closure over the validator environment AND the dependency
# environment. The dependency files are re-hashed at every gated call and
# compared with the digests taken when they were loaded, so a dependency edited
# after sourcing is caught here even before the manifest's own `depends:` check
# sees it. It RETURNS the dependency environment (review 2, H2; batch 12): every
# entry point takes the classifier and the event assertion through that
# returned value, never through a dynamic lookup of `.eda_deps`, and a rebound
# `.eda_private` or `.eda_deps` in the sourcing environment is itself detected
# here. The validator is called ONLY through `private`, which never held a
# dependency binding.
.eda_gate <- base::local({
  private <- .eda_private
  deps <- .eda_deps
  srcid_env <- .eda_srcid_env
  ref_file <- EDA_REFERENCE_FILE
  manifest_file <- EDA_MANIFEST_FILE
  function(unit, what) {
    src_env <- base::parent.env(base::environment(base::sys.function()))
    # TAMPER CHECK FIRST (integrity batch 13 fix round 3, R13C-2). Fix round 2
    # ran the source-identity call BEFORE verifying the sealed objects, so a
    # rebinding took effect before anything checked it. Verify here, before the
    # identity closure runs, that the three sealed objects this gate trusts --
    # the validator environment, the dependency environment, and the
    # source-identity handle environment -- are exactly the ones captured at
    # construction, and that the handle environment is still locked and still
    # parented on base.
    for (nm in base::c(".eda_private", ".eda_deps", ".eda_srcid_env")) {
      bound <- if (base::exists(nm, envir = src_env, inherits = FALSE))
        base::get(nm, envir = src_env, inherits = FALSE) else NULL
      want <- if (base::identical(nm, ".eda_private")) private
              else if (base::identical(nm, ".eda_deps")) deps else srcid_env
      if (!base::identical(bound, want))
        base::stop("eda-standards: `", nm, "` in the environment that sourced the ",
             "reference is not the environment the reference loaded; something ",
             "rebound it after sourcing. ESCALATE to the SME.", call. = FALSE)
    }
    if (!base::environmentIsLocked(srcid_env))
      base::stop("eda-standards: the source-identity handle environment is no longer ",
           "locked; something reopened it after the reference loaded. ESCALATE to ",
           "the SME.", call. = FALSE)
    if (!base::identical(base::parent.env(srcid_env), base::baseenv()))
      base::stop("eda-standards: the source-identity handle environment is no longer ",
           "parented on the base environment. ESCALATE to the SME.", call. = FALSE)
    # THE SEALED POLICY ENVIRONMENT IS CHECKED FOR ITS SEAL (integrity batch 17,
    # Codex round 16 R16-P1-1). The identity digest compares `.eda_policy`'s
    # bindings by name and by value, so a lookalike carrying different policy
    # is refused there. It could not, until now, refuse a lookalike carrying
    # the SAME values in an UNLOCKED environment: every value matched at gate
    # time, the gated body captured the replacement, and an argument promise
    # forced afterwards wrote a narrowed policy into it before the body read it.
    # The seal is what makes the capture-then-read sequence safe, so the seal
    # itself is verified here, before any argument promise can run.
    #
    # WHY THIS IS A STRUCTURAL CHECK AND NOT AN `identical()` COMPARISON WITH A
    # CONSTRUCTION-TIME CAPTURE, as the other three sealed objects above get.
    # `.eda_policy` is built from this file's policy constants, the last of
    # which (EDA_CORE_TABLES) is defined well below this block; the policy
    # environment therefore does not exist when this gate closure is
    # constructed and cannot be captured into it. What replaces the identity
    # comparison is the pair of checks the two halves already supply: the
    # identity digest fixes the CONTENTS (an environment whose bindings differ
    # from the ones on the authenticated disk side is refused), and the
    # checks below fix the SEAL (locked, every binding locked, parented on
    # base), so the contents cannot change after they are compared. A
    # substituted environment that satisfies both is byte-for-byte the policy
    # this file builds and is immutable for the rest of the call.
    pol <- if (base::exists(".eda_policy", envir = src_env, inherits = FALSE))
      base::get(".eda_policy", envir = src_env, inherits = FALSE) else NULL
    if (!base::is.environment(pol))
      base::stop("eda-standards: `.eda_policy` in the environment that sourced the ",
           "reference is not an environment; something rebound it after ",
           "sourcing. ESCALATE to the SME.", call. = FALSE)
    if (!base::environmentIsLocked(pol))
      base::stop("eda-standards: the sealed policy environment `.eda_policy` is not ",
           "locked; something replaced or reopened it after the reference ",
           "loaded. ESCALATE to the SME.", call. = FALSE)
    for (k in base::ls(pol, all.names = TRUE))
      if (!base::bindingIsLocked(k, pol))
        base::stop("eda-standards: the binding `", k, "` in the sealed policy ",
             "environment `.eda_policy` is not locked, so the policy this call ",
             "enforces can still be rewritten while it runs. ESCALATE to the SME.",
             call. = FALSE)
    if (!base::identical(base::parent.env(pol), base::baseenv()))
      base::stop("eda-standards: the sealed policy environment `.eda_policy` is no ",
           "longer parented on the base environment. ESCALATE to the SME.",
           call. = FALSE)
    # DEPENDENCY DRIFT IS CHECKED BEFORE ANYTHING IS EVALUATED (integrity batch
    # 15, Codex round 14 R14-P1-1). This comparison used to run AFTER the
    # source-identity check, whose throwaway evaluation of the authenticated
    # bytes re-ran the reference's dependency constructions and so executed the
    # top level of whatever was at the dependency path at that moment. The
    # ordering is now: hash every declared dependency and compare with the
    # digests taken when they were loaded, THEN check identity -- and the
    # identity check skips dependency-loading constructions rather than
    # executing them, so neither half of the pair can run unchecked bytes.
    dsha <- base::get("dependency_sha256", envir = deps)
    for (f in base::names(dsha)) {
      now <- if (base::file.exists(f))
        base::paste0("sha256:", digest::digest(file = f, algo = "sha256")) else "<missing>"
      if (!base::identical(now, base::`[[`(dsha, f)))
        base::stop("eda-standards: dependency ", f, " changed on disk after the ",
             "reference loaded it (loaded ", base::`[[`(dsha, f),
             ", now ", now, "). The running method no longer matches its ",
             "record. ESCALATE to the SME.", call. = FALSE)
    }
    # Call the identity closure through the verified sealed environment, by
    # value: `base::get("srcid", envir = srcid_env)` retrieves the object
    # captured at load (srcid_env was just checked identical to the
    # construction-time capture), so a post-source rebind of the top-level
    # `.eda_source_identity` symbol in the sourcing frame cannot substitute a
    # no-op (integrity batch 13 fix round 3, R13C-2).
    # ONE byte snapshot of the reference file, authenticated BEFORE anything
    # in it is evaluated (integrity batch 14, Codex round 13 R13-P1-1). Until
    # this batch the source-identity check hashed the file and then re-executed
    # the PATH with `sys.source()` during the gated call, so the top level of
    # whatever was at that path at that later instant ran BEFORE the manifest
    # validator below could reject its digest -- and a swap between the two
    # reads could cache the definitions of one set of bytes under the digest of
    # another. The bytes are read once here, hashed, compared with the digest
    # the manifest binds for this file, and only then handed to the identity
    # check, which parses exactly those bytes and caches on exactly that digest.
    if (!base::file.exists(ref_file))
      base::stop("eda-standards: the manifest's reference_path ", ref_file,
           " does not exist; the approved method is missing.", call. = FALSE)
    disk_bytes <- base::readBin(ref_file, what = "raw", n = base::file.size(ref_file))
    disk_key <- digest::digest(disk_bytes, algo = "sha256", serialize = FALSE)
    declared <- base::unique(base::vapply(
      base::Filter(function(u) base::identical(base::`[[`(u, "reference_path"), ref_file),
                   base::`[[`(base::get("read_method_manifest",
                                        envir = private)(manifest_file), "units")),
      function(u) base::`[[`(u, "sha256"), base::character(1)))
    if (!base::identical(declared, base::paste0("sha256:", disk_key)))
      base::stop("eda-standards: the bytes now at ", ref_file, " are not the bytes ",
           manifest_file, " binds for it (manifest ",
           base::paste(declared, collapse = ", "), ", on disk sha256:", disk_key,
           "). They were NOT evaluated: nothing they contain has run. Either the ",
           "reference was edited after the manifest was generated (regenerate with ",
           "write_method_manifest() and have the SME re-confirm) or something other ",
           "than the approved file is at that path. ESCALATE to the SME.",
           call. = FALSE)
    base::get("srcid", envir = srcid_env)(disk_bytes, disk_key)
    if (!base::get("require_validated_method", envir = private)(skill_dir    = "skills/eda-standards",
                                          calling_file = "skills/eda-standards/reference/eda.R",
                                          unit         = unit)) {
      base::stop("eda-standards unit '", unit, "' is not SME-validated (the manifest ",
           "unit is draft/rejected, or skills/eda-standards/MANIFEST.yaml has an ",
           "empty reviewed_by); ", what, " do not compute until the unit is ",
           "approved. ESCALATE to the SME.", call. = FALSE)
    }
    base::invisible(deps)
  }
}, envir = base::new.env())

# --- small shared helpers (not entry points) --------------------------------

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# `base::`-qualified (integrity batch 16, R15-P1-2 / D16-2). This helper is the
# column contract: an honest load followed by `E$setdiff <- function(...)
# character(0)` in the loading environment made `absent` empty and the contract
# vacuous, while every inventoried definition and every policy value still
# matched. The enclosure DEPTH check does not catch it -- rebuilding this helper
# from its own text IN the loading environment gives depth 0, exactly what the
# file owns. Qualification is the fix.
.eda_require_cols <- function(data, cols, what) {
  cols <- base::unique(cols[base::nzchar(cols)])
  absent <- base::setdiff(cols, base::names(data))
  if (length(absent)) {
    stop("eda reference: the data frame has no column(s) ",
         paste(absent, collapse = ", "), ", required for ", what,
         ". The column contract is resolved from the dataset's yspec ",
         "specification; a missing column means the spec and the data ",
         "disagree.", call. = FALSE)
  }
  invisible(TRUE)
}

# THE KEPT-RECORD RULE delegates to the harness-wide classifier (round 7,
# finding 2). The rule text and the tokens live in tools/record_selection.R;
# nothing is restated here.
# `priv` is the environment .eda_gate() returned to the calling entry point.
.eda_kept <- function(x, priv) priv$harness_kept_record(x)
.eda_exclusion_reason <- function(x, priv) priv$harness_exclusion_reason(x)

# Subject snapshot guard (review 2, L3): the snapshot is the FIRST kept record
# per subject; a subject whose first kept record has a missing value but a
# later kept record a value would silently lose it, so that case is a STOP
# naming the subjects. Declared and emitted with the summaries.
.eda_first_value <- function(x, id, what) {
  ids <- unique(id)
  first_na_later_value <- vapply(ids, function(i) {
    z <- x[id == i]
    is.na(z[1]) && any(!is.na(z))
  }, logical(1))
  if (any(first_na_later_value))
    stop("eda reference: ", what, " is missing on the first kept record of ",
         sum(first_na_later_value), " subject(s) (",
         paste(utils::head(as.character(ids)[first_na_later_value], 5), collapse = ", "),
         ") but present on a later kept record. The subject snapshot is the ",
         "first kept record per subject; a value that appears only later ",
         "cannot be represented by it. ESCALATE to the SME.", call. = FALSE)
  invisible(TRUE)
}

# Classification of one numeric vector for a log axis (round 7, finding 9):
# "missing" (NA or NaN), "zero", "negative" (finite), "nonfinite" (Inf, -Inf),
# "positive" (finite > 0). Shared by the audit and the built-layer
# reconciliation, so both apply the same rule by construction.
.eda_log_class <- function(x) {
  out <- rep("positive", length(x))
  out[is.na(x)] <- "missing"
  fin <- !is.na(x) & is.finite(x)
  out[fin & x == 0] <- "zero"
  out[fin & x < 0] <- "negative"
  out[!is.na(x) & !is.finite(x)] <- "nonfinite"
  out
}

# TRUE where a value is non-finite after the declared axis transform: for
# identity, NA/NaN/Inf; for log10, additionally zero and negative values. For a
# non-numeric (discrete) column only NA is non-finite.
.eda_nonfinite_after <- function(x, transform) {
  if (!is.numeric(x)) return(is.na(x))
  cls <- .eda_log_class(x)
  if (identical(transform, "log10")) cls != "positive"
  else cls %in% c("missing", "nonfinite")
}

# ===========================================================================
# UNIT: record_selection
# ===========================================================================

# The canonical EDA view. ONE object that every table, figure and audit
# downstream consumes.
#
# Inputs
#   data                the analysis dataset as read, one row per record.
#   id_col              subject identifier column (yspec flag eda_id).
#   exclusion_col       record-exclusion flag column (flag eda_exclusion).
#   event_col           event-type column (flag eda_event).
#   observation_values  the values of `event_col` that mark an OBSERVATION
#                       record, resolved by the caller from the event column's
#                       yspec `decode:` annotation.
#   dose_values         the values of `event_col` that mark a DOSE record,
#                       resolved the same way. Both are asserted against the
#                       NONMEM convention (observation = 0, dose = 1) by
#                       tools/record_selection.R, so QC, EDA and fitting agree.
#   row_id_col          name of the stable row id stamped on every record.
#
# Returns a plain list:
#   row_id_col, selector, selector_digest
#   rows$analysis       kept records
#   rows$observations   kept records whose event value is an observation
#   rows$doses          kept records whose event value is a dose
#   rows$subjects       the first kept record per subject, in input order
#   rows$excluded       records the kept-record rule excludes (never dropped)
#   digests, n_rows     one sha256 and one count per view above
#   exclusions          data.frame(reason, n): the exclusion reasons found
#   n_input, input_counts
#   parameters          data.frame(parameter, value): the method parameters
#                       this unit applied (round 7, finding 3)
reference_eda_view <- function(data,
                               id_col,
                               exclusion_col,
                               event_col,
                               observation_values,
                               dose_values,
                               row_id_col = "EDA_ROW") {
  priv <- .eda_gate("record_selection", "EDA record selections")
  # The sealed policy environment, captured before any argument is forced
  # (integrity batch 16, R15-P1-1 / D16-1).
  .P <- .eda_policy

  if (!is.data.frame(data))
    stop("reference_eda_view(): `data` must be a data frame.", call. = FALSE)
  for (nm in c("id_col", "exclusion_col", "event_col", "row_id_col")) {
    v <- get(nm)
    if (!is.character(v) || length(v) != 1L || is.na(v) || !nzchar(v))
      stop("reference_eda_view(): `", nm, "` must be a single column name.",
           call. = FALSE)
  }
  if (length(observation_values) == 0L || missing(dose_values) ||
      length(dose_values) == 0L)
    stop("reference_eda_view(): `observation_values` and `dose_values` must ",
         "both be resolved from the specification's event decodes; neither ",
         "may be empty or assumed.", call. = FALSE)
  priv$harness_assert_event_convention(
    observation_values, dose_values,
    source = paste0("the event column '", event_col, "'"))
  .eda_require_cols(data, c(id_col, exclusion_col, event_col),
                    "EDA record selection")
  if (row_id_col %in% names(data))
    stop("reference_eda_view(): the data already has a column named '",
         row_id_col, "'; choose another row_id_col so the stamped row id ",
         "cannot be confused with a data column.", call. = FALSE)

  d <- data
  d[[row_id_col]] <- seq_len(nrow(d))

  keep <- .eda_kept(d[[exclusion_col]], priv)
  analysis <- d[keep, , drop = FALSE]
  excluded <- d[!keep, , drop = FALSE]
  if (nrow(analysis) == 0L)
    stop("reference_eda_view(): every one of the ", nrow(d), " records is ",
         "excluded by column '", exclusion_col, "'. Either the dataset ",
         "contains no analysis records, or that column was read with a ",
         "different missing-value convention than the kept-record rule (",
         priv$HARNESS_RECORD_SELECTION_RULE, "). ESCALATE to the SME.",
         call. = FALSE)

  reasons <- .eda_exclusion_reason(excluded[[exclusion_col]], priv)
  exclusions <- if (length(reasons)) {
    lv <- sort(unique(reasons))
    data.frame(reason = lv,
               n = vapply(lv, function(r) sum(reasons == r), integer(1)),
               stringsAsFactors = FALSE)
  } else data.frame(reason = character(0), n = integer(0), stringsAsFactors = FALSE)
  rownames(exclusions) <- NULL

  ev <- as.character(analysis[[event_col]])
  observations <- analysis[ev %in% as.character(observation_values), , drop = FALSE]
  doses        <- analysis[ev %in% as.character(dose_values), , drop = FALSE]
  subjects     <- analysis[!duplicated(analysis[[id_col]]), , drop = FALSE]

  selector <- list(
    id_col             = id_col,
    exclusion_col      = exclusion_col,
    exclusion_rule     = priv$HARNESS_RECORD_SELECTION_RULE,
    event_col          = event_col,
    observation_values = as.character(observation_values),
    dose_values        = as.character(dose_values),
    subject_rule       = "first kept record per subject, in input order",
    row_id_col         = row_id_col)
  selector_digest <- .eda_digest(unlist(selector, use.names = TRUE))
  dig <- function(v) .eda_digest(c(selector_digest,
                                   sort(unique(v[[row_id_col]]))))
  views <- list(analysis = analysis, observations = observations,
                doses = doses, subjects = subjects, excluded = excluded)

  parameters <- data.frame(
    parameter = c("kept-record rule", "missing tokens (after trimming)",
                  "observation event values", "dose event values",
                  "event convention", "subject snapshot", "row id",
                  paste0("dependency ", base::get("dependency_files", envir = .P),
                         " sha256")),
    value = c(priv$HARNESS_RECORD_SELECTION_RULE,
              paste(sprintf('"%s"', priv$HARNESS_MISSING_TOKENS), collapse = ", "),
              paste(as.character(observation_values), collapse = ", "),
              paste(as.character(dose_values), collapse = ", "),
              sprintf("NONMEM: observation = %s, dose = %s (asserted against the specification's decodes)",
                      priv$HARNESS_EVENT_OBSERVATION,
                      priv$HARNESS_EVENT_DOSE),
              "first kept record per subject, in input order (any event type)",
              paste0(row_id_col, " = position of the record in the dataset as read"),
              unname(priv$dependency_sha256)),
    stringsAsFactors = FALSE)

  list(row_id_col      = row_id_col,
       selector        = selector,
       selector_digest = selector_digest,
       rows            = views,
       digests         = vapply(views, dig, character(1)),
       n_rows          = vapply(views, nrow, integer(1)),
       exclusions      = exclusions,
       all_records     = d,
       n_input         = nrow(data),
       input_counts    = data.frame(
         id = c("rows", "columns", "subjects"),
         quantity = c("rows read", "columns read",
                      "subjects (unique ID) read"),
         value = c(nrow(data), ncol(data), length(unique(data[[id_col]]))),
         stringsAsFactors = FALSE),
       parameters      = parameters)
}

# The digest of an arbitrary frame under a view's selector.
reference_eda_rowset_digest <- function(view, rows) {
  .eda_gate("record_selection", "EDA row-set identity checks")
  if (!is.list(view) || is.null(view$selector_digest) || is.null(view$row_id_col))
    stop("reference_eda_rowset_digest(): `view` must be the object returned ",
         "by reference_eda_view().", call. = FALSE)
  .eda_require_cols(rows, view$row_id_col, "the row-set identity check")
  .eda_digest(c(view$selector_digest,
                sort(unique(rows[[view$row_id_col]]))))
}

# ===========================================================================
# UNIT: summary_statistics
# ===========================================================================

.eda_check_view <- function(view, fn) {
  if (!is.list(view) || is.null(view$rows) || is.null(view$all_records))
    stop(fn, "(): `view` must be the object returned by reference_eda_view().",
         call. = FALSE)
  invisible(TRUE)
}

# Data inventory: subjects, observation records, BLQ observations, excluded
# records and records read, per declared grouping.
#
#   blq_col / blq_values  the BLQ flag and the values meaning below LLOQ,
#                 resolved from the specification. NULL blq_col -> n_blq is NA.
#   group_cols    zero or more grouping columns (e.g. study, dose group). Every
#                 count is GROUP-SPECIFIC: a record belongs to the group named
#                 by its own values of the grouping columns, excluded records
#                 included, so n_excluded and n_records_read add up over groups.
#   blq_missing   policy for a missing BLQ flag on an observation: "stop" (the
#                 default; an observation of unknown BLQ status is a data
#                 question) or "not_blq" (counted as not BLQ, stated).
# Returns list(table, parameters).
reference_eda_inventory <- function(view,
                                    blq_col = NULL,
                                    blq_values = 1,
                                    group_cols = character(0),
                                    overall_label = "all",
                                    blq_missing = c("stop", "not_blq")) {
  .eda_gate("summary_statistics", "EDA summary statistics")
  .eda_check_view(view, "reference_eda_inventory")
  blq_missing <- match.arg(blq_missing)
  all_rec <- view$all_records
  obs <- view$rows$observations
  exc <- view$rows$excluded
  id_col <- view$selector$id_col
  rid <- view$row_id_col
  if (!is.null(blq_col)) {
    .eda_require_cols(obs, blq_col, "the BLQ count")
    if (anyNA(obs[[blq_col]]) && identical(blq_missing, "stop"))
      stop("reference_eda_inventory(): ", sum(is.na(obs[[blq_col]])),
           " observation record(s) have a missing BLQ flag '", blq_col,
           "'. An observation of unknown BLQ status is a data question for the ",
           "assembly stage; the declared policy is to stop rather than count it ",
           "as not-BLQ. ESCALATE to the SME.", call. = FALSE)
  }
  if (length(group_cols)) .eda_require_cols(all_rec, group_cols, "the declared grouping")

  one <- function(o, e, a, label_row) {
    n_blq <- if (is.null(blq_col)) NA_integer_ else
      sum(as.character(o[[blq_col]]) %in% as.character(blq_values))
    cbind(label_row,
          data.frame(n_subjects     = length(unique(o[[id_col]])),
                     n_obs          = nrow(o),
                     n_blq          = n_blq,
                     n_excluded     = nrow(e),
                     n_records_read = nrow(a),
                     stringsAsFactors = FALSE))
  }

  if (!length(group_cols)) {
    out <- one(obs, exc, all_rec,
               data.frame(group = overall_label, stringsAsFactors = FALSE))
  } else {
    key_of <- function(fr) do.call(paste, c(lapply(group_cols, function(g)
      as.character(fr[[g]])), list(sep = "\037")))
    kall <- key_of(all_rec)
    first <- all_rec[!duplicated(kall), group_cols, drop = FALSE]
    ord <- do.call(order, unname(as.list(first)))
    first <- first[ord, , drop = FALSE]
    keys <- kall[!duplicated(kall)][ord]
    ko <- key_of(obs); ke <- key_of(exc)
    out <- do.call(rbind, lapply(seq_along(keys), function(i) {
      label_row <- first[i, , drop = FALSE]; rownames(label_row) <- NULL
      one(obs[ko == keys[i], , drop = FALSE], exc[ke == keys[i], , drop = FALSE],
          all_rec[kall == keys[i], , drop = FALSE], label_row)
    }))
  }
  rownames(out) <- NULL
  parameters <- data.frame(
    parameter = c("n_subjects", "n_obs", "n_blq", "BLQ missing-flag policy",
                  "n_excluded", "n_records_read", "grouping"),
    value = c("distinct subject identifiers among the group's observation records",
              "observation records (kept, event = observation) in the group",
              if (is.null(blq_col)) "NA: no BLQ flag declared" else
                sprintf("observation records whose '%s' is in {%s}", blq_col,
                        paste(as.character(blq_values), collapse = ", ")),
              if (identical(blq_missing, "stop"))
                "a missing BLQ flag on an observation stops"
              else "a missing BLQ flag on an observation counts as not BLQ",
              "records excluded by the kept-record rule whose own grouping values place them in the group",
              "all records read whose grouping values place them in the group",
              if (length(group_cols)) paste(group_cols, collapse = ", ") else
                "none: one row over the whole dataset"),
    stringsAsFactors = FALSE)
  list(table = out, parameters = parameters)
}

# Continuous-covariate summary. Returns list(table, parameters).
#
#   rows      "subjects" (the first kept record per subject; the covariate is
#             asserted CONSTANT within subject across kept records, otherwise
#             the snapshot would silently pick one of several values),
#             "observations" or "analysis".
#   digits    rounding applied to mean/median/sd for presentation, or NULL.
reference_eda_continuous_summary <- function(view, columns,
                                             rows = c("subjects",
                                                      "observations",
                                                      "analysis"),
                                             digits = NULL) {
  .eda_gate("summary_statistics", "EDA summary statistics")
  rows <- match.arg(rows)
  .eda_check_view(view, "reference_eda_continuous_summary")
  if (!is.character(columns) || !length(columns))
    stop("reference_eda_continuous_summary(): `columns` must name at least ",
         "one continuous covariate column. An analysis with none records the ",
         "continuous-covariate summary as inapplicable instead of calling ",
         "this with an empty set.", call. = FALSE)
  if (!is.null(digits) && (!is.numeric(digits) || length(digits) != 1L))
    stop("reference_eda_continuous_summary(): `digits` must be NULL or one number.",
         call. = FALSE)
  d <- view$rows[[rows]]
  .eda_require_cols(d, columns, "the continuous covariate summary")
  if (nrow(d) == 0L)
    stop("reference_eda_continuous_summary(): the '", rows, "' view is empty; ",
         "there is nothing to summarise. ESCALATE rather than reporting a ",
         "summary over no records.", call. = FALSE)
  id_col <- view$selector$id_col

  rnd <- function(x) if (is.null(digits)) x else round(x, digits)
  out <- do.call(rbind, lapply(columns, function(v) {
    x <- d[[v]]
    if (!is.numeric(x))
      stop("reference_eda_continuous_summary(): column '", v, "' is ",
           class(x)[1], ", not numeric. It is declared a CONTINUOUS covariate ",
           "by the specification; reconcile the specification and the data.",
           call. = FALSE)
    if (any(!is.na(x) & !is.finite(x)))
      stop("reference_eda_continuous_summary(): column '", v, "' holds ",
           sum(!is.na(x) & !is.finite(x)), " non-finite value(s) (Inf/-Inf) in ",
           "the '", rows, "' view. The declared policy is to stop: a non-finite ",
           "covariate is a data defect, not a value to summarise. ESCALATE.",
           call. = FALSE)
    if (identical(rows, "subjects")) {
      a <- view$rows$analysis
      xa <- a[[v]]
      .eda_first_value(xa, a[[id_col]], paste0("covariate '", v, "'"))
      varies <- tapply(xa, a[[id_col]], function(z) length(unique(z[!is.na(z)])) > 1L)
      if (any(varies, na.rm = TRUE))
        stop("reference_eda_continuous_summary(): column '", v, "' varies within ",
             "subject for ", sum(varies, na.rm = TRUE), " subject(s) (",
             paste(utils::head(names(varies)[which(varies)], 5), collapse = ", "),
             "). The subject snapshot (first kept record per subject) is only a ",
             "valid denominator for a covariate that is constant within subject; ",
             "a time-varying covariate needs its own declared summary. ESCALATE.",
             call. = FALSE)
    }
    xo <- x[!is.na(x)]
    if (!length(xo))
      stop("reference_eda_continuous_summary(): column '", v, "' has no ",
           "non-missing value in the '", rows, "' view; a mean, minimum and ",
           "maximum over nothing are not results. ESCALATE.", call. = FALSE)
    data.frame(variable  = v,
               rows      = rows,
               n         = length(xo),
               n_missing = sum(is.na(x)),
               mean      = rnd(mean(xo)),
               median    = rnd(stats::median(xo)),
               sd        = rnd(stats::sd(xo)),
               min       = min(xo),
               max       = max(xo),
               stringsAsFactors = FALSE)
  }))
  rownames(out) <- NULL
  parameters <- data.frame(
    parameter = c("rows", "sd", "median", "missing values", "non-finite values",
                  "rounding", "subject snapshot"),
    value = c(rows,
              "sample standard deviation, denominator n - 1 (stats::sd); NA when n = 1",
              "stats::median: the middle order statistic; for even n the arithmetic mean of the two central values",
              "NA and NaN are removed before every statistic and counted in n_missing",
              "Inf and -Inf stop (data defect), never summarised",
              if (is.null(digits)) "none: values as computed" else
                sprintf("mean, median and sd rounded to %s decimal place(s) for presentation; n, n_missing, min and max as computed", digits),
              if (identical(rows, "subjects"))
                "first kept record per subject in input order; each covariate is asserted constant within subject across kept records, and a value missing on that first record but present on a later kept record stops"
              else "not applicable: one row per record of the view"),
    stringsAsFactors = FALSE)
  list(table = out, parameters = parameters)
}

# Categorical-covariate summary: n and percent per level, one row per level
# per declared column. Returns list(table, parameters).
#
#   columns   the categorical covariate columns (flag eda_covariate_categorical).
#             EMPTY IS REFUSED: an analysis whose specification flags none
#             records the deliverable as inapplicable (with that reason) rather
#             than calling this with nothing.
#   decodes   optional named list, column -> data.frame(value, decode) from the
#             specification, used to label levels. Levels are the observed
#             non-missing values (as character), sorted.
#   digits    rounding of the percent for presentation.
reference_eda_categorical_summary <- function(view, columns,
                                              rows = c("subjects",
                                                       "observations",
                                                       "analysis"),
                                              decodes = NULL, digits = 1) {
  .eda_gate("summary_statistics", "EDA summary statistics")
  rows <- match.arg(rows)
  .eda_check_view(view, "reference_eda_categorical_summary")
  if (!is.character(columns) || !length(columns))
    stop("reference_eda_categorical_summary(): `columns` is empty. The ",
         "categorical-covariate summary is recorded as inapplicable (with the ",
         "reason: no column flagged eda_covariate_categorical) by the caller; ",
         "it is not computed over nothing.", call. = FALSE)
  d <- view$rows[[rows]]
  .eda_require_cols(d, columns, "the categorical covariate summary")
  if (nrow(d) == 0L)
    stop("reference_eda_categorical_summary(): the '", rows, "' view is empty.",
         call. = FALSE)
  out <- do.call(rbind, lapply(columns, function(v) {
    x <- as.character(d[[v]])
    if (identical(rows, "subjects")) {
      a <- view$rows$analysis
      .eda_first_value(as.character(a[[v]]), a[[view$selector$id_col]],
                       paste0("categorical covariate '", v, "'"))
    }
    xo <- x[!is.na(x)]
    if (!length(xo))
      stop("reference_eda_categorical_summary(): column '", v, "' has no ",
           "non-missing value in the '", rows, "' view. ESCALATE.", call. = FALSE)
    lv <- sort(unique(xo))
    dec <- decodes[[v]]
    lab <- if (is.null(dec)) rep(NA_character_, length(lv)) else {
      m <- match(lv, as.character(dec$value))
      ifelse(is.na(m), NA_character_, as.character(dec$decode)[m])
    }
    n <- vapply(lv, function(l) sum(xo == l), integer(1))
    data.frame(variable = v, rows = rows, level = lv, decode = lab, n = unname(n),
               percent = round(100 * unname(n) / length(xo), digits),
               n_nonmissing = length(xo), n_missing = sum(is.na(x)),
               stringsAsFactors = FALSE)
  }))
  rownames(out) <- NULL
  parameters <- data.frame(
    parameter = c("rows", "subject snapshot", "levels", "percent denominator", "rounding", "decodes"),
    value = c(rows,
              if (identical(rows, "subjects"))
                "first kept record per subject; a value missing on that record but present on a later kept record stops"
              else "not applicable: one row per record of the view",
              "the distinct non-missing values observed in the view, as character, sorted",
              "records of the view with a non-missing value of the column (n_nonmissing); missing values are counted separately",
              sprintf("percent rounded to %s decimal place(s); n as computed", digits),
              if (is.null(decodes)) "none supplied" else
                "level labels from the specification's values/decode annotation"),
    stringsAsFactors = FALSE)
  list(table = out, parameters = parameters)
}

# ===========================================================================
# UNIT: log_axis_audit
# ===========================================================================

# The census before any log-scale figure, on one column of one view (round 7,
# finding 9: classified by row id, with BLQ status).
#
# Returns list(rows, axis, column, transform, row_digest,
#              audit = data.frame(id, quantity, value),
#              detail = data.frame(row_id, value, value_class, blq)) where
# `detail` lists exactly the rows the axis drops.
reference_eda_log_axis_audit <- function(view, value_col,
                                         rows = c("observations", "analysis",
                                                  "subjects"),
                                         axis = c("y", "x"),
                                         blq_col = NULL, blq_values = NULL,
                                         transform = "log10") {
  .eda_gate("log_axis_audit", "EDA log-axis audits")
  rows <- match.arg(rows)
  axis <- match.arg(axis)
  if (!identical(transform, "log10"))
    stop("reference_eda_log_axis_audit(): the only audited transform is ",
         "'log10'.", call. = FALSE)
  .eda_check_view(view, "reference_eda_log_axis_audit")
  d <- view$rows[[rows]]
  .eda_require_cols(d, value_col, "the log-axis audit")
  x <- d[[value_col]]
  if (!is.numeric(x))
    stop("reference_eda_log_axis_audit(): column '", value_col, "' is ",
         class(x)[1], ", not numeric; a log axis is not defined for it.",
         call. = FALSE)
  cls <- .eda_log_class(x)
  if (is.null(blq_col)) {
    is_blq <- rep(NA, length(x))
  } else {
    .eda_require_cols(d, blq_col, "the BLQ cross-count of the log-axis audit")
    if (is.null(blq_values))
      stop("reference_eda_log_axis_audit(): `blq_col` was given without ",
           "`blq_values`; which value of the flag means below LLOQ is ",
           "resolved from the specification, not assumed.", call. = FALSE)
    is_blq <- as.character(d[[blq_col]]) %in% as.character(blq_values)
  }
  dropped <- cls != "positive"
  n_zero <- sum(cls == "zero"); n_neg <- sum(cls == "negative")
  n_nonpos_blq <- if (is.null(blq_col)) NA_integer_ else
    sum(cls %in% c("zero", "negative") & is_blq)
  audit <- data.frame(
    id = c("n_rows", "n_missing", "n_zero", "n_negative", "n_nonfinite",
           "n_nonpositive", "n_nonpositive_and_blq", "n_dropped_by_log"),
    quantity = c(
      paste0("rows in the '", rows, "' EDA view"),
      paste0("missing ", value_col, " among them (NA or NaN)"),
      paste0(value_col, " exactly zero"),
      paste0(value_col, " negative (finite)"),
      paste0(value_col, " non-finite (Inf or -Inf)"),
      paste0("non-positive ", value_col, " (zero + negative)"),
      "of the non-positive values, also flagged BLQ (NA when no BLQ flag was declared)",
      "rows a log axis drops (missing + zero + negative + non-finite)"),
    value = c(nrow(d), sum(cls == "missing"), n_zero, n_neg, sum(cls == "nonfinite"),
              n_zero + n_neg, n_nonpos_blq, sum(dropped)),
    stringsAsFactors = FALSE)
  detail <- data.frame(row_id = d[[view$row_id_col]][dropped],
                       value = x[dropped],
                       value_class = cls[dropped],
                       blq = is_blq[dropped],
                       stringsAsFactors = FALSE)
  rownames(detail) <- NULL
  list(rows = rows, axis = axis, column = value_col, transform = transform,
       row_digest = .eda_digest(c(view$selector_digest,
                                  sort(unique(d[[view$row_id_col]])))),
       audit = audit, detail = detail)
}

# One audit per log axis the plot registry DECLARES, as a table keyed by
# figure and axis, plus the row-level detail of every dropped value.
# `requests`: list of list(plot, axis, rows, column).
# Returns list(table, detail, total_dropped, n_requests).
reference_eda_log_axis_audit_table <- function(view, requests,
                                               blq_col = NULL,
                                               blq_values = NULL) {
  .eda_gate("log_axis_audit", "EDA log-axis audits")
  if (!is.list(requests))
    stop("reference_eda_log_axis_audit_table(): `requests` must be a list of ",
         "declarations.", call. = FALSE)
  empty <- data.frame(plot = character(0), axis = character(0),
                      rows = character(0), column = character(0),
                      n_rows = numeric(0), n_missing = numeric(0),
                      n_zero = numeric(0), n_negative = numeric(0),
                      n_nonfinite = numeric(0), n_nonpositive = numeric(0),
                      n_nonpositive_and_blq = numeric(0),
                      n_dropped_by_log = numeric(0), row_digest = character(0),
                      stringsAsFactors = FALSE)
  empty_detail <- data.frame(plot = character(0), axis = character(0),
                             rows = character(0), column = character(0),
                             row_id = integer(0), value = numeric(0),
                             value_class = character(0), blq = logical(0),
                             stringsAsFactors = FALSE)
  if (!length(requests))
    return(list(table = empty, detail = empty_detail, total_dropped = 0,
                n_requests = 0L))

  res <- lapply(requests, function(r) {
    need <- c("plot", "axis", "rows", "column")
    absent <- setdiff(need, names(r))
    if (length(absent))
      stop("reference_eda_log_axis_audit_table(): a request is missing ",
           paste(absent, collapse = ", "), ".", call. = FALSE)
    a <- reference_eda_log_axis_audit(view, value_col = r$column,
                                      rows = r$rows, axis = r$axis,
                                      blq_col = blq_col,
                                      blq_values = blq_values)
    v <- function(id) a$audit$value[a$audit$id == id]
    row <- data.frame(plot = r$plot, axis = a$axis, rows = a$rows, column = a$column,
                      n_rows = v("n_rows"), n_missing = v("n_missing"),
                      n_zero = v("n_zero"), n_negative = v("n_negative"),
                      n_nonfinite = v("n_nonfinite"),
                      n_nonpositive = v("n_nonpositive"),
                      n_nonpositive_and_blq = v("n_nonpositive_and_blq"),
                      n_dropped_by_log = v("n_dropped_by_log"),
                      row_digest = a$row_digest, stringsAsFactors = FALSE)
    det <- if (nrow(a$detail)) cbind(data.frame(plot = r$plot, axis = a$axis,
                                                rows = a$rows, column = a$column,
                                                stringsAsFactors = FALSE),
                                     a$detail) else empty_detail
    list(row = row, detail = det)
  })
  out <- do.call(rbind, lapply(res, `[[`, "row"))
  det <- do.call(rbind, lapply(res, `[[`, "detail"))
  rownames(out) <- NULL; rownames(det) <- NULL
  list(table = out, detail = det, total_dropped = sum(out$n_dropped_by_log),
       n_requests = nrow(out))
}

# ===========================================================================
# UNIT: display_transforms
# ===========================================================================

# Quantile binning of a continuous covariate.
#   probs          quantile probabilities, strictly increasing from 0 to 1.
#   quantile_type  the R quantile definition (stats::quantile `type`), declared.
#   breaks_from    the frame the quantiles are taken from (NULL = `data`).
# Intervals are right-closed, the lowest including its left end:
# [b0, b1], (b1, b2], ... (cut(include.lowest = TRUE, right = TRUE)).
# Returns list(data, breaks, levels, column, breaks_rows, n_breaks_rows,
#              parameters).
reference_eda_quantile_bins <- function(data, column, probs, labels = NULL,
                                        into = paste0(column, "_BIN"),
                                        breaks_from = NULL,
                                        quantile_type = 7L) {
  .eda_gate("display_transforms", "EDA display transforms")
  .eda_require_cols(data, column, "quantile binning")
  if (!is.numeric(quantile_type) || length(quantile_type) != 1L ||
      !quantile_type %in% 1:9)
    stop("reference_eda_quantile_bins(): `quantile_type` must be one of R's ",
         "quantile definitions 1..9.", call. = FALSE)
  if (!is.null(breaks_from)) {
    if (!is.data.frame(breaks_from))
      stop("reference_eda_quantile_bins(): `breaks_from` must be a data ",
           "frame (the rows the quantiles are taken from).", call. = FALSE)
    .eda_require_cols(breaks_from, column,
                      "the frame the quantile cut points are taken from")
  }
  if (!is.numeric(probs) || length(probs) < 3L ||
      !isTRUE(all.equal(probs[1], 0)) ||
      !isTRUE(all.equal(probs[length(probs)], 1)) ||
      any(diff(probs) <= 0))
    stop("reference_eda_quantile_bins(): `probs` must be strictly increasing ",
         "from 0 to 1 and define at least two bins.", call. = FALSE)
  x <- data[[column]]
  if (!is.numeric(x))
    stop("reference_eda_quantile_bins(): column '", column, "' is not numeric.",
         call. = FALSE)
  src <- if (is.null(breaks_from)) data else breaks_from
  xb <- src[[column]]
  if (!is.numeric(xb))
    stop("reference_eda_quantile_bins(): column '", column, "' is not numeric ",
         "in the frame the cut points are taken from.", call. = FALSE)
  brk <- stats::quantile(xb, probs = probs, na.rm = TRUE, names = FALSE,
                         type = quantile_type)
  if (anyDuplicated(brk))
    stop("reference_eda_quantile_bins(): the requested quantiles of '", column,
         "' are not distinct (", paste(signif(brk, 6), collapse = ", "),
         "); the covariate does not support this many bins. ESCALATE rather ",
         "than silently merging bins.", call. = FALSE)
  if (is.null(labels))
    labels <- paste0(column, " Q", seq_len(length(probs) - 1L))
  if (length(labels) != length(probs) - 1L)
    stop("reference_eda_quantile_bins(): ", length(labels), " label(s) for ",
         length(probs) - 1L, " bin(s).", call. = FALSE)
  binned <- cut(x, breaks = brk, include.lowest = TRUE, right = TRUE,
                labels = labels)
  counts <- table(binned)
  if (any(counts == 0L))
    stop("reference_eda_quantile_bins(): bin(s) ",
         paste(names(counts)[counts == 0L], collapse = ", "),
         " would contain no record of '", column,
         "'. A stratum with nothing in it is not a view; widen the bins or ",
         "change the denominator. ESCALATE to the SME.", call. = FALSE)
  n_outside <- sum(is.na(binned) & !is.na(x))
  if (n_outside > 0L)
    stop("reference_eda_quantile_bins(): ", n_outside, " record(s) of '",
         column, "' fall outside the cut points, which were taken from a ",
         "different set of rows than the ones being binned. Every record ",
         "must land in a stratum. ESCALATE to the SME.", call. = FALSE)
  data[[into]] <- binned
  list(data = data,
       breaks = data.frame(probability = probs, value = brk,
                           stringsAsFactors = FALSE),
       levels = as.character(labels),
       column = into,
       breaks_rows = if (is.null(breaks_from)) "the rows being binned"
                     else "a declared frame",
       n_breaks_rows = nrow(src),
       parameters = data.frame(
         parameter = c("quantile type", "interval closure", "missing values"),
         value = c(sprintf("R type %d (stats::quantile)", as.integer(quantile_type)),
                   "right-closed, lowest bin including its left end: [b0, b1], (b1, b2], ...",
                   "a missing covariate value has no stratum (NA) and is neither dropped nor counted in a bin"),
         stringsAsFactors = FALSE))
}

# Normalisation of a value column by another: value / by * scale.
reference_eda_normalize <- function(data, column, by, scale = 1,
                                    into = paste0(column, "_NORM")) {
  .eda_gate("display_transforms", "EDA display transforms")
  .eda_require_cols(data, c(column, by), "normalisation")
  if (!is.numeric(scale) || length(scale) != 1L || !is.finite(scale))
    stop("reference_eda_normalize(): `scale` must be one finite number.",
         call. = FALSE)
  num <- data[[column]]; den <- data[[by]]
  if (!is.numeric(num) || !is.numeric(den))
    stop("reference_eda_normalize(): both '", column, "' and '", by,
         "' must be numeric.", call. = FALSE)
  bad <- is.na(den) | den == 0
  out <- rep(NA_real_, length(num))
  out[!bad] <- num[!bad] / den[!bad] * scale
  data[[into]] <- out
  list(data = data, n_undefined = sum(bad), column = into,
       parameters = data.frame(
         parameter = c("definition", "undefined divisor"),
         value = c(sprintf("%s / %s * %s", column, by, format(scale)),
                   "a zero or missing divisor gives a missing result (counted in n_undefined), never a dropped record"),
         stringsAsFactors = FALSE))
}

# ===========================================================================
# UNIT: figure_construction
# ===========================================================================

# The declared shape of a plot-registry entry (kept as data so the validator,
# the builder and SKILL.md cannot drift apart).
EDA_REGISTRY_FIELDS <- c("tier", "file", "w", "h", "rows", "x_transform",
                         "y_transform", "kind")
EDA_REGISTRY_TIERS  <- c("core", "catalog")
EDA_REGISTRY_ROWS   <- c("analysis", "observations", "subjects")
EDA_MARKER_ROWS     <- c("doses", EDA_REGISTRY_ROWS)
EDA_REGISTRY_TRANSFORMS <- c("identity", "log10")
EDA_FIGURE_KINDS <- c("profiles", "individual_profiles", "histograms", "pairs",
                      "boxplots")
EDA_DERIVE_KINDS <- c("quantile_bins", "normalize")
EDA_SMOOTH_METHODS <- "loess"
EDA_BIN_RULES <- "equal_width"
EDA_CORRELATION_METHODS <- c("pearson", "spearman")
EDA_CORRELATION_USE <- c("complete.obs", "pairwise.complete.obs")
EDA_INAPPLICABLE_PREFIX <- "inapplicable: "

# Validates ONE registry entry's declaration. Returns the entry (with defaults
# filled) or stops with the entry name and the defect.
# `.P` IS REQUIRED, NOT DEFAULTED (integrity batch 16 fix round 1, review
# blocker B1). This helper enforces eleven registry policy vectors and is
# reached from three approved entry points, so a caller's argument promise could
# widen one of them after the gate had already run: the reviewer's fixture made
# reference_eda_registry_check() return a table carrying an undeclared tier.
# The sealed policy environment is now handed IN by the gated body, which
# captured it immediately after its own `.eda_gate()` call and before any of its
# arguments could be forced. A default of `.eda_policy` would not do: a default
# argument's promise is evaluated in this frame at first use, which is after the
# caller's promises have run, so it would read whatever the top-level symbol
# names by then. Every read below goes through `.P`, and the set operations that
# decide a refusal are `base::`-qualified (D16-2): an operator or a base
# function is an ordinary binding a caller can shadow in the loading
# environment.
.eda_check_entry <- function(nm, p, .P) {
  if (!base::is.environment(.P))
    stop("eda-standards: .eda_check_entry() was called without the sealed policy ",
         "environment its gated caller captured. ESCALATE to the SME.", call. = FALSE)
  registry_fields      <- base::get("registry_fields", envir = .P)
  registry_tiers       <- base::get("registry_tiers", envir = .P)
  registry_rows        <- base::get("registry_rows", envir = .P)
  registry_transforms  <- base::get("registry_transforms", envir = .P)
  figure_kinds         <- base::get("figure_kinds", envir = .P)
  derive_kinds         <- base::get("derive_kinds", envir = .P)
  marker_rows          <- base::get("marker_rows", envir = .P)
  smooth_methods       <- base::get("smooth_methods", envir = .P)
  bin_rules            <- base::get("bin_rules", envir = .P)
  correlation_methods  <- base::get("correlation_methods", envir = .P)
  correlation_use      <- base::get("correlation_use", envir = .P)
  bad <- function(...) stop("plot-registry entry '", nm, "' ", ..., call. = FALSE)
  if (!is.list(p)) bad("is not a list.")
  is_fn <- vapply(p, is.function, logical(1))
  if (any(is_fn) || "fn" %in% names(p))
    bad("carries an R function (field(s) ",
        paste(unique(c(names(p)[is_fn], intersect("fn", names(p)))), collapse = ", "),
        "). The registry is DECLARATIVE: a figure is a kind, roles, transforms ",
        "and parameters, built by the eda-standards reference. No arbitrary ",
        "code.")
  missing_f <- base::setdiff(registry_fields, base::names(p))
  if (length(missing_f))
    bad("is missing field(s): ", paste(missing_f, collapse = ", "), ".")
  if (!base::is.element(p$tier, registry_tiers))
    bad("has tier '", p$tier, "'; expected one of ",
        paste(registry_tiers, collapse = ", "), ".")
  if (!is.character(p$file) || length(p$file) != 1L || !nzchar(p$file) ||
      !grepl("\\.pdf$", p$file))
    bad("must declare exactly one output file name ending in .pdf.")
  if (!base::is.element(p$rows, registry_rows))
    bad("declares rows = '", p$rows, "'; expected one of ",
        paste(registry_rows, collapse = ", "), ".")
  for (ax in c("x_transform", "y_transform")) {
    if (!is.character(p[[ax]]) || length(p[[ax]]) != 1L ||
        !base::is.element(p[[ax]], registry_transforms))
      bad("declares ", ax, " = '", paste(format(p[[ax]]), collapse = ""),
          "'; expected one of ", paste(registry_transforms, collapse = ", "), ".")
  }
  if (!is.numeric(p$w) || !is.numeric(p$h) || length(p$w) != 1L ||
      length(p$h) != 1L || !is.finite(p$w) || !is.finite(p$h) || p$w <= 0 ||
      p$h <= 0)
    bad("must declare positive numeric w and h (inches).")
  if (!is.character(p$kind) || length(p$kind) != 1L || !base::is.element(p$kind, figure_kinds))
    bad("declares kind = '", paste(format(p$kind), collapse = ""),
        "'; expected one of ", paste(figure_kinds, collapse = ", "), ".")
  if (!is.null(p$requires) && !is.character(p$requires))
    bad("declares `requires` that is not a character vector of roles.")
  if (!is.null(p$derive)) {
    if (!is.list(p$derive) || is.null(p$derive$kind) ||
        !base::is.element(p$derive$kind, derive_kinds))
      bad("declares a `derive` block without a kind in {",
          paste(derive_kinds, collapse = ", "), "}.")
    if (!is.character(p$derive$into) || length(p$derive$into) != 1L)
      bad("declares a `derive` block without `into` (the derived column name).")
  }
  role1 <- function(field) {
    v <- p[[field]]
    if (!is.character(v) || length(v) != 1L || !nzchar(v))
      bad("must declare `", field, "` as one role name.")
    if (identical(v, "derived") && is.null(p$derive))
      bad("maps `", field, "` to the derived column but declares no `derive` block.")
  }
  transforms_identity <- function() {
    if (!identical(p$x_transform, "identity") || !identical(p$y_transform, "identity"))
      bad("of kind '", p$kind, "' must declare identity transforms: a log axis is ",
          "audited on the column an axis maps, and this kind maps several ",
          "columns per axis.")
  }
  switch(p$kind,
    profiles = {
      role1("x"); role1("y"); role1("group")
      if (identical(p$y, "derived") && !identical(p$y_transform, "identity"))
        bad("declares a log y transform on a DERIVED column; the log-axis audit ",
            "runs on view columns only, so such a figure is not auditable.")
      if (identical(p$x, "derived")) bad("cannot map x to the derived column.")
      if (!is.null(p$colour)) {
        cl <- p$colour
        if (!is.list(cl) || !is.character(cl$role) || length(cl$role) != 1L)
          bad("declares `colour` that is not list(role = <role>, when_absent = NULL | \"group\").")
        if (!is.null(cl$when_absent) && !identical(cl$when_absent, "group"))
          bad("declares colour$when_absent = '", cl$when_absent,
              "'; the only fallback is \"group\".")
      }
      if (!is.null(p$smooth)) {
        sm <- p$smooth
        if (!is.list(sm) || !identical(sm$method, smooth_methods) ||
            !is.numeric(sm$span) || length(sm$span) != 1L || !is.finite(sm$span) ||
            sm$span <= 0 || !is.numeric(sm$degree) || length(sm$degree) != 1L ||
            !sm$degree %in% 0:2)
          bad("declares a `smooth` block that is not list(method = \"loess\", ",
              "span = <positive number>, degree = <0, 1 or 2>). Every smooth is ",
              "computed by the reference with declared parameters.")
      }
    },
    individual_profiles = {
      role1("x"); role1("y"); role1("facet")
      if (identical(p$y, "derived")) bad("of kind individual_profiles cannot use a derived y.")
      mk <- p$markers
      if (!is.list(mk) || !is.character(mk$rows) || length(mk$rows) != 1L ||
          !base::is.element(mk$rows, marker_rows) || !is.character(mk$x) || length(mk$x) != 1L)
        bad("must declare `markers` = list(rows = <one of ",
            paste(marker_rows, collapse = ", "), ">, x = <role>).")
      pg <- p$page
      if (!is.list(pg) || !is.numeric(pg$per_page) || length(pg$per_page) != 1L ||
          !is.finite(pg$per_page) || pg$per_page < 1 ||
          pg$per_page != as.integer(pg$per_page))
        bad("must declare `page` = list(per_page = <integer >= 1>) (subjects per page).")
    },
    histograms = {
      transforms_identity(); role1("variables")
      bn <- p$bins
      if (!is.list(bn) || !identical(bn$rule, bin_rules) || !is.numeric(bn$n) ||
          length(bn$n) != 1L || bn$n < 2 || bn$n != as.integer(bn$n))
        bad("must declare `bins` = list(rule = \"equal_width\", n = <integer >= 2>).")
    },
    pairs = {
      transforms_identity(); role1("variables")
      bn <- p$bins
      if (!is.list(bn) || !identical(bn$rule, bin_rules) || !is.numeric(bn$n) ||
          length(bn$n) != 1L || bn$n < 2 || bn$n != as.integer(bn$n))
        bad("must declare `bins` = list(rule = \"equal_width\", n = <integer >= 2>) for the diagonal.")
      cr <- p$correlation
      if (!is.list(cr) || !base::is.element(cr$method, correlation_methods) ||
          !base::is.element(cr$use, correlation_use) || !is.numeric(cr$digits) ||
          length(cr$digits) != 1L)
        bad("must declare `correlation` = list(method = <pearson|spearman>, ",
            "use = <complete.obs|pairwise.complete.obs>, digits = <n>).")
    },
    boxplots = {
      transforms_identity(); role1("variables"); role1("by")
      bx <- p$box
      if (!is.list(bx) || !is.numeric(bx$quantile_type) || !bx$quantile_type %in% 1:9 ||
          !is.numeric(bx$whisker) || length(bx$whisker) != 1L || bx$whisker < 0)
        bad("must declare `box` = list(quantile_type = <1..9>, whisker = <IQR multiplier>).")
    })
  invisible(p)
}

# Resolves a role token to a column name from the resolved role mapping.
# "derived" -> the derive block's `into`. An unresolved role that the entry
# `requires` is an INAPPLICABLE state (returned as a reason string via the
# `inapplicable` attribute); an unresolved role it does not list is a stop.
# `.P` REQUIRED, for the same reason as `.eda_check_entry` (integrity batch 16
# fix round 1, review blocker B1): this helper is reached from
# reference_eda_build_figure and stamps EDA_INAPPLICABLE_PREFIX onto the reason
# an unresolved role carries, which is policy the caller must not be able to
# rewrite mid-call.
.eda_role_col <- function(role, cols, entry, nm, flags, .P) {
  if (!base::is.environment(.P))
    stop("eda-standards: .eda_role_col() was called without the sealed policy ",
         "environment its gated caller captured. ESCALATE to the SME.", call. = FALSE)
  if (identical(role, "derived")) return(entry$derive$into)
  v <- cols[[role]]
  if (is.null(v) || !length(v) || any(!nzchar(v))) {
    flag <- if (!is.null(flags) && role %in% names(flags)) flags[[role]] else
      paste0("eda_", role)
    if (role %in% (entry$requires %||% character(0)))
      return(structure(NA_character_, inapplicable = paste0(
        base::get("inapplicable_prefix", envir = .P),
        "role '", role, "' is unresolved: the ",
        "specification flags no column as `", flag, "`")))
    stop("plot-registry entry '", nm, "' maps role '", role, "', which the ",
         "specification does not annotate (no column flagged `", flag,
         "`) and which the entry does not list under `requires`. Add the ",
         "annotation, or declare the role under `requires` so the figure is ",
         "recorded as inapplicable when it is missing.", call. = FALSE)
  }
  v
}

# Axis label for a column from the caller-supplied label vector; a template
# "{role}" is substituted with the label of the column that role resolves to.
.eda_label <- function(x, cols, labels, template = NULL) {
  if (!is.null(template)) {
    out <- template
    for (r in names(cols)) {
      if (length(cols[[r]]) == 1L && grepl(paste0("{", r, "}"), out, fixed = TRUE))
        out <- gsub(paste0("{", r, "}"), .eda_label(cols[[r]], cols, labels), out,
                    fixed = TRUE)
    }
    return(out)
  }
  if (!x %in% names(labels))
    stop("eda reference: no axis label supplied for column '", x, "'. Labels ",
         "are derived from the specification's short label and unit by the ",
         "caller; a column without one cannot be drawn.", call. = FALSE)
  unname(labels[[x]])
}

# The LAYER PLAN of a figure kind: which geoms, in which order, drawing which
# view, mapping which frame columns to the x and y positions. The builder
# constructs exactly this and the checker verifies the built object against
# it, so the two cannot disagree about what a figure contains.
.eda_layer_plan <- function(entry) {
  L <- function(geom, rows, xcols, ycols)
    data.frame(geom = geom, rows = rows, xcols = paste(xcols, collapse = ","),
               ycols = paste(ycols, collapse = ","), stringsAsFactors = FALSE)
  plan <- switch(entry$kind,
    profiles = {
      base <- rbind(L("GeomLine", entry$rows, "EDA_X", "EDA_Y"),
                    L("GeomPoint", entry$rows, "EDA_X", "EDA_Y"))
      if (!is.null(entry$smooth))
        base <- rbind(base, L("GeomLine", entry$rows, "EDA_X", "EDA_FIT"))
      base
    },
    individual_profiles = rbind(
      L("GeomLine", entry$rows, "EDA_X", "EDA_Y"),
      L("GeomPoint", entry$rows, "EDA_X", "EDA_Y"),
      L("GeomVline", entry$markers$rows, "EDA_X", character(0))),
    histograms = L("GeomRect", entry$rows, c("EDA_LOWER", "EDA_UPPER"),
                   c("EDA_ZERO", "EDA_COUNT")),
    pairs = rbind(
      L("GeomPoint", entry$rows, "EDA_X", "EDA_Y"),
      L("GeomRect", entry$rows, c("EDA_LOWER", "EDA_UPPER"), c("EDA_ZERO", "EDA_COUNT")),
      L("GeomText", entry$rows, "EDA_X", "EDA_Y"),
      L("GeomBlank", entry$rows, "EDA_VX", "EDA_VY")),
    boxplots = rbind(
      L("GeomLinerange", entry$rows, "EDA_LEVEL", c("EDA_WLO", "EDA_WHI")),
      L("GeomCrossbar", entry$rows, "EDA_LEVEL", c("EDA_Q1", "EDA_MED", "EDA_Q3")),
      L("GeomPoint", entry$rows, "EDA_LEVEL", "EDA_Y")))
  plan$layer <- seq_len(nrow(plan))
  plan
}

# Equal-width bins over the observed range: breaks, and per value the bin
# index, its bounds and the bin count. Degenerate ranges stop.
.eda_bins <- function(x, n, what) {
  xo <- x[!is.na(x)]
  if (!length(xo))
    stop("eda reference: ", what, " has no non-missing value to bin. ESCALATE.",
         call. = FALSE)
  if (any(!is.finite(xo)))
    stop("eda reference: ", what, " holds non-finite values; bins are not ",
         "defined. ESCALATE.", call. = FALSE)
  lo <- min(xo); hi <- max(xo)
  if (hi <= lo)
    stop("eda reference: ", what, " has a single distinct value (", format(lo),
         "); an equal-width histogram is not defined. ESCALATE.", call. = FALSE)
  brk <- seq(lo, hi, length.out = n + 1L)
  idx <- as.integer(cut(x, breaks = brk, include.lowest = TRUE, right = TRUE))
  counts <- tabulate(idx[!is.na(idx)], nbins = n)
  list(breaks = brk, index = idx,
       lower = brk[idx], upper = brk[idx + 1L], count = counts[idx],
       table = data.frame(bin = seq_len(n), lower = brk[-(n + 1L)], upper = brk[-1L],
                          count = counts, stringsAsFactors = FALSE))
}

# Builds one figure from its declaration. Returns
#   list(plot, kind, inapplicable = NULL | "<reason>", pages = list(<ggplot>),
#        page_ids = list(<subject ids per page>), layers = data.frame(page,
#        layer, geom, rows, n_frame_rows, digest), statistics = list(...),
#        derived = data.frame(plot, transform, column, declared, computed),
#        parameters = data.frame(parameter, value), notes = character(),
#        transforms = c(x =, y =))
#
#   view     the object reference_eda_view() returned.
#   entry    one plot-registry entry (validated here again).
#   nm       its name.
#   cols     the resolved role -> column mapping.
#   labels   named character: column -> axis label (from the specification).
#   flags    named character: role -> yspec flag, used in inapplicable reasons.
#   decodes  optional named list: column -> data.frame(value, decode).
reference_eda_build_figure <- function(view, entry, nm, cols, labels,
                                       flags = NULL, decodes = NULL) {
  .eda_gate("figure_construction", "EDA figure construction")
  # The sealed policy environment, captured before any argument is forced
  # (integrity batch 16, R15-P1-1 / D16-1).
  .P <- .eda_policy
  .eda_inapplicable_prefix <- base::get("inapplicable_prefix", envir = .P)
  .eda_check_view(view, "reference_eda_build_figure")
  .eda_check_entry(nm, entry, .P)
  if (!is.character(labels) || is.null(names(labels)))
    stop("reference_eda_build_figure(): `labels` must be a named character ",
         "vector (column -> axis label).", call. = FALSE)
  rid <- view$row_id_col
  id_col <- view$selector$id_col
  notes <- character(0)
  params <- list()
  stats <- list()
  derived <- data.frame(plot = character(0), transform = character(0),
                        column = character(0), declared = character(0),
                        computed = character(0), stringsAsFactors = FALSE)
  addp <- function(parameter, value)
    params[[length(params) + 1L]] <<- data.frame(parameter = parameter, value = value,
                                                 stringsAsFactors = FALSE)
  inapplicable <- function(reason) {
    list(plot = nm, kind = entry$kind, inapplicable = reason, pages = list(),
         page_ids = list(),
         layers = data.frame(page = integer(0), layer = integer(0), geom = character(0),
                             rows = character(0), n_frame_rows = integer(0),
                             digest = character(0), stringsAsFactors = FALSE),
         statistics = list(), derived = derived,
         parameters = data.frame(parameter = "status", value = reason,
                                 stringsAsFactors = FALSE),
         notes = reason, transforms = c(x = entry$x_transform, y = entry$y_transform))
  }
  role <- function(r) {
    v <- .eda_role_col(r, cols, entry, nm, flags, .P)
    if (!is.null(attr(v, "inapplicable"))) return(v)
    v
  }
  # Any role the entry `requires` that is unresolved makes the whole figure
  # inapplicable, with the reason naming the flag the specification must gain.
  for (r in entry$requires %||% character(0)) {
    v <- .eda_role_col(r, cols, entry, nm, flags, .P)
    if (!is.null(attr(v, "inapplicable"))) return(inapplicable(attr(v, "inapplicable")))
  }

  d <- view$rows[[entry$rows]]
  theme <- ggplot2::theme_bw()

  # ---- declared display transform (gated display_transforms unit) ----------
  if (!is.null(entry$derive)) {
    der <- entry$derive
    if (identical(der$kind, "quantile_bins")) {
      col <- role(der$role)
      if (!is.null(attr(col, "inapplicable"))) return(inapplicable(attr(col, "inapplicable")))
      res <- reference_eda_quantile_bins(
        d, column = col, probs = der$probs, labels = der$labels, into = der$into,
        breaks_from = if (is.null(der$breaks_rows)) NULL else view$rows[[der$breaks_rows]],
        quantile_type = der$quantile_type %||% 7L)
      d <- res$data
      derived <- rbind(derived, data.frame(
        plot = nm, transform = "quantile bins", column = col,
        declared = paste0("probs = ", paste(signif(der$probs, 4), collapse = ", "),
                          "; cut points from ",
                          if (is.null(der$breaks_rows)) "the rows being binned"
                          else paste0("the '", der$breaks_rows, "' view"),
                          "; ", paste(res$parameters$value[1:2], collapse = "; ")),
        computed = paste0("cut points ", paste(signif(res$breaks$value, 6), collapse = ", "),
                          "; strata ", paste(res$levels, collapse = ", "),
                          "; taken from ", res$n_breaks_rows, " rows"),
        stringsAsFactors = FALSE))
      stats$quantile_breaks <- res$breaks
      for (i in seq_len(nrow(res$parameters)))
        addp(res$parameters$parameter[i], res$parameters$value[i])
      addp("quantile probabilities", paste(signif(der$probs, 4), collapse = ", "))
    } else if (identical(der$kind, "normalize")) {
      num <- role(der$numerator); den <- role(der$denominator)
      for (v in list(num, den))
        if (!is.null(attr(v, "inapplicable"))) return(inapplicable(attr(v, "inapplicable")))
      res <- reference_eda_normalize(d, column = num, by = den,
                                     scale = der$scale %||% 1, into = der$into)
      d <- res$data
      for (i in seq_len(nrow(res$parameters)))
        addp(res$parameters$parameter[i], res$parameters$value[i])
      derived <- rbind(derived, data.frame(
        plot = nm, transform = "normalisation", column = num,
        declared = paste0("divided by ", den, ", scaled by ", format(der$scale %||% 1)),
        computed = paste0(res$n_undefined, " row(s) undefined (divisor zero or ",
                          "missing), set to missing rather than dropped"),
        stringsAsFactors = FALSE))
    }
  }

  plan <- .eda_layer_plan(entry)
  layer_rows <- list()
  pages <- list(); page_ids <- list()

  # ---- kind-specific frames and pages -----------------------------------
  if (entry$kind %in% c("profiles", "individual_profiles")) {
    xcol <- role(entry$x); ycol <- role(entry$y)
    for (v in list(xcol, ycol))
      if (!is.null(attr(v, "inapplicable"))) return(inapplicable(attr(v, "inapplicable")))
    .eda_require_cols(d, c(xcol, ycol), paste0("figure '", nm, "'"))
    f <- d
    f$EDA_X <- d[[xcol]]; f$EDA_Y <- d[[ycol]]
    if (!is.numeric(f$EDA_X) || !is.numeric(f$EDA_Y))
      stop("plot-registry entry '", nm, "': x and y must be numeric columns.",
           call. = FALSE)
    ylab <- if (identical(entry$y, "derived")) {
      if (is.null(entry$derive$label))
        stop("plot-registry entry '", nm, "' maps y to a derived column and must ",
             "declare derive$label (a template such as \"{dv} per g of {dose}\").",
             call. = FALSE)
      .eda_label(NULL, cols, labels, template = entry$derive$label)
    } else .eda_label(ycol, cols, labels)
    xlab <- .eda_label(xcol, cols, labels)
    addp("x", paste0(xcol, " (", entry$x_transform, ")"))
    addp("y", paste0(ycol, " (", entry$y_transform, ")"))

    if (identical(entry$kind, "profiles")) {
      gcol <- role(entry$group)
      if (!is.null(attr(gcol, "inapplicable"))) return(inapplicable(attr(gcol, "inapplicable")))
      f$EDA_GROUP <- factor(d[[gcol]])
      colour_col <- NULL; legend <- TRUE; colour_lab <- NULL
      if (!is.null(entry$colour)) {
        cl <- entry$colour
        cc <- if (identical(cl$role, "derived")) entry$derive$into else cols[[cl$role]]
        if (is.null(cc) || !length(cc) || any(!nzchar(cc))) {
          flag <- if (!is.null(flags) && cl$role %in% names(flags)) flags[[cl$role]] else
            paste0("eda_", cl$role)
          if (identical(cl$when_absent, "group")) {
            notes <- c(notes, paste0(
              "colour by role '", cl$role, "' inapplicable: the specification flags ",
              "no column as `", flag, "`; drawn by the group role (", gcol,
              ") instead, legend suppressed"))
            colour_col <- "EDA_GROUP"; legend <- FALSE
            addp("colour", paste0("group (", gcol, "); role '", cl$role, "' unresolved"))
          } else {
            return(inapplicable(paste0(.eda_inapplicable_prefix, "colour role '", cl$role,
                                       "' is unresolved: the specification flags no ",
                                       "column as `", flag, "`")))
          }
        } else {
          f$EDA_COLOUR <- factor(d[[cc]])
          colour_col <- "EDA_COLOUR"
          colour_lab <- if (identical(cl$role, "derived")) {
            if (!is.null(entry$derive$role)) .eda_label(cols[[entry$derive$role]], cols, labels)
            else entry$derive$into
          } else .eda_label(cc, cols, labels)
          addp("colour", paste0(cc, " (", nlevels(f$EDA_COLOUR), " levels)"))
        }
      }
      if (!is.null(entry$smooth)) {
        sm <- entry$smooth
        fin <- is.finite(f$EDA_X) & is.finite(f$EDA_Y)
        if (sum(fin) < 5L)
          stop("plot-registry entry '", nm, "': fewer than 5 finite points for the ",
               "declared loess smooth. ESCALATE.", call. = FALSE)
        # surface = "direct": exact local fits at every evaluation point
        # (the loess.control default "interpolate" approximates through a
        # kd-tree). Declared here, emitted below (review 2, L1). The fit is on
        # the LINEAR scale of x and y as observed and is drawn on whatever
        # axis transform the entry declares.
        fit <- tryCatch(
          suppressWarnings(stats::loess(EDA_Y ~ EDA_X, data = f[fin, , drop = FALSE],
                                        span = sm$span, degree = sm$degree,
                                        family = "gaussian",
                                        control = stats::loess.control(surface = "direct"))),
          error = function(e) stop("plot-registry entry '", nm, "': the declared ",
                                   "loess smooth (span ", sm$span, ", degree ", sm$degree,
                                   ") could not be fitted: ", conditionMessage(e),
                                   ". ESCALATE.", call. = FALSE))
        f$EDA_FIT <- rep(NA_real_, nrow(f))
        okx <- is.finite(f$EDA_X)
        f$EDA_FIT[okx] <- suppressWarnings(
          stats::predict(fit, newdata = data.frame(EDA_X = f$EDA_X[okx])))
        stats$loess <- data.frame(row_id = f[[rid]], x = f$EDA_X, fitted = f$EDA_FIT,
                                  stringsAsFactors = FALSE)
        stats$loess_summary <- data.frame(n_rows_evaluated = nrow(f),
                                          n_fitted = sum(fin),
                                          n_fitted_missing = sum(is.na(f$EDA_FIT)),
                                          stringsAsFactors = FALSE)
        addp("smooth method", "loess (stats::loess, family gaussian), fitted on finite (x, y) pairs of the view, evaluated at every row's x")
        addp("smooth span", format(sm$span))
        addp("smooth degree", format(sm$degree))
        addp("smooth surface", "direct (exact local fit at each evaluation point; the loess.control default \"interpolate\" is not used)")
        addp("smooth fit scale", paste0("linear x and y as observed; drawn on the declared axes (y ",
                                        entry$y_transform, ")"))
        addp("smooth n fitted", format(sum(fin)))
      }
      aes_main <- ggplot2::aes(x = .data[["EDA_X"]], y = .data[["EDA_Y"]],
                               group = .data[["EDA_GROUP"]])
      # The complete theme is added FIRST so the legend suppression below is
      # not reset by it.
      gg <- ggplot2::ggplot(f, aes_main) + theme
      if (!is.null(colour_col)) {
        gg <- gg + ggplot2::geom_line(ggplot2::aes(colour = .data[[colour_col]]),
                                      data = f, alpha = 0.7) +
          ggplot2::geom_point(ggplot2::aes(colour = .data[[colour_col]]), data = f,
                              size = 1)
        if (legend && nlevels(f[[colour_col]]) <= 9L)
          gg <- gg + ggplot2::scale_colour_brewer(palette = "Set1", name = colour_lab)
        else if (legend) gg <- gg + ggplot2::labs(colour = colour_lab)
        if (!legend) gg <- gg + ggplot2::theme(legend.position = "none")
      } else {
        gg <- gg + ggplot2::geom_line(data = f, alpha = 0.7) +
          ggplot2::geom_point(data = f, size = 1)
      }
      if (!is.null(entry$smooth)) {
        fs <- f[order(f$EDA_X), , drop = FALSE]
        gg <- gg + ggplot2::geom_line(ggplot2::aes(x = .data[["EDA_X"]], y = .data[["EDA_FIT"]],
                                                   group = 1),
                                      data = fs, colour = "black", linewidth = 1,
                                      inherit.aes = FALSE)
      }
      gg <- gg + ggplot2::labs(x = xlab, y = ylab)
      if (identical(entry$x_transform, "log10")) gg <- gg + ggplot2::scale_x_log10()
      if (identical(entry$y_transform, "log10")) gg <- gg + ggplot2::scale_y_log10()
      pages[[1]] <- gg
      page_ids[[1]] <- unique(d[[id_col]])
      layer_rows[[1]] <- lapply(seq_len(nrow(plan)), function(i) f)
    } else {
      # individual profiles: facet per subject, dose markers, pagination over
      # the SUBJECTS view so a subject with doses but no observation still has
      # a panel and the marker union equals the doses view.
      fcol <- role(entry$facet); mxcol <- role(entry$markers$x)
      for (v in list(fcol, mxcol))
        if (!is.null(attr(v, "inapplicable"))) return(inapplicable(attr(v, "inapplicable")))
      md <- view$rows[[entry$markers$rows]]
      .eda_require_cols(md, c(mxcol, fcol), paste0("dose markers of figure '", nm, "'"))
      all_ids <- view$rows$subjects[[id_col]]
      per <- as.integer(entry$page$per_page)
      n_pages <- max(1L, ceiling(length(all_ids) / per))
      m <- md; m$EDA_X <- md[[mxcol]]
      stats$markers <- data.frame(row_id = md[[rid]], subject = md[[fcol]],
                                  x = m$EDA_X, stringsAsFactors = FALSE)
      addp("facet", fcol)
      addp("dose markers", paste0("vertical lines at ", mxcol, " of every record in the '",
                                  entry$markers$rows, "' view (", nrow(md), " records)"))
      addp("pagination", paste0(per, " subject(s) per page over the subjects view; ",
                                n_pages, " page(s)"))
      for (pg in seq_len(n_pages)) {
        ids <- all_ids[seq.int((pg - 1L) * per + 1L, min(pg * per, length(all_ids)))]
        fp <- f[f[[fcol]] %in% ids, , drop = FALSE]
        mp <- m[m[[fcol]] %in% ids, , drop = FALSE]
        fp$EDA_FACET <- factor(fp[[fcol]], levels = ids)
        mp$EDA_FACET <- factor(mp[[fcol]], levels = ids)
        gg <- ggplot2::ggplot(fp, ggplot2::aes(x = .data[["EDA_X"]], y = .data[["EDA_Y"]])) +
          ggplot2::geom_line(data = fp) +
          ggplot2::geom_point(data = fp, size = 1) +
          ggplot2::geom_vline(ggplot2::aes(xintercept = .data[["EDA_X"]]), data = mp,
                              linetype = "dashed", colour = "grey40", inherit.aes = FALSE) +
          ggplot2::facet_wrap(~ EDA_FACET, ncol = 4, drop = FALSE) +
          ggplot2::labs(x = xlab, y = ylab,
                        title = paste0(.eda_label(fcol, cols, labels), ": page ", pg,
                                       " of ", n_pages)) + theme
        if (identical(entry$x_transform, "log10")) gg <- gg + ggplot2::scale_x_log10()
        if (identical(entry$y_transform, "log10")) gg <- gg + ggplot2::scale_y_log10()
        pages[[pg]] <- gg
        page_ids[[pg]] <- ids
        layer_rows[[pg]] <- list(fp, fp, mp)
      }
    }
  } else if (entry$kind == "histograms") {
    vars <- role(entry$variables)
    if (!is.null(attr(vars, "inapplicable"))) return(inapplicable(attr(vars, "inapplicable")))
    .eda_require_cols(d, vars, paste0("figure '", nm, "'"))
    n <- as.integer(entry$bins$n)
    parts <- lapply(vars, function(v) {
      x <- d[[v]]
      if (!is.numeric(x)) stop("plot-registry entry '", nm, "': column '", v,
                               "' is not numeric.", call. = FALSE)
      b <- .eda_bins(x, n, paste0("column '", v, "'"))
      list(frame = data.frame(EDA_ROW_ = d[[rid]], EDA_VAR = .eda_label(v, cols, labels),
                              EDA_VALUE = x, EDA_LOWER = b$lower, EDA_UPPER = b$upper,
                              EDA_ZERO = 0, EDA_COUNT = b$count, stringsAsFactors = FALSE),
           table = cbind(data.frame(variable = v, stringsAsFactors = FALSE), b$table))
    })
    f <- do.call(rbind, lapply(parts, `[[`, "frame"))
    names(f)[names(f) == "EDA_ROW_"] <- rid
    stats$bins <- do.call(rbind, lapply(parts, `[[`, "table"))
    rownames(stats$bins) <- NULL
    addp("bins", paste0("rule equal_width, ", n, " bins per covariate over its observed [min, max]; ",
                        "right-closed intervals, lowest including its left end"))
    addp("bar height", "count of records of the view in the bin, computed by the reference and drawn with an identity rectangle")
    gg <- ggplot2::ggplot(f, ggplot2::aes(xmin = .data[["EDA_LOWER"]], xmax = .data[["EDA_UPPER"]],
                                          ymin = .data[["EDA_ZERO"]], ymax = .data[["EDA_COUNT"]])) +
      ggplot2::geom_rect(data = f, fill = "grey70", colour = "grey30") +
      ggplot2::facet_wrap(~ EDA_VAR, scales = "free_x") +
      ggplot2::labs(x = NULL, y = paste0("Records (", entry$rows, " view)")) + theme
    pages[[1]] <- gg; page_ids[[1]] <- unique(d[[id_col]])
    layer_rows[[1]] <- list(f)
  } else if (entry$kind == "pairs") {
    vars <- role(entry$variables)
    if (!is.null(attr(vars, "inapplicable"))) return(inapplicable(attr(vars, "inapplicable")))
    .eda_require_cols(d, vars, paste0("figure '", nm, "'"))
    k <- length(vars)
    if (k < 2L)
      return(inapplicable(paste0(.eda_inapplicable_prefix, "a pairs plot needs at least ",
                                 "two continuous covariates; the specification flags ", k)))
    for (v in vars) if (!is.numeric(d[[v]]))
      stop("plot-registry entry '", nm, "': column '", v, "' is not numeric.", call. = FALSE)
    n <- as.integer(entry$bins$n); cr <- entry$correlation
    cells <- list(); corr <- list()
    panel_levels <- character(0)
    for (i in seq_len(k)) for (j in seq_len(k)) {
      vi <- vars[i]; vj <- vars[j]
      lab <- paste0(.eda_label(vi, cols, labels), " ~ ", .eda_label(vj, cols, labels))
      panel_levels <- c(panel_levels, lab)
      base <- data.frame(EDA_ROW_ = d[[rid]], EDA_PANEL = lab, EDA_TYPE = NA_character_,
                         EDA_X = NA_real_, EDA_Y = NA_real_, EDA_LOWER = NA_real_,
                         EDA_UPPER = NA_real_, EDA_ZERO = 0, EDA_COUNT = NA_real_,
                         EDA_LABEL = NA_character_, EDA_VX = d[[vj]], EDA_VY = d[[vi]],
                         stringsAsFactors = FALSE)
      if (i == j) {
        b <- .eda_bins(d[[vi]], n, paste0("column '", vi, "'"))
        base$EDA_TYPE <- "diag"; base$EDA_LOWER <- b$lower; base$EDA_UPPER <- b$upper
        base$EDA_COUNT <- b$count
      } else if (i > j) {
        base$EDA_TYPE <- "lower"; base$EDA_X <- d[[vj]]; base$EDA_Y <- d[[vi]]
      } else {
        xi <- d[[vi]]; xj <- d[[vj]]
        cc <- is.finite(xi) & is.finite(xj)
        r <- if (sum(cc) >= 3L) stats::cor(xi[cc], xj[cc], use = cr$use, method = cr$method) else NA_real_
        corr[[length(corr) + 1L]] <- data.frame(x = vj, y = vi, method = cr$method,
                                                r = r, n = sum(cc), stringsAsFactors = FALSE)
        base$EDA_TYPE <- "upper"
        base$EDA_X <- (min(xj, na.rm = TRUE) + max(xj, na.rm = TRUE)) / 2
        base$EDA_Y <- (min(xi, na.rm = TRUE) + max(xi, na.rm = TRUE)) / 2
        base$EDA_LABEL <- paste0(cr$method, " r = ", format(round(r, cr$digits), nsmall = cr$digits),
                                 "\nn = ", sum(cc))
      }
      cells[[length(cells) + 1L]] <- base
    }
    f <- do.call(rbind, cells)
    names(f)[names(f) == "EDA_ROW_"] <- rid
    f$EDA_PANEL <- factor(f$EDA_PANEL, levels = panel_levels)
    stats$correlations <- do.call(rbind, corr); rownames(stats$correlations) <- NULL
    lower <- f[f$EDA_TYPE == "lower", , drop = FALSE]
    diag  <- f[f$EDA_TYPE == "diag", , drop = FALSE]
    upper <- f[f$EDA_TYPE == "upper", , drop = FALSE]
    addp("layout", paste0(k, " x ", k, " panels (facets, free scales): scatter below the ",
                          "diagonal, equal-width histogram (", n, " bins) on it, ",
                          cr$method, " correlation above it"))
    addp("correlation", paste0(cr$method, " (stats::cor on the complete finite pairs, use = ",
                               cr$use, "), one row per ", entry$rows,
                               " record; NA below 3 complete pairs or with zero variance, shown as r = NA; shown to ",
                               cr$digits, " decimals"))
    addp("pmplots::pairs_plot / GGally", "NOT used: they compute the displayed correlations and densities internally, outside the approved method")
    gg <- ggplot2::ggplot(f) +
      ggplot2::geom_point(ggplot2::aes(x = .data[["EDA_X"]], y = .data[["EDA_Y"]]), data = lower,
                          size = 1) +
      ggplot2::geom_rect(ggplot2::aes(xmin = .data[["EDA_LOWER"]], xmax = .data[["EDA_UPPER"]],
                                      ymin = .data[["EDA_ZERO"]], ymax = .data[["EDA_COUNT"]]),
                         data = diag, fill = "grey70", colour = "grey30") +
      ggplot2::geom_text(ggplot2::aes(x = .data[["EDA_X"]], y = .data[["EDA_Y"]],
                                      label = .data[["EDA_LABEL"]]), data = upper,
                         check_overlap = TRUE) +
      # A blank layer over the actual (x, y) values of the correlation panels
      # keeps their axis ranges those of the data, so the panel is read against
      # the same scale as the scatter it summarises; it draws nothing.
      ggplot2::geom_blank(ggplot2::aes(x = .data[["EDA_VX"]], y = .data[["EDA_VY"]]),
                          data = upper) +
      ggplot2::facet_wrap(~ EDA_PANEL, ncol = k, scales = "free") +
      ggplot2::labs(x = NULL, y = NULL) + theme
    pages[[1]] <- gg; page_ids[[1]] <- unique(d[[id_col]])
    layer_rows[[1]] <- list(lower, diag, upper, upper)
  } else if (entry$kind == "boxplots") {
    vars <- role(entry$variables); bys <- role(entry$by)
    for (v in list(vars, bys))
      if (!is.null(attr(v, "inapplicable"))) return(inapplicable(attr(v, "inapplicable")))
    .eda_require_cols(d, c(vars, bys), paste0("figure '", nm, "'"))
    bx <- entry$box
    parts <- list(); tabs <- list()
    for (v in vars) for (b in bys) {
      x <- d[[v]]
      if (!is.numeric(x)) stop("plot-registry entry '", nm, "': column '", v,
                               "' is not numeric.", call. = FALSE)
      lv_raw <- as.character(d[[b]])
      dec <- decodes[[b]]
      lv_lab <- if (is.null(dec)) lv_raw else {
        m <- match(lv_raw, as.character(dec$value))
        ifelse(is.na(m), lv_raw, as.character(dec$decode)[m])
      }
      # Levels in the order of the raw values: numeric order when the column
      # is numeric (review 2, L7: "10" must not sort before "2").
      ord <- if (is.numeric(d[[b]])) order(d[[b]]) else order(lv_raw)
      levs <- unique(lv_lab[ord]); levs <- levs[!is.na(levs)]
      fr <- data.frame(EDA_ROW_ = d[[rid]], EDA_VAR = .eda_label(v, cols, labels),
                       EDA_BY = .eda_label(b, cols, labels),
                       EDA_LEVEL = factor(lv_lab, levels = levs), EDA_Y = x,
                       EDA_Q1 = NA_real_, EDA_MED = NA_real_, EDA_Q3 = NA_real_,
                       EDA_WLO = NA_real_, EDA_WHI = NA_real_, EDA_OUT = NA,
                       stringsAsFactors = FALSE)
      for (l in levs) {
        sel <- !is.na(fr$EDA_LEVEL) & fr$EDA_LEVEL == l
        xo <- x[sel & !is.na(x) & is.finite(x)]
        if (!length(xo)) {
          # A level with no finite value is an empty position on the axis; it
          # is recorded, never silently skipped (review 2, L2).
          notes <- c(notes, paste0("boxplot level '", l, "' of ", b, " has no finite ",
                                   v, " value: no box is drawn for it"))
          tabs[[length(tabs) + 1L]] <- data.frame(
            variable = v, by = b, level = l, n = 0L, q1 = NA_real_, median = NA_real_,
            q3 = NA_real_, whisker_low = NA_real_, whisker_high = NA_real_,
            n_outside_whiskers = 0L, stringsAsFactors = FALSE)
          next
        }
        q <- stats::quantile(xo, c(0.25, 0.5, 0.75), type = bx$quantile_type, names = FALSE)
        iqr <- q[3] - q[1]
        wlo <- min(xo[xo >= q[1] - bx$whisker * iqr]); whi <- max(xo[xo <= q[3] + bx$whisker * iqr])
        fr$EDA_Q1[sel] <- q[1]; fr$EDA_MED[sel] <- q[2]; fr$EDA_Q3[sel] <- q[3]
        fr$EDA_WLO[sel] <- wlo; fr$EDA_WHI[sel] <- whi
        fr$EDA_OUT[sel] <- !is.na(x[sel]) & (x[sel] < wlo | x[sel] > whi)
        tabs[[length(tabs) + 1L]] <- data.frame(
          variable = v, by = b, level = l, n = length(xo), q1 = q[1], median = q[2], q3 = q[3],
          whisker_low = wlo, whisker_high = whi, n_outside_whiskers = sum(fr$EDA_OUT[sel], na.rm = TRUE),
          stringsAsFactors = FALSE)
      }
      parts[[length(parts) + 1L]] <- fr
    }
    f <- do.call(rbind, parts)
    names(f)[names(f) == "EDA_ROW_"] <- rid
    stats$box <- do.call(rbind, tabs); rownames(stats$box) <- NULL
    addp("box", paste0("q1/median/q3 = stats::quantile type ", bx$quantile_type,
                       " of the finite values per level; whiskers extend to the most ",
                       "extreme value within ", bx$whisker, " x IQR of the quartiles ",
                       "(Tukey); every record is drawn as a point"))
    gg <- ggplot2::ggplot(f, ggplot2::aes(x = .data[["EDA_LEVEL"]])) +
      ggplot2::geom_linerange(ggplot2::aes(ymin = .data[["EDA_WLO"]], ymax = .data[["EDA_WHI"]]),
                              data = f, colour = "grey40") +
      ggplot2::geom_crossbar(ggplot2::aes(y = .data[["EDA_MED"]], ymin = .data[["EDA_Q1"]],
                                          ymax = .data[["EDA_Q3"]]), data = f,
                             fill = "grey90", width = 0.6) +
      ggplot2::geom_point(ggplot2::aes(y = .data[["EDA_Y"]]), data = f, size = 1,
                          colour = "grey20") +
      ggplot2::facet_grid(EDA_VAR ~ EDA_BY, scales = "free") +
      ggplot2::labs(x = NULL, y = NULL) + theme
    pages[[1]] <- gg; page_ids[[1]] <- unique(d[[id_col]])
    layer_rows[[1]] <- list(f, f, f)
  }

  # ---- the layer table: what each layer of each page was drawn on ---------
  layers <- do.call(rbind, lapply(seq_along(pages), function(pg) {
    do.call(rbind, lapply(seq_len(nrow(plan)), function(i) {
      fr <- layer_rows[[pg]][[i]]
      data.frame(page = pg, layer = i, geom = plan$geom[i], rows = plan$rows[i],
                 n_frame_rows = nrow(fr),
                 digest = reference_eda_rowset_digest(view, fr),
                 stringsAsFactors = FALSE)
    }))
  }))
  rownames(layers) <- NULL
  addp("transforms", paste0("x ", entry$x_transform, ", y ", entry$y_transform,
                            " (applied by the reference as ggplot2 scales)"))
  list(plot = nm, kind = entry$kind, inapplicable = NULL, pages = pages,
       page_ids = page_ids, layers = layers, statistics = stats, derived = derived,
       parameters = do.call(rbind, params), notes = notes,
       transforms = c(x = entry$x_transform, y = entry$y_transform))
}

# Writes the built pages to `path` as a multi-page PDF at the entry's declared
# size and returns the sha256 of the written bytes (review 2, M1, decision B).
# This is the ONE file the reference writes: the file hashed in the gate must
# be the file the reference proved, so the caller never prints pages itself.
# Returns data.frame(path, pages, sha256).
reference_eda_write_figure <- function(built, entry, nm, path) {
  .eda_gate("figure_construction", "EDA figure construction")
  # The sealed policy environment, captured before any argument is forced
  # (integrity batch 16, R15-P1-1 / D16-1; extended to the registry vectors in
  # fix round 1, review blocker B1).
  .P <- .eda_policy
  .eda_check_entry(nm, entry, .P)
  if (!is.list(built) || is.null(built$pages) || !length(built$pages))
    stop("reference_eda_write_figure(): figure '", nm, "' has no pages (an ",
         "inapplicable figure is recorded, not written).", call. = FALSE)
  if (!is.character(path) || length(path) != 1L || !nzchar(path) ||
      !identical(basename(path), entry$file))
    stop("reference_eda_write_figure(): `path` must end in the entry's declared ",
         "file name '", entry$file, "'; got '", paste(format(path), collapse = ""),
         "'.", call. = FALSE)
  for (pg in built$pages)
    if (!inherits(pg, "ggplot"))
      stop("reference_eda_write_figure(): figure '", nm, "' has a page that is ",
           "not a ggplot.", call. = FALSE)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  grDevices::pdf(path, width = entry$w, height = entry$h, onefile = TRUE)
  done <- FALSE
  on.exit(if (!done) grDevices::dev.off(), add = TRUE)
  for (pg in built$pages) print(pg)
  grDevices::dev.off(); done <- TRUE
  data.frame(path = path, pages = length(built$pages),
             sha256 = paste0("sha256:", digest::digest(file = path, algo = "sha256")),
             stringsAsFactors = FALSE)
}

# ===========================================================================
# UNIT: registry_validation
# ===========================================================================

# The canonical set of core TABLE deliverables (review 2, M2). Every one must
# appear in the status a caller reports, computed or inapplicable with a
# reason; the registry drives the figure set, this constant drives the tables.
EDA_CORE_TABLES <- c("data inventory",
                     "data inventory grouping by study",
                     "data inventory grouping by dose group",
                     "continuous covariate summary",
                     "categorical covariate summary")

# THE SEALED POLICY ENVIRONMENT (integrity batch 16, Codex round 15 R15-P1-1;
# SME decision D16-1, 2026-09-06; completes D15-1).
#
# WHAT WENT WRONG. Integrity batch 15 put the policy constants' VALUES into the
# method-identity digest, so rebinding `EDA_CORE_TABLES` before a call made the
# next gated call refuse. It left the implementation reading the top-level
# symbol, and R's lazy arguments make that a same-call hole: an entry point
# gates FIRST and evaluates its arguments LATER, so an argument written as
#     figures = { E$EDA_CORE_TABLES <- "data inventory"; good_figures }
# is still an unforced promise while the gate checks the honest constant, and
# its evaluation narrows the constant before the required-table check reads it.
# Refusal at the NEXT call does not protect the computation already underway.
#
# WHAT THIS IS. The values of every policy constant a gated body enforces,
# copied at LOAD time into an environment parented on base and locked with its
# bindings, and read from here by the implementation instead of from the
# top-level symbol. Nothing a caller does after the file is sourced can change
# what is in here: the environment is locked, and its bindings are locked.
#
# HOW A SUBSTITUTED ENVIRONMENT IS CAUGHT. Rebinding the top-level symbol
# `.eda_policy` to a lookalike (a locked, base-parented environment holding a
# narrowed EDA_CORE_TABLES) would defeat a lock. `.eda_policy` is a top-level
# non-function binding, so it is part of method identity, and the identity
# digest hashes THIS environment's bindings by name and value rather than its
# useless deparse (see `digest_of`). A substituted environment is an identity
# refusal, and the identity check runs inside the gate at the top of every
# gated body, before any argument of that body is forced.
#
# HOW THE READ IS ORDERED. A gated body captures this environment into a local
# variable IMMEDIATELY after its `.eda_gate()` call and before anything can
# force one of its arguments, then reads every policy value through
# `base::get(<name>, envir = <that local>)`. So even a promise that rebinds the
# top-level symbol mid-call cannot change which environment the body reads.
#
# This is not a defence against a hostile caller's reflection in the same
# process -- reaching a sealed environment through `environment(fn)`,
# `parent.env<-` or an active binding stays out of scope by SME decision
# (D15-4, restated as D16-5 on 2026-09-06).
# THE KEYS ARE NOT THE TOP-LEVEL NAMES, deliberately. The manifest audit's
# binding index counts `base::assign("EDA_DEPENDENCY_FILES", ...)` as a second
# binding of that name, and a root path constant bound more than once is
# refused as unresolvable (integrity batch 12, decision 1) -- correctly, since
# the index cannot tell a copy from a rebinding. Naming the copies differently
# keeps each policy constant singly bound at the top level, which is what the
# audit reads and what the identity digest compares.
.eda_policy <- base::local({
  e <- base::new.env(parent = base::baseenv())
  base::assign("core_tables", EDA_CORE_TABLES, envir = e)
  base::assign("dependency_files", EDA_DEPENDENCY_FILES, envir = e)
  base::assign("inapplicable_prefix", EDA_INAPPLICABLE_PREFIX, envir = e)
  base::assign("registry_rows", EDA_REGISTRY_ROWS, envir = e)
  # THE REGISTRY POLICY VECTORS (integrity batch 16 fix round 1, review blocker
  # B1). Fix round 0 sealed the four constants that gated bodies read DIRECTLY
  # and missed the eleven the registry validator reads through the helper
  # `.eda_check_entry`, which is reached from three approved entry points. The
  # census that justified "all four are now sealed" only looked one level deep;
  # the reviewer's fixture forced a `registry`
  # promise that widened EDA_REGISTRY_TIERS after the gate had run, and
  # reference_eda_registry_check() returned a table carrying an undeclared tier.
  # The census is now transitive and every policy
  # vector a gated call can reach is copied here.
  base::assign("registry_fields", EDA_REGISTRY_FIELDS, envir = e)
  base::assign("registry_tiers", EDA_REGISTRY_TIERS, envir = e)
  base::assign("registry_transforms", EDA_REGISTRY_TRANSFORMS, envir = e)
  base::assign("figure_kinds", EDA_FIGURE_KINDS, envir = e)
  base::assign("derive_kinds", EDA_DERIVE_KINDS, envir = e)
  base::assign("marker_rows", EDA_MARKER_ROWS, envir = e)
  base::assign("smooth_methods", EDA_SMOOTH_METHODS, envir = e)
  base::assign("bin_rules", EDA_BIN_RULES, envir = e)
  base::assign("correlation_methods", EDA_CORRELATION_METHODS, envir = e)
  base::assign("correlation_use", EDA_CORRELATION_USE, envir = e)
  base::lockEnvironment(e, bindings = TRUE)
  e
}, envir = base::new.env())

# Validates a plot registry and returns it as a table (plot, tier, kind, file,
# rows, x_transform, y_transform, w, h). Stops on any malformed entry and on
# two entries sharing an output file.
reference_eda_registry_check <- function(registry) {
  .eda_gate("registry_validation", "EDA plot-registry validations")
  # The sealed policy environment, captured before any argument is forced
  # (integrity batch 16, R15-P1-1 / D16-1; extended to the registry vectors in
  # fix round 1, review blocker B1).
  .P <- .eda_policy
  if (!is.list(registry) || !length(registry) || is.null(names(registry)) ||
      any(!nzchar(names(registry))))
    stop("reference_eda_registry_check(): the registry must be a non-empty ",
         "NAMED list, one element per plot.", call. = FALSE)
  rows <- lapply(names(registry), function(nm) {
    p <- .eda_check_entry(nm, registry[[nm]], .P)
    data.frame(plot = nm, tier = p$tier, kind = p$kind, file = p$file, rows = p$rows,
               x_transform = p$x_transform, y_transform = p$y_transform,
               w = p$w, h = p$h, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  dup <- unique(out$file[duplicated(out$file)])
  if (length(dup))
    stop("reference_eda_registry_check(): registry entries share an output ",
         "file name (", paste(dup, collapse = ", "),
         "); one figure would silently overwrite another.", call. = FALSE)
  rownames(out) <- NULL
  out
}

# Which registry entries this analysis renders: core always, catalog only
# when the analysis enables it.
reference_eda_registry_active <- function(registry_table,
                                          enabled_catalog = character(0)) {
  .eda_gate("registry_validation", "EDA plot-registry validations")
  if (!is.character(enabled_catalog))
    stop("reference_eda_registry_active(): `enabled_catalog` must be a ",
         "character vector of registry entry names.", call. = FALSE)
  unknown <- setdiff(enabled_catalog, registry_table$plot)
  if (length(unknown))
    stop("reference_eda_registry_active(): the analysis enables EDA catalog ",
         "plot(s) that are not registry entries: ",
         paste(unknown, collapse = ", "), ".", call. = FALSE)
  not_catalog <- intersect(enabled_catalog,
                           registry_table$plot[registry_table$tier != "catalog"])
  if (length(not_catalog))
    stop("reference_eda_registry_active(): the analysis enables ",
         paste(not_catalog, collapse = ", "),
         ", which is not catalog tier; core entries render unconditionally.",
         call. = FALSE)
  registry_table$plot[registry_table$tier == "core" |
                        registry_table$plot %in% enabled_catalog]
}

# The transformation name of a built ggplot's x or y scale: "identity" when
# no scale was added or the scale is untransformed.
.eda_scale_transform_name <- function(gg, ax) {
  sc <- tryCatch(gg$scales$get_scales(ax), error = function(e) NULL)
  if (is.null(sc)) return("identity")
  tr <- if (!is.null(sc$transformation)) sc$transformation else sc$trans
  if (is.null(tr) || is.null(tr$name)) "identity" else tr$name
}

# THE ROW-SET ASSERTION on ONE built ggplot (pre-approval review finding H1):
# the digest of the data the plot draws (plot-level data and every layer that
# carries its own), all of which must be the same row set; and the refusal of
# any axis transform other than the DECLARED one (none, when `declared` is
# NULL).
reference_eda_plot_rowset_digest <- function(view, gg, declared = NULL) {
  .eda_gate("registry_validation", "EDA plot-registry validations")
  if (!inherits(gg, "ggplot"))
    stop("reference_eda_plot_rowset_digest(): `gg` must be a built figure (a ",
         "ggplot), not the frame handed to a plotting function. Got: ",
         paste(class(gg), collapse = "/"), ".", call. = FALSE)
  frames <- c(list(gg$data), lapply(gg$layers, function(l) l$data))
  frames <- Filter(is.data.frame, frames)
  if (!length(frames))
    stop("reference_eda_plot_rowset_digest(): the built plot carries no data ",
         "frame, so the rows it draws cannot be identified.", call. = FALSE)
  digs <- unique(vapply(frames, function(f)
    reference_eda_rowset_digest(view, f), character(1)))
  if (length(digs) != 1L)
    stop("reference_eda_plot_rowset_digest(): the plot-level data and the ",
         "layer data are different row sets (", length(digs),
         " distinct digests). One figure describes one row set.", call. = FALSE)
  want <- list(x = declared$x_transform %||% "identity",
               y = declared$y_transform %||% "identity")
  for (ax in c("x", "y")) {
    nm <- .eda_scale_transform_name(gg, ax)
    exp_nm <- if (identical(want[[ax]], "log10")) "log-10" else "identity"
    if (!identical(nm, exp_nm))
      stop("reference_eda_plot_rowset_digest(): the plotting function applied ",
           "a '", nm, "' transformation to the ", ax, " axis, but the registry ",
           "declares ", want[[ax]], ". Axis transforms are DECLARED in the plot ",
           "registry and applied by the reference builder.", call. = FALSE)
  }
  coord <- class(gg$coordinates)[1]
  if (!is.null(coord) && grepl("^CoordTrans", coord))
    stop("reference_eda_plot_rowset_digest(): the plotting function applied a ",
         "transforming coordinate system (", coord, "). Axis transforms are ",
         "declared in the plot registry and applied by the figure builder.",
         call. = FALSE)
  digs
}

# Page geometry of a PDF written by grDevices::pdf(): every /MediaBox and the
# page count, read from the file's bytes.
.eda_pdf_geometry <- function(path) {
  if (!file.exists(path))
    stop("eda reference: rendered figure file ", path, " does not exist.", call. = FALSE)
  b <- readBin(path, "raw", file.info(path)$size)
  s <- rawToChar(b[b != as.raw(0)])
  Encoding(s) <- "bytes"
  s <- iconv(s, "latin1", "ASCII", sub = "?")
  boxes <- regmatches(s, gregexpr("/MediaBox \\[[^]]*\\]", s))[[1]]
  pages <- regmatches(s, gregexpr("/Type\\s*/Page(?![s])", s, perl = TRUE))[[1]]
  if (!length(boxes) || !length(pages))
    stop("eda reference: ", path, " is not a PDF with a readable page geometry.",
         call. = FALSE)
  dims <- t(vapply(boxes, function(bx) {
    v <- as.numeric(strsplit(gsub("/MediaBox \\[|\\]", "", bx), "\\s+")[[1]])
    c(width = v[3] - v[1], height = v[4] - v[2])
  }, numeric(2)))
  list(pages = length(pages), width_pt = unique(dims[, "width"]),
       height_pt = unique(dims[, "height"]))
}

# THE FIGURE CHECK, from the BUILT object and the WRITTEN file (round 7,
# findings 6 and 9). Verifies, for one figure:
#   * the pages are plain ggplots with exactly the layer plan of the kind
#     (geom classes in order);
#   * every layer's data carries the row id; per layer, the union of the row
#     ids over all pages digests to the view the layer declares, every page is
#     a subset of it, and pages are pairwise disjoint (a partition);
#   * each page's x and y scales carry exactly the declared transform and the
#     coordinate system is not a transforming one;
#   * ggplot_build() of each page carries exactly the frame's rows per layer
#     (identity statistics) and exactly as many non-finite positional values
#     as the reference predicts from the frame under the declared transform —
#     the same classification the log-axis audit uses;
#   * the PDF at `path` has the declared page size on every page and one page
#     per built page, and -- when `sha256` (from reference_eda_write_figure) is
#     given -- its bytes are exactly the bytes the reference wrote (review 2, M1).
# Returns list(plot, row_digest, layers, reconciliation, file, transforms);
# `file` carries path, pages, width_in, height_in, sha256.
reference_eda_figure_check <- function(view, built, entry, nm, path, sha256 = NULL) {
  .eda_gate("registry_validation", "EDA plot-registry validations")
  # The sealed policy environment, captured before any argument is forced
  # (integrity batch 16, R15-P1-1 / D16-1; extended to the registry vectors in
  # fix round 1, review blocker B1).
  .P <- .eda_policy
  .eda_check_view(view, "reference_eda_figure_check")
  .eda_check_entry(nm, entry, .P)
  bad <- function(...) stop("figure '", nm, "': ", ..., call. = FALSE)
  if (!is.list(built) || is.null(built$pages) || !length(built$pages))
    bad("the build carries no pages (an inapplicable figure is not checked; it is recorded).")
  plan <- .eda_layer_plan(entry)
  rid <- view$row_id_col
  id_col <- view$selector$id_col
  view_ids <- lapply(view$rows, function(v) sort(unique(v[[rid]])))
  union_ids <- lapply(seq_len(nrow(plan)), function(i) integer(0))
  main_page_ids <- list()
  rec <- list()
  for (pg in seq_along(built$pages)) {
    gg <- built$pages[[pg]]
    if (!inherits(gg, "ggplot")) bad("page ", pg, " is not a ggplot.")
    if (length(gg$layers) != nrow(plan))
      bad("page ", pg, " has ", length(gg$layers), " layer(s); the kind '",
          entry$kind, "' plan has ", nrow(plan), ".")
    for (ax in c("x", "y")) {
      nm_tr <- .eda_scale_transform_name(gg, ax)
      exp_nm <- if (identical(entry[[paste0(ax, "_transform")]], "log10")) "log-10" else "identity"
      if (!identical(nm_tr, exp_nm))
        bad("page ", pg, " carries a '", nm_tr, "' ", ax, " scale; the registry declares ",
            entry[[paste0(ax, "_transform")]], ".")
    }
    if (grepl("^CoordTrans", class(gg$coordinates)[1]))
      bad("page ", pg, " uses a transforming coordinate system.")
    b <- suppressWarnings(ggplot2::ggplot_build(gg))
    for (i in seq_len(nrow(plan))) {
      ly <- gg$layers[[i]]
      gcls <- class(ly$geom)[1]
      if (!identical(gcls, plan$geom[i]))
        bad("page ", pg, " layer ", i, " is ", gcls, "; the plan says ", plan$geom[i], ".")
      fr <- if (is.data.frame(ly$data)) ly$data else gg$data
      if (!is.data.frame(fr)) bad("page ", pg, " layer ", i, " carries no data frame.")
      .eda_require_cols(fr, rid, paste0("figure '", nm, "' page ", pg, " layer ", i))
      ids <- sort(unique(fr[[rid]]))
      want <- view_ids[[plan$rows[i]]]
      if (is.null(want)) bad("layer ", i, " declares the unknown view '", plan$rows[i], "'.")
      if (length(setdiff(ids, want)))
        bad("page ", pg, " layer ", i, " draws ", length(setdiff(ids, want)),
            " row(s) outside the '", plan$rows[i], "' view: the layer's row set is not ",
            "the declared one.")
      if (length(intersect(ids, union_ids[[i]])))
        bad("page ", pg, " layer ", i, " repeats ", length(intersect(ids, union_ids[[i]])),
            " row(s) already drawn on an earlier page; pages must partition the view.")
      union_ids[[i]] <- c(union_ids[[i]], ids)
      # ggplot_build reconciliation.
      bd <- b$data[[i]]
      n_built <- nrow(bd)
      pos <- intersect(names(bd), c("x", "y", "xmin", "xmax", "ymin", "ymax", "xend",
                                    "yend", "xintercept", "yintercept"))
      nonfin_built <- if (length(pos)) sum(!Reduce(`&`, lapply(pos, function(p) {
        v <- bd[[p]]
        if (is.numeric(v)) is.finite(v) else !is.na(v)
      }))) else 0L
      xcols <- strsplit(plan$xcols[i], ",")[[1]]; xcols <- xcols[nzchar(xcols)]
      ycols <- strsplit(plan$ycols[i], ",")[[1]]; ycols <- ycols[nzchar(ycols)]
      .eda_require_cols(fr, c(xcols, ycols), paste0("figure '", nm, "' layer ", i))
      nf <- rep(FALSE, nrow(fr))
      for (cc in xcols) nf <- nf | .eda_nonfinite_after(fr[[cc]], entry$x_transform)
      for (cc in ycols) nf <- nf | .eda_nonfinite_after(fr[[cc]], entry$y_transform)
      n_expected <- sum(nf)
      rec[[length(rec) + 1L]] <- data.frame(
        page = pg, layer = i, geom = gcls, rows = plan$rows[i],
        n_frame_rows = nrow(fr), n_built = n_built,
        n_nonfinite_built = as.integer(nonfin_built),
        n_expected_nonfinite = as.integer(n_expected),
        ok = (n_built == nrow(fr)) && (nonfin_built == n_expected),
        stringsAsFactors = FALSE)
    }
    main_page_ids[[pg]] <- unique(gg$data[[id_col]] %||% integer(0))
  }
  reconciliation <- do.call(rbind, rec); rownames(reconciliation) <- NULL
  if (!all(reconciliation$ok)) {
    badrows <- reconciliation[!reconciliation$ok, , drop = FALSE]
    bad("ggplot_build() does not reconcile with the frames the reference drew: ",
        paste(sprintf("page %d layer %d (%s): frame %d rows, built %d, non-finite built %d, predicted %d",
                      badrows$page, badrows$layer, badrows$geom, badrows$n_frame_rows,
                      badrows$n_built, badrows$n_nonfinite_built,
                      badrows$n_expected_nonfinite), collapse = "; "),
        ". The audit does not describe what the figure draws.")
  }
  layers <- do.call(rbind, lapply(seq_len(nrow(plan)), function(i) {
    dg <- .eda_digest(c(view$selector_digest, sort(unique(union_ids[[i]]))))
    ok <- identical(dg, unname(view$digests[[plan$rows[i]]]))
    data.frame(layer = i, geom = plan$geom[i], rows = plan$rows[i],
               n_pages = length(built$pages), n_rows = length(unique(union_ids[[i]])),
               digest = dg, expected_digest = unname(view$digests[[plan$rows[i]]]),
               ok = ok, stringsAsFactors = FALSE)
  }))
  rownames(layers) <- NULL
  if (!all(layers$ok))
    bad("layer(s) ", paste(layers$layer[!layers$ok], collapse = ", "),
        " do not draw the row set of the view they declare (union over pages ",
        "differs from the view digest). A figure must be drawn on exactly the ",
        "rows its registry entry declares; equal row COUNTS are not evidence of ",
        "equal row sets.")
  geom <- .eda_pdf_geometry(path)
  disk_sha <- paste0("sha256:", digest::digest(file = path, algo = "sha256"))
  if (!is.null(sha256) && !identical(disk_sha, sha256))
    bad("the PDF ", path, " on disk (", disk_sha, ") is not the file the reference ",
        "wrote (", sha256, "). The figure hashed in the gate must be the figure ",
        "that was proved.")
  if (geom$pages != length(built$pages))
    bad("the PDF ", path, " has ", geom$pages, " page(s); the build has ",
        length(built$pages), ". Page count differs from the declaration.")
  if (length(geom$width_pt) != 1L || length(geom$height_pt) != 1L ||
      abs(geom$width_pt - entry$w * 72) > 0.5 || abs(geom$height_pt - entry$h * 72) > 0.5)
    bad("the PDF ", path, " page size is ", paste(geom$width_pt / 72, collapse = "/"),
        " x ", paste(geom$height_pt / 72, collapse = "/"), " in; the registry declares ",
        entry$w, " x ", entry$h, ".")
  list(plot = nm,
       row_digest = unname(view$digests[[entry$rows]]),
       layers = layers,
       reconciliation = reconciliation,
       file = data.frame(path = path, pages = as.integer(geom$pages),
                         width_in = geom$width_pt / 72, height_in = geom$height_pt / 72,
                         sha256 = disk_sha, stringsAsFactors = FALSE),
       transforms = c(x = entry$x_transform, y = entry$y_transform))
}

# The registry index that reaches the gate package (round 7, finding 6).
#
#   rendered      data.frame(plot, file, path, rows, row_digest, w, h, pages,
#                 sha256): one row per figure written, as read back by
#                 reference_eda_figure_check(); `sha256` is the digest of the
#                 bytes the reference wrote and is compared to the file on
#                 disk HERE, at packaging (review 2, M1).
#   inapplicable  data.frame(plot, reason): active entries that could not be
#                 produced, each with a non-empty reason.
# Every ACTIVE entry must appear EXACTLY ONCE in `rendered` or in
# `inapplicable`; a disabled entry may appear in neither. Rendered file,
# dimensions, row selector and row-set digest must equal the declaration.
reference_eda_registry_index <- function(registry_table, enabled_catalog,
                                         rendered, view_digests,
                                         inapplicable = NULL) {
  .eda_gate("registry_validation", "EDA plot-registry validations")
  # The sealed policy environment, captured before any argument is forced
  # (integrity batch 16, R15-P1-1 / D16-1).
  .P <- .eda_policy
  need <- c("plot", "file", "path", "rows", "row_digest", "w", "h", "pages", "sha256")
  if (!is.data.frame(rendered) || !all(need %in% names(rendered)))
    stop("reference_eda_registry_index(): `rendered` must be a data frame ",
         "with columns ", paste(need, collapse = ", "), ".", call. = FALSE)
  if (is.null(names(view_digests)) ||
      # `base::is.element`, not the bare `%in%` operator: an operator is an
      # ordinary binding a caller can shadow in the loading environment, and
      # this one decides whether the required registry rows are present
      # (integrity batch 16, R15-P1-2 / D16-2).
      !base::all(base::is.element(base::get("registry_rows", envir = .P),
                                  base::names(view_digests))))
    stop("reference_eda_registry_index(): `view_digests` must be the named ",
         "digest vector reference_eda_view() returned.", call. = FALSE)
  if (is.null(inapplicable))
    inapplicable <- data.frame(plot = character(0), reason = character(0),
                               stringsAsFactors = FALSE)
  if (!is.data.frame(inapplicable) || !all(c("plot", "reason") %in% names(inapplicable)))
    stop("reference_eda_registry_index(): `inapplicable` must be a data frame ",
         "with columns plot, reason.", call. = FALSE)

  stray <- setdiff(c(rendered$plot, inapplicable$plot), registry_table$plot)
  if (length(stray))
    stop("reference_eda_registry_index(): rendered figure(s) with no plot-",
         "registry entry: ", paste(stray, collapse = ", "),
         ". Every figure is a registry entry (skills/eda-standards); an ",
         "unregistered figure would reach the gate unlisted.", call. = FALSE)
  dup <- unique(rendered$plot[duplicated(rendered$plot)])
  if (length(dup))
    stop("reference_eda_registry_index(): figure(s) rendered more than once: ",
         paste(dup, collapse = ", "), ". Every active entry renders exactly once.",
         call. = FALSE)
  active <- reference_eda_registry_active(registry_table, enabled_catalog)
  disabled_render <- setdiff(c(rendered$plot, inapplicable$plot), active)
  if (length(disabled_render))
    stop("reference_eda_registry_index(): figure(s) rendered or recorded that ",
         "are not enabled for this analysis: ", paste(disabled_render, collapse = ", "),
         ". A catalog entry renders only when the analysis enables it.", call. = FALSE)
  both <- intersect(rendered$plot, inapplicable$plot)
  if (length(both))
    stop("reference_eda_registry_index(): figure(s) both rendered and declared ",
         "inapplicable: ", paste(both, collapse = ", "), ".", call. = FALSE)
  missing_active <- setdiff(active, c(rendered$plot, inapplicable$plot))
  if (length(missing_active))
    stop("reference_eda_registry_index(): active entry(ies) neither rendered nor ",
         "recorded as inapplicable with a reason: ",
         paste(missing_active, collapse = ", "),
         ". A core deliverable that cannot be produced is recorded as ",
         "inapplicable with its reason, never silently skipped.", call. = FALSE)
  no_reason <- inapplicable$plot[is.na(inapplicable$reason) |
                                   !nzchar(trimws(inapplicable$reason))]
  if (length(no_reason))
    stop("reference_eda_registry_index(): inapplicable entry(ies) without a ",
         "reason: ", paste(no_reason, collapse = ", "), ".", call. = FALSE)

  idx <- registry_table
  idx$enabled <- idx$plot %in% active
  m <- match(idx$plot, rendered$plot)
  mi <- match(idx$plot, inapplicable$plot)
  idx$status <- ifelse(!is.na(m), "rendered",
                       ifelse(!is.na(mi), "inapplicable", "disabled"))
  idx$reason <- ifelse(is.na(mi), "", inapplicable$reason[mi])
  idx$rendered <- !is.na(m)
  idx$path <- ifelse(is.na(m), NA_character_, rendered$path[m])
  idx$pages <- ifelse(is.na(m), NA_integer_, as.integer(rendered$pages[m]))
  idx$rendered_file <- ifelse(is.na(m), NA_character_, rendered$file[m])
  idx$rendered_w <- ifelse(is.na(m), NA_real_, rendered$w[m])
  idx$rendered_h <- ifelse(is.na(m), NA_real_, rendered$h[m])
  idx$rendered_rows <- ifelse(is.na(m), NA_character_, rendered$rows[m])
  idx$row_digest <- ifelse(is.na(m), NA_character_, rendered$row_digest[m])
  idx$expected_digest <- unname(view_digests[idx$rows])
  idx$sha256 <- ifelse(is.na(m), NA_character_, rendered$sha256[m])

  drawn <- which(idx$rendered)
  disk <- vapply(idx$path[drawn], function(p)
    if (file.exists(p)) paste0("sha256:", digest::digest(file = p, algo = "sha256"))
    else "<missing>", character(1))
  wrong_bytes <- drawn[disk != idx$sha256[drawn]]
  if (length(wrong_bytes))
    stop("reference_eda_registry_index(): figure file(s) on disk are not the ",
         "bytes the reference wrote: ",
         paste(sprintf("%s (%s: written %s, on disk %s)", idx$plot[wrong_bytes],
                       idx$path[wrong_bytes], idx$sha256[wrong_bytes],
                       disk[match(wrong_bytes, drawn)]), collapse = "; "),
         ". The figure hashed in the gate must be the figure that was proved.",
         call. = FALSE)
  wrong_file <- drawn[idx$rendered_file[drawn] != idx$file[drawn]]
  if (length(wrong_file))
    stop("reference_eda_registry_index(): figure(s) written to a file name ",
         "other than the declared one: ",
         paste(sprintf("%s (declared %s, rendered %s)", idx$plot[wrong_file],
                       idx$file[wrong_file], idx$rendered_file[wrong_file]),
               collapse = "; "), ".", call. = FALSE)
  wrong_dim <- drawn[abs(idx$rendered_w[drawn] - idx$w[drawn]) > 1e-6 |
                       abs(idx$rendered_h[drawn] - idx$h[drawn]) > 1e-6]
  if (length(wrong_dim))
    stop("reference_eda_registry_index(): figure(s) whose rendered dimensions ",
         "differ from the declaration: ",
         paste(sprintf("%s (declared %g x %g, rendered %g x %g)", idx$plot[wrong_dim],
                       idx$w[wrong_dim], idx$h[wrong_dim], idx$rendered_w[wrong_dim],
                       idx$rendered_h[wrong_dim]), collapse = "; "), ".", call. = FALSE)
  wrong_view <- drawn[idx$rendered_rows[drawn] != idx$rows[drawn]]
  if (length(wrong_view))
    stop("reference_eda_registry_index(): figure(s) drawn on a different row ",
         "selector than declared: ",
         paste(sprintf("%s (declared %s, drawn %s)", idx$plot[wrong_view],
                       idx$rows[wrong_view], idx$rendered_rows[wrong_view]),
               collapse = "; "), ".", call. = FALSE)
  wrong_rows <- drawn[idx$row_digest[drawn] != idx$expected_digest[drawn]]
  if (length(wrong_rows))
    stop("reference_eda_registry_index(): figure(s) whose row set does not ",
         "match the EDA view they declared: ",
         paste(sprintf("%s (digest %s, view '%s' digest %s)",
                       idx$plot[wrong_rows], idx$row_digest[wrong_rows],
                       idx$rows[wrong_rows], idx$expected_digest[wrong_rows]),
               collapse = "; "),
         ". A figure must be drawn on exactly the rows its registry entry ",
         "declares; equal row COUNTS are not evidence of equal row sets.",
         call. = FALSE)

  idx <- idx[, c("plot", "tier", "kind", "enabled", "status", "reason", "rendered",
                 "rows", "x_transform", "y_transform", "file", "path", "w", "h",
                 "pages", "sha256", "row_digest", "expected_digest"), drop = FALSE]
  rownames(idx) <- NULL
  idx
}

# The core-deliverable status table (round 7, finding 7). Every core figure
# (from the registry index) and every core table (from `tables`) appears with
# status "rendered" / "computed" or "inapplicable" + a non-empty reason; a
# core item in any other state stops. Catalog figures are listed with
# "rendered" / "disabled". Returns list(table, counts).
reference_eda_core_status <- function(figures, tables) {
  .eda_gate("registry_validation", "EDA plot-registry validations")
  # Captured HERE, immediately after the gate and before `figures` or `tables`
  # is forced (integrity batch 16, R15-P1-1). The gate's identity check has just
  # confirmed that `.eda_policy` in the sourcing frame is the environment this
  # file built at load; a promise forced below cannot change which environment
  # this local names.
  .P <- .eda_policy
  if (!is.data.frame(figures) || !all(c("plot", "tier", "status", "reason") %in% names(figures)))
    stop("reference_eda_core_status(): `figures` must be the registry index.",
         call. = FALSE)
  if (!is.data.frame(tables) || !all(c("deliverable", "status", "reason") %in% names(tables)))
    stop("reference_eda_core_status(): `tables` must be a data frame with ",
         "columns deliverable, status, reason.", call. = FALSE)
  fig <- data.frame(deliverable = paste0("figure: ", figures$plot), tier = figures$tier,
                    status = figures$status, reason = figures$reason,
                    stringsAsFactors = FALSE)
  tab <- data.frame(deliverable = paste0("table: ", tables$deliverable), tier = "core",
                    status = tables$status,
                    reason = ifelse(is.na(tables$reason), "", tables$reason),
                    stringsAsFactors = FALSE)
  out <- rbind(tab, fig)
  rownames(out) <- NULL
  # `base::setdiff`, not `setdiff` (integrity batch 16, Codex round 15 R15-P1-2;
  # SME decision D16-2). Method identity compares the reference's own
  # definitions; it does not compare every other binding in the environment the
  # file was loaded into. An honest load followed by
  #     E$setdiff <- function(...) character(0)
  # left every inventoried function and every policy value unchanged while both
  # the missing-table and the unknown-table checks received an empty result, so
  # the refusals silently stopped refusing. A policy-enforcing call is resolved
  # in the base namespace, where a caller cannot rebind it.
  .core <- base::get("core_tables", envir = .P)
  missing_t <- base::setdiff(.core, tables$deliverable)
  if (length(missing_t))
    stop("reference_eda_core_status(): core table(s) neither computed nor ",
         "recorded inapplicable: ", paste(missing_t, collapse = ", "),
         ". The core table set is EDA_CORE_TABLES in the reference; a caller ",
         "reports every one of them.", call. = FALSE)
  unknown_t <- base::setdiff(tables$deliverable, .core)
  if (length(unknown_t))
    stop("reference_eda_core_status(): table deliverable(s) not in the core set: ",
         paste(unknown_t, collapse = ", "), ".", call. = FALSE)
  allowed <- c("computed", "rendered", "inapplicable", "disabled")
  badst <- out$deliverable[!out$status %in% allowed]
  if (length(badst))
    stop("reference_eda_core_status(): deliverable(s) with an unknown status: ",
         paste(badst, collapse = ", "), ".", call. = FALSE)
  core_disabled <- out$deliverable[out$tier == "core" & out$status == "disabled"]
  if (length(core_disabled))
    stop("reference_eda_core_status(): core deliverable(s) in a disabled state: ",
         paste(core_disabled, collapse = ", "), ". Core deliverables are ",
         "produced or recorded as inapplicable with a reason.", call. = FALSE)
  no_reason <- out$deliverable[out$status == "inapplicable" & !nzchar(trimws(out$reason))]
  if (length(no_reason))
    stop("reference_eda_core_status(): inapplicable deliverable(s) without a ",
         "reason: ", paste(no_reason, collapse = ", "), ". An inapplicable ",
         "state carries the reason the specification gave.", call. = FALSE)
  counts <- data.frame(
    id = c("n_core", "n_core_produced", "n_core_inapplicable", "n_catalog_enabled",
           "n_figures_rendered"),
    quantity = c("core deliverables", "core deliverables produced (computed or rendered)",
                 "core deliverables inapplicable (with reason)",
                 "catalog figures enabled", "figures rendered"),
    value = c(sum(out$tier == "core"),
              sum(out$tier == "core" & out$status %in% c("computed", "rendered")),
              sum(out$tier == "core" & out$status == "inapplicable"),
              sum(figures$tier == "catalog" & figures$status != "disabled"),
              sum(figures$status == "rendered")),
    stringsAsFactors = FALSE)
  list(table = out, counts = counts)
}
