# REFERENCE IMPLEMENTATION — NPDE panels from an engine's NPDE table
#
# AUTHORITY: skills/diagnostics/MANIFEST.yaml is the authority for the SME signature
# (root reviewed_by / reviewed_at) and for the status and digest of unit `npde_panels`.
# For an engine-produced NPDE table, draw the same five panels as the
# nlmixr2 path. Record the supplied engine, simulation count and seed.
# unit (reference_npde_minimum_nsim, SME decision D1 of 2026-08-28).
#
# Input contract
#   df      data.frame with NPDE, PRED, TIME and TAD (or lowercase tad)
#   nsim    simulations behind the NPDE; >= reference_npde_minimum_nsim()
#   seed    the engine run's seed (mandatory, recorded)
#   engine  the engine that computed NPDE (mandatory, recorded)
#
# Returns list(npde_pred, npde_time, npde_tad, npde_hist, npde_q,
# record = list(engine, nsim, seed)).


# Self-gate on the intended-use unit "npde_panels": the private, locked
# validator environment of npde_tests.R (integrity batches 11 to 13; the
# construction is explained there).
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

# The dependency environment: npde.R, loaded once into its own locked,
# base-parented environment (the psn_covsearch.R construction, integrity batch
# 12). The house minimum comes from the approved npde unit, through its own
# gate, rather than a second copy of the number.
.mm_trusted_deps <- base::local({
  d <- base::new.env(parent = base::baseenv())
  for (f in base::c("skills/diagnostics/reference/npde.R")) {
    if (!base::file.exists(f))
      base::stop("diagnostics npde_panels reference: required file ", f,
                 " is missing (the reference is sourced with the repository root ",
                 "as the working directory).", call. = FALSE)
    base::sys.source(f, envir = d, keep.source = FALSE)
  }
  base::lockEnvironment(d, bindings = TRUE)
  d
}, envir = base::new.env(parent = base::baseenv()))

reference_npde_minimum_nsim <- base::local({
  sealed <- .mm_trusted_deps
  function(...) base::get("reference_npde_minimum_nsim", envir = sealed)(...)
}, envir = base::new.env())

.diag_npde_panels_gate <- base::local({
  private <- .mm_trusted_private
  function() {
    if (!base::get("require_validated_method", envir = private)(skill_dir = "skills/diagnostics",
                                          calling_file = "skills/diagnostics/reference/npde_panels.R",
                                          unit = "npde_panels")) {
      base::stop("diagnostics unit 'npde_panels' is not SME-validated (manifest unit ",
           "draft/rejected, or the manifest has an empty reviewed_by); the ",
           "NPDE panels do not compute until the unit is approved. ",
           "ESCALATE to the SME.", call. = FALSE)
    }
    base::invisible(TRUE)
  }
}, envir = base::new.env())

reference_npde_panels <- function(df, nsim, seed, engine) {
  .diag_npde_panels_gate()
  if (missing(seed) || is.null(seed) || length(seed) != 1L)
    stop("seed is mandatory: the engine run's seed is recorded with the panels.",
         call. = FALSE)
  if (missing(engine) || !is.character(engine) || length(engine) != 1L || !nzchar(engine))
    stop("engine is mandatory: name the engine that computed the NPDE.", call. = FALSE)
  minimum_nsim <- reference_npde_minimum_nsim()
  if (missing(nsim) || !is.numeric(nsim) || length(nsim) != 1L || !is.finite(nsim) ||
      nsim < minimum_nsim)
    stop("nsim below the house minimum of ", minimum_nsim,
         " (SME decision 2026-08-28); raise nsim or record a deviation.",
         call. = FALSE)
  if (!is.data.frame(df))
    stop("df must be a data.frame.", call. = FALSE)
  # Moved over unchanged (npde.R lines 141-148).
  # nlmixr2 emits time-after-dose as lowercase `tad`; pmplots expects TAD.
  if (!"TAD" %in% names(df) && "tad" %in% names(df)) df$TAD <- df$tad
  miss <- setdiff(c("NPDE", "PRED", "TIME", "TAD"), names(df))
  if (length(miss))
    stop("df is missing column(s): ", paste(miss, collapse = ", "), ".", call. = FALSE)
  list(
    npde_pred = pmplots::npde_pred(df),
    npde_time = pmplots::npde_time(df),
    npde_tad  = pmplots::npde_tad(df),
    npde_hist = pmplots::npde_hist(df),
    npde_q    = pmplots::npde_q(df),
    record    = list(engine = engine, nsim = nsim, seed = seed)
  )
}
