
# Reference code and local method review

Files under `skills/**/reference/**` are the deterministic method
implementations the harness computes with. Public manifests start unsigned.
Local review binds the method and its dependencies by digest.

- **Check manifest status before editing.** Each skill's `MANIFEST.yaml` records
  a sha256 per reference file and, per intended-use unit, a `status`
  (`approved | draft | rejected`) plus the SME `reviewed_by` sign-off. Read it
  (`tools/method_manifest.R` parses it) before touching any reference file.
- **Editing approved code stops it from running.** Any change alters the
  file's sha256, which no longer matches the signed digest in `MANIFEST.yaml`.
  That mismatch is a hard stop at the self-gate: the method refuses to compute,
  and the unit's recorded status is still `approved` until someone regenerates
  the manifest. The draft reset is performed by regeneration —
  `write_method_manifest()` sets any unit whose reference hash drifted back to
  `draft` and clears the root `reviewed_by` — not by the edit itself. Either
  way the method is unavailable until the SME re-signs. Do not edit approved
  reference code without an explicit SME decision to re-open and re-sign it.
- **Regenerating a manifest never blesses anything.** `write_method_manifest()`
  resets any unit whose reference hash drifted back to `draft` and clears the
  root `reviewed_by`, so the whole skill falls to draft until the SME re-signs
  the root and the units by hand from the terminal. Never "fix" a manifest by
  regenerating it.
- **Every reference file seals its validator; every entry point gates through
  it.** The gate no longer calls `require_validated_method()` by name off the
  search path. Each reference file builds ONE private, locked environment
  holding the validator, using exactly this block (quoted from
  `skills/diagnostics/reference/gof.R`; it is byte-identical in every sealed
  reference by design):

  ```r
  .mm_trusted_private <- base::local({
    e <- base::new.env(parent = base::baseenv())
    if (!base::file.exists("tools/method_manifest.R"))
      base::stop("this reference requires tools/method_manifest.R; references are ",
                 "sourced with the repository root as the working directory.",
                 call. = FALSE)
    base::sys.source("tools/method_manifest.R", envir = e, keep.source = FALSE)
    base::lockEnvironment(e, bindings = TRUE)
    e
  }, envir = base::new.env())
  ```

  Every construction primitive — `local`, `new.env`, `baseenv`, `file.exists`,
  `stop`, `sys.source`, `lockEnvironment` — is written with the exact `base::`
  qualification, and the block's own frame is supplied explicitly as
  `envir = base::new.env()`, never left to `local()`'s default (which R
  evaluates in the sourcing frame). The gate is a closure over that
  environment and dispatches `private$require_validated_method(...)` as its
  first statement, e.g. `.gof_gate <- local({ private <- .mm_trusted_private;
  function() { if (!private$require_validated_method(skill_dir = ..., calling_file
  = ..., unit = "gof")) stop(...) } })`. Nothing a caller binds before or after
  sourcing changes what the gate calls. `Rscript tools/ci_check.R` audits the
  block (a positive whitelist: exactly these heads, in exactly these
  positions) and the gate chain, and names any file or entry point that
  deviates.
- **Multi-load references keep two locked environments.** A reference that
  also loads helper files (`eda.R` loads `tools/record_selection.R`;
  `psn_covsearch.R` loads `psn_uncertainty.R` and `scm_config.R`) keeps the validator in one
  locked environment and its dependencies in a SECOND, separate locked
  environment; the validator environment loads only
  `tools/method_manifest.R`, and every dependency is declared under the unit's
  `depends:` in `MANIFEST.yaml` with its digest. The gate dispatches only off
  the validator environment.
- **Use the trusted loader for reference entrypoints.** It authenticates the
  reference and its declared dependencies before evaluating the reference
  from its authenticated byte snapshot. A dependency sourced by that
  reference is read from disk again. The gate detects later dependency
  drift before guarded computation. A dependency's top-level work could
  already have executed if its bytes changed between authentication and
  the disk read.
- **Reflective forms are refused at a reference file's top level.** `eval`,
  `evalq`, `eval.parent`, `parse`, `source`, `sys.source` (outside the sealed
  block), `get`, `get0`, `mget`, `match.fun`, `getFunction`, `Recall`,
  `do.call` whose `what` names a listed function, a listed function passed
  as a value to `lapply`/`Map`/`Reduce`, and top-level aliases of binders are
  refused by the audit. The audit is a BLACKLIST of named spellings over a
  free-form top level and says so in `tools/method_manifest.R`: it closes the
  spellings it names, and the sealed block's positive whitelist is what
  carries the guarantee.
- **What this means for a task-module or reference author under Copilot.**
  Copy the sealed construction block byte-for-byte from any sealed reference
  (never retype it, never "tidy" it, never drop a `base::`); write the gate
  only as a closure over that object dispatching
  `private$require_validated_method(...)` first — never any other way; never
  `source()` or `sys.source()` another reference file at top level — a helper
  goes in the second locked environment and under `depends:`; and never bind
  `::`, `{`, `<-`, `function` or any construction primitive at top level (the
  audit refuses the file).
- **A manifest edit outdates the store.** The manifests are a file target in
  every task's DAG, so changing one invalidates every stage — no gate can be
  rendered or approved until the pipeline runs again.
- **A method with no approved unit is a STOP.** Never author new numerics to
  make progress; escalate to the SME.
- **Control-stream templates are units too.** `skills/reference-models/reference/*.ctl`
  are hash-bound in that skill's `MANIFEST.yaml` (draft until the SME signs a
  per-structure validation run). Never edit a template in place — adapt a COPY
  into the analysis via bbr and record `model_diff` against the template.

<!-- ASSUMPTION: path-scoped instructions use YAML frontmatter `applyTo` with a
     comma-separated glob, per VS Code custom-instructions docs. -->
