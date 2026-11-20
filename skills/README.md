# Skills

Each competency documents its inputs, method choices and review obligations. Method reference files are under `reference/`. A skill's `MANIFEST.yaml` binds each intended-use unit to its entrypoints, reference bytes and declared dependencies.

All 39 public units start in draft state with unsigned manifests. An authenticated load establishes which bytes the caller inspects. A guarded entrypoint also requires the applicable unit to be locally approved. Record the qualified reviewer's name and date in the manifest after reviewing the method in its intended context. The role label SME does not identify a signer.

These seven units were developed as drafts and retain that designation:

| Skill | Draft units |
|---|---|
| Baseline fit | `baseline` |
| Covariate priors | `covariate_search`, `psn_covsearch`, `scm_config` |
| Reference models | `onecmt_oral`, `twocmt_iv`, `twocmt_oral` |

Use the analysis declaration for project-specific thresholds, inputs and method choices. Keep analytical calculations in the method references. Preserve public citations and document review evidence in the local project.

The vendored engineering skills under `.claude/skills/` cover R and targets development. Their license and attribution apply independently of the method manifests.
