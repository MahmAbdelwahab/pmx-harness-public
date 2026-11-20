# Skill: model-management

Status: guidance-only skill — it gates no method reference unit and therefore
carries NO method manifest and NO SME signature. (Derived from Metrum MeRGE
Expo 1 (NONMEM FOCE) model management practice (bbr); pending SME review and
amendment.)
Source: https://merge.metrumrg.com/expo/expo1-nonmem-foce/posts/model-management-demo.html

Used by: modeler and covariate-analyst roles (Stages 3–4). bbr's YAML
metadata + numbered runs + `based_on` lineage IS the run-record discipline the
harness audit trail requires — agents follow it exactly; no parallel
bookkeeping.

## Core bbr workflow

| Action | Call | Rule |
|---|---|---|
| New model | `new_model(path)` | Never create the `<run>.yaml` by hand |
| Iterate | `copy_model_from(parent, new, .inherit_tags = TRUE)` | Never duplicate files manually — guarantees `based_on` lineage |
| Submit | `submit_model(mod)` / `submit_models(list(...))` | Global NONMEM settings in `bbi.yaml` at model-dir root; per-run overrides via `add_bbi_args()` |
| Milestone label | `add_description("Base model" / "Final Model")` | Milestones only |
| Run observations | `add_notes(c(...))` | Record the reason for each model change immediately after the run |
| Structured labels | `add_tags(TAGS$...)`, `replace_tag()` | Only from the project-wide `TAGS` glossary — never raw strings |
| Flag candidate final | `add_star()` | |
| Summarize | `model_summary()`, `param_estimates()`, `get_omega()`, `get_sigma()` | |
| Compare | `run_log(MODEL_DIR)`, `summary_log(MODEL_DIR, .include = ...)` | OFV/condition-number/status comparison table across a model family |
| Verify intended change | `model_diff(parent, child)` | Run BEFORE submission |
| Check convergence | `tail_lst(mod)` | After each run, before annotating |

## Conventions

- Flat model directory (e.g. `model/pk/`); sequential integer run numbers
  (`100`, `101`, …); per run: `100.ctl` + `100/` output dir + `100.yaml`
  metadata. No per-run subdirectory nesting.
- `bbi.yaml` at model-dir root carries the NONMEM path — this is where the
  harness's user-supplied `nonmem.path` (constraint C1) lands.
- Lineage = `based_on` chains; `run_log()` reconstructs the full family tree.

## Agent obligations (harness-specific, on top of MeRGE practice)

1. `model_diff()` output is attached to every submission's run record — the
   intended change is documented before the run, not reconstructed after.
2. `add_notes()` entry per run states: hypothesis tested, expected effect,
   observed outcome. This trail renders into the MAR model-development
   narrative.
8. The base-model review package carries the comparison table, fitted subject
   and observation counts, the estimator's termination message, parameter
   estimates with uncertainty, and the [standard diagnostic panel](../diagnostics/SKILL.md).
   The local reviewer evaluates each candidate against the analysis plan.
9. Count submissions against the declared estimation-run budget.
10. Annotate and keep unsuccessful runs. Document changes before resubmission.

## Model management for a user-configured nlmixr2 project

When the estimation engine is nlmixr2 rather than NONMEM, the bbr mechanics
above do not apply, but the discipline transfers one-to-one as
targets-native artifacts:

| bbr concept | nlmixr2/targets equivalent |
|---|---|
| `<run>.ctl` control stream | model function (one R file per candidate, numbered the same way) |
| `<run>.yaml` metadata | run metadata list target: run id, `based_on`, tags, notes (hypothesis / expected / observed) |
| `submit_model()` | `nlmixr2(model, data, est = ...)` inside a target; fit object cached by targets |
| `model_diff()` | diff of the model function files, attached to the run record |
| `summary_log()` | comparison table target built from all fit objects (OFV, AIC, condition number, convergence, parameters, RSE, shrinkage) |
| `tail_lst()` convergence check | fit `$message` / covariance status check after each run |

Rules that transfer unchanged: iterate by copying the parent model file
(lineage recorded), notes immediately after each run, failed fits kept and
annotated, the declared budget counted at fit invocation, and the local reviewer picks from the
comparison table.

## SME amendments

<!-- SME: add your TAGS glossary, run numbering ranges (e.g. 100s base,
200s covariate, 300s final), retry policy (parallel_retries?), and any
NMQual/qualification requirements. -->
