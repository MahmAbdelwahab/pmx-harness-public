# PMx Harness architecture

The harness combines pharmacometric task declarations, deterministic tools, and project review. Analysts and agents can propose a change. Tools record and verify the inputs and outputs. A local reviewer decides whether the result supports the next stage.

## Task composition

An analysis declares a stable identifier, an ordered set of task modules, and their inputs. [`compose_pipeline()`](../pipeline/compose.R) builds the `targets` graph, adds environment attestation, and inserts each stage's review gate. A task module declares its targets and package. The composer owns the gate edges.

The [synthetic example](../analyses/synthetic_example.R) shows the normal entrypoint with generated inputs. It prepares an analysis-ready dataset and illustrates the transition from assembly to a gated EDA stage. For an independent workflow, declare dataset, specification, model, or other inputs where the chosen modules require them. Explicit file targets track the bytes used in the resulting packages.

## Repository layers

| Layer | Responsibilities |
| --- | --- |
| `roles/`, `skills/`, `templates/` | Domain guidance, method source, and review artifacts |
| `tools/`, `pipeline/` | Deterministic checks, execution wrappers, task composition and artifact provenance |
| `.claude/`, `.github/`, `copilot-adapter/` | Runtime-specific agents, instructions, and supported local hooks |

The runtime adapters use the same project roles and tools. The source release includes portable local tooling. Configuration, study inputs, generated outputs and review records belong to each project.

## Method and gate integrity

A method manifest identifies its reference file, entrypoints, dependencies, and byte digests. The public manifests start unsigned. `load_reference()` checks reference identity before evaluating code. Guarded entrypoints separately require the declared local method approval.

Stage gates bind the approved package and artifact bytes to their project. Approval and errata ledgers track the associated transactions. A change to bound inputs needs a new review before the changed output can support a downstream stage. The optional adapter hooks help protect these files. The execution tools enforce the underlying checks when called from a supported project.

Reporting renders the promoted project packages and checks the resulting artifacts. Review uses declared report, assessment, and worktree inputs. Missing required records or unsupported module inputs must be resolved at their boundary.

## Verification

The home workflow contains light, full, and validated-stack jobs. The validated stack exercises the required EDA and rendered-report checks using its declared toolchain. The Copilot export provides its own portable one-job workflow. Synthetic coverage exercises the harness and its controlled failure paths. A project's licensed engine setup, scientific validation, and review remain local responsibilities.

See the [workflow](WORKFLOW.md), [review guide](SME-GUIDE.md), [runtime setup](PORTABILITY.md), and [threat model](THREAT-MODEL.md).
