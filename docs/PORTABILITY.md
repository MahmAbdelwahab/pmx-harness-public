# Claude Code and GitHub Copilot

The harness uses shared task modules, roles, method manifests and gate tools
across both runtimes. Python 3.11 or newer is required to generate the Copilot
adapter. The editor-tool write guard uses PyYAML to read method dependencies.

## Claude Code

Open the repository with the public `.claude/settings.json`. Its local
SessionStart hook reads `roles/orchestrator.md`. Its PreToolUse hook inspects
editor writes. The main session orchestrates the specialist agents from
`.claude/agents/`. Claude Code reads the root `CLAUDE.md`.

## GitHub Copilot

1. Run `python3 copilot-adapter/setup.py install` from the project root.
2. Run `python3 copilot-adapter/setup.py check` to compare the generated files
   and inventory to the profile.
3. Open Copilot Chat and select `@pmx-orchestrator`. Follow the
   [Copilot runbook](COPILOT-RUNBOOK.md) to verify your host configuration.

Copilot settings and policy determine whether local hooks run. Record observed
behavior in the environment you use. The stock adapters invoke only local hook
scripts. An operator can independently configure additional tools under their
own site policy.

## Dispatch map

| Agent | Responsibility |
| --- | --- |
| `pmx-orchestrator` | Route stages, verify receipts and obtain decisions |
| `pmx-midd-strategist` | Frame the analysis plan and model-risk assessment |
| `pmx-data-assembler` | Build the analysis-ready data and run data checks |
| `pmx-exploratory-analyst` | Produce EDA and record observations |
| `pmx-modeler` | Draft and evaluate model candidates |
| `pmx-covariate-analyst` | Apply physiological priors and review candidates |
| `pmx-model-qualifier` | Run and interpret the declared diagnostics battery |
| `pmx-simulator` | Run the declared scenarios with recorded seeds |
| `pmx-report-writer` | Assemble the report and slides from gated artifacts |
| `pmx-reviewer` | Independently trace artifacts, reproduce results and route findings |

## Local gate signatures

Run the synthetic quick start in the [README](../README.md) first. A guarded
stage waits for a locally reviewed, digest-bound gate. Stock method manifests
also start unsigned. See the [method index](../skills/README.md) for scope and
the existing drafts.

Before approving a gate, render it from a fresh targets store and review the
generated package and evidence. The reviewer chooses the decision, supplies
their own name and date, and reviews the preview before confirming it. The
example variables below are paths and values supplied by that reviewer.

```sh
Rscript tools/sign_approve_gate.R preview "$gate" --decision approved \
  --signer "$reviewer_name" --date "$review_date" --out "$new_sibling_bundle"
Rscript tools/sign_approve_gate.R apply "$new_sibling_bundle" \
  --gate "$gate" --confirm "$preview_token"
Rscript tools/approve_gate.R verify "$analysis_id"
Rscript tools/errata.R verify "$analysis_id"
```

Use a new sibling bundle directory for preview. Keep the token local and use
the exact values emitted at preview. Release a signature through the approval
tool when its artifacts drift. Record corrections to signed claims through the
errata tool.

## Local guard and site configuration

The shared guard blocks editor-tool writes to gate records, their journals and
the hook scripts. It requires the owner's temporary unlock for harness core
and reviewed methods. R validates the exact gate and method bytes and both
journal chains independently. Shell commands run outside the editor-tool
interception. The editor rules preserve signed Markdown whitespace, and the
terminal approval settings request operator confirmation where supported.

Site operators choose and supply licensed engines, data, credentials and any
extra services independently. Keep operating values in the ignored local
configuration. Start from `harness.yaml.example` or inspect the local draft
from the installer. Use `python3 copilot-adapter/setup.py env` to review
environment drift.

See the [adapter README](../copilot-adapter/README.md) for standalone kits
and custom profiles.
