# Review guide

Use this guide to prepare a local project, review method and stage artifacts, and record the decision that permits dependent work.

## Prepare a project

Start with the [example configuration](../harness.yaml.example). Review the declared executable paths, library setup, and applicable site resources before confirming the environment in your local project configuration. Use `python3 copilot-adapter/setup.py env` to check the current environment against that configuration. Follow the [portability guide](PORTABILITY.md) for adapter and engine setup.

Keep analyst inputs and outputs in approved project storage. Licensed engine files and credentials are site resources. The synthetic quick start uses generated data and needs no real study inputs.

## Review a method

1. Identify the method unit, its declared entrypoints, reference bytes and dependencies in the corresponding `skills/` manifest.
2. Check the intended use against the analysis plan. Review representative positive and negative evidence and any limits of the method. Use the relevant skill instructions for its exact inputs and assumptions.
3. Record the decision against the exact revision per the local method process. Blank confirmation fields or draft methods keep the guarded entrypoints unavailable. Use an authorized local signer identity in the appropriate field.
4. Review changed reference or dependency bytes before treating a new manifest state as approved.

## Review a stage package

Inspect the declared inputs, resulting artifacts, and issues raised during the run. Confirm that the package and its tracked bytes describe the decision you are making. See [`tools/sign_approve_gate.R`](../tools/sign_approve_gate.R) for the staged `preview` and `apply` commands. Use preview to inspect the proposed approval and apply only the intended package with its confirmation value. Keep gate and approval records in the project audit trail.

If a discrepancy affects an approval, review the changed evidence and use the applicable local errata, revoke, or release process. The ledger verification commands in [`tools/approve_gate.R`](../tools/approve_gate.R) and [`tools/errata.R`](../tools/errata.R) check the recorded chains. The gate remains tied to the artifacts and context that were actually reviewed.

## Dispatch map (who owns what)

| Work | Role |
| --- | --- |
| Analysis coordination and review decisions | Orchestrator and local reviewer |
| Data preparation and quality checks | Data assembler |
| Exploratory data analysis | Exploratory analyst |
| Structural and covariate modeling | Modeler and covariate analyst |
| Qualification and simulation | Model qualifier and simulator |
| Report assembly and independent review | Report writer and reviewer |

The [role definitions](../roles/) give the dispatch boundaries. The [workflow guide](WORKFLOW.md) shows how those roles map onto the task graph. The [Copilot runbook](COPILOT-RUNBOOK.md) covers that runtime's setup and invocation.
