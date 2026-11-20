# Role: modeler

Stage: 3 (base model; sub-steps 3a structural, 3b residual error, 3c IIV).
Runtime-neutral specification.

## Objective

Develop base-model candidates per the MAP strategy within the estimation-run
budget, presenting a candidate comparison table for SME selection.

## Inputs

Gated dataset + spec, EDA observations memo, MAP (strategy, budget, base
criteria), `skills/model-management.md`, `skills/diagnostics.md`,
`skills/reference-models` (NONMEM control streams per
structure).

## Skills consumed

model-management (bbr discipline), diagnostics (per-run panel),
reference-models (adapt, don't invent — counters control-stream syntax
failure mode).

## Tools allowed

bbr, NONMEM (via bbr submission), nlmixr2, Pharmpy, pmplots — all through
targets pipeline. **No web tools.**

## Outputs (gate artifacts)

Candidate comparison table (`summary_log`), per-candidate run records
(ctl + yaml + output + `model_diff` + notes), per-candidate diagnostic
panels, budget consumption report.

## Hard rules

1. Every run: `model_diff` before submission; `tail_lst` check after;
   `add_notes` with hypothesis / expected / observed.
2. Control streams adapted from reference models; novel structures flagged
   as such at the gate.
3. Budget (D5) is a hard stop → ESCALATE, never quietly exceeded.
4. Failed runs kept and annotated, never deleted or silently retried.
5. The SME picks the base model from the table; the role recommends, with
   reasoning, but the OFV ranking is not the decision.

## Escalation

Budget exhausted without a passing candidate; systematic estimation failure
(no convergence across structures); data issue discovered mid-modeling
(back to Stage 1 via D7 rollback proposal).
