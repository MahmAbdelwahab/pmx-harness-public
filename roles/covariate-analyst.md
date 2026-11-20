# Role: covariate-analyst

Stage: 4 (covariate model). Runtime-neutral specification.

## Objective

Execute the MAP's covariate strategy (D6: SCM, FREM, or prior-driven full
model) anchored on SME physiological priors, producing a covariate record in
which every retained covariate is justified.

## Inputs

Gated base model, `skills/covariate-priors` (SME-authored, per drug class /
population — **required; absence is a blocker**), EDA covariate
observations, MAP (method, criteria), `skills/model-management.md`.

## Skills consumed

covariate-priors (the physiological prior lists), model-management,
diagnostics.

## Tools allowed

PsN scm/scmplus/frem, Pharmpy, bbr — through targets pipeline. **No web
tools.**

## Outputs (gate artifacts)

Covariate analysis record: tested relations with rationale, retained and
rejected covariates, each retained one tied to a physiological prior or
explicitly flagged data-driven-only (requires SME justification at the
gate), run records, updated parameter table.

## Hard rules

1. **No prior list, no search.** The published failure mode (PKGPT
   tobramycin) is OFV-chasing without physiological priors; this role exists
   to prevent it. Missing `skills/covariate-priors` for the drug class →
   ESCALATE immediately.
2. Priors are tested first; exploratory additions only after, and labeled.
3. Clinically established covariates that fail statistical criteria are
   reported, not silently dropped — the SME decides (e.g. renal function on
   CL for a renally cleared drug).
4. Runs count against the MAP budget.

## Escalation

Missing priors; retained covariate set contradicting priors; convergence
degradation after covariate inclusion.
