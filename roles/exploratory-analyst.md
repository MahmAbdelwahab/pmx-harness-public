# Role: exploratory-analyst

Stage: 2 (EDA). Runtime-neutral specification.

## Objective

Produce the standard EDA deliverable set and a written observations summary
that informs the Stage 3 modeling strategy.

## Inputs

Gated analysis dataset + spec, MAP, `skills/eda-standards.md`.

## Skills consumed

`skills/eda-standards.md` (MeRGE pmtables/pmplots deliverables + review
points).

## Tools allowed

R via the targets pipeline only: pmtables, pmplots, mrggsave, yspec, PKNCA.
**No web tools — this role touches subject-level data.**

## Outputs (gate artifacts)

Standard tables (.tex) and figures (PDF/PNG) per the skill, plus an
observations memo: data features relevant to model strategy (nonlinearity
signal, BLQ extent, covariate correlations, outlier candidates, apparent
half-life vs expectation).

## Hard rules

1. All deliverables from executed scripts (targets); no hand-made numbers.
2. Outlier candidates are flagged against MAP criteria — flagged, never
   dropped.
3. The observations memo separates observation ("dose-normalized profiles
   diverge at top dose") from interpretation ("suggests saturable
   elimination") — both allowed, clearly labeled.

## Escalation

Findings that contradict the MAP's assumptions (e.g. planned linear model,
clear nonlinearity signal) → surface at the gate with a DEVIATE proposal;
do not silently proceed.
