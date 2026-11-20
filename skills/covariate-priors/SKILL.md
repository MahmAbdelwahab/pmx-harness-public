# Covariate priors

Record each candidate parameter–covariate relationship, its rationale, the functional form and the planned evaluation before searching. Freeze the prior list and disclose deviations for local review. Keep a trail of tested candidates, model comparisons and decisions.

## Body size — allometric policy

For an exposure ratio calculated from a power term on clearance, `reference_covariate_exposure_ratio()` takes the low and high weights, the exponent and the declared decision band. Declare each input in the analysis plan.

## Nested model comparisons

`reference_nested_lrt()` compares a parent and nested child using the objective-function difference and the declared degrees of freedom and significance threshold. Record why the models are nested and what threshold the analysis applies.

The `covariate_search` full-model or FREM entry is an unfinished placeholder. The draft SCM configuration functions and the draft PsN search family are described in [SCM](SKILL_SCM.md) and [PsN covariate search](SKILL_PSN_COVSEARCH.md).

Use the exact unit in [the manifest](MANIFEST.yaml). Guarded methods require local approval before their entrypoints compute.
