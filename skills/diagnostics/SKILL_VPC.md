# Visual predictive checks (tidyvpc)

Companion to `SKILL.md`, covering VPC specifically. Implementation:
`reference/vpc.R`, built on tidyvpc (Certara) 1.6.0.

Sources of truth:
- https://certara.github.io/tidyvpc/articles/tidyvpc_cont.html
- https://certara.github.io/tidyvpc/articles/tidyvpc_cat.html
- https://certara.github.io/tidyvpc/articles/tidyvpc_qpc.html

Everything below that describes package behaviour was either stated in those
vignettes or verified against the installed package. Where the two disagreed,
the installed package won and the discrepancy is noted.

## The pipeline

```
observed()  →  simulated()  →  [stratify()]  →  [censoring()]
            →  [predcorrect()]  →  binning() | binless()  →  vpcstats()
            →  [qpcstats()]
```

Order matters: stratification, censoring and prediction correction come after
`simulated()` and before the aggregation step.

## Data requirements (all VPC types)

- **Filter `MDV == 0` from both observed and simulated data before the
  pipeline.** Missing-DV rows must not enter it.
- Simulated data holds all replicates stacked, identified by a replicate
  column (`REP`, `Replicate`).
- Simulated subject and time structure must align with the observed data.
- **Prediction correction requires `PRED` on the *observed* data**, taken from
  one simulation replicate and merged on during preprocessing.
  `reference_vpc_continuous()` refuses with an explicit message otherwise.
- **Categorical VPC requires sorted data**: observed by subject then
  independent variable; simulated by replicate, subject, then independent
  variable. The vignette states this explicitly.

## Choosing the aggregation method

This is a scientific decision recorded in the analysis plan, not an agent
default. It determines what the VPC can show.

| Situation | Method |
|---|---|
| Nominal time exists | `list(method = "column", bin_variable = "NTIME")` — the most interpretable and the common pharmacometric choice |
| Irregular x-spacing | `"pam"` or `"jenks"` — they find natural groupings |
| Balanced counts wanted, speed matters | `"ntile"` |
| Bin edges would be arbitrary; continuous x with no nominal times | `list(method = "binless")` — additive quantile regression, smooth curves, no bin-count decision |
| Bin placement dictated by design or by a house convention | `"centers"` or `"breaks"` with explicit values |

Available: `column`, `ntile`, `pam`, `kmeans`, `jenks`, `centers`, `breaks`,
`binless`. (tidyvpc also offers `sd`, `equal`, `pretty`, `quantile` through
classInt; add them to the dispatch if the SME wants them.)

**Inspect bins before trusting statistics.** `plot()` on the object *before*
`vpcstats()` renders bin placement only. `reference_vpc_bincheck()` exposes
this.

Binless notes: `optimize = TRUE` selects lambda by AIC and removes the
subjective bin-count decision, at a computational cost that grows with strata.
For a prediction-corrected binless VPC, set `span` explicitly — an
automatically selected LOESS span can vary between runs, which breaks
reproducibility.

## Continuous VPC

`reference_vpc_continuous()`. Defaults: percentiles 5/50/95
(`qpred = c(0.05, 0.5, 0.95)`), `conf.level = 0.95`, `quantile.type = 7`
(use `6` for Phoenix comparability).

Prediction correction (`pred_corr = TRUE`) needs `log = TRUE` when the model
was fitted on log-transformed DV. Getting this wrong silently distorts the
correction — it is a required entry in the analysis plan, not a default.

Censoring: pass the BLQ flag column and LLOQ (and/or ALQ and ULOQ). Censoring
applies to observed data; simulated data is shown unchanged. The resulting
object carries a percent-BLQ table.

## Categorical VPC

`reference_vpc_categorical()`. The endpoint is declared through `yobs`/`ysim`;
statistics are the **proportion in each category per bin**, with simulated
prediction intervals on those proportions. Percentiles do not apply.

Binless for categorical uses a binomial GAM. User-supplied smoothing
parameters are a named list following `{stratum}.{prob<level>}`, e.g.
`female.prob0`, `female.prob1`. The names must match the strata level
combinations exactly.

## Quantified prediction check

`reference_qpc()` scores a VPC numerically instead of leaving it to visual
judgment. Six penalties, weighted into `qpc_score` (lower is better):

| Penalty | Detects |
|---|---|
| `coverage_penalty_med` | observed median outside the simulated median interval |
| `coverage_penalty_tails` | tail quantile miss rate |
| `mae_penalty_all` | deviation scaled by interval half-width |
| `rho_penalty_all` | monotonic drift of residuals over x |
| `sharpness_penalty` | unnecessarily wide bands |
| `interval_penalty` | Winkler interval score |

Installed defaults (verified in 1.6.0, not stated in the vignette):
`alpha = 0.05`, `w = c(med_cov = 0.35, tail_cov = 0.20, mae = 0.15,
drift = 0.10, sharp = 0.10, interval = 0.10)`.

Two uses worth knowing: it catches **overdispersed models that pass visual
inspection** — wide bands cover everything but score badly on sharpness and
interval — and it enables model comparison, provided every model is scored
with the same `sharp_ref` and `interval_ref`. Without those anchors each
score is self-normalized and cross-model comparison is meaningless.

## Reading the plot — the band is not a prediction interval

The shaded band is a **confidence interval on a percentile of a study this
size**, not a prediction interval on the data: it answers "where would the
5th/50th/95th percentile of a study this size land if the model were true."
It narrows as simulation replicates increase and widens as the study
shrinks. Judging the model by whether raw observations fall inside the band
is a different — and much weaker — check than the intended one: whether the
**observed percentile lines** stay inside the bands for their percentile.
(Added 2026-08-28 from a reviewed external skill; see git history.)

## What the analysis plan must declare

1. Aggregation method and its parameters (bin variable, nbins, breaks, or
   binless settings including `span` for pcVPC).
2. Whether prediction correction is used, and `log` if so.
3. Stratification variables.
4. Censoring handling.
5. Percentiles, confidence level, and `quantile.type` if not the defaults.
6. Whether QPC is computed, with weights and any reference anchors.
7. The seed.

## Defaults for a locally reviewed plan

Per-analysis MAP declarations may override any of these with justification;
absent a declaration, these hold.

1. **Aggregation: always bin.** Nominal time when the dataset carries it;
   `ntile` quantile binning when sampling is subject-specific. Binless is a
   per-analysis choice, never the silent default.
2. **Percentiles 5/50/95, confidence level 0.95.**
3. **`quantile.type` 7** (R default, harness-wide consistency).
4. **Default stratification: study, dose level, formulation/route** — each
   applied when the analysis pools more than one and (for dose) no
   prediction correction is in effect. Single-level analyses do not need
   an additional stratum.
5. **pcVPC only for heterogeneous designs** (mixed/titrated/adaptive dosing,
   influential continuous covariates on exposure within a stratum). Plain
   VPC is the default otherwise.
6. **Percent-BLQ panel always rendered when BLQ observations exist**; a
   model can match continuous percentiles yet mispredict censoring. No BLQ,
   no panel.
7. **QPC: optional battery member** (MAP opt-in), tidyvpc 1.6.0 default
   weights, anchored on the analysis's base/reference model when one
   exists; otherwise self-normalized and declared non-comparable.
8. **Replicate minimum 200**; the MAP may raise it,
   never go below.

## Discrepancies found between the vignettes and the installed package

- The continuous vignette shows `observed(data, x, y)`; installed 1.6.0
  takes `yobs`. The reference implementation uses `yobs`, which is verified
  working for both continuous and categorical.
- `qpcstats()` in 1.6.0 exposes `alpha`, `w`, `sharp_ref`, `interval_ref`;
  the vignette documents only the reference anchors.
