# Skill: diagnostics

Review the exact local diagnostics method in `MANIFEST.yaml` before use. The public units ship as drafts. The guidance draws on Metrum MeRGE Expo 1 (NONMEM FOCE) model diagnostics practice and the PsN toolset.
Sources:
- https://merge.metrumrg.com/expo/expo1-nonmem-foce/posts/model-diagnostics.html
- https://uupharmacometrics.github.io/PsN/docs.html

Used by: modeler (per-run diagnostics, Stage 3), model-qualifier (Stage 5
battery). All plots via pmplots; data joined with `bbr::nm_join()`; axis
labels spec-derived via `yspec::axis_col_labs()` — never hardcoded. Filter to
`EVID == 0` before all diagnostic computation. Rendered as parameterized
reports (run number as parameter) so every model gets the identical panel.

## Standard per-run panel (Stage 3, every candidate)

| Group | Plots (pmplots) |
|---|---|
| GOF | `dv_pred()`, `dv_ipred()`, `dv_pred_ipred()` (individual profiles) |
| CWRES | `cwres_pred()`, `cwres_time()`, `cwres_tad()`, `cwres_q()`, `cwres_hist()` |
| NPDE | `npde_pred()`, `npde_time()`, `npde_tad()`, `npde_cont()`, `npde_cat()`, `npde_hist()` |
| ETA/EBE | `eta_pairs()`, `eta_cont()`, `eta_cat()` (covariate lists from yspec flags) |

Numeric companions per run (from `model_summary()`): OFV, condition number,
minimization status, parameter estimates with RSE, eta/epsilon shrinkage.

MeRGE prescribes no quantitative acceptance thresholds — **acceptance criteria
are pre-specified per analysis in the MAP technical criteria** (SME sets
them; the CHECK primitive tests them). Typical criteria the SME will
pre-specify: RSE bounds, shrinkage bounds for covariate-search eligibility,
condition number bound, successful minimization/covariance step.

## Plot integrity rules (added 2026-08-28)

Adapted from a reviewed external skill (`pmx-plots`); these are the defects
that make a diagnostic plot wrong rather than ugly, and they survive review
because the result still looks plausible:

- **Identity line, never a regression line**, on DV-vs-PRED/IPRED. A fitted
  line through the cloud hides exactly the bias the panel exists to show.
  pmplots does this correctly; the rule guards hand-rolled panels.
- **Shared axis limits and square aspect** on observed-vs-predicted, so a
  deviation of a given size looks the same in both directions.
- **Zero line and ±2 guides** on CWRES/NPDE panels — standardised residuals,
  roughly 5% of points belong outside ±2; none outside is as suspicious as
  many.
- **Log axes silently drop non-positive values** (warning only, never an
  error; the figure still looks normal). Count what will disappear before
  every log-scale plot and state the count — pre-dose zeros and BLQ coded
  as 0 are exactly this trap (this dataset's 9 C-flagged pre-dose zeros
  would vanish unremarked).
- **Median or geometric mean, never arithmetic mean**, when summarising
  concentration data — log-normal data pulls the arithmetic mean toward
  high outliers.

## Battery configuration (added 2026-08-28)

Simulation-based diagnostics are **configurable members, never hardcoded**.
The battery registry (`tools/qualification.R`) tags each member with the
MODEL_RISK levels that make it mandatory and whether it is simulation-based:

| Member | Mandatory at | Simulation-based |
|---|---|---|
| `gof` | low, medium, high | no |
| `vpc` | low, medium, high | yes |
| `pcvpc` | optional (enable in MAP) | yes |
| `npde` | medium, high | yes |
| `bootstrap` | medium, high | yes |
| external validation | high | depends |

The MAP declares: MODEL_RISK (selecting mandatory members), any optional
members enabled by name, and the simulation settings — replicate counts and
the **seed, fixed and recorded** so every simulation-based diagnostic is
reproducible. Disabling a risk-mandated member requires a written
justification and is recorded as a deviation; the tool refuses without one.

## Qualification battery tools (Stage 5, risk-scaled per D4/MODEL_RISK)

| Tool | Purpose |
|---|---|
| `psn vpc` / `npc` | Visual / numerical predictive check |
| `psn bootstrap` | Parameter uncertainty (percentile/BCa) |
| `psn sir` | Sampling importance resampling uncertainty |
| `psn cdd` | Case deletion diagnostics — influence by subject |
| `psn llp` | Log-likelihood profiling |
| `psn simeval` | Simulation-evaluation outlier detection |
| `psn qa` | Automated model quality checks (needs R + pandoc) |

Covariate-stage tools (Stage 4, per-MAP choice D6): `psn scm` / `scmplus` /
`boot_scm` / `xv_scm`, `psn frem`, `psn lasso`.

Utility tools agents may use: `update_inits`, `sumo`, `parallel_retries`,
`precond`, `resmod`, `transform`.

## Qualification numerics relocated from analysis code (2026-09-04)

`npde_tests` — the two global NPDE hypothesis tests. Once a reference NPDE
panel exists, the panel is summarised by two tests over all NPDE values: the
mean against 0 by a one-sample t-test, and the variance against 1 by a
two-sided chi-squared test on `(n - 1) * var`, the two-sided p-value taken as
`min(1, 2 * min(P(X <= x2), P(X > x2)))` on `n - 1` degrees of freedom. Both
p-values are compared with the MAP's flag threshold, which is a declared
parameter with no default. `reference_npde_global_tests(values, p_flag)` in
`reference/npde_tests.R` is the only place this arithmetic exists; it refuses
a missing threshold, a non-finite NPDE value, and fewer than two values.

`reference_vpc_coverage()` describes coverage of the simulated band by the
observed quantile and keeps the below-band and above-band counts. Use
`reference_weight_bands()` when the declared strategy uses weight bands.
`reference_npde_panels()` builds the NPDE panel set from an engine's prepared
NPDE table. Declare simulation settings, matching keys, and package versions
before comparing outputs across engines.

## Qualification verification

Load the declared references through the authenticated loader and check their
method status. The public manifest starts unsigned. Request local review of
the analysis plan, method inputs, and qualification criteria before running
guarded computations. Use the local approval and package checks before
promoting conclusions.

Use independently generated inputs to check coverage and miss counting,
stratum boundaries, bootstrap success accounting, interval summaries, and
qualification dispositions. Add missing, non-finite, and malformed input
cases to the same checks. Give the reviewer the declared test inputs,
criteria, and expected behavior in the project record. Where a criterion
requires visual judgment, retain the panel and ask the local reviewer for
the disposition.

## Cluster execution

PsN supports SLURM, Torque (PBS), SGE, and LSF; `-run_on_slurm` confirmed,
`-threads N` for parallel runs; cluster specifics configured in `psn.conf`.
bbr submission settings live in `bbi.yaml`. Harness `harness.yaml`
`cluster.scheduler` selects which; wiring finalized when the SME names the
scheduler.

## SME amendments

<!-- SME: set the default MAP technical-criteria menu (RSE/shrinkage/
condition-number bounds, VPC acceptance rules, bootstrap N and coverage
expectations), pcVPC vs VPC preference, stratification defaults, and which
battery members are mandatory at each MODEL_RISK level beyond the D4
defaults. -->
