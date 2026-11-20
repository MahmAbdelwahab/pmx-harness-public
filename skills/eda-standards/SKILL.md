# Skill: eda-standards

The public EDA units are draft. The data views and plot conventions draw
from Metrum MeRGE Expo 1. Figure registries use declarative entries.
Sources:
- https://merge.metrumrg.com/expo/expo1-nonmem-foce/posts/eda-tables.html
- https://merge.metrumrg.com/expo/expo1-nonmem-foce/posts/eda-figures.html

Used by: exploratory-analyst role (Stage 2). All deliverables are executed
scripts (targets), rendered to the EDA gate package.

The six manifest units start in draft state with a blank reviewer.
Use the authenticated reference loader to inspect the bound bytes. The
entrypoints require local approval of the applicable unit to compute.

## Toolchain

| Package | Role |
|---|---|
| `yspec` | Units/labels/decodes/roles from the specification: `ys_load()`, `ys_check()`, the `SETUP__: flags:` block (projected by `tools/external.R::ys_projection`) |
| `ggplot2` | The drawing layer. The REFERENCE builds every figure with identity geoms from frames it computed; ggplot2 computes nothing that is displayed |
| `pmtables` | LaTeX rendering of the reference-computed tables: `stable()` + `stable_save()`. The `pt_*()` helpers that compute inventories and summaries themselves are NOT used — values compute in the reference, pmtables formats |
| `pmplots` / `GGally` | The reference computes pairs-plot correlations and bins before drawing the matrix |
| `mrggsave`, `mrgmisc` | Not used by the reference (pagination is a declared registry parameter the reference applies; PDFs are written by the reference through `grDevices::pdf()` so the page geometry and the bytes can be read back and verified) |

## Reference implementation and intended-use units

`reference/eda.R` holds every EDA numeric AND the construction of every
figure, one manifest unit per intended use. No function in it knows a column
name: every column is a declared parameter, resolved by the caller from the
dataset's yspec specification.

| Unit | Entry points | What it decides |
|---|---|---|
| `record_selection` | `reference_eda_view`, `reference_eda_rowset_digest` | Which records the EDA describes: the kept-record rule (below), the observation and dose event filters (from the specification's decodes, asserted against the NONMEM convention), the one-row-per-subject rule. Stamps a stable row id, returns five views (analysis / observations / doses / subjects / excluded) with counts, a row-set digest each, the exclusion reasons found, and the method parameters applied. |
| `summary_statistics` | `reference_eda_inventory`, `reference_eda_continuous_summary`, `reference_eda_categorical_summary` | Inventory per declared grouping (study, dose group) with GROUP-SPECIFIC counts of subjects, observations, BLQ, excluded and records read; n / missing / mean / median / sd / min / max per continuous covariate; n / percent per level per categorical covariate. Every convention is a declared parameter emitted with the result (below). |
| `log_axis_audit` | `reference_eda_log_axis_audit`, `reference_eda_log_axis_audit_table` | The census below, per declared log axis, classifying every dropped value as missing / zero / negative / non-finite BY ROW ID with its BLQ status. |
| `display_transforms` | `reference_eda_quantile_bins`, `reference_eda_normalize` | Covariate quantile bins (declared quantile type and interval closure) for stratified profiles; normalisation of one column by another. |
| `figure_construction` | `reference_eda_build_figure` | Builds every figure from its DECLARATIVE registry entry. Computes the LOESS smooth (declared span and degree, evaluated at every row), histogram bins and counts (declared rule and count), pairs-plot correlations (declared method), box statistics (declared quantile type and whisker rule), dose-event markers, pagination; draws them with identity geoms from frames that carry the row id. |
| `registry_validation` | `reference_eda_registry_check`, `reference_eda_registry_active`, `reference_eda_registry_index`, `reference_eda_plot_rowset_digest`, `reference_eda_figure_check`, `reference_eda_core_status` | The registry's declared shape; the checks taken from the BUILT PLOT and the WRITTEN FILE (below); the index that requires every active entry exactly once and an enforced `inapplicable` state with a reason for any core deliverable that cannot be produced. |

Every unit records `tools/record_selection.R` under `depends:` in the
manifest (path and sha256): it is the harness-wide record classifier the
reference relies on, and an edit to it is a hard stop like an edit to the
reference itself. The reference also loads the validator
(`tools/method_manifest.R`) and that dependency into a PRIVATE, LOCKED
environment at source time and closes its self-gate over it (round-7 finding
4): a global `require_validated_method <- function(...) TRUE` does not unlock
a draft unit. Tests exercise the numerics through a manifest FIXTURE in a
tempdir (units approved with the current digests, `reviewed_by:
test-fixture`), never through a stub and never by touching the real manifest.

Limit: the reference's names are ordinary bindings in the environment that
sourced it. Caller code that rebinds `.eda_gate`, `.eda_source_identity` or
any helper after sourcing runs whatever it bound. This is not defended in
code; it is a defect in the caller, visible in the caller's diff, which the
gate package hashes (`inputs$eda$code`). What IS defended in code (review 2,
H2): the structural digest of every definition in `EDA_ENTRYPOINTS` and
`EDA_INTERNALS` is compared with the file on disk at every gated call in BOTH
source modes, so a rebound helper (`.eda_kept`, `.eda_bins`, ...) stops; the
gate returns its dependency environment (`.eda_deps`) to the entry point, so
the classifier and the event assertion are never looked up dynamically, and a
rebound `.eda_private` or `.eda_deps` stops. Since integrity batch 12 the
validator (`.eda_private`, loading only tools/method_manifest.R) and the
dependencies (`.eda_deps`, loading tools/record_selection.R) live in two
separate locked environments, so a dependency can never overwrite the
validator or its digest helper before the seal.

Task modules and analysis helpers are DECLARATIVE CALLERS: they wire
targets, declare parameters and assemble the gate package. A record filter,
an arithmetic expression or a plotting function in either of them is a
defect, not a shortcut. Every count in the gate package comes back from the
reference (`reference_eda_view$input_counts`, `reference_eda_core_status$counts`).

Performance note (review 2, L8): every gated call re-validates the manifest
and re-hashes the reference and its dependency. A cache keyed by manifest
mtime and size was considered and NOT added: mtime/size are not content, and a
cache that could serve a stale verdict for an edited manifest is exactly the
kind of shortcut the gate exists to refuse. The on-disk definitions digest
is cached by file sha256.

**Which file is the approved method.** The manifest names one
`reference_path`, and that is the only file allowed to compute. The reference
checks at every gated call that the functions now RUNNING came from that file
— by source reference where one is available, and otherwise by comparing a
digest of the running definitions against the definitions in the file on
disk — and that its dependency files still have the bytes it loaded.

### The kept-record rule (record_selection) — defined ONCE, in tools/record_selection.R

> A record is KEPT when its exclusion flag is NA, empty, whitespace-only, or
> the NONMEM missing token `.` (compared after trimming). Any other value is
> an exclusion reason and the record is outside the EDA views.

The rule, its token constant
(`HARNESS_MISSING_TOKENS`) and the reader's NA-coding constant
(`HARNESS_READ_NA_STRINGS`) live in `tools/record_selection.R` and are called
by `tools/external.R` (the reader), `tools/qc_battery.R` (the C-flag check),
this reference (`.eda_kept` delegates) and `tools/nlmixr_runs.R` (the fitting
set). None of them restates the predicate, so the EDA describes, QC checks and
the estimator fits the same record set. The exclusion reasons are tabulated
in the gate package. A view with no kept records stops the task.

**Event convention.** Which event value means "observation" and which means
"dose" is read from the specification's `decode:` annotation and then
ASSERTED to be exactly 0 and exactly 1 (`harness_assert_event_convention`),
because the QC battery and the fitting adapter select by those values. A
specification that decodes them differently stops with an ESCALATE; the
convention is enforced, not inferred. Where it is asserted: in the EDA
reference (`reference_eda_view`) and in the QC battery when a specification
is supplied (`run_qc_battery(data, expect, spec = <path>)`, item
`event_convention`; without a spec the item reports "not assessed"). The
fitting adapter (`tools/nlmixr_runs.R`) relies on assembly QC and EDA to
assert the specification convention.

### The subject denominators

`n_subjects` in the data inventory counts subjects present in the
**observations** view of the group; the covariate summaries are computed on
the **subjects** view, the first kept record per subject regardless of event
type, and each continuous covariate is asserted CONSTANT within subject across
kept records (a time-varying covariate stops: the snapshot would silently pick
one of several values). A covariate MISSING on a subject's first kept record
but present on a later kept record also stops (review 2, L3, decision: stop
rather than silently take a later value) — the rule is declared and emitted
with both summaries.

### Method parameters (round-7 finding 3)

Every estimator's convention is declared in code and emitted with its result,
and the gate package prints them in one table:

| Parameter | Convention |
|---|---|
| SD | sample standard deviation, denominator n - 1 (`stats::sd`); NA when n = 1 |
| median | `stats::median`: middle order statistic; for even n the mean of the two central values |
| missing values | NA and NaN removed before every statistic, counted in `n_missing` |
| non-finite values | Inf / -Inf STOP (data defect), never summarised |
| rounding | mean / median / sd rounded to the declared digits for presentation; n, min, max as computed; percent rounded to declared digits |
| quantile type | R type 7 (`stats::quantile`), declared per call |
| interval closure | right-closed, lowest bin including its left end: [b0, b1], (b1, b2], ... |
| BLQ | counted by flag match against the specification's decode; a MISSING BLQ flag on an observation stops (declared policy `blq_missing = "stop"`); no DV substitution or censoring is performed by the EDA |
| subject snapshot | first kept record per subject in input order; covariates asserted constant within subject; a value missing on that record but present later stops |
| exclusion denominators | per group: excluded records and records read are counted by the record's own grouping values, so grouped rows add up |
| LOESS | `stats::loess`, family gaussian, declared span and degree, `surface = "direct"` (exact local fit at each evaluation point; the `loess.control` default "interpolate" is not used), fitted on the LINEAR scale of x and y as observed on finite pairs, evaluated at every row's x and drawn on the declared axes |
| histogram bins | equal width over the observed [min, max], declared count, right-closed |
| correlation | `stats::cor` on the complete finite pairs, declared method (pearson / spearman) and `use`, one row per subject; NA (shown as `r = NA`) below 3 complete pairs or with zero variance |
| box statistics | quartiles by the declared quantile type; whiskers to the most extreme value within the declared multiple of the IQR (Tukey, 1.5); every record drawn as a point; levels ordered by the raw value (numerically when numeric); a level with no finite value is a recorded note and a zero-n row, never a silent skip |

## The column-role contract

The specification is projected to a plain list
(`tools/external.R::ys_projection`) and preserved as a pipeline target, and
the EDA takes its semantics from it:

| From the spec | Used for |
|---|---|
| `SETUP__: flags:` blocks | which column plays which ROLE |
| per-column `short:` + `unit:` | every axis label |
| per-column `values:` + `decode:` | which event value means an observation or a dose, which flag value means below LLOQ, categorical level labels |

House role vocabulary (the flag names a specification must carry):

| Flag | Role | Cardinality | When absent |
|---|---|---|---|
| `eda_id` | subject identifier | exactly 1 | STOP |
| `eda_exclusion` | record-exclusion / comment flag | exactly 1 | STOP |
| `eda_event` | event-type identifier (decodes `observation` and `dose`) | exactly 1 | STOP |
| `eda_time` | independent variable | exactly 1 | STOP |
| `eda_dv` | observed dependent variable | exactly 1 | STOP |
| `eda_blq` | below-LLOQ flag | exactly 1 | STOP |
| `eda_covariate_continuous` | continuous covariates | at least 1 | STOP |
| `eda_covariate_categorical` | categorical covariates | any number | the categorical summary and the boxplots are recorded INAPPLICABLE with the reason |
| `eda_study` | study identifier (inventory grouping) | 0 or 1 | the inventory grouping by study is recorded INAPPLICABLE |
| `eda_dose_group` | nominal dose group (inventory grouping, profile colour) | 0 or 1 | the inventory grouping by dose group is INAPPLICABLE; profile colour falls back to the group role (subject) and the figure notes say so |
| `eda_stratify` | covariate the stratified profile view bins | exactly 1, required only when that catalog figure is enabled | STOP when enabled |
| `eda_dose` | dose column the dose-normalised view divides by | exactly 1, required only when that catalog figure is enabled | STOP when enabled |

A role the specification does not annotate is never inferred from a column
name or short label. The resolved mapping is a table in the gate package.

**Overlay specifications (round-7 finding 8).** A dataset's assembly spec is
immutable once its gates are signed. EDA semantics are added in a SEMANTIC
OVERLAY file whose header declares `# overlay-of: <base spec>` and
`# overlay-of-sha256: sha256:<hex>`; the overlay is never called the assembly
spec. `tools/external.R::ys_overlay_provenance()` reads the header; the EDA
task checks at composition, and again before packaging, that the base spec on
disk still has that sha256, and the base spec is a file target hashed at the
gate. `tools/tests/test_eda_external_spec.R` asserts that every per-column key
of the overlay equals the base spec except the enumerated additions, and every
`SETUP__` key except the enumerated ones. The enumerated differences are
exactly three: the `SETUP__: flags:` block (added), the `SETUP__: description`
(an overlay-specific description), and the BLQ `decode:` annotation.

<!-- SME: extend or rename the role vocabulary here; it is house convention,
not a yspec requirement. -->

## Standard tables (all as `.tex` through `pmtables::stable()`, CSV alongside)

| Table | Reference entry point | Content |
|---|---|---|
| Data inventory | `reference_eda_inventory` | subjects, observations, BLQ, excluded, records read — by study and dose group when the specification flags them, one overall row otherwise (grouping recorded inapplicable) |
| Categorical covariates | `reference_eda_categorical_summary` | n and % per decoded level, per covariate (inapplicable when none is flagged) |
| Continuous covariates | `reference_eda_continuous_summary` | n, missing, mean, median, SD, min, max |

## Standard figures — DECLARATIVE registry, built by the reference

| Figure | Kind | What the reference computes |
|---|---|---|
| Concentration vs time, linear | `profiles` | colour by dose group (fallback: subject), LOESS smooth (declared span, degree) evaluated at every row |
| Concentration vs time, log | `profiles`, `y_transform = log10` | same colour rule; audited by the log-axis census |
| Individual profiles | `individual_profiles` | one panel per subject, dose-event vertical lines from the `doses` view, pagination over the subjects view at the declared subjects per page (a subject with doses and no observation still gets a panel) |
| Covariate distributions | `histograms` | equal-width bins and counts per continuous covariate, drawn as rectangles |
| Continuous-covariate pairs | `pairs` | k x k faceted matrix: scatter below the diagonal, histogram on it, correlation (declared method, n) above it |
| Continuous-by-categorical boxplots | `boxplots` | quartiles, whiskers, all points; inapplicable when no categorical covariate is flagged |
| Dose-normalised conc vs time (catalog) | `profiles` with `derive = normalize` | value / dose x scale, undefined divisors counted |
| Covariate-stratified profiles (catalog) | `profiles` with `derive = quantile_bins` | tertile cut points over the subjects view |

## Formatting conventions

- `ggplot2::theme_bw()` on all plots (the reference does not import
  pmplots); axis labels from the specification's short label and unit.
- Colour: `scale_colour_brewer(palette = "Set1")` when the colour role has at
  most 9 levels; otherwise the default discrete scale. When the colour role
  falls back to the subject the legend is suppressed.
- Tables: the CSV holds the values as computed; the `.tex` is
  `pmtables::stable()` over the same frame (no pmtables computation).

## Log-axis audit (added 2026-08-28; classified by row id 2026-09-03)

Before every log-scale figure, count the values the scale will drop. ggplot2
emits only a warning — which never reaches the gate package — and the plot
still looks entirely normal with the rows gone. The census is computed by
`reference_eda_log_axis_audit()` on the same canonical view object the
figures are drawn from, ONCE PER LOG AXIS THE REGISTRY DECLARES
(`reference_eda_log_axis_audit_table()`): each entry is audited on its own row
selector and on the column that axis maps. Every dropped value is classified
as missing (NA / NaN), zero, negative (finite) or non-finite (Inf / -Inf) and
listed BY ROW ID with its BLQ status; the table carries the counts and the
BLQ cross-count (non-positive values also flagged below LLOQ). Non-positive
values with no matching BLQ or exclusion-flag accounting are a data question
for the assembly stage, not a plotting detail.

**Reconciliation against the built plot (round-7 finding 9).** The same
classification predicts, for every layer of every rendered figure, how many
positional values ggplot2 will hold as non-finite after the declared
transform. `reference_eda_figure_check()` runs `ggplot2::ggplot_build()` on
each page and asserts, per layer, that the built data carries exactly the
frame's rows (identity statistics) and exactly the predicted number of
non-finite positions. A mismatch stops the figure: the audit must describe
what the figure draws. (ggplot2 removes those rows at draw time — in
`handle_na()`, after `ggplot_build()` — for point layers, and breaks lines at
interior ones for path layers; the reconciliation is stated at the build
stage, where both are visible as non-finite positions.)

## Plot tiers and the registry pattern (declarative shape 2026-09-03)

Every EDA figure is an entry in the project's plot registry
(`<analysis>/eda/eda.R`). An entry is a DECLARATION; it carries no R
function, and `reference_eda_registry_check()` refuses one that does:

| Field | Meaning |
|---|---|
| `tier` | `core` or `catalog` |
| `file`, `w`, `h` | output PDF name (unique across the registry) and page size in inches; VERIFIED against the written file (every `/MediaBox`, one page per built page, and the sha256 of the bytes the reference wrote) |
| `kind` | `profiles`, `individual_profiles`, `histograms`, `pairs`, `boxplots` — the layer plan the reference builds and the checker verifies |
| `rows` | the row selector: `analysis`, `observations` or `subjects` — the ROW-SET IDENTITY the built-layer digest check asserts |
| `x_transform`, `y_transform` | `identity` or `log10`, applied by the reference; a non-profile kind must declare identity; a log axis on a derived column is refused (not auditable) |
| roles | `x`, `y`, `group`, `facet`, `variables`, `by` name ROLES; `colour = list(role, when_absent = "group")`; `"derived"` names the column a `derive` block produces |
| `requires` | roles whose absence makes the entry INAPPLICABLE (recorded with the reason naming the flag) instead of a stop |
| parameters | `smooth`, `bins`, `correlation`, `box`, `markers`, `page`, `derive` — the declared parameters every displayed statistic is computed from |

The reference WRITES the PDF (`reference_eda_write_figure`, review 2 M1,
decision B) — the one file it writes, because the file hashed in the gate
must be the file the reference proved — and returns the sha256 of the written
bytes; `reference_eda_figure_check` compares that digest with the file it
reads back, and `reference_eda_registry_index` compares it again with the file
on disk at packaging, so a foreign PDF of the declared size is refused.

Render-time checks, taken from the BUILT OBJECT and the WRITTEN FILE
(`reference_eda_figure_check`): every layer's data carries the row id; per
layer, the union of row ids over all pages digests to the view the layer
declares (dose markers to `doses`, profile layers to the entry's `rows`),
every page is a subset and pages are pairwise disjoint; the x and y scales
carry exactly the declared transform and the coordinate system is not a
transforming one; `ggplot_build()` reconciles as above; the PDF has the
declared page size and page count. `reference_eda_registry_index()` then
requires every ACTIVE entry exactly once — rendered, or inapplicable with a
non-empty reason — refuses duplicate renders, renders of disabled entries,
and any rendered file name, dimension, row selector or row-set digest that
differs from the declaration.

| Tier | Default | Governance |
|---|---|---|
| **core** | Always rendered or recorded INAPPLICABLE with the specification's reason | Mandated by this skill; `reference_eda_core_status()` refuses a core deliverable (table or figure) in any other state. The core FIGURE set is the registry's core tier; the core TABLE set is `EDA_CORE_TABLES` in the reference (data inventory; its grouping by study; its grouping by dose group; continuous covariate summary; categorical covariate summary), and a caller that omits one is refused (review 2, M2). The gate package lists every core deliverable with its state |
| **catalog** | Off | Enabled per analysis in the MAP at the analysis-setup gate; mid-analysis enabling is additive (no deviation) |
| ad-hoc | — | New registry entries authored on SME request mid-analysis (as declarations of an existing kind; a new kind is a reference change under the manifest); enter gate review like any artifact |

Core set: data inventory (by study and dose group), categorical + continuous
covariate summaries, covariate distributions, continuous-covariate pairs plot,
continuous-by-categorical boxplots, conc-time linear + log, individual
profiles.

Catalog (initial): covariate-stratified profiles (by covariate tertile),
dose-normalized conc-time. Not yet declared as kinds (a reference change when
requested): conc vs TAD, missingness heatmap, per-study panels, time-varying
covariate profiles.
<!-- SME: extend the catalog and its when-applicable notes here. -->

### Inapplicable deliverables

A missing optional role makes its dependent deliverable inapplicable.
Record the role that would enable the deliverable. Use a nominal dose-group
column for group summaries and colors when the project defines dose levels.

## Spot-check review points (what the SME looks at, at the gate)

1. Inventory table: unexpected zeros or outlier counts per study/group.
2. Covariate summaries: implausible min/max/mean → data entry or unit errors;
   degenerate categorical distributions → coding errors.
3. Pairs plot: unexpected correlations, outlier clusters.
4. Individual profiles: missing doses (a panel with no marker), impossible
   concentrations, time-ordering errors.
5. Log-scale plot: BLQ pattern, non-positive concentrations (the audit table
   lists them by row id).
6. Dose-normalized plot: dose-proportionality departure (informs model
   nonlinearity strategy in Stage 3).
7. Apparent half-life vs prior/literature expectation (NCA sanity).
8. Method parameters table: every convention is the one the analysis plan
   intended.

Outlier candidates are flagged against SME-defined criteria — flagged, never
dropped; any exclusion goes through the exclusion flag with documented
rationale.

## SME amendments

<!-- SME: add required deliverables beyond MeRGE defaults — e.g. NCA summary
table policy, time-varying covariate plots, per-study paneling rules,
renal/hepatic subgroup views, whatever your standard reviews expect. -->
