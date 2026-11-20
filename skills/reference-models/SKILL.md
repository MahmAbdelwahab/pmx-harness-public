# Skill: reference-models

Status: DRAFT v0.1 — STARTER SET, NONMEM ONLY. Authored 2026-09-01 by the
modeler role against the input named as missing in `roles/modeler.md`
("reference-models: to be authored: validated control streams per
structure"). Every template in `reference/` is `draft` in
`MANIFEST.yaml` until the SME runs a validation run per structure and
flips it. This skill is born unsigned; it carries no signature line and
none is to be added by an agent.

Used by: modeler (Stage 3, base-model candidates) and covariate-analyst
(Stage 4, where a covariate model is an adaptation of the accepted base
model rather than of a template).

These example control streams illustrate common NONMEM starting structures.
They are supplied as draft method source for local adaptation and review.
See [NONMEM estimation](../nonmem-estimation/SKILL.md) for preparation and
[model management](../model-management/SKILL.md) for tracking the adaptation.

## Why this library exists

The published failure mode of LLM-authored pharmacometric work is
**control-stream syntax**: a stream that is plausible, compiles, runs, and
answers a different question than the one asked. The countermeasure is not
more careful authoring. It is to stop authoring.

**Control streams are ADAPTED from this library. They are never invented from
scratch.** The template is the validated baseline; the analysis-specific
delta is what gets reviewed, and it is reviewed as a diff, not as a whole
file a reader has to re-verify line by line.

### Novel structures

A structure that is not in this library is a **NOVEL STRUCTURE**. Building
one is allowed. Building one quietly is not.

- The candidate is labelled `novel structure` in its run record and in the
  base-model gate package, naming the closest template it departs from and
  in what way.
- The gate package states that no validated baseline covers it, so the SME's
  review of that candidate is a review of the whole control stream, not of a
  diff.
- This is an **escalation marker, not a prohibition**. The point is that the
  SME knows which candidates carry template backing and which do not, before
  choosing among them.

Structures the starter set does NOT cover, and which are therefore novel
today: transit-compartment or other non-first-order absorption, lag time,
zero-order or mixed absorption, three or more disposition compartments,
Michaelis-Menten or other non-linear elimination, target-mediated
disposition, any `$DES` / ADVAN6/13 general-nonlinear model, PD and
PK/PD-linked models, time-to-event and categorical models, BLQ likelihood
handling (M3/M4), and IOV. Several of these are ordinary requests; the label
records their status, it does not refuse them.

## The starter set

| Unit | File | Structure | Subroutines | Parameters | IIV in template |
|---|---|---|---|---|---|
| `onecmt_iv` | `reference/onecmt_iv.ctl` | 1-cmt, IV | ADVAN1 TRANS2 | CL, V | CL |
| `onecmt_oral` | `reference/onecmt_oral.ctl` | 1-cmt, first-order absorption | ADVAN2 TRANS2 | KA, CL, V | CL, KA |
| `twocmt_iv` | `reference/twocmt_iv.ctl` | 2-cmt, IV | ADVAN3 TRANS4 | CL, V1, Q, V2 | CL |
| `twocmt_oral` | `reference/twocmt_oral.ctl` | 2-cmt, first-order absorption | ADVAN4 TRANS4 | KA, CL, V2, Q, V3 | CL, KA |

All four share the same house idiom: mu-referenced log-scale THETAs,
proportional residual error, `$ESTIMATION METHOD=1 INTERACTION`, a
`$COVARIANCE` record, and a `$TABLE` whose first column is `NUM` for
`bbr::nm_join()`.

Two naming traps the templates comment on in place, because they are silent
when got wrong:

- **The scaling compartment.** `S1` under ADVAN1/ADVAN3 (central is
  compartment 1); `S2` under ADVAN2/ADVAN4 (compartment 1 is the depot).
  Scaling the wrong compartment produces predictions on the wrong scale that
  still look like a plausible fit.
- **The volume names under TRANS4.** ADVAN3 names the central volume `V1`
  and the peripheral `V2`; ADVAN4 names them `V2` and `V3`. Moving a model
  between the two ADVANs without renaming is the most common way a
  two-compartment stream ends up describing something else.

## Adaptation workflow

1. **Pick the template** from the table above. If none fits, see *Novel
   structures* — pick the closest and label the departure.

2. **Copy through bbr, never by hand.** `copy_model_from()` is what creates
   the `<run>.yaml` and writes the `based_on` lineage; a manual file copy
   produces a run with no ancestry and is a breach of
   `skills/model-management/SKILL.md`.

   ```r
   # First candidate of a family: seed the model directory from the template,
   # then let bbr create the model object and its YAML.
   file.copy("skills/reference-models/reference/onecmt_iv.ctl",
             file.path(MODEL_DIR, "100.ctl"))
   mod100 <- bbr::new_model(file.path(MODEL_DIR, "100"))
   bbr::add_tags(mod100, c(TAGS$one_compartment, TAGS$iv))
   bbr::add_notes(mod100, "Adapted from reference template onecmt_iv.ctl.")

   # Every later candidate: copy from its PARENT RUN, not from the template.
   mod101 <- bbr::copy_model_from(mod100, 101, .inherit_tags = TRUE)
   ```

   Lineage follows the model family once a parent run exists. The template is
   the ancestor of the *first* candidate only; a two-compartment candidate
   built as the extension of a fitted one-compartment run is `based_on` that
   run, and the template is named in the notes.

3. **Rename to the analysis.** Rewrite the `$PROBLEM` line, fill the
   run-record header block (run id, `based_on`, tags, hypothesis, expected),
   replace the `$INPUT` column list and the `$DATA` path, replace every THETA
   / OMEGA / SIGMA initial, and set the `$TABLE` file name. Every one of
   these is marked `ADAPT-HERE` in the template. Nothing marked `KEEP`
   changes without a note saying why.

4. **Diff against the template before the first submission.**

   ```r
   bbr::model_diff(.mod = mod100, .mod2 = <template-as-model>)   # or:
   diffobj::diffFile("skills/reference-models/reference/onecmt_iv.ctl",
                     file.path(MODEL_DIR, "100.ctl"))
   ```

   The diff is attached to the run record. This is the whole point of the
   library: what the SME reviews is the delta from a validated baseline, and
   that delta is recorded **before** the run, not reconstructed after it.
   For later candidates the recorded diff is against the parent run, per
   `skills/model-management/SKILL.md` — the template diff is taken once, for
   the candidate that first adapts it.

5. **Submit, check, annotate** — unchanged from
   `skills/model-management/SKILL.md`: `submit_model()`, then `tail_lst()`
   for the verbatim termination message, then `add_notes()` with hypothesis
   / expected / **observed**. A failed run is annotated and kept.

## Placeholder initials are not estimates

Every THETA, OMEGA, and SIGMA value in `reference/` is a round, neutral
number chosen so the template reads as a complete control stream. They are
labelled `PLACEHOLDER` on their own line. They encode no prior, no
literature value, and no expectation about any analysis. **Carrying a
placeholder initial into a submitted run is an error**, not a default: the
initials come from the EDA and the MAP.

The initials that most often decide whether a run converges at all are the
distribution and absorption parameters — `Q`, the peripheral volume, and
`KA`. Derive them from the observed profile shape.

## Building-block appendix

Canonical snippets for the two things that change most often between
candidates. These are code to be adapted in place, in the same sense as the
templates: copy the form, choose the initials from the analysis.

### Interindividual variability

**Diagonal, one ETA per parameter** — the default, and the only form in the
starter templates. Independent IIV, no covariance estimated. Add IIV one
parameter at a time; each addition is its own run with its own hypothesis.

```
$PK
MU_1 = THETA(1)
MU_2 = THETA(2)
CL   = EXP(MU_1 + ETA(1))
V    = EXP(MU_2 + ETA(2))

$OMEGA
0.09       ; OMEGA(1,1) IIV on CL, variance on the log scale
0.09       ; OMEGA(2,2) IIV on V
```

Applies when: starting out, or when the ETA pairs plot shows no structure.
Watch for: high shrinkage on an ETA means the data do not inform it — the
candidate that drops it is a legitimate simplification, run and recorded as
one, not assumed.

**Block(2), correlated ETAs** — estimates the covariance between two ETAs.
The `$PK` block is unchanged; only `$OMEGA` differs. The three values are the
lower triangle in row order: var(1), cov(1,2), var(2).

```
$OMEGA BLOCK(2)
0.09              ; OMEGA(1,1) IIV on CL
0.045  0.09       ; OMEGA(2,1) covariance, OMEGA(2,2) IIV on V
```

Applies when: `eta_pairs()` shows a clear CL/V (or CL/V1) correlation, and
the diagonal model has already been fitted so the block is a testable
one-step extension of it. Watch for: an off-diagonal initial must keep the
matrix positive-definite — a correlation of about 0.5 (`cov = 0.5 *
sqrt(var1*var2)`) is the conventional safe start. A block that will not
converge is a finding about identifiability, reported as one; it is not a
reason to keep re-running with nudged initials.

**Mu-referenced form** — the form every template already uses, restated here
because it is what makes IIV extension mechanical. `MU_n` is a function of
THETAs and covariates **only** — never of an ETA — and `MU_n` pairs with
`ETA(n)` **by index**. Order the THETAs so the parameters carrying IIV come
first and the indices line up without a gap.

```
$PK
; BASELINE covariates go INSIDE the MU expression, never after it
MU_1 = THETA(1) + THETA(3)*LOG(WT/70)
CL   = EXP(MU_1 + ETA(1))
V    = EXP(THETA(2))
```

Applies when: always, in this library. It is free under FOCE and it is what
lets an EM or Bayesian estimator be switched on later without restructuring
the `$PK` block. Watch for: writing `CL = EXP(MU_1) * EXP(ETA(1))` or
appending a baseline covariate outside the MU term breaks the mu-reference —
NONMEM does not necessarily complain, it just stops being mu-referenced.

The one exception, and it is a silent-wrong-answer trap: a **time-varying**
covariate must stay **outside** the MU expression
(`CL = WTNOW/70 * EXP(MU_1 + ETA(1))`). `MU_n` must be constant within an
individual; NONMEM handles a time-varying quantity inside `MU_n` by silently
averaging it across the subject's records (Co-Modeler consult, 2026-09-01).

### Residual error

**Proportional** — the form in all four templates.

```
$ERROR
IPRED = F
Y     = F*(1 + EPS(1))

$SIGMA
0.04       ; SIGMA(1,1) proportional error variance; sqrt = the approximate CV
```

Applies when: concentration data with roughly constant CV across the
observed range — the usual case for PK above the quantification limit.
Watch for: the model cannot describe scatter at concentrations near zero,
because the error shrinks with `F`. Pre-dose zeros and BLQ-as-zero rows make
this worse; handle them at data assembly, not here.

**Additive** — constant standard deviation on the observation scale.

```
$ERROR
IPRED = F
Y     = F + EPS(1)

$SIGMA
0.04       ; SIGMA(1,1) additive error variance, in DV units squared
```

Applies when: the data are on a log scale (where an additive error is a
proportional error on the natural scale), or for a response whose noise
genuinely does not scale with the level. Watch for: on a natural-scale
concentration, an additive-only error lets `Y` go negative and fits the high
concentrations at the expense of everything else.

**Combined** — additive plus proportional; the general form.

```
$ERROR
IPRED = F
W     = SQRT(THETA(4)**2 + (THETA(5)*IPRED)**2)   ; SD, one THETA per part
Y     = IPRED + W*EPS(1)

$SIGMA
1 FIX      ; SIGMA(1,1) fixed to 1; the two error THETAs carry the magnitudes
```

Applies when: the residual plots show proportional scatter at high
concentrations and a floor of constant scatter near the limit of
quantification — the common case across a wide concentration range. Watch
for: the additive part is frequently unidentifiable when there are few low
observations; it then drifts toward zero with an enormous RSE, and dropping
back to proportional is the honest result. When `W` can reach 0 (both error
THETAs free and IPRED 0), guard the `IWRES = (DV-IPRED)/W` division with the
`DEL` idiom (`W = SQRT(... ) + DEL` with `DEL` a tiny constant, or
conditionally) — Co-Modeler consult, 2026-09-01.

This `W`/`$SIGMA 1 FIX` form is the HOUSE DEFAULT (SME decision 4 below):
both error components are THETAs, so they appear in the parameter table with
RSEs, and only one EPS enters the model. The two-EPS form
(`Y = F*(1+EPS(1)) + EPS(2)`) is shown for recognition when reading external
control streams, not for use; the two forms are not comparable line by line
in a parameter table.

## Validation status

Per-template status lives in `MANIFEST.yaml`, schema v2, one unit per
template, keyed by the file stem. Each unit records the template's
`sha256`, so an edited template cannot keep an old approval.

- Every unit starts `status: draft`. Root `reviewed_by` is empty: the skill
  is unsigned.
- The SME flips a unit to `approved` after a **validation run for that
  structure** — the template adapted to a real dataset, submitted, and its
  output reviewed. Approval is per structure; approving `onecmt_iv` says
  nothing about `twocmt_oral`.
- A `draft` template may still be adapted and run. What changes with status
  is what the gate package says: a candidate built from a `draft` template
  is reported as adapted from an unvalidated baseline, alongside the novel
  structure flag if that also applies.
- Regenerating the manifest after editing a template resets that unit to
  `draft` and clears any root signature. That is the intended behaviour, not
  a fault: an edited baseline is a new baseline.

Because these units are control-stream templates rather than R functions,
the `entrypoints` field of each unit carries the template **file name** — the
adopted artifact is the stream itself, and there is no callable symbol. Each
unit's `reason` field states this. The schema v2 field is a list of names and
does not require R symbols, so no change to `tools/method_manifest.R` was
needed; note however that the runtime self-gate
`require_validated_method()` is written for R reference code and is not the
mechanism that enforces template status. Enforcement here is the gate
package: the modeler reports each candidate's template and that template's
manifest status to the SME.

### Preparing a template for local review

Choose the template whose structural and administration assumptions fit the
analysis plan. Record its unit name, starting revision, and methodological
changes when adapting it. Review the control stream, input mapping, initial
values, error model, and fit diagnostics for the intended analysis. Keep the
adaptation review and resulting gate as project records. An unsigned public
template is a draft starting point. Local approval applies to the declared
structure and bytes under review.
