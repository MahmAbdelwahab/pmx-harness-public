# Model Analysis Plan (MAP)

Per ICH M15 Section 4.1 and Appendix 2 (Final, Step 4, 29 Jan 2026).

**This MAP must be SME-approved before the data-assembly stage begins.**
Use it to document the analysis before data access (M15 Section 4.1).
- Planned model evaluation activities and technical criteria must be described
  here (M15 Section 3 and 4.1).
- This MAP is appended to the associated MAR (M15 Section 4.2). Deviations are
  justified and documented in the MAR.

Analysis ID: <!-- matches assessment table -->
Version: <!-- -->
Prepared by: <!-- -->
SME approval: <!-- name, date — REQUIRED BEFORE DATA ACCESS -->

## 1. Introduction

<!-- Rationale for the analysis; background information and knowledge;
pre-existing analyses with references. Cross-reference the assessment table
(question of interest, context of use, model risk rating). -->

## 2. Objectives

<!-- Objectives including the intended application of the model. Align with
the question(s) of interest or state the broader perspective. -->

## 3. Data

<!-- Data sources; inclusion/exclusion criteria and rationale; relevant design
features of contributing studies; planned data selection, transformations, and
imputations with justification. Record the corresponding decisions and
any deviations in the MAR (M15 Section 3). -->

## 4. Methods

### 4.1 Modeling and simulation methods
<!-- M&S methods, computational platforms and versions, model development
strategy (sequence of development, numerical methods), key assumptions with
justification and alternatives considered. -->

### 4.2 Planned model evaluation
<!-- Verification, validation, and applicability assessment activities, scaled
to the declared model risk. Name the diagnostics (GOF, VPC, bootstrap,
sensitivity analyses, external validation if risk demands it). -->

### 4.3 Technical criteria
<!-- Detailed, pre-specified acceptance criteria for model evaluation and
model outcomes, commensurate with model risk (M15 2.2.1). These are the
criteria agents check against and the SME signs off on. The assessment
table carries the key criteria in summary; this section carries the full
list. See skills/midd-framing for the criteria structure. For the ICH training
example, see Training Module 1 (slides 22–24).

4.3.1 Rationale
  Which factor the answer is most sensitive to, and therefore where the
  criteria concentrate (strata, extremes, the parameter the decision rule
  turns on). One paragraph, traceable to the question and context of use.

4.3.2 Criteria for model evaluation
  Question-specific criteria on the model's description and prediction of
  the data in the region the question depends on. Typically: stratified
  predictive-check coverage across the sensitive covariate; absence of
  systematic residual trend across that covariate; precision bounds
  (RSE) on the parameters the answer turns on; adequate characterization
  of uncertainty for any planned simulation. Each criterion states its
  metric, bound, and the executed check that computes it. The general
  qualification battery (GOF, VPC, NPDE, bootstrap; depth by model risk,
  tools/qualification.R) is declared in Section 4.2 and referenced here,
  not restated.

4.3.3 Criteria for model-outcomes evaluation
  (a) Representativeness: the simulated or analysed population must be
      representative of the target population, with sufficient
      representation at the extremes that matter (state the covariate,
      the range, and the minimum representation).
  (b) Outcome acceptance, one statement per metric and scenario:
      Identify the metric, scenario, strata, percentile, clinical target
      range and permitted margin. For a decision rule on an analysed
      quantity, give the metric, computation and acceptance band.
  P and M are case-specific: derived from the clinical exposure–response
  evidence for this compound and question, proposed by the strategist
  with that evidence cited, decided by the SME at signature. They
  tighten as model risk rises (higher P into the tail, smaller M,
  additional criteria at high risk). No house default exists and none
  may be assumed (ICH TM1 footnote 7). Once signed, margins may be
  tightened, never loosened silently.

4.3.4 Disposition of unmet criteria
  A criterion not fully met does not by itself invalidate the model (ICH
  TM1 slide 27 note). State here, before data access, how an unmet
  criterion will be handled: SME disposition at the owning gate as an
  accepted limitation (with justification recorded), a criterion change
  (recorded as a deviation and justified in the MAR), or rejection. -->

### 4.4 Prediction and simulation
<!-- If relevant: planned simulation methods and scenarios, including how
parameter and assumption uncertainty is accounted for.
House checklist (skills/simulation-standards, "What the analysis plan
must declare") — every item below, or "not applicable" with reason:
1. Fitted model being translated (run id) and the translation tolerance
   (house default: relative 1e-3).
2. Simulation engine and version (house: mrgsolve).
3. Scenarios, each NAMED — regimen (amt, cmt, ii, addl, rate),
   population (n, covariate source), sampling grid, what is reported.
   Names appear verbatim in outputs and the gate package.
4. Uncertainty tier per scenario (none / iiv / full; house default
   scales with the model risk rating).
5. Exposure metrics with exact definitions (AUC interval, Cmax,
   Ctrough at steady state, ...), each computed by declared code.
6. Replicates/subjects per scenario (house defaults: typical n = 1,
   IIV 200, exposure distributions 500 — may be raised here, never
   silently lowered).
7. Deliverables: which figures and tables the gate package carries.
Extrapolation beyond the fitted dose range, duration, population, or
formulation requires explicit SME approval recorded here or in
Section 6 (Amendments) BEFORE the run. -->

### 4.5 Covariate analysis
<!-- If relevant. The physiological prior list is FROZEN here, before
data access (skills/covariate-priors: no prior list, no search — the
scm test_relations section is generated verbatim from this list).
Declare: the prior list (covariate, parameter, functional form, and the
physiological rationale per relation); the search method (SCM, FREM, or
prior-driven full model); retention criteria (house default: forward
p 0.05, backward p 0.01, statistical significance only); the reference
values/categories. Exploratory data-driven additions are labelled as
such and require SME justification at the gate. -->

## 5. Execution declarations

<!-- Harness-required declarations with no other pre-specified home.
Each has a house default (recorded in the named skill); state the value
or the override WITH justification.
- Estimation engine and version; run budget (hard stop — exhaustion is
  an escalation, never a quiet increase).
- Simulation engine and version (skills/simulation-standards).
- THE analysis seed (one per analysis; every output record echoes it).
- Diagnostic replicate counts (house: VPC >= 200, bootstrap 500/1000 by
  model risk, NPDE nsim >= 500; may be raised here, never lowered).
- Parameter-uncertainty draws if the full tier is declared (house
  default n_draws = 200).
- Any house-default override, each with justification — an undeclared
  override is a deviation. -->

## 6. Amendments

<!-- Dated log; empty at first approval. Each entry: date, what
changed, why, SME approval (name, date). Extrapolation approvals
(Section 4.4) land here when they arise after initial MAP signature.
Analysis-affecting changes are additionally justified and documented in
the MAR (M15 Section 4.2); this log is the plan-side record. -->
