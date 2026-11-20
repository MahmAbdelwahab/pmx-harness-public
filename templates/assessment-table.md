# MIDD Evidence Assessment Table

Per ICH M15 Appendix 1 (Final, Step 4, 29 Jan 2026). One table per question of
interest (M15 Appendix 1, footnote 1). Keep entries concise; details belong in
the MAP, MAR, or regulatory interaction background materials (M15 p. 15).
See ICH M15 Training Module 1 for a worked example. Replace the drafting
prompts with your analysis entries.

Analysis ID: <!-- e.g. SYNTHETIC-EXAMPLE -->
Compound / project: <!-- -->
Prepared by: <!-- agent role + version -->
SME approval: <!-- name, date — required before data access for planning rows -->

## Section A — Key Assessment Elements
(required at planning AND submission stages)

| Item | Definition (M15) | Entry |
|---|---|---|
| Question of Interest | The question the evidence will address. Its scope can extend beyond the model's own use (M15 2.1.1). | <!-- A clinical/regulatory decision question or the house model-shaped form; either way it must name the decision it informs. The model's own role is scoped in Context of Use, not here. --> |
| Context of Use | How and where the model will be applied. Identify the model, development data and other evidence relevant to the question (M15 2.1.2). | **Model and data used:** <br>**Role and scope of the model:** <br>**Additional data or evidence informing the question:** <!-- state numerical target ranges once determined (ICH TM1 fn 5); if nothing else contributes, say so --> |
| Model Influence | How strongly the decision relies on model results in light of the other evidence. Justify a low, medium or high rating (M15 2.1.3). | Rating: <br>Justification: <!-- relative contribution of model vs non-model evidence; sole source = high --> |
| Consequence of Wrong Decision | Potential harm if the decision is wrong. Consider both the likelihood and severity of harm when assigning a low, medium or high rating (M15 2.1.4). | Rating: <br>Justification: <br>— A wrong decision here means: <br>— Efficacy consequence (likelihood, severity): [low/medium/high] <br>— Safety consequence (likelihood, severity): [low/medium/high] <br>— Other domain engaged, if any: <br>— Overall (combined by likelihood x severity, driven by the worst domain): |
| Model Risk | The risk associated with relying on the model. Use model influence and the consequence of a wrong decision to justify a low, medium or high rating (M15 2.1.5). | Rating: <br>Justification: <!-- state both inputs and the matrix cell; name the downstream consequences of this rating (documentation depth, battery, outcome-criterion margins) --> |
| Model Impact | How far the proposed strategy departs from applicable standards or expectations for the question. Justify a low, medium or high rating (M15 2.1.6). | Rating: <br>Justification: <!-- PLANNING-stage rating: name the regulatory standard or expectation for this question and state how far the proposed strategy departs from it. Not the close-out influence reconciliation statement, which lives in the Outcome row. --> |

## Section B — Additional Considerations

### Planning stage (also provided at submission stage)

| Item | Definition (M15) | Entry |
|---|---|---|
| Technical Criteria | The checks most relevant to accepting the model and its results for this question. Match the criteria to the model risk and record the detailed plan in the MAP (M15 2.2.1). | **Rationale:** <!-- which factor the answer is most sensitive to; where the criteria concentrate --> <br>**Criteria for model evaluation:** <!-- adequate description and prediction in the region the question depends on: central tendency, variability, no systematic bias across the relevant strata, uncertainty adequate for any planned simulation; MAP Section 4.3 holds the full list --> <br>**Criteria for model-outcomes evaluation:** <!-- (a) simulated/analysed population representative of the target population with sufficient representation at the extremes; (b) state the metric, scenario, strata, percentile, clinical target range and permitted margin — or the pre-specified decision rule where there is no simulation. P and M are case-specific, derived from the clinical exposure–response evidence, and tighten as model risk rises; no house default exists (ICH TM1 fn 7). --> |
| Appropriateness of Proposed MIDD | Explain why the approach can answer the question and how the selected checks support that conclusion (M15 2.2.2). | **Stage: planning / submission** <!-- delete one. At PLANNING: principle-based — why this method class answers this question, how the two criterion families secure the answer given the model risk, and why the approach is scientifically sufficient given the model impact. At SUBMISSION (re-drafted at close-out; planning text retained in the amendments log): results-based — a brief statement that the criteria were satisfied, pointing to the Evaluation and Outcome rows for the critical arguments (ICH TM1 slide 25 note). --> |

### Submission stage

| Item | Definition (M15) | Entry |
|---|---|---|
| Evaluation of Model(s) and Model Outcomes | Summarize the evaluation results against the declared criteria (M15 2.2.3). | **Summary:** <!-- one line: predefined criteria for model performance and for outcomes satisfied or not; any issue limiting utility --> <br>**Evaluation of model(s):** <!-- results against the model-evaluation criteria, with the numerical value against each bound (ICH TM1 fn 8 encourages numbers; in this harness they are rendered from executed code, never typed) --> <br>**Evaluation of model outcomes:** <!-- representativeness of the simulated/analysed population; realised percentile vs margin, or decision-rule value vs band; criteria not fully met carry their SME disposition --> |
| Outcome of the MIDD Evidence Assessment | Record the team's conclusion on the use of the results as evidence, taking the full assessment into account (M15 2.2.4). | <!-- Conclusion integrating model risk and model impact with the met criteria and the supporting non-model evidence. Includes the mandatory close-out influence reconciliation statement: which decision the model actually informed, whether actual influence matched the planned rating, and any divergence. --> |

## Cross-references
(M15 4.3: MAPs and MARs should be cross-referenced within the assessment table)

- MAP: <!-- path/ID -->
- MAR: <!-- path/ID -->
- Gate records: <!-- gates/... -->

## Amendments log

Record changes to assessment-table entries here. At close-out, before
replacing the Appropriateness of Proposed MIDD planning-stage entry with
the submission-stage entry, retain the planning-stage text verbatim in
this log. The current row contains the submission-stage text; the retained
planning text remains in this log, not as a second paragraph in that row.
Link each amendment to the relevant gate record; the SME decides and signs
at the gate as required by the midd-framing skill.

### Amendment: <!-- date and affected entry -->

- Stage transition or reason for change: <!-- -->
- Retained earlier text: <!-- paste the earlier entry verbatim -->
- Change made: <!-- concise description; the current entry remains in the table -->
- Gate record: <!-- path/ID -->
