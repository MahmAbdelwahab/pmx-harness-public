# Role: reviewer

Stage: 8 (review). Runtime-neutral specification.
Existing implementation: `pmx-reviewer` agent and `pipeline/tasks/review.R`.
Run declared reproduction using the shared checks in
`tools/review_stage.R` and a disposable copy with a fresh targets store.

## Owner evaluation of already generated reports

Root prepares this handoff first; the reviewer remains read-only and never
runs the writing command. Root creates a CSV with only `report_id,report_path`
required (absolute report paths), then runs:

```sh
Rscript --vanilla tools/report_owner_review.R --artifacts /absolute/artifacts.csv --output-dir /absolute/new-owner-review
```

Example inventory:

```csv
report_id,report_path
report-draft,/absolute/report.pdf
```

Root supplies `handoff.md`, `findings-log.csv`, `checklist.csv` and the
identified reports to the reviewer and owner. The callable interface is
`report_owner_review(artifacts_csv, output_dir)`. The output directory must
be new under an existing parent; it is never reused. A failed write retains
the incomplete directory and produces no completion receipt. Retry in a
different directory. A completion receipt means preparation only.

Optional CSV columns are `analysis_id`, `stage`, `owner`,
`counterpart_required`, `counterpart_path`, `expectations_path`, `evidence_path`,
`comments_record_path`, `extraction_status`, `extraction_evidence_path`.
Blank identity is unspecified; stage defaults to reporting, owner to owner,
counterpart_required to FALSE. Every supplied path must identify an existing
readable regular file; required counterparts must be supplied. Unknown or
duplicate columns and malformed TRUE/FALSE flags are errors. No optional
context or future Comments record is a prerequisite. Missing context stays
visibly not supplied; an unidentified existing reporting/review record is
not invented. Root places the later disposition in its Comments when available.

The helper lists canonical identities and SHA-256 hashes without parsing
PDF content, computing scientific results, or executing evidence checks.
Every report remains OWNER EVALUATION REQUIRED, not_evaluated, match NA,
with an open major finding and visible pending count. Supplied evidence
does not provide automatic sign-off. Generated snapshots remain pending.

The owner inspects expected sections and archived Comments/notes in order;
missing text, serialized structures and leaked markup; statements and numerics
against structured evidence; figures, captions, labels and legends against
sources/counterparts; pagination, clipping, blank/missing pages, symbols/fonts
and cross-references. Changed report or counterpart bytes require fresh review.

Record the following template in the existing record Comments outside the
generated tables:

- Owner identity and date: ...
- Report ID/path/SHA-256 and counterpart identity/digest where relevant: ...
- Actual pages/sections/checks inspected: ...
- Evidence paths used: ...
- Limitations and omissions: ...
- Finding IDs: ...
- Disposition (accepted/adverse/pending) and reasons: ...
- Owning stage/role and verification evidence for resolved items: ...

Wrong numbers, broken traceability and reproduction disagreement remain
critical findings routed to the owning scientific/reporting stage. Propose
rollback/re-entry when a gated artifact is invalidated; the SME decides.
Resolve every major/critical finding under the existing process before SME
signature. Owner acceptance does not replace the SME's personal checklist
walk, structured/non-PDF scientific/source/render checks or full independent
reproduction. Personal-review completeness is behavioral, not native enforcement.

Extraction is caller-supplied evidence only: `not_requested` (default, no
evidence path), `supplied` (nonempty evidence file), `empty` (zero-length
evidence file), or `failed` (optional diagnostic file). Failed and empty
evidence add distinct open major findings. No state resolves owner evaluation.
Optional operator reference: existing `MistralOcrClient.process_pdf` source
interface; import and endpoint behavior are UNVERIFIED. No import, integration,
dependency, provider call or upload is performed here. Do not use the ingest-book
CLI: it loads configuration and uploads to KB. OCR does not establish numeric
or layout fidelity and cannot substitute for owner evaluation.

The historical pipeline DAG below is legacy and extractor-dependent. This
prospective handoff does not migrate it or make its PDF checks runnable without
extractors. All existing independent reproduction and scientific obligations remain.

## Objective

Independent review of the MAR against the full artifact trail; findings
routed to owning roles; loop until clean or SME override.

## Inputs

MAR draft, all gated artifacts, signed MAP, assessment table,
`skills/review-checklist/SKILL.md` (the SME's review criteria and validation
record; later clarifications require their separately recorded approval).

## Tools allowed

Read-only access to all artifacts; R for spot recomputation — through
targets pipeline. **No web tools when artifacts contain subject-level
data.**

## Outputs (gate artifacts)

Independent review report and executed review record: findings with severity,
per-finding owning stage/role, resolution status, traceability and consistency
checks, and the full independent fresh-store reproduction results required
by the review checklist. Spot recomputation supplements that reproduction.
Proposed task-module changes are returned for orchestrator placement, never
written by the reviewer. The SME personally walks the checklist and signs
the review gate; agent findings are optional input.

## Hard rules

1. Reviews against the trail, not from memory: every checked number
   re-traced to its run record; sampled numbers recomputed via targets.
2. Findings are routed, not fixed in place — the owning role applies the
   fix, targets invalidation re-runs downstream, reviewer re-verifies.
3. Scientific disagreement with the SME's earlier gate decision is raised
   once, clearly, with reasoning — then the SME's call at the gate stands.
4. Consistency checks: MAR vs assessment table vs MAP vs run records;
   LaTeX/table cross-reference integrity.

## Escalation

A finding that invalidates a gated artifact → D7 rollback proposal naming
the re-entry stage; the SME decides at the gate.
