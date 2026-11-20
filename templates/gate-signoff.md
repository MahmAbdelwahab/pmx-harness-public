# Gate Sign-off Record

One record per lifecycle stage per analysis. The Layer 2 tool wrappers refuse
to run stage N+1 work while this record for stage N lacks the SME approval
block. This record set, together with the versioned artifacts it references,
constitutes the human-in-the-loop audit trail (supports M15 Section 3
traceability and Section 4.3 documentation expectations).

Analysis ID: <!-- -->
Stage: <!-- analysis-setup | data-assembly | eda | base-model |
covariate-model | qualification | simulation | reporting | review -->
Date opened: <!-- -->

## Artifacts presented for review

<!-- Paths + content hashes of every artifact this stage produced. Numbers in
artifacts must originate from executed tool runs; list the run records. -->

| Artifact | Path | Hash / run ID |
|---|---|---|
| | | |

## Agent summary

<!-- Written by the producing agent: what was done, what the acceptance
criteria were, whether they were met, any open concerns or escalations. An
agent that could not meet a criterion states so here — it never relaxes the
criterion. -->

## SME decision

- [ ] APPROVED — stage complete, next stage may start
- [ ] APPROVED WITH CHANGES — changes listed below were applied and re-verified
- [ ] REJECTED — returned to stage with instructions below

Comments / required changes:

<!-- -->

Signed: <!-- SME name -->
Date: <!-- -->
Digest signed: <!-- paste the Package-Digest line's sha256:<hex> here when signing -->

<!-- The wrapper checks for the literal line "Signed:" followed by a non-empty
name and an APPROVED checkbox marked [x]. It ALSO binds the signature to the
package: paste the "Package-Digest: sha256:<hex>" value from the generated
block above into "Digest signed:" — that paste is what proves your approval
covers this exact package. If the package is regenerated the digest changes and
a new signature is required. Do not alter these markers.

RECORDING STEP (approval ledger): after you sign by hand, the recording of that
signature into the append-only ledger gates/<ID>/approvals.jsonl is done by

    Rscript tools/approve_gate.R approve gates/<ID>/<stage>.md

which never edits this markdown — it verifies the content checks pass, extracts
the signer / date / decision FROM this record, and appends one chained event.
To withdraw an approval, an SME runs

    Rscript tools/approve_gate.R revoke gates/<ID>/<stage>.md "<reason>"

and require_gate then refuses the stage until a new approval supersedes it. A
signed record with no ledger event yet still passes with a one-line warning
(transitional); CI inventories those. -->
