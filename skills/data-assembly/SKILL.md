# Skill: data-assembly

Status: skills/data-assembly/MANIFEST.yaml is the authority for the SME
signature and for per-unit status and digests. This header carries no status
claim of its own — read the manifest. (Guidance derived
from Metrum MeRGE Expo 1 (NONMEM FOCE) data-prep practice; pending SME review
and amendment.)
Source: https://merge.metrumrg.com/expo/expo1-nonmem-foce/posts/data-prep.html

Used by: data-assembler role (Stage 1). Every step below runs as a target in
the analysis {targets} pipeline; the agent drafts the derivation script, the
pipeline executes it, the QC battery gates the result.

## Toolchain

| Package | Role |
|---|---|
| `yspec` | Dataset specification (YAML): columns, types, units, decodes, ranges. `ys_load()`, `ys_check()`, `ys_document()` |
| `mrgda` | Source directory reading, ID assignment, derived-data writing. `read_src_dir()`, `assign_id()`, `write_derived()` |
| `lastdose` | Derives TIME (TAFD) and TAD from dose events |
| `tidyverse`/`lubridate`/`tidyr` | Wrangling, datetime parsing, LOCF (`fill()`) |
| `mrgmisc` | `pool()` — variable-set comparison data vs spec |

## Canonical workflow

1. Load source domains (SDTM-style: `dm`, `lb`, `ex`, `pc`) with
   `read_src_dir()`.
2. Load the dataset spec (`data/derived/<analysis>-spec.yml`) with
   `ys_load()`. Include the specification in the analysis plan and obtain
   local review before assembly.
3. Organize derivations as two lists: subject-level (`sl`) and time-varying
   (`tv`).
4. Demographics: filter screen failures; encode categoricals per spec decodes.
5. Labs: filter to baseline; pivot test codes wide.
6. Dosing (`ex`): filter to relevant treatments; assign EVID/DVID; extract
   DATETIME.
7. Observations (`pc`): filter to analyte; flag BLQ; extract concentrations.
8. Join subject-level onto time-varying; `bind_rows()` doses + observations;
   sort by subject and datetime.
9. Derive TIME (TAFD) and TAD via `lastdose()`.
10. `assign_id()` — numeric IDs from USUBJID; reconcile reported subject
    count against expected enrollment.
11. Add NONMEM housekeeping: `MDV` (1 when DV missing or dose record), `C`
    (exclusion flag — flag, never delete), `NUM` (sequential row number).
12. `ys_check()` against the spec.
13. `write_derived()` to CSV; `ys_document(type = "regulatory")` for the
    define document.

## Dataset conventions

- Column names ≤ 8 characters (spec-enforced); short labels ≤ 40 characters.
- Required NONMEM columns: `ID, TIME, EVID, DVID, AMT, DV, MDV, NUM, C`.
- `TIME` = hours after first dose; `TAD` = time after most recent dose.
- Exclusions via `C` flag only — records are never deleted (M15: exclusion
  rationale documented; the `C` flag plus define document carry it).
- `NUM` unique and sequential — row-level traceability.
- Missing values encoded as `"."` in the written CSV.

## QC battery (deterministic CHECKs; every item must pass or be
SME-waived at the data-assembly gate)

1. Spec loads clean (`ys_load()` naming/label limits).
2. Subject count from `assign_id()` equals expected enrollment (from MAP).
3. `mrgmisc::pool()` — no variables present in data but absent from spec, or
   vice versa.
4. `ys_check()` passes: column presence and order; discrete values only from
   declared levels; continuous values within spec ranges.
5. `MDV` = 1 on all dose records and all missing-DV records.
6. `BLQ` flag consistent with declared LLOQ; %BLQ per analyte/occasion
   reported (feeds the BLQ-handling decision recorded in the MAP).
7. `C` flag review: all exclusions flagged with reasons, none deleted.
8. `NUM` sequential and unique.
9. TIME non-decreasing within ID; no observations before first dose unless
   pre-dose sampling is expected; TAD ≥ 0.
10. Unit consistency: dose, concentration, time units as declared in spec;
    any molar↔mass conversion logged with the molecular weight used.
11. Source reconciliation: record counts per domain traced to source files.

## Outputs (artifacts to the data-assembly gate)

- Analysis-ready dataset CSV + yspec YAML spec
- Regulatory define document (`ys_document`)
- Assembly script (target definition)
- QC battery report (pass/fail per item)

## SME amendments

<!-- SME: add practice-specific conventions here — covariate imputation
rules, LOCF policy, multiple-analyte handling, study-pooling conventions,
anything MeRGE leaves open. -->
