$PROBLEM REFERENCE TEMPLATE twocmt_iv - 2-compartment IV, ADVAN3 TRANS4
; ===========================================================================
; REFERENCE CONTROL-STREAM TEMPLATE - skills/reference-models
; ---------------------------------------------------------------------------
; Structure:   two compartments, intravenous input (bolus, or infusion
;              through a RATE column), first-order elimination from the
;              central compartment. Parameterized as CL, V1, Q and V2
;              (TRANS4). Compartment 1 is central, compartment 2 peripheral.
; Status:      see skills/reference-models/MANIFEST.yaml, unit `twocmt_iv`.
;              A template is `draft` until the SME approves it after a
;              validation run. A draft template may still be adapted; the
;              base-model gate package then states that status.
; Not a model: every THETA, OMEGA and SIGMA initial below is a TEMPLATE
;              PLACEHOLDER - a round, neutral number that only makes the
;              stream complete enough to read. None of them means anything
;              for an analysis; replace each with an initial taken from the
;              EDA and the MAP of the analysis at hand.
;
; ADAPTATION CONTRACT
;   CHANGE: the problem line (run id and description), the column list of
;           the input record, the path and IGNORE list of the data record,
;           every THETA, OMEGA and SIGMA initial, the table file name, and
;           the run-record header block below.
;   KEEP:   the subroutine choice, the mu-referenced form of the PK block,
;           the scaling statement, the estimation and covariance records,
;           and the leading NUM column of the table.
;   RECORD: run `bbr::model_diff(parent, child)` against this template
;           before the first submission, so the run record carries the
;           delta from the baseline. When this candidate extends a fitted
;           one-compartment run, base it on that run and diff against it:
;           once a parent run exists, lineage follows the model family,
;           not the template.
;
; RUN-RECORD HEADER - fill before submission (skills/model-management)
;   Run:        <run id>
;   Based on:   skills/reference-models/reference/twocmt_iv.ctl (template)
;   Tags:       <from the project TAGS glossary>
;   Hypothesis: <what this run tests>
;   Expected:   <what you expect to see, before you see it>
; ===========================================================================

; --- data ------------------------------------------------------------------
; ADAPT-HERE. The input record has to come before the data record whenever
; the IGNORE list names a column label (NM-TRAN error 679). The column list
; is a placeholder: list the gated dataset's columns in file order. C is the
; harness comment flag ('.' on kept rows, 'C' on commented rows); NUM is the
; row key that bbr::nm_join() needs.
$INPUT C NUM ID TIME EVID AMT DV MDV

; ADAPT-HERE. Point at the gated analysis-ready file. bbi resolves a
; relative path from the run subdirectory and PsN from this file's
; directory, so use an absolute path when both lanes read the file.
$DATA ../analysis-ready.csv IGNORE=@ IGNORE=(C.EQ.'C')

; --- structure -------------------------------------------------------------
; KEEP. ADVAN3 is two compartments with IV input. TRANS4 parameterizes it
; as CL, V1, Q and V2 (clearances and volumes) instead of the TRANS1
; micro-constants; the clearance form is what the covariate stage and the
; report both need, and it is far better conditioned.
$SUBROUTINES ADVAN3 TRANS4

$PK
; KEEP the mu-referenced form. MU_n depends on THETAs and covariates only,
; never on an ETA, and MU_n pairs with ETA(n) by index. The THETA that
; carries IIV therefore comes FIRST, so the MU and ETA indices line up with
; no gap. THETAs sit on the log scale, so every parameter is positive by
; construction.
;
; ADAPT-HERE for covariates: put a BASELINE covariate term INSIDE the MU_n
; expression, for example MU_1 = THETA(1) + THETA(5)*LOG(WT/70), never
; after it. A TIME-VARYING covariate stays OUTSIDE the MU expression (for
; example CL = WTNOW/70*EXP(MU_1 + ETA(1))), because NONMEM silently
; averages a time-varying quantity inside MU_n over each subject's records
MU_1 = THETA(1)
CL   = EXP(MU_1 + ETA(1))
V1   = EXP(THETA(2))
Q    = EXP(THETA(3))
V2   = EXP(THETA(4))

; KEEP. Central is compartment 1 under ADVAN3, so S1 is scaled by the
; central volume V1 and the prediction is a concentration. Check that the
; dataset's amount and concentration units agree; a mismatch is silent.
S1   = V1

; KEEP. ETA aliases for the table: NM-TRAN rejects an undefined name.
ET1  = ETA(1)

$ERROR
; Proportional residual error. The SKILL.md building-block appendix gives
; the additive and combined forms and when each applies.
IPRED = F
Y     = F*(1 + EPS(1))

; --- parameters ------------------------------------------------------------
; ADAPT-HERE, every line. PLACEHOLDER INITIALS with no analysis meaning.
; Order: the mu-referenced parameter first (CL), then the rest.
; A point of method, not of numbers: the distribution parameters of a
; two-compartment model are the ones that fail to converge from poor
; starting values. Derive the Q and V2 initials from the observed
; distribution phase instead of carrying these placeholders forward.
$THETA
(2.30)     ; 1 log CL  [log L/h]  PLACEHOLDER, exp = 10
(3.00)     ; 2 log V1  [log L]    PLACEHOLDER, exp = 20
(1.61)     ; 3 log Q   [log L/h]  PLACEHOLDER, exp = 5
(3.91)     ; 4 log V2  [log L]    PLACEHOLDER, exp = 50

; Diagonal IIV on CL only, the conservative start. A two-compartment base
; model rarely supports IIV on all four parameters at the first fit; add IIV
; one parameter at a time, each addition justified in its run record. The
; SKILL.md appendix gives the block(2) form, the usual next step for a
; correlated CL and V1 pair.
$OMEGA
0.09       ; OMEGA(1,1) IIV on CL, log-scale variance  PLACEHOLDER

$SIGMA
0.04       ; SIGMA(1,1) proportional error variance  PLACEHOLDER

; --- estimation ------------------------------------------------------------
; KEEP. FOCE with interaction is the house default for continuous PK with a
; proportional or combined error. NOABORT lets a run that meets a numerical
; problem finish and report instead of stopping without a report.
$ESTIMATION METHOD=1 INTERACTION MAXEVAL=9999 PRINT=5 NOABORT

; KEEP. Without the covariance step there are no standard errors and no
; RSEs, and the base-model gate package cannot be assembled. A
; two-compartment model that minimizes but fails the covariance step is an
; observation to report, not a run to retry quietly.
$COVARIANCE

; KEEP the leading NUM column: it is the join key for bbr::nm_join(), which
; brings the input columns (ID, TIME, DV, covariates) back next to these
; computed ones. Do not table DV or other input columns here; nm_join would
; find them twice and rename them.
; ADAPT-HERE: the file name only.
$TABLE NUM PRED IPRED CWRES ET1 NOPRINT ONEHEADER FILE=ADAPT-HERE-tab
