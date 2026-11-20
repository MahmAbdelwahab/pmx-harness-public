$PROBLEM REFERENCE TEMPLATE twocmt_oral - 2-cmt first-order abs, ADVAN4 TRANS4
; ===========================================================================
; REFERENCE CONTROL-STREAM TEMPLATE - skills/reference-models
; ---------------------------------------------------------------------------
; Structure:   two disposition compartments fed by a depot through
;              first-order absorption, first-order elimination from the
;              central compartment. Parameterized as KA, CL, V2, Q and V3
;              (TRANS4). Compartment 1 is the depot, 2 is central and 3 is
;              peripheral. Under ADVAN4 the CENTRAL volume is named V2 and
;              the peripheral volume V3, the most common source of confusion
;              when a model moves between ADVAN3 and ADVAN4.
; Status:      see skills/reference-models/MANIFEST.yaml, unit `twocmt_oral`.
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
;           one-compartment oral run, base it on that run and diff against
;           it: once a parent run exists, lineage follows the model family,
;           not the template.
;
; RUN-RECORD HEADER - fill before submission (skills/model-management)
;   Run:        <run id>
;   Based on:   skills/reference-models/reference/twocmt_oral.ctl (template)
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
;
; Compartment routing: under ADVAN4 a dose goes to compartment 1 (the
; depot) and an observation to compartment 2 (central) by default, so no
; CMT column is needed when every dose is oral. A study with both IV and
; oral doses DOES need a CMT column, in this list and in the dataset.
$INPUT C NUM ID TIME EVID AMT DV MDV

; ADAPT-HERE. Point at the gated analysis-ready file. bbi resolves a
; relative path from the run subdirectory and PsN from this file's
; directory, so use an absolute path when both lanes read the file.
$DATA ../analysis-ready.csv IGNORE=@ IGNORE=(C.EQ.'C')

; --- structure -------------------------------------------------------------
; KEEP. ADVAN4 is two compartments with first-order absorption from a
; depot. TRANS4 parameterizes it as KA, CL, V2, Q and V3 (clearances and
; volumes) instead of the TRANS1 micro-constants.
$SUBROUTINES ADVAN4 TRANS4

$PK
; KEEP the mu-referenced form. MU_n depends on THETAs and covariates only,
; never on an ETA, and MU_n pairs with ETA(n) by index. The THETAs that
; carry IIV therefore come FIRST, so the MU and ETA indices line up with no
; gap. THETAs sit on the log scale, so every parameter is positive by
; construction.
;
; ADAPT-HERE for covariates: put a BASELINE covariate term INSIDE the MU_n
; expression, for example MU_1 = THETA(1) + THETA(6)*LOG(WT/70), never
; after it. A TIME-VARYING covariate stays OUTSIDE the MU expression (for
; example CL = WTNOW/70*EXP(MU_1 + ETA(1))), because NONMEM silently
; averages a time-varying quantity inside MU_n over each subject's records
MU_1 = THETA(1)
MU_2 = THETA(2)
CL   = EXP(MU_1 + ETA(1))
KA   = EXP(MU_2 + ETA(2))
V2   = EXP(THETA(3))
Q    = EXP(THETA(4))
V3   = EXP(THETA(5))

; KEEP. Central is compartment 2 under ADVAN4 (1 is the depot, 3 is
; peripheral), so S2 is scaled by the central volume V2. Scaling compartment
; 1 instead is a classic silent error: the predictions land on the wrong
; scale and still look plausible. Check that the units agree.
S2   = V2

; KEEP. ETA aliases for the table: NM-TRAN rejects an undefined name.
ET1  = ETA(1)
ET2  = ETA(2)

$ERROR
; Proportional residual error. The SKILL.md building-block appendix gives
; the additive and combined forms and when each applies.
IPRED = F
Y     = F*(1 + EPS(1))

; --- parameters ------------------------------------------------------------
; ADAPT-HERE, every line. PLACEHOLDER INITIALS with no analysis meaning.
; Order: the mu-referenced parameters first (CL, KA), then the rest.
; A point of method, not of numbers: this is the most weakly identified
; structure in the starter set. Absorption and distribution can trade
; against each other (flip-flop, and KA against Q), so derive the KA, Q and
; V3 initials from the observed profile shape instead of carrying these
; placeholders forward, and expect one run for each competing solution.
$THETA
(2.30)     ; 1 log CL  [log L/h]  PLACEHOLDER, exp = 10
(0.00)     ; 2 log KA  [log 1/h]  PLACEHOLDER, exp = 1
(3.00)     ; 3 log V2  [log L]    PLACEHOLDER, exp = 20  (CENTRAL volume)
(1.61)     ; 4 log Q   [log L/h]  PLACEHOLDER, exp = 5
(3.91)     ; 5 log V3  [log L]    PLACEHOLDER, exp = 50  (PERIPHERAL volume)

; Diagonal IIV on CL and KA, the conservative start for an oral model. A
; two-compartment oral base model rarely supports IIV on all five
; parameters at the first fit; add IIV one parameter at a time, each
; addition justified in its run record. If KA is poorly identified (high
; shrinkage, large RSE), dropping OMEGA(2,2) is a legitimate candidate,
; recorded as one. The SKILL.md appendix gives the block(2) form.
$OMEGA
0.09       ; OMEGA(1,1) IIV on CL, log-scale variance  PLACEHOLDER
0.09       ; OMEGA(2,2) IIV on KA, log-scale variance  PLACEHOLDER

$SIGMA
0.04       ; SIGMA(1,1) proportional error variance  PLACEHOLDER

; --- estimation ------------------------------------------------------------
; KEEP. FOCE with interaction is the house default for continuous PK with a
; proportional or combined error. NOABORT lets a run that meets a numerical
; problem finish and report instead of stopping without a report.
$ESTIMATION METHOD=1 INTERACTION MAXEVAL=9999 PRINT=5 NOABORT

; KEEP. Without the covariance step there are no standard errors and no
; RSEs, and the base-model gate package cannot be assembled. A model that
; minimizes but fails the covariance step is an observation to report, not
; a run to retry quietly.
$COVARIANCE

; KEEP the leading NUM column: it is the join key for bbr::nm_join(), which
; brings the input columns (ID, TIME, DV, covariates) back next to these
; computed ones. Do not table DV or other input columns here; nm_join would
; find them twice and rename them.
; ADAPT-HERE: the file name only.
$TABLE NUM PRED IPRED CWRES ET1 ET2 NOPRINT ONEHEADER FILE=ADAPT-HERE-tab
