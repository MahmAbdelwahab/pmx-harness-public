# REFERENCE IMPLEMENTATION — Bootstrap agreement and bootstrap summaries
#
# AUTHORITY: skills/diagnostics/MANIFEST.yaml is the authority for the SME signature
# (root reviewed_by / reviewed_at) and for the status and digest of unit
# the manifest. The entrypoints summarize bootstrap agreement, success,
# interval finiteness and parameter precision from caller-supplied inputs.
#   - A3 item 2: the denominator is the REQUESTED replicate count; the parsed
#     sample-row count must equal it and the flag vector must have one value
#     per parsed row, else stop; a minimization_successful value that is
#     missing or not exactly 0 or 1 is a stop. The domain is checked on the
#     RAW flag values (erratum P-1.4-1), before any conversion to logical.
#     The original-fit marker is reconciled by the only producer of these
#     inputs, reference_psn_bootstrap_samples (A3 item 9), which refuses a
#     file whose `model` column does not mark row 1 as the original fit.
#   - A3 item 3: only UNSUCCESSFUL replicates are counted, and a replicate's
#     category is the list of PsN flags set on its row (value not 0), joined
#     with " + " in the declared order BOOTSTRAP_CATEGORY_FLAGS below;
#     "no flag set" is its own category. The category-forming set is the six
#     termination-condition flags of the PsN per-sample file. Not category
#     inputs: minimization_successful (0 on every counted row, so it carries
#     no information), covariance_step_run, covariance_step_successful and
#     covariance_step_warnings (they describe the later covariance step, and
#     the first two read 1 for success), and condition_number (a value, not a
#     flag). Every declared flag must be a non-negative whole number
#     (zero_gradients is a count in PsN's file, so any value above 0 is set).
#   - IIV precision: round(100 * SD / estimate, 3), using the final
#     model variance-term estimate.
#   - A3 item 5: fixed-effect bootstrap precision
#     round(100 * sd / abs(estimate), 3) on the estimation scale.
#   - A3 items 7 and A3.1: summaries over successful replicates only; a
#     non-finite estimate in a successful replicate stops; each replicate is
#     transformed to the natural scale first, then summarised with
#     stats::quantile type 6 (PsN parity, A3.1); the standard deviation uses
#     n - 1 on the estimation scale; fewer than two successful replicates stop.
#   - A3 item 8: back-transformation `identity` or `exp` per parameter, as
#     declared; any other form stops.
#
# Every threshold and interval level is a declared parameter with no default.


# Self-gate on the intended-use unit "bootstrap_agreement": the private, locked
# validator environment of npde_tests.R (integrity batches 11 to 13; the
# construction is explained there).
#
# THE HANDOVER, stated where the construction is written. Every executable
# name the construction reaches -- local, new.env, baseenv, file.exists, stop,
# sys.source and lockEnvironment, and since integrity batch 13 the `c` of
# every path vector, the `local` of every gate and accessor closure, and the
# heads of the pre-seal write -- is written with the exact `base::`
# qualification, and the block's own frame is supplied explicitly as
# `envir = base::new.env(parent = base::baseenv())`, so neither the frame nor
# its parent is the environment this file is sourced into. A caller who
# pre-binds a fake `sys.source`, `new.env`, `c` or `local` there is not
# consulted (integrity batches 12 and 13, Codex rounds 11 and 12). What
# remains is a caller who rebinds the `::` operator or R's syntax heads
# (`{`, `(`, `<-`, `if`, `for`, `!`, `function`) in that environment, or
# modifies the base namespace itself: the same class of act as modifying R.
# Same-process reflection -- a caller reaching a sealed environment through
# `environment(fn)`, `parent.env<-` or an active binding -- is out of scope
# of the method guard: its environment protects against ordinary
# rebinding, not against a hostile caller in the same process; the controls
# there are the trusted loader (tools/load_reference.R), the manifest
# digests, the write guard and the SME's review of the diff.
.mm_trusted_private <- base::local({
  e <- base::new.env(parent = base::baseenv())
  if (!base::file.exists("tools/method_manifest.R"))
    base::stop("this reference requires tools/method_manifest.R; references are ",
               "sourced with the repository root as the working directory.",
               call. = FALSE)
  base::sys.source("tools/method_manifest.R", envir = e, keep.source = FALSE)
  base::lockEnvironment(e, bindings = TRUE)
  e
}, envir = base::new.env(parent = base::baseenv()))

.diag_bootstrap_agreement_gate <- base::local({
  private <- .mm_trusted_private
  function() {
    if (!base::get("require_validated_method", envir = private)(skill_dir = "skills/diagnostics",
                                          calling_file = "skills/diagnostics/reference/bootstrap_agreement.R",
                                          unit = "bootstrap_agreement")) {
      base::stop("diagnostics unit 'bootstrap_agreement' is not SME-validated ",
           "(manifest unit draft/rejected, or the manifest has an empty ",
           "reviewed_by); the bootstrap summaries do not compute until the unit ",
           "is approved. ESCALATE to the SME.", call. = FALSE)
    }
    base::invisible(TRUE)
  }
}, envir = base::new.env())

BOOTSTRAP_CATEGORY_FLAGS <- c("estimate_near_boundary", "rounding_errors",
                              "zero_gradients", "final_zero_gradients",
                              "hessian_reset", "s_matrix_singular")

.ba_finite <- function(x, what) {
  if (!is.numeric(x) || !length(x) || any(!is.finite(x)))
    stop(what, " must be a non-empty vector of finite numbers.", call. = FALSE)
}
.ba_same_length <- function(a, b, what) {
  if (length(a) != length(b))
    stop(what, " must have the same length.", call. = FALSE)
}

# A2 item 3. Moved over unchanged (qualify.R line 294).
reference_bootstrap_pct_diff <- function(est_bt, med_bt) {
  .diag_bootstrap_agreement_gate()
  .ba_finite(est_bt, "est_bt"); .ba_finite(med_bt, "med_bt")
  .ba_same_length(est_bt, med_bt, "est_bt and med_bt")
  if (any(est_bt == 0))
    stop("a point estimate of zero cannot be a percentage denominator.", call. = FALSE)
  round(100 * abs(med_bt - est_bt) / abs(est_bt), 2)
}

# A3 item 2. `minimization_successful` is the RAW per-sample column (sample
# rows only), `n_samples` the parsed sample-row count and `requested` the
# replicate count the run was asked for.
reference_bootstrap_replicate_accounting <- function(minimization_successful,
                                                     n_samples, requested) {
  .diag_bootstrap_agreement_gate()
  for (nm in c("n_samples", "requested")) {
    v <- get(nm)
    if (!is.numeric(v) || length(v) != 1L || !is.finite(v) || v < 1 || v != floor(v))
      stop(nm, " must be a single positive whole number.", call. = FALSE)
  }
  if (n_samples != requested)
    stop("parsed sample rows (", n_samples, ") do not reconcile with the requested ",
         "replicates (", requested, "); no percentage is computed. ESCALATE.",
         call. = FALSE)
  if (length(minimization_successful) != n_samples)
    stop("the success flag vector has ", length(minimization_successful),
         " values for ", n_samples, " parsed sample rows.", call. = FALSE)
  raw <- as.character(minimization_successful)
  if (anyNA(raw) || !all(raw %in% c("0", "1")))
    stop("minimization_successful must be exactly 0 or 1 on every sample row; ",
         sum(is.na(raw) | !raw %in% c("0", "1")), " value(s) are missing or ",
         "outside that domain. ESCALATE.", call. = FALSE)
  ok <- raw == "1"
  n <- as.integer(requested)
  list(success = ok, n_requested = n, n_successful = sum(ok),
       success_rate_pct = round(100 * sum(ok) / n, 1))
}

# A3 item 3, the tabulation. Moved over unchanged (qualify.R lines 351-353).
reference_bootstrap_category_table <- function(msgs) {
  .diag_bootstrap_agreement_gate()
  if (!is.character(msgs) || anyNA(msgs))
    stop("msgs must be a character vector without missing values.", call. = FALSE)
  # No replicate to count: table() of an empty vector has no message column,
  # so the empty result is stated here rather than by the tabulation.
  if (!length(msgs))
    return(data.frame(replicate_message = character(0), count = integer(0),
                      stringsAsFactors = FALSE))
  msg_table <- as.data.frame(table(message = msgs),
                             stringsAsFactors = FALSE)
  names(msg_table) <- c("replicate_message", "count")
  msg_table
}

# A3 item 3, the categories (new). `success` is the logical vector of
# reference_bootstrap_replicate_accounting; `flags` a data frame with one row
# per sample and every column of BOOTSTRAP_CATEGORY_FLAGS.
reference_bootstrap_unsuccessful_counts <- function(success, flags) {
  .diag_bootstrap_agreement_gate()
  if (!is.logical(success) || anyNA(success))
    stop("success must be a logical vector without missing values.", call. = FALSE)
  if (!is.data.frame(flags) || nrow(flags) != length(success))
    stop("flags must be a data frame with one row per success value (", length(success),
         " rows).", call. = FALSE)
  miss <- setdiff(BOOTSTRAP_CATEGORY_FLAGS, names(flags))
  if (length(miss))
    stop("flags is missing declared column(s): ", paste(miss, collapse = ", "), ".",
         call. = FALSE)
  for (f in BOOTSTRAP_CATEGORY_FLAGS) {
    v <- flags[[f]]
    if (!is.numeric(v) || anyNA(v) || any(v < 0 | v != floor(v)))
      stop("flag column ", f, " must hold non-negative whole numbers without ",
           "missing values.", call. = FALSE)
  }
  bad <- flags[!success, BOOTSTRAP_CATEGORY_FLAGS, drop = FALSE]
  msgs <- vapply(seq_len(nrow(bad)), function(i) {
    set <- BOOTSTRAP_CATEGORY_FLAGS[unlist(bad[i, ]) != 0]
    if (length(set)) paste(set, collapse = " + ") else "no flag set"
  }, character(1))
  reference_bootstrap_category_table(msgs)
}

# A3 item 4 (new rounding): IIV variance terms, estimate > 0.
reference_bootstrap_iiv_rse <- function(sd, estimate) {
  .diag_bootstrap_agreement_gate()
  .ba_finite(sd, "sd"); .ba_finite(estimate, "estimate")
  .ba_same_length(sd, estimate, "sd and estimate")
  if (any(estimate <= 0))
    stop("a variance-term estimate must be positive to be an RSE denominator.",
         call. = FALSE)
  round(100 * sd / estimate, 3)
}

# A3 item 5 (new): fixed effects, estimation scale.
reference_bootstrap_fixed_rse <- function(sd, estimate) {
  .diag_bootstrap_agreement_gate()
  .ba_finite(sd, "sd"); .ba_finite(estimate, "estimate")
  .ba_same_length(sd, estimate, "sd and estimate")
  if (any(estimate == 0))
    stop("a point estimate of zero cannot be an RSE denominator.", call. = FALSE)
  round(100 * sd / abs(estimate), 3)
}

# A3 item 6. Moved over unchanged (qualify.R lines 298 and 312).
reference_bootstrap_ci_finite <- function(lower, upper) {
  .diag_bootstrap_agreement_gate()
  if (!is.numeric(lower) || !is.numeric(upper))
    stop("interval bounds must be numeric.", call. = FALSE)
  .ba_same_length(lower, upper, "lower and upper bounds")
  is.finite(lower) & is.finite(upper)
}

# A3 item 8 (new).
reference_bootstrap_back_transform <- function(x, transform) {
  .diag_bootstrap_agreement_gate()
  if (!is.numeric(x))
    stop("x must be numeric.", call. = FALSE)
  if (!is.character(transform) || length(transform) != 1L ||
      !transform %in% c("identity", "exp"))
    stop("transform must be 'identity' or 'exp' (declared per parameter); got ",
         deparse(transform), ". Any other form is a stop for the SME.", call. = FALSE)
  if (transform == "exp") exp(x) else x
}

# A3 items 7 and 8 with A3.1 (new). `estimates` has one column per parameter
# and one row per sample; `success` one logical per row; `transform` a named
# character vector covering every column; `ci` the declared interval level.
reference_bootstrap_summaries <- function(estimates, success, transform, ci) {
  .diag_bootstrap_agreement_gate()
  if (missing(ci) || !is.numeric(ci) || length(ci) != 1L || !is.finite(ci) ||
      ci <= 0 || ci >= 1)
    stop("ci is the declared interval level (a single number in (0, 1)); it has ",
         "no default.", call. = FALSE)
  if (!is.data.frame(estimates) || !ncol(estimates))
    stop("estimates must be a data frame with one column per parameter.", call. = FALSE)
  if (!is.logical(success) || anyNA(success) || length(success) != nrow(estimates))
    stop("success must be a logical vector without missing values, one per sample row.",
         call. = FALSE)
  miss <- setdiff(names(estimates), names(transform))
  if (length(miss))
    stop("no declared transform for parameter(s): ", paste(miss, collapse = ", "), ".",
         call. = FALSE)
  n <- sum(success)
  if (n < 2L)
    stop("the summaries need at least two successful replicates (a standard ",
         "deviation needs two); got ", n, ". ESCALATE.", call. = FALSE)
  p <- c((1 - ci) / 2, 1 - (1 - ci) / 2)
  rows <- lapply(names(estimates), function(nm) {
    x <- estimates[[nm]][success]
    if (!is.numeric(x) || any(!is.finite(x)))
      stop("a successful replicate carries a non-finite estimate of ", nm,
           "; the engine record is inconsistent. ESCALATE.", call. = FALSE)
    nat <- reference_bootstrap_back_transform(x, transform[[nm]])
    q <- stats::quantile(nat, c(0.5, p), type = 6, names = FALSE)
    data.frame(parameter = nm, transform = transform[[nm]], n_successful = n,
               median_natural = q[1], ci_lower_natural = q[2],
               ci_upper_natural = q[3], sd_estimation = stats::sd(x),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}
