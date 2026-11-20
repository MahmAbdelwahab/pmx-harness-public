# REFERENCE IMPLEMENTATION — Covariate search
#
# STATUS: PLACEHOLDER, NOT WRITTEN. Authored when the first covariate
# analysis is declared, matching the search method its MAP chooses
# (prior-anchored full model, SCM restricted to the prior list, or FREM
# per skills/covariate-priors/SKILL.md), and SME-validated before use.
#
# Validation record (SME fills in):
#   Reviewed by:
#   Date:
#   Source of truth:
#
# Expected contract:
#   reference_covariate_search(fit_base, prior_list, method, control, seed)
#   -> search trail (every step's OFV and decision, verbatim), final fit,
#      prior-list disposition table — the covariate-gate audit set.

# Hard stop by design: the harness rule "no reference implementation, no
# computation" is enforced structurally — any attempt to run a covariate
# search before this file is authored and SME-validated fails loudly.
reference_covariate_search <- function(...) {
  stop("covariate-priors reference implementation not yet authored; ",
       "it is written against the MAP's declared search method when the ",
       "first covariate analysis arrives. ESCALATE to the SME.",
       call. = FALSE)
}
