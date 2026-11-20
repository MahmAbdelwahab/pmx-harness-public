# THE ONE SEED HELPER (Codex round 8, finding F15).
#
# WHY THIS EXISTS ------------------------------------------------------------
# Every reference that resamples called bare `set.seed(seed)`. set.seed()
# seeds the CURRENT RNG, but which generator that is depends on the session's
# RNGkind()/normal.kind/sample.kind. R has changed those defaults before (the
# sample.kind switch from "Rounding" to "Rejection" in R 3.6.0 silently
# changed every sample()/resample result), and a user's .Rprofile can set a
# non-default kind. A bare set.seed() therefore does NOT pin reproducibility:
# the same seed under a different RNGkind draws a different stream. A stored
# result could fail to reproduce on a machine whose RNG defaults differ, with
# no error — exactly the class of silent drift the harness refuses.
#
# harness_seed() pins the generator kinds AND the seed, so a seeded reference
# draws the same stream on any machine. The pinned kinds are the R >= 3.6.0
# defaults (Mersenne-Twister / Inversion / Rejection), which is what the
# existing stores were built under — so pinning changes no stored number on a
# current R, it only guarantees the stream against a future default change or
# a non-default session.
#
# It RETURNS the prior RNG state (kinds + .Random.seed) so a caller can
# restore the session it inherited, and it records the pinned kinds in the
# returned object so a gate package can print exactly which generator produced
# a result. Base R only.

# The harness-pinned generator kinds. Named so a gate package / record can
# echo them verbatim.
HARNESS_RNG_KINDS <- c(kind = "Mersenne-Twister",
                       normal.kind = "Inversion",
                       sample.kind = "Rejection")

# Pin the RNG kinds and set the seed. Returns (invisibly) a record:
#   list(seed, kind, normal.kind, sample.kind, prior)
# where `prior` is list(kinds = RNGkind() before, random_seed = the prior
# .Random.seed or NULL) for restoration via harness_seed_restore().
harness_seed <- function(seed,
                         kind        = HARNESS_RNG_KINDS[["kind"]],
                         normal.kind = HARNESS_RNG_KINDS[["normal.kind"]],
                         sample.kind = HARNESS_RNG_KINDS[["sample.kind"]]) {
  if (missing(seed) || length(seed) != 1L || is.na(seed))
    stop("harness_seed(): a single non-NA seed is required.", call. = FALSE)
  prior <- list(
    kinds = RNGkind(),
    random_seed = if (exists(".Random.seed", envir = globalenv(),
                             inherits = FALSE))
      get(".Random.seed", envir = globalenv()) else NULL)
  # set.seed() with explicit kinds both pins the generator and seeds it.
  set.seed(seed, kind = kind, normal.kind = normal.kind,
           sample.kind = sample.kind)
  invisible(structure(
    list(seed = seed, kind = kind, normal.kind = normal.kind,
         sample.kind = sample.kind, prior = prior),
    class = "harness_seed"))
}

# Restore the RNG state captured in a harness_seed() record's `prior`.
harness_seed_restore <- function(record) {
  if (!inherits(record, "harness_seed"))
    stop("harness_seed_restore(): expects a harness_seed record.",
         call. = FALSE)
  p <- record$prior
  do.call(RNGkind, as.list(p$kinds))
  if (is.null(p$random_seed)) {
    if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
      rm(".Random.seed", envir = globalenv())
  } else {
    assign(".Random.seed", p$random_seed, envir = globalenv())
  }
  invisible(TRUE)
}
