# The trusted reference loader (integrity batch 13; Codex round 12 findings
# R12-P0-3 and decision 1B/2 of the SME, 2026-09-05).
#
# WHAT THIS IS. The one supported way for harness code -- the pipeline task
# modules under pipeline/tasks/, pipeline/compose.R, and the test batteries --
# to bring a skill's reference implementation into an R session:
#
#   env <- load_reference("skills/diagnostics", "skills/diagnostics/reference/gof.R")
#   load_reference("skills/diagnostics", ref, envir = environment())   # the
#                                              # pipeline's `source(local = TRUE)` shape
#
# WHY IT EXISTS: AUTHENTICATION BEFORE EXECUTION. Until batch 13 a reference
# was `source()`d directly, and its own sealed construction then sourced the
# declared dependencies and recorded their digests AFTER the load. A digest
# taken after execution is evidence of the disk's state, not authentication of
# the code that ran: a dependency replaced with a file carrying a top-level
# side effect performed that side effect before the later digest mismatch
# stopped computation, and a file restored before the gate ran could pass the
# later disk check while the environment retained the executed definitions
# (R12-P0-3). This loader inverts the order. It reads the manifest, finds the
# unit(s) whose reference_path is the requested file, READS THE REFERENCE
# BYTES ONCE, hashes them, compares with the manifest digest (refusing on a
# mismatch, naming both digests), does the same for every declared
# dependency, and only then evaluates THOSE BYTES -- `parse(text = <the
# checked bytes>)`, never a second read of the file -- into the target
# environment. A file whose bytes do not match is never parsed, so its top
# level never runs.
#
# WHAT IT DOES NOT DO, stated so nobody assumes it.
#   - It does NOT source the dependencies into the environment. The
#     reference's own sealed dependency construction does that, from disk,
#     when the reference's top level runs (eda.R's `.eda_deps`,
#     psn_covsearch.R's `.mm_trusted_deps`). The loader's job ends at
#     authentication: the dependency bytes it hashed are the bytes on disk at
#     that instant, and the construction re-reads the same path a few
#     milliseconds later. The window between the loader's check and the
#     construction's read is THE RESIDUAL: a time-of-check/time-of-use race on
#     the repository's own files, held by the write guard on those paths and
#     by the digest the gate re-checks at every gated call (eda.R compares
#     `dependency_sha256`, taken at load, with the file on every call; the
#     manifest validator re-hashes every `depends:` at every gated call). The
#     digest the reference records after its own load is a LOAD RECORD, not
#     the authentication -- the reference's comment says so too.
#   - It does NOT decide approval. A draft unit loads exactly as an approved
#     one does; its gate refuses at the first gated call, as before. Loading
#     is not authorization; the manifest's `status:` and root confirmation are.
#   - It does NOT protect against a hostile caller in the same R process
#     (reflection through `environment(fn)`, `parent.env<-`, active bindings):
#     out of scope by SME decision 3; see tools/method_manifest.R's invariant
#     comment and docs/PORTABILITY.md.
#   - It does NOT change the working directory. References are written for
#     the repository root (their sealed constructions load
#     "tools/method_manifest.R" by that relative path), and the manifest's
#     reference_path is repository-relative; callers run from the root, as
#     the pipeline does.
#
# THE TARGET ENVIRONMENT (SME decision D14-2, 2026-09-05). `envir = NULL` (the
# default) is THE SUPPORTED INTERFACE: a fresh environment whose parent is
# baseenv(). That chain is env -> base -> the EMPTY environment; it does NOT
# continue into R_GlobalEnv or the search path. (An earlier version of this
# comment claimed "env -> base -> R_GlobalEnv -> the search path"; that
# confused the base environment with the base namespace, whose parent IS the
# global environment. Check it:
# `Rscript -e 'environmentName(parent.env(baseenv()))'` prints "R_EmptyEnv".)
# The isolation is the point -- a caller's rebinding of a base name is not
# consulted -- and the price is that a reference loaded this way can call
# NOTHING it does not qualify. Every reference therefore qualifies every
# non-base call (`stats::quantile`, `stats::median`, `stats::setNames`,
# `utils::head`, `pmplots::`, `digest::`), attaches nothing at load, and
# `source()`s any harness helper it needs with `local = TRUE` so the helper
# lands in the calling frame rather than in the unreachable global
# environment. A census of unqualified non-base call heads over the sealed
# references is part of the integrity battery.
#
# Qualification alone is not sufficient for S3 dispatch across the boundary.
# Because the reference's frame does not reach the caller's global environment,
# an S3 method the CALLER defines there is not dispatched: `stats::vcov(fit)` on
# an object whose only `vcov` method is `vcov.myfit` in globalenv() fails with
# "no applicable method for 'vcov' applied to an object of class ...". A method a
# reference needs must be registered by a package (the usual case, and it works)
# or defined in the reference itself.
#
# `envir = environment()` -- the pipeline's former `source(ref, local = TRUE)`
# shape -- remains available for the pipeline task modules and is
# CALLER-OWNED: the reference is evaluated into a frame the caller controls,
# so the loader offers NO isolation guarantee for it. Authentication before
# execution still holds; name resolution is the caller's responsibility.
# The environment is returned either way, carrying the load record as
# attribute "load_reference" (reference path, digest, dependency paths and
# digests, manifest path).
#
# ITSELF. This file is harness core (HARNESS_CORE in the write guard): an
# editable loader is no loader. It is written with `base::` qualification
# throughout for the same reason the sealed constructions are, and it loads
# the manifest reader from tools/method_manifest.R into a private,
# base-parented environment rather than trusting whatever
# `read_method_manifest` a caller has on the search path.
#
# Since integrity batch 14 (Codex round 13, R13-P0-2) the loader is also
# CONSTRUCTED in a controlled enclosure: `base::local({ ... }, envir =
# base::new.env(parent = base::baseenv()))`, the same discipline the sealed
# reference constructions follow. Before that, the function's own body
# resolved through whatever environment the file had been sourced into --
# ordinarily the global environment -- so an ordinary binding of `[[` there
# could hand `base::eval()` an expression that was never hashed, and an
# ordinary binding of `$` could substitute the manifest reader. Neither is
# reflection. Every extraction and replacement operator in the body is now
# written `base::`[[`(x, i)`, `base::`[[<-`(...)`, `base::`[<-`(...)` and
# `base::get(name, envir = e)`, so nothing the body evaluates depends on a
# name a caller can bind.

load_reference <- base::local({
function(skill_dir, reference_path, envir = NULL) {
  refuse <- function(...) base::stop("load_reference: ", ..., call. = FALSE)
  if (!base::is.character(skill_dir) || base::length(skill_dir) != 1L || !base::nzchar(skill_dir))
    refuse("`skill_dir` must be a single skill directory such as \"skills/diagnostics\".")
  if (!base::is.character(reference_path) || base::length(reference_path) != 1L ||
      !base::nzchar(reference_path))
    refuse("`reference_path` must be a single repository-relative path to a reference .R file.")
  if (!base::grepl("\\.R$", reference_path))
    refuse("reference_path must end in .R; a template is never evaluated.")
  if (!base::is.null(envir) && !base::is.environment(envir))
    refuse("`envir` must be NULL or an environment.")

  # The manifest reader, from a private base-parented environment.
  mm_path <- "tools/method_manifest.R"
  if (!base::file.exists(mm_path))
    refuse("required file ", mm_path, " is missing; references are loaded with the ",
           "repository root as the working directory.")
  mm <- base::new.env(parent = base::baseenv())
  base::sys.source(mm_path, envir = mm, keep.source = FALSE)
  base::lockEnvironment(mm, bindings = TRUE)

  manifest_path <- base::file.path(skill_dir, "MANIFEST.yaml")
  if (!base::file.exists(manifest_path))
    refuse("no method manifest at ", manifest_path, "; a reference is loaded only ",
           "through the skill's MANIFEST.yaml, which names its units and digests.")
  m <- base::get("read_method_manifest", envir = mm)(manifest_path)

  # The unit(s) whose reference this is. The path is matched as written and,
  # when the caller passed another spelling of the same file, by its
  # repository-relative form.
  rel <- base::get(".mm_relpath", envir = mm)(reference_path)
  units <- base::Filter(function(u)
                          base::identical(base::`[[`(u, "reference_path"), reference_path) ||
                          base::identical(base::`[[`(u, "reference_path"), rel),
                        base::`[[`(m, "units"))
  if (!base::length(units))
    refuse("no unit of ", manifest_path, " names '", reference_path,
           "' as its reference_path; only a file the manifest binds can be loaded.")
  if (!base::startsWith(rel, base::paste0(skill_dir, "/")))
    refuse("'", reference_path, "' is not under ", skill_dir, "/; the reference ",
           "must belong to the skill whose manifest authorizes it.")
  declared <- base::unique(base::vapply(units, function(u) base::`[[`(u, "sha256"),
                                        base::character(1)))
  if (base::length(declared) != 1L)
    refuse("the units naming '", reference_path, "' in ", manifest_path,
           " declare different digests (", base::paste(declared, collapse = ", "),
           "); the manifest is inconsistent. Regenerate it with write_method_manifest().")

  # Read once, hash the bytes read, compare. The same bytes are parsed below.
  read_once <- function(path) {
    if (!base::file.exists(path)) refuse("'", path, "' does not exist.")
    con <- base::file(path, open = "rb")
    base::on.exit(base::close(con), add = TRUE)
    acc <- base::raw(0)
    repeat {
      chunk <- base::readBin(con, what = "raw", n = 1048576L)
      if (!base::length(chunk)) break
      acc <- base::c(acc, chunk)
    }
    acc
  }
  sha_of <- function(bytes)
    base::paste0("sha256:", digest::digest(bytes, algo = "sha256", serialize = FALSE))

  ref_bytes <- read_once(reference_path)
  ref_sha <- sha_of(ref_bytes)
  if (!base::identical(ref_sha, declared))
    refuse("the bytes of '", reference_path, "' do not match the digest ", manifest_path,
           " binds for it (manifest ", declared, ", on disk ", ref_sha, "). The file ",
           "was not evaluated: nothing it contains has run. Either the reference was ",
           "edited after the manifest was generated (regenerate with ",
           "write_method_manifest() and have the SME re-confirm) or something other ",
           "than the approved file is at that path. ESCALATE to the SME.")

  # Every declared dependency of every unit that names this reference.
  deps <- base::list()
  for (u in units) for (dp in base::`[[`(u, "depends"))
    deps <- base::`[[<-`(deps, base::`[[`(dp, "path"), base::`[[`(dp, "sha256"))
  dep_record <- base::character(0)
  for (dpath in base::names(deps)) {
    dbytes <- read_once(dpath)
    dsha <- sha_of(dbytes)
    if (!base::identical(dsha, base::`[[`(deps, dpath)))
      refuse("the bytes of the declared dependency '", dpath, "' do not match the ",
             "digest ", manifest_path, " binds for it under depends: (manifest ",
             base::`[[`(deps, dpath), ", on disk ", dsha, "). Neither the dependency nor '",
             reference_path, "' was evaluated: nothing in either has run. ",
             "ESCALATE to the SME.")
    dep_record <- base::`[<-`(dep_record, dpath, dsha)
  }

  # Evaluate exactly the bytes that were hashed. `parse(text = )` on the
  # decoded bytes -- never a second read of the path -- into the target.
  txt <- base::rawToChar(ref_bytes)
  base::Encoding(txt) <- "UTF-8"
  exprs <- base::parse(text = txt, keep.source = FALSE, srcfile = reference_path)
  target <- if (base::is.null(envir)) base::new.env(parent = base::baseenv()) else envir
  for (i in base::seq_along(exprs)) base::eval(base::`[[`(exprs, i), envir = target)
  base::attr(target, "load_reference") <- base::list(
    skill_dir = skill_dir, manifest_path = manifest_path,
    reference_path = reference_path, sha256 = ref_sha,
    depends = dep_record, units = base::names(units),
    loaded_at_utc = base::format(base::Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  target
}
}, envir = base::new.env(parent = base::baseenv()))
