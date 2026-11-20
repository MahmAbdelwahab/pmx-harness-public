# One-shot R environment setup for pmx-harness (development machines).
# Supply NONMEM, PsN, and bbi separately when the analysis requires them.

# In site provisioning mode, the system R library is authoritative.
# The environment check verifies it against config/r-packages.lock.
source("tools/r_lock.R")
if (identical(r_provision(), "site")) {
  stop("r_provision is 'site' (harness.yaml): this machine's validated ",
       "R library is authoritative and setup_environment.R must not ",
       "install into or shadow it. Use check_environment() to verify ",
       "the library against config/r-packages.lock; version mismatches ",
       "are SME decisions, never auto-fixes.", call. = FALSE)
}
#
# Package sources:
# - MPN supplies Metrum packages from a pinned stable snapshot.
# - CRAN dependencies install as Posit binaries (no compiler toolchain
#   needed; avoids the cmake/libuv source-build requirements of
#   RcppParallel and fs).
# - readr is pinned from the MPN snapshot: bbr 1.14.2 imports
#   readr::read_table2, which newer CRAN readr removed.

MPN_SNAPSHOT <- "2025-09-17"

# Posit Package Manager serves Linux binaries per distribution codename.
# Resolve the codename from /etc/os-release (present on every systemd-era
# distribution; lsb_release is not), fall back to the source repository
# when the host is not Linux or the codename is unknown, and let a site
# override the whole CRAN entry with `cran_repo:` in harness.yaml.
resolve_cran_repo <- function() {
  site <- tryCatch(yaml::read_yaml("harness.yaml")[["cran_repo"]],
                   error = function(e) NULL)
  if (is.character(site) && length(site) == 1L && nzchar(site)) return(site)
  source_repo <- "https://packagemanager.posit.co/cran/latest"
  if (!identical(tolower(Sys.info()[["sysname"]]), "linux")) {
    message("setup_environment: non-Linux host; CRAN packages install from source.")
    return(source_repo)
  }
  codename <- NA_character_
  if (file.exists("/etc/os-release")) {
    kv <- readLines("/etc/os-release", warn = FALSE)
    hit <- grep("^VERSION_CODENAME=", kv, value = TRUE)
    if (length(hit)) codename <- gsub('^VERSION_CODENAME=|"', "", hit[[1]])
  }
  if (is.na(codename) || !nzchar(codename)) {
    lsb <- suppressWarnings(tryCatch(system("lsb_release -cs", intern = TRUE),
                                     error = function(e) character(0)))
    if (length(lsb) == 1L && nzchar(lsb)) codename <- lsb
  }
  known <- c("focal", "jammy", "noble", "bullseye", "bookworm", "trixie",
             "rhel9", "rhel8", "centos7", "opensuse155", "opensuse156")
  if (is.na(codename) || !nzchar(codename) || !codename %in% known) {
    message(sprintf(paste0("setup_environment: distribution codename %s is not a ",
                           "known Posit binary target; CRAN packages install from ",
                           "source. Set `cran_repo:` in harness.yaml to use a site mirror."),
                    if (is.na(codename) || !nzchar(codename)) "(none)" else shQuote(codename)))
    return(source_repo)
  }
  sprintf("https://packagemanager.posit.co/cran/__linux__/%s/latest", codename)
}

options(
  repos = c(
    MPN  = sprintf("https://mpn.metworx.com/snapshots/stable/%s", MPN_SNAPSHOT),
    CRAN = resolve_cran_repo()
  ),
  HTTPUserAgent = sprintf("R/%s R (%s)", getRversion(),
                          paste(getRversion(), R.version$platform,
                                R.version$arch, R.version$os))
)

pkgs <- c(
  "targets", "here", "yaml",                       # pipeline
  "yspec", "mrgda", "lastdose", "mrgmisc",         # data assembly
  "pmtables", "pmplots", "mrggsave",               # EDA / diagnostics
  "bbr",                                           # model management (NONMEM)
  "rxode2", "nlmixr2",                             # estimation engine
  "tarchetypes", "quarto"                          # reporting (tar_quarto)
)
install.packages(pkgs, Ncpus = 4)

# readr MUST come from the MPN snapshot alone: install.packages() otherwise
# picks the newer PPM version, which removed read_table2 and breaks bbr.
# Same for the nlmixr2 support chain (PPM's newest Deriv needs R >= 4.5).
install.packages(c("readr", "Deriv", "xgxr", "nlmixr2plot"),
                 repos = getOption("repos")[["MPN"]])
pkgs <- c(pkgs, "readr")

missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Setup incomplete, missing: ", paste(missing, collapse = ", "))
}
cat("pmx-harness R environment ready (MPN snapshot ", MPN_SNAPSHOT, ")\n", sep = "")
