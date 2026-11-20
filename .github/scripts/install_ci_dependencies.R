# R packages for synthetic CI; NONMEM/PsN are not provisioned and live engine execution stays disabled.
# Entry point: Rscript .github/scripts/install_ci_dependencies.R full|validated
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || !args %in% c("full", "validated")) {
  stop("Usage: Rscript .github/scripts/install_ci_dependencies.R full|validated")
}
options(timeout = max(600, getOption("timeout")))
mpn <- "https://mpn.metworx.com/snapshots/stable/2025-09-17"
cran <- getOption("repos")
if (!length(cran) || any(cran == "@CRAN@")) stop("Configure the CRAN binary mirror first")
cran <- cran[!names(cran) %in% "MPN"]
repos <- c(MPN = mpn, cran)
cat("R:", R.version.string, "platform:", R.version$platform, "\n")
cat("Repository labels:", paste(names(repos), collapse = ", "), "\n")
pins <- c(readr = "2.1.6", nmrec = "0.5.1", mrgmisc = "0.3.0",
          patchwork = "1.3.2", pmplots = "0.5.1", tidyvpc = "1.6.0",
          mrgsolve = "1.6.1", bbr = "1.14.2")
if (args == "validated") pins <- c(pins, yspec = "0.7.0", pmtables = "0.9.0")
archive_urls <- c(
  readr = "https://cran.r-project.org/src/contrib/Archive/readr/readr_2.1.6.tar.gz",
  tidyvpc = "https://packagemanager.posit.co/cran/2026-10-28/src/contrib/tidyvpc_1.6.0.tar.gz")
archive_sha256 <- c(
  readr = "9d743ba3b0453448ae5dea0582d5d7e5a2ef0489308d04cff7672865540546d9",
  tidyvpc = "6c84c74960566e513e09b2d2032502bdb12fcabcce24b51443d0be6a71cabd2b")
mpn_pins <- setdiff(names(pins), names(archive_urls))
mpn_index <- available.packages(repos = mpn, type = "source")
if (!all(mpn_pins %in% rownames(mpn_index)) ||
    !identical(unname(mpn_index[mpn_pins, "Version"]), unname(pins[mpn_pins]))) {
  stop("The MPN index does not match the exact CI package versions")
}
# Prefer setup-r's compatible CRAN packages for the general dependency closure.
# Override only the explicit MPN pins; keep MPN-only dependencies available.
cran_index <- available.packages(repos = cran, type = "source")
fields <- intersect(colnames(cran_index), colnames(mpn_index))
cran_index <- cran_index[, fields, drop = FALSE]
mpn_index <- mpn_index[, fields, drop = FALSE]
index <- rbind(cran_index, mpn_index[!rownames(mpn_index) %in% rownames(cran_index), , drop = FALSE])
index[mpn_pins, ] <- mpn_index[mpn_pins, ]
base <- c("targets", "yaml", "callr", "tarchetypes", "digest", "quarto", "here",
          "ggplot2", "GGally", "knitr", "remotes", "MASS", "jsonlite",
          "dplyr", "stringr", "doParallel", "foreach", "ggpubr", "table1")
downloads <- tempfile("ci-mpn-")
dir.create(downloads)
# Audit exact archives before the ordinary installer resolves dependencies.
# Supplying their local repository rows keeps every pinned download immutable.
for (p in mpn_pins) {
  archive <- file.path(downloads, paste0(p, "_", pins[[p]], ".tar.gz"))
  url <- paste0(mpn_index[p, "Repository"], "/", basename(archive))
  if (!identical(mpn_index[p, "Repository"], paste0(mpn, "/src/contrib"))) {
    stop("Unexpected MPN source repository: ", p)
  }
  if (download.file(url, archive, mode = "wb") != 0L) stop("Download failed: ", p)
  expected <- mpn_index[p, "MD5sum"]
  if (is.na(expected) || !nzchar(expected) ||
      unname(tools::md5sum(archive)) != expected) stop("MPN checksum mismatch: ", p)
  index[p, "Repository"] <- paste0("file://", normalizePath(downloads))
  cat("MPN source:", p, pins[[p]], url, "MD5", expected, "\n")
}
# Derive each archive's dependency row from the verified archive itself: the current
# CRAN index can describe a different release and dependency closure.
metadata <- file.path(downloads, "metadata")
dir.create(metadata)
for (p in names(archive_urls)) {
  archive <- file.path(downloads, paste0(p, "_", pins[[p]], ".tar.gz"))
  url <- archive_urls[[p]]
  if (download.file(url, archive, mode = "wb") != 0L) stop("Download failed: ", p)
  checksum <- system2("sha256sum", shQuote(archive), stdout = TRUE)
  if (!is.null(attr(checksum, "status")) || length(checksum) != 1L ||
      substr(checksum, 1L, 64L) != archive_sha256[[p]]) stop("Archive SHA256 mismatch: ", p)
  untar(archive, files = paste0(p, "/DESCRIPTION"), exdir = metadata)
  description <- read.dcf(file.path(metadata, p, "DESCRIPTION"))
  if (description[1L, "Package"] != p ||
      description[1L, "Version"] != pins[[p]]) stop("Unexpected archive metadata: ", p)
  index[p, ] <- NA_character_
  description_fields <- intersect(colnames(description), colnames(index))
  index[p, description_fields] <- description[1L, description_fields]
  index[p, "Repository"] <- paste0("file://", normalizePath(downloads))
  index[p, "MD5sum"] <- unname(tools::md5sum(archive))
  cat("Archive source:", p, pins[[p]], url, "SHA256", archive_sha256[[p]], "\n")
}
install_selected <- function(pkgs) {
  withCallingHandlers(
    install.packages(pkgs, repos = repos, available = index, dependencies = NA,
                     type = "source"), warning = function(w) stop(w))
}
install_selected("readr")
check_readr <- function() {
  if (as.character(packageVersion("readr")) != pins[["readr"]] ||
      !"read_table2" %in% getNamespaceExports("readr")) {
    stop("bbr requires the pinned readr and its read_table2 export")
  }
}
# Check the export in a child process so installation cannot leave a stale
# loaded namespace in this installer while dependencies are resolved.
check_script <- tempfile(fileext = ".R")
writeLines(c('stopifnot(as.character(packageVersion("readr")) == "2.1.6")',
             'stopifnot("read_table2" %in% getNamespaceExports("readr"))'), check_script)
if (system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(check_script))) != 0L) {
  stop("Pre-bbr readr compatibility check failed")
}
install_selected(c(base, setdiff(names(pins), "readr")))
# Batch 14 exercises the retained public forest entry points in CI.
forest_commit <- "93275012d2c4f067e843104e54a114f88c9b6e8c"
forest_sha256 <- "d1dc0c6e87567945ea3b2737f32c924fb6f0694e7e54f7bdd735ad4cccb05970"
forest_version <- "1.2.15"
forest_url <- paste0("https://codeload.github.com/pharmetheus/PMXForest/tar.gz/", forest_commit)
forest_archive <- file.path(downloads, paste0("PMXForest_", forest_version, ".tar.gz"))
if (download.file(forest_url, forest_archive, mode = "wb") != 0L) stop("Download failed: PMXForest")
checksum <- system2("sha256sum", shQuote(forest_archive), stdout = TRUE)
if (!is.null(attr(checksum, "status")) || length(checksum) != 1L ||
    substr(checksum, 1L, 64L) != forest_sha256) stop("Archive SHA256 mismatch: PMXForest")
forest_lib <- .libPaths()[1L]
withCallingHandlers(
  install.packages(forest_archive, lib = forest_lib, repos = NULL, type = "source"),
  warning = function(w) stop(w))
required <- unique(c(base, names(pins), "PMXForest"))
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("CI setup incomplete: ", paste(missing, collapse = ", "))
for (p in names(pins)) {
  if (as.character(packageVersion(p)) != pins[[p]]) stop("Final pin drift: ", p)
}
check_readr()
if (!"qpcstats" %in% getNamespaceExports("tidyvpc")) {
  stop("The pinned tidyvpc must export qpcstats")
}
if (!identical(as.character(packageVersion("PMXForest", lib.loc = forest_lib)), forest_version)) {
  stop("Final pin drift: PMXForest")
}
cat("PMXForest source:", forest_version, forest_url, "SHA256", forest_sha256, "\n")
for (p in required) cat("CI package:", p, as.character(packageVersion(p)), find.package(p), "\n")
cat("CI dependency installation verified\n")
