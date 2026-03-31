# Purpose: define shared project paths and constants.
# Inputs: local raw data root at /Users/xinyuc/Documents/pilots/data
# Outputs: none; scripts source this file to access `paths` and shared constants.

# Runtime Environment ----

Sys.setenv(
  OMP_NUM_THREADS = "1",
  OPENBLAS_NUM_THREADS = "1",
  MKL_NUM_THREADS = "1"
)

# Package Assumptions ----

required_packages <- c(
  "readr",
  "dplyr",
  "stringr",
  "janitor",
  "readxl",
  "ggplot2",
  "ggrepel",
  "binsreg",
  "usmap",
  "stargazer",
  "sf",
  "ggpattern",
  "patchwork",
  "purrr",
  "tibble",
  "testthat",
  "xtable",
  "fixest"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
  stop(
    paste(
      "The following required R packages are not installed:",
      paste(missing_packages, collapse = ", ")
    ),
    call. = FALSE
  )
}

invisible(
  lapply(
    required_packages,
    function(pkg) {
      suppressPackageStartupMessages(
        library(pkg, character.only = TRUE)
      )
    }
  )
)

# Project Paths ----

project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)

project_path <- function(...) {
  file.path(project_root, ...)
}

# External Data Locations ----

raw_data_root <- "/Users/xinyuc/Documents/pilots/data"

raw_data_root <- normalizePath(raw_data_root, winslash = "/", mustWork = TRUE)

paths <- list(
  project_root = project_root,
  raw_data_root = raw_data_root,
  raw_airmen_data = file.path(raw_data_root, "raw", "airmen_data"),
  raw_aviationdb_data = file.path(raw_data_root, "raw", "aviationdb_data"),
  raw_bls = file.path(raw_data_root, "raw", "bls"),
  raw_soi = file.path(raw_data_root, "raw", "soi"),
  raw_tot_working_pop = file.path(raw_data_root, "raw", "tot_working_pop_weights"),
  xwalks = file.path(raw_data_root, "xwalks"),
  intermediate = project_path("data", "intermediate"),
  derived_aviationdb = project_path("data", "derived", "aviationdb"),
  taxsim_aviationdb = project_path("data", "derived", "aviationdb", "taxsim"),
  taxsim_output_aviationdb = project_path("data", "derived", "aviationdb", "taxsim_output"),
  derived_faa = project_path("data", "derived", "faa"),
  figures_aviationdb = project_path("output", "aviationdb", "figures"),
  tables_aviationdb = project_path("output", "aviationdb", "tables"),
  figures_faa = project_path("output", "faa", "figures"),
  tables_faa = project_path("output", "faa", "tables")
)

# Path Validation ----

# Fail early if the expected external folders are not available.
required_directories <- c(
  paths$raw_airmen_data,
  paths$raw_aviationdb_data,
  paths$raw_bls,
  paths$raw_soi,
  paths$raw_tot_working_pop,
  paths$xwalks
)

missing_directories <- required_directories[!dir.exists(required_directories)]

if (length(missing_directories) > 0) {
  stop(
    paste(
      "Raw data directory structure is incomplete under", raw_data_root,
      "\nMissing directories:",
      paste(missing_directories, collapse = "\n")
    ),
    call. = FALSE
  )
}

# Shared Constants ----

excluded_territories <- c(
  "PR", "VI", "AA", "AE", "GU", "AP", "AS", "MP", "FM", "MH", "PW"
)

analysis_years <- 2001:2022
soi_analysis_years <- analysis_years
bls_analysis_years <- 2007:2022
taxsim_years_soi <- soi_analysis_years
taxsim_years_bls <- bls_analysis_years
