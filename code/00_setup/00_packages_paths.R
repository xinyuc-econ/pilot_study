# Purpose: define shared project paths and constants.
# Inputs: local raw data root at /Users/xinyuc/Documents/pilots/data
# Outputs: none; scripts source this file to access `paths` and shared constants.

# Package Assumptions ----

required_packages <- c(
  "readr",
  "dplyr",
  "stringr",
  "janitor",
  "readxl",
  "ggplot2",
  "tibble",
  "testthat"
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
  raw_tot_working_pop = file.path(raw_data_root, "raw", "tot_working_pop_weights"),
  xwalks = file.path(raw_data_root, "xwalks"),
  intermediate = project_path("data", "intermediate"),
  derived = project_path("data", "derived"),
  figures = project_path("output", "figures"),
  tables = project_path("output", "tables")
)

# Path Validation ----

# Fail early if the expected external folders are not available.
required_directories <- c(
  paths$raw_airmen_data,
  paths$raw_aviationdb_data,
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

sampled_years <- c(2009, 2010, 2011, 2014, 2015, 2016, 2017, 2019, 2022, 2024, 2025)
