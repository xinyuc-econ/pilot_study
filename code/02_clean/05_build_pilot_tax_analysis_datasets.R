# Purpose: build merged pilot-tax analysis datasets for SOI and BLS robustness cases.
# Inputs: `data/derived/sum_stat_prop_atr_pilots.csv`, `data/derived/all_years_pit_soi_wide.csv`, and `data/derived/all_years_pit_bls.csv`
# Outputs: `data/derived/pilot_tax_analysis_soi.csv` and `data/derived/pilot_tax_analysis_bls.csv`

# Setup ----

source("code/00_setup/00_packages_paths.R")
source("code/utils/cleaning_helpers.R")

prop_pilots <- readr::read_csv(
  file.path(paths$derived, "sum_stat_prop_atr_pilots.csv"),
  show_col_types = FALSE
) |>
  dplyr::filter(.data$year >= 2009, .data$year <= 2022)

pit_soi_wide <- readr::read_csv(
  file.path(paths$derived, "all_years_pit_soi_wide.csv"),
  show_col_types = FALSE
)

pit_bls <- readr::read_csv(
  file.path(paths$derived, "all_years_pit_bls.csv"),
  show_col_types = FALSE
)

state_crosswalk <- load_state_fips_crosswalk(paths)

# Dataset Builders ----

pilot_tax_analysis_soi <- build_soi_pilot_tax_analysis(
  prop_pilots = prop_pilots,
  pit_soi_wide = pit_soi_wide,
  state_crosswalk = state_crosswalk
)

pilot_tax_analysis_bls <- build_bls_pilot_tax_analysis(
  prop_pilots = prop_pilots,
  pit_bls = pit_bls,
  state_crosswalk = state_crosswalk
)

# Outputs ----

readr::write_csv(
  pilot_tax_analysis_soi,
  file.path(paths$derived, "pilot_tax_analysis_soi.csv")
)

readr::write_csv(
  pilot_tax_analysis_bls,
  file.path(paths$derived, "pilot_tax_analysis_bls.csv")
)

# Reporting ----

message("Wrote SOI pilot-tax analysis dataset to ", file.path(paths$derived, "pilot_tax_analysis_soi.csv"))
message("Wrote BLS pilot-tax analysis dataset to ", file.path(paths$derived, "pilot_tax_analysis_bls.csv"))
