# Purpose: build pilot-level ATR mover datasets merged with BLS tax variables for airline mean and median cases.
# Inputs: `data/derived/main_us_pilots_atr_mover_panel.csv` and `data/derived/all_years_pit_bls.csv`
# Outputs: `data/derived/pilots_atr_tax_merged_bls_airline_mean.csv` and `data/derived/pilots_atr_tax_merged_bls_airline_median.csv`

# Setup ----

source("code/00_setup/00_packages_paths.R")
source("code/utils/cleaning_helpers.R")

# Inputs ----

mover_panel <- read_csv(
  file.path(paths$derived, "main_us_pilots_atr_mover_panel.csv"),
  show_col_types = FALSE
) |>
  mutate(year = as.integer(.data$year))

pit_bls <- read_csv(
  file.path(paths$derived, "all_years_pit_bls.csv"),
  show_col_types = FALSE
) |>
  mutate(year = as.integer(.data$year))

state_crosswalk <- load_state_fips_crosswalk(paths)

# Derived Datasets ----

pilots_atr_tax_merged_bls_airline_mean <- build_pilots_atr_tax_merged_bls(
  mover_panel = mover_panel,
  pit_bls = pit_bls,
  state_crosswalk = state_crosswalk,
  pilot_type = "airline",
  percentile = "mean",
  max_year = 2022L
)

pilots_atr_tax_merged_bls_airline_median <- build_pilots_atr_tax_merged_bls(
  mover_panel = mover_panel,
  pit_bls = pit_bls,
  state_crosswalk = state_crosswalk,
  pilot_type = "airline",
  percentile = "median",
  max_year = 2022L
)

# Outputs ----

mean_output_path <- file.path(paths$derived, "pilots_atr_tax_merged_bls_airline_mean.csv")
median_output_path <- file.path(paths$derived, "pilots_atr_tax_merged_bls_airline_median.csv")

write_csv(pilots_atr_tax_merged_bls_airline_mean, mean_output_path)
write_csv(pilots_atr_tax_merged_bls_airline_median, median_output_path)

# Reporting ----

message("Wrote BLS airline mean pilot-tax merged dataset to ", mean_output_path)
message("Rows: ", nrow(pilots_atr_tax_merged_bls_airline_mean))
message("Wrote BLS airline median pilot-tax merged dataset to ", median_output_path)
message("Rows: ", nrow(pilots_atr_tax_merged_bls_airline_median))
