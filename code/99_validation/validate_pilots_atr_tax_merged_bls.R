# Purpose: validate annual BLS pilot-tax merged outputs.
# Inputs: `data/derived/aviationdb/pilots_atr_tax_merged_bls_airline_mean.csv`
# Outputs: console validation messages only

# Setup ----

source("code/00_setup/00_packages_paths.R")

merged_data <- read_csv(
  file.path(paths$derived_aviationdb, "pilots_atr_tax_merged_bls_airline_mean.csv"),
  show_col_types = FALSE
) |>
  mutate(year = as.integer(year))

if (anyNA(merged_data$dest_atr)) {
  stop("BLS merged dataset has missing destination ATR values.", call. = FALSE)
}

if (any(merged_data$is_adjacent_year & is.na(merged_data$origin_atr), na.rm = TRUE)) {
  stop("BLS merged dataset has missing origin ATR values for adjacent-year observations.", call. = FALSE)
}

if (anyDuplicated(merged_data[c("unique_id", "year", "pilot_type", "percentile")]) > 0) {
  stop("BLS merged dataset is not unique by unique_id, year, pilot_type, and percentile.", call. = FALSE)
}

message("BLS pilot-tax merged validation passed.")
