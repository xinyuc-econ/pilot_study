# Purpose: validate annual SOI pilot-tax merged outputs.
# Inputs: `data/derived/aviationdb/pilots_atr_tax_merged_soi_p95.csv`
# Outputs: console validation messages only

# Setup ----

source("code/00_setup/00_packages_paths.R")

merged_data <- read_csv(
  file.path(paths$derived_aviationdb, "pilots_atr_tax_merged_soi_p95.csv"),
  show_col_types = FALSE
) |>
  mutate(year = as.integer(year))

if (anyNA(merged_data$dest_atr)) {
  stop("SOI merged dataset has missing destination ATR values.", call. = FALSE)
}

if (any(merged_data$is_adjacent_year & is.na(merged_data$origin_atr), na.rm = TRUE)) {
  stop("SOI merged dataset has missing origin ATR values for adjacent-year observations.", call. = FALSE)
}

if (anyDuplicated(merged_data[c("unique_id", "year", "percentile")]) > 0) {
  stop("SOI merged dataset is not unique by unique_id, year, and percentile.", call. = FALSE)
}

message("SOI pilot-tax merged validation passed.")
