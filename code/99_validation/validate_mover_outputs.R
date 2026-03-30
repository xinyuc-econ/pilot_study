# Purpose: validate annual mover outputs built from the AviationDB production panel.
# Inputs: `data/derived/aviationdb/main_us_pilots_atr.csv`, `data/derived/aviationdb/main_us_pilots_atr_mover_panel.csv`, and `data/derived/aviationdb/sum_stat_prop_atr_pilots.csv`
# Outputs: console validation messages only

# Setup ----

source("code/00_setup/00_packages_paths.R")

main_us_pilots_atr <- read_csv(
  file.path(paths$derived_aviationdb, "main_us_pilots_atr.csv"),
  show_col_types = FALSE
) |>
  mutate(year = as.integer(year))

mover_panel <- read_csv(
  file.path(paths$derived_aviationdb, "main_us_pilots_atr_mover_panel.csv"),
  show_col_types = FALSE
) |>
  mutate(year = as.integer(year))

sum_stat_prop_atr_pilots <- read_csv(
  file.path(paths$derived_aviationdb, "sum_stat_prop_atr_pilots.csv"),
  show_col_types = FALSE
) |>
  mutate(year = as.integer(year))

if (nrow(main_us_pilots_atr) != nrow(mover_panel)) {
  stop("Mover panel row count should match the cleaned ATR panel row count.", call. = FALSE)
}

if (any(mover_panel$is_adjacent_year & mover_panel$year - mover_panel$lag_year != 1, na.rm = TRUE)) {
  stop("Mover panel contains non-adjacent rows flagged as adjacent years.", call. = FALSE)
}

if (any(is.na(sum_stat_prop_atr_pilots$tot_work_pop))) {
  stop("Pilot share dataset has missing total working population values.", call. = FALSE)
}

if (anyDuplicated(sum_stat_prop_atr_pilots[c("year", "state")]) > 0) {
  stop("Pilot share dataset is not unique by year and state.", call. = FALSE)
}

message("Mover output validation passed.")
