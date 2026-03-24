# Purpose: build legacy-compatible cleaned pilot datasets without plots.
# Inputs: FAA-rich pooled panel, state crosswalk, and total working population files
# Outputs: `data/derived/main_us_pilots_any.csv`, `data/derived/main_us_pilots_atr.csv`, and `data/derived/sum_stat_prop_atr_pilots.csv`

source("code/00_setup/00_packages_paths.R")
source("code/utils/cleaning_helpers.R")

input_path <- Sys.getenv(
  "FAA_RICH_INGEST_INPUT",
  unset = file.path(paths$intermediate, "faa_pilot_rich_panel.csv")
)

pilot_data <- readr::read_csv(
  input_path,
  na = c("", "NA"),
  show_col_types = FALSE,
  col_types = readr::cols(.default = readr::col_character())
) |>
  dplyr::mutate(year = as.integer(.data$year)) |>
  dplyr::filter(.data$year != 2025)

processed_pilot_data <- add_us_migration_flags(pilot_data)
always_in_main_us_pilots <- restrict_always_in_main_us(
  processed_pilot_data,
  excluded_territories = excluded_territories
)
state_fips_crosswalk <- load_state_fips_crosswalk(paths)
main_us_pilots_any <- build_main_us_pilots_any(
  always_in_main_us_pilots,
  state_fips_crosswalk
)
main_us_pilots_atr <- build_main_us_pilots_atr(main_us_pilots_any)
tot_working_pop <- load_tot_working_population(paths)
sum_stat_prop_pilots <- build_sum_stat_prop_pilots(main_us_pilots_atr, tot_working_pop)

readr::write_csv(main_us_pilots_any, file.path(paths$derived, "main_us_pilots_any.csv"))
readr::write_csv(main_us_pilots_atr, file.path(paths$derived, "main_us_pilots_atr.csv"))
readr::write_csv(
  sum_stat_prop_pilots,
  file.path(paths$derived, "sum_stat_prop_atr_pilots.csv")
)

message("Wrote cleaned pilot datasets to ", paths$derived)
message("main_us_pilots_any rows: ", nrow(main_us_pilots_any))
message("main_us_pilots_atr rows: ", nrow(main_us_pilots_atr))
message("sum_stat_prop_atr_pilots rows: ", nrow(sum_stat_prop_pilots))
