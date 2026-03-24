# Purpose: helper functions for FAA-based pilot cleaning.

# Crosswalk and Label Helpers ----

load_state_fips_crosswalk <- function(paths) {
  readxl::read_excel(file.path(paths$xwalks, "StateFIPSicsprAB.xls")) |>
    janitor::clean_names() |>
    dplyr::select(.data$fips, .data$name, .data$ab) |>
    dplyr::rename(
      statefull = .data$name,
      state = .data$ab
    )
}

collapse_certificate_level <- function(level) {
  dplyr::case_when(
    level == "A" ~ "ATR",
    level == "C" ~ "C",
    TRUE ~ "other"
  )
}

# Sample Restriction Helpers ----

# These flags mirror the legacy logic for separating never-US and migrate-in/out pilots.
add_us_migration_flags <- function(data) {
  data |>
    dplyr::filter(!is.na(.data$country)) |>
    dplyr::group_by(.data$unique_id) |>
    dplyr::mutate(
      num_years = dplyr::n_distinct(.data$year),
      num_USA = sum(.data$country == "USA"),
      never_in_US = .data$num_USA == 0,
      migrate_in_out_US = (.data$num_USA > 0) & (.data$num_USA < .data$num_years)
    ) |>
    dplyr::ungroup()
}

# This restriction keeps pilots observed only in the 50 states plus DC across years.
restrict_always_in_main_us <- function(data, excluded_territories) {
  ever_in_us <- data |>
    dplyr::filter(!.data$never_in_US)

  always_in_us <- ever_in_us |>
    dplyr::filter(!.data$migrate_in_out_US)

  always_in_us |>
    dplyr::group_by(.data$unique_id) |>
    dplyr::mutate(
      num_not_in_main_US = sum(.data$state %in% excluded_territories),
      always_in_main_US = .data$num_not_in_main_US == 0,
      both_main_other_US = (.data$num_not_in_main_US > 0) &
        (.data$num_years > .data$num_not_in_main_US)
    ) |>
    dplyr::filter(.data$always_in_main_US) |>
    dplyr::ungroup() |>
    dplyr::select(-.data$num_not_in_main_US, -.data$always_in_main_US, -.data$both_main_other_US)
}

# Derived Dataset Builders ----

build_main_us_pilots_any <- function(data, state_fips_crosswalk) {
  state_fips_crosswalk |>
    dplyr::left_join(data, by = "state") |>
    dplyr::select(-dplyr::all_of(c("num_USA", "never_in_US", "migrate_in_out_US"))) |>
    dplyr::relocate(.data$year) |>
    dplyr::mutate(level_collapsed = collapse_certificate_level(.data$level))
}

build_main_us_pilots_atr <- function(main_us_pilots_any) {
  main_us_pilots_any |>
    dplyr::filter(.data$level_collapsed == "ATR") |>
    dplyr::select(-.data$level_collapsed, -.data$level, -.data$expire_date)
}

# Working Population Inputs ----

# The working-population files are already organized as one CSV per year.
load_tot_working_population <- function(paths) {
  csv_files <- list.files(
    paths$raw_tot_working_pop,
    pattern = "tot_working_pop\\.csv$",
    full.names = TRUE
  )

  readr::read_csv(
    csv_files,
    id = "file",
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_guess())
  ) |>
    dplyr::mutate(year = as.numeric(stringr::str_extract(.data$file, "\\d{4}"))) |>
    dplyr::select(-.data$file) |>
    dplyr::rename(state = .data$statefips)
}

# This matches the legacy ATR share summary statistic by state-year.
build_sum_stat_prop_pilots <- function(main_us_pilots_atr, tot_working_pop) {
  main_us_pilots_atr |>
    dplyr::count(.data$year, .data$state, name = "n_atr_pilots") |>
    dplyr::left_join(tot_working_pop, by = c("year", "state")) |>
    dplyr::mutate(prop_atr_pilots = .data$n_atr_pilots / .data$tot_work_pop * 100)
}
