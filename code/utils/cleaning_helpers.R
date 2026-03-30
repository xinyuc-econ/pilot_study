# Purpose: shared cleaning helpers used by more than one production script.

load_state_fips_crosswalk <- function(paths) {
  read_excel(file.path(paths$xwalks, "StateFIPSicsprAB.xls")) |>
    clean_names() |>
    select("fips", "name", "ab") |>
    rename(
      statefull = "name",
      state = "ab"
    )
}

load_total_working_population <- function(paths) {
  csv_files <- list.files(
    paths$raw_tot_working_pop,
    pattern = "tot_working_pop\\.csv$",
    full.names = TRUE
  )

  read_csv(
    csv_files,
    id = "file",
    show_col_types = FALSE,
    col_types = cols(.default = col_guess())
  ) |>
    mutate(year = as.integer(str_extract(file, "\\d{4}"))) |>
    select(-"file") |>
    rename(state = "statefips") |>
    mutate(state = trimws(as.character(state)))
}

build_adjacent_year_panel <- function(data) {
  data |>
    arrange(unique_id, year) |>
    group_by(unique_id) |>
    mutate(
      lag_year = lag(year),
      origin_state = lag(state),
      is_adjacent_year = !is.na(lag_year) & (year - lag_year == 1L),
      moved = if_else(is_adjacent_year & state != origin_state, 1L, 0L, missing = 0L)
    ) |>
    ungroup()
}
