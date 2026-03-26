# Purpose: compare migrated mover outputs against a direct legacy-equivalent reconstruction.
# Inputs: `data/derived/main_us_pilots_atr.csv`, `data/derived/main_us_pilots_atr_mover_panel.csv`, and `data/derived/sum_stat_prop_atr_pilots.csv`
# Outputs: console validation messages only

# Setup ----

source("code/00_setup/00_packages_paths.R")
source("code/utils/cleaning_helpers.R")

main_us_pilots_atr <- read_csv(
  file.path(paths$derived, "main_us_pilots_atr.csv"),
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
) |>
  mutate(year = as.integer(.data$year))

migrated_mover_panel <- read_csv(
  file.path(paths$derived, "main_us_pilots_atr_mover_panel.csv"),
  show_col_types = FALSE
) |>
  mutate(year = as.integer(.data$year))

sum_stat_prop_atr_pilots <- read_csv(
  file.path(paths$derived, "sum_stat_prop_atr_pilots.csv"),
  show_col_types = FALSE
) |>
  mutate(year = as.integer(.data$year))

# Legacy-Equivalent Reconstruction ----

legacy_subset <- main_us_pilots_atr |>
  select(
    "year",
    "fips",
    "statefull",
    "state",
    "unique_id",
    "first_name",
    "last_name",
    "street_1",
    "city",
    "zip_code",
    "num_years"
  )

legacy_mover_panel <- legacy_subset |>
  arrange(.data$year) |>
  arrange(.data$unique_id) |>
  group_by(.data$unique_id) |>
  rename(dest_state = "state") |>
  mutate(
    origin_state = lag(.data$dest_state),
    moved = ifelse(.data$dest_state != .data$origin_state, 1L, 0L)
  ) |>
  relocate("origin_state", "moved", .after = "dest_state") |>
  ungroup() |>
  group_by(.data$unique_id) |>
  mutate(num_moves = sum(.data$moved, na.rm = TRUE)) |>
  relocate("num_moves", .after = "moved") |>
  ungroup()

legacy_flow_panel <- legacy_mover_panel |>
  group_by(.data$unique_id) |>
  mutate(
    lag_year = lag(.data$year),
    time_period_yrs = .data$year - .data$lag_year
  ) |>
  relocate("lag_year", "time_period_yrs", .after = "year") |>
  filter(!is.na(.data$lag_year)) |>
  ungroup()

migrated_flow_panel <- migrated_mover_panel |>
  add_flow_time_fields() |>
  filter(!is.na(.data$lag_year))

# Comparisons ----

migrated_period_summary <- build_mover_period_summary(migrated_mover_panel)
legacy_period_summary <- build_mover_period_summary(legacy_mover_panel)

if (!isTRUE(all.equal(migrated_period_summary, legacy_period_summary, check.attributes = FALSE))) {
  stop("Migrated mover period summary does not match the legacy-equivalent reconstruction.", call. = FALSE)
}

migrated_all_states_flow <- build_migration_flow_table(migrated_flow_panel)
legacy_all_states_flow <- build_migration_flow_table(legacy_flow_panel)

if (!identical(dim(migrated_all_states_flow), dim(legacy_all_states_flow))) {
  stop("All-state flow table dimensions do not match the legacy-equivalent reconstruction.", call. = FALSE)
}

if (!identical(sum(migrated_all_states_flow), sum(legacy_all_states_flow))) {
  stop("All-state flow table totals do not match the legacy-equivalent reconstruction.", call. = FALSE)
}

top10_states <- select_top_states_by_average_atr_count(sum_stat_prop_atr_pilots, n_states = 10L)

migrated_top10_flow <- migrated_flow_panel |>
  filter(.data$origin_state %in% top10_states, .data$dest_state %in% top10_states) |>
  build_migration_flow_table()

legacy_top10_flow <- legacy_flow_panel |>
  filter(.data$origin_state %in% top10_states, .data$dest_state %in% top10_states) |>
  build_migration_flow_table()

if (!identical(dim(migrated_top10_flow), dim(legacy_top10_flow))) {
  stop("Top-10 flow table dimensions do not match the legacy-equivalent reconstruction.", call. = FALSE)
}

if (!identical(sum(migrated_top10_flow), sum(legacy_top10_flow))) {
  stop("Top-10 flow table totals do not match the legacy-equivalent reconstruction.", call. = FALSE)
}

message("Mover validation passed.")
