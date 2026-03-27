# Purpose: compare migrated mover outputs against a direct legacy-equivalent reconstruction.
# Inputs: `data/derived/main_us_pilots_atr.csv`, `data/derived/main_us_pilots_atr_mover_panel.csv`, and `data/derived/sum_stat_prop_atr_pilots.csv`
# Outputs: console validation messages only

# Setup ----

source("code/00_setup/00_packages_paths.R")

main_us_pilots_atr <- read_csv(
  file.path(paths$derived, "main_us_pilots_atr.csv"),
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
) |>
  mutate(year = as.integer(year))

migrated_mover_panel <- read_csv(
  file.path(paths$derived, "main_us_pilots_atr_mover_panel.csv"),
  show_col_types = FALSE
) |>
  mutate(year = as.integer(year))

sum_stat_prop_atr_pilots <- read_csv(
  file.path(paths$derived, "sum_stat_prop_atr_pilots.csv"),
  show_col_types = FALSE
) |>
  mutate(year = as.integer(year))

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
  arrange(year) |>
  arrange(unique_id) |>
  group_by(unique_id) |>
  rename(dest_state = "state") |>
  mutate(
    origin_state = lag(dest_state),
    moved = ifelse(dest_state != origin_state, 1L, 0L)
  ) |>
  relocate("origin_state", "moved", .after = "dest_state") |>
  ungroup() |>
  group_by(unique_id) |>
  mutate(num_moves = sum(moved, na.rm = TRUE)) |>
  relocate("num_moves", .after = "moved") |>
  ungroup()

legacy_flow_panel <- legacy_mover_panel |>
  group_by(unique_id) |>
  mutate(
    lag_year = lag(year),
    time_period_yrs = year - lag_year
  ) |>
  relocate("lag_year", "time_period_yrs", .after = "year") |>
  filter(!is.na(lag_year)) |>
  ungroup()

migrated_flow_panel <- migrated_mover_panel |>
  arrange(unique_id, year) |>
  group_by(unique_id) |>
  mutate(
    lag_year = lag(year),
    time_period_yrs = as.integer(year - lag_year)
  ) |>
  relocate("lag_year", "time_period_yrs", .after = "year") |>
  ungroup() |>
  filter(!is.na(lag_year))

# Comparisons ----

period_month_map <- tribble(
  ~period_key,   ~time_period,            ~months_between,
  "2009-2010", "11/2009 - 05/2010",  6,
  "2010-2011", "05/2010 - 09/2011", 16,
  "2011-2014", "09/2011 - 09/2014", 36,
  "2014-2015", "09/2014 - 09/2015", 12,
  "2015-2016", "09/2015 - 11/2016", 14,
  "2016-2017", "11/2016 - 09/2017", 10,
  "2017-2019", "06/2017 - 06/2019", 24,
  "2019-2022", "06/2019 - 10/2022", 40,
  "2022-2024", "10/2022 - 09/2024", 23
)

build_mover_period_summary <- function(mover_panel) {
  mover_panel |>
    group_by(year) |>
    summarise(
      n_pilots = n(),
      n_moved = sum(moved, na.rm = TRUE),
      prop_moved = n_moved / n_pilots * 100,
      .groups = "drop"
    ) |>
    mutate(
      lag_year = lag(year),
      period_key = paste(lag_year, year, sep = "-")
    ) |>
    filter(!is.na(lag_year)) |>
    left_join(period_month_map, by = "period_key") |>
    mutate(monthly_prop_moved = prop_moved / months_between) |>
    select("time_period", "n_pilots", "n_moved", "monthly_prop_moved") |>
    rename(
      `Time period` = "time_period",
      `# of pilots` = "n_pilots",
      `# of movers` = "n_moved",
      `Ave. monthly % moved` = "monthly_prop_moved"
    ) |>
    as.data.frame()
}

build_migration_flow_table <- function(data) {
  flow_data <- data |>
    filter(!is.na(origin_state), !is.na(dest_state))

  as.matrix(table(flow_data$origin_state, flow_data$dest_state))
}

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

top10_states <- sum_stat_prop_atr_pilots |>
  group_by(state) |>
  summarise(avg_n_atr_pilots = mean(n_atr_pilots), .groups = "drop") |>
  arrange(desc(avg_n_atr_pilots), state) |>
  slice_head(n = 10L) |>
  pull(state)

migrated_top10_flow <- migrated_flow_panel |>
  filter(origin_state %in% top10_states, dest_state %in% top10_states) |>
  build_migration_flow_table()

legacy_top10_flow <- legacy_flow_panel |>
  filter(origin_state %in% top10_states, dest_state %in% top10_states) |>
  build_migration_flow_table()

if (!identical(dim(migrated_top10_flow), dim(legacy_top10_flow))) {
  stop("Top-10 flow table dimensions do not match the legacy-equivalent reconstruction.", call. = FALSE)
}

if (!identical(sum(migrated_top10_flow), sum(legacy_top10_flow))) {
  stop("Top-10 flow table totals do not match the legacy-equivalent reconstruction.", call. = FALSE)
}

message("Mover validation passed.")
