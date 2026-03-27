# Purpose: compare the migrated airline-mean pilot-tax merged dataset against a legacy-equivalent reconstruction.
# Inputs: `data/derived/main_us_pilots_atr_mover_panel.csv`, `data/derived/all_years_pit_bls.csv`, and `data/derived/pilots_atr_tax_merged_bls_airline_mean.csv`
# Outputs: console validation messages only

# Setup ----

source("code/00_setup/00_packages_paths.R")
source("code/utils/cleaning_helpers.R")

mover_panel <- read_csv(
  file.path(paths$derived, "main_us_pilots_atr_mover_panel.csv"),
  show_col_types = FALSE
) |>
  mutate(year = as.integer(year))

pit_bls <- read_csv(
  file.path(paths$derived, "all_years_pit_bls.csv"),
  show_col_types = FALSE
) |>
  mutate(year = as.integer(year))

migrated_dataset <- read_csv(
  file.path(paths$derived, "pilots_atr_tax_merged_bls_airline_mean.csv"),
  show_col_types = FALSE
) |>
  mutate(year = as.integer(year))

state_crosswalk <- load_state_fips_crosswalk(paths)

# Legacy-Equivalent Reconstruction ----

xwalk <- state_crosswalk |>
  select("fips", "state") |>
  rename(origin_state = "state", origin_fips = "fips")

pit <- pit_bls |>
  filter(percentile == "mean", pilot_type == "airline") |>
  select("year", "fips", "atr") |>
  mutate(year = as.integer(year))

atr_changes <- pit |>
  filter(year %in% taxsim_years) |>
  select("year", "fips", "atr") |>
  mutate(atr = round(atr, 4)) |>
  arrange(fips, year) |>
  group_by(fips) |>
  mutate(atr_change = (log(atr) - log(lag(atr))) * 100) |>
  ungroup()

origin_atr_changes <- atr_changes |>
  rename(
    origin_fips = "fips",
    origin_atr = "atr",
    origin_atr_change = "atr_change"
  )

legacy_dataset <- mover_panel |>
  mutate(dest_fips = fips) |>
  left_join(
    atr_changes |>
      rename(dest_fips = "fips", dest_atr = "atr", dest_atr_change = "atr_change"),
    by = c("year", "dest_fips")
  ) |>
  left_join(xwalk, by = "origin_state") |>
  left_join(origin_atr_changes, by = c("year", "origin_fips")) |>
  mutate(
    pilot_type = "airline",
    percentile = "mean"
  ) |>
  relocate("pilot_type", "percentile", .after = "year") |>
  arrange(unique_id, year)

migrated_dataset <- migrated_dataset |>
  arrange(unique_id, year)

if (!identical(nrow(migrated_dataset), nrow(legacy_dataset))) {
  stop("Pilot-tax merged BLS mean dataset row count does not match the legacy-equivalent reconstruction.", call. = FALSE)
}

if (!isTRUE(all.equal(migrated_dataset, legacy_dataset, check.attributes = FALSE))) {
  stop("Pilot-tax merged BLS mean dataset does not match the legacy-equivalent reconstruction.", call. = FALSE)
}

message("Pilot-tax merged BLS validation passed.")
