# Purpose: build pilot-level ATR mover datasets merged with BLS tax variables for airline mean and median cases.
# Inputs: `data/derived/main_us_pilots_atr_mover_panel.csv` and `data/derived/all_years_pit_bls.csv`
# Outputs: `data/derived/pilots_atr_tax_merged_bls_airline_mean.csv` and `data/derived/pilots_atr_tax_merged_bls_airline_median.csv`

# Setup ----

source("code/00_setup/00_packages_paths.R")
source("code/utils/cleaning_helpers.R")

build_pilot_tax_case <- function(pilot_type, percentile, max_year = 2022L) {
  case_pilot_type <- pilot_type
  case_percentile <- percentile
  case_max_year <- max_year

  pit_bls_case <- pit_bls |>
    filter(
      pilot_type == case_pilot_type,
      percentile == case_percentile,
      year <= case_max_year
    ) |>
    select("year", "fips", "pilot_type", "percentile", "atr")

  atr_changes <- pit_bls_case |>
    filter(year %in% taxsim_years) |>
    select("year", "fips", "atr") |>
    mutate(atr = round(atr, 4)) |>
    arrange(fips, year) |>
    group_by(fips) |>
    mutate(atr_change = (log(atr) - log(lag(atr))) * 100) |>
    ungroup()

  origin_crosswalk <- state_crosswalk |>
    select("fips", "state") |>
    rename(origin_fips = "fips", origin_state = "state")

  origin_atr_changes <- atr_changes |>
    rename(
      origin_fips = "fips",
      origin_atr = "atr",
      origin_atr_change = "atr_change"
    )

  mover_panel |>
    mutate(dest_fips = fips) |>
    left_join(
      atr_changes |>
        rename(dest_fips = "fips", dest_atr = "atr", dest_atr_change = "atr_change"),
      by = c("year", "dest_fips")
    ) |>
    left_join(origin_crosswalk, by = "origin_state") |>
    left_join(origin_atr_changes, by = c("year", "origin_fips")) |>
    mutate(
      pilot_type = case_pilot_type,
      percentile = case_percentile
    ) |>
    relocate("pilot_type", "percentile", .after = "year")
}

# Inputs ----

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

state_crosswalk <- load_state_fips_crosswalk(paths)

# Derived Datasets ----

pilots_atr_tax_merged_bls_airline_mean <- build_pilot_tax_case(
  pilot_type = "airline",
  percentile = "mean"
)

pilots_atr_tax_merged_bls_airline_median <- build_pilot_tax_case(
  pilot_type = "airline",
  percentile = "median"
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
