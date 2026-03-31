# Purpose: build pilot-level ATR mover datasets merged with BLS and SOI tax variables.
# Inputs: `data/derived/aviationdb/main_us_pilots_atr_mover_panel.csv`, `data/derived/aviationdb/all_years_pit_bls.csv`, and `data/derived/aviationdb/all_years_pit_soi.csv`
# Outputs: BLS airline mean/median and SOI p90/p95/p99 pilot-tax merged datasets in `data/derived/aviationdb/`

# Setup ----

source("code/00_setup/00_packages_paths.R")
source("code/utils/cleaning_helpers.R")

build_pilot_tax_case <- function(pit_case, percentile, case_years, pilot_type = NULL) {
  case_percentile <- percentile

  atr_changes <- pit_case |>
    filter(year %in% case_years) |>
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
    filter(year %in% case_years) |>
    mutate(dest_fips = fips) |>
    left_join(
      atr_changes |>
        rename(dest_fips = "fips", dest_atr = "atr", dest_atr_change = "atr_change"),
      by = c("year", "dest_fips")
    ) |>
    left_join(origin_crosswalk, by = "origin_state") |>
    left_join(origin_atr_changes, by = c("year", "origin_fips")) |>
    mutate(percentile = case_percentile) |>
    relocate("percentile", .after = "year")
}

add_pilot_type <- function(data, pilot_type) {
  data |>
    mutate(pilot_type = pilot_type) |>
    relocate("pilot_type", .after = "year")
}

# Inputs ----

mover_panel <- read_csv(
  file.path(paths$derived_aviationdb, "main_us_pilots_atr_mover_panel.csv"),
  show_col_types = FALSE
) |>
  mutate(year = as.integer(year))

pit_bls <- read_csv(
  file.path(paths$derived_aviationdb, "all_years_pit_bls.csv"),
  show_col_types = FALSE
) |>
  mutate(year = as.integer(year))

pit_soi <- read_csv(
  file.path(paths$derived_aviationdb, "all_years_pit_soi.csv"),
  show_col_types = FALSE
) |>
  mutate(year = as.integer(year))

state_crosswalk <- load_state_fips_crosswalk(paths)

# Derived Datasets ----

BLS_airline_mean <- pit_bls |>
  filter(pilot_type == "airline", percentile == "mean", year %in% bls_analysis_years) |>
  select("year", "fips", "atr")

BLS_airline_median <- pit_bls |>
  filter(pilot_type == "airline", percentile == "median", year %in% bls_analysis_years) |>
  select("year", "fips", "atr")

SOI_p90 <- pit_soi |>
  filter(percentile == "p90", year %in% soi_analysis_years) |>
  select("year", "fips", "atr")

SOI_p95 <- pit_soi |>
  filter(percentile == "p95", year %in% soi_analysis_years) |>
  select("year", "fips", "atr")

SOI_p99 <- pit_soi |>
  filter(percentile == "p99", year %in% soi_analysis_years) |>
  select("year", "fips", "atr")

pilots_atr_tax_merged_bls_airline_mean <- build_pilot_tax_case(
  pit_case = BLS_airline_mean,
  percentile = "mean",
  case_years = bls_analysis_years
) |>
  add_pilot_type("airline")

pilots_atr_tax_merged_bls_airline_median <- build_pilot_tax_case(
  pit_case = BLS_airline_median,
  percentile = "median",
  case_years = bls_analysis_years
) |>
  add_pilot_type("airline")

pilots_atr_tax_merged_soi_p90 <- build_pilot_tax_case(
  pit_case = SOI_p90,
  percentile = "p90",
  case_years = soi_analysis_years
)

pilots_atr_tax_merged_soi_p95 <- build_pilot_tax_case(
  pit_case = SOI_p95,
  percentile = "p95",
  case_years = soi_analysis_years
)

pilots_atr_tax_merged_soi_p99 <- build_pilot_tax_case(
  pit_case = SOI_p99,
  percentile = "p99",
  case_years = soi_analysis_years
)

# Outputs ----

dir.create(paths$derived_aviationdb, recursive = TRUE, showWarnings = FALSE)

mean_output_path <- file.path(paths$derived_aviationdb, "pilots_atr_tax_merged_bls_airline_mean.csv")
median_output_path <- file.path(paths$derived_aviationdb, "pilots_atr_tax_merged_bls_airline_median.csv")
soi_p90_output_path <- file.path(paths$derived_aviationdb, "pilots_atr_tax_merged_soi_p90.csv")
soi_p95_output_path <- file.path(paths$derived_aviationdb, "pilots_atr_tax_merged_soi_p95.csv")
soi_p99_output_path <- file.path(paths$derived_aviationdb, "pilots_atr_tax_merged_soi_p99.csv")

write_csv(pilots_atr_tax_merged_bls_airline_mean, mean_output_path)
write_csv(pilots_atr_tax_merged_bls_airline_median, median_output_path)
write_csv(pilots_atr_tax_merged_soi_p90, soi_p90_output_path)
write_csv(pilots_atr_tax_merged_soi_p95, soi_p95_output_path)
write_csv(pilots_atr_tax_merged_soi_p99, soi_p99_output_path)

# Reporting ----

message("Wrote BLS airline mean pilot-tax merged dataset to ", mean_output_path)
message("Rows: ", nrow(pilots_atr_tax_merged_bls_airline_mean))
message("Wrote BLS airline median pilot-tax merged dataset to ", median_output_path)
message("Rows: ", nrow(pilots_atr_tax_merged_bls_airline_median))
message("Wrote SOI p90 pilot-tax merged dataset to ", soi_p90_output_path)
message("Rows: ", nrow(pilots_atr_tax_merged_soi_p90))
message("Wrote SOI p95 pilot-tax merged dataset to ", soi_p95_output_path)
message("Rows: ", nrow(pilots_atr_tax_merged_soi_p95))
message("Wrote SOI p99 pilot-tax merged dataset to ", soi_p99_output_path)
message("Rows: ", nrow(pilots_atr_tax_merged_soi_p99))
