# Purpose: reproduce the main legacy LPM outputs for BLS and SOI tax-measure cases.
# Inputs: `data/derived/pilots_atr_tax_merged_bls_airline_mean.csv` and `data/derived/pilots_atr_tax_merged_soi_*.csv`
# Outputs: one BLS airline-mean LPM table and SOI p90/p95/p99 regression tables in `output/tables/`

# Setup ----

source("code/00_setup/00_packages_paths.R")

setFixest_nthreads(1)

BLS_airline_mean <- read_csv(
  file.path(paths$derived, "pilots_atr_tax_merged_bls_airline_mean.csv"),
  show_col_types = FALSE
) |>
  mutate(year = as.integer(year))

bls_table_path <- file.path(paths$tables, "lpm_bls_airline_mean.tex")
soi_percentiles <- c("p90", "p95", "p99")

write_soi_lpm_table <- function(percentile) {
  case_percentile <- percentile
  input_path <- file.path(paths$derived, sprintf("pilots_atr_tax_merged_soi_%s.csv", case_percentile))
  table_path <- file.path(paths$tables, sprintf("lpm_soi_%s.tex", case_percentile))

  pilot_tax_merged <- read_csv(
    input_path,
    show_col_types = FALSE
  ) |>
    mutate(year = as.integer(year))

  # Keep the SOI sample rule visible here because it matches the BLS main LPM specification.
  lpm_sample <- pilot_tax_merged |>
    filter(year != 2024) |>
    group_by(unique_id) |>
    mutate(num_years = n()) |>
    ungroup() |>
    filter(!is.na(origin_fips), num_years == 9) |>
    filter(
      origin_state != "AK",
      origin_state != "HI",
      dest_state != "AK",
      dest_state != "HI"
    ) |>
    mutate(net_origin_atr = (1 - origin_atr) * 100)

  o1 <- feols(
    moved ~ net_origin_atr | year + origin_state + unique_id,
    data = lpm_sample,
    cluster = ~origin_state + unique_id
  )

  o2 <- feols(
    moved ~ net_origin_atr | year + origin_state,
    data = lpm_sample,
    cluster = ~origin_state + unique_id
  )

  etable(
    o2,
    o1,
    tex = TRUE,
    replace = TRUE,
    fontsize = "small",
    cluster = ~origin_state + unique_id,
    file = table_path
  )

  table_path
}

# Derived Analysis Inputs ----

# Keep the legacy sample rule visible here because it is part of the regression specification.
lpm_sample <- BLS_airline_mean |>
  filter(year != 2024) |>
  group_by(unique_id) |>
  mutate(num_years = n()) |>
  ungroup() |>
  filter(!is.na(origin_fips), num_years == 9) |>
  filter(
    origin_state != "AK",
    origin_state != "HI",
    dest_state != "AK",
    dest_state != "HI"
  ) |>
  mutate(net_origin_atr = (1 - origin_atr) * 100)

o1 <- feols(
  moved ~ net_origin_atr | year + origin_state + unique_id,
  data = lpm_sample,
  cluster = ~origin_state + unique_id
)

o2 <- feols(
  moved ~ net_origin_atr | year + origin_state,
  data = lpm_sample,
  cluster = ~origin_state + unique_id
)

# 1. Tables ----

## 1.1 Main LPM Table ----

etable(
  o2,
  o1,
  tex = TRUE,
  replace = TRUE,
  fontsize = "small",
  cluster = ~origin_state + unique_id,
  file = bls_table_path
)

## 1.2 Main SOI LPM Tables ----

soi_table_paths <- map_chr(soi_percentiles, write_soi_lpm_table)

# Reporting ----

message("Wrote BLS LPM table to ", bls_table_path)
walk(soi_table_paths, ~ message("Wrote SOI LPM table to ", .x))
