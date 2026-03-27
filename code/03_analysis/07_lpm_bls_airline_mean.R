# Purpose: reproduce the main legacy linear probability table using the BLS airline mean pilot-tax merged dataset.
# Inputs: `data/derived/pilots_atr_tax_merged_bls_airline_mean.csv`
# Outputs: `output/tables/lpm_bls_airline_mean.tex`

# Setup ----

source("code/00_setup/00_packages_paths.R")

setFixest_nthreads(1)

pilot_tax_merged <- read_csv(
  file.path(paths$derived, "pilots_atr_tax_merged_bls_airline_mean.csv"),
  show_col_types = FALSE
) |>
  mutate(year = as.integer(year))

table_path <- file.path(paths$tables, "lpm_bls_airline_mean.tex")

# Derived Analysis Inputs ----

# Keep the legacy sample rule visible here because it is part of the regression specification.
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

# 1. Tables ----

## 1.1 Main LPM Table ----

etable(
  o2,
  o1,
  tex = TRUE,
  replace = TRUE,
  fontsize = "small",
  cluster = ~origin_state + unique_id,
  file = table_path
)

# Reporting ----

message("Wrote LPM table to ", table_path)
