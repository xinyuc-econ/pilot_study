# Purpose: build AFM stock-analysis datasets from merged pilot-tax datasets.
# Inputs: `data/derived/pilots_atr_tax_merged_bls_airline_*.csv` and `data/derived/pilots_atr_tax_merged_soi_*.csv`
# Outputs: case-specific balanced and unbalanced AFM stock datasets in `data/derived/`

# Setup ----

source("code/00_setup/00_packages_paths.R")

analysis_years <- c(2009, 2010, 2011, 2014, 2015, 2016, 2017, 2019, 2022)
all_states <- c(state.abb, "DC")
pivot_state <- "CA"

build_afm_stock_dataset <- function(pilot_tax_merged, case_name, panel_variant) {
  pilots <- pilot_tax_merged |>
    mutate(year = as.integer(year)) |>
    filter(year %in% analysis_years) |>
    group_by(unique_id) |>
    mutate(panel_num_years = n()) |>
    ungroup()

  ordered_pairs <- expand.grid(
    origin_state = all_states,
    dest_state = all_states,
    stringsAsFactors = FALSE
  ) |>
    as_tibble()

  year_state_pair <- expand.grid(
    year = analysis_years,
    pair_index = seq_len(nrow(ordered_pairs)),
    stringsAsFactors = FALSE
  ) |>
    as_tibble() |>
    mutate(
      origin_state = ordered_pairs$origin_state[pair_index],
      dest_state = ordered_pairs$dest_state[pair_index]
    ) |>
    select("year", "origin_state", "dest_state")

  dest_pop <- pilots |>
    group_by(year, dest_state) |>
    count(name = "dest_n") |>
    ungroup()

  origin_pop <- dest_pop |>
    rename(origin_state = "dest_state", origin_n = "dest_n")

  balanced_dest_pop <- pilots |>
    filter(panel_num_years == 9) |>
    group_by(year, dest_state) |>
    count(name = "b_dest_n") |>
    ungroup()

  balanced_origin_pop <- balanced_dest_pop |>
    rename(origin_state = "dest_state", b_origin_n = "b_dest_n")

  pop_d <- year_state_pair |>
    left_join(origin_pop, by = c("year", "origin_state")) |>
    left_join(dest_pop, by = c("year", "dest_state")) |>
    left_join(balanced_origin_pop, by = c("year", "origin_state")) |>
    left_join(balanced_dest_pop, by = c("year", "dest_state")) |>
    filter(origin_state == pivot_state)

  dest_tax <- pilots |>
    select("year", "dest_state", "dest_atr") |>
    distinct()

  reg_d <- dest_tax |>
    left_join(pop_d, by = c("year", "dest_state")) |>
    mutate(
      case_name = case_name,
      panel_variant = panel_variant,
      lnet_atr = log(1 - dest_atr),
      stock_n = if_else(panel_variant == "balanced", b_dest_n, dest_n),
      log_stock_n = log(stock_n)
    ) |>
    filter(
      origin_state != "AK",
      origin_state != "HI",
      dest_state != "AK",
      dest_state != "HI"
    ) |>
    distinct(year, dest_state, .keep_all = TRUE) |>
    relocate("case_name", "panel_variant", .before = "year")

  reg_d
}

build_case_filename <- function(case_name, panel_variant) {
  if (panel_variant == "balanced") {
    return(sprintf("afm_stock_dataset_%s.csv", case_name))
  }

  sprintf("afm_stock_dataset_%s_%s.csv", case_name, panel_variant)
}

# Inputs ----

stock_cases <- tribble(
  ~case_name, ~input_file,
  "bls_airline_mean", "pilots_atr_tax_merged_bls_airline_mean.csv",
  "bls_airline_median", "pilots_atr_tax_merged_bls_airline_median.csv",
  "soi_p90", "pilots_atr_tax_merged_soi_p90.csv",
  "soi_p95", "pilots_atr_tax_merged_soi_p95.csv",
  "soi_p99", "pilots_atr_tax_merged_soi_p99.csv"
)

panel_variants <- c("balanced", "unbalanced")

stock_case_variants <- expand.grid(
  case_index = seq_len(nrow(stock_cases)),
  panel_variant = panel_variants,
  stringsAsFactors = FALSE
) |>
  as_tibble() |>
  mutate(
    case_name = stock_cases$case_name[case_index],
    input_file = stock_cases$input_file[case_index]
  ) |>
  select("case_name", "input_file", "panel_variant")

# 1. Derived Datasets ----

case_output_paths <- map_chr(
  seq_len(nrow(stock_case_variants)),
  ~ file.path(
    paths$derived,
    build_case_filename(
      stock_case_variants$case_name[[.x]],
      stock_case_variants$panel_variant[[.x]]
    )
  )
)

case_datasets <- pmap(
  stock_case_variants,
  function(case_name, input_file, panel_variant) {
    read_csv(
      file.path(paths$derived, input_file),
      show_col_types = FALSE
    ) |>
      build_afm_stock_dataset(
        case_name = case_name,
        panel_variant = panel_variant
      )
  }
)

walk2(case_datasets, case_output_paths, write_csv)

# Reporting ----

walk2(
  case_output_paths,
  case_datasets,
  function(path, data) {
    message("Wrote AFM stock dataset to ", path)
    message("Rows: ", nrow(data))
  }
)
