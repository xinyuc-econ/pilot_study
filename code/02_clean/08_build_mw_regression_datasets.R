# Purpose: build Moretti-Wilson style regression datasets from merged pilot-tax datasets.
# Inputs: `data/derived/pilots_atr_tax_merged_bls_airline_*.csv` and `data/derived/pilots_atr_tax_merged_soi_*.csv`
# Outputs: case-specific balanced and unbalanced MW regression datasets in `data/derived/`

# Setup ----

source("code/00_setup/00_packages_paths.R")

analysis_periods <- c(
  "2009-2010",
  "2010-2011",
  "2011-2014",
  "2014-2015",
  "2015-2016",
  "2016-2017",
  "2017-2019",
  "2019-2022"
)

build_mw_regression_dataset <- function(pilot_tax_merged, case_name, panel_variant) {
  filtered_panel <- pilot_tax_merged |>
    mutate(year = as.integer(year)) |>
    filter(year <= 2022) |>
    group_by(unique_id) |>
    mutate(panel_num_years = n()) |>
    ungroup()

  if (panel_variant == "balanced") {
    filtered_panel <- filtered_panel |>
      filter(panel_num_years == 9)
  }

  new_pilots <- filtered_panel |>
    arrange(unique_id, year) |>
    group_by(unique_id) |>
    mutate(base_year = lag(year)) |>
    relocate("base_year", .before = "year") |>
    ungroup() |>
    select(
      "base_year",
      "year",
      "unique_id",
      "first_name",
      "last_name",
      "origin_state",
      "origin_fips",
      "dest_state",
      "moved",
      "panel_num_years"
    )

  migration_flows <- new_pilots |>
    group_by(base_year, year, origin_state, dest_state) |>
    summarise(num_od = n(), .groups = "drop") |>
    mutate(time_period = paste(base_year, year, sep = "-")) |>
    filter(time_period %in% analysis_periods) |>
    select(-"time_period") |>
    mutate(moved = if_else(origin_state != dest_state, 1L, 0L))

  moved_flows <- migration_flows |>
    filter(moved == 1) |>
    select(-"moved")

  stayed_flows <- migration_flows |>
    filter(moved == 0) |>
    select(-"moved", -"dest_state") |>
    rename(num_oo = "num_od")

  origin_tax <- filtered_panel |>
    select("year", "origin_state", "origin_atr") |>
    distinct()

  dest_tax <- filtered_panel |>
    select("year", "dest_state", "dest_atr") |>
    distinct()

  regression_panel <- moved_flows |>
    left_join(stayed_flows, by = c("base_year", "year", "origin_state")) |>
    left_join(
      origin_tax,
      by = c("year", "origin_state")
    ) |>
    left_join(
      dest_tax,
      by = c("year", "dest_state")
    ) |>
    mutate(
      case_name = case_name,
      panel_variant = panel_variant,
      lodds_ratio = log(num_od / num_oo),
      lnet_tax_origin = log(1 - origin_atr),
      lnet_tax_dest = log(1 - dest_atr),
      lnet_tax_diff = lnet_tax_dest - lnet_tax_origin
    ) |>
    filter(
      origin_state != "AK",
      origin_state != "HI",
      dest_state != "AK",
      dest_state != "HI"
    ) |>
    relocate("case_name", "panel_variant", .before = "base_year")

  regression_panel
}

build_case_filename <- function(case_name, panel_variant) {
  if (panel_variant == "balanced") {
    return(sprintf("mw_regression_dataset_%s.csv", case_name))
  }

  sprintf("mw_regression_dataset_%s_%s.csv", case_name, panel_variant)
}

# Inputs ----

mw_cases <- tribble(
  ~case_name, ~input_file,
  "bls_airline_mean", "pilots_atr_tax_merged_bls_airline_mean.csv",
  "bls_airline_median", "pilots_atr_tax_merged_bls_airline_median.csv",
  "soi_p90", "pilots_atr_tax_merged_soi_p90.csv",
  "soi_p95", "pilots_atr_tax_merged_soi_p95.csv",
  "soi_p99", "pilots_atr_tax_merged_soi_p99.csv"
)

panel_variants <- c("balanced", "unbalanced")

mw_case_variants <- expand.grid(
  case_index = seq_len(nrow(mw_cases)),
  panel_variant = panel_variants,
  stringsAsFactors = FALSE
) |>
  as_tibble() |>
  mutate(
    case_name = mw_cases$case_name[case_index],
    input_file = mw_cases$input_file[case_index]
  ) |>
  select("case_name", "input_file", "panel_variant")

# 1. Derived Datasets ----

case_output_paths <- map_chr(
  seq_len(nrow(mw_case_variants)),
  ~ file.path(
    paths$derived,
    build_case_filename(
      mw_case_variants$case_name[[.x]],
      mw_case_variants$panel_variant[[.x]]
    )
  )
)

case_datasets <- pmap(
  mw_case_variants,
  function(case_name, input_file, panel_variant) {
    read_csv(
      file.path(paths$derived, input_file),
      show_col_types = FALSE
    ) |>
      build_mw_regression_dataset(
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
    message("Wrote MW regression dataset to ", path)
    message("Rows: ", nrow(data))
  }
)
