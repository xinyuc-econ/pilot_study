# Purpose: compare the migrated BLS airline-mean MW regression datasets against legacy-equivalent reconstructions.
# Inputs: `data/derived/pilots_atr_tax_merged_bls_airline_mean.csv` and balanced/unbalanced `data/derived/mw_regression_dataset_bls_airline_mean*.csv`
# Outputs: console validation messages only

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

  moved_flows |>
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
}

# Inputs ----

pilot_tax_merged <- read_csv(
  file.path(paths$derived, "pilots_atr_tax_merged_bls_airline_mean.csv"),
  show_col_types = FALSE
)

panel_variants <- c("balanced", "unbalanced")

walk(
  panel_variants,
  function(panel_variant) {
    migrated_path <- if (panel_variant == "balanced") {
      file.path(paths$derived, "mw_regression_dataset_bls_airline_mean.csv")
    } else {
      file.path(paths$derived, "mw_regression_dataset_bls_airline_mean_unbalanced.csv")
    }

    migrated_dataset <- read_csv(
      migrated_path,
      show_col_types = FALSE
    ) |>
      mutate(
        base_year = as.integer(base_year),
        year = as.integer(year)
      ) |>
      arrange(base_year, year, origin_state, dest_state)

    legacy_dataset <- build_mw_regression_dataset(
      pilot_tax_merged = pilot_tax_merged,
      case_name = "bls_airline_mean",
      panel_variant = panel_variant
    ) |>
      mutate(
        base_year = as.integer(base_year),
        year = as.integer(year)
      ) |>
      arrange(base_year, year, origin_state, dest_state)

    if (!identical(nrow(migrated_dataset), nrow(legacy_dataset))) {
      stop(
        paste("MW regression", panel_variant, "dataset row count does not match the legacy-equivalent reconstruction."),
        call. = FALSE
      )
    }

    if (!isTRUE(all.equal(migrated_dataset, legacy_dataset, check.attributes = FALSE))) {
      stop(
        paste("MW regression", panel_variant, "dataset does not match the legacy-equivalent reconstruction."),
        call. = FALSE
      )
    }
  }
)

message("MW regression dataset validation passed.")
