# Purpose: compare the migrated BLS airline-mean AFM stock datasets against legacy-equivalent reconstructions.
# Inputs: `data/derived/pilots_atr_tax_merged_bls_airline_mean.csv` and balanced/unbalanced `data/derived/afm_stock_dataset_bls_airline_mean*.csv`
# Outputs: console validation messages only

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

  dest_tax |>
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
}

# Inputs ----

pilot_tax_merged <- read_csv(
  file.path(paths$derived, "pilots_atr_tax_merged_bls_airline_mean.csv"),
  show_col_types = FALSE
)

panel_variants <- c("balanced", "unbalanced")

# Comparisons ----

walk(
  panel_variants,
  function(panel_variant) {
    migrated_path <- if (panel_variant == "balanced") {
      file.path(paths$derived, "afm_stock_dataset_bls_airline_mean.csv")
    } else {
      file.path(paths$derived, "afm_stock_dataset_bls_airline_mean_unbalanced.csv")
    }

    migrated_dataset <- read_csv(
      migrated_path,
      show_col_types = FALSE
    ) |>
      mutate(year = as.integer(year)) |>
      arrange(year, origin_state, dest_state)

    legacy_dataset <- build_afm_stock_dataset(
      pilot_tax_merged = pilot_tax_merged,
      case_name = "bls_airline_mean",
      panel_variant = panel_variant
    ) |>
      mutate(year = as.integer(year)) |>
      arrange(year, origin_state, dest_state)

    if (!identical(nrow(migrated_dataset), nrow(legacy_dataset))) {
      stop(
        paste("AFM stock", panel_variant, "dataset row count does not match the legacy-equivalent reconstruction."),
        call. = FALSE
      )
    }

    if (!isTRUE(all.equal(migrated_dataset, legacy_dataset, check.attributes = FALSE))) {
      stop(
        paste("AFM stock", panel_variant, "dataset does not match the legacy-equivalent reconstruction."),
        call. = FALSE
      )
    }

    if (nrow(migrated_dataset) != n_distinct(migrated_dataset$year, migrated_dataset$dest_state)) {
      stop(
        paste("AFM stock", panel_variant, "dataset is not unique by year and destination state."),
        call. = FALSE
      )
    }
  }
)

message("AFM stock dataset validation passed.")
