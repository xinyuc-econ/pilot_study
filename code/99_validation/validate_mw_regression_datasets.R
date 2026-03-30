# Purpose: validate annual MW regression datasets.
# Inputs: annual balanced and unbalanced `data/derived/aviationdb/mw_regression_dataset_bls_airline_mean*.csv`
# Outputs: console validation messages only

# Setup ----

source("code/00_setup/00_packages_paths.R")

panel_variants <- c("balanced", "unbalanced")

walk(
  panel_variants,
  function(panel_variant) {
    dataset_path <- if (panel_variant == "balanced") {
      file.path(paths$derived_aviationdb, "mw_regression_dataset_bls_airline_mean.csv")
    } else {
      file.path(paths$derived_aviationdb, "mw_regression_dataset_bls_airline_mean_unbalanced.csv")
    }

    dataset <- read_csv(dataset_path, show_col_types = FALSE) |>
      mutate(
        base_year = as.integer(base_year),
        year = as.integer(year)
      )

    if (any(dataset$year - dataset$base_year != 1, na.rm = TRUE)) {
      stop(paste("MW regression", panel_variant, "dataset contains non-adjacent year pairs."), call. = FALSE)
    }

    if (anyNA(dataset$lodds_ratio) || anyNA(dataset$lnet_tax_diff)) {
      stop(paste("MW regression", panel_variant, "dataset has missing core regression variables."), call. = FALSE)
    }

    if (anyDuplicated(dataset[c("base_year", "year", "origin_state", "dest_state")]) > 0) {
      stop(paste("MW regression", panel_variant, "dataset is not unique by year and OD pair."), call. = FALSE)
    }
  }
)

message("MW regression dataset validation passed.")
