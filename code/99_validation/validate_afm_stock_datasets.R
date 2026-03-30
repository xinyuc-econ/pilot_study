# Purpose: validate annual AFM stock datasets.
# Inputs: annual balanced and unbalanced `data/derived/aviationdb/afm_stock_dataset_bls_airline_mean*.csv`
# Outputs: console validation messages only

# Setup ----

source("code/00_setup/00_packages_paths.R")

panel_variants <- c("balanced", "unbalanced")

walk(
  panel_variants,
  function(panel_variant) {
    dataset_path <- if (panel_variant == "balanced") {
      file.path(paths$derived_aviationdb, "afm_stock_dataset_bls_airline_mean.csv")
    } else {
      file.path(paths$derived_aviationdb, "afm_stock_dataset_bls_airline_mean_unbalanced.csv")
    }

    dataset <- read_csv(dataset_path, show_col_types = FALSE) |>
      mutate(year = as.integer(year))

    if (anyNA(dataset$log_stock_n) || anyNA(dataset$lnet_atr)) {
      stop(paste("AFM stock", panel_variant, "dataset has missing stock or tax variables."), call. = FALSE)
    }

    if (anyDuplicated(dataset[c("year", "dest_state")]) > 0) {
      stop(paste("AFM stock", panel_variant, "dataset is not unique by year and destination state."), call. = FALSE)
    }
  }
)

message("AFM stock dataset validation passed.")
