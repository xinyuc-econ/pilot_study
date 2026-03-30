# Purpose: produce AFM stock-model regression tables from merged pilot-tax datasets.
# Inputs: `data/derived/aviationdb/afm_stock_dataset_*.csv`
# Outputs: balanced and unbalanced combined AFM stock tables in `output/aviationdb/tables/`

# Setup ----

source("code/00_setup/00_packages_paths.R")

setFixest_nthreads(1)

style_fixest <- style.tex("aer", fontsize = "small")

dir.create(paths$tables_aviationdb, recursive = TRUE, showWarnings = FALSE)

build_case_dataset_path <- function(case_name, panel_variant) {
  if (panel_variant == "balanced") {
    return(file.path(paths$derived_aviationdb, sprintf("afm_stock_dataset_%s.csv", case_name)))
  }

  file.path(paths$derived_aviationdb, sprintf("afm_stock_dataset_%s_%s.csv", case_name, panel_variant))
}

build_panel_label <- function(panel_variant) {
  if (panel_variant == "balanced") {
    return("Balanced panel")
  }

  "Unbalanced panel"
}

# Inputs ----

stock_cases <- tribble(
  ~case_name,
  "bls_airline_mean",
  "bls_airline_median",
  "soi_p90",
  "soi_p95",
  "soi_p99"
)

panel_variants <- c("balanced", "unbalanced")

# 1. Tables ----

walk(
  panel_variants,
  function(panel_variant) {
    case_datasets <- map(
      stock_cases$case_name,
      ~ read_csv(
        build_case_dataset_path(.x, panel_variant),
        show_col_types = FALSE
      ) |>
        mutate(year = as.integer(year))
    )

    names(case_datasets) <- stock_cases$case_name

    regression_models <- map(
      case_datasets,
      ~ feols(
        log_stock_n ~ lnet_atr | year + dest_state,
        data = .x,
        cluster = ~dest_state
      )
    )

    table_path <- if (panel_variant == "balanced") {
      file.path(paths$tables_aviationdb, "afm_stock_combined.tex")
    } else {
      file.path(paths$tables_aviationdb, "afm_stock_combined_unbalanced.tex")
    }

    etable(
      regression_models,
      tex = TRUE,
      replace = TRUE,
      fontsize = "small",
      style.tex = style_fixest,
      cluster = ~dest_state,
      title = paste("AFM stock model results:", build_panel_label(panel_variant)),
      headers = c(
        "BLS airline mean",
        "BLS airline median",
        "SOI p90",
        "SOI p95",
        "SOI p99"
      ),
      file = table_path
    )

    message("Wrote AFM stock table to ", table_path)
  }
)
