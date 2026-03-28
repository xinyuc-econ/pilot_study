# Purpose: produce Moretti-Wilson style regression outputs from merged pilot-tax datasets.
# Inputs: `data/derived/mw_regression_dataset_*.csv`
# Outputs: balanced and unbalanced combined MW regression tables and case-specific binscatter figures

# Setup ----

source("code/00_setup/00_packages_paths.R")

setFixest_nthreads(1)

style_fixest <- style.tex("aer", fontsize = "small")

build_case_dataset_path <- function(case_name, panel_variant) {
  if (panel_variant == "balanced") {
    return(file.path(paths$derived, sprintf("mw_regression_dataset_%s.csv", case_name)))
  }

  file.path(paths$derived, sprintf("mw_regression_dataset_%s_%s.csv", case_name, panel_variant))
}

build_case_label <- function(case_name) {
  case_labels <- c(
    bls_airline_mean = "BLS airline mean",
    bls_airline_median = "BLS airline median",
    soi_p90 = "SOI p90",
    soi_p95 = "SOI p95",
    soi_p99 = "SOI p99"
  )

  unname(case_labels[[case_name]])
}

build_panel_label <- function(panel_variant) {
  if (panel_variant == "balanced") {
    return("Balanced panel")
  }

  "Unbalanced panel"
}

build_binscatter_plot <- function(data, y_var, x_var, title, x_label) {
  bins_output <- suppressWarnings(
    binsreg(
      y = data[[y_var]],
      x = data[[x_var]],
      data = as.data.frame(data),
      polyreg = 1,
      nbins = 50
    )
  )

  bins_output$bins_plot +
    labs(
      title = title,
      x = x_label,
      y = "Migration log odds ratio"
    ) +
    theme(
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 16),
      plot.title = element_text(size = 20)
    )
}

prepare_binscatter_dataset <- function(data) {
  fit_y <- feols(
    lodds_ratio ~ 1 | origin_state^dest_state + year,
    data = data
  )

  fit_origin <- feols(
    lnet_tax_origin ~ 1 | origin_state^dest_state + year,
    data = data
  )

  fit_dest <- feols(
    lnet_tax_dest ~ 1 | origin_state^dest_state + year,
    data = data
  )

  fit_diff <- feols(
    lnet_tax_diff ~ 1 | origin_state^dest_state + year,
    data = data
  )

  data |>
    mutate(
      y_dm = resid(fit_y),
      x_origin_dm = resid(fit_origin),
      x_dest_dm = resid(fit_dest),
      x_diff_dm = resid(fit_diff)
    ) |>
    as.data.frame()
}

write_case_binscatters <- function(case_name, panel_variant) {
  data <- read_csv(
    build_case_dataset_path(case_name, panel_variant),
    show_col_types = FALSE
  ) |>
    mutate(
      base_year = as.integer(base_year),
      year = as.integer(year)
    )

  binscatter_data <- prepare_binscatter_dataset(data)

  origin_plot <- build_binscatter_plot(
    data = binscatter_data,
    y_var = "y_dm",
    x_var = "x_origin_dm",
    title = "Origin State",
    x_label = "Log net-of-tax"
  )

  dest_plot <- build_binscatter_plot(
    data = binscatter_data,
    y_var = "y_dm",
    x_var = "x_dest_dm",
    title = "Destination State",
    x_label = "Log net-of-tax"
  )

  diff_plot <- build_binscatter_plot(
    data = binscatter_data,
    y_var = "y_dm",
    x_var = "x_diff_dm",
    title = "Destination-Origin State Diff.",
    x_label = "Log net-of-tax difference"
  )

  if (panel_variant == "balanced") {
    figure_filename <- sprintf("mw_reg_binscatter_%s.png", case_name)
  } else {
    figure_filename <- sprintf("mw_reg_binscatter_%s_%s.png", case_name, panel_variant)
  }

  figure_path <- file.path(paths$figures, figure_filename)

  ggsave(
    filename = figure_path,
    plot = origin_plot + dest_plot + diff_plot +
      plot_annotation(title = paste(build_case_label(case_name), "-", build_panel_label(panel_variant))),
    width = 21,
    height = 7
  )

  figure_path
}

# Inputs ----

mw_cases <- tribble(
  ~case_name,
  "bls_airline_mean",
  "bls_airline_median",
  "soi_p90",
  "soi_p95",
  "soi_p99"
)

panel_variants <- c("balanced", "unbalanced")
mw_case_variants <- expand.grid(
  case_index = seq_len(nrow(mw_cases)),
  panel_variant = panel_variants,
  stringsAsFactors = FALSE
) |>
  as_tibble() |>
  mutate(case_name = mw_cases$case_name[case_index]) |>
  select("case_name", "panel_variant")

# 1. Tables ----

## 1.1 Combined MW Regression Tables ----

walk(
  panel_variants,
  function(panel_variant) {
    case_datasets <- map(
      mw_cases$case_name,
      ~ read_csv(
        build_case_dataset_path(.x, panel_variant),
        show_col_types = FALSE
      ) |>
        mutate(
          base_year = as.integer(base_year),
          year = as.integer(year)
        )
    )

    names(case_datasets) <- mw_cases$case_name

    regression_models <- map(
      case_datasets,
      ~ feols(
        lodds_ratio ~ lnet_tax_diff | year + origin_state + dest_state + origin_state^dest_state,
        data = .x,
        cluster = ~dest_state^origin_state + origin_state^year + dest_state^year
      )
    )

    regression_table_path <- if (panel_variant == "balanced") {
      file.path(paths$tables, "mw_reg_3way_cluster_combined.tex")
    } else {
      file.path(paths$tables, "mw_reg_3way_cluster_combined_unbalanced.tex")
    }

    etable(
      regression_models,
      tex = TRUE,
      replace = TRUE,
      fontsize = "small",
      style.tex = style_fixest,
      cluster = ~dest_state^origin_state + origin_state^year + dest_state^year,
      title = paste(
        "Moretti and Wilson (2017) Regression Specification Results, 3-way clustering:",
        build_panel_label(panel_variant)
      ),
      headers = c(
        "BLS airline mean",
        "BLS airline median",
        "SOI p90",
        "SOI p95",
        "SOI p99"
      ),
      file = regression_table_path
    )

    message("Wrote combined MW regression table to ", regression_table_path)
  }
)

## 1.2 Case-Specific MW Binscatters ----

binscatter_paths <- pmap_chr(
  mw_case_variants,
  function(case_name, panel_variant) {
    write_case_binscatters(case_name, panel_variant)
  }
)

# Reporting ----

walk(binscatter_paths, ~ message("Wrote MW binscatter figure to ", .x))
