# Purpose: generate legacy-style binscatter figures for SOI and BLS pilot-tax measures.
# Inputs: `data/derived/pilot_tax_analysis_soi.csv` and `data/derived/pilot_tax_analysis_bls.csv`
# Outputs: binscatter figures in `output/figures/`

# Setup ----

source("code/00_setup/00_packages_paths.R")
source("code/utils/cleaning_helpers.R")

pilot_tax_soi <- read_csv(
  file.path(paths$derived, "pilot_tax_analysis_soi.csv"),
  show_col_types = FALSE
)

pilot_tax_bls <- read_csv(
  file.path(paths$derived, "pilot_tax_analysis_bls.csv"),
  show_col_types = FALSE
)

compute_all_states_y_limits <- function(data) {
  y_range <- range(data$prop_atr_pilots, na.rm = TRUE)
  padding <- max((y_range[2] - y_range[1]) * 0.05, 0.005)
  c(0.04, min(0.16, y_range[2] + padding))
}

apply_legacy_binscatter_style <- function(
  plot,
  data,
  method,
  tax_measure,
  geography_variant,
  x_label,
  caption = NULL
) {
  styled_plot <- plot +
    labs(
      x = x_label,
      y = "Share of pilots in tot. working pop. (%)",
      caption = caption
    ) +
    theme(
      axis.title = element_text(size = 24),
      axis.text = element_text(size = 20),
      strip.text.x = element_text(size = 28),
      plot.caption = element_text(size = 14)
    )

  if (method == "bls") {
    if (geography_variant == "no_AK_HI") {
      styled_plot <- styled_plot +
        scale_y_continuous(
          limits = c(0.04, 0.12),
          breaks = seq(0.04, 0.12, 0.02)
        )
    } else {
      y_limits <- compute_all_states_y_limits(data)
      y_breaks <- seq(0.04, 0.16, 0.02)
      y_breaks <- y_breaks[y_breaks >= y_limits[1] & y_breaks <= y_limits[2]]

      styled_plot <- styled_plot +
        scale_y_continuous(
          limits = y_limits,
          breaks = y_breaks
        )
    }

    if (tax_measure == "atr") {
      styled_plot <- styled_plot +
        scale_x_continuous(
          limits = c(19, 29),
          breaks = seq(19, 29, 2)
        )
    }
  }

  styled_plot
}

build_binscatter_plot <- function(
  data,
  x_var,
  polyreg,
  method,
  tax_measure,
  geography_variant,
  x_label,
  caption = NULL
) {
  bins_output <- suppressWarnings(
    binsreg(
      y = data$prop_atr_pilots,
      x = data[[x_var]],
      data = as.data.frame(data),
      polyreg = polyreg
    )
  )

  suppressMessages(
    apply_legacy_binscatter_style(
      plot = bins_output$bins_plot,
      data = data,
      method = method,
      tax_measure = tax_measure,
      geography_variant = geography_variant,
      x_label = x_label,
      caption = caption
    )
  )
}

save_case_binscatters <- function(
  data,
  method,
  percentile,
  geography_variant,
  excluded_states,
  polyreg,
  pilot_type = NULL
) {
  filtered_data <- if (length(excluded_states) > 0) {
    data |>
      filter(!.data$state %in% excluded_states)
  } else {
    data
  }

  if (method == "soi") {
    x_label_astr <- sprintf(
      "Average state tax rate, %s (%%)",
      format_soi_percentile_label(percentile)
    )
    x_label_atr <- sprintf(
      "Average tax rate, %s (%%)",
      format_soi_percentile_label(percentile)
    )
    caption <- NULL
  } else {
    x_label_astr <- "Average state tax rate (%)"
    x_label_atr <- "Average tax rate (%)"
    caption <- sprintf(
      "Notes: tax measure simulated using %s wage of %s pilots",
      percentile,
      pilot_type
    )
  }

  astr_plot <- build_binscatter_plot(
    data = filtered_data,
    x_var = "astr",
    polyreg = polyreg,
    method = method,
    tax_measure = "astr",
    geography_variant = geography_variant,
    x_label = x_label_astr,
    caption = caption
  )

  atr_plot <- build_binscatter_plot(
    data = filtered_data,
    x_var = "atr",
    polyreg = polyreg,
    method = method,
    tax_measure = "atr",
    geography_variant = geography_variant,
    x_label = x_label_atr,
    caption = caption
  )

  suppressWarnings(
    ggsave(
      file.path(
        paths$figures,
        build_binscatter_output_filename(
          method = method,
          tax_measure = "astr",
          percentile = percentile,
          geography_variant = geography_variant,
          pilot_type = pilot_type
        )
      ),
      plot = astr_plot,
      width = 10,
      height = 8
    )
  )

  suppressWarnings(
    ggsave(
      file.path(
        paths$figures,
        build_binscatter_output_filename(
          method = method,
          tax_measure = "atr",
          percentile = percentile,
          geography_variant = geography_variant,
          pilot_type = pilot_type
        )
      ),
      plot = atr_plot,
      width = 10,
      height = 8
    )
  )
}

soi_cases <- tibble(percentile = c("p90", "p95", "p99"))

bls_cases <- pilot_tax_bls |>
  filter(.data$pilot_type == "airline", .data$percentile %in% c("mean", "median")) |>
  distinct(.data$pilot_type, .data$percentile) |>
  arrange(.data$pilot_type, .data$percentile)

geography_variants <- tribble(
  ~geography_variant, ~excluded_states,
  "no_AK_HI", list(c("AK", "HI")),
  "all_states", list(character(0))
)

# 1. Figures ----

## 1.1 SOI Binscatter Figures ----

walk(
  soi_cases$percentile,
  function(percentile) {
    case_percentile <- percentile

    pwalk(
      geography_variants,
      function(geography_variant, excluded_states) {
        case_geography_variant <- geography_variant
        case_excluded_states <- excluded_states[[1]]

        save_case_binscatters(
          data = pilot_tax_soi |>
            filter(.data$percentile == .env$case_percentile) |>
            mutate(
              astr = .data$astr * 100,
              atr = .data$atr * 100
            ),
          method = "soi",
          percentile = case_percentile,
          geography_variant = case_geography_variant,
          excluded_states = case_excluded_states,
          polyreg = 3
        )
      }
    )
  }
)

## 1.2 BLS Binscatter Figures ----

pwalk(
  bls_cases,
  function(pilot_type, percentile) {
    case_pilot_type <- pilot_type
    case_percentile <- percentile

    pwalk(
      geography_variants,
      function(geography_variant, excluded_states) {
        case_geography_variant <- geography_variant
        case_excluded_states <- excluded_states[[1]]

        save_case_binscatters(
          data = pilot_tax_bls |>
            filter(
              .data$pilot_type == .env$case_pilot_type,
              .data$percentile == .env$case_percentile
            ) |>
            mutate(
              astr = .data$astr * 100,
              atr = .data$atr * 100
            ),
          method = "bls",
          percentile = case_percentile,
          geography_variant = case_geography_variant,
          excluded_states = case_excluded_states,
          polyreg = 1,
          pilot_type = case_pilot_type
        )
      }
    )
  }
)

# Reporting ----

message("Wrote pilot-tax binscatter figures to ", paths$figures)
