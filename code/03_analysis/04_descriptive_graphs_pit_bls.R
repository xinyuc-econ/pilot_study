# Purpose: reproduce the working legacy BLS PIT map for airline-pilot mean wages.
# Inputs: `data/derived/all_years_pit_bls.csv`
# Outputs: one BLS PIT ATR map figure in `output/figures/`

# Setup ----

source("code/00_setup/00_packages_paths.R")
source("code/utils/cleaning_helpers.R")

pit <- readr::read_csv(
  file.path(paths$derived, "all_years_pit_bls.csv"),
  show_col_types = FALSE
)

state_crosswalk <- load_state_fips_crosswalk(paths) |>
  dplyr::select("fips", "state")

atr_map_path <- file.path(paths$figures, "atr_maps_bls_airline_mean.png")

# Derived Analysis Inputs ----

pit_airline_mean <- pit |>
  dplyr::left_join(state_crosswalk, by = "fips") |>
  dplyr::select("pilot_type", "percentile", "year", "state", "atr") |>
  dplyr::mutate(atr = .data$atr * 100) |>
  dplyr::filter(.data$pilot_type == "airline", .data$percentile == "mean")

# 1. Figures ----

## 1.1 Airline Mean Average Tax Rate Maps ----

atr_map <- usmap::plot_usmap(
  data = pit_airline_mean,
  values = "atr"
) +
  ggplot2::scale_fill_viridis_c(alpha = 0.9, direction = -1) +
  ggplot2::facet_wrap(~year) +
  ggplot2::labs(
    fill = "",
    title = "Average tax rate for airline pilots, mean wage measure, 2009-2022"
  ) +
  ggplot2::theme(
    legend.position = c(0.6, 0.03),
    legend.text = ggplot2::element_text(size = 12),
    strip.text.x = ggplot2::element_text(size = 12),
    plot.title = ggplot2::element_text(size = 17)
  )

ggplot2::ggsave(atr_map_path, plot = atr_map, width = 15, height = 10)

# Reporting ----

message("Wrote BLS PIT figure to ", atr_map_path)
