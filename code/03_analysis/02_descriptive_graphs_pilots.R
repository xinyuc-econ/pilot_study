# Purpose: reproduce legacy descriptive state graphs and summary statistics for ATR pilots.
# Inputs: `data/derived/sum_stat_prop_atr_pilots.csv`
# Outputs: six state-level figure files in `output/figures/` and one LaTeX table in `output/tables/`

# Setup ----

source("code/00_setup/00_packages_paths.R")
source("code/utils/cleaning_helpers.R")

state_prop_data <- readr::read_csv(
  file.path(paths$derived, "sum_stat_prop_atr_pilots.csv"),
  show_col_types = FALSE
)

zero_states <- c("TX", "FL", "NV", "WA", "WY", "SD", "TN", "AK", "NH")

table_path <- file.path(paths$tables, "summary_stats_atr_pilots.tex")
all_years_map_path <- file.path(paths$figures, "atr_pilot_share_map_by_year.png")
state_share_map_path <- file.path(paths$figures, "atr_pilot_share_map_2024.png")
share_change_map_path <- file.path(paths$figures, "atr_pilot_share_change_map_2009_2024.png")
total_share_map_path <- file.path(paths$figures, "atr_pilot_share_of_total_atr_map_2024.png")
residual_map_path <- file.path(paths$figures, "atr_pilot_share_residual_map_2024.png")
residual_zero_tax_map_path <- file.path(paths$figures, "atr_pilot_share_residual_map_zero_tax_2024.png")

# Derived Analysis Inputs ----

summary_table_data <- state_prop_data |>
  dplyr::mutate(
    n_atr_pilots = .data$n_atr_pilots / 1000,
    tot_work_pop = .data$tot_work_pop / 1000
  ) |>
  as.data.frame()

state_prop_binned <- build_state_prop_bins(state_prop_data)
state_prop_binned_2024 <- state_prop_binned |>
  dplyr::filter(.data$year == 2024)

state_prop_change <- build_state_prop_change(state_prop_data)

state_total_share <- build_state_total_share(state_prop_data)
state_total_share_2024 <- state_total_share |>
  dplyr::filter(.data$year == 2024)

residual_state_map <- build_residual_state_map(state_total_share, zero_states)

# 1. Tables ----

## 1.1 Summary Statistics Table ----

stargazer::stargazer(
  summary_table_data,
  title = "Summary Statistics: Number and Percentage of ATR pilots in each state",
  digits = 3,
  covariate.labels = c("Year", "Number of ATR pilots (K)", "Total working pop. (K)", "\\%, ATR pilots"),
  out = table_path,
  notes = "Notes: Total working population computed using NBER CPS morg data.",
  notes.align = "l",
  font.size = "small"
)

# 2. Figures ----

## 2.1 ATR Pilot Share by State and Year ----

all_years_map <- usmap::plot_usmap(
  data = state_prop_binned,
  regions = "states",
  values = "prop_bins"
) +
  ggplot2::scale_fill_viridis_d(alpha = 0.9, direction = -1) +
  ggplot2::facet_wrap(~year) +
  ggplot2::labs(
    fill = "",
    title = "Percentage of ATR pilots relative to total working population"
  ) +
  ggplot2::theme(
    legend.position = c(0.7, 0.03),
    legend.text = ggplot2::element_text(size = 12),
    strip.text.x = ggplot2::element_text(size = 12),
    plot.title = ggplot2::element_text(size = 17)
  )

ggplot2::ggsave(all_years_map_path, plot = all_years_map, width = 15, height = 10)

## 2.2 ATR Pilot Share by State in 2024 ----

state_share_map_2024 <- usmap::plot_usmap(
  data = state_prop_binned_2024,
  regions = "states",
  values = "prop_bins"
) +
  ggplot2::scale_fill_viridis_d(alpha = 0.9, direction = -1, name = "%") +
  ggplot2::labs(
    fill = "",
    title = "Share of Pilots Relative to State Total Working Population, 2024"
  ) +
  ggplot2::theme(
    legend.position = c(0.9, 0.03),
    legend.text = ggplot2::element_text(size = 20),
    legend.title = ggplot2::element_text(size = 20),
    strip.text.x = ggplot2::element_text(size = 20),
    plot.title = ggplot2::element_text(size = 25),
    legend.background = ggplot2::element_rect(fill = "transparent"),
    legend.box.background = ggplot2::element_rect(fill = "transparent")
  )

ggplot2::ggsave(state_share_map_path, plot = state_share_map_2024, width = 16, height = 10)

## 2.3 Change in ATR Pilot Share, 2009-2024 ----

share_change_map <- usmap::plot_usmap(
  data = state_prop_change,
  regions = "states",
  values = "d_prop_bins"
) +
  ggplot2::scale_fill_viridis_d(alpha = 0.9, direction = -1) +
  ggplot2::labs(
    fill = "",
    title = "Changes in proportion of ATR relative to total working population, 2009-2024"
  ) +
  ggplot2::theme(
    legend.position = c(0.9, 0.03),
    legend.text = ggplot2::element_text(size = 20),
    strip.text.x = ggplot2::element_text(size = 20),
    plot.title = ggplot2::element_text(size = 25),
    legend.background = ggplot2::element_rect(fill = "transparent"),
    legend.box.background = ggplot2::element_rect(fill = "transparent")
  )

ggplot2::ggsave(share_change_map_path, plot = share_change_map, width = 16, height = 10)

## 2.4 ATR Share of Total ATR Pilots in 2024 ----

total_share_map_2024 <- usmap::plot_usmap(
  data = state_total_share_2024,
  regions = "states",
  values = "prop_bins"
) +
  ggplot2::scale_fill_viridis_d(alpha = 0.9, direction = -1, name = "%") +
  ggplot2::labs(
    fill = "",
    title = "Share of Pilots Relative to Total # of Pilots, 2024"
  ) +
  ggplot2::theme(
    legend.position = c(0.9, 0.03),
    legend.text = ggplot2::element_text(size = 20),
    legend.title = ggplot2::element_text(size = 20),
    strip.text.x = ggplot2::element_text(size = 20),
    plot.title = ggplot2::element_text(size = 25),
    legend.background = ggplot2::element_rect(fill = "transparent"),
    legend.box.background = ggplot2::element_rect(fill = "transparent")
  )

ggplot2::ggsave(total_share_map_path, plot = total_share_map_2024, width = 16, height = 10)

## 2.5 Residual ATR Pilot Share Map in 2024 ----

residual_map <- ggplot2::ggplot(residual_state_map) +
  ggplot2::geom_sf(ggplot2::aes(fill = .data$resid_binned), color = "white", alpha = 0.9) +
  ggplot2::scale_fill_viridis_d(name = "Residual Bin", direction = -1) +
  ggplot2::labs(title = "Residuals from Regression of Pilot Share on State Total Working Population (2024)") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.text = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(size = 20),
    strip.text.x = ggplot2::element_text(size = 20),
    plot.title = ggplot2::element_text(size = 25),
    legend.title = ggplot2::element_text(size = 20)
  )

ggplot2::ggsave(residual_map_path, plot = residual_map, width = 13, height = 8)

## 2.6 Residual ATR Pilot Share Map with Zero-Tax Overlay in 2024 ----
residual_zero_tax_map <- ggplot2::ggplot(residual_state_map) +
  ggplot2::geom_sf(ggplot2::aes(fill = .data$resid_binned), color = "white", alpha = 0.9) +
  ggplot2::scale_fill_viridis_d(name = "Residual Bin", direction = -1) +
  ggpattern::geom_sf_pattern(
    data = residual_state_map[residual_state_map$zero_tax, ],
    ggplot2::aes(pattern = .data$zero_tax),
    fill = NA,
    pattern_color = "black",
    pattern_density = 0.1,
    pattern_spacing = 0.025,
    pattern_angle = 45
  ) +
  ggpattern::scale_pattern_manual(name = "Zero PIT", values = c("TRUE" = "crosshatch")) +
  ggplot2::labs(title = "Residuals from Regression of Pilot Share on State Total Working Population (2024)") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.text = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(size = 20),
    strip.text.x = ggplot2::element_text(size = 20),
    plot.title = ggplot2::element_text(size = 25),
    legend.title = ggplot2::element_text(size = 20)
  )

ggplot2::ggsave(residual_zero_tax_map_path, plot = residual_zero_tax_map, width = 13, height = 8)

# Reporting ----

message("Wrote descriptive graphs to ", paths$figures)
message("Wrote summary table to ", table_path)
