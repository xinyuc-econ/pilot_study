# Purpose: recreate the two saved legacy sanity-check plots from derived datasets.
# Inputs: `data/derived/main_us_pilots_any.csv` and `data/derived/main_us_pilots_atr.csv`
# Outputs: `output/figures/num_composition_pilots_by_year.png` and `output/figures/num_atr_pilots_by_year.png`

# Setup and Inputs ----

source("code/00_setup/00_packages_paths.R")

main_us_pilots_any <- readr::read_csv(
  file.path(paths$derived, "main_us_pilots_any.csv"),
  show_col_types = FALSE,
  col_types = readr::cols(.default = readr::col_character())
) |>
  dplyr::mutate(year = as.integer(.data$year))

main_us_pilots_atr <- readr::read_csv(
  file.path(paths$derived, "main_us_pilots_atr.csv"),
  show_col_types = FALSE,
  col_types = readr::cols(.default = readr::col_character())
) |>
  dplyr::mutate(year = as.integer(.data$year))

# 1. Figures ----

## 1.1 Composition of Pilot Certification Holders ----

composition_plot <- main_us_pilots_any |>
  ggplot2::ggplot(ggplot2::aes(x = as.factor(.data$year), fill = .data$level_collapsed)) +
  ggplot2::geom_bar() +
  ggplot2::scale_y_continuous(
    name = NULL,
    labels = scales::label_number(scale = 1 / 1000, suffix = "K"),
    breaks = seq(0, 550000, 50000),
    limits = c(0, 550000)
  ) +
  ggplot2::scale_x_discrete(name = NULL) +
  ggplot2::scale_fill_manual(
    values = viridisLite::viridis(3),
    labels = c("Airline Transport Pilot", "Commercial Pilot", "Other"),
    name = "Pilot Certification Level"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(size = 15),
    axis.text.y = ggplot2::element_text(size = 15),
    plot.title = ggplot2::element_text(size = 20),
    legend.text = ggplot2::element_text(size = 15),
    legend.title = ggplot2::element_text(size = 15)
  ) +
  ggplot2::labs(title = "Number and Composition of Pilot Certification Holders by Year")

ggplot2::ggsave(
  filename = file.path(paths$figures, "num_composition_pilots_by_year.png"),
  plot = composition_plot,
  width = 10,
  height = 7
)

## 1.2 Airline Transport Pilot Counts ----

atr_plot <- main_us_pilots_atr |>
  ggplot2::ggplot(ggplot2::aes(x = as.factor(.data$year))) +
  ggplot2::geom_bar(fill = "#FFCB05") +
  ggplot2::scale_y_continuous(
    name = NULL,
    labels = scales::label_number(scale = 1 / 1000, suffix = "K"),
    breaks = seq(0, 120000, 10000),
    limits = c(0, 120000)
  ) +
  ggplot2::scale_x_discrete(name = NULL) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(size = 15),
    axis.text.y = ggplot2::element_text(size = 15),
    plot.title = ggplot2::element_text(size = 20),
    legend.text = ggplot2::element_text(size = 15),
    legend.title = ggplot2::element_text(size = 15)
  ) +
  ggplot2::labs(title = "Number of Airline Transport Pilots by Year")

ggplot2::ggsave(
  filename = file.path(paths$figures, "num_atr_pilots_by_year.png"),
  plot = atr_plot,
  width = 9,
  height = 7
)

# Reporting ----

message("Saved plots to ", paths$figures)
