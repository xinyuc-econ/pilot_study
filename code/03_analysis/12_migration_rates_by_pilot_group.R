# Purpose: plot adjacent-year migration rates for ATR, commercial, and other pilots.
# Inputs: `data/derived/aviationdb/main_us_pilots_any_mover_panel.csv`
# Outputs: one figure in `output/aviationdb/figures/`

# Setup ----

source("code/00_setup/00_packages_paths.R")

dir.create(paths$figures_aviationdb, recursive = TRUE, showWarnings = FALSE)

figure_path <- file.path(paths$figures_aviationdb, "migration_rates_by_pilot_group.png")

# Input Loading ----

mover_panel <- read_csv(
  file.path(paths$derived_aviationdb, "main_us_pilots_any_mover_panel.csv"),
  show_col_types = FALSE
) |>
  mutate(year = as.integer(year))

# Derived Analysis Inputs ----

migration_rate_data <- mover_panel |>
  filter(is_adjacent_year) |>
  group_by(year, pilot_group) |>
  summarise(
    n_pilots = n(),
    n_moved = sum(moved, na.rm = TRUE),
    migration_rate = n_moved / n_pilots,
    .groups = "drop"
  ) |>
  mutate(
    pilot_group = factor(
      pilot_group,
      levels = c("atr", "commercial", "other"),
      labels = c("Airline transport pilots", "Commercial pilots", "Other pilots")
    )
  )

# 1. Figure ----
library(ggthemes)

migration_rate_plot <- migration_rate_data |>
  ggplot(aes(x = year, y = migration_rate, color = pilot_group)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = seq(min(migration_rate_data$year), max(migration_rate_data$year), by = 2)
  ) +
  scale_y_continuous(
    labels = function(x) paste0(round(100 * x, 2), "%"),
    breaks = seq(0, 0.07, 0.01)) +
  scale_colour_stata("s2color") +
  labs(
    x = NULL,
    y = "Interstate Migration Rate",
    color = NULL,
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.ticks = element_line(color = "black", linewidth = 2),
    legend.title = NULL,
    legend.text = element_text(size = 15),
    legend.position = "bottom"
  ) 


ggsave(figure_path, plot = migration_rate_plot, width = 9, height = 6)

# Reporting ----

message("Wrote migration-rate figure to ", figure_path)
