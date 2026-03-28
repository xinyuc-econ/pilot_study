# Purpose: reproduce legacy national and state-level ATR data coverage figures.
# Inputs: `data/intermediate/faa_pilot_rich_panel.csv`, `data/derived/faa_official_atr_pilots_by_state_year.csv`, and `data/derived/sum_stat_prop_atr_pilots.csv`
# Outputs: four coverage figure files in `output/figures/`

# Setup ----

source("code/00_setup/00_packages_paths.R")

faa_aggregate_path <- file.path(
  paths$raw_data_root,
  "raw",
  "airmen_aggregate_stats",
  "faa_atr_coverage.xlsx"
)

coverage_count_path <- file.path(paths$figures, "airmen_atr_coverage_count.png")
coverage_percentage_path <- file.path(paths$figures, "airmen_atr_coverage_percentage.png")
coverage_combined_path <- file.path(paths$figures, "airmen_atr_coverage_both.png")
state_coverage_map_path <- file.path(paths$figures, "state_coverage_atr_pilots.png")

# Input Loading ----

pilot_data <- read_csv(
  file.path(paths$intermediate, "faa_pilot_rich_panel.csv"),
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
) |>
  mutate(year = as.integer(year))

faa_coverage <- read_excel(faa_aggregate_path) |>
  mutate(year = as.integer(year))

faa_official_state_counts <- read_csv(
  file.path(paths$derived, "faa_official_atr_pilots_by_state_year.csv"),
  show_col_types = FALSE
)

state_prop_data <- read_csv(
  file.path(paths$derived, "sum_stat_prop_atr_pilots.csv"),
  show_col_types = FALSE
)

# Derived Analysis Inputs ----

airmen_coverage <- pilot_data |>
  filter(type == "P", level == "A", year != 2025) |>
  count(year, name = "n") |>
  mutate(data_type = "airmen")

coverage <- bind_rows(airmen_coverage, faa_coverage)

coverage_years <- sort(unique(coverage$year))

label_info <- coverage |>
  filter(year == 2019) |>
  mutate(
    label = case_when(
      data_type == "airmen" ~ "Airmen Certificate Data",
      data_type == "faa" ~ "Official Aggregate Data"
    )
  )

coverage_wide <- airmen_coverage |>
  select(year, airmen = n) |>
  left_join(
    faa_coverage |>
      select(year, faa = n),
    by = "year"
  ) |>
  mutate(coverage = airmen / faa)

state_coverage <- faa_official_state_counts |>
  left_join(
    state_prop_data |>
      rename(my_n_atr_pilots = n_atr_pilots),
    by = c("state", "year")
  ) |>
  mutate(
    coverage = my_n_atr_pilots / faa_n_atr_pilots * 100
  ) |>
  filter(!is.na(coverage))

state_map <- usmap::us_map("states")

state_centroids <- suppressWarnings(sf::st_point_on_surface(state_map))
state_centroid_coords <- sf::st_coordinates(state_centroids)

state_centers <- state_centroids |>
  sf::st_drop_geometry() |>
  mutate(
    X = state_centroid_coords[, "X"],
    Y = state_centroid_coords[, "Y"]
  ) |>
  select("abbr", "X", "Y")

state_coverage_labeled <- state_coverage |>
  left_join(state_centers, by = c("state" = "abbr"))

repel_states <- c("RI", "CT", "NJ", "DE", "MD", "MA", "DC", "VT", "NH")

labels_repel <- state_coverage_labeled |>
  filter(state %in% repel_states)

labels_regular <- state_coverage_labeled |>
  filter(!state %in% repel_states)

state_coverage_map_data <- state_map |>
  left_join(state_coverage, by = c("abbr" = "state"))

# 1. Figures ----

## 1.1 ATR Counts: Airmen Data vs Official FAA Totals ----

coverage_count_plot <- coverage |>
  ggplot(aes(x = year, y = n, color = data_type, shape = data_type)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1.2) +
  scale_y_continuous(
    name = NULL,
    labels = scales::label_number(scale = 1 / 1000, suffix = "K"),
    breaks = seq(100000, 180000, 10000)
  ) +
  scale_x_continuous(
    name = NULL,
    breaks = coverage_years
  ) +
  geom_text(
    data = label_info,
    aes(label = label, x = year + 3, y = n + 5000),
    fontface = "bold",
    size = 6,
    hjust = "right",
    vjust = "bottom"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 17),
    axis.text.y = element_text(size = 17),
    plot.title = element_text(size = 25),
    legend.position = "none"
  )

ggsave(
  filename = coverage_count_path,
  plot = coverage_count_plot,
  width = 8,
  height = 6.5
)

## 1.2 ATR Coverage Percentage by Year ----

coverage_percentage_plot <- coverage_wide |>
  filter(!is.na(coverage)) |>
  ggplot(aes(x = as.factor(year), y = coverage)) +
  geom_col() +
  scale_y_continuous(
    name = NULL,
    labels = scales::label_percent(accuracy = 1),
    breaks = seq(0, 1, 0.1)
  ) +
  scale_x_discrete(name = NULL) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 17),
    axis.text.y = element_text(size = 17),
    plot.title = element_text(size = 25)
  )

ggsave(
  filename = coverage_percentage_path,
  plot = coverage_percentage_plot,
  width = 8,
  height = 6.5
)

## 1.3 Combined Coverage Figure ----

combined_coverage_plot <- patchwork::wrap_plots(
  coverage_count_plot,
  coverage_percentage_plot,
  ncol = 2
)

ggsave(
  filename = coverage_combined_path,
  plot = combined_coverage_plot,
  width = 16,
  height = 6.5,
  dpi = 300
)

## 1.4 State-Level ATR Coverage by Year ----

state_coverage_map <- ggplot() +
  geom_sf(data = state_map, fill = NA, color = "black") +
  geom_sf(
    data = state_coverage_map_data,
    aes(fill = coverage)
  ) +
  scale_fill_viridis_c(
    name = "Data Coverage (%)",
    alpha = 0.9,
    direction = -1
  ) +
  geom_text(
    data = labels_regular,
    aes(x = X, y = Y, label = paste0(round(coverage, 0), "%")),
    color = "black",
    size = 4
  ) +
  ggrepel::geom_text_repel(
    data = labels_repel,
    aes(x = X, y = Y, label = paste0(round(coverage, 0), "%")),
    nudge_x = 6e5,
    direction = "x",
    segment.color = "gray50",
    point.padding = 0.2,
    max.overlaps = Inf,
    force = 5,
    force_pull = 0.5,
    size = 4,
    min.segment.length = 0
  ) +
  facet_wrap(~year) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    strip.text = element_text(size = 15)
  )

ggsave(
  filename = state_coverage_map_path,
  plot = state_coverage_map,
  width = 20,
  height = 10
)

# Reporting ----

message("Saved coverage figures to ", paths$figures)
