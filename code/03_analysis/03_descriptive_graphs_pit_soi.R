# Purpose: reproduce legacy PIT summary outputs and maps for the SOI p95 taxpayer case.
# Inputs: `data/derived/all_years_pit_soi_wide.csv`
# Outputs: one summary table and three PIT map figures in `output/tables/` and `output/figures/`

# Setup ----

source("code/00_setup/00_packages_paths.R")
source("code/utils/cleaning_helpers.R")

pit <- read_csv(
  file.path(paths$derived, "all_years_pit_soi_wide.csv"),
  show_col_types = FALSE
)

state_crosswalk <- load_state_fips_crosswalk(paths) |>
  select("fips", "state")

table_path <- file.path(paths$tables, "summary_stats_pit_soi_p95.tex")
srate_map_path <- file.path(paths$figures, "srate_maps_soi_p95.png")
astr_map_path <- file.path(paths$figures, "astr_maps_soi_p95.png")
atr_map_path <- file.path(paths$figures, "atr_maps_soi_p95.png")

# Derived Analysis Inputs ----

pit_for_plot <- pit |>
  left_join(state_crosswalk, by = "fips") |>
  filter(year >= 2009) |>
  select("year", "state", "srate_p95", "astr_p95", "atr_p95") |>
  mutate(
    astr_p95 = astr_p95 * 100,
    atr_p95 = atr_p95 * 100
  )

summary_table_data <- pit_for_plot |>
  select("year", "srate_p95", "astr_p95", "atr_p95") |>
  as.data.frame()

# 1. Tables ----

## 1.1 Summary Statistics Table ----

stargazer(
  summary_table_data,
  title = "Summary Statistics: 3 measures of PIT for a 95th income percentile taxpayer",
  digits = 2,
  covariate.labels = c("Year", "Marginal state tax rate", "Ave. state tax rate", "Ave. tax rate"),
  out = table_path,
  notes = "Notes: Measures constructed using NBER TAXSIM.",
  notes.align = "l",
  font.size = "small"
)

# 2. Figures ----

## 2.1 Marginal State Tax Rate Maps ----

srate_map <- plot_usmap(
  data = pit_for_plot,
  values = "srate_p95"
) +
  scale_fill_viridis_c(alpha = 0.9, direction = -1) +
  facet_wrap(~year) +
  labs(
    fill = "",
    title = "Marginal state tax rate, 95th income percentile, 2009-2022"
  ) +
  theme(
    legend.position = c(0.6, 0.03),
    legend.text = element_text(size = 12),
    strip.text.x = element_text(size = 12),
    plot.title = element_text(size = 17)
  )

ggsave(srate_map_path, plot = srate_map, width = 15, height = 10)

## 2.2 Average State Tax Rate Maps ----

astr_map <- plot_usmap(
  data = pit_for_plot,
  values = "astr_p95"
) +
  scale_fill_viridis_c(alpha = 0.9, direction = -1) +
  facet_wrap(~year) +
  labs(
    fill = "",
    title = "Average state tax rate, 95th income percentile, 2009-2022"
  ) +
  theme(
    legend.position = c(0.6, 0.03),
    legend.text = element_text(size = 12),
    strip.text.x = element_text(size = 12),
    plot.title = element_text(size = 17)
  )

ggsave(astr_map_path, plot = astr_map, width = 15, height = 10)

## 2.3 Average Total Tax Rate Maps ----

atr_map <- plot_usmap(
  data = pit_for_plot,
  values = "atr_p95"
) +
  scale_fill_viridis_c(alpha = 0.9, direction = -1) +
  facet_wrap(~year) +
  labs(
    fill = "",
    title = "Average tax rate, 95th income percentile, 2009-2022"
  ) +
  theme(
    legend.position = c(0.55, 0.03),
    legend.text = element_text(size = 12),
    strip.text.x = element_text(size = 12),
    plot.title = element_text(size = 17)
  )

ggsave(atr_map_path, plot = atr_map, width = 15, height = 10)

# Reporting ----

message("Wrote SOI PIT outputs to ", paths$figures, " and ", paths$tables)
