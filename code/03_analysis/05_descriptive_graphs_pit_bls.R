# Purpose: reproduce the working legacy BLS PIT map for airline-pilot mean wages.
# Inputs: `data/derived/aviationdb/all_years_pit_bls.csv`
# Outputs: one BLS PIT ATR map figure in `output/aviationdb/figures/`

# Setup ----

source("code/00_setup/00_packages_paths.R")
source("code/utils/cleaning_helpers.R")

pit <- read_csv(
  file.path(paths$derived_aviationdb, "all_years_pit_bls.csv"),
  show_col_types = FALSE
)

state_crosswalk <- load_state_fips_crosswalk(paths) |>
  select("fips", "state")

dir.create(paths$figures_aviationdb, recursive = TRUE, showWarnings = FALSE)

atr_map_path <- file.path(paths$figures_aviationdb, "atr_maps_bls_airline_mean.png")

# Derived Analysis Inputs ----

pit_airline_mean <- pit |>
  left_join(state_crosswalk, by = "fips") |>
  select("pilot_type", "percentile", "year", "state", "atr") |>
  mutate(atr = atr * 100) |>
  filter(pilot_type == "airline", percentile == "mean", year %in% analysis_years)

# 1. Figures ----

## 1.1 Airline Mean Average Tax Rate Maps ----

atr_map <- plot_usmap(
  data = pit_airline_mean,
  values = "atr"
) +
  scale_fill_viridis_c(alpha = 0.9, direction = -1) +
  facet_wrap(~year) +
  labs(
    fill = "",
    title = sprintf(
      "Average tax rate for airline pilots, mean wage measure, %s-%s",
      min(analysis_years),
      max(analysis_years)
    )
  ) +
  theme(
    legend.position = c(0.6, 0.03),
    legend.text = element_text(size = 12),
    strip.text.x = element_text(size = 12),
    plot.title = element_text(size = 17)
  )

ggsave(atr_map_path, plot = atr_map, width = 15, height = 10)

# Reporting ----

message("Wrote BLS PIT figure to ", atr_map_path)
