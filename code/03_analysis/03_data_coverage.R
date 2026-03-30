# Purpose: compare overlap-year ATR coverage between AviationDB and FAA flat files.
# Inputs: FAA and AviationDB raw inputs for overlapping years
# Outputs: overlap validation figure files in `output/faa/figures/`

# Setup ----

source("code/00_setup/00_packages_paths.R")
source("code/utils/ingest_helpers.R")
source("code/utils/cleaning_helpers.R")

dir.create(paths$figures_faa, recursive = TRUE, showWarnings = FALSE)

national_overlap_path <- file.path(paths$figures_faa, "aviationdb_faa_overlap_counts.png")
state_overlap_path <- file.path(paths$figures_faa, "aviationdb_faa_overlap_pct_diff.png")

faa_inputs <- discover_ingest_inputs(paths, source = "faa_flat")
aviationdb_inputs <- discover_ingest_inputs(paths, source = "aviationdb")
overlap_years <- intersect(faa_inputs$year, aviationdb_inputs$year)

if (length(overlap_years) == 0) {
  stop("No overlapping years found between FAA flat files and AviationDB.", call. = FALSE)
}

state_crosswalk <- load_state_fips_crosswalk(paths) |>
  select("fips", "state")

build_source_overlap_counts <- function(source, years) {
  build_pilot_ingest_dataset(paths, source = source, years = years) |>
    filter(certificate_level == "A") |>
    left_join(state_crosswalk, by = "state") |>
    filter(!is.na(fips)) |>
    mutate(source = source)
}

overlap_panel <- bind_rows(
  build_source_overlap_counts("faa_flat", overlap_years),
  build_source_overlap_counts("aviationdb", overlap_years)
)

national_counts <- overlap_panel |>
  count(year, source, name = "n_atr_pilots")

state_counts <- overlap_panel |>
  count(year, state, source, name = "n_atr_pilots") |>
  tidyr::pivot_wider(names_from = "source", values_from = "n_atr_pilots") |>
  mutate(
    row_diff = aviationdb - faa_flat,
    pct_diff = row_diff / faa_flat * 100
  )

# 1. Figures ----

## 1.1 National Overlap-Year ATR Counts ----

national_overlap_plot <- national_counts |>
  ggplot(aes(x = year, y = n_atr_pilots, color = source, shape = source)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  scale_y_continuous(labels = scales::label_number(scale = 1 / 1000, suffix = "K")) +
  scale_x_continuous(breaks = overlap_years) +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    shape = NULL,
    title = "ATR counts in overlapping FAA and AviationDB years"
  ) +
  theme_bw()

ggsave(national_overlap_path, plot = national_overlap_plot, width = 9, height = 6)

## 1.2 State-Year Percent Difference Heatmap ----

state_overlap_plot <- state_counts |>
  ggplot(aes(x = year, y = state, fill = pct_diff)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "#b2182b",
    mid = "white",
    high = "#2166ac",
    midpoint = 0,
    name = "% diff"
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "AviationDB minus FAA ATR counts, percent difference"
  ) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8))

ggsave(state_overlap_path, plot = state_overlap_plot, width = 12, height = 11)

# Reporting ----

message("Saved overlap validation figures to ", paths$figures_faa)
