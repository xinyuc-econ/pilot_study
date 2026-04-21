# Purpose: plot county-level counts of ATR pilots for 2017 and 2022 to inspect state-border discontinuities.
# Inputs: `data/derived/aviationdb/main_us_pilots_atr.csv`
# Outputs: one faceted county-level map in `output/aviationdb/figures/`

# Setup ----

source("code/00_setup/00_packages_paths.R")

if (!requireNamespace("tigris", quietly = TRUE)) {
  stop(
    "Package `tigris` is required for ZIP geometry downloads. Please install it before running this script.",
    call. = FALSE
  )
}

options(tigris_use_cache = FALSE)

plot_years <- c(2017L, 2022L)
excluded_states <- c("AK", "HI", "PR", "VI", "GU", "MP", "AS")

county_count_map_path <- file.path(
  paths$figures_aviationdb,
  "atr_pilot_county_count_map_2017_2022.png"
)

dir.create(paths$figures_aviationdb, recursive = TRUE, showWarnings = FALSE)

# Inputs ----

pilot_data <- read_csv(
  file.path(paths$derived_aviationdb, "main_us_pilots_atr.csv"),
  show_col_types = FALSE,
  col_types = cols(
    year = col_integer(),
    zip_code = col_character(),
    .default = col_character()
  )
)

# Derived Analysis Inputs ----

pilot_data_plot <- pilot_data |>
  filter(year %in% plot_years) |>
  mutate(
    zip5 = stringr::str_extract(zip_code, "^\\d{5}")
  )

zip_cleaning_summary <- pilot_data_plot |>
  summarise(
    pilot_rows_total = n(),
    pilot_rows_with_valid_zip = sum(!is.na(zip5)),
    pilot_rows_dropped_invalid_zip = sum(is.na(zip5))
  )

zip_count_data <- pilot_data_plot |>
  filter(!is.na(zip5)) |>
  count(year, zip5, name = "n_atr_pilots_zip") |>
  ungroup()

state_shapes <- tigris::states(year = 2020, cb = TRUE, class = "sf") |>
  filter(!STUSPS %in% excluded_states) |>
  select(state = STUSPS, geometry) |>
  st_transform(2163) |>
  sf::st_make_valid()

county_shapes <- tigris::counties(year = 2020, cb = TRUE, class = "sf") |>
  mutate(state = STATEFP) |>
  filter(!state %in% c("02", "15", "72", "78", "66", "69", "60")) |>
  transmute(
    county_fips = GEOID,
    county_name = NAME,
    state = STATEFP,
    geometry
  ) |>
  st_transform(2163) |>
  sf::st_make_valid()

zcta_shapes_raw <- tigris::zctas(year = 2020, cb = TRUE, class = "sf") |>
  st_transform(2163) |>
  mutate(row_id = row_number())

zcta_county_lookup <- suppressWarnings(
  sf::st_join(
    zcta_shapes_raw |>
      sf::st_point_on_surface() |>
      select(row_id),
    county_shapes,
    join = sf::st_within,
    left = FALSE,
    largest = TRUE
  )
) |>
  st_drop_geometry() |>
  select(row_id, county_fips, county_name)

zip_county_lookup <- zcta_shapes_raw |>
  st_drop_geometry() |>
  transmute(row_id, zip5 = ZCTA5CE20) |>
  left_join(zcta_county_lookup, by = "row_id") |>
  filter(!is.na(county_fips))

county_count_data <- zip_count_data |>
  left_join(zip_county_lookup, by = "zip5") |>
  filter(!is.na(county_fips)) |>
  group_by(year, county_fips, county_name) |>
  summarise(
    n_atr_pilots_county = sum(n_atr_pilots_zip),
    .groups = "drop"
  )

county_count_summary <- county_count_data |>
  group_by(year) |>
  summarise(
    n_counties_in_data = n_distinct(county_fips),
    min_county_count = min(n_atr_pilots_county),
    median_county_count = median(n_atr_pilots_county),
    max_county_count = max(n_atr_pilots_county),
    .groups = "drop"
  )

county_count_map_data <- county_shapes |>
  left_join(county_count_data, by = "county_fips")

matched_zip_summary <- zip_count_data |>
  left_join(zip_county_lookup, by = "zip5") |>
  filter(!is.na(county_fips)) |>
  distinct(year, zip5) |>
  count(year, name = "n_zip_matched_to_county") |>
  right_join(
    zip_count_data |>
      distinct(year, zip5) |>
      count(year, name = "n_zip_in_data"),
    by = "year"
  ) |>
  mutate(
    n_zip_matched_to_county = coalesce(n_zip_matched_to_county, 0L),
    n_zip_unmatched = n_zip_in_data - n_zip_matched_to_county
  )

# 1. Figures ----

## 1.1 County-Level ATR Pilot Count Map ----

county_count_map <- county_count_map_data |>
  filter(!is.na(year)) |>
  ggplot() +
  geom_sf(aes(fill = n_atr_pilots_county), color = NA) +
  geom_sf(
    data = state_shapes,
    fill = NA,
    color = "white",
    linewidth = 0.22
  ) +
  facet_wrap(~year) +
  scale_fill_viridis_c(
    trans = "log10",
    name = "ATR pilots\nper county",
    labels = scales::label_number(accuracy = 1),
    option = "C",
    na.value = "grey92"
  ) +
  labs(
    title = "County Counts of ATR Pilots",
    subtitle = "Counts are shown on a log scale; state borders are overlaid to inspect border discontinuities"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 12)
  )

ggsave(
  filename = county_count_map_path,
  plot = county_count_map,
  width = 14,
  height = 8.5,
  dpi = 300
)

# Reporting ----

message("Saved county-level ATR pilot count map to ", county_count_map_path)
message(
  "Pilot rows kept after ZIP cleaning: ",
  scales::comma(zip_cleaning_summary$pilot_rows_with_valid_zip),
  " of ",
  scales::comma(zip_cleaning_summary$pilot_rows_total),
  " (dropped ",
  scales::comma(zip_cleaning_summary$pilot_rows_dropped_invalid_zip),
  " invalid ZIP rows)."
)

message("County count summary by year:")
print(county_count_summary)

message("ZIP-to-county match summary:")
print(matched_zip_summary)
