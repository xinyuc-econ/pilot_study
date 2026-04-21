# Purpose: reproduce the zero-PIT analysis using border counties in zero-PIT states and adjacent border counties in non-zero-PIT states.
# Inputs: `data/derived/aviationdb/main_us_pilots_atr.csv`
# Outputs: one normalized trend figure and one county-border event-study figure in `output/aviationdb/figures/`

# Setup ----

source("code/00_setup/00_packages_paths.R")

if (!requireNamespace("tigris", quietly = TRUE)) {
  stop(
    "Package `tigris` is required for county and ZCTA geometry downloads. Please install it before running this script.",
    call. = FALSE
  )
}

if (!requireNamespace("ggfixest", quietly = TRUE)) {
  stop(
    "Package `ggfixest` is required for the event-study plot. Please install it before running this script.",
    call. = FALSE
  )
}

options(tigris_use_cache = FALSE)

zero_pit_states <- c("TX", "FL", "NV", "WA", "WY", "SD", "TN", "AK", "NH")
excluded_state_abbr <- c("AK", "HI", "PR", "VI", "GU", "MP", "AS")
excluded_state_fips <- c("02", "15", "72", "78", "66", "69", "60")

trend_plot_path <- file.path(
  paths$figures_aviationdb,
  "norm_pilot_pop_by_border_county_zero_PIT.png"
)

raw_count_plot_path <- file.path(
  paths$figures_aviationdb,
  "pilot_pop_by_border_county_zero_PIT_raw_counts.png"
)

event_study_path <- file.path(
  paths$figures_aviationdb,
  "pilot_pop_border_county_zero_PIT_event_study.png"
)

diagnostic_map_path <- file.path(
  paths$figures_aviationdb,
  "border_county_treatment_control_map.png"
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

# County Border Sample ----

state_lookup <- tigris::states(year = 2020, cb = TRUE, class = "sf") |>
  filter(!STUSPS %in% excluded_state_abbr) |>
  st_drop_geometry() |>
  transmute(
    state_fips = STATEFP,
    state = STUSPS,
    zero_pit_state = state %in% zero_pit_states
  )

county_shapes <- tigris::counties(year = 2020, cb = TRUE, class = "sf") |>
  filter(!STATEFP %in% excluded_state_fips) |>
  transmute(
    county_fips = GEOID,
    county_name = NAME,
    state_fips = STATEFP,
    geometry
  ) |>
  left_join(state_lookup, by = "state_fips") |>
  st_transform(2163) |>
  sf::st_make_valid()

county_adjacency <- sf::st_touches(county_shapes)

adjacency_pairs <- tibble(from_row = seq_len(nrow(county_shapes))) |>
  mutate(to_row = county_adjacency) |>
  tidyr::unnest(to_row) |>
  filter(from_row < to_row) |>
  transmute(
    county_fips = county_shapes$county_fips[from_row],
    county_name = county_shapes$county_name[from_row],
    state = county_shapes$state[from_row],
    zero_pit_state = county_shapes$zero_pit_state[from_row],
    neighbor_county_fips = county_shapes$county_fips[to_row],
    neighbor_county_name = county_shapes$county_name[to_row],
    neighbor_state = county_shapes$state[to_row],
    neighbor_zero_pit_state = county_shapes$zero_pit_state[to_row]
  ) |>
  filter(
    state != neighbor_state,
    zero_pit_state != neighbor_zero_pit_state
  )

border_counties <- bind_rows(
  adjacency_pairs |>
    transmute(
      county_fips,
      county_name,
      state,
      zero_pit_state
    ),
  adjacency_pairs |>
    transmute(
      county_fips = neighbor_county_fips,
      county_name = neighbor_county_name,
      state = neighbor_state,
      zero_pit_state = neighbor_zero_pit_state
    )
) |>
  distinct() |>
  mutate(
    border_group = if_else(
      zero_pit_state,
      "Zero-PIT Border County",
      "Non-Zero-PIT Border County"
    ),
    border_group = factor(
      border_group,
      levels = c("Zero-PIT Border County", "Non-Zero-PIT Border County")
    ),
    treat = if_else(zero_pit_state, 1L, 0L)
  )

border_county_map_data <- county_shapes |>
  left_join(
    border_counties |>
      st_drop_geometry() |>
      select(county_fips, border_group, treat),
    by = "county_fips"
  ) |>
  mutate(
    map_group = case_when(
      border_group == "Zero-PIT Border County" ~ "Treatment",
      border_group == "Non-Zero-PIT Border County" ~ "Control",
      TRUE ~ "Other"
    ),
    map_group = factor(map_group, levels = c("Treatment", "Control", "Other"))
  )

# Pilot Counts by County-Year ----

pilot_data_clean <- pilot_data |>
  mutate(zip5 = stringr::str_extract(zip_code, "^\\d{5}"))

zip_cleaning_summary <- pilot_data_clean |>
  summarise(
    pilot_rows_total = n(),
    pilot_rows_with_valid_zip = sum(!is.na(zip5)),
    pilot_rows_dropped_invalid_zip = sum(is.na(zip5))
  )

zip_count_data <- pilot_data_clean |>
  filter(!is.na(zip5), year %in% analysis_years) |>
  count(year, zip5, name = "n_atr_pilots_zip")

zcta_shapes_raw <- tigris::zctas(year = 2020, cb = TRUE, class = "sf") |>
  st_transform(2163) |>
  mutate(row_id = row_number())

zcta_county_lookup <- suppressWarnings(
  sf::st_join(
    zcta_shapes_raw |>
      sf::st_point_on_surface() |>
      select(row_id),
    county_shapes |>
      select(county_fips),
    join = sf::st_within,
    left = FALSE,
    largest = TRUE
  )
) |>
  st_drop_geometry() |>
  select(row_id, county_fips)

zip_county_lookup <- zcta_shapes_raw |>
  st_drop_geometry() |>
  transmute(row_id, zip5 = ZCTA5CE20) |>
  left_join(zcta_county_lookup, by = "row_id") |>
  filter(!is.na(county_fips))

county_count_data <- zip_count_data |>
  left_join(zip_county_lookup, by = "zip5") |>
  filter(!is.na(county_fips)) |>
  group_by(year, county_fips) |>
  summarise(
    n_atr_pilots_county = sum(n_atr_pilots_zip),
    .groups = "drop"
  )

border_county_panel <- tidyr::crossing(
  county_fips = border_counties$county_fips,
  year = analysis_years
) |>
  left_join(border_counties, by = "county_fips") |>
  left_join(county_count_data, by = c("county_fips", "year")) |>
  mutate(
    n_atr_pilots_county = coalesce(n_atr_pilots_county, 0L),
    post = if_else(year >= 2018, 1L, 0L),
    ln_county_pilots = log1p(n_atr_pilots_county)
  )

# Diagnostics ----

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

border_group_summary <- border_counties |>
  group_by(border_group) |>
  summarise(
    n_counties = n_distinct(county_fips),
    n_states = n_distinct(state),
    .groups = "drop"
  )

county_year_summary <- border_county_panel |>
  group_by(year, border_group) |>
  summarise(
    n_counties = n_distinct(county_fips),
    n_atr_pilots = sum(n_atr_pilots_county),
    .groups = "drop"
  )

county_count_summary <- border_county_panel |>
  group_by(year) |>
  summarise(
    n_counties_in_sample = n_distinct(county_fips),
    min_county_count = min(n_atr_pilots_county),
    median_county_count = median(n_atr_pilots_county),
    max_county_count = max(n_atr_pilots_county),
    .groups = "drop"
  )

# 1. Border County Diagnostic Map ----

diagnostic_map <- ggplot() +
  geom_sf(
    data = border_county_map_data |>
      filter(map_group == "Other"),
    fill = "grey95",
    color = "white",
    linewidth = 0.05
  ) +
  geom_sf(
    data = border_county_map_data |>
      filter(map_group != "Other"),
    aes(fill = map_group),
    color = "white",
    linewidth = 0.08
  ) +
  geom_sf(
    data = county_shapes |>
      group_by(state) |>
      summarise(geometry = sf::st_union(geometry), .groups = "drop"),
    fill = NA,
    color = "black",
    linewidth = 0.25
  ) +
  scale_fill_manual(
    values = c("Treatment" = "#C62828", "Control" = "grey55"),
    name = NULL
  ) +
  labs(
    title = "County Border Treatment and Control Groups",
    subtitle = "Treatment counties are zero-PIT border counties; control counties are adjacent non-zero-PIT border counties"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 14),
    plot.title = element_text(size = 18),
    plot.subtitle = element_text(size = 12)
  )

ggsave(diagnostic_map_path, diagnostic_map, width = 11, height = 7, dpi = 300)

# 2. Normalized Trend Plot ----

trend_plot_data <- border_county_panel |>
  group_by(year, border_group) |>
  summarise(
    n = sum(n_atr_pilots_county),
    .groups = "drop"
  )

base_year_counts <- trend_plot_data |>
  filter(year == 2001) |>
  transmute(
    border_group,
    base_n = n
  )

trend_plot_data <- trend_plot_data |>
  left_join(base_year_counts, by = "border_group") |>
  mutate(norm_n = n / base_n)

trend_plot <- trend_plot_data |>
  ggplot(aes(
    x = year,
    y = norm_n,
    color = border_group,
    group = border_group,
    shape = border_group,
    linetype = border_group
  )) +
  geom_point(size = 3) +
  geom_line(linewidth = 1.2) +
  geom_vline(
    xintercept = 2017,
    linetype = "dashed",
    color = "red",
    linewidth = 1.2
  ) +
  annotate(
    "text",
    x = 2012,
    y = max(trend_plot_data$norm_n, na.rm = TRUE) * 0.98,
    label = "TCJA SALT\nDeduction Cap",
    hjust = 0,
    size = 4,
    color = "red"
  ) +
  scale_color_manual(values = c("black", "grey50")) +
  scale_x_continuous(
    name = NULL,
    limits = c(2000, 2025),
    breaks = seq(2000, 2025, by = 5)
  ) +
  scale_y_continuous(
    name = "Normalized Pilot Population (basis 1 = 2001)"
  ) +
  labs(shape = NULL, linetype = NULL, color = NULL) +
  guides(
    shape = guide_legend(nrow = 2),
    linetype = guide_legend(nrow = 2)
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.title.y = element_text(size = 18),
    axis.ticks = element_line(color = "black", linewidth = 0.8),
    legend.title = NULL,
    legend.text = element_text(size = 18),
    legend.key.width = unit(2.5, "cm"),
    legend.position = "bottom"
  )

ggsave(trend_plot_path, trend_plot, width = 8, height = 6)

# 3. Raw Count Trend Plot ----

raw_count_plot <- trend_plot_data |>
  ggplot(aes(
    x = year,
    y = n,
    color = border_group,
    group = border_group,
    shape = border_group,
    linetype = border_group
  )) +
  geom_point(size = 3) +
  geom_line(linewidth = 1.2) +
  geom_vline(
    xintercept = 2017,
    linetype = "dashed",
    color = "red",
    linewidth = 1.2
  ) +
  annotate(
    "text",
    x = 2012,
    y = max(trend_plot_data$n, na.rm = TRUE) * 0.98,
    label = "TCJA SALT\nDeduction Cap",
    hjust = 0,
    size = 4,
    color = "red"
  ) +
  scale_color_manual(values = c("black", "grey50")) +
  scale_x_continuous(
    name = NULL,
    limits = c(2000, 2025),
    breaks = seq(2000, 2025, by = 5)
  ) +
  scale_y_continuous(
    name = "Pilot Population"
  ) +
  labs(shape = NULL, linetype = NULL, color = NULL) +
  guides(
    shape = guide_legend(nrow = 2),
    linetype = guide_legend(nrow = 2)
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.title.y = element_text(size = 18),
    axis.ticks = element_line(color = "black", linewidth = 0.8),
    legend.title = NULL,
    legend.text = element_text(size = 18),
    legend.key.width = unit(2.5, "cm"),
    legend.position = "bottom"
  )

ggsave(raw_count_plot_path, raw_count_plot, width = 8, height = 6)

# 4. Event Study ----

did_model <- fixest::feols(
  ln_county_pilots ~ treat:post | county_fips + year,
  data = border_county_panel,
  cluster = ~county_fips
)

did_estimate <- coef(did_model)[["treat:post"]]
did_se <- sqrt(vcov(did_model)["treat:post", "treat:post"])

event_study_model <- fixest::feols(
  ln_county_pilots ~ i(year, treat, ref = 2017) | county_fips + year,
  data = border_county_panel,
  cluster = ~county_fips
)

event_study_ci <- stats::confint(event_study_model)

event_study_plot <- ggfixest::ggiplot(
  event_study_model,
  ref.line = 2017,
  ref.line.par = list(col = "red", lty = 2, lwd = 1),
  main = "",
  xlab = NULL,
  ylab = NULL,
  geom_style = "ribbon",
  theme = theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16),
      axis.text.x = element_text(hjust = 1, size = 13),
      axis.text.y = element_text(size = 13),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "none"
    )
) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 8)
  ) +
  annotate(
    "label",
    x = Inf,
    y = min(event_study_ci[, 1], na.rm = TRUE),
    label = sprintf("DID: %.3f (%.3f)", did_estimate, did_se),
    hjust = 1.1,
    vjust = -0.6,
    size = 4.5
  )

ggsave(event_study_path, event_study_plot, width = 8, height = 6)

# Reporting ----

message("Saved treatment/control diagnostic map to ", diagnostic_map_path)
message("Saved normalized trend figure to ", trend_plot_path)
message("Saved raw count trend figure to ", raw_count_plot_path)
message("Saved county-border event-study figure to ", event_study_path)
message(
  "Pilot rows kept after ZIP cleaning: ",
  scales::comma(zip_cleaning_summary$pilot_rows_with_valid_zip),
  " of ",
  scales::comma(zip_cleaning_summary$pilot_rows_total),
  " (dropped ",
  scales::comma(zip_cleaning_summary$pilot_rows_dropped_invalid_zip),
  " invalid ZIP rows)."
)

message("Border county sample summary:")
print(border_group_summary)

message("County-year totals by year and border group:")
print(county_year_summary)

message("County count summary by year:")
print(county_count_summary)

message("ZIP-to-county match summary:")
print(matched_zip_summary)
