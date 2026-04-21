# Purpose: identify FAA large, medium, and small hub airports and plot their locations in the continental U.S.
# Inputs: `data/raw/NPIAS-2023-2027-Appendix-A.xlsx` and `data/raw/Airports.csv`
# Outputs: one FAA hub airport map in `output/aviationdb/figures/`

# Setup ----

source("code/00_setup/00_packages_paths.R")

npiAS_path <- file.path(paths$project_root, "data", "raw", "NPIAS-2023-2027-Appendix-A.xlsx")
airports_path <- file.path(paths$project_root, "data", "raw", "Airports.csv")

hub_map_path <- file.path(
  paths$figures_aviationdb,
  "faa_major_hub_airports_map.png"
)

excluded_states <- c("AK", "HI", "PR", "VI", "GU", "MP", "AS")
zero_pit_states <- c("TX", "FL", "NV", "WA", "WY", "SD", "TN", "NH")

dir.create(paths$figures_aviationdb, recursive = TRUE, showWarnings = FALSE)

# Inputs ----

npiAS_airports <- readxl::read_excel(
  npiAS_path,
  sheet = "All NPIAS Airports"
) |>
  janitor::clean_names()

airport_coordinates <- read_csv(
  airports_path,
  show_col_types = FALSE
) |>
  transmute(
    ident = IDENT,
    airport_name_airports = NAME,
    airport_state = STATE,
    longitude = X,
    latitude = Y
  )

# Hub Airport Merge ----

hub_airports <- npiAS_airports |>
  filter(!is.na(hub_fy23)) |>
  mutate(
    hub_category = case_when(
      hub_fy23 == "L" ~ "Large Hub",
      hub_fy23 == "M" ~ "Medium Hub",
      hub_fy23 == "S" ~ "Small Hub",
      hub_fy23 == "N" ~ "Nonhub",
      TRUE ~ NA_character_
    )
  )

plotted_hub_airports <- hub_airports |>
  filter(hub_fy23 %in% c("L", "M", "S")) |>
  select(
    loc_id,
    airport_name_npias = airport,
    npias_state = state,
    hub_fy23,
    hub_category,
    enplaned_cy21
  )

plotted_hub_airports_geo <- plotted_hub_airports |>
  left_join(
    airport_coordinates,
    by = c("loc_id" = "ident")
  )

plotted_hub_airports_contiguous <- plotted_hub_airports_geo |>
  filter(
    !is.na(longitude),
    !is.na(latitude),
    !airport_state %in% excluded_states
  ) |>
  mutate(
    hub_category = factor(
      hub_category,
      levels = c("Large Hub", "Medium Hub", "Small Hub")
    )
  )

merge_summary <- tibble(
  n_npias_hubs_all = nrow(hub_airports),
  n_hubs_plotted_kept = nrow(plotted_hub_airports),
  n_hubs_plotted_matched = sum(!is.na(plotted_hub_airports_geo$longitude)),
  n_hubs_plotted_contiguous = nrow(plotted_hub_airports_contiguous)
)

# Figure ----

us_states_map <- usmap::us_map("states") |>
  filter(!abbr %in% c("AK", "HI")) |>
  mutate(
    zero_pit_group = if_else(abbr %in% zero_pit_states, "Zero-PIT State", "Other State")
  )

plotted_hub_airports_sf <- plotted_hub_airports_contiguous |>
  sf::st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  ) |>
  sf::st_transform(sf::st_crs(us_states_map))

hub_map <- ggplot() +
  geom_sf(
    data = us_states_map,
    aes(fill = zero_pit_group),
    color = "black",
    linewidth = 0.3
  ) +
  geom_sf(
    data = plotted_hub_airports_sf,
    aes(
      color = hub_category,
      shape = hub_category
    ),
    size = 2.8,
    alpha = 0.9
  ) +
  scale_fill_manual(
    values = c("Zero-PIT State" = "#D6EAF8", "Other State" = "grey95"),
    guide = "none"
  ) +
  scale_color_manual(
    values = c(
      "Large Hub" = "#B22222",
      "Medium Hub" = "#4F6D7A",
      "Small Hub" = "#6C8E5E"
    ),
    name = NULL
  ) +
  scale_shape_manual(
    values = c("Large Hub" = 16, "Medium Hub" = 17, "Small Hub" = 15),
    name = NULL
  ) +
  labs(
    title = "FAA Large, Medium, and Small Hub Airports",
    subtitle = "Zero-PIT states are shaded light blue; FAA hub status is based on passenger enplanement share"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 18),
    plot.subtitle = element_text(size = 11)
  )

ggsave(
  filename = hub_map_path,
  plot = hub_map,
  width = 11,
  height = 7,
  dpi = 300
)

# Reporting ----

message("Saved FAA hub airport map to ", hub_map_path)
print(merge_summary)
