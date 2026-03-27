# Purpose: build an ATR pilot mover panel for downstream mover and migration-flow analysis.
# Inputs: `data/derived/main_us_pilots_atr.csv`
# Outputs: `data/derived/main_us_pilots_atr_mover_panel.csv`

# Setup ----

source("code/00_setup/00_packages_paths.R")

# Inputs ----

main_us_pilots_atr <- read_csv(
  file.path(paths$derived, "main_us_pilots_atr.csv"),
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
) |>
  mutate(year = as.integer(year))

# Derived Dataset ----

mover_panel <- main_us_pilots_atr |>
  select(
    "year",
    "fips",
    "statefull",
    "state",
    "unique_id",
    "first_name",
    "last_name",
    "street_1",
    "city",
    "zip_code",
    "num_years"
  ) |>
  arrange(unique_id, year) |>
  group_by(unique_id) |>
  mutate(
    dest_state = state,
    origin_state = lag(dest_state),
    moved = if_else(
      !is.na(origin_state) & (dest_state != origin_state),
      1L,
      0L
    )
  ) |>
  relocate("origin_state", "moved", .after = "dest_state") |>
  mutate(num_moves = sum(moved, na.rm = TRUE)) |>
  relocate("num_moves", .after = "moved") |>
  ungroup()

# Outputs ----

output_path <- file.path(paths$derived, "main_us_pilots_atr_mover_panel.csv")
write_csv(mover_panel, output_path)

# Reporting ----

message("Wrote ATR mover panel to ", output_path)
message("main_us_pilots_atr_mover_panel rows: ", nrow(mover_panel))
