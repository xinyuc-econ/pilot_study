# Purpose: build an ATR pilot mover panel for downstream mover and migration-flow analysis.
# Inputs: `data/derived/aviationdb/main_us_pilots_atr.csv`
# Outputs: `data/derived/aviationdb/main_us_pilots_atr_mover_panel.csv`

# Setup ----

source("code/00_setup/00_packages_paths.R")
source("code/utils/cleaning_helpers.R")

# Inputs ----

main_us_pilots_atr <- read_csv(
  file.path(paths$derived_aviationdb, "main_us_pilots_atr.csv"),
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
    "zip_code",
    "num_years"
  ) |>
  build_adjacent_year_panel() |>
  mutate(dest_state = state) |>
  relocate("lag_year", "origin_state", "is_adjacent_year", "moved", .after = "dest_state") |>
  group_by(unique_id) |>
  mutate(num_moves = sum(moved, na.rm = TRUE)) |>
  relocate("num_moves", .after = "moved") |>
  ungroup()

# Outputs ----

dir.create(paths$derived_aviationdb, recursive = TRUE, showWarnings = FALSE)

output_path <- file.path(paths$derived_aviationdb, "main_us_pilots_atr_mover_panel.csv")
write_csv(mover_panel, output_path)

# Reporting ----

message("Wrote ATR mover panel to ", output_path)
message("main_us_pilots_atr_mover_panel rows: ", nrow(mover_panel))
