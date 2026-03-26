# Purpose: build an ATR pilot mover panel for downstream mover and migration-flow analysis.
# Inputs: `data/derived/main_us_pilots_atr.csv`
# Outputs: `data/derived/main_us_pilots_atr_mover_panel.csv`

# Setup ----

source("code/00_setup/00_packages_paths.R")
source("code/utils/cleaning_helpers.R")

# Inputs ----

main_us_pilots_atr <- readr::read_csv(
  file.path(paths$derived, "main_us_pilots_atr.csv"),
  show_col_types = FALSE,
  col_types = readr::cols(.default = readr::col_character())
) |>
  dplyr::mutate(year = as.integer(.data$year))

# Derived Dataset ----

mover_panel <- main_us_pilots_atr |>
  select_mover_panel_columns() |>
  build_pilot_mover_panel() |>
  add_pilot_move_counts()

# Outputs ----

output_path <- file.path(paths$derived, "main_us_pilots_atr_mover_panel.csv")
readr::write_csv(mover_panel, output_path)

# Reporting ----

message("Wrote ATR mover panel to ", output_path)
message("main_us_pilots_atr_mover_panel rows: ", nrow(mover_panel))
