# Purpose: build a state-year-group panel for a simple post-2018 event study on pilot counts.
# Inputs: `data/derived/aviationdb/main_us_pilots_any.csv`
# Outputs: `data/derived/aviationdb/event_study_state_counts_panel.csv`

# Setup ----

source("code/00_setup/00_packages_paths.R")

# Input Loading ----

main_us_pilots_any <- read_csv(
  file.path(paths$derived_aviationdb, "main_us_pilots_any.csv"),
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
) |>
  mutate(year = as.integer(year)) |>
  filter(year %in% analysis_years)

# Derived Dataset ----

event_study_panel <- main_us_pilots_any |>
  filter(certificate_level != "C") |>
  mutate(pilot_group = if_else(certificate_level == "A", "atr", "other")) |>
  count(fips, statefull, state, year, pilot_group, name = "n_pilots") |>
  mutate(
    atr_group = if_else(pilot_group == "atr", 1L, 0L),
    post_2018 = if_else(year >= 2018L, 1L, 0L),
    log_n_pilots = log(n_pilots)
  ) |>
  arrange(state, pilot_group, year)

# Outputs ----

dir.create(paths$derived_aviationdb, recursive = TRUE, showWarnings = FALSE)

output_path <- file.path(paths$derived_aviationdb, "event_study_state_counts_panel.csv")
write_csv(event_study_panel, output_path)

# Reporting ----

message("Wrote event-study panel to ", output_path)
message("Rows: ", nrow(event_study_panel))
message("Years: ", min(event_study_panel$year), "-", max(event_study_panel$year))
message("States: ", n_distinct(event_study_panel$state))
