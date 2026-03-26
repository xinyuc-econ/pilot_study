# Purpose: reproduce legacy mover summary and pilot migration flow tables from the ATR mover panel.
# Inputs: `data/derived/main_us_pilots_atr_mover_panel.csv` and `data/derived/sum_stat_prop_atr_pilots.csv`
# Outputs: four LaTeX tables in `output/tables/`

# Setup ----

source("code/00_setup/00_packages_paths.R")
source("code/utils/cleaning_helpers.R")

mover_panel <- readr::read_csv(
  file.path(paths$derived, "main_us_pilots_atr_mover_panel.csv"),
  show_col_types = FALSE
) |>
  dplyr::mutate(year = as.integer(.data$year))

sum_stat_prop_atr_pilots <- readr::read_csv(
  file.path(paths$derived, "sum_stat_prop_atr_pilots.csv"),
  show_col_types = FALSE
) |>
  dplyr::mutate(year = as.integer(.data$year))

prop_moved_table_path <- file.path(paths$tables, "prop_moved_by_year.tex")
all_states_flow_table_path <- file.path(paths$tables, "total_outflow_observed_all_states.tex")
top10_flow_table_path <- file.path(paths$tables, "total_outflow_observed_top10.tex")
annual_top10_flow_table_path <- file.path(paths$tables, "annual_outflow_observed_top10.tex")

# Derived Analysis Inputs ----

flow_panel <- mover_panel |>
  add_flow_time_fields() |>
  dplyr::filter(!is.na(.data$lag_year))

prop_moved_by_period <- build_mover_period_summary(mover_panel)
all_states_flow_table <- build_migration_flow_table(flow_panel)

top10_states <- select_top_states_by_average_atr_count(sum_stat_prop_atr_pilots, n_states = 10L)

top10_flow_panel <- flow_panel |>
  dplyr::filter(.data$origin_state %in% top10_states, .data$dest_state %in% top10_states)

top10_flow_table <- build_migration_flow_table(top10_flow_panel)

annual_top10_flow_table <- top10_flow_panel |>
  dplyr::filter(.data$time_period_yrs == 1) |>
  build_migration_flow_table() / (2017 - 2014 + 2011 - 2009)

# 1. Tables ----

## 1.1 Proportion of Pilots Who Moved by Time Period ----

print(
  xtable::xtable(
    prop_moved_by_period,
    caption = "Proportion of pilots who moved state-to-state by time period",
    align = "ccccc",
    digits = c(0, 0, 0, 0, 2)
  ),
  include.rownames = FALSE,
  file = prop_moved_table_path
)

## 1.2 Total Observed Migration Flows Across All States ----

print(
  xtable::xtable(
    all_states_flow_table,
    caption = "Total number of moves of pilots observed, 2009-2024"
  ),
  file = all_states_flow_table_path
)

## 1.3 Total Observed Migration Flows for Top 10 States ----

print(
  xtable::xtable(
    top10_flow_table,
    caption = "Total observed migration flows of pilots, top 10 states, 2009-2024"
  ),
  file = top10_flow_table_path
)

## 1.4 Average Annual Migration Flows for Top 10 States ----

print(
  xtable::xtable(
    annual_top10_flow_table,
    caption = "Average annual migration flow of pilots, top 10 states, 2009-2011 and 2014-2017"
  ),
  file = annual_top10_flow_table_path
)

# Reporting ----

message("Wrote mover summary table to ", prop_moved_table_path)
message("Wrote migration flow tables to ", paths$tables)
