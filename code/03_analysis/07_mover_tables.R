# Purpose: reproduce legacy mover summary and pilot migration flow tables from the ATR mover panel.
# Inputs: `data/derived/main_us_pilots_atr_mover_panel.csv` and `data/derived/sum_stat_prop_atr_pilots.csv`
# Outputs: four LaTeX tables in `output/tables/`

# Setup ----

source("code/00_setup/00_packages_paths.R")

build_migration_flow_table <- function(data) {
  flow_data <- data |>
    filter(!is.na(origin_state), !is.na(dest_state))

  as.matrix(table(flow_data$origin_state, flow_data$dest_state))
}

mover_panel <- read_csv(
  file.path(paths$derived, "main_us_pilots_atr_mover_panel.csv"),
  show_col_types = FALSE
) |>
  mutate(year = as.integer(year))

sum_stat_prop_atr_pilots <- read_csv(
  file.path(paths$derived, "sum_stat_prop_atr_pilots.csv"),
  show_col_types = FALSE
) |>
  mutate(year = as.integer(year))

prop_moved_table_path <- file.path(paths$tables, "prop_moved_by_year.tex")
all_states_flow_table_path <- file.path(paths$tables, "total_outflow_observed_all_states.tex")
top10_flow_table_path <- file.path(paths$tables, "total_outflow_observed_top10.tex")
annual_top10_flow_table_path <- file.path(paths$tables, "annual_outflow_observed_top10.tex")

# Derived Analysis Inputs ----

flow_panel <- mover_panel |>
  arrange(unique_id, year) |>
  group_by(unique_id) |>
  mutate(
    lag_year = lag(year),
    time_period_yrs = as.integer(year - lag_year)
  ) |>
  relocate("lag_year", "time_period_yrs", .after = "year") |>
  ungroup() |>
  filter(!is.na(lag_year))

period_month_map <- tribble(
  ~period_key,   ~time_period,            ~months_between,
  "2009-2010", "11/2009 - 05/2010",  6,
  "2010-2011", "05/2010 - 09/2011", 16,
  "2011-2014", "09/2011 - 09/2014", 36,
  "2014-2015", "09/2014 - 09/2015", 12,
  "2015-2016", "09/2015 - 11/2016", 14,
  "2016-2017", "11/2016 - 09/2017", 10,
  "2017-2019", "06/2017 - 06/2019", 24,
  "2019-2022", "06/2019 - 10/2022", 40,
  "2022-2024", "10/2022 - 09/2024", 23
)

prop_moved_by_period <- mover_panel |>
  group_by(year) |>
  summarise(
    n_pilots = n(),
    n_moved = sum(moved, na.rm = TRUE),
    prop_moved = n_moved / n_pilots * 100,
    .groups = "drop"
  ) |>
  mutate(
    lag_year = lag(year),
    period_key = paste(lag_year, year, sep = "-")
  ) |>
  filter(!is.na(lag_year)) |>
  left_join(period_month_map, by = "period_key") |>
  mutate(monthly_prop_moved = prop_moved / months_between) |>
  select(
    "time_period",
    "n_pilots",
    "n_moved",
    "monthly_prop_moved"
  ) |>
  rename(
    `Time period` = "time_period",
    `# of pilots` = "n_pilots",
    `# of movers` = "n_moved",
    `Ave. monthly % moved` = "monthly_prop_moved"
  ) |>
  as.data.frame()

all_states_flow_table <- build_migration_flow_table(flow_panel)

top10_states <- sum_stat_prop_atr_pilots |>
  group_by(state) |>
  summarise(avg_n_atr_pilots = mean(n_atr_pilots), .groups = "drop") |>
  arrange(desc(avg_n_atr_pilots), state) |>
  slice_head(n = 10L) |>
  pull(state)

top10_flow_panel <- flow_panel |>
  filter(origin_state %in% top10_states, dest_state %in% top10_states)

top10_flow_table <- build_migration_flow_table(top10_flow_panel)

annual_top10_flow_table <- top10_flow_panel |>
  filter(time_period_yrs == 1) |>
  build_migration_flow_table() / (2017 - 2014 + 2011 - 2009)

# 1. Tables ----

## 1.1 Proportion of Pilots Who Moved by Time Period ----

print(
  xtable(
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
  xtable(
    all_states_flow_table,
    caption = "Total number of moves of pilots observed, 2009-2024"
  ),
  file = all_states_flow_table_path
)

## 1.3 Total Observed Migration Flows for Top 10 States ----

print(
  xtable(
    top10_flow_table,
    caption = "Total observed migration flows of pilots, top 10 states, 2009-2024"
  ),
  file = top10_flow_table_path
)

## 1.4 Average Annual Migration Flows for Top 10 States ----

print(
  xtable(
    annual_top10_flow_table,
    caption = "Average annual migration flow of pilots, top 10 states, 2009-2011 and 2014-2017"
  ),
  file = annual_top10_flow_table_path
)

# Reporting ----

message("Wrote mover summary table to ", prop_moved_table_path)
message("Wrote migration flow tables to ", paths$tables)
