# Purpose: build legacy-compatible cleaned pilot datasets without plots.
# Inputs: FAA-rich pooled panel, state crosswalk, and total working population files
# Outputs: `data/derived/main_us_pilots_any.csv`, `data/derived/main_us_pilots_atr.csv`, and `data/derived/sum_stat_prop_atr_pilots.csv`

# Setup ----

source("code/00_setup/00_packages_paths.R")
source("code/utils/cleaning_helpers.R")

# The cleaner still reads the FAA-rich intermediate so the legacy sample logic can be replicated.
input_path <- Sys.getenv(
  "FAA_RICH_INGEST_INPUT",
  unset = file.path(paths$intermediate, "faa_pilot_rich_panel.csv")
)

# Input Loading ----

pilot_data <- read_csv(
  input_path,
  na = c("", "NA"),
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
) |>
  mutate(year = as.integer(year)) |>
  filter(year != 2025)

# Sample Restrictions and Enrichment ----

processed_pilot_data <- pilot_data |>
  filter(!is.na(country)) |>
  group_by(unique_id) |>
  mutate(
    num_years = n_distinct(year),
    num_USA = sum(country == "USA"),
    never_in_US = num_USA == 0,
    migrate_in_out_US = (num_USA > 0) & (num_USA < num_years)
  ) |>
  ungroup()

always_in_main_us_pilots <- processed_pilot_data |>
  filter(!never_in_US) |>
  filter(!migrate_in_out_US) |>
  group_by(unique_id) |>
  mutate(
    num_not_in_main_US = sum(state %in% excluded_territories),
    always_in_main_US = num_not_in_main_US == 0
  ) |>
  filter(always_in_main_US) |>
  ungroup() |>
  select(
    -all_of(c(
      "num_USA",
      "never_in_US",
      "migrate_in_out_US",
      "num_not_in_main_US",
      "always_in_main_US"
    ))
  )

state_fips_crosswalk <- load_state_fips_crosswalk(paths)

main_us_pilots_any <- state_fips_crosswalk |>
  left_join(always_in_main_us_pilots, by = "state") |>
  relocate(year) |>
  mutate(
    level_collapsed = case_when(
      level == "A" ~ "ATR",
      level == "C" ~ "C",
      TRUE ~ "other"
    )
  )

main_us_pilots_atr <- main_us_pilots_any |>
  filter(level_collapsed == "ATR") |>
  select(-level_collapsed, -level, -expire_date)

csv_files <- list.files(
  paths$raw_tot_working_pop,
  pattern = "tot_working_pop\\.csv$",
  full.names = TRUE
)

tot_working_pop <- read_csv(
  csv_files,
  id = "file",
  show_col_types = FALSE,
  col_types = cols(.default = col_guess())
) |>
  mutate(year = as.numeric(str_extract(file, "\\d{4}"))) |>
  select(-file) |>
  rename(state = statefips)

sum_stat_prop_pilots <- main_us_pilots_atr |>
  count(year, state, name = "n_atr_pilots") |>
  left_join(tot_working_pop, by = c("year", "state")) |>
  mutate(prop_atr_pilots = n_atr_pilots / tot_work_pop * 100)

# Outputs ----

write_csv(main_us_pilots_any, file.path(paths$derived, "main_us_pilots_any.csv"))
write_csv(main_us_pilots_atr, file.path(paths$derived, "main_us_pilots_atr.csv"))
write_csv(
  sum_stat_prop_pilots,
  file.path(paths$derived, "sum_stat_prop_atr_pilots.csv")
)

# Reporting ----

message("Wrote cleaned pilot datasets to ", paths$derived)
message("main_us_pilots_any rows: ", nrow(main_us_pilots_any))
message("main_us_pilots_atr rows: ", nrow(main_us_pilots_atr))
message("sum_stat_prop_atr_pilots rows: ", nrow(sum_stat_prop_pilots))
