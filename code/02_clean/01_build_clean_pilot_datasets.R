# Purpose: build annual AviationDB-based cleaned pilot datasets.
# Inputs: `data/intermediate/pilot_ingest_panel.csv`, state crosswalk, and total working population files
# Outputs: `data/derived/aviationdb/main_us_pilots_any.csv`, `data/derived/aviationdb/main_us_pilots_atr.csv`, and `data/derived/aviationdb/sum_stat_prop_atr_pilots.csv`

# Setup ----

source("code/00_setup/00_packages_paths.R")
source("code/utils/cleaning_helpers.R")

input_path <- Sys.getenv(
  "PILOT_INGEST_INPUT",
  unset = file.path(paths$intermediate, "pilot_ingest_panel.csv")
)

# Input Loading ----

pilot_data <- read_csv(
  input_path,
  na = c("", "NA"),
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
) |>
  mutate(
    year = as.integer(year),
    state = str_trim(state),
    certificate_level = str_trim(certificate_level)
  ) |>
  filter(year %in% analysis_years)

state_fips_crosswalk <- load_state_fips_crosswalk(paths)
valid_states <- state_fips_crosswalk$state

# Sample Restrictions and Enrichment ----

main_us_pilots_any <- pilot_data |>
  filter(!is.na(unique_id), !is.na(state), state %in% valid_states) |>
  left_join(state_fips_crosswalk, by = "state") |>
  arrange(unique_id, year) |>
  group_by(unique_id) |>
  mutate(num_years = n_distinct(year)) |>
  ungroup() |>
  mutate(
    level_collapsed = case_when(
      certificate_level == "A" ~ "ATR",
      certificate_level == "C" ~ "Commercial",
      TRUE ~ "Other"
    )
  ) |>
  select(
    "year",
    "fips",
    "statefull",
    "state",
    "unique_id",
    "zip_code",
    "num_years",
    "certificate_level",
    "level_collapsed",
    "source"
  )

main_us_pilots_atr <- main_us_pilots_any |>
  filter(level_collapsed == "ATR") |>
  select(-"level_collapsed")

tot_working_pop <- load_total_working_population(paths)

sum_stat_prop_pilots <- main_us_pilots_atr |>
  count(year, state, name = "n_atr_pilots") |>
  left_join(tot_working_pop, by = c("year", "state")) |>
  mutate(prop_atr_pilots = n_atr_pilots / tot_work_pop * 100)

if (anyNA(sum_stat_prop_pilots$tot_work_pop)) {
  missing_rows <- sum_stat_prop_pilots |>
    filter(is.na(tot_work_pop)) |>
    distinct(year, state)

  stop(
    paste0(
      "Total working population data is missing for some pilot state-years.\n",
      paste(sprintf("%s %s", missing_rows$year, missing_rows$state), collapse = "\n")
    ),
    call. = FALSE
  )
}

# Outputs ----

dir.create(paths$derived_aviationdb, recursive = TRUE, showWarnings = FALSE)

write_csv(main_us_pilots_any, file.path(paths$derived_aviationdb, "main_us_pilots_any.csv"))
write_csv(main_us_pilots_atr, file.path(paths$derived_aviationdb, "main_us_pilots_atr.csv"))
write_csv(
  sum_stat_prop_pilots,
  file.path(paths$derived_aviationdb, "sum_stat_prop_atr_pilots.csv")
)

# Reporting ----

message("Wrote cleaned pilot datasets to ", paths$derived_aviationdb)
message("main_us_pilots_any rows: ", nrow(main_us_pilots_any))
message("main_us_pilots_atr rows: ", nrow(main_us_pilots_atr))
message("sum_stat_prop_atr_pilots rows: ", nrow(sum_stat_prop_pilots))
