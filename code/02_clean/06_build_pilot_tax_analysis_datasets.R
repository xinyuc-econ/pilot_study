# Purpose: build merged pilot-tax analysis datasets for SOI and BLS robustness cases.
# Inputs: `data/derived/aviationdb/sum_stat_prop_atr_pilots.csv`, `data/derived/aviationdb/all_years_pit_soi_wide.csv`, and `data/derived/aviationdb/all_years_pit_bls.csv`
# Outputs: `data/derived/aviationdb/pilot_tax_analysis_soi.csv` and `data/derived/aviationdb/pilot_tax_analysis_bls.csv`

# Setup ----

source("code/00_setup/00_packages_paths.R")
source("code/utils/cleaning_helpers.R")

assert_unique_case_rows <- function(data, keys, dataset_name) {
  duplicates <- data |>
    count(across(all_of(keys)), name = "n") |>
    filter(n > 1)

  if (nrow(duplicates) > 0) {
    stop(
      paste0(
        dataset_name,
        " contains duplicated rows for keys: ",
        paste(keys, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  data
}

prop_pilots <- read_csv(
  file.path(paths$derived_aviationdb, "sum_stat_prop_atr_pilots.csv"),
  show_col_types = FALSE
) |>
  filter(year %in% analysis_years)

pit_soi_wide <- read_csv(
  file.path(paths$derived_aviationdb, "all_years_pit_soi_wide.csv"),
  show_col_types = FALSE
)

pit_bls <- read_csv(
  file.path(paths$derived_aviationdb, "all_years_pit_bls.csv"),
  show_col_types = FALSE
)

state_crosswalk <- load_state_fips_crosswalk(paths)

# Dataset Builders ----

pit_soi_long <- pit_soi_wide |>
  left_join(
    state_crosswalk |>
      select("fips", "state"),
    by = "fips"
  ) |>
  tidyr::pivot_longer(
    cols = tidyselect::matches("^(astr|atr)_"),
    names_to = c(".value", "percentile"),
    names_sep = "_"
  ) |>
  mutate(method = "soi")

pilot_tax_analysis_soi <- pit_soi_long |>
  left_join(prop_pilots, by = c("year", "state")) |>
  select(
    "method",
    "percentile",
    "year",
    "fips",
    "state",
    "n_atr_pilots",
    "tot_work_pop",
    "prop_atr_pilots",
    "astr",
    "atr"
  )

if (anyNA(pilot_tax_analysis_soi$prop_atr_pilots)) {
  stop("SOI pilot-tax analysis data has unmatched pilot-share rows.", call. = FALSE)
}

pilot_tax_analysis_soi <- assert_unique_case_rows(
  pilot_tax_analysis_soi,
  keys = c("year", "fips", "percentile"),
  dataset_name = "SOI pilot-tax analysis data"
)

pilot_tax_analysis_bls <- pit_bls |>
  left_join(
    state_crosswalk |>
      select("fips", "state"),
    by = "fips"
  ) |>
  mutate(method = "bls") |>
  left_join(prop_pilots, by = c("year", "state")) |>
  select(
    "method",
    "pilot_type",
    "percentile",
    "year",
    "fips",
    "state",
    "n_atr_pilots",
    "tot_work_pop",
    "prop_atr_pilots",
    "astr",
    "atr"
  )

if (anyNA(pilot_tax_analysis_bls$prop_atr_pilots)) {
  stop("BLS pilot-tax analysis data has unmatched pilot-share rows.", call. = FALSE)
}

pilot_tax_analysis_bls <- assert_unique_case_rows(
  pilot_tax_analysis_bls,
  keys = c("year", "fips", "pilot_type", "percentile"),
  dataset_name = "BLS pilot-tax analysis data"
)

# Outputs ----

dir.create(paths$derived_aviationdb, recursive = TRUE, showWarnings = FALSE)

write_csv(
  pilot_tax_analysis_soi,
  file.path(paths$derived_aviationdb, "pilot_tax_analysis_soi.csv")
)

write_csv(
  pilot_tax_analysis_bls,
  file.path(paths$derived_aviationdb, "pilot_tax_analysis_bls.csv")
)

# Reporting ----

message("Wrote SOI pilot-tax analysis dataset to ", file.path(paths$derived_aviationdb, "pilot_tax_analysis_soi.csv"))
message("Wrote BLS pilot-tax analysis dataset to ", file.path(paths$derived_aviationdb, "pilot_tax_analysis_bls.csv"))
