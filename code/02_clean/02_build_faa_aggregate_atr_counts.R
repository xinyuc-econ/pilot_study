# Purpose: build state-year official FAA aggregate ATR pilot counts from annual civil airmen workbooks.
# Inputs: annual FAA aggregate workbooks under `/Users/xinyuc/Documents/pilots/data/raw/airmen_aggregate_stats/`
# Outputs: `data/derived/aviationdb/faa_official_atr_pilots_by_state_year.csv`

# Setup ----

source("code/00_setup/00_packages_paths.R")
source("code/utils/cleaning_helpers.R")

aggregate_stats_dir <- file.path(paths$raw_data_root, "raw", "airmen_aggregate_stats")
output_path <- file.path(paths$derived_aviationdb, "faa_official_atr_pilots_by_state_year.csv")

read_faa_aggregate_workbook <- function(workbook_path) {
  workbook_name <- basename(workbook_path)
  workbook_year <- str_extract(workbook_name, "^\\d{4}") |> as.integer()
  workbook_sheets <- excel_sheets(workbook_path)

  sheet_name <- case_when(
    "Table 5" %in% workbook_sheets ~ "Table 5",
    "T5" %in% workbook_sheets ~ "T5",
    TRUE ~ NA_character_
  )

  if (is.na(sheet_name)) {
    stop(
      paste(
        "Could not find FAA ATR sheet in workbook:",
        workbook_path,
        "\nExpected one of: Table 5, T5"
      ),
      call. = FALSE
    )
  }

  read_excel(workbook_path, sheet = sheet_name, skip = 4) |>
    clean_names() |>
    select("faa_region_and_state", "airline_transport_1") |>
    rename(
      statefull = "faa_region_and_state",
      faa_n_atr_pilots = "airline_transport_1"
    ) |>
    mutate(year = workbook_year)
}

# Input Discovery ----

workbook_paths <- list.files(
  aggregate_stats_dir,
  pattern = "^\\d{4}-civil-airmen-stats\\.xlsx$",
  full.names = TRUE
)

if (length(workbook_paths) == 0) {
  stop(
    paste("No FAA aggregate workbooks found in", aggregate_stats_dir),
    call. = FALSE
  )
}

workbook_paths <- workbook_paths[order(as.integer(str_extract(basename(workbook_paths), "^\\d{4}")))]

state_fips_crosswalk <- load_state_fips_crosswalk(paths)

# Build FAA Aggregate State-Year Dataset ----

faa_official_atr_counts <- purrr::map_dfr(workbook_paths, read_faa_aggregate_workbook) |>
  mutate(
    statefull = case_when(
      statefull == "Alaskan Region--Total" ~ "Alaska",
      TRUE ~ statefull
    )
  ) |>
  filter(statefull %in% state_fips_crosswalk$statefull) |>
  left_join(state_fips_crosswalk, by = "statefull") |>
  relocate("year", "statefull", "state", "fips", "faa_n_atr_pilots") |>
  arrange(year, fips)

# Outputs ----

dir.create(paths$derived_aviationdb, recursive = TRUE, showWarnings = FALSE)

write_csv(faa_official_atr_counts, output_path)

# Reporting ----

message("Wrote FAA aggregate ATR counts to ", output_path)
message("Years covered: ", min(faa_official_atr_counts$year), "-", max(faa_official_atr_counts$year))
message("Rows written: ", nrow(faa_official_atr_counts))
