# Purpose: validate that FAA and AviationDB normalize to the same schema and report overlap-year count differences.
# Inputs: FAA flat files and AviationDB files for overlapping years
# Outputs: console validation only

# Setup ----

source("code/00_setup/00_packages_paths.R")
source("code/utils/ingest_helpers.R")

# Load Source Inputs ----

faa_inputs <- discover_ingest_inputs(paths, source = "faa_flat")
aviationdb_inputs <- discover_ingest_inputs(paths, source = "aviationdb")

# Compare One Overlapping Year ----

overlap_years <- intersect(faa_inputs$year, aviationdb_inputs$year)

if (length(overlap_years) == 0) {
  stop("No overlapping years found between FAA flat files and AviationDB.", call. = FALSE)
}

faa_sample <- build_pilot_ingest_dataset(paths, source = "faa_flat", years = overlap_years[1])
aviationdb_sample <- build_pilot_ingest_dataset(paths, source = "aviationdb", years = overlap_years[1])

faa_columns <- names(faa_sample)
aviationdb_columns <- names(aviationdb_sample)

# Schema Check ----

if (!identical(faa_columns, aviationdb_columns)) {
  stop(
    paste(
      "Normalized ingest schemas differ.",
      "\nFAA columns:", paste(faa_columns, collapse = ", "),
      "\nAviationDB columns:", paste(aviationdb_columns, collapse = ", ")
    ),
    call. = FALSE
  )
}

# Overlap-Year Count Comparison ----

overlap_counts <- bind_rows(
  faa_inputs |>
    mutate(source = "faa_flat") |>
    rowwise() |>
    mutate(
      n_rows = nrow(build_pilot_ingest_dataset(paths, source = "faa_flat", years = year))
    ) |>
    ungroup() |>
    select("year", "source", "n_rows"),
  aviationdb_inputs |>
    mutate(source = "aviationdb") |>
    rowwise() |>
    mutate(
      n_rows = nrow(build_pilot_ingest_dataset(paths, source = "aviationdb", years = year))
    ) |>
    ungroup() |>
    select("year", "source", "n_rows")
) |>
  filter(year %in% overlap_years) |>
  tidyr::pivot_wider(names_from = "source", values_from = "n_rows") |>
  mutate(
    row_diff = aviationdb - faa_flat,
    pct_diff = row_diff / faa_flat * 100
  ) |>
  arrange(year)

# Reporting ----

message("Ingest schema validation passed for overlapping year: ", overlap_years[1])
message("Columns: ", paste(faa_columns, collapse = ", "))
print(overlap_counts)
