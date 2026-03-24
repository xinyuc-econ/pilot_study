# Purpose: validate that both ingest sources normalize to the same schema.
# Inputs: FAA flat files and AviationDB files for overlapping years
# Outputs: console validation only

source("code/00_setup/00_packages_paths.R")
source("code/utils/ingest_helpers.R")

faa_inputs <- discover_ingest_inputs(paths, source = "faa_flat")
aviationdb_inputs <- discover_ingest_inputs(paths, source = "aviationdb")

overlap_years <- intersect(faa_inputs$year, aviationdb_inputs$year)

if (length(overlap_years) == 0) {
  stop("No overlapping years found between FAA flat files and AviationDB.", call. = FALSE)
}

faa_sample <- build_pilot_ingest_dataset(paths, source = "faa_flat", years = overlap_years[1])
aviationdb_sample <- build_pilot_ingest_dataset(paths, source = "aviationdb", years = overlap_years[1])

faa_columns <- names(faa_sample)
aviationdb_columns <- names(aviationdb_sample)

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

message("Ingest schema validation passed for overlapping year: ", overlap_years[1])
message("Columns: ", paste(faa_columns, collapse = ", "))
