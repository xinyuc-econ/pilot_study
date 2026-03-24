# Purpose: build a canonical pooled pilot ingest dataset from the selected raw-data source.
# Inputs: raw source files under `paths$raw_airmen_data` or `paths$raw_aviationdb_data`
# Outputs: `data/intermediate/pilot_ingest_panel.csv` unless overridden by `PILOT_INGEST_OUTPUT`

source("code/00_setup/00_packages_paths.R")
source("code/utils/ingest_helpers.R")

ingest_source <- Sys.getenv("PILOT_INGEST_SOURCE", unset = "faa_flat")
year_subset <- Sys.getenv("PILOT_INGEST_YEARS", unset = "")
output_path <- Sys.getenv(
  "PILOT_INGEST_OUTPUT",
  unset = file.path(paths$intermediate, "pilot_ingest_panel.csv")
)

pilot_ingest <- build_pilot_ingest_dataset(
  paths = paths,
  source = ingest_source,
  years = year_subset
)

readr::write_csv(pilot_ingest, output_path)

message("Wrote canonical pilot ingest dataset to: ", output_path)
message("Rows: ", nrow(pilot_ingest))
message(
  "Years: ",
  paste(range(pilot_ingest$year, na.rm = TRUE), collapse = " - ")
)
