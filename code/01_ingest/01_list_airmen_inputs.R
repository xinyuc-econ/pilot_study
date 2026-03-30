# Purpose: list discoverable ingest inputs for the selected raw-data source.
# Inputs: raw source files under `paths$raw_airmen_data` or `paths$raw_aviationdb_data`
# Outputs: console summary only

# Setup ----

source("code/00_setup/00_packages_paths.R")
source("code/utils/ingest_helpers.R")

# These env vars make it easy to inspect a specific source or year subset.
ingest_source <- Sys.getenv("PILOT_INGEST_SOURCE", unset = "aviationdb")
year_subset <- Sys.getenv("PILOT_INGEST_YEARS", unset = "")

# Input Discovery ----

inputs <- discover_ingest_inputs(
  paths = paths,
  source = ingest_source,
  years = year_subset
)

# Reporting ----

message("Discovered ingest inputs for source: ", ingest_source)
print(inputs)
