# Purpose: build an FAA-rich pooled pilot dataset for legacy-compatible cleaning.
# Inputs: paired FAA basic and cert files under `paths$raw_airmen_data`
# Outputs: `data/intermediate/faa_pilot_rich_panel.csv` unless overridden by `FAA_RICH_INGEST_OUTPUT`

# Setup ----

source("code/00_setup/00_packages_paths.R")
source("code/utils/ingest_helpers.R")

# This builder preserves the richer FAA columns needed by legacy-compatible cleaning.
year_subset <- Sys.getenv("FAA_RICH_INGEST_YEARS", unset = "")
output_path <- Sys.getenv(
  "FAA_RICH_INGEST_OUTPUT",
  unset = file.path(paths$intermediate, "faa_pilot_rich_panel.csv")
)

# Build and Write ----

faa_pilot_rich_panel <- build_faa_rich_ingest_dataset(
  paths = paths,
  years = year_subset
)

readr::write_csv(faa_pilot_rich_panel, output_path)

# Reporting ----

message("Wrote FAA-rich ingest dataset to: ", output_path)
message("Rows: ", nrow(faa_pilot_rich_panel))
message(
  "Years: ",
  paste(range(faa_pilot_rich_panel$year, na.rm = TRUE), collapse = " - ")
)
