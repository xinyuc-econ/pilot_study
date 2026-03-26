# Purpose: construct analysis-ready PIT measure datasets from clean TAXSIM outputs.
# Inputs: split TAXSIM output CSVs under `data/derived/taxsim_output/`
# Outputs: SOI and BLS PIT datasets in `data/derived/`

# Setup ----

source("code/00_setup/00_packages_paths.R")
source("code/utils/cleaning_helpers.R")

# Input Loading ----

soi_pit_measures <- load_soi_pit_measures(paths)
bls_pit_measures <- load_bls_pit_measures(paths)
soi_pit_wide <- build_soi_pit_wide(soi_pit_measures)

# Outputs ----

write_csv(soi_pit_measures, file.path(paths$derived, "all_years_pit_soi.csv"))
write_csv(soi_pit_wide, file.path(paths$derived, "all_years_pit_soi_wide.csv"))
write_csv(bls_pit_measures, file.path(paths$derived, "all_years_pit_bls.csv"))

# Reporting ----

message("Wrote SOI PIT measures to ", file.path(paths$derived, "all_years_pit_soi.csv"))
message("Wrote SOI PIT wide measures to ", file.path(paths$derived, "all_years_pit_soi_wide.csv"))
message("Wrote BLS PIT measures to ", file.path(paths$derived, "all_years_pit_bls.csv"))
