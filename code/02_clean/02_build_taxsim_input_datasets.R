# Purpose: build TAXSIM-ready input datasets for SOI and BLS robustness cases.
# Inputs: SOI percentile thresholds, CPI series, BLS wage files, and IRS SOI state crosswalk
# Outputs: split TAXSIM-ready CSVs under `data/derived/taxsim/soi/` and `data/derived/taxsim/bls/`

# Setup ----

source("code/00_setup/00_packages_paths.R")
source("code/utils/cleaning_helpers.R")

dir.create(paths$taxsim, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(paths$taxsim, "soi"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(paths$taxsim, "bls"), recursive = TRUE, showWarnings = FALSE)

# Input Loading ----

irs_soi_crosswalk <- load_irs_soi_crosswalk(paths)
soi_thresholds <- load_soi_thresholds(paths, years = taxsim_years)
bls_wages <- load_bls_wages(paths, years = taxsim_years)

# 1. TAXSIM Inputs ----

## 1.1 SOI AGI Threshold Inputs ----

soi_taxsim_inputs <- build_soi_taxsim_inputs(soi_thresholds, irs_soi_crosswalk)

write_taxsim_case_outputs(
  data = soi_taxsim_inputs,
  output_dir = file.path(paths$taxsim, "soi")
)

## 1.2 BLS Pilot Wage Inputs ----

bls_taxsim_inputs <- build_bls_taxsim_inputs(bls_wages, irs_soi_crosswalk)

write_taxsim_case_outputs(
  data = bls_taxsim_inputs,
  output_dir = file.path(paths$taxsim, "bls")
)

# Reporting ----

message("Wrote SOI TAXSIM inputs to ", file.path(paths$taxsim, "soi"))
message("Wrote BLS TAXSIM inputs to ", file.path(paths$taxsim, "bls"))
message("SOI rows: ", nrow(soi_taxsim_inputs))
message("BLS rows: ", nrow(bls_taxsim_inputs))
