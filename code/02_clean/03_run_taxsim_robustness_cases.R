# Purpose: run all generated TAXSIM robustness-case inputs through the R TAXSIM interface.
# Inputs: split case files under `data/derived/taxsim/`
# Outputs: one TAXSIM output CSV per case under `data/derived/taxsim_output/`

# Setup ----

source("code/00_setup/00_packages_paths.R")
source("code/utils/cleaning_helpers.R")

dir.create(paths$taxsim_output, recursive = TRUE, showWarnings = FALSE)

return_all_information <- tolower(Sys.getenv("TAXSIM_RETURN_ALL_INFORMATION", unset = "true")) == "true"

# Input Discovery ----

taxsim_case_files <- discover_taxsim_case_files(paths)

if (nrow(taxsim_case_files) == 0) {
  stop("No TAXSIM input case files were found under data/derived/taxsim.", call. = FALSE)
}

# 1. TAXSIM Outputs ----

output_paths <- purrr::map_chr(
  taxsim_case_files$path,
  run_taxsim_case_file,
  output_dir = paths$taxsim_output,
  return_all_information = return_all_information
)

# Reporting ----

message("Ran ", length(output_paths), " TAXSIM robustness cases.")
message("Wrote TAXSIM outputs to ", paths$taxsim_output)
