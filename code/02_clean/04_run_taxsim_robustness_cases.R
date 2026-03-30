# Purpose: run all generated TAXSIM robustness-case inputs through the R TAXSIM interface.
# Inputs: split case files under `data/derived/aviationdb/taxsim/`
# Outputs: one TAXSIM output CSV per case under `data/derived/aviationdb/taxsim_output/`

# Setup ----

source("code/00_setup/00_packages_paths.R")

if (!requireNamespace("usincometaxes", quietly = TRUE)) {
  stop("The `usincometaxes` package is required to run TAXSIM robustness cases.", call. = FALSE)
}

suppressPackageStartupMessages(library(usincometaxes))

dir.create(paths$taxsim_output_aviationdb, recursive = TRUE, showWarnings = FALSE)

return_all_information <- tolower(Sys.getenv("TAXSIM_RETURN_ALL_INFORMATION", unset = "true")) == "true"

# Input Discovery ----

taxsim_case_files <- tibble(
  path = list.files(
    paths$taxsim_aviationdb,
    pattern = "^taxsim_input_.*\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  )
) |>
  mutate(
    file_stem = tools::file_path_sans_ext(basename(path)),
    method = if_else(
      str_detect(file_stem, "^taxsim_input_soi_"),
      "soi",
      "bls"
    ),
    percentile = if_else(
      method == "soi",
      str_remove(file_stem, "^taxsim_input_soi_"),
      str_match(file_stem, "^taxsim_input_bls_[0-9-]+_(.+)$")[, 2]
    ),
    occ_code = if_else(
      method == "bls",
      str_match(file_stem, "^taxsim_input_bls_([0-9-]+)_.+$")[, 2],
      NA_character_
    )
  ) |>
  select(-"file_stem") |>
  arrange(method, occ_code, percentile)

if (nrow(taxsim_case_files) == 0) {
  stop("No TAXSIM input case files were found under data/derived/aviationdb/taxsim.", call. = FALSE)
}

# 1. TAXSIM Outputs ----

output_paths <- map_chr(
  taxsim_case_files$path,
  function(path) {
    input_data <- read_csv(path, show_col_types = FALSE) |>
      mutate(taxsimid = row_number()) |>
      relocate("taxsimid")

    tax_results <- taxsim_calculate_taxes(
      .data = input_data,
      marginal_tax_rates = "Wages",
      return_all_information = return_all_information
    )

    if (!"taxsimid" %in% names(tax_results)) {
      stop("TAXSIM results are missing `taxsimid`; cannot safely join results back to inputs.", call. = FALSE)
    }

    if (nrow(tax_results) != nrow(input_data)) {
      stop("TAXSIM output row count does not match the input case file row count.", call. = FALSE)
    }

    if (anyDuplicated(tax_results$taxsimid) > 0) {
      stop("TAXSIM results contain duplicated `taxsimid` values.", call. = FALSE)
    }

    output_data <- input_data |>
      left_join(tax_results, by = "taxsimid")

    output_path <- file.path(
      paths$taxsim_output_aviationdb,
      sub("^taxsim_input_", "taxsim_output_", basename(path))
    )

    write_csv(output_data, output_path)
    output_path
  }
)

# Reporting ----

message("Ran ", length(output_paths), " TAXSIM robustness cases.")
message("Wrote TAXSIM outputs to ", paths$taxsim_output_aviationdb)
