# Purpose: build TAXSIM-ready input datasets for SOI and BLS robustness cases.
# Inputs: SOI percentile thresholds, CPI series, raw BLS wage workbooks, and IRS SOI state crosswalk
# Outputs: split TAXSIM-ready CSVs under `data/derived/aviationdb/taxsim/soi/` and `data/derived/aviationdb/taxsim/bls/`

# Setup ----

source("code/00_setup/00_packages_paths.R")

write_taxsim_case_outputs <- function(data, output_dir) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  if ("occ_code" %in% names(data)) {
    split_keys <- distinct(data, method, occ_code, percentile)

    pwalk(
      split_keys,
      function(method, occ_code, percentile) {
        case_method <- method
        case_occ_code <- occ_code
        case_percentile <- percentile

        output_path <- file.path(
          output_dir,
          sprintf("taxsim_input_%s_%s_%s.csv", case_method, case_occ_code, case_percentile)
        )

        data |>
          filter(
            method == case_method,
            occ_code == case_occ_code,
            percentile == case_percentile
          ) |>
          select("year", "state", "mstat", "pwages") |>
          write_csv(output_path)
      }
    )

    return(invisible(NULL))
  }

  split_keys <- distinct(data, method, percentile)

  pwalk(
    split_keys,
    function(method, percentile) {
      case_method <- method
      case_percentile <- percentile

      output_path <- file.path(
        output_dir,
        sprintf("taxsim_input_%s_%s.csv", case_method, case_percentile)
      )

      data |>
        filter(method == case_method, percentile == case_percentile) |>
        select("year", "state", "mstat", "pwages") |>
        write_csv(output_path)
    }
  )
}

extract_bls_year <- function(path) {
  path |>
    basename() |>
    str_extract("[0-9]{4}") |>
    as.integer()
}

read_bls_wage_workbook <- function(path) {
  workbook_data <- read_excel(path, sheet = excel_sheets(path)[1]) |>
    clean_names()

  if ("occ_titl" %in% names(workbook_data) && !("occ_title" %in% names(workbook_data))) {
    workbook_data <- workbook_data |>
      rename(occ_title = occ_titl)
  }

  workbook_data |>
    select(
      any_of(c(
        "occ_code",
        "occ_title",
        "a_mean",
        "a_median"
      ))
    ) |>
    filter(occ_code %in% c("53-2011", "53-2012")) |>
    mutate(
      year = extract_bls_year(path),
      across(
        c("a_mean", "a_median"),
        ~ parse_number(as.character(.x), na = c("", "NA", "#", "*", "**", "***"))
      )
    ) |>
    relocate("year")
}

dir.create(paths$taxsim_aviationdb, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(paths$taxsim_aviationdb, "soi"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(paths$taxsim_aviationdb, "bls"), recursive = TRUE, showWarnings = FALSE)

# Input Loading ----

irs_soi_crosswalk <- read_csv(
  file.path(paths$xwalks, "irs_soi_fips_crosswalk.csv"),
  show_col_types = FALSE
) |>
  clean_names()

soi <- read_excel(file.path(paths$raw_soi, "soi_income_p.xlsx"))
cpi <- read_csv(
  file.path(paths$raw_soi, "CPI_U_2yr_Moving_Avg.csv"),
  show_col_types = FALSE
) |>
  clean_names() |>
  select("year", "cpi_u")

cpi_2022 <- cpi |>
  filter(year == 2022) |>
  pull(cpi_u)

soi_thresholds <- soi |>
  filter(year %in% taxsim_years_soi) |>
  left_join(cpi, by = "year") |>
  mutate(
    across(
      starts_with("real_"),
      ~ .x * (cpi_2022 / cpi_u),
      .names = "nom_{str_remove(.col, '^real_')}"
    )
  ) |>
  select("year", starts_with("nom_"))

bls_files <- list.files(
  paths$raw_bls,
  pattern = "national.*[0-9]{4}.*_dl\\.xlsx?$",
  recursive = TRUE,
  full.names = TRUE
)

bls_wage_files <- tibble(path = bls_files) |>
  mutate(year = extract_bls_year(path)) |>
  filter(year %in% taxsim_years_bls) |>
  arrange(year)

if (nrow(bls_wage_files) == 0) {
  stop("No raw BLS wage files were found for the requested TAXSIM years.", call. = FALSE)
}

missing_bls_years <- setdiff(taxsim_years_bls, bls_wage_files$year)

if (length(missing_bls_years) > 0) {
  stop(
    paste(
      "Missing raw BLS wage files for TAXSIM years:",
      paste(missing_bls_years, collapse = ", ")
    ),
    call. = FALSE
  )
}

bls_wages <- map_dfr(bls_wage_files$path, read_bls_wage_workbook)

# 1. TAXSIM Inputs ----

## 1.1 SOI AGI Threshold Inputs ----

soi_taxsim_inputs <- soi_thresholds |>
  tidyr::crossing(state = irs_soi_crosswalk$irs_soi_code) |>
  mutate(mstat = 2L) |>
  tidyr::pivot_longer(
    cols = starts_with("nom_"),
    names_to = "percentile",
    values_to = "pwages"
  ) |>
  mutate(
    percentile = str_remove(percentile, "^nom_"),
    method = "soi"
  ) |>
  select("year", "state", "mstat", "percentile", "pwages", "method")

write_taxsim_case_outputs(
  data = soi_taxsim_inputs,
  output_dir = file.path(paths$taxsim_aviationdb, "soi")
)

## 1.2 BLS Pilot Wage Inputs ----

bls_taxsim_inputs <- bls_wages |>
  filter(occ_code %in% c("53-2011", "53-2012")) |>
  tidyr::crossing(state = irs_soi_crosswalk$irs_soi_code) |>
  mutate(mstat = 1L) |>
  tidyr::pivot_longer(
    cols = all_of(c("a_mean", "a_median")),
    names_to = "percentile",
    values_to = "pwages"
  ) |>
  mutate(
    percentile = str_remove(percentile, "^a_"),
    method = "bls"
  ) |>
  select(
    "year",
    "state",
    "mstat",
    "occ_code",
    "percentile",
    "pwages",
    "method"
  )

write_taxsim_case_outputs(
  data = bls_taxsim_inputs,
  output_dir = file.path(paths$taxsim_aviationdb, "bls")
)

# Reporting ----

message("Wrote SOI TAXSIM inputs to ", file.path(paths$taxsim_aviationdb, "soi"))
message("Wrote BLS TAXSIM inputs to ", file.path(paths$taxsim_aviationdb, "bls"))
message("SOI years: ", min(taxsim_years_soi), "-", max(taxsim_years_soi))
message("BLS years: ", min(taxsim_years_bls), "-", max(taxsim_years_bls))
message("SOI rows: ", nrow(soi_taxsim_inputs))
message("BLS rows: ", nrow(bls_taxsim_inputs))
