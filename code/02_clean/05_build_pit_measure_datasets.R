# Purpose: construct analysis-ready PIT measure datasets from clean TAXSIM outputs.
# Inputs: split TAXSIM output CSVs under `data/derived/aviationdb/taxsim_output/`
# Outputs: SOI and BLS PIT datasets in `data/derived/aviationdb/`

# Setup ----

source("code/00_setup/00_packages_paths.R")

build_pit_measures <- function(data) {
  data |>
    mutate(
      astr = siitax / v10_federal_agi,
      atr = (siitax + fiitax) / v10_federal_agi
    )
}

assert_unique_case_rows <- function(data, keys, dataset_name) {
  duplicates <- data |>
    count(across(all_of(keys)), name = "n") |>
    filter(n > 1)

  if (nrow(duplicates) > 0) {
    stop(
      paste0(
        dataset_name,
        " contains duplicated rows for keys: ",
        paste(keys, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  data
}

# Input Loading ----

taxsim_output_files <- tibble(
  path = list.files(
    paths$taxsim_output_aviationdb,
    pattern = "^taxsim_output_.*\\.csv$",
    recursive = FALSE,
    full.names = TRUE
  )
) |>
  mutate(
    file_stem = tools::file_path_sans_ext(basename(path)),
    method = if_else(
      str_detect(file_stem, "^taxsim_output_soi_"),
      "soi",
      "bls"
    ),
    percentile = if_else(
      method == "soi",
      str_remove(file_stem, "^taxsim_output_soi_"),
      str_match(file_stem, "^taxsim_output_bls_[0-9-]+_(.+)$")[, 2]
    ),
    occ_code = if_else(
      method == "bls",
      str_match(file_stem, "^taxsim_output_bls_([0-9-]+)_.+$")[, 2],
      NA_character_
    )
  ) |>
  select(-"file_stem") |>
  arrange(method, occ_code, percentile)

if (nrow(taxsim_output_files) == 0) {
  stop("No TAXSIM output files were found under data/derived/aviationdb/taxsim_output.", call. = FALSE)
}

irs_soi_crosswalk <- read_csv(
  file.path(paths$xwalks, "irs_soi_fips_crosswalk.csv"),
  show_col_types = FALSE
) |>
  clean_names()

soi_output_files <- taxsim_output_files |>
  filter(method == "soi")

bls_output_files <- taxsim_output_files |>
  filter(method == "bls")

soi_pit_measures <- map2_dfr(
  soi_output_files$path,
  soi_output_files$percentile,
  function(path, percentile) {
    read_csv(path, show_col_types = FALSE) |>
      select("year", "state", "fiitax", "siitax", "v10_federal_agi", "srate") |>
      build_pit_measures() |>
      mutate(percentile = percentile)
  }
) |>
  left_join(irs_soi_crosswalk, by = c("state" = "irs_soi_code")) |>
  rename(fips = "fips_code") |>
  select(-"state", -"state_name") |>
  assert_unique_case_rows(
    keys = c("year", "fips", "percentile"),
    dataset_name = "SOI PIT measures"
  )

soi_pit_wide <- soi_pit_measures |>
  select("year", "fips", "percentile", "srate", "astr", "atr") |>
  tidyr::pivot_wider(
    names_from = "percentile",
    values_from = c("srate", "astr", "atr")
  )

bls_pit_measures <- pmap_dfr(
  bls_output_files,
  function(path, method, percentile, occ_code) {
    read_csv(path, show_col_types = FALSE) |>
      select("year", "state", "fiitax", "siitax", "v10_federal_agi", "srate") |>
      build_pit_measures() |>
      mutate(
        percentile = percentile,
        pilot_type = case_when(
          occ_code == "53-2011" ~ "airline",
          occ_code == "53-2012" ~ "commercial",
          TRUE ~ NA_character_
        )
      )
  }
) |>
  left_join(irs_soi_crosswalk, by = c("state" = "irs_soi_code")) |>
  rename(fips = "fips_code") |>
  select(-"state", -"state_name") |>
  assert_unique_case_rows(
    keys = c("year", "fips", "pilot_type", "percentile"),
    dataset_name = "BLS PIT measures"
  )

# Outputs ----

dir.create(paths$derived_aviationdb, recursive = TRUE, showWarnings = FALSE)

write_csv(soi_pit_measures, file.path(paths$derived_aviationdb, "all_years_pit_soi.csv"))
write_csv(soi_pit_wide, file.path(paths$derived_aviationdb, "all_years_pit_soi_wide.csv"))
write_csv(bls_pit_measures, file.path(paths$derived_aviationdb, "all_years_pit_bls.csv"))

# Reporting ----

message("Wrote SOI PIT measures to ", file.path(paths$derived_aviationdb, "all_years_pit_soi.csv"))
message("Wrote SOI PIT wide measures to ", file.path(paths$derived_aviationdb, "all_years_pit_soi_wide.csv"))
message("Wrote BLS PIT measures to ", file.path(paths$derived_aviationdb, "all_years_pit_bls.csv"))
