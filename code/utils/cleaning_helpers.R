# Purpose: helper functions for FAA-based pilot cleaning.

# Crosswalk and Label Helpers ----

load_state_fips_crosswalk <- function(paths) {
  readxl::read_excel(file.path(paths$xwalks, "StateFIPSicsprAB.xls")) |>
    janitor::clean_names() |>
    dplyr::select(.data$fips, .data$name, .data$ab) |>
    dplyr::rename(
      statefull = .data$name,
      state = .data$ab
    )
}

collapse_certificate_level <- function(level) {
  dplyr::case_when(
    level == "A" ~ "ATR",
    level == "C" ~ "C",
    TRUE ~ "other"
  )
}

# Sample Restriction Helpers ----

# These flags mirror the legacy logic for separating never-US and migrate-in/out pilots.
add_us_migration_flags <- function(data) {
  data |>
    dplyr::filter(!is.na(.data$country)) |>
    dplyr::group_by(.data$unique_id) |>
    dplyr::mutate(
      num_years = dplyr::n_distinct(.data$year),
      num_USA = sum(.data$country == "USA"),
      never_in_US = .data$num_USA == 0,
      migrate_in_out_US = (.data$num_USA > 0) & (.data$num_USA < .data$num_years)
    ) |>
    dplyr::ungroup()
}

# This restriction keeps pilots observed only in the 50 states plus DC across years.
restrict_always_in_main_us <- function(data, excluded_territories) {
  ever_in_us <- data |>
    dplyr::filter(!.data$never_in_US)

  always_in_us <- ever_in_us |>
    dplyr::filter(!.data$migrate_in_out_US)

  always_in_us |>
    dplyr::group_by(.data$unique_id) |>
    dplyr::mutate(
      num_not_in_main_US = sum(.data$state %in% excluded_territories),
      always_in_main_US = .data$num_not_in_main_US == 0,
      both_main_other_US = (.data$num_not_in_main_US > 0) &
        (.data$num_years > .data$num_not_in_main_US)
    ) |>
    dplyr::filter(.data$always_in_main_US) |>
    dplyr::ungroup() |>
    dplyr::select(-.data$num_not_in_main_US, -.data$always_in_main_US, -.data$both_main_other_US)
}

# Derived Dataset Builders ----

build_main_us_pilots_any <- function(data, state_fips_crosswalk) {
  state_fips_crosswalk |>
    dplyr::left_join(data, by = "state") |>
    dplyr::select(-dplyr::all_of(c("num_USA", "never_in_US", "migrate_in_out_US"))) |>
    dplyr::relocate(.data$year) |>
    dplyr::mutate(level_collapsed = collapse_certificate_level(.data$level))
}

build_main_us_pilots_atr <- function(main_us_pilots_any) {
  main_us_pilots_any |>
    dplyr::filter(.data$level_collapsed == "ATR") |>
    dplyr::select(-.data$level_collapsed, -.data$level, -.data$expire_date)
}

# Working Population Inputs ----

# The working-population files are already organized as one CSV per year.
load_tot_working_population <- function(paths) {
  csv_files <- list.files(
    paths$raw_tot_working_pop,
    pattern = "tot_working_pop\\.csv$",
    full.names = TRUE
  )

  readr::read_csv(
    csv_files,
    id = "file",
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_guess())
  ) |>
    dplyr::mutate(year = as.numeric(stringr::str_extract(.data$file, "\\d{4}"))) |>
    dplyr::select(-.data$file) |>
    dplyr::rename(state = .data$statefips)
}

# This matches the legacy ATR share summary statistic by state-year.
build_sum_stat_prop_pilots <- function(main_us_pilots_atr, tot_working_pop) {
  main_us_pilots_atr |>
    dplyr::count(.data$year, .data$state, name = "n_atr_pilots") |>
    dplyr::left_join(tot_working_pop, by = c("year", "state")) |>
    dplyr::mutate(prop_atr_pilots = .data$n_atr_pilots / .data$tot_work_pop * 100)
}

# Analysis Builders ----

build_state_prop_bins <- function(data) {
  data |>
    dplyr::mutate(
      prop_bins = cut(.data$prop_atr_pilots, breaks = c(0.01, 0.05, 0.1, 0.2, 0.55))
    )
}

build_state_prop_change <- function(data) {
  data |>
    dplyr::filter(.data$year %in% c(2009, 2024)) |>
    dplyr::arrange(.data$state, .data$year) |>
    dplyr::group_by(.data$state) |>
    dplyr::mutate(
      lag_prop_atr_pilots = dplyr::lag(.data$prop_atr_pilots),
      d_prop_atr_pilots = log(.data$prop_atr_pilots) - log(.data$lag_prop_atr_pilots),
      lag_n_atr_pilots = dplyr::lag(.data$n_atr_pilots),
      d_n_atr_pilots = log(.data$n_atr_pilots) - log(.data$lag_n_atr_pilots)
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(.data$d_prop_atr_pilots)) |>
    dplyr::mutate(
      d_prop_bins = cut(.data$d_prop_atr_pilots, breaks = c(-0.35, -0.15, -0.01, 0.01, 0.15, 0.4))
    )
}

build_state_total_share <- function(data) {
  data |>
    dplyr::group_by(.data$year) |>
    dplyr::mutate(N_atr_pilots = sum(.data$n_atr_pilots)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      prop_atr_pilots_ofall = .data$n_atr_pilots / .data$N_atr_pilots * 100,
      prop_bins = cut(.data$prop_atr_pilots_ofall, breaks = c(0, 1, 2, 3, 10, 13))
    )
}

build_residual_state_map <- function(data, zero_states) {
  data_2024 <- data |>
    dplyr::filter(.data$year == 2024)

  model <- stats::lm(prop_atr_pilots_ofall ~ tot_work_pop, data = data_2024)

  residual_data <- data_2024 |>
    dplyr::mutate(
      pred = stats::predict(model, newdata = data_2024),
      resid = .data$prop_atr_pilots_ofall - .data$pred,
      resid_binned = cut(.data$resid, c(-4, -1, 0, 1, 4, 7))
    )

  states_map <- usmap::us_map(regions = "states") |>
    sf::st_as_sf()

  states_map |>
    dplyr::left_join(residual_data, by = c("abbr" = "state")) |>
    dplyr::mutate(zero_tax = .data$abbr %in% zero_states)
}

# TAXSIM Input Builders ----

load_irs_soi_crosswalk <- function(paths) {
  readr::read_csv(
    file.path(paths$xwalks, "irs_soi_fips_crosswalk.csv"),
    show_col_types = FALSE
  ) |>
    janitor::clean_names()
}

load_soi_thresholds <- function(paths, years = taxsim_years) {
  soi <- readxl::read_excel(file.path(paths$raw_soi, "soi_income_p.xlsx"))
  cpi <- readr::read_csv(
    file.path(paths$raw_soi, "CPI_U_2yr_Moving_Avg.csv"),
    show_col_types = FALSE
  ) |>
    janitor::clean_names() |>
    dplyr::select("year", "cpi_u")

  cpi_2022 <- cpi |>
    dplyr::filter(.data$year == 2022) |>
    dplyr::pull(.data$cpi_u)

  soi |>
    dplyr::filter(.data$year %in% years) |>
    dplyr::left_join(cpi, by = "year") |>
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("real_"),
        ~ .x * (cpi_2022 / .data$cpi_u),
        .names = "nom_{stringr::str_remove(.col, '^real_')}"
      )
    ) |>
    dplyr::select("year", dplyr::starts_with("nom_"))
}

build_soi_taxsim_inputs <- function(soi_thresholds, irs_soi_crosswalk) {
  soi_thresholds |>
    tidyr::crossing(state = irs_soi_crosswalk$irs_soi_code) |>
    dplyr::mutate(mstat = 2L) |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("nom_"),
      names_to = "percentile",
      values_to = "pwages"
    ) |>
    dplyr::mutate(
      percentile = stringr::str_remove(.data$percentile, "^nom_"),
      method = "soi"
    ) |>
    dplyr::select("year", "state", "mstat", "percentile", "pwages", "method")
}

discover_bls_wage_files <- function(paths, years = taxsim_years) {
  files <- list.files(
    paths$raw_bls,
    pattern = "^national_M[0-9]{4}_dl\\.xlsx?$",
    recursive = TRUE,
    full.names = TRUE
  )

  discovered <- tibble::tibble(path = files) |>
    dplyr::mutate(
      year = as.integer(stringr::str_extract(basename(.data$path), "[0-9]{4}"))
    ) |>
    dplyr::filter(.data$year %in% years) |>
    dplyr::arrange(.data$year)

  if (nrow(discovered) == 0) {
    stop("No raw BLS wage files were found for the requested TAXSIM years.", call. = FALSE)
  }

  discovered
}

load_bls_wages <- function(paths, years = taxsim_years) {
  bls_files <- discover_bls_wage_files(paths, years = years)

  purrr::map_dfr(bls_files$path, function(path) {
    readxl::read_excel(path) |>
      janitor::clean_names() |>
      dplyr::select(
        dplyr::any_of(c(
          "occ_code", "occ_title", "a_mean", "a_pct10", "a_pct25",
          "a_median", "a_pct75", "a_pct90"
        ))
      ) |>
      dplyr::filter(.data$occ_code %in% c("53-2010", "53-2011", "53-2012")) |>
      dplyr::mutate(year = as.integer(stringr::str_extract(basename(path), "[0-9]{4}"))) |>
      dplyr::relocate("year")
  }) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("a_"),
        ~ as.numeric(dplyr::na_if(as.character(.x), "#"))
      )
    )
}

build_bls_taxsim_inputs <- function(bls_wages, irs_soi_crosswalk) {
  bls_wages |>
    dplyr::filter(.data$occ_code %in% c("53-2011", "53-2012")) |>
    tidyr::crossing(state = irs_soi_crosswalk$irs_soi_code) |>
    dplyr::mutate(mstat = 1L) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(c("a_mean", "a_median")),
      names_to = "percentile",
      values_to = "pwages"
    ) |>
    dplyr::mutate(
      percentile = stringr::str_remove(.data$percentile, "^a_"),
      method = "bls"
    ) |>
    dplyr::select(
      "year",
      "state",
      "mstat",
      "occ_code",
      "percentile",
      "pwages",
      "method"
    )
}

taxsim_input_filename <- function(method, percentile = NULL, occ_code = NULL) {
  if (method == "soi") {
    return(sprintf("taxsim_input_soi_%s.csv", percentile))
  }

  if (method == "bls") {
    return(sprintf("taxsim_input_bls_%s_%s.csv", occ_code, percentile))
  }

  stop("Unsupported TAXSIM input method.", call. = FALSE)
}

taxsim_output_filename <- function(method, percentile = NULL, occ_code = NULL) {
  if (method == "soi") {
    return(sprintf("taxsim_output_soi_%s.csv", percentile))
  }

  if (method == "bls") {
    return(sprintf("taxsim_output_bls_%s_%s.csv", occ_code, percentile))
  }

  stop("Unsupported TAXSIM output method.", call. = FALSE)
}

write_taxsim_case_outputs <- function(data, output_dir) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  if ("occ_code" %in% names(data)) {
    split_keys <- dplyr::distinct(data, .data$method, .data$occ_code, .data$percentile)

    purrr::pwalk(
      split_keys,
      function(method, occ_code, percentile) {
        output_path <- file.path(output_dir, taxsim_input_filename(
          method = method,
          percentile = percentile,
          occ_code = occ_code
        ))

        data |>
          dplyr::filter(
            .data$method == method,
            .data$occ_code == occ_code,
            .data$percentile == percentile
          ) |>
          dplyr::select("year", "state", "mstat", "pwages") |>
          readr::write_csv(output_path)
      }
    )

    return(invisible(NULL))
  }

  split_keys <- dplyr::distinct(data, .data$method, .data$percentile)

  purrr::pwalk(
    split_keys,
    function(method, percentile) {
      output_path <- file.path(output_dir, taxsim_input_filename(
        method = method,
        percentile = percentile
      ))

      data |>
        dplyr::filter(.data$method == method, .data$percentile == percentile) |>
        dplyr::select("year", "state", "mstat", "pwages") |>
        readr::write_csv(output_path)
    }
  )
}

discover_taxsim_case_files <- function(paths) {
  files <- list.files(
    paths$taxsim,
    pattern = "^taxsim_input_.*\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  )

  tibble::tibble(path = files) |>
    dplyr::mutate(
      file_stem = tools::file_path_sans_ext(basename(.data$path)),
      method = dplyr::if_else(
        stringr::str_detect(.data$file_stem, "^taxsim_input_soi_"),
        "soi",
        "bls"
      ),
      percentile = dplyr::if_else(
        .data$method == "soi",
        stringr::str_remove(.data$file_stem, "^taxsim_input_soi_"),
        stringr::str_match(.data$file_stem, "^taxsim_input_bls_[0-9-]+_(.+)$")[, 2]
      ),
      occ_code = dplyr::if_else(
        .data$method == "bls",
        stringr::str_match(.data$file_stem, "^taxsim_input_bls_([0-9-]+)_.+$")[, 2],
        NA_character_
      )
    ) |>
    dplyr::select(-"file_stem") |>
    dplyr::arrange(.data$method, .data$occ_code, .data$percentile)
}

run_taxsim_case_file <- function(path, output_dir, return_all_information = TRUE) {
  input_data <- readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::mutate(taxsimid = dplyr::row_number()) |>
    dplyr::relocate("taxsimid")

  tax_results <- usincometaxes::taxsim_calculate_taxes(
    .data = input_data,
    marginal_tax_rates = "Wages",
    return_all_information = return_all_information
  )

  output_data <- input_data |>
    dplyr::left_join(tax_results, by = "taxsimid")

  base_name <- basename(path)
  output_name <- sub("^taxsim_input_", "taxsim_output_", base_name)
  output_path <- file.path(output_dir, output_name)

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(output_data, output_path)

  output_path
}
