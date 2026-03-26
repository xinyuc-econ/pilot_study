# Purpose: helper functions for FAA-based pilot cleaning.

# Crosswalk and Label Helpers ----

load_state_fips_crosswalk <- function(paths) {
  readxl::read_excel(file.path(paths$xwalks, "StateFIPSicsprAB.xls")) |>
    janitor::clean_names() |>
    dplyr::select("fips", "name", "ab") |>
    dplyr::rename(
      statefull = "name",
      state = "ab"
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

# Pilot Mover Analysis Helpers ----

select_mover_panel_columns <- function(main_us_pilots_atr) {
  main_us_pilots_atr |>
    dplyr::select(
      "year",
      "fips",
      "statefull",
      "state",
      "unique_id",
      "first_name",
      "last_name",
      "street_1",
      "city",
      "zip_code",
      "num_years"
    )
}

build_pilot_mover_panel <- function(data) {
  data |>
    dplyr::arrange(.data$unique_id, .data$year) |>
    dplyr::group_by(.data$unique_id) |>
    dplyr::mutate(
      dest_state = .data$state,
      origin_state = dplyr::lag(.data$dest_state),
      moved = dplyr::if_else(
        !is.na(.data$origin_state) & (.data$dest_state != .data$origin_state),
        1L,
        0L
      )
    ) |>
    dplyr::relocate("origin_state", "moved", .after = "dest_state") |>
    dplyr::ungroup()
}

add_pilot_move_counts <- function(data) {
  data |>
    dplyr::group_by(.data$unique_id) |>
    dplyr::mutate(num_moves = sum(.data$moved, na.rm = TRUE)) |>
    dplyr::relocate("num_moves", .after = "moved") |>
    dplyr::ungroup()
}

add_flow_time_fields <- function(data) {
  data |>
    dplyr::arrange(.data$unique_id, .data$year) |>
    dplyr::group_by(.data$unique_id) |>
    dplyr::mutate(
      lag_year = dplyr::lag(.data$year),
      time_period_yrs = as.integer(.data$year - .data$lag_year)
    ) |>
    dplyr::relocate("lag_year", "time_period_yrs", .after = "year") |>
    dplyr::ungroup()
}

build_mover_period_summary <- function(mover_panel) {
  period_month_map <- tibble::tribble(
    ~period_key,   ~time_period,            ~months_between,
    "2009-2010", "11/2009 - 05/2010",  6,
    "2010-2011", "05/2010 - 09/2011", 16,
    "2011-2014", "09/2011 - 09/2014", 36,
    "2014-2015", "09/2014 - 09/2015", 12,
    "2015-2016", "09/2015 - 11/2016", 14,
    "2016-2017", "11/2016 - 09/2017", 10,
    "2017-2019", "06/2017 - 06/2019", 24,
    "2019-2022", "06/2019 - 10/2022", 40,
    "2022-2024", "10/2022 - 09/2024", 23
  )

  mover_panel |>
    dplyr::group_by(.data$year) |>
    dplyr::summarise(
      n_pilots = dplyr::n(),
      n_moved = sum(.data$moved, na.rm = TRUE),
      prop_moved = .data$n_moved / .data$n_pilots * 100,
      .groups = "drop"
    ) |>
    dplyr::mutate(
      lag_year = dplyr::lag(.data$year),
      period_key = paste(.data$lag_year, .data$year, sep = "-")
    ) |>
    dplyr::filter(!is.na(.data$lag_year)) |>
    dplyr::left_join(period_month_map, by = "period_key") |>
    dplyr::mutate(monthly_prop_moved = .data$prop_moved / .data$months_between) |>
    dplyr::select(
      "time_period",
      "n_pilots",
      "n_moved",
      "monthly_prop_moved"
    ) |>
    dplyr::rename(
      `Time period` = "time_period",
      `# of pilots` = "n_pilots",
      `# of movers` = "n_moved",
      `Ave. monthly % moved` = "monthly_prop_moved"
    ) |>
    as.data.frame()
}

build_migration_flow_table <- function(data) {
  flow_data <- data |>
    dplyr::filter(!is.na(.data$origin_state), !is.na(.data$dest_state))

  as.matrix(table(flow_data$origin_state, flow_data$dest_state))
}

select_top_states_by_average_atr_count <- function(sum_stat_prop_atr_pilots, n_states = 10L) {
  sum_stat_prop_atr_pilots |>
    dplyr::group_by(.data$state) |>
    dplyr::summarise(avg_n_atr_pilots = mean(.data$n_atr_pilots), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(.data$avg_n_atr_pilots), .data$state) |>
    dplyr::slice_head(n = n_states) |>
    dplyr::pull(.data$state)
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
      case_method <- method
      case_occ_code <- occ_code
      case_percentile <- percentile

      output_path <- file.path(output_dir, taxsim_input_filename(
        method = case_method,
        percentile = case_percentile,
        occ_code = case_occ_code
      ))

      data |>
        dplyr::filter(
          .data$method == case_method,
          .data$occ_code == case_occ_code,
          .data$percentile == case_percentile
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
      case_method <- method
      case_percentile <- percentile

      output_path <- file.path(output_dir, taxsim_input_filename(
        method = case_method,
        percentile = case_percentile
      ))

      data |>
        dplyr::filter(.data$method == case_method, .data$percentile == case_percentile) |>
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
    dplyr::left_join(tax_results, by = "taxsimid")

  base_name <- basename(path)
  output_name <- sub("^taxsim_input_", "taxsim_output_", base_name)
  output_path <- file.path(output_dir, output_name)

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(output_data, output_path)

  output_path
}

# PIT Measure Builders ----

discover_taxsim_output_files <- function(paths, requested_method = NULL) {
  files <- list.files(
    paths$taxsim_output,
    pattern = "^taxsim_output_.*\\.csv$",
    recursive = FALSE,
    full.names = TRUE
  )

  discovered <- tibble::tibble(path = files) |>
    dplyr::mutate(
      file_stem = tools::file_path_sans_ext(basename(.data$path)),
      method = dplyr::if_else(
        stringr::str_detect(.data$file_stem, "^taxsim_output_soi_"),
        "soi",
        "bls"
      ),
      percentile = dplyr::if_else(
        .data$method == "soi",
        stringr::str_remove(.data$file_stem, "^taxsim_output_soi_"),
        stringr::str_match(.data$file_stem, "^taxsim_output_bls_[0-9-]+_(.+)$")[, 2]
      ),
      occ_code = dplyr::if_else(
        .data$method == "bls",
        stringr::str_match(.data$file_stem, "^taxsim_output_bls_([0-9-]+)_.+$")[, 2],
        NA_character_
      )
    ) |>
    dplyr::select(-"file_stem") |>
    dplyr::arrange(.data$method, .data$occ_code, .data$percentile)

  if (!is.null(requested_method)) {
    discovered <- discovered |>
      dplyr::filter(.data$method == requested_method)
  }

  if (nrow(discovered) == 0) {
    stop("No TAXSIM output files were found under data/derived/taxsim_output.", call. = FALSE)
  }

  discovered
}

assert_unique_case_rows <- function(data, keys, dataset_name) {
  duplicates <- data |>
    dplyr::count(dplyr::across(dplyr::all_of(keys)), name = "n") |>
    dplyr::filter(.data$n > 1)

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

build_pit_measures <- function(data) {
  data |>
    dplyr::mutate(
      astr = .data$siitax / .data$v10_federal_agi,
      atr = (.data$siitax + .data$fiitax) / .data$v10_federal_agi
    )
}

load_soi_pit_measures <- function(paths) {
  output_files <- discover_taxsim_output_files(paths, requested_method = "soi")
  xwalk <- load_irs_soi_crosswalk(paths)

  purrr::map2_dfr(
    output_files$path,
    output_files$percentile,
    function(path, percentile) {
      readr::read_csv(path, show_col_types = FALSE) |>
        dplyr::select("year", "state", "fiitax", "siitax", "v10_federal_agi", "srate") |>
        build_pit_measures() |>
        dplyr::mutate(percentile = percentile)
    }
  ) |>
    dplyr::left_join(xwalk, by = c("state" = "irs_soi_code")) |>
    dplyr::rename(fips = "fips_code") |>
    dplyr::select(-"state", -"state_name") |>
    assert_unique_case_rows(
      keys = c("year", "fips", "percentile"),
      dataset_name = "SOI PIT measures"
    )
}

build_soi_pit_wide <- function(data) {
  data |>
    dplyr::select("year", "fips", "percentile", "srate", "astr", "atr") |>
    tidyr::pivot_wider(
      names_from = "percentile",
      values_from = c("srate", "astr", "atr")
    )
}

pilot_type_from_occ_code <- function(occ_code) {
  dplyr::case_when(
    occ_code == "53-2011" ~ "airline",
    occ_code == "53-2012" ~ "commercial",
    TRUE ~ NA_character_
  )
}

load_bls_pit_measures <- function(paths) {
  output_files <- discover_taxsim_output_files(paths, requested_method = "bls")
  xwalk <- load_irs_soi_crosswalk(paths)

  purrr::pmap_dfr(
    output_files,
    function(path, method, percentile, occ_code) {
      readr::read_csv(path, show_col_types = FALSE) |>
        dplyr::select("year", "state", "fiitax", "siitax", "v10_federal_agi", "srate") |>
        build_pit_measures() |>
        dplyr::mutate(
          percentile = percentile,
          occ_code = occ_code,
          pilot_type = pilot_type_from_occ_code(occ_code)
        )
    }
  ) |>
    dplyr::left_join(xwalk, by = c("state" = "irs_soi_code")) |>
    dplyr::rename(fips = "fips_code") |>
    dplyr::select(-"state", -"state_name", -"occ_code") |>
    assert_unique_case_rows(
      keys = c("year", "fips", "pilot_type", "percentile"),
      dataset_name = "BLS PIT measures"
    )
}

# Pilot-Tax Analysis Builders ----

build_soi_pilot_tax_analysis <- function(prop_pilots, pit_soi_wide, state_crosswalk) {
  pit_soi_long <- pit_soi_wide |>
    dplyr::left_join(
      state_crosswalk |>
        dplyr::select("fips", "state"),
      by = "fips"
    ) |>
    tidyr::pivot_longer(
      cols = tidyselect::matches("^(astr|atr)_"),
      names_to = c(".value", "percentile"),
      names_sep = "_"
    ) |>
    dplyr::mutate(method = "soi")

  merged <- pit_soi_long |>
    dplyr::left_join(prop_pilots, by = c("year", "state")) |>
    dplyr::select(
      "method",
      "percentile",
      "year",
      "fips",
      "state",
      "n_atr_pilots",
      "tot_work_pop",
      "prop_atr_pilots",
      "astr",
      "atr"
    )

  if (anyNA(merged$prop_atr_pilots)) {
    stop("SOI pilot-tax analysis data has unmatched pilot-share rows.", call. = FALSE)
  }

  assert_unique_case_rows(
    merged,
    keys = c("year", "fips", "percentile"),
    dataset_name = "SOI pilot-tax analysis data"
  )
}

build_bls_pilot_tax_analysis <- function(prop_pilots, pit_bls, state_crosswalk) {
  merged <- pit_bls |>
    dplyr::left_join(
      state_crosswalk |>
        dplyr::select("fips", "state"),
      by = "fips"
    ) |>
    dplyr::mutate(method = "bls") |>
    dplyr::left_join(prop_pilots, by = c("year", "state")) |>
    dplyr::select(
      "method",
      "pilot_type",
      "percentile",
      "year",
      "fips",
      "state",
      "n_atr_pilots",
      "tot_work_pop",
      "prop_atr_pilots",
      "astr",
      "atr"
    )

  if (anyNA(merged$prop_atr_pilots)) {
    stop("BLS pilot-tax analysis data has unmatched pilot-share rows.", call. = FALSE)
  }

  assert_unique_case_rows(
    merged,
    keys = c("year", "fips", "pilot_type", "percentile"),
    dataset_name = "BLS pilot-tax analysis data"
  )
}

format_soi_percentile_label <- function(percentile) {
  percentile_num <- stringr::str_remove(percentile, "^p")
  sprintf("%sth income percentile", percentile_num)
}

build_binscatter_output_filename <- function(method, tax_measure, percentile, geography_variant, pilot_type = NULL) {
  if (method == "soi") {
    return(sprintf(
      "binscatter_%s_pilots_soi_%s_%s.png",
      tax_measure,
      percentile,
      geography_variant
    ))
  }

  if (method == "bls") {
    return(sprintf(
      "binscatter_%s_pilots_bls_%s_%s_%s.png",
      tax_measure,
      pilot_type,
      percentile,
      geography_variant
    ))
  }

  stop("Unsupported binscatter method.", call. = FALSE)
}
