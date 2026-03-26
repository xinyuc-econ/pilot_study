# Test Setup ----

project_root <- normalizePath(file.path("..", ".."), winslash = "/", mustWork = TRUE)
setwd(project_root)

source(file.path(project_root, "code", "00_setup", "00_packages_paths.R"))
source(file.path(project_root, "code", "utils", "cleaning_helpers.R"))

# Pilot-Tax Analysis Helper Tests ----

testthat::test_that("assert_unique_case_rows rejects duplicated case keys", {
  duplicated_data <- tibble::tibble(year = c(2009, 2009), fips = c(1, 1), percentile = c("p95", "p95"))

  testthat::expect_error(
    assert_unique_case_rows(duplicated_data, c("year", "fips", "percentile"), "duplicate test"),
    "contains duplicated rows"
  )
})

testthat::test_that("SOI pilot-tax analysis data is unique by state-year-percentile", {
  prop_pilots <- readr::read_csv(file.path(paths$derived, "sum_stat_prop_atr_pilots.csv"), show_col_types = FALSE) |>
    dplyr::filter(.data$year >= 2009, .data$year <= 2022)
  pit_soi_wide <- readr::read_csv(file.path(paths$derived, "all_years_pit_soi_wide.csv"), show_col_types = FALSE)
  state_crosswalk <- load_state_fips_crosswalk(paths)

  output <- build_soi_pilot_tax_analysis(prop_pilots, pit_soi_wide, state_crosswalk)

  testthat::expect_true(all(c("method", "percentile", "year", "fips", "state", "prop_atr_pilots", "astr", "atr") %in% names(output)))
  testthat::expect_equal(nrow(output), 51 * length(unique(output$year)) * length(unique(output$percentile)))
})

testthat::test_that("BLS pilot-tax analysis data is unique by state-year-case", {
  prop_pilots <- readr::read_csv(file.path(paths$derived, "sum_stat_prop_atr_pilots.csv"), show_col_types = FALSE) |>
    dplyr::filter(.data$year >= 2009, .data$year <= 2022)
  pit_bls <- readr::read_csv(file.path(paths$derived, "all_years_pit_bls.csv"), show_col_types = FALSE)
  state_crosswalk <- load_state_fips_crosswalk(paths)

  output <- build_bls_pilot_tax_analysis(prop_pilots, pit_bls, state_crosswalk)

  testthat::expect_true(all(c("method", "pilot_type", "percentile", "year", "fips", "state", "prop_atr_pilots", "astr", "atr") %in% names(output)))
  testthat::expect_equal(
    nrow(output),
    51 * length(unique(output$year)) * nrow(dplyr::distinct(output, .data$pilot_type, .data$percentile))
  )
})

testthat::test_that("binscatter filenames encode method and case metadata", {
  testthat::expect_identical(
    build_binscatter_output_filename("soi", "astr", "p95", "no_AK_HI"),
    "binscatter_astr_pilots_soi_p95_no_AK_HI.png"
  )

  testthat::expect_identical(
    build_binscatter_output_filename("bls", "atr", "median", "all_states", "airline"),
    "binscatter_atr_pilots_bls_airline_median_all_states.png"
  )
})
