# Test Setup ----

project_root <- normalizePath(file.path("..", ".."), winslash = "/", mustWork = TRUE)
setwd(project_root)

source(file.path(project_root, "code", "00_setup", "00_packages_paths.R"))
source(file.path(project_root, "code", "utils", "cleaning_helpers.R"))

# PIT Helper Tests ----

testthat::test_that("TAXSIM output discovery parses SOI and BLS metadata", {
  discovered <- discover_taxsim_output_files(paths)

  testthat::expect_equal(nrow(discovered), 10)
  testthat::expect_true(all(c("path", "method", "percentile", "occ_code") %in% names(discovered)))
  testthat::expect_true(any(discovered$method == "soi"))
  testthat::expect_true(any(discovered$method == "bls"))
})

testthat::test_that("PIT measures are derived from federal AGI and tax liabilities", {
  input <- tibble::tibble(
    fiitax = c(10, 20),
    siitax = c(5, 10),
    v10_federal_agi = c(100, 200)
  )

  output <- build_pit_measures(input)

  testthat::expect_equal(output$astr, c(0.05, 0.05))
  testthat::expect_equal(output$atr, c(0.15, 0.15))
})

testthat::test_that("SOI PIT measures load with percentile metadata and FIPS codes", {
  soi_pit <- load_soi_pit_measures(paths)

  testthat::expect_true(all(c("year", "fips", "srate", "astr", "atr", "percentile") %in% names(soi_pit)))
  testthat::expect_true(all(sort(unique(soi_pit$percentile)) == c("p90", "p95", "p96", "p97", "p98", "p99")))
  testthat::expect_equal(length(unique(soi_pit$fips)), 51)
})

testthat::test_that("SOI PIT wide reshaping creates percentile-specific tax columns", {
  soi_pit <- load_soi_pit_measures(paths)
  soi_wide <- build_soi_pit_wide(soi_pit)

  testthat::expect_true(all(c("srate_p95", "astr_p95", "atr_p95") %in% names(soi_wide)))
})

testthat::test_that("BLS PIT measures preserve pilot type and percentile metadata", {
  bls_pit <- load_bls_pit_measures(paths)

  testthat::expect_true(all(c("year", "fips", "pilot_type", "percentile", "srate", "astr", "atr") %in% names(bls_pit)))
  testthat::expect_identical(sort(unique(bls_pit$pilot_type)), c("airline", "commercial"))
  testthat::expect_identical(sort(unique(bls_pit$percentile)), c("mean", "median"))
})
