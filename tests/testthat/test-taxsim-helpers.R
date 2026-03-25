# Test Setup ----

project_root <- normalizePath(file.path("..", ".."), winslash = "/", mustWork = TRUE)

source(file.path(project_root, "code", "00_setup", "00_packages_paths.R"))
source(file.path(project_root, "code", "utils", "cleaning_helpers.R"))

# TAXSIM Helper Tests ----

testthat::test_that("SOI thresholds convert to nominal values for sampled TAXSIM years", {
  soi_thresholds <- load_soi_thresholds(paths, years = c(2009, 2022))

  testthat::expect_identical(sort(soi_thresholds$year), c(2009, 2022))
  testthat::expect_true(all(c("nom_p90", "nom_p95", "nom_p96", "nom_p97", "nom_p98", "nom_p99") %in% names(soi_thresholds)))
  testthat::expect_true(all(soi_thresholds$nom_p95 > 0))
})

testthat::test_that("BLS raw file discovery finds sampled-year wage files recursively", {
  bls_files <- discover_bls_wage_files(paths, years = c(2009, 2022))

  testthat::expect_equal(bls_files$year, c(2009, 2022))
  testthat::expect_true(all(file.exists(bls_files$path)))
})

testthat::test_that("IRS SOI state expansion reaches all 50 states plus DC", {
  crosswalk <- load_irs_soi_crosswalk(paths)
  soi_thresholds <- load_soi_thresholds(paths, years = 2009)
  soi_inputs <- build_soi_taxsim_inputs(soi_thresholds, crosswalk)

  testthat::expect_equal(dplyr::n_distinct(crosswalk$irs_soi_code), 51)
  testthat::expect_equal(nrow(dplyr::filter(soi_inputs, percentile == "p95")), 51)
  testthat::expect_true(all(dplyr::filter(soi_inputs, percentile == "p95")$mstat == 2L))
})

testthat::test_that("BLS TAXSIM inputs preserve occupation and filing-status metadata", {
  crosswalk <- load_irs_soi_crosswalk(paths)
  bls_wages <- load_bls_wages(paths, years = 2009)
  bls_inputs <- build_bls_taxsim_inputs(bls_wages, crosswalk)

  testthat::expect_identical(sort(unique(bls_inputs$occ_code)), c("53-2011", "53-2012"))
  testthat::expect_identical(sort(unique(bls_inputs$percentile)), c("mean", "median"))
  testthat::expect_true(all(bls_inputs$mstat == 1L))
})

testthat::test_that("TAXSIM case filenames encode method and robustness case", {
  testthat::expect_identical(
    taxsim_input_filename(method = "soi", percentile = "p95"),
    "taxsim_input_soi_p95.csv"
  )
  testthat::expect_identical(
    taxsim_input_filename(method = "bls", occ_code = "53-2011", percentile = "median"),
    "taxsim_input_bls_53-2011_median.csv"
  )
  testthat::expect_identical(
    taxsim_output_filename(method = "bls", occ_code = "53-2012", percentile = "mean"),
    "taxsim_output_bls_53-2012_mean.csv"
  )
})
