project_root <- normalizePath(file.path("..", ".."), winslash = "/", mustWork = TRUE)

source(file.path(project_root, "code", "00_setup", "00_packages_paths.R"))
source(file.path(project_root, "code", "utils", "ingest_helpers.R"))

testthat::test_that("year extraction works across source file names", {
  testthat::expect_equal(extract_year_from_path("RELDOMCB_2009.csv"), 2009)
  testthat::expect_equal(extract_year_from_path("PILOT_CERT_2022.csv"), 2022)
  testthat::expect_equal(extract_year_from_path("2018.tsv"), 2018)
})

testthat::test_that("FAA flat file discovery returns paired years", {
  inputs <- discover_ingest_inputs(paths, source = "faa_flat", years = c(2009, 2022))

  testthat::expect_true(all(c("year", "basic_path", "cert_path", "source") %in% names(inputs)))
  testthat::expect_equal(unname(sort(inputs$year)), c(2009, 2022))
  testthat::expect_true(all(inputs$source == "faa_flat"))
})

testthat::test_that("AviationDB discovery returns yearly TSV files", {
  inputs <- discover_ingest_inputs(paths, source = "aviationdb", years = c(2001, 2022))

  testthat::expect_true(all(c("year", "aviationdb_path", "source") %in% names(inputs)))
  testthat::expect_equal(unname(sort(inputs$year)), c(2001, 2022))
  testthat::expect_true(all(inputs$source == "aviationdb"))
})

testthat::test_that("both sources normalize to the same canonical columns", {
  faa_data <- build_pilot_ingest_dataset(paths, source = "faa_flat", years = 2009)
  aviationdb_data <- build_pilot_ingest_dataset(paths, source = "aviationdb", years = 2009)

  expected_columns <- c(
    "year",
    "unique_id",
    "state",
    "zip_code",
    "certificate_level",
    "source"
  )

  testthat::expect_identical(names(faa_data), expected_columns)
  testthat::expect_identical(names(aviationdb_data), expected_columns)
})
