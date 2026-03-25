# Test Setup ----

project_root <- normalizePath(file.path("..", ".."), winslash = "/", mustWork = TRUE)

source(file.path(project_root, "code", "00_setup", "00_packages_paths.R"))
source(file.path(project_root, "code", "utils", "cleaning_helpers.R"))

# Analysis Helper Tests ----

testthat::test_that("state-level change calculations keep one 2024 row per state", {
  input <- tibble::tibble(
    year = c(2009, 2024, 2009, 2024),
    state = c("CA", "CA", "TX", "TX"),
    n_atr_pilots = c(100, 125, 50, 60),
    tot_work_pop = c(1000, 1100, 800, 900),
    prop_atr_pilots = c(10, 12.5, 6.25, 7.5)
  )

  output <- build_state_prop_change(input)

  testthat::expect_equal(nrow(output), 2)
  testthat::expect_identical(sort(output$state), c("CA", "TX"))
  testthat::expect_true(all(output$year == 2024))
  testthat::expect_true(all(!is.na(output$d_prop_bins)))
})

testthat::test_that("total ATR share bins are computed within year", {
  input <- tibble::tibble(
    year = rep(2024, 10),
    state = c("CA", "TX", "FL", "NY", "WA", "OR", "NV", "AZ", "CO", "GA"),
    n_atr_pilots = c(12, 12, 11, 11, 10, 10, 9, 9, 8, 8),
    tot_work_pop = c(1000, 980, 950, 930, 910, 890, 870, 850, 830, 810),
    prop_atr_pilots = c(1.2, 1.22, 1.16, 1.18, 1.1, 1.12, 1.03, 1.06, 0.96, 0.99)
  )

  output <- build_state_total_share(input)

  testthat::expect_equal(round(sum(output$prop_atr_pilots_ofall), 8), 100)
  testthat::expect_true(all(!is.na(output$prop_bins)))
})
