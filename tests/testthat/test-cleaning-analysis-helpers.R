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

testthat::test_that("mover panel flags moves within pilot-year order", {
  input <- tibble::tibble(
    year = c(2011, 2009, 2010, 2014),
    fips = c("06", "06", "06", "32"),
    statefull = c("California", "California", "California", "Nevada"),
    state = c("CA", "CA", "CA", "NV"),
    unique_id = c("p1", "p1", "p1", "p1"),
    first_name = "A",
    last_name = "Pilot",
    street_1 = "1 Main",
    city = "Town",
    zip_code = "00000",
    num_years = 4
  )

  output <- build_pilot_mover_panel(input)

  testthat::expect_equal(output$year, c(2009, 2010, 2011, 2014))
  testthat::expect_true(is.na(output$origin_state[[1]]))
  testthat::expect_identical(output$moved, c(0L, 0L, 0L, 1L))
})

testthat::test_that("move counts equal the within-pilot sum of moves", {
  input <- tibble::tibble(
    year = c(2009, 2010, 2011, 2009, 2010),
    fips = c("06", "32", "32", "12", "12"),
    statefull = c("California", "Nevada", "Nevada", "Florida", "Florida"),
    state = c("CA", "NV", "NV", "FL", "FL"),
    unique_id = c("p1", "p1", "p1", "p2", "p2"),
    first_name = "A",
    last_name = "Pilot",
    street_1 = "1 Main",
    city = "Town",
    zip_code = "00000",
    num_years = c(3, 3, 3, 2, 2)
  )

  output <- input |>
    build_pilot_mover_panel() |>
    add_pilot_move_counts() |>
    dplyr::distinct(.data$unique_id, .data$num_moves)

  testthat::expect_equal(output$num_moves[output$unique_id == "p1"], 1)
  testthat::expect_equal(output$num_moves[output$unique_id == "p2"], 0)
})

testthat::test_that("flow time fields handle non-consecutive years", {
  input <- tibble::tibble(
    year = c(2009, 2011, 2014),
    unique_id = c("p1", "p1", "p1"),
    dest_state = c("CA", "NV", "OR"),
    origin_state = c(NA, "CA", "NV"),
    moved = c(0L, 1L, 1L)
  )

  output <- add_flow_time_fields(input)

  testthat::expect_true(is.na(output$lag_year[[1]]))
  testthat::expect_identical(output$time_period_yrs, c(NA_integer_, 2L, 3L))
})

testthat::test_that("mover period summary uses legacy labels and monthly divisors", {
  input <- tibble::tibble(
    year = c(2009, 2009, 2010, 2010, 2011, 2011),
    unique_id = c("p1", "p2", "p1", "p2", "p1", "p2"),
    dest_state = c("CA", "TX", "NV", "TX", "NV", "FL"),
    origin_state = c(NA, NA, "CA", "TX", "NV", "TX"),
    moved = c(0L, 0L, 1L, 0L, 0L, 1L)
  )

  output <- build_mover_period_summary(input)

  testthat::expect_identical(output$`Time period`, c("11/2009 - 05/2010", "05/2010 - 09/2011"))
  testthat::expect_equal(output$`Ave. monthly % moved`, c((50 / 6), (50 / 16)))
})

testthat::test_that("2016-2017 mover period reflects September 2017 timing", {
  input <- tibble::tibble(
    year = c(2016, 2016, 2017, 2017),
    unique_id = c("p1", "p2", "p1", "p2"),
    dest_state = c("CA", "TX", "NV", "TX"),
    origin_state = c(NA, NA, "CA", "TX"),
    moved = c(0L, 0L, 1L, 0L)
  )

  output <- build_mover_period_summary(input)

  testthat::expect_identical(output$`Time period`, "11/2016 - 09/2017")
  testthat::expect_equal(output$`Ave. monthly % moved`, 50 / 10)
})

testthat::test_that("top-state selection is deterministic on average ATR pilot counts", {
  input <- tibble::tibble(
    year = c(2022, 2024, 2022, 2024, 2022, 2024),
    state = c("CA", "CA", "TX", "TX", "FL", "FL"),
    n_atr_pilots = c(100, 120, 100, 120, 90, 95)
  )

  output <- select_top_states_by_average_atr_count(input, n_states = 2L)

  testthat::expect_identical(output, c("CA", "TX"))
})
