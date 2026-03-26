# Test Setup ----

project_root <- normalizePath(file.path("..", ".."), winslash = "/", mustWork = TRUE)
setwd(project_root)

source(file.path(project_root, "code", "00_setup", "00_packages_paths.R"))
source(file.path(project_root, "code", "utils", "cleaning_helpers.R"))

# Pilot-Level BLS Tax Merge Helper Tests ----

test_that("BLS PIT case filtering isolates the requested case", {
  input <- tibble(
    year = c(2021, 2021, 2022, 2023),
    fips = c(1, 1, 2, 2),
    pilot_type = c("airline", "airline", "airline", "airline"),
    percentile = c("mean", "median", "mean", "mean"),
    atr = c(0.2, 0.3, 0.25, 0.26)
  )

  output <- filter_bls_pit_case(input, pilot_type = "airline", percentile = "mean", max_year = 2022L)

  expect_equal(output$year, c(2021, 2022))
  expect_true(all(output$percentile == "mean"))
})

test_that("sparse ATR changes are computed within state across observed years", {
  input <- tibble(
    year = c(2009, 2010, 2009, 2010),
    fips = c(1, 1, 2, 2),
    atr = c(0.20, 0.22, 0.30, 0.33),
    pilot_type = "airline",
    percentile = "mean"
  )

  output <- build_sparse_atr_changes(input, years = c(2009, 2010))

  expect_true(is.na(output$atr_change[[1]]))
  expect_equal(output$atr_change[[2]], (log(0.22) - log(0.20)) * 100)
  expect_equal(output$atr_change[[4]], (log(0.33) - log(0.30)) * 100)
})

test_that("pilot-level BLS tax merge aligns destination and origin ATR variables", {
  mover_panel <- tibble(
    year = c(2009, 2010, 2009, 2010),
    fips = c(6, 32, 48, 48),
    statefull = c("California", "Nevada", "Texas", "Texas"),
    state = c("CA", "NV", "TX", "TX"),
    unique_id = c("p1", "p1", "p2", "p2"),
    first_name = "A",
    last_name = "Pilot",
    street_1 = "1 Main",
    city = "Town",
    zip_code = "00000",
    num_years = 2,
    dest_state = c("CA", "NV", "TX", "TX"),
    origin_state = c(NA, "CA", NA, "TX"),
    moved = c(0L, 1L, 0L, 0L),
    num_moves = c(1L, 1L, 0L, 0L)
  )

  pit_bls <- tibble(
    year = c(2009, 2010, 2009, 2010, 2009, 2010),
    fips = c(6, 6, 32, 32, 48, 48),
    pilot_type = "airline",
    percentile = "mean",
    atr = c(0.20, 0.21, 0.15, 0.16, 0.30, 0.31)
  )

  state_crosswalk <- tibble(
    fips = c(6, 32, 48),
    statefull = c("California", "Nevada", "Texas"),
    state = c("CA", "NV", "TX")
  )

  output <- build_pilots_atr_tax_merged_bls(
    mover_panel = mover_panel,
    pit_bls = pit_bls,
    state_crosswalk = state_crosswalk,
    pilot_type = "airline",
    percentile = "mean",
    max_year = 2022L,
    years_for_changes = c(2009, 2010)
  )

  p1_2010 <- output |>
    filter(.data$unique_id == "p1", .data$year == 2010)

  expect_equal(nrow(output), 4)
  expect_equal(p1_2010$dest_atr, 0.16)
  expect_equal(p1_2010$origin_fips, 6)
  expect_equal(p1_2010$origin_atr, 0.21)
})

test_that("legacy LPM sample restriction keeps only balanced non-AK-HI pilots", {
  input <- tibble(
    year = c(2009:2017, 2009:2016, 2009:2017),
    unique_id = c(rep("p1", 9), rep("p2", 8), rep("p3", 9)),
    origin_fips = c(rep(1, 9), rep(1, 8), rep(2, 9)),
    origin_state = c(rep("CA", 9), rep("CA", 8), rep("AK", 9)),
    dest_state = c(rep("NV", 9), rep("NV", 8), rep("NV", 9)),
    origin_atr = rep(0.2, 26),
    moved = rep(1L, 26)
  )

  output <- input |>
    filter(.data$year != 2024) |>
    group_by(.data$unique_id) |>
    mutate(num_years = n()) |>
    ungroup() |>
    filter(!is.na(.data$origin_fips), .data$num_years == 9) |>
    filter(
      .data$origin_state != "AK",
      .data$origin_state != "HI",
      .data$dest_state != "AK",
      .data$dest_state != "HI"
    ) |>
    mutate(net_origin_atr = (1 - .data$origin_atr) * 100)

  expect_identical(unique(output$unique_id), "p1")
  expect_true(all(output$num_years == 9))
  expect_true(all(output$net_origin_atr == 80))
})
