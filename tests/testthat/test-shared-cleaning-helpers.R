# Test Setup ----

project_root <- normalizePath(file.path("..", ".."), winslash = "/", mustWork = TRUE)
setwd(project_root)

source(file.path(project_root, "code", "00_setup", "00_packages_paths.R"))
source(file.path(project_root, "code", "utils", "cleaning_helpers.R"))

# Shared Helper Tests ----

test_that("state FIPS crosswalk loads the expected shared columns", {
  output <- load_state_fips_crosswalk(paths)

  expect_true(all(c("fips", "statefull", "state") %in% names(output)))
  expect_true(nrow(output) >= 51)
  expect_false(anyDuplicated(output$state))
})
