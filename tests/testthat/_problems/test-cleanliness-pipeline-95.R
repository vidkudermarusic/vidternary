# Extracted from test-cleanliness-pipeline.R:95

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "vidternary", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
clean_result <- function(settings = list(), data = make_cleanliness_data(), cols = CLEAN_COLS, ...) {
  run_cleanliness_analysis(data, cols, utils::modifyList(list(n_boot = 300), settings), ...)
}

# test -------------------------------------------------------------------------
n <- clean_result()$notes
expect_true(any(grepl("inspected area is 2.00 mm2", n$warnings, fixed = TRUE)))
expect_true(any(grepl("smaller than 1 um", n$warnings, fixed = TRUE)))
