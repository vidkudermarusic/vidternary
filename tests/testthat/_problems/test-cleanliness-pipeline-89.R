# Extracted from test-cleanliness-pipeline.R:89

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "vidternary", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
clean_result <- function(settings = list(), data = make_cleanliness_data(), cols = CLEAN_COLS, ...) {
  run_cleanliness_analysis(data, cols, utils::modifyList(list(n_boot = 300), settings), ...)
}

# test -------------------------------------------------------------------------
r <- clean_result(list(measured_ppm = c(S = 100)))
mb <- r$mass_balance
expect_setequal(mb$phases$family, c("aluminate", "silicate", "sulfide", "Ti-rich"))
expect_equal(mb$phases$volume_ppm[mb$phases$family == "sulfide"], pi / 4 * 50 * 3 / 2)
expect_equal(mb$elements$measured_ppm[mb$elements$element == "S"], 100)
expect_equal(attr(mb, "skipped"), "unclassified")
