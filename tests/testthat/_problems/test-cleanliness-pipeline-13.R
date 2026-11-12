# Extracted from test-cleanliness-pipeline.R:13

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "vidternary", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
clean_result <- function(settings = list(), data = make_cleanliness_data(), cols = CLEAN_COLS, ...) {
  run_cleanliness_analysis(data, cols, utils::modifyList(list(n_boot = 300), settings), ...)
}

# test -------------------------------------------------------------------------
r <- clean_result()
n <- table(r$particles$family)
expect_setequal(names(n), c("aluminate", "silicate", "sulfide", "Ti-rich", "unclassified"))
expect_equal(as.integer(n[c("aluminate", "silicate", "sulfide", "Ti-rich", "unclassified")]), c(3, 1, 1, 5, 22))
