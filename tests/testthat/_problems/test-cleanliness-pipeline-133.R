# Extracted from test-cleanliness-pipeline.R:133

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "vidternary", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
clean_result <- function(settings = list(), data = make_cleanliness_data(), cols = CLEAN_COLS, ...) {
  run_cleanliness_analysis(data, cols, utils::modifyList(list(n_boot = 300), settings), ...)
}

# test -------------------------------------------------------------------------
d <- make_cleanliness_data()
d$MyClass <- ifelse(d[[CLEAN_COLS$breadth]] > 1, "oxide", NA)
r <- clean_result(data = d, cols = c(CLEAN_COLS, list(family = "MyClass")))
