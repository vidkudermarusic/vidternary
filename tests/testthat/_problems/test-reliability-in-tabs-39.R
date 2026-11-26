# Extracted from test-reliability-in-tabs.R:39

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "vidternary", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
library(shiny)
clean_run <- function(settings = list(), data = make_cleanliness_data(), cols = CLEAN_COLS, ...) {
  run_cleanliness_analysis(data, cols, utils::modifyList(list(n_boot = 300), settings), ...)
}

# test -------------------------------------------------------------------------
off <- clean_run(list(min_size_to_range = 0))
expect_false(any(off$particles$family == "too small to classify"))
expect_equal(sum(off$particles$family == "unclassified"), 22)
high <- clean_run(list(beam_kv = 30))
expect_equal(high$range_um, electron_range_um(30, "Fe"))
expect_gt(sum(high$particles$family == "too small to classify"), 22)
