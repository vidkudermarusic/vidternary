# Extracted from test-settings-io.R:28

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "vidternary", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
library(shiny)

# test -------------------------------------------------------------------------
s <- list(a_col = "Area.(sq..um)", a_number = 0.1 + 0.2, a_flag = FALSE, many = c("x", "y"), one_of_many = "x",
            none = character(0), empty_number = NA_real_)
path <- tempfile(fileext = ".json")
write_settings_file(s, "evs", path)
back <- read_settings_file(path, "evs")
expect_equal(back$tab, "evs")
expect_match(back$saved, "^[0-9]{4}-[0-9]{2}-[0-9]{2} ")
expect_equal(back$settings$a_col, "Area.(sq..um)")
expect_identical(back$settings$a_number, 0.1 + 0.2)
