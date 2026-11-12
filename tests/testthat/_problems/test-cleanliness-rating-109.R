# Extracted from test-cleanliness-rating.R:109

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "vidternary", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
rating_objects <- function(type, series, field, length = NA, diameter = NA, oversize = FALSE, rated = TRUE) {
  data.frame(type = type, series = series, field = field, length = length, diameter = diameter,
             oversize = oversize, rated = rated, stringsAsFactors = FALSE)
}
rating_grid <- function(objects) {
  g <- rating_field_grid(numeric(0), numeric(0), bbox = c(0, 2000, 0, 1000))
  g$field <- objects$field
  g
}

# test -------------------------------------------------------------------------
o <- rating_objects(character(0), character(0), integer(0))
