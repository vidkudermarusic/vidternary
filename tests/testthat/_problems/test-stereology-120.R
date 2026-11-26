# Extracted from test-stereology.R:120

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "vidternary", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
sphere_sections <- function(diameter_um, per_mm3, area_mm2) {
  n <- round(per_mm3 * diameter_um / 1000 * area_mm2)
  diameter_um * sqrt(1 - ((seq_len(n) - 0.5) / n)^2)
}

# test -------------------------------------------------------------------------
mb <- inclusion_mass_balance(c(aluminate = 100, unclassified = 500))
expect_equal(mb$phases$family, "aluminate")
expect_equal(attr(mb, "skipped"), "unclassified")
none <- inclusion_mass_balance(c(unclassified = 500))
