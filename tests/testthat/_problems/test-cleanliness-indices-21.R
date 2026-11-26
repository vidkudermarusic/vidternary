# Extracted from test-cleanliness-indices.R:21

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "vidternary", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
ind <- cleanliness_indices(area_um2 = c(1, 4, 9), group = c("a", "a", "b"), inspected_area_mm2 = 2)
expect_equal(ind$group, c("All", "a", "b"))
expect_equal(ind$n, c(3, 2, 1))
expect_equal(ind$per_mm2, c(1.5, 1, 0.5))
expect_equal(ind$area_ppm, c(7, 2.5, 4.5))
expect_equal(ind$max_sqrt_area_um, c(3, 2, 3))
expect_equal(ind$max_ecd_um, 2 * sqrt(c(9, 4, 9) / pi))
expect_equal(ind$mean_ecd_um[2], mean(2 * sqrt(c(1, 4) / pi)))
expect_equal(ind$per_mm2_lower[1], poisson_count_ci(3)[1, "lower"] / 2)
