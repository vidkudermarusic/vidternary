# Extracted from test-cleanliness-indices.R:85

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "vidternary", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
tab <- size_class_table(c(0.5, 1, 1.5, 3, 25), c("a", "a", "b", "b", "b"), inspected_area_mm2 = 2, breaks = c(1, 2, 5))
expect_equal(levels(tab$size_class), c("< 1", "1 - 2", "2 - 5", ">= 5"))
all_rows <- tab[tab$group == "All", ]
expect_equal(all_rows$n, c(1, 2, 1, 1))
expect_equal(all_rows$per_mm2, c(0.5, 1, 0.5, 0.5))
expect_equal(tab$n[tab$group == "a"], c(1, 1, 0, 0))
expect_equal(tab$n[tab$group == "b"], c(0, 1, 1, 1))
expect_equal(all_rows$per_mm2_upper[1], poisson_count_ci(1)[1, "upper"] / 2)
