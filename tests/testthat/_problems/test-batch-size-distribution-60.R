# Extracted from test-batch-size-distribution.R:60

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "vidternary", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
ad_statistic_by_hand <- function(x, group) {
  group <- factor(group)
  z <- sort(unique(x))
  n_total <- length(x)
  total <- 0
  for (g in levels(group)) {
    xi <- x[group == g]
    inner <- 0
    for (j in seq_along(z)) {
      l <- sum(x == z[j])
      b <- sum(x < z[j]) + l / 2
      m <- sum(xi < z[j]) + sum(xi == z[j]) / 2
      denom <- b * (n_total - b) - n_total * l / 4
      if (denom > 0) inner <- inner + l / n_total * (n_total * m - length(xi) * b)^2 / denom
    }
    total <- total + inner / length(xi)
  }
  (n_total - 1) / n_total * total
}

# test -------------------------------------------------------------------------
withr::with_seed(4, bulk <- stats::rlnorm(400))
a <- bulk[1:200]
b <- c(bulk[201:380], seq(20, 60, length.out = 20))
expect_lt(ad_ksample_test(c(a, b), rep(c("a", "b"), each = 200), n_perm = 499)$p_value, 0.01)
