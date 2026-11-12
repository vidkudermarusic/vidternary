# Tests for R/batch_size_distribution.R: k-sample Anderson-Darling test.

# The tie-adjusted statistic written out term by term (Scholz & Stephens,
# 1987), to check the vectorised version against.
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

test_that("the statistic agrees with the formula written out term by term", {
  withr::with_seed(5, {
    x <- c(stats::rnorm(15), stats::rnorm(12, 0.5), stats::rnorm(9, -0.3))
  })
  g <- rep(c("a", "b", "c"), c(15, 12, 9))
  expect_equal(ad_ksample_test(x, g, n_perm = 19)$statistic, ad_statistic_by_hand(x, g))
  # and with heavy ties, as in pixel-quantised areas
  tied <- round(x * 2) / 2
  expect_equal(ad_ksample_test(tied, g, n_perm = 19)$statistic, ad_statistic_by_hand(tied, g))
})

test_that("samples from one distribution are not told apart, shifted ones are", {
  withr::with_seed(21, {
    a <- stats::rlnorm(80); b <- stats::rlnorm(80); shifted <- stats::rlnorm(80, meanlog = 1)
  })
  same <- ad_ksample_test(c(a, b), rep(c("a", "b"), each = 80), n_perm = 499)
  diff <- ad_ksample_test(c(a, shifted), rep(c("a", "b"), each = 80), n_perm = 499)
  expect_gt(same$p_value, 0.05)
  expect_equal(diff$p_value, 1 / 500)       # the smallest p-value 499 permutations can give
  expect_gt(diff$statistic, same$statistic)
  expect_equal(same$n, c(a = 80L, b = 80L))
})

test_that("only the order of the values matters, not their scale", {
  withr::with_seed(8, x <- stats::rlnorm(60))
  g <- rep(c("a", "b", "c"), 20)
  expect_equal(ad_ksample_test(log(x), g, n_perm = 99)$statistic, ad_ksample_test(x, g, n_perm = 99)$statistic)
  expect_equal(ad_ksample_test(1000 * x, g, n_perm = 99)$p_value, ad_ksample_test(x, g, n_perm = 99)$p_value)
})

test_that("a difference in the tail only is found, and sooner than by Kolmogorov-Smirnov", {
  # same bulk, but 30 of 200 values of one sample are very large
  withr::with_seed(4, bulk <- stats::rlnorm(400))
  a <- bulk[1:200]
  b <- c(bulk[201:370], seq(20, 60, length.out = 30))
  p <- ad_ksample_test(c(a, b), rep(c("a", "b"), each = 200), n_perm = 499)$p_value
  expect_lt(p, 0.01)
  expect_lt(p, suppressWarnings(stats::ks.test(a, b)$p.value))
})

test_that("under no difference the p-value is small about as often as it should be", {
  withr::with_seed(99, {
    p <- vapply(1:120, function(i) {
      ad_ksample_test(stats::rnorm(40), rep(c("a", "b"), 20), n_perm = 99, seed = i)$p_value
    }, numeric(1))
  })
  # nominal 10 %: between 3 % and 20 % of 120 runs
  expect_gt(mean(p <= 0.1), 0.03)
  expect_lt(mean(p <= 0.1), 0.20)
})

test_that("the same seed gives the same p-value and the caller's random stream is untouched", {
  withr::with_seed(2, x <- stats::rnorm(50))
  g <- rep(c("a", "b"), 25)
  set.seed(1); before <- stats::runif(1)
  set.seed(1)
  first <- ad_ksample_test(x, g, n_perm = 199)
  expect_equal(stats::runif(1), before)
  expect_equal(ad_ksample_test(x, g, n_perm = 199)$p_value, first$p_value)
})

test_that("missing values are dropped and too little data is refused", {
  r <- ad_ksample_test(c(1, 2, NA, 4, 5, 6, Inf), c("a", "a", "a", "b", "b", "b", "b"), n_perm = 19)
  expect_equal(r$n, c(a = 2L, b = 3L))
  expect_error(ad_ksample_test(1:5, rep("a", 5)), "At least 2 samples")
  expect_error(ad_ksample_test(1:3, c("a", "a", "b")), "At least 2 samples")
})

test_that("batches are compared overall, pair by pair and by their quantiles", {
  withr::with_seed(31, {
    size <- c(stats::rlnorm(150, log(2), 0.5), stats::rlnorm(150, log(2), 0.5), stats::rlnorm(150, log(3), 0.5))
  })
  batch <- rep(c("A", "B", "C"), each = 150)
  cmp <- compare_size_distributions(size, batch, n_perm = 199)
  expect_equal(cmp$quantiles$batch, c("A", "B", "C"))
  expect_equal(cmp$quantiles$n, c(150, 150, 150))
  expect_equal(cmp$quantiles$median[1], stats::median(size[1:150]))
  expect_equal(cmp$quantiles$p99[3], unname(stats::quantile(size[301:450], 0.99)))
  expect_equal(cmp$quantiles$max[3], max(size[301:450]))

  expect_equal(paste(cmp$pairs$batch_a, cmp$pairs$batch_b), c("A B", "A C", "B C"))
  expect_equal(cmp$pairs$p_adjusted, stats::p.adjust(cmp$pairs$p_value, "holm"))
  expect_gt(cmp$pairs$p_adjusted[1], 0.05)
  expect_lt(cmp$pairs$p_adjusted[2], 0.02)
  expect_lt(cmp$pairs$p_adjusted[3], 0.02)
  expect_equal(cmp$pairs$median_ratio[2], cmp$quantiles$median[1] / cmp$quantiles$median[3])
  expect_lt(cmp$pairs$median_ratio[2], 0.85)
  expect_equal(cmp$overall$p_value, 1 / 200)
  expect_error(compare_size_distributions(1:10, rep("A", 10)), "At least 2 batches")
})
