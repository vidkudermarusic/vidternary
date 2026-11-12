# Tests for R/batch_density.R: number density of several batches.

test_that("the index of dispersion is the variance over the mean of the counts per field", {
  d <- field_count_dispersion(c(1, 1, 2, 2, 2, 3))
  expect_equal(sort(d$counts), c(1, 2, 3))
  expect_equal(d$n_fields, 3)
  expect_equal(d$mean, 2)
  expect_equal(d$dispersion, 0.5)
})

test_that("fields without particles count as zeros", {
  d <- field_count_dispersion(c(1, 1, 2, 2, 2, 3), n_fields = 5)
  expect_equal(sort(d$counts), c(0, 0, 1, 2, 3))
  expect_equal(d$mean, 1.2)
  expect_equal(d$dispersion, stats::var(c(2, 3, 1, 0, 0)) / 1.2)
  # a number of fields smaller than those seen is ignored
  expect_equal(field_count_dispersion(c(1, 2, 3), n_fields = 1)$n_fields, 3)
})

test_that("random scatter gives a dispersion near 1 and clusters a larger one", {
  withr::with_seed(11, {
    scattered <- sample(1:200, 2000, replace = TRUE)
    clustered <- c(sample(1:200, 1000, replace = TRUE), rep(7, 1000))
  })
  expect_lt(abs(field_count_dispersion(scattered, 200)$dispersion - 1), 0.25)
  expect_gt(field_count_dispersion(clustered, 200)$dispersion, 20)
})

test_that("degenerate inputs give NA, not an error", {
  expect_true(is.na(field_count_dispersion(c(5, 5, 5))$dispersion))     # one field
  expect_true(is.na(field_count_dispersion(character(0))$dispersion))   # no particles
})

test_that("two batches are compared by their rate ratio with the exact test", {
  cmp <- compare_densities(c(A = 200, B = 100), c(2, 2))
  expect_equal(cmp$pairs$batch_a, "A")
  expect_equal(cmp$pairs$batch_b, "B")
  expect_equal(cmp$pairs$ratio, 2)
  expect_lt(cmp$pairs$ratio_lower, 2)
  expect_gt(cmp$pairs$ratio_upper, 2)
  expect_gt(cmp$pairs$ratio_lower, 1)
  # equal areas: the exact test is the binomial test of 200 out of 300 against 1/2
  expect_equal(cmp$pairs$p_value, stats::binom.test(200, 300, 0.5)$p.value)
  expect_equal(cmp$pairs$p_adjusted, cmp$pairs$p_value)
  # chi-squared: (200 - 150)^2 / 150 + (100 - 150)^2 / 150
  expect_equal(cmp$overall$statistic, 2 * 50^2 / 150)
  expect_equal(cmp$overall$df, 1)
})

test_that("counts in proportion to the inspected areas are not a difference", {
  cmp <- compare_densities(c(A = 100, B = 200), c(1, 2))
  expect_equal(cmp$pairs$ratio, 1)
  expect_equal(cmp$pairs$p_value, 1)
  expect_equal(cmp$overall$statistic, 0)
})

test_that("three batches give three pairs with Holm-adjusted p-values", {
  cmp <- compare_densities(c(A = 200, B = 190, C = 100), c(2, 2, 2))
  expect_equal(paste(cmp$pairs$batch_a, cmp$pairs$batch_b), c("A B", "A C", "B C"))
  expect_equal(cmp$pairs$p_adjusted, stats::p.adjust(cmp$pairs$p_value, "holm"))
  expect_true(all(cmp$pairs$p_adjusted >= cmp$pairs$p_value))
  expect_gt(cmp$pairs$p_adjusted[1], 0.05)
  expect_lt(cmp$pairs$p_adjusted[2], 0.001)
  expect_equal(cmp$overall$df, 2)
})

test_that("unnamed counts, empty batches and bad input are handled", {
  expect_equal(compare_densities(c(5, 9), c(1, 1))$pairs$batch_a, "Batch 1")
  none <- compare_densities(c(A = 0, B = 0), c(1, 1))
  expect_true(is.na(none$pairs$ratio))
  expect_true(is.na(none$overall$p_value))
  expect_error(compare_densities(c(A = 5), 1), "At least 2 batches")
  expect_error(compare_densities(c(A = 5, B = 6), 1), "One inspected area")
  expect_error(compare_densities(c(A = 5, B = 6), c(1, 0)), "greater than zero")
})

test_that("the rank test on counts per field sees a shift and ignores none", {
  same <- compare_field_counts(list(A = c(1, 2, 3, 4, 5, 6), B = c(1, 2, 3, 4, 5, 6)))
  expect_gt(same$p_value, 0.9)
  shifted <- compare_field_counts(list(A = rep(c(1, 2, 3), 10), B = rep(c(11, 12, 13), 10)))
  expect_lt(shifted$p_value, 1e-6)
  expect_equal(shifted$df, 1)
})

test_that("the rank test gives NA when it cannot be made", {
  expect_true(is.na(compare_field_counts(list(A = c(2, 2, 2), B = c(2, 2, 2)))$p_value))
  expect_true(is.na(compare_field_counts(list(A = 3, B = c(1, 2)))$p_value))
  expect_true(is.na(compare_field_counts(list(A = c(1, 2)))$p_value))
})
