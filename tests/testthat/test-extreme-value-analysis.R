# Tests for R/extreme_value_analysis.R - Murakami/Gumbel EVS pipeline.
# Pure statistics, no Shiny dependency, so these run directly without
# testServer().

test_that("compute_block_maxima takes the max sqrt(area) per group", {
  d <- data.frame(
    area = c(4, 9, 16, 1, 25, 36),
    field = c("A", "A", "A", "B", "B", "B")
  )
  bm <- compute_block_maxima(d, "area", "field")
  bm <- bm[order(bm$group), ]
  expect_equal(bm$group, c("A", "B"))
  expect_equal(bm$sqrt_area_max, c(4, 6))
  expect_equal(bm$n_inclusions, c(3L, 3L))
})

test_that("compute_block_maxima drops non-finite, non-positive, and NA-grouped rows", {
  d <- data.frame(
    area = c(4, NA, -1, Inf, 9, 16),
    field = c("A", "A", "A", "A", "A", NA)
  )
  bm <- compute_block_maxima(d, "area", "field")
  expect_equal(nrow(bm), 1)
  expect_equal(bm$group, "A")
  expect_equal(bm$n_inclusions, 2L)
  expect_equal(bm$sqrt_area_max, 3)
})

test_that("fit_evs_gumbel requires at least 3 groups", {
  expect_error(fit_evs_gumbel(c(1, 2)))
})

test_that("fit_evs_gumbel recovers known intercept/slope from a perfect linear relationship", {
  n <- 30
  j <- seq_len(n)
  F <- j / (n + 1)
  y <- -log(-log(F))
  a <- 5; b <- 2
  sqrt_area_max <- a + b * y
  fit <- suppressWarnings(fit_evs_gumbel(sqrt_area_max))
  expect_equal(fit$n, n)
  expect_equal(fit$intercept, a, tolerance = 1e-6)
  expect_equal(fit$slope, b, tolerance = 1e-6)
  expect_equal(fit$r_squared, 1, tolerance = 1e-6)
})

test_that("fit_evs_gumbel sorts input and drops non-finite/non-positive values", {
  fit <- fit_evs_gumbel(c(5, NA, -1, 0, 3, 8, 1))
  expect_equal(fit$n, 4)
  expect_equal(fit$data$sqrt_area_max, sort(c(5, 3, 8, 1)))
})

test_that("predict_evs_max rejects a return period <= 1", {
  fit <- fit_evs_gumbel(c(1, 2, 3, 4, 5))
  expect_error(predict_evs_max(fit, 1))
  expect_error(predict_evs_max(fit, 0.5))
  expect_error(predict_evs_max(fit, NA))
})

test_that("predict_evs_max returns an ordered prediction interval that grows with return period", {
  set.seed(1)
  n <- 20
  j <- seq_len(n)
  y <- -log(-log(j / (n + 1)))
  sqrt_area_max <- 5 + 2 * y + stats::rnorm(n, sd = 0.1)
  fit <- fit_evs_gumbel(sqrt_area_max)

  pred <- predict_evs_max(fit, return_period = 100)
  expect_equal(pred$return_period, 100)
  expect_true(pred$lower <= pred$predicted)
  expect_true(pred$predicted <= pred$upper)

  pred2 <- predict_evs_max(fit, return_period = 10)
  expect_true(pred$predicted > pred2$predicted)
})

test_that("gumbel_goodness_of_fit returns a valid bootstrap p-value", {
  set.seed(2)
  n <- 15
  j <- seq_len(n)
  y <- -log(-log(j / (n + 1)))
  sqrt_area_max <- 5 + 2 * y + stats::rnorm(n, sd = 0.05)
  fit <- fit_evs_gumbel(sqrt_area_max)
  gof <- gumbel_goodness_of_fit(fit, n_sim = 199, seed = 42)

  expect_true(is.finite(gof$statistic))
  expect_true(gof$p_value >= 0 && gof$p_value <= 1)
  expect_equal(gof$reject_at_05, gof$p_value < 0.05)
  expect_equal(gof$n, n)
})

test_that("gumbel_goodness_of_fit is reproducible for a fixed seed", {
  fit <- fit_evs_gumbel(c(1, 2, 3, 5, 8, 13, 21))
  gof1 <- gumbel_goodness_of_fit(fit, n_sim = 99, seed = 7)
  gof2 <- gumbel_goodness_of_fit(fit, n_sim = 99, seed = 7)
  expect_identical(gof1$p_value, gof2$p_value)
})

test_that("gumbel_goodness_of_fit restores the caller's RNG state", {
  fit <- fit_evs_gumbel(c(1, 2, 3, 5, 8, 13, 21))
  set.seed(123)
  before <- stats::runif(1)
  set.seed(123)
  invisible(gumbel_goodness_of_fit(fit, n_sim = 50, seed = 999))
  after <- stats::runif(1)
  expect_equal(before, after)
})

test_that("create_gumbel_plot returns a ggplot object, with and without a prediction", {
  fit <- fit_evs_gumbel(c(1, 2, 3, 5, 8, 13, 21))
  p1 <- create_gumbel_plot(fit)
  expect_s3_class(p1, "ggplot")

  pred <- predict_evs_max(fit, return_period = 50)
  p2 <- create_gumbel_plot(fit, pred)
  expect_s3_class(p2, "ggplot")
})
