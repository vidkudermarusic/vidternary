# Tests for R/repeatability.R: do repeated runs of one specimen agree?

test_that("runs that differ by counting scatter only are recognised as such", {
  r <- density_repeatability(c(a = 200, b = 210, c = 190), 2)
  expect_equal(r$runs$per_mm2, c(100, 105, 95))
  expect_equal(r$mean_per_mm2, 100)
  expect_equal(r$sd_observed, 5)
  # counting alone: sqrt(100 / 2) = 7.07 per mm2, more than what was seen
  expect_equal(r$sd_counting, sqrt(50))
  expect_equal(r$sd_excess, 0)
  expect_equal(r$statistic, sum((c(200, 210, 190) - 200)^2 / 200))
  expect_equal(r$df, 2)
  expect_gt(r$p_value, 0.5)
  # two runs differ by less than 1.96 * sqrt(2) * 7.07
  expect_equal(r$limit_per_mm2, stats::qnorm(0.975) * sqrt(2 * 50))
  expect_equal(r$limit_rel, r$limit_per_mm2 / 100)
})

test_that("runs that differ by more are flagged, and the excess is separated", {
  r <- density_repeatability(c(200, 300), c(2, 2))
  expect_lt(r$p_value, 0.001)
  expect_equal(r$sd_observed, stats::sd(c(100, 150)))
  expect_equal(r$sd_excess^2, stats::var(c(100, 150)) - 125 / 2, tolerance = 1e-12)
  expect_gt(r$limit_per_mm2, density_repeatability(c(250, 250), c(2, 2))$limit_per_mm2)
  expect_equal(r$runs$run, c("Run 1", "Run 2"))
})

test_that("clustering widens what counting scatter may explain", {
  plain <- density_repeatability(c(200, 260), 2)
  clustered <- density_repeatability(c(200, 260), 2, dispersion = 4)
  expect_equal(clustered$statistic, plain$statistic / 4)
  expect_gt(clustered$p_value, plain$p_value)
  expect_equal(clustered$sd_counting, 2 * plain$sd_counting)
})

test_that("the false-alarm rate of the test is the nominal one", {
  withr::local_seed(4)
  p <- replicate(2000, density_repeatability(stats::rpois(3, 300), 2)$p_value)
  expect_gt(mean(p < 0.05), 0.03)
  expect_lt(mean(p < 0.05), 0.07)
})

test_that("unequal areas are taken into account", {
  r <- density_repeatability(c(100, 300), c(1, 3))
  expect_equal(r$runs$per_mm2, c(100, 100))
  expect_equal(r$statistic, 0)
  expect_equal(r$sd_counting^2, mean(100 / c(1, 3)))
})

test_that("bad input is refused", {
  expect_error(density_repeatability(200, 2), "At least 2 repeated runs")
  expect_error(density_repeatability(c(1, 2), c(1, 2, 3)), "One inspected area is needed per run")
  expect_error(density_repeatability(c(1, 2), 0), "greater than zero")
  expect_error(density_repeatability(c(0, 0), 1), "no particles")
})

test_that("other quantities get their scatter and a limit", {
  q <- quantity_repeatability(list(`Median ECD (um)` = c(2, 2.2, 1.8), alone = 5, with_gap = c(1, NA, 3)))
  expect_equal(q$quantity, c("Median ECD (um)", "alone", "with_gap"))
  expect_equal(q$n_runs, c(3L, 1L, 2L))
  expect_equal(q$mean[1], 2)
  expect_equal(q$sd[1], 0.2)
  expect_equal(q$cv[1], 0.1)
  expect_equal(q$limit[1], stats::qnorm(0.975) * sqrt(2) * 0.2)
  expect_equal(c(q$min[1], q$max[1]), c(1.8, 2.2))
  expect_true(is.na(q$sd[2]))                       # one run: no scatter
  expect_equal(q$mean[3], 2)
})
