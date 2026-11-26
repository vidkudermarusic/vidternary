# Tests for R/planning_precision.R and R/planning_evs.R: how much area a
# wanted precision or a difference between batches takes.

test_that("the area for a precision follows from the counting formula", {
  z <- stats::qnorm(0.975)
  # 100 per mm2, +/- 10 %: z^2 / (100 * 0.01)
  expect_equal(area_for_density_precision(100, 0.1), z^2, tolerance = 1e-12)
  # clustering by a factor 3 takes 3 times the area; half the interval 4 times
  expect_equal(area_for_density_precision(100, 0.1, dispersion = 3), 3 * z^2, tolerance = 1e-12)
  expect_equal(area_for_density_precision(100, 0.05) / area_for_density_precision(100, 0.1), 4)
  # and back again
  expect_equal(density_relative_halfwidth(z^2, 100), 0.1, tolerance = 1e-12)
  expect_equal(density_relative_halfwidth(area_for_density_precision(40, 0.07, 2.5), 40, 2.5), 0.07, tolerance = 1e-12)
})

test_that("the predicted interval covers the true density of clustered counts", {
  # negative binomial counts per field: mean 3, variance 9, so index of dispersion 3
  withr::local_seed(1)
  half <- density_relative_halfwidth(400, 3, dispersion = 3)       # 400 fields of area 1
  est <- replicate(3000, mean(stats::rnbinom(400, mu = 3, size = 1.5)))
  coverage <- mean(abs(est - 3) / 3 <= half)
  expect_gt(coverage, 0.93)
  expect_lt(coverage, 0.97)
  # the Poisson formula would be far too optimistic here
  expect_lt(mean(abs(est - 3) / 3 <= density_relative_halfwidth(400, 3)), 0.80)
})

test_that("the area for a ratio gives the power asked for", {
  z <- stats::qnorm(0.975) + stats::qnorm(0.8)
  expect_equal(area_for_density_ratio(100, 1.2), z^2 * (1 + 1 / 1.2) / (100 * log(1.2)^2), tolerance = 1e-12)
  # a ratio below 1 is a difference too; a smaller one takes more area
  expect_gt(area_for_density_ratio(100, 0.8), 0)
  expect_gt(area_for_density_ratio(100, 1.1), area_for_density_ratio(100, 1.2))
  expect_gt(area_for_density_ratio(100, 1.2, power = 0.9), area_for_density_ratio(100, 1.2, power = 0.8))

  withr::local_seed(2)
  n_fields <- ceiling(area_for_density_ratio(3, 1.2, dispersion = 3))
  rejected <- replicate(2000, {
    a <- stats::rnbinom(n_fields, mu = 3, size = 1.5)
    b <- stats::rnbinom(n_fields, mu = 3.6, size = 1.8)
    d <- (stats::var(a) / mean(a) + stats::var(b) / mean(b)) / 2
    abs(log(sum(b) / sum(a)) / sqrt(d * (1 / sum(a) + 1 / sum(b)))) > stats::qnorm(0.975)
  })
  expect_gt(mean(rejected), 0.76)
  expect_lt(mean(rejected), 0.84)
})

test_that("the detectable ratio is the inverse of the area for a ratio", {
  area <- area_for_density_ratio(50, 1.3, dispersion = 2)
  expect_equal(detectable_density_ratio(area, 50, dispersion = 2), 1.3, tolerance = 1e-6)
  expect_gt(detectable_density_ratio(1, 50), detectable_density_ratio(10, 50))
})

test_that("impossible inputs are refused with a message", {
  expect_error(area_for_density_precision(0, 0.1), "number density must be greater than zero")
  expect_error(area_for_density_precision(100, 0), "wanted precision must be greater than zero")
  expect_error(area_for_density_precision(100, 0.1, dispersion = 0), "index of dispersion must be greater than zero")
  expect_error(area_for_density_ratio(100, 1), "positive number other than 1")
  expect_error(area_for_density_ratio(100, 1.2, power = 1), "Power must lie between")
  expect_error(density_relative_halfwidth(0, 100), "area must be greater than zero")
})

test_that("more control areas narrow the return level, by about the square root", {
  withr::local_seed(3)
  fit <- fit_evs_gumbel(5 - 2 * log(-log(stats::runif(40))))
  plan <- evs_precision_by_areas(fit, 100, c(40, 160), n_sim = 600)
  expect_equal(plan$n_areas, c(40L, 160L))
  expect_equal(plan$return_level, rep(fit$intercept + fit$slope * -log(-log(0.99)), 2))
  expect_equal(plan$halfwidth_um[1] / plan$halfwidth_um[2], 2, tolerance = 0.2)
  expect_equal(plan$rel_halfwidth, plan$halfwidth_um / plan$return_level)
  # same seed, same plan
  expect_equal(evs_precision_by_areas(fit, 100, c(40, 160), n_sim = 600), plan)
  expect_error(evs_precision_by_areas(fit, 100, 2), "3 or more control areas")
  expect_error(evs_precision_by_areas(fit, 1, 40), "greater than 1")
})
