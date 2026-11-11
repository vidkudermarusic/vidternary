rgumbel <- function(k, a, b) a - b * log(-log(stats::runif(k)))

test_that("evs_bootstrap_params refits like fit_evs_gumbel and is reproducible", {
  fit <- fit_evs_gumbel(c(2.1, 3.4, 3.9, 4.4, 5.2, 6.0, 7.7, 9.1))
  p1 <- evs_bootstrap_params(fit, n_boot = 300, seed = 3)
  p2 <- evs_bootstrap_params(fit, n_boot = 300, seed = 3)
  expect_equal(dim(p1), c(300, 2))
  expect_identical(p1, p2)
  expect_true(all(p1[, "b"] > 0))
  # Centre of the refits is near the fitted line's parameters.
  expect_equal(mean(p1[, "b"]), fit$slope, tolerance = 0.25)
})

test_that("the bootstrap leaves the caller's RNG state alone, also when no seed existed", {
  fit <- fit_evs_gumbel(c(2.1, 3.4, 3.9, 4.4, 5.2, 6.0, 7.7, 9.1))
  set.seed(10)
  invisible(stats::runif(1))
  expected <- stats::runif(1)
  set.seed(10)
  invisible(stats::runif(1))
  invisible(predict_evs_max(fit, 20, n_boot = 100))
  expect_equal(stats::runif(1), expected)

  if (exists(".Random.seed", envir = .GlobalEnv)) rm(".Random.seed", envir = .GlobalEnv)
  invisible(predict_evs_max(fit, 20, n_boot = 100))
  expect_false(exists(".Random.seed", envir = .GlobalEnv))
  set.seed(1)
})

test_that("predict_evs_max intervals are close to nominal on Gumbel data (T = number of areas)", {
  set.seed(21)
  a <- 3; b <- 1.5; n <- 30; T <- 30; truth <- a - b * log(-log(1 - 1 / T))
  hit <- t(replicate(150, {
    x <- rgumbel(n, a, b)
    p <- predict_evs_max(fit_evs_gumbel(x), T, n_boot = 400)
    m <- max(rgumbel(T, a, b))
    c(ci = truth >= p$ci_lower && truth <= p$ci_upper, max = m >= p$lower && m <= p$upper)
  }))
  # The regression intervals used before covered these only 23% / 29% of the
  # time; with 150 repeats the bootstrap ones must be well above 80%.
  expect_gt(mean(hit[, "ci"]), 0.8)
  expect_gt(mean(hit[, "max"]), 0.8)
})

test_that("evs_line_band brackets the fitted line and widens away from the data", {
  set.seed(4)
  fit <- fit_evs_gumbel(rgumbel(25, 3, 1.5))
  band <- evs_line_band(fit, c(0, 2, 5), n_boot = 400)
  line <- fit$intercept + fit$slope * band$y
  expect_true(all(band$lower <= line & line <= band$upper))
  expect_gt(band$upper[3] - band$lower[3], band$upper[1] - band$lower[1])
})
