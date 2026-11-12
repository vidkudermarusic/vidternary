# Tests for R/fatigue_murakami.R: fatigue limit from the largest inclusion.

test_that("the fatigue limit follows Murakami's formula for the three inclusion locations", {
  # HV 400, sqrt(area) 10 um: (HV + 120) / 10^(1/6) = 520 / 1.46780 = 354.27
  base <- 520 / 10^(1 / 6)
  expect_equal(murakami_fatigue_limit(10, 400, "surface"), 1.43 * base)
  expect_equal(murakami_fatigue_limit(10, 400, "subsurface"), 1.41 * base)
  expect_equal(murakami_fatigue_limit(10, 400, "internal"), 1.56 * base)
  expect_equal(murakami_fatigue_limit(10, 400), 506.6, tolerance = 1e-4)
})

test_that("a larger inclusion lowers the fatigue limit by the sixth root of its size", {
  s <- murakami_fatigue_limit(c(5, 10, 20, 640), 300)
  expect_true(all(diff(s) < 0))
  expect_equal(s[2] / s[3], 2^(1 / 6))
  expect_equal(s[2] / s[4], 2)       # 64 times larger -> half the fatigue limit
})

test_that("the stress ratio lowers the amplitude with the exponent 0.226 + HV / 10000", {
  r_minus1 <- murakami_fatigue_limit(10, 400)
  expect_equal(murakami_fatigue_limit(10, 400, stress_ratio = 0) / r_minus1, 0.5^(0.226 + 0.04))
  expect_equal(murakami_fatigue_limit(10, 400, stress_ratio = -1), r_minus1)
  expect_lt(murakami_fatigue_limit(10, 400, stress_ratio = 0.5), murakami_fatigue_limit(10, 400, stress_ratio = 0))
})

test_that("bad input is refused", {
  expect_error(murakami_fatigue_limit(10, 0), "Hardness")
  expect_error(murakami_fatigue_limit(10, NA), "Hardness")
  expect_error(murakami_fatigue_limit(0, 300), "sqrt\\(area\\)")
  expect_error(murakami_fatigue_limit(10, 300, stress_ratio = 1), "stress ratio")
  expect_error(murakami_fatigue_limit(10, 300, "core"))
})

test_that("notes appear only outside the range the model was checked for", {
  expect_length(murakami_validity_notes(15, 300), 0)
  expect_match(murakami_validity_notes(15, 60), "HV = 60")
  expect_match(murakami_validity_notes(15, 800), "HV = 800")
  expect_match(murakami_validity_notes(1500, 300), "1000 um")
  expect_length(murakami_validity_notes(1500, 800), 2)
})

test_that("an EVS prediction becomes a fatigue limit with turned-around intervals", {
  pred <- list(predicted = 10, ci_lower = 8, ci_upper = 12, lower = 6, upper = 20, level = 0.95, return_period = 380)
  tab <- evs_fatigue_limit(pred, hv = 400, location = "internal")
  expect_equal(nrow(tab), 3)
  expect_match(tab$quantity[1], "T = 380")
  expect_equal(tab$fatigue_limit_mpa[1], murakami_fatigue_limit(10, 400, "internal"))
  # the largest inclusion gives the lowest fatigue limit
  expect_equal(tab$fatigue_lower[2], murakami_fatigue_limit(12, 400, "internal"))
  expect_equal(tab$fatigue_upper[2], murakami_fatigue_limit(8, 400, "internal"))
  expect_equal(tab$fatigue_lower[3], murakami_fatigue_limit(20, 400, "internal"))
  expect_equal(tab$fatigue_upper[3], murakami_fatigue_limit(6, 400, "internal"))
  expect_true(all(tab$fatigue_lower[2:3] < tab$fatigue_limit_mpa[1]))
  expect_true(all(tab$fatigue_upper[2:3] > tab$fatigue_limit_mpa[1]))
  expect_equal(c(tab$sqrt_area_lower[3], tab$sqrt_area_upper[3]), c(6, 20))
})

test_that("a size interval that starts at zero does not break the table", {
  pred <- list(predicted = 5, ci_lower = 3, ci_upper = 7, lower = 0, upper = 15, level = 0.95, return_period = 100)
  tab <- evs_fatigue_limit(pred, hv = 300)
  expect_true(all(is.finite(tab$fatigue_lower[2:3])))
  expect_true(tab$fatigue_upper[3] > tab$fatigue_limit_mpa[1])
})

test_that("it works on a real EVS fit", {
  set.seed(3)
  maxima <- 3 - 1.5 * log(-log(stats::runif(40)))
  fit <- fit_evs_gumbel(maxima[maxima > 0])
  pred <- predict_evs_max(fit, 380, n_boot = 300)
  tab <- evs_fatigue_limit(pred, hv = 180)
  expect_equal(tab$fatigue_limit_mpa[1], 1.43 * 300 / pred$predicted^(1 / 6))
  expect_lt(tab$fatigue_lower[3], tab$fatigue_lower[2])
})

test_that("the return period for a stressed volume follows T = V / (S0 * h)", {
  r <- evs_volume_return_period(volume_mm3 = 1000, control_area_mm2 = 0.05, mean_sqrt_area_max_um = 4)
  expect_equal(r$control_volume_mm3, 0.05 * 0.004)
  expect_equal(r$return_period, 1000 / (0.05 * 0.004))
  expect_equal(r$thickness_um, 4)
  expect_error(evs_volume_return_period(0, 0.05, 4), "greater than zero")
  expect_error(evs_volume_return_period(1000, NA, 4), "greater than zero")
})
