# Tests for R/cleanliness_family.R: chemical family from the EDS analysis.

family_of <- function(...) classify_inclusion_family(data.frame(...))$family

test_that("each family is recognised from its marker element", {
  expect_equal(family_of(S = 30, Mn = 50, Fe = 20), "sulfide")
  expect_equal(family_of(Ti = 40, Fe = 60), "Ti-rich")
  expect_equal(family_of(Al = 30, Fe = 70), "aluminate")
  expect_equal(family_of(Si = 25, Al = 5, Fe = 70), "silicate")
  expect_equal(family_of(Ca = 20, Mg = 10, Fe = 70), "aluminate")
})

test_that("families are compared as compounds, not as raw wt%", {
  # 10 wt% S stands for 27 wt% MnS, 15 wt% Ti for 19 wt% TiN: sulfide wins
  # although the particle holds more Ti than S.
  r <- classify_inclusion_family(data.frame(S = 10, Ti = 15, Fe = 75))
  expect_equal(r$family, "sulfide")
  w <- inclusion_atomic_weights()
  s_eq <- 10 * (w[["Mn"]] + w[["S"]]) / w[["S"]]
  ti_eq <- 15 * (w[["Ti"]] + w[["N"]]) / w[["Ti"]]
  expect_equal(r$signal_pct, s_eq + ti_eq)
  expect_equal(r$family_share, s_eq / (s_eq + ti_eq))
})

test_that("a particle that is mostly matrix is unclassified", {
  expect_equal(family_of(S = 0.5, Ti = 0.5, Fe = 99), "unclassified")
  # ... unless the threshold is lowered
  expect_equal(classify_inclusion_family(data.frame(S = 0.5, Ti = 0.5, Fe = 99), min_signal_pct = 1)$family, "sulfide")
})

test_that("a missing reading gives 'unclassified', not a family from the remaining elements", {
  r <- classify_inclusion_family(data.frame(S = c(30, NA), Ti = c(0, 40), Fe = c(70, 60)))
  expect_equal(r$family, c("sulfide", "unclassified"))
  expect_true(is.na(r$family_share[2]))
})

test_that("N, O and C never decide the family", {
  expect_equal(family_of(N = 30, O = 30, C = 30, Ti = 6, Fe = 4), "Ti-rich")
})

test_that("wt% column names of an EDS export are found", {
  d <- rbind(incl_row(list(Al2O3 = 70), steel = 30), incl_row(list(MnS = 70), steel = 30),
             incl_row(list(SiO2 = 70), steel = 30), incl_row(list(TiN = 70), steel = 30))
  expect_equal(classify_inclusion_family(d)$family, c("aluminate", "sulfide", "silicate", "Ti-rich"))
})

test_that("one row and zero rows work, and data without marker elements is an error", {
  expect_equal(nrow(classify_inclusion_family(data.frame(S = 30, Fe = 70))), 1)
  expect_equal(nrow(classify_inclusion_family(data.frame(S = numeric(0), Fe = numeric(0)))), 0)
  expect_error(classify_inclusion_family(data.frame(Fe = 70, Cr = 20)), "marker elements")
})
