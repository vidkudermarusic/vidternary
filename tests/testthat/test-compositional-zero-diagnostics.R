zero_data <- function(n = 400, seed = 3) {
  set.seed(seed)
  d <- data.frame(Fe = stats::runif(n, 50, 70), Cr = stats::runif(n, 10, 20), Ni = stats::runif(n, 5, 10),
                  Ca = stats::runif(n, 0, 5), Mg = stats::runif(n, 0, 3))
  d$Ca[sample(n, 0.5 * n)] <- 0
  d$Mg[sample(n, 0.3 * n)] <- 0
  d
}

test_that("coda_zero_summary gives zero share and the pseudo-count clr_transform uses", {
  d <- data.frame(a = c(0, 2, 4, 8), b = c(1, 1, 1, 1), c = c(NA, 3, 0, 6))
  s <- coda_zero_summary(d, c("a", "b", "c"))
  expect_equal(s$Zero_pct, c(25, 0, 50))
  expect_equal(s$Pseudo_count, c(1, 0.5, 1.5))
})

test_that("coda_default_parts keeps parts with few zeros and falls back to all when too few qualify", {
  d <- zero_data()
  expect_equal(coda_default_parts(d, names(d)), c("Fe", "Cr", "Ni"))
  expect_equal(coda_default_parts(d, names(d), max_zero_share = 0.6), names(d))
  expect_equal(coda_default_parts(d, c("Fe", "Ca", "Mg")), c("Fe", "Ca", "Mg"))
  expect_equal(coda_default_parts(d, character(0)), character(0))
})

test_that("the zero-pattern R-squared is high when zeros drive the PCA and low without zeros", {
  d <- zero_data()
  all_parts <- names(d)
  r2_all <- coda_zero_pattern_r2(d, all_parts, compositional_pca(clr_transform(d, all_parts))$scores)
  expect_length(r2_all, 3)
  expect_gt(r2_all[1], 0.5)

  few <- c("Fe", "Cr", "Ni")
  expect_equal(coda_zero_pattern_r2(d, few, compositional_pca(clr_transform(d, few))$scores), c(0, 0, 0))
})

test_that("coda_zero_notes names the zero-heavy parts and the R-squared, and is silent otherwise", {
  d <- zero_data()
  s <- coda_zero_summary(d, names(d))
  msgs <- coda_zero_notes(s, c(0.9, 0.8, 0.7))
  expect_length(msgs, 2)
  expect_match(msgs[1], "2 of the selected parts have more than 10% zeros: Ca (50%), Mg (30%)", fixed = TRUE)
  expect_match(msgs[2], "explains 90% of PC1 and 80% of PC2")
  expect_length(coda_zero_notes(coda_zero_summary(d, c("Fe", "Cr", "Ni")), c(0.1, 0.1, 0.1)), 0)
})
