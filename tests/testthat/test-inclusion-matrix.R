# Matrix removal.

steel_wt <- function(...) {
  d <- data.frame(...)
  d
}

test_that("matrix_only removes the matrix element and rescales to 100", {
  wt <- steel_wt(Fe = c(49, 10), Cr = c(14, 2), Al = c(20, 60), O = c(17, 28))
  r <- remove_matrix(wt, "Fe", "matrix_only", ignore_elements = NULL)
  expect_equal(unlist(r$wt[1, ]), c(Fe = 0, Cr = 14, Al = 20, O = 17) / 51 * 100)
  expect_equal(rowSums(r$wt), c(100, 100))
})

test_that("matrix_and_alloys subtracts the steel's share of each alloying element", {
  # Inclusion Al 20 + O 10, plus steel 70 units of Fe 70 / Cr 20 / Ni 10.
  wt <- steel_wt(Fe = 49, Cr = 14, Ni = 7, Al = 20, O = 10)
  r <- remove_matrix(wt, "Fe", "matrix_and_alloys", alloy_elements = c("Cr", "Ni"),
                     steel_composition = c(Fe = 70, Cr = 20, Ni = 10), ignore_elements = NULL)
  expect_equal(unlist(r$wt[1, ]), c(Fe = 0, Cr = 0, Ni = 0, Al = 20 / 30 * 100, O = 10 / 30 * 100),
               tolerance = 1e-10)
  expect_equal(r$ratios, c(Cr = 20 / 70, Ni = 10 / 70))
  expect_equal(r$residual_pct, 30)
})

test_that("alloy correction never goes below zero and clipped values are counted", {
  wt <- steel_wt(Fe = c(50, 50), Cr = c(5, 20), Al = c(10, 10))
  r <- remove_matrix(wt, "Fe", "matrix_and_alloys", alloy_elements = "Cr",
                     steel_composition = c(Fe = 70, Cr = 20), ignore_elements = NULL)
  expect_true(all(r$wt >= 0))
  expect_equal(r$clipped_by_element[["Cr"]], 1L)
  expect_equal(r$wt$Cr[1], 0)
})

test_that("the steel composition is estimated from the most matrix-rich particles when not given", {
  set.seed(1)
  steel <- steel_wt(Fe = rep(70, 12), Cr = rep(20, 12), Ni = rep(10, 12), Al = 0)
  incl <- steel_wt(Fe = rep(20, 30), Cr = rep(5.7, 30), Ni = rep(2.9, 30), Al = 71.4)
  est <- estimate_matrix_ratios(rbind(steel, incl), "Fe", c("Cr", "Ni"), top_fraction = 0.1)
  expect_equal(est, c(Cr = 20 / 70, Ni = 10 / 70), tolerance = 1e-10)
  r <- remove_matrix(rbind(steel, incl), "Fe", "matrix_and_alloys", alloy_elements = c("Cr", "Ni"), ignore_elements = NULL)
  expect_equal(r$ratios, c(Cr = 20 / 70, Ni = 10 / 70), tolerance = 1e-10)
})

test_that("particles that are mostly matrix or carry no inclusion signal are flagged", {
  wt <- steel_wt(Fe = c(90, 70, 30), Cr = c(5, 20, 5), Ni = c(0, 10, 2), Al = c(5, 0, 60), O = c(0, 0, 3))
  r <- remove_matrix(wt, "Fe", "matrix_and_alloys", alloy_elements = c("Cr", "Ni"),
                     steel_composition = c(Fe = 70, Cr = 20, Ni = 10), ignore_elements = NULL,
                     max_matrix_pct = 80, min_residual_pct = 5)
  expect_equal(r$matrix_particle, c(TRUE, FALSE, FALSE))
  expect_equal(r$no_signal, c(FALSE, TRUE, FALSE))
  expect_true(all(is.na(r$wt[2, ])))
})

test_that("ignored elements such as carbon are dropped before rescaling and NA counts as zero", {
  wt <- steel_wt(Fe = c(10, NA), C = c(20, 30), Al = c(35, 35), O = c(35, 35))
  r <- remove_matrix(wt, "Fe", "matrix_only", ignore_elements = "C")
  expect_equal(r$wt$C, c(0, 0))
  expect_equal(r$wt$Al, c(50, 50))
})

test_that("no matrix element only drops the ignored elements; bad input gives clear errors", {
  wt <- steel_wt(Fe = 40, C = 10, Al = 50)
  r <- remove_matrix(wt, NULL, ignore_elements = "C")
  expect_equal(unlist(r$wt[1, ]), c(Fe = 40 / 90 * 100, C = 0, Al = 50 / 90 * 100))
  expect_false(r$matrix_particle)
  expect_error(remove_matrix(wt, "Zn"), "Matrix element Zn has no column")
  expect_error(remove_matrix(wt, "Fe", "matrix_and_alloys", alloy_elements = "Cr"), "without a column")
  expect_error(remove_matrix(wt, "Fe", "matrix_and_alloys", alloy_elements = "Al",
                             steel_composition = c(Fe = 70)), "must give")
})
