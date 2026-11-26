# Tests for R/eds_size_reliability.R: is a particle large enough for its
# EDS analysis to describe it?

test_that("the electron range follows the Kanaya-Okayama formula", {
  # iron at 15 kV: 0.0276 * 55.845 * 15^1.67 / (26^0.89 * 7.87)
  expect_equal(electron_range_um(15, "Fe"), 0.0276 * 55.845 * 15^1.67 / (26^0.89 * 7.87))
  expect_equal(electron_range_um(15, "Fe"), 0.99, tolerance = 0.01)      # about 1 um
  expect_equal(electron_range_um(), electron_range_um(15, "Fe"))
})

test_that("the range grows with the voltage and is larger in a light matrix", {
  r <- vapply(c(5, 10, 15, 20, 30), electron_range_um, numeric(1), matrix = "Fe")
  expect_true(all(diff(r) > 0))
  expect_equal(electron_range_um(30, "Fe") / electron_range_um(15, "Fe"), 2^1.67)
  expect_gt(electron_range_um(15, "Al"), 2 * electron_range_um(15, "Fe"))
  expect_gt(electron_range_um(15, "Ti"), electron_range_um(15, "Fe"))
  expect_lt(electron_range_um(15, "Ni"), electron_range_um(15, "Fe"))
})

test_that("bad voltage or an unknown matrix is refused", {
  expect_error(electron_range_um(0), "greater than zero")
  expect_error(electron_range_um(NA), "greater than zero")
  expect_error(electron_range_um(15, "Zn"), "No range data for matrix 'Zn'")
})

test_that("particles are sorted by their size relative to the range", {
  range <- electron_range_um(15, "Fe")
  r <- size_reliability(c(0.2, 0.99 * range, range, 1.99 * range, 2 * range, 10, NA), 15, "Fe")
  expect_equal(as.character(r$reliability),
               c("matrix-dominated", "matrix-dominated", "partly matrix", "partly matrix", "reliable", "reliable", NA))
  expect_equal(r$size_to_range[3], 1)
  expect_equal(r$size_to_range[1], 0.2 / range)
  expect_equal(attr(r, "range_um"), range)
  expect_equal(levels(r$reliability), c("matrix-dominated", "partly matrix", "reliable"))
})

test_that("a higher voltage makes the same particle less reliable, a wider caution band too", {
  expect_equal(as.character(size_reliability(1.5, 10)$reliability), "reliable")
  expect_equal(as.character(size_reliability(1.5, 15)$reliability), "partly matrix")
  expect_equal(as.character(size_reliability(1.5, 20)$reliability), "matrix-dominated")
  expect_equal(as.character(size_reliability(2.5, 15, caution_factor = 2)$reliability), "reliable")
  expect_equal(as.character(size_reliability(2.5, 15, caution_factor = 3)$reliability), "partly matrix")
  expect_error(size_reliability(1, caution_factor = 0.5), "1 or more")
})

test_that("the summary counts the classes and shows the matrix content of each", {
  rel <- c("matrix-dominated", "matrix-dominated", "matrix-dominated", "partly matrix", "reliable", "reliable")
  s <- reliability_summary(rel, matrix_pct = c(70, 60, 65, 40, 5, 15))
  expect_equal(s$reliability, c("matrix-dominated", "partly matrix", "reliable"))
  expect_equal(s$n, c(3, 1, 2))
  expect_equal(s$share_pct, 100 * c(3, 1, 2) / 6)
  expect_equal(s$median_matrix_pct, c(65, 40, 10))
  # without a matrix column, and with an empty class
  plain <- reliability_summary(c("reliable", "reliable"))
  expect_equal(plain$n, c(0, 0, 2))
  expect_true(all(is.na(plain$median_matrix_pct)))
  expect_equal(reliability_summary(character(0))$share_pct, c(0, 0, 0))
})

test_that("the two columns are added to a standard table, from the diameter or the area", {
  range <- electron_range_um(15, "Fe")
  from_ecd <- add_size_reliability(data.frame(`ECD.(um)` = c(0.5, 1.5, 5), check.names = FALSE))
  expect_equal(from_ecd$Size.to.range, c(0.5, 1.5, 5) / range)
  expect_equal(from_ecd$Composition.reliability, c("matrix-dominated", "partly matrix", "reliable"))
  from_area <- add_size_reliability(data.frame(`Area.(um2)` = pi / 4 * c(0.5, 1.5, 5)^2, check.names = FALSE))
  expect_equal(from_area$Size.to.range, from_ecd$Size.to.range)
  # other columns stay, and the new one works in the pre-analysis filter
  d <- add_size_reliability(data.frame(`ECD.(um)` = c(0.5, 1.5, 5), Keep = 1:3, check.names = FALSE), kv = 20)
  expect_equal(d$Keep, 1:3)
  expect_equal(nrow(apply_pre_filters(d, list(Size.to.range = "> 1"))), 1)       # only the 5 um particle at 20 kV
  expect_error(add_size_reliability(data.frame(Length = 1)), "neither an 'ECD.\\(um\\)' nor an 'Area.\\(um2\\)'")
})

test_that("adding the columns twice replaces them", {
  d <- add_size_reliability(data.frame(`ECD.(um)` = c(0.5, 5), check.names = FALSE))
  again <- add_size_reliability(d, kv = 30)
  expect_equal(sum(names(again) == "Size.to.range"), 1)
  expect_lt(again$Size.to.range[2], d$Size.to.range[2])
})
