# Tests for R/cleanliness_stringers.R: joining particles that lie in line.

test_that("three particles in line within the gap form one stringer, measured end to end", {
  s <- detect_stringers(x = c(0, 30, 60), y = c(0, 0, 0), length = rep(10, 3), breadth = c(3, 4, 2))
  expect_equal(s$object, c(1, 1, 1))
  expect_equal(nrow(s$objects), 1)
  expect_true(s$objects$is_stringer)
  expect_equal(s$objects$n_members, 3)
  expect_equal(s$objects$length, 70)          # from -5 to 65
  expect_equal(s$objects$width, 4)            # widest member
  expect_equal(c(s$objects$x, s$objects$y), c(30, 0))
  expect_equal(c(s$objects$x_start, s$objects$x_end), c(-5, 65))
})

test_that("the gap is measured between particle ends, not centres", {
  # centres 50 um apart, each 10 um long: gap 40 um -> joined; 41 um -> not
  expect_equal(nrow(detect_stringers(c(0, 50), c(0, 0), c(10, 10), c(2, 2))$objects), 1)
  expect_equal(nrow(detect_stringers(c(0, 51), c(0, 0), c(10, 10), c(2, 2))$objects), 2)
})

test_that("particles too far apart across the working direction are not joined", {
  x <- c(0, 30); len <- c(10, 10); br <- c(2, 2)
  expect_equal(nrow(detect_stringers(x, c(0, 10), len, br)$objects), 1)
  expect_equal(nrow(detect_stringers(x, c(0, 11), len, br)$objects), 2)
  # ASTM E45 allows 15 um
  expect_equal(nrow(detect_stringers(x, c(0, 11), len, br, max_offset = 15)$objects), 1)
})

test_that("a pair is one object but not a stringer", {
  s <- detect_stringers(c(0, 30), c(0, 0), c(10, 10), c(2, 2))
  expect_equal(s$objects$n_members, 2)
  expect_false(s$objects$is_stringer)
  expect_true(detect_stringers(c(0, 30), c(0, 0), c(10, 10), c(2, 2), min_members = 2)$objects$is_stringer)
})

test_that("the working direction can be turned", {
  # particles lined up along Y
  y <- c(0, 30, 60); x <- c(5, 5, 5); len <- rep(10, 3); br <- rep(2, 3)
  along_y <- detect_stringers(x, y, len, br, direction = rep(90, 3), axis_angle = 90)
  expect_equal(nrow(along_y$objects), 1)
  expect_equal(along_y$objects$length, 70)
  expect_equal(c(along_y$objects$x, along_y$objects$y), c(5, 30))
  expect_equal(c(along_y$objects$y_start, along_y$objects$y_end), c(-5, 65))
  # with the working direction along X they are 30 um apart across it
  expect_equal(nrow(detect_stringers(x, y, len, br, direction = rep(90, 3), axis_angle = 0)$objects), 3)
})

test_that("a particle lying across the working direction only spans its breadth", {
  # 30 x 2 um particles, centres 45 um apart
  along <- detect_stringers(c(0, 45), c(0, 0), c(30, 30), c(2, 2), direction = c(0, 0))
  across <- detect_stringers(c(0, 45), c(0, 0), c(30, 30), c(2, 2), direction = c(90, 90))
  expect_equal(nrow(along$objects), 1)      # gap 15 um
  expect_equal(along$objects$length, 75)
  expect_equal(nrow(across$objects), 2)     # gap 43 um
  expect_equal(across$objects$length, c(2, 2))
})

test_that("the result does not depend on the order of the rows", {
  x <- c(0, 30, 60, 500, 530, 1000); y <- c(0, 2, 4, 100, 100, 300)
  len <- rep(8, 6); br <- rep(2, 6)
  a <- detect_stringers(x, y, len, br)
  shuffle <- c(4, 1, 6, 3, 5, 2)
  b <- detect_stringers(x[shuffle], y[shuffle], len, br)
  same_object <- function(obj) outer(obj, obj, "==")
  expect_equal(same_object(b$object), same_object(a$object[shuffle]))
  expect_equal(sort(a$objects$n_members), c(1, 2, 3))
  expect_equal(sort(b$objects$n_members), c(1, 2, 3))
})

test_that("chains are followed through several links", {
  # 1-2 and 2-3 are close, 1-3 are not: all three belong together
  s <- detect_stringers(c(0, 40, 80), c(0, 8, 16), rep(10, 3), rep(2, 3))
  expect_equal(nrow(s$objects), 1)
})

test_that("empty and single inputs work, bad inputs are refused", {
  expect_equal(nrow(detect_stringers(numeric(0), numeric(0), numeric(0), numeric(0))$objects), 0)
  one <- detect_stringers(5, 7, 10, 2)
  expect_equal(one$object, 1)
  expect_equal(one$objects$length, 10)
  expect_error(detect_stringers(c(0, 1), 0, 1, 1), "same number")
  expect_error(detect_stringers(c(0, NA), c(0, 0), c(1, 1), c(1, 1)), "missing")
})
