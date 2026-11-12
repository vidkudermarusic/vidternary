# Tests for R/cleanliness_rating_tables.R: the numbers of the standards.
# The expected values below are Table 1 and Table 2 of ISO 4967:2013.

iso_table_1 <- list(
  A = c(37, 127, 261, 436, 649, 898),
  B = c(17, 77, 184, 343, 555, 822),
  C = c(18, 76, 176, 320, 510, 746),
  D = c(1, 4, 9, 16, 25, 36),
  DS = c(13, 19, 27, 38, 53, 76)
)
steps <- seq(0.5, 3, by = 0.5)

test_that("a value at a table minimum gets that chart index, a value just below the one before", {
  for (type in names(iso_table_1)) {
    expect_equal(cleanliness_rating_index(type, iso_table_1[[type]]), steps, info = type)
    expect_equal(cleanliness_rating_index(type, iso_table_1[[type]] - 0.01), steps - 0.5, info = type)
  }
})

test_that("values beyond chart picture 3 are reported as 3.5, nothing as 0", {
  upper <- c(A = 1181, B = 1147, C = 1029, D = 49, DS = 107)
  for (type in names(upper)) {
    expect_equal(cleanliness_rating_index(type, upper[[type]] - 0.01), 3, info = type)
    expect_equal(cleanliness_rating_index(type, c(upper[[type]], 1e6)), c(3.5, 3.5), info = type)
    expect_equal(cleanliness_rating_index(type, 0), 0, info = type)
  }
})

test_that("the continuous index agrees with the table at every chart step", {
  for (type in names(iso_table_1)) {
    exact <- cleanliness_rating_index(type, iso_table_1[[type]], exact = TRUE)
    expect_true(all(abs(exact - steps) < 0.04), info = type)
  }
  expect_equal(cleanliness_rating_index("A", c(0, -5), exact = TRUE), c(0, 0))
  # it rises with the measured value
  expect_true(all(diff(cleanliness_rating_index("B", c(20, 50, 100, 400), exact = TRUE)) > 0))
})

test_that("the formulas give back the lengths of Table 1 to within 1 %", {
  back <- function(i, slope, intercept) 10^((log10(i) - intercept) / slope)
  expect_true(all(abs(back(steps, 0.5605, -1.179) / iso_table_1$A - 1) < 0.01))
  expect_true(all(abs(back(steps[-1], 0.4626, -0.871) / iso_table_1$B[-1] - 1) < 0.015))
  expect_true(all(abs(back(steps[-1], 0.4807, -0.904) / iso_table_1$C[-1] - 1) < 0.01))
})

test_that("the width limits are those of ISO 4967:2013 Table 2", {
  w <- cleanliness_rating_standard("ISO 4967")$widths
  expect_equal(w$type, c("A", "B", "C", "D"))
  expect_equal(w$fine_min, c(2, 2, 2, 2))
  expect_equal(w$fine_max, c(4, 9, 5, 8))
  expect_equal(w$thick_max, c(12, 15, 12, 13))
})

test_that("the two standards differ in stringer offset and elongation limit only", {
  iso <- cleanliness_rating_standard("ISO 4967")
  astm <- cleanliness_rating_standard("ASTM E45")
  expect_equal(c(iso$max_gap, iso$max_offset, iso$aspect_limit), c(40, 10, 3))
  expect_equal(c(astm$max_gap, astm$max_offset, astm$aspect_limit), c(40, 15, 2))
  expect_equal(iso$field_area_mm2, 0.5)
  expect_equal(iso$min_members, 3)
  expect_equal(iso$ds_min_diameter, 13)
  expect_equal(iso$minimums, astm$minimums)
  expect_equal(cleanliness_rating_standard()$name, "ISO 4967")
})

test_that("unknown standards and types are refused", {
  expect_error(cleanliness_rating_standard("DIN 50602"))
  expect_error(cleanliness_rating_index("E", 10), "Unknown inclusion type")
})
