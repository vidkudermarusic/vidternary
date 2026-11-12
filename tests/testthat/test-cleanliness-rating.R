# Tests for R/cleanliness_rating.R: rating fields, field ratings, summary.

test_that("a 2 x 1 mm area is cut into six 0.5 mm2 fields, two of them complete", {
  g <- rating_field_grid(c(10, 800, 10, 1999), c(10, 10, 800, 999), bbox = c(0, 2000, 0, 1000))
  expect_equal(c(g$nx, g$ny), c(3, 2))
  expect_equal(g$side, sqrt(0.5) * 1000)
  expect_equal(g$field, c(1, 2, 4, 6))
  expect_equal(g$fields$field, 1:6)
  expect_equal(g$fields$complete, c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))
  # the fields together cover exactly the inspected 2 mm2
  expect_equal(sum(g$fields$covered_fraction) * 0.5, 2)
})

test_that("an area that is a whole number of fields has no incomplete field", {
  side <- sqrt(0.5) * 1000
  g <- rating_field_grid(c(1, 2 * side - 1), c(1, side - 1), bbox = c(0, 2 * side, 0, side))
  expect_equal(c(g$nx, g$ny), c(2, 1))
  expect_true(all(g$fields$complete))
  expect_equal(g$field, c(1, 2))
})

test_that("points on the upper edge fall in the last field, points outside the area in none", {
  g <- rating_field_grid(c(2000, 2500, NA), c(1000, 500, 5), bbox = c(0, 2000, 0, 1000))
  expect_equal(g$field, c(6, NA, NA))
})

test_that("the field size can be changed and the area defaults to the range of the points", {
  g <- rating_field_grid(c(0, 1000), c(0, 1000), field_area_mm2 = 0.25)
  expect_equal(c(g$nx, g$ny), c(2, 2))
  expect_true(all(g$fields$complete))
  expect_error(rating_field_grid(1, 1, field_area_mm2 = 0), "greater than zero")
  expect_error(rating_field_grid(NA_real_, NA_real_, bbox = c(NA, NA, NA, NA)), "no valid coordinates")
})

# Hand-made objects in a 3 x 2 grid: `field` is where each one lies.
rating_objects <- function(type, series, field, length = NA, diameter = NA, oversize = FALSE, rated = TRUE) {
  data.frame(type = type, series = series, field = field, length = length, diameter = diameter,
             oversize = oversize, rated = rated, stringsAsFactors = FALSE)
}
rating_grid <- function(objects) {
  g <- rating_field_grid(numeric(0), numeric(0), bbox = c(0, 2000, 0, 1000))
  g$field <- objects$field
  g
}

test_that("lengths are summed, D particles counted and the largest DS taken, per field", {
  o <- rbind(
    rating_objects("A", "fine", 1, length = 20),
    rating_objects("A", "fine", 1, length = 20),
    rating_objects("A", "thick", 1, length = 30),
    rating_objects("D", "fine", 1, diameter = 5),
    rating_objects("D", "fine", 1, diameter = 5),
    rating_objects("D", "fine", 1, diameter = 6),
    rating_objects("D", "fine", 1, diameter = 7),
    rating_objects("DS", "single", 2, diameter = 20),
    rating_objects("DS", "single", 2, diameter = 30),
    rating_objects("B", "fine", 3, length = 400, oversize = TRUE),
    rating_objects("C", "fine", 3, length = 999, rated = FALSE)
  )
  g <- rating_grid(o)
  fr <- rate_fields(o, g)
  expect_equal(nrow(fr), 6 * 9)
  cell <- function(field, series) fr[fr$field == field & fr$series == series, ]

  expect_equal(cell(1, "A fine")$value, 40)
  expect_equal(cell(1, "A fine")$index, 0.5)      # 37 <= 40 < 127
  expect_equal(cell(1, "A fine")$n_objects, 2)
  expect_equal(cell(1, "A thick")$index, 0)       # 30 < 37
  expect_equal(cell(1, "D fine")$value, 4)
  expect_equal(cell(1, "D fine")$index, 1)
  expect_equal(cell(2, "DS")$value, 30)
  expect_equal(cell(2, "DS")$index, 1.5)          # 27 <= 30 < 38
  expect_equal(cell(3, "B fine")$index, 2)        # 343 <= 400 < 555
  expect_equal(cell(3, "B fine")$n_oversize, 1)
  expect_equal(cell(3, "C fine")$value, 0)        # the unrated object is ignored
  expect_equal(sum(fr$value[fr$field %in% 4:6]), 0)
  expect_true(all(fr$index_exact[fr$value == 0] == 0))

  s <- summarise_rating(fr, g)
  expect_equal(s$series, c("A fine", "A thick", "B fine", "B thick", "C fine", "C thick", "D fine", "D thick", "DS"))
  row <- function(series) s[s$series == series, ]
  expect_equal(row("B fine")$worst_index, 2)
  expect_equal(row("B fine")$worst_field, 3)
  expect_false(row("B fine")$worst_field_complete)
  expect_equal(row("B fine")$mean_index, 2 / 6)
  expect_equal(row("B fine")$fields_rated, 1)
  expect_equal(row("B fine")$n_fields, 6)
  expect_equal(row("D fine")$worst_value, 4)
  expect_equal(row("D fine")$unit, "particles")
  expect_true(row("D fine")$worst_field_complete)
  expect_equal(row("DS")$unit, "um diameter")
  # a series with a value but no index still names its field ...
  expect_equal(row("A thick")$worst_index, 0)
  expect_equal(row("A thick")$worst_field, 1)
  # ... and an empty series names none
  expect_true(is.na(row("C thick")$worst_field))
  expect_equal(row("C thick")$n_objects, 0)
})

test_that("among fields with the same index the larger value is the worst", {
  o <- rbind(rating_objects("A", "fine", 2, length = 40), rating_objects("A", "fine", 5, length = 100))
  g <- rating_grid(o)
  s <- summarise_rating(rate_fields(o, g), g)
  expect_equal(s$worst_field[s$series == "A fine"], 5)
  expect_equal(s$worst_value[s$series == "A fine"], 100)
})

test_that("no objects at all gives a table of zeros", {
  o <- rating_objects("A", "fine", 1, length = 10)[0, ]
  g <- rating_grid(o)
  s <- summarise_rating(rate_fields(o, g), g)
  expect_equal(nrow(s), 9)
  expect_true(all(s$worst_index == 0))
  expect_true(all(is.na(s$worst_field)))
})
