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

test_that("an area that needs an absurd number of rating fields is refused before the grid is built", {
  # 2 x 1 mm in um read as mm: 2000 x 1000 mm, 2829 x 1415 fields of 0.5 mm2
  expect_error(rating_field_grid(c(0, 2e6), c(0, 1e6), bbox = c(0, 2e6, 0, 1e6)),
               "span 2,000 x 1,000 mm, which would be cut into 4,003,035 rating fields of 0.5 mm2 (at most 20,000 are allowed)", fixed = TRUE)
  expect_error(rating_field_grid(c(0, 2e6), c(0, 1e6)), "Check the coordinate unit (um or mm) and the coordinate columns.", fixed = TRUE)
  # the limit is on the number of fields: a 100 x 100 mm specimen is exactly 20000 fields and is allowed
  side <- sqrt(0.5) * 1000
  g <- rating_field_grid(c(0, 1), c(0, 1), bbox = c(0, 200 * side, 0, 100 * side))
  expect_equal(nrow(g$fields), 20000)
  expect_error(rating_field_grid(c(0, 1), c(0, 1), bbox = c(0, 201 * side, 0, 100 * side)), "20,100 rating fields")
  # a smaller field area or a lower limit move it; Inf switches it off
  expect_error(rating_field_grid(c(0, 1), c(0, 1), bbox = c(0, 2000, 0, 1000), max_fields = 5), "6 rating fields of 0.5 mm2 (at most 5", fixed = TRUE)
  expect_equal(nrow(rating_field_grid(c(0, 1), c(0, 1), bbox = c(0, 2000, 0, 1000), max_fields = 6)$fields), 6)
  expect_equal(nrow(rating_field_grid(c(0, 1), c(0, 1), bbox = c(0, 201 * side, 0, 100 * side), max_fields = Inf)$fields), 20100)
  # the message names the unit the user chose
  expect_error(.check_rating_field_count(c(0, 2e6, 0, 1e6), 0.5, 20000, unit = "mm"), "set to mm: if the coordinates are in um, change it to um")
  expect_error(.check_rating_field_count(c(0, 2e6, 0, 1e6), 0.5, 20000, unit = "um"), "set to um: check that the right coordinate columns are chosen")
  expect_equal(.check_rating_field_count(c(0, 2000, 0, 1000), 0.5, 20000), 6)
})

test_that("no objects at all gives a table of zeros", {
  o <- rating_objects("A", "fine", 1, length = 10)[0, ]
  g <- rating_grid(o)
  s <- summarise_rating(rate_fields(o, g), g)
  expect_equal(nrow(s), 9)
  expect_true(all(s$worst_index == 0))
  expect_true(all(is.na(s$worst_field)))
})

# ---- worst field with a sliding window ----

# 30 compact 5 um aluminates on a 6 x 5 lattice (50 um apart, too far for
# stringers) centred on the corner where four fixed fields meet, plus two
# sub-micrometre specks that pin the inspected area to 2 x 1.414 mm.
corner_cluster_data <- function() {
  side_mm <- sqrt(0.5)
  speck <- function(x, y) clean_particle(x, y, 0.4, 0.3, list(TiN = 3), steel = 97)
  grid <- expand.grid(dx = c(-125, -75, -25, 25, 75, 125), dy = c(-110, -60, -10, 40, 90))
  rbind(speck(0, 0), speck(2, 1.414),
        do.call(rbind, Map(function(dx, dy) clean_particle(side_mm + dx / 1000, side_mm + dy / 1000, 5, 5, list(Al2O3 = 70)),
                           grid$dx, grid$dy)))
}

test_that("one window position is the fixed grid", {
  res <- run_cleanliness_analysis(make_cleanliness_data(), CLEAN_COLS, list(n_boot = 100))
  w <- worst_window_rating(res$objects, res$bbox, 0.5, steps = 1)
  s <- summarise_rating(res$field_ratings, res$grid)
  expect_equal(w$index, s$worst_index)
  expect_equal(w$value, s$worst_value)
  expect_equal(ifelse(w$value > 0, w$field, NA), ifelse(s$worst_value > 0, s$worst_field, NA))
  expect_false(any(w$shifted, na.rm = TRUE))
})

test_that("a cluster on the corner of four fields is found whole by the sliding window", {
  res <- run_cleanliness_analysis(corner_cluster_data(), CLEAN_COLS, list(n_boot = 100))
  d <- res$rating_summary[res$rating_summary$series == "D fine", ]
  # the fixed grid splits the 30 particles 9 / 9 / 6 / 6
  expect_equal(d$worst_value_grid, 9)
  expect_equal(d$worst_index_grid, 1.5)
  # a shifted window holds all of them
  expect_equal(d$worst_value, 30)
  expect_equal(d$worst_index, 2.5)
  expect_true(d$worst_shifted)
  expect_true(is.na(d$worst_field))
  expect_true(d$worst_field_complete)
  # method B is still the mean over the fixed fields
  expect_equal(d$mean_index, mean(res$field_ratings$index[res$field_ratings$series == "D fine"]))
  expect_match(res$notes$notes, "laid at 16 positions, shifted by 1/4 of its side", fixed = TRUE, all = FALSE)

  tab <- cleanliness_display_tables(res)$rating
  expect_match(tab$`Worst field`[tab$Series == "D fine"], "^window at x 0[.][0-9]{3}, y 0[.][0-9]{3} mm$")
  # the window's lower-left corner is a shifted grid position: a multiple of a quarter field from the area's corner
  q <- sqrt(0.5) / 4
  expect_lt(abs(d$worst_x_mm / q - round(d$worst_x_mm / q)), 1e-6)
  expect_lt(abs(d$worst_y_mm / q - round(d$worst_y_mm / q)), 1e-6)

  # switched off: the fixed grid, as before
  fixed <- run_cleanliness_analysis(corner_cluster_data(), CLEAN_COLS, list(n_boot = 100, worst_window_steps = 1))
  f <- fixed$rating_summary[fixed$rating_summary$series == "D fine", ]
  expect_equal(c(f$worst_index, f$worst_value), c(1.5, 9))
  expect_false(f$worst_shifted)
  expect_match(fixed$notes$notes, "on the fixed grid of rating fields only", fixed = TRUE, all = FALSE)
})

test_that("the sliding window never rates lower than the fixed grid, and empty series stay empty", {
  res <- run_cleanliness_analysis(make_cleanliness_data(), CLEAN_COLS, list(n_boot = 100))
  s <- res$rating_summary
  expect_true(all(s$worst_index >= s$worst_index_grid))
  expect_true(all(s$worst_value >= s$worst_value_grid))
  empty <- s[s$series == "C fine", ]
  expect_equal(c(empty$worst_index, empty$worst_value), c(0, 0))
  expect_true(is.na(empty$worst_field))
  # the helper data has no cluster on a border: the worst fields are the fixed ones
  expect_equal(s$worst_index, s$worst_index_grid)
  expect_equal(s$worst_field, s$worst_field_grid)
  expect_error(worst_window_rating(res$objects, res$bbox, steps = 0), "whole number of 1 or more")
  expect_error(worst_window_rating(res$objects, res$bbox, steps = 2.5), "whole number of 1 or more")
})

test_that("the per-family rating uses the sliding window too", {
  res <- run_cleanliness_analysis(corner_cluster_data(), CLEAN_COLS, list(n_boot = 100))
  f <- res$rating_by_family
  expect_equal(f$worst_index[f$family == "aluminate" & f$series == "D fine"], 2.5)
})
