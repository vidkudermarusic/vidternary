# Tests for R/cleanliness_rating_objects.R: particles -> rated objects with
# a type (A, B, C, D, DS) and a width series.

# A particle list with one row per call; rows are 500 um apart in Y unless
# `y` is given, so nothing is joined by accident.
rating_particles <- function(...) {
  rows <- list(...)
  d <- do.call(rbind, lapply(seq_along(rows), function(i) {
    r <- rows[[i]]
    data.frame(x = r$x %||% 0, y = r$y %||% (i * 500), length = r$length, breadth = r$breadth,
               area = r$area %||% (pi / 4 * r$length * r$breadth), family = r$family,
               direction = r$direction %||% 0, stringsAsFactors = FALSE)
  }))
  d
}
type_of <- function(...) {
  built <- build_rating_objects(rating_particles(...))
  built$objects[, c("type", "series")]
}

test_that("an elongated sulfide is type A, a compact one type D", {
  expect_equal(type_of(list(length = 60, breadth = 3, family = "sulfide"))$type, "A")
  expect_equal(type_of(list(length = 5, breadth = 4, family = "sulfide"))$type, "D")
})

test_that("an elongated oxide is type C whatever its family, a compact one type D", {
  for (fam in c("silicate", "aluminate", "Ti-rich", "unclassified")) {
    expect_equal(type_of(list(length = 30, breadth = 5, family = fam))$type, "C", info = fam)
    expect_equal(type_of(list(length = 6, breadth = 5, family = fam))$type, "D", info = fam)
  }
})

test_that("the elongation limit is the standard's: 3 for ISO 4967, 2 for ASTM E45", {
  p <- rating_particles(list(length = 10, breadth = 4, family = "silicate"))  # aspect 2.5
  expect_equal(build_rating_objects(p, cleanliness_rating_standard("ISO 4967"))$objects$type, "D")
  expect_equal(build_rating_objects(p, cleanliness_rating_standard("ASTM E45"))$objects$type, "C")
})

test_that("three compact particles in line are one type B stringer", {
  built <- build_rating_objects(rating_particles(
    list(x = 100, y = 0, length = 4, breadth = 3, family = "aluminate"),
    list(x = 130, y = 0, length = 4, breadth = 3, family = "aluminate"),
    list(x = 160, y = 0, length = 4, breadth = 3, family = "aluminate")))
  expect_equal(nrow(built$objects), 1)
  expect_equal(built$object, c(1, 1, 1))
  expect_equal(built$objects$type, "B")
  expect_equal(built$objects$series, "fine")
  expect_equal(built$objects$length, 64)
  expect_equal(built$objects$n_members, 3)
})

test_that("a stringer of sulfides is type A", {
  built <- build_rating_objects(rating_particles(
    list(x = 100, y = 0, length = 4, breadth = 3, family = "sulfide"),
    list(x = 130, y = 0, length = 4, breadth = 3, family = "sulfide"),
    list(x = 160, y = 0, length = 4, breadth = 3, family = "sulfide")))
  expect_equal(built$objects$type, "A")
})

test_that("particles of different families are never joined", {
  built <- build_rating_objects(rating_particles(
    list(x = 100, y = 0, length = 4, breadth = 3, family = "aluminate"),
    list(x = 130, y = 0, length = 4, breadth = 3, family = "Ti-rich"),
    list(x = 160, y = 0, length = 4, breadth = 3, family = "aluminate")))
  expect_equal(nrow(built$objects), 3)
  expect_true(all(built$objects$type == "D"))
})

test_that("a close pair of compact particles stays two type D particles", {
  built <- build_rating_objects(rating_particles(
    list(x = 100, y = 0, length = 5, breadth = 4, family = "aluminate"),
    list(x = 130, y = 0, length = 5, breadth = 4, family = "aluminate")))
  expect_equal(nrow(built$objects), 2)
  expect_equal(built$objects$type, c("D", "D"))
  expect_equal(built$objects$n_members, c(1, 1))
  expect_equal(length(unique(built$object)), 2)
  expect_equal(built$objects$length, c(5, 5))
})

test_that("a close pair of elongated particles is rated as one inclusion", {
  built <- build_rating_objects(rating_particles(
    list(x = 100, y = 0, length = 20, breadth = 3, family = "sulfide"),
    list(x = 140, y = 0, length = 20, breadth = 3, family = "sulfide")))
  expect_equal(nrow(built$objects), 1)
  expect_equal(built$objects$type, "A")
  expect_equal(built$objects$length, 60)   # 20 + gap 20 + 20
})

test_that("round particles are sorted by diameter into D fine, D thick and DS", {
  round_particle <- function(d) list(length = d, breadth = d, family = "Ti-rich")
  r <- type_of(round_particle(2.1), round_particle(7.9), round_particle(8.5), round_particle(12.9),
               round_particle(13.1), round_particle(40))
  expect_equal(r$type, c("D", "D", "D", "D", "DS", "DS"))
  expect_equal(r$series, c("fine", "fine", "thick", "thick", "single", "single"))
})

test_that("the width series follow Table 2, and wider inclusions are flagged oversize", {
  sulfide <- function(w) list(length = 20 * w, breadth = w, family = "sulfide")
  built <- build_rating_objects(rating_particles(sulfide(2), sulfide(4), sulfide(4.1), sulfide(12), sulfide(12.5)))
  expect_equal(built$objects$series, c("fine", "fine", "thick", "thick", "thick"))
  expect_equal(built$objects$oversize, c(FALSE, FALSE, FALSE, FALSE, TRUE))
  expect_true(all(built$objects$rated))
})

test_that("particles narrower than the minimum width are left out", {
  built <- build_rating_objects(rating_particles(
    list(length = 3, breadth = 1.9, family = "aluminate"),
    list(length = 6, breadth = 5, family = "aluminate")))
  expect_equal(built$object, c(NA, 1))
  expect_equal(nrow(built$objects), 1)
  # with a lower floor the particle is an object, but below the fine series
  low <- build_rating_objects(rating_particles(list(length = 1.6, breadth = 1.5, family = "aluminate")), min_width = 1)
  expect_equal(low$objects$series, "below minimum width")
  expect_false(low$objects$rated)
})

test_that("an empty particle list gives an empty object table with all columns", {
  p <- rating_particles(list(length = 1, breadth = 0.5, family = "aluminate"))
  built <- build_rating_objects(p)
  expect_equal(nrow(built$objects), 0)
  expect_true(all(c("type", "series", "rated", "oversize", "family", "length", "width", "diameter") %in% names(built$objects)))
  expect_true(is.na(built$object))
})
