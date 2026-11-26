# Tests for R/cleanliness_indices.R: number density, area fraction, size classes.

test_that("the exact Poisson interval matches known values", {
  ci <- poisson_count_ci(c(0, 1, 10))
  expect_equal(ci[, "lower"], c(0, 0.02532, 4.7954), tolerance = 1e-3)
  expect_equal(ci[, "upper"], c(3.6889, 5.5716, 18.3904), tolerance = 1e-4)
  # a wider level gives a wider interval
  expect_gt(diff(poisson_count_ci(10, 0.99)[1, ]), diff(poisson_count_ci(10, 0.95)[1, ]))
})

test_that("counts, densities and area fractions are worked out per group and in total", {
  ind <- cleanliness_indices(area_um2 = c(1, 4, 9), group = c("a", "a", "b"), inspected_area_mm2 = 2)
  expect_equal(ind$group, c("All", "a", "b"))
  expect_equal(ind$n, c(3, 2, 1))
  expect_equal(ind$per_mm2, c(1.5, 1, 0.5))
  # um2 of inclusion per mm2 inspected is the area fraction in ppm
  expect_equal(ind$area_ppm, c(7, 2.5, 4.5))
  expect_equal(ind$max_sqrt_area_um, c(3, 2, 3))
  expect_equal(ind$max_ecd_um, 2 * sqrt(c(9, 4, 9) / pi))
  expect_equal(ind$mean_ecd_um[2], mean(2 * sqrt(c(1, 4) / pi)))
  expect_equal(ind$per_mm2_lower[1], unname(poisson_count_ci(3)[1, "lower"]) / 2)
  expect_equal(ind$per_mm2_upper[1], unname(poisson_count_ci(3)[1, "upper"]) / 2)
  # no field IDs, no interval for the area fraction
  expect_true(all(is.na(ind$area_ppm_lower)))
})

test_that("with field IDs the area fraction gets a bootstrap interval around the estimate", {
  set.seed(7)
  field <- rep(1:40, each = 5)
  area <- stats::rexp(200, rate = 1)
  ind <- cleanliness_indices(area, rep("a", 200), inspected_area_mm2 = 4, field = field, n_boot = 500)
  expect_true(all(ind$area_ppm_lower < ind$area_ppm & ind$area_ppm < ind$area_ppm_upper))
  # the same seed gives the same interval, and the caller's random stream is untouched
  set.seed(1); before <- stats::runif(1)
  set.seed(1)
  again <- cleanliness_indices(area, rep("a", 200), inspected_area_mm2 = 4, field = field, n_boot = 500)
  expect_equal(stats::runif(1), before)
  expect_equal(again$area_ppm_lower, ind$area_ppm_lower)
})

test_that("empty fields widen the interval of the area fraction", {
  field <- rep(1:10, each = 3)
  area <- rep(c(1, 2, 3), 10)
  full <- cleanliness_indices(area, rep("a", 30), 1, field = field, n_boot = 800)
  sparse <- cleanliness_indices(area, rep("a", 30), 1, field = field, n_fields = 100, n_boot = 800)
  # every field holds the same area: no spread without empty fields
  expect_equal(full$area_ppm_lower[1], full$area_ppm_upper[1])
  expect_lt(sparse$area_ppm_lower[1], sparse$area_ppm[1])
  expect_gt(sparse$area_ppm_upper[1], sparse$area_ppm[1])
})

test_that("one large particle makes the area-fraction interval far wider than the count interval", {
  field <- 1:50
  area <- c(rep(1, 49), 400)
  ind <- cleanliness_indices(area, rep("a", 50), 1, field = field, n_boot = 1000)[1, ]
  count_width <- (ind$per_mm2_upper - ind$per_mm2_lower) / ind$per_mm2
  area_width <- (ind$area_ppm_upper - ind$area_ppm_lower) / ind$area_ppm
  expect_gt(area_width, 2 * count_width)
})

test_that("no particles and a bad area are handled", {
  ind <- cleanliness_indices(numeric(0), character(0), 1)
  expect_equal(ind$group, "All")
  expect_equal(ind$n, 0)
  expect_equal(ind$area_ppm, 0)
  expect_error(cleanliness_indices(1, "a", 0), "greater than zero")
})

test_that("size-class limits typed as text are read", {
  expect_equal(parse_size_breaks("1, 2, 5, 10, 20"), c(1, 2, 5, 10, 20))
  expect_equal(parse_size_breaks("5; 1 2,2"), c(1, 2, 5))
  expect_error(parse_size_breaks("1, two"), "positive numbers")
  expect_error(parse_size_breaks("0, 1"), "positive numbers")
  expect_error(parse_size_breaks(""), "positive numbers")
})

test_that("particles are counted into size classes, a limit belonging to the class above it", {
  tab <- size_class_table(c(0.5, 1, 1.5, 3, 25), c("a", "a", "b", "b", "b"), inspected_area_mm2 = 2, breaks = c(1, 2, 5))
  expect_equal(levels(tab$size_class), c("< 1", "1 - 2", "2 - 5", ">= 5"))
  all_rows <- tab[tab$group == "All", ]
  expect_equal(all_rows$n, c(1, 2, 1, 1))
  expect_equal(all_rows$per_mm2, c(0.5, 1, 0.5, 0.5))
  expect_equal(tab$n[tab$group == "a"], c(1, 1, 0, 0))
  expect_equal(tab$n[tab$group == "b"], c(0, 1, 1, 1))
  expect_equal(all_rows$per_mm2_upper[1], unname(poisson_count_ci(1)[1, "upper"]) / 2)
  # a single limit gives two classes
  expect_equal(levels(size_class_table(c(1, 3), c("a", "a"), 1, breaks = 2)$size_class), c("< 2", ">= 2"))
})
