# Tests for R/stereology_saltykov.R and R/stereology_mass_balance.R.
#
# Sections of spheres with a known answer: a sphere of diameter D cut at a
# random height shows a circle of diameter D * sqrt(1 - u^2), u uniform on
# 0..1, and N_V spheres per mm3 give N_A = N_V * D sections per mm2. Taking
# u at evenly spaced values instead of at random makes the tests exact up
# to the spacing.
sphere_sections <- function(diameter_um, per_mm3, area_mm2) {
  n <- round(per_mm3 * diameter_um / 1000 * area_mm2)
  diameter_um * sqrt(1 - ((seq_len(n) - 0.5) / n)^2)
}

test_that("Fullman gives back the number and size of equal spheres", {
  d <- sphere_sections(10, per_mm3 = 5e5, area_mm2 = 4)   # 20000 sections
  f <- fullman_number_density(d, 4)
  expect_equal(f$n, 20000)
  expect_equal(f$per_mm2, 5000)
  expect_equal(f$per_mm3, 5e5, tolerance = 0.01)
  expect_equal(f$mean_sphere_um, 10, tolerance = 0.01)
  # the mean section of a sphere is pi/4 of its diameter
  expect_equal(f$mean_section_um, 10 * pi / 4, tolerance = 0.001)
  # Delesse: area fraction = volume fraction = N_V * pi/6 * D^3
  expect_equal(f$area_fraction, 5e5 * pi / 6 * 0.01^3, tolerance = 0.001)
  expect_equal(f$volume_ppm, f$area_fraction * 1e6)
})

test_that("Fullman ignores invalid diameters and copes with none", {
  expect_equal(fullman_number_density(c(2, NA, -1, 0, 2), 1)$n, 2)
  empty <- fullman_number_density(numeric(0), 1)
  expect_equal(c(empty$n, empty$per_mm3, empty$area_fraction), c(0, 0, 0))
  expect_error(fullman_number_density(1, 0), "greater than zero")
})

test_that("each sphere class spreads its sections over the classes below it, summing to its diameter", {
  m <- vidternary:::.saltykov_matrix(6)
  expect_equal(colSums(m), 1:6)
  expect_true(all(m[lower.tri(m)] == 0))
  expect_true(all(diag(m) > 0))
})

test_that("Saltykov puts equal spheres into the top class and nothing elsewhere", {
  d <- sphere_sections(10, per_mm3 = 5e5, area_mm2 = 4)
  u <- saltykov_unfold(d, 4, n_classes = 10, d_max = 10)
  expect_equal(nrow(u), 10)
  expect_equal(u$d_upper, 1:10)
  expect_equal(sum(u$n_sections), 20000)
  expect_equal(u$per_mm3[10], 5e5, tolerance = 0.01)
  expect_true(all(abs(u$per_mm3[1:9]) < 0.01 * 5e5))
})

test_that("Saltykov separates two sphere sizes", {
  d <- c(sphere_sections(10, per_mm3 = 2e5, area_mm2 = 10), sphere_sections(5, per_mm3 = 8e5, area_mm2 = 10))
  u <- saltykov_unfold(d, 10, n_classes = 10, d_max = 10)
  expect_equal(u$per_mm3[10], 2e5, tolerance = 0.01)
  expect_equal(u$per_mm3[5], 8e5, tolerance = 0.01)
  expect_true(all(abs(u$per_mm3[-c(5, 10)]) < 0.01 * 8e5))
  # the total agrees with Fullman's estimate from the same sections
  expect_equal(sum(u$per_mm3), fullman_number_density(d, 10)$per_mm3, tolerance = 0.02)
})

test_that("a negative class is reported as found and clipped in the second column", {
  # sections that no set of spheres can produce: only small ones in the top
  # class' shadow are missing, so a lower class goes negative
  u <- saltykov_unfold(c(rep(9.9, 200), rep(5.5, 1)), 1, n_classes = 10, d_max = 10)
  expect_true(any(u$per_mm3 < 0))
  expect_true(all(u$per_mm3_clipped >= 0))
  expect_equal(u$per_mm3_clipped[u$per_mm3 >= 0], u$per_mm3[u$per_mm3 >= 0])
})

test_that("Saltykov defaults to the largest section and refuses bad input", {
  u <- saltykov_unfold(c(1, 2, 4), 1, n_classes = 4)
  expect_equal(max(u$d_upper), 4)
  expect_equal(u$n_sections, c(1, 1, 0, 1))
  expect_error(saltykov_unfold(numeric(0), 1), "No valid")
  expect_error(saltykov_unfold(1:3, 1, n_classes = 1), "At least 2")
  expect_error(saltykov_unfold(1:3, 0), "greater than zero")
  expect_s3_class(create_saltykov_plot(u), "ggplot")
})

test_that("the phase table has the formula mass fractions", {
  ph <- inclusion_phase_table()
  w <- inclusion_atomic_weights()
  expect_equal(ph$family, c("sulfide", "aluminate", "silicate", "Ti-rich"))
  expect_equal(ph$compound, c("MnS", "Al2O3", "SiO2", "TiN"))
  expect_equal(ph$anion, c("S", "O", "O", "N"))
  expect_equal(ph$anion_fraction[2], 3 * w[["O"]] / (2 * w[["Al"]] + 3 * w[["O"]]))
  expect_equal(ph$anion_fraction[1], w[["S"]] / (w[["Mn"]] + w[["S"]]))
  expect_equal(ph$anion_fraction + ph$cation_fraction, rep(1, 4))
})

test_that("the mass balance turns a volume fraction into bound oxygen", {
  # 100 ppm by volume of Al2O3 (3.99 g/cm3) in steel of 7.9 g/cm3
  w <- inclusion_atomic_weights()
  mb <- inclusion_mass_balance(c(aluminate = 100), steel_density = 7.9)
  mass_ppm <- 1e6 * 1e-4 * 3.99 / (1e-4 * 3.99 + (1 - 1e-4) * 7.9)
  o_fraction <- 3 * w[["O"]] / (2 * w[["Al"]] + 3 * w[["O"]])
  expect_equal(mb$phases$mass_ppm, mass_ppm)
  expect_equal(mb$phases$anion_ppm, mass_ppm * o_fraction)
  expect_equal(mb$elements$element, c("O", "Al"))
  expect_equal(mb$elements$bound_ppm, mass_ppm * c(o_fraction, 1 - o_fraction))
  expect_true(all(is.na(mb$elements$measured_ppm)))
})

test_that("oxygen from aluminates and silicates is added up and compared with the bulk analysis", {
  mb <- inclusion_mass_balance(c(aluminate = 100, silicate = 50, sulfide = 20, `Ti-rich` = 10),
                               measured = c(S = 10))
  o_bound <- sum(mb$phases$anion_ppm[mb$phases$anion == "O"])
  expect_equal(mb$elements$bound_ppm[mb$elements$element == "O"], o_bound)
  s <- mb$elements[mb$elements$element == "S", ]
  expect_equal(s$measured_ppm, 10)
  expect_equal(s$bound_share, s$bound_ppm / 10)
  expect_true(is.na(mb$elements$measured_ppm[mb$elements$element == "N"]))
  expect_equal(mb$elements$element[1:3], c("N", "O", "S"))
})

test_that("families without a phase are skipped and named", {
  mb <- inclusion_mass_balance(c(aluminate = 100, unclassified = 500))
  expect_equal(mb$phases$family, "aluminate")
  expect_equal(attr(mb, "skipped"), "unclassified")
  none <- inclusion_mass_balance(c(unclassified = 500))
  expect_equal(nrow(none$phases), 0)
  expect_equal(nrow(none$elements), 0)
  expect_error(inclusion_mass_balance(c(aluminate = 1), steel_density = 0), "greater than zero")
})
