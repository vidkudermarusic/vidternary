# Atomic weights, compound table and element-column detection.

test_that("compound mass factors match the standard values", {
  cmp <- inclusion_compounds()
  f <- setNames(cmp$factor, cmp$compound)
  expect_equal(f[["Al2O3"]], 1.8894, tolerance = 1e-4)
  expect_equal(f[["MgO"]], 1.6583, tolerance = 1e-4)
  expect_equal(f[["CaO"]], 1.3992, tolerance = 1e-4)
  expect_equal(f[["SiO2"]], 2.1393, tolerance = 1e-4)
  expect_equal(f[["MnO"]], 1.2912, tolerance = 1e-4)
  expect_equal(f[["MnS"]], 1.5836, tolerance = 1e-4)
  expect_equal(f[["CaS"]], 1.7999, tolerance = 1e-4)
  expect_equal(f[["TiN"]], 1.2926, tolerance = 1e-4)
  expect_equal(f[["AlN"]], 1.5191, tolerance = 1e-4)
  expect_equal(cmp$molar_mass[cmp$compound == "Al2O3"], 101.96, tolerance = 1e-4)
})

test_that("every compound's cation and anion have an atomic weight", {
  cmp <- inclusion_compounds()
  w <- inclusion_atomic_weights()
  expect_true(all(c(cmp$cation, cmp$anion) %in% names(w)))
  expect_false(anyNA(cmp$molar_mass))
})

test_that("element columns are found in an EDS export and other columns are not mistaken for them", {
  nm <- c("Feature", "Area", "Field", "Rank", "Area.(sq..µm)", "Aspect.Ratio", "Mean.grey",
          "Spectrum.Area", "Stage.X.(mm)", "ECD.(µm)", "Shape", "Beam.X.(pixels)",
          "C.(Wt%)", "Al.(Wt%)", "Si.(Wt%)", "S.(Wt%)", "Ca.(Wt%)", "Fe.(Wt%)", "Ti (wt%)")
  cols <- inclusion_element_columns(nm)
  expect_equal(names(cols), c("C", "Al", "Si", "S", "Ca", "Fe", "Ti"))
  expect_equal(unname(cols["Al"]), "Al.(Wt%)")
})

test_that("bare element symbols count as element columns, and a duplicate element is an error", {
  expect_equal(names(inclusion_element_columns(c("Al", "Si", "Mn", "id"))), c("Al", "Si", "Mn"))
  expect_error(inclusion_element_columns(c("Al", "Al.(Wt%)")), "More than one column matches element Al")
})
