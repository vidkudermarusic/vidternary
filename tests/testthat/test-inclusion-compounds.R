# Element wt% -> compounds.

w <- inclusion_atomic_weights()

test_that("a stoichiometric spinel resolves to 28.3 % MgO and 71.7 % Al2O3, with calculated oxygen matching the measured one", {
  m_spinel <- w[["Mg"]] + 2 * w[["Al"]] + 4 * w[["O"]]
  wt <- data.frame(Mg = w[["Mg"]], Al = 2 * w[["Al"]], O = 4 * w[["O"]]) / m_spinel * 100
  r <- elements_to_compounds(wt)
  expect_equal(r$mass$MgO / r$total, 0.28331, tolerance = 1e-4)
  expect_equal(r$mass$Al2O3 / r$total, 0.71669, tolerance = 1e-4)
  expect_equal(r$total, 100 + 0, tolerance = 1e-9)
  expect_equal(r$o_calculated_pct, r$o_measured_pct, tolerance = 1e-9)
  expect_equal(r$other, 0)
})

test_that("sulfur goes to Ca first, then Mn, and leftover sulfur stays in `other`", {
  wt <- data.frame(Ca = 10, Mn = 5, S = 6)
  r <- elements_to_compounds(wt, s_order = c("Ca", "Mn"))
  s_mol <- 6 / w[["S"]]
  expect_equal(r$moles$CaS, s_mol)
  expect_equal(r$moles$MnS, 0)
  expect_equal(r$moles$CaO, 10 / w[["Ca"]] - s_mol)
  expect_equal(r$moles$MnO, 5 / w[["Mn"]])

  wt2 <- data.frame(Ca = 2, Mn = 5, S = 6)
  r2 <- elements_to_compounds(wt2, s_order = c("Ca", "Mn"))
  expect_equal(r2$moles$CaS, 2 / w[["Ca"]])
  expect_equal(r2$moles$MnS, 5 / w[["Mn"]])
  expect_equal(r2$moles$CaO, 0)
  expect_equal(r2$other, (6 / w[["S"]] - 2 / w[["Ca"]] - 5 / w[["Mn"]]) * w[["S"]])
})

test_that("sulfur order can be Mn only, and an undefined sulfide is an error", {
  r <- elements_to_compounds(data.frame(Ca = 10, Mn = 5, S = 6), s_order = "Mn")
  expect_equal(r$moles$MnS, 5 / w[["Mn"]])
  expect_equal(r$moles$CaS, 0)
  expect_equal(r$moles$CaO, 10 / w[["Ca"]])
  expect_error(elements_to_compounds(data.frame(Mg = 1, S = 1), s_order = "Mg"), "No compound MgS")
})

test_that("Ti is counted as TiN, TiO2, or by the measured nitrogen", {
  wt <- data.frame(Ti = 10, N = 3, Al = 5)
  ti_mol <- 10 / w[["Ti"]]
  r1 <- elements_to_compounds(wt, ti_as = "TiN")
  expect_equal(r1$moles$TiN, ti_mol); expect_equal(r1$moles$TiO2, 0); expect_equal(r1$moles$AlN, 0)
  r2 <- elements_to_compounds(wt, ti_as = "TiO2")
  expect_equal(r2$moles$TiO2, ti_mol); expect_equal(r2$moles$TiN, 0)
  r3 <- elements_to_compounds(wt, ti_as = "by_N")
  n_mol <- 3 / w[["N"]]
  expect_equal(r3$moles$TiN, ti_mol)
  expect_equal(r3$moles$AlN, n_mol - ti_mol)
  expect_equal(r3$moles$Al2O3, (5 / w[["Al"]] - (n_mol - ti_mol)) / 2)
  expect_equal(r3$other, 0, tolerance = 1e-12)
})

test_that("nitrogen is not used unless ti_as is by_N, and carbon and oxygen are never used", {
  r <- elements_to_compounds(data.frame(Al = 5, N = 4, C = 3, O = 6))
  expect_equal(r$other, 0)
  expect_equal(r$moles$AlN, 0)
  r_n <- elements_to_compounds(data.frame(Al = 5, N = 4), ti_as = "by_N", n_order = "Al")
  # more N than Al: all Al is taken by N
  expect_equal(r_n$moles$AlN, 5 / w[["Al"]])
})

test_that("elements that form no listed compound, and unremoved matrix, stay in `other`", {
  r <- elements_to_compounds(data.frame(Al = 10, Mo = 3, Fe = 4, C = 5))
  expect_equal(r$other, 7)
  expect_equal(r$total, r$mass$Al2O3 + 7)
})

test_that("a row with NA gives NA, and a column without an atomic weight is an error", {
  r <- elements_to_compounds(data.frame(Al = c(10, NA), Si = c(5, 5)))
  expect_false(is.na(r$total[1]))
  expect_true(is.na(r$total[2]))
  expect_error(elements_to_compounds(data.frame(Al = 1, Xx = 1)), "No atomic weight for: Xx")
})

test_that("corner_compounds reads single compounds, sums and groups", {
  expect_equal(corner_compounds("Al2O3"), "Al2O3")
  expect_equal(corner_compounds("CaO+MgO"), c("CaO", "MgO"))
  expect_true(all(c("Al2O3", "SiO2", "CaO") %in% corner_compounds("@oxides")))
  expect_equal(corner_compounds("@sulfides"), c("MnS", "CaS"))
  expect_error(corner_compounds("FeO"), "Unknown compound")
})
