# Pure helpers behind the inclusion panel.

test_that("a typed steel composition is read as Symbol=value pairs", {
  expect_equal(parse_steel_composition("Fe=70, Cr=20, Ni=10"), c(Fe = 70, Cr = 20, Ni = 10))
  expect_equal(parse_steel_composition("Fe = 70.5; Cr=18.25"), c(Fe = 70.5, Cr = 18.25))
  expect_equal(parse_steel_composition(" Fe=70 ,,Cr=.5 "), c(Fe = 70, Cr = 0.5))
})

test_that("a badly typed steel composition gives a clear message", {
  expect_error(parse_steel_composition(""), "Enter the steel composition")
  expect_error(parse_steel_composition("Fe=70,5"), "Cannot read '5'.*dot as the decimal separator")
  expect_error(parse_steel_composition("fe=70"), "Cannot read 'fe=70'")
  expect_error(parse_steel_composition("Fe:70"), "Cannot read")
  expect_error(parse_steel_composition("Fe=70, Fe=20"), "Element Fe is given twice")
  expect_error(parse_steel_composition("Xx=5"), "Unknown element: Xx")
  expect_error(parse_steel_composition("Fe=0"), "greater than zero")
  expect_error(parse_steel_composition("Fe=-3"), "Cannot read")
})

test_that("elements are detected from the wt% columns and the matrix element is the most abundant one", {
  d <- incl_data(incl_row(list(Al2O3 = 60), steel = 40), incl_row(steel = 100), incl_row(steel = 100))
  expect_true(all(c("Fe", "Cr", "Al", "O") %in% inclusion_detected_elements(d)))
  expect_false("Area" %in% inclusion_detected_elements(d))
  expect_equal(default_matrix_element(d), "Fe")
  expect_equal(inclusion_detected_elements(NULL), character(0))
  expect_equal(default_matrix_element(NULL), "")
  expect_equal(default_matrix_element(data.frame(id = 1:3)), "")
  expect_equal(default_matrix_element(data.frame(`Al.(Wt%)` = c(5, 6), `Si.(Wt%)` = c(3, 2), check.names = FALSE)), "")  # nothing reaches 10 wt%
})

test_that("preset choices are grouped by kind and hold the ids", {
  ch <- inclusion_preset_choices()
  expect_length(ch, 2)
  expect_equal(unname(ch[[1]]["CaO-Al2O3-MgO - Al-killed, Ca-treated steel"]), "cao-al2o3-mgo")
  expect_true("ti-al-n" %in% ch[[2]])
  expect_equal(length(unlist(ch)), 11)
})

test_that("the sulfur order key maps to elements, and an unknown key is an error", {
  expect_equal(inclusion_s_order("ca_mn"), c("Ca", "Mn"))
  expect_equal(inclusion_s_order("mn"), "Mn")
  expect_equal(inclusion_s_order("ca"), "Ca")
  expect_error(inclusion_s_order("x"), "Unknown sulfur order")
})

test_that("the export tables hold one row per input row with the corner fractions and the reason for dropping", {
  d <- incl_data(
    incl_row(list(MgO = 28.331, Al2O3 = 71.669), steel = 40),
    incl_row(steel = 100),
    incl_row(list(CaO = 48.53, Al2O3 = 51.47), steel = 20)
  )
  d$Feature <- c(101, 102, 103)
  d$`Area.(sq..µm)` <- c(5, 0.1, 2)
  r <- analyze_inclusion_preset(d, "cao-al2o3-mgo", matrix_element = "Fe", matrix_mode = "matrix_and_alloys",
                                steel_composition = INCL_STEEL)
  tb <- inclusion_result_tables(r, d)
  expect_named(tb, c("Particles", "Summary", "Settings", "Sources"))
  p <- tb$Particles
  expect_equal(nrow(p), 3)
  expect_equal(p$row, 1:3)
  expect_equal(p$Feature, c(101, 102, 103))
  expect_true(all(c("class", "CaO", "Al2O3", "MgO", "coverage_pct", "status") %in% names(p)))
  expect_true("Area.(sq..µm)" %in% names(p))
  expect_equal(p$status, c("plotted", "no inclusion signal after matrix removal", "plotted"))
  expect_equal(p$class, c("Spinel-type", NA, "C12A7"))
  expect_equal(p$Al2O3[1], 0.71669, tolerance = 1e-4)
  expect_equal(tb$Summary, r$summary)
  expect_true(all(startsWith(tb$Sources$url, "https://")))
  expect_gt(nrow(tb$Settings), 2)
})
