# End to end: synthetic particles of known composition through a preset.

d <- incl_data(
  incl_row(list(MgO = 28.331, Al2O3 = 71.669), steel = 40, carbon = 2),   # 1 spinel in steel
  incl_row(list(Al2O3 = 100), steel = 40, carbon = 2),                    # 2 alumina in steel
  incl_row(list(CaO = 48.53, Al2O3 = 51.47), steel = 20),                 # 3 C12A7 in steel
  incl_row(steel = 100),                                                  # 4 steel only
  incl_row(list(Al2O3 = 5), steel = 100, steel_comp = c(Fe = 95, Cr = 5)),# 5 almost all matrix
  incl_row(list(MnS = 100), steel = 30, carbon = 1)                       # 6 MnS in steel
)
run <- function(preset = "cao-al2o3-mgo", ...) {
  analyze_inclusion_preset(d, preset, matrix_element = "Fe", matrix_mode = "matrix_and_alloys",
                           steel_composition = INCL_STEEL, ...)
}

test_that("particles in steel are recovered and classified after matrix removal", {
  r <- run()
  expect_s3_class(r, "inclusion_result")
  expect_equal(r$class, c("Spinel-type", "Alumina", "C12A7", NA, NA, NA))
  expect_equal(r$keep, c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_equal(unlist(r$coords[1, ]), c(A = 0, B = 0.71669, C = 0.28331), tolerance = 1e-4)
  expect_equal(unlist(r$coords[2, ]), c(A = 0, B = 1, C = 0), tolerance = 1e-9)
  expect_equal(unlist(r$coords[3, ]), c(A = 0.4853, B = 0.5147, C = 0), tolerance = 1e-4)
  expect_true(all(is.na(r$coords[!r$keep, ])))
  expect_equal(nrow(r$coords), nrow(d))
})

test_that("dropped particles say why", {
  r <- run()
  expect_equal(r$excluded, c(NA, NA, NA, "no inclusion signal after matrix removal",
                             "matrix particle", "corners empty"))
  expect_equal(sum(r$summary$n), 3)
  expect_equal(sort(r$summary$class), c("Alumina", "C12A7", "Spinel-type"))
})

test_that("without alloy correction the steel's Cr and Ni lower the coverage, and the threshold decides", {
  args <- list(data = d, preset = "cao-al2o3-mgo", matrix_element = "Fe", matrix_mode = "matrix_only")
  loose <- do.call(analyze_inclusion_preset, args)
  strict <- do.call(analyze_inclusion_preset, c(args, list(threshold_pct = 90)))
  expect_equal(loose$class[1], "Spinel-type")
  expect_equal(unlist(loose$coords[1, ]), c(A = 0, B = 0.71669, C = 0.28331), tolerance = 1e-4)
  expect_lt(loose$coverage_pct[1], 90)
  expect_gt(loose$coverage_pct[1], 80)
  expect_equal(strict$excluded[1], "corners below threshold")
  expect_equal(run(threshold_pct = 90)$class[1], "Spinel-type")
})

test_that("with no matrix removal the steel stays as other elements and lowers the coverage", {
  r <- analyze_inclusion_preset(d, "cao-al2o3-mgo", threshold_pct = 1)
  expect_lt(r$coverage_pct[1], 75)
  expect_equal(r$class[1], "Spinel-type")
})

test_that("sulfide and oxide presets classify the MnS and spinel particles", {
  r <- run("mns-cas-oxides")
  expect_equal(r$class[c(1, 2, 3, 6)], c("Oxide-dominated", "Oxide-dominated", "Oxide-dominated", "MnS-rich sulfide"))
  expect_equal(unlist(r$coords[6, ]), c(A = 1, B = 0, C = 0), tolerance = 1e-9)
})

test_that("element presets use element wt% (oxygen is not a corner)", {
  r <- run("mg-al-ca")
  w <- inclusion_atomic_weights()
  expect_equal(unname(unlist(r$coords[1, ])), c(w[["Mg"]], 2 * w[["Al"]], 0) / (w[["Mg"]] + 2 * w[["Al"]]), tolerance = 1e-4)
  expect_equal(r$class[1:3], c("Spinel", "Al2O3 (alumina)", "C12A7"))
})

test_that("mole basis works for nearest-phase presets", {
  r <- run("mgo-sio2-al2o3", basis = "mole")
  expect_equal(unname(unlist(r$coords[1, ])), c(0.5, 0, 0.5), tolerance = 1e-4)
  expect_equal(r$class[1], "Spinel")
  expect_equal(r$class[2], "Al2O3 (alumina)")
  expect_error(run("cao-al2o3-mgo", basis = "mole"), "written in mass fractions")
})

test_that("the sulfur order and Ti treatment reach the conversion", {
  ti <- incl_data(incl_row(list(TiN = 60, Al2O3 = 40), steel = 30))
  a <- analyze_inclusion_preset(ti, "tin-al2o3-mgo", matrix_element = "Fe", matrix_mode = "matrix_and_alloys",
                                steel_composition = INCL_STEEL, ti_as = "TiN")
  expect_equal(unname(unlist(a$coords[1, ])), c(0.6, 0.4, 0), tolerance = 1e-4)
  expect_equal(a$class, "TiN")
  b <- analyze_inclusion_preset(ti, "tin-al2o3-mgo", matrix_element = "Fe", matrix_mode = "matrix_and_alloys",
                                steel_composition = INCL_STEEL, ti_as = "TiO2", threshold_pct = 1)
  expect_equal(b$conversion$settings$ti_as, "TiO2")
  expect_equal(b$conversion$moles$TiN, 0)
  expect_gt(b$conversion$moles$TiO2, 0)
})

test_that("results carry a readable settings note", {
  r <- run()
  expect_match(r$settings[1], "CaO-Al2O3-MgO \\(mass basis, rules classification\\)")
  expect_match(r$settings[2], "Fe removed \\(matrix and alloys; ignored: C\\)")
  expect_match(r$settings[3], "Sulfur to Ca then Mn; Ti as TiN")
  expect_match(r$settings[4], "Kept 3 of 6 particles \\(corners >= 50% of total\\)")
  expect_length(run("mg-al-ca")$settings, 3)
})

test_that("bad input gives clear errors", {
  expect_error(analyze_inclusion_preset(d, "nope"), "Unknown preset")
  expect_error(analyze_inclusion_preset(data.frame(Area = 1:3), "cao-al2o3-mgo"), "No element wt% columns found")
  expect_error(analyze_inclusion_preset(d, "cao-al2o3-mgo", matrix_element = "Zn"), "Matrix element Zn has no column")
})

test_that("the input data is not modified", {
  before <- d
  run()
  expect_identical(d, before)
})

test_that("every preset draws without error, including when no particle passes the threshold", {
  for (id in c("cao-al2o3-mgo", "mno-sio2-al2o3", "mns-cas-oxides", "mg-al-ca", "ti-al-n")) {
    r <- run(id, threshold_pct = 1)
    f <- tempfile(fileext = ".png")
    png(f, width = 900, height = 1000)
    expect_no_error(tab <- plot_inclusion_preset(r))
    dev.off()
    expect_gt(file.info(f)$size, 0)
    expect_equal(tab, r$summary)
  }
  none <- run(threshold_pct = 101)
  png(tempfile(fileext = ".png"))
  expect_no_error(plot_inclusion_preset(none, show_lines = FALSE, show_reference = FALSE, legend = FALSE, notes = FALSE))
  dev.off()
})
