# Tests for R/import_roles.R: which column of an export is what.

# A one-row table with the given column names; `text` names the columns
# that hold text instead of numbers.
table_with <- function(cols, text = character(0)) {
  d <- as.data.frame(lapply(cols, function(cn) if (cn %in% text) "x" else 1), stringsAsFactors = FALSE)
  names(d) <- cols
  d
}
micro <- "\u00b5"

# Column names of the Oxford AZtec Feature export used in this project, as
# openxlsx reads them.
aztec_cols <- c("Feature", "Area", "Field", "Rank", "Unclassified", paste0("Area.(sq..", micro, "m)"), "Aspect.Ratio",
                "Beam.X.(pixels)", "Beam.Y.(pixels)", paste0("Breadth.(", micro, "m)"), "Direction.(degrees)",
                paste0("ECD.(", micro, "m)"), paste0("Length.(", micro, "m)"), paste0("Perimeter.(", micro, "m)"), "Shape",
                "Mean.grey", "Spectrum.Area", "Stage.X.(mm)", "Stage.Y.(mm)", "Stage.Z.(mm)", "C.(Wt%)", "N.(Wt%)",
                "O.(Wt%)", "Al.(Wt%)", "S.(Wt%)", "Ti.(Wt%)", "V.(Wt%)", "Fe.(Wt%)", "Mo.(Wt%)")

test_that("every role of the AZtec Feature export is found", {
  r <- detect_column_roles(table_with(aztec_cols, text = c("Area", "Rank")))
  expect_equal(r[["feature"]], "Feature")
  expect_equal(r[["field"]], "Field")
  expect_equal(r[["area"]], paste0("Area.(sq..", micro, "m)"))      # not the text column "Area"
  expect_equal(r[["ecd"]], paste0("ECD.(", micro, "m)"))
  expect_equal(r[["length"]], paste0("Length.(", micro, "m)"))
  expect_equal(r[["breadth"]], paste0("Breadth.(", micro, "m)"))
  expect_equal(r[["aspect"]], "Aspect.Ratio")
  expect_equal(r[["direction"]], "Direction.(degrees)")
  expect_equal(r[["perimeter"]], paste0("Perimeter.(", micro, "m)"))
  expect_equal(r[["x"]], "Stage.X.(mm)")                             # not Beam.X.(pixels)
  expect_equal(r[["y"]], "Stage.Y.(mm)")
})

test_that("other common namings are recognised", {
  r <- detect_column_roles(table_with(c("Part No", "Frame", "Feature Area [um2]", "Dmax (um)", "Dmin (um)", "X_stage [mm]",
                                         "Y_stage [mm]", "Orientation", "Equivalent Circle Diameter")))
  expect_equal(unname(r[c("feature", "field", "area", "length", "breadth", "x", "y", "direction", "ecd")]),
               c("Part No", "Frame", "Feature Area [um2]", "Dmax (um)", "Dmin (um)", "X_stage [mm]", "Y_stage [mm]",
                 "Orientation", "Equivalent Circle Diameter"))
  r2 <- detect_column_roles(table_with(c("ID", "Image", "Area", "Max Feret", "Min Feret", "X", "Y", "Angle")))
  expect_equal(unname(r2[c("feature", "field", "area", "length", "breadth", "x", "y", "direction")]),
               c("ID", "Image", "Area", "Max Feret", "Min Feret", "X", "Y", "Angle"))
})

test_that("what is not there is NA, and look-alikes are not taken", {
  r <- detect_column_roles(table_with(c("Area fraction", "Spectrum Area", "Beam X (pixels)", "Mean grey")))
  expect_true(all(is.na(r)))
  # a text column cannot be a measurement, but can be an ID
  r <- detect_column_roles(table_with(c("Area", "Field"), text = c("Area", "Field")))
  expect_true(is.na(r[["area"]]))
  expect_equal(r[["field"]], "Field")
})

test_that("no column is given two roles", {
  r <- detect_column_roles(table_with(c("Length", "Width", "X", "Y")))
  expect_false(any(duplicated(r[!is.na(r)])))
  expect_equal(r[["breadth"]], "Width")
})

test_that("the role table is complete and consistent", {
  roles <- particle_roles()
  expect_equal(roles$role, c("feature", "field", "area", "ecd", "length", "breadth", "aspect", "direction", "perimeter", "x", "y"))
  expect_false(any(duplicated(roles$canonical)))
  expect_true(all(roles$kind %in% c("id", "length", "area", "position", "angle", "ratio")))
})

test_that("units are read from column names", {
  expect_equal(detect_unit(paste0("Length.(", micro, "m)")), "um")
  expect_equal(detect_unit("Length (um)"), "um")
  expect_equal(detect_unit("Area (sq. um)"), "um")
  expect_equal(detect_unit("Feature Area [um2]"), "um")
  expect_equal(detect_unit("Dmax [microns]"), "um")
  expect_equal(detect_unit("Stage.X.(mm)"), "mm")
  expect_equal(detect_unit("Area mm2"), "mm")
  expect_equal(detect_unit("ECD (nm)"), "nm")
  expect_equal(detect_unit("Beam X (pixels)"), "pixels")
  expect_true(is.na(detect_unit("Length")))
  expect_true(is.na(detect_unit(NA)))
  # "um" inside a word is not a unit
  expect_true(is.na(detect_unit("Spectrum number")))
})

test_that("element columns are found with their basis", {
  d <- table_with(c("Al (Wt%)", "Ti.(Wt%)", "Fe wt.%", "O At%", "N (Atomic %)", "Si K", "Mn", "Wt% Cr", "Mean grey", "Area"))
  e <- detect_element_columns(d)
  expect_equal(e$element, c("Al", "Ti", "Fe", "O", "N", "Si", "Mn", "Cr"))
  expect_equal(e$basis, c("wt", "wt", "wt", "at", "at", "unknown", "unknown", "wt"))
  expect_equal(e$column[8], "Wt% Cr")
})

test_that("an X-ray line letter is not read as potassium, and a position not as yttrium", {
  e <- detect_element_columns(table_with(c("Al K", "K K", "Y", "V")), exclude = "Y")
  expect_equal(e$element, c("Al", "K", "V"))
  # without the exclusion a bare Y would be yttrium
  expect_true("Y" %in% detect_element_columns(table_with(c("Y", "V")))$element)
})

test_that("error columns, repeats and text columns are not element columns", {
  d <- table_with(c("Fe (Wt%)", "Fe (Wt%) Sigma", "Fe wt% error", "Ti (Wt%)", "S"), text = "S")
  e <- detect_element_columns(d)
  expect_equal(e$element, c("Fe", "Ti"))
  expect_equal(e$column, c("Fe (Wt%)", "Ti (Wt%)"))
  expect_equal(nrow(detect_element_columns(table_with(c("Feature", "Area")))), 0)
})

test_that("the atomic weights for conversion extend the inclusion table without changing it", {
  w <- import_atomic_weights()
  base <- inclusion_atomic_weights()
  expect_equal(w[names(base)], base)
  expect_true(all(c("P", "Na", "K", "Ce", "La", "Y", "B") %in% names(w)))
  expect_false(any(duplicated(names(w))))
  expect_equal(w[["Ce"]], 140.116)
})
