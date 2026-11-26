# Tests for R/import_standardize.R and R/import_profiles.R.

foreign_export <- function() {
  data.frame(`Part No` = 1:3, Frame = c(1, 1, 2), `Feature Area [um2]` = c(12.5, 3.25, 7),
             `Dmax (um)` = c(5.2, 2.5, 4), `Dmin (um)` = c(3.1, 1.9, 2), `X_stage [mm]` = c(10, 10.25, 11.5),
             `Y_stage [mm]` = c(20.5, 20.75, 21), `Al At%` = c(40, 0, 20.5), `O At%` = c(60, 0, 30.5),
             `Ti At%` = c(0, 100, 49), Comment = c("a", "b", "c"), check.names = FALSE, stringsAsFactors = FALSE)
}

test_that("atomic percent becomes the weight percent of the compound", {
  # Al2O3: 40 at% Al, 60 at% O -> 52.93 wt% Al, 47.07 wt% O
  w <- inclusion_atomic_weights()
  wt <- atomic_to_weight_percent(data.frame(Al = c(40, 0), O = c(60, 0), Ti = c(0, 100)))
  expect_equal(wt$Al[1], 100 * 2 * w[["Al"]] / (2 * w[["Al"]] + 3 * w[["O"]]))
  expect_equal(wt$Al[1], 52.93, tolerance = 1e-3)
  expect_equal(unlist(wt[2, ], use.names = FALSE), c(0, 0, 100))
  expect_equal(rowSums(wt), c(100, 100), ignore_attr = TRUE)
})

test_that("atomic percentages that do not sum to 100 still give 100 wt%, and blanks give NA", {
  wt <- atomic_to_weight_percent(data.frame(Fe = c(10, 0), Cr = c(10, 0)))
  expect_equal(sum(wt[1, ]), 100)
  expect_true(all(is.na(wt[2, ])))
  expect_error(atomic_to_weight_percent(data.frame(Fe = 50, Xx = 50)), "No atomic weight for: Xx")
})

test_that("mapped columns get the standard names, in the standard order", {
  d <- foreign_export()
  p <- detect_import_profile(d)
  s <- apply_import_profile(d, p)
  expect_equal(names(s), c("Feature", "Field", "Area.(um2)", "ECD.(um)", "Length.(um)", "Breadth.(um)", "Aspect.Ratio",
                           "X.(mm)", "Y.(mm)", "Al.(Wt%)", "O.(Wt%)", "Ti.(Wt%)", "Comment"))
  expect_equal(s$Feature, 1:3)
  expect_equal(s[["Area.(um2)"]], c(12.5, 3.25, 7))
  expect_equal(s[["ECD.(um)"]], 2 * sqrt(c(12.5, 3.25, 7) / pi))
  expect_equal(s[["Aspect.Ratio"]], c(5.2 / 3.1, 2.5 / 1.9, 2))
  expect_equal(s[["Ti.(Wt%)"]][2], 100)
  expect_equal(s$Comment, c("a", "b", "c"))
  notes <- attr(s, "standardize_notes")
  expect_true(any(grepl("Equivalent circle diameter computed", notes)))
  expect_true(any(grepl("Aspect ratio computed", notes)))
  expect_true(any(grepl("Atomic percent converted to weight percent over 3 elements", notes)))
})

test_that("the standard table is recognised by the analysis tabs' own column detection", {
  s <- apply_import_profile(foreign_export(), detect_import_profile(foreign_export()))
  numeric_cols <- names(s)[vapply(s, is.numeric, logical(1))]
  expect_equal(first_match_or_null(numeric_cols, "^area"), "Area.(um2)")
  expect_equal(first_match_or_null(numeric_cols, "^length|feret.*max|max.*feret"), "Length.(um)")
  expect_equal(first_match_or_null(numeric_cols, "^breadth|^width|feret.*min|min.*feret"), "Breadth.(um)")
  expect_equal(first_match_or_null(numeric_cols, "stage.?x|^x"), "X.(mm)")
  expect_equal(first_match_or_null(names(s), "field|frame|fov"), "Field")
  expect_equal(names(inclusion_element_columns(names(s))), c("Al", "O", "Ti"))
})

test_that("units are converted to um, um2 and mm", {
  d <- data.frame(L = 2000, A = 3, X = 5000, Y = 250)
  s <- standardize_particle_table(d, c(length = "L", area = "A", x = "X", y = "Y"),
                                  units = list(length = "nm", area = "mm", x = "um", y = "um"))
  expect_equal(s[["Length.(um)"]], 2)
  expect_equal(s[["Area.(um2)"]], 3e6)
  expect_equal(s[["X.(mm)"]], 5)
  expect_equal(s[["Y.(mm)"]], 0.25)
  notes <- attr(s, "standardize_notes")
  expect_true(any(grepl("Length .*converted from nm", notes)))
  expect_true(any(grepl("Area: converted from mm2", notes)))
})

test_that("a unit named in the column is used, and a missing one is assumed with a note", {
  d <- data.frame(`Len (nm)` = 500, Breadth = 2, `Pos X (um)` = 1500, check.names = FALSE)
  s <- standardize_particle_table(d, c(length = "Len (nm)", breadth = "Breadth", x = "Pos X (um)"))
  expect_equal(s[["Length.(um)"]], 0.5)
  expect_equal(s[["Breadth.(um)"]], 2)
  expect_equal(s[["X.(mm)"]], 1.5)
  expect_true(any(grepl("no unit named in 'Breadth', taken as um", attr(s, "standardize_notes"), fixed = TRUE)))
})

test_that("the area is computed from the diameter when only that is given", {
  s <- standardize_particle_table(data.frame(D = c(2, 4)), c(ecd = "D"))
  expect_equal(s[["Area.(um2)"]], pi / 4 * c(4, 16))
  expect_equal(names(s), c("Area.(um2)", "ECD.(um)"))
})

test_that("weight percent columns are only renamed, and unmapped columns can be dropped", {
  d <- data.frame(Area = 1, `Al (Wt%)` = 12.5, `Fe wt%` = 80, Extra = "x", check.names = FALSE, stringsAsFactors = FALSE)
  el <- detect_element_columns(d)
  s <- standardize_particle_table(d, c(area = "Area"), elements = el, keep_other = FALSE)
  expect_equal(names(s), c("Area.(um2)", "ECD.(um)", "Al.(Wt%)", "Fe.(Wt%)"))
  expect_equal(s[["Al.(Wt%)"]], 12.5)
  expect_false(any(grepl("Atomic percent", attr(s, "standardize_notes"))))
})

test_that("a wrong mapping is refused in plain words", {
  d <- data.frame(A = 1, B = 2, `X (pixels)` = 3, check.names = FALSE)
  expect_error(standardize_particle_table(d, c(area = "Nope")), "Column not found in the file: Nope")
  expect_error(standardize_particle_table(d, c(area = "A", length = "A")), "assigned to more than one role")
  expect_error(standardize_particle_table(d, c(volume = "A")), "Unknown column role: volume")
  expect_error(standardize_particle_table(d, c(x = "X (pixels)")), "is in pixels")
  expect_error(standardize_particle_table(d, c(area = "A"), units = list(area = "inch")), "Unknown unit 'inch'")
  # "none" and NA mean "not mapped"
  expect_equal(names(standardize_particle_table(d, c(area = "A", length = "none", breadth = NA), keep_other = FALSE)),
               c("Area.(um2)", "ECD.(um)"))
})

test_that("a detected profile describes the file", {
  p <- detect_import_profile(foreign_export(), "My SEM")
  expect_equal(p$name, "My SEM")
  expect_equal(p$mapping$area, "Feature Area [um2]")
  expect_equal(p$units$x, "mm")
  expect_equal(p$units$length, "um")
  expect_equal(p$elements$element, c("Al", "O", "Ti"))
  expect_equal(p$basis, "at")
  expect_false("aspect" %in% names(p$mapping))
})

test_that("a profile survives being saved and read back", {
  d <- foreign_export()
  p <- detect_import_profile(d, "My SEM")
  path <- tempfile(fileext = ".json")
  write_import_profile(p, path)
  back <- read_import_profile(path)
  expect_equal(back$name, "My SEM")
  expect_equal(back$mapping, p$mapping)
  expect_equal(back$units, p$units)
  expect_equal(back$elements[, c("element", "column")], p$elements[, c("element", "column")], ignore_attr = TRUE)
  expect_equal(back$basis, "at")
  expect_equal(apply_import_profile(d, back), apply_import_profile(d, p))
})

test_that("a profile that does not fit the file, or is not a profile, is refused", {
  p <- detect_import_profile(foreign_export(), "My SEM")
  other <- data.frame(Area = 1, Length = 2)
  err <- expect_error(apply_import_profile(other, p))
  expect_match(conditionMessage(err), "does not match the profile 'My SEM'", fixed = TRUE)
  expect_match(conditionMessage(err), "Feature Area [um2]", fixed = TRUE)

  not_json <- tempfile(fileext = ".json"); writeLines("this is not json", not_json)
  expect_error(read_import_profile(not_json), "not valid JSON")
  wrong <- tempfile(fileext = ".json"); writeLines('{"kind": "tab settings"}', wrong)
  expect_error(read_import_profile(wrong), "not a vidternary import profile")
})

test_that("a profile without elements or units works", {
  d <- data.frame(Area = c(1, 4), Length = c(2, 3))
  p <- detect_import_profile(d)
  expect_equal(nrow(p$elements), 0)
  path <- tempfile(fileext = ".json")
  write_import_profile(p, path)
  back <- read_import_profile(path)
  expect_equal(nrow(back$elements), 0)
  expect_equal(names(apply_import_profile(d, back)), c("Area.(um2)", "ECD.(um)", "Length.(um)"))
})
