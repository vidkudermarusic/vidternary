test_that("text field IDs are grouped in natural order, not alphabetical", {
  ids <- paste0("F", 1:12)
  a <- assign_control_areas(ids, 4)
  expect_equal(a$control_area, rep(1:3, each = 4))
  expect_equal(natural_sort_ids(c("F10", "F2", "F1")), c("F1", "F2", "F10"))
  expect_equal(natural_sort_ids(c("b2", "a10", "a9", "b1")), c("a9", "a10", "b1", "b2"))
  expect_equal(natural_sort_ids(c("7", "x")), c("7", "x"))
})

test_that("assign_control_areas reports gaps in numeric IDs", {
  a <- assign_control_areas(c(1:5, 8:10), 2)
  expect_equal(a$n_fields, 10)
  expect_equal(a$n_fields_with_data, 8)
  expect_equal(a$n_missing_fields, 2)
  expect_equal(a$longest_missing_run, 2)
  expect_null(a$ids_sorted)
  b <- assign_control_areas(paste0("F", 1:3), 1)
  expect_equal(b$ids_sorted, c("F1", "F2", "F3"))
  expect_equal(b$n_missing_fields, 0)
})

test_that("evs_area_notes stays quiet for a normal series with the odd empty field", {
  ids <- setdiff(1:608, c(71, 443))
  a <- assign_control_areas(ids, 16)
  n <- evs_area_notes(a, a$n_control_areas, 16)
  expect_length(n$warnings, 0)
  expect_length(n$notes, 0)
})

test_that("evs_area_notes warns about separate plates, empty control areas and explains text IDs", {
  plates <- assign_control_areas(c(101:106, 201:206), 3)
  n <- evs_area_notes(plates, plates$n_control_areas, 3)
  expect_length(n$warnings, 1)
  expect_match(n$warnings, "Only 12 of the 106 field numbers")
  expect_match(n$warnings, "Number the fields consecutively")

  a <- assign_control_areas(1:20, 2)
  n2 <- evs_area_notes(a, 8, 2)
  expect_match(n2$warnings, "2 of the 10 control areas contain no valid inclusion")

  txt <- assign_control_areas(paste0("F", 1:12), 4)
  n3 <- evs_area_notes(txt, 3, 4)
  expect_length(n3$warnings, 0)
  expect_match(n3$notes, "ordered naturally")
  expect_match(n3$notes, "F1, F2, F3, F4")
})
