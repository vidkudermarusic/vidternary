# Tests for R/helpers_pre_analysis_filter.R - the shared "Pre-Analysis Data
# Filter" section reused by EVS, Spatial Clustering, and Point Pattern
# Analysis. `input` is accessed only via `[[`, so a plain named list stands
# in for a real Shiny input object with no mocking needed - same convention
# as test-helpers-filters.R.
#
# create_pre_filter_ui()/render_pre_filter_inputs() build shiny.tag objects
# and are exercised indirectly through each tab's own testServer() suite
# (test-server-evs.R/test-server-spatial.R/test-server-spatial-ppp.R) -
# what's tested directly here is the pure logic: collecting filter values
# from input, and applying them to real data.

test_that("collect_pre_filters() reads only the selected columns' own inputs, sanitizing names the same way the UI's textInput IDs are built", {
  input <- list(
    evs_filter_Area = "> 1",
    `evs_filter_Fe__wt___` = "> 10",  # "Fe (wt.%)" sanitized: non-alphanumerics -> "_"
    evs_filter_Unused = "> 999"       # present in input, but NOT in selected_columns below
  )
  result <- collect_pre_filters(input, "evs", c("Area", "Fe (wt.%)"))
  expect_equal(result, list(Area = "> 1", `Fe (wt.%)` = "> 10"))
})

test_that("collect_pre_filters() skips a selected column left blank/whitespace, and trims surrounding whitespace on real values", {
  input <- list(evs_filter_Area = "  > 1  ", evs_filter_Fe = "   ", evs_filter_O = NA)
  result <- collect_pre_filters(input, "evs", c("Area", "Fe", "O"))
  expect_equal(result, list(Area = "> 1"))
})

test_that("collect_pre_filters() returns an empty list for NULL/empty selected_columns", {
  expect_equal(collect_pre_filters(list(), "evs", NULL), list())
  expect_equal(collect_pre_filters(list(), "evs", character(0)), list())
})

test_that("apply_pre_filters() applies a single filter correctly", {
  d <- data.frame(Area = c(0.01, 0.5, 1, 2, 10))
  result <- apply_pre_filters(d, list(Area = "> 1"))
  expect_equal(result$Area, c(2, 10))
})

test_that("apply_pre_filters() combines multiple column filters with AND logic - matching the user's own worked example", {
  d <- data.frame(Area = c(0.01, 0.5, 2, 5, 20), Fe = c(5, 15, 20, 5, 30))
  # Only rows with BOTH Area > 1 AND Fe > 10 should survive: row 3 (Area=2,
  # Fe=20) and row 5 (Area=20, Fe=30). Row 2 (Area=0.5) fails Area; row 4
  # (Fe=5) fails Fe; row 1 fails both.
  result <- apply_pre_filters(d, list(Area = "> 1", Fe = "> 10"))
  expect_equal(nrow(result), 2)
  expect_equal(result$Area, c(2, 20))
  expect_equal(result$Fe, c(20, 30))
})

test_that("apply_pre_filters() returns the data frame unchanged when filters is empty", {
  d <- data.frame(Area = c(1, 2, 3))
  expect_identical(apply_pre_filters(d, list()), d)
})

test_that("apply_pre_filters() errors clearly when the named column doesn't exist in the data", {
  d <- data.frame(Area = c(1, 2, 3))
  expect_error(apply_pre_filters(d, list(NoSuchColumn = "> 1")),
               "Filter column 'NoSuchColumn' was not found", fixed = TRUE)
})

test_that("apply_pre_filters() errors clearly on a non-numeric filter column instead of a raw comparison-on-character error", {
  d <- data.frame(Area = c(1, 2, 3), Label = c("a", "b", "c"))
  expect_error(apply_pre_filters(d, list(Label = "> 1")),
               "Filter column 'Label' is not numeric", fixed = TRUE)
})

test_that("apply_pre_filters() names the specific column in an invalid-syntax error, not just apply_filter()'s own generic message", {
  d <- data.frame(Area = c(1, 2, 3))
  err <- tryCatch(apply_pre_filters(d, list(Area = "not a filter")), error = function(e) e)
  expect_match(conditionMessage(err), "Invalid filter for column 'Area'", fixed = TRUE)
})

test_that("apply_pre_filters() with the user's own real-world example (excluding inclusions below 1 um2) works end to end", {
  d <- data.frame(Area = c(0.01, 0.05, 0.5, 1, 1.5, 5, 20))
  result <- apply_pre_filters(d, list(Area = "> 1"))
  expect_equal(result$Area, c(1.5, 5, 20))
  expect_true(all(result$Area > 1))
  expect_true(!any(d$Area[d$Area <= 1] %in% result$Area))
})
