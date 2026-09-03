# Tests for R/helpers_filters.R's filter-collection/application helpers.
# `input` is accessed only via `[[`, so a plain named list stands in for a
# real Shiny input object with no mocking needed.
#
# apply_individual_filters() is deliberately not tested here - the
# top-level copy that used to live in this file was dead code (shadowed by
# prepare_ternary_plot_data()'s own local definition of the same name,
# confirmed to have zero real callers) and was removed this pass; the real
# implementation is exercised indirectly through prepare_ternary_plot_data()
# itself in test-pass5-regressions.R and elsewhere.

test_that("collect_filters() builds the main-tab input ID convention (filter_<type><dataset>_<element>) and skips empty/whitespace values", {
  input <- list(
    filter_A1_Al = "> 10",
    filter_A1_Si = "   ",
    filter_A1_Mn = NA
  )
  result <- collect_filters(c("Al", "Si", "Mn"), "A", input, prefix = "filter", dataset_num = 1)
  expect_equal(result, list(Al = "> 10"))
})

test_that("collect_filters() builds the batch-tab input ID convention (multiple_filter_<type>_<element>) with no dataset number", {
  input <- list(multiple_filter_A_Al = "< 5")
  result <- collect_filters(c("Al"), "A", input, prefix = "multiple_filter")
  expect_equal(result, list(Al = "< 5"))
})

test_that("collect_filters() sanitizes non-alphanumeric characters in element names the same way the dynamic filter UI does", {
  input <- setNames(list("> 1"), "filter_A1_Fe_Ka1")
  result <- collect_filters(c("Fe.Ka1"), "A", input, prefix = "filter", dataset_num = 1)
  expect_equal(result, list(`Fe.Ka1` = "> 1"))
})

test_that("collect_filters() returns an empty list for NULL/empty elements", {
  expect_equal(collect_filters(NULL, "A", list()), list())
  expect_equal(collect_filters(character(0), "A", list()), list())
})

test_that("collect_individual_filters() and collect_main_ternary_filters() delegate to collect_filters() with the right prefix/dataset_num", {
  input <- list(multiple_filter_B_Al = "> 1", filter_C2_Si = "< 9")
  expect_equal(collect_individual_filters("Al", "B", input), list(Al = "> 1"))
  expect_equal(collect_main_ternary_filters("Si", "C", 2, input), list(Si = "< 9"))
  # Cross-check: the batch-style call must NOT pick up the main-tab-style
  # input even though both are named after the same element/type letter.
  expect_equal(collect_main_ternary_filters("Al", "B", 2, input), list())
})

test_that("apply_filter() supports all six comparison operators", {
  d <- data.frame(x = 1:10)
  expect_equal(nrow(apply_filter(d, "x", "> 5")), 5)
  expect_equal(nrow(apply_filter(d, "x", "< 5")), 4)
  expect_equal(nrow(apply_filter(d, "x", ">= 5")), 6)
  expect_equal(nrow(apply_filter(d, "x", "<= 5")), 5)
  expect_equal(nrow(apply_filter(d, "x", "== 5")), 1)
  expect_equal(nrow(apply_filter(d, "x", "!= 5")), 9)
})

test_that("apply_filter() returns the data frame unchanged when filter is NULL", {
  d <- data.frame(x = 1:5)
  expect_identical(apply_filter(d, "x", NULL), d)
})

test_that("apply_filter() errors clearly on a non-numeric value or an unrecognized format", {
  d <- data.frame(x = 1:5)
  # as.numeric("abc") itself emits a real, expected base R "NAs introduced
  # by coercion" warning on the way to the intended error - suppressed once
  # confirmed expected, matching this suite's convention.
  suppressWarnings(expect_error(apply_filter(d, "x", "> abc"), "Invalid filter value"))
  expect_error(apply_filter(d, "x", "abc"), "Invalid filter format")
  expect_error(apply_filter(d, "x", "<>10"), "Invalid filter format")
})
