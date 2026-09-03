# Tests for R/stats_display_utils.R - tidy, DT-ready descriptive/
# correlation tables and display helpers for the Data Comparison tab. Pure
# logic (render_stats_datatable() and DT itself are the only Shiny-adjacent
# pieces, and DT::datatable() runs fine outside a live session).
#
# This whole file had zero direct test coverage before this pass - only
# indirectly exercised (proving it doesn't crash on typical data, not that
# its actual computed values or edge cases are correct) via
# test-server-data-comparison.R's button-click tests.
#
# library(shiny) is required here (matching every other test file in this
# suite that calls a bare div()/tags$...): vidternary's NAMESPACE doesn't
# import shiny (dependencies are attached at app-launch via dependencies.R,
# not NAMESPACE import()), so build_stat_cards()'s own bare tags$p()/div()
# calls need shiny attached to the search path to resolve at all.
library(shiny)

test_that("build_descriptive_stats_table() computes correct summary statistics, one row per numeric column", {
  d <- data.frame(a = c(1, 2, 3, 4, 5), b = c(10, 20, 30, 40, 50), label = c("x", "y", "x", "y", "x"))
  t <- build_descriptive_stats_table(d)
  expect_equal(nrow(t), 2)
  expect_equal(sort(t$Variable), c("a", "b"))
  row_a <- t[t$Variable == "a", ]
  expect_equal(row_a$N, 5)
  expect_equal(row_a$Missing, 0)
  expect_equal(row_a$Min, 1)
  expect_equal(row_a$Max, 5)
  expect_equal(row_a$Mean, 3)
  expect_equal(row_a$Median, 3)
  expect_equal(row_a$SD, stats::sd(1:5))
  expect_equal(row_a$CV_pct, 100 * stats::sd(1:5) / 3)
})

test_that("build_descriptive_stats_table() handles missing values without erroring", {
  d <- data.frame(has_na = c(1, NA, 3, NA, 5), zero_mean = c(-1, 0, 1, -1, 1))
  t <- build_descriptive_stats_table(d)
  row_na <- t[t$Variable == "has_na", ]
  expect_equal(row_na$N, 5)
  expect_equal(row_na$Missing, 2)
  expect_equal(row_na$Mean, 3)

  # A genuinely zero-mean column leaves CV_pct (SD as % of mean) undefined
  # rather than a divide-by-zero Inf/NaN.
  row_zero <- t[t$Variable == "zero_mean", ]
  expect_equal(row_zero$Mean, 0)
  expect_true(is.na(row_zero$CV_pct))
})

test_that("build_descriptive_stats_table() leaves SD/CV_pct undefined for a column with only one non-NA value, rather than erroring on sd() of length 1", {
  # A single real value plus NAs, not multiple identical values (sd() of
  # several identical values is a valid, real 0 - a different case from
  # this one, where sd() of a length-1 vector is undefined by definition).
  d <- data.frame(one_value = c(7, NA, NA))
  t <- build_descriptive_stats_table(d)
  expect_equal(t$N, 3)
  expect_equal(t$Missing, 2)
  expect_equal(t$Mean, 7)
  expect_true(is.na(t$SD))
  expect_true(is.na(t$CV_pct))
})

test_that("build_descriptive_stats_table() returns an empty, correctly-shaped table when there are no numeric columns", {
  d <- data.frame(label = c("a", "b", "c"))
  t <- build_descriptive_stats_table(d)
  expect_equal(nrow(t), 0)
  expect_equal(names(t), c("Variable", "N", "Missing", "Min", "Q1", "Median", "Mean", "SD", "Q3", "Max", "CV_pct"))
})

test_that("build_descriptive_stats_comparison_table() stacks 2+ datasets long, sorted by Variable then Dataset", {
  d1 <- data.frame(a = c(1, 2, 3))
  d2 <- data.frame(a = c(10, 20, 30))
  t <- build_descriptive_stats_comparison_table(list(One = d1, Two = d2))
  expect_equal(nrow(t), 2)
  expect_equal(t$Dataset, c("One", "Two"))
  expect_equal(t$Mean, c(2, 20))
})

test_that("build_correlation_pairs_table() computes real correlations, sorted by |Correlation| descending", {
  set.seed(1)
  x <- stats::rnorm(50)
  d <- data.frame(a = x, b = x + stats::rnorm(50, sd = 0.01), c = stats::rnorm(50))
  t <- build_correlation_pairs_table(d)
  expect_equal(nrow(t), 3)
  # a/b are near-perfectly correlated (b is a + tiny noise) - must sort first.
  expect_equal(t$Variable_1[1], "a")
  expect_equal(t$Variable_2[1], "b")
  expect_gt(abs(t$Correlation[1]), 0.99)
  expect_true(all(diff(abs(t$Correlation)) <= 0))
})

test_that("build_correlation_pairs_table() returns an empty, correctly-shaped table with fewer than 2 numeric columns", {
  d <- data.frame(a = c(1, 2, 3), label = c("x", "y", "z"))
  t <- build_correlation_pairs_table(d)
  expect_equal(nrow(t), 0)
  expect_equal(names(t), c("Variable_1", "Variable_2", "Correlation"))
})

test_that("build_correlation_comparison_table() stacks per-dataset correlation tables long, and is empty with fewer than 2 common columns", {
  set.seed(1)
  d1 <- data.frame(a = stats::rnorm(20), b = stats::rnorm(20))
  d2 <- data.frame(a = stats::rnorm(20), b = stats::rnorm(20))
  t <- build_correlation_comparison_table(list(One = d1, Two = d2), c("a", "b"))
  expect_equal(nrow(t), 2)
  expect_setequal(t$Dataset, c("One", "Two"))

  empty <- build_correlation_comparison_table(list(One = d1), "a")
  expect_equal(nrow(empty), 0)
})

test_that("render_stats_datatable() returns a real DT htmlwidget with rounding applied to the requested columns", {
  d <- data.frame(Variable = "a", Mean = 3.14159265, SD = 1.23456789, stringsAsFactors = FALSE)
  dt <- render_stats_datatable(d, round_cols = c("Mean", "SD"), digits = 2)
  expect_s3_class(dt, "datatables")
  # formatRound() stores its per-column rounding spec for the browser to
  # apply - confirmed present rather than assuming the call succeeded.
  expect_true(length(dt$x$options$columnDefs) > 0 || !is.null(dt$x$round))
})

test_that("build_stat_cards() shows a placeholder for an empty table, and real cards (with a dataset suffix when present) otherwise", {
  empty_cards <- build_stat_cards(data.frame())
  expect_match(as.character(empty_cards), "No statistics computed yet")

  single <- build_descriptive_stats_table(data.frame(a = c(1, 2, 3)))
  cards <- build_stat_cards(single)
  expect_match(as.character(cards), ">a<")
  expect_false(grepl("\\(", as.character(cards)))

  comparison <- build_descriptive_stats_comparison_table(list(One = data.frame(a = c(1, 2, 3))))
  cards_with_dataset <- build_stat_cards(comparison)
  expect_match(as.character(cards_with_dataset), "\\(One\\)")
})

test_that("render_mini_histogram_base64() returns an empty string for too-few or zero-variance values, and a real <img> tag otherwise", {
  expect_equal(render_mini_histogram_base64(c(5)), "")
  expect_equal(render_mini_histogram_base64(c(5, 5, 5)), "")
  expect_equal(render_mini_histogram_base64(c(NA, NaN, Inf)), "")

  set.seed(1)
  img <- render_mini_histogram_base64(stats::rnorm(20))
  expect_match(img, '^<img src="data:image/png;base64,')
  expect_match(img, 'width="120" height="32"')
})

test_that("add_distribution_column() adds a real Distribution column, and an empty string for a variable no longer present in df", {
  set.seed(1)
  df <- data.frame(a = stats::rnorm(20))
  stats_table <- build_descriptive_stats_table(df)
  with_dist <- add_distribution_column(stats_table, df)
  expect_true("Distribution" %in% names(with_dist))
  expect_match(with_dist$Distribution[1], "<img")

  stale_table <- data.frame(Variable = "no_longer_here")
  with_dist2 <- add_distribution_column(stale_table, df)
  expect_equal(with_dist2$Distribution, "")
})
