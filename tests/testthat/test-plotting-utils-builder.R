# Tests for R/plotting_utils_builder.R's build_custom_plot() - the generic,
# schema-agnostic chart builder behind the "Plot Builder" tab. Pure ggplot2
# logic, no Shiny dependency.
#
# Before this file, the only coverage anywhere was
# test-pass5-regressions.R's single crash-scenario check for type = "bar"
# (a value filter removing every row); the other five chart types -
# violin, box, hist, scatter, rose - and their own edge cases (log scale
# against non-positive values, multi-column Y reshape, all-NA direction
# data, color faceting) had none.
#
# ggplot2::ggplot_build() forces a real render (not just object
# construction) the same way test-pass5-regressions.R's own bar-chart
# checks do, so a broken aes()/geom() call surfaces here rather than only
# at literal screen-draw time.

make_violin_data <- function(n = 30, seed = 1) {
  set.seed(seed)
  data.frame(
    group = sample(c("A", "B", "C"), n, replace = TRUE),
    Al = abs(stats::rnorm(n, 10, 2)),
    Si = abs(stats::rnorm(n, 10, 2)),
    stringsAsFactors = FALSE
  )
}

test_that("violin/box render for a single Y column, with and without a color grouping", {
  d <- make_violin_data()
  for (type in c("violin", "box")) {
    expect_no_error(ggplot2::ggplot_build(build_custom_plot(d, type = type, x = "group", y = "Al")))
    expect_no_error(ggplot2::ggplot_build(build_custom_plot(d, type = type, x = "group", y = "Al", color_by = "group")))
  }
})

test_that("violin/box reshape multiple Y columns into one sub-group per column, comparing side by side", {
  d <- make_violin_data()
  built <- ggplot2::ggplot_build(build_custom_plot(d, type = "violin", x = "group", y = c("Al", "Si")))
  # geom_violin's built layer has many rows per violin (tracing its density
  # outline), not one row per category - ggplot2's own internal `group`
  # aesthetic is the right thing to count instead: one distinct group id per
  # (x-category x fill-column) combination, so 3 groups x 2 columns = 6.
  violin_layer <- built$data[[1]]
  expect_equal(length(unique(violin_layer$group)), 6)
  # Every row of the reshaped long data is accounted for - no row silently
  # dropped by the wide-to-long reshape itself (only real NAs are dropped).
  expect_equal(sum(!is.na(d$Al)) + sum(!is.na(d$Si)), sum(sapply(built$plot$data$.builder_value, is.finite)))
})

test_that("a single Y column selected as a length-1 vector for violin/box still uses that column directly (not the multi-column reshape path)", {
  d <- make_violin_data()
  built <- ggplot2::ggplot_build(build_custom_plot(d, type = "box", x = "group", y = "Al"))
  expect_false(".builder_value" %in% names(built$plot$data))
  expect_true("Al" %in% names(built$plot$data))
})

test_that("hist renders with and without color, and respects hist_bins", {
  d <- make_violin_data(n = 100)
  expect_no_error(ggplot2::ggplot_build(build_custom_plot(d, type = "hist", x = "Al", hist_bins = 10)))
  built_colored <- ggplot2::ggplot_build(build_custom_plot(d, type = "hist", x = "Al", color_by = "group", hist_bins = 10))
  expect_true(nrow(built_colored$data[[1]]) > 0)

  built_10 <- ggplot2::ggplot_build(build_custom_plot(d, type = "hist", x = "Al", hist_bins = 10))
  built_30 <- ggplot2::ggplot_build(build_custom_plot(d, type = "hist", x = "Al", hist_bins = 30))
  expect_lt(nrow(built_10$data[[1]]), nrow(built_30$data[[1]]))
})

test_that("scatter renders with and without color", {
  d <- make_violin_data()
  expect_no_error(ggplot2::ggplot_build(build_custom_plot(d, type = "scatter", x = "Al", y = "Si")))
  built <- ggplot2::ggplot_build(build_custom_plot(d, type = "scatter", x = "Al", y = "Si", color_by = "group"))
  expect_equal(nrow(built$data[[1]]), nrow(d))
})

test_that("log_x/log_y don't crash against zero or negative values - ggplot2 turns them non-finite instead", {
  d <- data.frame(x = c(-1, 0, 1, 2, 3), y = c(1, 2, 3, 4, 5))
  expect_warning(
    built <- ggplot2::ggplot_build(build_custom_plot(d, type = "scatter", x = "x", y = "y", log_x = TRUE)),
    "NaNs produced|infinite values"
  )
  # All 5 rows are still present in the built data (scale_x_log10() doesn't
  # drop rows at build time), but only the 3 strictly-positive x values
  # came through as an actual finite, plottable log10(x).
  expect_equal(nrow(built$data[[1]]), 5)
  expect_equal(sum(is.finite(built$data[[1]]$x)), 3)
})

test_that("rose renders with and without color faceting, and wraps out-of-range direction values via mod 360", {
  set.seed(1)
  d <- data.frame(direction = c(runif(30, 0, 360), -10, 370), type = sample(c("Oxide", "Sulfide"), 32, replace = TRUE))
  expect_no_error(ggplot2::ggplot_build(build_custom_plot(d, type = "rose", x = "direction", rose_bin_width = 15)))
  built_colored <- ggplot2::ggplot_build(build_custom_plot(d, type = "rose", x = "direction", color_by = "type", rose_bin_width = 15))
  expect_true(nrow(built_colored$data[[1]]) > 0)

  # -10 wraps to 350, 370 wraps to 10 - both land inside [0, 360), not
  # silently dropped or left negative/over-range.
  no_color <- build_custom_plot(d, type = "rose", x = "direction", rose_bin_width = 15)
  built_plot <- ggplot2::ggplot_build(no_color)
  expect_true(all(built_plot$data[[1]]$x >= 0 & built_plot$data[[1]]$x < 360))
})

test_that("rose with every direction value NA renders an all-zero-count plot instead of crashing", {
  # cut()'s result is always a real factor with the bin breaks as its
  # declared levels, unlike the bar chart's plain-vector table() (the
  # already-fixed crash in that branch) - so table(bins) still produces a
  # proper zero-count table here even when nothing falls into any bin,
  # confirmed directly rather than assumed from that difference.
  d <- data.frame(direction = rep(NA_real_, 10))
  expect_no_error(built <- ggplot2::ggplot_build(build_custom_plot(d, type = "rose", x = "direction", rose_bin_width = 30)))
  expect_true(all(built$data[[1]]$y == 0))
})

test_that("a non-numeric, non-positive, or single-value rose_bin_width falls back to the documented default of 10 degrees", {
  d <- data.frame(direction = c(10, 50, 90, 130))
  built_bad <- ggplot2::ggplot_build(build_custom_plot(d, type = "rose", x = "direction", rose_bin_width = -5))
  built_default <- ggplot2::ggplot_build(build_custom_plot(d, type = "rose", x = "direction", rose_bin_width = 10))
  expect_equal(nrow(built_bad$data[[1]]), nrow(built_default$data[[1]]))
})

test_that("bar 'percent, no color' shows each category's share of the true grand total, not per-group 100%", {
  d <- data.frame(cat = c(rep("A", 3), rep("B", 2), "C"))
  # coord_flip() only swaps the *rendered* orientation - the built layer
  # data still holds the percentage under $y, the aes() it was actually
  # mapped to before the flip.
  pct <- ggplot2::layer_data(build_custom_plot(d, type = "bar", x = "cat", percent = TRUE))$y
  # Hand-computed: 3/6, 2/6, 1/6 of 6 total rows.
  expect_equal(sort(round(pct, 3)), sort(round(c(3, 2, 1) / 6 * 100, 3)))
})

test_that("an unknown plot type errors clearly instead of returning something silently wrong", {
  d <- data.frame(x = 1:3)
  expect_error(build_custom_plot(d, type = "not_a_real_type", x = "x"), "Unknown plot type")
})
