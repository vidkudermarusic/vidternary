# Tests for R/helpers.R's plot-dimension/color helpers. Pure logic
# (create_group_legend() needs an open graphics device, since it's a thin
# wrapper around base graphics::legend()).
#
# Includes a regression test for a real bug found and fixed in this pass:
# generate_distinct_colors(n) silently returned MORE than n colors for
# n = 1, 2, 13, 14, 25, or 26 - RColorBrewer::brewer.pal() has an
# undocumented floor of 3 (it warns and returns 3 colors instead of
# erroring for a requested n below that), and all three size-band branches
# in this function request brewer.pal() at least once with a count that
# can legitimately be as low as 1.
#
# create_multi_line_title()/preview_title_layout()/calculate_plot_
# dimensions() are deliberately not tested here - an earlier version of
# this file did, before realizing (via a full-tree grep, not assumed) that
# all three top-level versions were dead code with zero real callers,
# shadowed everywhere that mattered by a completely different, actually-used
# local closure of the same name inside prepare_ternary_plot_data() - the
# same class of bug as the dead apply_individual_filters() found and
# removed elsewhere in this audit. All three were deleted rather than kept
# covered, so as not to lock in tests for code nobody calls.

test_that("generate_distinct_colors() returns exactly n_groups colors at every RColorBrewer floor-of-3 boundary", {
  # Before the fix: n=1/2 (Set3 branch), n=13/14 (Set3+Paired branch, the
  # "how many more" remainder hitting the floor), and n=25/26 (+Dark2
  # branch, same reason) all silently returned 3/15/27 colors instead of
  # the requested count.
  for (n in c(1, 2, 3, 12, 13, 14, 24, 25, 26, 32, 33, 40, 60)) {
    colors <- suppressWarnings(generate_distinct_colors(n))
    expect_length(colors, n)
  }
})

test_that("generate_distinct_colors() returns character(0) for n_groups <= 0", {
  expect_equal(generate_distinct_colors(0), character(0))
  expect_equal(generate_distinct_colors(-5), character(0))
})

test_that("show_message() prints a timestamped, upper-cased-type line to the console", {
  expect_output(show_message("Something happened", "warning"), "WARNING: Something happened")
})

test_that("create_group_legend() draws without error for a real set of groups, and is a no-op for zero groups", {
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp)
  on.exit({ grDevices::dev.off(); unlink(tmp) }, add = TRUE)
  graphics::plot(1, 1)

  counts <- c(Oxide = 14, Sulfide = 6)
  colors <- c("#FF0000", "#00FF00")
  expect_no_error(create_group_legend(c("Oxide", "Sulfide"), colors, counts))
  expect_no_error(create_group_legend(character(0), character(0), counts))
})
