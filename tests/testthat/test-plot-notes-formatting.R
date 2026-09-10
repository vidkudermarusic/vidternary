# Tests for the plot-notes formatting fix - a real user-reported bug:
# "when using isolation forest or mahalanobis distance, text overlap
# appears in the plot notes; plot notes must stay on the right side and,
# when the text is too long, continue onto the next line". Traced to two
# independent, stacked problems (both fixed here, both covered below):
#
# 1. prepare_ternary_plot_data()'s "Outlier Detection:"/"Statistical:"
#    lines used to comma-join every detail line (MDmean/MDthresh/stdMD/
#    Method for Mahalanobis; Trees/Contamination/Sample size for Isolation
#    Forest) into ONE long string instead of appending them as separate
#    elements - so col3_text had no "\n" between them at all, and mtext()
#    drew that one long line as-is instead of ever wrapping it.
#
# 2. Even after fixing (1), a single mtext() call on a "\n"-joined string
#    turned out to anchor its LAST line at the given `line=` position,
#    with earlier lines extending TOWARD the plot - confirmed directly by
#    rendering a real PNG and inspecting it, not assumed. Three columns of
#    different lengths (a short "Elements" column next to a long "Outlier
#    Detection: Mahalanobis..." column) had their LAST lines converge on
#    the same position and visually overlap. Fixed with
#    draw_plot_notes_column(), which top-anchors every column's FIRST
#    line at the same start_line and steps each subsequent line further
#    out individually - see that function's own comment in
#    ternary_plot_data_prep.R for the full explanation, including why the
#    per-line step is `cex * 1.1` specifically.

# Builds a valid prepare_ternary_plot_data() call, mirroring exactly the
# argument list general_ternary_plot() itself passes through (see that
# function's own call site in ternary_plot.R) with sensible defaults for
# everything not relevant to a given test, and the given overrides layered
# on top.
build_pd <- function(d, ...) {
  xlsx_path <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(d, xlsx_path)
  out_dir <- tempfile("plot_notes_test_")
  dir.create(out_dir, recursive = TRUE)

  defaults <- list(
    xlsx_file = xlsx_path, working_dir = getwd(), output_dir = out_dir,
    element_A = list(col = "Al"), element_B = list(col = "Si"), element_C = list(col = "Mn"),
    optional_param1 = NULL, optional_param2 = NULL, color_palette = "blue",
    xlsx_display_name = NULL, preview = TRUE,
    use_mahalanobis = FALSE, reference_data = NULL,
    optional_param1_representation = "point_size", output_format = "png",
    use_isolation_forest = FALSE, isolation_ntrees = 200, isolation_contamination = 0.10,
    use_iqr_filter = FALSE, use_zscore_filter = FALSE, use_mad_filter = FALSE,
    lambda = 1, omega = 0,
    keep_outliers_mahalanobis = FALSE, keep_outliers_isolation = FALSE,
    keep_outliers_iqr = FALSE, keep_outliers_zscore = FALSE, keep_outliers_mad = FALSE,
    individual_filters_A = NULL, individual_filters_B = NULL, individual_filters_C = NULL,
    custom_mdthresh = NULL, mdthresh_mode = "auto", mahalanobis_reference = "self",
    selected_columns = c("Al", "Si", "Mn", "Fe", "Ni"),
    include_plot_notes = TRUE, use_manual_point_size = FALSE, manual_point_size = 1.0,
    selected_groups = NULL, is_categorical_group = FALSE
  )
  args <- utils::modifyList(defaults, list(...))
  do.call(prepare_ternary_plot_data, args)
}

make_test_data <- function(n = 60, seed = 1) {
  set.seed(seed)
  data.frame(
    Al = abs(stats::rnorm(n, 10, 3)) + 1, Si = abs(stats::rnorm(n, 10, 3)) + 1,
    Mn = abs(stats::rnorm(n, 10, 3)) + 1, Fe = abs(stats::rnorm(n, 10, 3)) + 1,
    Ni = abs(stats::rnorm(n, 10, 3)) + 1
  )
}

test_that("Mahalanobis's detail lines (MDmean/MDthresh/stdMD/Method) are real separate lines in col3_text, not one comma-joined line", {
  pd <- build_pd(make_test_data(), use_mahalanobis = TRUE)
  col3_lines <- strsplit(pd$col3_text, "\n")[[1]]

  # Before the fix, col3_text's "Outlier Detection:" content was ONE
  # element with no "\n" of its own - grepl() for each detail label would
  # all match the SAME single line. After the fix, each is its own line.
  expect_true(any(grepl("^Outlier Detection:$", col3_lines)))
  expect_true(any(grepl("^Mahalanobis", col3_lines)))
  expect_true(any(grepl("^  MDmean:", col3_lines)))
  expect_true(any(grepl("^  MDthresh:", col3_lines)))
  expect_true(any(grepl("^  stdMD:", col3_lines)))
  # The real regression check: MDmean and MDthresh must NOT be on the same
  # line (that's exactly what the comma-join bug did).
  mdmean_line <- col3_lines[grepl("MDmean:", col3_lines)]
  expect_false(grepl("MDthresh:", mdmean_line))
})

test_that("Isolation Forest's detail lines (Trees/Contamination/Sample size) are real separate lines in col3_text", {
  pd <- build_pd(make_test_data(), use_isolation_forest = TRUE)
  col3_lines <- strsplit(pd$col3_text, "\n")[[1]]

  expect_true(any(grepl("^Outlier Detection:$", col3_lines)))
  expect_true(any(grepl("^Isolation Forest", col3_lines)))
  expect_true(any(grepl("^  Trees:", col3_lines)))
  expect_true(any(grepl("^  Contamination:", col3_lines)))
  expect_true(any(grepl("^  Sample size", col3_lines)))
  trees_line <- col3_lines[grepl("Trees:", col3_lines)]
  expect_false(grepl("Contamination:", trees_line))
})

test_that("notes_bottom_margin scales up for the tall Mahalanobis column and stays small when no method is active", {
  pd_short <- build_pd(make_test_data())
  pd_mahal <- build_pd(make_test_data(), use_mahalanobis = TRUE)

  short_lines <- length(strsplit(pd_short$col3_text, "\n")[[1]])
  mahal_lines <- length(strsplit(pd_mahal$col3_text, "\n")[[1]])
  expect_gt(mahal_lines, short_lines)
  expect_gt(pd_mahal$notes_bottom_margin, pd_short$notes_bottom_margin)
})

test_that("draw_plot_notes_column() draws one mtext() call per non-empty line, each with its OWN increasing `line=` position", {
  # This is the exact bug confirmed by direct rendering: a single mtext()
  # call on a "\n"-joined string anchors its LAST line at the given
  # `line=`, not its first - so columns of different lengths converged and
  # overlapped. Verified here via testthat's own sanctioned mocking of a
  # base graphics function (not this app's own statistics/plotting logic,
  # which this project's test suite deliberately never stubs - see
  # test-server-hex-ternary.R's own header comment) - the cleanest way to
  # confirm the exact `line=` value used per call without needing to
  # re-render and visually inspect a PNG for every regression run.
  calls <- list()
  testthat::local_mocked_bindings(
    mtext = function(text, side, line, cex, col, outer, adj) {
      calls[[length(calls) + 1]] <<- list(text = text, line = line)
    },
    .package = "graphics"
  )

  draw_plot_notes_column("Line A\nLine B\nLine C", side = 1, start_line = 3, cex = 0.55, col = "red", adj = 1)

  expect_equal(length(calls), 3)
  expect_equal(calls[[1]]$text, "Line A")
  expect_equal(calls[[2]]$text, "Line B")
  expect_equal(calls[[3]]$text, "Line C")
  # Strictly increasing, starting exactly at start_line - the top-anchor
  # fix, not the old bottom-anchor mtext() default.
  expect_equal(calls[[1]]$line, 3)
  expect_true(calls[[2]]$line > calls[[1]]$line)
  expect_true(calls[[3]]$line > calls[[2]]$line)
  # The documented step size (cex * 1.1), exactly.
  expect_equal(calls[[2]]$line, 3 + 0.55 * 1.1, tolerance = 1e-9)
  expect_equal(calls[[3]]$line, 3 + 2 * 0.55 * 1.1, tolerance = 1e-9)
})

test_that("draw_plot_notes_column() skips empty lines instead of drawing a blank mtext() call for them", {
  calls <- list()
  testthat::local_mocked_bindings(
    mtext = function(text, side, line, cex, col, outer, adj) {
      calls[[length(calls) + 1]] <<- list(text = text, line = line)
    },
    .package = "graphics"
  )

  draw_plot_notes_column("Line A\n\nLine C", side = 1, start_line = 2, cex = 0.6, col = "blue", adj = 0)

  expect_equal(length(calls), 2)
  expect_equal(calls[[1]]$text, "Line A")
  expect_equal(calls[[2]]$text, "Line C")
})

test_that("a real Mahalanobis-active plot renders end to end with no error after the fix (no regression)", {
  # Full pipeline check, not just the text-construction unit tests above -
  # confirms the fix doesn't break actual rendering (real device open/
  # close, real par(oma=...) with the new dynamic margin value).
  d <- make_test_data()
  xlsx_path <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(d, xlsx_path)
  out_dir <- tempfile("plot_notes_e2e_")
  dir.create(out_dir, recursive = TRUE)

  suppressWarnings(result <- general_ternary_plot(
    xlsx_file = xlsx_path, output_dir = out_dir,
    element_A = list(col = "Al"), element_B = list(col = "Si"), element_C = list(col = "Mn"),
    use_mahalanobis = TRUE, mahalanobis_reference = "self",
    selected_columns = c("Al", "Si", "Mn", "Fe", "Ni"),
    include_plot_notes = TRUE, preview = FALSE
  ))
  expect_true(file.exists(result))
  expect_gt(file.info(result)$size, 0)
})
