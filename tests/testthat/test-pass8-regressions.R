# Regression tests for pass 8 of the vidternary Structural Audit - one
# test_that() per finding fixed this pass, matching test-pass5/6/7-regressions.R's
# own established convention.

test_that("prepare_ternary_plot_data() stops with a clear error when more than one statistical/multivariate filter flag is TRUE (regression)", {
  # general_ternary_plot() and the Ternary Plots tab's own UI already
  # prevent this in the live app (see ternary_plot.R/server_ternary_plots.R),
  # but prepare_ternary_plot_data() is itself exported and directly
  # callable (as this package's own smoke tests and several other tests in
  # this suite do), bypassing both of those layers. It needs its own guard
  # so a direct caller with two conflicting flags set gets a clear error
  # instead of silently stacking filters.
  xlsx_path <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(data.frame(Al = c(1, 2, 3), Si = c(1, 2, 3), Mn = c(1, 2, 3)), xlsx_path)

  expect_error(
    prepare_ternary_plot_data(
      xlsx_file = xlsx_path, working_dir = getwd(), output_dir = tempfile(),
      element_A = list(col = "Al"), element_B = list(col = "Si"), element_C = list(col = "Mn"),
      optional_param1 = NULL, optional_param2 = NULL, color_palette = "blue",
      xlsx_display_name = NULL, preview = TRUE,
      use_mahalanobis = TRUE, reference_data = NULL,
      optional_param1_representation = "point_size", output_format = "png",
      use_isolation_forest = TRUE, isolation_ntrees = 200, isolation_contamination = 0.10,
      use_iqr_filter = FALSE, use_zscore_filter = FALSE, use_mad_filter = FALSE,
      stat_filter_log10 = FALSE,
      lambda = 1, omega = 0,
      keep_outliers_mahalanobis = FALSE, keep_outliers_isolation = FALSE,
      keep_outliers_iqr = FALSE, keep_outliers_zscore = FALSE, keep_outliers_mad = FALSE,
      individual_filters_A = NULL, individual_filters_B = NULL, individual_filters_C = NULL,
      custom_mdthresh = NULL, mdthresh_mode = "auto", mahalanobis_reference = "self",
      selected_columns = c("Al", "Si"),
      include_plot_notes = FALSE, use_manual_point_size = FALSE, manual_point_size = 1.0,
      selected_groups = NULL, is_categorical_group = FALSE
    ),
    regexp = "Only one statistical/multivariate outlier filter may be active"
  )
})

test_that("prepare_ternary_plot_data() runs normally when exactly one filter flag is TRUE (no false-positive from the new guard)", {
  xlsx_path <- tempfile(fileext = ".xlsx")
  set.seed(30)
  openxlsx::write.xlsx(
    data.frame(Al = abs(rnorm(20, 5, 1)), Si = abs(rnorm(20, 5, 1)), Mn = abs(rnorm(20, 5, 1))),
    xlsx_path
  )

  expect_no_error(
    prepare_ternary_plot_data(
      xlsx_file = xlsx_path, working_dir = getwd(), output_dir = tempfile(),
      element_A = list(col = "Al"), element_B = list(col = "Si"), element_C = list(col = "Mn"),
      optional_param1 = NULL, optional_param2 = NULL, color_palette = "blue",
      xlsx_display_name = NULL, preview = TRUE,
      use_mahalanobis = TRUE, reference_data = NULL,
      optional_param1_representation = "point_size", output_format = "png",
      use_isolation_forest = FALSE, isolation_ntrees = 200, isolation_contamination = 0.10,
      use_iqr_filter = FALSE, use_zscore_filter = FALSE, use_mad_filter = FALSE,
      stat_filter_log10 = FALSE,
      lambda = 1, omega = 0,
      keep_outliers_mahalanobis = FALSE, keep_outliers_isolation = FALSE,
      keep_outliers_iqr = FALSE, keep_outliers_zscore = FALSE, keep_outliers_mad = FALSE,
      individual_filters_A = NULL, individual_filters_B = NULL, individual_filters_C = NULL,
      custom_mdthresh = NULL, mdthresh_mode = "auto", mahalanobis_reference = "self",
      selected_columns = c("Al", "Si"),
      include_plot_notes = FALSE, use_manual_point_size = FALSE, manual_point_size = 1.0,
      selected_groups = NULL, is_categorical_group = FALSE
    )
  )
})
