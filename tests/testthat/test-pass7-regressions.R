# Regression tests for pass 7 of the vidternary Structural Audit - one
# test_that() per finding fixed this pass, matching test-pass5/6-regressions.R's
# own established convention (deliberately narrow, not a general coverage push).

# Mirrors test-plot-notes-formatting.R's own build_pd() helper (kept local to
# that file, so redefined here rather than shared) - a valid
# prepare_ternary_plot_data() call with sensible defaults, overridden per test.
build_pd <- function(d, ...) {
  xlsx_path <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(d, xlsx_path)
  out_dir <- tempfile("pass7_test_")
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
  args <- utils::modifyList(defaults, list(...))
  do.call(prepare_ternary_plot_data, args)
}

test_that("prepare_ternary_plot_data() curates an external reference dataset with the same element filter as the target before Mahalanobis calibration (regression)", {
  # The bug: mahalanobis_reference = "dataset1"/"dataset2" fed the RAW
  # uploaded reference straight into compute_mahalanobis_distance(), never
  # running it through the same element/optional-parameter/statistical
  # filters that built M - so a reference containing rows the analyst had
  # explicitly excluded from the plotted data (e.g. a different phase, an
  # out-of-range reading) silently distorted the reference distribution the
  # threshold is calibrated against. Fixed by curating reference_data with
  # the exact same apply_element_and_parameter_filters()/
  # apply_statistical_filtering() calls M itself goes through, before it
  # reaches apply_multivariate_filtering().
  set.seed(21)
  target <- data.frame(
    Al = abs(rnorm(30, 5, 1)), Si = abs(rnorm(30, 5, 1)), Mn = abs(rnorm(30, 5, 1))
  )

  # Raw "Dataset 2" reference: 40 rows matching the target's own population,
  # PLUS 10 "junk" rows with Al two orders of magnitude out of range - the
  # kind of row the element filter below is meant to exclude, but which a
  # raw/uncurated reference would still include.
  clean_ref_rows <- data.frame(Al = abs(rnorm(40, 5, 1)), Si = abs(rnorm(40, 5, 1)))
  junk_ref_rows <- data.frame(Al = abs(rnorm(10, 500, 20)), Si = abs(rnorm(10, 5, 1)))
  raw_reference <- rbind(clean_ref_rows, junk_ref_rows)

  # Element A filter that both the target (already within range) and the
  # reference (needs the junk rows stripped) should be judged against.
  individual_filters_A <- list(Al = "<50")

  pd <- build_pd(
    target,
    use_mahalanobis = TRUE, mahalanobis_reference = "dataset2",
    reference_data = raw_reference,
    individual_filters_A = individual_filters_A,
    selected_columns = c("Al", "Si")
  )

  expect_false(is.null(pd$mahal_result))

  # What SHOULD have been used: the reference with the same "<50" element
  # filter applied - i.e. the 40 clean rows only.
  expected <- compute_mahalanobis_distance(
    target, clean_ref_rows, selected_columns = c("Al", "Si")
  )
  # What the OLD (pre-fix) behavior would have used: the raw, unfiltered
  # 50-row reference, junk included.
  old_buggy <- compute_mahalanobis_distance(
    target, raw_reference, selected_columns = c("Al", "Si")
  )

  expect_equal(pd$mahal_result$MDmean, expected$MDmean, tolerance = 1e-8)
  expect_equal(pd$mahal_result$stdMD, expected$stdMD, tolerance = 1e-8)
  # Confirms the fix actually changes behavior versus the raw reference -
  # not just that both happen to produce a similar number.
  expect_false(isTRUE(all.equal(pd$mahal_result$MDmean, old_buggy$MDmean)))
})

test_that("prepare_ternary_plot_data() leaves self-reference mode unaffected by reference-curation (regression)", {
  set.seed(22)
  target <- data.frame(
    Al = abs(rnorm(30, 5, 1)), Si = abs(rnorm(30, 5, 1)), Mn = abs(rnorm(30, 5, 1))
  )
  pd <- build_pd(
    target,
    use_mahalanobis = TRUE, mahalanobis_reference = "self",
    individual_filters_A = list(Al = "<50"),
    selected_columns = c("Al", "Si")
  )
  expect_false(is.null(pd$mahal_result))
  # Self mode: reference is M itself AS IT STOOD right when
  # apply_multivariate_filtering() ran (after element filtering, before the
  # Mahalanobis step's own outlier removal trims M further) - not pd$M at
  # the very end of the pipeline, which by then reflects additional row
  # removal (outliers, and compute_ternary_coordinates()'s own valid-rows
  # mask). Reconstructed here the same way M itself was built, via the same
  # element-filtering function, for a like-for-like comparison.
  target_filtered <- apply_element_and_parameter_filters(
    M = target, element_A = list(col = "Al"), element_B = list(col = "Si"), element_C = list(col = "Mn"),
    individual_filters_A = list(Al = "<50"), individual_filters_B = NULL, individual_filters_C = NULL,
    optional_param1 = NULL, optional_param2 = NULL, preview = TRUE
  )$M
  expected <- compute_mahalanobis_distance(target_filtered, target_filtered, selected_columns = c("Al", "Si"))
  expect_equal(pd$mahal_result$MDmean, expected$MDmean, tolerance = 1e-8)
  expect_equal(pd$mahal_result$stdMD, expected$stdMD, tolerance = 1e-8)
})

test_that("prepare_ternary_plot_data() falls back to the raw reference dataset (no crash) when it lacks a column the target's own filters need (regression)", {
  set.seed(23)
  target <- data.frame(
    Al = abs(rnorm(20, 5, 1)), Si = abs(rnorm(20, 5, 1)), Mn = abs(rnorm(20, 5, 1))
  )
  # Reference dataset genuinely lacking the "Al" column the element filter
  # needs - curation must fail gracefully and fall back to the raw
  # reference rather than propagating an error out of prepare_ternary_plot_data().
  reference_missing_col <- data.frame(Si = abs(rnorm(15, 5, 1)), Other = abs(rnorm(15, 5, 1)))

  # No crash - but Isolation Forest can't run on this reference, so it must
  # warn and the plot must NOT claim the data was filtered (pass 9).
  expect_warning(
    pd <- build_pd(
      target,
      use_mahalanobis = FALSE, use_isolation_forest = TRUE,
      mahalanobis_reference = "dataset2", reference_data = reference_missing_col,
      individual_filters_A = list(Al = "<50"),
      selected_columns = c("Al", "Si")
    ),
    "NOT applied"
  )
  expect_equal(pd$mv_status, "failed")
  expect_match(pd$plot_title, "Isolation Forest \\(NOT APPLIED\\)")
  expect_false(grepl("(filtered)", pd$plot_title, fixed = TRUE))
})
