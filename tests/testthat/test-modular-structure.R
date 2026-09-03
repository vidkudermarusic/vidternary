# Test file for modular structure
# This file tests that all modules can be loaded and basic functionality works

test_that("Core functions are available", {
  # Test that key functions exist
  expect_true(exists("log_operation"))
  expect_true(exists("debug_log"))
  expect_true(exists("clean_column_names"))
  expect_true(exists("apply_filter"))
})

test_that("Multivariate analysis functions are available", {
  # Test that multivariate functions exist
  # Note: compute_robust_mahalanobis was deliberately removed (see
  # multivariate.R:372 - "use standard Mahalanobis instead"), not renamed.
  expect_true(exists("compute_mahalanobis_distance"))
  expect_true(exists("compute_isolation_forest"))
  expect_true(exists("validate_mahalanobis_inputs"))
})

test_that("Statistical filtering functions are available", {
  # Test that filtering functions exist
  expect_true(exists("apply_iqr_filter"))
  expect_true(exists("apply_zscore_filter"))
  expect_true(exists("apply_mad_filter"))
})

test_that("Plotting functions are available", {
  # Test that plotting functions exist. create_ternary_plot()/save_plot()/
  # apply_consistent_theme() were removed as confirmed-dead code (the app's
  # real ternary diagrams are built via general_ternary_plot()/the Ternary
  # package, not these ggplot2 helpers) - create_correlation_plot() is the
  # one genuinely wired into the Data Comparison tab.
  expect_true(exists("create_correlation_plot"))
})

test_that("UI components are available", {
  # Test that UI functions exist
  # Note: create_advanced_filter_ui/create_column_selection_ui/
  # create_export_ui were removed - filter/column/export UI now lives
  # inline in each ui_<tab>_tab.R file as part of the tab-based refactor
  # (confirmed: ui_components.R now only defines create_main_ui/cite_link).
  expect_true(exists("create_main_ui"))
  expect_true(exists("create_server_logic"))
})

test_that("Helper functions work correctly", {
  # Test helper function functionality. safe_column_names() was removed as
  # confirmed-dead code (zero app-code callers) in the same pass.
  test_names <- c("SiO2.(Wt%)", "Al2O3.(Wt%)", "Fe2O3.(Wt%)")
  cleaned_names <- clean_column_names(test_names)

  expect_equal(cleaned_names, c("SiO2", "Al2O3", "Fe2O3"))
})

test_that("Filter functions work", {
  # Test filtering functionality
  # Note: the z-score case needs more than the original 10 rows. For a
  # sample of size n, the largest z-score any single point can reach is
  # bounded by (n-1)/sqrt(n) - at n=10 that bound is ~2.85, which is
  # *always* below the default threshold of 3, no matter how extreme the
  # outlier value is (verified: even x=1000 among 9 small values still
  # only reaches z~2.85). That's a property of z-scores on small samples,
  # not a bug in apply_zscore_filter() - a larger n is needed for the
  # outlier to actually cross the threshold.
  test_data <- data.frame(
    x = c(1:29, 1000),
    y = 101:130
  )

  # Test IQR filter
  iqr_filtered <- apply_iqr_filter(test_data, c("x", "y"), keep_outliers = FALSE)
  expect_true(nrow(iqr_filtered) < nrow(test_data))

  # Test Z-score filter
  zscore_filtered <- apply_zscore_filter(test_data, c("x", "y"), keep_outliers = FALSE)
  expect_true(nrow(zscore_filtered) < nrow(test_data))

  # Test MAD filter
  mad_filtered <- apply_mad_filter(test_data, c("x", "y"), keep_outliers = FALSE)
  expect_true(nrow(mad_filtered) < nrow(test_data))
})

test_that("Main ternary plot function structure is correct", {
  # Test that the main function has the expected parameters
  func_formals <- formals(general_ternary_plot)
  expected_params <- c("xlsx_file", "element_A", "element_B", "element_C")
  
  for (param in expected_params) {
    expect_true(param %in% names(func_formals))
  }
})

test_that("Server logic can be created", {
  # Test that server function exists and is callable
  expect_true(is.function(create_server_logic))
  
  # Test that it returns a function
  # Note: This would need to be tested in a Shiny context
})

test_that("Package structure is complete", {
  # Test that all expected files exist
  # Note: testthat runs test files with the working directory set to
  # tests/testthat/, not the package root (verified empirically) - these
  # paths were relative to the package root and so always resolved to
  # FALSE when actually run through devtools::test()/test_check(), not
  # just when invoked a particular way.
  pkg_root <- file.path("..", "..")
  expect_true(file.exists(file.path(pkg_root, "R", "dependencies.R")))
  expect_true(file.exists(file.path(pkg_root, "R", "helpers.R")))
  expect_true(file.exists(file.path(pkg_root, "R", "multivariate.R")))
  expect_true(file.exists(file.path(pkg_root, "R", "statistical_filters.R")))
  expect_true(file.exists(file.path(pkg_root, "R", "ternary_plot.R")))
  expect_true(file.exists(file.path(pkg_root, "R", "plotting_utils.R")))
  expect_true(file.exists(file.path(pkg_root, "R", "ui_components.R")))
  expect_true(file.exists(file.path(pkg_root, "R", "server_logic.R")))
  expect_true(file.exists(file.path(pkg_root, "R", "app.R")))
  expect_true(file.exists(file.path(pkg_root, "DESCRIPTION")))
  expect_true(file.exists(file.path(pkg_root, "NAMESPACE")))
  expect_true(file.exists(file.path(pkg_root, "README.md")))
})

test_that("Error handling works", {
  # Test that functions handle errors gracefully
  
  # Test with invalid data
  expect_error(apply_iqr_filter(NULL, "x"))
  expect_error(apply_zscore_filter(NULL, "x"))
  expect_error(apply_mad_filter(NULL, "x"))
  
  # Test with invalid parameters
  expect_error(apply_iqr_filter(data.frame(x = 1:5), "x", multiplier = -1))
  expect_error(apply_zscore_filter(data.frame(x = 1:5), "x", threshold = -1))
  expect_error(apply_mad_filter(data.frame(x = 1:5), "x", threshold = -1))
})

test_that("Debug logging works", {
  # Test debug logging functionality
  
  # Enable debug mode
  options(ternary.debug = TRUE)
  
  # Test debug log function
  expect_no_error(debug_log("Test message"))
  expect_no_error(debug_log("Test with %s", "parameter"))
  
  # Test log operation function
  expect_no_error(log_operation("Test", "Test operation"))
  
  # Disable debug mode
  options(ternary.debug = FALSE)
})

print("All modular structure tests completed successfully!")
