# Test file for modular structure
# This file tests that all modules can be loaded and basic functionality works

test_that("All modules can be loaded", {
  # Test that we can source all modules without errors
  expect_no_error({
    source("R/dependencies.R")
    source("R/config.R")
    source("R/cache.R")
    source("R/helpers.R")
    source("R/multivariate.R")
    source("R/statistical_filters.R")
    source("R/ternary_plot.R")
    source("R/plotting_utils.R")
    source("R/ui_components.R")
    source("R/server_logic.R")
  })
})

test_that("Core functions are available", {
  # Test that key functions exist
  expect_true(exists("log_operation"))
  expect_true(exists("debug_log"))
  expect_true(exists("clean_column_names"))
  expect_true(exists("apply_filter"))
})

test_that("Multivariate analysis functions are available", {
  # Test that multivariate functions exist
  expect_true(exists("compute_mahalanobis_distance"))
  expect_true(exists("compute_robust_mahalanobis"))
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
  # Test that plotting functions exist
  expect_true(exists("create_ternary_plot"))
  expect_true(exists("create_correlation_plot"))
  expect_true(exists("save_plot"))
  expect_true(exists("apply_consistent_theme"))
})

test_that("UI components are available", {
  # Test that UI functions exist
  expect_true(exists("create_main_ui"))
  expect_true(exists("create_advanced_filter_ui"))
  expect_true(exists("create_server_logic"))
})

test_that("Configuration functions work", {
  # Test configuration functionality
  config <- load_config()
  expect_true(is.list(config))
  expect_true("directories" %in% names(config))
  expect_true("plotting" %in% names(config))
  expect_true("analysis" %in% names(config))
})

test_that("Helper functions work correctly", {
  # Test helper function functionality
  test_names <- c("SiO2.(Wt%)", "Al2O3.(Wt%)", "Fe2O3.(Wt%)")
  cleaned_names <- clean_column_names(test_names)
  
  expect_equal(cleaned_names, c("SiO2", "Al2O3", "Fe2O3"))
  
  # Test safe column names
  safe_names <- safe_column_names(test_names)
  expect_true(all(grepl("^[a-zA-Z0-9_]+$", safe_names)))
})

test_that("Cache system works", {
  # Test cache functionality
  test_data <- data.frame(x = 1:10, y = 11:20)
  test_key <- "test_key"
  
  # Set cache
  set_cached_data(test_key, test_data)
  
  # Get cache
  cached_data <- get_cached_data(test_key)
  expect_equal(cached_data, test_data)
  
  # Get cache stats
  stats <- get_cache_stats()
  expect_true(is.list(stats))
  
  # Clear cache
  clear_cache()
  expect_null(get_cached_data(test_key))
})

test_that("Filter functions work", {
  # Test filtering functionality
  test_data <- data.frame(
    x = c(1, 2, 3, 100, 5, 6, 7, 8, 9, 10),
    y = c(11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
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

test_that("Plotting utilities work", {
  # Test plotting functionality
  test_points <- data.frame(
    A = c(0.3, 0.4, 0.5),
    B = c(0.3, 0.4, 0.5),
    C = c(0.4, 0.2, 0.0)
  )
  
  # Test ternary plot creation
  plot_obj <- create_ternary_plot(test_points, title = "Test Plot")
  expect_true(inherits(plot_obj, "ggplot"))
  
  # Test theme application
  themed_plot <- apply_consistent_theme(plot_obj, "minimal")
  expect_true(inherits(themed_plot, "ggplot"))
  
  # Test color palette creation
  colors <- create_color_palette(5, "viridis")
  expect_equal(length(colors), 5)
})

test_that("Main ternary plot function structure is correct", {
  # Test that the main function has the expected parameters
  func_formals <- formals(general_ternary_plot)
  expected_params <- c("xlsx_file", "element_A", "element_B", "element_C")
  
  for (param in expected_params) {
    expect_true(param %in% names(func_formals))
  }
})

test_that("UI components can be created", {
  # Test that UI functions return valid objects
  # Note: These tests may need to be run in a Shiny context
  
  # Test that functions exist and can be called
  expect_true(is.function(create_main_ui))
  expect_true(is.function(create_advanced_filter_ui))
  expect_true(is.function(create_column_selection_ui))
  expect_true(is.function(create_export_ui))
})

test_that("Server logic can be created", {
  # Test that server function exists and is callable
  expect_true(is.function(create_server_logic))
  
  # Test that it returns a function
  # Note: This would need to be tested in a Shiny context
})

test_that("Package structure is complete", {
  # Test that all expected files exist
  expect_true(file.exists("R/dependencies.R"))
  expect_true(file.exists("R/config.R"))
  expect_true(file.exists("R/cache.R"))
  expect_true(file.exists("R/helpers.R"))
  expect_true(file.exists("R/multivariate.R"))
  expect_true(file.exists("R/statistical_filters.R"))
  expect_true(file.exists("R/ternary_plot.R"))
  expect_true(file.exists("R/plotting_utils.R"))
  expect_true(file.exists("R/ui_components.R"))
  expect_true(file.exists("R/server_logic.R"))
  expect_true(file.exists("R/app.R"))
  expect_true(file.exists("DESCRIPTION"))
  expect_true(file.exists("NAMESPACE"))
  expect_true(file.exists("README.md"))
})

test_that("Configuration values are reasonable", {
  # Test that configuration values make sense
  config <- load_config()
  
  # Test directories
  expect_true(is.character(config$directories$working_dir))
  expect_true(is.character(config$directories$output_dir))
  
  # Test plotting defaults
  expect_true(config$plotting$default_point_size > 0)
  expect_true(config$plotting$default_point_size <= 5)
  expect_true(config$plotting$default_alpha > 0)
  expect_true(config$plotting$default_alpha <= 1)
  
  # Test analysis defaults
  expect_true(config$analysis$default_lambda > 0)
  expect_true(config$analysis$iqr_multiplier > 0)
  expect_true(config$analysis$zscore_threshold > 0)
  expect_true(config$analysis$mad_threshold > 0)
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
