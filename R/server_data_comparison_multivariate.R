# ---- Server Data Comparison Module: Multivariate Analysis ----
# Split out of server_data_comparison.R: the Mahalanobis Distance /
# Isolation Forest button handlers, plus the unified comprehensive
# multivariate results display (mahalanobis_info).

register_data_comparison_multivariate_handlers <- function(input, output, session, rv, show_message, log_operation) {

  observeEvent(input$mahalanobis_analysis, {
    req(rv$df1, rv$df2, input$multivariate_columns)
    tryCatch({
      # Use columns selected in the Universal Column Selector
      selected_cols <- input$multivariate_columns

      # Validate that selected columns exist in both datasets
      numeric_cols1 <- sapply(rv$df1, is.numeric)
      numeric_cols2 <- sapply(rv$df2, is.numeric)
      available_cols1 <- colnames(rv$df1)[numeric_cols1]
      available_cols2 <- colnames(rv$df2)[numeric_cols2]

      # Check if all selected columns exist in both datasets
      missing_cols1 <- setdiff(selected_cols, available_cols1)
      missing_cols2 <- setdiff(selected_cols, available_cols2)

      if (length(missing_cols1) > 0 || length(missing_cols2) > 0) {
        error_msg <- "Selected columns not found in datasets:\n"
        if (length(missing_cols1) > 0) {
          error_msg <- paste(error_msg, "Missing in Dataset 1:", paste(missing_cols1, collapse = ", "), "\n")
        }
        if (length(missing_cols2) > 0) {
          error_msg <- paste(error_msg, "Missing in Dataset 2:", paste(missing_cols2, collapse = ", "), "\n")
        }
        output$mahalanobis_output <- renderText(error_msg)
        return()
      }

      if (length(selected_cols) < 2) {
        output$mahalanobis_output <- renderText("Need at least 2 columns selected in the Universal Column Selector")
        return()
      }

      output$mahalanobis_output <- renderPrint({
        cat("=== MAHALANOBIS DISTANCE ANALYSIS ===\n")
        cat("Selected columns from Universal Column Selector:", paste(selected_cols, collapse = ", "), "\n")
        cat("Dataset 1 rows:", nrow(rv$df1), "\n")
        cat("Dataset 2 rows:", nrow(rv$df2), "\n\n")

        # Additional data validation before analysis
        data1_subset <- rv$df1[, selected_cols, drop = FALSE]
        data2_subset <- rv$df2[, selected_cols, drop = FALSE]

        # Check for missing values and remove them
        complete_cases1 <- complete.cases(data1_subset)
        complete_cases2 <- complete.cases(data2_subset)

        if (sum(complete_cases1) < 2 || sum(complete_cases2) < 2) {
          cat("ERROR: Not enough complete cases for analysis.\n")
          cat("Dataset 1 complete cases:", sum(complete_cases1), "\n")
          cat("Dataset 2 complete cases:", sum(complete_cases2), "\n")
          cat("Need at least 2 complete cases per dataset for Mahalanobis.\n")
          return()
        }

        # Clean data by removing incomplete cases
        data1_clean <- data1_subset[complete_cases1, , drop = FALSE]
        data2_clean <- data2_subset[complete_cases2, , drop = FALSE]

        cat("Using clean data:\n")
        cat("Dataset 1 clean rows:", nrow(data1_clean), "\n")
        cat("Dataset 2 clean rows:", nrow(data2_clean), "\n\n")

        # Perform basic Mahalanobis analysis
        result <- compute_mahalanobis_distance(
          data1_clean,
          data2_clean,
          lambda = 1,
          omega = 0,
          keep_outliers = FALSE,
          custom_mdthresh = NULL,
          selected_columns = selected_cols,
          mdthresh_mode = "auto"
        )

        if (!is.null(result)) {
          cat("✅ Analysis completed successfully!\n\n")
          cat("Threshold method:", result$threshold_method, "\n")
          cat("Threshold value:", round(result$MDthresh, 3), "\n")
          cat("Total points analyzed:", result$total_points, "\n")
          cat("Outliers detected:", result$outlier_custom, "\n")
          cat("Outlier percentage:", round(result$outlier_custom / result$total_points * 100, 1), "%\n")
          cat("Degrees of freedom:", result$df, "\n")
          cat("MDmean:", round(result$MDmean, 3), "\n")
          cat("stdMD:", round(result$stdMD, 3), "\n")

          if (!is.null(result$threshold_formula)) {
            cat("\nThreshold formula:", result$threshold_formula, "\n")
          }
        } else {
          cat("❌ Analysis failed. Please check data quality.\n")
        }
      })

      log_operation("SUCCESS", "Mahalanobis analysis completed", paste("Analyzed", length(selected_cols), "variables"))

    }, error = function(e) {
      output$mahalanobis_output <- renderText(paste("Error in Mahalanobis analysis:", e$message))
      log_operation("ERROR", "Mahalanobis analysis failed", e$message)
    })
  })

  # Robust Mahalanobis analysis removed

  observeEvent(input$isolation_forest_analysis, {
    req(rv$df1, rv$df2, input$multivariate_columns)
    tryCatch({
      # Use columns selected in the Universal Column Selector
      selected_cols <- input$multivariate_columns

      # Validate that selected columns exist in both datasets
      numeric_cols1 <- sapply(rv$df1, is.numeric)
      numeric_cols2 <- sapply(rv$df2, is.numeric)
      available_cols1 <- colnames(rv$df1)[numeric_cols1]
      available_cols2 <- colnames(rv$df2)[numeric_cols2]

      # Check if all selected columns exist in both datasets
      missing_cols1 <- setdiff(selected_cols, available_cols1)
      missing_cols2 <- setdiff(selected_cols, available_cols2)

      if (length(missing_cols1) > 0 || length(missing_cols2) > 0) {
        error_msg <- "Selected columns not found in datasets:\n"
        if (length(missing_cols1) > 0) {
          error_msg <- paste(error_msg, "Missing in Dataset 1:", paste(missing_cols1, collapse = ", "), "\n")
        }
        if (length(missing_cols2) > 0) {
          error_msg <- paste(error_msg, "Missing in Dataset 2:", paste(missing_cols2, collapse = ", "), "\n")
        }
        output$isolation_forest_output <- renderText(error_msg)
        return()
      }

      if (length(selected_cols) < 2) {
        output$isolation_forest_output <- renderText("Need at least 2 columns selected in the Universal Column Selector")
        return()
      }

      output$isolation_forest_output <- renderPrint({
        cat("=== ISOLATION FOREST ANALYSIS ===\n")
        cat("Selected columns from Universal Column Selector:", paste(selected_cols, collapse = ", "), "\n")
        cat("Dataset 1 rows:", nrow(rv$df1), "\n")
        cat("Dataset 2 rows:", nrow(rv$df2), "\n\n")

        # Perform Isolation Forest analysis
        result <- compute_isolation_forest(
          rv$df1[, selected_cols, drop = FALSE],
          rv$df2[, selected_cols, drop = FALSE],
          selected_columns = selected_cols,
          keep_outliers = FALSE
        )

        if (!is.null(result)) {
          cat("✅ Analysis completed successfully!\n\n")
          cat("Threshold method:", result$threshold_method, "\n")
          cat("Threshold value:", round(result$threshold, 3), "\n")
          cat("Total points analyzed:", result$total_points, "\n")
          cat("Outliers detected:", result$outlier_count, "\n")
          cat("Outlier percentage:", round(result$outlier_count / result$total_points * 100, 1), "%\n")
        } else {
          cat("❌ Analysis failed. Please check data quality.\n")
        }
      })

      log_operation("SUCCESS", "Isolation Forest analysis completed", paste("Analyzed", length(selected_cols), "variables"))

    }, error = function(e) {
      output$isolation_forest_output <- renderText(paste("Error in Isolation Forest analysis:", e$message))
      log_operation("ERROR", "Isolation Forest analysis failed", e$message)
    })
  })

  # ---- Comprehensive Multivariate Analysis Display ----

  # Unified multivariate analysis results display
  output$mahalanobis_info <- renderPrint({
    req(rv$df1, rv$df2, input$multivariate_columns)

    # Use columns selected in the Universal Column Selector
    selected_cols <- input$multivariate_columns

    # Validate that selected columns exist in both datasets
    numeric_cols1 <- sapply(rv$df1, is.numeric)
    numeric_cols2 <- sapply(rv$df2, is.numeric)
    available_cols1 <- colnames(rv$df1)[numeric_cols1]
    available_cols2 <- colnames(rv$df2)[numeric_cols2]

    # Check if all selected columns exist in both datasets
    missing_cols1 <- setdiff(selected_cols, available_cols1)
    missing_cols2 <- setdiff(selected_cols, available_cols2)

    if (length(missing_cols1) > 0 || length(missing_cols2) > 0) {
      cat("❌ Selected columns not found in datasets:\n")
      if (length(missing_cols1) > 0) {
        cat("Missing in Dataset 1:", paste(missing_cols1, collapse = ", "), "\n")
      }
      if (length(missing_cols2) > 0) {
        cat("Missing in Dataset 2:", paste(missing_cols2, collapse = ", "), "\n")
      }
      cat("Please select valid columns in the Universal Column Selector.\n")
      return()
    }

    if (length(selected_cols) < 2) {
      cat("❌ Multivariate analysis not available. Please ensure:\n")
      cat("- Both datasets are loaded\n")
      cat("- At least 2 columns are selected in the Universal Column Selector\n")
      cat("- At least one multivariate method is selected\n")
      return()
    }

          # Get the analysis result using the multivariate_analysis function
      tryCatch({
        result <- multivariate_analysis(
          use_mahalanobis = TRUE,
          use_isolation_forest = TRUE,
          lambda = if (!is.null(input$lambda)) input$lambda else 1,
          omega = if (!is.null(input$omega)) input$omega else 0,
          custom_mdthresh = if (!is.null(input$custom_mdthresh) && !is.null(input$mdthresh_mode) && input$mdthresh_mode == "manual") input$custom_mdthresh else NULL,
          mdthresh_mode = if (!is.null(input$mdthresh_mode)) input$mdthresh_mode else "auto",
          selected_columns = selected_cols,
          xlsx_file1 = input$xlsx_file1,
          xlsx_file2 = input$xlsx_file2
        )

      if (!is.null(result)) {
        cat("=== MULTIVARIATE ANALYSIS RESULTS ===\n\n")

        if (result$method == "Mahalanobis Distance") {
          cat("Method:", result$method, "\n")
          cat("Total points analyzed:", result$total_points, "\n")
          cat("Outlier count (95% threshold):", result$outlier_count, "\n")
          cat("Outlier count (99% threshold):", result$outlier_99, "\n")
          cat("95% threshold value:", round(result$threshold_95, 3), "\n")
          cat("99% threshold value:", round(result$threshold_99, 3), "\n")
          cat("Columns used:", paste(result$common_cols, collapse = ", "), "\n")
          if (!is.null(result$robust_center)) {
            cat("Robust center (first 3 values):", paste(round(result$robust_center[seq_len(min(3, length(result$robust_center)))], 3), collapse = ", "), "\n")
          }
        } else if (result$method == "Isolation Forest") {
          cat("🌲 Isolation Forest Analysis:\n")
          cat("Method:", result$method, "\n")
          cat("Total points analyzed:", result$total_points, "\n")
          cat("Outlier count:", result$outlier_count, "\n")
          cat("Contamination rate:", result$contamination, "\n")
          cat("Threshold value:", round(result$threshold, 3), "\n")
          cat("Columns used:", paste(result$common_cols, collapse = ", "), "\n")
          cat("Score range:", round(result$score_range[1], 3), "to", round(result$score_range[2], 3), "\n")
          cat("Score mean:", round(result$score_mean, 3), "\n")
          cat("Score std dev:", round(result$score_sd, 3), "\n")
        } else {
          cat("📊 Standard Mahalanobis Distance Analysis:\n")
          cat("Total points analyzed:", result$total_points, "\n")
          cat("Degrees of freedom:", result$df, "\n")
          cat("Columns used:", paste(result$common_cols, collapse = ", "), "\n")
          cat("MDmean:", round(result$MDmean, 3), "\n")
          cat("stdMD:", round(result$stdMD, 3), "\n")

          if (!is.null(input$mdthresh_mode) && input$mdthresh_mode == "manual") {
            cat("Threshold mode: Manual\n")
            cat("Custom MDthresh:", round(result$MDthresh, 3), "\n")
          } else {
            cat("Threshold mode: Automatic (MDthresh=MDmean+√(100/(100+λ-ω))×stdMD)\n")
            if (!is.null(input$lambda) && !is.null(input$omega)) {
              cat("λ:", input$lambda, "ω:", input$omega, "\n")
            }
            cat("Calculated MDthresh:", round(result$MDthresh, 3), "\n")
            if (!is.null(result$threshold_formula)) {
              cat("Formula breakdown:", result$threshold_formula, "\n")
            }
          }

          cat("\n📈 Threshold Comparison:\n")
          cat("Points above 95% threshold:", result$outlier_95, "\n")
          cat("Points above 99% threshold:", result$outlier_99, "\n")
          if (!is.null(result$outlier_custom)) {
            cat("Points above custom threshold:", result$outlier_custom, "\n")
          }
          if (!is.null(result$p_values)) {
            cat("P-value range:", round(min(result$p_values), 4), "to", round(max(result$p_values), 4), "\n")
          }
        }

        cat("\n💡 Interpretation:\n")
        if (result$method == "Mahalanobis Distance") {
          cat("- Robust MCD is less sensitive to outliers in the reference dataset\n")
          cat("- Good for non-normal distributions and contaminated data\n")
        } else if (result$method == "Isolation Forest") {
          cat("- Isolation Forest detects anomalies based on data isolation\n")
          cat("- Threshold automatically set to top contamination% of scores\n")
          cat("- Good for high-dimensional data and non-linear relationships\n")
        } else {
          cat("- Standard Mahalanobis assumes multivariate normal distribution\n")
          if (!is.null(input$mdthresh_mode) && input$mdthresh_mode == "auto") {
            cat("- λ controls strictness: higher = stricter threshold\n")
            cat("- ω provides flexibility: higher = more lenient threshold\n")
          }
        }
      } else {
        cat("❌ Multivariate analysis failed to complete\n")
        cat("Please check data quality and try again\n")
      }

    }, error = function(e) {
      cat("❌ Error in multivariate analysis:", e$message, "\n")
      cat("Please check data quality and try again\n")
      log_operation("ERROR", "Comprehensive multivariate analysis failed", e$message)
    })
  })
}
