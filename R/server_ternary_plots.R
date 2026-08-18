# ---- Server Ternary Plots Module ----
# This module contains the single-file ternary plot generation
# functionality: parameter building, live previews, the analysis-report
# text, and the Save Plot 1/2/Both handlers.
#
# Related handlers live in sibling modules, split out for size:
#   server_ternary_plots_batch.R  - "Multiple Ternary Creator" batch preview/save
#   server_ternary_plots_groups.R - categorical group-selection UI
#
# BUGFIX (as part of this split): generate_analysis_report() below was
# previously missing its closing brace, so everything that followed it in
# the original single file - the analysis report renderer, the Save Plot
# buttons, and the group-selection UI - was accidentally nested inside its
# body and never executed. That's fixed here: the function now closes right
# after its return(), and the code that used to trail it is registered
# properly (below, and in server_ternary_plots_groups.R).

create_server_ternary_plots <- function(input, output, session, rv, show_message, log_operation, filter_management = NULL, directory_management = NULL) {

  # ---- Helper Functions ----

  # Function to collect individual element filters for main ternary plots tab
  # Now centralized in helpers.R as collect_main_ternary_filters

  # Simplified parameter extraction using unified function
  build_ternary_plot_params <- function(dataset_num, preview = FALSE) {
    req(rv[[paste0("df", dataset_num)]])
    params <- extract_ternary_params(input, rv, dataset_num, preview, directory_management, multiple_mode = FALSE)

    # Add the original filename for proper plot titles
    if (dataset_num == 1 && !is.null(input$xlsx_file1)) {
      params$xlsx_display_name <- input$xlsx_file1$name
    } else if (dataset_num == 2 && !is.null(input$xlsx_file2)) {
      params$xlsx_display_name <- input$xlsx_file2$name
    }

    params
  }

  # ---- Ternary Plot Previews ----

  # Dataset 1 ternary preview
  output$ternary_preview1 <- renderPlot({
    req(input$xlsx_file1)
    req(input$element_A1, input$element_B1, input$element_C1)

    if (getOption("ternary.debug", FALSE)) {
      cat("=== PREVIEW 1 DEBUGGING START ===\n")
      cat("DEBUG: Starting ternary preview 1\n")
      cat("DEBUG: Dataset 1 dimensions:", nrow(rv$df1), ncol(rv$df1), "\n")
      cat("DEBUG: Available columns:", paste(names(rv$df1), collapse = ", "), "\n")
    }

    tryCatch({
      # Build parameters for ternary plot
      params <- build_ternary_plot_params(1, TRUE)

      if (is.null(params)) {
        if (getOption("ternary.debug", FALSE)) cat("DEBUG: build_ternary_plot_params returned NULL\n")
        return()
      }

      if (getOption("ternary.debug", FALSE)) {
        cat("DEBUG: Parameters built successfully\n")
        cat("DEBUG: About to call general_ternary_plot\n")
      }

      # Call the main ternary plot function directly
      result <- do.call(general_ternary_plot, params)

      if (getOption("ternary.debug", FALSE)) {
        cat("DEBUG: general_ternary_plot returned:", result, "\n")
        cat("DEBUG: Preview mode - plot created on device\n")
      }

    }, error = function(e) {
      if (getOption("ternary.debug", FALSE)) {
        cat("DEBUG: Error in ternary preview 1:", e$message, "\n")
        cat("DEBUG: Error call:", toString(e$call), "\n")
      }
      log_operation("ERROR", "Ternary preview 1 failed", e$message)
      # Show a simple error plot
      plot(1, 1, type = "n", xlab = "", ylab = "", main = paste("Error:", e$message))
    })

    if (getOption("ternary.debug", FALSE)) cat("=== PREVIEW 1 DEBUGGING END ===\n\n")
  })

  # Dataset 2 ternary preview
  output$ternary_preview2 <- renderPlot({
    req(input$xlsx_file2)
    req(input$element_A2, input$element_B2, input$element_C2)

    if (getOption("ternary.debug", FALSE)) {
      cat("=== PREVIEW 2 DEBUGGING START ===\n")
      cat("DEBUG: Starting ternary preview 2\n")
      cat("DEBUG: Dataset 2 dimensions:", nrow(rv$df2), ncol(rv$df2), "\n")
      cat("DEBUG: Available columns:", paste(names(rv$df2), collapse = ", "), "\n")
      cat("DEBUG: input$element_A2 =", input$element_A2, "\n")
      cat("DEBUG: input$element_B2 =", input$element_B2, "\n")
      cat("DEBUG: input$element_C2 =", input$element_C2, "\n")
    }

    tryCatch({
      # Build parameters for ternary plot
      params <- build_ternary_plot_params(2, TRUE)

      if (is.null(params)) {
        if (getOption("ternary.debug", FALSE)) cat("DEBUG: build_ternary_plot_params returned NULL\n")
        return()
      }

      if (getOption("ternary.debug", FALSE)) {
        cat("DEBUG: Parameters built successfully\n")
        cat("DEBUG: About to call general_ternary_plot\n")
      }

      # Call the main ternary plot function directly
      result <- do.call(general_ternary_plot, params)

      if (getOption("ternary.debug", FALSE)) {
        cat("DEBUG: general_ternary_plot returned:", result, "\n")
        cat("DEBUG: Preview mode - plot created on device\n")
      }

    }, error = function(e) {
      if (getOption("ternary.debug", FALSE)) {
        cat("DEBUG: Error in ternary preview 2:", e$message, "\n")
        cat("DEBUG: Error call:", toString(e$call), "\n")
      }
      log_operation("ERROR", "Ternary preview 2 failed", e$message)
      # Show a simple error plot
      plot(1, 1, type = "n", xlab = "", ylab = "", main = paste("Error:", e$message))
    })

    if (getOption("ternary.debug", FALSE)) cat("=== PREVIEW 2 DEBUGGING END ===\n\n")
  })

  # ---- Batch and group-selection handlers (sibling modules) ----
  register_ternary_plots_batch_handlers(input, output, session, rv, show_message, log_operation, directory_management)
  register_ternary_plots_group_handlers(input, output, session, rv, show_message, log_operation, directory_management)

  # Observer to populate multivariate column selector when datasets are loaded
  observe({
    req(rv$df1)
    choices <- names(rv$df1)
    updateSelectizeInput(session, "multivariate_columns", choices = choices, selected = NULL)
  })

  # Observer to update multivariate column selector when dataset 2 is loaded
  observe({
    req(rv$df2)
    choices <- names(rv$df2)
    # Update choices to include both datasets' columns
    if (!is.null(rv$df1)) {
      choices <- unique(c(names(rv$df1), names(rv$df2)))
    }
    updateSelectizeInput(session, "multivariate_columns", choices = choices, selected = NULL)
  })

  # Analysis Report Generator
  generate_analysis_report <- function(input, rv) {
    # Ensure all required inputs are available
    req(input$use_mahalanobis, input$use_isolation_forest,
        input$use_iqr_filter, input$use_zscore_filter, input$use_mad_filter)

    report_lines <- c()
    report_lines <- c(report_lines, "=== ANALYSIS METHODS REPORT ===")
    report_lines <- c(report_lines, paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
    report_lines <- c(report_lines, "")

    # Ternary Plot Elements and Parameters Section
    report_lines <- c(report_lines, "=== TERNARY PLOT ELEMENTS AND PARAMETERS ===")

    # Element A
    if (!is.null(input$element_A1) && length(input$element_A1) > 0) {
      report_lines <- c(report_lines, paste("Element A:", paste(input$element_A1, collapse = " + ")))
      # Add individual filters for Element A
      if (!is.null(input$filter_A1) && length(input$filter_A1) > 0) {
        for (filter_name in names(input$filter_A1)) {
          if (!is.null(input$filter_A1[[filter_name]]) && nchar(input$filter_A1[[filter_name]]) > 0) {
            report_lines <- c(report_lines, paste("  • Filter", filter_name, ":", input$filter_A1[[filter_name]]))
          }
        }
      }
    }

    # Element B
    if (!is.null(input$element_B1) && length(input$element_B1) > 0) {
      report_lines <- c(report_lines, paste("Element B:", paste(input$element_B1, collapse = " + ")))
      # Add individual filters for Element B
      if (!is.null(input$filter_B1) && length(input$filter_B1) > 0) {
        for (filter_name in names(input$filter_B1)) {
          if (!is.null(input$filter_B1[[filter_name]]) && nchar(input$filter_B1[[filter_name]]) > 0) {
            report_lines <- c(report_lines, paste("  • Filter", filter_name, ":", input$filter_B1[[filter_name]]))
          }
        }
      }
    }

    # Element C
    if (!is.null(input$element_C1) && length(input$element_C1) > 0) {
      report_lines <- c(report_lines, paste("Element C:", paste(input$element_C1, collapse = " + ")))
      # Add individual filters for Element C
      if (!is.null(input$filter_C1) && length(input$filter_C1) > 0) {
        for (filter_name in names(input$filter_C1)) {
          if (!is.null(input$filter_C1[[filter_name]]) && nchar(input$filter_C1[[filter_name]]) > 0) {
            report_lines <- c(report_lines, paste("  • Filter", filter_name, ":", input$filter_C1[[filter_name]]))
          }
        }
      }
    }

    # Optional Parameters
    if (!is.null(input$optional_param1_1) && input$optional_param1_1 != "") {
      report_lines <- c(report_lines, paste("Optional Parameter 1 (Point Size):", input$optional_param1_1))
      if (!is.null(input$filter_op1_1) && nchar(input$filter_op1_1) > 0) {
        report_lines <- c(report_lines, paste("  • Filter:", input$filter_op1_1))
      }
      if (!is.null(input$optional_param1_representation1)) {
        report_lines <- c(report_lines, paste("  • Representation:", input$optional_param1_representation1))
      }
    }

    if (!is.null(input$optional_param2_1) && input$optional_param2_1 != "") {
      report_lines <- c(report_lines, paste("Optional Parameter 2 (Color):", input$optional_param2_1))
      if (!is.null(input$filter_op2_1) && nchar(input$filter_op2_1) > 0) {
        report_lines <- c(report_lines, paste("  • Filter:", input$filter_op2_1))
      }
    }

    report_lines <- c(report_lines, "")

    # Multivariate Analysis Section
    multivariate_methods <- c()
    if (input$use_mahalanobis) {
      multivariate_methods <- c(multivariate_methods, "Mahalanobis Distance")
      report_lines <- c(report_lines, "🔧 MAHALANOBIS DISTANCE:")
      report_lines <- c(report_lines, paste("  • Lambda (λ):", input$lambda))
      report_lines <- c(report_lines, paste("  • Omega (ω):", input$omega))
      report_lines <- c(report_lines, paste("  • Threshold Mode:", input$mdthresh_mode))
      if (input$mdthresh_mode == "manual") {
        report_lines <- c(report_lines, paste("  • Custom Threshold:", input$custom_mdthresh))
      } else {
        report_lines <- c(report_lines, "  • Formula: MDthresh = MDmean + √(100/(100+λ-ω)) × stdMD")
      }
      report_lines <- c(report_lines, paste("  • Outlier Handling:", if (input$outlier_mode_mahalanobis) "Keep only outliers" else "Remove outliers"))
      report_lines <- c(report_lines, paste("  • Reference Dataset:", input$mahalanobis_reference))
      report_lines <- c(report_lines, "")
    }


    if (input$use_isolation_forest) {
      multivariate_methods <- c(multivariate_methods, "Isolation Forest")
      report_lines <- c(report_lines, "🌲 ISOLATION FOREST:")
      report_lines <- c(report_lines, "  • Method: Machine learning anomaly detection")
      report_lines <- c(report_lines, paste("  • Outlier Handling:", if (input$outlier_mode_isolation) "Keep only outliers" else "Remove outliers"))
      report_lines <- c(report_lines, paste("  • Reference Dataset:", input$mahalanobis_reference_isolation))
      report_lines <- c(report_lines, "")
    }

    # Statistical Filtering Section
    statistical_methods <- c()
    if (input$use_iqr_filter) {
      statistical_methods <- c(statistical_methods, "IQR Filter")
      report_lines <- c(report_lines, "📊 IQR FILTER:")
      report_lines <- c(report_lines, "  • Method: Interquartile Range")
      report_lines <- c(report_lines, "  • Formula: Outliers > Q3+1.5×IQR (high values only)")
      report_lines <- c(report_lines, paste("  • Outlier Handling:", if (input$outlier_mode_iqr) "Keep only outliers" else "Remove outliers"))
      report_lines <- c(report_lines, "")
    }

    if (input$use_zscore_filter) {
      statistical_methods <- c(statistical_methods, "Z-Score Filter")
      report_lines <- c(report_lines, "📈 Z-SCORE FILTER:")
      report_lines <- c(report_lines, "  • Method: Standardized scores")
      report_lines <- c(report_lines, "  • Formula: Outliers z-score > 3 (high values only)")
      report_lines <- c(report_lines, paste("  • Outlier Handling:", if (input$outlier_mode_zscore) "Keep only outliers" else "Remove outliers"))
      report_lines <- c(report_lines, "")
    }

    if (input$use_mad_filter) {
      statistical_methods <- c(statistical_methods, "MAD Filter")
      report_lines <- c(report_lines, "📏 MAD FILTER:")
      report_lines <- c(report_lines, "  • Method: Median Absolute Deviation")
      report_lines <- c(report_lines, "  • Formula: Outliers > median+3×MAD (high values only)")
      report_lines <- c(report_lines, paste("  • Outlier Handling:", if (input$outlier_mode_mad) "Keep only outliers" else "Remove outliers"))
      report_lines <- c(report_lines, "")
    }

    # Summary Section
    report_lines <- c(report_lines, "=== SUMMARY ===")
    report_lines <- c(report_lines, paste("Multivariate Methods:", if (length(multivariate_methods) > 0) paste(multivariate_methods, collapse = ", ") else "None"))
    report_lines <- c(report_lines, paste("Statistical Methods:", if (length(statistical_methods) > 0) paste(statistical_methods, collapse = ", ") else "None"))
    report_lines <- c(report_lines, paste("Total Methods:", length(c(multivariate_methods, statistical_methods))))

    # Column Selection
    if (!is.null(input$multivariate_columns) && length(input$multivariate_columns) > 0) {
      report_lines <- c(report_lines, paste("Selected Columns:", paste(input$multivariate_columns, collapse = ", ")))
    } else {
      report_lines <- c(report_lines, "Selected Columns: All numeric columns (auto-selected)")
    }

    # First Ten Points Calculation
    report_lines <- c(report_lines, "")
    report_lines <- c(report_lines, "=== FIRST TEN POINTS CALCULATION ===")

    # Check if we have data available
    if (!is.null(rv$df1) && nrow(rv$df1) > 0) {
      report_lines <- c(report_lines, paste("Dataset 1 - Total rows:", nrow(rv$df1)))

      # Show selected columns for analysis
      if (!is.null(input$multivariate_columns) && length(input$multivariate_columns) > 0) {
        report_lines <- c(report_lines, paste("Selected columns for analysis:", paste(input$multivariate_columns, collapse = ", ")))
      }

      # Get the first 10 rows (or all if less than 10)
      first_ten_rows <- min(10, nrow(rv$df1))
      first_ten_data <- rv$df1[1:first_ten_rows, , drop = FALSE]

      # Determine which columns to show - selected columns if available, otherwise first 5
      columns_to_show <- if (!is.null(input$multivariate_columns) && length(input$multivariate_columns) > 0) {
        intersect(input$multivariate_columns, names(first_ten_data))
      } else {
        names(first_ten_data)[1:min(5, ncol(first_ten_data))]
      }

      report_lines <- c(report_lines, paste("First", first_ten_rows, "rows (Selected Columns):"))

      # Show selected columns for each row
      for (i in 1:first_ten_rows) {
        row_data <- first_ten_data[i, columns_to_show, drop = FALSE]
        col_values <- as.numeric(row_data[1, ])

        row_summary <- paste(sapply(1:length(columns_to_show), function(j) {
          paste(columns_to_show[j], "=", round(col_values[j], 3))
        }), collapse = ", ")

        report_lines <- c(report_lines, paste("  Row", i, ":", row_summary))
      }

      # Calculate ternary coordinates for first 10 rows
      report_lines <- c(report_lines, "")
      report_lines <- c(report_lines, "Ternary Coordinate Calculations:")

      # Get ternary elements
      element_A_cols <- if (!is.null(input$element_A1)) input$element_A1 else c()
      element_B_cols <- if (!is.null(input$element_B1)) input$element_B1 else c()
      element_C_cols <- if (!is.null(input$element_C1)) input$element_C1 else c()

      if (length(element_A_cols) > 0 && length(element_B_cols) > 0 && length(element_C_cols) > 0) {
        for (i in 1:first_ten_rows) {
          row_data <- first_ten_data[i, , drop = FALSE]

          # Calculate A, B, C values
          A_value <- sum(as.numeric(row_data[1, element_A_cols, drop = FALSE]), na.rm = TRUE)
          B_value <- sum(as.numeric(row_data[1, element_B_cols, drop = FALSE]), na.rm = TRUE)
          C_value <- sum(as.numeric(row_data[1, element_C_cols, drop = FALSE]), na.rm = TRUE)

          # Calculate ternary coordinates
          total <- A_value + B_value + C_value
          if (total > 0) {
            A_coord <- A_value / total
            B_coord <- B_value / total
            C_coord <- C_value / total
          } else {
            A_coord <- B_coord <- C_coord <- 0
          }

          # Calculate multivariate analysis values for this row
          multivariate_values <- c()

          # Mahalanobis Distance
          if (input$use_mahalanobis && !is.null(input$multivariate_columns) && length(input$multivariate_columns) > 1) {
            tryCatch({
              selected_cols <- intersect(input$multivariate_columns, names(row_data))
              if (length(selected_cols) > 1) {
                row_mahal_data <- row_data[1, selected_cols, drop = FALSE]
                # For single row, we need to use the full dataset for covariance calculation
                full_mahal_data <- rv$df1[, selected_cols, drop = FALSE]
                mahal_dist <- mahalanobis(row_mahal_data,
                                        colMeans(full_mahal_data, na.rm = TRUE),
                                        cov(full_mahal_data, use = "complete.obs"))
                multivariate_values <- c(multivariate_values, paste("MD =", round(mahal_dist, 2)))
              }
            }, error = function(e) {
              # Skip if calculation fails
            })
          }


          # Isolation Forest (simplified - would need actual model for proper calculation)
          if (input$use_isolation_forest && !is.null(input$multivariate_columns)) {
            multivariate_values <- c(multivariate_values, "IF = N/A (requires model)")
          }

          report_lines <- c(report_lines, paste("  Row", i, ":"))
          report_lines <- c(report_lines, paste("    • A (", paste(element_A_cols, collapse = "+"), "):", round(A_value, 3), "→", round(A_coord, 4)))
          report_lines <- c(report_lines, paste("    • B (", paste(element_B_cols, collapse = "+"), "):", round(B_value, 3), "→", round(B_coord, 4)))
          report_lines <- c(report_lines, paste("    • C (", paste(element_C_cols, collapse = "+"), "):", round(C_value, 3), "→", round(C_coord, 4)))
          report_lines <- c(report_lines, paste("    • Total:", round(total, 3), "| Ternary coordinates: A=", round(A_coord, 4), ", B=", round(B_coord, 4), ", C=", round(C_coord, 4)))

          # Add multivariate analysis values if available
          if (length(multivariate_values) > 0) {
            report_lines <- c(report_lines, paste("    • Multivariate:", paste(multivariate_values, collapse = ", ")))
          }
        }
      }

      # Calculate summary statistics for first 10 rows (selected columns only)
      if (first_ten_rows > 1) {
        # Use the same columns that were shown above
        selected_numeric_data <- first_ten_data[, columns_to_show, drop = FALSE]
        numeric_cols <- sapply(selected_numeric_data, is.numeric)

        if (sum(numeric_cols) > 0) {
          numeric_data <- selected_numeric_data[, numeric_cols, drop = FALSE]

          report_lines <- c(report_lines, "")
          report_lines <- c(report_lines, "Summary statistics for first 10 rows (Selected Columns):")
          report_lines <- c(report_lines, paste("  Mean values:", paste(sapply(numeric_data, function(x) round(mean(x, na.rm = TRUE), 3)), collapse = ", ")))
          report_lines <- c(report_lines, paste("  Std Dev:", paste(sapply(numeric_data, function(x) round(sd(x, na.rm = TRUE), 3)), collapse = ", ")))
          report_lines <- c(report_lines, paste("  Min values:", paste(sapply(numeric_data, function(x) round(min(x, na.rm = TRUE), 3)), collapse = ", ")))
          report_lines <- c(report_lines, paste("  Max values:", paste(sapply(numeric_data, function(x) round(max(x, na.rm = TRUE), 3)), collapse = ", ")))
        }
      }

      # Show selected analysis methods (without re-implementing the logic)
      selected_methods <- c()

      # Check which methods are selected
      if (input$use_mahalanobis) selected_methods <- c(selected_methods, "Mahalanobis Distance")
      if (input$use_isolation_forest) selected_methods <- c(selected_methods, "Isolation Forest")
      if (input$use_iqr_filter) selected_methods <- c(selected_methods, "IQR Filter")
      if (input$use_zscore_filter) selected_methods <- c(selected_methods, "Z-Score Filter")
      if (input$use_mad_filter) selected_methods <- c(selected_methods, "MAD Filter")

      if (length(selected_methods) > 0) {
        report_lines <- c(report_lines, "")
        report_lines <- c(report_lines, paste("Selected Analysis Methods:", paste(selected_methods, collapse = ", ")))
        report_lines <- c(report_lines, paste("Note: Actual filtering and analysis calculations are performed in the respective analysis modules"))
        report_lines <- c(report_lines, paste("(multivariate.R, statistical_filters.R, etc.) when plots are generated."))
      } else {
        report_lines <- c(report_lines, "")
        report_lines <- c(report_lines, "No analysis methods selected")
      }
    } else {
      report_lines <- c(report_lines, "No data available for first ten points calculation")
    }

    # Calculation Path Notes
    report_lines <- c(report_lines, "")
    report_lines <- c(report_lines, "=== CALCULATION PATH NOTES ===")
    report_lines <- c(report_lines, "1. Data Loading: Excel file loaded and validated (ternary_plot.R)")
    report_lines <- c(report_lines, "2. Individual Filters: Applied to Element A, B, C if specified (ternary_plot.R)")
    report_lines <- c(report_lines, "3. Optional Parameter Filters: Applied to point size and color parameters if specified (ternary_plot.R)")
    report_lines <- c(report_lines, "4. Statistical Filters: IQR, Z-Score, MAD applied to selected columns (statistical_filters.R)")
    report_lines <- c(report_lines, "5. Multivariate Analysis: Mahalanobis, Isolation Forest applied (multivariate.R)")
    report_lines <- c(report_lines, "6. Ternary Coordinates: Calculated as A/(A+B+C), B/(A+B+C), C/(A+B+C) (ternary_plot.R)")
    report_lines <- c(report_lines, "7. Plot Generation: Points plotted with optional parameters for size and color (ternary_plot.R)")
    report_lines <- c(report_lines, "8. File Output: Plot saved in selected format (PNG, JPEG, PDF, TIFF) (file_management.R)")

    return(paste(report_lines, collapse = "\n"))
  }

  # Render Analysis Report
  output$analysis_report <- renderText({
    req(input$use_mahalanobis || input$use_isolation_forest ||
        input$use_iqr_filter || input$use_zscore_filter || input$use_mad_filter)

    generate_analysis_report(input, rv)
  })

  # ---- Save Plot Buttons for Main Ternary Plots ----

  # Save Plot 1
  observeEvent(input$plot1, {
    req(input$xlsx_file1)
    req(input$element_A1, input$element_B1, input$element_C1)

    if (getOption("ternary.debug", FALSE)) {
      cat("DEBUG: Save Plot 1 button clicked\n")
      cat("DEBUG: File:", input$xlsx_file1$name, "\n")
      cat("DEBUG: Elements A:", paste(input$element_A1, collapse = ", "), "\n")
      cat("DEBUG: Elements B:", paste(input$element_B1, collapse = ", "), "\n")
      cat("DEBUG: Elements C:", paste(input$element_C1, collapse = ", "), "\n")
    }

    tryCatch({
      # Build parameters for ternary plot
      params <- build_ternary_plot_params(1, FALSE)

      if (is.null(params)) {
        if (getOption("ternary.debug", FALSE)) cat("DEBUG: build_ternary_plot_params returned NULL for Plot 1\n")
        output$status <- renderText("Error: Invalid parameters for Plot 1")
        return()
      }

      if (getOption("ternary.debug", FALSE)) {
        cat("DEBUG: Parameters built successfully for Plot 1\n")
        cat("DEBUG: About to call general_ternary_plot for Plot 1\n")
      }

      # Call the main ternary plot function
      result <- do.call(general_ternary_plot, params)

      if (getOption("ternary.debug", FALSE)) {
        cat("DEBUG: general_ternary_plot result for Plot 1:", result, "\n")
      }

      if (!is.null(result)) {
        output$status <- renderText(paste("✅ Plot 1 saved successfully!\n📍 Location:", result))
        log_operation("SUCCESS", "Plot 1 saved successfully", paste("Saved to:", result))
      } else {
        output$status <- renderText("❌ Failed to save Plot 1")
        log_operation("ERROR", "Failed to save Plot 1")
      }

    }, error = function(e) {
      output$status <- renderText(paste("Error saving Plot 1:", e$message))
      log_operation("ERROR", "Failed to save Plot 1", e$message)
    })
  })

  # Save Plot 2
  observeEvent(input$plot2, {
    req(input$xlsx_file2)
    req(input$element_A2, input$element_B2, input$element_C2)

    tryCatch({
      # Build parameters for ternary plot
      params <- build_ternary_plot_params(2, FALSE)

      if (is.null(params)) {
        output$status <- renderText("Error: Invalid parameters for Plot 2")
        return()
      }

      # Call the main ternary plot function
      result <- do.call(general_ternary_plot, params)

      if (!is.null(result)) {
        output$status <- renderText(paste("✅ Plot 2 saved successfully!\n📍 Location:", result))
        log_operation("SUCCESS", "Plot 2 saved successfully", paste("Saved to:", result))
      } else {
        output$status <- renderText("❌ Failed to save Plot 2")
        log_operation("ERROR", "Failed to save Plot 2")
      }

    }, error = function(e) {
      output$status <- renderText(paste("Error saving Plot 2:", e$message))
      log_operation("ERROR", "Failed to save Plot 2", e$message)
    })
  })

  # Save Both Plots
  observeEvent(input$plot_both, {
    req(input$xlsx_file1, input$xlsx_file2)
    req(input$element_A1, input$element_B1, input$element_C1)
    req(input$element_A2, input$element_B2, input$element_C2)

    tryCatch({
      plots_saved <- 0
      errors <- c()
      saved_files <- c()

      # Save Plot 1
      params1 <- build_ternary_plot_params(1, FALSE)
      if (!is.null(params1)) {
        result1 <- do.call(general_ternary_plot, params1)
        if (!is.null(result1)) {
          plots_saved <- plots_saved + 1
          saved_files <- c(saved_files, result1)
        }
      } else {
        errors <- c(errors, "Plot 1: Invalid parameters")
      }

      # Save Plot 2
      params2 <- build_ternary_plot_params(2, FALSE)
      if (!is.null(params2)) {
        result2 <- do.call(general_ternary_plot, params2)
        if (!is.null(result2)) {
          plots_saved <- plots_saved + 1
          saved_files <- c(saved_files, result2)
        }
      } else {
        errors <- c(errors, "Plot 2: Invalid parameters")
      }

      # Update status with file locations
      if (plots_saved == 2) {
        status_msg <- paste("✅ Both plots saved successfully!\n📍 Locations:\n• Plot 1:", saved_files[1], "\n• Plot 2:", saved_files[2])
        output$status <- renderText(status_msg)
        log_operation("SUCCESS", "Both plots saved successfully", paste("Saved to:", paste(saved_files, collapse = "; ")))
      } else if (plots_saved == 1) {
        status_msg <- paste("⚠️ One plot saved successfully, one failed\n📍 Saved:", saved_files[1])
        output$status <- renderText(status_msg)
        log_operation("WARNING", "One plot saved, one failed", paste("Saved to:", saved_files[1]))
      } else {
        output$status <- renderText("❌ Failed to save both plots")
        log_operation("ERROR", "Failed to save both plots")
      }

      if (length(errors) > 0) {
        error_msg <- paste("Errors:", paste(errors, collapse = "; "))
        output$status <- renderText(paste("Error saving plots:", error_msg))
        log_operation("ERROR", "Failed to save plots", error_msg)
      }

    }, error = function(e) {
      output$status <- renderText(paste("Error saving both plots:", e$message))
      log_operation("ERROR", "Failed to save both plots", e$message)
    })
  })

  # Return the ternary plot functions for integration
  return(list(
    # All functions are already set up as observeEvent and renderPlot
    # This function just sets up the event handlers
  ))
}
