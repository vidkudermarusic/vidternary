# ---- Server Export Module: Data Export Handlers ----
# Split out of server_export.R: the filtered-data / comprehensive-export
# button handlers (as opposed to the status/report renderers in
# server_export_reports.R).

register_export_data_handlers <- function(input, output, session, rv, show_message, log_operation, directory_management = NULL) {

  # Export filtered data for Dataset 1
  observeEvent(input$export_filtered_data1, {
    tryCatch({
      show_message("Generating filtered data for Dataset 1...", "info")

      # Generate filtered data using the complete filtering pipeline
      filtered_data <- generate_filtered_data_for_export(dataset_num = 1)

      # Create export filename
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      user_output_dir <- if (!is.null(directory_management)) directory_management$output_dir() else file.path(getwd(), "output")
      filename <- paste0("filtered_dataset1_", timestamp, ".csv")
      file_path <- file.path(user_output_dir, filename)

      # Export to CSV
      write.csv(filtered_data, file = file_path, row.names = FALSE)

      show_message(paste("Filtered Dataset 1 exported successfully to", filename), "success")
      log_operation("INFO", "Filtered Dataset 1 exported", paste("File:", filename, "Rows:", nrow(filtered_data), "Columns:", ncol(filtered_data)))

      # Update export status
      rv$last_export_results <- list(
        dataset = 1,
        filename = filename,
        rows = nrow(filtered_data),
        columns = ncol(filtered_data),
        timestamp = timestamp
      )

    }, error = function(e) {
      show_message(paste("Error exporting filtered Dataset 1:", e$message), "error")
      log_operation("ERROR", "Failed to export filtered Dataset 1", e$message)
    })
  })

  # Export filtered data for Dataset 2
  observeEvent(input$export_filtered_data2, {
    tryCatch({
      show_message("Generating filtered data for Dataset 2...", "info")

      # Generate filtered data using the complete filtering pipeline
      filtered_data <- generate_filtered_data_for_export(dataset_num = 2)

      # Create export filename
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      user_output_dir <- if (!is.null(directory_management)) directory_management$output_dir() else file.path(getwd(), "output")
      filename <- paste0("filtered_dataset2_", timestamp, ".csv")
      file_path <- file.path(user_output_dir, filename)

      # Export to CSV
      write.csv(filtered_data, file = file_path, row.names = FALSE)

      show_message(paste("Filtered Dataset 2 exported successfully to", filename), "success")
      log_operation("INFO", "Filtered Dataset 2 exported", paste("File:", filename, "Rows:", nrow(filtered_data), "Columns:", ncol(filtered_data)))

      # Update export status
      rv$last_export_results <- list(
        dataset = 2,
        filename = filename,
        rows = nrow(filtered_data),
        columns = ncol(filtered_data),
        timestamp = timestamp
      )

    }, error = function(e) {
      show_message(paste("Error exporting filtered Dataset 2:", e$message), "error")
      log_operation("ERROR", "Failed to export filtered Dataset 2", e$message)
    })
  })

  # Comprehensive Export All Functionality
  observeEvent(input$export_all, {
    req(input$xlsx_file1)

    show_message("Starting comprehensive export...", "info")

    # Initialize export tracking
    export_results <- list()
    user_output_dir <- if (!is.null(directory_management)) directory_management$output_dir() else file.path(getwd(), "output")
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

    # Create export subfolder in user-selected output directory
    export_folder <- file.path(user_output_dir, paste0("export_", timestamp))
    if (!dir.exists(export_folder)) {
      dir.create(export_folder, recursive = TRUE)
    }

    # Export filtered data
    if (input$export_filtered_data) {
      tryCatch({
        show_message("Exporting filtered data...", "info")

        # Generate filtered data for both datasets if available
        if (!is.null(input$xlsx_file1)) {
          df1 <- generate_filtered_data_for_export(1)
          filename1 <- paste0("filtered_data_dataset1_", timestamp, ".", input$export_format)
          filepath1 <- file.path(export_folder, filename1)

          if (input$export_format == "xlsx") {
            openxlsx::write.xlsx(df1, filepath1)
          } else if (input$export_format == "csv") {
            write.csv(df1, filepath1, row.names = FALSE)
          } else if (input$export_format == "rds") {
            saveRDS(df1, filepath1)
          }

          export_results$filtered_data1 <- list(filename = filename1, path = filepath1, rows = nrow(df1), cols = ncol(df1))
          show_message(paste("Dataset 1 filtered data exported:", filename1), "success")
        }

        if (!is.null(input$xlsx_file2)) {
          df2 <- generate_filtered_data_for_export(2)
          filename2 <- paste0("filtered_data_dataset2_", timestamp, ".", input$export_format)
          filepath2 <- file.path(export_folder, filename2)

          if (input$export_format == "xlsx") {
            openxlsx::write.xlsx(df2, filepath2)
          } else if (input$export_format == "csv") {
            write.csv(df2, filepath2, row.names = FALSE)
          } else if (input$export_format == "rds") {
            saveRDS(df2, filepath2)
          }

          export_results$filtered_data2 <- list(filename = filename2, path = filepath2, rows = nrow(df2), cols = ncol(df2))
          show_message(paste("Dataset 2 filtered data exported:", filename2), "success")
        }

      }, error = function(e) {
        show_message(paste("Filtered data export error:", e$message), "error")
        export_results$filtered_data_error <- e$message
      })
    }

    # Export comprehensive analysis
    if (input$export_comprehensive) {
      tryCatch({
        show_message("Exporting comprehensive analysis...", "info")

        # Collect comprehensive statistics and analysis results
        stats_data <- list()

        # Basic dataset statistics
        if (!is.null(input$xlsx_file1)) {
          df1 <- read_dataset_file(input$xlsx_file1)
          numeric_cols1 <- sapply(df1, is.numeric)

          stats_data$dataset1_summary <- list(
            rows = nrow(df1),
            columns = ncol(df1),
            numeric_columns = sum(numeric_cols1),
            missing_values = sum(is.na(df1)),
            numeric_column_names = paste(names(df1)[numeric_cols1], collapse = ", ")
          )

          # Detailed statistics for numeric columns
          if (sum(numeric_cols1) > 0) {
            stats_data$dataset1_detailed <- summary(df1[, numeric_cols1, drop = FALSE])

            # Correlation matrix
            if (sum(numeric_cols1) >= 2) {
              stats_data$dataset1_correlations <- cor(df1[, numeric_cols1, drop = FALSE], use = "complete.obs")
            }
          }
        }

        if (!is.null(input$xlsx_file2)) {
          df2 <- read_dataset_file(input$xlsx_file2)
          numeric_cols2 <- sapply(df2, is.numeric)

          stats_data$dataset2_summary <- list(
            rows = nrow(df2),
            columns = ncol(df2),
            numeric_columns = sum(numeric_cols2),
            missing_values = sum(is.na(df2)),
            numeric_column_names = paste(names(df2)[numeric_cols2], collapse = ", ")
          )

          # Detailed statistics for numeric columns
          if (sum(numeric_cols2) > 0) {
            stats_data$dataset2_detailed <- summary(df2[, numeric_cols2, drop = FALSE])

            # Correlation matrix
            if (sum(numeric_cols2) >= 2) {
              stats_data$dataset2_correlations <- cor(df2[, numeric_cols2, drop = FALSE], use = "complete.obs")
            }
          }
        }

        # Include multivariate analysis results if available
        if (!is.null(rv$mahalanobis_result)) {
          stats_data$multivariate_analysis <- rv$mahalanobis_result
        }

        # Include filtering results if available
        if (!is.null(rv$filtered_data1)) {
          stats_data$filtering_results_dataset1 <- list(
            original_rows = nrow(read_dataset_file(input$xlsx_file1)),
            filtered_rows = nrow(rv$filtered_data1),
            filtering_percentage = round(nrow(rv$filtered_data1) / nrow(read_dataset_file(input$xlsx_file1)) * 100, 2)
          )
        }

        if (!is.null(rv$filtered_data2)) {
          stats_data$filtering_results_dataset2 <- list(
            original_rows = nrow(read_dataset_file(input$xlsx_file2)),
            filtered_rows = nrow(rv$filtered_data2),
            filtering_percentage = round(nrow(rv$filtered_data2) / nrow(read_dataset_file(input$xlsx_file2)) * 100, 2)
          )
        }

        # Export statistics
        filename <- paste0("statistical_summary_", timestamp, ".xlsx")
        filepath <- file.path(export_folder, filename)

        # Create a comprehensive workbook with multiple sheets
        wb <- openxlsx::createWorkbook()

        # Summary sheet
        openxlsx::addWorksheet(wb, "Summary")
        summary_data <- data.frame(
          Metric = c("Export Timestamp", "Total Files", "Export Format", "Analysis Type"),
          Value = c(timestamp, length(export_results), input$export_format, "Comprehensive Analysis Export")
        )
        openxlsx::writeData(wb, "Summary", summary_data)

        # Dataset 1 statistics
        if (!is.null(stats_data$dataset1_summary)) {
          openxlsx::addWorksheet(wb, "Dataset1_Summary")
          stats1_df <- data.frame(
            Metric = names(unlist(stats_data$dataset1_summary)),
            Value = unlist(stats_data$dataset1_summary)
          )
          openxlsx::writeData(wb, "Dataset1_Summary", stats1_df)

          # Detailed statistics
          if (!is.null(stats_data$dataset1_detailed)) {
            openxlsx::addWorksheet(wb, "Dataset1_Detailed")
            openxlsx::writeData(wb, "Dataset1_Detailed", stats_data$dataset1_detailed)
          }

          # Correlation matrix
          if (!is.null(stats_data$dataset1_correlations)) {
            openxlsx::addWorksheet(wb, "Dataset1_Correlations")
            openxlsx::writeData(wb, "Dataset1_Correlations", stats_data$dataset1_correlations)
          }
        }

        # Dataset 2 statistics
        if (!is.null(stats_data$dataset2_summary)) {
          openxlsx::addWorksheet(wb, "Dataset2_Summary")
          stats2_df <- data.frame(
            Metric = names(unlist(stats_data$dataset2_summary)),
            Value = unlist(stats_data$dataset2_summary)
          )
          openxlsx::writeData(wb, "Dataset2_Summary", stats2_df)

          # Detailed statistics
          if (!is.null(stats_data$dataset2_detailed)) {
            openxlsx::addWorksheet(wb, "Dataset2_Detailed")
            openxlsx::writeData(wb, "Dataset2_Detailed", stats_data$dataset2_detailed)
          }

          # Correlation matrix
          if (!is.null(stats_data$dataset2_correlations)) {
            openxlsx::addWorksheet(wb, "Dataset2_Correlations")
            openxlsx::writeData(wb, "Dataset2_Correlations", stats_data$dataset2_correlations)
          }
        }

        # Multivariate analysis results
        if (!is.null(stats_data$multivariate_analysis)) {
          openxlsx::addWorksheet(wb, "Multivariate_Analysis")
          mahal_results <- stats_data$multivariate_analysis

          # Create a summary of multivariate analysis
          mahal_summary <- data.frame(
            Metric = c("Method", "Total Points", "Outliers Detected", "Threshold Value",
                      "Mean Distance", "Std Distance", "Degrees of Freedom"),
            Value = c(
              if (!is.null(mahal_results$method)) mahal_results$method else "Standard Mahalanobis",
              mahal_results$total_points,
              mahal_results$outlier_custom,
              round(mahal_results$MDthresh, 3),
              round(mahal_results$MDmean, 3),
              round(mahal_results$stdMD, 3),
              mahal_results$df
            )
          )
          openxlsx::writeData(wb, "Multivariate_Analysis", mahal_summary)
        }

        # Filtering results
        if (!is.null(stats_data$filtering_results_dataset1)) {
          openxlsx::addWorksheet(wb, "Filtering_Results_Dataset1")
          filtering_df1 <- data.frame(
            Metric = names(unlist(stats_data$filtering_results_dataset1)),
            Value = unlist(stats_data$filtering_results_dataset1)
          )
          openxlsx::writeData(wb, "Filtering_Results_Dataset1", filtering_df1)
        }

        if (!is.null(stats_data$filtering_results_dataset2)) {
          openxlsx::addWorksheet(wb, "Filtering_Results_Dataset2")
          filtering_df2 <- data.frame(
            Metric = names(unlist(stats_data$filtering_results_dataset2)),
            Value = unlist(stats_data$filtering_results_dataset2)
          )
          openxlsx::writeData(wb, "Filtering_Results_Dataset2", filtering_df2)
        }

        openxlsx::saveWorkbook(wb, filepath, overwrite = TRUE)
        export_results$statistics <- list(filename = filename, path = filepath)
        show_message(paste("Statistics exported:", filename), "success")

      }, error = function(e) {
        show_message(paste("Statistics export error:", e$message), "error")
        export_results$statistics_error <- e$message
      })
    }

    # NOTE: Duplicate comprehensive analysis section removed - using the first one above
    # Export comprehensive analysis
    if (FALSE && input$export_comprehensive) {
      tryCatch({
        show_message("Exporting comprehensive analysis...", "info")

        # Create comprehensive analysis workbook
        wb <- openxlsx::createWorkbook()

        # Helper function to safely add worksheet with error handling
        safe_add_worksheet <- function(wb, sheet_name, data_func) {
          tryCatch({
            openxlsx::addWorksheet(wb, sheet_name)
            data <- data_func()
            if (!is.null(data) && nrow(data) > 0) {
              openxlsx::writeData(wb, sheet_name, data)
            } else {
              openxlsx::writeData(wb, sheet_name, data.frame(Message = "No data available"))
            }
          }, error = function(e) {
            # If sheet creation fails, create a blank sheet with error message
            tryCatch({
              openxlsx::addWorksheet(wb, sheet_name)
              openxlsx::writeData(wb, sheet_name, data.frame(Error = paste("Sheet creation failed:", e$message)))
            }, error = function(e2) {
              # If even blank sheet fails, skip this sheet entirely
              show_message(paste("Warning: Could not create sheet", sheet_name, ":", e2$message), "warning")
            })
          })
        }

        # Sheet 1: Stats Dataset 1
        safe_add_worksheet(wb, "Stats Dataset 1", function() {
          if (!is.null(input$xlsx_file1)) {
            df1 <- read_dataset_file(input$xlsx_file1)
            numeric_cols1 <- sapply(df1, is.numeric)
            if (sum(numeric_cols1) > 0) {
              numeric_data <- df1[, numeric_cols1, drop = FALSE]
              # Calculate statistics manually to get clean numeric values
              stats_df <- data.frame(
                Statistic = c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max"),
                stringsAsFactors = FALSE
              )
              # Add columns for each numeric variable with clean values
              for (col in colnames(numeric_data)) {
                col_data <- numeric_data[, col]
                col_data <- col_data[!is.na(col_data)]
                if (length(col_data) > 0) {
                  stats_df[[col]] <- c(
                    min(col_data),
                    quantile(col_data, 0.25),
                    median(col_data),
                    mean(col_data),
                    quantile(col_data, 0.75),
                    max(col_data)
                  )
                } else {
                  stats_df[[col]] <- rep(NA, 6)
                }
              }
              return(stats_df)
            }
          }
          return(data.frame(Message = "Dataset 1 not loaded or no numeric columns"))
        })

        # Sheet 2: Stats Dataset 2
        safe_add_worksheet(wb, "Stats Dataset 2", function() {
          if (!is.null(input$xlsx_file2)) {
            df2 <- read_dataset_file(input$xlsx_file2)
            numeric_cols2 <- sapply(df2, is.numeric)
            if (sum(numeric_cols2) > 0) {
              numeric_data <- df2[, numeric_cols2, drop = FALSE]
              # Calculate statistics manually to get clean numeric values
              stats_df <- data.frame(
                Statistic = c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max"),
                stringsAsFactors = FALSE
              )
              # Add columns for each numeric variable with clean values
              for (col in colnames(numeric_data)) {
                col_data <- numeric_data[, col]
                col_data <- col_data[!is.na(col_data)]
                if (length(col_data) > 0) {
                  stats_df[[col]] <- c(
                    min(col_data),
                    quantile(col_data, 0.25),
                    median(col_data),
                    mean(col_data),
                    quantile(col_data, 0.75),
                    max(col_data)
                  )
                } else {
                  stats_df[[col]] <- rep(NA, 6)
                }
              }
              return(stats_df)
            }
          }
          return(data.frame(Message = "Dataset 2 not loaded or no numeric columns"))
        })

        # Sheet 3: Correlation Dataset 1
        safe_add_worksheet(wb, "Correlation Dataset 1", function() {
          if (!is.null(input$xlsx_file1)) {
            df1 <- read_dataset_file(input$xlsx_file1)
            numeric_cols1 <- sapply(df1, is.numeric)
            if (sum(numeric_cols1) >= 2) {
              return(cor(df1[, numeric_cols1, drop = FALSE], use = "complete.obs"))
            }
          }
          return(data.frame(Message = "Need at least 2 numeric columns for correlation"))
        })

        # Sheet 4: Correlation Dataset 2
        safe_add_worksheet(wb, "Correlation Dataset 2", function() {
          if (!is.null(input$xlsx_file2)) {
            df2 <- read_dataset_file(input$xlsx_file2)
            numeric_cols2 <- sapply(df2, is.numeric)
            if (sum(numeric_cols2) >= 2) {
              return(cor(df2[, numeric_cols2, drop = FALSE], use = "complete.obs"))
            }
          }
          return(data.frame(Message = "Need at least 2 numeric columns for correlation"))
        })

        # Sheet 5: Mahalanobis Distance
        safe_add_worksheet(wb, "Mahalanobis Distance", function() {
          if (!is.null(input$multivariate_columns) && length(input$multivariate_columns) >= 2) {
            tryCatch({
              if (!is.null(input$xlsx_file1) && !is.null(input$xlsx_file2)) {
                df1 <- read_dataset_file(input$xlsx_file1)
                df2 <- read_dataset_file(input$xlsx_file2)

                # Clean data
                data1_subset <- df1[, input$multivariate_columns, drop = FALSE]
                data2_subset <- df2[, input$multivariate_columns, drop = FALSE]
                complete_cases1 <- complete.cases(data1_subset)
                complete_cases2 <- complete.cases(data2_subset)

                if (sum(complete_cases1) >= 2 && sum(complete_cases2) >= 2) {
                  data1_clean <- data1_subset[complete_cases1, , drop = FALSE]
                  data2_clean <- data2_subset[complete_cases2, , drop = FALSE]

                  result <- compute_mahalanobis_distance(
                    data1_clean,
                    data2_clean,
                    lambda = 1,
                    omega = 0,
                    keep_outliers = FALSE,
                    custom_mdthresh = NULL,
                    selected_columns = input$multivariate_columns,
                    mdthresh_mode = "auto"
                  )

                  if (!is.null(result)) {
                    return(data.frame(
                      Metric = c("Selected Columns", "Reference Dataset", "Threshold Method", "Threshold Value", "Total Points", "Outliers Detected", "Outlier Percentage", "Degrees of Freedom", "MD Mean", "MD Std Dev", "Lambda", "Omega"),
                      Value = c(
                        paste(input$multivariate_columns, collapse = ", "),
                        if (!is.null(input$mahalanobis_reference)) input$mahalanobis_reference else "dataset1",
                        result$threshold_method,
                        result$MDthresh,
                        result$total_points,
                        result$outlier_custom,
                        round(result$outlier_custom / result$total_points * 100, 1),
                        result$df,
                        if (!is.null(result$MDmean)) round(result$MDmean, 3) else "N/A",
                        if (!is.null(result$stdMD)) round(result$stdMD, 3) else "N/A",
                        if (!is.null(input$lambda)) input$lambda else 1,
                        if (!is.null(input$omega)) input$omega else 0
                      )
                    ))
                  }
                }
              }
            }, error = function(e) {
              return(data.frame(Error = paste("Mahalanobis analysis failed:", e$message)))
            })
          }
          return(data.frame(Message = "Both datasets and multivariate columns required for Mahalanobis analysis"))
        })

        # Robust Mahalanobis MCD sheet removed

        # Sheet 7: Isolation Forest
        safe_add_worksheet(wb, "Isolation Forest", function() {
          if (!is.null(input$multivariate_columns) && length(input$multivariate_columns) >= 2) {
            tryCatch({
              if (!is.null(input$xlsx_file1)) {
                df1 <- read_dataset_file(input$xlsx_file1)
                df2 <- if (!is.null(input$xlsx_file2)) read_dataset_file(input$xlsx_file2) else df1

                # Clean data
                data1_subset <- df1[, input$multivariate_columns, drop = FALSE]
                data2_subset <- df2[, input$multivariate_columns, drop = FALSE]
                complete_cases1 <- complete.cases(data1_subset)
                complete_cases2 <- complete.cases(data2_subset)

                if (sum(complete_cases1) >= 2 && sum(complete_cases2) >= 2) {
                  data1_clean <- data1_subset[complete_cases1, , drop = FALSE]
                  data2_clean <- data2_subset[complete_cases2, , drop = FALSE]

                  result <- compute_isolation_forest(
                    data1_clean,
                    data2_clean,
                    selected_columns = input$multivariate_columns,
                    keep_outliers = FALSE
                  )

                  if (!is.null(result)) {
                    return(data.frame(
                      Metric = c("Selected Columns", "Threshold Method", "Threshold Value", "Total Points", "Outliers Detected", "Outlier Percentage", "Contamination Rate"),
                      Value = c(
                        paste(input$multivariate_columns, collapse = ", "),
                        result$threshold_method,
                        result$threshold,
                        result$total_points,
                        result$outlier_count,
                        round(result$outlier_count / result$total_points * 100, 1),
                        if (!is.null(result$contamination)) result$contamination else "N/A"
                      )
                    ))
                  }
                }
              }
            }, error = function(e) {
              return(data.frame(Error = paste("Isolation Forest analysis failed:", e$message)))
            })
          }
          return(data.frame(Message = "Both datasets and multivariate columns required for Isolation Forest analysis"))
        })

        # Sheet 8: Analysis Report
        safe_add_worksheet(wb, "Analysis Report", function() {
          # Generate comprehensive analysis report similar to ternary plots
          report_lines <- c()
          report_lines <- c(report_lines, "=== COMPREHENSIVE ANALYSIS REPORT ===")
          report_lines <- c(report_lines, paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
          report_lines <- c(report_lines, "")

          # Dataset Information
          report_lines <- c(report_lines, "=== DATASET INFORMATION ===")
          report_lines <- c(report_lines, paste("Dataset 1:", if (!is.null(input$xlsx_file1)) paste("Loaded -", input$xlsx_file1$name) else "Not Loaded"))
          report_lines <- c(report_lines, paste("Dataset 2:", if (!is.null(input$xlsx_file2)) paste("Loaded -", input$xlsx_file2$name) else "Not Loaded"))
          report_lines <- c(report_lines, "")

          # Ternary Plot Elements
          report_lines <- c(report_lines, "=== TERNARY PLOT ELEMENTS ===")
          if (!is.null(input$element_A1) && length(input$element_A1) > 0) {
            report_lines <- c(report_lines, paste("Element A:", paste(input$element_A1, collapse = " + ")))
          }
          if (!is.null(input$element_B1) && length(input$element_B1) > 0) {
            report_lines <- c(report_lines, paste("Element B:", paste(input$element_B1, collapse = " + ")))
          }
          if (!is.null(input$element_C1) && length(input$element_C1) > 0) {
            report_lines <- c(report_lines, paste("Element C:", paste(input$element_C1, collapse = " + ")))
          }
          report_lines <- c(report_lines, "")

          # Multivariate Analysis Methods
          report_lines <- c(report_lines, "=== MULTIVARIATE ANALYSIS METHODS ===")
          multivariate_methods <- c()
          if (isTRUE(input$use_mahalanobis)) {
            multivariate_methods <- c(multivariate_methods, "Mahalanobis Distance")
            report_lines <- c(report_lines, "🔧 MAHALANOBIS DISTANCE:")
            report_lines <- c(report_lines, paste("  • Lambda (λ):", if (!is.null(input$lambda)) input$lambda else "Default"))
            report_lines <- c(report_lines, paste("  • Omega (ω):", if (!is.null(input$omega)) input$omega else "Default"))
            report_lines <- c(report_lines, paste("  • Threshold Mode:", if (!is.null(input$mdthresh_mode)) input$mdthresh_mode else "Default"))
            report_lines <- c(report_lines, paste("  • Reference Dataset:", if (!is.null(input$mahalanobis_reference)) input$mahalanobis_reference else "Default"))
            report_lines <- c(report_lines, "")
          }

          if (isTRUE(input$use_isolation_forest)) {
            multivariate_methods <- c(multivariate_methods, "Isolation Forest")
            report_lines <- c(report_lines, "🌲 ISOLATION FOREST:")
            report_lines <- c(report_lines, "  • Method: Machine learning anomaly detection")
            report_lines <- c(report_lines, "")
          }

          # Statistical Filtering Methods
          report_lines <- c(report_lines, "=== STATISTICAL FILTERING METHODS ===")
          statistical_methods <- c()
          if (isTRUE(input$use_iqr_filter)) {
            statistical_methods <- c(statistical_methods, "IQR Filter")
            report_lines <- c(report_lines, "📊 IQR FILTER: Interquartile Range method")
          }
          if (isTRUE(input$use_zscore_filter)) {
            statistical_methods <- c(statistical_methods, "Z-Score Filter")
            report_lines <- c(report_lines, "📈 Z-SCORE FILTER: Standardized scores method")
          }
          if (isTRUE(input$use_mad_filter)) {
            statistical_methods <- c(statistical_methods, "MAD Filter")
            report_lines <- c(report_lines, "📏 MAD FILTER: Median Absolute Deviation method")
          }
          report_lines <- c(report_lines, "")

          # Summary
          report_lines <- c(report_lines, "=== SUMMARY ===")
          report_lines <- c(report_lines, paste("Multivariate Methods:", if (length(multivariate_methods) > 0) paste(multivariate_methods, collapse = ", ") else "None"))
          report_lines <- c(report_lines, paste("Statistical Methods:", if (length(statistical_methods) > 0) paste(statistical_methods, collapse = ", ") else "None"))
          report_lines <- c(report_lines, paste("Selected Columns:", if (!is.null(input$multivariate_columns)) paste(input$multivariate_columns, collapse = ", ") else "None"))
          report_lines <- c(report_lines, paste("Total Methods:", length(c(multivariate_methods, statistical_methods))))
          report_lines <- c(report_lines, "")
          report_lines <- c(report_lines, "=== EXPORT INFORMATION ===")
          report_lines <- c(report_lines, paste("Export Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
          report_lines <- c(report_lines, "Export Type: Comprehensive Analysis")
          report_lines <- c(report_lines, "Status: Completed Successfully")

          # Convert to data frame
          report_df <- data.frame(
            Line = report_lines,
            stringsAsFactors = FALSE
          )
          return(report_df)
        })

        # Sheet 10: Filtered Data 1
        safe_add_worksheet(wb, "Filtered Data 1", function() {
          tryCatch({
            return(generate_filtered_data_for_export(1, input$xlsx_file1, input$xlsx_file2))
          }, error = function(e) {
            return(data.frame(Message = paste("Error generating filtered data:", e$message)))
          })
        })

        # Sheet 11: Filtered Data 2
        safe_add_worksheet(wb, "Filtered Data 2", function() {
          tryCatch({
            return(generate_filtered_data_for_export(2, input$xlsx_file1, input$xlsx_file2))
          }, error = function(e) {
            return(data.frame(Message = paste("Error generating filtered data:", e$message)))
          })
        })

        # Sheet 12: Original Data from Dataset 1
        safe_add_worksheet(wb, "Original Data 1", function() {
          if (!is.null(input$xlsx_file1)) {
            return(read_dataset_file(input$xlsx_file1))
          }
          return(data.frame(Message = "Dataset 1 not loaded"))
        })

        # Sheet 13: Original Data from Dataset 2
        safe_add_worksheet(wb, "Original Data 2", function() {
          if (!is.null(input$xlsx_file2)) {
            return(read_dataset_file(input$xlsx_file2))
          }
          return(data.frame(Message = "Dataset 2 not loaded"))
        })

        # Save the comprehensive analysis workbook
        comprehensive_file <- file.path(export_folder, paste0("comprehensive_analysis_", timestamp, ".xlsx"))
        openxlsx::saveWorkbook(wb, comprehensive_file, overwrite = TRUE)
        export_results$comprehensive <- list(filename = basename(comprehensive_file), path = comprehensive_file)
        show_message(paste("Comprehensive analysis exported:", basename(comprehensive_file)), "success")

      }, error = function(e) {
        show_message(paste("Comprehensive analysis export error:", e$message), "error")
        export_results$comprehensive_error <- e$message
      })
    }

    # Export plots (if any are available)
    if (input$export_plots) {
      tryCatch({
        show_message("Exporting available plots...", "info")

        plot_exported <- FALSE

        # Check for available plots in reactive values
        if (!is.null(rv$scatter_plot)) {
          filename <- paste0("scatter_plot_", timestamp, ".", input$plot_export_format)
          filepath <- file.path(export_folder, filename)

          if (input$plot_export_format == "png") {
            png(filepath, width = 800, height = 600, res = 150)
            print(rv$scatter_plot)
            dev.off()
          } else if (input$plot_export_format == "pdf") {
            pdf(filepath, width = 8, height = 6)
            print(rv$scatter_plot)
            dev.off()
          } else if (input$plot_export_format == "svg") {
            svg(filepath, width = 8, height = 6)
            print(rv$scatter_plot)
            dev.off()
          }

          export_results$scatter_plot <- list(filename = filename, path = filepath)
          plot_exported <- TRUE
          show_message(paste("Scatter plot exported:", filename), "success")
        }

        if (!is.null(rv$histogram_plot)) {
          filename <- paste0("histogram_plot_", timestamp, ".", input$plot_export_format)
          filepath <- file.path(export_folder, filename)

          if (input$plot_export_format == "png") {
            png(filepath, width = 800, height = 600, res = 150)
            print(rv$histogram_plot)
            dev.off()
          } else if (input$plot_export_format == "pdf") {
            pdf(filepath, width = 8, height = 6)
            print(rv$histogram_plot)
            dev.off()
          } else if (input$plot_export_format == "svg") {
            svg(filepath, width = 8, height = 6)
            print(rv$histogram_plot)
            dev.off()
          }

          export_results$histogram_plot <- list(filename = filename, path = filepath)
          plot_exported <- TRUE
          show_message(paste("Histogram plot exported:", filename), "success")
        }

        if (!is.null(rv$boxplot_plot)) {
          filename <- paste0("boxplot_plot_", timestamp, ".", input$plot_export_format)
          filepath <- file.path(export_folder, filename)

          if (input$plot_export_format == "png") {
            png(filepath, width = 800, height = 600, res = 150)
            print(rv$boxplot_plot)
            dev.off()
          } else if (input$plot_export_format == "pdf") {
            pdf(filepath, width = 8, height = 6)
            print(rv$boxplot_plot)
            dev.off()
          } else if (input$plot_export_format == "svg") {
            svg(filepath, width = 8, height = 6)
            print(rv$scatter_plot)
            dev.off()
          }

          export_results$boxplot_plot <- list(filename = filename, path = filepath)
          plot_exported <- TRUE
          show_message(paste("Box plot exported:", filename), "success")
        }

        if (!plot_exported) {
          show_message("No plots available for export. Create plots first in the Multiple Plot Types tab.", "warning")
        }

      }, error = function(e) {
        show_message(paste("Plot export error:", e$message), "error")
        export_results$plots_error <- e$message
      })
    }

    # Store export history
    if (length(export_results) > 0) {
      rv$export_history <- c(rv$export_history, list(
        timestamp = format(Sys.time(), "%Y%m%d_%H%M%S"),
        results = export_results
      ))
    }

    show_message("Comprehensive export completed successfully!", "success")
    log_operation("INFO", "Comprehensive export completed", paste("Files exported:", length(export_results)))

    # Update last export results for status display
    rv$last_export_results <- export_results
    rv$last_export_folder <- export_folder

  })

  # Export selected items
  observeEvent(input$export_selected, {
    req(input$xlsx_file1)

    show_message("Exporting selected items...", "info")

    # This would allow users to select specific items to export
    # For now, just show a message
    show_message("Selective export functionality will be implemented in the next version. Use 'Export All Selected' for now.", "info")
  })
}
