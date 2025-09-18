# =============================================================================
# vidternary: Shiny Server Module - Data Comparison
# =============================================================================
# 
# Package:     vidternary
# Version:     1.0.0
# Author:      Vid Kuder Marušič <vidkm30@gmail.com>
# Maintainer:  Vid Kuder Marušič <vidkm30@gmail.com>
# License:     MIT + file LICENSE
# Repository:  https://github.com/vidkudermarusic/vidternary
# 
# Description: Server-side logic for data comparison functionality including
#              statistics, correlations, and multivariate analysis between datasets.
# 
# Key Functions:
#   - create_server_data_comparison(): Main data comparison server logic
#   - [Data comparison functions and statistical analysis]
# 
# Dependencies:
#   - R (>= 4.0.0)
#   - shiny, ggplot2, corrplot, DT
# 
# Last Modified: 2025-09-07
# 
# =============================================================================

create_server_data_comparison <- function(input, output, session, rv, show_message, log_operation) {
  
  # ---- Data Comparison Tab Functionality ----
  
  # Data readiness status for Data Comparison tab
  output$data_readiness_status <- renderPrint({
    if (is.null(rv$df1) || is.null(rv$df2)) {
      cat("📋 Please upload both datasets to begin comparison.\n")
      return()
    }
    
    tryCatch({
      # Basic data validation
      numeric_cols1 <- sapply(rv$df1, is.numeric)
      numeric_cols2 <- sapply(rv$df2, is.numeric)
      common_cols <- intersect(colnames(rv$df1)[numeric_cols1], colnames(rv$df2)[numeric_cols2])
      
      cat("=== DATA READINESS STATUS ===\n")
      cat("Dataset 1:", nrow(rv$df1), "rows ×", ncol(rv$df1), "columns\n")
      cat("Dataset 2:", nrow(rv$df2), "rows ×", ncol(rv$df2), "columns\n")
      cat("Common numeric columns:", length(common_cols), "\n")
      
      if (length(common_cols) >= 2) {
        cat("✅ Ready for multivariate analysis\n")
        cat("Available columns:", paste(common_cols, collapse = ", "), "\n")
      } else {
        cat("❌ Need at least 2 common numeric columns\n")
        cat("Dataset 1 numeric:", sum(numeric_cols1), "\n")
        cat("Dataset 2 numeric:", sum(numeric_cols2), "\n")
      }
      
    }, error = function(e) {
      cat("❌ Error reading datasets:", e$message, "\n")
    })
  })
  
  # ---- Descriptive Statistics ----
  
  observeEvent(input$compute_stats1, {
    req(rv$df1)
    tryCatch({
      numeric_cols <- sapply(rv$df1, is.numeric)
      if (sum(numeric_cols) == 0) {
        output$descriptive_stats_output <- renderText("No numeric columns found in Dataset 1")
        return()
      }
      
      stats <- summary(rv$df1[, numeric_cols, drop = FALSE])
      rv$stats1 <- stats
      
      # Format output
      output$descriptive_stats_output <- renderPrint({
        cat("=== Dataset 1 Descriptive Statistics ===\n")
        print(stats)
        cat("\n=== Data Info ===\n")
        cat("Total rows:", nrow(rv$df1), "\n")
        cat("Numeric columns:", sum(numeric_cols), "\n")
        cat("Missing values:", sum(is.na(rv$df1[, numeric_cols, drop = FALSE])), "\n")
      })
      
      log_operation("SUCCESS", "Computed stats for Dataset 1", paste("Analyzed", sum(numeric_cols), "numeric columns"))
      
    }, error = function(e) {
      output$descriptive_stats_output <- renderText(paste("Error computing stats for Dataset 1:", e$message))
      log_operation("ERROR", "Failed to compute stats for Dataset 1", e$message)
    })
  })
  
  observeEvent(input$compute_stats2, {
    req(rv$df2)
    tryCatch({
      numeric_cols <- sapply(rv$df2, is.numeric)
      if (sum(numeric_cols) == 0) {
        output$descriptive_stats_output <- renderText("No numeric columns found in Dataset 2")
        return()
      }
      
      stats <- summary(rv$df2[, numeric_cols, drop = FALSE])
      rv$stats2 <- stats
      
      # Format output
      output$descriptive_stats_output <- renderPrint({
        cat("=== Dataset 2 Descriptive Statistics ===\n")
        print(stats)
        cat("\n=== Data Info ===\n")
        cat("Total rows:", nrow(rv$df2), "\n")
        cat("Numeric columns:", sum(numeric_cols), "\n")
        cat("Missing values:", sum(is.na(rv$df2[, numeric_cols, drop = FALSE])), "\n")
      })
      
      log_operation("SUCCESS", "Computed stats for Dataset 2", paste("Analyzed", sum(numeric_cols), "numeric columns"))
      
    }, error = function(e) {
      output$descriptive_stats_output <- renderText(paste("Error computing stats for Dataset 2:", e$message))
      log_operation("ERROR", "Failed to compute stats for Dataset 2", e$message)
    })
  })
  
  observeEvent(input$compare_stats, {
    req(rv$df1, rv$df2)
    tryCatch({
      numeric_cols1 <- sapply(rv$df1, is.numeric)
      numeric_cols2 <- sapply(rv$df2, is.numeric)
      
      if (sum(numeric_cols1) == 0 || sum(numeric_cols2) == 0) {
        output$descriptive_stats_output <- renderText("Both datasets need numeric columns for comparison")
        return()
      }
      
      stats1 <- summary(rv$df1[, numeric_cols1, drop = FALSE])
      stats2 <- summary(rv$df2[, numeric_cols2, drop = FALSE])
      
      # Find common columns
      common_cols <- intersect(names(rv$df1)[numeric_cols1], names(rv$df2)[numeric_cols2])
      
      output$descriptive_stats_output <- renderPrint({
        cat("=== DATASET COMPARISON ===\n\n")
        cat("Dataset 1 - Rows:", nrow(rv$df1), "| Numeric columns:", sum(numeric_cols1), "\n")
        cat("Dataset 2 - Rows:", nrow(rv$df2), "| Numeric columns:", sum(numeric_cols2), "\n\n")
        
        cat("=== Dataset 1 Summary ===\n")
        print(stats1)
        cat("\n=== Dataset 2 Summary ===\n")
        print(stats2)
        
        if (length(common_cols) > 0) {
          cat("\n=== Common Numeric Columns ===\n")
          cat(paste(common_cols, collapse = ", "), "\n")
        }
      })
      
      log_operation("SUCCESS", "Compared both datasets", paste("Common columns:", length(common_cols)))
      
    }, error = function(e) {
      output$descriptive_stats_output <- renderText(paste("Error comparing datasets:", e$message))
      log_operation("ERROR", "Failed to compare datasets", e$message)
    })
  })
  
  # ---- Correlation Analysis ----
  
  observeEvent(input$compute_correlations1, {
    req(rv$df1)
    tryCatch({
      numeric_cols <- sapply(rv$df1, is.numeric)
      if (sum(numeric_cols) < 2) {
        output$correlation_output <- renderText("Need at least 2 numeric columns for correlation analysis")
        return()
      }
      
      corr_matrix <- cor(rv$df1[, numeric_cols, drop = FALSE], use = "complete.obs")
      rv$correlation1 <- corr_matrix
      
      output$correlation_output <- renderPrint({
        cat("=== Dataset 1 Correlation Matrix ===\n")
        print(round(corr_matrix, 3))
        
        # Find high correlations
        high_corr <- which(abs(corr_matrix) > 0.7 & corr_matrix != 1, arr.ind = TRUE)
        if (nrow(high_corr) > 0) {
          cat("\n=== High Correlations (|r| > 0.7) ===\n")
          for (i in 1:nrow(high_corr)) {
            row_name <- rownames(corr_matrix)[high_corr[i, 1]]
            col_name <- colnames(corr_matrix)[high_corr[i, 2]]
            corr_val <- corr_matrix[high_corr[i, 1], high_corr[i, 2]]
            cat(paste(row_name, "vs", col_name, ":", round(corr_val, 3)), "\n")
          }
        }
      })
      
      log_operation("SUCCESS", "Computed correlations for Dataset 1", paste("Analyzed", sum(numeric_cols), "variables"))
      
    }, error = function(e) {
      output$correlation_output <- renderText(paste("Error computing correlations for Dataset 1:", e$message))
      log_operation("ERROR", "Failed to compute correlations for Dataset 1", e$message)
    })
  })
  
  observeEvent(input$compute_correlations2, {
    req(rv$df2)
    tryCatch({
      numeric_cols <- sapply(rv$df2, is.numeric)
      if (sum(numeric_cols) < 2) {
        output$correlation_output <- renderText("Need at least 2 numeric columns for correlation analysis")
        return()
      }
      
      corr_matrix <- cor(rv$df2[, numeric_cols, drop = FALSE], use = "complete.obs")
      rv$correlation2 <- corr_matrix
      
      output$correlation_output <- renderPrint({
        cat("=== Dataset 2 Correlation Matrix ===\n")
        print(round(corr_matrix, 3))
        
        # Find high correlations
        high_corr <- which(abs(corr_matrix) > 0.7 & corr_matrix != 1, arr.ind = TRUE)
        if (nrow(high_corr) > 0) {
          cat("\n=== High Correlations (|r| > 0.7) ===\n")
          for (i in 1:nrow(high_corr)) {
            row_name <- rownames(corr_matrix)[high_corr[i, 1]]
            col_name <- colnames(corr_matrix)[high_corr[i, 2]]
            corr_val <- corr_matrix[high_corr[i, 1], high_corr[i, 2]]
            cat(paste(row_name, "vs", col_name, ":", round(corr_val, 3)), "\n")
          }
        }
      })
      
      log_operation("SUCCESS", "Computed correlations for Dataset 2", paste("Analyzed", sum(numeric_cols), "variables"))
      
    }, error = function(e) {
      output$correlation_output <- renderText(paste("Error computing correlations for Dataset 2:", e$message))
      log_operation("ERROR", "Failed to compute correlations for Dataset 2", e$message)
    })
  })
  
  observeEvent(input$compare_correlations, {
    req(rv$df1, rv$df2)
    tryCatch({
      numeric_cols1 <- sapply(rv$df1, is.numeric)
      numeric_cols2 <- sapply(rv$df2, is.numeric)
      
      if (sum(numeric_cols1) < 2 || sum(numeric_cols2) < 2) {
        output$correlation_output <- renderText("Both datasets need at least 2 numeric columns for correlation comparison")
        return()
      }
      
      # Find common columns
      common_cols <- intersect(names(rv$df1)[numeric_cols1], names(rv$df2)[numeric_cols2])
      
      if (length(common_cols) < 2) {
        output$correlation_output <- renderText("Need at least 2 common numeric columns for correlation comparison")
        return()
      }
      
      corr1 <- cor(rv$df1[, common_cols, drop = FALSE], use = "complete.obs")
      corr2 <- cor(rv$df2[, common_cols, drop = FALSE], use = "complete.obs")
      
      # Calculate direct correlation between datasets for each common column
      direct_correlations <- numeric(length(common_cols))
      names(direct_correlations) <- common_cols
      
      for (i in seq_along(common_cols)) {
        col_name <- common_cols[i]
        # Get complete cases for both datasets
        complete_cases <- complete.cases(rv$df1[[col_name]], rv$df2[[col_name]])
        if (sum(complete_cases) > 1) {
          direct_correlations[i] <- cor(rv$df1[[col_name]][complete_cases], 
                                       rv$df2[[col_name]][complete_cases], 
                                       use = "complete.obs")
        } else {
          direct_correlations[i] <- NA
        }
      }
      
      output$correlation_output <- renderPrint({
        cat("=== CORRELATION COMPARISON ===\n")
        cat("Common columns:", paste(common_cols, collapse = ", "), "\n\n")
        
        cat("=== Dataset 1 Internal Correlations ===\n")
        print(round(corr1, 3))
        
        cat("\n=== Dataset 2 Internal Correlations ===\n")
        print(round(corr2, 3))
        
        cat("\n=== Direct Correlation Between Datasets ===\n")
        cat("(Correlation between same columns in Dataset 1 vs Dataset 2)\n")
        direct_corr_df <- data.frame(
          Column = names(direct_correlations),
          Correlation = round(direct_correlations, 3),
          Complete_Cases = sapply(common_cols, function(col) {
            sum(complete.cases(rv$df1[[col]], rv$df2[[col]]))
          })
        )
        print(direct_corr_df)
        
        cat("\n=== Internal Correlation Differences ===\n")
        cat("(Difference between Dataset 1 and Dataset 2 internal correlations)\n")
        diff_matrix <- corr1 - corr2
        print(round(diff_matrix, 3))
        
        # Find large differences
        large_diff <- which(abs(diff_matrix) > 0.3 & diff_matrix != 0, arr.ind = TRUE)
        if (nrow(large_diff) > 0) {
          cat("\n=== Large Internal Correlation Differences (|diff| > 0.3) ===\n")
          for (i in 1:nrow(large_diff)) {
            row_name <- rownames(diff_matrix)[large_diff[i, 1]]
            col_name <- colnames(diff_matrix)[large_diff[i, 2]]
            diff_val <- diff_matrix[large_diff[i, 1], large_diff[i, 2]]
            cat(paste(row_name, "vs", col_name, ":", round(diff_val, 3)), "\n")
          }
        }
        
        # Summary of direct correlations
        valid_direct_corr <- direct_correlations[!is.na(direct_correlations)]
        if (length(valid_direct_corr) > 0) {
          cat("\n=== Direct Correlation Summary ===\n")
          cat("Mean direct correlation:", round(mean(valid_direct_corr), 3), "\n")
          cat("Median direct correlation:", round(median(valid_direct_corr), 3), "\n")
          cat("Min direct correlation:", round(min(valid_direct_corr), 3), "\n")
          cat("Max direct correlation:", round(max(valid_direct_corr), 3), "\n")
          cat("High direct correlations (|r| > 0.7):", 
              sum(abs(valid_direct_corr) > 0.7), "out of", length(valid_direct_corr), "\n")
        }
      })
      
      log_operation("SUCCESS", "Compared correlations", paste("Analyzed", length(common_cols), "common variables"))
      
    }, error = function(e) {
      output$correlation_output <- renderText(paste("Error comparing correlations:", e$message))
      log_operation("ERROR", "Failed to compare correlations", e$message)
    })
  })
  
  # ---- Multivariate Analysis ----
  
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
  
  observeEvent(input$robust_mahalanobis_analysis, {
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
        output$robust_mahalanobis_output <- renderText(error_msg)
        return()
      }
      
      if (length(selected_cols) < 2) {
        output$robust_mahalanobis_output <- renderText("Need at least 2 columns selected in the Universal Column Selector")
        return()
      }
      
      output$robust_mahalanobis_output <- renderPrint({
        cat("=== ROBUST MAHALANOBIS (MCD) ANALYSIS ===\n")
        cat("Selected columns from Universal Column Selector:", paste(selected_cols, collapse = ", "), "\n")
        cat("Dataset 1 rows:", nrow(rv$df1), "\n")
        cat("Dataset 2 rows:", nrow(rv$df2), "\n\n")
        
        # Additional data validation before analysis
        data1_subset <- rv$df1[, selected_cols, drop = FALSE]
        data2_subset <- rv$df2[, selected_cols, drop = FALSE]
        
        # Check for missing values and remove them
        complete_cases1 <- complete.cases(data1_subset)
        complete_cases2 <- complete.cases(data2_subset)
        
        if (sum(complete_cases1) < 3 || sum(complete_cases2) < 3) {
          cat("ERROR: Not enough complete cases for robust analysis.\n")
          cat("Dataset 1 complete cases:", sum(complete_cases1), "\n")
          cat("Dataset 2 complete cases:", sum(complete_cases2), "\n")
          cat("Need at least 3 complete cases per dataset for robust Mahalanobis.\n")
          return()
        }
        
        # Clean data by removing incomplete cases
        data1_clean <- data1_subset[complete_cases1, , drop = FALSE]
        data2_clean <- data2_subset[complete_cases2, , drop = FALSE]
        
        cat("Using clean data:\n")
        cat("Dataset 1 clean rows:", nrow(data1_clean), "\n")
        cat("Dataset 2 clean rows:", nrow(data2_clean), "\n\n")
        
        # Perform robust Mahalanobis analysis
        result <- compute_robust_mahalanobis(
          data1_clean, 
          data2_clean, 
          method = "MCD",
          keep_outliers = FALSE,
          selected_columns = selected_cols
        )
        
        if (!is.null(result)) {
          cat("✅ Analysis completed successfully!\n\n")
          cat("Threshold method:", result$threshold_method, "\n")
          cat("Threshold value:", round(result$MDthresh, 3), "\n")
          cat("Total points analyzed:", result$total_points, "\n")
          cat("Outliers detected:", result$outlier_count, "\n")
          cat("Outlier percentage:", round(result$outlier_count / result$total_points * 100, 1), "%\n")
        } else {
          cat("❌ Analysis failed. Please check data quality.\n")
        }
      })
      
      log_operation("SUCCESS", "Robust Mahalanobis analysis completed", paste("Analyzed", length(selected_cols), "variables"))
      
    }, error = function(e) {
      output$robust_mahalanobis_output <- renderText(paste("Error in Robust Mahalanobis analysis:", e$message))
      log_operation("ERROR", "Robust Mahalanobis analysis failed", e$message)
    })
  })
  
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
          keep_outliers = FALSE,
          selected_columns = selected_cols
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
  
  # ---- Enhanced Analysis Outputs ----
  
  
  output$analysis_validation1 <- renderPrint({
    req(rv$df1)
    tryCatch({
      numeric_cols <- sapply(rv$df1, is.numeric)
      if (sum(numeric_cols) == 0) {
        cat("No numeric columns found in Dataset 1")
        return()
      }
      
      cat("=== Dataset 1 Missing/Outlier Summary ===\n")
      
      # Missing values summary
      missing_summary <- sapply(rv$df1[, numeric_cols, drop = FALSE], function(x) sum(is.na(x)))
      cat("Missing values per column:\n")
      print(missing_summary[missing_summary > 0])
      
      # Outlier detection using IQR method
      outlier_summary <- sapply(rv$df1[, numeric_cols, drop = FALSE], function(x) {
        if (length(x) < 4) return(0)
        Q1 <- quantile(x, 0.25, na.rm = TRUE)
        Q3 <- quantile(x, 0.75, na.rm = TRUE)
        IQR <- Q3 - Q1
        lower_bound <- Q1 - 1.5 * IQR
        upper_bound <- Q3 + 1.5 * IQR
        sum(x < lower_bound | x > upper_bound, na.rm = TRUE)
      })
      
      cat("\nOutliers per column (IQR method):\n")
      print(outlier_summary[outlier_summary > 0])
      
      cat("\n=== Data Quality Summary ===\n")
      cat("Total rows:", nrow(rv$df1), "\n")
      cat("Numeric columns:", sum(numeric_cols), "\n")
      cat("Complete cases:", sum(complete.cases(rv$df1[, numeric_cols, drop = FALSE])), "\n")
      cat("Complete case percentage:", round(sum(complete.cases(rv$df1[, numeric_cols, drop = FALSE])) / nrow(rv$df1) * 100, 1), "%\n")
      
    }, error = function(e) {
      cat("Error computing validation for Dataset 1:", e$message, "\n")
    })
  })
  
  output$excel_preview1 <- DT::renderDataTable({
    req(input$xlsx_file1)
    openxlsx::read.xlsx(input$xlsx_file1$datapath, sheet = 1)
  })
  
  
  output$analysis_validation2 <- renderPrint({
    req(rv$df2)
    tryCatch({
      numeric_cols <- sapply(rv$df2, is.numeric)
      if (sum(numeric_cols) == 0) {
        cat("No numeric columns found in Dataset 2")
        return()
      }
      
      cat("=== Dataset 2 Missing/Outlier Summary ===\n")
      
      # Missing values summary
      missing_summary <- sapply(rv$df2[, numeric_cols, drop = FALSE], function(x) sum(is.na(x)))
      cat("Missing values per column:\n")
      print(missing_summary[missing_summary > 0])
      
      # Outlier detection using IQR method
      outlier_summary <- sapply(rv$df2[, numeric_cols, drop = FALSE], function(x) {
        if (length(x) < 4) return(0)
        Q1 <- quantile(x, 0.25, na.rm = TRUE)
        Q3 <- quantile(x, 0.75, na.rm = TRUE)
        IQR <- Q3 - Q1
        lower_bound <- Q1 - 1.5 * IQR
        upper_bound <- Q3 + 1.5 * IQR
        sum(x < lower_bound | x > upper_bound, na.rm = TRUE)
      })
      
      cat("\nOutliers per column (IQR method):\n")
      print(outlier_summary[outlier_summary > 0])
      
      cat("\n=== Data Quality Summary ===\n")
      cat("Total rows:", nrow(rv$df2), "\n")
      cat("Numeric columns:", sum(numeric_cols), "\n")
      cat("Complete cases:", sum(complete.cases(rv$df2[, numeric_cols, drop = FALSE])), "\n")
      cat("Complete case percentage:", round(sum(complete.cases(rv$df2[, numeric_cols, drop = FALSE])) / nrow(rv$df2) * 100, 1), "%\n")
      
    }, error = function(e) {
      cat("Error computing validation for Dataset 2:", e$message, "\n")
    })
  })
  
  output$excel_preview2 <- DT::renderDataTable({
    req(input$xlsx_file2)
    openxlsx::read.xlsx(input$xlsx_file2$datapath, sheet = 1)
  })
  
  # Dynamic analysis buttons and outputs
  selected_analysis <- reactiveVal(NULL)
  observeEvent(input$show_missing1, { selected_analysis("missing1") })
  observeEvent(input$show_excel1, { selected_analysis("excel1") })
  observeEvent(input$show_missing2, { selected_analysis("missing2") })
  observeEvent(input$show_excel2, { selected_analysis("excel2") })
  
  output$analysis_buttons <- renderUI({
    req(input$xlsx_file1, input$xlsx_file2)
    tagList(
      h4("Dataset 1 Analysis:"),
      actionButton("show_missing1", "Missing/Outlier Summary 1"),
      actionButton("show_excel1", "Excel File Preview 1"),
      br(), br(),
      h4("Dataset 2 Analysis:"),
      actionButton("show_missing2", "Missing/Outlier Summary 2"),
      actionButton("show_excel2", "Excel File Preview 2")
    )
  })
  
  output$dynamic_output <- renderUI({
    req(input$xlsx_file1, input$xlsx_file2)
    sel <- selected_analysis()
    if (is.null(sel) || length(sel) != 1) return(NULL)
    switch(sel,
           missing1 = verbatimTextOutput("analysis_validation1"),
           excel1 = DT::dataTableOutput("excel_preview1"),
           missing2 = verbatimTextOutput("analysis_validation2"),
           excel2 = DT::dataTableOutput("excel_preview2"),
           NULL
    )
  })
  
  
  # Enhanced data readiness status
  output$data_readiness_status <- renderPrint({
    if (is.null(input$xlsx_file1) || is.null(input$xlsx_file2)) {
      cat("📋 Please upload both datasets to begin comparison.\n")
      return()
    }
    
    tryCatch({
      df1 <- openxlsx::read.xlsx(input$xlsx_file1$datapath, sheet=1)
      df2 <- openxlsx::read.xlsx(input$xlsx_file2$datapath, sheet=1)
      
      # Basic data validation
      numeric_cols1 <- sapply(df1, is.numeric)
      numeric_cols2 <- sapply(df2, is.numeric)
      common_cols <- intersect(colnames(df1)[numeric_cols1], colnames(df2)[numeric_cols2])
      
      cat("=== DATA READINESS STATUS ===\n")
      cat("Dataset 1:", nrow(df1), "rows ×", ncol(df1), "columns\n")
      cat("Dataset 2:", nrow(df2), "rows ×", ncol(df2), "columns\n")
      cat("Common numeric columns:", length(common_cols), "\n")
      
      if (length(common_cols) >= 2) {
        cat("✅ Ready for multivariate analysis\n")
        cat("Available columns:", paste(common_cols, collapse = ", "), "\n")
      } else {
        cat("❌ Need at least 2 common numeric columns\n")
        cat("Dataset 1 numeric:", sum(numeric_cols1), "\n")
        cat("Dataset 2 numeric:", sum(numeric_cols2), "\n")
      }
      
    }, error = function(e) {
      cat("❌ Error reading datasets:", e$message, "\n")
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
          use_robust_mahalanobis = TRUE,
          use_isolation_forest = TRUE,
          lambda = if (exists("input$lambda")) input$lambda else 1,
          omega = if (exists("input$omega")) input$omega else 0,
          custom_mdthresh = if (exists("input$custom_mdthresh") && exists("input$mdthresh_mode") && input$mdthresh_mode == "manual") input$custom_mdthresh else NULL,
          mdthresh_mode = if (exists("input$mdthresh_mode")) input$mdthresh_mode else "auto",
          selected_columns = selected_cols,
          xlsx_file1 = input$xlsx_file1,
          xlsx_file2 = input$xlsx_file2
        )
      
      if (!is.null(result)) {
        cat("=== MULTIVARIATE ANALYSIS RESULTS ===\n\n")
        
        if (result$method == "Robust Mahalanobis (MCD)") {
          cat("🔍 Robust Mahalanobis Analysis (MCD):\n")
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
          
          if (exists("input$mdthresh_mode") && input$mdthresh_mode == "manual") {
            cat("Threshold mode: Manual\n")
            cat("Custom MDthresh:", round(result$MDthresh, 3), "\n")
          } else {
            cat("Threshold mode: Automatic (MDthresh=MDmean+√(100/(100+λ-ω))×stdMD)\n")
            if (exists("input$lambda") && exists("input$omega")) {
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
          if (exists("result$outlier_custom")) {
            cat("Points above custom threshold:", result$outlier_custom, "\n")
          }
          if (!is.null(result$p_values)) {
            cat("P-value range:", round(min(result$p_values), 4), "to", round(max(result$p_values), 4), "\n")
          }
        }
        
        cat("\n💡 Interpretation:\n")
        if (result$method == "Robust Mahalanobis (MCD)") {
          cat("- Robust MCD is less sensitive to outliers in the reference dataset\n")
          cat("- Good for non-normal distributions and contaminated data\n")
        } else if (result$method == "Isolation Forest") {
          cat("- Isolation Forest detects anomalies based on data isolation\n")
          cat("- Threshold automatically set to top contamination% of scores\n")
          cat("- Good for high-dimensional data and non-linear relationships\n")
        } else {
          cat("- Standard Mahalanobis assumes multivariate normal distribution\n")
          if (exists("input$mdthresh_mode") && input$mdthresh_mode == "auto") {
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
  
  # Return the module functions (if any are needed externally)
  return(list(
    # This module primarily contains observeEvent and output rendering functions
    # No external functions to return at this time
  ))
}
