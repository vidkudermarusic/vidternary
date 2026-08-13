# ---- Server Data Comparison Module: Descriptive Stats & Correlation ----
# Split out of server_data_comparison.R: the initial data-readiness check,
# descriptive-statistics buttons, and correlation-analysis buttons (as
# opposed to the multivariate analysis handlers in
# server_data_comparison_multivariate.R, or the validation/preview renderers
# in server_data_comparison_preview.R).
#
# NOTE: output$data_readiness_status is assigned here AND again in
# server_data_comparison_preview.R (a pre-existing duplicate in the original
# single file - the second assignment wins, since it re-reads the raw
# uploaded files directly rather than relying on cached rv$df1/df2). To keep
# that "second one wins" behavior unchanged, register_data_comparison_stats_
# handlers() must be called before register_data_comparison_preview_handlers()
# in server_data_comparison.R.

register_data_comparison_stats_handlers <- function(input, output, session, rv, show_message, log_operation) {

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
}
