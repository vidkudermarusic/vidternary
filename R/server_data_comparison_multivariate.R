# ---- Server Data Comparison Module: Multivariate Analysis ----
# Split out of server_data_comparison.R: the Mahalanobis Distance /
# Isolation Forest button handlers, plus the unified comprehensive
# multivariate results display (mahalanobis_info) - all driven by the
# target/reference/columns selectors from server_data_comparison_upload.R
# (comparison_mv_target/comparison_mv_reference/comparison_mv_columns)
# instead of the fixed rv$df1 (target) / rv$df2 (reference) pair. Reference
# "__self__" means the target dataset is tested against its own
# distribution (self-referential outlier detection); any other reference
# tests the target against that dataset's distribution instead - the same
# underlying compute_mahalanobis_distance()/compute_isolation_forest() call
# handles both, since self-reference is just target==reference.
#
# NOTE: the previous version of the "Comprehensive Analysis Results" panel
# called multivariate_analysis() (helpers_multivariate.R) and read fields
# like result$method/result$total_points directly off its return value -
# but multivariate_analysis() actually returns a list with nested
# $mahalanobis_results/$isolation_forest_results, not those fields at the
# top level, so that panel was reading undefined fields (silently NULL)
# even before this rewrite. Fixed here by calling
# compute_mahalanobis_distance()/compute_isolation_forest() directly, the
# same way the two single-method buttons already did.

register_data_comparison_multivariate_handlers <- function(input, output, session, rv, show_message, log_operation) {

  # Resolves the current target/reference data frames and names, or NULL if
  # any prerequisite (dataset selection, columns) isn't set yet. Does not
  # call req() itself - callers req() their own inputs first, matching this
  # app's usual pattern of guarding at the top of each observer/render.
  resolve_target_reference <- function() {
    target_name <- input$comparison_mv_target
    reference_choice <- input$comparison_mv_reference
    target_df <- rv$comparison_data[[target_name]]
    if (is.null(target_df)) return(NULL)
    if (is.null(reference_choice) || reference_choice == "__self__") {
      reference_df <- target_df
      reference_name <- target_name
    } else {
      reference_df <- rv$comparison_data[[reference_choice]]
      reference_name <- reference_choice
    }
    if (is.null(reference_df)) return(NULL)
    list(target = target_df, reference = reference_df, target_name = target_name, reference_name = reference_name)
  }

  # Returns an error message string if the selected columns are unusable
  # against the given target/reference pair, or NULL if they're fine.
  validate_mv_columns <- function(td, selected_cols) {
    avail_target <- names(td$target)[sapply(td$target, is.numeric)]
    avail_ref <- names(td$reference)[sapply(td$reference, is.numeric)]
    missing_target <- setdiff(selected_cols, avail_target)
    missing_ref <- setdiff(selected_cols, avail_ref)
    if (length(missing_target) > 0 || length(missing_ref) > 0) {
      msg <- "Selected columns not found in datasets:\n"
      if (length(missing_target) > 0) msg <- paste0(msg, "Missing in ", td$target_name, ": ", paste(missing_target, collapse = ", "), "\n")
      if (length(missing_ref) > 0) msg <- paste0(msg, "Missing in ", td$reference_name, ": ", paste(missing_ref, collapse = ", "), "\n")
      return(msg)
    }
    if (length(selected_cols) < 2) {
      return("Need at least 2 columns selected in the multivariate column selector")
    }
    NULL
  }

  observeEvent(input$mahalanobis_analysis, {
    req(rv$comparison_data, input$comparison_mv_target, input$comparison_mv_columns)
    td <- resolve_target_reference()
    req(td)
    selected_cols <- input$comparison_mv_columns

    col_error <- validate_mv_columns(td, selected_cols)
    if (!is.null(col_error)) {
      output$mahalanobis_output <- renderText(col_error)
      return()
    }

    tryCatch({
      output$mahalanobis_output <- renderPrint({
        cat("=== MAHALANOBIS DISTANCE ANALYSIS ===\n")
        cat("Target:", td$target_name, "| Reference:", td$reference_name, "\n")
        cat("Columns:", paste(selected_cols, collapse = ", "), "\n")
        cat("Target rows:", nrow(td$target), "| Reference rows:", nrow(td$reference), "\n\n")

        result <- compute_mahalanobis_distance(
          td$target[, selected_cols, drop = FALSE],
          td$reference[, selected_cols, drop = FALSE],
          lambda = if (!is.null(input$comparison_mv_lambda)) input$comparison_mv_lambda else 1,
          omega = if (!is.null(input$comparison_mv_omega)) input$comparison_mv_omega else 0,
          keep_outliers = FALSE,
          custom_mdthresh = if (!is.null(input$comparison_mv_mdthresh_mode) && input$comparison_mv_mdthresh_mode == "manual") input$comparison_mv_custom_mdthresh else NULL,
          selected_columns = selected_cols,
          mdthresh_mode = if (!is.null(input$comparison_mv_mdthresh_mode)) input$comparison_mv_mdthresh_mode else "auto"
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

      log_operation("SUCCESS", "Mahalanobis analysis completed", paste("Target:", td$target_name, "Reference:", td$reference_name, "Columns:", length(selected_cols)))

    }, error = function(e) {
      output$mahalanobis_output <- renderText(paste("Error in Mahalanobis analysis:", e$message))
      log_operation("ERROR", "Mahalanobis analysis failed", e$message)
    })
  })

  observeEvent(input$isolation_forest_analysis, {
    req(rv$comparison_data, input$comparison_mv_target, input$comparison_mv_columns)
    td <- resolve_target_reference()
    req(td)
    selected_cols <- input$comparison_mv_columns

    col_error <- validate_mv_columns(td, selected_cols)
    if (!is.null(col_error)) {
      output$isolation_forest_output <- renderText(col_error)
      return()
    }

    tryCatch({
      output$isolation_forest_output <- renderPrint({
        cat("=== ISOLATION FOREST ANALYSIS ===\n")
        cat("Target:", td$target_name, "| Reference:", td$reference_name, "\n")
        cat("Columns:", paste(selected_cols, collapse = ", "), "\n")
        cat("Target rows:", nrow(td$target), "| Reference rows:", nrow(td$reference), "\n\n")

        result <- compute_isolation_forest(
          td$target[, selected_cols, drop = FALSE],
          td$reference[, selected_cols, drop = FALSE],
          selected_columns = selected_cols,
          keep_outliers = FALSE
        )

        if (!is.null(result)) {
          # compute_isolation_forest() returns outlier_indices/threshold/
          # contamination/columns_used - not total_points/outlier_count/
          # threshold_method (those are compute_mahalanobis_distance()
          # fields; this analysis was silently printing blanks for them).
          total_points <- length(result$outlier_indices)
          outlier_count <- sum(result$outlier_indices, na.rm = TRUE)
          cat("✅ Analysis completed successfully!\n\n")
          cat("Threshold method: Quantile of reference scores at (1 - contamination) =", result$contamination, "\n")
          cat("Threshold value:", round(result$threshold, 3), "\n")
          cat("Total points analyzed:", total_points, "\n")
          cat("Outliers detected:", outlier_count, "\n")
          cat("Outlier percentage:", round(outlier_count / total_points * 100, 1), "%\n")
        } else {
          cat("❌ Analysis failed. Please check data quality.\n")
        }
      })

      log_operation("SUCCESS", "Isolation Forest analysis completed", paste("Target:", td$target_name, "Reference:", td$reference_name, "Columns:", length(selected_cols)))

    }, error = function(e) {
      output$isolation_forest_output <- renderText(paste("Error in Isolation Forest analysis:", e$message))
      log_operation("ERROR", "Isolation Forest analysis failed", e$message)
    })
  })

  # ---- Comprehensive Multivariate Analysis Display ----

  output$mahalanobis_info <- renderPrint({
    req(rv$comparison_data, input$comparison_mv_target, input$comparison_mv_columns)
    td <- resolve_target_reference()
    req(td)
    selected_cols <- input$comparison_mv_columns

    col_error <- validate_mv_columns(td, selected_cols)
    if (!is.null(col_error)) {
      cat("❌", col_error)
      return()
    }

    tryCatch({
      lambda <- if (!is.null(input$comparison_mv_lambda)) input$comparison_mv_lambda else 1
      omega <- if (!is.null(input$comparison_mv_omega)) input$comparison_mv_omega else 0
      mdthresh_mode <- if (!is.null(input$comparison_mv_mdthresh_mode)) input$comparison_mv_mdthresh_mode else "auto"
      custom_mdthresh <- if (!is.null(input$comparison_mv_mdthresh_mode) && input$comparison_mv_mdthresh_mode == "manual") input$comparison_mv_custom_mdthresh else NULL

      mahal_result <- compute_mahalanobis_distance(
        td$target[, selected_cols, drop = FALSE],
        td$reference[, selected_cols, drop = FALSE],
        lambda = lambda, omega = omega, keep_outliers = FALSE,
        custom_mdthresh = custom_mdthresh, selected_columns = selected_cols, mdthresh_mode = mdthresh_mode
      )

      iso_result <- compute_isolation_forest(
        td$target[, selected_cols, drop = FALSE],
        td$reference[, selected_cols, drop = FALSE],
        selected_columns = selected_cols, keep_outliers = FALSE
      )

      cat("=== MULTIVARIATE ANALYSIS RESULTS ===\n")
      cat("Target:", td$target_name, "| Reference:", td$reference_name, "\n")
      cat("Columns used:", paste(selected_cols, collapse = ", "), "\n\n")

      if (!is.null(mahal_result)) {
        cat("📊 Mahalanobis Distance:\n")
        cat("  Total points analyzed:", mahal_result$total_points, "\n")
        cat("  Degrees of freedom:", mahal_result$df, "\n")
        cat("  MDmean:", round(mahal_result$MDmean, 3), "\n")
        cat("  stdMD:", round(mahal_result$stdMD, 3), "\n")
        cat("  Threshold method:", mahal_result$threshold_method, "\n")
        cat("  MDthresh:", round(mahal_result$MDthresh, 3), "\n")
        cat("  Outliers detected:", mahal_result$outlier_custom, "(", round(mahal_result$outlier_custom / mahal_result$total_points * 100, 1), "%)\n")
      } else {
        cat("📊 Mahalanobis Distance: analysis failed\n")
      }

      cat("\n")

      if (!is.null(iso_result)) {
        # compute_isolation_forest() returns outlier_indices, not
        # total_points/outlier_count directly.
        iso_total_points <- length(iso_result$outlier_indices)
        iso_outlier_count <- sum(iso_result$outlier_indices, na.rm = TRUE)
        cat("🌲 Isolation Forest:\n")
        cat("  Total points analyzed:", iso_total_points, "\n")
        cat("  Threshold value:", round(iso_result$threshold, 3), "\n")
        cat("  Outliers detected:", iso_outlier_count, "(", round(iso_outlier_count / iso_total_points * 100, 1), "%)\n")
      } else {
        cat("🌲 Isolation Forest: analysis failed\n")
      }

      cat("\n💡 Interpretation:\n")
      if (td$target_name == td$reference_name) {
        cat("- Self-reference: points flagged as outliers stand out within", td$target_name, "itself.\n")
      } else {
        cat("- Cross-reference: points in", td$target_name, "are flagged relative to", td$reference_name, "'s distribution.\n")
      }
      cat("- Mahalanobis assumes multivariate normality; Isolation Forest makes no such assumption and can catch non-linear anomalies.\n")

    }, error = function(e) {
      cat("❌ Error in multivariate analysis:", e$message, "\n")
      log_operation("ERROR", "Comprehensive multivariate analysis failed", e$message)
    })
  })
}
