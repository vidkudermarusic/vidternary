# ---- Server Data Comparison Module: Outlier Detection ----
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
# NOTE: multivariate_analysis() (helpers_multivariate.R) returns a list
# with nested $mahalanobis_results/$isolation_forest_results, not fields
# like $method/$total_points at the top level - so the "Comprehensive
# Analysis Results" panel below calls
# compute_mahalanobis_distance()/compute_isolation_forest() directly
# instead, the same way the two single-method buttons already do.

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

  # Everything below runs once, now, inside this observeEvent - not inside
  # the renderPrint() at the end, whose body must have no reactive reads of
  # its own. Assigning a renderPrint() inside an observeEvent only controls
  # when a new render function is installed, not when it re-fires; if its
  # body read inputs like lambda/omega directly, those become live
  # dependencies and it would recompute on every parameter tweak with no
  # click required.
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

    result <- tryCatch({
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

      report_text <- capture.output({
        cat("=== MAHALANOBIS DISTANCE ANALYSIS ===\n")
        cat("Target:", td$target_name, "| Reference:", td$reference_name, "\n")
        cat("Columns:", paste(selected_cols, collapse = ", "), "\n")
        cat("Target rows:", nrow(td$target), "| Reference rows:", nrow(td$reference), "\n\n")

        if (!is.null(mahal_result)) {
          cat(" Analysis completed successfully!\n\n")
          cat("Threshold method:", mahal_result$threshold_method, "\n")
          cat("Threshold value:", round(mahal_result$MDthresh, 3), "\n")
          cat("Total points analyzed:", mahal_result$total_points, "\n")
          cat("Outliers detected:", mahal_result$outlier_custom, "\n")
          cat("Outlier percentage:", round(mahal_result$outlier_custom / mahal_result$total_points * 100, 1), "%\n")
          cat("Degrees of freedom:", mahal_result$df, "\n")
          cat("MDmean:", round(mahal_result$MDmean, 3), "\n")
          cat("stdMD:", round(mahal_result$stdMD, 3), "\n")
          if (!is.null(mahal_result$threshold_formula)) {
            cat("\nThreshold formula:", mahal_result$threshold_formula, "\n")
          }
        } else {
          cat(" Analysis failed. Please check data quality.\n")
        }
      })

      log_operation("SUCCESS", "Mahalanobis analysis completed", paste("Target:", td$target_name, "Reference:", td$reference_name, "Columns:", length(selected_cols)))
      paste(report_text, collapse = "\n")
    }, error = function(e) {
      log_operation("ERROR", "Mahalanobis analysis failed", e$message)
      paste("Error in Mahalanobis analysis:", e$message)
    })

    output$mahalanobis_output <- renderPrint(cat(result, "\n"))
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

    # Captured once, now, from the button-click-time inputs (same pattern
    # as lambda/omega below in the comprehensive panel) - NA-safety guard
    # matches every other numericInput read in this file, since a cleared
    # field reports as NA_real_, not NULL.
    ntrees <- if (!is.null(input$comparison_iso_ntrees) && !is.na(input$comparison_iso_ntrees)) input$comparison_iso_ntrees else 200
    contamination <- if (!is.null(input$comparison_iso_contamination) && !is.na(input$comparison_iso_contamination)) input$comparison_iso_contamination else 0.10

    # renderPrint({...}) only builds and returns a render closure; it does
    # NOT execute the body at assignment time (Shiny calls it later, on
    # flush). So a tryCatch wrapping only the assignment can never catch an
    # error raised from inside the render body. Matching the
    # mahalanobis_analysis observer just above (and the comprehensive panel
    # below): compute everything and capture it as text INSIDE the
    # tryCatch, then assign only a trivial renderPrint() over the
    # already-computed text afterward, so the render closure's own body has
    # nothing left that can fail.
    result <- tryCatch({
      iso_result <- compute_isolation_forest(
        td$target[, selected_cols, drop = FALSE],
        td$reference[, selected_cols, drop = FALSE],
        selected_columns = selected_cols,
        keep_outliers = FALSE,
        ntrees = ntrees, contamination = contamination
      )

      report_text <- capture.output({
        cat("=== ISOLATION FOREST ANALYSIS ===\n")
        cat("Target:", td$target_name, "| Reference:", td$reference_name, "\n")
        cat("Columns:", paste(selected_cols, collapse = ", "), "\n")
        cat("Target rows:", nrow(td$target), "| Reference rows:", nrow(td$reference), "\n\n")

        if (!is.null(iso_result)) {
          # compute_isolation_forest() returns outlier_indices/threshold/
          # contamination/columns_used - not total_points/outlier_count/
          # threshold_method (those are compute_mahalanobis_distance()
          # fields; this analysis was silently printing blanks for them).
          total_points <- length(iso_result$outlier_indices)
          outlier_count <- sum(iso_result$outlier_indices, na.rm = TRUE)
          cat(" Analysis completed successfully!\n\n")
          # Model parameters reported explicitly - ntrees/contamination are
          # user-adjustable; sample_size is read back from the result
          # itself rather than assumed, since it always equals the
          # reference's own complete-row count for the selected columns,
          # not a value chosen here.
          cat("Trees:", iso_result$ntrees, "| Contamination:", iso_result$contamination,
              "| Sample size (reference rows used):", iso_result$sample_size, "\n")
          cat("Threshold method: Quantile of reference scores at (1 - contamination) =", iso_result$contamination, "\n")
          cat("Threshold value:", round(iso_result$threshold, 3), "\n")
          cat("Total points analyzed:", total_points, "\n")
          cat("Outliers detected:", outlier_count, "\n")
          cat("Outlier percentage:", round(outlier_count / total_points * 100, 1), "%\n")
        } else {
          cat(" Analysis failed. Please check data quality.\n")
        }
      })

      log_operation("SUCCESS", "Isolation Forest analysis completed", paste("Target:", td$target_name, "Reference:", td$reference_name, "Columns:", length(selected_cols)))
      paste(report_text, collapse = "\n")
    }, error = function(e) {
      log_operation("ERROR", "Isolation Forest analysis failed", e$message)
      paste("Error in Isolation Forest analysis:", e$message)
    })

    output$isolation_forest_output <- renderPrint(cat(result, "\n"))
  })

  # ---- Comprehensive Outlier Detection Display ----
  # Gated behind its own "Run Comprehensive Analysis" button, matching the
  # two single-method panels above.

  observeEvent(input$comparison_mv_run_comprehensive, {
    req(rv$comparison_data, input$comparison_mv_target, input$comparison_mv_columns)
    td <- resolve_target_reference()
    req(td)
    selected_cols <- input$comparison_mv_columns

    col_error <- validate_mv_columns(td, selected_cols)
    if (!is.null(col_error)) {
      output$mahalanobis_info <- renderPrint(cat("", col_error))
      return()
    }

    # Everything from here down runs once, now, inside this observeEvent -
    # not inside the renderPrint() below (see the comment above the
    # mahalanobis_analysis observeEvent for why).
    result <- tryCatch({
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

      iso_ntrees <- if (!is.null(input$comparison_iso_ntrees) && !is.na(input$comparison_iso_ntrees)) input$comparison_iso_ntrees else 200
      iso_contamination <- if (!is.null(input$comparison_iso_contamination) && !is.na(input$comparison_iso_contamination)) input$comparison_iso_contamination else 0.10
      iso_result <- compute_isolation_forest(
        td$target[, selected_cols, drop = FALSE],
        td$reference[, selected_cols, drop = FALSE],
        selected_columns = selected_cols, keep_outliers = FALSE,
        ntrees = iso_ntrees, contamination = iso_contamination
      )

      report_text <- capture.output({
        cat("=== OUTLIER DETECTION RESULTS (Mahalanobis distance & Isolation Forest) ===\n")
        cat("Target:", td$target_name, "| Reference:", td$reference_name, "\n")
        cat("Columns used:", paste(selected_cols, collapse = ", "), "\n\n")

        if (!is.null(mahal_result)) {
          cat(" Mahalanobis Distance:\n")
          cat("  Total points analyzed:", mahal_result$total_points, "\n")
          cat("  Degrees of freedom:", mahal_result$df, "\n")
          cat("  MDmean:", round(mahal_result$MDmean, 3), "\n")
          cat("  stdMD:", round(mahal_result$stdMD, 3), "\n")
          cat("  Threshold method:", mahal_result$threshold_method, "\n")
          cat("  MDthresh:", round(mahal_result$MDthresh, 3), "\n")
          cat("  Outliers detected:", mahal_result$outlier_custom, "(", round(mahal_result$outlier_custom / mahal_result$total_points * 100, 1), "%)\n")
        } else {
          cat(" Mahalanobis Distance: analysis failed\n")
        }

        cat("\n")

        if (!is.null(iso_result)) {
          # compute_isolation_forest() returns outlier_indices, not
          # total_points/outlier_count directly.
          iso_total_points <- length(iso_result$outlier_indices)
          iso_outlier_count <- sum(iso_result$outlier_indices, na.rm = TRUE)
          cat(" Isolation Forest:\n")
          cat("  Trees:", iso_result$ntrees, "| Contamination:", iso_result$contamination,
              "| Sample size (reference rows used):", iso_result$sample_size, "\n")
          cat("  Total points analyzed:", iso_total_points, "\n")
          cat("  Threshold value:", round(iso_result$threshold, 3), "\n")
          cat("  Outliers detected:", iso_outlier_count, "(", round(iso_outlier_count / iso_total_points * 100, 1), "%)\n")
        } else {
          cat(" Isolation Forest: analysis failed\n")
        }

        cat("\n Interpretation:\n")
        if (td$target_name == td$reference_name) {
          cat("- Self-reference: points flagged as outliers stand out within", td$target_name, "itself.\n")
        } else {
          cat("- Cross-reference: points in", td$target_name, "are flagged relative to", td$reference_name, "'s distribution.\n")
        }
        cat("- Mahalanobis assumes multivariate normality; Isolation Forest makes no such assumption and can catch non-linear anomalies.\n")
      })

      log_operation("SUCCESS", "Comprehensive multivariate analysis completed", paste("Target:", td$target_name, "Reference:", td$reference_name, "Columns:", length(selected_cols)))
      paste(report_text, collapse = "\n")
    }, error = function(e) {
      log_operation("ERROR", "Comprehensive multivariate analysis failed", e$message)
      paste(" Error in multivariate analysis:", e$message)
    })

    output$mahalanobis_info <- renderPrint(cat(result, "\n"))
  })
}
