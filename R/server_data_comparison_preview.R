# ---- Server Data Comparison Module: Validation & Excel Preview ----
# Split out of server_data_comparison.R: missing-value/outlier summaries and
# raw Excel previews for whichever loaded dataset the user picks via
# comparison_preview_target (populated by server_data_comparison_upload.R).
# data_readiness_status lives in server_data_comparison_upload.R now - the
# original single file had it duplicated here and in the stats module (the
# second registration silently winning); that duplication is gone now that
# both modules read from the same rv$comparison_data instead of each
# re-reading xlsx_file1/xlsx_file2 independently.

register_data_comparison_preview_handlers <- function(input, output, session, rv, show_message, log_operation) {

  # Per-column outlier counts for one method - mirrors the row-level flag
  # helpers in statistical_filters.R (get_iqr/zscore/mad_outlier_flags), but
  # counted per column rather than OR'd across columns into one row flag,
  # since this report shows "how many outliers did each column contribute"
  # rather than "which rows are outliers overall". Positive outliers only
  # (values above the upper bound), matching this app's convention elsewhere.
  per_column_outlier_counts <- function(df, numeric_cols, method) {
    sapply(df[, numeric_cols, drop = FALSE], function(x) {
      if (length(x) < 4 || !is.numeric(x)) return(0)
      flagged <- switch(method,
        iqr = {
          Q1 <- quantile(x, 0.25, na.rm = TRUE)
          Q3 <- quantile(x, 0.75, na.rm = TRUE)
          x > Q3 + 1.5 * (Q3 - Q1)
        },
        zscore = {
          z <- as.numeric(scale(x))
          z > 3
        },
        mad = {
          med <- median(x, na.rm = TRUE)
          mad_val <- mad(x, na.rm = TRUE)
          x > med + 3 * mad_val
        }
      )
      sum(flagged, na.rm = TRUE)
    })
  }

  missing_outlier_summary <- function(df, label) {
    tryCatch({
      numeric_cols <- sapply(df, is.numeric)
      if (sum(numeric_cols) == 0) {
        cat("No numeric columns found in", label, "\n")
        return(invisible())
      }

      cat("=== ", label, " Missing/Outlier Summary ===\n", sep = "")

      missing_summary <- sapply(df[, numeric_cols, drop = FALSE], function(x) sum(is.na(x)))
      cat("Missing values per column:\n")
      print(missing_summary[missing_summary > 0])

      # All three statistical methods, printed one below another and
      # clearly labeled, so it's unambiguous which method produced which
      # counts - each uses the same "positive outliers only" convention and
      # default threshold as the main Ternary Plots tab's Statistical
      # Filtering section (IQR: 1.5xIQR, Z-score: 3, MAD: 3xMAD).
      cat("\n--- Outliers per column: IQR method (> Q3 + 1.5xIQR) ---\n")
      iqr_summary <- per_column_outlier_counts(df, numeric_cols, "iqr")
      print(iqr_summary[iqr_summary > 0])

      cat("\n--- Outliers per column: Z-Score method (z-score > 3) ---\n")
      zscore_summary <- per_column_outlier_counts(df, numeric_cols, "zscore")
      print(zscore_summary[zscore_summary > 0])

      cat("\n--- Outliers per column: MAD method (> median + 3xMAD) ---\n")
      mad_summary <- per_column_outlier_counts(df, numeric_cols, "mad")
      print(mad_summary[mad_summary > 0])

      cat("\n=== Data Quality Summary ===\n")
      cat("Total rows:", nrow(df), "\n")
      cat("Numeric columns:", sum(numeric_cols), "\n")
      cat("Complete cases:", sum(complete.cases(df[, numeric_cols, drop = FALSE])), "\n")
      cat("Complete case percentage:", round(sum(complete.cases(df[, numeric_cols, drop = FALSE])) / nrow(df) * 100, 1), "%\n")
    }, error = function(e) {
      cat("Error computing validation for", label, ":", e$message, "\n")
    })
  }

  output$comparison_preview_validation <- renderPrint({
    req(rv$comparison_data, input$comparison_preview_target)
    df <- rv$comparison_data[[input$comparison_preview_target]]
    req(df)
    missing_outlier_summary(df, input$comparison_preview_target)
  })

  output$comparison_preview_excel <- DT::renderDataTable({
    req(rv$comparison_data, input$comparison_preview_target)
    rv$comparison_data[[input$comparison_preview_target]]
  })

  selected_preview <- reactiveVal(NULL)
  observeEvent(input$show_missing_selected, { selected_preview("missing") })
  observeEvent(input$show_excel_selected, { selected_preview("excel") })
  observeEvent(input$comparison_preview_target, { selected_preview(NULL) })

  output$comparison_preview_output <- renderUI({
    sel <- selected_preview()
    if (is.null(sel)) return(NULL)
    switch(sel,
           missing = verbatimTextOutput(session$ns("comparison_preview_validation")),
           excel = DT::dataTableOutput(session$ns("comparison_preview_excel")),
           NULL
    )
  })
}
