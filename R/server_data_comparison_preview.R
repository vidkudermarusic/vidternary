# ---- Server Data Comparison Module: Validation & Excel Preview ----
# Split out of server_data_comparison.R: missing-value/outlier summaries,
# raw Excel previews, and the dynamic analysis-buttons/output-switcher UI.
#
# NOTE: output$data_readiness_status is assigned again here, overriding the
# version in server_data_comparison_stats.R (pre-existing behavior in the
# original single file - this version re-reads the raw uploaded files
# directly rather than relying on cached rv$df1/df2). Preserved as-is;
# register_data_comparison_stats_handlers() must run first in
# server_data_comparison.R for this override order to hold.

register_data_comparison_preview_handlers <- function(input, output, session, rv, show_message, log_operation) {

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
}
