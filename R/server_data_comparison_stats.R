# ---- Server Data Comparison Module: Descriptive Stats & Correlation ----
# Split out of server_data_comparison.R: the initial data-readiness check,
# descriptive-statistics buttons, and correlation-analysis buttons (as
# opposed to the multivariate analysis handlers in
# server_data_comparison_multivariate.R, or the validation/preview renderers
# in server_data_comparison_preview.R).
#
# Descriptive/correlation output is rendered as DT::datatable (sortable,
# searchable, with built-in CSV/Excel export buttons) via the helpers in
# stats_display_utils.R, instead of printing summary()/cor() dumps into
# verbatimTextOutput - plus a row of quick-scan stat "cards", a per-row
# mini-histogram column (single-dataset tables only - a comparison table's
# rows don't map 1:1 to one data frame's raw column), and a real
# correlation heatmap via the existing (previously unused)
# create_correlation_plot() in plotting_utils.R.
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

  descriptive_round_cols <- c("Min", "Q1", "Median", "Mean", "SD", "Q3", "Max", "CV_pct")

  observeEvent(input$compute_stats1, {
    req(rv$df1)
    tryCatch({
      stats_table <- build_descriptive_stats_table(rv$df1)
      if (nrow(stats_table) == 0) {
        show_message("No numeric columns found in Dataset 1", "warning")
        return()
      }
      rv$stats1 <- stats_table
      output$descriptive_stats_cards <- renderUI(build_stat_cards(stats_table))
      table_with_dist <- add_distribution_column(stats_table, rv$df1)
      output$descriptive_stats_output <- DT::renderDataTable(render_stats_datatable(table_with_dist, descriptive_round_cols, escape_html = FALSE))
      log_operation("SUCCESS", "Computed stats for Dataset 1", paste("Analyzed", nrow(stats_table), "numeric columns"))
    }, error = function(e) {
      show_message(paste("Error computing stats for Dataset 1:", e$message), "error")
      log_operation("ERROR", "Failed to compute stats for Dataset 1", e$message)
    })
  })

  observeEvent(input$compute_stats2, {
    req(rv$df2)
    tryCatch({
      stats_table <- build_descriptive_stats_table(rv$df2)
      if (nrow(stats_table) == 0) {
        show_message("No numeric columns found in Dataset 2", "warning")
        return()
      }
      rv$stats2 <- stats_table
      output$descriptive_stats_cards <- renderUI(build_stat_cards(stats_table))
      table_with_dist <- add_distribution_column(stats_table, rv$df2)
      output$descriptive_stats_output <- DT::renderDataTable(render_stats_datatable(table_with_dist, descriptive_round_cols, escape_html = FALSE))
      log_operation("SUCCESS", "Computed stats for Dataset 2", paste("Analyzed", nrow(stats_table), "numeric columns"))
    }, error = function(e) {
      show_message(paste("Error computing stats for Dataset 2:", e$message), "error")
      log_operation("ERROR", "Failed to compute stats for Dataset 2", e$message)
    })
  })

  observeEvent(input$compare_stats, {
    req(rv$df1, rv$df2)
    tryCatch({
      comparison_table <- build_descriptive_stats_comparison_table(rv$df1, rv$df2)
      if (nrow(comparison_table) == 0) {
        show_message("Both datasets need numeric columns for comparison", "warning")
        return()
      }
      output$descriptive_stats_cards <- renderUI(build_stat_cards(comparison_table))
      output$descriptive_stats_output <- DT::renderDataTable(render_stats_datatable(comparison_table, descriptive_round_cols))
      log_operation("SUCCESS", "Compared both datasets", paste("Variables compared:", length(unique(comparison_table$Variable))))
    }, error = function(e) {
      show_message(paste("Error comparing datasets:", e$message), "error")
      log_operation("ERROR", "Failed to compare datasets", e$message)
    })
  })

  # ---- Correlation Analysis ----

  observeEvent(input$compute_correlations1, {
    req(rv$df1)
    tryCatch({
      pairs_table <- build_correlation_pairs_table(rv$df1)
      if (nrow(pairs_table) == 0) {
        show_message("Need at least 2 numeric columns for correlation analysis", "warning")
        return()
      }
      rv$correlation1 <- pairs_table
      numeric_cols <- names(rv$df1)[sapply(rv$df1, is.numeric)]
      output$correlation_heatmap <- renderPlot(create_correlation_plot(rv$df1[, numeric_cols, drop = FALSE], title = "Dataset 1 Correlation Heatmap"))
      output$correlation_output <- DT::renderDataTable(render_stats_datatable(pairs_table, "Correlation"))
      log_operation("SUCCESS", "Computed correlations for Dataset 1", paste("Pairs analyzed:", nrow(pairs_table)))
    }, error = function(e) {
      show_message(paste("Error computing correlations for Dataset 1:", e$message), "error")
      log_operation("ERROR", "Failed to compute correlations for Dataset 1", e$message)
    })
  })

  observeEvent(input$compute_correlations2, {
    req(rv$df2)
    tryCatch({
      pairs_table <- build_correlation_pairs_table(rv$df2)
      if (nrow(pairs_table) == 0) {
        show_message("Need at least 2 numeric columns for correlation analysis", "warning")
        return()
      }
      rv$correlation2 <- pairs_table
      numeric_cols <- names(rv$df2)[sapply(rv$df2, is.numeric)]
      output$correlation_heatmap <- renderPlot(create_correlation_plot(rv$df2[, numeric_cols, drop = FALSE], title = "Dataset 2 Correlation Heatmap"))
      output$correlation_output <- DT::renderDataTable(render_stats_datatable(pairs_table, "Correlation"))
      log_operation("SUCCESS", "Computed correlations for Dataset 2", paste("Pairs analyzed:", nrow(pairs_table)))
    }, error = function(e) {
      show_message(paste("Error computing correlations for Dataset 2:", e$message), "error")
      log_operation("ERROR", "Failed to compute correlations for Dataset 2", e$message)
    })
  })

  observeEvent(input$compare_correlations, {
    req(rv$df1, rv$df2)
    tryCatch({
      numeric_cols1 <- names(rv$df1)[sapply(rv$df1, is.numeric)]
      numeric_cols2 <- names(rv$df2)[sapply(rv$df2, is.numeric)]
      common_cols <- intersect(numeric_cols1, numeric_cols2)

      if (length(common_cols) < 2) {
        show_message("Need at least 2 common numeric columns for correlation comparison", "warning")
        return()
      }

      comparison_table <- build_correlation_comparison_table(rv$df1, rv$df2, common_cols)
      output$correlation_heatmap <- renderPlot({
        graphics::par(mfrow = c(1, 2))
        create_correlation_plot(rv$df1[, common_cols, drop = FALSE], title = "Dataset 1")
        create_correlation_plot(rv$df2[, common_cols, drop = FALSE], title = "Dataset 2")
      })
      output$correlation_output <- DT::renderDataTable(render_stats_datatable(comparison_table, c("Dataset1_r", "Dataset2_r", "Difference")))
      log_operation("SUCCESS", "Compared correlations", paste("Common columns:", length(common_cols)))
    }, error = function(e) {
      show_message(paste("Error comparing correlations:", e$message), "error")
      log_operation("ERROR", "Failed to compare correlations", e$message)
    })
  })
}
