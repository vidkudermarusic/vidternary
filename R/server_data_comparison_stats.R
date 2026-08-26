# ---- Server Data Comparison Module: Descriptive Stats & Correlation ----
# Split out of server_data_comparison.R: descriptive-statistics and
# correlation-analysis handlers, operating on whichever datasets the user
# has selected via comparison_selected (populated by
# server_data_comparison_upload.R) - one dataset shows single-dataset
# stats/heatmap, two or more show a side-by-side comparison. Not limited to
# exactly two datasets.
#
# Descriptive/correlation output is rendered as DT::datatable (sortable,
# searchable, with built-in CSV/Excel export buttons) via the helpers in
# stats_display_utils.R, instead of printing summary()/cor() dumps into
# verbatimTextOutput - plus a row of quick-scan stat "cards", a per-row
# mini-histogram column (single-dataset tables only - a comparison table's
# rows don't map 1:1 to one data frame's raw column), and a real
# correlation heatmap via create_correlation_plot() in plotting_utils.R.
#
# Every DT::renderDataTable() call below passes server = FALSE. DT's
# renderDataTable() defaults to server = TRUE (server-side processing), in
# which the browser only ever holds the rows for the currently visible
# page - so even with exportOptions.modifier.page = "all" set on the
# Copy/CSV/Excel buttons (in render_stats_datatable()), there is no "rest
# of the data" client-side to export: the Excel/CSV export silently
# contained only the first page's rows. These tables are small (at most a
# few hundred rows), so server-side processing buys nothing here and
# server = FALSE is required for a full-table export to actually work.

register_data_comparison_stats_handlers <- function(input, output, session, rv, show_message, log_operation) {

  descriptive_round_cols <- c("Min", "Q1", "Median", "Mean", "SD", "Q3", "Max", "CV_pct")

  observeEvent(input$compute_stats, {
    req(rv$comparison_data, input$comparison_selected)
    tryCatch({
      selected <- input$comparison_selected
      dfs <- rv$comparison_data[selected]

      if (length(dfs) == 1) {
        stats_table <- build_descriptive_stats_table(dfs[[1]])
        if (nrow(stats_table) == 0) {
          show_message(paste("No numeric columns found in", names(dfs)[1]), "warning")
          return()
        }
        output$descriptive_stats_cards <- renderUI(build_stat_cards(stats_table))
        table_with_dist <- add_distribution_column(stats_table, dfs[[1]])
        output$descriptive_stats_output <- DT::renderDataTable(render_stats_datatable(table_with_dist, descriptive_round_cols, escape_html = FALSE), server = FALSE)
        log_operation("SUCCESS", paste("Computed stats for", names(dfs)[1]), paste("Analyzed", nrow(stats_table), "numeric columns"))
      } else {
        comparison_table <- build_descriptive_stats_comparison_table(dfs)
        if (is.null(comparison_table) || nrow(comparison_table) == 0) {
          show_message("Selected datasets need numeric columns for comparison", "warning")
          return()
        }
        output$descriptive_stats_cards <- renderUI(build_stat_cards(comparison_table))
        output$descriptive_stats_output <- DT::renderDataTable(render_stats_datatable(comparison_table, descriptive_round_cols), server = FALSE)
        log_operation("SUCCESS", "Compared selected datasets", paste("Datasets:", paste(names(dfs), collapse = ", ")))
      }
    }, error = function(e) {
      show_message(paste("Error computing statistics:", e$message), "error")
      log_operation("ERROR", "Failed to compute descriptive statistics", e$message)
    })
  })

  # ---- Correlation Analysis ----

  observeEvent(input$compute_correlations, {
    req(rv$comparison_data, input$comparison_selected)
    tryCatch({
      selected <- input$comparison_selected
      dfs <- rv$comparison_data[selected]

      if (length(dfs) == 1) {
        nm <- names(dfs)[1]
        pairs_table <- build_correlation_pairs_table(dfs[[1]])
        if (nrow(pairs_table) == 0) {
          show_message("Need at least 2 numeric columns for correlation analysis", "warning")
          return()
        }
        numeric_cols <- names(dfs[[1]])[sapply(dfs[[1]], is.numeric)]
        output$correlation_heatmap <- renderPlot(create_correlation_plot(dfs[[1]][, numeric_cols, drop = FALSE], title = paste(nm, "Correlation Heatmap")))
        output$correlation_output <- DT::renderDataTable(render_stats_datatable(pairs_table, "Correlation"), server = FALSE)
        log_operation("SUCCESS", paste("Computed correlations for", nm), paste("Pairs analyzed:", nrow(pairs_table)))
      } else {
        common_cols <- Reduce(intersect, lapply(dfs, function(d) names(d)[sapply(d, is.numeric)]))
        if (length(common_cols) < 2) {
          show_message("Need at least 2 common numeric columns across the selected datasets", "warning")
          return()
        }

        # The comparison table works for any number of datasets, but a
        # side-by-side heatmap only stays readable for exactly two - beyond
        # that the panels get too small to read the labels/colors. So the
        # heatmap is limited to a 2-dataset selection, with a clear warning
        # (not a silent skip) when more are selected; the table below still
        # covers every selected dataset regardless.
        if (length(dfs) > 2) {
          show_message(paste0("Correlation heatmap needs exactly 2 selected datasets (", length(dfs), " selected) - narrow your selection to 2 to see it. The comparison table below still covers all ", length(dfs), " datasets."), "warning")
          output$correlation_heatmap <- renderPlot({
            plot.new()
            text(0.5, 0.5, paste0("Heatmap needs exactly 2 selected datasets (", length(dfs), " currently selected).\nSelect only 2 datasets above to see the heatmap.\n\nThe comparison table below still covers all ", length(dfs), " datasets."), cex = 1, col = "grey30")
          })
        } else {
          output$correlation_heatmap <- renderPlot({
            graphics::par(mfrow = c(1, 2))
            for (nm in names(dfs)) {
              create_correlation_plot(dfs[[nm]][, common_cols, drop = FALSE], title = nm)
            }
          })
        }

        comparison_table <- build_correlation_comparison_table(dfs, common_cols)
        output$correlation_output <- DT::renderDataTable(render_stats_datatable(comparison_table, "Correlation"), server = FALSE)
        log_operation("SUCCESS", "Compared correlations", paste("Datasets:", paste(names(dfs), collapse = ", "), "| Common columns:", length(common_cols)))
      }
    }, error = function(e) {
      show_message(paste("Error computing correlations:", e$message), "error")
      log_operation("ERROR", "Failed to compute correlations", e$message)
    })
  })
}
