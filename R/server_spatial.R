# ---- Server: "Spatial Clustering" tab ----
# Wires R/spatial_clustering_analysis.R's pure statistics/plotting
# functions into the app. Structure mirrors server_evs.R (combined-data
# reactive, column-choice observer, eventReactive for the fit).
#
# NOTE: shiny::validate()/shiny::need() must be fully qualified in this
# package - `import(jsonlite)` in NAMESPACE masks the unqualified name
# with jsonlite::validate, as discovered while building the Plot Builder
# tab.

create_server_spatial <- function(input, output, session, rv, show_message, log_operation, directory_management = NULL) {

  combined_data <- reactive({
    req(input$spatial_files)
    n_files <- nrow(input$spatial_files)
    dfs <- lapply(seq_len(n_files), function(i) {
      d <- tryCatch(openxlsx::read.xlsx(input$spatial_files$datapath[i], sheet = 1), error = function(e) NULL)
      if (is.null(d)) return(NULL)
      if (n_files > 1) d$source_file <- tools::file_path_sans_ext(input$spatial_files$name[i])
      d
    })
    dfs <- Filter(Negate(is.null), dfs)
    shiny::validate(shiny::need(length(dfs) > 0, "None of the selected files could be read."))
    if (length(dfs) == 1) return(dfs[[1]])
    common_cols <- Reduce(intersect, lapply(dfs, names))
    shiny::validate(shiny::need(length(common_cols) > 0, "The selected files have no columns in common."))
    do.call(rbind, lapply(dfs, function(d) d[, common_cols, drop = FALSE]))
  })

  first_match_or_null <- function(x, pattern) {
    hit <- x[grepl(pattern, x, ignore.case = TRUE)]
    if (length(hit) == 0) NULL else hit[1]
  }

  observe({
    d <- tryCatch(combined_data(), error = function(e) NULL)
    if (is.null(d)) return()
    numeric_cols <- names(d)[sapply(d, is.numeric)]
    updateSelectInput(session, "spatial_x_col", choices = numeric_cols,
                       selected = first_match_or_null(numeric_cols, "^stage.*x"))
    updateSelectInput(session, "spatial_y_col", choices = numeric_cols,
                       selected = first_match_or_null(numeric_cols, "^stage.*y"))
    updateSelectInput(session, "spatial_color_col", choices = c("None" = "none", names(d)))
  })

  result <- eventReactive(input$spatial_analyze, {
    d <- combined_data()
    shiny::validate(shiny::need(!is.null(input$spatial_x_col) && input$spatial_x_col %in% names(d), "Select a valid X coordinate column."))
    shiny::validate(shiny::need(!is.null(input$spatial_y_col) && input$spatial_y_col %in% names(d), "Select a valid Y coordinate column."))

    x <- suppressWarnings(as.numeric(d[[input$spatial_x_col]]))
    y <- suppressWarnings(as.numeric(d[[input$spatial_y_col]]))
    valid <- is.finite(x) & is.finite(y)

    color_by <- NULL
    if (!is.null(input$spatial_color_col) && input$spatial_color_col != "none" && input$spatial_color_col %in% names(d)) {
      color_by <- d[[input$spatial_color_col]][valid]
    }

    ce <- tryCatch(clark_evans_test(x[valid], y[valid]),
                    error = function(e) { shiny::validate(paste("Error running spatial analysis:", e$message)) })
    list(ce = ce, x = x[valid], y = y[valid], color_by = color_by, color_label = input$spatial_color_col)
  })

  output$spatial_status <- renderText({
    res <- tryCatch(result(), error = function(e) NULL)
    if (is.null(res)) return("Upload data, choose X/Y coordinate columns, and click \"Analyze Spatial Pattern\".")
    ce <- res$ce
    sprintf("n = %d points | R = %.3f | Asymptotic p = %.4f | Monte Carlo p = %.4f (n_sim = %d)\n%s",
            ce$n, ce$R, ce$p_value_asymptotic, ce$p_value_monte_carlo, ce$n_sim, ce$verdict)
  })

  output$spatial_scatter_plot <- renderPlot({
    res <- result()
    print(create_spatial_scatter_plot(res$x, res$y, res$color_by, res$color_label))
  })

  output$spatial_nnd_histogram <- renderPlot({
    res <- result()
    print(create_nnd_histogram(res$ce))
  })

  output$spatial_summary_table <- renderTable({
    ce <- result()$ce
    data.frame(
      Metric = c("Points (n)", "Bounding-box area", "Density (points/area)",
                 "Observed mean NND", "Expected mean NND (Donnelly-corrected)",
                 "R statistic", "Z (asymptotic)", "p-value (asymptotic)",
                 "p-value (Monte Carlo)"),
      Value = c(sprintf("%d", ce$n), sprintf("%.4g", ce$area), sprintf("%.6g", ce$density),
                sprintf("%.4f", ce$Dobs), sprintf("%.4f", ce$Dkevin),
                sprintf("%.4f", ce$R), sprintf("%.3f", ce$Z), sprintf("%.4f", ce$p_value_asymptotic),
                sprintf("%.4f", ce$p_value_monte_carlo))
    )
  })

  output$spatial_download_scatter <- downloadHandler(
    filename = function() paste0("spatial_scatter_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      res <- result()
      ggplot2::ggsave(file, plot = create_spatial_scatter_plot(res$x, res$y, res$color_by, res$color_label), width = 8, height = 7, dpi = 300)
    }
  )

  output$spatial_download_histogram <- downloadHandler(
    filename = function() paste0("spatial_nnd_histogram_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      ggplot2::ggsave(file, plot = create_nnd_histogram(result()$ce), width = 8, height = 7, dpi = 300)
    }
  )

  output$spatial_download_table <- downloadHandler(
    filename = function() paste0("spatial_nnd_values_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"),
    content = function(file) {
      res <- result()
      df <- data.frame(x = res$x, y = res$y, nearest_neighbor_distance = res$ce$nnd)
      writexl::write_xlsx(df, file)
    }
  )

  return(list(
    module_name = "server_spatial"
  ))
}
