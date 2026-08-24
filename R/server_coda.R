# ---- Server: "Compositional Analysis" tab ----
# Wires R/compositional_data_analysis.R's pure statistics/plotting
# functions into the app. Structure mirrors server_evs.R/server_spatial.R
# (combined-data reactive, column-choice observer, eventReactive for the
# transform+PCA).
#
# NOTE: shiny::validate()/shiny::need() must be fully qualified in this
# package - `import(jsonlite)` in NAMESPACE masks the unqualified name
# with jsonlite::validate, as discovered while building the Plot Builder
# tab.

create_server_coda <- function(input, output, session, rv, show_message, log_operation, directory_management = NULL) {

  combined_data <- reactive({
    req(input$coda_files)
    n_files <- nrow(input$coda_files)
    dfs <- lapply(seq_len(n_files), function(i) {
      d <- tryCatch(openxlsx::read.xlsx(input$coda_files$datapath[i], sheet = 1), error = function(e) NULL)
      if (is.null(d)) return(NULL)
      if (n_files > 1) d$source_file <- tools::file_path_sans_ext(input$coda_files$name[i])
      d
    })
    dfs <- Filter(Negate(is.null), dfs)
    shiny::validate(shiny::need(length(dfs) > 0, "None of the selected files could be read."))
    if (length(dfs) == 1) return(dfs[[1]])
    common_cols <- Reduce(intersect, lapply(dfs, names))
    shiny::validate(shiny::need(length(common_cols) > 0, "The selected files have no columns in common."))
    do.call(rbind, lapply(dfs, function(d) d[, common_cols, drop = FALSE]))
  })

  observe({
    d <- tryCatch(combined_data(), error = function(e) NULL)
    if (is.null(d)) return()
    numeric_cols <- names(d)[sapply(d, is.numeric)]
    wt_cols <- numeric_cols[grepl("wt%", numeric_cols, ignore.case = TRUE)]
    updateSelectizeInput(session, "coda_parts", choices = numeric_cols,
                          selected = if (length(wt_cols) >= 3) wt_cols else NULL)
  })

  result <- eventReactive(input$coda_run, {
    d <- combined_data()
    parts <- input$coda_parts
    shiny::validate(shiny::need(!is.null(parts) && length(parts) >= 3, "Select at least 3 compositional parts (columns)."))
    shiny::validate(shiny::need(all(parts %in% names(d)), "One or more selected columns are not present in the data."))

    d_complete <- d[stats::complete.cases(d[, parts, drop = FALSE]), , drop = FALSE]
    shiny::validate(shiny::need(nrow(d_complete) >= length(parts) + 1,
                                 "Not enough complete rows (with no missing values in the selected parts) to run PCA."))

    clr <- tryCatch(clr_transform(d_complete, parts), error = function(e) { shiny::validate(paste("Error computing CLR:", e$message)) })
    ilr_res <- tryCatch(ilr_transform(d_complete, parts), error = function(e) { shiny::validate(paste("Error computing ILR:", e$message)) })
    pca <- tryCatch(compositional_pca(clr), error = function(e) { shiny::validate(paste("Error running PCA:", e$message)) })

    list(clr = clr, ilr = ilr_res$ilr, pca = pca, n = nrow(d_complete), parts = parts)
  })

  output$coda_status <- renderText({
    res <- tryCatch(result(), error = function(e) NULL)
    if (is.null(res)) return("Upload data, select 3 or more compositional (Wt%) columns, and click \"Transform & Run PCA\".")
    sprintf("PCA complete: n = %d complete rows, %d parts. PC1+PC2 explain %.1f%% of the (Aitchison) variance.",
            res$n, length(res$parts), sum(res$pca$var_explained[1:2]))
  })

  output$coda_biplot <- renderPlot({
    print(create_coda_biplot(result()$pca))
  })

  output$coda_variance_table <- renderTable({
    pca <- result()$pca
    data.frame(
      Component = paste0("PC", seq_along(pca$var_explained)),
      Variance_pct = sprintf("%.2f%%", pca$var_explained),
      Cumulative_pct = sprintf("%.2f%%", cumsum(pca$var_explained))
    )
  })

  output$coda_loadings_table <- renderTable({
    loadings <- result()$pca$loadings
    df <- data.frame(Element = rownames(loadings), loadings, row.names = NULL, check.names = FALSE)
    df
  })

  output$coda_download_clr <- downloadHandler(
    filename = function() paste0("coda_clr_transformed_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"),
    content = function(file) writexl::write_xlsx(result()$clr, file)
  )

  output$coda_download_ilr <- downloadHandler(
    filename = function() paste0("coda_ilr_transformed_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"),
    content = function(file) writexl::write_xlsx(result()$ilr, file)
  )

  output$coda_download_biplot <- downloadHandler(
    filename = function() paste0("coda_biplot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      ggplot2::ggsave(file, plot = create_coda_biplot(result()$pca), width = 9, height = 7, dpi = 300)
    }
  )

  return(list(
    module_name = "server_coda"
  ))
}
