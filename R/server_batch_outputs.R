# ---- Server: "Batch Comparison" tab outputs ----
# Tables, plots and downloads for the result of run_batch_comparison().
# Split from server_batch.R for size; called once from there.

# Registers the Batch Comparison tab's outputs. `result` is the
# eventReactive from create_server_batch().
register_batch_outputs <- function(input, output, result) {
  placeholder <- "Upload one file per batch, check the columns, and click \"Compare Batches\"."
  download_placeholder <- "Upload the files and click \"Compare Batches\" before downloading."
  tables <- reactive(batch_display_tables(result()))

  output$batch_status <- renderText({
    render_gated_status(input, "batch_run", placeholder, function() {
      res <- result()
      s <- res$summary
      sprintf("%d batches compared: %s. Particles: %s.%s", nrow(s), paste(s$batch, collapse = ", "),
              paste(s$n, collapse = ", "),
              if (is.null(res$composition)) " Composition not compared (no elements chosen)." else
                sprintf(" Composition compared on %d elements.", length(res$parts)))
    })
  })

  # Supplementary boxes: nothing shown without a result (the status line
  # already carries any validation message).
  output$batch_notes <- renderUI({
    res <- tryCatch(result(), error = function(e) NULL)
    if (is.null(res)) return(NULL)
    tagList(
      lapply(res$notes$warnings, function(msg)
        div(style = "color: #856404; background-color: #fff3cd; border: 1px solid #ffeeba; border-radius: 4px; padding: 10px; font-size: 13px; margin: 10px 0;",
            strong("Check: "), msg)),
      tags$ul(style = "font-size: 12px; margin: 6px 0;", lapply(res$notes$notes, tags$li))
    )
  })

  output$batch_summary_table <- renderTable(tables()$summary, align = "l")
  output$batch_overall_table <- renderTable(tables()$overall, align = "l")
  output$batch_density_pairs_table <- renderTable(tables()$density_pairs, align = "l")
  output$batch_size_quantiles_table <- renderTable(tables()$size_quantiles, align = "l")
  output$batch_size_pairs_table <- renderTable(tables()$size_pairs, align = "l")
  output$batch_clr_table <- renderTable(tables()$clr, align = "l")
  output$batch_composition_pairs_table <- renderTable(tables()$composition_pairs, align = "l")
  output$batch_spread_table <- renderTable(tables()$spread, align = "l")
  output$batch_evs_table <- renderTable(tables()$evs, align = "l")

  output$batch_density_plot <- renderPlot({
    print(create_batch_interval_chart(result()$summary, "per_mm2", "per_mm2_lower", "per_mm2_upper",
                                      "Particles per mm2", "Number density"))
  })
  output$batch_area_plot <- renderPlot({
    s <- result()$summary
    with_interval <- !all(is.na(s$area_ppm_lower))
    print(create_batch_interval_chart(s, "area_ppm", if (with_interval) "area_ppm_lower", if (with_interval) "area_ppm_upper",
                                      "Area fraction (ppm)", "Area fraction"))
  })
  output$batch_size_plot <- renderPlot({
    p <- result()$particles
    print(create_size_tail_plot(p$ecd, p$batch))
  })
  output$batch_clr_plot <- renderPlot({
    shiny::validate(shiny::need(!is.null(result()$composition), "Choose two or more elements to compare the composition."))
    print(create_clr_difference_plot(result()$composition$clr_means))
  })
  output$batch_evs_plot <- renderPlot({
    e <- result()$evs
    shiny::validate(shiny::need(!is.null(e), "The largest-inclusion estimate needs a SEM field ID column in every file."))
    shiny::validate(shiny::need(any(!is.na(e$return_level)), "No batch has enough control areas for an extreme-value fit."))
    print(create_batch_interval_chart(e, "return_level", "ci_lower", "ci_upper", "Return level sqrt(area) (um)",
                                      sprintf("Return level for T = %s control areas", format(result()$settings$return_period))))
  })

  script_lines <- function(res) batch_script(res$files$name, res$cols, res$settings, res$filters, batch_names = res$summary$batch)
  output$batch_download_tables <- downloadHandler(
    filename = function() paste0("batch_comparison_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"),
    content = function(file) {
      res <- safe_reactive_result(function() result(), download_placeholder)
      writexl::write_xlsx(batch_result_tables(res), file)
    }
  )
  output$batch_download_report <- downloadHandler(
    filename = function() paste0("batch_comparison_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html"),
    content = function(file) {
      res <- safe_reactive_result(function() result(), download_placeholder)
      provenance <- report_provenance(res$files$name, res$files$datapath, res$files$rows)
      writeLines(batch_report(res, provenance, script_lines(res), res$filters), file, useBytes = TRUE)
    }
  )
  output$batch_download_script <- downloadHandler(
    filename = function() paste0("batch_comparison_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".R"),
    content = function(file) {
      res <- safe_reactive_result(function() result(), download_placeholder)
      writeLines(enc2utf8(script_lines(res)), file, useBytes = TRUE)
    }
  )
  invisible(NULL)
}
