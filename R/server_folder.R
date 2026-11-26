# ---- Server: "Folder Summary" tab ----
# Wires R/folder_pipeline.R's run_folder_summary() into the app: many
# uploaded files, column choices from the columns they share, acceptance
# limits, pre-analysis filter and the button; tables, charts and downloads
# are at the end of this file.
#
# NOTE: shiny::validate()/need() must stay fully qualified here - see
# server_spatial.R header comment for why.

#' Wire up the Folder Summary tab's server logic
#'
#' @param input The Shiny `input` object.
#' @param output The Shiny `output` object.
#' @param session The Shiny session object.
#' @param rv The app's shared `reactiveValues` object.
#' @param show_message Function to show a user-facing status message.
#' @param log_operation Function to record a structured log entry.
#' @return A list with `module_name`.
#' @export
create_server_folder <- function(input, output, session, rv, show_message, log_operation) {

  # Every file that can be read, and the reason for each that cannot.
  uploaded <- reactive({
    req(input$folder_files)
    files <- input$folder_files
    read <- lapply(seq_len(nrow(files)), function(i) {
      tryCatch(read_particle_table(files$datapath[i], name = files$name[i]), error = function(e) conditionMessage(e))
    })
    failed <- vapply(read, is.character, logical(1))
    shiny::validate(shiny::need(!all(failed), paste("None of the files could be read:", read[[1]])))
    tables <- read[!failed]
    names(tables) <- batch_labels(files$name[!failed])
    list(tables = tables, name = files$name[!failed], datapath = files$datapath[!failed],
         unreadable = data.frame(file = files$name[failed], problem = as.character(unlist(read[failed])), stringsAsFactors = FALSE))
  })

  observe({
    up <- tryCatch(uploaded(), error = function(e) NULL)
    if (is.null(up)) return()
    tables <- up$tables
    cols <- Reduce(intersect, lapply(tables, names))
    numeric_cols <- cols[vapply(cols, function(cn) all(vapply(tables, function(d) is.numeric(d[[cn]]), logical(1))), logical(1))]
    optional <- function(v) c("None" = "none", v)
    updateSelectInput(session, "folder_area_col", choices = numeric_cols, selected = first_match_or_null(numeric_cols, "^area"))
    updateSelectInput(session, "folder_length_col", choices = numeric_cols,
                      selected = first_match_or_null(numeric_cols, "^length|feret.*max|max.*feret"))
    updateSelectInput(session, "folder_breadth_col", choices = numeric_cols,
                      selected = first_match_or_null(numeric_cols, "^breadth|^width|feret.*min|min.*feret"))
    updateSelectInput(session, "folder_direction_col", choices = optional(numeric_cols),
                      selected = first_match_or_null(numeric_cols, "direction|orientation|angle") %||% "none")
    updateSelectInput(session, "folder_field_col", choices = optional(cols),
                      selected = first_match_or_null(cols, "field|frame|fov") %||% "none")
    x_col <- first_match_or_null(numeric_cols, "stage.?x|^x")
    updateSelectInput(session, "folder_x_col", choices = numeric_cols, selected = x_col)
    updateSelectInput(session, "folder_y_col", choices = numeric_cols, selected = first_match_or_null(numeric_cols, "stage.?y|^y"))
    if (!is.null(x_col)) updateSelectInput(session, "folder_coord_unit", selected = if (grepl("um\\)|\u00b5m", x_col)) "um" else "mm")
    updateSelectizeInput(session, "folder_filter_cols", choices = numeric_cols)
  })

  output$folder_file_info <- renderText({
    up <- uploaded()
    paste(c(sprintf("%d files read, %d rows in all.", length(up$tables), sum(vapply(up$tables, nrow, integer(1)))),
            if (nrow(up$unreadable)) sprintf("Not read: %s (%s)", up$unreadable$file, up$unreadable$problem)), collapse = "\n")
  })
  output$folder_filter_inputs <- renderUI(render_pre_filter_inputs(session$ns, "folder", input$folder_filter_cols))

  result <- eventReactive(input$folder_run, {
    up <- uploaded()
    req(input$folder_area_col)
    optional <- function(v) if (is.null(v) || identical(v, "none") || !nzchar(v)) NULL else v
    cols <- list(area = input$folder_area_col, length = input$folder_length_col, breadth = input$folder_breadth_col,
                 x = input$folder_x_col, y = input$folder_y_col,
                 direction = optional(input$folder_direction_col), field = optional(input$folder_field_col))
    filters <- collect_pre_filters(input, "folder", input$folder_filter_cols)
    acc <- tryCatch(read_acceptance_limits(input, "folder"), error = function(e) shiny::validate(e$message))
    num_or <- function(v, default) if (is.null(v) || !is.finite(v)) default else v
    settings <- list(cleanliness = list(standard = input$folder_standard %||% "ISO 4967",
                                        coord_unit = input$folder_coord_unit %||% "mm",
                                        beam_kv = num_or(input$folder_beam_kv, 15), n_boot = 1000),
                     fields_per_area = acc$fields_per_area, return_period = acc$return_period)

    n <- length(up$tables)
    res <- shiny::withProgress(message = "Analysing files", value = 0, {
      tryCatch(run_folder_summary(up$tables, cols, settings, acc$limits, filters,
                                  progress = function(name) shiny::incProgress(1 / n, detail = name)),
               error = function(e) shiny::validate(paste("Folder summary failed:", e$message)))
    })
    res$files <- list(name = up$name, datapath = up$datapath, rows = vapply(up$tables, nrow, integer(1)))
    log_operation("INFO", "Folder summary", sprintf("%d files, %d analysed", n, sum(res$summary$status == "ok")))
    res
  })

  placeholder <- "Select the files, check the columns, and click \"Analyse All Files\"."
  download_placeholder <- "Select the files and click \"Analyse All Files\" before downloading."
  tables <- reactive(folder_display_tables(result()))

  output$folder_status <- renderText({
    render_gated_status(input, "folder_run", placeholder, function() {
      s <- result()$summary
      verdicts <- s$verdict[s$status == "ok" & !is.na(s$verdict)]
      sprintf("%d of %d files analysed.%s%s", sum(s$status == "ok"), nrow(s),
              if (any(s$status != "ok")) sprintf(" Not analysed: %s.", paste(s$file[s$status != "ok"], collapse = ", ")) else "",
              if (length(verdicts)) sprintf(" Acceptance: %s.", paste(sprintf("%d %s", as.integer(table(verdicts)), names(table(verdicts))), collapse = ", ")) else "")
    })
  })

  output$folder_overview_table <- renderTable(tables()$overview, align = "l")
  output$folder_failed_table <- renderTable(tables()$failed, align = "l")
  output$folder_ratings_table <- renderTable(tables()$ratings, align = "l")
  output$folder_acceptance_table <- renderTable(tables()$acceptance, align = "l")
  output$folder_warnings_table <- renderTable(tables()$warnings, align = "l")

  file_chart <- function(value, lower, upper, label, title, missing_msg) {
    ok <- result()$summary
    ok <- ok[ok$status == "ok", , drop = FALSE]
    shiny::validate(shiny::need(nrow(ok) > 0 && any(!is.na(ok[[value]])), missing_msg))
    names(ok)[names(ok) == "file"] <- "batch"
    with_interval <- !all(is.na(ok[[lower]]))
    print(create_batch_interval_chart(ok, value, if (with_interval) lower, if (with_interval) upper, label, title) +
            ggplot2::labs(x = "File", subtitle = "Point = estimate, bar = 95% interval, dashed line = mean of the files") +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)))
  }
  output$folder_density_plot <- renderPlot(file_chart("per_mm2", "per_mm2_lower", "per_mm2_upper", "Particles per mm2",
                                                      "Number density", "No file could be analysed."))
  output$folder_area_plot <- renderPlot(file_chart("area_ppm", "area_ppm_lower", "area_ppm_upper", "Area fraction (ppm)",
                                                   "Area fraction", "No file could be analysed."))
  output$folder_evs_plot <- renderPlot(file_chart("return_level_um", "return_level_lower", "return_level_upper",
                                                  "Return level sqrt(area) (um)", "Largest inclusion expected",
                                                  "The largest-inclusion estimate needs a SEM field ID column and enough control areas."))

  output$folder_download_tables <- downloadHandler(
    filename = function() paste0("folder_summary_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"),
    content = function(file) {
      res <- safe_reactive_result(function() result(), download_placeholder)
      writexl::write_xlsx(folder_result_tables(res), file)
    }
  )
  output$folder_download_report <- downloadHandler(
    filename = function() paste0("folder_summary_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html"),
    content = function(file) {
      res <- safe_reactive_result(function() result(), download_placeholder)
      writeLines(folder_report(res, report_provenance(res$files$name, res$files$datapath, res$files$rows)), file, useBytes = TRUE)
    }
  )
  output$folder_download_archive <- downloadHandler(
    filename = function() paste0("folder_reports_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip"),
    content = function(file) {
      res <- safe_reactive_result(function() result(), download_placeholder)
      folder_report_archive(res, file)
    }
  )

  register_settings_io(input, output, session, tab = "folder", prefix = "folder")

  return(list(
    module_name = "server_folder"
  ))
}
