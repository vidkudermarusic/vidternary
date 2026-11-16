# ---- Server: "Measurement Planning" tab ----
# Wires R/planning_pipeline.R's plan_measurement() into the app: one
# uploaded file per run, column choices from the columns the files share,
# pre-analysis filter and the button. Tables, plot and download are at the
# end of this file.
#
# NOTE: shiny::validate()/need() must stay fully qualified here - see
# server_spatial.R header comment for why.

#' Wire up the Measurement Planning tab's server logic
#'
#' @param input The Shiny `input` object.
#' @param output The Shiny `output` object.
#' @param session The Shiny session object.
#' @param rv The app's shared `reactiveValues` object.
#' @param show_message Function to show a user-facing status message.
#' @param log_operation Function to record a structured log entry.
#' @return A list with `module_name`.
#' @export
create_server_planning <- function(input, output, session, rv, show_message, log_operation) {

  uploaded <- reactive({
    req(input$plan_files)
    files <- input$plan_files
    tables <- lapply(seq_len(nrow(files)), function(i) {
      tryCatch(read_particle_table(files$datapath[i], name = files$name[i]), error = function(e) conditionMessage(e))
    })
    failed <- vapply(tables, is.character, logical(1))
    shiny::validate(shiny::need(!any(failed), paste("Could not read:", paste(sprintf("%s (%s)", files$name[failed], unlist(tables[failed])), collapse = "; "))))
    names(tables) <- batch_labels(files$name)
    tables
  })

  observe({
    tables <- tryCatch(uploaded(), error = function(e) NULL)
    if (is.null(tables)) return()
    cols <- Reduce(intersect, lapply(tables, names))
    numeric_cols <- cols[vapply(cols, function(cn) all(vapply(tables, function(d) is.numeric(d[[cn]]), logical(1))), logical(1))]
    optional <- function(v) c("None" = "none", v)
    updateSelectInput(session, "plan_area_col", choices = numeric_cols, selected = first_match_or_null(numeric_cols, "^area"))
    updateSelectInput(session, "plan_field_col", choices = optional(cols),
                      selected = first_match_or_null(cols, "field|frame|fov") %||% "none")
    x_col <- first_match_or_null(numeric_cols, "stage.?x|^x")
    updateSelectInput(session, "plan_x_col", choices = optional(numeric_cols), selected = x_col %||% "none")
    updateSelectInput(session, "plan_y_col", choices = optional(numeric_cols),
                      selected = first_match_or_null(numeric_cols, "stage.?y|^y") %||% "none")
    if (!is.null(x_col)) updateSelectInput(session, "plan_coord_unit", selected = if (grepl("um\\)|\u00b5m", x_col)) "um" else "mm")
    updateSelectizeInput(session, "plan_filter_cols", choices = numeric_cols)
  })

  output$plan_file_info <- renderText({
    tables <- uploaded()
    paste(sprintf("%s: %d rows", names(tables), vapply(tables, nrow, integer(1))), collapse = "\n")
  })
  output$plan_filter_inputs <- renderUI(render_pre_filter_inputs(session$ns, "plan", input$plan_filter_cols))

  result <- eventReactive(input$plan_run, {
    all_runs <- uploaded()
    filters <- collect_pre_filters(input, "plan", input$plan_filter_cols)
    runs <- tryCatch(lapply(all_runs, apply_pre_filters, filters = filters),
                     error = function(e) shiny::validate(paste("Pre-analysis filter error:", e$message)))
    empty <- names(runs)[vapply(runs, nrow, integer(1)) == 0]
    shiny::validate(shiny::need(length(empty) == 0, paste0("No rows remain after the pre-analysis filter(s) in: ", paste(empty, collapse = ", "), ".")))
    req(input$plan_area_col)

    optional <- function(v) if (is.null(v) || identical(v, "none") || !nzchar(v)) NULL else v
    cols <- list(area = input$plan_area_col, field = optional(input$plan_field_col),
                 x = optional(input$plan_x_col), y = optional(input$plan_y_col))
    num_or <- function(v, default) if (is.null(v) || !is.finite(v)) default else v
    settings <- tryCatch(list(
      coord_unit = input$plan_coord_unit %||% "mm",
      inspected_area_mm2 = parse_inspected_areas(input$plan_inspected_area),
      target_precision_pct = num_or(input$plan_precision, 10),
      target_ratio = num_or(input$plan_ratio, 1.2),
      power = num_or(input$plan_power, 80) / 100,
      fields_per_area = num_or(input$plan_fields_per_area, 1),
      return_period = num_or(input$plan_return_period, 100)
    ), error = function(e) shiny::validate(e$message))

    res <- tryCatch(plan_measurement(runs, cols, settings, all_runs = all_runs),
                    error = function(e) shiny::validate(paste("Planning failed:", e$message)))
    log_operation("INFO", "Measurement plan", sprintf("%d run(s), %d particles, %.2f mm2", length(runs), res$pilot$n, res$pilot$inspected_area_mm2))
    res
  })

  placeholder <- "Upload the pilot run, check the columns, and click \"Make the Plan\"."
  tables <- reactive(planning_display_tables(result()))

  output$plan_status <- renderText({
    render_gated_status(input, "plan_run", placeholder, function() {
      p <- result()$pilot
      target <- result()$precision[result()$precision$chosen, , drop = FALSE]
      sprintf("%d particles on %.2f mm2: %.1f per mm2, known to +/- %.1f %% now. For +/- %g %%: %.3g mm2 (%.1f times the pilot area).",
              p$n, p$inspected_area_mm2, p$per_mm2, 100 * p$rel_halfwidth_now, target$precision_pct[1],
              target$area_mm2[1], target$times_pilot_area[1])
    })
  })
  output$plan_notes <- renderUI({
    res <- tryCatch(result(), error = function(e) NULL)
    if (is.null(res)) return(NULL)
    tagList(
      lapply(res$notes$warnings, function(msg)
        div(style = "color: #856404; background-color: #fff3cd; border: 1px solid #ffeeba; border-radius: 4px; padding: 10px; font-size: 13px; margin: 10px 0;",
            strong("Check: "), msg)),
      tags$ul(style = "font-size: 12px; margin: 6px 0;", lapply(res$notes$notes, tags$li))
    )
  })

  output$plan_pilot_table <- renderTable(tables()$pilot, align = "l")
  output$plan_precision_table <- renderTable(tables()$precision, align = "l")
  output$plan_ratio_table <- renderTable(tables()$ratio, align = "l")
  output$plan_evs_table <- renderTable(tables()$evs, align = "l")
  output$plan_runs_table <- renderTable(tables()$runs, align = "l")
  output$plan_repeat_density_table <- renderTable(tables()$repeat_density, align = "l")
  output$plan_repeat_other_table <- renderTable(tables()$repeat_other, align = "l")
  output$plan_precision_plot <- renderPlot(print(create_precision_curve(result())))

  output$plan_download_tables <- downloadHandler(
    filename = function() paste0("measurement_plan_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"),
    content = function(file) {
      res <- safe_reactive_result(function() result(), "Upload the pilot run and click \"Make the Plan\" before downloading.")
      writexl::write_xlsx(planning_result_tables(res), file)
    }
  )

  register_settings_io(input, output, session, tab = "planning", prefix = "plan")

  return(list(
    module_name = "server_planning"
  ))
}
