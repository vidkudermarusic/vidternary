# ---- Server: "Steel Cleanliness" tab ----
# Wires R/cleanliness_pipeline.R's run_cleanliness_analysis() into the app:
# upload, column auto-detection, pre-analysis filter and the Run button.
# The outputs are registered in server_cleanliness_outputs.R.
#
# NOTE: shiny::validate()/need() must stay fully qualified here - see
# server_spatial.R header comment for why (jsonlite also exports
# validate(), and it wins by library() attach order).

# Bulk analysis typed as "O=25, S=10": named numeric vector, or NULL when empty.
parse_measured_ppm <- function(text) {
  if (is.null(text) || is.na(text) || !nzchar(trimws(text))) return(NULL)
  parse_steel_composition(text)
}

#' Wire up the Steel Cleanliness tab's server logic
#'
#' Registers the observers/renderers for the "Steel Cleanliness" tab: file
#' upload, column auto-detection, the standard's default distances, the
#' pre-analysis filter and the analysis itself
#' ([run_cleanliness_analysis()]).
#'
#' @param input The Shiny `input` object.
#' @param output The Shiny `output` object.
#' @param session The Shiny session object.
#' @param rv The app's shared `reactiveValues` object.
#' @param show_message Function to show a user-facing status message.
#' @param log_operation Function to record a structured log entry.
#' @return A list with `module_name`.
#' @export
create_server_cleanliness <- function(input, output, session, rv, show_message, log_operation) {

  uploaded_data <- make_combined_upload_reactive(input, "clean_files", allow_multiple = FALSE)

  observe({
    d <- tryCatch(uploaded_data(), error = function(e) NULL)
    if (is.null(d)) return()
    nm <- names(d)
    numeric_cols <- nm[vapply(d, is.numeric, logical(1))]
    with_none <- function(cols) c("None" = "none", cols)
    updateSelectInput(session, "clean_area_col", choices = numeric_cols,
                      selected = first_match_or_null(numeric_cols, "^area"))
    updateSelectInput(session, "clean_length_col", choices = numeric_cols,
                      selected = first_match_or_null(numeric_cols, "^length|feret.*max|max.*feret"))
    updateSelectInput(session, "clean_breadth_col", choices = numeric_cols,
                      selected = first_match_or_null(numeric_cols, "^breadth|^width|feret.*min|min.*feret"))
    updateSelectInput(session, "clean_direction_col", choices = with_none(numeric_cols),
                      selected = first_match_or_null(numeric_cols, "direction|orientation|angle") %||% "none")
    updateSelectInput(session, "clean_field_col", choices = with_none(nm),
                      selected = first_match_or_null(nm, "field|frame|fov") %||% "none")
    x_col <- first_match_or_null(numeric_cols, "stage.?x|^x")
    y_col <- first_match_or_null(numeric_cols, "stage.?y|^y")
    updateSelectInput(session, "clean_x_col", choices = numeric_cols, selected = x_col)
    updateSelectInput(session, "clean_y_col", choices = numeric_cols, selected = y_col)
    if (!is.null(x_col)) {
      updateSelectInput(session, "clean_coord_unit", selected = if (grepl("um\\)|\u00b5m", x_col)) "um" else "mm")
    }
    updateSelectInput(session, "clean_family_col", choices = nm,
                      selected = first_match_or_null(nm, "class|family|type|rank"))
    updateSelectizeInput(session, "clean_filter_cols", choices = numeric_cols)
  })

  # Inspected area from the range of the chosen coordinates.
  observe({
    d <- tryCatch(uploaded_data(), error = function(e) NULL)
    if (is.null(d) || is.null(input$clean_x_col) || is.null(input$clean_y_col)) return()
    if (!all(c(input$clean_x_col, input$clean_y_col) %in% names(d))) return()
    factor <- if (identical(input$clean_coord_unit, "um")) 1 else 1000
    bbox <- tryCatch(.cleanliness_bbox(d, list(x = input$clean_x_col, y = input$clean_y_col), factor),
                     error = function(e) NULL)
    if (is.null(bbox)) return()
    updateNumericInput(session, "clean_inspected_area",
                       value = signif((bbox[2] - bbox[1]) * (bbox[4] - bbox[3]) / 1e6, 4))
  })

  # Choosing a standard resets its distances and elongation limit.
  observeEvent(input$clean_standard, {
    std <- cleanliness_rating_standard(input$clean_standard)
    updateNumericInput(session, "clean_max_gap", value = std$max_gap)
    updateNumericInput(session, "clean_max_offset", value = std$max_offset)
    updateNumericInput(session, "clean_aspect_limit", value = std$aspect_limit)
    updateNumericInput(session, "clean_field_area", value = std$field_area_mm2)
  }, ignoreInit = TRUE)

  output$clean_filter_inputs <- renderUI({
    render_pre_filter_inputs(session$ns, "clean", input$clean_filter_cols)
  })

  filtered_data <- reactive({
    d <- uploaded_data()
    filters <- collect_pre_filters(input, "clean", input$clean_filter_cols)
    tryCatch(apply_pre_filters(d, filters), error = function(e) {
      shiny::validate(paste("Pre-analysis filter error:", e$message))
    })
  })

  result <- eventReactive(input$clean_run, {
    all_data <- uploaded_data()
    d <- filtered_data()
    shiny::validate(shiny::need(nrow(d) > 0, "No rows remain after applying the pre-analysis filter(s) - loosen or remove them and try again."))
    req(input$clean_area_col)

    optional <- function(v) if (is.null(v) || identical(v, "none") || !nzchar(v)) NULL else v
    cols <- list(area = input$clean_area_col, length = input$clean_length_col, breadth = input$clean_breadth_col,
                 x = input$clean_x_col, y = input$clean_y_col,
                 direction = optional(input$clean_direction_col), field = optional(input$clean_field_col),
                 family = if (identical(input$clean_family_source, "column")) optional(input$clean_family_col) else NULL)
    missing_cols <- names(cols)[c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE) &
                                  !vapply(cols, function(v) !is.null(v) && v %in% names(d), logical(1))]
    shiny::validate(shiny::need(length(missing_cols) == 0,
      paste0("Choose a valid column for: ", paste(missing_cols, collapse = ", "), ".")))

    num_or <- function(v, default) if (is.null(v) || !is.finite(v)) default else v
    settings <- tryCatch(list(
      standard = input$clean_standard %||% "ISO 4967",
      axis_angle = num_or(input$clean_axis_angle, 0),
      max_gap = num_or(input$clean_max_gap, 40),
      max_offset = num_or(input$clean_max_offset, 10),
      aspect_limit = num_or(input$clean_aspect_limit, 3),
      min_width = num_or(input$clean_min_width, 2),
      field_area_mm2 = num_or(input$clean_field_area, 0.5),
      coord_unit = input$clean_coord_unit %||% "mm",
      inspected_area_mm2 = num_or(input$clean_inspected_area, NA_real_),
      min_signal_pct = num_or(input$clean_min_signal, 5),
      size_breaks = parse_size_breaks(input$clean_size_breaks %||% "1, 2, 5, 10, 20"),
      saltykov_classes = num_or(input$clean_saltykov_classes, 10),
      steel_density = num_or(input$clean_steel_density, 7.9),
      measured_ppm = parse_measured_ppm(input$clean_measured)
    ), error = function(e) shiny::validate(e$message))

    res <- tryCatch(run_cleanliness_analysis(d, cols, settings, all_data = all_data),
                    error = function(e) shiny::validate(paste("Cleanliness analysis failed:", e$message)))
    res$n_rows_before_filter <- nrow(all_data)
    # What the report and the script need to repeat this run.
    res$cols <- cols
    res$filters <- collect_pre_filters(input, "clean", input$clean_filter_cols)
    res$file <- list(name = input$clean_files$name[1], datapath = input$clean_files$datapath[1])
    log_operation("INFO", "Steel cleanliness analysis",
                  sprintf("%s, %d particles, %d rated objects, %.2f mm2", res$standard$name,
                          nrow(res$particles), sum(res$objects$rated), res$inspected_area_mm2))
    res
  })

  register_cleanliness_outputs(input, output, result)

  # Save / load of this tab's settings (settings_io.R).
  register_settings_io(input, output, session, tab = "cleanliness", prefix = "clean")

  return(list(
    module_name = "server_cleanliness"
  ))
}
