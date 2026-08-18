# ---- Server: "Plot Builder" tab ----
# Generic X/Y/type/color chart builder plus user-saved presets. rv$plot_presets
# is initialized (from disk, via load_builder_presets()) in server_logic.R
# alongside the rest of the shared reactiveValues, since presets are
# meaningfully app-level state, not tab-local.

BUILDER_TYPES_WITH_Y <- c("violin", "box", "scatter")

create_server_plot_builder <- function(input, output, session, rv, show_message, log_operation, directory_management = NULL) {

  combined_data <- reactive({
    req(input$builder_files)
    n_files <- nrow(input$builder_files)
    dfs <- lapply(seq_len(n_files), function(i) {
      d <- tryCatch(openxlsx::read.xlsx(input$builder_files$datapath[i], sheet = 1), error = function(e) NULL)
      if (is.null(d)) return(NULL)
      if (n_files > 1) d$source_file <- tools::file_path_sans_ext(input$builder_files$name[i])
      d
    })
    dfs <- Filter(Negate(is.null), dfs)
    # shiny::validate must be qualified - this package's `import(jsonlite)`
    # masks the unqualified name with jsonlite::validate (a JSON validator).
    shiny::validate(shiny::need(length(dfs) > 0, "None of the selected files could be read."))
    if (length(dfs) == 1) return(dfs[[1]])
    common_cols <- Reduce(intersect, lapply(dfs, names))
    shiny::validate(shiny::need(length(common_cols) > 0, "The selected files have no columns in common."))
    do.call(rbind, lapply(dfs, function(d) d[, common_cols, drop = FALSE]))
  })

  numeric_cols <- reactive({
    d <- combined_data()
    names(d)[sapply(d, is.numeric)]
  })
  categorical_cols <- reactive({
    d <- combined_data()
    names(d)[sapply(d, function(col) is.character(col) || is.factor(col))]
  })

  observe({
    updateSelectInput(session, "builder_color_by", choices = c("None" = "none", categorical_cols()))
  })

  output$builder_axis_selectors <- renderUI({
    req(input$builder_type)
    num_cols <- numeric_cols()
    cat_cols <- categorical_cols()
    switch(input$builder_type,
      "violin" = tagList(
        selectInput("builder_x", "X axis (group)", choices = cat_cols),
        selectInput("builder_y", "Y axis (value)", choices = num_cols)
      ),
      "box" = tagList(
        selectInput("builder_x", "X axis (group)", choices = cat_cols),
        selectInput("builder_y", "Y axis (value)", choices = num_cols)
      ),
      "bar" = tagList(
        selectInput("builder_x", "X axis (category)", choices = cat_cols),
        checkboxInput("builder_percent", "Show percentages instead of counts", value = FALSE)
      ),
      "hist" = tagList(
        selectInput("builder_x", "Variable", choices = num_cols)
      ),
      "scatter" = tagList(
        selectInput("builder_x", "X axis", choices = num_cols),
        selectInput("builder_y", "Y axis", choices = num_cols)
      )
    )
  })

  current_plot <- reactive({
    d <- combined_data()
    req(input$builder_type, input$builder_x)
    y_needed <- input$builder_type %in% BUILDER_TYPES_WITH_Y
    if (y_needed) req(input$builder_y)
    build_custom_plot(
      d, input$builder_type, x = input$builder_x,
      y = if (y_needed) input$builder_y else NULL,
      color_by = if (is.null(input$builder_color_by)) "none" else input$builder_color_by,
      log_x = isTRUE(input$builder_log_x), log_y = isTRUE(input$builder_log_y),
      percent = isTRUE(input$builder_percent)
    )
  })

  output$builder_plot <- renderPlot({
    tryCatch({
      print(current_plot())
    }, error = function(e) {
      shiny::validate(paste("Error rendering plot:", e$message))
    })
  })

  output$builder_download <- downloadHandler(
    filename = function() paste0("plot_builder_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      ggplot2::ggsave(file, plot = current_plot(), width = 10, height = 7, dpi = 300)
    }
  )

  # ---- Presets ----

  update_preset_choices <- function(selected = NULL) {
    names_available <- names(rv$plot_presets)
    updateSelectInput(session, "builder_preset_select", choices = names_available,
                       selected = if (!is.null(selected)) selected else if (length(names_available) > 0) names_available[1] else character(0))
  }

  # Reading rv$plot_presets requires a reactive consumer context - isolate()
  # here since this is a one-off initialization call at server start, not
  # inside an observe()/observeEvent() (the other call sites below are
  # already inside observeEvent handlers, which qualify on their own).
  isolate(update_preset_choices())

  observeEvent(input$builder_save_preset, {
    name <- trimws(input$builder_preset_name)
    if (nchar(name) == 0) {
      show_message("Enter a name for the preset before saving.", "warning")
      return()
    }
    y_needed <- input$builder_type %in% BUILDER_TYPES_WITH_Y
    rv$plot_presets[[name]] <- list(
      type = input$builder_type,
      x = input$builder_x,
      y = if (y_needed) input$builder_y else NULL,
      color_by = if (is.null(input$builder_color_by)) "none" else input$builder_color_by,
      log_x = isTRUE(input$builder_log_x),
      log_y = isTRUE(input$builder_log_y),
      percent = isTRUE(input$builder_percent)
    )
    save_builder_presets(rv$plot_presets)
    update_preset_choices(selected = name)
    show_message(paste("Preset saved:", name), "success")
    log_operation("SUCCESS", "Plot builder preset saved", name)
  })

  observeEvent(input$builder_load_preset, {
    req(input$builder_preset_select)
    preset <- rv$plot_presets[[input$builder_preset_select]]
    if (is.null(preset)) {
      show_message("Preset not found.", "error")
      return()
    }
    updateSelectInput(session, "builder_type", selected = preset$type)
    # X/Y selectors are rebuilt by renderUI when builder_type changes; delay
    # setting them until after that UI exists.
    session$onFlushed(function() {
      updateSelectInput(session, "builder_x", selected = preset$x)
      if (!is.null(preset$y)) updateSelectInput(session, "builder_y", selected = preset$y)
      if (!is.null(preset$percent)) updateCheckboxInput(session, "builder_percent", value = preset$percent)
    }, once = TRUE)
    updateSelectInput(session, "builder_color_by", selected = preset$color_by)
    updateCheckboxInput(session, "builder_log_x", value = preset$log_x)
    updateCheckboxInput(session, "builder_log_y", value = preset$log_y)
    log_operation("INFO", "Plot builder preset loaded", input$builder_preset_select)
  })

  observeEvent(input$builder_delete_preset, {
    req(input$builder_preset_select)
    rv$plot_presets[[input$builder_preset_select]] <- NULL
    save_builder_presets(rv$plot_presets)
    update_preset_choices()
    show_message("Preset deleted.", "info")
  })

  return(list(
    module_name = "server_plot_builder"
  ))
}
