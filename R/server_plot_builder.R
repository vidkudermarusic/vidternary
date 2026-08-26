# ---- Server: "Plot Builder" tab ----
# Generic X/Y/type/color chart builder plus user-saved presets. rv$plot_presets
# is initialized (from disk, via load_builder_presets()) in server_logic.R
# alongside the rest of the shared reactiveValues, since presets are
# meaningfully app-level state, not tab-local.

BUILDER_TYPES_WITH_Y <- c("violin", "box", "scatter")

#' Wire up the Plot Builder tab's server logic
#'
#' Registers the observers/renderers for the "Plot Builder" tab: file
#' upload/combine, the chart-type-dependent axis selector UI, the live
#' chart render (via `build_custom_plot()`), and the preset save/load/delete
#' handlers (via `load_builder_presets()`/`save_builder_presets()`).
#'
#' @param input The Shiny `input` object.
#' @param output The Shiny `output` object.
#' @param session The Shiny session object.
#' @param rv The app's shared `reactiveValues` object (holds `plot_presets`).
#' @param show_message Function to show a user-facing status message.
#' @param log_operation Function to record a structured log entry.
#' @param directory_management Optional directory-management module (unused
#'   by this tab, accepted for interface consistency with other tabs).
#' @return A list with `module_name`.
#' @export
create_server_plot_builder <- function(input, output, session, rv, show_message, log_operation, directory_management = NULL) {

  # Disambiguated per-file labels (e.g. two uploads both named "sample.xlsx"
  # get "sample" / "sample #2") - shared between the dataset-selector UI and
  # combined_data() so a file's selector label always matches its
  # source_file tag. Mirrors the Data Comparison tab's own file selector
  # (server_data_comparison_upload.R).
  builder_file_names <- reactive({
    req(input$builder_files)
    make.unique(tools::file_path_sans_ext(input$builder_files$name), sep = " #")
  })

  output$builder_dataset_selector_ui <- renderUI({
    req(input$builder_files)
    names_available <- builder_file_names()
    selectizeInput(session$ns("builder_selected_files"), "Datasets to include:",
      choices = names_available, selected = names_available, multiple = TRUE)
  })

  combined_data <- reactive({
    req(input$builder_files, input$builder_selected_files)
    file_names <- builder_file_names()
    keep <- file_names %in% input$builder_selected_files
    shiny::validate(shiny::need(any(keep), "Select at least one dataset above."))

    files_df <- input$builder_files[keep, , drop = FALSE]
    kept_names <- file_names[keep]
    n_files <- nrow(files_df)
    dfs <- lapply(seq_len(n_files), function(i) {
      d <- tryCatch(openxlsx::read.xlsx(files_df$datapath[i], sheet = 1), error = function(e) NULL)
      if (is.null(d)) return(NULL)
      if (n_files > 1) d$source_file <- kept_names[i]
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
    # X axis (group/category) used to only offer categorical_cols(),
    # excluding every numeric column (e.g. an element's Wt%) from ever being
    # usable as the grouping axis - Y axis already offers every numeric
    # column with no such restriction. all_cols gives X axis the same
    # freedom: any column, numeric or categorical, can define the groups.
    # build_custom_plot() wraps whatever's chosen in factor() so a numeric
    # column still produces one discrete violin/box/bar per distinct value
    # instead of being treated as a continuous axis.
    all_cols <- union(cat_cols, num_cols)
    switch(input$builder_type,
      "violin" = tagList(
        selectInput(session$ns("builder_x"), "X axis (group)", choices = all_cols),
        # A plain multi-select selectizeInput starts with no selection by
        # default (unlike single-select, which auto-picks the first choice) -
        # without an explicit default, req(input$builder_y) below would block
        # forever on first load until the user manually multi-selects
        # something, so nothing ever renders out of the box.
        selectizeInput(session$ns("builder_y"), "Y axis (value(s))", choices = num_cols,
          selected = if (length(num_cols) > 0) num_cols[1] else NULL, multiple = TRUE),
        helpText("Select two or more columns (e.g. several elements' Wt%) to compare them side by side within each X group - each selected column becomes its own sub-group, and overrides the Color / group by selection below.")
      ),
      "box" = tagList(
        selectInput(session$ns("builder_x"), "X axis (group)", choices = all_cols),
        # A plain multi-select selectizeInput starts with no selection by
        # default (unlike single-select, which auto-picks the first choice) -
        # without an explicit default, req(input$builder_y) below would block
        # forever on first load until the user manually multi-selects
        # something, so nothing ever renders out of the box.
        selectizeInput(session$ns("builder_y"), "Y axis (value(s))", choices = num_cols,
          selected = if (length(num_cols) > 0) num_cols[1] else NULL, multiple = TRUE),
        helpText("Select two or more columns (e.g. several elements' Wt%) to compare them side by side within each X group - each selected column becomes its own sub-group, and overrides the Color / group by selection below.")
      ),
      "bar" = tagList(
        selectInput(session$ns("builder_x"), "X axis (category)", choices = all_cols),
        uiOutput(session$ns("builder_bar_value_selector")),
        checkboxInput(session$ns("builder_percent"), "Show percentages instead of counts", value = FALSE)
      ),
      "hist" = tagList(
        selectInput(session$ns("builder_x"), "Variable", choices = num_cols),
        numericInput(session$ns("builder_hist_bins"), "Number of bins", value = 30, min = 2, max = 200, step = 1)
      ),
      "scatter" = tagList(
        selectInput(session$ns("builder_x"), "X axis", choices = num_cols),
        selectInput(session$ns("builder_y"), "Y axis", choices = num_cols)
      ),
      "rose" = tagList(
        selectInput(session$ns("builder_x"), "Direction column (degrees)", choices = num_cols),
        numericInput(session$ns("builder_rose_bin_width"), "Bin width (degrees)", value = 10, min = 1, max = 90, step = 1),
        helpText("Values are binned into fixed-width sectors around a full 0-360 circle. Log-scale X/Y below don't apply to a polar axis and are ignored. 'Color / group by' shows one rose diagram per category (side by side) instead of coloring within a single plot. ",
          cite_link("Mardia & Jupp, 2000", "https://doi.org/10.1002/9780470316979"))
      )
    )
  })

  # Which distinct values of the relevant category column to plot as bars -
  # counting every distinct value unconditionally (the previous behavior)
  # wasn't useful for a high-cardinality column, and silently included
  # everything with no way to focus on just the categories that matter.
  # When a Color / group by breakdown is chosen, that column is what the
  # user actually wants to narrow down ("which categories to show, per
  # file") - the X axis is normally the file/group being compared, not the
  # thing being filtered - so choices are sourced from color_by instead of
  # X in that case. Rebuilt whenever the X column or color_by changes;
  # defaults to all values selected so existing behavior is unchanged until
  # the user deliberately narrows it down.
  output$builder_bar_value_selector <- renderUI({
    req(input$builder_type == "bar", input$builder_x)
    d <- combined_data()
    has_color <- !is.null(input$builder_color_by) && input$builder_color_by != "none"
    filter_col <- if (has_color) input$builder_color_by else input$builder_x
    req(filter_col %in% names(d))
    vals <- sort(unique(as.character(d[[filter_col]])))
    label <- if (has_color) paste0("Categories to show (", input$builder_color_by, "):") else "Categories to show:"
    selectizeInput(session$ns("builder_bar_values"), label,
      choices = vals, selected = vals, multiple = TRUE)
  })

  current_plot <- reactive({
    d <- combined_data()
    req(input$builder_type, input$builder_x)
    y_needed <- input$builder_type %in% BUILDER_TYPES_WITH_Y
    if (y_needed) req(input$builder_y)
    if (input$builder_type == "bar") req(input$builder_bar_values)
    if (input$builder_type == "rose") req(input$builder_rose_bin_width)
    if (input$builder_type == "hist") req(input$builder_hist_bins)
    build_custom_plot(
      d, input$builder_type, x = input$builder_x,
      y = if (y_needed) input$builder_y else NULL,
      color_by = if (is.null(input$builder_color_by)) "none" else input$builder_color_by,
      log_x = isTRUE(input$builder_log_x), log_y = isTRUE(input$builder_log_y),
      percent = isTRUE(input$builder_percent),
      bar_values = if (input$builder_type == "bar") input$builder_bar_values else NULL,
      rose_bin_width = if (input$builder_type == "rose") input$builder_rose_bin_width else 10,
      hist_bins = if (input$builder_type == "hist") input$builder_hist_bins else 30
    )
  })

  # geom_point()'s `size` is a fixed physical size (mm), not relative to
  # the plot - matching the preview device's aspect ratio/inches to the
  # download's 10x7in avoids a preview/download point-size mismatch
  # (affects the "scatter" chart type; matching the whole device also
  # keeps line widths/text sizing consistent across all chart types). See
  # server_spatial.R for the full explanation, including why
  # renderPlot()'s width/height must stay close to plotOutput's actual
  # on-screen size (they also set the browser's literal display size, not
  # just the internal device resolution) - height must match
  # ui_plot_builder_tab.R's plotOutput(..., height=) exactly; width is
  # derived from the same 10:7 ratio as the download. (580px - actually
  # slightly BELOW the original 600px: at a 10:7 aspect ratio, the
  # mainPanel(width=8) container only measures ~843px wide at a typical
  # desktop viewport, which caps height at ~590px before overflowing:
  # the original 600px/857px pairing was already marginally overflowing
  # its column before this change, just not enough to have been
  # noticed. 580px is the largest height that fits without overflow.)
  builder_plot_height_px <- 580
  output$builder_plot <- renderPlot({
    tryCatch({
      print(current_plot())
    }, error = function(e) {
      shiny::validate(paste("Error rendering plot:", e$message))
    })
  }, width = round(builder_plot_height_px * 10 / 7), height = builder_plot_height_px, res = builder_plot_height_px / 7)

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
      percent = isTRUE(input$builder_percent),
      bar_values = if (input$builder_type == "bar") input$builder_bar_values else NULL,
      rose_bin_width = if (input$builder_type == "rose") input$builder_rose_bin_width else NULL,
      hist_bins = if (input$builder_type == "hist") input$builder_hist_bins else NULL
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
      # builder_y is selectize-based either way (Shiny's selectInput()
      # defaults to selectize = TRUE) - updateSelectizeInput works whether
      # it's currently the single-select (scatter) or multi-select
      # (violin/box) variant, and correctly restores a multi-value selection.
      if (!is.null(preset$y)) updateSelectizeInput(session, "builder_y", selected = preset$y)
      if (!is.null(preset$percent)) updateCheckboxInput(session, "builder_percent", value = preset$percent)
      if (!is.null(preset$rose_bin_width)) updateNumericInput(session, "builder_rose_bin_width", value = preset$rose_bin_width)
      if (!is.null(preset$hist_bins)) updateNumericInput(session, "builder_hist_bins", value = preset$hist_bins)
      if (!is.null(preset$bar_values)) {
        # builder_bar_value_selector is a nested uiOutput that only
        # (re)renders after input$builder_x's new value has propagated
        # through a further reactive flush, so restoring its selection
        # needs to wait one more flush cycle beyond this one.
        session$onFlushed(function() {
          updateSelectizeInput(session, "builder_bar_values", selected = preset$bar_values)
        }, once = TRUE)
      }
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
