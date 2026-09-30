# ---- Server: "Inclusion Presets" tab ----
# Wires ui_inclusion_tab.R / ui_inclusion_presets.R to analyze_inclusion_preset()
# and plot_inclusion_preset(). One uploaded file (Sheet 1) is the dataset.
#
# NOTE: shiny::validate()/need() must stay fully qualified here - see
# server_spatial.R header comment for why (jsonlite also exports validate()).

#' Wire up the "Inclusion Presets" tab's server logic
#'
#' Registers the observers/renderers of the "Inclusion Presets" tab: the
#' single-file upload, element choices from the loaded data, the preset
#' description, the analysis reactive, the diagram, the class/status tables
#' and the two downloads.
#'
#' @param input The Shiny `input` object.
#' @param output The Shiny `output` object.
#' @param session The Shiny session object.
#' @param rv The app's shared `reactiveValues` object (not used; kept so
#'   every tab's server factory has the same signature).
#' @param show_message Function to show a user-facing status message.
#' @param log_operation Function to record a structured log entry.
#' @return A list with `module_name`.
#' @export
create_server_inclusion <- function(input, output, session, rv, show_message, log_operation) {
  library <- load_inclusion_presets()

  upload <- make_combined_upload_reactive(input, "incl_file", allow_multiple = FALSE)
  dataset <- reactive({
    shiny::validate(shiny::need(!is.null(input$incl_file), "Upload an XLSX file to draw an inclusion diagram."))
    upload()
  })
  symbols <- reactive({
    s <- inclusion_detected_elements(dataset())
    shiny::validate(shiny::need(length(s) > 0, "No element wt% columns (e.g. \"Al.(Wt%)\") found in this dataset."))
    s
  })

  output$incl_data_info <- renderUI({
    d <- tryCatch(dataset(), error = function(e) NULL)
    if (is.null(d)) return(helpText("No file loaded yet."))
    s <- inclusion_detected_elements(d)
    div(style = "font-size: 12px;",
      p(strong(format(nrow(d), big.mark = ","), " particles"), ", ", ncol(d), " columns."),
      if (length(s) > 0) p(strong("Element columns found: "), paste(s, collapse = ", "))
      else p(style = "color: #856404;", "No element wt% columns (e.g. \"Al.(Wt%)\") found in this file.")
    )
  })

  # ---- Element choices follow the dataset ----
  observeEvent(symbols(), {
    s <- symbols()
    mat <- default_matrix_element(dataset())
    updateSelectInput(session, "incl_matrix_element", choices = c("None (do not remove)" = "", s), selected = mat)
    updateSelectizeInput(session, "incl_alloy_elements", choices = setdiff(s, mat),
                         selected = intersect(c("Cr", "Ni", "Mo"), setdiff(s, mat)))
    updateSelectizeInput(session, "incl_ignore_elements", choices = s, selected = intersect("C", s))
  })
  observeEvent(input$incl_matrix_element, {
    s <- tryCatch(symbols(), error = function(e) character(0))
    mat <- input$incl_matrix_element %||% ""
    keep <- setdiff(input$incl_alloy_elements %||% character(0), mat)
    if (length(keep) == 0) keep <- intersect(c("Cr", "Ni", "Mo"), setdiff(s, mat))
    updateSelectizeInput(session, "incl_alloy_elements", choices = setdiff(s, mat), selected = keep)
  }, ignoreInit = TRUE)

  # ---- Preset ----
  base_preset <- reactive({
    req(input$incl_preset)
    get_inclusion_preset(input$incl_preset, library)
  })
  # "true"/"false" text read by the compound-options conditionalPanel in the UI
  output$incl_is_compound <- renderText(if (base_preset()$kind == "compound") "true" else "false")
  outputOptions(output, "incl_is_compound", suspendWhenHidden = FALSE)

  observeEvent(base_preset(), {
    updateNumericInput(session, "incl_threshold", value = base_preset()$threshold_pct)
  })

  output$incl_basis_ui <- renderUI({
    p <- base_preset()
    if (p$classification == "rules") {
      helpText("Basis: mass fractions (fixed - the class rules of this preset are written in mass fractions).")
    } else {
      radioButtons(session$ns("incl_basis"), "Basis:", choices = c("Mass fractions" = "mass", "Mole fractions" = "mole"),
                   selected = p$basis, inline = TRUE)
    }
  })
  basis <- reactive({
    p <- base_preset()
    if (p$classification == "rules") p$basis else (input$incl_basis %||% p$basis)
  })

  output$incl_preset_info <- renderUI({
    p <- base_preset()
    src <- lapply(seq_len(nrow(p$sources)), function(i) tags$li(cite_link(p$sources$citation[i], p$sources$url[i])))
    classes <- if (p$classification == "rules") {
      "Classes from the rules shown as dashed lines (conventions, not a standard)."
    } else {
      "Classes by the nearest reference phase; dashed lines are the boundaries (a convention, not a standard)."
    }
    div(style = "font-size: 12px; margin: 6px 0; padding: 8px; background-color: #f0f8ff; border-radius: 4px;",
      p(style = "margin: 0 0 4px 0;", p$description),
      p(style = "margin: 0 0 4px 0;", strong("Used for: "), p$applies_to),
      p(style = "margin: 0 0 4px 0;", strong("Corners: "), paste(p$labels, collapse = " - "), ". ", classes),
      if (length(src) > 0) tagList(strong("Sources:"), tags$ul(style = "margin: 2px 0 0 0; padding-left: 18px;", src))
    )
  })

  # ---- Analysis ----
  analysis_args <- reactive({
    mat <- input$incl_matrix_element %||% ""
    mode <- input$incl_matrix_mode %||% "matrix_and_alloys"
    args <- list(
      matrix_element = if (nzchar(mat)) mat else NULL,
      matrix_mode = mode,
      ignore_elements = input$incl_ignore_elements %||% character(0),
      s_order = inclusion_s_order(input$incl_s_order %||% "ca_mn"),
      ti_as = input$incl_ti_as %||% "TiN",
      basis = basis()
    )
    thr <- input$incl_threshold
    shiny::validate(shiny::need(is.numeric(thr) && is.finite(thr) && thr >= 0 && thr <= 100,
                                "The coverage threshold must be a number between 0 and 100."))
    args$threshold_pct <- thr
    mx <- input$incl_max_matrix; mr <- input$incl_min_residual
    shiny::validate(shiny::need(is.numeric(mx) && is.finite(mx) && mx > 0 && mx <= 100 &&
                                  is.numeric(mr) && is.finite(mr) && mr >= 0 && mr <= 100,
                                "The matrix particle and no-signal limits must be percentages."))
    args$max_matrix_pct <- mx
    args$min_residual_pct <- mr
    if (nzchar(mat) && mode == "matrix_and_alloys") {
      if (identical(input$incl_steel_source, "enter")) {
        args$steel_composition <- tryCatch(parse_steel_composition(input$incl_steel_composition %||% ""),
                                           error = function(e) shiny::validate(conditionMessage(e)))
      } else {
        alloys <- input$incl_alloy_elements %||% character(0)
        shiny::validate(shiny::need(length(alloys) > 0,
                                    "Choose the alloying elements to correct, or remove the matrix element only."))
        args$alloy_elements <- alloys
      }
    }
    args
  })

  result <- reactive({
    args <- analysis_args()
    d <- dataset()
    p <- base_preset()
    tryCatch(do.call(analyze_inclusion_preset, c(list(data = d, preset = p$id), args)),
             error = function(e) {
               log_operation("ERROR", "Inclusion preset analysis failed", conditionMessage(e))
               shiny::validate(conditionMessage(e))
             })
  })

  # ---- Outputs ----
  plot_width <- function() {
    d <- session$clientData[[paste0("output_", session$ns("incl_plot"), "_width")]]
    if (is.null(d) || !is.finite(d) || d <= 0) 700 else d
  }
  draw <- function(r) {
    plot_inclusion_preset(r,
      show_lines = input$incl_show_lines %||% TRUE, show_reference = input$incl_show_reference %||% TRUE,
      legend = input$incl_show_legend %||% TRUE, notes = input$incl_show_notes %||% TRUE,
      cex = input$incl_point_size %||% 0.5)
  }
  output$incl_plot <- renderPlot(draw(result()), width = plot_width, height = 680)

  output$incl_message <- renderUI({
    r <- tryCatch(result(), error = function(e) NULL)
    if (is.null(r)) return(NULL)
    warn <- function(...) div(style = "color: #856404; background-color: #fff3cd; border: 1px solid #ffeeba; border-radius: 4px; padding: 8px; font-size: 12px; margin-bottom: 8px;", ...)
    note <- function(...) div(style = "color: #0c5460; background-color: #d1ecf1; border: 1px solid #bee5eb; border-radius: 4px; padding: 8px; font-size: 12px; margin-bottom: 8px;", ...)
    out <- list()
    if (!any(r$keep)) {
      out <- c(out, list(warn(strong("No particle is plotted. "),
        "Lower the coverage threshold, or check the matrix settings; the Particles table shows why particles were dropped.")))
    }
    if (!is.null(r$matrix$ratios)) {
      rat <- paste(sprintf("%s %.3f", names(r$matrix$ratios), r$matrix$ratios), collapse = ", ")
      cl <- r$matrix$clipped_by_element
      out <- c(out, list(note(
        strong(paste0("Steel ratios to ", r$matrix$settings$matrix_element, ": ")), paste0(rat, ". "),
        "Corrections that would go below zero were set to 0 in ",
        paste(sprintf("%s: %d", names(cl), cl), collapse = ", "), " particles.")))
    }
    if (length(out) == 0) NULL else tagList(out)
  })

  output$incl_summary_table <- renderTable({
    tab <- result()$summary
    if (nrow(tab) == 0) return(data.frame(Class = "none plotted", Particles = 0L, Percent = 0))
    data.frame(Class = tab$class, Particles = tab$n, Percent = tab$percent)
  }, digits = 2)

  output$incl_status_table <- renderTable({
    r <- result()
    tb <- table(factor(ifelse(is.na(r$excluded), "plotted", r$excluded)))
    data.frame(Status = names(tb), Particles = as.integer(tb))
  })

  output$incl_settings <- renderText(paste(result()$settings, collapse = "\n"))

  # ---- Downloads ----
  placeholder <- "Upload a file and choose a preset before downloading."
  output$incl_download_plot <- downloadHandler(
    filename = function() paste0("inclusion_", input$incl_preset, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      r <- safe_reactive_result(function() result(), placeholder)
      grDevices::png(file, width = 2000, height = 2200, res = 220)
      on.exit(grDevices::dev.off(), add = TRUE)
      draw(r)
      log_operation("INFO", "Inclusion diagram saved", r$settings[1])
    }
  )
  output$incl_download_table <- downloadHandler(
    filename = function() paste0("inclusion_", input$incl_preset, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"),
    content = function(file) {
      r <- safe_reactive_result(function() result(), placeholder)
      writexl::write_xlsx(inclusion_result_tables(r, dataset()), file)
      log_operation("INFO", "Inclusion results saved", r$settings[1])
    }
  )

  list(module_name = "server_inclusion")
}
