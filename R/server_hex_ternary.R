# ---- Server: "Hexagonal Ternary Diagram" tab ----
# Two-step workflow: "Generate" renders into tempdir() for an on-page
# preview; "Save" (a downloadButton/downloadHandler) builds the same
# composite into its own fresh tempdir and hands it to the browser's
# native Save dialog - see the vidternary Structural Audit's §03 for why
# this moved off the old global Output Directory picker.

#' Wire up the Hexagonal Ternary Diagram tab's server logic
#'
#' Registers the observers/renderers for the "Hexagonal Ternary Diagram"
#' tab: file upload, the 7 element-slot dropdowns, and the Generate/Save
#' handlers (via `create_hex_ternary_diagram()`).
#'
#' @param input The Shiny `input` object.
#' @param output The Shiny `output` object.
#' @param session The Shiny session object.
#' @param rv The app's shared `reactiveValues` object.
#' @param show_message Function to show a user-facing status message.
#' @param log_operation Function to record a structured log entry.
#' @return A list with `module_name`.
#' @export
create_server_hex_ternary <- function(input, output, session, rv, show_message, log_operation) {

  hex_result_path <- reactiveVal(NULL)

  observeEvent(input$hex_xlsx_file, {
    req(input$hex_xlsx_file)
    tryCatch({
      cols <- names(openxlsx::read.xlsx(input$hex_xlsx_file$datapath, sheet = 1))
      for (i in seq_len(7)) {
        updateSelectizeInput(session, paste0("hex_element_", i), choices = cols)
      }
      log_operation("INFO", "Hex ternary file loaded", paste("File:", input$hex_xlsx_file$name, "Columns:", length(cols)))
    }, error = function(e) {
      show_message(paste("Error reading Excel file:", e$message), "error")
      log_operation("ERROR", "Failed to read hex ternary file", e$message)
    })
  })

  collect_element_strings <- function() {
    els <- lapply(seq_len(7), function(i) input[[paste0("hex_element_", i)]])
    if (any(vapply(els, length, integer(1)) == 0)) return(NULL)
    vapply(els, paste, character(1), collapse = "+")
  }

  observeEvent(input$hex_generate, {
    req(input$hex_xlsx_file)
    el_strings <- collect_element_strings()
    if (is.null(el_strings)) {
      output$hex_status <- renderText("Please select at least one column for all 7 element positions (A-G)")
      return()
    }

    out_dir <- file.path(tempdir(), paste0("hex_preview_", as.integer(Sys.time())))
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

    output$hex_status <- renderText("Creating hexagonal ternary diagram...")

    tryCatch({
      composite <- do.call(create_hex_ternary_diagram,
                            c(list(xlsx_file = input$hex_xlsx_file$datapath, output_dir = out_dir, working_dir = NULL),
                              as.list(el_strings)))
      if (!is.null(composite) && file.exists(composite)) {
        hex_result_path(composite)
        output$hex_status <- renderText("Hexagonal ternary diagram created successfully (preview).")
        log_operation("SUCCESS", "Hex ternary diagram created", composite)
      } else {
        output$hex_status <- renderText("Diagram was not created - check the selected columns.")
      }
    }, error = function(e) {
      output$hex_status <- renderText(paste("Error creating diagram:", e$message))
      log_operation("ERROR", "Failed to create hex ternary diagram", e$message)
    })
  })

  # Hands the composite PNG straight to the browser's own Save dialog
  # (downloadButton/downloadHandler) instead of writing it into a
  # pre-chosen server-side folder - see the vidternary Structural Audit's
  # §03 for why the previous global Output Directory picker was removed.
  # create_hex_ternary_diagram() still needs a real output_dir to work
  # with; a fresh, single-use temp directory supplies that.
  output$hex_save <- downloadHandler(
    filename = function() {
      folder_name <- if (!is.null(input$hex_output_folder) && nchar(trimws(input$hex_output_folder)) > 0) {
        trimws(input$hex_output_folder)
      } else {
        "hex_ternary_diagrams"
      }
      paste0(folder_name, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      if (is.null(input$hex_xlsx_file)) stop("Please upload a file first.")
      el_strings <- collect_element_strings()
      if (is.null(el_strings)) {
        stop("Please select at least one column for all 7 element positions (A-G).")
      }

      composite <- tryCatch({
        out_dir <- tempfile("hex_save_")
        dir.create(out_dir, recursive = TRUE)

        do.call(create_hex_ternary_diagram,
                c(list(xlsx_file = input$hex_xlsx_file$datapath, output_dir = out_dir, working_dir = NULL),
                  as.list(el_strings)))
      }, error = function(e) {
        output$hex_status <- renderText(paste("Error saving diagram:", e$message))
        log_operation("ERROR", "Failed to save hex ternary diagram", e$message)
        stop(e$message)
      })

      if (is.null(composite) || !file.exists(composite)) {
        output$hex_status <- renderText("Diagram was not saved - check the selected columns.")
        stop("Diagram was not saved - check the selected columns.")
      }

      hex_result_path(composite)
      output$hex_status <- renderText(paste("Successfully saved hexagonal ternary diagram:", basename(composite)))
      log_operation("SUCCESS", "Hex ternary diagram saved", composite)
      file.copy(composite, file, overwrite = TRUE)
    }
  )

  output$hex_plot_container <- renderUI({
    if (is.null(hex_result_path())) {
      tags$p("Upload a file, select all 7 element positions and click “Ustvari heksagonalni diagram”.")
    } else {
      # height must track the image's actual rendered size, not a fixed
      # px value: the composite PNG is generated at 1400x1400
      # (hex_ternary_plot.R) and displayed at width="100%", so at typical
      # browser widths it renders well over 700px tall. A fixed-height
      # container doesn't grow to match, so the image overflowed past its
      # box while the page layout still treated the container as only
      # 700px tall, overlapping whatever came after it on the page.
      imageOutput(session$ns("hex_plot"), height = "auto")
    }
  })

  output$hex_plot <- renderImage({
    req(hex_result_path())
    list(src = hex_result_path(), contentType = "image/png", width = "100%")
  }, deleteFile = FALSE)

  return(list(
    module_name = "server_hex_ternary"
  ))
}
