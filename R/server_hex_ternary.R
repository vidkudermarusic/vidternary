# ---- Server: "Hexagonal Ternary Diagram" tab ----
# Mirrors the two-step preview/save workflow used by the "Multiple Ternary
# Creator" tab (server_ternary_plots_batch.R): "Generate" renders into
# tempdir() for preview, "Save" writes into a timestamped subfolder under
# the app's configured output directory.

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
#' @param directory_management Optional directory-management module, used
#'   to resolve the output directory for saved diagrams.
#' @return A list with `module_name`.
#' @export
create_server_hex_ternary <- function(input, output, session, rv, show_message, log_operation, directory_management = NULL) {

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

  observeEvent(input$hex_save, {
    req(input$hex_xlsx_file)
    el_strings <- collect_element_strings()
    if (is.null(el_strings)) {
      output$hex_status <- renderText("Please select at least one column for all 7 element positions (A-G)")
      return()
    }

    tryCatch({
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      user_output_dir <- if (!is.null(directory_management) && !is.null(directory_management$output_dir)) {
        directory_management$output_dir()
      } else {
        file.path(getwd(), "output")
      }

      folder_name <- if (!is.null(input$hex_output_folder) && nchar(trimws(input$hex_output_folder)) > 0) {
        trimws(input$hex_output_folder)
      } else {
        "hex_ternary_diagrams"
      }

      out_dir <- file.path(user_output_dir, paste0(folder_name, "_", timestamp))
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

      output$hex_status <- renderText(paste("Saving hexagonal ternary diagram to:", out_dir, "..."))

      composite <- do.call(create_hex_ternary_diagram,
                            c(list(xlsx_file = input$hex_xlsx_file$datapath, output_dir = out_dir, working_dir = NULL),
                              as.list(el_strings)))

      if (!is.null(composite) && file.exists(composite)) {
        hex_result_path(composite)
        output$hex_status <- renderText(paste("Successfully saved hexagonal ternary diagram to:", out_dir))
        log_operation("SUCCESS", "Hex ternary diagram saved", out_dir)
      } else {
        output$hex_status <- renderText("Diagram was not saved - check the selected columns.")
      }
    }, error = function(e) {
      output$hex_status <- renderText(paste("Error saving diagram:", e$message))
      log_operation("ERROR", "Failed to save hex ternary diagram", e$message)
    })
  })

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
      # 700px tall - the Directory Settings section right after it in the
      # page ended up visually overlapping the middle of the image.
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
