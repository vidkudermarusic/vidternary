# ---- Server: "Hexagonal Ternary Diagram" tab ----
# Single "Create & Save diagram" downloadButton/downloadHandler: one click
# both builds the composite (into its own fresh tempdir, handed to the
# browser's native Save dialog) and updates the on-page preview - a
# genuine two-step workflow (separate "Generate"/"Save" buttons) used to
# exist here, merged into one at the user's request since "Save" already
# did everything "Generate" did (build the composite, update the preview)
# plus the save itself - the two-button version was one redundant extra
# click, not two independently useful actions. See the vidternary
# Structural Audit's Sec.03 for why this uses a downloadButton/downloadHandler
# rather than the old global Output Directory picker in the first place.

#' Wire up the Hexagonal Ternary Diagram tab's server logic
#'
#' Registers the observers/renderers for the "Hexagonal Ternary Diagram"
#' tab: file upload, the 7 element-slot dropdowns, and the single
#' "Create & Save diagram" handler (via `create_hex_ternary_diagram()`).
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
      d <- openxlsx::read.xlsx(input$hex_xlsx_file$datapath, sheet = 1)
      cols <- names(d)
      for (i in seq_len(7)) {
        updateSelectizeInput(session, paste0("hex_element_", i), choices = cols)
      }
      updateSelectizeInput(session, "hex_filter_cols", choices = cols[sapply(d, is.numeric)])
      log_operation("INFO", "Hex ternary file loaded", paste("File:", input$hex_xlsx_file$name, "Columns:", length(cols)))
    }, error = function(e) {
      show_message(paste("Error reading Excel file:", e$message), "error")
      log_operation("ERROR", "Failed to read hex ternary file", e$message)
    })
  })

  # Pre-Analysis Data Filter (helpers_pre_analysis_filter.R, shared with
  # EVS/Spatial Clustering/Point Pattern Analysis) - lets rows be excluded
  # (e.g. "Area > 1") before the composite diagram is built. Unlike those
  # three tabs, this one has no combined_data()/reactive data frame at all:
  # create_hex_ternary_diagram() takes an xlsx file PATH and reads it
  # internally, so filtering here means reading the upload, filtering it in
  # R, and writing a fresh filtered xlsx whose path is handed to that
  # function instead - create_hex_ternary_diagram() itself needed no
  # changes.
  output$hex_filter_inputs <- renderUI({
    render_pre_filter_inputs(session$ns, "hex", input$hex_filter_cols)
  })

  collect_element_strings <- function() {
    els <- lapply(seq_len(7), function(i) input[[paste0("hex_element_", i)]])
    if (any(vapply(els, length, integer(1)) == 0)) return(NULL)
    vapply(els, paste, character(1), collapse = "+")
  }

  # Returns the xlsx file path create_hex_ternary_diagram() should actually
  # read: the original upload unchanged when no filter is selected (no
  # filtered-copy write needed, and matches this tab's own pre-existing
  # "no filter = behaves exactly as before" baseline - create_hex_ternary_
  # diagram() gets the exact same path it always did), or a freshly-written
  # filtered copy otherwise. Either way the file is read once here anyway,
  # to report real row counts back to the caller.
  resolve_hex_input_file <- function() {
    orig_path <- input$hex_xlsx_file$datapath
    filters <- collect_pre_filters(input, "hex", input$hex_filter_cols)
    if (length(filters) == 0) {
      d <- openxlsx::read.xlsx(orig_path, sheet = 1)
      return(list(path = orig_path, n_before = nrow(d), n_after = nrow(d)))
    }
    d <- openxlsx::read.xlsx(orig_path, sheet = 1)
    n_before <- nrow(d)
    d_filtered <- apply_pre_filters(d, filters)
    if (nrow(d_filtered) == 0) {
      stop("No rows remain after applying the pre-analysis filter(s) - loosen or remove them and try again.")
    }
    filtered_path <- tempfile("hex_filtered_", fileext = ".xlsx")
    writexl::write_xlsx(d_filtered, filtered_path)
    list(path = filtered_path, n_before = n_before, n_after = nrow(d_filtered))
  }

  # Hands the composite PNG straight to the browser's own Save dialog
  # (downloadButton/downloadHandler) instead of writing it into a
  # pre-chosen server-side folder - see the vidternary Structural Audit's
  # Sec.03 for why the previous global Output Directory picker was removed.
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

      hex_input <- tryCatch(resolve_hex_input_file(), error = function(e) {
        output$hex_status <- renderText(paste("Error saving diagram:", e$message))
        log_operation("ERROR", "Failed to apply pre-analysis filter for hex ternary diagram", e$message)
        stop(e$message)
      })

      composite <- tryCatch({
        out_dir <- tempfile("hex_save_")
        dir.create(out_dir, recursive = TRUE)

        do.call(create_hex_ternary_diagram,
                c(list(xlsx_file = hex_input$path, output_dir = out_dir, working_dir = NULL),
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
      output$hex_status <- renderText(sprintf(
        "Successfully saved hexagonal ternary diagram: %s (%d of %d rows used after pre-analysis filter)",
        basename(composite), hex_input$n_after, hex_input$n_before))
      log_operation("SUCCESS", "Hex ternary diagram saved", composite)
      file.copy(composite, file, overwrite = TRUE)
    }
  )

  output$hex_plot_container <- renderUI({
    if (is.null(hex_result_path())) {
      tags$p("Upload a file, select all 7 element positions and click 'Create & Save diagram'.")
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
