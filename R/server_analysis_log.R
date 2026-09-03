# ---- Server Analysis Log Module ----
# This module contains analysis log functionality including controls and rendering

#' Wire up the "Analysis Log" tab's server logic
#'
#' Registers the "Clear Log" handler, the level/search filtering
#' (`input$log_level`/`input$search_log`, applied via a shared
#' `filtered_log_entries()` helper so the on-screen display and "Export
#' Log" can never disagree about which entries are shown), the on-screen
#' log display (`output$analysis_log`), the filtered entry-count summary
#' (`output$log_stats`), and the "Export Log" download handler. There used
#' to also be a separate "Save Log to File" button that wrote the same
#' entries to a pre-chosen server-side folder - removed as a fully
#' redundant duplicate of "Export Log" (see the vidternary Structural
#' Audit's §03), not converted to anything, once the global directory
#' picker it depended on went away.
#'
#' @param input The Shiny `input` object.
#' @param output The Shiny `output` object.
#' @param session The Shiny session object.
#' @param rv The app's shared `reactiveValues` object (reads
#'   `rv$analysis_log`, the entries every tab's [log_operation()] call
#'   appends to).
#' @param show_message Function to show a user-facing status message.
#' @param log_operation Function to record a structured log entry.
#' @return An empty list - this function's effect is entirely the
#'   observers/outputs it registers.
#' @export
create_server_analysis_log <- function(input, output, session, rv, show_message, log_operation) {

  # ---- Shared filter/search logic ----
  # Used by the on-screen display, "Save Log to File", and "Export Log" so
  # all three can never disagree about which entries are shown.
  filtered_log_entries <- function() {
    entries <- rv$analysis_log
    if (length(entries) == 0) return(list())

    if (!is.null(input$log_level) && input$log_level != "all") {
      entries <- entries[sapply(entries, function(entry) entry$level == input$log_level)]
    }

    if (!is.null(input$log_search) && nzchar(input$log_search)) {
      search_term <- tolower(input$log_search)
      entries <- entries[sapply(entries, function(entry) {
        grepl(search_term, tolower(entry$message)) ||
          (!is.null(entry$details) && grepl(search_term, tolower(entry$details)))
      })]
    }

    entries
  }

  log_entries_to_dataframe <- function(entries) {
    if (length(entries) == 0) {
      return(data.frame(Timestamp = character(0), Level = character(0), Message = character(0), Details = character(0)))
    }
    data.frame(
      Timestamp = sapply(entries, function(e) e$timestamp),
      Level = sapply(entries, function(e) e$level),
      Message = sapply(entries, function(e) e$message),
      Details = sapply(entries, function(e) if (is.null(e$details)) "" else e$details),
      stringsAsFactors = FALSE
    )
  }

  # ---- Analysis Log Controls ----

  # Clear analysis log
  observeEvent(input$clear_log, {
    rv$analysis_log <- list()
    log_operation("INFO", "Analysis log cleared by user")
    show_message("Analysis log cleared.", "info")
  })

  # "Save Log to File" (an actionButton writing to the old global Output
  # Directory) used to live here - removed as a fully redundant duplicate
  # of "Export Log" below, which already does the identical computation
  # and download via a real downloadHandler (see this function's own doc).

  # Search analysis log (filtering itself is already live/reactive via
  # input$log_search - this just records that a search was performed)
  observeEvent(input$search_log, {
    log_operation("INFO", "User searched analysis log", input$log_search)
  })

  # ---- Analysis Log Rendering ----

  # Main analysis log display
  output$analysis_log <- renderText({
    if (length(rv$analysis_log) == 0) {
      "No activities logged yet. Start using the app to see activity history."
    } else {
      entries <- filtered_log_entries()
      if (length(entries) == 0) return("No log entries match the current filter/search criteria.")

      log_text <- ""
      for (entry in entries) {
        log_text <- paste0(log_text,
                           "[", entry$timestamp, "] ", entry$level, ": ", entry$message)
        if (!is.null(entry$details)) {
          log_text <- paste0(log_text, " (", entry$details, ")")
        }
        log_text <- paste0(log_text, "\n")
      }
      log_text
    }
  })

  # Log statistics display. Uses filtered_log_entries() - the same entries
  # "Recent Activities", "Save Log to File", and "Export Log" all agree on -
  # rather than the raw rv$analysis_log, so a user filtering to e.g. ERROR
  # only sees stats for what's actually showing, not the whole unfiltered
  # log.
  output$log_stats <- renderText({
    if (length(rv$analysis_log) == 0) {
      "No log entries"
    } else {
      entries <- filtered_log_entries()
      if (length(entries) == 0) return("No log entries match the current filter/search criteria.")

      total_entries <- length(entries)
      success_count <- sum(sapply(entries, function(entry) entry$level == "SUCCESS"))
      info_count <- sum(sapply(entries, function(entry) entry$level == "INFO"))
      warning_count <- sum(sapply(entries, function(entry) entry$level == "WARNING"))
      error_count <- sum(sapply(entries, function(entry) entry$level == "ERROR"))

      paste("Total Entries:", total_entries,
            "\nSUCCESS:", success_count,
            "\nINFO:", info_count,
            "\nWARNING:", warning_count,
            "\nERROR:", error_count)
    }
  })

  # Export (browser download) of the currently filtered log
  output$export_log <- downloadHandler(
    filename = function() paste0("analysis_log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"),
    content = function(file) {
      writexl::write_xlsx(log_entries_to_dataframe(filtered_log_entries()), file)
    }
  )

  # Return the module functions (if any are needed externally)
  return(list(
    # This module primarily contains observeEvent and output rendering functions
    # No external functions to return at this time
  ))
}
