# ---- UI: "Analysis Log" tab (split out of ui_components.R) ----
# Filterable/searchable activity log with basic controls.

#' Build the "Analysis Log" tab's UI
#'
#' @param id Module namespace id - must match the id passed to
#'   `moduleServer()` for this tab in `server_logic.R`.
#' @return A `shiny::tabPanel()`.
#' @export
create_analysis_log_tab <- function(id) {
  ns <- NS(id)
  tabPanel("Analysis Log",
    fluidRow(
      column(12,
        h3("Analysis Activity Log"),
        fluidRow(
          column(8,
            h4("Recent Activities"),
            div(style = "border: 1px solid #ddd; padding: 10px; border-radius: 5px; max-height: 400px; overflow-y: auto;",
              verbatimTextOutput(ns("analysis_log"))
            ),

            h4("Log Controls"),
            actionButton(ns("clear_log"), "Clear Log", class = "btn-warning"),
            downloadButton(ns("export_log"), "Export Log", class = "btn-info")
          ),
          column(4,
            h4("Log Statistics"),
            verbatimTextOutput(ns("log_stats")),

            h4("Filter Log"),
            selectInput(ns("log_level"), "Log Level:",
              choices = c("All" = "all", "SUCCESS" = "SUCCESS", "INFO" = "INFO", "WARNING" = "WARNING", "ERROR" = "ERROR"),
              selected = "all"),

            h4("Search Log"),
            textInput(ns("log_search"), "Search in Log:", placeholder = "Enter search term"),
            actionButton(ns("search_log"), "Search", class = "btn-sm btn-outline-primary")
          )
        )
      )
    )
  )
}
