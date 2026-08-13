# ---- UI: "Analysis Log" tab (split out of ui_components.R) ----
# Filterable/searchable activity log with basic controls.

create_analysis_log_tab <- function() {
  tabPanel("Analysis Log",
    fluidRow(
      column(12,
        h3("Analysis Activity Log"),
        fluidRow(
          column(8,
            h4("Recent Activities"),
            div(style = "border: 1px solid #ddd; padding: 10px; border-radius: 5px; max-height: 400px; overflow-y: auto;",
              verbatimTextOutput("analysis_log")
            ),

            h4("Log Controls"),
            actionButton("clear_log", "Clear Log", class = "btn-warning"),
            actionButton("export_log", "Export Log", class = "btn-info"),
            actionButton("save_log", "Save Log to File", class = "btn-success")
          ),
          column(4,
            h4("Log Statistics"),
            verbatimTextOutput("log_stats"),

            h4("Filter Log"),
            selectInput("log_level", "Log Level:",
              choices = c("All" = "all", "INFO" = "INFO", "WARNING" = "WARNING", "ERROR" = "ERROR"),
              selected = "all"),

            h4("Search Log"),
            textInput("log_search", "Search in Log:", placeholder = "Enter search term"),
            actionButton("search_log", "Search", class = "btn-sm btn-outline-primary")
          )
        )
      )
    )
  )
}
