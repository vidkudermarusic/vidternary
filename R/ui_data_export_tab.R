# ---- UI: "Data Export" tab (split out of ui_components.R) ----
# Comprehensive analysis export, filtered-data status, download links, and
# export history.

create_data_export_tab <- function() {
  tabPanel("Data Export",
    fluidRow(
      column(12,
        h3("Export Analysis Results"),

        # Export functionality description
        div(style = "border: 1px solid #28a745; padding: 15px; border-radius: 5px; margin: 10px 0; background-color: #d4edda;",
          h5("📤 Export Capabilities", style = "margin-top: 0; color: #155724;"),
          p("This tab provides comprehensive data export functionality:", style = "margin: 5px 0; color: #155724;"),
          tags$ul(
            tags$li("Comprehensive Analysis: Complete analysis results from Data Comparison and Ternary Plots tabs"),
            tags$li("All exports are organized in timestamped folders for easy management")
          )
        ),
        fluidRow(
          column(6,
            h4("Export Options"),

            # Comprehensive Analysis Export
            h5("Comprehensive Analysis Export"),
            checkboxInput("export_comprehensive", "Export Comprehensive Analysis", value = TRUE),
            helpText("Includes all analysis results from Data Comparison and Ternary Plots tabs"),

            actionButton("export_comprehensive_btn", "Export Comprehensive Analysis", class = "btn-success btn-lg")
          ),
          column(6,
            h4("Export Status"),
            verbatimTextOutput("export_status"),

            # Filtered data status
            h5("Filtered Data Status"),
            uiOutput("filtered_data_status"),

            h4("Download Links"),
            uiOutput("download_links"),

            h4("Export History"),
            verbatimTextOutput("export_history")
          )
        )
      )
    )
  )
}
