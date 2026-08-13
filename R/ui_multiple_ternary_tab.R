# ---- UI: "Multiple Ternary Creator" tab (split out of ui_components.R) ----
# Batch-generates one ternary plot per uploaded file using shared axis and
# per-element filter settings (statistical filtering intentionally omitted
# here to keep batch processing simple/fast).

create_multiple_ternary_tab <- function() {
  tabPanel("Multiple Ternary Creator",
    fluidRow(
      column(12,
        h3("Create Ternary Plots for Multiple Files"),
        helpText("This tool allows you to create ternary plots for multiple Excel files using the same parameters."),

        # Purpose and limitations note
        div(style = "border: 1px solid #17a2b8; padding: 15px; border-radius: 5px; margin: 10px 0; background-color: #d1ecf1;",
          h5("🎯 Purpose & Limitations", style = "margin-top: 0; color: #0c5460;"),
          p("This tool is designed for batch processing of ternary plots with consistent parameters:", style = "margin: 5px 0; color: #0c5460;"),
          tags$ul(
            tags$li("Focus: Element selection, optional parameters, and individual element filters"),
            tags$li("Statistical filtering: Disabled to maintain simplicity and performance for batch processing"),
            tags$li("For advanced multivariate analysis with statistical filters, use the main 'Ternary Plots' tab"),
            tags$li("Ideal for: Consistent visualization across multiple datasets with the same analysis parameters")
          )
        ),

        # File selection
        fluidRow(
          column(6,
            h4("File Selection"),
            fileInput("multiple_xlsx_files", "Select Multiple Excel Files",
              multiple = TRUE, accept = c(".xlsx", ".xls")),
            helpText("Select multiple Excel files to process. Each file will generate a separate ternary plot.")
          ),
          column(6,
            h4("Output Settings"),
            helpText("Output directory: Use the 'Directory Settings' section at the bottom of the app."),
            textInput("multiple_output_folder", "Folder Name for Plots",
              value = "multiple_ternary_plots", placeholder = "Enter folder name"),
            selectInput("multiple_output_format", "Output Format",
              choices = c("PNG" = "png", "JPEG" = "jpeg", "PDF" = "pdf", "TIFF" = "tiff"),
              selected = "png")
          )
        ),

        # Parameters
        fluidRow(
          column(12,
            h4("Ternary Plot Parameters"),
            div(style = "border: 1px solid #dee2e6; padding: 15px; border-radius: 5px; margin: 10px 0; background-color: #f8f9fa;",
              h5("Elements"),
              fluidRow(
                column(4,
                  selectizeInput("multiple_element_A", "Element A:", choices = NULL, multiple = TRUE),
                  uiOutput("multiple_filters_A")
                ),
                column(4,
                  selectizeInput("multiple_element_B", "Element B:", choices = NULL, multiple = TRUE),
                  uiOutput("multiple_filters_B")
                ),
                column(4,
                  selectizeInput("multiple_element_C", "Element C:", choices = NULL, multiple = TRUE),
                  uiOutput("multiple_filters_C")
                )
              ),

              h5("Optional Parameters"),
              fluidRow(
                column(6,
                  selectizeInput("multiple_optional_param1", "Optional Parameter 1 (Point Size/Type):", choices = NULL, multiple = TRUE),
                  radioButtons("multiple_optional_param1_representation", "Representation:",
                    choices = c("Point Size" = "point_size", "Point Type" = "point_type"),
                    selected = "point_size", inline = TRUE),
                  uiOutput("multiple_optional_param1_filter")
                ),
                column(6,
                  selectizeInput("multiple_optional_param2", "Optional Parameter 2 (Color):", choices = NULL, multiple = TRUE),
                  selectInput("multiple_color_palette", "Color Palette:",
                    choices = c("Blue" = "blue", "Red" = "red", "Viridis" = "viridis", "Rainbow" = "rainbow"),
                    selected = "blue"),
                  uiOutput("multiple_optional_param2_filter")
                )
              )
            )
          )
        ),

        # Workflow explanation
        fluidRow(
          column(12, style = "text-align: center; margin: 15px 0;",
            div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; border-left: 4px solid #007bff;",
              h5("📋 Two-Step Workflow", style = "margin-top: 0; color: #495057;"),
              p("1. Click 'Create All Ternary Plots' to preview your plots without saving", style = "margin: 5px 0; color: #6c757d;"),
              p("2. Click 'Save All Plots to Subfolder' to save all plots to your output directory", style = "margin: 5px 0; color: #6c757d;")
            )
          )
        ),

        # Action buttons
        fluidRow(
          column(12, style = "text-align: center; margin-top: 20px;",
            actionButton("create_multiple_ternary", "Create All Ternary Plots",
              class = "btn-primary btn-lg", style = "font-size: 18px;"),
            br(), br(),
            actionButton("save_multiple_ternary", "Save All Plots to Subfolder",
              class = "btn-success btn-lg", style = "font-size: 18px;")
          )
        ),

        # Progress and output
        fluidRow(
          column(12,
            div(style = "background-color: #e7f3ff; padding: 10px; border-radius: 5px; margin: 10px 0; border-left: 4px solid #2196F3;",
              h6("💡 Preview Mode Information", style = "margin-top: 0; color: #1976D2;"),
              p("• Preview mode creates plots in memory without saving to disk", style = "margin: 2px 0; font-size: 12px; color: #424242;"),
              p("• Use this to test your settings before processing all files", style = "margin: 2px 0; font-size: 12px; color: #424242;"),
              p("• No output directory is needed for preview mode", style = "margin: 2px 0; font-size: 12px; color: #424242;")
            ),
            verbatimTextOutput("multiple_ternary_status"),
            uiOutput("multiple_ternary_output")
          )
        )
      )
    )
  )
}
