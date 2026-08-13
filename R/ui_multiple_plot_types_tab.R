# ---- UI: "Multiple Plot Types" tab (split out of ui_components.R) ----
# Six sub-tabs (Scatter, Histograms, Box Plots, Violin Plots, Connected
# Scatter, Stacked Bar Charts), each with per-dataset and multi-file
# comparison modes.

create_multiple_plot_types_tab <- function() {
  tabPanel("Multiple Plot Types",
    fluidRow(
      column(12,
        h3("Advanced Plotting Options"),
        div(style = "border: 1px solid #ffc107; padding: 15px; border-radius: 5px; margin: 10px 0; background-color: #fff3cd;",
          h5("📊 Purpose & Scope", style = "margin-top: 0; color: #856404;"),
          p("This section provides basic data visualization tools for exploration and comparison:", style = "margin: 5px 0; color: #856404;"),
          tags$ul(
            tags$li("Scatter Plots: Visualize relationships between variables"),
            tags$li("Histograms: Explore data distributions"),
            tags$li("Box Plots: Compare distributions across columns"),
            tags$li("Note: For advanced multivariate analysis, use the 'Ternary Plots' tab")
          )
        ),
        tabsetPanel(
          tabPanel("Scatter Plots",
            fluidRow(
              column(4,
                h5("Dataset Selection"),
                radioButtons("scatter_dataset", "Select Dataset:",
                  choices = c("Dataset 1" = "dataset1", "Dataset 2" = "dataset2", "Compare Both" = "both", "Multi-File Comparison" = "multifile"),
                  selected = "dataset1", inline = TRUE),

                # Multi-file comparison options
                conditionalPanel(
                  condition = "input.scatter_dataset == 'multifile'",
                  h6("Multi-File Comparison", style = "color: #007bff; font-weight: bold;"),
                  fileInput("scatter_multifile_files", "Select Multiple Files",
                    multiple = TRUE, accept = c(".xlsx", ".xls", ".csv")),
                  selectizeInput("scatter_multifile_column", "Column to Compare Across Files",
                    choices = NULL, multiple = FALSE),
                  helpText("Compare the same column from multiple files with different colors"),
                  checkboxInput("scatter_multifile_normalize", "Normalize Data", value = FALSE),
                  helpText("Normalize data to 0-1 scale for better comparison")
                ),

                # Regular dataset options
                conditionalPanel(
                  condition = "input.scatter_dataset != 'multifile'",
                  selectizeInput("scatter_columns", "Select Columns for Analysis", choices = NULL, multiple = TRUE),
                  selectizeInput("scatter_x_col", "X-axis Column (optional)", choices = NULL, multiple = FALSE),
                  selectizeInput("scatter_y_col", "Y-axis Column (optional)", choices = NULL, multiple = FALSE),
                  numericInput("scatter_point_size", "Point Size", value = 0.8, min = 0.1, max = 3, step = 0.1),
                  checkboxInput("scatter_add_trendline", "Add Trend Line", value = FALSE),
                  checkboxInput("scatter_add_smooth", "Add Smooth Curve", value = FALSE),
                  checkboxInput("scatter_log_x", "Use Log X-axis", value = FALSE),
                  checkboxInput("scatter_log_y", "Use Log Y-axis", value = FALSE)
                )
              ),
              column(4,
                h5("Column Colors"),
                uiOutput("scatter_color_inputs"),
                helpText("Colors are automatically assigned using rainbow palette"),
                br(),
                h5("Save Options"),
                selectInput("scatter_output_format", "Output Format",
                  choices = c("PNG" = "png", "JPEG" = "jpeg", "PDF" = "pdf", "TIFF" = "tiff"),
                  selected = "png"),
                textInput("scatter_filename", "Filename (without extension)",
                  value = "scatter_plot", placeholder = "Enter filename"),
                actionButton("save_scatter", "Save Scatter Plot", class = "btn-success")
              ),
              column(4,
                actionButton("create_scatter", "Create Scatter Plot", class = "btn-primary"),
                plotOutput("scatter_plot_output", height = "300px"),
                verbatimTextOutput("scatter_filename_suggestion")
              )
            )
          ),
          tabPanel("Histograms",
            fluidRow(
              column(4,
                h5("Dataset Selection"),
                radioButtons("histogram_dataset", "Select Dataset:",
                  choices = c("Dataset 1" = "dataset1", "Dataset 2" = "dataset2", "Compare Both" = "both", "Multi-File Comparison" = "multifile"),
                  selected = "dataset1", inline = TRUE),

                # Multi-file comparison options
                conditionalPanel(
                  condition = "input.histogram_dataset == 'multifile'",
                  h6("Multi-File Comparison", style = "color: #007bff; font-weight: bold;"),
                  fileInput("histogram_multifile_files", "Select Multiple Files",
                    multiple = TRUE, accept = c(".xlsx", ".xls", ".csv")),
                  selectizeInput("histogram_multifile_column", "Column to Compare Across Files",
                    choices = NULL, multiple = FALSE),
                  helpText("Compare the same column from multiple files with different colors"),
                  checkboxInput("histogram_multifile_normalize", "Normalize Data", value = FALSE),
                  helpText("Normalize data to 0-1 scale for better comparison")
                ),

                # Regular dataset options
                conditionalPanel(
                  condition = "input.histogram_dataset != 'multifile'",
                  selectizeInput("histogram_columns", "Select Columns for Analysis", choices = NULL, multiple = TRUE),
                  numericInput("histogram_bins", "Number of Bins", value = 30, min = 5, max = 100, step = 1),
                  checkboxInput("histogram_density", "Show Density Curve", value = TRUE),
                  checkboxInput("histogram_fill", "Fill Histogram", value = TRUE),
                  checkboxInput("histogram_faceted", "Create Separate Panels", value = FALSE),
                  checkboxInput("histogram_overlay", "Overlay Multiple Columns", value = FALSE),
                  checkboxInput("histogram_statistical", "Show Statistical Info", value = FALSE),
                  checkboxInput("histogram_log_x", "Use Log X-axis", value = FALSE),
                  checkboxInput("histogram_log_y", "Use Log Y-axis", value = FALSE)
                )
              ),
              column(4,
                h5("Styling Options"),
                uiOutput("histogram_color_inputs"),
                helpText("Colors are automatically assigned using rainbow palette"),
                numericInput("histogram_alpha", "Transparency", value = 0.7, min = 0.1, max = 1, step = 0.1),
                selectInput("histogram_position", "Position",
                  choices = c("Stack" = "stack", "Dodge" = "dodge", "Identity" = "identity"),
                  selected = "stack"),
                br(),
                h5("Save Options"),
                selectInput("histogram_output_format", "Output Format",
                  choices = c("PNG" = "png", "JPEG" = "jpeg", "PDF" = "pdf", "TIFF" = "tiff"),
                  selected = "png"),
                textInput("histogram_filename", "Filename (without extension)",
                  value = "histogram", placeholder = "Enter filename"),
                actionButton("save_histogram", "Save Histogram", class = "btn-success")
              ),
              column(4,
                actionButton("create_histogram", "Create Histogram", class = "btn-primary"),
                plotOutput("histogram_plot_output", height = "300px"),
                verbatimTextOutput("histogram_filename_suggestion")
              )
            )
          ),
          tabPanel("Box Plots",
            fluidRow(
              column(4,
                h5("Dataset Selection"),
                radioButtons("boxplot_dataset", "Select Dataset:",
                  choices = c("Dataset 1" = "dataset1", "Dataset 2" = "dataset2", "Compare Both" = "both", "Multi-File Comparison" = "multifile"),
                  selected = "dataset1", inline = TRUE),

                # Multi-file comparison options
                conditionalPanel(
                  condition = "input.boxplot_dataset == 'multifile'",
                  h6("Multi-File Comparison", style = "color: #007bff; font-weight: bold;"),
                  fileInput("boxplot_multifile_files", "Select Multiple Files",
                    multiple = TRUE, accept = c(".xlsx", ".xls", ".csv")),
                  selectizeInput("boxplot_multifile_column", "Column to Compare Across Files",
                    choices = NULL, multiple = FALSE),
                  helpText("Compare the same column from multiple files with different colors"),
                  checkboxInput("boxplot_multifile_normalize", "Normalize Data", value = FALSE),
                  helpText("Normalize data to 0-1 scale for better comparison")
                ),

                # Regular dataset options
                conditionalPanel(
                  condition = "input.boxplot_dataset != 'multifile'",
                  selectizeInput("boxplot_columns", "Select Columns for Analysis", choices = NULL, multiple = TRUE),
                  checkboxInput("boxplot_outliers", "Show Outliers", value = TRUE),
                  checkboxInput("boxplot_notch", "Show Notch", value = FALSE),
                  checkboxInput("boxplot_fill", "Fill Boxes", value = TRUE),
                  checkboxInput("boxplot_horizontal", "Horizontal Orientation", value = FALSE),
                  checkboxInput("boxplot_violin", "Show Violin Plot Overlay", value = FALSE),
                  checkboxInput("boxplot_log_x", "Use Log X-axis", value = FALSE),
                  checkboxInput("boxplot_log_y", "Use Log Y-axis", value = FALSE)
                )
              ),
              column(4,
                h5("Styling Options"),
                uiOutput("boxplot_color_inputs"),
                helpText("Colors are automatically assigned using rainbow palette"),
                numericInput("boxplot_alpha", "Transparency", value = 0.7, min = 0.1, max = 1, step = 0.1),
                br(),
                h5("Save Options"),
                selectInput("boxplot_output_format", "Output Format",
                  choices = c("PNG" = "png", "JPEG" = "jpeg", "PDF" = "pdf", "TIFF" = "tiff"),
                  selected = "png"),
                textInput("boxplot_filename", "Filename (without extension)",
                  value = "boxplot", placeholder = "Enter filename"),
                actionButton("save_boxplot", "Save Box Plot", class = "btn-success")
              ),
              column(4,
                actionButton("create_boxplot", "Create Box Plot", class = "btn-primary"),
                plotOutput("boxplot_plot_output", height = "300px"),
                verbatimTextOutput("boxplot_filename_suggestion")
              )
            )
          ),
          tabPanel("Violin Plots",
            fluidRow(
              column(4,
                h5("Dataset Selection"),
                radioButtons("violin_dataset", "Select Dataset:",
                  choices = c("Dataset 1" = "dataset1", "Dataset 2" = "dataset2", "Compare Both" = "both", "Multi-File Comparison" = "multifile"),
                  selected = "dataset1", inline = TRUE),

                # Multi-file comparison options
                conditionalPanel(
                  condition = "input.violin_dataset == 'multifile'",
                  h6("Multi-File Comparison", style = "color: #007bff; font-weight: bold;"),
                  fileInput("violin_multifile_files", "Select Multiple Files",
                    multiple = TRUE, accept = c(".xlsx", ".xls", ".csv")),
                  selectizeInput("violin_multifile_column", "Column to Compare Across Files",
                    choices = NULL, multiple = FALSE),
                  helpText("Compare the same column from multiple files with different colors"),
                  checkboxInput("violin_multifile_normalize", "Normalize Data", value = FALSE),
                  helpText("Normalize data to 0-1 scale for better comparison")
                ),

                # Regular dataset options
                conditionalPanel(
                  condition = "input.violin_dataset != 'multifile'",
                  selectizeInput("violin_columns", "Select Columns for Analysis", choices = NULL, multiple = TRUE),
                  selectizeInput("violin_group_column", "Grouping Column (optional)", choices = NULL, multiple = FALSE),
                  checkboxInput("violin_fill", "Fill Violins", value = TRUE),
                  checkboxInput("violin_boxplot", "Add Box Plot Overlay", value = TRUE),
                  checkboxInput("violin_points", "Show Individual Points", value = FALSE),
                  checkboxInput("violin_log_x", "Use Log X-axis", value = FALSE),
                  checkboxInput("violin_log_y", "Use Log Y-axis", value = FALSE)
                )
              ),
              column(4,
                h5("Styling Options"),
                uiOutput("violin_color_inputs"),
                helpText("Colors are automatically assigned using rainbow palette"),
                numericInput("violin_alpha", "Transparency", value = 0.7, min = 0.1, max = 1, step = 0.1),
                numericInput("violin_scale", "Scale Method", value = 1, min = 0.1, max = 2, step = 0.1),
                helpText("Scale: 1=area, 2=width"),
                br(),
                h5("Save Options"),
                selectInput("violin_output_format", "Output Format",
                  choices = c("PNG" = "png", "JPEG" = "jpeg", "PDF" = "pdf", "TIFF" = "tiff"),
                  selected = "png"),
                textInput("violin_filename", "Filename (without extension)",
                  value = "violin_plot", placeholder = "Enter filename"),
                actionButton("save_violin", "Save Violin Plot", class = "btn-success")
              ),
              column(4,
                actionButton("create_violin", "Create Violin Plot", class = "btn-primary"),
                plotOutput("violin_plot_output", height = "300px"),
                verbatimTextOutput("violin_filename_suggestion")
              )
            )
          ),
          tabPanel("Connected Scatter",
            fluidRow(
              column(4,
                h5("Dataset Selection"),
                radioButtons("connected_dataset", "Select Dataset:",
                  choices = c("Dataset 1" = "dataset1", "Dataset 2" = "dataset2", "Compare Both" = "both", "Multi-File Comparison" = "multifile"),
                  selected = "dataset1", inline = TRUE),

                # Multi-file comparison options
                conditionalPanel(
                  condition = "input.connected_dataset == 'multifile'",
                  h6("Multi-File Comparison", style = "color: #007bff; font-weight: bold;"),
                  fileInput("connected_multifile_files", "Select Multiple Files",
                    multiple = TRUE, accept = c(".xlsx", ".xls", ".csv")),
                  selectizeInput("connected_multifile_column", "Column to Compare Across Files",
                    choices = NULL, multiple = FALSE),
                  helpText("Compare the same column from multiple files with different colors"),
                  checkboxInput("connected_multifile_normalize", "Normalize Data", value = FALSE),
                  helpText("Normalize data to 0-1 scale for better comparison")
                ),

                # Regular dataset options
                conditionalPanel(
                  condition = "input.connected_dataset != 'multifile'",
                  selectizeInput("connected_columns", "Select Columns for Analysis", choices = NULL, multiple = TRUE),
                  selectizeInput("connected_x_column", "X-axis Column", choices = NULL, multiple = FALSE),
                  selectizeInput("connected_group_column", "Grouping Column (optional)", choices = NULL, multiple = FALSE),
                  checkboxInput("connected_points", "Show Points", value = TRUE),
                  checkboxInput("connected_lines", "Show Lines", value = TRUE),
                  checkboxInput("connected_smooth", "Smooth Lines", value = FALSE),
                  checkboxInput("connected_log_x", "Use Log X-axis", value = FALSE),
                  checkboxInput("connected_log_y", "Use Log Y-axis", value = FALSE)
                )
              ),
              column(4,
                h5("Styling Options"),
                uiOutput("connected_color_inputs"),
                helpText("Colors are automatically assigned using rainbow palette"),
                numericInput("connected_line_width", "Line Width", value = 1, min = 0.1, max = 3, step = 0.1),
                numericInput("connected_point_size", "Point Size", value = 2, min = 0.5, max = 5, step = 0.1),
                br(),
                h5("Save Options"),
                selectInput("connected_output_format", "Output Format",
                  choices = c("PNG" = "png", "JPEG" = "jpeg", "PDF" = "pdf", "TIFF" = "tiff"),
                  selected = "png"),
                textInput("connected_filename", "Filename (without extension)",
                  value = "connected_scatter", placeholder = "Enter filename"),
                actionButton("save_connected", "Save Connected Scatter", class = "btn-success")
              ),
              column(4,
                actionButton("create_connected", "Create Connected Scatter", class = "btn-primary"),
                plotOutput("connected_plot_output", height = "300px"),
                verbatimTextOutput("connected_filename_suggestion")
              )
            )
          ),
          tabPanel("Stacked Bar Charts",
            fluidRow(
              column(4,
                h5("Dataset Selection"),
                radioButtons("stacked_dataset", "Select Dataset:",
                  choices = c("Dataset 1" = "dataset1", "Dataset 2" = "dataset2", "Compare Both" = "both", "Multi-File Comparison" = "multifile"),
                  selected = "dataset1", inline = TRUE),

                # Multi-file comparison options
                conditionalPanel(
                  condition = "input.stacked_dataset == 'multifile'",
                  h6("Multi-File Comparison", style = "color: #007bff; font-weight: bold;"),
                  fileInput("stacked_multifile_files", "Select Multiple Files",
                    multiple = TRUE, accept = c(".xlsx", ".xls", ".csv")),
                  selectizeInput("stacked_multifile_column", "Column to Compare Across Files",
                    choices = NULL, multiple = FALSE),
                  helpText("Compare the same column from multiple files with different colors"),
                  checkboxInput("stacked_multifile_normalize", "Normalize Data", value = FALSE),
                  helpText("Normalize data to 0-1 scale for better comparison")
                ),

                # Regular dataset options
                conditionalPanel(
                  condition = "input.stacked_dataset != 'multifile'",
                  selectizeInput("stacked_columns", "Select Columns for Stacking", choices = NULL, multiple = TRUE),
                  selectizeInput("stacked_x_column", "X-axis Column (Categories)", choices = NULL, multiple = FALSE),
                  checkboxInput("stacked_percentage", "Show as Percentage", value = FALSE),
                  checkboxInput("stacked_horizontal", "Horizontal Orientation", value = FALSE),
                  checkboxInput("stacked_labels", "Show Value Labels", value = FALSE),
                  checkboxInput("stacked_log_x", "Use Log X-axis", value = FALSE),
                  checkboxInput("stacked_log_y", "Use Log Y-axis", value = FALSE)
                )
              ),
              column(4,
                h5("Styling Options"),
                uiOutput("stacked_color_inputs"),
                helpText("Colors are automatically assigned using rainbow palette"),
                numericInput("stacked_alpha", "Transparency", value = 0.8, min = 0.1, max = 1, step = 0.1),
                br(),
                h5("Save Options"),
                selectInput("stacked_output_format", "Output Format",
                  choices = c("PNG" = "png", "JPEG" = "jpeg", "PDF" = "pdf", "TIFF" = "tiff"),
                  selected = "png"),
                textInput("stacked_filename", "Filename (without extension)",
                  value = "stacked_bar", placeholder = "Enter filename"),
                actionButton("save_stacked", "Save Stacked Bar Chart", class = "btn-success")
              ),
              column(4,
                actionButton("create_stacked", "Create Stacked Bar Chart", class = "btn-primary"),
                plotOutput("stacked_plot_output", height = "300px"),
                verbatimTextOutput("stacked_filename_suggestion")
              )
            )
          )
        )
      )
    )
  )
}
