# ---- UI: "Ternary Plots" tab (split out of ui_components.R) ----
# The main workflow tab: two-dataset upload, axis/element selection,
# per-element filters, analysis methods (multivariate + statistical),
# output options, and the live preview/status area.

create_ternary_plots_tab <- function() {
  tabPanel("Ternary Plots",
    fluidRow(
      column(12,
        fluidRow(
          column(6, h3("Dataset 1 (Primary)")),
          column(6,
            h3("Dataset 2 (Reference)"),
            div(style = "margin-top: 10px;",
              actionButton("copy_settings", "📋 Copy Settings from Dataset 1",
                          class = "btn-info btn-sm",
                          style = "font-size: 0.9em; padding: 5px 10px;"),
              helpText("Copy all settings from Dataset 1 to Dataset 2")
            )
          )
        ),
        fluidRow(
          column(6,
            fileInput("xlsx_file1", "Choose Primary XLSX File", accept = c(".xlsx"))
          ),
          column(6,
            fileInput("xlsx_file2", "Choose Reference XLSX File", accept = c(".xlsx"))
          )
        )
      )
    ),

    # Plot Previews
    fluidRow(
      column(12,
        hr(),
        h3("Plot Previews"),
        fluidRow(
          column(6,
            textOutput("preview_info1"),
            plotOutput("ternary_preview1", height = "500px")
          ),
          column(6,
            textOutput("preview_info2"),
            plotOutput("ternary_preview2", height = "500px")
          )
        )
      )
    ),

    # Save Plot Buttons
    fluidRow(
      column(12, style = "text-align: center; margin: 20px 0;",
        h4("Save Plots"),
        actionButton("plot1", "Save Plot 1", class = "btn-primary btn-lg", style = "margin: 0 10px;"),
        actionButton("plot2", "Save Plot 2", class = "btn-primary btn-lg", style = "margin: 0 10px;"),
        actionButton("plot_both", "Save Both Plots", class = "btn-success btn-lg", style = "margin: 0 10px;")
      )
    ),

    # Element Selection
    fluidRow(
      column(12,
        fluidRow(
          column(6,
            div(style = "border: 2px solid #dc3545; padding: 10px; border-radius: 5px; margin: 10px 0;",
              h4(style = "color: #dc3545; margin-top: 0;", "Element A (Required)"),
              selectInput("element_A1", "Element A (multiple allowed):", choices = NULL, multiple = TRUE),
              uiOutput("dynamic_filters_A1"),
              helpText("Note: Each selected element can have its own filter condition (logical AND between elements)"),
              helpText("Example: Fe > 10, Al > 5, Si > 0 (each element gets its own threshold)")
            ),
            div(style = "border: 2px solid #dc3545; padding: 10px; border-radius: 5px; margin: 10px 0;",
              h4(style = "color: #dc3545; margin-top: 0;", "Element B (Required)"),
              selectInput("element_B1", "Element B (multiple allowed):", choices = NULL, multiple = TRUE),
              uiOutput("dynamic_filters_B1")
            ),
            div(style = "border: 2px solid #dc3545; padding: 10px; border-radius: 5px; margin: 10px 0;",
              h4(style = "color: #dc3545; margin-top: 0;", "Element C (Required)"),
              selectInput("element_C1", "Element C (multiple allowed):", choices = NULL, multiple = TRUE),
              uiOutput("dynamic_filters_C1")
            ),
            selectInput("optional_param1_1", "Optional Param 1:", choices = c("", NULL)),
            selectInput("optional_param1_representation1", "Optional Param 1 Representation:",
              choices = c("Point Size" = "point_size", "Point Type" = "point_type"),
              selected = "point_size"),
            helpText("Choose how to represent Optional Param 1: Point Size (variable size) or Point Type (different shapes)."),
            textInput("filter_op1_1", "Filter for Optional Param 1", ""),
            helpText("Enter a filter, e.g. > 0.5. Leave blank for no filter."),
            selectInput("optional_param2_1", "Optional Param 2:", choices = c("", NULL)),
            textInput("filter_op2_1", "Filter for Optional Param 2", ""),
            helpText("Enter a filter, e.g. > 0.5. Leave blank for no filter."),

            # Group selection for Dataset 1
            uiOutput("group_selection_ui_1"),

            selectInput("color_palette1", "Color Palette for Optional Param 2:",
              choices = c("Blue" = "blue", "Red" = "red", "Viridis" = "viridis", "Rainbow" = "rainbow"),
              selected = "blue")
          ),
          column(6,
            div(style = "border: 2px solid #dc3545; padding: 10px; border-radius: 5px; margin: 10px 0;",
              h4(style = "color: #dc3545; margin-top: 0;", "Element A (Required)"),
              selectInput("element_A2", "Element A (multiple allowed):", choices = NULL, multiple = TRUE),
              uiOutput("dynamic_filters_A2")
            ),
            div(style = "border: 2px solid #dc3545; padding: 10px; border-radius: 5px; margin: 10px 0;",
              h4(style = "color: #dc3545; margin-top: 0;", "Element B (Required)"),
              selectInput("element_B2", "Element B (multiple allowed):", choices = NULL, multiple = TRUE),
              uiOutput("dynamic_filters_B2")
            ),
            div(style = "border: 2px solid #dc3545; padding: 10px; border-radius: 5px; margin: 10px 0;",
              h4(style = "color: #dc3545; margin-top: 0;", "Element C (Required)"),
              selectInput("element_C2", "Element C (multiple allowed):", choices = NULL, multiple = TRUE),
              uiOutput("dynamic_filters_C2")
            ),
            selectInput("optional_param1_2", "Optional Param 1:", choices = c("", NULL)),
            selectInput("optional_param1_representation2", "Optional Param 1 Representation:",
              choices = c("Point Size" = "point_size", "Point Type" = "point_type"),
              selected = "point_size"),
            helpText("Choose how to represent Optional Param 1: Point Size (variable size) or Point Type (different shapes)."),
            textInput("filter_op1_2", "Filter for Optional Param 1", ""),
            helpText("Enter a filter, e.g. > 0.5. Leave blank for no filter."),
            selectInput("optional_param2_2", "Optional Param 2:", choices = c("", NULL)),
            textInput("filter_op2_2", "Filter for Optional Param 2", ""),
            helpText("Enter a filter, e.g. > 0.5. Leave blank for no filter."),

            # Group selection for Dataset 2
            uiOutput("group_selection_ui_2"),

            selectInput("color_palette2", "Color Palette for Optional Param 2:",
              choices = c("Blue" = "blue", "Red" = "red", "Viridis" = "viridis", "Rainbow" = "rainbow"),
              selected = "blue")
          )
        )
      )
    ),

    # Analysis Methods
    fluidRow(
      column(12,
        hr(),
        h3("Analysis Methods"),
        p(style = "font-size: 12px; color: #555; font-style: italic; margin-top: -8px;",
          "Only one filter (from either box below) is applied per ternary plot. Selecting a new one automatically deselects the previous choice."),
        fluidRow(
          column(4,
            div(style = "border: 2px solid #007bff; padding: 15px; border-radius: 8px; margin: 10px 0; background-color: #f8f9fa;",
              h4(style = "color: #007bff; margin-top: 0;", "🔧 Multivariate Analysis"),

              # Universal column selector for all multivariate methods
              div(style = "margin-bottom: 15px; padding: 10px; background-color: #e3f2fd; border-radius: 5px; border-left: 4px solid #2196f3;",
                h5(style = "color: #1976d2; margin-top: 0; margin-bottom: 10px;", "📋 Universal Column Selector (REQUIRED)"),
                p(style = "font-size: 12px; color: #d32f2f; margin-bottom: 10px; font-weight: bold;",
                  "⚠️ Column selection is MANDATORY for ALL analysis methods. Select at least 2 numeric columns."),
                p(style = "font-size: 11px; color: #1976d2; margin-bottom: 10px;",
                  "🔗 This column selection is used for BOTH multivariate analysis AND statistical filtering"),
                selectizeInput("multivariate_columns", "Columns for analysis:",
                  choices = NULL, multiple = TRUE,
                  options = list(placeholder = "Select at least 2 numeric columns (REQUIRED)"))
              ),

              checkboxInput("use_mahalanobis", "Use Mahalanobis Distance", value = FALSE),
              div(style = "margin-left: 20px; margin-bottom: 10px; padding: 8px; background-color: #f0f8ff; border-radius: 4px; border-left: 3px solid #007bff;",
                p(style = "font-size: 11px; margin: 0; color: #555;",
                  "📐 Measures distance from data center using covariance structure. ",
                  "Formula: MD = √[(x-μ)ᵀΣ⁻¹(x-μ)]. Automatic threshold: ",
                  cite_link("Vode et al., 2022", "https://doi.org/10.3390/ma15020684"), ".")
              ),



              checkboxInput("use_isolation_forest", "Use Isolation Forest", value = FALSE),
              div(style = "margin-left: 20px; margin-bottom: 10px; padding: 8px; background-color: #f0f8ff; border-radius: 4px; border-left: 3px solid #007bff;",
                p(style = "font-size: 11px; margin: 0; color: #555;",
                  "🌲 Machine learning approach using isolation trees. ",
                  "Measures how easily points can be isolated from the rest. ",
                  cite_link("Liu, Ting & Zhou, 2008", "https://doi.org/10.1109/ICDM.2008.17"), ".")
              ),

              # Advanced Mahalanobis parameters
              conditionalPanel(
                condition = "input.use_mahalanobis == true",
                hr(),
                h5("Mahalanobis Parameters"),
                numericInput("lambda", "Lambda (λ) parameter:", value = 1, min = 0, step = 0.1),
                numericInput("omega", "Omega (ω) parameter:", value = 0, min = 0, step = 0.1),
                radioButtons("outlier_mode_mahalanobis", "Outlier handling:",
                  choices = c("Keep only outliers" = TRUE, "Remove outliers" = FALSE),
                  selected = FALSE, inline = TRUE),
                radioButtons("mdthresh_mode", "Threshold mode:",
                  choices = c("Automatic" = "auto", "Manual" = "manual"),
                  selected = "auto", inline = TRUE),
                conditionalPanel(
                  condition = "input.mdthresh_mode == 'auto'",
                  div(style = "margin-top: 10px; padding: 8px; background-color: #e8f5e8; border-radius: 4px; border-left: 3px solid #28a745;",
                    p(style = "font-size: 12px; margin: 0; color: #155724; font-weight: bold;",
                      "📐 Automatic Threshold Formula:"),
                    p(style = "font-size: 11px; margin: 5px 0 0 0; color: #155724; font-family: monospace;",
                      "MDthresh = MDmean + √(100/(100+λ-ω)) × stdMD"),
                    p(style = "font-size: 11px; margin: 3px 0 0 0; color: #155724;",
                      cite_link("Vode et al., 2022", "https://doi.org/10.3390/ma15020684"))
                  )
                ),
                conditionalPanel(
                  condition = "input.mdthresh_mode == 'manual'",
                  numericInput("custom_mdthresh", "Custom threshold:", value = 10, min = 0.1, step = 0.1)
                ),
                radioButtons("mahalanobis_reference", "Reference dataset:",
                  choices = c("Self-reference" = "self", "Dataset 1" = "dataset1", "Dataset 2" = "dataset2"),
                  selected = "self", inline = TRUE),
                p(style = "font-size: 12px; color: #666; font-style: italic;",
                  "Columns selected above will be used for this analysis.")
              ),


              # Advanced Isolation Forest parameters
              conditionalPanel(
                condition = "input.use_isolation_forest == true",
                hr(),
                h5("Isolation Forest Parameters"),
                radioButtons("outlier_mode_isolation", "Outlier handling:",
                  choices = c("Keep only outliers" = TRUE, "Remove outliers" = FALSE),
                  selected = FALSE, inline = TRUE),
                radioButtons("mahalanobis_reference_isolation", "Reference dataset:",
                  choices = c("Self-reference" = "self", "Dataset 1" = "dataset1", "Dataset 2" = "dataset2"),
                  selected = "self", inline = TRUE),
                p(style = "font-size: 12px; color: #666; font-style: italic;",
                  "Columns selected above will be used for this analysis.")
              )
            )
          ),
          column(4,
            div(style = "border: 2px solid #28a745; padding: 15px; border-radius: 8px; margin: 10px 0; background-color: #f8f9fa;",
              h4(style = "color: #28a745; margin-top: 0;", "📊 Statistical Filtering"),

              # Universal column selector reminder for statistical filters
              div(style = "margin-bottom: 15px; padding: 10px; background-color: #fff3cd; border-radius: 5px; border-left: 4px solid #ffc107;",
                h5(style = "color: #856404; margin-top: 0; margin-bottom: 10px;", "📋 Universal Column Selector"),
                p(style = "font-size: 12px; color: #d32f2f; margin-bottom: 10px; font-weight: bold;",
                  "⚠️ IMPORTANT: Statistical filtering uses the SAME column selection as multivariate analysis!"),
                p(style = "font-size: 11px; color: #856404; margin-bottom: 5px;",
                  "• Select columns in the 'Multivariate Analysis' section above"),
                p(style = "font-size: 11px; color: #856404; margin-bottom: 5px;",
                  "• At least 2 numeric columns are required"),
                p(style = "font-size: 11px; color: #856404; margin-bottom: 0;",
                  "• The same columns will be used for ALL filtering methods")
              ),
              checkboxInput("use_iqr_filter", "Use IQR Filtering", value = FALSE),
              div(style = "margin-left: 20px; margin-bottom: 10px; padding: 8px; background-color: #f0f9ff; border-radius: 4px; border-left: 3px solid #28a745;",
                p(style = "font-size: 11px; margin: 0; color: #555;",
                  "📊 Uses Interquartile Range. Outliers: > Q3+1.5×IQR (high values only). ",
                  cite_link("Tukey, 1977"), ".")
              ),

              checkboxInput("use_zscore_filter", "Use Z-Score Filtering", value = FALSE),
              div(style = "margin-left: 20px; margin-bottom: 10px; padding: 8px; background-color: #f0f9ff; border-radius: 4px; border-left: 3px solid #28a745;",
                p(style = "font-size: 11px; margin: 0; color: #555;",
                  "📈 Standardized scores. Outliers: z-score > 3 (3 standard deviations above mean, high values only)")
              ),

              checkboxInput("use_mad_filter", "Use MAD Filtering", value = FALSE),
              div(style = "margin-left: 20px; margin-bottom: 10px; padding: 8px; background-color: #f0f9ff; border-radius: 4px; border-left: 3px solid #28a745;",
                p(style = "font-size: 11px; margin: 0; color: #555;",
                  "📏 Median Absolute Deviation. Outliers: > median+3×MAD (high values only). ",
                  cite_link("Leys et al., 2013", "https://doi.org/10.1016/j.jesp.2013.03.013"), ".")
              ),

              # Advanced IQR parameters
              conditionalPanel(
                condition = "input.use_iqr_filter == true",
                hr(),
                h5("IQR Filter Parameters"),
                radioButtons("outlier_mode_iqr", "Outlier handling:",
                  choices = c("Keep only outliers" = TRUE, "Remove outliers" = FALSE),
                  selected = FALSE, inline = TRUE)
              ),

              # Advanced Z-score parameters
              conditionalPanel(
                condition = "input.use_zscore_filter == true",
                hr(),
                h5("Z-Score Filter Parameters"),
                radioButtons("outlier_mode_zscore", "Outlier handling:",
                  choices = c("Keep only outliers" = TRUE, "Remove outliers" = FALSE),
                  selected = FALSE, inline = TRUE)
              ),

              # Advanced MAD parameters
              conditionalPanel(
                condition = "input.use_mad_filter == true",
                hr(),
                h5("MAD Filter Parameters"),
                radioButtons("outlier_mode_mad", "Outlier handling:",
                  choices = c("Keep only outliers" = TRUE, "Remove outliers" = FALSE),
                  selected = FALSE, inline = TRUE)
              )
            )
          ),
          column(4,
            div(style = "border: 2px solid #ffc107; padding: 15px; border-radius: 8px; margin: 10px 0; background-color: #f8f9fa;",
              h4(style = "color: #ffc107; margin-top: 0;", "🎨 Output Options"),
              selectInput("output_format", "Output Format:",
                choices = c("PNG" = "png", "JPEG" = "jpeg", "PDF" = "pdf", "TIFF" = "tiff"),
                selected = "png"),
              hr(),
              h5("Point Size Control"),
              sliderInput("manual_point_size", "Manual Point Size:",
                min = 0.1, max = 3.0, value = 1.0, step = 0.1,
                ticks = TRUE),
              checkboxInput("use_manual_point_size", "Use Manual Point Size", value = FALSE),
              helpText("Override automatic point sizing with manual control. When enabled, all points will use the same size regardless of Optional Param 1 settings."),
              checkboxInput("include_plot_notes", "Include plot notes", value = TRUE)
            )
          )
        )
      )
    ),

    # Analysis Report Section
    fluidRow(
      column(12,
        conditionalPanel(
          condition = "input.use_mahalanobis == true || input.use_isolation_forest == true || input.use_iqr_filter == true || input.use_zscore_filter == true || input.use_mad_filter == true",
          hr(),
          div(style = "border: 2px solid #6c757d; padding: 15px; border-radius: 8px; margin: 10px 0; background-color: #f8f9fa;",
            h4(style = "color: #6c757d; margin-top: 0;", "📋 Analysis Report"),
            p(style = "font-size: 12px; color: #666; margin-bottom: 15px;",
              "This report will show details about the applied filtering and analysis methods after plot generation."),
            verbatimTextOutput("analysis_report")
          )
        )
      )
    ),

    # Status and Output
    fluidRow(
      column(12,
        verbatimTextOutput("status"),
        uiOutput("analysis_buttons"),
        uiOutput("dynamic_output")
      )
    )
  )
}
