
# =============================================================================
# vidternary: Shiny UI Components Module
# =============================================================================
# 
# Package:     vidternary
# Version:     1.0.0
# Author:      Vid Kuder Marušič <vidkm30@gmail.com>
# Maintainer:  Vid Kuder Marušič <vidkm30@gmail.com>
# License:     MIT + file LICENSE
# Repository:  https://github.com/vidkudermarusic/vidternary
# 
# Description: Shiny UI components and interface elements for the interactive
#              ternary plot application with advanced filtering and analysis
#              capabilities.
# 
# Key Functions:
#   - create_main_ui(): Main application UI layout
#   - [UI component functions for various interface elements]
# 
# Dependencies:
#   - R (>= 4.0.0)
#   - shiny, shinyFiles, shinyjqui, shinyBS, colourpicker, DT
# 
# Last Modified: 2025-09-07
# 
# =============================================================================

# Main UI function
create_main_ui <- function() {
  fluidPage(
    titlePanel("Ternary Plot Generator with Advanced Filtering"),
    
    # Add error handling and user feedback
    tags$head(
      tags$style(HTML("
        .error-message { 
          color: #d32f2f; 
          background-color: #ffebee; 
          padding: 10px; 
          border-radius: 4px; 
          margin: 10px 0; 
          border-left: 4px solid #d32f2f; 
        }
        .success-message { 
          color: #388e3c; 
          background-color: #e8f5e8; 
          padding: 10px; 
          border-radius: 4px; 
          margin: 10px 0; 
          border-left: 4px solid #388e3c; 
        }
        .warning-message { 
          color: #f57c00; 
          background-color: #fff3e0; 
          padding: 10px; 
          border-radius: 4px; 
          margin: 10px 0; 
          border-left: 4px solid #f57c00; 
        }
        .info-box {
          background-color: #e3f2fd;
          border: 1px solid #2196f3;
          border-radius: 4px;
          padding: 15px;
          margin: 10px 0;
        }
      ")),
      tags$script(HTML("
        Shiny.addCustomMessageHandler('showMessage', function(data) {
          var messageDiv = document.createElement('div');
          messageDiv.className = data.type + '-message';
          messageDiv.textContent = data.message;
          
          // Insert at the top of the page
          document.body.insertBefore(messageDiv, document.body.firstChild);
          
          // Remove after 5 seconds
          setTimeout(function() {
            if (messageDiv.parentNode) {
              messageDiv.parentNode.removeChild(messageDiv);
            }
          }, 5000);
        });
      "))
    ),
    
    fluidRow(
      column(12,
        div(
          style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 20px;",
          h2("Custom Ternary Builder v6 - Individual Element Filtering"),
          actionButton("help_button", "?", 
            style = "background-color: #007bff; color: white; border: none; border-radius: 50%; height: 30px; font-weight: bold; font-size: 16px;",
            title = "Help")
        )
      )
    ),
    
    hr(),
    
    # Main Tabset Panel
    tabsetPanel(
      # Tab 1: Ternary Plots
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
                      "Formula: MD = √[(x-μ)ᵀΣ⁻¹(x-μ)]")
                  ),
                  
                  checkboxInput("use_robust_mahalanobis", "Use Robust Mahalanobis", value = FALSE),
                  div(style = "margin-left: 20px; margin-bottom: 10px; padding: 8px; background-color: #f0f8ff; border-radius: 4px; border-left: 3px solid #007bff;",
                    p(style = "font-size: 11px; margin: 0; color: #555;",
                      "🛡️ Robust version using Minimum Covariance Determinant (MCD). ",
                      "Resistant to outliers in covariance estimation.")
                  ),
                  
                  checkboxInput("use_isolation_forest", "Use Isolation Forest", value = FALSE),
                  div(style = "margin-left: 20px; margin-bottom: 10px; padding: 8px; background-color: #f0f8ff; border-radius: 4px; border-left: 3px solid #007bff;",
                    p(style = "font-size: 11px; margin: 0; color: #555;",
                      "🌲 Machine learning approach using isolation trees. ",
                      "Measures how easily points can be isolated from the rest.")
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
                          "MDthresh = MDmean + √(100/(100+λ-ω)) × stdMD")
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
                  
                  # Advanced Robust Mahalanobis parameters
                  conditionalPanel(
                    condition = "input.use_robust_mahalanobis == true",
                    hr(),
                    h5("Robust Mahalanobis Parameters"),
                    radioButtons("outlier_mode_robust", "Outlier handling:",
                      choices = c("Keep only outliers" = TRUE, "Remove outliers" = FALSE),
                      selected = FALSE, inline = TRUE),
                    radioButtons("mahalanobis_reference_robust", "Reference dataset:",
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
                      "📊 Uses Interquartile Range. Outliers: < Q1-1.5×IQR or > Q3+1.5×IQR")
                  ),
                  
                  checkboxInput("use_zscore_filter", "Use Z-Score Filtering", value = FALSE),
                  div(style = "margin-left: 20px; margin-bottom: 10px; padding: 8px; background-color: #f0f9ff; border-radius: 4px; border-left: 3px solid #28a745;",
                    p(style = "font-size: 11px; margin: 0; color: #555;",
                      "📈 Standardized scores. Outliers: |z-score| > 3 (3 standard deviations)")
                  ),
                  
                  checkboxInput("use_mad_filter", "Use MAD Filtering", value = FALSE),
                  div(style = "margin-left: 20px; margin-bottom: 10px; padding: 8px; background-color: #f0f9ff; border-radius: 4px; border-left: 3px solid #28a745;",
                    p(style = "font-size: 11px; margin: 0; color: #555;",
                      "📏 Median Absolute Deviation. Outliers: < median-3×MAD or > median+3×MAD")
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
              condition = "input.use_mahalanobis == true || input.use_robust_mahalanobis == true || input.use_isolation_forest == true || input.use_iqr_filter == true || input.use_zscore_filter == true || input.use_mad_filter == true",
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
      ),
      
      # Tab 2: Data Comparison
      tabPanel("Data Comparison",
        fluidRow(
          column(12,
            h3("Dataset Comparison"),
            div(style = "border: 1px solid #dee2e6; padding: 15px; border-radius: 5px; margin: 10px 0; background-color: #f8f9fa;",
              h5("📊 Data Readiness Check", style = "margin-top: 0; color: #495057;"),
              verbatimTextOutput("data_readiness_status")
            ),
            
            # Descriptive Statistics Section
            fluidRow(
              column(6,
                h4("Descriptive Statistics"),
                actionButton("compute_stats1", "Compute Stats Dataset 1", class = "btn-primary"),
                actionButton("compute_stats2", "Compute Stats Dataset 2", class = "btn-primary"),
                actionButton("compare_stats", "Compare Both Datasets", class = "btn-success"),
                br(), br(),
                verbatimTextOutput("descriptive_stats_output")
              ),
              column(6,
                h4("Correlation Analysis"),
                actionButton("compute_correlations1", "Correlations Dataset 1", class = "btn-info"),
                actionButton("compute_correlations2", "Correlations Dataset 2", class = "btn-info"),
                actionButton("compare_correlations", "Compare Correlations", class = "btn-warning"),
                br(), br(),
                verbatimTextOutput("correlation_output")
              )
            ),
            
            # Multivariate Analysis Section
            fluidRow(
              column(12,
                hr(),
                h4("Multivariate Analysis"),
                div(style = "border: 1px solid #007bff; padding: 15px; border-radius: 8px; margin: 10px 0; background-color: #f8f9fa;",
                  h5("🔧 Multivariate Analysis Options", style = "margin-top: 0; color: #007bff;"),
                  fluidRow(
                    column(4,
                      h6("Mahalanobis Distance"),
                      actionButton("mahalanobis_analysis", "Run Mahalanobis", class = "btn-primary btn-sm"),
                      verbatimTextOutput("mahalanobis_output")
                    ),
                    column(4,
                      h6("Robust Mahalanobis (MCD)"),
                      actionButton("robust_mahalanobis_analysis", "Run Robust MCD", class = "btn-primary btn-sm"),
                      verbatimTextOutput("robust_mahalanobis_output")
                    ),
                    column(4,
                      h6("Isolation Forest"),
                      actionButton("isolation_forest_analysis", "Run Isolation Forest", class = "btn-primary btn-sm"),
                      verbatimTextOutput("isolation_forest_output")
                    )
                  ),
                  
                  # Comprehensive Multivariate Analysis Display
                  fluidRow(
                    column(12,
                      h5("📊 Comprehensive Analysis Results"),
                      verbatimTextOutput("mahalanobis_info")
                    )
                  )
                )
              )
            ),
            
            # Enhanced Analysis Tools Section
            fluidRow(
              column(12,
                hr(),
                h4("🔍 Interactive Analysis Tools"),
                div(style = "border: 1px solid #6f42c1; padding: 15px; border-radius: 8px; margin: 10px 0; background-color: #f8f9fa;",
                  h5("📊 Dataset Analysis Options", style = "margin-top: 0; color: #6f42c1;"),
                  uiOutput("analysis_buttons"),
                  br(),
                  uiOutput("dynamic_output")
                )
              )
            ),
            
          )
        )
      ),
      
      # Tab 3: Multiple Plot Types
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
      ),
      
      # Tab 4: Multiple Ternary Creator
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
      ),
      
      
      # Tab 5: Data Export
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
      ),
      
      # Tab 6: Analysis Log
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
    ),  # Close main tabsetPanel
    
    # Directory Settings Section
    fluidRow(
      column(12,
        h3("Directory Settings"),
        fluidRow(
          column(6,
            h4("Working Directory"),
            shinyDirButton("working_dir", "Choose Working Directory", "Select Working Directory"),
            verbatimTextOutput("working_dir_text")
          ),
          column(6,
            h4("Output Directory"),
            shinyDirButton("output_dir", "Choose Output Directory", "Select Output Directory"),
            verbatimTextOutput("output_dir_text")
          )
        )
      )
    ),
    
    # Cache Management Section
    fluidRow(
      column(12,
        h3("Cache Management"),
        fluidRow(
          column(4,
            actionButton("clear_cache", "Clear All Cache", class = "btn-warning"),
            helpText("Remove all cached data")
          ),
          column(4,
            actionButton("clear_expired_cache", "Clear Expired Cache", class = "btn-info"),
            helpText("Remove only expired cache entries")
          ),
          column(4,
            verbatimTextOutput("cache_stats"),
            helpText("Current cache status"),
            actionButton("refresh_cache_stats", "Refresh Stats", class = "btn-sm btn-info")
          )
        ),
        fluidRow(
          column(12,
            checkboxInput("debug_mode", "Enable Debug Mode", value = FALSE),
            helpText("Enable detailed debug output in console. Controls verbose logging for multivariate analysis, caching, and performance monitoring.")
          )
        )
      )
    ),
    

    
    tags$hr(),
    tags$footer(
      HTML("© 2025 Vid Kuder Marušič — <a href='mailto:vid.kudermarusic@gmail.com'>vid.kudermarusic@gmail.com</a>"),
      align = "center",
      style = "color: #888; background-color: #f9f9f9; padding: 10px 0; font-size: 0.95em;"
    )
  )
}
