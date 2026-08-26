# ---- UI: "Data Comparison" tab (split out of ui_components.R) ----
# Descriptive statistics, correlation analysis, and multivariate
# (Mahalanobis / Isolation Forest) comparison across a user-chosen set of
# uploaded Excel files - independent of the main "Ternary Plots" tab's
# Dataset 1/2 uploads, and not limited to exactly two files.

create_data_comparison_tab <- function() {
  tabPanel("Data Comparison",
    fluidRow(
      column(12,
        h3("Dataset Comparison"),

        # ---- Data Source ----
        div(style = "border: 1px solid #dee2e6; padding: 15px; border-radius: 5px; margin: 10px 0; background-color: #f8f9fa;",
          h5("📁 Data Source", style = "margin-top: 0; color: #495057;"),
          fileInput("comparison_files", "Select Excel File(s)",
            multiple = TRUE, accept = c(".xlsx", ".xls")),
          helpText("Each file's Sheet 1 is read independently. Select any 2 or more datasets below to compare them - not limited to two files."),
          uiOutput("comparison_dataset_selector_ui")
        ),

        div(style = "border: 1px solid #dee2e6; padding: 15px; border-radius: 5px; margin: 10px 0; background-color: #f8f9fa;",
          h5("📊 Data Readiness Check", style = "margin-top: 0; color: #495057;"),
          verbatimTextOutput("data_readiness_status")
        ),

        # Descriptive Statistics Section
        fluidRow(
          column(6,
            h4("Descriptive Statistics"),
            helpText("Selecting one dataset above shows its statistics; selecting two or more shows a side-by-side comparison."),
            actionButton("compute_stats", "Compute Descriptive Statistics", class = "btn-primary"),
            br(), br(),
            uiOutput("descriptive_stats_cards"),
            br(),
            DT::dataTableOutput("descriptive_stats_output")
          ),
          column(6,
            h4("Correlation Analysis"),
            helpText("Selecting one dataset above shows its correlation heatmap; the comparison table supports any number of datasets, but the heatmap itself needs exactly 2 selected to stay readable."),
            actionButton("compute_correlations", "Compute Correlations", class = "btn-info"),
            br(), br(),
            plotOutput("correlation_heatmap", height = "400px"),
            br(),
            DT::dataTableOutput("correlation_output")
          )
        ),

        # Multivariate Analysis Section
        fluidRow(
          column(12,
            hr(),
            h4("Multivariate Analysis"),
            div(style = "border: 1px solid #007bff; padding: 15px; border-radius: 8px; margin: 10px 0; background-color: #f8f9fa;",
              h5("🔧 Multivariate Analysis Options", style = "margin-top: 0; color: #007bff;"),
              helpText("Target: the dataset being analyzed. Reference: the dataset its distribution is compared against - pick \"Self\" to detect outliers within the target dataset alone, or another dataset to test the target against that dataset's distribution."),
              fluidRow(
                column(4, selectInput("comparison_mv_target", "Target dataset:", choices = NULL)),
                column(4, selectInput("comparison_mv_reference", "Reference:", choices = NULL)),
                column(4, selectizeInput("comparison_mv_columns", "Columns for analysis:", choices = NULL, multiple = TRUE))
              ),
              fluidRow(
                column(6,
                  h6("Mahalanobis Distance"),
                  p(style = "font-size: 11px; margin: 0 0 5px 0;", cite_link("Vode et al., 2022", "https://doi.org/10.3390/ma15020684")),
                  actionButton("mahalanobis_analysis", "Run Mahalanobis", class = "btn-primary btn-sm"),
                  verbatimTextOutput("mahalanobis_output")
                ),
                column(6,
                  h6("Isolation Forest"),
                  p(style = "font-size: 11px; margin: 0 0 5px 0;", cite_link("Liu, Ting & Zhou, 2008", "https://doi.org/10.1109/ICDM.2008.17")),
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

        # Missing/Outlier Summary & Excel Preview Section
        fluidRow(
          column(12,
            hr(),
            h4("🔍 Interactive Analysis Tools"),
            div(style = "border: 1px solid #6f42c1; padding: 15px; border-radius: 8px; margin: 10px 0; background-color: #f8f9fa;",
              h5("📊 Dataset Analysis Options", style = "margin-top: 0; color: #6f42c1;"),
              selectInput("comparison_preview_target", "Dataset:", choices = NULL),
              actionButton("show_missing_selected", "Missing/Outlier Summary"),
              actionButton("show_excel_selected", "Excel File Preview"),
              br(), br(),
              uiOutput("comparison_preview_output")
            )
          )
        )

      )
    )
  )
}
