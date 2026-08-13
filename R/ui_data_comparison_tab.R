# ---- UI: "Data Comparison" tab (split out of ui_components.R) ----
# Descriptive statistics, correlation analysis, and multivariate
# (Mahalanobis / Isolation Forest) comparison between the two datasets.

create_data_comparison_tab <- function() {
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
  )
}
