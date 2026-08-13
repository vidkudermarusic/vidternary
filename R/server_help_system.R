# ---- Server Help System Module ----
# This module contains help and documentation functionality

create_server_help_system <- function(input, output, session) {
  
  # ---- Help System Functions ----
  
  # Help button functionality
  observeEvent(input$help_button, {
    showModal(modalDialog(
      title = "Mahalanobis Distance Filtering - Help Guide",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      tagList(
        h4("🔍 What is Mahalanobis Distance?"),
        p("Mahalanobis distance is a multivariate statistical measure that identifies outliers by measuring how far each data point is from the center of the data distribution, taking into account the correlations between variables."),
        
        h4("📊 Three Analysis Methods:"),
        tags$ul(
          tags$li(strong("Standard Mahalanobis:"), "Classical approach assuming normal distribution"),
          tags$li(strong("Isolation Forest:"), "Machine learning approach for anomaly detection")
        ),
        
        h4("⚙️ Threshold Calculation:"),
        tags$ul(
          tags$li(strong("Automatic Mode:"), "Uses λ (lambda) and ω (omega) parameters"),
          tags$li(strong("Manual Mode:"), "Direct input of threshold value"),
          tags$li(strong("Formula:"), "MDthresh = MDmean + √(100/(100+λ-ω)) × stdMD")
        ),
        
        h4("🎯 Parameter Guidelines:"),
        tags$ul(
          tags$li(strong("λ (lambda):"), "Controls strictness - Higher = stricter threshold"),
          tags$li(strong("ω (omega):"), "Provides flexibility - Higher = more lenient threshold"),
          tags$li(strong("Typical ranges:"), "λ = 0-10, ω = 0-5"),
          tags$li(strong("Default values:"), "λ=1, ω=0 (balanced threshold)")
        ),
        
        h4("📈 Threshold Comparison:"),
        tags$ul(
          tags$li(strong("Custom threshold:"), "User-defined or calculated from λ,ω formula")
        ),
        
        h4("💡 Best Practices:"),
        tags$ul(
          tags$li("Use at least 2 numeric columns for analysis"),
          tags$li("Ensure reference dataset has sufficient observations (n > p)"),
          tags$li("Check data quality before analysis"),
          tags$li("Compare results across different methods"),
          tags$li("Start with default parameters and adjust based on results")
        ),
        
        h4("⚠️ Common Issues:"),
        tags$ul(
          tags$li("Singular covariance matrix: Remove highly correlated columns"),
          tags$li("Insufficient observations: Use fewer columns or more data"),
          tags$li("Zero variance columns: These cannot be used in analysis"),
          tags$li("Missing packages: Install 'robustbase' and 'isotree' if needed")
        ),
        
        h4("🔧 Troubleshooting:"),
        tags$ul(
          tags$li("If no outliers detected: Lower threshold or increase λ"),
          tags$li("If too many outliers: Raise threshold or decrease λ"),
          tags$li("For non-normal data: Use robust methods (MCD or Isolation Forest)"),
          tags$li("For high-dimensional data: Consider Isolation Forest")
        ),
        
        hr(),
        
        h4("📋 General Usage Guide:"),
        tags$ol(
          tags$li("Select your working and output directories"),
          tags$li("Upload your Excel files (Dataset 1 and Dataset 2)"),
          tags$li("Choose elements A, B, and C for each dataset"),
          tags$li("Configure optional parameters and filters"),
          tags$li("Set up multivariate analysis if needed"),
          tags$li("Click 'Generate Plot 1', 'Generate Plot 2', or 'Generate Both Plots'")
        ),
        
        h4("🎨 Advanced Features:"),
        tags$ul(
          tags$li("Individual element filtering with custom conditions"),
          tags$li("Multiple multivariate analysis methods"),
          tags$li("Customizable plot options and legends"),
          tags$li("Progress tracking and error handling"),
          tags$li("Data caching for better performance"),
          tags$li("Advanced plotting with multiple plot types"),
          tags$li("Comprehensive analysis logging"),
          tags$li("Interactive analysis tools and comparison features")
        )
      )
    ))
  })
  
  # Return the module functions (if any are needed externally)
  return(list(
    # This module primarily contains the help button observeEvent function
    # No external functions to return at this time
  ))
}
