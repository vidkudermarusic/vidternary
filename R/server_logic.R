# =============================================================================
# vidternary: Shiny Server Logic Module
# =============================================================================
# 
# Package:     vidternary
# Version:     1.0.0
# Author:      Vid Kuder Marušič <vidkm30@gmail.com>
# Maintainer:  Vid Kuder Marušič <vidkm30@gmail.com>
# License:     MIT + file LICENSE
# Repository:  https://github.com/vidkudermarusic/vidternary
# 
# Description: Main server-side logic for the Shiny application including
#              reactive values, event handlers, and core application flow.
# 
# Key Functions:
#   - create_server_logic(): Main server function with reactive logic
#   - [Server-side event handlers and reactive expressions]
# 
# Dependencies:
#   - R (>= 4.0.0)
#   - shiny, shinyFiles, shinyjqui, shinyBS
# 
# Last Modified: 2025-09-07
# 
# =============================================================================

create_server_logic <- function(input, output, session) {
  
  # Default directories
  default_working_dir <- getwd()
  default_output_dir <- file.path(getwd(), "output")
  
  # Initialize cache cleanup on startup
  clear_expired_cache()
  
  # Periodic cache cleanup (every 5 minutes)
  cache_cleanup_timer <- reactiveTimer(300000)  # 5 minutes in milliseconds
  observe({
    cache_cleanup_timer()
    clear_expired_cache()
  })
  
  # Directory management moved to server_directory_management.R module
  
  # Reactive values for data and results
  rv <- reactiveValues(
    working_dir = character(0),
    output_dir = character(0),
    stats1 = NULL,
    stats2 = NULL,
    validation1 = NULL,
    validation2 = NULL,
    correlation1 = NULL,
    correlation2 = NULL,
    df1 = NULL,
    df2 = NULL,
    mahalanobis_result = NULL,
    # Filtered data storage for export functionality
    filtered_data1 = NULL,
    filtered_data2 = NULL,
    last_filter_config1 = NULL,
    last_filter_config2 = NULL,
    # Plot storage for Multiple Plot Types
    scatter_plot = NULL,
    histogram_plot = NULL,
    boxplot_plot = NULL,
    # Export functionality storage
    last_export_results = NULL,
    last_export_folder = NULL,
    export_history = NULL,
    # Analysis log
    analysis_log = list(),
    xlsx_file1 = NULL,  # Added this line
    xlsx_file2 = NULL,  # Added this line
    # Enhanced features storage
    advanced_plot_data = NULL,
    export_files = list(),
    # Group selection management
    group_selections_1 = NULL,  # Persistent selections for dataset 1
    group_selections_2 = NULL,  # Persistent selections for dataset 2
    group_counts_1 = NULL,      # Group counts for dataset 1
    group_counts_2 = NULL,      # Group counts for dataset 2
    is_categorical_group_1 = FALSE,  # Whether dataset 1 optional param 2 is categorical
    is_categorical_group_2 = FALSE   # Whether dataset 2 optional param 2 is categorical
  )
  
  # Add error handling and user feedback
  show_message <- function(message, type = "info") {
    session$sendCustomMessage("showMessage", list(
      message = message,
      type = type
    ))
  }
  
  # Import directory management functions first (needed by other modules)
  directory_management <- create_server_directory_management(input, output, session, default_working_dir, default_output_dir)
  
  # ---- File Upload Handlers ----
  # File handling functionality moved to server_file_handlers.R module
  
  # Import file handler functions
  file_handlers <- create_server_file_handlers(input, output, session, rv, show_message, log_operation)
  
  # Import filter management functions (needed by ternary_plots)
  filter_management <- create_server_filter_management(input, output, session, rv, show_message, log_operation)
  
  # Import ternary plot functions
  ternary_plots <- create_server_ternary_plots(input, output, session, rv, show_message, log_operation, filter_management, directory_management)
  
  # Import export functions
  export_functions <- create_server_export(input, output, session, rv, show_message, log_operation, directory_management)
  
  # Import plot types functions
  plot_types <- create_server_plot_types(input, output, session, rv, show_message, log_operation, directory_management)
  
  # Ternary core functionality now integrated into server_ternary_plots
  
  # Import UI coordination functions
  ui_coordination <- create_server_ui_coordination(input, output, session, rv)
  
  # Import status output functions
  status_outputs <- create_server_status_outputs(input, output, session, rv)
  
  # Import analysis log functions
  analysis_log <- create_server_analysis_log(input, output, session, rv, show_message, log_operation)
  
  # Import help system functions
  help_system <- create_server_help_system(input, output, session)
  
  # Import data comparison functions
  data_comparison <- create_server_data_comparison(input, output, session, rv, show_message, log_operation)
  
  # Import multiple ternary creator functions
  # multiple_ternary <- create_server_multiple_ternary(input, output, session, rv, show_message, log_operation, directory_management)
  # NOTE: Disabled old handler - new handler in server_ternary_plots.R is working correctly
  
  # Advanced plot functionality moved to server_plot_types.R module
  # Analysis log functionality moved to server_analysis_log.R module
  
  # ---- Multiple Ternary Creator Helper Functions ----
  # Filter management functionality moved to server_filter_management.R module
  
  # ---- Dynamic Filter UI Generation ----
  # Dynamic filter UI generation functionality moved to server_filter_management.R module
  
  # Dynamic color inputs moved to server_plot_types.R module
  
  # ---- Ternary Plot Previews ----
  # Ternary plot functionality moved to server_ternary_plots.R module
  
  # Ternary preview 1 moved to server_ternary_plots.R module
  
  # Ternary preview 2 moved to server_ternary_plots.R module
  
  # ---- Helper function to get individual filters ----
  # Helper function functionality moved to server_filter_management.R module
  
    # ---- Plot Save Buttons ----
  # Core ternary plot functionality moved to server_ternary_core.R module
  
  # Multiple plot types functionality moved to server_plot_types.R module
  
  # ---- Data Comparison Tab Functionality ----
  # Data comparison functionality moved to server_data_comparison.R module
  
  # ---- Enhanced Analysis Outputs ----
  # Enhanced analysis outputs functionality moved to server_data_comparison.R module
  
  # ---- Multiple Ternary Creator Functionality ----
  # Multiple ternary creator functionality moved to server_multiple_ternary.R module
  
  # ---- Export Functionality ----
  # Export functionality moved to server_export.R module
  
  # Comprehensive Export All Functionality
  # Comprehensive export functionality moved to server_export.R module
  
  # Export selected items functionality moved to server_export.R module
  
    # ---- Cache Management ----
  # Cache functionality moved to server_cache_management.R module
  
  # Import cache management functions
  cache_management <- create_server_cache_management(input, output, session, rv, show_message, log_operation)  
  # ---- Help Button ----
  # Help system functionality moved to server_help_system.R module
  
  # ---- Status Outputs ----
  # Status output functionality moved to server_status_outputs.R module
  
  # Analysis log rendering functionality moved to server_analysis_log.R module
  
  # ---- Enhanced Multiple Plot Types Functionality ----
  # UI coordination functionality moved to server_ui_coordination.R module
  
  # Dynamic color inputs moved to server_plot_types.R module
  # Plot functionality moved to server_plot_types.R module
  # Export functionality moved to server_export.R module

  # ---- Enhanced Directory Settings ----
  # Directory management functionality moved to server_directory_management.R module
  
  # ---- Multivariate Analysis Functions ----
  # Compute_mahalanobis_distance function is now in multivariate.R to avoid duplication
  # Compute_robust_mahalanobis_distance function is now in multivariate.R to avoid duplication
  # Compute_isolation_forest_outliers function is now in multivariate.R to avoid duplication

  # ---- Complete Filtering Pipeline for Export ----
  # Export functionality moved to server_export.R module
  
  # ---- Return reactive values ----
  return(rv)
}
