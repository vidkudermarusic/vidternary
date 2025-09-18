# =============================================================================
# vidternary: Configuration Management Module
# =============================================================================
# 
# Package:     vidternary
# Version:     1.0.0
# Author:      Vid Kuder Marušič <vidkm30@gmail.com>
# Maintainer:  Vid Kuder Marušič <vidkm30@gmail.com>
# License:     MIT + file LICENSE
# Repository:  https://github.com/vidkudermarusic/vidternary
# 
# Description: Configuration management for application settings, directories,
#              and user preferences with cross-platform compatibility.
# 
# Key Functions:
#   - load_config(): Load configuration from file
#   - save_config(): Save configuration to file
#   - get_config_value(): Retrieve configuration values
#   - set_config_value(): Set configuration values
#   - validate_and_fix_config(): Validate and fix configuration
# 
# Dependencies:
#   - R (>= 4.0.0)
#   - fs, jsonlite
# 
# Last Modified: 2025-09-07
# 
# =============================================================================

# Default configuration
# Uses fs::path_home() instead of hard-coded paths for cross-platform compatibility
default_config <- list(
  directories = list(
    working_dir = fs::path_home("mag_naloga_R"),
    output_dir = file.path(fs::path_home("mag_naloga_R"), "output")
  ),
  plotting = list(
    default_color_palette = "viridis",
    default_point_size = 2,
    default_alpha = 0.7,
    max_samples_preview = 20000
  ),
  analysis = list(
    default_lambda = 1,
    default_omega = 0,
    default_contamination = 0.1,
    iqr_multiplier = 1.5,
    zscore_threshold = 3,
    mad_threshold = 3
  ),
  ui = list(
    theme = "default",
    language = "en",
    auto_save = TRUE
  )
)

# Load configuration from file if it exists
load_config <- function() {
  config_file <- "ternary_config.json"
  if (file.exists(config_file)) {
    tryCatch({
      config <- jsonlite::fromJSON(config_file)
      cat("Configuration loaded from:", config_file, "\n")
      return(config)
    }, error = function(e) {
      cat("Warning: Could not load configuration file:", e$message, "\n")
      cat("Using default configuration.\n")
      return(NULL)
    })
  }
  return(NULL)
}

# Save configuration to file
save_config <- function(config) {
  tryCatch({
    jsonlite::write_json(config, "ternary_config.json", pretty = TRUE, auto_unbox = TRUE)
    cat("Configuration saved to: ternary_config.json\n")
  }, error = function(e) {
    cat("Warning: Could not save configuration:", e$message, "\n")
  })
}

# Validate and fix configuration
validate_and_fix_config <- function(config) {
  # Check if directories exist and are writable
  if (!dir.exists(config$directories$working_dir)) {
    cat("Warning: Working directory does not exist, creating:", config$directories$working_dir, "\n")
    tryCatch({
      dir.create(config$directories$working_dir, recursive = TRUE)
    }, error = function(e) {
      cat("Error creating working directory:", e$message, "\n")
      config$directories$working_dir <- getwd()
    })
  }
  
  if (!dir.exists(config$directories$output_dir)) {
    cat("Warning: Output directory does not exist, creating:", config$directories$output_dir, "\n")
    tryCatch({
      dir.create(config$directories$output_dir, recursive = TRUE)
    }, error = function(e) {
      cat("Error creating output directory:", e$message, "\n")
      config$directories$output_dir <- file.path(getwd(), "output")
    })
  }
  
  # Validate numeric parameters
  if (!is.numeric(config$analysis$default_lambda) || config$analysis$default_lambda < 0) {
    cat("Warning: Invalid lambda value, resetting to default\n")
    config$analysis$default_lambda <- 1
  }
  
  if (!is.numeric(config$analysis$default_omega) || config$analysis$default_omega < 0) {
    cat("Warning: Invalid omega value, resetting to default\n")
    config$analysis$default_omega <- 0
  }
  
  if (!is.numeric(config$analysis$default_contamination) || 
      config$analysis$default_contamination < 0 || config$analysis$default_contamination > 1) {
    cat("Warning: Invalid contamination value, resetting to default\n")
    config$analysis$default_contamination <- 0.1
  }
  
  return(config)
}

# Initialize configuration
initialize_config <- function() {
  # Load or create configuration
  app_config <- load_config()
  if (is.null(app_config)) {
    app_config <- default_config
    save_config(app_config)
  }
  
  # Apply validation
  app_config <- validate_and_fix_config(app_config)
  
  # Set global variables
  default_working_dir <<- app_config$directories$working_dir
  default_output_dir <<- app_config$directories$output_dir
  
  return(app_config)
}

# Get configuration value
get_config_value <- function(section, key, default = NULL) {
  if (exists("app_config") && !is.null(app_config[[section]][[key]])) {
    return(app_config[[section]][[key]])
  }
  return(default)
}

# Set configuration value
set_config_value <- function(section, key, value) {
  if (!exists("app_config")) {
    app_config <<- default_config
  }
  
  if (is.null(app_config[[section]])) {
    app_config[[section]] <<- list()
  }
  
  app_config[[section]][[key]] <<- value
  
  # Auto-save if enabled
  if (get_config_value("ui", "auto_save", TRUE)) {
    save_config(app_config)
  }
}

# Note: Functions are exported via NAMESPACE file
