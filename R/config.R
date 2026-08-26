# ---- Configuration Management ----
# This module handles application configuration, directories, and settings

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

#' Load app configuration from `ternary_config.json`, if present
#'
#' @return The parsed configuration as a list, or `NULL` if the file
#'   doesn't exist or fails to parse.
#' @export
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

#' Save app configuration to `ternary_config.json`
#'
#' @param config Configuration list to write, in the shape of `default_config`.
#' @return `NULL`, invisibly. Called for its file-writing side effect.
#' @export
save_config <- function(config) {
  tryCatch({
    jsonlite::write_json(config, "ternary_config.json", pretty = TRUE, auto_unbox = TRUE)
    cat("Configuration saved to: ternary_config.json\n")
  }, error = function(e) {
    cat("Warning: Could not save configuration:", e$message, "\n")
  })
}

#' Validate a configuration list, resetting invalid fields to defaults
#'
#' Ensures the configured working/output directories exist (creating them
#' if needed) and that `lambda`/`omega`/`contamination` hold sane numeric
#' values, resetting to `default_config`'s values otherwise.
#'
#' @param config Configuration list to validate, in the shape of `default_config`.
#' @return The (possibly corrected) configuration list.
#' @export
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

#' Load, validate, and apply the app configuration
#'
#' Loads `ternary_config.json` (creating it from `default_config` if
#' missing), validates it, and sets the `default_working_dir`/
#' `default_output_dir` globals from it.
#'
#' @return The loaded (and validated) configuration list.
#' @export
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

#' Read one value from the global `app_config`
#'
#' @param section Top-level config section name (e.g. `"plotting"`).
#' @param key Key within `section` to read.
#' @param default Value to return if `app_config`, `section`, or `key`
#'   don't exist.
#' @return The configured value, or `default`.
#' @export
get_config_value <- function(section, key, default = NULL) {
  if (exists("app_config") && !is.null(app_config[[section]][[key]])) {
    return(app_config[[section]][[key]])
  }
  return(default)
}

#' Set one value in the global `app_config`, auto-saving if enabled
#'
#' Creates `app_config`/`section` if they don't yet exist. Writes the
#' updated config to disk via `save_config()` when `ui.auto_save` is true
#' (the default).
#'
#' @param section Top-level config section name (e.g. `"plotting"`).
#' @param key Key within `section` to set.
#' @param value Value to store.
#' @return `NULL`, invisibly.
#' @export
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
