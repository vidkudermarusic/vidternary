# =============================================================================
# LEGACY FILE: Test6_clean.r
# =============================================================================
# 
# Description: Legacy test and development file for ternary plot analysis
# Status: DEPRECATED - Use the modular R package structure instead
# 
# This file contains test code and development scripts that have been
# integrated into the modular vidternary package structure.
# 
# Dependencies: See package requirements below
# 
# =============================================================================

# ---- Libraries ----
required_packages <- c(
  "openxlsx","Ternary","PlotTools","shiny","shinyFiles","shinyjqui","shinyBS",
  "ggplot2","GGally","rmarkdown","corrplot","knitr","colourpicker","DT",
  "robustbase","isotree","plotly","writexl","jsonlite","zip","fs","htmlwidgets",
  "moments","digest","viridisLite","devtools"
)
# ---- Default Directories ----
default_working_dir <- getwd()
default_output_dir <- file.path(getwd(), "output")

# ---- Helper Functions ----
# ---- Constants ----
# Point size settings
MIN_POINT_SIZE <- 0.1
MAX_POINT_SIZE <- 2.5

# ---- Comprehensive Caching System for Performance Optimization ----
# This system provides:
# 1. Data caching: Caches Excel file data based on file modification time and size
# 2. Plot result caching: Caches generated plot data to avoid regeneration
# 3. Automatic expiration: Cache entries expire after 5 minutes
# 4. Cache statistics: Monitor cache usage and performance
# 5. Debug logging: Track cache hits/misses when debug mode is enabled
#
ternary_cache <- new.env()
cache_timeout <- 300  # 5 minutes in seconds

# ---- Debug Mode Control ----
# Set this to TRUE to enable debug output
# Can be toggled via UI checkbox or set programmatically
options(ternary.debug = FALSE)

# Helper function for debug logging
# Usage: debug_log("Processing %d items", length(items))
# All verbose output is now gated behind this flag for production cleanliness
debug_log <- function(message, ...) {
  if (getOption("ternary.debug", FALSE)) {
    cat(sprintf(message, ...), "\n")
  }
}

# Function to generate cache key
# Enhanced to include plot styling parameters to prevent cache mismatches
# when users change visual appearance settings
generate_cache_key <- function(data_hash, filters, elements, plot_styling = NULL) {
  key_data <- list(
    data_hash = data_hash,
    filters = filters,
    elements = elements
  )
  
  # Include plot styling parameters if provided
  # This ensures cached plots match user's visual preferences
  if (!is.null(plot_styling)) {
    key_data$plot_styling <- plot_styling
  }
  
  digest::digest(key_data)
}

# Function to get cached result
get_cached_result <- function(key) {
  if (exists(key, envir = ternary_cache)) {
    cached_item <- ternary_cache[[key]]
    # Check if cache is still valid
    if (difftime(Sys.time(), cached_item$timestamp, units = "secs") < cache_timeout) {
      return(cached_item$result)
    } else {
      # Remove expired cache
      rm(list = key, envir = ternary_cache)
    }
  }
  return(NULL)
}

# Function to cache result
cache_result <- function(key, result) {
  ternary_cache[[key]] <- list(
    result = result,
    timestamp = Sys.time()
  )
  
  # Log cache operation for debugging
  debug_log("DEBUG: Cached result for key: %s...", substr(key, 1, 8))
}

# Function to cache plot results specifically
# Enhanced to include plot styling parameters in cache key
# This prevents cache mismatches when users change visual appearance
cache_plot_result <- function(plot_key, plot_data, plot_type = "ternary", plot_styling = NULL) {
  # Generate a comprehensive cache key for plots including styling
  plot_cache_key <- digest::digest(list(
    plot_key = plot_key,
    plot_type = plot_type,
    plot_styling = plot_styling
  ))
  
  # Cache the plot result
  cache_result(plot_cache_key, list(
    plot_data = plot_data,
    plot_type = plot_type,
    plot_styling = plot_styling,
    timestamp = Sys.time()
  ))
  
  debug_log("DEBUG: Cached plot result for: %s - %s...", plot_type, substr(plot_key, 1, 8))
}

# Function to clear expired cache
clear_expired_cache <- function() {
  current_time <- Sys.time()
  keys_to_remove <- character(0)
  
  for (key in ls(ternary_cache)) {
    if (difftime(current_time, ternary_cache[[key]]$timestamp, units = "secs") >= cache_timeout) {
      keys_to_remove <- c(keys_to_remove, key)
    }
  }
  
  if (length(keys_to_remove) > 0) {
    rm(list = keys_to_remove, envir = ternary_cache)
  }
}

# Function to clear all cache
clear_all_cache <- function() {
  rm(list = ls(ternary_cache), envir = ternary_cache)
}

# Function to get cache statistics (unified)
get_cache_stats <- function() {
  cache_size <- length(ls(ternary_cache))
  if (cache_size == 0) {
    return("Cache is empty")
  }
  
  current_time <- Sys.time()
  expired_count <- 0
  total_size <- 0
  oldest_entry <- NULL
  newest_entry <- NULL
  
  for (key in ls(ternary_cache)) {
    cached_item <- ternary_cache[[key]]
    timestamp <- cached_item$timestamp
    
    # Track age
    if (is.null(oldest_entry) || timestamp < oldest_entry) {
      oldest_entry <- timestamp
    }
    if (is.null(newest_entry) || timestamp > newest_entry) {
      newest_entry <- timestamp
    }
    
    # Check expiration
    if (difftime(current_time, timestamp, units = "secs") >= cache_timeout) {
      expired_count <- expired_count + 1
    }
    
    total_size <- total_size + object.size(cached_item)
  }
  
  # Calculate performance metrics
  active_cache_size <- cache_size - expired_count
  cache_efficiency <- if (cache_size > 0) round((active_cache_size / cache_size) * 100, 1) else 0
  
  # Format output
  result <- paste0(
    "Cache Performance Metrics:\n",
    "  Total entries: ", cache_size, "\n",
    "  Active entries: ", active_cache_size, "\n",
    "  Expired entries: ", expired_count, "\n",
    "  Cache efficiency: ", cache_efficiency, "%\n",
    "  Memory usage: ", format(total_size, units = "auto"), "\n",
    "  Oldest entry: ", if (!is.null(oldest_entry)) format(oldest_entry, "%H:%M:%S") else "N/A", "\n",
    "  Newest entry: ", if (!is.null(newest_entry)) format(newest_entry, "%H:%M:%S") else "N/A"
  )
  
  return(result)
}

# Function to check if plot result is cached
is_plot_cached <- function(plot_key, plot_type = "ternary", plot_styling = NULL) {
  # Generate the same cache key that would be used for caching
  plot_cache_key <- digest::digest(list(
    plot_key = plot_key,
    plot_type = plot_type,
    plot_styling = plot_styling
  ))
  
  # Check if we have a cached result
  cached_result <- get_cached_result(plot_cache_key)
  return(!is.null(cached_result))
}

# Function to get cached plot result
get_cached_plot <- function(plot_key, plot_type = "ternary", plot_styling = NULL) {
  # Generate the same cache key that would be used for caching
  plot_cache_key <- digest::digest(list(
    plot_key = plot_key,
    plot_type = plot_type,
    plot_styling = plot_styling
  ))
  
  # Return cached result if available
  return(get_cached_result(plot_cache_key))
}

# Helper function to create plot styling parameters for cache keys
# Use this to create consistent styling parameter lists for cache keys
# Ensures that changes in visual appearance create new cache entries
create_plot_styling_cache_key <- function(color_palette, point_size, point_type, alpha = NULL, 
                                         optional_param1_representation = NULL, output_format = NULL) {
  # Create a list of styling parameters that affect plot appearance
  styling_params <- list(
    color_palette = color_palette,
    point_size = point_size,
    point_type = point_type
  )
  
  # Add optional parameters if they exist
  if (!is.null(alpha)) styling_params$alpha <- alpha
  if (!is.null(optional_param1_representation)) styling_params$optional_param1_representation <- optional_param1_representation
  if (!is.null(output_format)) styling_params$output_format <- output_format
  
  return(styling_params)
}

# Function removed - consolidated into get_cache_stats()

# ---- Basic Helper Functions ----
# Function to clean column names (remove dots and replace with spaces)
clean_column_names <- function(col_names) {
  # Remove .(Wt%) suffix first
  cleaned <- gsub("\\.\\(Wt%\\)", "", col_names)
  # Replace dots with spaces
  cleaned <- gsub("\\.", " ", cleaned)
  # Replace underscores with spaces
  cleaned <- gsub("_", " ", cleaned)
  # Capitalize first letter of each word
  cleaned <- gsub("\\b([a-z])", "\\U\\1", cleaned, perl = TRUE)
  return(cleaned)
}

# Function to safely handle column names with special characters
safe_column_names <- function(col_names) {
  # Handle various formats of weight percentage columns
  cleaned <- gsub("\\.\\(Wt%\\)", "", col_names)
  cleaned <- gsub("\\.\\(Wt\\.%\\)", "", cleaned)
  cleaned <- gsub("\\.\\(Wt\\. %\\)", "", cleaned)
  cleaned <- gsub("\\.\\(Wt\\.%\\)", "", cleaned)
  cleaned <- gsub("\\.\\(Wt\\. %\\)", "", cleaned)
  
  # Handle other common special character patterns
  cleaned <- gsub("\\.\\(%\\)", "", cleaned)
  cleaned <- gsub("\\.\\(wt%\\)", "", cleaned)
  cleaned <- gsub("\\.\\(wt\\.%\\)", "", cleaned)
  
  # Clean up any remaining special characters that might cause issues
  cleaned <- gsub("[^A-Za-z0-9._]", "_", cleaned)
  
  return(cleaned)
}

# Function to get clean display names for columns
get_display_names <- function(col_names) {
  # For display purposes, keep the original names but clean them nicely
  display_names <- clean_column_names(col_names)
  return(display_names)
}

# Function to generate plot summary text
generate_plot_summary <- function(element_A, element_B, element_C, optional_param1, optional_param2, 
                                use_mahalanobis = FALSE, use_robust_mahalanobis = FALSE, use_isolation_forest = FALSE,
                                use_iqr_filter = FALSE, use_zscore_filter = FALSE, use_mad_filter = FALSE,
                                lambda = 1, omega = 0, custom_mdthresh = NULL, 
                                keep_outliers_mahalanobis = FALSE, keep_outliers_robust = FALSE, 
                                keep_outliers_isolation = FALSE, keep_outliers_iqr = FALSE, 
                                keep_outliers_zscore = FALSE, keep_outliers_mad = FALSE,
                                individual_filters_A = NULL, individual_filters_B = NULL, individual_filters_C = NULL) {
  summary_lines <- c()
  
  # Add elements and their filters
  summary_lines <- c(summary_lines, "Elements and Filters:")
  
  # Element A with detailed filter information
    summary_lines <- c(summary_lines, paste("  A:", paste(element_A$col, collapse = "+")))
  if (!is.null(individual_filters_A) && length(individual_filters_A) > 0) {
    active_filters_A <- individual_filters_A[!sapply(individual_filters_A, is.null) & nzchar(as.character(individual_filters_A))]
    if (length(active_filters_A) > 0) {
      filter_details <- paste(sapply(names(active_filters_A), function(name) {
        paste(name, ":", active_filters_A[[name]])
      }), collapse = ", ")
      summary_lines <- c(summary_lines, paste("    Filters:", filter_details))
    }
  }
  
  # Element B with detailed filter information
    summary_lines <- c(summary_lines, paste("  B:", paste(element_B$col, collapse = "+")))
  if (!is.null(individual_filters_B) && length(individual_filters_B) > 0) {
    active_filters_B <- individual_filters_B[!sapply(individual_filters_B, is.null) & nzchar(as.character(individual_filters_B))]
    if (length(active_filters_B) > 0) {
      filter_details <- paste(sapply(names(active_filters_B), function(name) {
        paste(name, ":", active_filters_B[[name]])
      }), collapse = ", ")
      summary_lines <- c(summary_lines, paste("    Filters:", filter_details))
    }
  }
  
  # Element C with detailed filter information
    summary_lines <- c(summary_lines, paste("  C:", paste(element_C$col, collapse = "+")))
  if (!is.null(individual_filters_C) && length(individual_filters_C) > 0) {
    active_filters_C <- individual_filters_C[!sapply(individual_filters_C, is.null) & nzchar(as.character(individual_filters_C))]
    if (length(active_filters_C) > 0) {
      filter_details <- paste(sapply(names(active_filters_C), function(name) {
        paste(name, ":", active_filters_C[[name]])
      }), collapse = ", ")
      summary_lines <- c(summary_lines, paste("    Filters:", filter_details))
    }
  }
  
  # Add optional parameters with detailed filter information
  if (!is.null(optional_param1)) {
    summary_lines <- c(summary_lines, "")
    summary_lines <- c(summary_lines, "Optional Parameter 1 (Point Size):")
    summary_lines <- c(summary_lines, paste("  Column:", paste(optional_param1$col, collapse = "+")))
    if (!is.null(optional_param1$filter) && nchar(optional_param1$filter) > 0) {
      summary_lines <- c(summary_lines, paste("  Filter:", optional_param1$filter))
    }
    if (!is.null(optional_param1$representation)) {
      summary_lines <- c(summary_lines, paste("  Representation:", optional_param1$representation))
    }
  }
  
  if (!is.null(optional_param2)) {
    summary_lines <- c(summary_lines, "")
    summary_lines <- c(summary_lines, "Optional Parameter 2 (Color):")
    summary_lines <- c(summary_lines, paste("  Column:", paste(optional_param2$col, collapse = "+")))
    if (!is.null(optional_param2$filter) && nchar(optional_param2$filter) > 0) {
      summary_lines <- c(summary_lines, paste("  Filter:", optional_param2$filter))
    }
  }
  
  # Add statistical and multivariate analysis information
  if (use_mahalanobis || use_robust_mahalanobis || use_isolation_forest || use_iqr_filter || use_zscore_filter || use_mad_filter) {
    summary_lines <- c(summary_lines, "")
    summary_lines <- c(summary_lines, "Statistical and Multivariate Analysis:")
    
    if (use_mahalanobis) {
      outlier_text <- if (keep_outliers_mahalanobis) "(outliers only)" else "(filtered)"
      if (!is.null(custom_mdthresh)) {
        summary_lines <- c(summary_lines, paste("  Mahalanobis Distance:", outlier_text, "| Threshold:", custom_mdthresh))
      } else {
        summary_lines <- c(summary_lines, paste("  Mahalanobis Distance:", outlier_text, "| λ:", lambda, "| ω:", omega))
      }
    }
    
    if (use_robust_mahalanobis) {
      outlier_text <- if (keep_outliers_robust) "(outliers only)" else "(filtered)"
      summary_lines <- c(summary_lines, paste("  Robust Mahalanobis (MCD):", outlier_text, "| λ:", lambda, "| ω:", omega))
    }
    
    if (use_isolation_forest) {
      outlier_text <- if (keep_outliers_isolation) "(outliers only)" else "(filtered)"
      summary_lines <- c(summary_lines, paste("  Isolation Forest:", outlier_text, "| ω:", omega))
    }
    
    if (use_iqr_filter) {
      outlier_text <- if (keep_outliers_iqr) "(outliers only)" else "(filtered)"
      summary_lines <- c(summary_lines, paste("  IQR Filter:", outlier_text))
    }
    
    if (use_zscore_filter) {
      outlier_text <- if (keep_outliers_zscore) "(outliers only)" else "(filtered)"
      summary_lines <- c(summary_lines, paste("  Z-Score Filter:", outlier_text))
    }
    
    if (use_mad_filter) {
      outlier_text <- if (keep_outliers_mad) "(outliers only)" else "(filtered)"
      summary_lines <- c(summary_lines, paste("  MAD Filter:", outlier_text))
    }
  }
  
  # Add data filtering summary only if filters are applied
  total_filters <- sum(c(use_mahalanobis, use_robust_mahalanobis, use_isolation_forest, use_iqr_filter, use_zscore_filter, use_mad_filter))
  if (total_filters > 0) {
    summary_lines <- c(summary_lines, "")
    summary_lines <- c(summary_lines, "Data Filtering Summary:")
    summary_lines <- c(summary_lines, paste("  Total filters applied:", total_filters))
    outlier_handling <- if (any(as.logical(c(keep_outliers_mahalanobis, keep_outliers_robust, keep_outliers_isolation, keep_outliers_iqr, keep_outliers_zscore, keep_outliers_mad)))) "Keep outliers" else "Remove outliers"
    summary_lines <- c(summary_lines, paste("  Outlier handling:", outlier_handling))
  }
  
  return(paste(summary_lines, collapse = "\n"))
}

# Function to check required packages (no runtime installation)
# Production-ready: No automatic package installation during runtime
# Users must install dependencies before running the app
check_required_packages <- function(packages) {
  missing_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  
  if (length(missing_packages) > 0) {
    cat("WARNING: The following required packages are missing:\n")
    cat(paste("  -", missing_packages), sep = "\n")
    cat("\nPlease install them manually before running the app:\n")
        cat("install.packages(c(", 
        paste(paste0('"', missing_packages, '"'), collapse = ", "), "))\n")
    cat("\nOr use renv for dependency management:\n")
    cat("renv::init()\n")
    cat("renv::install(c(", 
            paste(paste0('"', missing_packages, '"'), collapse = ", "), "))\n")
        
    # Stop execution if critical packages are missing
    critical_packages <- c("shiny", "openxlsx")
    if (any(critical_packages %in% missing_packages)) {
      stop("Critical packages missing. Please install required packages first.")
    }
  }
  
  # Return available packages
  available_packages <- packages[packages %in% installed.packages()[,"Package"]]
  return(available_packages)
}

# Check required packages (no installation)
available_packages <- check_required_packages(required_packages)

# Load available packages with error handling
load_package_safely <- function(package_name) {
  tryCatch({
    library(package_name, character.only = TRUE)
    return(TRUE)
  }, error = function(e) {
    cat("Warning: Could not load package '", package_name, "': ", e$message, "\n")
    return(FALSE)
  })
}

# Load essential packages first
essential_packages <- c("shiny", "openxlsx", "ggplot2")
for (pkg in essential_packages) {
  if (pkg %in% available_packages) {
    load_package_safely(pkg)
  }
}

# Load optional packages
optional_packages <- setdiff(available_packages, essential_packages)
for (pkg in optional_packages) {
  load_package_safely(pkg)
}

# Check if critical packages are loaded
if (!"shiny" %in% loadedNamespaces()) {
  stop("Critical package 'shiny' could not be loaded. Please check your R installation.")
}

cat("Package loading completed. Available packages:", paste(available_packages, collapse = ", "), "\n")

options(shiny.maxRequestSize = 100 * 1024^2)  # 100 MB

# ---- Configuration System ----
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
    max_samples_preview = 10000
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

# Load or create configuration
app_config <- load_config()
if (is.null(app_config)) {
  app_config <- default_config
  save_config(app_config)
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

# Apply validation
app_config <- validate_and_fix_config(app_config)

# ---- Default Directories ----
default_working_dir <- app_config$directories$working_dir
default_output_dir <- app_config$directories$output_dir

# ---- Progress Tracking System ----
progress_tracker <- list(
  operations = list(),
  start_time = NULL,
  current_operation = NULL
)

start_progress <- function(operation_name, total_steps = 100) {
  progress_tracker$current_operation <<- operation_name
  progress_tracker$start_time <<- Sys.time()
  progress_tracker$operations[[operation_name]] <<- list(
    total_steps = total_steps,
    current_step = 0,
    start_time = Sys.time(),
    status = "running"
  )
  cat("Starting:", operation_name, "\n")
}

update_progress <- function(step, message = "", operation_name = NULL) {
  if (is.null(operation_name)) {
    operation_name <- progress_tracker$current_operation
  }
  
  if (!is.null(operation_name) && !is.null(progress_tracker$operations[[operation_name]])) {
    progress_tracker$operations[[operation_name]]$current_step <<- step
    if (nzchar(message)) {
      cat(sprintf("[%s] Step %d/%d: %s\n", 
                  operation_name, step, 
                  progress_tracker$operations[[operation_name]]$total_steps, 
                  message))
    }
  }
}

complete_progress <- function(operation_name = NULL, status = "completed") {
  if (is.null(operation_name)) {
    operation_name <- progress_tracker$current_operation
  }
  
  if (!is.null(operation_name) && !is.null(progress_tracker$operations[[operation_name]])) {
    end_time <- Sys.time()
    duration <- as.numeric(difftime(end_time, progress_tracker$operations[[operation_name]]$start_time, units = "secs"))
    progress_tracker$operations[[operation_name]]$status <<- status
    progress_tracker$operations[[operation_name]]$end_time <<- end_time
    progress_tracker$operations[[operation_name]]$duration <<- duration
    
    cat(sprintf("Completed: %s (%.2f seconds)\n", operation_name, duration))
  }
}

get_progress_summary <- function() {
  if (length(progress_tracker$operations) == 0) {
    return("No operations tracked")
  }
  
  summary_text <- "Progress Summary:\n"
  for (op_name in names(progress_tracker$operations)) {
    op <- progress_tracker$operations[[op_name]]
    if (op$status == "running") {
      progress_pct <- round((op$current_step / op$total_steps) * 100, 1)
      summary_text <- paste0(summary_text, sprintf("  %s: %s (%d%%)\n", 
                                                   op_name, op$status, progress_pct))
    } else {
      summary_text <- paste0(summary_text, sprintf("  %s: %s (%.2fs)\n", 
                                                   op_name, op$status, op$duration))
    }
  }
  return(summary_text)
}

# ---- Helper Analysis Functions ----
# Function to get individual filters for elements (consolidated)
get_individual_filters <- function(elements, dataset_suffix) {
  if (is.null(elements) || length(elements) == 0) {
    return(list())
  }
  
  # For global scope usage, return empty list (will be populated by UI)
  # This function is primarily used in the Shiny server context where 'input' is available
  filters <- list()
  for (element in elements) {
    filters[[element]] <- NULL  # Will be filled by UI
  }
  return(filters)
}

validate_data <- function(df, cols) {
  missing_counts <- sapply(df[, cols, drop=FALSE], function(x) sum(is.na(x)))
  outlier_flags <- sapply(cols, function(col) {
    vals <- as.numeric(df[[col]])
    q1 <- quantile(vals, 0.25, na.rm=TRUE)
    q3 <- quantile(vals, 0.75, na.rm=TRUE)
    iqr <- q3 - q1
    lower <- q1 - 1.5 * iqr
    upper <- q3 + 1.5 * iqr
    sum(vals < lower | vals > upper, na.rm=TRUE)
  })
  list(missing=missing_counts, outliers=outlier_flags)
}

generate_stats <- function(df, cols) {
  stats <- lapply(cols, function(col) {
    vals <- as.numeric(df[[col]])
    list(
      mean=mean(vals, na.rm=TRUE),
      median=median(vals, na.rm=TRUE),
      sd=sd(vals, na.rm=TRUE)
    )
  })
  names(stats) <- cols
  return(stats)
}

compute_correlation <- function(df, cols) {
  cor(df[, cols], use="complete.obs")
}

plot_correlation_matrix <- function(df, cols, file) {
  g <- GGally::ggcorr(df[, cols], label=TRUE)
  ggplot2::ggsave(file, plot=g, width=6, height=6)
}

# ---- New Helper Functions for Improved Code Quality ----
safe_execute <- function(expr, error_msg = "Operation failed") {
  tryCatch({
    eval(expr)
  }, error = function(e) {
    message(error_msg, ": ", e$message)
    return(NULL)
  })
}

# Enhanced data validation with detailed error reporting
validate_data_enhanced <- function(df, cols, operation_name = "Data validation") {
  errors <- character(0)
  warnings <- character(0)
  
  # Check if dataframe exists
  if (is.null(df)) {
    errors <- c(errors, "Dataframe is NULL")
    return(list(valid = FALSE, errors = errors, warnings = warnings))
  }
  
  # Check if dataframe has rows
  if (nrow(df) == 0) {
    errors <- c(errors, "Dataframe has no rows")
    return(list(valid = FALSE, errors = errors, warnings = warnings))
  }
  
  # Check if dataframe has columns
  if (ncol(df) == 0) {
    errors <- c(errors, "Dataframe has no columns")
    return(list(valid = FALSE, errors = errors, warnings = warnings))
  }
  
  # Check if required columns exist
  missing_cols <- setdiff(cols, colnames(df))
  if (length(missing_cols) > 0) {
    errors <- c(errors, paste("Missing columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Check for numeric columns if needed
  numeric_cols <- cols[sapply(df[, cols, drop = FALSE], is.numeric)]
  if (length(numeric_cols) == 0) {
    warnings <- c(warnings, "No numeric columns found in selected columns")
  }
  
  # Check for missing values
  missing_counts <- sapply(df[, cols, drop = FALSE], function(x) sum(is.na(x)))
  high_missing <- names(missing_counts[missing_counts > nrow(df) * 0.5])
  if (length(high_missing) > 0) {
    warnings <- c(warnings, paste("High missing values (>50%) in columns:", paste(high_missing, collapse = ", ")))
  }
  
  # Check for infinite values
  infinite_counts <- sapply(df[, cols, drop = FALSE], function(x) sum(is.infinite(x)))
  infinite_cols <- names(infinite_counts[infinite_counts > 0])
  if (length(infinite_cols) > 0) {
    warnings <- c(warnings, paste("Infinite values found in columns:", paste(infinite_cols, collapse = ", ")))
  }
  
  # Check for zero variance columns
  zero_var_cols <- sapply(df[, cols, drop = FALSE], function(x) {
    if (is.numeric(x)) var(x, na.rm = TRUE) == 0 else FALSE
  })
  zero_var_names <- names(zero_var_cols[zero_var_cols])
  if (length(zero_var_names) > 0) {
    warnings <- c(warnings, paste("Zero variance columns:", paste(zero_var_names, collapse = ", ")))
  }
  
  # Log validation results
  if (length(errors) > 0) {
    log_operation("ERROR", paste(operation_name, "- Validation failed"), paste(errors, collapse = "; "))
  } else if (length(warnings) > 0) {
    log_operation("WARNING", paste(operation_name, "- Validation passed with warnings"), paste(warnings, collapse = "; "))
  } else {
    log_operation("INFO", paste(operation_name, "- Validation passed"), "All checks passed")
  }
  
  return(list(
    valid = length(errors) == 0,
    errors = errors,
    warnings = warnings,
    missing_counts = missing_counts,
    infinite_counts = infinite_counts,
    zero_var_cols = zero_var_names,
    numeric_cols = numeric_cols
  ))
}

log_operation <- function(operation, details, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  message(sprintf("[%s] %s: %s - %s", timestamp, level, operation, details))
}

validate_inputs <- function(inputs) {
  required_fields <- c("element_A", "element_B", "element_C")
  missing_fields <- required_fields[!sapply(required_fields, function(x) !is.null(inputs[[x]]) && length(inputs[[x]]) > 0)]
  
  if (length(missing_fields) > 0) {
    stop("Missing required fields: ", paste(missing_fields, collapse = ", "))
  }
}

# ---- Data Caching for Performance Improvement ----
# Note: Using global hash-keyed cache system instead of local reactiveVal
# Enhanced cache keys include plot styling parameters to prevent mismatches
# when users change visual appearance settings

# Function to get cached data or load from file
get_cached_data <- function(file_path, cache_key) {
  # Generate a proper cache key based on file path and modification time
  file_info <- file.info(file_path)
  cache_key_data <- list(
    file_path = file_path,
    file_size = file_info$size,
    file_mtime = file_info$mtime,
    cache_key = cache_key
  )
  actual_cache_key <- digest::digest(cache_key_data)
  
  # Check if we have valid cached data in the global cache
  cached_result <- get_cached_result(actual_cache_key)
  if (!is.null(cached_result)) {
    log_operation("Data caching", paste("Using cached data for", cache_key, "(cache hit)"))
    return(cached_result)
  }
  
  # Load data from file and cache it
  log_operation("Data caching", paste("Loading data from file for", cache_key, "(cache miss)"))
  data <- openxlsx::read.xlsx(file_path, sheet = 1)
  
  # Cache the result using the global caching system
  cache_result(actual_cache_key, data)
  
  log_operation("Data caching", paste("Data cached for", cache_key, "with", nrow(data), "rows"))
  return(data)
}

# Performance optimization for large datasets
optimize_for_large_datasets <- function(data, max_rows = 100000) {
  optimizations <- list(
    original_dim = dim(data),
    applied = FALSE,
    sampling = FALSE
  )
  
  # Check if optimization is needed
  if (nrow(data) > max_rows) {
    optimizations$applied <- TRUE
    
    # Row sampling for very large datasets
    if (nrow(data) > max_rows) {
      set.seed(123) # For reproducible sampling
      sample_indices <- sample(seq_len(nrow(data)), max_rows)
      data <- data[sample_indices, , drop = FALSE]
      optimizations$sampling <- TRUE
      log_operation("Performance", paste("Sampled", max_rows, "rows from", optimizations$original_dim[1], "total rows"))
    }
  }
  
  return(list(data = data, optimizations = optimizations))
}

# Memory-efficient data processing
process_data_efficiently <- function(data, operation, chunk_size = 10000) {
  if (nrow(data) <= chunk_size) {
    # Process all data at once for small datasets
    return(operation(data))
  }
  
  # Process data in chunks for large datasets
  results <- list()
  total_chunks <- ceiling(nrow(data) / chunk_size)
  
  for (i in 1:total_chunks) {
    start_idx <- (i - 1) * chunk_size + 1
    end_idx <- min(i * chunk_size, nrow(data))
    
    chunk <- data[start_idx:end_idx, , drop = FALSE]
    chunk_result <- operation(chunk)
    
    results[[i]] <- chunk_result
    
    # Update progress
    if (i %% 10 == 0) {
      debug_log("Processed chunk %d/%d (%.1f%%)", i, total_chunks, (i/total_chunks)*100)
    }
  }
  
  # Combine results
  if (is.data.frame(results[[1]])) {
    return(do.call(rbind, results))
  } else if (is.list(results[[1]])) {
    return(do.call(c, results))
  } else {
    return(unlist(results))
  }
}

# ---- Mahalanobis Distance Function ----
# Input validation function for Mahalanobis distance parameters
validate_mahalanobis_inputs <- function(lambda, omega, custom_mdthresh, mdthresh_mode, selected_columns = NULL) {
  # Validate threshold mode and parameters
  if (mdthresh_mode == "manual") {
    if (is.null(custom_mdthresh) || !is.numeric(custom_mdthresh) || custom_mdthresh <= 0) {
      stop("Custom threshold must be a positive numeric value")
    }
    if (custom_mdthresh > 10000) {
      warning("Custom threshold value is very high (", custom_mdthresh, "). This may result in no outliers being detected.")
    }
  } else {
    # Automatic mode validation
    if (!is.numeric(lambda) || lambda < 0) {
      stop("Lambda parameter must be a non-negative numeric value")
    }
    if (!is.numeric(omega) || omega < 0) {
      stop("Omega parameter must be a non-negative numeric value")
    }
    if (lambda - omega < -50) {
      warning("Lambda - omega difference is very negative (", lambda - omega, "). This may result in very strict threshold.")
    }
    if (lambda - omega > 100) {
      warning("Lambda - omega difference is very positive (", lambda - omega, "). This may result in very lenient threshold.")
    }
  }
  
  # Validate selected columns if provided
  if (!is.null(selected_columns)) {
    if (!is.character(selected_columns) || length(selected_columns) == 0) {
      stop("Selected columns must be a non-empty character vector")
    }
    if (length(selected_columns) < 2) {
      stop("At least 2 columns must be selected for Mahalanobis distance calculation")
    }
  }
  
  return(TRUE)
}

compute_mahalanobis_distance <- function(data1, data2, lambda = 1, omega = 0, keep_outliers = FALSE, custom_mdthresh = NULL, selected_columns = NULL, mdthresh_mode = "auto") {
  # Input validation
  validate_mahalanobis_inputs(lambda, omega, custom_mdthresh, mdthresh_mode, selected_columns)
  
  # UNIFIED VALIDATION: Apply consistent pre-checks across all multivariate methods
  validation_result <- validate_multivariate_data(data1, data2, selected_columns, method = "Mahalanobis Distance", min_obs_ratio = 2)
  
  # Extract validated data
  data1_clean <- validation_result$data1_clean
  data2_clean <- validation_result$data2_clean
  common_cols <- validation_result$common_cols
  
  # Log validation results for debugging
  debug_log("DEBUG: Mahalanobis validation passed")
  debug_log("DEBUG: Variables: %d", validation_result$n_vars)
  debug_log("DEBUG: Observations (data): %d", validation_result$n_obs1)
  debug_log("DEBUG: Observations (reference): %d", validation_result$n_obs2)
  debug_log("DEBUG: Condition number: %.2e", validation_result$condition_number)
    if (!is.null(validation_result$high_correlations)) {
    debug_log("DEBUG: High correlations detected")
  }
  
  # Calculate covariance matrix from reference dataset (data2)
  cov_matrix <- cov(data2_clean)
  
  # Calculate Mahalanobis distances for data1 relative to data2
  mahal_distances <- mahalanobis(data1_clean, 
                                 center = colMeans(data2_clean), 
                                 cov = cov_matrix)
  
  # Calculate threshold based on mode
  MDmean <- mean(mahal_distances)
  stdMD <- sd(mahal_distances)
  
  if (getOption("ternary.debug", FALSE)) {
    cat("DEBUG: Threshold calculation:\n")
    cat("DEBUG: mdthresh_mode =", mdthresh_mode, "\n")
    cat("DEBUG: custom_mdthresh =", if (is.null(custom_mdthresh)) "NULL" else custom_mdthresh, "\n")
    cat("DEBUG: lambda =", lambda, ", omega =", omega, "\n")
    cat("DEBUG: MDmean =", MDmean, ", stdMD =", stdMD, "\n")
  }
  
  if (mdthresh_mode == "manual" && !is.null(custom_mdthresh)) {
    MDthresh <- custom_mdthresh
    threshold_method <- "Manual"
    threshold_formula <- sprintf("Manual threshold: %.3f", custom_mdthresh)
    if (getOption("ternary.debug", FALSE)) cat("DEBUG: Using MANUAL threshold:", MDthresh, "\n")
  } else {
    # Default to automatic mode
    if (getOption("ternary.debug", FALSE)) {
      cat("DEBUG: Mode selection: Defaulting to AUTOMATIC mode\n")
      cat("DEBUG: mdthresh_mode was:", mdthresh_mode, "(not 'manual')\n")
    }
    MDthresh <- MDmean + sqrt(100/(100 + lambda - omega)) * stdMD
    threshold_method <- "Automatic (MDthresh=MDmean+√(100/(100+λ-ω))×stdMD)"
    threshold_formula <- sprintf("MDthresh = %.3f + √(100/(100+%.1f-%.1f)) × %.3f = %.3f", 
                               MDmean, lambda, omega, stdMD, MDthresh)
    if (getOption("ternary.debug", FALSE)) cat("DEBUG: Using AUTOMATIC threshold:", MDthresh, "\n")
  }
  
  
  
  # Identify outliers based on custom threshold
  outlier_indices <- mahal_distances > MDthresh
  
  if (getOption("ternary.debug", FALSE)) {
    cat("DEBUG: Outlier detection:\n")
    cat("DEBUG: Threshold used:", MDthresh, "\n")
    cat("DEBUG: Min distance:", min(mahal_distances), "\n")
    cat("DEBUG: Max distance:", max(mahal_distances), "\n")
    cat("DEBUG: Outliers detected:", sum(outlier_indices), "out of", length(mahal_distances), "\n")
    cat("DEBUG: Outlier percentage:", round(100 * sum(outlier_indices) / length(mahal_distances), 1), "%\n")
  }
  
  return(list(
    distances = mahal_distances,
    MDthresh = MDthresh,
    MDmean = MDmean,
    stdMD = stdMD,
    lambda = lambda,
    omega = omega,
    outlier_95 = sum(mahal_distances > threshold_95),
    outlier_99 = sum(mahal_distances > threshold_99),
    outlier_custom = sum(outlier_indices),
    total_points = length(mahal_distances),
    df = df,
    common_cols = common_cols,
    outlier_indices = outlier_indices,
    keep_outliers = keep_outliers,
    threshold_method = threshold_method,
    threshold_formula = threshold_formula
  ))
}

# ---- Enhanced Data Quality Check Function ----
check_data_quality <- function(data1, data2) {
  start_performance_monitor("data_quality_check")
  
  quality_report <- list()
  
  # Find common numeric columns
  numeric_cols1 <- sapply(data1, is.numeric)
  numeric_cols2 <- sapply(data2, is.numeric)
  common_cols <- intersect(colnames(data1)[numeric_cols1], colnames(data2)[numeric_cols2])
  
  quality_report$common_cols <- common_cols
  quality_report$num_common_cols <- length(common_cols)
  quality_report$data1_rows <- nrow(data1)
  quality_report$data2_rows <- nrow(data2)
  
  # Check for missing values
  quality_report$missing_values <- list(
    data1 = sapply(data1, function(x) sum(is.na(x))),
    data2 = sapply(data2, function(x) sum(is.na(x)))
  )
  
  # Check for infinite values
  quality_report$infinite_values <- list(
    data1 = sapply(data1, function(x) sum(is.infinite(x))),
    data2 = sapply(data2, function(x) sum(is.infinite(x)))
  )
  
  # Check for zero variance columns
  quality_report$zero_variance <- list(
    data1 = sapply(data1, function(x) if(is.numeric(x)) var(x, na.rm=TRUE) == 0 else FALSE),
    data2 = sapply(data2, function(x) if(is.numeric(x)) var(x, na.rm=TRUE) == 0 else FALSE)
  )
  
  # Check data types
  quality_report$data_types <- list(
    data1 = sapply(data1, class),
    data2 = sapply(data2, class)
  )
  
  # Check for outliers using IQR method
  quality_report$outliers_iqr <- list(
    data1 = sapply(data1, function(x) {
      if(is.numeric(x)) {
        q1 <- quantile(x, 0.25, na.rm=TRUE)
        q3 <- quantile(x, 0.75, na.rm=TRUE)
        iqr <- q3 - q1
        lower <- q1 - 1.5 * iqr
        upper <- q3 + 1.5 * iqr
        sum(x < lower | x > upper, na.rm=TRUE)
      } else 0
    }),
    data2 = sapply(data2, function(x) {
      if(is.numeric(x)) {
        q1 <- quantile(x, 0.25, na.rm=TRUE)
        q3 <- quantile(x, 0.75, na.rm=TRUE)
        iqr <- q3 - q1
        lower <- q1 - 1.5 * iqr
        upper <- q3 + 1.5 * iqr
        sum(x < lower | x > upper, na.rm=TRUE)
      } else 0
    })
  )
  
  # Enhanced quality metrics
  quality_report$correlation_analysis <- list(
    data1 = if(ncol(data1) > 1) cor(data1[, sapply(data1, is.numeric)], use = "pairwise.complete.obs") else NULL,
    data2 = if(ncol(data2) > 1) cor(data2[, sapply(data2, is.numeric)], use = "pairwise.complete.obs") else NULL
  )
  
  # Check for high correlation (potential multicollinearity)
  quality_report$high_correlation <- list(
    data1 = if(!is.null(quality_report$correlation_analysis$data1)) {
      cor_matrix <- quality_report$correlation_analysis$data1
      high_cor <- which(abs(cor_matrix) > 0.9 & cor_matrix != 1, arr.ind = TRUE)
      if(length(high_cor) > 0) {
        data.frame(
          var1 = rownames(cor_matrix)[high_cor[, 1]],
          var2 = colnames(cor_matrix)[high_cor[, 2]],
          correlation = cor_matrix[high_cor]
        )
      } else NULL
    } else NULL,
    data2 = if(!is.null(quality_report$correlation_analysis$data2)) {
      cor_matrix <- quality_report$correlation_analysis$data2
      high_cor <- which(abs(cor_matrix) > 0.9 & cor_matrix != 1, arr.ind = TRUE)
      if(length(high_cor) > 0) {
        data.frame(
          var1 = rownames(cor_matrix)[high_cor[, 1]],
          var2 = colnames(cor_matrix)[high_cor[, 2]],
          correlation = cor_matrix[high_cor]
        )
      } else NULL
    } else NULL
  )
  
  # Data distribution summary
  quality_report$distribution_summary <- list(
    data1 = sapply(data1, function(x) {
      if(is.numeric(x)) {
        c(mean = mean(x, na.rm = TRUE),
          median = median(x, na.rm = TRUE),
          sd = sd(x, na.rm = TRUE),
          skewness = if(length(x) > 2) tryCatch(moments::skewness(x, na.rm = TRUE), error = function(e) NA) else NA,
          kurtosis = if(length(x) > 2) tryCatch(moments::kurtosis(x, na.rm = TRUE), error = function(e) NA) else NA)
      } else NULL
    }),
    data2 = sapply(data2, function(x) {
      if(is.numeric(x)) {
        c(mean = mean(x, na.rm = TRUE),
          median = median(x, na.rm = TRUE),
          sd = sd(x, na.rm = TRUE),
          skewness = if(length(x) > 2) tryCatch(moments::skewness(x, na.rm = TRUE), error = function(e) NA) else NA,
          kurtosis = if(length(x) > 2) tryCatch(moments::kurtosis(x, na.rm = TRUE), error = function(e) NA) else NA)
      } else NULL
    })
  )
  
  # Overall quality score
  quality_report$quality_score <- list(
    data1 = calculate_quality_score(quality_report$missing_values$data1, 
                                    quality_report$infinite_values$data1,
                                    quality_report$zero_variance$data1,
                                    quality_report$outliers_iqr$data1,
                                    nrow(data1), ncol(data1)),
    data2 = calculate_quality_score(quality_report$missing_values$data2, 
                                    quality_report$infinite_values$data2,
                                    quality_report$zero_variance$data2,
                                    quality_report$outliers_iqr$data2,
                                    nrow(data2), ncol(data2))
  )
  
  end_performance_monitor("data_quality_check")
  return(quality_report)
}

# Helper function to calculate overall quality score
calculate_quality_score <- function(missing_vals, infinite_vals, zero_var, outliers, n_rows, n_cols) {
  total_cells <- n_rows * n_cols
  
  # Penalties
  missing_penalty <- sum(missing_vals) / total_cells * 100
  infinite_penalty <- sum(infinite_vals) / total_cells * 100
  zero_var_penalty <- sum(zero_var) / n_cols * 50
  outlier_penalty <- min(sum(outliers) / total_cells * 20, 30) # Cap at 30%
  
  # Base score starts at 100
  base_score <- 100
  
  # Subtract penalties
  final_score <- base_score - missing_penalty - infinite_penalty - zero_var_penalty - outlier_penalty
  
  # Ensure score is between 0 and 100
  final_score <- max(0, min(100, final_score))
  
  # Assign grade
  grade <- if(final_score >= 90) "A" else
    if(final_score >= 80) "B" else
      if(final_score >= 70) "C" else
        if(final_score >= 60) "D" else "F"
  
  return(list(
    score = round(final_score, 1),
    grade = grade,
    details = list(
      missing_penalty = round(missing_penalty, 1),
      infinite_penalty = round(infinite_penalty, 1),
      zero_var_penalty = round(zero_var_penalty, 1),
      outlier_penalty = round(outlier_penalty, 1)
    )
  ))
}

# ---- Additional Multivariate Analysis Functions ----
compute_robust_mahalanobis <- function(data1, data2, method = "MCD", keep_outliers = FALSE, selected_columns = NULL) {
  # Robust Mahalanobis using Minimum Covariance Determinant (MCD)
  if (!requireNamespace("robustbase", quietly = TRUE)) {
    stop("Package 'robustbase' is required for robust Mahalanobis distance calculation. Please install it first.")
  }
  
  # Validate method parameter
  if (!method %in% c("MCD", "MVE")) {
    stop("Method must be either 'MCD' (Minimum Covariance Determinant) or 'MVE' (Minimum Volume Ellipsoid)")
  }
  
  # UNIFIED VALIDATION: Apply consistent pre-checks across all multivariate methods
  # MCD requires more observations per variable (typically 2-3x)
  validation_result <- validate_multivariate_data(data1, data2, selected_columns, method = paste("Robust Mahalanobis (", method, ")"), min_obs_ratio = 3)
  
  # Extract validated data
  data1_clean <- validation_result$data1_clean
  data2_clean <- validation_result$data2_clean
  common_cols <- validation_result$common_cols
  
  # Log validation results for debugging
  debug_log("DEBUG: Robust Mahalanobis validation passed")
  debug_log("DEBUG: Method: %s", method)
  debug_log("DEBUG: Variables: %d", validation_result$n_vars)
  debug_log("DEBUG: Observations (data): %d", validation_result$n_obs1)
  debug_log("DEBUG: Observations (reference): %d", validation_result$n_obs2)
  debug_log("DEBUG: Condition number: %.2e", validation_result$condition_number)
    if (!is.null(validation_result$high_correlations)) {
    debug_log("DEBUG: High correlations detected")
  }
  
  # Use robust covariance estimation
  tryCatch({
    robust_cov <- robustbase::covMcd(data2_clean)
  }, error = function(e) {
    stop(sprintf(
      "Robust covariance estimation failed: %s\nThis may happen with highly correlated data or insufficient observations.",
      e$message
    ))
  })
  
  # Calculate robust Mahalanobis distances
  robust_distances <- mahalanobis(data1_clean, 
                                  center = robust_cov$center, 
                                  cov = robust_cov$cov)
  
  # Identify outliers using 95% quantile
  threshold_95 <- quantile(robust_distances, 0.95)
  outlier_indices <- robust_distances > threshold_95
  
  return(list(
    distances = robust_distances,
    threshold_95 = threshold_95,
    outlier_count = sum(outlier_indices),
    outlier_99 = sum(robust_distances > threshold_95),
    total_points = length(robust_distances),
    method = method,
    outlier_indices = outlier_indices,
    keep_outliers = keep_outliers,
    common_cols = common_cols,
    robust_center = robust_cov$center,
    robust_cov = robust_cov$cov,
    best_subset_size = robust_cov$best
  ))
}

compute_isolation_forest <- function(data1, data2, contamination = 0.1, keep_outliers = FALSE, selected_columns = NULL) {
  # Isolation Forest for outlier detection
  if (!requireNamespace("isotree", quietly = TRUE)) {
    stop("Package 'isotree' is required for isolation forest outlier detection. Please install it first.")
  }
  
  # Validate contamination parameter
  if (!is.numeric(contamination) || contamination <= 0 || contamination >= 1) {
    stop("Contamination must be a numeric value between 0 and 1 (exclusive)")
  }
  if (contamination > 0.5) {
    warning("Contamination value is high (", contamination, "). This means more than half of the data will be considered outliers.")
  }
  
  # UNIFIED VALIDATION: Apply consistent pre-checks across all multivariate methods
  # Isolation Forest needs sufficient data for training but is more flexible with sample size
  validation_result <- validate_multivariate_data(data1, data2, selected_columns, method = "Isolation Forest", min_obs_ratio = 1.5)
  
  # Extract validated data
  data1_clean <- validation_result$data1_clean
  data2_clean <- validation_result$data2_clean
  common_cols <- validation_result$common_cols
  
  # Additional Isolation Forest specific checks
  if (nrow(data2_clean) < 10) {
    stop(sprintf(
      "Insufficient observations for isolation forest training. Need at least 10 rows, but have %d. Consider using more data or different methods.",
      nrow(data2_clean)
    ))
  }
  
  # Log validation results for debugging
  debug_log("DEBUG: Isolation Forest validation passed")
  debug_log("DEBUG: Variables: %d", validation_result$n_vars)
  debug_log("DEBUG: Observations (data): %d", validation_result$n_obs1)
  debug_log("DEBUG: Observations (reference): %d", validation_result$n_obs2)
  debug_log("DEBUG: Condition number: %.2e", validation_result$condition_number)
    if (!is.null(validation_result$high_correlations)) {
    debug_log("DEBUG: High correlations detected")
  }
  
  # Train isolation forest on reference data
  tryCatch({
    iso_model <- isotree::isolation.forest(data2_clean, contamination = contamination)
  }, error = function(e) {
    stop(sprintf(
      "Isolation forest training failed: %s\nThis may happen with insufficient data or highly correlated columns.",
      e$message
    ))
  })
  
  # Predict anomaly scores for data1
  tryCatch({
    anomaly_scores <- predict(iso_model, data1_clean)
  }, error = function(e) {
    stop(sprintf(
      "Anomaly score prediction failed: %s\nThis may happen if the data structure changed between training and prediction.",
      e$message
    ))
  })
  
  # Determine threshold (top contamination% are outliers)
  threshold <- quantile(anomaly_scores, 1 - contamination)
  
  # Identify outliers
  outlier_indices <- anomaly_scores > threshold
  
  return(list(
    scores = anomaly_scores,
    threshold = threshold,
    contamination = contamination,
    outlier_count = sum(outlier_indices),
    total_points = length(anomaly_scores),
    method = "Isolation Forest",
    outlier_indices = outlier_indices,
    keep_outliers = keep_outliers,
    common_cols = common_cols,
    model = iso_model,
    score_range = range(anomaly_scores),
    score_mean = mean(anomaly_scores),
    score_sd = sd(anomaly_scores)
  ))
}

# ---- Statistical Filtering Functions ----
apply_iqr_filter <- function(data, cols, multiplier = 1.5, keep_outliers = FALSE) {
  # IQR-based outlier filtering
  filtered_data <- data
  outlier_indices <- logical(nrow(data))
  
  for (col in cols) {
    if (is.numeric(data[[col]])) {
      q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
      q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      lower_bound <- q1 - multiplier * iqr
      upper_bound <- q3 + multiplier * iqr
      
      col_outliers <- data[[col]] < lower_bound | data[[col]] > upper_bound
      outlier_indices <- outlier_indices | col_outliers
    }
  }
  
  if (keep_outliers) {
    # Keep only outliers
    filtered_data <- data[outlier_indices, ]
  } else {
    # Remove outliers
    filtered_data <- data[!outlier_indices, ]
  }
  
  return(filtered_data)
}

apply_zscore_filter <- function(data, cols, threshold = 3, keep_outliers = FALSE) {
  # Z-score based outlier filtering
  filtered_data <- data
  outlier_indices <- logical(nrow(data))
  
  for (col in cols) {
    if (is.numeric(data[[col]])) {
      z_scores <- abs(scale(data[[col]]))
      col_outliers <- z_scores > threshold
      outlier_indices <- outlier_indices | col_outliers
    }
  }
  
  if (keep_outliers) {
    # Keep only outliers
    filtered_data <- data[outlier_indices, ]
  } else {
    # Remove outliers
    filtered_data <- data[!outlier_indices, ]
  }
  
  return(filtered_data)
}

apply_mad_filter <- function(data, cols, threshold = 3, keep_outliers = FALSE) {
  # Median Absolute Deviation (MAD) based filtering
  filtered_data <- data
  outlier_indices <- logical(nrow(data))
  
  for (col in cols) {
    if (is.numeric(data[[col]])) {
      median_val <- median(data[[col]], na.rm = TRUE)
      mad_val <- mad(data[[col]], na.rm = TRUE)
      lower_bound <- median_val - threshold * mad_val
      upper_bound <- median_val + threshold * mad_val
      
      col_outliers <- data[[col]] < lower_bound | data[[col]] > upper_bound
      outlier_indices <- outlier_indices | col_outliers
    }
  }
  
  if (keep_outliers) {
    # Keep only outliers
    filtered_data <- data[outlier_indices, ]
  } else {
    # Remove outliers
    filtered_data <- data[!outlier_indices, ]
  }
  
  return(filtered_data)
}

# ---- Report Generation Function ----
generate_report <- function(stats, correlation, plot_files, output_path = NULL) {
  timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  if (is.null(output_path)) {
    output_path <- paste0("Ternary_Analysis_Report_", timestamp, ".html")
  }
  
  # Create a simple HTML report since we don't have the Rmd template
  html_content <- paste0(
    "<!DOCTYPE html>",
    "<html><head><title>Ternary Analysis Report</title></head>",
    "<body><h1>Ternary Analysis Report</h1>",
    "<p>Generated on: ", timestamp, "</p>",
    "<h2>Statistics</h2>",
    "<pre>", capture.output(print(stats)), "</pre>",
    "<h2>Correlation Matrix</h2>",
    "<pre>", capture.output(print(correlation)), "</pre>",
    "</body></html>"
  )
  
  writeLines(html_content, output_path)
  return(output_path)
}

# Note: Old export functions removed - now using generate_filtered_data_for_export() for filtered data export

# Note: Old export summary function removed - no longer needed with new export system

# Enhanced Data Visualization Functions
create_quality_dashboard <- function(quality_report, output_dir = NULL) {
  if (is.null(output_dir)) {
    output_dir <- getwd()
  }
  
  # Create HTML dashboard
  dashboard_html <- paste0(
    "<!DOCTYPE html>",
    "<html><head>",
    "<title>Data Quality Dashboard</title>",
    "<style>",
    "body { font-family: Arial, sans-serif; margin: 20px; }",
    ".metric { background: #f5f5f5; padding: 15px; margin: 10px 0; border-radius: 5px; }",
    ".score { font-size: 24px; font-weight: bold; }",
    ".grade-A { color: #28a745; }",
    ".grade-B { color: #17a2b8; }",
    ".grade-C { color: #ffc107; }",
    ".grade-D { color: #fd7e14; }",
    ".grade-F { color: #dc3545; }",
    "table { border-collapse: collapse; }",
    "th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }",
    "th { background-color: #f2f2f2; }",
    "</style></head><body>",
    "<h1>Data Quality Dashboard</h1>",
    "<p>Generated on: ", Sys.time(), "</p>",
    
    "<h2>Overall Quality Scores</h2>",
    "<div class='metric'>",
    "<h3>Dataset 1</h3>",
    "<div class='score grade-", quality_report$quality_score$data1$grade, "'>",
    "Score: ", quality_report$quality_score$data1$score, "/100 (Grade: ", quality_report$quality_score$data1$grade, ")</div>",
    "<p>Missing: ", quality_report$quality_score$data1$details$missing_penalty, "%, ",
    "Infinite: ", quality_report$quality_score$data1$details$infinite_penalty, "%, ",
    "Zero Variance: ", quality_report$quality_score$data1$details$zero_var_penalty, "%, ",
    "Outliers: ", quality_report$quality_score$data1$details$outlier_penalty, "%</p>",
    "</div>",
    
    "<div class='metric'>",
    "<h3>Dataset 2</h3>",
    "<div class='score grade-", quality_report$quality_score$data2$grade, "'>",
    "Score: ", quality_report$quality_score$data2$score, "/100 (Grade: ", quality_report$quality_score$data2$grade, ")</div>",
    "<p>Missing: ", quality_report$quality_score$data2$details$missing_penalty, "%, ",
    "Infinite: ", quality_report$quality_score$data2$details$infinite_penalty, "%, ",
    "Zero Variance: ", quality_report$quality_score$data2$details$zero_var_penalty, "%, ",
    "Outliers: ", quality_report$quality_score$data2$details$outlier_penalty, "%</p>",
    "</div>",
    
    "<h2>Detailed Analysis</h2>",
    "<h3>Missing Values</h3>",
    "<table><tr><th>Column</th><th>Dataset 1</th><th>Dataset 2</th></tr>",
    paste0("<tr><td>", names(quality_report$missing_values$data1), "</td><td>", 
           quality_report$missing_values$data1, "</td><td>", 
           quality_report$missing_values$data2, "</td></tr>", collapse = ""),
    "</table>",
    
    "<h3>Outliers (IQR Method)</h3>",
    "<table><tr><th>Column</th><th>Dataset 1</th><th>Dataset 2</th></tr>",
    paste0("<tr><td>", names(quality_report$outliers_iqr$data1), "</td><td>", 
           quality_report$outliers_iqr$data1, "</td><td>", 
           quality_report$outliers_iqr$data2, "</td></tr>", collapse = ""),
    "</table>",
    
    "</body></html>"
  )
  
  dashboard_file <- file.path(output_dir, "data_quality_dashboard.html")
  writeLines(dashboard_html, dashboard_file)
  cat("Quality dashboard created:", dashboard_file, "\n")
  return(dashboard_file)
}

# Create correlation heatmap
create_correlation_heatmap <- function(data, output_dir = NULL, filename = "correlation_heatmap") {
  if (is.null(output_dir)) {
    output_dir <- getwd()
  }
  
  if (!requireNamespace("corrplot", quietly = TRUE)) {
    cat("Package 'corrplot' not available for correlation heatmap\n")
    return(NULL)
  }
  
  # Get numeric columns
  numeric_cols <- sapply(data, is.numeric)
  if (sum(numeric_cols) < 2) {
    cat("Need at least 2 numeric columns for correlation heatmap\n")
    return(NULL)
  }
  
  numeric_data <- data[, numeric_cols, drop = FALSE]
  
  # Calculate correlation matrix
  cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
  
  # Create heatmap
  png_file <- file.path(output_dir, paste0(filename, ".png"))
  png(png_file, width = 800, height = 600)
  
  corrplot::corrplot(cor_matrix, 
                     method = "color",
                     type = "upper",
                     order = "hclust",
                     tl.cex = 0.8,
                     tl.col = "black",
                     addCoef.col = "black",
                     number.cex = 0.6)
  
  dev.off()
  
  cat("Correlation heatmap created:", png_file, "\n")
  return(png_file)
}

# Create distribution plots
create_distribution_plots <- function(data, output_dir = NULL, filename = "distributions") {
  if (is.null(output_dir)) {
    output_dir <- getwd()
  }
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cat("Package 'ggplot2' not available for distribution plots\n")
    return(NULL)
  }
  
  numeric_cols <- sapply(data, is.numeric)
  if (sum(numeric_cols) == 0) {
    cat("No numeric columns found for distribution plots\n")
    return(NULL)
  }
  
  numeric_data <- data[, numeric_cols, drop = FALSE]
  
  # Create plots for each numeric column
  plot_files <- list()
  
  for (col in colnames(numeric_data)) {
    png_file <- file.path(output_dir, paste0(filename, "_", col, ".png"))
    png(png_file, width = 600, height = 400)
    
    # Set margins to prevent title clipping
    op <- par(oma = c(0, 0, 2, 0))
    
    # Histogram with density
    hist(numeric_data[[col]], 
         main = paste("Distribution of", col),
         xlab = col,
         freq = FALSE,
         col = "lightblue",
         border = "black")
    
    # Add density line
    if (sum(!is.na(numeric_data[[col]])) > 1) {
      lines(density(numeric_data[[col]], na.rm = TRUE), col = "red", lwd = 2)
    }
    
    # Restore margins immediately
    par(op)
    dev.off()
    plot_files[[col]] <- png_file
  }
  
  debug_log("Distribution plots created: %d files", length(plot_files))
  return(plot_files)
}

# Performance Monitoring System
performance_monitor <- list(
  operations = list(),
  memory_usage = list(),
  start_time = Sys.time()
)

start_performance_monitor <- function(operation_name) {
  performance_monitor$operations[[operation_name]] <<- list(
    start_time = Sys.time(),
    start_memory = gc(reset = TRUE)
  )
  debug_log("Performance monitoring started for: %s", operation_name)
}

end_performance_monitor <- function(operation_name) {
  if (!is.null(performance_monitor$operations[[operation_name]])) {
    end_time <- Sys.time()
    end_memory <- gc(reset = FALSE)
    
    duration <- as.numeric(difftime(end_time, performance_monitor$operations[[operation_name]]$start_time, units = "secs"))
    memory_diff <- end_memory[2, 3] - performance_monitor$operations[[operation_name]]$start_memory[2, 3]
    
    performance_monitor$operations[[operation_name]]$end_time <<- end_time
    performance_monitor$operations[[operation_name]]$duration <<- duration
    performance_monitor$operations[[operation_name]]$memory_used <<- memory_diff
    
    debug_log("Performance: %s completed in %.2fs, memory: %.2f MB", 
              operation_name, duration, memory_diff / 1024^2)
  }
}

get_performance_summary <- function() {
  if (length(performance_monitor$operations) == 0) {
    return("No performance data available")
  }
  
  summary_text <- "Performance Summary:\n"
  total_time <- 0
  
  for (op_name in names(performance_monitor$operations)) {
    op <- performance_monitor$operations[[op_name]]
    if (!is.null(op$duration)) {
      summary_text <- paste0(summary_text, 
                             sprintf("  %s: %.2fs (%.2f MB)\n", 
                                     op_name, op$duration, 
                                     ifelse(is.null(op$memory_used), 0, op$memory_used / 1024^2)))
      total_time <- total_time + op$duration
    }
  }
  
  summary_text <- paste0(summary_text, sprintf("\nTotal time: %.2fs\n", total_time))
  return(summary_text)
}

# Memory usage monitoring
monitor_memory_usage <- function() {
  current_memory <- gc(reset = FALSE)
  memory_info <- list(
    timestamp = Sys.time(),
    used = current_memory[2, 3],
    gc_count = current_memory[2, 4]
  )
  
  performance_monitor$memory_usage[[length(performance_monitor$memory_usage) + 1]] <<- memory_info
  
  # Keep only last 100 memory snapshots
  if (length(performance_monitor$memory_usage) > 100) {
    performance_monitor$memory_usage <<- tail(performance_monitor$memory_usage, 100)
  }
  
  return(memory_info)
}

# Comprehensive Analysis Pipeline
run_comprehensive_analysis <- function(data1, data2, analysis_config = NULL, output_dir = NULL) {
  if (is.null(output_dir)) {
    output_dir <- file.path(getwd(), "analysis_output")
  }
  
  # Create output directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Start overall performance monitoring
  start_performance_monitor("comprehensive_analysis")
  
  # Initialize results container
  results <- list(
    timestamp = Sys.time(),
    config = analysis_config,
    output_dir = output_dir
  )
  
  cat("=== Starting Comprehensive Analysis ===\n")
  cat("Output directory:", output_dir, "\n")
  cat("Dataset 1 dimensions:", dim(data1), "\n")
  cat("Dataset 2 dimensions:", dim(data2), "\n\n")
  
  # Step 1: Data Quality Assessment
  cat("Step 1: Assessing data quality...\n")
  start_progress(1, "Data quality assessment", 6)
  
  quality_report <- check_data_quality(data1, data2)
  results$quality_report <- quality_report
  
  # Create quality dashboard
  quality_dashboard <- create_quality_dashboard(quality_report, output_dir)
  results$quality_dashboard <- quality_dashboard
  
  update_progress(1, "Data quality assessment completed")
  
  # Step 2: Data Visualization
  cat("Step 2: Creating visualizations...\n")
  start_progress(2, "Data visualization", 6)
  
  # Correlation heatmaps
  cor_heatmap1 <- create_correlation_heatmap(data1, output_dir, "dataset1_correlation")
  cor_heatmap2 <- create_correlation_heatmap(data2, output_dir, "dataset2_correlation")
  
  # Distribution plots
  dist_plots1 <- create_distribution_plots(data1, output_dir, "dataset1_distributions")
  dist_plots2 <- create_distribution_plots(data2, output_dir, "dataset2_distributions")
  
  results$visualizations <- list(
    correlation_heatmaps = list(dataset1 = cor_heatmap1, dataset2 = cor_heatmap2),
    distribution_plots = list(dataset1 = dist_plots1, dataset2 = dist_plots2)
  )
  
  update_progress(2, "Data visualization completed")
  
  # Step 3: Statistical Analysis
  cat("Step 3: Performing statistical analysis...\n")
  start_progress(3, "Statistical analysis", 6)
  
  # Basic statistics
  stats1 <- generate_stats(data1, colnames(data1)[sapply(data1, is.numeric)])
  stats2 <- generate_stats(data2, colnames(data2)[sapply(data2, is.numeric)])
  
  # Correlation analysis
  cor1 <- compute_correlation(data1, colnames(data1)[sapply(data1, is.numeric)])
  cor2 <- compute_correlation(data2, colnames(data2)[sapply(data2, is.numeric)])
  
  results$statistics <- list(
    dataset1 = stats1,
    dataset2 = stats2,
    correlations = list(dataset1 = cor1, dataset2 = cor2)
  )
  
  update_progress(3, "Statistical analysis completed")
  
  # Step 4: Multivariate Analysis
  cat("Step 4: Performing multivariate analysis...\n")
  start_progress(4, "Multivariate analysis", 6)
  
  # Get numeric columns for multivariate analysis
  numeric_cols1 <- colnames(data1)[sapply(data1, is.numeric)]
  numeric_cols2 <- colnames(data2)[sapply(data2, is.numeric)]
  common_numeric_cols <- intersect(numeric_cols1, numeric_cols2)
  
  if (length(common_numeric_cols) >= 2) {
    # Standard Mahalanobis
    tryCatch({
      mahal_result <- compute_mahalanobis_distance(data1, data2, 
                                                   selected_columns = common_numeric_cols[seq_len(min(5, length(common_numeric_cols)))],
                                                   mdthresh_mode = "auto")
      results$mahalanobis_standard <- mahal_result
    }, error = function(e) {
      cat("Standard Mahalanobis failed:", e$message, "\n")
    })
    
    # Robust Mahalanobis
    tryCatch({
      robust_result <- compute_robust_mahalanobis(data1, data2, 
                                                  selected_columns = common_numeric_cols[seq_len(min(5, length(common_numeric_cols)))])
      results$mahalanobis_robust <- robust_result
    }, error = function(e) {
      cat("Robust Mahalanobis failed:", e$message, "\n")
    })
    
    # Isolation Forest
    tryCatch({
      isolation_result <- compute_isolation_forest(data1, data2, 
                                                   selected_columns = common_numeric_cols[seq_len(min(5, length(common_numeric_cols)))])
      results$isolation_forest <- isolation_result
    }, error = function(e) {
      cat("Isolation Forest failed:", e$message, "\n")
    })
  }
  
  update_progress(4, "Multivariate analysis completed")
  
  # Step 5: Data Export (fixed, explicit paths)
  cat("Step 5: Exporting results.\n")
  start_progress(5, "Data export", 6)
  
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  files <- list(
    dataset1_csv        = file.path(output_dir, "dataset1.csv"),
    dataset2_csv        = file.path(output_dir, "dataset2.csv"),
    quality_summary_csv = file.path(output_dir, "quality_summary.csv")
  )
  
  write.csv(data1, files$dataset1_csv, row.names = FALSE)
  write.csv(data2, files$dataset2_csv, row.names = FALSE)
  
  quality_summary_df <- data.frame(
    metric   = c("Missing Values","Infinite Values","Zero Variance","Outliers"),
        dataset1 = c(sum(quality_report$missing_values$data1),
                     sum(quality_report$infinite_values$data1),
                     sum(quality_report$zero_variance$data1),
                     sum(quality_report$outliers_iqr$data1)),
        dataset2 = c(sum(quality_report$missing_values$data2),
                     sum(quality_report$infinite_values$data2),
                     sum(quality_report$zero_variance$data2),
                     sum(quality_report$outliers_iqr$data2))
      )
  write.csv(quality_summary_df, files$quality_summary_csv, row.names = FALSE)
  
  results$export_files <- files
  
  update_progress(5, "Data export completed")
  
  # Step 6: Generate Report
  cat("Step 6: Generating final report...\n")
  start_progress(6, "Report generation", 6)
  
  # Create comprehensive report
  report_file <- generate_comprehensive_report(results, output_dir)
  results$final_report <- report_file
  
  update_progress(6, "Report generation completed")
  
  # Complete analysis
  complete_progress("comprehensive_analysis")
  end_performance_monitor("comprehensive_analysis")
  
  cat("\n=== Comprehensive Analysis Completed ===\n")
  cat("Results saved to:", output_dir, "\n")
  cat("Performance summary:\n")
  cat(get_performance_summary())
  
  return(results)
}

# Generate comprehensive report
generate_comprehensive_report <- function(results, output_dir) {
  report_html <- paste0(
    "<!DOCTYPE html>",
    "<html><head>",
    "<title>Comprehensive Analysis Report</title>",
    "<style>",
    "body { font-family: Arial, sans-serif; margin: 20px; line-height: 1.6; }",
    ".header { background: #2c3e50; color: white; padding: 20px; border-radius: 5px; }",
    ".section { background: #f8f9fa; padding: 15px; margin: 15px 0; border-radius: 5px; border-left: 4px solid #007bff; }",
    ".metric { background: white; padding: 10px; margin: 10px 0; border-radius: 3px; box-shadow: 0 1px 3px rgba(0,0,0,0.1); }",
    ".score { font-size: 20px; font-weight: bold; }",
    ".grade-A { color: #28a745; }",
    ".grade-B { color: #17a2b8; }",
    ".grade-C { color: #ffc107; }",
    ".grade-D { color: #fd7e14; }",
    ".grade-F { color: #dc3545; }",
    "table { border-collapse: collapse; margin: 10px 0; }",
    "th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }",
    "th { background-color: #f2f2f2; }",
    ".file-list { background: #e9ecef; padding: 10px; border-radius: 3px; }",
    "</style></head><body>",
    
    "<div class='header'>",
    "<h1>Comprehensive Analysis Report</h1>",
    "<p>Generated on: ", format(results$timestamp, "%Y-%m-%d %H:%M:%S"), "</p>",
    "</div>",
    
    "<div class='section'>",
    "<h2>Executive Summary</h2>",
    "<p>This report presents a comprehensive analysis of two datasets including data quality assessment, ",
    "statistical analysis, multivariate analysis, and visualization results.</p>",
    "</div>",
    
    "<div class='section'>",
    "<h2>Data Quality Assessment</h2>",
    "<div class='metric'>",
    "<h3>Dataset 1 Quality Score</h3>",
    "<div class='score grade-", results$quality_report$quality_score$data1$grade, "'>",
    "Score: ", results$quality_report$quality_score$data1$score, "/100 (Grade: ", results$quality_report$quality_score$data1$grade, ")</div>",
    "</div>",
    "<div class='metric'>",
    "<h3>Dataset 2 Quality Score</h3>",
    "<div class='score grade-", results$quality_report$quality_score$data2$grade, "'>",
    "Score: ", results$quality_report$quality_score$data2$score, "/100 (Grade: ", results$quality_report$quality_score$data2$grade, ")</div>",
    "</div>",
    "</div>",
    
    "<div class='section'>",
    "<h2>Generated Files</h2>",
    "<div class='file-list'>",
    "<h3>Visualizations</h3>",
    "<ul>",
    ifelse(!is.null(results$visualizations$correlation_heatmaps$dataset1), 
           "<li>Dataset 1 Correlation Heatmap</li>", ""),
    ifelse(!is.null(results$visualizations$correlation_heatmaps$dataset2), 
           "<li>Dataset 2 Correlation Heatmap</li>", ""),
    ifelse(!is.null(results$visualizations$distribution_plots$dataset1), 
           "<li>Dataset 1 Distribution Plots</li>", ""),
    ifelse(!is.null(results$visualizations$distribution_plots$dataset2), 
           "<li>Dataset 2 Distribution Plots</li>", ""),
    "</ul>",
    "<h3>Data Exports</h3>",
    "<ul>",
    if (is.list(results$export_files)) {
      paste0("<li>", names(results$export_files), ": ",
             basename(unlist(results$export_files)), "</li>", collapse = "")
    } else {
      paste0("<li>", basename(results$export_files), "</li>", collapse = "")
    },
    "</ul>",
    "</div>",
    "</div>",
    
    "<div class='section'>",
    "<h2>Analysis Details</h2>",
    "<p>For detailed analysis results, please refer to the individual files listed above.</p>",
    "<p>All results are saved in: ", output_dir, "</p>",
    "</div>",
    
    "</body></html>"
  )
  
  report_file <- file.path(output_dir, "comprehensive_analysis_report.html")
  writeLines(report_html, report_file)
  cat("Comprehensive report created:", report_file, "\n")
  return(report_file)
}

# ---- Main Ternary Plot Function ----

general_ternary_plot <- function(
    xlsx_file,
    working_dir = getwd(),
    output_dir = NULL,
    element_A,
    element_B,
    element_C,
    optional_param1 = NULL,
    optional_param2 = NULL,
    color_palette = "blue",
    xlsx_display_name = NULL,
    preview = FALSE,
    use_mahalanobis = FALSE,
    reference_data = NULL,  # Must be provided by caller for dataset1/dataset2 reference modes
    optional_param1_representation = "point_size",
    output_format = "png",
    use_robust_mahalanobis = FALSE,
    use_isolation_forest = FALSE,
    use_iqr_filter = FALSE,
    use_zscore_filter = FALSE,
    use_mad_filter = FALSE,
    lambda = 1,
    omega = 0,
    keep_outliers_mahalanobis = FALSE,
    keep_outliers_robust = FALSE,
    keep_outliers_isolation = FALSE,
    keep_outliers_iqr = FALSE,
    keep_outliers_zscore = FALSE,
    keep_outliers_mad = FALSE,
    individual_filters_A = NULL,
    individual_filters_B = NULL,
    individual_filters_C = NULL,
    custom_mdthresh = NULL,
    mdthresh_mode = "auto",
    mahalanobis_reference = "dataset2",
    selected_columns = NULL,
    # Inclusion area feature removed
    include_plot_notes = TRUE
) {
  
  # Input validation
  log_operation("Input validation", "Starting validation of function parameters")
  
  if (is.null(xlsx_file) || !file.exists(xlsx_file)) {
    stop("Invalid xlsx_file: file does not exist or is NULL")
  }
  
  if (is.null(element_A) || is.null(element_B) || is.null(element_C)) {
    stop("Missing required elements: element_A, element_B, and element_C must be provided")
  }
  
  if (length(element_A$col) == 0 || length(element_B$col) == 0 || length(element_C$col) == 0) {
    stop("Empty element columns: all elements must have at least one column selected")
  }
  
  if (!output_format %in% c("png", "jpeg", "pdf", "tiff")) {
    stop("Invalid output_format: must be one of 'png', 'jpeg', 'pdf', 'tiff'")
  }
  
  # Inclusion area validation removed
  
  log_operation("Input validation", "All inputs validated successfully")
  
  # Store original working directory and restore on exit
  # This prevents setwd() side-effects from affecting Shiny's working directory
  original_wd <- getwd()
  on.exit(setwd(original_wd), add = TRUE)
  setwd(working_dir)
  log_operation("Data loading", paste("Loading data from:", xlsx_file))
  M <- openxlsx::read.xlsx(xlsx_file, sheet = 1)
  if (getOption("ternary.debug", FALSE)) {
  cat("DEBUG: Initial data loaded, dimensions:", dim(M), "\n")
  cat("DEBUG: Available columns:", paste(colnames(M), collapse = ", "), "\n")
  }
  if (!preview) print(colnames(M))
  
  log_operation("Data dimensions", paste("Initial data dimensions:", dim(M)[1], "rows x", dim(M)[2], "columns"))
  
  apply_filter <- function(df, col, filter) {
    if (is.null(filter)) return(df)
    
    # Safe filtering using base R functions instead of dangerous eval()
    if (grepl("^[><=!]+", filter)) {
      # Handle comparison operators safely
      operator <- gsub("^([><=!]+).*", "\\1", filter)
      value_str <- gsub("^[><=!]+\\s*", "", filter)
      value <- as.numeric(value_str)
      
      if (is.na(value)) {
        stop("Invalid filter value: ", value_str, ". Must be a numeric value.")
      }
      
      if (operator == ">") return(df[df[[col]] > value, , drop = FALSE])
      if (operator == "<") return(df[df[[col]] < value, , drop = FALSE])
      if (operator == ">=") return(df[df[[col]] >= value, , drop = FALSE])
      if (operator == "<=") return(df[df[[col]] <= value, , drop = FALSE])
      if (operator == "==") return(df[df[[col]] == value, , drop = FALSE])
      if (operator == "!=") return(df[df[[col]] != value, , drop = FALSE])
    }
    
    stop("Invalid filter format. Use operators: >, <, >=, <=, ==, !=")
  }
  
  label_with_element <- function(el) {
    if (is.null(el$col)) return("")
    if (length(el$col) > 1) paste(el$col, collapse = "+") else el$col
  }
  label_with_filter <- function(el) {
    if (is.null(el$col)) return("")
    col_label <- if (length(el$col) > 1) paste(el$col, collapse = "+") else el$col
    if (!is.null(el$filter) && nzchar(as.character(el$filter))) {
      paste0(col_label, " (", paste(el$filter, collapse = ", "), ")")
    } else {
      col_label
    }
  }
  
  # Intelligent title splitting function for better readability
  create_multi_line_title <- function(title_parts) {
    if (length(title_parts) <= 3) {
      # Short titles: keep as single line
      return(paste(title_parts, collapse = ", "))
    }
    
    # Check for extremely long individual parts that need breaking
    title_parts <- sapply(title_parts, function(part) {
      if (nchar(part) > 60) {
        # Break very long parts at natural break points
        if (grepl("\\(", part)) {
          # Break at parentheses
          gsub("\\(", "\n(", part)
        } else if (grepl(":", part)) {
          # Break at colons
          gsub(":", ":\n", part)
        } else {
          # Break at spaces if possible
          words <- strsplit(part, " ")[[1]]
          if (length(words) > 3) {
            mid <- ceiling(length(words) / 2)
            paste(paste(words[1:mid], collapse = " "), paste(words[(mid+1):length(words)], collapse = " "), sep = "\n")
          } else {
            part
          }
        }
      } else {
        part
      }
    })
    
    # Calculate total character length
    total_length <- sum(nchar(title_parts)) + (length(title_parts) - 1) * 2  # +2 for ", "
    
    if (total_length <= 80) {
      # Short enough: single line
      return(paste(title_parts, collapse = ", "))
    }
    
    # Long titles: split intelligently
    if (length(title_parts) <= 6) {
      # Split roughly in half
      mid_point <- ceiling(length(title_parts) / 2)
      title_line1 <- paste(title_parts[1:mid_point], collapse = ", ")
      title_line2 <- paste(title_parts[(mid_point + 1):length(title_parts)], collapse = ", ")
      return(paste(title_line1, title_line2, sep = "\n"))
    } else {
      # For very long titles, create 3 lines
      line1_count <- ceiling(length(title_parts) / 3)
      line2_count <- ceiling((length(title_parts) - line1_count) / 2)
      
      title_line1 <- paste(title_parts[1:line1_count], collapse = ", ")
      title_line2 <- paste(title_parts[(line1_count + 1):(line1_count + line2_count)], collapse = ", ")
      title_line3 <- paste(title_parts[(line1_count + line2_count + 1):length(title_parts)], collapse = ", ")
      
      return(paste(title_line1, title_line2, title_line3, sep = "\n"))
    }
  }
  
  # Function to preview title layout
  preview_title_layout <- function(title_parts) {
    final_title <- create_multi_line_title(title_parts)
    if (getOption("ternary.debug", FALSE)) {
      cat("DEBUG: Title preview:\n")
      cat("Original parts:", length(title_parts), "\n")
      cat("Final title:\n", final_title, "\n")
      cat("Line count:", length(strsplit(final_title, "\n")[[1]]), "\n")
    }
    return(final_title)
  }
  
  # Function to calculate optimal plot dimensions based on title length
  calculate_plot_dimensions <- function(title_parts) {
    final_title <- create_multi_line_title(title_parts)
    line_count <- length(strsplit(final_title, "\n")[[1]])
    
    # Base dimensions
    base_width <- 1200
    base_height <- 1400
    
    # Adjust height based on title lines
    if (line_count == 1) {
      # Single line: standard height
      height <- base_height
    } else if (line_count == 2) {
      # Two lines: increase height slightly
      height <- base_height + 100
    } else {
      # Three or more lines: increase height more
      height <- base_height + 200
    }
    
    return(list(width = base_width, height = height))
  }
  
  # Apply individual element filtering (A, B, C) with individual filters
  apply_individual_filters <- function(data, element, individual_filters, element_name, preview = FALSE) {
    if (is.null(element) || is.null(element$col) || length(element$col) == 0) {
          if (getOption("ternary.debug", FALSE)) cat("DEBUG: No", element_name, "elements selected\n")
      return(data)
    }
    
  if (getOption("ternary.debug", FALSE)) {
    cat("DEBUG: Processing", element_name, "elements:", paste(element$col, collapse = ", "), "\n")
    cat("DEBUG: Individual filters for", element_name, ":", if(is.null(individual_filters)) "NULL" else paste(names(individual_filters), collapse = ", "), "\n")
  }
    
    # If no individual filters provided, use the old single filter method
    if (is.null(individual_filters) || length(individual_filters) == 0) {
              if (!is.null(element$filter) && !is.na(element$filter) && nzchar(as.character(element$filter))) {
        if (length(element$col) > 1) {
          # For multiple columns, apply same filter to each column individually
          data[, element$col] <- lapply(data[, element$col, drop = FALSE], as.numeric)
          keep_rows <- rep(TRUE, nrow(data))
          for (col in element$col) {
            # Safe filtering instead of eval()
            if (grepl("^[><=!]+", element$filter)) {
              operator <- gsub("^([><=!]+).*", "\\1", element$filter)
              value_str <- gsub("^[><=!]+\\s*", "", element$filter)
              value <- as.numeric(value_str)
              
              if (is.na(value)) {
                stop("Invalid filter value: ", value_str, ". Must be a numeric value.")
              }
              
              if (operator == ">") col_filter <- data[[col]] > value
              else if (operator == "<") col_filter <- data[[col]] < value
              else if (operator == ">=") col_filter <- data[[col]] >= value
              else if (operator == "<=") col_filter <- data[[col]] <= value
              else if (operator == "==") col_filter <- data[[col]] == value
              else if (operator == "!=") col_filter <- data[[col]] != value
              else stop("Invalid filter format. Use operators: >, <, >=, <=, ==, !=")
              
              keep_rows <- keep_rows & col_filter
            } else {
              stop("Invalid filter format. Use operators: >, <, >=, <=, ==, !=")
            }
          }
          data <- data[keep_rows, , drop = FALSE]
        } else {
          data[, element$col] <- as.numeric(data[, element$col])
          # Handle single column with list filter structure
          if (is.list(element$filter) && length(element$filter) > 0) {
            # Extract the actual filter value from the list
            filter_value <- element$filter[[1]]
            if (!is.null(filter_value) && !is.na(filter_value) && nchar(trimws(as.character(filter_value))) > 0) {
              data <- apply_filter(data, element$col, filter_value)
            }
          } else {
            # Direct filter value
          data <- apply_filter(data, element$col, element$filter)
          }
        }
        if (!preview) {
          cat("After filtering", paste(element$col, collapse = "+"), "with filter", paste(element$filter, collapse = ", "), ":\n")
          print(dim(data))
        }
      }
    } else {
      # Apply individual filters to each element
      data[, element$col] <- lapply(data[, element$col, drop = FALSE], as.numeric)
      keep_rows <- rep(TRUE, nrow(data))
      
      for (col in element$col) {
        if (col %in% names(individual_filters) && !is.null(individual_filters[[col]]) && !is.na(individual_filters[[col]]) && nzchar(as.character(individual_filters[[col]]))) {
          # Safe filtering instead of eval()
          filter_expr <- individual_filters[[col]]
          if (grepl("^[><=!]+", filter_expr)) {
            operator <- gsub("^([><=!]+).*", "\\1", filter_expr)
            value_str <- gsub("^[><=!]+\\s*", "", filter_expr)
            value <- as.numeric(value_str)
            
            if (is.na(value)) {
              # Try to clean the value string by removing invalid characters
              clean_value_str <- gsub("[^0-9.-]", "", value_str)
              value <- as.numeric(clean_value_str)
            if (is.na(value)) {
              stop("Invalid filter value: ", value_str, ". Must be a numeric value.")
              }
            }
            
            if (operator == ">") col_filter <- data[[col]] > value
            else if (operator == "<") col_filter <- data[[col]] < value
            else if (operator == ">=") col_filter <- data[[col]] >= value
            else if (operator == "<=") col_filter <- data[[col]] <= value
            else if (operator == "==") col_filter <- data[[col]] == value
            else if (operator == "!=") col_filter <- data[[col]] != value
            else stop("Invalid filter format. Use operators: >, <, >=, <=, ==, !=")
            
            keep_rows <- keep_rows & col_filter
          } else {
            stop("Invalid filter format. Use operators: >, <, >=, <=, ==, !=")
          }
          
          if (!preview) {
            cat("Applied filter to", col, ":", individual_filters[[col]], "\n")
          }
        }
      }
      
      data <- data[keep_rows, , drop = FALSE]
      if (!preview) {
        cat("After filtering", element_name, "elements with individual filters:\n")
        print(dim(data))
      }
    }
    
      if (getOption("ternary.debug", FALSE)) cat("DEBUG: After", element_name, "filtering, data dimensions:", dim(data), "\n")
    return(data)
  }
  
  # Apply filtering for elements A, B, C
  log_operation("Element filtering", "Starting element filtering process")
  M <- apply_individual_filters(M, element_A, individual_filters_A, "A", preview)
  M <- apply_individual_filters(M, element_B, individual_filters_B, "B", preview)
  M <- apply_individual_filters(M, element_C, individual_filters_C, "C", preview)
  
  if (getOption("ternary.debug", FALSE)) cat("DEBUG: After all element filtering, data dimensions:", dim(M), "\n")
  log_operation("Element filtering", paste("Completed. Data dimensions after filtering:", dim(M)[1], "rows x", dim(M)[2], "columns"))
  
  # Apply optional parameter filtering (sum filtering for these)
  log_operation("Optional parameter filtering", "Starting optional parameter filtering")
  for (el in list(optional_param1, optional_param2)) {
    if (!is.null(el) && !is.null(el$col) && !is.null(el$filter) && !is.na(el$filter) && nzchar(as.character(el$filter))) {
      log_operation("Optional parameter filtering", paste("Processing parameter:", paste(el$col, collapse = "+"), "with filter:", paste(el$filter, collapse = ", ")))
      if (length(el$col) > 1) {
        M[, el$col] <- lapply(M[, el$col, drop = FALSE], as.numeric)
        row_sum <- rowSums(M[, el$col, drop = FALSE], na.rm = TRUE)
        
        # Safe filtering instead of eval()
        if (grepl("^[><=!]+", el$filter)) {
          operator <- gsub("^([><=!]+).*", "\\1", el$filter)
          value_str <- gsub("^[><=!]+\\s*", "", el$filter)
          value <- as.numeric(value_str)
          
          if (is.na(value)) {
            # Try to clean the value string by removing invalid characters
            clean_value_str <- gsub("[^0-9.-]", "", value_str)
            value <- as.numeric(clean_value_str)
          if (is.na(value)) {
            stop("Invalid filter value: ", value_str, ". Must be a numeric value.")
            }
          }
          
          if (operator == ">") M <- M[row_sum > value, , drop = FALSE]
          else if (operator == "<") M <- M[row_sum < value, , drop = FALSE]
          else if (operator == ">=") M <- M[row_sum >= value, , drop = FALSE]
          else if (operator == "<=") M <- M[row_sum <= value, , drop = FALSE]
          else if (operator == "==") M <- M[row_sum == value, , drop = FALSE]
          else if (operator == "!=") M <- M[row_sum != value, , drop = FALSE]
          else stop("Invalid filter format. Use operators: >, <, >=, <=, ==, !=")
        } else {
          stop("Invalid filter format. Use operators: >, <, >=, <=, ==, !=")
        }
      } else {
        M[, el$col] <- as.numeric(M[, el$col])
        # Handle single column with list filter structure
        if (is.list(el$filter) && length(el$filter) > 0) {
          # Extract the actual filter value from the list
          filter_value <- el$filter[[1]]
          if (!is.null(filter_value) && !is.na(filter_value) && nchar(trimws(as.character(filter_value))) > 0) {
            M <- apply_filter(M, el$col, filter_value)
          }
        } else {
          # Direct filter value
        M <- apply_filter(M, el$col, el$filter)
        }
      }
      if (!preview) {
        cat("After filtering", paste(el$col, collapse = "+"), "with filter", paste(el$filter, collapse = ", "), ":\n")
        print(dim(M))
        print(colnames(M))
      }
      log_operation("Optional parameter filtering", paste("Completed filtering. New dimensions:", dim(M)[1], "rows x", dim(M)[2], "columns"))
    }
  }
  if (nrow(M) == 0) stop("No data left after filtering. Please check your filter conditions and data.")
  
  if (getOption("ternary.debug", FALSE)) cat("DEBUG: After optional parameter filtering, data dimensions:", dim(M), "\n")
  
  # Apply statistical filtering methods
  all_selected_elements <- c(element_A$col, element_B$col, element_C$col)
  
  if (use_iqr_filter) {
    M <- apply_iqr_filter(M, all_selected_elements, keep_outliers = keep_outliers_iqr)
    if (!preview) cat("Applied IQR filtering\n")
  }
  
  if (use_zscore_filter) {
    M <- apply_zscore_filter(M, all_selected_elements, keep_outliers = keep_outliers_zscore)
    if (!preview) cat("Applied Z-score filtering\n")
  }
  
  if (use_mad_filter) {
    M <- apply_mad_filter(M, all_selected_elements, keep_outliers = keep_outliers_mad)
    if (!preview) cat("Applied MAD filtering\n")
  }
  
  if (getOption("ternary.debug", FALSE)) cat("DEBUG: After statistical filtering, data dimensions:", dim(M), "\n")
  
  # Apply multivariate analysis filtering if requested
  if (getOption("ternary.debug", FALSE)) {
    cat("DEBUG: Multivariate analysis check:\n")
    cat("DEBUG: use_mahalanobis =", use_mahalanobis, "\n")
    cat("DEBUG: use_robust_mahalanobis =", use_robust_mahalanobis, "\n")
    cat("DEBUG: use_isolation_forest =", use_isolation_forest, "\n")

    cat("DEBUG: reference_data is.null =", is.null(reference_data), "\n")
    if (!is.null(reference_data)) {
      cat("DEBUG: reference_data dimensions:", dim(reference_data), "\n")
    }
    cat("DEBUG: Will enter multivariate section =", use_mahalanobis || use_robust_mahalanobis || use_isolation_forest, "\n")
    cat("DEBUG: Any multivariate method enabled =", use_mahalanobis || use_robust_mahalanobis || use_isolation_forest, "\n")
  }
  
  if (use_mahalanobis || use_robust_mahalanobis || use_isolation_forest) {
    # Determine reference dataset based on user selection
    # IMPORTANT: reference_data parameter must be provided by caller for dataset1/dataset2 modes
    # For self-reference mode, the function uses its own data (M)
    actual_reference_data <- NULL
    
    # Use mahalanobis_reference for all multivariate methods
    reference_mode <- mahalanobis_reference
    
    if (reference_mode == "self") {
      actual_reference_data <- M  # Self-reference
      if (!preview) debug_log("Using self-reference for multivariate analysis")
    } else if (reference_mode == "dataset1") {
      # Use the reference_data parameter provided by caller
      actual_reference_data <- reference_data
      if (!preview) debug_log("Using Dataset 1 as reference for multivariate analysis (reference_data: %d rows)", 
                              if (!is.null(reference_data)) nrow(reference_data) else 0)
    } else if (reference_mode == "dataset2") {
      actual_reference_data <- reference_data  # Dataset 2 reference
      if (!preview) debug_log("Using Dataset 2 as reference for multivariate analysis (reference_data: %d rows)", 
                              if (!is.null(reference_data)) nrow(reference_data) else 0)
    }
    
    # Skip if reference dataset is not available
    if (is.null(actual_reference_data)) {
      if (!preview) debug_log("Skipping multivariate analysis: No reference dataset provided")
      if (!preview) debug_log("Reference data status: mahalanobis_reference=%s, reference_data=%s", 
                              mahalanobis_reference, if (is.null(reference_data)) "NULL" else paste("dataframe with", nrow(reference_data), "rows"))
    } else {
      tryCatch({
        if (use_robust_mahalanobis) {
          mahal_result <- compute_robust_mahalanobis(M, actual_reference_data, keep_outliers = keep_outliers_robust, selected_columns = selected_columns)
          threshold_to_use <- mahal_result$threshold_95
          keep_indices <- if (keep_outliers_robust) {
            mahal_result$outlier_indices
          } else {
            which(mahal_result$distances <= threshold_to_use)
          }
        } else if (use_isolation_forest) {
          iso_result <- compute_isolation_forest(M, actual_reference_data, keep_outliers = keep_outliers_isolation, selected_columns = selected_columns)
          keep_indices <- if (keep_outliers_isolation) {
            iso_result$outlier_indices
          } else {
            !iso_result$outlier_indices
          }
        } else {
          if (getOption("ternary.debug", FALSE)) {
            cat("DEBUG: About to call compute_mahalanobis_distance:\n")
            cat("DEBUG: M dimensions:", dim(M), "\n")
            cat("DEBUG: actual_reference_data dimensions:", dim(actual_reference_data), "\n")
            cat("DEBUG: lambda =", lambda, ", omega =", omega, "\n")
            cat("DEBUG: keep_outliers_mahalanobis =", keep_outliers_mahalanobis, "\n")
            cat("DEBUG: custom_mdthresh =", if (is.null(custom_mdthresh)) "NULL" else custom_mdthresh, "\n")
            cat("DEBUG: selected_columns =", if (is.null(selected_columns)) "NULL" else paste(selected_columns, collapse = ", "), "\n")
            cat("DEBUG: mdthresh_mode =", mdthresh_mode, "\n")

          }
          
          mahal_result <- compute_mahalanobis_distance(M, actual_reference_data, lambda, omega, keep_outliers = keep_outliers_mahalanobis, custom_mdthresh = custom_mdthresh, selected_columns = selected_columns, mdthresh_mode = mdthresh_mode)
          
          if (getOption("ternary.debug", FALSE)) {
            cat("DEBUG: compute_mahalanobis_distance returned:\n")
            cat("DEBUG: MDthresh =", mahal_result$MDthresh, "\n")
            cat("DEBUG: outlier_indices length =", length(mahal_result$outlier_indices), "\n")
            cat("DEBUG: outlier_indices sum =", sum(mahal_result$outlier_indices), "\n")
            cat("DEBUG: threshold_method =", mahal_result$threshold_method, "\n")
          }
          
          # Use the new threshold formula
          threshold_to_use <- mahal_result$MDthresh
          keep_indices <- if (keep_outliers_mahalanobis) {
            mahal_result$outlier_indices
          } else {
            !mahal_result$outlier_indices
          }
          
          if (getOption("ternary.debug", FALSE)) {
            cat("DEBUG: keep_indices calculation:\n")
            cat("DEBUG: keep_outliers_mahalanobis =", keep_outliers_mahalanobis, "\n")
            cat("DEBUG: keep_indices length =", length(keep_indices), "\n")
            cat("DEBUG: keep_indices sum =", sum(keep_indices), "\n")
          }
        }
        
        # Apply the filtering
        common_cols <- if (use_robust_mahalanobis) mahal_result$common_cols else 
          if (use_isolation_forest) iso_result$common_cols else mahal_result$common_cols
        M_numeric <- as.matrix(M[, common_cols, drop=FALSE])
        original_indices <- which(complete.cases(M_numeric))[keep_indices]
        
        if (getOption("ternary.debug", FALSE)) {
          cat("DEBUG: Multivariate filtering details:\n")
          cat("DEBUG: common_cols:", paste(common_cols, collapse = ", "), "\n")
          cat("DEBUG: keep_indices length:", length(keep_indices), "\n")
          cat("DEBUG: keep_indices sum:", sum(keep_indices), "\n")
          cat("DEBUG: original_indices length:", length(original_indices), "\n")
          cat("DEBUG: M before filtering:", nrow(M), "rows\n")
        }
        
        M <- M[original_indices, , drop = FALSE]
        
        if (getOption("ternary.debug", FALSE)) {
          cat("DEBUG: M after filtering:", nrow(M), "rows\n")
        }
        
        if (!preview) {
          method_name <- if (use_robust_mahalanobis) "Robust Mahalanobis" else
            if (use_isolation_forest) "Isolation Forest" else "Mahalanobis"
          ref_name <- if (mahalanobis_reference == "self") "self-reference" else 
            if (mahalanobis_reference == "dataset1") "Dataset 1 reference" else "Dataset 2 reference"
          debug_log("After %s filtering (%s):", method_name, ref_name)
          debug_log("Outlier points remaining: %d", sum(keep_indices))
          debug_log("Columns used: %s", paste(common_cols, collapse = ", "))
        }
      }, error = function(e) {
        if (getOption("ternary.debug", FALSE)) {
          cat("DEBUG: Multivariate filtering ERROR:\n")
          cat("DEBUG: Error message:", e$message, "\n")
          cat("DEBUG: Error call:", toString(e$call), "\n")
          cat("DEBUG: Error occurred in:", e$call[[1]], "\n")
        }
        if (!preview) debug_log("Multivariate filtering failed: %s", e$message)
      })
    }
  }
  
  if (getOption("ternary.debug", FALSE)) {
    cat("DEBUG: After multivariate filtering, data dimensions:", dim(M), "\n")
    if (use_mahalanobis && !is.null(reference_data)) {
      cat("DEBUG: Multivariate analysis was applied. Original data should be filtered.\n")
      cat("DEBUG: Sample of filtered data (first 5 rows):\n")
      print(head(M, 5))
    }
  }
  
  # Always define file_base for plot title generation
  if (!is.null(xlsx_display_name)) {
    file_base <- gsub("\\.xlsx$", "", basename(xlsx_display_name))
  } else {
    file_base <- gsub("\\.xlsx$", "", basename(xlsx_file))
  }
  
  # Only create directories if NOT in preview mode and output_dir is specified
  if (!preview && !is.null(output_dir)) {
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    plot_folder_name <- paste0("charge", file_base)
    custom_folder <- file.path(output_dir, plot_folder_name)
    if (dir.exists(custom_folder)) {
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      custom_folder <- file.path(output_dir, paste0(plot_folder_name, "_", timestamp))
    }
    dir.create(custom_folder, recursive = TRUE, showWarnings = FALSE)
  } else if (preview) {
    # For preview mode, just set a dummy folder name
    custom_folder <- NULL
  } else {
    # Fallback to plots2 directory only if not preview and no output_dir
    plots_dir <- file.path(getwd(), "plots2")
    if (!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE)
  plot_folder_name <- paste0("charge", file_base)
  custom_folder <- file.path(plots_dir, plot_folder_name)
  if (dir.exists(custom_folder)) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    custom_folder <- file.path(plots_dir, paste0(plot_folder_name, "_", timestamp))
  }
  dir.create(custom_folder, recursive = TRUE, showWarnings = FALSE)
  }
  
  all_selected_elements <- c(element_A$col, element_B$col, element_C$col)
  needed_columns <- unique(c(all_selected_elements, 
                             if (!is.null(optional_param1)) optional_param1$col,
                             if (!is.null(optional_param2)) optional_param2$col))
  
  log_operation("Column validation", paste("Checking required columns:", paste(needed_columns, collapse = ", ")))
  
  if (!all(needed_columns %in% colnames(M))) {
    missing_cols <- setdiff(needed_columns, colnames(M))
    log_operation("Column validation", paste("ERROR: Missing columns:", paste(missing_cols, collapse = ", ")))
    stop("Error: One or more selected elements/parameters are missing in the dataset.\nAvailable columns: ", 
         paste(colnames(M), collapse = ", "))
  }
  
  log_operation("Column validation", "All required columns found")
  
  matrika <- M[, needed_columns]
  cat("DEBUG: Matrika dimensions after column selection:", dim(matrika), "\n")
  if (getOption("ternary.debug", FALSE) && use_mahalanobis && !is.null(reference_data)) {
    cat("DEBUG: Matrika created from filtered M. Sample data (first 5 rows):\n")
    print(head(matrika, 5))
  }
  log_operation("Data processing", paste("Selected columns. Matrix dimensions:", dim(matrika)[1], "rows x", dim(matrika)[2], "columns"))
  matrika[, all_selected_elements] <- lapply(matrika[, all_selected_elements, drop = FALSE], as.numeric)
  row_sums <- rowSums(matrika[, all_selected_elements, drop = FALSE], na.rm = TRUE)
  matrika <- matrika[row_sums > 0, , drop = FALSE]
  cat("DEBUG: Matrika dimensions after removing zero-sum rows:", dim(matrika), "\n")
  matrika <- na.omit(matrika)
  cat("DEBUG: Matrika dimensions after na.omit:", dim(matrika), "\n")
  log_operation("Data processing", paste("After removing NA values. Matrix dimensions:", dim(matrika)[1], "rows x", dim(matrika)[2], "columns"))
  
  matrika[] <- lapply(matrika, as.numeric)
  matrika <- as.matrix(matrika)
  
  log_operation("Ternary coordinates", "Generating ternary coordinates")
  
  # First set: normalized
  ternary_data <- data.frame(
    A = if (length(element_A$col) == 1) matrika[, element_A$col] else rowSums(matrika[, element_A$col, drop = FALSE], na.rm = TRUE),
    B = if (length(element_B$col) == 1) matrika[, element_B$col] else rowSums(matrika[, element_B$col, drop = FALSE], na.rm = TRUE),
    C = if (length(element_C$col) == 1) matrika[, element_C$col] else rowSums(matrika[, element_C$col, drop = FALSE], na.rm = TRUE)
  )
  Total <- ternary_data$A + ternary_data$B + ternary_data$C
  fA <- ternary_data$A / Total
  fB <- ternary_data$B / Total
  fC <- 1 - (fA + fB)
  ternary_points1 <- data.frame(A = fA, B = fB, C = fC)
  ternary_data <- ternary_data / rowSums(ternary_data)
  if (getOption("ternary.debug", FALSE)) {
  cat("DEBUG: Ternary points dimensions:", dim(ternary_points1), "\n")
  cat("DEBUG: First few ternary points:\n")
  print(head(ternary_points1))
    if (use_mahalanobis && !is.null(reference_data)) {
      cat("DEBUG: Ternary points created from filtered data. Count should match filtered M.\n")
      cat("DEBUG: Original M rows:", nrow(M), "| Ternary points:", nrow(ternary_points1), "\n")
    }
  }
  if (nrow(ternary_data) == 0) stop("Error: No valid data left after filtering.")
  
  log_operation("Ternary coordinates", paste("Generated ternary coordinates for", nrow(ternary_points1), "points"))
  
  labels_A <- label_with_element(element_A)
  labels_B <- label_with_element(element_B)
  labels_C <- label_with_element(element_C)
  clean_labels_A <- gsub("\\.\\(Wt%\\)", "", labels_A)
  clean_labels_B <- gsub("\\.\\(Wt%\\)", "", labels_B)
  clean_labels_C <- gsub("\\.\\(Wt%\\)", "", labels_C)
  opt1_label <- if (!is.null(optional_param1)) label_with_filter(optional_param1) else NULL
  opt2_label <- if (!is.null(optional_param2)) label_with_filter(optional_param2) else NULL

  # Build comprehensive plot title with all applied methods
  log_operation("Plot title", "Generating comprehensive plot title")
  title_parts <- c(paste0("Ternary Plot of ", clean_labels_A, ", ", clean_labels_B, ", ", clean_labels_C))
  
  # Add optional parameters
  if (!is.null(opt1_label)) title_parts <- c(title_parts, opt1_label)
  if (!is.null(opt2_label)) title_parts <- c(title_parts, opt2_label)
  
  # Add statistical filtering methods
  stat_methods <- c()
  if (use_iqr_filter) stat_methods <- c(stat_methods, paste0("IQR", if (keep_outliers_iqr) "(outliers only)" else "(filtered)"))
  if (use_zscore_filter) stat_methods <- c(stat_methods, paste0("Z-score", if (keep_outliers_zscore) "(outliers only)" else "(filtered)"))
  if (use_mad_filter) stat_methods <- c(stat_methods, paste0("MAD", if (keep_outliers_mad) "(outliers only)" else "(filtered)"))
  if (length(stat_methods) > 0) title_parts <- c(title_parts, paste("Stats:", paste(stat_methods, collapse = "+")))
  
  # Add multivariate analysis methods
  mv_methods <- c()
  if (use_mahalanobis && !is.null(reference_data)) {
    if (use_robust_mahalanobis) {
      mv_methods <- c(mv_methods, paste0("RobustMD", if (keep_outliers_robust) "(outliers only)" else "(filtered)"))
    } else if (use_isolation_forest) {
      mv_methods <- c(mv_methods, paste0("IsoForest", if (keep_outliers_isolation) "(outliers only)" else "(filtered)"))
    } else {
      mv_methods <- c(mv_methods, paste0("Mahalanobis", if (keep_outliers_mahalanobis) "(outliers only)" else "(filtered)"))
    }
  }
  
  if (length(mv_methods) > 0) title_parts <- c(title_parts, paste("Multivariate:", paste(mv_methods, collapse = "+")))
  
  # Add plot options info
  plot_options <- c()
          # Inclusion area feature removed
  if (length(plot_options) > 0) title_parts <- c(title_parts, paste("Plot:", paste(plot_options, collapse = "+")))
  
  # Add individual filter info if any applied
  filter_info <- c()
  if (!is.null(individual_filters_A) && length(individual_filters_A) > 0) {
    active_filters_A <- individual_filters_A[!sapply(individual_filters_A, is.null) & nzchar(individual_filters_A)]
    if (length(active_filters_A) > 0) {
      filter_info <- c(filter_info, paste0("A:", length(active_filters_A), "f"))
    }
  }
  if (!is.null(individual_filters_B) && length(individual_filters_B) > 0) {
    active_filters_B <- individual_filters_B[!sapply(individual_filters_B, is.null) & nzchar(individual_filters_B)]
    if (length(active_filters_B) > 0) {
      filter_info <- c(filter_info, paste0("B:", length(active_filters_B), "f"))
    }
  }
  if (!is.null(individual_filters_C) && length(individual_filters_C) > 0) {
    active_filters_C <- individual_filters_C[!sapply(individual_filters_C, is.null) & nzchar(individual_filters_C)]
    if (length(active_filters_C) > 0) {
      filter_info <- c(filter_info, paste0("C:", length(active_filters_C), "f"))
    }
  }
  if (length(filter_info) > 0) title_parts <- c(title_parts, paste("Filters:", paste(filter_info, collapse = ",")))
  
  title_parts <- c(title_parts, paste("charge", file_base))
  
  # Intelligent title splitting for better readability
  plot_title <- preview_title_layout(title_parts)
  
  log_operation("Plot title", paste("Generated title with", length(title_parts), "parts"))
  log_operation("Plot title", paste("Final title:", substr(plot_title, 1, 100), "..."))
  
  # Only generate file path if not in preview mode
  if (!preview) {
  file_name <- gsub("_+", "_", gsub("[^A-Za-z0-9]+", "_", plot_title))
  file_name <- gsub("^_+|_+$", "", file_name) 
  file_name <- paste0(file_name, ".", output_format)
  file_path <- file.path(custom_folder, file_name)
  file_path <- normalizePath(file_path, winslash = "/", mustWork = FALSE)
  
  log_operation("File saving", paste("Preparing to save plot as:", file_name))
  log_operation("File saving", paste("Full path:", file_path))
  
    # Calculate optimal plot dimensions based on title length
    plot_dims <- calculate_plot_dimensions(title_parts)
    
    log_operation("File saving", paste("Opening", output_format, "device"))
    if (output_format == "png") {
      png(file_path, width = plot_dims$width, height = plot_dims$height, res = 200)
    } else if (output_format == "jpeg") {
      jpeg(file_path, width = plot_dims$width, height = plot_dims$height, res = 200, quality = 95)
    } else if (output_format == "pdf") {
      pdf(file_path, width = plot_dims$width/100, height = plot_dims$height/100)
    } else if (output_format == "tiff") {
      tiff(file_path, width = plot_dims$width, height = plot_dims$height, res = 200, compression = "lzw")
    }
  }
  
  # Optional param 1: point size or point type
  if (!is.null(optional_param1)) {
    param1_values <- matrika[, optional_param1$col]
    
    if (optional_param1_representation == "point_size") {
      # Point size representation
      minPointSize <- MIN_POINT_SIZE
      maxSize <- MAX_POINT_SIZE
      pointSize <- param1_values * (maxSize - minPointSize) / max(param1_values, na.rm = TRUE) + minPointSize
      pointType <- 16  # Default circle
    } else if (optional_param1_representation == "point_type") {
      # Point type representation
      pointSize <- 0.7  # Fixed size
      # Create bins for point types
      param1_breaks <- quantile(param1_values, probs = seq(0, 1, length.out = 6), na.rm = TRUE)
      param1_breaks <- unique(param1_breaks)
      
      if (length(param1_breaks) < 2) {
        param1_bins <- factor(rep(1, length(param1_values)), labels = "All")
        pointType <- rep(16, length(param1_values))  # All circles
      } else {
        param1_bins <- cut(param1_values, breaks = param1_breaks, include.lowest = TRUE)
        # Assign different point types based on bins
        point_types <- c(16, 17, 15, 18, 19)  # circle, triangle, square, diamond, filled diamond
        pointType <- point_types[as.numeric(param1_bins)]
      }
    }
  } else {
    pointSize <- MIN_POINT_SIZE
    pointType <- 16
  }
  
  # Optional param 2: color (default: "grey")
  if (!is.null(optional_param2)) {
    param2_values <- matrika[, optional_param2$col]
    param2_breaks <- quantile(param2_values, probs = seq(0, 1, length.out = 6), na.rm = TRUE)
    param2_breaks <- unique(param2_breaks) # <-- make breaks unique
    
    if (length(param2_breaks) < 2) {
      # All values identical or not enough to make bins; fallback coloring
      param2_bins <- factor(rep(1, length(param2_values)), labels = "All")
      n_colors <- 1
    } else {
      param2_bins <- cut(param2_values, breaks = param2_breaks, include.lowest = TRUE)
      n_colors <- length(levels(param2_bins))
    }
    
    if (color_palette == "blue") {
      param2_colors <- colorRampPalette(c("#357ABD", "#002147"))(n_colors)
    } else if (color_palette == "red") {
      param2_colors <- colorRampPalette(c("#FF6666", "#990000"))(n_colors)
    } else if (color_palette == "viridis") {
      if (!requireNamespace("viridisLite", quietly = TRUE)) {
        warning("viridisLite package not available. Using rainbow palette instead.")
        param2_colors <- rainbow(n_colors)
      } else {
      param2_colors <- viridisLite::viridis(n_colors)
      }
    } else if (color_palette == "rainbow") {
      param2_colors <- rainbow(n_colors)
    } else {
      param2_colors <- rep("black", n_colors)
    }
    pointCol <- param2_colors[as.numeric(param2_bins)]
    
  } else {
    pointCol <- "black"
  }
  
  if (getOption("ternary.debug", FALSE)) {
  cat("DEBUG: Point size range:", range(pointSize), "\n")
  cat("DEBUG: Point type range:", range(pointType), "\n")
  cat("DEBUG: Point color unique values:", unique(pointCol), "\n")
  }
  
  # Simple plotting approach - plot all points
  if (getOption("ternary.debug", FALSE)) cat("DEBUG: About to plot all points, count:", nrow(ternary_points1), "\n")
  log_operation("Plotting", paste("Starting to plot", nrow(ternary_points1), "\n"))
  
  # Set outer margins to prevent clipping of multi-line titles and notes
  # Top margin for titles, bottom margin for plot notes
  op <- par(oma = c(2, 0, 3, 0))
  on.exit(par(op))
  
  Ternary::TernaryPlot(
    atip = clean_labels_A, btip = clean_labels_B, ctip = clean_labels_C,
    alab = paste(clean_labels_A, "→"), blab = paste(clean_labels_B, "→"), clab = paste("←", clean_labels_C),
    col = "white", 
    grid.lines = 5, 
    grid.lty = "dotted", 
    grid.minor.lines = 1, 
    grid.minor.lty = "dotted"
  )
  
  # Simple plotting - just plot all points
  if (getOption("ternary.debug", FALSE)) cat("DEBUG: Plotting all points with TernaryPoints\n")
  Ternary::TernaryPoints(ternary_points1, cex = pointSize, col = pointCol, pch = pointType)
  if (getOption("ternary.debug", FALSE)) cat("DEBUG: All points plotted successfully\n")
  
  # Enhanced title plotting for multi-line titles
  if (grepl("\n", plot_title)) {
    # Multi-line title: adjust line position and size
    title(main = plot_title, cex.main = 0.7, line = 0.8)
  } else {
    # Single-line title: standard formatting
  title(main = plot_title, cex.main = 0.8, line = 0.5)
  }
  
  # Add legends for optional parameters if selected
  if (!is.null(optional_param1)) {
    # Legend for optional parameter 1 (point size) - top right
    if (length(optional_param1$col) == 1 && optional_param1_representation == "point_size") {
      if (requireNamespace("PlotTools", quietly = TRUE)) {
        PlotTools::SizeLegend(
          "topright",
          width = c(MIN_POINT_SIZE, MAX_POINT_SIZE),
          lend = "round",
          legend = paste(
            signif(seq(max(param1_values, na.rm = TRUE), min(param1_values, na.rm = TRUE), length.out = 5), digits = 3)
          ),
          title = paste(optional_param1$col, collapse = "+"),
          bty = "n",
          cex = 0.7
        )
      } else {
        # Fallback to regular legend
        size_range <- seq(min(param1_values, na.rm = TRUE), max(param1_values, na.rm = TRUE), length.out = 5)
        legend_sizes <- size_range * (MAX_POINT_SIZE - MIN_POINT_SIZE) / max(param1_values, na.rm = TRUE) + MIN_POINT_SIZE
        
        legend("topright", 
               title = paste(optional_param1$col, collapse = "+"),
               legend = paste("Size:", round(size_range, 2)),
               pt.cex = legend_sizes,
               pch = 16,
               cex = 0.7)
      }
    } else if (length(optional_param1$col) == 1 && optional_param1_representation == "point_type") {
      # Point type representation - show different point types
      if (exists("param1_bins") && length(levels(param1_bins)) > 1) {
        point_types <- c(16, 17, 15, 18, 19)  # circle, triangle, square, diamond, filled diamond
        legend("topright", 
               title = paste(optional_param1$col, collapse = "+"),
               legend = levels(param1_bins),
               pch = point_types[seq_len(length(levels(param1_bins)))],
               pt.cex = 1.0,
               cex = 0.7)
      } else {
        legend("topright", 
               title = paste(optional_param1$col, collapse = "+"),
               legend = "All",
               pch = 16,
               pt.cex = 1.0,
               cex = 0.7)
      }
    } else if (length(optional_param1$col) > 1) {
      # Multiple columns - show combined legend
      legend("topright", 
             title = paste(optional_param1$col, collapse = "+"),
             legend = "Combined",
             pt.cex = 1.5,
             pch = 16,
             cex = 0.7)
    }
  }
  
  if (!is.null(optional_param2)) {
    # Legend for optional parameter 2 (color) - top left
    if (length(optional_param2$col) == 1) {
      # Single column - show color legend
      legend("topleft", 
             legend = levels(param2_bins), 
             col = param2_colors, 
             pch = 16, 
             title = paste(paste(optional_param2$col, collapse = "+")),
             cex = 0.7,
             y.intersp = 1.2)
    } else {
      # Multiple columns - show combined legend
      legend("topleft", 
             title = paste(optional_param2$col, collapse = "+"),
             legend = "Combined",
             fill = "blue",
             cex = 0.7)
    }
  }
  
  # Add plot notes if requested
  if (include_plot_notes) {
    summary_text <- generate_plot_summary(element_A, element_B, element_C, optional_param1, optional_param2,
                                        use_mahalanobis, use_robust_mahalanobis, use_isolation_forest,
                                        use_iqr_filter, use_zscore_filter, use_mad_filter,
                                        lambda, omega, custom_mdthresh,
                                        keep_outliers_mahalanobis, keep_outliers_robust,
                                        keep_outliers_isolation, keep_outliers_iqr,
                                        keep_outliers_zscore, keep_outliers_mad,
                                        individual_filters_A, individual_filters_B, individual_filters_C)
    # Enhanced plot notes positioning for better readability
    # Split long summary text into multiple lines if needed
    summary_lines <- strsplit(summary_text, "\n")[[1]]
    
    # Calculate optimal positioning based on text length and content
    if (length(summary_lines) <= 6) {
      # Short summary: use standard positioning
    mtext(summary_text, side = 1, line = -2, outer = TRUE, cex = 0.6, adj = 0)
    } else if (length(summary_lines) <= 12) {
      # Medium summary: use two lines
      mtext(summary_text, side = 1, line = -2, outer = TRUE, cex = 0.55, adj = 0)
    } else {
      # Long summary: split into multiple text blocks for better readability
      # Find natural break points
      main_elements_end <- which(summary_lines == "Optional Parameter 1 (Point Size):")[1]
      if (is.na(main_elements_end)) {
        main_elements_end <- which(summary_lines == "Statistical and Multivariate Analysis:")[1]
      }
      if (is.na(main_elements_end)) {
        main_elements_end <- which(summary_lines == "Data Filtering Summary:")[1]
      }
      if (is.na(main_elements_end)) {
        main_elements_end <- ceiling(length(summary_lines) / 2)
      }
      
      # Main elements section
      main_text <- paste(summary_lines[1:main_elements_end], collapse = "\n")
      mtext(main_text, side = 1, line = -3, outer = TRUE, cex = 0.5, adj = 0)
      
      # Remaining sections
      if (main_elements_end < length(summary_lines)) {
        remaining_text <- paste(summary_lines[(main_elements_end + 1):length(summary_lines)], collapse = "\n")
        mtext(remaining_text, side = 1, line = -1, outer = TRUE, cex = 0.5, adj = 0)
      }
    }
  }
  
  log_operation("Plotting", "Plot completed successfully")
  
  if (!preview) {
    log_operation("File saving", "Closing plot device")
    dev.off()
    log_operation("File saving", paste("Plot saved successfully to:", file_path))
    return(file_path)
  } else {
      # Final summary for debugging
  if (getOption("ternary.debug", FALSE)) {
    cat("DEBUG: Final function summary:\n")
    cat("DEBUG: Original data loaded:", nrow(M), "rows\n")
    cat("DEBUG: Final M dimensions:", dim(M), "\n")
    cat("DEBUG: Ternary points created:", nrow(ternary_points1), "\n")
    cat("DEBUG: Multivariate analysis applied:", use_mahalanobis, "\n")
    if (use_mahalanobis) {
      cat("DEBUG: Reference data used:", !is.null(reference_data), "\n")
      if (!is.null(reference_data)) {
        cat("DEBUG: Reference data dimensions:", dim(reference_data), "\n")
      }
    }
  }
  
    # For preview mode, just return NULL since no file was saved
    return(NULL)
  }
  
  log_operation("Function completion", "general_ternary_plot function completed successfully")
}

# ---- Shiny App ----
run_ternary_gui <- function(xlsx_file = NULL, working_dir = default_working_dir, output_dir = default_output_dir) {
  if (!is.null(xlsx_file)) {
    M <- openxlsx::read.xlsx(xlsx_file, sheet = 1)
    col_names <- colnames(M)
  } else {
    M <- NULL
    col_names <- character(0)
  }
  
  ui <- fluidPage(
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
              column(6, h3("Dataset 2 (Reference)"))
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
                  selectInput("element_A1", "Element A (multiple allowed):", choices = col_names, multiple = TRUE),
                  uiOutput("dynamic_filters_A1"),
                  helpText("Note: Each selected element can have its own filter condition (logical AND between elements)"),
                  helpText("Example: Fe > 10, Al > 5, Si > 0 (each element gets its own threshold)")
                ),
                div(style = "border: 2px solid #dc3545; padding: 10px; border-radius: 5px; margin: 10px 0;",
                  h4(style = "color: #dc3545; margin-top: 0;", "Element B (Required)"),
                  selectInput("element_B1", "Element B (multiple allowed):", choices = col_names, multiple = TRUE),
                  uiOutput("dynamic_filters_B1")
                ),
                div(style = "border: 2px solid #dc3545; padding: 10px; border-radius: 5px; margin: 10px 0;",
                  h4(style = "color: #dc3545; margin-top: 0;", "Element C (Required)"),
                  selectInput("element_C1", "Element C (multiple allowed):", choices = col_names, multiple = TRUE),
                  uiOutput("dynamic_filters_C1")
                ),
                selectInput("optional_param1_1", "Optional Param 1:", choices = c("", col_names)),
                selectInput("optional_param1_representation1", "Optional Param 1 Representation:",
                  choices = c("Point Size" = "point_size", "Point Type" = "point_type"),
                  selected = "point_size"),
                textInput("filter_op1_1", "Filter for Optional Param 1", ""),
                selectInput("optional_param2_1", "Optional Param 2:", choices = c("", col_names)),
                textInput("filter_op1_2", "Filter for Optional Param 2", ""),
                selectInput("color_palette1", "Color Palette for Optional Param 2:",
                  choices = c("Blue" = "blue", "Red" = "red", "Viridis" = "viridis", "Rainbow" = "rainbow"),
                  selected = "blue")
              ),
              column(6,
                div(style = "border: 2px solid #dc3545; padding: 10px; border-radius: 5px; margin: 10px 0;",
                  h4(style = "color: #dc3545; margin-top: 0;", "Element A (Required)"),
                  selectInput("element_A2", "Element A (multiple allowed):", choices = col_names, multiple = TRUE),
                  uiOutput("dynamic_filters_A2")
                ),
                div(style = "border: 2px solid #dc3545; padding: 10px; border-radius: 5px; margin: 10px 0;",
                  h4(style = "color: #dc3545; margin-top: 0;", "Element B (Required)"),
                  selectInput("element_B2", "Element B (multiple allowed):", choices = col_names, multiple = TRUE),
                  uiOutput("dynamic_filters_B2")
                ),
                div(style = "border: 2px solid #dc3545; padding: 10px; border-radius: 5px; margin: 10px 0;",
                  h4(style = "color: #dc3545; margin-top: 0;", "Element C (Required)"),
                  selectInput("element_C2", "Element C (multiple allowed):", choices = col_names, multiple = TRUE),
                  uiOutput("dynamic_filters_C2")
                ),
                selectInput("optional_param1_2", "Optional Param 1:", choices = c("", col_names)),
                selectInput("optional_param1_representation2", "Optional Param 1 Representation:",
                  choices = c("Point Size" = "point_size", "Point Type" = "point_type"),
                  selected = "point_size"),
                textInput("filter_op1_2", "Filter for Optional Param 1", ""),
                selectInput("optional_param2_2", "Optional Param 2:", choices = c("", col_names)),
                textInput("filter_op2_2", "Filter for Optional Param 2", ""),
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
                  checkboxInput("use_mahalanobis", "Use Mahalanobis Distance", value = FALSE),
                  checkboxInput("use_robust_mahalanobis", "Use Robust Mahalanobis", value = FALSE),
                  checkboxInput("use_isolation_forest", "Use Isolation Forest", value = FALSE)
                )
              ),
              column(4,
                div(style = "border: 2px solid #28a745; padding: 15px; border-radius: 8px; margin: 10px 0; background-color: #f8f9fa;",
                  h4(style = "color: #28a745; margin-top: 0;", "📊 Statistical Filtering"),
                  checkboxInput("use_iqr_filter", "Use IQR Filtering", value = FALSE),
                  checkboxInput("use_zscore_filter", "Use Z-Score Filtering", value = FALSE),
                  checkboxInput("use_mad_filter", "Use MAD Filtering", value = FALSE)
                )
              ),
              column(4,
                div(style = "border: 2px solid #ffc107; padding: 15px; border-radius: 8px; margin: 10px 0; background-color: #f8f9fa;",
                  h4(style = "color: #ffc107; margin-top: 0;", "🎨 Output Options"),
                  selectInput("output_format", "Output Format:",
                    choices = c("PNG" = "png", "JPEG" = "jpeg", "PDF" = "pdf", "TIFF" = "tiff"),
                    selected = "png"),
                  checkboxInput("include_plot_notes", "Include plot notes", value = TRUE)
                )
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
            uiOutput("comparison_buttons"),
            uiOutput("comparison_output")
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
                      choices = c("Dataset 1" = "dataset1", "Dataset 2" = "dataset2", "Compare Both" = "both"),
                      selected = "dataset1", inline = TRUE),
                    selectizeInput("scatter_columns", "Select Columns for Analysis", choices = NULL, multiple = TRUE),
                    selectizeInput("scatter_x_col", "X-axis Column (optional)", choices = NULL, multiple = FALSE),
                    selectizeInput("scatter_y_col", "Y-axis Column (optional)", choices = NULL, multiple = FALSE),
                    numericInput("scatter_point_size", "Point Size", value = 0.8, min = 0.1, max = 3, step = 0.1)
                  ),
                  column(4,
                    h5("Column Colors"),
                    uiOutput("scatter_color_inputs"),
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
                      choices = c("Dataset 1" = "dataset1", "Dataset 2" = "dataset2", "Compare Both" = "both"),
                      selected = "dataset1", inline = TRUE),
                    selectizeInput("histogram_columns", "Select Columns for Analysis", choices = NULL, multiple = TRUE),
                    selectInput("histogram_breaks", "Number of Breaks", 
                      choices = c("Sturges" = "Sturges", "Scott" = "Scott", "FD" = "FD", "Manual" = "manual"),
                      selected = "Sturges"),
                    conditionalPanel(
                      condition = "input.histogram_breaks == 'manual'",
                      numericInput("histogram_manual_breaks", "Manual Number of Breaks", 
                        value = 20, min = 5, max = 100, step = 1)
                    ),
                    radioButtons("histogram_type", "Data Representation", 
                      choices = c("Frequency" = "frequency", "Density" = "density", "Proportion" = "proportion"),
                      selected = "frequency", inline = TRUE),
                    numericInput("histogram_alpha", "Transparency (0-1)", 
                      value = 0.7, min = 0, max = 1, step = 0.1)
                  ),
                  column(4,
                    h5("Column Colors"),
                    uiOutput("histogram_color_inputs"),
                    br(),
                    h5("Save Options"),
                    selectInput("histogram_output_format", "Output Format", 
                      choices = c("PNG" = "png", "JPEG" = "jpeg", "PDF" = "pdf", "TIFF" = "tiff"),
                      selected = "png"),
                    textInput("histogram_filename", "Filename (without extension)", 
                      value = "histogram_plot", placeholder = "Enter filename"),
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
                      choices = c("Dataset 1" = "dataset1", "Dataset 2" = "dataset2", "Compare Both" = "both"),
                      selected = "dataset1", inline = TRUE),
                    selectizeInput("boxplot_columns", "Select Columns for Analysis", choices = NULL, multiple = TRUE),
                    checkboxInput("boxplot_horizontal", "Horizontal Box Plot", value = FALSE),
                    numericInput("boxplot_width", "Box Width", value = 0.8, min = 0.1, max = 2, step = 0.1),
                    checkboxInput("boxplot_notch", "Notched Boxes", value = FALSE),
                    checkboxInput("boxplot_outliers", "Show Outliers", value = TRUE)
                  ),
                  column(4,
                    h5("Column Colors"),
                    uiOutput("boxplot_color_inputs"),
                    br(),
                    h5("Save Options"),
                    selectInput("boxplot_output_format", "Output Format", 
                      choices = c("PNG" = "png", "JPEG" = "jpeg", "PDF" = "pdf", "TIFF" = "tiff"),
                      selected = "png"),
                    textInput("boxplot_filename", "Filename (without extension)", 
                      value = "boxplot_plot", placeholder = "Enter filename"),
                    actionButton("save_boxplot", "Save Box Plot", class = "btn-success")
                  ),
                  column(4,
                    actionButton("create_boxplot", "Create Box Plot", class = "btn-primary"),
                    plotOutput("boxplot_plot_output", height = "300px"),
                    verbatimTextOutput("boxplot_filename_suggestion")
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
                tags$li("Filtered Data: Export data after applying all active filters and multivariate analysis"),
                tags$li("Statistical Results: Basic dataset summaries, correlations, and outlier analysis summaries"),
                tags$li("Plots: Export any plots created in the Multiple Plot Types tab"),
                tags$li("Project Export: Complete project summary with all configurations and results"),
                tags$li("All exports are organized in timestamped folders for easy management")
              )
            ),
            fluidRow(
              column(6,
                h4("Export Options"),
                
                # Filtered data export
                h5("Filtered Data"),
                checkboxInput("export_filtered_data", "Export Filtered Data", value = TRUE),
                helpText("Exports data after applying all active filters (element filters, optional parameters, statistical filters, multivariate analysis)"),
                selectInput("export_format", "Export Format:",
                  choices = c("Excel (.xlsx)" = "xlsx", 
                    "CSV (.csv)" = "csv",
                    "R Data (.rds)" = "rds"),
                  selected = "xlsx"),
                
                # Statistics export
                h5("Statistical Results"),
                checkboxInput("export_stats", "Export Statistics", value = TRUE),
                checkboxInput("export_correlations", "Export Correlations", value = TRUE),
                checkboxInput("export_outliers", "Export Outlier Analysis", value = TRUE),
                
                # Plot export
                h5("Plots"),
                checkboxInput("export_plots", "Export Plots", value = TRUE),
                selectInput("plot_export_format", "Plot Format:",
                  choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg", "HTML (Interactive)" = "html"),
                  selected = "png"),
                
                # Project export
                h5("Project Export"),
                checkboxInput("export_project", "Export Complete Project", value = TRUE),
                helpText("Includes all data, results, and configurations"),
                
                actionButton("export_all", "Export All Selected", class = "btn-success btn-lg"),
                actionButton("export_selected", "Export Selected Items", class = "btn-primary")
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
            shinyDirButton("working_dir", "Select Working Directory", "Please select a working directory"),
            verbatimTextOutput("working_dir_text")
          ),
          column(6,
            shinyDirButton("output_dir", "Select Output Directory", "Please select an output directory"),
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
    
    # Project Management Section
    fluidRow(
      column(12,
        h3("Project Management"),
        fluidRow(
          column(4,
            textInput("project_name", "Project Name:", placeholder = "Enter project name"),
            helpText("Enter a descriptive name for your analysis project")
          ),
          column(4,
            actionButton("save_project", "Save Project", class = "btn-success"),
            actionButton("load_project", "Load Project", class = "btn-info"),
            helpText("Save/load analysis configurations and results")
          ),
          column(4,
            verbatimTextOutput("project_status"),
            helpText("Current project status and last saved")
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
  )  # Close fluidPage
  
  # Server function would go here
  server <- function(input, output, session) {
    # Server logic would be implemented here
    # This is a placeholder for the server functionality
  }
  
 server <- function(input, output, session) {
    
    # Initialize cache cleanup on startup
    clear_expired_cache()
    
    # Periodic cache cleanup (every 5 minutes)
    cache_cleanup_timer <- reactiveTimer(300000)  # 5 minutes in milliseconds
    observe({
      cache_cleanup_timer()
      clear_expired_cache()
    })
    

    
    # Reactive values for working and output directories
    working_dir <- reactiveVal(character(0))
    output_dir <- reactiveVal(character(0))
    
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
      export_history = NULL
    )
    
    # Add error handling and user feedback
    show_message <- function(message, type = "info") {
      session$sendCustomMessage("showMessage", list(
        message = message,
        type = type
      ))
    }
    
    # ---- Multiple Plot Types Functions ----
    
    # ---- Multiple Ternary Creator Helper Functions ----
    # REFACTORED: These functions eliminate code duplication between preview and save operations
    # All filter collection logic is now centralized in reusable functions
    # Function to collect individual element filters for multiple ternary creator
    collect_individual_filters <- function(elements, element_type, input) {
      filters <- list()
      if (length(elements) > 0) {
        for (element in elements) {
          input_id <- paste0("multiple_filter_", element_type, "_", gsub("[^A-Za-z0-9]", "_", element))
          filter_value <- input[[input_id]]
          if (!is.null(filter_value) && !is.na(filter_value) && nchar(trimws(as.character(filter_value))) > 0) {
            filters[[element]] <- filter_value
          }
        }
      }
      return(filters)
    }
    
    # Function to collect optional parameter filters for multiple ternary creator
    collect_optional_param_filters <- function(elements, param_type, input) {
      filters <- list()
      if (length(elements) > 0) {
        for (element in elements) {
          input_id <- paste0("multiple_filter_", param_type, "_", gsub("[^A-Za-z0-9]", "_", element))
          filter_value <- input[[input_id]]
          if (!is.null(filter_value) && !is.na(filter_value) && nchar(trimws(as.character(filter_value))) > 0) {
            filters[[element]] <- filter_value
          }
        }
      }
      return(filters)
    }
    
    # Function to collect all filters for multiple ternary creator
    collect_all_multiple_filters <- function(input) {
      list(
        individual_filters_A = collect_individual_filters(input$multiple_element_A, "A", input),
        individual_filters_B = collect_individual_filters(input$multiple_element_B, "B", input),
        individual_filters_C = collect_individual_filters(input$multiple_element_C, "C", input),
        optional_param1_filters = collect_optional_param_filters(input$multiple_optional_param1, "op1", input),
        optional_param2_filters = collect_optional_param_filters(input$multiple_optional_param2, "op2", input)
      )
    }
    
    # Function to print debug information for multiple ternary creator
    print_multiple_debug_info <- function(input, filters, operation = "preview") {
      debug_log("DEBUG %s: Selected elements A: %s", toupper(operation), paste(input$multiple_element_A, collapse = ", "))
      debug_log("DEBUG %s: Selected elements B: %s", toupper(operation), paste(input$multiple_element_B, collapse = ", "))
      debug_log("DEBUG %s: Selected elements C: %s", toupper(operation), paste(input$multiple_element_C, collapse = ", "))
      debug_log("DEBUG %s: Selected optional param1: %s", toupper(operation), paste(input$multiple_optional_param1, collapse = ", "))
      debug_log("DEBUG %s: Selected optional param2: %s", toupper(operation), paste(input$multiple_optional_param2, collapse = ", "))
      
      debug_log("DEBUG %s: Collected filters A: %s", toupper(operation), 
                if(length(filters$individual_filters_A) > 0) paste(names(filters$individual_filters_A), "=", unlist(filters$individual_filters_A), collapse = ", ") else "none")
      debug_log("DEBUG %s: Collected filters B: %s", toupper(operation), 
                if(length(filters$individual_filters_A) > 0) paste(names(filters$individual_filters_B), "=", unlist(filters$individual_filters_B), collapse = ", ") else "none")
      debug_log("DEBUG %s: Collected filters C: %s", toupper(operation), 
                if(length(filters$individual_filters_C) > 0) paste(names(filters$individual_filters_C), "=", unlist(filters$individual_filters_C), collapse = ", ") else "none")
      debug_log("DEBUG %s: Collected optional param1 filters: %s", toupper(operation), 
                if(length(filters$optional_param1_filters) > 0) paste(names(filters$optional_param1_filters), "=", unlist(filters$optional_param1_filters), collapse = ", ") else "none")
      debug_log("DEBUG %s: Collected optional param2 filters: %s", toupper(operation), 
                if(length(filters$optional_param2_filters) > 0) paste(names(filters$optional_param2_filters), "=", unlist(filters$optional_param2_filters), collapse = ", ") else "none")
    }
    
    # ---- Unified Multivariate Analysis Validation System ----
    # Centralized function to validate data before multivariate analysis
    validate_multivariate_data <- function(data1, data2, selected_columns = NULL, method = "general", min_obs_ratio = 2) {
      # method: "general", "MCD", "isolation_forest"
      # min_obs_ratio: minimum observations per variable (default 2 for general, higher for robust methods)
      
      # Validate inputs
      if (is.null(data1) || is.null(data2)) {
        stop("Both datasets must be provided for multivariate analysis")
      }
      
      if (!is.data.frame(data1) || !is.data.frame(data2)) {
        stop("Both datasets must be data frames")
      }
      
      # Determine columns to use
      if (!is.null(selected_columns) && length(selected_columns) > 0) {
        # Use user-selected columns
        if (!is.character(selected_columns)) {
          stop("Selected columns must be a character vector")
        }
        
        available_cols1 <- colnames(data1)[colnames(data1) %in% selected_columns]
        available_cols2 <- colnames(data2)[colnames(data2) %in% selected_columns]
        common_cols <- intersect(available_cols1, available_cols2)
        
        # Check if we have enough selected columns
        if (length(common_cols) < 2) {
          missing_cols <- setdiff(selected_columns, union(available_cols1, available_cols2))
          stop(sprintf(
            "Insufficient common columns for %s analysis. Selected: %d, Available in both datasets: %d. Missing: %s",
            method, length(selected_columns), length(common_cols), 
            ifelse(length(missing_cols) > 0, paste(missing_cols, collapse = ", "), "none")
          ))
        }
      } else {
        # Use all numeric columns
        numeric_cols1 <- sapply(data1, is.numeric)
        numeric_cols2 <- sapply(data2, is.numeric)
        common_cols <- intersect(colnames(data1)[numeric_cols1], colnames(data2)[numeric_cols2])
      }
      
      # CRITICAL CHECK 1: Minimum columns
      if (length(common_cols) < 2) {
        stop(sprintf(
          "Need at least 2 common numeric columns for %s analysis. Found: %d columns",
          method, length(common_cols)
        ))
      }
      
      # Extract numeric data
      data1_numeric <- as.matrix(data1[, common_cols, drop = FALSE])
      data2_numeric <- as.matrix(data2[, common_cols, drop = FALSE])
      
      # Remove rows with NA values
      data1_clean <- data1_numeric[complete.cases(data1_numeric), , drop = FALSE]
      data2_clean <- data2_numeric[complete.cases(data2_numeric), , drop = FALSE]
      
      # CRITICAL CHECK 2: Complete cases
      if (nrow(data1_clean) == 0 || nrow(data2_clean) == 0) {
        stop(sprintf(
          "No complete cases found. Dataset 1: %d complete rows, Dataset 2: %d complete rows",
          nrow(data1_clean), nrow(data2_clean)
        ))
      }
      
      # CRITICAL CHECK 3: Sample size requirements (n >> p)
      min_obs_required <- ceiling(min_obs_ratio * length(common_cols))
      if (nrow(data2_clean) < min_obs_required) {
        stop(sprintf(
          "Insufficient observations for %s analysis. Need at least %d rows (%.1f × %d variables), but have %d. Consider:\n- Using fewer columns\n- Collecting more data\n- Using simpler methods",
          method, min_obs_required, min_obs_ratio, length(common_cols), nrow(data2_clean)
        ))
      }
      
      # CRITICAL CHECK 4: Zero variance columns
      col_vars <- apply(data2_clean, 2, var, na.rm = TRUE)
      zero_var_cols <- common_cols[col_vars == 0 | is.na(col_vars)]
      if (length(zero_var_cols) > 0) {
        stop(sprintf(
          "Columns with zero variance in reference dataset: %s\nThese columns cannot be used for multivariate analysis as they provide no information for distance calculations.",
          paste(zero_var_cols, collapse = ", ")
        ))
      }
      
      # CRITICAL CHECK 5: Covariance matrix conditioning
      cov_matrix <- cov(data2_clean)
      
      # Check determinant
      det_value <- det(cov_matrix)
      if (det_value == 0 || is.na(det_value)) {
        stop(sprintf(
          "Covariance matrix is singular (determinant = %.2e).\nThis means columns are perfectly correlated or have identical values.\nConsider:\n- Removing highly correlated columns\n- Using robust methods\n- Checking data quality",
          det_value
        ))
      }
      
      # Check condition number
      eigenvals <- eigen(cov_matrix, only.values = TRUE)$values
      min_eigenval <- min(eigenvals)
      max_eigenval <- max(eigenvals)
      condition_number <- max_eigenval / min_eigenval
      
      # Warning for high condition numbers (potential numerical instability)
      if (condition_number > 1e12) {
        warning(sprintf(
          "High condition number detected (%.2e). This may cause numerical instability.\nConsider:\n- Removing highly correlated columns\n- Standardizing variables\n- Using robust methods",
          condition_number
        ))
      }
      
      # CRITICAL CHECK 6: Correlation structure
      cor_matrix <- cor(data2_clean)
      high_corr_pairs <- which(abs(cor_matrix) > 0.95 & upper.tri(cor_matrix), arr.ind = TRUE)
      
      if (nrow(high_corr_pairs) > 0) {
        high_corr_vars <- apply(high_corr_pairs, 1, function(pair) {
          paste(common_cols[pair[1]], "↔", common_cols[pair[2]], 
                sprintf("(r=%.3f)", cor_matrix[pair[1], pair[2]]))
        })
        
        warning(sprintf(
          "High correlations detected (>0.95):\n%s\nThis may cause numerical instability in multivariate analysis.",
          paste(high_corr_vars, collapse = "\n")
        ))
      }
      
      # Return validated data and metadata
      return(list(
        data1_clean = data1_clean,
        data2_clean = data2_clean,
        common_cols = common_cols,
        n_vars = length(common_cols),
        n_obs1 = nrow(data1_clean),
        n_obs2 = nrow(data2_clean),
        condition_number = condition_number,
        high_correlations = if (nrow(high_corr_pairs) > 0) high_corr_vars else NULL,
        validation_passed = TRUE
      ))
    }
    
    # Function to generate filtered data for export based on current filter configuration
    generate_filtered_data_for_export <- function(dataset_num = 1) {
      if (dataset_num == 1) {
        req(input$xlsx_file1, input$element_A1, input$element_B1, input$element_C1)
        file_path <- input$xlsx_file1$datapath
        element_A <- input$element_A1
        element_B <- input$element_B1
        element_C <- input$element_C1
        optional_param1 <- input$optional_param1_1
        optional_param2 <- input$optional_param2_1
        filter_op1 <- input$filter_op1_1
        filter_op2 <- input$filter_op2_1
        use_mahalanobis <- input$use_mahalanobis
        use_robust_mahalanobis <- input$use_robust_mahalanobis
        use_isolation_forest <- input$use_isolation_forest
        use_iqr_filter <- input$use_iqr_filter
        use_zscore_filter <- input$use_zscore_filter
        use_mad_filter <- input$use_mad_filter
        lambda <- input$lambda
        omega <- input$omega
        keep_outliers_mahalanobis <- input$outlier_mode_mahalanobis
        keep_outliers_robust <- input$outlier_mode_robust
        keep_outliers_isolation <- input$outlier_mode_isolation
        keep_outliers_iqr <- input$outlier_mode_iqr
        keep_outliers_zscore <- input$outlier_mode_zscore
        keep_outliers_mad <- input$outlier_mode_mad
        mahalanobis_reference <- input$mahalanobis_reference
        selected_columns <- input$mahalanobis_columns
        custom_mdthresh <- if (input$mdthresh_mode == "manual") input$custom_mdthresh else NULL
      } else {
        req(input$xlsx_file2, input$element_A2, input$element_B2, input$element_C2)
        file_path <- input$xlsx_file2$datapath
        element_A <- input$element_A2
        element_B <- input$element_B2
        element_C <- input$element_C2
        optional_param1 <- input$optional_param1_2
        optional_param2 <- input$optional_param2_2
        filter_op1 <- input$filter_op1_2
        filter_op2 <- input$filter_op2_2
        use_mahalanobis <- input$use_mahalanobis
        use_robust_mahalanobis <- input$use_robust_mahalanobis
        use_isolation_forest <- input$use_isolation_forest
        use_iqr_filter <- input$use_iqr_filter
        use_zscore_filter <- input$use_zscore_filter
        use_mad_filter <- input$use_mad_filter
        lambda <- input$lambda
        omega <- input$omega
        keep_outliers_mahalanobis <- input$outlier_mode_mahalanobis
        keep_outliers_robust <- input$outlier_mode_robust
        keep_outliers_isolation <- input$outlier_mode_isolation
        keep_outliers_iqr <- input$outlier_mode_iqr
        keep_outliers_zscore <- input$outlier_mode_zscore
        keep_outliers_mad <- input$outlier_mode_mad
        mahalanobis_reference <- input$mahalanobis_reference
        selected_columns <- input$mahalanobis_columns
        custom_mdthresh <- if (input$mdthresh_mode == "manual") input$custom_mdthresh else NULL
      }
      
      # Get individual filters
      individual_filters_A <- get_individual_filters(element_A, paste0("A", dataset_num))
      individual_filters_B <- get_individual_filters(element_B, paste0("B", dataset_num))
      individual_filters_C <- get_individual_filters(element_C, paste0("C", dataset_num))
      
      # Create filter configuration for caching
      filter_config <- list(
        file_path = file_path,
        element_A = element_A,
        element_B = element_B,
        element_C = element_C,
        optional_param1 = optional_param1,
        optional_param2 = optional_param2,
        filter_op1 = filter_op1,
        filter_op2 = filter_op2,
        use_mahalanobis = use_mahalanobis,
        use_robust_mahalanobis = use_robust_mahalanobis,
        use_isolation_forest = use_isolation_forest,
        use_iqr_filter = use_iqr_filter,
        use_zscore_filter = use_zscore_filter,
        use_mad_filter = use_mad_filter,
        lambda = lambda,
        omega = omega,
        keep_outliers_mahalanobis = keep_outliers_mahalanobis,
        keep_outliers_robust = keep_outliers_robust,
        keep_outliers_isolation = keep_outliers_isolation,
        keep_outliers_iqr = keep_outliers_iqr,
        keep_outliers_zscore = keep_outliers_zscore,
        keep_outliers_mad = keep_outliers_mad,
        mahalanobis_reference = mahalanobis_reference,
        selected_columns = selected_columns,
        custom_mdthresh = custom_mdthresh
      )
      
      # Check if we have cached filtered data for this configuration
      if (dataset_num == 1) {
        if (identical(rv$last_filter_config1, filter_config) && !is.null(rv$filtered_data1)) {
          return(rv$filtered_data1)
        }
      } else {
        if (identical(rv$last_filter_config2, filter_config) && !is.null(rv$filtered_data2)) {
          return(rv$filtered_data2)
        }
      }
      
      # Load and filter data
      data <- openxlsx::read.xlsx(file_path, sheet = 1)
      
      # Apply individual element filters
      if (length(element_A) > 0) {
        data <- apply_individual_filters(data, list(col = element_A), individual_filters_A, "A", FALSE)
      }
      if (length(element_B) > 0) {
        data <- apply_individual_filters(data, list(col = element_B), individual_filters_B, "B", FALSE)
      }
      if (length(element_C) > 0) {
        data <- apply_individual_filters(data, list(col = element_C), individual_filters_C, "C", FALSE)
      }
      
      # Apply optional parameter filters
      if (!is.null(optional_param1) && nzchar(optional_param1) && !is.null(filter_op1) && nzchar(filter_op1)) {
        data <- apply_filter(data, optional_param1, filter_op1)
      }
      if (!is.null(optional_param2) && nzchar(optional_param2) && !is.null(filter_op2) && nzchar(filter_op2)) {
        data <- apply_filter(data, optional_param2, filter_op2)
      }
      
      # Apply statistical filters
      all_selected_elements <- c(element_A, element_B, element_C)
      
      if (use_iqr_filter) {
        data <- apply_iqr_filter(data, all_selected_elements, keep_outliers = keep_outliers_iqr)
      }
              if (use_zscore_filter) {
          data <- apply_zscore_filter(data, all_selected_elements, keep_outliers = keep_outliers_zscore)
        }
      if (use_mad_filter) {
        data <- apply_mad_filter(data, all_selected_elements, keep_outliers = keep_outliers_mad)
      }
      
      # Apply multivariate analysis filters
      if (use_mahalanobis || use_robust_mahalanobis || use_isolation_forest) {
        # For export, we'll use the current dataset as reference (self-reference)
        reference_data <- data
        
        # Apply unified validation before multivariate analysis
        tryCatch({
          if (use_robust_mahalanobis) {
            result <- compute_robust_mahalanobis(data, reference_data, keep_outliers = keep_outliers_robust, selected_columns = selected_columns)
            keep_indices <- if (keep_outliers_robust) result$outlier_indices else which(result$distances <= result$threshold_95)
          } else if (use_isolation_forest) {
            result <- compute_isolation_forest(data, reference_data, keep_outliers = keep_outliers_isolation, selected_columns = selected_columns)
            keep_indices <- if (keep_outliers_isolation) result$outlier_indices else !result$outlier_indices
          } else {
            result <- compute_mahalanobis_distance(data, reference_data, lambda, omega, keep_outliers = keep_outliers_mahalanobis, custom_mdthresh = custom_mdthresh, selected_columns = selected_columns, mdthresh_mode = "auto")
            keep_indices <- if (keep_outliers_mahalanobis) result$outlier_indices else !result$outlier_indices
          }
          
          # Apply the filtering
          if (length(keep_indices) > 0) {
            data <- data[keep_indices, , drop = FALSE]
          }
        }, error = function(e) {
          warning(sprintf("Multivariate analysis failed during export: %s. Proceeding with unfiltered data.", e$message))
        })
      }
      
      # Cache the filtered data and configuration
      if (dataset_num == 1) {
        rv$filtered_data1 <- data
        rv$last_filter_config1 <- filter_config
      } else {
        rv$filtered_data2 <- data
        rv$last_filter_config2 <- filter_config
      }
      
      return(data)
    }
    
    # Function to create scatter plots with multiple columns and custom colors
    create_scatter_plots <- function(data, columns, colors = NULL, x_column = NULL, y_column = NULL, point_size = 0.8, dataset_mode = "dataset1") {
      if (is.null(colors)) {
        colors <- rainbow(length(columns))
      }
      
      if (length(colors) != length(columns)) {
        colors <- rep(colors, length.out = length(columns))
      }
      
      # Handle dataset comparison
      if (dataset_mode == "both" && is.list(data) && length(data) == 2) {
        data1 <- data[[1]]
        data2 <- data[[2]]
        
        # If no x/y columns specified, use first two numeric columns from dataset 1
        if (is.null(x_column) || is.null(y_column)) {
          numeric_cols <- sapply(data1, is.numeric)
          if (sum(numeric_cols) >= 2) {
            numeric_col_names <- names(data1)[numeric_cols]
            x_column <- numeric_col_names[1]
            y_column <- numeric_col_names[2]
          } else {
            stop("Need at least 2 numeric columns for scatter plot")
          }
        }
        
        # Find range for both datasets
        all_x <- c(data1[[x_column]], data2[[x_column]])
        all_y <- c(data1[[y_column]], data2[[y_column]])
        x_range <- range(all_x, na.rm = TRUE)
        y_range <- range(all_y, na.rm = TRUE)
        
        # Create plot with both datasets
        plot(data1[[x_column]], data1[[y_column]], 
             type = "n", 
             xlab = x_column, 
             ylab = y_column,
             xlim = x_range,
             ylim = y_range,
             main = "Scatter Plot: Dataset 1 vs Dataset 2")
        
        # Add points for dataset 1 (blue)
        points(data1[[x_column]], data1[[y_column]], 
               col = "blue", 
               pch = 16, 
               cex = point_size)
        
        # Add points for dataset 2 (red)
        points(data2[[x_column]], data2[[y_column]], 
               col = "red", 
               pch = 17, 
               cex = point_size)
        
        # Add legend
        legend("topright", 
               legend = c("Dataset 1", "Dataset 2"),
               col = c("blue", "red"),
               pch = c(16, 17),
               cex = 0.8)
        
        return(list(x_column = x_column, y_column = y_column, colors = c("blue", "red"), dataset_mode = "both"))
        
      } else {
        # Single dataset mode
      # If no x/y columns specified, use first two numeric columns
      if (is.null(x_column) || is.null(y_column)) {
        numeric_cols <- sapply(data, is.numeric)
        if (sum(numeric_cols) >= 2) {
          numeric_col_names <- names(data)[numeric_cols]
          x_column <- numeric_col_names[1]
          y_column <- numeric_col_names[2]
        } else {
          stop("Need at least 2 numeric columns for scatter plot")
        }
      }
      
      # Create plot
      plot(data[[x_column]], data[[y_column]], 
           type = "n", 
           xlab = x_column, 
           ylab = y_column,
             main = paste("Scatter Plot:", if(dataset_mode == "dataset2") "Dataset 2" else "Dataset 1"))
      
      # Add points for each column
      for (i in seq_along(columns)) {
        col_name <- columns[i]
        if (col_name %in% names(data)) {
          points(data[[x_column]], data[[y_column]], 
                 col = colors[i], 
                 pch = 16, 
                 cex = point_size)
        }
      }
      
      # Add legend
      legend("topright", 
             legend = columns,
             col = colors,
             pch = 16,
             cex = 0.8)
      
        return(list(x_column = x_column, y_column = y_column, colors = colors, dataset_mode = dataset_mode))
      }
    }
    
    # Function to create histograms with multiple columns and custom colors
    create_histograms <- function(data, columns, colors = NULL, breaks = "Sturges", alpha = 0.7, 
                                 manual_breaks = NULL, data_type = "frequency", dataset_mode = "dataset1") {
      if (is.null(colors)) {
        colors <- rainbow(length(columns))
      }
      
      if (length(colors) != length(columns)) {
        colors <- rep(colors, length.out = length(columns))
      }
      
      # Handle dataset comparison
      if (dataset_mode == "both" && is.list(data) && length(data) == 2) {
        data1 <- data[[1]]
        data2 <- data[[2]]
        
        # Use first column for comparison if multiple columns selected
        selected_col <- columns[1]
        
        # Find range for both datasets
        all_values <- c(data1[[selected_col]], data2[[selected_col]])
        x_range <- range(all_values, na.rm = TRUE)
        
        # Set y-axis label based on data type
        y_lab <- switch(data_type,
                        "frequency" = "Frequency",
                        "density" = "Density",
                        "proportion" = "Proportion")
        
        # Create histograms for both datasets
        hist1 <- hist(data1[[selected_col]], breaks = breaks, plot = FALSE)
        hist2 <- hist(data2[[selected_col]], breaks = breaks, plot = FALSE)
        
        # Convert to desired data type
        if (data_type == "density") {
          hist1$counts <- hist1$density
          hist2$counts <- hist2$density
        } else if (data_type == "proportion") {
          hist1$counts <- hist1$counts / sum(hist1$counts)
          hist2$counts <- hist2$counts / sum(hist2$counts)
        }
        
        # Plot first histogram (Dataset 1 - blue)
        plot(hist1, 
             col = rgb(0, 0, 255, alpha = alpha * 255, maxColorValue = 255),
             border = "blue",
             xlim = x_range,
             xlab = "Value",
             ylab = y_lab,
             main = paste("Histogram Comparison:", selected_col))
        
        # Add second histogram (Dataset 2 - red)
        plot(hist2, 
             col = rgb(255, 0, 0, alpha = alpha * 255, maxColorValue = 255),
             border = "red",
             add = TRUE)
        
        # Add legend
        legend("topright", 
               legend = c("Dataset 1", "Dataset 2"),
               fill = c(rgb(0, 0, 255, alpha = alpha * 255, maxColorValue = 255), 
                       rgb(255, 0, 0, alpha = alpha * 255, maxColorValue = 255)),
               border = c("blue", "red"),
               cex = 0.8)
        
        return(list(colors = c("blue", "red"), breaks = breaks, data_type = data_type, dataset_mode = "both"))
      }
      
      # Single dataset mode
      # Create transparent colors
      transparent_colors <- sapply(colors, function(col) {
        rgb(t(col2rgb(col)), alpha = alpha * 255, maxColorValue = 255)
      })
      
      # Handle manual breaks
      if (breaks == "manual" && !is.null(manual_breaks)) {
        breaks <- manual_breaks
      }
      
      # Find range for all columns to set consistent x-axis
      all_values <- unlist(data[columns])
      x_range <- range(all_values, na.rm = TRUE)
      
      # Set y-axis label based on data type
      y_lab <- switch(data_type,
                      "frequency" = "Frequency",
                      "density" = "Density",
                      "proportion" = "Proportion")
      
      # Create first histogram
      hist_data <- hist(data[[columns[1]]], 
                        breaks = breaks,
                        plot = FALSE)
      
      # Convert to desired data type
      if (data_type == "density") {
        hist_data$counts <- hist_data$density
      } else if (data_type == "proportion") {
        hist_data$counts <- hist_data$counts / sum(hist_data$counts)
      }
      
      # Plot first histogram
      plot(hist_data, 
           col = transparent_colors[1],
           border = colors[1],
           xlim = x_range,
           xlab = "Value",
           ylab = y_lab,
           main = paste("Histograms:", if(dataset_mode == "dataset2") "Dataset 2" else "Dataset 1"))
      
      # Add additional histograms
      for (i in 2:length(columns)) {
        col_name <- columns[i]
        if (col_name %in% names(data)) {
          hist_data_i <- hist(data[[col_name]], 
                              breaks = breaks,
                              plot = FALSE)
          
          # Convert to desired data type
          if (data_type == "density") {
            hist_data_i$counts <- hist_data_i$density
          } else if (data_type == "proportion") {
            hist_data_i$counts <- hist_data_i$counts / sum(hist_data_i$counts)
          }
          
          plot(hist_data_i, 
               col = transparent_colors[i],
               border = colors[i],
               add = TRUE)
        }
      }
      
      # Add legend
      legend("topright", 
             legend = columns,
             fill = transparent_colors,
             border = colors,
             cex = 0.8)
      
      return(list(colors = colors, breaks = breaks, data_type = data_type))
    }
    
    # Function to create box plots with multiple columns and custom colors
    create_box_plots <- function(data, columns, colors = NULL, horizontal = FALSE, 
                                 box_width = 0.8, notch = FALSE, show_outliers = TRUE, dataset_mode = "dataset1") {
      if (is.null(colors)) {
        colors <- rainbow(length(columns))
      }
      
      if (length(colors) != length(columns)) {
        colors <- rep(colors, length.out = length(columns))
      }
      
      # Handle dataset comparison
      if (dataset_mode == "both" && is.list(data) && length(data) == 2) {
        data1 <- data[[1]]
        data2 <- data[[2]]
        
        # Use first column for comparison if multiple columns selected
        selected_col <- columns[1]
        
        # Prepare data for comparison boxplot
        plot_data <- list(
          "Dataset 1" = data1[[selected_col]],
          "Dataset 2" = data2[[selected_col]]
        )
        
        # Create comparison boxplot
        boxplot(plot_data, 
                col = c("blue", "red"),
                border = c("blue", "red"),
                horizontal = horizontal,
                width = rep(box_width, 2),
                notch = notch,
                outline = show_outliers,
                xlab = ifelse(horizontal, "Value", "Dataset"),
                ylab = ifelse(horizontal, "Dataset", "Value"),
                main = paste("Box Plot Comparison:", selected_col))
        
        # Add legend
        legend("topright", 
               legend = c("Dataset 1", "Dataset 2"),
               fill = c("blue", "red"),
               border = c("blue", "red"),
               cex = 0.8)
        
        return(list(colors = c("blue", "red"), horizontal = horizontal, box_width = box_width, dataset_mode = "both"))
      }
      
      # Single dataset mode
      # Prepare data for boxplot
      plot_data <- data[columns]
      
      # Handle horizontal boxplot with many columns
      if (horizontal && length(columns) > 2) {
        # Show only first 2 column names for readability
        col_names <- columns
        if (length(columns) > 2) {
          col_names[3:length(columns)] <- paste0("Col", 3:length(columns))
        }
        names(plot_data) <- col_names
      }
      
      # Create boxplot
      boxplot(plot_data, 
              col = colors,
              border = "black",
              horizontal = horizontal,
              width = rep(box_width, length(columns)),
              notch = notch,
              outline = show_outliers,
              xlab = ifelse(horizontal, "Value", "Column"),
              ylab = ifelse(horizontal, "Column", "Value"),
              main = paste("Box Plots:", if(dataset_mode == "dataset2") "Dataset 2" else "Dataset 1"))
      
      # Add legend
      legend("topright", 
             legend = columns,
             fill = colors,
             cex = 0.8)
      
      return(list(colors = colors, horizontal = horizontal, box_width = box_width, dataset_mode = dataset_mode))
    }
    
    # Function to validate and clean column names for analysis
    validate_columns_for_analysis <- function(data, columns) {
      # Check if columns exist
      missing_cols <- setdiff(columns, names(data))
      if (length(missing_cols) > 0) {
        warning("The following columns are missing: ", paste(missing_cols, collapse = ", "))
        columns <- intersect(columns, names(data))
      }
      
      # Check if columns are numeric
      numeric_cols <- sapply(data[columns], is.numeric)
      non_numeric_cols <- columns[!numeric_cols]
      if (length(non_numeric_cols) > 0) {
        warning("The following columns are not numeric and will be skipped: ", 
                paste(non_numeric_cols, collapse = ", "))
        columns <- columns[numeric_cols]
      }
      
      # Remove any remaining problematic columns
      columns <- columns[!is.na(columns) & nzchar(columns)]
      
      if (length(columns) == 0) {
        stop("No valid columns found for analysis")
      }
      
      return(columns)
    }
    
    # Enhanced logging system
    log_operation <- function(level, message, details = NULL) {
      timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      log_entry <- list(
        timestamp = timestamp,
        level = level,
        message = message,
        details = details
      )
      
      rv$analysis_log <- c(rv$analysis_log, list(log_entry))
      
          # Keep only last 10000 log entries for performance
    if (length(rv$analysis_log) > 10000) {
      rv$analysis_log <- rv$analysis_log[-(1:(length(rv$analysis_log) - 10000))]
    }
      
      # Console output for debugging
      cat(sprintf("[%s] %s: %s\n", timestamp, level, message))
    }
    
    # Error handling wrapper
    safe_execute <- function(expr, error_msg = "Operation failed") {
      tryCatch({
        log_operation("INFO", "Starting operation", error_msg)
        result <- eval(expr)
        log_operation("INFO", "Operation completed successfully", error_msg)
        return(result)
      }, error = function(e) {
        log_operation("ERROR", paste(error_msg, ":", e$message))
        show_message(paste(error_msg, ":", e$message), "error")
        return(NULL)
      })
    }
    
    # For sharing between observers
    rv <- reactiveValues(
      stats1=NULL, validation1=NULL, correlation1=NULL, df1=NULL,
      stats2=NULL, validation2=NULL, correlation2=NULL, df2=NULL,
      mahalanobis_result=NULL,
      # New reactive values for enhanced features
      project_data=NULL,
      analysis_log=list(),
      export_files=list(),
      advanced_plot_data=NULL
    )
    
    # Status output
    output$status <- renderText({
      "Ready to generate plots. Use the buttons above to create your ternary plots."
    })
    
    # Project management functions
    create_project_folder <- function(project_name) {
      if (is.null(project_name) || project_name == "") {
        project_name <- paste0("Project_", format(Sys.time(), "%Y%m%d_%H%M%S"))
      }
      
      # Clean project name for file system
      safe_name <- gsub("[^a-zA-Z0-9._-]", "_", project_name)
      project_path <- file.path(working_dir(), safe_name)
      
      # Create project folder if it doesn't exist
      if (!dir.exists(project_path)) {
        dir.create(project_path, recursive = TRUE)
        log_operation("INFO", "Created project folder", project_path)
      }
      
      return(list(name = project_name, path = project_path, safe_name = safe_name))
    }
    
    save_project <- function() {
      project_info <- create_project_folder(input$project_name)
      
      # Collect all current state
      project_data <- list(
        timestamp = Sys.time(),
        project_name = project_info$name,
        working_dir = working_dir(),
        output_dir = output_dir(),
        # UI state
        element_A1 = input$element_A1,
        element_B1 = input$element_B1,
        element_C1 = input$element_C1,
        element_A2 = input$element_A2,
        element_B2 = input$element_B2,
        element_C2 = input$element_C2,
        # Multivariate analysis settings
        use_mahalanobis = input$use_mahalanobis,
        mahalanobis_reference = input$mahalanobis_reference,
        mahalanobis_columns = input$mahalanobis_columns,
        lambda = input$lambda,
        omega = input$omega,
        # Results
        stats1 = rv$stats1,
        stats2 = rv$stats2,
        mahalanobis_result = rv$mahalanobis_result,
        # Analysis log
        analysis_log = rv$analysis_log
      )
      
      # Save project file
      project_file <- file.path(project_info$path, paste0(project_info$safe_name, ".json"))
      writeLines(jsonlite::toJSON(project_data, auto_unbox = TRUE, pretty = TRUE), project_file)
      
      # Save data files if they exist
      if (!is.null(rv$df1)) {
        writexl::write_xlsx(rv$df1, file.path(project_info$path, "dataset1.xlsx"))
      }
      if (!is.null(rv$df2)) {
        writexl::write_xlsx(rv$df2, file.path(project_info$path, "dataset2.xlsx"))
      }
      
      rv$project_data <- project_data
      log_operation("INFO", "Project saved successfully", project_file)
      show_message("Project saved successfully!", "success")
      
      return(project_file)
    }
    
    load_project <- function() {
      # Create a file input for project loading
      project_file_input <- fileInput("project_file_input", "Select Project File (.json)", 
                                      accept = c(".json"), multiple = FALSE)
      
      # Show modal with file input
      showModal(modalDialog(
        title = "Load Project",
        tagList(
          p("Please select a project file (.json) to load:"),
          project_file_input,
          p(em("Note: The project file should contain all your analysis settings and results."))
        ),
        easyClose = TRUE,
        size = "m",
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_load_project", "Load Project", class = "btn-primary")
        )
      ))
      
      # Handle project loading when file is selected and confirmed
      observeEvent(input$confirm_load_project, {
        req(input$project_file_input)
        
        tryCatch({
          # Read project file
          project_data <- jsonlite::fromJSON(input$project_file_input$datapath)
          
          # Validate project data structure
          if (!all(c("timestamp", "project_name", "working_dir", "output_dir") %in% names(project_data))) {
            show_message("Invalid project file format", "error")
            return()
          }
          
          # Restore project state
          rv$project_data <- project_data
          
          # Update reactive values
          if (!is.null(project_data$stats1)) rv$stats1 <- project_data$stats1
          if (!is.null(project_data$stats2)) rv$stats2 <- project_data$stats2
          if (!is.null(project_data$mahalanobis_result)) rv$mahalanobis_result <- project_data$mahalanobis_result
          if (!is.null(project_data$analysis_log)) rv$analysis_log <- project_data$analysis_log
          
          # Log successful load
          log_operation("INFO", "Project loaded successfully", project_data$project_name)
          show_message("Project loaded successfully!", "success")
          
          # Close modal
          removeModal()
          
        }, error = function(e) {
          show_message(paste("Error loading project:", e$message), "error")
          log_operation("ERROR", "Failed to load project", e$message)
        })
      })
    }
    
    # Help button
    observeEvent(input$help_button, {
      showModal(modalDialog(
        title = "Help - Ternary Plot Generator",
        HTML("
          <h4>How to use:</h4>
          <ol>
            <li>Select your working and output directories</li>
            <li>Upload your Excel files (Dataset 1 and Dataset 2)</li>
            <li>Choose elements A, B, and C for each dataset</li>
            <li>Configure optional parameters and filters</li>
            <li>Set up multivariate analysis if needed</li>
            <li>Click 'Save Plot 1', 'Save Plot 2', or 'Save Both Plots'</li>
          </ol>
          
          <h4>Features:</h4>
          <ul>
            <li>Individual element filtering</li>
            <li>Multiple multivariate analysis methods</li>
            <li>Customizable plot options</li>
            <li>Progress tracking and error handling</li>
            <li>Data caching for better performance</li>
            <li>Project management and data export</li>
            <li>Advanced plotting options</li>
            <li>Analysis logging</li>
                     <li>Plot notes and details (can be toggled on/off)</li>
                     <li>Professional legends for optional parameters (point size/type and color)</li>
                     <li>Multiple plot types (scatter, histogram, box plots) with custom colors</li>
                     <li>Robust column name handling for special characters (e.g., 'Ti. (Wt.%)')</li>
                     <li>Automatic title splitting for long plot titles</li>
                     <li>Comprehensive statistical analysis information in plot notes</li>
                     <li>Multiple Ternary Creator with preview-then-save workflow</li>
                     <li>Enhanced plot saving with file type selection</li>
                     <li>Advanced histogram options (manual breaks, data representation)</li>
                     <li>Improved box plot functionality (notches, outliers, width control)</li>
                   </ul>
                   
                   <h4>Multiple Plot Types:</h4>
                   <ul>
                     <li><strong>Scatter Plots:</strong> Select multiple columns and assign custom colors</li>
                     <li><strong>Histograms:</strong> Overlay multiple distributions with transparency</li>
                     <li><strong>Box Plots:</strong> Compare distributions across multiple columns</li>
                     <li>All plots support columns with special characters</li>
                     <li>Use the 'Multiple Plot Types' tab to access these features</li>
                   </ul>
                   
                   <h4>Legends:</h4>
                   <ul>
                     <li><strong>Point Size/Type Legend:</strong> Positioned at top-right, shows size scale or point types</li>
                     <li><strong>Color Legend:</strong> Positioned at top-left, shows color categories</li>
                     <li>Legends are visible in both preview and saved plots</li>
                     <li>Uses original column names (not cleaned names)</li>
                   </ul>
                   
                   <h4>Plot Notes:</h4>
                   <ul>
                     <li>Comprehensive information about elements, filters, and parameters</li>
                     <li>Statistical and multivariate analysis details with parameters</li>
                     <li>Can be toggled on/off with checkbox</li>
                   </ul>
                   
                   <h4>Multiple Plot Types - Enhanced Features:</h4>
                   <ul>
                     <li><strong>Scatter Plots:</strong> Customizable point sizes, multiple column selection, custom colors</li>
                     <li><strong>Histograms:</strong> Manual break control, data representation options (frequency/density/proportion)</li>
                     <li><strong>Box Plots:</strong> Horizontal orientation, notch options, outlier control, width adjustment</li>
                     <li><strong>Save Options:</strong> Multiple output formats (PNG, JPEG, PDF, TIFF), custom filenames</li>
                     <li><strong>Filename Suggestions:</strong> Automatic filename generation with timestamps</li>
                   </ul>
                   
                   <h4>Multiple Ternary Creator:</h4>
                   <ul>
                     <li>Batch processing of multiple Excel files</li>
                     <li>Same parameter settings for all files</li>
                     <li>Two-step workflow: Preview first, then save</li>
                     <li>Preview mode: Create plots without saving files</li>
                     <li>Save mode: Save all plots to organized subfolder</li>
                     <li>Organized output folder structure</li>
                   </ul>
                   
                   <h4>Statistical Analysis:</h4>
                   <ul>
                     <li>Mahalanobis Distance with formula explanation</li>
                     <li>Robust Mahalanobis (MCD) analysis</li>
                     <li>Isolation Forest outlier detection</li>
                     <li>IQR, Z-Score, and MAD filtering options</li>
                     <li>Configurable outlier handling (keep or filter)</li>
          </ul>
        "),
        easyClose = TRUE,
        size = "l"
      ))
    })
    
    volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), shinyFiles::getVolumes()())
    shinyDirChoose(input, "working_dir", roots = volumes, session = session)
    shinyDirChoose(input, "output_dir", roots = volumes, session = session)
    
    working_dir <- reactive({
      wd <- shinyFiles::parseDirPath(volumes, input$working_dir)
      if (length(wd) == 0) default_working_dir else wd
    })
    
    output_dir <- reactive({
      od <- shinyFiles::parseDirPath(volumes, input$output_dir)
      if (length(od) == 0) default_output_dir else od
    })
    
    output$working_dir_text <- renderText({
      wd <- working_dir()
      if (length(wd) == 0) "" else wd
    })
    
    output$output_dir_text <- renderText({
      od <- output_dir()
      if (length(od) == 0) "" else od
    })
    
    # New output renderers for enhanced features
    output$project_status <- renderText({
      if (is.null(rv$project_data)) {
        "No project saved yet"
      } else {
        paste("Last saved:", format(rv$project_data$timestamp, "%Y-%m-%d %H:%M:%S"),
              "\nProject:", rv$project_data$project_name)
      }
    })
    
    output$advanced_plot <- renderPlotly({
      if (is.null(rv$advanced_plot_data)) {
        plotly::plot_ly() %>%
          plotly::add_annotations(text = "Generate a plot to see it here",
                                  showarrow = FALSE,
                                  x = 0.5, y = 0.5,
                                  xref = "paper", yref = "paper")
      } else {
        rv$advanced_plot_data
      }
    })
    
    output$plot_info <- renderText({
      if (is.null(rv$advanced_plot_data)) {
        "No plot generated yet. Select plot type and click 'Generate Plot'."
      } else {
        paste("Plot type:", input$plot_type,
              "\nDataset:", input$plot_dataset,
              "\nSamples used:", ifelse(is.null(input$max_samples), "All", input$max_samples))
      }
    })
    
    output$export_status <- renderText({
      if (length(rv$export_files) == 0) {
        "No exports yet. Select items and click export buttons."
      } else {
        paste("Last export:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
              "\nTotal exports:", length(rv$export_files))
      }
    })
    
    output$download_links <- renderUI({
      if (length(rv$export_files) == 0) {
        return("No files available for download")
      }
      
      # Create download links for recent exports
      recent_exports <- tail(rv$export_files, 5) # Show last 5 exports
      
      links <- lapply(recent_exports, function(export) {
        tags$a(href = export$path, 
               target = "_blank",
               paste("📁", export$name, "(", export$type, ")"),
               style = "display: block; margin: 5px 0; color: #007bff; text-decoration: none;")
      })
      
      do.call(tagList, links)
    })
    
    output$export_history <- renderText({
      if (length(rv$export_files) == 0) {
        "No export history available"
      } else {
        history_text <- "Export History:\n"
        for (i in seq_along(rv$export_files)) {
          export <- rv$export_files[[length(rv$export_files) - i + 1]] # Reverse order
          history_text <- paste0(history_text, 
                                 i, ". ", export$name, " (", export$type, ") - ",
                                 format(export$timestamp, "%Y-%m-%d %H:%M:%S"), "\n")
        }
        history_text
      }
    })
    
    output$analysis_log <- renderText({
      if (length(rv$analysis_log) == 0) {
        "No activities logged yet. Start using the app to see activity history."
      } else {
        # Filter by log level if specified
        filtered_log <- rv$analysis_log
        if (input$log_level != "all") {
          filtered_log <- rv$analysis_log[sapply(rv$analysis_log, function(entry) entry$level == input$log_level)]
        }
        
        # Search in log if search term provided
        if (!is.null(input$log_search) && nzchar(input$log_search)) {
          search_term <- tolower(input$log_search)
          filtered_log <- filtered_log[sapply(filtered_log, function(entry) {
            grepl(search_term, tolower(entry$message)) || 
              grepl(search_term, tolower(entry$details))
          })]
        }
        
        # Format log entries
        log_text <- ""
        for (entry in filtered_log) {
          log_text <- paste0(log_text, 
                             "[", entry$timestamp, "] ", entry$level, ": ", entry$message)
          if (!is.null(entry$details)) {
            log_text <- paste0(log_text, " (", entry$details, ")")
          }
          log_text <- paste0(log_text, "\n")
        }
        
        if (nzchar(log_text)) {
          log_text
        } else {
          "No log entries match the current filter/search criteria."
        }
      }
    })
    
    output$log_stats <- renderText({
      if (length(rv$analysis_log) == 0) {
        "No log entries"
      } else {
        total_entries <- length(rv$analysis_log)
        info_count <- sum(sapply(rv$analysis_log, function(entry) entry$level == "INFO"))
        warning_count <- sum(sapply(rv$analysis_log, function(entry) entry$level == "WARNING"))
        error_count <- sum(sapply(rv$analysis_log, function(entry) entry$level == "ERROR"))
        
        paste("Total Entries:", total_entries,
              "\nINFO:", info_count,
              "\nWARNING:", warning_count,
              "\nERROR:", error_count)
      }
    })
    
    # Update column choices when files are uploaded
    observeEvent(input$xlsx_file1, {
      req(input$xlsx_file1)
      new_M <- openxlsx::read.xlsx(input$xlsx_file1$datapath, sheet = 1)
      new_col_names <- colnames(new_M)
      updateSelectInput(session, "element_A1", choices = new_col_names, selected = character(0))
      updateSelectInput(session, "element_B1", choices = new_col_names, selected = character(0))
      updateSelectInput(session, "element_C1", choices = new_col_names, selected = character(0))
      updateSelectInput(session, "optional_param1_1", choices = c("", new_col_names), selected = "")
      updateSelectInput(session, "optional_param2_1", choices = c("", new_col_names), selected = "")
      
      # Update universal column choices with numeric columns
      numeric_cols <- names(new_M)[sapply(new_M, is.numeric)]
      # Note: universal_columns_ui is reactive, so it updates automatically
      

    })
    
    observeEvent(input$xlsx_file2, {
      req(input$xlsx_file2)
      new_M <- openxlsx::read.xlsx(input$xlsx_file2$datapath, sheet = 1)
      new_col_names <- colnames(new_M)
      updateSelectInput(session, "element_A2", choices = new_col_names, selected = character(0))
      updateSelectInput(session, "element_B2", choices = new_col_names, selected = character(0))
      updateSelectInput(session, "element_C2", choices = new_col_names, selected = character(0))
      updateSelectInput(session, "optional_param1_2", choices = c("", new_col_names), selected = "")
      updateSelectInput(session, "optional_param2_2", choices = c("", new_col_names), selected = "")
      
      # Update Mahalanobis column choices if second dataset is loaded
      if (!is.null(input$xlsx_file1)) {
        df1 <- openxlsx::read.xlsx(input$xlsx_file1$datapath, sheet = 1)
        df2 <- new_M
        numeric_cols1 <- names(df1)[sapply(df1, is.numeric)]
        numeric_cols2 <- names(df2)[sapply(df2, is.numeric)]
        common_numeric_cols <- intersect(numeric_cols1, numeric_cols2)
        
        # Update universal column choices with common numeric columns
        # Note: universal_columns_ui is reactive, so it updates automatically
        

      }
    })
    
    # Column validation function
    validate_mahalanobis_columns <- function(df1, df2, selected_columns, reference_mode) {
      if (is.null(selected_columns) || length(selected_columns) == 0) {
        # Use all common numeric columns
        numeric_cols1 <- names(df1)[sapply(df1, is.numeric)]
        numeric_cols2 <- names(df2)[sapply(df2, is.numeric)]
        common_cols <- intersect(numeric_cols1, numeric_cols2)
        selected_columns <- common_cols
      }
      
      # Validation checks
      issues <- character(0)
      warnings <- character(0)
      
      # Check minimum columns
      if (length(selected_columns) < 2) {
        issues <- c(issues, "❌ Need at least 2 columns for Mahalanobis distance calculation")
      }
      
      # Check if columns exist in both datasets
      missing_in_df1 <- setdiff(selected_columns, names(df1))
      missing_in_df2 <- setdiff(selected_columns, names(df2))
      
      if (length(missing_in_df1) > 0) {
        issues <- c(issues, paste("❌ Columns missing in Dataset 1:", paste(missing_in_df1, collapse = ", ")))
      }
      if (length(missing_in_df2) > 0) {
        issues <- c(issues, paste("❌ Columns missing in Dataset 2:", paste(missing_in_df2, collapse = ", ")))
      }
      
      # Check for numeric columns
      non_numeric_df1 <- selected_columns[!sapply(df1[, selected_columns, drop=FALSE], is.numeric)]
      non_numeric_df2 <- selected_columns[!sapply(df2[, selected_columns, drop=FALSE], is.numeric)]
      
      if (length(non_numeric_df1) > 0) {
        issues <- c(issues, paste("❌ Non-numeric columns in Dataset 1:", paste(non_numeric_df1, collapse = ", ")))
      }
      if (length(non_numeric_df2) > 0) {
        issues <- c(issues, paste("❌ Non-numeric columns in Dataset 2:", paste(non_numeric_df2, collapse = ", ")))
      }
      
      # Check for sufficient observations
      if (reference_mode == "dataset2") {
        if (nrow(df2) <= length(selected_columns)) {
          warnings <- c(warnings, paste("⚠️ Dataset 2 has few observations (", nrow(df2), ") for", length(selected_columns), "columns"))
        }
      } else if (reference_mode == "dataset1") {
        if (nrow(df1) <= length(selected_columns)) {
          warnings <- c(warnings, paste("⚠️ Dataset 1 has few observations (", nrow(df1), ") for", length(selected_columns), "columns"))
        }
      }
      
      # Check for zero variance
      if (reference_mode == "dataset2") {
        col_vars <- apply(df2[, selected_columns, drop=FALSE], 2, var, na.rm = TRUE)
        zero_var_cols <- selected_columns[col_vars == 0 | is.na(col_vars)]
        if (length(zero_var_cols) > 0) {
          issues <- c(issues, paste("❌ Zero variance columns in Dataset 2:", paste(zero_var_cols, collapse = ", ")))
        }
      } else if (reference_mode == "dataset1") {
        col_vars <- apply(df1[, selected_columns, drop=FALSE], 2, var, na.rm = TRUE)
        zero_var_cols <- selected_columns[col_vars == 0 | is.na(col_vars)]
        if (length(zero_var_cols) > 0) {
          issues <- c(issues, paste("❌ Zero variance columns in Dataset 1:", paste(zero_var_cols, collapse = ", ")))
        }
      }
      
      # Success message
      if (length(issues) == 0) {
        success_msg <- paste("✅ Column validation successful! Using", length(selected_columns), "columns:", paste(selected_columns, collapse = ", "))
        if (length(warnings) > 0) {
          success_msg <- paste0(success_msg, "\n\n⚠️ Warnings:\n", paste(warnings, collapse = "\n"))
        }
        return(list(valid = TRUE, message = success_msg, columns = selected_columns))
      } else {
        return(list(valid = FALSE, message = paste("❌ Validation failed:\n", paste(issues, collapse = "\n"), "\n\n⚠️ Warnings:\n", paste(warnings, collapse = "\n")), columns = NULL))
      }
    }
    

    
    # Dynamic filter UI generation
    create_dynamic_filters <- function(elements, dataset_suffix) {
      if (is.null(elements) || length(elements) == 0) {
        return(NULL)
      }
      
      filter_inputs <- lapply(seq_along(elements), function(i) {
        element_name <- elements[i]
        input_id <- paste0("filter_", gsub("[^A-Za-z0-9]", "_", element_name), "_", dataset_suffix)
        
        div(
          style = "margin-bottom: 5px;",
          textInput(
            inputId = input_id,
            label = paste("Filter for", element_name, ":"),
            value = "",
            placeholder = "e.g. > 0.5"
          )
        )
      })
      
      do.call(tagList, filter_inputs)
    }
    
    # Dynamic filter outputs
    output$dynamic_filters_A1 <- renderUI({
      create_dynamic_filters(input$element_A1, "A1")
    })
    
    output$dynamic_filters_B1 <- renderUI({
      create_dynamic_filters(input$element_B1, "B1")
    })
    
    output$dynamic_filters_C1 <- renderUI({
      create_dynamic_filters(input$element_C1, "C1")
    })
    
    output$dynamic_filters_A2 <- renderUI({
      create_dynamic_filters(input$element_A2, "A2")
    })
    
    output$dynamic_filters_B2 <- renderUI({
      create_dynamic_filters(input$element_B2, "B2")
    })
    
    output$dynamic_filters_C2 <- renderUI({
      create_dynamic_filters(input$element_C2, "C2")
    })
    
    # Universal columns renderUI (for all multivariate methods)
    output$universal_columns_ui <- renderUI({
      # Get current column names based on loaded datasets
      if (!is.null(input$xlsx_file1)) {
        df1 <- openxlsx::read.xlsx(input$xlsx_file1$datapath, sheet = 1)
        numeric_cols1 <- names(df1)[sapply(df1, is.numeric)]
        
        if (!is.null(input$xlsx_file2)) {
          # Both datasets loaded - use common numeric columns
          df2 <- openxlsx::read.xlsx(input$xlsx_file2$datapath, sheet = 1)
          numeric_cols2 <- names(df2)[sapply(df2, is.numeric)]
          common_numeric_cols <- intersect(numeric_cols1, numeric_cols2)
          choices <- common_numeric_cols
        } else {
          # Only dataset 1 loaded
          choices <- numeric_cols1
        }
      } else if (!is.null(input$xlsx_file2)) {
        # Only dataset 2 loaded
        df2 <- openxlsx::read.xlsx(input$xlsx_file2$datapath, sheet = 1)
        numeric_cols2 <- names(df2)[sapply(df2, is.numeric)]
        choices <- numeric_cols2
      } else {
        # No datasets loaded
        choices <- character(0)
      }
      
      selectInput("universal_columns", "Columns for all multivariate methods:", 
                  choices = choices, multiple = TRUE,
                  selected = NULL)
    })
    

    
    # Enhanced feature event handlers
    
    # Project management
    observeEvent(input$save_project, {
      log_operation("INFO", "User initiated project save")
      save_project()
    })
    
    observeEvent(input$load_project, {
      log_operation("INFO", "User initiated project load")
      load_project()
    })
    
    # Update column choices for advanced plots when files are uploaded
    observeEvent(input$xlsx_file1, {
      req(input$xlsx_file1)
      new_M <- openxlsx::read.xlsx(input$xlsx_file1$datapath, sheet = 1)
      new_col_names <- colnames(new_M)
      
      # Update advanced plot column choices
      updateSelectInput(session, "scatter_x", choices = new_col_names, selected = character(0))
      updateSelectInput(session, "scatter_y", choices = new_col_names, selected = character(0))
      updateSelectInput(session, "scatter_color", choices = c("None" = "none", new_col_names), selected = "none")
      updateSelectInput(session, "hist_column", choices = new_col_names, selected = character(0))
      updateSelectInput(session, "corr_columns", choices = new_col_names, selected = character(0))
      updateSelectInput(session, "box_y", choices = new_col_names, selected = character(0))
      updateSelectInput(session, "box_x", choices = c("None" = "none", new_col_names), selected = "none")
      updateSelectInput(session, "density_column", choices = new_col_names, selected = character(0))
      updateSelectInput(session, "density_group", choices = c("None" = "none", new_col_names), selected = "none")
    })
    
    observeEvent(input$xlsx_file2, {
      req(input$xlsx_file2)
      new_M <- openxlsx::read.xlsx(input$xlsx_file2$datapath, sheet = 1)
      new_col_names <- colnames(new_M)
      
      # Update advanced plot column choices for dataset 2
      if (input$plot_dataset == "dataset2") {
        updateSelectInput(session, "scatter_x", choices = new_col_names, selected = character(0))
        updateSelectInput(session, "scatter_y", choices = new_col_names, selected = character(0))
        updateSelectInput(session, "scatter_color", choices = c("None" = "none", new_col_names), selected = "none")
        updateSelectInput(session, "hist_column", choices = new_col_names, selected = character(0))
        updateSelectInput(session, "corr_columns", choices = new_col_names, selected = character(0))
        updateSelectInput(session, "box_y", choices = new_col_names, selected = character(0))
        updateSelectInput(session, "box_x", choices = c("None" = "none", new_col_names), selected = "none")
        updateSelectInput(session, "density_column", choices = new_col_names, selected = character(0))
        updateSelectInput(session, "density_group", choices = c("None" = "none", new_col_names), selected = "none")
      }
    })
    
    # Advanced plot generation
    observeEvent(input$generate_plot, {
      req(input$plot_type, input$plot_dataset)
      
      log_operation("INFO", "Generating advanced plot", paste("Type:", input$plot_type, "Dataset:", input$plot_dataset))
      
      # Get the appropriate dataset
      if (input$plot_dataset == "dataset1" && !is.null(input$xlsx_file1)) {
        data <- openxlsx::read.xlsx(input$xlsx_file1$datapath, sheet = 1)
      } else if (input$plot_dataset == "dataset2" && !is.null(input$xlsx_file2)) {
        data <- openxlsx::read.xlsx(input$xlsx_file2$datapath, sheet = 1)
      } else {
        show_message("Please upload the required dataset first.", "error")
        return()
      }
      
      # Limit samples if specified
      if (nrow(data) > input$max_samples) {
        set.seed(123) # For reproducible sampling
        data <- data[sample(nrow(data), input$max_samples), ]
        log_operation("INFO", "Sampled data for plotting", paste("Original:", nrow(data), "Samples:", input$max_samples))
      }
      
      # Generate plot based on type
      tryCatch({
        if (input$plot_type == "scatter") {
          req(input$scatter_x, input$scatter_y)
          plot_data <- create_scatter_plot(data, input$scatter_x, input$scatter_y, input$scatter_color, input$scatter_size)
        } else if (input$plot_type == "histogram") {
          req(input$hist_column)
          plot_data <- create_histogram_plot(data, input$hist_column, input$hist_bins, input$hist_density)
        } else if (input$plot_type == "correlation") {
          req(input$corr_columns)
          plot_data <- create_correlation_plot(data, input$corr_columns, input$corr_method)
        } else if (input$plot_type == "boxplot") {
          req(input$box_y)
          plot_data <- create_boxplot_plot(data, input$box_y, input$box_x)
        } else if (input$plot_type == "density") {
          req(input$density_column)
          plot_data <- create_density_plot(data, input$density_column, input$density_group)
        }
        
        rv$advanced_plot_data <- plot_data
        log_operation("INFO", "Advanced plot generated successfully", input$plot_type)
        show_message("Plot generated successfully!", "success")
        
      }, error = function(e) {
        log_operation("ERROR", "Failed to generate advanced plot", e$message)
        show_message(paste("Failed to generate plot:", e$message), "error")
      })
    })
    
    # Save advanced plot
    observeEvent(input$save_advanced_plot, {
      if (is.null(rv$advanced_plot_data)) {
        show_message("No plot to save. Please generate a plot first.", "error")
        return()
      }
      
      tryCatch({
        # Create filename with timestamp
        filename <- paste0("advanced_plot_", input$plot_type, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
        
        # Determine output directory
        output_dir <- if (length(working_dir()) > 0) working_dir() else getwd()
        
        # Save plot based on format
        if (input$advanced_output_format == "png") {
          file_path <- file.path(output_dir, paste0(filename, ".png"))
          png(file_path, width = 1200, height = 800, res = 150)
          print(rv$advanced_plot_data)
          dev.off()
        } else if (input$advanced_output_format == "pdf") {
          file_path <- file.path(output_dir, paste0(filename, ".pdf"))
          pdf(file_path, width = 12, height = 8)
          print(rv$advanced_plot_data)
          dev.off()
        } else if (input$advanced_output_format == "svg") {
          file_path <- file.path(output_dir, paste0(filename, ".svg"))
          svg(file_path, width = 12, height = 8)
          print(rv$advanced_plot_data)
          dev.off()
        } else if (input$advanced_output_format == "html") {
          file_path <- file.path(output_dir, paste0(filename, ".html"))
          # For HTML, we need to save the plotly object
          if ("plotly" %in% class(rv$advanced_plot_data)) {
            htmlwidgets::saveWidget(rv$advanced_plot_data, file_path)
          } else {
            show_message("HTML export only available for plotly plots", "warning")
            return()
          }
        }
        
        show_message(paste("Advanced plot saved successfully to:", file_path), "success")
        
      }, error = function(e) {
        show_message(paste("Error saving advanced plot:", e$message), "error")
      })
    })
    
    # Analysis log controls
    observeEvent(input$clear_log, {
      rv$analysis_log <- list()
      log_operation("INFO", "Analysis log cleared by user")
      show_message("Analysis log cleared.", "info")
    })
    
    observeEvent(input$export_log, {
      log_operation("INFO", "User exported analysis log")
      # Implementation for log export
      show_message("Log export functionality coming soon!", "info")
    })
    
    observeEvent(input$save_log, {
      log_operation("INFO", "User saved analysis log to file")
      # Implementation for log save
      show_message("Log save functionality coming soon!", "info")
    })
    
    observeEvent(input$search_log, {
      log_operation("INFO", "User searched analysis log", input$log_search)
      # Implementation for log search
    })
    

    
    # Multiple plot types functionality
    # Update column choices for multiple plot types when data is loaded
    observe({
      if (!is.null(input$xlsx_file1)) {
        data <- openxlsx::read.xlsx(input$xlsx_file1$datapath, sheet = 1)
        # Update scatter plot columns
        updateSelectizeInput(session, "scatter_columns", choices = names(data))
        updateSelectizeInput(session, "scatter_x_col", choices = names(data))
        updateSelectizeInput(session, "scatter_y_col", choices = names(data))
        
        # Update histogram columns
        updateSelectizeInput(session, "histogram_columns", choices = names(data))
        
        # Update boxplot columns
        updateSelectizeInput(session, "boxplot_columns", choices = names(data))
      }
    })
    
    # Dynamic color inputs for scatter plots
    output$scatter_color_inputs <- renderUI({
      if (is.null(input$scatter_columns) || length(input$scatter_columns) == 0) {
        return(p("Select columns first"))
      }
      
      color_inputs <- lapply(seq_along(input$scatter_columns), function(i) {
        col_name <- input$scatter_columns[i]
        colourInput(paste0("scatter_color_", i), 
                   label = paste("Color for", col_name), 
                   value = rainbow(length(input$scatter_columns))[i])
      })
      
      do.call(tagList, color_inputs)
    })
    
    # Dynamic color inputs for histograms
    output$histogram_color_inputs <- renderUI({
      if (is.null(input$histogram_columns) || length(input$histogram_columns) == 0) {
        return(p("Select columns first"))
      }
      
      color_inputs <- lapply(seq_along(input$histogram_columns), function(i) {
        col_name <- input$histogram_columns[i]
        colourInput(paste0("histogram_color_", i), 
                   label = paste("Color for", col_name), 
                   value = rainbow(length(input$histogram_columns))[i])
      })
      
      do.call(tagList, color_inputs)
    })
    
    # Dynamic color inputs for box plots
    output$boxplot_color_inputs <- renderUI({
      if (is.null(input$boxplot_columns) || length(input$boxplot_columns) == 0) {
        return(p("Select columns first"))
      }
      
      color_inputs <- lapply(seq_along(input$boxplot_columns), function(i) {
        col_name <- input$boxplot_columns[i]
        colourInput(paste0("boxplot_color_", i), 
                   label = paste("Color for", col_name), 
                   value = rainbow(length(input$boxplot_columns))[i])
      })
      
      do.call(tagList, color_inputs)
    })
    
    # Create scatter plot
    observeEvent(input$create_scatter, {
      if (is.null(input$xlsx_file1) || is.null(input$scatter_columns) || length(input$scatter_columns) == 0) {
        show_message("Please select data and columns for scatter plot", "error")
        return()
      }
      
      tryCatch({
        # Load data based on dataset selection
        if (input$scatter_dataset == "dataset1") {
        data <- openxlsx::read.xlsx(input$xlsx_file1$datapath, sheet = 1)
          dataset_name <- "Dataset 1"
        } else if (input$scatter_dataset == "dataset2") {
          if (is.null(input$xlsx_file2)) {
            show_message("Please upload Dataset 2 for comparison", "error")
            return()
          }
          data <- openxlsx::read.xlsx(input$xlsx_file2$datapath, sheet = 1)
          dataset_name <- "Dataset 2"
        } else if (input$scatter_dataset == "both") {
          if (is.null(input$xlsx_file2)) {
            show_message("Please upload Dataset 2 for comparison", "error")
            return()
          }
          data1 <- openxlsx::read.xlsx(input$xlsx_file1$datapath, sheet = 1)
          data2 <- openxlsx::read.xlsx(input$xlsx_file2$datapath, sheet = 1)
          dataset_name <- "Both Datasets"
        }
        
        # Get colors for each column
        colors <- sapply(seq_along(input$scatter_columns), function(i) {
          input[[paste0("scatter_color_", i)]]
        })
        
        # Validate columns
        valid_columns <- validate_columns_for_analysis(data, input$scatter_columns)
        
        if (length(valid_columns) == 0) {
          show_message("No valid columns selected for scatter plot", "error")
          return()
        }
        
        # Create scatter plot
        plot_obj <- create_scatter_plots(
          data = if(input$scatter_dataset == "both") list(data1, data2) else data,
            columns = valid_columns,
            colors = colors,
            x_column = input$scatter_x_col,
            y_column = input$scatter_y_col,
          point_size = input$scatter_point_size,
          dataset_mode = input$scatter_dataset
        )
        
        # Store plot in reactive values
        rv$scatter_plot <- plot_obj
        
        # Display plot
        output$scatter_plot_output <- renderPlot({
          print(plot_obj)
        })
        
        show_message(paste("Scatter plot created successfully using", dataset_name, "!"), "success")
        
        # Generate filename suggestion
        output$scatter_filename_suggestion <- renderText({
          paste("Suggested filename:", paste0("scatter_", paste(valid_columns, collapse = "_"), "_", 
                                            format(Sys.time(), "%Y%m%d_%H%M%S")))
        })
      }, error = function(e) {
        show_message(paste("Error creating scatter plot:", e$message), "error")
      })
    })
    
    # Create histogram
    observeEvent(input$create_histogram, {
      if (is.null(input$xlsx_file1) || is.null(input$histogram_columns) || length(input$histogram_columns) == 0) {
        show_message("Please select data and columns for histogram", "error")
        return()
      }
      
      tryCatch({
        # Load data based on dataset selection
        if (input$histogram_dataset == "dataset1") {
        data <- openxlsx::read.xlsx(input$xlsx_file1$datapath, sheet = 1)
          dataset_name <- "Dataset 1"
        } else if (input$histogram_dataset == "dataset2") {
          if (is.null(input$xlsx_file2)) {
            show_message("Please upload Dataset 2 for comparison", "error")
            return()
          }
          data <- openxlsx::read.xlsx(input$xlsx_file2$datapath, sheet = 1)
          dataset_name <- "Dataset 2"
        } else if (input$histogram_dataset == "both") {
          if (is.null(input$xlsx_file2)) {
            show_message("Please upload Dataset 2 for comparison", "error")
            return()
          }
          data1 <- openxlsx::read.xlsx(input$xlsx_file1$datapath, sheet = 1)
          data2 <- openxlsx::read.xlsx(input$xlsx_file2$datapath, sheet = 1)
          dataset_name <- "Both Datasets"
        }
        
        # Get colors for each column
        colors <- sapply(seq_along(input$histogram_columns), function(i) {
          input[[paste0("histogram_color_", i)]]
        })
        
        # Validate columns
        valid_columns <- validate_columns_for_analysis(data, input$histogram_columns)
        
        if (length(valid_columns) == 0) {
          show_message("No valid columns selected for histogram", "error")
          return()
        }
        
        # Create histogram
        plot_obj <- create_histograms(
          data = if(input$histogram_dataset == "both") list(data1, data2) else data,
            columns = valid_columns,
            colors = colors,
            breaks = input$histogram_breaks,
            alpha = input$histogram_alpha,
            manual_breaks = if(input$histogram_breaks == "manual") input$histogram_manual_breaks else NULL,
          data_type = input$histogram_type,
          dataset_mode = input$histogram_dataset
        )
        
        # Store plot in reactive values
        rv$histogram_plot <- plot_obj
        
        # Display plot
        output$histogram_plot_output <- renderPlot({
          print(plot_obj)
        })
        
        show_message(paste("Histogram created successfully using", dataset_name, "!"), "success")
        
        # Generate filename suggestion
        output$histogram_filename_suggestion <- renderText({
          paste("Suggested filename:", paste0("histogram_", paste(valid_columns, collapse = "_"), "_", 
                                            format(Sys.time(), "%Y%m%d_%H%M%S")))
        })
      }, error = function(e) {
        show_message(paste("Error creating histogram:", e$message), "error")
      })
    })
    
    # Create box plot
    observeEvent(input$create_boxplot, {
      if (is.null(input$xlsx_file1) || is.null(input$boxplot_columns) || length(input$boxplot_columns) == 0) {
        show_message("Please select data and columns for box plot", "error")
        return()
      }
      
      tryCatch({
        # Load data based on dataset selection
        if (input$boxplot_dataset == "dataset1") {
        data <- openxlsx::read.xlsx(input$xlsx_file1$datapath, sheet = 1)
          dataset_name <- "Dataset 1"
        } else if (input$boxplot_dataset == "dataset2") {
          if (is.null(input$xlsx_file2)) {
            show_message("Please upload Dataset 2 for comparison", "error")
            return()
          }
          data <- openxlsx::read.xlsx(input$xlsx_file2$datapath, sheet = 1)
          dataset_name <- "Dataset 2"
        } else if (input$boxplot_dataset == "both") {
          if (is.null(input$xlsx_file2)) {
            show_message("Please upload Dataset 2 for comparison", "error")
            return()
          }
          data1 <- openxlsx::read.xlsx(input$xlsx_file1$datapath, sheet = 1)
          data2 <- openxlsx::read.xlsx(input$xlsx_file2$datapath, sheet = 1)
          dataset_name <- "Both Datasets"
        }
        
        # Get colors for each column
        colors <- sapply(seq_along(input$boxplot_columns), function(i) {
          input[[paste0("boxplot_color_", i)]]
        })
        
        # Validate columns
        valid_columns <- validate_columns_for_analysis(data, input$boxplot_columns)
        
        if (length(valid_columns) == 0) {
          show_message("No valid columns selected for box plot", "error")
          return()
        }
        
        # Create box plot
        plot_obj <- create_box_plots(
          data = if(input$boxplot_dataset == "both") list(data1, data2) else data,
            columns = valid_columns,
            colors = colors,
            horizontal = input$boxplot_horizontal,
            box_width = input$boxplot_width,
            notch = input$boxplot_notch,
          show_outliers = input$boxplot_outliers,
          dataset_mode = input$boxplot_dataset
        )
        
        # Store plot in reactive values
        rv$boxplot_plot <- plot_obj
        
        # Display plot
        output$boxplot_plot_output <- renderPlot({
          print(plot_obj)
        })
        
        show_message(paste("Box plot created successfully using", dataset_name, "!"), "success")
        
        # Generate filename suggestion
        output$boxplot_filename_suggestion <- renderText({
          paste("Suggested filename:", paste0("boxplot_", paste(valid_columns, collapse = "_"), "_", 
                                            format(Sys.time(), "%Y%m%d_%H%M%S")))
        })
      }, error = function(e) {
        show_message(paste("Error creating box plot:", e$message), "error")
      })
    })
    
    # Advanced plot creation functions
    create_scatter_plot <- function(data, x_col, y_col, color_col, point_size) {
      if (color_col == "none") {
        p <- ggplot2::ggplot(data, ggplot2::aes_string(x = x_col, y = y_col)) +
          ggplot2::geom_point(size = point_size, alpha = 0.7) +
          ggplot2::theme_minimal() +
          ggplot2::labs(title = paste("Scatter Plot:", x_col, "vs", y_col),
                        x = x_col, y = y_col)
      } else {
        p <- ggplot2::ggplot(data, ggplot2::aes_string(x = x_col, y = y_col, color = color_col)) +
          ggplot2::geom_point(size = point_size, alpha = 0.7) +
          ggplot2::theme_minimal() +
          ggplot2::labs(title = paste("Scatter Plot:", x_col, "vs", y_col, "colored by", color_col),
                        x = x_col, y = y_col)
      }
      
      plotly::ggplotly(p, tooltip = c("x", "y", "color"))
    }
    
    create_histogram_plot <- function(data, column, bins, show_density) {
      p <- ggplot2::ggplot(data, ggplot2::aes_string(x = column)) +
        ggplot2::geom_histogram(bins = bins, fill = "steelblue", alpha = 0.7, color = "black")
      
      if (show_density) {
        p <- p + ggplot2::geom_density(ggplot2::aes(y = ..density.. * max(..count..)), 
                                       color = "red", size = 1)
      }
      
      p <- p + ggplot2::theme_minimal() +
        ggplot2::labs(title = paste("Histogram of", column),
                      x = column, y = "Count")
      
      plotly::ggplotly(p, tooltip = c("x", "y"))
    }
    
    create_correlation_plot <- function(data, columns, method) {
      if (length(columns) < 2) {
        return(NULL)
      }
      
      # Calculate correlation matrix
      cor_matrix <- cor(data[, columns, drop = FALSE], method = method, use = "complete.obs")
      
      # Create correlation plot
      p <- corrplot::corrplot(cor_matrix, method = "color", type = "upper", 
                              order = "hclust", tl.cex = 0.8, tl.col = "black",
                              addCoef.col = "black", number.cex = 0.7)
      
      # Convert to plotly if possible
      tryCatch({
        plotly::plot_ly(z = cor_matrix, x = columns, y = columns, 
                        type = "heatmap", colorscale = "RdBu") %>%
          plotly::layout(title = paste("Correlation Matrix (", method, ")"),
                         xaxis = list(title = ""), yaxis = list(title = ""))
      }, error = function(e) {
        # Fallback to static plot
        p
      })
    }
    
    create_boxplot_plot <- function(data, y_col, x_col) {
      if (x_col == "none") {
        p <- ggplot2::ggplot(data, ggplot2::aes_string(y = y_col)) +
          ggplot2::geom_boxplot(fill = "steelblue", alpha = 0.7) +
          ggplot2::theme_minimal() +
          ggplot2::labs(title = paste("Box Plot of", y_col),
                        y = y_col)
      } else {
        p <- ggplot2::ggplot(data, ggplot2::aes_string(x = x_col, y = y_col)) +
          ggplot2::geom_boxplot(fill = "steelblue", alpha = 0.7) +
          ggplot2::theme_minimal() +
          ggplot2::labs(title = paste("Box Plot of", y_col, "by", x_col),
                        x = x_col, y = y_col)
      }
      
      plotly::ggplotly(p, tooltip = c("x", "y"))
    }
    
    create_density_plot <- function(data, column, group_col) {
      if (group_col == "none") {
        p <- ggplot2::ggplot(data, ggplot2::aes_string(x = column)) +
          ggplot2::geom_density(fill = "steelblue", alpha = 0.7, color = "black") +
          ggplot2::theme_minimal() +
          ggplot2::labs(title = paste("Density Plot of", column),
                        x = column, y = "Density")
      } else {
        p <- ggplot2::ggplot(data, ggplot2::aes_string(x = column, fill = group_col)) +
          ggplot2::geom_density(alpha = 0.7) +
          ggplot2::theme_minimal() +
          ggplot2::labs(title = paste("Density Plot of", column, "by", group_col),
                        x = column, y = "Density")
      }
      
      plotly::ggplotly(p, tooltip = c("x", "y", "fill"))
    }
    
    # Save scatter plot
    observeEvent(input$save_scatter, {
      if (is.null(input$scatter_filename) || nchar(input$scatter_filename) == 0) {
        show_message("Please enter a filename for the scatter plot", "error")
        return()
      }
      
      if (is.null(rv$scatter_plot)) {
          show_message("No scatter plot to save. Please create a plot first.", "error")
          return()
        }
        
      tryCatch({
        # Create filename with extension
        filename <- paste0(input$scatter_filename, ".", input$scatter_output_format)
        
        # Determine output directory
        output_dir <- if (length(working_dir()) > 0) working_dir() else getwd()
        file_path <- file.path(output_dir, filename)
        
        # Save plot based on format
        if (input$scatter_output_format == "png") {
          png(file_path, width = 1200, height = 800, res = 150)
          print(rv$scatter_plot)
          dev.off()
        } else if (input$scatter_output_format == "jpeg") {
          jpeg(file_path, width = 1200, height = 800, res = 150, quality = 95)
          print(rv$scatter_plot)
          dev.off()
        } else if (input$scatter_output_format == "pdf") {
          pdf(file_path, width = 12, height = 8)
          print(rv$scatter_plot)
          dev.off()
        } else if (input$scatter_output_format == "tiff") {
          tiff(file_path, width = 1200, height = 800, res = 150)
          print(rv$scatter_plot)
          dev.off()
        }
        
        show_message(paste("Scatter plot saved successfully to:", file_path), "success")
        
      }, error = function(e) {
        show_message(paste("Error saving scatter plot:", e$message), "error")
      })
    })
    
    # Save histogram
    observeEvent(input$save_histogram, {
      if (is.null(input$histogram_filename) || nchar(input$histogram_filename) == 0) {
        show_message("Please enter a filename for the histogram", "error")
        return()
      }
      
      if (is.null(rv$histogram_plot)) {
          show_message("No histogram to save. Please create a plot first.", "error")
          return()
        }
        
      tryCatch({
        # Create filename with extension
        filename <- paste0(input$histogram_filename, ".", input$histogram_output_format)
        
        # Determine output directory
        output_dir <- if (length(working_dir()) > 0) working_dir() else getwd()
        file_path <- file.path(output_dir, filename)
        
        # Save plot based on format
        if (input$histogram_output_format == "png") {
          png(file_path, width = 1200, height = 800, res = 150)
          print(rv$histogram_plot)
          dev.off()
        } else if (input$histogram_output_format == "jpeg") {
          jpeg(file_path, width = 1200, height = 800, res = 150, quality = 95)
          print(rv$histogram_plot)
          dev.off()
        } else if (input$histogram_output_format == "pdf") {
          pdf(file_path, width = 12, height = 8)
          print(rv$histogram_plot)
          dev.off()
        } else if (input$histogram_output_format == "tiff") {
          tiff(file_path, width = 1200, height = 800, res = 150)
          print(rv$histogram_plot)
          dev.off()
        }
        
        show_message(paste("Histogram saved successfully to:", file_path), "success")
        
      }, error = function(e) {
        show_message(paste("Error saving histogram:", e$message), "error")
      })
    })
    
    # Save box plot
    observeEvent(input$save_boxplot, {
      if (is.null(input$boxplot_filename) || nchar(input$boxplot_filename) == 0) {
        show_message("Please enter a filename for the box plot", "error")
        return()
      }
      
      if (is.null(rv$boxplot_plot)) {
          show_message("No box plot to save. Please create a plot first.", "error")
          return()
        }
        
      tryCatch({
        # Create filename with extension
        filename <- paste0(input$boxplot_filename, ".", input$boxplot_output_format)
        
        # Determine output directory
        output_dir <- if (length(working_dir()) > 0) working_dir() else getwd()
        file_path <- file.path(output_dir, filename)
        
        # Save plot based on format
        if (input$boxplot_output_format == "png") {
          png(file_path, width = 1200, height = 800, res = 150)
          print(rv$boxplot_plot)
          dev.off()
        } else if (input$boxplot_output_format == "jpeg") {
          jpeg(file_path, width = 1200, height = 800, res = 150, quality = 95)
          print(rv$boxplot_plot)
          dev.off()
        } else if (input$boxplot_output_format == "pdf") {
          pdf(file_path, width = 12, height = 8)
          print(rv$boxplot_plot)
          dev.off()
        } else if (input$boxplot_output_format == "tiff") {
          tiff(file_path, width = 1200, height = 800, res = 150)
          print(rv$boxplot_plot)
          dev.off()
        }
        
        show_message(paste("Box plot saved successfully to:", file_path), "success")
        
      }, error = function(e) {
        show_message(paste("Error saving box plot:", e$message), "error")
      })
    })
    
    # Helper function to get individual filter values
    get_individual_filters <- function(elements, dataset_suffix) {
      if (is.null(elements) || length(elements) == 0) {
        return(list())
      }
      
      filters <- lapply(elements, function(element_name) {
        input_id <- paste0("filter_", gsub("[^A-Za-z0-9]", "_", element_name), "_", dataset_suffix)
        filter_value <- input[[input_id]]
        if (is.null(filter_value) || is.na(filter_value) || !nzchar(as.character(filter_value))) {
          return(NULL)
        }
        return(filter_value)
      })
      
      names(filters) <- elements
      # Remove NULL values
      filters[!sapply(filters, is.null)]
    }
    
    # Copy all parameters from dataset 1 to dataset 2
    observeEvent(c(input$element_A1, input$element_B1, input$element_C1, 
                   input$optional_param1_1, input$optional_param2_1,
                   input$filter_op1_1, input$filter_op2_1,
                   input$optional_param1_representation1, input$color_palette1), {
                     if (!is.null(input$xlsx_file2)) {
                       # Get current dataset 2 column names
                       df2 <- openxlsx::read.xlsx(input$xlsx_file2$datapath, sheet = 1)
                       col_names2 <- colnames(df2)
                       
                       # Copy elements if they exist in dataset 2
                       if (length(input$element_A1) > 0) {
                         matching_A <- intersect(input$element_A1, col_names2)
                         if (length(matching_A) > 0) {
                           updateSelectInput(session, "element_A2", selected = matching_A)
                           # Copy individual filters for matching elements
                           filters_A1 <- get_individual_filters(input$element_A1, "A1")
                           for (element in matching_A) {
                             if (element %in% names(filters_A1)) {
                               input_id_A2 <- paste0("filter_", gsub("[^A-Za-z0-9]", "_", element), "_A2")
                               updateTextInput(session, input_id_A2, value = filters_A1[[element]])
                             }
                           }
                         }
                       }
                       
                       if (length(input$element_B1) > 0) {
                         matching_B <- intersect(input$element_B1, col_names2)
                         if (length(matching_B) > 0) {
                           updateSelectInput(session, "element_B2", selected = matching_B)
                           # Copy individual filters for matching elements
                           filters_B1 <- get_individual_filters(input$element_B1, "B1")
                           for (element in matching_B) {
                             if (element %in% names(filters_B1)) {
                               input_id_B2 <- paste0("filter_", gsub("[^A-Za-z0-9]", "_", element), "_B2")
                               updateTextInput(session, input_id_B2, value = filters_B1[[element]])
                             }
                           }
                         }
                       }
                       
                       if (length(input$element_C1) > 0) {
                         matching_C <- intersect(input$element_C1, col_names2)
                         if (length(matching_C) > 0) {
                           updateSelectInput(session, "element_C2", selected = matching_C)
                           # Copy individual filters for matching elements
                           filters_C1 <- get_individual_filters(input$element_C1, "C1")
                           for (element in matching_C) {
                             if (element %in% names(filters_C1)) {
                               input_id_C2 <- paste0("filter_", gsub("[^A-Za-z0-9]", "_", element), "_C2")
                               updateTextInput(session, input_id_C2, value = filters_C1[[element]])
                             }
                           }
                         }
                       }
                       
                       # Copy optional parameters if they exist in dataset 2
                       if (input$optional_param1_1 != "" && input$optional_param1_1 %in% col_names2) {
                         updateSelectInput(session, "optional_param1_2", selected = input$optional_param1_1)
                         updateTextInput(session, "filter_op1_2", value = input$filter_op1_1)
                         updateSelectInput(session, "optional_param1_representation2", selected = input$optional_param1_representation1)
                       }
                       
                       if (input$optional_param2_1 != "" && input$optional_param2_1 %in% col_names2) {
                         updateSelectInput(session, "optional_param2_2", selected = input$optional_param2_1)
                         updateTextInput(session, "filter_op2_2", value = input$filter_op2_1)
                         updateSelectInput(session, "color_palette2", selected = input$color_palette1)
                       }
                     }
                   })
    
    # Data analysis functions for both datasets
    analyze_data1 <- reactive({
      req(input$element_A1, input$element_B1, input$element_C1, input$xlsx_file1)
      df <- openxlsx::read.xlsx(input$xlsx_file1$datapath, sheet=1)
      cols <- c(input$element_A1, input$element_B1, input$element_C1,
                if (input$optional_param1_1 != "") input$optional_param1_1,
                if (input$optional_param2_1 != "") input$optional_param2_1)
      validation <- validate_data(df, cols)
      stats <- generate_stats(df, cols)
      correlation <- compute_correlation(df, cols)
      rv$stats1 <- stats
      rv$validation1 <- validation
      rv$correlation1 <- correlation
      rv$df1 <- df
      list(df=df, stats=stats, validation=validation, correlation=correlation)
    })
    
    analyze_data2 <- reactive({
      req(input$element_A2, input$element_B2, input$element_C2, input$xlsx_file2)
      df <- openxlsx::read.xlsx(input$xlsx_file2$datapath, sheet=1)
      cols <- c(input$element_A2, input$element_B2, input$element_C2,
                if (input$optional_param1_2 != "") input$optional_param1_2,
                if (input$optional_param2_2 != "") input$optional_param2_2)
      validation <- validate_data(df, cols)
      stats <- generate_stats(df, cols)
      correlation <- compute_correlation(df, cols)
      rv$stats2 <- stats
      rv$validation2 <- validation
      rv$correlation2 <- correlation
      rv$df2 <- df
      list(df=df, stats=stats, validation=validation, correlation=correlation)
    })
    
    # Multivariate analysis calculation
    multivariate_analysis <- reactive({
      # Check if at least one method is selected
      if (!(input$use_mahalanobis || input$use_robust_mahalanobis || input$use_isolation_forest)) {
        return(NULL)
      }
      
      # Check if files are uploaded based on universal reference mode
      if (input$universal_reference == "dataset2") {
        # Need both files for dataset2 reference
        if (is.null(input$xlsx_file1) || is.null(input$xlsx_file2)) {
          return(NULL)
        }
      } else {
        # Self-reference only needs one file
        if (is.null(input$xlsx_file1)) {
          return(NULL)
        }
      }
      
      tryCatch({
        df1 <- openxlsx::read.xlsx(input$xlsx_file1$datapath, sheet=1)
        df2 <- openxlsx::read.xlsx(input$xlsx_file2$datapath, sheet=1)
        
        # Get custom MDthresh if manual mode is selected
        custom_mdthresh_val <- if (input$mdthresh_mode == "manual") input$custom_mdthresh else NULL
        
        # Get selected columns for multivariate analysis (universal)
        selected_cols <- if (length(input$universal_columns) > 0) input$universal_columns else NULL
        
        # Determine reference dataset based on universal reference selection
        if (input$universal_reference == "self") {
          # Self-reference: use df1 as both datasets
          reference_df <- df1
          cat("Using self-reference (Dataset 1 as both data and reference)\n")
        } else if (input$universal_reference == "dataset1") {
          # Dataset 1 reference: use df1 as reference for df2
          reference_df <- df1
          cat("Using Dataset 1 as reference for Dataset 2\n")
        } else if (input$universal_reference == "dataset2") {
          # Dataset 2 reference: use df2 as reference for df1
          reference_df <- df2
          cat("Using Dataset 2 as reference for Dataset 1\n")
        }
        
        # Determine which dataset to analyze based on universal reference mode
        if (input$universal_reference == "dataset1") {
          # Analyze dataset2 against dataset1 as reference
          data_to_analyze <- df2
          reference_data <- df1
        } else {
          # Analyze dataset1 against reference (either self or dataset2)
          data_to_analyze <- df1
          reference_data <- reference_df
        }
        
        if (input$use_robust_mahalanobis) {
          result <- compute_robust_mahalanobis(data_to_analyze, reference_data, selected_columns = selected_cols)
        } else if (input$use_isolation_forest) {
          result <- compute_isolation_forest(data_to_analyze, reference_data, selected_columns = selected_cols)
        } else {
          result <- compute_mahalanobis_distance(data_to_analyze, reference_data, input$lambda, input$omega, custom_mdthresh = custom_mdthresh_val, selected_columns = selected_cols, mdthresh_mode = input$mdthresh_mode)
        }
        
        rv$mahalanobis_result <- result
        result
      }, error = function(e) {
        # Print error for debugging
        cat("Error in multivariate analysis:", e$message, "\n")
        NULL
      })
    })
    
    output$mahalanobis_info <- renderPrint({
      result <- multivariate_analysis()
      if (is.null(result)) {
        cat("Multivariate analysis not available. Please ensure:\n")
        cat("- Both datasets are loaded\n")
        cat("- Both datasets have at least 2 common numeric columns\n")
        cat("- At least one multivariate method is selected\n")
      } else {
        cat("=== MULTIVARIATE ANALYSIS RESULTS ===\n\n")
        
        if (input$use_robust_mahalanobis) {
          cat("🔍 Robust Mahalanobis Analysis (MCD):\n")
          cat("Method:", result$method, "\n")
          cat("Total points analyzed:", result$total_points, "\n")
          cat("Outlier count (95% threshold):", result$outlier_count, "\n")
          cat("Outlier count (99% threshold):", result$outlier_99, "\n")
          cat("95% threshold value:", round(result$threshold_95, 3), "\n")
          cat("99% threshold value:", round(result$threshold_99, 3), "\n")
          cat("Columns used:", paste(result$common_cols, collapse = ", "), "\n")
          if (!is.null(result$robust_center)) {
            cat("Robust center (first 3 values):", paste(round(result$robust_center[seq_len(min(3, length(result$robust_center)))], 3), collapse = ", "), "\n")
          }
        } else if (input$use_isolation_forest) {
          cat("🌲 Isolation Forest Analysis:\n")
          cat("Method:", result$method, "\n")
          cat("Total points analyzed:", result$total_points, "\n")
          cat("Outlier count:", result$outlier_count, "\n")
          cat("Contamination rate:", result$contamination, "\n")
          cat("Threshold value:", round(result$threshold, 3), "\n")
          cat("Columns used:", paste(result$common_cols, collapse = ", "), "\n")
          cat("Score range:", round(result$score_range[1], 3), "to", round(result$score_range[2], 3), "\n")
          cat("Score mean:", round(result$score_mean, 3), "\n")
          cat("Score std dev:", round(result$score_sd, 3), "\n")
        } else {
          cat("📊 Standard Mahalanobis Distance Analysis:\n")
          cat("Total points analyzed:", result$total_points, "\n")
          cat("Degrees of freedom:", result$df, "\n")
          cat("Columns used:", paste(result$common_cols, collapse = ", "), "\n")
          cat("MDmean:", round(result$MDmean, 3), "\n")
          cat("stdMD:", round(result$stdMD, 3), "\n")
          
          if (input$mdthresh_mode == "manual") {
            cat("Threshold mode: Manual\n")
            cat("Custom MDthresh:", round(result$MDthresh, 3), "\n")
          } else {
            cat("Threshold mode: Automatic (MDthresh=MDmean+√(100/(100+λ-ω))×stdMD)\n")
            cat("λ:", input$lambda, "ω:", input$omega, "\n")
            cat("Calculated MDthresh:", round(result$MDthresh, 3), "\n")
            if (!is.null(result$threshold_formula)) {
              cat("Formula breakdown:", result$threshold_formula, "\n")
            }
          }
          
          cat("\n📈 Threshold Comparison:\n")
          cat("Points above 95% threshold:", result$outlier_95, "\n")
          cat("Points above 99% threshold:", result$outlier_99, "\n")
          cat("Points above custom threshold:", result$outlier_custom, "\n")
          cat("P-value range:", round(min(result$p_values), 4), "to", round(max(result$p_values), 4), "\n")
        }
        
        cat("\n💡 Interpretation:\n")
        if (input$use_robust_mahalanobis) {
          cat("- Robust MCD is less sensitive to outliers in the reference dataset\n")
  
          cat("- Good for non-normal distributions and contaminated data\n")
        } else if (input$use_isolation_forest) {
          cat("- Isolation Forest detects anomalies based on data isolation\n")
          cat("- Threshold automatically set to top contamination% of scores\n")
          cat("- Good for high-dimensional data and non-linear relationships\n")
        } else {
          cat("- Standard Mahalanobis assumes multivariate normal distribution\n")
          if (input$mdthresh_mode == "auto") {
            cat("- λ controls strictness: higher = stricter threshold\n")
            cat("- ω provides flexibility: higher = more lenient threshold\n")
          }

        }
      }
    })
    
    # Preview functions
    output$preview_info1 <- renderText({
      if (length(input$element_A1) == 0 || length(input$element_B1) == 0 || length(input$element_C1) == 0) {
        "Preview 1: Select elements A, B and C for Dataset 1!"
      } else {
        ""
      }
    })
    
    output$preview_info2 <- renderText({
      if (length(input$element_A2) == 0 || length(input$element_B2) == 0 || length(input$element_C2) == 0) {
        "Preview 2: Select elements A, B and C for Dataset 2!"
      } else {
        ""
      }
    })
    
    # Ternary plot previews
    output$ternary_preview1 <- renderPlot({
      req(input$element_A1, input$element_B1, input$element_C1, input$xlsx_file1)
      
      # Use helper function to build parameters
      params <- build_ternary_plot_params(dataset_num = 1, preview = TRUE)
      
      # Call general_ternary_plot with built parameters
      do.call(general_ternary_plot, params)
    })
    
    output$ternary_preview2 <- renderPlot({
      req(input$element_A2, input$element_B2, input$element_C2, input$xlsx_file2)
      
      # Use helper function to build parameters
      params <- build_ternary_plot_params(dataset_num = 2, preview = TRUE)
      
      # Call general_ternary_plot with built parameters
      do.call(general_ternary_plot, params)
    })
    
    # Plot saving functions
    observeEvent(input$plot1, {
      req(input$element_A1, input$element_B1, input$element_C1, input$xlsx_file1)
      
      show_message("Generating Plot 1...", "info")
      
      result <- safe_execute({
        withProgress(message = "Generating Plot 1", value = 0, {
          setProgress(0.1, detail = "Initializing...")
          
            setProgress(0.15, detail = "Loading reference data...")
          
          # Use helper function to build parameters
          params <- build_ternary_plot_params(dataset_num = 1, preview = FALSE)
          
          setProgress(0.25, detail = "Processing element filters...")
          setProgress(0.4, detail = "Preparing plot parameters...")
          setProgress(0.6, detail = "Generating ternary plot...")
          setProgress(0.8, detail = "Saving plot...")
          
          # Call general_ternary_plot with built parameters
          do.call(general_ternary_plot, params)
        })
      }, "Failed to generate Plot 1")
      
      if (!is.null(result)) {
        show_message("Plot 1 generated successfully!", "success")
        
        # Cache filtered data for export functionality
        tryCatch({
          rv$filtered_data1 <- generate_filtered_data_for_export(1)
        }, error = function(e) {
          cat("Warning: Could not cache filtered data for export:", e$message, "\n")
        })
      }
    })
    
    observeEvent(input$plot2, {
      req(input$element_A2, input$element_B2, input$element_C2, input$xlsx_file2)
      
      show_message("Generating Plot 2...", "info")
      
      result <- safe_execute({
        withProgress(message = "Generating Plot 2", value = 0, {
          setProgress(0.1, detail = "Initializing...")
          
            setProgress(0.15, detail = "Loading reference data...")
          
          # Use helper function to build parameters
          params <- build_ternary_plot_params(dataset_num = 2, preview = FALSE)
          
          setProgress(0.25, detail = "Processing element filters...")
          setProgress(0.4, detail = "Preparing plot parameters...")
          setProgress(0.6, detail = "Generating ternary plot...")
          setProgress(0.8, detail = "Saving plot...")
          
          # Call general_ternary_plot with built parameters
          do.call(general_ternary_plot, params)
        })
      }, "Failed to generate Plot 2")
      
      if (!is.null(result)) {
        show_message("Plot 2 generated successfully!", "success")
        
        # Cache filtered data for export functionality
        tryCatch({
          rv$filtered_data2 <- generate_filtered_data_for_export(2)
        }, error = function(e) {
          cat("Warning: Could not cache filtered data for export:", e$message, "\n")
        })
      }
    })
    
    observeEvent(input$plot_both, {
      req(input$element_A1, input$element_B1, input$element_C1, input$xlsx_file1,
          input$element_A2, input$element_B2, input$element_C2, input$xlsx_file2)
      
      show_message("Generating both plots...", "info")
      
      # Generate both plots sequentially
      result1 <- safe_execute({
        withProgress(message = "Generating Plot 1", value = 0, {
          setProgress(0.1, detail = "Initializing Plot 1...")
          
            setProgress(0.15, detail = "Loading reference data...")
          
          # Use helper function to build parameters
          params <- build_ternary_plot_params(dataset_num = 1, preview = FALSE)
          
          setProgress(0.25, detail = "Processing element filters...")
          setProgress(0.4, detail = "Preparing plot parameters...")
          setProgress(0.6, detail = "Generating ternary plot...")
          setProgress(0.8, detail = "Saving Plot 1...")
          
          # Call general_ternary_plot with built parameters
          do.call(general_ternary_plot, params)
        })
      }, "Failed to generate Plot 1")
      
      if (!is.null(result1)) {
        show_message("Plot 1 generated successfully!", "success")
        
        # Now generate Plot 2
        result2 <- safe_execute({
          withProgress(message = "Generating Plot 2", value = 0, {
          setProgress(0.1, detail = "Initializing Plot 2...")
            
            setProgress(0.15, detail = "Loading reference data...")
            
            # Use helper function to build parameters
            params <- build_ternary_plot_params(dataset_num = 2, preview = FALSE)
            
            setProgress(0.25, detail = "Processing element filters...")
            setProgress(0.4, detail = "Preparing plot parameters...")
            setProgress(0.6, detail = "Generating ternary plot...")
            setProgress(0.8, detail = "Saving Plot 2...")
            
            # Call general_ternary_plot with built parameters
            do.call(general_ternary_plot, params)
          })
        }, "Failed to generate Plot 2")
        
        if (!is.null(result2)) {
          show_message("Both plots generated successfully!", "success")
          
          # Cache filtered data for export functionality
          tryCatch({
            rv$filtered_data1 <- generate_filtered_data_for_export(1)
            rv$filtered_data2 <- generate_filtered_data_for_export(2)
          }, error = function(e) {
            cat("Warning: Could not cache filtered data for export:", e$message, "\n")
          })
        }
      }
    })
    
    observeEvent(input$close_app, {
      stopApp()
    })
    
    
    
    # Helper function to build ternary plot parameters (DRY principle)
    build_ternary_plot_params <- function(dataset_num = 1, preview = FALSE) {
      # Determine which dataset inputs to use
      suffix <- if (dataset_num == 1) "1" else "2"
            
            # Get reference data for multivariate analysis if needed
            reference_data <- NULL
      if (input$use_mahalanobis || input$use_robust_mahalanobis || input$use_isolation_forest) {
        # Use universal reference for all multivariate methods
        reference_mode <- input$universal_reference
        
        if (reference_mode == "self") {
          # Self-reference: use current dataset as both data and reference
          reference_data <- get_cached_data(input[[paste0("xlsx_file", suffix)]]$datapath, paste0("dataset", suffix))
          debug_log("DEBUG: Self-reference mode - using dataset %d as reference (%d rows)", dataset_num, if (!is.null(reference_data)) nrow(reference_data) else 0)
        } else if (reference_mode == "dataset1") {
          # Dataset1 reference: use dataset1 as reference
                reference_data <- get_cached_data(input$xlsx_file1$datapath, "dataset1")
          debug_log("DEBUG: Dataset1 reference mode - using dataset1 as reference (%d rows)", if (!is.null(reference_data)) nrow(reference_data) else 0)
        } else if (reference_mode == "dataset2" && !is.null(input$xlsx_file2)) {
          # Dataset2 reference: use dataset2 as reference
                reference_data <- get_cached_data(input$xlsx_file2$datapath, "dataset2")
          debug_log("DEBUG: Dataset2 reference mode - using dataset2 as reference (%d rows)", if (!is.null(reference_data)) nrow(reference_data) else 0)
        } else if (reference_mode == "dataset2" && is.null(input$xlsx_file2)) {
          debug_log("DEBUG: Dataset2 reference mode selected but no dataset2 loaded")
              }
            }
            
            # Get individual filters
      individual_filters_A <- get_individual_filters(input[[paste0("element_A", suffix)]], paste0("A", suffix))
      individual_filters_B <- get_individual_filters(input[[paste0("element_B", suffix)]], paste0("B", suffix))
      individual_filters_C <- get_individual_filters(input[[paste0("element_C", suffix)]], paste0("C", suffix))
            
            # Get custom MDthresh if manual mode is selected
            custom_mdthresh_val <- if (input$mdthresh_mode == "manual") input$custom_mdthresh else NULL
          
      # Build parameter list
      params <- list(
        xlsx_file = input[[paste0("xlsx_file", suffix)]]$datapath,
              working_dir = if (length(working_dir()) > 0) working_dir() else getwd(),
        output_dir = if (preview) NULL else output_dir(),
        element_A = list(col = input[[paste0("element_A", suffix)]]),
        element_B = list(col = input[[paste0("element_B", suffix)]]),
        element_C = list(col = input[[paste0("element_C", suffix)]]),
        optional_param1 = if (input[[paste0("optional_param1_", suffix)]] != "") {
          list(col = input[[paste0("optional_param1_", suffix)]], 
               filter = if (nzchar(input[[paste0("filter_op1_", suffix)]])) input[[paste0("filter_op1_", suffix)]] else NULL)
        } else NULL,
        optional_param2 = if (input[[paste0("optional_param2_", suffix)]] != "") {
          list(col = input[[paste0("optional_param2_", suffix)]], 
               filter = if (nzchar(input[[paste0("filter_op2_", suffix)]])) input[[paste0("filter_op2_", suffix)]] else NULL)
        } else NULL,
        color_palette = input[[paste0("color_palette", suffix)]],
        xlsx_display_name = input[[paste0("xlsx_file", suffix)]]$name,
        preview = preview,
              use_mahalanobis = input$use_mahalanobis || input$use_robust_mahalanobis || input$use_isolation_forest,
              reference_data = reference_data,
        mahalanobis_reference = input$universal_reference,
        selected_columns = if (length(input$universal_columns) > 0) input$universal_columns else NULL,
        optional_param1_representation = input[[paste0("optional_param1_representation", suffix)]],
              output_format = input$output_format,
              use_robust_mahalanobis = input$use_robust_mahalanobis,
              use_isolation_forest = input$use_isolation_forest,
              use_iqr_filter = input$use_iqr_filter,
              use_zscore_filter = input$use_zscore_filter,
              use_mad_filter = input$use_mad_filter,
              lambda = input$lambda,
              omega = input$omega,
              keep_outliers_mahalanobis = input$outlier_mode_mahalanobis,
              keep_outliers_robust = input$outlier_mode_robust,
              keep_outliers_isolation = input$outlier_mode_isolation,

              keep_outliers_iqr = input$outlier_mode_iqr,
              keep_outliers_zscore = input$outlier_mode_zscore,
              keep_outliers_mad = input$outlier_mode_mad,
        individual_filters_A = individual_filters_A,
        individual_filters_B = individual_filters_B,
        individual_filters_C = individual_filters_C,
              custom_mdthresh = custom_mdthresh_val,
        mdthresh_mode = input$mdthresh_mode,

              include_plot_notes = input$include_plot_notes
            )
      
      # Log final reference_data status for debugging
      debug_log("DEBUG: Final reference_data for dataset %d: %s", dataset_num, 
                if (is.null(reference_data)) "NULL" else paste("dataframe with", nrow(reference_data), "rows"))
      
      return(params)
    }
    
    # Analysis outputs
    output$analysis_stats1 <- renderPrint({
      ad <- analyze_data1()
      ad$stats
    })
    
    output$analysis_validation1 <- renderPrint({
      ad <- analyze_data1()
      ad$validation
    })
    
    output$excel_preview1 <- DT::renderDataTable({
      req(input$xlsx_file1)
      openxlsx::read.xlsx(input$xlsx_file1$datapath, sheet = 1)
    })
    
    output$analysis_stats2 <- renderPrint({
      ad <- analyze_data2()
      ad$stats
    })
    
    output$analysis_validation2 <- renderPrint({
      ad <- analyze_data2()
      ad$validation
    })
    
    output$excel_preview2 <- DT::renderDataTable({
      req(input$xlsx_file2)
      openxlsx::read.xlsx(input$xlsx_file2$datapath, sheet = 1)
    })
    
    # Dynamic analysis buttons and outputs
    selected_analysis <- reactiveVal(NULL)
    observeEvent(input$show_stats1, { selected_analysis("stats1") })
    observeEvent(input$show_missing1, { selected_analysis("missing1") })
    observeEvent(input$show_excel1, { selected_analysis("excel1") })
    observeEvent(input$show_stats2, { selected_analysis("stats2") })
    observeEvent(input$show_missing2, { selected_analysis("missing2") })
    observeEvent(input$show_excel2, { selected_analysis("excel2") })
    
    output$analysis_buttons <- renderUI({
      req(input$xlsx_file1, input$xlsx_file2)
      tagList(
        h4("Dataset 1 Analysis:"),
        actionButton("show_stats1", "Descriptive Statistics 1"),
        actionButton("show_missing1", "Missing/Outlier Summary 1"),
        actionButton("show_excel1", "Excel File Preview 1"),
        br(), br(),
        h4("Dataset 2 Analysis:"),
        actionButton("show_stats2", "Descriptive Statistics 2"),
        actionButton("show_missing2", "Missing/Outlier Summary 2"),
        actionButton("show_excel2", "Excel File Preview 2")
      )
    })
    
    output$dynamic_output <- renderUI({
      req(input$xlsx_file1, input$xlsx_file2)
      sel <- selected_analysis()
      if (is.null(sel) || length(sel) != 1) return(NULL)
      switch(sel,
             stats1 = verbatimTextOutput("analysis_stats1"),
             missing1 = verbatimTextOutput("analysis_validation1"),
             excel1 = DT::dataTableOutput("excel_preview1"),
             stats2 = verbatimTextOutput("analysis_stats2"),
             missing2 = verbatimTextOutput("analysis_validation2"),
             excel2 = DT::dataTableOutput("excel_preview2"),
             NULL
      )
    })
    
    # Comparison tab
    selected_comparison <- reactiveVal(NULL)
    observeEvent(input$compare_stats, { selected_comparison("stats") })
    observeEvent(input$compare_mahalanobis, { selected_comparison("mahalanobis") })
    
    output$comparison_buttons <- renderUI({
      req(input$xlsx_file1, input$xlsx_file2)
      tagList(
        actionButton("compare_stats", "Compare Statistics"),
        actionButton("compare_mahalanobis", "Mahalanobis Analysis")
      )
    })
    
    output$comparison_output <- renderUI({
      req(input$xlsx_file1, input$xlsx_file2)
      sel <- selected_comparison()
      if (is.null(sel) || length(sel) != 1) return(NULL)
      switch(sel,
             stats = verbatimTextOutput("comparison_stats"),
             mahalanobis = verbatimTextOutput("comparison_mahalanobis"),
             NULL
      )
    })
    
    output$comparison_stats <- renderPrint({
      ad1 <- analyze_data1()
      ad2 <- analyze_data2()
      cat("=== DATASET 1 STATISTICS ===\n")
      print(ad1$stats)
      cat("\n=== DATASET 2 STATISTICS ===\n")
      print(ad2$stats)
    })
    
    # Data readiness status for Data Comparison tab
    output$data_readiness_status <- renderPrint({
      if (is.null(input$xlsx_file1) || is.null(input$xlsx_file2)) {
        cat("📋 Please upload both datasets to begin comparison.\n")
        return()
      }
      
      tryCatch({
        df1 <- openxlsx::read.xlsx(input$xlsx_file1$datapath, sheet=1)
        df2 <- openxlsx::read.xlsx(input$xlsx_file2$datapath, sheet=1)
        
        # Basic data validation
        numeric_cols1 <- sapply(df1, is.numeric)
        numeric_cols2 <- sapply(df2, is.numeric)
        common_cols <- intersect(colnames(df1)[numeric_cols1], colnames(df2)[numeric_cols2])
        
        cat("=== DATA READINESS STATUS ===\n")
        cat("Dataset 1:", nrow(df1), "rows ×", ncol(df1), "columns\n")
        cat("Dataset 2:", nrow(df2), "rows ×", ncol(df2), "columns\n")
        cat("Common numeric columns:", length(common_cols), "\n")
        
        if (length(common_cols) >= 2) {
          cat("✅ Ready for multivariate analysis\n")
          cat("Available columns:", paste(common_cols, collapse = ", "), "\n")
        } else {
          cat("❌ Need at least 2 common numeric columns\n")
          cat("Dataset 1 numeric:", sum(numeric_cols1), "\n")
          cat("Dataset 2 numeric:", sum(numeric_cols2), "\n")
        }
        
      }, error = function(e) {
        cat("❌ Error reading datasets:", e$message, "\n")
      })
    })
    
    output$comparison_mahalanobis <- renderPrint({
      # Simplified Mahalanobis analysis for Data Comparison tab
      if (is.null(input$xlsx_file1) || is.null(input$xlsx_file2)) {
        cat("Please upload both datasets first.\n")
        return()
      }
      
      tryCatch({
        df1 <- openxlsx::read.xlsx(input$xlsx_file1$datapath, sheet=1)
        df2 <- openxlsx::read.xlsx(input$xlsx_file2$datapath, sheet=1)
        
        # Find common numeric columns
        numeric_cols1 <- sapply(df1, is.numeric)
        numeric_cols2 <- sapply(df2, is.numeric)
        common_cols <- intersect(colnames(df1)[numeric_cols1], colnames(df2)[numeric_cols2])
        
        if (length(common_cols) < 2) {
          cat("❌ Insufficient common numeric columns for Mahalanobis analysis.\n")
          cat("Need at least 2, found:", length(common_cols), "\n")
          cat("Dataset 1 numeric columns:", sum(numeric_cols1), "\n")
          cat("Dataset 2 numeric columns:", sum(numeric_cols2), "\n")
          return()
        }
        
        # Use first 2 common columns for simplicity in comparison tab
        selected_cols <- common_cols[1:min(2, length(common_cols))]
        
        cat("=== MAHALANOBIS DISTANCE ANALYSIS ===\n")
        cat("Using columns:", paste(selected_cols, collapse = ", "), "\n")
        cat("Dataset 1 rows:", nrow(df1), "\n")
        cat("Dataset 2 rows:", nrow(df2), "\n\n")
        
        # Perform basic Mahalanobis analysis
        result <- compute_mahalanobis_distance(
          df1[, selected_cols, drop = FALSE], 
          df2[, selected_cols, drop = FALSE], 
          lambda = 1, 
          omega = 0,
          keep_outliers = FALSE,
          custom_mdthresh = NULL,
          selected_columns = selected_cols,
          mdthresh_mode = "auto",

        )
        
        if (!is.null(result)) {
          cat("✅ Analysis completed successfully!\n\n")
          cat("Threshold method:", result$threshold_method, "\n")
          cat("Threshold value:", round(result$MDthresh, 3), "\n")
          cat("Total points analyzed:", result$total_points, "\n")
          cat("Outliers detected:", result$outlier_count, "\n")
          cat("Outlier percentage:", round(result$outlier_count / result$total_points * 100, 1), "%\n")
          
          if (!is.null(result$threshold_formula)) {
            cat("\nThreshold formula:", result$threshold_formula, "\n")
          }
      } else {
          cat("❌ Analysis failed. Please check data quality.\n")
        }
        
      }, error = function(e) {
        cat("Error in Mahalanobis analysis:", e$message, "\n")
        cat("Please ensure:\n")
        cat("- Both datasets have numeric columns\n")
        cat("- At least 2 common numeric columns exist\n")
        cat("- Data is properly formatted\n")
      })
    })
    
    # Tooltips
    bsTooltip("filter_A1", "Enter a filter, e.g. > 0.5. Leave blank for no filter. Applied to each element individually.", "right", options = list(container = "body"))
    bsTooltip("filter_B1", "Enter a filter, e.g. < 1. Leave blank for no filter. Applied to each element individually.", "right", options = list(container = "body"))
    bsTooltip("filter_C1", "Enter a filter, e.g. == 2. Leave blank for no filter. Applied to each element individually.", "right", options = list(container = "body"))
    bsTooltip("filter_A2", "Enter a filter, e.g. > 0.5. Leave blank for no filter. Applied to each element individually.", "right", options = list(container = "body"))
    bsTooltip("filter_B2", "Enter a filter, e.g. < 1. Leave blank for no filter. Applied to each element individually.", "right", options = list(container = "body"))
    bsTooltip("filter_C2", "Enter a filter, e.g. == 2. Leave blank for no filter. Applied to each element individually.", "right", options = list(container = "body"))
    bsTooltip("optional_param1_representation1", "Choose how to represent Optional Param 1: Point Size (variable size) or Point Type (different shapes).", "right", options = list(container = "body"))
    
    # Help modal content
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
            tags$li(strong("Robust Mahalanobis (MCD):"), "Uses robust covariance estimation, less sensitive to outliers"),
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
          )
        )
      ))
    })
    
    # Multiple Ternary Creator server logic
    # Update column choices when files are selected
    observe({
      req(input$multiple_xlsx_files)
      
      # Read the first file to get column names
      if (length(input$multiple_xlsx_files$datapath) > 0) {
        tryCatch({
          df <- openxlsx::read.xlsx(input$multiple_xlsx_files$datapath[1], sheet = 1)
          col_names <- colnames(df)
          
          # Update element dropdowns
          updateSelectizeInput(session, "multiple_element_A", choices = col_names, selected = NULL)
          updateSelectizeInput(session, "multiple_element_B", choices = col_names, selected = NULL)
          updateSelectizeInput(session, "multiple_element_C", choices = col_names, selected = NULL)
          
          # Update optional parameter dropdowns
          updateSelectizeInput(session, "multiple_optional_param1", choices = col_names, selected = NULL)
          updateSelectizeInput(session, "multiple_optional_param2", choices = col_names, selected = NULL)
          

          
        }, error = function(e) {
          show_message(paste("Error reading file:", e$message), "error")
        })
      }
    })
    
    # Dynamic filter UI for elements
    output$multiple_filters_A <- renderUI({
      req(input$multiple_element_A)
      lapply(input$multiple_element_A, function(element) {
        textInput(
          inputId = paste0("multiple_filter_A_", gsub("[^A-Za-z0-9]", "_", element)),
          label = paste("Filter for", element),
          placeholder = "e.g., > 0.5, < 100"
        )
      })
    })
    
    output$multiple_filters_B <- renderUI({
      req(input$multiple_element_B)
      lapply(input$multiple_element_B, function(element) {
        textInput(
          inputId = paste0("multiple_filter_B_", gsub("[^A-Za-z0-9]", "_", element)),
          label = paste("Filter for", element),
          placeholder = "e.g., > 0.5, < 100"
        )
      })
    })
    
    output$multiple_filters_C <- renderUI({
      req(input$multiple_element_C)
      lapply(input$multiple_element_C, function(element) {
        textInput(
          inputId = paste0("multiple_filter_C_", gsub("[^A-Za-z0-9]", "_", element)),
          label = paste("Filter for", element),
          placeholder = "e.g., > 0.5, < 100"
        )
      })
    })
    
    # Dynamic filter UI for optional parameters
    output$multiple_optional_param1_filter <- renderUI({
      req(input$multiple_optional_param1)
      lapply(input$multiple_optional_param1, function(element) {
        textInput(
          inputId = paste0("multiple_filter_op1_", gsub("[^A-Za-z0-9]", "_", element)),
          label = paste("Filter for", element),
          placeholder = "e.g., > 0.5, < 100"
        )
      })
    })
    
    output$multiple_optional_param2_filter <- renderUI({
      req(input$multiple_optional_param2)
      lapply(input$multiple_optional_param2, function(element) {
        textInput(
          inputId = paste0("multiple_filter_op2_", gsub("[^A-Za-z0-9]", "_", element)),
          label = paste("Filter for", element),
          placeholder = "e.g., > 0.5, < 100"
        )
      })
    })
    
    # Create multiple ternary plots (PREVIEW ONLY - no saving)
    observeEvent(input$create_multiple_ternary, {
      req(input$multiple_xlsx_files, input$multiple_element_A, input$multiple_element_B, input$multiple_element_C)
      
      show_message("Creating multiple ternary plots (preview mode)...", "info")
      
      # Collect all filters using helper functions
      filters <- collect_all_multiple_filters(input)
      
      # Print debug information
      print_multiple_debug_info(input, filters, "preview")
      
      # Create plots for each file (PREVIEW ONLY)
      withProgress(message = "Creating ternary plots (preview)", value = 0, {
        total_files <- length(input$multiple_xlsx_files$datapath)
        
        for (i in seq_along(input$multiple_xlsx_files$datapath)) {
          setProgress(i / total_files, detail = paste("Processing file", i, "of", total_files))
          
          tryCatch({
            general_ternary_plot(
              xlsx_file = input$multiple_xlsx_files$datapath[i],
              working_dir = if (length(working_dir()) > 0) working_dir() else getwd(),
              output_dir = NULL,  # Keep as NULL for preview
              element_A = list(col = input$multiple_element_A),
              element_B = list(col = input$multiple_element_B),
              element_C = list(col = input$multiple_element_C),
              optional_param1 = if (length(input$multiple_optional_param1) > 0) list(col = input$multiple_optional_param1, filter = filters$optional_param1_filters, representation = input$multiple_optional_param1_representation) else NULL,
              optional_param2 = if (length(input$multiple_optional_param2) > 0) list(col = input$multiple_optional_param2, filter = filters$optional_param2_filters) else NULL,
              color_palette = input$multiple_color_palette,
              xlsx_display_name = input$multiple_xlsx_files$name[i],
              preview = TRUE,      # Set to TRUE for preview mode
              use_mahalanobis = FALSE,  # Disabled for multiple ternary creator
              use_robust_mahalanobis = FALSE,  # Disabled for multiple ternary creator
              use_isolation_forest = FALSE,  # Disabled for multiple ternary creator
              use_iqr_filter = FALSE,  # Disabled for multiple ternary creator
              use_zscore_filter = FALSE,  # Disabled for multiple ternary creator
              use_mad_filter = FALSE,  # Disabled for multiple ternary creator
              lambda = 1,  # Default value
              omega = 0,   # Default value
              keep_outliers_mahalanobis = FALSE,  # Disabled for multiple ternary creator
              keep_outliers_robust = FALSE,       # Disabled for multiple ternary creator
              keep_outliers_isolation = FALSE,    # Disabled for multiple ternary creator
              keep_outliers_iqr = FALSE,  # Disabled for multiple ternary creator
              keep_outliers_zscore = FALSE,  # Disabled for multiple ternary creator
              keep_outliers_mad = FALSE,  # Disabled for multiple ternary creator
              individual_filters_A = filters$individual_filters_A,
              individual_filters_B = filters$individual_filters_B,
              individual_filters_C = filters$individual_filters_C,
              custom_mdthresh = NULL,  # Disabled since UI removed
              # Inclusion area feature removed
              include_plot_notes = TRUE
            )
          }, error = function(e) {
            show_message(paste("Error processing file", input$multiple_xlsx_files$name[i], ":", e$message), "error")
          })
        }
      })
      
      show_message("Multiple ternary plots created successfully (preview mode)!", "success")
    })
    
    # Save multiple ternary plots to folder
    observeEvent(input$save_multiple_ternary, {
      req(input$multiple_xlsx_files, input$multiple_output_folder)
      
      # Check if output directory is set
      if (length(output_dir()) == 0) {
        show_message("Please set an output directory in 'Directory Settings' section first", "error")
        return()
      }
      
      # Create subfolder inside the global output directory
      plots_subfolder <- file.path(output_dir(), input$multiple_output_folder)
      if (!dir.exists(plots_subfolder)) {
        dir.create(plots_subfolder, recursive = TRUE)
      }
      
      show_message("Saving multiple ternary plots to subfolder...", "info")
      
      show_message(paste("Plots will be saved to:", plots_subfolder), "info")
      
      # Now recreate the plots with the output directory
      if (length(input$multiple_element_A) > 0 && length(input$multiple_element_B) > 0 && length(input$multiple_element_C) > 0) {
        show_message("Recreating plots with save directory...", "info")
        
        # Collect all filters using helper functions
        filters <- collect_all_multiple_filters(input)
        
        # Print debug information
        print_multiple_debug_info(input, filters, "save")
        
        # Create plots for each file with output directory
        withProgress(message = "Saving ternary plots", value = 0, {
          total_files <- length(input$multiple_xlsx_files$datapath)
          
          for (i in seq_along(input$multiple_xlsx_files$datapath)) {
            setProgress(i / total_files, detail = paste("Saving file", i, "of", total_files))
            
            tryCatch({
              general_ternary_plot(
                xlsx_file = input$multiple_xlsx_files$datapath[i],
                working_dir = if (length(working_dir()) > 0) working_dir() else getwd(),
                output_dir = plots_subfolder,
                output_format = input$multiple_output_format,
                element_A = list(col = input$multiple_element_A),
                element_B = list(col = input$multiple_element_B),
                element_C = list(col = input$multiple_element_C),
                optional_param1 = if (length(input$multiple_optional_param1) > 0) list(col = input$multiple_optional_param1, filter = filters$optional_param1_filters, representation = input$multiple_optional_param1_representation) else NULL,
                optional_param2 = if (length(input$multiple_optional_param2) > 0) list(col = input$multiple_optional_param2, filter = filters$optional_param2_filters) else NULL,
                color_palette = input$multiple_color_palette,
                xlsx_display_name = input$multiple_xlsx_files$name[i],
                preview = FALSE,
                use_mahalanobis = FALSE,  # Disabled for multiple ternary creator
                use_robust_mahalanobis = FALSE,  # Disabled for multiple ternary creator
                use_isolation_forest = FALSE,  # Disabled for multiple ternary creator
                use_iqr_filter = FALSE,  # Disabled for multiple ternary creator
                use_zscore_filter = FALSE,  # Disabled for multiple ternary creator
                use_mad_filter = FALSE,  # Disabled for multiple ternary creator
                lambda = 1,  # Default value
                omega = 0,   # Default value
                keep_outliers_mahalanobis = FALSE,  # Disabled for multiple ternary creator
                keep_outliers_robust = FALSE,       # Disabled for multiple ternary creator
                keep_outliers_isolation = FALSE,    # Disabled for multiple ternary creator
                keep_outliers_iqr = FALSE,  # Disabled for multiple ternary creator
                keep_outliers_zscore = FALSE,  # Disabled for multiple ternary creator
                keep_outliers_mad = FALSE,  # Disabled for multiple ternary creator
                individual_filters_A = filters$individual_filters_A,
                individual_filters_B = filters$individual_filters_B,
                individual_filters_C = filters$individual_filters_C,
                custom_mdthresh = NULL,  # Disabled since UI removed
                # Inclusion area feature removed
                include_plot_notes = TRUE
              )
            }, error = function(e) {
              show_message(paste("Error saving plot for file", input$multiple_xlsx_files$name[i], ":", e$message), "error")
            })
          }
        })
        
        show_message("All ternary plots saved successfully!", "success")
      } else {
        show_message("Please create the plots first before saving", "warning")
      }
    })
    
    # Cache management server logic
    observeEvent(input$clear_cache, {
      clear_all_cache()
      show_message("All cache cleared successfully!", "success")
    })
    
    observeEvent(input$clear_expired_cache, {
      clear_expired_cache()
      show_message("Expired cache entries cleared!", "success")
    })
    
    observeEvent(input$refresh_cache_stats, {
      # Force refresh of cache statistics
      output$cache_stats <- renderText({
        get_cache_stats()
      })
      show_message("Cache statistics refreshed!", "info")
    })
    
    output$cache_stats <- renderText({
      get_cache_stats()
    })
    
    # Debug mode toggle
    observeEvent(input$debug_mode, {
      options(ternary.debug = input$debug_mode)
      if (input$debug_mode) {
        show_message("Debug mode enabled - detailed output will be shown in console", "info")
      } else {
        show_message("Debug mode disabled", "info")
      }
    })
    

    
    # Check multivariate data readiness

    
    # Filtered data status output
    output$filtered_data_status <- renderUI({
      status_text <- ""
      status_color <- "black"
      
      if (!is.null(rv$filtered_data1)) {
        rows1 <- nrow(rv$filtered_data1)
        original1 <- if (!is.null(rv$df1)) nrow(rv$df1) else "unknown"
        status_text <- paste0("Dataset 1: ", rows1, " rows (filtered from ", original1, " original)")
        status_color <- "green"
      } else {
        status_text <- "Dataset 1: No filtered data available (generate a plot first)"
        status_color <- "red"
      }
      
      if (!is.null(rv$filtered_data2)) {
        rows2 <- nrow(rv$filtered_data2)
        original2 <- if (!is.null(rv$df2)) nrow(rv$df2) else "unknown"
        status_text <- paste0(status_text, "\nDataset 2: ", rows2, " rows (filtered from ", original2, " original)")
      } else {
        status_text <- paste0(status_text, "\nDataset 2: No filtered data available (generate a plot first)")
      }
      
      div(
        style = paste0("color: ", status_color, "; font-weight: bold; padding: 10px; border: 1px solid #ddd; border-radius: 5px; background-color: #f9f9f9;"),
        pre(status_text)
      )
    })
    
    # Data Export server logic
    observeEvent(input$export_all, {
      req(input$xlsx_file1)
      
      show_message("Starting comprehensive export...", "info")
      
      # Initialize export tracking
      export_results <- list()
      export_dir <- if (length(working_dir()) > 0) working_dir() else getwd()
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      
      # Create export subfolder in output directory, not working directory
      export_folder <- file.path(if (length(output_dir()) > 0) output_dir() else export_dir, paste0("export_", timestamp))
      if (!dir.exists(export_folder)) {
        dir.create(export_folder, recursive = TRUE)
      }
      
      # Export filtered data
      if (input$export_filtered_data) {
        tryCatch({
          show_message("Exporting filtered data...", "info")
          
          # Generate filtered data for both datasets if available
          if (!is.null(input$xlsx_file1)) {
            df1 <- generate_filtered_data_for_export(1)
            filename1 <- paste0("filtered_data_dataset1_", timestamp, ".", input$export_format)
            filepath1 <- file.path(export_folder, filename1)
          
          if (input$export_format == "xlsx") {
              openxlsx::write.xlsx(df1, filepath1)
          } else if (input$export_format == "csv") {
              write.csv(df1, filepath1, row.names = FALSE)
          } else if (input$export_format == "rds") {
              saveRDS(df1, filepath1)
            }
            
            export_results$filtered_data1 <- list(filename = filename1, path = filepath1, rows = nrow(df1), cols = ncol(df1))
            show_message(paste("Dataset 1 filtered data exported:", filename1), "success")
          }
          
          if (!is.null(input$xlsx_file2)) {
            df2 <- generate_filtered_data_for_export(2)
            filename2 <- paste0("filtered_data_dataset2_", timestamp, ".", input$export_format)
            filepath2 <- file.path(export_folder, filename2)
            
            if (input$export_format == "xlsx") {
              openxlsx::write.xlsx(df2, filepath2)
            } else if (input$export_format == "csv") {
              write.csv(df2, filepath2, row.names = FALSE)
            } else if (input$export_format == "rds") {
              saveRDS(df2, filepath2)
            }
            
            export_results$filtered_data2 <- list(filename = filename2, path = filepath2, rows = nrow(df2), cols = ncol(df2))
            show_message(paste("Dataset 2 filtered data exported:", filename2), "success")
          }
          
        }, error = function(e) {
          show_message(paste("Filtered data export error:", e$message), "error")
          export_results$filtered_data_error <- e$message
        })
      }
      
      # Export statistics
      if (input$export_stats) {
        tryCatch({
          show_message("Exporting statistical results...", "info")
          
          # Collect available statistics
          stats_data <- list()
          
          # Basic dataset statistics
          if (!is.null(input$xlsx_file1)) {
            df1 <- openxlsx::read.xlsx(input$xlsx_file1$datapath, sheet = 1)
            stats_data$dataset1_summary <- list(
              rows = nrow(df1),
              columns = ncol(df1),
              numeric_columns = sum(sapply(df1, is.numeric)),
              missing_values = sum(is.na(df1))
            )
          }
          
          if (!is.null(input$xlsx_file2)) {
            df2 <- openxlsx::read.xlsx(input$xlsx_file2$datapath, sheet = 1)
            stats_data$dataset2_summary <- list(
              rows = nrow(df2),
              columns = ncol(df2),
              numeric_columns = sum(sapply(df2, is.numeric)),
              missing_values = sum(is.na(df2))
            )
          }
          
          # Export statistics
          filename <- paste0("statistical_summary_", timestamp, ".xlsx")
          filepath <- file.path(export_folder, filename)
          
          # Create a workbook with multiple sheets
          wb <- openxlsx::createWorkbook()
          
          # Summary sheet
          openxlsx::addWorksheet(wb, "Summary")
          summary_data <- data.frame(
            Metric = c("Export Timestamp", "Total Files", "Export Format"),
            Value = c(timestamp, length(export_results), input$export_format)
          )
          openxlsx::writeData(wb, "Summary", summary_data)
          
          # Dataset statistics
          if (!is.null(stats_data$dataset1_summary)) {
            openxlsx::addWorksheet(wb, "Dataset1_Stats")
            stats1_df <- data.frame(
              Metric = names(unlist(stats_data$dataset1_summary)),
              Value = unlist(stats_data$dataset1_summary)
            )
            openxlsx::writeData(wb, "Dataset1_Stats", stats1_df)
          }
          
          if (!is.null(stats_data$dataset2_summary)) {
            openxlsx::addWorksheet(wb, "Dataset2_Stats")
            stats2_df <- data.frame(
              Metric = names(unlist(stats_data$dataset2_summary)),
              Value = unlist(stats_data$dataset2_summary)
            )
            openxlsx::writeData(wb, "Dataset2_Stats", stats2_df)
          }
          
          openxlsx::saveWorkbook(wb, filepath, overwrite = TRUE)
          export_results$statistics <- list(filename = filename, path = filepath)
          show_message(paste("Statistics exported:", filename), "success")
          
        }, error = function(e) {
          show_message(paste("Statistics export error:", e$message), "error")
          export_results$statistics_error <- e$message
        })
      }
      
      # Export correlations
      if (input$export_correlations) {
        tryCatch({
          show_message("Exporting correlation analysis...", "info")
          
          if (!is.null(input$xlsx_file1)) {
            df1 <- openxlsx::read.xlsx(input$xlsx_file1$datapath, sheet = 1)
            numeric_cols1 <- sapply(df1, is.numeric)
            
            if (sum(numeric_cols1) >= 2) {
              cor_matrix1 <- cor(df1[, numeric_cols1], use = "complete.obs")
              filename1 <- paste0("correlations_dataset1_", timestamp, ".xlsx")
              filepath1 <- file.path(export_folder, filename1)
              openxlsx::write.xlsx(cor_matrix1, filepath1)
              export_results$correlations1 <- list(filename = filename1, path = filepath1)
              show_message(paste("Dataset 1 correlations exported:", filename1), "success")
            }
          }
          
          if (!is.null(input$xlsx_file2)) {
            df2 <- openxlsx::read.xlsx(input$xlsx_file2$datapath, sheet = 1)
            numeric_cols2 <- sapply(df2, is.numeric)
            
            if (sum(numeric_cols2) >= 2) {
              cor_matrix2 <- cor(df2[, numeric_cols2], use = "complete.obs")
              filename2 <- paste0("correlations_dataset2_", timestamp, ".xlsx")
              filepath2 <- file.path(export_folder, filename2)
              openxlsx::write.xlsx(cor_matrix2, filepath2)
              export_results$correlations2 <- list(filename = filename2, path = filepath2)
              show_message(paste("Dataset 2 correlations exported:", filename2), "success")
            }
          }
          
        }, error = function(e) {
          show_message(paste("Correlations export error:", e$message), "error")
          export_results$correlations_error <- e$message
        })
      }
      
      # Export outlier analysis
      if (input$export_outliers) {
        tryCatch({
          show_message("Exporting outlier analysis...", "info")
          
          # Create outlier summary
          outlier_summary <- data.frame(
            Analysis_Type = c("Mahalanobis Distance", "Robust Mahalanobis", "Isolation Forest", "IQR Filter", "Z-Score Filter", "MAD Filter"),
            Available = c("Available", "Available", "Available", "Available", "Available", "Available"),
            Status = c("Ready for analysis", "Ready for analysis", "Ready for analysis", "Ready for analysis", "Ready for analysis", "Ready for analysis")
          )
          
          filename <- paste0("outlier_analysis_summary_", timestamp, ".xlsx")
          filepath <- file.path(export_folder, filename)
          openxlsx::write.xlsx(outlier_summary, filepath)
          
          export_results$outliers <- list(filename = filename, path = filepath)
          show_message(paste("Outlier analysis summary exported:", filename), "success")
          
        }, error = function(e) {
          show_message(paste("Outlier analysis export error:", e$message), "error")
          export_results$outliers_error <- e$message
        })
      }
      
      # Export plots (if any are available)
      if (input$export_plots) {
        tryCatch({
          show_message("Exporting available plots...", "info")
          
          plot_exported <- FALSE
          
          # Check for available plots in reactive values
          if (!is.null(rv$scatter_plot)) {
            filename <- paste0("scatter_plot_", timestamp, ".", input$plot_export_format)
            filepath <- file.path(export_folder, filename)
            
            if (input$plot_export_format == "png") {
              png(filepath, width = 800, height = 600, res = 150)
              print(rv$scatter_plot)
              dev.off()
            } else if (input$plot_export_format == "pdf") {
              pdf(filepath, width = 8, height = 6)
              print(rv$scatter_plot)
              dev.off()
            } else if (input$plot_export_format == "svg") {
              svg(filepath, width = 8, height = 6)
              print(rv$scatter_plot)
              dev.off()
            }
            
            export_results$scatter_plot <- list(filename = filename, path = filepath)
            plot_exported <- TRUE
            show_message(paste("Scatter plot exported:", filename), "success")
          }
          
          if (!is.null(rv$histogram_plot)) {
            filename <- paste0("histogram_plot_", timestamp, ".", input$plot_export_format)
            filepath <- file.path(export_folder, filename)
            
            if (input$plot_export_format == "png") {
              png(filepath, width = 800, height = 600, res = 150)
              print(rv$histogram_plot)
              dev.off()
            } else if (input$plot_export_format == "pdf") {
              pdf(filepath, width = 8, height = 6)
              print(rv$histogram_plot)
              dev.off()
            } else if (input$plot_export_format == "svg") {
              svg(filepath, width = 8, height = 6)
              print(rv$histogram_plot)
              dev.off()
            }
            
            export_results$histogram_plot <- list(filename = filename, path = filepath)
            plot_exported <- TRUE
            show_message(paste("Histogram plot exported:", filename), "success")
          }
          
          if (!is.null(rv$boxplot_plot)) {
            filename <- paste0("boxplot_plot_", timestamp, ".", input$plot_export_format)
            filepath <- file.path(export_folder, filename)
            
            if (input$plot_export_format == "png") {
              png(filepath, width = 800, height = 600, res = 150)
              print(rv$boxplot_plot)
              dev.off()
            } else if (input$plot_export_format == "pdf") {
              pdf(filepath, width = 8, height = 6)
              print(rv$boxplot_plot)
              dev.off()
            } else if (input$plot_export_format == "svg") {
              svg(filepath, width = 8, height = 6)
              print(rv$boxplot_plot)
              dev.off()
            }
            
            export_results$boxplot_plot <- list(filename = filename, path = filepath)
            plot_exported <- TRUE
            show_message(paste("Box plot exported:", filename), "success")
          }
          
          if (!plot_exported) {
            show_message("No plots available for export. Create plots first in the Multiple Plot Types tab.", "warning")
          }
          
        }, error = function(e) {
          show_message(paste("Plot export error:", e$message), "error")
          export_results$plots_error <- e$message
        })
      }
      
      # Export project (complete project export)
      if (input$export_project) {
        tryCatch({
          show_message("Exporting complete project...", "info")
          
          # Create project summary
          project_summary <- list(
            export_timestamp = timestamp,
            export_format = input$export_format,
            plot_format = input$plot_export_format,
            datasets = list(
              dataset1 = if (!is.null(input$xlsx_file1)) input$xlsx_file1$name else NULL,
              dataset2 = if (!is.null(input$xlsx_file2)) input$xlsx_file2$name else NULL
            ),
            export_contents = names(export_results),
            working_directory = working_dir(),
            export_folder = export_folder
          )
          
          # Save project summary
          filename <- paste0("project_summary_", timestamp, ".rds")
          filepath <- file.path(export_folder, filename)
          saveRDS(project_summary, filepath)
          
          export_results$project_summary <- list(filename = filename, path = filepath)
          show_message(paste("Project summary exported:", filename), "success")
          
        }, error = function(e) {
          show_message(paste("Project export error:", e$message), "error")
          export_results$project_error <- e$message
        })
      }
      
      # Store export results and update UI
      rv$last_export_results <- export_results
      rv$last_export_folder <- export_folder
      
      # Create export summary
      summary_msg <- paste("Export completed successfully!\n",
                          "Files saved to:", export_folder, "\n",
                          "Total files exported:", length(export_results))
      
      show_message(summary_msg, "success")
      
      # Update export status
      output$export_status <- renderPrint({
        cat("=== EXPORT STATUS ===\n")
        cat("Status: Completed\n")
        cat("Export folder:", export_folder, "\n")
        cat("Files exported:", length(export_results), "\n")
        cat("Timestamp:", timestamp, "\n\n")
        
        if (length(export_results) > 0) {
          cat("Exported items:\n")
          for (i in seq_along(export_results)) {
            item_name <- names(export_results)[i]
            item <- export_results[[i]]
            if (is.list(item) && !is.null(item$filename)) {
              cat("-", item_name, ":", item$filename, "\n")
            }
          }
        }
      })
      
      # Update export history
      current_history <- rv$export_history
      if (is.null(current_history)) current_history <- list()
      
      new_entry <- list(
        timestamp = timestamp,
        folder = export_folder,
        items = names(export_results),
        format = input$export_format
      )
      
      rv$export_history <- c(list(new_entry), current_history)
      
    })
    
    # Export selected items
    observeEvent(input$export_selected, {
      req(input$xlsx_file1)
      
      show_message("Exporting selected items...", "info")
      
      # This would allow users to select specific items to export
      # For now, just show a message
      show_message("Selective export functionality will be implemented in the next version. Use 'Export All Selected' for now.", "info")
    })
    
    # Export status output
    output$export_status <- renderPrint({
      if (is.null(rv$last_export_results)) {
        cat("=== EXPORT STATUS ===\n")
        cat("Status: No exports performed yet\n")
        cat("Use 'Export All Selected' to start exporting data\n")
      } else {
        cat("=== EXPORT STATUS ===\n")
        cat("Status: Last export completed\n")
        cat("Export folder:", rv$last_export_folder, "\n")
        cat("Files exported:", length(rv$last_export_results), "\n")
        cat("Last export:", names(rv$last_export_results), "\n")
      }
    })
    
    # Filtered data status
    output$filtered_data_status <- renderUI({
      if (is.null(input$xlsx_file1)) {
        div(style = "color: #dc3545;", "No dataset loaded")
      } else {
        tryCatch({
          df1 <- openxlsx::read.xlsx(input$xlsx_file1$datapath, sheet = 1)
          df2 <- if (!is.null(input$xlsx_file2)) openxlsx::read.xlsx(input$xlsx_file2$datapath, sheet = 1) else NULL
          
          div(
            h6("Dataset 1:", style = "margin: 0;"),
            p(paste("Rows:", nrow(df1), "| Columns:", ncol(df1)), style = "margin: 2px 0; font-size: 0.9em;"),
            if (!is.null(df2)) {
              div(
                h6("Dataset 2:", style = "margin: 0;"),
                p(paste("Rows:", nrow(df2), "| Columns:", ncol(df2)), style = "margin: 2px 0; font-size: 0.9em;")
              )
            }
          )
        }, error = function(e) {
          div(style = "color: #dc3545;", paste("Error reading data:", e$message))
        })
      }
    })
    
    # Download links
    output$download_links <- renderUI({
      if (is.null(rv$last_export_results)) {
        div(style = "color: #6c757d;", "No exports available for download")
      } else {
        export_folder <- rv$last_export_folder
        if (!is.null(export_folder) && dir.exists(export_folder)) {
          files <- list.files(export_folder, full.names = TRUE)
          if (length(files) > 0) {
            div(
              h6("Available files:"),
              lapply(files, function(file) {
                filename <- basename(file)
                div(
                  style = "margin: 2px 0; padding: 2px; background-color: #f8f9fa; border-radius: 3px;",
                  paste("📁", filename)
                )
              })
            )
          } else {
            div(style = "color: #6c757d;", "No files found in export folder")
          }
        } else {
          div(style = "color: #6c757d;", "Export folder not accessible")
        }
      }
    })
    
    # Export history
    output$export_history <- renderPrint({
      if (is.null(rv$export_history) || length(rv$export_history) == 0) {
        cat("=== EXPORT HISTORY ===\n")
        cat("No export history available\n")
      } else {
        cat("=== EXPORT HISTORY ===\n")
        for (i in seq_along(rv$export_history)) {
          entry <- rv$export_history[[i]]
          cat("Export", i, ":", entry$timestamp, "\n")
          cat("  Folder:", entry$folder, "\n")
          cat("  Items:", paste(entry$items, collapse = ", "), "\n")
          cat("  Format:", entry$format, "\n")
          cat("  ---\n")
        }
      }
    })
  }
  
  shinyApp(ui, server)
}

# ---- Launch the App ----
run_ternary_gui()


# ---- Comprehensive Testing and Validation System ----
run_system_tests <- function() {
  cat("=== Running System Tests ===\n")
  test_results <- list()
  
  # Test 1: Package availability
  cat("Test 1: Checking package availability...\n")
  test_results$packages <- all(required_packages %in% installed.packages()[,"Package"])
  if (test_results$packages) {
    cat("✅ All required packages are available\n")
  } else {
    cat("❌ Some required packages are missing\n")
    missing_pkgs <- setdiff(required_packages, installed.packages()[,"Package"])
    cat("Missing packages:", paste(missing_pkgs, collapse = ", "), "\n")
  }
  
  # Test 2: Configuration validation
  cat("Test 2: Validating configuration...\n")
  test_results$config <- tryCatch({
    validate_and_fix_config(app_config)
    TRUE
  }, error = function(e) {
    cat("❌ Configuration validation failed:", e$message, "\n")
    FALSE
  })
  if (test_results$config) cat("✅ Configuration is valid\n")
  
  # Test 3: Directory permissions
  cat("Test 3: Checking directory permissions...\n")
  test_results$directories <- tryCatch({
    dir.create(file.path(tempdir(), "test_dir"), showWarnings = FALSE)
    file.create(file.path(tempdir(), "test_dir", "test_file"))
    unlink(file.path(tempdir(), "test_dir"), recursive = TRUE)
    TRUE
  }, error = function(e) {
    cat("❌ Directory permission test failed:", e$message, "\n")
    FALSE
  })
  if (test_results$directories) cat("✅ Directory permissions are working\n")
  
  # Test 4: Function availability
  cat("Test 4: Checking function availability...\n")
  required_functions <- c("validate_data", "generate_stats", "compute_correlation", 
                          "check_data_quality", "compute_mahalanobis_distance")
  test_results$functions <- all(sapply(required_functions, exists))
  if (test_results$functions) {
    cat("✅ All required functions are available\n")
  } else {
    cat("❌ Some required functions are missing\n")
    missing_funcs <- required_functions[!sapply(required_functions, exists)]
    cat("Missing functions:", paste(missing_funcs, collapse = ", "), "\n")
  }
  
  # Test 5: Data processing capabilities
  cat("Test 5: Testing data processing...\n")
  test_results$data_processing <- tryCatch({
    test_data <- data.frame(
      x = 1:100,
      y = rnorm(100),
      z = sample(letters[1:5], 100, replace = TRUE)
    )
    
    # Test validation
    val_result <- validate_data_enhanced(test_data, c("x", "y"))
    if (!val_result$valid) stop("Data validation failed")
    
    # Test statistics
    stats_result <- generate_stats(test_data, c("x", "y"))
    if (is.null(stats_result)) stop("Statistics generation failed")
    
    # Test correlation
    cor_result <- compute_correlation(test_data, c("x", "y"))
    if (is.null(cor_result)) stop("Correlation computation failed")
    
    TRUE
  }, error = function(e) {
    cat("❌ Data processing test failed:", e$message, "\n")
    FALSE
  })
  if (test_results$data_processing) cat("✅ Data processing is working\n")
  
  # Test 6: Performance optimization
  cat("Test 6: Testing performance optimization...\n")
  test_results$performance <- tryCatch({
    large_data <- data.frame(
      x = 1:15000,
      y = rnorm(15000),
      z = sample(letters[1:5], 15000, replace = TRUE)
    )
    
            opt_result <- optimize_for_large_datasets(large_data, max_rows = 100000)
            if (nrow(opt_result$data) != 100000) stop("Performance optimization failed")
    
    TRUE
  }, error = function(e) {
    cat("❌ Performance optimization test failed:", e$message, "\n")
    FALSE
  })
  if (test_results$performance) cat("✅ Performance optimization is working\n")
  
  # Summary
  cat("\n=== Test Summary ===\n")
  passed_tests <- sum(unlist(test_results))
  total_tests <- length(test_results)
  
  cat(sprintf("Tests passed: %d/%d (%.1f%%)\n", passed_tests, total_tests, (passed_tests/total_tests)*100))
  
  if (passed_tests == total_tests) {
    cat("🎉 All tests passed! The system is ready to use.\n")
  } else {
    cat("⚠️ Some tests failed. Please check the issues above.\n")
  }
  
  return(test_results)
}

# Run tests if called directly
if (interactive()) {
  cat("System ready. Run 'run_system_tests()' to validate the installation.\n")
} 
