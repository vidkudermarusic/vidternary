# =============================================================================
# vidternary: Caching System Module
# =============================================================================
# 
# Package:     vidternary
# Version:     1.0.0
# Author:      Vid Kuder Marušič <vidkm30@gmail.com>
# Maintainer:  Vid Kuder Marušič <vidkm30@gmail.com>
# License:     MIT + file LICENSE
# Repository:  https://github.com/vidkudermarusic/vidternary
# 
# Description: Comprehensive caching system for performance optimization including
#              data caching, plot result caching, automatic expiration, and
#              performance monitoring.
# 
# Key Functions:
#   - get_cached_data(): Retrieve cached data
#   - cache_result(): Store results in cache
#   - clear_expired_cache(): Clean up expired cache entries
#   - get_cache_stats(): Monitor cache usage and performance
#   - start_progress(): Progress monitoring functions
# 
# Dependencies:
#   - R (>= 4.0.0)
#   - digest
# 
# Last Modified: 2025-09-07
# 
# =============================================================================

# Global cache environment
ternary_cache <- new.env()
cache_timeout <- 300  # 5 minutes in seconds

# Debug Mode Control
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

# ---- Performance Monitoring System ----
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

# Enhanced performance monitoring with detailed metrics
start_performance_monitor_enhanced <- function(operation_name, include_memory = TRUE, include_system = TRUE) {
  if (!exists("performance_monitors", envir = .GlobalEnv)) {
    .GlobalEnv$performance_monitors <- new.env()
  }
  
  monitor_info <- list(
    start_time = Sys.time(),
    operation_name = operation_name
  )
  
  if (include_memory) {
    monitor_info$memory_start <- gc(reset = TRUE)
  }
  
  if (include_system) {
    monitor_info$system_start <- list(
      cpu_time = proc.time(),
      memory_limit = memory.limit(),
      memory_size = memory.size()
    )
  }
  
  .GlobalEnv$performance_monitors[[operation_name]] <- monitor_info
  
  debug_log("DEBUG: Started enhanced performance monitoring for: %s", operation_name)
  return(monitor_info)
}

end_performance_monitor_enhanced <- function(operation_name, include_memory = TRUE, include_system = TRUE) {
  if (!exists("performance_monitors", envir = .GlobalEnv) || 
      !exists(operation_name, envir = .GlobalEnv$performance_monitors)) {
    warning("No performance monitor found for: ", operation_name)
    return(NULL)
  }
  
  monitor <- .GlobalEnv$performance_monitors[[operation_name]]
  end_time <- Sys.time()
  
  result <- list(
    operation = operation_name,
    start_time = monitor$start_time,
    end_time = end_time,
    duration = as.numeric(difftime(end_time, monitor$start_time, units = "secs"))
  )
  
  if (include_memory) {
    memory_end <- gc()
    result$memory_metrics <- list(
      memory_used_mb = round((memory_end[2, 3] - monitor$memory_start[2, 3]) / 1024^2, 2),
      peak_memory_mb = round(memory_end[2, 6] / 1024^2, 2),
      final_memory_mb = round(memory_end[2, 3] / 1024^2, 2)
    )
  }
  
  if (include_system) {
    system_end <- list(
      cpu_time = proc.time(),
      memory_limit = memory.limit(),
      memory_size = memory.size()
    )
    
    result$system_metrics <- list(
      cpu_time_user = system_end$cpu_time[1] - monitor$system_start$cpu_time[1],
      cpu_time_system = system_end$cpu_time[2] - monitor$system_start$cpu_time[2],
      cpu_time_elapsed = system_end$cpu_time[3] - monitor$system_start$cpu_time[3],
      memory_limit_gb = round(system_end$memory_limit / 1024^3, 2),
      memory_size_gb = round(system_end$memory_size / 1024^3, 2)
    )
  }
  
  # Performance rating
  if (result$duration < 1) {
    result$performance_rating <- "Excellent"
  } else if (result$duration < 5) {
    result$performance_rating <- "Good"
  } else if (result$duration < 30) {
    result$performance_rating <- "Acceptable"
  } else {
    result$performance_rating <- "Slow"
  }
  
  debug_log("DEBUG: Enhanced performance monitoring completed for %s: %.2f seconds (%s)", 
            operation_name, result$duration, result$performance_rating)
  
  # Clean up
  rm(list = operation_name, envir = .GlobalEnv$performance_monitors)
  
  return(result)
}

# Global performance summary
get_global_performance_summary <- function() {
  if (!exists("performance_monitors", envir = .GlobalEnv)) {
    return("No global performance monitors active")
  }
  
  active_monitors <- ls(.GlobalEnv$performance_monitors)
  if (length(active_monitors) == 0) {
    return("No active global performance monitors")
  }
  
  summary_text <- paste("Global Performance Monitors (", length(active_monitors), "):\n", sep = "")
  
  for (monitor_name in active_monitors) {
    monitor <- .GlobalEnv$performance_monitors[[monitor_name]]
    start_time <- monitor$start_time
    duration <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    
    summary_text <- paste(summary_text, 
                         sprintf("  %s: Running for %.2f seconds\n", monitor_name, duration), 
                         sep = "")
  }
  
  return(summary_text)
}

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
    debug_log("DEBUG: Data caching", paste("Using cached data for", cache_key, "(cache hit)"))
    return(cached_result)
  }
  
  # Load data from file and cache it
  debug_log("DEBUG: Data caching", paste("Loading data from file for", cache_key, "(cache miss)"))
  data <- openxlsx::read.xlsx(file_path, sheet = 1)
  
  # Cache the result using the global caching system
  cache_result(actual_cache_key, data)
  
  debug_log("DEBUG: Data caching", paste("Data cached for", cache_key, "with", nrow(data), "rows"))
  return(data)
}

# Note: Functions are exported via NAMESPACE file
