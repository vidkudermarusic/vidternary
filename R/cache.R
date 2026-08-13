# ---- Comprehensive Caching System for Performance Optimization ----
# This system provides:
# 1. Data caching: Caches Excel file data based on file modification time and size
# 2. Plot result caching: Caches generated plot data to avoid regeneration
# 3. Automatic expiration: Cache entries expire after 5 minutes
# 4. Cache statistics: Monitor cache usage and performance
# 5. Debug logging: Track cache hits/misses when debug mode is enabled
#
# Progress/performance-monitoring helpers live in cache_performance.R.

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

# Progress and performance monitoring functions moved to cache_performance.R

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
