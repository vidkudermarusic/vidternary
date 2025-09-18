# =============================================================================
# vidternary: Shiny Server Module - Cache Management
# =============================================================================
# 
# Package:     vidternary
# Version:     1.0.0
# Author:      Vid Kuder Marušič <vidkm30@gmail.com>
# Maintainer:  Vid Kuder Marušič <vidkm30@gmail.com>
# License:     MIT + file LICENSE
# Repository:  https://github.com/vidkudermarusic/vidternary
# 
# Description: Server-side logic for cache management functionality including
#              cache cleanup, event handlers, and cache operations.
# 
# Key Functions:
#   - create_server_cache_management(): Main cache management server logic
#   - [Cache management functions and operations]
# 
# Dependencies:
#   - R (>= 4.0.0)
#   - shiny
# 
# Last Modified: 2025-09-07
# 
# =============================================================================

create_server_cache_management <- function(input, output, session, rv, show_message, log_operation) {
  
  # ---- Cache Management Functionality ----
  
  # Initialize cache cleanup timer
  cache_cleanup_timer <- reactiveTimer(300000) # 5 minutes
  
  # Periodic cache cleanup
  observe({
    cache_cleanup_timer()
    clear_expired_cache()
  })
  
  # Clear expired cache entries
  clear_expired_cache <- function() {
    tryCatch({
      # Get current time
      current_time <- Sys.time()
      
      # Check if cache directory exists
      cache_dir <- file.path(rv$working_dir, "cache")
      if (!dir.exists(cache_dir)) {
        return()
      }
      
      # Get all cache files
      cache_files <- list.files(cache_dir, full.names = TRUE, pattern = "\\.rds$")
      
      if (length(cache_files) == 0) {
        return()
      }
      
      # Check file modification times and remove expired ones
      expired_count <- 0
      for (file_path in cache_files) {
        file_info <- file.info(file_path)
        file_age <- as.numeric(difftime(current_time, file_info$mtime, units = "hours"))
        
        # Remove files older than 24 hours
        if (file_age > 24) {
          file.remove(file_path)
          expired_count <- expired_count + 1
        }
      }
      
      if (expired_count > 0) {
        log_operation("INFO", "Cache cleanup completed", 
                     paste("Removed", expired_count, "expired cache files"))
      }
      
    }, error = function(e) {
      log_operation("ERROR", "Cache cleanup failed", e$message)
    })
  }
  
  # Clear all cache
  clear_cache <- function() {
    tryCatch({
      cache_dir <- file.path(rv$working_dir, "cache")
      
      if (dir.exists(cache_dir)) {
        # Remove all cache files
        cache_files <- list.files(cache_dir, full.names = TRUE, pattern = "\\.rds$")
        removed_count <- length(cache_files)
        
        if (removed_count > 0) {
          file.remove(cache_files)
          show_message(paste("Cleared", removed_count, "cache files"), "success")
          log_operation("SUCCESS", "Cache cleared", paste("Removed", removed_count, "files"))
        } else {
          show_message("Cache is already empty", "info")
        }
      } else {
        show_message("No cache directory found", "info")
      }
      
    }, error = function(e) {
      show_message(paste("Error clearing cache:", e$message), "error")
      log_operation("ERROR", "Failed to clear cache", e$message)
    })
  }
  
  # Clear expired cache (manual trigger)
  clear_expired_cache_manual <- function() {
    tryCatch({
      clear_expired_cache()
      show_message("Expired cache cleanup completed", "success")
      
    }, error = function(e) {
      show_message(paste("Error during cache cleanup:", e$message), "error")
      log_operation("ERROR", "Manual cache cleanup failed", e$message)
    })
  }
  
  # Refresh cache statistics
  refresh_cache_stats <- function() {
    tryCatch({
      cache_dir <- file.path(rv$working_dir, "cache")
      
      if (dir.exists(cache_dir)) {
        cache_files <- list.files(cache_dir, full.names = TRUE, pattern = "\\.rds$")
        total_size <- sum(file.size(cache_files))
        
        # Get file ages
        file_info <- file.info(cache_files)
        file_ages <- as.numeric(difftime(Sys.time(), file_info$mtime, units = "hours"))
        
        # Count files by age
        recent_files <- sum(file_ages <= 1)      # Last hour
        today_files <- sum(file_ages <= 24)      # Last 24 hours
        old_files <- sum(file_ages > 24)         # Older than 24 hours
        
        stats <- list(
          total_files = length(cache_files),
          total_size_mb = round(total_size / 1024^2, 2),
          recent_files = recent_files,
          today_files = today_files,
          old_files = old_files
        )
        
        # Store stats in reactive values
        rv$cache_stats <- stats
        
        show_message(paste("Cache stats updated:", stats$total_files, "files,", 
                          stats$total_size_mb, "MB"), "success")
        
        log_operation("SUCCESS", "Cache stats refreshed", 
                     paste("Files:", stats$total_files, "Size:", stats$total_size_mb, "MB"))
        
        return(stats)
        
      } else {
        rv$cache_stats <- list(
          total_files = 0,
          total_size_mb = 0,
          recent_files = 0,
          today_files = 0,
          old_files = 0
        )
        
        show_message("No cache directory found", "info")
        return(rv$cache_stats)
      }
      
    }, error = function(e) {
      show_message(paste("Error refreshing cache stats:", e$message), "error")
      log_operation("ERROR", "Failed to refresh cache stats", e$message)
      return(NULL)
    })
  }
  
  # Get cache statistics
  get_cache_stats <- function() {
    if (is.null(rv$cache_stats)) {
      refresh_cache_stats()
    }
    return(rv$cache_stats)
  }
  
  # Cache event handlers
  observeEvent(input$clear_cache, {
    clear_cache()
  })
  
  observeEvent(input$clear_expired_cache, {
    clear_expired_cache_manual()
  })
  
  observeEvent(input$refresh_cache_stats, {
    refresh_cache_stats()
  })
  
  # Return the module functions for external use
  return(list(
    # Core cache functions
    clear_cache = clear_cache,
    clear_expired_cache = clear_expired_cache,
    clear_expired_cache_manual = clear_expired_cache_manual,
    refresh_cache_stats = refresh_cache_stats,
    get_cache_stats = get_cache_stats,
    
    # Event handlers (for external binding)
    clear_cache_handler = function() clear_cache(),
    clear_expired_handler = function() clear_expired_cache_manual(),
    refresh_stats_handler = function() refresh_cache_stats()
  ))
}
