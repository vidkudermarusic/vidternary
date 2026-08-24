# ---- Helper Functions Module ----
# This module contains core utility functions used throughout the app:
# logging, error handling, column-name cleaning, plot-title/summary text,
# large-dataset performance helpers, and progress/performance tracking.
#
# Related helper functions live in sibling modules, split out for size:
#   helpers_filters.R      - filter collection & application
#   helpers_validation.R   - data quality / validation checks
#   helpers_multivariate.R - Mahalanobis/Isolation Forest orchestration
#   helpers_reporting.R    - report/dashboard generation, file I/O

# Note: MIN_POINT_SIZE and MAX_POINT_SIZE constants are defined in options.R

# Enhanced logging system with structured logging and performance optimization
#
# log_operation() is defined at package top level but is called from deep
# inside every create_server_*() factory function, where `rv` is a local
# parameter - and sometimes from plain helper functions that those
# observers/renderers call synchronously (e.g. read_dataset_file()).
# `rv` is never reachable via plain `exists("rv")` (that resolves through
# log_operation's own *lexical* scope - the package namespace - not the
# caller's). Instead, walk the live call stack: for each active frame,
# look up `rv` via ordinary (lexical) scoping starting from that frame.
# A direct call from an observer/renderer finds `rv` immediately (the
# handler expression is lexically nested inside its create_server_*()
# closure); a call via an intermediate helper function finds it once the
# walk reaches that still-executing observer's own frame further up the
# stack. Every access is wrapped in shiny::isolate() - reading a
# reactiveValues field from an *active* reactive context (e.g. the
# observer that ends up calling log_operation()) registers a read
# dependency for that context; the subsequent write to the same field
# would then invalidate that same context, which re-runs, calls
# log_operation() again, and so on - a self-sustaining infinite reactive
# loop (this was hit and confirmed during development: a call site that
# errors on every invocation produced 10000+ log_operation() calls in
# seconds). isolate() suppresses dependency registration for whichever
# context happens to be calling log_operation(), while the write still
# correctly invalidates unrelated, already-subscribed consumers (e.g. the
# Analysis Log tab's own display). Also guarded with tryCatch since
# log_operation() is sometimes called from non-reactive contexts, where
# reactiveValues access throws outright.
log_operation <- function(level, message, details = NULL) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- list(
    timestamp = timestamp,
    level = level,
    message = message,
    details = details
  )

  tryCatch({
    n <- sys.nframe()
    if (n > 1) {
      for (i in rev(seq_len(n - 1))) {
        frame_env <- sys.frame(i)
        if (exists("rv", envir = frame_env, inherits = TRUE)) {
          rv_obj <- get("rv", envir = frame_env, inherits = TRUE)
          logged <- shiny::isolate({
            current_log <- tryCatch(rv_obj$analysis_log, error = function(e) NULL)
            if (!is.null(current_log)) {
              rv_obj$analysis_log <- c(current_log, list(log_entry))
              if (length(rv_obj$analysis_log) > 10000) {
                rv_obj$analysis_log <- rv_obj$analysis_log[-(1:(length(rv_obj$analysis_log) - 10000))]
              }
              TRUE
            } else {
              FALSE
            }
          })
          if (isTRUE(logged)) break
        }
      }
    }
  }, error = function(e) NULL)

  # Console output for debugging
  cat(sprintf("[%s] %s: %s\n", timestamp, level, message))
  if (!is.null(details)) {
    cat(sprintf("  Details: %s\n", details))
  }
}

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

# Error handling wrapper for safe execution
safe_execute <- function(expr, error_msg = "Operation failed") {
  tryCatch({
    log_operation("INFO", "Starting operation", error_msg)
    result <- eval(expr)
    log_operation("INFO", "Operation completed successfully", error_msg)
    return(result)
  }, error = function(e) {
    log_operation("ERROR", paste(error_msg, ":", e$message))
    # Try to show message if in Shiny context
    if (exists("show_message")) {
      show_message(paste(error_msg, ":", e$message), "error")
    }
    return(NULL)
  })
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

# Function to show messages to the user
show_message <- function(message, type = "info") {
  # In a Shiny context, this would typically use showNotification or similar
  # For now, we'll just print to console
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(sprintf("[%s] %s: %s\n", timestamp, toupper(type), message))
}

# Function to get clean display names for columns
get_display_names <- function(col_names) {
  # For display purposes, keep the original names but clean them nicely
  display_names <- clean_column_names(col_names)
  return(display_names)
}

# Function to generate plot summary text (ternary-plot filter settings).
# Renamed from generate_plot_summary() to resolve an accidental name
# collision with the unrelated generate_plot_summary(plot_obj, data=NULL)
# in plotting_utils.R - the two were never the same function, they just
# shared a name. Nothing calls the old name, so this rename is safe.
generate_ternary_filter_summary <- function(element_A, element_B, element_C, optional_param1, optional_param2,
                                use_mahalanobis = FALSE, use_isolation_forest = FALSE,
                                use_iqr_filter = FALSE, use_zscore_filter = FALSE, use_mad_filter = FALSE,
                                lambda = 1, omega = 0, custom_mdthresh = NULL,
                                keep_outliers_mahalanobis = FALSE,
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
  if (use_mahalanobis || use_isolation_forest || use_iqr_filter || use_zscore_filter || use_mad_filter) {
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
  total_filters <- sum(c(use_mahalanobis, use_isolation_forest, use_iqr_filter, use_zscore_filter, use_mad_filter))
  if (total_filters > 0) {
    summary_lines <- c(summary_lines, "")
    summary_lines <- c(summary_lines, "Data Filtering Summary:")
    summary_lines <- c(summary_lines, paste("  Total filters applied:", total_filters))
    outlier_handling <- if (any(as.logical(c(keep_outliers_mahalanobis, keep_outliers_isolation, keep_outliers_iqr, keep_outliers_zscore, keep_outliers_mad)))) "Keep only outliers" else "Remove outliers"
    summary_lines <- c(summary_lines, paste("  Outlier handling:", outlier_handling))
  }

  return(paste(summary_lines, collapse = "\n"))
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

  # Always return the list structure
  result <- list(data = data, optimizations = optimizations)
  return(result)
}

# Enhanced performance optimization with memory management
optimize_for_large_datasets_enhanced <- function(data, max_rows = 100000, memory_threshold = 0.8) {
  start_time <- Sys.time()
  optimizations <- list(
    original_dim = dim(data),
    applied = FALSE,
    sampling = FALSE,
    memory_optimization = FALSE,
    chunking = FALSE
  )

  # Check memory usage
  memory_usage <- object.size(data) / 1024^3  # GB
  available_memory <- memory.limit() / 1024^3  # GB

  if (memory_usage > available_memory * memory_threshold) {
    optimizations$memory_optimization <- TRUE
    log_operation("Performance", paste("Memory usage high (", round(memory_usage, 2), "GB), applying optimizations"))
  }

  # Check if optimization is needed
  if (nrow(data) > max_rows || optimizations$memory_optimization) {
    optimizations$applied <- TRUE

    # Row sampling for very large datasets
    if (nrow(data) > max_rows) {
      set.seed(123) # For reproducible sampling
      sample_indices <- sample(seq_len(nrow(data)), max_rows)
      data <- data[sample_indices, , drop = FALSE]
      optimizations$sampling <- TRUE
      log_operation("Performance", paste("Sampled", max_rows, "rows from", optimizations$original_dim[1], "total rows"))
    }

    # Apply chunking for memory optimization
    if (optimizations$memory_optimization) {
      chunk_size <- min(10000, ceiling(nrow(data) / 10))
      optimizations$chunking <- TRUE
      log_operation("Performance", paste("Applied chunking with size:", chunk_size))
    }
  }

  end_time <- Sys.time()
  optimizations$processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

  log_operation("Performance", paste("Optimization completed in", round(optimizations$processing_time, 2), "seconds"))

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
      log_operation("Progress", sprintf("Processed chunk %d/%d (%.1f%%)", i, total_chunks, (i/total_chunks)*100))
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

# Enhanced chunked processing with memory monitoring and error handling
process_data_efficiently_enhanced <- function(data, operation, chunk_size = 10000,
                                           memory_monitoring = TRUE, error_handling = TRUE) {
  start_time <- Sys.time()

  if (nrow(data) <= chunk_size) {
    # Process all data at once for small datasets
    return(operation(data))
  }

  # Process data in chunks for large datasets
  results <- list()
  total_chunks <- ceiling(nrow(data) / chunk_size)
  successful_chunks <- 0
  failed_chunks <- 0

  # Memory monitoring
  if (memory_monitoring) {
    initial_memory <- gc(reset = TRUE)
    log_operation("Memory", paste("Initial memory usage:", round(initial_memory[2, 3] / 1024^2, 2), "MB"))
  }

  for (i in 1:total_chunks) {
    start_idx <- (i - 1) * chunk_size + 1
    end_idx <- min(i * chunk_size, nrow(data))

    chunk <- data[start_idx:end_idx, , drop = FALSE]

    # Process chunk with error handling
    if (error_handling) {
      chunk_result <- tryCatch({
        operation(chunk)
      }, error = function(e) {
        log_operation("ERROR", paste("Chunk", i, "failed:", e$message))
        failed_chunks <- failed_chunks + 1
        return(NULL)
      })
    } else {
      chunk_result <- operation(chunk)
    }

    if (!is.null(chunk_result)) {
      results[[i]] <- chunk_result
      successful_chunks <- successful_chunks + 1
    }

    # Memory monitoring and cleanup
    if (memory_monitoring && i %% 5 == 0) {
      current_memory <- gc()
      log_operation("Memory", paste("Chunk", i, "memory:", round(current_memory[2, 3] / 1024^2, 2), "MB"))

      # Force garbage collection every 5 chunks
      gc()
    }

    # Update progress
    if (i %% 10 == 0) {
      log_operation("Progress", sprintf("Processed chunk %d/%d (%.1f%%) - Success: %d, Failed: %d",
                                      i, total_chunks, (i/total_chunks)*100, successful_chunks, failed_chunks))
    }
  }

  # Final memory cleanup
  if (memory_monitoring) {
    final_memory <- gc()
    log_operation("Memory", paste("Final memory usage:", round(final_memory[2, 3] / 1024^2, 2), "MB"))
  }

  # Combine results
  if (length(results) == 0) {
    log_operation("ERROR", "No chunks processed successfully")
    return(NULL)
  }

  # Remove NULL results
  results <- results[!sapply(results, is.null)]

  if (is.data.frame(results[[1]])) {
    final_result <- do.call(rbind, results)
  } else if (is.list(results[[1]])) {
    final_result <- do.call(c, results)
  } else {
    final_result <- unlist(results)
  }

  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

  log_operation("Performance", sprintf("Chunked processing completed in %.2f seconds - %d/%d chunks successful",
                                     processing_time, successful_chunks, total_chunks))

  return(final_result)
}

# Create multi-line title
create_multi_line_title <- function(title_parts) {
  # Clean and format title parts
  title_parts <- sapply(title_parts, function(part) {
    if (is.null(part) || length(part) == 0) {
      return(NULL)
    }
    return(as.character(part))
  })

  # Remove NULL parts
  title_parts <- title_parts[!sapply(title_parts, is.null)]

  if (length(title_parts) == 0) {
    return("Ternary Plot")
  }

  return(paste(title_parts, collapse = " | "))
}

# Preview title layout
preview_title_layout <- function(title_parts) {
  title <- create_multi_line_title(title_parts)
  message("Preview title layout:")
  message("Title:", title)
  message("Length:", nchar(title))
  message("Estimated width:", nchar(title) * 0.6, "inches")
}

# Calculate plot dimensions
calculate_plot_dimensions <- function(title_parts) {
  title <- create_multi_line_title(title_parts)
  title_length <- nchar(title)

  # Base dimensions
  base_width <- 10
  base_height <- 8

  # Adjust for title length
  if (title_length > 50) {
    base_width <- base_width + (title_length - 50) * 0.05
  }

  return(list(width = base_width, height = base_height))
}

# Comprehensive Testing and Validation System
run_system_tests <- function() {
  cat("=== Running System Tests ===\n")
  test_results <- list()

  # Test 1: Package availability
  cat("Test 1: Checking package availability...\n")
  required_packages <- c("shiny", "openxlsx", "Ternary", "PlotTools")
  test_results$packages <- all(required_packages %in% installed.packages()[,"Package"])
  if (test_results$packages) {
    cat("✅ All required packages are available\n")
  } else {
    cat("❌ Some required packages are missing\n")
    missing_pkgs <- setdiff(required_packages, installed.packages()[,"Package"])
    cat("Missing packages:", paste(missing_pkgs, collapse = ", "), "\n")
  }

  # Test 2: Function availability
  cat("Test 2: Checking function availability...\n")
  required_functions <- c("validate_data", "generate_stats", "compute_correlation",
                          "check_data_quality")
  test_results$functions <- all(sapply(required_functions, exists))
  if (test_results$functions) {
    cat("✅ All required functions are available\n")
  } else {
    cat("❌ Some required functions are missing\n")
    missing_funcs <- required_functions[!sapply(required_functions, exists)]
    cat("Missing functions:", paste(missing_funcs, collapse = ", "), "\n")
  }

  # Test 3: Data processing capabilities
  cat("Test 3: Testing data processing...\n")
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

  # Test 4: Performance optimization
  cat("Test 4: Testing performance optimization...\n")
  test_results$performance <- tryCatch({
    large_data <- data.frame(
      x = 1:15000,
      y = rnorm(15000),
      z = sample(letters[1:5], 15000, replace = TRUE)
    )

    cat("Created test data with", nrow(large_data), "rows\n")
    opt_result <- optimize_for_large_datasets(large_data, max_rows = 100000)
    cat("Optimization result has", nrow(opt_result$data), "rows\n")

    if (nrow(opt_result$data) != 15000) stop("Performance optimization failed - should not sample when under max_rows")

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

# Function to generate distinct colors for categorical groups
generate_distinct_colors <- function(n_groups) {
  if (n_groups <= 0) return(character(0))

  # Use ColorBrewer palettes for maximum distinction
  if (n_groups <= 12) {
    colors <- RColorBrewer::brewer.pal(n_groups, "Set3")
  } else if (n_groups <= 24) {
    colors <- c(RColorBrewer::brewer.pal(12, "Set3"),
                RColorBrewer::brewer.pal(min(12, n_groups-12), "Paired"))
  } else if (n_groups <= 32) {
    colors <- c(RColorBrewer::brewer.pal(12, "Set3"),
                RColorBrewer::brewer.pal(12, "Paired"),
                RColorBrewer::brewer.pal(min(8, n_groups-24), "Dark2"))
  } else {
    # For >32 groups, use viridis sampling
    colors <- viridis::viridis(n_groups)
  }
  return(colors)
}

# Function to create group legend
create_group_legend <- function(groups, colors, counts) {
  if (length(groups) == 0) return()

  # Sort groups by frequency (most frequent first)
  group_order <- order(counts[groups], decreasing = TRUE)
  sorted_groups <- groups[group_order]
  sorted_colors <- colors[group_order]

  # Create multi-column legend
  legend("topright",
         legend = sorted_groups,
         col = sorted_colors,
         pch = 16,
         title = "Groups",
         cex = 0.6,
         ncol = 2, # 2 columns
         y.intersp = 0.8)
}

# Progress monitoring functions for comprehensive analysis
progress_tracker <- list()

start_progress <- function(step_name, total_steps) {
  progress_tracker$current_step <<- step_name
  progress_tracker$total_steps <<- total_steps
  progress_tracker$start_time <<- Sys.time()
  cat("Starting:", step_name, "\n")
}

update_progress <- function(step_number, message) {
  progress_tracker$current_step_number <<- step_number
  progress_tracker$current_message <<- message
  cat("Step", step_number, ":", message, "\n")
}

start_performance_monitor <- function(analysis_name) {
  progress_tracker$analysis_name <<- analysis_name
  progress_tracker$analysis_start_time <<- Sys.time()
  cat("=== Starting", analysis_name, "===\n")
}

end_performance_monitor <- function(analysis_name) {
  if (!is.null(progress_tracker$analysis_start_time)) {
    duration <- Sys.time() - progress_tracker$analysis_start_time
    cat("=== Completed", analysis_name, "in", round(as.numeric(duration, units = "secs"), 2), "seconds ===\n")
  }
}

get_performance_summary <- function() {
  if (!is.null(progress_tracker$analysis_start_time)) {
    duration <- Sys.time() - progress_tracker$analysis_start_time
    return(paste("Total analysis time:", round(as.numeric(duration, units = "secs"), 2), "seconds"))
  }
  return("Performance monitoring not available")
}

# Note: Functions are exported via NAMESPACE file
