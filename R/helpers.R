# =============================================================================
# vidternary: Utility Functions Module
# =============================================================================
# 
# Package:     vidternary
# Version:     1.0.0
# Author:      Vid Kuder Marušič <vidkm30@gmail.com>
# Maintainer:  Vid Kuder Marušič <vidkm30@gmail.com>
# License:     MIT + file LICENSE
# Repository:  https://github.com/vidkudermarusic/vidternary
# 
# Description: Utility functions for data processing, validation, logging,
#              and general operations used throughout the package.
# 
# Key Functions:
#   - log_operation(): Enhanced logging system with structured logging
#   - validate_data_quality(): Comprehensive data validation utilities
#   - debug_log(): Debug logging with formatting
#   - generate_stats(): Statistical summary generation
#   - safe_execute(): Safe function execution with error handling
# 
# Dependencies:
#   - R (>= 4.0.0)
#   - digest, moments
# 
# Last Modified: 2025-09-07
# 
# =============================================================================

# Constants
MIN_POINT_SIZE <- 0.1
MAX_POINT_SIZE <- 2.5

# Enhanced logging system with structured logging and performance optimization
log_operation <- function(level, message, details = NULL) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- list(
    timestamp = timestamp,
    level = level,
    message = message,
    details = details
  )
  
  # Store in global analysis log if available
  if (exists("rv") && !is.null(rv$analysis_log)) {
    rv$analysis_log <- c(rv$analysis_log, list(log_entry))
    
    # Keep only last 10000 log entries for performance
    if (length(rv$analysis_log) > 10000) {
      rv$analysis_log <- rv$analysis_log[-(1:(length(rv$analysis_log) - 10000))]
    }
  }
  
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

# Unified filter collection function
collect_filters <- function(elements, filter_type, input, prefix = "filter", dataset_num = NULL) {
  if (is.null(elements) || length(elements) == 0) return(list())
  
  filters <- list()
  for (element in elements) {
    # Create safe element name
    safe_element <- gsub("[^A-Za-z0-9]", "_", element)
    
    # Build input ID based on prefix and parameters
    if (prefix == "multiple_filter") {
      input_id <- paste0(prefix, "_", filter_type, "_", safe_element)
    } else {
      # For main ternary filters, match the UI element naming convention
      input_id <- paste0(prefix, "_", filter_type, dataset_num, "_", safe_element)
    }
    
    # Get filter value and validate
    filter_value <- input[[input_id]]
    if (!is.null(filter_value) && !is.na(filter_value) && nchar(trimws(as.character(filter_value))) > 0) {
      filters[[element]] <- filter_value
    }
    

  }
  return(filters)
}

# Simplified wrapper functions for backward compatibility
collect_individual_filters <- function(elements, element_type, input) {
  collect_filters(elements, element_type, input, prefix = "multiple_filter")
}

collect_optional_param_filters <- function(elements, param_type, input) {
  collect_filters(elements, param_type, input, prefix = "multiple_filter")
}

collect_main_ternary_filters <- function(elements, element_type, dataset_num, input) {
  collect_filters(elements, element_type, input, prefix = "filter", dataset_num = dataset_num)
}

# Function to collect all filters for multiple ternary creator
collect_all_multiple_filters <- function(input) {
  list(
    individual_filters_A = collect_filters(input$multiple_element_A, "A", input, prefix = "multiple_filter"),
    individual_filters_B = collect_filters(input$multiple_element_B, "B", input, prefix = "multiple_filter"),
    individual_filters_C = collect_filters(input$multiple_element_C, "C", input, prefix = "multiple_filter"),
    optional_param1_filters = collect_filters(input$multiple_optional_param1, "op1", input, prefix = "multiple_filter"),
    optional_param2_filters = collect_filters(input$multiple_optional_param2, "op2", input, prefix = "multiple_filter")
  )
}

# Function to show messages to the user
show_message <- function(message, type = "info") {
  # In a Shiny context, this would typically use showNotification or similar
  # For now, we'll just print to console
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(sprintf("[%s] %s: %s\n", timestamp, toupper(type), message))
}

# Function to validate data quality
validate_data_quality <- function(data, cols = NULL) {
  if (is.null(cols)) {
    cols <- colnames(data)
  }
  
  # Check for missing values
  missing_counts <- sapply(data[, cols, drop = FALSE], function(x) sum(is.na(x)))
  
  # Check for infinite values
  infinite_counts <- sapply(data[, cols, drop = FALSE], function(x) sum(is.infinite(x)))
  
  # Check for zero variance columns
  zero_var_cols <- sapply(data[, cols, drop = FALSE], function(x) {
    if (is.numeric(x)) var(x, na.rm = TRUE) == 0 else FALSE
  })
  
  # Check for outliers using IQR method
  outliers_iqr <- sapply(data[, cols, drop = FALSE], function(x) {
    if (is.numeric(x)) {
      q1 <- quantile(x, 0.25, na.rm = TRUE)
      q3 <- quantile(x, 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      lower <- q1 - 1.5 * iqr
      upper <- q3 + 1.5 * iqr
      sum(x < lower | x > upper, na.rm = TRUE)
    } else 0
  })
  
  return(list(
    missing_counts = missing_counts,
    zero_var_cols = names(zero_var_cols[zero_var_cols]),
    outliers_iqr = outliers_iqr
  ))
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
    outlier_handling <- if (any(as.logical(c(keep_outliers_mahalanobis, keep_outliers_robust, keep_outliers_isolation, keep_outliers_iqr, keep_outliers_zscore, keep_outliers_mad)))) "Keep only outliers" else "Remove outliers"
    summary_lines <- c(summary_lines, paste("  Outlier handling:", outlier_handling))
  }
  
  return(paste(summary_lines, collapse = "\n"))
}

# Function to apply filter safely
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

# Function to safely execute expressions
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

# Function to validate inputs
validate_inputs <- function(inputs) {
  required_fields <- c("element_A", "element_B", "element_C")
  missing_fields <- required_fields[!sapply(required_fields, function(x) !is.null(inputs[[x]]) && length(inputs[[x]]) > 0)]
  
  if (length(missing_fields) > 0) {
    stop("Missing required fields: ", paste(missing_fields, collapse = ", "))
  }
}

# Function to get individual filters for elements
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

# Note: check_required_packages function is now in dependencies.R to avoid duplication
# Use the version from dependencies.R for package management

# Note: load_package_safely function is now in dependencies.R to avoid duplication
# Use the version from dependencies.R for package loading

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

# Apply individual filters
apply_individual_filters <- function(data, element, individual_filters, element_name, preview = FALSE) {
  if (is.null(individual_filters) || length(individual_filters) == 0) {
    return(data)
  }
  
  filtered_data <- data
  
  for (filter_name in names(individual_filters)) {
    filter_value <- individual_filters[[filter_name]]
    if (!is.null(filter_value) && length(filter_value) > 0) {
      filtered_data <- apply_filter(filtered_data, filter_name, filter_value)
    }
  }
  
  if (preview) {
    message(paste("Applied filters for", element_name, ":", nrow(filtered_data), "rows remaining"))
  }
  
  return(filtered_data)
}

# Enhanced Data Quality Check Function
check_data_quality <- function(data1, data2) {
  start_time <- Sys.time()
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
          kurtosis = if(length(x) > 2) tryCatch(moments::skewness(x, na.rm = TRUE), error = function(e) NA) else NA)
      } else NULL
    }),
    data2 = sapply(data2, function(x) {
      if(is.numeric(x)) {
        c(mean = mean(x, na.rm = TRUE),
          median = median(x, na.rm = TRUE),
          sd = sd(x, na.rm = TRUE),
          skewness = if(length(x) > 2) tryCatch(moments::skewness(x, na.rm = TRUE), error = function(e) NA) else NA,
          kurtosis = if(length(x) > 2) tryCatch(moments::skewness(x, na.rm = TRUE), error = function(e) NA) else NA)
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
  
  end_time <- Sys.time()
  quality_report$processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  log_operation("Performance", paste("Data quality check completed in", round(quality_report$processing_time, 2), "seconds"))
  
  return(quality_report)
}

# Comprehensive data quality assessment with advanced metrics
check_data_quality_comprehensive <- function(data1, data2, include_advanced_metrics = TRUE) {
  start_time <- Sys.time()
  
  # Basic quality check
  quality_report <- check_data_quality(data1, data2)
  
  if (include_advanced_metrics) {
    # Advanced data quality metrics
    quality_report$advanced_metrics <- list()
    
    # Data consistency checks
    quality_report$advanced_metrics$consistency <- list(
      data1 = check_data_consistency(data1),
      data2 = check_data_consistency(data2)
    )
    
    # Statistical distribution tests
    quality_report$advanced_metrics$distribution_tests <- list(
      data1 = perform_distribution_tests(data1),
      data2 = perform_distribution_tests(data2)
    )
    
    # Data integrity checks
    quality_report$advanced_metrics$integrity <- list(
      data1 = check_data_integrity(data1),
      data2 = check_data_integrity(data2)
    )
    
    # Cross-dataset comparison
    quality_report$advanced_metrics$cross_comparison <- compare_datasets(data1, data2)
  }
  
  end_time <- Sys.time()
  quality_report$total_processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  log_operation("Performance", paste("Comprehensive data quality check completed in", 
                                   round(quality_report$total_processing_time, 2), "seconds"))
  
  return(quality_report)
}

# Helper functions for comprehensive data quality assessment
check_data_consistency <- function(data) {
  consistency_report <- list()
  
  # Check for consistent data types within columns
  consistency_report$type_consistency <- sapply(data, function(x) {
    if (length(unique(sapply(x, class))) == 1) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })
  
  # Check for consistent value ranges
  consistency_report$range_consistency <- sapply(data, function(x) {
    if (is.numeric(x)) {
      range_val <- range(x, na.rm = TRUE)
      return(list(min = range_val[1], max = range_val[2], range = diff(range_val)))
    } else {
      return(NULL)
    }
  })
  
  # Check for consistent missing value patterns
  consistency_report$missing_patterns <- sapply(data, function(x) {
    if (is.numeric(x)) {
      missing_pct <- sum(is.na(x)) / length(x) * 100
      return(round(missing_pct, 2))
    } else {
      return(0)
    }
  })
  
  return(consistency_report)
}

perform_distribution_tests <- function(data) {
  distribution_report <- list()
  
  numeric_cols <- sapply(data, is.numeric)
  if (sum(numeric_cols) == 0) {
    return(list(message = "No numeric columns for distribution tests"))
  }
  
  numeric_data <- data[, numeric_cols, drop = FALSE]
  
  # Normality tests for each numeric column
  distribution_report$normality_tests <- sapply(numeric_data, function(x) {
    if (length(x) > 3) {
      tryCatch({
        test_result <- shapiro.test(x[!is.na(x)])
        return(list(
          statistic = test_result$statistic,
          p_value = test_result$p.value,
          is_normal = test_result$p.value > 0.05
        ))
      }, error = function(e) {
        return(list(error = e$message))
      })
    } else {
      return(list(message = "Insufficient data for normality test"))
    }
  })
  
  # Skewness and kurtosis
  distribution_report$shape_metrics <- sapply(numeric_data, function(x) {
    if (length(x) > 2) {
      tryCatch({
        c(skewness = moments::skewness(x, na.rm = TRUE),
          kurtosis = moments::kurtosis(x, na.rm = TRUE))
      }, error = function(e) {
        return(c(skewness = NA, kurtosis = NA))
      })
    } else {
      return(c(skewness = NA, kurtosis = NA))
    }
  })
  
  return(distribution_report)
}

check_data_integrity <- function(data) {
  integrity_report <- list()
  
  # Check for duplicate rows
  integrity_report$duplicate_rows <- sum(duplicated(data))
  integrity_report$duplicate_percentage <- round(sum(duplicated(data)) / nrow(data) * 100, 2)
  
  # Check for duplicate column names
  integrity_report$duplicate_columns <- sum(duplicated(colnames(data)))
  
  # Check for empty columns
  integrity_report$empty_columns <- sapply(data, function(x) {
    if (is.numeric(x)) {
      return(all(is.na(x) | x == 0))
    } else {
      return(all(is.na(x) | x == ""))
    }
  })
  
  # Check for constant columns
  integrity_report$constant_columns <- sapply(data, function(x) {
    if (is.numeric(x)) {
      return(length(unique(x[!is.na(x)])) <= 1)
    } else {
      return(length(unique(x[!is.na(x)])) <= 1)
    }
  })
  
  return(integrity_report)
}

compare_datasets <- function(data1, data2) {
  comparison_report <- list()
  
  # Compare dimensions
  comparison_report$dimensions <- list(
    data1 = dim(data1),
    data2 = dim(data2),
    difference = c(dim(data1)[1] - dim(data2)[1], dim(data1)[2] - dim(data2)[2])
  )
  
  # Compare column names
  common_cols <- intersect(colnames(data1), colnames(data2))
  unique_to_data1 <- setdiff(colnames(data1), colnames(data2))
  unique_to_data2 <- setdiff(colnames(data2), colnames(data1))
  
  comparison_report$columns <- list(
    common = common_cols,
    unique_to_data1 = unique_to_data1,
    unique_to_data2 = unique_to_data2,
    overlap_percentage = round(length(common_cols) / max(ncol(data1), ncol(data2)) * 100, 2)
  )
  
  # Compare data types for common columns
  if (length(common_cols) > 0) {
    type_comparison <- sapply(common_cols, function(col) {
      type1 <- class(data1[[col]])
      type2 <- class(data2[[col]])
      return(list(
        data1_type = type1,
        data2_type = type2,
        compatible = type1 == type2
      ))
    })
    comparison_report$type_compatibility <- type_comparison
  }
  
  return(comparison_report)
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

# Report Generation Function
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

# Enhanced Data Visualization Functions
create_quality_dashboard <- function(quality_report, output_dir = NULL) {
  if (is.null(output_dir)) {
    output_dir <- getwd()
  }
  
  # Create HTML dashboard in parts
  header <- paste0(
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
    "<p>Generated on: ", Sys.time(), "</p>"
  )
  
  dataset1_section <- paste0(
    "<h2>Overall Quality Scores</h2>",
    "<div class='metric'>",
    "<h3>Dataset 1</h3>",
    "<div class='score grade-", quality_report$quality_score$data1$grade, "'>",
    "Score: ", quality_report$quality_score$data1$score, "/100 (Grade: ", quality_report$quality_score$data1$grade, ")</div>",
    "<p>Missing: ", quality_report$quality_score$data1$details$missing_penalty, "%, ",
    "Infinite: ", quality_report$quality_score$data1$details$infinite_penalty, "%, ",
    "Zero Variance: ", quality_report$quality_score$data1$details$zero_var_penalty, "%, ",
    "Outliers: ", quality_report$quality_score$data1$details$outlier_penalty, "%</p>",
    "</div>"
  )
  
  dataset2_section <- paste0(
    "<div class='metric'>",
    "<h3>Dataset 2</h3>",
    "<div class='score grade-", quality_report$quality_score$data2$grade, "'>",
    "Score: ", quality_report$quality_score$data2$score, "/100 (Grade: ", quality_report$quality_score$data2$grade, ")</div>",
    "<p>Missing: ", quality_report$quality_score$data2$details$missing_penalty, "%, ",
    "Infinite: ", quality_report$quality_score$data2$details$infinite_penalty, "%, ",
    "Zero Variance: ", quality_report$quality_score$data2$details$zero_var_penalty, "%, ",
    "Outliers: ", quality_report$quality_score$data2$details$outlier_penalty, "%</p>",
    "</div>"
  )
  
  missing_values_table <- paste0(
    "<h2>Detailed Analysis</h2>",
    "<h3>Missing Values</h3>",
    "<table><tr><th>Column</th><th>Dataset 1</th><th>Dataset 2</th></tr>",
    paste0("<tr><td>", names(quality_report$missing_values$data1), "</td><td>", 
           quality_report$missing_values$data1, "</td><td>", 
           quality_report$missing_values$data2, "</td></tr>", collapse = ""),
    "</table>"
  )
  
  outliers_table <- paste0(
    "<h3>Outliers (IQR Method)</h3>",
    "<table><tr><th>Column</th><th>Dataset 1</th><th>Dataset 2</th></tr>",
    paste0("<tr><td>", names(quality_report$outliers_iqr$data1), "</td><td>", 
           quality_report$outliers_iqr$data1, "</td><td>", 
           quality_report$outliers_iqr$data2, "</td></tr>", collapse = ""),
    "</table>"
  )
  
  footer <- "</body></html>"
  
  # Combine all parts
  dashboard_html <- paste0(header, dataset1_section, dataset2_section, missing_values_table, outliers_table, footer)
  
  dashboard_file <- file.path(output_dir, "data_quality_dashboard.html")
  writeLines(dashboard_html, dashboard_file)
  cat("Quality dashboard created:", dashboard_file, "\n")
  return(dashboard_file)
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

# ---- Advanced Data Analysis Functions ----

# Data analysis function for Dataset 1
analyze_data1 <- function(element_A1, element_B1, element_C1, xlsx_file1, 
                          optional_param1_1 = "", optional_param2_1 = "") {
  if (is.null(xlsx_file1)) {
    return(NULL)
  }
  
  tryCatch({
    df <- openxlsx::read.xlsx(xlsx_file1$datapath, sheet = 1)
    
    # Collect all columns for analysis - ensure they are character strings
    cols <- character(0)
    if (!is.null(element_A1) && element_A1 != "") cols <- c(cols, as.character(element_A1))
    if (!is.null(element_B1) && element_B1 != "") cols <- c(cols, as.character(element_B1))
    if (!is.null(element_C1) && element_C1 != "") cols <- c(cols, as.character(element_C1))
    if (!is.null(optional_param1_1) && optional_param1_1 != "") cols <- c(cols, as.character(optional_param1_1))
    if (!is.null(optional_param2_1) && optional_param2_1 != "") cols <- c(cols, as.character(optional_param2_1))
    
    # Remove duplicates and ensure columns exist
    cols <- unique(cols)
    available_cols <- colnames(df)
    cols <- cols[cols %in% available_cols]
    
    if (length(cols) == 0) {
      log_operation("WARNING", "No valid columns found for Dataset 1 analysis")
      return(NULL)
    }
    
    # Validate data
    validation <- validate_data_enhanced(df, cols)
    if (!validation$valid) {
      log_operation("WARNING", "Data validation failed for Dataset 1", validation$issues)
      return(NULL)
    }
    
    # Generate statistics
    stats <- generate_stats(df, cols)
    
    # Compute correlation
    correlation <- compute_correlation(df, cols)
    
    # Data quality check
    quality <- check_data_quality(df, cols)
    
    result <- list(
      df = df,
      stats = stats,
      validation = validation,
      correlation = correlation,
      quality = quality,
      timestamp = Sys.time()
    )
    
    log_operation("INFO", "Dataset 1 analysis completed successfully", 
                  paste("Columns analyzed:", length(cols), "Rows:", nrow(df)))
    
    return(result)
    
  }, error = function(e) {
    log_operation("ERROR", "Failed to analyze Dataset 1", e$message)
    return(NULL)
  })
}

# Data analysis function for Dataset 2
analyze_data2 <- function(element_A2, element_B2, element_C2, xlsx_file2, 
                          optional_param1_2 = "", optional_param2_2 = "") {
  if (is.null(xlsx_file2)) {
    return(NULL)
  }
  
  tryCatch({
    df <- openxlsx::read.xlsx(xlsx_file2$datapath, sheet = 1)
    
    # Collect all columns for analysis - ensure they are character strings
    cols <- character(0)
    if (!is.null(element_A2) && element_A2 != "") cols <- c(cols, as.character(element_A2))
    if (!is.null(element_B2) && element_B2 != "") cols <- c(cols, as.character(element_B2))
    if (!is.null(element_C2) && element_C2 != "") cols <- c(cols, as.character(element_C2))
    if (!is.null(optional_param1_2) && optional_param1_2 != "") cols <- c(cols, as.character(optional_param1_2))
    if (!is.null(optional_param2_2) && optional_param2_2 != "") cols <- c(cols, as.character(optional_param2_2))
    
    # Remove duplicates and ensure columns exist
    cols <- unique(cols)
    available_cols <- colnames(df)
    cols <- cols[cols %in% available_cols]
    
    if (length(cols) == 0) {
      log_operation("WARNING", "No valid columns found for Dataset 2 analysis")
      return(NULL)
    }
    
    # Validate data
    validation <- validate_data_enhanced(df, cols)
    if (!validation$valid) {
      log_operation("WARNING", "Data validation failed for Dataset 2", validation$issues)
      return(NULL)
    }
    
    # Generate statistics
    stats <- generate_stats(df, cols)
    
    # Compute correlation
    correlation <- compute_correlation(df, cols)
    
    # Data quality check
    quality <- check_data_quality(df, cols)
    
    result <- list(
      df = df,
      stats = stats,
      validation = validation,
      correlation = correlation,
      quality = quality,
      timestamp = Sys.time()
    )
    
    log_operation("INFO", "Dataset 2 analysis completed successfully", 
                  paste("Columns analyzed:", length(cols), "Rows:", nrow(df)))
    
    return(result)
    
  }, error = function(e) {
    log_operation("ERROR", "Failed to analyze Dataset 2", e$message)
    return(NULL)
  })
}

# Wrapper functions for multivariate analysis methods
perform_mahalanobis_analysis <- function(data, columns, lambda) {
  if (length(columns) < 2) return(NULL)
  numeric_cols <- sapply(data, is.numeric)
  available_cols <- colnames(data)[numeric_cols]
  selected_cols <- intersect(columns, available_cols)
  
  if (length(selected_cols) < 2) return(NULL)
  
  result <- compute_mahalanobis_distance(
    data[, selected_cols, drop = FALSE], 
    data[, selected_cols, drop = FALSE], 
    lambda = lambda, 
    omega = 0,
    keep_outliers = FALSE,
    custom_mdthresh = NULL,
    selected_columns = selected_cols,
    mdthresh_mode = "auto"
  )
  return(result)
}

perform_robust_mahalanobis_analysis <- function(data, columns, lambda) {
  if (length(columns) < 2) return(NULL)
  numeric_cols <- sapply(data, is.numeric)
  available_cols <- colnames(data)[numeric_cols]
  selected_cols <- intersect(columns, available_cols)
  
  if (length(selected_cols) < 2) return(NULL)
  
  result <- compute_robust_mahalanobis(
    data[, selected_cols, drop = FALSE], 
    data[, selected_cols, drop = FALSE], 
    method = "MCD",
    keep_outliers = FALSE,
    selected_columns = selected_cols
  )
  return(result)
}

perform_isolation_forest_analysis <- function(data, columns, omega) {
  if (length(columns) < 2) return(NULL)
  numeric_cols <- sapply(data, is.numeric)
  available_cols <- colnames(data)[numeric_cols]
  selected_cols <- intersect(columns, available_cols)
  
  if (length(selected_cols) < 2) return(NULL)
  
  result <- compute_isolation_forest(
    data[, selected_cols, drop = FALSE], 
    data[, selected_cols, drop = FALSE], 
    keep_outliers = FALSE,
    selected_columns = selected_cols
  )
  return(result)
}

# Function to combine outlier results from multiple methods
combine_outlier_results <- function(results) {
  # Initialize combined outliers list
  combined <- list(
    outlier_indices = logical(0),
    outlier_count = 0,
    methods_agreement = NULL,
    summary = NULL
  )
  
  # Extract outlier indices from each method
  outlier_methods <- list()
  
  if (!is.null(results$mahalanobis_results) && !is.null(results$mahalanobis_results$outlier_indices)) {
    outlier_methods$mahalanobis <- results$mahalanobis_results$outlier_indices
  }
  
  if (!is.null(results$robust_mahalanobis_results) && !is.null(results$robust_mahalanobis_results$outlier_indices)) {
    outlier_methods$robust_mahalanobis <- results$robust_mahalanobis_results$outlier_indices
  }
  
  if (!is.null(results$isolation_forest_results) && !is.null(results$isolation_forest_results$outlier_indices)) {
    outlier_methods$isolation_forest <- results$isolation_forest_results$outlier_indices
  }
  
  if (!is.null(results$iqr_filter_results) && !is.null(results$iqr_filter_results$outlier_indices)) {
    outlier_methods$iqr_filter <- results$iqr_filter_results$outlier_indices
  }
  
  if (!is.null(results$zscore_filter_results) && !is.null(results$zscore_filter_results$outlier_indices)) {
    outlier_methods$zscore_filter <- results$zscore_filter_results$outlier_indices
  }
  
  if (!is.null(results$mad_filter_results) && !is.null(results$mad_filter_results$outlier_indices)) {
    outlier_methods$mad_filter <- results$mad_filter_results$outlier_indices
  }
  
  # If no outlier methods available, return empty result
  if (length(outlier_methods) == 0) {
    return(combined)
  }
  
  # Find common length (should be the same for all methods)
  lengths <- sapply(outlier_methods, length)
  if (length(unique(lengths)) > 1) {
    warning("Outlier methods have different lengths, using minimum length")
    min_length <- min(lengths)
    outlier_methods <- lapply(outlier_methods, function(x) x[1:min_length])
  }
  
  # Combine using union (any method flags as outlier)
  if (length(outlier_methods) > 0) {
    if (length(outlier_methods) == 1) {
      combined$outlier_indices <- outlier_methods[[1]]
    } else {
      combined$outlier_indices <- Reduce(`|`, outlier_methods)
    }
    combined$outlier_count <- sum(combined$outlier_indices)
    
    # Calculate agreement between methods
    if (length(outlier_methods) > 1) {
      agreement_matrix <- matrix(0, nrow = length(outlier_methods), ncol = length(outlier_methods))
      method_names <- names(outlier_methods)
      rownames(agreement_matrix) <- method_names
      colnames(agreement_matrix) <- method_names
      
      for (i in seq_along(outlier_methods)) {
        for (j in seq_along(outlier_methods)) {
          if (i != j) {
            agreement_matrix[i, j] <- sum(outlier_methods[[i]] & outlier_methods[[j]]) / 
                                     sum(outlier_methods[[i]] | outlier_methods[[j]]) * 100
          } else {
            agreement_matrix[i, j] <- 100
          }
        }
      }
      combined$methods_agreement <- agreement_matrix
    }
    
    # Create summary
    combined$summary <- list(
      total_methods = length(outlier_methods),
      total_outliers = combined$outlier_count,
      outlier_percentage = round(combined$outlier_count / length(combined$outlier_indices) * 100, 2),
      methods_used = names(outlier_methods)
    )
  }
  
  return(combined)
}

# Multivariate analysis calculation with multiple methods
multivariate_analysis <- function(use_mahalanobis = FALSE, use_robust_mahalanobis = FALSE, 
                                 use_isolation_forest = FALSE, use_iqr_filter = FALSE,
                                 use_zscore_filter = FALSE, use_mad_filter = FALSE,
                                 lambda = 2, omega = 0.1, outlier_mode = "both",
                                 mdthresh_mode = "auto", custom_mdthresh = NULL,
                                 mahalanobis_reference = "dataset1", multivariate_columns = NULL,
                                 selected_columns = NULL, xlsx_file1 = NULL, xlsx_file2 = NULL, 
                                 universal_reference = "dataset1") {
  
  # Check if at least one method is selected
  if (!(use_mahalanobis || use_robust_mahalanobis || use_isolation_forest)) {
    log_operation("WARNING", "No multivariate analysis method selected")
    return(NULL)
  }
  
  # Check if files are uploaded based on universal reference mode
  if (universal_reference == "dataset2") {
    # Need both files for dataset2 reference
    if (is.null(xlsx_file1) || is.null(xlsx_file2)) {
      log_operation("ERROR", "Both datasets required for dataset2 reference mode")
      return(NULL)
    }
  } else {
    # Self-reference only needs one file
    if (is.null(xlsx_file1)) {
      log_operation("ERROR", "Dataset 1 required for analysis")
      return(NULL)
    }
  }
  
  tryCatch({
    # Load data
    df1 <- if (!is.null(xlsx_file1)) openxlsx::read.xlsx(xlsx_file1$datapath, sheet = 1) else NULL
    df2 <- if (!is.null(xlsx_file2)) openxlsx::read.xlsx(xlsx_file2$datapath, sheet = 1) else NULL
    
    results <- list(
      methods_used = c(),
      mahalanobis_results = NULL,
      robust_mahalanobis_results = NULL,
      isolation_forest_results = NULL,
      iqr_filter_results = NULL,
      zscore_filter_results = NULL,
      mad_filter_results = NULL,
      combined_outliers = NULL,
      timestamp = Sys.time()
    )
    
    # Mahalanobis distance analysis
    if (use_mahalanobis && !is.null(multivariate_columns)) {
      if (mahalanobis_reference == "dataset1" && !is.null(df1)) {
        results$mahalanobis_results <- perform_mahalanobis_analysis(df1, multivariate_columns, lambda)
        results$methods_used <- c(results$methods_used, "Mahalanobis")
      } else if (mahalanobis_reference == "dataset2" && !is.null(df2)) {
        results$mahalanobis_results <- perform_mahalanobis_analysis(df2, multivariate_columns, lambda)
        results$methods_used <- c(results$methods_used, "Mahalanobis (Dataset 2)")
      }
    }
    
    # Robust Mahalanobis analysis
    if (use_robust_mahalanobis && !is.null(multivariate_columns)) {
      if (mahalanobis_reference == "dataset1" && !is.null(df1)) {
        results$robust_mahalanobis_results <- perform_robust_mahalanobis_analysis(df1, multivariate_columns, lambda)
        results$methods_used <- c(results$methods_used, "Robust Mahalanobis")
      } else if (mahalanobis_reference == "dataset2" && !is.null(df2)) {
        results$robust_mahalanobis_results <- perform_robust_mahalanobis_analysis(df2, multivariate_columns, lambda)
        results$methods_used <- c(results$methods_used, "Robust Mahalanobis (Dataset 2)")
      }
    }
    
    # Isolation Forest analysis
    if (use_isolation_forest && !is.null(multivariate_columns)) {
      if (mahalanobis_reference == "dataset1" && !is.null(df1)) {
        results$isolation_forest_results <- perform_isolation_forest_analysis(df1, multivariate_columns, omega)
        results$methods_used <- c(results$methods_used, "Isolation Forest")
      } else if (mahalanobis_reference == "dataset2" && !is.null(df2)) {
        results$isolation_forest_results <- perform_isolation_forest_analysis(df2, multivariate_columns, omega)
        results$methods_used <- c(results$methods_used, "Isolation Forest (Dataset 2)")
      }
    }
    
    # Statistical filters
    if (use_iqr_filter) {
      results$iqr_filter_results <- apply_iqr_filter(df1, multivariate_columns)
      results$methods_used <- c(results$methods_used, "IQR Filter")
    }
    
    if (use_zscore_filter) {
      results$zscore_filter_results <- apply_zscore_filter(df1, multivariate_columns, lambda)
      results$methods_used <- c(results$methods_used, "Z-Score Filter")
    }
    
    if (use_mad_filter) {
      results$mad_filter_results <- apply_mad_filter(df1, multivariate_columns, lambda)
      results$methods_used <- c(results$methods_used, "MAD Filter")
    }
    
    # Combine outlier results
    results$combined_outliers <- combine_outlier_results(results)
    
    log_operation("INFO", "Multivariate analysis completed successfully", 
                  paste("Methods used:", paste(results$methods_used, collapse = ", ")))
    
    return(results)
    
  }, error = function(e) {
    log_operation("ERROR", "Multivariate analysis failed", e$message)
    return(NULL)
  })
}

# Unified parameter extraction for ternary plots
extract_ternary_params <- function(input, rv, dataset_num, preview = FALSE, directory_management = NULL, multiple_mode = FALSE) {
  # Essential parameters
  xlsx_file <- rv[[paste0("xlsx_file", dataset_num)]]
  
  # Required elements (A, B, C) - handle both main and multiple modes
  if (multiple_mode) {
    element_A <- list(col = input$multiple_element_A)
    element_B <- list(col = input$multiple_element_B)
    element_C <- list(col = input$multiple_element_C)
  } else {
    element_A <- list(col = input[[paste0("element_A", dataset_num)]])
    element_B <- list(col = input[[paste0("element_B", dataset_num)]])
    element_C <- list(col = input[[paste0("element_C", dataset_num)]])
  }
  
  # Validate required elements
  if (is.null(element_A$col) || length(element_A$col) == 0 ||
      is.null(element_B$col) || length(element_B$col) == 0 ||
      is.null(element_C$col) || length(element_C$col) == 0) {
    return(NULL)  # Return NULL if required elements are missing
  }
  
  # Optional parameters
  optional_param1 <- NULL
  optional_param1_representation <- "point_size"
  if (multiple_mode) {
    if (!is.null(input$multiple_optional_param1) &&
        input$multiple_optional_param1 != "" &&
        length(input$multiple_optional_param1) > 0) {
      # Get optional parameter 1 filters for multiple mode
      optional_param1_filters <- collect_filters(input$multiple_optional_param1, "op1", input, prefix = "multiple_filter")
      # Extract the first filter value from the list
      optional_param1_filter <- if (length(optional_param1_filters) > 0) {
        first_filter_name <- names(optional_param1_filters)[1]
        optional_param1_filters[[first_filter_name]]
      } else NULL
      
      optional_param1 <- list(
        col = input$multiple_optional_param1,
        filter = optional_param1_filter
      )
      
      # Debug output for optional parameter 1 filters
      if (getOption("ternary.debug", FALSE)) {
        cat("DEBUG: Optional param1 filters collected:", if (is.null(optional_param1_filter)) "NULL" else optional_param1_filter, "\n")
        cat("DEBUG: Optional param1 filters list length:", length(optional_param1_filters), "\n")
        cat("DEBUG: Optional param1 filters list names:", paste(names(optional_param1_filters), collapse = ", "), "\n")
      }
      if (!is.null(input$multiple_optional_param1_representation)) {
        optional_param1_representation <- input$multiple_optional_param1_representation
      }
    }
  } else {
    # Enhanced debugging for main ternary plots
    if (getOption("ternary.debug", FALSE)) {
      cat("DEBUG: Checking optional_param1 for dataset", dataset_num, "\n")
      cat("DEBUG: input$optional_param1_", dataset_num, " = ", 
          if (is.null(input[[paste0("optional_param1_", dataset_num)]])) "NULL" else paste(input[[paste0("optional_param1_", dataset_num)]], collapse = ", "), "\n")
      cat("DEBUG: input$filter_op1_", dataset_num, " = ", 
          if (is.null(input[[paste0("filter_op1_", dataset_num)]])) "NULL" else input[[paste0("filter_op1_", dataset_num)]], "\n")
    }
    
    if (!is.null(input[[paste0("optional_param1_", dataset_num)]]) &&
        input[[paste0("optional_param1_", dataset_num)]] != "" &&
        length(input[[paste0("optional_param1_", dataset_num)]]) > 0) {
      optional_param1 <- list(
        col = input[[paste0("optional_param1_", dataset_num)]],
        filter = if (!is.null(input[[paste0("filter_op1_", dataset_num)]]) && nzchar(input[[paste0("filter_op1_", dataset_num)]])) input[[paste0("filter_op1_", dataset_num)]] else NULL
      )
      if (!is.null(input[[paste0("optional_param1_representation", dataset_num)]])) {
        optional_param1_representation <- input[[paste0("optional_param1_representation", dataset_num)]]
      }
      
      if (getOption("ternary.debug", FALSE)) {
        cat("DEBUG: Optional param1 created:", paste(optional_param1$col, collapse = ", "), "\n")
        cat("DEBUG: Optional param1 filter:", if (is.null(optional_param1$filter)) "NULL" else optional_param1$filter, "\n")
        cat("DEBUG: Optional param1 representation:", optional_param1_representation, "\n")
      }
    }
  }
  
  optional_param2 <- NULL
  color_palette <- "blue"
  if (multiple_mode) {
    if (!is.null(input$multiple_optional_param2) &&
        input$multiple_optional_param2 != "" &&
        length(input$multiple_optional_param2) > 0) {
      # Get optional parameter 2 filters for multiple mode
      optional_param2_filters <- collect_filters(input$multiple_optional_param2, "op2", input, prefix = "multiple_filter")
      # Extract the first filter value from the list
      optional_param2_filter <- if (length(optional_param2_filters) > 0) {
        first_filter_name <- names(optional_param2_filters)[1]
        optional_param2_filters[[first_filter_name]]
      } else NULL
      
      optional_param2 <- list(
        col = input$multiple_optional_param2,
        filter = optional_param2_filter
      )
      
      # Debug output for optional parameter 2 filters
      if (getOption("ternary.debug", FALSE)) {
        cat("DEBUG: Optional param2 filters collected:", if (is.null(optional_param2_filter)) "NULL" else optional_param2_filter, "\n")
        cat("DEBUG: Optional param2 filters list length:", length(optional_param2_filters), "\n")
        cat("DEBUG: Optional param2 filters list names:", paste(names(optional_param2_filters), collapse = ", "), "\n")
      }
      if (!is.null(input$multiple_color_palette)) {
        color_palette <- input$multiple_color_palette
      }
    }
  } else {
    # Enhanced debugging for optional_param2
    if (getOption("ternary.debug", FALSE)) {
      cat("DEBUG: Checking optional_param2 for dataset", dataset_num, "\n")
      cat("DEBUG: input$optional_param2_", dataset_num, " = ", 
          if (is.null(input[[paste0("optional_param2_", dataset_num)]])) "NULL" else paste(input[[paste0("optional_param2_", dataset_num)]], collapse = ", "), "\n")
      cat("DEBUG: input$filter_op2_", dataset_num, " = ", 
          if (is.null(input[[paste0("filter_op2_", dataset_num)]])) "NULL" else input[[paste0("filter_op2_", dataset_num)]], "\n")
      cat("DEBUG: input$color_palette", dataset_num, " = ", 
          if (is.null(input[[paste0("color_palette", dataset_num)]])) "NULL" else input[[paste0("color_palette", dataset_num)]], "\n")
    }
    
    if (!is.null(input[[paste0("optional_param2_", dataset_num)]]) &&
        input[[paste0("optional_param2_", dataset_num)]] != "" &&
        length(input[[paste0("optional_param2_", dataset_num)]]) > 0) {
      optional_param2 <- list(
        col = input[[paste0("optional_param2_", dataset_num)]],
        filter = if (!is.null(input[[paste0("filter_op2_", dataset_num)]]) && nzchar(input[[paste0("filter_op2_", dataset_num)]])) input[[paste0("filter_op2_", dataset_num)]] else NULL
      )
      if (!is.null(input[[paste0("color_palette", dataset_num)]])) {
        color_palette <- input[[paste0("color_palette", dataset_num)]]
      }
      
      if (getOption("ternary.debug", FALSE)) {
        cat("DEBUG: Optional param2 created:", paste(optional_param2$col, collapse = ", "), "\n")
        cat("DEBUG: Optional param2 filter:", if (is.null(optional_param2$filter)) "NULL" else optional_param2$filter, "\n")
        cat("DEBUG: Color palette:", color_palette, "\n")
      }
    }
  }
  
  # Individual filters (A, B, C)
  if (multiple_mode) {
    individual_filters_A <- collect_individual_filters(element_A$col, "A", input)
    individual_filters_B <- collect_individual_filters(element_B$col, "B", input)
    individual_filters_C <- collect_individual_filters(element_C$col, "C", input)
  } else {
    individual_filters_A <- collect_main_ternary_filters(element_A$col, "A", dataset_num, input)
    individual_filters_B <- collect_main_ternary_filters(element_B$col, "B", dataset_num, input)
    individual_filters_C <- collect_main_ternary_filters(element_C$col, "C", dataset_num, input)
  }
  

  
  # Global parameters (same for all datasets)
  use_mahalanobis <- if (!is.null(input$use_mahalanobis)) input$use_mahalanobis else FALSE
  use_robust_mahalanobis <- if (!is.null(input$use_robust_mahalanobis)) input$use_robust_mahalanobis else FALSE
  use_isolation_forest <- if (!is.null(input$use_isolation_forest)) input$use_isolation_forest else FALSE
  
  use_iqr_filter <- if (!is.null(input$use_iqr_filter)) input$use_iqr_filter else FALSE
  use_zscore_filter <- if (!is.null(input$use_zscore_filter)) input$use_zscore_filter else FALSE
  use_mad_filter <- if (!is.null(input$use_mad_filter)) input$use_mad_filter else FALSE
  
  lambda <- if (!is.null(input$lambda)) input$lambda else 1
  omega <- if (!is.null(input$omega)) input$omega else 0
  
  # Reference data handling
  reference_data <- NULL
  mahalanobis_reference <- "self"
  if (use_mahalanobis || use_robust_mahalanobis || use_isolation_forest) {
    mahalanobis_reference <- if (!is.null(input$mahalanobis_reference)) input$mahalanobis_reference else "self"
    
    if (mahalanobis_reference == "dataset1" && !is.null(rv$df1)) {
      reference_data <- rv$df1
    } else if (mahalanobis_reference == "dataset2" && !is.null(rv$df2)) {
      reference_data <- rv$df2
    }
  }
  
  # Other parameters
  custom_mdthresh <- if (!is.null(input$custom_mdthresh)) input$custom_mdthresh else NULL
  mdthresh_mode <- if (!is.null(input$mdthresh_mode)) input$mdthresh_mode else "auto"
  selected_columns <- if (!is.null(input$multivariate_columns)) input$multivariate_columns else NULL
  
  # File format
  output_format <- if (!is.null(input$output_format)) input$output_format else "png"
  
  # Manual point size control
  use_manual_point_size <- if (!is.null(input$use_manual_point_size)) input$use_manual_point_size else FALSE
  manual_point_size <- if (!is.null(input$manual_point_size)) input$manual_point_size else 1.0
  
  # Group selection for categorical data
  selected_groups <- if (!is.null(input[[paste0("selected_groups_", dataset_num)]])) input[[paste0("selected_groups_", dataset_num)]] else NULL
  is_categorical_group <- if (!is.null(rv[[paste0("is_categorical_group_", dataset_num)]])) rv[[paste0("is_categorical_group_", dataset_num)]] else FALSE
  
  # Debug output for group selection
  if (getOption("ternary.debug", FALSE)) {
    cat("DEBUG: selected_groups:", if (is.null(selected_groups)) "NULL" else paste(selected_groups, collapse = ", "), "\n")
    cat("DEBUG: is_categorical_group:", is_categorical_group, "\n")
    cat("DEBUG: selected_groups length:", if (is.null(selected_groups)) 0 else length(selected_groups), "\n")
  }
  
  # Additional safety check for categorical detection
  if (!is_categorical_group && !is.null(optional_param2) && !is.null(xlsx_file)) {
    # Check if the data is actually categorical even if not detected as such
    data <- rv[[paste0("df", dataset_num)]]
    if (!is.null(data) && optional_param2$col %in% names(data)) {
      column_data <- data[[optional_param2$col]]
      is_categorical_group <- is.character(column_data) || is.factor(column_data) || 
                             (!is.numeric(column_data) && length(unique(column_data)) <= 50)
    }
  }
  
  keep_outliers_mahalanobis <- if (!is.null(input$outlier_mode_mahalanobis)) input$outlier_mode_mahalanobis else FALSE
  keep_outliers_robust <- if (!is.null(input$outlier_mode_robust)) input$outlier_mode_robust else FALSE
  keep_outliers_isolation <- if (!is.null(input$outlier_mode_isolation)) input$outlier_mode_isolation else FALSE
  keep_outliers_iqr <- if (!is.null(input$outlier_mode_iqr)) input$outlier_mode_iqr else FALSE
  keep_outliers_zscore <- if (!is.null(input$outlier_mode_zscore)) input$outlier_mode_zscore else FALSE
  keep_outliers_mad <- if (!is.null(input$outlier_mode_mad)) input$outlier_mode_mad else FALSE
  
  # Return parameters list
  list(
    xlsx_file = xlsx_file,
    xlsx_display_name = if (!is.null(input$xlsx_display_name)) input$xlsx_display_name else NULL,
    working_dir = getwd(),
    output_dir = if (!is.null(directory_management)) directory_management$output_dir() else file.path(getwd(), "output"),
    element_A = element_A,
    element_B = element_B,
    element_C = element_C,
    optional_param1 = optional_param1,
    optional_param2 = optional_param2,
    color_palette = color_palette,
    optional_param1_representation = optional_param1_representation,
    preview = preview,
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
    individual_filters_A = individual_filters_A,
    individual_filters_B = individual_filters_B,
    individual_filters_C = individual_filters_C,
    custom_mdthresh = custom_mdthresh,
    mdthresh_mode = mdthresh_mode,
    mahalanobis_reference = mahalanobis_reference,
    selected_columns = selected_columns,
    reference_data = reference_data,
    include_plot_notes = if (!is.null(input$include_plot_notes)) input$include_plot_notes else TRUE,
    output_format = output_format,
    use_manual_point_size = use_manual_point_size,
    manual_point_size = manual_point_size,
    selected_groups = selected_groups,
    is_categorical_group = is_categorical_group
  )
}

# Helper function to collect main ternary filters
# Now handled by the unified collect_filters function with wrapper collect_main_ternary_filters

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

# Function to generate filtered data for export
generate_filtered_data_for_export <- function(dataset_num, xlsx_file1 = NULL, xlsx_file2 = NULL) {
  # This function should return the filtered data from ternary plots
  # For now, return original data until we implement proper filtered data storage
  tryCatch({
    if (dataset_num == 1 && !is.null(xlsx_file1)) {
      return(openxlsx::read.xlsx(xlsx_file1$datapath, sheet = 1))
    } else if (dataset_num == 2 && !is.null(xlsx_file2)) {
      return(openxlsx::read.xlsx(xlsx_file2$datapath, sheet = 1))
    } else {
      return(data.frame(Message = paste("Dataset", dataset_num, "not available")))
    }
  }, error = function(e) {
    return(data.frame(Error = paste("Error loading dataset", dataset_num, ":", e$message)))
  })
}

# ---- Centralized File Operations ----
# These functions consolidate common file reading and data processing patterns

# File type detection function
detect_file_type <- function(file_path) {
  if (grepl("\\.csv$", file_path, ignore.case = TRUE)) {
    return("csv")
  } else if (grepl("\\.xlsx?$", file_path, ignore.case = TRUE)) {
    return("excel")
  } else {
    return("unknown")
  }
}

# Centralized file reading function
read_file_by_type <- function(file_path, sheet = 1) {
  file_type <- detect_file_type(file_path)
  
  switch(file_type,
    "csv" = read.csv(file_path),
    "excel" = openxlsx::read.xlsx(file_path, sheet = sheet),
    stop("Unsupported file type: ", file_type)
  )
}

# Centralized dataset file reading for Shiny inputs
read_dataset_file <- function(file_input, sheet = 1) {
  if (is.null(file_input)) return(NULL)
  
  tryCatch({
    return(read_file_by_type(file_input$datapath, sheet))
  }, error = function(e) {
    log_operation("ERROR", "Failed to read dataset file", paste("File:", file_input$name, "Error:", e$message))
    return(NULL)
  })
}

# Centralized numeric column detection
get_numeric_columns <- function(df) {
  if (is.null(df)) return(character(0))
  return(names(df)[sapply(df, is.numeric)])
}

# Centralized statistical summary creation
create_statistical_summary <- function(df, numeric_cols = NULL) {
  if (is.null(df)) return(data.frame(Message = "No data available"))
  
  if (is.null(numeric_cols)) {
    numeric_cols <- get_numeric_columns(df)
  }
  
  if (length(numeric_cols) == 0) {
    return(data.frame(Message = "No numeric columns found"))
  }
  
  stats_df <- data.frame(
    Statistic = c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max"),
    stringsAsFactors = FALSE
  )
  
  for (col in numeric_cols) {
    col_data <- df[, col]
    col_data <- col_data[!is.na(col_data)]
    if (length(col_data) > 0) {
      stats_df[[col]] <- c(
        min(col_data),
        quantile(col_data, 0.25),
        median(col_data),
        mean(col_data),
        quantile(col_data, 0.75),
        max(col_data)
      )
    } else {
      stats_df[[col]] <- rep(NA, 6)
    }
  }
  
  return(stats_df)
}

# Centralized safe worksheet creation for Excel exports
safe_add_worksheet <- function(wb, sheet_name, data_func) {
  tryCatch({
    openxlsx::addWorksheet(wb, sheet_name)
    data <- data_func()
    if (!is.null(data) && (is.data.frame(data) && nrow(data) > 0 || is.matrix(data))) {
      openxlsx::writeData(wb, sheet_name, data)
    } else {
      openxlsx::writeData(wb, sheet_name, data.frame(Message = "No data available or insufficient data"))
    }
  }, error = function(e) {
    tryCatch({
      openxlsx::addWorksheet(wb, sheet_name)
      openxlsx::writeData(wb, sheet_name, data.frame(Error = paste("Sheet creation failed:", e$message)))
    }, error = function(e2) {
      log_operation("WARNING", "Could not create worksheet", paste("Sheet:", sheet_name, "Error:", e2$message))
    })
  })
}

# Centralized correlation matrix creation
create_correlation_matrix <- function(df, numeric_cols = NULL) {
  if (is.null(df)) return(data.frame(Message = "No data available"))
  
  if (is.null(numeric_cols)) {
    numeric_cols <- get_numeric_columns(df)
  }
  
  if (length(numeric_cols) < 2) {
    return(data.frame(Message = "Insufficient numeric columns for correlation analysis"))
  }
  
  tryCatch({
    return(cor(df[, numeric_cols, drop = FALSE], use = "complete.obs"))
  }, error = function(e) {
    return(data.frame(Error = paste("Correlation calculation failed:", e$message)))
  })
}

# Centralized success message helper
create_success_message <- function(operation, filename = NULL, details = NULL) {
  base_message <- paste(operation, "completed successfully!")
  if (!is.null(filename)) {
    base_message <- paste(operation, "exported successfully to", filename)
  }
  if (!is.null(details)) {
    base_message <- paste(base_message, details)
  }
  return(base_message)
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

# Correlation heatmap creation function
create_correlation_heatmap <- function(data, output_dir, filename_prefix) {
  if (is.null(data) || nrow(data) == 0) {
    return(NULL)
  }
  
  # Get numeric columns
  numeric_cols <- sapply(data, is.numeric)
  if (sum(numeric_cols) < 2) {
    return(NULL)
  }
  
  numeric_data <- data[, numeric_cols, drop = FALSE]
  
  # Calculate correlation matrix
  cor_matrix <- cor(numeric_data, use = "complete.obs")
  
  # Create output filename
  if (!is.null(output_dir)) {
    filename <- file.path(output_dir, paste0(filename_prefix, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"))
    
    # Create correlation heatmap
    png(filename, width = 800, height = 600, res = 150)
    tryCatch({
      if (requireNamespace("corrplot", quietly = TRUE)) {
        corrplot::corrplot(cor_matrix, method = "color", type = "upper", 
                          order = "hclust", tl.cex = 0.8, tl.col = "black", tl.srt = 45)
      } else {
        # Fallback to base R heatmap
        heatmap(cor_matrix, main = paste("Correlation Heatmap -", filename_prefix))
      }
    }, error = function(e) {
      cat("Correlation heatmap creation failed:", e$message, "\n")
    })
    dev.off()
    
    return(filename)
  }
  
  return(NULL)
}

# Distribution plots creation function
create_distribution_plots <- function(data, output_dir, filename_prefix) {
  if (is.null(data) || nrow(data) == 0) {
    return(NULL)
  }
  
  # Get numeric columns
  numeric_cols <- sapply(data, is.numeric)
  if (sum(numeric_cols) == 0) {
    return(NULL)
  }
  
  numeric_data <- data[, numeric_cols, drop = FALSE]
  
  # Create output filename
  if (!is.null(output_dir)) {
    filename <- file.path(output_dir, paste0(filename_prefix, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"))
    
    # Create distribution plots
    png(filename, width = 1200, height = 800, res = 150)
    tryCatch({
      # Calculate number of plots needed
      n_cols <- min(4, ncol(numeric_data))
      n_rows <- ceiling(ncol(numeric_data) / n_cols)
      
      par(mfrow = c(n_rows, n_cols), mar = c(4, 4, 2, 1))
      
      for (i in 1:ncol(numeric_data)) {
        col_name <- colnames(numeric_data)[i]
        values <- numeric_data[, i]
        values <- values[!is.na(values)]  # Remove NAs
        
        if (length(values) > 0) {
          hist(values, main = col_name, xlab = "Value", ylab = "Frequency", 
               col = "lightblue", border = "black")
        } else {
          plot(1, 1, type = "n", main = paste(col_name, "(No data)"), 
               xlab = "", ylab = "")
        }
      }
    }, error = function(e) {
      cat("Distribution plots creation failed:", e$message, "\n")
    })
    dev.off()
    
    return(filename)
  }
  
  return(NULL)
}

# Note: Functions are exported via NAMESPACE file