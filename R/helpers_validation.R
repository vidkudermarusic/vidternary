# ---- Helper Functions Module: Data Quality & Validation ----
# Split out of helpers.R: functions that validate/inspect data quality.

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
