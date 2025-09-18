# =============================================================================
# vidternary: Multivariate Analysis Module
# =============================================================================
# 
# Package:     vidternary
# Version:     1.0.0
# Author:      Vid Kuder Marušič <vidkm30@gmail.com>
# Maintainer:  Vid Kuder Marušič <vidkm30@gmail.com>
# License:     MIT + file LICENSE
# Repository:  https://github.com/vidkudermarusic/vidternary
# 
# Description: Multivariate analysis methods for outlier detection and data
#              filtering including Mahalanobis Distance, , and
#              Isolation Forest algorithms.
# 
# Key Functions:
#   - compute_mahalanobis_distance(): Standard Mahalanobis distance calculation
#   - compute_isolation_forest(): Isolation Forest outlier detection
#   - validate_mahalanobis_inputs(): Input validation for multivariate analysis
# 
# Dependencies:
#   - R (>= 4.0.0)
#   - robustbase, isotree, moments
# 
# Last Modified: 2025-09-07
# 
# =============================================================================

# Input validation function for Mahalanobis distance parameters
validate_mahalanobis_inputs <- function(lambda, omega, custom_mdthresh, mdthresh_mode, selected_columns) {
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

# Unified validation function for multivariate analysis
validate_multivariate_data <- function(data1, data2, selected_columns, method = "Multivariate Analysis", min_obs_ratio = 2) {
  # MANDATORY COLUMN SELECTION: User must select columns for multivariate analysis
  if (is.null(selected_columns) || length(selected_columns) == 0) {
    stop("Column selection is MANDATORY for multivariate analysis. Please select at least 2 numeric columns before proceeding.")
  }
  
  # Validate that selected columns exist in both datasets
  missing_in_data1 <- setdiff(selected_columns, colnames(data1))
  missing_in_data2 <- setdiff(selected_columns, colnames(data2))
  
  if (length(missing_in_data1) > 0) {
    stop("Selected columns missing in data1: ", paste(missing_in_data1, collapse = ", "))
  }
  if (length(missing_in_data2) > 0) {
    stop("Selected columns missing in data2: ", paste(missing_in_data2, collapse = ", "))
  }
  
  # Check that selected columns are numeric
  non_numeric_in_data1 <- selected_columns[!sapply(data1[, selected_columns, drop = FALSE], is.numeric)]
  non_numeric_in_data2 <- selected_columns[!sapply(data2[, selected_columns, drop = FALSE], is.numeric)]
  
  if (length(non_numeric_in_data1) > 0) {
    stop("Non-numeric selected columns in data1: ", paste(non_numeric_in_data1, collapse = ", "))
  }
  if (length(non_numeric_in_data2) > 0) {
    stop("Non-numeric selected columns in data2: ", paste(non_numeric_in_data2, collapse = ", "))
  }
  
  # Ensure minimum number of columns
  if (length(selected_columns) < 2) {
    stop("At least 2 numeric columns must be selected for multivariate analysis. Currently selected: ", length(selected_columns))
  }
  
  common_cols <- selected_columns
  
  # Extract common columns and remove rows with missing values
  data1_clean <- data1[, common_cols, drop = FALSE]
  data2_clean <- data2[, common_cols, drop = FALSE]
  
  # Remove rows with missing values
  complete_cases1 <- complete.cases(data1_clean)
  complete_cases2 <- complete.cases(data2_clean)
  
  data1_clean <- data1_clean[complete_cases1, , drop = FALSE]
  data2_clean <- data2_clean[complete_cases2, , drop = FALSE]
  
  # Check sample size requirements
  n_vars <- length(common_cols)
  n_obs1 <- nrow(data1_clean)
  n_obs2 <- nrow(data2_clean)
  
  if (n_obs1 < n_vars * min_obs_ratio) {
    stop(sprintf("Insufficient observations in data1: %d observations for %d variables (need at least %d)", 
                 n_obs1, n_vars, n_vars * min_obs_ratio))
  }
  
  if (n_obs2 < n_vars * min_obs_ratio) {
    stop(sprintf("Insufficient observations in data2: %d observations for %d variables (need at least %d)", 
                 n_obs2, n_vars, n_vars * min_obs_ratio))
  }
  
  # Check for zero variance columns
  zero_var_cols <- sapply(common_cols, function(col) {
    var1 <- var(data1_clean[[col]], na.rm = TRUE)
    var2 <- var(data2_clean[[col]], na.rm = TRUE)
    var1 == 0 || var2 == 0
  })
  
  if (any(zero_var_cols)) {
    zero_var_names <- common_cols[zero_var_cols]
    warning("Zero variance columns detected: ", paste(zero_var_names, collapse = ", "), 
            ". These may cause issues in multivariate analysis.")
  }
  
  # Check for high correlations (potential multicollinearity)
  high_correlations <- NULL
  if (n_vars > 2) {
    cor_matrix1 <- cor(data1_clean, use = "pairwise.complete.obs")
    cor_matrix2 <- cor(data2_clean, use = "pairwise.complete.obs")
    
    # Find high correlations (>0.9) excluding diagonal
    high_cor1 <- which(abs(cor_matrix1) > 0.9 & cor_matrix1 != 1, arr.ind = TRUE)
    high_cor2 <- which(abs(cor_matrix2) > 0.9 & cor_matrix2 != 1, arr.ind = TRUE)
    
    if (nrow(high_cor1) > 0 || nrow(high_cor2) > 0) {
      high_correlations <- list(
        data1 = if (nrow(high_cor1) > 0) {
          data.frame(
            var1 = rownames(cor_matrix1)[high_cor1[, 1]],
            var2 = colnames(cor_matrix1)[high_cor1[, 2]],
            correlation = cor_matrix1[high_cor1]
          )
        } else NULL,
        data2 = if (nrow(high_cor2) > 0) {
          data.frame(
            var1 = rownames(cor_matrix2)[high_cor2[, 1]],
            var2 = colnames(cor_matrix2)[high_cor2[, 2]],
            correlation = cor_matrix2[high_cor2]
          )
        } else NULL
      )
      
      warning("High correlations (>0.9) detected. This may cause multicollinearity issues.")
    }
  }
  
  # Check condition number for numerical stability
  tryCatch({
    # Use data2 (reference) for condition number calculation
    cov_matrix <- cov(data2_clean)
    eigenvals <- eigen(cov_matrix, only.values = TRUE)$values
    condition_number <- max(eigenvals) / min(eigenvals)
  }, error = function(e) {
    condition_number <- NA
    warning("Could not calculate condition number: ", e$message)
  })
  
  if (!is.na(condition_number) && condition_number > 1e10) {
    warning("High condition number (", format(condition_number, scientific = TRUE), 
            ") detected. This may indicate numerical instability.")
  }
  
  return(list(
    data1_clean = data1_clean,
    data2_clean = data2_clean,
    common_cols = common_cols,
    n_vars = n_vars,
    n_obs1 = n_obs1,
    n_obs2 = n_obs2,
    condition_number = condition_number,
    high_correlations = high_correlations,
    zero_var_cols = common_cols[zero_var_cols]
  ))
}

# Standard Mahalanobis Distance Function
compute_mahalanobis_distance <- function(data1, data2, lambda = 1, omega = 0, keep_outliers = FALSE, custom_mdthresh = NULL, selected_columns, mdthresh_mode = "auto") {
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
  
  # Check for singular matrix and handle it
  tryCatch({
    # Try to calculate the inverse of the covariance matrix
    cov_inv <- solve(cov_matrix)
  }, error = function(e) {
    if (grepl("singular", e$message, ignore.case = TRUE)) {
      # If matrix is singular, try using a regularized version
      if (getOption("ternary.debug", FALSE)) {
        cat("DEBUG: Singular covariance matrix detected. Applying regularization.\n")
      }
      
      # Add small regularization to diagonal
      n_vars <- ncol(cov_matrix)
      regularization <- 1e-6 * diag(n_vars)
      cov_matrix <<- cov_matrix + regularization
      
      # Try again
      cov_inv <<- solve(cov_matrix)
      
      if (getOption("ternary.debug", FALSE)) {
        cat("DEBUG: Regularization applied successfully.\n")
      }
    } else {
      stop("Covariance matrix error: ", e$message)
    }
  })
  
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
  
  # Calculate additional thresholds for reference
  threshold_95 <- qchisq(0.95, df = length(common_cols))
  threshold_99 <- qchisq(0.99, df = length(common_cols))
  
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
    df = length(common_cols),
    common_cols = common_cols,
    outlier_indices = outlier_indices,
    keep_outliers = keep_outliers,
    threshold_method = threshold_method,
    threshold_formula = threshold_formula
  ))
}

# Robust Mahalanobis using Minimum Covariance Determinant (MCD)
compute_robust_mahalanobis <- function(data1, data2, method = "MCD", keep_outliers = FALSE, selected_columns) {
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
  
  # Additional safety check: Remove zero-variance columns and non-numeric data
  zero_var_cols <- c()
  non_numeric_cols <- c()
  
  for (col in common_cols) {
    # Check for zero variance
    if (var(data2_clean[, col], na.rm = TRUE) == 0 || 
        all(data2_clean[, col] == 0, na.rm = TRUE) ||
        all(is.na(data2_clean[, col]))) {
      zero_var_cols <- c(zero_var_cols, col)
    }
    
    # Check for non-numeric data
    if (!is.numeric(data2_clean[, col]) || any(is.infinite(data2_clean[, col]), na.rm = TRUE)) {
      non_numeric_cols <- c(non_numeric_cols, col)
    }
  }
  
  # Remove problematic columns
  cols_to_remove <- unique(c(zero_var_cols, non_numeric_cols))
  
  if (length(cols_to_remove) > 0) {
    debug_log("DEBUG: Removing problematic columns: %s", paste(cols_to_remove, collapse = ", "))
    data1_clean <- data1_clean[, !colnames(data1_clean) %in% cols_to_remove, drop = FALSE]
    data2_clean <- data2_clean[, !colnames(data2_clean) %in% cols_to_remove, drop = FALSE]
    common_cols <- common_cols[!common_cols %in% cols_to_remove]
  }
  
  # Check if we still have enough columns
  if (ncol(data1_clean) < 2) {
    stop("Not enough valid columns for robust Mahalanobis analysis after removing zero-variance and non-numeric columns")
  }
  
  # Final data type check and conversion
  data1_clean <- as.data.frame(lapply(data1_clean, as.numeric))
  data2_clean <- as.data.frame(lapply(data2_clean, as.numeric))
  
  # Remove any remaining infinite or NA values
  data1_clean <- data1_clean[complete.cases(data1_clean), , drop = FALSE]
  data2_clean <- data2_clean[complete.cases(data2_clean), , drop = FALSE]
  
  # Log validation results for debugging
  debug_log("DEBUG: Robust Mahalanobis validation passed")
  debug_log("DEBUG: Variables: %d", ncol(data1_clean))
  debug_log("DEBUG: Observations (data): %d", nrow(data1_clean))
  debug_log("DEBUG: Observations (reference): %d", nrow(data2_clean))
  
  # Calculate robust covariance matrix using MCD
  tryCatch({
    if (method == "MCD") {
      # Use a more conservative alpha for better stability
      alpha_val <- min(0.75, max(0.5, (nrow(data2_clean) - ncol(data2_clean)) / nrow(data2_clean)))
      robust_cov <- robustbase::covMcd(data2_clean, alpha = alpha_val)
      robust_center <- robust_cov$center
      robust_cov_matrix <- robust_cov$cov
    } else {
      # Use a more conservative alpha for better stability
      alpha_val <- min(0.75, max(0.5, (nrow(data2_clean) - ncol(data2_clean)) / nrow(data2_clean)))
      robust_cov <- robustbase::covMve(data2_clean, alpha = alpha_val)
      robust_center <- robust_cov$center
      robust_cov_matrix <- robust_cov$cov
    }
  }, error = function(e) {
    # If robust estimation fails, fall back to regular covariance with regularization
    warning("Robust covariance estimation failed, using regular covariance with regularization: ", e$message)
    robust_cov_matrix <- cov(data2_clean)
    robust_center <- colMeans(data2_clean)
    
    # Add regularization to prevent singularity
    diag(robust_cov_matrix) <- diag(robust_cov_matrix) + 1e-6
  })
  
  # Check for singular matrix and handle it
  robust_cov_inv <- tryCatch({
    solve(robust_cov_matrix)
  }, error = function(e) {
    if (grepl("singular", e$message, ignore.case = TRUE)) {
      # Add small regularization to diagonal
      diag(robust_cov_matrix) <- diag(robust_cov_matrix) + 1e-6
      solve(robust_cov_matrix)
    } else {
      stop("Failed to compute robust covariance inverse: ", e$message)
    }
  })
  
  # Calculate robust Mahalanobis distances using vectorized operations
  tryCatch({
    # Convert to matrices for efficient computation
    data1_matrix <- as.matrix(data1_clean)
    center_vector <- as.numeric(robust_center)
    
    # Calculate differences
    diff_matrix <- sweep(data1_matrix, 2, center_vector, "-")
    
    # Calculate Mahalanobis distances using vectorized operations
    mahal_distances <- sqrt(rowSums((diff_matrix %*% robust_cov_inv) * diff_matrix))
    
    # Ensure all distances are finite
    mahal_distances[!is.finite(mahal_distances)] <- 0
    
  }, error = function(e) {
    # Fallback to individual calculation if vectorized fails
    warning("Vectorized calculation failed, using individual calculations: ", e$message)
    mahal_distances <- numeric(nrow(data1_clean))
    for (i in 1:nrow(data1_clean)) {
      tryCatch({
        diff_vector <- as.numeric(data1_clean[i, ]) - as.numeric(robust_center)
        mahal_distances[i] <- sqrt(as.numeric(diff_vector %*% robust_cov_inv %*% diff_vector))
      }, error = function(e2) {
        mahal_distances[i] <<- 0
      })
    }
  })
  
  # Calculate robust statistics
  robust_mean <- mean(mahal_distances, na.rm = TRUE)
  robust_sd <- sd(mahal_distances, na.rm = TRUE)
  
  # Calculate thresholds
  threshold_95 <- qchisq(0.95, df = length(common_cols))
  threshold_99 <- qchisq(0.99, df = length(common_cols))
  
  # Use robust mean + 2*robust_sd as threshold
  robust_threshold <- robust_mean + 2 * robust_sd
  
  # Identify outliers
  outlier_indices <- mahal_distances > robust_threshold
  
  return(list(
    distances = mahal_distances,
    MDthresh = robust_threshold,
    MDmean = robust_mean,
    stdMD = robust_sd,
    outlier_95 = sum(mahal_distances > threshold_95),
    outlier_99 = sum(mahal_distances > threshold_99),
    outlier_custom = sum(outlier_indices),
    total_points = length(mahal_distances),
    df = length(common_cols),
    common_cols = common_cols,
    outlier_indices = outlier_indices,
    keep_outliers = keep_outliers,
    threshold_method = paste("Robust", method, "(mean + 2*SD)"),
    robust_center = robust_center,
    method = paste("Robust Mahalanobis (", method, ")")
  ))
}

# Alias for the function name used in server_data_comparison.R
compute_robust_mahalanobis_distance <- function(data1, data2, keep_outliers = FALSE, selected_columns) {
  return(compute_robust_mahalanobis(data1, data2, method = "MCD", keep_outliers = keep_outliers, selected_columns = selected_columns))
}

# Isolation Forest for outlier detection
compute_isolation_forest <- function(data1, data2, contamination = 0.1, keep_outliers = FALSE, selected_columns) {
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
    iso_model <- isotree::isolation.forest(data2_clean)
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

# Note: Functions are exported via NAMESPACE file

