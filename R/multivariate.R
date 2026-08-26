# ---- Multivariate Analysis Module ----
# This module handles all multivariate analysis methods including:
# - Standard Mahalanobis Distance
# - Mahalanobis Distance
# - Isolation Forest
# - Data validation for multivariate analysis

#' Validate Mahalanobis distance parameters
#'
#' Raises an error (via `stop()`) for invalid values, and a `warning()` for
#' valid-but-suspicious ones (e.g. an extreme lambda/omega gap, or a very
#' high manual threshold).
#'
#' @param lambda Sensitivity parameter for the automatic threshold formula (non-negative).
#' @param omega Leniency parameter for the automatic threshold formula (non-negative).
#' @param custom_mdthresh Manual distance threshold, required (positive numeric) when `mdthresh_mode == "manual"`.
#' @param mdthresh_mode Either `"auto"` or `"manual"`.
#' @param selected_columns Optional character vector of column names; if
#'   given, must have at least 2 entries.
#' @return `TRUE`, invisibly, if all checks pass.
#' @export
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

#' Validate and clean two datasets for multivariate analysis
#'
#' Checks that `selected_columns` exist, are numeric, and number at least
#' 2, in both datasets; drops incomplete rows; checks each dataset has
#' enough observations relative to the number of variables
#' (`min_obs_ratio`); and warns (without erroring) on zero-variance
#' columns, high pairwise correlations, or a poorly-conditioned covariance
#' matrix in `data2`.
#'
#' @param data1 First dataset, as a data frame.
#' @param data2 Second (typically reference) dataset, as a data frame.
#' @param selected_columns Character vector of numeric column names to
#'   use, mandatory (length >= 2).
#' @param method Method name used only in the sample-size error message.
#'   Default `"Multivariate Analysis"`.
#' @param min_obs_ratio Minimum ratio of complete observations to
#'   variables required in each dataset. Default 2.
#' @return A list: `data1_clean`, `data2_clean` (complete-case data
#'   frames), `common_cols`, `n_vars`, `n_obs1`, `n_obs2`,
#'   `condition_number`, `high_correlations`, `zero_var_cols`.
#' @export
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
    cov_matrix <- cov(data2_clean, use = "complete.obs")
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

#' Compute Mahalanobis distances and flag outliers
#'
#' Computes the Mahalanobis distance of each `data1` row from `data2`'s
#' distribution over `selected_columns`, then flags outliers against
#' either a manual threshold or the automatic formula `MDthresh = MDmean +
#' sqrt(100/(100 + lambda - omega)) * stdMD` (Vode et al., 2022,
#' <https://doi.org/10.3390/ma15020684>).
#'
#' @param data1 Dataset whose points are scored, as a data frame.
#' @param data2 Reference dataset the distribution is fit to, as a data frame.
#' @param lambda Sensitivity parameter for the automatic threshold. Default 1.
#' @param omega Leniency parameter for the automatic threshold. Default 0.
#' @param keep_outliers Currently stored on the result but not used to
#'   filter within this function. Default `FALSE`.
#' @param custom_mdthresh Manual distance threshold, used when `mdthresh_mode == "manual"`.
#' @param selected_columns Character vector of at least 2 numeric column names to use.
#' @param mdthresh_mode Either `"auto"` (default) or `"manual"`.
#' @return A list: `distances`, `MDthresh`, `MDmean`, `stdMD`, `lambda`,
#'   `omega`, `outlier_95`/`outlier_99`/`outlier_custom` (counts),
#'   `total_points`, `df`, `common_cols`, `outlier_indices`,
#'   `keep_outliers`, `threshold_method`, `threshold_formula`.
#' @export
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
  cov_matrix <- cov(data2_clean, use = "complete.obs")
  
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
    # This automatic threshold (MDmean + sqrt(100/(100+lambda-omega)) * stdMD)
    # is not an ad-hoc heuristic - it is the empirical Mahalanobis-distance
    # cutoff formula from:
    #   Vode, F., Tehovnik, F., Kosec, G., Steiner Petrovič, D. (2022).
    #   "Classification of Hot-Rolled Plates Using the Mahalanobis Distance
    #   of NMIs in Ti-Stabilized Austenitic Stainless-Steel Produced by
    #   Secondary Metallurgy." Materials, 15(2), 684. MDPI.
    #   https://doi.org/10.3390/ma15020684
    # lambda/omega are the article's sensitivity/leniency parameters, not
    # general-purpose statistical knobs, which is why they are kept separate
    # from the Z-score/MAD thresholds elsewhere in this codebase.
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

# Robust Mahalanobis function removed - use standard Mahalanobis instead


#' Flag outliers in one dataset using an Isolation Forest trained on another
#'
#' Trains an isolation forest (`isotree::isolation.forest()`) on `data2`
#' (the reference), derives an anomaly-score threshold from `data2`'s own
#' scores at the `1 - contamination` quantile, then scores `data1` against
#' that threshold. Liu, Ting & Zhou (2008), <https://doi.org/10.1109/ICDM.2008.17>.
#'
#' @param data1 Dataset to score for outliers, as a data frame.
#' @param data2 Reference dataset the model is trained on, as a data frame.
#' @param selected_columns Character vector of column names to use for analysis.
#' @param contamination Expected proportion of outliers (0-1). Default 0.10.
#' @param keep_outliers If `TRUE`, `kept_mask`/`filtered_data1` keep the
#'   flagged outliers instead of the inliers. Default `FALSE`.
#' @param ntrees Number of trees in the isolation forest. Default 200.
#' @param sample_size Sample size drawn per tree (capped at `nrow(data2)`). Default 256.
#' @param score_type Score type passed to `predict.isolation_forest()`, `"score"` or `"outlier"`. Default `"score"`.
#' @param seed Random seed for reproducibility. Default 42.
#' @return A list: `model` (the fitted isolation forest), `columns_used`,
#'   `threshold`, `contamination`, `scores` (length `nrow(data1)`),
#'   `outlier_indices`, `kept_mask`, `filtered_data1`, `ref_scores_sum`.
#' @export
compute_isolation_forest <- function(
  data1, data2, selected_columns,
  contamination = 0.10,
  keep_outliers = FALSE,
  ntrees = 200,
  sample_size = 256,
  score_type = "score",
  seed = 42
) {
  stopifnot(is.data.frame(data1), is.data.frame(data2))

  # Check for isotree package
  if (!requireNamespace("isotree", quietly = TRUE)) {
    stop("Package 'isotree' is required for isolation forest outlier detection. Please install it first.")
  }

  # 1) Podnabor in čiščenje
  common_cols <- intersect(selected_columns, intersect(colnames(data1), colnames(data2)))
  if (length(common_cols) < 2L) stop("Premalo skupnih numeričnih spremenljivk.")

  X1 <- data1[, common_cols, drop = FALSE]
  X2 <- data2[, common_cols, drop = FALSE]

  # obdrži samo numerične stolpce
  num_cols <- names(X1)[vapply(X1, is.numeric, logical(1))]
  X1 <- X1[, num_cols, drop = FALSE]
  X2 <- X2[, num_cols, drop = FALSE]
  if (ncol(X1) < 2L) stop("Po filtriranju numeričnih je ostalo premalo stolpcev.")

  # odstrani konstante / NA vrstice
  nzv <- vapply(X2, function(v) length(unique(na.omit(v))) > 1L, logical(1))
  X1 <- X1[, nzv, drop = FALSE]
  X2 <- X2[, nzv, drop = FALSE]
  cc1 <- complete.cases(X1); cc2 <- complete.cases(X2)
  X1c <- X1[cc1, , drop = FALSE]
  X2c <- X2[cc2, , drop = FALSE]

  # 2) Treniranje na referenci
  set.seed(seed)
  ss <- min(sample_size, nrow(X2c))
  iso_model <- isotree::isolation.forest(
    X2c,
    ntrees = ntrees,
    sample_size = ss
  )

  # 3) Prag iz REFERENČNIH score-ov
  scores_ref <- as.numeric(predict(iso_model, X2c, type = score_type))
  threshold  <- as.numeric(stats::quantile(scores_ref, 1 - contamination, na.rm = TRUE))

  # 4) Ocene za data1 + označevanje outlierjev
  scores1_c  <- as.numeric(predict(iso_model, X1c, type = score_type))
  # mapiraj nazaj na originalni red
  scores1 <- rep(NA_real_, nrow(X1)); scores1[cc1] <- scores1_c
  outlier_indices <- !is.na(scores1) & (scores1 >= threshold)

  # 5) Izvoz filtriranih podatkov (po želji)
  kept <- if (keep_outliers) outlier_indices else !outlier_indices
  kept[is.na(kept)] <- FALSE

  return(list(
    model            = iso_model,
    columns_used     = colnames(X1c),
    threshold        = threshold,
    contamination    = contamination,
    scores           = scores1,                  # dolžina = nrow(data1)
    outlier_indices  = outlier_indices,          # logični vektor za data1
    kept_mask        = kept,                     # kaj obdržiš glede na keep_outliers
    filtered_data1   = data1[kept, , drop = FALSE],
    ref_scores_sum   = summary(scores_ref)       # za QC
  ))
}

# Note: Functions are exported via NAMESPACE file

