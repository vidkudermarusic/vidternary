# ---- Statistical Filtering Module ----
# This module provides various statistical methods for outlier detection and filtering:
# - IQR (Interquartile Range) filtering - detects positive outliers only (values > Q3 + multiplier*IQR)
# - Z-score filtering - detects positive outliers only (z-scores > threshold)
# - MAD (Median Absolute Deviation) filtering - detects positive outliers only (values > median + threshold*MAD)
#
# Note: All methods now focus on positive outliers (unusually high values) rather than both positive and negative outliers.
# This is useful for detecting high-value anomalies while preserving the main data distribution.

# Outlier-flag helpers (positive outliers only). Each returns a full-length
# logical vector aligned 1:1 with nrow(data), TRUE = flagged as outlier.
# NA values in a filtered column never satisfy "> upper_bound" for real, but
# comparing against NA yields NA (not FALSE) in R, and combining with `|`
# then propagates that NA into outlier_indices. Left unguarded, indexing a
# data frame with an NA logical/position silently inserts a phantom all-NA
# row instead of keeping or dropping the real one - so NA comparisons are
# explicitly coerced to FALSE (never flagged as an outlier) below.
get_iqr_outlier_flags <- function(data, cols, multiplier = 1.5) {
  outlier_indices <- logical(nrow(data))
  for (col in cols) {
    if (is.numeric(data[[col]])) {
      q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
      q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      upper_bound <- q3 + multiplier * iqr

      # Only consider positive outliers (values above upper bound)
      col_outliers <- data[[col]] > upper_bound
      col_outliers[is.na(col_outliers)] <- FALSE
      outlier_indices <- outlier_indices | col_outliers
    }
  }
  outlier_indices
}

get_zscore_outlier_flags <- function(data, cols, threshold = 3) {
  outlier_indices <- logical(nrow(data))
  for (col in cols) {
    if (is.numeric(data[[col]])) {
      z_scores <- as.numeric(scale(data[[col]]))
      # Only consider positive outliers (positive z-scores above threshold)
      col_outliers <- z_scores > threshold
      col_outliers[is.na(col_outliers)] <- FALSE
      outlier_indices <- outlier_indices | col_outliers
    }
  }
  outlier_indices
}

get_mad_outlier_flags <- function(data, cols, threshold = 3) {
  outlier_indices <- logical(nrow(data))
  for (col in cols) {
    if (is.numeric(data[[col]])) {
      median_val <- median(data[[col]], na.rm = TRUE)
      mad_val <- mad(data[[col]], na.rm = TRUE)
      upper_bound <- median_val + threshold * mad_val

      # Only consider positive outliers (values above upper bound)
      col_outliers <- data[[col]] > upper_bound
      col_outliers[is.na(col_outliers)] <- FALSE
      outlier_indices <- outlier_indices | col_outliers
    }
  }
  outlier_indices
}

# IQR-based outlier filtering (positive outliers only)
apply_iqr_filter <- function(data, cols, multiplier = 1.5, keep_outliers = FALSE) {
  # IQR-based outlier filtering - only considers positive outliers (values > Q3 + multiplier*IQR)
  outlier_indices <- get_iqr_outlier_flags(data, cols, multiplier)

  if (keep_outliers) {
    # Keep only positive outliers
    filtered_data <- data[outlier_indices, , drop = FALSE]
  } else {
    # Remove positive outliers
    filtered_data <- data[!outlier_indices, , drop = FALSE]
  }

  return(filtered_data)
}

# Z-score based outlier filtering (positive outliers only)
apply_zscore_filter <- function(data, cols, threshold = 3, keep_outliers = FALSE) {
  # Z-score based outlier filtering - only considers positive outliers (z-scores > threshold)
  outlier_indices <- get_zscore_outlier_flags(data, cols, threshold)

  if (keep_outliers) {
    # Keep only positive outliers
    filtered_data <- data[outlier_indices, , drop = FALSE]
  } else {
    # Remove positive outliers
    filtered_data <- data[!outlier_indices, , drop = FALSE]
  }

  return(filtered_data)
}

# Median Absolute Deviation (MAD) based filtering (positive outliers only)
apply_mad_filter <- function(data, cols, threshold = 3, keep_outliers = FALSE) {
  # Median Absolute Deviation (MAD) based filtering - only considers positive outliers (values > median + threshold*MAD)
  outlier_indices <- get_mad_outlier_flags(data, cols, threshold)

  if (keep_outliers) {
    # Keep only positive outliers
    filtered_data <- data[outlier_indices, , drop = FALSE]
  } else {
    # Remove positive outliers
    filtered_data <- data[!outlier_indices, , drop = FALSE]
  }

  return(filtered_data)
}

# Note: check_data_quality function is now in helpers.R to avoid duplication
# Use the version from helpers.R for comprehensive data quality checking

# Note: calculate_quality_score function is now in helpers.R to avoid duplication
# Use the version from helpers.R for comprehensive quality scoring

# Basic statistical functions
# Note: validate_data function is now in helpers.R as validate_data_enhanced to avoid duplication
# Use the enhanced version from helpers.R for comprehensive data validation

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

# Note: Functions are exported via NAMESPACE file
