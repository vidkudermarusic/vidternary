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

#' Filter rows by IQR outlier status (positive/high-value side only)
#'
#' Flags a row as an outlier if any of `cols` exceeds `Q3 + multiplier * IQR`
#' (Tukey, 1977).
#'
#' @param data A data frame.
#' @param cols Character vector of numeric column names to check.
#' @param multiplier IQR multiplier for the threshold. Default 1.5.
#' @param keep_outliers If `TRUE`, keep only the flagged rows; if `FALSE`
#'   (default), remove them.
#' @return The filtered data frame.
#' @export
apply_iqr_filter <- function(data, cols, multiplier = 1.5, keep_outliers = FALSE) {
  # A cleared/backspaced numericInput reports as NA_real_ in Shiny, not
  # NULL - is.numeric(NA_real_) is TRUE, so without an explicit is.na()
  # check `multiplier < 0` silently evaluates to NA and if(NA) is a hard
  # "missing value where TRUE/FALSE needed" crash rather than this
  # function's intended graceful stop(), matching the exact hazard already
  # fixed in validate_mahalanobis_inputs() (multivariate.R).
  if (!is.numeric(multiplier) || is.na(multiplier) || multiplier < 0) stop("multiplier must be a non-negative number.")
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

#' Filter rows by Z-score outlier status (positive/high-value side only)
#'
#' Flags a row as an outlier if any of `cols` has a Z-score above `threshold`.
#'
#' @param data A data frame.
#' @param cols Character vector of numeric column names to check.
#' @param threshold Z-score threshold. Default 3.
#' @param keep_outliers If `TRUE`, keep only the flagged rows; if `FALSE`
#'   (default), remove them.
#' @return The filtered data frame.
#' @export
apply_zscore_filter <- function(data, cols, threshold = 3, keep_outliers = FALSE) {
  # Same NA_real_-from-a-cleared-numericInput hazard as apply_iqr_filter()
  # above - see its comment for the full explanation.
  if (!is.numeric(threshold) || is.na(threshold) || threshold < 0) stop("threshold must be a non-negative number.")
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

#' Filter rows by MAD outlier status (positive/high-value side only)
#'
#' Flags a row as an outlier if any of `cols` exceeds
#' `median + threshold * MAD` (Leys et al., 2013).
#'
#' @param data A data frame.
#' @param cols Character vector of numeric column names to check.
#' @param threshold MAD multiplier for the threshold. Default 3.
#' @param keep_outliers If `TRUE`, keep only the flagged rows; if `FALSE`
#'   (default), remove them.
#' @return The filtered data frame.
#' @export
apply_mad_filter <- function(data, cols, threshold = 3, keep_outliers = FALSE) {
  # Same NA_real_-from-a-cleared-numericInput hazard as apply_iqr_filter()
  # above - see its comment for the full explanation.
  if (!is.numeric(threshold) || is.na(threshold) || threshold < 0) stop("threshold must be a non-negative number.")
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

# generate_stats()/compute_correlation() used to live here too - both
# @export'ed, but confirmed dead: a full cross-reference of every call site
# in R/, tests/, and vignettes/ found zero real callers anywhere in the
# current app. Their only actual callers were in legacy/App6.0.1.R (the
# pre-modularization script - not sourced by anything, see this package's
# own "Repository hygiene" audit history for why that's healthy archival
# practice rather than live code). The app's own current, more specialized
# machinery has since grown past what these did: build_descriptive_stats_
# table() (stats_display_utils.R) covers generate_stats()'s mean/median/SD,
# and the Data Comparison tab's own correlation handlers
# (server_data_comparison_stats.R) call stats::cor() directly rather than
# through this wrapper. Confirmed via the user before removing, since both
# were real, documented public API (exported, with their own man/ pages)
# even though nothing internal used them.

# Note: Functions are exported via NAMESPACE file
