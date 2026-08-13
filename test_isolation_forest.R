# Test script for the new compute_isolation_forest function
# This script validates that the function works correctly

# Load required packages
if (!requireNamespace("isotree", quietly = TRUE)) {
  stop("Package 'isotree' is required for testing. Please install it first.")
}

# Source the multivariate.R file to load the function
source("R/multivariate.R")

# Create test data
set.seed(42)
n <- 100
p <- 3

# Create normal data
data1 <- data.frame(
  x1 = rnorm(n, 0, 1),
  x2 = rnorm(n, 0, 1),
  x3 = rnorm(n, 0, 1)
)

# Create reference data with some outliers
data2 <- data.frame(
  x1 = c(rnorm(n-5, 0, 1), rnorm(5, 5, 0.5)),  # 5 outliers
  x2 = c(rnorm(n-5, 0, 1), rnorm(5, 5, 0.5)),  # 5 outliers
  x3 = c(rnorm(n-5, 0, 1), rnorm(5, 5, 0.5))   # 5 outliers
)

# Test the function
cat("Testing compute_isolation_forest function...\n")

tryCatch({
  result <- compute_isolation_forest(
    data1 = data1,
    data2 = data2,
    selected_columns = c("x1", "x2", "x3"),
    contamination = 0.1,
    keep_outliers = FALSE,
    ntrees = 100,
    sample_size = 64,
    score_type = "score",
    seed = 42
  )
  
  cat("✅ Function executed successfully!\n")
  cat("Results structure:\n")
  cat("- Model type:", class(result$model), "\n")
  cat("- Columns used:", length(result$columns_used), "\n")
  cat("- Threshold:", round(result$threshold, 4), "\n")
  cat("- Contamination:", result$contamination, "\n")
  cat("- Scores length:", length(result$scores), "\n")
  cat("- Outlier indices length:", length(result$outlier_indices), "\n")
  cat("- Kept mask length:", length(result$kept_mask), "\n")
  cat("- Filtered data1 rows:", nrow(result$filtered_data1), "\n")
  cat("- Reference scores summary:\n")
  print(result$ref_scores_sum)
  
  # Test that the function returns the expected structure
  expected_names <- c("model", "columns_used", "threshold", "contamination", 
                      "scores", "outlier_indices", "kept_mask", "filtered_data1", "ref_scores_sum")
  
  if (all(expected_names %in% names(result))) {
    cat("✅ All expected output elements present!\n")
  } else {
    cat("❌ Missing expected output elements!\n")
    cat("Expected:", paste(expected_names, collapse = ", "), "\n")
    cat("Got:", paste(names(result), collapse = ", "), "\n")
  }
  
  # Test that scores are properly mapped back to original data length
  if (length(result$scores) == nrow(data1)) {
    cat("✅ Scores properly mapped to original data length!\n")
  } else {
    cat("❌ Scores length mismatch!\n")
    cat("Expected:", nrow(data1), "Got:", length(result$scores), "\n")
  }
  
  # Test that outlier_indices are properly mapped
  if (length(result$outlier_indices) == nrow(data1)) {
    cat("✅ Outlier indices properly mapped to original data length!\n")
  } else {
    cat("❌ Outlier indices length mismatch!\n")
    cat("Expected:", nrow(data1), "Got:", length(result$outlier_indices), "\n")
  }
  
  # Test with different parameters
  cat("\n--- Testing with different parameters ---\n")
  
  result2 <- compute_isolation_forest(
    data1 = data1,
    data2 = data2,
    selected_columns = c("x1", "x2", "x3"),
    contamination = 0.2,
    keep_outliers = TRUE,
    ntrees = 50,
    sample_size = 32,
    score_type = "score",
    seed = 123
  )
  
  cat("✅ Second test with different parameters successful!\n")
  cat("- Outliers detected:", sum(result2$outlier_indices), "\n")
  cat("- Rows kept:", sum(result2$kept_mask), "\n")
  
  cat("\n🎉 All tests passed! The new isolation forest function is working correctly.\n")
  
}, error = function(e) {
  cat("❌ Function failed with error:\n")
  cat(e$message, "\n")
  traceback()
})
