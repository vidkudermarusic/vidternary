# =============================================================================
# vidternary: Comprehensive Analysis Pipeline Module
# =============================================================================
# 
# Package:     vidternary
# Version:     1.0.0
# Author:      Vid Kuder Marušič <vidkm30@gmail.com>
# Maintainer:  Vid Kuder Marušič <vidkm30@gmail.com>
# License:     MIT + file LICENSE
# Repository:  https://github.com/vidkudermarusic/vidternary
# 
# Description: End-to-end analysis workflows that orchestrate all existing
#              functions for complete data analysis and reporting.
# 
# Key Functions:
#   - run_comprehensive_analysis(): Complete analysis pipeline
#   - generate_comprehensive_report(): Generate detailed analysis reports
#   - run_quick_analysis(): Quick analysis workflow
#   - create_analysis_output_dir(): Analysis output directory management
# 
# Dependencies:
#   - R (>= 4.0.0)
#   - openxlsx, ggplot2, corrplot, writexl
# 
# Last Modified: 2025-09-07
# 
# =============================================================================

# Comprehensive Analysis Pipeline
run_comprehensive_analysis <- function(data1, data2, analysis_config = NULL, output_dir = NULL) {
  if (is.null(output_dir)) {
    output_dir <- file.path(getwd(), "analysis_output")
  }
  
  # Create output directory using file management module
  if (!is.null(output_dir)) {
    safe_create_directory(output_dir, recursive = TRUE)
  }
  
  # Start overall performance monitoring
  start_performance_monitor("comprehensive_analysis")
  
  # Initialize results container
  results <- list(
    timestamp = Sys.time(),
    config = analysis_config,
    output_dir = output_dir
  )
  
  cat("=== Starting Comprehensive Analysis ===\n")
  cat("Output directory:", output_dir, "\n")
  cat("Dataset 1 dimensions:", dim(data1), "\n")
  cat("Dataset 2 dimensions:", dim(data2), "\n\n")
  
  # Step 1: Data Quality Assessment
  cat("Step 1: Assessing data quality...\n")
  start_progress("Data quality assessment", 6)
  
  quality_report <- check_data_quality(data1, data2)
  results$quality_report <- quality_report
  
  # Create quality dashboard
  quality_dashboard <- create_quality_dashboard(quality_report, output_dir)
  results$quality_dashboard <- quality_dashboard
  
  update_progress(1, "Data quality assessment completed")
  
  # Step 2: Data Visualization
  cat("Step 2: Creating visualizations...\n")
  start_progress("Data visualization", 6)
  
  # Correlation heatmaps
  cor_heatmap1 <- create_correlation_heatmap(data1, output_dir, "dataset1_correlation")
  cor_heatmap2 <- create_correlation_heatmap(data2, output_dir, "dataset2_correlation")
  
  # Distribution plots
  dist_plots1 <- create_distribution_plots(data1, output_dir, "dataset1_distributions")
  dist_plots2 <- create_distribution_plots(data2, output_dir, "dataset2_distributions")
  
  results$visualizations <- list(
    correlation_heatmaps = list(dataset1 = cor_heatmap1, dataset2 = cor_heatmap2),
    distribution_plots = list(dataset1 = dist_plots1, dataset2 = dist_plots2)
  )
  
  update_progress(2, "Data visualization completed")
  
  # Step 3: Statistical Analysis
  cat("Step 3: Performing statistical analysis...\n")
  start_progress("Statistical analysis", 6)
  
  # Basic statistics
  stats1 <- generate_stats(data1, colnames(data1)[sapply(data1, is.numeric)])
  stats2 <- generate_stats(data2, colnames(data2)[sapply(data2, is.numeric)])
  
  # Correlation analysis
  cor1 <- compute_correlation(data1, colnames(data1)[sapply(data1, is.numeric)])
  cor2 <- compute_correlation(data2, colnames(data2)[sapply(data2, is.numeric)])
  
  results$statistics <- list(
    dataset1 = stats1,
    dataset2 = stats2,
    correlations = list(dataset1 = cor1, dataset2 = cor2)
  )
  
  update_progress(3, "Statistical analysis completed")
  
  # Step 4: Multivariate Analysis
  cat("Step 4: Performing multivariate analysis...\n")
  start_progress("Multivariate analysis", 6)
  
  # Get numeric columns for multivariate analysis
  numeric_cols1 <- colnames(data1)[sapply(data1, is.numeric)]
  numeric_cols2 <- colnames(data2)[sapply(data2, is.numeric)]
  common_numeric_cols <- intersect(numeric_cols1, numeric_cols2)
  
  if (length(common_numeric_cols) >= 2) {
    # Standard Mahalanobis
    tryCatch({
      mahal_result <- compute_mahalanobis_distance(data1, data2, 
                                                   selected_columns = common_numeric_cols[seq_len(min(5, length(common_numeric_cols)))],
                                                   mdthresh_mode = "auto")
      results$mahalanobis_standard <- mahal_result
    }, error = function(e) {
      cat("Standard Mahalanobis failed:", e$message, "\n")
    })
    
    # Robust Mahalanobis
    tryCatch({
      robust_result <- compute_robust_mahalanobis(data1, data2, 
                                                  selected_columns = common_numeric_cols[seq_len(min(5, length(common_numeric_cols)))])
      results$mahalanobis_robust <- robust_result
    }, error = function(e) {
      cat("Robust Mahalanobis failed:", e$message, "\n")
    })
    
    # Isolation Forest
    tryCatch({
      isolation_result <- compute_isolation_forest(data1, data2, 
                                                   contamination = 0.1,
                                                   keep_outliers = FALSE,
                                                   selected_columns = common_numeric_cols[seq_len(min(5, length(common_numeric_cols)))])
      results$isolation_forest <- isolation_result
    }, error = function(e) {
      cat("Isolation Forest failed:", e$message, "\n")
    })
  }
  
  update_progress(4, "Multivariate analysis completed")
  
  # Step 5: Data Export (fixed, explicit paths)
  cat("Step 5: Exporting results.\n")
  start_progress("Data export", 6)
  
  safe_create_directory(output_dir, recursive = TRUE)
  
  files <- list(
    dataset1_csv        = file.path(output_dir, "dataset1.csv"),
    dataset2_csv        = file.path(output_dir, "dataset2.csv"),
    quality_summary_csv = file.path(output_dir, "quality_summary.csv")
  )
  
  write.csv(data1, files$dataset1_csv, row.names = FALSE)
  write.csv(data2, files$dataset2_csv, row.names = FALSE)
  
  quality_summary_df <- data.frame(
    metric   = c("Missing Values","Infinite Values","Zero Variance","Outliers"),
        dataset1 = c(sum(quality_report$missing_values$data1),
                     sum(quality_report$missing_values$data1),
                     sum(quality_report$zero_variance$data1),
                     sum(quality_report$outliers_iqr$data1)),
        dataset2 = c(sum(quality_report$missing_values$data2),
                     sum(quality_report$missing_values$data2),
                     sum(quality_report$zero_variance$data2),
                     sum(quality_report$outliers_iqr$data2))
      )
  write.csv(quality_summary_df, files$quality_summary_csv, row.names = FALSE)
  
  results$export_files <- files
  
  update_progress(5, "Data export completed")
  
  # Step 6: Generate Report
  cat("Step 6: Generating final report...\n")
  start_progress("Report generation", 6)
  
  # Create comprehensive report
  report_file <- generate_comprehensive_report(results, output_dir)
  results$final_report <- report_file
  
  update_progress(6, "Report generation completed")
  
  # Complete analysis
  complete_progress("comprehensive_analysis")
  end_performance_monitor("comprehensive_analysis")
  
  cat("\n=== Comprehensive Analysis Completed ===\n")
  cat("Results saved to:", output_dir, "\n")
  cat("Performance summary:\n")
  cat(get_performance_summary())
  
  return(results)
}

# Generate comprehensive report
generate_comprehensive_report <- function(results, output_dir) {
  report_html <- paste0(
    "<!DOCTYPE html>",
    "<html><head>",
    "<title>Comprehensive Analysis Report</title>",
    "<style>",
    "body { font-family: Arial, sans-serif; margin: 20px; line-height: 1.6; }",
    ".header { background: #2c3e50; color: white; padding: 20px; border-radius: 5px; }",
    ".section { background: #f8f9fa; padding: 15px; margin: 15px 0; border-radius: 5px; border-left: 4px solid #007bff; }",
    ".metric { background: white; padding: 10px; margin: 10px 0; border-radius: 3px; box-shadow: 0 1px 3px rgba(0,0,0,0.1); }",
    ".score { font-size: 20px; font-weight: bold; }",
    ".grade-A { color: #28a745; }",
    ".grade-B { color: #17a2b8; }",
    ".grade-C { color: #ffc107; }",
    ".grade-D { color: #fd7e14; }",
    ".grade-F { color: #dc3545; }",
    "table { border-collapse: collapse; margin: 10px 0; }",
    "th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }",
    "th { background-color: #f2f2f2; }",
    ".file-list { background: #e9ecef; padding: 10px; border-radius: 3px; }",
    "</style></head><body>",
    
    "<div class='header'>",
    "<h1>Comprehensive Analysis Report</h1>",
    "<p>Generated on: ", format(results$timestamp, "%Y-%m-%d %H:%M:%S"), "</p>",
    "</div>",
    
    "<div class='section'>",
    "<h2>Executive Summary</h2>",
    "<p>This report presents a comprehensive analysis of two datasets including data quality assessment, ",
    "statistical analysis, multivariate analysis, and visualization results.</p>",
    "</div>",
    
    "<div class='section'>",
    "<h2>Data Quality Assessment</h2>",
    "<div class='metric'>",
    "<h3>Dataset 1 Quality Score</h3>",
    "<div class='score grade-", results$quality_report$quality_score$data1$grade, "'>",
    "Score: ", results$quality_report$quality_score$data1$score, "/100 (Grade: ", results$quality_report$quality_score$data1$grade, ")</div>",
    "</div>",
    "<div class='metric'>",
    "<h3>Dataset 2 Quality Score</h3>",
    "<div class='score grade-", results$quality_report$quality_score$data2$grade, "'>",
    "Score: ", results$quality_report$quality_score$data2$score, "/100 (Grade: ", results$quality_report$quality_score$data2$grade, ")</div>",
    "</div>",
    "</div>",
    
    "<div class='section'>",
    "<h2>Generated Files</h2>",
    "<div class='file-list'>",
    "<h3>Visualizations</h3>",
    "<ul>",
    ifelse(!is.null(results$visualizations$correlation_heatmaps$dataset1), 
           "<li>Dataset 1 Correlation Heatmap</li>", ""),
    ifelse(!is.null(results$visualizations$correlation_heatmaps$dataset2), 
           "<li>Dataset 2 Correlation Heatmap</li>", ""),
    ifelse(!is.null(results$visualizations$distribution_plots$dataset1), 
           "<li>Dataset 1 Distribution Plots</li>", ""),
    ifelse(!is.null(results$visualizations$distribution_plots$dataset2), 
           "<li>Dataset 2 Distribution Plots</li>", ""),
    "</ul>",
    "<h3>Data Exports</h3>",
    "<ul>",
    if (is.list(results$export_files)) {
      paste0("<li>", names(results$export_files), ": ",
             basename(unlist(results$export_files)), "</li>", collapse = "")
    } else {
      paste0("<li>", basename(results$export_files), "</li>", collapse = "")
    },
    "</ul>",
    "</div>",
    "</div>",
    
    "<div class='section'>",
    "<h2>Analysis Details</h2>",
    "<p>For detailed analysis results, please refer to the individual files listed above.</p>",
    "<p>All results are saved in: ", output_dir, "</p>",
    "</div>",
    
    "</body></html>"
  )
  
  report_file <- file.path(output_dir, "comprehensive_analysis_report.html")
  writeLines(report_html, report_file)
  cat("Comprehensive report created:", report_file, "\n")
  return(report_file)
}

# Quick Analysis Function (Simplified version)
run_quick_analysis <- function(data1, data2, output_dir = NULL) {
  if (is.null(output_dir)) {
    output_dir <- file.path(getwd(), "quick_analysis")
  }
  
  # Create output directory
  safe_create_directory(output_dir, recursive = TRUE)
  
  cat("=== Starting Quick Analysis ===\n")
  cat("Output directory:", output_dir, "\n")
  
  # Run essential analyses only
  results <- list(
    timestamp = Sys.time(),
    output_dir = output_dir
  )
  
  # Data quality check
  quality_report <- check_data_quality(data1, data2)
  results$quality_report <- quality_report
  
  # Basic statistics
  numeric_cols1 <- colnames(data1)[sapply(data1, is.numeric)]
  numeric_cols2 <- colnames(data2)[sapply(data2, is.numeric)]
  
  if (length(numeric_cols1) > 0) {
    results$stats1 <- generate_stats(data1, numeric_cols1)
  }
  if (length(numeric_cols2) > 0) {
    results$stats2 <- generate_stats(data2, numeric_cols2)
  }
  
  # Export results
  write.csv(data1, file.path(output_dir, "dataset1.csv"), row.names = FALSE)
  write.csv(data2, file.path(output_dir, "dataset2.csv"), row.names = FALSE)
  
  cat("Quick analysis completed. Results saved to:", output_dir, "\n")
  return(results)
}

# Note: Functions are exported via NAMESPACE file
