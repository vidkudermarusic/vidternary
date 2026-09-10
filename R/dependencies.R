# ---- Package Dependencies Management ----
# This module handles all package dependencies, checking, and loading

# Required packages for the application. Kept in sync with DESCRIPTION's
# Imports: - digest/htmlwidgets/robustbase/moments were removed from both
# (confirmed zero live callers anywhere in R/); devtools moved to
# DESCRIPTION's Suggests: (a dev-time tool - installing/running the app
# itself never calls it) and correspondingly isn't loaded here either;
# RANN (already correctly in DESCRIPTION's Imports: - Spatial Clustering's
# exact-nearest-neighbor method) was missing from this list, so a user
# following this script's own guidance specifically wouldn't have gotten
# it installed. shinyFiles/fs were removed once the global Working/Output
# Directory picker (server_directory_management.R) was replaced by
# per-save browser downloads (see the vidternary Structural Audit's Sec.03) -
# neither is used anywhere else in R/. zip was previously removed here as
# dead code, then reinstated for a genuine new use: the batch save handler
# (server_ternary_plots_batch.R's "Create & Save all ternary plots") zips
# its outputs into one download instead of writing to a chosen folder.
# (The Ternary Plots tab's own "Save Both Plots" button used to be a
# second reason zip was needed - removed since separate "Save Plot 1"/
# "Save Plot 2" downloads already cover the same need.) spatstat.geom/
# spatstat.explore were added for the "Point Pattern Analysis" tab
# (spatial_point_pattern_analysis.R - Ripley's K/L, the G-function, a CSR
# envelope test, kernel intensity) - only those two sub-packages of the
# spatstat family are imported, not the full `spatstat` umbrella, which
# also pulls in spatstat.model/spatstat.linnet/etc that nothing here uses.
# GGally, colourpicker, plotly, shinyBS and shinyjqui were removed once
# `R CMD check` (and a fresh grep of R/) confirmed zero call sites anywhere
# - a leftover from an earlier UI design. knitr/rmarkdown are vignette-
# build tooling only (DESCRIPTION's Suggests + VignetteBuilder), never
# loaded at app runtime, so they are not listed here either.
required_packages <- c(
  "openxlsx", "Ternary", "PlotTools", "shiny",
  "ggplot2", "corrplot", "DT",
  "isotree", "RColorBrewer", "writexl", "jsonlite", "zip",
  "viridisLite", "magick", "png", "rlang", "RANN",
  "spatstat.geom", "spatstat.explore"
)

# Essential packages that must be loaded for core functionality
essential_packages <- c("shiny", "openxlsx", "ggplot2")

# Function to check required packages (no runtime installation)
# Production-ready: No automatic package installation during runtime
# Users must install dependencies before running the app
check_required_packages <- function(packages) {
  missing_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  
  if (length(missing_packages) > 0) {
    cat("WARNING: The following required packages are missing:\n")
    cat(paste("  -", missing_packages), sep = "\n")
    cat("\nPlease install them manually before running the app:\n")
    cat("install.packages(c(", 
        paste(paste0('"', missing_packages, '"'), collapse = ", "), "))\n")
    cat("\nOr use renv for dependency management:\n")
    cat("renv::init()\n")
    cat("renv::install(c(", 
            paste(paste0('"', missing_packages, '"'), collapse = ", "), "))\n")
        
    # Stop execution if critical packages are missing
    critical_packages <- c("shiny", "openxlsx")
    if (any(critical_packages %in% missing_packages)) {
      stop("Critical packages missing. Please install required packages first.")
    }
  }
  
  # Return available packages
  available_packages <- packages[packages %in% installed.packages()[,"Package"]]
  return(available_packages)
}

# Load available packages with error handling
load_package_safely <- function(package_name) {
  tryCatch({
    library(package_name, character.only = TRUE)
    return(TRUE)
  }, error = function(e) {
    cat("Warning: Could not load package '", package_name, "': ", e$message, "\n")
    return(FALSE)
  })
}

# Initialize package loading
initialize_packages <- function() {
  # Check required packages (no installation)
  available_packages <- check_required_packages(required_packages)
  
  # Load essential packages first
  for (pkg in essential_packages) {
    if (pkg %in% available_packages) {
      load_package_safely(pkg)
    }
  }
  
  # Load optional packages
  optional_packages <- setdiff(available_packages, essential_packages)
  for (pkg in optional_packages) {
    load_package_safely(pkg)
  }
  
  # Check if critical packages are loaded
  if (!"shiny" %in% loadedNamespaces()) {
    stop("Critical package 'shiny' could not be loaded. Please check your R installation.")
  }
  
  cat("Package loading completed. Available packages:", paste(available_packages, collapse = ", "), "\n")
  
  # Set Shiny options
  options(shiny.maxRequestSize = 100 * 1024^2)  # 100 MB
  
  return(available_packages)
}

# Note: Functions are exported via NAMESPACE file
