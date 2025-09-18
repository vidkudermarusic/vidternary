# =============================================================================
# vidternary: Package Dependencies Management Module
# =============================================================================
# 
# Package:     vidternary
# Version:     1.0.0
# Author:      Vid Kuder Marušič <vidkm30@gmail.com>
# Maintainer:  Vid Kuder Marušič <vidkm30@gmail.com>
# License:     MIT + file LICENSE
# Repository:  https://github.com/vidkudermarusic/vidternary
# 
# Description: Package dependencies management including checking, loading,
#              and installation of required packages for the application.
# 
# Key Functions:
#   - initialize_packages(): Initialize and load all required packages
#   - check_package_dependencies(): Check if all dependencies are available
#   - install_missing_packages(): Install missing packages automatically
# 
# Dependencies:
#   - R (>= 4.0.0)
#   - All package dependencies listed in required_packages
# 
# Last Modified: 2025-09-07
# 
# =============================================================================

# Required packages for the application
required_packages <- c(
  "openxlsx", "Ternary", "PlotTools", "shiny", "shinyFiles", "shinyjqui", "shinyBS",
  "ggplot2", "GGally", "rmarkdown", "corrplot", "knitr", "colourpicker", "DT",
  "robustbase", "isotree", "plotly", "writexl", "jsonlite", "zip", "fs", "htmlwidgets",
  "moments", "digest", "viridisLite", "devtools"
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
