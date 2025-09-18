# =============================================================================
# vidternary: Application Options and Constants Module
# =============================================================================
# 
# Package:     vidternary
# Version:     1.0.0
# Author:      Vid Kuder Marušič <vidkm30@gmail.com>
# Maintainer:  Vid Kuder Marušič <vidkm30@gmail.com>
# License:     MIT + file LICENSE
# Repository:  https://github.com/vidkudermarusic/vidternary
# 
# Description: Application-wide options and constants used throughout the
#              application. Centralizing these makes it easier to maintain
#              and modify the app behavior.
# 
# Key Functions:
#   - [Application constants and configuration values]
# 
# Dependencies:
#   - R (>= 4.0.0)
# 
# Last Modified: 2025-09-07
# 
# =============================================================================

# Point size settings
MIN_POINT_SIZE <- 0.1
MAX_POINT_SIZE <- 2.5

# Cache settings
CACHE_TIMEOUT <- 300  # 5 minutes in seconds

# Default directories
DEFAULT_WORKING_DIR <- getwd()
DEFAULT_OUTPUT_DIR <- file.path(getwd(), "output")

# Required packages
REQUIRED_PACKAGES <- c(
  "openxlsx", "Ternary", "PlotTools", "shiny", "shinyFiles", "shinyjqui", "shinyBS",
  "ggplot2", "GGally", "rmarkdown", "corrplot", "knitr", "colourpicker", "DT",
  "robustbase", "isotree", "plotly", "writexl", "jsonlite", "zip", "fs", "htmlwidgets",
  "moments", "digest", "viridisLite", "devtools"
)

# Plot styling defaults
DEFAULT_COLOR_PALETTE <- "viridis"
DEFAULT_POINT_SIZE <- 1.0
DEFAULT_POINT_TYPE <- 16
DEFAULT_ALPHA <- 0.7

# Filter defaults
DEFAULT_IQR_MULTIPLIER <- 1.5
DEFAULT_ZSCORE_THRESHOLD <- 3.0
DEFAULT_MAD_MULTIPLIER <- 3.0

# Analysis defaults
DEFAULT_MAHALANOBIS_LAMBDA <- 1
DEFAULT_MAHALANOBIS_OMEGA <- 0
DEFAULT_ISOLATION_FOREST_CONTAMINATION <- 0.1

# Export settings
DEFAULT_IMAGE_DPI <- 300
DEFAULT_IMAGE_WIDTH <- 10
DEFAULT_IMAGE_HEIGHT <- 8
