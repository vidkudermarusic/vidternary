# =============================================================================
# vidternary: File Management Module
# =============================================================================
# 
# Package:     vidternary
# Version:     1.0.0
# Author:      Vid Kuder Marušič <vidkm30@gmail.com>
# Maintainer:  Vid Kuder Marušič <vidkm30@gmail.com>
# License:     MIT + file LICENSE
# Repository:  https://github.com/vidkudermarusic/vidternary
# 
# Description: File management utilities for output directory creation, file
#              naming, timestamp logic, and cleanup operations.
# 
# Key Functions:
#   - extract_file_base(): Extract file base name from xlsx files
#   - create_ternary_output_dir(): Create output directories for ternary plots
#   - generate_ternary_filename(): Generate standardized filenames
#   - safe_create_directory(): Safe directory creation with error handling
#   - cleanup_old_outputs(): Clean up old output files
# 
# Dependencies:
#   - R (>= 4.0.0)
#   - fs
# 
# Last Modified: 2025-09-07
# 
# =============================================================================

# Extract file base name from xlsx file or display name
extract_file_base <- function(xlsx_file, xlsx_display_name = NULL) {

  
  if (!is.null(xlsx_display_name)) {
    file_base <- gsub("\\.xlsx$", "", basename(xlsx_display_name))
  } else {
    file_base <- gsub("\\.xlsx$", "", basename(xlsx_file))
  }
  

  
  return(file_base)
}

# Create output directory structure for ternary plots
create_ternary_output_dir <- function(
    xlsx_file,
    xlsx_display_name = NULL,
    output_dir = NULL,
    preview = FALSE,
    working_dir = getwd()
) {
  # Store original working directory and restore on exit
  original_wd <- getwd()
  on.exit(setwd(original_wd), add = TRUE)
  setwd(working_dir)
  
  # Extract file base name
  file_base <- extract_file_base(xlsx_file, xlsx_display_name)
  
  # Initialize plot_folder_name
  plot_folder_name <- paste0("charge", file_base)
  
  # Only create directories if NOT in preview mode and output_dir is specified
  if (!preview && !is.null(output_dir)) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    # Create plot folder with "charge" prefix (matching legacy code)
    custom_folder <- file.path(output_dir, plot_folder_name)
    
    # Add timestamp if folder already exists
    if (dir.exists(custom_folder)) {
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      custom_folder <- file.path(output_dir, paste0(plot_folder_name, "_", timestamp))
    }
    
    dir.create(custom_folder, recursive = TRUE, showWarnings = FALSE)
    
  } else if (preview) {
    # For preview mode, just set a dummy folder name
    custom_folder <- NULL
    
  } else {
    # Fallback to plots2 directory only if not preview and no output_dir
    plots_dir <- file.path(getwd(), "plots2")
    if (!dir.exists(plots_dir)) {
      dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    # Create plot folder with "charge" prefix
    custom_folder <- file.path(plots_dir, plot_folder_name)
    
    # Add timestamp if folder already exists
    if (dir.exists(custom_folder)) {
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      custom_folder <- file.path(plots_dir, paste0(plot_folder_name, "_", timestamp))
    }
    
    dir.create(custom_folder, recursive = TRUE, showWarnings = FALSE)
  }
  
  return(list(
    custom_folder = custom_folder,
    file_base = file_base,
    plot_folder_name = plot_folder_name
  ))
}

# Generate standardized filename for ternary plots
generate_ternary_filename <- function(
    file_base,
    custom_folder,
    output_format = "png",
    timestamp = NULL
) {
  if (is.null(timestamp)) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  }
  
  filename <- file.path(custom_folder, paste0(file_base, "_", timestamp, ".", output_format))
  return(filename)
}

# Create output directory for comprehensive analysis
create_analysis_output_dir <- function(
    analysis_name,
    output_dir = NULL,
    working_dir = getwd()
) {
  # Store original working directory and restore on exit
  original_wd <- getwd()
  on.exit(setwd(original_wd), add = TRUE)
  setwd(working_dir)
  
  if (is.null(output_dir)) {
    # Create default output directory
    output_dir <- file.path(getwd(), "output")
  }
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Create timestamped subfolder for this analysis
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  analysis_folder <- file.path(output_dir, paste0(analysis_name, "_", timestamp))
  dir.create(analysis_folder, recursive = TRUE, showWarnings = FALSE)
  
  return(analysis_folder)
}

# Validate and create output directory with error handling
safe_create_directory <- function(dir_path, recursive = TRUE) {
  tryCatch({
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = recursive, showWarnings = FALSE)
    }
    return(TRUE)
  }, error = function(e) {
    warning("Failed to create directory '", dir_path, "': ", e$message)
    return(FALSE)
  })
}

# Clean up old output directories (optional utility)
cleanup_old_outputs <- function(base_dir, max_age_days = 30) {
  if (!dir.exists(base_dir)) return(invisible())
  
  current_time <- Sys.time()
  cutoff_time <- current_time - (max_age_days * 24 * 60 * 60)
  
  dirs <- list.dirs(base_dir, full.names = TRUE, recursive = FALSE)
  dirs <- dirs[dirs != base_dir]  # Exclude base directory
  
  for (dir in dirs) {
    dir_info <- file.info(dir)
    if (dir_info$mtime < cutoff_time) {
      tryCatch({
        unlink(dir, recursive = TRUE, force = TRUE)
        message("Removed old output directory: ", basename(dir))
      }, error = function(e) {
        warning("Failed to remove old directory '", dir, "': ", e$message)
      })
    }
  }
}
