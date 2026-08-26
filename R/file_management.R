# ---- File Management Module ----
# This module handles output directory creation, file naming, and timestamp logic
# for ternary plots and other analysis outputs

#' Extract a base filename (no `.xlsx` extension) for plot titles/filenames
#'
#' @param xlsx_file Path to the uploaded `.xlsx` file (temp upload path).
#' @param xlsx_display_name Optional original filename to prefer over
#'   `xlsx_file`'s (temp, often non-descriptive) basename.
#' @return The base filename, with any `.xlsx` extension removed.
#' @export
extract_file_base <- function(xlsx_file, xlsx_display_name = NULL) {

  
  if (!is.null(xlsx_display_name)) {
    file_base <- gsub("\\.xlsx$", "", basename(xlsx_display_name))
  } else {
    file_base <- gsub("\\.xlsx$", "", basename(xlsx_file))
  }
  

  
  return(file_base)
}

#' Resolve/create the output folder for a ternary plot save
#'
#' In preview mode, no directory is created. Otherwise creates
#' `<output_dir>/charge<file_base>` (falling back to `<cwd>/plots2/...` if
#' `output_dir` is `NULL`), appending a timestamp if that folder already exists.
#'
#' @param xlsx_file Path to the uploaded `.xlsx` file.
#' @param xlsx_display_name Optional original filename, passed to `extract_file_base()`.
#' @param output_dir Base output directory. If `NULL` and not in preview
#'   mode, falls back to `<working_dir>/plots2`.
#' @param preview If `TRUE`, skip directory creation entirely.
#' @param working_dir Directory to temporarily `setwd()` into for relative
#'   path resolution; restored on exit. Default `getwd()`.
#' @return A list: `custom_folder` (path, or `NULL` in preview mode),
#'   `file_base`, `plot_folder_name`.
#' @export
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

#' Build a standardized, timestamped ternary plot filename
#'
#' @param file_base Base filename, e.g. from `extract_file_base()`.
#' @param custom_folder Destination folder, e.g. from
#'   `create_ternary_output_dir()$custom_folder`.
#' @param output_format File extension (without a dot). Default `"png"`.
#' @param timestamp Timestamp string to embed. Defaults to the current
#'   time, formatted `%Y%m%d_%H%M%S`.
#' @return The full file path: `<custom_folder>/<file_base>_<timestamp>.<output_format>`.
#' @export
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

#' Create a timestamped output subfolder for an analysis run
#'
#' @param analysis_name Name used as the subfolder's prefix (e.g.
#'   `"comprehensive_analysis"`).
#' @param output_dir Base output directory. Defaults to `<working_dir>/output`.
#' @param working_dir Directory to temporarily `setwd()` into for relative
#'   path resolution; restored on exit. Default `getwd()`.
#' @return The path to the created `<output_dir>/<analysis_name>_<timestamp>/` folder.
#' @export
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

#' Delete output subdirectories older than a given age
#'
#' @param base_dir Directory whose immediate subdirectories are checked.
#' @param max_age_days Subdirectories with a modification time older than
#'   this many days are deleted. Default 30.
#' @return `NULL`, invisibly. Called for its file-deletion side effect.
#' @export
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
