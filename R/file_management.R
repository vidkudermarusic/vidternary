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

