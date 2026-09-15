# ---- Ternary Plot: Output Directory Resolution (split out of ternary_plot_data_prep.R) ----
# Resolves a ternary plot's output folder and file-base name, for both the
# Multiple Ternary Creator's shared-output-dir path and the original
# per-charge-subfolder path.

#' Resolve a ternary plot's output folder and file-base name
#'
#' Two distinct code paths, both producing `custom_folder`, `file_base`,
#' and `plot_folder_name`: when `output_dir` is supplied for a real
#' (non-preview) save, uses `output_dir` directly (the Multiple Ternary
#' Creator's "one shared folder for every plot" behavior), routing the
#' display name through [extract_file_base()] (`file_management.R`) so a
#' crafted upload filename containing path separators can't steer the save
#' outside `output_dir`; otherwise delegates to
#' [create_ternary_output_dir()] (`file_management.R`), which creates a
#' per-charge subfolder (with a timestamp suffix if one already exists) or
#' returns `custom_folder = NULL` in preview mode. Of its three outputs,
#' only `file_base` is read again by this function's caller (passed to
#' [build_ternary_plot_title()] for the title's "charge" line);
#' `custom_folder`/`plot_folder_name` are computed but not read by any
#' downstream consumer today.
#'
#' @param xlsx_file Path to the uploaded `.xlsx` file (temp upload path).
#' @param xlsx_display_name Optional original filename, preferred over
#'   `xlsx_file`'s temp-upload basename.
#' @param output_dir Base output directory for a real (non-preview) save,
#'   or `NULL` to use the original per-charge-subfolder logic.
#' @param preview If `TRUE`, this call is only feeding a live preview
#'   render, not a save - no directory is created.
#' @param working_dir Directory to resolve relative paths against, passed
#'   through to [create_ternary_output_dir()].
#' @return This function's entire local environment as a list
#'   (`as.list(environment())`) - `custom_folder`, `file_base`, and
#'   `plot_folder_name` are the fields that existed in
#'   [prepare_ternary_plot_data()]'s own environment before this
#'   extraction; `dir_info` is this function's own internal working
#'   variable in the `create_ternary_output_dir()` branch, echoed back
#'   unchanged.
#' @export
resolve_ternary_output_directory <- function(xlsx_file, xlsx_display_name, output_dir, preview, working_dir) {
  # Create output directory structure using file management module
  # For multiple ternary plots, use the output_dir directly instead of creating subfolders

  if (!is.null(output_dir) && !preview) {
    # Use the output_dir directly for multiple ternary plots. Routed
    # through extract_file_base() (file_management.R), the same safe
    # basename()-then-strip-extension helper the preview branch below
    # already uses via create_ternary_output_dir() - a raw
    # tools::file_path_sans_ext(xlsx_display_name) here (the client-
    # supplied upload filename) would let a crafted name containing path
    # separators steer this save outside output_dir.
    custom_folder <- output_dir
    file_base <- extract_file_base(xlsx_file, xlsx_display_name)
    plot_folder_name <- paste0("charge", file_base)
  } else {
    # Use the original directory creation logic for single plots
  dir_info <- create_ternary_output_dir(
    xlsx_file = xlsx_file,
    xlsx_display_name = xlsx_display_name,
    output_dir = output_dir,
    preview = preview,
    working_dir = working_dir
  )

  custom_folder <- dir_info$custom_folder
  file_base <- dir_info$file_base
  plot_folder_name <- dir_info$plot_folder_name
  }

  as.list(environment())
}
