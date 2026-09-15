# ---- Ternary Plot: Shared Data Preparation (split out of ternary_plot.R) ----
# Everything general_ternary_plot() needs to do exactly once, regardless of
# preview vs. save: load the Excel file, apply individual/optional-parameter/
# statistical/multivariate filters, compute ternary coordinates, build the
# plot title, and work out point size/color/shape. Nothing here touches a
# graphics device - that happens in ternary_plot_preview.R and
# ternary_plot_save.R, both of which take the list this function returns.
#
# The caller (general_ternary_plot() in ternary_plot.R) is responsible for
# setwd(working_dir) + on.exit(restore) *before* calling this, since that
# needs to stay in effect across prep AND the preview/save render calls that
# follow - not just for the duration of this function.
#
# Returns the function's entire local environment as a list (as.list(environment())).
# This is deliberate: dozens of locals computed here (clean_labels_A/B/C,
# ternary_points1, pointSize/pointCol/pointType, plot_title, title_parts,
# param1_values, param1_bins, unique_groups, group_colors, group_counts,
# col1_text/col2_text/col3_text, mahal_result, iso_result, and more) are all
# read by the preview and/or save renderers, and hand-picking a parameter
# list for each risked silently dropping one. Renderers access fields via
# `pd$name` or `with(pd, {...})`.
#
# This function's own seven data-prep stages now each live in their own file
# (see each stage's own header): ternary_plot_data_load.R (load + validate),
# ternary_plot_filters.R (element/optional-parameter, statistical, and
# multivariate filtering), ternary_plot_output_dir.R (output directory
# resolution), ternary_plot_coordinates.R (coordinate computation),
# ternary_plot_title.R (title/axis-label building), and ternary_plot_styling.R
# (point size/color/shape). ternary_plot_notes.R holds draw_plot_notes_column(),
# a small shared drawing helper used by ternary_plot_preview.R/
# ternary_plot_save.R but not by this file. This file itself keeps only the
# orchestrator, prepare_ternary_plot_data(), which calls each stage in order
# and list2env()-merges its result back into its own local environment -
# exactly as it always has.

#' Load, filter, and prepare data for a single ternary plot
#'
#' Does everything [general_ternary_plot()] needs exactly once, regardless
#' of preview vs. save: loads the Excel file, applies individual-element,
#' optional-parameter, statistical, and multivariate filters, computes
#' ternary coordinates, builds the plot title, and works out point
#' size/color/shape. Touches no graphics device - see
#' `ternary_plot_preview.R`/`ternary_plot_save.R`'s renderers for that.
#' The caller is responsible
#' for `setwd(working_dir)` (with `on.exit()` restore) before calling this,
#' since that needs to stay in effect across this call and the render call
#' that follows it.
#'
#' @param xlsx_file Path to the uploaded `.xlsx` file (temp upload path).
#' @param working_dir Directory to resolve relative paths against; the
#'   caller must already have `setwd()`'d into it.
#' @param output_dir Base output directory for a real (non-preview) save.
#' @param element_A,element_B,element_C Ternary-axis element specs, each a
#'   `list(col = <one or more column names>)`.
#' @param optional_param1 Optional `list(col = <column name(s)>, filter =
#'   <filter string or NULL>)` for point size/type representation.
#' @param optional_param2 Optional `list(col = <column name(s)>, filter =
#'   <filter string or NULL>)` for point color / categorical grouping.
#' @param color_palette Palette name (`"blue"`, `"red"`, `"viridis"`, or
#'   `"rainbow"`) used when `optional_param2` drives point color.
#' @param xlsx_display_name Optional original filename, preferred over
#'   `xlsx_file`'s temp-upload basename for titles/output filenames.
#' @param preview If `TRUE`, skip directory/file creation - this call is
#'   only feeding a live preview render, not a save.
#' @param use_mahalanobis Apply Mahalanobis-distance outlier filtering.
#' @param reference_data Optional reference dataset for multivariate
#'   filtering, when `mahalanobis_reference`/an isolation-forest reference
#'   mode needs one other than the file being processed.
#' @param optional_param1_representation `"point_size"` or `"point_type"` -
#'   how `optional_param1` maps onto the plotted points.
#' @param output_format File format for a real save (e.g. `"png"`).
#' @param use_isolation_forest Apply isolation-forest outlier filtering.
#' @param isolation_ntrees,isolation_contamination Number of trees and
#'   contamination fraction for [compute_isolation_forest()], when
#'   `use_isolation_forest = TRUE`. User-adjustable in the UI; default 200
#'   trees, 0.10 contamination.
#' @param isolation_sample_size Rows each isolation tree trains on. `NULL`
#'   (default) uses every complete reference row; a whole number `>= 2`
#'   sub-samples that many per tree (see [compute_isolation_forest()]).
#' @param use_iqr_filter,use_zscore_filter,use_mad_filter Apply IQR /
#'   Z-score / MAD statistical outlier filtering. Only one
#'   statistical/multivariate filter is meant to be active per plot -
#'   enforced upstream in [general_ternary_plot()].
#' @param stat_filter_log10 If `TRUE`, the active IQR/Z-score/MAD fence is
#'   fitted on `log10(value)` rather than the raw value (passed straight
#'   through to [apply_statistical_filtering()]). Default `FALSE`.
#' @param lambda,omega Sensitivity/leniency parameters for the automatic
#'   Mahalanobis threshold formula (see [compute_mahalanobis_distance()]).
#' @param keep_outliers_mahalanobis,keep_outliers_isolation,keep_outliers_iqr,keep_outliers_zscore,keep_outliers_mad
#'   If `TRUE` for the active filter method, keep only the flagged
#'   outliers instead of removing them.
#' @param individual_filters_A,individual_filters_B,individual_filters_C
#'   Named lists (by column) of per-element filter strings, as built by
#'   `collect_main_ternary_filters()`.
#' @param custom_mdthresh Manual Mahalanobis distance threshold, used when
#'   `mdthresh_mode == "manual"`.
#' @param mdthresh_mode `"auto"` or `"manual"` Mahalanobis threshold mode.
#' @param mahalanobis_reference Which dataset the Mahalanobis/isolation-
#'   forest reference distribution is fit to (`"self"`, `"dataset1"`, or
#'   `"dataset2"`).
#' @param selected_columns Character vector of numeric columns used for
#'   multivariate/statistical filtering (independent of the ternary axes).
#' @param include_plot_notes If `TRUE`, include the filter/method summary
#'   notes text alongside the plot.
#' @param use_manual_point_size If `TRUE`, use `manual_point_size` for
#'   every point instead of `optional_param1_representation`'s mapping.
#' @param manual_point_size Fixed point size used when
#'   `use_manual_point_size` is `TRUE`.
#' @param selected_groups Character vector of categorical group values
#'   (from `optional_param2`) to include, when `is_categorical_group` is
#'   `TRUE`; other rows are excluded.
#' @param is_categorical_group Whether `optional_param2`'s column has been
#'   detected as categorical (drives group-based filtering/coloring
#'   instead of a continuous color scale).
#' @return This function's entire local environment as a list
#'   (`as.list(environment())`), deliberately untyped: dozens of locals
#'   computed here (`clean_labels_A`/`B`/`C`, `ternary_points1`,
#'   `pointSize`/`pointCol`/`pointType`, `plot_title`, `title_parts`,
#'   `param1_values`, `param1_bins`, `unique_groups`, `group_colors`,
#'   `group_counts`, `col1_text`/`col2_text`/`col3_text`, `mahal_result`,
#'   `iso_result`, and more) are all read by the preview and/or save
#'   renderers via `pd$name` or `with(pd, {...})`, and hand-picking a
#'   narrower return list risked silently dropping one.
#' @export
#'
#' @section Restructuring:
#' This function's logic is split into seven top-level, independently
#' testable/documented functions, called below in this order and merged
#' back into this function's own local environment via `list2env()`:
#' [load_and_validate_ternary_source_data()], [apply_element_and_parameter_filters()]
#' (moves three of this function's five local closures into its own scope,
#' leaving `preview_title_layout()`/`calculate_plot_dimensions()` as the
#' two that still live here), [apply_statistical_filtering()],
#' [apply_multivariate_filtering()], [resolve_ternary_output_directory()],
#' [compute_ternary_coordinates()], [build_ternary_plot_title()], and
#' [compute_point_styling()].
prepare_ternary_plot_data <- function(
    xlsx_file,
    working_dir,
    output_dir,
    element_A,
    element_B,
    element_C,
    optional_param1,
    optional_param2,
    color_palette,
    xlsx_display_name,
    preview,
    use_mahalanobis,
    reference_data,
    optional_param1_representation,
    output_format,
    use_isolation_forest,
    isolation_ntrees = 200,
    isolation_contamination = 0.10,
    isolation_sample_size = NULL,
    use_iqr_filter,
    use_zscore_filter,
    use_mad_filter,
    stat_filter_log10 = FALSE,
    lambda,
    omega,
    keep_outliers_mahalanobis,
    keep_outliers_isolation,
    keep_outliers_iqr,
    keep_outliers_zscore,
    keep_outliers_mad,
    individual_filters_A,
    individual_filters_B,
    individual_filters_C,
    custom_mdthresh,
    mdthresh_mode,
    mahalanobis_reference,
    selected_columns,
    include_plot_notes,
    use_manual_point_size,
    manual_point_size,
    selected_groups,
    is_categorical_group
) {

  # Variables that are only conditionally assigned below (depending on which
  # filter method or point-styling branch runs). Pre-declaring them as NULL
  # lets the render functions check `!is.null(pd$x)` instead of relying on
  # R's exists() the way the original single-function version did (exists()
  # only works within the same function's scope, which no longer holds once
  # this logic is split across three functions).
  mahal_result <- NULL
  iso_result <- NULL
  param1_bins <- NULL
  param1_values <- NULL
  unique_groups <- NULL
  group_colors <- NULL
  group_counts <- NULL

  # Load + validate: extracted into load_and_validate_ternary_source_data()
  # (see this function's own "Restructuring" doc section above) - identical
  # behavior, its only output is M.
  M <- load_and_validate_ternary_source_data(xlsx_file, element_A, element_B, element_C, output_format, preview)

  # ---- CRITICAL HELPER FUNCTIONS (title/dimension formatting) ----
  # preview_title_layout()/calculate_plot_dimensions() are local/nested on
  # purpose - they are NOT the same functions as similarly-named ones
  # elsewhere in the package (different, ternary-plot-specific behavior),
  # so they must stay local rather than becoming top-level functions, to
  # avoid silently shadowing unrelated global utilities package-wide.

  # Function to preview title layout for debugging
  preview_title_layout <- function(title_parts) {
    final_title <- paste(title_parts, collapse = "\n")
    if (getOption("ternary.debug", FALSE)) {
      cat("DEBUG: Title preview:\n")
      cat("Original parts:", length(title_parts), "\n")
      cat("Final title:\n", final_title, "\n")
      cat("Line count:", length(strsplit(final_title, "\n")[[1]]), "\n")
    }
    return(final_title)
  }

  # Function to calculate optimal plot dimensions based on title length.
  # Used by ternary_plot_save.R (via pd$calculate_plot_dimensions()) since
  # that's the only place that needs pixel dimensions for a file device.
  calculate_plot_dimensions <- function(title_parts) {
    final_title <- paste(title_parts, collapse = "\n")
    line_count <- length(strsplit(final_title, "\n")[[1]])

    # Base dimensions (matching legacy file)
    base_width <- 1200
    base_height <- 1400

    # Adjust height based on title lines
    if (line_count == 1) {
      # Single line: standard height
      height <- base_height
    } else if (line_count == 2) {
      # Two lines: increase height slightly
      height <- base_height + 100
    } else {
      # Three or more lines: increase height more
      height <- base_height + 200
    }

    return(list(width = base_width, height = height))
  }

  # Per-element and optional-parameter filtering: extracted into
  # apply_element_and_parameter_filters() (see this function's own
  # "Restructuring" doc section above) - identical behavior; the only
  # fields read again by this function itself are M (filtered) and
  # all_selected_elements (used by the coordinate-computation code below).
  filter_result <- apply_element_and_parameter_filters(
    M = M,
    element_A = element_A, element_B = element_B, element_C = element_C,
    individual_filters_A = individual_filters_A,
    individual_filters_B = individual_filters_B,
    individual_filters_C = individual_filters_C,
    optional_param1 = optional_param1, optional_param2 = optional_param2,
    preview = preview
  )
  list2env(filter_result, environment())

  # Statistical-outlier dispatch: extracted into apply_statistical_filtering()
  # (see this function's own "Restructuring" doc section above) - identical
  # behavior; the only field read back afterward is M (possibly re-filtered).
  stat_result <- apply_statistical_filtering(
    M = M,
    use_iqr_filter = use_iqr_filter,
    use_zscore_filter = use_zscore_filter,
    use_mad_filter = use_mad_filter,
    selected_columns = selected_columns,
    keep_outliers_iqr = keep_outliers_iqr,
    keep_outliers_zscore = keep_outliers_zscore,
    keep_outliers_mad = keep_outliers_mad,
    stat_filter_log10 = stat_filter_log10
  )
  list2env(stat_result, environment())

  # Multivariate outlier dispatch: extracted into apply_multivariate_filtering()
  # (see this function's own "Restructuring" doc section above) - identical
  # behavior; the only fields read back afterward are M (possibly
  # re-filtered), mahal_result, and iso_result.
  mv_result <- apply_multivariate_filtering(
    M = M,
    use_mahalanobis = use_mahalanobis,
    use_isolation_forest = use_isolation_forest,
    isolation_ntrees = isolation_ntrees,
    isolation_contamination = isolation_contamination,
    isolation_sample_size = isolation_sample_size,
    selected_columns = selected_columns,
    mahalanobis_reference = mahalanobis_reference,
    reference_data = reference_data,
    preview = preview,
    keep_outliers_isolation = keep_outliers_isolation,
    keep_outliers_mahalanobis = keep_outliers_mahalanobis,
    lambda = lambda,
    omega = omega,
    custom_mdthresh = custom_mdthresh,
    mdthresh_mode = mdthresh_mode
  )
  list2env(mv_result, environment())

  if (getOption("ternary.debug", FALSE)) {
    cat("DEBUG: After multivariate filtering, data dimensions:", dim(M), "\n")
    if (use_mahalanobis && !is.null(reference_data)) {
      cat("DEBUG: Multivariate analysis was applied. Original data should be filtered.\n")
      cat("DEBUG: Sample of filtered data (first 5 rows):\n")
      print(head(M, 5))
    }
  }

  # Output-directory resolution: extracted into
  # resolve_ternary_output_directory() (see this function's own
  # "Restructuring" doc section above) - identical behavior; the only
  # field read again by this function itself is file_base (passed to
  # build_ternary_plot_title() below).
  outdir_result <- resolve_ternary_output_directory(
    xlsx_file = xlsx_file,
    xlsx_display_name = xlsx_display_name,
    output_dir = output_dir,
    preview = preview,
    working_dir = working_dir
  )
  list2env(outdir_result, environment())

  # Coordinate computation + validation: extracted into
  # compute_ternary_coordinates() (see this function's own "Restructuring"
  # doc section above) - identical behavior; matrika and ternary_points1
  # are the fields read again by this function itself (and, via pd, by the
  # preview/save renderers).
  coord_result <- compute_ternary_coordinates(
    M = M,
    all_selected_elements = all_selected_elements,
    element_A = element_A, element_B = element_B, element_C = element_C,
    optional_param1 = optional_param1, optional_param2 = optional_param2,
    use_mahalanobis = use_mahalanobis, reference_data = reference_data
  )
  list2env(coord_result, environment())

  # list2env() merges every local build_ternary_plot_title() computed
  # (clean_labels_A/B/C, axis_labels_A/B/C, title_parts, plot_title, and
  # the transient opt1_label/opt2_label/mv_methods/stat_methods/indicator/
  # fallback_name locals along the way) back into this function's own
  # environment.
  list2env(
    build_ternary_plot_title(
      element_A = element_A, element_B = element_B, element_C = element_C,
      optional_param1 = optional_param1, optional_param1_representation = optional_param1_representation,
      optional_param2 = optional_param2,
      use_mahalanobis = use_mahalanobis, keep_outliers_mahalanobis = keep_outliers_mahalanobis,
      use_isolation_forest = use_isolation_forest, keep_outliers_isolation = keep_outliers_isolation,
      use_iqr_filter = use_iqr_filter, keep_outliers_iqr = keep_outliers_iqr,
      use_zscore_filter = use_zscore_filter, keep_outliers_zscore = keep_outliers_zscore,
      use_mad_filter = use_mad_filter, keep_outliers_mad = keep_outliers_mad,
      file_base = file_base, xlsx_display_name = xlsx_display_name, xlsx_file = xlsx_file,
      title_layout_fn = preview_title_layout
    ),
    environment()
  )

  # list2env() merges pointSize/pointType/pointCol, the possibly-filtered
  # ternary_points1, the possibly-reassigned selected_groups, and every
  # legend-metadata field (param1_values/param1_bins,
  # unique_groups/group_colors/group_counts, MIN_POINT_SIZE/MAX_POINT_SIZE)
  # back into this function's own environment.
  ps_result <- compute_point_styling(
    ternary_points1 = ternary_points1,
    matrika = matrika,
    optional_param1 = optional_param1,
    optional_param1_representation = optional_param1_representation,
    optional_param2 = optional_param2,
    color_palette = color_palette,
    use_manual_point_size = use_manual_point_size,
    manual_point_size = manual_point_size,
    is_categorical_group = is_categorical_group,
    selected_groups = selected_groups
  )
  list2env(ps_result, environment())

  # ---- Plot notes text (computed once here; drawn via mtext() by both
  # ternary_plot_preview.R and ternary_plot_save.R, which only differ in
  # which graphics device is active when they call mtext()) ----
  if (include_plot_notes) {
    # Generate comprehensive plot summary organized into 3 columns
    # Column 1: Elements and their filters
    elements_summary <- c()
    elements_summary <- c(elements_summary, paste("Data points:", nrow(ternary_points1)))
    elements_summary <- c(elements_summary, paste("Elements A:", paste(element_A$col, collapse = "+")))
    elements_summary <- c(elements_summary, paste("Elements B:", paste(element_B$col, collapse = "+")))
    elements_summary <- c(elements_summary, paste("Elements C:", paste(element_C$col, collapse = "+")))



    if (!is.null(individual_filters_A) && length(individual_filters_A) > 0) {
      filter_text <- paste("Element A filters:", paste(sapply(names(individual_filters_A), function(x) paste0(x, ":", individual_filters_A[[x]])), collapse = ", "))
      elements_summary <- c(elements_summary, filter_text)
    }

    if (!is.null(individual_filters_B) && length(individual_filters_B) > 0) {
      filter_text <- paste("Element B filters:", paste(sapply(names(individual_filters_B), function(x) paste0(x, ":", individual_filters_B[[x]])), collapse = ", "))
      elements_summary <- c(elements_summary, filter_text)
    }

    if (!is.null(individual_filters_C) && length(individual_filters_C) > 0) {
      filter_text <- paste("Element C filters:", paste(sapply(names(individual_filters_C), function(x) paste0(x, ":", individual_filters_C[[x]])), collapse = ", "))
      elements_summary <- c(elements_summary, filter_text)
    }

    # Column 2: Optional parameters and their filters
    optional_summary <- c()
    optional_summary <- c(optional_summary, "Optional Parameters:")

    if (!is.null(optional_param1)) {
      optional_summary <- c(optional_summary, paste("Parameter 1:", paste(optional_param1$col, collapse = "+")))

      if (!is.null(optional_param1$filter) && nzchar(optional_param1$filter)) {
        optional_summary <- c(optional_summary, paste("  Filter:", optional_param1$filter))

      } else {
        if (getOption("ternary.debug", FALSE)) {
          cat("DEBUG: Optional param1 filter is NULL or empty\n")
        }
      }
    }

    if (!is.null(optional_param2)) {
      optional_summary <- c(optional_summary, paste("Parameter 2:", paste(optional_param2$col, collapse = "+")))

      if (!is.null(optional_param2$filter) && nzchar(optional_param2$filter)) {
        optional_summary <- c(optional_summary, paste("  Filter:", optional_param2$filter))

      } else {
        if (getOption("ternary.debug", FALSE)) {
          cat("DEBUG: Optional param2 filter is NULL or empty\n")
        }
      }
      optional_summary <- c(optional_summary, paste("  Color palette:", color_palette))
    }

    # Column 3: Statistical filtering and outlier detection
    analysis_summary <- c()
    analysis_summary <- c(analysis_summary, "Analysis Methods:")

    # Outlier detection: Mahalanobis distance (multivariate statistical
    # method) and/or Isolation Forest (machine-learning algorithm)
    if (use_mahalanobis || use_isolation_forest) {
      mv_info <- c()
      if (use_mahalanobis) {
        outlier_status <- if (keep_outliers_mahalanobis) "(keep only outliers)" else "(remove outliers)"
        mv_info <- c(mv_info, paste("Mahalanobis (lambda=", lambda, ", omega=", omega, ")", outlier_status))

        # Add detailed Mahalanobis distance information if available
        if (!is.null(mahal_result)) {
          mv_info <- c(mv_info, paste("  MDmean:", round(mahal_result$MDmean, 3)))
          mv_info <- c(mv_info, paste("  MDthresh:", round(mahal_result$MDthresh, 3)))
          mv_info <- c(mv_info, paste("  stdMD:", round(mahal_result$stdMD, 3)))
          if (!is.null(mahal_result$threshold_method)) {
            mv_info <- c(mv_info, paste("  Method:", mahal_result$threshold_method))
          }
        }
      }
      if (use_isolation_forest) {
        outlier_status <- if (keep_outliers_isolation) "(keep only outliers)" else "(remove outliers)"
        mv_info <- c(mv_info, paste("Isolation Forest", outlier_status))

        # Model parameters shown explicitly on the plot itself - ntrees/
        # contamination/sample_size are all user-adjustable (previously
        # fixed internal defaults with no way to see what was actually
        # used), matching how Mahalanobis's own lambda/omega/threshold are
        # already shown just above. sample_size is read back from
        # iso_result rather than assumed, since the requested value is
        # clamped to the reference's own complete-row count (see
        # compute_isolation_forest()'s own comment).
        if (!is.null(iso_result)) {
          mv_info <- c(mv_info, paste("  Trees:", iso_result$ntrees))
          mv_info <- c(mv_info, paste("  Contamination:", iso_result$contamination))
          ss_label <- if (is.null(isolation_sample_size)) {
            paste0(iso_result$sample_size, " (all reference rows)")
          } else {
            paste0(iso_result$sample_size, " (sub-sampled per tree)")
          }
          mv_info <- c(mv_info, paste("  Sample size:", ss_label))
        }
      }
      # mv_info's entries are appended as their OWN separate lines (not
      # joined with ", " into one string) - Mahalanobis/Isolation Forest
      # each contribute several detail lines here (MDmean/MDthresh/stdMD/
      # Method, or Trees/Contamination/Sample size), and a single
      # comma-joined line easily runs to 100+ characters. mtext() draws an
      # embedded "\n" as a real line break, but only WITHIN one string - a
      # single element of analysis_summary with no "\n" of its own never
      # wraps at all, so a long line would be drawn as-is and (at adj=1,
      # right-aligned) extend far enough left to overlap the center/left
      # plot-notes columns.
      analysis_summary <- c(analysis_summary, "Outlier Detection:", mv_info)
    }

    # Statistical filtering
    if (use_iqr_filter || use_zscore_filter || use_mad_filter) {
      stat_info <- c()
      if (use_iqr_filter) {
        outlier_status <- if (keep_outliers_iqr) "(keep only outliers)" else "(remove outliers)"
        stat_info <- c(stat_info, paste("IQR", outlier_status))
      }
      if (use_zscore_filter) {
        outlier_status <- if (keep_outliers_zscore) "(keep only outliers)" else "(remove outliers)"
        stat_info <- c(stat_info, paste("Z-Score", outlier_status))
      }
      if (use_mad_filter) {
        outlier_status <- if (keep_outliers_mad) "(keep only outliers)" else "(remove outliers)"
        stat_info <- c(stat_info, paste("MAD", outlier_status))
      }
      # Same fix as "Outlier Detection:" just above - each active filter's
      # own line stays a separate element instead of being comma-joined
      # into one long line, for consistency (and in case a future
      # filter's label grows).
      analysis_summary <- c(analysis_summary, "Statistical:", stat_info)
    }

    # Create three-column layout for plot notes with intelligent positioning
    # Calculate positions for better visibility based on content length
    col1_text <- paste(elements_summary, collapse = "\n")
    col2_text <- paste(optional_summary, collapse = "\n")
    col3_text <- paste(analysis_summary, collapse = "\n")

    # Calculate optimal positioning based on text length and content
    col1_lines <- length(strsplit(col1_text, "\n")[[1]])
    col2_lines <- length(strsplit(col2_text, "\n")[[1]])
    col3_lines <- length(strsplit(col3_text, "\n")[[1]])

    # Determine optimal line positioning based on content length
    max_notes_lines <- max(col1_lines, col2_lines, col3_lines)
    if (max_notes_lines <= 6) {
      # Short content: use standard positioning
      line_pos <- 2
      text_cex <- 0.6
    } else if (max_notes_lines <= 12) {
      # Medium content: adjust positioning
      line_pos <- 3
      text_cex <- 0.55
    } else {
      # Long content: use extended positioning
      line_pos <- 4
      text_cex <- 0.5
    }

    # Bottom outer margin (in mtext() "line" units) needed to fit the
    # TALLEST of the 3 columns without its lines running past the margin
    # boundary - previously a fixed oma[1]=4 regardless of how many lines
    # were actually drawn (see draw_plot_notes_column()'s own comment for
    # why a per-line step of text_cex*1.1 was chosen). +1 is a small
    # safety buffer so the last line isn't flush against the device edge.
    notes_bottom_margin <- line_pos + (max_notes_lines - 1) * text_cex * 1.1 + 1
  } else {
    # If plot notes are not included, create empty variables to avoid errors
    col1_text <- ""
    col2_text <- ""
    col3_text <- ""
    line_pos <- 2
    text_cex <- 0.6
    notes_bottom_margin <- 4  # unused when plot notes are off, kept only so pd$notes_bottom_margin is always defined
  }

  return(as.list(environment()))
}
