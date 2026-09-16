# ---- Ternary Plot: Point Styling (split out of ternary_plot_data_prep.R) ----
# Turns Optional Parameter 1 (point size or point type) and Optional
# Parameter 2 (point color / categorical grouping) into per-point
# pointSize/pointType/pointCol vectors, plus legend metadata.

#' Work out each point's plotted size, color, and shape
#'
#' Turns Optional Parameter 1 (point size or point type) and Optional
#' Parameter 2 (point color / categorical grouping) into per-point
#' `pointSize`/`pointType`/`pointCol` vectors aligned with `ternary_points1`,
#' plus whatever legend metadata the preview/save renderers need to draw a
#' matching legend (`param1_values`/`param1_bins` for the size/type legend,
#' `unique_groups`/`group_colors`/`group_counts` for a categorical color
#' legend). When Optional Parameter 2 is a categorical grouping, this is
#' also where rows outside the selected groups get dropped from
#' `ternary_points1` itself (with the point size/type vectors kept in sync) -
#' the reason this function takes and can return a modified
#' `ternary_points1` and `selected_groups`, unlike a pure "compute some
#' columns" helper. Everything here is genuinely self-contained once
#' `ternary_points1`/`matrika` exist, with no interaction with the
#' filtering or multivariate-analysis steps that ran earlier.
#'
#' @param ternary_points1 Data frame of computed ternary coordinates (`A`,
#'   `B`, `C` columns), one row per plotted point so far.
#' @param matrika The prepared data frame `optional_param1`/`optional_param2`
#'   values are read from (element columns numeric, optional-parameter
#'   columns preserved as-is for categorical grouping).
#' @param optional_param1 Optional `list(col = <column name(s)>, filter =
#'   ...)` driving point size/type; `NULL` for the plain default styling.
#' @param optional_param1_representation `"point_size"` or `"point_type"` -
#'   how `optional_param1` maps onto the plotted points.
#' @param optional_param2 Optional `list(col = <column name>, filter = ...)`
#'   driving point color / categorical grouping; `NULL` for plain black
#'   points.
#' @param color_palette Palette name (`"blue"`, `"red"`, `"viridis"`, or
#'   `"rainbow"`) used when `optional_param2` is numeric rather than
#'   categorical.
#' @param use_manual_point_size If `TRUE`, use `manual_point_size` for every
#'   point instead of `optional_param1_representation`'s mapping.
#' @param manual_point_size Fixed point size used when
#'   `use_manual_point_size` is `TRUE`.
#' @param is_categorical_group Whether `optional_param2`'s column has been
#'   detected as categorical (drives group-based filtering/coloring instead
#'   of a continuous color scale).
#' @param selected_groups Character vector of categorical group values (from
#'   `optional_param2`) to include, when `is_categorical_group` is `TRUE`;
#'   other rows are excluded. Reassigned to "every group present" if none of
#'   the originally-requested groups actually match any data.
#' @return This function's entire local environment as a list
#'   (`as.list(environment())`) - `pointSize`, `pointType`, `pointCol`,
#'   `ternary_points1` (possibly row-filtered), `selected_groups` (possibly
#'   reassigned), `param1_values`, `param1_bins`, `unique_groups`,
#'   `group_colors`, `group_counts`, `MIN_POINT_SIZE`, and `MAX_POINT_SIZE`
#'   are the fields [prepare_ternary_plot_data()] and the preview/save
#'   renderers actually read back; the rest are this block's own internal
#'   working variables, echoed back unchanged from how they already existed
#'   in `prepare_ternary_plot_data()`'s own environment before this
#'   extraction.
#' @export
compute_point_styling <- function(ternary_points1, matrika, optional_param1, optional_param1_representation,
                                   optional_param2, color_palette, use_manual_point_size, manual_point_size,
                                   is_categorical_group, selected_groups) {
  # Prepare optional parameters for plotting
  MIN_POINT_SIZE <- 0.1
  MAX_POINT_SIZE <- 2.5
  pointSize <- rep(MIN_POINT_SIZE, nrow(ternary_points1))
  pointType <- rep(16, nrow(ternary_points1))
  pointCol <- rep("black", nrow(ternary_points1))

  # Optional param 1: point size or point type (enhanced from legacy code)
  if (use_manual_point_size) {
    # Use manual point size for all points
    pointSize <- rep(manual_point_size, nrow(ternary_points1))
    pointType <- rep(16, nrow(ternary_points1))  # Default circle
    if (getOption("ternary.debug", FALSE)) {
      cat("DEBUG: Using manual point size:", manual_point_size, "\n")
    }
  } else if (!is.null(optional_param1)) {
    # Enhanced debugging for optional param1 data extraction
    if (getOption("ternary.debug", FALSE)) {
      cat("DEBUG: Processing optional_param1\n")
      cat("DEBUG: optional_param1$col:", paste(optional_param1$col, collapse = ", "), "\n")
      cat("DEBUG: matrika dimensions:", dim(matrika), "\n")
      cat("DEBUG: matrika column names:", paste(names(matrika), collapse = ", "), "\n")
      cat("DEBUG: optional_param1$col in matrika:", optional_param1$col %in% names(matrika), "\n")
    }

    # Safety check: ensure the column exists in matrika
    if (!all(optional_param1$col %in% names(matrika))) {
      cat("ERROR: Optional param1 column(s) not found in matrika:",
          paste(setdiff(optional_param1$col, names(matrika)), collapse = ", "), "\n")
      cat("Available columns:", paste(names(matrika), collapse = ", "), "\n")
      stop("Optional param1 column not found in processed data")
    }

    param1_values <- matrika[, optional_param1$col, drop = FALSE]
    if (ncol(param1_values) > 1) {
      param1_values <- rowSums(param1_values, na.rm = TRUE)
    } else {
      param1_values <- param1_values[, 1]
    }

    if (getOption("ternary.debug", FALSE)) {
      cat("DEBUG: param1_values extracted, length:", length(param1_values), "\n")
      cat("DEBUG: param1_values class:", class(param1_values), "\n")
      cat("DEBUG: param1_values range:", range(param1_values, na.rm = TRUE), "\n")
    }

    if (optional_param1_representation == "point_size") {
      # Point size representation
      minPointSize <- MIN_POINT_SIZE
      maxSize <- MAX_POINT_SIZE
      # Optional Param 1 is a non-negative physical measurement (wt%, ECD,
      # area, etc.), so this formula's "0 -> minPointSize, max ->
      # maxSize" scaling only breaks down in one real case: every selected
      # value is exactly 0, making max(param1_values) 0 and the division
      # below 0/0 = NaN for every point - which the function's own
      # end-of-function safety check then silently caught and replaced the
      # WHOLE pointSize vector with, collapsing point-size styling entirely
      # with no indication why (confirmed by direct reproduction). Guarded
      # directly instead: every point legitimately sits at the "0" end of
      # the scale in that case, so it maps to minPointSize with no division
      # needed and nothing actually out of range to warn about.
      max_param1 <- max(param1_values, na.rm = TRUE)
      if (is.finite(max_param1) && max_param1 <= 0) {
        pointSize <- rep(minPointSize, length(param1_values))
      } else {
        pointSize <- param1_values * (maxSize - minPointSize) / max_param1 + minPointSize
        # Defense in depth for a value outside [0, max_param1] getting
        # here at all (e.g. a direct, non-UI caller) - clipped to the
        # intended range with a warning naming how many points were
        # affected, instead of letting an out-of-range point render
        # invisibly (a size below minPointSize) or oversized silently.
        out_of_range <- pointSize < minPointSize | pointSize > maxSize
        out_of_range[is.na(out_of_range)] <- FALSE
        if (any(out_of_range)) {
          warning(sprintf(
            "%d point(s) have an out-of-range Optional Param 1 value under Point Size representation; clipped to the visible size range [%.2g, %.2g].",
            sum(out_of_range), minPointSize, maxSize
          ))
        }
        pointSize <- pmin(pmax(pointSize, minPointSize), maxSize)
      }
      pointType <- rep(16, length(param1_values))  # Default circle
    } else if (optional_param1_representation == "point_type") {
      # Point type representation
      pointSize <- 0.7  # Fixed size
      # Create bins for point types
      param1_breaks <- quantile(param1_values, probs = seq(0, 1, length.out = 6), na.rm = TRUE)
      param1_breaks <- unique(param1_breaks)

      if (length(param1_breaks) < 2) {
        param1_bins <- factor(rep(1, length(param1_values)), labels = "All")
        pointType <- rep(16, length(param1_values))  # All circles
      } else {
        param1_bins <- cut(param1_values, breaks = param1_breaks, include.lowest = TRUE)
        # Assign different point types based on bins
        point_types <- c(16, 17, 15, 18, 19)  # circle, triangle, square, diamond, filled diamond
        pointType <- point_types[as.numeric(param1_bins)]
      }
    }
  }
  # No else branch needed here: pointSize/pointType are already correctly
  # set to the full-length rep(MIN_POINT_SIZE/16, nrow(ternary_points1))
  # defaults a few lines up, for exactly this "neither manual size nor
  # optional_param1" case.

  # Optional param 2: color (enhanced to handle categorical groups)
  if (!is.null(optional_param2)) {
    # Enhanced debugging for optional param2 data extraction
    if (getOption("ternary.debug", FALSE)) {
      cat("DEBUG: Processing optional_param2\n")
      cat("DEBUG: optional_param2$col:", paste(optional_param2$col, collapse = ", "), "\n")
      cat("DEBUG: matrika dimensions:", dim(matrika), "\n")
      cat("DEBUG: matrika column names:", paste(names(matrika), collapse = ", "), "\n")
      cat("DEBUG: optional_param2$col in matrika:", optional_param2$col %in% names(matrika), "\n")
    }

    # Safety check: ensure the column exists in matrika
    if (!all(optional_param2$col %in% names(matrika))) {
      cat("ERROR: Optional param2 column(s) not found in matrika:",
          paste(setdiff(optional_param2$col, names(matrika)), collapse = ", "), "\n")
      cat("Available columns:", paste(names(matrika), collapse = ", "), "\n")
      stop("Optional param2 column not found in processed data")
    }

    param2_values <- matrika[, optional_param2$col, drop = FALSE]

    # Optional Param 2 supports exactly one column (see
    # apply_element_and_parameter_filters()'s own matching check) - every
    # UI control that offers it is single-select, but this function is
    # also exported/directly callable, so a caller that bypasses the UI
    # gets a clear error here instead of silently using only the first
    # column.
    if (ncol(param2_values) > 1) {
      stop("Optional Param 2 (", paste(optional_param2$col, collapse = ", "),
           ") has more than one column selected, but it supports exactly one ",
           "(it is a styling dimension, not a summed composition axis like Elements A/B/C). ",
           "Choose a single column.")
    }
    param2_values <- param2_values[, 1]

    if (getOption("ternary.debug", FALSE)) {
      cat("DEBUG: optional_param2$col:", optional_param2$col, "\n")
      cat("DEBUG: matrika dimensions:", dim(matrika), "\n")
      cat("DEBUG: matrika column names:", paste(names(matrika), collapse = ", "), "\n")
      cat("DEBUG: param2_values class:", class(param2_values), "\n")
      cat("DEBUG: param2_values length:", length(param2_values), "\n")
      cat("DEBUG: param2_values first 10 values:", paste(head(param2_values, 10), collapse = ", "), "\n")
      cat("DEBUG: param2_values unique values:", paste(unique(param2_values), collapse = ", "), "\n")
    }

    # Check if this is categorical data
    if (getOption("ternary.debug", FALSE)) {
      cat("DEBUG: is_categorical_group:", is_categorical_group, "\n")
      cat("DEBUG: selected_groups:", if (is.null(selected_groups)) "NULL" else paste(selected_groups, collapse = ", "), "\n")
      cat("DEBUG: selected_groups length:", if (is.null(selected_groups)) 0 else length(selected_groups), "\n")
      cat("DEBUG: param2_values unique values:", paste(unique(param2_values), collapse = ", "), "\n")
    }

    # is_categorical_group alone, not also requiring a non-empty
    # selected_groups: the moment a user picks a categorical column for
    # Optional Param 2, rv$is_categorical_group_1/_2 flips to TRUE
    # immediately (server_ternary_plots_groups.R's detection observer) -
    # but rv$group_selections_1/_2 (and so selected_groups here) stays
    # NULL/empty until the user actually checks a box in the group
    # checklist that appears below it. Routing on is_categorical_group
    # alone handles that gap correctly with no further changes needed:
    # gsub() on a NULL selected_groups returns character(0), matching
    # nothing in param2_values, which lands on the "no groups matched"
    # fallback immediately below and shows every group - the graceful
    # "nothing chosen yet" behavior for that case.
    if (is_categorical_group) {
      # Handle categorical groups
      # Extract group names from selected_groups (remove sample counts in parentheses)
      group_names <- gsub("\\s*\\([^)]*\\)$", "", selected_groups)

      if (getOption("ternary.debug", FALSE)) {
        cat("DEBUG: Extracted group names:", paste(group_names, collapse = ", "), "\n")
        cat("DEBUG: Original selected_groups:", paste(selected_groups, collapse = ", "), "\n")
      }

      # Filter data to selected groups
      group_mask <- param2_values %in% group_names

      if (getOption("ternary.debug", FALSE)) {
        cat("DEBUG: Group mask sum:", sum(group_mask), "out of", length(group_mask), "\n")
        cat("DEBUG: Matching groups found:", sum(group_mask) > 0, "\n")
      }

      # Safety check: if no groups match (including the now-routed-here
      # case of no groups selected yet at all), show all groups instead.
      if (sum(group_mask) == 0) {
        if (is.null(group_names) || length(group_names) == 0) {
          cat("No groups selected yet - showing all groups.\n")
        } else {
          cat("Warning: No data matches selected groups. Showing all groups instead.\n")
        }
        group_mask <- rep(TRUE, length(param2_values))
        selected_groups <- unique(param2_values)
        group_names <- selected_groups
      }

      ternary_points1 <- ternary_points1[group_mask, ]
      param2_values <- param2_values[group_mask]
      pointSize <- pointSize[group_mask]
      pointType <- pointType[group_mask]

      # Generate distinct colors for groups
      unique_groups <- unique(param2_values)
      n_groups <- length(unique_groups)
      group_colors <- generate_distinct_colors(n_groups)

      # Assign colors to groups
      group_color_map <- setNames(group_colors, unique_groups)
      pointCol <- group_color_map[as.character(param2_values)]

      # Safety check: ensure pointCol doesn't contain NA values
      if (any(is.na(pointCol))) {
        cat("Warning: Some groups don't have colors assigned. Using default colors.\n")
        pointCol[is.na(pointCol)] <- "black"
      }

      # Safety check: ensure pointSize and pointType don't contain NA values
      if (any(is.na(pointSize))) {
        cat("Warning: Some point sizes are NA. Using default size.\n")
        pointSize[is.na(pointSize)] <- MIN_POINT_SIZE
      }
      if (any(is.na(pointType))) {
        cat("Warning: Some point types are NA. Using default type.\n")
        pointType[is.na(pointType)] <- 16
      }

      # Store group information for legend
      group_counts <- table(param2_values)

    } else {
      # Handle numeric data (existing logic)
      #
      # Guard: this branch is only reachable for a column the caller has
      # already decided ISN'T categorical (is_categorical_group FALSE, or
      # TRUE with no matching groups after the earlier fallback) - but
      # nothing downstream of that decision re-checks it's actually
      # numeric before quantile()ing it a few lines down. In the live app
      # this can't happen via the UI as of today's cap fix (a text column
      # with too many distinct values to be treated as categorical is
      # exactly the scenario that fix targets - see
      # server_ternary_plots_groups.R's own comment), but
      # prepare_ternary_plot_data()/general_ternary_plot() are exported,
      # directly callable functions, not gated behind that UI-side
      # detection - a direct call passing a genuinely non-numeric
      # optional_param2 column with is_categorical_group left FALSE (or a
      # categorical column whose real cardinality exceeds what the caller
      # checked) would otherwise still reach quantile() on text data and
      # crash with a raw "non-numeric argument to binary operator" -
      # exactly the class of bug fixed for the "no groups selected yet"
      # case earlier in this file. Failing clearly here, at the actual
      # point of the mismatch, costs nothing for the common numeric case
      # and replaces that raw crash with an actionable message for the
      # rest.
      if (!is.numeric(param2_values)) {
        # Deliberately not claiming a specific cause (e.g. "too many
        # distinct values") - is_categorical_group can land FALSE here for
        # two different, real reasons: the column's own cardinality
        # exceeded the 50-unique-values cap (server_ternary_plots_groups.R),
        # or - in the Multiple Ternary Creator's batch path specifically -
        # categorical detection is never wired up at all regardless of the
        # column's cardinality (extract_ternary_params()'s own safety-check
        # re-detection short-circuits there before it can run at all,
        # confirmed by reading server_ternary_plots_batch.R's caller - a
        # documented, pre-existing limitation of that tab, not something
        # this fix changes). A message asserting the wrong one of those two
        # causes would send a batch-tab user chasing a column-cardinality
        # fix that was never the actual problem.
        stop("Optional Param 2 (", paste(optional_param2$col, collapse = "+"),
             ") has non-numeric values but isn't being treated as a categorical color grouping here ",
             "(either it has more than 50 distinct values, or this tab doesn't support categorical grouping for Optional Param 2). ",
             "Choose a column with 50 or fewer distinct values in a tab that supports categorical grouping, or use a numeric column instead.")
      }
      # Check if the selected column is Aspect.Ratio for special handling
      if (length(optional_param2$col) == 1 && optional_param2$col == "Aspect.Ratio") {
        # Use hardcoded breaks for Aspect.Ratio
        param2_breaks <- c(1, 1.5, 3, 5, 10, 100000)
        param2_labels <- c("1-1.5", "1.5-3", "3-5", "5-10", "10+")
        param2_bins <- cut(param2_values, breaks = param2_breaks, labels = param2_labels, include.lowest = TRUE)
        n_colors <- length(levels(param2_bins))
      } else {
        # Use quantile-based binning for other columns
        param2_breaks <- quantile(param2_values, probs = seq(0, 1, length.out = 6), na.rm = TRUE)
        param2_breaks <- unique(param2_breaks) # Make breaks unique

        if (length(param2_breaks) < 2) {
          # All values identical or not enough to make bins; fallback coloring
          param2_bins <- factor(rep(1, length(param2_values)), labels = "All")
          n_colors <- 1
        } else {
          param2_bins <- cut(param2_values, breaks = param2_breaks, include.lowest = TRUE)
          n_colors <- length(levels(param2_bins))
        }
      }

      if (color_palette == "blue") {
        param2_colors <- colorRampPalette(c("#357ABD", "#002147"))(n_colors)
      } else if (color_palette == "red") {
        param2_colors <- colorRampPalette(c("#FF6666", "#990000"))(n_colors)
      } else if (color_palette == "viridis") {
        param2_colors <- viridisLite::viridis(n_colors)
      } else if (color_palette == "rainbow") {
        param2_colors <- rainbow(n_colors)
      } else {
        param2_colors <- rep("grey", n_colors)
      }
      pointCol <- param2_colors[as.numeric(param2_bins)]
    }

  }
  # No else branch needed here either - same reasoning as the matching
  # pointSize/pointType case above: pointCol is already correctly
  # rep("black", nrow(ternary_points1)) from its initial declaration.

  # Final safety check: ensure all vectors are properly initialized
  n_points <- nrow(ternary_points1)
  if (length(pointSize) != n_points || any(is.na(pointSize))) {
    cat("Warning: Point size vector has issues. Reinitializing.\n")
    pointSize <- rep(MIN_POINT_SIZE, n_points)
  }
  if (length(pointType) != n_points || any(is.na(pointType))) {
    cat("Warning: Point type vector has issues. Reinitializing.\n")
    pointType <- rep(16, n_points)
  }
  if (length(pointCol) != n_points || any(is.na(pointCol))) {
    cat("Warning: Point color vector has issues. Reinitializing.\n")
    pointCol <- rep("black", n_points)
  }

  if (getOption("ternary.debug", FALSE)) {
    if (length(pointSize) > 0) {
      cat("DEBUG: Point size range:", range(pointSize), "\n")
    } else {
      cat("DEBUG: Point size range: empty vector\n")
    }
    if (length(pointType) > 0) {
      cat("DEBUG: Point type range:", range(pointType), "\n")
    } else {
      cat("DEBUG: Point type range: empty vector\n")
    }
    if (length(pointCol) > 0) {
      cat("DEBUG: Point color unique values:", unique(pointCol), "\n")
    } else {
      cat("DEBUG: Point color unique values: empty vector\n")
    }
  }

  as.list(environment())
}
