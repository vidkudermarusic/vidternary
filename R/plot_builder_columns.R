# ---- Plot Builder: which columns can be a grouping X axis ----
# Violin, box and bar charts draw one group per distinct X value
# (build_custom_plot() wraps X in factor()). A continuous column such as a
# diameter has almost one distinct value per particle, so it would produce
# thousands of groups: on 2316 particles a violin chart took about 20 s to
# draw and 50 s to save, with the app frozen meanwhile. These helpers keep
# such columns out of the X choices and refuse them in build_custom_plot().

# Largest number of groups the Plot Builder draws on a grouping X axis.
BUILDER_MAX_GROUPS <- 50

# Chart types whose X axis is a grouping (one group per distinct value).
BUILDER_GROUPED_X_TYPES <- c("violin", "box", "bar")

#' Count the distinct non-missing values of a column
#'
#' @param values A vector.
#' @return An integer.
#' @export
builder_distinct_count <- function(values) {
  length(unique(values[!is.na(values)]))
}

#' Columns that can serve as a grouping X axis in the Plot Builder
#'
#' Returns the text/factor columns first, then the numeric ones, keeping only
#' columns with between 1 and `max_groups` distinct non-missing values. A
#' numeric column with few values (a field number, a class code) stays
#' available; a continuous measurement does not.
#'
#' @param data A data frame.
#' @param max_groups Largest allowed number of distinct values. Default
#'   `BUILDER_MAX_GROUPS`.
#' @return A character vector of column names (possibly empty).
#' @export
builder_group_columns <- function(data, max_groups = BUILDER_MAX_GROUPS) {
  if (!is.data.frame(data) || ncol(data) == 0) return(character(0))
  is_cat <- vapply(data, function(col) is.character(col) || is.factor(col) || is.logical(col), logical(1))
  is_num <- vapply(data, is.numeric, logical(1))
  n_distinct <- vapply(data, builder_distinct_count, integer(1))
  ok <- n_distinct >= 1 & n_distinct <= max_groups
  c(names(data)[is_cat & ok], names(data)[is_num & !is_cat & ok])
}

#' Stop when a grouping X column has too many distinct values
#'
#' Called by `build_custom_plot()` for violin, box and bar charts, so a preset
#' or a direct call cannot bring back a continuous X column.
#'
#' @param data A data frame.
#' @param x Name of the X column.
#' @param type Chart type.
#' @param max_groups Largest allowed number of distinct values.
#' @return `invisible(TRUE)`, or an error with an explanation.
#' @export
check_builder_group_count <- function(data, x, type, max_groups = BUILDER_MAX_GROUPS) {
  if (!(type %in% BUILDER_GROUPED_X_TYPES)) return(invisible(TRUE))
  if (is.null(x) || !(x %in% names(data))) stop("Column '", x, "' is not in the data.")
  n <- builder_distinct_count(data[[x]])
  if (n > max_groups) {
    stop("Column '", x, "' has ", n, " different values, so a ", type,
         " chart would draw ", n, " groups (limit ", max_groups, "). ",
         "Choose a column with groups (file, class) for the X axis, ",
         "or use a histogram or scatter plot for a continuous column.",
         call. = FALSE)
  }
  invisible(TRUE)
}
