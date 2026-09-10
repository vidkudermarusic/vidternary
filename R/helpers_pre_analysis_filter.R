# ---- Pre-Analysis Data Filter (shared across EVS, Spatial Clustering, and
#      Point Pattern Analysis) ----
# Lets the user exclude rows before the tab's own statistical analysis runs
# at all - e.g. "Area > 1" to drop inclusions too small to matter, or "Fe >
# 10" to restrict to a chemistry range - rather than only being able to
# filter downstream of an already-computed result. Requested directly: EVS,
# Spatial Clustering, and Point Pattern Analysis all analyze whatever rows
# survive upload with no way to exclude some first (e.g. a real dataset
# with inclusions as small as 0.01 um2, where running extreme-value/spatial
# statistics on the smallest ones is described as "pointless").
#
# Deliberately a set of small, shared, independently-testable helper
# functions - not a merge of the three tabs themselves, which stay their
# own separate files (this project's own established convention: EVS/
# Spatial Clustering/Point Pattern Analysis are already three separate
# files despite conceptual overlap). Each tab calls these same functions
# from its own ui_*.R/server_*.R, the same way `cite_link()` or
# `apply_filter()` itself is already shared rather than copy-pasted three
# times with three chances to drift apart.
#
# Built directly on `apply_filter()` (helpers_filters.R) - the same safe,
# non-eval() "> 10"/"<= 5.2"/"!= 0" comparison parser the Ternary Plots
# tab's own per-element filters already use, so a user who already knows
# that syntax from elsewhere in the app doesn't have to learn a second one
# here. Multiple selected filter columns combine with AND logic (applied
# in sequence) - matching how the Ternary Plots tab's own per-element
# filters already combine, and the user's own worked example ("Area > 1"
# AND "Fe > 10" together, not either/or).

#' Build the static part of a tab's "Pre-Analysis Data Filter" UI section
#'
#' A column selector plus a `uiOutput()` placeholder for the dynamic
#' per-column filter boxes, rendered separately by
#' `render_pre_filter_inputs()` (called from the tab's own `renderUI()`, so
#' the number of filter boxes can grow/shrink as columns are selected).
#'
#' @param ns The tab's own namespacing function (`NS(id)`, as already built
#'   by every `create_*_tab()`).
#' @param prefix Short, tab-unique input-ID prefix (e.g. `"evs"`,
#'   `"spatial"`, `"ppp"`) - keeps this section's input IDs from colliding
#'   with the tab's own other inputs, and lets three separate tabs each
#'   safely use these same functions inside their own module namespace.
#' @return A `shiny::div()`.
#' @export
create_pre_filter_ui <- function(ns, prefix) {
  div(style = "border: 1px solid #6c757d; padding: 15px; border-radius: 5px; margin: 10px 0; background-color: #f8f9fa;",
    h4(" Pre-Analysis Data Filter (optional)"),
    helpText("Exclude rows before running the analysis below - e.g. \"Area > 1\" to drop inclusions smaller than 1 (in whatever unit your Area column uses), or \"Fe > 10\" to restrict to a chemistry range. Leave this empty to use every uploaded row. Multiple columns combine with AND (a row must satisfy all of them)."),
    selectizeInput(ns(paste0(prefix, "_filter_cols")), "Filter columns:", choices = NULL, multiple = TRUE,
                    options = list(placeholder = "Select column(s) to filter on, e.g. Area, Fe (wt.%)")),
    uiOutput(ns(paste0(prefix, "_filter_inputs")))
  )
}

#' Render the dynamic per-column filter condition inputs
#'
#' Call from the tab's own `output$<prefix>_filter_inputs <- renderUI({ ... })`.
#' One `textInput()` per selected column, each accepting the same
#' `apply_filter()` syntax (`"> 10"`, `"<= 5.2"`, `"!= 0"`, etc.).
#'
#' @param ns The tab's own namespacing function.
#' @param prefix Same prefix passed to `create_pre_filter_ui()`.
#' @param selected_columns Character vector of currently-selected filter
#'   columns (typically `input[[paste0(prefix, "_filter_cols")]]`).
#' @return A `shiny::tagList()` of `textInput()`s, or `NULL` if
#'   `selected_columns` is empty.
#' @export
render_pre_filter_inputs <- function(ns, prefix, selected_columns) {
  if (is.null(selected_columns) || length(selected_columns) == 0) return(NULL)
  tagList(lapply(selected_columns, function(col) {
    safe_col <- gsub("[^A-Za-z0-9]", "_", col)
    textInput(ns(paste0(prefix, "_filter_", safe_col)),
              paste0("Filter - ", col, ":"),
              placeholder = "e.g. > 1, <= 50, != 0")
  }))
}

#' Collect the current pre-analysis filter values from Shiny `input`
#'
#' @param input The Shiny `input` object.
#' @param prefix Same prefix passed to `create_pre_filter_ui()`.
#' @param selected_columns Character vector of currently-selected filter
#'   columns.
#' @return A named list of non-empty filter condition strings, keyed by
#'   column name. A column selected but left blank is simply omitted (not
#'   an error) - matches `collect_filters()`'s own established convention
#'   for the Ternary Plots tab's per-element filters.
#' @export
collect_pre_filters <- function(input, prefix, selected_columns) {
  if (is.null(selected_columns) || length(selected_columns) == 0) return(list())
  filters <- list()
  for (col in selected_columns) {
    safe_col <- gsub("[^A-Za-z0-9]", "_", col)
    val <- input[[paste0(prefix, "_filter_", safe_col)]]
    # is.na(val) is checked explicitly before nzchar()/trimws() - nzchar(NA)
    # returns TRUE by default in base R (a well-known gotcha: NA is treated
    # as "having characters" unless keepNA is set), so without this guard
    # a literal NA value here would silently be collected as a real,
    # non-empty filter condition ("NA", after trimws()'s own coercion)
    # rather than skipped - confirmed to actually happen, not a
    # theoretical risk, before this guard was added.
    if (!is.null(val) && !is.na(val) && nzchar(trimws(val))) filters[[col]] <- trimws(val)
  }
  filters
}

#' Apply a set of pre-analysis column filters to a data frame (AND logic)
#'
#' Applies each column's filter condition in sequence via `apply_filter()`
#' - a row must satisfy every one to survive. Column names and filter
#' syntax errors are both reported with the specific column named, rather
#' than `apply_filter()`'s own column-agnostic message alone.
#'
#' @param df A data frame.
#' @param filters A named list of filter condition strings, e.g. from
#'   `collect_pre_filters()` - `list(Area = "> 1", "Fe (wt.%)" = "> 10")`.
#' @return The filtered data frame (`df` unchanged if `filters` is empty).
#' @export
apply_pre_filters <- function(df, filters) {
  if (length(filters) == 0) return(df)
  for (col in names(filters)) {
    if (!col %in% names(df)) {
      stop("Filter column '", col, "' was not found in the uploaded data.")
    }
    if (!is.numeric(df[[col]])) {
      stop("Filter column '", col, "' is not numeric - only numeric columns can be filtered this way.")
    }
    df <- tryCatch(apply_filter(df, col, filters[[col]]), error = function(e) {
      stop("Invalid filter for column '", col, "' (\"", filters[[col]], "\"): ", e$message)
    })
  }
  df
}
