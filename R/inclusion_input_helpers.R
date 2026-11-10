# ---- Inclusion add-on: helpers for the Shiny panel (pure functions) ----

#' Read a steel composition typed as text
#'
#' Accepts `Symbol=value` pairs separated by commas or semicolons, e.g.
#' `"Fe=70, Cr=20, Ni=10"`. Values are wt% written with a dot as the
#' decimal separator (a comma separates the elements).
#'
#' @param text The typed text.
#' @return A named numeric vector, element symbol -> wt%.
#' @export
parse_steel_composition <- function(text) {
  parts <- trimws(strsplit(as.character(text), "[,;]")[[1]])
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0) stop("Enter the steel composition, e.g. Fe=70, Cr=20, Ni=10.", call. = FALSE)
  m <- regmatches(parts, regexec("^([A-Z][a-z]?)\\s*=\\s*([0-9]*\\.?[0-9]+)$", parts))
  bad <- lengths(m) != 3
  if (any(bad)) {
    stop("Cannot read '", parts[bad][1], "'. Use Symbol=value with a dot as the decimal separator, e.g. Fe=70.5, Cr=18.", call. = FALSE)
  }
  out <- setNames(as.numeric(vapply(m, `[`, character(1), 3)), vapply(m, `[`, character(1), 2))
  if (anyDuplicated(names(out))) stop("Element ", names(out)[duplicated(names(out))][1], " is given twice.", call. = FALSE)
  unknown <- setdiff(names(out), names(inclusion_atomic_weights()))
  if (length(unknown) > 0) stop("Unknown element: ", paste(unknown, collapse = ", "), ".", call. = FALSE)
  if (any(out <= 0)) stop("Every value must be greater than zero.", call. = FALSE)
  out
}

#' Element symbols with a wt% column in a data frame
#'
#' @param data A data frame.
#' @return Character vector of element symbols (order of the columns).
#' @export
inclusion_detected_elements <- function(data) {
  if (is.null(data)) return(character(0))
  names(inclusion_element_columns(names(data)))
}

#' The likely matrix element of a dataset
#'
#' The element with the highest median wt% (Fe in steel), or `""` when no
#' element reaches `min_median` wt%.
#'
#' @param data A data frame with element wt% columns.
#' @param min_median Smallest median wt% that counts as a matrix.
#' @return An element symbol or `""`.
#' @export
default_matrix_element <- function(data, min_median = 10) {
  cols <- if (is.null(data)) character(0) else inclusion_element_columns(names(data))
  if (length(cols) == 0) return("")
  med <- vapply(cols, function(cn) stats::median(suppressWarnings(as.numeric(data[[cn]])), na.rm = TRUE), numeric(1))
  if (all(is.na(med)) || max(med, na.rm = TRUE) < min_median) return("")
  names(med)[which.max(med)]
}

#' Preset choices for a select input, grouped by kind
#'
#' @param library From [load_inclusion_presets()].
#' @return A named list of named character vectors (label -> preset id).
#' @export
inclusion_preset_choices <- function(library = load_inclusion_presets()) {
  p <- library$presets
  mk <- function(rows) stats::setNames(rows$id, paste0(rows$name, " - ", rows$applies_to))
  list("Compound diagrams (oxides, sulfides, nitrides)" = mk(p[p$kind == "compound", , drop = FALSE]),
       "Element diagrams (element wt%)" = mk(p[p$kind == "element", , drop = FALSE]))
}

#' Sulfur assignment order from its input value
#'
#' @param key `"ca_mn"`, `"mn"` or `"ca"`.
#' @return Character vector of elements, in order.
#' @export
inclusion_s_order <- function(key) {
  switch(key, ca_mn = c("Ca", "Mn"), mn = "Mn", ca = "Ca", stop("Unknown sulfur order '", key, "'.", call. = FALSE))
}

#' Tables for exporting an inclusion analysis
#'
#' @param result From [analyze_inclusion_preset()].
#' @param data The data frame that was analysed.
#' @return A named list of data frames: `Particles` (one row per input row:
#'   row number, `Feature`/`Field`/area columns if present, class, corner
#'   fractions, coverage, why excluded), `Summary`, `Settings`, `Sources`.
#' @export
inclusion_result_tables <- function(result, data) {
  lab <- result$preset$labels
  ids <- data[, intersect(c("Feature", "Field", grep("^Area\\.", names(data), value = TRUE)), names(data)), drop = FALSE]
  particles <- data.frame(row = seq_len(nrow(data)), ids, class = result$class,
                          stringsAsFactors = FALSE, check.names = FALSE)
  particles[[lab[1]]] <- result$coords$A
  particles[[lab[2]]] <- result$coords$B
  particles[[lab[3]]] <- result$coords$C
  particles$coverage_pct <- result$coverage_pct
  particles$status <- ifelse(is.na(result$excluded), "plotted", result$excluded)
  src <- result$preset$sources
  list(
    Particles = particles,
    Summary = result$summary,
    Settings = data.frame(setting = result$settings, stringsAsFactors = FALSE),
    Sources = if (nrow(src) > 0) src[, c("citation", "url")] else data.frame(citation = character(0), url = character(0))
  )
}
