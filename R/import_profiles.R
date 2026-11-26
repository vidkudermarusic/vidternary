# ---- Import: profiles (a saved column mapping for one kind of export) ----
# A profile records which column plays which role, the units, the element
# columns and whether they are weight or atomic percent. Saved as a small
# JSON file, it turns the next file from the same instrument into a
# one-click import, and documents how a dataset was read.

#' Build an import profile from a table
#'
#' @param data A data frame as read by [read_particle_table()].
#' @param name Name of the profile (e.g. the instrument and software).
#' @return A list: `name`, `mapping` (role -> column), `units` (role ->
#'   unit for the roles that have one), `elements` (data frame `element`,
#'   `column`, `basis`), `basis` (`"at"` when most element columns are
#'   atomic percent, else `"wt"`).
#' @export
detect_import_profile <- function(data, name = "Detected automatically") {
  mapping <- detect_column_roles(data)
  roles <- particle_roles()
  with_unit <- roles$role[roles$kind %in% c("length", "area", "position")]
  units <- lapply(stats::setNames(with_unit, with_unit), function(r) {
    if (is.na(mapping[[r]])) return(NULL)
    u <- detect_unit(mapping[[r]])
    if (is.na(u) || u == "pixels") NULL else u
  })
  elements <- detect_element_columns(data, exclude = mapping[!is.na(mapping)])
  list(name = name, mapping = as.list(mapping[!is.na(mapping)]),
       units = units[!vapply(units, is.null, logical(1))], elements = elements,
       basis = if (nrow(elements) && mean(elements$basis == "at") > 0.5) "at" else "wt")
}

#' Save and read an import profile
#'
#' @param profile A profile, see [detect_import_profile()].
#' @param path File to write or read (JSON).
#' @return `write_import_profile()` returns `path` invisibly;
#'   `read_import_profile()` returns the profile.
#' @export
write_import_profile <- function(profile, path) {
  out <- list(app = "vidternary", kind = "import profile", version = 1L, name = profile$name,
              mapping = profile$mapping, units = profile$units,
              elements = stats::setNames(as.list(profile$elements$column), profile$elements$element),
              basis = profile$basis)
  jsonlite::write_json(out, path, pretty = TRUE, auto_unbox = TRUE, null = "null")
  invisible(path)
}

#' @rdname write_import_profile
#' @export
read_import_profile <- function(path) {
  raw <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE),
                  error = function(e) stop("The profile file is not valid JSON.", call. = FALSE))
  if (!identical(raw$kind, "import profile")) {
    stop("This file is not a vidternary import profile.", call. = FALSE)
  }
  as_chr <- function(x) lapply(x, function(v) if (is.null(v)) NA_character_ else as.character(v))
  el <- as_chr(raw$elements)
  list(name = raw$name %||% "Loaded profile",
       mapping = as_chr(raw$mapping), units = as_chr(raw$units),
       elements = data.frame(element = names(el) %||% character(0), column = unlist(el, use.names = FALSE) %||% character(0),
                             basis = rep(raw$basis %||% "wt", length(el)), stringsAsFactors = FALSE),
       basis = raw$basis %||% "wt")
}

#' Check a profile against a table and apply it
#'
#' @param data A data frame as read by [read_particle_table()].
#' @param profile A profile.
#' @param keep_other See [standardize_particle_table()].
#' @return The standardised table. Stops with the list of missing columns
#'   when the file does not match the profile.
#' @export
apply_import_profile <- function(data, profile, keep_other = TRUE) {
  needed <- c(unlist(profile$mapping), profile$elements$column)
  needed <- needed[!is.na(needed)]
  missing_cols <- setdiff(needed, names(data))
  if (length(missing_cols)) {
    stop("The file does not match the profile '", profile$name, "'. Missing column(s): ",
         paste(missing_cols, collapse = ", "), ".", call. = FALSE)
  }
  standardize_particle_table(data, profile$mapping, profile$units, profile$elements, profile$basis, keep_other)
}
