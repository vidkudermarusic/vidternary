# ---- Import: bring a particle table into the app's standard shape ----
# standardize_particle_table() renames the mapped columns to fixed names
# with fixed units (um, um2, mm, degrees, wt%), fills in what can be
# computed (equivalent circle diameter, aspect ratio) and converts atomic
# percent to weight percent. Every tab then finds its columns by itself,
# whatever instrument the file came from.

#' Atomic percent to weight percent
#'
#' `wt_i = at_i * M_i / sum(at_j * M_j) * 100`, per row, over the elements
#' given. The result always sums to 100 over those elements, also when the
#' atomic percentages did not (e.g. because an element was left out).
#'
#' @param at Data frame or matrix of atomic percentages, one column per
#'   element, named by element symbol.
#' @param weights Atomic weights, see [import_atomic_weights()].
#' @return A data frame of weight percentages, same shape and names.
#' @export
atomic_to_weight_percent <- function(at, weights = import_atomic_weights()) {
  at <- as.data.frame(at, check.names = FALSE)
  unknown <- setdiff(names(at), names(weights))
  if (length(unknown)) stop("No atomic weight for: ", paste(unknown, collapse = ", "), ".", call. = FALSE)
  m <- as.matrix(at)
  storage.mode(m) <- "double"
  mass <- sweep(m, 2, weights[colnames(m)], "*")
  total <- rowSums(mass, na.rm = TRUE)
  out <- 100 * mass / ifelse(total > 0, total, NA_real_)
  as.data.frame(out, check.names = FALSE)
}

#' Bring a particle table into the standard shape
#'
#' @param data The table as read ([read_particle_table()]).
#' @param mapping Named character vector or list role -> column name (see
#'   [particle_roles()]); roles left out or `NA` are not mapped.
#' @param units Named list role -> unit (`"nm"`, `"um"`, `"mm"`) for the
#'   length, area and position roles. Default: the unit named in the
#'   column, else um (lengths, area) and mm (positions).
#' @param elements Data frame `element`, `column` (see
#'   [detect_element_columns()]); `NULL` for none.
#' @param basis `"wt"` when the element columns are weight percent, `"at"`
#'   when they are atomic percent and have to be converted.
#' @param keep_other Keep the columns that were not mapped (after the
#'   standard ones). Default `TRUE`.
#' @return A data frame with the standard columns (`Feature`, `Field`,
#'   `Area.(um2)`, `ECD.(um)`, `Length.(um)`, `Breadth.(um)`,
#'   `Aspect.Ratio`, `Direction.(degrees)`, `Perimeter.(um)`, `X.(mm)`,
#'   `Y.(mm)`, then `El.(Wt%)` per element), and a `standardize_notes`
#'   attribute listing what was converted or computed.
#' @export
standardize_particle_table <- function(data, mapping, units = list(), elements = NULL,
                                       basis = c("wt", "at"), keep_other = TRUE) {
  basis <- match.arg(basis)
  roles <- particle_roles()
  mapping <- unlist(mapping)
  mapping <- mapping[!is.na(mapping) & nzchar(mapping) & mapping != "none"]
  unknown_roles <- setdiff(names(mapping), roles$role)
  if (length(unknown_roles)) stop("Unknown column role: ", paste(unknown_roles, collapse = ", "), ".", call. = FALSE)
  missing_cols <- setdiff(mapping, names(data))
  if (length(missing_cols)) stop("Column not found in the file: ", paste(missing_cols, collapse = ", "), ".", call. = FALSE)
  if (anyDuplicated(mapping)) {
    stop("Column '", mapping[duplicated(mapping)][1], "' is assigned to more than one role.", call. = FALSE)
  }

  notes <- character(0)
  out <- list()
  for (i in seq_len(nrow(roles))) {
    role <- roles$role[i]
    if (!role %in% names(mapping)) next
    col <- mapping[[role]]
    v <- data[[col]]
    kind <- roles$kind[i]
    if (kind %in% c("length", "area", "position")) {
      v <- suppressWarnings(as.numeric(v))
      unit <- units[[role]]
      if (is.null(unit) || is.na(unit) || !nzchar(unit) || identical(unit, "auto")) unit <- detect_unit(col)
      if (is.na(unit) || identical(unit, "pixels")) {
        if (identical(unit, "pixels")) {
          stop("Column '", col, "' is in pixels; choose its real unit, or map a column in um or mm.", call. = FALSE)
        }
        unit <- if (kind == "position") "mm" else "um"
        notes <- c(notes, sprintf("%s: no unit named in '%s', taken as %s.", roles$label[i], col, unit))
      }
      factor <- .unit_to_um(unit)
      factor <- switch(kind, length = factor, area = factor^2, position = factor / 1000)
      if (factor != 1) {
        notes <- c(notes, sprintf("%s: converted from %s%s.", roles$label[i], unit, if (kind == "area") "2" else ""))
      }
      v <- v * factor
    } else if (kind %in% c("angle", "ratio")) {
      v <- suppressWarnings(as.numeric(v))
    }
    out[[roles$canonical[i]]] <- v
  }

  if (is.null(out[["ECD.(um)"]]) && !is.null(out[["Area.(um2)"]])) {
    out[["ECD.(um)"]] <- 2 * sqrt(out[["Area.(um2)"]] / pi)
    notes <- c(notes, "Equivalent circle diameter computed from the area.")
  }
  if (is.null(out[["Area.(um2)"]]) && !is.null(out[["ECD.(um)"]])) {
    out[["Area.(um2)"]] <- pi / 4 * out[["ECD.(um)"]]^2
    notes <- c(notes, "Area computed from the equivalent circle diameter.")
  }
  if (is.null(out[["Aspect.Ratio"]]) && !is.null(out[["Length.(um)"]]) && !is.null(out[["Breadth.(um)"]])) {
    out[["Aspect.Ratio"]] <- out[["Length.(um)"]] / out[["Breadth.(um)"]]
    notes <- c(notes, "Aspect ratio computed as length / breadth.")
  }
  ordered <- intersect(roles$canonical, names(out))
  result <- as.data.frame(out[ordered], check.names = FALSE, stringsAsFactors = FALSE)
  if (length(ordered) == 0) result <- data.frame(row.names = seq_len(nrow(data)))

  used <- unname(mapping)
  if (!is.null(elements) && nrow(elements) > 0) {
    missing_el <- setdiff(elements$column, names(data))
    if (length(missing_el)) stop("Element column not found in the file: ", paste(missing_el, collapse = ", "), ".", call. = FALSE)
    chem <- as.data.frame(lapply(elements$column, function(cn) suppressWarnings(as.numeric(data[[cn]]))))
    names(chem) <- elements$element
    if (basis == "at") {
      chem <- atomic_to_weight_percent(chem)
      notes <- c(notes, sprintf("Atomic percent converted to weight percent over %d elements (normalised to 100 %%).", ncol(chem)))
    }
    names(chem) <- paste0(names(chem), ".(Wt%)")
    result <- cbind(result, chem)
    used <- c(used, elements$column)
  }
  if (keep_other) {
    other <- setdiff(names(data), c(used, names(result)))
    if (length(other)) result <- cbind(result, data[, other, drop = FALSE])
  }
  rownames(result) <- NULL
  attr(result, "standardize_notes") <- notes
  result
}
