# ---- EDS reliability: X-ray lines that overlap ----
# Some elements cannot be told apart reliably by EDS because their lines
# fall on top of each other: N K (0.39 keV) under Ti L (0.45), O K (0.52)
# under Cr L (0.57), V K-alpha (4.95) under Ti K-beta (4.93), and S K
# (2.31), Mo L (2.29), Pb M (2.35) and Bi M (2.42) in one cluster. The
# software splits such a peak by deconvolution, so one element of a group
# can be reported in place of another - most of all in small particles
# with a weak signal. Two elements of one group should therefore not be
# read as independent measurements in one diagram, filter or analysis.
#
# The functions here find such combinations among chosen columns and word
# the warning every tab shows. The line energies are textbook values,
# quoted from memory to explain the groups; the groups themselves are the
# ones this project has kept apart in its figures.

.eds_overlap_groups <- list(c("Ti", "N"), c("Cr", "O"), c("Ti", "V"), c("S", "Mo", "Pb", "Bi"))

#' EDS peak overlaps among a set of elements
#'
#' @param elements Element symbols.
#' @return Character vector, one entry per overlap group with two or more
#'   of its elements present (e.g. `"Ti - N"`); empty when there is none.
#' @export
eds_overlaps <- function(elements) {
  hits <- lapply(.eds_overlap_groups, function(g) intersect(g, elements))
  vapply(hits[lengths(hits) >= 2], paste, character(1), collapse = " - ")
}

#' Element symbols behind a set of column names
#'
#' A column counts as an element when its name is an element symbol, alone
#' or followed by something that is not a letter (`"Ti"`, `"Ti.(Wt%)"`,
#' `"Ti (At%)"`). Other columns (`"Area"`, `"Field"`, `"Mean.grey"`) are
#' skipped.
#'
#' @param columns Column names (any vector; `NULL`, empty strings and
#'   `"none"` are ignored).
#' @param weights Atomic weights; only symbols in it are recognised.
#' @return Character vector of distinct element symbols, in order of first
#'   appearance.
#' @export
column_elements <- function(columns, weights = import_atomic_weights()) {
  columns <- as.character(unlist(columns))
  columns <- columns[!is.na(columns) & nzchar(columns) & columns != "none"]
  if (length(columns) == 0) return(character(0))
  m <- regmatches(columns, regexec("^([A-Z][a-z]?)(?:[^A-Za-z].*)?$", columns, perl = TRUE))
  sym <- vapply(m, function(x) if (length(x) == 2) x[2] else NA_character_, character(1))
  unique(sym[!is.na(sym) & sym %in% names(weights)])
}

#' Warning text for overlapping EDS lines among chosen columns
#'
#' @param columns Column names chosen for one diagram or analysis (axes,
#'   colour, size, filters - everything that is read together).
#' @param what What the columns are chosen for, to complete the sentence
#'   "... chosen for <what>" (e.g. `"this diagram"`).
#' @return One string, or `NULL` when no two chosen elements overlap.
#' @export
eds_overlap_message <- function(columns, what = "this diagram") {
  overlaps <- eds_overlaps(column_elements(columns))
  if (length(overlaps) == 0) return(NULL)
  sprintf("The elements chosen for %s include overlapping EDS lines (%s). One element of such a group can be reported in place of another, most of all in small particles, so a pattern that involves both may be an artefact of peak deconvolution. Leave one of them out, or check the pattern on a few spectra.",
          what, paste(overlaps, collapse = "; "))
}

#' Warning box for overlapping EDS lines
#'
#' @param columns,what See [eds_overlap_message()].
#' @return A `shiny::div()`, or `NULL` when there is nothing to warn about.
#' @export
eds_overlap_ui <- function(columns, what = "this diagram") {
  msg <- eds_overlap_message(columns, what)
  if (is.null(msg)) return(NULL)
  div(style = "color: #856404; background-color: #fff3cd; border: 1px solid #ffeeba; border-radius: 4px; padding: 8px; font-size: 12px; margin: 8px 0;",
      strong("EDS peak overlap: "), msg)
}

#' Elements an inclusion preset reads from the analysis
#'
#' For an element preset these are the corner elements; for a compound
#' preset the cations of the corner compounds, sulfur when a corner holds a
#' sulfide, and nitrogen when a corner holds a nitride or titanium is
#' assigned by the measured nitrogen. Oxygen is never read: oxides are
#' computed from their cations.
#'
#' @param preset A preset from [get_inclusion_preset()].
#' @param ti_as How titanium is counted (`"TiN"`, `"TiO2"` or `"by_N"`).
#' @param ignore_elements Elements left out of the analysis.
#' @return Character vector of element symbols.
#' @export
inclusion_preset_elements <- function(preset, ti_as = "TiN", ignore_elements = character(0)) {
  if (preset$kind == "compound") {
    compounds <- inclusion_compounds()
    used <- compounds[compounds$compound %in% unlist(lapply(preset$corners, corner_compounds, compounds = compounds)), ]
    elements <- c(used$cation, if (any(used$kind == "sulfide")) "S",
                  if (any(used$kind == "nitride") && identical(ti_as, "by_N")) "N")
  } else {
    elements <- unlist(strsplit(preset$corners, "+", fixed = TRUE))
  }
  setdiff(unique(trimws(elements)), ignore_elements)
}
