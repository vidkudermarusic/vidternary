# ---- Import: which column of an export is what ----
# Every automated SEM system names its columns differently ("Area (sq.
# um)", "Feature Area", "Dmax", "Max Feret", "StageX" ...). The rest of the
# app only needs to know which column is the area, the length, the
# position and so on - the "roles" below. detect_column_roles() guesses
# them from the column names; the user confirms or corrects the guess in
# the Data Import tab, and can save it as a profile for the next file of
# the same instrument.
#
# Only the Oxford AZtec Feature export has been checked against real files
# in this project. The name patterns for other systems are educated
# guesses from common usage and must be confirmed on the first file.

#' The column roles a particle table can have
#'
#' @return A data frame: `role`, `label` (shown in the app), `canonical`
#'   (column name after standardisation), `kind` (`"id"`, `"length"`,
#'   `"area"`, `"position"`, `"angle"`, `"ratio"`), `pattern` (regular
#'   expression matched against the lower-case column name with dots and
#'   underscores turned into spaces).
#' @export
particle_roles <- function() {
  data.frame(
    role = c("feature", "field", "area", "ecd", "length", "breadth", "aspect", "direction", "perimeter", "x", "y"),
    label = c("Particle ID", "SEM field ID", "Area", "Equivalent circle diameter", "Length (longest dimension)",
              "Breadth (shortest dimension)", "Aspect ratio", "Direction of the long axis", "Perimeter",
              "X position", "Y position"),
    canonical = c("Feature", "Field", "Area.(um2)", "ECD.(um)", "Length.(um)", "Breadth.(um)", "Aspect.Ratio",
                  "Direction.(degrees)", "Perimeter.(um)", "X.(mm)", "Y.(mm)"),
    kind = c("id", "id", "area", "length", "length", "length", "ratio", "angle", "length", "position", "position"),
    pattern = c(
      "^(feature|particle|part|object|inclusion)( ?(id|no|nr|number|#))?$|^(id|no|nr|#)$",
      "field|frame|fov|^image( ?(id|no|nr|number))?$",
      "^area(?! ?(fraction|%))|(feature|particle|projected) area",
      "^ecd|equivalent ?(circle|circular|circ)|^d ?(eq|circle|ave|mean)\\b|^dave|heywood|^diameter",
      "^length|max(imum)? ?feret|feret ?max|^d ?max|longest|major ?axis",
      "^breadth|^width|min(imum)? ?feret|feret ?min|^d ?min|^d ?perp|shortest|minor ?axis",
      "aspect",
      "direction|orientation|^angle",
      "perimeter",
      "stage ?x|^x ?(stage|abs|pos|position|coord|coordinate|centroid|centre|center)|centroid ?x|^x ?cg|^x( |$)",
      "stage ?y|^y ?(stage|abs|pos|position|coord|coordinate|centroid|centre|center)|centroid ?y|^y ?cg|^y( |$)"
    ),
    stringsAsFactors = FALSE
  )
}

# Column name as matched: lower case, dots / underscores as spaces.
.normalise_name <- function(x) trimws(gsub("\\s+", " ", gsub("[._]", " ", tolower(x))))

#' Guess which column plays which role
#'
#' Each role takes the first column whose name matches its pattern and that
#' no earlier role has taken. Roles other than the two IDs only consider
#' numeric columns, so a text column named "Area" (the scan-area label of
#' some exports) is not mistaken for the particle area.
#'
#' @param data A data frame.
#' @return A named character vector role -> column name (`NA` where no
#'   column was found).
#' @export
detect_column_roles <- function(data) {
  roles <- particle_roles()
  cols <- names(data)
  norm <- .normalise_name(cols)
  numeric_col <- vapply(data, is.numeric, logical(1))
  out <- stats::setNames(rep(NA_character_, nrow(roles)), roles$role)
  taken <- rep(FALSE, length(cols))
  # Stage coordinates before the rest, so "Beam X (pixels)" is not taken
  # for the position when a stage position exists.
  for (i in order(!roles$role %in% c("x", "y"))) {
    candidate <- !taken & grepl(roles$pattern[i], norm, perl = TRUE)
    if (roles$kind[i] != "id") candidate <- candidate & numeric_col
    if (roles$role[i] %in% c("x", "y")) {
      stage <- candidate & grepl("stage", norm)
      if (any(stage)) candidate <- stage
    }
    if (any(candidate)) {
      hit <- which(candidate)[1]
      out[roles$role[i]] <- cols[hit]
      taken[hit] <- TRUE
    }
  }
  out
}

#' Unit named in a column name
#'
#' @param col_name A column name such as `"Length (um)"` or `"Stage.X.(mm)"`.
#' @return `"nm"`, `"um"`, `"mm"`, `"pixels"`, or `NA` when no unit is named.
#'   For an area column the unit of length is returned (`"um"` for um2).
#' @export
detect_unit <- function(col_name) {
  if (is.null(col_name) || is.na(col_name)) return(NA_character_)
  n <- .normalise_name(col_name)
  n <- gsub("\u00b5|\u03bc", "u", n)
  if (grepl("pixel|\\bpx\\b", n)) return("pixels")
  if (grepl("\\bnm\\b|nanomet", n)) return("nm")
  if (grepl("\\bum\\b|um2|um\\^2|micron|micromet|sq ?um", n)) return("um")
  if (grepl("\\bmm\\b|mm2|mm\\^2|millimet", n)) return("mm")
  NA_character_
}

# Factor that turns a value in `unit` into um.
.unit_to_um <- function(unit) {
  switch(unit, nm = 1e-3, um = 1, mm = 1e3, stop("Unknown unit '", unit, "'. Use nm, um or mm.", call. = FALSE))
}

#' Find the element columns of an export and their basis
#'
#' More forgiving than [inclusion_element_columns()]: a column counts as an
#' element when its name is an element symbol alone, or a symbol with a
#' marker for weight or atomic percent or an X-ray line (`"Al (Wt%)"`,
#' `"Al wt.%"`, `"Al At%"`, `"Al K"`, `"Wt% Al"`). Columns already used for
#' another role are skipped, so a bare `"Y"` that is the Y position is not
#' read as yttrium.
#'
#' @param data A data frame.
#' @param exclude Column names that already have another role.
#' @param weights Atomic weights; only symbols in it are recognised.
#' @return A data frame: `element`, `column`, `basis` (`"wt"`, `"at"` or
#'   `"unknown"`), one row per element (the first matching column).
#' @export
detect_element_columns <- function(data, exclude = character(0), weights = import_atomic_weights()) {
  cols <- setdiff(names(data)[vapply(data, is.numeric, logical(1))], exclude)
  symbols <- names(weights)
  rows <- lapply(cols, function(cn) {
    n <- trimws(gsub("\\s+", " ", gsub("[._()\\[\\]]", " ", cn, perl = TRUE)))
    tokens <- strsplit(n, " ", fixed = TRUE)[[1]]
    tokens <- tokens[nzchar(tokens)]
    if (length(tokens) == 0) return(NULL)
    # The symbol comes first ("Al Wt%", "Al K") or last ("Wt% Al"). Taking
    # it by position keeps the K of "Al K" (an X-ray line) from being read
    # as potassium.
    at <- if (tokens[1] %in% symbols) 1 else if (tokens[length(tokens)] %in% symbols) length(tokens) else return(NULL)
    rest <- tolower(paste(tokens[-at], collapse = " "))
    if (grepl("sigma|error|\\berr\\b|\\bstd\\b|uncert", rest)) return(NULL)
    basis <- if (grepl("(^|[^a-z])at(om(ic)?)?([^a-z]|$)", rest)) "at"
             else if (grepl("wt|weight|mass", rest)) "wt"
             else if (!nzchar(rest) || grepl("^[klm]( ?(a|b|alpha|beta))?$|^%$|^conc", rest)) "unknown"
             else return(NULL)
    data.frame(element = tokens[at], column = cn, basis = basis, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) return(data.frame(element = character(0), column = character(0), basis = character(0), stringsAsFactors = FALSE))
  out[!duplicated(out$element), , drop = FALSE]
}

#' Atomic weights for converting atomic percent to weight percent
#'
#' The weights of [inclusion_atomic_weights()] plus further elements that
#' turn up in EDS exports. They are used only for the conversion in the
#' Data Import tab; the inclusion presets keep their own shorter list.
#' Source: CIAAW standard atomic weights (conventional values).
#'
#' @return A named numeric vector, element symbol -> g/mol.
#' @export
import_atomic_weights <- function() {
  extra <- c(B = 10.81, F = 18.998, Na = 22.990, P = 30.974, Cl = 35.45, K = 39.098, Zn = 65.38,
             As = 74.922, Se = 78.971, Y = 88.906, Ag = 107.868, Sb = 121.760, Te = 127.60, Ba = 137.327,
             La = 138.905, Ce = 140.116, Nd = 144.242, Hf = 178.49, Ta = 180.948)
  w <- c(inclusion_atomic_weights(), extra)
  w[!duplicated(names(w))]
}
