# ---- EDS reliability: is the particle large enough for its analysis? ----
# The electron beam spreads in the specimen. A spectrum taken on a particle
# smaller than that spread is mostly steel matrix, and its "composition"
# says little about the particle. The spread is estimated with the
# electron range of Kanaya & Okayama (1972):
#
#   R (um) = 0.0276 * A * E0^1.67 / (Z^0.89 * rho)
#
# with A the atomic weight (g/mol), Z the atomic number, rho the density
# (g/cm3) and E0 the beam energy (keV). For iron at 15 kV this gives about
# 1 um. The range is computed for the matrix; inside a lighter inclusion
# (oxide, sulfide) the beam spreads further, so the value is a lower bound.
# The formula was entered from memory of the standard text (Goldstein et
# al., Scanning Electron Microscopy and X-Ray Microanalysis) and not
# compared with a printed copy in this project.
#
# Particles are sorted into three classes by their equivalent circle
# diameter relative to that range. The limits (1x and 2x the range) are a
# convention of this app, not a standard.

# Atomic number and density (g/cm3) of the matrix metals offered.
.range_matrix <- data.frame(
  element = c("Fe", "Ni", "Co", "Cu", "Ti", "Al"),
  z = c(26, 28, 27, 29, 22, 13),
  density = c(7.87, 8.90, 8.86, 8.96, 4.51, 2.70),
  stringsAsFactors = FALSE
)

#' Electron range in the matrix (Kanaya-Okayama)
#'
#' @param kv Accelerating voltage in kV.
#' @param matrix Matrix metal: `"Fe"`, `"Ni"`, `"Co"`, `"Cu"`, `"Ti"` or
#'   `"Al"`.
#' @return The electron range in um.
#' @export
electron_range_um <- function(kv = 15, matrix = "Fe") {
  if (length(kv) != 1 || !is.finite(kv) || kv <= 0) stop("The accelerating voltage must be greater than zero.", call. = FALSE)
  m <- .range_matrix[.range_matrix$element == matrix, ]
  if (nrow(m) == 0) {
    stop("No range data for matrix '", matrix, "'. Use one of: ", paste(.range_matrix$element, collapse = ", "), ".", call. = FALSE)
  }
  0.0276 * inclusion_atomic_weights()[[matrix]] * kv^1.67 / (m$z^0.89 * m$density)
}

# Reliability classes, from worst to best.
.reliability_levels <- c("matrix-dominated", "partly matrix", "reliable")

#' Sort particles by whether their size allows a reliable analysis
#'
#' * **matrix-dominated**: smaller than the electron range - the spectrum
#'   is mostly matrix.
#' * **partly matrix**: up to `caution_factor` times the range - the
#'   elements present are right, their amounts are diluted by matrix.
#' * **reliable**: larger than that.
#'
#' @param ecd_um Equivalent circle diameters (um).
#' @param kv,matrix See [electron_range_um()].
#' @param caution_factor Multiple of the range from which a particle counts
#'   as reliable. Default 2.
#' @return A data frame: `size_to_range` (diameter divided by the electron
#'   range) and `reliability` (factor with the three levels above); the
#'   range is in the `range_um` attribute.
#' @export
size_reliability <- function(ecd_um, kv = 15, matrix = "Fe", caution_factor = 2) {
  if (length(caution_factor) != 1 || !is.finite(caution_factor) || caution_factor < 1) {
    stop("The caution factor must be 1 or more.", call. = FALSE)
  }
  range_um <- electron_range_um(kv, matrix)
  ratio <- ecd_um / range_um
  class <- ifelse(is.na(ratio), NA_character_,
           ifelse(ratio < 1, .reliability_levels[1], ifelse(ratio < caution_factor, .reliability_levels[2], .reliability_levels[3])))
  out <- data.frame(size_to_range = ratio, reliability = factor(class, levels = .reliability_levels))
  attr(out, "range_um") <- range_um
  out
}

#' How many particles fall in each reliability class
#'
#' With `matrix_pct` the table also shows the median matrix content per
#' class - the check on real data that small particles are indeed read as
#' matrix.
#'
#' @param reliability The `reliability` column of [size_reliability()].
#' @param matrix_pct Optional matrix-element content of each particle (wt%).
#' @return A data frame: `reliability`, `n`, `share_pct`, `median_matrix_pct`
#'   (`NA` without `matrix_pct`).
#' @export
reliability_summary <- function(reliability, matrix_pct = NULL) {
  reliability <- factor(as.character(reliability), levels = .reliability_levels)
  n <- as.integer(table(reliability))
  median_matrix <- if (is.null(matrix_pct)) rep(NA_real_, 3) else {
    vapply(.reliability_levels, function(l) {
      v <- suppressWarnings(as.numeric(matrix_pct))[!is.na(reliability) & reliability == l]
      if (any(is.finite(v))) stats::median(v, na.rm = TRUE) else NA_real_
    }, numeric(1))
  }
  data.frame(reliability = .reliability_levels, n = n, share_pct = if (sum(n)) 100 * n / sum(n) else rep(0, 3),
             median_matrix_pct = unname(median_matrix), stringsAsFactors = FALSE)
}

#' Add the size-reliability columns to a particle table
#'
#' Adds `Size.to.range` (number, usable in the pre-analysis filters, e.g.
#' `> 1`) and `Composition.reliability` (text, usable as a colour or group
#' column). The diameter is taken from `ECD.(um)` or computed from
#' `Area.(um2)` of a standard table.
#'
#' @param data A particle table in the standard shape
#'   ([standardize_particle_table()]).
#' @param kv,matrix,caution_factor See [size_reliability()].
#' @return `data` with the two columns added (replaced if present).
#' @export
add_size_reliability <- function(data, kv = 15, matrix = "Fe", caution_factor = 2) {
  ecd <- if ("ECD.(um)" %in% names(data)) suppressWarnings(as.numeric(data[["ECD.(um)"]]))
         else if ("Area.(um2)" %in% names(data)) 2 * sqrt(suppressWarnings(as.numeric(data[["Area.(um2)"]])) / pi)
         else stop("The table has neither an 'ECD.(um)' nor an 'Area.(um2)' column, so the particle size is not known.", call. = FALSE)
  r <- size_reliability(ecd, kv, matrix, caution_factor)
  data[["Size.to.range"]] <- r$size_to_range
  data[["Composition.reliability"]] <- as.character(r$reliability)
  data
}
