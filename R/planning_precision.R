# ---- Measurement planning: how much area has to be inspected? ----
# The number density of inclusions is a count over an area, so its
# uncertainty is set by how many particles were counted - and by how much
# they cluster. With N particles on the area A and counts per SEM field
# that vary D times more than random scatter (the index of dispersion, see
# field_count_dispersion()),
#
#   relative standard error of N / A  =  sqrt(D / N)  =  sqrt(D / (density * A)).
#
# Everything here follows from that one relation and the normal
# approximation, which holds from a few dozen particles on. D is a property
# of the field size it was measured at: the formulas assume that a larger
# measurement means more fields of the same size, not larger fields.

.check_planning_inputs <- function(density_per_mm2, dispersion) {
  if (any(!is.finite(density_per_mm2) | density_per_mm2 <= 0)) {
    stop("The number density must be greater than zero.", call. = FALSE)
  }
  if (any(!is.finite(dispersion) | dispersion <= 0)) {
    stop("The index of dispersion must be greater than zero (1 = randomly scattered particles).", call. = FALSE)
  }
  invisible(TRUE)
}

#' Precision of a number density measured on a given area
#'
#' Half-width of the confidence interval of particles per mm2, relative to
#' the density itself (0.1 = plus or minus 10 %).
#'
#' @param area_mm2 Inspected area (mm2).
#' @param density_per_mm2 Number density expected (from a pilot run).
#' @param dispersion Index of dispersion of the counts per SEM field
#'   ([field_count_dispersion()]); 1 for randomly scattered particles.
#' @param level Confidence level. Default 0.95.
#' @return Relative half-width(s) of the interval.
#' @export
density_relative_halfwidth <- function(area_mm2, density_per_mm2, dispersion = 1, level = 0.95) {
  .check_planning_inputs(density_per_mm2, dispersion)
  if (any(!is.finite(area_mm2) | area_mm2 <= 0)) stop("The area must be greater than zero.", call. = FALSE)
  stats::qnorm((1 + level) / 2) * sqrt(dispersion / (density_per_mm2 * area_mm2))
}

#' Area needed to know the number density to a given precision
#'
#' @param density_per_mm2 Number density expected (from a pilot run).
#' @param rel_halfwidth Wanted half-width of the interval relative to the
#'   density (0.1 = plus or minus 10 %).
#' @param dispersion Index of dispersion of the counts per SEM field.
#' @param level Confidence level. Default 0.95.
#' @return Area(s) in mm2.
#' @export
area_for_density_precision <- function(density_per_mm2, rel_halfwidth, dispersion = 1, level = 0.95) {
  .check_planning_inputs(density_per_mm2, dispersion)
  if (any(!is.finite(rel_halfwidth) | rel_halfwidth <= 0)) {
    stop("The wanted precision must be greater than zero.", call. = FALSE)
  }
  stats::qnorm((1 + level) / 2)^2 * dispersion / (density_per_mm2 * rel_halfwidth^2)
}

#' Area needed per batch to detect a difference in number density
#'
#' Two batches are inspected on the same area each and their densities
#' compared on the log scale. Returns the area per batch at which a true
#' ratio of `ratio` is found with probability `power` in a two-sided test
#' at level `alpha`.
#'
#' @param density_per_mm2 Number density of the reference batch.
#' @param ratio Ratio of the densities to detect (1.2 = 20 % more, 0.8 =
#'   20 % fewer); not 1.
#' @param dispersion Index of dispersion of the counts per SEM field.
#' @param power Probability of detecting the difference. Default 0.8.
#' @param alpha Significance level of the two-sided test. Default 0.05.
#' @return Area(s) per batch in mm2.
#' @export
area_for_density_ratio <- function(density_per_mm2, ratio, dispersion = 1, power = 0.8, alpha = 0.05) {
  .check_planning_inputs(density_per_mm2, dispersion)
  if (any(!is.finite(ratio) | ratio <= 0 | ratio == 1)) {
    stop("The ratio to detect must be a positive number other than 1.", call. = FALSE)
  }
  if (!is.finite(power) || power <= alpha || power >= 1) stop("Power must lie between alpha and 1.", call. = FALSE)
  z <- stats::qnorm(1 - alpha / 2) + stats::qnorm(power)
  z^2 * dispersion * (1 + 1 / ratio) / (density_per_mm2 * log(ratio)^2)
}

#' Smallest density ratio an area can detect
#'
#' The inverse of [area_for_density_ratio()]: the ratio above 1 that two
#' batches inspected on `area_mm2` each show with probability `power`.
#'
#' @param area_mm2 Inspected area per batch (mm2).
#' @inheritParams area_for_density_ratio
#' @return The detectable ratio (greater than 1); the same difference
#'   downwards is its reciprocal, to a close approximation.
#' @export
detectable_density_ratio <- function(area_mm2, density_per_mm2, dispersion = 1, power = 0.8, alpha = 0.05) {
  .check_planning_inputs(density_per_mm2, dispersion)
  if (!is.finite(area_mm2) || area_mm2 <= 0) stop("The area must be greater than zero.", call. = FALSE)
  gap <- function(log_ratio) {
    area_for_density_ratio(density_per_mm2, exp(log_ratio), dispersion, power, alpha) - area_mm2
  }
  # The needed area falls steadily as the ratio grows.
  if (gap(log(1e6)) > 0) return(Inf)
  exp(stats::uniroot(gap, c(1e-6, log(1e6)), tol = 1e-10)$root)
}
