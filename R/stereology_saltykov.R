# ---- Stereology: from sections on a polished surface to particles in the volume ----
# A polished section cuts each particle at a random height, so the circles
# seen are on average smaller than the particles, and large particles are
# cut more often than small ones. These functions correct for both, under
# the assumption that the particles are spheres and that every section
# above the smallest measured size was detected.
#
#   Delesse:  volume fraction V_V = area fraction A_A (no shape assumption).
#   Fullman (1953): N_V = (2 / pi) * N_A * mean(1 / d), mean sphere
#       diameter = pi / (2 * mean(1 / d)).
#   Schwartz-Saltykov: sections sorted into k classes of width delta; a
#       sphere of diameter j * delta gives a section in class i with
#       probability (sqrt(j^2 - (i-1)^2) - sqrt(j^2 - i^2)) / j, so
#       N_A(i) = sum_j N_V(j) * delta * (sqrt(j^2 - (i-1)^2) - sqrt(j^2 - i^2))
#       and the triangular system is solved from the largest class down.
#       The coefficients are computed here, not copied from a table.

#' Number of particles per volume and their mean diameter (Fullman)
#'
#' @param d_um Section diameters (equivalent circle diameters, um).
#' @param inspected_area_mm2 Total inspected area.
#' @return A list: `n`, `per_mm2` (N_A), `per_mm3` (N_V), `mean_section_um`,
#'   `mean_sphere_um` (mean 3D diameter), `area_fraction` (= volume
#'   fraction, Delesse), `volume_ppm`.
#' @export
fullman_number_density <- function(d_um, inspected_area_mm2) {
  d_um <- d_um[is.finite(d_um) & d_um > 0]
  n <- length(d_um)
  if (!is.finite(inspected_area_mm2) || inspected_area_mm2 <= 0) stop("The inspected area must be greater than zero.", call. = FALSE)
  n_a <- n / inspected_area_mm2
  inv <- if (n) mean(1 / d_um) else NA_real_
  area_fraction <- sum(pi * d_um^2 / 4) / (inspected_area_mm2 * 1e6)
  list(
    n = n,
    per_mm2 = n_a,
    per_mm3 = if (n) (2 / pi) * n_a * inv * 1000 else 0,
    mean_section_um = if (n) mean(d_um) else NA_real_,
    mean_sphere_um = if (n) pi / (2 * inv) else NA_real_,
    area_fraction = area_fraction,
    volume_ppm = area_fraction * 1e6
  )
}

# Coefficient matrix of the Schwartz-Saltykov system, in units of delta.
.saltykov_matrix <- function(k) {
  m <- matrix(0, k, k)
  for (j in seq_len(k)) {
    i <- seq_len(j)
    m[i, j] <- sqrt(j^2 - (i - 1)^2) - sqrt(j^2 - i^2)
  }
  m
}

#' Size distribution of spheres from their sections (Schwartz-Saltykov)
#'
#' Sorts the section diameters into `n_classes` classes of equal width up
#' to `d_max` and solves for the number of spheres per mm3 in each class.
#' Sampling noise and non-spherical particles can make a class negative;
#' such values are reported as found (`per_mm3`) and set to zero in
#' `per_mm3_clipped`.
#'
#' Sections smaller than the detection limit are missing from the data, so
#' the smallest classes are underestimated - read them with caution.
#'
#' @param d_um Section diameters (equivalent circle diameters, um).
#' @param inspected_area_mm2 Total inspected area.
#' @param n_classes Number of size classes. Default 10.
#' @param d_max Upper limit of the largest class (um); default the largest
#'   section.
#' @return A data frame with one row per class: `class`, `d_lower`,
#'   `d_upper` (um; spheres of the class are taken to have diameter
#'   `d_upper`), `n_sections`, `per_mm2`, `per_mm3`, `per_mm3_clipped`.
#' @export
saltykov_unfold <- function(d_um, inspected_area_mm2, n_classes = 10, d_max = NULL) {
  d_um <- d_um[is.finite(d_um) & d_um > 0]
  if (length(d_um) == 0) stop("No valid section diameters.", call. = FALSE)
  if (!is.finite(inspected_area_mm2) || inspected_area_mm2 <= 0) stop("The inspected area must be greater than zero.", call. = FALSE)
  k <- as.integer(n_classes)
  if (!is.finite(k) || k < 2) stop("At least 2 size classes are needed.", call. = FALSE)
  if (is.null(d_max)) d_max <- max(d_um)
  if (!is.finite(d_max) || d_max <= 0) stop("The largest diameter must be greater than zero.", call. = FALSE)

  delta <- d_max / k
  cls <- pmin(pmax(ceiling(d_um / delta - 1e-9), 1), k)
  n_sections <- tabulate(cls[d_um <= d_max * (1 + 1e-9)], nbins = k)
  n_a <- n_sections / inspected_area_mm2
  # delta in mm, so N_V comes out per mm3.
  n_v <- backsolve(.saltykov_matrix(k) * delta / 1000, n_a)

  data.frame(class = seq_len(k), d_lower = (seq_len(k) - 1) * delta, d_upper = seq_len(k) * delta,
             n_sections = n_sections, per_mm2 = n_a, per_mm3 = n_v, per_mm3_clipped = pmax(n_v, 0))
}

#' Plot the unfolded size distribution
#'
#' @param unfolded From [saltykov_unfold()].
#' @return A `ggplot` object: spheres per mm3 against sphere diameter.
#' @export
create_saltykov_plot <- function(unfolded) {
  unfolded$mid <- (unfolded$d_lower + unfolded$d_upper) / 2
  width <- unfolded$d_upper[1] - unfolded$d_lower[1]
  ggplot2::ggplot(unfolded, ggplot2::aes(x = mid, y = per_mm3_clipped)) +
    ggplot2::geom_col(width = width * 0.92, fill = "#357ABD") +
    ggplot2::labs(x = "Sphere diameter (um)", y = expression("Particles per " * mm^3),
                  title = "Size distribution in the volume (Schwartz-Saltykov)",
                  subtitle = "Spheres assumed; classes below the detection limit are underestimated") +
    ggplot2::theme_minimal()
}
