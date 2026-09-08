# ---- Point Pattern Analysis (Ripley's K/L, G-function, CSR envelope, kernel intensity) ----
# A second, complementary way of asking "are these inclusion positions
# randomly scattered, clustered, or more evenly spread out than chance
# would predict?" - the existing "Spatial Clustering" tab
# (spatial_clustering_analysis.R) answers this with a single first-nearest-
# neighbour summary (Clark-Evans). This module answers the same underlying
# question with the standard *point process* toolkit instead: Ripley's
# K-function (how many neighbours fall within EVERY distance r, not just
# the nearest one), its variance-stabilized L-transform, the nearest-
# neighbour distance distribution function G(r), a Monte Carlo complete-
# spatial-randomness (CSR) envelope test, and a kernel intensity ("hotspot")
# map. Genuinely separate tab/module (not merged into Spatial Clustering) -
# same convention as EVS/Spatial Clustering/Compositional Analysis each
# being their own file despite all analyzing "is this pattern unusual".
#
# Built directly on the `spatstat` family (spatstat.geom for the point
# pattern/window, spatstat.explore for the summary functions) rather than
# a from-scratch reimplementation like Clark-Evans - unlike that single
# closed-form test, K/L/G/envelope/intensity are exactly what spatstat
# exists to compute, are the standard reference implementations R users
# quote in publications, and reimplementing edge-corrected K/L/G by hand
# would be pure risk for zero benefit (spatstat's own correctness is
# CRAN's most battle-tested spatial-statistics ground truth). Only
# spatstat.geom and spatstat.explore are imported (not the full `spatstat`
# umbrella, which also pulls in spatstat.model/spatstat.linnet/etc that
# nothing here uses).
#
#   Ripley's K-function     K(r) = (expected # of further points within
#                            distance r of a typical point) / intensity
#   CSR expectation          K_CSR(r) = pi * r^2
#   L-function (variance-    L(r) = sqrt(K(r) / pi)          - linearizes
#     stabilizing transform)                                   K so CSR
#                                                                plots as
#                                                                a straight
#                                                                line
#     (Ripley, 1977, <https://doi.org/10.1111/j.2517-6161.1977.tb01615.x>
#      - K-function and, in Besag's published discussion of that same
#      paper, the L-transform.)
#   G-function                empirical CDF of nearest-neighbour distances;
#                              rises faster than the CSR expectation under
#                              clustering, slower under inhibition/regularity.
#   CSR envelope test         nsim independent CSR patterns simulated in the
#                              same observation window; pointwise min/max at
#                              each r gives the confidence band an observed
#                              curve is compared against.
#
# General references: Diggle (2013) <https://doi.org/10.1201/b15326>;
# Baddeley, Rubak & Turner (2015) <https://doi.org/10.1201/b19708> - the
# spatstat authors' own textbook, the standard modern reference for this
# entire toolkit.

# Build a spatstat point pattern (ppp), bounded by the convex hull of the
# data - the standard choice when no separately-known study-region boundary
# exists (there isn't one here: an inclusion's X/Y stage position has no
# natural "window" beyond the sampled points themselves). Marks/colouring
# are handled separately by this file's own plotting function, not carried
# on the ppp object itself - K/L/G/envelope/intensity are all computed from
# point LOCATIONS only, matching how spatial_clustering_analysis.R also
# keeps `color_by` completely out of clark_evans_test() and confined to its
# own plotting function.
#' Build a point pattern (`spatstat.geom::ppp`) bounded by the convex hull of the data
#'
#' @param x Numeric vector of X coordinates.
#' @param y Numeric vector of Y coordinates (same length as `x`).
#' @return A `spatstat.geom::ppp` object, windowed to the convex hull of `(x, y)`.
#' @export
build_point_pattern <- function(x, y) {
  if (!requireNamespace("spatstat.geom", quietly = TRUE)) {
    stop("Package 'spatstat.geom' is required for point pattern analysis.")
  }
  valid <- is.finite(x) & is.finite(y)
  x <- x[valid]; y <- y[valid]
  n <- length(x)
  # A convex hull needs at least 3 non-collinear points to have a non-zero
  # area at all; 4 is required here rather than 3 so the hull isn't just a
  # single degenerate triangle with every point on its own boundary - matches
  # this project's convention of guarding against just-barely-valid inputs
  # rather than the bare theoretical minimum (see clark_evans_test()'s own
  # n >= 3 floor for a similarly-motivated but distinct minimum).
  if (n < 4) stop("At least 4 valid points are required for point pattern analysis.")
  # Fully collinear points (including the all-X-constant or all-Y-constant
  # special cases) can't form a valid 2-D convex hull at all - confirmed
  # directly: convexhull.xy() doesn't return a zero-area owin for this
  # case, it throws its own raw internal assertion ("is.owin(w) is not
  # TRUE") before ever getting that far. Caught here so every degenerate-
  # geometry input gets the same clear, friendly message regardless of
  # which of spatstat's two different failure paths it happens to hit.
  win <- tryCatch(spatstat.geom::convexhull.xy(x, y), error = function(e) NULL)
  if (is.null(win) || !is.finite(spatstat.geom::area.owin(win)) || spatstat.geom::area.owin(win) <= 0) {
    stop("Points must span a non-zero area (X and Y cannot be constant or collinear).")
  }
  spatstat.geom::ppp(x, y, window = win, checkdup = FALSE)
}

# Pull the "recommended" edge-corrected column out of a spatstat `fv`
# (function value) object generically, via spatstat's own fvnames(.,".y")
# rather than a hardcoded column name - Kest()/Lest() default to "iso"
# (isotropic correction) and Gest() defaults to "km" (Kaplan-Meier), and
# which corrections are even available can depend on the window's shape
# (confirmed directly: a convex-hull window still offers the same iso/
# border/trans corrections a rectangular one does, but relying on
# fvnames() rather than assuming that stays true is the robust choice,
# not a shortcut).
.spp_fv_to_df <- function(fv_obj) {
  y_col <- spatstat.explore::fvnames(fv_obj, ".y")
  df <- as.data.frame(fv_obj)
  data.frame(r = df$r, theo = df$theo, obs = df[[y_col]], correction = y_col)
}

#' Ripley's K-function
#'
#' `K(r)`: the expected number of further points within distance `r` of a
#' typical point, divided by the pattern's intensity - compared against the
#' CSR expectation `K(r) = pi * r^2`. Edge-corrected using spatstat's own
#' recommended correction for this point pattern (`spatstat.explore::Kest()`'s
#' default).
#'
#' @param pp A `spatstat.geom::ppp`, e.g. from `build_point_pattern()`.
#' @return A data frame: `r`, `theo` (CSR expectation), `obs` (edge-corrected
#'   observed K(r)), `correction` (which edge-correction column was used).
#' @export
compute_ripley_k <- function(pp) {
  if (!requireNamespace("spatstat.explore", quietly = TRUE)) {
    stop("Package 'spatstat.explore' is required for point pattern analysis.")
  }
  .spp_fv_to_df(spatstat.explore::Kest(pp))
}

#' L-function (variance-stabilizing transform of Ripley's K)
#'
#' `L(r) = sqrt(K(r) / pi)`, so complete spatial randomness plots as a
#' straight line `L(r) = r`. Computed directly via
#' `spatstat.explore::Lest()` (not derived by hand from `compute_ripley_k()`)
#' so the same edge correction is applied consistently within the transform,
#' matching spatstat's own recommended usage.
#'
#' @param pp A `spatstat.geom::ppp`, e.g. from `build_point_pattern()`.
#' @return A data frame: `r`, `theo` (`= r` under CSR), `obs` (edge-corrected
#'   observed L(r)), `obs_minus_r` (`obs - r`, the conventional clustering
#'   diagnostic - positive means more neighbours than CSR predicts at that
#'   distance), `correction`.
#' @export
compute_l_function <- function(pp) {
  if (!requireNamespace("spatstat.explore", quietly = TRUE)) {
    stop("Package 'spatstat.explore' is required for point pattern analysis.")
  }
  df <- .spp_fv_to_df(spatstat.explore::Lest(pp))
  df$obs_minus_r <- df$obs - df$r
  df
}

#' Nearest-neighbour distance distribution function G(r)
#'
#' The empirical CDF of nearest-neighbour distances: `G(r)` = the proportion
#' of points whose nearest neighbour lies within distance `r`. Rises faster
#' than the CSR expectation under clustering, slower under
#' inhibition/regularity. Edge-corrected via `spatstat.explore::Gest()`'s own
#' default (Kaplan-Meier).
#'
#' @param pp A `spatstat.geom::ppp`, e.g. from `build_point_pattern()`.
#' @return A data frame: `r`, `theo` (CSR expectation), `obs` (edge-corrected
#'   observed G(r)), `correction`.
#' @export
compute_g_function <- function(pp) {
  if (!requireNamespace("spatstat.explore", quietly = TRUE)) {
    stop("Package 'spatstat.explore' is required for point pattern analysis.")
  }
  .spp_fv_to_df(spatstat.explore::Gest(pp))
}

# CSR envelope test, run on the L-function specifically (rather than adding
# a separate function-choice input the user's own spec doesn't list): the
# spec's own Interpretation text ties these two together directly - "L(r) -
# r ... is the conventional clustering diagnostic. Envelope bands that
# exclude the observed curve indicate statistically significant departure
# from CSR" - describing one combined diagnostic, not three independent
# envelope computations. `nsim` random CSR patterns are simulated inside the
# SAME observation window (spatstat's own default `envelope()` behaviour),
# giving pointwise (not global) confidence bands - matching the spec's own
# wording, "pointwise confidence bands".
#' Monte Carlo CSR envelope test on the L-function
#'
#' Simulates `nsim` independent complete-spatial-randomness (CSR) point
#' patterns in the observed pattern's own window, and reports the pointwise
#' min/max of their L(r) curves as a confidence envelope around the
#' theoretical CSR line - the observed L(r) falling outside that envelope at
#' a given `r` is evidence of a real departure from CSR at that distance.
#'
#' @param pp A `spatstat.geom::ppp`, e.g. from `build_point_pattern()`.
#' @param nsim Number of Monte Carlo CSR simulations. Default 99 (a
#'   conventional choice - gives pointwise bands at roughly the 2/(nsim+1)
#'   two-sided significance level).
#' @param seed RNG seed for the simulations; the caller's RNG state is saved
#'   and restored afterward, matching `clark_evans_test()`'s own convention.
#'   Default 42.
#' @return A data frame: `r`, `obs` (observed L(r)), `theo` (`= r` under
#'   CSR), `lo`/`hi` (pointwise envelope bounds), `obs_minus_r`, `lo_minus_r`,
#'   `hi_minus_r` (the same three shifted by `-r`, for plotting the
#'   conventional `L(r) - r` diagnostic with its own envelope).
#' @export
compute_csr_envelope <- function(pp, nsim = 99, seed = 42) {
  if (!requireNamespace("spatstat.explore", quietly = TRUE)) {
    stop("Package 'spatstat.explore' is required for point pattern analysis.")
  }
  if (!is.numeric(nsim) || length(nsim) != 1L || is.na(nsim) || nsim < 1 || nsim != round(nsim)) {
    stop("nsim must be a single positive whole number.")
  }
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv)) .GlobalEnv$.Random.seed else NULL
  on.exit(if (!is.null(old_seed)) assign(".Random.seed", old_seed, envir = .GlobalEnv), add = TRUE)
  set.seed(seed)
  env <- spatstat.explore::envelope(pp, spatstat.explore::Lest, nsim = nsim, verbose = FALSE, savefuns = FALSE)
  df <- as.data.frame(env)
  df$obs_minus_r <- df$obs - df$r
  df$lo_minus_r  <- df$lo  - df$r
  df$hi_minus_r  <- df$hi  - df$r
  df
}

#' Kernel intensity ("hotspot") estimate
#'
#' A smoothed estimate of point density across the observation window, via
#' `spatstat.explore::density.ppp()` with its own automatic bandwidth
#' selection (no `sigma` override - lets spatstat pick a data-driven default
#' rather than guessing one).
#'
#' @param pp A `spatstat.geom::ppp`, e.g. from `build_point_pattern()`.
#' @return A data frame with one row per grid cell: `x`, `y`, `intensity`
#'   (estimated points per unit area at that location).
#' @export
compute_kernel_intensity <- function(pp) {
  if (!requireNamespace("spatstat.explore", quietly = TRUE)) {
    stop("Package 'spatstat.explore' is required for point pattern analysis.")
  }
  dens <- spatstat.explore::density.ppp(pp)
  df <- as.data.frame(dens)
  names(df)[names(df) == "value"] <- "intensity"
  df[is.finite(df$intensity), , drop = FALSE]
}

#' Point pattern scatter plot, optionally coloured by a mark
#'
#' @param x Numeric vector of X coordinates.
#' @param y Numeric vector of Y coordinates.
#' @param mark_by Optional vector (same length as `x`/`y`) to colour points
#'   by; continuous columns use a viridis scale, matching
#'   `create_spatial_scatter_plot()`'s own convention.
#' @param mark_label Legend title for `mark_by`. Default `"Mark"`.
#' @return A `ggplot` object.
#' @export
create_point_pattern_plot <- function(x, y, mark_by = NULL, mark_label = "Mark") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 package is required for plotting")
  df <- data.frame(x = x, y = y)
  if (!is.null(mark_by)) {
    df$mark_by <- mark_by
    p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, color = mark_by)) +
      ggplot2::geom_point(alpha = 0.7, size = 1.1) +
      ggplot2::labs(color = mark_label)
    if (is.numeric(mark_by)) p <- p + ggplot2::scale_color_viridis_c()
  } else {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point(alpha = 0.7, size = 1.1, color = "#357ABD")
  }
  p + ggplot2::coord_fixed() +
    ggplot2::labs(title = "Point Pattern", x = "X", y = "Y") +
    ggplot2::theme_minimal()
}

#' Plot Ripley's K-function against its CSR expectation
#'
#' @param k_result A result from `compute_ripley_k()`.
#' @return A `ggplot` object.
#' @export
create_ripley_k_plot <- function(k_result) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 package is required for plotting")
  ggplot2::ggplot(k_result, ggplot2::aes(x = r)) +
    ggplot2::geom_line(ggplot2::aes(y = theo, linetype = "CSR expectation"), color = "#d32f2f", linewidth = 0.9) +
    ggplot2::geom_line(ggplot2::aes(y = obs, linetype = "Observed"), color = "#002147", linewidth = 0.9) +
    ggplot2::scale_linetype_manual(name = NULL, values = c("CSR expectation" = "dashed", "Observed" = "solid")) +
    ggplot2::labs(title = "Ripley's K-function",
                  subtitle = "Above the CSR line: clustering at that distance. Below: regularity.",
                  x = "r (distance)", y = "K(r)") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
}

#' Plot the L-function as L(r) - r (the conventional clustering diagnostic)
#'
#' @param l_result A result from `compute_l_function()`.
#' @return A `ggplot` object.
#' @export
create_l_function_plot <- function(l_result) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 package is required for plotting")
  ggplot2::ggplot(l_result, ggplot2::aes(x = r, y = obs_minus_r)) +
    ggplot2::geom_hline(yintercept = 0, color = "#d32f2f", linetype = "dashed", linewidth = 0.9) +
    ggplot2::geom_line(color = "#002147", linewidth = 0.9) +
    ggplot2::labs(title = "L-function (L(r) - r)",
                  subtitle = "Above zero: clustering at that distance. Below zero: regularity.",
                  x = "r (distance)", y = "L(r) - r") +
    ggplot2::theme_minimal()
}

#' Plot the nearest-neighbour distance distribution G(r) against its CSR expectation
#'
#' @param g_result A result from `compute_g_function()`.
#' @return A `ggplot` object.
#' @export
create_g_function_plot <- function(g_result) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 package is required for plotting")
  ggplot2::ggplot(g_result, ggplot2::aes(x = r)) +
    ggplot2::geom_line(ggplot2::aes(y = theo, linetype = "CSR expectation"), color = "#d32f2f", linewidth = 0.9) +
    ggplot2::geom_line(ggplot2::aes(y = obs, linetype = "Observed"), color = "#002147", linewidth = 0.9) +
    ggplot2::scale_linetype_manual(name = NULL, values = c("CSR expectation" = "dashed", "Observed" = "solid")) +
    ggplot2::labs(title = "Nearest-Neighbour Distance Distribution G(r)",
                  subtitle = "Rises faster than CSR under clustering, slower under regularity.",
                  x = "r (distance)", y = "G(r)") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
}

#' Plot the CSR envelope test on L(r) - r, with the pointwise confidence band shaded
#'
#' @param env_result A result from `compute_csr_envelope()`.
#' @return A `ggplot` object.
#' @export
create_csr_envelope_plot <- function(env_result) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 package is required for plotting")
  ggplot2::ggplot(env_result, ggplot2::aes(x = r)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lo_minus_r, ymax = hi_minus_r), fill = "#357ABD", alpha = 0.25) +
    ggplot2::geom_hline(yintercept = 0, color = "#d32f2f", linetype = "dashed", linewidth = 0.9) +
    ggplot2::geom_line(ggplot2::aes(y = obs_minus_r), color = "#002147", linewidth = 0.9) +
    ggplot2::labs(title = "CSR Envelope Test (L(r) - r)",
                  subtitle = "Shaded band: pointwise range across simulated random patterns. Observed line leaving the band is significant departure from CSR.",
                  x = "r (distance)", y = "L(r) - r") +
    ggplot2::theme_minimal()
}

#' Plot the kernel intensity ("hotspot") map
#'
#' @param intensity_result A result from `compute_kernel_intensity()`.
#' @return A `ggplot` object.
#' @export
create_kernel_intensity_plot <- function(intensity_result) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 package is required for plotting")
  ggplot2::ggplot(intensity_result, ggplot2::aes(x = x, y = y, fill = intensity)) +
    ggplot2::geom_raster() +
    # "points per unit area", not "points" - a LOCAL density, not a
    # running count, spelled out here (not just "Intensity") since users
    # have directly asked why a hotspot's colour-scale value can exceed
    # their total point count: it can, legitimately, in a tight cluster -
    # this map integrates back to the true total over the whole window,
    # but any one small area within a cluster can carry far more than its
    # "fair share" of that total per unit area.
    ggplot2::scale_fill_viridis_c(name = "Local intensity\n(points per\nunit area)") +
    ggplot2::coord_fixed() +
    ggplot2::labs(title = "Kernel Intensity Estimate",
                  subtitle = "A local density (points/area), not a point count - can exceed n in tight clusters.",
                  x = "X", y = "Y") +
    ggplot2::theme_minimal()
}
