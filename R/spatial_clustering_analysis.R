# ---- Spatial Clustering Analysis (Clark-Evans nearest-neighbour test) ----
# Inclusion size alone doesn't capture fatigue risk: literature is explicit
# that spacing/clustering of inclusions (microstructural banding) has a
# synergistic effect on crack initiation, and standard cleanliness methods
# (including this app's own EVS tab) don't measure it. This module answers
# "are these inclusions randomly scattered, clustered together, or more
# evenly spread out than chance would predict?" from their X/Y positions
# (typically Stage X/Y in mm).
#
# Formulas verified against spatstat.explore's own source (clarkevans.R).
# Uses Donnelly's (1978) edge correction for a rectangular window, which
# is spatstat's own *default* for the asymptotic test on a rectangle -
# not an optional extra. Reason: points near the boundary of a finite
# window systematically appear to have farther nearest neighbours than
# they really do (their true nearest neighbour may lie just outside the
# sampled window), inflating Dobs. The naive (uncorrected) formula was
# tried first and checked empirically the same way the EVS goodness-of-fit
# test was calibrated: 500 simulated CSR (uniform random) point sets
# incorrectly flagged "significantly dispersed" 24% of the time instead of
# the nominal 5% (119/120 false rejections were all in the "dispersed"
# direction, matching the known bias direction). Donnelly's correction
# fixes this - re-run against the same simulation, see the verification
# notes for this file.
#
#   intensity (density)      lambda = n / area
#   naive expected NND       Dpois = 1 / (2 * sqrt(lambda))
#   perimeter                perim = 2 * (width + height) of the bounding box
#   Donnelly-corrected mean  Dkevin = Dpois + (0.0514 + 0.0412/sqrt(n)) * perim / n
#   R statistic              R = Dobs / Dkevin   (Dobs = observed mean NND)
#   SE (naive, reused)       SE(Dobs) = sqrt((4-pi)*area / (4*pi)) / n
#   SE(R)                    SE(Dobs) / Dkevin
#   Z = (R - 1) / SE(R), two-sided p-value = 2*(1 - pnorm(abs(Z)))

# Pure Euclidean nearest-neighbour distance per point (excluding self).
#'
#' Two methods, both exact (verified to agree to floating-point precision -
#' `"kdtree"` is not an approximation): `"matrix"` builds the full n x n
#' pairwise distance matrix (`O(n^2)` time *and* memory - at n=16705 that's
#' a ~2.2GB matrix, rebuilt on every Monte Carlo simulation in
#' `clark_evans_test()`, which is minutes-to-tens-of-minutes slow).
#' `"kdtree"` uses `RANN::nn2()` (`O(n log n)`), ~150-250x faster at a few
#' thousand points and the only practical option much above that - kept
#' as an option alongside `"matrix"` rather than replacing it outright.
#' @param x Numeric vector of X coordinates.
#' @param y Numeric vector of Y coordinates (same length as `x`). Requires
#'   at least 2 points.
#' @param method `"kdtree"` (default; fast, `RANN::nn2()`) or `"matrix"`
#'   (slow for large `n`, full distance matrix via `stats::dist()`).
#' @return A numeric vector (same length as `x`), each point's distance to
#'   its nearest other point.
#' @export
compute_nearest_neighbor_distances <- function(x, y, method = c("kdtree", "matrix")) {
  method <- match.arg(method)
  n <- length(x)
  if (n < 2) stop("At least 2 points are required to compute nearest-neighbour distances.")
  if (method == "kdtree") {
    if (!requireNamespace("RANN", quietly = TRUE)) stop("RANN package is required for method = \"kdtree\".")
    RANN::nn2(cbind(x, y), k = 2)$nn.dists[, 2]
  } else {
    d <- as.matrix(stats::dist(cbind(x, y)))
    diag(d) <- Inf
    apply(d, 1, min)
  }
}

# Clark-Evans test of complete spatial randomness (CSR). Reports both the
# Donnelly-corrected asymptotic Z-test (standard practice, comparable to
# published values) and a Monte Carlo p-value (simulate n_sim CSR point
# sets in the same bounding box, compare observed Dobs to that empirical
# distribution) - even the Donnelly correction was checked empirically to
# still over-reject somewhat at small n (~15% instead of 5% at n=40); the
# Monte Carlo p-value doesn't rely on any asymptotic approximation and is
# exact by construction for whatever window shape/size and n are actually
# used, so it's the more trustworthy figure at small n.
#
# n_sim's default depends on nn_method: the "matrix" NND method is O(n^2)
# per simulation (measured: ~0.9 sec/sim at n=3186, i.e. ~50 sec for the
# default 99 sims - and n>2000 already only gets 99 sims specifically to
# avoid this getting much worse), so its defaults stay conservative. The
# "kdtree" method is O(n log n) (measured: ~0.02 sec/sim at n=16705, so
# even 999 sims is ~20 sec there) and defaults much higher accordingly.
#' Clark-Evans nearest-neighbour test of complete spatial randomness
#'
#' Reports both a Donnelly (1978) edge-corrected asymptotic Z-test and a
#' Monte Carlo p-value (simulating `n_sim` CSR point sets in the same
#' bounding box). Both are known to over-reject somewhat at small n - see
#' this file's header comment and the package's Statistical Appendix
#' vignette for the empirically-measured false-positive rates and why the
#' bias is asymmetric (favors false "dispersed" verdicts, not "clustered" ones).
#'
#' @param x Numeric vector of X coordinates.
#' @param y Numeric vector of Y coordinates. Requires at least 3 valid
#'   (finite) points, spanning a non-zero area.
#' @param n_sim Number of Monte Carlo simulations. Defaults to 999 for
#'   `nn_method = "kdtree"`, or 299/199/99 (depending on `n`) for
#'   `nn_method = "matrix"`, to keep runtime reasonable either way.
#' @param seed RNG seed for the Monte Carlo simulation; the caller's RNG
#'   state is saved and restored afterward. Default 42.
#' @param nn_method Nearest-neighbour algorithm, passed to
#'   `compute_nearest_neighbor_distances()`: `"kdtree"` (default, fast,
#'   exact) or `"matrix"` (slow for large `n`, exact). Both give identical
#'   results - this only affects runtime.
#' @return A list: `n`, `area`, `density`, `nnd` (per-point distances),
#'   `Dobs`, `Dpois`, `Dkevin`, `R`, `SE_R`, `Z`, `p_value_asymptotic`,
#'   `n_sim`, `nn_method`, `p_value_monte_carlo`, `p_value` (alias for the
#'   Monte Carlo one), `verdict` (text summary).
#' @export
clark_evans_test <- function(x, y, n_sim = NULL, seed = 42, nn_method = c("kdtree", "matrix")) {
  nn_method <- match.arg(nn_method)
  valid <- is.finite(x) & is.finite(y)
  x <- x[valid]; y <- y[valid]
  n <- length(x)
  if (n < 3) stop("At least 3 valid points are required for the Clark-Evans test.")

  width <- max(x) - min(x)
  height <- max(y) - min(y)
  area <- width * height
  if (!is.finite(area) || area <= 0) stop("Points must span a non-zero area (X and Y cannot both be constant).")
  perim <- 2 * (width + height)
  xr <- range(x); yr <- range(y)

  nnd <- compute_nearest_neighbor_distances(x, y, method = nn_method)
  Dobs <- mean(nnd)

  density <- n / area
  Dpois <- 1 / (2 * sqrt(density))
  Dkevin <- Dpois + (0.0514 + 0.0412 / sqrt(n)) * perim / n
  R <- Dobs / Dkevin

  SE_Dobs <- sqrt((4 - pi) * area / (4 * pi)) / n
  SE_R <- SE_Dobs / Dkevin
  Z <- (R - 1) / SE_R
  p_value <- 2 * (1 - stats::pnorm(abs(Z)))

  if (is.null(n_sim)) {
    n_sim <- if (nn_method == "kdtree") 999 else if (n > 2000) 99 else if (n > 800) 199 else 299
  }

  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv)) .GlobalEnv$.Random.seed else NULL
  on.exit(if (!is.null(old_seed)) assign(".Random.seed", old_seed, envir = .GlobalEnv), add = TRUE)
  set.seed(seed)
  Dobs_sim <- vapply(seq_len(n_sim), function(s) {
    sx <- stats::runif(n, xr[1], xr[2])
    sy <- stats::runif(n, yr[1], yr[2])
    mean(compute_nearest_neighbor_distances(sx, sy, method = nn_method))
  }, numeric(1))
  rank_le <- sum(Dobs_sim <= Dobs) + 1
  rank_ge <- sum(Dobs_sim >= Dobs) + 1
  p_mc <- min(2 * min(rank_le, rank_ge) / (n_sim + 1), 1)

  verdict <- if (p_mc >= 0.05) {
    "No significant evidence against complete spatial randomness (points appear randomly scattered)."
  } else if (R < 1) {
    "Points are significantly CLUSTERED (p < 0.05) - consistent with a localized defect zone or banding."
  } else {
    "Points are significantly more REGULAR/DISPERSED than random (p < 0.05)."
  }

  list(
    n = n, area = area, density = density,
    nnd = nnd, Dobs = Dobs, Dpois = Dpois, Dkevin = Dkevin,
    R = R, SE_R = SE_R, Z = Z, p_value_asymptotic = p_value,
    n_sim = n_sim, nn_method = nn_method, p_value_monte_carlo = p_mc,
    p_value = p_mc,
    verdict = verdict
  )
}

# Scatter plot of point positions, optionally coloured by another column
# (e.g. Field, or an inclusion size/area measure) to visually spot zones.
#' Plot point positions, optionally colored by another variable
#'
#' @param x Numeric vector of X coordinates.
#' @param y Numeric vector of Y coordinates.
#' @param color_by Optional vector (same length as `x`/`y`) to color
#'   points by; continuous columns use a viridis scale.
#' @param color_label Legend title for `color_by`. Default `"Value"`.
#' @return A `ggplot` object.
#' @export
create_spatial_scatter_plot <- function(x, y, color_by = NULL, color_label = "Value") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 package is required for plotting")
  df <- data.frame(x = x, y = y)
  if (!is.null(color_by)) {
    df$color_by <- color_by
    p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, color = color_by)) +
      ggplot2::geom_point(alpha = 0.6, size = 0.8) +
      ggplot2::labs(color = color_label)
    if (is.numeric(color_by)) p <- p + ggplot2::scale_color_viridis_c()
  } else {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point(alpha = 0.6, size = 0.8, color = "#357ABD")
  }
  p + ggplot2::coord_fixed() +
    ggplot2::labs(title = "Spatial Distribution of Points", x = "X", y = "Y") +
    ggplot2::theme_minimal()
}

# Histogram of nearest-neighbour distances, with the (edge-corrected)
# CSR-expected mean marked for visual comparison against the observed
# distribution.
#' Plot a histogram of nearest-neighbour distances
#'
#' Marks the observed mean NND and the Donnelly-corrected expected mean
#' under CSR for visual comparison.
#'
#' @param ce_result A result from `clark_evans_test()`.
#' @return A `ggplot` object.
#' @export
create_nnd_histogram <- function(ce_result) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 package is required for plotting")
  df <- data.frame(nnd = ce_result$nnd)
  ggplot2::ggplot(df, ggplot2::aes(x = nnd)) +
    ggplot2::geom_histogram(bins = 30, fill = "#357ABD", alpha = 0.7) +
    ggplot2::geom_vline(xintercept = ce_result$Dobs, color = "#002147", linewidth = 1, linetype = "solid") +
    ggplot2::geom_vline(xintercept = ce_result$Dkevin, color = "#d32f2f", linewidth = 1, linetype = "dashed") +
    ggplot2::labs(title = "Nearest-Neighbour Distance Distribution",
                  subtitle = "Solid line: observed mean · Dashed line: expected mean under complete spatial randomness",
                  x = "Nearest-neighbour distance", y = "Count") +
    ggplot2::theme_minimal()
}
