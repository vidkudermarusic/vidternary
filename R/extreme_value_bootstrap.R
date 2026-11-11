# ---- Bootstrap intervals for the EVS return level ----
# The Gumbel line is fitted by least squares through the ORDERED block
# maxima. Ordinary regression intervals assume independent points of equal
# spread, which ordered maxima are not: in a simulation (Gumbel a = 3,
# b = 1.5, 38 control areas, T = 38) the regression "95% CI" contained the
# true return level in 22.5% of runs. A parametric bootstrap that refits
# the line exactly the way fit_evs_gumbel() does gives ~94-96%, so the
# intervals in predict_evs_max() and the band in create_gumbel_plot() come
# from the functions below instead.

# Runs `expr` with the RNG seeded, then puts the caller's RNG state back
# exactly as it was - including "no seed existed yet".
.evs_with_seed <- function(seed, expr) {
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(seed)
  expr
}

#' Parametric-bootstrap refits of the Gumbel probability-plot line
#'
#' Draws `n_boot` samples of the same size as the data from the fitted
#' Gumbel distribution (`a = fit$intercept`, `b = fit$slope`) and refits
#' each one the way [fit_evs_gumbel()] does (plotting position
#' `j / (n + 1)`, least squares on the reduced variate).
#'
#' @param fit A result from [fit_evs_gumbel()].
#' @param n_boot Number of bootstrap samples. Default 2000.
#' @param seed RNG seed; the caller's RNG state is restored afterwards.
#' @return A two-column matrix (`a`, `b`), one row per bootstrap sample.
#' @export
evs_bootstrap_params <- function(fit, n_boot = 2000, seed = 42) {
  n <- fit$n
  j <- seq_len(n)
  y <- -log(-log(j / (n + 1)))
  y_mean <- mean(y)
  y_centered <- y - y_mean
  Syy <- sum(y_centered^2)

  u <- .evs_with_seed(seed, matrix(stats::runif(n_boot * n), nrow = n_boot))
  x_star <- fit$intercept - fit$slope * log(-log(u))
  x_star <- t(apply(x_star, 1, sort))
  b_star <- as.vector(x_star %*% y_centered) / Syy
  a_star <- rowMeans(x_star) - b_star * y_mean
  cbind(a = a_star, b = b_star)
}

# Basic (reverse-percentile) bootstrap interval around an estimate:
# unlike the plain percentile interval it corrects the upward bias of the
# least-squares estimate (about +0.4 um in the simulation above).
.evs_basic_interval <- function(estimate, replicates, level = 0.95) {
  q <- stats::quantile(replicates, c((1 + level) / 2, (1 - level) / 2), names = FALSE)
  c(lower = 2 * estimate - q[1], upper = 2 * estimate - q[2])
}

#' Bootstrap confidence band of the fitted line
#'
#' Interval of `a + b * y` at each reduced-variate value in `y_grid`, for
#' the band drawn behind the probability plot.
#'
#' @param fit A result from [fit_evs_gumbel()].
#' @param y_grid Reduced-variate values to evaluate.
#' @param level Confidence level. Default 0.95.
#' @param n_boot,seed See [evs_bootstrap_params()].
#' @return A data frame: `y`, `lower`, `upper`.
#' @export
evs_line_band <- function(fit, y_grid, level = 0.95, n_boot = 2000, seed = 42) {
  par <- evs_bootstrap_params(fit, n_boot, seed)
  bounds <- vapply(y_grid, function(yy) {
    .evs_basic_interval(fit$intercept + fit$slope * yy, par[, "a"] + par[, "b"] * yy, level)
  }, numeric(2))
  data.frame(y = y_grid, lower = bounds[1, ], upper = bounds[2, ])
}
