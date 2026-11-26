# ---- Measurement planning: control areas for the largest-inclusion estimate ----
# The return level of the extreme-value fit (EVS tab) gets more precise
# with every control area, but slowly. There is no simple formula for the
# probability-plot fit used here, so the precision is found by simulation:
# draw the planned number of block maxima from the fitted Gumbel
# distribution, refit them the same way, and look at how much the return
# level scatters.

#' Precision of the EVS return level for a planned number of control areas
#'
#' @param fit A pilot fit from [fit_evs_gumbel()].
#' @param return_period Return period T in control areas.
#' @param n_areas Planned numbers of control areas (each 3 or more).
#' @param n_sim Simulated measurements per plan. Default 400.
#' @param level Confidence level. Default 0.95.
#' @param seed RNG seed; the caller's RNG state is restored afterwards.
#' @return A data frame: `n_areas`, `return_level` (of the pilot fit),
#'   `halfwidth_um` (half the width of the central `level` range of the
#'   simulated return levels), `rel_halfwidth`.
#' @export
evs_precision_by_areas <- function(fit, return_period, n_areas, n_sim = 400, level = 0.95, seed = 42) {
  if (!is.finite(return_period) || return_period <= 1) stop("Return period T must be a finite number greater than 1.", call. = FALSE)
  n_areas <- sort(unique(as.integer(n_areas[is.finite(n_areas)])))
  if (length(n_areas) == 0 || any(n_areas < 3)) stop("Each plan needs 3 or more control areas.", call. = FALSE)
  if (!is.finite(fit$slope) || fit$slope <= 0) stop("The pilot fit has no positive slope; no plan can be made from it.", call. = FALSE)
  y_T <- -log(-log(1 - 1 / return_period))
  level_now <- fit$intercept + fit$slope * y_T

  rows <- .evs_with_seed(seed, lapply(n_areas, function(n) {
    y <- -log(-log(seq_len(n) / (n + 1)))
    y_c <- y - mean(y)
    sims <- vapply(seq_len(n_sim), function(i) {
      x <- sort(fit$intercept - fit$slope * log(-log(stats::runif(n))))
      slope <- sum(y_c * x) / sum(y_c^2)
      mean(x) + slope * (y_T - mean(y))
    }, numeric(1))
    q <- stats::quantile(sims, c((1 - level) / 2, (1 + level) / 2), names = FALSE)
    data.frame(n_areas = n, return_level = level_now, halfwidth_um = (q[2] - q[1]) / 2)
  }))
  out <- do.call(rbind, rows)
  out$rel_halfwidth <- out$halfwidth_um / out$return_level
  out
}
