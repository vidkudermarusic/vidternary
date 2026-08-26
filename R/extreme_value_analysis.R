# ---- Extreme Value Statistics (EVS) for Inclusion Rating ----
# Murakami's sqrt(area) method, as standardized in ASTM E2283 ("Standard
# Practice for Extreme Value Analysis of Nonmetallic Inclusions in Steel
# and Other Microstructural Features"): partition the inspected area into
# n equal "control areas" S0 (one SEM field-of-view each, when available),
# take the largest inclusion (by sqrt(area)) per control area, and fit a
# Gumbel probability plot to the resulting block maxima. The fitted line
# extrapolates to the expected largest inclusion over a much larger area,
# expressed as a return period T = (number of control areas being
# predicted over).
#
# Pure statistics/plotting - no Shiny dependency, mirroring the split
# between hex_ternary_plot.R (plotting) and server_hex_ternary.R (wiring).

# Block maxima: largest sqrt(area) per control-area group.
#' Compute per-group block maxima of sqrt(area)
#'
#' Murakami's method: within each control-area group, take the largest
#' inclusion by `sqrt(area)`. Non-finite, non-positive, or NA-grouped rows
#' are dropped first.
#'
#' @param data A data frame containing `area_col` and `group_col`.
#' @param area_col Name of the numeric inclusion-area column.
#' @param group_col Name of the control-area grouping column.
#' @return A data frame with one row per group: `group`, `n_inclusions`, `sqrt_area_max`.
#' @export
compute_block_maxima <- function(data, area_col, group_col) {
  area <- suppressWarnings(as.numeric(data[[area_col]]))
  group <- data[[group_col]]
  valid <- is.finite(area) & area > 0 & !is.na(group)
  area <- area[valid]
  group <- group[valid]

  sqrt_area <- sqrt(area)
  block_max <- tapply(sqrt_area, group, max)
  block_n <- tapply(sqrt_area, group, length)

  data.frame(
    group = names(block_max),
    n_inclusions = as.integer(block_n[names(block_max)]),
    sqrt_area_max = as.numeric(block_max),
    row.names = NULL
  )
}

# Fit the Gumbel probability plot: sqrt_area_max ~ reduced variate y.
# Plotting position F_j = j/(n+1) (Weibull/mean position, the convention
# used in Murakami's original papers); reduced variate y = -ln(-ln(F)).
#' Fit a Gumbel probability plot to block maxima
#'
#' Plotting position `F_j = j/(n+1)` (Weibull/mean position), reduced
#' variate `y = -ln(-ln(F))`, fit by OLS: `sqrt_area_max ~ y`.
#'
#' @param sqrt_area_max Numeric vector of block maxima (e.g.
#'   `compute_block_maxima()$sqrt_area_max`). Requires at least 3 finite,
#'   positive values.
#' @return A list: `data` (fit data frame with `rank`/`sqrt_area_max`/`F`/`y`),
#'   `model` (the `lm` object), `intercept`, `slope`, `r_squared`, `n`.
#' @export
fit_evs_gumbel <- function(sqrt_area_max) {
  sqrt_area_max <- sort(sqrt_area_max[is.finite(sqrt_area_max) & sqrt_area_max > 0])
  n <- length(sqrt_area_max)
  if (n < 3) stop("At least 3 control-area groups are required to fit an EVS model.")

  j <- seq_len(n)
  F <- j / (n + 1)
  y <- -log(-log(F))

  fit_data <- data.frame(rank = j, sqrt_area_max = sqrt_area_max, F = F, y = y)
  model <- stats::lm(sqrt_area_max ~ y, data = fit_data)
  coefs <- stats::coef(model)

  list(
    data = fit_data,
    model = model,
    intercept = unname(coefs[1]),
    slope = unname(coefs[2]),
    r_squared = summary(model)$r.squared,
    n = n
  )
}

# Anderson-Darling goodness-of-fit test for the fitted Gumbel distribution,
# calibrated by parametric bootstrap. R² alone doesn't say whether the
# block maxima actually follow a Gumbel distribution - a mixture of
# populations (e.g. two inclusion types with different size distributions)
# can still fit a straight line reasonably well while visibly curving away
# from it at the tail.
#
# Published Anderson-Darling critical values for the Gumbel distribution
# (Stephens 1977 / D'Agostino & Stephens 1986, as used by
# scipy.stats.anderson(dist="gumbel_r")) assume parameters were estimated
# by maximum likelihood. fit_evs_gumbel() instead estimates (a, b) by
# least-squares regression on the probability plot - a different
# estimator with a different null distribution for the test statistic.
# Using the MLE-calibrated table here was checked empirically (1000
# simulated Gumbel-true samples) and over-rejected at ~15% instead of the
# nominal 5%. Instead, the null distribution is simulated directly for
# this exact estimator: simulate many samples from Gumbel(a, b), refit
# each the same way, and see where the observed statistic falls.
#' Anderson-Darling goodness-of-fit test for a fitted Gumbel model
#'
#' Tests whether the block maxima plausibly come from a single Gumbel
#' distribution, via a parametric-bootstrap null distribution calibrated
#' for this exact (least-squares probability-plot) estimator, rather than
#' the published MLE-calibrated critical values (which were checked
#' empirically to over-reject at ~15% instead of the nominal 5% here).
#'
#' @param fit A result from `fit_evs_gumbel()`.
#' @param n_sim Number of bootstrap replicates. Default 999.
#' @param seed RNG seed for the bootstrap; the caller's RNG state is saved
#'   and restored afterward.
#' @return A list: `statistic` (observed A²), `n`, `n_sim`, `p_value`,
#'   `p_value_bracket` (formatted string), `reject_at_05` (logical).
#' @export
gumbel_goodness_of_fit <- function(fit, n_sim = 999, seed = 42) {
  x <- sort(fit$data$sqrt_area_max)
  n <- length(x)
  a <- fit$intercept
  b <- fit$slope

  compute_A2 <- function(x_sorted, a, b) {
    Fx <- exp(-exp(-(x_sorted - a) / b))
    eps <- 1e-10
    Fx <- pmin(pmax(Fx, eps), 1 - eps)
    i <- seq_len(length(x_sorted))
    -length(x_sorted) - mean((2 * i - 1) * (log(Fx) + log(1 - rev(Fx))))
  }

  A2_obs <- compute_A2(x, a, b)

  # Reduced variate is fixed (it only depends on rank/n), so the
  # least-squares refit for each simulated sample reduces to closed-form
  # simple linear regression instead of a much slower repeated lm() call.
  j <- seq_len(n)
  y <- -log(-log(j / (n + 1)))
  y_mean <- mean(y)
  y_centered <- y - y_mean
  Syy <- sum(y_centered^2)

  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv)) .GlobalEnv$.Random.seed else NULL
  on.exit(if (!is.null(old_seed)) assign(".Random.seed", old_seed, envir = .GlobalEnv), add = TRUE)
  set.seed(seed)

  A2_sim <- vapply(seq_len(n_sim), function(s) {
    u <- stats::runif(n)
    x_star <- sort(a - b * log(-log(u)))
    x_mean <- mean(x_star)
    b_star <- sum(y_centered * (x_star - x_mean)) / Syy
    a_star <- x_mean - b_star * y_mean
    compute_A2(x_star, a_star, b_star)
  }, numeric(1))

  p_value <- (1 + sum(A2_sim >= A2_obs)) / (1 + n_sim)

  list(
    statistic = A2_obs,
    n = n,
    n_sim = n_sim,
    p_value = p_value,
    p_value_bracket = sprintf("= %.3f (bootstrap)", p_value),
    reject_at_05 = p_value < 0.05
  )
}

# Predict the largest sqrt(area) expected over a larger area, expressed as
# a return period T (multiples of the control area S0). T must be > 1.
#' Predict the largest inclusion expected over a larger area
#'
#' Extrapolates the fitted Gumbel line to reduced variate
#' `y_T = -ln(-ln(1 - 1/T))` for return period `T`, with a 95% prediction
#' interval from the underlying linear model.
#'
#' @param fit A result from `fit_evs_gumbel()`.
#' @param return_period Return period `T` (multiples of the control area),
#'   a finite number greater than 1.
#' @return A list: `return_period`, `y`, `predicted`, `lower`, `upper` (95% PI bounds).
#' @export
predict_evs_max <- function(fit, return_period) {
  if (!is.finite(return_period) || return_period <= 1) {
    stop("Return period T must be a finite number greater than 1.")
  }
  y_T <- -log(-log(1 - 1 / return_period))
  pred <- stats::predict(fit$model, newdata = data.frame(y = y_T), interval = "prediction", level = 0.95)

  list(
    return_period = return_period,
    y = y_T,
    predicted = unname(pred[1, "fit"]),
    lower = unname(pred[1, "lwr"]),
    upper = unname(pred[1, "upr"])
  )
}

# Gumbel probability plot: reduced variate y (x-axis, with a secondary
# probability-scale axis) vs sqrt(area) block maxima (y-axis), fitted
# line, and the extrapolated prediction when supplied.
#' Build the Gumbel probability plot
#'
#' Reduced variate (x-axis, with a secondary cumulative-probability axis)
#' vs. sqrt(area) block maxima, fitted line with confidence band, and the
#' extrapolated prediction point/interval if supplied.
#'
#' @param fit A result from `fit_evs_gumbel()`.
#' @param prediction Optional result from `predict_evs_max()`, plotted as
#'   an additional point with error bars.
#' @return A `ggplot` object.
#' @export
create_gumbel_plot <- function(fit, prediction = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 package is required for plotting")

  f_breaks <- c(0.5, 0.8, 0.9, 0.95, 0.99, 0.999)
  y_breaks <- -log(-log(f_breaks))

  p <- ggplot2::ggplot(fit$data, ggplot2::aes(x = y, y = sqrt_area_max)) +
    ggplot2::geom_point(size = 2, color = "#357ABD") +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "#002147", fill = "#357ABD", alpha = 0.15) +
    ggplot2::scale_x_continuous(
      name = "Reduced variate y = -ln(-ln(F))",
      sec.axis = ggplot2::sec_axis(~., name = "Cumulative probability F", breaks = y_breaks, labels = f_breaks)
    ) +
    ggplot2::labs(y = expression(sqrt(Area)~"("*mu*m*")"),
                  title = "Gumbel Extreme Value Probability Plot") +
    ggplot2::theme_minimal()

  if (!is.null(prediction)) {
    pred_df <- data.frame(y = prediction$y, sqrt_area_max = prediction$predicted)
    p <- p +
      ggplot2::geom_errorbar(data = pred_df,
                              ggplot2::aes(ymin = prediction$lower, ymax = prediction$upper),
                              width = 0.3, color = "#d32f2f") +
      ggplot2::geom_point(data = pred_df, color = "#d32f2f", size = 3, shape = 18) +
      ggplot2::labs(subtitle = sprintf(
        "Predicted for T = %.0f control areas: √Area = %.2f µm [%.2f, %.2f] (95%% PI)",
        prediction$return_period, prediction$predicted, prediction$lower, prediction$upper
      ))
  }

  p
}
