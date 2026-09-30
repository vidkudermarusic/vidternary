# ---- Extreme Value Statistics (EVS) for Inclusion Rating ----
# Murakami's sqrt(area) method, as standardized in ASTM E2283 ("Standard
# Practice for Extreme Value Analysis of Nonmetallic Inclusions in Steel
# and Other Microstructural Features"): partition the inspected area into
# n equal "control areas" S0 (k whole SEM fields-of-view each),
# take the largest inclusion (by sqrt(area)) per control area, and fit a
# Gumbel probability plot to the resulting block maxima. The fitted line
# extrapolates to the expected largest inclusion over a much larger area,
# expressed as a return period T = (number of control areas being
# predicted over).
#
# Pure statistics/plotting - no Shiny dependency, mirroring the split
# between hex_ternary_plot.R (plotting) and server_hex_ternary.R (wiring).

# Control areas: k consecutive fields merged into one. When most single
# fields hold only detection-limit particles, their block maxima are not
# inclusions at all and bend the Gumbel line; larger control areas fix
# that. All control areas must have equal area, so an incomplete trailing
# group is left out rather than kept as a smaller one.
#' Assign fields to control areas of `fields_per_area` fields each
#'
#' Whole-number field IDs are taken as consecutively numbered fields, so
#' `min(id)..max(id)` counts as the inspected fields (a missing number is a
#' field with no detected particle) and fields are grouped by number
#' (`1..k`, `k+1..2k`, ...). Any other IDs are grouped in sorted order of
#' the distinct values. Fields in an incomplete last group are left out.
#'
#' @param field Field ID of each row to assign.
#' @param fields_per_area Whole number of fields per control area (>= 1).
#' @param all_fields Field IDs of all inspected rows, used to count the
#'   fields (defaults to `field`; pass the unfiltered data's IDs so rows
#'   removed by a filter don't shrink the inspected area).
#' @return A list: `control_area` (per-row control-area number, `NA` for
#'   rows left out or with an unmatched ID), `n_fields`, `n_control_areas`,
#'   `n_leftover_fields`, `numeric_ids`.
#' @export
assign_control_areas <- function(field, fields_per_area = 1, all_fields = field) {
  k <- fields_per_area
  if (length(k) != 1 || !is.finite(k) || k < 1 || k != round(k)) {
    stop("Fields per control area must be a whole number of 1 or more.")
  }
  all_ids <- all_fields[!is.na(all_fields)]
  if (length(all_ids) == 0) stop("The field / frame ID column has no values.")

  all_num <- suppressWarnings(as.numeric(as.character(all_ids)))
  numeric_ids <- all(is.finite(all_num)) && all(all_num == round(all_num))
  if (numeric_ids) {
    first_id <- min(all_num)
    n_fields <- max(all_num) - first_id + 1
    idx <- suppressWarnings(as.numeric(as.character(field))) - first_id + 1
  } else {
    ids_sorted <- sort(unique(as.character(all_ids)))
    n_fields <- length(ids_sorted)
    idx <- match(as.character(field), ids_sorted)
  }

  n_control_areas <- n_fields %/% k
  if (n_control_areas < 1) {
    stop(sprintf("Only %d field(s) inspected - fewer than the %d fields per control area.", n_fields, k))
  }
  control_area <- ceiling(idx / k)
  control_area[!is.na(control_area) & (control_area < 1 | control_area > n_control_areas)] <- NA

  list(
    control_area = control_area,
    n_fields = n_fields,
    n_control_areas = n_control_areas,
    n_leftover_fields = n_fields - n_control_areas * k,
    numeric_ids = numeric_ids
  )
}

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

# Plotting position F_j = j/(n+1) uses the Weibull/mean-position
# convention from Murakami's original papers.
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
# calibrated by parametric bootstrap. R2 alone doesn't say whether the
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
#' @return A list: `statistic` (observed A2), `n`, `n_sim`, `p_value`,
#'   `p_value_bracket` (formatted string), `reject_at_05` (logical) - or
#'   `NULL` if the fit is degenerate (see below), matching how the rest of
#'   this pipeline signals "not applicable."
#' @export
gumbel_goodness_of_fit <- function(fit, n_sim = 999, seed = 42) {
  x <- sort(fit$data$sqrt_area_max)
  n <- length(x)
  a <- fit$intercept
  b <- fit$slope

  # A degenerate fit - block maxima that are numerically all identical
  # (duplicate rows, or measurements tied at the same rounded value) -
  # drives the OLS slope to exactly/near zero, which would make
  # compute_A2() below divide by a near-zero `b`. Checked directly on the
  # data's spread, not just on `b` itself, since floating-point noise in
  # the regression could nudge `b` slightly off exact zero even when `x`
  # is degenerate.
  if (!is.finite(b) || diff(range(x)) < sqrt(.Machine$double.eps) * max(1, mean(abs(x)))) {
    return(NULL)
  }

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
#' `y_T = -ln(-ln(1 - 1/T))` for return period `T`, and returns two
#' intervals from the underlying linear model:
#'
#' * **Prediction interval** (`lower`/`upper`): the range a *single* future
#'   block maximum over `T` control areas is expected to fall in - the
#'   "largest inclusion you might actually see" bound. Wider, because it
#'   adds the residual scatter of block maxima about the fitted line.
#' * **Confidence interval** (`ci_lower`/`ci_upper`) and its standard error
#'   `se_fit`: the uncertainty in the *estimated* return level `predicted`
#'   itself (`se_fit` is the delta-method SE of `a_hat + b_hat * y_T`, i.e.
#'   `sqrt(c' vcov(model) c)` with `c = (1, y_T)`). This is the quantity
#'   ASTM E2283 reports as the confidence bound on the predicted maximum.
#'
#' Both are **defensible approximations**, not exact: they treat the
#' reduced variate `y_T` as known and the straight-line Gumbel
#' probability-plot model as correct. They do not propagate the sampling
#' uncertainty in the block maxima themselves, nor Gumbel-model
#' misspecification - the latter is assessed separately by
#' `gumbel_goodness_of_fit()`. This is the standard interval treatment for
#' the least-squares probability-plot method (an exact interval would
#' require MLE fitting and the Gumbel parameters' full information matrix).
#'
#' @param fit A result from `fit_evs_gumbel()`.
#' @param return_period Return period `T` (multiples of the control area),
#'   a finite number greater than 1.
#' @return A list: `return_period`, `y`, `predicted`, `lower`/`upper` (95%
#'   prediction-interval bounds), `ci_lower`/`ci_upper` (95%
#'   confidence-interval bounds on `predicted`), `se_fit` (standard error
#'   of `predicted`).
#' @export
predict_evs_max <- function(fit, return_period) {
  if (!is.finite(return_period) || return_period <= 1) {
    stop("Return period T must be a finite number greater than 1.")
  }
  y_T <- -log(-log(1 - 1 / return_period))
  newdata <- data.frame(y = y_T)
  pi <- stats::predict(fit$model, newdata = newdata, interval = "prediction", level = 0.95)
  # interval = "confidence" is exactly the delta-method CI for a_hat +
  # b_hat*y_T (se.fit = sqrt(c' vcov(model) c), c = (1, y_T)); asking
  # predict() for it directly keeps the arithmetic identical to R's own
  # rather than re-deriving vcov() by hand.
  ci <- stats::predict(fit$model, newdata = newdata, interval = "confidence", level = 0.95, se.fit = TRUE)

  list(
    return_period = return_period,
    y = y_T,
    predicted = unname(pi[1, "fit"]),
    lower = unname(pi[1, "lwr"]),
    upper = unname(pi[1, "upr"]),
    ci_lower = unname(ci$fit[1, "lwr"]),
    ci_upper = unname(ci$fit[1, "upr"]),
    se_fit = unname(ci$se.fit[1])
  )
}

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
    ggplot2::geom_point(size = 1.6, color = "#357ABD") +
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
      ggplot2::geom_point(data = pred_df, color = "#d32f2f", size = 2.4, shape = 18) +
      ggplot2::labs(subtitle = sprintf(
        "T = %.0f control areas: sqrtArea = %.2f um  |  95%% PI [%.2f, %.2f] (single future max)  |  95%% CI [%.2f, %.2f] (on the estimate, ASTM-style)",
        prediction$return_period, prediction$predicted, prediction$lower, prediction$upper,
        prediction$ci_lower, prediction$ci_upper
      ))
  }

  p
}
