# ---- Repeatability: do repeated measurements of one specimen agree? ----
# Two runs on the same specimen never give the same count. Part of the
# difference is counting statistics (a different set of particles falls
# into the fields, clustered or not); anything beyond that comes from the
# measurement itself - focus, threshold, drift, preparation. The functions
# here separate the two, so that a difference between batches can be judged
# against what a mere repeat would show.

#' Repeatability of the number density
#'
#' @param n Particle counts of the repeated runs (2 or more).
#' @param area_mm2 Inspected area of each run (mm2).
#' @param dispersion Index of dispersion of the counts per SEM field
#'   (1 = randomly scattered particles); widens the counting scatter that
#'   is expected.
#' @param level Level of the repeatability limit. Default 0.95.
#' @return A list: `runs` (data frame: `run`, `n`, `area_mm2`, `per_mm2`),
#'   `mean_per_mm2` (pooled density), `sd_observed`, `cv_observed`,
#'   `sd_counting`, `cv_counting` (scatter expected from counting alone),
#'   `sd_excess` (scatter between runs beyond counting, 0 when there is
#'   none), `statistic`, `df`, `p_value` (chi-squared test of "the runs
#'   differ by counting scatter only", with the statistic divided by
#'   `dispersion`), `limit_per_mm2` and `limit_rel` (repeatability limit:
#'   two runs on areas like these differ by less than this in `level` of
#'   cases).
#' @export
density_repeatability <- function(n, area_mm2, dispersion = 1, level = 0.95) {
  if (length(n) < 2) stop("At least 2 repeated runs are needed.", call. = FALSE)
  if (length(area_mm2) == 1) area_mm2 <- rep(area_mm2, length(n))
  if (length(area_mm2) != length(n)) stop("One inspected area is needed per run.", call. = FALSE)
  if (any(!is.finite(area_mm2) | area_mm2 <= 0)) stop("Every inspected area must be greater than zero.", call. = FALSE)
  if (!is.finite(dispersion) || dispersion <= 0) stop("The index of dispersion must be greater than zero.", call. = FALSE)
  run <- names(n) %||% paste("Run", seq_along(n))
  n <- as.numeric(n)
  if (sum(n) == 0) stop("The runs hold no particles.", call. = FALSE)

  density <- n / area_mm2
  pooled <- sum(n) / sum(area_mm2)
  expected <- pooled * area_mm2
  statistic <- sum((n - expected)^2 / expected) / dispersion
  df <- length(n) - 1
  var_counting <- mean(dispersion * pooled / area_mm2)
  var_observed <- stats::var(density)
  var_excess <- max(0, var_observed - var_counting)
  limit <- stats::qnorm((1 + level) / 2) * sqrt(2 * (var_counting + var_excess))

  list(runs = data.frame(run = run, n = n, area_mm2 = area_mm2, per_mm2 = density, stringsAsFactors = FALSE),
       mean_per_mm2 = pooled, sd_observed = sqrt(var_observed), cv_observed = sqrt(var_observed) / pooled,
       sd_counting = sqrt(var_counting), cv_counting = sqrt(var_counting) / pooled,
       sd_excess = sqrt(var_excess), statistic = statistic, df = df,
       p_value = stats::pchisq(statistic, df, lower.tail = FALSE),
       limit_per_mm2 = limit, limit_rel = limit / pooled, dispersion = dispersion, level = level)
}

#' Repeatability of any quantity read from each run
#'
#' For numbers without a counting model (median diameter, area fraction,
#' return level): the scatter between the runs and the repeatability limit
#' that follows from it. With two or three runs the standard deviation is
#' itself poorly known - the limit is then a rough guide.
#'
#' @param values Named list or data frame: one numeric vector per quantity,
#'   one value per run.
#' @param level Level of the repeatability limit. Default 0.95.
#' @return A data frame: `quantity`, `n_runs`, `mean`, `sd`, `cv`, `min`,
#'   `max`, `limit` (`qnorm * sqrt(2) * sd`), `limit_rel`.
#' @export
quantity_repeatability <- function(values, level = 0.95) {
  values <- as.list(values)
  rows <- lapply(names(values), function(name) {
    v <- as.numeric(values[[name]])
    v <- v[is.finite(v)]
    s <- if (length(v) >= 2) stats::sd(v) else NA_real_
    m <- if (length(v)) mean(v) else NA_real_
    limit <- stats::qnorm((1 + level) / 2) * sqrt(2) * s
    data.frame(quantity = name, n_runs = length(v), mean = m, sd = s, cv = s / m,
               min = if (length(v)) min(v) else NA_real_, max = if (length(v)) max(v) else NA_real_,
               limit = limit, limit_rel = limit / m, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}
