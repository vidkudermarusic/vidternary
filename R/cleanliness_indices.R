# ---- Steel cleanliness: number density, area fraction and size classes ----
# Indices that do not depend on a chart: particles per mm2, area fraction
# in ppm and counts per size class, each with an interval that says how far
# the small inspected area lets the number be trusted.

#' Exact (Garwood) confidence interval of a Poisson count
#'
#' @param n Observed count(s).
#' @param level Confidence level. Default 0.95.
#' @return A two-column matrix (`lower`, `upper`), one row per count.
#' @export
poisson_count_ci <- function(n, level = 0.95) {
  a <- (1 - level) / 2
  lower <- ifelse(n > 0, stats::qchisq(a, 2 * n) / 2, 0)
  upper <- stats::qchisq(1 - a, 2 * n + 2) / 2
  cbind(lower = unname(lower), upper = unname(upper))
}

#' Number density and area fraction per group
#'
#' For each group and for all particles together: count, particles per
#' mm2 with an exact Poisson interval, area fraction in ppm (um2 of
#' inclusion per mm2 inspected, which is the same number) and the mean and
#' largest equivalent circle diameter.
#'
#' The area fraction is dominated by a few large particles, so a Poisson
#' interval would be far too narrow for it. When `field` is given, its
#' interval is a bootstrap over SEM fields instead (fields are resampled
#' with replacement, fields without particles included).
#'
#' @param area_um2 Particle areas (um2).
#' @param group Group of each particle (e.g. chemical family).
#' @param inspected_area_mm2 Total inspected area.
#' @param field Optional SEM field ID of each particle.
#' @param n_fields Number of inspected fields, empty ones included;
#'   default: the number of distinct `field` values.
#' @param n_boot Bootstrap samples. Default 2000.
#' @param level Confidence level. Default 0.95.
#' @param seed RNG seed; the caller's RNG state is restored afterwards.
#' @return A data frame: `group`, `n`, `per_mm2`, `per_mm2_lower`,
#'   `per_mm2_upper`, `area_ppm`, `area_ppm_lower`, `area_ppm_upper` (`NA`
#'   without `field`), `mean_ecd_um`, `max_ecd_um`, `max_sqrt_area_um`.
#' @export
cleanliness_indices <- function(area_um2, group, inspected_area_mm2, field = NULL, n_fields = NULL,
                                n_boot = 2000, level = 0.95, seed = 42) {
  if (!is.finite(inspected_area_mm2) || inspected_area_mm2 <= 0) {
    stop("The inspected area must be greater than zero.", call. = FALSE)
  }
  group <- as.character(group)
  groups <- c("All", sort(unique(group)))
  member <- function(g) if (g == "All") rep(TRUE, length(group)) else group == g

  boot <- NULL
  if (!is.null(field) && length(area_um2) > 0) {
    ids <- unique(field[!is.na(field)])
    if (is.null(n_fields)) n_fields <- length(ids)
    n_fields <- max(n_fields, length(ids))
    # Area per field and group; rows of zeros stand for the empty fields.
    sums <- vapply(groups, function(g) {
      per_field <- tapply(area_um2[member(g)], factor(field[member(g)], levels = ids), sum)
      per_field[is.na(per_field)] <- 0
      c(as.numeric(per_field), rep(0, n_fields - length(ids)))
    }, numeric(n_fields))
    if (n_fields == 1) sums <- matrix(sums, nrow = 1, dimnames = list(NULL, groups))
    draws <- .evs_with_seed(seed, matrix(sample.int(n_fields, n_fields * n_boot, replace = TRUE), nrow = n_boot))
    boot <- vapply(seq_along(groups), function(k) {
      totals <- rowSums(matrix(sums[draws, k], nrow = n_boot))
      stats::quantile(totals / inspected_area_mm2, c((1 - level) / 2, (1 + level) / 2), names = FALSE)
    }, numeric(2))
  }

  rows <- lapply(seq_along(groups), function(k) {
    sel <- member(groups[k])
    a <- area_um2[sel]
    ci <- poisson_count_ci(length(a), level)
    ecd <- 2 * sqrt(a / pi)
    data.frame(
      group = groups[k], n = length(a),
      per_mm2 = length(a) / inspected_area_mm2,
      per_mm2_lower = ci[1, "lower"] / inspected_area_mm2,
      per_mm2_upper = ci[1, "upper"] / inspected_area_mm2,
      area_ppm = sum(a) / inspected_area_mm2,
      area_ppm_lower = if (is.null(boot)) NA_real_ else boot[1, k],
      area_ppm_upper = if (is.null(boot)) NA_real_ else boot[2, k],
      mean_ecd_um = if (length(a)) mean(ecd) else NA_real_,
      max_ecd_um = if (length(a)) max(ecd) else NA_real_,
      max_sqrt_area_um = if (length(a)) sqrt(max(a)) else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Read size-class limits typed as text
#'
#' @param text Limits separated by commas, semicolons or spaces, e.g.
#'   `"1, 2, 5, 10, 20"`.
#' @return Sorted numeric vector of distinct positive limits.
#' @export
parse_size_breaks <- function(text) {
  parts <- strsplit(trimws(as.character(text)), "[,;[:space:]]+")[[1]]
  parts <- parts[nzchar(parts)]
  values <- suppressWarnings(as.numeric(parts))
  if (length(values) == 0 || anyNA(values) || any(values <= 0)) {
    stop("Size-class limits must be positive numbers separated by commas, e.g. 1, 2, 5, 10, 20.", call. = FALSE)
  }
  sort(unique(values))
}

#' Counts and number density per size class and group
#'
#' @param ecd_um Equivalent circle diameters (um).
#' @param group Group of each particle.
#' @param inspected_area_mm2 Total inspected area.
#' @param breaks Class limits in um. Default `c(1, 2, 5, 10, 20)`, giving
#'   the classes `< 1`, `1 - 2`, ..., `>= 20`.
#' @return A data frame in long form: `size_class`, `group`, `n`,
#'   `per_mm2`, `per_mm2_lower`, `per_mm2_upper` (exact Poisson interval).
#' @export
size_class_table <- function(ecd_um, group, inspected_area_mm2, breaks = c(1, 2, 5, 10, 20)) {
  breaks <- sort(unique(breaks))
  labels <- c(paste("<", breaks[1]),
              if (length(breaks) > 1) paste(breaks[-length(breaks)], "-", breaks[-1]),
              paste(">=", breaks[length(breaks)]))
  cls <- factor(labels[findInterval(ecd_um, breaks) + 1], levels = labels)
  group <- as.character(group)
  groups <- c("All", sort(unique(group)))
  rows <- lapply(groups, function(g) {
    n <- as.integer(table(if (g == "All") cls else cls[group == g]))
    ci <- poisson_count_ci(n)
    data.frame(size_class = factor(labels, levels = labels), group = g, n = n,
               per_mm2 = n / inspected_area_mm2,
               per_mm2_lower = ci[, "lower"] / inspected_area_mm2,
               per_mm2_upper = ci[, "upper"] / inspected_area_mm2,
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}
