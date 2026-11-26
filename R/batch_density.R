# ---- Batch comparison: how many inclusions, and is the difference real? ----
# Counts of independent particles follow a Poisson distribution, and two
# Poisson counts on known areas are compared exactly with a binomial test.
# Inclusions are rarely independent, though: they come in clusters and
# stringers, which makes counts vary more from field to field than Poisson
# allows and Poisson p-values too small. The index of dispersion of the
# counts per SEM field (variance / mean, 1 for Poisson) shows how much, and
# a rank test on the counts per field is reported next to the Poisson test
# because it does not assume independence of particles within a field.

#' Index of dispersion of the particle counts per SEM field
#'
#' Variance over mean of the number of particles per field, counting the
#' fields without particles as zeros. About 1 for randomly scattered
#' particles; clearly above 1 when particles cluster.
#'
#' @param field SEM field ID of each particle.
#' @param n_fields Number of inspected fields, empty ones included
#'   (default: the number of distinct IDs).
#' @return A list: `counts` (particles per field, zeros included),
#'   `n_fields`, `mean`, `dispersion` (`NA` with fewer than 2 fields or no
#'   particles).
#' @export
field_count_dispersion <- function(field, n_fields = NULL) {
  per_field <- as.integer(table(field[!is.na(field)]))
  if (is.null(n_fields)) n_fields <- length(per_field)
  n_fields <- max(n_fields, length(per_field))
  counts <- c(per_field, rep(0L, n_fields - length(per_field)))
  m <- if (length(counts)) mean(counts) else NA_real_
  list(counts = counts, n_fields = n_fields, mean = m,
       dispersion = if (length(counts) > 1 && isTRUE(m > 0)) stats::var(counts) / m else NA_real_)
}

#' Compare the number density of several batches
#'
#' The overall test asks whether the counts are in proportion to the
#' inspected areas (chi-squared); each pair of batches is compared with the
#' exact Poisson rate-ratio test, with Holm's correction for the number of
#' pairs. Both assume independent particles - see [field_count_dispersion()].
#'
#' @param n Named vector of particle counts per batch.
#' @param area_mm2 Inspected area of each batch (mm2).
#' @param level Confidence level of the ratio interval. Default 0.95.
#' @return A list: `overall` (`statistic`, `df`, `p_value`) and `pairs`, a
#'   data frame with one row per pair: `batch_a`, `batch_b`, `ratio`
#'   (density of a over b), `ratio_lower`, `ratio_upper`, `p_value`,
#'   `p_adjusted`.
#' @export
compare_densities <- function(n, area_mm2, level = 0.95) {
  if (length(n) != length(area_mm2)) stop("One inspected area is needed per batch.", call. = FALSE)
  if (length(n) < 2) stop("At least 2 batches are needed for a comparison.", call. = FALSE)
  if (any(!is.finite(area_mm2) | area_mm2 <= 0)) stop("Every inspected area must be greater than zero.", call. = FALSE)
  batch <- names(n) %||% paste("Batch", seq_along(n))
  n <- as.numeric(n)

  overall <- if (sum(n) > 0) {
    ch <- suppressWarnings(stats::chisq.test(n, p = area_mm2 / sum(area_mm2)))
    list(statistic = unname(ch$statistic), df = unname(ch$parameter), p_value = ch$p.value)
  } else list(statistic = NA_real_, df = length(n) - 1, p_value = NA_real_)

  idx <- utils::combn(length(n), 2)
  rows <- lapply(seq_len(ncol(idx)), function(k) {
    a <- idx[1, k]; b <- idx[2, k]
    if (n[a] + n[b] == 0) {
      return(data.frame(batch_a = batch[a], batch_b = batch[b], ratio = NA_real_, ratio_lower = NA_real_,
                        ratio_upper = NA_real_, p_value = NA_real_, stringsAsFactors = FALSE))
    }
    t <- stats::poisson.test(c(n[a], n[b]), c(area_mm2[a], area_mm2[b]), conf.level = level)
    data.frame(batch_a = batch[a], batch_b = batch[b], ratio = unname(t$estimate),
               ratio_lower = t$conf.int[1], ratio_upper = t$conf.int[2], p_value = t$p.value,
               stringsAsFactors = FALSE)
  })
  pairs <- do.call(rbind, rows)
  pairs$p_adjusted <- stats::p.adjust(pairs$p_value, method = "holm")
  list(overall = overall, pairs = pairs)
}

#' Compare the counts per SEM field of several batches (rank test)
#'
#' Kruskal-Wallis test on the number of particles per field. It treats the
#' field, not the particle, as the unit, so clustering within a field does
#' not inflate it. The fields of all batches must have the same size.
#'
#' @param counts Named list: particles per field of each batch, zeros
#'   included (see [field_count_dispersion()]).
#' @return A list: `statistic`, `df`, `p_value` (`NA` when a batch has fewer
#'   than 2 fields or all counts are equal).
#' @export
compare_field_counts <- function(counts) {
  ok <- length(counts) >= 2 && all(lengths(counts) >= 2)
  values <- unlist(counts, use.names = FALSE)
  if (!ok || length(unique(values)) < 2) return(list(statistic = NA_real_, df = length(counts) - 1, p_value = NA_real_))
  t <- stats::kruskal.test(values, rep(seq_along(counts), lengths(counts)))
  list(statistic = unname(t$statistic), df = unname(t$parameter), p_value = t$p.value)
}
