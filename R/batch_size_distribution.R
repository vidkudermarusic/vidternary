# ---- Batch comparison: do the batches have the same size distribution? ----
# The k-sample Anderson-Darling statistic of Scholz & Stephens (1987)
# compares the whole distributions and is more sensitive in the tails than
# Kolmogorov-Smirnov - and the tail (the large inclusions) is what matters
# for steel. Particle areas from an SEM are multiples of the pixel area, so
# small sizes are heavily tied; the tie-adjusted (mid-rank) form of the
# statistic is used. The p-value comes from permuting the batch labels, so
# it does not rest on the asymptotic tables (or on the exact form of the
# statistic): any statistic is valid in a permutation test.

# Tie-adjusted k-sample Anderson-Darling statistic. `index` is the position
# of each observation among the sorted distinct values (1..L), `multiplicity`
# the number of pooled observations at each distinct value.
.ad_ksample_statistic <- function(index, group, n_groups, multiplicity) {
  n_total <- length(index)
  n_distinct <- length(multiplicity)
  pooled_below <- cumsum(multiplicity) - multiplicity / 2        # B_aj
  denom <- pooled_below * (n_total - pooled_below) - n_total * multiplicity / 4
  use <- denom > 0
  total <- 0
  for (g in seq_len(n_groups)) {
    f <- tabulate(index[group == g], nbins = n_distinct)          # f_ij
    n_g <- sum(f)
    below <- cumsum(f) - f / 2                                    # M_aij
    total <- total + sum((multiplicity / n_total * (n_total * below - n_g * pooled_below)^2 / denom)[use]) / n_g
  }
  (n_total - 1) / n_total * total
}

#' k-sample Anderson-Darling test with a permutation p-value
#'
#' Tests whether several samples come from one distribution. The statistic
#' is the tie-adjusted k-sample Anderson-Darling statistic of Scholz &
#' Stephens (1987); the p-value is the share of label permutations with a
#' statistic at least as large as the observed one.
#'
#' @param x Numeric values (e.g. equivalent circle diameters).
#' @param group Sample (batch) of each value.
#' @param n_perm Number of permutations. Default 999.
#' @param seed RNG seed; the caller's RNG state is restored afterwards.
#' @return A list: `statistic`, `p_value`, `n_perm`, `n` (sample sizes).
#' @export
ad_ksample_test <- function(x, group, n_perm = 999, seed = 42) {
  ok <- is.finite(x) & !is.na(group)
  x <- x[ok]
  group <- factor(group[ok])
  n <- table(group)
  if (nlevels(group) < 2 || any(n < 2)) stop("At least 2 samples with 2 or more values each are needed.", call. = FALSE)

  distinct <- sort(unique(x))
  index <- match(x, distinct)
  multiplicity <- tabulate(index, nbins = length(distinct))
  g <- as.integer(group)
  k <- nlevels(group)
  observed <- .ad_ksample_statistic(index, g, k, multiplicity)
  permuted <- .evs_with_seed(seed, vapply(seq_len(n_perm), function(i) {
    .ad_ksample_statistic(index, sample(g), k, multiplicity)
  }, numeric(1)))
  # A tiny tolerance so that a permutation equal to the observed labelling
  # is not lost to rounding.
  p <- (1 + sum(permuted >= observed * (1 - 1e-12))) / (1 + n_perm)
  list(statistic = observed, p_value = p, n_perm = n_perm, n = stats::setNames(as.integer(n), names(n)))
}

#' Compare the size distributions of several batches
#'
#' Overall k-sample Anderson-Darling test, the same test for every pair
#' (Holm-adjusted), and the quantiles that show how large the difference is.
#' With thousands of particles even a negligible difference is
#' "significant", so read the quantiles before the p-values.
#'
#' @param size Particle sizes (e.g. equivalent circle diameters, um).
#' @param batch Batch of each particle.
#' @param n_perm,seed See [ad_ksample_test()].
#' @return A list: `overall` (from [ad_ksample_test()]), `pairs` (data
#'   frame: `batch_a`, `batch_b`, `statistic`, `p_value`, `p_adjusted`,
#'   `median_ratio`, `p90_ratio`) and `quantiles` (data frame per batch:
#'   `n`, `median`, `p90`, `p99`, `max`).
#' @export
compare_size_distributions <- function(size, batch, n_perm = 999, seed = 42) {
  ok <- is.finite(size) & !is.na(batch)
  size <- size[ok]
  batch <- as.character(batch[ok])
  batches <- unique(batch)
  if (length(batches) < 2) stop("At least 2 batches are needed for a comparison.", call. = FALSE)

  q <- function(v, p) unname(stats::quantile(v, p))
  quantiles <- do.call(rbind, lapply(batches, function(b) {
    v <- size[batch == b]
    data.frame(batch = b, n = length(v), median = q(v, 0.5), p90 = q(v, 0.9), p99 = q(v, 0.99),
               max = max(v), stringsAsFactors = FALSE)
  }))

  overall <- ad_ksample_test(size, batch, n_perm, seed)
  idx <- utils::combn(length(batches), 2)
  pairs <- do.call(rbind, lapply(seq_len(ncol(idx)), function(k) {
    a <- batches[idx[1, k]]; b <- batches[idx[2, k]]
    sel <- batch %in% c(a, b)
    t <- ad_ksample_test(size[sel], batch[sel], n_perm, seed)
    qa <- quantiles[quantiles$batch == a, ]; qb <- quantiles[quantiles$batch == b, ]
    data.frame(batch_a = a, batch_b = b, statistic = t$statistic, p_value = t$p_value,
               median_ratio = qa$median / qb$median, p90_ratio = qa$p90 / qb$p90, stringsAsFactors = FALSE)
  }))
  pairs$p_adjusted <- stats::p.adjust(pairs$p_value, method = "holm")
  list(overall = overall, pairs = pairs[, c("batch_a", "batch_b", "statistic", "p_value", "p_adjusted",
                                            "median_ratio", "p90_ratio")], quantiles = quantiles)
}
