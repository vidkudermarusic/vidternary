# ---- Batch comparison: do the batches have the same size distribution? ----
# The k-sample Anderson-Darling statistic of Scholz & Stephens (1987)
# compares the whole distributions and is more sensitive in the tails than
# Kolmogorov-Smirnov - and the tail (the large inclusions) is what matters
# for steel. Particle areas from an SEM are multiples of the pixel area, so
# small sizes are heavily tied; the tie-adjusted (mid-rank) form of the
# statistic is used ("version 2" in Scholz & Stephens). The test is computed
# by the kSamples package of Scholz & Zhu, written by the first author of
# the method; the p-value comes from its random splits of the pooled values
# into samples of the observed sizes, i.e. from permuting the batch labels.
#
# Two details of kSamples::ad.test() are handled here:
# * it stores the version-2 statistic rounded to 3 significant digits but
#   its standardised form T.AD = (AD - (k - 1)) / sigma to 5, so the
#   statistic is taken back from T.AD and sigma;
# * its simulated p-value is (number of splits with AD >= observed) / Nsim,
#   which is 0 when no split reaches the observed value. The observed
#   labelling is itself one of the possible splits, so the p-value reported
#   is (count + 1) / (Nsim + 1), which is never 0 (Phipson & Smyth, 2010).
# The project's own implementation of the statistic, used up to 2026-11,
# agreed with kSamples to 4 digits on the test data.

#' k-sample Anderson-Darling test with a permutation p-value
#'
#' Tests whether several samples come from one distribution, with
#' [kSamples::ad.test()]. The statistic is the tie-adjusted k-sample
#' Anderson-Darling statistic of Scholz & Stephens (1987); the p-value is
#' the share of `n_perm` random relabellings, plus the observed one, whose
#' statistic is at least as large as the observed one.
#'
#' @param x Numeric values (e.g. equivalent circle diameters).
#' @param group Sample (batch) of each value.
#' @param n_perm Number of permutations. Default 999.
#' @param seed RNG seed; the caller's RNG state is restored afterwards.
#' @return A list: `statistic`, `p_value` (permutation), `p_value_asymptotic`
#'   (from the asymptotic distribution of Scholz & Stephens, for reference),
#'   `n_perm`, `n` (sample sizes).
#' @export
ad_ksample_test <- function(x, group, n_perm = 999, seed = 42) {
  ok <- is.finite(x) & !is.na(group)
  x <- x[ok]
  group <- factor(group[ok])
  n <- table(group)
  if (nlevels(group) < 2 || any(n < 2)) stop("At least 2 samples with 2 or more values each are needed.", call. = FALSE)
  if (length(n_perm) != 1 || !is.finite(n_perm) || n_perm < 1) stop("The number of permutations must be 1 or more.", call. = FALSE)
  n_perm <- as.integer(n_perm)

  samples <- split(x, group)
  res <- .evs_with_seed(seed, kSamples::ad.test(samples, method = "simulated", Nsim = n_perm))
  version2 <- unclass(res$ad)[2, ]
  k <- nlevels(group)
  statistic <- unname(version2[2]) * res$sig + (k - 1)
  exceed <- round(unname(version2[4]) * n_perm)
  list(statistic = statistic, p_value = (exceed + 1) / (n_perm + 1), p_value_asymptotic = unname(version2[3]),
       n_perm = n_perm, n = stats::setNames(as.integer(n), names(n)))
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
