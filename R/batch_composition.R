# ---- Batch comparison: do the batches differ in inclusion chemistry? ----
# Wt% values are compositional (they share one total), so they are compared
# in isometric log-ratio (ilr) coordinates, where ordinary distances are
# meaningful (Aitchison distance). The test is a permutation MANOVA
# (Anderson, 2001): the share of the total variation that lies between the
# batch centroids, compared with what relabelling the particles at random
# gives. It also reacts to batches that merely differ in spread, so the
# spread is tested separately (distance of each particle to its batch
# centroid, Anderson 2006).

# EDS lines that overlap: an element of a group can be reported in place of
# another, so two of them should not be read as independent measurements.
.eds_overlap_groups <- list(c("Ti", "N"), c("Cr", "O"), c("Ti", "V"), c("S", "Mo", "Pb", "Bi"))

#' EDS peak overlaps among a set of elements
#'
#' @param elements Element symbols.
#' @return Character vector, one entry per overlap group with two or more
#'   of its elements present (e.g. `"Ti - N"`); empty when there is none.
#' @export
eds_overlaps <- function(elements) {
  hits <- lapply(.eds_overlap_groups, function(g) intersect(g, elements))
  vapply(hits[lengths(hits) >= 2], paste, character(1), collapse = " - ")
}

# Pseudo-F and R2 of a one-way layout from group sums (Euclidean distances).
.permanova_stat <- function(coords, group, n_groups, ss_total, grand) {
  n_g <- tabulate(group, nbins = n_groups)
  means <- rowsum(coords, group, reorder = TRUE) / n_g
  ss_among <- sum(n_g * rowSums(sweep(means, 2, grand)^2))
  ss_within <- ss_total - ss_among
  n <- length(group)
  c(f = (ss_among / (n_groups - 1)) / (ss_within / (n - n_groups)), r2 = ss_among / ss_total)
}

#' Permutation MANOVA on a matrix of coordinates
#'
#' One-way permutation MANOVA with Euclidean distances (Anderson, 2001).
#' On ilr coordinates this is a test on Aitchison distances.
#'
#' @param coords Numeric matrix or data frame, one row per observation.
#' @param group Group of each row.
#' @param n_perm Number of permutations. Default 999.
#' @param seed RNG seed; the caller's RNG state is restored afterwards.
#' @return A list: `f` (pseudo-F), `r2` (share of the total variation
#'   between the groups), `p_value`, `n_perm`, `df` (`c(k - 1, n - k)`).
#' @export
permanova_test <- function(coords, group, n_perm = 999, seed = 42) {
  coords <- as.matrix(coords)
  group <- factor(group)
  k <- nlevels(group)
  n <- nrow(coords)
  if (k < 2 || n <= k) stop("At least 2 groups and more rows than groups are needed.", call. = FALSE)
  if (anyNA(coords) || any(!is.finite(coords))) stop("The coordinates must not contain missing values.", call. = FALSE)
  g <- as.integer(group)
  grand <- colMeans(coords)
  ss_total <- sum(sweep(coords, 2, grand)^2)
  if (ss_total <= 0) stop("All rows are identical; there is nothing to compare.", call. = FALSE)

  observed <- .permanova_stat(coords, g, k, ss_total, grand)
  permuted <- .evs_with_seed(seed, vapply(seq_len(n_perm), function(i) {
    .permanova_stat(coords, sample(g), k, ss_total, grand)[["f"]]
  }, numeric(1)))
  list(f = observed[["f"]], r2 = observed[["r2"]],
       p_value = (1 + sum(permuted >= observed[["f"]] * (1 - 1e-12))) / (1 + n_perm),
       n_perm = n_perm, df = c(k - 1, n - k))
}

#' Compare the inclusion chemistry of several batches
#'
#' The parts are ilr-transformed together ([ilr_transform()], zeros
#' replaced per part), then compared with [permanova_test()] overall and
#' pair by pair (Holm-adjusted). `dispersion` tests whether the batches
#' differ in spread, which the MANOVA would also pick up. `clr_means` shows
#' which elements drive a difference: the clr mean of each batch minus the
#' clr mean of all particles (positive = relatively more of that element).
#'
#' With thousands of particles a tiny shift is "significant"; `r2` says how
#' much of the variation the batches explain.
#'
#' @param data Data frame with the compositional columns.
#' @param parts Column names of the parts (at least 2).
#' @param batch Batch of each row.
#' @param n_perm,seed See [permanova_test()].
#' @return A list: `overall`, `pairs` (data frame: `batch_a`, `batch_b`,
#'   `f`, `r2`, `p_value`, `p_adjusted`), `dispersion` (list: `per_batch`
#'   data frame with the mean distance to the batch centroid, and the
#'   permutation test `f`, `p_value`), `clr_means` (data frame: `batch`,
#'   `part`, `clr_mean`, `difference`), `zeros` (from [coda_zero_summary()])
#'   and `n` (rows per batch).
#' @export
compare_compositions <- function(data, parts, batch, n_perm = 999, seed = 42) {
  if (length(parts) < 2) stop("At least 2 compositional parts (columns) are required.", call. = FALSE)
  batch <- as.character(batch)
  batches <- unique(batch)
  if (length(batches) < 2) stop("At least 2 batches are needed for a comparison.", call. = FALSE)

  ilr <- as.matrix(ilr_transform(data, parts)$ilr)
  clr <- as.matrix(clr_transform(data, parts))
  overall <- permanova_test(ilr, batch, n_perm, seed)

  idx <- utils::combn(length(batches), 2)
  pairs <- do.call(rbind, lapply(seq_len(ncol(idx)), function(k) {
    a <- batches[idx[1, k]]; b <- batches[idx[2, k]]
    sel <- batch %in% c(a, b)
    t <- permanova_test(ilr[sel, , drop = FALSE], batch[sel], n_perm, seed)
    data.frame(batch_a = a, batch_b = b, f = t$f, r2 = t$r2, p_value = t$p_value, stringsAsFactors = FALSE)
  }))
  pairs$p_adjusted <- stats::p.adjust(pairs$p_value, method = "holm")

  # Spread: distance of each particle to the centroid of its own batch.
  g <- match(batch, batches)
  centroids <- rowsum(ilr, g) / tabulate(g, nbins = length(batches))
  distance <- sqrt(rowSums((ilr - centroids[g, , drop = FALSE])^2))
  spread <- permanova_test(matrix(distance, ncol = 1), batch, n_perm, seed)
  dispersion <- list(
    per_batch = data.frame(batch = batches, mean_distance = as.numeric(tapply(distance, g, mean)), stringsAsFactors = FALSE),
    f = spread$f, p_value = spread$p_value)

  grand <- colMeans(clr)
  # Element symbols as labels when every part is an element wt% column.
  labels <- tryCatch(names(inclusion_element_columns(parts)), error = function(e) character(0))
  labels <- if (length(labels) == length(parts)) labels else parts
  clr_means <- do.call(rbind, lapply(batches, function(b) {
    m <- colMeans(clr[batch == b, , drop = FALSE])
    data.frame(batch = b, part = labels, clr_mean = unname(m), difference = unname(m - grand), stringsAsFactors = FALSE)
  }))

  list(overall = overall, pairs = pairs, dispersion = dispersion, clr_means = clr_means,
       zeros = coda_zero_summary(data, parts), n = stats::setNames(as.integer(table(factor(batch, levels = batches))), batches))
}
