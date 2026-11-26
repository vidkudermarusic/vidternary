# ---- Batch comparison: permutation of whole SEM fields ----
# The size and composition tests ask whether the particles of the batches
# could come from one population. Permuting single particles between the
# batches treats them as independent, but particles of one SEM field are
# not: inclusions come in clusters and stringers, the counts per field vary
# more than random scatter (index of dispersion above 1), and the p-values
# then come out too small - a reviewer will call it pseudoreplication.
#
# Here the SEM field is the unit instead: under "the batches do not
# differ", whole fields are exchangeable between the batches, so the batch
# labels are permuted field by field (every particle of a field moves
# together; each batch keeps its number of fields). The test statistic is
# the same as in the particle test. A particle without a field ID is its own
# unit.
#
# For the Anderson-Darling test the statistic has to be computed once per
# permutation, which kSamples::ad.test() is far too slow for (0.4 s a call
# on 7500 particles); kSamples also only permutes single particles. The
# statistic is therefore computed here, with the tie-adjusted formula of
# Scholz & Stephens (1987, "version 2"). It agrees with kSamples to about 5
# digits (checked in test-field-permutation.R on continuous and on tied
# data), so the field test and the particle test use the same statistic.

# Tie-adjusted k-sample Anderson-Darling statistic. `index` is the position
# of each observation among the sorted distinct values (1..L), `group` the
# sample of each observation (1..n_groups), `multiplicity` the number of
# pooled observations at each distinct value.
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
    if (n_g == 0) next
    below <- cumsum(f) - f / 2                                    # M_aij
    total <- total + sum((multiplicity / n_total * (n_total * below - n_g * pooled_below)^2 / denom)[use]) / n_g
  }
  (n_total - 1) / n_total * total
}

# One key per SEM field of a batch (the same field number in two batches is
# two fields); a particle without a field ID is a unit of its own.
.field_units <- function(field, group) {
  key <- paste(as.character(group), as.character(field), sep = "\r")
  key[is.na(field)] <- paste0("\r\rparticle ", which(is.na(field)))
  key
}

# Permutation p-value with whole fields as the unit. `group` is the batch
# number (1..k) of each particle, `unit` its field key, `stat_fun` the test
# statistic as a function of the batch number of every particle.
.field_permutation <- function(group, unit, stat_fun, n_perm, seed) {
  units <- unique(unit)
  member <- match(unit, units)
  unit_group <- group[match(units, unit)]
  observed <- stat_fun(group)
  permuted <- .evs_with_seed(seed, vapply(seq_len(n_perm), function(i) {
    stat_fun(sample(unit_group)[member])
  }, numeric(1)))
  # A tiny tolerance so that a permutation equal to the observed labelling
  # is not lost to rounding; the observed labelling counts as one of them.
  list(observed = observed, p_value = (1 + sum(permuted >= observed * (1 - 1e-12))) / (1 + n_perm),
       n_units = tabulate(unit_group, nbins = max(group)))
}

#' k-sample Anderson-Darling test with whole SEM fields permuted
#'
#' The same tie-adjusted statistic as [ad_ksample_test()], but the p-value
#' comes from permuting the batch labels of whole SEM fields, so that
#' clustering of particles within a field does not make it too small.
#'
#' @param x Numeric values (e.g. equivalent circle diameters).
#' @param group Sample (batch) of each value.
#' @param field SEM field ID of each value (within its batch); a value
#'   without a field ID is a unit of its own.
#' @param n_perm Number of permutations. Default 999.
#' @param seed RNG seed; the caller's RNG state is restored afterwards.
#' @return A list: `statistic`, `p_value`, `n_perm`, `n` (values per
#'   sample), `n_fields` (fields per sample), `unit` (`"SEM fields"`).
#' @export
ad_field_permutation_test <- function(x, group, field, n_perm = 999, seed = 42) {
  if (length(field) != length(x)) stop("One SEM field ID is needed per value.", call. = FALSE)
  ok <- is.finite(x) & !is.na(group)
  x <- x[ok]
  field <- field[ok]
  group <- factor(group[ok])
  n <- table(group)
  if (nlevels(group) < 2 || any(n < 2)) stop("At least 2 samples with 2 or more values each are needed.", call. = FALSE)
  if (length(n_perm) != 1 || !is.finite(n_perm) || n_perm < 1) stop("The number of permutations must be 1 or more.", call. = FALSE)
  n_perm <- as.integer(n_perm)
  unit <- .field_units(field, group)
  g <- as.integer(group)
  k <- nlevels(group)
  n_fields <- stats::setNames(tabulate(g[match(unique(unit), unit)], nbins = k), levels(group))
  out <- list(statistic = 0, p_value = 1, n_perm = n_perm, n = stats::setNames(as.integer(n), names(n)),
              n_fields = n_fields, unit = "SEM fields")
  if (length(unique(x)) == 1) return(out)

  distinct <- sort(unique(x))
  index <- match(x, distinct)
  multiplicity <- tabulate(index, nbins = length(distinct))
  perm <- .field_permutation(g, unit, function(gg) .ad_ksample_statistic(index, gg, k, multiplicity), n_perm, seed)
  out$statistic <- perm$observed
  out$p_value <- perm$p_value
  out
}

#' Permutation MANOVA with whole SEM fields permuted
#'
#' The same pseudo-F as [permanova_test()], with the p-value from
#' permuting the batch labels of whole SEM fields.
#'
#' @param coords Numeric matrix or data frame, one row per observation.
#' @param group Group of each row.
#' @param field SEM field ID of each row (within its group).
#' @param n_perm,seed See [ad_field_permutation_test()].
#' @return A list: `f`, `r2`, `p_value`, `n_perm`, `df`, `n_fields`,
#'   `unit` (`"SEM fields"`).
#' @export
permanova_field_test <- function(coords, group, field, n_perm = 999, seed = 42) {
  coords <- as.matrix(coords)
  if (length(field) != nrow(coords)) stop("One SEM field ID is needed per row.", call. = FALSE)
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
  unit <- .field_units(field, group)
  perm <- .field_permutation(g, unit, function(gg) .permanova_stat(coords, gg, k, ss_total, grand)[["f"]], n_perm, seed)
  list(f = observed[["f"]], r2 = observed[["r2"]], p_value = perm$p_value, n_perm = as.integer(n_perm),
       df = c(k - 1, n - k), n_fields = stats::setNames(perm$n_units, levels(group)), unit = "SEM fields")
}
