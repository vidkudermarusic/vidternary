# ---- EDS reliability: how sure is the share of each inclusion class? ----
# "62 % of the inclusions are TiN" is an estimate from the particles that
# happened to lie in the inspected area. Particles of one kind come in
# groups (a stringer, a cluster in one field), so treating every particle
# as an independent draw gives intervals that are too narrow. When the SEM
# field of each particle is known, whole fields are resampled instead
# (cluster bootstrap); without it, the Wilson score interval is given and
# labelled as such.

#' Share of each class with a confidence interval
#'
#' @param class Class of each particle (`NA` is left out).
#' @param field Optional SEM field ID of each particle. With it, the
#'   interval is a percentile bootstrap over fields; without it, the Wilson
#'   score interval, which assumes independent particles.
#' @param n_boot Bootstrap samples. Default 2000.
#' @param level Confidence level. Default 0.95.
#' @param seed RNG seed; the caller's RNG state is restored afterwards.
#' @return A data frame sorted by count: `class`, `n`, `share_pct`,
#'   `lower_pct`, `upper_pct`, `method` (`"bootstrap over fields"` or
#'   `"Wilson (independent particles)"`).
#' @export
class_share_intervals <- function(class, field = NULL, n_boot = 2000, level = 0.95, seed = 42) {
  keep <- !is.na(class)
  if (!is.null(field)) keep <- keep & !is.na(field)
  class <- as.character(class[keep])
  if (!is.null(field)) field <- field[keep]
  empty <- data.frame(class = character(0), n = integer(0), share_pct = numeric(0), lower_pct = numeric(0),
                      upper_pct = numeric(0), method = character(0), stringsAsFactors = FALSE)
  n_total <- length(class)
  if (n_total == 0) return(empty)

  classes <- names(sort(table(class), decreasing = TRUE))
  n <- as.integer(table(factor(class, levels = classes)))
  share <- n / n_total
  a <- (1 - level) / 2
  use_fields <- !is.null(field) && length(unique(field)) >= 2

  if (use_fields) {
    # Particles per field and class; fields are drawn with replacement.
    counts <- unclass(table(factor(field), factor(class, levels = classes)))
    n_fields <- nrow(counts)
    draws <- .evs_with_seed(seed, matrix(sample.int(n_fields, n_fields * n_boot, replace = TRUE), nrow = n_boot))
    boot <- vapply(seq_along(classes), function(k) rowSums(matrix(counts[draws, k], nrow = n_boot)), numeric(n_boot))
    boot <- matrix(boot, nrow = n_boot)
    totals <- rowSums(boot)
    shares <- boot / ifelse(totals > 0, totals, NA_real_)
    lower <- apply(shares, 2, stats::quantile, probs = a, na.rm = TRUE, names = FALSE)
    upper <- apply(shares, 2, stats::quantile, probs = 1 - a, na.rm = TRUE, names = FALSE)
    method <- "bootstrap over fields"
  } else {
    z <- stats::qnorm(1 - a)
    centre <- (share + z^2 / (2 * n_total)) / (1 + z^2 / n_total)
    half <- z * sqrt(share * (1 - share) / n_total + z^2 / (4 * n_total^2)) / (1 + z^2 / n_total)
    lower <- pmax(centre - half, 0)
    upper <- pmin(centre + half, 1)
    method <- "Wilson (independent particles)"
  }
  data.frame(class = classes, n = n, share_pct = 100 * share, lower_pct = 100 * lower, upper_pct = 100 * upper,
             method = method, stringsAsFactors = FALSE, row.names = NULL)
}
