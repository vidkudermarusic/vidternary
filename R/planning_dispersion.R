# ---- Index of dispersion pooled within repeated runs ----
# Measurement Planning takes several files as repeated runs of one specimen.
# Putting the counts per field of all runs into one pile counts a difference
# between the runs' mean densities as clustering: two runs with 0.73 and 1.14
# on their own gave 1.70 together (audit pass 7). The variance is therefore
# taken around each run's own mean and pooled; the difference between the
# runs is judged separately by density_repeatability(), which divides by
# this index - with the pooled-pile index a real difference between runs
# inflated the index and so hid itself. Runs taken at different
# magnifications (different field sizes) stay correct too, since no count of
# one run is compared with the mean of another.

#' Index of dispersion of counts per SEM field, pooled within runs
#'
#' D = sum over runs and fields of (x - mean of the run)^2, divided by the sum
#' over runs of (fields - 1) * mean of the run. This is the weighted mean of
#' the runs' own indices (variance / mean), each weighted by
#' (fields - 1) * mean; with one run it is that run's index. A run with fewer
#' than 2 fields or no particles adds nothing.
#'
#' @param counts A list with one integer vector of counts per field (empty
#'   fields included as 0) per run.
#' @return A list: `dispersion` (`NA` when no run has 2 fields and a
#'   particle), `n_fields` (all fields of all runs), `runs_used` (runs that
#'   contributed).
#' @export
pooled_within_dispersion <- function(counts) {
  if (!is.list(counts)) counts <- list(counts)
  num <- 0
  den <- 0
  used <- 0L
  for (x in counts) {
    x <- as.numeric(x)
    k <- length(x)
    if (k < 2) next
    m <- mean(x)
    if (!isTRUE(m > 0)) next
    num <- num + sum((x - m)^2)
    den <- den + (k - 1) * m
    used <- used + 1L
  }
  list(dispersion = if (den > 0) num / den else NA_real_,
       n_fields = sum(lengths(counts)),
       runs_used = used)
}
