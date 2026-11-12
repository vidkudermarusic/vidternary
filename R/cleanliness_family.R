# ---- Steel cleanliness: chemical family of each particle ----
# The standard inclusion ratings (ISO 4967, ASTM E45) sort inclusions by
# chemistry AND shape. This file does the chemistry half from an EDS
# analysis; the shape half is in cleanliness_rating.R.
#
# Only the cations that mark an inclusion are used: S, Ti, Al, Mg, Ca, Si.
# N, O and C are deliberately not used: the N K line overlaps Ti L, the O K
# line overlaps Cr L, and C is mostly contamination. So a "Ti-rich"
# particle may be a nitride, carbonitride or oxide - EDS cannot tell them
# apart reliably and this function does not try.

# Mass of compound per unit mass of the marker element, so the families are
# compared on what they weigh as a phase, not on raw wt% (1 wt% S stands
# for 2.7 wt% MnS, 1 wt% Ti for 1.3 wt% TiN).
.family_factors <- function(weights) {
  cmp <- inclusion_compounds(weights)
  list(
    sulfide = c(S = unname(cmp["MnS", "molar_mass"] / weights["S"])),
    `Ti-rich` = c(Ti = cmp["TiN", "factor"]),
    aluminate = c(Al = cmp["Al2O3", "factor"], Mg = cmp["MgO", "factor"], Ca = cmp["CaO", "factor"]),
    silicate = c(Si = cmp["SiO2", "factor"])
  )
}

#' Chemical family of each particle from its EDS analysis
#'
#' Each particle is assigned to the family with the largest
#' compound-equivalent mass: **sulfide** (S counted as MnS), **Ti-rich**
#' (Ti counted as TiN), **aluminate** (Al, Mg and Ca counted as Al2O3, MgO
#' and CaO) or **silicate** (Si counted as SiO2). A particle whose four
#' families together stay below `min_signal_pct` is mostly steel matrix and
#' is returned as `"unclassified"`, as is a particle with a missing reading.
#'
#' N, O and C are not used (peak overlaps Ti-N and Cr-O, carbon
#' contamination), so "Ti-rich" covers nitrides, carbonitrides and Ti
#' oxides alike. In Mo-bearing steels the S K line overlaps Mo L: small
#' particles can show false sulfur, which `min_signal_pct` screens out only
#' partly - check sulfides on a few spectra.
#'
#' @param data A data frame with element wt% columns.
#' @param element_cols Named character vector element -> column; default:
#'   detected with [inclusion_element_columns()].
#' @param min_signal_pct Smallest summed compound-equivalent (wt%) for a
#'   particle to be classified. Default 5.
#' @param weights Atomic weights, see [inclusion_atomic_weights()].
#' @return A data frame with one row per row of `data`: `family`,
#'   `family_share` (share of the winning family, 0 to 1) and `signal_pct`
#'   (summed compound-equivalent, wt%).
#' @export
classify_inclusion_family <- function(data, element_cols = NULL, min_signal_pct = 5,
                                      weights = inclusion_atomic_weights()) {
  if (is.null(element_cols)) element_cols <- inclusion_element_columns(names(data), weights)
  factors <- .family_factors(weights)
  used <- intersect(unlist(lapply(factors, names)), names(element_cols))
  if (length(used) == 0) {
    stop("None of the marker elements (S, Ti, Al, Mg, Ca, Si) has a wt% column, so the chemical family cannot be worked out.", call. = FALSE)
  }

  n <- nrow(data)
  if (n == 0) {
    return(data.frame(family = character(0), family_share = numeric(0), signal_pct = numeric(0),
                      stringsAsFactors = FALSE))
  }
  missing_reading <- rep(FALSE, n)
  equiv <- vapply(factors, function(f) {
    total <- numeric(n)
    for (el in intersect(names(f), used)) {
      v <- suppressWarnings(as.numeric(data[[element_cols[[el]]]]))
      missing_reading <<- missing_reading | is.na(v)
      total <- total + ifelse(is.na(v), 0, pmax(v, 0)) * f[[el]]
    }
    total
  }, numeric(n))
  if (n == 1) equiv <- matrix(equiv, nrow = 1, dimnames = list(NULL, names(factors)))

  signal <- rowSums(equiv)
  best <- max.col(equiv, ties.method = "first")
  family <- colnames(equiv)[best]
  share <- ifelse(signal > 0, equiv[cbind(seq_len(n), best)] / signal, NA_real_)
  unclassified <- missing_reading | !is.finite(signal) | signal < min_signal_pct
  family[unclassified] <- "unclassified"
  share[unclassified] <- NA_real_

  data.frame(family = family, family_share = share, signal_pct = signal, stringsAsFactors = FALSE)
}
