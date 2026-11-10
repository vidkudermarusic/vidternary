# Helpers for the inclusion add-on tests: synthetic EDS analyses built from
# known compounds, so the expected answer of every step is known exactly.

INCL_ELEMENTS <- c("C", "N", "O", "Mg", "Al", "Si", "S", "Ca", "Ti", "V", "Cr", "Mn", "Fe", "Ni", "Zr", "Mo")

# One EDS-like row (wt%, columns "Mg.(Wt%)" ...): an inclusion made of
# `compounds` (named masses), plus `steel` mass of steel with composition
# `steel_comp`, plus `carbon` mass; normalised to 100 wt%.
incl_row <- function(compounds = list(), steel = 0, carbon = 0,
                     steel_comp = c(Fe = 70, Cr = 20, Ni = 10)) {
  cmp <- inclusion_compounds()
  el <- setNames(rep(0, length(INCL_ELEMENTS)), INCL_ELEMENTS)
  for (nm in names(compounds)) {
    r <- cmp[cmp$compound == nm, ]
    cat_mass <- compounds[[nm]] / r$factor
    el[r$cation] <- el[r$cation] + cat_mass
    el[r$anion] <- el[r$anion] + compounds[[nm]] - cat_mass
  }
  el[names(steel_comp)] <- el[names(steel_comp)] + steel * steel_comp / sum(steel_comp)
  el["C"] <- el["C"] + carbon
  el <- 100 * el / sum(el)
  out <- as.data.frame(as.list(el))
  names(out) <- paste0(INCL_ELEMENTS, ".(Wt%)")
  out
}

incl_data <- function(...) {
  d <- do.call(rbind, list(...))
  d$Area <- seq_len(nrow(d))
  d
}

INCL_STEEL <- c(Fe = 70, Cr = 20, Ni = 10)
