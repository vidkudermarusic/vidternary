# ---- Inclusion add-on: atomic weights, compound definitions, column parsing ----
# Building blocks for turning element wt% (SEM/EDS) into the compounds
# (oxides, sulfides, nitrides) that inclusion ternary diagrams are drawn in.
# Nothing here touches the existing ternary pipeline.

#' Standard atomic weights used by the inclusion add-on
#'
#' CIAAW standard atomic weights (2024 table); for elements whose weight is
#' given as an interval (C, N, O, Mg, Si, S, Pb) the conventional value in
#' common use is taken. Source: <https://www.ciaaw.org/atomic-weights.htm>.
#'
#' @return A named numeric vector, element symbol -> g/mol.
#' @export
inclusion_atomic_weights <- function() {
  c(C = 12.011, N = 14.007, O = 15.999, Mg = 24.305, Al = 26.9815384,
    Si = 28.085, S = 32.06, Ca = 40.078, Ti = 47.867, V = 50.9415,
    Cr = 51.9961, Mn = 54.938043, Fe = 55.845, Co = 58.933194,
    Ni = 58.6934, Cu = 63.546, Zr = 91.222, Nb = 92.90637, Mo = 95.95,
    Sn = 118.710, W = 183.84, Pb = 207.2, Bi = 208.98040)
}

#' Compounds an inclusion analysis can be resolved into
#'
#' One row per compound: its cation and anion with their counts per formula
#' unit, its kind, molar mass and `factor` (mass of compound per unit mass
#' of cation, e.g. 1.8894 for Al2O3 from Al). Oxides use the formula units
#' Al2O3, Cr2O3 (not AlO1.5), so mole fractions are per formula unit.
#'
#' @param weights Atomic weights, see [inclusion_atomic_weights()].
#' @return A data frame: `compound`, `cation`, `n_cation`, `anion`,
#'   `n_anion`, `kind`, `molar_mass`, `factor`.
#' @export
inclusion_compounds <- function(weights = inclusion_atomic_weights()) {
  d <- data.frame(
    compound = c("Al2O3", "MgO", "CaO", "SiO2", "MnO", "TiO2", "Cr2O3", "ZrO2",
                 "MnS", "CaS", "TiN", "AlN"),
    cation   = c("Al", "Mg", "Ca", "Si", "Mn", "Ti", "Cr", "Zr",
                 "Mn", "Ca", "Ti", "Al"),
    n_cation = c(2, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1),
    anion    = c("O", "O", "O", "O", "O", "O", "O", "O", "S", "S", "N", "N"),
    n_anion  = c(3, 1, 1, 2, 1, 2, 3, 2, 1, 1, 1, 1),
    kind     = c(rep("oxide", 8), rep("sulfide", 2), rep("nitride", 2)),
    stringsAsFactors = FALSE
  )
  d$molar_mass <- d$n_cation * weights[d$cation] + d$n_anion * weights[d$anion]
  d$factor <- d$molar_mass / (d$n_cation * weights[d$cation])
  rownames(d) <- d$compound
  d$molar_mass <- unname(d$molar_mass)
  d$factor <- unname(d$factor)
  d
}

#' Find the element wt% columns in an EDS export
#'
#' A column counts as an element column when it starts with an element
#' symbol followed by a non-letter (`"Al.(Wt%)"`, `"Al (wt%)"`) and its name
#' contains "wt", or when the whole name is the symbol (`"Al"`). Names such
#' as `"Feature"`, `"Area.(sq..um)"` or `"Spectrum.Area"` are not matched.
#'
#' @param col_names Character vector of column names.
#' @param weights Atomic weights; only symbols in it are recognised.
#' @return A named character vector: element symbol -> column name.
#' @export
inclusion_element_columns <- function(col_names, weights = inclusion_atomic_weights()) {
  m <- regmatches(col_names, regexec("^([A-Z][a-z]?)(?:[^A-Za-z].*)?$", col_names, perl = TRUE))
  sym <- vapply(m, function(x) if (length(x) == 2) x[2] else NA_character_, character(1))
  ok <- !is.na(sym) & sym %in% names(weights) &
    (col_names == sym | grepl("wt", col_names, ignore.case = TRUE))
  out <- setNames(col_names[ok], sym[ok])
  dup <- names(out)[duplicated(names(out))]
  if (length(dup) > 0) {
    stop("More than one column matches element ", paste(unique(dup), collapse = ", "),
         "; name the columns explicitly with `element_cols`.", call. = FALSE)
  }
  out
}
