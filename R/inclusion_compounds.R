# ---- Inclusion add-on: element wt% -> compounds (oxides, sulfides, nitrides) ----

#' Resolve element wt% into oxide, sulfide and nitride amounts
#'
#' Assignment order, per particle:
#' 1. **Sulfur** goes to the elements in `s_order` in turn (one S per metal
#'    atom): with `c("Ca", "Mn")` first CaS, then MnS. Sulfur left over
#'    stays in `other`.
#' 2. **Titanium/nitrogen**, by `ti_as`: `"TiN"` counts all Ti as TiN (the
#'    measured N, unreliable in EDS next to Ti, is not used); `"TiO2"`
#'    counts all Ti as TiO2; `"by_N"` assigns the measured N to the
#'    elements in `n_order` (TiN, then AlN) and the rest of Ti to TiO2.
#' 3. **Remaining metals** become oxides at fixed ratios (Al2O3, MgO, CaO,
#'    SiO2, MnO, TiO2, Cr2O3, ZrO2). Oxygen is calculated from these, not
#'    taken from the measurement.
#'
#' Elements that form none of these compounds (e.g. Mo, V, Ni, Pb) stay in
#' `other` as elements. C and O, `ignore_elements`, and (unless
#' `ti_as = "by_N"`) N are not used. The order is a convention and should
#' be stated when results are reported.
#'
#' @param wt Data frame of (matrix-corrected) element wt%, columns named by
#'   element symbol. Rows containing `NA` give `NA` results.
#' @param s_order Elements that take sulfur, in order.
#' @param ti_as `"TiN"`, `"TiO2"` or `"by_N"`.
#' @param n_order Elements that take nitrogen when `ti_as = "by_N"`.
#' @param ignore_elements Elements never used (default carbon).
#' @param weights Atomic weights.
#' @return A list: `moles` and `mass` (data frames, one column per
#'   compound), `other` (wt% of elements left as elements), `total`
#'   (`mass` summed plus `other`), `o_calculated_pct` (oxygen in the oxides,
#'   as % of `total`) and `o_measured_pct` (the O column of `wt`, if any)
#'   as a plausibility check.
#' @export
elements_to_compounds <- function(wt, s_order = c("Ca", "Mn"),
                                  ti_as = c("TiN", "TiO2", "by_N"),
                                  n_order = c("Ti", "Al"), ignore_elements = "C",
                                  weights = inclusion_atomic_weights()) {
  ti_as <- match.arg(ti_as)
  cmp <- inclusion_compounds(weights)
  w <- as.matrix(wt)
  storage.mode(w) <- "double"
  unknown <- setdiff(colnames(w), names(weights))
  if (length(unknown) > 0) stop("No atomic weight for: ", paste(unknown, collapse = ", "), call. = FALSE)
  n <- nrow(w)
  zero <- rep(0, n)

  mol <- lapply(setNames(names(weights), names(weights)), function(e) {
    if (e %in% colnames(w)) unname(w[, e]) / weights[[e]] else zero
  })
  cm <- setNames(rep(list(zero), nrow(cmp)), cmp$compound)

  assign_anion <- function(order, anion, suffix) {
    rem <- mol[[anion]]
    for (cat in order) {
      name <- paste0(cat, suffix)
      if (!name %in% cmp$compound) stop("No compound ", name, " is defined.", call. = FALSE)
      take <- pmin(mol[[cat]], rem)
      cm[[name]] <<- cm[[name]] + take
      mol[[cat]] <<- mol[[cat]] - take
      rem <- rem - take
    }
    rem
  }

  s_left <- assign_anion(s_order, "S", "S")
  n_left <- zero
  if (ti_as == "by_N") {
    n_left <- assign_anion(n_order, "N", "N")
  } else if (ti_as == "TiN") {
    cm[["TiN"]] <- mol[["Ti"]]
    mol[["Ti"]] <- zero
  }
  for (i in which(cmp$kind == "oxide")) {
    cat <- cmp$cation[i]
    cm[[cmp$compound[i]]] <- cm[[cmp$compound[i]]] + mol[[cat]] / cmp$n_cation[i]
    mol[[cat]] <- zero
  }

  moles <- as.data.frame(cm)
  mass <- as.data.frame(Map(function(x, mm) x * mm, cm, cmp$molar_mass))
  names(moles) <- names(mass) <- cmp$compound

  used <- unique(c("O", "S", "N", cmp$cation, ignore_elements))
  keep_el <- setdiff(colnames(w), used)
  other <- if (length(keep_el) > 0) unname(rowSums(w[, keep_el, drop = FALSE])) else zero
  other <- other + s_left * weights[["S"]] + n_left * weights[["N"]]

  o_mass <- unname(rowSums(as.matrix(moles[, cmp$kind == "oxide", drop = FALSE]) *
                             rep(cmp$n_anion[cmp$kind == "oxide"], each = n))) * weights[["O"]]
  total <- unname(rowSums(mass)) + other
  list(
    moles = moles, mass = mass, other = other, total = total,
    o_calculated_pct = 100 * o_mass / total,
    o_measured_pct = if ("O" %in% colnames(w)) unname(w[, "O"]) else rep(NA_real_, n),
    settings = list(s_order = s_order, ti_as = ti_as, n_order = n_order, ignore_elements = ignore_elements)
  )
}

#' Compounds belonging to a diagram corner
#'
#' A corner is one compound (`"Al2O3"`), several summed with `+`
#' (`"CaO+MgO"`), or a group: `"@oxides"`, `"@sulfides"`, `"@nitrides"`.
#'
#' @param token Corner definition.
#' @param compounds Table from [inclusion_compounds()].
#' @return Character vector of compound names.
#' @export
corner_compounds <- function(token, compounds = inclusion_compounds()) {
  if (startsWith(token, "@")) {
    kind <- sub("s$", "", substring(token, 2))
    if (!kind %in% compounds$kind) stop("Unknown compound group ", token, call. = FALSE)
    return(compounds$compound[compounds$kind == kind])
  }
  parts <- trimws(strsplit(token, "+", fixed = TRUE)[[1]])
  bad <- setdiff(parts, compounds$compound)
  if (length(bad) > 0) stop("Unknown compound(s) in corner '", token, "': ", paste(bad, collapse = ", "), call. = FALSE)
  parts
}
