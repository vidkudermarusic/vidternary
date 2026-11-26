# ---- Stereology: oxygen, sulfur and nitrogen bound in the inclusions ----
# Area fraction = volume fraction (Delesse). With the density of the phase
# and of the steel this becomes a mass fraction, and with the formula of the
# phase the mass of O, S or N it holds. Comparing that with the bulk
# analysis of the steel (combustion / inert-gas fusion) shows whether the
# SEM run saw a representative amount of inclusions.

#' Phase taken for each chemical family in the mass balance
#'
#' One compound per family, with its density and the mass fractions of its
#' anion and cation worked out from the formula. The densities are handbook
#' values for the pure crystalline compounds (MnS alabandite 4.0, Al2O3
#' corundum 3.99, SiO2 quartz 2.65, TiN 5.40 g/cm3); real inclusions are
#' mixed phases, so edit the table when the phases are known.
#'
#' @param weights Atomic weights, see [inclusion_atomic_weights()].
#' @return A data frame: `family`, `compound`, `density_g_cm3`, `anion`,
#'   `anion_fraction`, `cation`, `cation_fraction`.
#' @export
inclusion_phase_table <- function(weights = inclusion_atomic_weights()) {
  cmp <- inclusion_compounds(weights)
  d <- data.frame(
    family = c("sulfide", "aluminate", "silicate", "Ti-rich"),
    compound = c("MnS", "Al2O3", "SiO2", "TiN"),
    density_g_cm3 = c(4.0, 3.99, 2.65, 5.40),
    stringsAsFactors = FALSE
  )
  rows <- cmp[d$compound, ]
  d$anion <- rows$anion
  d$anion_fraction <- rows$n_anion * unname(weights[rows$anion]) / rows$molar_mass
  d$cation <- rows$cation
  d$cation_fraction <- 1 - d$anion_fraction
  d
}

#' Mass of O, S and N bound in the inclusions
#'
#' @param area_ppm Named numeric vector: area fraction in ppm per chemical
#'   family (names as in `phases$family`; other names are ignored and
#'   listed in the `skipped` attribute of the result).
#' @param phases From [inclusion_phase_table()].
#' @param steel_density Density of the steel in g/cm3. Default 7.9.
#' @param measured Optional named numeric vector of bulk contents in mass
#'   ppm (e.g. `c(O = 25, S = 10)`) to compare with.
#' @return A list: `phases` (per family: volume ppm, mass ppm of the
#'   phase, and of its anion and cation) and `elements` (per element:
#'   `bound_ppm`, `measured_ppm`, `bound_share` = bound / measured).
#' @export
inclusion_mass_balance <- function(area_ppm, phases = inclusion_phase_table(), steel_density = 7.9,
                                   measured = NULL) {
  if (!is.finite(steel_density) || steel_density <= 0) stop("The steel density must be greater than zero.", call. = FALSE)
  known <- intersect(names(area_ppm), phases$family)
  p <- phases[match(known, phases$family), , drop = FALSE]
  v <- as.numeric(area_ppm[known]) / 1e6
  # Mass of each phase per mass of the whole (steel + inclusions).
  mix_density <- sum(v * p$density_g_cm3) + (1 - sum(v)) * steel_density
  mass <- v * p$density_g_cm3 / mix_density

  per_phase <- data.frame(
    family = p$family, compound = p$compound, density_g_cm3 = p$density_g_cm3,
    volume_ppm = v * 1e6, mass_ppm = mass * 1e6,
    anion = p$anion, anion_ppm = mass * p$anion_fraction * 1e6,
    cation = p$cation, cation_ppm = mass * p$cation_fraction * 1e6,
    stringsAsFactors = FALSE
  )
  if (length(known) == 0) {
    elements <- data.frame(element = character(0), bound_ppm = numeric(0), stringsAsFactors = FALSE)
  } else {
    bound <- tapply(c(per_phase$anion_ppm, per_phase$cation_ppm), c(per_phase$anion, per_phase$cation), sum)
    bound <- bound[order(!names(bound) %in% c("O", "S", "N"), names(bound))]
    elements <- data.frame(element = names(bound), bound_ppm = as.numeric(bound), stringsAsFactors = FALSE)
  }
  elements$measured_ppm <- if (is.null(measured)) rep(NA_real_, nrow(elements)) else
    unname(as.numeric(measured[elements$element]))
  elements$bound_share <- elements$bound_ppm / elements$measured_ppm

  out <- list(phases = per_phase, elements = elements)
  attr(out, "skipped") <- setdiff(names(area_ppm), phases$family)
  out
}
