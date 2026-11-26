# ---- Fatigue limit from the largest inclusion (Murakami's sqrt(area) model) ----
# Murakami & Endo: the fatigue limit of a steel that fails from a small
# defect depends only on the Vickers hardness of the matrix and on
# sqrt(area) of the defect projected on the plane normal to the stress:
#
#   sigma_w = C * (HV + 120) / sqrt(area)^(1/6) * ((1 - R) / 2)^alpha
#
# with sigma_w in MPa, sqrt(area) in um, HV in kgf/mm2, alpha = 0.226 +
# HV * 1e-4, and C = 1.43 for a defect at the surface, 1.41 for one just
# below the surface (touching it) and 1.56 for an internal one. Murakami
# gives the model as accurate to about 10 % for HV 70 to 720 and sqrt(area)
# below about 1000 um.
# Source: Murakami & Endo, Int. J. Fatigue 16 (1994) 163-182; Murakami,
# "Metal Fatigue: Effects of Small Defects and Nonmetallic Inclusions"
# (Elsevier, 2002). The three constants, the units and the stated accuracy
# were checked on 2026-11-11 against open-access papers that quote the model
# (https://pmc.ncbi.nlm.nih.gov/articles/PMC8345296, and for the R-ratio
# term an ECF13 conference paper), not against the original publications.

.murakami_location_factor <- c(surface = 1.43, subsurface = 1.41, internal = 1.56)

#' Fatigue limit from sqrt(area) of the largest inclusion (Murakami)
#'
#' @param sqrt_area_um Square root of the inclusion's projected area (um).
#' @param hv Vickers hardness of the matrix (kgf/mm2).
#' @param location Where the inclusion lies: `"surface"`, `"subsurface"`
#'   (just below the surface, touching it) or `"internal"`.
#' @param stress_ratio Stress ratio R = minimum / maximum stress. Default
#'   -1 (fully reversed loading).
#' @return Fatigue limit (stress amplitude) in MPa, same length as
#'   `sqrt_area_um`.
#' @export
murakami_fatigue_limit <- function(sqrt_area_um, hv, location = c("surface", "subsurface", "internal"),
                                   stress_ratio = -1) {
  location <- match.arg(location)
  if (length(hv) != 1 || !is.finite(hv) || hv <= 0) stop("Hardness HV must be a number greater than zero.", call. = FALSE)
  if (length(stress_ratio) != 1 || !is.finite(stress_ratio) || stress_ratio >= 1) {
    stop("The stress ratio R must be smaller than 1.", call. = FALSE)
  }
  if (any(!is.finite(sqrt_area_um) | sqrt_area_um <= 0)) stop("sqrt(area) must be greater than zero.", call. = FALSE)
  alpha <- 0.226 + hv * 1e-4
  .murakami_location_factor[[location]] * (hv + 120) / sqrt_area_um^(1 / 6) * ((1 - stress_ratio) / 2)^alpha
}

#' Notes on whether the Murakami model applies
#'
#' @param sqrt_area_um Largest sqrt(area) the model is applied to (um).
#' @param hv Vickers hardness.
#' @return Character vector of notes (empty when inside the stated range).
#' @export
murakami_validity_notes <- function(sqrt_area_um, hv) {
  c(
    if (is.finite(hv) && (hv < 70 || hv > 720))
      sprintf("HV = %g is outside the range the model was checked for (about HV 70 to 720).", hv),
    if (any(is.finite(sqrt_area_um) & sqrt_area_um > 1000))
      "sqrt(area) above about 1000 um: the defect is no longer 'small' and the model overestimates the fatigue limit."
  )
}

#' Fatigue limit for an extreme-value prediction of the largest inclusion
#'
#' Applies [murakami_fatigue_limit()] to the return level of
#' [predict_evs_max()] and to the ends of its two intervals. A larger
#' inclusion gives a lower fatigue limit, so the upper end of a size
#' interval becomes the lower end of the fatigue-limit interval.
#'
#' The fatigue limit is the lower bound for the inspected material only if
#' the return period matches the stressed volume of the part, see
#' [evs_volume_return_period()].
#'
#' @param prediction From [predict_evs_max()].
#' @param hv,location,stress_ratio See [murakami_fatigue_limit()].
#' @return A data frame with three rows (return level, its confidence
#'   interval, interval for the largest inclusion): `quantity`,
#'   `sqrt_area_um`, `sqrt_area_lower`, `sqrt_area_upper`,
#'   `fatigue_limit_mpa`, `fatigue_lower`, `fatigue_upper`.
#' @export
evs_fatigue_limit <- function(prediction, hv, location = c("surface", "subsurface", "internal"),
                              stress_ratio = -1) {
  location <- match.arg(location)
  f <- function(s) murakami_fatigue_limit(pmax(s, .Machine$double.eps), hv, location, stress_ratio)
  lvl <- round(100 * prediction$level)
  data.frame(
    quantity = c(sprintf("Return level for T = %g control areas", prediction$return_period),
                 sprintf("%d%% confidence interval of the return level", lvl),
                 sprintf("%d%% interval for the largest inclusion over T control areas", lvl)),
    sqrt_area_um = c(prediction$predicted, NA, NA),
    sqrt_area_lower = c(NA, prediction$ci_lower, prediction$lower),
    sqrt_area_upper = c(NA, prediction$ci_upper, prediction$upper),
    fatigue_limit_mpa = c(f(prediction$predicted), NA, NA),
    fatigue_lower = c(NA, f(prediction$ci_upper), f(prediction$upper)),
    fatigue_upper = c(NA, f(max(prediction$ci_lower, .Machine$double.eps)), f(max(prediction$lower, .Machine$double.eps))),
    stringsAsFactors = FALSE
  )
}

#' Return period that matches a stressed volume
#'
#' Murakami's rule for using a 2D inspection to predict the largest
#' inclusion in a volume: one control area `S0` stands for the control
#' volume `V0 = S0 * h`, where `h` is the mean of the block maxima
#' sqrt(area). The return period for a stressed volume `V` is `T = V / V0`.
#'
#' @param volume_mm3 Stressed volume of the part or test piece (mm3).
#' @param control_area_mm2 Area of one control area `S0` (mm2).
#' @param mean_sqrt_area_max_um Mean of the block maxima sqrt(area) (um).
#' @return A list: `return_period`, `control_volume_mm3`, `thickness_um`.
#' @export
evs_volume_return_period <- function(volume_mm3, control_area_mm2, mean_sqrt_area_max_um) {
  ok <- function(v) length(v) == 1 && is.finite(v) && v > 0
  if (!ok(volume_mm3) || !ok(control_area_mm2) || !ok(mean_sqrt_area_max_um)) {
    stop("Volume, control area and mean sqrt(area) must all be greater than zero.", call. = FALSE)
  }
  v0 <- control_area_mm2 * mean_sqrt_area_max_um / 1000
  list(return_period = volume_mm3 / v0, control_volume_mm3 = v0, thickness_um = mean_sqrt_area_max_um)
}
