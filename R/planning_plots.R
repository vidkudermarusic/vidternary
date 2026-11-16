# ---- Measurement planning: plots and display tables ----

#' Precision of the number density against the inspected area
#'
#' The curve of [plan_measurement()] with the pilot run marked as a point
#' and the wanted precision as a dashed line. Both axes are logarithmic:
#' halving the interval takes four times the area.
#'
#' @param result From [plan_measurement()].
#' @return A `ggplot` object.
#' @export
create_precision_curve <- function(result) {
  p <- result$pilot
  target <- result$settings$target_precision_pct
  curve <- result$curve
  curve$model <- factor(curve$model, levels = unique(curve$model))
  ggplot2::ggplot(curve, ggplot2::aes(x = area_mm2, y = 100 * rel_halfwidth, linetype = model)) +
    ggplot2::geom_line(colour = "#357ABD", linewidth = 0.8) +
    ggplot2::geom_hline(yintercept = target, linetype = "dashed", colour = "grey40") +
    ggplot2::annotate("point", x = p$inspected_area_mm2, y = 100 * p$rel_halfwidth_now, size = 3.5, colour = "#002147") +
    ggplot2::scale_x_log10() + ggplot2::scale_y_log10() +
    ggplot2::labs(x = "Inspected area (mm2)", y = "Half-width of the 95% interval (% of the density)", linetype = NULL,
                  title = "Precision of the number density",
                  subtitle = sprintf("Point = the pilot run, dashed line = wanted precision (%g %%)", target)) +
    ggplot2::theme_minimal() + ggplot2::theme(legend.position = "bottom")
}

#' Display versions of the measurement-plan tables
#'
#' @param res From [plan_measurement()].
#' @return A named list of data frames with readable headers and rounded
#'   numbers: `pilot`, `precision`, `ratio`, `evs`, `runs`,
#'   `repeat_density`, `repeat_other`.
#' @export
planning_display_tables <- function(res) {
  p <- res$pilot
  dash <- function(v, fmt) ifelse(is.na(v), "-", sprintf(fmt, v))
  area_fmt <- function(a) ifelse(a >= 100, sprintf("%.0f", a), sprintf("%.3g", a))
  pilot <- data.frame(
    Quantity = c("Runs", "Particles", "Inspected area (mm2)", "Particles per mm2", "95% interval (Poisson)",
                 "SEM fields", "Area of one field (mm2)", "Index of dispersion (counts per field)",
                 "Precision now (half-width of the 95% interval)",
                 "Smallest density ratio found between two batches inspected on this area each"),
    Value = c(p$runs, p$n, sprintf("%.2f", p$inspected_area_mm2), sprintf("%.1f", p$per_mm2),
              sprintf("[%.1f, %.1f]", p$per_mm2_lower, p$per_mm2_upper), dash(p$n_fields, "%d"),
              dash(p$field_area_mm2, "%.4g"), dash(p$field_dispersion, "%.2f"),
              sprintf("+/- %.1f %%", 100 * p$rel_halfwidth_now),
              if (is.finite(p$detectable_ratio_now)) sprintf("%.2f (or 1 / %.2f)", p$detectable_ratio_now, p$detectable_ratio_now) else "none"),
    check.names = FALSE, stringsAsFactors = FALSE)

  plan_cols <- function(d) data.frame(
    `Area (mm2)` = area_fmt(d$area_mm2), `SEM fields` = dash(d$fields, "%.0f"),
    `Particles expected` = sprintf("%.0f", d$particles_expected),
    `Times the pilot area` = sprintf("%.2g", d$times_pilot_area),
    ` ` = ifelse(d$chosen, "<- your target", ""), check.names = FALSE, stringsAsFactors = FALSE)
  precision <- cbind(`Precision (half-width of the 95% interval)` = sprintf("+/- %g %%", res$precision$precision_pct),
                     plan_cols(res$precision), stringsAsFactors = FALSE)
  ratio <- cbind(`Density ratio to detect` = sprintf("%g", res$ratio$ratio), plan_cols(res$ratio), stringsAsFactors = FALSE)
  names(ratio)[2] <- "Area per batch (mm2)"
  names(ratio)[3] <- "SEM fields per batch"
  names(ratio)[4] <- "Particles expected (reference batch)"

  e <- res$evs
  evs <- if (is.null(e)) data.frame(Note = res$evs_problem %||% "Needs a SEM field ID column.") else data.frame(
    `Control areas` = e$n_areas, `Area (mm2)` = area_fmt(e$area_mm2), `Times the pilot` = sprintf("%g", e$times_pilot),
    `Return level sqrt(area) (um)` = sprintf("%.2f", e$return_level),
    `Half-width of the 95% range (um)` = sprintf("%.2f", e$halfwidth_um),
    `Half-width (%)` = sprintf("%.0f", 100 * e$rel_halfwidth), check.names = FALSE, stringsAsFactors = FALSE)

  r <- res$per_run
  runs <- data.frame(Run = r$run, Particles = r$n, `Inspected area (mm2)` = sprintf("%.2f", r$inspected_area_mm2),
                     `Per mm2` = sprintf("%.1f", r$per_mm2), `Area fraction (ppm)` = sprintf("%.0f", r$area_ppm),
                     `Median ECD (um)` = sprintf("%.2f", r$median_ecd_um), `90th percentile (um)` = sprintf("%.2f", r$p90_ecd_um),
                     `Largest sqrt(area) (um)` = sprintf("%.2f", r$max_sqrt_area_um),
                     `Index of dispersion` = dash(r$field_dispersion, "%.2f"), check.names = FALSE, stringsAsFactors = FALSE)

  one_run <- data.frame(Note = "Upload two or more runs of the same specimen to see their repeatability.")
  rp <- res$repeatability
  if (is.null(rp)) {
    repeat_density <- repeat_other <- one_run
  } else {
    d <- rp$density
    repeat_density <- data.frame(
      Quantity = c("Mean number density (per mm2)", "Scatter between the runs (standard deviation, per mm2)",
                   "Scatter expected from counting alone (per mm2)", "Scatter beyond counting (per mm2)",
                   "Do the runs differ by more than counting scatter? (chi-squared, p)",
                   "Repeatability limit: two runs differ by less than"),
      Value = c(sprintf("%.1f", d$mean_per_mm2), sprintf("%.2f (%.1f %%)", d$sd_observed, 100 * d$cv_observed),
                sprintf("%.2f (%.1f %%)", d$sd_counting, 100 * d$cv_counting), sprintf("%.2f", d$sd_excess),
                .format_p(d$p_value), sprintf("%.1f per mm2 (%.0f %%)", d$limit_per_mm2, 100 * d$limit_rel)),
      check.names = FALSE, stringsAsFactors = FALSE)
    q <- rp$quantities
    repeat_other <- data.frame(Quantity = q$quantity, Mean = sprintf("%.3g", q$mean),
                               `Standard deviation between runs` = sprintf("%.3g", q$sd),
                               `Coefficient of variation (%)` = sprintf("%.1f", 100 * q$cv),
                               Range = sprintf("%.3g - %.3g", q$min, q$max),
                               `Repeatability limit` = sprintf("%.3g (%.0f %%)", q$limit, 100 * q$limit_rel),
                               check.names = FALSE, stringsAsFactors = FALSE)
  }
  list(pilot = pilot, precision = precision, ratio = ratio, evs = evs, runs = runs,
       repeat_density = repeat_density, repeat_other = repeat_other)
}
