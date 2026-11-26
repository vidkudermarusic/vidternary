# ---- Reproducibility: report and script for the Extreme Value Analysis tab ----

#' R script that repeats an extreme-value analysis
#'
#' @param file_name Name of the data file.
#' @param area_col,field_col Area and SEM field ID columns.
#' @param fields_per_area SEM fields per control area.
#' @param return_period Return period T in control areas.
#' @param filters Pre-analysis filters, column -> condition (may be empty).
#' @param hv Vickers hardness for the fatigue estimate, or `NULL` / `NA`.
#' @param location,stress_ratio See [murakami_fatigue_limit()].
#' @return The script as a character vector of lines.
#' @export
evs_script <- function(file_name, area_col, field_col, fields_per_area = 1, return_period = 100,
                       filters = list(), hv = NULL, location = "surface", stress_ratio = -1) {
  with_fatigue <- !is.null(hv) && length(hv) == 1 && is.finite(hv) && hv > 0
  c(.script_header("Extreme value analysis", "Extreme Value Analysis"),
    sprintf("file <- %s", .r_literal(file_name)),
    "all_data <- read_particle_table(file)",
    if (length(filters)) sprintf("data <- apply_pre_filters(all_data, %s)", .r_literal(filters)) else "data <- all_data",
    "",
    sprintf("area_col <- %s", .r_literal(area_col)),
    sprintf("field_col <- %s", .r_literal(field_col)),
    sprintf("areas <- assign_control_areas(data[[field_col]], %s, all_fields = all_data[[field_col]])", .r_literal(fields_per_area)),
    if (fields_per_area > 1) c("data$.control_area <- areas$control_area",
                               "maxima <- compute_block_maxima(data, area_col, \".control_area\")")
    else "maxima <- compute_block_maxima(data, area_col, field_col)",
    "fit <- fit_evs_gumbel(maxima$sqrt_area_max)",
    "gof <- gumbel_goodness_of_fit(fit)",
    sprintf("prediction <- predict_evs_max(fit, %s)", .r_literal(return_period)),
    "",
    "prediction[c(\"predicted\", \"ci_lower\", \"ci_upper\", \"lower\", \"upper\")]",
    "gof$p_value",
    "create_gumbel_plot(fit, prediction)",
    if (with_fatigue) sprintf("evs_fatigue_limit(prediction, hv = %s, location = %s, stress_ratio = %s)",
                              .r_literal(hv), .r_literal(location), .r_literal(stress_ratio)))
}

#' HTML report of an extreme-value analysis
#'
#' @param fit The fit of the EVS tab: a result of [fit_evs_gumbel()] with
#'   `block_maxima`, `gof`, `fields_per_area`, `n_fields`,
#'   `n_control_areas` and `area_notes` added.
#' @param prediction From [predict_evs_max()].
#' @param fatigue From [evs_fatigue_limit()], or `NULL`.
#' @param provenance Data frame from [report_provenance()], or `NULL`.
#' @param script Script lines from [evs_script()], or `NULL`.
#' @param filters Pre-analysis filters that were applied.
#' @return The HTML document as one string (see [build_html_report()]).
#' @export
evs_report <- function(fit, prediction, fatigue = NULL, provenance = NULL, script = NULL, filters = list()) {
  lvl <- round(100 * prediction$level)
  summary <- data.frame(
    Quantity = c("Fields inspected", "Fields per control area", "Control areas inspected", "Control areas in the fit",
                 "Intercept a (um)", "Slope b (um)", "R2",
                 "Anderson-Darling goodness-of-fit, p (bootstrap)",
                 "Return period T (control areas)", "Return level sqrt(area) (um)",
                 sprintf("%d%% confidence interval of the return level", lvl),
                 sprintf("%d%% interval for the largest inclusion over T control areas", lvl),
                 "Largest sqrt(area) observed (um)"),
    Value = c(format(fit$n_fields %||% NA), format(fit$fields_per_area %||% NA), format(fit$n_control_areas %||% NA),
              format(fit$n), sprintf("%.3f", fit$intercept), sprintf("%.3f", fit$slope), sprintf("%.3f", fit$r_squared),
              if (is.null(fit$gof)) "not available" else sprintf("%.3f", fit$gof$p_value),
              format(prediction$return_period), sprintf("%.2f", prediction$predicted),
              sprintf("[%.2f, %.2f]", prediction$ci_lower, prediction$ci_upper),
              sprintf("[%.2f, %.2f]", prediction$lower, prediction$upper),
              sprintf("%.2f", max(fit$data$sqrt_area_max))),
    stringsAsFactors = FALSE)
  filter_table <- if (length(filters)) data.frame(`Filter column` = names(filters), Condition = unlist(filters, use.names = FALSE),
                                                  check.names = FALSE, stringsAsFactors = FALSE)
  warnings <- c(fit$area_notes$warnings,
                if (!is.null(fit$gof) && isTRUE(fit$gof$reject_at_05))
                  "The goodness-of-fit test rejects a single Gumbel distribution at the 5% level: the block maxima may come from more than one population, and the extrapolation may understate the tail.")
  fatigue_table <- if (!is.null(fatigue)) data.frame(
    Quantity = fatigue$quantity,
    `sqrt(area) (um)` = ifelse(is.na(fatigue$sqrt_area_um), sprintf("[%.2f, %.2f]", fatigue$sqrt_area_lower, fatigue$sqrt_area_upper),
                               sprintf("%.2f", fatigue$sqrt_area_um)),
    `Fatigue limit, stress amplitude (MPa)` = ifelse(is.na(fatigue$fatigue_limit_mpa),
                                                     sprintf("[%.0f, %.0f]", fatigue$fatigue_lower, fatigue$fatigue_upper),
                                                     sprintf("%.0f", fatigue$fatigue_limit_mpa)),
    check.names = FALSE, stringsAsFactors = FALSE)

  sections <- list(
    list(title = "Data and settings", content = list(provenance, filter_table)),
    list(title = "What to keep in mind", content = list(
      report_warning(warnings), report_list(c(fit$area_notes$notes,
        "The return level is the size exceeded on average once per T control areas. The largest inclusion actually found in T control areas lies above it in about 63% of cases; compare it with the interval for the largest inclusion.")))),
    list(title = "Gumbel fit and prediction", content = list(create_gumbel_plot(fit, prediction), summary)),
    list(title = "Block maxima", content = list(fit$block_maxima))
  )
  if (!is.null(fatigue_table)) {
    sections <- c(sections, list(list(title = "Fatigue limit estimate (Murakami)", content = list(
      "sigma_w = C (HV + 120) / sqrt(area)^(1/6), C = 1.43 at the surface, 1.41 just below it, 1.56 inside the part; stated as accurate to about 10% for HV 70 to 720.",
      fatigue_table))))
  }
  sections <- c(sections, list(list(title = "Methods", content = list(report_list(c(
    "Murakami (1994), ASTM E2283: block maxima of sqrt(area), Gumbel probability plot with plotting position j / (n + 1), least-squares line.",
    "Intervals: parametric bootstrap of the fitted line; goodness of fit: Anderson-Darling statistic calibrated by parametric bootstrap.",
    if (!is.null(fatigue_table)) "Fatigue limit: Murakami & Endo (1994), sqrt(area) parameter model."))))))
  if (!is.null(script)) {
    sections <- c(sections, list(list(title = "R script that repeats this analysis", content = list(report_code(script)))))
  }
  build_html_report("Extreme value analysis report", sections,
                    subtitle = sprintf("%d control areas, return level %.2f um for T = %s", fit$n, prediction$predicted,
                                       format(prediction$return_period)),
                    footer = .report_footer())
}
