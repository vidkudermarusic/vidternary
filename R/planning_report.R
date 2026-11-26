# ---- Reproducibility: report and script for the Measurement Planning tab ----
# Same shape as the other tabs' reports (cleanliness_report.R,
# batch_report.R): an HTML document from build_html_report() and an R script
# that repeats the plan without the app.

#' R script that repeats a measurement plan
#'
#' @param file_names Names of the data files, one per run.
#' @param cols The column list given to [plan_measurement()].
#' @param settings The settings of the finished plan (`result$settings`), so
#'   every value - the inspected areas included - is written out.
#' @param filters Pre-analysis filters, column -> condition (may be empty).
#' @param run_names Names of the runs as the app shows them.
#' @return The script as a character vector of lines.
#' @export
planning_script <- function(file_names, cols, settings, filters = list(),
                            run_names = batch_labels(file_names)) {
  cols <- cols[!vapply(cols, is.null, logical(1))]
  c(.script_header("Measurement plan", "Measurement Planning"),
    sprintf("files <- %s", .r_literal(file_names)),
    "all_runs <- lapply(files, read_particle_table)",
    sprintf("names(all_runs) <- %s", .r_literal(run_names)),
    if (length(filters)) sprintf("runs <- lapply(all_runs, apply_pre_filters, filters = %s)", .r_literal(filters))
    else "runs <- all_runs",
    "",
    sprintf("cols <- %s", .r_literal(cols)),
    sprintf("settings <- %s", .r_literal(settings)),
    "result <- plan_measurement(runs, cols, settings, all_runs = all_runs)",
    "",
    "result$pilot                  # what the pilot run(s) give now",
    "result$precision              # area for a wanted precision of the number density",
    "result$ratio                  # area per batch to detect a density ratio",
    "result$evs                    # largest inclusion: precision with more control areas",
    "result$repeatability$density  # repeated runs (NULL for one run)",
    "result$notes                  # warnings and notes",
    "# writexl::write_xlsx(planning_result_tables(result), \"measurement_plan.xlsx\")")
}

#' HTML report of a measurement plan
#'
#' @param result From [plan_measurement()].
#' @param provenance Data frame from [report_provenance()], or `NULL`.
#' @param script Script lines from [planning_script()], or `NULL`.
#' @param filters Pre-analysis filters that were applied.
#' @return The HTML document as one string (see [build_html_report()]).
#' @export
planning_report <- function(result, provenance = NULL, script = NULL, filters = list()) {
  tab <- planning_display_tables(result)
  st <- result$settings
  p <- result$pilot
  settings <- data.frame(Setting = names(st),
                         Value = vapply(st, function(v) if (is.null(v)) "" else paste(format(v), collapse = ", "), character(1)),
                         stringsAsFactors = FALSE, row.names = NULL)
  if (length(filters)) {
    settings <- rbind(settings, data.frame(Setting = paste("filter:", names(filters)), Value = unlist(filters, use.names = FALSE),
                                           stringsAsFactors = FALSE))
  }
  target <- result$precision[result$precision$chosen, , drop = FALSE]

  sections <- list(
    list(title = "Data and settings", content = list(provenance, settings)),
    list(title = "What to keep in mind", content = list(report_warning(result$notes$warnings), report_list(result$notes$notes))),
    list(title = "The pilot measurement", content = list(
      sprintf("%d particles on %.2f mm2: %.1f per mm2, known to +/- %.1f %% now. For +/- %g %%: %.3g mm2 (%.1f times the pilot area).",
              p$n, p$inspected_area_mm2, p$per_mm2, 100 * p$rel_halfwidth_now, target$precision_pct[1],
              target$area_mm2[1], target$times_pilot_area[1]),
      tab$pilot)),
    list(title = "Number density", content = list(
      create_precision_curve(result),
      "Area needed for a wanted precision (half-width of the confidence interval):", tab$precision,
      "Area each of two batches needs so that a density ratio is found:", tab$ratio)),
    list(title = "Largest inclusion (extreme value statistics)", content = list(
      "How the interval of the return level narrows with more control areas:", tab$evs)),
    list(title = "Repeated runs", content = list(tab$runs, tab$repeat_density, tab$repeat_other)),
    list(title = "Methods", content = list(report_list(c(
      "Number density: relative half-width z * sqrt(D / N) for N particles, with D the index of dispersion of the counts per SEM field (variance over mean; pooled within the runs when there are several; D below 1 replaced by 1). Area for a wanted half-width r: z^2 D / (density r^2).",
      "Density ratio: two batches on the same area each, two-sided test, area (z_alpha/2 + z_beta)^2 D (1 + 1/ratio) / (density ln(ratio)^2).",
      "Largest inclusion: Murakami / ASTM E2283 block maxima, Gumbel fit; the interval for more control areas by simulation from the fitted distribution.",
      "Repeated runs: chi-squared test of the run counts against their areas, divided by D; scatter split into counting and the rest; repeatability limit 1.96 sqrt(2) times the total standard deviation."))))
  )
  if (!is.null(script)) {
    sections <- c(sections, list(list(title = "R script that repeats this plan", content = list(report_code(script)))))
  }
  build_html_report("Measurement plan", sections,
                    subtitle = sprintf("%d run(s), %d particles, inspected area %.2f mm2", p$runs, p$n, p$inspected_area_mm2),
                    footer = .report_footer())
}
