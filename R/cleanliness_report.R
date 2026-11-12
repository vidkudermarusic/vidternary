# ---- Reproducibility: report and script for the Steel Cleanliness tab ----

#' R script that repeats a cleanliness analysis
#'
#' @param file_name Name of the data file.
#' @param cols The column list given to [run_cleanliness_analysis()].
#' @param settings The settings of the finished run (`result$settings`), so
#'   every value - the inspected area included - is written out.
#' @param filters Pre-analysis filters, column -> condition (may be empty).
#' @return The script as a character vector of lines.
#' @export
cleanliness_script <- function(file_name, cols, settings, filters = list()) {
  cols <- cols[!vapply(cols, is.null, logical(1))]
  c(.script_header("Steel cleanliness analysis", "Steel Cleanliness"),
    sprintf("file <- %s", .r_literal(file_name)),
    "all_data <- read_particle_table(file)",
    if (length(filters)) sprintf("data <- apply_pre_filters(all_data, %s)", .r_literal(filters)) else "data <- all_data",
    "",
    sprintf("cols <- %s", .r_literal(cols)),
    sprintf("settings <- %s", .r_literal(settings)),
    "result <- run_cleanliness_analysis(data, cols, settings, all_data = all_data)",
    "",
    "result$rating_summary      # chart rating per type and width series",
    "result$indices             # particles per mm2, area fraction",
    "result$notes               # warnings and notes",
    "# writexl::write_xlsx(cleanliness_result_tables(result), \"steel_cleanliness.xlsx\")")
}

#' HTML report of a cleanliness analysis
#'
#' @param result From [run_cleanliness_analysis()].
#' @param provenance Data frame from [report_provenance()], or `NULL`.
#' @param script Script lines from [cleanliness_script()], or `NULL`.
#' @param filters Pre-analysis filters that were applied (for the settings
#'   section).
#' @return The HTML document as one string (see [build_html_report()]).
#' @export
cleanliness_report <- function(result, provenance = NULL, script = NULL, filters = list()) {
  tab <- cleanliness_display_tables(result)
  st <- result$settings
  settings <- data.frame(Setting = names(st),
                         Value = vapply(st, function(v) if (is.null(v)) "" else paste(format(v), collapse = ", "), character(1)),
                         stringsAsFactors = FALSE, row.names = NULL)
  if (length(filters)) {
    settings <- rbind(settings, data.frame(Setting = paste("filter:", names(filters)), Value = unlist(filters, use.names = FALSE),
                                           stringsAsFactors = FALSE))
  }
  has_direction <- "direction" %in% names(result$particles) && any(!is.na(result$particles$object))
  rated <- !is.na(result$particles$object)

  sections <- list(
    list(title = "Data and settings", content = list(provenance, settings)),
    list(title = "What to keep in mind", content = list(report_warning(result$notes$warnings), report_list(result$notes$notes))),
    list(title = sprintf("Chart rating (%s)", result$standard$name), content = list(
      "Index per inclusion type and width series, for 0.5 mm2 rating fields cut from the particle coordinates. Worst field = method A, mean over all fields = method B.",
      tab$rating, "The same rating with each chemical family on its own:", tab$rating_family)),
    list(title = "Stringers", content = list(
      create_stringer_map(result),
      if (has_direction) create_direction_rose(result$particles$direction[rated], result$particles$length[rated], st$axis_angle),
      tab$stringers)),
    list(title = "Number density and area fraction", content = list(tab$indices, create_size_class_plot(result$size_classes), tab$sizes)),
    list(title = "Stereology and mass balance", content = list(
      tab$stereology, if (!is.null(result$saltykov)) create_saltykov_plot(result$saltykov), tab$saltykov,
      "O, S and N bound in the inclusions, each family taken as one pure compound:", tab$phases, tab$elements)),
    list(title = "Methods", content = list(report_list(c(
      "Chart rating: ISO 4967:2013 / ASTM E45; SEM-based rating in the sense of ASTM E2142. Not a rating to the standard: an SEM run covers far less area and sees smaller particles than the light microscope of the chart method.",
      "Number density: exact Poisson interval (Garwood). Area fraction: bootstrap over SEM fields.",
      "Stereology: Delesse (volume fraction = area fraction), Fullman (1953), Schwartz-Saltykov unfolding; spheres assumed.",
      "Chemical family from S, Ti, Al, Mg, Ca and Si as compound mass; N, O and C are not used (Ti-N and Cr-O peak overlaps, carbon contamination)."))))
  )
  if (!is.null(script)) {
    sections <- c(sections, list(list(title = "R script that repeats this analysis", content = list(report_code(script)))))
  }
  build_html_report("Steel cleanliness report", sections,
                    subtitle = sprintf("%d particles, %d rated objects, inspected area %.2f mm2", nrow(result$particles),
                                       sum(result$objects$rated), result$inspected_area_mm2),
                    footer = .report_footer())
}
