# ---- Folder summary: display tables, export and report ----

#' Display versions of the folder-summary tables
#'
#' @param res From [run_folder_summary()].
#' @return A named list of data frames with readable headers and rounded
#'   numbers: `overview`, `ratings`, `acceptance`, `failed`, `warnings`.
#' @export
folder_display_tables <- function(res) {
  s <- res$summary
  ok <- s[s$status == "ok", , drop = FALSE]
  interval <- function(lo, hi, fmt = "%.1f") ifelse(is.na(lo), "-", sprintf(paste0("[", fmt, ", ", fmt, "]"), lo, hi))
  none <- data.frame(Note = "No file could be analysed.")

  if (nrow(ok) == 0) {
    overview <- ratings <- none
  } else {
    overview <- data.frame(
      File = ok$file, Particles = ok$n, `Inspected area (mm2)` = sprintf("%.2f", ok$inspected_area_mm2),
      `Per mm2` = sprintf("%.1f", ok$per_mm2), `95% interval (Poisson)` = interval(ok$per_mm2_lower, ok$per_mm2_upper),
      `Area fraction (ppm)` = sprintf("%.0f", ok$area_ppm),
      `95% interval (bootstrap over fields)` = interval(ok$area_ppm_lower, ok$area_ppm_upper, "%.0f"),
      `Median ECD (um)` = sprintf("%.2f", ok$median_ecd_um), `90th percentile (um)` = sprintf("%.2f", ok$p90_ecd_um),
      `Largest sqrt(area) (um)` = sprintf("%.2f", ok$max_sqrt_area_um),
      `Largest expected, EVS (um)` = ifelse(is.na(ok$return_level_um), "-", sprintf("%.2f", ok$return_level_um)),
      `95% interval (EVS)` = interval(ok$return_level_lower, ok$return_level_upper, "%.2f"),
      Stringers = ok$stringers, check.names = FALSE, stringsAsFactors = FALSE)
    if (!all(is.na(ok$verdict))) overview$Verdict <- ok$verdict
    overview$`Warnings` <- ok$warnings

    ratings <- data.frame(File = ok$file, check.names = FALSE, stringsAsFactors = FALSE)
    for (series in .rating_series) ratings[[series]] <- sprintf("%.1f", ok[[paste0("index_", gsub(" ", "_", series))]])
  }

  acceptance <- if (is.null(res$acceptance)) data.frame(Note = "Enter one or more limits to see which files meet them.") else {
    a <- res$acceptance
    data.frame(File = a$file, Criterion = a$criterion, `Limit (at most)` = sprintf("%g", a$limit),
               Measured = ifelse(is.na(a$value), "-", sprintf("%.4g", a$value)),
               `95% interval` = ifelse(is.na(a$lower) | is.na(a$upper), "-", sprintf("[%.4g, %.4g]", a$lower, a$upper)),
               Verdict = a$verdict, check.names = FALSE, stringsAsFactors = FALSE)
  }
  bad <- s[s$status != "ok", , drop = FALSE]
  failed <- if (nrow(bad) == 0) data.frame(Note = "Every file was analysed.") else
    data.frame(File = bad$file, `Why it could not be analysed` = bad$problem, check.names = FALSE, stringsAsFactors = FALSE)
  warnings <- if (is.null(res$warnings)) data.frame(Note = "No warnings.") else
    data.frame(File = res$warnings$file, `Check` = res$warnings$text, check.names = FALSE, stringsAsFactors = FALSE)
  list(overview = overview, ratings = ratings, acceptance = acceptance, failed = failed, warnings = warnings)
}

#' Tables for exporting a folder summary
#'
#' @param res From [run_folder_summary()].
#' @return A named list of data frames (one per sheet).
#' @export
folder_result_tables <- function(res) {
  st <- c(res$settings[setdiff(names(res$settings), "cleanliness")], res$settings$cleanliness, res$cols)
  flat <- function(v) if (is.null(v)) "" else paste(format(v), collapse = ", ")
  empty <- data.frame(note = "nothing to report", stringsAsFactors = FALSE)
  limits <- res$limits
  limit_rows <- if (length(limits)) {
    ratings <- limits$ratings
    plain <- limits[setdiff(names(limits), "ratings")]
    data.frame(limit = c(names(plain), if (length(ratings)) paste("chart index", names(ratings))),
               at_most = c(unlist(plain, use.names = FALSE), unname(ratings)), stringsAsFactors = FALSE)
  } else empty
  list(Summary = res$summary, Acceptance = res$acceptance %||% empty, Limits = limit_rows,
       Warnings = res$warnings %||% empty,
       Settings = data.frame(setting = names(st), value = vapply(st, flat, character(1)), stringsAsFactors = FALSE, row.names = NULL),
       Filters = if (length(res$filters)) data.frame(column = names(res$filters), condition = unlist(res$filters, use.names = FALSE),
                                                    stringsAsFactors = FALSE) else empty)
}

#' HTML report of a folder summary
#'
#' @param result From [run_folder_summary()].
#' @param provenance Data frame from [report_provenance()], or `NULL`.
#' @return The HTML document as one string (see [build_html_report()]).
#' @export
folder_report <- function(result, provenance = NULL) {
  tab <- folder_display_tables(result)
  tables <- folder_result_tables(result)
  ok <- result$summary[result$summary$status == "ok", , drop = FALSE]
  names(ok)[names(ok) == "file"] <- "batch"
  chart <- function(value, lower, upper, label, title) {
    if (nrow(ok) == 0 || all(is.na(ok[[value]]))) return(NULL)
    with_interval <- !all(is.na(ok[[lower]]))
    create_batch_interval_chart(ok, value, if (with_interval) lower, if (with_interval) upper, label, title) +
      ggplot2::labs(x = "File", subtitle = "Point = estimate, bar = 95% interval, dashed line = mean of the files") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  }
  n_failed <- sum(result$summary$status != "ok")
  verdicts <- if (nrow(ok) && !all(is.na(ok$verdict))) table(ok$verdict) else NULL

  sections <- list(
    list(title = "Data and settings", content = list(provenance, tables$Settings,
                                                     if (length(result$filters)) tables$Filters)),
    list(title = "All files at a glance", content = list(
      if (!is.null(verdicts)) sprintf("Acceptance: %s.", paste(sprintf("%d %s", as.integer(verdicts), names(verdicts)), collapse = ", ")),
      tab$overview)),
    list(title = "Amount of inclusions", content = list(
      chart("per_mm2", "per_mm2_lower", "per_mm2_upper", "Particles per mm2", "Number density"),
      chart("area_ppm", "area_ppm_lower", "area_ppm_upper", "Area fraction (ppm)", "Area fraction"))),
    list(title = "Chart indices, worst field", content = list(
      "Index per inclusion type and width series for 0.5 mm2 rating fields cut from the particle coordinates. An SEM-based estimate, not a rating to the standard.",
      tab$ratings)),
    list(title = "Largest inclusion (extreme value statistics)", content = list(
      chart("return_level_um", "return_level_lower", "return_level_upper", "Return level sqrt(area) (um)",
            sprintf("Return level for T = %s control areas", format(result$settings$return_period))))),
    if (!is.null(result$acceptance)) list(title = "Acceptance limits", content = list(
      tables$Limits, tab$acceptance,
      "'Not certain' means the interval of the measured value reaches across the limit. Chart indices and the largest inclusion found have no interval and are judged by their value alone.")),
    list(title = "What to keep in mind", content = list(
      if (n_failed > 0) report_warning(sprintf("%d of %d files could not be analysed.", n_failed, nrow(result$summary))),
      if (n_failed > 0) tab$failed,
      tab$warnings,
      report_list(c("Every file is analysed on its own, with the same columns and settings. The files are not compared with a test here; use the Batch Comparison tab for that.",
                    "The intervals describe the counting uncertainty within each file. Two files whose intervals overlap may still differ, and the other way round - a test is needed to say.")))),
    list(title = "Methods", content = list(report_list(c(
      "Per file: the steel-cleanliness analysis (chart rating after ISO 4967 / ASTM E45 from SEM data, number density with an exact Poisson interval, area fraction with a bootstrap over SEM fields).",
      "Largest inclusion: Murakami / ASTM E2283 block maxima and Gumbel fit per file, bootstrap intervals.",
      "Acceptance: each limit against the measured value and its interval."))))
  )
  sections <- sections[!vapply(sections, is.null, logical(1))]
  build_html_report("Folder summary report", sections,
                    subtitle = sprintf("%d files, %d analysed", nrow(result$summary), nrow(ok)),
                    footer = .report_footer())
}

#' One cleanliness report per file, in a zip archive
#'
#' @param result From [run_folder_summary()].
#' @param zip_path Path of the archive to write.
#' @param progress Function called with the file name before each report,
#'   or `NULL`.
#' @return `zip_path`, invisibly.
#' @export
folder_report_archive <- function(result, zip_path, progress = NULL) {
  if (length(result$results) == 0) stop("No file was analysed, so there are no reports to write.", call. = FALSE)
  dir <- tempfile("folder_reports_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  safe <- make.unique(gsub("[^A-Za-z0-9._-]+", "_", names(result$results)), sep = "_")
  for (i in seq_along(result$results)) {
    if (!is.null(progress)) progress(names(result$results)[i])
    html <- cleanliness_report(result$results[[i]], filters = result$filters, acceptance = result$assessments[[names(result$results)[i]]])
    writeLines(html, file.path(dir, paste0(safe[i], "_report.html")), useBytes = TRUE)
  }
  writeLines(folder_report(result), file.path(dir, "folder_summary_report.html"), useBytes = TRUE)
  zip::zip(zip_path, files = list.files(dir), root = dir)
  invisible(zip_path)
}
