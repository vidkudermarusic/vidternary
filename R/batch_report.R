# ---- Batch comparison: display tables, report and script ----

# p-value for display: never "0", since a permutation test cannot give one.
.format_p <- function(p) ifelse(is.na(p), "-", ifelse(p < 0.001, "< 0.001", sprintf("%.3f", p)))

#' Display versions of the batch-comparison tables
#'
#' @param res From [run_batch_comparison()].
#' @return A named list of data frames with readable headers and rounded
#'   numbers: `summary`, `overall`, `density_pairs`, `size_quantiles`,
#'   `size_pairs`, `composition_pairs`, `clr`, `spread`, `evs`.
#' @export
batch_display_tables <- function(res) {
  s <- res$summary
  interval <- function(lo, hi, fmt = "%.1f") ifelse(is.na(lo), "-", sprintf(paste0("[", fmt, ", ", fmt, "]"), lo, hi))
  summary <- data.frame(
    Batch = s$batch, Particles = s$n, `Inspected area (mm2)` = sprintf("%.2f", s$inspected_area_mm2),
    `Per mm2` = sprintf("%.1f", s$per_mm2), `95% interval (Poisson)` = interval(s$per_mm2_lower, s$per_mm2_upper),
    `Area fraction (ppm)` = sprintf("%.0f", s$area_ppm),
    `95% interval (bootstrap over fields)` = interval(s$area_ppm_lower, s$area_ppm_upper, "%.0f"),
    `Median ECD (um)` = sprintf("%.2f", s$median_ecd_um), `90th percentile (um)` = sprintf("%.2f", s$p90_ecd_um),
    `99th percentile (um)` = sprintf("%.2f", s$p99_ecd_um), `Largest sqrt(area) (um)` = sprintf("%.2f", s$max_sqrt_area_um),
    `Index of dispersion (counts per field)` = ifelse(is.na(s$field_dispersion), "-", sprintf("%.2f", s$field_dispersion)),
    check.names = FALSE, stringsAsFactors = FALSE)

  tests <- batch_result_tables(res)$`Overall tests`
  overall <- data.frame(Question = c(
    "Number density: are the counts in proportion to the inspected areas?",
    if (!is.null(res$field_counts)) "Counts per SEM field: do the batches differ? (does not assume independent particles)",
    "Size distribution: is it the same in all batches?",
    if (!is.null(res$composition)) "Composition: do the batch centroids differ?",
    if (!is.null(res$composition)) "Composition: do the batches differ in spread?"),
    Test = gsub("^[(]|[)]$", "", regmatches(tests$test, regexpr("[(][^()]*[)]$", tests$test))),
    Statistic = sprintf("%.3g", tests$statistic), `p-value` = .format_p(tests$p_value),
    check.names = FALSE, stringsAsFactors = FALSE)
  if (!is.null(res$composition)) {
    overall$`Share of variation explained (R2)` <- ""
    overall$`Share of variation explained (R2)`[grepl("centroids", overall$Question)] <- sprintf("%.1f %%", 100 * res$composition$overall$r2)
  }

  dp <- res$density$pairs
  density_pairs <- data.frame(
    Pair = paste(dp$batch_a, "vs", dp$batch_b), `Density ratio` = sprintf("%.2f", dp$ratio),
    `95% interval` = interval(dp$ratio_lower, dp$ratio_upper, "%.2f"),
    `p-value` = .format_p(dp$p_value), `p-value (Holm)` = .format_p(dp$p_adjusted),
    check.names = FALSE, stringsAsFactors = FALSE)

  q <- res$size$quantiles
  size_quantiles <- data.frame(Batch = q$batch, Particles = q$n, `Median (um)` = sprintf("%.2f", q$median),
                               `90th percentile (um)` = sprintf("%.2f", q$p90), `99th percentile (um)` = sprintf("%.2f", q$p99),
                               `Largest (um)` = sprintf("%.2f", q$max), check.names = FALSE, stringsAsFactors = FALSE)
  sp <- res$size$pairs
  size_pairs <- data.frame(
    Pair = paste(sp$batch_a, "vs", sp$batch_b), `Median ratio` = sprintf("%.2f", sp$median_ratio),
    `90th percentile ratio` = sprintf("%.2f", sp$p90_ratio), `Anderson-Darling statistic` = sprintf("%.2f", sp$statistic),
    `p-value` = .format_p(sp$p_value), `p-value (Holm)` = .format_p(sp$p_adjusted),
    check.names = FALSE, stringsAsFactors = FALSE)

  none <- data.frame(Note = "Choose two or more chemistry columns to compare the composition.")
  cmp <- res$composition
  if (is.null(cmp)) {
    composition_pairs <- clr <- spread <- none
  } else {
    composition_pairs <- data.frame(
      Pair = paste(cmp$pairs$batch_a, "vs", cmp$pairs$batch_b), `Pseudo-F` = sprintf("%.2f", cmp$pairs$f),
      `Share of variation explained (R2)` = sprintf("%.1f %%", 100 * cmp$pairs$r2),
      `p-value` = .format_p(cmp$pairs$p_value), `p-value (Holm)` = .format_p(cmp$pairs$p_adjusted),
      check.names = FALSE, stringsAsFactors = FALSE)
    parts <- unique(cmp$clr_means$part)
    clr <- data.frame(Element = parts, check.names = FALSE, stringsAsFactors = FALSE)
    for (b in unique(cmp$clr_means$batch)) {
      d <- cmp$clr_means[cmp$clr_means$batch == b, ]
      clr[[b]] <- sprintf("%+.2f", d$difference[match(parts, d$part)])
    }
    clr$`Zero readings (%)` <- sprintf("%.0f", cmp$zeros$Zero_pct)
    spread <- data.frame(Batch = cmp$dispersion$per_batch$batch, Particles = unname(cmp$n[cmp$dispersion$per_batch$batch]),
                         `Mean distance to the batch centroid` = sprintf("%.2f", cmp$dispersion$per_batch$mean_distance),
                         check.names = FALSE, stringsAsFactors = FALSE)
  }

  e <- res$evs
  evs <- if (is.null(e)) data.frame(Note = "Needs a SEM field ID column in every file.") else data.frame(
    Batch = e$batch, `Control areas in the fit` = ifelse(is.na(e$n_control_areas), "-", as.character(e$n_control_areas)),
    `Return level sqrt(area) (um)` = ifelse(is.na(e$return_level), "-", sprintf("%.2f", e$return_level)),
    `95% interval of the return level` = interval(e$ci_lower, e$ci_upper, "%.2f"),
    `95% interval for the largest inclusion` = interval(e$max_lower, e$max_upper, "%.2f"),
    `Largest observed (um)` = ifelse(is.na(e$largest_observed), "-", sprintf("%.2f", e$largest_observed)),
    `Gumbel fit, p` = .format_p(e$gof_p_value), Problem = ifelse(is.na(e$problem), "", e$problem),
    check.names = FALSE, stringsAsFactors = FALSE)

  list(summary = summary, overall = overall, density_pairs = density_pairs, size_quantiles = size_quantiles,
       size_pairs = size_pairs, composition_pairs = composition_pairs, clr = clr, spread = spread, evs = evs)
}

#' R script that repeats a batch comparison
#'
#' @param file_names Names of the data files, in batch order.
#' @param batch_names Batch labels (default: the file names without
#'   extension).
#' @param cols The column list given to [run_batch_comparison()].
#' @param settings The settings of the finished run (`result$settings`).
#' @param filters Pre-analysis filters, column -> condition (may be empty).
#' @return The script as a character vector of lines.
#' @export
batch_script <- function(file_names, cols, settings, filters = list(),
                         batch_names = tools::file_path_sans_ext(basename(file_names))) {
  cols <- cols[!vapply(cols, is.null, logical(1))]
  c(.script_header("Batch comparison", "Batch Comparison"),
    sprintf("files <- %s", .r_literal(file_names)),
    "all_batches <- lapply(files, read_particle_table)",
    sprintf("names(all_batches) <- %s", .r_literal(batch_names)),
    if (length(filters)) sprintf("batches <- lapply(all_batches, apply_pre_filters, filters = %s)", .r_literal(filters))
    else "batches <- all_batches",
    "",
    sprintf("cols <- %s", .r_literal(cols)),
    sprintf("settings <- %s", .r_literal(settings)),
    "result <- run_batch_comparison(batches, cols, settings, all_batches = all_batches)",
    "",
    "result$summary             # one row per batch",
    "result$density$pairs       # density ratios",
    "result$size$pairs          # size distributions",
    "result$composition$pairs   # chemistry (NULL when no parts were chosen)",
    "result$notes               # warnings and notes",
    "# writexl::write_xlsx(batch_result_tables(result), \"batch_comparison.xlsx\")")
}

#' HTML report of a batch comparison
#'
#' @param result From [run_batch_comparison()].
#' @param provenance Data frame from [report_provenance()], or `NULL`.
#' @param script Script lines from [batch_script()], or `NULL`.
#' @param filters Pre-analysis filters that were applied.
#' @return The HTML document as one string (see [build_html_report()]).
#' @export
batch_report <- function(result, provenance = NULL, script = NULL, filters = list()) {
  tab <- batch_display_tables(result)
  s <- result$summary
  st <- result$settings
  settings <- data.frame(Setting = names(st),
                         Value = vapply(st, function(v) if (is.null(v)) "" else paste(format(v), collapse = ", "), character(1)),
                         stringsAsFactors = FALSE, row.names = NULL)
  if (length(filters)) {
    settings <- rbind(settings, data.frame(Setting = paste("filter:", names(filters)), Value = unlist(filters, use.names = FALSE),
                                           stringsAsFactors = FALSE))
  }
  has_interval <- !all(is.na(s$area_ppm_lower))
  evs_ok <- !is.null(result$evs) && any(!is.na(result$evs$return_level))

  sections <- list(
    list(title = "Data and settings", content = list(provenance, settings)),
    list(title = "What to keep in mind", content = list(report_warning(result$notes$warnings), report_list(result$notes$notes))),
    list(title = "The batches at a glance", content = list(tab$summary, tab$overall)),
    list(title = "Amount of inclusions", content = list(
      create_batch_interval_chart(s, "per_mm2", "per_mm2_lower", "per_mm2_upper", "Particles per mm2", "Number density"),
      create_batch_interval_chart(s, "area_ppm", if (has_interval) "area_ppm_lower", if (has_interval) "area_ppm_upper",
                                  "Area fraction (ppm)", "Area fraction"),
      tab$density_pairs)),
    list(title = "Size distribution", content = list(
      create_size_tail_plot(result$particles$ecd, result$particles$batch), tab$size_quantiles, tab$size_pairs)),
    list(title = "Composition", content = if (is.null(result$composition)) list(tab$composition_pairs) else list(
      create_clr_difference_plot(result$composition$clr_means),
      "Clr mean of each batch minus the clr mean of all particles (positive = relatively more of that element):",
      tab$clr, tab$composition_pairs, tab$spread)),
    list(title = "Largest inclusion (extreme value statistics)", content = list(
      if (evs_ok) create_batch_interval_chart(result$evs, "return_level", "ci_lower", "ci_upper",
                                              "Return level sqrt(area) (um)", sprintf("Return level for T = %s control areas", format(st$return_period))),
      tab$evs)),
    list(title = "Methods", content = list(report_list(c(
      "Number density: exact Poisson rate-ratio test per pair; chi-squared test of counts against the inspected areas; Kruskal-Wallis test on the counts per SEM field.",
      "Size distribution: k-sample Anderson-Darling statistic (Scholz & Stephens, 1987, tie-adjusted), p-value by permutation of the batch labels.",
      "Composition: permutation MANOVA (Anderson, 2001) on isometric log-ratio coordinates, i.e. on Aitchison distances; spread tested on the distances to the batch centroid.",
      "Largest inclusion: Murakami / ASTM E2283 block maxima and Gumbel fit per batch, bootstrap intervals.",
      "Pairwise p-values adjusted by Holm's method."))))
  )
  if (!is.null(script)) {
    sections <- c(sections, list(list(title = "R script that repeats this comparison", content = list(report_code(script)))))
  }
  build_html_report("Batch comparison report", sections,
                    subtitle = sprintf("%d batches: %s", nrow(s), paste(s$batch, collapse = ", ")),
                    footer = .report_footer())
}
