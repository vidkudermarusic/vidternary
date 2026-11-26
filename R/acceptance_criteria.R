# ---- Acceptance: does the specimen meet the limits? ----
# A purchase specification or an internal rule sets upper limits - a chart
# index per inclusion type, a number density, an area fraction, a largest
# inclusion. assess_acceptance() compares a finished cleanliness analysis
# with such limits and, where the quantity has an interval, says whether
# the verdict holds beyond the uncertainty of the measurement:
#
#   pass                 value and whole interval at or below the limit
#   pass, not certain    value below the limit, interval reaches above it
#   fail, not certain    value above the limit, interval reaches below it
#   fail                 value and whole interval above the limit
#
# Quantities without an interval (chart indices, the largest particle
# found) are judged by their value alone, and the table says so.

#' Read chart-index limits typed as text
#'
#' @param text Limits as `"A=2, B=1.5, D fine=1, DS=1"`: an inclusion type
#'   (A, B, C, D - applies to its fine and thick series - or DS) or one
#'   series (`"A fine"`, `"D thick"`), each with the largest index allowed.
#' @return Named numeric vector (names are series), or `NULL` when empty.
#' @export
parse_rating_limits <- function(text) {
  if (is.null(text) || is.na(text) || !nzchar(trimws(text))) return(NULL)
  parts <- trimws(strsplit(as.character(text), "[,;]")[[1]])
  parts <- parts[nzchar(parts)]
  m <- regmatches(parts, regexec("^(DS|[ABCD])( +(fine|thick))?\\s*=\\s*([0-9]*\\.?[0-9]+)$", parts, ignore.case = TRUE))
  bad <- lengths(m) != 5
  if (any(bad)) {
    stop("Cannot read '", parts[bad][1], "'. Use Type=index with a decimal point, e.g. A=2, B=1.5, D fine=1, DS=1.", call. = FALSE)
  }
  out <- numeric(0)
  for (hit in m) {
    type <- toupper(hit[2])
    width <- tolower(hit[4])
    if (type == "DS" && nzchar(width)) stop("DS has no fine or thick series.", call. = FALSE)
    series <- if (type == "DS") "DS" else if (nzchar(width)) paste(type, width) else paste(type, c("fine", "thick"))
    out[series] <- as.numeric(hit[5])
  }
  out[intersect(.rating_series, names(out))]
}

#' Collect the limits a specimen has to meet
#'
#' Every limit is an upper limit; leave out (or pass `NA`) what is not
#' specified.
#'
#' @param per_mm2 Largest number density allowed (particles per mm2).
#' @param area_ppm Largest area fraction allowed (ppm).
#' @param largest_um Largest inclusion allowed among those found, as
#'   sqrt(area) in um.
#' @param return_level_um Largest inclusion allowed as predicted by the
#'   extreme-value fit (return level, sqrt(area) in um).
#' @param ratings Chart-index limits from [parse_rating_limits()].
#' @return A list of the limits that were given.
#' @export
acceptance_limits <- function(per_mm2 = NULL, area_ppm = NULL, largest_um = NULL, return_level_um = NULL, ratings = NULL) {
  given <- function(v) !is.null(v) && length(v) == 1 && is.finite(v)
  out <- list(per_mm2 = per_mm2, area_ppm = area_ppm, largest_um = largest_um, return_level_um = return_level_um)
  out <- out[vapply(out, given, logical(1))]
  if (any(vapply(out, function(v) v < 0, logical(1)))) stop("A limit cannot be negative.", call. = FALSE)
  if (length(ratings)) out$ratings <- ratings
  out
}

.acceptance_verdict <- function(value, limit, lower = NA_real_, upper = NA_real_) {
  if (!is.finite(value)) return("not assessed")
  if (value <= limit) {
    if (is.finite(upper) && upper > limit) "pass, not certain" else "pass"
  } else {
    if (is.finite(lower) && lower <= limit) "fail, not certain" else "fail"
  }
}

#' Compare a cleanliness analysis with acceptance limits
#'
#' @param result From [run_cleanliness_analysis()].
#' @param limits From [acceptance_limits()].
#' @param evs One row of [batch_evs_table()] for this specimen, needed for
#'   the `return_level_um` limit (or `NULL`).
#' @param level Confidence level of the number-density interval. Default
#'   0.95.
#' @return A list: `table` (one row per limit: `criterion`, `limit`,
#'   `value`, `lower`, `upper`, `interval` (what the interval is),
#'   `verdict`), `verdict` (`"pass"`, `"pass, not certain"`,
#'   `"fail, not certain"`, `"fail"`, or `"no limits"`), `summary` (one
#'   sentence).
#' @export
assess_acceptance <- function(result, limits, evs = NULL, level = 0.95) {
  row <- function(criterion, limit, value, lower = NA_real_, upper = NA_real_, interval = "none") {
    data.frame(criterion = criterion, limit = limit, value = value, lower = lower, upper = upper,
               interval = interval, verdict = .acceptance_verdict(value, limit, lower, upper), stringsAsFactors = FALSE)
  }
  all_row <- result$indices[result$indices$group == "All", , drop = FALSE]
  rows <- list()
  numbering <- if (length(result$field_check$warnings)) "; check the SEM field numbering (see the warnings)" else ""

  if (!is.null(limits$per_mm2)) {
    # Clustered particles: the Poisson interval is too narrow, so it is
    # widened by the index of dispersion of the counts per SEM field.
    disp <- NA_real_
    if (!is.null(result$particles$field)) {
      disp <- field_count_dispersion(result$particles$field, result$n_fields)$dispersion
    }
    area <- result$inspected_area_mm2
    if (isTRUE(disp > 1)) {
      half <- stats::qnorm((1 + level) / 2) * sqrt(disp * all_row$n) / area
      lower <- max(0, all_row$per_mm2 - half); upper <- all_row$per_mm2 + half
      kind <- sprintf("counting, widened for clustering (index of dispersion %.1f)%s", disp, numbering)
    } else {
      lower <- all_row$per_mm2_lower; upper <- all_row$per_mm2_upper
      kind <- if (is.na(disp)) "Poisson (assumes randomly scattered particles)" else "Poisson"
    }
    rows <- c(rows, list(row("Particles per mm2", limits$per_mm2, all_row$per_mm2, lower, upper, kind)))
  }
  if (!is.null(limits$area_ppm)) {
    has <- is.finite(all_row$area_ppm_lower)
    rows <- c(rows, list(row("Area fraction (ppm)", limits$area_ppm, all_row$area_ppm, all_row$area_ppm_lower,
                             all_row$area_ppm_upper, if (has) paste0("bootstrap over SEM fields", numbering) else "none (needs a field ID column)")))
  }
  if (!is.null(limits$largest_um)) {
    rows <- c(rows, list(row("Largest inclusion found, sqrt(area) (um)", limits$largest_um, all_row$max_sqrt_area_um,
                             interval = "none (the largest on the inspected area only)")))
  }
  if (!is.null(limits$return_level_um)) {
    ok <- !is.null(evs) && nrow(evs) == 1 && is.finite(evs$return_level)
    rows <- c(rows, list(if (ok) {
      row("Largest inclusion expected, sqrt(area) (um)", limits$return_level_um, evs$return_level, max(0, evs$ci_lower), evs$ci_upper,
          "bootstrap of the extreme-value fit")
    } else {
      row("Largest inclusion expected, sqrt(area) (um)", limits$return_level_um, NA_real_,
          interval = paste("no extreme-value fit:", if (is.null(evs)) "needs a SEM field ID column" else evs$problem %||% "no fit"))
    }))
  }
  for (series in names(limits$ratings)) {
    s <- result$rating_summary[result$rating_summary$series == series, , drop = FALSE]
    rows <- c(rows, list(row(sprintf("Chart index %s, worst field", series), limits$ratings[[series]],
                             if (nrow(s)) s$worst_index else NA_real_, interval = "none (worst rating field)")))
  }
  if (length(rows) == 0) {
    return(list(table = row("x", 0, 0)[0, ], verdict = "no limits", summary = "No limits entered."))
  }
  table <- do.call(rbind, rows)

  order <- c("fail", "fail, not certain", "not assessed", "pass, not certain", "pass")
  worst <- order[min(match(table$verdict, order))]
  names_of <- function(v) paste(table$criterion[table$verdict == v], collapse = "; ")
  summary <- switch(worst,
    "fail" = sprintf("Does not meet the limits: %s.", names_of("fail")),
    "fail, not certain" = sprintf("Above the limit, but within the uncertainty of the measurement: %s. A larger inspected area would decide it.", names_of("fail, not certain")),
    "not assessed" = sprintf("Meets the limits that could be assessed; not assessed: %s.", names_of("not assessed")),
    "pass, not certain" = sprintf("Meets the limits, but not beyond the uncertainty of the measurement for: %s.", names_of("pass, not certain")),
    "pass" = "Meets all limits.")
  list(table = table, verdict = worst, summary = summary)
}

#' Extreme-value return level of one specimen, for the acceptance check
#'
#' @param area Particle areas (um2) of the analysed particles.
#' @param field SEM field ID of each of them.
#' @param all_field Field IDs of all inspected rows (before any filter).
#' @param fields_per_area SEM fields per control area. Default 1.
#' @param return_period Return period T in control areas. Default 100.
#' @param n_boot Bootstrap samples. Default 2000.
#' @return One row of [batch_evs_table()].
#' @export
acceptance_evs <- function(area, field, all_field = field, fields_per_area = 1, return_period = 100, n_boot = 2000) {
  batch_evs_table(list(specimen = data.frame(area = area, field = field)), "area", "field", fields_per_area, return_period,
                  all_batches = list(specimen = data.frame(field = all_field)), n_boot = n_boot)
}

#' Display version of the acceptance table
#'
#' @param assessment From [assess_acceptance()].
#' @return A data frame with readable headers and rounded numbers.
#' @export
acceptance_display_table <- function(assessment) {
  t <- assessment$table
  if (nrow(t) == 0) return(data.frame(Note = "Enter one or more limits to see whether the specimen meets them."))
  data.frame(Criterion = t$criterion, `Limit (at most)` = sprintf("%g", t$limit),
             Measured = ifelse(is.na(t$value), "-", sprintf("%.4g", t$value)),
             `95% interval` = ifelse(is.na(t$lower) | is.na(t$upper), "-", sprintf("[%.4g, %.4g]", t$lower, t$upper)),
             `Interval from` = t$interval, Verdict = t$verdict, check.names = FALSE, stringsAsFactors = FALSE)
}
