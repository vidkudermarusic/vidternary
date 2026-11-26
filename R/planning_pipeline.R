# ---- Measurement planning: the whole plan in one pure function ----
# plan_measurement() takes one pilot run of a specimen - or several repeated
# runs of it - and answers three questions before more instrument time is
# spent: how precise is the number density now and how much area would a
# wanted precision take; how much area per batch shows a given difference
# between two batches; and how many control areas the largest-inclusion
# estimate needs. With two or more runs it also says whether the runs agree
# (R/repeatability.R). No Shiny in here (server_planning.R only wires it).

#' Default settings of the measurement plan
#'
#' @return A named list of settings for [plan_measurement()].
#' @export
planning_default_settings <- function() {
  list(coord_unit = "mm", inspected_area_mm2 = NULL, target_precision_pct = 10, target_ratio = 1.2,
       power = 0.8, alpha = 0.05, level = 0.95, fields_per_area = 1, return_period = 100,
       n_sim = 400, seed = 42, max_sem_fields = 100000)
}

#' Plan a measurement from a pilot run
#'
#' @param runs Named list of data frames: the pilot run, or several
#'   repeated runs of the same specimen (after any filter).
#' @param cols Named list of column names: `area` (um2), and optionally
#'   `field` (SEM field ID), `x`, `y` (coordinates, for the inspected area).
#' @param settings Settings that differ from [planning_default_settings()].
#' @param all_runs The unfiltered data frames, used for the inspected area
#'   and the number of SEM fields.
#' @return A list: `pilot` (one-row data frame: what was measured),
#'   `per_run` (one row per run), `precision` (area for a wanted precision
#'   of the number density), `ratio` (area per batch to detect a density
#'   ratio), `curve` (precision against area, for the plot), `evs`
#'   (precision of the return level against control areas, or `NULL`),
#'   `repeatability` (list with `density` and `quantities`, or `NULL` with
#'   one run), `notes`, `settings`.
#' @export
plan_measurement <- function(runs, cols, settings = list(), all_runs = runs) {
  st <- utils::modifyList(planning_default_settings(), settings)
  if (length(runs) < 1) stop("Upload the pilot run.", call. = FALSE)
  if (is.null(names(runs)) || anyDuplicated(names(runs))) stop("Every run needs its own name.", call. = FALSE)
  if (is.null(cols$area) || !all(vapply(runs, function(d) cols$area %in% names(d), logical(1)))) {
    stop("Choose the area column (it must be present in every file).", call. = FALSE)
  }
  if (!is.finite(st$target_precision_pct) || st$target_precision_pct <= 0) stop("The wanted precision must be greater than 0 %.", call. = FALSE)
  if (!is.finite(st$target_ratio) || st$target_ratio <= 0 || st$target_ratio == 1) {
    stop("The density ratio to detect must be a positive number other than 1 (e.g. 1.2 for 20 % more).", call. = FALSE)
  }
  all_runs <- all_runs[names(runs)]
  has_field <- !is.null(cols$field) && all(vapply(runs, function(d) cols$field %in% names(d), logical(1)))
  inspected <- .batch_areas(all_runs, cols, st)
  if (has_field) {
    for (name in names(runs)) check_sem_field_count(all_runs[[name]][[cols$field]], st$max_sem_fields,
                                                    if (length(runs) > 1) sprintf("run '%s'", name))
  }

  per_run <- do.call(rbind, lapply(names(runs), function(name) {
    d <- runs[[name]]
    area <- suppressWarnings(as.numeric(d[[cols$area]]))
    valid <- is.finite(area) & area > 0
    counts <- if (has_field) {
      field_count_dispersion(d[[cols$field]][valid], .cleanliness_n_fields(all_runs[[name]][[cols$field]]))
    } else list(counts = integer(0), n_fields = NA_integer_, dispersion = NA_real_)
    ecd <- 2 * sqrt(area[valid] / pi)
    q <- if (any(valid)) stats::quantile(ecd, c(0.5, 0.9), names = FALSE) else c(NA_real_, NA_real_)
    data.frame(run = name, n = sum(valid), inspected_area_mm2 = inspected[[name]],
                      per_mm2 = sum(valid) / inspected[[name]], area_ppm = sum(area[valid]) / inspected[[name]],
                      median_ecd_um = q[1], p90_ecd_um = q[2],
                      max_sqrt_area_um = if (any(valid)) sqrt(max(area[valid])) else NA_real_,
                      n_fields = counts$n_fields, field_dispersion = counts$dispersion, stringsAsFactors = FALSE)
  }))
  n_total <- sum(per_run$n)
  if (n_total == 0) stop("The pilot run holds no particle with a valid area.", call. = FALSE)
  area_total <- sum(per_run$inspected_area_mm2)
  density <- n_total / area_total
  ci <- poisson_count_ci(n_total, st$level) / area_total

  # Counts per field of all runs together: one index of dispersion.
  field_counts <- if (has_field) unlist(lapply(names(runs), function(name) {
    d <- runs[[name]]
    area <- suppressWarnings(as.numeric(d[[cols$area]]))
    field_count_dispersion(d[[cols$field]][is.finite(area) & area > 0],
                           .cleanliness_n_fields(all_runs[[name]][[cols$field]]))$counts
  })) else NULL
  n_fields <- if (has_field) length(field_counts) else NA_integer_
  measured_dispersion <- if (has_field && length(field_counts) > 1 && mean(field_counts) > 0) {
    stats::var(field_counts) / mean(field_counts)
  } else NA_real_
  # Under-dispersion is not used to promise a better precision than Poisson.
  dispersion <- if (is.na(measured_dispersion)) 1 else max(1, measured_dispersion)
  field_area <- if (has_field) area_total / n_fields else NA_real_

  halfwidth_now <- density_relative_halfwidth(area_total, density, dispersion, st$level)
  pilot <- data.frame(runs = length(runs), n = n_total, inspected_area_mm2 = area_total, per_mm2 = density,
                      per_mm2_lower = ci[1, "lower"], per_mm2_upper = ci[1, "upper"],
                      n_fields = n_fields, field_area_mm2 = field_area, field_dispersion = measured_dispersion,
                      dispersion_used = dispersion, rel_halfwidth_now = halfwidth_now,
                      detectable_ratio_now = detectable_density_ratio(area_total, density, dispersion, st$power, st$alpha),
                      stringsAsFactors = FALSE)

  plan_row <- function(area) {
    data.frame(area_mm2 = area, fields = if (has_field) ceiling(area / field_area) else NA_real_,
               particles_expected = density * area, times_pilot_area = area / area_total)
  }
  targets <- sort(unique(c(50, 30, 20, 10, 5, st$target_precision_pct)), decreasing = TRUE)
  precision <- do.call(rbind, lapply(targets, function(pct) {
    cbind(precision_pct = pct, plan_row(area_for_density_precision(density, pct / 100, dispersion, st$level)),
          chosen = isTRUE(all.equal(pct, st$target_precision_pct)))
  }))
  ratios <- unique(c(2, 1.5, 1.2, 1.1, st$target_ratio))
  ratios <- ratios[order(abs(log(ratios)), decreasing = TRUE)]
  ratio <- do.call(rbind, lapply(ratios, function(r) {
    cbind(ratio = r, plan_row(area_for_density_ratio(density, r, dispersion, st$power, st$alpha)),
          chosen = isTRUE(all.equal(r, st$target_ratio)))
  }))

  largest_plan <- max(precision$area_mm2[precision$precision_pct >= 5], area_total)
  grid <- exp(seq(log(area_total / 4), log(max(largest_plan, 4 * area_total)), length.out = 80))
  curve <- rbind(
    data.frame(area_mm2 = grid, rel_halfwidth = density_relative_halfwidth(grid, density, dispersion, st$level),
               model = if (dispersion > 1) sprintf("as measured (index of dispersion %.1f)", dispersion) else "randomly scattered particles",
               stringsAsFactors = FALSE),
    if (dispersion > 1) data.frame(area_mm2 = grid, rel_halfwidth = density_relative_halfwidth(grid, density, 1, st$level),
                                   model = "if the particles were randomly scattered", stringsAsFactors = FALSE))

  evs <- NULL
  evs_problem <- NULL
  if (has_field) {
    evs <- tryCatch({
      maxima <- unlist(lapply(names(runs), function(name) {
        d <- runs[[name]]
        d$.control_area <- assign_control_areas(d[[cols$field]], st$fields_per_area,
                                                all_fields = all_runs[[name]][[cols$field]])$control_area
        compute_block_maxima(d, cols$area, ".control_area")$sqrt_area_max
      }))
      if (length(maxima) < 3) stop("fewer than 3 control areas with a valid inclusion")
      fit <- fit_evs_gumbel(maxima)
      out <- evs_precision_by_areas(fit, st$return_period, fit$n * c(1, 2, 4, 8, 16), st$n_sim, st$level, st$seed)
      out$area_mm2 <- out$n_areas * st$fields_per_area * field_area
      out$times_pilot <- out$n_areas / fit$n
      out
    }, error = function(e) { evs_problem <<- conditionMessage(e); NULL })
  }

  repeatability <- NULL
  if (length(runs) >= 2) {
    repeatability <- list(
      density = density_repeatability(stats::setNames(per_run$n, per_run$run), per_run$inspected_area_mm2, dispersion, st$level),
      quantities = quantity_repeatability(list(`Area fraction (ppm)` = per_run$area_ppm,
                                               `Median ECD (um)` = per_run$median_ecd_um,
                                               `90th percentile ECD (um)` = per_run$p90_ecd_um,
                                               `Largest sqrt(area) (um)` = per_run$max_sqrt_area_um), st$level))
  }

  field_checks <- if (has_field) {
    stats::setNames(lapply(names(runs), function(name) {
      field_numbering_notes(all_runs[[name]][[cols$field]], label = if (length(runs) > 1) sprintf("run '%s'", name))
    }), names(runs))
  } else NULL

  st$inspected_area_mm2 <- unname(inspected)
  result <- list(pilot = pilot, per_run = per_run, precision = precision, ratio = ratio, curve = curve, evs = evs,
                 evs_problem = evs_problem, repeatability = repeatability, has_field = has_field,
                 field_checks = field_checks, settings = st)
  result$notes <- planning_notes(result)
  result
}

#' Warnings and notes for a measurement plan
#'
#' @param result From [plan_measurement()] (before `notes` is set).
#' @return A list: `warnings` and `notes` (character vectors).
#' @export
planning_notes <- function(result) {
  p <- result$pilot
  st <- result$settings
  warn <- character(0)
  note <- character(0)
  # Field numbering: the dispersion and the field area come from it.
  warn <- c(warn, unlist(lapply(result$field_checks, `[[`, "warnings"), use.names = FALSE))
  note <- c(note, unlist(lapply(result$field_checks, `[[`, "notes"), use.names = FALSE))
  if (p$n < 30) {
    warn <- c(warn, sprintf(
      "The pilot run holds %d particles. The density it gives (%.1f per mm2, interval %.1f to %.1f) is itself uncertain, and every area below scales with one over that density - treat the plan as a rough guide.",
      p$n, p$per_mm2, p$per_mm2_lower, p$per_mm2_upper))
  }
  if (!result$has_field) {
    warn <- c(warn, "No SEM field ID column: the plan assumes randomly scattered particles. Inclusions usually cluster, so the areas given are a lower bound; choose the field column to measure the clustering.")
  } else if (isTRUE(p$field_dispersion > 2)) {
    warn <- c(warn, sprintf(
      "The counts per SEM field vary %.1f times more than random scatter allows: the particles are clustered. Every area in the plan is %.1f times larger than it would be for randomly scattered particles.",
      p$field_dispersion, p$dispersion_used))
  } else if (isTRUE(p$field_dispersion < 1)) {
    note <- c(note, sprintf("The counts per SEM field vary less than random scatter (index of dispersion %.2f); the plan uses 1, so it does not promise more than counting statistics allow.",
                            p$field_dispersion))
  }
  if (result$has_field && isTRUE(p$n_fields < 30)) {
    note <- c(note, sprintf("The index of dispersion comes from %d fields only and is itself uncertain.", p$n_fields))
  }
  # "p = 0.21" or "p < 0.001"
  p_text <- function(p) { f <- .format_p(p); if (startsWith(f, "<")) paste("p", f) else paste("p =", f) }
  rep <- result$repeatability
  if (!is.null(rep)) {
    if (isTRUE(rep$density$p_value < 0.05)) {
      warn <- c(warn, sprintf(
        "The repeated runs differ by more than counting scatter explains (%s): scatter between runs of %.1f per mm2 beyond counting. Something other than chance changes between runs - check threshold, focus, the scanned region and the filter. More area does not remove this part of the uncertainty.",
        p_text(rep$density$p_value), rep$density$sd_excess))
    } else {
      note <- c(note, sprintf("The repeated runs agree within counting scatter (%s).", p_text(rep$density$p_value)))
    }
    if (nrow(rep$density$runs) < 4) {
      note <- c(note, sprintf("With %d runs the scatter between runs is poorly known; the repeatability limits are a rough guide.", nrow(rep$density$runs)))
    }
  }
  if (!is.null(result$evs_problem)) {
    note <- c(note, sprintf("No plan for the largest-inclusion estimate: %s.", result$evs_problem))
  }
  note <- c(note,
    "The plan assumes that a larger measurement means more SEM fields of the same size on the same kind of surface, analysed with the same filter.",
    "The precision covers this specimen and section only. Differences between sections, positions in the product or heats are not in it and need their own specimens.",
    sprintf("Detecting a ratio: two batches inspected on the same area each, two-sided test at %g %%, found with %g %% probability.",
            100 * st$alpha, 100 * st$power))
  list(warnings = warn, notes = note)
}

#' Tables for exporting a measurement plan
#'
#' @param result From [plan_measurement()].
#' @return A named list of data frames (one per sheet).
#' @export
planning_result_tables <- function(result) {
  st <- result$settings
  flat <- function(v) if (is.null(v)) "" else paste(format(v), collapse = ", ")
  empty <- data.frame(note = "not computed", stringsAsFactors = FALSE)
  rp <- result$repeatability
  list(
    Pilot = result$pilot,
    Runs = result$per_run,
    `Area for precision` = result$precision,
    `Area for ratio` = result$ratio,
    `Largest inclusion` = result$evs %||% empty,
    `Repeatability density` = if (is.null(rp)) empty else data.frame(
      mean_per_mm2 = rp$density$mean_per_mm2, sd_observed = rp$density$sd_observed, cv_observed = rp$density$cv_observed,
      sd_counting = rp$density$sd_counting, cv_counting = rp$density$cv_counting, sd_excess = rp$density$sd_excess,
      statistic = rp$density$statistic, df = rp$density$df, p_value = rp$density$p_value,
      limit_per_mm2 = rp$density$limit_per_mm2, limit_rel = rp$density$limit_rel),
    `Repeatability other` = if (is.null(rp)) empty else rp$quantities,
    Notes = data.frame(kind = c(rep("warning", length(result$notes$warnings)), rep("note", length(result$notes$notes))),
                       text = c(result$notes$warnings, result$notes$notes), stringsAsFactors = FALSE),
    Settings = data.frame(setting = names(st), value = vapply(st, flat, character(1)),
                          stringsAsFactors = FALSE, row.names = NULL)
  )
}
