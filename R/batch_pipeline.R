# ---- Batch comparison: the whole comparison in one pure function ----
# run_batch_comparison() takes one data frame per batch (heat, specimen)
# and returns every table the "Batch Comparison" tab shows: amount of
# inclusions, size distribution, chemistry and largest-inclusion estimate,
# each with a test of whether the batches differ and a number that says by
# how much. No Shiny in here (server_batch.R only wires it).

#' Default settings of the batch comparison
#'
#' @return A named list of settings for [run_batch_comparison()].
#' @export
batch_default_settings <- function() {
  list(coord_unit = "mm", inspected_area_mm2 = NULL, n_perm = 999, n_boot = 2000,
       fields_per_area = 1, return_period = 100, seed = 42)
}

# Inspected area of each batch: the entered value(s), else the area
# spanned by the coordinates of the unfiltered data.
.batch_areas <- function(all_batches, cols, st) {
  entered <- st$inspected_area_mm2
  k <- length(all_batches)
  if (!is.null(entered) && length(entered) > 0 && all(is.finite(entered))) {
    if (!length(entered) %in% c(1, k)) {
      stop(sprintf("Enter one inspected area for all batches or one per batch (%d batches, %d areas given).",
                   k, length(entered)), call. = FALSE)
    }
    if (any(entered <= 0)) stop("Every inspected area must be greater than zero.", call. = FALSE)
    return(stats::setNames(rep_len(entered, k), names(all_batches)))
  }
  if (is.null(cols$x) || is.null(cols$y)) {
    stop("Enter the inspected area, or choose the X and Y coordinate columns so it can be worked out.", call. = FALSE)
  }
  unit_factor <- if (identical(st$coord_unit, "um")) 1 else 1000
  vapply(all_batches, function(d) {
    bbox <- .cleanliness_bbox(d, cols, unit_factor)
    (bbox[2] - bbox[1]) * (bbox[4] - bbox[3]) / 1e6
  }, numeric(1))
}

#' Compare several batches of inclusions
#'
#' @param batches Named list of data frames, one per batch (after any
#'   filter). The names are the batch labels.
#' @param cols Named list of column names: `area` (um2), and optionally
#'   `field` (SEM field ID), `x`, `y` (coordinates, for the inspected area)
#'   and `parts` (character vector of the compositional columns to compare).
#' @param settings Settings that differ from [batch_default_settings()].
#' @param all_batches The unfiltered data frames, used for the inspected
#'   area and the number of SEM fields so that a filter does not shrink
#'   them.
#' @return A list: `summary` (one row per batch), `density`
#'   ([compare_densities()]), `field_counts` ([compare_field_counts()], or
#'   `NULL`), `size` ([compare_size_distributions()]), `composition`
#'   ([compare_compositions()], or `NULL`), `evs` ([batch_evs_table()], or
#'   `NULL`), `particles` (batch, area, ECD of every particle), `notes`,
#'   `settings`.
#' @export
run_batch_comparison <- function(batches, cols, settings = list(), all_batches = batches) {
  st <- utils::modifyList(batch_default_settings(), settings)
  if (length(batches) < 2) stop("Upload at least 2 files (one per batch) to compare.", call. = FALSE)
  if (is.null(names(batches)) || anyDuplicated(names(batches))) stop("Every batch needs its own name.", call. = FALSE)
  all_batches <- all_batches[names(batches)]
  for (name in names(batches)) {
    if (is.null(cols$area) || !cols$area %in% names(batches[[name]])) {
      stop("Choose the area column (it must be present in every file).", call. = FALSE)
    }
  }
  has_field <- !is.null(cols$field) && all(vapply(batches, function(d) cols$field %in% names(d), logical(1)))
  inspected <- .batch_areas(all_batches, cols, st)

  per_batch <- lapply(names(batches), function(name) {
    d <- batches[[name]]
    area <- suppressWarnings(as.numeric(d[[cols$area]]))
    valid <- is.finite(area) & area > 0
    if (!any(valid)) stop("Batch '", name, "' has no particle with a valid area.", call. = FALSE)
    list(rows = which(valid), area = area[valid], ecd = 2 * sqrt(area[valid] / pi),
         field = if (has_field) d[[cols$field]][valid] else NULL,
         n_fields = if (has_field) .cleanliness_n_fields(all_batches[[name]][[cols$field]]) else NULL)
  })
  names(per_batch) <- names(batches)

  summary <- do.call(rbind, lapply(names(batches), function(name) {
    b <- per_batch[[name]]
    ind <- cleanliness_indices(b$area, rep("x", length(b$area)), inspected[[name]], field = b$field,
                               n_fields = b$n_fields, n_boot = st$n_boot, seed = st$seed)[1, ]
    disp <- if (has_field) field_count_dispersion(b$field, b$n_fields) else list(n_fields = NA_integer_, dispersion = NA_real_)
    q <- stats::quantile(b$ecd, c(0.5, 0.9, 0.99), names = FALSE)
    data.frame(batch = name, n = ind$n, inspected_area_mm2 = inspected[[name]],
               per_mm2 = ind$per_mm2, per_mm2_lower = ind$per_mm2_lower, per_mm2_upper = ind$per_mm2_upper,
               area_ppm = ind$area_ppm, area_ppm_lower = ind$area_ppm_lower, area_ppm_upper = ind$area_ppm_upper,
               median_ecd_um = q[1], p90_ecd_um = q[2], p99_ecd_um = q[3], max_sqrt_area_um = ind$max_sqrt_area_um,
               n_fields = disp$n_fields, field_dispersion = disp$dispersion, stringsAsFactors = FALSE)
  }))

  density <- compare_densities(stats::setNames(summary$n, summary$batch), summary$inspected_area_mm2)
  field_counts <- if (has_field) {
    compare_field_counts(lapply(per_batch, function(b) field_count_dispersion(b$field, b$n_fields)$counts))
  } else NULL

  particles <- do.call(rbind, lapply(names(batches), function(name) {
    data.frame(batch = name, row = per_batch[[name]]$rows, area = per_batch[[name]]$area,
               ecd = per_batch[[name]]$ecd, stringsAsFactors = FALSE)
  }))
  size <- compare_size_distributions(particles$ecd, particles$batch, st$n_perm, st$seed)

  composition <- NULL
  parts <- cols$parts
  if (length(parts) >= 2) {
    missing_parts <- parts[!vapply(parts, function(p) all(vapply(batches, function(d) p %in% names(d), logical(1))), logical(1))]
    if (length(missing_parts)) {
      stop("These chemistry columns are not present in every file: ", paste(missing_parts, collapse = ", "), ".", call. = FALSE)
    }
    chem <- do.call(rbind, lapply(names(batches), function(name) {
      d <- batches[[name]][per_batch[[name]]$rows, parts, drop = FALSE]
      d[] <- lapply(d, function(v) suppressWarnings(as.numeric(v)))
      d
    }))
    composition <- compare_compositions(chem, parts, particles$batch, st$n_perm, st$seed)
  }

  evs <- if (has_field) {
    batch_evs_table(batches, cols$area, cols$field, st$fields_per_area, st$return_period, all_batches, st$n_boot)
  } else NULL

  st$inspected_area_mm2 <- unname(inspected)
  result <- list(summary = summary, density = density, field_counts = field_counts, size = size,
                 composition = composition, evs = evs, particles = particles, parts = parts,
                 has_field = has_field, settings = st)
  result$notes <- batch_notes(result)
  result
}

#' Warnings and notes for a batch comparison
#'
#' @param result From [run_batch_comparison()] (before `notes` is set).
#' @return A list: `warnings` and `notes` (character vectors).
#' @export
batch_notes <- function(result) {
  s <- result$summary
  warn <- character(0)
  note <- character(0)

  clustered <- s$batch[!is.na(s$field_dispersion) & s$field_dispersion > 2]
  if (length(clustered)) {
    warn <- c(warn, sprintf(
      "The counts per SEM field vary more than random scatter allows in: %s (index of dispersion above 2). The particles are clustered, so the Poisson intervals and p-values for the number density are too optimistic - rely on the test on counts per field.",
      paste(clustered, collapse = ", ")))
  }
  if (!result$has_field) {
    note <- c(note, "No SEM field ID column in every file: the area fraction has no interval, the rank test on counts per field and the extreme-value estimates are left out.")
  }
  if (any(s$n >= 1000)) {
    note <- c(note, "With a thousand particles or more, tests find differences that are too small to matter. Judge the size of a difference from the ratios, the quantiles and R2, and use the p-value only to rule out chance.")
  }
  if (any(s$n < 30)) {
    warn <- c(warn, sprintf("Fewer than 30 particles in: %s. Quantiles such as the 90th or 99th percentile are then set by one or two particles.",
                            paste(s$batch[s$n < 30], collapse = ", ")))
  }
  area_ratio <- max(s$inspected_area_mm2) / min(s$inspected_area_mm2)
  if (area_ratio > 1.5) {
    note <- c(note, sprintf("The inspected areas differ by a factor of %.1f. Densities and area fractions are per mm2 and stay comparable; the largest particle found does not (a larger area finds a larger one) - compare the extreme-value return levels instead.", area_ratio))
  }
  cmp <- result$composition
  if (!is.null(cmp)) {
    elements <- tryCatch(names(inclusion_element_columns(result$parts)), error = function(e) character(0))
    overlaps <- eds_overlaps(elements)
    if (length(overlaps)) {
      warn <- c(warn, sprintf("The compared elements include overlapping EDS lines (%s). A difference between batches in these elements may be an artefact of peak deconvolution; repeat the comparison without one element of each group.",
                              paste(overlaps, collapse = "; ")))
    }
    many_zeros <- cmp$zeros$Part[cmp$zeros$Zero_pct > 100 * COMPOSITIONAL_MAX_ZERO_SHARE]
    if (length(many_zeros)) {
      warn <- c(warn, sprintf("More than %g %% of the readings are zero in: %s. Zeros are replaced by a small value before the log-ratios are taken, so batches may then differ mainly in how often an element was detected. Leave these columns out or filter to the particles where they were measured.",
                              100 * COMPOSITIONAL_MAX_ZERO_SHARE, paste(many_zeros, collapse = ", ")))
    }
    if (isTRUE(cmp$dispersion$p_value < 0.05)) {
      note <- c(note, sprintf("The batches differ in the spread of their chemistry (p = %.3f). The composition test also reacts to that, so a significant result may mean a different spread, not a different average composition - check the clr means.",
                              cmp$dispersion$p_value))
    }
  }
  if (!is.null(result$evs) && any(!is.na(result$evs$problem))) {
    bad <- result$evs[!is.na(result$evs$problem), ]
    warn <- c(warn, sprintf("No extreme-value estimate for: %s.", paste(sprintf("%s (%s)", bad$batch, bad$problem), collapse = "; ")))
  }
  note <- c(note, "Pairwise p-values are adjusted for the number of pairs (Holm). Size and composition tests use permutations of the batch labels, so they treat particles as independent; batches that differ only through one cluster of particles can still come out as different.")
  list(warnings = warn, notes = note)
}

#' Tables for exporting a batch comparison
#'
#' @param result From [run_batch_comparison()].
#' @return A named list of data frames (one per sheet).
#' @export
batch_result_tables <- function(result) {
  st <- result$settings
  flat <- function(v) if (is.null(v)) "" else paste(format(v), collapse = ", ")
  test_row <- function(name, t, stat) {
    data.frame(test = name, statistic = t[[stat]] %||% NA_real_, p_value = t$p_value %||% NA_real_, stringsAsFactors = FALSE)
  }
  overall <- rbind(
    test_row("Number density: counts in proportion to the areas (chi-squared)", result$density$overall, "statistic"),
    if (!is.null(result$field_counts)) test_row("Counts per SEM field (Kruskal-Wallis)", result$field_counts, "statistic"),
    test_row("Size distribution (k-sample Anderson-Darling, permutation)", result$size$overall, "statistic"),
    if (!is.null(result$composition)) test_row("Composition (permutation MANOVA on ilr, pseudo-F)", result$composition$overall, "f"),
    if (!is.null(result$composition)) test_row("Spread of composition (distance to batch centroid, F)", result$composition$dispersion, "f")
  )
  empty <- data.frame(note = "not computed", stringsAsFactors = FALSE)
  list(
    Summary = result$summary,
    `Overall tests` = overall,
    `Density pairs` = result$density$pairs,
    `Size quantiles` = result$size$quantiles,
    `Size pairs` = result$size$pairs,
    `Composition pairs` = if (is.null(result$composition)) empty else result$composition$pairs,
    `Composition clr means` = if (is.null(result$composition)) empty else result$composition$clr_means,
    `Composition spread` = if (is.null(result$composition)) empty else result$composition$dispersion$per_batch,
    `Extreme values` = result$evs %||% empty,
    Notes = data.frame(kind = c(rep("warning", length(result$notes$warnings)), rep("note", length(result$notes$notes))),
                       text = c(result$notes$warnings, result$notes$notes), stringsAsFactors = FALSE),
    Settings = data.frame(setting = names(st), value = vapply(st, flat, character(1)),
                          stringsAsFactors = FALSE, row.names = NULL)
  )
}
