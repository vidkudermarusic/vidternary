# ---- Steel cleanliness: the whole analysis in one pure function ----
# run_cleanliness_analysis() takes a data frame and returns every table the
# "Steel Cleanliness" tab shows. No Shiny in here, so the analysis can be
# scripted and tested without the app (server_cleanliness.R only wires it).

#' Default settings of the cleanliness analysis
#'
#' @param standard `"ISO 4967"` or `"ASTM E45"`; sets the stringer
#'   distances and the aspect-ratio limit.
#' @return A named list of settings for [run_cleanliness_analysis()].
#' @export
cleanliness_default_settings <- function(standard = "ISO 4967") {
  std <- cleanliness_rating_standard(standard)
  list(standard = std$name, axis_angle = 0, max_gap = std$max_gap, max_offset = std$max_offset,
       aspect_limit = std$aspect_limit, min_members = std$min_members, min_width = 2,
       field_area_mm2 = std$field_area_mm2, coord_unit = "mm", inspected_area_mm2 = NULL,
       min_signal_pct = 5, size_breaks = c(1, 2, 5, 10, 20), saltykov_classes = 10,
       steel_density = 7.9, measured_ppm = NULL, n_boot = 2000)
}

# Inspected area as c(xmin, xmax, ymin, ymax) in um.
.cleanliness_bbox <- function(data, cols, unit_factor) {
  x <- suppressWarnings(as.numeric(data[[cols$x]])) * unit_factor
  y <- suppressWarnings(as.numeric(data[[cols$y]])) * unit_factor
  ok <- is.finite(x) & is.finite(y)
  if (!any(ok)) stop("The coordinate columns hold no valid numbers.", call. = FALSE)
  c(range(x[ok]), range(y[ok]))
}

# Number of inspected SEM fields, empty ones included (whole-number IDs
# are taken as one consecutive series, as in the EVS tab).
.cleanliness_n_fields <- function(field) {
  tryCatch(assign_control_areas(field, 1)$n_fields, error = function(e) NULL)
}

#' Run the steel-cleanliness analysis
#'
#' Chemical family ([classify_inclusion_family()], or a column of the
#' data), stringers and rated objects ([build_rating_objects()]), chart
#' rating per 0.5 mm2 field ([rate_fields()], [summarise_rating()]), number
#' density and area fraction ([cleanliness_indices()], [size_class_table()])
#' and stereology ([fullman_number_density()], [saltykov_unfold()],
#' [inclusion_mass_balance()]).
#'
#' @param data Data frame of particles to analyse (after any filter).
#' @param cols Named list of column names: `area` (um2), `length`,
#'   `breadth` (um), `x`, `y`, and optionally `direction` (degrees),
#'   `field` (SEM field ID) and `family` (a ready-made class column used
#'   instead of the EDS classification).
#' @param settings Settings that differ from
#'   [cleanliness_default_settings()].
#' @param all_data The unfiltered data, used for the inspected area and the
#'   number of SEM fields so that a filter does not shrink them.
#' @return A list: `particles`, `objects`, `grid`, `field_ratings`,
#'   `rating_summary`, `rating_by_family`, `indices`, `size_classes`,
#'   `stereology`, `saltykov`, `mass_balance`, `inspected_area_mm2`,
#'   `notes`, `settings`.
#' @export
run_cleanliness_analysis <- function(data, cols, settings = list(), all_data = data) {
  st <- utils::modifyList(cleanliness_default_settings(settings$standard %||% "ISO 4967"), settings)
  for (need in c("area", "length", "breadth", "x", "y")) {
    if (is.null(cols[[need]]) || !cols[[need]] %in% names(data)) {
      stop("Choose the ", need, " column.", call. = FALSE)
    }
  }
  unit_factor <- if (identical(st$coord_unit, "um")) 1 else 1000
  num <- function(col) suppressWarnings(as.numeric(data[[col]]))
  has <- function(key) !is.null(cols[[key]]) && cols[[key]] %in% names(data)

  area <- num(cols$area); len <- num(cols$length); breadth <- num(cols$breadth)
  x <- num(cols$x) * unit_factor; y <- num(cols$y) * unit_factor
  valid <- is.finite(area) & area > 0 & is.finite(len) & len > 0 & is.finite(breadth) & breadth > 0 &
    is.finite(x) & is.finite(y)
  if (!any(valid)) stop("No particle has a valid area, length, breadth and position.", call. = FALSE)

  family <- if (has("family")) {
    f <- as.character(data[[cols$family]])
    ifelse(is.na(f) | !nzchar(f), "unclassified", f)
  } else {
    classify_inclusion_family(data, min_signal_pct = st$min_signal_pct)$family
  }

  particles <- data.frame(row = which(valid), x = x[valid], y = y[valid], length = len[valid],
                          breadth = breadth[valid], area = area[valid], family = family[valid],
                          stringsAsFactors = FALSE)
  particles$ecd <- 2 * sqrt(particles$area / pi)
  particles$aspect <- particles$length / particles$breadth
  if (has("direction")) particles$direction <- num(cols$direction)[valid]
  if (has("field")) particles$field <- data[[cols$field]][valid]

  bbox <- .cleanliness_bbox(all_data, cols, unit_factor)
  bbox_area <- (bbox[2] - bbox[1]) * (bbox[4] - bbox[3]) / 1e6
  inspected <- st$inspected_area_mm2
  if (is.null(inspected) || !is.finite(inspected) || inspected <= 0) inspected <- bbox_area
  if (!is.finite(inspected) || inspected <= 0) {
    stop("The inspected area cannot be worked out from the coordinates; enter it in mm2.", call. = FALSE)
  }

  std <- cleanliness_rating_standard(st$standard)
  std[c("max_gap", "max_offset", "aspect_limit", "min_members")] <-
    st[c("max_gap", "max_offset", "aspect_limit", "min_members")]
  built <- build_rating_objects(particles, std, st$axis_angle, st$min_width)
  particles$object <- built$object
  objects <- built$objects
  grid <- rating_field_grid(objects$x, objects$y, st$field_area_mm2, bbox)
  objects$field <- grid$field
  particles$type <- objects$type[match(particles$object, objects$object)]
  field_ratings <- rate_fields(objects, grid)
  rating_summary <- summarise_rating(field_ratings, grid)

  by_family <- lapply(sort(unique(objects$family[objects$rated])), function(fam) {
    sel <- objects$family == fam
    g <- grid
    g$field <- grid$field[sel]
    s <- summarise_rating(rate_fields(objects[sel, , drop = FALSE], g), g)
    cbind(family = fam, s[s$n_objects > 0, , drop = FALSE], stringsAsFactors = FALSE)
  })
  rating_by_family <- if (length(by_family)) do.call(rbind, by_family) else NULL

  n_fields <- if (has("field")) .cleanliness_n_fields(all_data[[cols$field]]) else NULL
  indices <- cleanliness_indices(particles$area, particles$family, inspected,
                                 field = if (has("field")) particles$field else NULL,
                                 n_fields = n_fields, n_boot = st$n_boot)
  size_classes <- size_class_table(particles$ecd, particles$family, inspected, st$size_breaks)

  groups <- indices$group
  stereology <- do.call(rbind, lapply(groups, function(g) {
    d <- if (g == "All") particles$ecd else particles$ecd[particles$family == g]
    data.frame(group = g, fullman_number_density(d, inspected), stringsAsFactors = FALSE)
  }))
  saltykov <- tryCatch(saltykov_unfold(particles$ecd, inspected, st$saltykov_classes), error = function(e) NULL)
  area_ppm <- stats::setNames(indices$area_ppm[indices$group != "All"], indices$group[indices$group != "All"])
  mass_balance <- inclusion_mass_balance(area_ppm, steel_density = st$steel_density, measured = st$measured_ppm)

  st$inspected_area_mm2 <- inspected
  result <- list(particles = particles, objects = objects, grid = grid, field_ratings = field_ratings,
                 rating_summary = rating_summary, rating_by_family = rating_by_family, indices = indices,
                 size_classes = size_classes, stereology = stereology, saltykov = saltykov,
                 mass_balance = mass_balance, inspected_area_mm2 = inspected, bbox = bbox,
                 n_rows = nrow(data), n_valid = sum(valid), standard = std, settings = st)
  result$notes <- cleanliness_notes(result, has_direction = has("direction"), bbox_area = bbox_area)
  result
}

#' Warnings and notes for a cleanliness analysis
#'
#' @param result From [run_cleanliness_analysis()] (before `notes` is set).
#' @param has_direction Whether a particle-direction column was used.
#' @param bbox_area Area of the coordinates' bounding box (mm2).
#' @return A list: `warnings` and `notes` (character vectors).
#' @export
cleanliness_notes <- function(result, has_direction = TRUE, bbox_area = NA_real_) {
  std <- result$standard
  st <- result$settings
  fields <- result$grid$fields
  p <- result$particles
  warn <- character(0)
  note <- character(0)

  if (result$inspected_area_mm2 < std$min_specimen_area_mm2) {
    warn <- c(warn, sprintf(
      "The inspected area is %.2f mm2; %s asks for about %g mm2. With %d rating field%s (%d complete) the worst-field index says little about the heat - read the indices as an estimate for this small area, not as a rating to the standard.",
      result$inspected_area_mm2, std$name, std$min_specimen_area_mm2, nrow(fields),
      if (nrow(fields) == 1) "" else "s", sum(fields$complete)))
  }
  if (is.finite(bbox_area) && bbox_area > 0 && abs(result$inspected_area_mm2 / bbox_area - 1) > 0.2) {
    warn <- c(warn, sprintf(
      "The inspected area entered (%.2f mm2) differs from the area spanned by the coordinates (%.2f mm2) by more than 20 %%. Check the coordinate unit and the area.",
      result$inspected_area_mm2, bbox_area))
  }
  n_small <- sum(p$ecd < 1)
  if (n_small > 0.5 * nrow(p)) {
    warn <- c(warn, sprintf(
      "%.0f %% of the particles are smaller than 1 um. Their EDS spectra are mostly steel matrix, so their chemical family is unreliable (in Mo-bearing steels they often read as sulfide because S K overlaps Mo L) and they dominate the counts per family. The chart rating is not affected (only particles from %g um width are rated); for the indices and the stereology consider the pre-analysis filter, e.g. Area > 1.",
      100 * n_small / nrow(p), st$min_width))
  }
  n_thin <- sum(is.na(p$object))
  note <- c(note, sprintf(
    "%d of %d particles (%.1f %%) are narrower than %g um and were not rated; they are still counted in the indices and the stereology.",
    n_thin, nrow(p), 100 * n_thin / nrow(p), st$min_width))
  n_uncl <- sum(p$family == "unclassified")
  if (n_uncl > 0) {
    note <- c(note, sprintf("%d particles (%.1f %%) have no clear chemical family ('unclassified') and are rated by shape alone.",
                            n_uncl, 100 * n_uncl / nrow(p)))
  }
  if (!has_direction) {
    note <- c(note, "No particle-direction column: every particle is taken as lying along the working direction.")
  }
  if (result$n_valid < result$n_rows) {
    note <- c(note, sprintf("%d rows without a valid area, length, breadth or position were left out.",
                            result$n_rows - result$n_valid))
  }
  note <- c(note,
    "Rating fields are cut from the particle coordinates; an object belongs to the field its centre lies in.",
    "An SEM detects far more and far smaller particles than the light microscope of the chart method, so these indices are not interchangeable with a chart rating (see ASTM E2142).")
  list(warnings = warn, notes = note)
}

#' Tables for exporting a cleanliness analysis
#'
#' @param result From [run_cleanliness_analysis()].
#' @return A named list of data frames (one per sheet).
#' @export
cleanliness_result_tables <- function(result) {
  st <- result$settings
  flat <- function(v) if (is.null(v)) "" else paste(format(v), collapse = ", ")
  settings <- data.frame(setting = names(st), value = vapply(st, flat, character(1)),
                         stringsAsFactors = FALSE, row.names = NULL)
  empty <- data.frame(note = "nothing to report", stringsAsFactors = FALSE)
  tables <- list(
    Rating = result$rating_summary,
    `Rating by family` = result$rating_by_family %||% empty,
    `Field ratings` = result$field_ratings,
    `Rating fields` = result$grid$fields,
    Objects = result$objects,
    Indices = result$indices,
    `Size classes` = result$size_classes,
    Stereology = result$stereology,
    Saltykov = result$saltykov %||% empty,
    `Mass balance phases` = result$mass_balance$phases,
    `Mass balance elements` = result$mass_balance$elements,
    Particles = result$particles,
    Notes = data.frame(kind = c(rep("warning", length(result$notes$warnings)), rep("note", length(result$notes$notes))),
                       text = c(result$notes$warnings, result$notes$notes), stringsAsFactors = FALSE),
    Settings = settings
  )
  lapply(tables, function(d) { d[] <- lapply(d, function(v) if (is.factor(v)) as.character(v) else v); d })
}
