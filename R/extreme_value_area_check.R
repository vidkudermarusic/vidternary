# ---- Sanity checks on the field IDs behind the EVS control areas ----
# Control areas are built from field IDs (see assign_control_areas()), so
# IDs that do not run 1, 2, 3, ... silently give wrong control areas:
# two plates numbered 101-106 and 201-206 count as 106 fields, and text IDs
# sorted alphabetically put F1, F10, F11, F12 into the same area. The
# helpers below order text IDs naturally and describe what looks wrong so
# the tab can say so.

# Sorts distinct IDs so that numbers inside text compare as numbers
# ("F2" before "F10"), as opposed to sort()'s "F1", "F10", "F2".
natural_sort_ids <- function(x) {
  x <- unique(as.character(x))
  key <- x
  hits <- gregexpr("[0-9]+", x)
  regmatches(key, hits) <- lapply(regmatches(x, hits), function(d) sprintf("%015.0f", as.numeric(d)))
  x[order(key, x, method = "radix")]
}

# Share of missing numbers among numeric field IDs above which the IDs are
# reported as probably not one consecutive series. A real map has only the
# odd empty field (2 of 608 in the test data); separate plates or a
# every-other-field scan leave far more.
EVS_MAX_MISSING_FIELD_SHARE <- 0.10

#' Messages about the field IDs and control areas of an EVS fit
#'
#' @param areas Result of [assign_control_areas()].
#' @param n_areas_in_fit Number of control areas that entered the fit (those
#'   with at least one valid inclusion).
#' @param fields_per_area Fields per control area.
#' @return A list: `warnings` and `notes`, each a character vector (possibly
#'   empty).
#' @export
evs_area_notes <- function(areas, n_areas_in_fit, fields_per_area) {
  warnings <- character(0)
  notes <- character(0)

  if (isTRUE(areas$numeric_ids)) {
    share <- areas$n_missing_fields / areas$n_fields
    if (is.finite(share) && share > EVS_MAX_MISSING_FIELD_SHARE) {
      warnings <- c(warnings, sprintf(
        "Only %d of the %d field numbers between the first and last ID have inclusions (%.0f%% are missing, longest gap %d). Missing numbers are counted as fields with nothing detected, so if the gaps are separate plates or scans (e.g. IDs 101-106 and 201-206), the number of fields, the control areas and T are wrong. Number the fields consecutively 1, 2, 3, ... before running EVS.",
        areas$n_fields_with_data, areas$n_fields, 100 * share, areas$longest_missing_run))
    }
  } else if (!is.null(areas$ids_sorted)) {
    first_area <- areas$ids_sorted[seq_len(min(fields_per_area, length(areas$ids_sorted)))]
    notes <- c(notes, sprintf(
      "The field IDs are text and are ordered naturally (F2 before F10). The first control area holds: %s. Check that this is the order in which the fields were scanned.",
      paste(first_area, collapse = ", ")))
  }

  n_empty <- areas$n_control_areas - n_areas_in_fit
  if (n_empty > 0) {
    warnings <- c(warnings, sprintf(
      "%d of the %d control areas contain no valid inclusion (nothing detected, or everything removed by the pre-analysis filter). They are left out of the fit but still counted in T.",
      n_empty, areas$n_control_areas))
  }

  list(warnings = warnings, notes = notes)
}
