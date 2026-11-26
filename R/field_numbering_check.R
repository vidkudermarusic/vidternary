# ---- Field IDs: are the SEM fields numbered as one consecutive series? ----
# Every tab that counts SEM fields - for the intervals that resample whole
# fields, the index of dispersion of the counts per field and the rank test
# on those counts - takes the inspected fields to be every number from the
# smallest to the largest field ID: a number missing from the file is a
# field where nothing was detected. That is right for an automated SEM run,
# which numbers every field it visits, and wrong when the numbering has
# gaps of its own (separate plates 101-106 and 201-206, IDs that step by
# 10). The EVS tab checks this for its control areas (evs_area_notes());
# field_numbering_notes() words the same check for the other tabs, with the
# same threshold for "too many missing numbers".

# Greatest common divisor of two whole numbers.
.gcd <- function(a, b) {
  while (b > 0) {
    r <- a %% b
    a <- b
    b <- r
  }
  a
}

#' Refuse a field ID column that would count an absurd number of fields
#'
#' Whole-number field IDs count every number from the smallest to the
#' largest as an inspected field, and the counts per field and the
#' bootstrap over fields are built at that size. One stray ID (a typing
#' error, or a wrong column such as a time stamp or spectrum number) would
#' then exhaust the memory: an ID of 3,000,000 took 10 GB. This stops
#' before anything is built per field.
#'
#' @param field Field ID of every inspected row.
#' @param max_fields Largest number of fields allowed. Default 100000 (a
#'   200 mm2 specimen in fields of 0.003 mm2 is about 64000); `Inf`
#'   switches the check off.
#' @param label Name of the batch or run for the message, or `NULL`.
#' @return The number of fields that would be counted, invisibly.
#' @export
check_sem_field_count <- function(field, max_fields = 100000, label = NULL) {
  ids <- field[!is.na(field)]
  if (length(ids) == 0) return(invisible(0))
  num <- suppressWarnings(as.numeric(as.character(ids)))
  whole <- all(is.finite(num)) && all(num == round(num))
  n <- if (whole) max(num) - min(num) + 1 else length(unique(as.character(ids)))
  if (is.finite(max_fields) && n > max_fields) {
    of <- if (is.null(label)) "" else sprintf(" of %s", label)
    big <- function(v) format(v, big.mark = ",", scientific = FALSE, trim = TRUE)
    what <- if (whole) {
      sprintf("The SEM field IDs%s run from %s to %s, so %s fields would be counted (%s of them hold particles; at most %s are allowed).",
              of, big(min(num)), big(max(num)), big(n), big(length(unique(num))), big(max_fields))
    } else {
      sprintf("The SEM field column%s holds %s different IDs (at most %s are allowed).", of, big(n), big(max_fields))
    }
    stop(what, " Check the field ID column: a wrong column (a time stamp, a spectrum or particle number) or one mistyped ID does this.",
         call. = FALSE)
  }
  invisible(n)
}

#' Messages about the numbering of the SEM field IDs
#'
#' @param field Field ID of every inspected row (before any filter).
#' @param label Name of the file, batch or run, put into the messages when
#'   several are checked; `NULL` for none.
#' @return A list: `warnings` and `notes` (character vectors, possibly
#'   empty), `step` (common step of whole-number IDs, 1 when consecutive or
#'   when the IDs are text) and `n_fields` (fields counted, empty ones
#'   included).
#' @export
field_numbering_notes <- function(field, label = NULL) {
  out <- list(warnings = character(0), notes = character(0), step = 1, n_fields = NA_integer_)
  ids <- field[!is.na(field)]
  if (length(ids) == 0) return(out)
  areas <- tryCatch(assign_control_areas(ids, 1), error = function(e) NULL)
  if (is.null(areas)) return(out)
  out$n_fields <- as.integer(areas$n_fields)
  of <- if (is.null(label)) "" else sprintf(" of %s", label)

  if (!isTRUE(areas$numeric_ids)) {
    out$notes <- sprintf(
      "The SEM field IDs%s are text, so only fields that hold at least one particle can be counted (%d). Fields where nothing was detected are missing from the counts per field: the intervals over fields and the index of dispersion describe the occupied fields only.",
      of, areas$n_fields)
    return(out)
  }

  present <- sort(unique(as.numeric(as.character(ids))))
  step <- if (length(present) >= 3) Reduce(.gcd, diff(present)) else 1
  out$step <- step
  share <- areas$n_missing_fields / areas$n_fields
  if (step > 1) {
    out$warnings <- sprintf(
      "The SEM field IDs%s step by %g (%s, ...). Every number from %g to %g is counted as an inspected field - %d fields, of which %d hold particles. If the IDs simply count in steps of %g, there are about %d fields, and the number of fields, the intervals over fields and the index of dispersion are wrong. Number the fields 1, 2, 3, ... (e.g. divide the IDs by %g) before the analysis.",
      of, step, paste(utils::head(present, 3), collapse = ", "), min(present), max(present),
      areas$n_fields, areas$n_fields_with_data, step, length(present), step)
  } else if (is.finite(share) && share > EVS_MAX_MISSING_FIELD_SHARE) {
    out$warnings <- sprintf(
      "Only %d of the %d field numbers between the first and last SEM field ID%s hold particles (%.0f %% are missing, longest gap %d). Each missing number is counted as an inspected field where nothing was detected. If the gaps are separate plates or scans (e.g. IDs 101-106 and 201-206) rather than empty fields, the number of fields, the intervals over fields and the index of dispersion are wrong; number the fields 1, 2, 3, ... before the analysis.",
      areas$n_fields_with_data, areas$n_fields, of, 100 * share, areas$longest_missing_run)
  } else if (areas$n_missing_fields > 0) {
    out$notes <- sprintf(
      "%d of the %d SEM fields%s hold no particle (their numbers are missing from the file); they are counted as inspected fields where nothing was detected.",
      areas$n_missing_fields, areas$n_fields, of)
  }
  out
}
