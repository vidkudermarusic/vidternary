# ---- Folder summary: many files, one table ----
# run_folder_summary() runs the steel-cleanliness analysis on every file of
# a set (the specimens of a week, a heat, a trial) with the same columns
# and settings and puts the key numbers of each into one row: amount, size,
# chart indices, largest-inclusion estimate and - when limits are given -
# the acceptance verdict. Nothing is compared or tested here (that is the
# Batch Comparison tab); a file that cannot be analysed is kept in the
# table with the reason. process_folder() does the same from a directory,
# for use in a script. No Shiny in here (server_folder.R only wires it).

#' Default settings of the folder summary
#'
#' @return A named list: `cleanliness` (settings for
#'   [run_cleanliness_analysis()]), `fields_per_area`, `return_period`
#'   (extreme-value estimate), `n_boot`.
#' @export
folder_default_settings <- function() {
  list(cleanliness = list(), fields_per_area = 1, return_period = 100, n_boot = 1000)
}

# One row of the summary for a finished analysis.
.folder_row <- function(name, res, evs, assessment) {
  all_row <- res$indices[res$indices$group == "All", , drop = FALSE]
  idx <- stats::setNames(res$rating_summary$worst_index, res$rating_summary$series)[.rating_series]
  names(idx) <- paste0("index_", gsub(" ", "_", .rating_series))
  ecd <- res$particles$ecd
  cbind(
    data.frame(file = name, status = "ok", problem = NA_character_, n = all_row$n,
               inspected_area_mm2 = res$inspected_area_mm2,
               per_mm2 = all_row$per_mm2, per_mm2_lower = all_row$per_mm2_lower, per_mm2_upper = all_row$per_mm2_upper,
               area_ppm = all_row$area_ppm, area_ppm_lower = all_row$area_ppm_lower, area_ppm_upper = all_row$area_ppm_upper,
               median_ecd_um = stats::median(ecd), p90_ecd_um = stats::quantile(ecd, 0.9, names = FALSE),
               max_sqrt_area_um = all_row$max_sqrt_area_um,
               stringers = sum(res$objects$is_stringer & res$objects$rated), stringsAsFactors = FALSE),
    as.data.frame(as.list(idx)),
    data.frame(return_level_um = if (is.null(evs)) NA_real_ else evs$return_level,
               return_level_lower = if (is.null(evs)) NA_real_ else max(0, evs$ci_lower),   # a size cannot be negative
               return_level_upper = if (is.null(evs)) NA_real_ else evs$ci_upper,
               verdict = if (is.null(assessment)) NA_character_ else assessment$verdict,
               warnings = length(res$notes$warnings), stringsAsFactors = FALSE))
}

#' Analyse every file of a set and summarise them in one table
#'
#' @param tables Named list of data frames, one per file (the names are
#'   the file labels).
#' @param cols Column names as for [run_cleanliness_analysis()]; the same
#'   for every file.
#' @param settings Settings that differ from [folder_default_settings()].
#' @param limits Acceptance limits from [acceptance_limits()], or `NULL`.
#' @param filters Pre-analysis filters, column -> condition, applied to
#'   every file (see [apply_pre_filters()]).
#' @param progress Function called with the file name before each file
#'   (for a progress bar), or `NULL`.
#' @return A list: `summary` (one row per file; `status` is `"ok"` or
#'   `"failed"` with the reason in `problem`), `acceptance` (one row per
#'   file and limit, or `NULL`), `results` (the full analysis of each file
#'   that worked), `assessments`, `warnings` (data frame: `file`, `text`),
#'   `limits`, `filters`, `settings`.
#' @export
run_folder_summary <- function(tables, cols, settings = list(), limits = NULL, filters = list(), progress = NULL) {
  st <- utils::modifyList(folder_default_settings(), settings)
  if (length(tables) < 1) stop("No files to analyse.", call. = FALSE)
  if (is.null(names(tables)) || anyDuplicated(names(tables))) stop("Every file needs its own name.", call. = FALSE)
  has_limits <- length(limits) > 0
  results <- list()
  assessments <- list()
  rows <- list()

  for (name in names(tables)) {
    if (!is.null(progress)) progress(name)
    all_data <- tables[[name]]
    out <- tryCatch({
      data <- if (length(filters)) apply_pre_filters(all_data, filters) else all_data
      if (nrow(data) == 0) stop("no rows remain after the pre-analysis filter(s)")
      res <- run_cleanliness_analysis(data, cols, st$cleanliness, all_data = all_data)
      evs <- NULL
      if (!is.null(cols$field) && cols$field %in% names(data)) {
        e <- acceptance_evs(suppressWarnings(as.numeric(data[[cols$area]])), data[[cols$field]], all_data[[cols$field]],
                            st$fields_per_area, st$return_period, st$n_boot)
        res$evs <- e
        if (is.finite(e$return_level)) evs <- e
      }
      assessment <- if (has_limits) assess_acceptance(res, limits, res$evs) else NULL
      list(res = res, row = .folder_row(name, res, evs, assessment), assessment = assessment)
    }, error = function(e) conditionMessage(e))
    if (is.character(out)) {
      rows[[name]] <- data.frame(file = name, status = "failed", problem = out, stringsAsFactors = FALSE)
    } else {
      rows[[name]] <- out$row
      results[[name]] <- out$res
      assessments[[name]] <- out$assessment
    }
  }

  columns <- unique(unlist(lapply(rows, names)))
  summary <- do.call(rbind, lapply(rows, function(r) {
    r[setdiff(columns, names(r))] <- NA
    r[, columns, drop = FALSE]
  }))
  rownames(summary) <- NULL
  acceptance <- if (length(assessments)) {
    do.call(rbind, lapply(names(assessments), function(name) cbind(file = name, assessments[[name]]$table, stringsAsFactors = FALSE)))
  } else NULL
  warnings <- do.call(rbind, lapply(names(results), function(name) {
    w <- results[[name]]$notes$warnings
    if (length(w)) data.frame(file = name, text = w, stringsAsFactors = FALSE)
  }))
  list(summary = summary, acceptance = acceptance, results = results, assessments = assessments,
       warnings = warnings, limits = limits, filters = filters, cols = cols, settings = st)
}

#' Summarise every particle file of a directory
#'
#' Reads the files with [read_particle_table()] and hands them to
#' [run_folder_summary()]. Without `cols`, the columns are guessed from the
#' first file with [detect_column_roles()] - check `result$cols`.
#'
#' @param dir Directory holding the files.
#' @param pattern Regular expression of the file names to take.
#' @param cols Column names as for [run_cleanliness_analysis()], or `NULL`
#'   to guess them.
#' @param ... Passed to [run_folder_summary()] (`settings`, `limits`,
#'   `filters`).
#' @param report Path of an HTML report to write ([folder_report()]), or
#'   `NULL`.
#' @return The result of [run_folder_summary()], with `files` (data frame:
#'   `name`, `path`, `rows`) and `unreadable` (data frame: `name`,
#'   `problem`).
#' @export
process_folder <- function(dir, pattern = "\\.(xlsx|csv|txt|tsv)$", cols = NULL, ..., report = NULL) {
  if (!dir.exists(dir)) stop("The directory '", dir, "' does not exist.", call. = FALSE)
  paths <- sort(list.files(dir, pattern = pattern, full.names = TRUE, ignore.case = TRUE))
  paths <- paths[!grepl("^~\\$", basename(paths))]
  if (length(paths) == 0) stop("No particle files (", pattern, ") in '", dir, "'.", call. = FALSE)
  read <- lapply(paths, function(p) tryCatch(read_particle_table(p), error = function(e) conditionMessage(e)))
  failed <- vapply(read, is.character, logical(1))
  if (all(failed)) stop("None of the files could be read: ", read[[1]], call. = FALSE)
  tables <- read[!failed]
  names(tables) <- batch_labels(basename(paths[!failed]))

  if (is.null(cols)) {
    roles <- detect_column_roles(tables[[1]])
    cols <- as.list(roles[c("area", "length", "breadth", "x", "y", "direction", "field")])
    cols <- cols[!vapply(cols, is.na, logical(1))]
    missing <- setdiff(c("area", "length", "breadth", "x", "y"), names(cols))
    if (length(missing)) {
      stop("Could not find a column for: ", paste(missing, collapse = ", "), ". Pass `cols` yourself.", call. = FALSE)
    }
  }
  result <- run_folder_summary(tables, cols, ...)
  result$files <- data.frame(name = basename(paths[!failed]), path = paths[!failed],
                             rows = vapply(tables, nrow, integer(1)), stringsAsFactors = FALSE, row.names = NULL)
  result$unreadable <- data.frame(name = basename(paths[failed]), problem = unlist(read[failed]) %||% character(0),
                                  stringsAsFactors = FALSE)
  if (!is.null(report)) {
    provenance <- report_provenance(result$files$name, result$files$path, result$files$rows)
    writeLines(folder_report(result, provenance), report, useBytes = TRUE)
  }
  result
}
