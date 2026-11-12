# ---- Batch comparison: largest-inclusion estimate per batch ----
# The extreme-value fit of the EVS tab, run on each batch with the same
# control-area size and return period, so the return levels can be set side
# by side. A batch whose fit fails (too few control areas, no field IDs) is
# kept in the table with the reason, not dropped.

#' Extreme-value return level of each batch
#'
#' Per batch: control areas of `fields_per_area` SEM fields
#' ([assign_control_areas()]), block maxima, Gumbel fit and the return
#' level for `return_period` control areas with its bootstrap intervals
#' ([predict_evs_max()]).
#'
#' @param batches Named list of data frames (particles to analyse).
#' @param area_col,field_col Area and SEM field ID columns.
#' @param fields_per_area SEM fields per control area. Default 1.
#' @param return_period Return period T in control areas. Default 100.
#' @param all_batches The unfiltered data frames, so a filter does not
#'   shrink the number of inspected fields (default: `batches`).
#' @param n_boot Bootstrap samples. Default 2000.
#' @return A data frame with one row per batch: `batch`, `n_control_areas`,
#'   `return_level`, `ci_lower`, `ci_upper` (interval of the return level),
#'   `max_lower`, `max_upper` (interval for the largest inclusion),
#'   `largest_observed` (um, sqrt(area)), `gof_p_value`, `problem` (why the
#'   fit failed, else `NA`).
#' @export
batch_evs_table <- function(batches, area_col, field_col, fields_per_area = 1, return_period = 100,
                            all_batches = batches, n_boot = 2000) {
  rows <- lapply(names(batches), function(name) {
    d <- batches[[name]]
    out <- data.frame(batch = name, n_control_areas = NA_integer_, return_level = NA_real_, ci_lower = NA_real_,
                      ci_upper = NA_real_, max_lower = NA_real_, max_upper = NA_real_, largest_observed = NA_real_,
                      gof_p_value = NA_real_, problem = NA_character_, stringsAsFactors = FALSE)
    res <- tryCatch({
      areas <- assign_control_areas(d[[field_col]], fields_per_area, all_fields = all_batches[[name]][[field_col]])
      d$.control_area <- areas$control_area
      maxima <- compute_block_maxima(d, area_col, ".control_area")
      if (nrow(maxima) < 3) stop("fewer than 3 control areas with a valid inclusion")
      fit <- fit_evs_gumbel(maxima$sqrt_area_max)
      pred <- predict_evs_max(fit, return_period, n_boot = n_boot)
      gof <- gumbel_goodness_of_fit(fit)
      list(n = fit$n, pred = pred, largest = max(maxima$sqrt_area_max), gof = if (is.null(gof)) NA_real_ else gof$p_value)
    }, error = function(e) conditionMessage(e))
    if (is.character(res)) {
      out$problem <- res
      return(out)
    }
    out$n_control_areas <- res$n
    out$return_level <- res$pred$predicted
    out$ci_lower <- res$pred$ci_lower; out$ci_upper <- res$pred$ci_upper
    out$max_lower <- res$pred$lower; out$max_upper <- res$pred$upper
    out$largest_observed <- res$largest
    out$gof_p_value <- res$gof
    out
  })
  do.call(rbind, rows)
}
