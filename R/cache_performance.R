# ---- Progress & Performance Monitoring (split out of cache.R) ----
# These track named operations (progress steps, timing, memory) independently
# of the data/plot caching system in cache.R.
#
# start_progress/update_progress/start_performance_monitor/
# end_performance_monitor/get_performance_summary used to be defined here
# too, but were always shadowed by same-named, differently-implemented
# functions in helpers.R (R sources package files alphabetically, and
# "helpers*" sorts after "cache*") - removed as genuinely dead code rather
# than left in place unreachable. complete_progress()/get_progress_summary()
# below have no such collision and remain reachable, but note they still
# read/write the package-wide `progress_tracker` binding, whose *initial*
# shape is set by helpers.R's simpler start_progress() (flat fields, no
# `$operations` list) - so until something calls the richer initializer
# this file used to provide, these two will typically see no operations to
# report.

#' Mark a tracked operation as finished
#'
#' Records an end time/duration/status against `operation_name` in the
#' shared `progress_tracker` state. See this file's header note: the
#' operation-name-keyed shape this function expects is only populated by
#' this file's own (now-removed) `start_progress()`; the currently-active
#' `start_progress()` in helpers.R uses a simpler, flat shape, so this
#' typically has nothing to record today.
#'
#' @param operation_name Name of the operation to complete. Defaults to
#'   `progress_tracker$current_operation` if not supplied.
#' @param status Status label to record. Default `"completed"`.
#' @return `NULL`, invisibly.
#' @export
complete_progress <- function(operation_name = NULL, status = "completed") {
  if (is.null(operation_name)) {
    operation_name <- progress_tracker$current_operation
  }

  if (!is.null(operation_name) && !is.null(progress_tracker$operations[[operation_name]])) {
    end_time <- Sys.time()
    duration <- as.numeric(difftime(end_time, progress_tracker$operations[[operation_name]]$start_time, units = "secs"))
    progress_tracker$operations[[operation_name]]$status <- status
    progress_tracker$operations[[operation_name]]$end_time <- end_time
    progress_tracker$operations[[operation_name]]$duration <- duration

    cat(sprintf("Completed: %s (%.2f seconds)\n", operation_name, duration))
  }
}

#' Summarize all tracked operations as human-readable text
#'
#' See `complete_progress()`'s note on the currently-active `progress_tracker`
#' shape - typically reports "No operations tracked" today.
#'
#' @return A single formatted character string.
#' @export
get_progress_summary <- function() {
  if (length(progress_tracker$operations) == 0) {
    return("No operations tracked")
  }

  summary_text <- "Progress Summary:\n"
  for (op_name in names(progress_tracker$operations)) {
    op <- progress_tracker$operations[[op_name]]
    if (op$status == "running") {
      progress_pct <- round((op$current_step / op$total_steps) * 100, 1)
      summary_text <- paste0(summary_text, sprintf("  %s: %s (%d%%)\n",
                                                   op_name, op$status, progress_pct))
    } else {
      summary_text <- paste0(summary_text, sprintf("  %s: %s (%.2fs)\n",
                                                   op_name, op$status, op$duration))
    }
  }
  return(summary_text)
}

# ---- Performance Monitoring System ----
# An environment, not a plain list - see progress_tracker's comment in
# helpers.R for why (a plain list mutated via <<- can't be reassigned once
# the package namespace is locked).
performance_monitor <- new.env()
performance_monitor$operations <- list()
performance_monitor$memory_usage <- list()
performance_monitor$start_time <- Sys.time()

#' Record a memory-usage snapshot
#'
#' Appends a `{timestamp, used, gc_count}` snapshot (from `gc()`) to
#' `performance_monitor$memory_usage`, keeping only the last 100.
#'
#' @return The snapshot just recorded, as a list.
#' @export
monitor_memory_usage <- function() {
  current_memory <- gc(reset = FALSE)
  memory_info <- list(
    timestamp = Sys.time(),
    used = current_memory[2, 3],
    gc_count = current_memory[2, 4]
  )

  performance_monitor$memory_usage[[length(performance_monitor$memory_usage) + 1]] <- memory_info

  # Keep only last 100 memory snapshots
  if (length(performance_monitor$memory_usage) > 100) {
    performance_monitor$memory_usage <- tail(performance_monitor$memory_usage, 100)
  }

  return(memory_info)
}

# Enhanced performance monitoring with detailed metrics
start_performance_monitor_enhanced <- function(operation_name, include_memory = TRUE, include_system = TRUE) {
  if (!exists("performance_monitors", envir = .GlobalEnv)) {
    .GlobalEnv$performance_monitors <- new.env()
  }

  monitor_info <- list(
    start_time = Sys.time(),
    operation_name = operation_name
  )

  if (include_memory) {
    monitor_info$memory_start <- gc(reset = TRUE)
  }

  if (include_system) {
    monitor_info$system_start <- list(
      cpu_time = proc.time(),
      memory_limit = memory.limit(),
      memory_size = memory.size()
    )
  }

  .GlobalEnv$performance_monitors[[operation_name]] <- monitor_info

  debug_log("DEBUG: Started enhanced performance monitoring for: %s", operation_name)
  return(monitor_info)
}

end_performance_monitor_enhanced <- function(operation_name, include_memory = TRUE, include_system = TRUE) {
  if (!exists("performance_monitors", envir = .GlobalEnv) ||
      !exists(operation_name, envir = .GlobalEnv$performance_monitors)) {
    warning("No performance monitor found for: ", operation_name)
    return(NULL)
  }

  monitor <- .GlobalEnv$performance_monitors[[operation_name]]
  end_time <- Sys.time()

  result <- list(
    operation = operation_name,
    start_time = monitor$start_time,
    end_time = end_time,
    duration = as.numeric(difftime(end_time, monitor$start_time, units = "secs"))
  )

  if (include_memory) {
    memory_end <- gc()
    result$memory_metrics <- list(
      memory_used_mb = round((memory_end[2, 3] - monitor$memory_start[2, 3]) / 1024^2, 2),
      peak_memory_mb = round(memory_end[2, 6] / 1024^2, 2),
      final_memory_mb = round(memory_end[2, 3] / 1024^2, 2)
    )
  }

  if (include_system) {
    system_end <- list(
      cpu_time = proc.time(),
      memory_limit = memory.limit(),
      memory_size = memory.size()
    )

    result$system_metrics <- list(
      cpu_time_user = system_end$cpu_time[1] - monitor$system_start$cpu_time[1],
      cpu_time_system = system_end$cpu_time[2] - monitor$system_start$cpu_time[2],
      cpu_time_elapsed = system_end$cpu_time[3] - monitor$system_start$cpu_time[3],
      memory_limit_gb = round(system_end$memory_limit / 1024^3, 2),
      memory_size_gb = round(system_end$memory_size / 1024^3, 2)
    )
  }

  # Performance rating
  if (result$duration < 1) {
    result$performance_rating <- "Excellent"
  } else if (result$duration < 5) {
    result$performance_rating <- "Good"
  } else if (result$duration < 30) {
    result$performance_rating <- "Acceptable"
  } else {
    result$performance_rating <- "Slow"
  }

  debug_log("DEBUG: Enhanced performance monitoring completed for %s: %.2f seconds (%s)",
            operation_name, result$duration, result$performance_rating)

  # Clean up
  rm(list = operation_name, envir = .GlobalEnv$performance_monitors)

  return(result)
}

# Global performance summary
get_global_performance_summary <- function() {
  if (!exists("performance_monitors", envir = .GlobalEnv)) {
    return("No global performance monitors active")
  }

  active_monitors <- ls(.GlobalEnv$performance_monitors)
  if (length(active_monitors) == 0) {
    return("No active global performance monitors")
  }

  summary_text <- paste("Global Performance Monitors (", length(active_monitors), "):\n", sep = "")

  for (monitor_name in active_monitors) {
    monitor <- .GlobalEnv$performance_monitors[[monitor_name]]
    start_time <- monitor$start_time
    duration <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

    summary_text <- paste(summary_text,
                         sprintf("  %s: Running for %.2f seconds\n", monitor_name, duration),
                         sep = "")
  }

  return(summary_text)
}
