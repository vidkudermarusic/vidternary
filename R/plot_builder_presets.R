# ---- Plot Builder Presets ----
# User-saved plot-builder configurations, persisted to their own JSON file
# (kept separate from ternary_config.json to avoid touching the main config
# schema). Mirrors the load_config()/save_config() pattern in config.R.

PLOT_BUILDER_PRESETS_FILE <- "plot_builder_presets.json"

#' Load saved Plot Builder presets from `plot_builder_presets.json`
#'
#' @return A named list of saved presets, or an empty list if the file
#'   doesn't exist or fails to parse.
#' @export
load_builder_presets <- function() {
  if (file.exists(PLOT_BUILDER_PRESETS_FILE)) {
    tryCatch({
      presets <- jsonlite::fromJSON(PLOT_BUILDER_PRESETS_FILE, simplifyVector = FALSE)
      cat("Plot builder presets loaded from:", PLOT_BUILDER_PRESETS_FILE, "\n")
      return(presets)
    }, error = function(e) {
      cat("Warning: Could not load plot builder presets:", e$message, "\n")
      return(list())
    })
  }
  list()
}

#' Save Plot Builder presets to `plot_builder_presets.json`
#'
#' @param presets Named list of presets to write.
#' @return `NULL`, invisibly. Called for its file-writing side effect.
#' @export
save_builder_presets <- function(presets) {
  tryCatch({
    jsonlite::write_json(presets, PLOT_BUILDER_PRESETS_FILE, pretty = TRUE, auto_unbox = TRUE, null = "null")
    cat("Plot builder presets saved to:", PLOT_BUILDER_PRESETS_FILE, "\n")
  }, error = function(e) {
    cat("Warning: Could not save plot builder presets:", e$message, "\n")
  })
}

#' Apply one preset change against whatever is currently saved on disk
#'
#' `PLOT_BUILDER_PRESETS_FILE` is one bare path shared by every Shiny
#' session in the same R process; each session's own `rv$plot_presets` is
#' loaded once at server-creation time and never refreshed. Re-reading the
#' file immediately before writing keeps a save from clobbering a preset a
#' *different* session wrote in the meantime - narrow enough that a single
#' shared JSON file for a local-first Shiny app doesn't warrant real file
#' locking on top of it.
#'
#' @param mutate A function taking the freshly-loaded on-disk presets list
#'   and returning the updated list to save. Called with the real current
#'   contents, not the caller's own possibly-stale in-memory copy.
#' @return The updated, already-saved presets list - store this back into
#'   the caller's own `rv$plot_presets` so its in-memory copy reflects
#'   what's now really on disk, not just its own one change.
#' @export
save_builder_preset_change <- function(mutate) {
  current <- load_builder_presets()
  updated <- mutate(current)
  save_builder_presets(updated)
  updated
}
