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
