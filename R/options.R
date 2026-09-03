#' App-wide options and constants
#'
#' Point-size bounds shared by every plot's point-size legend/scaling
#' (ternary_plot_save.R, ternary_plot_preview.R). Everything else that used
#' to live in this file (a duplicate, drifted REQUIRED_PACKAGES list, a
#' duplicate CACHE_TIMEOUT, unused plotting/filter/analysis/export defaults)
#' had no reader anywhere in the app - every real call site hardcodes its
#' own default instead of reading a shared constant - so it was removed
#' rather than kept as unused scaffolding.

# Point size settings
MIN_POINT_SIZE <- 0.1
MAX_POINT_SIZE <- 2.5
