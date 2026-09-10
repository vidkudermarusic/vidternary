#' vidternary: Modular Ternary Plot Analysis Tool
#'
#' An interactive Shiny application and supporting function library for
#' compositional / ternary-diagram analysis of nonmetallic-inclusion
#' chemistry from automated SEM/EDS data: ternary and hexagonal-ternary
#' plotting, log-ratio (CLR / ILR) compositional analysis, Mahalanobis and
#' Isolation-Forest outlier detection, IQR / Z-score / MAD filtering,
#' extreme-value (Murakami / ASTM E2283) inclusion rating, and spatial
#' point-pattern analysis (Clark-Evans, Ripley's K/L/G, CSR envelope,
#' kernel intensity).
#'
#' @keywords internal
"_PACKAGE"

## ---- Namespace imports --------------------------------------------------
## The Shiny UI/server layer calls shiny functions unqualified throughout
## (NS(), div(), reactive(), renderPlot(), observeEvent(), ...), so the
## whole namespace is imported rather than listing ~80 individual names.
## The base-package functions below are the ones R CMD check flags as
## "no visible global function definition".
#' @import shiny
#' @importFrom stats complete.cases cor cov mad mahalanobis median na.omit
#'   predict qchisq quantile sd setNames var
#' @importFrom graphics legend mtext plot.new text
#' @importFrom grDevices as.raster colorRampPalette rainbow
#' @importFrom utils capture.output head install.packages installed.packages
NULL

## Column names used only inside ggplot2 aes() mappings (non-standard
## evaluation), plus the parent-scope bindings that prepare_ternary_plot_
## data()'s list2env() restructuring merges back in - none are real
## globals; this just tells R CMD check not to flag them.
utils::globalVariables(c(
  "PC1", "PC2", "label",
  "r", "theo", "obs", "obs_minus_r", "lo_minus_r", "hi_minus_r",
  "intensity", "x", "y",
  "nnd",
  "sqrt_area_max",
  "pct", "n",
  "all_selected_elements", "file_base", "ternary_points1", "matrika"
))
