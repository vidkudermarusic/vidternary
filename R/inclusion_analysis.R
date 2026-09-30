# ---- Inclusion add-on: analysing a dataset with a preset ----

# Amount (mass or mole) of each of the three corners, per particle (n x 3).
.incl_corner_values <- function(preset, wt, conv, weights, compounds, basis = preset$basis) {
  cols <- lapply(preset$corners, function(tok) {
    if (preset$kind == "compound") {
      src <- if (basis == "mass") conv$mass else conv$moles
      rowSums(src[, corner_compounds(tok, compounds), drop = FALSE])
    } else {
      els <- intersect(strsplit(tok, "+", fixed = TRUE)[[1]], colnames(wt))
      if (length(els) == 0) return(rep(0, nrow(wt)))
      amt <- sweep(as.matrix(wt[, els, drop = FALSE]), 2, if (basis == "mass") 1 else weights[els], "/")
      rowSums(amt)
    }
  })
  m <- do.call(cbind, cols)
  colnames(m) <- preset$corners
  m
}

#' Analyse a dataset with an inclusion diagram preset
#'
#' Pipeline: pick the element wt% columns, remove the steel matrix
#' ([remove_matrix()]), resolve the rest into compounds
#' ([elements_to_compounds()], compound presets only), sum the corners of
#' the preset, keep the particles whose corners make up at least
#' `threshold_pct` of the particle's total, and classify them
#' ([classify_inclusions()]). Nothing is modified in `data`; row numbers of
#' `data` are returned so results can be joined back.
#'
#' The threshold is measured on mass: for compound presets as the corner
#' compounds' share of all compounds plus the elements left over; for
#' element presets as the corner elements' share of the (matrix-corrected)
#' analysis.
#'
#' @param data Data frame with element wt% columns.
#' @param preset Preset id or an object from [get_inclusion_preset()].
#' @param element_cols Named character vector element -> column; default:
#'   detected with [inclusion_element_columns()].
#' @param matrix_element,matrix_mode,alloy_elements,steel_composition,ignore_elements,max_matrix_pct,min_residual_pct
#'   See [remove_matrix()]. `matrix_element = NULL` skips matrix removal.
#' @param s_order,ti_as,n_order See [elements_to_compounds()].
#' @param basis `"mass"` or `"mole"`; default the preset's.
#' @param threshold_pct Coverage threshold in percent; default the preset's.
#' @return A list of class `inclusion_result`: `preset`, `coords` (data
#'   frame `A`, `B`, `C`, `NA` for particles not kept, all rows of `data`),
#'   `keep`, `class`, `coverage_pct`, `excluded` (why each dropped particle
#'   was dropped), `summary` (count and share per class), `matrix`,
#'   `conversion`, `settings` (text lines for a plot note or report).
#' @export
analyze_inclusion_preset <- function(data, preset, element_cols = NULL,
                                     matrix_element = NULL,
                                     matrix_mode = c("matrix_only", "matrix_and_alloys"),
                                     alloy_elements = NULL, steel_composition = NULL,
                                     ignore_elements = "C", max_matrix_pct = 80, min_residual_pct = 5,
                                     s_order = c("Ca", "Mn"), ti_as = c("TiN", "TiO2", "by_N"),
                                     n_order = c("Ti", "Al"), basis = NULL, threshold_pct = NULL) {
  matrix_mode <- match.arg(matrix_mode)
  ti_as <- match.arg(ti_as)
  weights <- inclusion_atomic_weights()
  compounds <- inclusion_compounds(weights)
  if (is.character(preset)) preset <- get_inclusion_preset(preset, basis = basis, weights = weights)
  else if (!is.null(basis) && basis != preset$basis) preset <- get_inclusion_preset(preset$id, basis = basis, weights = weights)
  if (is.null(threshold_pct)) threshold_pct <- preset$threshold_pct

  if (is.null(element_cols)) element_cols <- inclusion_element_columns(names(data), weights)
  if (length(element_cols) == 0) stop("No element wt% columns found; name them with `element_cols`.", call. = FALSE)
  wt <- as.data.frame(lapply(element_cols, function(cn) suppressWarnings(as.numeric(data[[cn]]))))
  names(wt) <- names(element_cols)

  mx <- remove_matrix(wt, matrix_element, matrix_mode, alloy_elements, steel_composition,
                      ignore_elements, max_matrix_pct, min_residual_pct)
  conv <- NULL
  if (preset$kind == "compound") {
    conv <- elements_to_compounds(mx$wt, s_order, ti_as, n_order, ignore_elements, weights)
    total_mass <- conv$total
  } else {
    total_mass <- unname(rowSums(mx$wt))
  }
  corner_mass <- .incl_corner_values(preset, mx$wt, conv, weights, compounds, basis = "mass")
  corner <- if (preset$basis == "mass") corner_mass else .incl_corner_values(preset, mx$wt, conv, weights, compounds)
  corner_sum <- unname(rowSums(corner))
  coverage <- 100 * unname(rowSums(corner_mass)) / total_mass

  low_cov <- !is.finite(coverage) | coverage < threshold_pct
  excluded <- ifelse(mx$matrix_particle, "matrix particle",
              ifelse(mx$no_signal, "no inclusion signal after matrix removal",
              ifelse(!is.finite(corner_sum) | corner_sum <= 0, "corners empty",
              ifelse(low_cov, "corners below threshold", NA_character_))))
  keep <- is.na(excluded)

  coords <- data.frame(A = corner[, 1] / corner_sum, B = corner[, 2] / corner_sum, C = corner[, 3] / corner_sum)
  coords[!keep, ] <- NA_real_
  cls <- classify_inclusions(coords, preset)
  found <- cls[!is.na(cls)]
  lv <- unique(found)
  tab <- data.frame(class = lv, n = as.integer(table(factor(found, levels = lv))), stringsAsFactors = FALSE)
  tab$percent <- if (sum(tab$n) > 0) round(100 * tab$n / sum(tab$n), 2) else numeric(0)
  tab <- tab[order(-tab$n), , drop = FALSE]; rownames(tab) <- NULL

  settings <- c(
    sprintf("Preset: %s (%s basis, %s classification)", preset$name, preset$basis, preset$classification),
    sprintf("Matrix: %s", if (is.null(matrix_element)) "not removed" else
      sprintf("%s removed (%s%s)", matrix_element, gsub("_", " ", matrix_mode), if (length(ignore_elements)) paste0("; ignored: ", paste(ignore_elements, collapse = ",")) else "")),
    if (preset$kind == "compound") sprintf("Sulfur to %s; Ti as %s", paste(s_order, collapse = " then "), ti_as),
    sprintf("Kept %d of %d particles (corners >= %g%% of total)", sum(keep), nrow(data), threshold_pct)
  )
  structure(list(
    preset = preset, coords = coords, keep = keep, class = cls, coverage_pct = coverage,
    excluded = excluded, summary = tab, matrix = mx, conversion = conv,
    settings = settings[!vapply(settings, is.null, logical(1))]
  ), class = "inclusion_result")
}
