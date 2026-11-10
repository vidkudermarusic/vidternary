# ---- Inclusion add-on: removing the steel matrix from EDS analyses ----
# A small inclusion is analysed together with the steel around it, so its
# EDS result contains the matrix element (Fe) and the alloying elements in
# their steel ratio. This removes that contribution and rescales what is
# left to 100 wt%, and flags particles that are matrix or carry no
# inclusion signal.

#' Estimate the alloying-to-matrix ratios of the steel from the data
#'
#' Takes the particles with the highest matrix-element share (the ones that
#' are mostly steel) and returns, per alloying element, the median of
#' `wt(alloy) / wt(matrix)` over them.
#'
#' @param wt Numeric matrix/data frame of element wt%, columns named by symbol.
#' @param matrix_element Symbol of the matrix element (e.g. `"Fe"`).
#' @param alloy_elements Symbols of the alloying elements.
#' @param top_fraction Share of particles (highest matrix wt%) used.
#' @return A named numeric vector, alloy symbol -> ratio to the matrix element.
#' @export
estimate_matrix_ratios <- function(wt, matrix_element, alloy_elements, top_fraction = 0.1) {
  w <- as.matrix(wt)
  m <- w[, matrix_element]
  sel <- is.finite(m) & m > 0 & m >= stats::quantile(m[is.finite(m)], 1 - top_fraction)
  if (sum(sel) < 3) stop("Too few particles with a matrix signal to estimate the steel composition; ",
                         "give `steel_composition` instead.", call. = FALSE)
  vapply(alloy_elements, function(e) stats::median(w[sel, e] / m[sel]), numeric(1))
}

#' Remove the steel matrix from EDS analyses
#'
#' Two modes:
#' * `"matrix_only"`: the matrix element is set to 0.
#' * `"matrix_and_alloys"`: additionally, each alloying element `j` is
#'   reduced by the steel's share carried by the matrix signal,
#'   `wt_j - wt_matrix * (steel_j / steel_matrix)` (negative results become 0).
#'   The steel composition comes from `steel_composition` (named wt%, must
#'   include the matrix element) or, if `NULL`, is estimated with
#'   [estimate_matrix_ratios()].
#'
#' `ignore_elements` (default carbon, which is mostly contamination on
#' polished samples) are set to 0 as well. What remains is rescaled to
#' 100 wt%. NA values are treated as 0.
#'
#' Two flags are returned per particle: `matrix_particle` (matrix share of
#' the original analysis above `max_matrix_pct`) and `no_signal` (less than
#' `min_residual_pct` of the original analysis is left after removal, so the
#' rescaled composition would only amplify noise).
#'
#' @param wt Data frame of element wt%, columns named by element symbol.
#' @param matrix_element Symbol of the matrix element, or `NULL` for none.
#' @param mode `"matrix_only"` or `"matrix_and_alloys"`.
#' @param alloy_elements Alloying elements to correct in the second mode;
#'   default: the elements named in `steel_composition`, or, without it,
#'   those of Cr, Ni, Mo, Cu, Co, W present in `wt`.
#' @param steel_composition Named numeric wt% of the steel, or `NULL`.
#' @param ignore_elements Elements set to 0 before rescaling.
#' @param max_matrix_pct Particles with a larger matrix share are flagged.
#' @param min_residual_pct Particles with a smaller remainder are flagged.
#' @param top_fraction Passed to [estimate_matrix_ratios()].
#' @return A list: `wt` (rescaled data frame, NA rows for empty remainders),
#'   `matrix_pct`, `residual_pct`, `matrix_particle`, `no_signal`, `ratios`,
#'   `clipped_by_element`, `settings`.
#' @export
remove_matrix <- function(wt, matrix_element = NULL,
                          mode = c("matrix_only", "matrix_and_alloys"),
                          alloy_elements = NULL, steel_composition = NULL,
                          ignore_elements = "C", max_matrix_pct = 80,
                          min_residual_pct = 5, top_fraction = 0.1) {
  mode <- match.arg(mode)
  w <- as.matrix(wt)
  storage.mode(w) <- "double"
  w[is.na(w)] <- 0
  n <- nrow(w)
  total0 <- rowSums(w)
  if (!is.null(matrix_element) && !matrix_element %in% colnames(w)) {
    stop("Matrix element ", matrix_element, " has no column in the data.", call. = FALSE)
  }

  adj <- w
  ratios <- NULL
  clipped <- integer(0)
  matrix_pct <- rep(0, n)
  if (!is.null(matrix_element)) {
    matrix_pct <- ifelse(total0 > 0, 100 * w[, matrix_element] / total0, 0)
    if (mode == "matrix_and_alloys") {
      alloys <- if (!is.null(alloy_elements)) alloy_elements
                else if (!is.null(steel_composition)) intersect(names(steel_composition), colnames(w))
                else intersect(c("Cr", "Ni", "Mo", "Cu", "Co", "W"), colnames(w))
      alloys <- setdiff(alloys, matrix_element)
      missing_cols <- setdiff(alloys, colnames(w))
      if (length(missing_cols) > 0) stop("Alloying element(s) without a column: ", paste(missing_cols, collapse = ", "), call. = FALSE)
      if (length(alloys) == 0) stop("No alloying elements to correct; choose some or use mode 'matrix_only'.", call. = FALSE)
      ratios <- if (is.null(steel_composition)) {
        estimate_matrix_ratios(w, matrix_element, alloys, top_fraction)
      } else {
        need <- c(matrix_element, alloys)
        if (!all(need %in% names(steel_composition))) {
          stop("`steel_composition` must give ", paste(need, collapse = ", "), ".", call. = FALSE)
        }
        unlist(lapply(alloys, function(e) steel_composition[[e]] / steel_composition[[matrix_element]]))
      }
      names(ratios) <- alloys
      clipped <- integer(length(alloys)); names(clipped) <- alloys
      for (e in alloys) {
        d <- w[, e] - w[, matrix_element] * ratios[[e]]
        clipped[[e]] <- sum(d < 0 & w[, e] > 0)
        adj[, e] <- pmax(d, 0)
      }
    }
    adj[, matrix_element] <- 0
  }
  ign <- intersect(ignore_elements, colnames(adj))
  if (length(ign) > 0) adj[, ign] <- 0

  s <- rowSums(adj)
  residual_pct <- ifelse(total0 > 0, 100 * s / total0, NA_real_)
  out <- adj / s * 100
  out[!is.finite(s) | s <= 0, ] <- NA_real_

  list(
    wt = as.data.frame(out),
    matrix_pct = matrix_pct,
    residual_pct = residual_pct,
    matrix_particle = matrix_pct > max_matrix_pct,
    no_signal = !is.finite(residual_pct) | residual_pct < min_residual_pct,
    ratios = ratios,
    clipped_by_element = clipped,
    settings = list(matrix_element = matrix_element, mode = mode, ignore_elements = ignore_elements,
                    max_matrix_pct = max_matrix_pct, min_residual_pct = min_residual_pct)
  )
}
