# ---- Inclusion add-on: classification rules and their boundary lines ----
# A preset classifies points either by an ordered list of rules (straight
# lines in the ternary) or by the nearest reference phase. Both give the
# class of every point and the line segments to draw. Rule text is parsed
# with a small fixed grammar, never evaluated as code.
#
# Conditions (joined with ";", all must hold), written with the corner
# labels of the preset, in the plot's basis (fractions summing to 1):
#   <corner> <op> <value>                  e.g.  MgO>=0.30
#   <corner1>/(<corner1>+<corner2>) <op> <value>   e.g.  CaO/(CaO+Al2O3)<0.15
# The first form is a line parallel to the edge opposite the corner; the
# second a line through the third corner. op is one of >= <= > <.

.incl_compare <- function(x, op, v) {
  switch(op, ">=" = x >= v, "<=" = x <= v, ">" = x > v, "<" = x < v)
}

#' Parse the conditions of one rule
#'
#' @param text Conditions joined by `;`.
#' @param labels The three corner labels (A, B, C order).
#' @return A list of conditions, each `list(kind, k or i/j, op, value)`.
#' @export
parse_rule_conditions <- function(text, labels) {
  parts <- trimws(strsplit(text, ";", fixed = TRUE)[[1]])
  if (length(parts) == 0 || any(parts == "")) stop("Empty rule condition in '", text, "'.", call. = FALSE)
  tok <- "([A-Za-z0-9_]+)"
  num <- "([0-9]*\\.?[0-9]+)"
  re_thr <- paste0("^\\s*", tok, "\\s*(>=|<=|>|<)\\s*", num, "\\s*$")
  re_rat <- paste0("^\\s*", tok, "\\s*/\\s*\\(\\s*", tok, "\\s*\\+\\s*", tok, "\\s*\\)\\s*(>=|<=|>|<)\\s*", num, "\\s*$")
  idx <- function(t) {
    k <- match(t, labels)
    if (is.na(k)) stop("'", t, "' is not a corner of this diagram (", paste(labels, collapse = ", "), ").", call. = FALSE)
    k
  }
  lapply(parts, function(p) {
    m <- regmatches(p, regexec(re_rat, p))[[1]]
    if (length(m) == 6) {
      if (m[2] != m[3]) stop("Ratio '", p, "' must have the form X/(X+Y).", call. = FALSE)
      i <- idx(m[2]); j <- idx(m[4])
      if (i == j) stop("Ratio '", p, "' needs two different corners.", call. = FALSE)
      return(list(kind = "ratio", i = i, j = j, op = m[5], value = as.numeric(m[6])))
    }
    m <- regmatches(p, regexec(re_thr, p))[[1]]
    if (length(m) == 4) return(list(kind = "threshold", k = idx(m[2]), op = m[3], value = as.numeric(m[4])))
    stop("Cannot read the condition '", p, "'. Use e.g. MgO>=0.30 or CaO/(CaO+Al2O3)<0.15.", call. = FALSE)
  })
}

#' Which points satisfy one parsed condition
#'
#' @param cond A condition from [parse_rule_conditions()].
#' @param coords Data frame with columns `A`, `B`, `C`.
#' @return Logical vector; `NA` (e.g. 0/0 in a ratio) counts as `FALSE`.
#' @export
eval_rule_condition <- function(cond, coords) {
  M <- as.matrix(coords[, c("A", "B", "C")])
  x <- if (cond$kind == "threshold") M[, cond$k] else M[, cond$i] / (M[, cond$i] + M[, cond$j])
  r <- .incl_compare(x, cond$op, cond$value)
  r[is.na(r)] <- FALSE
  r
}

# Barycentric <-> plane coordinates of an equilateral triangle (A at the
# origin, B at (1, 0)); only used for distances and half-plane clipping.
.incl_bary_to_xy <- function(M) cbind(x = M[, 2] + 0.5 * M[, 3], y = sqrt(3) / 2 * M[, 3])
.incl_xy_to_bary <- function(xy) {
  cc <- xy[, 2] / (sqrt(3) / 2)
  bb <- xy[, 1] - 0.5 * cc
  cbind(A = 1 - bb - cc, B = bb, C = cc)
}

#' Class of every point of a preset
#'
#' `"rules"` presets: the rules in priority order, the first whose
#' conditions all hold wins, the rest are `"Other"`. `"nearest"` presets:
#' the reference phase closest in the ternary plane. Points with `NA`
#' coordinates get `NA`.
#'
#' @param coords Data frame with columns `A`, `B`, `C` (fractions).
#' @param preset A preset from [get_inclusion_preset()].
#' @return A character vector of class names.
#' @export
classify_inclusions <- function(coords, preset) {
  M <- as.matrix(coords[, c("A", "B", "C")])
  cls <- rep(NA_character_, nrow(M))
  ok <- stats::complete.cases(M)
  if (preset$classification == "none" || !any(ok)) return(cls)
  if (preset$classification == "rules") {
    cls[ok] <- "Other"
    open <- ok
    for (r in seq_len(nrow(preset$rules))) {
      conds <- parse_rule_conditions(preset$rules$conditions[r], preset$labels)
      hit <- open
      for (cd in conds) hit <- hit & eval_rule_condition(cd, coords)
      cls[hit] <- preset$rules$class[r]
      open <- open & !hit
    }
  } else {
    refs <- preset$references
    if (nrow(refs) == 0) stop("Preset '", preset$id, "' has no reference phases to classify by.", call. = FALSE)
    P <- .incl_bary_to_xy(as.matrix(refs[, c("A", "B", "C")]))
    Q <- .incl_bary_to_xy(M[ok, , drop = FALSE])
    d2 <- outer(Q[, 1], P[, 1], "-")^2 + outer(Q[, 2], P[, 2], "-")^2
    cls[ok] <- refs$phase[max.col(-d2, ties.method = "first")]
  }
  cls
}

# Sutherland-Hodgman clip of a convex polygon (matrix of x,y rows) to the
# half-plane a*x + b*y <= c.
.incl_clip <- function(poly, a, b, c) {
  n <- nrow(poly)
  if (n == 0) return(poly)
  out <- NULL
  for (i in seq_len(n)) {
    p <- poly[i, ]; q <- poly[i %% n + 1, ]
    fp <- a * p[1] + b * p[2] - c; fq <- a * q[1] + b * q[2] - c
    if (fp <= 0) out <- rbind(out, p)
    if ((fp < 0 && fq > 0) || (fp > 0 && fq < 0)) out <- rbind(out, p + (q - p) * fp / (fp - fq))
  }
  if (is.null(out)) matrix(numeric(0), 0, 2) else out
}

#' Boundary lines between the cells of the nearest-phase classification
#'
#' @param refs Reference points (`A`, `B`, `C` columns).
#' @return A list of segments, each a 2 x 3 matrix of barycentric ends.
#' @export
nearest_boundary_segments <- function(refs) {
  P <- .incl_bary_to_xy(as.matrix(refs[, c("A", "B", "C")]))
  P <- P[!duplicated(round(P, 9)), , drop = FALSE]
  if (nrow(P) < 2) return(list())
  tri <- .incl_bary_to_xy(rbind(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1)))
  segs <- list(); keys <- character(0)
  for (i in seq_len(nrow(P))) {
    poly <- tri
    for (j in setdiff(seq_len(nrow(P)), i)) {
      poly <- .incl_clip(poly, 2 * (P[j, 1] - P[i, 1]), 2 * (P[j, 2] - P[i, 2]),
                         sum(P[j, ]^2) - sum(P[i, ]^2))
    }
    if (nrow(poly) < 2) next
    B <- .incl_xy_to_bary(poly)
    for (e in seq_len(nrow(B))) {
      s <- rbind(B[e, ], B[e %% nrow(B) + 1, ])
      on_edge <- any(colSums(abs(s) < 1e-9) == 2)
      if (on_edge || sum(abs(s[1, ] - s[2, ])) < 1e-9) next
      key <- paste(sort(c(paste(round(s[1, ], 6), collapse = ","), paste(round(s[2, ], 6), collapse = ","))), collapse = "|")
      if (key %in% keys) next
      keys <- c(keys, key)
      segs[[length(segs) + 1]] <- unname(s)
    }
  }
  segs
}

#' Line segments to draw for a preset
#'
#' Rule presets: one segment per distinct condition (threshold lines are
#' parallel to an edge, ratio lines pass through a corner). Nearest presets:
#' the cell boundaries. Segments are 2 x 3 matrices in A, B, C fractions.
#'
#' @param preset A preset from [get_inclusion_preset()].
#' @return A list of 2 x 3 matrices.
#' @export
preset_boundary_segments <- function(preset) {
  if (preset$classification == "nearest") return(nearest_boundary_segments(preset$references))
  if (preset$classification != "rules") return(list())
  e <- function(k) { v <- c(0, 0, 0); v[k] <- 1; v }
  segs <- list(); keys <- character(0)
  for (txt in preset$rules$conditions) {
    for (cd in parse_rule_conditions(txt, preset$labels)) {
      v <- cd$value
      if (v <= 0 || v >= 1) next
      if (cd$kind == "threshold") {
        o <- setdiff(1:3, cd$k)
        s <- rbind(v * e(cd$k) + (1 - v) * e(o[1]), v * e(cd$k) + (1 - v) * e(o[2]))
        key <- paste("t", cd$k, round(v, 6))
      } else {
        l <- setdiff(1:3, c(cd$i, cd$j))
        s <- rbind(e(l), v * e(cd$i) + (1 - v) * e(cd$j))
        lo <- min(cd$i, cd$j)
        key <- paste("r", lo, max(cd$i, cd$j), round(if (cd$i == lo) v else 1 - v, 6))
      }
      if (key %in% keys) next
      keys <- c(keys, key)
      segs[[length(segs) + 1]] <- s
    }
  }
  segs
}

#' Colours for the classes of a preset
#'
#' Rule presets use the colours of `rules.csv`; other classes (and
#' `"Other"`) get a colour-blind-friendly qualitative palette / grey.
#'
#' @param preset A preset from [get_inclusion_preset()].
#' @param classes Class names to colour (default: all the preset knows).
#' @return A named character vector, class -> colour.
#' @export
inclusion_class_colors <- function(preset, classes = NULL) {
  pal <- c("#0072B2", "#E69F00", "#009E73", "#D55E00", "#CC79A7", "#56B4E9", "#8E5572",
           "#0B7A75", "#B8860B", "#6A3D9A", "#A6761D", "#1B9E77")
  if (preset$classification == "rules") {
    cols <- setNames(preset$rules$colour, preset$rules$class)
  } else {
    nm <- preset$references$phase
    cols <- setNames(rep(pal, length.out = length(nm)), nm)
  }
  cols <- c(cols, Other = "#999999")
  if (!is.null(classes)) cols <- cols[intersect(names(cols), classes)]
  cols
}
