# ---- Steel cleanliness: chart rating per field (ISO 4967 / ASTM E45) ----
# The chart methods rate square fields of 0.5 mm2. An automated SEM run
# uses much smaller fields, so the inspected area is re-cut here into
# 0.5 mm2 rating fields from the particle coordinates. Each field gets an
# index per type and width series; method A reports the worst field,
# method B the mean over all fields.

# Rating series in report order.
.rating_series <- c("A fine", "A thick", "B fine", "B thick", "C fine", "C thick", "D fine", "D thick", "DS")

# Number of rating fields an inspected area would be cut into, checked
# against a limit BEFORE anything is built per field. Coordinates in um that
# are read as mm make a 2 x 1 mm scan 2000 x 1000 mm: four million fields,
# and the tables built per field then exhaust the memory. `bbox` is
# c(xmin, xmax, ymin, ymax) in um; `unit` is the coordinate unit chosen by
# the user ("mm" or "um"), named in the message when it is known.
.check_rating_field_count <- function(bbox, field_area_mm2, max_fields, unit = NULL) {
  side <- sqrt(field_area_mm2) * 1000
  width <- bbox[2] - bbox[1]
  height <- bbox[4] - bbox[3]
  tol <- 1e-9 * side
  n <- max(1, ceiling((width - tol) / side)) * max(1, ceiling((height - tol) / side))
  if (is.finite(max_fields) && n > max_fields) {
    hint <- if (identical(unit, "mm")) {
      " The coordinate unit is set to mm: if the coordinates are in um, change it to um."
    } else if (identical(unit, "um")) {
      " The coordinate unit is set to um: check that the right coordinate columns are chosen."
    } else " Check the coordinate unit (um or mm) and the coordinate columns."
    stop(sprintf("The coordinates span %s x %s mm, which would be cut into %s rating fields of %g mm2 (at most %s are allowed). No specimen is that large.%s",
                 format(signif(width / 1000, 4), big.mark = ",", scientific = FALSE), format(signif(height / 1000, 4), big.mark = ",", scientific = FALSE),
                 format(n, big.mark = ",", scientific = FALSE), field_area_mm2, format(max_fields, big.mark = ",", scientific = FALSE), hint),
         call. = FALSE)
  }
  invisible(n)
}

#' Cut the inspected area into square rating fields
#'
#' Fields are laid from the lower-left corner of the inspected area. Fields
#' on the upper and right edges are usually incomplete; they are kept and
#' marked, because leaving them out would discard part of a small
#' inspected area.
#'
#' @param x,y Coordinates to place (um).
#' @param field_area_mm2 Area of one rating field. Default 0.5.
#' @param bbox Inspected area as `c(xmin, xmax, ymin, ymax)` in um;
#'   default: the range of `x` and `y`.
#' @param max_fields Largest number of rating fields allowed. An area that
#'   needs more stops with an error, because it nearly always means the
#'   coordinates are in another unit than assumed. Default 20000 (10000 mm2
#'   of 0.5 mm2 fields); `Inf` switches the check off.
#' @return A list: `field` (field number of each point, `NA` outside the
#'   area), `fields` (data frame: `field`, `col`, `row`, `x_min`, `x_max`,
#'   `y_min`, `y_max`, `covered_fraction`, `complete`), `side` (um), `nx`,
#'   `ny`.
#' @export
rating_field_grid <- function(x, y, field_area_mm2 = 0.5, bbox = NULL, max_fields = 20000) {
  if (!is.finite(field_area_mm2) || field_area_mm2 <= 0) stop("The rating field area must be greater than zero.", call. = FALSE)
  if (is.null(bbox)) bbox <- c(range(x, na.rm = TRUE), range(y, na.rm = TRUE))
  if (!all(is.finite(bbox))) stop("The inspected area cannot be worked out: no valid coordinates.", call. = FALSE)
  .check_rating_field_count(bbox, field_area_mm2, max_fields)
  side <- sqrt(field_area_mm2) * 1000
  width <- bbox[2] - bbox[1]
  height <- bbox[4] - bbox[3]
  tol <- 1e-9 * side
  nx <- max(1L, as.integer(ceiling((width - tol) / side)))
  ny <- max(1L, as.integer(ceiling((height - tol) / side)))

  col <- pmin(floor((x - bbox[1]) / side), nx - 1) + 1
  row <- pmin(floor((y - bbox[3]) / side), ny - 1) + 1
  inside <- !is.na(x) & !is.na(y) & x >= bbox[1] - tol & x <= bbox[2] + tol & y >= bbox[3] - tol & y <= bbox[4] + tol
  field <- ifelse(inside, (row - 1) * nx + col, NA)

  grid <- expand.grid(col = seq_len(nx), row = seq_len(ny))
  x_min <- bbox[1] + (grid$col - 1) * side
  y_min <- bbox[3] + (grid$row - 1) * side
  x_max <- pmin(x_min + side, bbox[2])
  y_max <- pmin(y_min + side, bbox[4])
  covered <- pmax(x_max - x_min, 0) * pmax(y_max - y_min, 0) / side^2
  fields <- data.frame(field = (grid$row - 1) * nx + grid$col, col = grid$col, row = grid$row,
                       x_min = x_min, x_max = x_max, y_min = y_min, y_max = y_max,
                       covered_fraction = covered, complete = covered > 1 - 1e-6)
  list(field = as.integer(field), fields = fields, side = side, nx = nx, ny = ny)
}

#' Rate every field for every inclusion type and width series
#'
#' Per field: total length of the A, B and C objects (um), number of D
#' particles and diameter of the largest DS particle, each turned into a
#' chart index with [cleanliness_rating_index()]. An object belongs to the
#' field its centre lies in.
#'
#' @param objects From [build_rating_objects()] (`$objects`).
#' @param grid From [rating_field_grid()], made for the object centres.
#' @return A data frame with one row per field and series: `field`,
#'   `series`, `type`, `value`, `index`, `index_exact`, `n_objects`,
#'   `n_oversize`.
#' @export
rate_fields <- function(objects, grid) {
  n_fields <- nrow(grid$fields)
  out <- expand.grid(field = grid$fields$field, series = .rating_series, stringsAsFactors = FALSE)
  out$type <- sub(" .*$", "", out$series)
  out$value <- 0
  out$n_objects <- 0L
  out$n_oversize <- 0L

  rated <- objects$rated & !is.na(grid$field)
  obj_series <- ifelse(objects$type == "DS", "DS", paste(objects$type, objects$series))
  key <- paste(grid$field, obj_series)
  out_key <- paste(out$field, out$series)
  for (k in unique(key[rated])) {
    rows <- which(rated & key == k)
    i <- match(k, out_key)
    if (is.na(i)) next
    type <- objects$type[rows[1]]
    out$value[i] <- switch(type, D = length(rows), DS = max(objects$diameter[rows]), sum(objects$length[rows]))
    out$n_objects[i] <- length(rows)
    out$n_oversize[i] <- sum(objects$oversize[rows])
  }
  out$index <- 0
  out$index_exact <- 0
  for (type in unique(out$type)) {
    sel <- out$type == type
    out$index[sel] <- cleanliness_rating_index(type, out$value[sel])
    out$index_exact[sel] <- cleanliness_rating_index(type, out$value[sel], exact = TRUE)
  }
  out[order(out$field, match(out$series, .rating_series)), ]
}

#' Summarise field ratings: worst field (method A) and mean (method B)
#'
#' @param field_ratings From [rate_fields()].
#' @param grid The grid used for it.
#' @return A data frame with one row per series: `series`, `worst_index`,
#'   `worst_value`, `unit`, `worst_field`, `worst_field_complete`,
#'   `mean_index` (mean chart index over all fields, empty ones included),
#'   `fields_rated` (fields with index 0.5 or more), `n_fields`,
#'   `n_objects`, `n_oversize`.
#' @export
summarise_rating <- function(field_ratings, grid) {
  rows <- lapply(.rating_series, function(s) {
    d <- field_ratings[field_ratings$series == s, , drop = FALSE]
    worst <- order(-d$index, -d$value)[1]
    type <- d$type[1]
    data.frame(
      series = s,
      worst_index = d$index[worst],
      worst_value = d$value[worst],
      unit = switch(type, D = "particles", DS = "um diameter", "um total length"),
      worst_field = if (d$value[worst] > 0) d$field[worst] else NA_integer_,
      worst_field_complete = if (d$value[worst] > 0) grid$fields$complete[match(d$field[worst], grid$fields$field)] else NA,
      mean_index = mean(d$index),
      fields_rated = sum(d$index >= 0.5),
      n_fields = nrow(d),
      n_objects = sum(d$n_objects),
      n_oversize = sum(d$n_oversize),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Worst rating field found with a shifted (sliding) window
#'
#' The fixed grid of [rating_field_grid()] can split a cluster of
#' inclusions that lies on a field border between two or four fields, and
#' the worst field then rates lower than the cluster deserves. Method A of
#' the chart standards asks for the worst field of the specimen, wherever it
#' lies, so here the 0.5 mm2 field is laid at `steps` x `steps` positions:
#' the grid is shifted by 1/`steps` of a field side at a time in each
#' direction (the unshifted grid included), and per series the window with
#' the highest chart index - on a tie, the larger value, then the unshifted
#' grid - is the worst field. An object belongs to the window its centre
#' lies in, as on the fixed grid.
#'
#' @param objects From [build_rating_objects()] (`$objects`): `x`, `y` (um),
#'   `type`, `series`, `length`, `diameter`, `rated`.
#' @param bbox Inspected area as `c(xmin, xmax, ymin, ymax)` in um.
#' @param field_area_mm2 Area of one rating field. Default 0.5.
#' @param steps Positions per field side. Default 4 (16 positions); 1 gives
#'   the fixed grid only.
#' @return A data frame with one row per series: `series`, `index`,
#'   `value`, `n_objects`, `x_min`, `y_min` (lower-left corner of the
#'   window, um), `shifted` (not a field of the fixed grid), `field` (field
#'   number on the fixed grid, `NA` for a shifted window or an empty
#'   series), `complete` (the window lies wholly inside the inspected
#'   area).
#' @export
worst_window_rating <- function(objects, bbox, field_area_mm2 = 0.5, steps = 4) {
  if (length(steps) != 1 || !is.finite(steps) || steps < 1 || steps != round(steps)) {
    stop("The number of window positions per field side must be a whole number of 1 or more.", call. = FALSE)
  }
  steps <- as.integer(steps)
  side <- sqrt(field_area_mm2) * 1000
  nx <- max(1L, as.integer(ceiling((bbox[2] - bbox[1] - 1e-9 * side) / side)))
  ny <- max(1L, as.integer(ceiling((bbox[4] - bbox[3] - 1e-9 * side) / side)))
  ok <- objects$rated & is.finite(objects$x) & is.finite(objects$y)
  ob <- objects[ok, , drop = FALSE]
  ob_series <- ifelse(ob$type == "DS", "DS", paste(ob$type, ob$series))

  best <- data.frame(series = .rating_series, index = 0, value = 0, n_objects = 0L, x_min = NA_real_,
                     y_min = NA_real_, shifted = NA, field = NA_integer_, complete = NA, stringsAsFactors = FALSE)
  for (i in seq_len(steps) - 1L) {
    for (j in seq_len(steps) - 1L) {
      x0 <- bbox[1] - i * side / steps
      y0 <- bbox[3] - j * side / steps
      col <- floor((ob$x - x0) / side)
      row <- floor((ob$y - y0) / side)
      if (i == 0L && j == 0L) {
        # the fixed grid, exactly as rating_field_grid() cuts it
        col <- pmin(col, nx - 1)
        row <- pmin(row, ny - 1)
      }
      for (s in unique(ob_series)) {
        sel <- ob_series == s
        cell <- paste(col[sel], row[sel])
        type <- sub(" .*$", "", s)
        value <- switch(type,
          D = tapply(rep(1, sum(sel)), cell, sum),
          DS = tapply(ob$diameter[sel], cell, max),
          tapply(ob$length[sel], cell, sum))
        count <- tapply(rep(1L, sum(sel)), cell, sum)
        index <- cleanliness_rating_index(type, value)
        k <- order(-index, -value)[1]
        b <- match(s, best$series)
        if (index[k] > best$index[b] || (index[k] == best$index[b] && value[k] > best$value[b])) {
          cr <- as.numeric(strsplit(names(value)[k], " ", fixed = TRUE)[[1]])
          x_min <- x0 + cr[1] * side
          y_min <- y0 + cr[2] * side
          covered <- max(0, min(x_min + side, bbox[2]) - max(x_min, bbox[1])) *
            max(0, min(y_min + side, bbox[4]) - max(y_min, bbox[3])) / side^2
          best[b, c("index", "value")] <- c(unname(index[k]), unname(value[k]))
          best$n_objects[b] <- unname(count[k])
          best$x_min[b] <- x_min
          best$y_min[b] <- y_min
          best$shifted[b] <- i > 0L || j > 0L
          best$field[b] <- if (i == 0L && j == 0L) as.integer(cr[2] * nx + cr[1] + 1) else NA_integer_
          best$complete[b] <- covered > 1 - 1e-6
        }
      }
    }
  }
  best
}

# Puts the sliding-window worst field into a summary from
# summarise_rating(): the fixed-grid values are kept as *_grid columns.
.apply_window_worst <- function(summary, window) {
  w <- window[match(summary$series, window$series), , drop = FALSE]
  summary$worst_index_grid <- summary$worst_index
  summary$worst_value_grid <- summary$worst_value
  summary$worst_field_grid <- summary$worst_field
  summary$worst_index <- w$index
  summary$worst_value <- w$value
  summary$worst_field <- ifelse(w$value > 0, w$field, NA_integer_)
  summary$worst_field_complete <- ifelse(w$value > 0, w$complete, NA)
  summary$worst_shifted <- ifelse(w$value > 0, w$shifted, NA)
  summary$worst_x_mm <- ifelse(w$value > 0, w$x_min / 1000, NA_real_)
  summary$worst_y_mm <- ifelse(w$value > 0, w$y_min / 1000, NA_real_)
  summary
}
