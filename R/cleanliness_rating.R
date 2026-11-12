# ---- Steel cleanliness: chart rating per field (ISO 4967 / ASTM E45) ----
# The chart methods rate square fields of 0.5 mm2. An automated SEM run
# uses much smaller fields, so the inspected area is re-cut here into
# 0.5 mm2 rating fields from the particle coordinates. Each field gets an
# index per type and width series; method A reports the worst field,
# method B the mean over all fields.

# Rating series in report order.
.rating_series <- c("A fine", "A thick", "B fine", "B thick", "C fine", "C thick", "D fine", "D thick", "DS")

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
#' @return A list: `field` (field number of each point, `NA` outside the
#'   area), `fields` (data frame: `field`, `col`, `row`, `x_min`, `x_max`,
#'   `y_min`, `y_max`, `covered_fraction`, `complete`), `side` (um), `nx`,
#'   `ny`.
#' @export
rating_field_grid <- function(x, y, field_area_mm2 = 0.5, bbox = NULL) {
  if (!is.finite(field_area_mm2) || field_area_mm2 <= 0) stop("The rating field area must be greater than zero.", call. = FALSE)
  if (is.null(bbox)) bbox <- c(range(x, na.rm = TRUE), range(y, na.rm = TRUE))
  if (!all(is.finite(bbox))) stop("The inspected area cannot be worked out: no valid coordinates.", call. = FALSE)
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
