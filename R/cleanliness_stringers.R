# ---- Steel cleanliness: stringers (inclusions aligned in the working direction) ----
# ISO 4967 treats two inclusions as one when they lie in line within 40 um
# of each other and their centre lines are no more than 10 um apart (ASTM
# E45: 40 um and 15 um); three or more such particles form a stringer.
# Positions come from the stage coordinates of an automated SEM run, so a
# stringer that crosses an SEM field border is still found, as long as the
# instrument did not cut the particles at the border.

# Extent of a particle along an axis: an ellipse with axes `length` and
# `breadth` whose long axis is turned by `phi` (radians) from that axis.
.axis_extent <- function(length, breadth, phi) {
  sqrt((length * cos(phi))^2 + (breadth * sin(phi))^2)
}

# Connected groups of the pairs (from, to) among n items (union-find).
.connected_groups <- function(n, from, to) {
  parent <- seq_len(n)
  root <- function(i) {
    while (parent[i] != i) {
      parent[i] <<- parent[parent[i]]
      i <- parent[i]
    }
    i
  }
  for (k in seq_along(from)) {
    a <- root(from[k]); b <- root(to[k])
    if (a != b) parent[max(a, b)] <- min(a, b)
  }
  roots <- vapply(seq_len(n), root, integer(1))
  match(roots, unique(roots))
}

#' Find stringers: particles lined up along the working direction
#'
#' Coordinates are turned so that one axis runs along the working (rolling)
#' direction. Two particles are joined when the gap between them along that
#' axis is at most `max_gap` and their centres are at most `max_offset`
#' apart across it; joined particles form one object, and an object of
#' `min_members` or more particles is a stringer.
#'
#' Chains are built by single linkage, so a long stringer may drift
#' sideways by more than `max_offset` from end to end.
#'
#' @param x,y Particle centre coordinates (um).
#' @param length,breadth Particle length and breadth (um).
#' @param direction Orientation of each particle's long axis in degrees, in
#'   the same frame as `axis_angle`; `NULL` takes every particle as lying
#'   along the working direction.
#' @param axis_angle Working direction in degrees from the X axis. Default 0.
#' @param max_gap Largest gap along the working direction (um). Default 40.
#' @param max_offset Largest centre-to-centre distance across it (um).
#'   Default 10 (ISO 4967); ASTM E45 uses 15.
#' @param min_members Particles needed for a stringer. Default 3.
#' @return A list: `object` (object number of each particle) and `objects`,
#'   a data frame with one row per object: `object`, `n_members`,
#'   `is_stringer`, `length` (end to end along the working direction, gaps
#'   included), `width` (breadth of the widest member), `x`, `y` (centre),
#'   `x_start`, `y_start`, `x_end`, `y_end` (ends along the working
#'   direction).
#' @export
detect_stringers <- function(x, y, length, breadth, direction = NULL, axis_angle = 0,
                             max_gap = 40, max_offset = 10, min_members = 3) {
  n <- length(x)
  if (length(y) != n || length(length) != n || length(breadth) != n) {
    stop("x, y, length and breadth must have the same number of values.", call. = FALSE)
  }
  empty <- data.frame(object = integer(0), n_members = integer(0), is_stringer = logical(0),
                      length = numeric(0), width = numeric(0), x = numeric(0), y = numeric(0),
                      x_start = numeric(0), y_start = numeric(0), x_end = numeric(0), y_end = numeric(0))
  if (n == 0) return(list(object = integer(0), objects = empty))
  if (anyNA(c(x, y, length, breadth))) stop("x, y, length and breadth must not contain missing values.", call. = FALSE)

  theta <- axis_angle * pi / 180
  along <- x * cos(theta) + y * sin(theta)
  across <- -x * sin(theta) + y * cos(theta)
  phi <- if (is.null(direction)) rep(0, n) else (direction - axis_angle) * pi / 180
  phi[is.na(phi)] <- 0
  half <- .axis_extent(length, breadth, phi) / 2
  start <- along - half
  end <- along + half

  # Sorted by start, every pair is tested from its earlier-starting member:
  # the gap is then start[later] - end[earlier] (or 0 when they overlap).
  ord <- order(start)
  s_start <- start[ord]; s_end <- end[ord]; s_across <- across[ord]
  last <- findInterval(s_end + max_gap, s_start)
  from <- vector("list", n); to <- vector("list", n)
  for (i in seq_len(n - 1)) {
    if (last[i] <= i) next
    j <- (i + 1):last[i]
    j <- j[abs(s_across[j] - s_across[i]) <= max_offset]
    if (length(j)) { from[[i]] <- rep(i, length(j)); to[[i]] <- j }
  }
  group_sorted <- .connected_groups(n, unlist(from), unlist(to))
  object <- integer(n)
  object[ord] <- group_sorted
  object <- match(object, unique(object))

  n_members <- as.integer(tabulate(object))
  t_start <- as.numeric(tapply(start, object, min))
  t_end <- as.numeric(tapply(end, object, max))
  s_mid <- as.numeric(tapply(across, object, mean))
  back_x <- function(t, s) t * cos(theta) - s * sin(theta)
  back_y <- function(t, s) t * sin(theta) + s * cos(theta)
  t_mid <- (t_start + t_end) / 2

  objects <- data.frame(
    object = seq_along(n_members),
    n_members = n_members,
    is_stringer = n_members >= min_members,
    length = t_end - t_start,
    width = as.numeric(tapply(breadth, object, max)),
    x = back_x(t_mid, s_mid), y = back_y(t_mid, s_mid),
    x_start = back_x(t_start, s_mid), y_start = back_y(t_start, s_mid),
    x_end = back_x(t_end, s_mid), y_end = back_y(t_end, s_mid)
  )
  list(object = object, objects = objects)
}
