# ---- Steel cleanliness: plots ----
# Map of the rated objects and stringers, rose diagram of particle
# directions, and the size-class bar chart. Pure ggplot2, no Shiny.

# One fixed colour per inclusion type, so a type looks the same in every plot.
.inclusion_type_colors <- c(A = "#E69F00", B = "#0072B2", C = "#009E73", D = "#CC79A7", DS = "#D55E00")

#' Map of rated objects, stringers and rating fields
#'
#' Particles too narrow to be rated are drawn as small grey dots, rated
#' objects as points coloured by type, and objects of more than one
#' particle as a line from end to end. Dashed squares are the rating
#' fields.
#'
#' @param result From [run_cleanliness_analysis()].
#' @return A `ggplot` object (coordinates in mm).
#' @export
create_stringer_map <- function(result) {
  p <- result$particles
  o <- result$objects[result$objects$rated, , drop = FALSE]
  f <- result$grid$fields
  thin <- p[is.na(p$object), , drop = FALSE]
  lines <- o[o$n_members > 1, , drop = FALSE]
  mm <- function(v) v / 1000

  ggplot2::ggplot() +
    ggplot2::geom_rect(data = f, ggplot2::aes(xmin = mm(x_min), xmax = mm(x_max), ymin = mm(y_min), ymax = mm(y_max)),
                       fill = NA, colour = "grey60", linetype = "dashed", linewidth = 0.3) +
    ggplot2::geom_point(data = thin, ggplot2::aes(x = mm(x), y = mm(y)), colour = "grey75", size = 0.2) +
    ggplot2::geom_segment(data = lines, ggplot2::aes(x = mm(x_start), y = mm(y_start), xend = mm(x_end), yend = mm(y_end), colour = type),
                          linewidth = 1.1) +
    ggplot2::geom_point(data = o, ggplot2::aes(x = mm(x), y = mm(y), colour = type), size = 1.8) +
    ggplot2::scale_colour_manual(values = .inclusion_type_colors, drop = FALSE, name = "Type") +
    ggplot2::coord_fixed() +
    ggplot2::labs(x = "X (mm)", y = "Y (mm)", title = "Rated inclusions and stringers",
                  subtitle = sprintf("%d rated objects, %d stringers; dashed squares = %g mm2 rating fields",
                                     nrow(o), sum(o$is_stringer), result$settings$field_area_mm2)) +
    ggplot2::theme_minimal()
}

#' Rose diagram of particle directions
#'
#' Directions are axial (a particle at 10 degrees also points at 190), so
#' each bar is drawn twice. Bars are weighted by particle length, so a few
#' long inclusions count for more than many small round ones, whose
#' direction is not well defined.
#'
#' @param direction Particle directions in degrees (0 to 180), as reported
#'   by the instrument.
#' @param weight Weight of each particle (default: equal weights).
#' @param axis_angle Working direction in degrees, drawn as a red line.
#' @param bin_width Bar width in degrees. Default 10.
#' @return A `ggplot` object.
#' @export
create_direction_rose <- function(direction, weight = NULL, axis_angle = 0, bin_width = 10) {
  if (is.null(weight)) weight <- rep(1, length(direction))
  ok <- is.finite(direction) & is.finite(weight)
  direction <- direction[ok] %% 180
  weight <- weight[ok]
  n_bins <- as.integer(round(180 / bin_width))
  bin <- pmin(floor(direction / bin_width), n_bins - 1)
  total <- as.numeric(tapply(weight, factor(bin, levels = 0:(n_bins - 1)), sum))
  total[is.na(total)] <- 0
  share <- if (sum(total) > 0) 100 * total / sum(total) else total
  mid <- (0:(n_bins - 1) + 0.5) * bin_width
  d <- data.frame(angle = c(mid, mid + 180), share = c(share, share))
  axis <- data.frame(angle = c(axis_angle %% 360, (axis_angle + 180) %% 360))

  ggplot2::ggplot(d, ggplot2::aes(x = angle, y = share)) +
    ggplot2::geom_col(width = bin_width, fill = "#357ABD", colour = "white", linewidth = 0.2) +
    ggplot2::geom_vline(data = axis, ggplot2::aes(xintercept = angle), colour = "#d32f2f", linewidth = 0.7) +
    ggplot2::scale_x_continuous(limits = c(0, 360), breaks = seq(0, 315, by = 45)) +
    # 0 degrees at 3 o'clock, angles counter-clockwise, as on an X-Y plot.
    ggplot2::coord_polar(start = -pi / 2, direction = -1) +
    ggplot2::labs(x = NULL, y = "Share of total particle length (%)", title = "Particle directions",
                  subtitle = "Red line = working direction; angles as reported by the instrument") +
    ggplot2::theme_minimal()
}

#' Bar chart of number density per size class
#'
#' @param size_classes From [size_class_table()].
#' @return A `ggplot` object: particles per mm2 per size class, stacked by
#'   group.
#' @export
create_size_class_plot <- function(size_classes) {
  d <- size_classes[size_classes$group != "All", , drop = FALSE]
  ggplot2::ggplot(d, ggplot2::aes(x = size_class, y = per_mm2, fill = group)) +
    ggplot2::geom_col() +
    ggplot2::labs(x = "Equivalent circle diameter (um)", y = expression("Particles per " * mm^2),
                  fill = "Family", title = "Number density per size class") +
    ggplot2::theme_minimal()
}
