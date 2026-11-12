# ---- Steel cleanliness: from particles to rated objects (type A, B, C, D, DS) ----
# The chart standards rate objects, not single particles: a stringer is one
# object. This file joins particles into objects (per chemical family, see
# cleanliness_stringers.R) and gives each object its type and width series.
#
# Type by chemistry and shape:
#   sulfide, elongated or a stringer          -> A
#   not sulfide, stringer of compact particles -> B
#   not sulfide, elongated                     -> C
#   compact single particle                    -> D, or DS from 13 um diameter
# Families the standards do not name (Ti-rich, unclassified) are rated by
# shape alone, as ISO 4967 prescribes for non-traditional inclusions; the
# family is kept so the rating can be reported per family.

#' Join particles into rated objects and assign their type
#'
#' Stringers are searched within each chemical family. Particles narrower
#' than `min_width` are left out: they would not be visible at the 100x of
#' the chart method, and thousands of sub-micrometre SEM detections would
#' otherwise chain into meaningless stringers. Two or more compact
#' particles that are close but too few for a stringer stay separate
#' objects.
#'
#' @param particles Data frame with `x`, `y` (um), `length`, `breadth`
#'   (um), `area` (um2), `family` and optionally `direction` (degrees).
#' @param standard From [cleanliness_rating_standard()].
#' @param axis_angle Working direction in degrees from the X axis.
#' @param min_width Narrowest particle that is rated (um). Default 2.
#' @return A list: `object` (object number per row of `particles`, `NA`
#'   for particles left out) and `objects` (data frame: `object`, `family`,
#'   `n_members`, `is_stringer`, `length`, `width`, `diameter`, `aspect`,
#'   `type`, `series`, `oversize`, `rated`, position columns).
#' @export
build_rating_objects <- function(particles, standard = cleanliness_rating_standard(),
                                 axis_angle = 0, min_width = 2) {
  n <- nrow(particles)
  object <- rep(NA_integer_, n)
  keep <- which(is.finite(particles$breadth) & particles$breadth >= min_width)
  parts <- list()
  next_id <- 0L
  aspect_particle <- particles$length / particles$breadth
  ecd <- 2 * sqrt(particles$area / pi)

  for (fam in unique(particles$family[keep])) {
    idx <- keep[particles$family[keep] == fam]
    st <- detect_stringers(particles$x[idx], particles$y[idx], particles$length[idx], particles$breadth[idx],
                           direction = if ("direction" %in% names(particles)) particles$direction[idx] else NULL,
                           axis_angle = axis_angle, max_gap = standard$max_gap,
                           max_offset = standard$max_offset, min_members = standard$min_members)
    obj <- st$objects
    member_aspect <- as.numeric(tapply(aspect_particle[idx], st$object, stats::median))
    elongated <- member_aspect >= standard$aspect_limit

    # A close pair of compact particles is neither a stringer nor one
    # elongated inclusion: its members are rated one by one.
    split <- obj$n_members > 1 & !obj$is_stringer & !elongated
    local_id <- st$object
    if (any(split)) {
      loose <- which(local_id %in% obj$object[split])
      local_id[loose] <- max(local_id) + seq_along(loose)
      single <- data.frame(object = local_id[loose], n_members = 1L, is_stringer = FALSE,
                           length = particles$length[idx[loose]], width = particles$breadth[idx[loose]],
                           x = particles$x[idx[loose]], y = particles$y[idx[loose]],
                           x_start = NA_real_, y_start = NA_real_, x_end = NA_real_, y_end = NA_real_)
      obj <- rbind(obj[!split, , drop = FALSE], single)
      member_aspect <- c(member_aspect[!split], aspect_particle[idx[loose]])
    }
    obj$family <- fam
    obj$member_aspect <- member_aspect
    obj$diameter <- NA_real_
    one <- obj$n_members == 1
    first_member <- idx[match(obj$object, local_id)]
    obj$diameter[one] <- ecd[first_member[one]]

    new_id <- next_id + seq_len(nrow(obj))
    object[idx] <- new_id[match(local_id, obj$object)]
    obj$object <- new_id
    next_id <- next_id + nrow(obj)
    parts[[length(parts) + 1]] <- obj
  }

  objects <- if (length(parts)) do.call(rbind, parts) else
    data.frame(object = integer(0), n_members = integer(0), is_stringer = logical(0), length = numeric(0),
               width = numeric(0), x = numeric(0), y = numeric(0), x_start = numeric(0), y_start = numeric(0),
               x_end = numeric(0), y_end = numeric(0), family = character(0), member_aspect = numeric(0),
               diameter = numeric(0), stringsAsFactors = FALSE)
  rownames(objects) <- NULL
  objects <- assign_inclusion_types(objects, standard)
  list(object = object, objects = objects)
}

#' Type (A, B, C, D, DS) and width series of rated objects
#'
#' @param objects Data frame with `family`, `n_members`, `is_stringer`,
#'   `length`, `width`, `member_aspect` (median length/breadth of the
#'   members) and `diameter` (single particles).
#' @param standard From [cleanliness_rating_standard()].
#' @return `objects` with added `aspect`, `type`, `series` (`"fine"`,
#'   `"thick"`, `"single"` for DS, `"below minimum width"`), `oversize`
#'   (wider than the thick series, still rated as thick) and `rated`.
#' @export
assign_inclusion_types <- function(objects, standard = cleanliness_rating_standard()) {
  if (nrow(objects) == 0) {
    return(cbind(objects, data.frame(aspect = numeric(0), type = character(0), series = character(0),
                                     oversize = logical(0), rated = logical(0), stringsAsFactors = FALSE)))
  }
  elongated <- objects$member_aspect >= standard$aspect_limit
  sulfide <- objects$family == "sulfide"
  type <- ifelse(sulfide & (elongated | objects$is_stringer), "A",
          ifelse(!sulfide & elongated, "C",
          ifelse(!sulfide & objects$is_stringer, "B", "D")))

  # Globular particles are measured by their diameter, the others by width.
  size <- ifelse(type == "D", objects$diameter, objects$width)
  type[type == "D" & size >= standard$ds_min_diameter] <- "DS"

  w <- standard$widths[match(type, standard$widths$type), ]
  series <- ifelse(type == "DS", "single",
            ifelse(size < w$fine_min, "below minimum width",
            ifelse(size <= w$fine_max, "fine", "thick")))
  objects$aspect <- objects$length / objects$width
  objects$type <- type
  objects$series <- series
  objects$oversize <- type != "DS" & !is.na(w$thick_max) & size > w$thick_max
  objects$rated <- series != "below minimum width"
  objects
}
