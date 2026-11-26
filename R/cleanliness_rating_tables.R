# ---- Steel cleanliness: numbers of the chart-rating standards ----
# The minimum values per chart index, the width limits of the fine and
# thick series and the stringer distances of ISO 4967 (method A/B) and
# ASTM E45 (methods A and D), kept in one place so they can be checked
# against the user's own copy of the standard.
#
# Where each number comes from (checked 2026-11-11):
# * Minimum values per chart index (Table 1), width limits (Table 2), the
#   aspect-ratio limit of 3, "at least three" particles in group B, the
#   0,50 mm2 field and the 200 mm2 specimen: read from the publisher's
#   preview of ISO 4967:2013 (pages 1-2),
#   https://standards.iteh.ai/catalog/standards/iso/631317f1-2dc3-40f5-bfcc-025c6035b946/iso-4967-2013
# * ASTM E45 stringer rule (three or more type B or C inclusions, offset no
#   more than 15 um, less than 40 um apart) and globular oxides with an
#   aspect ratio of at most 2: read from excerpts of ASTM E45-10 found by
#   web search, not from a full copy.
# * ISO 4967 stringer distances (40 um along, 10 um across) and the Annex D
#   formulas: from memory, not in the preview. EN 10247 uses the same
#   40 um / 10 um rule (ZEISS knowledge base). The formulas reproduce the
#   lengths of Table 1 to within 1 % (tested in
#   test-cleanliness-rating-tables.R), so their coefficients are close to
#   but may not be exactly those printed in Annex D. They are used only for
#   the continuous index; the chart index itself comes from Table 1.
# * ASTM E45 minimum values, width limits and the 160 mm2 specimen: taken
#   to be the same as ISO 4967 Table 1 and 2, from memory.
# A fourth edition, ISO 4967:2026, was announced; its tables have not been
# compared with the values below.

# Minimum value for chart index 0.5, 1, ... 3 and the upper limit of index 3
# (reported here as "3.5 = beyond the chart"). A, B, C: total length in um
# per 0.5 mm2 field; D: number of particles; DS: diameter in um.
.rating_minimums <- list(
  A = c(37, 127, 261, 436, 649, 898, 1181),
  B = c(17, 77, 184, 343, 555, 822, 1147),
  C = c(18, 76, 176, 320, 510, 746, 1029),
  D = c(1, 4, 9, 16, 25, 36, 49),
  DS = c(13, 19, 27, 38, 53, 76, 107)
)
.rating_index_steps <- seq(0.5, 3.5, by = 0.5)

#' Settings of a chart-rating standard
#'
#' @param standard `"ISO 4967"` or `"ASTM E45"`.
#' @return A list: `name`, `field_area_mm2` (0.5), `max_gap` and
#'   `max_offset` (um, stringer distances), `aspect_limit` (length/width
#'   from which a particle counts as elongated), `min_members` (particles
#'   in a stringer), `ds_min_diameter` (um), `min_specimen_area_mm2`,
#'   `widths` (data frame `type`, `fine_min`, `fine_max`, `thick_max`, um)
#'   and `minimums` (list of the minimum value per chart index and type).
#' @export
cleanliness_rating_standard <- function(standard = c("ISO 4967", "ASTM E45")) {
  standard <- match.arg(standard)
  iso <- standard == "ISO 4967"
  list(
    name = standard,
    field_area_mm2 = 0.5,
    max_gap = 40,
    max_offset = if (iso) 10 else 15,
    aspect_limit = if (iso) 3 else 2,
    min_members = 3,
    ds_min_diameter = 13,
    min_specimen_area_mm2 = if (iso) 200 else 160,
    widths = data.frame(
      type = c("A", "B", "C", "D"),
      fine_min = c(2, 2, 2, 2),
      fine_max = c(4, 9, 5, 8),
      thick_max = c(12, 15, 12, 13),
      stringsAsFactors = FALSE
    ),
    minimums = .rating_minimums
  )
}

#' Chart index for a measured field value
#'
#' The index is the highest chart picture whose minimum the value reaches
#' (0 below the first picture, 3.5 for values beyond picture 3).
#' `exact = TRUE` instead returns the continuous index from the regression
#' formulas of ISO 4967 Annex D, useful for comparing fields that share a
#' chart index.
#'
#' @param type `"A"`, `"B"`, `"C"`, `"D"` or `"DS"`.
#' @param value Total length (um; A, B, C), number of particles (D) or
#'   diameter (um; DS) in one 0.5 mm2 field.
#' @param exact Return the formula value instead of the chart step.
#' @return Numeric vector of indices, same length as `value`.
#' @export
cleanliness_rating_index <- function(type, value, exact = FALSE) {
  if (!type %in% names(.rating_minimums)) stop("Unknown inclusion type '", type, "'.", call. = FALSE)
  value <- as.numeric(value)
  if (!exact) {
    return(c(0, .rating_index_steps)[findInterval(value, .rating_minimums[[type]]) + 1])
  }
  out <- suppressWarnings(switch(type,
    A = 10^(0.5605 * log10(value) - 1.179),
    B = 10^(0.4626 * log10(value) - 0.871),
    C = 10^(0.4807 * log10(value) - 0.904),
    D = 10^(0.5 * log10(value) - 0.301),
    DS = 3.311 * log10(value) - 3.22
  ))
  out[!is.finite(out) | value <= 0] <- 0
  pmax(out, 0)
}
