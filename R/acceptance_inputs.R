# ---- Acceptance limits: the input boxes shared by two tabs ----
# The Steel Cleanliness tab (one specimen) and the Folder Summary tab (many
# files) take the same limits; this is their common UI and the function
# that reads it. See R/acceptance_criteria.R for what is done with them.

#' Input boxes for the acceptance limits
#'
#' @param ns The tab's namespacing function.
#' @param prefix Short, tab-unique input-id prefix (e.g. `"clean"`); the
#'   ids are `<prefix>_acc_per_mm2`, `_acc_area_ppm`, `_acc_largest`,
#'   `_acc_ratings`, `_acc_return_level`, `_acc_fields_per_area` and
#'   `_acc_return_period`.
#' @param note Sentence shown under the heading.
#' @return A `shiny::div()`.
#' @export
acceptance_limits_ui <- function(ns, prefix, note = NULL) {
  id <- function(name) ns(paste0(prefix, "_acc_", name))
  div(style = "border: 1px solid #6c757d; padding: 10px 15px; border-radius: 5px; margin: 10px 0;",
    h4("Acceptance limits (optional)"),
    helpText("Upper limits from a specification. Leave a box empty for no limit. For each limit the result says whether it is met and whether that holds beyond the uncertainty of the measurement.", note),
    fluidRow(
      column(3, numericInput(id("per_mm2"), "Particles per mm2, at most:", value = NA, min = 0)),
      column(3, numericInput(id("area_ppm"), "Area fraction (ppm), at most:", value = NA, min = 0)),
      column(3, numericInput(id("largest"), "Largest inclusion found, sqrt(area) (um), at most:", value = NA, min = 0)),
      column(3, textInput(id("ratings"), "Chart index (worst field), at most:", value = "", placeholder = "e.g. A=2, B=1.5, D fine=1, DS=1"))
    ),
    fluidRow(
      column(3, numericInput(id("return_level"), "Largest inclusion expected (EVS), sqrt(area) (um), at most:", value = NA, min = 0)),
      column(3, numericInput(id("fields_per_area"), "EVS: fields per control area:", value = 1, min = 1, step = 1)),
      column(3, numericInput(id("return_period"), "EVS: return period T (control areas):", value = 100, min = 1.01, step = 1))
    )
  )
}

#' Read the acceptance limits from the input boxes
#'
#' @param input The tab's Shiny `input` object.
#' @param prefix Same prefix passed to [acceptance_limits_ui()].
#' @return A list: `limits` ([acceptance_limits()]), `fields_per_area`,
#'   `return_period` (for the extreme-value limit).
#' @export
read_acceptance_limits <- function(input, prefix) {
  get <- function(name) input[[paste0(prefix, "_acc_", name)]]
  num <- function(v) if (is.null(v) || length(v) != 1 || !is.finite(v)) NULL else v
  list(limits = acceptance_limits(num(get("per_mm2")), num(get("area_ppm")), num(get("largest")),
                                  num(get("return_level")), parse_rating_limits(get("ratings"))),
       fields_per_area = num(get("fields_per_area")) %||% 1,
       return_period = num(get("return_period")) %||% 100)
}
