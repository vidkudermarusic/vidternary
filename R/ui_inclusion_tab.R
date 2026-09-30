# ---- UI: "Inclusion Presets" tab ----
# Ternary diagrams of common inclusion systems (oxides, sulfides, nitrides or
# element wt%) with classification lines, matrix removal and per-particle
# classes, from one SEM/EDS file. The controls and outputs are in
# create_inclusion_presets_panel() (ui_inclusion_presets.R); the analysis is
# in inclusion_analysis.R and the Shiny wiring in server_inclusion.R.

#' Build the "Inclusion Presets" tab's UI
#'
#' @param id Module namespace id - must match the id passed to
#'   `moduleServer()` for this tab in `server_logic.R`.
#' @return A `shiny::tabPanel()`.
#' @export
create_inclusion_tab <- function(id) {
  ns <- NS(id)
  tabPanel("Inclusion Presets",
    fluidRow(
      column(12,
        h3("Inclusion Diagram Presets"),
        helpText("Ternary diagrams for common inclusion systems (oxides, sulfides, nitrides, or element wt%) with classification lines, from the element wt% of an SEM/EDS analysis. Room-temperature compositions only."),

        info_box(" How it works",
          tags$ul(
            tags$li("Upload one XLSX file (Sheet 1) with element wt% columns such as ", tags$code("Al.(Wt%)"), ". One file is one specimen."),
            tags$li("Optionally remove the steel matrix: choose the matrix element (and, if wanted, its alloying elements), so that small inclusions are not dominated by the surrounding steel."),
            tags$li("Elements are resolved into oxides, sulfides and nitrides (compound presets) or used as element wt% (element presets), and plotted on the corners of the chosen preset."),
            tags$li("Points are classified by rules or by the nearest reference phase; the dashed lines are the class boundaries. The rules and boundaries are conventions, not a standard - state the settings when reporting results."),
            tags$li("Only particles whose three corners make up enough of their analysed mass (coverage threshold) are plotted; the Particles table shows why the others were dropped.")
          )
        ),

        fluidRow(
          column(6, create_file_selection_column(ns("incl_file"), multiple = FALSE)),
          column(6, h4("Loaded data"), uiOutput(ns("incl_data_info")))
        ),

        create_inclusion_presets_panel(ns)
      )
    )
  )
}
