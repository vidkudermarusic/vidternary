# ---- UI: "Measurement Planning" tab ----
# From a pilot run (or several repeated runs of one specimen): how precise
# the number density is, how much area a wanted precision or a difference
# between batches takes, how many control areas the largest-inclusion
# estimate needs, and whether repeated runs agree. See
# R/planning_pipeline.R for the analysis and R/server_planning.R for the
# Shiny wiring.

#' Build the "Measurement Planning" tab's UI
#'
#' @param id Module namespace id - must match the id passed to
#'   `moduleServer()` for this tab in `server_logic.R`.
#' @return A `shiny::tabPanel()`.
#' @export
create_planning_tab <- function(id) {
  ns <- NS(id)
  tabPanel("Measurement Planning",
    fluidRow(
      column(12,
        h3("Measurement Planning: How Much Area Is Enough?"),
        helpText("Upload a pilot run of a specimen - or several repeated runs of the same specimen - to see how precise the result is and how much more area a wanted precision takes."),

        info_box(" How it works",
          tags$ul(
            tags$li(strong("Precision: "), "the number density is a count over an area. Its relative uncertainty is the square root of (index of dispersion / number of particles), so halving the interval takes four times the area."),
            tags$li(strong("Clustering: "), "inclusions come in groups, so counts per SEM field vary more than random scatter. The index of dispersion (variance over mean of the counts per field) measures that, and every area in the plan grows by that factor. With several runs it is measured within each run and pooled; a difference between the runs is judged under Repeated runs."),
            tags$li(strong("Difference between batches: "), "the area each of two batches needs so that a given density ratio is found with the chosen probability."),
            tags$li(strong("Largest inclusion: "), "the return level of the extreme-value fit is refitted on simulated measurements with more control areas, to show how slowly its interval narrows."),
            tags$li(strong("Repeated runs: "), "with two or more runs of one specimen, the scatter between them is compared with what counting alone would give. A difference between batches smaller than the repeatability limit cannot be told from a mere repeat."),
            tags$li(strong("Limits: "), "the plan covers counting statistics on this specimen. It says nothing about differences between sections, positions or heats.")
          )
        ),

        fluidRow(
          column(4,
            h4("Files"),
            div(`data-example-files` = "example_run_1.xlsx,example_run_2.xlsx",
              fileInput(ns("plan_files"), "Pilot run, or repeated runs of one specimen", multiple = TRUE, accept = particle_file_accept())),
            helpText("One file = one run. Several files are taken as repeats of the same specimen and pooled for the plan."),
            verbatimTextOutput(ns("plan_file_info"))
          ),
          column(4,
            h4("Columns"),
            selectInput(ns("plan_area_col"), "Area (um2):", choices = NULL),
            selectInput(ns("plan_field_col"), "SEM field ID (for the clustering):", choices = c("None" = "none")),
            create_xy_coordinate_selectors(ns("plan_x_col"), ns("plan_y_col"), "X coordinate column (optional):", "Y coordinate column (optional):"),
            selectInput(ns("plan_coord_unit"), "Coordinate unit:", choices = c("mm" = "mm", "um" = "um"), selected = "mm"),
            textInput(ns("plan_inspected_area"), "Inspected area in mm2 (optional):", value = "", placeholder = "one value, or one per file: 1.9, 1.9"),
            helpText("Leave empty to take the area spanned by the coordinates of each file.")
          ),
          column(4,
            h4("Targets"),
            numericInput(ns("plan_precision"), "Wanted precision of the number density (+/- %):", value = 10, min = 0.5, max = 100, step = 1),
            numericInput(ns("plan_ratio"), "Density ratio between two batches to detect:", value = 1.2, min = 0.05, step = 0.05),
            helpText("1.2 = one batch holds 20 % more particles per mm2 than the other."),
            numericInput(ns("plan_power"), "Probability of detecting it (%):", value = 80, min = 50, max = 99, step = 5),
            h4("Largest inclusion"),
            numericInput(ns("plan_fields_per_area"), "Fields per control area:", value = 1, min = 1, step = 1),
            numericInput(ns("plan_return_period"), "Return period T (control areas):", value = 100, min = 1.01, step = 1)
          )
        ),

        fluidRow(column(12, create_pre_filter_ui(ns, "plan"))),
        fluidRow(column(12, settings_io_ui(ns, "plan"))),
        centered_action_button_row(ns("plan_run"), "Make the Plan", margin_top = "10px"),

        fluidRow(
          column(12,
            verbatimTextOutput(ns("plan_status")),
            uiOutput(ns("plan_notes")),
            downloadButton(ns("plan_download_tables"), "Download all tables (xlsx)"),
            downloadButton(ns("plan_download_report"), "Download report (HTML)"),
            downloadButton(ns("plan_download_script"), "Download R script")
          )
        ),

        tabsetPanel(
          tabPanel("Number density",
            h4("What the pilot run gives"),
            tableOutput(ns("plan_pilot_table")),
            plotOutput(ns("plan_precision_plot"), height = "400px"),
            h4("Area needed for a wanted precision"),
            tableOutput(ns("plan_precision_table")),
            h4("Area needed per batch to detect a difference"),
            tableOutput(ns("plan_ratio_table"))
          ),
          tabPanel("Largest inclusion",
            h4("Precision of the return level against the number of control areas"),
            helpText("Simulated from the pilot fit. The first row is the pilot itself."),
            tableOutput(ns("plan_evs_table"))
          ),
          tabPanel("Repeated runs",
            h4("The runs"),
            div(style = "overflow-x: auto;", tableOutput(ns("plan_runs_table"))),
            h4("Repeatability of the number density"),
            tableOutput(ns("plan_repeat_density_table")),
            h4("Repeatability of the other quantities"),
            helpText("From the scatter between the runs alone, without a counting model."),
            tableOutput(ns("plan_repeat_other_table"))
          )
        )
      )
    )
  )
}
