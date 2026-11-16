# ---- UI: "Folder Summary" tab ----
# Many files, each analysed on its own with the same settings, and one
# table with a row per file. See R/folder_pipeline.R for the analysis and
# R/server_folder.R for the Shiny wiring.

#' Build the "Folder Summary" tab's UI
#'
#' @param id Module namespace id - must match the id passed to
#'   `moduleServer()` for this tab in `server_logic.R`.
#' @return A `shiny::tabPanel()`.
#' @export
create_folder_tab <- function(id) {
  ns <- NS(id)
  tabPanel("Folder Summary",
    fluidRow(
      column(12,
        h3("Folder Summary: Many Files, One Table"),
        helpText("Select all the files of a folder (the specimens of a week, a heat, a trial). Each is analysed on its own with the same columns and settings; the result is one row per file and one report."),

        info_box(" How it works",
          tags$ul(
            tags$li(strong("Per file: "), "the analysis of the Steel Cleanliness tab (number density, area fraction, chart indices, stringers) and the largest-inclusion estimate of the EVS tab."),
            tags$li(strong("Limits: "), "with acceptance limits, every file gets a verdict - and the table says where a verdict does not hold beyond the uncertainty of the measurement."),
            tags$li(strong("Files that fail: "), "a file with missing columns or no valid particles stays in the list with the reason; the others are still analysed."),
            tags$li(strong("Not a comparison: "), "the files are set side by side, not tested against each other. To ask whether two batches differ, use the Batch Comparison tab."),
            tags$li(strong("From a script: "), code("process_folder(\"path/to/folder\", report = \"summary.html\")"), " does the same without the app.")
          )
        ),

        fluidRow(
          column(4,
            h4("Files"),
            fileInput(ns("folder_files"), "Select all files of the folder", multiple = TRUE, accept = c(".xlsx", ".csv", ".txt", ".tsv")),
            helpText("Open the folder in the file dialog and select everything (Ctrl+A). Only columns present in every file can be chosen."),
            verbatimTextOutput(ns("folder_file_info")),
            selectInput(ns("folder_standard"), "Rating standard:", choices = c("ISO 4967", "ASTM E45"), selected = "ISO 4967")
          ),
          column(4,
            h4("Particle columns"),
            selectInput(ns("folder_area_col"), "Area (um2):", choices = NULL),
            selectInput(ns("folder_length_col"), "Length (um):", choices = NULL),
            selectInput(ns("folder_breadth_col"), "Breadth (um):", choices = NULL),
            selectInput(ns("folder_direction_col"), "Direction of the long axis (degrees, optional):", choices = c("None" = "none")),
            selectInput(ns("folder_field_col"), "SEM field ID (optional):", choices = c("None" = "none"))
          ),
          column(4,
            h4("Position"),
            create_xy_coordinate_selectors(ns("folder_x_col"), ns("folder_y_col")),
            selectInput(ns("folder_coord_unit"), "Coordinate unit:", choices = c("mm" = "mm", "um" = "um"), selected = "mm"),
            helpText("The inspected area of each file is the area spanned by its coordinates."),
            numericInput(ns("folder_beam_kv"), "Accelerating voltage (kV):", value = 15, min = 1, max = 40, step = 1)
          )
        ),

        fluidRow(column(12, acceptance_limits_ui(ns, "folder", "The two EVS settings are also used for the largest-inclusion estimate of every file."))),
        fluidRow(column(12, create_pre_filter_ui(ns, "folder"))),
        fluidRow(column(12, settings_io_ui(ns, "folder"))),
        centered_action_button_row(ns("folder_run"), "Analyse All Files", margin_top = "10px"),

        fluidRow(
          column(12,
            verbatimTextOutput(ns("folder_status")),
            downloadButton(ns("folder_download_tables"), "Download all tables (xlsx)"),
            downloadButton(ns("folder_download_report"), "Download summary report (HTML)"),
            downloadButton(ns("folder_download_archive"), "Download a report per file (zip)")
          )
        ),

        tabsetPanel(
          tabPanel("Overview",
            div(style = "overflow-x: auto;", tableOutput(ns("folder_overview_table"))),
            h4("Files that could not be analysed"),
            tableOutput(ns("folder_failed_table"))
          ),
          tabPanel("Charts",
            plotOutput(ns("folder_density_plot"), height = "380px"),
            plotOutput(ns("folder_area_plot"), height = "380px"),
            plotOutput(ns("folder_evs_plot"), height = "380px")
          ),
          tabPanel("Chart indices",
            helpText("Worst-field index per inclusion type and width series. An SEM-based estimate, not a rating to the standard."),
            tableOutput(ns("folder_ratings_table"))
          ),
          tabPanel("Acceptance",
            tableOutput(ns("folder_acceptance_table"))
          ),
          tabPanel("Warnings",
            tableOutput(ns("folder_warnings_table"))
          )
        )
      )
    )
  )
}
