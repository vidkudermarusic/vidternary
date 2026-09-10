# ---- UI: "Extreme Value Analysis" tab ----
# Murakami / ASTM E2283 extreme value statistics for inclusion rating:
# predicts the largest inclusion expected over a larger inspection area
# from the distribution of per-field (or per-group) block maxima. See
# R/extreme_value_analysis.R for the statistics/plotting and
# R/server_evs.R for the Shiny wiring.

#' Build the "Extreme Value Analysis" tab's UI
#'
#' @param id Module namespace id - must match the id passed to
#'   `moduleServer()` for this tab in `server_logic.R`.
#' @return A `shiny::tabPanel()`.
#' @export
create_evs_tab <- function(id) {
  ns <- NS(id)
  tabPanel("Extreme Value Analysis",
    fluidRow(
      column(12,
        h3("Extreme Value Statistics (Murakami / ASTM E2283)"),
        helpText("Predicts the size of the largest inclusion expected over a larger inspection area, from the distribution of per-field maximum inclusion sizes (Gumbel probability plot)."),

        div(style = "border: 1px solid #17a2b8; padding: 15px; border-radius: 5px; margin: 10px 0; background-color: #d1ecf1;",
          h5(" How it works", style = "margin-top: 0; color: #0c5460;"),
          tags$ul(
            tags$li("The inspected area is split into equal 'control areas' - ideally one SEM field-of-view each, identified by a field/frame ID column in your data."),
            tags$li("The largest inclusion (by sqrtArea) in each control area is its block maximum."),
            tags$li("Block maxima are fit to a Gumbel probability plot; the fitted line predicts the largest inclusion over T control areas."),
            tags$li(strong("A real field/frame ID column is required."),
              " Each distinct value of that column is one control area of known physical size, so T = 100 means a real area 100x one field. ",
              "The method has no valid meaning without genuine per-field grouping - splitting a flat row list into N arbitrary chunks is not a set of control areas (it depends on sort order and forces equal inclusion counts rather than equal areas), so that fallback has been removed."),
            tags$li("Method: ", cite_link("Murakami, 1994", "https://doi.org/10.6028/jres.099.032"),
              ", standardized in ", cite_link("ASTM E2283-08(2019)"), ". ",
              "Goodness-of-fit is tested with the Anderson-Darling statistic (",
              cite_link("Anderson & Darling, 1952", "https://doi.org/10.1214/aoms/1177729437"), "; ",
              cite_link("Stephens, 1977", "https://doi.org/10.1093/biomet/64.3.583"), ")")
          )
        ),

        fluidRow(
          column(6,
            h4("File Selection"),
            fileInput(ns("evs_files"), "Select Excel File(s)", multiple = TRUE, accept = c(".xlsx", ".xls")),
            helpText("Each file's Sheet 1 is read and combined.")
          ),
          column(6,
            h4("Control Area Grouping"),
            selectInput(ns("evs_area_col"), "Area column (um2):", choices = NULL),
            selectInput(ns("evs_group_col"), "Field / frame ID column (required):", choices = NULL),
            div(style = "color: #856404; background-color: #fff3cd; border: 1px solid #ffeeba; border-radius: 4px; padding: 8px; font-size: 12px;",
              strong("Required: "), "pick the column that identifies which SEM field / frame each inclusion came from. ",
              "Each distinct value becomes one control area. If your export has no such column, EVS cannot be run on it - the method needs genuine per-field grouping, not an arbitrary split of the row list."
            )
          )
        ),

        fluidRow(
          column(12, create_pre_filter_ui(ns, "evs"))
        ),

        fluidRow(
          column(12, style = "text-align: center; margin-top: 10px;",
            actionButton(ns("evs_fit"), "Fit Extreme Value Model", class = "btn-primary btn-lg", style = "font-size: 18px;")
          )
        ),

        fluidRow(
          column(12,
            verbatimTextOutput(ns("evs_status")),
            uiOutput(ns("evs_gof_warning"))
          )
        ),

        fluidRow(
          column(4,
            h4("Prediction"),
            numericInput(ns("evs_return_period"), "Return period T (multiples of the control area):", value = 100, min = 1.01, step = 1),
            helpText("E.g. T = 100 predicts the largest inclusion expected over 100x one control area (one SEM field / frame)."),
            downloadButton(ns("evs_download_plot"), "Download plot (PNG)"),
            br(), br(),
            downloadButton(ns("evs_download_table"), "Download block maxima (xlsx)")
          ),
          column(8,
            # width="821px" matches server_evs.R's derived width (575px
            # height * 10:7 download aspect ratio).
            plotOutput(ns("evs_plot"), width = "821px", height = "575px")
          )
        ),

        fluidRow(
          column(12,
            h4("Fit Summary"),
            tableOutput(ns("evs_summary_table"))
          )
        )
      )
    )
  )
}
