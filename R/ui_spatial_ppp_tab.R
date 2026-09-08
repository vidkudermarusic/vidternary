# ---- UI: "Point Pattern Analysis" tab ----
# Ripley's K/L functions, the nearest-neighbour G-function, a Monte Carlo
# CSR envelope test, and a kernel intensity map on inclusion X/Y positions -
# a second, complementary spatial-randomness toolkit alongside the existing
# "Spatial Clustering" (Clark-Evans) tab. See
# R/spatial_point_pattern_analysis.R for the statistics/plotting and
# R/server_spatial_ppp.R for the Shiny wiring.

#' Build the "Point Pattern Analysis" tab's UI
#'
#' @param id Module namespace id - must match the id passed to
#'   `moduleServer()` for this tab in `server_logic.R`.
#' @return A `shiny::tabPanel()`.
#' @export
create_spatial_ppp_tab <- function(id) {
  ns <- NS(id)
  tabPanel("Point Pattern Analysis",
    fluidRow(
      column(12,
        h3("Spatial Point Pattern Analysis (K/L/G Functions, CSR Envelope, Kernel Intensity)"),
        helpText("A second way of testing whether inclusion positions are randomly scattered, clustered, or more evenly spread out than chance would predict - built on the standard spatial point process toolkit (spatstat), complementing the Spatial Clustering tab's single nearest-neighbour test."),

        div(style = "border: 1px solid #17a2b8; padding: 15px; border-radius: 5px; margin: 10px 0; background-color: #d1ecf1;",
          h5("🎯 How it works", style = "margin-top: 0; color: #0c5460;"),
          tags$ul(
            tags$li("The point pattern is converted to a planar point process bounded by the convex hull of the data - the smallest polygon containing every point, used as the observation window since no separately-known study-region boundary exists."),
            tags$li(strong("Ripley's K(r): "), "the expected number of further points within distance r of a typical point, divided by intensity - compared against the complete-spatial-randomness (CSR) expectation K(r) = πr². Above the CSR line suggests clustering at that distance; below suggests regularity."),
            tags$li(strong("L(r): "), "a variance-stabilizing transform, L(r) = √(K(r)/π), so CSR plots as a straight line through the origin. L(r) − r above zero is the conventional clustering diagnostic."),
            tags$li(strong("G(r): "), "the empirical distribution of nearest-neighbour distances - rises faster than the CSR expectation under clustering, slower under inhibition/regularity."),
            tags$li(strong("CSR envelope test: "), "simulates the chosen number of random CSR patterns in the same window to build a pointwise confidence band around L(r) − r. The observed curve leaving that band indicates a statistically significant departure from CSR at that distance."),
            tags$li(strong("Kernel intensity map: "), "a smoothed estimate of point density across the observation window, revealing spatial hotspots. Colour values are a ", em("local"), " density (points per unit² of your X/Y coordinate units), not a running point count - they can legitimately exceed your total number of points in a tight cluster, since a small area within that cluster can carry far more than its \"fair share\" of the total per unit area. The summary table below shows both the mean intensity (n ÷ window area) and the map's own peak value side by side, so that relationship is visible directly."),
            tags$li("Method: ", cite_link("Ripley, 1977", "https://doi.org/10.1111/j.2517-6161.1977.tb01615.x"),
              " (K-function and, in the published discussion of that paper, Besag's L-transform); general reference: ",
              cite_link("Baddeley, Rubak & Turner, 2015", "https://doi.org/10.1201/b19708"), ".")
          )
        ),

        fluidRow(
          column(6,
            h4("File Selection"),
            fileInput(ns("ppp_files"), "Select Excel File(s)", multiple = TRUE, accept = c(".xlsx", ".xls")),
            helpText("Each file's Sheet 1 is read and combined.")
          ),
          column(6,
            h4("Coordinates"),
            selectInput(ns("ppp_x_col"), "X coordinate column:", choices = NULL),
            selectInput(ns("ppp_y_col"), "Y coordinate column:", choices = NULL),
            selectInput(ns("ppp_mark_col"), "Mark column (optional, for point-pattern plot colour):", choices = c("None" = "none"))
          )
        ),

        fluidRow(
          column(6,
            h4("CSR Envelope"),
            numericInput(ns("ppp_nsim"), "Number of CSR envelope simulations:", value = 99, min = 19, max = 999, step = 10),
            helpText("More simulations give a finer-grained envelope at the cost of runtime. K/L/G functions themselves are computed once regardless of this setting - only the envelope test re-simulates.")
          )
        ),

        fluidRow(
          column(12, create_pre_filter_ui(ns, "ppp"))
        ),

        fluidRow(
          column(12, style = "text-align: center; margin-top: 10px;",
            actionButton(ns("ppp_analyze"), "Analyze Point Pattern", class = "btn-primary btn-lg", style = "font-size: 18px;")
          )
        ),

        fluidRow(
          column(12,
            verbatimTextOutput(ns("ppp_status"))
          )
        ),

        fluidRow(
          column(6, plotOutput(ns("ppp_pattern_plot"), width = "591px", height = "517px")),
          column(6, plotOutput(ns("ppp_intensity_plot"), width = "591px", height = "517px"))
        ),
        fluidRow(
          column(6, plotOutput(ns("ppp_k_plot"), width = "591px", height = "517px")),
          column(6, plotOutput(ns("ppp_l_plot"), width = "591px", height = "517px"))
        ),
        fluidRow(
          column(6, plotOutput(ns("ppp_g_plot"), width = "591px", height = "517px")),
          column(6, plotOutput(ns("ppp_envelope_plot"), width = "591px", height = "517px"))
        ),

        fluidRow(
          column(12,
            h4("Results"),
            tableOutput(ns("ppp_summary_table")),
            downloadButton(ns("ppp_download_pattern"), "Download point pattern plot (PNG)"),
            downloadButton(ns("ppp_download_intensity"), "Download intensity map (PNG)"),
            downloadButton(ns("ppp_download_k"), "Download K-function plot (PNG)"),
            downloadButton(ns("ppp_download_l"), "Download L-function plot (PNG)"),
            downloadButton(ns("ppp_download_g"), "Download G-function plot (PNG)"),
            downloadButton(ns("ppp_download_envelope"), "Download CSR envelope plot (PNG)"),
            downloadButton(ns("ppp_download_data"), "Download K/L/G/envelope values (xlsx)")
          )
        )
      )
    )
  )
}
