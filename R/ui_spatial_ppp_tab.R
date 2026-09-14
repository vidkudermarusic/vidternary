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

        info_box(" How it works",
          tags$ul(
            tags$li("The point pattern is converted to a planar point process bounded by the ", strong("convex hull"), " of the data (default) - the smallest polygon containing every point, used as the observation window since no separately-known study-region boundary exists. Switch to ", strong("bounding box"), " below if the analysed region is a rectangular SEM scan."),
            tags$li(strong("Ripley's K(r): "), "the expected number of further points within distance r of a typical point, divided by intensity - compared against the complete-spatial-randomness (CSR) expectation K(r) = pir2. Above the CSR line suggests clustering at that distance; below suggests regularity."),
            tags$li(strong("L(r): "), "a variance-stabilizing transform, L(r) = sqrt(K(r)/pi), so CSR plots as a straight line through the origin. L(r) - r above zero is the conventional clustering diagnostic."),
            tags$li(strong("G(r): "), "the empirical distribution of nearest-neighbour distances - rises faster than the CSR expectation under clustering, slower under inhibition/regularity."),
            tags$li(strong("CSR envelope test: "), "simulates the chosen number of random CSR patterns in the same window. By default it builds a ", strong("global (simultaneous)"), " band - the observed L(r) - r leaving it ", em("anywhere"), " is a valid significant departure from CSR for the whole curve. The ", strong("pointwise"), " option (per-r min/max) looks tighter but, under true randomness, the observed curve strays outside it at ", em("some"), " r far more than 5% of the time, so a pointwise band is descriptive only, not a test."),
            tags$li(strong("Kernel intensity map: "), "a smoothed, ", strong("edge-corrected"), " (Diggle) estimate of point density across the observation window, revealing spatial hotspots. Colour values are a ", em("local"), " density (points per unit2 of your X/Y coordinate units), not a running point count - they can legitimately exceed your total number of points in a tight cluster, since a small area within that cluster can carry far more than its \"fair share\" of the total per unit area. The summary table below shows both the mean intensity (n / window area) and the map's own peak value side by side, so that relationship is visible directly."),
            tags$li(strong("Homogeneity assumption: "), "K/L/G and the CSR envelope all assume one uniform underlying process. Inclusions whose ", em("density"), " varies across the section for reasons unrelated to point interaction (banding, an edge-affected zone, a compositional gradient) will breach the CSR envelope purely from that inhomogeneity - the test cannot separate \"points attract each other\" from \"some regions simply have more points\". Read a breach together with the intensity map, not on its own."),
            tags$li("Method: ", cite_link("Ripley, 1977", "https://doi.org/10.1111/j.2517-6161.1977.tb01615.x"),
              " (K-function and, in the published discussion of that paper, Besag's L-transform); general reference: ",
              cite_link("Baddeley, Rubak & Turner, 2015", "https://doi.org/10.1201/b19708"), ".")
          )
        ),

        fluidRow(
          column(6,
            create_file_selection_column(ns("ppp_files"))
          ),
          column(6,
            h4("Coordinates"),
            create_xy_coordinate_selectors(ns("ppp_x_col"), ns("ppp_y_col")),
            selectInput(ns("ppp_mark_col"), "Mark column (optional, for point-pattern plot colour):", choices = c("None" = "none"))
          )
        ),

        fluidRow(
          column(6,
            h4("CSR Envelope"),
            numericInput(ns("ppp_nsim"), "Number of CSR envelope simulations:", value = 99, min = 19, max = 999, step = 10),
            helpText("More simulations give a finer-grained envelope at the cost of runtime. K/L/G functions themselves are computed once regardless of this setting - only the envelope test re-simulates."),
            checkboxInput(ns("ppp_global_envelope"),
              "Global (simultaneous) envelope - a valid whole-curve test", value = TRUE),
            helpText("Untick for the pointwise per-r band (looks tighter, but is descriptive only - see \"How it works\" above).")
          ),
          column(6,
            h4("Observation window & intensity"),
            radioButtons(ns("ppp_window"), "Observation window:",
              choices = c("Convex hull of the points" = "convex_hull",
                          "Rectangular scan area (bounding box)" = "rectangle"),
              selected = "convex_hull"),
            checkboxInput(ns("ppp_edge_correct"),
              "Edge-correct the kernel intensity map (Diggle)", value = TRUE),
            helpText("Edge correction removes the boundary under-estimate in the hotspot map; leave it on unless you specifically want the raw estimator.")
          )
        ),

        fluidRow(
          column(12, create_pre_filter_ui(ns, "ppp"))
        ),

        centered_action_button_row(ns("ppp_analyze"), "Analyze Point Pattern", margin_top = "10px"),

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
