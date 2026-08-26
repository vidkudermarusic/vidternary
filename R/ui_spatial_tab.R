# ---- UI: "Spatial Clustering" tab ----
# Clark-Evans nearest-neighbour test of complete spatial randomness (CSR)
# on inclusion X/Y positions. See R/spatial_clustering_analysis.R for the
# statistics/plotting and R/server_spatial.R for the Shiny wiring.

#' Build the "Spatial Clustering" tab's UI
#'
#' @param id Module namespace id - must match the id passed to
#'   `moduleServer()` for this tab in `server_logic.R`.
#' @return A `shiny::tabPanel()`.
#' @export
create_spatial_tab <- function(id) {
  ns <- NS(id)
  tabPanel("Spatial Clustering",
    fluidRow(
      column(12,
        h3("Spatial Clustering / Spacing Analysis (Clark-Evans Test)"),
        helpText("Tests whether inclusion positions are randomly scattered, clustered together, or more evenly spread out than chance would predict."),

        div(style = "border: 1px solid #17a2b8; padding: 15px; border-radius: 5px; margin: 10px 0; background-color: #d1ecf1;",
          h5("🎯 How it works", style = "margin-top: 0; color: #0c5460;"),
          tags$ul(
            tags$li("For every point, the distance to its nearest neighbour is measured (nearest-neighbour distance, NND)."),
            tags$li("The observed mean NND is compared to what would be expected if the points were completely randomly scattered (CSR) in the same area."),
            tags$li("R = observed/expected: R < 1 means clustering, R > 1 means a more regular/even spread, R ≈ 1 means no evidence against randomness."),
            tags$li(strong("Two p-values are reported: "), "an asymptotic one (Donnelly edge-corrected, the standard method used by the spatstat R package) and a Monte Carlo one (simulates many random point sets in the same bounding box). Trust the Monte Carlo value when the two disagree."),
            tags$li(strong("Small-sample caveat (checked empirically, simulating known-random data): "), "both p-values over-report significance somewhat below ~40 points, because the sampling window is estimated from the same points being tested. This bias is asymmetric - at n=15 a false ", em("\"significantly dispersed\""), " verdict occurred ~18% of the time (Monte Carlo) vs a nominal 5%, while a false ", em("\"significantly clustered\""), " verdict occurred well under 1% of the time. In practice: a ", strong("clustered/banding"), " verdict is trustworthy even at small n; treat a ", strong("regular/dispersed"), " verdict with real skepticism below ~40 points, regardless of which p-value you look at."),
            tags$li("Method: ", cite_link("Clark & Evans, 1954", "https://doi.org/10.2307/1931034"),
              "; edge correction: ", cite_link("Donnelly, 1978"), ".")
          )
        ),

        fluidRow(
          column(6,
            h4("File Selection"),
            fileInput(ns("spatial_files"), "Select Excel File(s)", multiple = TRUE, accept = c(".xlsx", ".xls")),
            helpText("Each file's Sheet 1 is read and combined.")
          ),
          column(6,
            h4("Coordinates"),
            selectInput(ns("spatial_x_col"), "X coordinate column:", choices = NULL),
            selectInput(ns("spatial_y_col"), "Y coordinate column:", choices = NULL),
            selectInput(ns("spatial_color_col"), "Colour points by (optional):", choices = c("None" = "none"))
          )
        ),

        fluidRow(
          column(6,
            h4("Nearest-neighbour method"),
            selectInput(ns("spatial_nn_method"), "Calculation method:",
              choices = c("k-d tree (fast, recommended for large datasets)" = "kdtree",
                          "Distance matrix (slower for large n)" = "matrix"),
              selected = "kdtree"),
            helpText("Both give identical results - this only affects speed. The distance-matrix method is O(n²) and can take many minutes above a few thousand points; the k-d tree method is O(n log n) and stays fast even at tens of thousands of points.")
          ),
          column(6,
            h4("Monte Carlo simulations"),
            numericInput(ns("spatial_n_sim"), "Number of simulations:", value = 999, min = 49, max = 9999, step = 50),
            helpText("More simulations give a finer-grained p-value (the smallest reportable p-value is roughly 2/(simulations+1)) at the cost of runtime. With the k-d tree method this is cheap even at 999+; with the distance-matrix method, keep this lower for large datasets.")
          )
        ),

        fluidRow(
          column(12, style = "text-align: center; margin-top: 10px;",
            actionButton(ns("spatial_analyze"), "Analyze Spatial Pattern", class = "btn-primary btn-lg", style = "font-size: 18px;")
          )
        ),

        fluidRow(
          column(12,
            verbatimTextOutput(ns("spatial_status"))
          )
        ),

        fluidRow(
          # width="591px" matches server_spatial.R's spatial_plot_dim (517px
          # height * 8:7 download aspect ratio) - see that file's comment for
          # why the plot's *display* size, not just its internal device
          # size, has to be set explicitly to avoid an oversized preview.
          column(6, plotOutput(ns("spatial_scatter_plot"), width = "591px", height = "517px")),
          column(6, plotOutput(ns("spatial_nnd_histogram"), width = "591px", height = "517px"))
        ),

        fluidRow(
          column(12,
            h4("Results"),
            tableOutput(ns("spatial_summary_table")),
            downloadButton(ns("spatial_download_scatter"), "Download scatter plot (PNG)"),
            downloadButton(ns("spatial_download_histogram"), "Download NND histogram (PNG)"),
            downloadButton(ns("spatial_download_table"), "Download NND values (xlsx)")
          )
        )
      )
    )
  )
}
