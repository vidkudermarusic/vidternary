# ---- UI: "Spatial Clustering" tab ----
# Clark-Evans nearest-neighbour test of complete spatial randomness (CSR)
# on inclusion X/Y positions. See R/spatial_clustering_analysis.R for the
# statistics/plotting and R/server_spatial.R for the Shiny wiring.

create_spatial_tab <- function() {
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
            tags$li(strong("Two p-values are reported: "), "an asymptotic one (Donnelly edge-corrected, the standard method used by the spatstat R package) and a Monte Carlo one (simulates many random point sets in the same area - more reliable for smaller point counts). Trust the Monte Carlo value when the two disagree.")
          )
        ),

        fluidRow(
          column(6,
            h4("File Selection"),
            fileInput("spatial_files", "Select Excel File(s)", multiple = TRUE, accept = c(".xlsx", ".xls")),
            helpText("Each file's Sheet 1 is read and combined.")
          ),
          column(6,
            h4("Coordinates"),
            selectInput("spatial_x_col", "X coordinate column:", choices = NULL),
            selectInput("spatial_y_col", "Y coordinate column:", choices = NULL),
            selectInput("spatial_color_col", "Colour points by (optional):", choices = c("None" = "none"))
          )
        ),

        fluidRow(
          column(12, style = "text-align: center; margin-top: 10px;",
            actionButton("spatial_analyze", "Analyze Spatial Pattern", class = "btn-primary btn-lg", style = "font-size: 18px;")
          )
        ),

        fluidRow(
          column(12,
            verbatimTextOutput("spatial_status")
          )
        ),

        fluidRow(
          column(6, plotOutput("spatial_scatter_plot", height = "450px")),
          column(6, plotOutput("spatial_nnd_histogram", height = "450px"))
        ),

        fluidRow(
          column(12,
            h4("Results"),
            tableOutput("spatial_summary_table"),
            downloadButton("spatial_download_scatter", "Download scatter plot (PNG)"),
            downloadButton("spatial_download_histogram", "Download NND histogram (PNG)"),
            downloadButton("spatial_download_table", "Download NND values (xlsx)")
          )
        )
      )
    )
  )
}
