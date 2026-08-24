# ---- UI: "Compositional Analysis" tab ----
# Log-ratio (CLR/ILR) transforms + PCA for Wt% compositional chemistry
# data. See R/compositional_data_analysis.R for the statistics/plotting
# and R/server_coda.R for the Shiny wiring.

create_coda_tab <- function() {
  tabPanel("Compositional Analysis",
    fluidRow(
      column(12,
        h3("Compositional Data Analysis (CLR / ILR)"),
        helpText("Wt% chemistry columns are compositional data (constrained to sum to ~100%), so ordinary statistics/PCA on raw percentages can be misleading. Log-ratio transforms fix this before running PCA."),

        div(style = "border: 1px solid #17a2b8; padding: 15px; border-radius: 5px; margin: 10px 0; background-color: #d1ecf1;",
          h5("🎯 How it works", style = "margin-top: 0; color: #0c5460;"),
          tags$ul(
            tags$li(strong("CLR"), " (centered log-ratio): each element's log-share relative to the geometric mean of all selected elements. Each axis maps directly to one element, so PCA loadings/biplots stay directly interpretable - used for the biplot below."),
            tags$li(strong("ILR"), " (isometric log-ratio): an orthonormal-coordinate version with a non-singular covariance matrix - each coordinate is an abstract contrast between groups of elements rather than one single element, but gives identical PCA/distance structure to CLR. Available as a download for use in methods that need non-singular covariance."),
            tags$li("Zeros are replaced with a small pseudo-count (half the smallest positive value found) before taking logs, since log(0) is undefined.")
          )
        ),

        fluidRow(
          column(6,
            h4("File Selection"),
            fileInput("coda_files", "Select Excel File(s)", multiple = TRUE, accept = c(".xlsx", ".xls")),
            helpText("Each file's Sheet 1 is read and combined.")
          ),
          column(6,
            h4("Compositional Parts"),
            selectizeInput("coda_parts", "Element / Wt% columns (select 3 or more):", choices = NULL, multiple = TRUE),
            helpText("Columns matching \"(Wt%)\" are pre-selected automatically when available.")
          )
        ),

        fluidRow(
          column(12, style = "text-align: center; margin-top: 10px;",
            actionButton("coda_run", "Transform & Run PCA", class = "btn-primary btn-lg", style = "font-size: 18px;")
          )
        ),

        fluidRow(
          column(12,
            verbatimTextOutput("coda_status")
          )
        ),

        fluidRow(
          column(8, plotOutput("coda_biplot", height = "500px")),
          column(4,
            h4("Variance Explained"),
            tableOutput("coda_variance_table"),
            tags$hr(),
            downloadButton("coda_download_clr", "Download CLR-transformed data (xlsx)"),
            br(), br(),
            downloadButton("coda_download_ilr", "Download ILR-transformed data (xlsx)"),
            br(), br(),
            downloadButton("coda_download_biplot", "Download biplot (PNG)")
          )
        ),

        fluidRow(
          column(12,
            h4("PCA Loadings (CLR basis)"),
            tableOutput("coda_loadings_table")
          )
        )
      )
    )
  )
}
