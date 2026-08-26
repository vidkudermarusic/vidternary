# ---- UI: "Compositional Analysis" tab ----
# Log-ratio (CLR/ILR) transforms + PCA for Wt% compositional chemistry
# data. See R/compositional_data_analysis.R for the statistics/plotting
# and R/server_coda.R for the Shiny wiring.

#' Build the "Compositional Analysis" tab's UI
#'
#' @param id Module namespace id - must match the id passed to
#'   `moduleServer()` for this tab in `server_logic.R`.
#' @return A `shiny::tabPanel()`.
#' @export
create_coda_tab <- function(id) {
  ns <- NS(id)
  tabPanel("Compositional Analysis",
    fluidRow(
      column(12,
        h3("Compositional Data Analysis (CLR / ILR)"),
        helpText("Wt% chemistry columns are compositional data (constrained to sum to ~100%), so ordinary statistics/PCA on raw percentages can be misleading. Log-ratio transforms fix this before running PCA."),

        div(style = "border: 1px solid #17a2b8; padding: 15px; border-radius: 5px; margin: 10px 0; background-color: #d1ecf1;",
          h5("🎯 How it works", style = "margin-top: 0; color: #0c5460;"),
          tags$ul(
            tags$li(strong("CLR"), " (centered log-ratio): each element's log-share relative to the geometric mean of all selected elements. Each axis maps directly to one element, so PCA loadings/biplots stay directly interpretable - used for the biplot below. ", cite_link("Aitchison, 1986"), "."),
            tags$li(strong("ILR"), " (isometric log-ratio): an orthonormal-coordinate version with a non-singular covariance matrix - each coordinate is an abstract contrast between groups of elements rather than one single element, but gives identical PCA/distance structure to CLR. Available as a download for use in methods that need non-singular covariance. ", cite_link("Egozcue et al., 2003", "https://doi.org/10.1023/A:1023818214614"), "."),
            tags$li("Zeros are replaced with a small pseudo-count (half the smallest positive value found) before taking logs, since log(0) is undefined - a simplified version of ", cite_link("Martín-Fernández et al., 2003", "https://doi.org/10.1023/A:1023866030544"), "."),
            tags$li("PCA: ", cite_link("Jolliffe, 2002", "https://doi.org/10.1007/b98835"), ".")
          )
        ),

        fluidRow(
          column(6,
            h4("File Selection"),
            fileInput(ns("coda_files"), "Select Excel File(s)", multiple = TRUE, accept = c(".xlsx", ".xls")),
            helpText("Each file's Sheet 1 is read and combined.")
          ),
          column(6,
            h4("Compositional Parts"),
            selectizeInput(ns("coda_parts"), "Element / Wt% columns (select 3 or more):", choices = NULL, multiple = TRUE),
            helpText("Columns matching \"(Wt%)\" are pre-selected automatically when available.")
          )
        ),

        fluidRow(
          column(12, style = "text-align: center; margin-top: 10px;",
            actionButton(ns("coda_run"), "Transform & Run PCA", class = "btn-primary btn-lg", style = "font-size: 18px;")
          )
        ),

        fluidRow(
          column(12,
            verbatimTextOutput(ns("coda_status"))
          )
        ),

        fluidRow(
          column(6,
            h4("Biplot (CLR basis)"),
            # width="617px" matches server_coda.R's derived width (480px
            # height * 9:7 download aspect ratio).
            plotOutput(ns("coda_biplot"), width = "617px", height = "480px")
          ),
          column(6,
            h4("Biplot (ILR basis)"),
            plotOutput(ns("coda_biplot_ilr"), width = "617px", height = "480px")
          )
        ),

        fluidRow(
          column(4,
            h4("Variance Explained"),
            tableOutput(ns("coda_variance_table")),
            helpText("Identical for both bases, since ILR is an isometry of CLR.")
          ),
          column(4,
            h4("Download data"),
            downloadButton(ns("coda_download_clr"), "CLR-transformed data (xlsx)"),
            br(), br(),
            downloadButton(ns("coda_download_ilr"), "ILR-transformed data (xlsx)")
          ),
          column(4,
            h4("Download biplots"),
            downloadButton(ns("coda_download_biplot"), "CLR biplot (PNG)"),
            br(), br(),
            downloadButton(ns("coda_download_biplot_ilr"), "ILR biplot (PNG)")
          )
        ),

        fluidRow(
          column(6,
            h4("PCA Loadings (CLR basis)"),
            helpText("Each row is one element - directly interpretable."),
            tableOutput(ns("coda_loadings_table"))
          ),
          column(6,
            h4("PCA Loadings (ILR basis)"),
            helpText("Each row is an abstract balance (ilr_j contrasts the mean of parts 1..j against part j+1, in the order the parts are listed above) - not per-element, but usable where a non-singular covariance matrix is required. Variance explained and PC scores are identical to the CLR basis, since ILR is an isometry of CLR."),
            tableOutput(ns("coda_loadings_table_ilr"))
          )
        )
      )
    )
  )
}
