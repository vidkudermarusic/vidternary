# ---- UI: "Batch Comparison" tab ----
# Compares several batches (heats, specimens), one file each: amount of
# inclusions, size distribution, chemistry and largest-inclusion estimate.
# See R/batch_pipeline.R for the analysis and R/server_batch.R for the
# Shiny wiring.

#' Build the "Batch Comparison" tab's UI
#'
#' @param id Module namespace id - must match the id passed to
#'   `moduleServer()` for this tab in `server_logic.R`.
#' @return A `shiny::tabPanel()`.
#' @export
create_batch_tab <- function(id) {
  ns <- NS(id)
  tabPanel("Batch Comparison",
    fluidRow(
      column(12,
        h3("Batch Comparison: Do the Batches Differ, and by How Much?"),
        helpText("Upload one file per batch (heat, specimen). The batches are compared in the amount of inclusions, their size distribution, their chemistry and the largest inclusion to expect."),

        info_box(" How it works",
          tags$ul(
            tags$li(strong("Amount: "), "particles per mm2 and area fraction per batch with intervals; density ratios per pair with the exact Poisson test. Inclusions cluster, which makes Poisson p-values too small - the index of dispersion of the counts per SEM field shows it, and a rank test on those counts is given as well."),
            tags$li(strong("Size: "), "the whole distributions are compared with the k-sample Anderson-Darling test, which is sensitive in the tail (the large inclusions); the p-value comes from permuting the batch labels."),
            tags$li(strong("Chemistry: "), "the chosen elements are compared in log-ratio coordinates with a permutation MANOVA; R2 says how much of the variation the batches explain, and the clr means show which elements differ."),
            tags$li(strong("Largest inclusion: "), "the extreme-value fit of the EVS tab, run on each batch with the same control area and return period."),
            tags$li(strong("Reading the result: "), "with many particles every difference is 'significant'. Look at the ratios, quantiles and R2 first, and at the p-value only to rule out chance."),
            tags$li("Methods: ", cite_link("Scholz & Stephens, 1987", "https://doi.org/10.1080/01621459.1987.10478517"), ", ",
              cite_link("Anderson, 2001", "https://doi.org/10.1111/j.1442-9993.2001.01070.pp.x"), ", ",
              cite_link("Murakami, 1994", "https://doi.org/10.6028/jres.099.032"), ".")
          )
        ),

        fluidRow(
          column(4,
            h4("Files"),
            fileInput(ns("batch_files"), "Select one file per batch (2 or more)", multiple = TRUE, accept = c(".xlsx", ".csv", ".txt", ".tsv")),
            helpText("The file name is the batch label. Only columns present in every file can be chosen."),
            verbatimTextOutput(ns("batch_file_info"))
          ),
          column(4,
            h4("Columns"),
            selectInput(ns("batch_area_col"), "Area (um2):", choices = NULL),
            selectInput(ns("batch_field_col"), "SEM field ID (optional):", choices = c("None" = "none")),
            selectizeInput(ns("batch_parts"), "Elements to compare (optional, 2 or more):", choices = NULL, multiple = TRUE,
                           options = list(placeholder = "Chemistry columns")),
            helpText("Pre-selected: element columns with at most 10 % zero readings, without C, N and O (peak overlaps and contamination).")
          ),
          column(4,
            h4("Inspected area"),
            create_xy_coordinate_selectors(ns("batch_x_col"), ns("batch_y_col"), "X coordinate column (optional):", "Y coordinate column (optional):"),
            selectInput(ns("batch_coord_unit"), "Coordinate unit:", choices = c("mm" = "mm", "um" = "um"), selected = "mm"),
            textInput(ns("batch_inspected_area"), "Inspected area in mm2 (optional):", value = "", placeholder = "one value, or one per file: 1.9, 1.9, 2.4"),
            helpText("Leave empty to take the area spanned by the coordinates of each file.")
          )
        ),
        fluidRow(
          column(4,
            h4("Largest inclusion"),
            numericInput(ns("batch_fields_per_area"), "Fields per control area:", value = 1, min = 1, step = 1),
            numericInput(ns("batch_return_period"), "Return period T (control areas):", value = 100, min = 1.01, step = 1)
          ),
          column(4,
            h4("Permutation tests"),
            numericInput(ns("batch_n_perm"), "Number of permutations:", value = 999, min = 99, max = 9999, step = 100),
            helpText("The smallest p-value that can be reported is 1 / (permutations + 1).")
          )
        ),

        fluidRow(column(12, create_pre_filter_ui(ns, "batch"))),
        fluidRow(column(12, settings_io_ui(ns, "batch"))),
        centered_action_button_row(ns("batch_run"), "Compare Batches", margin_top = "10px"),

        fluidRow(
          column(12,
            verbatimTextOutput(ns("batch_status")),
            uiOutput(ns("batch_notes")),
            downloadButton(ns("batch_download_tables"), "Download all tables (xlsx)"),
            downloadButton(ns("batch_download_report"), "Download report (HTML)"),
            downloadButton(ns("batch_download_script"), "Download R script")
          )
        ),

        tabsetPanel(
          tabPanel("Overview",
            h4("The batches at a glance"),
            div(style = "overflow-x: auto;", tableOutput(ns("batch_summary_table"))),
            h4("Do the batches differ?"),
            tableOutput(ns("batch_overall_table"))
          ),
          tabPanel("Amount",
            fluidRow(
              column(6, plotOutput(ns("batch_density_plot"), height = "360px")),
              column(6, plotOutput(ns("batch_area_plot"), height = "360px"))
            ),
            h4("Number density, pair by pair"),
            tableOutput(ns("batch_density_pairs_table"))
          ),
          tabPanel("Size",
            plotOutput(ns("batch_size_plot"), height = "420px"),
            h4("Size quantiles"),
            tableOutput(ns("batch_size_quantiles_table")),
            h4("Size distribution, pair by pair"),
            tableOutput(ns("batch_size_pairs_table"))
          ),
          tabPanel("Composition",
            plotOutput(ns("batch_clr_plot"), height = "400px"),
            h4("Clr mean of each batch minus the clr mean of all particles"),
            helpText("Positive = the batch holds relatively more of that element; exp(value) is the factor."),
            tableOutput(ns("batch_clr_table")),
            h4("Composition, pair by pair"),
            tableOutput(ns("batch_composition_pairs_table")),
            h4("Spread of the composition"),
            tableOutput(ns("batch_spread_table"))
          ),
          tabPanel("Largest inclusion",
            plotOutput(ns("batch_evs_plot"), height = "380px"),
            tableOutput(ns("batch_evs_table"))
          )
        )
      )
    )
  )
}
