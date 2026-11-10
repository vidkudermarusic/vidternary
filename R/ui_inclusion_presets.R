# ---- UI: controls and outputs of the "Inclusion Presets" tab ----
# The body of the tab (create_inclusion_tab() in ui_inclusion_tab.R adds the
# heading, the how-it-works box and the file upload above it): preset,
# matrix-removal and compound options on the left; diagram, tables and
# downloads on the right. All input/output ids start with incl_.

#' Build the controls and outputs of the "Inclusion Presets" tab
#'
#' @param ns The tab's namespace function (`NS(id)`).
#' @return A `shiny::fluidRow()` with the controls (left) and the diagram,
#'   tables and downloads (right).
#' @export
create_inclusion_presets_panel <- function(ns) {
  box <- function(title, ..., color = "#17a2b8") {
    div(style = paste0("border: 2px solid ", color, "; padding: 10px; border-radius: 5px; margin: 10px 0;"),
        h4(style = paste0("color: ", color, "; margin-top: 0;"), title), ...)
  }
  fluidRow(
    column(4,
      box("Diagram",
        selectInput(ns("incl_preset"), "Preset:", choices = inclusion_preset_choices(), selected = "cao-al2o3-mgo"),
        uiOutput(ns("incl_preset_info")),
        uiOutput(ns("incl_basis_ui")),
        numericInput(ns("incl_threshold"), "Coverage threshold (%):", value = 50, min = 0, max = 100, step = 5),
        helpText("A particle is plotted only if the three corners make up at least this share of its analysed mass.")
      ),
      box("Matrix removal",
        selectInput(ns("incl_matrix_element"), "Matrix element:", choices = c("None (do not remove)" = "")),
        conditionalPanel(
          condition = paste0("input['", ns("incl_matrix_element"), "'] != ''"),
          radioButtons(ns("incl_matrix_mode"), "Remove:",
            choices = c("Matrix element only" = "matrix_only", "Matrix element and its alloying elements" = "matrix_and_alloys"),
            selected = "matrix_and_alloys"),
          conditionalPanel(
            condition = paste0("input['", ns("incl_matrix_mode"), "'] == 'matrix_and_alloys'"),
            radioButtons(ns("incl_steel_source"), "Steel composition:",
              choices = c("Estimate from the data (most matrix-rich particles)" = "estimate",
                          "Enter it (wt%)" = "enter")),
            conditionalPanel(
              condition = paste0("input['", ns("incl_steel_source"), "'] == 'estimate'"),
              selectizeInput(ns("incl_alloy_elements"), "Alloying elements to correct:", choices = NULL, multiple = TRUE)
            ),
            conditionalPanel(
              condition = paste0("input['", ns("incl_steel_source"), "'] == 'enter'"),
              textInput(ns("incl_steel_composition"), "Steel composition (wt%):", placeholder = "Fe=70, Cr=20, Ni=10"),
              helpText("Symbol=value pairs, comma separated, decimal point for decimals. The listed elements are the ones corrected.")
            ),
            helpText("Each corrected element is reduced by the steel's share carried by the matrix signal; negative results become 0.")
          ),
          fluidRow(
            column(6, numericInput(ns("incl_max_matrix"), "Matrix particle above (%):", value = 80, min = 1, max = 100, step = 5)),
            column(6, numericInput(ns("incl_min_residual"), "No signal below (%):", value = 5, min = 0, max = 100, step = 1))
          ),
          helpText("Particles that are more than the first value matrix, or keep less than the second value of their analysis after removal, are not plotted.")
        ),
        selectizeInput(ns("incl_ignore_elements"), "Ignore elements (e.g. carbon contamination):", choices = NULL, multiple = TRUE)
      ),
      conditionalPanel(
        condition = paste0("output['", ns("incl_is_compound"), "'] == 'true'"),
        box("Compounds",
          selectInput(ns("incl_s_order"), "Sulfur goes to:",
            choices = c("Ca first, then Mn" = "ca_mn", "Mn only" = "mn", "Ca only" = "ca")),
          selectInput(ns("incl_ti_as"), "Titanium counts as:",
            choices = c("TiN (all Ti)" = "TiN", "TiO2 (all Ti)" = "TiO2",
                        "By measured N: TiN, then AlN, rest TiO2" = "by_N")),
          helpText("Oxygen is calculated from the oxides; measured O is only a check. The order is a convention - state it when reporting results.")
        )
      ),
      box("Display",
        checkboxInput(ns("incl_show_lines"), "Classification lines", value = TRUE),
        checkboxInput(ns("incl_show_reference"), "Reference phases", value = TRUE),
        checkboxInput(ns("incl_show_legend"), "Class legend", value = TRUE),
        checkboxInput(ns("incl_show_notes"), "Settings note under the diagram", value = TRUE),
        sliderInput(ns("incl_point_size"), "Point size:", min = 0.1, max = 3, value = 0.5, step = 0.1)
      )
    ),
    column(8,
      uiOutput(ns("incl_message")),
      plotOutput(ns("incl_plot"), height = "680px"),
      div(style = "text-align: center; margin: 10px 0;",
        downloadButton(ns("incl_download_plot"), "Save diagram (PNG)", class = "btn-primary", style = "margin: 0 8px;"),
        downloadButton(ns("incl_download_table"), "Save results (xlsx)", class = "btn-primary", style = "margin: 0 8px;")
      ),
      fluidRow(
        column(6, h5("Classes"), tableOutput(ns("incl_summary_table"))),
        column(6, h5("Particles"), tableOutput(ns("incl_status_table")))
      ),
      h5("Settings"),
      verbatimTextOutput(ns("incl_settings"))
    )
  )
}
