# ---- UI: "Hexagonal Ternary Diagram" tab ----
# Composites 6 triangular ternary plots (sharing a central element/
# combination) into one hexagonal image from a single uploaded file.
# See R/hex_ternary_plot.R for the plotting logic and R/server_hex_ternary.R
# for the Shiny wiring.

HEX_ELEMENT_SLOT_LABELS <- c(
  "Element 1 (A) – zgoraj",
  "Element 2 (B) – zgoraj desno",
  "Element 3 (C) – osrednja os (v vseh 6 diagramih)",
  "Element 4 (D) – zgoraj levo",
  "Element 5 (E) – spodaj levo",
  "Element 6 (F) – spodaj",
  "Element 7 (G) – spodaj desno"
)

# Legend diagram for the Purpose box: which of the 6 composited triangles
# (numbered 1-6, matching hex_ternary_plot.R's panel order) sits at which
# pair of outer element positions (roman numerals I-VII, matching the A-G
# slots above - III is skipped as an outer vertex since Element 3/C is the
# shared central axis of every triangle, drawn at the hexagon's center
# instead). Inline SVG rather than a bundled image file - no static asset
# serving needed, and it stays crisp at any size.
HEX_LEGEND_SVG <- '
<svg viewBox="0 0 320 300" xmlns="http://www.w3.org/2000/svg" style="max-width: 320px; width: 100%; height: auto;">
  <polygon points="105,54.7 215,54.7 270,150 215,245.3 105,245.3 50,150" fill="none" stroke="#333" stroke-width="1.5"/>
  <polygon points="105,54.7 215,54.7 160,150" fill="#E8963E" fill-opacity="0.85"/>
  <polygon points="215,54.7 270,150 160,150" fill="#4CAF9E" fill-opacity="0.85"/>
  <polygon points="270,150 215,245.3 160,150" fill="#D9C24A" fill-opacity="0.85"/>
  <polygon points="215,245.3 105,245.3 160,150" fill="#8BC34A" fill-opacity="0.85"/>
  <polygon points="105,245.3 50,150 160,150" fill="#D6598F" fill-opacity="0.85"/>
  <polygon points="50,150 105,54.7 160,150" fill="#9575CD" fill-opacity="0.85"/>
  <line x1="160" y1="150" x2="105" y2="54.7" stroke="#333" stroke-width="1" stroke-dasharray="3,3"/>
  <line x1="160" y1="150" x2="215" y2="54.7" stroke="#333" stroke-width="1" stroke-dasharray="3,3"/>
  <line x1="160" y1="150" x2="270" y2="150" stroke="#333" stroke-width="1" stroke-dasharray="3,3"/>
  <line x1="160" y1="150" x2="215" y2="245.3" stroke="#333" stroke-width="1" stroke-dasharray="3,3"/>
  <line x1="160" y1="150" x2="105" y2="245.3" stroke="#333" stroke-width="1" stroke-dasharray="3,3"/>
  <line x1="160" y1="150" x2="50" y2="150" stroke="#333" stroke-width="1" stroke-dasharray="3,3"/>
  <circle cx="160" cy="150" r="4" fill="#e74c3c"/>
  <text x="176" y="146" font-size="15" font-weight="bold" fill="#333">III</text>
  <text x="160" y="80" font-size="16" font-weight="bold" fill="#222" text-anchor="middle">2</text>
  <text x="205" y="118" font-size="16" font-weight="bold" fill="#222" text-anchor="middle">1</text>
  <text x="205" y="185" font-size="16" font-weight="bold" fill="#222" text-anchor="middle">6</text>
  <text x="160" y="222" font-size="16" font-weight="bold" fill="#222" text-anchor="middle">5</text>
  <text x="115" y="185" font-size="16" font-weight="bold" fill="#222" text-anchor="middle">4</text>
  <text x="115" y="118" font-size="16" font-weight="bold" fill="#222" text-anchor="middle">3</text>
  <text x="105" y="35" font-size="15" font-weight="bold" fill="#333" text-anchor="middle">IV.</text>
  <text x="215" y="35" font-size="15" font-weight="bold" fill="#333" text-anchor="middle">I.</text>
  <text x="295" y="155" font-size="15" font-weight="bold" fill="#333" text-anchor="middle">II.</text>
  <text x="215" y="270" font-size="15" font-weight="bold" fill="#333" text-anchor="middle">VII.</text>
  <text x="105" y="270" font-size="15" font-weight="bold" fill="#333" text-anchor="middle">VI.</text>
  <text x="25" y="155" font-size="15" font-weight="bold" fill="#333" text-anchor="middle">V.</text>
</svg>
'

#' Build the "Hexagonal Ternary Diagram" tab's UI
#'
#' @param id Module namespace id - must match the id passed to
#'   `moduleServer()` for this tab in `server_logic.R`.
#' @return A `shiny::tabPanel()`.
#' @export
create_hex_ternary_tab <- function(id) {
  ns <- NS(id)
  tabPanel("Hexagonal Ternary Diagram",
    fluidRow(
      column(12,
        h3("Create a Hexagonal Joint Ternary Diagram"),
        helpText("Combines 6 triangular ternary plots that share a central axis into one composite image."),

        div(style = "border: 1px solid #17a2b8; padding: 15px; border-radius: 5px; margin: 10px 0; background-color: #d1ecf1;",
          h5("\U0001F3AF Purpose", style = "margin-top: 0; color: #0c5460;"),
          tags$ul(
            tags$li("Select one or more columns for each of the 7 positions (A-G) - multiple selections are summed."),
            tags$li("Element/position 3 (C) is the shared central axis and appears in all 6 triangles."),
            tags$li("Works from a single uploaded Excel file (Sheet 1).")
          ),
          div(style = "text-align: center; margin-top: 10px;",
            HTML(HEX_LEGEND_SVG),
            tags$p(style = "font-size: 12px; color: #0c5460; margin-top: 5px;",
              "Legend: triangles 1-6 are the six composited ternary plots; I, II, IV, V, VI, VII are Elements 1, 2, 4, 5, 6, 7 (A, B, D, E, F, G) at the outer positions above. III (Element 3 / C) sits at the shared center of every triangle.")
          )
        ),

        fluidRow(
          column(6,
            h4("File Selection"),
            fileInput(ns("hex_xlsx_file"), "Select Excel File", accept = c(".xlsx", ".xls")),
            helpText("The composite diagram is built from this file's Sheet 1.")
          ),
          column(6,
            h4("Output Settings"),
            helpText("Clicking \"Shrani diagram\" opens your browser's own Save dialog - choose where the file goes there."),
            textInput(ns("hex_output_folder"), "Filename for Diagram",
              value = "hex_ternary_diagrams", placeholder = "Enter filename")
          )
        ),

        fluidRow(
          column(12,
            h4("Element Selection (A-G)"),
            div(style = "border: 1px solid #dee2e6; padding: 15px; border-radius: 5px; margin: 10px 0; background-color: #f8f9fa;",
              fluidRow(
                column(4, selectizeInput(ns("hex_element_1"), HEX_ELEMENT_SLOT_LABELS[1], choices = NULL, multiple = TRUE)),
                column(4, selectizeInput(ns("hex_element_2"), HEX_ELEMENT_SLOT_LABELS[2], choices = NULL, multiple = TRUE)),
                column(4, selectizeInput(ns("hex_element_3"), HEX_ELEMENT_SLOT_LABELS[3], choices = NULL, multiple = TRUE))
              ),
              fluidRow(
                column(4, selectizeInput(ns("hex_element_4"), HEX_ELEMENT_SLOT_LABELS[4], choices = NULL, multiple = TRUE)),
                column(4, selectizeInput(ns("hex_element_5"), HEX_ELEMENT_SLOT_LABELS[5], choices = NULL, multiple = TRUE)),
                column(4, selectizeInput(ns("hex_element_6"), HEX_ELEMENT_SLOT_LABELS[6], choices = NULL, multiple = TRUE))
              ),
              fluidRow(
                column(4, selectizeInput(ns("hex_element_7"), HEX_ELEMENT_SLOT_LABELS[7], choices = NULL, multiple = TRUE))
              )
            )
          )
        ),

        fluidRow(
          column(12, style = "text-align: center; margin-top: 10px;",
            actionButton(ns("hex_generate"), "Ustvari heksagonalni diagram",
              class = "btn-primary btn-lg", style = "font-size: 18px;"),
            br(), br(),
            downloadButton(ns("hex_save"), "Shrani diagram",
              class = "btn-success btn-lg", style = "font-size: 18px;")
          )
        ),

        fluidRow(
          column(12,
            verbatimTextOutput(ns("hex_status")),
            uiOutput(ns("hex_plot_container"))
          )
        )
      )
    )
  )
}
