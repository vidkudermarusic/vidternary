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

create_hex_ternary_tab <- function() {
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
          )
        ),

        fluidRow(
          column(6,
            h4("File Selection"),
            fileInput("hex_xlsx_file", "Select Excel File", accept = c(".xlsx", ".xls")),
            helpText("The composite diagram is built from this file's Sheet 1.")
          ),
          column(6,
            h4("Output Settings"),
            helpText("Output directory: Use the 'Directory Settings' section at the bottom of the app."),
            textInput("hex_output_folder", "Folder Name for Diagram",
              value = "hex_ternary_diagrams", placeholder = "Enter folder name")
          )
        ),

        fluidRow(
          column(12,
            h4("Element Selection (A-G)"),
            div(style = "border: 1px solid #dee2e6; padding: 15px; border-radius: 5px; margin: 10px 0; background-color: #f8f9fa;",
              fluidRow(
                column(4, selectizeInput("hex_element_1", HEX_ELEMENT_SLOT_LABELS[1], choices = NULL, multiple = TRUE)),
                column(4, selectizeInput("hex_element_2", HEX_ELEMENT_SLOT_LABELS[2], choices = NULL, multiple = TRUE)),
                column(4, selectizeInput("hex_element_3", HEX_ELEMENT_SLOT_LABELS[3], choices = NULL, multiple = TRUE))
              ),
              fluidRow(
                column(4, selectizeInput("hex_element_4", HEX_ELEMENT_SLOT_LABELS[4], choices = NULL, multiple = TRUE)),
                column(4, selectizeInput("hex_element_5", HEX_ELEMENT_SLOT_LABELS[5], choices = NULL, multiple = TRUE)),
                column(4, selectizeInput("hex_element_6", HEX_ELEMENT_SLOT_LABELS[6], choices = NULL, multiple = TRUE))
              ),
              fluidRow(
                column(4, selectizeInput("hex_element_7", HEX_ELEMENT_SLOT_LABELS[7], choices = NULL, multiple = TRUE))
              )
            )
          )
        ),

        fluidRow(
          column(12, style = "text-align: center; margin-top: 10px;",
            actionButton("hex_generate", "Ustvari heksagonalni diagram",
              class = "btn-primary btn-lg", style = "font-size: 18px;"),
            br(), br(),
            actionButton("hex_save", "Shrani v izhodno mapo",
              class = "btn-success btn-lg", style = "font-size: 18px;")
          )
        ),

        fluidRow(
          column(12,
            verbatimTextOutput("hex_status"),
            uiOutput("hex_plot_container")
          )
        )
      )
    )
  )
}
