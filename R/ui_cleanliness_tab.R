# ---- UI: "Steel Cleanliness" tab ----
# Chart rating after ISO 4967 / ASTM E45 from automated SEM data, stringer
# detection, number density / area fraction, and stereology with a mass
# balance. See R/cleanliness_pipeline.R for the analysis and
# R/server_cleanliness.R for the Shiny wiring.

# Column, standard and analysis settings (the upper half of the tab).
cleanliness_settings_ui <- function(ns) {
  tagList(
    fluidRow(
      column(4,
        create_file_selection_column(ns("clean_files"), multiple = FALSE),
        h4("Chemical family"),
        radioButtons(ns("clean_family_source"), NULL,
          choices = c("From the EDS analysis (S, Ti, Al, Mg, Ca, Si)" = "eds",
                      "From a column of the file" = "column"), selected = "eds"),
        conditionalPanel("input.clean_family_source == 'column'", ns = ns,
          selectInput(ns("clean_family_col"), "Family / class column:", choices = NULL)),
        conditionalPanel("input.clean_family_source == 'eds'", ns = ns,
          numericInput(ns("clean_min_signal"), "Smallest inclusion signal to classify (wt%):", value = 5, min = 0, step = 1),
          helpText("N, O and C are not used (Ti-N and Cr-O peak overlaps, carbon contamination), so 'Ti-rich' covers nitrides, carbonitrides and Ti oxides."))
      ),
      column(4,
        h4("Particle columns"),
        selectInput(ns("clean_area_col"), "Area (um2):", choices = NULL),
        selectInput(ns("clean_length_col"), "Length (um):", choices = NULL),
        selectInput(ns("clean_breadth_col"), "Breadth (um):", choices = NULL),
        selectInput(ns("clean_direction_col"), "Direction of the long axis (degrees, optional):", choices = c("None" = "none")),
        selectInput(ns("clean_field_col"), "SEM field ID (optional, for the area-fraction interval):", choices = c("None" = "none"))
      ),
      column(4,
        h4("Position and inspected area"),
        create_xy_coordinate_selectors(ns("clean_x_col"), ns("clean_y_col")),
        selectInput(ns("clean_coord_unit"), "Coordinate unit:", choices = c("mm" = "mm", "um" = "um"), selected = "mm"),
        numericInput(ns("clean_inspected_area"), "Inspected area (mm2):", value = NA, min = 0, step = 0.1),
        helpText("Filled in from the range of the coordinates when a file is loaded; correct it if the scan area is known.")
      )
    ),
    fluidRow(
      column(4,
        h4("Rating standard"),
        selectInput(ns("clean_standard"), "Standard:", choices = c("ISO 4967", "ASTM E45"), selected = "ISO 4967"),
        numericInput(ns("clean_field_area"), "Rating field area (mm2):", value = 0.5, min = 0.01, step = 0.1),
        numericInput(ns("clean_min_width"), "Narrowest particle that is rated (um):", value = 2, min = 0, step = 0.5),
        numericInput(ns("clean_aspect_limit"), "Elongated from length / breadth of:", value = 3, min = 1, step = 0.5)
      ),
      column(4,
        h4("Stringers"),
        numericInput(ns("clean_axis_angle"), "Working direction (degrees from the X axis):", value = 0, min = -180, max = 180, step = 5),
        numericInput(ns("clean_max_gap"), "Largest gap along the working direction (um):", value = 40, min = 0, step = 5),
        numericInput(ns("clean_max_offset"), "Largest offset across it (um):", value = 10, min = 0, step = 1),
        helpText("Choosing a standard resets the two distances and the elongation limit to that standard's values.")
      ),
      column(4,
        h4("Indices and stereology"),
        textInput(ns("clean_size_breaks"), "Size-class limits, equivalent circle diameter (um):", value = "1, 2, 5, 10, 20"),
        numericInput(ns("clean_saltykov_classes"), "Size classes for the Saltykov unfolding:", value = 10, min = 2, max = 30, step = 1),
        numericInput(ns("clean_steel_density"), "Steel density (g/cm3):", value = 7.9, min = 1, step = 0.05),
        textInput(ns("clean_measured"), "Bulk analysis to compare with (mass ppm, optional):", value = "", placeholder = "e.g. O=25, S=10, N=120")
      )
    )
  )
}

#' Build the "Steel Cleanliness" tab's UI
#'
#' @param id Module namespace id - must match the id passed to
#'   `moduleServer()` for this tab in `server_logic.R`.
#' @return A `shiny::tabPanel()`.
#' @export
create_cleanliness_tab <- function(id) {
  ns <- NS(id)
  tabPanel("Steel Cleanliness",
    fluidRow(
      column(12,
        h3("Steel Cleanliness: Chart Rating, Stringers, Indices and Stereology"),
        helpText("Turns an automated SEM/EDS particle list into the numbers steelmakers report: a rating after ISO 4967 / ASTM E45, stringers, particles per mm2 and area fraction, and the amount of O, S and N bound in the inclusions."),

        info_box(" How it works",
          tags$ul(
            tags$li(strong("Chemical family: "), "each particle is a sulfide, aluminate, silicate or Ti-rich particle, whichever weighs most as a compound (S as MnS, Al/Mg/Ca as oxides, Si as SiO2, Ti as TiN)."),
            tags$li(strong("Stringers: "), "particles of one family lined up along the working direction, within the gap and offset set below, form one object; three or more are a stringer."),
            tags$li(strong("Type: "), "sulfides that are elongated or in a stringer are type A; stringers of compact particles type B; other elongated inclusions type C; compact single particles type D, or DS from 13 um diameter. Each type has a fine and a thick series by width."),
            tags$li(strong("Rating: "), "the inspected area is cut into 0.5 mm2 fields from the particle coordinates. Per field, the total length (A, B, C), the number of particles (D) or the largest diameter (DS) gives a chart index from 0.5 to 3. The worst field is method A, the mean over all fields method B."),
            tags$li(strong("Indices: "), "particles per mm2 with an exact Poisson interval, and area fraction in ppm with a bootstrap over SEM fields."),
            tags$li(strong("Stereology: "), "volume fraction equals area fraction (Delesse); particles per mm3 after Fullman and Schwartz-Saltykov, assuming spheres; O, S and N bound in the inclusions from the phase densities."),
            tags$li(strong("Limits: "), "an SEM run usually covers a few mm2, the standards ask for 160 to 200 mm2 and a light microscope at 100x. Read the result as an SEM-based estimate (", cite_link("ASTM E2142"), "), not as a certified rating. The index limits and widths were checked against the preview of ISO 4967:2013; the stringer distances and the ASTM E45 values were not checked against a full copy - compare them with yours before reporting."),
            tags$li("Methods: ", cite_link("ISO 4967:2013"), ", ", cite_link("ASTM E45"), ", ",
              cite_link("Fullman, Trans. AIME 197 (1953) 447"), ", ",
              cite_link("Saltykov, 1958 / Underwood, Quantitative Stereology, 1970"), ".")
          )
        ),

        cleanliness_settings_ui(ns),
        fluidRow(column(12, create_pre_filter_ui(ns, "clean"))),
        fluidRow(column(12, settings_io_ui(ns, "clean"))),
        centered_action_button_row(ns("clean_run"), "Run Cleanliness Analysis", margin_top = "10px"),

        fluidRow(
          column(12,
            verbatimTextOutput(ns("clean_status")),
            uiOutput(ns("clean_notes")),
            downloadButton(ns("clean_download_tables"), "Download all tables (xlsx)"),
            downloadButton(ns("clean_download_report"), "Download report (HTML)"),
            downloadButton(ns("clean_download_script"), "Download R script")
          )
        ),

        tabsetPanel(
          tabPanel("Chart rating",
            h4("Rating per type and width series"),
            tableOutput(ns("clean_rating_table")),
            h4("Rating per chemical family"),
            helpText("The same rating with each family on its own, e.g. to report Ti nitrides separately from oxides."),
            tableOutput(ns("clean_rating_family_table"))
          ),
          tabPanel("Stringers",
            fluidRow(
              column(8, plotOutput(ns("clean_map_plot"), height = "520px"),
                     downloadButton(ns("clean_download_map"), "Download map (PNG)")),
              column(4, plotOutput(ns("clean_rose_plot"), height = "380px"))
            ),
            h4("Objects of more than one particle"),
            tableOutput(ns("clean_stringer_table"))
          ),
          tabPanel("Indices",
            h4("Number density and area fraction"),
            tableOutput(ns("clean_indices_table")),
            h4("Particles per size class"),
            plotOutput(ns("clean_size_plot"), height = "380px"),
            tableOutput(ns("clean_size_table"))
          ),
          tabPanel("Stereology and mass balance",
            h4("Particles per volume (Fullman) and volume fraction (Delesse)"),
            tableOutput(ns("clean_stereology_table")),
            h4("Size distribution in the volume (Schwartz-Saltykov, all particles)"),
            plotOutput(ns("clean_saltykov_plot"), height = "360px"),
            tableOutput(ns("clean_saltykov_table")),
            h4("O, S and N bound in the inclusions"),
            helpText("Each family is taken as one pure compound (sulfide = MnS, aluminate = Al2O3, silicate = SiO2, Ti-rich = TiN) with its handbook density. A bound share near 100 % of the bulk analysis means the SEM run saw a representative amount of that inclusion type."),
            tableOutput(ns("clean_mass_phase_table")),
            tableOutput(ns("clean_mass_element_table"))
          )
        )
      )
    )
  )
}
