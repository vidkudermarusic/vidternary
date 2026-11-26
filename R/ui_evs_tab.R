# ---- UI: "Extreme Value Analysis" tab ----
# Murakami / ASTM E2283 extreme value statistics for inclusion rating:
# return level and largest-inclusion interval for a larger inspection area
# from the distribution of per-field (or per-group) block maxima. See
# R/extreme_value_analysis.R for the statistics/plotting and
# R/server_evs.R for the Shiny wiring.

#' Build the "Extreme Value Analysis" tab's UI
#'
#' @param id Module namespace id - must match the id passed to
#'   `moduleServer()` for this tab in `server_logic.R`.
#' @return A `shiny::tabPanel()`.
#' @export
create_evs_tab <- function(id) {
  ns <- NS(id)
  tabPanel("Extreme Value Analysis",
    fluidRow(
      column(12,
        h3("Extreme Value Statistics (Murakami / ASTM E2283)"),
        helpText("Estimates the size of inclusions expected over a larger inspection area, from the distribution of per-field maximum inclusion sizes (Gumbel probability plot)."),

        info_box(" How it works",
          tags$ul(
            tags$li("The inspected area is split into equal 'control areas' of one or more whole SEM fields-of-view each, identified by a field/frame ID column in your data."),
            tags$li("The largest inclusion (by sqrtArea) in each control area is its block maximum."),
            tags$li("Block maxima are fit to a Gumbel probability plot; the fitted line gives the return level for T control areas - the size exceeded on average once per T control areas."),
            tags$li("The largest inclusion actually found in T control areas is random. It lies above the return level in about 63% of cases, so it is compared with a separate 95% interval for the largest inclusion over T control areas. Both intervals come from a parametric bootstrap, because ordinary regression intervals are far too narrow for ordered block maxima."),
            tags$li(strong("A real field/frame ID column is required."),
              " Each distinct value of that column is one field of known physical size, so with 1 field per control area T = 100 means a real area 100x one field. ",
              "The method has no valid meaning without genuine per-field grouping - splitting a flat row list into N arbitrary chunks is not a set of control areas (it depends on sort order and forces equal inclusion counts rather than equal areas), so that fallback has been removed."),
            tags$li("Method: ", cite_link("Murakami, 1994", "https://doi.org/10.6028/jres.099.032"),
              ", standardized in ", cite_link("ASTM E2283-08(2019)"), ". ",
              "Goodness-of-fit is tested with the Anderson-Darling statistic (",
              cite_link("Anderson & Darling, 1952", "https://doi.org/10.1214/aoms/1177729437"), "; ",
              cite_link("Stephens, 1977", "https://doi.org/10.1093/biomet/64.3.583"), ")")
          )
        ),

        fluidRow(
          column(6,
            create_file_selection_column(ns("evs_files"), multiple = FALSE)
          ),
          column(6,
            h4("Control Area Grouping"),
            selectInput(ns("evs_area_col"), "Area column (um2):", choices = NULL),
            selectInput(ns("evs_group_col"), "Field / frame ID column (required):", choices = NULL),
            div(style = "color: #856404; background-color: #fff3cd; border: 1px solid #ffeeba; border-radius: 4px; padding: 8px; font-size: 12px;",
              strong("Required: "), "pick the column that identifies which SEM field / frame each inclusion came from. ",
              "Each distinct value is one field. If your export has no such column, EVS cannot be run on it - the method needs genuine per-field grouping, not an arbitrary split of the row list."
            ),
            numericInput(ns("evs_fields_per_area"), "Fields per control area:", value = 1, min = 1, step = 1),
            helpText("Merges every k consecutive fields into one control area, without overlap: with k = 16, fields 1 to 16 form control area 1, fields 17 to 32 control area 2, and so on. ",
              "Whole-number IDs are taken as consecutively numbered, so a missing number counts as a field with nothing detected. ",
              "Increase it when most single fields contain only tiny particles near the detection limit: each control area should contain at least one real inclusion, otherwise the Gumbel plot bends and the prediction falls short. ",
              "Fields in an incomplete last control area are left out so all control areas have the same area.")
          )
        ),

        fluidRow(
          column(12,
            create_pre_filter_ui(ns, "evs"),
            div(style = "color: #856404; background-color: #fff3cd; border: 1px solid #ffeeba; border-radius: 4px; padding: 8px; font-size: 12px; margin-top: -5px;",
              strong("Caution when filtering by Area: "),
              "this method needs each control area's TRUE largest inclusion, whatever its size. ",
              "Filtering on Area itself (e.g. \"Area > 1\" to drop small inclusions) can silently distort the fit: ",
              "a control area whose real maximum happens to fall below your threshold either loses that ",
              "maximum to a smaller surviving inclusion, or is dropped from the analysis entirely if every ",
              "inclusion in it is filtered out - biasing the fitted Gumbel line and the predicted largest ",
              "inclusion. Filtering on a variable unrelated to size (e.g. a chemistry/inclusion-type column) ",
              "does not have this problem. If you do filter by Area, check the Fit Summary's control-area ",
              "count against your unfiltered data to see whether any control areas were lost."
            )
          )
        ),

        fluidRow(column(12, settings_io_ui(ns, "evs"))),
        centered_action_button_row(ns("evs_fit"), "Fit Extreme Value Model", margin_top = "10px"),

        fluidRow(
          column(12,
            verbatimTextOutput(ns("evs_status")),
            uiOutput(ns("evs_area_warning")),
            uiOutput(ns("evs_gof_warning"))
          )
        ),

        fluidRow(
          column(4,
            h4("Prediction"),
            numericInput(ns("evs_return_period"), "Return period T (multiples of the control area):", value = 100, min = 1.01, step = 1),
            helpText("E.g. T = 100 gives the return level exceeded on average once per 100 control areas, and an interval for the largest inclusion expected over 100x one control area."),
            uiOutput(ns("evs_return_period_note")),
            downloadButton(ns("evs_download_plot"), "Download plot (PNG)"),
            br(), br(),
            downloadButton(ns("evs_download_table"), "Download block maxima (xlsx)"),
            br(), br(),
            downloadButton(ns("evs_download_report"), "Download report (HTML)"),
            br(), br(),
            downloadButton(ns("evs_download_script"), "Download R script")
          ),
          column(8,
            # width="821px" matches server_evs.R's derived width (575px
            # height * 10:7 download aspect ratio).
            plotOutput(ns("evs_plot"), width = "821px", height = "575px")
          )
        ),

        fluidRow(
          column(12,
            h4("Fit Summary"),
            tableOutput(ns("evs_summary_table"))
          )
        ),

        fluidRow(
          column(12,
            h4("Fatigue Limit Estimate (Murakami sqrt(area) model)"),
            helpText("Turns the predicted largest inclusion into a fatigue limit: sigma_w = C (HV + 120) / sqrtArea^(1/6), with C = 1.43 for an inclusion at the surface, 1.41 just below it and 1.56 inside the part (",
              cite_link("Murakami & Endo, 1994", "https://doi.org/10.1016/0142-1123(94)90001-9"),
              "). The model is stated as accurate to about 10 % for HV 70 to 720. It gives the stress amplitude below which a crack starting at that inclusion does not grow; it says nothing about failures that start elsewhere."),
            fluidRow(
              column(3, numericInput(ns("evs_hv"), "Vickers hardness HV of the steel:", value = NA, min = 1, step = 10)),
              column(3, selectInput(ns("evs_incl_location"), "Inclusion location:",
                choices = c("At the surface" = "surface", "Just below the surface" = "subsurface", "Inside the part" = "internal"))),
              column(3, numericInput(ns("evs_stress_ratio"), "Stress ratio R (min / max stress):", value = -1, max = 0.99, step = 0.1))
            ),
            tableOutput(ns("evs_fatigue_table")),
            uiOutput(ns("evs_fatigue_notes")),
            h5("Return period for a stressed volume"),
            helpText("The fatigue limit applies to a part only if T matches its stressed volume. Murakami's rule: one control area S0 stands for the volume V0 = S0 x h, with h the mean block maximum, and T = V / V0."),
            fluidRow(
              column(3, numericInput(ns("evs_control_area_mm2"), "Area of one control area S0 (mm2):", value = NA, min = 0, step = 0.01)),
              column(3, numericInput(ns("evs_volume_mm3"), "Stressed volume V (mm3):", value = NA, min = 0, step = 10))
            ),
            textOutput(ns("evs_volume_note"))
          )
        )
      )
    )
  )
}
