# ---- UI: "Ternary Plots" tab (split out of ui_components.R) ----
# The main workflow tab: two-dataset upload, axis/element selection,
# per-element filters, analysis methods (multivariate + statistical),
# output options, and the live preview/status area.

#' Build the "Ternary Plots" tab's UI
#'
#' @param id Module namespace id - must match the id passed to
#'   `moduleServer()` for this tab in `server_logic.R`.
#' @return A `shiny::tabPanel()`.
#' @export
create_ternary_plots_tab <- function(id) {
  ns <- NS(id)
  tabPanel("Ternary Plots",
    fluidRow(
      column(12,
        fluidRow(
          column(6, h3("Dataset 1 (Primary)")),
          column(6,
            h3("Dataset 2 (Reference)"),
            div(style = "margin-top: 10px;",
              actionButton(ns("copy_settings"), " Copy Settings from Dataset 1",
                          class = "btn-info btn-sm",
                          style = "font-size: 0.9em; padding: 5px 10px;"),
              helpText("Copy all settings from Dataset 1 to Dataset 2")
            )
          )
        ),
        fluidRow(
          column(6,
            fileInput(ns("xlsx_file1"), "Choose Primary XLSX File", accept = c(".xlsx"))
          ),
          column(6,
            fileInput(ns("xlsx_file2"), "Choose Reference XLSX File", accept = c(".xlsx"))
          )
        )
      )
    ),

    # Plot Previews
    fluidRow(
      column(12,
        hr(),
        h3("Plot Previews"),
        fluidRow(
          column(6,
            plotOutput(ns("ternary_preview1"), height = "500px")
          ),
          column(6,
            plotOutput(ns("ternary_preview2"), height = "500px")
          )
        )
      )
    ),

    # Save Plot Buttons
    fluidRow(
      column(12, style = "text-align: center; margin: 20px 0;",
        h4("Save Plots"),
        downloadButton(ns("plot1"), "Save Plot 1", class = "btn-primary btn-lg", style = "margin: 0 10px;"),
        downloadButton(ns("plot2"), "Save Plot 2", class = "btn-primary btn-lg", style = "margin: 0 10px;")
      )
    ),

    # Element Selection
    fluidRow(
      column(12,
        fluidRow(
          column(6,
            div(style = "border: 2px solid #dc3545; padding: 10px; border-radius: 5px; margin: 10px 0;",
              h4(style = "color: #dc3545; margin-top: 0;", "Element A (Required)"),
              selectInput(ns("element_A1"), "Element A (multiple allowed):", choices = NULL, multiple = TRUE),
              uiOutput(ns("dynamic_filters_A1")),
              helpText("Note: Each selected element can have its own filter condition (logical AND between elements)"),
              helpText("Example: Fe > 10, Al > 5, Si > 0 (each element gets its own threshold)")
            ),
            div(style = "border: 2px solid #dc3545; padding: 10px; border-radius: 5px; margin: 10px 0;",
              h4(style = "color: #dc3545; margin-top: 0;", "Element B (Required)"),
              selectInput(ns("element_B1"), "Element B (multiple allowed):", choices = NULL, multiple = TRUE),
              uiOutput(ns("dynamic_filters_B1"))
            ),
            div(style = "border: 2px solid #dc3545; padding: 10px; border-radius: 5px; margin: 10px 0;",
              h4(style = "color: #dc3545; margin-top: 0;", "Element C (Required)"),
              selectInput(ns("element_C1"), "Element C (multiple allowed):", choices = NULL, multiple = TRUE),
              uiOutput(ns("dynamic_filters_C1"))
            ),
            selectInput(ns("optional_param1_1"), "Optional Param 1:", choices = c("", NULL)),
            selectInput(ns("optional_param1_representation1"), "Optional Param 1 Representation:",
              choices = c("Point Size" = "point_size", "Point Type" = "point_type"),
              selected = "point_size"),
            helpText("Choose how to represent Optional Param 1: Point Size (variable size) or Point Type (different shapes)."),
            textInput(ns("filter_op1_1"), "Filter for Optional Param 1", ""),
            helpText("Enter a filter, e.g. > 0.5. Leave blank for no filter."),
            selectInput(ns("optional_param2_1"), "Optional Param 2:", choices = c("", NULL)),
            textInput(ns("filter_op2_1"), "Filter for Optional Param 2", ""),
            helpText("Enter a filter, e.g. > 0.5. Leave blank for no filter."),

            # Group selection for Dataset 1
            uiOutput(ns("group_selection_ui_1")),

            selectInput(ns("color_palette1"), "Color Palette for Optional Param 2:",
              choices = c("Blue" = "blue", "Red" = "red", "Viridis" = "viridis", "Rainbow" = "rainbow"),
              selected = "blue")
          ),
          column(6,
            div(style = "border: 2px solid #dc3545; padding: 10px; border-radius: 5px; margin: 10px 0;",
              h4(style = "color: #dc3545; margin-top: 0;", "Element A (Required)"),
              selectInput(ns("element_A2"), "Element A (multiple allowed):", choices = NULL, multiple = TRUE),
              uiOutput(ns("dynamic_filters_A2"))
            ),
            div(style = "border: 2px solid #dc3545; padding: 10px; border-radius: 5px; margin: 10px 0;",
              h4(style = "color: #dc3545; margin-top: 0;", "Element B (Required)"),
              selectInput(ns("element_B2"), "Element B (multiple allowed):", choices = NULL, multiple = TRUE),
              uiOutput(ns("dynamic_filters_B2"))
            ),
            div(style = "border: 2px solid #dc3545; padding: 10px; border-radius: 5px; margin: 10px 0;",
              h4(style = "color: #dc3545; margin-top: 0;", "Element C (Required)"),
              selectInput(ns("element_C2"), "Element C (multiple allowed):", choices = NULL, multiple = TRUE),
              uiOutput(ns("dynamic_filters_C2"))
            ),
            selectInput(ns("optional_param1_2"), "Optional Param 1:", choices = c("", NULL)),
            selectInput(ns("optional_param1_representation2"), "Optional Param 1 Representation:",
              choices = c("Point Size" = "point_size", "Point Type" = "point_type"),
              selected = "point_size"),
            helpText("Choose how to represent Optional Param 1: Point Size (variable size) or Point Type (different shapes)."),
            textInput(ns("filter_op1_2"), "Filter for Optional Param 1", ""),
            helpText("Enter a filter, e.g. > 0.5. Leave blank for no filter."),
            selectInput(ns("optional_param2_2"), "Optional Param 2:", choices = c("", NULL)),
            textInput(ns("filter_op2_2"), "Filter for Optional Param 2", ""),
            helpText("Enter a filter, e.g. > 0.5. Leave blank for no filter."),

            # Group selection for Dataset 2
            uiOutput(ns("group_selection_ui_2")),

            selectInput(ns("color_palette2"), "Color Palette for Optional Param 2:",
              choices = c("Blue" = "blue", "Red" = "red", "Viridis" = "viridis", "Rainbow" = "rainbow"),
              selected = "blue")
          )
        )
      )
    ),

    # Analysis Methods
    fluidRow(
      column(12,
        hr(),
        h3("Analysis Methods"),
        p(style = "font-size: 12px; color: #555; font-style: italic; margin-top: -8px;",
          "Only one filter (from either box below) is applied per ternary plot. Selecting a new one automatically deselects the previous choice."),
        fluidRow(
          column(4,
            div(style = "border: 2px solid #007bff; padding: 15px; border-radius: 8px; margin: 10px 0; background-color: #f8f9fa;",
              h4(style = "color: #007bff; margin-top: 0;", " Outlier Detection: Mahalanobis Distance & Isolation Forest"),

              # Universal column selector for both outlier-detection methods
              # below - Mahalanobis distance (a multivariate statistical
              # method) and Isolation Forest (a machine-learning algorithm).
              # Grouped together because both share this column selection,
              # not because both are the same kind of method.
              div(style = "margin-bottom: 15px; padding: 10px; background-color: #e3f2fd; border-radius: 5px; border-left: 4px solid #2196f3;",
                h5(style = "color: #1976d2; margin-top: 0; margin-bottom: 10px;", " Universal Column Selector (REQUIRED)"),
                p(style = "font-size: 12px; color: #d32f2f; margin-bottom: 10px; font-weight: bold;",
                  " Column selection is MANDATORY for ALL analysis methods. Select at least 2 numeric columns."),
                p(style = "font-size: 11px; color: #1976d2; margin-bottom: 10px;",
                  " This column selection is used for BOTH outlier detection (Mahalanobis/Isolation Forest) AND statistical filtering"),
                selectizeInput(ns("multivariate_columns"), "Columns for analysis:",
                  choices = NULL, multiple = TRUE,
                  options = list(placeholder = "Select at least 2 numeric columns (REQUIRED)"))
              ),

              checkboxInput(ns("use_mahalanobis"), "Use Mahalanobis Distance", value = FALSE),
              div(style = "margin-left: 20px; margin-bottom: 10px; padding: 8px; background-color: #f0f8ff; border-radius: 4px; border-left: 3px solid #007bff;",
                p(style = "font-size: 11px; margin: 0; color: #555;",
                  " Measures distance from data center using covariance structure. ",
                  "Formula: MD = sqrt[(x-u)TSigma-1(x-u)]. Automatic threshold: ",
                  cite_link("Vode et al., 2022", "https://doi.org/10.3390/ma15020684"), ".")
              ),



              checkboxInput(ns("use_isolation_forest"), "Use Isolation Forest", value = FALSE),
              div(style = "margin-left: 20px; margin-bottom: 10px; padding: 8px; background-color: #f0f8ff; border-radius: 4px; border-left: 3px solid #007bff;",
                p(style = "font-size: 11px; margin: 0; color: #555;",
                  " Machine learning approach using isolation trees. ",
                  "Measures how easily points can be isolated from the rest. ",
                  cite_link("Liu, Ting & Zhou, 2008", "https://doi.org/10.1109/ICDM.2008.17"), ".")
              ),

              # Advanced Mahalanobis parameters
              conditionalPanel(
                condition = paste0("input['", ns("use_mahalanobis"), "'] == true"),
                hr(),
                h5("Mahalanobis Parameters"),
                numericInput(ns("lambda"), "Lambda (lambda) parameter:", value = 1, min = 0, step = 0.1),
                numericInput(ns("omega"), "Omega (omega) parameter:", value = 0, min = 0, step = 0.1),
                radioButtons(ns("outlier_mode_mahalanobis"), "Outlier handling:",
                  choices = c("Keep only outliers" = TRUE, "Remove outliers" = FALSE),
                  selected = FALSE, inline = TRUE),
                radioButtons(ns("mdthresh_mode"), "Threshold mode:",
                  choices = c("Automatic" = "auto", "Manual" = "manual"),
                  selected = "auto", inline = TRUE),
                conditionalPanel(
                  condition = paste0("input['", ns("mdthresh_mode"), "'] == 'auto'"),
                  div(style = "margin-top: 10px; padding: 8px; background-color: #e8f5e8; border-radius: 4px; border-left: 3px solid #28a745;",
                    p(style = "font-size: 12px; margin: 0; color: #155724; font-weight: bold;",
                      " Automatic Threshold Formula:"),
                    p(style = "font-size: 11px; margin: 5px 0 0 0; color: #155724; font-family: monospace;",
                      "MDthresh = MDmean + sqrt(100/(100+lambda-omega)) x stdMD"),
                    p(style = "font-size: 11px; margin: 3px 0 0 0; color: #155724;",
                      cite_link("Vode et al., 2022", "https://doi.org/10.3390/ma15020684"))
                  )
                ),
                conditionalPanel(
                  condition = paste0("input['", ns("mdthresh_mode"), "'] == 'manual'"),
                  numericInput(ns("custom_mdthresh"), "Custom threshold:", value = 10, min = 0.1, step = 0.1)
                ),
                radioButtons(ns("mahalanobis_reference"), "Reference dataset:",
                  choices = c("Self-reference" = "self", "Dataset 1" = "dataset1", "Dataset 2" = "dataset2"),
                  selected = "self", inline = TRUE),
                p(style = "font-size: 12px; color: #666; font-style: italic;",
                  "Columns selected above will be used for this analysis.")
              ),


              # Advanced Isolation Forest parameters
              conditionalPanel(
                condition = paste0("input['", ns("use_isolation_forest"), "'] == true"),
                hr(),
                h5("Isolation Forest Parameters"),
                radioButtons(ns("outlier_mode_isolation"), "Outlier handling:",
                  choices = c("Keep only outliers" = TRUE, "Remove outliers" = FALSE),
                  selected = FALSE, inline = TRUE),
                fluidRow(
                  column(6, numericInput(ns("isolation_ntrees"), "Number of trees:", value = 200, min = 1, step = 1)),
                  column(6, numericInput(ns("isolation_contamination"), "Contamination:", value = 0.10, min = 0.001, max = 0.999, step = 0.01))
                ),
                checkboxInput(ns("isolation_use_all_rows"),
                  "Train each tree on all reference rows (n = row count)", value = TRUE),
                conditionalPanel(
                  condition = paste0("input['", ns("isolation_use_all_rows"), "'] == false"),
                  numericInput(ns("isolation_sample_size"), "Sub-sample size per tree:", value = 256, min = 2, step = 1),
                  div(style = "font-size: 11px; color: #555; margin: -5px 0 8px 0;",
                    "Classic Isolation Forest (", cite_link("Liu, Ting & Zhou, 2008", "https://doi.org/10.1109/ICDM.2008.17"),
                    ") sub-samples ~256 rows per tree: shallower trees, and the anomaly-score scale stays matched to the sub-sample. ",
                    "Values above the available row count are clamped down.")
                ),
                p(style = "font-size: 12px; color: #666; font-style: italic;",
                  "Columns selected above will be used for this analysis. ",
                  "\"All reference rows\" is simple and fully reproducible, but a departure from the published algorithm - untick it to sub-sample."),
                p(style = "font-size: 12px; color: #666; font-style: italic;",
                  "Reference dataset: Isolation Forest uses the same reference as Mahalanobis Distance ",
                  "(the \"Reference dataset\" control in the Mahalanobis Parameters panel; self-reference by default).")
              )
            )
          ),
          column(4,
            div(style = "border: 2px solid #28a745; padding: 15px; border-radius: 8px; margin: 10px 0; background-color: #f8f9fa;",
              h4(style = "color: #28a745; margin-top: 0;", " Statistical Filtering"),

              # One-sided by design: all three methods below (IQR, Z-score,
              # MAD) flag ONLY the upper tail - unusually HIGH values (>
              # Q3+k-IQR, z > k, > median+k-MAD). Low-side outliers are never
              # flagged. Stated once here, prominently, in addition to the
              # per-method "(high values only)" notes, since it changes how
              # results should be read (e.g. an unusually small inclusion is
              # left in the data untouched).
              div(style = "margin-bottom: 15px; padding: 10px; background-color: #d4edda; border-radius: 5px; border-left: 4px solid #28a745;",
                p(style = "font-size: 12px; margin: 0 0 8px 0; color: #155724;",
                  strong("Upper tail only. "),
                  "IQR, Z-score and MAD filtering all detect ", strong("unusually high values only"),
                  " - the upper threshold. Unusually low values are never flagged and stay in the data. ",
                  "For two-sided detection across a covariance structure, use Mahalanobis distance instead."),
                p(style = "font-size: 12px; margin: 0 0 8px 0; color: #155724;",
                  strong("Any-column rule. "),
                  "A row is removed (or kept) if it crosses the fence in ", strong("any one"),
                  " of the selected columns. Across k columns the chance of a row being flagged by noise alone is roughly 1 - (1 - p)^k, well above the single-column rate - so select as few columns as the question needs."),
                p(style = "font-size: 12px; margin: 0; color: #155724;",
                  strong("Skew. "),
                  "Inclusion measurements (wt%, ECD, area) are strongly right-skewed, and a raw upper fence sits close to the bulk - flagging part of the legitimate right tail. Tick ",
                  em("Fence on log10(value)"), " below to fit the fence on a log scale, which suits skewed positive data.")
              ),

              # Universal column selector reminder for statistical filters
              div(style = "margin-bottom: 15px; padding: 10px; background-color: #fff3cd; border-radius: 5px; border-left: 4px solid #ffc107;",
                h5(style = "color: #856404; margin-top: 0; margin-bottom: 10px;", " Universal Column Selector"),
                p(style = "font-size: 12px; color: #d32f2f; margin-bottom: 10px; font-weight: bold;",
                  " IMPORTANT: Statistical filtering uses the SAME column selection as outlier detection!"),
                p(style = "font-size: 11px; color: #856404; margin-bottom: 5px;",
                  "- Select columns in the 'Outlier Detection' section above"),
                p(style = "font-size: 11px; color: #856404; margin-bottom: 5px;",
                  "- At least 2 numeric columns are required"),
                p(style = "font-size: 11px; color: #856404; margin-bottom: 0;",
                  "- The same columns will be used for ALL filtering methods")
              ),
              checkboxInput(ns("use_iqr_filter"), "Use IQR Filtering", value = FALSE),
              div(style = "margin-left: 20px; margin-bottom: 10px; padding: 8px; background-color: #f0f9ff; border-radius: 4px; border-left: 3px solid #28a745;",
                p(style = "font-size: 11px; margin: 0; color: #555;",
                  " Uses Interquartile Range. Outliers: > Q3+1.5xIQR (high values only). ",
                  cite_link("Tukey, 1977"), ".")
              ),

              checkboxInput(ns("use_zscore_filter"), "Use Z-Score Filtering", value = FALSE),
              div(style = "margin-left: 20px; margin-bottom: 10px; padding: 8px; background-color: #f0f9ff; border-radius: 4px; border-left: 3px solid #28a745;",
                p(style = "font-size: 11px; margin: 0 0 4px 0; color: #555;",
                  " Standardized scores. Outliers: z-score > 3 (3 standard deviations above mean, high values only)"),
                p(style = "font-size: 11px; margin: 0; color: #b8860b;",
                  strong("Needs a large sample. "),
                  "The biggest z-score any point can reach in n rows is (n-1)/sqrt(n), so at the fixed threshold of 3 ",
                  strong("nothing is flagged until n is at least 11"), ", and the fit stays masked for n in the hundreds. On a small filtered subset this filter can do nothing - use IQR or MAD there.")
              ),

              checkboxInput(ns("use_mad_filter"), "Use MAD Filtering", value = FALSE),
              div(style = "margin-left: 20px; margin-bottom: 10px; padding: 8px; background-color: #f0f9ff; border-radius: 4px; border-left: 3px solid #28a745;",
                p(style = "font-size: 11px; margin: 0; color: #555;",
                  " Median Absolute Deviation. Outliers: > median+3xMAD (high values only). ",
                  cite_link("Leys et al., 2013", "https://doi.org/10.1016/j.jesp.2013.03.013"), ".")
              ),

              checkboxInput(ns("stat_filter_log10"), "Fence on log10(value) (better for right-skewed data)", value = FALSE),
              div(style = "margin-left: 20px; margin-bottom: 10px; padding: 8px; background-color: #f0f9ff; border-radius: 4px; border-left: 3px solid #28a745;",
                p(style = "font-size: 11px; margin: 0; color: #555;",
                  " Applies to whichever of IQR / Z-score / MAD is active. The fence is fitted on log10(value); non-positive values are dropped from the fit and never flagged.")
              ),

              # Advanced IQR parameters
              conditionalPanel(
                condition = paste0("input['", ns("use_iqr_filter"), "'] == true"),
                hr(),
                h5("IQR Filter Parameters"),
                radioButtons(ns("outlier_mode_iqr"), "Outlier handling:",
                  choices = c("Keep only outliers" = TRUE, "Remove outliers" = FALSE),
                  selected = FALSE, inline = TRUE)
              ),

              # Advanced Z-score parameters
              conditionalPanel(
                condition = paste0("input['", ns("use_zscore_filter"), "'] == true"),
                hr(),
                h5("Z-Score Filter Parameters"),
                radioButtons(ns("outlier_mode_zscore"), "Outlier handling:",
                  choices = c("Keep only outliers" = TRUE, "Remove outliers" = FALSE),
                  selected = FALSE, inline = TRUE)
              ),

              # Advanced MAD parameters
              conditionalPanel(
                condition = paste0("input['", ns("use_mad_filter"), "'] == true"),
                hr(),
                h5("MAD Filter Parameters"),
                radioButtons(ns("outlier_mode_mad"), "Outlier handling:",
                  choices = c("Keep only outliers" = TRUE, "Remove outliers" = FALSE),
                  selected = FALSE, inline = TRUE)
              )
            )
          ),
          column(4,
            div(style = "border: 2px solid #ffc107; padding: 15px; border-radius: 8px; margin: 10px 0; background-color: #f8f9fa;",
              h4(style = "color: #ffc107; margin-top: 0;", " Output Options"),
              selectInput(ns("output_format"), "Output Format:",
                choices = c("PNG" = "png", "JPEG" = "jpeg", "PDF" = "pdf", "TIFF" = "tiff"),
                selected = "png"),
              hr(),
              h5("Point Size Control"),
              sliderInput(ns("manual_point_size"), "Manual Point Size:",
                min = 0.1, max = 3.0, value = 1.0, step = 0.1,
                ticks = TRUE),
              checkboxInput(ns("use_manual_point_size"), "Use Manual Point Size", value = FALSE),
              helpText("Override automatic point sizing with manual control. When enabled, all points will use the same size regardless of Optional Param 1 settings."),
              checkboxInput(ns("include_plot_notes"), "Include plot notes", value = TRUE)
            )
          )
        )
      )
    ),

    # Analysis Report Section
    fluidRow(
      column(12,
        conditionalPanel(
          condition = paste0(
            "input['", ns("use_mahalanobis"), "'] == true || ",
            "input['", ns("use_isolation_forest"), "'] == true || ",
            "input['", ns("use_iqr_filter"), "'] == true || ",
            "input['", ns("use_zscore_filter"), "'] == true || ",
            "input['", ns("use_mad_filter"), "'] == true"
          ),
          hr(),
          div(style = "border: 2px solid #6c757d; padding: 15px; border-radius: 8px; margin: 10px 0; background-color: #f8f9fa;",
            h4(style = "color: #6c757d; margin-top: 0;", " Analysis Report"),
            p(style = "font-size: 12px; color: #666; margin-bottom: 15px;",
              "This report will show details about the applied filtering and analysis methods after plot generation."),
            verbatimTextOutput(ns("analysis_report"))
          )
        )
      )
    ),

    # Status and Output
    fluidRow(
      column(12,
        verbatimTextOutput(ns("status"))
      )
    )
  )
}
