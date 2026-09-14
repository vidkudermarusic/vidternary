# ---- Server: "Extreme Value Analysis" tab ----
# Wires R/extreme_value_analysis.R's pure statistics/plotting functions
# into the app. Combined-data reactive mirrors server_plot_builder.R's
# multi-file read/combine; column-choice observer mirrors the pattern in
# server_plot_types.R.
#
# NOTE: shiny::validate()/need() must stay fully qualified here - see
# server_spatial.R header comment for why (jsonlite also exports
# validate(), and it wins by library() attach order).

#' Wire up the Extreme Value Analysis tab's server logic
#'
#' Registers the observers/renderers for the "Extreme Value Analysis" tab:
#' file upload/combine, area/group column auto-detection, and the
#' Murakami/Gumbel fit pipeline (`compute_block_maxima()`/
#' `fit_evs_gumbel()`/`gumbel_goodness_of_fit()`/`predict_evs_max()`).
#'
#' @param input The Shiny `input` object.
#' @param output The Shiny `output` object.
#' @param session The Shiny session object.
#' @param rv The app's shared `reactiveValues` object.
#' @param show_message Function to show a user-facing status message.
#' @param log_operation Function to record a structured log entry.
#' @return A list with `module_name`.
#' @export
create_server_evs <- function(input, output, session, rv, show_message, log_operation) {

  combined_data <- make_combined_upload_reactive(input, "evs_files")

  observe({
    d <- tryCatch(combined_data(), error = function(e) NULL)
    if (is.null(d)) return()
    numeric_cols <- names(d)[sapply(d, is.numeric)]
    updateSelectInput(session, "evs_area_col", choices = numeric_cols,
                       selected = first_match_or_null(numeric_cols, "^area"))
    updateSelectInput(session, "evs_group_col", choices = names(d),
                       selected = first_match_or_null(names(d), "field|frame|fov|f\\.o\\.v"))
    updateSelectizeInput(session, "evs_filter_cols", choices = numeric_cols)
  })

  # Pre-Analysis Data Filter (helpers_pre_analysis_filter.R, shared with
  # Spatial Clustering and Point Pattern Analysis) - lets rows be excluded
  # (e.g. "Area > 1", to drop inclusions too small to be worth analyzing)
  # before block maxima are ever computed, rather than only being able to
  # filter downstream of an already-fitted result.
  output$evs_filter_inputs <- renderUI({
    render_pre_filter_inputs(session$ns, "evs", input$evs_filter_cols)
  })

  filtered_data <- reactive({
    d <- combined_data()
    filters <- collect_pre_filters(input, "evs", input$evs_filter_cols)
    tryCatch(apply_pre_filters(d, filters), error = function(e) {
      shiny::validate(paste("Pre-analysis filter error:", e$message))
    })
  })

  fit_result <- eventReactive(input$evs_fit, {
    d <- filtered_data()
    n_before_filter <- nrow(combined_data())
    n_after_filter <- nrow(d)
    shiny::validate(shiny::need(nrow(d) > 0, "No rows remain after applying the pre-analysis filter(s) - loosen or remove them and try again."))
    req(input$evs_area_col)
    shiny::validate(shiny::need(input$evs_area_col %in% names(d), "Select a valid area column."))

    # A genuine per-field / per-frame ID column is mandatory: ASTM control
    # areas are defined by equal inspected AREA, not equal inclusion COUNTS
    # (a field with more inclusions legitimately has a larger block
    # maximum), so grouping by row position is not valid. With no valid
    # grouping there is no valid EVS fit, so this hard-stops with an
    # actionable message.
    shiny::validate(shiny::need(!is.null(input$evs_group_col) && input$evs_group_col %in% names(d),
                                 "Select the field / frame ID column that identifies which SEM field each inclusion came from. EVS needs genuine per-field grouping and cannot run without it."))
    group_col <- input$evs_group_col

    block_maxima <- tryCatch(compute_block_maxima(d, input$evs_area_col, group_col),
                              error = function(e) { shiny::validate(paste("Error computing block maxima:", e$message)) })
    shiny::validate(shiny::need(nrow(block_maxima) >= 3, "At least 3 control-area groups with valid data are required."))

    fit <- tryCatch(fit_evs_gumbel(block_maxima$sqrt_area_max),
                     error = function(e) { shiny::validate(paste("Error fitting EVS model:", e$message)) })
    fit$block_maxima <- block_maxima
    fit$gof <- tryCatch(gumbel_goodness_of_fit(fit), error = function(e) NULL)
    fit$n_rows_before_filter <- n_before_filter
    fit$n_rows_after_filter <- n_after_filter
    fit
  })

  prediction <- reactive({
    req(fit_result())
    tryCatch(predict_evs_max(fit_result(), input$evs_return_period), error = function(e) NULL)
  })

  evs_placeholder_msg <- "Upload data, choose the area and grouping columns, and click \"Fit Extreme Value Model\"."

  output$evs_status <- renderText({
    # Deliberately not a plain tryCatch(..., error = function(e) NULL): a
    # shiny::validate() failure throws a "shiny.silent.error"/"validation"
    # condition that Shiny's own output-rendering machinery is specially
    # built to catch and display as a distinct "please fix this input"
    # message - but only if this render function doesn't itself catch and
    # discard the error first.
    #
    # The "not yet fitted" case is handled up front by checking whether
    # the button has been clicked, rather than by catching fit_result()'s
    # error, but that alone isn't quite enough: the button has no
    # server-side gating (see ui_evs_tab.R - it's always clickable), so a
    # user can click "Fit" before uploading a file or choosing an area
    # column. That path fails via req() rather than validate() - req()
    # throws this exact same condition class, by design, as a *silent*
    # stop with an EMPTY message (there's nothing to tell the user beyond
    # "you're not ready yet"). render_gated_status() (R/helpers.R) tells
    # the two apart: an empty message falls back to the placeholder, while
    # a real validate() message is re-thrown unchanged so Shiny's own
    # machinery still gets to display it (preserving its distinct
    # validation-error styling). A genuine (non-validation) error is a
    # different condition class entirely and is untouched by that
    # handling, so it still propagates and displays as a real error.
    render_gated_status(input, "evs_fit", evs_placeholder_msg, function() {
      fit <- fit_result()
      base_msg <- sprintf("Fit successful: n = %d control areas, R2 = %.3f, intercept a = %.3f, slope b = %.3f",
                           fit$n, fit$r_squared, fit$intercept, fit$slope)
      if (is.null(fit$gof)) return(base_msg)
      gof_msg <- if (fit$gof$reject_at_05) {
        sprintf("Goodness-of-fit: Anderson-Darling A2 = %.3f, p %s -> data DEVIATE from a single Gumbel distribution (see note below).",
                fit$gof$statistic, fit$gof$p_value_bracket)
      } else {
        sprintf("Goodness-of-fit: Anderson-Darling A2 = %.3f, p %s -> no evidence against a single Gumbel distribution.",
                fit$gof$statistic, fit$gof$p_value_bracket)
      }
      paste(base_msg, gof_msg, sep = "\n")
    })
  })

  output$evs_gof_warning <- renderUI({
    # Unlike evs_status above, swallowing the error here to NULL is correct,
    # not a copy of that bug: this output's only job is to conditionally
    # show a supplementary warning banner on top of a successful fit whose
    # goodness-of-fit test rejected Gumbel. When there's no such fit - no
    # click yet, a validate() failure, or a genuine error - the right
    # result is no banner at all, which returning NULL already gives; the
    # validate() message itself is still surfaced to the user via
    # evs_status, so nothing is lost by staying silent here.
    fit <- tryCatch(fit_result(), error = function(e) NULL)
    if (is.null(fit) || is.null(fit$gof) || !fit$gof$reject_at_05) return(NULL)
    div(style = "border: 1px solid #dc3545; padding: 12px; border-radius: 5px; margin: 10px 0; background-color: #f8d7da; color: #721c24;",
      strong(" Goodness-of-fit test (Anderson-Darling) rejects a single Gumbel distribution at the 5% level. "),
      "The block maxima likely come from more than one population (e.g. a mix of inclusion types with different size distributions), or one control area is an outlier. ",
      "The straight-line fit and its extrapolation may understate the true tail - consider filtering/stratifying the data by inclusion type before fitting, or inspecting individual control areas for outliers."
    )
  })

  # geom_point()'s `size` is a fixed physical size (mm), not relative to
  # the plot - matching the preview device's aspect ratio/inches to the
  # download's 10x7in avoids a preview/download point-size mismatch. See
  # server_spatial.R for the full explanation, including why
  # renderPlot()'s width/height must stay close to plotOutput's actual
  # on-screen size (they also set the browser's literal display size, not
  # just the internal device resolution) - height must match
  # ui_evs_tab.R's plotOutput(..., height=) exactly; width is derived
  # from the same 10:7 ratio as the download. (575px, capped by the actual
  # rendered width of its column(8) container at a typical desktop
  # viewport, ~843px measured, minus a safety margin.)
  evs_plot_height_px <- 575
  output$evs_plot <- renderPlot({
    fit <- fit_result()
    print(create_gumbel_plot(fit, prediction()))
  }, width = round(evs_plot_height_px * 10 / 7), height = evs_plot_height_px, res = evs_plot_height_px / 7)

  output$evs_summary_table <- renderTable({
    fit <- fit_result()
    pred <- prediction()
    df <- data.frame(
      Metric = c("Rows before pre-analysis filter", "Rows after pre-analysis filter",
                 "Control areas (n)", "Intercept (a)", "Slope (b)", "R2"),
      Value = c(sprintf("%d", fit$n_rows_before_filter), sprintf("%d", fit$n_rows_after_filter),
                sprintf("%d", fit$n), sprintf("%.4f", fit$intercept), sprintf("%.4f", fit$slope), sprintf("%.4f", fit$r_squared))
    )
    if (!is.null(fit$gof)) {
      df <- rbind(df, data.frame(
        Metric = c("Anderson-Darling A2", "Goodness-of-fit (p-value)", "Rejects Gumbel at 5%?"),
        Value = c(sprintf("%.4f", fit$gof$statistic), fit$gof$p_value_bracket, if (fit$gof$reject_at_05) "Yes" else "No")
      ))
    }
    if (!is.null(pred)) {
      df <- rbind(df, data.frame(
        Metric = c("Return period T", "Predicted sqrtArea (um)",
                   "95% prediction interval (single future max)",
                   "95% confidence interval (on the estimate, ASTM-style)",
                   "Std. error of the estimate"),
        Value = c(sprintf("%.0f", pred$return_period), sprintf("%.2f", pred$predicted),
                  sprintf("[%.2f, %.2f]", pred$lower, pred$upper),
                  sprintf("[%.2f, %.2f]", pred$ci_lower, pred$ci_upper),
                  sprintf("%.3f", pred$se_fit))
      ))
    }
    df
  })

  # Both download handlers below call fit_result() - an eventReactive gated
  # on input$evs_fit - with no server-side gating on the buttons themselves
  # (see ui_evs_tab.R: neither downloadButton sits inside a
  # conditionalPanel, so both are clickable before "Fit Extreme Value
  # Model" is ever pressed). Before that click, fit_result() throws a
  # shiny::validate()/req() condition whose $message is always "" by
  # design. safe_reactive_result() (R/helpers.R) gives both handlers below
  # a clear, actionable message for that case, while still surfacing a
  # genuine error's own text.
  evs_download_placeholder_msg <- "Upload data, choose the area and grouping columns, and click \"Fit Extreme Value Model\" before downloading."

  output$evs_download_plot <- downloadHandler(
    filename = function() paste0("evs_gumbel_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      fit <- safe_reactive_result(function() fit_result(), evs_download_placeholder_msg)
      ggplot2::ggsave(file, plot = create_gumbel_plot(fit, prediction()), width = 10, height = 7, dpi = 300)
    }
  )

  output$evs_download_table <- downloadHandler(
    filename = function() paste0("evs_block_maxima_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"),
    content = function(file) {
      writexl::write_xlsx(safe_reactive_result(function() fit_result(), evs_download_placeholder_msg)$block_maxima, file)
    }
  )

  return(list(
    module_name = "server_evs"
  ))
}
