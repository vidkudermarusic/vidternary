# ---- Server: "Extreme Value Analysis" tab ----
# Wires R/extreme_value_analysis.R's pure statistics/plotting functions
# into the app. Combined-data reactive mirrors server_plot_builder.R's
# multi-file read/combine; column-choice observer mirrors the pattern in
# server_plot_types.R.
#
# NOTE: shiny::validate()/shiny::need() must be fully qualified in this
# package - jsonlite also exports its own validate() (a JSON schema
# validator), and this package attaches its dependencies at app-launch
# time via dependencies.R's initialize_packages() (a sequence of
# library() calls), not via NAMESPACE import()/importFrom() - so an
# unqualified validate()/need() resolves to whichever of shiny/jsonlite
# was library()'d most recently (attach order), not necessarily shiny.
# jsonlite is library()'d after shiny there, so it wins - confirmed
# empirically while building the Plot Builder tab. (An earlier version of
# this comment blamed a NAMESPACE `import(jsonlite)` for the masking -
# NAMESPACE has no blanket import() at all; this is the real mechanism.)

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

  combined_data <- reactive({
    req(input$evs_files)
    n_files <- nrow(input$evs_files)
    dfs <- lapply(seq_len(n_files), function(i) {
      d <- tryCatch(openxlsx::read.xlsx(input$evs_files$datapath[i], sheet = 1), error = function(e) NULL)
      if (is.null(d)) return(NULL)
      if (n_files > 1) d$source_file <- tools::file_path_sans_ext(input$evs_files$name[i])
      d
    })
    dfs <- Filter(Negate(is.null), dfs)
    shiny::validate(shiny::need(length(dfs) > 0, "None of the selected files could be read."))
    if (length(dfs) == 1) return(dfs[[1]])
    common_cols <- Reduce(intersect, lapply(dfs, names))
    shiny::validate(shiny::need(length(common_cols) > 0, "The selected files have no columns in common."))
    do.call(rbind, lapply(dfs, function(d) d[, common_cols, drop = FALSE]))
  })

  first_match_or_null <- function(x, pattern) {
    hit <- x[grepl(pattern, x, ignore.case = TRUE)]
    if (length(hit) == 0) NULL else hit[1]
  }

  observe({
    d <- tryCatch(combined_data(), error = function(e) NULL)
    if (is.null(d)) return()
    numeric_cols <- names(d)[sapply(d, is.numeric)]
    updateSelectInput(session, "evs_area_col", choices = numeric_cols,
                       selected = first_match_or_null(numeric_cols, "^area"))
    updateSelectInput(session, "evs_group_col", choices = names(d),
                       selected = first_match_or_null(names(d), "^field$"))
  })

  fit_result <- eventReactive(input$evs_fit, {
    d <- combined_data()
    req(input$evs_area_col)
    shiny::validate(shiny::need(input$evs_area_col %in% names(d), "Select a valid area column."))

    if (isTRUE(input$evs_use_manual_groups)) {
      n_groups <- input$evs_n_groups
      # Upper-bounded at nrow(d)/2 so every group averages at least 2 rows -
      # a "block maximum" is meaningless as the max of a single measurement,
      # and undermines the method's statistical premise (see ui_evs_tab.R's
      # own note that this fallback is for trend estimation only). Without
      # this, a value close to nrow(d) silently created singleton
      # pseudo-groups with no warning.
      max_groups <- max(3, floor(nrow(d) / 2))
      shiny::validate(shiny::need(
        is.finite(n_groups) && n_groups >= 3 && n_groups <= max_groups,
        sprintf("Number of groups must be between 3 and %d (at least 2 rows per group, on average, for %d total rows).", max_groups, nrow(d))
      ))
      d$.evs_group <- cut(seq_len(nrow(d)), breaks = n_groups, labels = FALSE)
      group_col <- ".evs_group"
    } else {
      shiny::validate(shiny::need(!is.null(input$evs_group_col) && input$evs_group_col %in% names(d),
                                   "Select a field/group ID column, or enable manual grouping."))
      group_col <- input$evs_group_col
    }

    block_maxima <- tryCatch(compute_block_maxima(d, input$evs_area_col, group_col),
                              error = function(e) { shiny::validate(paste("Error computing block maxima:", e$message)) })
    shiny::validate(shiny::need(nrow(block_maxima) >= 3, "At least 3 control-area groups with valid data are required."))

    fit <- tryCatch(fit_evs_gumbel(block_maxima$sqrt_area_max),
                     error = function(e) { shiny::validate(paste("Error fitting EVS model:", e$message)) })
    fit$block_maxima <- block_maxima
    fit$gof <- tryCatch(gumbel_goodness_of_fit(fit), error = function(e) NULL)
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
    # discard the error first. The old code caught every error the same
    # way (as NULL), so every validate() failure - wrong area column,
    # out-of-range evs_n_groups, etc. - showed this same generic "Upload
    # data..." text instead of the specific message telling the user what
    # to fix.
    #
    # The "not yet fitted" case is handled up front by checking whether
    # the button has been clicked, rather than by catching fit_result()'s
    # error, but that alone isn't quite enough: the button has no
    # server-side gating (see ui_evs_tab.R - it's always clickable), so a
    # user can click "Fit" before uploading a file or choosing an area
    # column. That path fails via req() rather than validate() - req()
    # throws this exact same condition class, by design, as a *silent*
    # stop with an EMPTY message (there's nothing to tell the user beyond
    # "you're not ready yet"). tryCatch()ing specifically for that class
    # lets us tell the two apart: an empty message falls back to the
    # placeholder, while a real validate() message is re-thrown unchanged
    # so Shiny's own machinery still gets to display it (preserving its
    # distinct validation-error styling) - only its handling for that one
    # empty-message case is short-circuited. A genuine (non-validation)
    # error is a different condition class entirely and is untouched by
    # this handler, so it still propagates and displays as a real error.
    if (is.null(input$evs_fit) || input$evs_fit == 0) {
      return(evs_placeholder_msg)
    }
    tryCatch({
      fit <- fit_result()
      base_msg <- sprintf("Fit successful: n = %d control areas, R² = %.3f, intercept a = %.3f, slope b = %.3f",
                           fit$n, fit$r_squared, fit$intercept, fit$slope)
      if (is.null(fit$gof)) return(base_msg)
      gof_msg <- if (fit$gof$reject_at_05) {
        sprintf("Goodness-of-fit: Anderson-Darling A² = %.3f, p %s -> data DEVIATE from a single Gumbel distribution (see note below).",
                fit$gof$statistic, fit$gof$p_value_bracket)
      } else {
        sprintf("Goodness-of-fit: Anderson-Darling A² = %.3f, p %s -> no evidence against a single Gumbel distribution.",
                fit$gof$statistic, fit$gof$p_value_bracket)
      }
      paste(base_msg, gof_msg, sep = "\n")
    }, shiny.silent.error = function(e) {
      if (!nzchar(conditionMessage(e))) return(evs_placeholder_msg)
      stop(e)
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
      strong("⚠ Goodness-of-fit test (Anderson-Darling) rejects a single Gumbel distribution at the 5% level. "),
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
  # from the same 10:7 ratio as the download. (575px, up from the
  # original 500px, per a user request to size these plots up slightly -
  # capped by the actual rendered width of its column(8) container at a
  # typical desktop viewport, ~843px measured, minus a safety margin.)
  evs_plot_height_px <- 575
  output$evs_plot <- renderPlot({
    fit <- fit_result()
    print(create_gumbel_plot(fit, prediction()))
  }, width = round(evs_plot_height_px * 10 / 7), height = evs_plot_height_px, res = evs_plot_height_px / 7)

  output$evs_summary_table <- renderTable({
    fit <- fit_result()
    pred <- prediction()
    df <- data.frame(
      Metric = c("Control areas (n)", "Intercept (a)", "Slope (b)", "R²"),
      Value = c(sprintf("%d", fit$n), sprintf("%.4f", fit$intercept), sprintf("%.4f", fit$slope), sprintf("%.4f", fit$r_squared))
    )
    if (!is.null(fit$gof)) {
      df <- rbind(df, data.frame(
        Metric = c("Anderson-Darling A²", "Goodness-of-fit (p-value)", "Rejects Gumbel at 5%?"),
        Value = c(sprintf("%.4f", fit$gof$statistic), fit$gof$p_value_bracket, if (fit$gof$reject_at_05) "Yes" else "No")
      ))
    }
    if (!is.null(pred)) {
      df <- rbind(df, data.frame(
        Metric = c("Return period T", "Predicted √Area (µm)", "95% prediction interval"),
        Value = c(sprintf("%.0f", pred$return_period), sprintf("%.2f", pred$predicted),
                  sprintf("[%.2f, %.2f]", pred$lower, pred$upper))
      ))
    }
    df
  })

  output$evs_download_plot <- downloadHandler(
    filename = function() paste0("evs_gumbel_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      ggplot2::ggsave(file, plot = create_gumbel_plot(fit_result(), prediction()), width = 10, height = 7, dpi = 300)
    }
  )

  output$evs_download_table <- downloadHandler(
    filename = function() paste0("evs_block_maxima_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"),
    content = function(file) {
      writexl::write_xlsx(fit_result()$block_maxima, file)
    }
  )

  return(list(
    module_name = "server_evs"
  ))
}
