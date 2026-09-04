# ---- Server: "Spatial Clustering" tab ----
# Wires R/spatial_clustering_analysis.R's pure statistics/plotting
# functions into the app. Structure mirrors server_evs.R (combined-data
# reactive, column-choice observer, eventReactive for the fit).
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

#' Wire up the Spatial Clustering tab's server logic
#'
#' Registers the observers/renderers for the "Spatial Clustering" tab: file
#' upload/combine, X/Y coordinate column auto-detection, and the
#' Clark-Evans test pipeline (`clark_evans_test()`).
#'
#' @param input The Shiny `input` object.
#' @param output The Shiny `output` object.
#' @param session The Shiny session object.
#' @param rv The app's shared `reactiveValues` object.
#' @param show_message Function to show a user-facing status message.
#' @param log_operation Function to record a structured log entry.
#' @return A list with `module_name`.
#' @export
create_server_spatial <- function(input, output, session, rv, show_message, log_operation) {

  combined_data <- reactive({
    req(input$spatial_files)
    n_files <- nrow(input$spatial_files)
    dfs <- lapply(seq_len(n_files), function(i) {
      d <- tryCatch(openxlsx::read.xlsx(input$spatial_files$datapath[i], sheet = 1), error = function(e) NULL)
      if (is.null(d)) return(NULL)
      if (n_files > 1) d$source_file <- tools::file_path_sans_ext(input$spatial_files$name[i])
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
    updateSelectInput(session, "spatial_x_col", choices = numeric_cols,
                       selected = first_match_or_null(numeric_cols, "^stage.*x"))
    updateSelectInput(session, "spatial_y_col", choices = numeric_cols,
                       selected = first_match_or_null(numeric_cols, "^stage.*y"))
    updateSelectInput(session, "spatial_color_col", choices = c("None" = "none", names(d)))
  })

  result <- eventReactive(input$spatial_analyze, {
    d <- combined_data()
    shiny::validate(shiny::need(!is.null(input$spatial_x_col) && input$spatial_x_col %in% names(d), "Select a valid X coordinate column."))
    shiny::validate(shiny::need(!is.null(input$spatial_y_col) && input$spatial_y_col %in% names(d), "Select a valid Y coordinate column."))

    x <- suppressWarnings(as.numeric(d[[input$spatial_x_col]]))
    y <- suppressWarnings(as.numeric(d[[input$spatial_y_col]]))
    valid <- is.finite(x) & is.finite(y)

    color_by <- NULL
    if (!is.null(input$spatial_color_col) && input$spatial_color_col != "none" && input$spatial_color_col %in% names(d)) {
      color_by <- d[[input$spatial_color_col]][valid]
    }

    nn_method <- if (!is.null(input$spatial_nn_method) && input$spatial_nn_method == "matrix") "matrix" else "kdtree"
    n_sim <- if (!is.null(input$spatial_n_sim) && is.finite(input$spatial_n_sim) && input$spatial_n_sim >= 10) {
      round(input$spatial_n_sim)
    } else NULL

    ce <- tryCatch(clark_evans_test(x[valid], y[valid], n_sim = n_sim, nn_method = nn_method),
                    error = function(e) { shiny::validate(paste("Error running spatial analysis:", e$message)) })
    list(ce = ce, x = x[valid], y = y[valid], color_by = color_by, color_label = input$spatial_color_col)
  })

  spatial_placeholder_msg <- "Upload data, choose X/Y coordinate columns, and click \"Analyze Spatial Pattern\"."

  output$spatial_status <- renderText({
    # Same fix as server_evs.R's output$evs_status - see that comment for
    # the full explanation. In short: a plain tryCatch(..., error =
    # function(e) NULL) catches shiny::validate()'s condition the same way
    # it catches req()'s, so every validate() failure (bad X/Y column,
    # etc.) showed this same generic placeholder instead of the specific
    # message telling the user what to fix. req()'s condition has the same
    # class but an empty message, which is how the two are told apart below.
    if (is.null(input$spatial_analyze) || input$spatial_analyze == 0) {
      return(spatial_placeholder_msg)
    }
    tryCatch({
      res <- result()
      ce <- res$ce
      sprintf("n = %d points | R = %.3f | Asymptotic p = %.4f | Monte Carlo p = %.4f (n_sim = %d, %s method)\n%s",
              ce$n, ce$R, ce$p_value_asymptotic, ce$p_value_monte_carlo, ce$n_sim, ce$nn_method, ce$verdict)
    }, shiny.silent.error = function(e) {
      if (!nzchar(conditionMessage(e))) return(spatial_placeholder_msg)
      stop(e)
    })
  })

  # geom_point()'s `size` is a fixed *physical* size (mm), not relative to
  # the plot - so a point occupies a different FRACTION of the image
  # depending on the device's physical width/height in inches, regardless
  # of pixel resolution (verified empirically: identical inches at very
  # different DPI give an identical point-to-width fraction; different
  # inches at the same DPI don't). The download is a fixed 8x7in via
  # ggsave(), so matching the *ratio* of preview width:height:res to that
  # is what keeps a point's relative size consistent between preview and
  # download.
  #
  # IMPORTANT: unlike ggsave(), renderPlot()'s width/height args are NOT
  # just a device-sizing input - Shiny sends that same number to the
  # browser as the <img> width/height attributes, i.e. they ALSO set the
  # literal on-screen CSS display size (confirmed by reading shiny's own
  # drawPlot()/resizeSavedPlot() source: `img$width <- width`, unscaled).
  # An earlier version of this fix used width=8*150=1200/height=7*150=1050
  # to get 8x7in at a "nice" 150 DPI - correct for the inches ratio, but
  # it also told the browser to display the plot at literal 1200x1050 CSS
  # pixels, wildly oversized next to the ~600px-wide column it sits in.
  # Fix: anchor on plotOutput's height (must match ui_spatial_tab.R's
  # plotOutput(..., height=) exactly), derive a width in the *same* 8:7
  # ratio as the download, and set res so width_px/res and height_px/res
  # both equal the download's inches - same physical aspect ratio, at a
  # sane on-screen pixel size. (517px, up from the original 450px, per a
  # user request to size these plots up slightly - capped by the actual
  # rendered width of its column(6) container at a typical desktop
  # viewport, ~632px measured, minus a safety margin, so the plot doesn't
  # overflow its column.)
  spatial_plot_height_px <- 517
  spatial_plot_dim <- list(
    width = round(spatial_plot_height_px * 8 / 7),
    height = spatial_plot_height_px,
    res = spatial_plot_height_px / 7
  )

  output$spatial_scatter_plot <- renderPlot({
    res <- result()
    print(create_spatial_scatter_plot(res$x, res$y, res$color_by, res$color_label))
  }, width = spatial_plot_dim$width, height = spatial_plot_dim$height, res = spatial_plot_dim$res)

  output$spatial_nnd_histogram <- renderPlot({
    res <- result()
    print(create_nnd_histogram(res$ce))
  }, width = spatial_plot_dim$width, height = spatial_plot_dim$height, res = spatial_plot_dim$res)

  output$spatial_summary_table <- renderTable({
    ce <- result()$ce
    data.frame(
      Metric = c("Points (n)", "Bounding-box area", "Density (points/area)",
                 "Observed mean NND", "Expected mean NND (Donnelly-corrected)",
                 "R statistic", "Z (asymptotic)", "p-value (asymptotic)",
                 "p-value (Monte Carlo)", "Monte Carlo simulations", "Nearest-neighbour method"),
      Value = c(sprintf("%d", ce$n), sprintf("%.4g", ce$area), sprintf("%.6g", ce$density),
                sprintf("%.4f", ce$Dobs), sprintf("%.4f", ce$Dkevin),
                sprintf("%.4f", ce$R), sprintf("%.3f", ce$Z), sprintf("%.4f", ce$p_value_asymptotic),
                sprintf("%.4f", ce$p_value_monte_carlo), sprintf("%d", ce$n_sim), ce$nn_method)
    )
  })

  # All three download handlers below call result() - an eventReactive
  # gated on input$spatial_analyze - with no server-side gating on the
  # buttons themselves (see ui_spatial_tab.R: none of the 3
  # downloadButtons sit inside a conditionalPanel, so all are clickable
  # before "Analyze Spatial Pattern" is ever pressed). Before that click,
  # result() throws a shiny::validate()/req() condition whose $message is
  # always "" by design - the same blank-error gap found and fixed in
  # server_plot_builder.R's output$builder_download (see the vidternary
  # Structural Audit's §03 for that writeup); confirmed reachable here the
  # same way, via direct testServer() reproduction against the unmodified
  # handler. safe_result() gives every handler below a clear, actionable
  # message for that case, while still surfacing a genuine error's own
  # text.
  safe_result <- function() {
    tryCatch(result(), error = function(e) {
      if (nzchar(e$message)) {
        stop("Could not generate this download: ", e$message)
      }
      stop("Upload data, choose X/Y coordinate columns, and click \"Analyze Spatial Pattern\" before downloading.")
    })
  }

  output$spatial_download_scatter <- downloadHandler(
    filename = function() paste0("spatial_scatter_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      res <- safe_result()
      ggplot2::ggsave(file, plot = create_spatial_scatter_plot(res$x, res$y, res$color_by, res$color_label), width = 8, height = 7, dpi = 300)
    }
  )

  output$spatial_download_histogram <- downloadHandler(
    filename = function() paste0("spatial_nnd_histogram_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      ggplot2::ggsave(file, plot = create_nnd_histogram(safe_result()$ce), width = 8, height = 7, dpi = 300)
    }
  )

  output$spatial_download_table <- downloadHandler(
    filename = function() paste0("spatial_nnd_values_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"),
    content = function(file) {
      res <- safe_result()
      df <- data.frame(x = res$x, y = res$y, nearest_neighbor_distance = res$ce$nnd)
      writexl::write_xlsx(df, file)
    }
  )

  return(list(
    module_name = "server_spatial"
  ))
}
