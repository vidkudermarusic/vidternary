# ---- Server: "Point Pattern Analysis" tab ----
# Wires R/spatial_point_pattern_analysis.R's pure statistics/plotting
# functions into the app. Structure mirrors server_spatial.R (combined-data
# reactive, column-choice observer, eventReactive for the fit) - the same
# established pattern used by every "upload -> pick columns -> analyze"
# tab in this app.
#
# NOTE: shiny::validate()/shiny::need() must be fully qualified in this
# package - see server_spatial.R's own header comment for why (jsonlite's
# own validate() masking shiny's, confirmed empirically).

#' Wire up the Point Pattern Analysis tab's server logic
#'
#' Registers the observers/renderers for the "Point Pattern Analysis" tab:
#' file upload/combine, X/Y coordinate column auto-detection, and the
#' K/L/G-function + CSR envelope + kernel intensity pipeline
#' (`build_point_pattern()` and friends in spatial_point_pattern_analysis.R).
#'
#' @param input The Shiny `input` object.
#' @param output The Shiny `output` object.
#' @param session The Shiny session object.
#' @param rv The app's shared `reactiveValues` object.
#' @param show_message Function to show a user-facing status message.
#' @param log_operation Function to record a structured log entry.
#' @return A list with `module_name`.
#' @export
create_server_spatial_ppp <- function(input, output, session, rv, show_message, log_operation) {

  combined_data <- reactive({
    req(input$ppp_files)
    n_files <- nrow(input$ppp_files)
    dfs <- lapply(seq_len(n_files), function(i) {
      d <- tryCatch(openxlsx::read.xlsx(input$ppp_files$datapath[i], sheet = 1), error = function(e) NULL)
      if (is.null(d)) return(NULL)
      if (n_files > 1) d$source_file <- tools::file_path_sans_ext(input$ppp_files$name[i])
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
    updateSelectInput(session, "ppp_x_col", choices = numeric_cols,
                       selected = first_match_or_null(numeric_cols, "^stage.*x"))
    updateSelectInput(session, "ppp_y_col", choices = numeric_cols,
                       selected = first_match_or_null(numeric_cols, "^stage.*y"))
    updateSelectInput(session, "ppp_mark_col", choices = c("None" = "none", names(d)))
    updateSelectizeInput(session, "ppp_filter_cols", choices = numeric_cols)
  })

  # Pre-Analysis Data Filter (helpers_pre_analysis_filter.R, shared with EVS
  # and Spatial Clustering) - lets rows be excluded (e.g. "Area > 1") before
  # the point pattern is ever built, rather than only being able to filter
  # downstream of an already-computed result.
  output$ppp_filter_inputs <- renderUI({
    render_pre_filter_inputs(session$ns, "ppp", input$ppp_filter_cols)
  })

  filtered_data <- reactive({
    d <- combined_data()
    filters <- collect_pre_filters(input, "ppp", input$ppp_filter_cols)
    tryCatch(apply_pre_filters(d, filters), error = function(e) {
      shiny::validate(paste("Pre-analysis filter error:", e$message))
    })
  })

  result <- eventReactive(input$ppp_analyze, {
    d <- filtered_data()
    shiny::validate(shiny::need(nrow(d) > 0, "No rows remain after applying the pre-analysis filter(s) - loosen or remove them and try again."))
    shiny::validate(shiny::need(!is.null(input$ppp_x_col) && input$ppp_x_col %in% names(d), "Select a valid X coordinate column."))
    shiny::validate(shiny::need(!is.null(input$ppp_y_col) && input$ppp_y_col %in% names(d), "Select a valid Y coordinate column."))

    x <- suppressWarnings(as.numeric(d[[input$ppp_x_col]]))
    y <- suppressWarnings(as.numeric(d[[input$ppp_y_col]]))
    valid <- is.finite(x) & is.finite(y)

    mark_by <- NULL
    if (!is.null(input$ppp_mark_col) && input$ppp_mark_col != "none" && input$ppp_mark_col %in% names(d)) {
      mark_by <- d[[input$ppp_mark_col]][valid]
    }

    # A cleared/backspaced numericInput reports NA_real_ in Shiny, not NULL
    # - same hazard documented at length in compute_isolation_forest()'s own
    # ntrees/contamination guards and validate_mahalanobis_inputs(). Falls
    # back to the documented default of 99 rather than reaching
    # compute_csr_envelope()'s own stop() with a less specific message, or
    # (worse) reaching spatstat.explore::envelope(nsim = NA) directly, which
    # does not fail cleanly.
    nsim <- if (!is.null(input$ppp_nsim) && is.finite(input$ppp_nsim) && input$ppp_nsim >= 1) {
      round(input$ppp_nsim)
    } else 99
    ppp_window <- if (!is.null(input$ppp_window) && input$ppp_window == "rectangle") "rectangle" else "convex_hull"
    # Global (simultaneous) envelope by default - a valid whole-curve test;
    # unticking gives the descriptive pointwise band. NULL (input not yet
    # rendered) -> the default TRUE.
    global_env <- !isFALSE(input$ppp_global_envelope)
    edge_correct <- !isFALSE(input$ppp_edge_correct)

    # Progress bar, not a speedup: profiled directly (see the vidternary
    # Structural Audit for the numbers) that the CSR envelope test alone -
    # nsim separate simulations, each rebuilding a random pattern AND
    # re-running the L-function on it - is 20-50x more expensive than
    # building the pattern and computing K/L/G/intensity combined, and
    # gets slower still as the dataset grows (confirmed: ~7.5s for 99
    # simulations at just 500 points). Nothing here makes that
    # computation itself faster - `spatstat.explore::envelope()` has no
    # progress-callback hook to report real per-simulation completion
    # without reimplementing the Monte Carlo loop by hand (a real
    # correctness risk for a purely cosmetic feature, avoided
    # deliberately - see this file's sibling spatial_point_pattern_
    # analysis.R's own header comment on why K/L/G/envelope lean on
    # spatstat rather than a hand-rolled reimplementation). What this DOES
    # fix is the actual complaint: with no feedback at all, a genuinely
    # slow-but-working computation is indistinguishable from a frozen
    # app. The bar below moves honestly through the fast stages, then
    # sits with a clear "this is the slow step" message for as long as
    # the envelope simulations actually take - no fabricated
    # per-simulation percentage it can't really measure.
    shiny::withProgress(message = "Analyzing point pattern...", value = 0, {
      shiny::incProgress(0.05, detail = "Building point pattern")
      pp <- tryCatch(build_point_pattern(x[valid], y[valid], window = ppp_window),
                      error = function(e) { shiny::validate(paste("Error building point pattern:", e$message)) })

      shiny::incProgress(0.15, detail = "Computing K/L/G functions")
      k <- tryCatch(compute_ripley_k(pp),
                     error = function(e) { shiny::validate(paste("Error computing K-function:", e$message)) })
      l <- tryCatch(compute_l_function(pp),
                     error = function(e) { shiny::validate(paste("Error computing L-function:", e$message)) })
      g <- tryCatch(compute_g_function(pp),
                     error = function(e) { shiny::validate(paste("Error computing G-function:", e$message)) })

      shiny::incProgress(0.15, detail = "Computing kernel intensity map")
      dens <- tryCatch(compute_kernel_intensity(pp, edge_correct = edge_correct),
                         error = function(e) { shiny::validate(paste("Error computing kernel intensity:", e$message)) })

      shiny::incProgress(0.05, detail = sprintf(
        "Running %d CSR envelope simulations - this is the slow step, especially for larger datasets. Lower “Number of CSR envelope simulations” above for a faster (still valid, just coarser) result.",
        nsim))
      env <- tryCatch(compute_csr_envelope(pp, nsim = nsim, global = global_env),
                        error = function(e) { shiny::validate(paste("Error running CSR envelope test:", e$message)) })

      shiny::incProgress(0.60, detail = "Done")

      list(pp = pp, k = k, l = l, g = g, env = env, dens = dens,
           x = x[valid], y = y[valid], mark_by = mark_by, mark_label = input$ppp_mark_col,
           nsim = nsim, window = ppp_window,
           envelope_type = if (global_env) "global" else "pointwise",
           edge_corrected = edge_correct,
           n_rows_before_filter = nrow(combined_data()), n_rows_after_filter = nrow(d),
           n = spatstat.geom::npoints(pp), area = spatstat.geom::area.owin(spatstat.geom::Window(pp)),
           # "intensity" here is the single GLOBAL average (n / area) - a
           # genuinely different number from the kernel intensity PLOT's
           # own per-pixel values, which is a smoothed LOCAL density
           # surface, not a running count. In a tight cluster, that local
           # rate can be several times the global average (confirmed
           # directly: up to ~9x in a clustered test case) - both numbers
           # are shown in the summary table below specifically so that
           # relationship is visible rather than a surprise (see the peak
           # value's own comment there).
           intensity = spatstat.geom::intensity.ppp(pp),
           peak_intensity = max(dens$intensity, na.rm = TRUE))
    })
  })

  ppp_placeholder_msg <- "Upload data, choose X/Y coordinate columns, and click \"Analyze Point Pattern\"."

  output$ppp_status <- renderText({
    # Same fix as server_spatial.R's/server_evs.R's own output$*_status -
    # see those comments for the full explanation of why a plain tryCatch
    # here would show this same generic placeholder for every validate()
    # failure instead of the specific message telling the user what to fix.
    if (is.null(input$ppp_analyze) || input$ppp_analyze == 0) {
      return(ppp_placeholder_msg)
    }
    tryCatch({
      res <- result()
      sprintf("n = %d points | window area = %.4g | mean intensity = %.6g points/area | peak (hotspot) intensity = %.6g points/area | CSR envelope simulations = %d",
              res$n, res$area, res$intensity, res$peak_intensity, res$nsim)
    }, shiny.silent.error = function(e) {
      if (!nzchar(conditionMessage(e))) return(ppp_placeholder_msg)
      stop(e)
    })
  })

  # Matches server_spatial.R's own spatial_plot_dim exactly (same 8:7
  # download aspect ratio, same 517px on-screen height, same reasoning for
  # why renderPlot()'s width/height must be set explicitly rather than
  # relying on the plot's internal device size alone) - see that file's own
  # comment for the full explanation.
  ppp_plot_height_px <- 517
  ppp_plot_dim <- list(
    width = round(ppp_plot_height_px * 8 / 7),
    height = ppp_plot_height_px,
    res = ppp_plot_height_px / 7
  )

  output$ppp_pattern_plot <- renderPlot({
    res <- result()
    print(create_point_pattern_plot(res$x, res$y, res$mark_by, res$mark_label))
  }, width = ppp_plot_dim$width, height = ppp_plot_dim$height, res = ppp_plot_dim$res)

  output$ppp_intensity_plot <- renderPlot({
    print(create_kernel_intensity_plot(result()$dens))
  }, width = ppp_plot_dim$width, height = ppp_plot_dim$height, res = ppp_plot_dim$res)

  output$ppp_k_plot <- renderPlot({
    print(create_ripley_k_plot(result()$k))
  }, width = ppp_plot_dim$width, height = ppp_plot_dim$height, res = ppp_plot_dim$res)

  output$ppp_l_plot <- renderPlot({
    print(create_l_function_plot(result()$l))
  }, width = ppp_plot_dim$width, height = ppp_plot_dim$height, res = ppp_plot_dim$res)

  output$ppp_g_plot <- renderPlot({
    print(create_g_function_plot(result()$g))
  }, width = ppp_plot_dim$width, height = ppp_plot_dim$height, res = ppp_plot_dim$res)

  output$ppp_envelope_plot <- renderPlot({
    print(create_csr_envelope_plot(result()$env))
  }, width = ppp_plot_dim$width, height = ppp_plot_dim$height, res = ppp_plot_dim$res)

  output$ppp_summary_table <- renderTable({
    res <- result()
    window_label <- if (isTRUE(res$window == "rectangle")) "Window area (bounding box)" else "Window area (convex hull)"
    data.frame(
      Metric = c("Rows before pre-analysis filter", "Rows after pre-analysis filter",
                 "Points (n)", "Observation window", window_label, "Mean intensity (n / area)",
                 "Peak kernel intensity (hotspot map's highest local value)",
                 "Kernel intensity edge correction", "CSR envelope type", "CSR envelope simulations"),
      Value = c(sprintf("%d", res$n_rows_before_filter), sprintf("%d", res$n_rows_after_filter),
                sprintf("%d", res$n),
                if (isTRUE(res$window == "rectangle")) "Rectangle (bounding box)" else "Convex hull",
                sprintf("%.4g", res$area), sprintf("%.6g", res$intensity),
                sprintf("%.6g", res$peak_intensity),
                if (isTRUE(res$edge_corrected)) "Diggle (on)" else "Off",
                if (identical(res$envelope_type, "pointwise")) "Pointwise (descriptive)" else "Global (simultaneous)",
                sprintf("%d", res$nsim))
    )
  })

  # Same blank-error-on-unclicked-download gap fixed for server_spatial.R's
  # own downloads (see that file's comment) - confirmed reachable here the
  # same way, via direct testServer() reproduction against the unguarded
  # handler, before adding this same fix rather than assuming it applied.
  safe_result <- function() {
    tryCatch(result(), error = function(e) {
      if (nzchar(e$message)) {
        stop("Could not generate this download: ", e$message)
      }
      stop("Upload data, choose X/Y coordinate columns, and click \"Analyze Point Pattern\" before downloading.")
    })
  }

  output$ppp_download_pattern <- downloadHandler(
    filename = function() paste0("ppp_pattern_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      res <- safe_result()
      ggplot2::ggsave(file, plot = create_point_pattern_plot(res$x, res$y, res$mark_by, res$mark_label), width = 8, height = 7, dpi = 300)
    }
  )
  output$ppp_download_intensity <- downloadHandler(
    filename = function() paste0("ppp_intensity_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) ggplot2::ggsave(file, plot = create_kernel_intensity_plot(safe_result()$dens), width = 8, height = 7, dpi = 300)
  )
  output$ppp_download_k <- downloadHandler(
    filename = function() paste0("ppp_k_function_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) ggplot2::ggsave(file, plot = create_ripley_k_plot(safe_result()$k), width = 8, height = 7, dpi = 300)
  )
  output$ppp_download_l <- downloadHandler(
    filename = function() paste0("ppp_l_function_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) ggplot2::ggsave(file, plot = create_l_function_plot(safe_result()$l), width = 8, height = 7, dpi = 300)
  )
  output$ppp_download_g <- downloadHandler(
    filename = function() paste0("ppp_g_function_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) ggplot2::ggsave(file, plot = create_g_function_plot(safe_result()$g), width = 8, height = 7, dpi = 300)
  )
  output$ppp_download_envelope <- downloadHandler(
    filename = function() paste0("ppp_csr_envelope_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) ggplot2::ggsave(file, plot = create_csr_envelope_plot(safe_result()$env), width = 8, height = 7, dpi = 300)
  )
  output$ppp_download_data <- downloadHandler(
    filename = function() paste0("ppp_values_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"),
    content = function(file) {
      res <- safe_result()
      writexl::write_xlsx(list(K_function = res$k, L_function = res$l, G_function = res$g, CSR_envelope = res$env), file)
    }
  )

  return(list(
    module_name = "server_spatial_ppp"
  ))
}
