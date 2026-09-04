# ---- Server: "Compositional Analysis" tab ----
# Wires R/compositional_data_analysis.R's pure statistics/plotting
# functions into the app. Structure mirrors server_evs.R/server_spatial.R
# (combined-data reactive, column-choice observer, eventReactive for the
# transform+PCA).
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

#' Wire up the Compositional Analysis tab's server logic
#'
#' Registers the observers/renderers for the "Compositional Analysis" tab:
#' file upload/combine, Wt% column auto-detection, and the CLR/ILR
#' transform + PCA pipeline (`clr_transform()`/`ilr_transform()`/
#' `compositional_pca()`).
#'
#' @param input The Shiny `input` object.
#' @param output The Shiny `output` object.
#' @param session The Shiny session object.
#' @param rv The app's shared `reactiveValues` object.
#' @param show_message Function to show a user-facing status message.
#' @param log_operation Function to record a structured log entry.
#' @return A list with `module_name`.
#' @export
create_server_coda <- function(input, output, session, rv, show_message, log_operation) {

  combined_data <- reactive({
    req(input$coda_files)
    n_files <- nrow(input$coda_files)
    dfs <- lapply(seq_len(n_files), function(i) {
      d <- tryCatch(openxlsx::read.xlsx(input$coda_files$datapath[i], sheet = 1), error = function(e) NULL)
      if (is.null(d)) return(NULL)
      if (n_files > 1) d$source_file <- tools::file_path_sans_ext(input$coda_files$name[i])
      d
    })
    dfs <- Filter(Negate(is.null), dfs)
    shiny::validate(shiny::need(length(dfs) > 0, "None of the selected files could be read."))
    if (length(dfs) == 1) return(dfs[[1]])
    common_cols <- Reduce(intersect, lapply(dfs, names))
    shiny::validate(shiny::need(length(common_cols) > 0, "The selected files have no columns in common."))
    do.call(rbind, lapply(dfs, function(d) d[, common_cols, drop = FALSE]))
  })

  observe({
    d <- tryCatch(combined_data(), error = function(e) NULL)
    if (is.null(d)) return()
    numeric_cols <- names(d)[sapply(d, is.numeric)]
    wt_cols <- numeric_cols[grepl("wt%", numeric_cols, ignore.case = TRUE)]
    updateSelectizeInput(session, "coda_parts", choices = numeric_cols,
                          selected = if (length(wt_cols) >= 3) wt_cols else NULL)
  })

  result <- eventReactive(input$coda_run, {
    d <- combined_data()
    parts <- input$coda_parts
    shiny::validate(shiny::need(!is.null(parts) && length(parts) >= 3, "Select at least 3 compositional parts (columns)."))
    shiny::validate(shiny::need(all(parts %in% names(d)), "One or more selected columns are not present in the data."))

    d_complete <- d[stats::complete.cases(d[, parts, drop = FALSE]), , drop = FALSE]
    shiny::validate(shiny::need(nrow(d_complete) >= length(parts) + 1,
                                 "Not enough complete rows (with no missing values in the selected parts) to run PCA."))

    clr <- tryCatch(clr_transform(d_complete, parts), error = function(e) { shiny::validate(paste("Error computing CLR:", e$message)) })
    ilr_res <- tryCatch(ilr_transform(d_complete, parts), error = function(e) { shiny::validate(paste("Error computing ILR:", e$message)) })
    pca <- tryCatch(compositional_pca(clr), error = function(e) { shiny::validate(paste("Error running PCA:", e$message)) })
    # ILR is an orthonormal (isometric) re-expression of CLR, so PCA on ILR
    # gives the same eigenvalues/variance-explained and scores (up to
    # per-component sign) as PCA on CLR - only the loadings differ, since
    # they're expressed in the abstract ilr_1..ilr_(D-1) balance coordinates
    # instead of one coordinate per element. Run separately so that basis's
    # own loadings table has something to show - previously ILR data was
    # only ever reachable via the download button, never actually displayed.
    pca_ilr <- tryCatch(compositional_pca(ilr_res$ilr), error = function(e) { shiny::validate(paste("Error running PCA on ILR:", e$message)) })

    list(clr = clr, ilr = ilr_res$ilr, pca = pca, pca_ilr = pca_ilr, n = nrow(d_complete), parts = parts)
  })

  coda_placeholder_msg <- "Upload data, select 3 or more compositional (Wt%) columns, and click \"Transform & Run PCA\"."

  output$coda_status <- renderText({
    # Same fix as server_evs.R's output$evs_status - see that comment for
    # the full explanation. In short: a plain tryCatch(..., error =
    # function(e) NULL) catches shiny::validate()'s condition the same way
    # it catches req()'s, so every validate() failure (too few parts,
    # missing columns, not enough complete rows, etc.) showed this same
    # generic placeholder instead of the specific message telling the user
    # what to fix. req()'s condition has the same class but an empty
    # message, which is how the two are told apart below.
    if (is.null(input$coda_run) || input$coda_run == 0) {
      return(coda_placeholder_msg)
    }
    tryCatch({
      res <- result()
      sprintf("PCA complete: n = %d complete rows, %d parts. PC1+PC2 explain %.1f%% of the (Aitchison) variance.",
              res$n, length(res$parts), sum(res$pca$var_explained[1:2]))
    }, shiny.silent.error = function(e) {
      if (!nzchar(conditionMessage(e))) return(coda_placeholder_msg)
      stop(e)
    })
  })

  # geom_point()'s `size` is a fixed physical size (mm), not relative to
  # the plot - matching the preview device's aspect ratio/inches to the
  # download's 9x7in avoids a preview/download point-size mismatch. See
  # server_spatial.R for the full explanation, including why
  # renderPlot()'s width/height must stay close to plotOutput's actual
  # on-screen size (they also set the browser's literal display size, not
  # just the internal device resolution) - height must match
  # ui_coda_tab.R's plotOutput(..., height=) exactly; width is derived
  # from the same 9:7 ratio as the download. (480px, up from the
  # original 450px, per a user request to size these plots up slightly -
  # capped by the actual rendered width of its column(6) container at a
  # typical desktop viewport, ~632px measured, minus a safety margin; the
  # 9:7 ratio in a half-width column leaves less headroom than the other
  # 3 tabs, so this one grows less.)
  coda_plot_height_px <- 480
  coda_plot_width_px <- round(coda_plot_height_px * 9 / 7)
  coda_plot_res <- coda_plot_height_px / 7

  output$coda_biplot <- renderPlot({
    print(create_coda_biplot(result()$pca))
  }, width = coda_plot_width_px, height = coda_plot_height_px, res = coda_plot_res)

  output$coda_biplot_ilr <- renderPlot({
    print(create_coda_biplot(result()$pca_ilr))
  }, width = coda_plot_width_px, height = coda_plot_height_px, res = coda_plot_res)

  output$coda_variance_table <- renderTable({
    pca <- result()$pca
    data.frame(
      Component = paste0("PC", seq_along(pca$var_explained)),
      Variance_pct = sprintf("%.2f%%", pca$var_explained),
      Cumulative_pct = sprintf("%.2f%%", cumsum(pca$var_explained))
    )
  })

  output$coda_loadings_table <- renderTable({
    loadings <- result()$pca$loadings
    df <- data.frame(Element = rownames(loadings), loadings, row.names = NULL, check.names = FALSE)
    df
  })

  output$coda_loadings_table_ilr <- renderTable({
    loadings <- result()$pca_ilr$loadings
    df <- data.frame(Balance = rownames(loadings), loadings, row.names = NULL, check.names = FALSE)
    df
  })

  # Every download handler below calls result() - an eventReactive gated on
  # input$coda_run - with no server-side gating on the buttons themselves
  # (see ui_coda_tab.R: none of the 4 downloadButtons sit inside a
  # conditionalPanel, so all are clickable before "Transform & Run PCA" is
  # ever pressed). Before that click, result() throws a
  # shiny::validate()/req() condition whose $message is always "" by
  # design - the same blank-error gap found and fixed in
  # server_plot_builder.R's output$builder_download (see the vidternary
  # Structural Audit's §03 for that writeup); confirmed reachable here the
  # same way, via direct testServer() reproduction against the unmodified
  # handler. safe_result() gives every handler below a clear, actionable
  # message for that case, while still surfacing a genuine error's own text.
  safe_result <- function() {
    tryCatch(result(), error = function(e) {
      if (nzchar(e$message)) {
        stop("Could not generate this download: ", e$message)
      }
      stop("Select at least 3 compositional parts and click \"Transform & Run PCA\" before downloading.")
    })
  }

  output$coda_download_clr <- downloadHandler(
    filename = function() paste0("coda_clr_transformed_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"),
    content = function(file) writexl::write_xlsx(safe_result()$clr, file)
  )

  output$coda_download_ilr <- downloadHandler(
    filename = function() paste0("coda_ilr_transformed_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"),
    content = function(file) writexl::write_xlsx(safe_result()$ilr, file)
  )

  output$coda_download_biplot <- downloadHandler(
    filename = function() paste0("coda_biplot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      ggplot2::ggsave(file, plot = create_coda_biplot(safe_result()$pca), width = 9, height = 7, dpi = 300)
    }
  )

  output$coda_download_biplot_ilr <- downloadHandler(
    filename = function() paste0("coda_biplot_ilr_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      ggplot2::ggsave(file, plot = create_coda_biplot(safe_result()$pca_ilr), width = 9, height = 7, dpi = 300)
    }
  )

  return(list(
    module_name = "server_coda"
  ))
}
