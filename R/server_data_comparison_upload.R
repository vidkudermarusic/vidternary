# ---- Server Data Comparison Module: File Upload & Dataset Selection ----
# Owns rv$comparison_data (a named list of data frames, one per uploaded
# Excel file) and every selector that depends on which files are loaded -
# independent of the main "Ternary Plots" tab's rv$df1/rv$df2, and not
# limited to exactly two files. The stats/correlation/multivariate/preview
# modules all read from rv$comparison_data and the selectors registered
# here instead.

register_data_comparison_upload_handlers <- function(input, output, session, rv, show_message, log_operation) {

  observeEvent(input$comparison_files, {
    req(input$comparison_files)
    tryCatch({
      n <- nrow(input$comparison_files)
      raw_names <- tools::file_path_sans_ext(input$comparison_files$name)

      # Disambiguate duplicate file base names (e.g. two files both named
      # "sample.xlsx" from different folders) so every entry gets a unique
      # key in rv$comparison_data.
      dataset_names <- make.unique(raw_names, sep = " #")

      dfs <- list()
      failed <- c()
      for (i in seq_len(n)) {
        d <- tryCatch(openxlsx::read.xlsx(input$comparison_files$datapath[i], sheet = 1), error = function(e) NULL)
        if (is.null(d)) {
          failed <- c(failed, input$comparison_files$name[i])
        } else {
          dfs[[dataset_names[i]]] <- d
        }
      }

      rv$comparison_data <- dfs

      if (length(failed) > 0) {
        show_message(paste("Could not read:", paste(failed, collapse = ", ")), "warning")
      }
      if (length(dfs) > 0) {
        show_message(paste("Loaded", length(dfs), "dataset(s) for comparison"), "success")
        log_operation("SUCCESS", "Data Comparison files loaded", paste("Datasets:", paste(names(dfs), collapse = ", ")))
      }

    }, error = function(e) {
      show_message(paste("Error loading files:", e$message), "error")
      log_operation("ERROR", "Failed to load Data Comparison files", e$message)
    })
  })

  # Which datasets to include in stats/correlation - defaults to all loaded.
  # req() alone doesn't block on an empty list (lists aren't atomic, so
  # isTruthy() treats any non-NULL list - even length 0 - as truthy), hence
  # the explicit length check.
  output$comparison_dataset_selector_ui <- renderUI({
    req(length(rv$comparison_data) > 0)
    selectizeInput(session$ns("comparison_selected"), "Datasets to analyze:",
      choices = names(rv$comparison_data), selected = names(rv$comparison_data), multiple = TRUE)
  })

  # Multivariate target/reference/columns selectors, kept independent from
  # the main tab's shared "Universal Column Selector" (input$multivariate_columns).
  observe({
    req(length(rv$comparison_data) > 0)
    dataset_names <- names(rv$comparison_data)
    updateSelectInput(session, "comparison_mv_target", choices = dataset_names,
      selected = if (!is.null(input$comparison_mv_target) && input$comparison_mv_target %in% dataset_names) input$comparison_mv_target else dataset_names[1])
    updateSelectInput(session, "comparison_preview_target", choices = dataset_names,
      selected = if (!is.null(input$comparison_preview_target) && input$comparison_preview_target %in% dataset_names) input$comparison_preview_target else dataset_names[1])
  })

  observe({
    req(length(rv$comparison_data) > 0, input$comparison_mv_target)
    other_names <- setdiff(names(rv$comparison_data), input$comparison_mv_target)
    choices <- c("Self (same dataset)" = "__self__", stats::setNames(other_names, other_names))
    updateSelectInput(session, "comparison_mv_reference", choices = choices,
      selected = if (!is.null(input$comparison_mv_reference) && input$comparison_mv_reference %in% choices) input$comparison_mv_reference else "__self__")
  })

  observe({
    req(length(rv$comparison_data) > 0, input$comparison_mv_target)
    target_df <- rv$comparison_data[[input$comparison_mv_target]]
    req(target_df)
    numeric_cols <- names(target_df)[sapply(target_df, is.numeric)]
    updateSelectizeInput(session, "comparison_mv_columns", choices = numeric_cols, selected = character(0))
  })

  # Data readiness status for Data Comparison tab
  output$data_readiness_status <- renderPrint({
    if (is.null(rv$comparison_data) || length(rv$comparison_data) == 0) {
      cat("📋 Please upload one or more Excel files to begin.\n")
      # invisible(), not a bare return(): renderPrint() replicates console
      # auto-print semantics (via withVisible()) - a plain return() yields
      # a VISIBLE NULL, which renderPrint() then explicitly prints as a
      # literal trailing "NULL" line right under the cat() message above.
      # Confirmed directly against the real shiny::renderPrint() (not just
      # inferred): the unfixed version produced exactly
      # "\U0001F4CB Please upload one or more Excel files to begin.\nNULL"
      # - visible on this tab's very first load, before any file is ever
      # uploaded. The populated-data branch below doesn't have this problem
      # - every one of its own branches ends in a cat() call, whose own
      # return value is already invisible.
      return(invisible())
    }

    dfs <- rv$comparison_data
    numeric_cols_per_df <- lapply(dfs, function(d) names(d)[sapply(d, is.numeric)])
    common_cols <- Reduce(intersect, numeric_cols_per_df)

    cat("=== DATA READINESS STATUS ===\n")
    for (nm in names(dfs)) {
      cat(nm, ":", nrow(dfs[[nm]]), "rows x", ncol(dfs[[nm]]), "columns\n")
    }
    cat("Common numeric columns across all", length(dfs), "dataset(s):", length(common_cols), "\n")

    if (length(dfs) >= 2 && length(common_cols) >= 2) {
      cat("✅ Ready for comparison and multivariate analysis\n")
      cat("Available columns:", paste(common_cols, collapse = ", "), "\n")
    } else if (length(dfs) == 1) {
      cat("ℹ️ One dataset loaded - upload another to enable comparison, or proceed with single-dataset stats/correlation.\n")
    } else {
      cat("❌ Need at least 2 common numeric columns across the selected datasets\n")
    }
  })
}
