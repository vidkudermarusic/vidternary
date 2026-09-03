# ---- Server Ternary Plots Module ----
# This module contains the single-file ternary plot generation
# functionality: parameter building, live previews, the analysis-report
# text, and the Save Plot 1/2/Both handlers.
#
# Related handlers live in sibling files, split out for size, both called
# from within this tab's own moduleServer() wrapper in server_logic.R:
#   server_ternary_plots_groups.R - categorical group-selection UI
#   server_file_handlers.R        - Dataset 1/2 upload + copy-settings
#
# "Multiple Ternary Creator" used to be registered from inside this same
# function (register_ternary_plots_batch_handlers(), called below) - it's
# now a fully independent sibling tab/module (server_ternary_plots_batch.R,
# wired directly from server_logic.R's own moduleServer("multiple_ternary",
# ...) call), not nested here, since the two turned out to be one entangled
# server unit rather than genuinely separate tabs (see the vidternary
# Structural Audit for the full cross-tab dependency map that found this).
#
# The per-element dynamic filter UI (dynamic_filters_A1/B1/C1/A2/B2/C2) and
# the "only one filter method active at a time" enforcement used to live in
# a shared server_filter_management.R that also built Multiple Ternary
# Creator's filter UI in the same registration call - split apart for the
# same reason, with this tab's half moved directly into this file below.
#
# BUGFIX (as part of an earlier split): generate_analysis_report() below was
# previously missing its closing brace, so everything that followed it in
# the original single file - the analysis report renderer, the Save Plot
# buttons, and the group-selection UI - was accidentally nested inside its
# body and never executed. That's fixed here: the function now closes right
# after its return(), and the code that used to trail it is registered
# properly (below, and in server_ternary_plots_groups.R).

#' Wire up the "Ternary Plots" tab's single-file server logic
#'
#' Registers this tab's core handlers: the mutually-exclusive
#' statistical/multivariate filter checkboxes, the per-element dynamic
#' filter UI (`dynamic_filters_A1`/`B1`/`C1`/`A2`/`B2`/`C2`), the Dataset
#' 1/2 live preview renders, the analysis-report text, and the Save Plot
#' 1/2/Both handlers. Also calls
#' [register_ternary_plots_group_handlers()] for the categorical
#' group-selection UI; Dataset 1/2 upload and "Copy Settings" are wired
#' separately by [create_server_file_handlers()] (both called from the
#' same `moduleServer("ternary_plots", ...)` wrapper in `server_logic.R`,
#' sharing this tab's namespace and `rv`).
#'
#' @param input The Shiny `input` object.
#' @param output The Shiny `output` object.
#' @param session The Shiny session object.
#' @param rv The app's shared `reactiveValues` object.
#' @param show_message Function to show a user-facing status message.
#' @param log_operation Function to record a structured log entry.
#' @return An empty list - this function's effect is entirely the
#'   observers/renderers it registers.
#' @export
create_server_ternary_plots <- function(input, output, session, rv, show_message, log_operation) {

  # ---- Enforce one filter per ternary plot ----
  # Mahalanobis, Isolation Forest, IQR, Z-score, and MAD are alternative
  # ways to flag outliers, not stages meant to compound on the same plot.
  # Checking one here unchecks the other four, so the checkbox UI can only
  # ever represent zero or one active filter at a time (general_ternary_plot()
  # also enforces this server-side as a defense-in-depth backstop for any
  # caller that bypasses this UI, e.g. batch/programmatic use).
  filter_method_checkboxes <- c("use_mahalanobis", "use_isolation_forest",
                                 "use_iqr_filter", "use_zscore_filter", "use_mad_filter")

  lapply(filter_method_checkboxes, function(checkbox_id) {
    observeEvent(input[[checkbox_id]], {
      if (isTRUE(input[[checkbox_id]])) {
        other_checkboxes <- setdiff(filter_method_checkboxes, checkbox_id)
        for (other_id in other_checkboxes) {
          if (isTRUE(input[[other_id]])) {
            updateCheckboxInput(session, other_id, value = FALSE)
          }
        }
      }
    }, ignoreInit = TRUE)
  })

  # ---- Dynamic Filter UI Generation (per-element, Dataset 1/2) ----
  output$dynamic_filters_A1 <- renderUI({
    req(input$element_A1)
    lapply(input$element_A1, function(element) {
      div(
        style = "margin: 5px 0; padding: 5px; border: 1px solid #ddd; border-radius: 3px;",
        h6(paste("Filter for", element)),
        textInput(session$ns(paste0("filter_A1_", gsub("[^A-Za-z0-9]", "_", element))),
                 paste("Threshold for", element),
                 placeholder = paste("e.g., > 10"))
      )
    })
  })

  output$dynamic_filters_B1 <- renderUI({
    req(input$element_B1)
    lapply(input$element_B1, function(element) {
      div(
        style = "margin: 5px 0; padding: 5px; border: 1px solid #ddd; border-radius: 3px;",
        h6(paste("Filter for", element)),
        textInput(session$ns(paste0("filter_B1_", gsub("[^A-Za-z0-9]", "_", element))),
                 paste("Threshold for", element),
                 placeholder = paste("e.g., > 10"))
      )
    })
  })

  output$dynamic_filters_C1 <- renderUI({
    req(input$element_C1)
    lapply(input$element_C1, function(element) {
      div(
        style = "margin: 5px 0; padding: 5px; border: 1px solid #ddd; border-radius: 3px;",
        h6(paste("Filter for", element)),
        textInput(session$ns(paste0("filter_C1_", gsub("[^A-Za-z0-9]", "_", element))),
                 paste("Threshold for", element),
                 placeholder = paste("e.g., > 10"))
      )
    })
  })

  output$dynamic_filters_A2 <- renderUI({
    req(input$element_A2)
    lapply(input$element_A2, function(element) {
      div(
        style = "margin: 5px 0; padding: 5px; border: 1px solid #ddd; border-radius: 3px;",
        h6(paste("Filter for", element)),
        textInput(session$ns(paste0("filter_A2_", gsub("[^A-Za-z0-9]", "_", element))),
                 paste("Threshold for", element),
                 placeholder = paste("e.g., > 10"))
      )
    })
  })

  output$dynamic_filters_B2 <- renderUI({
    req(input$element_B2)
    lapply(input$element_B2, function(element) {
      div(
        style = "margin: 5px 0; padding: 5px; border: 1px solid #ddd; border-radius: 3px;",
        h6(paste("Filter for", element)),
        textInput(session$ns(paste0("filter_B2_", gsub("[^A-Za-z0-9]", "_", element))),
                 paste("Threshold for", element),
                 placeholder = paste("e.g., > 10"))
      )
    })
  })

  output$dynamic_filters_C2 <- renderUI({
    req(input$element_C2)
    lapply(input$element_C2, function(element) {
      div(
        style = "margin: 5px 0; padding: 5px; border: 1px solid #ddd; border-radius: 3px;",
        h6(paste("Filter for", element)),
        textInput(session$ns(paste0("filter_C2_", gsub("[^A-Za-z0-9]", "_", element))),
                 paste("Threshold for", element),
                 placeholder = paste("e.g., > 10"))
      )
    })
  })

  # ---- Application status (baseline; Save Plot 1/2/Both below overwrite
  # this with a one-shot save-result message once clicked) ----
  output$status <- renderText({
    paste("Application Status: Ready\n",
          "Dataset 1:", ifelse(is.null(rv$df1), "Not loaded", paste(nrow(rv$df1), "rows,", ncol(rv$df1), "columns")), "\n",
          "Dataset 2:", ifelse(is.null(rv$df2), "Not loaded", paste(nrow(rv$df2), "rows,", ncol(rv$df2), "columns")))
  })

  # ---- Helper Functions ----

  # Function to collect individual element filters for main ternary plots tab
  # Now centralized in helpers.R as collect_main_ternary_filters

  # Simplified parameter extraction using unified function
  build_ternary_plot_params <- function(dataset_num, preview = FALSE) {
    req(rv[[paste0("df", dataset_num)]])
    params <- extract_ternary_params(input, rv, dataset_num, preview, multiple_mode = FALSE)

    # Add the original filename for proper plot titles
    if (dataset_num == 1 && !is.null(input$xlsx_file1)) {
      params$xlsx_display_name <- input$xlsx_file1$name
    } else if (dataset_num == 2 && !is.null(input$xlsx_file2)) {
      params$xlsx_display_name <- input$xlsx_file2$name
    }

    params
  }

  # ---- Ternary Plot Previews ----

  # renderPlot()'s default width/height = "auto" reads straight from
  # session$clientData with no floor - if the client reports 0 (e.g. the
  # browser hasn't finished laying out the tab yet), Shiny opens the PNG
  # device at 0 width/height and crashes in graphics::plot.new() with
  # "figure margins too large", *inside Shiny's own device setup*, before
  # this reactive's body (and its req() guards) ever runs. A req()/tryCatch
  # inside the render expression can't help - the fix has to supply
  # width/height with a real fallback instead.
  safe_plot_dim <- function(output_id, suffix, fallback) {
    function() {
      d <- session$clientData[[paste0("output_", output_id, "_", suffix)]]
      if (is.null(d) || !is.finite(d) || d <= 0) fallback else d
    }
  }

  # Dataset 1 ternary preview
  output$ternary_preview1 <- renderPlot({
    req(input$xlsx_file1)
    req(input$element_A1, input$element_B1, input$element_C1)

    if (getOption("ternary.debug", FALSE)) {
      cat("=== PREVIEW 1 DEBUGGING START ===\n")
      cat("DEBUG: Starting ternary preview 1\n")
      cat("DEBUG: Dataset 1 dimensions:", nrow(rv$df1), ncol(rv$df1), "\n")
      cat("DEBUG: Available columns:", paste(names(rv$df1), collapse = ", "), "\n")
    }

    tryCatch({
      # Build parameters for ternary plot
      params <- build_ternary_plot_params(1, TRUE)

      if (is.null(params)) {
        if (getOption("ternary.debug", FALSE)) cat("DEBUG: build_ternary_plot_params returned NULL\n")
        return()
      }

      if (getOption("ternary.debug", FALSE)) {
        cat("DEBUG: Parameters built successfully\n")
        cat("DEBUG: About to call general_ternary_plot\n")
      }

      # Call the main ternary plot function directly
      result <- do.call(general_ternary_plot, params)

      if (getOption("ternary.debug", FALSE)) {
        cat("DEBUG: general_ternary_plot returned:", result, "\n")
        cat("DEBUG: Preview mode - plot created on device\n")
      }

    }, error = function(e) {
      if (getOption("ternary.debug", FALSE)) {
        cat("DEBUG: Error in ternary preview 1:", e$message, "\n")
        cat("DEBUG: Error call:", toString(e$call), "\n")
      }
      log_operation("ERROR", "Ternary preview 1 failed", e$message)
      # Show a simple error plot
      plot(1, 1, type = "n", xlab = "", ylab = "", main = paste("Error:", e$message))
    })

    if (getOption("ternary.debug", FALSE)) cat("=== PREVIEW 1 DEBUGGING END ===\n\n")
  }, width = safe_plot_dim("ternary_preview1", "width", 450), height = safe_plot_dim("ternary_preview1", "height", 500))

  # Dataset 2 ternary preview
  output$ternary_preview2 <- renderPlot({
    req(input$xlsx_file2)
    req(input$element_A2, input$element_B2, input$element_C2)

    if (getOption("ternary.debug", FALSE)) {
      cat("=== PREVIEW 2 DEBUGGING START ===\n")
      cat("DEBUG: Starting ternary preview 2\n")
      cat("DEBUG: Dataset 2 dimensions:", nrow(rv$df2), ncol(rv$df2), "\n")
      cat("DEBUG: Available columns:", paste(names(rv$df2), collapse = ", "), "\n")
      cat("DEBUG: input$element_A2 =", input$element_A2, "\n")
      cat("DEBUG: input$element_B2 =", input$element_B2, "\n")
      cat("DEBUG: input$element_C2 =", input$element_C2, "\n")
    }

    tryCatch({
      # Build parameters for ternary plot
      params <- build_ternary_plot_params(2, TRUE)

      if (is.null(params)) {
        if (getOption("ternary.debug", FALSE)) cat("DEBUG: build_ternary_plot_params returned NULL\n")
        return()
      }

      if (getOption("ternary.debug", FALSE)) {
        cat("DEBUG: Parameters built successfully\n")
        cat("DEBUG: About to call general_ternary_plot\n")
      }

      # Call the main ternary plot function directly
      result <- do.call(general_ternary_plot, params)

      if (getOption("ternary.debug", FALSE)) {
        cat("DEBUG: general_ternary_plot returned:", result, "\n")
        cat("DEBUG: Preview mode - plot created on device\n")
      }

    }, error = function(e) {
      if (getOption("ternary.debug", FALSE)) {
        cat("DEBUG: Error in ternary preview 2:", e$message, "\n")
        cat("DEBUG: Error call:", toString(e$call), "\n")
      }
      log_operation("ERROR", "Ternary preview 2 failed", e$message)
      # Show a simple error plot
      plot(1, 1, type = "n", xlab = "", ylab = "", main = paste("Error:", e$message))
    })

    if (getOption("ternary.debug", FALSE)) cat("=== PREVIEW 2 DEBUGGING END ===\n\n")
  }, width = safe_plot_dim("ternary_preview2", "width", 450), height = safe_plot_dim("ternary_preview2", "height", 500))

  # ---- Group-selection handlers (sibling file, same tab/module) ----
  # Multiple Ternary Creator's batch handlers used to also be registered
  # here - now wired independently from server_logic.R (see this file's
  # header comment).
  register_ternary_plots_group_handlers(input, output, session, rv, show_message, log_operation)

  # Observer to populate multivariate column selector when datasets are loaded
  observe({
    req(rv$df1)
    choices <- names(rv$df1)
    updateSelectizeInput(session, "multivariate_columns", choices = choices, selected = NULL)
  })

  # Observer to update multivariate column selector when dataset 2 is loaded
  observe({
    req(rv$df2)
    choices <- names(rv$df2)
    # Update choices to include both datasets' columns
    if (!is.null(rv$df1)) {
      choices <- unique(c(names(rv$df1), names(rv$df2)))
    }
    updateSelectizeInput(session, "multivariate_columns", choices = choices, selected = NULL)
  })

  # Analysis Report Generator
  generate_analysis_report <- function(input, rv) {
    # Ensure at least one filter method is active. Passing these as separate
    # req() arguments (the previous form) requires every single one to be
    # TRUE at once to proceed - but this tab enforces "only one filter
    # active at a time" (see the mutual-exclusivity observers above), so
    # that older form could never actually pass in normal use and the
    # report body below never ran. Matches the `||` gate already used by
    # this function's one caller, output$analysis_report, below.
    req(input$use_mahalanobis || input$use_isolation_forest ||
        input$use_iqr_filter || input$use_zscore_filter || input$use_mad_filter)

    report_lines <- c()
    report_lines <- c(report_lines, "=== ANALYSIS METHODS REPORT ===")
    report_lines <- c(report_lines, paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
    report_lines <- c(report_lines, "")

    # Ternary Plot Elements and Parameters Section
    report_lines <- c(report_lines, "=== TERNARY PLOT ELEMENTS AND PARAMETERS ===")

    # Element A
    if (!is.null(input$element_A1) && length(input$element_A1) > 0) {
      report_lines <- c(report_lines, paste("Element A:", paste(input$element_A1, collapse = " + ")))
      # Add individual filters for Element A. Reads the same dynamically-
      # named filter_A1_<element> inputs (built by the dynamic_filters_A1
      # renderUI above) that the actual plot filtering already reads via
      # collect_main_ternary_filters() inside extract_ternary_params() -
      # this used to look up a single "filter_A1" input that never existed
      # (the real ones are per-element), so no filter ever showed up here
      # even though it was genuinely being applied to the plot.
      filters_A1 <- collect_main_ternary_filters(input$element_A1, "A", 1, input)
      for (filter_name in names(filters_A1)) {
        report_lines <- c(report_lines, paste("  • Filter", filter_name, ":", filters_A1[[filter_name]]))
      }
    }

    # Element B
    if (!is.null(input$element_B1) && length(input$element_B1) > 0) {
      report_lines <- c(report_lines, paste("Element B:", paste(input$element_B1, collapse = " + ")))
      # Add individual filters for Element B - see the Element A comment above.
      filters_B1 <- collect_main_ternary_filters(input$element_B1, "B", 1, input)
      for (filter_name in names(filters_B1)) {
        report_lines <- c(report_lines, paste("  • Filter", filter_name, ":", filters_B1[[filter_name]]))
      }
    }

    # Element C
    if (!is.null(input$element_C1) && length(input$element_C1) > 0) {
      report_lines <- c(report_lines, paste("Element C:", paste(input$element_C1, collapse = " + ")))
      # Add individual filters for Element C - see the Element A comment above.
      filters_C1 <- collect_main_ternary_filters(input$element_C1, "C", 1, input)
      for (filter_name in names(filters_C1)) {
        report_lines <- c(report_lines, paste("  • Filter", filter_name, ":", filters_C1[[filter_name]]))
      }
    }

    # Optional Parameters
    if (!is.null(input$optional_param1_1) && input$optional_param1_1 != "") {
      report_lines <- c(report_lines, paste("Optional Parameter 1 (Point Size):", input$optional_param1_1))
      if (!is.null(input$filter_op1_1) && nchar(input$filter_op1_1) > 0) {
        report_lines <- c(report_lines, paste("  • Filter:", input$filter_op1_1))
      }
      if (!is.null(input$optional_param1_representation1)) {
        report_lines <- c(report_lines, paste("  • Representation:", input$optional_param1_representation1))
      }
    }

    if (!is.null(input$optional_param2_1) && input$optional_param2_1 != "") {
      report_lines <- c(report_lines, paste("Optional Parameter 2 (Color):", input$optional_param2_1))
      if (!is.null(input$filter_op2_1) && nchar(input$filter_op2_1) > 0) {
        report_lines <- c(report_lines, paste("  • Filter:", input$filter_op2_1))
      }
    }

    report_lines <- c(report_lines, "")

    # Multivariate Analysis Section
    multivariate_methods <- c()
    if (input$use_mahalanobis) {
      multivariate_methods <- c(multivariate_methods, "Mahalanobis Distance")
      report_lines <- c(report_lines, "🔧 MAHALANOBIS DISTANCE:")
      report_lines <- c(report_lines, paste("  • Lambda (λ):", input$lambda))
      report_lines <- c(report_lines, paste("  • Omega (ω):", input$omega))
      report_lines <- c(report_lines, paste("  • Threshold Mode:", input$mdthresh_mode))
      if (input$mdthresh_mode == "manual") {
        report_lines <- c(report_lines, paste("  • Custom Threshold:", input$custom_mdthresh))
      } else {
        report_lines <- c(report_lines, "  • Formula: MDthresh = MDmean + √(100/(100+λ-ω)) × stdMD")
      }
      report_lines <- c(report_lines, paste("  • Outlier Handling:", if (input$outlier_mode_mahalanobis) "Keep only outliers" else "Remove outliers"))
      report_lines <- c(report_lines, paste("  • Reference Dataset:", input$mahalanobis_reference))
      report_lines <- c(report_lines, "")
    }


    if (input$use_isolation_forest) {
      multivariate_methods <- c(multivariate_methods, "Isolation Forest")
      report_lines <- c(report_lines, "🌲 ISOLATION FOREST:")
      report_lines <- c(report_lines, "  • Method: Machine learning anomaly detection")
      report_lines <- c(report_lines, paste("  • Outlier Handling:", if (input$outlier_mode_isolation) "Keep only outliers" else "Remove outliers"))
      report_lines <- c(report_lines, paste("  • Reference Dataset:", input$mahalanobis_reference_isolation))
      report_lines <- c(report_lines, "")
    }

    # Statistical Filtering Section
    statistical_methods <- c()
    if (input$use_iqr_filter) {
      statistical_methods <- c(statistical_methods, "IQR Filter")
      report_lines <- c(report_lines, "📊 IQR FILTER:")
      report_lines <- c(report_lines, "  • Method: Interquartile Range")
      report_lines <- c(report_lines, "  • Formula: Outliers > Q3+1.5×IQR (high values only)")
      report_lines <- c(report_lines, paste("  • Outlier Handling:", if (input$outlier_mode_iqr) "Keep only outliers" else "Remove outliers"))
      report_lines <- c(report_lines, "")
    }

    if (input$use_zscore_filter) {
      statistical_methods <- c(statistical_methods, "Z-Score Filter")
      report_lines <- c(report_lines, "📈 Z-SCORE FILTER:")
      report_lines <- c(report_lines, "  • Method: Standardized scores")
      report_lines <- c(report_lines, "  • Formula: Outliers z-score > 3 (high values only)")
      report_lines <- c(report_lines, paste("  • Outlier Handling:", if (input$outlier_mode_zscore) "Keep only outliers" else "Remove outliers"))
      report_lines <- c(report_lines, "")
    }

    if (input$use_mad_filter) {
      statistical_methods <- c(statistical_methods, "MAD Filter")
      report_lines <- c(report_lines, "📏 MAD FILTER:")
      report_lines <- c(report_lines, "  • Method: Median Absolute Deviation")
      report_lines <- c(report_lines, "  • Formula: Outliers > median+3×MAD (high values only)")
      report_lines <- c(report_lines, paste("  • Outlier Handling:", if (input$outlier_mode_mad) "Keep only outliers" else "Remove outliers"))
      report_lines <- c(report_lines, "")
    }

    # Summary Section
    report_lines <- c(report_lines, "=== SUMMARY ===")
    report_lines <- c(report_lines, paste("Multivariate Methods:", if (length(multivariate_methods) > 0) paste(multivariate_methods, collapse = ", ") else "None"))
    report_lines <- c(report_lines, paste("Statistical Methods:", if (length(statistical_methods) > 0) paste(statistical_methods, collapse = ", ") else "None"))
    report_lines <- c(report_lines, paste("Total Methods:", length(c(multivariate_methods, statistical_methods))))

    # Column Selection
    if (!is.null(input$multivariate_columns) && length(input$multivariate_columns) > 0) {
      report_lines <- c(report_lines, paste("Selected Columns:", paste(input$multivariate_columns, collapse = ", ")))
    } else {
      report_lines <- c(report_lines, "Selected Columns: All numeric columns (auto-selected)")
    }

    # First Ten Points Calculation
    report_lines <- c(report_lines, "")
    report_lines <- c(report_lines, "=== FIRST TEN POINTS CALCULATION ===")

    # Check if we have data available
    if (!is.null(rv$df1) && nrow(rv$df1) > 0) {
      report_lines <- c(report_lines, paste("Dataset 1 - Total rows:", nrow(rv$df1)))

      # Show selected columns for analysis
      if (!is.null(input$multivariate_columns) && length(input$multivariate_columns) > 0) {
        report_lines <- c(report_lines, paste("Selected columns for analysis:", paste(input$multivariate_columns, collapse = ", ")))
      }

      # Get the first 10 rows (or all if less than 10)
      first_ten_rows <- min(10, nrow(rv$df1))
      first_ten_data <- rv$df1[1:first_ten_rows, , drop = FALSE]

      # Determine which columns to show - selected columns if available, otherwise first 5
      columns_to_show <- if (!is.null(input$multivariate_columns) && length(input$multivariate_columns) > 0) {
        intersect(input$multivariate_columns, names(first_ten_data))
      } else {
        names(first_ten_data)[1:min(5, ncol(first_ten_data))]
      }

      report_lines <- c(report_lines, paste("First", first_ten_rows, "rows (Selected Columns):"))

      # Show selected columns for each row
      for (i in 1:first_ten_rows) {
        row_data <- first_ten_data[i, columns_to_show, drop = FALSE]
        col_values <- as.numeric(row_data[1, ])

        row_summary <- paste(sapply(1:length(columns_to_show), function(j) {
          paste(columns_to_show[j], "=", round(col_values[j], 3))
        }), collapse = ", ")

        report_lines <- c(report_lines, paste("  Row", i, ":", row_summary))
      }

      # Calculate ternary coordinates for first 10 rows
      report_lines <- c(report_lines, "")
      report_lines <- c(report_lines, "Ternary Coordinate Calculations:")

      # Get ternary elements
      element_A_cols <- if (!is.null(input$element_A1)) input$element_A1 else c()
      element_B_cols <- if (!is.null(input$element_B1)) input$element_B1 else c()
      element_C_cols <- if (!is.null(input$element_C1)) input$element_C1 else c()

      if (length(element_A_cols) > 0 && length(element_B_cols) > 0 && length(element_C_cols) > 0) {
        for (i in 1:first_ten_rows) {
          row_data <- first_ten_data[i, , drop = FALSE]

          # Calculate A, B, C values
          A_value <- sum(as.numeric(row_data[1, element_A_cols, drop = FALSE]), na.rm = TRUE)
          B_value <- sum(as.numeric(row_data[1, element_B_cols, drop = FALSE]), na.rm = TRUE)
          C_value <- sum(as.numeric(row_data[1, element_C_cols, drop = FALSE]), na.rm = TRUE)

          # Calculate ternary coordinates
          total <- A_value + B_value + C_value
          if (total > 0) {
            A_coord <- A_value / total
            B_coord <- B_value / total
            C_coord <- C_value / total
          } else {
            A_coord <- B_coord <- C_coord <- 0
          }

          # Calculate multivariate analysis values for this row
          multivariate_values <- c()

          # Mahalanobis Distance
          if (input$use_mahalanobis && !is.null(input$multivariate_columns) && length(input$multivariate_columns) > 1) {
            tryCatch({
              selected_cols <- intersect(input$multivariate_columns, names(row_data))
              if (length(selected_cols) > 1) {
                row_mahal_data <- row_data[1, selected_cols, drop = FALSE]
                # For single row, we need to use the full dataset for covariance calculation
                full_mahal_data <- rv$df1[, selected_cols, drop = FALSE]
                mahal_dist <- mahalanobis(row_mahal_data,
                                        colMeans(full_mahal_data, na.rm = TRUE),
                                        cov(full_mahal_data, use = "complete.obs"))
                multivariate_values <- c(multivariate_values, paste("MD =", round(mahal_dist, 2)))
              }
            }, error = function(e) {
              # Skip if calculation fails
            })
          }


          # Isolation Forest (simplified - would need actual model for proper calculation)
          if (input$use_isolation_forest && !is.null(input$multivariate_columns)) {
            multivariate_values <- c(multivariate_values, "IF = N/A (requires model)")
          }

          report_lines <- c(report_lines, paste("  Row", i, ":"))
          report_lines <- c(report_lines, paste("    • A (", paste(element_A_cols, collapse = "+"), "):", round(A_value, 3), "→", round(A_coord, 4)))
          report_lines <- c(report_lines, paste("    • B (", paste(element_B_cols, collapse = "+"), "):", round(B_value, 3), "→", round(B_coord, 4)))
          report_lines <- c(report_lines, paste("    • C (", paste(element_C_cols, collapse = "+"), "):", round(C_value, 3), "→", round(C_coord, 4)))
          report_lines <- c(report_lines, paste("    • Total:", round(total, 3), "| Ternary coordinates: A=", round(A_coord, 4), ", B=", round(B_coord, 4), ", C=", round(C_coord, 4)))

          # Add multivariate analysis values if available
          if (length(multivariate_values) > 0) {
            report_lines <- c(report_lines, paste("    • Multivariate:", paste(multivariate_values, collapse = ", ")))
          }
        }
      }

      # Calculate summary statistics for first 10 rows (selected columns only)
      if (first_ten_rows > 1) {
        # Use the same columns that were shown above
        selected_numeric_data <- first_ten_data[, columns_to_show, drop = FALSE]
        numeric_cols <- sapply(selected_numeric_data, is.numeric)

        if (sum(numeric_cols) > 0) {
          numeric_data <- selected_numeric_data[, numeric_cols, drop = FALSE]

          report_lines <- c(report_lines, "")
          report_lines <- c(report_lines, "Summary statistics for first 10 rows (Selected Columns):")
          report_lines <- c(report_lines, paste("  Mean values:", paste(sapply(numeric_data, function(x) round(mean(x, na.rm = TRUE), 3)), collapse = ", ")))
          report_lines <- c(report_lines, paste("  Std Dev:", paste(sapply(numeric_data, function(x) round(sd(x, na.rm = TRUE), 3)), collapse = ", ")))
          report_lines <- c(report_lines, paste("  Min values:", paste(sapply(numeric_data, function(x) round(min(x, na.rm = TRUE), 3)), collapse = ", ")))
          report_lines <- c(report_lines, paste("  Max values:", paste(sapply(numeric_data, function(x) round(max(x, na.rm = TRUE), 3)), collapse = ", ")))
        }
      }

      # Show selected analysis methods (without re-implementing the logic)
      selected_methods <- c()

      # Check which methods are selected
      if (input$use_mahalanobis) selected_methods <- c(selected_methods, "Mahalanobis Distance")
      if (input$use_isolation_forest) selected_methods <- c(selected_methods, "Isolation Forest")
      if (input$use_iqr_filter) selected_methods <- c(selected_methods, "IQR Filter")
      if (input$use_zscore_filter) selected_methods <- c(selected_methods, "Z-Score Filter")
      if (input$use_mad_filter) selected_methods <- c(selected_methods, "MAD Filter")

      if (length(selected_methods) > 0) {
        report_lines <- c(report_lines, "")
        report_lines <- c(report_lines, paste("Selected Analysis Methods:", paste(selected_methods, collapse = ", ")))
        report_lines <- c(report_lines, paste("Note: Actual filtering and analysis calculations are performed in the respective analysis modules"))
        report_lines <- c(report_lines, paste("(multivariate.R, statistical_filters.R, etc.) when plots are generated."))
      } else {
        report_lines <- c(report_lines, "")
        report_lines <- c(report_lines, "No analysis methods selected")
      }
    } else {
      report_lines <- c(report_lines, "No data available for first ten points calculation")
    }

    # Calculation Path Notes
    report_lines <- c(report_lines, "")
    report_lines <- c(report_lines, "=== CALCULATION PATH NOTES ===")
    report_lines <- c(report_lines, "1. Data Loading: Excel file loaded and validated (ternary_plot.R)")
    report_lines <- c(report_lines, "2. Individual Filters: Applied to Element A, B, C if specified (ternary_plot.R)")
    report_lines <- c(report_lines, "3. Optional Parameter Filters: Applied to point size and color parameters if specified (ternary_plot.R)")
    report_lines <- c(report_lines, "4. Statistical Filters: IQR, Z-Score, MAD applied to selected columns (statistical_filters.R)")
    report_lines <- c(report_lines, "5. Multivariate Analysis: Mahalanobis, Isolation Forest applied (multivariate.R)")
    report_lines <- c(report_lines, "6. Ternary Coordinates: Calculated as A/(A+B+C), B/(A+B+C), C/(A+B+C) (ternary_plot.R)")
    report_lines <- c(report_lines, "7. Plot Generation: Points plotted with optional parameters for size and color (ternary_plot.R)")
    report_lines <- c(report_lines, "8. File Output: Plot saved in selected format (PNG, JPEG, PDF, TIFF) (file_management.R)")

    return(paste(report_lines, collapse = "\n"))
  }

  # Render Analysis Report
  output$analysis_report <- renderText({
    req(input$use_mahalanobis || input$use_isolation_forest ||
        input$use_iqr_filter || input$use_zscore_filter || input$use_mad_filter)

    generate_analysis_report(input, rv)
  })

  # ---- Save Plot Buttons for Main Ternary Plots ----
  # Each hands the saved file straight to the browser's own Save dialog
  # (downloadButton/downloadHandler) instead of writing to a pre-chosen
  # server-side folder - see the vidternary Structural Audit's §03 for why
  # the previous global Working/Output Directory picker was removed.
  # general_ternary_plot() still needs a real output_dir to actually save
  # (preview = FALSE, output_dir = NULL would just draw and return NULL,
  # same as a live preview) - a fresh, single-use temp directory supplies
  # that; its contents are copied into the browser's download and then
  # left for the OS's normal temp-file cleanup, exactly like every other
  # downloadHandler in this app already does for its own generated files.

  # Save Plot 1
  output$plot1 <- downloadHandler(
    filename = function() paste0("Plot1_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", input$output_format %||% "png"),
    content = function(file) {
      if (is.null(input$xlsx_file1) || is.null(input$element_A1) || is.null(input$element_B1) || is.null(input$element_C1)) {
        stop("Please upload a file and select elements A, B, and C first.")
      }

      result <- tryCatch({
        params <- build_ternary_plot_params(1, FALSE)
        if (is.null(params)) stop("Invalid parameters for Plot 1.")

        params$output_dir <- tempfile("plot1_save_")
        dir.create(params$output_dir, recursive = TRUE)

        do.call(general_ternary_plot, params)
      }, error = function(e) {
        output$status <- renderText(paste("Error saving Plot 1:", e$message))
        log_operation("ERROR", "Failed to save Plot 1", e$message)
        stop(e$message)
      })

      if (is.null(result)) {
        output$status <- renderText("❌ Failed to save Plot 1")
        log_operation("ERROR", "Failed to save Plot 1")
        stop("Failed to save Plot 1 - see the Analysis Log.")
      }

      output$status <- renderText(paste("✅ Plot 1 saved successfully!\n📍 Location:", result))
      log_operation("SUCCESS", "Plot 1 saved successfully", paste("Saved to:", result))
      file.copy(result, file, overwrite = TRUE)
    }
  )

  # Save Plot 2
  output$plot2 <- downloadHandler(
    filename = function() paste0("Plot2_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", input$output_format %||% "png"),
    content = function(file) {
      if (is.null(input$xlsx_file2) || is.null(input$element_A2) || is.null(input$element_B2) || is.null(input$element_C2)) {
        stop("Please upload a file and select elements A, B, and C first.")
      }

      result <- tryCatch({
        params <- build_ternary_plot_params(2, FALSE)
        if (is.null(params)) stop("Invalid parameters for Plot 2.")

        params$output_dir <- tempfile("plot2_save_")
        dir.create(params$output_dir, recursive = TRUE)

        do.call(general_ternary_plot, params)
      }, error = function(e) {
        output$status <- renderText(paste("Error saving Plot 2:", e$message))
        log_operation("ERROR", "Failed to save Plot 2", e$message)
        stop(e$message)
      })

      if (is.null(result)) {
        output$status <- renderText("❌ Failed to save Plot 2")
        log_operation("ERROR", "Failed to save Plot 2")
        stop("Failed to save Plot 2 - see the Analysis Log.")
      }

      output$status <- renderText(paste("✅ Plot 2 saved successfully!\n📍 Location:", result))
      log_operation("SUCCESS", "Plot 2 saved successfully", paste("Saved to:", result))
      file.copy(result, file, overwrite = TRUE)
    }
  )

  # Save Both Plots - both files zipped into one download. Each plot's
  # save attempt keeps its own independent tryCatch (unchanged from
  # before this conversion): a real error from one can't prevent the
  # other from being attempted, so a genuine one-succeeds-one-fails
  # outcome still delivers the one that worked - as a one-file zip, with
  # an errors.txt alongside it naming what didn't, rather than discarding
  # the successful plot just because its sibling failed.
  output$plot_both <- downloadHandler(
    filename = function() paste0("TernaryPlots_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip"),
    content = function(file) {
      if (is.null(input$xlsx_file1) || is.null(input$xlsx_file2) ||
          is.null(input$element_A1) || is.null(input$element_B1) || is.null(input$element_C1) ||
          is.null(input$element_A2) || is.null(input$element_B2) || is.null(input$element_C2)) {
        stop("Please upload both files and select elements A, B, and C for each.")
      }

      out_dir <- tempfile("plot_both_save_")
      dir.create(out_dir, recursive = TRUE)

      plots_saved <- 0
      errors <- c()
      saved_files <- c()

      tryCatch({
        params1 <- build_ternary_plot_params(1, FALSE)
        if (!is.null(params1)) {
          params1$output_dir <- out_dir
          result1 <- do.call(general_ternary_plot, params1)
          if (!is.null(result1)) {
            plots_saved <- plots_saved + 1
            saved_files <- c(saved_files, result1)
          } else {
            errors <- c(errors, "Plot 1: Failed to save (see Analysis Log)")
          }
        } else {
          errors <- c(errors, "Plot 1: Invalid parameters")
        }
      }, error = function(e) {
        errors <<- c(errors, paste("Plot 1:", e$message))
      })

      tryCatch({
        params2 <- build_ternary_plot_params(2, FALSE)
        if (!is.null(params2)) {
          params2$output_dir <- out_dir
          result2 <- do.call(general_ternary_plot, params2)
          if (!is.null(result2)) {
            plots_saved <- plots_saved + 1
            saved_files <- c(saved_files, result2)
          } else {
            errors <- c(errors, "Plot 2: Failed to save (see Analysis Log)")
          }
        } else {
          errors <- c(errors, "Plot 2: Invalid parameters")
        }
      }, error = function(e) {
        errors <<- c(errors, paste("Plot 2:", e$message))
      })

      # One status message reflecting the actual combined outcome - same
      # convention as before this conversion: exactly one message per
      # outcome, always including any errors that actually occurred.
      if (plots_saved == 2) {
        status_msg <- paste("✅ Both plots saved successfully!\n📍 Locations:\n• Plot 1:", saved_files[1], "\n• Plot 2:", saved_files[2])
        log_operation("SUCCESS", "Both plots saved successfully", paste("Saved to:", paste(saved_files, collapse = "; ")))
      } else if (plots_saved == 1) {
        status_msg <- paste0("⚠️ One plot saved, one failed\n📍 Saved: ", saved_files[1], "\n❌ ", errors[1])
        log_operation("WARNING", "One plot saved, one failed", paste("Saved:", saved_files[1], "| Error:", errors[1]))
      } else {
        status_msg <- paste("❌ Failed to save both plots\n", paste(errors, collapse = "\n"))
        log_operation("ERROR", "Failed to save both plots", paste(errors, collapse = "; "))
      }
      output$status <- renderText(status_msg)

      if (plots_saved == 0) stop(paste("Failed to save both plots:", paste(errors, collapse = "; ")))
      if (length(errors) > 0) writeLines(errors, file.path(out_dir, "errors.txt"))

      zip::zip(file, files = list.files(out_dir), root = out_dir)
    }
  )

  # Return the ternary plot functions for integration
  return(list(
    # All functions are already set up as observeEvent and renderPlot
    # This function just sets up the event handlers
  ))
}
