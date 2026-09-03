# ---- Server Ternary Plots Module: Categorical Group Selection ----
# Split out of server_ternary_plots.R: detects when Optional Parameter 2
# holds categorical data and manages the group-selection checklist UI, for
# both Dataset 1 and Dataset 2.
#
# NOTE: prior to this split, generate_analysis_report() (in
# server_ternary_plots.R) was missing its closing brace, which meant this
# entire file's worth of code - the group-selection observers/outputs below -
# was accidentally nested inside that function's body and never executed
# (create_server_ternary_plots() never reached it). That's fixed as part of
# this refactor: register_ternary_plots_group_handlers() is now a real,
# reachable function called from create_server_ternary_plots().

#' Wire up categorical group-selection handling for the "Ternary Plots" tab
#'
#' Registers the Dataset 1/2 categorical-group detection observers (which
#' inspect whatever column is picked as Optional Parameter 2), the
#' group-selection checklist UI and its "Select All"/"Deselect All"
#' buttons, the group summary table, and the group-count display - all
#' duplicated per dataset (`_1`/`_2` suffixed inputs/outputs/`rv` fields).
#' Called from [create_server_ternary_plots()], sharing its
#' `moduleServer("ternary_plots", ...)` namespace and `rv`.
#'
#' @param input The Shiny `input` object.
#' @param output The Shiny `output` object.
#' @param session The Shiny session object.
#' @param rv The app's shared `reactiveValues` object (reads/writes
#'   `group_counts_1`/`_2`, `group_selections_1`/`_2`,
#'   `is_categorical_group_1`/`_2`).
#' @param show_message Function to show a user-facing status message.
#' @param log_operation Function to record a structured log entry.
#' @return Not meaningful (whatever its last statement happens to
#'   evaluate to, with no explicit `return()`) - called for its side
#'   effect of registering observers/outputs.
#' @export
register_ternary_plots_group_handlers <- function(input, output, session, rv, show_message, log_operation) {

  # ---- Group Selection Management for Dataset 1 ----

  # Detect categorical groups for Dataset 1
  observeEvent(input$optional_param2_1, {
    if (!is.null(input$optional_param2_1) && input$optional_param2_1 != "" && !is.null(rv$df1)) {
      group_column <- input$optional_param2_1
      data <- rv$df1

      if (group_column %in% names(data)) {
        # Check if column is categorical - more robust detection
        column_data <- data[[group_column]]
        is_categorical <- is.character(column_data) || is.factor(column_data) ||
                         (!is.numeric(column_data) && length(unique(column_data)) <= 50)

        # Debug output
        cat("DEBUG: Group column:", group_column, "\n")
        cat("DEBUG: Column data type:", class(column_data), "\n")
        cat("DEBUG: Unique values count:", length(unique(column_data)), "\n")
        cat("DEBUG: Is categorical:", is_categorical, "\n")
        cat("DEBUG: Sample values:", paste(head(unique(column_data)), collapse = ", "), "\n")

        rv$is_categorical_group_1 <- is_categorical

        if (is_categorical) {
          # Get unique groups and counts
          group_counts <- table(data[[group_column]])
          group_counts <- sort(group_counts, decreasing = TRUE) # Sort by frequency

          # Create choices with counts
          choices <- paste0(names(group_counts), " (", group_counts, " samples)")
          names(choices) <- names(group_counts)

          # Use persistent selections or empty
          selected <- rv$group_selections_1 %||% character(0)

          # Update UI
          updateCheckboxGroupInput(session, "selected_groups_1",
                                  choices = choices, selected = selected)

          # Store counts for display
          rv$group_counts_1 <- group_counts

          cat("DEBUG: Group counts:", paste(names(group_counts), collapse = ", "), "\n")
        }
      }
    } else {
      rv$is_categorical_group_1 <- FALSE
    }
  })

  # Save selections for Dataset 1
  observeEvent(input$selected_groups_1, {
    rv$group_selections_1 <- input$selected_groups_1
  })

  # Select All/Deselect All for Dataset 1
  observeEvent(input$select_all_groups_1, {
    if (!is.null(rv$group_counts_1)) {
      # checkboxGroupInput's choices (built above) use names(group_counts) as
      # the displayed LABEL but the "Name (N samples)" string as the actual
      # submitted VALUE - updateCheckboxGroupInput(selected=) has to match
      # against that value side, not the label, or nothing gets selected.
      # Reconstructed here rather than reading it back from `choices` since
      # that local variable isn't itself persisted in rv.
      all_groups <- paste0(names(rv$group_counts_1), " (", rv$group_counts_1, " samples)")
      updateCheckboxGroupInput(session, "selected_groups_1", selected = all_groups)
    }
  })

  observeEvent(input$deselect_all_groups_1, {
    updateCheckboxGroupInput(session, "selected_groups_1", selected = character(0))
  })

  # Group count display for Dataset 1
  output$group_count_1 <- renderText({
    if (!is.null(rv$group_counts_1) && !is.null(input$selected_groups_1)) {
      total_groups <- length(rv$group_counts_1)
      selected_count <- length(input$selected_groups_1)
      paste("Showing", selected_count, "of", total_groups, "groups")
    }
  })

  # Group summary table for Dataset 1
  output$group_summary_1 <- renderTable({
    if (!is.null(rv$group_counts_1)) {
      # Create a data frame with group names and counts
      summary_df <- data.frame(
        Group = names(rv$group_counts_1),
        Samples = as.numeric(rv$group_counts_1),
        Percentage = round(as.numeric(rv$group_counts_1) / sum(rv$group_counts_1) * 100, 1),
        stringsAsFactors = FALSE
      )

      # Sort by sample count (descending)
      summary_df <- summary_df[order(summary_df$Samples, decreasing = TRUE), ]

      # Add percentage column
      summary_df$Percentage <- paste0(summary_df$Percentage, "%")

      # Limit to top 10 groups if there are many
      if (nrow(summary_df) > 10) {
        summary_df <- summary_df[1:10, ]
        summary_df <- rbind(summary_df,
                           data.frame(Group = "...", Samples = "...", Percentage = "...", stringsAsFactors = FALSE))
      }

      summary_df
    }
  }, striped = TRUE, hover = TRUE, bordered = TRUE,
     caption = "Sample counts by group (sorted by frequency)")

  # Dynamic UI for group selection Dataset 1
  output$group_selection_ui_1 <- renderUI({
    if (!is.null(input$optional_param2_1) && input$optional_param2_1 != "" &&
        !is.null(rv$is_categorical_group_1) && rv$is_categorical_group_1) {
      tagList(
        hr(),
        h6("Select Groups to Display (Dataset 1):"),

        # Group summary table
        div(style = "margin-bottom: 10px; padding: 8px; background-color: #e9ecef; border-radius: 4px;",
          h6("Group Summary:", style = "margin-top: 0; margin-bottom: 8px; color: #495057;"),
          tableOutput(session$ns("group_summary_1"))
        ),

        div(style = "max-height: 200px; overflow-y: auto; border: 1px solid #ccc; padding: 10px; background-color: #f8f9fa;",
          checkboxGroupInput(session$ns("selected_groups_1"), "",
                            choices = NULL, # Populated dynamically
                            selected = NULL) # User chooses
        ),
        div(style = "margin-top: 5px;",
          actionButton(session$ns("select_all_groups_1"), "Select All", class = "btn-sm btn-outline-primary"),
          actionButton(session$ns("deselect_all_groups_1"), "Deselect All", class = "btn-sm btn-outline-secondary")
        ),
        div(style = "font-size: 12px; color: #666; margin-top: 5px;",
            textOutput(session$ns("group_count_1")))
      )
    }
  })

  # ---- Group Selection Management for Dataset 2 ----

  # Detect categorical groups for Dataset 2
  observeEvent(input$optional_param2_2, {
    if (!is.null(input$optional_param2_2) && input$optional_param2_2 != "" && !is.null(rv$df2)) {
      group_column <- input$optional_param2_2
      data <- rv$df2

      if (group_column %in% names(data)) {
        # Check if column is categorical - more robust detection
        column_data <- data[[group_column]]
        is_categorical <- is.character(column_data) || is.factor(column_data) ||
                         (!is.numeric(column_data) && length(unique(column_data)) <= 50)
        rv$is_categorical_group_2 <- is_categorical

        if (is_categorical) {
          # Get unique groups and counts
          group_counts <- table(data[[group_column]])
          group_counts <- sort(group_counts, decreasing = TRUE) # Sort by frequency

          # Create choices with counts
          choices <- paste0(names(group_counts), " (", group_counts, " samples)")
          names(choices) <- names(group_counts)

          # Use persistent selections or empty
          selected <- rv$group_selections_2 %||% character(0)

          # Update UI
          updateCheckboxGroupInput(session, "selected_groups_2",
                                  choices = choices, selected = selected)

          # Store counts for display
          rv$group_counts_2 <- group_counts
        }
      }
    } else {
      rv$is_categorical_group_2 <- FALSE
    }
  })

  # Save selections for Dataset 2
  observeEvent(input$selected_groups_2, {
    rv$group_selections_2 <- input$selected_groups_2
  })

  # Select All/Deselect All for Dataset 2
  observeEvent(input$select_all_groups_2, {
    if (!is.null(rv$group_counts_2)) {
      # Same fix as Dataset 1's handler above - see that comment.
      all_groups <- paste0(names(rv$group_counts_2), " (", rv$group_counts_2, " samples)")
      updateCheckboxGroupInput(session, "selected_groups_2", selected = all_groups)
    }
  })

  observeEvent(input$deselect_all_groups_2, {
    updateCheckboxGroupInput(session, "selected_groups_2", selected = character(0))
  })

  # Group count display for Dataset 2
  output$group_count_2 <- renderText({
    if (!is.null(rv$group_counts_2) && !is.null(input$selected_groups_2)) {
      total_groups <- length(rv$group_counts_2)
      selected_count <- length(input$selected_groups_2)
      paste("Showing", selected_count, "of", total_groups, "groups")
    }
  })

  # Group summary table for Dataset 2
  output$group_summary_2 <- renderTable({
    if (!is.null(rv$group_counts_2)) {
      # Create a data frame with group names and counts
      summary_df <- data.frame(
        Group = names(rv$group_counts_2),
        Samples = as.numeric(rv$group_counts_2),
        Percentage = round(as.numeric(rv$group_counts_2) / sum(rv$group_counts_2) * 100, 1),
        stringsAsFactors = FALSE
      )

      # Sort by sample count (descending)
      summary_df <- summary_df[order(summary_df$Samples, decreasing = TRUE), ]

      # Add percentage column
      summary_df$Percentage <- paste0(summary_df$Percentage, "%")

      # Limit to top 10 groups if there are many
      if (nrow(summary_df) > 10) {
        summary_df <- summary_df[1:10, ]
        summary_df <- rbind(summary_df,
                           data.frame(Group = "...", Samples = "...", Percentage = "...", stringsAsFactors = FALSE))
      }

      summary_df
    }
  }, striped = TRUE, hover = TRUE, bordered = TRUE,
     caption = "Sample counts by group (sorted by frequency)")

  # Dynamic UI for group selection Dataset 2
  output$group_selection_ui_2 <- renderUI({
    if (!is.null(input$optional_param2_2) && input$optional_param2_2 != "" &&
        !is.null(rv$is_categorical_group_2) && rv$is_categorical_group_2) {
      tagList(
        hr(),
        h6("Select Groups to Display (Dataset 2):"),

        # Group summary table
        div(style = "margin-bottom: 10px; padding: 8px; background-color: #e9ecef; border-radius: 4px;",
          h6("Group Summary:", style = "margin-top: 0; margin-bottom: 8px; color: #495057;"),
          tableOutput(session$ns("group_summary_2"))
        ),

        div(style = "max-height: 200px; overflow-y: auto; border: 1px solid #ccc; padding: 10px; background-color: #f8f9fa;",
          checkboxGroupInput(session$ns("selected_groups_2"), "",
                            choices = NULL, # Populated dynamically
                            selected = NULL) # User chooses
        ),
        div(style = "margin-top: 5px;",
          actionButton(session$ns("select_all_groups_2"), "Select All", class = "btn-sm btn-outline-primary"),
          actionButton(session$ns("deselect_all_groups_2"), "Deselect All", class = "btn-sm btn-outline-secondary")
        ),
        div(style = "font-size: 12px; color: #666; margin-top: 5px;",
            textOutput(session$ns("group_count_2")))
      )
    }
  })
}
