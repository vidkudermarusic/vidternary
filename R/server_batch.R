# ---- Server: "Batch Comparison" tab ----
# Wires R/batch_pipeline.R's run_batch_comparison() into the app: one
# uploaded file per batch, column choices from the columns the files
# share, pre-analysis filter and the Compare button. The outputs are
# registered in server_batch_outputs.R.
#
# NOTE: shiny::validate()/need() must stay fully qualified here - see
# server_spatial.R header comment for why.

# Inspected areas typed as text ("1.9" or "1.9, 1.9, 2.4"): numeric vector,
# or NULL when empty.
parse_inspected_areas <- function(text) {
  if (is.null(text) || is.na(text) || !nzchar(trimws(text))) return(NULL)
  parts <- strsplit(trimws(text), "[,;[:space:]]+")[[1]]
  parts <- parts[nzchar(parts)]
  values <- suppressWarnings(as.numeric(parts))
  if (length(values) == 0 || anyNA(values) || any(values <= 0)) {
    stop("Inspected areas must be positive numbers with a decimal point, separated by commas, e.g. 1.9, 1.9, 2.4.", call. = FALSE)
  }
  values
}

# Batch labels from file names: without extension, made unique.
batch_labels <- function(file_names) make.unique(tools::file_path_sans_ext(basename(file_names)), sep = " #")

#' Wire up the Batch Comparison tab's server logic
#'
#' @param input The Shiny `input` object.
#' @param output The Shiny `output` object.
#' @param session The Shiny session object.
#' @param rv The app's shared `reactiveValues` object.
#' @param show_message Function to show a user-facing status message.
#' @param log_operation Function to record a structured log entry.
#' @return A list with `module_name`.
#' @export
create_server_batch <- function(input, output, session, rv, show_message, log_operation) {

  # One data frame per uploaded file, named by batch; unreadable files are
  # reported, not silently dropped.
  uploaded <- reactive({
    req(input$batch_files)
    files <- input$batch_files
    tables <- lapply(seq_len(nrow(files)), function(i) {
      tryCatch(read_particle_table(files$datapath[i], name = files$name[i]), error = function(e) conditionMessage(e))
    })
    failed <- vapply(tables, is.character, logical(1))
    shiny::validate(shiny::need(!any(failed), paste("Could not read:", paste(sprintf("%s (%s)", files$name[failed], unlist(tables[failed])), collapse = "; "))))
    names(tables) <- batch_labels(files$name)
    tables
  })
  common_columns <- reactive(Reduce(intersect, lapply(uploaded(), names)))

  observe({
    tables <- tryCatch(uploaded(), error = function(e) NULL)
    if (is.null(tables)) return()
    cols <- common_columns()
    numeric_cols <- cols[vapply(cols, function(cn) all(vapply(tables, function(d) is.numeric(d[[cn]]), logical(1))), logical(1))]
    optional <- function(v) c("None" = "none", v)
    updateSelectInput(session, "batch_area_col", choices = numeric_cols, selected = first_match_or_null(numeric_cols, "^area"))
    updateSelectInput(session, "batch_field_col", choices = optional(cols),
                      selected = first_match_or_null(cols, "field|frame|fov") %||% "none")
    x_col <- first_match_or_null(numeric_cols, "stage.?x|^x")
    updateSelectInput(session, "batch_x_col", choices = optional(numeric_cols), selected = x_col %||% "none")
    updateSelectInput(session, "batch_y_col", choices = optional(numeric_cols),
                      selected = first_match_or_null(numeric_cols, "stage.?y|^y") %||% "none")
    if (!is.null(x_col)) updateSelectInput(session, "batch_coord_unit", selected = if (grepl("um\\)|\u00b5m", x_col)) "um" else "mm")
    element_cols <- tryCatch(inclusion_element_columns(numeric_cols), error = function(e) character(0))
    candidates <- unname(element_cols[!names(element_cols) %in% c("C", "N", "O")])
    pooled <- do.call(rbind, lapply(tables, function(d) d[, candidates, drop = FALSE]))
    updateSelectizeInput(session, "batch_parts", choices = unname(element_cols),
                         selected = if (length(candidates) >= 2) coda_default_parts(pooled, candidates) else character(0))
    updateSelectizeInput(session, "batch_filter_cols", choices = numeric_cols)
  })

  output$batch_file_info <- renderText({
    tables <- uploaded()
    paste(sprintf("%s: %d rows", names(tables), vapply(tables, nrow, integer(1))), collapse = "\n")
  })
  output$batch_filter_inputs <- renderUI(render_pre_filter_inputs(session$ns, "batch", input$batch_filter_cols))

  result <- eventReactive(input$batch_run, {
    all_batches <- uploaded()
    shiny::validate(shiny::need(length(all_batches) >= 2, "Upload at least 2 files (one per batch) to compare."))
    filters <- collect_pre_filters(input, "batch", input$batch_filter_cols)
    batches <- tryCatch(lapply(all_batches, apply_pre_filters, filters = filters),
                        error = function(e) shiny::validate(paste("Pre-analysis filter error:", e$message)))
    empty <- names(batches)[vapply(batches, nrow, integer(1)) == 0]
    shiny::validate(shiny::need(length(empty) == 0, paste0("No rows remain after the pre-analysis filter(s) in: ", paste(empty, collapse = ", "), ".")))
    req(input$batch_area_col)

    optional <- function(v) if (is.null(v) || identical(v, "none") || !nzchar(v)) NULL else v
    cols <- list(area = input$batch_area_col, field = optional(input$batch_field_col),
                 x = optional(input$batch_x_col), y = optional(input$batch_y_col),
                 parts = if (length(input$batch_parts) >= 2) input$batch_parts else NULL)
    shiny::validate(shiny::need(length(input$batch_parts) != 1, "Choose two or more elements to compare, or none."))
    num_or <- function(v, default) if (is.null(v) || !is.finite(v)) default else v
    settings <- tryCatch(list(
      coord_unit = input$batch_coord_unit %||% "mm",
      inspected_area_mm2 = parse_inspected_areas(input$batch_inspected_area),
      n_perm = num_or(input$batch_n_perm, 999),
      fields_per_area = num_or(input$batch_fields_per_area, 1),
      return_period = num_or(input$batch_return_period, 100)
    ), error = function(e) shiny::validate(e$message))

    res <- tryCatch(run_batch_comparison(batches, cols, settings, all_batches = all_batches),
                    error = function(e) shiny::validate(paste("Batch comparison failed:", e$message)))
    res$cols <- cols
    res$filters <- filters
    res$files <- list(name = input$batch_files$name, datapath = input$batch_files$datapath,
                      rows = vapply(all_batches, nrow, integer(1)))
    log_operation("INFO", "Batch comparison", sprintf("%d batches: %s", length(batches), paste(names(batches), collapse = ", ")))
    res
  })

  register_batch_outputs(input, output, result)
  register_settings_io(input, output, session, tab = "batch", prefix = "batch")

  return(list(
    module_name = "server_batch"
  ))
}
