# ---- Server: "Steel Cleanliness" tab outputs ----
# Tables, plots and downloads for the result of run_cleanliness_analysis().
# Split from server_cleanliness.R for size; called once from there.

# Display versions of the result tables: readable headers, rounded numbers.
cleanliness_display_tables <- function(res) {
  rating <- function(d, with_family = FALSE) {
    if (is.null(d) || nrow(d) == 0) return(data.frame(Note = "No rated inclusions."))
    out <- data.frame(
      Series = d$series,
      `Worst-field index (method A)` = sprintf("%.1f", d$worst_index),
      `Worst-field value` = ifelse(d$worst_value > 0, sprintf("%.4g %s", d$worst_value, d$unit), "-"),
      `Worst field` = ifelse(is.na(d$worst_field), "-",
                             paste0(d$worst_field, ifelse(d$worst_field_complete %in% FALSE, " (incomplete)", ""))),
      `Mean index (method B)` = sprintf("%.2f", d$mean_index),
      `Fields with index >= 0.5` = sprintf("%d of %d", d$fields_rated, d$n_fields),
      `Rated objects` = d$n_objects,
      `Oversize` = d$n_oversize,
      check.names = FALSE, stringsAsFactors = FALSE)
    if (with_family) out <- cbind(Family = d$family, out, stringsAsFactors = FALSE)
    out
  }
  ind <- res$indices
  indices <- data.frame(
    Group = ind$group, Particles = ind$n,
    `Per mm2` = sprintf("%.1f", ind$per_mm2),
    `95% interval (Poisson)` = sprintf("[%.1f, %.1f]", ind$per_mm2_lower, ind$per_mm2_upper),
    `Area fraction (ppm)` = sprintf("%.1f", ind$area_ppm),
    `95% interval (bootstrap over fields)` = ifelse(is.na(ind$area_ppm_lower), "needs a field ID column",
                                                    sprintf("[%.1f, %.1f]", ind$area_ppm_lower, ind$area_ppm_upper)),
    `Mean ECD (um)` = sprintf("%.2f", ind$mean_ecd_um),
    `Largest ECD (um)` = sprintf("%.2f", ind$max_ecd_um),
    `Largest sqrt(area) (um)` = sprintf("%.2f", ind$max_sqrt_area_um),
    check.names = FALSE, stringsAsFactors = FALSE)

  sc <- res$size_classes
  groups <- unique(sc$group)
  sizes <- data.frame(`ECD (um)` = levels(sc$size_class), check.names = FALSE, stringsAsFactors = FALSE)
  for (g in groups) sizes[[g]] <- sc$n[sc$group == g]

  s <- res$stereology
  stereology <- data.frame(
    Group = s$group, Sections = s$n,
    `Per mm2` = sprintf("%.1f", s$per_mm2),
    `Per mm3 (Fullman)` = sprintf("%.3g", s$per_mm3),
    `Mean section diameter (um)` = sprintf("%.2f", s$mean_section_um),
    `Mean sphere diameter (um)` = sprintf("%.2f", s$mean_sphere_um),
    `Volume fraction (ppm)` = sprintf("%.1f", s$volume_ppm),
    check.names = FALSE, stringsAsFactors = FALSE)

  saltykov <- if (is.null(res$saltykov)) data.frame(Note = "Not enough particles for the unfolding.") else data.frame(
    `Sphere diameter (um)` = sprintf("%.2f - %.2f", res$saltykov$d_lower, res$saltykov$d_upper),
    Sections = res$saltykov$n_sections,
    `Sections per mm2` = sprintf("%.2f", res$saltykov$per_mm2),
    `Spheres per mm3` = sprintf("%.4g", res$saltykov$per_mm3),
    check.names = FALSE, stringsAsFactors = FALSE)

  ph <- res$mass_balance$phases
  phases <- if (nrow(ph) == 0) data.frame(Note = "No family with a known phase.") else data.frame(
    Family = ph$family, `Taken as` = ph$compound, `Density (g/cm3)` = ph$density_g_cm3,
    `Volume (ppm)` = sprintf("%.1f", ph$volume_ppm), `Mass (ppm)` = sprintf("%.1f", ph$mass_ppm),
    check.names = FALSE, stringsAsFactors = FALSE)
  el <- res$mass_balance$elements
  elements <- if (nrow(el) == 0) data.frame(Note = "No family with a known phase.") else data.frame(
    Element = el$element, `Bound in inclusions (mass ppm)` = sprintf("%.1f", el$bound_ppm),
    `Bulk analysis (mass ppm)` = ifelse(is.na(el$measured_ppm), "-", sprintf("%.1f", el$measured_ppm)),
    `Bound share of bulk` = ifelse(is.na(el$bound_share), "-", sprintf("%.0f %%", 100 * el$bound_share)),
    check.names = FALSE, stringsAsFactors = FALSE)

  o <- res$objects[res$objects$n_members > 1, , drop = FALSE]
  stringers <- if (nrow(o) == 0) data.frame(Note = "No particles lie close enough in line to be joined.") else data.frame(
    Family = o$family, Type = o$type, Series = o$series, Particles = o$n_members,
    Stringer = ifelse(o$is_stringer, "yes", "no (pair)"),
    `Length (um)` = sprintf("%.1f", o$length), `Width (um)` = sprintf("%.1f", o$width),
    `Rating field` = o$field, `X (mm)` = sprintf("%.3f", o$x / 1000), `Y (mm)` = sprintf("%.3f", o$y / 1000),
    check.names = FALSE, stringsAsFactors = FALSE)

  rel <- res$reliability
  reliability <- data.frame(
    `Composition from EDS` = c("matrix-dominated: smaller than the electron range",
                               "partly matrix: 1 to 2 electron ranges", "reliable: larger"),
    Particles = rel$n, `Share (%)` = sprintf("%.1f", rel$share_pct),
    `Median matrix element (wt%)` = ifelse(is.na(rel$median_matrix_pct), "-", sprintf("%.1f", rel$median_matrix_pct)),
    check.names = FALSE, stringsAsFactors = FALSE)
  shares <- function(d, label) {
    if (is.null(d) || nrow(d) == 0) return(data.frame(Note = "No particles to count."))
    out <- data.frame(Class = d$class, Particles = d$n, `Share (%)` = sprintf("%.1f", d$share_pct),
                      `95% interval (%)` = sprintf("[%.1f, %.1f]", d$lower_pct, d$upper_pct), Interval = d$method,
                      check.names = FALSE, stringsAsFactors = FALSE)
    names(out)[1] <- label
    out
  }

  list(rating = rating(res$rating_summary), rating_family = rating(res$rating_by_family, TRUE),
       indices = indices, sizes = sizes, reliability = reliability,
       family_shares = shares(res$family_shares, "Chemical family"), type_shares = shares(res$type_shares, "Type"),
       stereology = stereology, saltykov = saltykov,
       phases = phases, elements = elements, stringers = stringers)
}

# Registers the Steel Cleanliness tab's outputs. `result` is the
# eventReactive from create_server_cleanliness().
register_cleanliness_outputs <- function(input, output, result) {
  placeholder <- "Upload a file, check the columns, and click \"Run Cleanliness Analysis\"."
  download_placeholder <- "Upload a file and click \"Run Cleanliness Analysis\" before downloading."
  tables <- reactive(cleanliness_display_tables(result()))

  output$clean_status <- renderText({
    render_gated_status(input, "clean_run", placeholder, function() {
      res <- result()
      sprintf("%s: %d particles analysed (%d rows uploaded), %d rated objects in %d rating fields (%d complete), %d stringers. Inspected area %.2f mm2.",
              res$standard$name, nrow(res$particles), res$n_rows_before_filter, sum(res$objects$rated),
              nrow(res$grid$fields), sum(res$grid$fields$complete),
              sum(res$objects$is_stringer & res$objects$rated), res$inspected_area_mm2)
    })
  })

  # Supplementary boxes: nothing shown without a result (the status line
  # already carries any validation message).
  output$clean_notes <- renderUI({
    res <- tryCatch(result(), error = function(e) NULL)
    if (is.null(res)) return(NULL)
    tagList(
      lapply(res$notes$warnings, function(msg)
        div(style = "color: #856404; background-color: #fff3cd; border: 1px solid #ffeeba; border-radius: 4px; padding: 10px; font-size: 13px; margin: 10px 0;",
            strong("Check: "), msg)),
      tags$ul(style = "font-size: 12px; margin: 6px 0;", lapply(res$notes$notes, tags$li))
    )
  })

  output$clean_rating_table <- renderTable(tables()$rating, align = "l")
  output$clean_rating_family_table <- renderTable(tables()$rating_family, align = "l")
  output$clean_stringer_table <- renderTable(tables()$stringers, align = "l")
  output$clean_indices_table <- renderTable(tables()$indices, align = "l")
  output$clean_size_table <- renderTable(tables()$sizes, align = "l")
  output$clean_reliability_table <- renderTable(tables()$reliability, align = "l")
  output$clean_family_shares_table <- renderTable(tables()$family_shares, align = "l")
  output$clean_type_shares_table <- renderTable(tables()$type_shares, align = "l")
  output$clean_stereology_table <- renderTable(tables()$stereology, align = "l")
  output$clean_saltykov_table <- renderTable(tables()$saltykov, align = "l")
  output$clean_mass_phase_table <- renderTable(tables()$phases, align = "l")
  output$clean_mass_element_table <- renderTable(tables()$elements, align = "l")

  output$clean_map_plot <- renderPlot(print(create_stringer_map(result())))
  output$clean_rose_plot <- renderPlot({
    p <- result()$particles
    shiny::validate(shiny::need("direction" %in% names(p), "Choose a direction column to see the rose diagram."))
    rated <- !is.na(p$object)
    shiny::validate(shiny::need(any(rated), "No rated particles."))
    print(create_direction_rose(p$direction[rated], p$length[rated], result()$settings$axis_angle))
  })
  output$clean_size_plot <- renderPlot(print(create_size_class_plot(result()$size_classes)))
  output$clean_saltykov_plot <- renderPlot({
    shiny::validate(shiny::need(!is.null(result()$saltykov), "Not enough particles for the unfolding."))
    print(create_saltykov_plot(result()$saltykov))
  })

  output$clean_download_tables <- downloadHandler(
    filename = function() paste0("steel_cleanliness_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"),
    content = function(file) {
      res <- safe_reactive_result(function() result(), download_placeholder)
      writexl::write_xlsx(cleanliness_result_tables(res), file)
    }
  )
  output$clean_download_map <- downloadHandler(
    filename = function() paste0("steel_cleanliness_map_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      res <- safe_reactive_result(function() result(), download_placeholder)
      ggplot2::ggsave(file, plot = create_stringer_map(res), width = 10, height = 7, dpi = 300)
    }
  )
  script_lines <- function(res) cleanliness_script(res$file$name %||% "data.xlsx", res$cols, res$settings, res$filters)
  output$clean_download_report <- downloadHandler(
    filename = function() paste0("steel_cleanliness_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html"),
    content = function(file) {
      res <- safe_reactive_result(function() result(), download_placeholder)
      provenance <- report_provenance(res$file$name %||% "data", res$file$datapath, res$n_rows_before_filter)
      writeLines(cleanliness_report(res, provenance, script_lines(res), res$filters), file, useBytes = TRUE)
    }
  )
  output$clean_download_script <- downloadHandler(
    filename = function() paste0("steel_cleanliness_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".R"),
    content = function(file) {
      res <- safe_reactive_result(function() result(), download_placeholder)
      writeLines(enc2utf8(script_lines(res)), file, useBytes = TRUE)
    }
  )
  invisible(NULL)
}
