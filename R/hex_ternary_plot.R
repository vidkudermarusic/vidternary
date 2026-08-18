# ---- Hexagonal Joint Ternary Diagram ----
# Composites 6 triangular ternary plots (sharing a common central element/
# combination) into one hexagonal image. Ported from the sibling
# "Kode za objavo/hexagonal_ternary_plot/create_joint_ternary_diagram.R"
# script, with two bugs fixed there already carried over here:
#   1. `working_dir` must always be matched by name in the call - R's
#      positional-argument matching otherwise silently binds the first
#      element string to it instead of `...`, since it precedes `...` in
#      the signature.
#   2. `file_base` is computed unconditionally up front (it used to only
#      exist when `output_dir` was NULL, so passing an explicit output_dir
#      crashed the composite-title step every time).
#
# Reuses the same Ternary::TernaryPlot/TernaryPoints call pattern already
# used elsewhere in this app (see ternary_plot_preview.R), just without the
# statistical-filter/optional-parameter machinery of the main ternary tab -
# this feature only needs raw element selection and plotting.

create_hex_ternary_diagram <- function(xlsx_file, output_dir, working_dir = NULL, ...) {
  if (!requireNamespace("Ternary", quietly = TRUE)) stop("Ternary package is required.")
  if (!requireNamespace("magick", quietly = TRUE)) stop("magick package is required.")
  if (!requireNamespace("openxlsx", quietly = TRUE)) stop("openxlsx package is required.")
  if (!requireNamespace("png", quietly = TRUE)) stop("png package is required.")

  if (!is.null(working_dir)) setwd(working_dir)

  clean_label <- function(x) gsub("\\.\\(Wt%\\)", "", x)

  el_raw <- list(...)
  if (length(el_raw) != 7) {
    stop("You must provide exactly 7 element strings (A..G).")
  }
  element_sets <- lapply(el_raw, function(s) trimws(unlist(strsplit(s, "\\+"))))

  cfg <- element_sets
  element_configs <- list(
    list(A = cfg[[1]], B = cfg[[2]], C = cfg[[3]]),
    list(A = cfg[[3]], B = cfg[[4]], C = cfg[[1]]),
    list(A = cfg[[4]], B = cfg[[3]], C = cfg[[5]]),
    list(A = cfg[[6]], B = cfg[[5]], C = cfg[[3]]),
    list(A = cfg[[3]], B = cfg[[7]], C = cfg[[6]]),
    list(A = cfg[[7]], B = cfg[[3]], C = cfg[[2]])
  )

  element_labels <- vapply(cfg, function(x) paste(clean_label(x), collapse = "+"), character(1))
  all_elements <- unique(unlist(cfg))
  all_symbols <- gsub("\\..*", "", all_elements)
  elements_labels <- paste(all_symbols, collapse = ",")
  elements_labels_safe <- gsub("[^A-Za-z0-9]", "_", elements_labels)

  M <- openxlsx::read.xlsx(xlsx_file, sheet = 1)
  file_base <- gsub("\\.xlsx$", "", basename(xlsx_file))
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  custom_folder <- output_dir

  cut_triangle <- function(infile, outfile = infile) {
    img <- magick::image_read(infile)
    w <- magick::image_info(img)$width
    h <- magick::image_info(img)$height
    h_tri <- h * sqrt(3) / 2
    y_base <- (h - h_tri) / 2
    y_apex <- (h + h_tri) / 2
    maskfile <- tempfile(fileext = ".png")
    grDevices::png(maskfile, width = w, height = h, bg = "black")
    graphics::par(mar = rep(0, 4))
    graphics::plot.new(); graphics::plot.window(xlim = c(0, w), ylim = c(0, h), asp = 1)
    graphics::polygon(x = c(w / 2, 0, w), y = c(y_apex, y_base, y_base), col = "white", border = NA)
    grDevices::dev.off()
    mask <- magick::image_read(maskfile)
    unlink(maskfile)
    img_tri <- magick::image_composite(img, mask, operator = "copyopacity")
    img_trim <- magick::image_trim(img_tri)
    magick::image_write(img_trim, outfile)
    invisible(outfile)
  }

  plot_ternary <- function(M, elements_A, elements_B, elements_C, custom_folder, plot_num) {
    all_selected_elements <- c(elements_A, elements_B, elements_C)
    if (!all(all_selected_elements %in% colnames(M))) {
      missing <- setdiff(all_selected_elements, colnames(M))
      stop(paste("Column(s) missing in Excel file:", paste(missing, collapse = ", ")))
    }
    matrika <- M[, all_selected_elements, drop = FALSE]
    row_sums <- rowSums(matrika, na.rm = TRUE)
    matrika <- matrika[row_sums > 0, , drop = FALSE]
    matrika <- na.omit(matrika)
    matrika <- as.matrix(matrika)
    ternary_data <- data.frame(
      A = if (length(elements_A) == 1) matrika[, elements_A] else rowSums(matrika[, elements_A, drop = FALSE], na.rm = TRUE),
      B = if (length(elements_B) == 1) matrika[, elements_B] else rowSums(matrika[, elements_B, drop = FALSE], na.rm = TRUE),
      C = if (length(elements_C) == 1) matrika[, elements_C] else rowSums(matrika[, elements_C, drop = FALSE], na.rm = TRUE)
    )
    clean_labels_A <- paste(clean_label(elements_A), collapse = "+")
    clean_labels_B <- paste(clean_label(elements_B), collapse = "+")
    clean_labels_C <- paste(clean_label(elements_C), collapse = "+")
    plot_title <- paste0(plot_num, " Ternary Plot of ", clean_labels_A, ", ", clean_labels_B, ", ", clean_labels_C, " (charge ", file_base, ")")
    file_name <- paste0(gsub("[^A-Za-z0-9]", "_", plot_title), ".png")
    file_path <- normalizePath(file.path(custom_folder, file_name), winslash = "/", mustWork = FALSE)
    grDevices::png(file_path, width = 800, height = 800, bg = "transparent")
    graphics::par(mar = c(0, 0, 0, 0))
    Ternary::TernaryPlot(
      atip = NULL, btip = NULL, ctip = NULL,
      alab = NULL, blab = NULL, clab = NULL,
      col = "lightgrey",
      grid.lines = 5,
      grid.lty = "dotted",
      grid.minor.lines = 10,
      grid.minor.lty = "dotted",
      padding = 0,
      axis.labels = FALSE
    )
    Ternary::TernaryPoints(ternary_data, type = "p", cex = 1, pch = 20, col = "black")
    grDevices::dev.off()
    cut_triangle(file_path)
  }

  for (i in seq_along(element_configs)) {
    config <- element_configs[[i]]
    plot_ternary(M, elements_A = config$A, elements_B = config$B, elements_C = config$C,
                 custom_folder = custom_folder, plot_num = i)
  }

  png_files <- list.files(custom_folder, pattern = "\\.png$", full.names = TRUE)
  if (length(png_files) < 6) {
    warning("Not enough PNG files found to create a hexagonal composite plot.")
    return(NULL)
  }

  file_ctimes <- file.info(png_files)$ctime
  png_files_sorted <- png_files[order(file_ctimes, decreasing = FALSE)]
  composite_path <- file.path(custom_folder, paste0("Hexagonal_Ternary_of_", elements_labels_safe, ".png"))
  grDevices::png(composite_path, width = 1400, height = 1400, bg = "white")
  graphics::plot(NA, xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5), asp = 1, axes = FALSE, xlab = "", ylab = "")

  graphics::title(main = paste("Združeni ternarni diagrami:", paste(element_labels, collapse = ", ")),
                   cex.main = 2.5, font.main = 2, line = 2)
  graphics::title(main = paste("Na osnovi:", element_labels[3], paste(", šarža", file_base)),
                   cex.main = 2.0, font.main = 1, line = 0.1)

  n <- 6
  tri_width <- 1.5
  radius <- tri_width / sqrt(3)
  angles <- seq(pi / 6, pi / 6 + 2 * pi, length.out = n + 1)[1:n]
  cx <- radius * cos(angles)
  cy <- radius * sin(angles)
  for (i in 1:n) {
    img <- png::readPNG(png_files_sorted[i])
    if (i %% 2 == 0) img <- as.raster(magick::image_rotate(magick::image_read(png_files_sorted[i]), 180))
    s <- tri_width
    h <- s * sqrt(3) / 2
    v_x <- c(0, -s / 2, s / 2); v_y <- c(2 * h / 3, -h / 3, -h / 3)
    if (i %% 2 == 0) v_y <- -v_y
    x_min <- min(cx[i] + v_x); x_max <- max(cx[i] + v_x)
    y_min <- min(cy[i] + v_y); y_max <- max(cy[i] + v_y)
    graphics::rasterImage(img, x_min, y_min, x_max, y_max)
  }

  label_positions <- list(
    list(text = paste0(element_labels[2], " mas. % →"), pos = c(1.2, 0.7), rot = -60, size = 1.5),
    list(text = paste0(element_labels[1], " mas. % →"), pos = c(0, 1.4), rot = 0, size = 1.5),
    list(text = paste0(element_labels[4], " mas. % →"), pos = c(-1.2, 0.7), rot = 60, size = 1.5),
    list(text = paste0("←", element_labels[5], " mas. %"), pos = c(-1.2, -0.7), rot = -60, size = 1.5),
    list(text = paste0("←", element_labels[6], " mas. %"), pos = c(0, -1.4), rot = 0, size = 1.5),
    list(text = paste0("←", element_labels[7], " mas. %"), pos = c(1.2, -0.7), rot = 60, size = 1.5)
  )
  for (label in label_positions) {
    graphics::text(label$pos[1], label$pos[2], label$text, cex = label$size,
                    srt = label$rot, adj = 0.5, col = "black")
  }

  graphics::text(1.6, 0, element_labels[2], cex = 2.0)
  graphics::text(0.8, 1.4, element_labels[1], cex = 2.0)
  graphics::text(-0.8, 1.4, element_labels[4], cex = 2.0)
  graphics::text(-1.6, 0, element_labels[5], cex = 2.0)
  graphics::text(-0.8, -1.4, element_labels[6], cex = 2.0)
  graphics::text(0.8, -1.4, element_labels[7], cex = 2.0)

  grDevices::dev.off()
  composite_path
}
