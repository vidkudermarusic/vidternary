# ---- Batch comparison: plots ----
# One point with its interval per batch (the same picture for density, area
# fraction and extreme-value return level), the size distributions on one
# plot, and the elements that set the batches apart. Pure ggplot2.

#' One value with its interval per batch
#'
#' A dashed line marks the mean of the batches. No control limits are
#' drawn: with a handful of batches they would be computed from the very
#' points they are meant to judge.
#'
#' @param data Data frame with a `batch` column (drawn in row order).
#' @param value,lower,upper Names of the value and interval columns
#'   (`lower`/`upper` may be `NULL` for points without an interval).
#' @param y_label Axis label.
#' @param title Plot title.
#' @return A `ggplot` object.
#' @export
create_batch_interval_chart <- function(data, value, lower = NULL, upper = NULL, y_label = value, title = NULL) {
  d <- data.frame(batch = factor(data$batch, levels = unique(data$batch)), value = data[[value]],
                  lower = if (is.null(lower)) NA_real_ else data[[lower]],
                  upper = if (is.null(upper)) NA_real_ else data[[upper]])
  d <- d[is.finite(d$value), , drop = FALSE]
  p <- ggplot2::ggplot(d, ggplot2::aes(x = batch, y = value)) +
    ggplot2::geom_hline(yintercept = mean(d$value), linetype = "dashed", colour = "grey50")
  if (any(is.finite(d$lower) & is.finite(d$upper))) {
    p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper), width = 0.15, colour = "#357ABD", na.rm = TRUE)
  }
  p + ggplot2::geom_point(size = 3, colour = "#002147") +
    ggplot2::labs(x = "Batch", y = y_label, title = title,
                  subtitle = "Point = estimate, bar = 95% interval, dashed line = mean of the batches") +
    ggplot2::theme_minimal()
}

#' Size distributions of the batches on one plot
#'
#' Share of particles larger than a given size (complementary cumulative
#' distribution), with both axes logarithmic so the tail - the few large
#' inclusions - is visible.
#'
#' @param size Particle sizes (um).
#' @param batch Batch of each particle.
#' @return A `ggplot` object.
#' @export
create_size_tail_plot <- function(size, batch) {
  ok <- is.finite(size) & size > 0 & !is.na(batch)
  batch <- as.character(batch[ok])
  size <- size[ok]
  d <- do.call(rbind, lapply(unique(batch), function(b) {
    v <- sort(size[batch == b])
    data.frame(batch = b, size = v, share = 100 * (length(v) - seq_along(v) + 1) / length(v), stringsAsFactors = FALSE)
  }))
  d$batch <- factor(d$batch, levels = unique(batch))
  ggplot2::ggplot(d, ggplot2::aes(x = size, y = share, colour = batch)) +
    ggplot2::geom_step(linewidth = 0.7) +
    ggplot2::scale_x_log10() + ggplot2::scale_y_log10() +
    ggplot2::labs(x = "Equivalent circle diameter (um)", y = "Share of particles at least this large (%)",
                  colour = "Batch", title = "Size distributions") +
    ggplot2::theme_minimal()
}

#' Which elements set the batches apart
#'
#' Clr mean of each batch minus the clr mean of all particles: a bar above
#' zero means the batch holds relatively more of that element than the
#' average particle, by a factor of `exp(difference)`.
#'
#' @param clr_means From [compare_compositions()] (`$clr_means`).
#' @return A `ggplot` object.
#' @export
create_clr_difference_plot <- function(clr_means) {
  clr_means$batch <- factor(clr_means$batch, levels = unique(clr_means$batch))
  clr_means$part <- factor(clr_means$part, levels = unique(clr_means$part))
  ggplot2::ggplot(clr_means, ggplot2::aes(x = part, y = difference, fill = batch)) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::geom_hline(yintercept = 0, colour = "grey30") +
    ggplot2::labs(x = NULL, y = "clr mean of the batch minus clr mean of all particles", fill = "Batch",
                  title = "Elements that set the batches apart") +
    ggplot2::theme_minimal()
}
