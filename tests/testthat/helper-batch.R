# Helpers for the batch-comparison tests: synthetic batches with known
# differences.

BATCH_COLS <- list(area = "Area.(sq..um)", field = "Field", x = "Stage.X.(mm)", y = "Stage.Y.(mm)",
                   parts = c("Ti.(Wt%)", "Fe.(Wt%)", "Cr.(Wt%)"))

# n particles scattered over a 2 x 1 mm area cut into 20 SEM fields, with
# log-normal sizes (median equivalent circle diameter `size` um) and a
# Ti - Fe - Cr chemistry around `ti` wt% Ti. Two particles pin the corners,
# so every batch spans exactly 2 mm2.
make_batch <- function(n = 200, size = 2, ti = 40, seed = 1) {
  withr::with_seed(seed, {
    x <- c(0, 2, stats::runif(n - 2, 0, 2))
    y <- c(0, 1, stats::runif(n - 2, 0, 1))
    ecd <- size * exp(stats::rnorm(n, 0, 0.5))
    ti_wt <- pmin(pmax(ti + stats::rnorm(n, 0, 5), 1), 90)
    cr_wt <- (100 - ti_wt) * 0.2 * exp(stats::rnorm(n, 0, 0.1))
    data.frame(Field = pmin(floor(x * 10) + 1, 20), `Area.(sq..um)` = pi / 4 * ecd^2,
               `Stage.X.(mm)` = x, `Stage.Y.(mm)` = y,
               `Ti.(Wt%)` = ti_wt, `Fe.(Wt%)` = 100 - ti_wt - cr_wt, `Cr.(Wt%)` = cr_wt, check.names = FALSE)
  })
}

# A and B are drawn from the same population; C has half as many
# particles, larger ones, and more Ti.
make_batches <- function() {
  list(A = make_batch(200, 2, 40, seed = 1), B = make_batch(200, 2, 40, seed = 2), C = make_batch(100, 3, 55, seed = 3))
}

# The batches as uploaded xlsx files, in the shape Shiny's fileInput() produces.
make_batch_upload <- function(batches = make_batches()) {
  dir <- tempfile("batches_")
  dir.create(dir)
  paths <- vapply(names(batches), function(name) {
    path <- file.path(dir, paste0(name, ".xlsx"))
    openxlsx::write.xlsx(batches[[name]], path)
    path
  }, character(1))
  data.frame(name = basename(paths), size = file.info(paths)$size, type = "", datapath = unname(paths), stringsAsFactors = FALSE)
}
