# Writes the example files in inst/extdata/examples/. Run from the package
# root:  Rscript data-raw/make_example_data.R
#
# The files are SIMULATED. They have the columns of an automated SEM/EDS
# particle export and plausible numbers, so every tab of the app can be
# tried without a measurement of one's own, but they describe no real
# steel. What is built in on purpose:
#   - 200 SEM fields of 0.1 x 0.1 mm on a 2 x 1 mm area;
#   - most particles are small (median diameter about 1.2 um), so many lie
#     below the electron range and their spectra are mostly matrix;
#   - a third of the particles sit in clusters (counts per field vary more
#     than random scatter) and each file holds a few aluminate stringers;
#   - example_run_1 and example_run_2 are two runs of the same simulated
#     specimen; example_other_heat has fewer, larger particles and more
#     sulfides.

atomic <- c(C = 12.011, N = 14.007, O = 15.999, Mg = 24.305, Al = 26.982, Si = 28.086, S = 32.06, Ca = 40.078,
            Ti = 47.867, Cr = 51.996, Mn = 54.938, Fe = 55.845, Ni = 58.693, Mo = 95.95)
compound <- function(...) {
  atoms <- c(...)
  mass <- atoms * atomic[names(atoms)]
  mass / sum(mass)
}
families <- list(
  alumina = compound(Al = 2, O = 3), spinel = compound(Mg = 1, Al = 2, O = 4), ca_aluminate = compound(Ca = 1, Al = 2, O = 4),
  sulfide = compound(Mn = 1, S = 1), nitride = compound(Ti = 1, N = 1), silicate = compound(Si = 1, O = 2))
steel <- c(Fe = 95.5, Cr = 1.5, Ni = 1.4, Mn = 0.8, Si = 0.3, Mo = 0.3, C = 0.2) / 100
elements <- names(atomic)

make_file <- function(seed, n, median_ecd, mix, n_stringers = 3) {
  set.seed(seed)
  n_cluster <- round(n / 3)
  n_random <- n - n_cluster
  centres <- cbind(runif(ceiling(n_cluster / 8), 0.05, 1.95), runif(ceiling(n_cluster / 8), 0.05, 0.95))
  pick <- sample(nrow(centres), n_cluster, replace = TRUE)
  x <- c(runif(n_random, 0, 2), centres[pick, 1] + rnorm(n_cluster, 0, 0.025))
  y <- c(runif(n_random, 0, 1), centres[pick, 2] + rnorm(n_cluster, 0, 0.025))
  family <- sample(names(mix), n, replace = TRUE, prob = mix)
  ecd <- pmax(0.3, median_ecd * exp(rnorm(n, 0, 0.7)))
  aspect <- ifelse(family == "sulfide", 1.5 + rexp(n, 0.6), 1 + rexp(n, 4))
  direction <- ifelse(family == "sulfide", rnorm(n, 0, 8) %% 180, runif(n, 0, 180))

  # Aluminate stringers along X: 4 to 6 compact particles, 15 to 30 um apart.
  for (k in seq_len(n_stringers)) {
    m <- sample(4:6, 1)
    x0 <- runif(1, 0.1, 1.7); y0 <- runif(1, 0.1, 0.9)
    x <- c(x, x0 + cumsum(c(0, runif(m - 1, 0.015, 0.030))))
    y <- c(y, y0 + rnorm(m, 0, 0.002))
    family <- c(family, rep("alumina", m))
    ecd <- c(ecd, runif(m, 3, 6)); aspect <- c(aspect, runif(m, 1, 1.5)); direction <- c(direction, runif(m, 0, 180))
  }
  keep <- x >= 0 & x <= 2 & y >= 0 & y <= 1
  x <- x[keep]; y <- y[keep]; family <- family[keep]; ecd <- ecd[keep]; aspect <- aspect[keep]; direction <- direction[keep]
  n <- length(x)

  # Share of the spectrum that comes from the particle: small ones are mostly matrix.
  share <- 1 - exp(-(ecd / 1.6)^1.5)
  chem <- t(vapply(seq_len(n), function(i) {
    w <- stats::setNames(numeric(length(elements)), elements)
    w[names(families[[family[i]]])] <- share[i] * families[[family[i]]]
    w[names(steel)] <- w[names(steel)] + (1 - share[i]) * steel
    w <- w * exp(rnorm(length(w), 0, 0.05))
    100 * w / sum(w)
  }, numeric(length(elements))))
  chem[chem < 0.05] <- 0
  colnames(chem) <- paste0(elements, " (Wt%)")

  area <- pi / 4 * ecd^2
  field <- pmin(floor(x / 0.1), 19) + 20 * pmin(floor(y / 0.1), 9) + 1
  out <- data.frame(Feature = seq_len(n), Field = field, `Area (sq. um)` = area, `Aspect Ratio` = aspect,
                    `Breadth (um)` = ecd / sqrt(aspect), `Direction (degrees)` = direction, `ECD (um)` = ecd,
                    `Length (um)` = ecd * sqrt(aspect), `Stage X (mm)` = 30 + x, `Stage Y (mm)` = 20 + y,
                    check.names = FALSE)
  out <- cbind(out, chem)[order(field, x), ]
  out$Feature <- seq_len(n)
  out[] <- lapply(out, function(v) if (is.integer(v)) v else signif(v, 5))
  rownames(out) <- NULL
  out
}

same <- c(alumina = 0.35, spinel = 0.10, ca_aluminate = 0.10, sulfide = 0.20, nitride = 0.15, silicate = 0.10)
other <- c(alumina = 0.20, spinel = 0.05, ca_aluminate = 0.10, sulfide = 0.40, nitride = 0.15, silicate = 0.10)
files <- list(example_run_1 = make_file(101, 900, 1.2, same), example_run_2 = make_file(102, 880, 1.2, same),
              example_other_heat = make_file(103, 500, 1.8, other, n_stringers = 1))

dir.create("inst/extdata/examples", recursive = TRUE, showWarnings = FALSE)
for (name in names(files)) {
  openxlsx::write.xlsx(files[[name]], file.path("inst/extdata/examples", paste0(name, ".xlsx")))
  cat(name, nrow(files[[name]]), "particles\n")
}
