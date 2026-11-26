# Helpers for the steel-cleanliness tests: a small synthetic particle list
# in the shape of an automated SEM export, built so that the right answer of
# every step is known. Chemistry rows come from incl_row() (helper-inclusion.R).

CLEAN_COLS <- list(area = "Area.(sq..um)", length = "Length.(um)", breadth = "Breadth.(um)",
                   x = "Stage.X.(mm)", y = "Stage.Y.(mm)", direction = "Direction.(degrees)", field = "Field")

# One particle: an ellipse of `length` x `breadth` um at (x_mm, y_mm), made
# of `compounds` plus `steel` (see incl_row()).
clean_particle <- function(x_mm, y_mm, length, breadth, compounds, steel = 30, direction = 0) {
  cbind(
    data.frame(Field = max(1, ceiling(x_mm * 4)), `Area.(sq..um)` = pi / 4 * length * breadth,
               `Length.(um)` = length, `Breadth.(um)` = breadth, `Direction.(degrees)` = direction,
               `Stage.X.(mm)` = x_mm, `Stage.Y.(mm)` = y_mm, check.names = FALSE),
    incl_row(compounds, steel = steel)
  )
}

# 32 particles on a 2 x 1 mm area (six 0.5 mm2 rating fields, two complete):
#   field 1: a stringer of 3 compact aluminates, 64 um end to end -> B fine 0.5
#            one sulfide 50 x 3 um                               -> A fine 0.5
#            four round Ti-rich particles of 5 um                -> D fine 1.0
#   field 2: one silicate 30 x 6 um                              -> C thick 0.5
#   field 6: one round Ti-rich particle of 15 um                 -> DS 0.5
# plus 22 particles of 0.4 x 0.3 um that are mostly matrix (unclassified),
# two of them pinning the corners of the inspected area.
make_cleanliness_data <- function() {
  matrix_speck <- function(x, y) clean_particle(x, y, 0.4, 0.3, list(TiN = 3), steel = 97)
  rows <- c(
    list(matrix_speck(0, 0), matrix_speck(2, 1)),
    lapply(c(0.100, 0.130, 0.160), function(x) clean_particle(x, 0.200, 4, 3, list(Al2O3 = 70))),
    list(clean_particle(0.400, 0.500, 50, 3, list(MnS = 70)),
         clean_particle(0.900, 0.300, 30, 6, list(SiO2 = 70))),
    Map(function(x, y) clean_particle(x, y, 5, 5, list(TiN = 70)),
        c(0.300, 0.300, 0.500, 0.600), c(0.100, 0.300, 0.100, 0.600)),
    list(clean_particle(1.600, 0.800, 15, 15, list(TiN = 70))),
    lapply(seq(0.05, 1.95, length.out = 20), function(x) matrix_speck(x, 0.9))
  )
  do.call(rbind, rows)
}

# The data as an uploaded xlsx, in the shape Shiny's fileInput() produces.
make_cleanliness_upload <- function(data = make_cleanliness_data()) {
  path <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(data, path)
  data.frame(name = basename(path), size = file.info(path)$size, type = "",
             datapath = path, stringsAsFactors = FALSE)
}
