# ---- Inclusion add-on: diagram presets ----
# A preset fixes the three corners of an inclusion ternary (compounds or
# elements), the basis (mass or mole fractions), a default coverage
# threshold and how points are classified. Presets, reference phases,
# rules and sources live in inst/extdata/inclusion_presets/*.csv so they
# can be read and edited without touching code.

inclusion_preset_dir <- function() {
  pkg <- utils::packageName()
  if (is.null(pkg)) pkg <- "vidternary"
  system.file("extdata", "inclusion_presets", package = pkg)
}

# Split a phase's "CaO:12;Al2O3:7" into named moles.
.incl_parse_components <- function(txt, compounds) {
  parts <- strsplit(txt, ";", fixed = TRUE)[[1]]
  kv <- strsplit(parts, ":", fixed = TRUE)
  out <- setNames(vapply(kv, function(x) as.numeric(x[2]), numeric(1)), vapply(kv, `[`, character(1), 1))
  bad <- setdiff(names(out), compounds$compound)
  if (length(bad) > 0 || anyNA(out)) stop("Cannot read phase components '", txt, "'.", call. = FALSE)
  out
}

# Element moles of a phase given its compound moles (formula units).
.incl_phase_elements <- function(comps, compounds) {
  el <- numeric(0)
  add <- function(sym, n) el[sym] <<- (if (sym %in% names(el)) el[[sym]] else 0) + n
  for (nm in names(comps)) {
    r <- compounds[compounds$compound == nm, ]
    add(r$cation, comps[[nm]] * r$n_cation)
    add(r$anion, comps[[nm]] * r$n_anion)
  }
  el
}

# Reference-point coordinates (fractions of the three corners) of every
# phase that fits a diagram: compound diagrams need all components among the
# corner compounds; element diagrams need all elements but O among the
# corner elements.
inclusion_phase_references <- function(phases, kind, corners, basis, weights, compounds) {
  rows <- lapply(seq_len(nrow(phases)), function(i) {
    comps <- .incl_parse_components(phases$components[i], compounds)
    if (kind == "compound") {
      members <- lapply(corners, corner_compounds, compounds = compounds)
      if (!all(names(comps) %in% unlist(members))) return(NULL)
      wt <- if (basis == "mass") compounds$molar_mass[match(names(comps), compounds$compound)] else 1
      amount <- comps * wt
      val <- vapply(members, function(m) sum(amount[names(amount) %in% m]), numeric(1))
    } else {
      el <- .incl_phase_elements(comps, compounds)
      members <- strsplit(corners, "+", fixed = TRUE)
      if (!all(setdiff(names(el), "O") %in% unlist(members))) return(NULL)
      amount <- if (basis == "mass") el * weights[names(el)] else el
      val <- vapply(members, function(m) sum(amount[names(amount) %in% m]), numeric(1))
    }
    if (sum(val) <= 0) return(NULL)
    data.frame(phase = phases$phase[i], A = val[1] / sum(val), B = val[2] / sum(val),
               C = val[3] / sum(val), description = phases$description[i],
               row.names = NULL, stringsAsFactors = FALSE)
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) {
    return(data.frame(phase = character(0), A = numeric(0), B = numeric(0), C = numeric(0), description = character(0)))
  }
  do.call(rbind, rows)
}

#' Load the preset library
#'
#' Reads `presets.csv`, `phases.csv`, `rules.csv` and `sources.csv` from
#' `dir` and checks them: corner definitions, phase components and every
#' rule condition must be readable, and every rule preset must have rules.
#'
#' @param dir Folder with the four CSV files (default: the package's own).
#' @return A list with data frames `presets`, `phases`, `rules`, `sources`.
#' @export
load_inclusion_presets <- function(dir = inclusion_preset_dir()) {
  rd <- function(f) utils::read.csv(file.path(dir, f), stringsAsFactors = FALSE, na.strings = "",
                                    strip.white = TRUE, comment.char = "")
  lib <- list(presets = rd("presets.csv"), phases = rd("phases.csv"),
              rules = rd("rules.csv"), sources = rd("sources.csv"))
  compounds <- inclusion_compounds()
  p <- lib$presets
  if (anyDuplicated(p$id)) stop("Duplicate preset ids in presets.csv.", call. = FALSE)
  for (i in seq_len(nrow(p))) {
    corners <- c(p$A[i], p$B[i], p$C[i])
    if (p$kind[i] == "compound") lapply(corners, corner_compounds, compounds = compounds)
    if (!p$basis[i] %in% c("mass", "mole")) stop("Preset ", p$id[i], ": basis must be mass or mole.", call. = FALSE)
    if (!p$classification[i] %in% c("rules", "nearest", "none")) stop("Preset ", p$id[i], ": unknown classification.", call. = FALSE)
    if (p$classification[i] == "rules") {
      r <- lib$rules[lib$rules$preset_id == p$id[i], ]
      if (nrow(r) == 0) stop("Preset ", p$id[i], " uses rules but has none in rules.csv.", call. = FALSE)
      for (txt in r$conditions) parse_rule_conditions(txt, sub("^@", "", corners))
    }
  }
  for (i in seq_len(nrow(lib$phases))) .incl_parse_components(lib$phases$components[i], compounds)
  lib
}

#' List the available presets
#'
#' @param library From [load_inclusion_presets()].
#' @return A data frame: `id`, `name`, `kind`, `basis`, `classification`,
#'   `threshold_pct`, `applies_to`.
#' @export
list_inclusion_presets <- function(library = load_inclusion_presets()) {
  library$presets[, c("id", "name", "kind", "basis", "classification", "threshold_pct", "applies_to")]
}

#' Get one preset, ready to use
#'
#' @param id Preset id, see [list_inclusion_presets()].
#' @param library From [load_inclusion_presets()].
#' @param basis Optional `"mass"` or `"mole"`, replacing the preset's own.
#'   Presets classified by rules only support the basis their thresholds
#'   are written in.
#' @param weights Atomic weights.
#' @return A list of class `inclusion_preset`: `id`, `name`, `kind`,
#'   `corners`, `labels`, `basis`, `threshold_pct`, `classification`,
#'   `description`, `applies_to`, `rules`, `references` (phase
#'   coordinates), `sources`.
#' @export
get_inclusion_preset <- function(id, library = load_inclusion_presets(), basis = NULL,
                                 weights = inclusion_atomic_weights()) {
  row <- library$presets[library$presets$id == id, ]
  if (nrow(row) != 1) stop("Unknown preset '", id, "'. Available: ", paste(library$presets$id, collapse = ", "), call. = FALSE)
  if (is.null(basis)) basis <- row$basis
  if (!basis %in% c("mass", "mole")) stop("basis must be 'mass' or 'mole'.", call. = FALSE)
  if (row$classification == "rules" && basis != row$basis) {
    stop("The rules of preset '", id, "' are written in ", row$basis, " fractions; it cannot be used in ", basis, ".", call. = FALSE)
  }
  compounds <- inclusion_compounds(weights)
  corners <- c(row$A, row$B, row$C)
  rules <- library$rules[library$rules$preset_id == id, , drop = FALSE]
  rules <- rules[order(rules$priority), , drop = FALSE]
  src <- strsplit(row$sources, ";", fixed = TRUE)[[1]]
  structure(list(
    id = row$id, name = row$name, kind = row$kind, corners = corners,
    labels = sub("^@", "", corners), basis = basis, threshold_pct = row$threshold_pct,
    classification = row$classification, description = row$description, applies_to = row$applies_to,
    rules = rules,
    references = inclusion_phase_references(library$phases, row$kind, corners, basis, weights, compounds),
    sources = library$sources[library$sources$id %in% src, , drop = FALSE]
  ), class = "inclusion_preset")
}
