# vidternary fork: inclusion presets (started 2026-09-29)

Copy of `vidternary_20250917/vidternary` (master, commit 79e7598) with an
add-on for **inclusion diagrams**. The original package is untouched; the
existing ternary pipeline, UI and tests are unchanged. The package name is
still `vidternary`; rename it (DESCRIPTION, `tests/testthat.R`,
`asNamespace("vidternary")` / `.package = "vidternary"` in a few tests,
vignettes, `R/vidternary-package.R`) before installing both side by side.

## What the add-on does

element wt% (EDS) -> remove steel matrix -> resolve into compounds ->
preset diagram (corners, basis, threshold) -> classes + classification lines.

| File | Contents |
|---|---|
| `R/inclusion_atomic_weights.R` | CIAAW atomic weights, compound table, element-column detection |
| `R/inclusion_matrix_removal.R` | `remove_matrix()`, `estimate_matrix_ratios()` |
| `R/inclusion_compounds.R` | `elements_to_compounds()`, `corner_compounds()` |
| `R/inclusion_presets.R` | preset library (`load_/list_/get_inclusion_preset`) |
| `R/inclusion_rules.R` | rule grammar, `classify_inclusions()`, boundary lines |
| `R/inclusion_analysis.R` | `analyze_inclusion_preset()` |
| `R/inclusion_plot.R` | `plot_inclusion_preset()` |
| `R/inclusion_input_helpers.R` | steel-composition parser, matrix-element default, preset choices, export tables |
| `R/ui_inclusion_tab.R`, `R/ui_inclusion_presets.R`, `R/server_inclusion.R` | the "Inclusion Presets" tab (module `inclusion`, own single-file upload) |
| `inst/extdata/inclusion_presets/*.csv` | presets, reference phases, rules, sources (editable) |
| `tests/testthat/test-inclusion-*.R`, `helper-inclusion.R` | tests with synthetic particles of known composition |

Quick use:

```r
devtools::load_all()
list_inclusion_presets()
d <- openxlsx::read.xlsx("1.xlsx", sheet = 1)
r <- analyze_inclusion_preset(d, "cao-al2o3-mgo", matrix_element = "Fe",
                              matrix_mode = "matrix_and_alloys",
                              steel_composition = c(Fe = 70, Cr = 20, Ni = 10))
plot_inclusion_preset(r)
r$summary; r$excluded; r$settings
```

## Conventions (state them when reporting results)

* Sulfur goes to Ca first, then Mn (`s_order`); Ti counts as TiN (`ti_as`);
  oxygen is calculated from the oxides, measured O is only a check.
* Carbon is ignored (`ignore_elements`); matrix particles (> 80 % matrix)
  and particles with < 5 % of the analysis left after matrix removal are not plotted.
* Coverage threshold (share of the three corners in the particle's total):
  50 % compound presets, 30 % element presets (the Thermo Fisher ParticleX value).
* Class rules of `cao-al2o3-mgo` and `mns-cas-oxides` are conventions
  (midpoints between neighbouring calcium aluminates, etc.), not a standard;
  the other presets classify by the nearest reference phase.
* Room-temperature compositions only: no liquidus or other thermodynamic lines.

## "Inclusion Presets" tab

Its own tab, second after "Ternary Plots" (module id `inclusion`, inputs
`incl_*`), with its own single-file upload (Sheet 1 of one XLSX = one
specimen, like the EVS and spatial tabs). The first tab is byte-identical to
the original. Edits to existing files: `R/server_logic.R` (registers the
module, 5 lines), `R/ui_components.R` (adds the tab, 1 line + doc count),
`R/vidternary-package.R` (two base-graphics imports).

Controls: preset (grouped by kind, with description, use and source links),
basis (fixed to mass for rule presets), coverage threshold, matrix element
(default: the most abundant element), matrix-only or matrix-and-alloys
removal (steel composition estimated from the data or typed as
`Fe=70, Cr=20, Ni=10`), elements to ignore, sulfur order and Ti treatment
(compound presets only), display options. Outputs: loaded-data summary,
diagram, class table, dropped-particle table, settings note, PNG download
and an xlsx with one row per particle (class, corner fractions, coverage,
reason if dropped), the class summary, settings and sources.

Default: matrix and alloying elements removed, steel composition estimated
from the 10 % most matrix-rich particles (the ratios used are shown above
the diagram).

## Sources and licences

* Code: written for this project; no code was copied from the sources
  below. Dependencies (shiny, Ternary, openxlsx, ...) are installed
  separately, not bundled. Package licence: MIT (see `LICENSE`).
* Atomic weights (`R/inclusion_atomic_weights.R`): 23 elements from the
  IUPAC/CIAAW Standard Atomic Weights (2024 table, <https://www.ciaaw.org/atomic-weights.htm>,
  (c) CIAAW; conventional values where the table gives an interval).
* Reference phase compositions are calculated from the stoichiometric
  formulas with those weights; no tables or figures were copied.
* `inst/extdata/inclusion_presets/sources.csv` lists the documents the
  preset definitions were informed by (Thermo Fisher and nanoScience
  application notes, phase-equilibrium papers). They are cited by link only:
  none of their text, figures or tables is reproduced. The 30 % default
  coverage threshold of the element presets is the value shown in the Thermo
  Fisher note AN0197 example; product names are used only to say where an idea
  or a value comes from, not to imply endorsement.

## Not done yet

* Hooking presets into the Multiple Ternary Creator (batch) tab.
* Saving the preset settings with a project, PDF/TIFF export of the diagram.
* Point size / colour by Optional Param 1/2 on the preset diagrams.
