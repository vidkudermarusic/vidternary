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
(default: Fe, Ni or Co if one has a median of at least 10 wt%, else none), matrix-only or matrix-and-alloys
removal (steel composition estimated from the data or typed as
`Fe=70, Cr=20, Ni=10`), elements to ignore, sulfur order and Ti treatment
(compound presets only), display options. Outputs: loaded-data summary,
diagram, class table, dropped-particle table, settings note, PNG download
and an xlsx with one row per particle (class, corner fractions, coverage,
reason if dropped), the class summary, settings and sources.

Default: matrix and alloying elements removed, steel composition estimated
from the 10 % most matrix-rich particles (the ratios used are shown above
the diagram).

Rules for the input and the presets (after the audit of 2026-09-30):

* A blank or non-numeric reading is a missing reading, not a 0: the
  particle is dropped with the reason "missing reading" (as in the main
  ternary pipeline). A column that is blank in every row counts as not
  measured; ignored elements (default C) are not checked.
* `presets.csv` has an optional column `extra_refs` (`label@A;label@C`): an
  extra reference phase at a pure corner. Ti-Al-N uses it for
  "Ti (no or little N)", so a Ti-rich particle without a measured N is not
  called TiN (EDS nitrogen next to Ti is unreliable).
* Class colours: 18-colour palette, so the 14 classes of CaO-SiO2-Al2O3 all
  differ.

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

## Steel cleanliness and fatigue (branch `nove-funkcije`, 2026-11-11)

New tab **Steel Cleanliness** and a fatigue-limit panel in the **Extreme
Value Analysis** tab. Pure functions first (`run_cleanliness_analysis()` in
`R/cleanliness_pipeline.R` runs everything without Shiny), then the tab.

What it does:

* **Chemical family** per particle from S, Ti, Al, Mg, Ca, Si, compared as
  compound mass (S as MnS, Ti as TiN, Al/Mg/Ca as oxides, Si as SiO2). N, O
  and C are not used (Ti-N and Cr-O overlaps, carbon contamination), so
  "Ti-rich" is not split into nitride / carbonitride / oxide. A class column
  of the file can be used instead.
* **Stringers**: particles of one family in line along the working
  direction, gap <= 40 um and offset <= 10 um (ISO 4967) or 15 um (ASTM E45);
  three or more are a stringer.
* **Type**: sulfide elongated or in a stringer = A; stringer of compact
  particles = B; other elongated = C; compact single = D, DS from 13 um.
  Families the standards do not name are rated by shape, as ISO 4967 allows,
  and the rating is also reported per family.
* **Rating**: the inspected area is re-cut into 0.5 mm2 fields from the
  stage coordinates (SEM fields are far smaller). Index from Table 1; worst
  field (method A) and mean over all fields (method B).
* **Indices**: particles per mm2 with an exact Poisson interval, area
  fraction with a bootstrap over SEM fields, counts per size class.
* **Stereology**: Delesse, Fullman, Schwartz-Saltykov (coefficients computed,
  not copied from a table), O/S/N bound in the inclusions.
* **Fatigue**: Murakami's sqrt(area) model applied to the EVS return level
  and both of its intervals; helper for T = V / (S0 x h).

Conventions and limits (state them when reporting results):

* An SEM run covers a few mm2 (batch 1: 1.90 mm2 = six rating fields, two
  complete); ISO 4967 asks for about 200 mm2 at 100x. The result is an
  SEM-based estimate, not a rating to the standard (see ASTM E2142).
* Particles narrower than 2 um are not rated (setting). An object belongs to
  the rating field its centre lies in. Particle direction is used as
  reported by the instrument.
* Most detections below 1 um are matrix signal: their family is unreliable
  and in Mo-bearing steel often reads as sulfide (S K / Mo L overlap). Use
  the pre-analysis filter (e.g. Area > 1) for indices and stereology.
* Mass balance takes one pure compound per family with handbook densities
  (MnS 4.0, Al2O3 3.99, SiO2 2.65, TiN 5.40 g/cm3).

Where the numbers come from (checked 2026-11-11):

* ISO 4967:2013 Table 1 (index limits), Table 2 (widths), group definitions,
  0.50 mm2 field, 200 mm2 specimen: read from the publisher's preview,
  <https://standards.iteh.ai/catalog/standards/iso/631317f1-2dc3-40f5-bfcc-025c6035b946/iso-4967-2013>.
  No table or figure is reproduced beyond these limit values.
* ASTM E45 stringer rule (15 um, 40 um, three or more) and aspect ratio 2:
  from excerpts found by web search, not from a full copy. Its index limits
  and widths are taken as equal to ISO 4967.
* ISO 4967 stringer distances (40 / 10 um) and the Annex D formulas: from
  memory; EN 10247 uses the same distances. The formulas match Table 1 to
  about 1 % and are used only for the continuous index.
* Murakami constants (1.43 / 1.41 / 1.56, HV + 120, exponent 1/6, R-ratio
  exponent 0.226 + HV x 1e-4): checked against open-access papers quoting
  the model, not the original publications.
* ISO 4967:2026 (fourth edition) exists; its tables were not compared.

Not done: DIN 50602 K-value (weight table not available, standard
withdrawn), EN 10247, objects split across rating-field borders, a proper
light-microscope equivalence.

## Import, reproducibility and batch comparison (branch `nove-funkcije`, 2026-11-12)

**Data Import tab.** Reads `.xlsx` and delimited text (`.csv`, `.txt`,
`.tsv`; delimiter, decimal comma, BOM and Latin-1 detected), proposes which
column is the area, length, position ... from its name, converts units
(lengths to um, area to um2, positions to mm) and atomic to weight percent,
and writes the standard table every other tab recognises by itself. The
mapping can be saved as a JSON profile. The single-upload tabs now also
accept text files directly (`read_particle_table()` behind
`make_combined_upload_reactive()`).

* Only the Oxford AZtec Feature export was checked against real files. The
  name patterns for other systems (Feret, Dmax, StageX ...) are guesses from
  common usage - no Bruker, Thermo or Zeiss export was available. Confirm the
  proposal on the first file and save it as a profile.
* Files with title lines above the header, thousands separators, or `.xls`
  are not handled.
* Atomic percent is converted over the mapped elements and normalised to
  100 wt%.

**Reproducibility.** Steel Cleanliness, EVS, Batch Comparison, Spatial
Clustering, Point Pattern Analysis and Compositional Analysis save / load
all their inputs as JSON (`R/settings_io.R`). Steel Cleanliness, EVS and
Batch Comparison also write a self-contained HTML report (plots embedded,
settings, notes, MD5 of the data file; no pandoc needed) and an R script that
repeats the analysis. Tests run each generated script and compare its result
with the tab's.

* Load the data file before the settings: column choices can only be
  restored once the columns are known. Values that could not be set are
  listed.
* Not wired: Ternary Plots, Multiple Ternary Creator, Hexagonal diagram,
  Inclusion Presets, Data Comparison (many dynamic inputs; Plot Builder has
  its own presets).

**Batch Comparison tab.** One file per batch; `run_batch_comparison()` is the
pure entry point.

* Amount: particles per mm2 (exact Poisson interval), area fraction
  (bootstrap over SEM fields), density ratio per pair (exact Poisson test),
  chi-squared of the counts against the areas. Inclusions cluster, so the
  index of dispersion of the counts per field is shown and a Kruskal-Wallis
  test on those counts is given next to the Poisson test.
* Size: k-sample Anderson-Darling statistic (Scholz & Stephens 1987,
  tie-adjusted form) with a permutation p-value; quantiles and their ratios
  as effect size. The formula was entered from memory and checked against a
  term-by-term implementation, not against the `kSamples` package (not
  installed); the p-value is valid whatever the exact form, because it comes
  from permutations.
* Chemistry: permutation MANOVA (Anderson 2001) on ilr coordinates, checked
  to agree with `vegan::adonis2` to all digits; R2 as effect size; a separate
  test of spread; clr means per batch. Default elements leave out C, N and O;
  a warning names overlapping EDS lines among the chosen elements (Ti-N,
  Cr-O, Ti-V, S-Mo-Pb-Bi).
* Largest inclusion: the EVS fit per batch, same control area and T.
* No control limits are drawn on the batch charts: with a handful of batches
  they would be computed from the points they are meant to judge.
* Permutation tests treat particles as independent. One cluster of similar
  particles can make two batches "differ"; the notes say so.

Batches 1, 2, 3 with Area > 1 (checked 2026-11-12): batch 2 has 0.58 times
the number density of 1 and 3 (Poisson p < 0.001, but only p = 0.046 on
counts per field - batches 1 and 3 are clustered, dispersion 4.2 and 2.2);
its median ECD is 2.8 um against 1.95 um; chemistry differs with R2 = 4 % on
eight elements; return levels for T = 380 are 14.1, 12.2 and 15.7 um with
overlapping intervals.

## Reliability of the EDS analysis (branch `nove-funkcije`, 2026-11-12)

* **Peak overlaps** (`R/eds_overlap.R`). Groups: Ti-N, Cr-O, Ti-V,
  S-Mo-Pb-Bi. A yellow box appears as soon as the columns chosen for one
  diagram or analysis hold two elements of a group: Ternary Plots (axes,
  size and colour per diagram, and the outlier columns), Multiple Ternary
  Creator, Hexagonal diagram (anywhere in the hexagon), Compositional
  Analysis, Data Comparison (outlier columns), Batch Comparison, Inclusion
  Presets (elements the preset reads, minus the ignored ones). It is a
  warning only - nothing is blocked. Not wired: Plot Builder, and the
  pre-analysis filters of EVS and the spatial tabs.
* **Particle size** (`R/eds_size_reliability.R`). Electron range after
  Kanaya & Okayama, R = 0.0276 A E^1.67 / (Z^0.89 rho); 0.99 um in Fe at
  15 kV. Classes: below the range "matrix-dominated", up to twice the range
  "partly matrix", above "reliable". The factor 2 is a convention of this
  app. The range is that of the matrix; in a lighter inclusion the beam
  spreads further, so it is a lower bound. Formula entered from memory.
  Steel Cleanliness gives particles below the range the family "too small
  to classify" (can be switched off; a class column of the file is never
  overridden). Data Import can add `Size.to.range` and
  `Composition.reliability` to the standard table.
* **Class shares** (`R/class_share_intervals.R`). Percentile bootstrap over
  SEM fields; Wilson interval, labelled as assuming independent particles,
  when there is no field column. In Steel Cleanliness (families and types)
  and Inclusion Presets (classes).

Batch 1, all detections (checked 2026-11-12): 98.0 % of the particles are
below the range, with a median Fe content of 46 wt%; 1.2 % partly matrix
(35 wt%); 0.8 % reliable (12 wt%). With the size check the "sulfides" drop
from 816 to 14 - the others were matrix spectra with S read from the Mo L
line. Family shares: Ti-rich 76 % [61, 90], silicate 14 % [1, 29] (wide,
because the silicates sit in a few fields), sulfide 6 % [3, 9], aluminate
4 % [1, 7].

## Not done yet

* Hooking presets into the Multiple Ternary Creator (batch) tab.
* Saving the preset settings with a project, PDF/TIFF export of the diagram.
* Point size / colour by Optional Param 1/2 on the preset diagrams.
