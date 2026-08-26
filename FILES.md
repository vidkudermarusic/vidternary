# Pregled datotek v paketu `vidternary`

Kratek opis vsake datoteke v paketu, urejen po funkcionalnih sklopih. Za podrobnejši opis posamezne funkcije glej komentar na vrhu vsake datoteke.

## Vstopna točka

| Datoteka | Opis |
|---|---|
| `R/app.R` | Zažene celotno Shiny aplikacijo (glavna vstopna točka `run_app()`-podobne funkcije). |

## UI moduli (`ui_*.R`)

| Datoteka | Opis |
|---|---|
| `R/ui_components.R` | Lupina celotnega UI: naslov, globalni CSS/JS, glavni tabset ter vedno vidne nastavitve direktorijev in predpomnilnika. Posamezni zavihki so izločeni v spodnje datoteke. |
| `R/ui_ternary_plots_tab.R` | Zavihek **Ternary Plots** – glavni delovni tok: nalaganje dveh datasetov, izbira osi/elementov, filtri po elementih, analitične metode, izhodne nastavitve, predogled. |
| `R/ui_data_comparison_tab.R` | Zavihek **Data Comparison** – opisna statistika, korelacijska analiza in multivariatna primerjava (Mahalanobis/Isolation Forest) dveh datasetov. |
| `R/ui_multiple_ternary_tab.R` | Zavihek **Multiple Ternary Creator** – paketna izdelava enega ternarnega grafa na vsako naloženo datoteko. |
| `R/ui_hex_ternary_tab.R` | Zavihek **Hexagonal Ternary Diagram** – sestavi 6 trikotnih ternarnih grafov iz ene datoteke v en heksagonalni prikaz. |
| `R/ui_plot_builder_tab.R` | Zavihek **Plot Builder** – splošni graditelj grafov (tip grafa, X/Y os, barva/skupina, log skale) + shranjeni uporabniški predlogi. |
| `R/ui_evs_tab.R` | Zavihek **Extreme Value Analysis** – Murakami/ASTM E2283 statistika ekstremnih vrednosti za napoved velikosti vključkov. |
| `R/ui_spatial_tab.R` | Zavihek **Spatial Clustering** – Clark-Evans test naključnosti prostorske razporeditve vključkov (spacing/gručenje). |
| `R/ui_coda_tab.R` | Zavihek **Compositional Analysis** – CLR/ILR log-ratio transformacije in PCA za sestavne (Wt%) kemijske podatke. |
| `R/ui_analysis_log_tab.R` | Zavihek **Analysis Log** – filtriran/iskalen dnevnik dejavnosti aplikacije. |

## Server moduli (`server_*.R`)

| Datoteka | Opis |
|---|---|
| `R/server_logic.R` | Osrednji server modul – inicializira skupni `reactiveValues` (`rv`) in poveže vse ostale server module. |
| `R/server_ternary_plots.R` | Logika za enodatotečno izdelavo ternarnih grafov: gradnja parametrov, predogled, poročilo analize, gumbi Save. |
| `R/server_ternary_plots_batch.R` | Paketni predogled/shranjevanje za **Multiple Ternary Creator** (izločeno iz `server_ternary_plots.R`). |
| `R/server_ternary_plots_groups.R` | Zaznava kategorične podatke v "Optional Parameter 2" in upravlja UI za izbiro skupin. |
| `R/server_data_comparison.R` | Registrira vse handlerje zavihka **Data Comparison** v pravilnem vrstnem redu (statistika, multivariatno, predogled). |
| `R/server_data_comparison_stats.R` | Gumbi za opisno statistiko in korelacije – DT tabele, statistične kartice, korelacijski heatmap, mini-histogrami. |
| `R/server_data_comparison_multivariate.R` | Gumbi za Mahalanobisovo razdaljo in Isolation Forest ter skupen prikaz rezultatov. |
| `R/server_data_comparison_preview.R` | Povzetki manjkajočih vrednosti/osamelcev in surov predogled naloženih Excel datotek. |
| `R/server_hex_ternary.R` | Logika zavihka **Hexagonal Ternary Diagram** (generiraj v predogled / shrani v datoteko). |
| `R/server_plot_builder.R` | Logika zavihka **Plot Builder** – gradnja grafov ter shranjevanje/nalaganje/brisanje uporabniških predlog. |
| `R/server_evs.R` | Logika zavihka **Extreme Value Analysis** – prilagajanje Gumbelove porazdelitve, graf, goodness-of-fit test. |
| `R/server_spatial.R` | Logika zavihka **Spatial Clustering** – Clark-Evans test, razsevni graf, histogram razdalj do najbližjega soseda. |
| `R/server_coda.R` | Logika zavihka **Compositional Analysis** – CLR/ILR transformacija, PCA, biplot. |
| `R/server_analysis_log.R` | Beleženje in prikaz dnevnika dejavnosti, shranjevanje/izvoz dnevnika v datoteko. |
| `R/server_cache_management.R` | Periodično čiščenje predpomnilnika in ročne akcije upravljanja predpomnilnika. |
| `R/server_directory_management.R` | Upravljanje delovnega direktorija in direktorija za izhodne datoteke. |
| `R/server_file_handlers.R` | Nalaganje/prenos datotek in kopiranje parametrov med datasetoma. |
| `R/server_filter_management.R` | Zbiranje vrednosti filtrov iz UI in generiranje dinamičnega UI za filtre. |
| `R/server_help_system.R` | Pomoč in dokumentacija znotraj aplikacije. |
| `R/server_multiple_ternary.R` | Del logike za **Multiple Ternary Creator** (starejši/pomožni del, glej opombo v `server_logic.R`). |
| `R/server_status_outputs.R` | Statusni izpisi in povratne informacije uporabniku. |
| `R/server_ui_coordination.R` | Sinhronizacija UI vnosov (npr. posodabljanje izbir stolpcev med zavihki). |

## Statistika in analiza (brez Shiny odvisnosti)

| Datoteka | Opis |
|---|---|
| `R/extreme_value_analysis.R` | Murakami/ASTM E2283 statistika ekstremnih vrednosti: block maxima, Gumbel fit, napoved, goodness-of-fit test (parametrični bootstrap). |
| `R/spatial_clustering_analysis.R` | Clark-Evans test gručenja/naključnosti prostorske razporeditve vključkov, z Donnellyjevo korekcijo robov. |
| `R/compositional_data_analysis.R` | CLR/ILR log-ratio transformacije in PCA za sestavne (Wt%) kemijske podatke. |
| `R/multivariate.R` | Mahalanobisova razdalja, Isolation Forest in validacija vhodnih podatkov za multivariatno analizo. |
| `R/statistical_filters.R` | IQR/Z-score/MAD filtriranje (pozitivnih) osamelcev. |
| `R/stats_display_utils.R` | Priprava DT tabel, statističnih kartic in mini-histogramov za prikaz statistike v zavihku Data Comparison. |

## Risanje grafov

| Datoteka | Opis |
|---|---|
| `R/plotting_utils.R` | Osnovni graditelji grafov za en dataset (korelacija, histogram+gostota, boxplot, scatter matrix), teme in barvne palete. |
| `R/plotting_utils_builder.R` | Splošni graditelj grafa za zavihek **Plot Builder** (violin/box/histogram/scatter/bar). |
| `R/ternary_plot.R` | `general_ternary_plot()` – tanek orkestrator, ki kliče spodnje module za pripravo/risanje/shranjevanje. |
| `R/ternary_plot_data_prep.R` | Priprava podatkov za ternarni graf: nalaganje datoteke, filtri, ternarne koordinate, naslov, barve/oblike točk. |
| `R/ternary_plot_preview.R` | Risanje ternarnega grafa na trenutno aktivno grafično napravo (predogled). |
| `R/ternary_plot_save.R` | Ponovno risanje in shranjevanje ternarnega grafa v datoteko. |
| `R/hex_ternary_plot.R` | Sestavljanje 6 ternarnih grafov v en heksagonalni diagram. |

## Pomožne (helper) funkcije

| Datoteka | Opis |
|---|---|
| `R/helpers.R` | Osrednje pomožne funkcije: beleženje (`log_operation`), čiščenje imen stolpcev, spremljanje zmogljivosti. |
| `R/helpers_filters.R` | Zbiranje filtrov iz Shiny vnosov (`input`) in njihova uporaba na podatkih. |
| `R/helpers_validation.R` | Preverjanje kakovosti in veljavnosti podatkov. |
| `R/helpers_multivariate.R` | Orkestracija multivariatnih analiz (ovojnica okoli `multivariate.R`). |
| `R/helpers_reporting.R` | Generiranje poročil/nadzornih plošč (uporablja `comprehensive_analysis.R`) - korelacijske toplotne karte, distribucijski grafi. |

## Konfiguracija, predpomnjenje, izvoz in ostalo

| Datoteka | Opis |
|---|---|
| `R/config.R` | Upravljanje konfiguracije aplikacije (nastavitve, privzeti direktoriji). |
| `R/options.R` | Konstante in nastavitve, uporabljene po celotni aplikaciji. |
| `R/dependencies.R` | Preverjanje in nalaganje potrebnih R paketov. |
| `R/cache.R` | Sistem predpomnjenja podatkov in grafov s časovnim potekom veljavnosti. |
| `R/cache_performance.R` | Sledenje napredku in zmogljivosti (čas, pomnilnik) posameznih operacij. |
| `R/file_management.R` | Ustvarjanje izhodnih map, poimenovanje datotek, časovni žigi. |
| `R/plot_builder_presets.R` | Shranjevanje/nalaganje uporabniških predlog za zavihek Plot Builder (JSON datoteka). |
| `R/comprehensive_analysis.R` | Celovit analitični cevovod, ki povezuje obstoječe funkcije v enoten potek analize in poročanja. |

## Struktura paketa (izven `R/`)

| Pot | Opis |
|---|---|
| `DESCRIPTION`, `NAMESPACE` | Standardna metapodatka R paketa in seznam izvoženih funkcij. |
| `README.md` | Glavni opis projekta in navodila za uporabo. |
| `vignettes/vidternary-intro.Rmd` | Uvodna vinjeta / vodič po paketu. |
| `tests/testthat/test-modular-structure.R` | Osnovni testi strukture modulov. |
| `testdata/` | Testni Excel podatki (`test_data.xlsx`) z opisom. |
| `legacy/` | Stare/opuščene skripte, ohranjene za referenco (`App6.0.1.R`, `ternary_plot_old.R`, ...) – niso del aktivnega paketa. |
| `renv/` | `renv` upravljalnik R okolja/odvisnosti paketa. |
