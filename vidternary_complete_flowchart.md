# VIDTERNARY - CELOTEN FLOWCHART APLIKACIJE

## Glavni proces aplikacije z filtriranjem

```mermaid
flowchart TD
    START([Uporabnik odpre aplikacijo]) --> INIT[Inicializacija aplikacije]
    INIT --> LOAD_PKG[Naloži pakete]
    LOAD_PKG --> UI[Prikaži uporabniški vmesnik]
    
    UI --> UPLOAD[Uporabnik naloži podatke]
    UPLOAD --> UPLOAD1[Dataset 1 - Excel/CSV]
    UPLOAD --> UPLOAD2[Dataset 2 - Excel/CSV]
    
    UPLOAD1 --> VALIDATE1[Preveri Dataset 1]
    UPLOAD2 --> VALIDATE2[Preveri Dataset 2]
    
    VALIDATE1 --> SHOW1[Prikaži podatke Dataset 1]
    VALIDATE2 --> SHOW2[Prikaži podatke Dataset 2]
    
    SHOW1 --> CONFIG[Konfiguracija analize]
    SHOW2 --> CONFIG
    
    CONFIG --> CONFIG1[Izberi elemente A, B, C]
    CONFIG --> CONFIG2[Izberi opcijske parametre]
    CONFIG --> CONFIG3[Izberi metodo filtriranja]
    
    CONFIG3 --> FILTER_TYPE{Metoda filtriranja}
    
    %% Elementno filtriranje
    FILTER_TYPE -->|Elementno| ELEM_FILTER[Elementno filtriranje]
    ELEM_FILTER --> ELEM_UI[Prikaži UI za elemente]
    ELEM_UI --> ELEM_SELECT[Uporabnik izbere elemente]
    ELEM_SELECT --> ELEM_FILTER_UI[Prikaži filtre za elemente]
    ELEM_FILTER_UI --> ELEM_APPLY[Uporabi elementne filtre]
    
    %% Multivariatna analiza
    FILTER_TYPE -->|Multivariatna| MULTI_FILTER[Multivariatna analiza]
    MULTI_FILTER --> MULTI_UI[Prikaži UI za multivariatno analizo]
    MULTI_UI --> MULTI_SELECT[Uporabnik izbere stolpce]
    MULTI_SELECT --> MULTI_METHOD{Metoda}
    
    MULTI_METHOD -->|Mahalanobis| MAH_UI[UI za Mahalanobis]
    MULTI_METHOD -->|Robust Mahalanobis| ROB_UI[UI za Robust Mahalanobis]
    MULTI_METHOD -->|Isolation Forest| ISO_UI[UI za Isolation Forest]
    
    MAH_UI --> MAH_PARAMS[Parametri za Mahalanobis]
    ROB_UI --> ROB_PARAMS[Parametri za Robust Mahalanobis]
    ISO_UI --> ISO_PARAMS[Parametri za Isolation Forest]
    
    MAH_PARAMS --> MULTI_APPLY[Uporabi multivariatno analizo]
    ROB_PARAMS --> MULTI_APPLY
    ISO_PARAMS --> MULTI_APPLY
    
    %% Statistično filtriranje
    FILTER_TYPE -->|Statistično| STAT_FILTER[Statistično filtriranje]
    STAT_FILTER --> STAT_UI[Prikaži UI za statistično filtriranje]
    STAT_UI --> STAT_METHOD{Statistična metoda}
    
    STAT_METHOD -->|IQR| IQR_UI[UI za IQR]
    STAT_METHOD -->|Z-score| ZSC_UI[UI za Z-score]
    STAT_METHOD -->|MAD| MAD_UI[UI za MAD]
    
    IQR_UI --> STAT_APPLY[Uporabi statistično filtriranje]
    ZSC_UI --> STAT_APPLY
    MAD_UI --> STAT_APPLY
    
    %% Opcijski parametri
    FILTER_TYPE -->|Opcijski| OPT_FILTER[Opcijski parametri]
    OPT_FILTER --> OPT_UI[Prikaži UI za opcijske parametre]
    OPT_UI --> OPT_SELECT[Uporabnik izbere parametre]
    OPT_SELECT --> OPT_APPLY[Uporabi filtre za opcijske parametre]
    
    %% Uporaba filtriranja
    ELEM_APPLY --> APPLY_FILTER[Uporabi filtriranje na podatke]
    MULTI_APPLY --> APPLY_FILTER
    STAT_APPLY --> APPLY_FILTER
    OPT_APPLY --> APPLY_FILTER
    
    APPLY_FILTER --> FILTER_RESULT[Rezultati filtriranja]
    FILTER_RESULT --> CHOICE{Obdrži outlierje?}
    
    CHOICE -->|DA| KEEP_OUTLIERS[Obdrži samo outlierje]
    CHOICE -->|NE| REMOVE_OUTLIERS[Odstrani outlierje]
    
    KEEP_OUTLIERS --> FILTERED_DATA[Filtrirani podatki]
    REMOVE_OUTLIERS --> FILTERED_DATA
    
    FILTERED_DATA --> PLOT[Ustvari ternary plot]
    PLOT --> PLOT_CONFIG[Konfiguracija plota]
    PLOT_CONFIG --> PLOT_CREATE[Ustvari plot]
    PLOT_CREATE --> PLOT_DISPLAY[Prikaži plot]
    
    PLOT_DISPLAY --> EXPORT[Izvozi rezultate]
    EXPORT --> EXPORT_FORMAT{Format izvoza}
    
    EXPORT_FORMAT -->|PNG| EXPORT_PNG[Izvozi kot PNG]
    EXPORT_FORMAT -->|JPEG| EXPORT_JPEG[Izvozi kot JPEG]
    EXPORT_FORMAT -->|PDF| EXPORT_PDF[Izvozi kot PDF]
    EXPORT_FORMAT -->|TIFF| EXPORT_TIFF[Izvozi kot TIFF]
    EXPORT_FORMAT -->|Excel| EXPORT_EXCEL[Izvozi kot Excel]
    EXPORT_FORMAT -->|CSV| EXPORT_CSV[Izvozi kot CSV]
    
    EXPORT_PNG --> SAVE[Shrani datoteko]
    EXPORT_JPEG --> SAVE
    EXPORT_PDF --> SAVE
    EXPORT_TIFF --> SAVE
    EXPORT_EXCEL --> SAVE
    EXPORT_CSV --> SAVE
    
    SAVE --> SUCCESS[Uspešen izvoz]
    SUCCESS --> CONTINUE{Nadaljuj z analizo?}
    
    CONTINUE -->|DA| CONFIG
    CONTINUE -->|NE| END([Konec])
    
    %% Napake
    VALIDATE1 -->|Napaka| ERROR1[Prikaži napako nalaganja]
    VALIDATE2 -->|Napaka| ERROR2[Prikaži napako nalaganja]
    ELEM_APPLY -->|Napaka| ERROR3[Prikaži napako filtriranja]
    MULTI_APPLY -->|Napaka| ERROR4[Prikaži napako multivariatne analize]
    STAT_APPLY -->|Napaka| ERROR5[Prikaži napako statističnega filtriranja]
    PLOT_CREATE -->|Napaka| ERROR6[Prikaži napako ustvarjanja plota]
    SAVE -->|Napaka| ERROR7[Prikaži napako shranjevanja]
    
    ERROR1 --> CONFIG
    ERROR2 --> CONFIG
    ERROR3 --> CONFIG
    ERROR4 --> CONFIG
    ERROR5 --> CONFIG
    ERROR6 --> CONFIG
    ERROR7 --> CONFIG
    
    %% Stili
    classDef startEnd fill:#e1f5fe,stroke:#01579b,stroke-width:3px
    classDef process fill:#f3e5f5,stroke:#4a148c,stroke-width:2px
    classDef decision fill:#fff3e0,stroke:#e65100,stroke-width:2px
    classDef ui fill:#e8f5e8,stroke:#2e7d32,stroke-width:2px
    classDef filter fill:#fff8e1,stroke:#f57f17,stroke-width:2px
    classDef plot fill:#fce4ec,stroke:#c2185b,stroke-width:2px
    classDef export fill:#f3e5f5,stroke:#7b1fa2,stroke-width:2px
    classDef error fill:#ffebee,stroke:#c62828,stroke-width:2px
    
    class START,END startEnd
    class INIT,LOAD_PKG,VALIDATE1,VALIDATE2,APPLY_FILTER,FILTER_RESULT,FILTERED_DATA,PLOT,PLOT_CONFIG,PLOT_CREATE,PLOT_DISPLAY,SAVE,SUCCESS process
    class UI,SHOW1,SHOW2,CONFIG,CONFIG1,CONFIG2,CONFIG3,ELEM_UI,MULTI_UI,STAT_UI,OPT_UI,PLOT_DISPLAY ui
    class FILTER_TYPE,MULTI_METHOD,STAT_METHOD,CHOICE,EXPORT_FORMAT,CONTINUE decision
    class ELEM_FILTER,MULTI_FILTER,STAT_FILTER,OPT_FILTER,ELEM_APPLY,MULTI_APPLY,STAT_APPLY,OPT_APPLY filter
    class PLOT,PLOT_CONFIG,PLOT_CREATE plot
    class EXPORT,EXPORT_PNG,EXPORT_JPEG,EXPORT_PDF,EXPORT_TIFF,EXPORT_EXCEL,EXPORT_CSV export
    class ERROR1,ERROR2,ERROR3,ERROR4,ERROR5,ERROR6,ERROR7 error
```

## Podroben opis komponent

### 1. INICIALIZACIJA APLIKACIJE
```mermaid
flowchart LR
    INIT_START[Začetek] --> LOAD_DEPS[Naloži odvisnosti]
    LOAD_DEPS --> CHECK_PKG[Preveri pakete]
    CHECK_PKG --> INIT_UI[Inicializiraj UI]
    INIT_UI --> READY[Pripravljen za uporabo]
```

### 2. NALAGANJE PODATKOV
```mermaid
flowchart TD
    UPLOAD_START[Uporabnik izbere datoteko] --> READ_FILE[Preberi datoteko]
    READ_FILE --> CHECK_FORMAT{Format datoteke}
    CHECK_FORMAT -->|Excel| READ_EXCEL[openxlsx::read.xlsx]
    CHECK_FORMAT -->|CSV| READ_CSV[read.csv]
    CHECK_FORMAT -->|Drugo| ERROR_FORMAT[Napaka: Nepodprt format]
    
    READ_EXCEL --> VALIDATE_DATA[Preveri podatke]
    READ_CSV --> VALIDATE_DATA
    VALIDATE_DATA --> SHOW_DATA[Prikaži podatke]
    ERROR_FORMAT --> UPLOAD_START
```

### 3. KONFIGURACIJA FILTRIRANJA
```mermaid
flowchart TD
    CONFIG_START[Začetek konfiguracije] --> SELECT_ELEM[Izberi elemente A, B, C]
    SELECT_ELEM --> SELECT_OPT[Izberi opcijske parametre]
    SELECT_OPT --> SELECT_FILTER[Izberi metodo filtriranja]
    SELECT_FILTER --> CONFIG_END[Konfiguracija končana]
```

### 4. USTVARJANJE PLOTA
```mermaid
flowchart LR
    PLOT_START[Začetek ustvarjanja plota] --> PLOT_DATA[Pripravi podatke]
    PLOT_DATA --> PLOT_TERNARY[Ustvari ternary plot]
    PLOT_TERNARY --> PLOT_STYLE[Uporabi stile]
    PLOT_STYLE --> PLOT_SAVE[Shrani plot]
    PLOT_SAVE --> PLOT_END[Plot končan]
```

### 5. IZVOZ REZULTATOV
```mermaid
flowchart TD
    EXPORT_START[Začetek izvoza] --> SELECT_FORMAT[Izberi format]
    SELECT_FORMAT --> EXPORT_DATA[Izvozi podatke]
    EXPORT_DATA --> EXPORT_PLOT[Izvozi plot]
    EXPORT_PLOT --> EXPORT_SAVE[Shrani datoteke]
    EXPORT_SAVE --> EXPORT_END[Izvoz končan]
```

## Ključne funkcije aplikacije

### Glavna aplikacija
```r
# app.R
shinyApp(
  ui = create_ui(),
  server = create_server()
)
```

### Ustvarjanje UI
```r
# ui_components.R
create_ui <- function() {
  fluidPage(
    # Header
    # Sidebar za konfiguracijo
    # Main panel za prikaz rezultatov
  )
}
```

### Server logika
```r
# server_logic.R
create_server <- function(input, output, session) {
  # Reactive values
  # Event handlers
  # Output renderers
}
```

### Filtriranje podatkov
```r
# multivariate.R, statistical_filters.R, ternary_plot.R
# Različne metode filtriranja
```

## Uporaba flowchart-a

1. **Razumevanje aplikacije**: Sledite glavnemu toku za razumevanje, kako deluje aplikacija
2. **Debugiranje**: Identificirajte, kje se pojavi napaka
3. **Razvoj**: Dodajte nove funkcionalnosti po vzoru obstoječih
4. **Dokumentacija**: Uporabite za razlago uporabnikom
5. **Testiranje**: Preverite vse možne poti skozi aplikacijo

## Napake in obravnavanje

- **Nalaganje podatkov**: Preverjanje formata in veljavnosti
- **Filtriranje**: Preverjanje parametrov in podatkov
- **Ustvarjanje plota**: Preverjanje podatkov in konfiguracije
- **Izvoz**: Preverjanje dovoljenj in prostora

## Optimizacija

- **Cachiranje**: Shranjevanje vmesnih rezultatov
- **Asinhrono nalaganje**: Nalaganje podatkov v ozadju
- **Validacija**: Preverjanje vhodnih podatkov pred obdelavo
- **Napake**: Robustno obravnavanje napak z uporabnimi sporočili
