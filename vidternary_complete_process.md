# VIDTERNARY - CELOTEN PROCES APLIKACIJE

## Kompletni proces aplikacije z vsemi komponentami

```mermaid
flowchart TD
    START([Uporabnik odpre aplikacijo]) --> INIT[Inicializacija aplikacije]
    INIT --> INIT1[Naloži pakete]
    INIT1 --> INIT2[Preveri odvisnosti]
    INIT2 --> INIT3[Inicializiraj UI]
    INIT3 --> INIT4[Prikaži uporabniški vmesnik]
    
    INIT4 --> UPLOAD[Uporabnik naloži podatke]
    UPLOAD --> UPLOAD1[Dataset 1 - Excel/CSV]
    UPLOAD --> UPLOAD2[Dataset 2 - Excel/CSV]
    
    UPLOAD1 --> READ1[Preberi Dataset 1]
    UPLOAD2 --> READ2[Preberi Dataset 2]
    
    READ1 --> VALIDATE1[Preveri Dataset 1]
    READ2 --> VALIDATE2[Preveri Dataset 2]
    
    VALIDATE1 --> SHOW1[Prikaži podatke Dataset 1]
    VALIDATE2 --> SHOW2[Prikaži podatke Dataset 2]
    
    SHOW1 --> CONFIG[Konfiguracija analize]
    SHOW2 --> CONFIG
    
    CONFIG --> CONFIG1[Izberi elemente A, B, C]
    CONFIG1 --> CONFIG2[Izberi opcijske parametre]
    CONFIG2 --> CONFIG3[Izberi metodo filtriranja]
    CONFIG3 --> CONFIG4[Izberi parametre filtriranja]
    
    CONFIG4 --> APPLY[Uporabi filtriranje]
    APPLY --> APPLY1[Preveri vhodne podatke]
    APPLY1 --> APPLY2[Uporabi izbrano metodo]
    APPLY2 --> APPLY3[Kombiniraj rezultate]
    APPLY3 --> APPLY4[Ustvari filtrirane podatke]
    
    APPLY4 --> PLOT[Ustvari ternary plot]
    PLOT --> PLOT1[Pripravi podatke za plot]
    PLOT1 --> PLOT2[Ustvari ternary plot]
    PLOT2 --> PLOT3[Uporabi stile in barve]
    PLOT3 --> PLOT4[Prikaži plot]
    
    PLOT4 --> EXPORT[Izvozi rezultate]
    EXPORT --> EXPORT1[Izberi format izvoza]
    EXPORT1 --> EXPORT2[Izvozi podatke]
    EXPORT2 --> EXPORT3[Izvozi plot]
    EXPORT3 --> EXPORT4[Shrani datoteke]
    
    EXPORT4 --> SUCCESS[Uspešen izvoz]
    SUCCESS --> CONTINUE{Nadaljuj z analizo?}
    
    CONTINUE -->|DA| CONFIG
    CONTINUE -->|NE| END([Konec])
    
    %% Napake
    INIT2 -->|Napaka| ERROR1[Prikaži napako paketov]
    READ1 -->|Napaka| ERROR2[Prikaži napako nalaganja Dataset 1]
    READ2 -->|Napaka| ERROR3[Prikaži napako nalaganja Dataset 2]
    VALIDATE1 -->|Napaka| ERROR4[Prikaži napako validacije Dataset 1]
    VALIDATE2 -->|Napaka| ERROR5[Prikaži napako validacije Dataset 2]
    APPLY2 -->|Napaka| ERROR6[Prikaži napako filtriranja]
    PLOT2 -->|Napaka| ERROR7[Prikaži napako ustvarjanja plota]
    EXPORT4 -->|Napaka| ERROR8[Prikaži napako shranjevanja]
    
    ERROR1 --> CONFIG
    ERROR2 --> CONFIG
    ERROR3 --> CONFIG
    ERROR4 --> CONFIG
    ERROR5 --> CONFIG
    ERROR6 --> CONFIG
    ERROR7 --> CONFIG
    ERROR8 --> CONFIG
    
    %% Stili
    classDef startEnd fill:#e1f5fe,stroke:#01579b,stroke-width:3px
    classDef process fill:#f3e5f5,stroke:#4a148c,stroke-width:2px
    classDef decision fill:#fff3e0,stroke:#e65100,stroke-width:2px
    classDef error fill:#ffebee,stroke:#c62828,stroke-width:2px
    classDef success fill:#e8f5e8,stroke:#2e7d32,stroke-width:2px
    
    class START,END startEnd
    class INIT,INIT1,INIT2,INIT3,INIT4,UPLOAD,UPLOAD1,UPLOAD2,READ1,READ2,VALIDATE1,VALIDATE2,SHOW1,SHOW2,CONFIG,CONFIG1,CONFIG2,CONFIG3,CONFIG4,APPLY,APPLY1,APPLY2,APPLY3,APPLY4,PLOT,PLOT1,PLOT2,PLOT3,PLOT4,EXPORT,EXPORT1,EXPORT2,EXPORT3,EXPORT4,SUCCESS process
    class CONTINUE decision
    class ERROR1,ERROR2,ERROR3,ERROR4,ERROR5,ERROR6,ERROR7,ERROR8 error
    class SUCCESS success
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

### 3. KONFIGURACIJA ANALIZE
```mermaid
flowchart TD
    CONFIG_START[Začetek konfiguracije] --> SELECT_ELEM[Izberi elemente A, B, C]
    SELECT_ELEM --> SELECT_OPT[Izberi opcijske parametre]
    SELECT_OPT --> SELECT_FILTER[Izberi metodo filtriranja]
    SELECT_FILTER --> SELECT_PARAMS[Izberi parametre filtriranja]
    SELECT_PARAMS --> CONFIG_END[Konfiguracija končana]
```

### 4. FILTRIRANJE PODATKOV
```mermaid
flowchart TD
    FILTER_START[Začetek filtriranja] --> CHECK_DATA[Preveri vhodne podatke]
    CHECK_DATA --> APPLY_METHOD[Uporabi izbrano metodo]
    APPLY_METHOD --> COMBINE[Kombiniraj rezultate]
    COMBINE --> CREATE_FILTERED[Ustvari filtrirane podatke]
    CREATE_FILTERED --> FILTER_END[Filtriranje končano]
```

### 5. USTVARJANJE PLOTA
```mermaid
flowchart LR
    PLOT_START[Začetek ustvarjanja plota] --> PREPARE_DATA[Pripravi podatke]
    PREPARE_DATA --> CREATE_TERNARY[Ustvari ternary plot]
    CREATE_TERNARY --> APPLY_STYLES[Uporabi stile]
    APPLY_STYLES --> DISPLAY_PLOT[Prikaži plot]
    DISPLAY_PLOT --> PLOT_END[Plot končan]
```

### 6. IZVOZ REZULTATOV
```mermaid
flowchart TD
    EXPORT_START[Začetek izvoza] --> SELECT_FORMAT[Izberi format]
    SELECT_FORMAT --> EXPORT_DATA[Izvozi podatke]
    EXPORT_DATA --> EXPORT_PLOT[Izvozi plot]
    EXPORT_PLOT --> SAVE_FILES[Shrani datoteke]
    SAVE_FILES --> EXPORT_END[Izvoz končan]
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
    titlePanel("VIDTERNARY - Ternary Plot Analysis"),
    
    # Sidebar za konfiguracijo
    sidebarLayout(
      sidebarPanel(
        # Nalaganje podatkov
        fileInput("file1", "Dataset 1", accept = c(".xlsx", ".csv")),
        fileInput("file2", "Dataset 2", accept = c(".xlsx", ".csv")),
        
        # Konfiguracija elementov
        selectInput("element_A", "Element A", choices = NULL),
        selectInput("element_B", "Element B", choices = NULL),
        selectInput("element_C", "Element C", choices = NULL),
        
        # Metode filtriranja
        selectInput("filter_method", "Metoda filtriranja", 
                   choices = c("Elementno", "Multivariatna", "Statistično")),
        
        # Parametri filtriranja
        uiOutput("filter_params_ui"),
        
        # Gumbi za analizo
        actionButton("analyze", "Analiziraj"),
        actionButton("export", "Izvozi rezultate")
      ),
      
      # Main panel za prikaz rezultatov
      mainPanel(
        # Prikaz podatkov
        tabsetPanel(
          tabPanel("Podatki", 
                   dataTableOutput("data1_table"),
                   dataTableOutput("data2_table")),
          tabPanel("Plot", 
                   plotOutput("ternary_plot")),
          tabPanel("Rezultati", 
                   verbatimTextOutput("analysis_results")),
          tabPanel("Izvoz", 
                   downloadButton("download_data", "Prenesi podatke"),
                   downloadButton("download_plot", "Prenesi plot"))
        )
      )
    )
  )
}
```

### Server logika
```r
# server_logic.R
create_server <- function(input, output, session) {
  # Reactive values
  rv <- reactiveValues(
    data1 = NULL,
    data2 = NULL,
    filtered_data1 = NULL,
    filtered_data2 = NULL,
    plot = NULL,
    results = NULL
  )
  
  # Event handlers
  observeEvent(input$file1, {
    rv$data1 <- read_dataset_file(input$file1$datapath)
  })
  
  observeEvent(input$file2, {
    rv$data2 <- read_dataset_file(input$file2$datapath)
  })
  
  observeEvent(input$analyze, {
    # Uporabi filtriranje
    rv$filtered_data1 <- apply_filtering(rv$data1, input$filter_method, input$filter_params)
    rv$filtered_data2 <- apply_filtering(rv$data2, input$filter_method, input$filter_params)
    
    # Ustvari plot
    rv$plot <- create_ternary_plot(rv$filtered_data1, rv$filtered_data2, input$elements)
    
    # Shrani rezultate
    rv$results <- list(
      filtered_data1 = rv$filtered_data1,
      filtered_data2 = rv$filtered_data2,
      plot = rv$plot,
      method = input$filter_method,
      params = input$filter_params
    )
  })
  
  # Output renderers
  output$data1_table <- renderDataTable({
    rv$data1
  })
  
  output$data2_table <- renderDataTable({
    rv$data2
  })
  
  output$ternary_plot <- renderPlot({
    rv$plot
  })
  
  output$analysis_results <- renderPrint({
    rv$results
  })
  
  # Download handlers
  output$download_data <- downloadHandler(
    filename = function() {
      paste("filtered_data_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(rv$results$filtered_data1, file, sheetName = "Dataset1")
      write.xlsx(rv$results$filtered_data2, file, sheetName = "Dataset2", append = TRUE)
    }
  )
  
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("ternary_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, rv$plot, width = 12, height = 10, dpi = 300)
    }
  )
}
```

### Filtriranje podatkov
```r
# multivariate.R, statistical_filters.R, ternary_plot.R
apply_filtering <- function(data, method, params) {
  switch(method,
    "Elementno" = apply_element_filtering(data, params),
    "Multivariatna" = apply_multivariate_filtering(data, params),
    "Statistično" = apply_statistical_filtering(data, params),
    stop("Unknown filtering method: ", method)
  )
}
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
