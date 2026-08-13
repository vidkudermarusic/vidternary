# VIDTERNARY - IMPLEMENTACIJA FILTRIRANJA

## Detajlna implementacija filtriranja

```mermaid
flowchart TD
    START([Začetek filtriranja]) --> INPUT[Vhodni podatki]
    INPUT --> INPUT1[data1: DataFrame]
    INPUT --> INPUT2[data2: DataFrame]
    INPUT --> INPUT3[selected_columns: Character]
    INPUT --> INPUT4[filter_params: List]
    
    INPUT1 --> VALIDATE[Preveri vhodne podatke]
    INPUT2 --> VALIDATE
    INPUT3 --> VALIDATE
    INPUT4 --> VALIDATE
    
    VALIDATE --> VALIDATE1[Preveri data.frame]
    VALIDATE1 --> VALIDATE2[Preveri selected_columns]
    VALIDATE2 --> VALIDATE3[Preveri filter_params]
    VALIDATE3 --> VALIDATE4[Preveri numeričnost stolpcev]
    
    VALIDATE4 --> FILTER_TYPE{Metoda filtriranja}
    
    %% Elementno filtriranje
    FILTER_TYPE -->|Elementno| ELEM[Elementno filtriranje]
    ELEM --> ELEM1[apply_individual_filters]
    ELEM1 --> ELEM2[Preveri element A]
    ELEM2 --> ELEM3[Preveri element B]
    ELEM3 --> ELEM4[Preveri element C]
    
    ELEM4 --> ELEM5{Posamezni filtri?}
    ELEM5 -->|DA| ELEM6[Uporabi individual_filters]
    ELEM5 -->|NE| ELEM7[Uporabi element$filter]
    
    ELEM6 --> ELEM8[apply_filter za vsak element]
    ELEM7 --> ELEM8
    ELEM8 --> ELEM9[Preveri operatorje]
    ELEM9 --> ELEM10[Pretvori v numerične vrednosti]
    ELEM10 --> ELEM11[Uporabi filtriranje]
    ELEM11 --> ELEM12[Vrne filtrirane podatke]
    
    %% Multivariatna analiza
    FILTER_TYPE -->|Multivariatna| MULTI[Multivariatna analiza]
    MULTI --> MULTI1[validate_multivariate_data]
    MULTI1 --> MULTI2[Preveri stolpce]
    MULTI2 --> MULTI3[Preveri opazovanja]
    MULTI3 --> MULTI4[Preveri korelacije]
    MULTI4 --> MULTI5[Preveri numerično stabilnost]
    
    MULTI5 --> MULTI_METHOD{Metoda}
    MULTI_METHOD -->|Mahalanobis| MAH[compute_mahalanobis_distance]
    MULTI_METHOD -->|Robust Mahalanobis| ROB[compute_robust_mahalanobis]
    MULTI_METHOD -->|Isolation Forest| ISO[compute_isolation_forest]
    
    MAH --> MAH1[Izračunaj kovarianco]
    MAH1 --> MAH2[Preveri singularnost]
    MAH2 --> MAH3[Izračunaj razdalje]
    MAH3 --> MAH4[Nastavi prag]
    MAH4 --> MAH5[Označi outlierje]
    
    ROB --> ROB1[Uporabi MCD/MVE]
    ROB1 --> ROB2[Robustna kovarianca]
    ROB2 --> ROB3[Robustne razdalje]
    ROB3 --> ROB4[Robustni prag]
    ROB4 --> ROB5[Označi outlierje]
    
    ISO --> ISO1[Nastavi parametre]
    ISO1 --> ISO2[Treniraj model]
    ISO2 --> ISO3[Izračunaj score]
    ISO3 --> ISO4[Nastavi prag]
    ISO4 --> ISO5[Označi outlierje]
    
    MAH5 --> MULTI_RES[Rezultati multivariatne analize]
    ROB5 --> MULTI_RES
    ISO5 --> MULTI_RES
    
    %% Statistično filtriranje
    FILTER_TYPE -->|Statistično| STAT[Statistično filtriranje]
    STAT --> STAT_METHOD{Statistična metoda}
    
    STAT_METHOD -->|IQR| IQR[apply_iqr_filter]
    STAT_METHOD -->|Z-score| ZSC[apply_zscore_filter]
    STAT_METHOD -->|MAD| MAD[apply_mad_filter]
    
    IQR --> IQR1[Izračunaj Q1, Q3]
    IQR1 --> IQR2[Izračunaj IQR]
    IQR2 --> IQR3[Nastavi meje]
    IQR3 --> IQR4[Označi outlierje]
    
    ZSC --> ZSC1[Izračunaj povprečje, std]
    ZSC1 --> ZSC2[Izračunaj Z-score]
    ZSC2 --> ZSC3[Preveri prag]
    ZSC3 --> ZSC4[Označi outlierje]
    
    MAD --> MAD1[Izračunaj mediano]
    MAD1 --> MAD2[Izračunaj MAD]
    MAD2 --> MAD3[Nastavi meje]
    MAD3 --> MAD4[Označi outlierje]
    
    IQR4 --> STAT_RES[Rezultati statističnega filtriranja]
    ZSC4 --> STAT_RES
    MAD4 --> STAT_RES
    
    %% Kombiniranje rezultatov
    ELEM12 --> COMBINE[Kombiniraj rezultate]
    MULTI_RES --> COMBINE
    STAT_RES --> COMBINE
    
    COMBINE --> COMBINE1[Združi outlier_indices]
    COMBINE1 --> COMBINE2[Združi scores]
    COMBINE2 --> COMBINE3[Združi thresholds]
    COMBINE3 --> COMBINE4[Združi metode]
    
    COMBINE4 --> FINAL_CHOICE{Obdrži outlierje?}
    FINAL_CHOICE -->|DA| KEEP[Obdrži samo outlierje]
    FINAL_CHOICE -->|NE| REMOVE[Odstrani outlierje]
    
    KEEP --> FINAL_DATA[Končni filtrirani podatki]
    REMOVE --> FINAL_DATA
    
    FINAL_DATA --> RETURN[Vrne rezultate]
    RETURN --> RETURN1[outlier_indices]
    RETURN --> RETURN2[scores]
    RETURN --> RETURN3[thresholds]
    RETURN --> RETURN4[methods]
    RETURN --> RETURN5[filtered_data]
    
    RETURN1 --> END([Konec])
    RETURN2 --> END
    RETURN3 --> END
    RETURN4 --> END
    RETURN5 --> END
    
    %% Napake
    VALIDATE1 -->|Napaka| ERROR1[Stop: Neveljavni podatki]
    VALIDATE2 -->|Napaka| ERROR2[Stop: Neveljavni stolpci]
    VALIDATE3 -->|Napaka| ERROR3[Stop: Neveljavni parametri]
    VALIDATE4 -->|Napaka| ERROR4[Stop: Nenumerični stolpci]
    
    ELEM9 -->|Napaka| ERROR5[Stop: Neveljavni operatorji]
    ELEM10 -->|Napaka| ERROR6[Stop: Neveljavne vrednosti]
    
    MAH2 -->|Napaka| ERROR7[Stop: Singularna matrika]
    ROB2 -->|Napaka| ERROR8[Stop: Napaka robustne kovariance]
    ISO2 -->|Napaka| ERROR9[Stop: Napaka treniranja modela]
    
    ERROR1 --> END
    ERROR2 --> END
    ERROR3 --> END
    ERROR4 --> END
    ERROR5 --> END
    ERROR6 --> END
    ERROR7 --> END
    ERROR8 --> END
    ERROR9 --> END
    
    %% Stili
    classDef startEnd fill:#e1f5fe,stroke:#01579b,stroke-width:3px
    classDef process fill:#f3e5f5,stroke:#4a148c,stroke-width:2px
    classDef decision fill:#fff3e0,stroke:#e65100,stroke-width:2px
    classDef element fill:#fce4ec,stroke:#c2185b,stroke-width:2px
    classDef multivariate fill:#e8f5e8,stroke:#2e7d32,stroke-width:2px
    classDef statistical fill:#fff8e1,stroke:#f57f17,stroke-width:2px
    classDef error fill:#ffebee,stroke:#c62828,stroke-width:2px
    classDef result fill:#f3e5f5,stroke:#7b1fa2,stroke-width:2px
    
    class START,END startEnd
    class INPUT,INPUT1,INPUT2,INPUT3,INPUT4,VALIDATE,VALIDATE1,VALIDATE2,VALIDATE3,VALIDATE4,COMBINE,COMBINE1,COMBINE2,COMBINE3,COMBINE4,FINAL_DATA,RETURN,RETURN1,RETURN2,RETURN3,RETURN4,RETURN5 process
    class FILTER_TYPE,ELEM5,MULTI_METHOD,STAT_METHOD,FINAL_CHOICE decision
    class ELEM,ELEM1,ELEM2,ELEM3,ELEM4,ELEM6,ELEM7,ELEM8,ELEM9,ELEM10,ELEM11,ELEM12 element
    class MULTI,MULTI1,MULTI2,MULTI3,MULTI4,MULTI5,MAH,ROB,ISO,MAH1,MAH2,MAH3,MAH4,MAH5,ROB1,ROB2,ROB3,ROB4,ROB5,ISO1,ISO2,ISO3,ISO4,ISO5,MULTI_RES multivariate
    class STAT,IQR,ZSC,MAD,IQR1,IQR2,IQR3,IQR4,ZSC1,ZSC2,ZSC3,ZSC4,MAD1,MAD2,MAD3,MAD4,STAT_RES statistical
    class ERROR1,ERROR2,ERROR3,ERROR4,ERROR5,ERROR6,ERROR7,ERROR8,ERROR9 error
    class KEEP,REMOVE result
```

## Ključne funkcije v implementaciji

### 1. Varno filtriranje
```r
apply_filter <- function(df, col, filter) {
  if (is.null(filter)) return(df)
  
  # Varno filtriranje brez eval()
  if (grepl("^[><=!]+", filter)) {
    operator <- gsub("^([><=!]+).*", "\\1", filter)
    value_str <- gsub("^[><=!]+\\s*", "", filter)
    value <- as.numeric(value_str)
    
    if (is.na(value)) {
      stop("Invalid filter value: ", value_str)
    }
    
    switch(operator,
      ">" = df[df[[col]] > value, ],
      "<" = df[df[[col]] < value, ],
      ">=" = df[df[[col]] >= value, ],
      "<=" = df[df[[col]] <= value, ],
      "==" = df[df[[col]] == value, ],
      "!=" = df[df[[col]] != value, ],
      stop("Invalid operator: ", operator)
    )
  } else {
    stop("Invalid filter format")
  }
}
```

### 2. Validacija multivariatnih podatkov
```r
validate_multivariate_data <- function(data1, data2, selected_columns, method, min_obs_ratio) {
  # Preveri obstoj stolpcev
  missing_in_data1 <- setdiff(selected_columns, colnames(data1))
  missing_in_data2 <- setdiff(selected_columns, colnames(data2))
  
  if (length(missing_in_data1) > 0) {
    stop("Selected columns missing in data1: ", paste(missing_in_data1, collapse = ", "))
  }
  if (length(missing_in_data2) > 0) {
    stop("Selected columns missing in data2: ", paste(missing_in_data2, collapse = ", "))
  }
  
  # Preveri numeričnost
  non_numeric_in_data1 <- selected_columns[!sapply(data1[, selected_columns, drop = FALSE], is.numeric)]
  non_numeric_in_data2 <- selected_columns[!sapply(data2[, selected_columns, drop = FALSE], is.numeric)]
  
  if (length(non_numeric_in_data1) > 0) {
    stop("Non-numeric selected columns in data1: ", paste(non_numeric_in_data1, collapse = ", "))
  }
  if (length(non_numeric_in_data2) > 0) {
    stop("Non-numeric selected columns in data2: ", paste(non_numeric_in_data2, collapse = ", "))
  }
  
  # Preveri minimalno število opazovanj
  n_vars <- length(selected_columns)
  n_obs1 <- nrow(data1_clean)
  n_obs2 <- nrow(data2_clean)
  
  if (n_obs1 < n_vars * min_obs_ratio) {
    stop(sprintf("Insufficient observations in data1: %d observations for %d variables", n_obs1, n_vars))
  }
  if (n_obs2 < n_vars * min_obs_ratio) {
    stop(sprintf("Insufficient observations in data2: %d observations for %d variables", n_obs2, n_vars))
  }
  
  # Preveri korelacije
  if (n_vars > 2) {
    cor_matrix1 <- cor(data1_clean, use = "pairwise.complete.obs")
    cor_matrix2 <- cor(data2_clean, use = "pairwise.complete.obs")
    
    high_cor1 <- which(abs(cor_matrix1) > 0.9 & cor_matrix1 != 1, arr.ind = TRUE)
    high_cor2 <- which(abs(cor_matrix2) > 0.9 & cor_matrix2 != 1, arr.ind = TRUE)
    
    if (nrow(high_cor1) > 0 || nrow(high_cor2) > 0) {
      warning("High correlations (>0.9) detected. This may cause multicollinearity issues.")
    }
  }
  
  # Preveri numerično stabilnost
  tryCatch({
    cov_matrix <- cov(data2_clean)
    eigenvals <- eigen(cov_matrix, only.values = TRUE)$values
    condition_number <- max(eigenvals) / min(eigenvals)
    
    if (condition_number > 1e10) {
      warning("High condition number detected. This may indicate numerical instability.")
    }
  }, error = function(e) {
    warning("Could not calculate condition number: ", e$message)
  })
  
  return(list(
    data1_clean = data1_clean,
    data2_clean = data2_clean,
    common_cols = selected_columns,
    n_vars = n_vars,
    n_obs1 = n_obs1,
    n_obs2 = n_obs2,
    condition_number = condition_number,
    high_correlations = high_correlations
  ))
}
```

### 3. Robustno obravnavanje napak
```r
tryCatch({
  # Glavna logika
  result <- main_function()
  return(result)
}, error = function(e) {
  # Logiranje napake
  debug_log("ERROR: %s", e$message)
  
  # Fallback logika
  warning("Operation failed, using fallback: ", e$message)
  
  # Vrne varni rezultat
  return(safe_fallback())
})
```

### 4. Kombiniranje rezultatov
```r
combine_outlier_results <- function(results) {
  if (is.null(results) || length(results) == 0) {
    return(NULL)
  }
  
  # Združi outlier_indices
  outlier_indices <- Reduce(`|`, lapply(results, function(x) x$outlier_indices))
  
  # Združi scores
  scores <- do.call(cbind, lapply(results, function(x) x$scores))
  colnames(scores) <- names(results)
  
  # Združi thresholds
  thresholds <- sapply(results, function(x) x$threshold)
  names(thresholds) <- names(results)
  
  # Združi metode
  methods <- sapply(results, function(x) x$method)
  names(methods) <- names(results)
  
  return(list(
    outlier_indices = outlier_indices,
    scores = scores,
    thresholds = thresholds,
    methods = methods,
    combined = TRUE
  ))
}
```

## Prednosti implementacije

1. **Varnost**: Varno filtriranje brez eval()
2. **Robustnost**: Robustno obravnavanje napak
3. **Fleksibilnost**: Podpora za različne tipe filtriranja
4. **Validacija**: Preverjanje vhodnih podatkov
5. **Kombiniranje**: Možnost kombiniranja različnih metod
6. **Dokumentacija**: Jasna dokumentacija in opisi

## Uporaba flowchart-a

- **Razumevanje**: Sledite korakom za razumevanje implementacije
- **Debugiranje**: Identificirajte, kje se pojavi napaka
- **Razvoj**: Dodajte nove metode po vzoru obstoječih
- **Optimizacija**: Identificirajte možne izboljšave
- **Testiranje**: Preverite vse možne poti skozi kodo
