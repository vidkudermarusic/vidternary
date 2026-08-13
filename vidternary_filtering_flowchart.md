# VIDTERNARY - FLOWCHART ZA FILTRIRANJE PODATKOV

## Mermaid Flowchart Koda

```mermaid
flowchart TD
    A[Začetek - Naloženi podatki] --> B{Preveri tip filtriranja}
    
    B -->|Elementno filtriranje| C[Elementno filtriranje]
    B -->|Multivariatna analiza| D[Multivariatna analiza]
    B -->|Statistično filtriranje| E[Statistično filtriranje]
    B -->|Opcijski parametri| F[Opcijski parametri]
    
    %% Elementno filtriranje
    C --> C1[Izberi elemente A, B, C]
    C1 --> C2{Posamezni filtri?}
    C2 -->|DA| C3[Uporabi posamezne filtre za vsak element]
    C2 -->|NE| C4[Uporabi en filter za vse elemente]
    C3 --> C5[Preveri operatorje: >, <, >=, <=, ==, !=]
    C4 --> C5
    C5 --> C6[Pretvori v numerične vrednosti]
    C6 --> C7[Uporabi filtriranje na podatke]
    C7 --> C8[Vrne filtrirane podatke]
    
    %% Multivariatna analiza
    D --> D1[Izberi stolpce za analizo]
    D1 --> D2{Metoda analize}
    D2 -->|Mahalanobis| D3[Mahalanobis razdalja]
    D2 -->|Robust Mahalanobis| D4[Robust Mahalanobis MCD/MVE]
    D2 -->|Isolation Forest| D5[Isolation Forest]
    
    D3 --> D6[Izračunaj kovariančno matriko]
    D6 --> D7[Preveri singularnost matrike]
    D7 --> D8[Izračunaj Mahalanobis razdalje]
    D8 --> D9[Nastavi prag za outlierje]
    D9 --> D10[Označi outlierje]
    
    D4 --> D11[Uporabi robustno kovarianco MCD/MVE]
    D11 --> D12[Preveri singularnost]
    D12 --> D13[Izračunaj robustne razdalje]
    D13 --> D14[Nastavi robustni prag]
    D14 --> D15[Označi outlierje]
    
    D5 --> D16[Nastavi parametre: ntrees, sample_size, contamination]
    D16 --> D17[Treniraj model na referenčnih podatkih]
    D17 --> D18[Izračunaj prag iz referenčnih score-ov]
    D18 --> D19[Predvidi score za analizirane podatke]
    D19 --> D20[Označi outlierje glede na prag]
    
    D10 --> D21[Vrne rezultate multivariatne analize]
    D15 --> D21
    D20 --> D21
    
    %% Statistično filtriranje
    E --> E1{Metoda statističnega filtriranja}
    E1 -->|IQR| E2[Interquartile Range filtriranje]
    E1 -->|Z-score| E3[Z-score filtriranje]
    E1 -->|MAD| E4[Median Absolute Deviation filtriranje]
    
    E2 --> E5[Izračunaj Q1 in Q3]
    E5 --> E6[Izračunaj IQR = Q3 - Q1]
    E6 --> E7[Nastavi meje: Q1 - 1.5*IQR, Q3 + 1.5*IQR]
    E7 --> E8[Označi vrednosti zunaj mej kot outlierje]
    
    E3 --> E9[Izračunaj povprečje in standardni odklon]
    E9 --> E10[Izračunaj Z-score za vsako vrednost]
    E10 --> E11[Označi vrednosti z |Z-score| > prag kot outlierje]
    
    E4 --> E12[Izračunaj mediano]
    E12 --> E13[Izračunaj MAD]
    E13 --> E14[Nastavi meje: mediana ± prag*MAD]
    E14 --> E15[Označi vrednosti zunaj mej kot outlierje]
    
    E8 --> E16[Vrne statistično filtrirane podatke]
    E11 --> E16
    E15 --> E16
    
    %% Opcijski parametri
    F --> F1[Opcijski parameter 1]
    F1 --> F2[Opcijski parameter 2]
    F2 --> F3[Uporabi filtre za opcijske parametre]
    F3 --> F4[Preveri operatorje in vrednosti]
    F4 --> F5[Uporabi filtriranje]
    F5 --> F6[Vrne filtrirane podatke z opcijskimi parametri]
    
    %% Končni rezultati
    C8 --> G[Kombiniraj rezultate]
    D21 --> G
    E16 --> G
    F6 --> G
    
    G --> H{Obdrži ali odstrani outlierje?}
    H -->|Obdrži| I[Obdrži samo outlierje]
    H -->|Odstrani| J[Odstrani outlierje]
    
    I --> K[Končni filtrirani podatki]
    J --> K
    
    K --> L[Izvozi rezultate]
    L --> M[Konec]
    
    %% Stili
    classDef startEnd fill:#e1f5fe,stroke:#01579b,stroke-width:2px
    classDef process fill:#f3e5f5,stroke:#4a148c,stroke-width:2px
    classDef decision fill:#fff3e0,stroke:#e65100,stroke-width:2px
    classDef multivariate fill:#e8f5e8,stroke:#2e7d32,stroke-width:2px
    classDef statistical fill:#fff8e1,stroke:#f57f17,stroke-width:2px
    classDef element fill:#fce4ec,stroke:#c2185b,stroke-width:2px
    
    class A,M startEnd
    class C1,C3,C4,C5,C6,C7,C8,F1,F2,F3,F4,F5,F6 process
    class B,C2,D2,E1,H decision
    class D1,D3,D4,D5,D6,D7,D8,D9,D10,D11,D12,D13,D14,D15,D16,D17,D18,D19,D20,D21 multivariate
    class E2,E3,E4,E5,E6,E7,E8,E9,E10,E11,E12,E13,E14,E15,E16 statistical
    class C element
```

## Opis procesa filtriranja v VIDTERNARY

### 1. ELEMENTNO FILTRIRANJE
- **Namen**: Filtriranje podatkov glede na izbrane elemente (A, B, C)
- **Tipi**: Posamezni filtri za vsak element ali en filter za vse elemente
- **Operatorji**: >, <, >=, <=, ==, !=
- **Proces**: Pretvorba v numerične vrednosti → Uporaba operatorjev → Filtriranje

### 2. MULTIVARIATNA ANALIZA
- **Mahalanobis razdalja**: Standardna multivariatna analiza z kovariančno matriko
- **Robust Mahalanobis**: Uporaba MCD/MVE za robustno oceno kovariance
- **Isolation Forest**: Napredna metoda za zaznavanje outlierjev z drevesi

### 3. STATISTIČNO FILTRIRANJE
- **IQR**: Interquartile Range - odstrani vrednosti zunaj Q1-1.5*IQR in Q3+1.5*IQR
- **Z-score**: Standardizirane vrednosti - odstrani vrednosti z |Z-score| > prag
- **MAD**: Median Absolute Deviation - robustna alternativa standardnemu odklonu

### 4. OPCIJSKI PARAMETRI
- **Parameter 1**: Velikost točk ali tip točk
- **Parameter 2**: Barva točk
- **Filtriranje**: Možnost filtriranja tudi za opcijske parametre

### 5. KONČNI REZULTATI
- **Izbira**: Obdrži outlierje ali jih odstrani
- **Izvoz**: Shrani filtrirane podatke v različnih formatih
- **Vizualizacija**: Prikaži rezultate v ternary plotih

## Ključne značilnosti

### Varnost
- Vsi filtri uporabljajo varno filtriranje brez `eval()`
- Preverjanje veljavnosti operatorjev in vrednosti
- Robustno obravnavanje napak

### Fleksibilnost
- Podpora za različne tipe filtriranja
- Možnost kombiniranja različnih metod
- Prilagodljivi parametri za vsako metodo

### Uporabnost
- Intuitivni operatorji za filtriranje
- Jasni opisi in pomoč za uporabnike
- Avtomatsko preverjanje podatkov pred filtriranjem
