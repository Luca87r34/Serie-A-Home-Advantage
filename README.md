# Analisi del Fattore Campo in Serie A (2016-2026)

Progetto di Data Analysis in R per verificare l'impatto del pubblico e delle restrizioni pandemiche (porte chiuse) sulle prestazioni casalinghe.

## Strumenti Utilizzati
* **Linguaggio:** R
* **Librerie:** `tidyverse` (`dplyr`, `ggplot2`)
* **Test Statistico:** Welch Two Sample t-test

## Principali Evidenze
* **Analisi Temporale:** Tra il 2019 e il 2022 si osserva una contrazione nel margine gol casalingo, seguita da una rapida risalita nelle stagioni successive.
* **Validazione Statistica:** Il t-test condotto sulla differenza gol media (Con Pubblico vs Porte Chiuse) ha restituito un **p-value = 0.9191**. Non vi è alcuna differenza statisticamente significativa nel lungo periodo, ridimensionando l'idea di un annullamento permanente del fattore campo.
