# OBIETTIVO: Un'analisi statistica rigorosa 
#per dimostrare se giocare in casa influisce davvero sul risultato.

install.packages("readr")
library(readr)

seriea1617<- read_csv('1617.csv')
seriea1718<- read_csv('1718.csv')
seriea1819<- read_csv('1819.csv')
seriea1920<- read_csv('1920.csv')
seriea2021<- read_csv('2021.csv')
seriea2122<- read_csv('2122.csv')
seriea2223<- read_csv('2223.csv')
seriea2324<- read_csv('2324.csv')
seriea2425<- read_csv('2425.csv')
seriea2526<- read_csv('2526.csv')

# 1. Carica il pacchetto per la manipolazione dati
install.packages("dplyr")
library(dplyr)

# 2. Unisci i file e seleziona solo le colonne fondamentali per l'Home Advantage
dataset_completo <- bind_rows(
  seriea1617, seriea1718, seriea1819, seriea1920,
  seriea2021, seriea2122, seriea2223, seriea2324,
  seriea2425, seriea2526
) %>% 
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)

# 3. Rinomina le colonne in italiano per comodità
dataset_completo <- dataset_completo %>% 
  rename(
    Data = Date,
    SquadraCasa = HomeTeam,
    SquadraOspite = AwayTeam,
    GolCasa = FTHG,
    GolOspite = FTAG,
    Risultato = FTR
  )

# Calcola la media gol in casa vs trasferta
dataset_completo %>% 
  summarise(
    Media_Gol_Casa = mean(GolCasa, na.rm = TRUE),         #per rimuovere le celle NA  (1.48)
    Media_Gol_Trasferta = mean(GolOspite, na.rm = TRUE),  #per rimuovere le celle NA  (1.26)
    Totale_Partite = n()                                  #per contare le partite analizzate  (3800)
  )


#Differenza Gol
dataset_completo <- dataset_completo %>% 
  mutate(DiffGol = GolCasa - GolOspite)

# Convertiamo prima la Data in formato data reale per R (se non lo è già)
# Nota: i file di Football-Data usano il formato giorno/mese/anno
dataset_completo <- dataset_completo %>% 
  mutate(
    # 1. Assegnazione Punti alla squadra di casa
    PuntiCasa = case_when(
      Risultato == "H" ~ 3,
      Risultato == "D" ~ 1,
      Risultato == "A" ~ 0
    ),
    
    # 2. Identificazione partite a porte chiuse
    # (In Italia le porte chiuse/capienza ridotta sono state principalmente da Marzo 2020 a Maggio 2021)
    Data_Formattata = as.Date(Data, format = "%d/%m/%Y"),
    Pubblico = if_else(
      Data_Formattata >= as.Date("2020-03-08") & Data_Formattata <= as.Date("2021-05-23"),
      "Porte Chiuse",
      "Con Pubblico"
    )
  )

dataset_completo %>% 
  group_by(Pubblico) %>% 
  summarise(
    Partite = n(),
    Media_Punti_Casa = mean(PuntiCasa, na.rm = TRUE),
    Media_Gol_Casa = mean(GolCasa, na.rm = TRUE),
    Media_Diff_Gol = mean(DiffGol, na.rm = TRUE),
    Percentuale_Vittorie_Casa = mean(Risultato == "H", na.rm = TRUE) * 100
  )

head(dataset_comp)
install.packages("ggplot2")
library(ggplot2)

dati_grafico <- dataset_completo %>% 
  group_by(Pubblico) %>% 
  summarise(Media_Punti_Casa = mean(PuntiCasa, na.rm = TRUE))

viz <- ggplot(data = dati_grafico, aes(x = Pubblico, y = Media_Punti_Casa, fill = Pubblico)) +
  geom_col(width = 0.5) +
  labs(
    title = "Impatto del Pubblico sui Punti in Casa",
    x = "Condizione Stadio",
    y = "Media Punti Casa a Partita"
  ) +
  theme_minimal()

# Per visualizzare il grafico:
viz

# 1. Creiamo la colonna Stagione basandoci sulla data della partita
dataset_completo <- dataset_completo %>% 
  mutate(
    Anno = as.numeric(format(Data_Formattata, "%Y")),
    Mese = as.numeric(format(Data_Formattata, "%m")),
    Stagione = if_else(Mese >= 8, 
                       paste0(Anno, "/", Anno + 1), 
                       paste0(Anno - 1, "/", Anno))
  )

# 2. Aggreghiamo i dati per Stagione
dati_stagione <- dataset_completo %>% 
  group_by(Stagione) %>% 
  summarise(
    Media_Diff_Gol = mean(DiffGol, na.rm = TRUE),
    Perc_Vittorie_Casa = mean(Risultato == "H", na.rm = TRUE) * 100
  )

# 3. Generiamo il grafico a linee
viz_stagione <- ggplot(data = dati_stagione, aes(x = Stagione, y = Media_Diff_Gol, group = 1)) +
  geom_line(color = "#1f77b4", linewidth = 1.2) +
  geom_point(color = "#d62728", size = 3) +
  labs(
    title = "Andamento del Fattore Campo in Serie A",
    subtitle = "Media differenza gol casalinga per stagione",
    x = "Stagione",
    y = "Media Differenza Gol (Casa - Ospiti)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

viz_stagione

t.test(DiffGol ~ Pubblico, data = dataset_completo)

#Il t-test di Student (p-value = 0,9191$) dimostra che, sull'intero storico,
#non esiste una differenza statisticamente significativa nella differenza gol casalinga 
#tra partite con pubblico ($\mu = 0,218$) e a porte chiuse ($\mu = 0,227$).