################################################### Libraries ####################################################################################
if (!require("ggcorrplot")) {
  install.packages("ggcorrplot")
  library(ggcorrplot)
}

if (!require("tidyverse")) {
  install.packages("tidyverse")
  library(tidyverse)
}

if (!require("patchwork")) {
  install.packages("patchwork")
  library(patchwork)
}

if (!require("CADFtest")) {
  install.packages("CADFtest")
  library(CADFtest)
}

if (!require("fDMA")) {
  install.packages("fDMA")
  library(fDMA)
}

if (!require("rugarch")) {
  install.packages("rugarch")
  library(rugarch)
}

if (!require("fGarch")) {
  install.packages("fGarch")
  library(fGarch)
}

if (!require("rmgarch")) {
  install.packages("rmgarch")
  library(rmgarch)
}

if (!require("moments")) {
  install.packages("moments")
  library(moments)
}

if (!require("qqplotr")) {
  install.packages("qqplotr")
  library(qqplotr)
}
##################################################################################################################################################

################################################### Environmental Setting ########################################################################
# Removing all items in Global Environment
rm(list=ls())

# Closing all video devices.
graphics.off()

# Clearing all Plots
try(dev.off(),silent=TRUE)

# Setting the current directory as the work directory. 
WD <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(WD)

# Clearing the console.
cat("\014")
##################################################################################################################################################

# Percorso della cartella dati
data_folder <- "../data"

################################################### Conversione dei Tassi Risk-Free (da Base Annua a Base Giornaliera) ###########################
# Lista dei file CSV
files <- c("daily-treasury-rates_2023.csv",
           "daily-treasury-rates_2024.csv",
           "daily-treasury-rates_2025.csv")

# Carica e unisci tutti i file
treasury_data <- files %>%
  map_df(~ read_csv(file.path(data_folder, .x), show_col_types = FALSE))

# Conversione del formato data (MM/DD/YYYY → YYYY-MM-DD)
treasury_data <- treasury_data %>%
  mutate(Date = mdy(Date))

# Filtra per date tra 2023-07-31 e 2025-07-29 (inclusi)
treasury_filtered <- treasury_data %>%
  filter(Date >= as.Date("2023-07-31") & Date <= as.Date("2025-07-29"))

# Estrai solo la colonna 3 Mo e calcola tasso giornaliero
risk_free_df <- treasury_filtered %>%
  select(Date, `3 Mo`) %>%
  rename(Rf_3Mo_AnnualNominal = `3 Mo`) %>%
  mutate(Rf_Daily_LogReturn = 100 * (log(1 + Rf_3Mo_AnnualNominal / 100) / 251)) %>%  # log-return giornaliero in % 
  arrange(Date)

# Visualizza i primi valori
print(head(risk_free_df, 10))
# # A tibble: 10 × 3
#    Date       Rf_3Mo_AnnualNominal Rf_Daily_LogReturn
#    <date>                    <dbl>              <dbl>
#  1 2023-07-31                 5.55             0.0215
#  2 2023-08-01                 5.54             0.0215
#  3 2023-08-02                 5.53             0.0214
#  4 2023-08-03                 5.54             0.0215
#  5 2023-08-04                 5.54             0.0215
#  6 2023-08-07                 5.56             0.0216
#  7 2023-08-08                 5.57             0.0216
#  8 2023-08-09                 5.55             0.0215
#  9 2023-08-10                 5.54             0.0215
# 10 2023-08-11                 5.54             0.0215

# Salva il file finale (opzionale)
write_csv(risk_free_df, file.path(data_folder, "risk_free_3mo_daily.csv"))
##################################################################################################################################################

################################################### Calcolo dei Rendimenti Logaritmici Giornalieri (Percentuali) per i titoli rischiosi ##########
# Elenco dei ticker
tickers <- c('SPY', 'AAPL', 'UNH', 'JPM', 'AMZN', 'XOM')

# Funzione per leggere un singolo file e calcolare rendimento log
read_and_process <- function(ticker) {
  file_path <- file.path(data_folder, paste0(ticker, "_data.csv"))
  
  # Leggi CSV
  df <- read_csv(file_path, show_col_types = FALSE) %>%
    select(Date, `Adj Close`) %>%
    rename(!!paste0(ticker, "_AdjClose") := `Adj Close`) %>%
    mutate(Date = ymd(Date)) %>%
    arrange(Date)
  
  # Calcola rendimento log giornaliero in %
  df <- df %>%
    mutate(!!paste0(ticker, "_LogReturn") := 100 * (log(!!sym(paste0(ticker, "_AdjClose"))) - log(lag(!!sym(paste0(ticker, "_AdjClose"))))))
  
  return(df)
}

# Leggi e processa tutti i file, poi fai il join per data
list_dfs <- lapply(tickers, read_and_process)

# Unisci tutti i dataframe per colonna "Date"
merged_data <- purrr::reduce(list_dfs, full_join, by = "Date")

# Unisci con il dataframe dei tassi risk-free
merged_data <- merged_data %>%
  left_join(risk_free_df %>% select(Date, Rf_Daily_LogReturn), by = "Date") %>%
  arrange(Date) %>%
  drop_na(Date) %>%
  mutate(Index = row_number()) %>%
  select(Index, everything())

# Controlla la struttura finale
glimpse(merged_data)
# Rows: 501
# Columns: 15
# $ Index              <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33…
# $ Date               <date> 2023-07-31, 2023-08-01, 2023-08-02, 2023-08-03, 2023-08-04, 2023-08-07, 2023-08-08, 2023-08-09, 2023-08-10, 2023-08-11, …
# $ SPY_AdjClose       <dbl> 445.9211, 444.6450, 438.4597, 437.2031, 435.2258, 439.0246, 437.1154, 434.1931, 434.3491, 434.0958, 436.4920, 431.4073, 4…
# $ SPY_LogReturn      <dbl> NA, -0.28657344, -1.40083838, -0.28700380, -0.45328587, 0.86904662, -0.43581684, -0.67077322, 0.03590253, -0.05833322, 0.…
# $ AAPL_AdjClose      <dbl> 194.5026, 193.6709, 190.6709, 189.2749, 180.1859, 177.0770, 178.0176, 176.4236, 176.2058, 176.2653, 177.9209, 175.9282, 1…
# $ AAPL_LogReturn     <dbl> NA, -0.4285181, -1.5611262, -0.7348508, -4.9211211, -1.7404436, 0.5297655, -0.8994639, -0.1235487, 0.0337669, 0.9349219, …
# $ UNH_AdjClose       <dbl> 489.3621, 487.7869, 487.8449, 487.9029, 485.8445, 493.5854, 488.8693, 485.2452, 486.5692, 490.9471, 493.7690, 489.6714, 4…
# $ UNH_LogReturn      <dbl> NA, -0.32240665, 0.01189258, 0.01187866, -0.42278190, 1.58072929, -0.96006481, -0.74407486, 0.27248121, 0.89571860, 0.573…
# $ JPM_AdjClose       <dbl> 150.7003, 149.9562, 148.2580, 149.1643, 148.8495, 149.5555, 148.7159, 146.7220, 146.5026, 147.3516, 147.6570, 143.8980, 1…
# $ JPM_LogReturn      <dbl> NA, -0.49501147, -1.13893315, 0.60947653, -0.21126815, 0.47313814, -0.56292726, -1.34986268, -0.14964021, 0.57788003, 0.2…
# $ AMZN_AdjClose      <dbl> 133.68, 131.69, 128.21, 128.91, 139.57, 142.22, 139.94, 137.85, 138.56, 138.41, 140.57, 137.67, 135.07, 133.98, 133.22, 1…
# $ AMZN_LogReturn     <dbl> NA, -1.49981352, -2.67810973, 0.54449179, 7.94518051, 1.88088430, -1.61613863, -1.50475950, 0.51372454, -0.10831058, 1.54…
# $ XOM_AdjClose       <dbl> 100.10175, 99.52301, 98.28154, 99.98974, 100.26975, 100.06440, 100.55911, 102.26731, 102.79004, 104.38621, 104.47021, 101…
# $ XOM_LogReturn      <dbl> NA, -0.579824852, -1.255266006, 1.723134917, 0.279651386, -0.205010332, 0.493176325, 1.684434312, 0.509836978, 1.54091033…
# $ Rf_Daily_LogReturn <dbl> 0.02151976, 0.02148201, 0.02144426, 0.02148201, 0.02148201, 0.02155750, 0.02159524, 0.02151976, 0.02148201, 0.02148201, 0…

# Salva il dataset completo
write_csv(merged_data, file.path(data_folder, "merged_data_with_log_returns_and_rf.csv"))
##################################################################################################################################################

# Costruisci i nomi delle colonne da mantenere
risky_logreturn_cols <- paste0(tickers, "_LogReturn")

# Estrai solo queste colonne
log_return_cols <- merged_data %>%
  select(all_of(risky_logreturn_cols))

# Rimuovi eventuali NA (prima riga)
log_return_clean <- na.omit(log_return_cols)

################################################### Pre-Analisi ##################################################################################
# Funzione per tracciare il line plot di adjusted closing price per un singolo ticker
plot_adj_close <- function(df, ticker) {
  adj_col <- paste0(ticker, "_AdjClose")
  
  ggplot(df, aes(x = Date, y = .data[[adj_col]])) +
    geom_line(color = "steelblue", size = 1.1) +
    labs(title = paste(ticker, " - Prezzo di Chiusura Giornaliero Aggiustato: Andamento Completo dal 2023-07-31 al 2025-07-29"),
         x = "Date",
         y = "Adjusted Close Price (USD)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5)
    )
}

# Plot per ogni ticker - usa i dataframe già letti in list_dfs
for (i in seq_along(tickers)) {
  print(plot_adj_close(list_dfs[[i]], tickers[i]))
}


# Funzione per disegnare i grafici a candele e i grafici dei volumi (per un singolo ticker) - con filtro data
plot_candlestick_from_file <- function(ticker, data_folder) {
  file_path <- file.path(data_folder, paste0(ticker, "_data.csv"))
  
  df <- read_csv(file_path, show_col_types = FALSE) %>%
    mutate(Date = ymd(Date)) %>%
    arrange(Date)
  
  # Filtra il periodo desiderato
  df <- df %>%
    filter(Date >= as.Date("2025-04-29") & Date <= as.Date("2025-07-29"))
  
  df_plot <- df %>%
    mutate(Direction = ifelse(Close >= Open, "Up", "Down"))
  
  candle_width <- 0.4
  
  # Prezzo (candlestick)
  p_price <- ggplot(df_plot, aes(x = Date)) +
    geom_segment(aes(y = Low, yend = High, xend = Date, color = Direction)) +
    geom_rect(aes(xmin = Date - candle_width,
                  xmax = Date + candle_width,
                  ymin = pmin(Open, Close),
                  ymax = pmax(Open, Close),
                  fill = Direction),
              color = "black") +
    scale_fill_manual(values = c("Up" = "forestgreen", "Down" = "firebrick")) +
    scale_color_manual(values = c("Up" = "forestgreen", "Down" = "firebrick")) +
    theme_minimal() +
    labs(title = paste(ticker, "- Grafico a Candele con Volume: dal 2025-04-29 al 2025-07-29"),
         y = "Price (USD)") +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5)) +
    scale_x_date(date_labels = "%b %d", date_breaks = "1 week")
  
  # Volume con colore coerente
  p_volume <- ggplot(df_plot, aes(x = Date, y = Volume, fill = Direction)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("Up" = "forestgreen", "Down" = "firebrick")) +
    theme_minimal() +
    labs(y = "Volume", x = "") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none") +
    scale_x_date(date_labels = "%b %d", date_breaks = "1 week")
  
  # Combina i due plot
  combined_plot <- p_price / p_volume + plot_layout(heights = c(3, 1))
  print(combined_plot)
}

# Candlestick chart per ogni ticker nel periodo desiderato
for (ticker in tickers) {
  plot_candlestick_from_file(ticker, data_folder)
}

# Nel candlestick chart, ogni candela rappresenta i prezzi di: apertura, chiusura, massimo e minimo giornaliero. Le ombre indicano i prezzi
# estremi, mentre il colore segnala la direzione del prezzo.
# Il volume degli scambi indica quante unità dell’asset sono state comprate e vendute.
# - Barra verde: la chiusura è stata maggiore rispetto a quella del giorno precedente (giornata di rialzo).
# - Barra rossa: la chiusura è stata inferiore rispetto aquella del giorno precedente (giornata di ribasso).


# Come prima analisi quantitativa, visualizziamo l’andamento del prezzo di chiusura aggiustato.
# Funzione per tracciare andamento Adj Close con regressione e LOESS
plot_adjclose_with_regression_loess <- function(merged_df, ticker, start_date = "2023-07-31", end_date = "2025-07-29") {
  adj_col <- paste0(ticker, "_AdjClose")
  
  df <- merged_df %>%
    filter(Date >= as.Date(start_date) & Date <= as.Date(end_date)) %>%
    select(Index, Date, all_of(adj_col)) %>%
    rename(AdjClose = all_of(adj_col))
  
  # Modelli
  lm_model <- lm(AdjClose ~ Index, data = df)
  loess_model <- loess(AdjClose ~ Index, data = df)
  
  df$Regression <- predict(lm_model, newdata = df)
  df$LOESS <- predict(loess_model, newdata = df)
  
  plot_df <- df %>%
    pivot_longer(cols = c("AdjClose", "Regression", "LOESS"),
                 names_to = "Tipo", values_to = "Valore") %>%
    mutate(Tipo = factor(Tipo, levels = c("AdjClose", "Regression", "LOESS")))
  
  colori <- c("AdjClose" = "blue", "Regression" = "green", "LOESS" = "red")
  linee <- c("AdjClose" = "solid", "Regression" = "dashed", "LOESS" = "dashed")
  
  ggplot(plot_df, aes(x = Index, y = Valore, color = Tipo, linetype = Tipo)) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = colori,
                       labels = c("Prezzo di Chiusura Aggiustato", "Regressione Lineare", "LOESS")) +
    scale_linetype_manual(values = linee,
                          labels = c("Prezzo di Chiusura Aggiustato", "Regressione Lineare", "LOESS")) +
    labs(title = paste("Andamento del Prezzo di Chiusura Giornaliero Aggiustato di", ticker, 
                       "dal", start_date, "al", end_date),
         y = "Adjusted Close Price (USD)", x = "Time",
         color = "", linetype = "") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
          plot.title = element_text(hjust = 0.5))
}

# Traccia i grafici
for (ticker in tickers) {
  print(plot_adjclose_with_regression_loess(merged_data, ticker))
}

# Abbiamo estratto i dati su un intervallo temporale definito, aggiungendo un indice temporale per trattare il tempo come una variabile
# indipendente in modelli di regressione.
# Abbiamo sovrapposto due stime di trend:
# - una regressione lineare semplice,
# - una curva LOESS.
# Abbiamo così confrontato una tendenza globale con una tendenza locale, più sensibile alle variazioni giornaliere.


# Simuliamo, ora, un modello di previsione del prezzo di chiusura aggiustato
plot_adjclose_with_train_test_index <- function(merged_df, ticker, start_date = "2023-07-31", end_date = "2025-07-29") {
  adj_col <- paste0(ticker, "_AdjClose")
  
  df <- merged_df %>%
    filter(Date >= as.Date(start_date) & Date <= as.Date(end_date)) %>%
    select(Index, Date, all_of(adj_col)) %>%
    rename(AdjClose = all_of(adj_col)) %>%
    arrange(Index)
  
  # Calcolo split_index (95% training)
  total_len <- nrow(df)
  split_index <- floor(total_len * 0.95)
  split_day <- df$Date[split_index]
  
  train_df <- df[1:split_index, ]
  test_df <- df[(split_index + 1):nrow(df), ]
  
  # Modelli con Index come variabile indipendente
  lm_model <- lm(AdjClose ~ Index, data = train_df)
  loess_model <- loess(AdjClose ~ Index, data = train_df)
  
  train_df <- train_df %>%
    mutate(
      Regression = predict(lm_model, newdata = .),
      LOESS = predict(loess_model, newdata = .),
      Set = "Training"
    )
  
  test_df <- test_df %>%
    mutate(
      Regression = NA,
      LOESS = NA,
      Set = "Test"
    )
  
  df <- bind_rows(train_df, test_df) %>%
    arrange(Index)
  
  # Long format
  plot_df <- df %>%
    pivot_longer(cols = c("AdjClose", "Regression", "LOESS"),
                 names_to = "Tipo", values_to = "Valore") %>%
    mutate(
      Tipo = factor(Tipo, levels = c("AdjClose", "Regression", "LOESS")),
      Set = factor(Set, levels = c("Training", "Test")),
      LegendaTipo = case_when(
        Tipo == "AdjClose" & Set == "Training" ~ "Training Set",
        Tipo == "AdjClose" & Set == "Test"     ~ "Test Set",
        Tipo == "Regression"                   ~ "Regressione Lineare",
        Tipo == "LOESS"                        ~ "LOESS",
        TRUE ~ NA_character_
      ),
      LinetypeLegenda = case_when(
        LegendaTipo %in% c("Training Set", "Test Set") ~ "solid",
        LegendaTipo %in% c("Regressione Lineare", "LOESS") ~ "dashed",
        TRUE ~ NA_character_
      )
    )
  
  plot_df$LegendaTipo <- factor(plot_df$LegendaTipo, levels = c("Training Set", "Test Set", "Regressione Lineare", "LOESS"))
  plot_df$LinetypeLegenda <- factor(plot_df$LinetypeLegenda, levels = c("solid", "dashed"))
  
  # Plot con asse x = Index (tempo come variabile numerica)
  ggplot(plot_df, aes(x = Index, y = Valore)) +
    geom_line(data = filter(plot_df, Tipo == "AdjClose", !is.na(Valore)),
              aes(color = LegendaTipo, linetype = LegendaTipo), linewidth = 1) +
    geom_line(data = filter(plot_df, Tipo %in% c("Regression", "LOESS"), !is.na(Valore)),
              aes(color = LegendaTipo, linetype = LegendaTipo), linewidth = 1) +
    geom_vline(xintercept = split_index, linetype = "solid", color = "darkgrey", linewidth = 1) +
    scale_color_manual(name = NULL,
                       values = c(
                         "Training Set" = "black",
                         "Test Set" = "blue",
                         "Regressione Lineare" = "green",
                         "LOESS" = "red"
                       )) +
    scale_linetype_manual(name = NULL,
                          values = c(
                            "Training Set" = "solid",
                            "Test Set" = "solid",
                            "Regressione Lineare" = "dashed",
                            "LOESS" = "dashed"
                          )) +
    labs(
      title = paste(ticker, "- Prezzo di Chiusura Aggiustato Giornaliero - Training e Test Set",
                    "\nTrnS: Dal", start_date, "al", split_day, 
                    " - TstS: Dal", split_day + 1, "al", end_date),
      y = "Adjusted Close Price (USD)", x = "Time Index"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 0),
      plot.title = element_text(hjust = 0.5)
    )
}

# Usa la funzione su tutti i tickers
for (ticker in tickers) {
  print(plot_adjclose_with_train_test_index(merged_data, ticker))
}

# La regressione lineare mostra solo la tendenza media dei prezzi, risultando rigida per fenomeni finanziari. La LOESS,invece, si adatta
# meglio alle variazioni locali.
# La distribuzione irregolare dei dati attorno alla LOESS suggerisce eteroschedasticità, indicando la necessità
# di un modello più avanzato per gestire la volatilità nel tempo.
##################################################################################################################################################

# Calcolo del numero totale di osservazioni
total_rows <- nrow(merged_data)

# Calcolo del limite per il 95% (training set)
split_index <- floor(total_rows * 0.95)

# Estrazione del training set
training_set <- merged_data %>%
  arrange(Index) %>%
  slice(1:split_index)

# Controlla la struttura finale
glimpse(training_set)
# Rows: 475
# Columns: 15
# $ Index              <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 3…
# $ Date               <date> 2023-07-31, 2023-08-01, 2023-08-02, 2023-08-03, 2023-08-04, 2023-08-07, 2023-08-08, 2023-08-09, 2023-08-10, 2023-08-11, 202…
# $ SPY_AdjClose       <dbl> 445.9211, 444.6450, 438.4597, 437.2031, 435.2258, 439.0246, 437.1154, 434.1931, 434.3491, 434.0958, 436.4920, 431.4073, 428.…
# $ SPY_LogReturn      <dbl> NA, -0.28657344, -1.40083838, -0.28700380, -0.45328587, 0.86904662, -0.43581684, -0.67077322, 0.03590253, -0.05833322, 0.550…
# $ AAPL_AdjClose      <dbl> 194.5026, 193.6709, 190.6709, 189.2749, 180.1859, 177.0770, 178.0176, 176.4236, 176.2058, 176.2653, 177.9209, 175.9282, 175.…
# $ AAPL_LogReturn     <dbl> NA, -0.4285181, -1.5611262, -0.7348508, -4.9211211, -1.7404436, 0.5297655, -0.8994639, -0.1235487, 0.0337669, 0.9349219, -1.…
# $ UNH_AdjClose       <dbl> 489.3621, 487.7869, 487.8449, 487.9029, 485.8445, 493.5854, 488.8693, 485.2452, 486.5692, 490.9471, 493.7690, 489.6714, 486.…
# $ UNH_LogReturn      <dbl> NA, -0.32240665, 0.01189258, 0.01187866, -0.42278190, 1.58072929, -0.96006481, -0.74407486, 0.27248121, 0.89571860, 0.573141…
# $ JPM_AdjClose       <dbl> 150.7003, 149.9562, 148.2580, 149.1643, 148.8495, 149.5555, 148.7159, 146.7220, 146.5026, 147.3516, 147.6570, 143.8980, 143.…
# $ JPM_LogReturn      <dbl> NA, -0.49501147, -1.13893315, 0.60947653, -0.21126815, 0.47313814, -0.56292726, -1.34986268, -0.14964021, 0.57788003, 0.2069…
# $ AMZN_AdjClose      <dbl> 133.68, 131.69, 128.21, 128.91, 139.57, 142.22, 139.94, 137.85, 138.56, 138.41, 140.57, 137.67, 135.07, 133.98, 133.22, 134.…
# $ AMZN_LogReturn     <dbl> NA, -1.49981352, -2.67810973, 0.54449179, 7.94518051, 1.88088430, -1.61613863, -1.50475950, 0.51372454, -0.10831058, 1.54853…
# $ XOM_AdjClose       <dbl> 100.10175, 99.52301, 98.28154, 99.98974, 100.26975, 100.06440, 100.55911, 102.26731, 102.79004, 104.38621, 104.47021, 101.78…
# $ XOM_LogReturn      <dbl> NA, -0.579824852, -1.255266006, 1.723134917, 0.279651386, -0.205010332, 0.493176325, 1.684434312, 0.509836978, 1.540910338, …
# $ Rf_Daily_LogReturn <dbl> 0.02151976, 0.02148201, 0.02144426, 0.02148201, 0.02148201, 0.02155750, 0.02159524, 0.02151976, 0.02148201, 0.02148201, 0.02…

# Salva il training set
write_csv(training_set, file.path(data_folder, "training_set_with_log_returns_and_rf.csv"))

################################################### Analisi di Autocorrelazione sui Rendimenti Logaritmici #######################################
# Autocorrelogramma
plot_acf <- function(data, col, max_lag = NULL, ci_levels = c(0.90, 0.95, 0.99)) {
  
  x <- data[[col]]
  x <- na.omit(x)
  n <- length(x)
  
  if (is.null(max_lag)) {
    max_lag <- min(10, floor(n / 4))  # Hyndman
  }
  
  # Calcolo ACF (escludendo lag 0, che è sempre 1)
  acf_res <- acf(x, lag.max = max_lag, plot = FALSE)
  acf_vals <- acf_res$acf[-1]
  lags <- 1:length(acf_vals)
  
  # Calcolo intervalli di confidenza per i livelli dati
  ci_vals <- sapply(ci_levels, function(level) qnorm((1 + level) / 2) / sqrt(n))
  names(ci_vals) <- paste0(ci_levels * 100, "%")
  
  # Dataframe ACF
  df <- data.frame(lag = lags, acf = acf_vals)
  
  # Dataframe intervalli di confidenza per ggplot (sia + che -)
  ci_df <- do.call(rbind, lapply(names(ci_vals), function(name) {
    data.frame(
      ci_level = name,
      y = c(ci_vals[name], -ci_vals[name])
    )
  }))
  
  ci_df$ci_level <- factor(ci_df$ci_level, levels = c("90%", "95%", "99%"))
  
  ci_colors <- c("90%" = "green", "95%" = "blue", "99%" = "red")
  ci_linetypes <- c("90%" = "dotted", "95%" = "dashed", "99%" = "dotdash")
  
  # Estrai il ticker dal nome colonna
  ticker <- sub("_.*", "", col)
  
  p <- ggplot(df, aes(x = lag, y = acf)) +
    geom_segment(aes(x = lag, xend = lag, y = 0, yend = acf), color = "black") +
    geom_point(shape = 18, size = 3, color = "black") +
    theme_minimal() +
    geom_hline(data = ci_df, aes(yintercept = y, color = ci_level, linetype = ci_level), size = 1.1) +
    scale_color_manual(name = "Confidence Interval", values = ci_colors) +
    scale_linetype_manual(name = "Confidence Interval", values = ci_linetypes) +
    labs(title = paste(ticker, "- ACF dei Rendimenti Logaritmici"), x = "Lag", y = "ACF Value") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5)
    )
  
  print(p)
}

# Plot ACF per ogni titolo
for (col in risky_logreturn_cols) {
  plot_acf(training_set, col)
}

# La tolleranza rigorosa per i picchi è:
#
# - per la linea di confidenza del 90%: ⌊(maxlag × 0.10)⌋ = ⌊(10 × 0.10)⌋ = 1
# - per la linea di confidenza del 95%: ⌊(maxlag × 0.05)⌋ = ⌊(10 × 0.05)⌋ = 0
# - per la linea di confidenza del 99%: ⌊(maxlag × 0.01)⌋ = ⌊(10 × 0.01)⌋ = 0

# Questa viene sempre rispettata da JPM_LogReturn e AMZN_LogReturn; per cui possiamo assumere l’assenza di autocorrelazione
# con significatività α = 0.10, α = 0.05 e α = 0.01 per queste serie storiche. Per tutte le altre serie, invece, la tolleranza
# rigorosa viene rispettata con significatività α = 0.01.
#
# Per ora, quindi, possiamo assumere l'assenza di autocorrelazione per tutte le serie storiche con un livello di significatività α = 0.01.
#
# Ricordiamo, però, che l’assenza di autocorrelazione non implica indipendenza, ma solo che non ci sono evidenze di correlazioni sequenziali
# significative nei dati. 


# Autocorrelogramma parziale
plot_pacf <- function(data, col, max_lag = NULL, ci_levels = c(0.90, 0.95, 0.99)) {
  
  x <- data[[col]]
  x <- na.omit(x)
  n <- length(x)
  
  if (is.null(max_lag)) {
    max_lag <- min(10, floor(n / 4))  # Hyndman
  }
  
  # Calcolo PACF (rimuovo lag 0, che è sempre 1)
  pacf_res <- pacf(x, lag.max = max_lag, plot = FALSE)
  pacf_vals <- pacf_res$acf
  lags <- 1:length(pacf_vals)
  
  # Intervalli di confidenza per i livelli dati
  ci_vals <- sapply(ci_levels, function(level) qnorm((1 + level) / 2) / sqrt(n))
  names(ci_vals) <- paste0(ci_levels * 100, "%")
  
  # Dataframe PACF
  df <- data.frame(lag = lags, pacf = pacf_vals)
  
  # Dataframe intervalli di confidenza per ggplot (+ e -)
  ci_df <- do.call(rbind, lapply(names(ci_vals), function(name) {
    data.frame(
      ci_level = name,
      y = c(ci_vals[name], -ci_vals[name])
    )
  }))
  ci_df$ci_level <- factor(ci_df$ci_level, levels = c("90%", "95%", "99%"))
  
  # Palette e linee
  ci_colors <- c("90%" = "green", "95%" = "blue", "99%" = "red")
  ci_linetypes <- c("90%" = "dotted", "95%" = "dashed", "99%" = "dotdash")
  
  # Estrai il ticker dal nome colonna
  ticker <- sub("_.*", "", col)
  
  p <- ggplot(df, aes(x = lag, y = pacf)) +
    geom_segment(aes(x = lag, xend = lag, y = 0, yend = pacf), color = "black") +
    geom_point(shape = 18, size = 3, color = "black") +
    theme_minimal() +
    geom_hline(data = ci_df, aes(yintercept = y, color = ci_level, linetype = ci_level), size = 1.1) +
    scale_color_manual(name = "Confidence Interval", values = ci_colors) +
    scale_linetype_manual(name = "Confidence Interval", values = ci_linetypes) +
    labs(title = paste(ticker, "- PACF dei Rendimenti Logaritmici"), x = "Lag", y = "PACF Value") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5)
    )
  
  print(p)
}

# Plot PACF per ogni titolo
for (col in risky_logreturn_cols) {
  plot_pacf(training_set, col)
}

# Nel PACF possiamo trarre le stesse conclusioni di prima.

# In aggiunta alle evidenze grafiche degli autocorrelogrammi, integriamo le osservazioni con il test di Ljung-Box, che forniscono una
# valutazione quantitativa più robusta dell'ipotesi di assenza di autocorrelazione.


# Test di Ljung-Box.
# Questo test assume come ipotesi nulla che non ci sia autocorrelazione fino al lag specificato.

# Operatore coalescenza
`%||%` <- function(a, b) if (!is.null(a)) a else b

# Funzione Ljung-Box test su una singola serie
ljungbox_test <- function(series, col_name = NULL) {
  series <- na.omit(series)
  n <- length(series)
  max_lag <- min(10, floor(n / 4))
  
  cat(sprintf("Ljung-Box test per %s (max_lag = %d):\n", col_name %||% "", max_lag))
  
  results <- data.frame(
    lag = integer(),
    lb_stat = numeric(),
    lb_pvalue = numeric()
  )
  
  for (lag in 1:max_lag) {
    test_res <- Box.test(series, lag = lag, type = "Ljung-Box", fitdf = 0)
    results <- rbind(results, data.frame(
      lag = lag,
      lb_stat = test_res$statistic,
      lb_pvalue = test_res$p.value
    ))
  }
  
  print(results)
  cat("\n")
}

# Esegui il test per ciascuna serie
for (col in risky_logreturn_cols) {
  ljungbox_test(training_set[[col]], col_name = col)
}
# Ljung-Box test per SPY_LogReturn (max_lag = 10):
#            lag   lb_stat   lb_pvalue
# X-squared    1  2.151799 0.142402974
# X-squared1   2  5.359999 0.068563197
# X-squared2   3 11.537234 0.009148828
# X-squared3   4 13.431263 0.009349960
# X-squared4   5 13.431468 0.019654094
# X-squared5   6 14.167211 0.027823087
# X-squared6   7 14.362075 0.045103743
# X-squared7   8 14.641084 0.066511746
# X-squared8   9 14.719625 0.098930951
# X-squared9  10 14.899972 0.135751491
# 
# Ljung-Box test per AAPL_LogReturn (max_lag = 10):
#            lag   lb_stat  lb_pvalue
# X-squared    1  0.626536 0.42862879
# X-squared1   2  4.964292 0.08356372
# X-squared2   3  7.676838 0.05318466
# X-squared3   4  9.566887 0.04839087
# X-squared4   5 12.968073 0.02367980
# X-squared5   6 12.970345 0.04350930
# X-squared6   7 13.913169 0.05274746
# X-squared7   8 13.940614 0.08332647
# X-squared8   9 14.335059 0.11089577
# X-squared9  10 15.289106 0.12187167
# 
# Ljung-Box test per UNH_LogReturn (max_lag = 10):
#            lag  lb_stat  lb_pvalue
# X-squared    1 4.662199 0.03083367
# X-squared1   2 5.115029 0.07749714
# X-squared2   3 5.115069 0.16356259
# X-squared3   4 5.744857 0.21902999
# X-squared4   5 5.830190 0.32309448
# X-squared5   6 5.834089 0.44202997
# X-squared6   7 6.084154 0.52995827
# X-squared7   8 6.496059 0.59184484
# X-squared8   9 7.219119 0.61431763
# X-squared9  10 7.300999 0.69675598
# 
# Ljung-Box test per JPM_LogReturn (max_lag = 10):
#            lag      lb_stat lb_pvalue
# X-squared    1 0.0006007107 0.9804463
# X-squared1   2 1.7120331464 0.4248511
# X-squared2   3 2.2020114559 0.5315523
# X-squared3   4 4.0958341797 0.3931914
# X-squared4   5 4.6500621622 0.4600600
# X-squared5   6 6.0725623376 0.4151109
# X-squared6   7 6.7365902699 0.4568116
# X-squared7   8 8.4348048463 0.3921891
# X-squared8   9 8.4397890389 0.4905039
# X-squared9  10 8.6445328762 0.5661378
# 
# Ljung-Box test per AMZN_LogReturn (max_lag = 10):
#            lag   lb_stat lb_pvalue
# X-squared    1 0.3734689 0.5411195
# X-squared1   2 1.9200791 0.3828777
# X-squared2   3 1.9831027 0.5759212
# X-squared3   4 2.0122239 0.7335104
# X-squared4   5 2.2019152 0.8205593
# X-squared5   6 2.2323575 0.8971367
# X-squared6   7 3.2988166 0.8560526
# X-squared7   8 3.8789187 0.8678792
# X-squared8   9 4.4710841 0.8777684
# X-squared9  10 4.5023862 0.9218515
# 
# Ljung-Box test per XOM_LogReturn (max_lag = 10):
#            lag     lb_stat lb_pvalue
# X-squared    1  0.01160408 0.9142160
# X-squared1   2  0.82284350 0.6627074
# X-squared2   3  1.22021958 0.7481590
# X-squared3   4  1.30738270 0.8601217
# X-squared4   5  1.91841065 0.8603154
# X-squared5   6  1.96407191 0.9229730
# X-squared6   7  7.14305549 0.4141386
# X-squared7   8  7.17297347 0.5180905
# X-squared8   9 10.54229401 0.3083904
# X-squared9  10 10.54875777 0.3937360

# Analizzando i p-value del test di Ljung-Box per ciascuna serie di log-return fino al lag 10, osserviamo che:
# 
# - Per **SPY_LogReturn** e **AAPL_LogReturn**, alcuni p-value (in particolare ai lag 3-6) sono inferiori a 0.05 e anche a 0.01,
#   il che indica **presenza di autocorrelazione significativa** a quei lag. Possiamo quindi **rifiutare l’ipotesi nulla**
#   di assenza di autocorrelazione per questi casi.
#
# - Per **UNH_LogReturn**, il primo lag è significativo al livello del 5% (p = 0.0308), ma i successivi non lo sono.
#   Questo suggerisce una **debole autocorrelazione iniziale**, che però **non persiste**.
#
# - Per **JPM_LogReturn**, **AMZN_LogReturn** e **XOM_LogReturn**, tutti i p-value sono ben superiori a 0.05 (in realtà anche a 0.10),
#   dunque **non c'è evidenza statistica sufficiente** per rifiutare l’ipotesi nulla di assenza di autocorrelazione.
#
# Concludendo:
# - **Non possiamo dire che tutte le serie siano prive di autocorrelazione**.
# - In particolare, **SPY** e **AAPL** mostrano **autocorrelazione statisticamente significativa**.
# - Per le altre serie, l’evidenza di autocorrelazione è **assente o debole**, ma ricordiamo che:
#   *non possiamo confermare la totale assenza di autocorrelazione — possiamo solo dire che non ne abbiamo trovato evidenza con questo test.*
##################################################################################################################################################

################################################### Analisi di Cross-Correlazione sui Rendimenti Logaritmici #####################################
# Verifichiamo se gli insiemi di dati sono correlati tra loro o meno: abbiamo trovato che questi
# non sono autocorrelati, ma questo non impedisce la possibilità di una correlazione incrociata.

# La matrice di varianza-covarianza misura la relazione lineare tra le serie in termini di varianza condivisa.
# Un valore alto (positivo o negativo) nella matrice indica che le serie cambiano “insieme”.

# Calcola la matrice di varianza-covarianza empirica
calculate_cov_matrix <- function(df, columns) {
  df_clean <- df %>% select(all_of(columns)) %>% na.omit()
  cov_matrix <- cov(df_clean)
  return(cov_matrix)
}

cov_matrix <- calculate_cov_matrix(training_set, risky_logreturn_cols)

# Mostra la matrice
print(round(cov_matrix, 4))
#                SPY_LogReturn AAPL_LogReturn UNH_LogReturn JPM_LogReturn AMZN_LogReturn XOM_LogReturn
# SPY_LogReturn         1.1126         1.3101        0.1838        0.9832         1.5427        0.5055
# AAPL_LogReturn        1.3101         3.0971       -0.0350        0.8771         1.7142        0.5621
# UNH_LogReturn         0.1838        -0.0350        5.4032        0.2135        -0.0109        0.0114
# JPM_LogReturn         0.9832         0.8771        0.2135        2.2095         1.1734        0.8042
# AMZN_LogReturn        1.5427         1.7142       -0.0109        1.1734         3.9468        0.3016
# XOM_LogReturn         0.5055         0.5621        0.0114        0.8042         0.3016        1.9743

# Visualizza la matrice di varianza-covarianza con una heatmap
ggcorrplot(cov_matrix, lab = TRUE, lab_size = 3, type = "lower",
           title = "Matrice di Covarianza dei Rendimenti Logaritmici",
           colors = c("blue", "white", "red"))


# Calcola la matrice di correlazione.
# La matrice di correlazione misura la forza e la direzione della relazione lineare tra le serie.
plot_corr_heatmap <- function(data, title) {
  corr_matrix <- cor(data, use = "pairwise.complete.obs")
  
  melted_corr <- reshape2::melt(corr_matrix)
  
  ggplot(melted_corr, aes(Var1, Var2, fill = value)) +
    geom_tile() +
    geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                         limits = c(-1, 1), name = "Correlazione") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5)
    ) +
    labs(title = title, x = "", y = "")
}

# Seleziona i rendimenti logaritmici dei soli titoli rischiosi
risky_returns_df <- training_set %>%
  select(all_of(risky_logreturn_cols)) %>%
  na.omit()

# Stampa la matrice di correlazione dei rendimenti logaritmici
corr_returns <- cor(risky_returns_df, use = "pairwise.complete.obs")
print(round(corr_returns, 4))
#                SPY_LogReturn AAPL_LogReturn UNH_LogReturn JPM_LogReturn AMZN_LogReturn XOM_LogReturn
# SPY_LogReturn         1.0000         0.7058        0.0749        0.6271         0.7362        0.3411
# AAPL_LogReturn        0.7058         1.0000       -0.0086        0.3353         0.4903        0.2273
# UNH_LogReturn         0.0749        -0.0086        1.0000        0.0618        -0.0024        0.0035
# JPM_LogReturn         0.6271         0.3353        0.0618        1.0000         0.3974        0.3850
# AMZN_LogReturn        0.7362         0.4903       -0.0024        0.3974         1.0000        0.1080
# XOM_LogReturn         0.3411         0.2273        0.0035        0.3850         0.1080        1.0000

# Stampa la matrice di correlazione dei quadrati dei rendimenti
corr_squared_returns <- cor(risky_returns_df^2, use = "pairwise.complete.obs")
print(round(corr_squared_returns, 4))
#                SPY_LogReturn AAPL_LogReturn UNH_LogReturn JPM_LogReturn AMZN_LogReturn XOM_LogReturn
# SPY_LogReturn         1.0000         0.9026        0.0181        0.5372         0.6883        0.5489
# AAPL_LogReturn        0.9026         1.0000        0.0173        0.4747         0.6798        0.4780
# UNH_LogReturn         0.0181         0.0173        1.0000        0.0231         0.0048        0.0563
# JPM_LogReturn         0.5372         0.4747        0.0231        1.0000         0.4279        0.4489
# AMZN_LogReturn        0.6883         0.6798        0.0048        0.4279         1.0000        0.3339
# XOM_LogReturn         0.5489         0.4780        0.0563        0.4489         0.3339        1.0000

# Stampa la matrice di correlazione dei valori assoluti
corr_abs_returns <- cor(abs(risky_returns_df), use = "pairwise.complete.obs")
print(round(corr_abs_returns, 4))
#                SPY_LogReturn AAPL_LogReturn UNH_LogReturn JPM_LogReturn AMZN_LogReturn XOM_LogReturn
# SPY_LogReturn         1.0000         0.6584        0.0947        0.5642         0.6416        0.3643
# AAPL_LogReturn        0.6584         1.0000        0.0733        0.3820         0.4535        0.2954
# UNH_LogReturn         0.0947         0.0733        1.0000        0.1260         0.0494        0.1084
# JPM_LogReturn         0.5642         0.3820        0.1260        1.0000         0.3902        0.3423
# AMZN_LogReturn        0.6416         0.4535        0.0494        0.3902         1.0000        0.2272
# XOM_LogReturn         0.3643         0.2954        0.1084        0.3423         0.2272        1.0000

# Genera le heatmaps
plot_corr_heatmap(risky_returns_df, "Matrice di Correlazione dei Rendimenti Logaritmici dei Titoli Rischiosi")
plot_corr_heatmap(risky_returns_df^2, "Matrice di Correlazione dei Quadrati dei Rendimenti Logaritmici dei Titoli Rischiosi")
plot_corr_heatmap(abs(risky_returns_df), "Matrice di Correlazione dei Valori Assoluti dei Rendimenti Logaritmici dei Titoli Rischiosi")

# L’analisi della matrice di correlazione sui rendimenti logaritmici giornalieri mostra che alcune coppie di titoli
# presentano correlazioni **moderate o forti**, in particolare:
# - **SPY–AMZN**: 0.7362
# - **SPY–AAPL**: 0.7058
# - **SPY–JPM**: 0.6271
# 
# Al contrario, altre coppie mostrano correlazioni molto **basse o quasi nulle**, come:
# - **UNH con tutti gli altri titoli**, ad esempio UNH–AAPL: –0.0086, UNH–SPY: 0.0749
# 
# Questo suggerisce che alcuni titoli (es. UNH) si muovono in modo **più indipendente**, mentre altri
# (es. SPY, AAPL, AMZN) sono più **sincronizzati** nei loro rendimenti.

# Le **correlazioni sui quadrati dei rendimenti** (proxy della **volatilità**) risultano **molto più elevate** per molte coppie:
# - **SPY–AAPL**: 0.9026
# - **SPY–AMZN**: 0.6883
# - **SPY–XOM**: 0.5489
# - Anche altri titoli (es. JPM, XOM) mostrano moderate correlazioni con SPY e AAPL.

# Questo evidenzia una **comune dinamica della volatilità** tra molti titoli: pur muovendosi in direzioni diverse,
# le serie tendono a mostrare **simultaneamente alti o bassi livelli di variabilità**.

# Le **correlazioni sui valori assoluti** dei rendimenti confermano il quadro:
# - Ancora SPY–AAPL: 0.6584
# - SPY–AMZN: 0.6416
# - SPY–JPM: 0.5642
# 
# Questi risultati rafforzano l’idea di una **co-movimentazione della volatilità**, pur senza necessariamente indicare
# una forte correlazione direzionale.

# In conclusione:
# - Le serie **non sono indipendenti**, soprattutto in termini di volatilità.
# - Alcune coppie di titoli mostrano **forte co-movimento**, specialmente SPY, AAPL e AMZN.
# - L’uso dei quadrati o dei valori assoluti evidenzia **strutture di dipendenza latente**, anche in assenza
#   di forti correlazioni nei rendimenti grezzi.


# Likelihood Ratio Test.
# Il test LR confronta un modello completo (che include tutte le variabili di interesse)
# e un modello ridotto (che include solo una parte di esse). In particolare, confronta la log-verosimiglianza dei due modelli. Ha come:
#  
# - H_0: Le matrici di covarianza del modello completo e del modello ridotto SONO uguali;
# - H_1: Le matrici di covarianza del modello completo e del modello ridotto NON sono uguali.
lr_test <- function(df, alpha = 0.10) {
  n <- nrow(df)
  p <- ncol(df)
  
  # Matrici di covarianza
  cov_full <- cov(df)
  cov_reduced <- diag(diag(cov_full))
  
  # Determinanti
  det_full <- det(cov_full)
  det_reduced <- det(cov_reduced)
  
  # Statistica del test (Likelihood Ratio)
  lr_stat <- n * (log(det_reduced) - log(det_full))
  
  # Gradi di libertà
  df_test <- p * (p - 1) / 2
  
  # p-value
  p_value <- 1 - pchisq(lr_stat, df = df_test)
  
  # Header
  cat("Likelihood Ratio Test\n")
  cat(sprintf("Statistic: %f\np-value: %g\n", lr_stat, p_value))
  
  # Conclusione
  if (p_value > alpha) {
    cat(sprintf("\n** CONCLUSIONE: p-value > %.2f **\nNon possiamo rigettare l'ipotesi nulla con un livello di significatività del %d%%\n→ Le matrici di covarianza del modello completo e del modello ridotto SONO uguali.\n\n",
                alpha, alpha * 100))
  } else {
    cat(sprintf("\n** CONCLUSIONE: p-value <= %.2f **\nPossiamo rigettare l'ipotesi nulla al livello di significatività del %d%%\n→ Le matrici di covarianza del modello completo e del modello ridotto NON sono uguali.\n\n",
                alpha, alpha * 100))
  }
}

# Calcolo sui soli rendimenti del training set
log_return_training <- training_set %>%
  select(all_of(risky_logreturn_cols)) %>%
  na.omit()

# Esegue il test
lr_test(log_return_training, alpha = 0.01)
# Likelihood Ratio Test
# Statistic: 1079.347437
# p-value: 0
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla al livello di significatività del 1%
# → Le matrici di covarianza del modello completo e del modello ridotto NON sono uguali.

# Alla luce dei risultati, il test LR rileva differenze significative tra le matrici di covarianza dei modelli considerati.
# Pertanto, non assumiamo l’indipendenza tra le serie.
##################################################################################################################################################

################################################### Analisi della Volatilità dell’S&P 500 tramite Modello GARCH ##################################
# Quello che vogliamo fare adesso è applicare un modello GARCH alla serie storica dell’S&P 500 al fine di stimare la volatilità condizionata.
#
# Prima di applicare un modello GARCH, occorre verificare la presenza di una unit root, che indicherebbe un comportamento simile a un
# random walk, in cui la conoscenza del passato oltre lo stato presente non migliora la previsione.
#
# Poiché i modelli GARCH descrivono l'evoluzione della varianza condizionata nel tempo, è necessario che la serie sia stazionaria:
# solo in questo caso la dinamica della varianza è ben definita e convergente, e il processo GARCH ammette una soluzione con varianza finita.
# In caso contrario, il modello non è affidabile dal punto di visto statistico, né utile in pratica.

# Stazionarietà dei prezzi aggiustati:
# ADF Test.
# L’ADF (Augmented Dickey-Fuller) testa la presenza di una radice unitaria nella serie temporale, cioè verifica se la serie è stazionaria o meno.
# Il test ha come:
#
# - H_0: La serie ha una radice unitaria → NON stazionaria;
# - H_1 (a seconda del modello):
#   - none: Serie STAZIONARIA con media zero (nessun intercept);
#   - drift: Serie STAZIONARIA con media costante (intercept);
#   - trend: Serie STAZIONARIA attorno a un trend lineare (intercept + trend).
adf_test_all <- function(data, col, alpha = 0.01) {
  x <- na.omit(data[[col]])
  
  types <- c("none", "drift", "trend")
  names <- c("Senza Intercetta", "Con Intercetta (Drift)", "Con Intercetta e Trend")
  
  for (i in seq_along(types)) {
    type <- types[i]
    label <- names[i]
    
    cat(sprintf("\n[%s] ADF Test - %s\n", col, label))
    
    result <- CADFtest(x, type = type, criterion = "AIC")
    
    cat(sprintf("Statistic: %f\n", result$stat))
    cat(sprintf("p-value  : %g\n", result$p.value))
    
    if (result$p.value <= alpha) {
      cat(sprintf("\n** CONCLUSIONE: p-value <= %.2f **\nPossiamo rigettare l'ipotesi nulla con un livello di significatività del %d%%\n→ I dati SONO stazionari.\n\n", alpha, alpha * 100))
    } else {
      cat(sprintf("\n** CONCLUSIONE: p-value > %.2f **\nNon possiamo rigettare l'ipotesi nulla con un livello di significatività del %d%%\n→ I dati NON sono stazionari.\n\n", alpha, alpha * 100))
    }
  }
}

# Esegue il test ADF
adf_test_all(training_set, "SPY_AdjClose")
# 
# [SPY_AdjClose] ADF Test - Senza Intercetta
# Statistic: 1.201244
# p-value  : 0.94142
# 
# ** CONCLUSIONE: p-value > 0.01 **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON sono stazionari.
# 
# 
# [SPY_AdjClose] ADF Test - Con Intercetta (Drift)
# Statistic: -1.005416
# p-value  : 0.752652
# 
# ** CONCLUSIONE: p-value > 0.01 **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON sono stazionari.
# 
# 
# [SPY_AdjClose] ADF Test - Con Intercetta e Trend
# Statistic: -2.453081
# p-value  : 0.351598
# 
# ** CONCLUSIONE: p-value > 0.01 **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON sono stazionari.


# KPSS Test.
# Anche il KPSS (Kwiatkowski-Phillips-Schmidt-Shin) test serve a verificare se una serie temporale è stazionaria o meno.
# A differenza dell'ADF, qui l'ipotesi nulla e alternativa sono invertite.
# Il test ha come:
#
# - H_0: La serie è stazionaria (nessuna radice unitaria);
# - H_1 (a seconda del modello):
#   - Level: La serie NON è stazionaria → ha una radice unitaria rispetto al livello (cioè la media varia nel tempo);
#   - Trend: La serie NON è stazionaria → ha una radice unitaria rispetto al trend (cioè non staziona attorno a un trend deterministico).
kpss_test_all <- function(data, col, alpha = 0.01) {
  x <- na.omit(data[[col]])
  
  types <- c("Level", "Trend")
  names <- c("Stazionarietà attorno a una media costante", 
             "Stazionarietà attorno a un trend deterministico")
  
  # Tabelle dei valori critici standard (da KPSS)
  critical_values <- list(
    Level = c(`10%` = 0.347, `5%` = 0.463, `2.5%` = 0.574, `1%` = 0.739),
    Trend = c(`10%` = 0.119, `5%` = 0.146, `2.5%` = 0.176, `1%` = 0.216)
  )
  
  # Funzione per trovare la soglia più vicina
  get_critical <- function(cv, alpha) {
    key <- names(cv)[which.min(abs(as.numeric(gsub("%", "", names(cv))) / 100 - alpha))]
    value <- unname(cv[[key]])
    return(c(value = as.numeric(value), level = key))
  }
  
  # Funzione per stimare il p-value approssimato
  estimate_pvalue <- function(stat, cv) {
    if (stat > cv["1%"]) {
      return("p-value < 0.01")
    } else if (stat > cv["2.5%"]) {
      return("p-value ≈ 0.025")
    } else if (stat > cv["5%"]) {
      return("p-value ≈ 0.05")
    } else if (stat > cv["10%"]) {
      return("p-value ≈ 0.10")
    } else {
      return("p-value > 0.10")
    }
  }
  
  for (i in seq_along(types)) {
    type <- types[i]
    label <- names[i]
    
    cat(sprintf("\n[%s] KPSS Test - %s\n", col, label))
    
    result <- kpss.test(x, null = type)
    
    stat <- result$statistic
    crit <- get_critical(critical_values[[type]], alpha)
    all_cv <- critical_values[[type]]
    
    pvalue_msg <- estimate_pvalue(stat, all_cv)
    
    cat(sprintf("Statistic: %f\n", stat))
    cat(sprintf("Valore critico (circa %s): %f\n", crit["level"], as.numeric(crit["value"])))
    cat(sprintf("Interpretazione del p-value: %s\n", pvalue_msg))
    
    if (stat > crit["value"]) {
      cat(sprintf("\n** CONCLUSIONE: Statistic > valore critico (%s) **\nPossiamo rigettare l'ipotesi nulla con un livello di significatività del %d%%\n→ I dati NON sono stazionari.\n\n", crit["level"], alpha * 100))
    } else {
      cat(sprintf("\n** CONCLUSIONE: Statistic <= valore critico (%s) **\nNon possiamo rigettare l'ipotesi nulla con un livello di significatività del %d%%\n→ I dati SONO stazionari.\n\n", crit["level"], alpha * 100))
    }
  }
}

# Esegue il test KPSS
kpss_test_all(training_set, "SPY_AdjClose")
# 
# [SPY_AdjClose] KPSS Test - Stazionarietà attorno a una media costante
# Statistic: 7.068768
# Valore critico (circa 1%): 0.739000
# Interpretazione del p-value: p-value < 0.01
# 
# ** CONCLUSIONE: Statistic > valore critico (1%) **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON sono stazionari.
# 
# 
# [SPY_AdjClose] KPSS Test - Stazionarietà attorno a un trend deterministico
# Statistic: 1.048090
# Valore critico (circa 1%): 0.216000
# Interpretazione del p-value: p-value < 0.01
# 
# ** CONCLUSIONE: Statistic > valore critico (1%) **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON sono stazionari.

# Entrambi i test hanno dato lo stesso esito per tutte le possibili ipotesi alternative.
# Possiamo affermare, con una significatività del 1%, che i dati non sono stazionari. Entrambi i test suggeriscono la presenza di una unit root:
# la serie non può essere direttamente utilizzata per stimare un modello GARCH, ma è necessario prima trasformarla in una forma stazionaria.


# Il metodo più semplice per rendere una serie stazionaria consiste nel calcolare la prima differenza della serie.
# Nel caso di una serie finanziaria, è preferibile applicare la trasformazione logaritmica.
# - La differenza elimina la componente di random walk.
# - La trasformazione logaritmica elimina un possibile trend deterministico esponenziale rendendolo lineare.

# Stazionarietà dei rendimenti logaritmici:
# ADF Test
#result <- urca::ur.df(na.omit(training_set[["SPY_LogReturn"]]), type = "none")
#summary(result)
adf_test_all(training_set, "SPY_LogReturn")
# 
# [SPY_LogReturn] ADF Test - Senza Intercetta
# Statistic: -14.609955
# p-value  : 2.41712e-30
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO stazionari.
# 
# 
# [SPY_LogReturn] ADF Test - Con Intercetta (Drift)
# Statistic: -14.679349
# p-value  : 1.5244e-29
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO stazionari.
# 
# 
# [SPY_LogReturn] ADF Test - Con Intercetta e Trend
# Statistic: -14.665818
# p-value  : 3.18746e-32
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO stazionari.


# KPSS Test
kpss_test_all(training_set, "SPY_LogReturn")
# 
# [SPY_LogReturn] KPSS Test - Stazionarietà attorno a una media costante
# Statistic: 0.067189
# Valore critico (circa 1%): 0.739000
# Interpretazione del p-value: p-value > 0.10
# 
# ** CONCLUSIONE: Statistic <= valore critico (1%) **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO stazionari.
# 
# 
# [SPY_LogReturn] KPSS Test - Stazionarietà attorno a un trend deterministico
# Statistic: 0.061828
# Valore critico (circa 1%): 0.216000
# Interpretazione del p-value: p-value > 0.10
# 
# ** CONCLUSIONE: Statistic <= valore critico (1%) **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO stazionari.

# Entrambi i test hanno dato lo stesso esito per tutte le possibili ipotesi alternative.
# Le trasformazioni effettuate, indicano che la nuova serie dei log-rendimenti può essere considerata stazionaria.


# Adesso, controlliamo anche la stazionarietà in varianza, ovvero se la serie è omoschedastica o eteroschedastica (incondizionatamente).
# Per prima cosa, vediamo se c'è curtosi nei rendimenti logaritmici.

# Calcola la curtosi della serie SPY_LogReturn
kurt_value <- kurtosis(training_set$SPY_LogReturn, na.rm = TRUE)

cat(sprintf("Curtosi: %.4f\n", kurt_value))
# Curtosi: 22.6554

# La curtosi c'è. Quindi, applichiamo la forma "studentized" dei test di Breusch-Pagan e White per la stazionarietà in varianza.

# Test di Breusch-Pagan.
# Il test di Breusch-Pagan verifica la presenza di eteroschedasticità non condizionata (varianza non costante degli errori) in un modello di
# regressione. Il test ha come:
#
# - H_0: I dati SONO omoschedastici (NON sono eteroschedastici);
# - H_1: I dati NON sono omoschedastici (SONO eteroschedastici).
breusch_pagan_test <- function(index, group, col, alpha = 0.01) {
  model <- lm(group ~ index)
  bp <- lmtest::bptest(model, studentize = TRUE)
  
  cat(sprintf("[%s] Breusch-Pagan Test\n", col))
  
  cat(sprintf("Statistic: %f\np-value: %g\n",
              bp$statistic, bp$p.value))
  
  if (bp$p.value > alpha) {
    cat(sprintf("\n** Conclusione: p-value > %.2f **\nNon possiamo rigettare l'ipotesi nulla con un livello di significatività del %d%%\n→ I dati NON sono eteroschedastici.\n\n",
                alpha, alpha * 100))
  } else {
    cat(sprintf("\n** Conclusione: p-value <= %.2f **\nPossiamo rigettare l'ipotesi nulla con un livello di significatività del %d%%\n→ I dati SONO eteroschedastici.\n\n",
                alpha, alpha * 100))
  }
}

# Estrai index e group, rimuovi righe con NA in group
valid_rows <- !is.na(training_set$SPY_LogReturn)
index_clean <- training_set$Index[valid_rows]
group_clean <- training_set$SPY_LogReturn[valid_rows]

# Esegue il test di Breusch-Pagan
breusch_pagan_test(index_clean, group_clean, "SPY_LogReturn")
# [SPY_LogReturn] Breusch-Pagan Test
# Statistic: 8.847582
# p-value: 0.00293477
# 
# ** Conclusione: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO eteroschedastici.

# Test di White.
# Anche il test di White verifica la presenza di eteroschedasticità non condizionata in un modello di regressione.
# Il test ha come:
#
# - H_0: I dati SONO omoschedastici (NON sono eteroschedastici);
# - H_1: I dati NON sono omoschedastici (SONO eteroschedastici).
white_test <- function(index, group, col, alpha = 0.01) {
  model <- lm(group ~ index)
  
  wt <- lmtest::bptest(model, ~ fitted.values(model) + I(fitted.values(model)^2), studentize = TRUE)
  
  cat(sprintf("[%s] White Test\n", col))
  
  cat(sprintf("Statistic: %f\np-value: %g\n",
              wt$statistic, wt$p.value))
  
  if (wt$p.value > alpha) {
    cat(sprintf("\n** Conclusione: p-value > %.2f **\nNon possiamo rigettare l'ipotesi nulla con un livello di significatività del %d%%\n→ I dati NON sono eteroschedastici.\n\n",
                alpha, alpha * 100))
  } else {
    cat(sprintf("\n** Conclusione: p-value <= %.2f **\nPossiamo rigettare l'ipotesi nulla con un livello di significatività del %d%%\n→ I dati SONO eteroschedastici.\n\n",
                alpha, alpha * 100))
  }
}

# Esegue il test di White
white_test(index_clean, group_clean, "SPY_LogReturn")
# [SPY_LogReturn] White Test
# Statistic: 12.469880
# p-value: 0.00195975
# 
# ** Conclusione: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO eteroschedastici.

# Alla luce dei risultati, possiamo rifiutare l'ipotesi nulla di omoschedasticità: c'è eteroschedasticità non condizionata.


# Un'altra caratteristica tipica delle serie finanziarie è la presenza di autocorrelazione tra i rendimenti, cioè la dipendenza tra
# osservazioni successive.
# Questa analisi è già stata svolta precedentemente, ma la rifacciamo per evidenziare solo i risultati relativi allo S&P 500.

# Test di Ljung-Box
ljungbox_test(training_set[["SPY_LogReturn"]], col_name = "SPY_LogReturn")
# Ljung-Box test per SPY_LogReturn (max_lag = 10):
#            lag   lb_stat   lb_pvalue
# X-squared    1  2.151799 0.142402974
# X-squared1   2  5.359999 0.068563197
# X-squared2   3 11.537234 0.009148828
# X-squared3   4 13.431263 0.009349960
# X-squared4   5 13.431468 0.019654094
# X-squared5   6 14.167211 0.027823087
# X-squared6   7 14.362075 0.045103743
# X-squared7   8 14.641084 0.066511746
# X-squared8   9 14.719625 0.098930951
# X-squared9  10 14.899972 0.135751491

# Osservando i p-value del test di Ljung-Box sui rendimenti logaritmici di SPY fino al lag 10,
# notiamo che l’ipotesi nulla di assenza di autocorrelazione viene rifiutata per alcuni lag intermedi
# (ad esempio, lag 3–7, dove p-value < 0.10). Ciò indica la presenza di autocorrelazione residua nei rendimenti.

# Tuttavia, il test di Ljung-Box misura l'autocorrelazione nei rendimenti, non nella loro varianza.
# Per valutare la presenza di autocorrelazione condizionata nella volatilità (cluster di volatilità),
# occorre invece eseguire un test ARCH di Engle.

# Se si osservano autocorrelazioni significative dal test ARCH di Engle, allora un modello GARCH
# può essere appropriato per modellare la dinamica della varianza condizionata.

# ACF
plot_acf(training_set, "SPY_LogReturn")

# PACF
plot_pacf(training_set, "SPY_LogReturn")

# L’analisi visiva di ACF e PACF mostra che solo una barra supera le bande di confidenza al 95%, ma non al 99%.
# Dunque, non c'è una forte evidenza visiva di autocorrelazione.


# Infine, come dicevamo prima, è fondamentale verificare l'esistenza dell'eteroschedasticità condizionata, ovvero se
# la varianza cambia nel tempo in modo sistematico. Se tale condizione non fosse presente, allora sarebbe inappropriato
# utilizzare un modello GARCH.

# Test ARCH di Engle.
# Il test ARCH di Engle è un test LM (Lagrange Multiplier) che ha come:
#
# - H_0: La serie non presenta effetti ARCH;
# - H_1: La serie presenta effetti ARCH con un certo ritardo (lag).
arch_effects_test <- function(series, col_name = NULL) {
  series <- as.numeric(na.omit(series))
  max_lag <- 5
  
  cat(sprintf("ARCH Test per %s (max_lag = %d):\n", col_name %||% "", max_lag))
  
  results <- data.frame(
    lag = integer(),
    arch_stat = numeric(),
    arch_pvalue = numeric()
  )
  
  for (lag in 1:max_lag) {
    test_res <- fDMA::archtest(series, lag = lag)
    results <- rbind(results, data.frame(
      lag = lag,
      arch_stat = test_res$statistic,
      arch_pvalue = test_res$p.value
    ))
  }
  
  print(results)
  cat("\n")
}

# Esegue il test ARCH di Engle
arch_effects_test(training_set[["SPY_LogReturn"]], col_name = "SPY_LogReturn")
# ARCH Test per SPY_LogReturn (max_lag = 5):
#            lag arch_stat  arch_pvalue
# statistic    1  27.36938 1.680719e-07
# statistic1   2  28.16878 7.642337e-07
# statistic2   3  70.84084 2.819652e-15
# statistic3   4  77.70128 5.343398e-16
# statistic4   5  77.77460 2.450060e-15

# Alla luce dei risultati, possiamo affermare che abbiamo un rigetto dell'ipotesi nulla di assenza di effetti ARCH con un livello di
# significatività dell'1% per tutti i lag.
# Pertanto, costruiremo un modello GARCH per i rendimenti logaritmici percentuali dei prezzi di chiusura aggiustati giornalieri dell’S&P 500
# presenti nel training set.


# Utilizziamo, dunque, il modello GARCH per stimare la volatilità dei ritorni logaritmici,
# esplorando diverse configurazioni di parametri (p, q) con p, q ∈ {0,...,4}, escludendo il caso p = q = 0.
# Per ciascuna configurazione, stimiamo il modello con diverse distribuzioni per le innovazioni:
# - normale ("norm")
# - skewed Student-t ("sstd")
# - skewed GED ("sged")
#
# Consideriamo solo i modelli per cui la stima converge.
# Tra questi, ordiniamo i modelli secondo i criteri informativi AICc e BIC, ignorando la log-likelihood (già incorporata in essi).
# 
# A partire dai modelli ordinati, selezioniamo il primo che soddisfa le seguenti condizioni sui residui standardizzati:
#  - assenza di autocorrelazione (verificata tramite test Ljung-Box, autocorrelogramma e autocorrelogramma parziale),
#  - coerenza con la distribuzione ipotizzata (tramite test di normalità quali SW, JB e QQ-Plot e test di Kolmogorov-Smirnov),
#  - verifica di stazionarietà e omoschedasticità non condizionata (tramite grafici e test quali KPSS, Breusch-Pagan e White),
#  - assenza di eteroschedasticità condizionata (tramite test ARCH di Engle).
#
# Questo approccio consente di selezionare un modello GARCH che bilancia qualità dell’adattamento,
# parsimonia e correttezza statistica dei residui.
# La libreria utilizzata per la stima è 'rugarch'.

# Serie dei rendimenti logaritmici
returns <- na.omit(training_set$SPY_LogReturn)

# Distribuzioni per le innovazioni
distributions <- c("norm", "sstd", "sged")

# Data frame per salvare i risultati
model_results <- data.frame(
  p = integer(),
  q = integer(),
  dist = character(),
  converged = logical(),
  hessian_ok = logical(),
  AIC = numeric(),
  AICc = numeric(),
  BIC = numeric(),
  stringsAsFactors = FALSE
)

# Loop su p e q in {0,1,2,3,4} escluso p = 0, q = 0
for (p in 0:4) {
  for (q in 0:4) {
    if (p == 0 && q == 0) next  # evita GARCH(0,0)
    for (dist in distributions) {
      spec <- ugarchspec(
        variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
        mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
        distribution.model = dist
      )
    
      tryCatch({
        fit <- ugarchfit(spec, data = returns, solver = "hybrid")
        info_criteria <- infocriteria(fit)
        loglik <- likelihood(fit)
        n <- length(returns)
        k <- length(coef(fit))
        AIC <- as.numeric(info_criteria[1])  # Akaike
        BIC <- as.numeric(info_criteria[2])  # Bayes
        AICc <- AIC + (2 * k^2 + 2 * k) / (n - k - 1)
        
        # Controlla se l'Hessiano è invertito correttamente
        hessian_ok <- all(!is.na(fit@fit$se.coef))
        
        model_results <- rbind(model_results, data.frame(
          p = p,
          q = q,
          dist = dist,
          converged = fit@fit$convergence == 0,
          hessian_ok = hessian_ok,
          AIC = AIC,
          AICc = AICc,
          BIC = BIC
        ))
      }, error = function(e) {
        message(sprintf("Errore per GARCH(%d,%d) - %s: %s", p, q, dist, e$message))
      })
    }
  }
}

# Ordina per AICc + BIC solo i modelli che:
# - convergono
# - hanno invertito l’Hessiano correttamente
valid_models <- model_results %>% 
  filter(converged == TRUE, hessian_ok == TRUE) %>%
  arrange(AICc + BIC)

# Stampa i modelli validi ordinati
print(valid_models)
#    p q dist converged hessian_ok       AIC      AICc       BIC
# 1  1 1 sstd      TRUE       TRUE  2.543947  2.672152  2.587842
# 2  1 1 sged      TRUE       TRUE  2.548239  2.676444  2.592133
# 3  1 0 sstd      TRUE       TRUE  2.587333  2.672621  2.622449
# 4  2 1 sstd      TRUE       TRUE  2.547432  2.727304  2.600106
# 5  1 2 sstd      TRUE       TRUE  2.548272  2.728144  2.600946
# 6  0 1 sstd      TRUE       TRUE  2.606670  2.691958  2.641786
# 7  2 1 sged      TRUE       TRUE  2.551877  2.731749  2.604551
# 8  1 2 sged      TRUE       TRUE  2.552684  2.732556  2.605357
# 9  2 0 sstd      TRUE       TRUE  2.586443  2.714648  2.630338
# 10 1 1 norm      TRUE       TRUE  2.634497  2.685561  2.660834
# 11 3 0 sstd      TRUE       TRUE  2.564514  2.744385  2.617187
# 12 3 1 sstd      TRUE       TRUE  2.538394  2.778737  2.599846
# 13 3 1 sged      TRUE       TRUE  2.543024  2.783368  2.604477
# 14 0 2 sstd      TRUE       TRUE  2.610328  2.738533  2.654222
# 15 2 1 norm      TRUE       TRUE  2.638648  2.723936  2.673764
# 16 1 2 norm      TRUE       TRUE  2.639231  2.724519  2.674346
# 17 2 2 sstd      TRUE       TRUE  2.551652  2.791995  2.613104
# 18 1 3 sstd      TRUE       TRUE  2.552205  2.792549  2.613658
# 19 2 2 sged      TRUE       TRUE  2.556096  2.796440  2.617549
# 20 1 3 sged      TRUE       TRUE  2.556575  2.796919  2.618028
# 21 3 1 norm      TRUE       TRUE  2.623741  2.751946  2.667635
# 22 4 0 sged      TRUE       TRUE  2.559381  2.799724  2.620834
# 23 4 0 sstd      TRUE       TRUE  2.560955  2.801299  2.622408
# 24 4 0 norm      TRUE       TRUE  2.633297  2.761502  2.677191
# 25 1 3 norm      TRUE       TRUE  2.641110  2.769315  2.685005
# 26 2 2 norm      TRUE       TRUE  2.642867  2.771073  2.686762
# 27 4 1 norm      TRUE       TRUE  2.612933  2.792804  2.665606
# 28 0 3 sstd      TRUE       TRUE  2.614369  2.794241  2.667043
# 29 4 1 sstd      TRUE       TRUE  2.541340  2.851018  2.611572
# 30 3 2 sstd      TRUE       TRUE  2.542613  2.852291  2.612845
# 31 4 1 sged      TRUE       TRUE  2.543761  2.853438  2.613992
# 32 3 0 norm      TRUE       TRUE  2.673865  2.759153  2.708981
# 33 3 2 sged      TRUE       TRUE  2.547244  2.856921  2.617475
# 34 3 2 norm      TRUE       TRUE  2.627960  2.807832  2.680634
# 35 2 3 sstd      TRUE       TRUE  2.555831  2.865508  2.626062
# 36 1 4 sstd      TRUE       TRUE  2.556739  2.866416  2.626970
# 37 2 3 sged      TRUE       TRUE  2.560243  2.869921  2.630475
# 38 1 4 sged      TRUE       TRUE  2.561208  2.870886  2.631440
# 39 1 4 norm      TRUE       TRUE  2.641904  2.821775  2.694577
# 40 4 2 norm      TRUE       TRUE  2.609231  2.849575  2.670684
# 41 2 3 norm      TRUE       TRUE  2.646709  2.826580  2.699382
# 42 0 4 sstd      TRUE       TRUE  2.618468  2.858811  2.679920
# 43 4 2 sged      TRUE       TRUE  2.542860  2.930791  2.621870
# 44 4 2 sstd      TRUE       TRUE  2.545560  2.933491  2.624570
# 45 3 3 sstd      TRUE       TRUE  2.546833  2.934764  2.625843
# 46 3 3 norm      TRUE       TRUE  2.632180  2.872523  2.693632
# 47 3 3 sged      TRUE       TRUE  2.551463  2.939394  2.630473
# 48 2 4 sstd      TRUE       TRUE  2.560185  2.948116  2.639196
# 49 2 4 sged      TRUE       TRUE  2.564673  2.952604  2.643684
# 50 2 4 norm      TRUE       TRUE  2.651511  2.891855  2.712964
# 51 4 3 norm      TRUE       TRUE  2.613451  2.923128  2.683682
# 52 1 0 norm      TRUE       TRUE  2.794771  2.820249  2.812329
# 53 3 4 norm      TRUE       TRUE  2.637390  2.947068  2.707622
# 54 4 3 sged      TRUE       TRUE  2.547080  3.022242  2.634869
# 55 4 3 sstd      TRUE       TRUE  2.549779  3.024941  2.637568
# 56 3 4 sstd      TRUE       TRUE  2.551580  3.026742  2.639369
# 57 2 0 norm      TRUE       TRUE  2.795653  2.846717  2.821990
# 58 3 4 sged      TRUE       TRUE  2.556257  3.031419  2.644046
# 59 4 4 norm      TRUE       TRUE  2.617670  3.005601  2.696680
# 60 4 4 sged      TRUE       TRUE  2.551299  3.122728  2.647867
# 61 4 4 sstd      TRUE       TRUE  2.553999  3.125427  2.650567
# 62 0 1 norm      TRUE       TRUE  2.928595  2.954073  2.946153
# 63 0 2 norm      TRUE       TRUE  2.930480  2.981544  2.956817
# 64 0 3 norm      TRUE       TRUE  2.933902  3.019190  2.969018
# 65 0 4 norm      TRUE       TRUE  2.937641  3.065846  2.981535
# 66 0 2 sged      TRUE       TRUE  6.417943  6.546148  6.461838
# 67 0 1 sged      TRUE       TRUE 10.632542 10.717830 10.667658
# 68 3 0 sged      TRUE       TRUE 44.803797 44.983669 44.856471
# 69 2 0 sged      TRUE       TRUE 62.200416 62.328621 62.244311

# N.B.: Teniamo presente che p rappresenta il GARCH order (indice beta) e q l'ARCH order (indice alfa).

# Analizziamo il primo modello: GARCH(1,1) sstd
p <- valid_models$p[1]
q <- valid_models$q[1]
dist <- valid_models$dist[1]

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = dist
)

fit <- tryCatch(
  ugarchfit(spec = spec, data = returns, solver = "hybrid"),
  error = function(e) NULL
)

# Risultati
show(fit)
# 
# *---------------------------------*
# *          GARCH Model Fit        *
# *---------------------------------*
#   
# Conditional Variance Dynamics 	
# -----------------------------------
# GARCH Model	    : sGARCH(1,1)
# Mean Model	    : ARFIMA(0,0,0)
# Distribution	  : sstd 
# 
# Optimal Parameters
# ------------------------------------
#         Estimate  Std. Error  t value Pr(>|t|)
# omega   0.081727    0.034748   2.3520 0.018674
# alpha1  0.106856    0.038130   2.8024 0.005072
# beta1   0.810146    0.051279  15.7989 0.000000
# skew    0.825109    0.049559  16.6491 0.000000
# shape   4.827492    1.149651   4.1991 0.000027
# 
# Robust Standard Errors:
#         Estimate  Std. Error  t value Pr(>|t|)
# omega   0.081727    0.027731   2.9471 0.003207
# alpha1  0.106856    0.038507   2.7749 0.005521
# beta1   0.810146    0.035873  22.5840 0.000000
# skew    0.825109    0.043362  19.0284 0.000000
# shape   4.827492    1.447355   3.3354 0.000852
# 
# LogLikelihood : -597.9155 
# 
# Information Criteria
# ------------------------------------
#   
# Akaike       2.5439
# Bayes        2.5878
# Shibata      2.5437
# Hannan-Quinn 2.5612
# 
# Weighted Ljung-Box Test on Standardized Residuals
# ------------------------------------
#                         statistic p-value
# Lag[1]                   0.008892  0.9249
# Lag[2*(p+q)+(p+q)-1][2]  0.091217  0.9258
# Lag[4*(p+q)+(p+q)-1][5]  0.859801  0.8905
# d.o.f=0
# H0 : No serial correlation
# 
# Weighted Ljung-Box Test on Standardized Squared Residuals
# ------------------------------------
#                         statistic   p-value
# Lag[1]                      2.327 1.272e-01
# Lag[2*(p+q)+(p+q)-1][5]    19.245 3.125e-05
# Lag[4*(p+q)+(p+q)-1][9]    29.755 5.093e-07
# d.o.f=2
# 
# Weighted ARCH LM Tests
# ------------------------------------
#             Statistic Shape Scale   P-Value
# ARCH Lag[3]     1.689 0.500 2.000 1.938e-01
# ARCH Lag[5]    30.270 1.440 1.667 5.361e-08
# ARCH Lag[7]    32.410 2.315 1.543 3.729e-08
# 
# Nyblom stability test
# ------------------------------------
# Joint Statistic:  1.1825
# Individual Statistics:              
# omega  0.04244
# alpha1 0.06196
# beta1  0.03271
# skew   0.05418
# shape  0.29712
# 
# Asymptotic Critical Values (10% 5% 1%)
# Joint Statistic:     	   1.28 1.47 1.88
# Individual Statistic:	   0.35 0.47 0.75
# 
# Sign Bias Test
# ------------------------------------
#                    t-value   prob sig
# Sign Bias          1.06693 0.2866    
# Negative Sign Bias 1.10803 0.2684    
# Positive Sign Bias 0.09087 0.9276    
# Joint Effect       6.17971 0.1032    
# 
# 
# Adjusted Pearson Goodness-of-Fit Test:
# ------------------------------------
#   group statistic p-value(g-1)
# 1    20     19.16      0.44633
# 2    30     33.22      0.26914
# 3    40     38.24      0.50453
# 4    50     64.82      0.06447
# 
# 
# Elapsed time : 0.1756508

# Analizzando i p-value del test pesato di Ljung-Box sui residui standardizzati, osserviamo che questi sono tutti maggiori di α = 0.01
# (in realtà anche di α = 0.05 e α = 0.10). Quindi, non abbiamo evidenza statistica sufficiente per rifiutare l’ipotesi nulla di assenza di
# autocorrelazione.

# Applichiamo, ora, dei dest di normalità sui residui.

# Estrai residui standardizzati
z_hat <- residuals(fit, standardize = TRUE)

# Prendi le date del training_set:
# Rimuoviamo le righe con NA nella colonna usata per il modello
valid_dates <- training_set$Date[!is.na(training_set$SPY_LogReturn)]

# Converti in Date se non lo sono già
valid_dates <- as.Date(valid_dates)

# Sovrascrivi l’index di z_hat con le date corrette
index(z_hat) <- valid_dates

# Controlla che le lunghezze coincidano
if(length(z_hat) != length(valid_dates)) {
  stop("Errore: lunghezza residui e date non coincide!")
}

# Visualizza i primi valori
head(z_hat)
#                  [,1]
# 2023-08-01 -0.2715260
# 2023-08-02 -1.4058166
# 2023-08-03 -0.2741671
# 2023-08-04 -0.4582823
# 2023-08-07  0.9179650
# 2023-08-08 -0.4623469

# Test di Shapiro-Wilk (SW).
# Questo test è uno dei più potenti per la verifica della normalità, specialmente con campioni piccoli o medi. È molto sensibile a
# deviazioni dalla normalità sia nelle code che nella parte centrale della distribuzione. Il test ha come:
#
# - H_0: I dati PROVENGONO da una distribuzione normale;
# - H_1: I dati NON provengono da una distribuzione normale.
shapirowilk_test <- function(x, name = "Serie", alpha = 0.01) {
  test <- shapiro.test(x)
  
  cat(sprintf("\n[%s] Shapiro-Wilk Test\n", name))
  
  cat(sprintf("Statistic: %f\n", test$statistic))
  cat(sprintf("p-value  : %g\n", test$p.value))
  
  if (test$p.value <= alpha) {
    cat(sprintf("\n** CONCLUSIONE: p-value <= %.2f **\nPossiamo rigettare l'ipotesi nulla con un livello di significatività del %d%%\n→ I dati NON seguono una distribuzione normale.\n\n", alpha, alpha * 100))
  } else {
    cat(sprintf("\n** CONCLUSIONE: p-value > %.2f **\nNon possiamo rigettare l'ipotesi nulla con un livello di significatività del %d%%\n→ I dati SEGUONO una distribuzione normale.\n\n", alpha, alpha * 100))
  }
}

# Esegue il test SW
shapirowilk_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Shapiro-Wilk Test
# Statistic: 0.962873
# p-value  : 1.42984e-09
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Test di Jarque-Bera.
# Il test di Jarque-Bera (JB) combina test di skewness e kurtosis per verificare la normalità ed è più adatto per campioni più grandi.
# Tuttavia, sono meno sensibili alle deviazioni rispetto a Shapiro-Wilk, specialmente per campioni piccoli e medi. Il test ha come:
#
# - H_0: I dati PROVENGONO da una distribuzione normale;
# - H_1: I dati NON provengono da una distribuzione normale.
jarque_bera_test <- function(x, name = "Serie", alpha = 0.01) {
  test <- jarque.bera.test(x)
  
  cat(sprintf("\n[%s] Jarque-Bera Test\n", name))
  
  cat(sprintf("Statistic: %f\n", test$statistic))
  cat(sprintf("p-value  : %g\n", test$p.value))
  
  if (test$p.value <= alpha) {
    cat(sprintf("\n** CONCLUSIONE: p-value <= %.2f **\nPossiamo rigettare l'ipotesi nulla con un livello di significatività del %d%%\n→ I dati NON seguono una distribuzione normale.\n\n", alpha, alpha * 100))
  } else {
    cat(sprintf("\n** CONCLUSIONE: p-value > %.2f **\nNon possiamo rigettare l'ipotesi nulla con un livello di significatività del %d%%\n→ I dati SEGUONO una distribuzione normale.\n\n", alpha, alpha * 100))
  }
}

# Esegue il test JB
jarque_bera_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Jarque-Bera Test
# Statistic: 207.549710
# p-value  : 0
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Come atteso, i residui standardizzati del GARCH(1,1) con distribuzione skewed Student-t non seguono una distribuzione normale.

# QQ-Plot SSTD:
# Residui standardizzati ordinati
y <- sort(as.numeric(z_hat))
n <- length(y)

# Parametri stimati da 'fit'
params <- coef(fit)
SSTD_xi <- params["skew"]
SSTD_nu <- params["shape"]
mean <- 0
sd <- 1

# Parametri della distribuzione skewed Student-t
distr <- "sstd"
distr_pars <- list(mean = mean, sd = sd, nu = SSTD_nu, xi = SSTD_xi)

# Quantili teorici
quants <- qsstd(ppoints(n), mean = mean, sd = sd, nu = SSTD_nu, xi = SSTD_xi)

# DataFrame per plotting
QQ_plot_df <- data.frame(
  T = 1:n,
  Q = quants,
  X = y,
  Y = y
)

# Calcolo retta interquartile
quart_probs <- c(0.25, 0.75)
quart_X <- as.vector(quantile(QQ_plot_df$X, quart_probs))
quart_Q <- qsstd(quart_probs, mean = mean, sd = sd, nu = SSTD_nu, xi = SSTD_xi)
slope <- diff(quart_X) / diff(quart_Q)
intercept <- quart_X[1] - slope * quart_Q[1]

# Titoli
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 MPSMF 2024-2025",
                             paste("Q-Q plot of the Standardized Residuals from the GARCH(1,1) model for SPY (skewed Student-t)")))
subtitle_content <- bquote(paste("Sample size: ", .(n), 
                                 "; SSTD parameters → mean = ", .(mean), 
                                 ", sd = ", .(sd), 
                                 ", df = ", .(round(SSTD_nu, 3)), 
                                 ", skew = ", .(round(SSTD_xi, 3)), "."))

caption_content <- "Author: Matteo Basili"
x_name <- bquote("Theoretical Quantiles (SSTD)")
y_name <- bquote("Standardized Residuals")

# Assi e legende
x_breaks <- seq(from = floor(min(QQ_plot_df$Q)), to = ceiling(max(QQ_plot_df$Q)), by = 0.5)
x_labs <- format(x_breaks, scientific = FALSE)
y_breaks_num <- length(x_breaks)
y_binwidth <- round((max(QQ_plot_df$Y) - min(QQ_plot_df$Y)) / y_breaks_num, digits = 3)
y_breaks <- round(seq(from = floor(min(QQ_plot_df$Y)/y_binwidth)*y_binwidth, 
                      to = ceiling(max(QQ_plot_df$Y)/y_binwidth)*y_binwidth, 
                      by = y_binwidth), 3)
y_labs <- format(y_breaks, scientific = FALSE)

# Legende e colori
leg_shape_labs <- bquote("Q-Q plot")
leg_fill_labs <- c(bquote("90% confidence interval"), bquote("95% confidence interval"))
leg_col_labs <- c(bquote("Interquartile line"), bquote("Regression line"), bquote("y = x"))

Stand_Res_std_QQ_plot <- ggplot(QQ_plot_df) +
  stat_qq_band(aes(sample = X, fill = "95"), distribution = distr, dparams = distr_pars, conf = 0.95) +
  stat_qq_band(aes(sample = X, fill = "90"), distribution = distr, dparams = distr_pars, conf = 0.90) +
  geom_abline(aes(slope = slope, intercept = intercept, colour = "IQR"), linewidth = 0.8) +
  stat_smooth(aes(x = Q, y = Y, colour = "Reg", group = 1), method = "lm", se = FALSE, linewidth = 0.8) +
  geom_abline(aes(slope = 1, intercept = 0, colour = "45deg"), linewidth = 0.8) +
  stat_qq_point(aes(sample = X), shape = 19, color = "black", size = 1.2, alpha = 0.8, distribution = distr, dparams = distr_pars) +
  scale_x_continuous(name = x_name, breaks = x_breaks, labels = x_labs) +
  scale_y_continuous(name = y_name, breaks = y_breaks, labels = NULL,
                     sec.axis = sec_axis(~ ., breaks = y_breaks, labels = y_labs)) +
  ggtitle(title_content) +
  labs(subtitle = subtitle_content, caption = caption_content) +
  scale_shape_manual(name = "", labels = leg_shape_labs, values = c("19")) +
  scale_fill_manual(name = "", values = c("90" = "chartreuse1", "95" = "deepskyblue1"), labels = leg_fill_labs) +
  scale_colour_manual(name = "", values = c("IQR" = "cyan", "Reg" = "red", "45deg" = "black"), labels = leg_col_labs) +
  guides(shape = guide_legend(order = 1), fill = guide_legend(order = 2), colour = guide_legend(order = 3)) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.x = element_text(angle = 0, vjust = 1),
    legend.key.width = unit(0.8, "cm"),
    legend.position = "bottom"
  )

# Mostra il plot
plot(Stand_Res_std_QQ_plot)

# Calcola densità teorica nei quantili (pdf)
dens <- dsstd(quants, mean=mean, sd=sd, nu=SSTD_nu, xi=SSTD_xi)

# Calcola errore standard dei quantili teorici (formula banda Q-Q)
# Errore standard: SE = (p * (1-p)) / (n * f(Q)^2)
p <- ppoints(n)
SE <- sqrt(p * (1-p) / (n * dens^2))

# Limiti di confidenza (95%)
alpha <- 0.05
z <- qnorm(1 - alpha/2)

lower_95 <- quants - z * SE
upper_95 <- quants + z * SE

# Verifica quanti punti osservati cadono dentro le bande
# Ricorda che QQ_plot_df$X = dati ordinati osservati

inside_95 <- (QQ_plot_df$X >= lower_95) & (QQ_plot_df$X <= upper_95)

percent_inside_95 <- mean(inside_95) * 100

cat(sprintf("Percentuale di punti dentro la banda di confidenza al 95%%: %.2f%%\n", percent_inside_95))
# Percentuale di punti dentro la banda di confidenza al 95%: 96.41%

# Alpha per il 90%
alpha_90 <- 0.10
z_90 <- qnorm(1 - alpha_90/2)  # quantile normale per il 90%

# Limiti di confidenza al 90%
lower_90 <- quants - z_90 * SE
upper_90 <- quants + z_90 * SE

# Verifica quanti punti osservati cadono dentro la banda 90%
inside_90 <- (QQ_plot_df$X >= lower_90) & (QQ_plot_df$X <= upper_90)

percent_inside_90 <- mean(inside_90) * 100

cat(sprintf("Percentuale di punti dentro la banda di confidenza al 90%%: %.2f%%\n", percent_inside_90))
# Percentuale di punti dentro la banda di confidenza al 90%: 86.92%

# Kolmogorov-Smirnov test (K-S test)
ks_result <- ks.test(y, 
                     y = function(q) psstd(q, mean=0, sd=1, nu=SSTD_nu, xi=SSTD_xi),
                     alternative = "two.sided")

print(ks_result)
# 
#         Asymptotic one-sample Kolmogorov-Smirnov test
# 
# data:  y
# D = 0.049803, p-value = 0.1903
# alternative hypothesis: two-sided

# Dall'analisi del Q-Q Plot e del test di K-S vediamo, quindi, che i residui standardizzati seguono la distribuzione skewed Student-t con un livello
# di significatività del 5% (in realtà il test di K-S risulta anche al 10%).

# Verifichiamo ora la stazionarietà e l'omoschedasticità (non condizionata).

# Grafico dei residui
df_residui <- data.frame(
  Date = index(z_hat),
  Residui = as.numeric(z_hat)
)

ggplot(df_residui, aes(x = Date, y = Residui)) +
  geom_line(color = "steelblue", linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  labs(
    title = "Grafico dei residui standardizzati",
    subtitle = "Modello GARCH(1,1) con distribuzione skewed Student-t per SPY",
    x = "Data",
    y = "Residui standardizzati"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    axis.title = element_text(size = 11)
  )

# KPSS Test
kpss_test_level <- function(data, col, alpha = 0.01) {
  x <- na.omit(data[[col]])

  # Valori critici per null = "Level"
  critical_values <- c(`10%` = 0.347, `5%` = 0.463, `2.5%` = 0.574, `1%` = 0.739)

  # Funzione per trovare la soglia più vicina
  get_critical <- function(cv, alpha) {
    key <- names(cv)[which.min(abs(as.numeric(gsub("%", "", names(cv))) / 100 - alpha))]
    value <- unname(cv[[key]])
    return(c(value = as.numeric(value), level = key))
  }

  # Funzione per stimare il p-value approssimato
  estimate_pvalue <- function(stat, cv) {
    if (stat > cv["1%"]) {
      return("p-value < 0.01")
    } else if (stat > cv["2.5%"]) {
      return("p-value ≈ 0.025")
    } else if (stat > cv["5%"]) {
      return("p-value ≈ 0.05")
    } else if (stat > cv["10%"]) {
      return("p-value ≈ 0.10")
    } else {
      return("p-value > 0.10")
    }
  }

  cat(sprintf("\n[%s] KPSS Test - Stazionarietà attorno a una media costante\n", col))

  result <- kpss.test(x, null = "Level")
  stat <- result$statistic
  crit <- get_critical(critical_values, alpha)

  pvalue_msg <- estimate_pvalue(stat, critical_values)

  cat(sprintf("Statistic: %f\n", stat))
  cat(sprintf("Valore critico (circa %s): %f\n", crit["level"], as.numeric(crit["value"])))
  cat(sprintf("Interpretazione del p-value: %s\n", pvalue_msg))

  if (stat > crit["value"]) {
    cat(sprintf("\n** CONCLUSIONE: Statistic > valore critico (%s) **\nPossiamo rigettare l'ipotesi nulla con un livello di significatività del %d%%\n→ I dati NON sono stazionari.\n\n", crit["level"], alpha * 100))
  } else {
    cat(sprintf("\n** CONCLUSIONE: Statistic <= valore critico (%s) **\nNon possiamo rigettare l'ipotesi nulla con un livello di significatività del %d%%\n→ I dati SONO stazionari.\n\n", crit["level"], alpha * 100))
  }
}

kpss_test_level(df_residui, col = "Residui", alpha = 0.01)
# 
# [Residui] KPSS Test - Stazionarietà attorno a una media costante
# Statistic: 0.115609
# Valore critico (circa 1%): 0.739000
# Interpretazione del p-value: p-value > 0.10
# 
# ** CONCLUSIONE: Statistic <= valore critico (1%) **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO stazionari.

# Quindi, i residui standardizzati sono stazionari.

# Per l'omoschedasticità non condizionata, per prima cosa, vediamo se c'è curtosi nei residui standardizzati.

# Calcola la curtosi
kurt_z_hat <- kurtosis(as.numeric(z_hat), na.rm = TRUE)

cat(sprintf("Curtosi dei residui standardizzati: %.4f\n", kurt_z_hat))
# Curtosi dei residui standardizzati: 6.0689

# La curtosi c'è. Quindi, applichiamo la forma "studentized" dei test di Breusch-Pagan e White per la stazionarietà in varianza.

# Breusch-Pagan Test:
# Crea un indice temporale (può essere anche un numero progressivo)
df_residui$Index <- seq_along(df_residui$Residui)

# Esegui il test BP
breusch_pagan_test(
  index = df_residui$Index,
  group = df_residui$Residui,
  col = "Residui standardizzati"
)
# [Residui standardizzati] Breusch-Pagan Test
# Statistic: 4.725135
# p-value: 0.0297248
# 
# ** Conclusione: p-value > 0.01 **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON sono eteroschedastici.

# White Test
white_test(
  index = df_residui$Index,
  group = df_residui$Residui,
  col = "Residui standardizzati"
)
# [Residui standardizzati] White Test
# Statistic: 5.843998
# p-value: 0.053826
# 
# ** Conclusione: p-value > 0.01 **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON sono eteroschedastici.

# Dai risultati dei test si vede che è verificata anche la condizione di omoschedasticità (non condizionata).

# Per verificare ulteriormente l'assenza di autocorrelazione per i residui standardizzati
# di questo modello, facciamo anche l'ACF, il PACF e il test di Ljung-Box classico.

# ACF
plot_acf_residuals <- function(data, col, max_lag = NULL, ci_levels = c(0.90, 0.95, 0.99)) {

  x <- data[[col]]
  x <- na.omit(x)
  n <- length(x)

  if (is.null(max_lag)) {
    max_lag <- min(10, floor(n / 4))  # Hyndman
  }

  # Calcolo ACF (escludendo lag 0, che è sempre 1)
  acf_res <- acf(x, lag.max = max_lag, plot = FALSE)
  acf_vals <- acf_res$acf[-1]
  lags <- 1:length(acf_vals)

  # Calcolo intervalli di confidenza per i livelli dati
  ci_vals <- sapply(ci_levels, function(level) qnorm((1 + level) / 2) / sqrt(n))
  names(ci_vals) <- paste0(ci_levels * 100, "%")

  # Dataframe ACF
  df <- data.frame(lag = lags, acf = acf_vals)

  # Dataframe intervalli di confidenza per ggplot (sia + che -)
  ci_df <- do.call(rbind, lapply(names(ci_vals), function(name) {
    data.frame(
      ci_level = name,
      y = c(ci_vals[name], -ci_vals[name])
    )
  }))

  ci_df$ci_level <- factor(ci_df$ci_level, levels = c("90%", "95%", "99%"))

  ci_colors <- c("90%" = "green", "95%" = "blue", "99%" = "red")
  ci_linetypes <- c("90%" = "dotted", "95%" = "dashed", "99%" = "dotdash")

  p <- ggplot(df, aes(x = lag, y = acf)) +
    geom_segment(aes(x = lag, xend = lag, y = 0, yend = acf), color = "black") +
    geom_point(shape = 18, size = 3, color = "black") +
    theme_minimal() +
    geom_hline(data = ci_df, aes(yintercept = y, color = ci_level, linetype = ci_level), size = 1.1) +
    scale_color_manual(name = "Confidence Interval", values = ci_colors) +
    scale_linetype_manual(name = "Confidence Interval", values = ci_linetypes) +
    labs(title = "ACF dei Residui Standardizzati", x = "Lag", y = "ACF Value") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5)
    )

  print(p)
}

df_z <- data.frame(z_hat = as.numeric(z_hat))
plot_acf_residuals(df_z, col = "z_hat")

# PACF
plot_pacf_residuals <- function(data, col, max_lag = NULL, ci_levels = c(0.90, 0.95, 0.99)) {

  x <- data[[col]]
  x <- na.omit(x)
  n <- length(x)

  if (is.null(max_lag)) {
    max_lag <- min(10, floor(n / 4))  # Hyndman
  }

  # Calcolo PACF (rimuovo lag 0, che è sempre 1)
  pacf_res <- pacf(x, lag.max = max_lag, plot = FALSE)
  pacf_vals <- pacf_res$acf
  lags <- 1:length(pacf_vals)

  # Intervalli di confidenza per i livelli dati
  ci_vals <- sapply(ci_levels, function(level) qnorm((1 + level) / 2) / sqrt(n))
  names(ci_vals) <- paste0(ci_levels * 100, "%")

  # Dataframe PACF
  df <- data.frame(lag = lags, pacf = pacf_vals)

  # Dataframe intervalli di confidenza per ggplot (+ e -)
  ci_df <- do.call(rbind, lapply(names(ci_vals), function(name) {
    data.frame(
      ci_level = name,
      y = c(ci_vals[name], -ci_vals[name])
    )
  }))
  ci_df$ci_level <- factor(ci_df$ci_level, levels = c("90%", "95%", "99%"))

  # Palette e linee
  ci_colors <- c("90%" = "green", "95%" = "blue", "99%" = "red")
  ci_linetypes <- c("90%" = "dotted", "95%" = "dashed", "99%" = "dotdash")

  p <- ggplot(df, aes(x = lag, y = pacf)) +
    geom_segment(aes(x = lag, xend = lag, y = 0, yend = pacf), color = "black") +
    geom_point(shape = 18, size = 3, color = "black") +
    theme_minimal() +
    geom_hline(data = ci_df, aes(yintercept = y, color = ci_level, linetype = ci_level), size = 1.1) +
    scale_color_manual(name = "Confidence Interval", values = ci_colors) +
    scale_linetype_manual(name = "Confidence Interval", values = ci_linetypes) +
    labs(title = "PACF dei Residui Standardizzati", x = "Lag", y = "PACF Value") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5)
    )

  print(p)
}

plot_pacf_residuals(df_z, col = "z_hat")

# L’analisi visiva di ACF e PACF mostra che, in entrambi i grafici, nessuna barra supera la banda di confidenza al 90%.
# Dunque, non c'è una forte evidenza visiva di autocorrelazione.

# Ljung-Box Test
ljungbox_test(as.numeric(z_hat), col_name = "Residui standardizzati")
# Ljung-Box test per Residui standardizzati (max_lag = 10):
#            lag     lb_stat lb_pvalue
# X-squared    1 0.008892036 0.9248728
# X-squared1   2 0.173541902 0.9168871
# X-squared2   3 0.963572979 0.8100652
# X-squared3   4 1.570977615 0.8139986
# X-squared4   5 1.582018775 0.9034152
# X-squared5   6 2.302617752 0.8898709
# X-squared6   7 2.633265406 0.9167323
# X-squared7   8 2.709263273 0.9512583
# X-squared8   9 3.178790794 0.9567824
# X-squared9  10 3.219150077 0.9757860

# Osservando i p-value associati ai vari lag, vediamo che tutti questi sono maggiori di α = 0.10. Dunque, non abbiamo evidenza statistica
# sufficiente per rifiutare l’ipotesi nulla di assenza di autocorrelazione fino al lag specificato.

# Infine, verifichiamo anche l’assenza di eteroschedasticità condizionata nei residui standardizzati.

# Test ARCH di Engle
arch_effects_test(z_hat, col_name = "Residui Standardizzati")
# ARCH Test per Residui Standardizzati (max_lag = 5):
#            lag arch_stat  arch_pvalue
# statistic    1  2.920659 8.745212e-02
# statistic1   2  5.011056 8.163249e-02
# statistic2   3  7.400773 6.016358e-02
# statistic3   4 39.471208 5.567405e-08
# statistic4   5 39.612275 1.787826e-07

# Alla luce dei risultati, a partire dal lag 4, il test ARCH rivela evidenza significativa di eteroschedasticità residua, con p-value < 1%.
# Questo suggerisce che il modello GARCH(1,1) con distribuzione skewed Student-t non ha catturato completamente la dinamica della varianza
# condizionata, e che potrebbero essere presenti effetti ARCH residui a orizzonti più lunghi.
# Perciò, possiamo concludere che questo modello è da scartare.

# Analizziamo il prossimo nella classifica: GARCH(1,1) sged
p <- valid_models$p[2]
q <- valid_models$q[2]
dist <- valid_models$dist[2]

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = dist
)

fit <- tryCatch(
  ugarchfit(spec = spec, data = returns, solver = "hybrid"),
  error = function(e) NULL
)

# Risultati
show(fit)
# 
# *---------------------------------*
# *          GARCH Model Fit        *
# *---------------------------------*
#   
# Conditional Variance Dynamics 	
# -----------------------------------
# GARCH Model	    : sGARCH(1,1)
# Mean Model	    : ARFIMA(0,0,0)
# Distribution	  : sged 
# 
# Optimal Parameters
# ------------------------------------
#         Estimate  Std. Error  t value Pr(>|t|)
# omega   0.061486    0.021326   2.8832 0.003937
# alpha1  0.114254    0.032876   3.4753 0.000510
# beta1   0.818393    0.038093  21.4840 0.000000
# skew    0.879638    0.044189  19.9062 0.000000
# shape   1.219957    0.109453  11.1459 0.000000
# 
# Robust Standard Errors:
#         Estimate  Std. Error  t value Pr(>|t|)
# omega   0.061486    0.014927   4.1190 0.000038
# alpha1  0.114254    0.037891   3.0153 0.002567
# beta1   0.818393    0.025834  31.6785 0.000000
# skew    0.879638    0.047178  18.6453 0.000000
# shape   1.219957    0.150331   8.1151 0.000000
# 
# LogLikelihood : -598.9325 
# 
# Information Criteria
# ------------------------------------
#   
# Akaike       2.5482
# Bayes        2.5921
# Shibata      2.5480
# Hannan-Quinn 2.5655
# 
# Weighted Ljung-Box Test on Standardized Residuals
# ------------------------------------
#                         statistic p-value
# Lag[1]                   0.004364  0.9473
# Lag[2*(p+q)+(p+q)-1][2]  0.050900  0.9556
# Lag[4*(p+q)+(p+q)-1][5]  0.650584  0.9322
# d.o.f=0
# H0 : No serial correlation
# 
# Weighted Ljung-Box Test on Standardized Squared Residuals
# ------------------------------------
#                         statistic   p-value
# Lag[1]                      1.293 2.556e-01
# Lag[2*(p+q)+(p+q)-1][5]    14.877 4.795e-04
# Lag[4*(p+q)+(p+q)-1][9]    23.251 2.854e-05
# d.o.f=2
# 
# Weighted ARCH LM Tests
# ------------------------------------
#             Statistic Shape Scale   P-Value
# ARCH Lag[3]    0.7102 0.500 2.000 3.994e-01
# ARCH Lag[5]   23.4432 1.440 1.667 2.897e-06
# ARCH Lag[7]   25.2295 2.315 1.543 2.865e-06
# 
# Nyblom stability test
# ------------------------------------
# Joint Statistic:  0.9849
# Individual Statistics:             
# omega  0.1061
# alpha1 0.1903
# beta1  0.1113
# skew   0.1672
# shape  0.4236
# 
# Asymptotic Critical Values (10% 5% 1%)
# Joint Statistic:     	   1.28 1.47 1.88
# Individual Statistic:	   0.35 0.47 0.75
# 
# Sign Bias Test
# ------------------------------------
#                    t-value   prob sig
# Sign Bias           1.2090 0.2273    
# Negative Sign Bias  0.7529 0.4519    
# Positive Sign Bias  0.2116 0.8325    
# Joint Effect        5.6189 0.1317    
# 
# 
# Adjusted Pearson Goodness-of-Fit Test:
# ------------------------------------
#   group statistic p-value(g-1)
# 1    20     19.25      0.44097
# 2    30     39.16      0.09855
# 3    40     45.16      0.23024
# 4    50     70.09      0.02561
# 
# 
# Elapsed time : 0.207762

# Analizzando i p-value del test pesato di Ljung-Box sui residui standardizzati, osserviamo che questi sono tutti maggiori di α = 0.01
# (in realtà anche di α = 0.05 e α = 0.10). Quindi, non abbiamo evidenza statistica sufficiente per rifiutare l’ipotesi nulla di assenza di
# autocorrelazione.

# Applichiamo, ora, dei dest di normalità sui residui.

# Estrai residui standardizzati
z_hat <- residuals(fit, standardize = TRUE)

# Prendi le date del training_set:
# Rimuoviamo le righe con NA nella colonna usata per il modello
valid_dates <- training_set$Date[!is.na(training_set$SPY_LogReturn)]

# Converti in Date se non lo sono già
valid_dates <- as.Date(valid_dates)

# Sovrascrivi l’index di z_hat con le date corrette
index(z_hat) <- valid_dates

# Controlla che le lunghezze coincidano
if(length(z_hat) != length(valid_dates)) {
  stop("Errore: lunghezza residui e date non coincide!")
}

# Visualizza i primi valori
head(z_hat)
#                  [,1]
# 2023-08-01 -0.2715260
# 2023-08-02 -1.4132719
# 2023-08-03 -0.2749317
# 2023-08-04 -0.4619747
# 2023-08-07  0.9301890
# 2023-08-08 -0.4693765

# Test di SW
shapirowilk_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Shapiro-Wilk Test
# Statistic: 0.964475
# p-value  : 2.71107e-09
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Test di JB
jarque_bera_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Jarque-Bera Test
# Statistic: 186.377436
# p-value  : 0
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Come atteso, i residui standardizzati del GARCH(1,1) con distribuzione skewed GED non seguono una distribuzione normale.

# QQ-Plot SGED:
# Residui standardizzati ordinati
y <- sort(as.numeric(z_hat))
n <- length(y)

# Parametri stimati da 'fit'
params <- coef(fit)
SGED_xi <- params["skew"]
SGED_nu <- params["shape"]
mean <- 0
sd <- 1

# Parametri della distribuzione skewed GED
distr <- "sged"
distr_pars <- list(mean = mean, sd = sd, nu = SGED_nu, xi = SGED_xi)

# Quantili teorici
quants <- qsged(ppoints(n), mean = mean, sd = sd, nu = SGED_nu, xi = SGED_xi)

# DataFrame per plotting
QQ_plot_df <- data.frame(
  T = 1:n,
  Q = quants,
  X = y,
  Y = y
)

# Calcolo retta interquartile
quart_probs <- c(0.25, 0.75)
quart_X <- as.vector(quantile(QQ_plot_df$X, quart_probs))
quart_Q <- qsged(quart_probs, mean = mean, sd = sd, nu = SGED_nu, xi = SGED_xi)
slope <- diff(quart_X) / diff(quart_Q)
intercept <- quart_X[1] - slope * quart_Q[1]

# Titoli
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 MPSMF 2024-2025",
                             paste("Q-Q plot of the Standardized Residuals from the GARCH(1,1) model for SPY (skewed GED)")))
subtitle_content <- bquote(paste("Sample size: ", .(n), 
                                 "; SGED parameters → mean = ", .(mean), 
                                 ", sd = ", .(sd), 
                                 ", shape = ", .(round(SGED_nu, 3)), 
                                 ", skew = ", .(round(SGED_xi, 3)), "."))

caption_content <- "Author: Matteo Basili"
x_name <- bquote("Theoretical Quantiles (SGED)")
y_name <- bquote("Standardized Residuals")

# Assi e legende
x_breaks <- seq(from = floor(min(QQ_plot_df$Q)), to = ceiling(max(QQ_plot_df$Q)), by = 0.5)
x_labs <- format(x_breaks, scientific = FALSE)
y_breaks_num <- length(x_breaks)
y_binwidth <- round((max(QQ_plot_df$Y) - min(QQ_plot_df$Y)) / y_breaks_num, digits = 3)
y_breaks <- round(seq(from = floor(min(QQ_plot_df$Y)/y_binwidth)*y_binwidth, 
                      to = ceiling(max(QQ_plot_df$Y)/y_binwidth)*y_binwidth, 
                      by = y_binwidth), 3)
y_labs <- format(y_breaks, scientific = FALSE)

# Legende e colori
leg_shape_labs <- bquote("Q-Q plot")
leg_fill_labs <- c(bquote("90% confidence interval"), bquote("95% confidence interval"))
leg_col_labs <- c(bquote("Interquartile line"), bquote("Regression line"), bquote("y = x"))

Stand_Res_std_QQ_plot <- ggplot(QQ_plot_df) + 
  stat_qq_band(aes(sample = X, fill = "95"), distribution = distr, dparams = distr_pars, conf = 0.95) +
  stat_qq_band(aes(sample = X, fill = "90"), distribution = distr, dparams = distr_pars, conf = 0.90) +
  geom_abline(aes(slope = slope, intercept = intercept, colour = "IQR"), linewidth = 0.8) +
  stat_smooth(aes(x = Q, y = Y, colour = "Reg", group = 1), method = "lm", se = FALSE, linewidth = 0.8) +
  geom_abline(aes(slope = 1, intercept = 0, colour = "45deg"), linewidth = 0.8) +
  stat_qq_point(aes(sample = X), shape = 19, color = "black", size = 1.2, alpha = 0.8, distribution = distr, dparams = distr_pars) +
  scale_x_continuous(name = x_name, breaks = x_breaks, labels = x_labs) +
  scale_y_continuous(name = y_name, breaks = y_breaks, labels = NULL,
                     sec.axis = sec_axis(~ ., breaks = y_breaks, labels = y_labs)) +
  ggtitle(title_content) +
  labs(subtitle = subtitle_content, caption = caption_content) +
  scale_shape_manual(name = "", labels = leg_shape_labs, values = c("19")) +
  scale_fill_manual(name = "", values = c("90" = "chartreuse1", "95" = "deepskyblue1"), labels = leg_fill_labs) +
  scale_colour_manual(name = "", values = c("IQR" = "cyan", "Reg" = "red", "45deg" = "black"), labels = leg_col_labs) +
  guides(shape = guide_legend(order = 1), fill = guide_legend(order = 2), colour = guide_legend(order = 3)) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.x = element_text(angle = 0, vjust = 1),
    legend.key.width = unit(0.8, "cm"),
    legend.position = "bottom"
  )

# Mostra il plot
plot(Stand_Res_std_QQ_plot)

# Calcola densità teorica nei quantili (SGED)
dens <- dsged(quants, mean = mean, sd = sd, nu = SGED_nu, xi = SGED_xi)

# Probabilità associate ai quantili
p <- ppoints(n)

# Errore standard dei quantili teorici
SE <- sqrt(p * (1 - p) / (n * dens^2))

# Limiti di confidenza al 95%
alpha_95 <- 0.05
z_95 <- qnorm(1 - alpha_95 / 2)
lower_95 <- quants - z_95 * SE
upper_95 <- quants + z_95 * SE

# Percentuale di punti dentro la banda 95%
inside_95 <- (QQ_plot_df$X >= lower_95) & (QQ_plot_df$X <= upper_95)
percent_inside_95 <- mean(inside_95) * 100
cat(sprintf("Percentuale di punti dentro la banda di confidenza al 95%%: %.2f%%\n", percent_inside_95))
# Percentuale di punti dentro la banda di confidenza al 95%: 96.84%

# Limiti di confidenza al 90%
alpha_90 <- 0.10
z_90 <- qnorm(1 - alpha_90 / 2)
lower_90 <- quants - z_90 * SE
upper_90 <- quants + z_90 * SE

# Percentuale di punti dentro la banda 90%
inside_90 <- (QQ_plot_df$X >= lower_90) & (QQ_plot_df$X <= upper_90)
percent_inside_90 <- mean(inside_90) * 100
cat(sprintf("Percentuale di punti dentro la banda di confidenza al 90%%: %.2f%%\n", percent_inside_90))
# Percentuale di punti dentro la banda di confidenza al 90%: 83.97%

# Kolmogorov-Smirnov test (K-S test)
ks_result <- ks.test(y, 
                     y = function(q) psged(q, mean=0, sd=1, nu=SGED_nu, xi=SGED_xi),
                     alternative = "two.sided")

print(ks_result)

#         Asymptotic one-sample Kolmogorov-Smirnov test
# 
# data:  y
# D = 0.048248, p-value = 0.2198
# alternative hypothesis: two-sided

# Dall'analisi del Q-Q Plot e del test di K-S vediamo, quindi, che i residui standardizzati seguono la distribuzione skewed GED con un livello
# di significatività del 5% (in realtà il test di K-S risulta anche al 10%).

# Verifichiamo ora la stazionarietà e l'omoschedasticità (non condizionata).

# Grafico dei residui
df_residui <- data.frame(
  Date = index(z_hat),
  Residui = as.numeric(z_hat)
)

ggplot(df_residui, aes(x = Date, y = Residui)) +
  geom_line(color = "steelblue", linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  labs(
    title = "Grafico dei residui standardizzati",
    subtitle = "Modello GARCH(1,1) con distribuzione skewed GED per SPY",
    x = "Data",
    y = "Residui standardizzati"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    axis.title = element_text(size = 11)
  )

# KPSS Test
kpss_test_level(df_residui, col = "Residui", alpha = 0.01)
# 
# [Residui] KPSS Test - Stazionarietà attorno a una media costante
# Statistic: 0.124534
# Valore critico (circa 1%): 0.739000
# Interpretazione del p-value: p-value > 0.10
# 
# ** CONCLUSIONE: Statistic <= valore critico (1%) **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO stazionari.

# Quindi, i residui standardizzati sono stazionari.

# Per l'omoschedasticità non condizionata, per prima cosa, vediamo se c'è curtosi nei residui standardizzati.

# Calcola la curtosi
kurt_z_hat <- kurtosis(as.numeric(z_hat), na.rm = TRUE)

cat(sprintf("Curtosi dei residui standardizzati: %.4f\n", kurt_z_hat))
# Curtosi dei residui standardizzati: 5.8514

# La curtosi c'è. Quindi, applichiamo la forma "studentized" dei test di Breusch-Pagan e White per la stazionarietà in varianza.

# Breusch-Pagan Test:
# Crea un indice temporale (può essere anche un numero progressivo)
df_residui$Index <- seq_along(df_residui$Residui)

# Esegui il test BP
breusch_pagan_test(
  index = df_residui$Index,
  group = df_residui$Residui,
  col = "Residui standardizzati"
)
# [Residui standardizzati] Breusch-Pagan Test
# Statistic: 3.743900
# p-value: 0.0530006
# 
# ** Conclusione: p-value > 0.01 **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON sono eteroschedastici.

# White Test
white_test(
  index = df_residui$Index,
  group = df_residui$Residui,
  col = "Residui standardizzati"
)
# [Residui standardizzati] White Test
# Statistic: 4.426885
# p-value: 0.109324
# 
# ** Conclusione: p-value > 0.01 **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON sono eteroschedastici.

# Dai risultati dei test si vede che è verificata anche la condizione di omoschedasticità (non condizionata).

# Per verificare ulteriormente l'assenza di autocorrelazione per i residui standardizzati
# di questo modello, facciamo anche l'ACF, il PACF e il test di Ljung-Box classico.

# ACF
df_z <- data.frame(z_hat = as.numeric(z_hat))
plot_acf_residuals(df_z, col = "z_hat")

# PACF
plot_pacf_residuals(df_z, col = "z_hat")

# L’analisi visiva di ACF e PACF mostra che, in entrambi i grafici, nessuna barra supera la banda di confidenza al 90%.
# Dunque, non c'è una forte evidenza visiva di autocorrelazione.

# Ljung-Box Test
ljungbox_test(as.numeric(z_hat), col_name = "Residui standardizzati")
# Ljung-Box test per Residui standardizzati (max_lag = 10):
#            lag     lb_stat lb_pvalue
# X-squared    1 0.004363809 0.9473307
# X-squared1   2 0.097436777 0.9524493
# X-squared2   3 0.772063491 0.8561367
# X-squared3   4 1.169615602 0.8830754
# X-squared4   5 1.209442511 0.9439683
# X-squared5   6 1.941025808 0.9250417
# X-squared6   7 2.363331369 0.9370321
# X-squared7   8 2.429503617 0.9649372
# X-squared8   9 2.884003817 0.9687166
# X-squared9  10 2.950207167 0.9825717

# Osservando i p-value associati ai vari lag, vediamo che tutti questi sono maggiori di α = 0.10. Dunque, non abbiamo evidenza statistica
# sufficiente per rifiutare l’ipotesi nulla di assenza di autocorrelazione fino al lag specificato.

# Infine, verifichiamo anche l’assenza di eteroschedasticità condizionata nei residui standardizzati.

# Test ARCH di Engle
arch_effects_test(z_hat, col_name = "Residui Standardizzati")
# ARCH Test per Residui Standardizzati (max_lag = 5):
#            lag arch_stat  arch_pvalue
# statistic    1  1.718505 1.898856e-01
# statistic1   2  3.916283 1.411204e-01
# statistic2   3  5.017682 1.705069e-01
# statistic3   4 31.058912 2.977842e-06
# statistic4   5 31.073899 9.057738e-06

# Alla luce dei risultati, a partire dal lag 4, il test ARCH rivela evidenza significativa di eteroschedasticità residua, con p-value < 1%.
# Questo suggerisce che il modello GARCH(1,1) con distribuzione skewed GED non ha catturato completamente la dinamica della varianza
# condizionata, e che potrebbero essere presenti effetti ARCH residui a orizzonti più lunghi.
# Perciò, possiamo concludere che anche questo modello è da scartare.

# Analizziamo il prossimo nella classifica: GARCH(1,0) sstd
p <- valid_models$p[3]
q <- valid_models$q[3]
dist <- valid_models$dist[3]

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = dist
)

fit <- tryCatch(
  ugarchfit(spec = spec, data = returns, solver = "hybrid"),
  error = function(e) NULL
)

# Risultati
show(fit)
# 
# *---------------------------------*
# *          GARCH Model Fit        *
# *---------------------------------*
#   
# Conditional Variance Dynamics 	
# -----------------------------------
# GARCH Model	    : sGARCH(1,0)
# Mean Model	    : ARFIMA(0,0,0)
# Distribution	  : sstd 
# 
# Optimal Parameters
# ------------------------------------
#         Estimate  Std. Error  t value Pr(>|t|)
# omega    0.88077    0.161422   5.4563  0.00000
# alpha1   0.18002    0.088723   2.0291  0.04245
# skew     0.83637    0.047750  17.5157  0.00000
# shape    3.67749    0.695796   5.2853  0.00000
# 
# Robust Standard Errors:
#         Estimate  Std. Error  t value Pr(>|t|)
# omega    0.88077     0.20097   4.3825 0.000012
# alpha1   0.18002     0.10992   1.6377 0.101474
# skew     0.83637     0.04180  20.0087 0.000000
# shape    3.67749     0.94859   3.8768 0.000106
# 
# LogLikelihood : -609.198 
# 
# Information Criteria
# ------------------------------------
#   
# Akaike       2.5873
# Bayes        2.6224
# Shibata      2.5872
# Hannan-Quinn 2.6011
# 
# Weighted Ljung-Box Test on Standardized Residuals
# ------------------------------------
#                         statistic p-value
# Lag[1]                     0.5933  0.4412
# Lag[2*(p+q)+(p+q)-1][2]    1.3088  0.4081
# Lag[4*(p+q)+(p+q)-1][5]    5.0364  0.1502
# d.o.f=0
# H0 : No serial correlation
# 
# Weighted Ljung-Box Test on Standardized Squared Residuals
# ------------------------------------
#                         statistic   p-value
# Lag[1]                     0.6070 4.359e-01
# Lag[2*(p+q)+(p+q)-1][2]    0.6234 6.384e-01
# Lag[4*(p+q)+(p+q)-1][5]   18.2302 5.924e-05
# d.o.f=1
# 
# Weighted ARCH LM Tests
# ------------------------------------
#             Statistic Shape Scale   P-Value
# ARCH Lag[2]   0.03241 0.500 2.000 8.571e-01
# ARCH Lag[4]  21.44632 1.397 1.611 5.355e-06
# ARCH Lag[6]  28.76508 2.222 1.500 1.657e-07
# 
# Nyblom stability test
# ------------------------------------
# Joint Statistic:  1.4943
# Individual Statistics:              
# omega  0.51851
# alpha1 0.47092
# skew   0.02673
# shape  0.08955
# 
# Asymptotic Critical Values (10% 5% 1%)
# Joint Statistic:     	   1.07 1.24 1.6
# Individual Statistic:	   0.35 0.47 0.75
# 
# Sign Bias Test
# ------------------------------------
#                    t-value   prob sig
# Sign Bias          0.52841 0.5975    
# Negative Sign Bias 1.46993 0.1423    
# Positive Sign Bias 0.06128 0.9512    
# Joint Effect       5.18144 0.1590    
# 
# 
# Adjusted Pearson Goodness-of-Fit Test:
# ------------------------------------
#   group statistic p-value(g-1)
# 1    20     35.96      0.01068
# 2    30     49.42      0.01043
# 3    40     62.20      0.01051
# 4    50     58.70      0.16154
# 
# 
# Elapsed time : 1.713458 

# Analizzando i p-value del test pesato di Ljung-Box sui residui standardizzati, osserviamo che questi sono tutti maggiori di α = 0.01
# (in realtà anche di α = 0.05 e α = 0.10). Quindi, non abbiamo evidenza statistica sufficiente per rifiutare l’ipotesi nulla di assenza di
# autocorrelazione.

# Applichiamo, ora, dei dest di normalità sui residui.

# Estrai residui standardizzati
z_hat <- residuals(fit, standardize = TRUE)

# Prendi le date del training_set:
# Rimuoviamo le righe con NA nella colonna usata per il modello
valid_dates <- training_set$Date[!is.na(training_set$SPY_LogReturn)]

# Converti in Date se non lo sono già
valid_dates <- as.Date(valid_dates)

# Sovrascrivi l’index di z_hat con le date corrette
index(z_hat) <- valid_dates

# Controlla che le lunghezze coincidano
if(length(z_hat) != length(valid_dates)) {
  stop("Errore: lunghezza residui e date non coincide!")
}

# Visualizza i primi valori
head(z_hat)
#                  [,1]
# 2023-08-01 -0.2715260
# 2023-08-02 -1.4802699
# 2023-08-03 -0.2583579
# 2023-08-04 -0.4789766
# 2023-08-07  0.9071467
# 2023-08-08 -0.4322148

# Test di SW
shapirowilk_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Shapiro-Wilk Test
# Statistic: 0.896255
# p-value  : 2.16358e-17
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Test di JB
jarque_bera_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Jarque-Bera Test
# Statistic: 3869.152601
# p-value  : 0
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Come atteso, i residui standardizzati del GARCH(1,0) con distribuzione skewed Student-t non seguono una distribuzione normale.

# QQ-Plot SSTD:
# Residui standardizzati ordinati
y <- sort(as.numeric(z_hat))
n <- length(y)

# Parametri stimati da 'fit'
params <- coef(fit)
SSTD_xi <- params["skew"]
SSTD_nu <- params["shape"]
mean <- 0
sd <- 1

# Parametri della distribuzione skewed Student-t
distr <- "sstd"
distr_pars <- list(mean = mean, sd = sd, nu = SSTD_nu, xi = SSTD_xi)

# Quantili teorici
quants <- qsstd(ppoints(n), mean = mean, sd = sd, nu = SSTD_nu, xi = SSTD_xi)

# DataFrame per plotting
QQ_plot_df <- data.frame(
  T = 1:n,
  Q = quants,
  X = y,
  Y = y
)

# Calcolo retta interquartile
quart_probs <- c(0.25, 0.75)
quart_X <- as.vector(quantile(QQ_plot_df$X, quart_probs))
quart_Q <- qsstd(quart_probs, mean = mean, sd = sd, nu = SSTD_nu, xi = SSTD_xi)
slope <- diff(quart_X) / diff(quart_Q)
intercept <- quart_X[1] - slope * quart_Q[1]

# Titoli
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 MPSMF 2024-2025",
                             paste("Q-Q plot of the Standardized Residuals from the GARCH(1,0) model for SPY (skewed Student-t)")))
subtitle_content <- bquote(paste("Sample size: ", .(n), 
                                 "; SSTD parameters → mean = ", .(mean), 
                                 ", sd = ", .(sd), 
                                 ", df = ", .(round(SSTD_nu, 3)), 
                                 ", skew = ", .(round(SSTD_xi, 3)), "."))

caption_content <- "Author: Matteo Basili"
x_name <- bquote("Theoretical Quantiles (SSTD)")
y_name <- bquote("Standardized Residuals")

# Assi e legende
x_breaks <- seq(from = floor(min(QQ_plot_df$Q)), to = ceiling(max(QQ_plot_df$Q)), by = 0.5)
x_labs <- format(x_breaks, scientific = FALSE)
y_breaks_num <- length(x_breaks)
y_binwidth <- round((max(QQ_plot_df$Y) - min(QQ_plot_df$Y)) / y_breaks_num, digits = 3)
y_breaks <- round(seq(from = floor(min(QQ_plot_df$Y)/y_binwidth)*y_binwidth, 
                      to = ceiling(max(QQ_plot_df$Y)/y_binwidth)*y_binwidth, 
                      by = y_binwidth), 3)
y_labs <- format(y_breaks, scientific = FALSE)

# Legende e colori
leg_shape_labs <- bquote("Q-Q plot")
leg_fill_labs <- c(bquote("90% confidence interval"), bquote("95% confidence interval"))
leg_col_labs <- c(bquote("Interquartile line"), bquote("Regression line"), bquote("y = x"))

Stand_Res_std_QQ_plot <- ggplot(QQ_plot_df) +
  stat_qq_band(aes(sample = X, fill = "95"), distribution = distr, dparams = distr_pars, conf = 0.95) +
  stat_qq_band(aes(sample = X, fill = "90"), distribution = distr, dparams = distr_pars, conf = 0.90) +
  geom_abline(aes(slope = slope, intercept = intercept, colour = "IQR"), linewidth = 0.8) +
  stat_smooth(aes(x = Q, y = Y, colour = "Reg", group = 1), method = "lm", se = FALSE, linewidth = 0.8) +
  geom_abline(aes(slope = 1, intercept = 0, colour = "45deg"), linewidth = 0.8) +
  stat_qq_point(aes(sample = X), shape = 19, color = "black", size = 1.2, alpha = 0.8, distribution = distr, dparams = distr_pars) +
  scale_x_continuous(name = x_name, breaks = x_breaks, labels = x_labs) +
  scale_y_continuous(name = y_name, breaks = y_breaks, labels = NULL,
                     sec.axis = sec_axis(~ ., breaks = y_breaks, labels = y_labs)) +
  ggtitle(title_content) +
  labs(subtitle = subtitle_content, caption = caption_content) +
  scale_shape_manual(name = "", labels = leg_shape_labs, values = c("19")) +
  scale_fill_manual(name = "", values = c("90" = "chartreuse1", "95" = "deepskyblue1"), labels = leg_fill_labs) +
  scale_colour_manual(name = "", values = c("IQR" = "cyan", "Reg" = "red", "45deg" = "black"), labels = leg_col_labs) +
  guides(shape = guide_legend(order = 1), fill = guide_legend(order = 2), colour = guide_legend(order = 3)) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.x = element_text(angle = 0, vjust = 1),
    legend.key.width = unit(0.8, "cm"),
    legend.position = "bottom"
  )

# Mostra il plot
plot(Stand_Res_std_QQ_plot)

# Calcola densità teorica nei quantili (pdf)
dens <- dsstd(quants, mean=mean, sd=sd, nu=SSTD_nu, xi=SSTD_xi)

# Calcola errore standard dei quantili teorici (formula banda Q-Q)
# Errore standard: SE = (p * (1-p)) / (n * f(Q)^2)
p <- ppoints(n)
SE <- sqrt(p * (1-p) / (n * dens^2))

# Limiti di confidenza (95%)
alpha <- 0.05
z <- qnorm(1 - alpha/2)

lower_95 <- quants - z * SE
upper_95 <- quants + z * SE

# Verifica quanti punti osservati cadono dentro le bande
# Ricorda che QQ_plot_df$X = dati ordinati osservati

inside_95 <- (QQ_plot_df$X >= lower_95) & (QQ_plot_df$X <= upper_95)

percent_inside_95 <- mean(inside_95) * 100

cat(sprintf("Percentuale di punti dentro la banda di confidenza al 95%%: %.2f%%\n", percent_inside_95))
# Percentuale di punti dentro la banda di confidenza al 95%: 98.52%

# Alpha per il 90%
alpha_90 <- 0.10
z_90 <- qnorm(1 - alpha_90/2)  # quantile normale per il 90%

# Limiti di confidenza al 90%
lower_90 <- quants - z_90 * SE
upper_90 <- quants + z_90 * SE

# Verifica quanti punti osservati cadono dentro la banda 90%
inside_90 <- (QQ_plot_df$X >= lower_90) & (QQ_plot_df$X <= upper_90)

percent_inside_90 <- mean(inside_90) * 100

cat(sprintf("Percentuale di punti dentro la banda di confidenza al 90%%: %.2f%%\n", percent_inside_90))
# Percentuale di punti dentro la banda di confidenza al 90%: 92.19%

# Kolmogorov-Smirnov test (K-S test)
ks_result <- ks.test(y, 
                     y = function(q) psstd(q, mean=0, sd=1, nu=SSTD_nu, xi=SSTD_xi),
                     alternative = "two.sided")

print(ks_result)
# 
#         Asymptotic one-sample Kolmogorov-Smirnov test
# 
# data:  y
# D = 0.045013, p-value = 0.2921
# alternative hypothesis: two-sided

# Dall'analisi del Q-Q Plot e del test di K-S vediamo, quindi, che i residui standardizzati seguono la distribuzione skewed Student-t con un livello
# di significatività del 10%.

# Verifichiamo ora la stazionarietà e l'omoschedasticità (non condizionata).

# Grafico dei residui
df_residui <- data.frame(
  Date = index(z_hat),
  Residui = as.numeric(z_hat)
)

ggplot(df_residui, aes(x = Date, y = Residui)) +
  geom_line(color = "steelblue", linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  labs(
    title = "Grafico dei residui standardizzati",
    subtitle = "Modello GARCH(1,0) con distribuzione skewed Student-t per SPY",
    x = "Data",
    y = "Residui standardizzati"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    axis.title = element_text(size = 11)
  )

# KPSS Test
kpss_test_level(df_residui, col = "Residui", alpha = 0.01)
# 
# [Residui] KPSS Test - Stazionarietà attorno a una media costante
# Statistic: 0.063325
# Valore critico (circa 1%): 0.739000
# Interpretazione del p-value: p-value > 0.10
# 
# ** CONCLUSIONE: Statistic <= valore critico (1%) **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO stazionari.

# Quindi, i residui standardizzati sono stazionari.

# Per l'omoschedasticità non condizionata, per prima cosa, vediamo se c'è curtosi nei residui standardizzati.

# Calcola la curtosi
kurt_z_hat <- kurtosis(as.numeric(z_hat), na.rm = TRUE)

cat(sprintf("Curtosi dei residui standardizzati: %.4f\n", kurt_z_hat))
# Curtosi dei residui standardizzati: 16.8930

# La curtosi c'è. Quindi, applichiamo la forma "studentized" dei test di Breusch-Pagan e White per la stazionarietà in varianza.

# Breusch-Pagan Test:
# Crea un indice temporale (può essere anche un numero progressivo)
df_residui$Index <- seq_along(df_residui$Residui)

# Esegui il test BP
breusch_pagan_test(
  index = df_residui$Index,
  group = df_residui$Residui,
  col = "Residui standardizzati"
)
# [Residui standardizzati] Breusch-Pagan Test
# Statistic: 7.429219
# p-value: 0.00641732
# 
# ** Conclusione: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO eteroschedastici.

# White Test
white_test(
  index = df_residui$Index,
  group = df_residui$Residui,
  col = "Residui standardizzati"
)
# [Residui standardizzati] White Test
# Statistic: 10.325220
# p-value: 0.00572673
# 
# ** Conclusione: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO eteroschedastici.

# Dai risultati dei test possiamo rigettare l'ipotesi nulla di omoschedasticità incondizionata con un livello di significatività dell'1%. Quindi,
# c'è evidenza statistica di eteroschedasticità non condizionata.
# Perciò, possiamo concludere che anche questo modello è da scartare.

# Analizziamo il prossimo nella classifica: GARCH(2,1) sstd
p <- valid_models$p[4]
q <- valid_models$q[4]
dist <- valid_models$dist[4]

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = dist
)

fit <- tryCatch(
  ugarchfit(spec = spec, data = returns, solver = "hybrid"),
  error = function(e) NULL
)

# Risultati
show(fit)
# 
# *---------------------------------*
# *          GARCH Model Fit        *
# *---------------------------------*
#   
# Conditional Variance Dynamics 	
# -----------------------------------
# GARCH Model	    : sGARCH(2,1)
# Mean Model	    : ARFIMA(0,0,0)
# Distribution	  : sstd 
# 
# Optimal Parameters
# ------------------------------------
#         Estimate  Std. Error  t value Pr(>|t|)
# omega   0.089236    0.039112  2.28152 0.022518
# alpha1  0.074277    0.058410  1.27165 0.203497
# alpha2  0.045566    0.070128  0.64976 0.515847
# beta1   0.790935    0.062380 12.67934 0.000000
# skew    0.823081    0.050017 16.45610 0.000000
# shape   4.808402    1.140284  4.21684 0.000025
# 
# Robust Standard Errors:
#         Estimate  Std. Error  t value Pr(>|t|)
# omega   0.089236    0.028648  3.11495 0.001840
# alpha1  0.074277    0.059761  1.24290 0.213905
# alpha2  0.045566    0.057659  0.79027 0.429369
# beta1   0.790935    0.041904 18.87481 0.000000
# skew    0.823081    0.044266 18.59392 0.000000
# shape   4.808402    1.438651  3.34230 0.000831
# 
# LogLikelihood : -597.7414 
# 
# Information Criteria
# ------------------------------------
#   
# Akaike       2.5474
# Bayes        2.6001
# Shibata      2.5471
# Hannan-Quinn 2.5681
# 
# Weighted Ljung-Box Test on Standardized Residuals
# ------------------------------------
#                         statistic p-value
# Lag[1]                    0.04076  0.8400
# Lag[2*(p+q)+(p+q)-1][2]   0.12193  0.9045
# Lag[4*(p+q)+(p+q)-1][5]   0.88149  0.8859
# d.o.f=0
# H0 : No serial correlation
# 
# Weighted Ljung-Box Test on Standardized Squared Residuals
# ------------------------------------
#                          statistic   p-value
# Lag[1]                       5.667 1.728e-02
# Lag[2*(p+q)+(p+q)-1][8]     28.161 7.347e-07
# Lag[4*(p+q)+(p+q)-1][14]    34.226 6.678e-07
# d.o.f=3
# 
# Weighted ARCH LM Tests
# ------------------------------------
#             Statistic Shape Scale   P-Value
# ARCH Lag[4]     29.39 0.500 2.000 5.916e-08
# ARCH Lag[6]     29.51 1.461 1.711 1.394e-07
# ARCH Lag[8]     29.95 2.368 1.583 3.004e-07
# 
# Nyblom stability test
# ------------------------------------
# Joint Statistic:  1.2512
# Individual Statistics:              
# omega  0.04153
# alpha1 0.07002
# alpha2 0.05070
# beta1  0.03254
# skew   0.05876
# shape  0.30013
# 
# Asymptotic Critical Values (10% 5% 1%)
# Joint Statistic:     	   1.49 1.68 2.12
# Individual Statistic:	   0.35 0.47 0.75
# 
# Sign Bias Test
# ------------------------------------
#                    t-value    prob sig
# Sign Bias          0.90213 0.36745    
# Negative Sign Bias 1.66602 0.09638   *
# Positive Sign Bias 0.08645 0.93115    
# Joint Effect       8.00760 0.04585  **
#   
#   
# Adjusted Pearson Goodness-of-Fit Test:
# ------------------------------------
#   group statistic p-value(g-1)
# 1    20     20.18      0.38399
# 2    30     32.84      0.28443
# 3    40     37.73      0.52775
# 4    50     67.56      0.04047
# 
# 
# Elapsed time : 0.2402289

# Analizzando i p-value del test pesato di Ljung-Box sui residui standardizzati, osserviamo che questi sono tutti maggiori di α = 0.01
# (in realtà anche di α = 0.05 e α = 0.10). Quindi, non abbiamo evidenza statistica sufficiente per rifiutare l’ipotesi nulla di assenza di
# autocorrelazione.

# Applichiamo, ora, dei dest di normalità sui residui.

# Estrai residui standardizzati
z_hat <- residuals(fit, standardize = TRUE)

# Prendi le date del training_set:
# Rimuoviamo le righe con NA nella colonna usata per il modello
valid_dates <- training_set$Date[!is.na(training_set$SPY_LogReturn)]

# Converti in Date se non lo sono già
valid_dates <- as.Date(valid_dates)

# Sovrascrivi l’index di z_hat con le date corrette
index(z_hat) <- valid_dates

# Controlla che le lunghezze coincidano
if(length(z_hat) != length(valid_dates)) {
  stop("Errore: lunghezza residui e date non coincide!")
}

# Visualizza i primi valori
head(z_hat)
#                  [,1]
# 2023-08-01 -0.2715260
# 2023-08-02 -1.3272829
# 2023-08-03 -0.2712218
# 2023-08-04 -0.4381198
# 2023-08-07  0.8893364
# 2023-08-08 -0.4568722

# Test di SW
shapirowilk_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Shapiro-Wilk Test
# Statistic: 0.962236
# p-value  : 1.11409e-09
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Test di JB
jarque_bera_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Jarque-Bera Test
# Statistic: 205.575297
# p-value  : 0
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Come atteso, i residui standardizzati del GARCH(2,1) con distribuzione skewed Student-t non seguono una distribuzione normale.

# QQ-Plot SSTD:
# Residui standardizzati ordinati
y <- sort(as.numeric(z_hat))
n <- length(y)

# Parametri stimati da 'fit'
params <- coef(fit)
SSTD_xi <- params["skew"]
SSTD_nu <- params["shape"]
mean <- 0
sd <- 1

# Parametri della distribuzione skewed Student-t
distr <- "sstd"
distr_pars <- list(mean = mean, sd = sd, nu = SSTD_nu, xi = SSTD_xi)

# Quantili teorici
quants <- qsstd(ppoints(n), mean = mean, sd = sd, nu = SSTD_nu, xi = SSTD_xi)

# DataFrame per plotting
QQ_plot_df <- data.frame(
  T = 1:n,
  Q = quants,
  X = y,
  Y = y
)

# Calcolo retta interquartile
quart_probs <- c(0.25, 0.75)
quart_X <- as.vector(quantile(QQ_plot_df$X, quart_probs))
quart_Q <- qsstd(quart_probs, mean = mean, sd = sd, nu = SSTD_nu, xi = SSTD_xi)
slope <- diff(quart_X) / diff(quart_Q)
intercept <- quart_X[1] - slope * quart_Q[1]

# Titoli
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 MPSMF 2024-2025",
                             paste("Q-Q plot of the Standardized Residuals from the GARCH(2,1) model for SPY (skewed Student-t)")))
subtitle_content <- bquote(paste("Sample size: ", .(n), 
                                 "; SSTD parameters → mean = ", .(mean), 
                                 ", sd = ", .(sd), 
                                 ", df = ", .(round(SSTD_nu, 3)), 
                                 ", skew = ", .(round(SSTD_xi, 3)), "."))

caption_content <- "Author: Matteo Basili"
x_name <- bquote("Theoretical Quantiles (SSTD)")
y_name <- bquote("Standardized Residuals")

# Assi e legende
x_breaks <- seq(from = floor(min(QQ_plot_df$Q)), to = ceiling(max(QQ_plot_df$Q)), by = 0.5)
x_labs <- format(x_breaks, scientific = FALSE)
y_breaks_num <- length(x_breaks)
y_binwidth <- round((max(QQ_plot_df$Y) - min(QQ_plot_df$Y)) / y_breaks_num, digits = 3)
y_breaks <- round(seq(from = floor(min(QQ_plot_df$Y)/y_binwidth)*y_binwidth, 
                      to = ceiling(max(QQ_plot_df$Y)/y_binwidth)*y_binwidth, 
                      by = y_binwidth), 3)
y_labs <- format(y_breaks, scientific = FALSE)

# Legende e colori
leg_shape_labs <- bquote("Q-Q plot")
leg_fill_labs <- c(bquote("90% confidence interval"), bquote("95% confidence interval"))
leg_col_labs <- c(bquote("Interquartile line"), bquote("Regression line"), bquote("y = x"))

Stand_Res_std_QQ_plot <- ggplot(QQ_plot_df) +
  stat_qq_band(aes(sample = X, fill = "95"), distribution = distr, dparams = distr_pars, conf = 0.95) +
  stat_qq_band(aes(sample = X, fill = "90"), distribution = distr, dparams = distr_pars, conf = 0.90) +
  geom_abline(aes(slope = slope, intercept = intercept, colour = "IQR"), linewidth = 0.8) +
  stat_smooth(aes(x = Q, y = Y, colour = "Reg", group = 1), method = "lm", se = FALSE, linewidth = 0.8) +
  geom_abline(aes(slope = 1, intercept = 0, colour = "45deg"), linewidth = 0.8) +
  stat_qq_point(aes(sample = X), shape = 19, color = "black", size = 1.2, alpha = 0.8, distribution = distr, dparams = distr_pars) +
  scale_x_continuous(name = x_name, breaks = x_breaks, labels = x_labs) +
  scale_y_continuous(name = y_name, breaks = y_breaks, labels = NULL,
                     sec.axis = sec_axis(~ ., breaks = y_breaks, labels = y_labs)) +
  ggtitle(title_content) +
  labs(subtitle = subtitle_content, caption = caption_content) +
  scale_shape_manual(name = "", labels = leg_shape_labs, values = c("19")) +
  scale_fill_manual(name = "", values = c("90" = "chartreuse1", "95" = "deepskyblue1"), labels = leg_fill_labs) +
  scale_colour_manual(name = "", values = c("IQR" = "cyan", "Reg" = "red", "45deg" = "black"), labels = leg_col_labs) +
  guides(shape = guide_legend(order = 1), fill = guide_legend(order = 2), colour = guide_legend(order = 3)) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.x = element_text(angle = 0, vjust = 1),
    legend.key.width = unit(0.8, "cm"),
    legend.position = "bottom"
  )

# Mostra il plot
plot(Stand_Res_std_QQ_plot)

# Calcola densità teorica nei quantili (pdf)
dens <- dsstd(quants, mean=mean, sd=sd, nu=SSTD_nu, xi=SSTD_xi)

# Calcola errore standard dei quantili teorici (formula banda Q-Q)
# Errore standard: SE = (p * (1-p)) / (n * f(Q)^2)
p <- ppoints(n)
SE <- sqrt(p * (1-p) / (n * dens^2))

# Limiti di confidenza (95%)
alpha <- 0.05
z <- qnorm(1 - alpha/2)

lower_95 <- quants - z * SE
upper_95 <- quants + z * SE

# Verifica quanti punti osservati cadono dentro le bande
# Ricorda che QQ_plot_df$X = dati ordinati osservati

inside_95 <- (QQ_plot_df$X >= lower_95) & (QQ_plot_df$X <= upper_95)

percent_inside_95 <- mean(inside_95) * 100

cat(sprintf("Percentuale di punti dentro la banda di confidenza al 95%%: %.2f%%\n", percent_inside_95))
# Percentuale di punti dentro la banda di confidenza al 95%: 98.10%

# Alpha per il 90%
alpha_90 <- 0.10
z_90 <- qnorm(1 - alpha_90/2)  # quantile normale per il 90%

# Limiti di confidenza al 90%
lower_90 <- quants - z_90 * SE
upper_90 <- quants + z_90 * SE

# Verifica quanti punti osservati cadono dentro la banda 90%
inside_90 <- (QQ_plot_df$X >= lower_90) & (QQ_plot_df$X <= upper_90)

percent_inside_90 <- mean(inside_90) * 100

cat(sprintf("Percentuale di punti dentro la banda di confidenza al 90%%: %.2f%%\n", percent_inside_90))
# Percentuale di punti dentro la banda di confidenza al 90%: 88.19%

# Kolmogorov-Smirnov test (K-S test)
ks_result <- ks.test(y, 
                     y = function(q) psstd(q, mean=0, sd=1, nu=SSTD_nu, xi=SSTD_xi),
                     alternative = "two.sided")

print(ks_result)
# 
#         Asymptotic one-sample Kolmogorov-Smirnov test
# 
# data:  y
# D = 0.051286, p-value = 0.1652
# alternative hypothesis: two-sided

# Dall'analisi del Q-Q Plot e del test di K-S vediamo, quindi, che i residui standardizzati seguono la distribuzione skewed Student-t con un livello
# di significatività del 5% (in realtà il test di K-S risulta anche al 10%).

# Verifichiamo ora la stazionarietà e l'omoschedasticità (non condizionata).

# Grafico dei residui
df_residui <- data.frame(
  Date = index(z_hat),
  Residui = as.numeric(z_hat)
)

ggplot(df_residui, aes(x = Date, y = Residui)) +
  geom_line(color = "steelblue", linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  labs(
    title = "Grafico dei residui standardizzati",
    subtitle = "Modello GARCH(2,1) con distribuzione skewed Student-t per SPY",
    x = "Data",
    y = "Residui standardizzati"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    axis.title = element_text(size = 11)
  )

# KPSS Test
kpss_test_level(df_residui, col = "Residui", alpha = 0.01)
# 
# [Residui] KPSS Test - Stazionarietà attorno a una media costante
# Statistic: 0.119361
# Valore critico (circa 1%): 0.739000
# Interpretazione del p-value: p-value > 0.10
# 
# ** CONCLUSIONE: Statistic <= valore critico (1%) **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO stazionari.

# Quindi, i residui standardizzati sono stazionari.

# Per l'omoschedasticità non condizionata, per prima cosa, vediamo se c'è curtosi nei residui standardizzati.

# Calcola la curtosi
kurt_z_hat <- kurtosis(as.numeric(z_hat), na.rm = TRUE)

cat(sprintf("Curtosi dei residui standardizzati: %.4f\n", kurt_z_hat))
# Curtosi dei residui standardizzati: 6.0162

# La curtosi c'è. Quindi, applichiamo la forma "studentized" dei test di Breusch-Pagan e White per la stazionarietà in varianza.

# Breusch-Pagan Test:
# Crea un indice temporale (può essere anche un numero progressivo)
df_residui$Index <- seq_along(df_residui$Residui)

# Esegui il test BP
breusch_pagan_test(
  index = df_residui$Index,
  group = df_residui$Residui,
  col = "Residui standardizzati"
)
# [Residui standardizzati] Breusch-Pagan Test
# Statistic: 4.914806
# p-value: 0.0266274
# 
# ** Conclusione: p-value > 0.01 **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON sono eteroschedastici.

# White Test
white_test(
  index = df_residui$Index,
  group = df_residui$Residui,
  col = "Residui standardizzati"
)
# [Residui standardizzati] White Test
# Statistic: 6.088802
# p-value: 0.0476248
# 
# ** Conclusione: p-value > 0.01 **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON sono eteroschedastici.

# Dai risultati dei test si vede che è verificata anche la condizione di omoschedasticità (non condizionata).

# Per verificare ulteriormente l'assenza di autocorrelazione per i residui standardizzati
# di questo modello, facciamo anche l'ACF, il PACF e il test di Ljung-Box classico.

# ACF
df_z <- data.frame(z_hat = as.numeric(z_hat))
plot_acf_residuals(df_z, col = "z_hat")

# PACF
plot_pacf_residuals(df_z, col = "z_hat")

# L’analisi visiva di ACF e PACF mostra che, in entrambi i grafici, nessuna barra supera la banda di confidenza al 90%.
# Dunque, non c'è una forte evidenza visiva di autocorrelazione.

# Ljung-Box Test
ljungbox_test(as.numeric(z_hat), col_name = "Residui standardizzati")
# Ljung-Box test per Residui standardizzati (max_lag = 10):
#            lag    lb_stat lb_pvalue
# X-squared    1 0.04075727 0.8400072
# X-squared1   2 0.20309578 0.9034379
# X-squared2   3 1.10828250 0.7750750
# X-squared3   4 1.52729995 0.8217949
# X-squared4   5 1.52803559 0.9098120
# X-squared5   6 2.23988349 0.8963678
# X-squared6   7 2.58567467 0.9205087
# X-squared7   8 2.64557244 0.9545968
# X-squared8   9 3.21191601 0.9552970
# X-squared9  10 3.23055025 0.9754659

# Osservando i p-value associati ai vari lag, vediamo che tutti questi sono maggiori di α = 0.10. Dunque, non abbiamo evidenza statistica
# sufficiente per rifiutare l’ipotesi nulla di assenza di autocorrelazione fino al lag specificato.

# Infine, verifichiamo anche l’assenza di eteroschedasticità condizionata nei residui standardizzati.

# Test ARCH di Engle
arch_effects_test(z_hat, col_name = "Residui Standardizzati")
# ARCH Test per Residui Standardizzati (max_lag = 5):
#            lag arch_stat  arch_pvalue
# statistic    1  6.551452 1.047986e-02
# statistic1   2  9.512163 8.599238e-03
# statistic2   3 13.354677 3.929126e-03
# statistic3   4 37.313499 1.552346e-07
# statistic4   5 37.637109 4.462574e-07

# Alla luce dei risultati, a partire dal lag 2, il test ARCH rivela evidenza significativa di eteroschedasticità residua, con p-value < 1%.
# Questo suggerisce che il modello GARCH(2,1) con distribuzione skewed Student-t non ha catturato completamente la dinamica della varianza
# condizionata, e che potrebbero essere presenti effetti ARCH residui a orizzonti più lunghi.
# Perciò, possiamo concludere che anche questo modello è da scartare.

# Analizziamo il prossimo nella classifica: GARCH(1,2) sstd
p <- valid_models$p[5]
q <- valid_models$q[5]
dist <- valid_models$dist[5]

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = dist
)

fit <- tryCatch(
  ugarchfit(spec = spec, data = returns, solver = "hybrid"),
  error = function(e) NULL
)

# Risultati
show(fit)
# 
# *---------------------------------*
# *          GARCH Model Fit        *
# *---------------------------------*
#   
# Conditional Variance Dynamics 	
# -----------------------------------
# GARCH Model	    : sGARCH(1,2)
# Mean Model	    : ARFIMA(0,0,0)
# Distribution	  : sstd 
# 
# Optimal Parameters
# ------------------------------------
#         Estimate  Std. Error   t value Pr(>|t|)
# omega   0.082262    0.021979  3.742813 0.000182
# alpha1  0.107310    0.021306  5.036581 0.000000
# beta1   0.809506    0.410314  1.972896 0.048507
# beta2   0.000000    0.353102  0.000001 0.999999
# skew    0.825262    0.049533 16.660938 0.000000
# shape   4.798251    1.138592  4.214199 0.000025
# 
# Robust Standard Errors:
#         Estimate  Std. Error   t value Pr(>|t|)
# omega   0.082262    0.028978  2.838737 0.004529
# alpha1  0.107310    0.041916  2.560131 0.010463
# beta1   0.809506    0.211507  3.827319 0.000130
# beta2   0.000000    0.179196  0.000002 0.999998
# skew    0.825262    0.043347 19.038562 0.000000
# shape   4.798251    1.426213  3.364330 0.000767
# 
# LogLikelihood : -597.9405 
# 
# Information Criteria
# ------------------------------------
#   
# Akaike       2.5483
# Bayes        2.6009
# Shibata      2.5480
# Hannan-Quinn 2.5690
# 
# Weighted Ljung-Box Test on Standardized Residuals
# ------------------------------------
#                         statistic p-value
# Lag[1]                   0.008509  0.9265
# Lag[2*(p+q)+(p+q)-1][2]  0.090130  0.9266
# Lag[4*(p+q)+(p+q)-1][5]  0.855951  0.8913
# d.o.f=0
# H0 : No serial correlation
# 
# Weighted Ljung-Box Test on Standardized Squared Residuals
# ------------------------------------
#                          statistic   p-value
# Lag[1]                       2.331 1.268e-01
# Lag[2*(p+q)+(p+q)-1][8]     27.998 8.146e-07
# Lag[4*(p+q)+(p+q)-1][14]    35.107 3.928e-07
# d.o.f=3
# 
# Weighted ARCH LM Tests
# ------------------------------------
#             Statistic Shape Scale   P-Value
# ARCH Lag[4]     35.77 0.500 2.000 2.218e-09
# ARCH Lag[6]     35.83 1.461 1.711 3.770e-09
# ARCH Lag[8]     36.41 2.368 1.583 6.537e-09
# 
# Nyblom stability test
# ------------------------------------
# Joint Statistic:  1.3543
# Individual Statistics:              
# omega  0.04246
# alpha1 0.06245
# beta1  0.03300
# beta2  0.03153
# skew   0.05469
# shape  0.28314
# 
# Asymptotic Critical Values (10% 5% 1%)
# Joint Statistic:     	   1.49 1.68 2.12
# Individual Statistic:	   0.35 0.47 0.75
# 
# Sign Bias Test
# ------------------------------------
#                    t-value   prob sig
# Sign Bias          1.05986 0.2898    
# Negative Sign Bias 1.10898 0.2680    
# Positive Sign Bias 0.09199 0.9267    
# Joint Effect       6.14588 0.1047    
# 
# 
# Adjusted Pearson Goodness-of-Fit Test:
# ------------------------------------
#   group statistic p-value(g-1)
# 1    20     18.24      0.50671
# 2    30     33.72      0.24960
# 3    40     36.21      0.59779
# 4    50     69.46      0.02878
# 
# 
# Elapsed time : 0.2134051

# Analizzando i p-value del test pesato di Ljung-Box sui residui standardizzati, osserviamo che questi sono tutti maggiori di α = 0.01
# (in realtà anche di α = 0.05 e α = 0.10). Quindi, non abbiamo evidenza statistica sufficiente per rifiutare l’ipotesi nulla di assenza di
# autocorrelazione.

# Applichiamo, ora, dei dest di normalità sui residui.

# Estrai residui standardizzati
z_hat <- residuals(fit, standardize = TRUE)

# Prendi le date del training_set:
# Rimuoviamo le righe con NA nella colonna usata per il modello
valid_dates <- training_set$Date[!is.na(training_set$SPY_LogReturn)]

# Converti in Date se non lo sono già
valid_dates <- as.Date(valid_dates)

# Sovrascrivi l’index di z_hat con le date corrette
index(z_hat) <- valid_dates

# Controlla che le lunghezze coincidano
if(length(z_hat) != length(valid_dates)) {
  stop("Errore: lunghezza residui e date non coincide!")
}

# Visualizza i primi valori
head(z_hat)
#                  [,1]
# 2023-08-01 -0.2715260
# 2023-08-02 -1.3272829
# 2023-08-03 -0.2625936
# 2023-08-04 -0.4406645
# 2023-08-07  0.8865732
# 2023-08-08 -0.4492422

# Test di SW
shapirowilk_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Shapiro-Wilk Test
# Statistic: 0.962790
# p-value  : 1.38353e-09
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Test di JB
jarque_bera_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Jarque-Bera Test
# Statistic: 208.952928
# p-value  : 0
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Come atteso, i residui standardizzati del GARCH(1,2) con distribuzione skewed Student-t non seguono una distribuzione normale.

# QQ-Plot SSTD:
# Residui standardizzati ordinati
y <- sort(as.numeric(z_hat))
n <- length(y)

# Parametri stimati da 'fit'
params <- coef(fit)
SSTD_xi <- params["skew"]
SSTD_nu <- params["shape"]
mean <- 0
sd <- 1

# Parametri della distribuzione skewed Student-t
distr <- "sstd"
distr_pars <- list(mean = mean, sd = sd, nu = SSTD_nu, xi = SSTD_xi)

# Quantili teorici
quants <- qsstd(ppoints(n), mean = mean, sd = sd, nu = SSTD_nu, xi = SSTD_xi)

# DataFrame per plotting
QQ_plot_df <- data.frame(
  T = 1:n,
  Q = quants,
  X = y,
  Y = y
)

# Calcolo retta interquartile
quart_probs <- c(0.25, 0.75)
quart_X <- as.vector(quantile(QQ_plot_df$X, quart_probs))
quart_Q <- qsstd(quart_probs, mean = mean, sd = sd, nu = SSTD_nu, xi = SSTD_xi)
slope <- diff(quart_X) / diff(quart_Q)
intercept <- quart_X[1] - slope * quart_Q[1]

# Titoli
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 MPSMF 2024-2025",
                             paste("Q-Q plot of the Standardized Residuals from the GARCH(1,2) model for SPY (skewed Student-t)")))
subtitle_content <- bquote(paste("Sample size: ", .(n), 
                                 "; SSTD parameters → mean = ", .(mean), 
                                 ", sd = ", .(sd), 
                                 ", df = ", .(round(SSTD_nu, 3)), 
                                 ", skew = ", .(round(SSTD_xi, 3)), "."))

caption_content <- "Author: Matteo Basili"
x_name <- bquote("Theoretical Quantiles (SSTD)")
y_name <- bquote("Standardized Residuals")

# Assi e legende
x_breaks <- seq(from = floor(min(QQ_plot_df$Q)), to = ceiling(max(QQ_plot_df$Q)), by = 0.5)
x_labs <- format(x_breaks, scientific = FALSE)
y_breaks_num <- length(x_breaks)
y_binwidth <- round((max(QQ_plot_df$Y) - min(QQ_plot_df$Y)) / y_breaks_num, digits = 3)
y_breaks <- round(seq(from = floor(min(QQ_plot_df$Y)/y_binwidth)*y_binwidth, 
                      to = ceiling(max(QQ_plot_df$Y)/y_binwidth)*y_binwidth, 
                      by = y_binwidth), 3)
y_labs <- format(y_breaks, scientific = FALSE)

# Legende e colori
leg_shape_labs <- bquote("Q-Q plot")
leg_fill_labs <- c(bquote("90% confidence interval"), bquote("95% confidence interval"))
leg_col_labs <- c(bquote("Interquartile line"), bquote("Regression line"), bquote("y = x"))

Stand_Res_std_QQ_plot <- ggplot(QQ_plot_df) +
  stat_qq_band(aes(sample = X, fill = "95"), distribution = distr, dparams = distr_pars, conf = 0.95) +
  stat_qq_band(aes(sample = X, fill = "90"), distribution = distr, dparams = distr_pars, conf = 0.90) +
  geom_abline(aes(slope = slope, intercept = intercept, colour = "IQR"), linewidth = 0.8) +
  stat_smooth(aes(x = Q, y = Y, colour = "Reg", group = 1), method = "lm", se = FALSE, linewidth = 0.8) +
  geom_abline(aes(slope = 1, intercept = 0, colour = "45deg"), linewidth = 0.8) +
  stat_qq_point(aes(sample = X), shape = 19, color = "black", size = 1.2, alpha = 0.8, distribution = distr, dparams = distr_pars) +
  scale_x_continuous(name = x_name, breaks = x_breaks, labels = x_labs) +
  scale_y_continuous(name = y_name, breaks = y_breaks, labels = NULL,
                     sec.axis = sec_axis(~ ., breaks = y_breaks, labels = y_labs)) +
  ggtitle(title_content) +
  labs(subtitle = subtitle_content, caption = caption_content) +
  scale_shape_manual(name = "", labels = leg_shape_labs, values = c("19")) +
  scale_fill_manual(name = "", values = c("90" = "chartreuse1", "95" = "deepskyblue1"), labels = leg_fill_labs) +
  scale_colour_manual(name = "", values = c("IQR" = "cyan", "Reg" = "red", "45deg" = "black"), labels = leg_col_labs) +
  guides(shape = guide_legend(order = 1), fill = guide_legend(order = 2), colour = guide_legend(order = 3)) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.x = element_text(angle = 0, vjust = 1),
    legend.key.width = unit(0.8, "cm"),
    legend.position = "bottom"
  )

# Mostra il plot
plot(Stand_Res_std_QQ_plot)

# Calcola densità teorica nei quantili (pdf)
dens <- dsstd(quants, mean=mean, sd=sd, nu=SSTD_nu, xi=SSTD_xi)

# Calcola errore standard dei quantili teorici (formula banda Q-Q)
# Errore standard: SE = (p * (1-p)) / (n * f(Q)^2)
p <- ppoints(n)
SE <- sqrt(p * (1-p) / (n * dens^2))

# Limiti di confidenza (95%)
alpha <- 0.05
z <- qnorm(1 - alpha/2)

lower_95 <- quants - z * SE
upper_95 <- quants + z * SE

# Verifica quanti punti osservati cadono dentro le bande
# Ricorda che QQ_plot_df$X = dati ordinati osservati

inside_95 <- (QQ_plot_df$X >= lower_95) & (QQ_plot_df$X <= upper_95)

percent_inside_95 <- mean(inside_95) * 100

cat(sprintf("Percentuale di punti dentro la banda di confidenza al 95%%: %.2f%%\n", percent_inside_95))
# Percentuale di punti dentro la banda di confidenza al 95%: 96.20%

# Alpha per il 90%
alpha_90 <- 0.10
z_90 <- qnorm(1 - alpha_90/2)  # quantile normale per il 90%

# Limiti di confidenza al 90%
lower_90 <- quants - z_90 * SE
upper_90 <- quants + z_90 * SE

# Verifica quanti punti osservati cadono dentro la banda 90%
inside_90 <- (QQ_plot_df$X >= lower_90) & (QQ_plot_df$X <= upper_90)

percent_inside_90 <- mean(inside_90) * 100

cat(sprintf("Percentuale di punti dentro la banda di confidenza al 90%%: %.2f%%\n", percent_inside_90))
# Percentuale di punti dentro la banda di confidenza al 90%: 86.92%

# Kolmogorov-Smirnov test (K-S test)
ks_result <- ks.test(y, 
                     y = function(q) psstd(q, mean=0, sd=1, nu=SSTD_nu, xi=SSTD_xi),
                     alternative = "two.sided")

print(ks_result)
# 
#         Asymptotic one-sample Kolmogorov-Smirnov test
# 
# data:  y
# D = 0.050259, p-value = 0.1823
# alternative hypothesis: two-sided

# Dall'analisi del Q-Q Plot e del test di K-S vediamo, quindi, che i residui standardizzati seguono la distribuzione skewed Student-t con un livello
# di significatività del 5% (in realtà il test di K-S risulta anche al 10%).

# Verifichiamo ora la stazionarietà e l'omoschedasticità (non condizionata).

# Grafico dei residui
df_residui <- data.frame(
  Date = index(z_hat),
  Residui = as.numeric(z_hat)
)

ggplot(df_residui, aes(x = Date, y = Residui)) +
  geom_line(color = "steelblue", linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  labs(
    title = "Grafico dei residui standardizzati",
    subtitle = "Modello GARCH(1,2) con distribuzione skewed Student-t per SPY",
    x = "Data",
    y = "Residui standardizzati"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    axis.title = element_text(size = 11)
  )

# KPSS Test
kpss_test_level(df_residui, col = "Residui", alpha = 0.01)
# 
# [Residui] KPSS Test - Stazionarietà attorno a una media costante
# Statistic: 0.115785
# Valore critico (circa 1%): 0.739000
# Interpretazione del p-value: p-value > 0.10
# 
# ** CONCLUSIONE: Statistic <= valore critico (1%) **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO stazionari.

# Quindi, i residui standardizzati sono stazionari.

# Per l'omoschedasticità non condizionata, per prima cosa, vediamo se c'è curtosi nei residui standardizzati.

# Calcola la curtosi
kurt_z_hat <- kurtosis(as.numeric(z_hat), na.rm = TRUE)

cat(sprintf("Curtosi dei residui standardizzati: %.4f\n", kurt_z_hat))
# Curtosi dei residui standardizzati: 6.0805

# La curtosi c'è. Quindi, applichiamo la forma "studentized" dei test di Breusch-Pagan e White per la stazionarietà in varianza.

# Breusch-Pagan Test:
# Crea un indice temporale (può essere anche un numero progressivo)
df_residui$Index <- seq_along(df_residui$Residui)

# Esegui il test BP
breusch_pagan_test(
  index = df_residui$Index,
  group = df_residui$Residui,
  col = "Residui standardizzati"
)
# [Residui standardizzati] Breusch-Pagan Test
# Statistic: 4.815013
# p-value: 0.0282129
# 
# ** Conclusione: p-value > 0.01 **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON sono eteroschedastici.

# White Test
white_test(
  index = df_residui$Index,
  group = df_residui$Residui,
  col = "Residui standardizzati"
)
# [Residui standardizzati] White Test
# Statistic: 5.895447
# p-value: 0.052459
# 
# ** Conclusione: p-value > 0.01 **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON sono eteroschedastici.

# Dai risultati dei test si vede che è verificata anche la condizione di omoschedasticità (non condizionata).

# Per verificare ulteriormente l'assenza di autocorrelazione per i residui standardizzati
# di questo modello, facciamo anche l'ACF, il PACF e il test di Ljung-Box classico.

# ACF
df_z <- data.frame(z_hat = as.numeric(z_hat))
plot_acf_residuals(df_z, col = "z_hat")

# PACF
plot_pacf_residuals(df_z, col = "z_hat")

# L’analisi visiva di ACF e PACF mostra che, in entrambi i grafici, nessuna barra supera la banda di confidenza al 90%.
# Dunque, non c'è una forte evidenza visiva di autocorrelazione.

# Ljung-Box Test
ljungbox_test(as.numeric(z_hat), col_name = "Residui standardizzati")
# Ljung-Box test per Residui standardizzati (max_lag = 10):
#            lag     lb_stat lb_pvalue
# X-squared    1 0.008509299 0.9265027
# X-squared1   2 0.171750455 0.9177087
# X-squared2   3 0.952345492 0.8127807
# X-squared3   4 1.566993301 0.8147118
# X-squared4   5 1.580156852 0.9036385
# X-squared5   6 2.309148523 0.8891858
# X-squared6   7 2.635116981 0.9165837
# X-squared7   8 2.708221393 0.9513139
# X-squared8   9 3.189391255 0.9563102
# X-squared9  10 3.230847017 0.9754575

# Osservando i p-value associati ai vari lag, vediamo che tutti questi sono maggiori di α = 0.10. Dunque, non abbiamo evidenza statistica
# sufficiente per rifiutare l’ipotesi nulla di assenza di autocorrelazione fino al lag specificato.

# Infine, verifichiamo anche l’assenza di eteroschedasticità condizionata nei residui standardizzati.

# Test ARCH di Engle
arch_effects_test(z_hat, col_name = "Residui Standardizzati")
# ARCH Test per Residui Standardizzati (max_lag = 5):
#            lag arch_stat  arch_pvalue
# statistic    1  2.927802 8.706594e-02
# statistic1   2  5.003223 8.195284e-02
# statistic2   3  7.378117 6.077424e-02
# statistic3   4 39.514632 5.453533e-08
# statistic4   5 39.652368 1.754873e-07

# Alla luce dei risultati, a partire dal lag 4, il test ARCH rivela evidenza significativa di eteroschedasticità residua, con p-value < 1%.
# Questo suggerisce che il modello GARCH(1,2) con distribuzione skewed Student-t non ha catturato completamente la dinamica della varianza
# condizionata, e che potrebbero essere presenti effetti ARCH residui a orizzonti più lunghi.
# Perciò, possiamo concludere che anche questo modello è da scartare.

# Analizziamo il prossimo nella classifica: GARCH(0,1) sstd
p <- valid_models$p[6]
q <- valid_models$q[6]
dist <- valid_models$dist[6]

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = dist
)

fit <- tryCatch(
  ugarchfit(spec = spec, data = returns, solver = "hybrid"),
  error = function(e) NULL
)

# Risultati
show(fit)
# 
# *---------------------------------*
# *          GARCH Model Fit        *
# *---------------------------------*
#   
# Conditional Variance Dynamics 	
# -----------------------------------
# GARCH Model	    : sGARCH(0,1)
# Mean Model	    : ARFIMA(0,0,0)
# Distribution	  : sstd 
# 
# Optimal Parameters
# ------------------------------------
#        Estimate  Std. Error   t value Pr(>|t|)
# omega  0.002648    0.004562   0.58034  0.56169
# beta1  0.999000    0.002897 344.81869  0.00000
# skew   0.843118    0.045295  18.61395  0.00000
# shape  2.864943    0.402735   7.11372  0.00000
# 
# Robust Standard Errors:
#        Estimate  Std. Error   t value Pr(>|t|)
# omega  0.002648    0.008507   0.31124 0.755615
# beta1  0.999000    0.005494 181.82110 0.000000
# skew   0.843118    0.041012  20.55783 0.000000
# shape  2.864943    0.805856   3.55515 0.000378
# 
# LogLikelihood : -613.7808 
# 
# Information Criteria
# ------------------------------------
#   
# Akaike       2.6067
# Bayes        2.6418
# Shibata      2.6065
# Hannan-Quinn 2.6205
# 
# Weighted Ljung-Box Test on Standardized Residuals
# ------------------------------------
#                         statistic p-value
# Lag[1]                      1.498 0.22100
# Lag[2*(p+q)+(p+q)-1][2]     2.877 0.15151
# Lag[4*(p+q)+(p+q)-1][5]     7.369 0.04215
# d.o.f=0
# H0 : No serial correlation
# 
# Weighted Ljung-Box Test on Standardized Squared Residuals
# ------------------------------------
#                         statistic   p-value
# Lag[1]                      24.47 7.551e-07
# Lag[2*(p+q)+(p+q)-1][2]     24.52 2.907e-07
# Lag[4*(p+q)+(p+q)-1][5]     58.93 2.220e-16
# d.o.f=1
# 
# Weighted ARCH LM Tests
# ------------------------------------
#             Statistic Shape Scale   P-Value
# ARCH Lag[2]   0.09236 0.500 2.000 7.612e-01
# ARCH Lag[4]  41.88649 1.397 1.611 2.130e-11
# ARCH Lag[6]  51.13867 2.222 1.500 1.084e-13
# 
# Nyblom stability test
# ------------------------------------
# Joint Statistic:  5.2793
# Individual Statistics:             
# omega 0.49938
# beta1 0.45550
# skew  0.03879
# shape 0.16915
# 
# Asymptotic Critical Values (10% 5% 1%)
# Joint Statistic:     	   1.07 1.24 1.6
# Individual Statistic:	   0.35 0.47 0.75
# 
# Sign Bias Test
# ------------------------------------
#                    t-value      prob sig
# Sign Bias           0.5045 6.141e-01    
# Negative Sign Bias  4.0920 5.035e-05 ***
# Positive Sign Bias  2.8599 4.426e-03 ***
# Joint Effect       28.4277 2.954e-06 ***
#   
#   
# Adjusted Pearson Goodness-of-Fit Test:
# ------------------------------------
#   group statistic p-value(g-1)
# 1    20     30.05      0.05116
# 2    30     47.27      0.01749
# 3    40     54.44      0.05126
# 4    50     69.04      0.03108
# 
# 
# Elapsed time : 0.1750231

# Analizzando i p-value del test pesato di Ljung-Box sui residui standardizzati, osserviamo che questi sono tutti maggiori di α = 0.01.
# Quindi, non abbiamo evidenza statistica sufficiente per rifiutare l’ipotesi nulla di assenza di autocorrelazione.

# Applichiamo, ora, dei dest di normalità sui residui.

# Estrai residui standardizzati
z_hat <- residuals(fit, standardize = TRUE)

# Prendi le date del training_set:
# Rimuoviamo le righe con NA nella colonna usata per il modello
valid_dates <- training_set$Date[!is.na(training_set$SPY_LogReturn)]

# Converti in Date se non lo sono già
valid_dates <- as.Date(valid_dates)

# Sovrascrivi l’index di z_hat con le date corrette
index(z_hat) <- valid_dates

# Controlla che le lunghezze coincidano
if(length(z_hat) != length(valid_dates)) {
  stop("Errore: lunghezza residui e date non coincide!")
}

# Visualizza i primi valori
head(z_hat)
#                  [,1]
# 2023-08-01 -0.2715260
# 2023-08-02 -1.3263700
# 2023-08-03 -0.2715603
# 2023-08-04 -0.4286012
# 2023-08-07  0.8211597
# 2023-08-08 -0.4115216

# Test di SW
shapirowilk_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Shapiro-Wilk Test
# Statistic: 0.872202
# p-value  : 2.7006e-19
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Test di JB
jarque_bera_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Jarque-Bera Test
# Statistic: 5333.421033
# p-value  : 0
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Come atteso, i residui standardizzati del GARCH(0,1) con distribuzione skewed Student-t non seguono una distribuzione normale.

# QQ-Plot SSTD:
# Residui standardizzati ordinati
y <- sort(as.numeric(z_hat))
n <- length(y)

# Parametri stimati da 'fit'
params <- coef(fit)
SSTD_xi <- params["skew"]
SSTD_nu <- params["shape"]
mean <- 0
sd <- 1

# Parametri della distribuzione skewed Student-t
distr <- "sstd"
distr_pars <- list(mean = mean, sd = sd, nu = SSTD_nu, xi = SSTD_xi)

# Quantili teorici
quants <- qsstd(ppoints(n), mean = mean, sd = sd, nu = SSTD_nu, xi = SSTD_xi)

# DataFrame per plotting
QQ_plot_df <- data.frame(
  T = 1:n,
  Q = quants,
  X = y,
  Y = y
)

# Calcolo retta interquartile
quart_probs <- c(0.25, 0.75)
quart_X <- as.vector(quantile(QQ_plot_df$X, quart_probs))
quart_Q <- qsstd(quart_probs, mean = mean, sd = sd, nu = SSTD_nu, xi = SSTD_xi)
slope <- diff(quart_X) / diff(quart_Q)
intercept <- quart_X[1] - slope * quart_Q[1]

# Titoli
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 MPSMF 2024-2025",
                             paste("Q-Q plot of the Standardized Residuals from the GARCH(0,1) model for SPY (skewed Student-t)")))
subtitle_content <- bquote(paste("Sample size: ", .(n), 
                                 "; SSTD parameters → mean = ", .(mean), 
                                 ", sd = ", .(sd), 
                                 ", df = ", .(round(SSTD_nu, 3)), 
                                 ", skew = ", .(round(SSTD_xi, 3)), "."))

caption_content <- "Author: Matteo Basili"
x_name <- bquote("Theoretical Quantiles (SSTD)")
y_name <- bquote("Standardized Residuals")

# Assi e legende
x_breaks <- seq(from = floor(min(QQ_plot_df$Q)), to = ceiling(max(QQ_plot_df$Q)), by = 0.5)
x_labs <- format(x_breaks, scientific = FALSE)
y_breaks_num <- length(x_breaks)
y_binwidth <- round((max(QQ_plot_df$Y) - min(QQ_plot_df$Y)) / y_breaks_num, digits = 3)
y_breaks <- round(seq(from = floor(min(QQ_plot_df$Y)/y_binwidth)*y_binwidth, 
                      to = ceiling(max(QQ_plot_df$Y)/y_binwidth)*y_binwidth, 
                      by = y_binwidth), 3)
y_labs <- format(y_breaks, scientific = FALSE)

# Legende e colori
leg_shape_labs <- bquote("Q-Q plot")
leg_fill_labs <- c(bquote("90% confidence interval"), bquote("95% confidence interval"))
leg_col_labs <- c(bquote("Interquartile line"), bquote("Regression line"), bquote("y = x"))

Stand_Res_std_QQ_plot <- ggplot(QQ_plot_df) +
  stat_qq_band(aes(sample = X, fill = "95"), distribution = distr, dparams = distr_pars, conf = 0.95) +
  stat_qq_band(aes(sample = X, fill = "90"), distribution = distr, dparams = distr_pars, conf = 0.90) +
  geom_abline(aes(slope = slope, intercept = intercept, colour = "IQR"), linewidth = 0.8) +
  stat_smooth(aes(x = Q, y = Y, colour = "Reg", group = 1), method = "lm", se = FALSE, linewidth = 0.8) +
  geom_abline(aes(slope = 1, intercept = 0, colour = "45deg"), linewidth = 0.8) +
  stat_qq_point(aes(sample = X), shape = 19, color = "black", size = 1.2, alpha = 0.8, distribution = distr, dparams = distr_pars) +
  scale_x_continuous(name = x_name, breaks = x_breaks, labels = x_labs) +
  scale_y_continuous(name = y_name, breaks = y_breaks, labels = NULL,
                     sec.axis = sec_axis(~ ., breaks = y_breaks, labels = y_labs)) +
  ggtitle(title_content) +
  labs(subtitle = subtitle_content, caption = caption_content) +
  scale_shape_manual(name = "", labels = leg_shape_labs, values = c("19")) +
  scale_fill_manual(name = "", values = c("90" = "chartreuse1", "95" = "deepskyblue1"), labels = leg_fill_labs) +
  scale_colour_manual(name = "", values = c("IQR" = "cyan", "Reg" = "red", "45deg" = "black"), labels = leg_col_labs) +
  guides(shape = guide_legend(order = 1), fill = guide_legend(order = 2), colour = guide_legend(order = 3)) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.x = element_text(angle = 0, vjust = 1),
    legend.key.width = unit(0.8, "cm"),
    legend.position = "bottom"
  )

# Mostra il plot
plot(Stand_Res_std_QQ_plot)

# Calcola densità teorica nei quantili (pdf)
dens <- dsstd(quants, mean=mean, sd=sd, nu=SSTD_nu, xi=SSTD_xi)

# Calcola errore standard dei quantili teorici (formula banda Q-Q)
# Errore standard: SE = (p * (1-p)) / (n * f(Q)^2)
p <- ppoints(n)
SE <- sqrt(p * (1-p) / (n * dens^2))

# Limiti di confidenza (95%)
alpha <- 0.05
z <- qnorm(1 - alpha/2)

lower_95 <- quants - z * SE
upper_95 <- quants + z * SE

# Verifica quanti punti osservati cadono dentro le bande
# Ricorda che QQ_plot_df$X = dati ordinati osservati

inside_95 <- (QQ_plot_df$X >= lower_95) & (QQ_plot_df$X <= upper_95)

percent_inside_95 <- mean(inside_95) * 100

cat(sprintf("Percentuale di punti dentro la banda di confidenza al 95%%: %.2f%%\n", percent_inside_95))
# Percentuale di punti dentro la banda di confidenza al 95%: 100.00%

# Alpha per il 90%
alpha_90 <- 0.10
z_90 <- qnorm(1 - alpha_90/2)  # quantile normale per il 90%

# Limiti di confidenza al 90%
lower_90 <- quants - z_90 * SE
upper_90 <- quants + z_90 * SE

# Verifica quanti punti osservati cadono dentro la banda 90%
inside_90 <- (QQ_plot_df$X >= lower_90) & (QQ_plot_df$X <= upper_90)

percent_inside_90 <- mean(inside_90) * 100

cat(sprintf("Percentuale di punti dentro la banda di confidenza al 90%%: %.2f%%\n", percent_inside_90))
# Percentuale di punti dentro la banda di confidenza al 90%: 96.84%

# Kolmogorov-Smirnov test (K-S test)
ks_result <- ks.test(y, 
                     y = function(q) psstd(q, mean=0, sd=1, nu=SSTD_nu, xi=SSTD_xi),
                     alternative = "two.sided")

print(ks_result)
# 
#         Asymptotic one-sample Kolmogorov-Smirnov test
# 
# data:  y
# D = 0.040294, p-value = 0.4249
# alternative hypothesis: two-sided

# Dall'analisi del Q-Q Plot e del test di K-S vediamo, quindi, che i residui standardizzati seguono la distribuzione skewed Student-t con un livello
# di significatività del 10%.

# Verifichiamo ora la stazionarietà e l'omoschedasticità (non condizionata).

# Grafico dei residui
df_residui <- data.frame(
  Date = index(z_hat),
  Residui = as.numeric(z_hat)
)

ggplot(df_residui, aes(x = Date, y = Residui)) +
  geom_line(color = "steelblue", linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  labs(
    title = "Grafico dei residui standardizzati",
    subtitle = "Modello GARCH(0,1) con distribuzione skewed Student-t per SPY",
    x = "Data",
    y = "Residui standardizzati"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    axis.title = element_text(size = 11)
  )

# KPSS Test
kpss_test_level(df_residui, col = "Residui", alpha = 0.01)
# 
# [Residui] KPSS Test - Stazionarietà attorno a una media costante
# Statistic: 0.074278
# Valore critico (circa 1%): 0.739000
# Interpretazione del p-value: p-value > 0.10
# 
# ** CONCLUSIONE: Statistic <= valore critico (1%) **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO stazionari.

# Quindi, i residui standardizzati sono stazionari.

# Per l'omoschedasticità non condizionata, per prima cosa, vediamo se c'è curtosi nei residui standardizzati.

# Calcola la curtosi
kurt_z_hat <- kurtosis(as.numeric(z_hat), na.rm = TRUE)

cat(sprintf("Curtosi dei residui standardizzati: %.4f\n", kurt_z_hat))
# Curtosi dei residui standardizzati: 19.3757

# La curtosi c'è. Quindi, applichiamo la forma "studentized" dei test di Breusch-Pagan e White per la stazionarietà in varianza.

# Breusch-Pagan Test:
# Crea un indice temporale (può essere anche un numero progressivo)
df_residui$Index <- seq_along(df_residui$Residui)

# Esegui il test BP
breusch_pagan_test(
  index = df_residui$Index,
  group = df_residui$Residui,
  col = "Residui standardizzati"
)
# [Residui standardizzati] Breusch-Pagan Test
# Statistic: 6.947447
# p-value: 0.00839389
# 
# ** Conclusione: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO eteroschedastici.

# White Test
white_test(
  index = df_residui$Index,
  group = df_residui$Residui,
  col = "Residui standardizzati"
)
# [Residui standardizzati] White Test
# Statistic: 10.759178
# p-value: 0.00460972
# 
# ** Conclusione: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO eteroschedastici.

# Dai risultati dei test possiamo rigettare l'ipotesi nulla di omoschedasticità incondizionata con un livello di significatività dell'1%. Quindi,
# c'è evidenza statistica di eteroschedasticità non condizionata.
# Perciò, possiamo concludere che anche questo modello è da scartare.

# Analizziamo il prossimo nella classifica: GARCH(2,1) sged
p <- valid_models$p[7]
q <- valid_models$q[7]
dist <- valid_models$dist[7]

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = dist
)

fit <- tryCatch(
  ugarchfit(spec = spec, data = returns, solver = "hybrid"),
  error = function(e) NULL
)

# Risultati
show(fit)
# 
# *---------------------------------*
# *          GARCH Model Fit        *
# *---------------------------------*
#   
# Conditional Variance Dynamics 	
# -----------------------------------
# GARCH Model	    : sGARCH(2,1)
# Mean Model	    : ARFIMA(0,0,0)
# Distribution	  : sged 
# 
# Optimal Parameters
# ------------------------------------
#         Estimate  Std. Error  t value Pr(>|t|)
# omega   0.067083    0.025943  2.58580 0.009715
# alpha1  0.084058    0.050924  1.65066 0.098809
# alpha2  0.042503    0.059037  0.71994 0.471562
# beta1   0.800751    0.048321 16.57153 0.000000
# skew    0.875266    0.045704 19.15096 0.000000
# shape   1.223428    0.109691 11.15343 0.000000
# 
# Robust Standard Errors:
#         Estimate  Std. Error  t value Pr(>|t|)
# omega   0.067083    0.020015   3.3517 0.000803
# alpha1  0.084058    0.046551   1.8057 0.070962
# alpha2  0.042503    0.036223   1.1734 0.240649
# beta1   0.800751    0.034674  23.0939 0.000000
# skew    0.875266    0.049594  17.6486 0.000000
# shape   1.223428    0.151886   8.0549 0.000000
# 
# LogLikelihood : -598.7948 
# 
# Information Criteria
# ------------------------------------
#   
# Akaike       2.5519
# Bayes        2.6046
# Shibata      2.5516
# Hannan-Quinn 2.5726
# 
# Weighted Ljung-Box Test on Standardized Residuals
# ------------------------------------
#                         statistic p-value
# Lag[1]                    0.02120  0.8842
# Lag[2*(p+q)+(p+q)-1][2]   0.06918  0.9418
# Lag[4*(p+q)+(p+q)-1][5]   0.66357  0.9298
# d.o.f=0
# H0 : No serial correlation
# 
# Weighted Ljung-Box Test on Standardized Squared Residuals
# ------------------------------------
#                          statistic   p-value
# Lag[1]                       3.286 6.986e-02
# Lag[2*(p+q)+(p+q)-1][8]     21.401 4.945e-05
# Lag[4*(p+q)+(p+q)-1][14]    26.520 6.054e-05
# d.o.f=3
# 
# Weighted ARCH LM Tests
# ------------------------------------
#             Statistic Shape Scale   P-Value
# ARCH Lag[4]     23.81 0.500 2.000 1.065e-06
# ARCH Lag[6]     23.91 1.461 1.711 3.347e-06
# ARCH Lag[8]     24.29 2.368 1.583 8.164e-06
# 
# Nyblom stability test
# ------------------------------------
# Joint Statistic:  1.0556
# Individual Statistics:             
# omega  0.1072
# alpha1 0.2132
# alpha2 0.1612
# beta1  0.1170
# skew   0.1867
# shape  0.4383
# 
# Asymptotic Critical Values (10% 5% 1%)
# Joint Statistic:     	   1.49 1.68 2.12
# Individual Statistic:	   0.35 0.47 0.75
# 
# Sign Bias Test
# ------------------------------------
#                    t-value    prob sig
# Sign Bias          1.07717 0.28196    
# Negative Sign Bias 1.20612 0.22838    
# Positive Sign Bias 0.06484 0.94833    
# Joint Effect       6.70030 0.08209   *
#   
#   
# Adjusted Pearson Goodness-of-Fit Test:
# ------------------------------------
#   group statistic p-value(g-1)
# 1    20     18.07       0.5179
# 2    30     37.14       0.1427
# 3    40     42.46       0.3244
# 4    50     59.97       0.1355
# 
# 
# Elapsed time : 1.271469

# Analizzando i p-value del test pesato di Ljung-Box sui residui standardizzati, osserviamo che questi sono tutti maggiori di α = 0.01
# (in realtà anche di α = 0.05 e α = 0.10). Quindi, non abbiamo evidenza statistica sufficiente per rifiutare l’ipotesi nulla di assenza di
# autocorrelazione.

# Applichiamo, ora, dei dest di normalità sui residui.

# Estrai residui standardizzati
z_hat <- residuals(fit, standardize = TRUE)

# Prendi le date del training_set:
# Rimuoviamo le righe con NA nella colonna usata per il modello
valid_dates <- training_set$Date[!is.na(training_set$SPY_LogReturn)]

# Converti in Date se non lo sono già
valid_dates <- as.Date(valid_dates)

# Sovrascrivi l’index di z_hat con le date corrette
index(z_hat) <- valid_dates

# Controlla che le lunghezze coincidano
if(length(z_hat) != length(valid_dates)) {
  stop("Errore: lunghezza residui e date non coincide!")
}

# Visualizza i primi valori
head(z_hat)
#                  [,1]
# 2023-08-01 -0.2715260
# 2023-08-02 -1.3272829
# 2023-08-03 -0.2702912
# 2023-08-04 -0.4402184
# 2023-08-07  0.8978582
# 2023-08-08 -0.4620987

# Test di SW
shapirowilk_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Shapiro-Wilk Test
# Statistic: 0.964012
# p-value  : 2.24942e-09
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Test di JB
jarque_bera_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Jarque-Bera Test
# Statistic: 185.936119
# p-value  : 0
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Come atteso, i residui standardizzati del GARCH(2,1) con distribuzione skewed GED non seguono una distribuzione normale.

# QQ-Plot SGED:
# Residui standardizzati ordinati
y <- sort(as.numeric(z_hat))
n <- length(y)

# Parametri stimati da 'fit'
params <- coef(fit)
SGED_xi <- params["skew"]
SGED_nu <- params["shape"]
mean <- 0
sd <- 1

# Parametri della distribuzione skewed GED
distr <- "sged"
distr_pars <- list(mean = mean, sd = sd, nu = SGED_nu, xi = SGED_xi)

# Quantili teorici
quants <- qsged(ppoints(n), mean = mean, sd = sd, nu = SGED_nu, xi = SGED_xi)

# DataFrame per plotting
QQ_plot_df <- data.frame(
  T = 1:n,
  Q = quants,
  X = y,
  Y = y
)

# Calcolo retta interquartile
quart_probs <- c(0.25, 0.75)
quart_X <- as.vector(quantile(QQ_plot_df$X, quart_probs))
quart_Q <- qsged(quart_probs, mean = mean, sd = sd, nu = SGED_nu, xi = SGED_xi)
slope <- diff(quart_X) / diff(quart_Q)
intercept <- quart_X[1] - slope * quart_Q[1]

# Titoli
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 MPSMF 2024-2025",
                             paste("Q-Q plot of the Standardized Residuals from the GARCH(2,1) model for SPY (skewed GED)")))
subtitle_content <- bquote(paste("Sample size: ", .(n), 
                                 "; SGED parameters → mean = ", .(mean), 
                                 ", sd = ", .(sd), 
                                 ", shape = ", .(round(SGED_nu, 3)), 
                                 ", skew = ", .(round(SGED_xi, 3)), "."))

caption_content <- "Author: Matteo Basili"
x_name <- bquote("Theoretical Quantiles (SGED)")
y_name <- bquote("Standardized Residuals")

# Assi e legende
x_breaks <- seq(from = floor(min(QQ_plot_df$Q)), to = ceiling(max(QQ_plot_df$Q)), by = 0.5)
x_labs <- format(x_breaks, scientific = FALSE)
y_breaks_num <- length(x_breaks)
y_binwidth <- round((max(QQ_plot_df$Y) - min(QQ_plot_df$Y)) / y_breaks_num, digits = 3)
y_breaks <- round(seq(from = floor(min(QQ_plot_df$Y)/y_binwidth)*y_binwidth, 
                      to = ceiling(max(QQ_plot_df$Y)/y_binwidth)*y_binwidth, 
                      by = y_binwidth), 3)
y_labs <- format(y_breaks, scientific = FALSE)

# Legende e colori
leg_shape_labs <- bquote("Q-Q plot")
leg_fill_labs <- c(bquote("90% confidence interval"), bquote("95% confidence interval"))
leg_col_labs <- c(bquote("Interquartile line"), bquote("Regression line"), bquote("y = x"))

Stand_Res_std_QQ_plot <- ggplot(QQ_plot_df) + 
  stat_qq_band(aes(sample = X, fill = "95"), distribution = distr, dparams = distr_pars, conf = 0.95) +
  stat_qq_band(aes(sample = X, fill = "90"), distribution = distr, dparams = distr_pars, conf = 0.90) +
  geom_abline(aes(slope = slope, intercept = intercept, colour = "IQR"), linewidth = 0.8) +
  stat_smooth(aes(x = Q, y = Y, colour = "Reg", group = 1), method = "lm", se = FALSE, linewidth = 0.8) +
  geom_abline(aes(slope = 1, intercept = 0, colour = "45deg"), linewidth = 0.8) +
  stat_qq_point(aes(sample = X), shape = 19, color = "black", size = 1.2, alpha = 0.8, distribution = distr, dparams = distr_pars) +
  scale_x_continuous(name = x_name, breaks = x_breaks, labels = x_labs) +
  scale_y_continuous(name = y_name, breaks = y_breaks, labels = NULL,
                     sec.axis = sec_axis(~ ., breaks = y_breaks, labels = y_labs)) +
  ggtitle(title_content) +
  labs(subtitle = subtitle_content, caption = caption_content) +
  scale_shape_manual(name = "", labels = leg_shape_labs, values = c("19")) +
  scale_fill_manual(name = "", values = c("90" = "chartreuse1", "95" = "deepskyblue1"), labels = leg_fill_labs) +
  scale_colour_manual(name = "", values = c("IQR" = "cyan", "Reg" = "red", "45deg" = "black"), labels = leg_col_labs) +
  guides(shape = guide_legend(order = 1), fill = guide_legend(order = 2), colour = guide_legend(order = 3)) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.x = element_text(angle = 0, vjust = 1),
    legend.key.width = unit(0.8, "cm"),
    legend.position = "bottom"
  )

# Mostra il plot
plot(Stand_Res_std_QQ_plot)

# Calcola densità teorica nei quantili (SGED)
dens <- dsged(quants, mean = mean, sd = sd, nu = SGED_nu, xi = SGED_xi)

# Probabilità associate ai quantili
p <- ppoints(n)

# Errore standard dei quantili teorici
SE <- sqrt(p * (1 - p) / (n * dens^2))

# Limiti di confidenza al 95%
alpha_95 <- 0.05
z_95 <- qnorm(1 - alpha_95 / 2)
lower_95 <- quants - z_95 * SE
upper_95 <- quants + z_95 * SE

# Percentuale di punti dentro la banda 95%
inside_95 <- (QQ_plot_df$X >= lower_95) & (QQ_plot_df$X <= upper_95)
percent_inside_95 <- mean(inside_95) * 100
cat(sprintf("Percentuale di punti dentro la banda di confidenza al 95%%: %.2f%%\n", percent_inside_95))
# Percentuale di punti dentro la banda di confidenza al 95%: 97.89%

# Limiti di confidenza al 90%
alpha_90 <- 0.10
z_90 <- qnorm(1 - alpha_90 / 2)
lower_90 <- quants - z_90 * SE
upper_90 <- quants + z_90 * SE

# Percentuale di punti dentro la banda 90%
inside_90 <- (QQ_plot_df$X >= lower_90) & (QQ_plot_df$X <= upper_90)
percent_inside_90 <- mean(inside_90) * 100
cat(sprintf("Percentuale di punti dentro la banda di confidenza al 90%%: %.2f%%\n", percent_inside_90))
# Percentuale di punti dentro la banda di confidenza al 90%: 86.71%

# Kolmogorov-Smirnov test (K-S test)
ks_result <- ks.test(y, 
                     y = function(q) psged(q, mean=0, sd=1, nu=SGED_nu, xi=SGED_xi),
                     alternative = "two.sided")

print(ks_result)
# 
#         Asymptotic one-sample Kolmogorov-Smirnov test
# 
# data:  y
# D = 0.04935, p-value = 0.1986
# alternative hypothesis: two-sided

# Dall'analisi del Q-Q Plot e del test di K-S vediamo, quindi, che i residui standardizzati seguono la distribuzione skewed GED con un livello
# di significatività del 5% (in realtà il test di K-S risulta anche al 10%).

# Verifichiamo ora la stazionarietà e l'omoschedasticità (non condizionata).

# Grafico dei residui
df_residui <- data.frame(
  Date = index(z_hat),
  Residui = as.numeric(z_hat)
)

ggplot(df_residui, aes(x = Date, y = Residui)) +
  geom_line(color = "steelblue", linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  labs(
    title = "Grafico dei residui standardizzati",
    subtitle = "Modello GARCH(2,1) con distribuzione skewed GED per SPY",
    x = "Data",
    y = "Residui standardizzati"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    axis.title = element_text(size = 11)
  )

# KPSS Test
kpss_test_level(df_residui, col = "Residui", alpha = 0.01)
# 
# [Residui] KPSS Test - Stazionarietà attorno a una media costante
# Statistic: 0.127131
# Valore critico (circa 1%): 0.739000
# Interpretazione del p-value: p-value > 0.10
# 
# ** CONCLUSIONE: Statistic <= valore critico (1%) **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO stazionari.

# Quindi, i residui standardizzati sono stazionari.

# Per l'omoschedasticità non condizionata, per prima cosa, vediamo se c'è curtosi nei residui standardizzati.

# Calcola la curtosi
kurt_z_hat <- kurtosis(as.numeric(z_hat), na.rm = TRUE)

cat(sprintf("Curtosi dei residui standardizzati: %.4f\n", kurt_z_hat))
# Curtosi dei residui standardizzati: 5.8196

# La curtosi c'è. Quindi, applichiamo la forma "studentized" dei test di Breusch-Pagan e White per la stazionarietà in varianza.

# Breusch-Pagan Test:
# Crea un indice temporale (può essere anche un numero progressivo)
df_residui$Index <- seq_along(df_residui$Residui)

# Esegui il test BP
breusch_pagan_test(
  index = df_residui$Index,
  group = df_residui$Residui,
  col = "Residui standardizzati"
)
# [Residui standardizzati] Breusch-Pagan Test
# Statistic: 3.936940
# p-value: 0.0472366
# 
# ** Conclusione: p-value > 0.01 **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON sono eteroschedastici.

# White Test
white_test(
  index = df_residui$Index,
  group = df_residui$Residui,
  col = "Residui standardizzati"
)
# [Residui standardizzati] White Test
# Statistic: 4.667595
# p-value: 0.096927
# 
# ** Conclusione: p-value > 0.01 **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON sono eteroschedastici.

# Dai risultati dei test si vede che è verificata anche la condizione di omoschedasticità (non condizionata).

# Per verificare ulteriormente l'assenza di autocorrelazione per i residui standardizzati
# di questo modello, facciamo anche l'ACF, il PACF e il test di Ljung-Box classico.

# ACF
df_z <- data.frame(z_hat = as.numeric(z_hat))
plot_acf_residuals(df_z, col = "z_hat")

# PACF
plot_pacf_residuals(df_z, col = "z_hat")

# L’analisi visiva di ACF e PACF mostra che, in entrambi i grafici, nessuna barra supera la banda di confidenza al 90%.
# Dunque, non c'è una forte evidenza visiva di autocorrelazione.

# Ljung-Box Test
ljungbox_test(as.numeric(z_hat), col_name = "Residui standardizzati")
# Ljung-Box test per Residui standardizzati (max_lag = 10):
#            lag    lb_stat lb_pvalue
# X-squared    1 0.02119613 0.8842459
# X-squared1   2 0.11716084 0.9431024
# X-squared2   3 0.86900038 0.8329015
# X-squared3   4 1.14540021 0.8870037
# X-squared4   5 1.16506746 0.9481844
# X-squared5   6 1.89324981 0.9292498
# X-squared6   7 2.31667076 0.9402582
# X-squared7   8 2.36892468 0.9675631
# X-squared8   9 2.90696980 0.9678692
# X-squared9  10 2.94533726 0.9826813

# Osservando i p-value associati ai vari lag, vediamo che tutti questi sono maggiori di α = 0.10. Dunque, non abbiamo evidenza statistica
# sufficiente per rifiutare l’ipotesi nulla di assenza di autocorrelazione fino al lag specificato.

# Infine, verifichiamo anche l’assenza di eteroschedasticità condizionata nei residui standardizzati.

# Test ARCH di Engle
arch_effects_test(z_hat, col_name = "Residui Standardizzati")
# ARCH Test per Residui Standardizzati (max_lag = 5):
#            lag arch_stat  arch_pvalue
# statistic    1  3.933539 4.733224e-02
# statistic1   2  6.786162 3.360499e-02
# statistic2   3  8.553081 3.586269e-02
# statistic3   4 29.149900 7.287966e-06
# statistic4   5 29.270046 2.052365e-05

# Alla luce dei risultati, a partire dal lag 4, il test ARCH rivela evidenza significativa di eteroschedasticità residua, con p-value < 1%.
# Questo suggerisce che il modello GARCH(2,1) con distribuzione skewed GED non ha catturato completamente la dinamica della varianza
# condizionata, e che potrebbero essere presenti effetti ARCH residui a orizzonti più lunghi.
# Perciò, possiamo concludere che anche questo modello è da scartare.

# Analizziamo il prossimo nella classifica: GARCH(1,2) sged
p <- valid_models$p[8]
q <- valid_models$q[8]
dist <- valid_models$dist[8]

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = dist
)

fit <- tryCatch(
  ugarchfit(spec = spec, data = returns, solver = "hybrid"),
  error = function(e) NULL
)

# Risultati
show(fit)
# 
# *---------------------------------*
# *          GARCH Model Fit        *
# *---------------------------------*
#   
# Conditional Variance Dynamics 	
# -----------------------------------
# GARCH Model	    : sGARCH(1,2)
# Mean Model	    : ARFIMA(0,0,0)
# Distribution	  : sged 
# 
# Optimal Parameters
# ------------------------------------
#         Estimate  Std. Error   t value Pr(>|t|)
# omega   0.061752    0.022056  2.799812 0.005113
# alpha1  0.114417    0.033264  3.439632 0.000583
# beta1   0.817963    0.062978 12.988092 0.000000
# beta2   0.000000    0.057736  0.000007 0.999994
# skew    0.879694    0.044369 19.826573 0.000000
# shape   1.218571    0.109460 11.132545 0.000000
# 
# Robust Standard Errors:
#         Estimate  Std. Error  t value Pr(>|t|)
# omega   0.061752    0.016181  3.81634 0.000135
# alpha1  0.114417    0.039044  2.93049 0.003384
# beta1   0.817963    0.013135 62.27141 0.000000
# beta2   0.000000    0.019929  0.00002 0.999984
# skew    0.879694    0.047569 18.49308 0.000000
# shape   1.218571    0.150174  8.11440 0.000000
# 
# LogLikelihood : -598.9861 
# 
# Information Criteria
# ------------------------------------
#   
# Akaike       2.5527
# Bayes        2.6054
# Shibata      2.5524
# Hannan-Quinn 2.5734
# 
# Weighted Ljung-Box Test on Standardized Residuals
# ------------------------------------
#                         statistic p-value
# Lag[1]                   0.004145  0.9487
# Lag[2*(p+q)+(p+q)-1][2]  0.050061  0.9563
# Lag[4*(p+q)+(p+q)-1][5]  0.647078  0.9328
# d.o.f=0
# H0 : No serial correlation
# 
# Weighted Ljung-Box Test on Standardized Squared Residuals
# ------------------------------------
#                          statistic   p-value
# Lag[1]                       1.305 2.534e-01
# Lag[2*(p+q)+(p+q)-1][8]     21.849 3.758e-05
# Lag[4*(p+q)+(p+q)-1][14]    27.727 3.044e-05
# d.o.f=3
# 
# Weighted ARCH LM Tests
# ------------------------------------
#             Statistic Shape Scale   P-Value
# ARCH Lag[4]     28.49 0.500 2.000 9.416e-08
# ARCH Lag[6]     28.56 1.461 1.711 2.393e-07
# ARCH Lag[8]     29.02 2.368 1.583 5.174e-07
# 
# Nyblom stability test
# ------------------------------------
# Joint Statistic:  1.1258
# Individual Statistics:             
# omega  0.1070
# alpha1 0.1918
# beta1  0.1132
# beta2  0.1063
# skew   0.1673
# shape  0.4145
# 
# Asymptotic Critical Values (10% 5% 1%)
# Joint Statistic:     	   1.49 1.68 2.12
# Individual Statistic:	   0.35 0.47 0.75
# 
# Sign Bias Test
# ------------------------------------
#                    t-value   prob sig
# Sign Bias           1.2003 0.2306    
# Negative Sign Bias  0.7571 0.4494    
# Positive Sign Bias  0.2120 0.8322    
# Joint Effect        5.5873 0.1335    
# 
# 
# Adjusted Pearson Goodness-of-Fit Test:
# ------------------------------------
#   group statistic p-value(g-1)
# 1    20     18.49      0.49000
# 2    30     39.16      0.09855
# 3    40     45.66      0.21482
# 4    50     73.26      0.01396
# 
# 
# Elapsed time : 0.2276711 

# Analizzando i p-value del test pesato di Ljung-Box sui residui standardizzati, osserviamo che questi sono tutti maggiori di α = 0.01
# (in realtà anche di α = 0.05 e α = 0.10). Quindi, non abbiamo evidenza statistica sufficiente per rifiutare l’ipotesi nulla di assenza di
# autocorrelazione.

# Applichiamo, ora, dei dest di normalità sui residui.

# Estrai residui standardizzati
z_hat <- residuals(fit, standardize = TRUE)

# Prendi le date del training_set:
# Rimuoviamo le righe con NA nella colonna usata per il modello
valid_dates <- training_set$Date[!is.na(training_set$SPY_LogReturn)]

# Converti in Date se non lo sono già
valid_dates <- as.Date(valid_dates)

# Sovrascrivi l’index di z_hat con le date corrette
index(z_hat) <- valid_dates

# Controlla che le lunghezze coincidano
if(length(z_hat) != length(valid_dates)) {
  stop("Errore: lunghezza residui e date non coincide!")
}

# Visualizza i primi valori
head(z_hat)
#                  [,1]
# 2023-08-01 -0.2715260
# 2023-08-02 -1.3272829
# 2023-08-03 -0.2622802
# 2023-08-04 -0.4422318
# 2023-08-07  0.8941551
# 2023-08-08 -0.4541644

# Test di SW
shapirowilk_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Shapiro-Wilk Test
# Statistic: 0.964375
# p-value  : 2.60291e-09
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Test di JB
jarque_bera_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Jarque-Bera Test
# Statistic: 187.828375
# p-value  : 0
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Come atteso, i residui standardizzati del GARCH(1,2) con distribuzione skewed GED non seguono una distribuzione normale.

# QQ-Plot SGED:
# Residui standardizzati ordinati
y <- sort(as.numeric(z_hat))
n <- length(y)

# Parametri stimati da 'fit'
params <- coef(fit)
SGED_xi <- params["skew"]
SGED_nu <- params["shape"]
mean <- 0
sd <- 1

# Parametri della distribuzione skewed GED
distr <- "sged"
distr_pars <- list(mean = mean, sd = sd, nu = SGED_nu, xi = SGED_xi)

# Quantili teorici
quants <- qsged(ppoints(n), mean = mean, sd = sd, nu = SGED_nu, xi = SGED_xi)

# DataFrame per plotting
QQ_plot_df <- data.frame(
  T = 1:n,
  Q = quants,
  X = y,
  Y = y
)

# Calcolo retta interquartile
quart_probs <- c(0.25, 0.75)
quart_X <- as.vector(quantile(QQ_plot_df$X, quart_probs))
quart_Q <- qsged(quart_probs, mean = mean, sd = sd, nu = SGED_nu, xi = SGED_xi)
slope <- diff(quart_X) / diff(quart_Q)
intercept <- quart_X[1] - slope * quart_Q[1]

# Titoli
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 MPSMF 2024-2025",
                             paste("Q-Q plot of the Standardized Residuals from the GARCH(1,2) model for SPY (skewed GED)")))
subtitle_content <- bquote(paste("Sample size: ", .(n), 
                                 "; SGED parameters → mean = ", .(mean), 
                                 ", sd = ", .(sd), 
                                 ", shape = ", .(round(SGED_nu, 3)), 
                                 ", skew = ", .(round(SGED_xi, 3)), "."))

caption_content <- "Author: Matteo Basili"
x_name <- bquote("Theoretical Quantiles (SGED)")
y_name <- bquote("Standardized Residuals")

# Assi e legende
x_breaks <- seq(from = floor(min(QQ_plot_df$Q)), to = ceiling(max(QQ_plot_df$Q)), by = 0.5)
x_labs <- format(x_breaks, scientific = FALSE)
y_breaks_num <- length(x_breaks)
y_binwidth <- round((max(QQ_plot_df$Y) - min(QQ_plot_df$Y)) / y_breaks_num, digits = 3)
y_breaks <- round(seq(from = floor(min(QQ_plot_df$Y)/y_binwidth)*y_binwidth, 
                      to = ceiling(max(QQ_plot_df$Y)/y_binwidth)*y_binwidth, 
                      by = y_binwidth), 3)
y_labs <- format(y_breaks, scientific = FALSE)

# Legende e colori
leg_shape_labs <- bquote("Q-Q plot")
leg_fill_labs <- c(bquote("90% confidence interval"), bquote("95% confidence interval"))
leg_col_labs <- c(bquote("Interquartile line"), bquote("Regression line"), bquote("y = x"))

Stand_Res_std_QQ_plot <- ggplot(QQ_plot_df) + 
  stat_qq_band(aes(sample = X, fill = "95"), distribution = distr, dparams = distr_pars, conf = 0.95) +
  stat_qq_band(aes(sample = X, fill = "90"), distribution = distr, dparams = distr_pars, conf = 0.90) +
  geom_abline(aes(slope = slope, intercept = intercept, colour = "IQR"), linewidth = 0.8) +
  stat_smooth(aes(x = Q, y = Y, colour = "Reg", group = 1), method = "lm", se = FALSE, linewidth = 0.8) +
  geom_abline(aes(slope = 1, intercept = 0, colour = "45deg"), linewidth = 0.8) +
  stat_qq_point(aes(sample = X), shape = 19, color = "black", size = 1.2, alpha = 0.8, distribution = distr, dparams = distr_pars) +
  scale_x_continuous(name = x_name, breaks = x_breaks, labels = x_labs) +
  scale_y_continuous(name = y_name, breaks = y_breaks, labels = NULL,
                     sec.axis = sec_axis(~ ., breaks = y_breaks, labels = y_labs)) +
  ggtitle(title_content) +
  labs(subtitle = subtitle_content, caption = caption_content) +
  scale_shape_manual(name = "", labels = leg_shape_labs, values = c("19")) +
  scale_fill_manual(name = "", values = c("90" = "chartreuse1", "95" = "deepskyblue1"), labels = leg_fill_labs) +
  scale_colour_manual(name = "", values = c("IQR" = "cyan", "Reg" = "red", "45deg" = "black"), labels = leg_col_labs) +
  guides(shape = guide_legend(order = 1), fill = guide_legend(order = 2), colour = guide_legend(order = 3)) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.x = element_text(angle = 0, vjust = 1),
    legend.key.width = unit(0.8, "cm"),
    legend.position = "bottom"
  )

# Mostra il plot
plot(Stand_Res_std_QQ_plot)

# Calcola densità teorica nei quantili (SGED)
dens <- dsged(quants, mean = mean, sd = sd, nu = SGED_nu, xi = SGED_xi)

# Probabilità associate ai quantili
p <- ppoints(n)

# Errore standard dei quantili teorici
SE <- sqrt(p * (1 - p) / (n * dens^2))

# Limiti di confidenza al 95%
alpha_95 <- 0.05
z_95 <- qnorm(1 - alpha_95 / 2)
lower_95 <- quants - z_95 * SE
upper_95 <- quants + z_95 * SE

# Percentuale di punti dentro la banda 95%
inside_95 <- (QQ_plot_df$X >= lower_95) & (QQ_plot_df$X <= upper_95)
percent_inside_95 <- mean(inside_95) * 100
cat(sprintf("Percentuale di punti dentro la banda di confidenza al 95%%: %.2f%%\n", percent_inside_95))
# Percentuale di punti dentro la banda di confidenza al 95%: 97.05%

# Limiti di confidenza al 90%
alpha_90 <- 0.10
z_90 <- qnorm(1 - alpha_90 / 2)
lower_90 <- quants - z_90 * SE
upper_90 <- quants + z_90 * SE

# Percentuale di punti dentro la banda 90%
inside_90 <- (QQ_plot_df$X >= lower_90) & (QQ_plot_df$X <= upper_90)
percent_inside_90 <- mean(inside_90) * 100
cat(sprintf("Percentuale di punti dentro la banda di confidenza al 90%%: %.2f%%\n", percent_inside_90))
# Percentuale di punti dentro la banda di confidenza al 90%: 83.97%

# Kolmogorov-Smirnov test (K-S test)
ks_result <- ks.test(y, 
                     y = function(q) psged(q, mean=0, sd=1, nu=SGED_nu, xi=SGED_xi),
                     alternative = "two.sided")

print(ks_result)
# 
#         Asymptotic one-sample Kolmogorov-Smirnov test
# 
# data:  y
# D = 0.048605, p-value = 0.2127
# alternative hypothesis: two-sided

# Dall'analisi del Q-Q Plot e del test di K-S vediamo, quindi, che i residui standardizzati seguono la distribuzione skewed GED con un livello
# di significatività del 5% (in realtà il test di K-S risulta anche al 10%).

# Verifichiamo ora la stazionarietà e l'omoschedasticità (non condizionata).

# Grafico dei residui
df_residui <- data.frame(
  Date = index(z_hat),
  Residui = as.numeric(z_hat)
)

ggplot(df_residui, aes(x = Date, y = Residui)) +
  geom_line(color = "steelblue", linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  labs(
    title = "Grafico dei residui standardizzati",
    subtitle = "Modello GARCH(1,2) con distribuzione skewed GED per SPY",
    x = "Data",
    y = "Residui standardizzati"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    axis.title = element_text(size = 11)
  )

# KPSS Test
kpss_test_level(df_residui, col = "Residui", alpha = 0.01)
# [Residui] KPSS Test - Stazionarietà attorno a una media costante
# Statistic: 0.124865
# Valore critico (circa 1%): 0.739000
# Interpretazione del p-value: p-value > 0.10
# 
# ** CONCLUSIONE: Statistic <= valore critico (1%) **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO stazionari.

# Quindi, i residui standardizzati sono stazionari.

# Per l'omoschedasticità non condizionata, per prima cosa, vediamo se c'è curtosi nei residui standardizzati.

# Calcola la curtosi
kurt_z_hat <- kurtosis(as.numeric(z_hat), na.rm = TRUE)

cat(sprintf("Curtosi dei residui standardizzati: %.4f\n", kurt_z_hat))
# Curtosi dei residui standardizzati: 5.8642

# La curtosi c'è. Quindi, applichiamo la forma "studentized" dei test di Breusch-Pagan e White per la stazionarietà in varianza.

# Breusch-Pagan Test:
# Crea un indice temporale (può essere anche un numero progressivo)
df_residui$Index <- seq_along(df_residui$Residui)

# Esegui il test BP
breusch_pagan_test(
  index = df_residui$Index,
  group = df_residui$Residui,
  col = "Residui standardizzati"
)
# [Residui standardizzati] Breusch-Pagan Test
# Statistic: 3.838080
# p-value: 0.0501009
# 
# ** Conclusione: p-value > 0.01 **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON sono eteroschedastici.

# White Test
white_test(
  index = df_residui$Index,
  group = df_residui$Residui,
  col = "Residui standardizzati"
)
# [Residui standardizzati] White Test
# Statistic: 4.484840
# p-value: 0.106201
# 
# ** Conclusione: p-value > 0.01 **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON sono eteroschedastici.

# Dai risultati dei test si vede che è verificata anche la condizione di omoschedasticità (non condizionata).

# Per verificare ulteriormente l'assenza di autocorrelazione per i residui standardizzati
# di questo modello, facciamo anche l'ACF, il PACF e il test di Ljung-Box classico.

# ACF
df_z <- data.frame(z_hat = as.numeric(z_hat))
plot_acf_residuals(df_z, col = "z_hat")

# PACF
plot_pacf_residuals(df_z, col = "z_hat")

# L’analisi visiva di ACF e PACF mostra che, in entrambi i grafici, nessuna barra supera la banda di confidenza al 90%.
# Dunque, non c'è una forte evidenza visiva di autocorrelazione.

# Ljung-Box Test
ljungbox_test(as.numeric(z_hat), col_name = "Residui standardizzati")
# Ljung-Box test per Residui standardizzati (max_lag = 10):
#            lag     lb_stat lb_pvalue
# X-squared    1 0.004144538 0.9486692
# X-squared1   2 0.095977404 0.9531446
# X-squared2   3 0.760976227 0.8587764
# X-squared3   4 1.164970622 0.8838316
# X-squared4   5 1.209322634 0.9439799
# X-squared5   6 1.950225937 0.9242189
# X-squared6   7 2.366227099 0.9368291
# X-squared7   8 2.428947811 0.9649619
# X-squared8   9 2.895995764 0.9682759
# X-squared9  10 2.963751734 0.9822643

# Osservando i p-value associati ai vari lag, vediamo che tutti questi sono maggiori di α = 0.10. Dunque, non abbiamo evidenza statistica
# sufficiente per rifiutare l’ipotesi nulla di assenza di autocorrelazione fino al lag specificato.

# Infine, verifichiamo anche l’assenza di eteroschedasticità condizionata nei residui standardizzati.

# Test ARCH di Engle
arch_effects_test(z_hat, col_name = "Residui Standardizzati")
# ARCH Test per Residui Standardizzati (max_lag = 5):
#            lag arch_stat  arch_pvalue
# statistic    1  1.734046 1.878951e-01
# statistic1   2  3.915499 1.411758e-01
# statistic2   3  5.015263 1.706829e-01
# statistic3   4 31.135211 2.872993e-06
# statistic4   5 31.146163 8.764900e-06

# Alla luce dei risultati, a partire dal lag 4, il test ARCH rivela evidenza significativa di eteroschedasticità residua, con p-value < 1%.
# Questo suggerisce che il modello GARCH(1,2) con distribuzione skewed GED non ha catturato completamente la dinamica della varianza
# condizionata, e che potrebbero essere presenti effetti ARCH residui a orizzonti più lunghi.
# Perciò, possiamo concludere che anche questo modello è da scartare.

# Analizziamo il prossimo nella classifica: GARCH(2,0) sstd
p <- valid_models$p[9]
q <- valid_models$q[9]
dist <- valid_models$dist[9]

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = dist
)

fit <- tryCatch(
  ugarchfit(spec = spec, data = returns, solver = "hybrid"),
  error = function(e) NULL
)

# Risultati
show(fit)
# 
# *---------------------------------*
# *          GARCH Model Fit        *
# *---------------------------------*
#   
# Conditional Variance Dynamics 	
# -----------------------------------
# GARCH Model	    : sGARCH(2,0)
# Mean Model	    : ARFIMA(0,0,0)
# Distribution	  : sstd 
# 
# Optimal Parameters
# ------------------------------------
#         Estimate  Std. Error  t value Pr(>|t|)
# omega   0.826089    0.151817   5.4414  0.00000
# alpha1  0.157245    0.086377   1.8205  0.06869
# alpha2  0.061597    0.059908   1.0282  0.30386
# skew    0.839869    0.048516  17.3113  0.00000
# shape   3.722554    0.691524   5.3831  0.00000
# 
# Robust Standard Errors:
#         Estimate  Std. Error  t value Pr(>|t|)
# omega   0.826089    0.185972   4.4420 0.000009
# alpha1  0.157245    0.101989   1.5418 0.123126
# alpha2  0.061597    0.042814   1.4387 0.150239
# skew    0.839869    0.042816  19.6156 0.000000
# shape   3.722554    0.949715   3.9197 0.000089
# 
# LogLikelihood : -607.987 
# 
# Information Criteria
# ------------------------------------
#   
# Akaike       2.5864
# Bayes        2.6303
# Shibata      2.5862
# Hannan-Quinn 2.6037
# 
# Weighted Ljung-Box Test on Standardized Residuals
# ------------------------------------
#                         statistic p-value
# Lag[1]                     0.2126  0.6448
# Lag[2*(p+q)+(p+q)-1][2]    0.6385  0.6320
# Lag[4*(p+q)+(p+q)-1][5]    4.4007  0.2081
# d.o.f=0
# H0 : No serial correlation
# 
# Weighted Ljung-Box Test on Standardized Squared Residuals
# ------------------------------------
#                         statistic   p-value
# Lag[1]                      0.143 7.053e-01
# Lag[2*(p+q)+(p+q)-1][5]    17.208 1.125e-04
# Lag[4*(p+q)+(p+q)-1][9]    29.155 7.424e-07
# d.o.f=2
# 
# Weighted ARCH LM Tests
# ------------------------------------
#             Statistic Shape Scale   P-Value
# ARCH Lag[3]     2.103 0.500 2.000 1.470e-01
# ARCH Lag[5]    33.227 1.440 1.667 9.454e-09
# ARCH Lag[7]    35.831 2.315 1.543 4.608e-09
# 
# Nyblom stability test
# ------------------------------------
# Joint Statistic:  1.4885
# Individual Statistics:              
# omega  0.43161
# alpha1 0.36424
# alpha2 0.19864
# skew   0.03008
# shape  0.06065
# 
# Asymptotic Critical Values (10% 5% 1%)
# Joint Statistic:     	   1.28 1.47 1.88
# Individual Statistic:	   0.35 0.47 0.75
# 
# Sign Bias Test
# ------------------------------------
#                    t-value   prob sig
# Sign Bias          0.46396 0.6429    
# Negative Sign Bias 1.56731 0.1177    
# Positive Sign Bias 0.03951 0.9685    
# Joint Effect       5.36655 0.1468    
# 
# 
# Adjusted Pearson Goodness-of-Fit Test:
# ------------------------------------
#   group statistic p-value(g-1)
# 1    20     34.78     0.014858
# 2    30     42.96     0.045898
# 3    40     70.47     0.001492
# 4    50     70.73     0.022752
# 
# 
# Elapsed time : 0.2746429

# Analizzando i p-value del test pesato di Ljung-Box sui residui standardizzati, osserviamo che questi sono tutti maggiori di α = 0.01
# (in realtà anche di α = 0.05 e α = 0.10). Quindi, non abbiamo evidenza statistica sufficiente per rifiutare l’ipotesi nulla di
# assenza di autocorrelazione.

# Applichiamo, ora, dei dest di normalità sui residui.

# Estrai residui standardizzati
z_hat <- residuals(fit, standardize = TRUE)

# Prendi le date del training_set:
# Rimuoviamo le righe con NA nella colonna usata per il modello
valid_dates <- training_set$Date[!is.na(training_set$SPY_LogReturn)]

# Converti in Date se non lo sono già
valid_dates <- as.Date(valid_dates)

# Sovrascrivi l’index di z_hat con le date corrette
index(z_hat) <- valid_dates

# Controlla che le lunghezze coincidano
if(length(z_hat) != length(valid_dates)) {
  stop("Errore: lunghezza residui e date non coincide!")
}

# Visualizza i primi valori
head(z_hat)
#                  [,1]
# 2023-08-01 -0.2715260
# 2023-08-02 -1.3272829
# 2023-08-03 -0.2688371
# 2023-08-04 -0.4626533
# 2023-08-07  0.9352313
# 2023-08-08 -0.4453833

# Test di SW
shapirowilk_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Shapiro-Wilk Test
# Statistic: 0.889124
# p-value  : 5.49385e-18
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Test di JB
jarque_bera_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Jarque-Bera Test
# Statistic: 4830.605842
# p-value  : 0
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Come atteso, i residui standardizzati del GARCH(2,0) con distribuzione skewed Student-t non seguono una distribuzione normale.

# QQ-Plot SSTD:
# Residui standardizzati ordinati
y <- sort(as.numeric(z_hat))
n <- length(y)

# Parametri stimati da 'fit'
params <- coef(fit)
SSTD_xi <- params["skew"]
SSTD_nu <- params["shape"]
mean <- 0
sd <- 1

# Parametri della distribuzione skewed Student-t
distr <- "sstd"
distr_pars <- list(mean = mean, sd = sd, nu = SSTD_nu, xi = SSTD_xi)

# Quantili teorici
quants <- qsstd(ppoints(n), mean = mean, sd = sd, nu = SSTD_nu, xi = SSTD_xi)

# DataFrame per plotting
QQ_plot_df <- data.frame(
  T = 1:n,
  Q = quants,
  X = y,
  Y = y
)

# Calcolo retta interquartile
quart_probs <- c(0.25, 0.75)
quart_X <- as.vector(quantile(QQ_plot_df$X, quart_probs))
quart_Q <- qsstd(quart_probs, mean = mean, sd = sd, nu = SSTD_nu, xi = SSTD_xi)
slope <- diff(quart_X) / diff(quart_Q)
intercept <- quart_X[1] - slope * quart_Q[1]

# Titoli
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 MPSMF 2024-2025",
                             paste("Q-Q plot of the Standardized Residuals from the GARCH(2,0) model for SPY (skewed Student-t)")))
subtitle_content <- bquote(paste("Sample size: ", .(n), 
                                 "; SSTD parameters → mean = ", .(mean), 
                                 ", sd = ", .(sd), 
                                 ", df = ", .(round(SSTD_nu, 3)), 
                                 ", skew = ", .(round(SSTD_xi, 3)), "."))

caption_content <- "Author: Matteo Basili"
x_name <- bquote("Theoretical Quantiles (SSTD)")
y_name <- bquote("Standardized Residuals")

# Assi e legende
x_breaks <- seq(from = floor(min(QQ_plot_df$Q)), to = ceiling(max(QQ_plot_df$Q)), by = 0.5)
x_labs <- format(x_breaks, scientific = FALSE)
y_breaks_num <- length(x_breaks)
y_binwidth <- round((max(QQ_plot_df$Y) - min(QQ_plot_df$Y)) / y_breaks_num, digits = 3)
y_breaks <- round(seq(from = floor(min(QQ_plot_df$Y)/y_binwidth)*y_binwidth, 
                      to = ceiling(max(QQ_plot_df$Y)/y_binwidth)*y_binwidth, 
                      by = y_binwidth), 3)
y_labs <- format(y_breaks, scientific = FALSE)

# Legende e colori
leg_shape_labs <- bquote("Q-Q plot")
leg_fill_labs <- c(bquote("90% confidence interval"), bquote("95% confidence interval"))
leg_col_labs <- c(bquote("Interquartile line"), bquote("Regression line"), bquote("y = x"))

Stand_Res_std_QQ_plot <- ggplot(QQ_plot_df) +
  stat_qq_band(aes(sample = X, fill = "95"), distribution = distr, dparams = distr_pars, conf = 0.95) +
  stat_qq_band(aes(sample = X, fill = "90"), distribution = distr, dparams = distr_pars, conf = 0.90) +
  geom_abline(aes(slope = slope, intercept = intercept, colour = "IQR"), linewidth = 0.8) +
  stat_smooth(aes(x = Q, y = Y, colour = "Reg", group = 1), method = "lm", se = FALSE, linewidth = 0.8) +
  geom_abline(aes(slope = 1, intercept = 0, colour = "45deg"), linewidth = 0.8) +
  stat_qq_point(aes(sample = X), shape = 19, color = "black", size = 1.2, alpha = 0.8, distribution = distr, dparams = distr_pars) +
  scale_x_continuous(name = x_name, breaks = x_breaks, labels = x_labs) +
  scale_y_continuous(name = y_name, breaks = y_breaks, labels = NULL,
                     sec.axis = sec_axis(~ ., breaks = y_breaks, labels = y_labs)) +
  ggtitle(title_content) +
  labs(subtitle = subtitle_content, caption = caption_content) +
  scale_shape_manual(name = "", labels = leg_shape_labs, values = c("19")) +
  scale_fill_manual(name = "", values = c("90" = "chartreuse1", "95" = "deepskyblue1"), labels = leg_fill_labs) +
  scale_colour_manual(name = "", values = c("IQR" = "cyan", "Reg" = "red", "45deg" = "black"), labels = leg_col_labs) +
  guides(shape = guide_legend(order = 1), fill = guide_legend(order = 2), colour = guide_legend(order = 3)) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.x = element_text(angle = 0, vjust = 1),
    legend.key.width = unit(0.8, "cm"),
    legend.position = "bottom"
  )

# Mostra il plot
plot(Stand_Res_std_QQ_plot)

# Calcola densità teorica nei quantili (pdf)
dens <- dsstd(quants, mean=mean, sd=sd, nu=SSTD_nu, xi=SSTD_xi)

# Calcola errore standard dei quantili teorici (formula banda Q-Q)
# Errore standard: SE = (p * (1-p)) / (n * f(Q)^2)
p <- ppoints(n)
SE <- sqrt(p * (1-p) / (n * dens^2))

# Limiti di confidenza (95%)
alpha <- 0.05
z <- qnorm(1 - alpha/2)

lower_95 <- quants - z * SE
upper_95 <- quants + z * SE

# Verifica quanti punti osservati cadono dentro le bande
# Ricorda che QQ_plot_df$X = dati ordinati osservati

inside_95 <- (QQ_plot_df$X >= lower_95) & (QQ_plot_df$X <= upper_95)

percent_inside_95 <- mean(inside_95) * 100

cat(sprintf("Percentuale di punti dentro la banda di confidenza al 95%%: %.2f%%\n", percent_inside_95))
# Percentuale di punti dentro la banda di confidenza al 95%: 98.31%

# Alpha per il 90%
alpha_90 <- 0.10
z_90 <- qnorm(1 - alpha_90/2)  # quantile normale per il 90%

# Limiti di confidenza al 90%
lower_90 <- quants - z_90 * SE
upper_90 <- quants + z_90 * SE

# Verifica quanti punti osservati cadono dentro la banda 90%
inside_90 <- (QQ_plot_df$X >= lower_90) & (QQ_plot_df$X <= upper_90)

percent_inside_90 <- mean(inside_90) * 100

cat(sprintf("Percentuale di punti dentro la banda di confidenza al 90%%: %.2f%%\n", percent_inside_90))
# Percentuale di punti dentro la banda di confidenza al 90%: 92.62%

# Kolmogorov-Smirnov test (K-S test)
ks_result <- ks.test(y, 
                     y = function(q) psstd(q, mean=0, sd=1, nu=SSTD_nu, xi=SSTD_xi),
                     alternative = "two.sided")

print(ks_result)
# 
#         Asymptotic one-sample Kolmogorov-Smirnov test
# 
# data:  y
# D = 0.045675, p-value = 0.2761
# alternative hypothesis: two-sided

# Dall'analisi del Q-Q Plot e del test di K-S vediamo, quindi, che i residui standardizzati seguono la distribuzione skewed Student-t con un livello
# di significatività del 10%.

# Verifichiamo ora la stazionarietà e l'omoschedasticità (non condizionata).

# Grafico dei residui
df_residui <- data.frame(
  Date = index(z_hat),
  Residui = as.numeric(z_hat)
)

ggplot(df_residui, aes(x = Date, y = Residui)) +
  geom_line(color = "steelblue", linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  labs(
    title = "Grafico dei residui standardizzati",
    subtitle = "Modello GARCH(2,0) con distribuzione skewed Student-t per SPY",
    x = "Data",
    y = "Residui standardizzati"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    axis.title = element_text(size = 11)
  )

# KPSS Test
kpss_test_level(df_residui, col = "Residui", alpha = 0.01)
# 
# [Residui] KPSS Test - Stazionarietà attorno a una media costante
# Statistic: 0.060021
# Valore critico (circa 1%): 0.739000
# Interpretazione del p-value: p-value > 0.10
# 
# ** CONCLUSIONE: Statistic <= valore critico (1%) **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO stazionari.

# Quindi, i residui standardizzati sono stazionari.

# Per l'omoschedasticità non condizionata, per prima cosa, vediamo se c'è curtosi nei residui standardizzati.

# Calcola la curtosi
kurt_z_hat <- kurtosis(as.numeric(z_hat), na.rm = TRUE)

cat(sprintf("Curtosi dei residui standardizzati: %.4f\n", kurt_z_hat))
# Curtosi dei residui standardizzati: 18.5137

# La curtosi c'è. Quindi, applichiamo la forma "studentized" dei test di Breusch-Pagan e White per la stazionarietà in varianza.

# Breusch-Pagan Test:
# Crea un indice temporale (può essere anche un numero progressivo)
df_residui$Index <- seq_along(df_residui$Residui)

# Esegui il test BP
breusch_pagan_test(
  index = df_residui$Index,
  group = df_residui$Residui,
  col = "Residui standardizzati"
)
# [Residui standardizzati] Breusch-Pagan Test
# Statistic: 6.737970
# p-value: 0.0094382
# 
# ** Conclusione: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO eteroschedastici.

# White Test
white_test(
  index = df_residui$Index,
  group = df_residui$Residui,
  col = "Residui standardizzati"
)
# [Residui standardizzati] White Test
# Statistic: 9.350198
# p-value: 0.0093246
# 
# ** Conclusione: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO eteroschedastici.

# Dai risultati dei test possiamo rigettare l'ipotesi nulla di omoschedasticità incondizionata con un livello di significatività dell'1%. Quindi,
# c'è evidenza statistica di eteroschedasticità non condizionata.
# Perciò, possiamo concludere che anche questo modello è da scartare.

# Analizziamo il prossimo nella classifica: GARCH(1,1) norm
p <- valid_models$p[10]
q <- valid_models$q[10]
dist <- valid_models$dist[10]

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = dist
)

fit <- tryCatch(
  ugarchfit(spec = spec, data = returns, solver = "hybrid"),
  error = function(e) NULL
)

# Risultati
show(fit)
# 
# *---------------------------------*
# *          GARCH Model Fit        *
# *---------------------------------*
#   
# Conditional Variance Dynamics 	
# -----------------------------------
# GARCH Model	    : sGARCH(1,1)
# Mean Model	    : ARFIMA(0,0,0)
# Distribution	  : norm 
# 
# Optimal Parameters
# ------------------------------------
#         Estimate  Std. Error  t value Pr(>|t|)
# omega   0.054855    0.019209   2.8556 0.004295
# alpha1  0.124299    0.027693   4.4884 0.000007
# beta1   0.819110    0.035174  23.2872 0.000000
# 
# Robust Standard Errors:
#         Estimate  Std. Error  t value Pr(>|t|)
# omega   0.054855    0.028206   1.9448 0.051799
# alpha1  0.124299    0.046939   2.6481 0.008094
# beta1   0.819110    0.039449  20.7636 0.000000
# 
# LogLikelihood : -621.3758 
# 
# Information Criteria
# ------------------------------------
#   
# Akaike       2.6345
# Bayes        2.6608
# Shibata      2.6344
# Hannan-Quinn 2.6449
# 
# Weighted Ljung-Box Test on Standardized Residuals
# ------------------------------------
#                         statistic p-value
# Lag[1]                   0.002039  0.9640
# Lag[2*(p+q)+(p+q)-1][2]  0.033895  0.9691
# Lag[4*(p+q)+(p+q)-1][5]  0.557627  0.9484
# d.o.f=0
# H0 : No serial correlation
# 
# Weighted Ljung-Box Test on Standardized Squared Residuals
# ------------------------------------
#                         statistic   p-value
# Lag[1]                     0.7872 0.3749616
# Lag[2*(p+q)+(p+q)-1][5]   12.6593 0.0018678
# Lag[4*(p+q)+(p+q)-1][9]   19.9078 0.0002121
# d.o.f=2
# 
# Weighted ARCH LM Tests
# ------------------------------------
#             Statistic Shape Scale   P-Value
# ARCH Lag[3]     0.334 0.500 2.000 5.633e-01
# ARCH Lag[5]    19.886 1.440 1.667 2.289e-05
# ARCH Lag[7]    21.481 2.315 1.543 2.669e-05
# 
# Nyblom stability test
# ------------------------------------
# Joint Statistic:  0.4791
# Individual Statistics:             
# omega  0.2707
# alpha1 0.3783
# beta1  0.2812
# 
# Asymptotic Critical Values (10% 5% 1%)
# Joint Statistic:     	   0.846 1.01 1.35
# Individual Statistic:	   0.35 0.47 0.75
# 
# Sign Bias Test
# ------------------------------------
#                    t-value   prob sig
# Sign Bias           1.2985 0.1947    
# Negative Sign Bias  0.5247 0.6001    
# Positive Sign Bias  0.2858 0.7751    
# Joint Effect        5.3861 0.1456    
# 
# 
# Adjusted Pearson Goodness-of-Fit Test:
# ------------------------------------
#   group statistic p-value(g-1)
# 1    20     46.93    0.0003657
# 2    30     57.01    0.0014317
# 3    40     63.72    0.0074838
# 4    50     74.95    0.0099478
# 
# 
# Elapsed time : 0.05727887 

# Analizzando i p-value del test pesato di Ljung-Box sui residui standardizzati, osserviamo che questi sono tutti maggiori di α = 0.01
# (in realtà anche di α = 0.05 e α = 0.10). Quindi, non abbiamo evidenza statistica sufficiente per rifiutare l’ipotesi nulla di
# assenza di autocorrelazione.

# Applichiamo, ora, dei dest di normalità sui residui.

# Estrai residui standardizzati
z_hat <- residuals(fit, standardize = TRUE)

# Prendi le date del training_set:
# Rimuoviamo le righe con NA nella colonna usata per il modello
valid_dates <- training_set$Date[!is.na(training_set$SPY_LogReturn)]

# Converti in Date se non lo sono già
valid_dates <- as.Date(valid_dates)

# Sovrascrivi l’index di z_hat con le date corrette
index(z_hat) <- valid_dates

# Controlla che le lunghezze coincidano
if(length(z_hat) != length(valid_dates)) {
  stop("Errore: lunghezza residui e date non coincide!")
}

# Visualizza i primi valori
head(z_hat)
#                  [,1]
# 2023-08-01 -0.2715260
# 2023-08-02 -1.4168867
# 2023-08-03 -0.2737180
# 2023-08-04 -0.4612777
# 2023-08-07  0.9309845
# 2023-08-08 -0.4692783

# Test di SW
shapirowilk_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Shapiro-Wilk Test
# Statistic: 0.964909
# p-value  : 3.23303e-09
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Test di JB
jarque_bera_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Jarque-Bera Test
# Statistic: 180.392949
# p-value  : 0
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Come vediamo, i residui standardizzati del GARCH(1,1) con distribuzione normale non seguono una distribuzione normale.
# Perciò, possiamo concludere che anche questo modello è da scartare.

# Analizziamo il prossimo nella classifica: GARCH(3,0) sstd
p <- valid_models$p[11]
q <- valid_models$q[11]
dist <- valid_models$dist[11]

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = dist
)

fit <- tryCatch(
  ugarchfit(spec = spec, data = returns, solver = "hybrid"),
  error = function(e) NULL
)

# Risultati
show(fit)
# 
# *---------------------------------*
# *          GARCH Model Fit        *
# *---------------------------------*
#   
# Conditional Variance Dynamics 	
# -----------------------------------
# GARCH Model	    : sGARCH(3,0)
# Mean Model	    : ARFIMA(0,0,0)
# Distribution	  : sstd 
# 
# Optimal Parameters
# ------------------------------------
#         Estimate  Std. Error  t value Pr(>|t|)
# omega    0.61599    0.105814  5.82146 0.000000
# alpha1   0.10989    0.063021  1.74367 0.081217
# alpha2   0.04819    0.056698  0.84994 0.395359
# alpha3   0.22754    0.091079  2.49826 0.012481
# skew     0.81869    0.050046 16.35889 0.000000
# shape    4.58814    1.048598  4.37549 0.000012
# 
# Robust Standard Errors:
#         Estimate  Std. Error  t value Pr(>|t|)
# omega    0.61599    0.115347  5.34033 0.000000
# alpha1   0.10989    0.061502  1.78672 0.073982
# alpha2   0.04819    0.051892  0.92865 0.353070
# alpha3   0.22754    0.091067  2.49861 0.012468
# skew     0.81869    0.045478 18.00203 0.000000
# shape    4.58814    1.289630  3.55771 0.000374
# 
# LogLikelihood : -601.7897 
# 
# Information Criteria
# ------------------------------------
#   
# Akaike       2.5645
# Bayes        2.6172
# Shibata      2.5642
# Hannan-Quinn 2.5852
# 
# Weighted Ljung-Box Test on Standardized Residuals
# ------------------------------------
#                         statistic p-value
# Lag[1]                     0.1029  0.7483
# Lag[2*(p+q)+(p+q)-1][2]    0.3092  0.7906
# Lag[4*(p+q)+(p+q)-1][5]    1.1306  0.8295
# d.o.f=0
# H0 : No serial correlation
# 
# Weighted Ljung-Box Test on Standardized Squared Residuals
# ------------------------------------
#                          statistic   p-value
# Lag[1]                       4.421 3.551e-02
# Lag[2*(p+q)+(p+q)-1][8]     21.352 5.095e-05
# Lag[4*(p+q)+(p+q)-1][14]    27.795 2.926e-05
# d.o.f=3
# 
# Weighted ARCH LM Tests
# ------------------------------------
#             Statistic Shape Scale   P-Value
# ARCH Lag[4]     24.99 0.500 2.000 5.756e-07
# ARCH Lag[6]     25.52 1.461 1.711 1.349e-06
# ARCH Lag[8]     26.64 2.368 1.583 2.086e-06
# 
# Nyblom stability test
# ------------------------------------
# Joint Statistic:  1.4537
# Individual Statistics:              
# omega  0.24007
# alpha1 0.17335
# alpha2 0.09593
# alpha3 0.22527
# skew   0.03989
# shape  0.08045
# 
# Asymptotic Critical Values (10% 5% 1%)
# Joint Statistic:     	   1.49 1.68 2.12
# Individual Statistic:	   0.35 0.47 0.75
# 
# Sign Bias Test
# ------------------------------------
#                    t-value   prob sig
# Sign Bias          0.79848 0.4250    
# Negative Sign Bias 0.88389 0.3772    
# Positive Sign Bias 0.06833 0.9455    
# Joint Effect       3.42887 0.3301    
# 
# 
# Adjusted Pearson Goodness-of-Fit Test:
# ------------------------------------
#   group statistic p-value(g-1)
# 1    20     28.36      0.07669
# 2    30     38.66      0.10838
# 3    40     45.66      0.21482
# 4    50     50.05      0.43146
# 
# 
# Elapsed time : 1.861321 

# Analizzando i p-value del test pesato di Ljung-Box sui residui standardizzati, osserviamo che questi sono tutti maggiori di α = 0.01
# (in realtà anche di α = 0.05 e α = 0.10). Quindi, non abbiamo evidenza statistica sufficiente per rifiutare l’ipotesi nulla di
# assenza di autocorrelazione.

# Applichiamo, ora, dei dest di normalità sui residui.

# Estrai residui standardizzati
z_hat <- residuals(fit, standardize = TRUE)

# Prendi le date del training_set:
# Rimuoviamo le righe con NA nella colonna usata per il modello
valid_dates <- training_set$Date[!is.na(training_set$SPY_LogReturn)]

# Converti in Date se non lo sono già
valid_dates <- as.Date(valid_dates)

# Sovrascrivi l’index di z_hat con le date corrette
index(z_hat) <- valid_dates

# Controlla che le lunghezze coincidano
if(length(z_hat) != length(valid_dates)) {
  stop("Errore: lunghezza residui e date non coincide!")
}

# Visualizza i primi valori
head(z_hat)
#                  [,1]
# 2023-08-01 -0.2715260
# 2023-08-02 -1.3272829
# 2023-08-03 -0.2719337
# 2023-08-04 -0.5275417
# 2023-08-07  0.8327573
# 2023-08-08 -0.5109154

# Test di SW
shapirowilk_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Shapiro-Wilk Test
# Statistic: 0.956594
# p-value  : 1.35817e-10
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Test di JB
jarque_bera_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Jarque-Bera Test
# Statistic: 311.671953
# p-value  : 0
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Come atteso, i residui standardizzati del GARCH(3,0) con distribuzione skewed Student-t non seguono una distribuzione normale.

# QQ-Plot SSTD:
# Residui standardizzati ordinati
y <- sort(as.numeric(z_hat))
n <- length(y)

# Parametri stimati da 'fit'
params <- coef(fit)
SSTD_xi <- params["skew"]
SSTD_nu <- params["shape"]
mean <- 0
sd <- 1

# Parametri della distribuzione skewed Student-t
distr <- "sstd"
distr_pars <- list(mean = mean, sd = sd, nu = SSTD_nu, xi = SSTD_xi)

# Quantili teorici
quants <- qsstd(ppoints(n), mean = mean, sd = sd, nu = SSTD_nu, xi = SSTD_xi)

# DataFrame per plotting
QQ_plot_df <- data.frame(
  T = 1:n,
  Q = quants,
  X = y,
  Y = y
)

# Calcolo retta interquartile
quart_probs <- c(0.25, 0.75)
quart_X <- as.vector(quantile(QQ_plot_df$X, quart_probs))
quart_Q <- qsstd(quart_probs, mean = mean, sd = sd, nu = SSTD_nu, xi = SSTD_xi)
slope <- diff(quart_X) / diff(quart_Q)
intercept <- quart_X[1] - slope * quart_Q[1]

# Titoli
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 MPSMF 2024-2025",
                             paste("Q-Q plot of the Standardized Residuals from the GARCH(3,0) model for SPY (skewed Student-t)")))
subtitle_content <- bquote(paste("Sample size: ", .(n), 
                                 "; SSTD parameters → mean = ", .(mean), 
                                 ", sd = ", .(sd), 
                                 ", df = ", .(round(SSTD_nu, 3)), 
                                 ", skew = ", .(round(SSTD_xi, 3)), "."))

caption_content <- "Author: Matteo Basili"
x_name <- bquote("Theoretical Quantiles (SSTD)")
y_name <- bquote("Standardized Residuals")

# Assi e legende
x_breaks <- seq(from = floor(min(QQ_plot_df$Q)), to = ceiling(max(QQ_plot_df$Q)), by = 0.5)
x_labs <- format(x_breaks, scientific = FALSE)
y_breaks_num <- length(x_breaks)
y_binwidth <- round((max(QQ_plot_df$Y) - min(QQ_plot_df$Y)) / y_breaks_num, digits = 3)
y_breaks <- round(seq(from = floor(min(QQ_plot_df$Y)/y_binwidth)*y_binwidth, 
                      to = ceiling(max(QQ_plot_df$Y)/y_binwidth)*y_binwidth, 
                      by = y_binwidth), 3)
y_labs <- format(y_breaks, scientific = FALSE)

# Legende e colori
leg_shape_labs <- bquote("Q-Q plot")
leg_fill_labs <- c(bquote("90% confidence interval"), bquote("95% confidence interval"))
leg_col_labs <- c(bquote("Interquartile line"), bquote("Regression line"), bquote("y = x"))

Stand_Res_std_QQ_plot <- ggplot(QQ_plot_df) +
  stat_qq_band(aes(sample = X, fill = "95"), distribution = distr, dparams = distr_pars, conf = 0.95) +
  stat_qq_band(aes(sample = X, fill = "90"), distribution = distr, dparams = distr_pars, conf = 0.90) +
  geom_abline(aes(slope = slope, intercept = intercept, colour = "IQR"), linewidth = 0.8) +
  stat_smooth(aes(x = Q, y = Y, colour = "Reg", group = 1), method = "lm", se = FALSE, linewidth = 0.8) +
  geom_abline(aes(slope = 1, intercept = 0, colour = "45deg"), linewidth = 0.8) +
  stat_qq_point(aes(sample = X), shape = 19, color = "black", size = 1.2, alpha = 0.8, distribution = distr, dparams = distr_pars) +
  scale_x_continuous(name = x_name, breaks = x_breaks, labels = x_labs) +
  scale_y_continuous(name = y_name, breaks = y_breaks, labels = NULL,
                     sec.axis = sec_axis(~ ., breaks = y_breaks, labels = y_labs)) +
  ggtitle(title_content) +
  labs(subtitle = subtitle_content, caption = caption_content) +
  scale_shape_manual(name = "", labels = leg_shape_labs, values = c("19")) +
  scale_fill_manual(name = "", values = c("90" = "chartreuse1", "95" = "deepskyblue1"), labels = leg_fill_labs) +
  scale_colour_manual(name = "", values = c("IQR" = "cyan", "Reg" = "red", "45deg" = "black"), labels = leg_col_labs) +
  guides(shape = guide_legend(order = 1), fill = guide_legend(order = 2), colour = guide_legend(order = 3)) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.x = element_text(angle = 0, vjust = 1),
    legend.key.width = unit(0.8, "cm"),
    legend.position = "bottom"
  )

# Mostra il plot
plot(Stand_Res_std_QQ_plot)

# Calcola densità teorica nei quantili (pdf)
dens <- dsstd(quants, mean=mean, sd=sd, nu=SSTD_nu, xi=SSTD_xi)

# Calcola errore standard dei quantili teorici (formula banda Q-Q)
# Errore standard: SE = (p * (1-p)) / (n * f(Q)^2)
p <- ppoints(n)
SE <- sqrt(p * (1-p) / (n * dens^2))

# Limiti di confidenza (95%)
alpha <- 0.05
z <- qnorm(1 - alpha/2)

lower_95 <- quants - z * SE
upper_95 <- quants + z * SE

# Verifica quanti punti osservati cadono dentro le bande
# Ricorda che QQ_plot_df$X = dati ordinati osservati

inside_95 <- (QQ_plot_df$X >= lower_95) & (QQ_plot_df$X <= upper_95)

percent_inside_95 <- mean(inside_95) * 100

cat(sprintf("Percentuale di punti dentro la banda di confidenza al 95%%: %.2f%%\n", percent_inside_95))
# Percentuale di punti dentro la banda di confidenza al 95%: 98.52%

# Alpha per il 90%
alpha_90 <- 0.10
z_90 <- qnorm(1 - alpha_90/2)  # quantile normale per il 90%

# Limiti di confidenza al 90%
lower_90 <- quants - z_90 * SE
upper_90 <- quants + z_90 * SE

# Verifica quanti punti osservati cadono dentro la banda 90%
inside_90 <- (QQ_plot_df$X >= lower_90) & (QQ_plot_df$X <= upper_90)

percent_inside_90 <- mean(inside_90) * 100

cat(sprintf("Percentuale di punti dentro la banda di confidenza al 90%%: %.2f%%\n", percent_inside_90))
# Percentuale di punti dentro la banda di confidenza al 90%: 91.77%

# Kolmogorov-Smirnov test (K-S test)
ks_result <- ks.test(y, 
                     y = function(q) psstd(q, mean=0, sd=1, nu=SSTD_nu, xi=SSTD_xi),
                     alternative = "two.sided")

print(ks_result)
# 
#         Asymptotic one-sample Kolmogorov-Smirnov test
# 
# data:  y
# D = 0.044892, p-value = 0.2951
# alternative hypothesis: two-sided

# Dall'analisi del Q-Q Plot e del test di K-S vediamo, quindi, che i residui standardizzati seguono la distribuzione skewed Student-t con un livello
# di significatività del 10%.

# Verifichiamo ora la stazionarietà e l'omoschedasticità (non condizionata).

# Grafico dei residui
df_residui <- data.frame(
  Date = index(z_hat),
  Residui = as.numeric(z_hat)
)

ggplot(df_residui, aes(x = Date, y = Residui)) +
  geom_line(color = "steelblue", linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  labs(
    title = "Grafico dei residui standardizzati",
    subtitle = "Modello GARCH(3,0) con distribuzione skewed Student-t per SPY",
    x = "Data",
    y = "Residui standardizzati"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    axis.title = element_text(size = 11)
  )

# KPSS Test
kpss_test_level(df_residui, col = "Residui", alpha = 0.01)
# 
# [Residui] KPSS Test - Stazionarietà attorno a una media costante
# Statistic: 0.126403
# Valore critico (circa 1%): 0.739000
# Interpretazione del p-value: p-value > 0.10
# 
# ** CONCLUSIONE: Statistic <= valore critico (1%) **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO stazionari.

# Quindi, i residui standardizzati sono stazionari.

# Per l'omoschedasticità non condizionata, per prima cosa, vediamo se c'è curtosi nei residui standardizzati.

# Calcola la curtosi
kurt_z_hat <- kurtosis(as.numeric(z_hat), na.rm = TRUE)

cat(sprintf("Curtosi dei residui standardizzati: %.4f\n", kurt_z_hat))
# Curtosi dei residui standardizzati: 6.7066

# La curtosi c'è. Quindi, applichiamo la forma "studentized" dei test di Breusch-Pagan e White per la stazionarietà in varianza.

# Breusch-Pagan Test:
# Crea un indice temporale (può essere anche un numero progressivo)
df_residui$Index <- seq_along(df_residui$Residui)

# Esegui il test BP
breusch_pagan_test(
  index = df_residui$Index,
  group = df_residui$Residui,
  col = "Residui standardizzati"
)
# [Residui standardizzati] Breusch-Pagan Test
# Statistic: 8.607082
# p-value: 0.00334858
# 
# ** Conclusione: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO eteroschedastici.

# White Test
white_test(
  index = df_residui$Index,
  group = df_residui$Residui,
  col = "Residui standardizzati"
)
# [Residui standardizzati] White Test
# Statistic: 11.455848
# p-value: 0.00325383
# 
# ** Conclusione: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO eteroschedastici.

# Dai risultati dei test possiamo rigettare l'ipotesi nulla di omoschedasticità incondizionata con un livello di significatività dell'1%. Quindi,
# c'è evidenza statistica di eteroschedasticità non condizionata.
# Perciò, possiamo concludere che anche questo modello è da scartare.

# Analizziamo il prossimo nella classifica: GARCH(3,1) sstd
p <- valid_models$p[12]
q <- valid_models$q[12]
dist <- valid_models$dist[12]

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = dist
)

fit <- tryCatch(
  ugarchfit(spec = spec, data = returns, solver = "hybrid"),
  error = function(e) NULL
)

# Risultati
show(fit)
# 
# *---------------------------------*
# *          GARCH Model Fit        *
# *---------------------------------*
#   
# Conditional Variance Dynamics 	
# -----------------------------------
# GARCH Model	    : sGARCH(3,1)
# Mean Model	    : ARFIMA(0,0,0)
# Distribution	  : sstd 
# 
# Optimal Parameters
# ------------------------------------
#         Estimate  Std. Error   t value Pr(>|t|)
# omega   0.116483    0.046033  2.530405 0.011393
# alpha1  0.035036    0.043790  0.800079 0.423665
# alpha2  0.000000    0.058165  0.000001 0.999999
# alpha3  0.162333    0.073083  2.221213 0.026337
# beta1   0.691876    0.076401  9.055880 0.000000
# skew    0.809349    0.050937 15.889176 0.000000
# shape   5.252397    1.362764  3.854224 0.000116
# 
# Robust Standard Errors:
#         Estimate  Std. Error   t value Pr(>|t|)
# omega   0.116483    0.030040  3.877598 0.000105
# alpha1  0.035036    0.042574  0.822933 0.410546
# alpha2  0.000000    0.063521  0.000001 0.999999
# alpha3  0.162333    0.079157  2.050766 0.040290
# beta1   0.691876    0.048782 14.182900 0.000000
# skew    0.809349    0.046200 17.518199 0.000000
# shape   5.252397    1.607268  3.267904 0.001083
# 
# LogLikelihood : -594.5994 
# 
# Information Criteria
# ------------------------------------
#   
# Akaike       2.5384
# Bayes        2.5998
# Shibata      2.5380
# Hannan-Quinn 2.5626
# 
# Weighted Ljung-Box Test on Standardized Residuals
# ------------------------------------
#                         statistic p-value
# Lag[1]                     0.2036  0.6518
# Lag[2*(p+q)+(p+q)-1][2]    0.2628  0.8168
# Lag[4*(p+q)+(p+q)-1][5]    0.7282  0.9174
# d.o.f=0
# H0 : No serial correlation
# 
# Weighted Ljung-Box Test on Standardized Squared Residuals
# ------------------------------------
#                          statistic   p-value
# Lag[1]                       11.68 0.0006334
# Lag[2*(p+q)+(p+q)-1][11]     18.87 0.0010484
# Lag[4*(p+q)+(p+q)-1][19]     21.00 0.0081228
# d.o.f=4
# 
# Weighted ARCH LM Tests
# ------------------------------------
#             Statistic Shape Scale P-Value
# ARCH Lag[5]    0.2879 0.500 2.000  0.5915
# ARCH Lag[7]    0.4143 1.473 1.746  0.9198
# ARCH Lag[9]    0.8982 2.402 1.619  0.9446
# 
# Nyblom stability test
# ------------------------------------
# Joint Statistic:  1.4535
# Individual Statistics:              
# omega  0.04246
# alpha1 0.04631
# alpha2 0.06821
# alpha3 0.05915
# beta1  0.03108
# skew   0.08626
# shape  0.32699
# 
# Asymptotic Critical Values (10% 5% 1%)
# Joint Statistic:     	   1.69 1.9 2.35
# Individual Statistic:	   0.35 0.47 0.75
# 
# Sign Bias Test
# ------------------------------------
#                    t-value    prob sig
# Sign Bias           0.7611 0.44697    
# Negative Sign Bias  2.5583 0.01083  **
# Positive Sign Bias  0.2765 0.78225    
# Joint Effect       13.3816 0.00388 ***
#   
#   
# Adjusted Pearson Goodness-of-Fit Test:
# ------------------------------------
#   group statistic p-value(g-1)
# 1    20     22.96      0.23902
# 2    30     37.27      0.13955
# 3    40     46.17      0.20012
# 4    50     67.35      0.04199
# 
# 
# Elapsed time : 0.1836801 

# Analizzando i p-value del test pesato di Ljung-Box sui residui standardizzati, osserviamo che questi sono tutti maggiori di α = 0.01
# (in realtà anche di α = 0.05 e α = 0.10). Quindi, non abbiamo evidenza statistica sufficiente per rifiutare l’ipotesi nulla di
# assenza di autocorrelazione.

# Applichiamo, ora, dei dest di normalità sui residui.

# Estrai residui standardizzati
z_hat <- residuals(fit, standardize = TRUE)

# Prendi le date del training_set:
# Rimuoviamo le righe con NA nella colonna usata per il modello
valid_dates <- training_set$Date[!is.na(training_set$SPY_LogReturn)]

# Converti in Date se non lo sono già
valid_dates <- as.Date(valid_dates)

# Sovrascrivi l’index di z_hat con le date corrette
index(z_hat) <- valid_dates

# Controlla che le lunghezze coincidano
if(length(z_hat) != length(valid_dates)) {
  stop("Errore: lunghezza residui e date non coincide!")
}

# Visualizza i primi valori
head(z_hat)
#                  [,1]
# 2023-08-01 -0.2715260
# 2023-08-02 -1.3272829
# 2023-08-03 -0.2719337
# 2023-08-04 -0.4769088
# 2023-08-07  0.8412142
# 2023-08-08 -0.4607418

# Test di SW
shapirowilk_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Shapiro-Wilk Test
# Statistic: 0.961570
# p-value  : 8.60267e-10
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Test di JB
jarque_bera_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Jarque-Bera Test
# Statistic: 187.181779
# p-value  : 0
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Come atteso, i residui standardizzati del GARCH(3,1) con distribuzione skewed Student-t non seguono una distribuzione normale.

# QQ-Plot SSTD:
# Residui standardizzati ordinati
y <- sort(as.numeric(z_hat))
n <- length(y)

# Parametri stimati da 'fit'
params <- coef(fit)
SSTD_xi <- params["skew"]
SSTD_nu <- params["shape"]
mean <- 0
sd <- 1

# Parametri della distribuzione skewed Student-t
distr <- "sstd"
distr_pars <- list(mean = mean, sd = sd, nu = SSTD_nu, xi = SSTD_xi)

# Quantili teorici
quants <- qsstd(ppoints(n), mean = mean, sd = sd, nu = SSTD_nu, xi = SSTD_xi)

# DataFrame per plotting
QQ_plot_df <- data.frame(
  T = 1:n,
  Q = quants,
  X = y,
  Y = y
)

# Calcolo retta interquartile
quart_probs <- c(0.25, 0.75)
quart_X <- as.vector(quantile(QQ_plot_df$X, quart_probs))
quart_Q <- qsstd(quart_probs, mean = mean, sd = sd, nu = SSTD_nu, xi = SSTD_xi)
slope <- diff(quart_X) / diff(quart_Q)
intercept <- quart_X[1] - slope * quart_Q[1]

# Titoli
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 MPSMF 2024-2025",
                             paste("Q-Q plot of the Standardized Residuals from the GARCH(3,1) model for SPY (skewed Student-t)")))
subtitle_content <- bquote(paste("Sample size: ", .(n), 
                                 "; SSTD parameters → mean = ", .(mean), 
                                 ", sd = ", .(sd), 
                                 ", df = ", .(round(SSTD_nu, 3)), 
                                 ", skew = ", .(round(SSTD_xi, 3)), "."))

caption_content <- "Author: Matteo Basili"
x_name <- bquote("Theoretical Quantiles (SSTD)")
y_name <- bquote("Standardized Residuals")

# Assi e legende
x_breaks <- seq(from = floor(min(QQ_plot_df$Q)), to = ceiling(max(QQ_plot_df$Q)), by = 0.5)
x_labs <- format(x_breaks, scientific = FALSE)
y_breaks_num <- length(x_breaks)
y_binwidth <- round((max(QQ_plot_df$Y) - min(QQ_plot_df$Y)) / y_breaks_num, digits = 3)
y_breaks <- round(seq(from = floor(min(QQ_plot_df$Y)/y_binwidth)*y_binwidth, 
                      to = ceiling(max(QQ_plot_df$Y)/y_binwidth)*y_binwidth, 
                      by = y_binwidth), 3)
y_labs <- format(y_breaks, scientific = FALSE)

# Legende e colori
leg_shape_labs <- bquote("Q-Q plot")
leg_fill_labs <- c(bquote("90% confidence interval"), bquote("95% confidence interval"))
leg_col_labs <- c(bquote("Interquartile line"), bquote("Regression line"), bquote("y = x"))

Stand_Res_std_QQ_plot <- ggplot(QQ_plot_df) +
  stat_qq_band(aes(sample = X, fill = "95"), distribution = distr, dparams = distr_pars, conf = 0.95) +
  stat_qq_band(aes(sample = X, fill = "90"), distribution = distr, dparams = distr_pars, conf = 0.90) +
  geom_abline(aes(slope = slope, intercept = intercept, colour = "IQR"), linewidth = 0.8) +
  stat_smooth(aes(x = Q, y = Y, colour = "Reg", group = 1), method = "lm", se = FALSE, linewidth = 0.8) +
  geom_abline(aes(slope = 1, intercept = 0, colour = "45deg"), linewidth = 0.8) +
  stat_qq_point(aes(sample = X), shape = 19, color = "black", size = 1.2, alpha = 0.8, distribution = distr, dparams = distr_pars) +
  scale_x_continuous(name = x_name, breaks = x_breaks, labels = x_labs) +
  scale_y_continuous(name = y_name, breaks = y_breaks, labels = NULL,
                     sec.axis = sec_axis(~ ., breaks = y_breaks, labels = y_labs)) +
  ggtitle(title_content) +
  labs(subtitle = subtitle_content, caption = caption_content) +
  scale_shape_manual(name = "", labels = leg_shape_labs, values = c("19")) +
  scale_fill_manual(name = "", values = c("90" = "chartreuse1", "95" = "deepskyblue1"), labels = leg_fill_labs) +
  scale_colour_manual(name = "", values = c("IQR" = "cyan", "Reg" = "red", "45deg" = "black"), labels = leg_col_labs) +
  guides(shape = guide_legend(order = 1), fill = guide_legend(order = 2), colour = guide_legend(order = 3)) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.x = element_text(angle = 0, vjust = 1),
    legend.key.width = unit(0.8, "cm"),
    legend.position = "bottom"
  )

# Mostra il plot
plot(Stand_Res_std_QQ_plot)

# Calcola densità teorica nei quantili (pdf)
dens <- dsstd(quants, mean=mean, sd=sd, nu=SSTD_nu, xi=SSTD_xi)

# Calcola errore standard dei quantili teorici (formula banda Q-Q)
# Errore standard: SE = (p * (1-p)) / (n * f(Q)^2)
p <- ppoints(n)
SE <- sqrt(p * (1-p) / (n * dens^2))

# Limiti di confidenza (95%)
alpha <- 0.05
z <- qnorm(1 - alpha/2)

lower_95 <- quants - z * SE
upper_95 <- quants + z * SE

# Verifica quanti punti osservati cadono dentro le bande
# Ricorda che QQ_plot_df$X = dati ordinati osservati

inside_95 <- (QQ_plot_df$X >= lower_95) & (QQ_plot_df$X <= upper_95)

percent_inside_95 <- mean(inside_95) * 100

cat(sprintf("Percentuale di punti dentro la banda di confidenza al 95%%: %.2f%%\n", percent_inside_95))
# Percentuale di punti dentro la banda di confidenza al 95%: 97.05%

# Alpha per il 90%
alpha_90 <- 0.10
z_90 <- qnorm(1 - alpha_90/2)  # quantile normale per il 90%

# Limiti di confidenza al 90%
lower_90 <- quants - z_90 * SE
upper_90 <- quants + z_90 * SE

# Verifica quanti punti osservati cadono dentro la banda 90%
inside_90 <- (QQ_plot_df$X >= lower_90) & (QQ_plot_df$X <= upper_90)

percent_inside_90 <- mean(inside_90) * 100

cat(sprintf("Percentuale di punti dentro la banda di confidenza al 90%%: %.2f%%\n", percent_inside_90))
# Percentuale di punti dentro la banda di confidenza al 90%: 89.03%

# Kolmogorov-Smirnov test (K-S test)
ks_result <- ks.test(y, 
                     y = function(q) psstd(q, mean=0, sd=1, nu=SSTD_nu, xi=SSTD_xi),
                     alternative = "two.sided")

print(ks_result)
# 
#         Asymptotic one-sample Kolmogorov-Smirnov test
# 
# data:  y
# D = 0.050764, p-value = 0.1737
# alternative hypothesis: two-sided

# Dall'analisi del Q-Q Plot e del test di K-S vediamo, quindi, che i residui standardizzati seguono la distribuzione skewed Student-t con un livello
# di significatività del 5% (in realtà il test di K-S risulta anche al 10%).

# Verifichiamo ora la stazionarietà e l'omoschedasticità (non condizionata).

# Grafico dei residui
df_residui <- data.frame(
  Date = index(z_hat),
  Residui = as.numeric(z_hat)
)

ggplot(df_residui, aes(x = Date, y = Residui)) +
  geom_line(color = "steelblue", linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  labs(
    title = "Grafico dei residui standardizzati",
    subtitle = "Modello GARCH(3,1) con distribuzione skewed Student-t per SPY",
    x = "Data",
    y = "Residui standardizzati"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    axis.title = element_text(size = 11)
  )

# KPSS Test
kpss_test_level(df_residui, col = "Residui", alpha = 0.01)
# 
# [Residui] KPSS Test - Stazionarietà attorno a una media costante
# Statistic: 0.139924
# Valore critico (circa 1%): 0.739000
# Interpretazione del p-value: p-value > 0.10
# 
# ** CONCLUSIONE: Statistic <= valore critico (1%) **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO stazionari.

# Quindi, i residui standardizzati sono stazionari.

# Per l'omoschedasticità non condizionata, per prima cosa, vediamo se c'è curtosi nei residui standardizzati.

# Calcola la curtosi
kurt_z_hat <- kurtosis(as.numeric(z_hat), na.rm = TRUE)

cat(sprintf("Curtosi dei residui standardizzati: %.4f\n", kurt_z_hat))
# Curtosi dei residui standardizzati: 5.7231

# La curtosi c'è. Quindi, applichiamo la forma "studentized" dei test di Breusch-Pagan e White per la stazionarietà in varianza.

# Breusch-Pagan Test:
# Crea un indice temporale (può essere anche un numero progressivo)
df_residui$Index <- seq_along(df_residui$Residui)

# Esegui il test BP
breusch_pagan_test(
  index = df_residui$Index,
  group = df_residui$Residui,
  col = "Residui standardizzati"
)
# [Residui standardizzati] Breusch-Pagan Test
# Statistic: 4.391529
# p-value: 0.0361179
# 
# ** Conclusione: p-value > 0.01 **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON sono eteroschedastici.

# White Test
white_test(
  index = df_residui$Index,
  group = df_residui$Residui,
  col = "Residui standardizzati"
)
# [Residui standardizzati] White Test
# Statistic: 5.455633
# p-value: 0.0653619
# 
# ** Conclusione: p-value > 0.01 **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON sono eteroschedastici.

# Dai risultati dei test si vede che è verificata anche la condizione di omoschedasticità (non condizionata).

# Per verificare ulteriormente l'assenza di autocorrelazione per i residui standardizzati
# di questo modello, facciamo anche l'ACF, il PACF e il test di Ljung-Box classico.

# ACF
df_z <- data.frame(z_hat = as.numeric(z_hat))
plot_acf_residuals(df_z, col = "z_hat")

# PACF
plot_pacf_residuals(df_z, col = "z_hat")

# L’analisi visiva di ACF e PACF mostra che, in entrambi i grafici, nessuna barra supera la banda di confidenza al 90%.
# Dunque, non c'è una forte evidenza visiva di autocorrelazione.

# Ljung-Box Test
ljungbox_test(as.numeric(z_hat), col_name = "Residui standardizzati")
# Ljung-Box test per Residui standardizzati (max_lag = 10):
#            lag   lb_stat lb_pvalue
# X-squared    1 0.2036205 0.6518142
# X-squared1   2 0.3219475 0.8513144
# X-squared2   3 1.0137314 0.7979294
# X-squared3   4 1.0297747 0.9052481
# X-squared4   5 1.0717965 0.9565695
# X-squared5   6 1.8547720 0.9325588
# X-squared6   7 2.3964224 0.9346926
# X-squared7   8 2.4130796 0.9656610
# X-squared8   9 2.9420261 0.9665491
# X-squared9  10 2.9583718 0.9823868

# Osservando i p-value associati ai vari lag, vediamo che tutti questi sono maggiori di α = 0.10. Dunque, non abbiamo evidenza statistica
# sufficiente per rifiutare l’ipotesi nulla di assenza di autocorrelazione fino al lag specificato.

# Infine, verifichiamo anche l’assenza di eteroschedasticità condizionata nei residui standardizzati.

# Test ARCH di Engle
arch_effects_test(z_hat, col_name = "Residui Standardizzati")
# ARCH Test per Residui Standardizzati (max_lag = 5):
#            lag arch_stat  arch_pvalue
# statistic    1  12.64659 0.0003762519
# statistic1   2  15.11532 0.0005220963
# statistic2   3  16.29171 0.0009880349
# statistic3   4  22.01780 0.0001987916
# statistic4   5  21.98976 0.0005259506

# Alla luce dei risultati, a partire dal lag 1, il test ARCH rivela evidenza significativa di eteroschedasticità residua, con p-value < 1%.
# Questo suggerisce che il modello GARCH(3,1) con distribuzione skewed Student-t non ha catturato completamente la dinamica della varianza
# condizionata, e che potrebbero essere presenti effetti ARCH residui a orizzonti più lunghi.
# Perciò, possiamo concludere che anche questo modello è da scartare.

# Analizziamo il prossimo nella classifica: GARCH(3,1) sged
p <- valid_models$p[13]
q <- valid_models$q[13]
dist <- valid_models$dist[13]

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = dist
)

fit <- tryCatch(
  ugarchfit(spec = spec, data = returns, solver = "hybrid"),
  error = function(e) NULL
)

# Risultati
show(fit)
# 
# *---------------------------------*
# *          GARCH Model Fit        *
# *---------------------------------*
#   
# Conditional Variance Dynamics 	
# -----------------------------------
# GARCH Model	    : sGARCH(3,1)
# Mean Model	    : ARFIMA(0,0,0)
# Distribution	  : sged 
# 
# Optimal Parameters
# ------------------------------------
#         Estimate  Std. Error  t value Pr(>|t|)
# omega   0.098638    0.039715   2.4837 0.013004
# alpha1  0.047265    0.031163   1.5167 0.129335
# alpha2  0.000000    0.015255   0.0000 1.000000
# alpha3  0.153984    0.059555   2.5856 0.009722
# beta1   0.695453    0.070487   9.8664 0.000000
# skew    0.856695    0.059504  14.3972 0.000000
# shape   1.274538    0.118682  10.7391 0.000000
# 
# Robust Standard Errors:
#         Estimate  Std. Error   t value Pr(>|t|)
# omega   0.098638    0.029525  3.340814 0.000835
# alpha1  0.047265    0.024857  1.901500 0.057237
# alpha2  0.000000    0.003745  0.000001 0.999999
# alpha3  0.153984    0.053868  2.858573 0.004256
# beta1   0.695453    0.050689 13.719893 0.000000
# skew    0.856695    0.088035  9.731353 0.000000
# shape   1.274538    0.172360  7.394623 0.000000
# 
# LogLikelihood : -595.6967 
# 
# Information Criteria
# ------------------------------------
#   
# Akaike       2.5430
# Bayes        2.6045
# Shibata      2.5426
# Hannan-Quinn 2.5672
# 
# Weighted Ljung-Box Test on Standardized Residuals
# ------------------------------------
#                         statistic p-value
# Lag[1]                     0.1156  0.7339
# Lag[2*(p+q)+(p+q)-1][2]    0.1575  0.8810
# Lag[4*(p+q)+(p+q)-1][5]    0.5557  0.9488
# d.o.f=0
# H0 : No serial correlation
# 
# Weighted Ljung-Box Test on Standardized Squared Residuals
# ------------------------------------
#                          statistic  p-value
# Lag[1]                       6.999 0.008156
# Lag[2*(p+q)+(p+q)-1][11]    13.709 0.016347
# Lag[4*(p+q)+(p+q)-1][19]    15.935 0.068905
# d.o.f=4
# 
# Weighted ARCH LM Tests
# ------------------------------------
#             Statistic Shape Scale P-Value
# ARCH Lag[5]    0.1439 0.500 2.000  0.7044
# ARCH Lag[7]    0.3118 1.473 1.746  0.9454
# ARCH Lag[9]    0.9218 2.402 1.619  0.9417
# 
# Nyblom stability test
# ------------------------------------
# Joint Statistic:  1.688
# Individual Statistics:              
# omega  0.09843
# alpha1 0.15357
# alpha2 0.28165
# alpha3 0.20164
# beta1  0.10072
# skew   0.30929
# shape  0.46689
# 
# Asymptotic Critical Values (10% 5% 1%)
# Joint Statistic:     	   1.69 1.9 2.35
# Individual Statistic:	   0.35 0.47 0.75
# 
# Sign Bias Test
# ------------------------------------
#                    t-value   prob sig
# Sign Bias           1.0452 0.2964    
# Negative Sign Bias  1.8482 0.0652   *
# Positive Sign Bias  0.1211 0.9037    
# Joint Effect       10.0916 0.0178  **
#   
#   
# Adjusted Pearson Goodness-of-Fit Test:
# ------------------------------------
#   group statistic p-value(g-1)
# 1    20     26.76     0.110381
# 2    30     50.30     0.008374
# 3    40     51.91     0.080861
# 4    50     64.61     0.066738
# 
# 
# Elapsed time : 0.3509951 

# Analizzando i p-value del test pesato di Ljung-Box sui residui standardizzati, osserviamo che questi sono tutti maggiori di α = 0.01
# (in realtà anche di α = 0.05 e α = 0.10). Quindi, non abbiamo evidenza statistica sufficiente per rifiutare l’ipotesi nulla di assenza di
# autocorrelazione.

# Applichiamo, ora, dei dest di normalità sui residui.

# Estrai residui standardizzati
z_hat <- residuals(fit, standardize = TRUE)

# Prendi le date del training_set:
# Rimuoviamo le righe con NA nella colonna usata per il modello
valid_dates <- training_set$Date[!is.na(training_set$SPY_LogReturn)]

# Converti in Date se non lo sono già
valid_dates <- as.Date(valid_dates)

# Sovrascrivi l’index di z_hat con le date corrette
index(z_hat) <- valid_dates

# Controlla che le lunghezze coincidano
if(length(z_hat) != length(valid_dates)) {
  stop("Errore: lunghezza residui e date non coincide!")
}

# Visualizza i primi valori
head(z_hat)
#                  [,1]
# 2023-08-01 -0.2715260
# 2023-08-02 -1.3272829
# 2023-08-03 -0.2719337
# 2023-08-04 -0.4805232
# 2023-08-07  0.8565601
# 2023-08-08 -0.4691642

# Test di SW
shapirowilk_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Shapiro-Wilk Test
# Statistic: 0.963842
# p-value  : 2.10109e-09
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Test di JB
jarque_bera_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Jarque-Bera Test
# Statistic: 166.703118
# p-value  : 0
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Come atteso, i residui standardizzati del GARCH(3,1) con distribuzione skewed GED non seguono una distribuzione normale.

# QQ-Plot SGED:
# Residui standardizzati ordinati
y <- sort(as.numeric(z_hat))
n <- length(y)

# Parametri stimati da 'fit'
params <- coef(fit)
SGED_xi <- params["skew"]
SGED_nu <- params["shape"]
mean <- 0
sd <- 1

# Parametri della distribuzione skewed GED
distr <- "sged"
distr_pars <- list(mean = mean, sd = sd, nu = SGED_nu, xi = SGED_xi)

# Quantili teorici
quants <- qsged(ppoints(n), mean = mean, sd = sd, nu = SGED_nu, xi = SGED_xi)

# DataFrame per plotting
QQ_plot_df <- data.frame(
  T = 1:n,
  Q = quants,
  X = y,
  Y = y
)

# Calcolo retta interquartile
quart_probs <- c(0.25, 0.75)
quart_X <- as.vector(quantile(QQ_plot_df$X, quart_probs))
quart_Q <- qsged(quart_probs, mean = mean, sd = sd, nu = SGED_nu, xi = SGED_xi)
slope <- diff(quart_X) / diff(quart_Q)
intercept <- quart_X[1] - slope * quart_Q[1]

# Titoli
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 MPSMF 2024-2025",
                             paste("Q-Q plot of the Standardized Residuals from the GARCH(3,1) model for SPY (skewed GED)")))
subtitle_content <- bquote(paste("Sample size: ", .(n), 
                                 "; SGED parameters → mean = ", .(mean), 
                                 ", sd = ", .(sd), 
                                 ", shape = ", .(round(SGED_nu, 3)), 
                                 ", skew = ", .(round(SGED_xi, 3)), "."))

caption_content <- "Author: Matteo Basili"
x_name <- bquote("Theoretical Quantiles (SGED)")
y_name <- bquote("Standardized Residuals")

# Assi e legende
x_breaks <- seq(from = floor(min(QQ_plot_df$Q)), to = ceiling(max(QQ_plot_df$Q)), by = 0.5)
x_labs <- format(x_breaks, scientific = FALSE)
y_breaks_num <- length(x_breaks)
y_binwidth <- round((max(QQ_plot_df$Y) - min(QQ_plot_df$Y)) / y_breaks_num, digits = 3)
y_breaks <- round(seq(from = floor(min(QQ_plot_df$Y)/y_binwidth)*y_binwidth, 
                      to = ceiling(max(QQ_plot_df$Y)/y_binwidth)*y_binwidth, 
                      by = y_binwidth), 3)
y_labs <- format(y_breaks, scientific = FALSE)

# Legende e colori
leg_shape_labs <- bquote("Q-Q plot")
leg_fill_labs <- c(bquote("90% confidence interval"), bquote("95% confidence interval"))
leg_col_labs <- c(bquote("Interquartile line"), bquote("Regression line"), bquote("y = x"))

Stand_Res_std_QQ_plot <- ggplot(QQ_plot_df) + 
  stat_qq_band(aes(sample = X, fill = "95"), distribution = distr, dparams = distr_pars, conf = 0.95) +
  stat_qq_band(aes(sample = X, fill = "90"), distribution = distr, dparams = distr_pars, conf = 0.90) +
  geom_abline(aes(slope = slope, intercept = intercept, colour = "IQR"), linewidth = 0.8) +
  stat_smooth(aes(x = Q, y = Y, colour = "Reg", group = 1), method = "lm", se = FALSE, linewidth = 0.8) +
  geom_abline(aes(slope = 1, intercept = 0, colour = "45deg"), linewidth = 0.8) +
  stat_qq_point(aes(sample = X), shape = 19, color = "black", size = 1.2, alpha = 0.8, distribution = distr, dparams = distr_pars) +
  scale_x_continuous(name = x_name, breaks = x_breaks, labels = x_labs) +
  scale_y_continuous(name = y_name, breaks = y_breaks, labels = NULL,
                     sec.axis = sec_axis(~ ., breaks = y_breaks, labels = y_labs)) +
  ggtitle(title_content) +
  labs(subtitle = subtitle_content, caption = caption_content) +
  scale_shape_manual(name = "", labels = leg_shape_labs, values = c("19")) +
  scale_fill_manual(name = "", values = c("90" = "chartreuse1", "95" = "deepskyblue1"), labels = leg_fill_labs) +
  scale_colour_manual(name = "", values = c("IQR" = "cyan", "Reg" = "red", "45deg" = "black"), labels = leg_col_labs) +
  guides(shape = guide_legend(order = 1), fill = guide_legend(order = 2), colour = guide_legend(order = 3)) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.x = element_text(angle = 0, vjust = 1),
    legend.key.width = unit(0.8, "cm"),
    legend.position = "bottom"
  )

# Mostra il plot
plot(Stand_Res_std_QQ_plot)

# Calcola densità teorica nei quantili (SGED)
dens <- dsged(quants, mean = mean, sd = sd, nu = SGED_nu, xi = SGED_xi)

# Probabilità associate ai quantili
p <- ppoints(n)

# Errore standard dei quantili teorici
SE <- sqrt(p * (1 - p) / (n * dens^2))

# Limiti di confidenza al 95%
alpha_95 <- 0.05
z_95 <- qnorm(1 - alpha_95 / 2)
lower_95 <- quants - z_95 * SE
upper_95 <- quants + z_95 * SE

# Percentuale di punti dentro la banda 95%
inside_95 <- (QQ_plot_df$X >= lower_95) & (QQ_plot_df$X <= upper_95)
percent_inside_95 <- mean(inside_95) * 100
cat(sprintf("Percentuale di punti dentro la banda di confidenza al 95%%: %.2f%%\n", percent_inside_95))
# Percentuale di punti dentro la banda di confidenza al 95%: 98.31%

# Limiti di confidenza al 90%
alpha_90 <- 0.10
z_90 <- qnorm(1 - alpha_90 / 2)
lower_90 <- quants - z_90 * SE
upper_90 <- quants + z_90 * SE

# Percentuale di punti dentro la banda 90%
inside_90 <- (QQ_plot_df$X >= lower_90) & (QQ_plot_df$X <= upper_90)
percent_inside_90 <- mean(inside_90) * 100
cat(sprintf("Percentuale di punti dentro la banda di confidenza al 90%%: %.2f%%\n", percent_inside_90))
# Percentuale di punti dentro la banda di confidenza al 90%: 88.82%

# Kolmogorov-Smirnov test (K-S test)
ks_result <- ks.test(y, 
                     y = function(q) psged(q, mean=0, sd=1, nu=SGED_nu, xi=SGED_xi),
                     alternative = "two.sided")

print(ks_result)
# 
#         Asymptotic one-sample Kolmogorov-Smirnov test
# 
# data:  y
# D = 0.048893, p-value = 0.2072
# alternative hypothesis: two-sided

# Dall'analisi del Q-Q Plot e del test di K-S vediamo, quindi, che i residui standardizzati seguono la distribuzione skewed GED con un livello
# di significatività del 5% (in realtà il test di K-S risulta anche al 10%).

# Verifichiamo ora la stazionarietà e l'omoschedasticità (non condizionata).

# Grafico dei residui
df_residui <- data.frame(
  Date = index(z_hat),
  Residui = as.numeric(z_hat)
)

ggplot(df_residui, aes(x = Date, y = Residui)) +
  geom_line(color = "steelblue", linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  labs(
    title = "Grafico dei residui standardizzati",
    subtitle = "Modello GARCH(3,1) con distribuzione skewed GED per SPY",
    x = "Data",
    y = "Residui standardizzati"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    axis.title = element_text(size = 11)
  )

# KPSS Test
kpss_test_level(df_residui, col = "Residui", alpha = 0.01)
# 
# [Residui] KPSS Test - Stazionarietà attorno a una media costante
# Statistic: 0.140033
# Valore critico (circa 1%): 0.739000
# Interpretazione del p-value: p-value > 0.10
# 
# ** CONCLUSIONE: Statistic <= valore critico (1%) **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO stazionari.

# Quindi, i residui standardizzati sono stazionari.

# Per l'omoschedasticità non condizionata, per prima cosa, vediamo se c'è curtosi nei residui standardizzati.

# Calcola la curtosi
kurt_z_hat <- kurtosis(as.numeric(z_hat), na.rm = TRUE)

cat(sprintf("Curtosi dei residui standardizzati: %.4f\n", kurt_z_hat))
# Curtosi dei residui standardizzati: 5.5543

# La curtosi c'è. Quindi, applichiamo la forma "studentized" dei test di Breusch-Pagan e White per la stazionarietà in varianza.

# Breusch-Pagan Test:
# Crea un indice temporale (può essere anche un numero progressivo)
df_residui$Index <- seq_along(df_residui$Residui)

# Esegui il test BP
breusch_pagan_test(
  index = df_residui$Index,
  group = df_residui$Residui,
  col = "Residui standardizzati"
)
# [Residui standardizzati] Breusch-Pagan Test
# Statistic: 3.851028
# p-value: 0.0497155
# 
# ** Conclusione: p-value > 0.01 **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON sono eteroschedastici.

# White Test
white_test(
  index = df_residui$Index,
  group = df_residui$Residui,
  col = "Residui standardizzati"
)
# [Residui standardizzati] White Test
# Statistic: 4.674606
# p-value: 0.0965878
# 
# ** Conclusione: p-value > 0.01 **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON sono eteroschedastici.

# Dai risultati dei test si vede che è verificata anche la condizione di omoschedasticità (non condizionata).

# Per verificare ulteriormente l'assenza di autocorrelazione per i residui standardizzati
# di questo modello, facciamo anche l'ACF, il PACF e il test di Ljung-Box classico.

# ACF
df_z <- data.frame(z_hat = as.numeric(z_hat))
plot_acf_residuals(df_z, col = "z_hat")

# PACF
plot_pacf_residuals(df_z, col = "z_hat")

# L’analisi visiva di ACF e PACF mostra che, in entrambi i grafici, nessuna barra supera la banda di confidenza al 90%.
# Dunque, non c'è una forte evidenza visiva di autocorrelazione.

# Ljung-Box Test
ljungbox_test(as.numeric(z_hat), col_name = "Residui standardizzati")
# Ljung-Box test per Residui standardizzati (max_lag = 10):
#            lag   lb_stat lb_pvalue
# X-squared    1 0.1155779 0.7338810
# X-squared1   2 0.1995161 0.9050564
# X-squared2   3 0.7854043 0.8529549
# X-squared3   4 0.7977541 0.9387489
# X-squared4   5 0.8801423 0.9716426
# X-squared5   6 1.7135620 0.9440695
# X-squared6   7 2.2546535 0.9444128
# X-squared7   8 2.2738442 0.9714446
# X-squared8   9 2.7686357 0.9727659
# X-squared9  10 2.7936315 0.9858720

# Osservando i p-value associati ai vari lag, vediamo che tutti questi sono maggiori di α = 0.10. Dunque, non abbiamo evidenza statistica
# sufficiente per rifiutare l’ipotesi nulla di assenza di autocorrelazione fino al lag specificato.

# Infine, verifichiamo anche l’assenza di eteroschedasticità condizionata nei residui standardizzati.

# Test ARCH di Engle
arch_effects_test(z_hat, col_name = "Residui Standardizzati")
# ARCH Test per Residui Standardizzati (max_lag = 5):
#            lag arch_stat arch_pvalue
# statistic    1  7.798265 0.005229643
# statistic1   2  9.883236 0.007143031
# statistic2   3 10.240157 0.016631241
# statistic3   4 16.358222 0.002574292
# statistic4   5 16.346225 0.005922059

# Alla luce dei risultati, per i lag 1, 2, 4 e 5 il test ARCH rivela evidenza significativa di eteroschedasticità residua, con p-value < 1%.
# Questo suggerisce che il modello GARCH(3,1) con distribuzione skewed GED non ha catturato completamente la dinamica della varianza
# condizionata, e che potrebbero essere presenti effetti ARCH residui (anche) a orizzonti più lunghi.
# Perciò, possiamo concludere che anche questo modello è da scartare.

# Analizziamo il prossimo nella classifica: GARCH(0,2) sstd
p <- valid_models$p[14]
q <- valid_models$q[14]
dist <- valid_models$dist[14]

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = dist
)

fit <- tryCatch(
  ugarchfit(spec = spec, data = returns, solver = "hybrid"),
  error = function(e) NULL
)

# Risultati
show(fit)
# 
# *---------------------------------*
# *          GARCH Model Fit        *
# *---------------------------------*
#   
# Conditional Variance Dynamics 	
# -----------------------------------
# GARCH Model	    : sGARCH(0,2)
# Mean Model	    : ARFIMA(0,0,0)
# Distribution	  : sstd 
# 
# Optimal Parameters
# ------------------------------------
#        Estimate  Std. Error    t value Pr(>|t|)
# omega  0.004014    0.002897 1.3855e+00  0.16590
# beta1  0.000002    0.002089 1.0150e-03  0.99919
# beta2  0.998998    0.000288 3.4701e+03  0.00000
# skew   0.843382    0.045248 1.8639e+01  0.00000
# shape  2.861323    0.252999 1.1310e+01  0.00000
# 
# Robust Standard Errors:
#        Estimate  Std. Error    t value Pr(>|t|)
# omega  0.004014    0.002447 1.6408e+00  0.10084
# beta1  0.000002    0.001176 1.8030e-03  0.99856
# beta2  0.998998    0.000563 1.7737e+03  0.00000
# skew   0.843382    0.041121 2.0510e+01  0.00000
# shape  2.861323    0.349082 8.1967e+00  0.00000
# 
# LogLikelihood : -613.6477 
# 
# Information Criteria
# ------------------------------------
#   
# Akaike       2.6103
# Bayes        2.6542
# Shibata      2.6101
# Hannan-Quinn 2.6276
# 
# Weighted Ljung-Box Test on Standardized Residuals
# ------------------------------------
#                         statistic p-value
# Lag[1]                      1.455 0.22773
# Lag[2*(p+q)+(p+q)-1][2]     2.813 0.15772
# Lag[4*(p+q)+(p+q)-1][5]     7.235 0.04546
# d.o.f=0
# H0 : No serial correlation
# 
# Weighted Ljung-Box Test on Standardized Squared Residuals
# ------------------------------------
#                         statistic   p-value
# Lag[1]                      24.40 7.825e-07
# Lag[2*(p+q)+(p+q)-1][5]     58.79 2.220e-16
# Lag[4*(p+q)+(p+q)-1][9]     75.14 0.000e+00
# d.o.f=2
# 
# Weighted ARCH LM Tests
# ------------------------------------
#             Statistic Shape Scale   P-Value
# ARCH Lag[3]     36.44 0.500 2.000 1.572e-09
# ARCH Lag[5]     61.13 1.440 1.667 6.661e-16
# ARCH Lag[7]     63.39 2.315 1.543 2.220e-16
# 
# Nyblom stability test
# ------------------------------------
# Joint Statistic:  5.3961
# Individual Statistics:             
# omega 0.47664
# beta1 0.43589
# beta2 0.43541
# skew  0.03962
# shape 0.16464
# 
# Asymptotic Critical Values (10% 5% 1%)
# Joint Statistic:     	   1.28 1.47 1.88
# Individual Statistic:	   0.35 0.47 0.75
# 
# Sign Bias Test
# ------------------------------------
#                    t-value      prob sig
# Sign Bias            0.510 6.103e-01    
# Negative Sign Bias   4.089 5.096e-05 ***
# Positive Sign Bias   2.858 4.452e-03 ***
# Joint Effect        28.418 2.968e-06 ***
#   
#   
# Adjusted Pearson Goodness-of-Fit Test:
# ------------------------------------
#   group statistic p-value(g-1)
# 1    20     29.29     0.061541
# 2    30     49.92     0.009203
# 3    40     56.63     0.033667
# 4    50     67.77     0.038991
# 
# 
# Elapsed time : 0.144757

# Analizzando i p-value del test pesato di Ljung-Box sui residui standardizzati, osserviamo che questi sono tutti maggiori di α = 0.01.
# Quindi, non abbiamo evidenza statistica sufficiente per rifiutare l’ipotesi nulla di assenza di autocorrelazione.

# Applichiamo, ora, dei dest di normalità sui residui.

# Estrai residui standardizzati
z_hat <- residuals(fit, standardize = TRUE)

# Prendi le date del training_set:
# Rimuoviamo le righe con NA nella colonna usata per il modello
valid_dates <- training_set$Date[!is.na(training_set$SPY_LogReturn)]

# Converti in Date se non lo sono già
valid_dates <- as.Date(valid_dates)

# Sovrascrivi l’index di z_hat con le date corrette
index(z_hat) <- valid_dates

# Controlla che le lunghezze coincidano
if(length(z_hat) != length(valid_dates)) {
  stop("Errore: lunghezza residui e date non coincide!")
}

# Visualizza i primi valori
head(z_hat)
#                  [,1]
# 2023-08-01 -0.2715260
# 2023-08-02 -1.3272829
# 2023-08-03 -0.2715804
# 2023-08-04 -0.4289266
# 2023-08-07  0.8212799
# 2023-08-08 -0.4118624

# Test di SW
shapirowilk_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Shapiro-Wilk Test
# Statistic: 0.873785
# p-value  : 3.53398e-19
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Test di JB
jarque_bera_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Jarque-Bera Test
# Statistic: 5174.764876
# p-value  : 0
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Come atteso, i residui standardizzati del GARCH(0,2) con distribuzione skewed Student-t non seguono una distribuzione normale.

# QQ-Plot SSTD:
# Residui standardizzati ordinati
y <- sort(as.numeric(z_hat))
n <- length(y)

# Parametri stimati da 'fit'
params <- coef(fit)
SSTD_xi <- params["skew"]
SSTD_nu <- params["shape"]
mean <- 0
sd <- 1

# Parametri della distribuzione skewed Student-t
distr <- "sstd"
distr_pars <- list(mean = mean, sd = sd, nu = SSTD_nu, xi = SSTD_xi)

# Quantili teorici
quants <- qsstd(ppoints(n), mean = mean, sd = sd, nu = SSTD_nu, xi = SSTD_xi)

# DataFrame per plotting
QQ_plot_df <- data.frame(
  T = 1:n,
  Q = quants,
  X = y,
  Y = y
)

# Calcolo retta interquartile
quart_probs <- c(0.25, 0.75)
quart_X <- as.vector(quantile(QQ_plot_df$X, quart_probs))
quart_Q <- qsstd(quart_probs, mean = mean, sd = sd, nu = SSTD_nu, xi = SSTD_xi)
slope <- diff(quart_X) / diff(quart_Q)
intercept <- quart_X[1] - slope * quart_Q[1]

# Titoli
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 MPSMF 2024-2025",
                             paste("Q-Q plot of the Standardized Residuals from the GARCH(0,2) model for SPY (skewed Student-t)")))
subtitle_content <- bquote(paste("Sample size: ", .(n), 
                                 "; SSTD parameters → mean = ", .(mean), 
                                 ", sd = ", .(sd), 
                                 ", df = ", .(round(SSTD_nu, 3)), 
                                 ", skew = ", .(round(SSTD_xi, 3)), "."))

caption_content <- "Author: Matteo Basili"
x_name <- bquote("Theoretical Quantiles (SSTD)")
y_name <- bquote("Standardized Residuals")

# Assi e legende
x_breaks <- seq(from = floor(min(QQ_plot_df$Q)), to = ceiling(max(QQ_plot_df$Q)), by = 0.5)
x_labs <- format(x_breaks, scientific = FALSE)
y_breaks_num <- length(x_breaks)
y_binwidth <- round((max(QQ_plot_df$Y) - min(QQ_plot_df$Y)) / y_breaks_num, digits = 3)
y_breaks <- round(seq(from = floor(min(QQ_plot_df$Y)/y_binwidth)*y_binwidth, 
                      to = ceiling(max(QQ_plot_df$Y)/y_binwidth)*y_binwidth, 
                      by = y_binwidth), 3)
y_labs <- format(y_breaks, scientific = FALSE)

# Legende e colori
leg_shape_labs <- bquote("Q-Q plot")
leg_fill_labs <- c(bquote("90% confidence interval"), bquote("95% confidence interval"))
leg_col_labs <- c(bquote("Interquartile line"), bquote("Regression line"), bquote("y = x"))

Stand_Res_std_QQ_plot <- ggplot(QQ_plot_df) +
  stat_qq_band(aes(sample = X, fill = "95"), distribution = distr, dparams = distr_pars, conf = 0.95) +
  stat_qq_band(aes(sample = X, fill = "90"), distribution = distr, dparams = distr_pars, conf = 0.90) +
  geom_abline(aes(slope = slope, intercept = intercept, colour = "IQR"), linewidth = 0.8) +
  stat_smooth(aes(x = Q, y = Y, colour = "Reg", group = 1), method = "lm", se = FALSE, linewidth = 0.8) +
  geom_abline(aes(slope = 1, intercept = 0, colour = "45deg"), linewidth = 0.8) +
  stat_qq_point(aes(sample = X), shape = 19, color = "black", size = 1.2, alpha = 0.8, distribution = distr, dparams = distr_pars) +
  scale_x_continuous(name = x_name, breaks = x_breaks, labels = x_labs) +
  scale_y_continuous(name = y_name, breaks = y_breaks, labels = NULL,
                     sec.axis = sec_axis(~ ., breaks = y_breaks, labels = y_labs)) +
  ggtitle(title_content) +
  labs(subtitle = subtitle_content, caption = caption_content) +
  scale_shape_manual(name = "", labels = leg_shape_labs, values = c("19")) +
  scale_fill_manual(name = "", values = c("90" = "chartreuse1", "95" = "deepskyblue1"), labels = leg_fill_labs) +
  scale_colour_manual(name = "", values = c("IQR" = "cyan", "Reg" = "red", "45deg" = "black"), labels = leg_col_labs) +
  guides(shape = guide_legend(order = 1), fill = guide_legend(order = 2), colour = guide_legend(order = 3)) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.x = element_text(angle = 0, vjust = 1),
    legend.key.width = unit(0.8, "cm"),
    legend.position = "bottom"
  )

# Mostra il plot
plot(Stand_Res_std_QQ_plot)

# Calcola densità teorica nei quantili (pdf)
dens <- dsstd(quants, mean=mean, sd=sd, nu=SSTD_nu, xi=SSTD_xi)

# Calcola errore standard dei quantili teorici (formula banda Q-Q)
# Errore standard: SE = (p * (1-p)) / (n * f(Q)^2)
p <- ppoints(n)
SE <- sqrt(p * (1-p) / (n * dens^2))

# Limiti di confidenza (95%)
alpha <- 0.05
z <- qnorm(1 - alpha/2)

lower_95 <- quants - z * SE
upper_95 <- quants + z * SE

# Verifica quanti punti osservati cadono dentro le bande
# Ricorda che QQ_plot_df$X = dati ordinati osservati

inside_95 <- (QQ_plot_df$X >= lower_95) & (QQ_plot_df$X <= upper_95)

percent_inside_95 <- mean(inside_95) * 100

cat(sprintf("Percentuale di punti dentro la banda di confidenza al 95%%: %.2f%%\n", percent_inside_95))
# Percentuale di punti dentro la banda di confidenza al 95%: 100.00%

# Alpha per il 90%
alpha_90 <- 0.10
z_90 <- qnorm(1 - alpha_90/2)  # quantile normale per il 90%

# Limiti di confidenza al 90%
lower_90 <- quants - z_90 * SE
upper_90 <- quants + z_90 * SE

# Verifica quanti punti osservati cadono dentro la banda 90%
inside_90 <- (QQ_plot_df$X >= lower_90) & (QQ_plot_df$X <= upper_90)

percent_inside_90 <- mean(inside_90) * 100

cat(sprintf("Percentuale di punti dentro la banda di confidenza al 90%%: %.2f%%\n", percent_inside_90))
# Percentuale di punti dentro la banda di confidenza al 90%: 96.41%

# Kolmogorov-Smirnov test (K-S test)
ks_result <- ks.test(y, 
                     y = function(q) psstd(q, mean=0, sd=1, nu=SSTD_nu, xi=SSTD_xi),
                     alternative = "two.sided")

print(ks_result)
# 
#         Asymptotic one-sample Kolmogorov-Smirnov test
# 
# data:  y
# D = 0.040722, p-value = 0.4115
# alternative hypothesis: two-sided

# Dall'analisi del Q-Q Plot e del test di K-S vediamo, quindi, che i residui standardizzati seguono la distribuzione skewed Student-t con un livello
# di significatività del 10%.

# Verifichiamo ora la stazionarietà e l'omoschedasticità (non condizionata).

# Grafico dei residui
df_residui <- data.frame(
  Date = index(z_hat),
  Residui = as.numeric(z_hat)
)

ggplot(df_residui, aes(x = Date, y = Residui)) +
  geom_line(color = "steelblue", linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  labs(
    title = "Grafico dei residui standardizzati",
    subtitle = "Modello GARCH(0,2) con distribuzione skewed Student-t per SPY",
    x = "Data",
    y = "Residui standardizzati"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    axis.title = element_text(size = 11)
  )

# KPSS Test
kpss_test_level(df_residui, col = "Residui", alpha = 0.01)
# 
# [Residui] KPSS Test - Stazionarietà attorno a una media costante
# Statistic: 0.074890
# Valore critico (circa 1%): 0.739000
# Interpretazione del p-value: p-value > 0.10
# 
# ** CONCLUSIONE: Statistic <= valore critico (1%) **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO stazionari.

# Quindi, i residui standardizzati sono stazionari.

# Per l'omoschedasticità non condizionata, per prima cosa, vediamo se c'è curtosi nei residui standardizzati.

# Calcola la curtosi
kurt_z_hat <- kurtosis(as.numeric(z_hat), na.rm = TRUE)

cat(sprintf("Curtosi dei residui standardizzati: %.4f\n", kurt_z_hat))
# Curtosi dei residui standardizzati: 19.1305

# La curtosi c'è. Quindi, applichiamo la forma "studentized" dei test di Breusch-Pagan e White per la stazionarietà in varianza.

# Breusch-Pagan Test:
# Crea un indice temporale (può essere anche un numero progressivo)
df_residui$Index <- seq_along(df_residui$Residui)

# Esegui il test BP
breusch_pagan_test(
  index = df_residui$Index,
  group = df_residui$Residui,
  col = "Residui standardizzati"
)
# [Residui standardizzati] Breusch-Pagan Test
# Statistic: 6.837421
# p-value: 0.00892676
# 
# ** Conclusione: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO eteroschedastici.

# White Test
white_test(
  index = df_residui$Index,
  group = df_residui$Residui,
  col = "Residui standardizzati"
)
# [Residui standardizzati] White Test
# Statistic: 10.623561
# p-value: 0.00493314
# 
# ** Conclusione: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO eteroschedastici.

# Dai risultati dei test possiamo rigettare l'ipotesi nulla di omoschedasticità incondizionata con un livello di significatività dell'1%. Quindi,
# c'è evidenza statistica di eteroschedasticità non condizionata.
# Perciò, possiamo concludere che anche questo modello è da scartare.

# Analizziamo il prossimo nella classifica: GARCH(2,1) norm
p <- valid_models$p[15]
q <- valid_models$q[15]
dist <- valid_models$dist[15]

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = dist
)

fit <- tryCatch(
  ugarchfit(spec = spec, data = returns, solver = "hybrid"),
  error = function(e) NULL
)

# Risultati
show(fit)
# 
# *---------------------------------*
# *          GARCH Model Fit        *
# *---------------------------------*
#   
# Conditional Variance Dynamics 	
# -----------------------------------
# GARCH Model	    : sGARCH(2,1)
# Mean Model	    : ARFIMA(0,0,0)
# Distribution	  : norm 
# 
# Optimal Parameters
# ------------------------------------
#         Estimate  Std. Error  t value Pr(>|t|)
# omega   0.058821    0.022447  2.62046 0.008781
# alpha1  0.105144    0.042681  2.46349 0.013759
# alpha2  0.028057    0.052367  0.53579 0.592106
# beta1   0.806491    0.045730 17.63601 0.000000
# 
# Robust Standard Errors:
#         Estimate  Std. Error  t value Pr(>|t|)
# omega   0.058821    0.034891  1.68584 0.091827
# alpha1  0.105144    0.052986  1.98438 0.047213
# alpha2  0.028057    0.044982  0.62375 0.532794
# beta1   0.806491    0.058664 13.74769 0.000000
# 
# LogLikelihood : -621.3596 
# 
# Information Criteria
# ------------------------------------
#   
# Akaike       2.6386
# Bayes        2.6738
# Shibata      2.6385
# Hannan-Quinn 2.6525
# 
# Weighted Ljung-Box Test on Standardized Residuals
# ------------------------------------
#                         statistic p-value
# Lag[1]                   0.007059  0.9330
# Lag[2*(p+q)+(p+q)-1][2]  0.040325  0.9639
# Lag[4*(p+q)+(p+q)-1][5]  0.558405  0.9483
# d.o.f=0
# H0 : No serial correlation
# 
# Weighted Ljung-Box Test on Standardized Squared Residuals
# ------------------------------------
#                          statistic   p-value
# Lag[1]                       1.542 0.2142936
# Lag[2*(p+q)+(p+q)-1][8]     18.081 0.0003665
# Lag[4*(p+q)+(p+q)-1][14]    22.904 0.0004519
# d.o.f=3
# 
# Weighted ARCH LM Tests
# ------------------------------------
#             Statistic Shape Scale   P-Value
# ARCH Lag[4]     21.98 0.500 2.000 2.749e-06
# ARCH Lag[6]     22.08 1.461 1.711 9.472e-06
# ARCH Lag[8]     22.45 2.368 1.583 2.361e-05
# 
# Nyblom stability test
# ------------------------------------
# Joint Statistic:  0.5092
# Individual Statistics:             
# omega  0.2760
# alpha1 0.4043
# alpha2 0.3218
# beta1  0.2898
# 
# Asymptotic Critical Values (10% 5% 1%)
# Joint Statistic:     	   1.07 1.24 1.6
# Individual Statistic:	   0.35 0.47 0.75
# 
# Sign Bias Test
# ------------------------------------
#                    t-value   prob sig
# Sign Bias           1.2303 0.2192    
# Negative Sign Bias  0.7596 0.4479    
# Positive Sign Bias  0.2050 0.8377    
# Joint Effect        5.7578 0.1240    
# 
# 
# Adjusted Pearson Goodness-of-Fit Test:
# ------------------------------------
#   group statistic p-value(g-1)
# 1    20     48.78    0.0001974
# 2    30     58.53    0.0009373
# 3    40     68.45    0.0024612
# 4    50     76.63    0.0070186
# 
# 
# Elapsed time : 0.07643819 

# Analizzando i p-value del test pesato di Ljung-Box sui residui standardizzati, osserviamo che questi sono tutti maggiori di α = 0.01
# (in realtà anche di α = 0.05 e α = 0.10). Quindi, non abbiamo evidenza statistica sufficiente per rifiutare l’ipotesi nulla di
# assenza di autocorrelazione.

# Applichiamo, ora, dei dest di normalità sui residui.

# Estrai residui standardizzati
z_hat <- residuals(fit, standardize = TRUE)

# Prendi le date del training_set:
# Rimuoviamo le righe con NA nella colonna usata per il modello
valid_dates <- training_set$Date[!is.na(training_set$SPY_LogReturn)]

# Converti in Date se non lo sono già
valid_dates <- as.Date(valid_dates)

# Sovrascrivi l’index di z_hat con le date corrette
index(z_hat) <- valid_dates

# Controlla che le lunghezze coincidano
if(length(z_hat) != length(valid_dates)) {
  stop("Errore: lunghezza residui e date non coincide!")
}

# Visualizza i primi valori
head(z_hat)
#                  [,1]
# 2023-08-01 -0.2715260
# 2023-08-02 -1.3272829
# 2023-08-03 -0.2658114
# 2023-08-04 -0.4396988
# 2023-08-07  0.8964298
# 2023-08-08 -0.4588900

# Test di SW
shapirowilk_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Shapiro-Wilk Test
# Statistic: 0.964704
# p-value  : 2.97489e-09
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Test di JB
jarque_bera_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Jarque-Bera Test
# Statistic: 180.253765
# p-value  : 0
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Come vediamo, i residui standardizzati del GARCH(2,1) con distribuzione normale non seguono una distribuzione normale.
# Perciò, possiamo concludere che anche questo modello è da scartare.

# Analizziamo il prossimo nella classifica: GARCH(1,2) norm
p <- valid_models$p[16]
q <- valid_models$q[16]
dist <- valid_models$dist[16]

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = dist
)

fit <- tryCatch(
  ugarchfit(spec = spec, data = returns, solver = "hybrid"),
  error = function(e) NULL
)

# Risultati
show(fit)
# 
# *---------------------------------*
# *          GARCH Model Fit        *
# *---------------------------------*
#   
# Conditional Variance Dynamics 	
# -----------------------------------
# GARCH Model	    : sGARCH(1,2)
# Mean Model	    : ARFIMA(0,0,0)
# Distribution	  : norm 
# 
# Optimal Parameters
# ------------------------------------
#         Estimate  Std. Error  t value Pr(>|t|)
# omega   0.055091    0.014857   3.7081 0.000209
# alpha1  0.124492    0.002618  47.5454 0.000000
# beta1   0.818659    0.303358   2.6987 0.006962
# beta2   0.000000    0.264451   0.0000 1.000000
# 
# Robust Standard Errors:
#         Estimate  Std. Error  t value Pr(>|t|)
# omega   0.055091    0.031171 1.767397 0.077162
# alpha1  0.124492    0.053199 2.340126 0.019277
# beta1   0.818659    0.204559 4.002065 0.000063
# beta2   0.000000    0.159228 0.000001 0.999999
# 
# LogLikelihood : -621.4977 
# 
# Information Criteria
# ------------------------------------
#   
# Akaike       2.6392
# Bayes        2.6743
# Shibata      2.6391
# Hannan-Quinn 2.6530
# 
# Weighted Ljung-Box Test on Standardized Residuals
# ------------------------------------
#                         statistic p-value
# Lag[1]                   0.001879  0.9654
# Lag[2*(p+q)+(p+q)-1][2]  0.033143  0.9697
# Lag[4*(p+q)+(p+q)-1][5]  0.553959  0.9490
# d.o.f=0
# H0 : No serial correlation
# 
# Weighted Ljung-Box Test on Standardized Squared Residuals
# ------------------------------------
#                          statistic   p-value
# Lag[1]                      0.7958 0.3723465
# Lag[2*(p+q)+(p+q)-1][8]    18.6669 0.0002584
# Lag[4*(p+q)+(p+q)-1][14]   23.8963 0.0002625
# d.o.f=3
# 
# Weighted ARCH LM Tests
# ------------------------------------
#             Statistic Shape Scale   P-Value
# ARCH Lag[4]     24.51 0.500 2.000 7.408e-07
# ARCH Lag[6]     24.58 1.461 1.711 2.293e-06
# ARCH Lag[8]     25.00 2.368 1.583 5.420e-06
# 
# Nyblom stability test
# ------------------------------------
# Joint Statistic:  0.5426
# Individual Statistics:             
# omega  0.2713
# alpha1 0.3794
# beta1  0.2834
# beta2  0.2686
# 
# Asymptotic Critical Values (10% 5% 1%)
# Joint Statistic:     	   1.07 1.24 1.6
# Individual Statistic:	   0.35 0.47 0.75
# 
# Sign Bias Test
# ------------------------------------
#                    t-value   prob sig
# Sign Bias           1.2897 0.1978    
# Negative Sign Bias  0.5285 0.5974    
# Positive Sign Bias  0.2864 0.7747    
# Joint Effect        5.3506 0.1479    
# 
# 
# Adjusted Pearson Goodness-of-Fit Test:
# ------------------------------------
#   group statistic p-value(g-1)
# 1    20     46.59    0.0004086
# 2    30     55.87    0.0019565
# 3    40     61.02    0.0136039
# 4    50     78.32    0.0049042
# 
# 
# Elapsed time : 0.151052 

# Analizzando i p-value del test pesato di Ljung-Box sui residui standardizzati, osserviamo che questi sono tutti maggiori di α = 0.01
# (in realtà anche di α = 0.05 e α = 0.10). Quindi, non abbiamo evidenza statistica sufficiente per rifiutare l’ipotesi nulla di
# assenza di autocorrelazione.

# Applichiamo, ora, dei dest di normalità sui residui.

# Estrai residui standardizzati
z_hat <- residuals(fit, standardize = TRUE)

# Prendi le date del training_set:
# Rimuoviamo le righe con NA nella colonna usata per il modello
valid_dates <- training_set$Date[!is.na(training_set$SPY_LogReturn)]

# Converti in Date se non lo sono già
valid_dates <- as.Date(valid_dates)

# Sovrascrivi l’index di z_hat con le date corrette
index(z_hat) <- valid_dates

# Controlla che le lunghezze coincidano
if(length(z_hat) != length(valid_dates)) {
  stop("Errore: lunghezza residui e date non coincide!")
}

# Visualizza i primi valori
head(z_hat)
#                  [,1]
# 2023-08-01 -0.2715260
# 2023-08-02 -1.3272829
# 2023-08-03 -0.2607727
# 2023-08-04 -0.4408975
# 2023-08-07  0.8935150
# 2023-08-08 -0.4534967

# Test di SW
shapirowilk_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Shapiro-Wilk Test
# Statistic: 0.964812
# p-value  : 3.10789e-09
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Test di JB
jarque_bera_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Jarque-Bera Test
# Statistic: 181.821832
# p-value  : 0
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Come vediamo, i residui standardizzati del GARCH(1,2) con distribuzione normale non seguono una distribuzione normale.
# Perciò, possiamo concludere che anche questo modello è da scartare.

# Analizziamo il prossimo nella classifica: GARCH(2,2) sstd
p <- valid_models$p[17]
q <- valid_models$q[17]
dist <- valid_models$dist[17]

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = dist
)

fit <- tryCatch(
  ugarchfit(spec = spec, data = returns, solver = "hybrid"),
  error = function(e) NULL
)

# Risultati
show(fit)
# 
# *---------------------------------*
# *          GARCH Model Fit        *
# *---------------------------------*
#   
# Conditional Variance Dynamics 	
# -----------------------------------
# GARCH Model	    : sGARCH(2,2)
# Mean Model	    : ARFIMA(0,0,0)
# Distribution	  : sstd 
# 
# Optimal Parameters
# ------------------------------------
#         Estimate  Std. Error  t value Pr(>|t|)
# omega   0.089235    0.030614  2.91489 0.003558
# alpha1  0.074278    0.031498  2.35814 0.018367
# alpha2  0.045566    0.065406  0.69666 0.486014
# beta1   0.790935    0.407955  1.93878 0.052528
# beta2   0.000000    0.362010  0.00000 1.000000
# skew    0.823080    0.049741 16.54734 0.000000
# shape   4.808452    1.139131  4.22116 0.000024
# 
# Robust Standard Errors:
#         Estimate  Std. Error  t value Pr(>|t|)
# omega   0.089235    0.037170   2.4007 0.016362
# alpha1  0.074278    0.043926   1.6910 0.090843
# alpha2  0.045566    0.036515   1.2479 0.212083
# beta1   0.790935    0.386517   2.0463 0.040726
# beta2   0.000000    0.319878   0.0000 1.000000
# skew    0.823080    0.043753  18.8121 0.000000
# shape   4.808452    1.424830   3.3748 0.000739
# 
# LogLikelihood : -597.7414 
# 
# Information Criteria
# ------------------------------------
#   
# Akaike       2.5517
# Bayes        2.6131
# Shibata      2.5512
# Hannan-Quinn 2.5758
# 
# Weighted Ljung-Box Test on Standardized Residuals
# ------------------------------------
#                         statistic p-value
# Lag[1]                    0.04076  0.8400
# Lag[2*(p+q)+(p+q)-1][2]   0.12192  0.9045
# Lag[4*(p+q)+(p+q)-1][5]   0.88148  0.8859
# d.o.f=0
# H0 : No serial correlation
# 
# Weighted Ljung-Box Test on Standardized Squared Residuals
# ------------------------------------
#                          statistic   p-value
# Lag[1]                       5.667 1.729e-02
# Lag[2*(p+q)+(p+q)-1][11]    31.911 4.628e-07
# Lag[4*(p+q)+(p+q)-1][19]    36.860 2.105e-06
# d.o.f=4
# 
# Weighted ARCH LM Tests
# ------------------------------------
#             Statistic Shape Scale P-Value
# ARCH Lag[5]   0.07908 0.500 2.000  0.7785
# ARCH Lag[7]   0.27571 1.473 1.746  0.9539
# ARCH Lag[9]   1.18232 2.402 1.619  0.9048
# 
# Nyblom stability test
# ------------------------------------
# Joint Statistic:  2.7644
# Individual Statistics:              
# omega  0.04153
# alpha1 0.07002
# alpha2 0.05070
# beta1  0.03254
# beta2  0.03167
# skew   0.05876
# shape  0.30012
# 
# Asymptotic Critical Values (10% 5% 1%)
# Joint Statistic:     	   1.69 1.9 2.35
# Individual Statistic:	   0.35 0.47 0.75
# 
# Sign Bias Test
# ------------------------------------
#                    t-value    prob sig
# Sign Bias          0.90214 0.36745    
# Negative Sign Bias 1.66599 0.09638   *
#   Positive Sign Bias 0.08644 0.93116    
# Joint Effect       8.00749 0.04586  **
#   
#   
# Adjusted Pearson Goodness-of-Fit Test:
# ------------------------------------
#   group statistic p-value(g-1)
# 1    20     20.18      0.38399
# 2    30     32.84      0.28443
# 3    40     37.73      0.52775
# 4    50     67.56      0.04047
# 
# 
# Elapsed time : 0.1699739 

# Analizzando i p-value del test pesato di Ljung-Box sui residui standardizzati, osserviamo che questi sono tutti maggiori di α = 0.01 (in realtà
# anche di α = 0.05 e α = 0.10). Quindi, non abbiamo evidenza statistica sufficiente per rifiutare l’ipotesi nulla di assenza di autocorrelazione.

# Applichiamo, ora, dei dest di normalità sui residui.

# Estrai residui standardizzati
z_hat <- residuals(fit, standardize = TRUE)

# Prendi le date del training_set:
# Rimuoviamo le righe con NA nella colonna usata per il modello
valid_dates <- training_set$Date[!is.na(training_set$SPY_LogReturn)]

# Converti in Date se non lo sono già
valid_dates <- as.Date(valid_dates)

# Sovrascrivi l’index di z_hat con le date corrette
index(z_hat) <- valid_dates

# Controlla che le lunghezze coincidano
if(length(z_hat) != length(valid_dates)) {
  stop("Errore: lunghezza residui e date non coincide!")
}

# Visualizza i primi valori
head(z_hat)
#                  [,1]
# 2023-08-01 -0.2715260
# 2023-08-02 -1.3272829
# 2023-08-03 -0.2712217
# 2023-08-04 -0.4381200
# 2023-08-07  0.8893370
# 2023-08-08 -0.4568725

# Test di SW
shapirowilk_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Shapiro-Wilk Test
# Statistic: 0.962237
# p-value  : 1.11415e-09
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Test di JB
jarque_bera_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Jarque-Bera Test
# Statistic: 205.574068
# p-value  : 0
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Come atteso, i residui standardizzati del GARCH(2,2) con distribuzione skewed Student-t non seguono una distribuzione normale.

# QQ-Plot SSTD:
# Residui standardizzati ordinati
y <- sort(as.numeric(z_hat))
n <- length(y)

# Parametri stimati da 'fit'
params <- coef(fit)
SSTD_xi <- params["skew"]
SSTD_nu <- params["shape"]
mean <- 0
sd <- 1

# Parametri della distribuzione skewed Student-t
distr <- "sstd"
distr_pars <- list(mean = mean, sd = sd, nu = SSTD_nu, xi = SSTD_xi)

# Quantili teorici
quants <- qsstd(ppoints(n), mean = mean, sd = sd, nu = SSTD_nu, xi = SSTD_xi)

# DataFrame per plotting
QQ_plot_df <- data.frame(
  T = 1:n,
  Q = quants,
  X = y,
  Y = y
)

# Calcolo retta interquartile
quart_probs <- c(0.25, 0.75)
quart_X <- as.vector(quantile(QQ_plot_df$X, quart_probs))
quart_Q <- qsstd(quart_probs, mean = mean, sd = sd, nu = SSTD_nu, xi = SSTD_xi)
slope <- diff(quart_X) / diff(quart_Q)
intercept <- quart_X[1] - slope * quart_Q[1]

# Titoli
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 MPSMF 2024-2025",
                             paste("Q-Q plot of the Standardized Residuals from the GARCH(2,2) model for SPY (skewed Student-t)")))
subtitle_content <- bquote(paste("Sample size: ", .(n), 
                                 "; SSTD parameters → mean = ", .(mean), 
                                 ", sd = ", .(sd), 
                                 ", df = ", .(round(SSTD_nu, 3)), 
                                 ", skew = ", .(round(SSTD_xi, 3)), "."))

caption_content <- "Author: Matteo Basili"
x_name <- bquote("Theoretical Quantiles (SSTD)")
y_name <- bquote("Standardized Residuals")

# Assi e legende
x_breaks <- seq(from = floor(min(QQ_plot_df$Q)), to = ceiling(max(QQ_plot_df$Q)), by = 0.5)
x_labs <- format(x_breaks, scientific = FALSE)
y_breaks_num <- length(x_breaks)
y_binwidth <- round((max(QQ_plot_df$Y) - min(QQ_plot_df$Y)) / y_breaks_num, digits = 3)
y_breaks <- round(seq(from = floor(min(QQ_plot_df$Y)/y_binwidth)*y_binwidth, 
                      to = ceiling(max(QQ_plot_df$Y)/y_binwidth)*y_binwidth, 
                      by = y_binwidth), 3)
y_labs <- format(y_breaks, scientific = FALSE)

# Legende e colori
leg_shape_labs <- bquote("Q-Q plot")
leg_fill_labs <- c(bquote("90% confidence interval"), bquote("95% confidence interval"))
leg_col_labs <- c(bquote("Interquartile line"), bquote("Regression line"), bquote("y = x"))

Stand_Res_std_QQ_plot <- ggplot(QQ_plot_df) +
  stat_qq_band(aes(sample = X, fill = "95"), distribution = distr, dparams = distr_pars, conf = 0.95) +
  stat_qq_band(aes(sample = X, fill = "90"), distribution = distr, dparams = distr_pars, conf = 0.90) +
  geom_abline(aes(slope = slope, intercept = intercept, colour = "IQR"), linewidth = 0.8) +
  stat_smooth(aes(x = Q, y = Y, colour = "Reg", group = 1), method = "lm", se = FALSE, linewidth = 0.8) +
  geom_abline(aes(slope = 1, intercept = 0, colour = "45deg"), linewidth = 0.8) +
  stat_qq_point(aes(sample = X), shape = 19, color = "black", size = 1.2, alpha = 0.8, distribution = distr, dparams = distr_pars) +
  scale_x_continuous(name = x_name, breaks = x_breaks, labels = x_labs) +
  scale_y_continuous(name = y_name, breaks = y_breaks, labels = NULL,
                     sec.axis = sec_axis(~ ., breaks = y_breaks, labels = y_labs)) +
  ggtitle(title_content) +
  labs(subtitle = subtitle_content, caption = caption_content) +
  scale_shape_manual(name = "", labels = leg_shape_labs, values = c("19")) +
  scale_fill_manual(name = "", values = c("90" = "chartreuse1", "95" = "deepskyblue1"), labels = leg_fill_labs) +
  scale_colour_manual(name = "", values = c("IQR" = "cyan", "Reg" = "red", "45deg" = "black"), labels = leg_col_labs) +
  guides(shape = guide_legend(order = 1), fill = guide_legend(order = 2), colour = guide_legend(order = 3)) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.x = element_text(angle = 0, vjust = 1),
    legend.key.width = unit(0.8, "cm"),
    legend.position = "bottom"
  )

# Mostra il plot
plot(Stand_Res_std_QQ_plot)

# Calcola densità teorica nei quantili (pdf)
dens <- dsstd(quants, mean=mean, sd=sd, nu=SSTD_nu, xi=SSTD_xi)

# Calcola errore standard dei quantili teorici (formula banda Q-Q)
# Errore standard: SE = (p * (1-p)) / (n * f(Q)^2)
p <- ppoints(n)
SE <- sqrt(p * (1-p) / (n * dens^2))

# Limiti di confidenza (95%)
alpha <- 0.05
z <- qnorm(1 - alpha/2)

lower_95 <- quants - z * SE
upper_95 <- quants + z * SE

# Verifica quanti punti osservati cadono dentro le bande
# Ricorda che QQ_plot_df$X = dati ordinati osservati

inside_95 <- (QQ_plot_df$X >= lower_95) & (QQ_plot_df$X <= upper_95)

percent_inside_95 <- mean(inside_95) * 100

cat(sprintf("Percentuale di punti dentro la banda di confidenza al 95%%: %.2f%%\n", percent_inside_95))
# Percentuale di punti dentro la banda di confidenza al 95%: 98.10%

# Alpha per il 90%
alpha_90 <- 0.10
z_90 <- qnorm(1 - alpha_90/2)  # quantile normale per il 90%

# Limiti di confidenza al 90%
lower_90 <- quants - z_90 * SE
upper_90 <- quants + z_90 * SE

# Verifica quanti punti osservati cadono dentro la banda 90%
inside_90 <- (QQ_plot_df$X >= lower_90) & (QQ_plot_df$X <= upper_90)

percent_inside_90 <- mean(inside_90) * 100

cat(sprintf("Percentuale di punti dentro la banda di confidenza al 90%%: %.2f%%\n", percent_inside_90))
# Percentuale di punti dentro la banda di confidenza al 90%: 88.19%

# Kolmogorov-Smirnov test (K-S test)
ks_result <- ks.test(y, 
                     y = function(q) psstd(q, mean=0, sd=1, nu=SSTD_nu, xi=SSTD_xi),
                     alternative = "two.sided")

print(ks_result)
# 
#         Asymptotic one-sample Kolmogorov-Smirnov test
# 
# data:  y
# D = 0.051286, p-value = 0.1652
# alternative hypothesis: two-sided

# Dall'analisi del Q-Q Plot e del test di K-S vediamo, quindi, che i residui standardizzati seguono la distribuzione skewed Student-t con un livello
# di significatività del 5% (in realtà il test di K-S risulta anche al 10%).

# Verifichiamo ora la stazionarietà e l'omoschedasticità (non condizionata).

# Grafico dei residui
df_residui <- data.frame(
  Date = index(z_hat),
  Residui = as.numeric(z_hat)
)

ggplot(df_residui, aes(x = Date, y = Residui)) +
  geom_line(color = "steelblue", linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  labs(
    title = "Grafico dei residui standardizzati",
    subtitle = "Modello GARCH(2,2) con distribuzione skewed Student-t per SPY",
    x = "Data",
    y = "Residui standardizzati"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    axis.title = element_text(size = 11)
  )

# KPSS Test
kpss_test_level(df_residui, col = "Residui", alpha = 0.01)
# 
# [Residui] KPSS Test - Stazionarietà attorno a una media costante
# Statistic: 0.119361
# Valore critico (circa 1%): 0.739000
# Interpretazione del p-value: p-value > 0.10
# 
# ** CONCLUSIONE: Statistic <= valore critico (1%) **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO stazionari.

# Quindi, i residui standardizzati sono stazionari.

# Per l'omoschedasticità non condizionata, per prima cosa, vediamo se c'è curtosi nei residui standardizzati.

# Calcola la curtosi
kurt_z_hat <- kurtosis(as.numeric(z_hat), na.rm = TRUE)

cat(sprintf("Curtosi dei residui standardizzati: %.4f\n", kurt_z_hat))
# Curtosi dei residui standardizzati: 6.0162

# La curtosi c'è. Quindi, applichiamo la forma "studentized" dei test di Breusch-Pagan e White per la stazionarietà in varianza.

# Breusch-Pagan Test:
# Crea un indice temporale (può essere anche un numero progressivo)
df_residui$Index <- seq_along(df_residui$Residui)

# Esegui il test BP
breusch_pagan_test(
  index = df_residui$Index,
  group = df_residui$Residui,
  col = "Residui standardizzati"
)
# [Residui standardizzati] Breusch-Pagan Test
# Statistic: 4.914765
# p-value: 0.0266281
# 
# ** Conclusione: p-value > 0.01 **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON sono eteroschedastici.

# White Test
white_test(
  index = df_residui$Index,
  group = df_residui$Residui,
  col = "Residui standardizzati"
)
# [Residui standardizzati] White Test
# Statistic: 6.088743
# p-value: 0.0476262
# 
# ** Conclusione: p-value > 0.01 **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON sono eteroschedastici.

# Dai risultati dei test si vede che è verificata anche la condizione di omoschedasticità (non condizionata).

# Per verificare ulteriormente l'assenza di autocorrelazione per i residui standardizzati
# di questo modello, facciamo anche l'ACF, il PACF e il test di Ljung-Box classico.

# ACF
df_z <- data.frame(z_hat = as.numeric(z_hat))
plot_acf_residuals(df_z, col = "z_hat")

# PACF
plot_pacf_residuals(df_z, col = "z_hat")

# L’analisi visiva di ACF e PACF mostra che, in entrambi i grafici, nessuna barra supera la banda di confidenza al 90%.
# Dunque, non c'è una forte evidenza visiva di autocorrelazione.

# Ljung-Box Test
ljungbox_test(as.numeric(z_hat), col_name = "Residui standardizzati")
# Ljung-Box test per Residui standardizzati (max_lag = 10):
#            lag    lb_stat lb_pvalue
# X-squared    1 0.04075577 0.8400101
# X-squared1   2 0.20309091 0.9034401
# X-squared2   3 1.10826815 0.7750785
# X-squared3   4 1.52727941 0.8217985
# X-squared4   5 1.52801535 0.9098144
# X-squared5   6 2.23986528 0.8963696
# X-squared6   7 2.58565868 0.9205100
# X-squared7   8 2.64555648 0.9545976
# X-squared8   9 3.21189874 0.9552978
# X-squared9  10 3.23053359 0.9754664

# Osservando i p-value associati ai vari lag, vediamo che tutti questi sono maggiori di α = 0.10. Dunque, non abbiamo evidenza statistica
# sufficiente per rifiutare l’ipotesi nulla di assenza di autocorrelazione fino al lag specificato.

# Infine, verifichiamo anche l’assenza di eteroschedasticità condizionata nei residui standardizzati.

# Test ARCH di Engle
arch_effects_test(z_hat, col_name = "Residui Standardizzati")
# ARCH Test per Residui Standardizzati (max_lag = 5):
#            lag arch_stat  arch_pvalue
# statistic    1  6.551262 1.048098e-02
# statistic1   2  9.511962 8.600105e-03
# statistic2   3 13.354318 3.929785e-03
# statistic3   4 37.313059 1.552669e-07
# statistic4   5 37.636659 4.463502e-07

# Alla luce dei risultati, a partire dal lag 2, il test ARCH rivela evidenza significativa di eteroschedasticità residua, con p-value < 1%.
# Questo suggerisce che il modello GARCH(2,2) con distribuzione skewed Student-t non ha catturato completamente la dinamica della varianza
# condizionata, e che potrebbero essere presenti effetti ARCH residui a orizzonti più lunghi.
# Perciò, possiamo concludere che anche questo modello è da scartare.

# Analizziamo il prossimo nella classifica: GARCH(1,3) sstd
p <- valid_models$p[18]
q <- valid_models$q[18]
dist <- valid_models$dist[18]

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = dist
)

fit <- tryCatch(
  ugarchfit(spec = spec, data = returns, solver = "hybrid"),
  error = function(e) NULL
)

# Risultati
show(fit)
# 
# *---------------------------------*
# *          GARCH Model Fit        *
# *---------------------------------*
#   
# Conditional Variance Dynamics 	
# -----------------------------------
# GARCH Model	    : sGARCH(1,3)
# Mean Model	    : ARFIMA(0,0,0)
# Distribution	  : sstd 
# 
# Optimal Parameters
# ------------------------------------
#         Estimate  Std. Error   t value Pr(>|t|)
# omega    0.08214    0.027643  2.971494 0.002964
# alpha1   0.10845    0.021147  5.128628 0.000000
# beta1    0.80919    0.385510  2.099007 0.035816
# beta2    0.00000    0.549871  0.000001 0.999999
# beta3    0.00000    0.483271  0.000000 1.000000
# skew     0.82515    0.049926 16.527413 0.000000
# shape    4.79534    1.179338  4.066130 0.000048
# 
# Robust Standard Errors:
#         Estimate  Std. Error  t value Pr(>|t|)
# omega    0.08214    0.034025   2.4141 0.015775
# alpha1   0.10845    0.039796   2.7252 0.006426
# beta1    0.80919    0.335976   2.4085 0.016019
# beta2    0.00000    0.900544   0.0000 1.000000
# beta3    0.00000    0.613173   0.0000 1.000000
# skew     0.82515    0.044988  18.3416 0.000000
# shape    4.79534    1.446138   3.3160 0.000913
# 
# LogLikelihood : -597.8726 
# 
# Information Criteria
# ------------------------------------
#   
# Akaike       2.5522
# Bayes        2.6137
# Shibata      2.5518
# Hannan-Quinn 2.5764
# 
# Weighted Ljung-Box Test on Standardized Residuals
# ------------------------------------
#                         statistic p-value
# Lag[1]                   0.008046  0.9285
# Lag[2*(p+q)+(p+q)-1][2]  0.088315  0.9279
# Lag[4*(p+q)+(p+q)-1][5]  0.847039  0.8932
# d.o.f=0
# H0 : No serial correlation
# 
# Weighted Ljung-Box Test on Standardized Squared Residuals
# ------------------------------------
#                          statistic   p-value
# Lag[1]                        2.26 1.328e-01
# Lag[2*(p+q)+(p+q)-1][11]     32.05 4.246e-07
# Lag[4*(p+q)+(p+q)-1][19]     37.71 1.295e-06
# d.o.f=4
# 
# Weighted ARCH LM Tests
# ------------------------------------
#             Statistic Shape Scale P-Value
# ARCH Lag[5]    0.0262 0.500 2.000  0.8714
# ARCH Lag[7]    0.1812 1.473 1.746  0.9744
# ARCH Lag[9]    1.3221 2.402 1.619  0.8825
# 
# Nyblom stability test
# ------------------------------------
# Joint Statistic:  2.9437
# Individual Statistics:              
# omega  0.04236
# alpha1 0.05715
# beta1  0.03221
# beta2  0.03085
# beta3  0.03201
# skew   0.05503
# shape  0.28953
# 
# Asymptotic Critical Values (10% 5% 1%)
# Joint Statistic:     	   1.69 1.9 2.35
# Individual Statistic:	   0.35 0.47 0.75
# 
# Sign Bias Test
# ------------------------------------
#                    t-value   prob sig
# Sign Bias          1.06981 0.2853    
# Negative Sign Bias 1.08710 0.2775    
# Positive Sign Bias 0.09855 0.9215    
# Joint Effect       6.10789 0.1065    
# 
# 
# Adjusted Pearson Goodness-of-Fit Test:
# ------------------------------------
#   group statistic p-value(g-1)
# 1    20     19.16      0.44633
# 2    30     33.34      0.26417
# 3    40     37.39      0.54330
# 4    50     67.35      0.04199
# 
# 
# Elapsed time : 0.194886 

# Analizzando i p-value del test pesato di Ljung-Box sui residui standardizzati, osserviamo che questi sono tutti maggiori di α = 0.01 (in realtà
# anche di α = 0.05 e α = 0.10). Quindi, non abbiamo evidenza statistica sufficiente per rifiutare l’ipotesi nulla di assenza di autocorrelazione.

# Applichiamo, ora, dei dest di normalità sui residui.

# Estrai residui standardizzati
z_hat <- residuals(fit, standardize = TRUE)

# Prendi le date del training_set:
# Rimuoviamo le righe con NA nella colonna usata per il modello
valid_dates <- training_set$Date[!is.na(training_set$SPY_LogReturn)]

# Converti in Date se non lo sono già
valid_dates <- as.Date(valid_dates)

# Sovrascrivi l’index di z_hat con le date corrette
index(z_hat) <- valid_dates

# Controlla che le lunghezze coincidano
if(length(z_hat) != length(valid_dates)) {
  stop("Errore: lunghezza residui e date non coincide!")
}

# Visualizza i primi valori
head(z_hat)
#                  [,1]
# 2023-08-01 -0.2715260
# 2023-08-02 -1.3272829
# 2023-08-03 -0.2719337
# 2023-08-04 -0.4550104
# 2023-08-07  0.9122676
# 2023-08-08 -0.4598059

# Test di SW
shapirowilk_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Shapiro-Wilk Test
# Statistic: 0.962959
# p-value  : 1.47915e-09
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Test di JB
jarque_bera_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Jarque-Bera Test
# Statistic: 207.212752
# p-value  : 0
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Come atteso, i residui standardizzati del GARCH(1,3) con distribuzione skewed Student-t non seguono una distribuzione normale.

# QQ-Plot SSTD:
# Residui standardizzati ordinati
y <- sort(as.numeric(z_hat))
n <- length(y)

# Parametri stimati da 'fit'
params <- coef(fit)
SSTD_xi <- params["skew"]
SSTD_nu <- params["shape"]
mean <- 0
sd <- 1

# Parametri della distribuzione skewed Student-t
distr <- "sstd"
distr_pars <- list(mean = mean, sd = sd, nu = SSTD_nu, xi = SSTD_xi)

# Quantili teorici
quants <- qsstd(ppoints(n), mean = mean, sd = sd, nu = SSTD_nu, xi = SSTD_xi)

# DataFrame per plotting
QQ_plot_df <- data.frame(
  T = 1:n,
  Q = quants,
  X = y,
  Y = y
)

# Calcolo retta interquartile
quart_probs <- c(0.25, 0.75)
quart_X <- as.vector(quantile(QQ_plot_df$X, quart_probs))
quart_Q <- qsstd(quart_probs, mean = mean, sd = sd, nu = SSTD_nu, xi = SSTD_xi)
slope <- diff(quart_X) / diff(quart_Q)
intercept <- quart_X[1] - slope * quart_Q[1]

# Titoli
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 MPSMF 2024-2025",
                             paste("Q-Q plot of the Standardized Residuals from the GARCH(1,3) model for SPY (skewed Student-t)")))
subtitle_content <- bquote(paste("Sample size: ", .(n), 
                                 "; SSTD parameters → mean = ", .(mean), 
                                 ", sd = ", .(sd), 
                                 ", df = ", .(round(SSTD_nu, 3)), 
                                 ", skew = ", .(round(SSTD_xi, 3)), "."))

caption_content <- "Author: Matteo Basili"
x_name <- bquote("Theoretical Quantiles (SSTD)")
y_name <- bquote("Standardized Residuals")

# Assi e legende
x_breaks <- seq(from = floor(min(QQ_plot_df$Q)), to = ceiling(max(QQ_plot_df$Q)), by = 0.5)
x_labs <- format(x_breaks, scientific = FALSE)
y_breaks_num <- length(x_breaks)
y_binwidth <- round((max(QQ_plot_df$Y) - min(QQ_plot_df$Y)) / y_breaks_num, digits = 3)
y_breaks <- round(seq(from = floor(min(QQ_plot_df$Y)/y_binwidth)*y_binwidth, 
                      to = ceiling(max(QQ_plot_df$Y)/y_binwidth)*y_binwidth, 
                      by = y_binwidth), 3)
y_labs <- format(y_breaks, scientific = FALSE)

# Legende e colori
leg_shape_labs <- bquote("Q-Q plot")
leg_fill_labs <- c(bquote("90% confidence interval"), bquote("95% confidence interval"))
leg_col_labs <- c(bquote("Interquartile line"), bquote("Regression line"), bquote("y = x"))

Stand_Res_std_QQ_plot <- ggplot(QQ_plot_df) +
  stat_qq_band(aes(sample = X, fill = "95"), distribution = distr, dparams = distr_pars, conf = 0.95) +
  stat_qq_band(aes(sample = X, fill = "90"), distribution = distr, dparams = distr_pars, conf = 0.90) +
  geom_abline(aes(slope = slope, intercept = intercept, colour = "IQR"), linewidth = 0.8) +
  stat_smooth(aes(x = Q, y = Y, colour = "Reg", group = 1), method = "lm", se = FALSE, linewidth = 0.8) +
  geom_abline(aes(slope = 1, intercept = 0, colour = "45deg"), linewidth = 0.8) +
  stat_qq_point(aes(sample = X), shape = 19, color = "black", size = 1.2, alpha = 0.8, distribution = distr, dparams = distr_pars) +
  scale_x_continuous(name = x_name, breaks = x_breaks, labels = x_labs) +
  scale_y_continuous(name = y_name, breaks = y_breaks, labels = NULL,
                     sec.axis = sec_axis(~ ., breaks = y_breaks, labels = y_labs)) +
  ggtitle(title_content) +
  labs(subtitle = subtitle_content, caption = caption_content) +
  scale_shape_manual(name = "", labels = leg_shape_labs, values = c("19")) +
  scale_fill_manual(name = "", values = c("90" = "chartreuse1", "95" = "deepskyblue1"), labels = leg_fill_labs) +
  scale_colour_manual(name = "", values = c("IQR" = "cyan", "Reg" = "red", "45deg" = "black"), labels = leg_col_labs) +
  guides(shape = guide_legend(order = 1), fill = guide_legend(order = 2), colour = guide_legend(order = 3)) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.x = element_text(angle = 0, vjust = 1),
    legend.key.width = unit(0.8, "cm"),
    legend.position = "bottom"
  )

# Mostra il plot
plot(Stand_Res_std_QQ_plot)

# Calcola densità teorica nei quantili (pdf)
dens <- dsstd(quants, mean=mean, sd=sd, nu=SSTD_nu, xi=SSTD_xi)

# Calcola errore standard dei quantili teorici (formula banda Q-Q)
# Errore standard: SE = (p * (1-p)) / (n * f(Q)^2)
p <- ppoints(n)
SE <- sqrt(p * (1-p) / (n * dens^2))

# Limiti di confidenza (95%)
alpha <- 0.05
z <- qnorm(1 - alpha/2)

lower_95 <- quants - z * SE
upper_95 <- quants + z * SE

# Verifica quanti punti osservati cadono dentro le bande
# Ricorda che QQ_plot_df$X = dati ordinati osservati

inside_95 <- (QQ_plot_df$X >= lower_95) & (QQ_plot_df$X <= upper_95)

percent_inside_95 <- mean(inside_95) * 100

cat(sprintf("Percentuale di punti dentro la banda di confidenza al 95%%: %.2f%%\n", percent_inside_95))
# Percentuale di punti dentro la banda di confidenza al 95%: 96.62%

# Alpha per il 90%
alpha_90 <- 0.10
z_90 <- qnorm(1 - alpha_90/2)  # quantile normale per il 90%

# Limiti di confidenza al 90%
lower_90 <- quants - z_90 * SE
upper_90 <- quants + z_90 * SE

# Verifica quanti punti osservati cadono dentro la banda 90%
inside_90 <- (QQ_plot_df$X >= lower_90) & (QQ_plot_df$X <= upper_90)

percent_inside_90 <- mean(inside_90) * 100

cat(sprintf("Percentuale di punti dentro la banda di confidenza al 90%%: %.2f%%\n", percent_inside_90))
# Percentuale di punti dentro la banda di confidenza al 90%: 87.34%

# Kolmogorov-Smirnov test (K-S test)
ks_result <- ks.test(y, 
                     y = function(q) psstd(q, mean=0, sd=1, nu=SSTD_nu, xi=SSTD_xi),
                     alternative = "two.sided")

print(ks_result)
# 
#         Asymptotic one-sample Kolmogorov-Smirnov test
# 
# data:  y
# D = 0.050048, p-value = 0.186
# alternative hypothesis: two-sided

# Dall'analisi del Q-Q Plot e del test di K-S vediamo, quindi, che i residui standardizzati seguono la distribuzione skewed Student-t con un livello
# di significatività del 5% (in realtà il test di K-S risulta anche al 10%).

# Verifichiamo ora la stazionarietà e l'omoschedasticità (non condizionata).

# Grafico dei residui
df_residui <- data.frame(
  Date = index(z_hat),
  Residui = as.numeric(z_hat)
)

ggplot(df_residui, aes(x = Date, y = Residui)) +
  geom_line(color = "steelblue", linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  labs(
    title = "Grafico dei residui standardizzati",
    subtitle = "Modello GARCH(1,3) con distribuzione skewed Student-t per SPY",
    x = "Data",
    y = "Residui standardizzati"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    axis.title = element_text(size = 11)
  )

# KPSS Test
kpss_test_level(df_residui, col = "Residui", alpha = 0.01)
# 
# [Residui] KPSS Test - Stazionarietà attorno a una media costante
# Statistic: 0.115835
# Valore critico (circa 1%): 0.739000
# Interpretazione del p-value: p-value > 0.10
# 
# ** CONCLUSIONE: Statistic <= valore critico (1%) **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO stazionari.

# Quindi, i residui standardizzati sono stazionari.

# Per l'omoschedasticità non condizionata, per prima cosa, vediamo se c'è curtosi nei residui standardizzati.

# Calcola la curtosi
kurt_z_hat <- kurtosis(as.numeric(z_hat), na.rm = TRUE)

cat(sprintf("Curtosi dei residui standardizzati: %.4f\n", kurt_z_hat))
# Curtosi dei residui standardizzati: 6.0653

# La curtosi c'è. Quindi, applichiamo la forma "studentized" dei test di Breusch-Pagan e White per la stazionarietà in varianza.

# Breusch-Pagan Test:
# Crea un indice temporale (può essere anche un numero progressivo)
df_residui$Index <- seq_along(df_residui$Residui)

# Esegui il test BP
breusch_pagan_test(
  index = df_residui$Index,
  group = df_residui$Residui,
  col = "Residui standardizzati"
)
# [Residui standardizzati] Breusch-Pagan Test
# Statistic: 4.744518
# p-value: 0.0293918
# 
# ** Conclusione: p-value > 0.01 **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON sono eteroschedastici.

# White Test
white_test(
  index = df_residui$Index,
  group = df_residui$Residui,
  col = "Residui standardizzati"
)
# [Residui standardizzati] White Test
# Statistic: 5.833679
# p-value: 0.0541044
# 
# ** Conclusione: p-value > 0.01 **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON sono eteroschedastici.

# Dai risultati dei test si vede che è verificata anche la condizione di omoschedasticità (non condizionata).

# Per verificare ulteriormente l'assenza di autocorrelazione per i residui standardizzati
# di questo modello, facciamo anche l'ACF, il PACF e il test di Ljung-Box classico.

# ACF
df_z <- data.frame(z_hat = as.numeric(z_hat))
plot_acf_residuals(df_z, col = "z_hat")

# PACF
plot_pacf_residuals(df_z, col = "z_hat")

# L’analisi visiva di ACF e PACF mostra che, in entrambi i grafici, nessuna barra supera la banda di confidenza al 90%.
# Dunque, non c'è una forte evidenza visiva di autocorrelazione.

# Ljung-Box Test
ljungbox_test(as.numeric(z_hat), col_name = "Residui standardizzati")
# Ljung-Box test per Residui standardizzati (max_lag = 10):
#            lag     lb_stat lb_pvalue
# X-squared    1 0.008045776 0.9285270
# X-squared1   2 0.168584651 0.9191625
# X-squared2   3 0.946793527 0.8141232
# X-squared3   4 1.549548668 0.8178298
# X-squared4   5 1.562222828 0.9057795
# X-squared5   6 2.288130357 0.8913850
# X-squared6   7 2.618852598 0.9178847
# X-squared7   8 2.693828958 0.9520794
# X-squared8   9 3.172191870 0.9570748
# X-squared9  10 3.213920903 0.9759320

# Osservando i p-value associati ai vari lag, vediamo che tutti questi sono maggiori di α = 0.10. Dunque, non abbiamo evidenza statistica
# sufficiente per rifiutare l’ipotesi nulla di assenza di autocorrelazione fino al lag specificato.

# Infine, verifichiamo anche l’assenza di eteroschedasticità condizionata nei residui standardizzati.

# Test ARCH di Engle
arch_effects_test(z_hat, col_name = "Residui Standardizzati")
# ARCH Test per Residui Standardizzati (max_lag = 5):
#            lag arch_stat  arch_pvalue
# statistic    1  2.846085 9.159676e-02
# statistic1   2  4.932675 8.489520e-02
# statistic2   3  7.215970 6.532356e-02
# statistic3   4 39.102365 6.635385e-08
# statistic4   5 39.237914 2.126847e-07

# Alla luce dei risultati, a partire dal lag 4, il test ARCH rivela evidenza significativa di eteroschedasticità residua, con p-value < 1%.
# Questo suggerisce che il modello GARCH(1,3) con distribuzione skewed Student-t non ha catturato completamente la dinamica della varianza
# condizionata, e che potrebbero essere presenti effetti ARCH residui a orizzonti più lunghi.
# Perciò, possiamo concludere che anche questo modello è da scartare.

# Analizziamo il prossimo nella classifica: GARCH(2,2) sged
p <- valid_models$p[19]
q <- valid_models$q[19]
dist <- valid_models$dist[19]

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = dist
)

fit <- tryCatch(
  ugarchfit(spec = spec, data = returns, solver = "hybrid"),
  error = function(e) NULL
)

# Risultati
show(fit)
# 
# *---------------------------------*
# *          GARCH Model Fit        *
# *---------------------------------*
#   
# Conditional Variance Dynamics 	
# -----------------------------------
# GARCH Model	    : sGARCH(2,2)
# Mean Model	    : ARFIMA(0,0,0)
# Distribution	  : sged 
# 
# Optimal Parameters
# ------------------------------------
#         Estimate  Std. Error   t value Pr(>|t|)
# omega   0.067077    0.006905  9.713626  0.00000
# alpha1  0.084047    0.014007  6.000392  0.00000
# alpha2  0.042507    0.083845  0.506968  0.61218
# beta1   0.800768    0.081111  9.872520  0.00000
# beta2   0.000000    0.160567  0.000003  1.00000
# skew    0.875272    0.045616 19.187939  0.00000
# shape   1.223434    0.109523 11.170610  0.00000
# 
# Robust Standard Errors:
#         Estimate  Std. Error   t value Pr(>|t|)
# omega   0.067077    0.038794  1.729071 0.083796
# alpha1  0.084047    0.099277  0.846588 0.397225
# alpha2  0.042507    0.149508  0.284310 0.776172
# beta1   0.800768    0.190885  4.195028 0.000027
# beta2   0.000000    0.274029  0.000002 0.999999
# skew    0.875272    0.049451 17.699960 0.000000
# shape   1.223434    0.150905  8.107299 0.000000
# 
# LogLikelihood : -598.7948 
# 
# Information Criteria
# ------------------------------------
#   
# Akaike       2.5561
# Bayes        2.6175
# Shibata      2.5557
# Hannan-Quinn 2.5803
# 
# Weighted Ljung-Box Test on Standardized Residuals
# ------------------------------------
#                         statistic p-value
# Lag[1]                    0.02121  0.8842
# Lag[2*(p+q)+(p+q)-1][2]   0.06919  0.9418
# Lag[4*(p+q)+(p+q)-1][5]   0.66360  0.9298
# d.o.f=0
# H0 : No serial correlation
# 
# Weighted Ljung-Box Test on Standardized Squared Residuals
# ------------------------------------
#                          statistic   p-value
# Lag[1]                       3.287 6.981e-02
# Lag[2*(p+q)+(p+q)-1][11]    24.525 4.062e-05
# Lag[4*(p+q)+(p+q)-1][19]    28.928 1.623e-04
# d.o.f=4
# 
# Weighted ARCH LM Tests
# ------------------------------------
#             Statistic Shape Scale P-Value
# ARCH Lag[5]    0.0437 0.500 2.000  0.8344
# ARCH Lag[7]    0.3454 1.473 1.746  0.9372
# ARCH Lag[9]    1.2252 2.402 1.619  0.8981
# 
# Nyblom stability test
# ------------------------------------
# Joint Statistic:  2.7474
# Individual Statistics:             
# omega  0.1073
# alpha1 0.2133
# alpha2 0.1612
# beta1  0.1171
# beta2  0.1105
# skew   0.1866
# shape  0.4382
# 
# Asymptotic Critical Values (10% 5% 1%)
# Joint Statistic:     	   1.69 1.9 2.35
# Individual Statistic:	   0.35 0.47 0.75
# 
# Sign Bias Test
# ------------------------------------
#                    t-value    prob sig
# Sign Bias          1.07708 0.28200    
# Negative Sign Bias 1.20642 0.22826    
# Positive Sign Bias 0.06476 0.94839    
# Joint Effect       6.70116 0.08206   *
#   
#   
# Adjusted Pearson Goodness-of-Fit Test:
# ------------------------------------
#   group statistic p-value(g-1)
# 1    20     18.07       0.5179
# 2    30     37.14       0.1427
# 3    40     42.46       0.3244
# 4    50     59.97       0.1355
# 
# 
# Elapsed time : 0.273927

# Analizzando i p-value del test pesato di Ljung-Box sui residui standardizzati, osserviamo che questi sono tutti maggiori di α = 0.01
# (in realtà anche di α = 0.05 e α = 0.10). Quindi, non abbiamo evidenza statistica sufficiente per rifiutare l’ipotesi nulla di assenza di
# autocorrelazione.

# Applichiamo, ora, dei dest di normalità sui residui.

# Estrai residui standardizzati
z_hat <- residuals(fit, standardize = TRUE)

# Prendi le date del training_set:
# Rimuoviamo le righe con NA nella colonna usata per il modello
valid_dates <- training_set$Date[!is.na(training_set$SPY_LogReturn)]

# Converti in Date se non lo sono già
valid_dates <- as.Date(valid_dates)

# Sovrascrivi l’index di z_hat con le date corrette
index(z_hat) <- valid_dates

# Controlla che le lunghezze coincidano
if(length(z_hat) != length(valid_dates)) {
  stop("Errore: lunghezza residui e date non coincide!")
}

# Visualizza i primi valori
head(z_hat)
#                  [,1]
# 2023-08-01 -0.2715260
# 2023-08-02 -1.3272829
# 2023-08-03 -0.2702921
# 2023-08-04 -0.4402155
# 2023-08-07  0.8978477
# 2023-08-08 -0.4620933

# Test di SW
shapirowilk_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Shapiro-Wilk Test
# Statistic: 0.964012
# p-value  : 2.24905e-09
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Test di JB
jarque_bera_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Jarque-Bera Test
# Statistic: 185.936466
# p-value  : 0
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Come atteso, i residui standardizzati del GARCH(2,2) con distribuzione skewed GED non seguono una distribuzione normale.

# QQ-Plot SGED:
# Residui standardizzati ordinati
y <- sort(as.numeric(z_hat))
n <- length(y)

# Parametri stimati da 'fit'
params <- coef(fit)
SGED_xi <- params["skew"]
SGED_nu <- params["shape"]
mean <- 0
sd <- 1

# Parametri della distribuzione skewed GED
distr <- "sged"
distr_pars <- list(mean = mean, sd = sd, nu = SGED_nu, xi = SGED_xi)

# Quantili teorici
quants <- qsged(ppoints(n), mean = mean, sd = sd, nu = SGED_nu, xi = SGED_xi)

# DataFrame per plotting
QQ_plot_df <- data.frame(
  T = 1:n,
  Q = quants,
  X = y,
  Y = y
)

# Calcolo retta interquartile
quart_probs <- c(0.25, 0.75)
quart_X <- as.vector(quantile(QQ_plot_df$X, quart_probs))
quart_Q <- qsged(quart_probs, mean = mean, sd = sd, nu = SGED_nu, xi = SGED_xi)
slope <- diff(quart_X) / diff(quart_Q)
intercept <- quart_X[1] - slope * quart_Q[1]

# Titoli
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 MPSMF 2024-2025",
                             paste("Q-Q plot of the Standardized Residuals from the GARCH(2,2) model for SPY (skewed GED)")))
subtitle_content <- bquote(paste("Sample size: ", .(n), 
                                 "; SGED parameters → mean = ", .(mean), 
                                 ", sd = ", .(sd), 
                                 ", shape = ", .(round(SGED_nu, 3)), 
                                 ", skew = ", .(round(SGED_xi, 3)), "."))

caption_content <- "Author: Matteo Basili"
x_name <- bquote("Theoretical Quantiles (SGED)")
y_name <- bquote("Standardized Residuals")

# Assi e legende
x_breaks <- seq(from = floor(min(QQ_plot_df$Q)), to = ceiling(max(QQ_plot_df$Q)), by = 0.5)
x_labs <- format(x_breaks, scientific = FALSE)
y_breaks_num <- length(x_breaks)
y_binwidth <- round((max(QQ_plot_df$Y) - min(QQ_plot_df$Y)) / y_breaks_num, digits = 3)
y_breaks <- round(seq(from = floor(min(QQ_plot_df$Y)/y_binwidth)*y_binwidth, 
                      to = ceiling(max(QQ_plot_df$Y)/y_binwidth)*y_binwidth, 
                      by = y_binwidth), 3)
y_labs <- format(y_breaks, scientific = FALSE)

# Legende e colori
leg_shape_labs <- bquote("Q-Q plot")
leg_fill_labs <- c(bquote("90% confidence interval"), bquote("95% confidence interval"))
leg_col_labs <- c(bquote("Interquartile line"), bquote("Regression line"), bquote("y = x"))

Stand_Res_std_QQ_plot <- ggplot(QQ_plot_df) + 
  stat_qq_band(aes(sample = X, fill = "95"), distribution = distr, dparams = distr_pars, conf = 0.95) +
  stat_qq_band(aes(sample = X, fill = "90"), distribution = distr, dparams = distr_pars, conf = 0.90) +
  geom_abline(aes(slope = slope, intercept = intercept, colour = "IQR"), linewidth = 0.8) +
  stat_smooth(aes(x = Q, y = Y, colour = "Reg", group = 1), method = "lm", se = FALSE, linewidth = 0.8) +
  geom_abline(aes(slope = 1, intercept = 0, colour = "45deg"), linewidth = 0.8) +
  stat_qq_point(aes(sample = X), shape = 19, color = "black", size = 1.2, alpha = 0.8, distribution = distr, dparams = distr_pars) +
  scale_x_continuous(name = x_name, breaks = x_breaks, labels = x_labs) +
  scale_y_continuous(name = y_name, breaks = y_breaks, labels = NULL,
                     sec.axis = sec_axis(~ ., breaks = y_breaks, labels = y_labs)) +
  ggtitle(title_content) +
  labs(subtitle = subtitle_content, caption = caption_content) +
  scale_shape_manual(name = "", labels = leg_shape_labs, values = c("19")) +
  scale_fill_manual(name = "", values = c("90" = "chartreuse1", "95" = "deepskyblue1"), labels = leg_fill_labs) +
  scale_colour_manual(name = "", values = c("IQR" = "cyan", "Reg" = "red", "45deg" = "black"), labels = leg_col_labs) +
  guides(shape = guide_legend(order = 1), fill = guide_legend(order = 2), colour = guide_legend(order = 3)) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.x = element_text(angle = 0, vjust = 1),
    legend.key.width = unit(0.8, "cm"),
    legend.position = "bottom"
  )

# Mostra il plot
plot(Stand_Res_std_QQ_plot)

# Calcola densità teorica nei quantili (SGED)
dens <- dsged(quants, mean = mean, sd = sd, nu = SGED_nu, xi = SGED_xi)

# Probabilità associate ai quantili
p <- ppoints(n)

# Errore standard dei quantili teorici
SE <- sqrt(p * (1 - p) / (n * dens^2))

# Limiti di confidenza al 95%
alpha_95 <- 0.05
z_95 <- qnorm(1 - alpha_95 / 2)
lower_95 <- quants - z_95 * SE
upper_95 <- quants + z_95 * SE

# Percentuale di punti dentro la banda 95%
inside_95 <- (QQ_plot_df$X >= lower_95) & (QQ_plot_df$X <= upper_95)
percent_inside_95 <- mean(inside_95) * 100
cat(sprintf("Percentuale di punti dentro la banda di confidenza al 95%%: %.2f%%\n", percent_inside_95))
# Percentuale di punti dentro la banda di confidenza al 95%: 97.89%

# Limiti di confidenza al 90%
alpha_90 <- 0.10
z_90 <- qnorm(1 - alpha_90 / 2)
lower_90 <- quants - z_90 * SE
upper_90 <- quants + z_90 * SE

# Percentuale di punti dentro la banda 90%
inside_90 <- (QQ_plot_df$X >= lower_90) & (QQ_plot_df$X <= upper_90)
percent_inside_90 <- mean(inside_90) * 100
cat(sprintf("Percentuale di punti dentro la banda di confidenza al 90%%: %.2f%%\n", percent_inside_90))
# Percentuale di punti dentro la banda di confidenza al 90%: 86.71%

# Kolmogorov-Smirnov test (K-S test)
ks_result <- ks.test(y, 
                     y = function(q) psged(q, mean=0, sd=1, nu=SGED_nu, xi=SGED_xi),
                     alternative = "two.sided")

print(ks_result)
# 
#         Asymptotic one-sample Kolmogorov-Smirnov test
# 
# data:  y
# D = 0.049353, p-value = 0.1985
# alternative hypothesis: two-sided

# Dall'analisi del Q-Q Plot e del test di K-S vediamo, quindi, che i residui standardizzati seguono la distribuzione skewed GED con un livello
# di significatività del 5% (in realtà il test di K-S risulta anche al 10%).

# Verifichiamo ora la stazionarietà e l'omoschedasticità (non condizionata).

# Grafico dei residui
df_residui <- data.frame(
  Date = index(z_hat),
  Residui = as.numeric(z_hat)
)

ggplot(df_residui, aes(x = Date, y = Residui)) +
  geom_line(color = "steelblue", linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  labs(
    title = "Grafico dei residui standardizzati",
    subtitle = "Modello GARCH(2,2) con distribuzione skewed GED per SPY",
    x = "Data",
    y = "Residui standardizzati"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    axis.title = element_text(size = 11)
  )

# KPSS Test
kpss_test_level(df_residui, col = "Residui", alpha = 0.01)
# 
# [Residui] KPSS Test - Stazionarietà attorno a una media costante
# Statistic: 0.127136
# Valore critico (circa 1%): 0.739000
# Interpretazione del p-value: p-value > 0.10
# 
# ** CONCLUSIONE: Statistic <= valore critico (1%) **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO stazionari.

# Quindi, i residui standardizzati sono stazionari.

# Per l'omoschedasticità non condizionata, per prima cosa, vediamo se c'è curtosi nei residui standardizzati.

# Calcola la curtosi
kurt_z_hat <- kurtosis(as.numeric(z_hat), na.rm = TRUE)

cat(sprintf("Curtosi dei residui standardizzati: %.4f\n", kurt_z_hat))
# Curtosi dei residui standardizzati: 5.8196

# La curtosi c'è. Quindi, applichiamo la forma "studentized" dei test di Breusch-Pagan e White per la stazionarietà in varianza.

# Breusch-Pagan Test:
# Crea un indice temporale (può essere anche un numero progressivo)
df_residui$Index <- seq_along(df_residui$Residui)

# Esegui il test BP
breusch_pagan_test(
  index = df_residui$Index,
  group = df_residui$Residui,
  col = "Residui standardizzati"
)
# [Residui standardizzati] Breusch-Pagan Test
# Statistic: 3.936780
# p-value: 0.0472411
# 
# ** Conclusione: p-value > 0.01 **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON sono eteroschedastici.

# White Test
white_test(
  index = df_residui$Index,
  group = df_residui$Residui,
  col = "Residui standardizzati"
)
# [Residui standardizzati] White Test
# Statistic: 4.667341
# p-value: 0.0969393
# 
# ** Conclusione: p-value > 0.01 **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON sono eteroschedastici.

# Dai risultati dei test si vede che è verificata anche la condizione di omoschedasticità (non condizionata).

# Per verificare ulteriormente l'assenza di autocorrelazione per i residui standardizzati
# di questo modello, facciamo anche l'ACF, il PACF e il test di Ljung-Box classico.

# ACF
df_z <- data.frame(z_hat = as.numeric(z_hat))
plot_acf_residuals(df_z, col = "z_hat")

# PACF
plot_pacf_residuals(df_z, col = "z_hat")

# L’analisi visiva di ACF e PACF mostra che, in entrambi i grafici, nessuna barra supera la banda di confidenza al 90%.
# Dunque, non c'è una forte evidenza visiva di autocorrelazione.

# Ljung-Box Test
ljungbox_test(as.numeric(z_hat), col_name = "Residui standardizzati")
# Ljung-Box test per Residui standardizzati (max_lag = 10):
#            lag   lb_stat lb_pvalue
# X-squared    1 0.0212084 0.8842127
# X-squared1   2 0.1171740 0.9430962
# X-squared2   3 0.8690616 0.8328868
# X-squared3   4 1.1454541 0.8869950
# X-squared4   5 1.1651148 0.9481800
# X-squared5   6 1.8932392 0.9292507
# X-squared6   7 2.3166978 0.9402564
# X-squared7   8 2.3689429 0.9675624
# X-squared8   9 2.9069804 0.9678688
# X-squared9  10 2.9453551 0.9826809

# Osservando i p-value associati ai vari lag, vediamo che tutti questi sono maggiori di α = 0.10. Dunque, non abbiamo evidenza statistica
# sufficiente per rifiutare l’ipotesi nulla di assenza di autocorrelazione fino al lag specificato.

# Infine, verifichiamo anche l’assenza di eteroschedasticità condizionata nei residui standardizzati.

# Test ARCH di Engle
arch_effects_test(z_hat, col_name = "Residui Standardizzati")
# ARCH Test per Residui Standardizzati (max_lag = 5):
#            lag arch_stat  arch_pvalue
# statistic    1  3.934858 4.729514e-02
# statistic1   2  6.787551 3.358165e-02
# statistic2   3  8.555300 3.582675e-02
# statistic3   4 29.151662 7.281959e-06
# statistic4   5 29.271813 2.050727e-05

# Alla luce dei risultati, a partire dal lag 4, il test ARCH rivela evidenza significativa di eteroschedasticità residua, con p-value < 1%.
# Questo suggerisce che il modello GARCH(2,2) con distribuzione skewed GED non ha catturato completamente la dinamica della varianza
# condizionata, e che potrebbero essere presenti effetti ARCH residui (anche) a orizzonti più lunghi.
# Perciò, possiamo concludere che anche questo modello è da scartare.

# Analizziamo il prossimo nella classifica: GARCH(1,3) sged
p <- valid_models$p[20]
q <- valid_models$q[20]
dist <- valid_models$dist[20]

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = dist
)

fit <- tryCatch(
  ugarchfit(spec = spec, data = returns, solver = "hybrid"),
  error = function(e) NULL
)

# Risultati
show(fit)
# 
# *---------------------------------*
# *          GARCH Model Fit        *
# *---------------------------------*
#   
# Conditional Variance Dynamics 	
# -----------------------------------
# GARCH Model	    : sGARCH(1,3)
# Mean Model	    : ARFIMA(0,0,0)
# Distribution	  : sged 
# 
# Optimal Parameters
# ------------------------------------
#         Estimate  Std. Error   t value Pr(>|t|)
# omega   0.061513    0.021378  2.877422 0.004009
# alpha1  0.115436    0.033358  3.460519 0.000539
# beta1   0.817783    0.040881 20.004142 0.000000
# beta2   0.000006    0.010904  0.000556 0.999557
# beta3   0.000001    0.010716  0.000081 0.999935
# skew    0.879569    0.044108 19.941492 0.000000
# shape   1.219136    0.109330 11.150955 0.000000
# 
# Robust Standard Errors:
#         Estimate  Std. Error   t value Pr(>|t|)
# omega   0.061513    0.014812  4.153036 0.000033
# alpha1  0.115436    0.038165  3.024645 0.002489
# beta1   0.817783    0.025648 31.884867 0.000000
# beta2   0.000006    0.000392  0.015471 0.987657
# beta3   0.000001    0.000811  0.001075 0.999142
# skew    0.879569    0.047008 18.710940 0.000000
# shape   1.219136    0.149898  8.133109 0.000000
# 
# LogLikelihood : -598.9084 
# 
# Information Criteria
# ------------------------------------
#   
# Akaike       2.5566
# Bayes        2.6180
# Shibata      2.5561
# Hannan-Quinn 2.5807
# 
# Weighted Ljung-Box Test on Standardized Residuals
# ------------------------------------
#                         statistic p-value
# Lag[1]                   0.003796  0.9509
# Lag[2*(p+q)+(p+q)-1][2]  0.048844  0.9572
# Lag[4*(p+q)+(p+q)-1][5]  0.641178  0.9339
# d.o.f=0
# H0 : No serial correlation
# 
# Weighted Ljung-Box Test on Standardized Squared Residuals
# ------------------------------------
#                          statistic   p-value
# Lag[1]                       1.256 2.625e-01
# Lag[2*(p+q)+(p+q)-1][11]    25.166 2.778e-05
# Lag[4*(p+q)+(p+q)-1][19]    30.058 8.925e-05
# d.o.f=4
# 
# Weighted ARCH LM Tests
# ------------------------------------
#             Statistic Shape Scale P-Value
# ARCH Lag[5]   0.01747 0.500 2.000  0.8948
# ARCH Lag[7]   0.27789 1.473 1.746  0.9534
# ARCH Lag[9]   1.30632 2.402 1.619  0.8851
# 
# Nyblom stability test
# ------------------------------------
# Joint Statistic:  3.0016
# Individual Statistics:             
# omega  0.1040
# alpha1 0.1803
# beta1  0.1077
# beta2  0.1012
# beta3  0.1025
# skew   0.1696
# shape  0.4204
# 
# Asymptotic Critical Values (10% 5% 1%)
# Joint Statistic:     	   1.69 1.9 2.35
# Individual Statistic:	   0.35 0.47 0.75
# 
# Sign Bias Test
# ------------------------------------
#                    t-value   prob sig
# Sign Bias           1.2095 0.2271    
# Negative Sign Bias  0.7375 0.4612    
# Positive Sign Bias  0.2178 0.8277    
# Joint Effect        5.5698 0.1345    
# 
# 
# Adjusted Pearson Goodness-of-Fit Test:
# ------------------------------------
#   group statistic p-value(g-1)
# 1    20     19.25      0.44097
# 2    30     37.90      0.12460
# 3    40     45.16      0.23024
# 4    50     64.19      0.07146
# 
# 
# Elapsed time : 0.313683 

# Analizzando i p-value del test pesato di Ljung-Box sui residui standardizzati, osserviamo che questi sono tutti maggiori di α = 0.01
# (in realtà anche di α = 0.05 e α = 0.10). Quindi, non abbiamo evidenza statistica sufficiente per rifiutare l’ipotesi nulla di assenza di
# autocorrelazione.

# Applichiamo, ora, dei dest di normalità sui residui.

# Estrai residui standardizzati
z_hat <- residuals(fit, standardize = TRUE)

# Prendi le date del training_set:
# Rimuoviamo le righe con NA nella colonna usata per il modello
valid_dates <- training_set$Date[!is.na(training_set$SPY_LogReturn)]

# Converti in Date se non lo sono già
valid_dates <- as.Date(valid_dates)

# Sovrascrivi l’index di z_hat con le date corrette
index(z_hat) <- valid_dates

# Controlla che le lunghezze coincidano
if(length(z_hat) != length(valid_dates)) {
  stop("Errore: lunghezza residui e date non coincide!")
}

# Visualizza i primi valori
head(z_hat)
#                  [,1]
# 2023-08-01 -0.2715260
# 2023-08-02 -1.3272829
# 2023-08-03 -0.2719337
# 2023-08-04 -0.4574297
# 2023-08-07  0.9220828
# 2023-08-08 -0.4658767

# Test di SW
shapirowilk_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Shapiro-Wilk Test
# Statistic: 0.964493
# p-value  : 2.73102e-09
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Test di JB
jarque_bera_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Jarque-Bera Test
# Statistic: 186.619456
# p-value  : 0
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Come atteso, i residui standardizzati del GARCH(1,3) con distribuzione skewed GED non seguono una distribuzione normale.

# QQ-Plot SGED:
# Residui standardizzati ordinati
y <- sort(as.numeric(z_hat))
n <- length(y)

# Parametri stimati da 'fit'
params <- coef(fit)
SGED_xi <- params["skew"]
SGED_nu <- params["shape"]
mean <- 0
sd <- 1

# Parametri della distribuzione skewed GED
distr <- "sged"
distr_pars <- list(mean = mean, sd = sd, nu = SGED_nu, xi = SGED_xi)

# Quantili teorici
quants <- qsged(ppoints(n), mean = mean, sd = sd, nu = SGED_nu, xi = SGED_xi)

# DataFrame per plotting
QQ_plot_df <- data.frame(
  T = 1:n,
  Q = quants,
  X = y,
  Y = y
)

# Calcolo retta interquartile
quart_probs <- c(0.25, 0.75)
quart_X <- as.vector(quantile(QQ_plot_df$X, quart_probs))
quart_Q <- qsged(quart_probs, mean = mean, sd = sd, nu = SGED_nu, xi = SGED_xi)
slope <- diff(quart_X) / diff(quart_Q)
intercept <- quart_X[1] - slope * quart_Q[1]

# Titoli
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 MPSMF 2024-2025",
                             paste("Q-Q plot of the Standardized Residuals from the GARCH(1,3) model for SPY (skewed GED)")))
subtitle_content <- bquote(paste("Sample size: ", .(n), 
                                 "; SGED parameters → mean = ", .(mean), 
                                 ", sd = ", .(sd), 
                                 ", shape = ", .(round(SGED_nu, 3)), 
                                 ", skew = ", .(round(SGED_xi, 3)), "."))

caption_content <- "Author: Matteo Basili"
x_name <- bquote("Theoretical Quantiles (SGED)")
y_name <- bquote("Standardized Residuals")

# Assi e legende
x_breaks <- seq(from = floor(min(QQ_plot_df$Q)), to = ceiling(max(QQ_plot_df$Q)), by = 0.5)
x_labs <- format(x_breaks, scientific = FALSE)
y_breaks_num <- length(x_breaks)
y_binwidth <- round((max(QQ_plot_df$Y) - min(QQ_plot_df$Y)) / y_breaks_num, digits = 3)
y_breaks <- round(seq(from = floor(min(QQ_plot_df$Y)/y_binwidth)*y_binwidth, 
                      to = ceiling(max(QQ_plot_df$Y)/y_binwidth)*y_binwidth, 
                      by = y_binwidth), 3)
y_labs <- format(y_breaks, scientific = FALSE)

# Legende e colori
leg_shape_labs <- bquote("Q-Q plot")
leg_fill_labs <- c(bquote("90% confidence interval"), bquote("95% confidence interval"))
leg_col_labs <- c(bquote("Interquartile line"), bquote("Regression line"), bquote("y = x"))

Stand_Res_std_QQ_plot <- ggplot(QQ_plot_df) + 
  stat_qq_band(aes(sample = X, fill = "95"), distribution = distr, dparams = distr_pars, conf = 0.95) +
  stat_qq_band(aes(sample = X, fill = "90"), distribution = distr, dparams = distr_pars, conf = 0.90) +
  geom_abline(aes(slope = slope, intercept = intercept, colour = "IQR"), linewidth = 0.8) +
  stat_smooth(aes(x = Q, y = Y, colour = "Reg", group = 1), method = "lm", se = FALSE, linewidth = 0.8) +
  geom_abline(aes(slope = 1, intercept = 0, colour = "45deg"), linewidth = 0.8) +
  stat_qq_point(aes(sample = X), shape = 19, color = "black", size = 1.2, alpha = 0.8, distribution = distr, dparams = distr_pars) +
  scale_x_continuous(name = x_name, breaks = x_breaks, labels = x_labs) +
  scale_y_continuous(name = y_name, breaks = y_breaks, labels = NULL,
                     sec.axis = sec_axis(~ ., breaks = y_breaks, labels = y_labs)) +
  ggtitle(title_content) +
  labs(subtitle = subtitle_content, caption = caption_content) +
  scale_shape_manual(name = "", labels = leg_shape_labs, values = c("19")) +
  scale_fill_manual(name = "", values = c("90" = "chartreuse1", "95" = "deepskyblue1"), labels = leg_fill_labs) +
  scale_colour_manual(name = "", values = c("IQR" = "cyan", "Reg" = "red", "45deg" = "black"), labels = leg_col_labs) +
  guides(shape = guide_legend(order = 1), fill = guide_legend(order = 2), colour = guide_legend(order = 3)) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.x = element_text(angle = 0, vjust = 1),
    legend.key.width = unit(0.8, "cm"),
    legend.position = "bottom"
  )

# Mostra il plot
plot(Stand_Res_std_QQ_plot)

# Calcola densità teorica nei quantili (SGED)
dens <- dsged(quants, mean = mean, sd = sd, nu = SGED_nu, xi = SGED_xi)

# Probabilità associate ai quantili
p <- ppoints(n)

# Errore standard dei quantili teorici
SE <- sqrt(p * (1 - p) / (n * dens^2))

# Limiti di confidenza al 95%
alpha_95 <- 0.05
z_95 <- qnorm(1 - alpha_95 / 2)
lower_95 <- quants - z_95 * SE
upper_95 <- quants + z_95 * SE

# Percentuale di punti dentro la banda 95%
inside_95 <- (QQ_plot_df$X >= lower_95) & (QQ_plot_df$X <= upper_95)
percent_inside_95 <- mean(inside_95) * 100
cat(sprintf("Percentuale di punti dentro la banda di confidenza al 95%%: %.2f%%\n", percent_inside_95))
# Percentuale di punti dentro la banda di confidenza al 95%: 97.26%

# Limiti di confidenza al 90%
alpha_90 <- 0.10
z_90 <- qnorm(1 - alpha_90 / 2)
lower_90 <- quants - z_90 * SE
upper_90 <- quants + z_90 * SE

# Percentuale di punti dentro la banda 90%
inside_90 <- (QQ_plot_df$X >= lower_90) & (QQ_plot_df$X <= upper_90)
percent_inside_90 <- mean(inside_90) * 100
cat(sprintf("Percentuale di punti dentro la banda di confidenza al 90%%: %.2f%%\n", percent_inside_90))
# Percentuale di punti dentro la banda di confidenza al 90%: 84.60%

# Kolmogorov-Smirnov test (K-S test)
ks_result <- ks.test(y, 
                     y = function(q) psged(q, mean=0, sd=1, nu=SGED_nu, xi=SGED_xi),
                     alternative = "two.sided")

print(ks_result)
# 
#         Asymptotic one-sample Kolmogorov-Smirnov test
# 
# data:  y
# D = 0.04847, p-value = 0.2154
# alternative hypothesis: two-sided

# Dall'analisi del Q-Q Plot e del test di K-S vediamo, quindi, che i residui standardizzati seguono la distribuzione skewed GED con un livello
# di significatività del 5% (in realtà il test di K-S risulta anche al 10%).

# Verifichiamo ora la stazionarietà e l'omoschedasticità (non condizionata).

# Grafico dei residui
df_residui <- data.frame(
  Date = index(z_hat),
  Residui = as.numeric(z_hat)
)

ggplot(df_residui, aes(x = Date, y = Residui)) +
  geom_line(color = "steelblue", linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  labs(
    title = "Grafico dei residui standardizzati",
    subtitle = "Modello GARCH(1,3) con distribuzione skewed GED per SPY",
    x = "Data",
    y = "Residui standardizzati"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    axis.title = element_text(size = 11)
  )

# KPSS Test
kpss_test_level(df_residui, col = "Residui", alpha = 0.01)
# 
# [Residui] KPSS Test - Stazionarietà attorno a una media costante
# Statistic: 0.124907
# Valore critico (circa 1%): 0.739000
# Interpretazione del p-value: p-value > 0.10
# 
# ** CONCLUSIONE: Statistic <= valore critico (1%) **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO stazionari.

# Quindi, i residui standardizzati sono stazionari.

# Per l'omoschedasticità non condizionata, per prima cosa, vediamo se c'è curtosi nei residui standardizzati.

# Calcola la curtosi
kurt_z_hat <- kurtosis(as.numeric(z_hat), na.rm = TRUE)

cat(sprintf("Curtosi dei residui standardizzati: %.4f\n", kurt_z_hat))
# Curtosi dei residui standardizzati: 5.8525

# La curtosi c'è. Quindi, applichiamo la forma "studentized" dei test di Breusch-Pagan e White per la stazionarietà in varianza.

# Breusch-Pagan Test:
# Crea un indice temporale (può essere anche un numero progressivo)
df_residui$Index <- seq_along(df_residui$Residui)

# Esegui il test BP
breusch_pagan_test(
  index = df_residui$Index,
  group = df_residui$Residui,
  col = "Residui standardizzati"
)
# [Residui standardizzati] Breusch-Pagan Test
# Statistic: 3.767055
# p-value: 0.0522716
# 
# ** Conclusione: p-value > 0.01 **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON sono eteroschedastici.

# White Test
white_test(
  index = df_residui$Index,
  group = df_residui$Residui,
  col = "Residui standardizzati"
)
# [Residui standardizzati] White Test
# Statistic: 4.421627
# p-value: 0.109611
# 
# ** Conclusione: p-value > 0.01 **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON sono eteroschedastici.

# Dai risultati dei test si vede che è verificata anche la condizione di omoschedasticità (non condizionata).

# Per verificare ulteriormente l'assenza di autocorrelazione per i residui standardizzati
# di questo modello, facciamo anche l'ACF, il PACF e il test di Ljung-Box classico.

# ACF
df_z <- data.frame(z_hat = as.numeric(z_hat))
plot_acf_residuals(df_z, col = "z_hat")

# PACF
plot_pacf_residuals(df_z, col = "z_hat")

# L’analisi visiva di ACF e PACF mostra che, in entrambi i grafici, nessuna barra supera la banda di confidenza al 90%.
# Dunque, non c'è una forte evidenza visiva di autocorrelazione.

# Ljung-Box Test
ljungbox_test(as.numeric(z_hat), col_name = "Residui standardizzati")
# Ljung-Box test per Residui standardizzati (max_lag = 10):
#            lag     lb_stat lb_pvalue
# X-squared    1 0.003796298 0.9508701
# X-squared1   2 0.093890860 0.9541395
# X-squared2   3 0.758474981 0.8593713
# X-squared3   4 1.153258813 0.8857326
# X-squared4   5 1.196466819 0.9452159
# X-squared5   6 1.933250697 0.9257340
# X-squared6   7 2.355688205 0.9375664
# X-squared7   8 2.420275149 0.9653450
# X-squared8   9 2.884326068 0.9687048
# X-squared9  10 2.952657036 0.9825163

# Osservando i p-value associati ai vari lag, vediamo che tutti questi sono maggiori di α = 0.10. Dunque, non abbiamo evidenza statistica
# sufficiente per rifiutare l’ipotesi nulla di assenza di autocorrelazione fino al lag specificato.

# Infine, verifichiamo anche l’assenza di eteroschedasticità condizionata nei residui standardizzati.

# Test ARCH di Engle
arch_effects_test(z_hat, col_name = "Residui Standardizzati")
# ARCH Test per Residui Standardizzati (max_lag = 5):
#            lag arch_stat  arch_pvalue
# statistic    1  1.676618 1.953745e-01
# statistic1   2  3.870656 1.443769e-01
# statistic2   3  4.917365 1.779491e-01
# statistic3   4 30.777696 3.398260e-06
# statistic4   5 30.789215 1.030900e-05

# Alla luce dei risultati, a partire dal lag 4, il test ARCH rivela evidenza significativa di eteroschedasticità residua, con p-value < 1%.
# Questo suggerisce che il modello GARCH(1,3) con distribuzione skewed GED non ha catturato completamente la dinamica della varianza
# condizionata, e che potrebbero essere presenti effetti ARCH residui (anche) a orizzonti più lunghi.
# Perciò, possiamo concludere che anche questo modello è da scartare.

# Analizziamo il prossimo nella classifica: GARCH(3,1) norm
p <- valid_models$p[21]
q <- valid_models$q[21]
dist <- valid_models$dist[21]

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = dist
)

fit <- tryCatch(
  ugarchfit(spec = spec, data = returns, solver = "hybrid"),
  error = function(e) NULL
)

# Risultati
show(fit)
# 
# *---------------------------------*
# *          GARCH Model Fit        *
# *---------------------------------*
#   
# Conditional Variance Dynamics 	
# -----------------------------------
# GARCH Model	    : sGARCH(3,1)
# Mean Model	    : ARFIMA(0,0,0)
# Distribution	  : norm 
# 
# Optimal Parameters
# ------------------------------------
#         Estimate  Std. Error  t value Pr(>|t|)
# omega   0.105367    0.045403   2.3207 0.020303
# alpha1  0.068542    0.028427   2.4111 0.015904
# alpha2  0.000000    0.080827   0.0000 1.000000
# alpha3  0.151382    0.071307   2.1230 0.033756
# beta1   0.672040    0.090561   7.4209 0.000000
# 
# Robust Standard Errors:
#         Estimate  Std. Error  t value Pr(>|t|)
# omega   0.105367    0.066073   1.5947 0.110776
# alpha1  0.068542    0.040936   1.6744 0.094056
# alpha2  0.000000    0.130703   0.0000 1.000000
# alpha3  0.151382    0.120751   1.2537 0.209960
# beta1   0.672040    0.116040   5.7915 0.000000
# 
# LogLikelihood : -616.8266 
# 
# Information Criteria
# ------------------------------------
#   
# Akaike       2.6237
# Bayes        2.6676
# Shibata      2.6235
# Hannan-Quinn 2.6410
# 
# Weighted Ljung-Box Test on Standardized Residuals
# ------------------------------------
#                         statistic p-value
# Lag[1]                    0.05764  0.8103
# Lag[2*(p+q)+(p+q)-1][2]   0.09403  0.9239
# Lag[4*(p+q)+(p+q)-1][5]   0.46405  0.9631
# d.o.f=0
# H0 : No serial correlation
# 
# Weighted Ljung-Box Test on Standardized Squared Residuals
# ------------------------------------
#                          statistic p-value
# Lag[1]                       3.586 0.05827
# Lag[2*(p+q)+(p+q)-1][11]    10.555 0.07399
# Lag[4*(p+q)+(p+q)-1][19]    12.754 0.20996
# d.o.f=4
# 
# Weighted ARCH LM Tests
# ------------------------------------
#             Statistic Shape Scale P-Value
# ARCH Lag[5]   0.04383 0.500 2.000  0.8342
# ARCH Lag[7]   0.17395 1.473 1.746  0.9758
# ARCH Lag[9]   0.86081 2.402 1.619  0.9492
# 
# Nyblom stability test
# ------------------------------------
# Joint Statistic:  1.6403
# Individual Statistics:             
# omega  0.2839
# alpha1 0.3041
# alpha2 0.7022
# alpha3 0.4016
# beta1  0.2930
# 
# Asymptotic Critical Values (10% 5% 1%)
# Joint Statistic:     	   1.28 1.47 1.88
# Individual Statistic:	   0.35 0.47 0.75
# 
# Sign Bias Test
# ------------------------------------
#                    t-value    prob sig
# Sign Bias          1.29581 0.19568    
# Negative Sign Bias 1.14658 0.25214    
# Positive Sign Bias 0.02909 0.97681    
# Joint Effect       7.66552 0.05345   *
#   
#   
# Adjusted Pearson Goodness-of-Fit Test:
# ------------------------------------
#   group statistic p-value(g-1)
# 1    20     39.33     0.003999
# 2    30     54.86     0.002572
# 3    40     58.66     0.022393
# 4    50     80.43     0.003092
# 
# 
# Elapsed time : 0.09960079 

# Analizzando i p-value del test pesato di Ljung-Box sui residui standardizzati, osserviamo che questi sono tutti maggiori di α = 0.01
# (in realtà anche di α = 0.05 e α = 0.10). Quindi, non abbiamo evidenza statistica sufficiente per rifiutare l’ipotesi nulla di
# assenza di autocorrelazione.

# Applichiamo, ora, dei dest di normalità sui residui.

# Estrai residui standardizzati
z_hat <- residuals(fit, standardize = TRUE)

# Prendi le date del training_set:
# Rimuoviamo le righe con NA nella colonna usata per il modello
valid_dates <- training_set$Date[!is.na(training_set$SPY_LogReturn)]

# Converti in Date se non lo sono già
valid_dates <- as.Date(valid_dates)

# Sovrascrivi l’index di z_hat con le date corrette
index(z_hat) <- valid_dates

# Controlla che le lunghezze coincidano
if(length(z_hat) != length(valid_dates)) {
  stop("Errore: lunghezza residui e date non coincide!")
}

# Visualizza i primi valori
head(z_hat)
#                  [,1]
# 2023-08-01 -0.2715260
# 2023-08-02 -1.3272829
# 2023-08-03 -0.2719337
# 2023-08-04 -0.4854058
# 2023-08-07  0.8679370
# 2023-08-08 -0.4745663

# Test di SW
shapirowilk_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Shapiro-Wilk Test
# Statistic: 0.965684
# p-value  : 4.44448e-09
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Test di JB
jarque_bera_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Jarque-Bera Test
# Statistic: 155.310919
# p-value  : 0
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Come vediamo, i residui standardizzati del GARCH(3,1) con distribuzione normale non seguono una distribuzione normale.
# Perciò, possiamo concludere che anche questo modello è da scartare.

# Analizziamo il prossimo nella classifica: GARCH(4,0) sged
p <- valid_models$p[22]
q <- valid_models$q[22]
dist <- valid_models$dist[22]

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = dist
)

fit <- tryCatch(
  ugarchfit(spec = spec, data = returns, solver = "hybrid"),
  error = function(e) NULL
)

# Risultati
show(fit)
# 
# *---------------------------------*
# *          GARCH Model Fit        *
# *---------------------------------*
#   
# Conditional Variance Dynamics 	
# -----------------------------------
# GARCH Model	    : sGARCH(4,0)
# Mean Model	    : ARFIMA(0,0,0)
# Distribution	  : sged 
# 
# Optimal Parameters
# ------------------------------------
#         Estimate  Std. Error  t value Pr(>|t|)
# omega    0.49031    0.074166  6.61096 0.000000
# alpha1   0.09217    0.051897  1.77601 0.075731
# alpha2   0.03518    0.049702  0.70782 0.479056
# alpha3   0.15090    0.074679  2.02061 0.043321
# alpha4   0.17451    0.078474  2.22379 0.026163
# skew     0.88289    0.040314 21.90022 0.000000
# shape    1.22502    0.114124 10.73410 0.000000
# 
# Robust Standard Errors:
#         Estimate  Std. Error  t value Pr(>|t|)
# omega    0.49031    0.068560  7.15153 0.000000
# alpha1   0.09217    0.049894  1.84732 0.064701
# alpha2   0.03518    0.041299  0.85184 0.394304
# alpha3   0.15090    0.061579  2.45048 0.014267
# alpha4   0.17451    0.094984  1.83726 0.066172
# skew     0.88289    0.040180 21.97321 0.000000
# shape    1.22502    0.141308  8.66917 0.000000
# 
# LogLikelihood : -599.5733 
# 
# Information Criteria
# ------------------------------------
#   
# Akaike       2.5594
# Bayes        2.6208
# Shibata      2.5590
# Hannan-Quinn 2.5835
# 
# Weighted Ljung-Box Test on Standardized Residuals
# ------------------------------------
#                         statistic p-value
# Lag[1]                    0.01938  0.8893
# Lag[2*(p+q)+(p+q)-1][2]   0.12575  0.9020
# Lag[4*(p+q)+(p+q)-1][5]   0.89022  0.8840
# d.o.f=0
# H0 : No serial correlation
# 
# Weighted Ljung-Box Test on Standardized Squared Residuals
# ------------------------------------
#                          statistic p-value
# Lag[1]                       2.153  0.1423
# Lag[2*(p+q)+(p+q)-1][11]     6.865  0.3280
# Lag[4*(p+q)+(p+q)-1][19]    10.290  0.4203
# d.o.f=4
# 
# Weighted ARCH LM Tests
# ------------------------------------
#             Statistic Shape Scale P-Value
# ARCH Lag[5]   0.08928 0.500 2.000  0.7651
# ARCH Lag[7]   1.46000 1.473 1.746  0.6330
# ARCH Lag[9]   2.49317 2.402 1.619  0.6620
# 
# Nyblom stability test
# ------------------------------------
# Joint Statistic:  1.426
# Individual Statistics:              
# omega  0.27573
# alpha1 0.17274
# alpha2 0.09834
# alpha3 0.27530
# alpha4 0.55326
# skew   0.19975
# shape  0.21452
# 
# Asymptotic Critical Values (10% 5% 1%)
# Joint Statistic:     	   1.69 1.9 2.35
# Individual Statistic:	   0.35 0.47 0.75
# 
# Sign Bias Test
# ------------------------------------
#                    t-value    prob sig
# Sign Bias           1.1597 0.24674    
# Negative Sign Bias  1.2052 0.22872    
# Positive Sign Bias  0.2122 0.83206    
# Joint Effect        7.6491 0.05385   *
#   
#   
# Adjusted Pearson Goodness-of-Fit Test:
# ------------------------------------
#   group statistic p-value(g-1)
# 1    20     22.46      0.26217
# 2    30     34.10      0.23557
# 3    40     43.47      0.28682
# 4    50     65.45      0.05807
# 
# 
# Elapsed time : 0.8967829  

# Analizzando i p-value del test pesato di Ljung-Box sui residui standardizzati, osserviamo che questi sono tutti maggiori di α = 0.01
# (in realtà anche di α = 0.05 e α = 0.10). Quindi, non abbiamo evidenza statistica sufficiente per rifiutare l’ipotesi nulla di assenza di
# autocorrelazione.

# Applichiamo, ora, dei dest di normalità sui residui.

# Estrai residui standardizzati
z_hat <- residuals(fit, standardize = TRUE)

# Prendi le date del training_set:
# Rimuoviamo le righe con NA nella colonna usata per il modello
valid_dates <- training_set$Date[!is.na(training_set$SPY_LogReturn)]

# Converti in Date se non lo sono già
valid_dates <- as.Date(valid_dates)

# Sovrascrivi l’index di z_hat con le date corrette
index(z_hat) <- valid_dates

# Controlla che le lunghezze coincidano
if(length(z_hat) != length(valid_dates)) {
  stop("Errore: lunghezza residui e date non coincide!")
}

# Visualizza i primi valori
head(z_hat)
#                  [,1]
# 2023-08-01 -0.2715260
# 2023-08-02 -1.3272829
# 2023-08-03 -0.2719337
# 2023-08-04 -0.4294846
# 2023-08-07  0.9581896
# 2023-08-08 -0.4538712

# Test di SW
shapirowilk_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Shapiro-Wilk Test
# Statistic: 0.971016
# p-value  : 4.50416e-08
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Test di JB
jarque_bera_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Jarque-Bera Test
# Statistic: 100.541327
# p-value  : 0
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Come atteso, i residui standardizzati del GARCH(4,0) con distribuzione skewed GED non seguono una distribuzione normale.

# QQ-Plot SGED:
# Residui standardizzati ordinati
y <- sort(as.numeric(z_hat))
n <- length(y)

# Parametri stimati da 'fit'
params <- coef(fit)
SGED_xi <- params["skew"]
SGED_nu <- params["shape"]
mean <- 0
sd <- 1

# Parametri della distribuzione skewed GED
distr <- "sged"
distr_pars <- list(mean = mean, sd = sd, nu = SGED_nu, xi = SGED_xi)

# Quantili teorici
quants <- qsged(ppoints(n), mean = mean, sd = sd, nu = SGED_nu, xi = SGED_xi)

# DataFrame per plotting
QQ_plot_df <- data.frame(
  T = 1:n,
  Q = quants,
  X = y,
  Y = y
)

# Calcolo retta interquartile
quart_probs <- c(0.25, 0.75)
quart_X <- as.vector(quantile(QQ_plot_df$X, quart_probs))
quart_Q <- qsged(quart_probs, mean = mean, sd = sd, nu = SGED_nu, xi = SGED_xi)
slope <- diff(quart_X) / diff(quart_Q)
intercept <- quart_X[1] - slope * quart_Q[1]

# Titoli
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 MPSMF 2024-2025",
                             paste("Q-Q plot of the Standardized Residuals from the GARCH(4,0) model for SPY (skewed GED)")))
subtitle_content <- bquote(paste("Sample size: ", .(n), 
                                 "; SGED parameters → mean = ", .(mean), 
                                 ", sd = ", .(sd), 
                                 ", shape = ", .(round(SGED_nu, 3)), 
                                 ", skew = ", .(round(SGED_xi, 3)), "."))

caption_content <- "Author: Matteo Basili"
x_name <- bquote("Theoretical Quantiles (SGED)")
y_name <- bquote("Standardized Residuals")

# Assi e legende
x_breaks <- seq(from = floor(min(QQ_plot_df$Q)), to = ceiling(max(QQ_plot_df$Q)), by = 0.5)
x_labs <- format(x_breaks, scientific = FALSE)
y_breaks_num <- length(x_breaks)
y_binwidth <- round((max(QQ_plot_df$Y) - min(QQ_plot_df$Y)) / y_breaks_num, digits = 3)
y_breaks <- round(seq(from = floor(min(QQ_plot_df$Y)/y_binwidth)*y_binwidth, 
                      to = ceiling(max(QQ_plot_df$Y)/y_binwidth)*y_binwidth, 
                      by = y_binwidth), 3)
y_labs <- format(y_breaks, scientific = FALSE)

# Legende e colori
leg_shape_labs <- bquote("Q-Q plot")
leg_fill_labs <- c(bquote("90% confidence interval"), bquote("95% confidence interval"))
leg_col_labs <- c(bquote("Interquartile line"), bquote("Regression line"), bquote("y = x"))

Stand_Res_std_QQ_plot <- ggplot(QQ_plot_df) + 
  stat_qq_band(aes(sample = X, fill = "95"), distribution = distr, dparams = distr_pars, conf = 0.95) +
  stat_qq_band(aes(sample = X, fill = "90"), distribution = distr, dparams = distr_pars, conf = 0.90) +
  geom_abline(aes(slope = slope, intercept = intercept, colour = "IQR"), linewidth = 0.8) +
  stat_smooth(aes(x = Q, y = Y, colour = "Reg", group = 1), method = "lm", se = FALSE, linewidth = 0.8) +
  geom_abline(aes(slope = 1, intercept = 0, colour = "45deg"), linewidth = 0.8) +
  stat_qq_point(aes(sample = X), shape = 19, color = "black", size = 1.2, alpha = 0.8, distribution = distr, dparams = distr_pars) +
  scale_x_continuous(name = x_name, breaks = x_breaks, labels = x_labs) +
  scale_y_continuous(name = y_name, breaks = y_breaks, labels = NULL,
                     sec.axis = sec_axis(~ ., breaks = y_breaks, labels = y_labs)) +
  ggtitle(title_content) +
  labs(subtitle = subtitle_content, caption = caption_content) +
  scale_shape_manual(name = "", labels = leg_shape_labs, values = c("19")) +
  scale_fill_manual(name = "", values = c("90" = "chartreuse1", "95" = "deepskyblue1"), labels = leg_fill_labs) +
  scale_colour_manual(name = "", values = c("IQR" = "cyan", "Reg" = "red", "45deg" = "black"), labels = leg_col_labs) +
  guides(shape = guide_legend(order = 1), fill = guide_legend(order = 2), colour = guide_legend(order = 3)) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.x = element_text(angle = 0, vjust = 1),
    legend.key.width = unit(0.8, "cm"),
    legend.position = "bottom"
  )

# Mostra il plot
plot(Stand_Res_std_QQ_plot)

# Calcola densità teorica nei quantili (SGED)
dens <- dsged(quants, mean = mean, sd = sd, nu = SGED_nu, xi = SGED_xi)

# Probabilità associate ai quantili
p <- ppoints(n)

# Errore standard dei quantili teorici
SE <- sqrt(p * (1 - p) / (n * dens^2))

# Limiti di confidenza al 95%
alpha_95 <- 0.05
z_95 <- qnorm(1 - alpha_95 / 2)
lower_95 <- quants - z_95 * SE
upper_95 <- quants + z_95 * SE

# Percentuale di punti dentro la banda 95%
inside_95 <- (QQ_plot_df$X >= lower_95) & (QQ_plot_df$X <= upper_95)
percent_inside_95 <- mean(inside_95) * 100
cat(sprintf("Percentuale di punti dentro la banda di confidenza al 95%%: %.2f%%\n", percent_inside_95))
# Percentuale di punti dentro la banda di confidenza al 95%: 96.41%

# Limiti di confidenza al 90%
alpha_90 <- 0.10
z_90 <- qnorm(1 - alpha_90 / 2)
lower_90 <- quants - z_90 * SE
upper_90 <- quants + z_90 * SE

# Percentuale di punti dentro la banda 90%
inside_90 <- (QQ_plot_df$X >= lower_90) & (QQ_plot_df$X <= upper_90)
percent_inside_90 <- mean(inside_90) * 100
cat(sprintf("Percentuale di punti dentro la banda di confidenza al 90%%: %.2f%%\n", percent_inside_90))
# Percentuale di punti dentro la banda di confidenza al 90%: 79.54%

# Kolmogorov-Smirnov test (K-S test)
ks_result <- ks.test(y, 
                     y = function(q) psged(q, mean=0, sd=1, nu=SGED_nu, xi=SGED_xi),
                     alternative = "two.sided")

print(ks_result)
# 
#         Asymptotic one-sample Kolmogorov-Smirnov test
# 
# data:  y
# D = 0.048102, p-value = 0.2227
# alternative hypothesis: two-sided

# Dall'analisi del Q-Q Plot e del test di K-S vediamo, quindi, che i residui standardizzati seguono la distribuzione skewed GED con un livello
# di significatività del 5% (in realtà il test di K-S risulta anche al 10%).

# Verifichiamo ora la stazionarietà e l'omoschedasticità (non condizionata).

# Grafico dei residui
df_residui <- data.frame(
  Date = index(z_hat),
  Residui = as.numeric(z_hat)
)

ggplot(df_residui, aes(x = Date, y = Residui)) +
  geom_line(color = "steelblue", linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  labs(
    title = "Grafico dei residui standardizzati",
    subtitle = "Modello GARCH(4,0) con distribuzione skewed GED per SPY",
    x = "Data",
    y = "Residui standardizzati"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    axis.title = element_text(size = 11)
  )

# KPSS Test
kpss_test_level(df_residui, col = "Residui", alpha = 0.01)
# 
# [Residui] KPSS Test - Stazionarietà attorno a una media costante
# Statistic: 0.117720
# Valore critico (circa 1%): 0.739000
# Interpretazione del p-value: p-value > 0.10
# 
# ** CONCLUSIONE: Statistic <= valore critico (1%) **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO stazionari.

# Quindi, i residui standardizzati sono stazionari.

# Per l'omoschedasticità non condizionata, per prima cosa, vediamo se c'è curtosi nei residui standardizzati.

# Calcola la curtosi
kurt_z_hat <- kurtosis(as.numeric(z_hat), na.rm = TRUE)

cat(sprintf("Curtosi dei residui standardizzati: %.4f\n", kurt_z_hat))
# Curtosi dei residui standardizzati: 5.0007

# La curtosi c'è. Quindi, applichiamo la forma "studentized" dei test di Breusch-Pagan e White per la stazionarietà in varianza.

# Breusch-Pagan Test:
# Crea un indice temporale (può essere anche un numero progressivo)
df_residui$Index <- seq_along(df_residui$Residui)

# Esegui il test BP
breusch_pagan_test(
  index = df_residui$Index,
  group = df_residui$Residui,
  col = "Residui standardizzati"
)
# [Residui standardizzati] Breusch-Pagan Test
# Statistic: 7.104960
# p-value: 0.0076871
# 
# ** Conclusione: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO eteroschedastici.

# White Test
white_test(
  index = df_residui$Index,
  group = df_residui$Residui,
  col = "Residui standardizzati"
)
# [Residui standardizzati] White Test
# Statistic: 9.190087
# p-value: 0.0101018
# 
# ** Conclusione: p-value > 0.01 **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON sono eteroschedastici.

# Visto che i due test ci forniscono risultati contrastanti, riandiamo a vedere graficamente i residui per vedere se possiamo trarre delle
# conclusioni (questa volta con uno scatterplot).

# DataFrame con residui e date
df_resid <- data.frame(
  Date = index(z_hat),
  Residuals = as.numeric(z_hat)
)

# Scatterplot dei residui nel tempo
ggplot(df_resid, aes(x = Date, y = Residuals)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Residui Standardizzati nel Tempo",
    subtitle = "Modello GARCH(4,0) con distribuzione skewed GED per SPY",
    x = "Data",
    y = "Residui Standardizzati"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11)
  )

# Dal grafico, negli ultimi mesi, si riesce a vedere una leggera volatilità crescente. Quindi, abbiamo un'evidenza
# statistica (non fortissima) di eteroschedasticità non condizionata.
# Perciò, possiamo concludere che anche questo modello è da scartare.

# Analizziamo il prossimo nella classifica: GARCH(4,0) sstd
p <- valid_models$p[23]
q <- valid_models$q[23]
dist <- valid_models$dist[23]

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = dist
)

fit <- tryCatch(
  ugarchfit(spec = spec, data = returns, solver = "hybrid"),
  error = function(e) NULL
)

# Risultati
show(fit)
# 
# *---------------------------------*
# *          GARCH Model Fit        *
# *---------------------------------*
#   
# Conditional Variance Dynamics 	
# -----------------------------------
# GARCH Model	    : sGARCH(4,0)
# Mean Model	    : ARFIMA(0,0,0)
# Distribution	  : sstd 
# 
# Optimal Parameters
# ------------------------------------
#         Estimate  Std. Error  t value Pr(>|t|)
# omega   0.550343    0.098412  5.59223 0.000000
# alpha1  0.086231    0.059524  1.44866 0.147432
# alpha2  0.038448    0.049361  0.77891 0.436032
# alpha3  0.193145    0.089171  2.16602 0.030310
# alpha4  0.126757    0.081935  1.54704 0.121853
# skew    0.820005    0.050807 16.13965 0.000000
# shape   4.877828    1.214708  4.01564 0.000059
# 
# Robust Standard Errors:
#         Estimate  Std. Error  t value Pr(>|t|)
# omega   0.550343    0.093176  5.90649 0.000000
# alpha1  0.086231    0.056684  1.52125 0.128198
# alpha2  0.038448    0.042938  0.89542 0.370564
# alpha3  0.193145    0.084066  2.29754 0.021588
# alpha4  0.126757    0.097595  1.29881 0.194008
# skew    0.820005    0.046371 17.68367 0.000000
# shape   4.877828    1.370169  3.56002 0.000371
# 
# LogLikelihood : -599.9464 
# 
# Information Criteria
# ------------------------------------
#   
# Akaike       2.5610
# Bayes        2.6224
# Shibata      2.5605
# Hannan-Quinn 2.5851
# 
# Weighted Ljung-Box Test on Standardized Residuals
# ------------------------------------
#                         statistic p-value
# Lag[1]                    0.05963  0.8071
# Lag[2*(p+q)+(p+q)-1][2]   0.18259  0.8650
# Lag[4*(p+q)+(p+q)-1][5]   0.94246  0.8726
# d.o.f=0
# H0 : No serial correlation
# 
# Weighted Ljung-Box Test on Standardized Squared Residuals
# ------------------------------------
#                          statistic p-value
# Lag[1]                       4.646 0.03113
# Lag[2*(p+q)+(p+q)-1][11]    11.780 0.04200
# Lag[4*(p+q)+(p+q)-1][19]    15.613 0.07784
# d.o.f=4
# 
# Weighted ARCH LM Tests
# ------------------------------------
#             Statistic Shape Scale P-Value
# ARCH Lag[5]    0.2201 0.500 2.000  0.6389
# ARCH Lag[7]    1.3379 1.473 1.746  0.6649
# ARCH Lag[9]    2.2776 2.402 1.619  0.7042
# 
# Nyblom stability test
# ------------------------------------
# Joint Statistic:  1.4046
# Individual Statistics:              
# omega  0.12876
# alpha1 0.13099
# alpha2 0.06633
# alpha3 0.16872
# alpha4 0.40262
# skew   0.06275
# shape  0.08924
# 
# Asymptotic Critical Values (10% 5% 1%)
# Joint Statistic:     	   1.69 1.9 2.35
# Individual Statistic:	   0.35 0.47 0.75
# 
# Sign Bias Test
# ------------------------------------
#                    t-value    prob sig
# Sign Bias          0.96281 0.33614    
# Negative Sign Bias 1.46329 0.14406    
# Positive Sign Bias 0.03611 0.97121    
# Joint Effect       7.41571 0.05976   *
#   
#   
# Adjusted Pearson Goodness-of-Fit Test:
# ------------------------------------
#   group statistic p-value(g-1)
# 1    20     27.86      0.08623
# 2    30     41.57      0.06136
# 3    40     50.89      0.09614
# 4    50     50.68      0.40696
# 
# 
# Elapsed time : 0.2152689

# Analizzando i p-value del test pesato di Ljung-Box sui residui standardizzati, osserviamo che questi sono tutti maggiori di α = 0.01 (in realtà
# anche di α = 0.05 e α = 0.10). Quindi, non abbiamo evidenza statistica sufficiente per rifiutare l’ipotesi nulla di assenza di autocorrelazione.

# Applichiamo, ora, dei dest di normalità sui residui.

# Estrai residui standardizzati
z_hat <- residuals(fit, standardize = TRUE)

# Prendi le date del training_set:
# Rimuoviamo le righe con NA nella colonna usata per il modello
valid_dates <- training_set$Date[!is.na(training_set$SPY_LogReturn)]

# Converti in Date se non lo sono già
valid_dates <- as.Date(valid_dates)

# Sovrascrivi l’index di z_hat con le date corrette
index(z_hat) <- valid_dates

# Controlla che le lunghezze coincidano
if(length(z_hat) != length(valid_dates)) {
  stop("Errore: lunghezza residui e date non coincide!")
}

# Visualizza i primi valori
head(z_hat)
#                  [,1]
# 2023-08-01 -0.2715260
# 2023-08-02 -1.3272829
# 2023-08-03 -0.2719337
# 2023-08-04 -0.4294846
# 2023-08-07  0.8866645
# 2023-08-08 -0.4624798

# Test di SW
shapirowilk_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Shapiro-Wilk Test
# Statistic: 0.967658
# p-value  : 1.01977e-08
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Test di JB
jarque_bera_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Jarque-Bera Test
# Statistic: 132.292441
# p-value  : 0
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Come atteso, i residui standardizzati del GARCH(4,0) con distribuzione skewed Student-t non seguono una distribuzione normale.

# QQ-Plot SSTD:
# Residui standardizzati ordinati
y <- sort(as.numeric(z_hat))
n <- length(y)

# Parametri stimati da 'fit'
params <- coef(fit)
SSTD_xi <- params["skew"]
SSTD_nu <- params["shape"]
mean <- 0
sd <- 1

# Parametri della distribuzione skewed Student-t
distr <- "sstd"
distr_pars <- list(mean = mean, sd = sd, nu = SSTD_nu, xi = SSTD_xi)

# Quantili teorici
quants <- qsstd(ppoints(n), mean = mean, sd = sd, nu = SSTD_nu, xi = SSTD_xi)

# DataFrame per plotting
QQ_plot_df <- data.frame(
  T = 1:n,
  Q = quants,
  X = y,
  Y = y
)

# Calcolo retta interquartile
quart_probs <- c(0.25, 0.75)
quart_X <- as.vector(quantile(QQ_plot_df$X, quart_probs))
quart_Q <- qsstd(quart_probs, mean = mean, sd = sd, nu = SSTD_nu, xi = SSTD_xi)
slope <- diff(quart_X) / diff(quart_Q)
intercept <- quart_X[1] - slope * quart_Q[1]

# Titoli
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 MPSMF 2024-2025",
                             paste("Q-Q plot of the Standardized Residuals from the GARCH(4,0) model for SPY (skewed Student-t)")))
subtitle_content <- bquote(paste("Sample size: ", .(n), 
                                 "; SSTD parameters → mean = ", .(mean), 
                                 ", sd = ", .(sd), 
                                 ", df = ", .(round(SSTD_nu, 3)), 
                                 ", skew = ", .(round(SSTD_xi, 3)), "."))

caption_content <- "Author: Matteo Basili"
x_name <- bquote("Theoretical Quantiles (SSTD)")
y_name <- bquote("Standardized Residuals")

# Assi e legende
x_breaks <- seq(from = floor(min(QQ_plot_df$Q)), to = ceiling(max(QQ_plot_df$Q)), by = 0.5)
x_labs <- format(x_breaks, scientific = FALSE)
y_breaks_num <- length(x_breaks)
y_binwidth <- round((max(QQ_plot_df$Y) - min(QQ_plot_df$Y)) / y_breaks_num, digits = 3)
y_breaks <- round(seq(from = floor(min(QQ_plot_df$Y)/y_binwidth)*y_binwidth, 
                      to = ceiling(max(QQ_plot_df$Y)/y_binwidth)*y_binwidth, 
                      by = y_binwidth), 3)
y_labs <- format(y_breaks, scientific = FALSE)

# Legende e colori
leg_shape_labs <- bquote("Q-Q plot")
leg_fill_labs <- c(bquote("90% confidence interval"), bquote("95% confidence interval"))
leg_col_labs <- c(bquote("Interquartile line"), bquote("Regression line"), bquote("y = x"))

Stand_Res_std_QQ_plot <- ggplot(QQ_plot_df) +
  stat_qq_band(aes(sample = X, fill = "95"), distribution = distr, dparams = distr_pars, conf = 0.95) +
  stat_qq_band(aes(sample = X, fill = "90"), distribution = distr, dparams = distr_pars, conf = 0.90) +
  geom_abline(aes(slope = slope, intercept = intercept, colour = "IQR"), linewidth = 0.8) +
  stat_smooth(aes(x = Q, y = Y, colour = "Reg", group = 1), method = "lm", se = FALSE, linewidth = 0.8) +
  geom_abline(aes(slope = 1, intercept = 0, colour = "45deg"), linewidth = 0.8) +
  stat_qq_point(aes(sample = X), shape = 19, color = "black", size = 1.2, alpha = 0.8, distribution = distr, dparams = distr_pars) +
  scale_x_continuous(name = x_name, breaks = x_breaks, labels = x_labs) +
  scale_y_continuous(name = y_name, breaks = y_breaks, labels = NULL,
                     sec.axis = sec_axis(~ ., breaks = y_breaks, labels = y_labs)) +
  ggtitle(title_content) +
  labs(subtitle = subtitle_content, caption = caption_content) +
  scale_shape_manual(name = "", labels = leg_shape_labs, values = c("19")) +
  scale_fill_manual(name = "", values = c("90" = "chartreuse1", "95" = "deepskyblue1"), labels = leg_fill_labs) +
  scale_colour_manual(name = "", values = c("IQR" = "cyan", "Reg" = "red", "45deg" = "black"), labels = leg_col_labs) +
  guides(shape = guide_legend(order = 1), fill = guide_legend(order = 2), colour = guide_legend(order = 3)) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.x = element_text(angle = 0, vjust = 1),
    legend.key.width = unit(0.8, "cm"),
    legend.position = "bottom"
  )

# Mostra il plot
plot(Stand_Res_std_QQ_plot)

# Calcola densità teorica nei quantili (pdf)
dens <- dsstd(quants, mean=mean, sd=sd, nu=SSTD_nu, xi=SSTD_xi)

# Calcola errore standard dei quantili teorici (formula banda Q-Q)
# Errore standard: SE = (p * (1-p)) / (n * f(Q)^2)
p <- ppoints(n)
SE <- sqrt(p * (1-p) / (n * dens^2))

# Limiti di confidenza (95%)
alpha <- 0.05
z <- qnorm(1 - alpha/2)

lower_95 <- quants - z * SE
upper_95 <- quants + z * SE

# Verifica quanti punti osservati cadono dentro le bande
# Ricorda che QQ_plot_df$X = dati ordinati osservati

inside_95 <- (QQ_plot_df$X >= lower_95) & (QQ_plot_df$X <= upper_95)

percent_inside_95 <- mean(inside_95) * 100

cat(sprintf("Percentuale di punti dentro la banda di confidenza al 95%%: %.2f%%\n", percent_inside_95))
# Percentuale di punti dentro la banda di confidenza al 95%: 96.62%

# Alpha per il 90%
alpha_90 <- 0.10
z_90 <- qnorm(1 - alpha_90/2)  # quantile normale per il 90%

# Limiti di confidenza al 90%
lower_90 <- quants - z_90 * SE
upper_90 <- quants + z_90 * SE

# Verifica quanti punti osservati cadono dentro la banda 90%
inside_90 <- (QQ_plot_df$X >= lower_90) & (QQ_plot_df$X <= upper_90)

percent_inside_90 <- mean(inside_90) * 100

cat(sprintf("Percentuale di punti dentro la banda di confidenza al 90%%: %.2f%%\n", percent_inside_90))
# Percentuale di punti dentro la banda di confidenza al 90%: 89.45%

# Kolmogorov-Smirnov test (K-S test)
ks_result <- ks.test(y, 
                     y = function(q) psstd(q, mean=0, sd=1, nu=SSTD_nu, xi=SSTD_xi),
                     alternative = "two.sided")

print(ks_result)
# 
#         Asymptotic one-sample Kolmogorov-Smirnov test
# 
# data:  y
# D = 0.050082, p-value = 0.1854
# alternative hypothesis: two-sided

# Dall'analisi del Q-Q Plot e del test di K-S vediamo, quindi, che i residui standardizzati seguono la distribuzione skewed Student-t con un livello
# di significatività del 5% (in realtà il test di K-S risulta anche al 10%).

# Verifichiamo ora la stazionarietà e l'omoschedasticità (non condizionata).

# Grafico dei residui
df_residui <- data.frame(
  Date = index(z_hat),
  Residui = as.numeric(z_hat)
)

ggplot(df_residui, aes(x = Date, y = Residui)) +
  geom_line(color = "steelblue", linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  labs(
    title = "Grafico dei residui standardizzati",
    subtitle = "Modello GARCH(4,0) con distribuzione skewed Student-t per SPY",
    x = "Data",
    y = "Residui standardizzati"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    axis.title = element_text(size = 11)
  )

# KPSS Test
kpss_test_level(df_residui, col = "Residui", alpha = 0.01)
# 
# [Residui] KPSS Test - Stazionarietà attorno a una media costante
# Statistic: 0.122659
# Valore critico (circa 1%): 0.739000
# Interpretazione del p-value: p-value > 0.10
# 
# ** CONCLUSIONE: Statistic <= valore critico (1%) **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO stazionari.

# Quindi, i residui standardizzati sono stazionari.

# Per l'omoschedasticità non condizionata, per prima cosa, vediamo se c'è curtosi nei residui standardizzati.

# Calcola la curtosi
kurt_z_hat <- kurtosis(as.numeric(z_hat), na.rm = TRUE)

cat(sprintf("Curtosi dei residui standardizzati: %.4f\n", kurt_z_hat))
# Curtosi dei residui standardizzati: 5.3167

# La curtosi c'è. Quindi, applichiamo la forma "studentized" dei test di Breusch-Pagan e White per la stazionarietà in varianza.

# Breusch-Pagan Test:
# Crea un indice temporale (può essere anche un numero progressivo)
df_residui$Index <- seq_along(df_residui$Residui)

# Esegui il test BP
breusch_pagan_test(
  index = df_residui$Index,
  group = df_residui$Residui,
  col = "Residui standardizzati"
)
# [Residui standardizzati] Breusch-Pagan Test
# Statistic: 7.660452
# p-value: 0.00564444
# 
# ** Conclusione: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO eteroschedastici.

# White Test
white_test(
  index = df_residui$Index,
  group = df_residui$Residui,
  col = "Residui standardizzati"
)
# [Residui standardizzati] White Test
# Statistic: 10.034218
# p-value: 0.00662365
# 
# ** Conclusione: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO eteroschedastici.

# Dai risultati dei test possiamo rigettare l'ipotesi nulla di omoschedasticità incondizionata con un livello di significatività dell'1%. Quindi,
# c'è evidenza statistica di eteroschedasticità non condizionata.
# Perciò, possiamo concludere che anche questo modello è da scartare.

# Analizziamo il prossimo nella classifica: GARCH(4,0) norm
p <- valid_models$p[24]
q <- valid_models$q[24]
dist <- valid_models$dist[24]

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = dist
)

fit <- tryCatch(
  ugarchfit(spec = spec, data = returns, solver = "hybrid"),
  error = function(e) NULL
)

# Risultati
show(fit)
# 
# *---------------------------------*
# *          GARCH Model Fit        *
# *---------------------------------*
#   
# Conditional Variance Dynamics 	
# -----------------------------------
# GARCH Model	    : sGARCH(4,0)
# Mean Model	    : ARFIMA(0,0,0)
# Distribution	  : norm 
# 
# Optimal Parameters
# ------------------------------------
#         Estimate  Std. Error  t value Pr(>|t|)
# omega   0.464817    0.057360  8.10357 0.000000
# alpha1  0.116901    0.041824  2.79504 0.005189
# alpha2  0.027443    0.040002  0.68605 0.492679
# alpha3  0.094393    0.056570  1.66861 0.095195
# alpha4  0.257521    0.079895  3.22323 0.001268
# 
# Robust Standard Errors:
#         Estimate  Std. Error  t value Pr(>|t|)
# omega   0.464817    0.075070  6.19177 0.000000
# alpha1  0.116901    0.059200  1.97469 0.048304
# alpha2  0.027443    0.045055  0.60911 0.542454
# alpha3  0.094393    0.066336  1.42294 0.154753
# alpha4  0.257521    0.174519  1.47561 0.140050
# 
# LogLikelihood : -619.0913 
# 
# Information Criteria
# ------------------------------------
#   
# Akaike       2.6333
# Bayes        2.6772
# Shibata      2.6331
# Hannan-Quinn 2.6506
# 
# Weighted Ljung-Box Test on Standardized Residuals
# ------------------------------------
#                         statistic p-value
# Lag[1]                  3.893e-05  0.9950
# Lag[2*(p+q)+(p+q)-1][2] 1.094e-01  0.9131
# Lag[4*(p+q)+(p+q)-1][5] 9.182e-01  0.8779
# d.o.f=0
# H0 : No serial correlation
# 
# Weighted Ljung-Box Test on Standardized Squared Residuals
# ------------------------------------
#                          statistic p-value
# Lag[1]                       0.245  0.6206
# Lag[2*(p+q)+(p+q)-1][11]     3.528  0.8009
# Lag[4*(p+q)+(p+q)-1][19]     6.937  0.7895
# d.o.f=4
# 
# Weighted ARCH LM Tests
# ------------------------------------
#             Statistic Shape Scale P-Value
# ARCH Lag[5]  0.008708 0.500 2.000  0.9257
# ARCH Lag[7]  1.733539 1.473 1.746  0.5651
# ARCH Lag[9]  3.082388 2.402 1.619  0.5502
# 
# Nyblom stability test
# ------------------------------------
# Joint Statistic:  0.9284
# Individual Statistics:             
# omega  0.4574
# alpha1 0.2042
# alpha2 0.1296
# alpha3 0.3031
# alpha4 0.6312
# 
# Asymptotic Critical Values (10% 5% 1%)
# Joint Statistic:     	   1.28 1.47 1.88
# Individual Statistic:	   0.35 0.47 0.75
# 
# Sign Bias Test
# ------------------------------------
#                    t-value    prob sig
# Sign Bias           1.4513 0.14737    
# Negative Sign Bias  0.7215 0.47095    
# Positive Sign Bias  0.4443 0.65702    
# Joint Effect        7.8331 0.04959  **
#   
#   
# Adjusted Pearson Goodness-of-Fit Test:
# ------------------------------------
#   group statistic p-value(g-1)
# 1    20     46.68    0.0003974
# 2    30     58.03    0.0010804
# 3    40     67.43    0.0031446
# 4    50     75.37    0.0091256
# 
# 
# Elapsed time : 0.104552  

# Analizzando i p-value del test pesato di Ljung-Box sui residui standardizzati, osserviamo che questi sono tutti maggiori di α = 0.01
# (in realtà anche di α = 0.05 e α = 0.10). Quindi, non abbiamo evidenza statistica sufficiente per rifiutare l’ipotesi nulla di
# assenza di autocorrelazione.

# Applichiamo, ora, dei dest di normalità sui residui.

# Estrai residui standardizzati
z_hat <- residuals(fit, standardize = TRUE)

# Prendi le date del training_set:
# Rimuoviamo le righe con NA nella colonna usata per il modello
valid_dates <- training_set$Date[!is.na(training_set$SPY_LogReturn)]

# Converti in Date se non lo sono già
valid_dates <- as.Date(valid_dates)

# Sovrascrivi l’index di z_hat con le date corrette
index(z_hat) <- valid_dates

# Controlla che le lunghezze coincidano
if(length(z_hat) != length(valid_dates)) {
  stop("Errore: lunghezza residui e date non coincide!")
}

# Visualizza i primi valori
head(z_hat)
#                  [,1]
# 2023-08-01 -0.2715260
# 2023-08-02 -1.3272829
# 2023-08-03 -0.2719337
# 2023-08-04 -0.4294846
# 2023-08-07  1.0405865
# 2023-08-08 -0.4209531

# Test di SW
shapirowilk_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Shapiro-Wilk Test
# Statistic: 0.975079
# p-value  : 3.12226e-07
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Test di JB
jarque_bera_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Jarque-Bera Test
# Statistic: 70.981004
# p-value  : 3.33067e-16
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Come vediamo, i residui standardizzati del GARCH(4,0) con distribuzione normale non seguono una distribuzione normale.
# Perciò, possiamo concludere che anche questo modello è da scartare.

# Analizziamo il prossimo nella classifica: GARCH(1,3) norm
p <- valid_models$p[25]
q <- valid_models$q[25]
dist <- valid_models$dist[25]

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = dist
)

fit <- tryCatch(
  ugarchfit(spec = spec, data = returns, solver = "hybrid"),
  error = function(e) NULL
)

# Risultati
show(fit)
# 
# *---------------------------------*
# *          GARCH Model Fit        *
# *---------------------------------*
#   
# Conditional Variance Dynamics 	
# -----------------------------------
# GARCH Model	    : sGARCH(1,3)
# Mean Model	    : ARFIMA(0,0,0)
# Distribution	  : norm 
# 
# Optimal Parameters
# ------------------------------------
#         Estimate  Std. Error   t value Pr(>|t|)
# omega   0.076104    0.019162  3.971696 0.000071
# alpha1  0.168190    0.014631 11.495616 0.000000
# beta1   0.341162    0.187784  1.816782 0.069251
# beta2   0.411605    0.148454  2.772611 0.005561
# beta3   0.000013    0.142920  0.000091 0.999928
# 
# Robust Standard Errors:
#         Estimate  Std. Error  t value Pr(>|t|)
# omega   0.076104    0.041456 1.835756 0.066394
# alpha1  0.168190    0.058685 2.865954 0.004158
# beta1   0.341162    0.162110 2.104506 0.035334
# beta2   0.411605    0.086029 4.784515 0.000002
# beta3   0.000013    0.100368 0.000129 0.999897
# 
# LogLikelihood : -620.9431 
# 
# Information Criteria
# ------------------------------------
#   
# Akaike       2.6411
# Bayes        2.6850
# Shibata      2.6409
# Hannan-Quinn 2.6584
# 
# Weighted Ljung-Box Test on Standardized Residuals
# ------------------------------------
#                         statistic p-value
# Lag[1]                  7.989e-05  0.9929
# Lag[2*(p+q)+(p+q)-1][2] 2.678e-02  0.9749
# Lag[4*(p+q)+(p+q)-1][5] 4.976e-01  0.9580
# d.o.f=0
# H0 : No serial correlation
# 
# Weighted Ljung-Box Test on Standardized Squared Residuals
# ------------------------------------
#                          statistic   p-value
# Lag[1]                      0.1077 0.7427330
# Lag[2*(p+q)+(p+q)-1][11]   22.5623 0.0001283
# Lag[4*(p+q)+(p+q)-1][19]   27.3903 0.0003610
# d.o.f=4
# 
# Weighted ARCH LM Tests
# ------------------------------------
#             Statistic Shape Scale P-Value
# ARCH Lag[5]  0.005803 0.500 2.000  0.9393
# ARCH Lag[7]  0.239258 1.473 1.746  0.9621
# ARCH Lag[9]  1.352000 2.402 1.619  0.8775
# 
# Nyblom stability test
# ------------------------------------
# Joint Statistic:  0.5368
# Individual Statistics:             
# omega  0.2661
# alpha1 0.3510
# beta1  0.2672
# beta2  0.2575
# beta3  0.2407
# 
# Asymptotic Critical Values (10% 5% 1%)
# Joint Statistic:     	   1.28 1.47 1.88
# Individual Statistic:	   0.35 0.47 0.75
# 
# Sign Bias Test
# ------------------------------------
#                    t-value   prob sig
# Sign Bias          1.40319 0.1612    
# Negative Sign Bias 0.07183 0.9428    
# Positive Sign Bias 0.42542 0.6707    
# Joint Effect       4.81271 0.1860    
# 
# 
# Adjusted Pearson Goodness-of-Fit Test:
# ------------------------------------
#   group statistic p-value(g-1)
# 1    20     41.86     0.001848
# 2    30     55.24     0.002322
# 3    40     59.84     0.017505
# 4    50     69.88     0.026631
# 
# 
# Elapsed time : 0.212019

# Analizzando i p-value del test pesato di Ljung-Box sui residui standardizzati, osserviamo che questi sono tutti maggiori di α = 0.01
# (in realtà anche di α = 0.05 e α = 0.10). Quindi, non abbiamo evidenza statistica sufficiente per rifiutare l’ipotesi nulla di
# assenza di autocorrelazione.

# Applichiamo, ora, dei dest di normalità sui residui.

# Estrai residui standardizzati
z_hat <- residuals(fit, standardize = TRUE)

# Prendi le date del training_set:
# Rimuoviamo le righe con NA nella colonna usata per il modello
valid_dates <- training_set$Date[!is.na(training_set$SPY_LogReturn)]

# Converti in Date se non lo sono già
valid_dates <- as.Date(valid_dates)

# Sovrascrivi l’index di z_hat con le date corrette
index(z_hat) <- valid_dates

# Controlla che le lunghezze coincidano
if(length(z_hat) != length(valid_dates)) {
  stop("Errore: lunghezza residui e date non coincide!")
}

# Visualizza i primi valori
head(z_hat)
#                  [,1]
# 2023-08-01 -0.2715260
# 2023-08-02 -1.3272829
# 2023-08-03 -0.2719337
# 2023-08-04 -0.4704188
# 2023-08-07  0.9233011
# 2023-08-08 -0.4626001

# Test di SW
shapirowilk_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Shapiro-Wilk Test
# Statistic: 0.964871
# p-value  : 3.18359e-09
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Test di JB
jarque_bera_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Jarque-Bera Test
# Statistic: 186.695828
# p-value  : 0
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Come vediamo, i residui standardizzati del GARCH(1,3) con distribuzione normale non seguono una distribuzione normale.
# Perciò, possiamo concludere che anche questo modello è da scartare.

# Analizziamo il prossimo nella classifica: GARCH(2,2) norm
p <- valid_models$p[26]
q <- valid_models$q[26]
dist <- valid_models$dist[26]

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = dist
)

fit <- tryCatch(
  ugarchfit(spec = spec, data = returns, solver = "hybrid"),
  error = function(e) NULL
)

# Risultati
show(fit)
# 
# *---------------------------------*
# *          GARCH Model Fit        *
# *---------------------------------*
#   
# Conditional Variance Dynamics 	
# -----------------------------------
# GARCH Model	    : sGARCH(2,2)
# Mean Model	    : ARFIMA(0,0,0)
# Distribution	  : norm 
# 
# Optimal Parameters
# ------------------------------------
#         Estimate  Std. Error  t value Pr(>|t|)
# omega   0.058823    0.019190 3.065247 0.002175
# alpha1  0.105141    0.024628 4.269221 0.000020
# alpha2  0.028066    0.051796 0.541846 0.587924
# beta1   0.806483    0.333119 2.421008 0.015478
# beta2   0.000002    0.296415 0.000006 0.999995
# 
# Robust Standard Errors:
#         Estimate  Std. Error  t value Pr(>|t|)
# omega   0.058823    0.041261 1.425629 0.153976
# alpha1  0.105141    0.052837 1.989910 0.046601
# alpha2  0.028066    0.037900 0.740522 0.458984
# beta1   0.806483    0.363934 2.216014 0.026691
# beta2   0.000002    0.280491 0.000006 0.999995
# 
# LogLikelihood : -621.3596 
# 
# Information Criteria
# ------------------------------------
#   
# Akaike       2.6429
# Bayes        2.6868
# Shibata      2.6426
# Hannan-Quinn 2.6601
# 
# Weighted Ljung-Box Test on Standardized Residuals
# ------------------------------------
#                         statistic p-value
# Lag[1]                    0.00706  0.9330
# Lag[2*(p+q)+(p+q)-1][2]   0.04033  0.9639
# Lag[4*(p+q)+(p+q)-1][5]   0.55841  0.9483
# d.o.f=0
# H0 : No serial correlation
# 
# Weighted Ljung-Box Test on Standardized Squared Residuals
# ------------------------------------
#                          statistic   p-value
# Lag[1]                       1.542 0.2142646
# Lag[2*(p+q)+(p+q)-1][11]    21.005 0.0003145
# Lag[4*(p+q)+(p+q)-1][19]    25.242 0.0010713
# d.o.f=4
# 
# Weighted ARCH LM Tests
# ------------------------------------
#             Statistic Shape Scale P-Value
# ARCH Lag[5]   0.02072 0.500 2.000  0.8855
# ARCH Lag[7]   0.35920 1.473 1.746  0.9338
# ARCH Lag[9]   1.27598 2.402 1.619  0.8900
# 
# Nyblom stability test
# ------------------------------------
# Joint Statistic:  2.0154
# Individual Statistics:             
# omega  0.2761
# alpha1 0.4043
# alpha2 0.3218
# beta1  0.2898
# beta2  0.2739
# 
# Asymptotic Critical Values (10% 5% 1%)
# Joint Statistic:     	   1.28 1.47 1.88
# Individual Statistic:	   0.35 0.47 0.75
# 
# Sign Bias Test
# ------------------------------------
#                    t-value   prob sig
# Sign Bias           1.2303 0.2192    
# Negative Sign Bias  0.7596 0.4479    
# Positive Sign Bias  0.2050 0.8377    
# Joint Effect        5.7579 0.1240    
# 
# 
# Adjusted Pearson Goodness-of-Fit Test:
# ------------------------------------
#   group statistic p-value(g-1)
# 1    20     48.78    0.0001974
# 2    30     58.53    0.0009373
# 3    40     68.45    0.0024612
# 4    50     76.63    0.0070186
# 
# 
# Elapsed time : 0.1322191 

# Analizzando i p-value del test pesato di Ljung-Box sui residui standardizzati, osserviamo che questi sono tutti maggiori di α = 0.01
# (in realtà anche di α = 0.05 e α = 0.10). Quindi, non abbiamo evidenza statistica sufficiente per rifiutare l’ipotesi nulla di
# assenza di autocorrelazione.

# Applichiamo, ora, dei dest di normalità sui residui.

# Estrai residui standardizzati
z_hat <- residuals(fit, standardize = TRUE)

# Prendi le date del training_set:
# Rimuoviamo le righe con NA nella colonna usata per il modello
valid_dates <- training_set$Date[!is.na(training_set$SPY_LogReturn)]

# Converti in Date se non lo sono già
valid_dates <- as.Date(valid_dates)

# Sovrascrivi l’index di z_hat con le date corrette
index(z_hat) <- valid_dates

# Controlla che le lunghezze coincidano
if(length(z_hat) != length(valid_dates)) {
  stop("Errore: lunghezza residui e date non coincide!")
}

# Visualizza i primi valori
head(z_hat)
#                  [,1]
# 2023-08-01 -0.2715260
# 2023-08-02 -1.3272829
# 2023-08-03 -0.2658126
# 2023-08-04 -0.4396984
# 2023-08-07  0.8964310
# 2023-08-08 -0.4588917

# Test di SW
shapirowilk_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Shapiro-Wilk Test
# Statistic: 0.964704
# p-value  : 2.97484e-09
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Test di JB
jarque_bera_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Jarque-Bera Test
# Statistic: 180.253732
# p-value  : 0
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Come vediamo, i residui standardizzati del GARCH(2,2) con distribuzione normale non seguono una distribuzione normale.
# Perciò, possiamo concludere che anche questo modello è da scartare.

# Analizziamo il prossimo nella classifica: GARCH(4,1) norm
p <- valid_models$p[27]
q <- valid_models$q[27]
dist <- valid_models$dist[27]

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = dist
)

fit <- tryCatch(
  ugarchfit(spec = spec, data = returns, solver = "hybrid"),
  error = function(e) NULL
)

# Risultati
show(fit)
# 
# *---------------------------------*
# *          GARCH Model Fit        *
# *---------------------------------*
#   
# Conditional Variance Dynamics 	
# -----------------------------------
# GARCH Model	    : sGARCH(4,1)
# Mean Model	    : ARFIMA(0,0,0)
# Distribution	  : norm 
# 
# Optimal Parameters
# ------------------------------------
#         Estimate  Std. Error  t value Pr(>|t|)
# omega   0.138343    0.050295   2.7506 0.005948
# alpha1  0.067057    0.031745   2.1124 0.034651
# alpha2  0.000000    0.061259   0.0000 1.000000
# alpha3  0.046765    0.059309   0.7885 0.430403
# alpha4  0.235186    0.083780   2.8072 0.004997
# beta1   0.523752    0.094815   5.5239 0.000000
# 
# Robust Standard Errors:
#         Estimate  Std. Error  t value Pr(>|t|)
# omega   0.138343    0.053455  2.58802 0.009653
# alpha1  0.067057    0.045810  1.46380 0.143248
# alpha2  0.000000    0.077016  0.00000 1.000000
# alpha3  0.046765    0.073284  0.63814 0.523385
# alpha4  0.235186    0.159332  1.47607 0.139924
# beta1   0.523752    0.104904  4.99266 0.000001
# 
# LogLikelihood : -613.2651 
# 
# Information Criteria
# ------------------------------------
#   
# Akaike       2.6129
# Bayes        2.6656
# Shibata      2.6126
# Hannan-Quinn 2.6336
# 
# Weighted Ljung-Box Test on Standardized Residuals
# ------------------------------------
#                         statistic p-value
# Lag[1]                    0.01738  0.8951
# Lag[2*(p+q)+(p+q)-1][2]   0.06267  0.9467
# Lag[4*(p+q)+(p+q)-1][5]   0.56481  0.9472
# d.o.f=0
# H0 : No serial correlation
# 
# Weighted Ljung-Box Test on Standardized Squared Residuals
# ------------------------------------
#                          statistic p-value
# Lag[1]                      0.9831  0.3214
# Lag[2*(p+q)+(p+q)-1][14]    5.1995  0.7444
# Lag[4*(p+q)+(p+q)-1][24]    7.8347  0.8848
# d.o.f=5
# 
# Weighted ARCH LM Tests
# ------------------------------------
#              Statistic Shape Scale P-Value
# ARCH Lag[6]     0.0499 0.500 2.000  0.8232
# ARCH Lag[8]     0.4450 1.480 1.774  0.9150
# ARCH Lag[10]    1.5438 2.424 1.650  0.8535
# 
# Nyblom stability test
# ------------------------------------
# Joint Statistic:  1.3296
# Individual Statistics:             
# omega  0.1854
# alpha1 0.1588
# alpha2 0.4984
# alpha3 0.2928
# alpha4 0.4268
# beta1  0.1847
# 
# Asymptotic Critical Values (10% 5% 1%)
# Joint Statistic:     	   1.49 1.68 2.12
# Individual Statistic:	   0.35 0.47 0.75
# 
# Sign Bias Test
# ------------------------------------
#                    t-value    prob sig
# Sign Bias            1.656 0.09837   *
# Negative Sign Bias   1.096 0.27347    
# Positive Sign Bias   0.245 0.80660    
# Joint Effect        10.777 0.01300  **
#   
#   
# Adjusted Pearson Goodness-of-Fit Test:
# ------------------------------------
#   group statistic p-value(g-1)
# 1    20     37.65     0.006582
# 2    30     50.94     0.007145
# 3    40     57.81     0.026599
# 4    50     63.13     0.084501
# 
# 
# Elapsed time : 0.13977 

# Analizzando i p-value del test pesato di Ljung-Box sui residui standardizzati, osserviamo che questi sono tutti maggiori di α = 0.01
# (in realtà anche di α = 0.05 e α = 0.10). Quindi, non abbiamo evidenza statistica sufficiente per rifiutare l’ipotesi nulla di
# assenza di autocorrelazione.

# Applichiamo, ora, dei dest di normalità sui residui.

# Estrai residui standardizzati
z_hat <- residuals(fit, standardize = TRUE)

# Prendi le date del training_set:
# Rimuoviamo le righe con NA nella colonna usata per il modello
valid_dates <- training_set$Date[!is.na(training_set$SPY_LogReturn)]

# Converti in Date se non lo sono già
valid_dates <- as.Date(valid_dates)

# Sovrascrivi l’index di z_hat con le date corrette
index(z_hat) <- valid_dates

# Controlla che le lunghezze coincidano
if(length(z_hat) != length(valid_dates)) {
  stop("Errore: lunghezza residui e date non coincide!")
}

# Visualizza i primi valori
head(z_hat)
#                  [,1]
# 2023-08-01 -0.2715260
# 2023-08-02 -1.3272829
# 2023-08-03 -0.2719337
# 2023-08-04 -0.4294846
# 2023-08-07  0.9444952
# 2023-08-08 -0.4159561

# Test di SW
shapirowilk_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Shapiro-Wilk Test
# Statistic: 0.973593
# p-value  : 1.50847e-07
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Test di JB
jarque_bera_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Jarque-Bera Test
# Statistic: 89.194397
# p-value  : 0
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Come vediamo, i residui standardizzati del GARCH(4,1) con distribuzione normale non seguono una distribuzione normale.
# Perciò, possiamo concludere che anche questo modello è da scartare.

# Analizziamo il prossimo nella classifica: GARCH(0,3) sstd
p <- valid_models$p[28]
q <- valid_models$q[28]
dist <- valid_models$dist[28]

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = dist
)

fit <- tryCatch(
  ugarchfit(spec = spec, data = returns, solver = "hybrid"),
  error = function(e) NULL
)

# Risultati
show(fit)
# 
# *---------------------------------*
# *          GARCH Model Fit        *
# *---------------------------------*
#   
# Conditional Variance Dynamics 	
# -----------------------------------
# GARCH Model	    : sGARCH(0,3)
# Mean Model	    : ARFIMA(0,0,0)
# Distribution	  : sstd 
# 
# Optimal Parameters
# ------------------------------------
#        Estimate  Std. Error    t value Pr(>|t|)
# omega  0.005373    0.006281   0.855564  0.39224
# beta1  0.000002    0.091072   0.000020  0.99998
# beta2  0.000004    0.090800   0.000039  0.99997
# beta3  0.998995    0.001066 937.179012  0.00000
# skew   0.843510    0.045237  18.646342  0.00000
# shape  2.861092    0.250928  11.402049  0.00000
# 
# Robust Standard Errors:
#        Estimate  Std. Error    t value Pr(>|t|)
# omega  0.005373    0.004336   1.239257  0.21525
# beta1  0.000002    0.009735   0.000183  0.99985
# beta2  0.000004    0.009718   0.000363  0.99971
# beta3  0.998995    0.002158 462.905278  0.00000
# skew   0.843510    0.041121  20.512980  0.00000
# shape  2.861092    0.345380   8.283888  0.00000
# 
# LogLikelihood : -613.6055 
# 
# Information Criteria
# ------------------------------------
#   
# Akaike       2.6144
# Bayes        2.6670
# Shibata      2.6141
# Hannan-Quinn 2.6351
# 
# Weighted Ljung-Box Test on Standardized Residuals
# ------------------------------------
#                         statistic p-value
# Lag[1]                      1.443 0.22972
# Lag[2*(p+q)+(p+q)-1][2]     2.794 0.15959
# Lag[4*(p+q)+(p+q)-1][5]     7.196 0.04648
# d.o.f=0
# H0 : No serial correlation
# 
# Weighted Ljung-Box Test on Standardized Squared Residuals
# ------------------------------------
#                          statistic   p-value
# Lag[1]                       24.35 8.017e-07
# Lag[2*(p+q)+(p+q)-1][8]      72.18 0.000e+00
# Lag[4*(p+q)+(p+q)-1][14]     83.28 0.000e+00
# d.o.f=3
# 
# Weighted ARCH LM Tests
# ------------------------------------
#             Statistic Shape Scale   P-Value
# ARCH Lag[4]     28.70 0.500 2.000 8.432e-08
# ARCH Lag[6]     31.02 1.461 1.711 5.905e-08
# ARCH Lag[8]     33.23 2.368 1.583 4.321e-08
# 
# Nyblom stability test
# ------------------------------------
# Joint Statistic:  5.2148
# Individual Statistics:             
# omega 0.47016
# beta1 0.42990
# beta2 0.42984
# beta3 0.42975
# skew  0.03991
# shape 0.16318
# 
# Asymptotic Critical Values (10% 5% 1%)
# Joint Statistic:     	   1.49 1.68 2.12
# Individual Statistic:	   0.35 0.47 0.75
# 
# Sign Bias Test
# ------------------------------------
#                    t-value      prob sig
# Sign Bias           0.5126 6.085e-01    
# Negative Sign Bias  4.0862 5.158e-05 ***
# Positive Sign Bias  2.8573 4.463e-03 ***
# Joint Effect       28.3960 2.999e-06 ***
#   
#   
# Adjusted Pearson Goodness-of-Fit Test:
# ------------------------------------
#   group statistic p-value(g-1)
# 1    20     30.30      0.04806
# 2    30     49.42      0.01043
# 3    40     57.14      0.03046
# 4    50     66.08      0.05221
# 
# 
# Elapsed time : 0.2086411  

# Analizzando i p-value del test pesato di Ljung-Box sui residui standardizzati, osserviamo che questi sono tutti maggiori di α = 0.01.
# Quindi, non abbiamo evidenza statistica sufficiente per rifiutare l’ipotesi nulla di assenza di autocorrelazione.

# Applichiamo, ora, dei dest di normalità sui residui.

# Estrai residui standardizzati
z_hat <- residuals(fit, standardize = TRUE)

# Prendi le date del training_set:
# Rimuoviamo le righe con NA nella colonna usata per il modello
valid_dates <- training_set$Date[!is.na(training_set$SPY_LogReturn)]

# Converti in Date se non lo sono già
valid_dates <- as.Date(valid_dates)

# Sovrascrivi l’index di z_hat con le date corrette
index(z_hat) <- valid_dates

# Controlla che le lunghezze coincidano
if(length(z_hat) != length(valid_dates)) {
  stop("Errore: lunghezza residui e date non coincide!")
}

# Visualizza i primi valori
head(z_hat)
#                  [,1]
# 2023-08-01 -0.2715260
# 2023-08-02 -1.3272829
# 2023-08-03 -0.2719337
# 2023-08-04 -0.4286658
# 2023-08-07  0.8218447
# 2023-08-08 -0.4121456

# Test di SW
shapirowilk_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Shapiro-Wilk Test
# Statistic: 0.874277
# p-value  : 3.84384e-19
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Test di JB
jarque_bera_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Jarque-Bera Test
# Statistic: 5128.307501
# p-value  : 0
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Come atteso, i residui standardizzati del GARCH(0,3) con distribuzione skewed Student-t non seguono una distribuzione normale.

# QQ-Plot SSTD:
# Residui standardizzati ordinati
y <- sort(as.numeric(z_hat))
n <- length(y)

# Parametri stimati da 'fit'
params <- coef(fit)
SSTD_xi <- params["skew"]
SSTD_nu <- params["shape"]
mean <- 0
sd <- 1

# Parametri della distribuzione skewed Student-t
distr <- "sstd"
distr_pars <- list(mean = mean, sd = sd, nu = SSTD_nu, xi = SSTD_xi)

# Quantili teorici
quants <- qsstd(ppoints(n), mean = mean, sd = sd, nu = SSTD_nu, xi = SSTD_xi)

# DataFrame per plotting
QQ_plot_df <- data.frame(
  T = 1:n,
  Q = quants,
  X = y,
  Y = y
)

# Calcolo retta interquartile
quart_probs <- c(0.25, 0.75)
quart_X <- as.vector(quantile(QQ_plot_df$X, quart_probs))
quart_Q <- qsstd(quart_probs, mean = mean, sd = sd, nu = SSTD_nu, xi = SSTD_xi)
slope <- diff(quart_X) / diff(quart_Q)
intercept <- quart_X[1] - slope * quart_Q[1]

# Titoli
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 MPSMF 2024-2025",
                             paste("Q-Q plot of the Standardized Residuals from the GARCH(0,3) model for SPY (skewed Student-t)")))
subtitle_content <- bquote(paste("Sample size: ", .(n), 
                                 "; SSTD parameters → mean = ", .(mean), 
                                 ", sd = ", .(sd), 
                                 ", df = ", .(round(SSTD_nu, 3)), 
                                 ", skew = ", .(round(SSTD_xi, 3)), "."))

caption_content <- "Author: Matteo Basili"
x_name <- bquote("Theoretical Quantiles (SSTD)")
y_name <- bquote("Standardized Residuals")

# Assi e legende
x_breaks <- seq(from = floor(min(QQ_plot_df$Q)), to = ceiling(max(QQ_plot_df$Q)), by = 0.5)
x_labs <- format(x_breaks, scientific = FALSE)
y_breaks_num <- length(x_breaks)
y_binwidth <- round((max(QQ_plot_df$Y) - min(QQ_plot_df$Y)) / y_breaks_num, digits = 3)
y_breaks <- round(seq(from = floor(min(QQ_plot_df$Y)/y_binwidth)*y_binwidth, 
                      to = ceiling(max(QQ_plot_df$Y)/y_binwidth)*y_binwidth, 
                      by = y_binwidth), 3)
y_labs <- format(y_breaks, scientific = FALSE)

# Legende e colori
leg_shape_labs <- bquote("Q-Q plot")
leg_fill_labs <- c(bquote("90% confidence interval"), bquote("95% confidence interval"))
leg_col_labs <- c(bquote("Interquartile line"), bquote("Regression line"), bquote("y = x"))

Stand_Res_std_QQ_plot <- ggplot(QQ_plot_df) +
  stat_qq_band(aes(sample = X, fill = "95"), distribution = distr, dparams = distr_pars, conf = 0.95) +
  stat_qq_band(aes(sample = X, fill = "90"), distribution = distr, dparams = distr_pars, conf = 0.90) +
  geom_abline(aes(slope = slope, intercept = intercept, colour = "IQR"), linewidth = 0.8) +
  stat_smooth(aes(x = Q, y = Y, colour = "Reg", group = 1), method = "lm", se = FALSE, linewidth = 0.8) +
  geom_abline(aes(slope = 1, intercept = 0, colour = "45deg"), linewidth = 0.8) +
  stat_qq_point(aes(sample = X), shape = 19, color = "black", size = 1.2, alpha = 0.8, distribution = distr, dparams = distr_pars) +
  scale_x_continuous(name = x_name, breaks = x_breaks, labels = x_labs) +
  scale_y_continuous(name = y_name, breaks = y_breaks, labels = NULL,
                     sec.axis = sec_axis(~ ., breaks = y_breaks, labels = y_labs)) +
  ggtitle(title_content) +
  labs(subtitle = subtitle_content, caption = caption_content) +
  scale_shape_manual(name = "", labels = leg_shape_labs, values = c("19")) +
  scale_fill_manual(name = "", values = c("90" = "chartreuse1", "95" = "deepskyblue1"), labels = leg_fill_labs) +
  scale_colour_manual(name = "", values = c("IQR" = "cyan", "Reg" = "red", "45deg" = "black"), labels = leg_col_labs) +
  guides(shape = guide_legend(order = 1), fill = guide_legend(order = 2), colour = guide_legend(order = 3)) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.x = element_text(angle = 0, vjust = 1),
    legend.key.width = unit(0.8, "cm"),
    legend.position = "bottom"
  )

# Mostra il plot
plot(Stand_Res_std_QQ_plot)

# Calcola densità teorica nei quantili (pdf)
dens <- dsstd(quants, mean=mean, sd=sd, nu=SSTD_nu, xi=SSTD_xi)

# Calcola errore standard dei quantili teorici (formula banda Q-Q)
# Errore standard: SE = (p * (1-p)) / (n * f(Q)^2)
p <- ppoints(n)
SE <- sqrt(p * (1-p) / (n * dens^2))

# Limiti di confidenza (95%)
alpha <- 0.05
z <- qnorm(1 - alpha/2)

lower_95 <- quants - z * SE
upper_95 <- quants + z * SE

# Verifica quanti punti osservati cadono dentro le bande
# Ricorda che QQ_plot_df$X = dati ordinati osservati

inside_95 <- (QQ_plot_df$X >= lower_95) & (QQ_plot_df$X <= upper_95)

percent_inside_95 <- mean(inside_95) * 100

cat(sprintf("Percentuale di punti dentro la banda di confidenza al 95%%: %.2f%%\n", percent_inside_95))
# Percentuale di punti dentro la banda di confidenza al 95%: 100.00%

# Alpha per il 90%
alpha_90 <- 0.10
z_90 <- qnorm(1 - alpha_90/2)  # quantile normale per il 90%

# Limiti di confidenza al 90%
lower_90 <- quants - z_90 * SE
upper_90 <- quants + z_90 * SE

# Verifica quanti punti osservati cadono dentro la banda 90%
inside_90 <- (QQ_plot_df$X >= lower_90) & (QQ_plot_df$X <= upper_90)

percent_inside_90 <- mean(inside_90) * 100

cat(sprintf("Percentuale di punti dentro la banda di confidenza al 90%%: %.2f%%\n", percent_inside_90))
# Percentuale di punti dentro la banda di confidenza al 90%: 96.41%

# Kolmogorov-Smirnov test (K-S test)
ks_result <- ks.test(y, 
                     y = function(q) psstd(q, mean=0, sd=1, nu=SSTD_nu, xi=SSTD_xi),
                     alternative = "two.sided")

print(ks_result)
# 
#         Asymptotic one-sample Kolmogorov-Smirnov test
# 
# data:  y
# D = 0.040877, p-value = 0.4068
# alternative hypothesis: two-sided

# Dall'analisi del Q-Q Plot e del test di K-S vediamo, quindi, che i residui standardizzati seguono la distribuzione skewed Student-t con un livello
# di significatività del 10%.

# Verifichiamo ora la stazionarietà e l'omoschedasticità (non condizionata).

# Grafico dei residui
df_residui <- data.frame(
  Date = index(z_hat),
  Residui = as.numeric(z_hat)
)

ggplot(df_residui, aes(x = Date, y = Residui)) +
  geom_line(color = "steelblue", linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  labs(
    title = "Grafico dei residui standardizzati",
    subtitle = "Modello GARCH(0,3) con distribuzione skewed Student-t per SPY",
    x = "Data",
    y = "Residui standardizzati"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    axis.title = element_text(size = 11)
  )

# KPSS Test
kpss_test_level(df_residui, col = "Residui", alpha = 0.01)
# 
# [Residui] KPSS Test - Stazionarietà attorno a una media costante
# Statistic: 0.075029
# Valore critico (circa 1%): 0.739000
# Interpretazione del p-value: p-value > 0.10
# 
# ** CONCLUSIONE: Statistic <= valore critico (1%) **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO stazionari.

# Quindi, i residui standardizzati sono stazionari.

# Per l'omoschedasticità non condizionata, per prima cosa, vediamo se c'è curtosi nei residui standardizzati.

# Calcola la curtosi
kurt_z_hat <- kurtosis(as.numeric(z_hat), na.rm = TRUE)

cat(sprintf("Curtosi dei residui standardizzati: %.4f\n", kurt_z_hat))
# Curtosi dei residui standardizzati: 19.0578

# La curtosi c'è. Quindi, applichiamo la forma "studentized" dei test di Breusch-Pagan e White per la stazionarietà in varianza.

# Breusch-Pagan Test:
# Crea un indice temporale (può essere anche un numero progressivo)
df_residui$Index <- seq_along(df_residui$Residui)

# Esegui il test BP
breusch_pagan_test(
  index = df_residui$Index,
  group = df_residui$Residui,
  col = "Residui standardizzati"
)
# [Residui standardizzati] Breusch-Pagan Test
# Statistic: 6.799929
# p-value: 0.00911615
# 
# ** Conclusione: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO eteroschedastici.

# White Test
white_test(
  index = df_residui$Index,
  group = df_residui$Residui,
  col = "Residui standardizzati"
)
# [Residui standardizzati] White Test
# Statistic: 10.574871
# p-value: 0.00505471
# 
# ** Conclusione: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO eteroschedastici.

# Dai risultati dei test possiamo rigettare l'ipotesi nulla di omoschedasticità incondizionata con un livello di significatività dell'1%. Quindi,
# c'è evidenza statistica di eteroschedasticità non condizionata.
# Perciò, possiamo concludere che anche questo modello è da scartare.

# Analizziamo il prossimo nella classifica: GARCH(4,1) sstd
p <- valid_models$p[29]
q <- valid_models$q[29]
dist <- valid_models$dist[29]

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = dist
)

fit <- tryCatch(
  ugarchfit(spec = spec, data = returns, solver = "hybrid"),
  error = function(e) NULL
)

# Risultati
show(fit)
# 
# *---------------------------------*
# *          GARCH Model Fit        *
# *---------------------------------*
#   
# Conditional Variance Dynamics 	
# -----------------------------------
# GARCH Model	    : sGARCH(4,1)
# Mean Model	    : ARFIMA(0,0,0)
# Distribution	  : sstd 
# 
# Optimal Parameters
# ------------------------------------
#         Estimate  Std. Error  t value Pr(>|t|)
# omega   0.129589    0.050075  2.58791 0.009656
# alpha1  0.026696    0.045241  0.59009 0.555130
# alpha2  0.000000    0.052467  0.00000 1.000000
# alpha3  0.110484    0.084168  1.31266 0.189298
# alpha4  0.101313    0.108422  0.93443 0.350080
# beta1   0.642057    0.090730  7.07659 0.000000
# skew    0.814677    0.051113 15.93887 0.000000
# shape   5.293810    1.397893  3.78699 0.000152
# 
# Robust Standard Errors:
#         Estimate  Std. Error  t value Pr(>|t|)
# omega   0.129589    0.033042  3.92190 0.000088
# alpha1  0.026696    0.043472  0.61410 0.539151
# alpha2  0.000000    0.058132  0.00000 1.000000
# alpha3  0.110484    0.089129  1.23960 0.215124
# alpha4  0.101313    0.116436  0.87012 0.384236
# beta1   0.642057    0.066745  9.61954 0.000000
# skew    0.814677    0.045583 17.87225 0.000000
# shape   5.293810    1.637353  3.23315 0.001224
# 
# LogLikelihood : -594.2977 
# 
# Information Criteria
# ------------------------------------
#   
# Akaike       2.5413
# Bayes        2.6116
# Shibata      2.5408
# Hannan-Quinn 2.5690
# 
# Weighted Ljung-Box Test on Standardized Residuals
# ------------------------------------
#                         statistic p-value
# Lag[1]                     0.1859  0.6663
# Lag[2*(p+q)+(p+q)-1][2]    0.2531  0.8224
# Lag[4*(p+q)+(p+q)-1][5]    0.7888  0.9053
# d.o.f=0
# H0 : No serial correlation
# 
# Weighted Ljung-Box Test on Standardized Squared Residuals
# ------------------------------------
#                          statistic  p-value
# Lag[1]                       10.22 0.001393
# Lag[2*(p+q)+(p+q)-1][14]     16.95 0.009893
# Lag[4*(p+q)+(p+q)-1][24]     19.41 0.061109
# d.o.f=5
# 
# Weighted ARCH LM Tests
# ------------------------------------
#              Statistic Shape Scale P-Value
# ARCH Lag[6]    0.07183 0.500 2.000  0.7887
# ARCH Lag[8]    0.28983 1.480 1.774  0.9526
# ARCH Lag[10]   0.92201 2.424 1.650  0.9459
# 
# Nyblom stability test
# ------------------------------------
# Joint Statistic:  1.4275
# Individual Statistics:              
# omega  0.04228
# alpha1 0.03696
# alpha2 0.05502
# alpha3 0.05942
# alpha4 0.06385
# beta1  0.03178
# skew   0.09812
# shape  0.30789
# 
# Asymptotic Critical Values (10% 5% 1%)
# Joint Statistic:     	   1.89 2.11 2.59
# Individual Statistic:	   0.35 0.47 0.75
# 
# Sign Bias Test
# ------------------------------------
#                    t-value      prob sig
# Sign Bias           0.7417 0.4586519    
# Negative Sign Bias  2.8798 0.0041620 ***
# Positive Sign Bias  0.1804 0.8569001    
# Joint Effect       16.4257 0.0009274 ***
#   
#   
# Adjusted Pearson Goodness-of-Fit Test:
# ------------------------------------
#   group statistic p-value(g-1)
# 1    20     24.23      0.18756
# 2    30     34.10      0.23557
# 3    40     50.89      0.09614
# 4    50     59.33      0.14810
# 
# 
# Elapsed time : 0.2214761 

# Analizzando i p-value del test pesato di Ljung-Box sui residui standardizzati, osserviamo che questi sono tutti maggiori di α = 0.01 (in realtà
# anche di α = 0.05 e α = 0.10). Quindi, non abbiamo evidenza statistica sufficiente per rifiutare l’ipotesi nulla di assenza di autocorrelazione.

# Applichiamo, ora, dei dest di normalità sui residui.

# Estrai residui standardizzati
z_hat <- residuals(fit, standardize = TRUE)

# Prendi le date del training_set:
# Rimuoviamo le righe con NA nella colonna usata per il modello
valid_dates <- training_set$Date[!is.na(training_set$SPY_LogReturn)]

# Converti in Date se non lo sono già
valid_dates <- as.Date(valid_dates)

# Sovrascrivi l’index di z_hat con le date corrette
index(z_hat) <- valid_dates

# Controlla che le lunghezze coincidano
if(length(z_hat) != length(valid_dates)) {
  stop("Errore: lunghezza residui e date non coincide!")
}

# Visualizza i primi valori
head(z_hat)
#                  [,1]
# 2023-08-01 -0.2715260
# 2023-08-02 -1.3272829
# 2023-08-03 -0.2719337
# 2023-08-04 -0.4294846
# 2023-08-07  0.8380293
# 2023-08-08 -0.4256933

# Test di SW
shapirowilk_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Shapiro-Wilk Test
# Statistic: 0.964001
# p-value  : 2.23949e-09
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Test di JB
jarque_bera_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Jarque-Bera Test
# Statistic: 165.827854
# p-value  : 0
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Come atteso, i residui standardizzati del GARCH(4,1) con distribuzione skewed Student-t non seguono una distribuzione normale.

# QQ-Plot SSTD:
# Residui standardizzati ordinati
y <- sort(as.numeric(z_hat))
n <- length(y)

# Parametri stimati da 'fit'
params <- coef(fit)
SSTD_xi <- params["skew"]
SSTD_nu <- params["shape"]
mean <- 0
sd <- 1

# Parametri della distribuzione skewed Student-t
distr <- "sstd"
distr_pars <- list(mean = mean, sd = sd, nu = SSTD_nu, xi = SSTD_xi)

# Quantili teorici
quants <- qsstd(ppoints(n), mean = mean, sd = sd, nu = SSTD_nu, xi = SSTD_xi)

# DataFrame per plotting
QQ_plot_df <- data.frame(
  T = 1:n,
  Q = quants,
  X = y,
  Y = y
)

# Calcolo retta interquartile
quart_probs <- c(0.25, 0.75)
quart_X <- as.vector(quantile(QQ_plot_df$X, quart_probs))
quart_Q <- qsstd(quart_probs, mean = mean, sd = sd, nu = SSTD_nu, xi = SSTD_xi)
slope <- diff(quart_X) / diff(quart_Q)
intercept <- quart_X[1] - slope * quart_Q[1]

# Titoli
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 MPSMF 2024-2025",
                             paste("Q-Q plot of the Standardized Residuals from the GARCH(4,1) model for SPY (skewed Student-t)")))
subtitle_content <- bquote(paste("Sample size: ", .(n), 
                                 "; SSTD parameters → mean = ", .(mean), 
                                 ", sd = ", .(sd), 
                                 ", df = ", .(round(SSTD_nu, 3)), 
                                 ", skew = ", .(round(SSTD_xi, 3)), "."))

caption_content <- "Author: Matteo Basili"
x_name <- bquote("Theoretical Quantiles (SSTD)")
y_name <- bquote("Standardized Residuals")

# Assi e legende
x_breaks <- seq(from = floor(min(QQ_plot_df$Q)), to = ceiling(max(QQ_plot_df$Q)), by = 0.5)
x_labs <- format(x_breaks, scientific = FALSE)
y_breaks_num <- length(x_breaks)
y_binwidth <- round((max(QQ_plot_df$Y) - min(QQ_plot_df$Y)) / y_breaks_num, digits = 3)
y_breaks <- round(seq(from = floor(min(QQ_plot_df$Y)/y_binwidth)*y_binwidth, 
                      to = ceiling(max(QQ_plot_df$Y)/y_binwidth)*y_binwidth, 
                      by = y_binwidth), 3)
y_labs <- format(y_breaks, scientific = FALSE)

# Legende e colori
leg_shape_labs <- bquote("Q-Q plot")
leg_fill_labs <- c(bquote("90% confidence interval"), bquote("95% confidence interval"))
leg_col_labs <- c(bquote("Interquartile line"), bquote("Regression line"), bquote("y = x"))

Stand_Res_std_QQ_plot <- ggplot(QQ_plot_df) +
  stat_qq_band(aes(sample = X, fill = "95"), distribution = distr, dparams = distr_pars, conf = 0.95) +
  stat_qq_band(aes(sample = X, fill = "90"), distribution = distr, dparams = distr_pars, conf = 0.90) +
  geom_abline(aes(slope = slope, intercept = intercept, colour = "IQR"), linewidth = 0.8) +
  stat_smooth(aes(x = Q, y = Y, colour = "Reg", group = 1), method = "lm", se = FALSE, linewidth = 0.8) +
  geom_abline(aes(slope = 1, intercept = 0, colour = "45deg"), linewidth = 0.8) +
  stat_qq_point(aes(sample = X), shape = 19, color = "black", size = 1.2, alpha = 0.8, distribution = distr, dparams = distr_pars) +
  scale_x_continuous(name = x_name, breaks = x_breaks, labels = x_labs) +
  scale_y_continuous(name = y_name, breaks = y_breaks, labels = NULL,
                     sec.axis = sec_axis(~ ., breaks = y_breaks, labels = y_labs)) +
  ggtitle(title_content) +
  labs(subtitle = subtitle_content, caption = caption_content) +
  scale_shape_manual(name = "", labels = leg_shape_labs, values = c("19")) +
  scale_fill_manual(name = "", values = c("90" = "chartreuse1", "95" = "deepskyblue1"), labels = leg_fill_labs) +
  scale_colour_manual(name = "", values = c("IQR" = "cyan", "Reg" = "red", "45deg" = "black"), labels = leg_col_labs) +
  guides(shape = guide_legend(order = 1), fill = guide_legend(order = 2), colour = guide_legend(order = 3)) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.x = element_text(angle = 0, vjust = 1),
    legend.key.width = unit(0.8, "cm"),
    legend.position = "bottom"
  )

# Mostra il plot
plot(Stand_Res_std_QQ_plot)

# Calcola densità teorica nei quantili (pdf)
dens <- dsstd(quants, mean=mean, sd=sd, nu=SSTD_nu, xi=SSTD_xi)

# Calcola errore standard dei quantili teorici (formula banda Q-Q)
# Errore standard: SE = (p * (1-p)) / (n * f(Q)^2)
p <- ppoints(n)
SE <- sqrt(p * (1-p) / (n * dens^2))

# Limiti di confidenza (95%)
alpha <- 0.05
z <- qnorm(1 - alpha/2)

lower_95 <- quants - z * SE
upper_95 <- quants + z * SE

# Verifica quanti punti osservati cadono dentro le bande
# Ricorda che QQ_plot_df$X = dati ordinati osservati

inside_95 <- (QQ_plot_df$X >= lower_95) & (QQ_plot_df$X <= upper_95)

percent_inside_95 <- mean(inside_95) * 100

cat(sprintf("Percentuale di punti dentro la banda di confidenza al 95%%: %.2f%%\n", percent_inside_95))
# Percentuale di punti dentro la banda di confidenza al 95%: 97.05%

# Alpha per il 90%
alpha_90 <- 0.10
z_90 <- qnorm(1 - alpha_90/2)  # quantile normale per il 90%

# Limiti di confidenza al 90%
lower_90 <- quants - z_90 * SE
upper_90 <- quants + z_90 * SE

# Verifica quanti punti osservati cadono dentro la banda 90%
inside_90 <- (QQ_plot_df$X >= lower_90) & (QQ_plot_df$X <= upper_90)

percent_inside_90 <- mean(inside_90) * 100

cat(sprintf("Percentuale di punti dentro la banda di confidenza al 90%%: %.2f%%\n", percent_inside_90))
# Percentuale di punti dentro la banda di confidenza al 90%: 88.61%

# Kolmogorov-Smirnov test (K-S test)
ks_result <- ks.test(y, 
                     y = function(q) psstd(q, mean=0, sd=1, nu=SSTD_nu, xi=SSTD_xi),
                     alternative = "two.sided")

print(ks_result)
# 
#         Asymptotic one-sample Kolmogorov-Smirnov test
# 
# data:  y
# D = 0.049515, p-value = 0.1955
# alternative hypothesis: two-sided

# Dall'analisi del Q-Q Plot e del test di K-S vediamo, quindi, che i residui standardizzati seguono la distribuzione skewed Student-t con un livello
# di significatività del 5% (in realtà il test di K-S risulta anche al 10%).

# Verifichiamo ora la stazionarietà e l'omoschedasticità (non condizionata).

# Grafico dei residui
df_residui <- data.frame(
  Date = index(z_hat),
  Residui = as.numeric(z_hat)
)

ggplot(df_residui, aes(x = Date, y = Residui)) +
  geom_line(color = "steelblue", linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  labs(
    title = "Grafico dei residui standardizzati",
    subtitle = "Modello GARCH(4,1) con distribuzione skewed Student-t per SPY",
    x = "Data",
    y = "Residui standardizzati"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    axis.title = element_text(size = 11)
  )

# KPSS Test
kpss_test_level(df_residui, col = "Residui", alpha = 0.01)
# 
# [Residui] KPSS Test - Stazionarietà attorno a una media costante
# Statistic: 0.132848
# Valore critico (circa 1%): 0.739000
# Interpretazione del p-value: p-value > 0.10
# 
# ** CONCLUSIONE: Statistic <= valore critico (1%) **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO stazionari.

# Quindi, i residui standardizzati sono stazionari.

# Per l'omoschedasticità non condizionata, per prima cosa, vediamo se c'è curtosi nei residui standardizzati.

# Calcola la curtosi
kurt_z_hat <- kurtosis(as.numeric(z_hat), na.rm = TRUE)

cat(sprintf("Curtosi dei residui standardizzati: %.4f\n", kurt_z_hat))
# Curtosi dei residui standardizzati: 5.5602

# La curtosi c'è. Quindi, applichiamo la forma "studentized" dei test di Breusch-Pagan e White per la stazionarietà in varianza.

# Breusch-Pagan Test:
# Crea un indice temporale (può essere anche un numero progressivo)
df_residui$Index <- seq_along(df_residui$Residui)

# Esegui il test BP
breusch_pagan_test(
  index = df_residui$Index,
  group = df_residui$Residui,
  col = "Residui standardizzati"
)
# [Residui standardizzati] Breusch-Pagan Test
# Statistic: 4.209761
# p-value: 0.040192
# 
# ** Conclusione: p-value > 0.01 **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON sono eteroschedastici.

# White Test
white_test(
  index = df_residui$Index,
  group = df_residui$Residui,
  col = "Residui standardizzati"
)
# [Residui standardizzati] White Test
# Statistic: 5.233825
# p-value: 0.073028
# 
# ** Conclusione: p-value > 0.01 **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON sono eteroschedastici.

# Dai risultati dei test si vede che è verificata anche la condizione di omoschedasticità (non condizionata).

# Per verificare ulteriormente l'assenza di autocorrelazione per i residui standardizzati
# di questo modello, facciamo anche l'ACF, il PACF e il test di Ljung-Box classico.

# ACF
df_z <- data.frame(z_hat = as.numeric(z_hat))
plot_acf_residuals(df_z, col = "z_hat")

# PACF
plot_pacf_residuals(df_z, col = "z_hat")

# L’analisi visiva di ACF e PACF mostra che, in entrambi i grafici, nessuna barra supera la banda di confidenza al 90%.
# Dunque, non c'è una forte evidenza visiva di autocorrelazione.

# Ljung-Box Test
ljungbox_test(as.numeric(z_hat), col_name = "Residui standardizzati")
# Ljung-Box test per Residui standardizzati (max_lag = 10):
#            lag   lb_stat lb_pvalue
# X-squared    1 0.1859421 0.6663156
# X-squared1   2 0.3202445 0.8520396
# X-squared2   3 1.1112083 0.7743691
# X-squared3   4 1.1392174 0.8880011
# X-squared4   5 1.1871759 0.9461017
# X-squared5   6 1.8414947 0.9336837
# X-squared6   7 2.4451221 0.9311724
# X-squared7   8 2.4513585 0.9639606
# X-squared8   9 3.0237194 0.9633471
# X-squared9  10 3.0287503 0.9807393

# Osservando i p-value associati ai vari lag, vediamo che tutti questi sono maggiori di α = 0.10. Dunque, non abbiamo evidenza statistica
# sufficiente per rifiutare l’ipotesi nulla di assenza di autocorrelazione fino al lag specificato.

# Infine, verifichiamo anche l’assenza di eteroschedasticità condizionata nei residui standardizzati.

# Test ARCH di Engle
arch_effects_test(z_hat, col_name = "Residui Standardizzati")
# ARCH Test per Residui Standardizzati (max_lag = 5):
#            lag arch_stat  arch_pvalue
# statistic    1  11.26432 0.0007901096
# statistic1   2  13.36075 0.0012553073
# statistic2   3  15.99518 0.0011365680
# statistic3   4  18.74877 0.0008804792
# statistic4   5  18.71256 0.0021740108

# Alla luce dei risultati, a partire dal lag 1, il test ARCH rivela evidenza significativa di eteroschedasticità residua, con p-value < 1%.
# Questo suggerisce che il modello GARCH(4,1) con distribuzione skewed Student-t non ha catturato completamente la dinamica della varianza
# condizionata, e che potrebbero essere presenti effetti ARCH residui a orizzonti più lunghi.
# Perciò, possiamo concludere che anche questo modello è da scartare.

# Analizziamo il prossimo nella classifica: GARCH(3,2) sstd
p <- valid_models$p[30]
q <- valid_models$q[30]
dist <- valid_models$dist[30]

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = dist
)

fit <- tryCatch(
  ugarchfit(spec = spec, data = returns, solver = "hybrid"),
  error = function(e) NULL
)

# Risultati
show(fit)
# 
# *---------------------------------*
# *          GARCH Model Fit        *
# *---------------------------------*
#   
# Conditional Variance Dynamics 	
# -----------------------------------
# GARCH Model	    : sGARCH(3,2)
# Mean Model	    : ARFIMA(0,0,0)
# Distribution	  : sstd 
# 
# Optimal Parameters
# ------------------------------------
#         Estimate  Std. Error   t value Pr(>|t|)
# omega   0.116485    0.066736  1.745457 0.080905
# alpha1  0.035036    0.007359  4.761017 0.000002
# alpha2  0.000000    0.050244  0.000000 1.000000
# alpha3  0.162333    0.062813  2.584387 0.009755
# beta1   0.691874    0.864803  0.800037 0.423690
# beta2   0.000000    0.676979  0.000001 0.999999
# skew    0.809349    0.050722 15.956561 0.000000
# shape   5.252360    1.354685  3.877183 0.000106
# 
# Robust Standard Errors:
#         Estimate  Std. Error  t value Pr(>|t|)
# omega   0.116485    0.120847  0.96391 0.335092
# alpha1  0.035036    0.070860  0.49443 0.621002
# alpha2  0.000000    0.075256  0.00000 1.000000
# alpha3  0.162333    0.110448  1.46977 0.141624
# beta1   0.691874    1.295951  0.53387 0.593429
# beta2   0.000000    1.009855  0.00000 1.000000
# skew    0.809349    0.046769 17.30516 0.000000
# shape   5.252360    1.638087  3.20640 0.001344
# 
# LogLikelihood : -594.5994 
# 
# Information Criteria
# ------------------------------------
#   
# Akaike       2.5426
# Bayes        2.6128
# Shibata      2.5421
# Hannan-Quinn 2.5702
# 
# Weighted Ljung-Box Test on Standardized Residuals
# ------------------------------------
#                         statistic p-value
# Lag[1]                     0.2036  0.6518
# Lag[2*(p+q)+(p+q)-1][2]    0.2628  0.8168
# Lag[4*(p+q)+(p+q)-1][5]    0.7282  0.9174
# d.o.f=0
# H0 : No serial correlation
# 
# Weighted Ljung-Box Test on Standardized Squared Residuals
# ------------------------------------
#                          statistic   p-value
# Lag[1]                       11.68 0.0006334
# Lag[2*(p+q)+(p+q)-1][14]     19.70 0.0024838
# Lag[4*(p+q)+(p+q)-1][24]     22.18 0.0214362
# d.o.f=5
# 
# Weighted ARCH LM Tests
# ------------------------------------
#              Statistic Shape Scale P-Value
# ARCH Lag[6]    0.01857 0.500 2.000  0.8916
# ARCH Lag[8]    0.18490 1.480 1.774  0.9747
# ARCH Lag[10]   0.78549 2.424 1.650  0.9612
# 
# Nyblom stability test
# ------------------------------------
# Joint Statistic:  1.6157
# Individual Statistics:              
# omega  0.04247
# alpha1 0.04631
# alpha2 0.06822
# alpha3 0.05915
# beta1  0.03108
# beta2  0.03236
# skew   0.08626
# shape  0.32699
# 
# Asymptotic Critical Values (10% 5% 1%)
# Joint Statistic:     	   1.89 2.11 2.59
# Individual Statistic:	   0.35 0.47 0.75
# 
# Sign Bias Test
# ------------------------------------
#                    t-value    prob sig
# Sign Bias           0.7611 0.44697    
# Negative Sign Bias  2.5583 0.01083  **
# Positive Sign Bias  0.2766 0.78224    
# Joint Effect       13.3816 0.00388 ***
#   
#   
# Adjusted Pearson Goodness-of-Fit Test:
# ------------------------------------
#   group statistic p-value(g-1)
# 1    20     22.96      0.23902
# 2    30     37.27      0.13955
# 3    40     46.17      0.20012
# 4    50     67.35      0.04199
# 
# 
# Elapsed time : 0.1889911 

# Analizzando i p-value del test pesato di Ljung-Box sui residui standardizzati, osserviamo che questi sono tutti maggiori di α = 0.01 (in realtà
# anche di α = 0.05 e α = 0.10). Quindi, non abbiamo evidenza statistica sufficiente per rifiutare l’ipotesi nulla di assenza di autocorrelazione.

# Applichiamo, ora, dei dest di normalità sui residui.

# Estrai residui standardizzati
z_hat <- residuals(fit, standardize = TRUE)

# Prendi le date del training_set:
# Rimuoviamo le righe con NA nella colonna usata per il modello
valid_dates <- training_set$Date[!is.na(training_set$SPY_LogReturn)]

# Converti in Date se non lo sono già
valid_dates <- as.Date(valid_dates)

# Sovrascrivi l’index di z_hat con le date corrette
index(z_hat) <- valid_dates

# Controlla che le lunghezze coincidano
if(length(z_hat) != length(valid_dates)) {
  stop("Errore: lunghezza residui e date non coincide!")
}

# Visualizza i primi valori
head(z_hat)
#                  [,1]
# 2023-08-01 -0.2715260
# 2023-08-02 -1.3272829
# 2023-08-03 -0.2719337
# 2023-08-04 -0.4769088
# 2023-08-07  0.8412137
# 2023-08-08 -0.4607416

# Test di SW
shapirowilk_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Shapiro-Wilk Test
# Statistic: 0.961570
# p-value  : 8.60256e-10
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Test di JB
jarque_bera_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Jarque-Bera Test
# Statistic: 187.182294
# p-value  : 0
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Come atteso, i residui standardizzati del GARCH(3,2) con distribuzione skewed Student-t non seguono una distribuzione normale.

# QQ-Plot SSTD:
# Residui standardizzati ordinati
y <- sort(as.numeric(z_hat))
n <- length(y)

# Parametri stimati da 'fit'
params <- coef(fit)
SSTD_xi <- params["skew"]
SSTD_nu <- params["shape"]
mean <- 0
sd <- 1

# Parametri della distribuzione skewed Student-t
distr <- "sstd"
distr_pars <- list(mean = mean, sd = sd, nu = SSTD_nu, xi = SSTD_xi)

# Quantili teorici
quants <- qsstd(ppoints(n), mean = mean, sd = sd, nu = SSTD_nu, xi = SSTD_xi)

# DataFrame per plotting
QQ_plot_df <- data.frame(
  T = 1:n,
  Q = quants,
  X = y,
  Y = y
)

# Calcolo retta interquartile
quart_probs <- c(0.25, 0.75)
quart_X <- as.vector(quantile(QQ_plot_df$X, quart_probs))
quart_Q <- qsstd(quart_probs, mean = mean, sd = sd, nu = SSTD_nu, xi = SSTD_xi)
slope <- diff(quart_X) / diff(quart_Q)
intercept <- quart_X[1] - slope * quart_Q[1]

# Titoli
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 MPSMF 2024-2025",
                             paste("Q-Q plot of the Standardized Residuals from the GARCH(3,2) model for SPY (skewed Student-t)")))
subtitle_content <- bquote(paste("Sample size: ", .(n), 
                                 "; SSTD parameters → mean = ", .(mean), 
                                 ", sd = ", .(sd), 
                                 ", df = ", .(round(SSTD_nu, 3)), 
                                 ", skew = ", .(round(SSTD_xi, 3)), "."))

caption_content <- "Author: Matteo Basili"
x_name <- bquote("Theoretical Quantiles (SSTD)")
y_name <- bquote("Standardized Residuals")

# Assi e legende
x_breaks <- seq(from = floor(min(QQ_plot_df$Q)), to = ceiling(max(QQ_plot_df$Q)), by = 0.5)
x_labs <- format(x_breaks, scientific = FALSE)
y_breaks_num <- length(x_breaks)
y_binwidth <- round((max(QQ_plot_df$Y) - min(QQ_plot_df$Y)) / y_breaks_num, digits = 3)
y_breaks <- round(seq(from = floor(min(QQ_plot_df$Y)/y_binwidth)*y_binwidth, 
                      to = ceiling(max(QQ_plot_df$Y)/y_binwidth)*y_binwidth, 
                      by = y_binwidth), 3)
y_labs <- format(y_breaks, scientific = FALSE)

# Legende e colori
leg_shape_labs <- bquote("Q-Q plot")
leg_fill_labs <- c(bquote("90% confidence interval"), bquote("95% confidence interval"))
leg_col_labs <- c(bquote("Interquartile line"), bquote("Regression line"), bquote("y = x"))

Stand_Res_std_QQ_plot <- ggplot(QQ_plot_df) +
  stat_qq_band(aes(sample = X, fill = "95"), distribution = distr, dparams = distr_pars, conf = 0.95) +
  stat_qq_band(aes(sample = X, fill = "90"), distribution = distr, dparams = distr_pars, conf = 0.90) +
  geom_abline(aes(slope = slope, intercept = intercept, colour = "IQR"), linewidth = 0.8) +
  stat_smooth(aes(x = Q, y = Y, colour = "Reg", group = 1), method = "lm", se = FALSE, linewidth = 0.8) +
  geom_abline(aes(slope = 1, intercept = 0, colour = "45deg"), linewidth = 0.8) +
  stat_qq_point(aes(sample = X), shape = 19, color = "black", size = 1.2, alpha = 0.8, distribution = distr, dparams = distr_pars) +
  scale_x_continuous(name = x_name, breaks = x_breaks, labels = x_labs) +
  scale_y_continuous(name = y_name, breaks = y_breaks, labels = NULL,
                     sec.axis = sec_axis(~ ., breaks = y_breaks, labels = y_labs)) +
  ggtitle(title_content) +
  labs(subtitle = subtitle_content, caption = caption_content) +
  scale_shape_manual(name = "", labels = leg_shape_labs, values = c("19")) +
  scale_fill_manual(name = "", values = c("90" = "chartreuse1", "95" = "deepskyblue1"), labels = leg_fill_labs) +
  scale_colour_manual(name = "", values = c("IQR" = "cyan", "Reg" = "red", "45deg" = "black"), labels = leg_col_labs) +
  guides(shape = guide_legend(order = 1), fill = guide_legend(order = 2), colour = guide_legend(order = 3)) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.x = element_text(angle = 0, vjust = 1),
    legend.key.width = unit(0.8, "cm"),
    legend.position = "bottom"
  )

# Mostra il plot
plot(Stand_Res_std_QQ_plot)

# Calcola densità teorica nei quantili (pdf)
dens <- dsstd(quants, mean=mean, sd=sd, nu=SSTD_nu, xi=SSTD_xi)

# Calcola errore standard dei quantili teorici (formula banda Q-Q)
# Errore standard: SE = (p * (1-p)) / (n * f(Q)^2)
p <- ppoints(n)
SE <- sqrt(p * (1-p) / (n * dens^2))

# Limiti di confidenza (95%)
alpha <- 0.05
z <- qnorm(1 - alpha/2)

lower_95 <- quants - z * SE
upper_95 <- quants + z * SE

# Verifica quanti punti osservati cadono dentro le bande
# Ricorda che QQ_plot_df$X = dati ordinati osservati

inside_95 <- (QQ_plot_df$X >= lower_95) & (QQ_plot_df$X <= upper_95)

percent_inside_95 <- mean(inside_95) * 100

cat(sprintf("Percentuale di punti dentro la banda di confidenza al 95%%: %.2f%%\n", percent_inside_95))
# Percentuale di punti dentro la banda di confidenza al 95%: 97.05%

# Alpha per il 90%
alpha_90 <- 0.10
z_90 <- qnorm(1 - alpha_90/2)  # quantile normale per il 90%

# Limiti di confidenza al 90%
lower_90 <- quants - z_90 * SE
upper_90 <- quants + z_90 * SE

# Verifica quanti punti osservati cadono dentro la banda 90%
inside_90 <- (QQ_plot_df$X >= lower_90) & (QQ_plot_df$X <= upper_90)

percent_inside_90 <- mean(inside_90) * 100

cat(sprintf("Percentuale di punti dentro la banda di confidenza al 90%%: %.2f%%\n", percent_inside_90))
# Percentuale di punti dentro la banda di confidenza al 90%: 89.03%

# Kolmogorov-Smirnov test (K-S test)
ks_result <- ks.test(y, 
                     y = function(q) psstd(q, mean=0, sd=1, nu=SSTD_nu, xi=SSTD_xi),
                     alternative = "two.sided")

print(ks_result)
# 
#         Asymptotic one-sample Kolmogorov-Smirnov test
# 
# data:  y
# D = 0.050764, p-value = 0.1737
# alternative hypothesis: two-sided

# Dall'analisi del Q-Q Plot e del test di K-S vediamo, quindi, che i residui standardizzati seguono la distribuzione skewed Student-t con un livello
# di significatività del 5% (in realtà il test di K-S risulta anche al 10%).

# Verifichiamo ora la stazionarietà e l'omoschedasticità (non condizionata).

# Grafico dei residui
df_residui <- data.frame(
  Date = index(z_hat),
  Residui = as.numeric(z_hat)
)

ggplot(df_residui, aes(x = Date, y = Residui)) +
  geom_line(color = "steelblue", linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  labs(
    title = "Grafico dei residui standardizzati",
    subtitle = "Modello GARCH(3,2) con distribuzione skewed Student-t per SPY",
    x = "Data",
    y = "Residui standardizzati"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    axis.title = element_text(size = 11)
  )

# KPSS Test
kpss_test_level(df_residui, col = "Residui", alpha = 0.01)
# 
# [Residui] KPSS Test - Stazionarietà attorno a una media costante
# Statistic: 0.139924
# Valore critico (circa 1%): 0.739000
# Interpretazione del p-value: p-value > 0.10
# 
# ** CONCLUSIONE: Statistic <= valore critico (1%) **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO stazionari.

# Quindi, i residui standardizzati sono stazionari.

# Per l'omoschedasticità non condizionata, per prima cosa, vediamo se c'è curtosi nei residui standardizzati.

# Calcola la curtosi
kurt_z_hat <- kurtosis(as.numeric(z_hat), na.rm = TRUE)

cat(sprintf("Curtosi dei residui standardizzati: %.4f\n", kurt_z_hat))
# Curtosi dei residui standardizzati: 5.7231

# La curtosi c'è. Quindi, applichiamo la forma "studentized" dei test di Breusch-Pagan e White per la stazionarietà in varianza.

# Breusch-Pagan Test:
# Crea un indice temporale (può essere anche un numero progressivo)
df_residui$Index <- seq_along(df_residui$Residui)

# Esegui il test BP
breusch_pagan_test(
  index = df_residui$Index,
  group = df_residui$Residui,
  col = "Residui standardizzati"
)
# [Residui standardizzati] Breusch-Pagan Test
# Statistic: 4.391565
# p-value: 0.0361171
# 
# ** Conclusione: p-value > 0.01 **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON sono eteroschedastici.

# White Test
white_test(
  index = df_residui$Index,
  group = df_residui$Residui,
  col = "Residui standardizzati"
)
# [Residui standardizzati] White Test
# Statistic: 5.455685
# p-value: 0.0653601
# 
# ** Conclusione: p-value > 0.01 **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON sono eteroschedastici.

# Dai risultati dei test si vede che è verificata anche la condizione di omoschedasticità (non condizionata).

# Per verificare ulteriormente l'assenza di autocorrelazione per i residui standardizzati
# di questo modello, facciamo anche l'ACF, il PACF e il test di Ljung-Box classico.

# ACF
df_z <- data.frame(z_hat = as.numeric(z_hat))
plot_acf_residuals(df_z, col = "z_hat")

# PACF
plot_pacf_residuals(df_z, col = "z_hat")

# L’analisi visiva di ACF e PACF mostra che, in entrambi i grafici, nessuna barra supera la banda di confidenza al 90%.
# Dunque, non c'è una forte evidenza visiva di autocorrelazione.

# Ljung-Box Test
ljungbox_test(as.numeric(z_hat), col_name = "Residui standardizzati")
# Ljung-Box test per Residui standardizzati (max_lag = 10):
#            lag   lb_stat lb_pvalue
# X-squared    1 0.2036232 0.6518121
# X-squared1   2 0.3219524 0.8513123
# X-squared2   3 1.0137406 0.7979272
# X-squared3   4 1.0297848 0.9052465
# X-squared4   5 1.0718051 0.9565688
# X-squared5   6 1.8547817 0.9325579
# X-squared6   7 2.3964299 0.9346920
# X-squared7   8 2.4130872 0.9656606
# X-squared8   9 2.9420358 0.9665487
# X-squared9  10 2.9583811 0.9823866

# Osservando i p-value associati ai vari lag, vediamo che tutti questi sono maggiori di α = 0.10. Dunque, non abbiamo evidenza statistica
# sufficiente per rifiutare l’ipotesi nulla di assenza di autocorrelazione fino al lag specificato.

# Infine, verifichiamo anche l’assenza di eteroschedasticità condizionata nei residui standardizzati.

# Test ARCH di Engle
arch_effects_test(z_hat, col_name = "Residui Standardizzati")
# ARCH Test per Residui Standardizzati (max_lag = 5):
#            lag arch_stat  arch_pvalue
# statistic    1  12.64673 0.0003762237
# statistic1   2  15.11547 0.0005220558
# statistic2   3  16.29190 0.0009879475
# statistic3   4  22.01802 0.0001987717
# statistic4   5  21.98998 0.0005259002

# Alla luce dei risultati, a partire dal lag 1, il test ARCH rivela evidenza significativa di eteroschedasticità residua, con p-value < 1%.
# Questo suggerisce che il modello GARCH(3,2) con distribuzione skewed Student-t non ha catturato completamente la dinamica della varianza
# condizionata, e che potrebbero essere presenti effetti ARCH residui a orizzonti più lunghi.
# Perciò, possiamo concludere che anche questo modello è da scartare.

# Analizziamo il prossimo nella classifica: GARCH(4,1) sged
p <- valid_models$p[31]
q <- valid_models$q[31]
dist <- valid_models$dist[31]

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = dist
)

fit <- tryCatch(
  ugarchfit(spec = spec, data = returns, solver = "hybrid"),
  error = function(e) NULL
)

# Risultati
show(fit)
# 
# *---------------------------------*
# *          GARCH Model Fit        *
# *---------------------------------*
#   
# Conditional Variance Dynamics 	
# -----------------------------------
# GARCH Model	    : sGARCH(4,1)
# Mean Model	    : ARFIMA(0,0,0)
# Distribution	  : sged 
# 
# Optimal Parameters
# ------------------------------------
#         Estimate  Std. Error  t value Pr(>|t|)
# omega   0.120182    0.042198   2.8480 0.004399
# alpha1  0.041154    0.037729   1.0908 0.275377
# alpha2  0.000000    0.054014   0.0000 1.000000
# alpha3  0.087421    0.075216   1.1623 0.245129
# alpha4  0.149657    0.104407   1.4334 0.151742
# beta1   0.603965    0.083864   7.2017 0.000000
# skew    0.862880    0.041977  20.5562 0.000000
# shape   1.281121    0.118590  10.8030 0.000000
# 
# Robust Standard Errors:
#         Estimate  Std. Error  t value Pr(>|t|)
# omega   0.120182    0.024477   4.9100 0.000001
# alpha1  0.041154    0.035818   1.1490 0.250572
# alpha2  0.000000    0.054711   0.0000 1.000000
# alpha3  0.087421    0.074383   1.1753 0.239883
# alpha4  0.149657    0.122579   1.2209 0.222124
# beta1   0.603965    0.064951   9.2988 0.000000
# skew    0.862880    0.041641  20.7217 0.000000
# shape   1.281121    0.148535   8.6250 0.000000
# 
# LogLikelihood : -594.8713 
# 
# Information Criteria
# ------------------------------------
#   
# Akaike       2.5438
# Bayes        2.6140
# Shibata      2.5432
# Hannan-Quinn 2.5714
# 
# Weighted Ljung-Box Test on Standardized Residuals
# ------------------------------------
#                         statistic p-value
# Lag[1]                    0.07872  0.7790
# Lag[2*(p+q)+(p+q)-1][2]   0.12702  0.9011
# Lag[4*(p+q)+(p+q)-1][5]   0.60519  0.9403
# d.o.f=0
# H0 : No serial correlation
# 
# Weighted Ljung-Box Test on Standardized Squared Residuals
# ------------------------------------
#                          statistic p-value
# Lag[1]                       4.536 0.03319
# Lag[2*(p+q)+(p+q)-1][14]     9.492 0.23817
# Lag[4*(p+q)+(p+q)-1][24]    12.023 0.50256
# d.o.f=5
# 
# Weighted ARCH LM Tests
# ------------------------------------
#              Statistic Shape Scale P-Value
# ARCH Lag[6]    0.07757 0.500 2.000  0.7806
# ARCH Lag[8]    0.37380 1.480 1.774  0.9327
# ARCH Lag[10]   1.23074 2.424 1.650  0.9040
# 
# Nyblom stability test
# ------------------------------------
# Joint Statistic:  1.5253
# Individual Statistics:              
# omega  0.07368
# alpha1 0.10480
# alpha2 0.21655
# alpha3 0.18948
# alpha4 0.21567
# beta1  0.07477
# skew   0.28321
# shape  0.38109
# 
# Asymptotic Critical Values (10% 5% 1%)
# Joint Statistic:     	   1.89 2.11 2.59
# Individual Statistic:	   0.35 0.47 0.75
# 
# Sign Bias Test
# ------------------------------------
#                     t-value     prob sig
# Sign Bias           1.18776 0.235528    
# Negative Sign Bias  1.98260 0.047995  **
# Positive Sign Bias  0.03006 0.976034    
# Joint Effect       12.60800 0.005566 ***
#   
#   
# Adjusted Pearson Goodness-of-Fit Test:
# ------------------------------------
#   group statistic p-value(g-1)
# 1    20     22.96       0.2390
# 2    30     34.73       0.2134
# 3    40     41.95       0.3442
# 4    50     60.81       0.1200
# 
# 
# Elapsed time : 0.8319969 

# Analizzando i p-value del test pesato di Ljung-Box sui residui standardizzati, osserviamo che questi sono tutti maggiori di α = 0.01
# (in realtà anche di α = 0.05 e α = 0.10). Quindi, non abbiamo evidenza statistica sufficiente per rifiutare l’ipotesi nulla di assenza di
# autocorrelazione.

# Applichiamo, ora, dei dest di normalità sui residui.

# Estrai residui standardizzati
z_hat <- residuals(fit, standardize = TRUE)

# Prendi le date del training_set:
# Rimuoviamo le righe con NA nella colonna usata per il modello
valid_dates <- training_set$Date[!is.na(training_set$SPY_LogReturn)]

# Converti in Date se non lo sono già
valid_dates <- as.Date(valid_dates)

# Sovrascrivi l’index di z_hat con le date corrette
index(z_hat) <- valid_dates

# Controlla che le lunghezze coincidano
if(length(z_hat) != length(valid_dates)) {
  stop("Errore: lunghezza residui e date non coincide!")
}

# Visualizza i primi valori
head(z_hat)
#                  [,1]
# 2023-08-01 -0.2715260
# 2023-08-02 -1.3272829
# 2023-08-03 -0.2719337
# 2023-08-04 -0.4294846
# 2023-08-07  0.8755324
# 2023-08-08 -0.4258833

# Test di SW
shapirowilk_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Shapiro-Wilk Test
# Statistic: 0.968424
# p-value  : 1.41904e-08
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Test di JB
jarque_bera_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Jarque-Bera Test
# Statistic: 124.136484
# p-value  : 0
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Come atteso, i residui standardizzati del GARCH(4,1) con distribuzione skewed GED non seguono una distribuzione normale.

# QQ-Plot SGED:
# Residui standardizzati ordinati
y <- sort(as.numeric(z_hat))
n <- length(y)

# Parametri stimati da 'fit'
params <- coef(fit)
SGED_xi <- params["skew"]
SGED_nu <- params["shape"]
mean <- 0
sd <- 1

# Parametri della distribuzione skewed GED
distr <- "sged"
distr_pars <- list(mean = mean, sd = sd, nu = SGED_nu, xi = SGED_xi)

# Quantili teorici
quants <- qsged(ppoints(n), mean = mean, sd = sd, nu = SGED_nu, xi = SGED_xi)

# DataFrame per plotting
QQ_plot_df <- data.frame(
  T = 1:n,
  Q = quants,
  X = y,
  Y = y
)

# Calcolo retta interquartile
quart_probs <- c(0.25, 0.75)
quart_X <- as.vector(quantile(QQ_plot_df$X, quart_probs))
quart_Q <- qsged(quart_probs, mean = mean, sd = sd, nu = SGED_nu, xi = SGED_xi)
slope <- diff(quart_X) / diff(quart_Q)
intercept <- quart_X[1] - slope * quart_Q[1]

# Titoli
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 MPSMF 2024-2025",
                             paste("Q-Q plot of the Standardized Residuals from the GARCH(4,1) model for SPY (skewed GED)")))
subtitle_content <- bquote(paste("Sample size: ", .(n), 
                                 "; SGED parameters → mean = ", .(mean), 
                                 ", sd = ", .(sd), 
                                 ", shape = ", .(round(SGED_nu, 3)), 
                                 ", skew = ", .(round(SGED_xi, 3)), "."))

caption_content <- "Author: Matteo Basili"
x_name <- bquote("Theoretical Quantiles (SGED)")
y_name <- bquote("Standardized Residuals")

# Assi e legende
x_breaks <- seq(from = floor(min(QQ_plot_df$Q)), to = ceiling(max(QQ_plot_df$Q)), by = 0.5)
x_labs <- format(x_breaks, scientific = FALSE)
y_breaks_num <- length(x_breaks)
y_binwidth <- round((max(QQ_plot_df$Y) - min(QQ_plot_df$Y)) / y_breaks_num, digits = 3)
y_breaks <- round(seq(from = floor(min(QQ_plot_df$Y)/y_binwidth)*y_binwidth, 
                      to = ceiling(max(QQ_plot_df$Y)/y_binwidth)*y_binwidth, 
                      by = y_binwidth), 3)
y_labs <- format(y_breaks, scientific = FALSE)

# Legende e colori
leg_shape_labs <- bquote("Q-Q plot")
leg_fill_labs <- c(bquote("90% confidence interval"), bquote("95% confidence interval"))
leg_col_labs <- c(bquote("Interquartile line"), bquote("Regression line"), bquote("y = x"))

Stand_Res_std_QQ_plot <- ggplot(QQ_plot_df) + 
  stat_qq_band(aes(sample = X, fill = "95"), distribution = distr, dparams = distr_pars, conf = 0.95) +
  stat_qq_band(aes(sample = X, fill = "90"), distribution = distr, dparams = distr_pars, conf = 0.90) +
  geom_abline(aes(slope = slope, intercept = intercept, colour = "IQR"), linewidth = 0.8) +
  stat_smooth(aes(x = Q, y = Y, colour = "Reg", group = 1), method = "lm", se = FALSE, linewidth = 0.8) +
  geom_abline(aes(slope = 1, intercept = 0, colour = "45deg"), linewidth = 0.8) +
  stat_qq_point(aes(sample = X), shape = 19, color = "black", size = 1.2, alpha = 0.8, distribution = distr, dparams = distr_pars) +
  scale_x_continuous(name = x_name, breaks = x_breaks, labels = x_labs) +
  scale_y_continuous(name = y_name, breaks = y_breaks, labels = NULL,
                     sec.axis = sec_axis(~ ., breaks = y_breaks, labels = y_labs)) +
  ggtitle(title_content) +
  labs(subtitle = subtitle_content, caption = caption_content) +
  scale_shape_manual(name = "", labels = leg_shape_labs, values = c("19")) +
  scale_fill_manual(name = "", values = c("90" = "chartreuse1", "95" = "deepskyblue1"), labels = leg_fill_labs) +
  scale_colour_manual(name = "", values = c("IQR" = "cyan", "Reg" = "red", "45deg" = "black"), labels = leg_col_labs) +
  guides(shape = guide_legend(order = 1), fill = guide_legend(order = 2), colour = guide_legend(order = 3)) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.x = element_text(angle = 0, vjust = 1),
    legend.key.width = unit(0.8, "cm"),
    legend.position = "bottom"
  )

# Mostra il plot
plot(Stand_Res_std_QQ_plot)

# Calcola densità teorica nei quantili (SGED)
dens <- dsged(quants, mean = mean, sd = sd, nu = SGED_nu, xi = SGED_xi)

# Probabilità associate ai quantili
p <- ppoints(n)

# Errore standard dei quantili teorici
SE <- sqrt(p * (1 - p) / (n * dens^2))

# Limiti di confidenza al 95%
alpha_95 <- 0.05
z_95 <- qnorm(1 - alpha_95 / 2)
lower_95 <- quants - z_95 * SE
upper_95 <- quants + z_95 * SE

# Percentuale di punti dentro la banda 95%
inside_95 <- (QQ_plot_df$X >= lower_95) & (QQ_plot_df$X <= upper_95)
percent_inside_95 <- mean(inside_95) * 100
cat(sprintf("Percentuale di punti dentro la banda di confidenza al 95%%: %.2f%%\n", percent_inside_95))
# Percentuale di punti dentro la banda di confidenza al 95%: 98.73%

# Limiti di confidenza al 90%
alpha_90 <- 0.10
z_90 <- qnorm(1 - alpha_90 / 2)
lower_90 <- quants - z_90 * SE
upper_90 <- quants + z_90 * SE

# Percentuale di punti dentro la banda 90%
inside_90 <- (QQ_plot_df$X >= lower_90) & (QQ_plot_df$X <= upper_90)
percent_inside_90 <- mean(inside_90) * 100
cat(sprintf("Percentuale di punti dentro la banda di confidenza al 90%%: %.2f%%\n", percent_inside_90))
# Percentuale di punti dentro la banda di confidenza al 90%: 90.93%

# Kolmogorov-Smirnov test (K-S test)
ks_result <- ks.test(y, 
                     y = function(q) psged(q, mean=0, sd=1, nu=SGED_nu, xi=SGED_xi),
                     alternative = "two.sided")

print(ks_result)
# 
#         Asymptotic one-sample Kolmogorov-Smirnov test
# 
# data:  y
# D = 0.045313, p-value = 0.2848
# alternative hypothesis: two-sided

# Dall'analisi del Q-Q Plot e del test di K-S vediamo, quindi, che i residui standardizzati seguono la distribuzione skewed GED con un livello
# di significatività del 10%.

# Verifichiamo ora la stazionarietà e l'omoschedasticità (non condizionata).

# Grafico dei residui
df_residui <- data.frame(
  Date = index(z_hat),
  Residui = as.numeric(z_hat)
)

ggplot(df_residui, aes(x = Date, y = Residui)) +
  geom_line(color = "steelblue", linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  labs(
    title = "Grafico dei residui standardizzati",
    subtitle = "Modello GARCH(4,1) con distribuzione skewed GED per SPY",
    x = "Data",
    y = "Residui standardizzati"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    axis.title = element_text(size = 11)
  )

# KPSS Test
kpss_test_level(df_residui, col = "Residui", alpha = 0.01)
# 
# [Residui] KPSS Test - Stazionarietà attorno a una media costante
# Statistic: 0.127408
# Valore critico (circa 1%): 0.739000
# Interpretazione del p-value: p-value > 0.10
# 
# ** CONCLUSIONE: Statistic <= valore critico (1%) **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO stazionari.

# Quindi, i residui standardizzati sono stazionari.

# Per l'omoschedasticità non condizionata, per prima cosa, vediamo se c'è curtosi nei residui standardizzati.

# Calcola la curtosi
kurt_z_hat <- kurtosis(as.numeric(z_hat), na.rm = TRUE)

cat(sprintf("Curtosi dei residui standardizzati: %.4f\n", kurt_z_hat))
# Curtosi dei residui standardizzati: 5.1879

# La curtosi c'è. Quindi, applichiamo la forma "studentized" dei test di Breusch-Pagan e White per la stazionarietà in varianza.

# Breusch-Pagan Test:
# Crea un indice temporale (può essere anche un numero progressivo)
df_residui$Index <- seq_along(df_residui$Residui)

# Esegui il test BP
breusch_pagan_test(
  index = df_residui$Index,
  group = df_residui$Residui,
  col = "Residui standardizzati"
)
# [Residui standardizzati] Breusch-Pagan Test
# Statistic: 3.547527
# p-value: 0.0596342
# 
# ** Conclusione: p-value > 0.01 **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON sono eteroschedastici.

# White Test
white_test(
  index = df_residui$Index,
  group = df_residui$Residui,
  col = "Residui standardizzati"
)
# [Residui standardizzati] White Test
# Statistic: 4.325986
# p-value: 0.11498
# 
# ** Conclusione: p-value > 0.01 **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON sono eteroschedastici.

# Dai risultati dei test si vede che è verificata anche la condizione di omoschedasticità (non condizionata).

# Per verificare ulteriormente l'assenza di autocorrelazione per i residui standardizzati
# di questo modello, facciamo anche l'ACF, il PACF e il test di Ljung-Box classico.

# ACF
df_z <- data.frame(z_hat = as.numeric(z_hat))
plot_acf_residuals(df_z, col = "z_hat")

# PACF
plot_pacf_residuals(df_z, col = "z_hat")

# L’analisi visiva di ACF e PACF mostra che, in entrambi i grafici, nessuna barra supera la banda di confidenza al 90%.
# Dunque, non c'è una forte evidenza visiva di autocorrelazione.

# Ljung-Box Test
ljungbox_test(as.numeric(z_hat), col_name = "Residui standardizzati")
# Ljung-Box test per Residui standardizzati (max_lag = 10):
#            lag    lb_stat lb_pvalue
# X-squared    1 0.07871866 0.7790414
# X-squared1   2 0.17531404 0.9160750
# X-squared2   3 0.87087404 0.8324503
# X-squared3   4 0.89830913 0.9248033
# X-squared4   5 1.00273746 0.9623447
# X-squared5   6 1.69959742 0.9451523
# X-squared6   7 2.29960107 0.9414170
# X-squared7   8 2.30404156 0.9702435
# X-squared8   9 2.85694235 0.9696975
# X-squared9  10 2.86207235 0.9844861

# Osservando i p-value associati ai vari lag, vediamo che tutti questi sono maggiori di α = 0.10. Dunque, non abbiamo evidenza statistica
# sufficiente per rifiutare l’ipotesi nulla di assenza di autocorrelazione fino al lag specificato.

# Infine, verifichiamo anche l’assenza di eteroschedasticità condizionata nei residui standardizzati.

# Test ARCH di Engle
arch_effects_test(z_hat, col_name = "Residui Standardizzati")
# ARCH Test per Residui Standardizzati (max_lag = 5):
#            lag arch_stat arch_pvalue
# statistic    1  5.345330  0.02077785
# statistic1   2  7.070649  0.02914930
# statistic2   3  8.301631  0.04017237
# statistic3   4 10.334174  0.03515950
# statistic4   5 10.347799  0.06595938

# Alla luce dei risultati, non possiamo rigettare l'ipotesi nulla di assenza di effetti ARCH con un livello di
# significatività dell'1% per tutti i lag. Perciò, possiamo concludere che i residui standardizzati sono privi di eteroschedasticità
# condizionata.

# In conclusione, alla luce di tutti i risultati ottenuti dall’analisi dei residui standardizzati, possiamo concludere che
# il modello GARCH(4,1) con distribuzione skewed GED ha catturato correttamente la dinamica della volatilità.

best_valid_p <- valid_models$p[31]
best_valid_q <- valid_models$q[31]
best_valid_dist <- valid_models$dist[31]

cat(sprintf("Modello valido migliore (secondo il criterio AICc + BIC): GARCH(%d,%d) con distribuzione %s\n", best_valid_p, best_valid_q, best_valid_dist))
# Modello valido migliore (secondo il criterio AICc + BIC): GARCH(4,1) con distribuzione sged
##################################################################################################################################################