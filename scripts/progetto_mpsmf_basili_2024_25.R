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
  
  wt <- lmtest::bptest(model, ~ fitted.values(model) + I(fitted.values(model)^2))
  
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
#  - assenza di autocorrelazione (verificata tramite test Ljung-Box),
#  - coerenza con la distribuzione ipotizzata (tramite test di normalità quali SW, JB e QQ-Plot e test di Kolmogorov-Smirnov),
#  - verifica di stazionarietà e omoschedasticità non condizionata (tramite grafico e test quali KPSS, Breusch-Pagan e White),
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
# Estrai i parametri stimati della distribuzione sstd dal fit corrente
params <- coef(fit)
skew  <- params["skew"]
shape <- params["shape"]

# Residui standardizzati
z_hat_sorted <- sort(as.numeric(z_hat))
n <- length(z_hat_sorted)

# Calcola i quantili teorici della skewed Student-t
probs <- ppoints(n)
theoretical_q <- qdist("sstd", p = probs, skew = skew, shape = shape)

# Calcola la regressione lineare per la linea ideale
fit_line <- lm(z_hat_sorted ~ theoretical_q)
intercept <- coef(fit_line)[1]
slope <- coef(fit_line)[2]

# Calcola z-score per banda di confidenza al 99%
confidence_level <- 0.99
alpha <- 1 - confidence_level
z_score <- qnorm(1 - alpha / 2)

# Approssimazione della deviazione standard dei residui
se <- sd(z_hat_sorted)
se_band <- se * z_score / sqrt(n)

# Bande superiori e inferiori
ci_upper <- slope * theoretical_q + intercept + se_band
ci_lower <- slope * theoretical_q + intercept - se_band

# Crea data frame per ggplot
df_qq <- data.frame(
  Theoretical = theoretical_q,
  Observed = z_hat_sorted,
  CI_upper = ci_upper,
  CI_lower = ci_lower
)

# Q-Q Plot
ggplot(df_qq, aes(x = Theoretical, y = Observed)) +
  geom_point(aes(color = "Dati osservati"), size = 1.5) +
  geom_abline(aes(slope = slope, intercept = intercept, linetype = "Linea ideale"), color = "red") +
  geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper, fill = "Banda di confidenza al 99%"), alpha = 0.3) +
  scale_color_manual(name = "", values = c("Dati osservati" = "black")) +
  scale_linetype_manual(name = "", values = c("Linea ideale" = "dashed")) +
  scale_fill_manual(name = "", values = c("Banda di confidenza al 99%" = "aquamarine")) +
  labs(
    title = "Q-Q Plot skewed Student-t dei residui standardizzati con bande di confidenza",
    subtitle = "Modello GARCH(1,1) con distribuzione skewed Student-t per SPY",
    x = "Quantili teorici (skewed Student-t)",
    y = "Residui standardizzati"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.direction = "vertical",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# Verifica quali residui stanno dentro le bande di confidenza
df_qq$inside_band <- with(df_qq, Observed >= CI_lower & Observed <= CI_upper)

# Conta quanti punti rientrano nelle bande
n_inside <- sum(df_qq$inside_band)
n_total <- nrow(df_qq)
percentage_inside <- round(n_inside / n_total * 100, 2)

# Output
cat("Numero di punti all'interno delle bande di confidenza al 99%:", n_inside, "su", n_total, "\n")
# Numero di punti all'interno delle bande di confidenza al 99%: 454 su 474
cat("Percentuale di punti dentro le bande:", percentage_inside, "%\n")
# Percentuale di punti dentro le bande: 95.78 %

# Kolmogorov-Smirnov test (K-S test)
stats::ks.test(x=z_hat, y="psstd", mean=0, sd=1, nu=shape, xi=skew, alternative= "two.sided")
# 
#         Asymptotic one-sample Kolmogorov-Smirnov test
# 
# data:  z_hat
# D = 0.92304, p-value < 2.2e-16
# alternative hypothesis: two-sided

# Dall'analisi del Q-Q Plot e del test di K-S vediamo, quindi, che i residui standardizzati non seguono la distribuzione skewed Student-t.
# Perciò, questo modello è da scartare.

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
# Elapsed time : 0.3166351

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

# Esegue il test SW
shapirowilk_test(as.numeric(z_hat), name = "Residui standardizzati")
# 
# [Residui standardizzati] Shapiro-Wilk Test
# Statistic: 0.964475
# p-value  : 2.71107e-09
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Esegue il test JB
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
# Estrai i parametri stimati della distribuzione SGED dal fit corrente
params <- coef(fit)
skew  <- params["skew"]
shape <- params["shape"]

# Residui standardizzati
z_hat_sorted <- sort(as.numeric(z_hat))
n <- length(z_hat_sorted)

# Calcola i quantili teorici della skewed GED (SGED)
probs <- ppoints(n)
theoretical_q <- qdist("sged", p = probs, skew = skew, shape = shape)

# Calcola la regressione lineare per la linea ideale
fit_line <- lm(z_hat_sorted ~ theoretical_q)
intercept <- coef(fit_line)[1]
slope <- coef(fit_line)[2]

# Calcola z-score per banda di confidenza al 99%
confidence_level <- 0.99
alpha <- 1 - confidence_level
z_score <- qnorm(1 - alpha / 2)

# Approssimazione della deviazione standard dei residui
se <- sd(z_hat_sorted)
se_band <- se * z_score / sqrt(n)

# Bande superiori e inferiori
ci_upper <- slope * theoretical_q + intercept + se_band
ci_lower <- slope * theoretical_q + intercept - se_band

# Crea data frame per ggplot
df_qq <- data.frame(
  Theoretical = theoretical_q,
  Observed = z_hat_sorted,
  CI_upper = ci_upper,
  CI_lower = ci_lower
)

# Q-Q Plot
ggplot(df_qq, aes(x = Theoretical, y = Observed)) +
  geom_point(aes(color = "Dati osservati"), size = 1.5) +
  geom_abline(aes(slope = slope, intercept = intercept, linetype = "Linea ideale"), color = "red") +
  geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper, fill = "Banda di confidenza al 99%"), alpha = 0.3) +
  scale_color_manual(name = "", values = c("Dati osservati" = "black")) +
  scale_linetype_manual(name = "", values = c("Linea ideale" = "dashed")) +
  scale_fill_manual(name = "", values = c("Banda di confidenza al 99%" = "aquamarine")) +
  labs(
    title = "Q-Q Plot skewed GED dei residui standardizzati con bande di confidenza",
    subtitle = "Modello GARCH(1,1) con distribuzione skewed GED per SPY",
    x = "Quantili teorici (skewed GED)",
    y = "Residui standardizzati"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.direction = "vertical",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# Verifica quali residui stanno dentro le bande di confidenza
df_qq$inside_band <- with(df_qq, Observed >= CI_lower & Observed <= CI_upper)

# Conta quanti punti rientrano nelle bande
n_inside <- sum(df_qq$inside_band)
n_total <- nrow(df_qq)
percentage_inside <- round(n_inside / n_total * 100, 2)

# Output
cat("Numero di punti all'interno delle bande di confidenza al 99%:", n_inside, "su", n_total, "\n")
# Numero di punti all'interno delle bande di confidenza al 99%: 447 su 474
cat("Percentuale di punti dentro le bande:", percentage_inside, "%\n")
# Percentuale di punti dentro le bande: 94.3 %

# Kolmogorov-Smirnov test (K-S test)
stats::ks.test(x=z_hat, y="psged", mean=0, sd=1, nu=shape, xi=skew, alternative= "two.sided")
# 
#         Asymptotic one-sample Kolmogorov-Smirnov test
# 
# data:  z_hat
# D = 0.921, p-value < 2.2e-16
# alternative hypothesis: two-sided

# Dall'analisi del Q-Q Plot e del test di K-S vediamo, quindi, che i residui standardizzati non seguono la distribuzione skewed GED.
# Perciò, anche questo modello è da scartare.

# Analizziamo il prossimo nella classifica: GARCH(1,0) sstd




















# # Analizziamo il prossimo nella classifica: GARCH(0,1) norm
# p <- valid_models$p[2]
# q <- valid_models$q[2]
# dist <- valid_models$dist[2]
# 
# spec <- ugarchspec(
#   variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
#   mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
#   distribution.model = dist
# )
# 
# fit <- tryCatch(
#   ugarchfit(spec = spec, data = returns, solver = "hybrid"),
#   error = function(e) NULL
# )
# 
# # Risultati
# show(fit)
# #
# # *---------------------------------*
# # *          GARCH Model Fit        *
# # *---------------------------------*
# #
# # Conditional Variance Dynamics
# # -----------------------------------
# # GARCH Model 	  : sGARCH(0,1)
# # Mean Model	    : ARFIMA(0,0,0)
# # Distribution	  : norm
# #
# # Optimal Parameters
# # ------------------------------------
# #        Estimate  Std. Error   t value Pr(>|t|)
# # omega  0.000798    0.000092    8.7141        0
# # beta1  0.999000    0.000142 7032.3749        0
# #
# # Robust Standard Errors:
# #        Estimate  Std. Error   t value Pr(>|t|)
# # omega  0.000798    0.000099    8.0559        0
# # beta1  0.999000    0.000148 6772.8475        0
# #
# # LogLikelihood : -475.0635
# #
# # Information Criteria
# # ------------------------------------
# #
# # Akaike       2.3913
# # Bayes        2.4113
# # Shibata      2.3912
# # Hannan-Quinn 2.3992
# #
# # Weighted Ljung-Box Test on Standardized Residuals
# # ------------------------------------
# #                         statistic p-value
# # Lag[1]                     0.7878  0.3748
# # Lag[2*(p+q)+(p+q)-1][2]    0.8376  0.5540
# # Lag[4*(p+q)+(p+q)-1][5]    1.3082  0.7869
# # d.o.f=0
# # H0 : No serial correlation
# #
# # Weighted Ljung-Box Test on Standardized Squared Residuals
# # ------------------------------------
# #                         statistic p-value
# # Lag[1]                     0.6610 0.41619
# # Lag[2*(p+q)+(p+q)-1][2]    0.9552 0.51301
# # Lag[4*(p+q)+(p+q)-1][5]    5.8924 0.09534
# # d.o.f=1
# #
# # Weighted ARCH LM Tests
# # ------------------------------------
# #             Statistic Shape Scale P-Value
# # ARCH Lag[2]    0.5824 0.500 2.000 0.44538
# # ARCH Lag[4]    6.1721 1.397 1.611 0.04544
# # ARCH Lag[6]    9.0003 2.222 1.500 0.02403
# #
# # Nyblom stability test
# # ------------------------------------
# # Joint Statistic:  1.1562
# # Individual Statistics:
# # omega 0.1430
# # beta1 0.1384
# #
# # Asymptotic Critical Values (10% 5% 1%)
# # Joint Statistic:     	   0.61 0.749 1.07
# # Individual Statistic:	   0.35 0.47 0.75
# #
# # Sign Bias Test
# # ------------------------------------
# #                    t-value    prob sig
# # Sign Bias           1.5725 0.11663
# # Negative Sign Bias  0.7203 0.47180
# # Positive Sign Bias  0.4206 0.67424
# # Joint Effect        7.7482 0.05151   *
# #
# #
# # Adjusted Pearson Goodness-of-Fit Test:
# # ------------------------------------
# #   group statistic p-value(g-1)
# # 1    20     44.66    0.0007648
# # 2    30     62.28    0.0003189
# # 3    40     71.48    0.0011586
# # 4    50     88.34    0.0004864
# #
# #
# # Elapsed time : 0.06993294
# 
# # Analizzando i p-value del test pesato di Ljung-Box sui residui standardizzati, osserviamo che questi sono tutti maggiori di α = 0.01
# # (in realtà anche di α = 0.05 e α = 0.10). Quindi, non abbiamo evidenza statistica sufficiente per rifiutare l’ipotesi nulla di assenza di
# # autocorrelazione.
# 
# # Applichiamo, ora, i dest di normalità sui residui.
# 
# # Estrai residui standardizzati
# z_hat <- residuals(fit, standardize = TRUE)
# 
# # Prendi le date del training_set:
# # Rimuoviamo le righe con NA nella colonna usata per il modello
# valid_dates <- training_set$Date[!is.na(training_set$SPY_LogReturn)]
# 
# # Converti in Date se non lo sono già
# valid_dates <- as.Date(valid_dates)
# 
# # Sovrascrivi l’index di z_hat con le date corrette
# index(z_hat) <- valid_dates
# 
# # Controlla che le lunghezze coincidano
# if(length(z_hat) != length(valid_dates)) {
#   stop("Errore: lunghezza residui e date non coincide!")
# }
# 
# # Visualizza i primi valori
# head(z_hat)
# #                  [,1]
# # 2023-08-01 -0.3598271
# # 2023-08-02 -1.7586932
# # 2023-08-03 -0.3602748
# # 2023-08-04 -0.5689350
# # 2023-08-07  1.0906310
# # 2023-08-08 -0.5468688
# 
# # Esegue il test SW
# shapirowilk_test(as.numeric(z_hat), name = "Residui standardizzati")
# #
# # [Residui standardizzati] Shapiro-Wilk Test
# # Statistic: 0.983377
# # p-value  : 0.00015127
# #
# # ** CONCLUSIONE: p-value <= 0.01 **
# # Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# # → I dati NON seguono una distribuzione normale.
# 
# # Esegue il test JB
# jarque_bera_test(as.numeric(z_hat), name = "Residui standardizzati")
# #
# # [Residui standardizzati] Jarque-Bera Test
# # Statistic: 26.449214
# # p-value  : 1.80562e-06
# #
# # ** CONCLUSIONE: p-value <= 0.01 **
# # Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# # → I dati NON seguono una distribuzione normale.
# 
# # Visto che i residui standardizzati del GARCH(0,1) con distribuzione normale non seguono una distribuzione normale,
# # questo modello è da scartare.
# 
# # Analizziamo il prossimo nella classifica: GARCH(0,1) sged
# p <- valid_models$p[3]
# q <- valid_models$q[3]
# dist <- valid_models$dist[3]
# 
# spec <- ugarchspec(
#   variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
#   mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
#   distribution.model = dist
# )
# 
# fit <- tryCatch(
#   ugarchfit(spec = spec, data = returns, solver = "hybrid"),
#   error = function(e) NULL
# )
# 
# # Risultati
# show(fit)
# #
# # *---------------------------------*
# # *          GARCH Model Fit        *
# # *---------------------------------*
# #
# # Conditional Variance Dynamics
# # -----------------------------------
# # GARCH Model	    : sGARCH(0,1)
# # Mean Model	    : ARFIMA(0,0,0)
# # Distribution	  : sged
# #
# # Optimal Parameters
# # ------------------------------------
# #        Estimate  Std. Error  t value Pr(>|t|)
# # omega  0.000795    0.000118    6.751        0
# # beta1  0.999000    0.000216 4616.366        0
# # skew   0.888516    0.044940   19.771        0
# # shape  1.367556    0.135935   10.060        0
# #
# # Robust Standard Errors:
# #        Estimate  Std. Error   t value Pr(>|t|)
# # omega  0.000795    0.000186    4.2728  1.9e-05
# # beta1  0.999000    0.000172 5808.6056  0.0e+00
# # skew   0.888516    0.036383   24.4209  0.0e+00
# # shape  1.367556    0.152439    8.9712  0.0e+00
# #
# # LogLikelihood : -465.8509
# #
# # Information Criteria
# # ------------------------------------
# #
# # Akaike       2.3551
# # Bayes        2.3951
# # Shibata      2.3549
# # Hannan-Quinn 2.3710
# #
# # Weighted Ljung-Box Test on Standardized Residuals
# # ------------------------------------
# #                         statistic p-value
# # Lag[1]                     0.7873  0.3749
# # Lag[2*(p+q)+(p+q)-1][2]    0.8372  0.5542
# # Lag[4*(p+q)+(p+q)-1][5]    1.3081  0.7869
# # d.o.f=0
# # H0 : No serial correlation
# #
# # Weighted Ljung-Box Test on Standardized Squared Residuals
# # ------------------------------------
# #                         statistic p-value
# # Lag[1]                     0.6621 0.41584
# # Lag[2*(p+q)+(p+q)-1][2]    0.9568 0.51247
# # Lag[4*(p+q)+(p+q)-1][5]    5.8962 0.09515
# # d.o.f=1
# #
# # Weighted ARCH LM Tests
# # ------------------------------------
# #             Statistic Shape Scale P-Value
# # ARCH Lag[2]    0.5835 0.500 2.000 0.44493
# # ARCH Lag[4]    6.1753 1.397 1.611 0.04536
# # ARCH Lag[6]    9.0042 2.222 1.500 0.02398
# #
# # Nyblom stability test
# # ------------------------------------
# # Joint Statistic:  1.2203
# # Individual Statistics:
# # omega 0.1166
# # beta1 0.1138
# # skew  0.1638
# # shape 0.2466
# #
# # Asymptotic Critical Values (10% 5% 1%)
# # Joint Statistic:     	   1.07 1.24 1.6
# # Individual Statistic:	   0.35 0.47 0.75
# #
# # Sign Bias Test
# # ------------------------------------
# #                    t-value    prob sig
# # Sign Bias           1.5722 0.11671
# # Negative Sign Bias  0.7206 0.47156
# # Positive Sign Bias  0.4206 0.67427
# # Joint Effect        7.7476 0.05153   *
# #
# #
# # Adjusted Pearson Goodness-of-Fit Test:
# # ------------------------------------
# #   group statistic p-value(g-1)
# # 1    20     32.73     0.025826
# # 2    30     46.64     0.020251
# # 3    40     67.87     0.002833
# # 4    50     75.56     0.008768
# #
# #
# # Elapsed time : 0.250386
# 
# # Analizzando i p-value del test pesato di Ljung-Box sui residui standardizzati, osserviamo che questi sono tutti maggiori di α = 0.01
# # (in realtà anche di α = 0.05 e α = 0.10). Quindi, non abbiamo evidenza statistica sufficiente per rifiutare l’ipotesi nulla di assenza di
# # autocorrelazione.
# 
# # Applichiamo, ora, i dest di normalità sui residui.
# 
# # Estrai residui standardizzati
# z_hat <- residuals(fit, standardize = TRUE)
# 
# # Prendi le date del training_set:
# # Rimuoviamo le righe con NA nella colonna usata per il modello
# valid_dates <- training_set$Date[!is.na(training_set$SPY_LogReturn)]
# 
# # Converti in Date se non lo sono già
# valid_dates <- as.Date(valid_dates)
# 
# # Sovrascrivi l’index di z_hat con le date corrette
# index(z_hat) <- valid_dates
# 
# # Controlla che le lunghezze coincidano
# if(length(z_hat) != length(valid_dates)) {
#   stop("Errore: lunghezza residui e date non coincide!")
# }
# 
# # Visualizza i primi valori
# head(z_hat)
# #                  [,1]
# # 2023-08-01 -0.3598271
# # 2023-08-02 -1.7586971
# # 2023-08-03 -0.3602764
# # 2023-08-04 -0.5689387
# # 2023-08-07  1.0906406
# # 2023-08-08 -0.5468748
# 
# # Esegue il test SW
# shapirowilk_test(as.numeric(z_hat), name = "Residui standardizzati")
# #
# # [Residui standardizzati] Shapiro-Wilk Test
# # Statistic: 0.983372
# # p-value  : 0.000150817
# #
# # ** CONCLUSIONE: p-value <= 0.01 **
# # Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# # → I dati NON seguono una distribuzione normale.
# 
# # Esegue il test JB
# jarque_bera_test(as.numeric(z_hat), name = "Residui standardizzati")
# #
# # [Residui standardizzati] Jarque-Bera Test
# # Statistic: 26.482109
# # p-value  : 1.77616e-06
# #
# # ** CONCLUSIONE: p-value <= 0.01 **
# #   Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# # → I dati NON seguono una distribuzione normale.
# 
# # Come atteso, i residui standardizzati del GARCH(0,1) con distribuzione skewed GED non seguono una distribuzione normale.
# 
# # QQ-Plot SGED:
# # Estrai i parametri SGED stimati
# pars <- coef(fit)
# 
# # Individua i nomi rilevanti
# shape <- pars["shape"]
# skew  <- pars["skew"]
# 
# # Calcola i quantili teorici SGED
# n <- length(z_hat)
# sged_quantiles <- qsged(ppoints(n), mean = 0, sd = 1, nu = shape, xi = skew)
# 
# # Grafico
# qqplot(sged_quantiles, sort(z_hat),
#        main = "Q-Q Plot SGED dei residui standardizzati del modello GARCH(0,1) con distribuzione skewed GED per SPY",
#        xlab = "Quantili teorici SGED",
#        ylab = "Quantili empirici residui")
# abline(0, 1, col = "blue", lwd = 2)
# 
# # Dal grafico vediamo che i punti risultano molto sparsi, disordinati e non seguono minimamente la retta di riferimento: questo è un segnale
# # chiaro che i residui standardizzati NON seguono affatto la distribuzione SGED. Perciò, anche questo modello è da scartare.
# 
# # Analizziamo il prossimo nella classifica: GARCH(1,0) sstd
# p <- valid_models$p[4]
# q <- valid_models$q[4]
# dist <- valid_models$dist[4]
# 
# spec <- ugarchspec(
#   variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
#   mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
#   distribution.model = dist
# )
# 
# fit <- tryCatch(
#   ugarchfit(spec = spec, data = returns, solver = "hybrid"),
#   error = function(e) NULL
# )
# 
# # Risultati
# show(fit)
# #
# # *---------------------------------*
# # *          GARCH Model Fit        *
# # *---------------------------------*
# #
# # Conditional Variance Dynamics
# # -----------------------------------
# # GARCH Model	    : sGARCH(1,0)
# # Mean Model	    : ARFIMA(0,0,0)
# # Distribution	  : sstd
# #
# # Optimal Parameters
# # ------------------------------------
# #         Estimate  Std. Error  t value Pr(>|t|)
# # omega    0.64256    0.076113  8.44219 0.000000
# # alpha1   0.03439    0.065740  0.52312 0.600892
# # skew     0.84630    0.057393 14.74560 0.000000
# # shape    7.50665    3.105219  2.41743 0.015631
# #
# # Robust Standard Errors:
# #         Estimate  Std. Error  t value Pr(>|t|)
# # omega    0.64256    0.080825   7.9500 0.000000
# # alpha1   0.03439    0.081706   0.4209 0.673831
# # skew     0.84630    0.047938  17.6539 0.000000
# # shape    7.50665    3.024080   2.4823 0.013054
# #
# # LogLikelihood : -467.381
# #
# # Information Criteria
# # ------------------------------------
# #
# # Akaike       2.3628
# # Bayes        2.4028
# # Shibata      2.3626
# # Hannan-Quinn 2.3786
# #
# # Weighted Ljung-Box Test on Standardized Residuals
# # ------------------------------------
# #                         statistic p-value
# # Lag[1]                     0.6451  0.4219
# # Lag[2*(p+q)+(p+q)-1][2]    0.6763  0.6163
# # Lag[4*(p+q)+(p+q)-1][5]    1.1483  0.8253
# # d.o.f=0
# # H0 : No serial correlation
# #
# # Weighted Ljung-Box Test on Standardized Squared Residuals
# # ------------------------------------
# #                         statistic p-value
# # Lag[1]                    0.02651  0.8707
# # Lag[2*(p+q)+(p+q)-1][2]   0.22872  0.8368
# # Lag[4*(p+q)+(p+q)-1][5]   4.53198  0.1948
# # d.o.f=1
# #
# # Weighted ARCH LM Tests
# # ------------------------------------
# #             Statistic Shape Scale P-Value
# # ARCH Lag[2]    0.4004 0.500 2.000 0.52690
# # ARCH Lag[4]    5.2428 1.397 1.611 0.07682
# # ARCH Lag[6]    7.9466 2.222 1.500 0.04266
# #
# # Nyblom stability test
# # ------------------------------------
# # Joint Statistic:  0.7367
# # Individual Statistics:
# # omega  0.09929
# # alpha1 0.07004
# # skew   0.05973
# # shape  0.17895
# #
# # Asymptotic Critical Values (10% 5% 1%)
# # Joint Statistic:     	   1.07 1.24 1.6
# # Individual Statistic:	   0.35 0.47 0.75
# #
# # Sign Bias Test
# # ------------------------------------
# #                    t-value    prob sig
# # Sign Bias           1.6948 0.09089   *
# # Negative Sign Bias  0.1887 0.85040
# # Positive Sign Bias  0.1550 0.87691
# # Joint Effect        6.9133 0.07471   *
# #
# #
# # Adjusted Pearson Goodness-of-Fit Test:
# # ------------------------------------
# #   group statistic p-value(g-1)
# # 1    20     33.93      0.01872
# # 2    30     42.58      0.04977
# # 3    40     57.04      0.03106
# # 4    50     62.28      0.09641
# #
# #
# # Elapsed time : 0.2489622
# 
# # Analizzando i p-value del test pesato di Ljung-Box sui residui standardizzati, osserviamo che questi sono tutti maggiori di α = 0.01
# # (in realtà anche di α = 0.05 e α = 0.10). Quindi, non abbiamo evidenza statistica sufficiente per rifiutare l’ipotesi nulla di assenza di
# # autocorrelazione.
# 
# # Applichiamo, ora, i dest di normalità sui residui.
# 
# # Estrai residui standardizzati
# z_hat <- residuals(fit, standardize = TRUE)
# 
# # Prendi le date del training_set:
# # Rimuoviamo le righe con NA nella colonna usata per il modello
# valid_dates <- training_set$Date[!is.na(training_set$SPY_LogReturn)]
# 
# # Converti in Date se non lo sono già
# valid_dates <- as.Date(valid_dates)
# 
# # Sovrascrivi l’index di z_hat con le date corrette
# index(z_hat) <- valid_dates
# 
# # Controlla che le lunghezze coincidano
# if(length(z_hat) != length(valid_dates)) {
#   stop("Errore: lunghezza residui e date non coincide!")
# }
# 
# # Visualizza i primi valori
# head(z_hat)
# #                  [,1]
# # 2023-08-01 -0.3598271
# # 2023-08-02 -1.7437296
# # 2023-08-03 -0.3406002
# # 2023-08-04 -0.5642355
# # 2023-08-07  1.0782306
# # 2023-08-08 -0.5330193
# 
# # Esegue il test SW
# shapirowilk_test(as.numeric(z_hat), name = "Residui standardizzati")
# #
# # [Residui standardizzati] Shapiro-Wilk Test
# # Statistic: 0.984016
# # p-value  : 0.000215894
# #
# # ** CONCLUSIONE: p-value <= 0.01 **
# # Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# # → I dati NON seguono una distribuzione normale.
# 
# # Esegue il test JB
# jarque_bera_test(as.numeric(z_hat), name = "Residui standardizzati")
# #
# # [Residui standardizzati] Jarque-Bera Test
# # Statistic: 25.384330
# # p-value  : 3.07512e-06
# #
# # ** CONCLUSIONE: p-value <= 0.01 **
# # Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# # → I dati NON seguono una distribuzione normale.
# 
# # Come atteso, i residui standardizzati del GARCH(1,0) con distribuzione skewed Student-t non seguono una distribuzione normale.
# 
# # QQ-Plot SSTD:
# # Estrai i parametri stimati della distribuzione sstd dal fit corrente
# params <- coef(fit)
# skew  <- params["skew"]
# shape <- params["shape"]
# 
# # Residui standardizzati
# z_hat_sorted <- sort(as.numeric(z_hat))
# n <- length(z_hat_sorted)
# 
# # Calcola i quantili teorici della skewed Student-t
# probs <- ppoints(n)
# theoretical_q <- qdist("sstd", p = probs, skew = skew, shape = shape)
# 
# # Calcola la regressione lineare per la linea ideale
# fit_line <- lm(z_hat_sorted ~ theoretical_q)
# intercept <- coef(fit_line)[1]
# slope <- coef(fit_line)[2]
# 
# # Calcola z-score per banda di confidenza al 99%
# confidence_level <- 0.99
# alpha <- 1 - confidence_level
# z_score <- qnorm(1 - alpha / 2)
# 
# # Approssimazione della deviazione standard dei residui
# se <- sd(z_hat_sorted)
# se_band <- se * z_score / sqrt(n)
# 
# # Bande superiori e inferiori
# ci_upper <- slope * theoretical_q + intercept + se_band
# ci_lower <- slope * theoretical_q + intercept - se_band
# 
# # Crea data frame per ggplot
# df_qq <- data.frame(
#   Theoretical = theoretical_q,
#   Observed = z_hat_sorted,
#   CI_upper = ci_upper,
#   CI_lower = ci_lower
# )
# 
# # Q-Q Plot
# ggplot(df_qq, aes(x = Theoretical, y = Observed)) +
#   geom_point(aes(color = "Dati osservati"), size = 1.5) +
#   geom_abline(aes(slope = slope, intercept = intercept, linetype = "Linea ideale"), color = "red") +
#   geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper, fill = "Banda di confidenza al 99%"), alpha = 0.3) +
#   scale_color_manual(name = "", values = c("Dati osservati" = "black")) +
#   scale_linetype_manual(name = "", values = c("Linea ideale" = "dashed")) +
#   scale_fill_manual(name = "", values = c("Banda di confidenza al 99%" = "aquamarine")) +
#   labs(
#     title = "Q-Q Plot skewed Student-t dei residui standardizzati con bande di confidenza",
#     subtitle = "Modello GARCH(1,0) con distribuzione skewed Student-t per SPY",
#     x = "Quantili teorici (skewed Student-t)",
#     y = "Residui standardizzati"
#   ) +
#   theme_minimal() +
#   theme(
#     legend.position = "right",
#     legend.direction = "vertical",
#     plot.title = element_text(hjust = 0.5),
#     plot.subtitle = element_text(hjust = 0.5)
#   )
# 
# # Verifica quali residui stanno dentro le bande di confidenza
# df_qq$inside_band <- with(df_qq, Observed >= CI_lower & Observed <= CI_upper)
# 
# # Conta quanti punti rientrano nelle bande
# n_inside <- sum(df_qq$inside_band)
# n_total <- nrow(df_qq)
# percentage_inside <- round(n_inside / n_total * 100, 2)
# 
# # Output
# cat("Numero di punti all'interno delle bande di confidenza al 99%:", n_inside, "su", n_total, "\n")
# # Numero di punti all'interno delle bande di confidenza al 99%: 378 su 399
# cat("Percentuale di punti dentro le bande:", percentage_inside, "%\n")
# # Percentuale di punti dentro le bande: 94.74 %
# 
# # Asymptotic one-sample Kolmogorov-Smirnov test
# stats::ks.test(x=z_hat, y="psstd", mean=0, sd=1, nu=shape, xi=skew, alternative= "two.sided")
# #
# #         Asymptotic one-sample Kolmogorov-Smirnov test
# #
# # data:  z_hat
# # D = 0.96918, p-value < 2.2e-16
# # alternative hypothesis: two-sided
# 
# # Dal Q-Q Plot vediamo, quindi, che i residui standardizzati seguono la distribuzione skewed Student-t.
# 
# 
# 
# 
# 
# # Analizziamo il prossimo nella classifica: GARCH(1,1) sged
# p <- valid_models$p[9]
# q <- valid_models$q[9]
# dist <- valid_models$dist[9]
# 
# spec <- ugarchspec(
#   variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
#   mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
#   distribution.model = dist
# )
# 
# fit <- tryCatch(
#   ugarchfit(spec = spec, data = returns, solver = "hybrid"),
#   error = function(e) NULL
# )
# 
# # Risultati
# show(fit)
# #
# # *---------------------------------*
# # *          GARCH Model Fit        *
# # *---------------------------------*
# #
# # Conditional Variance Dynamics
# # -----------------------------------
# # GARCH Model	    : sGARCH(1,1)
# # Mean Model	    : ARFIMA(0,0,0)
# # Distribution	  : sged
# #
# # Optimal Parameters
# # ------------------------------------
# #         Estimate  Std. Error  t value Pr(>|t|)
# # omega   0.070622    0.033960   2.0796 0.037563
# # alpha1  0.071459    0.034632   2.0634 0.039077
# # beta1   0.822016    0.066000  12.4548 0.000000
# # skew    0.856352    0.051399  16.6607 0.000000
# # shape   1.466952    0.164495   8.9179 0.000000
# #
# # Robust Standard Errors:
# #         Estimate  Std. Error  t value Pr(>|t|)
# # omega   0.070622    0.018127   3.8960 0.000098
# # alpha1  0.071459    0.031838   2.2445 0.024802
# # beta1   0.822016    0.030820  26.6716 0.000000
# # skew    0.856352    0.052738  16.2378 0.000000
# # shape   1.466952    0.199269   7.3617 0.000000
# #
# # LogLikelihood : -462.1746
# #
# # Information Criteria
# # ------------------------------------
# #
# # Akaike       2.3417
# # Bayes        2.3917
# # Shibata      2.3414
# # Hannan-Quinn 2.3615
# #
# # Weighted Ljung-Box Test on Standardized Residuals
# # ------------------------------------
# #                         statistic p-value
# # Lag[1]                     0.2626  0.6083
# # Lag[2*(p+q)+(p+q)-1][2]    0.2809  0.8065
# # Lag[4*(p+q)+(p+q)-1][5]    0.7358  0.9159
# # d.o.f=0
# # H0 : No serial correlation
# #
# # Weighted Ljung-Box Test on Standardized Squared Residuals
# # ------------------------------------
# #                         statistic p-value
# # Lag[1]                     0.5784  0.4470
# # Lag[2*(p+q)+(p+q)-1][5]    1.2069  0.8113
# # Lag[4*(p+q)+(p+q)-1][9]    2.0113  0.9037
# # d.o.f=2
# #
# # Weighted ARCH LM Tests
# # ------------------------------------
# #             Statistic Shape Scale P-Value
# # ARCH Lag[3]   0.02788 0.500 2.000  0.8674
# # ARCH Lag[5]   0.45428 1.440 1.667  0.8970
# # ARCH Lag[7]   0.89512 2.315 1.543  0.9300
# #
# # Nyblom stability test
# # ------------------------------------
# # Joint Statistic:  0.9326
# # Individual Statistics:
# # omega  0.05294
# # alpha1 0.07384
# # beta1  0.04710
# # skew   0.24851
# # shape  0.28098
# #
# # Asymptotic Critical Values (10% 5% 1%)
# # Joint Statistic:     	   1.28 1.47 1.88
# # Individual Statistic:	   0.35 0.47 0.75
# #
# # Sign Bias Test
# # ------------------------------------
# #                    t-value    prob sig
# # Sign Bias           1.9846 0.04788  **
# # Negative Sign Bias  0.6899 0.49066
# # Positive Sign Bias  0.1336 0.89382
# # Joint Effect        7.0881 0.06914   *
# #
# #
# # Adjusted Pearson Goodness-of-Fit Test:
# # ------------------------------------
# #   group statistic p-value(g-1)
# # 1    20     39.85     0.003427
# # 2    30     33.86     0.244528
# # 3    40     66.26     0.004156
# # 4    50     68.79     0.032486
# #
# #
# # Elapsed time : 0.3208668
# 
# # Analizzando i p-value del test pesato di Ljung-Box sui residui standardizzati, osserviamo che questi sono tutti maggiori di α = 0.01
# # (in realtà anche di α = 0.05 e α = 0.10). Quindi, non abbiamo evidenza statistica sufficiente per rifiutare l’ipotesi nulla di assenza di
# # autocorrelazione.
# 
# # Applichiamo, ora, i dest di normalità sui residui.
# 
# # Estrai residui standardizzati
# z_hat <- residuals(fit, standardize = TRUE)
# 
# # Prendi le date del training_set:
# # Rimuoviamo le righe con NA nella colonna usata per il modello
# valid_dates <- training_set$Date[!is.na(training_set$SPY_LogReturn)]
# 
# # Converti in Date se non lo sono già
# valid_dates <- as.Date(valid_dates)
# 
# # Sovrascrivi l’index di z_hat con le date corrette
# index(z_hat) <- valid_dates
# 
# # Controlla che le lunghezze coincidano
# if(length(z_hat) != length(valid_dates)) {
#   stop("Errore: lunghezza residui e date non coincide!")
# }
# 
# # Visualizza i primi valori
# head(z_hat)
# #                  [,1]
# # 2023-08-01 -0.3598271
# # 2023-08-02 -1.8116734
# # 2023-08-03 -0.3424681
# # 2023-08-04 -0.5605840
# # 2023-08-07  1.1012406
# # 2023-08-08 -0.5462619
# 
# # Esegue il test SW
# shapirowilk_test(as.numeric(z_hat), name = "Residui standardizzati")
# #
# # [Residui standardizzati] Shapiro-Wilk Test
# # Statistic: 0.980928
# # p-value  : 4.07243e-05
# #
# # ** CONCLUSIONE: p-value <= 0.01 **
# # Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# # → I dati NON seguono una distribuzione normale.
# 
# # Esegue il test JB
# jarque_bera_test(as.numeric(z_hat), name = "Residui standardizzati")
# #
# # [Residui standardizzati] Jarque-Bera Test
# # Statistic: 37.951949
# # p-value  : 5.73904e-09
# #
# # ** CONCLUSIONE: p-value <= 0.01 **
# # Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# # → I dati NON seguono una distribuzione normale.
# 
# # Come atteso, i residui standardizzati del GARCH(1,1) con distribuzione skewed GED non seguono una distribuzione normale.
# 
# # QQ-Plot SGED:
# # Estrai i parametri SGED stimati
# pars <- coef(fit)
# 
# # Individua i nomi rilevanti
# shape <- pars["shape"]
# skew  <- pars["skew"]
# 
# # Calcola i quantili teorici SGED
# n <- length(z_hat)
# sged_quantiles <- qsged(ppoints(n), mean = 0, sd = 1, nu = shape, xi = skew)
# 
# # Grafico
# qqplot(sged_quantiles, sort(z_hat),
#        main = "Q-Q Plot SGED dei residui standardizzati del modello GARCH(1,1) con distribuzione skewed GED per SPY",
#        xlab = "Quantili teorici SGED",
#        ylab = "Quantili empirici residui")
# abline(0, 1, col = "blue", lwd = 2)
# 
# # Verifica quali residui stanno dentro le bande di confidenza
# df_qq$inside_band <- with(df_qq, Observed >= CI_lower & Observed <= CI_upper)
# 
# # Conta quanti punti rientrano nelle bande
# n_inside <- sum(df_qq$inside_band)
# n_total <- nrow(df_qq)
# percentage_inside <- round(n_inside / n_total * 100, 2)
# 
# # Output
# cat("Numero di punti all'interno delle bande di confidenza al 99%:", n_inside, "su", n_total, "\n")
# # Numero di punti all'interno delle bande di confidenza al 99%: 378 su 399
# cat("Percentuale di punti dentro le bande:", percentage_inside, "%\n")
# # Percentuale di punti dentro le bande: 94.74 %
# 
# # Asymptotic one-sample Kolmogorov-Smirnov test
# stats::ks.test(x=z_hat, y="psged", mean=0, sd=1, nu=shape, xi=skew, alternative= "two.sided")
# #
# #         Asymptotic one-sample Kolmogorov-Smirnov test
# #
# # data:  z_hat
# # D = 0.96574, p-value < 2.2e-16
# # alternative hypothesis: two-sided
# 
# # Analizziamo il prossimo nella classifica: GARCH(1,1) sstd
# p <- valid_models$p[10]
# q <- valid_models$q[10]
# dist <- valid_models$dist[10]
# 
# spec <- ugarchspec(
#   variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
#   mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
#   distribution.model = dist
# )
# 
# fit <- tryCatch(
#   ugarchfit(spec = spec, data = returns, solver = "hybrid"),
#   error = function(e) NULL
# )
# 
# # Risultati
# show(fit)
# #
# # *---------------------------------*
# # *          GARCH Model Fit        *
# # *---------------------------------*
# #
# # Conditional Variance Dynamics
# # -----------------------------------
# # GARCH Model	    : sGARCH(1,1)
# # Mean Model	    : ARFIMA(0,0,0)
# # Distribution	  : sstd
# #
# # Optimal Parameters
# # ------------------------------------
# #         Estimate  Std. Error  t value Pr(>|t|)
# # omega   0.063617    0.036698   1.7335 0.083001
# # alpha1  0.077741    0.036247   2.1448 0.031972
# # beta1   0.831371    0.069897  11.8942 0.000000
# # skew    0.814647    0.059038  13.7987 0.000000
# # shape   9.320806    4.447211   2.0959 0.036093
# #
# # Robust Standard Errors:
# #         Estimate  Std. Error  t value Pr(>|t|)
# # omega   0.063617    0.024030   2.6475 0.008110
# # alpha1  0.077741    0.032863   2.3657 0.017998
# # beta1   0.831371    0.041246  20.1566 0.000000
# # skew    0.814647    0.055010  14.8090 0.000000
# # shape   9.320806    4.513771   2.0650 0.038926
# #
# # LogLikelihood : -462.9908
# #
# # Information Criteria
# # ------------------------------------
# #
# # Akaike       2.3458
# # Bayes        2.3958
# # Shibata      2.3455
# # Hannan-Quinn 2.3656
# #
# # Weighted Ljung-Box Test on Standardized Residuals
# # ------------------------------------
# #                         statistic p-value
# # Lag[1]                     0.2449  0.6207
# # Lag[2*(p+q)+(p+q)-1][2]    0.2713  0.8119
# # Lag[4*(p+q)+(p+q)-1][5]    0.7398  0.9152
# # d.o.f=0
# # H0 : No serial correlation
# #
# # Weighted Ljung-Box Test on Standardized Squared Residuals
# # ------------------------------------
# #                         statistic p-value
# # Lag[1]                     0.6615  0.4160
# # Lag[2*(p+q)+(p+q)-1][5]    1.3489  0.7770
# # Lag[4*(p+q)+(p+q)-1][9]    2.0654  0.8971
# # d.o.f=2
# #
# # Weighted ARCH LM Tests
# # ------------------------------------
# #             Statistic Shape Scale P-Value
# # ARCH Lag[3]  0.001476 0.500 2.000  0.9694
# # ARCH Lag[5]  0.425038 1.440 1.667  0.9055
# # ARCH Lag[7]  0.790184 2.315 1.543  0.9451
# #
# # Nyblom stability test
# # ------------------------------------
# # Joint Statistic:  0.8852
# # Individual Statistics:
# # omega  0.03252
# # alpha1 0.04276
# # beta1  0.02983
# # skew   0.11576
# # shape  0.31301
# #
# # Asymptotic Critical Values (10% 5% 1%)
# # Joint Statistic:     	   1.28 1.47 1.88
# # Individual Statistic:	   0.35 0.47 0.75
# #
# # Sign Bias Test
# # ------------------------------------
# #                    t-value    prob sig
# # Sign Bias           2.0043 0.04573  **
# # Negative Sign Bias  0.7683 0.44275
# # Positive Sign Bias  0.1665 0.86786
# # Joint Effect        7.1703 0.06666   *
# #
# #
# # Adjusted Pearson Goodness-of-Fit Test:
# # ------------------------------------
# #   group statistic p-value(g-1)
# # 1    20     35.44     0.012370
# # 2    30     34.91     0.207518
# # 3    40     65.26     0.005257
# # 4    50     83.58     0.001513
# #
# #
# # Elapsed time : 0.3861911
# 
# # Analizzando i p-value del test pesato di Ljung-Box sui residui standardizzati, osserviamo che questi sono tutti maggiori di α = 0.01
# # (in realtà anche di α = 0.05 e α = 0.10). Quindi, non abbiamo evidenza statistica sufficiente per rifiutare l’ipotesi nulla di assenza di
# # autocorrelazione.
# 
# # Applichiamo, ora, i dest di normalità sui residui.
# 
# # Estrai residui standardizzati
# z_hat <- residuals(fit, standardize = TRUE)
# 
# # Prendi le date del training_set:
# # Rimuoviamo le righe con NA nella colonna usata per il modello
# valid_dates <- training_set$Date[!is.na(training_set$SPY_LogReturn)]
# 
# # Converti in Date se non lo sono già
# valid_dates <- as.Date(valid_dates)
# 
# # Sovrascrivi l’index di z_hat con le date corrette
# index(z_hat) <- valid_dates
# 
# # Controlla che le lunghezze coincidano
# if(length(z_hat) != length(valid_dates)) {
#   stop("Errore: lunghezza residui e date non coincide!")
# }
# 
# # Visualizza i primi valori
# head(z_hat)
# #                  [,1]
# # 2023-08-01 -0.3598271
# # 2023-08-02 -1.8125156
# # 2023-08-03 -0.3399474
# # 2023-08-04 -0.5568603
# # 2023-08-07  1.0944984
# # 2023-08-08 -0.5420359
# 
# # Esegue il test SW
# shapirowilk_test(as.numeric(z_hat), name = "Residui standardizzati")
# #
# # [Residui standardizzati] Shapiro-Wilk Test
# # Statistic: 0.980289
# # p-value  : 2.92863e-05
# #
# # ** CONCLUSIONE: p-value <= 0.01 **
# # Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# # → I dati NON seguono una distribuzione normale.
# 
# # Esegue il test JB
# jarque_bera_test(as.numeric(z_hat), name = "Residui standardizzati")
# #
# # [Residui standardizzati] Jarque-Bera Test
# # Statistic: 41.224097
# # p-value  : 1.11764e-09
# #
# # ** CONCLUSIONE: p-value <= 0.01 **
# # Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# # → I dati NON seguono una distribuzione normale.
# 
# # Come atteso, i residui standardizzati del GARCH(1,1) con distribuzione skewed Student-t non seguono una distribuzione normale.
# 
# # QQ-Plot SSTD:
# # Estrai i parametri stimati della distribuzione sstd dal fit corrente
# params <- coef(fit)
# skew  <- params["skew"]
# shape <- params["shape"]
# 
# # Residui standardizzati
# z_hat_sorted <- sort(as.numeric(z_hat))
# n <- length(z_hat_sorted)
# 
# # Calcola i quantili teorici della skewed Student-t
# probs <- ppoints(n)
# theoretical_q <- qdist("sstd", p = probs, skew = skew, shape = shape)
# 
# # Calcola la regressione lineare per la linea ideale
# fit_line <- lm(z_hat_sorted ~ theoretical_q)
# intercept <- coef(fit_line)[1]
# slope <- coef(fit_line)[2]
# 
# # Calcola z-score per banda di confidenza al 99%
# confidence_level <- 0.99
# alpha <- 1 - confidence_level
# z_score <- qnorm(1 - alpha / 2)
# 
# # Approssimazione della deviazione standard dei residui
# se <- sd(z_hat_sorted)
# se_band <- se * z_score / sqrt(n)
# 
# # Bande superiori e inferiori
# ci_upper <- slope * theoretical_q + intercept + se_band
# ci_lower <- slope * theoretical_q + intercept - se_band
# 
# # Crea data frame per ggplot
# df_qq <- data.frame(
#   Theoretical = theoretical_q,
#   Observed = z_hat_sorted,
#   CI_upper = ci_upper,
#   CI_lower = ci_lower
# )
# 
# # Q-Q Plot
# ggplot(df_qq, aes(x = Theoretical, y = Observed)) +
#   geom_point(aes(color = "Dati osservati"), size = 1.5) +
#   geom_abline(aes(slope = slope, intercept = intercept, linetype = "Linea ideale"), color = "red") +
#   geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper, fill = "Banda di confidenza al 99%"), alpha = 0.3) +
#   scale_color_manual(name = "", values = c("Dati osservati" = "black")) +
#   scale_linetype_manual(name = "", values = c("Linea ideale" = "dashed")) +
#   scale_fill_manual(name = "", values = c("Banda di confidenza al 99%" = "aquamarine")) +
#   labs(
#     title = "Q-Q Plot skewed Student-t dei residui standardizzati con bande di confidenza",
#     subtitle = "Modello GARCH(1,1) con distribuzione skewed Student-t per SPY",
#     x = "Quantili teorici (skewed Student-t)",
#     y = "Residui standardizzati"
#   ) +
#   theme_minimal() +
#   theme(
#     legend.position = "right",
#     legend.direction = "vertical",
#     plot.title = element_text(hjust = 0.5),
#     plot.subtitle = element_text(hjust = 0.5)
#   )
# 
# # Verifica quali residui stanno dentro le bande di confidenza
# df_qq$inside_band <- with(df_qq, Observed >= CI_lower & Observed <= CI_upper)
# 
# # Conta quanti punti rientrano nelle bande
# n_inside <- sum(df_qq$inside_band)
# n_total <- nrow(df_qq)
# percentage_inside <- round(n_inside / n_total * 100, 2)
# 
# # Output
# cat("Numero di punti all'interno delle bande di confidenza al 99%:", n_inside, "su", n_total, "\n")
# # Numero di punti all'interno delle bande di confidenza al 99%: 384 su 399
# cat("Percentuale di punti dentro le bande:", percentage_inside, "%\n")
# # Percentuale di punti dentro le bande: 96.24 %
# 
# # Asymptotic one-sample Kolmogorov-Smirnov test
# stats::ks.test(x=z_hat, y="psstd", mean=0, sd=1, nu=shape, xi=skew, alternative= "two.sided")
# #
# #         Asymptotic one-sample Kolmogorov-Smirnov test
# #
# # data:  z_hat
# # D = 0.96564, p-value < 2.2e-16
# # alternative hypothesis: two-sided
# 
# # RIFARE L'ANALISI
# 
# 
# 
# 
# 
# 
# 
# 
# # Verifichiamo ora la stazionarietà e l'omoschedasticità (non condizionata).
# 
# # Grafico dei residui
# df_residui <- data.frame(
#   Date = index(z_hat),
#   Residui = as.numeric(z_hat)
# )
# 
# ggplot(df_residui, aes(x = Date, y = Residui)) +
#   geom_line(color = "steelblue", linewidth = 0.6) +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
#   labs(
#     title = "Grafico dei residui standardizzati",
#     subtitle = "Modello GARCH(1,0) con distribuzione skewed Student-t per SPY",
#     x = "Data",
#     y = "Residui standardizzati"
#   ) +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
#     plot.subtitle = element_text(hjust = 0.5, size = 11),
#     axis.title = element_text(size = 11)
#   )
# 
# # KPSS Test
# kpss_test_level <- function(data, col, alpha = 0.01) {
#   x <- na.omit(data[[col]])
# 
#   # Valori critici per null = "Level"
#   critical_values <- c(`10%` = 0.347, `5%` = 0.463, `2.5%` = 0.574, `1%` = 0.739)
# 
#   # Funzione per trovare la soglia più vicina
#   get_critical <- function(cv, alpha) {
#     key <- names(cv)[which.min(abs(as.numeric(gsub("%", "", names(cv))) / 100 - alpha))]
#     value <- unname(cv[[key]])
#     return(c(value = as.numeric(value), level = key))
#   }
# 
#   # Funzione per stimare il p-value approssimato
#   estimate_pvalue <- function(stat, cv) {
#     if (stat > cv["1%"]) {
#       return("p-value < 0.01")
#     } else if (stat > cv["2.5%"]) {
#       return("p-value ≈ 0.025")
#     } else if (stat > cv["5%"]) {
#       return("p-value ≈ 0.05")
#     } else if (stat > cv["10%"]) {
#       return("p-value ≈ 0.10")
#     } else {
#       return("p-value > 0.10")
#     }
#   }
# 
#   cat(sprintf("\n[%s] KPSS Test - Stazionarietà attorno a una media costante\n", col))
# 
#   result <- kpss.test(x, null = "Level")
#   stat <- result$statistic
#   crit <- get_critical(critical_values, alpha)
# 
#   pvalue_msg <- estimate_pvalue(stat, critical_values)
# 
#   cat(sprintf("Statistic: %f\n", stat))
#   cat(sprintf("Valore critico (circa %s): %f\n", crit["level"], as.numeric(crit["value"])))
#   cat(sprintf("Interpretazione del p-value: %s\n", pvalue_msg))
# 
#   if (stat > crit["value"]) {
#     cat(sprintf("\n** CONCLUSIONE: Statistic > valore critico (%s) **\nPossiamo rigettare l'ipotesi nulla con un livello di significatività del %d%%\n→ I dati NON sono stazionari.\n\n", crit["level"], alpha * 100))
#   } else {
#     cat(sprintf("\n** CONCLUSIONE: Statistic <= valore critico (%s) **\nNon possiamo rigettare l'ipotesi nulla con un livello di significatività del %d%%\n→ I dati SONO stazionari.\n\n", crit["level"], alpha * 100))
#   }
# }
# 
# kpss_test_level(df_residui, col = "Residui", alpha = 0.01)
# #
# # [Residui] KPSS Test - Stazionarietà attorno a una media costante
# # Statistic: 0.103813
# # Valore critico (circa 1%): 0.739000
# # Interpretazione del p-value: p-value > 0.10
# #
# # ** CONCLUSIONE: Statistic <= valore critico (1%) **
# # Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# # → I dati SONO stazionari.
# 
# # Quindi, i residui standardizzati sono stazionari.
# 
# # Breusch-Pagan Test:
# # Calcola i residui al quadrato
# df_residui$Residui_Sq <- df_residui$Residui^2
# 
# # Crea un indice temporale (può essere anche un numero progressivo)
# df_residui$Index <- seq_along(df_residui$Residui)
# 
# # Esegui il test BP
# breusch_pagan_test(
#   index = df_residui$Index,
#   group = df_residui$Residui_Sq,
#   col = "Residui standardizzati"
# )
# # [Residui standardizzati] Breusch-Pagan Test
# # Statistic: 3.612974
# # p-value: 0.0573305
# #
# # ** Conclusione: p-value > 0.01 **
# # Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# # → I dati NON sono eteroschedastici.
# 
# # White Test
# white_test(
#   index = df_residui$Index,
#   group = df_residui$Residui_Sq,
#   col = "Residui standardizzati"
# )
# # [Residui standardizzati] White Test
# # Statistic: 3.613709
# # p-value: 0.16417
# #
# # ** Conclusione: p-value > 0.01 **
# # Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# # → I dati NON sono eteroschedastici.
# 
# # Dai risultati dei test si vede che è verificata anche la condizione di omoschedasticità.
# 
# # Per verificare ulteriormente l'assenza di autocorrelazione per i residui standardizzati
# # di questo modello, facciamo anche l'ACF, il PACF e il test di Ljung-Box classico.
# 
# # ACF
# plot_acf_residuals <- function(data, col, max_lag = NULL, ci_levels = c(0.90, 0.95, 0.99)) {
# 
#   x <- data[[col]]
#   x <- na.omit(x)
#   n <- length(x)
# 
#   if (is.null(max_lag)) {
#     max_lag <- min(10, floor(n / 4))  # Hyndman
#   }
# 
#   # Calcolo ACF (escludendo lag 0, che è sempre 1)
#   acf_res <- acf(x, lag.max = max_lag, plot = FALSE)
#   acf_vals <- acf_res$acf[-1]
#   lags <- 1:length(acf_vals)
# 
#   # Calcolo intervalli di confidenza per i livelli dati
#   ci_vals <- sapply(ci_levels, function(level) qnorm((1 + level) / 2) / sqrt(n))
#   names(ci_vals) <- paste0(ci_levels * 100, "%")
# 
#   # Dataframe ACF
#   df <- data.frame(lag = lags, acf = acf_vals)
# 
#   # Dataframe intervalli di confidenza per ggplot (sia + che -)
#   ci_df <- do.call(rbind, lapply(names(ci_vals), function(name) {
#     data.frame(
#       ci_level = name,
#       y = c(ci_vals[name], -ci_vals[name])
#     )
#   }))
# 
#   ci_df$ci_level <- factor(ci_df$ci_level, levels = c("90%", "95%", "99%"))
# 
#   ci_colors <- c("90%" = "green", "95%" = "blue", "99%" = "red")
#   ci_linetypes <- c("90%" = "dotted", "95%" = "dashed", "99%" = "dotdash")
# 
#   p <- ggplot(df, aes(x = lag, y = acf)) +
#     geom_segment(aes(x = lag, xend = lag, y = 0, yend = acf), color = "black") +
#     geom_point(shape = 18, size = 3, color = "black") +
#     theme_minimal() +
#     geom_hline(data = ci_df, aes(yintercept = y, color = ci_level, linetype = ci_level), size = 1.1) +
#     scale_color_manual(name = "Confidence Interval", values = ci_colors) +
#     scale_linetype_manual(name = "Confidence Interval", values = ci_linetypes) +
#     labs(title = "ACF dei Residui Standardizzati", x = "Lag", y = "ACF Value") +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1),
#           plot.title = element_text(hjust = 0.5)
#     )
# 
#   print(p)
# }
# 
# df_z <- data.frame(z_hat = as.numeric(z_hat))
# plot_acf_residuals(df_z, col = "z_hat")
# 
# # PACF
# plot_pacf_residuals <- function(data, col, max_lag = NULL, ci_levels = c(0.90, 0.95, 0.99)) {
# 
#   x <- data[[col]]
#   x <- na.omit(x)
#   n <- length(x)
# 
#   if (is.null(max_lag)) {
#     max_lag <- min(10, floor(n / 4))  # Hyndman
#   }
# 
#   # Calcolo PACF (rimuovo lag 0, che è sempre 1)
#   pacf_res <- pacf(x, lag.max = max_lag, plot = FALSE)
#   pacf_vals <- pacf_res$acf
#   lags <- 1:length(pacf_vals)
# 
#   # Intervalli di confidenza per i livelli dati
#   ci_vals <- sapply(ci_levels, function(level) qnorm((1 + level) / 2) / sqrt(n))
#   names(ci_vals) <- paste0(ci_levels * 100, "%")
# 
#   # Dataframe PACF
#   df <- data.frame(lag = lags, pacf = pacf_vals)
# 
#   # Dataframe intervalli di confidenza per ggplot (+ e -)
#   ci_df <- do.call(rbind, lapply(names(ci_vals), function(name) {
#     data.frame(
#       ci_level = name,
#       y = c(ci_vals[name], -ci_vals[name])
#     )
#   }))
#   ci_df$ci_level <- factor(ci_df$ci_level, levels = c("90%", "95%", "99%"))
# 
#   # Palette e linee
#   ci_colors <- c("90%" = "green", "95%" = "blue", "99%" = "red")
#   ci_linetypes <- c("90%" = "dotted", "95%" = "dashed", "99%" = "dotdash")
# 
#   p <- ggplot(df, aes(x = lag, y = pacf)) +
#     geom_segment(aes(x = lag, xend = lag, y = 0, yend = pacf), color = "black") +
#     geom_point(shape = 18, size = 3, color = "black") +
#     theme_minimal() +
#     geom_hline(data = ci_df, aes(yintercept = y, color = ci_level, linetype = ci_level), size = 1.1) +
#     scale_color_manual(name = "Confidence Interval", values = ci_colors) +
#     scale_linetype_manual(name = "Confidence Interval", values = ci_linetypes) +
#     labs(title = "PACF dei Residui Standardizzati", x = "Lag", y = "PACF Value") +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1),
#           plot.title = element_text(hjust = 0.5)
#     )
# 
#   print(p)
# }
# 
# plot_pacf_residuals(df_z, col = "z_hat")
# 
# # L’analisi visiva di ACF e PACF mostra che, in entrambi i grafici, nessuna barra supera la banda di confidenza al 90%.
# # Dunque, non c'è una forte evidenza visiva di autocorrelazione.
# 
# # Ljung-Box Test
# ljungbox_test(as.numeric(z_hat), col_name = "Residui standardizzati")
# # Ljung-Box test per Residui standardizzati (max_lag = 10):
# #            lag   lb_stat lb_pvalue
# # X-squared    1 0.6450903 0.4218735
# # X-squared1   2 0.7075932 0.7020177
# # X-squared2   3 0.8887092 0.8281517
# # X-squared3   4 0.9057814 0.9237299
# # X-squared4   5 2.5941968 0.7622468
# # X-squared5   6 2.6429612 0.8521375
# # X-squared6   7 3.6290162 0.8213773
# # X-squared7   8 3.6795752 0.8848149
# # X-squared8   9 3.7601410 0.9264784
# # X-squared9  10 5.3052993 0.8698732
# 
# # Osservando i p-value associati ai vari lag, vediamo che tutti questi sono maggiori di α = 0.10. Dunque, non abbiamo evidenza statistica
# # sufficiente per rifiutare l’ipotesi nulla di assenza di autocorrelazione fino al lag specificato.
# 
# # Infine, verifichiamo anche l’assenza di eteroschedasticità condizionata nei residui standardizzati.
# 
# # Test ARCH di Engle
# arch_effects_test(z_hat, col_name = "Residui Standardizzati")
# # ARCH Test per Residui Standardizzati (max_lag = 5):
# #            lag arch_stat arch_pvalue
# # statistic    1 0.1217198   0.7271767
# # statistic1   2 0.4671017   0.7917173
# # statistic2   3 6.0105983   0.1110957
# # statistic3   4 6.9040708   0.1410453
# # statistic4   5 7.9025084   0.1616911
# 
# # Alla luce dei risultati, non possiamo rigettare l'ipotesi nulla di assenza di effetti ARCH con un livello di
# # significatività del 10% per tutti i lag. Perciò, possiamo concludere che i residui standardizzati sono privi di eteroschedasticità
# # condizionata.
# 
# # In conclusione, alla luce di tutti i risultati ottenuti dall’analisi dei residui standardizzati, possiamo concludere che
# # il modello GARCH(1,0) con distribuzione skewed Student-t ha catturato correttamente la dinamica della volatilità.
# 
# best_p <- p
# best_q <- q
# best_dist <- dist
# 
# cat(sprintf("\nModello migliore (secondo il criterio AICc + BIC): GARCH(%d,%d) con distribuzione %s\n", best_p, best_q, best_dist))
# #
# # Modello migliore (secondo il criterio AICc + BIC): GARCH(1,0) con distribuzione sstd
##################################################################################################################################################