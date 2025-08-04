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
merged_data <- reduce(list_dfs, full_join, by = "Date")

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
  
  # Calcolo split_index (80% training)
  total_len <- nrow(df)
  split_index <- floor(total_len * 0.8)
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

# Calcolo del limite per l'80% (training set)
split_index <- floor(total_rows * 0.8)

# Estrazione del training set
training_set <- merged_data %>%
  arrange(Index) %>%
  slice(1:split_index)

# Controlla la struttura finale
glimpse(training_set)
# Rows: 400
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

# Questa viene sempre rispettata da SPY_LogReturn, AAPL_LogReturn, JPM_LogReturn e XOM_LogReturn, per cui possiamo assumere l’assenza di
# autocorrelazione con significatività α = 0.10, α = 0.05 e α = 0.01 per queste serie storiche. Per la serie UNH_LogReturn la tolleranza
# rigorosa viene rispettata con significatività α = 0.05. Per la serie AMZN_LogReturn, invece, la tolleranza rigorosa viene rispettata solo
# con significatività α = 0.01.
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
#            lag   lb_stat lb_pvalue
# X-squared    1 0.7603370 0.3832231
# X-squared1   2 0.8638688 0.6492520
# X-squared2   3 1.0758307 0.7829116
# X-squared3   4 1.0897241 0.8958991
# X-squared4   5 2.7215923 0.7428141
# X-squared5   6 2.7531034 0.8391355
# X-squared6   7 3.7957194 0.8029830
# X-squared7   8 3.8413651 0.8711448
# X-squared8   9 3.9453148 0.9149747
# X-squared9  10 5.4709378 0.8575848
# 
# Ljung-Box test per AAPL_LogReturn (max_lag = 10):
#            lag  lb_stat lb_pvalue
# X-squared    1 2.545114 0.1106359
# X-squared1   2 2.620594 0.2697399
# X-squared2   3 3.004504 0.3909312
# X-squared3   4 3.959769 0.4114780
# X-squared4   5 5.280014 0.3826712
# X-squared5   6 5.598484 0.4696343
# X-squared6   7 5.624032 0.5842684
# X-squared7   8 5.878393 0.6608515
# X-squared8   9 6.088558 0.7310233
# X-squared9  10 7.232670 0.7033120
# 
# Ljung-Box test per UNH_LogReturn (max_lag = 10):
#            lag    lb_stat lb_pvalue
# X-squared    1  0.3568924 0.5502372
# X-squared1   2  0.8851608 0.6423767
# X-squared2   3  4.4100307 0.2204571
# X-squared3   4  4.6094922 0.3297612
# X-squared4   5  5.2975760 0.3806530
# X-squared5   6  5.4318134 0.4897360
# X-squared6   7  7.3783980 0.3905719
# X-squared7   8 10.8019104 0.2131778
# X-squared8   9 11.3156052 0.2546946
# X-squared9  10 11.4777440 0.3215273
# 
# Ljung-Box test per JPM_LogReturn (max_lag = 10):
#            lag    lb_stat lb_pvalue
# X-squared    1  0.7464076 0.3876160
# X-squared1   2  0.8242773 0.6622324
# X-squared2   3  1.9054520 0.5922604
# X-squared3   4  3.8655409 0.4245086
# X-squared4   5  4.9624905 0.4204751
# X-squared5   6  6.9821230 0.3225036
# X-squared6   7  8.4629179 0.2935474
# X-squared7   8  9.6994803 0.2867551
# X-squared8   9 10.2648772 0.3294707
# X-squared9  10 11.9328604 0.2895750
# 
# Ljung-Box test per AMZN_LogReturn (max_lag = 10):
#            lag     lb_stat  lb_pvalue
# X-squared    1  0.02628823 0.87119839
# X-squared1   2  5.67353455 0.05861485
# X-squared2   3  6.26852633 0.09925215
# X-squared3   4  7.55877701 0.10914523
# X-squared4   5  7.62915810 0.17789255
# X-squared5   6  7.66596060 0.26361132
# X-squared6   7  8.62855921 0.28043497
# X-squared7   8  8.83068307 0.35678019
# X-squared8   9 11.49368586 0.24337990
# X-squared9  10 12.51747217 0.25191479
# 
# Ljung-Box test per XOM_LogReturn (max_lag = 10):
#            lag    lb_stat lb_pvalue
# X-squared    1 0.02959503 0.8634123
# X-squared1   2 1.31590673 0.5179102
# X-squared2   3 1.31745567 0.7249923
# X-squared3   4 1.31752146 0.8583958
# X-squared4   5 2.55017761 0.7689200
# X-squared5   6 2.57036758 0.8605108
# X-squared6   7 4.79470366 0.6849999
# X-squared7   8 4.79776060 0.7789569
# X-squared8   9 4.83864377 0.8481411
# X-squared9  10 5.21272625 0.8765221

# Analizzando i p-value del test di Ljung-Box per ciascuna serie di log-return, osserviamo che questi sono tutti maggiori di α = 0.01 (in realtà
# anche di α = 0.05). Quindi, non abbiamo evidenza statistica sufficiente per rifiutare l’ipotesi nulla di assenza di
# autocorrelazione fino al lag specificato.
#
# Ovviamente, sottolineiamo ancora come il test non conferma la scorrelazione, ma piuttosto che non c'è evidenza sufficiente per dimostrare
# il contrario (come detto più volte a lezione: l'assenza di prove a un processo non implica necessariamente innocenza).
#
# Dunque, non possiamo rifiutare l'ipotesi nulla: i dati non sono autocorrelati.
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
# SPY_LogReturn         0.6319         0.6072        0.1288        0.5024         0.9705        0.1548
# AAPL_LogReturn        0.6072         2.0117       -0.0064        0.1332         0.8353       -0.0238
# UNH_LogReturn         0.1288        -0.0064        2.6175        0.2437        -0.1203        0.2063
# JPM_LogReturn         0.5024         0.1332        0.2437        1.8089         0.5487        0.4881
# AMZN_LogReturn        0.9705         0.8353       -0.1203        0.5487         3.2796       -0.1341
# XOM_LogReturn         0.1548        -0.0238        0.2063        0.4881        -0.1341        1.6489

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
# SPY_LogReturn         1.0000         0.5385        0.1001        0.4699         0.6742        0.1517
# AAPL_LogReturn        0.5385         1.0000       -0.0028        0.0698         0.3252       -0.0131
# UNH_LogReturn         0.1001        -0.0028        1.0000        0.1120        -0.0411        0.0993
# JPM_LogReturn         0.4699         0.0698        0.1120        1.0000         0.2253        0.2826
# AMZN_LogReturn        0.6742         0.3252       -0.0411        0.2253         1.0000       -0.0577
# XOM_LogReturn         0.1517        -0.0131        0.0993        0.2826        -0.0577        1.0000

# Stampa la matrice di correlazione dei quadrati dei rendimenti
corr_squared_returns <- cor(risky_returns_df^2, use = "pairwise.complete.obs")
print(round(corr_squared_returns, 4))
#                SPY_LogReturn AAPL_LogReturn UNH_LogReturn JPM_LogReturn AMZN_LogReturn XOM_LogReturn
# SPY_LogReturn         1.0000         0.2354        0.0921        0.3146         0.3617        0.0338
# AAPL_LogReturn        0.2354         1.0000        0.0280        0.0004         0.1300       -0.0393
# UNH_LogReturn         0.0921         0.0280        1.0000        0.1440         0.0250        0.0836
# JPM_LogReturn         0.3146         0.0004        0.1440        1.0000         0.1597        0.0785
# AMZN_LogReturn        0.3617         0.1300        0.0250        0.1597         1.0000       -0.0168
# XOM_LogReturn         0.0338        -0.0393        0.0836        0.0785        -0.0168        1.0000

# Stampa la matrice di correlazione dei valori assoluti
corr_abs_returns <- cor(abs(risky_returns_df), use = "pairwise.complete.obs")
print(round(corr_abs_returns, 4))
#                SPY_LogReturn AAPL_LogReturn UNH_LogReturn JPM_LogReturn AMZN_LogReturn XOM_LogReturn
# SPY_LogReturn         1.0000         0.3263        0.0764        0.3183         0.4847        0.0304
# AAPL_LogReturn        0.3263         1.0000        0.0178        0.0405         0.1775       -0.0160
# UNH_LogReturn         0.0764         0.0178        1.0000        0.1707         0.0239        0.0373
# JPM_LogReturn         0.3183         0.0405        0.1707        1.0000         0.1957        0.1218
# AMZN_LogReturn        0.4847         0.1775        0.0239        0.1957         1.0000        0.0257
# XOM_LogReturn         0.0304        -0.0160        0.0373        0.1218         0.0257        1.0000

# Genera le heatmaps
plot_corr_heatmap(risky_returns_df, "Matrice di Correlazione dei Rendimenti Logaritmici dei Titoli Rischiosi")
plot_corr_heatmap(risky_returns_df^2, "Matrice di Correlazione dei Quadrati dei Rendimenti Logaritmici dei Titoli Rischiosi")
plot_corr_heatmap(abs(risky_returns_df), "Matrice di Correlazione dei Valori Assoluti dei Rendimenti Logaritmici dei Titoli Rischiosi")

# L’analisi della matrice di correlazione sui rendimenti logaritmici giornalieri mostra che solo alcune coppie di titoli presentano
# correlazioni moderate o relativamente forti, come ad esempio SPY–AMZN e SPY–AAPL.
#
# La maggior parte delle correlazioni è debole o trascurabile, specialmente tra UNH e gli altri titoli, il che indica che non tutte le serie
# si muovono in modo sincronizzato.
#
# Le correlazioni calcolate sui valori assoluti e sui quadrati dei rendimenti indicano una debole co-movimentazione della volatilità,
# ma non suggeriscono una forte dipendenza in termini di direzione del movimento dei prezzi.
#
# Pertanto, non possiamo considerare l’ipotesi che le serie siano completamente indipendenti tra loro.


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
# Statistic: 582.247179
# p-value: 0
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla al livello di significatività del 1%
# → Le matrici di covarianza del modello completo e del modello ridotto NON sono uguali.

# Alla luce dei risultati, il test LR rileva differenze significative tra le matrici di covarianza dei modelli considerati.
# Pertanto, non assumiamo l’indipendenza tra le serie.
##################################################################################################################################################

################################################### Analisi della Volatilità dell’S&P 500 tramite Modello GARCH ##################################
# Quello che vogliamo fare adesso è applicare un modello GARCH(1,1) alla serie storica dell’S&P 500 al fine di stimare la volatilità condizionata.
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
# Statistic: 1.457150
# p-value  : 0.964231
# 
# ** CONCLUSIONE: p-value > 0.01 **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON sono stazionari.
# 
# 
# [SPY_AdjClose] ADF Test - Con Intercetta (Drift)
# Statistic: -0.808345
# p-value  : 0.815276
# 
# ** CONCLUSIONE: p-value > 0.01 **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON sono stazionari.
# 
# 
# [SPY_AdjClose] ADF Test - Con Intercetta e Trend
# Statistic: -2.934036
# p-value  : 0.152864
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
# Statistic: 6.563054
# Valore critico (circa 1%): 0.739000
# Interpretazione del p-value: p-value < 0.01
# 
# ** CONCLUSIONE: Statistic > valore critico (1%) **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON sono stazionari.
# 
# 
# [SPY_AdjClose] KPSS Test - Stazionarietà attorno a un trend deterministico
# Statistic: 0.278260
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
adf_test_all(training_set, "SPY_LogReturn")
#
# [SPY_LogReturn] ADF Test - Senza Intercetta
# Statistic: -18.976249
# p-value  : 2.51358e-37
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO stazionari.
# 
# 
# [SPY_LogReturn] ADF Test - Con Intercetta (Drift)
# Statistic: -19.084201
# p-value  : 1.85222e-34
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO stazionari.
# 
# 
# [SPY_LogReturn] ADF Test - Con Intercetta e Trend
# Statistic: -19.065190
# p-value  : 2.33523e-40
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO stazionari.


# KPSS Test
kpss_test_all(training_set, "SPY_LogReturn")
#
# [SPY_LogReturn] KPSS Test - Stazionarietà attorno a una media costante
# Statistic: 0.102447
# Valore critico (circa 1%): 0.739000
# Interpretazione del p-value: p-value > 0.10
# 
# ** CONCLUSIONE: Statistic <= valore critico (1%) **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO stazionari.
# 
# 
# [SPY_LogReturn] KPSS Test - Stazionarietà attorno a un trend deterministico
# Statistic: 0.097492
# Valore critico (circa 1%): 0.216000
# Interpretazione del p-value: p-value > 0.10
# 
# ** CONCLUSIONE: Statistic <= valore critico (1%) **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO stazionari.

# Entrambi i test hanno dato lo stesso esito per tutte le possibili ipotesi alternative.
# Le trasformazioni effettuate, indicano che la nuova serie dei log-rendimenti può essere considerata stazionaria.


# Occorre anche controllare la stazionarietà in varianza, ovvero se la serie è omoschedastica o eteroschedastica:

# Test di Breusch-Pagan.
# Il test di Breusch-Pagan verifica la presenza di eteroschedasticità (varianza non costante degli errori) in un modello di regressione.
# Il test ha come:
#
# - H_0: I dati SONO omoschedastici (NON sono eteroschedastici);
# - H_1: I dati NON sono omoschedastici (SONO eteroschedastici).
breusch_pagan_test <- function(index, group, col, alpha = 0.01) {
  model <- lm(group ~ index)
  bp <- lmtest::bptest(model)
  
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
# Statistic: 1.873334
# p-value: 0.171094
# 
# ** Conclusione: p-value > 0.01 **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON sono eteroschedastici.


# Test di White.
# Anche il test di White verifica la presenza di eteroschedasticità in un modello di regressione.
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
# Statistic: 2.572405
# p-value: 0.276318
# 
# ** Conclusione: p-value > 0.01 **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON sono eteroschedastici.

# Alla luce dei risultati, non possiamo rifiutare l'ipotesi nulla di omoschedasticità: non c'è eteroschedasticità non condizionata.


# Un'altra caratteristica tipica delle serie finanziarie è la presenza di autocorrelazione tra i rendimenti, cioè la dipendenza tra
# osservazioni successive.
# Questa analisi è già stata svolta precedentemente, ma la rifacciamo per evidenziare solo i risultati relativi allo S&P 500.

# Test di Ljung-Box
ljungbox_test(training_set[["SPY_LogReturn"]], col_name = "SPY_LogReturn")
# Ljung-Box test per SPY_LogReturn (max_lag = 10):
#            lag   lb_stat lb_pvalue
# X-squared    1 0.7603370 0.3832231
# X-squared1   2 0.8638688 0.6492520
# X-squared2   3 1.0758307 0.7829116
# X-squared3   4 1.0897241 0.8958991
# X-squared4   5 2.7215923 0.7428141
# X-squared5   6 2.7531034 0.8391355
# X-squared6   7 3.7957194 0.8029830
# X-squared7   8 3.8413651 0.8711448
# X-squared8   9 3.9453148 0.9149747
# X-squared9  10 5.4709378 0.8575848

# Osservando i p-value associati ai vari lag, vediamo che tutti questi sono maggiori di α = 0.10. Dunque, non abbiamo evidenza statistica
# sufficiente per rifiutare l’ipotesi nulla di assenza di autocorrelazione fino al lag specificato.
# L’assenza di forte autocorrelazione giustifica ulteriormente l’uso di un modello GARCH.


# ACF
plot_acf(training_set, "SPY_LogReturn")

# PACF
plot_pacf(training_set, "SPY_LogReturn")

# L’analisi visiva di ACF e PACF mostra che, in entrambi i grafici, nessuna barra supera la banda di confidenza al 90%.
# Dunque, non c'è una forte evidenza visiva di autocorrelazione.


# Infine, è fondamentale verificare l'esistenza dell'eteroschedasticità condizionata, ovvero se la varianza cambia nel tempo in modo sistematico.
# Se tale condizione non fosse presente, allora sarebbe inappropriato utilizzare un modello GARCH.

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
#            lag arch_stat arch_pvalue
# statistic    1  1.048248  0.30591092
# statistic1   2  1.631908  0.44221733
# statistic2   3  7.972773  0.04657777
# statistic3   4  8.623328  0.07123587
# statistic4   5  9.363585  0.09541163

# Alla luce dei risultati, possiamo affermare che abbiamo un rigetto dell'ipotesi nulla di assenza di effetti ARCH con un livello di
# significatività del 5% al lag 3 e con un livello di significatività del 10% a partire dal lag 3.
# Pertanto, costruiremo un modello GARCH per i rendimenti logaritmici percentuali dei prezzi di chiusura aggiustati giornalieri dell’S&P 500
# presenti nel training set.


# Utilizziamo, dunque, il modello GARCH per stimare la volatilità dei ritorni logaritmici ed esploriamo diverse configurazioni di parametri
# p e q (escluso il caso p = q = 0, al variare di p e q tra 0,..,4), per poi selezionare il modello che massimizza la Log-Likelihood e
# minimizza AIC/BIC, bilanciando, così, il trade-off tra adattamento e complessità.
# Per fare ciò, ci avvaliamo della libreria rugarch.

# Serie dei rendimenti logaritmici
returns <- na.omit(training_set$SPY_LogReturn)

# Data frame per salvare i risultati
model_results <- data.frame(
  p = integer(),
  q = integer(),
  loglik = numeric(),
  AIC = numeric(),
  BIC = numeric(),
  stringsAsFactors = FALSE
)

# Loop su p e q in {0,1,2,3,4} escluso p=0, q=0
for (p in 0:4) {
  for (q in 0:4) {
    if (p == 0 && q == 0) next  # salta il caso p = 0 e q = 0
    
    spec <- ugarchspec(
      variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
      mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
      distribution.model = "norm"
    )
    
    tryCatch({
      fit <- ugarchfit(spec, data = returns, solver = "hybrid")
      model_results <- rbind(model_results, data.frame(
        p = p,
        q = q,
        loglik = likelihood(fit),
        AIC = infocriteria(fit)[1],
        BIC = infocriteria(fit)[2]
      ))
    }, error = function(e) {
      message(sprintf("Errore per GARCH(%d,%d): %s", p, q, e$message))
    })
  }
}

# Ranking (minore è meglio per AIC/BIC, maggiore è meglio per log-likelihood)
model_results$rank_loglik <- rank(-model_results$loglik, ties.method = "min")
model_results$rank_AIC <- rank(model_results$AIC, ties.method = "min")
model_results$rank_BIC <- rank(model_results$BIC, ties.method = "min")

# Somma dei ranghi: punteggio complessivo (basso è meglio)
model_results$total_score <- model_results$rank_loglik + model_results$rank_AIC + model_results$rank_BIC

# Ordina per punteggio totale
model_results <- model_results[order(model_results$total_score), ]

# Stampa i primi 5 modelli migliori
print(head(model_results, 5))
#    p q    loglik      AIC      BIC rank_loglik rank_AIC rank_BIC total_score
# 5  1 0 -474.6866 2.389406 2.409401           5        2        1           8
# 15 3 0 -472.1077 2.386505 2.426495           2        1        6           9
# 20 4 0 -471.8992 2.390472 2.440459           1        3        7          11
# 10 2 0 -474.4540 2.393253 2.423245           4        5        3          12
# 2  0 2 -475.0407 2.396194 2.426186          17        6        4          27

# Estrai modello "ottimale" secondo il punteggio combinato
best_p <- model_results$p[1]
best_q <- model_results$q[1]

cat(sprintf("\nModello selezionato (compromesso tra loglik, AIC, BIC): GARCH(%d,%d)\n", best_p, best_q))
#
# Modello selezionato (compromesso tra loglik, AIC, BIC): GARCH(1,0)

# Stima finale
best_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(best_p, best_q)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = "norm"
)

best_fit <- ugarchfit(spec = best_spec, data = returns)

# Mostra i risultati finali
show(best_fit)
#
# *---------------------------------*
# *          GARCH Model Fit        *
# *---------------------------------*
#   
# Conditional Variance Dynamics 	
# -----------------------------------
# GARCH Model	    : sGARCH(1,0)
# Mean Model	    : ARFIMA(0,0,0)
# Distribution	  : norm 
# 
# Optimal Parameters
# ------------------------------------
#         Estimate  Std. Error  t value Pr(>|t|)
# omega   0.598388    0.053273  11.2324   0.0000
# alpha1  0.059943    0.058728   1.0207   0.3074
# 
# Robust Standard Errors:
#         Estimate  Std. Error  t value Pr(>|t|)
# omega   0.598388    0.070563  8.48025  0.00000
# alpha1  0.059943    0.090672  0.66109  0.50855
# 
# LogLikelihood : -474.6866 
# 
# Information Criteria
# ------------------------------------
#   
# Akaike       2.3894
# Bayes        2.4094
# Shibata      2.3894
# Hannan-Quinn 2.3973
# 
# Weighted Ljung-Box Test on Standardized Residuals
# ------------------------------------
#                         statistic p-value
# Lag[1]                     0.5536  0.4569
# Lag[2*(p+q)+(p+q)-1][2]    0.5733  0.6602
# Lag[4*(p+q)+(p+q)-1][5]    1.0371  0.8512
# d.o.f=0
# H0 : No serial correlation
# 
# Weighted Ljung-Box Test on Standardized Squared Residuals
# ------------------------------------
#                         statistic p-value
# Lag[1]                    0.08733  0.7676
# Lag[2*(p+q)+(p+q)-1][2]   0.23461  0.8333
# Lag[4*(p+q)+(p+q)-1][5]   4.01020  0.2528
# d.o.f=1
# 
# Weighted ARCH LM Tests
# ------------------------------------
#             Statistic Shape Scale P-Value
# ARCH Lag[2]    0.2916 0.500 2.000 0.58918
# ARCH Lag[4]    4.5080 1.397 1.611 0.11573
# ARCH Lag[6]    7.0627 2.222 1.500 0.06824
# 
# Nyblom stability test
# ------------------------------------
# Joint Statistic:  0.3028
# Individual Statistics:             
# omega  0.2460
# alpha1 0.1291
# 
# Asymptotic Critical Values (10% 5% 1%)
# Joint Statistic:     	   0.61 0.749 1.07
# Individual Statistic:	   0.35 0.47 0.75
# 
# Sign Bias Test
# ------------------------------------
#                    t-value    prob sig
# Sign Bias          1.78678 0.07474   *
# Negative Sign Bias 0.19592 0.84477    
# Positive Sign Bias 0.04405 0.96489    
# Joint Effect       6.80731 0.07830   *
#   
#   
# Adjusted Pearson Goodness-of-Fit Test:
# ------------------------------------
#   group statistic p-value(g-1)
# 1    20     41.65     0.001975
# 2    30     55.96     0.001910
# 3    40     61.25     0.012945
# 4    50     74.56     0.010759
# 
# 
# Elapsed time : 0.1311309


# Adesso, dopo aver stimato il modello GARCH migliore, possiamo estrarre facilmente i residui standardizzati, analizzarli e usarli
# per verificare se il modello ha effettivamente catturato la dinamica della volatilità.

# Estrai residui standardizzati
z_hat <- residuals(best_fit, standardize = TRUE)

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
# 2023-08-01 -0.3598271
# 2023-08-02 -1.8035051
# 2023-08-03 -0.3391767
# 2023-08-04 -0.5835744
# 2023-08-07  1.1120581
# 2023-08-08 -0.5432201


# Applichiamo, ora, dei dest di normalità sui residui.

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
# Statistic: 0.984416
# p-value  : 0.000270527
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
# Statistic: 23.916069
# p-value  : 6.40754e-06
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON seguono una distribuzione normale.

# Visto che i residui standardizzati del GARCH con distribuzione normale non seguono una distribuzione normale, potrebbe servire un modello
# con t-Student o GED.


# Per catturare meglio la presenza di code pesanti e asimmetrie, stimiamo nuovamente diversi modelli GARCH ma assumendo per i residui:
# - una distribuzione t-Student;
# - una distribuzione GED (Generalized Error Distribution).

# Modello con t-Student
# Data frame per salvare i risultati con distribuzione t-Student
model_results_t <- data.frame(
  p = integer(),
  q = integer(),
  loglik = numeric(),
  AIC = numeric(),
  BIC = numeric(),
  stringsAsFactors = FALSE
)

for (p in 0:4) {
  for (q in 0:4) {
    if (p == 0 && q == 0) next
    
    spec <- ugarchspec(
      variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
      mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
      distribution.model = "std"  # t-Student
    )
    
    tryCatch({
      fit <- ugarchfit(spec, data = returns, solver = "hybrid")
      model_results_t <- rbind(model_results_t, data.frame(
        p = p,
        q = q,
        loglik = likelihood(fit),
        AIC = infocriteria(fit)[1],
        BIC = infocriteria(fit)[2]
      ))
    }, error = function(e) {
      message(sprintf("Errore per GARCH(%d,%d) con t-Student: %s", p, q, e$message))
    })
  }
}

# Ranking e selezione analoga a prima
model_results_t$rank_loglik <- rank(-model_results_t$loglik, ties.method = "min")
model_results_t$rank_AIC <- rank(model_results_t$AIC, ties.method = "min")
model_results_t$rank_BIC <- rank(model_results_t$BIC, ties.method = "min")
model_results_t$total_score <- model_results_t$rank_loglik + model_results_t$rank_AIC + model_results_t$rank_BIC
model_results_t <- model_results_t[order(model_results_t$total_score), ]

# Stampa i primi 5 modelli migliori con t-Student
print(head(model_results_t, 5))
#    p q    loglik      AIC      BIC rank_loglik rank_AIC rank_BIC total_score
# 16 3 1 -465.9035 2.365431 2.425416           2        1        7          10
# 5  1 0 -470.7916 2.374895 2.404887           7        4        1          12
# 15 3 0 -468.6915 2.374394 2.424381           4        3        6          13
# 21 4 1 -465.4422 2.368131 2.438113           1        2       11          14
# 10 2 0 -470.5455 2.378674 2.418664           6        7        3          16

# Estrai modello "ottimale" secondo il punteggio combinato
best_p_t <- model_results_t$p[1]
best_q_t <- model_results_t$q[1]

cat(sprintf("\nModello selezionato (compromesso tra loglik, AIC, BIC): GARCH(%d,%d)\n", best_p_t, best_q_t))
#
# Modello selezionato (compromesso tra loglik, AIC, BIC): GARCH(3,1)

# Stima finale
best_spec_t <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(best_p_t, best_q_t)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = "std"
)

best_fit_t <- ugarchfit(spec = best_spec_t, data = returns)

# Mostra i risultati finali
show(best_fit_t)
#
# *---------------------------------*
# *          GARCH Model Fit        *
# *---------------------------------*
#   
# Conditional Variance Dynamics 	
# -----------------------------------
# GARCH Model	    : sGARCH(3,1)
# Mean Model	    : ARFIMA(0,0,0)
# Distribution	  : std 
# 
# Optimal Parameters
# ------------------------------------
#         Estimate  Std. Error  t value Pr(>|t|)
# omega   0.101170    0.052076 1.942733 0.052048
# alpha1  0.000002    0.118059 0.000016 0.999987
# alpha2  0.020757    0.110639 0.187608 0.851184
# alpha3  0.104667    0.068155 1.535724 0.124606
# beta1   0.722564    0.099777 7.241791 0.000000
# shape   9.345211    4.178368 2.236569 0.025315
# 
# Robust Standard Errors:
#         Estimate  Std. Error   t value Pr(>|t|)
# omega   0.101170    0.046896  2.157297 0.030983
# alpha1  0.000002    0.283723  0.000007 0.999995
# alpha2  0.020757    0.251221  0.082624 0.934151
# alpha3  0.104667    0.066650  1.570399 0.116322
# beta1   0.722564    0.061260 11.794967 0.000000
# shape   9.345211    4.780982  1.954664 0.050623
# 
# LogLikelihood : -465.9035 
# 
# Information Criteria
# ------------------------------------
#   
# Akaike       2.3654
# Bayes        2.4254
# Shibata      2.3650
# Hannan-Quinn 2.3892
# 
# Weighted Ljung-Box Test on Standardized Residuals
# ------------------------------------
#                         statistic p-value
# Lag[1]                     0.2523  0.6155
# Lag[2*(p+q)+(p+q)-1][2]    0.2837  0.8048
# Lag[4*(p+q)+(p+q)-1][5]    0.7783  0.9075
# d.o.f=0
# H0 : No serial correlation
# 
# Weighted Ljung-Box Test on Standardized Squared Residuals
# ------------------------------------
#                          statistic p-value
# Lag[1]                    0.001155  0.9729
# Lag[2*(p+q)+(p+q)-1][11]  1.410836  0.9878
# Lag[4*(p+q)+(p+q)-1][19]  3.102175  0.9941
# d.o.f=4
# 
# Weighted ARCH LM Tests
# ------------------------------------
#             Statistic Shape Scale P-Value
# ARCH Lag[5]    0.1491 0.500 2.000  0.6994
# ARCH Lag[7]    0.6365 1.473 1.746  0.8597
# ARCH Lag[9]    1.6447 2.402 1.619  0.8259
# 
# Nyblom stability test
# ------------------------------------
# Joint Statistic:  1.0057
# Individual Statistics:              
# omega  0.04730
# alpha1 0.06709
# alpha2 0.10946
# alpha3 0.08071
# beta1  0.04234
# shape  0.30169
# 
# Asymptotic Critical Values (10% 5% 1%)
# Joint Statistic:     	   1.49 1.68 2.12
# Individual Statistic:	   0.35 0.47 0.75
# 
# Sign Bias Test
# ------------------------------------
#                    t-value    prob sig
# Sign Bias          2.01387 0.04470  **
# Negative Sign Bias 0.08586 0.93162    
# Positive Sign Bias 0.34152 0.73289    
# Joint Effect       7.56633 0.05588   *
#   
#   
# Adjusted Pearson Goodness-of-Fit Test:
# ------------------------------------
#   group statistic p-value(g-1)
# 1    20     32.43      0.02795
# 2    30     43.63      0.03976
# 3    40     61.45      0.01239
# 4    50     68.04      0.03717
# 
# 
# Elapsed time : 24.1166

# Modello con GED
# Data frame per salvare i risultati con distribuzione GED
model_results_ged <- data.frame(
  p = integer(),
  q = integer(),
  loglik = numeric(),
  AIC = numeric(),
  BIC = numeric(),
  stringsAsFactors = FALSE
)

for (p in 0:4) {
  for (q in 0:4) {
    if (p == 0 && q == 0) next
    
    spec <- ugarchspec(
      variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
      mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
      distribution.model = "ged"  # GED
    )
    
    tryCatch({
      fit <- ugarchfit(spec, data = returns, solver = "hybrid")
      model_results_ged <- rbind(model_results_ged, data.frame(
        p = p,
        q = q,
        loglik = likelihood(fit),
        AIC = infocriteria(fit)[1],
        BIC = infocriteria(fit)[2]
      ))
    }, error = function(e) {
      message(sprintf("Errore per GARCH(%d,%d) con GED: %s", p, q, e$message))
    })
  }
}

# Pulisci i risultati rimuovendo eventuali righe con NA o problemi
model_results_ged <- model_results_ged[complete.cases(model_results_ged), ]

# Ranking e selezione analoga a prima
model_results_ged$rank_loglik <- rank(-model_results_ged$loglik, ties.method = "min")
model_results_ged$rank_AIC <- rank(model_results_ged$AIC, ties.method = "min")
model_results_ged$rank_BIC <- rank(model_results_ged$BIC, ties.method = "min")
model_results_ged$total_score <- model_results_ged$rank_loglik + model_results_ged$rank_AIC + model_results_ged$rank_BIC
model_results_ged <- model_results_ged[order(model_results_ged$total_score), ]

# Stampa i primi 5 modelli migliori con GED
print(head(model_results_ged, 5))
#    p q    loglik        AIC        BIC rank_loglik rank_AIC rank_BIC total_score
# 5  1 0   -1.1000 0.02055138 0.05054357           1        1        1           3
# 11 2 1 -465.7680 2.35973929 2.40972628           3        2        3           8
# 21 4 1 -464.4644 2.36323024 2.43321202           2        3       10          15
# 15 3 0 -467.1013 2.36642274 2.41640972           5        5        6          16
# 20 4 0 -466.8799 2.37032546 2.43030984           4        6        9          19

# Estrai modello "ottimale" secondo il punteggio combinato
best_p_ged <- model_results_ged$p[1]
best_q_ged <- model_results_ged$q[1]

cat(sprintf("\nModello selezionato (compromesso tra loglik, AIC, BIC): GARCH(%d,%d)\n", best_p_ged, best_q_ged))
#
# Modello selezionato (compromesso tra loglik, AIC, BIC): GARCH(1,0)

# Stima finale
best_spec_ged <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(best_p_ged, best_q_ged)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = "ged"
)

best_fit_ged <- ugarchfit(spec = best_spec_ged, data = returns)

# Mostra i risultati finali
show(best_fit_ged)
##################################################################################################################################################