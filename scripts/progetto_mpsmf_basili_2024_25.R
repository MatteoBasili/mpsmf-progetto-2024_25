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
  plot_acf(merged_data, col)
}

# La tolleranza rigorosa per i picchi è:
#
# - per la linea di confidenza del 90%: ⌊(maxlag × 0.10)⌋ = ⌊(10 × 0.10)⌋ = 1
# - per la linea di confidenza del 95%: ⌊(maxlag × 0.05)⌋ = ⌊(10 × 0.05)⌋ = 0
# - per la linea di confidenza del 99%: ⌊(maxlag × 0.01)⌋ = ⌊(10 × 0.01)⌋ = 0

# Questa viene sempre rispettata da UNH_LogReturn, JPM_LogReturn e AMZN_LogReturn, per cui possiamo assumere l’assenza di autocorrelazione con
# significatività α = 0.10, α = 0.05 e α = 0.01 per queste serie storiche. Per le serie SPY_LogReturn e AAPL_LogReturn la tolleranza rigorosa
# viene rispettata solo con significatività α = 0.01. Per la variabile XOM_LogReturn, invece, la tolleranza rigorosa non viene mai rispettata.
#
# Per ora, non possiamo, quindi, assumere l’assenza di autocorrelazione per la serie XOM_LogReturn, mentre possiamo assumere l'assenza di
# autocorrelazione per tutte le altre serie storiche con un livello di significatività α = 0.01.
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
  plot_pacf(merged_data, col)
}

# Nel PACF, ora, per la serie XOM_LogReturn possiamo assumere l'assenza di autocorrelazione parziale con significatività α = 0.01, mentre per le
# altre serie possiamo trarre le stesse conclusioni di prima.

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
  ljungbox_test(merged_data[[col]], col_name = col)
}
# Ljung-Box test per SPY_LogReturn (max_lag = 10):
#            lag   lb_stat   lb_pvalue
# X-squared    1  2.151626 0.142418961
# X-squared1   2  5.436812 0.065979849
# X-squared2   3 11.668729 0.008608552
# X-squared3   4 13.494766 0.009095025
# X-squared4   5 13.494803 0.019157864
# X-squared5   6 14.136206 0.028151125
# X-squared6   7 14.294220 0.046189081
# X-squared7   8 14.502604 0.069569836
# X-squared8   9 14.574400 0.103309632
# X-squared9  10 14.775374 0.140468457
# 
# Ljung-Box test per AAPL_LogReturn (max_lag = 10):
#            lag    lb_stat  lb_pvalue
# X-squared    1  0.8107328 0.36790368
# X-squared1   2  5.1030605 0.07796227
# X-squared2   3  7.9138552 0.04782602
# X-squared3   4  9.8854829 0.04240153
# X-squared4   5 13.3245762 0.02051976
# X-squared5   6 13.3245978 0.03816132
# X-squared6   7 14.3646265 0.04506339
# X-squared7   8 14.3735117 0.07253460
# X-squared8   9 14.7355257 0.09846154
# X-squared9  10 15.9176791 0.10201330
# 
# Ljung-Box test per UNH_LogReturn (max_lag = 10):
#            lag  lb_stat  lb_pvalue
# X-squared    1 2.940947 0.08636007
# X-squared1   2 3.298267 0.19221636
# X-squared2   3 3.339238 0.34221873
# X-squared3   4 3.988325 0.40758818
# X-squared4   5 4.088408 0.53675893
# X-squared5   6 4.203304 0.64918531
# X-squared6   7 4.825012 0.68130796
# X-squared7   8 4.968564 0.76093074
# X-squared8   9 5.333322 0.80433781
# X-squared9  10 5.336205 0.86761801
# 
# Ljung-Box test per JPM_LogReturn (max_lag = 10):
#            lag     lb_stat lb_pvalue
# X-squared    1 0.001371536 0.9704577
# X-squared1   2 1.355756630 0.5076930
# X-squared2   3 1.793473236 0.6163563
# X-squared3   4 3.485675208 0.4800598
# X-squared4   5 4.276157826 0.5103789
# X-squared5   6 5.491555824 0.4824779
# X-squared6   7 5.985976342 0.5413878
# X-squared7   8 8.499269274 0.3862782
# X-squared8   9 8.510487630 0.4836287
# X-squared9  10 8.720641589 0.5588051
# 
# Ljung-Box test per AMZN_LogReturn (max_lag = 10):
#            lag   lb_stat lb_pvalue
# X-squared    1 0.4867213 0.4853935
# X-squared1   2 2.1606233 0.3394897
# X-squared2   3 2.2306694 0.5259328
# X-squared3   4 2.2936766 0.6819206
# X-squared4   5 2.5839026 0.7638095
# X-squared5   6 2.5845335 0.8588897
# X-squared6   7 3.4591562 0.8395325
# X-squared7   8 4.0805986 0.8497793
# X-squared8   9 4.8519644 0.8470172
# X-squared9  10 4.8660782 0.8999426
# 
# Ljung-Box test per XOM_LogReturn (max_lag = 10):
#            lag     lb_stat lb_pvalue
# X-squared    1  0.06534642 0.7982370
# X-squared1   2  0.60308064 0.7396780
# X-squared2   3  0.98284967 0.8054017
# X-squared3   4  1.21406500 0.8757775
# X-squared4   5  2.25732414 0.8125140
# X-squared5   6  2.41337197 0.8780340
# X-squared6   7  9.13177403 0.2433332
# X-squared7   8  9.66440497 0.2893746
# X-squared8   9 13.54618546 0.1394087
# X-squared9  10 13.66088282 0.1890292

# Analizzando i p-value del test di Ljung-Box per ciascuna serie di log-return, osserviamo:
# - SPY_LogReturn e AAPL_LogReturn mostrano p-value significativamente bassi (inferiori a 0.05) per diversi lag, specialmente tra lag 3 e lag 7.
#   Ciò indica che possiamo rifiutare l’ipotesi nulla di assenza di autocorrelazione per questi lag. Pertanto, per questi titoli c’è evidenza
#   statistica di autocorrelazione nelle serie di rendimenti logaritmici.
# - Per UNH_LogReturn, JPM_LogReturn, AMZN_LogReturn e XOM_LogReturn, i p-value sono generalmente alti (superiori a 0.05) per tutti i lag
#   considerati. Non abbiamo quindi evidenza statistica sufficiente per rifiutare l’ipotesi nulla di assenza di autocorrelazione.
#   Tuttavia, questo non significa che possiamo affermare con certezza che non esista autocorrelazione, ma semplicemente che il test non ne
#   rileva in modo significativo.
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

cov_matrix <- calculate_cov_matrix(merged_data, risky_logreturn_cols)

# Mostra la matrice
print(round(cov_matrix, 4))
#                SPY_LogReturn AAPL_LogReturn UNH_LogReturn JPM_LogReturn AMZN_LogReturn XOM_LogReturn
# SPY_LogReturn         1.0669         1.2488        0.1862        0.9508         1.4718        0.4726
# AAPL_LogReturn        1.2488         2.9768       -0.0220        0.8446         1.6145        0.5394
# UNH_LogReturn         0.1862        -0.0220        5.4824        0.2086         0.0014        0.0053
# JPM_LogReturn         0.9508         0.8446        0.2086        2.1589         1.1316        0.7445
# AMZN_LogReturn        1.4718         1.6145        0.0014        1.1316         3.8201        0.2746
# XOM_LogReturn         0.4726         0.5394        0.0053        0.7445         0.2746        1.9861

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
risky_returns_df <- merged_data %>%
  select(all_of(risky_logreturn_cols)) %>%
  na.omit()

# Genera la heatmap
plot_corr_heatmap(risky_returns_df, "Matrice di Correlazione dei Rendimenti Logaritmici dei Titoli Rischiosi")

plot_corr_heatmap(risky_returns_df^2, "Matrice di Correlazione dei Quadrati dei Rendimenti Logaritmici dei Titoli Rischiosi")

plot_corr_heatmap(abs(risky_returns_df), "Matrice di Correlazione dei Valori Assoluti dei Rendimenti Logaritmici dei Titoli Rischiosi")

# Dai risultati ottenuti, osserviamo che i coefficienti di correlazione sono quasi tutti molto alti, il che suggerisce che esiste una
# correlazione significativa tra le serie, tranne che per la serie UNH_LogReturn e tutte le altre. Pertanto, non possiamo considerare
# l’ipotesi che le serie siano completamente indipendenti tra loro.


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

# Esegue il test
lr_test(log_return_clean, alpha = 0.01)
# Likelihood Ratio Test
# Statistic: 1104.484370
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
#   - none: serie STAZIONARIA con media zero (nessun intercept);
#   - drift: serie STAZIONARIA con media costante (intercept);
#   - trend: serie STAZIONARIA attorno a un trend lineare (intercept + trend).
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
adf_test_all(merged_data, "SPY_AdjClose")
# [SPY_AdjClose] ADF Test - Senza Intercetta
# Statistic: 1.588481
# p-value  : 0.97284
# 
# ** CONCLUSIONE: p-value > 0.01 **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON sono stazionari.
# 
# 
# [SPY_AdjClose] ADF Test - Con Intercetta (Drift)
# Statistic: -0.572014
# p-value  : 0.873703
# 
# ** CONCLUSIONE: p-value > 0.01 **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON sono stazionari.
# 
# 
# [SPY_AdjClose] ADF Test - Con Intercetta e Trend
# Statistic: -2.594388
# p-value  : 0.283127
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
#   - Level: la serie NON è stazionaria → ha una radice unitaria rispetto al livello (cioè la media varia nel tempo);
#   - Trend: la serie NON è stazionaria → ha una radice unitaria rispetto al trend (cioè non staziona attorno a un trend deterministico).
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
kpss_test_all(merged_data, "SPY_AdjClose")
# [SPY_AdjClose] KPSS Test - Stazionarietà attorno a una media costante
# Statistic: 7.440873
# Valore critico (circa 1%): 0.739000
# Interpretazione del p-value: p-value < 0.01
# 
# ** CONCLUSIONE: Statistic > valore critico (1%) **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON sono stazionari.
# 
# 
# [SPY_AdjClose] KPSS Test - Stazionarietà attorno a un trend deterministico
# Statistic: 0.965340
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
adf_test_all(merged_data, "SPY_LogReturn")
# [SPY_LogReturn] ADF Test - Senza Intercetta
# Statistic: -14.977208
# p-value  : 3.49039e-31
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO stazionari.
# 
# 
# [SPY_LogReturn] ADF Test - Con Intercetta (Drift)
# Statistic: -15.083055
# p-value  : 1.28862e-30
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO stazionari.
# 
# 
# [SPY_LogReturn] ADF Test - Con Intercetta e Trend
# Statistic: -15.067622
# p-value  : 7.68936e-34
# 
# ** CONCLUSIONE: p-value <= 0.01 **
# Possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO stazionari.


# KPSS Test
kpss_test_all(merged_data, "SPY_LogReturn")
# [SPY_LogReturn] KPSS Test - Stazionarietà attorno a una media costante
# Statistic: 0.058694
# Valore critico (circa 1%): 0.739000
# Interpretazione del p-value: p-value > 0.10
# 
# ** CONCLUSIONE: Statistic <= valore critico (1%) **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati SONO stazionari.
# 
# 
# [SPY_LogReturn] KPSS Test - Stazionarietà attorno a un trend deterministico
# Statistic: 0.063419
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
# - H_1: i dati NON sono omoschedastici (SONO eteroschedastici).
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
valid_rows <- !is.na(merged_data$SPY_LogReturn)
index_clean <- merged_data$Index[valid_rows]
group_clean <- merged_data$SPY_LogReturn[valid_rows]

# Esegue il test di Breusch-Pagan
breusch_pagan_test(index_clean, group_clean, "SPY_LogReturn")
# [SPY_LogReturn] Breusch-Pagan Test
# Statistic: 6.215749
# p-value: 0.0126619
# 
# ** Conclusione: p-value > 0.01 **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON sono eteroschedastici.


# Test di White.
# Anche il test di White verifica la presenza di eteroschedasticità in un modello di regressione.
# Il test ha come:
#
# - H_0: I dati SONO omoschedastici (NON sono eteroschedastici);
# - H_1: i dati NON sono omoschedastici (SONO eteroschedastici).
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
# Statistic: 6.771341
# p-value: 0.0338549
# 
# ** Conclusione: p-value > 0.01 **
# Non possiamo rigettare l'ipotesi nulla con un livello di significatività del 1%
# → I dati NON sono eteroschedastici.

# Alla luce dei risultati, non possiamo rifiutare l'ipotesi nulla di omoschedasticità: non c'è eteroschedasticità non condizionata.


# Un'altra caratteristica tipica delle serie finanziarie è la presenza di autocorrelazione tra i rendimenti, cioè la dipendenza tra
# osservazioni successive.
# Questa analisi è già stata svolta precedentemente, ma la rifacciamo per evidenziare solo i risultati relativi allo S&P 500.

# Test di Ljung-Box
ljungbox_test(merged_data[["SPY_LogReturn"]], col_name = "SPY_LogReturn")
# Ljung-Box test per SPY_LogReturn (max_lag = 10):
#            lag   lb_stat   lb_pvalue
# X-squared    1  2.151626 0.142418961
# X-squared1   2  5.436812 0.065979849
# X-squared2   3 11.668729 0.008608552
# X-squared3   4 13.494766 0.009095025
# X-squared4   5 13.494803 0.019157864
# X-squared5   6 14.136206 0.028151125
# X-squared6   7 14.294220 0.046189081
# X-squared7   8 14.502604 0.069569836
# X-squared8   9 14.574400 0.103309632
# X-squared9  10 14.775374 0.140468457
##################################################################################################################################################