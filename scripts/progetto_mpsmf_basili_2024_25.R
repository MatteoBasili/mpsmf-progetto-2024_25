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
  rename(Rf_Annual_LogReturn = `3 Mo`) %>%
  mutate(Rf_Daily_LogReturn = 100 * (log(1 + Rf_Annual_LogReturn / 100) / 251)) %>%  # log-return giornaliero in % 
  arrange(Date)

# Visualizza i primi valori
print(head(risk_free_df, 10))

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
    geom_line(color = "steelblue") +
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
    geom_line(size = 1) +
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
    geom_line(data = filter(plot_df, Tipo == "AdjClose"),
              aes(color = LegendaTipo, linetype = LegendaTipo), size = 1) +
    geom_line(data = filter(plot_df, Tipo %in% c("Regression", "LOESS")),
              aes(color = LegendaTipo, linetype = LegendaTipo), size = 1) +
    geom_vline(xintercept = split_index, linetype = "solid", color = "darkgrey", size = 1) +
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

# Poiché tutti i p-value per le variabili del test di Ljung-Box sono maggiori di α = 0.01 (in realtà anche di α = 0.05 e α = 0.10),
# non abbiamo evidenza statistica sufficiente per rifiutare l'ipotesi nulla di assenza di autocorrelazione fino al lag specificato.

# Ovviamente, sottolineiamo ancora come il test non conferma la scorrelazione, ma piuttosto che non c'è evidenza sufficiente per dimostrare
# il contrario (come detto più volte a lezione: l'assenza di prove a un processo non implica necessariamente innocenza).

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

cov_matrix <- calculate_cov_matrix(merged_data, risky_logreturn_cols)

# Mostra la matrice
print("Matrice di varianza-covarianza dei rendimenti logaritmici (%):")
print(round(cov_matrix, 4))

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
# H_0: Le matrici di covarianza del modello completo e del modello ridotto SONO uguali;
# H_1: Le matrici di covarianza del modello completo e del modello ridotto NON sono uguali;

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
  cat(sprintf("Test statistic: %f\nP-value: %g\n", lr_stat, p_value))
  
  # Conclusione
  if (p_value > alpha) {
    cat(sprintf("\n** CONCLUSIONE: p-value > %.2f **\nNon rifiutiamo l'ipotesi nulla con un livello di significatività del %d%%\n→ Le matrici di covarianza del modello completo e del modello ridotto SONO uguali.\n\n",
                alpha, alpha * 100))
  } else {
    cat(sprintf("\n** CONCLUSIONE: p-value <= %.2f **\nRifiutiamo l'ipotesi nulla al livello di significatività del %d%%\n→ Le matrici di covarianza del modello completo e del modello ridotto NON sono uguali.\n\n",
                alpha, alpha * 100))
  }
}

# Esegue il test
lr_test(log_return_clean, alpha = 0.01)

# Alla luce dei risultati, il test LR rileva differenze significative tra le matrici di covarianza dei modelli considerati.
# Pertanto, non assumiamo l’indipendenza tra le serie.
##################################################################################################################################################
