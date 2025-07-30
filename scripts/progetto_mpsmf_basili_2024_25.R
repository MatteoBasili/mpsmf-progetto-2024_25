################################################### Libraries ###################################################
if (!require("ggcorrplot")) {
  install.packages("ggcorrplot")
  library(ggcorrplot)
}
#############################################################################################################################

################################################### Environmental Setting ###################################################
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
#############################################################################################################################

################################################### Calcolo dei Rendimenti Logaritmici Giornalieri (Percentuali) ############
# Percorso della cartella dati
data_folder <- "../data"

# Elenco dei ticker
tickers <- c('SPY', 'AAPL', 'UNH', 'JPM', 'AMZN', 'XOM', 'IEF')

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

# Ordina per data e opzionale: rimuovi righe senza dati
merged_data <- merged_data %>%
  arrange(Date) %>%
  drop_na(Date) %>%
  mutate(Index = row_number()) %>%
  select(Index, everything())

# Controlla la struttura finale
glimpse(merged_data)

# Salva il dataset completo
write_csv(merged_data, file.path(data_folder, "merged_data_with_log_returns.csv"))
#############################################################################################################################

################################################### Costruzione della Matrice di Varianza-Covarianza Empirica ############
# Estrai solo le colonne dei rendimenti logaritmici
log_return_cols <- merged_data %>%
  select(ends_with("_LogReturn"))

# Rimuovi eventuali NA (prima riga)
log_return_clean <- na.omit(log_return_cols)

# Calcola la matrice di varianza-covarianza (Σ)
sigma_matrix <- cov(log_return_clean)

# Mostra la matrice
print("Matrice di varianza-covarianza dei rendimenti logaritmici (%):")
print(round(sigma_matrix, 4))

# Visualizza la matrice di varianza-covarianza con una heatmap
ggcorrplot(sigma_matrix, lab = TRUE, lab_size = 3, type = "lower",
           title = "Matrice di Covarianza dei Rendimenti Logaritmici",
           colors = c("blue", "white", "red"))

# Calcola la matrice di correlazione
corr_matrix <- cor(log_return_clean)

# Visualizza la matrice di correlazione con una heatmap
ggcorrplot(corr_matrix, lab = TRUE, lab_size = 3, type = "lower",
           title = "Matrice di Correlazione dei Rendimenti Logaritmici",
           colors = c("blue", "white", "red"))
#############################################################################################################################