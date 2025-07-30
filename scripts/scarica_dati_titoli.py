import os
import yfinance as yf
from datetime import datetime, timedelta

# Cartella per i dati
data_folder = os.path.join(os.path.dirname(__file__), '..', 'data')
os.makedirs(data_folder, exist_ok=True)

# Lista dei ticker da scaricare
tickers = ['SPY', 'AAPL', 'UNH', 'JPM', 'AMZN', 'XOM', 'IEF']

# Date di inizio e fine
end_date = datetime.today()
start_date = end_date - timedelta(days=2*365)

# Itera su ciascun ticker
for ticker in tickers:
    # Scarica i dati per il ticker corrente
    data = yf.download(ticker, start=start_date.strftime('%Y-%m-%d'),
                       end=end_date.strftime('%Y-%m-%d'),
                       interval='1d', auto_adjust=False)
    # Seleziona solo le colonne desiderate e resetta l'indice
    data_filtered = data[["Open", "High", "Low", "Close", "Adj Close","Volume"]].reset_index()
    
    # Nome del file CSV
    csv_filename = os.path.join(data_folder, f'{ticker}_data.csv')
    
    # Salva il CSV senza indice
    data_filtered.to_csv(csv_filename, index=False)
    
    # **Elimina la seconda riga**
    with open(csv_filename, "r") as f:
        lines = f.readlines()

    # Riscrivi il file senza la seconda riga
    with open(csv_filename, "w") as f:
        f.writelines([lines[0]] + lines[2:])  # Salta la seconda riga (indice 1)
    
    print(f"File CSV salvato correttamente: {csv_filename}")
    
print("\n\nTutti i dati sono stati scaricati.")
