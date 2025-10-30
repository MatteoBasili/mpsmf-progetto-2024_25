# 💡 Progetto MPSMF – A.A. 2024/2025

**Corso:** Metodi Probabilistici e Statistici per i Mercati Finanziari (MPSMF)  
**Studente:** Matteo Basili  
**Professore:** Roberto Monte  

---

## 📝 Descrizione del Progetto

### 📌 Obiettivi

L’obiettivo del progetto è quello di valutare la performance di un portafoglio multi-asset composto da più titoli azionari e una componente obbligazionaria, confrontandola con un portafoglio benchmark costituito da un ETF sullo S&P500 combinato con la stessa componente di bond.
L’analisi è stata condotta su un periodo di out-of-sample, con l’obiettivo di analizzare la robustezza delle stime e la coerenza dei risultati in termini di rischio e rendimento.

### 📄 Passi del progetto

1. Stima GARCH univariati per titoli rischiosi
Per ciascun titolo azionario è stato stimato un modello GARCH(1,1) per catturare la dinamica della volatilità condizionata.
I parametri sono stati stimati su un periodo di training, utilizzando i log-rendimenti giornalieri.

2. Analisi Treasury e tasso risk-free medio
È stato stimato il tasso privo di rischio medio partendo dai rendimenti dei Treasury a breve scadenza, da utilizzare nel calcolo dell’eccesso di rendimento e nei successivi indicatori di performance.

3. Creazione del dataset finale di test
I dati sono stati aggregati in un unico dataset contenente rendimenti azionari, rendimento risk-free e serie obbligazionarie, destinato alla fase di validazione e confronto.

4. Stima DCC-GARCH / costruzione matrice di covarianza
È stato implementato un modello DCC-GARCH per stimare la correlazione dinamica tra i titoli, al fine di costruire una matrice di covarianza condizionata.
In alternativa, sono stati testati approcci più semplici (es. covarianza storica mobile) per valutarne la stabilità e l’impatto sui risultati.

5. Costruzione portafoglio titoli + bond
Il portafoglio è stato costruito ottimizzando i pesi dei titoli azionari e della componente obbligazionaria, con vincoli di non negatività e somma a 1.
Sono state considerate diverse strategie (es. varianza minima, Sharpe massimo).

6. Costruzione benchmark ETF + bond
Il benchmark è costituito da un ETF che replica l’indice S&P500 combinato con la stessa quota obbligazionaria del portafoglio oggetto di analisi, per garantire confrontabilità in termini di rischio complessivo.

7. Confronto delle performance
Le due strategie sono state confrontate in base a indicatori di performance classici:

- Sharpe ratio  
- Sortino ratio  
- Value at Risk (VaR)  
- Expected Shortfall (CVaR)  
- Volatilità annualizzata  
- Rendimento medio out-of-sample  

Sono inoltre stati analizzati gli andamenti cumulati dei rendimenti e la stabilità temporale del rischio.

8. Conclusioni economiche e statistiche
L’analisi finale discute i risultati ottenuti in termini di efficienza del portafoglio, capacità di diversificazione e coerenza con la teoria di portafoglio.
Vengono inoltre tratte considerazioni sul contributo del modello GARCH/DCC nella gestione del rischio e sulla possibile estensione del lavoro a orizzonti temporali differenti o a portafogli multi-asset globali.

---

## 📁 Struttura del repository

- `data/` → Dataset storici e serie dei rendimenti utilizzati
- `notebooks/` → Notebook Jupyter con il codice di stima e analisi
- `results/` → Output delle stime, grafici e tabelle di performance
- `plots/` → Visualizzazioni delle serie e dei risultati
- `src/` → Funzioni di supporto per modelli GARCH, DCC e metriche di performance  
