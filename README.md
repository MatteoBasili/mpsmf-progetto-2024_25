# 💡 Progetto MPSMF – A.A. 2024/2025

**Corso:** Metodi Probabilistici e Statistici per i Mercati Finanziari (MPSMF)  
**Studente:** Matteo Basili  
**Professore:** Roberto Monte  

---

## 📝 Descrizione del Progetto

### 📌 Obiettivi

L’obiettivo del progetto è quello di **valutare la performance di un portafoglio multi-asset composto da più titoli azionari e una componente obbligazionaria**, confrontandola con un portafoglio **benchmark** costituito da un **ETF sullo S&P500** combinato con la stessa componente di **bond**.

### 📄 Passi del progetto

1. **Stima GARCH univariati per titoli rischiosi**  
Per ciascun titolo azionario è stato stimato un modello GARCH per catturare la dinamica della volatilità condizionata.
I parametri sono stati stimati su un periodo di training, utilizzando i log-rendimenti giornalieri.

2. **Analisi Treasury e tasso risk-free medio**  
È stato stimato il tasso privo di rischio medio partendo dai rendimenti dei Treasury a breve scadenza, da utilizzare nel calcolo dell’eccesso di rendimento e nei successivi indicatori di performance.

3. **Creazione del dataset finale di test**  
I dati sono stati aggregati in un unico dataset contenente rendimenti azionari, rendimento risk-free e serie obbligazionarie, destinato alla fase di validazione e confronto.

4. **Stima DCC-GARCH / costruzione matrice di covarianza**  
È stato implementato un modello DCC-GARCH per stimare la **correlazione dinamica** tra i titoli, al fine di costruire una matrice di covarianza condizionata.

5. **Costruzione portafoglio titoli + bond**  
Il portafoglio è stato costruito ottimizzando i pesi dei titoli azionari e della componente obbligazionaria, con vincoli di somma a 1.
E' stata considerata la strategia dello Sharpe massimo.

6. **Costruzione benchmark ETF + bond**  
Il benchmark è costituito da un ETF che replica l’indice S&P500 combinato con la stessa quota obbligazionaria del portafoglio oggetto di analisi, per garantire confrontabilità in termini di rischio complessivo.

7. **Confronto delle performance**  
Le due strategie sono state confrontate in base a indicatori di performance classici:

   - **Sharpe ratio**  
   - **Volatilità annualizzata**  
   - **Rendimento medio out-of-sample**  

   Sono inoltre stati analizzati gli andamenti cumulati dei rendimenti.

---

## 📁 Struttura del repository

- `data/` → Dataset storici, serie dei rendimenti utilizzati e output  
- `scripts/` → Codice per scaricare i dati di interesse e analizzarli  
