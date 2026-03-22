# 🌿 PhytoPhys Analysis Suite
> **Framework R per la diagnostica avanzata della fisiologia vegetale e l'analisi nutrizionale.**

Questa suite di script è stata progettata per automatizzare il processing dei dati derivanti da sensori portatili, trasformando l'output grezzo in grafici ad alta risoluzione pronti per report scientifici o pubblicazioni.

---

## 🛠️ Gli Strumenti (Scripts)

| Script | Sensore Target | Parametri Analizzati | Key Feature |
| :--- | :--- | :--- | :--- |
| **`plant_nutrients.R`** | LaquaTwin | $NO_3^-$ e $K^+$ | Gestione automatica dei decimali e grafico a doppio asse Y. |
| **`plant_multiparameter.R`**| MPM-100 | ChlM, FlvM, AnthM, NFI | Test di Tukey (HSD) e rimozione outlier tramite $IQR$. |
| **`plant_chlorophyll.R`** | SPAD | Indice di Clorofilla | Visualizzazione "Ghost Points" per mostrare la variabilità reale. |

---

## 📉 Workflow di Analisi
Il sistema segue una pipeline standardizzata per garantire l'integrità del dato:

1.  **Data Cleaning:** Lo script corregge i separatori decimali e filtra i valori anomali (outlier) utilizzando il metodo dell'**Interquartile Range ($IQR$)**.
2.  **Statistica Temporale:** Per ogni data di rilievo viene eseguita un'**ANOVA** seguita dal test di **Tukey HSD** per l'assegnazione automatica delle lettere di significatività.
3.  **Rendering Grafico:** Generazione di file `.png` (300 DPI) con temi minimali e palette di colori ottimizzate per la distinzione delle tesi.
