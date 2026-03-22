# ==============================================================================
# PROJECT: PLANT PHYSIOLOGY - CHLOROPHYLL CONTENT (SPAD)
# AUTHOR:  Gabriele
# DATE:    2026
# SYSTEM:  Temporal ANOVA + Tukey HSD per Date + Jitter Distribution
# ==============================================================================

library(tidyverse)
library(readxl)
library(agricolae)
library(ggrepel)
library(lubridate)

# --- 1. SETUP E CARICAMENTO ---
# Colloca il file Excel in "data/spad_data.xlsx"
file_path  <- "data/spad_data.xlsx"
sheet_name <- "Sheet1"

if (!file.exists(file_path)) {
  stop("Errore: File non trovato in 'data/spad_data.xlsx'")
}

dati_grezzi <- read_excel(file_path, sheet = sheet_name)

# Forziamo i nomi delle colonne per coerenza interna
# Ordine atteso: Data, Trattamento, Valore SPAD
colnames(dati_grezzi)[1:3] <- c("Date", "Treatment", "SPAD")

# --- 2. DATA CLEANING & OUTLIER REMOVAL ---
dati_clean <- dati_grezzi %>%
  mutate(
    # Pulizia data e conversione numerica (gestione virgole/punti)
    Date = as.Date(floor_date(as.POSIXct(Date), "day")), 
    Treatment = as.factor(Treatment),
    SPAD = as.numeric(gsub(",", ".", as.character(SPAD)))
  ) %>%
  drop_na(SPAD, Date) %>%
  # Rimozione Outlier basata su IQR per ogni gruppo
  group_by(Treatment, Date) %>%
  mutate(
    Q1 = quantile(SPAD, 0.25), Q3 = quantile(SPAD, 0.75), IQR = Q3 - Q1,
    Outlier = SPAD < (Q1 - 1.5 * IQR) | SPAD > (Q3 + 1.5 * IQR)
  ) %>%
  filter(!Outlier) %>%
  ungroup()

# --- 3. ANALISI STATISTICA TEMPORALE ---
date_rilievi <- sort(unique(dati_clean$Date))
y_ceiling    <- max(dati_clean$SPAD, na.rm = TRUE) * 1.15
tabella_lettere <- data.frame()

for (d in as.character(date_rilievi)) {
  sub_data <- dati_clean %>% filter(Date == as.Date(d))
  
  if(length(unique(sub_data$Treatment)) > 1 && sd(sub_data$SPAD, na.rm = TRUE) > 0) {
    modello <- aov(SPAD ~ Treatment, data = sub_data)
    hsd <- HSD.test(modello, "Treatment", group = TRUE)
    
    let <- hsd$groups %>% 
      rownames_to_column("Treatment") %>% 
      rename(Letter = groups) %>% 
      mutate(Date = as.Date(d), Pos_Y = y_ceiling)
    
    tabella_lettere <- rbind(tabella_lettere, let)
  }
}

# Calcolo medie e deviazione standard per il plotting
medie_spad <- dati_clean %>%
  group_by(Treatment, Date) %>%
  summarise(Mean = mean(SPAD), SD = sd(SPAD), .groups = 'drop')

# --- 4. VISUALIZZAZIONE AVANZATA ---
p_spad <- ggplot(medie_spad, aes(x = Date, y = Mean, color = Treatment, fill = Treatment)) +
  # Background: Jitter dei dati grezzi per mostrare la variabilità reale
  geom_jitter(data = dati_clean, aes(y = SPAD), width = 0.2, alpha = 0.1, size = 0.5, color = "grey30") +
  # Fascia di errore (Standard Deviation)
  geom_ribbon(aes(ymin = Mean - SD, ymax = Mean + SD), alpha = 0.15, color = NA) +
  # Linee di trend e punti halo
  geom_line(linewidth = 1.2) + 
  geom_point(size = 3.5, shape = 21, color = "white", stroke = 0.8) +
  # Lettere Tukey HSD posizionate dinamicamente al "soffitto" del grafico
  geom_text_repel(data = tabella_lettere, aes(label = Letter, y = Pos_Y), 
                  direction = "y", segment.size = 0, size = 4.5, fontface = "bold",
                  show.legend = FALSE, box.padding = 0.1) +
  
  labs(
    title = "Evoluzione Indice SPAD (Contenuto Clorofilla)", 
    y = "SPAD Value", 
    x = "",
    subtitle = "Analisi ANOVA + Tukey HSD per singola data di rilievo"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey94", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5)
  ) +
  # Utilizzo di una palette dinamica per adattarsi a qualsiasi numero di tesi
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  scale_x_date(breaks = date_rilievi, date_labels = "%d %b") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.25)))

print(p_spad)

# Export
ggsave("SPAD_Physiological_Trend.png", p_spad, width = 10, height = 7, dpi = 300)