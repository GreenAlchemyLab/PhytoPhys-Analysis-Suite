# ==============================================================================
# PROJECT: PLANT PHYSIOLOGY - MULTI-PARAMETER ANALYSIS (MPM-100)
# AUTHOR:  Gabriele
# DATE:    2026
# SYSTEM:  Time-series with Tukey HSD Statistics & Outlier Removal
# ==============================================================================

library(tidyverse)
library(readxl)
library(agricolae) # Per il test di Tukey
library(ggpubr)
library(ggrepel)

# --- 1. SETUP E CARICAMENTO ---
file_path <- "data/mpm_data.xlsx"
sheet_name <- "Sheet1"

if (!file.exists(file_path)) {
  stop("Errore: File non trovato in 'data/mpm_data.xlsx'")
}

dati_grezzi <- read_excel(file_path, sheet = sheet_name)

# Mappa dei nomi completi per i grafici
mappa_titoli <- c(
  "ChlM"  = "Chlorophyll Index (ChlM)",
  "FlvM"  = "Flavonol Index (FlvM)",
  "AnthM" = "Anthocyanin Index (AnthM)",
  "NFI"   = "Nitrogen Flavonol Index (NFI)"
)

# --- 2. DATA CLEANING & OUTLIER REMOVAL ---
dati_long <- dati_grezzi %>%
  mutate(Date = as.Date(Data), Treatment = as.factor(Tesi)) %>%
  pivot_longer(cols = any_of(names(mappa_titoli)), names_to = "Variable", values_to = "Value") %>%
  mutate(Value = as.numeric(as.character(Value))) %>%
  drop_na(Value) %>%
  # Rimozione Outlier basata su IQR (Interquartile Range)
  group_by(Treatment, Date, Variable) %>%
  mutate(
    Q1 = quantile(Value, 0.25), Q3 = quantile(Value, 0.75), IQR = Q3 - Q1,
    Outlier = Value < (Q1 - 1.5 * IQR) | Value > (Q3 + 1.5 * IQR)
  ) %>%
  filter(!Outlier) %>%
  ungroup()

# --- 3. PIPELINE STATISTICA E GRAFICA ---
date_list <- sort(unique(dati_long$Date))
plot_list <- list()

for (v in unique(dati_long$Variable)) {
  
  var_subset <- dati_long %>% filter(Variable == v)
  # Calcolo del limite superiore per il posizionamento delle lettere statistiche
  y_ceiling <- max(var_subset$Value, na.rm = TRUE) * 1.15
  
  # A. Analisi della Varianza (ANOVA) e Test di Tukey (HSD)
  tukey_letters <- data.frame()
  for (d in as.character(date_list)) {
    sub_data <- var_subset %>% filter(Date == as.Date(d))
    
    if(length(unique(sub_data$Treatment)) > 1 && sd(sub_data$Value) > 0) {
      model <- aov(Value ~ Treatment, data = sub_data)
      hsd <- HSD.test(model, "Treatment", group = TRUE)
      
      # Estrazione lettere
      letters <- hsd$groups %>% 
        rownames_to_column("Treatment") %>% 
        rename(Letter = groups) %>% 
        mutate(Date = as.Date(d), Pos_Y = y_ceiling)
      
      tukey_letters <- rbind(tukey_letters, letters)
    }
  }
  
  # B. Calcolo Medie per il plot
  plot_stats <- var_subset %>% 
    group_by(Treatment, Date) %>%
    summarise(Mean = mean(Value), SD = sd(Value), .groups = 'drop')
  
  # C. Costruzione Grafico
  p <- ggplot(plot_stats, aes(x = Date, y = Mean, color = Treatment, fill = Treatment)) +
    # Background: Dati grezzi (jitter) e Area di Errore
    geom_jitter(data = var_subset, aes(y = Value), 
                width = 0.2, alpha = 0.08, size = 0.5, color = "grey20") +
    geom_ribbon(aes(ymin = Mean - SD, ymax = Mean + SD), alpha = 0.15, color = NA) +
    # Linee di tendenza e punti
    geom_line(linewidth = 1.2) + 
    geom_point(size = 3, shape = 21, color = "white", stroke = 0.7) +
    # Inserimento lettere statistiche
    geom_text_repel(
      data = tukey_letters, aes(label = Letter, y = Pos_Y, x = Date),
      direction = "y", segment.size = 0, size = 4, fontface = "bold", 
      show.legend = FALSE, box.padding = 0.1
    ) +
    # Estetica
    labs(title = mappa_titoli[v], y = "Index Value", x = "") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      panel.grid.major = element_line(color = "grey92", linewidth = 0.2), 
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black"),
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5)
    ) +
    scale_color_brewer(palette = "Set1") +
    scale_fill_brewer(palette = "Set1") +
    scale_x_date(breaks = date_list, date_labels = "%d %b") +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.35)))
  
  plot_list[[v]] <- p
}

# --- 4. OUTPUT ---
final_grid <- ggarrange(
  plotlist = plot_list, 
  ncol = 2, nrow = 2, 
  common.legend = TRUE, legend = "bottom"
)

print(final_grid)
ggsave("MPM_Physiological_Trends.png", final_grid, width = 12, height = 10, dpi = 300)