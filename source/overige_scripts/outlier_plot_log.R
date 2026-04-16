library(ggplot2)
library(dplyr)
library(patchwork) # Optioneel, maar fantastisch om plots naast elkaar te zetten

# STAP 1: Voeg een 'rij_id' toe aan je data voor de Y-as
data_voor_plot <- test2 %>%
  arrange(TU_sum_s) %>%               # Sorteer op grootte (geeft het mooiste effect)
  mutate(rij_id = row_number())  # Maak een volgnummer aan

# STAP 2: Plot de ongetransformeerde data (RAW)
plot_raw <- ggplot(data_voor_plot, aes(x = TU_sum_s, y = rij_id)) +
  geom_point(color = "steelblue", alpha = 0.6, size = 2) +
  theme_minimal() +
  labs(
    title = "Ongetransformeerd (TU_sum_s)",
    subtitle = "Zoek naar losse stippen ver naar rechts",
    x = "Concentratie Totaal Stikstof",
    y = "Volgorde (Rij ID)"
  )

# STAP 3: Plot de log-getransformeerde data
plot_log <- ggplot(data_voor_plot, aes(x = TU_sum_log, y = rij_id)) +
  geom_point(color = "darkgreen", alpha = 0.6, size = 2) +
  theme_minimal() +
  labs(
    title = "Log-getransformeerd (n_t_log)",
    subtitle = "Kijk hoe de extreme waarden naar de groep trekken",
    x = "Log(Totaal Stikstof)",
    y = "" # Y-as label weglaten voor de netheid als ze naast elkaar staan
  )

# STAP 4: Zet ze naast elkaar (vereist het package 'patchwork')
# Heb je patchwork niet? Run dan gewoon plot_raw en plot_log apart.
plot_raw + plot_log
