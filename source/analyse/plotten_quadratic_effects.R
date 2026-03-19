# Installeer ggeffects als je dat nog niet had: install.packages("ggeffects")
library(ggeffects)
library(ggplot2)

# STAP 1: Maak een tijdelijke versie van je model, puur voor de plot
# We halen je harde kolom (- jaar_s_sq) eruit, en zetten de wiskunde (+ I(jaar_s^2)) erin
mmif_plot_model <- update(mmif, . ~ . - jaar_s_sq + I(jaar_s^2))

# STAP 2: Bereken het conditionele effect
# ggeffects houdt nu netjes alle ándere variabelen (O2, P, N, etc.) constant op hun gemiddelde
effect_jaar <- ggpredict(mmif_plot_model, terms = "jaar_s [all]")

# STAP 3: Plot de curve!
plot(effect_jaar) +
  theme_minimal() +
  labs(
    title = "Kwadratisch effect van Jaar op MMIF",
    x = "Jaar (Gestandaardiseerd)",
    y = "Voorspelde MMIF"
  ) +
  # Optioneel: pas de kleuren aan naar wens
  scale_color_manual(values = "darkblue") +
  scale_fill_manual(values = "lightblue")


# STAP 1: Maak een tijdelijke versie van je model, puur voor de plot
# We halen je harde kolom (- jaar_s_sq) eruit, en zetten de wiskunde (+ I(jaar_s^2)) erin
ept_plot_model <- update(ept, . ~ . - o2_verz_s_sq + I(o2_verz_s^2))

# STAP 2: Bereken het conditionele effect
# ggeffects houdt nu netjes alle ándere variabelen (O2, P, N, etc.) constant op hun gemiddelde
effect_jaar <- ggpredict(ept_plot_model, terms = "o2_verz_s [all]")

# STAP 3: Plot de curve!
plot(effect_jaar) +
  theme_minimal() +
  labs(
    title = "Kwadratisch effect van Jaar op MMIF",
    x = "Jaar (Gestandaardiseerd)",
    y = "Voorspelde MMIF"
  ) +
  # Optioneel: pas de kleuren aan naar wens
  scale_color_manual(values = "darkblue") +
  scale_fill_manual(values = "lightblue")
