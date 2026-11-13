# Installeer indien nodig:
# install.packages("ggeffects")

library(ggeffects)
library(ggplot2)

# ==============================================================================
# 1. VOORSPELLINGEN BEREKENEN UIT HET MODEL
# ==============================================================================
# We berekenen de voorspelde trend voor 3 niveaus van initiële MMIF:
# 0.3 (Slecht), 0.5 (Matig), 0.7 (Goed/Matig-Goed)
voorspellingen <- ggpredict(
  model_cover,
  terms = c("mean_PA_cover [all]", "initiele_mmif [0.3, 0.5, 0.7]")
)

# ==============================================================================
# 2. MOOIE GGPLOT MAKEN
# ==============================================================================
p_interactie <- ggplot(voorspellingen, aes(x = x, y = predicted, color = group, fill = group)) +

  # Nul-lijn (geen verandering) ter referentie
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", alpha = 0.7) +

  # Betrouwbaarheidsintervallen (95% CI ribbons)
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, color = NA) +

  # De trendlijnen
  geom_line(linewidth = 1.2) +

  # Kleurenpalet (oranje voor slecht, paars voor matig, groen/blauw voor goed)
  scale_color_manual(
    name = "Initiële Kwaliteit (MMIF)",
    values = c("#d95f02", "#7570b3", "#1b9e77"),
    labels = c("Slecht (MMIF = 0.3)", "Matig (MMIF = 0.5)", "Goed (MMIF = 0.7)")
  ) +
  scale_fill_manual(
    name = "Initiële Kwaliteit (MMIF)",
    values = c("#d95f02", "#7570b3", "#1b9e77"),
    labels = c("Slecht (MMIF = 0.3)", "Matig (MMIF = 0.5)", "Goed (MMIF = 0.7)")
  ) +

  # As-labels en titels
  labs(
    title = "Effect van Natuurbescherming op Ecologisch Herstel",
    subtitle = "Interactie tussen beschermingsgraad en initiële toestand van de waterloop",
    x = "Gemiddelde bedekking beschermd gebied in stroomgebied (% PA cover)",
    y = "Temporele trend MMIF (% verandering per jaar)",
    caption = "Gecorrigeerd voor riviergrootte, tijdreekslengte en ruimtelijke coördinaten."
  ) +

  # Styling voor een strakke publicatie-look
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold")
  )

# Toon de figuur
print(p_interactie)

# Opslaan in hoge resolutie voor je thesis
ggsave("figuur_interactie_pa_initieel.png", p_interactie, width = 8, height = 6, dpi = 300)

library(marginaleffects)

# Genereer een raster van voorspellingen over beide variabelen
predictions_grid <- predictions(
  gam_model_cover,
  newdata = datagrid(
    mean_PA_cover = seq(0, 100, length.out = 50),
    initiele_mmif = seq(0.1, 0.9, length.out = 50)
  )
)

# Plot als een heatmap met iso-lijnen (contour)
ggplot(predictions_grid, aes(x = mean_PA_cover, y = initiele_mmif, fill = estimate)) +
  geom_tile() +
  geom_contour(aes(z = estimate), color = "white", alpha = 0.5) +

  # Gradient van blauw/rood (achteruitgang) naar groen/geel (herstel)
  scale_fill_viridis_c(
    option = "viridis",
    name = "Trend (%/jaar)"
  ) +

  labs(
    title = "Ecologische Trend als Oppervlak",
    x = "Gemiddeld Beschermd Gebied (%)",
    y = "Initiële MMIF Score",
    subtitle = "Hogere waarden (geel/groen) duiden op sneller biologisch herstel"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

# Installeer patchwork als je dit nog niet hebt:
# install.packages("patchwork")

library(ggplot2)
library(ggeffects)
library(patchwork)

# ==============================================================================
# 1. BEREKEN VOORSPELLINGEN VOOR BEIDE MODELLEN
# ==============================================================================
# Voorspelling voor Model 1: PA Cover (Gemiddelde bescherming)
voorsp_cover <- ggpredict(
  model_cover,
  terms = c("mean_PA_cover [all]", "initiele_mmif [0.3, 0.5, 0.7]")
)

# Voorspelling voor Model 2: PA Gain (Uitbreiding van bescherming)
voorsp_gain <- ggpredict(
  model_gain,
  terms = c("rate_of_PA_gain [all]", "initiele_mmif [0.3, 0.5, 0.7]")
)

# ==============================================================================
# 2. MAAK DE TWEE APARTE PLOTS
# ==============================================================================
# Kleurenpalet hergebruiken
mijn_kleuren <- c("0.3" = "#d95f02", "0.5" = "#7570b3", "0.7" = "#1b9e77")
mijn_labels <- c("Slecht (0.3)", "Matig (0.5)", "Goed (0.7)")

# Plot A: PA Cover
plot_cover <- ggplot(voorsp_cover, aes(x = x, y = predicted, color = group, fill = group)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1) +
  scale_color_manual(name = "Initiële Kwaliteit", values = mijn_kleuren, labels = mijn_labels) +
  scale_fill_manual(name = "Initiële Kwaliteit", values = mijn_kleuren, labels = mijn_labels) +
  labs(
    title = "A. Effect van Totale Bescherming",
    x = "Gemiddeld Beschermd Gebied (% in ASG)",
    y = "Trend in Kwaliteit (% verandering/jaar)"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

# Plot B: PA Gain
plot_gain <- ggplot(voorsp_gain, aes(x = x, y = predicted, color = group, fill = group)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1) +
  scale_color_manual(name = "Initiële Kwaliteit", values = mijn_kleuren, labels = mijn_labels) +
  scale_fill_manual(name = "Initiële Kwaliteit", values = mijn_kleuren, labels = mijn_labels) +
  labs(
    title = "B. Effect van Natuuruitbreiding",
    x = "Winst in Beschermd Gebied (% toename/jaar)",
    y = "Trend in Kwaliteit (% verandering/jaar)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.y = element_blank() # Verberg de y-as titel hier voor een rustiger beeld
  )

# ==============================================================================
# 3. COMBINEER MET PATCHWORK
# ==============================================================================
# Het '+' teken zet ze naast elkaar.
# plot_layout(guides = "collect") zorgt dat ze 1 gezamenlijke legende delen!
figuur_gecombineerd <- plot_cover + plot_gain +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom") # Zet de legende netjes onderaan

# Bekijk het resultaat
print(figuur_gecombineerd)

# Sla op in een brede resolutie (ideaal voor een A4-pagina in Word)
ggsave("figuur_PA_effecten_gecombineerd.png", figuur_gecombineerd, width = 10, height = 5, dpi = 300)
