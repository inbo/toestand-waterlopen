library(dplyr)
library(mgcv)

# Maak binaire variabelen (factoren) voor aanwezigheid en winst
analyse_finaal_cat <- analyse_finaal_clean %>%
  mutate(
    # Is er überhaupt natuur in het afstroomgebied?
    has_PA = as.factor(ifelse(mean_PA_cover > 0, "Ja", "Nee")),

    # Is er natuur bijgekomen gedurende de meetperiode?
    has_gain = as.factor(ifelse(rate_of_PA_gain > 0, "Ja", "Nee"))
  )

# Even checken hoe de verdeling is (altijd handig voor je resultaten-sectie!)
table(analyse_finaal_cat$has_PA)
table(analyse_finaal_cat$has_gain)

model_gain_cat <- gam(
  trend_pct_jaar ~
    # 1. Het vaste effect van de categorie (ligt de trend gemiddeld hoger bij "Ja"?)
    has_gain +

    # 2. De interactie: een aparte vloeiende curve voor MMIF per categorie ("Ja" en "Nee")
    s(initiele_mmif, by = has_gain) +

    # 3. De vertrouwde controles
    initiele_PA_cover +
    # log_oppervlakte +
    lengte_tijdreeks + groep +

    # 4. De random effecten
    s(owl, bs = "re") +
    s(bekken, bs = "re"),

  family = scat(), # We behouden de robuuste verdeling tegen uitschieters

  data = analyse_finaal_cat,
  method = "REML"
)

summary(model_gain_cat)

library(ggplot2)
library(ggeffects)

# ==============================================================================
# 1. BEREKEN VOORSPELLINGEN
# ==============================================================================
# We vragen het model om de voorspelde trend te berekenen voor de héle range
# van de initiële MMIF (x-as), opgesplitst voor beken MET en ZONDER winst (groepen).
voorsp_cat <- ggpredict(
  model_gain_cat,
  terms = c("initiele_mmif [all]", "has_gain")
)

# ==============================================================================
# 2. MAAK DE PLOT
# ==============================================================================
# We gebruiken duidelijke, contrasterende kleuren: rood/oranje voor "Nee", groen voor "Ja"
plot_cat <- ggplot(voorsp_cat, aes(x = x, y = predicted, color = group, fill = group)) +

  # Nul-lijn ter referentie (boven nul = herstel, onder nul = achteruitgang)
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.8) +

  # De schaduwbanden (betrouwbaarheidsintervallen)
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +

  # De GAM-curves zelf
  geom_line(linewidth = 1.2) +

  # Kleuren en legenda instellen
  scale_color_manual(name = "Natuurwinst in stroomgebied?", values = c("Nee" = "#d95f02", "Ja" = "#1b9e77")) +
  scale_fill_manual(name = "Natuurwinst in stroomgebied?", values = c("Nee" = "#d95f02", "Ja" = "#1b9e77")) +

  # Labels
  labs(
    title = "Verschil in Ecologisch Herstel: Wel vs. Geen Natuurwinst",
    subtitle = "Vloeiende GAM-curves per categorie, afhankelijk van de startpositie",
    x = "Initiële Kwaliteit (MMIF)",
    y = "Ecologische Trend (% verandering/jaar)"
  ) +

  # Thema
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

# Bekijk de plot
print(plot_cat)

# Het GAM model voor de aanwezigheid van beschermd gebied
model_PA_cat <- gam(
  trend_pct_jaar ~
    # 1. Het vaste effect (gemiddeld verschil tussen wel/geen natuur)
    has_PA +

    # 2. De interactie: vloeiende curves per categorie
    s(initiele_mmif, by = has_PA) +

    # 3. Controles (Let op: PA_cover is hier vervangen door PA_gain)
    rate_of_PA_gain + lengte_tijdreeks + groep +

    # 4. Random effecten voor de ruimtelijke/hiërarchische structuur
    s(owl, bs = "re") +
    s(bekken, bs = "re"),

  family = scat(), # Onze robuuste 'heavy tail' verdeling

  data = analyse_finaal_cat,
  method = "REML"
)

# Bekijk de statistische resultaten
summary(model_PA_cat)

library(ggplot2)
library(ggeffects)

# 1. Voorspellingen berekenen voor de plot
voorsp_PA_cat <- ggpredict(
  model_PA_cat,
  terms = c("initiele_mmif [all]", "has_PA")
)

# 2. De Plot bouwen
plot_PA_cat <- ggplot(voorsp_PA_cat, aes(x = x, y = predicted, color = group, fill = group)) +

  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1.2) +

  # We gebruiken hier blauw en rood/oranje om de plot te onderscheiden van de 'winst' plot
  scale_color_manual(name = "Natuur in stroomgebied?", values = c("Nee" = "#d95f02", "Ja" = "#7570b3")) +
  scale_fill_manual(name = "Natuur in stroomgebied?", values = c("Nee" = "#d95f02", "Ja" = "#7570b3")) +

  labs(
    title = "Effect van Natuuraanwezigheid op Ecologisch Herstel",
    subtitle = "Vloeiende GAM-curves per categorie, afhankelijk van de startpositie",
    x = "Initiële Kwaliteit (MMIF)",
    y = "Ecologische Trend (% verandering/jaar)"
  ) +

  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

print(plot_PA_cat)
