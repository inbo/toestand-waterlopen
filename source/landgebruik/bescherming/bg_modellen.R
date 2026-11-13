library(ggplot2)
library(ggeffects)
library(patchwork)
analyse_finaal <- readRDS(here::here("data", "verwerkt", "analyse_finaal_modellen.rds")) %>%
  mutate(owl = as.factor(owl),
         bekken = as.factor(bekken))

# Bereken de initiële bescherming (PA in het startjaar)
initiele_pa <- pa_afstroomgebied_tijdreeks %>%
  group_by(meetplaats) %>%
  arrange(meet_jaar) %>%
  slice_head(n = 1) %>% # Pak het allereerste jaar van de reeks
  select(meetplaats, initiele_PA_cover = beschermd_pct)

# Voeg toe aan je finale dataset
analyse_finaal <- analyse_finaal %>%
  left_join(initiele_pa, by = "meetplaats")

# Model A: Effect van de gemiddelde beschermingsgraad
model_cover <- glmmTMB(
  trend_pct_jaar ~ mean_PA_cover * initiele_mmif + log_oppervlakte + lengte_tijdreeks + (1|owl) + (1|bekken) + groep,
  data = analyse_finaal
)

# Model B: Effect van de natuuruitbreiding (winst)
model_gain <- glmmTMB(
  trend_pct_jaar ~ rate_of_PA_gain * initiele_mmif + initiele_PA_cover + log_oppervlakte + lengte_tijdreeks + (1|owl) + (1|bekken) + groep,
  data = analyse_finaal)



# SAC test
model <- model_cover
# 1. Set glmmTMB to simulate conditionally on fitted REs
glmmTMB::set_simcodes(model$obj, val = "fix")
# 2. Run DHARMa as usual
res <- simulateResiduals(fittedModel = model)
glmmTMB::set_simcodes(model$obj, val = "random")

# analyse_finaal <- analyse_finaal %>%
#   filter(groep %in% c("beek", "kempen"))

locaties_match <- analyse_finaal %>%
  group_by(owl) %>%
  summarize(x = dplyr::first(X_coord), y = dplyr::first(Y_coord), .groups = "drop") %>%
  arrange(owl) # DHARMa sorteert groepen standaard op naam/factor level

# 3. Aggregeer residuen
res_grouped <- recalculateResiduals(res, group = analyse_finaal$owl)

# 4. De test
testSpatialAutocorrelation(res_grouped,
                           x = locaties_match$x,
                           y = locaties_match$y)

# Bekijk de statistische resultaten in je console!
print("--- RESULTATEN MODEL COVER ---")
summary(model_cover)
print("--- RESULTATEN MODEL GAIN ---")
summary(model_gain)
#

# Voorspellingen berekenen voor 3 klassen van initiële kwaliteit
voorsp_cover <- ggpredict(model_cover, terms = c("mean_PA_cover [all]", "initiele_mmif [0.3, 0.5, 0.7]"))
voorsp_gain <- ggpredict(model_gain, terms = c("rate_of_PA_gain [all]", "initiele_mmif [0.3, 0.5, 0.7]"))

# Vaste stijlelementen
mijn_kleuren <- c("0.3" = "#d95f02", "0.5" = "#7570b3", "0.7" = "#1b9e77")
mijn_labels <- c("Slecht (0.3)", "Matig (0.5)", "Goed (0.7)")

# Figuur A: PA Cover
plot_cover <- ggplot(voorsp_cover, aes(x = x, y = predicted, color = group, fill = group)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(name = "Initiële Kwaliteit (MMIF)", values = mijn_kleuren, labels = mijn_labels) +
  scale_fill_manual(name = "Initiële Kwaliteit (MMIF)", values = mijn_kleuren, labels = mijn_labels) +
  labs(
    title = "A. Effect van Gemiddelde Bescherming",
    x = "Gemiddeld Beschermd Gebied (% in ASG)",
    y = "Ecologische Trend (% verandering/jaar)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

# Figuur B: PA Gain
plot_gain <- ggplot(voorsp_gain, aes(x = x, y = predicted, color = group, fill = group)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(name = "Initiële Kwaliteit (MMIF)", values = mijn_kleuren, labels = mijn_labels) +
  scale_fill_manual(name = "Initiële Kwaliteit (MMIF)", values = mijn_kleuren, labels = mijn_labels) +
  labs(
    title = "B. Effect van Natuuruitbreiding",
    x = "Winst in Beschermd Gebied (% toename/jaar)",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

# Combineer beide figuren met patchwork
figuur_finaal <- plot_cover + plot_gain +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

print(figuur_finaal)
ggsave(here::here("data", "verwerkt", "figuur_PA_modellen.png"), figuur_finaal, width = 10, height = 5, dpi = 300)


# gam

library(mgcv)

# We gebruiken s() voor vloeiende curves, net als in de paper!
model_gain_gam <- gam(
  trend_pct_jaar ~
    # Tensor product smooth voor de interactie (het kromme oppervlak)
    te(rate_of_PA_gain, initiele_mmif) +
    s(initiele_PA_cover) +
    s(log_oppervlakte) +
    s(lengte_tijdreeks) +
    groep +
    s(owl, bs = "re") + # Dit is de manier om een (1|owl) random effect in een GAM te zetten
    s(bekken, bs = "re"),
  family = scat(), # Super robuust tegen afwijkingen

  data = analyse_finaal,
  method = "REML"
)

# Test het nieuwe model in DHARMa
res_gam <- simulateResiduals(model_gain_gam)
plot(res_gam)
model_gain_gam %>% summary

# 1. Verwijder de extreme, onrealistische wiskundige uitschieters
analyse_finaal_clean <- analyse_finaal %>%
  filter(
    trend_pct_jaar > -10,  # Haal de extreme dalers eruit
    trend_pct_jaar < 15    # Haal de wiskundig absurde stijgers eruit
  )

# Optioneel: Check hoeveel punten je bent kwijtgeraakt (dat zullen er maar een handvol zijn!)
nrow(analyse_finaal) - nrow(analyse_finaal_clean)

# 2. Draai je GAM op deze opgeschoonde dataset, MET de scat() family voor de overgebleven spreiding!
library(mgcv)

model_gain_gam_finaal <- gam(
  trend_pct_jaar ~
    te(rate_of_PA_gain, initiele_mmif) +
    initiele_PA_cover + log_oppervlakte + lengte_tijdreeks + groep +
    s(owl, bs = "re") +
    s(bekken, bs = "re"),

  family = scat(), # Super robuust tegen afwijkingen

  data = analyse_finaal_clean,
  method = "REML"
)

# 3. De ultieme DHARMa test
res_finaal <- simulateResiduals(model_gain_gam_finaal)
plot(res_finaal)

library(ggplot2)
library(ggeffects)

# 1. Voorspellingen berekenen (ggeffects filtert de random effecten hier automatisch netjes uit!)
voorsp_gam <- ggpredict(
  model_gain_gam_finaal,
  terms = c("rate_of_PA_gain [all]", "initiele_mmif [0.3, 0.5, 0.7]")
)

# 2. Vaste kleuren (voor consistentie met je eerdere plots)
mijn_kleuren <- c("0.3" = "#d95f02", "0.5" = "#7570b3", "0.7" = "#1b9e77")
mijn_labels <- c("Slecht (0.3)", "Matig (0.5)", "Goed (0.7)")

# 3. De plot bouwen
plot_gam_lijnen <- ggplot(voorsp_gam, aes(x = x, y = predicted, color = group, fill = group)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +

  # De schaduwbanden (betrouwbaarheidsintervallen)
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, color = NA) +

  # De lijnen (dit zijn nu curves in plaats van strakke rechte lijnen!)
  geom_line(linewidth = 1.2) +

  scale_color_manual(name = "Initiële Kwaliteit", values = mijn_kleuren, labels = mijn_labels) +
  scale_fill_manual(name = "Initiële Kwaliteit", values = mijn_kleuren, labels = mijn_labels) +

  labs(
    title = "Effect van Natuuruitbreiding op Ecologisch Herstel",
    subtitle = "GAM-voorspellingen met robuuste 'scat' verdeling",
    x = "Winst in Beschermd Gebied (% toename/jaar)",
    y = "Ecologische Trend (% verandering/jaar)"
  ) +

  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

print(plot_gam_lijnen)

#######################################
# test initiele bescherminsstatus
#####################################



# Model C: Effect van initiële bescherming
model_initiele_pa <- lm(
  trend_pct_jaar ~
    initiele_PA_cover * initiele_mmif +  # De nieuwe interactie
    log_oppervlakte + lengte_tijdreeks + X_coord + Y_coord,
  data = analyse_finaal
)

summary(model_initiele_pa)

library(ggplot2)
library(ggeffects)

# ==============================================================================
# 1. BEREKEN VOORSPELLINGEN VOOR HET NIEUWE MODEL
# ==============================================================================
# We gebruiken dezelfde 3 niveaus voor initiële kwaliteit (Slecht, Matig, Goed)
voorsp_init_pa <- ggpredict(
  model_initiele_pa,
  terms = c("initiele_PA_cover [all]", "initiele_mmif [0.3, 0.5, 0.7]")
)

# ==============================================================================
# 2. MAAK DE PLOT
# ==============================================================================
# Zelfde kleuren en labels voor visuele consistentie in je thesis
mijn_kleuren <- c("0.3" = "#d95f02", "0.5" = "#7570b3", "0.7" = "#1b9e77")
mijn_labels <- c("Slecht (0.3)", "Matig (0.5)", "Goed (0.7)")

plot_init_pa <- ggplot(voorsp_init_pa, aes(x = x, y = predicted, color = group, fill = group)) +

  # Nul-lijn ter referentie
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +

  # Betrouwbaarheidsintervallen
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, color = NA) +

  # De effectlijnen
  geom_line(linewidth = 1.2) +

  # Kleuren en legenda
  scale_color_manual(name = "Initiële Kwaliteit (MMIF)", values = mijn_kleuren, labels = mijn_labels) +
  scale_fill_manual(name = "Initiële Kwaliteit (MMIF)", values = mijn_kleuren, labels = mijn_labels) +

  # Labels
  labs(
    title = "C. Effect van Initiële Beschermingsstatus",
    subtitle = "Invloed van het beschermingsniveau bij de start van de meetreeks",
    x = "Initieel Beschermd Gebied in ASG (%)",
    y = "Ecologische Trend (% verandering/jaar)"
  ) +

  # Thema
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

# Bekijk het resultaat
print(plot_init_pa)

# Opslaan
ggsave(here::here("data", "verwerkt", "figuur_initiele_PA.png"), plot_init_pa, width = 7, height = 5, dpi = 300)
