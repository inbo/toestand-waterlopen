
#data laden uit mi_datasets_prep
n_total <- nrow(data4)

mi_nested <- data4 %>%
  # Eerst de scores naar 0-1 brengen
  mutate(across(c("ept", "swd", "nst", "tax", "mts"), ~ .x / 4)) %>%
  # Transformeren om exacte 0 en 1 te vermijden voor de Beta-familie
  mutate(across(c(mmif, ept, swd, nst, tax, mts),
                ~ (.x * (n_total - 1) + 0.5) / n_total)) %>%
  mutate(subset = case_when(
    statuut %in% c("Natuurlijk", "Sterk Veranderd") & groep == "beek"   ~ "nat_sv_beek",
    statuut %in% c("Natuurlijk", "Sterk Veranderd") & groep == "kempen" ~ "nat_sv_kempen",
    statuut %in% c("Natuurlijk", "Sterk Veranderd") & groep == "polder" ~ "nat_sv_polder",
    statuut %in% c("Natuurlijk", "Sterk Veranderd") & groep == "rivier" ~ "nat_sv_rivier",
    statuut == "Kunstmatig"                                            ~ "kunstmatig",
    type == "RtNt"                                                     ~ "rtnt",
    TRUE                                                               ~ "overig"
  )) %>%
  filter(subset != "overig")

head(mi_nested
     )
library(tidyverse)
library(sf)

# 1. Hoe vaak is elke meetplaats bezocht?
visit_freq <- mi_nested %>%
  group_by(subset, meetplaats) %>%
  summarise(n_visits = n(), .groups = "drop")

# Visualisatie van de continuïteit
ggplot(visit_freq, aes(x = n_visits, fill = subset)) +
  geom_histogram(binwidth = 1, color = "white") +
  facet_wrap(~subset) +
  labs(title = "Aantal bezoeken per meetplaats",
       x = "Aantal jaren bemonsterd", y = "Aantal locaties") +
  theme_minimal()

# Extraheer XY coördinaten uit de geom kolom
mi_coords <- mi_nested %>%
  mutate(x = st_coordinates(geom)[,1],
         y = st_coordinates(geom)[,2])

# Bereken het zwaartepunt van de monsters per jaar
spatial_drift <- mi_coords %>%
  group_by(subset, jaar) %>%
  summarise(mean_x = mean(x), mean_y = mean(y), n = n(), .groups = "drop")

# Visualiseer of het zwaartepunt van de bemonstering verschuift over de jaren
ggplot(spatial_drift, aes(x = mean_x, y = mean_y, color = jaar)) +
  geom_path(arrow = arrow(length = unit(0.2, "cm"))) +
  geom_point(aes(size = n)) +
  facet_wrap(~subset, scales = "free") +
  scale_color_viridis_c() +
  labs(title = "Spatiale drift van monstername per jaar",
       subtitle = "Pijlen volgen de tijd; grote verschuivingen duiden op sampling bias") +
  theme_minimal()

library(tidyverse)
library(sf)
library(rnaturalearth) # Voor de kaartgrenzen
library(ggspatial)     # Voor schaalbalk en noordpijl

# 1. Voorbereiden van de data (spatial drift berekenen)
# We gaan ervan uit dat 'mi_nested' een sf-object is in Lambert 72
spatial_drift_sf <- mi_nested %>%
  mutate(
    x = st_coordinates(geom)[,1],
    y = st_coordinates(geom)[,2]
  ) %>%
  group_by(subset, jaar) %>%
  summarise(
    mean_x = mean(x),
    mean_y = mean(y),
    n = n(),
    .groups = "drop"
  ) %>%
  # Maak van de gemiddelden weer een sf-object (Lambert 72)
  st_as_sf(coords = c("mean_x", "mean_y"), crs = 31370)

# 2. Belgische landgrenzen ophalen en omzetten naar Lambert 72
# belgium <- ne_countries(scale = "large", country = "belgium", returnclass = "sf") %>%
#   st_transform(31370)

vlaanderen <- st_read(here("data", "ruw", "vlaanderen", "vlaanderen_wgs84.shp")) %>%
  st_transform(31370)

# 3. Plotten op de kaart
ggplot() +
  # Achtergrond: kaart van België
  geom_sf(data = vlaanderen, fill = "grey95", color = "grey80") +
  # De drift-paden (lijnen tussen de jaren)
  geom_sf(data = spatial_drift_sf %>% group_by(subset) %>%
            summarise(do_union = FALSE) %>% st_cast("LINESTRING"),
          aes(color = subset), alpha = 0.5, size = 0.8) +
  # De jaarlijkse zwaartepunten
  geom_sf(data = spatial_drift_sf, aes(size = n, fill = jaar), shape = 21, color = "white") +
  # Faceting per subset om overlap te voorkomen
  facet_wrap(~subset) +
  # Kleurenschalen en labels
  scale_fill_viridis_c(option = "plasma") +
  scale_color_discrete(guide = "none") + # Kleur van de lijn volgt subset
  # annotation_scale(location = "bl", width_hint = 0.5) +
  # annotation_north_arrow(location = "bl", which_north = "true",
  #                        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                        style = north_arrow_fancy_orienteering) +
  labs(
    title = "Spatiale drift van de bemonstering (Zwaartepunten per jaar)",
    subtitle = "Geprojecteerd op de kaart van België (Lambert 72)",
    fill = "Jaar",
    size = "Aantal monsters",
    x = "L72 X",
    y = "L72 Y"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(
  filename =  here("output", "figuren", "ruimtelijke_drift_samples_per_jaar.png"),
  plot = last_plot(), # Expliciet de laatste plot kiezen
  width = 40,
  height = 20,
  units = "cm",
  dpi = 300,
  bg = "white"
)


# ==============================================================================
# NIEUWE SECTIE: Facet-kaartjes per jaar voor 'nat_sv_beek'
# ==============================================================================

library(ggthemes) # Voor theme_map, optioneel maar mooi voor kaarten
# 1. Data filteren en voorbereiden
data_beek <- mi_nested %>%
  filter(subset == "nat_sv_beek") %>%
  # Zorg dat jaar een factor is voor de facets,
  # maar behoud een numerieke versie als dat nodig is voor sortering.
  mutate(jaar_factor = as.factor(jaar))
data_beek <- st_as_sf(data_beek)

# Controleer hoeveel jaren er zijn. Als het er heel veel zijn (>20),
# wordt de plot onoverzichtelijk.
unieke_jaren <- unique(data_beek$jaar)
message(paste("Er worden kaartjes gemaakt voor", length(unieke_jaren), "jaren."))

# 2. De Plot maken
plot_beek_per_jaar <- ggplot() +
  # Achtergrond: Kaart van Vlaanderen
  geom_sf(data = vlaanderen, fill = "grey95", color = "grey80", size = 0.3) +
  # De feitelijke meetpunten
  geom_sf(data = data_beek,
          # Je kunt variëren met kleur/grootte gebaseerd op een score, bijv mmif
          aes(color = mmif),
          alpha = 0.7, size = 1.5) +
  # Faceting per jaar
  facet_wrap(~jaar_factor, drop = TRUE) +
  # Opmaak
  scale_color_viridis_c(option = "turbo", name = "MMIF Score") + # Of een andere schaal
  theme_minimal() +
  labs(
    title = "Ruimtelijke spreiding metingen 'nat_sv_beek' per jaar",
    subtitle = "Elke stip is een uitgevoerde bemonstering",
    x = NULL, y = NULL
  ) +
  theme(
    strip.background = element_rect(fill = "gray90", color = NA),
    strip.text = element_text(face = "bold"),
    axis.text = element_blank(), # Coördinaten op de assen weghalen voor rust
    axis.ticks = element_blank(),
    panel.grid = element_blank(), # Gridlijnen weghalen
    legend.position = "bottom"
  )

# 3. Plot tonen
print(plot_beek_per_jaar)

# 4. Plot opslaan
# We maken de plot hoog/breed afhankelijk van het aantal jaren (grid layout)
ggsave(
  filename = here("output", "figuren", "spreiding_nat_sv_beek_per_jaar.png"),
  plot = plot_beek_per_jaar,
  width = 30,  # Breedte aanpassen naar wens
  height = 25, # Hoogte aanpassen afhankelijk van aantal facets
  units = "cm",
  dpi = 300,
  bg = "white"
)

################################################################################

mmif_model_beek <- glmmTMB(
  formula = mmif ~ ec_20_log + o2_s + spei6_s + jaar_s +
    intensiteit_combo_afstr_s + t_s + n_t_log +
    p_t_log + verharding_afstr_s +
    (1 | meetplaats) + (1 | owl),
  data = dredge_data_beek,
  family = ordbeta() # Gebaseerd op de "ordbeta family" in je output
)

mmif_model_kempen <- glmmTMB(
  formula = mmif ~ ec_20_log + o2_s + spei6_s + jaar_s +
    overstorten_blootstelling_index_log + p_h_s + n_t_log +
    p_t_log +
    (1 | meetplaats) + (1 | bekken),
  data = dredge_data_kempen,
  family = ordbeta() # Gebaseerd op de "ordbeta family" in je output
)


mmif_model_rivier <- glmmTMB(
  formula = mmif ~ ec_20_log + ekc2_waterlichaam_s + p_h_s + jaar_s + (1 |      meetplaats) + (1 | bekken) + (1|owl) + spei6_s,
  data = dredge_data_rivier,
  family = ordbeta() # Gebaseerd op de "ordbeta family" in je output
)

# 1. Residuals berekenen uit het model
# We voegen ze direct toe aan de dataframe die gebruikt is voor het model
dredge_data_res <- dredge_data_beek %>%
  mutate(
    res = residuals(mmif_best_model_updated, type = "pearson"),
    res_teken = if_else(res >= 0, "Onderschatting (Positief)", "Overschatting (Negatief)"),
    res_abs = abs(res)
  )

# 2. Filteren op de groep 'nat_sv_beek' voor de kaart
# Zorg dat deze dataset 'sf' eigenschappen heeft
plot_data_res <- dredge_data_res %>%
  left_join(., mi_nat_sv_beek %>%
              select(meetplaats, monsternamedatum, geom),
            by = c("meetplaats", "monsternamedatum")) %>%
  # filter(subset == "nat_sv_beek") %>%
  st_as_sf()

plot_residuals_jaar <- ggplot() +
  # Achtergrond Vlaanderen
  geom_sf(data = vlaanderen, fill = "grey96", color = "grey85", size = 0.2) +

  # De residuals plotten
  geom_sf(data = plot_data_res,
          aes(size = res_abs, color = res_teken, geometry = geom),
          alpha = 0.6) +

  # Faceting per jaar
  facet_wrap(~jaar, drop = TRUE) +

  # Kleuren en schaalinstellingen
  scale_color_manual(values = c("Onderschatting (Positief)" = "#2c7bb6",
                                "Overschatting (Negatief)" = "#d7191c"),
                     name = "Teken Residual") +
  scale_size_continuous(name = "Grootte afwijking (abs. res)",
                        range = c(0.5, 4)) +

  # Layout
  theme_minimal() +
  labs(
    title = "Spatiale distributie van Model Residuals (nat_sv_beek)",
    subtitle = "Blauw: Model onderschat de kwaliteit | Rood: Model overschat de kwaliteit",
    x = NULL, y = NULL
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    strip.text = element_text(face = "bold"),
    panel.grid = element_blank(),
    axis.text = element_blank()
  )

# Tonen en opslaan
print(plot_residuals_jaar)

ggsave(
  filename = here("output", "figuren", "residuals_spatial_per_jaar.png"),
  plot = plot_residuals_jaar,
  width = 35, height = 25, units = "cm", dpi = 300, bg = "white"
)


ntot_best_model_updated

library(tidyverse)
library(sf)

plot_spatial_residuals <- function(model_obj, data, sf_context, label = "Model") {
  # 1. Residuals berekenen en toevoegen aan data
  # We gebruiken Pearson residuals voor standaardisatie tussen verschillende families
  data_res <- data %>%
    mutate(
      res = residuals(model_obj, type = "pearson"),
      res_teken = if_else(res >= 0, "Onderschatting (Positief)", "Overschatting (Negatief)"),
      res_abs = abs(res)
    )

  # 2. Geometrie koppelen
  # We gaan ervan uit dat 'sf_context' de geom en koppelvelden bevat
  plot_data <- data_res %>%
    left_join(sf_context %>% select(meetplaats, monsternamedatum, geom),
              by = c("meetplaats", "monsternamedatum")) %>%
    st_as_sf()

  # 3. De Plot genereren
  p <- ggplot() +
    # Achtergrond (zorg dat 'vlaanderen' globaal beschikbaar is of voeg toe als argument)
    geom_sf(data = vlaanderen, fill = "grey96", color = "grey85", size = 0.2) +

    # De residuals
    geom_sf(data = plot_data,
            aes(size = res_abs, color = res_teken, geometry = geom),
            alpha = 0.6) +

    # Faceting
    facet_wrap(~jaar, drop = TRUE) +

    # Kleuren en schalen
    scale_color_manual(values = c("Onderschatting (Positief)" = "#2c7bb6",
                                  "Overschatting (Negatief)" = "#d7191c"),
                       name = "Teken Residual") +
    scale_size_continuous(name = "Grootte afwijking (abs. res)",
                          range = c(0.5, 4)) +

    # Layout en Labels
    theme_minimal() +
    labs(
      title = paste("Spatiale distributie van Residuals:", label),
      subtitle = "Blauw: Onderschatting | Rood: Overschatting",
      x = NULL, y = NULL
    ) +
    theme(
      legend.position = "bottom",
      legend.box = "vertical",
      strip.text = element_text(face = "bold"),
      panel.grid = element_blank(),
      axis.text = element_blank()
    )

  return(p)
}

# Voorbeeld voor het MMIF model
p1 <- plot_spatial_residuals(mmif_model_kempen, dredge_data_kempen, mi_nat_sv_kempen, "MMIF")
print(p1)

p1 <- plot_spatial_residuals(mmif_model_beek, dredge_data_beek, mi_nat_sv_beek, "MMIF")
print(p1)

p1 <- plot_spatial_residuals(mmif_model_rivier, dredge_data_rivier, mi_nat_sv_rivier, "MMIF")
print(p1)


# Voorbeeld voor het Totaal Fosfor model
p2 <- plot_spatial_residuals(ptot_best_model_updated, dredge_data, mi_nat_sv_kempen, "P-totaal")
print(p2)

# Snel alle plaatjes opslaan in een loop (optioneel)
# Modellenlijst maken
model_lijst <- list(
  "MMIF" = mmif_best_model_beek,
  "Ptot" = ptot_best_model_beek,
  "Ntot" = ntot_best_model_beek
)

for (naam in names(model_lijst)) {
  p <- plot_spatial_residuals(model_lijst[[naam]], dredge_data, mi_nat_sv_beek, naam)
  ggsave(filename = paste0("residuals_", naam, ".png"), plot = p, width = 30, height = 20, units = "cm")
}
