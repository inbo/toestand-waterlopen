library(mapview)
library(sf)
library(tidyverse)
library(purrr)
library(glue)
library(here)
library(glmmTMB)
library(sjPlot)
library(DHARMa)

load(here("data", "verwerkt", "mi_data.rdata"))

locations <- read_sf(here("data", "verwerkt", "hydrologisch",
                          "mi_meetpunten_snapped_to_streams.shp"))

# inlezen afstroomgebieden met verschillende buffers
buffer_afstanden <- c(100, 250, 500, 1000, 2500, 5000)

bestanden <- paste0("mi_meetpunten_watersheds_buffered_", buffer_afstanden, "m.gpkg")
object_namen <- paste0("afstroomgebied_buffered_", buffer_afstanden, "m")

walk2(bestanden, object_namen, ~ {
  pad <- here("data", "verwerkt", "hydrologisch", .x)
  assign(.y, st_read(pad, quiet = TRUE), envir = .GlobalEnv)
})

# inlezen overstorten
overstorten_uitlaat_vha <- st_read(here("data", "ruw", "overstorten", "P_OS_uitlaat_VHA.shp")) # overstorten die uitlaten op waterloop

# selecteren meetplaatsen uit locations waar binnen het afstroomgebied van 100m minstens 1 overstort aanwezig is

# Controleer dat CRS hetzelfde is
overstorten_uitlaat_vha <- st_transform(overstorten_uitlaat_vha, st_crs(afstroomgebied_buffered_100m))

# Zoek intersecties tussen afstroomgebieden (100 m) en overstorten
intersecties <- st_intersects(afstroomgebied_buffered_100m, overstorten_uitlaat_vha)

# Voeg een kolom toe: heeft overstort (ja/nee)
afstroomgebied_buffered_100m <- afstroomgebied_buffered_100m %>%
  mutate(heeft_overstort = lengths(intersecties) > 0)

# Filter enkel meetplaatsen waar minstens één overstort voorkomt
locations_met_overstort <- locations %>%
  filter(meetplaats %in%
           afstroomgebied_buffered_100m$meetplaats[afstroomgebied_buffered_100m$heeft_overstort])

# Controle
nrow(locations_met_overstort)

#visualisatie

mapview(afstroomgebied_buffered_100m, zcol = "heeft_overstort") +
  # mapview(overstorten_uitlaat_vha, cex = 2, col.region = "red") +
  mapview(locations_met_overstort, cex = 3, col.region = "green")

######
# Aantal overstorten per meetplaats en bufferafstanden
#########

# Zorg dat alle lagen hetzelfde CRS hebben
overstorten_uitlaat_vha <- st_transform(overstorten_uitlaat_vha, st_crs(afstroomgebied_buffered_100m))

# Definieer bufferafstanden en bijhorende objectnamen
buffer_afstanden <- c(100, 250, 500, 1000, 2500, 5000)
buffer_obj_namen <- paste0("afstroomgebied_buffered_", buffer_afstanden, "m")

# Helperfunctie die overstorten koppelt aan afstroomgebieden en meetplaatsen selecteert
bepaal_meetplaatsen_met_overstort <- function(buffer_obj, afstand) {

  afstroom <- get(buffer_obj, envir = .GlobalEnv)

  intersecties <- st_intersects(afstroom, overstorten_uitlaat_vha)

  afstroom <- afstroom %>%
    mutate(heeft_overstort = lengths(intersecties) > 0)

  # Selecteer de corresponderende meetplaatsen
  locations_met_overstort <- locations %>%
    filter(meetplaats %in%
             afstroom$meetplaats[afstroom$heeft_overstort]) %>%
    mutate(buffer_m = afstand)

  # Resultaat teruggeven als lijst
  list(
    afstand = afstand,
    afstroom = afstroom,
    locations_met_overstort = locations_met_overstort,
    aantal_meetplaatsen = nrow(locations_met_overstort)
  )
}

# Pas de functie toe op alle bufferafstanden
resultaten_buffers <- map2(buffer_obj_namen, buffer_afstanden, bepaal_meetplaatsen_met_overstort)

# =====================================================================
# Samenvattende tabel: aantal meetplaatsen met overstort per bufferafstand
# =====================================================================
samenvatting <- map_dfr(resultaten_buffers, ~ tibble(
  buffer_m = .x$afstand,
  n_meetplaatsen_met_overstort = .x$aantal_meetplaatsen
))

print(samenvatting)

# =====================================================================
# Dataframe: per meetplaats aantal overstort per bufferafstand
# =====================================================================

# Zorg dat overstorten in hetzelfde CRS staan
overstorten_uitlaat_vha <- st_transform(overstorten_uitlaat_vha, st_crs(afstroomgebied_buffered_100m))

# Definieer buffers en bijhorende objectnamen
buffer_afstanden <- c(100, 250, 500, 1000, 2500, 5000)
buffer_obj_namen <- paste0("afstroomgebied_buffered_", buffer_afstanden, "m")

# Helperfunctie: tel aantal overstorten per meetplaats voor een bepaalde bufferafstand
tel_overstorten_per_buffer <- function(buffer_obj, afstand) {
  afstroom <- get(buffer_obj, envir = .GlobalEnv)

  intersecties <- st_intersects(afstroom, overstorten_uitlaat_vha)

  afstroom %>%
    st_drop_geometry() %>%
    mutate(!!glue("aantal_overstorten_{afstand}m") := lengths(intersecties)) %>%
    select(meetplaats, starts_with("aantal_overstorten_"))
}

# Bereken aantallen voor alle bufferafstanden
overstort_tellingen <- map2(buffer_obj_namen, buffer_afstanden, tel_overstorten_per_buffer)

# Combineer alle resultaten per meetplaats
overstort_tellingen_df <- reduce(overstort_tellingen, full_join, by = "meetplaats")

# Voeg eventueel info over locaties toe (optioneel)
overstort_tellingen_df <- locations %>%
  st_drop_geometry() %>%
  select(meetplaats) %>%
  left_join(overstort_tellingen_df, by = "meetplaats")

# Resultaat bekijken
print(overstort_tellingen_df)

save(overstort_tellingen_df, file = here("data", "verwerkt", "overstorten", "overstort_tellingen_df.rdata"))

# =====================================================================
# Visualisatie
# =====================================================================

# Zet brede tabel om naar lange vorm
overstort_tellingen_long <- overstort_tellingen_df %>%
  pivot_longer(
    cols = starts_with("aantal_overstorten_"),
    names_to = "buffer_m",
    names_prefix = "aantal_overstorten_",
    names_transform = list(buffer_m = readr::parse_number),
    values_to = "aantal_overstorten"
  )

# Gemiddeld aantal overstorten per bufferafstand
overstort_samenvatting <- overstort_tellingen_long %>%
  group_by(buffer_m) %>%
  summarise(gemiddeld_aantal = mean(aantal_overstorten, na.rm = TRUE))

# Plots

ggplot(samenvatting, aes(x = buffer_m, y = n_meetplaatsen_met_overstort)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Aantal meetplaatsen met een overstort per bufferafstand",
    x = "Bufferafstand (m)",
    y = "Aantal meetplaatsen met minstens één overstort"
  )

ggplot(overstort_samenvatting, aes(x = buffer_m, y = gemiddeld_aantal)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Gemiddeld aantal overstorten per bufferafstand",
    x = "Bufferafstand (m)",
    y = "Gemiddeld aantal overstorten per meetplaats"
  )

ggplot(overstort_tellingen_long, aes(x = buffer_m, y = aantal_overstorten, group = meetplaats)) +
  geom_line(alpha = 0.3) +
  geom_smooth(aes(group = 1), color = "red", se = FALSE, linewidth = 1.2) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Toename van aantal overstorten per meetplaats met bufferafstand",
    x = "Bufferafstand (m)",
    y = "Aantal overstorten"
  )

#####
# Analyse
##########
load(here("data", "verwerkt", "mi_nat_sv.rdata"))

data_model <- overstort_tellingen_df %>%
  select(meetplaats, aantal_overstorten_250m) %>%
  mutate(pa_overstort_250m = as.factor(if_else(aantal_overstorten_250m > 0, 1, 0)),
         aantal_overstorten_250m = as.factor(aantal_overstorten_250m)) %>%
  right_join(mi_nat_sv, by = "meetplaats") %>%
  select(meetplaats, bekken, jaar, groep, mmif, ta_xw, ep_tw, ns_tw, mt_sw, sw_dw, kjn, p_t, o2, ec_20, landbouw_intens_afstr, hooggroen_afstr, verharding_afstr, pa_overstort_250m, aantal_overstorten_250m) %>%
  filter(if_all(everything(), ~ !is.na(.))) %>%
  mutate(ep_tw = as.integer(ep_tw),
         ta_xw = as.integer(ta_xw))

model_mmif <- glmmTMB(mmif ~
                              groep * (pa_overstort_250m + scale(jaar) + scale(verharding_afstr)) +
                              (1 | bekken/meetplaats),
                            data = data_model
                            )
summary(model_mmif)
plot_model(model_mmif, "pred", terms = c("pa_overstort_250m", "groep"))

model_ept <- glmmTMB(ep_tw ~
                        pa_overstort_250m + scale(jaar) + scale(verharding_afstr) +
                        (1 | bekken/meetplaats),
                       family = poisson,
                      data = data_model %>%
                       filter(!groep %in% c("rivier", "polder"))

)

summary(model_ept)
plot_model(model_ept, "pred", terms = c("pa_overstort_250m"))

model_mt_sw <- glmmTMB(mt_sw ~
                         pa_overstort_250m + scale(jaar) + scale(verharding_afstr) +
                         (1 | bekken/meetplaats),
                       data = data_model %>%
                         filter(!groep %in% c("rivier", "polder"))

)

summary(model_ept)
plot_model(model_ept, "pred", terms = c("pa_overstort_250m"))

model_tax <- glmmTMB(ta_xw ~
                       groep * (pa_overstort_250m + scale(jaar) + scale(verharding_afstr)) +
                       (1 | bekken/meetplaats),
                     family = poisson,
                     data = data_model
)

summary(model_tax)
plot_model(model_tax, "pred", terms = c("pa_overstort_250m", "groep"))

model_o2 <- glmmTMB(o2 ~
                       groep * (pa_overstort_250m + scale(jaar) + scale(verharding_afstr)) +
                       (1 | bekken/meetplaats),
                    data = data_model
)

summary(model_o2)
plot_model(model_o2, "pred", terms = c("pa_overstort_250m", "groep"))

model_kjn <- glmmTMB(kjn ~
                      groep * (pa_overstort_250m + scale(jaar) + scale(verharding_afstr)) +
                      (1 | bekken/meetplaats),
                    data = data_model
)

summary(model_kjn)
plot_model(model_kjn, "pred", terms = c("pa_overstort_250m", "groep"))


data_model <- overstort_tellingen_df %>%
  select(meetplaats, aantal_overstorten_250m, aantal_overstorten_1000m) %>%
  mutate(pa_overstort_250m = as.factor(if_else(aantal_overstorten_250m > 0, 1, 0))) %>%
         # mutate(aantal_overstorten_250m = as.factor(aantal_overstorten_250m)) %>%
  right_join(mi_nat_sv, by = "meetplaats") %>%
  select(meetplaats, bekken, jaar, groep, mmif, ta_xw, ep_tw, ns_tw, mt_sw, sw_dw, kjn, p_t, o2, ec_20, landbouw_intens_afstr, hooggroen_afstr, verharding_afstr, pa_overstort_250m, aantal_overstorten_250m, aantal_overstorten_1000m, aantal_overstorten_500m) %>%
  filter(if_all(everything(), ~ !is.na(.))) %>%
  mutate(ep_tw = as.integer(ep_tw),
         ta_xw = as.integer(ta_xw),
         ns_tw = as.integer(ns_tw))

model_mmif_beek_kempen <- glmmTMB(mmif ~
                        scale(aantal_overstorten_500m) + scale(jaar) + scale(verharding_afstr) +
                        (1 | bekken/meetplaats),
                        family = ordbeta,
                      data = data_model %>%
                        filter(!groep %in% c("rivier", "polder"))
)
summary(model_mmif_beek_kempen)
plot_model(model_mmif_beek_kempen, "pred", terms = c("aantal_overstorten_500m"))
simulationOutput <- simulateResiduals(fittedModel = model_mmif_beek_kempen, plot = F)
plot(simulationOutput)


model_mts_beek_kempen <- glmmTMB(mt_sw ~
                                    scale(aantal_overstorten_500m) + scale(jaar) + scale(verharding_afstr) + scale(kjn) + scale(o2) +
                                    (1 | bekken/meetplaats),
                                 family = ordbeta(),
                                  data = data_model %>%
                                    filter(!groep %in% c("rivier", "polder")) %>%
                                             mutate(mt_sw = mt_sw/10))
summary(model_mts_beek_kempen)
plot_model(model_mts_beek_kempen, "pred", terms = c("aantal_overstorten_500m"))

model_swd_beek_kempen <- glmmTMB(sw_dw ~
                                   scale(aantal_overstorten_500m) + scale(jaar) + scale(verharding_afstr) + scale(kjn) + scale(o2) +
                                   (1 | bekken/meetplaats),
                                  data = data_model %>%
                                   filter(!groep %in% c("rivier", "polder")) %>%
                                   mutate(mt_sw = mt_sw/10))
summary(model_swd_beek_kempen)

plot(model_mts_beek_kempen)

simulationOutput <- simulateResiduals(fittedModel = model_mts_beek_kempen, plot = F)
plot(simulationOutput)
testDispersion(simulationOutput) # geen overdispersie
testZeroInflation(simulationOutput) # geen zero_inflation
testUniformity(simulationOutput)

model_ept_beek_kempen <- glmmTMB(ep_tw/ta_xw ~
                                   scale(aantal_overstorten_500m) + scale(jaar) + scale(verharding_afstr) + scale(kjn) + scale(o2) +
                                   (1 | bekken/meetplaats),
                                 family = binomial(),
                                 weights = ta_xw,
                                 data = data_model %>%
                                   filter(!groep %in% c("rivier", "polder")) %>%
                                   mutate(mt_sw = mt_sw/10))
summary(model_ept_beek_kempen)
plot_model(model_ept_beek_kempen, "pred", terms = c("aantal_overstorten_500m"))
simulationOutput <- simulateResiduals(fittedModel = model_ept_beek_kempen, plot = F)
plot(simulationOutput)

model_tax_beek_kempen <- glmmTMB(ta_xw ~
                                   scale(aantal_overstorten_500m) + scale(jaar) + scale(verharding_afstr) + scale(kjn) + scale(o2) +
                                   (1 | bekken/meetplaats),
                                 family = poisson(),
                                 data = data_model %>%
                                   filter(!groep %in% c("rivier", "polder")) %>%
                                   mutate(mt_sw = mt_sw/10))
summary(model_tax_beek_kempen)
plot_model(model_tax_beek_kempen, "pred", terms = c("aantal_overstorten_500m"))
simulationOutput <- simulateResiduals(fittedModel = model_ept_beek_kempen, plot = F)
plot(simulationOutput)

model_nst_beek_kempen <- glmmTMB(ns_tw/ta_xw ~ scale(aantal_overstorten_500m) + scale(jaar) + scale(verharding_afstr) + scale(kjn) + scale(o2) + (1 | bekken/meetplaats),
                                 family = binomial(),
                                 weights = ta_xw,
                                 data = data_model %>%
                                   filter(!groep %in% c("rivier", "polder")) %>%
                                   mutate(n_stress = as.integer(ep_tw + ns_tw)) %>%
                                   mutate(mt_sw = mt_sw/10))
summary(model_nst_beek_kempen)
plot_model(model_nst_beek_kempen, "pred", terms = c("aantal_overstorten_500m"))
simulationOutput <- simulateResiduals(fittedModel = model_ept_beek_kempen, plot = F)
plot(simulationOutput)
testZeroInflation(simulationOutput)



model_stress_beek_kempen <- glmmTMB(n_stress ~
                                      scale(aantal_overstorten_500m) + scale(jaar) + scale(hooggroen_afstr) + scale(kjn) + scale(o2) + scale(ec_20) +
                                      (1 | bekken/meetplaats),
                                    family = binomial,
                                    weights = ta_xw,
                                    data = data_model %>%
                                      filter(!groep %in% c("rivier", "polder")) %>%
                                      mutate(n_stress = (ep_tw + ns_tw) / ta_xw) %>%
                                      mutate(mt_sw = mt_sw/10))
summary(model_stress_beek_kempen)
plot_model(model_stress_beek_kempen, "pred", terms = c("aantal_overstorten_500m"))
simulationOutput <- simulateResiduals(fittedModel = model_stress_beek_kempen, plot = F)
plot(simulationOutput)

# overdispersion

model_stress_beek_kempen <- glmmTMB(cbind(n_stress, ta_xw - n_stress) ~
                                      scale(aantal_overstorten_500m) + scale(jaar) + scale(hooggroen_afstr) + scale(kjn) + scale(o2) +
                                      (1 | meetplaats),
                                    family = betabinomial,
                                    data = data_model %>%
                                      filter(!groep %in% c("rivier", "polder")) %>%
                                      mutate(n_stress = as.integer(ep_tw + ns_tw)) %>%
                                      mutate(mt_sw = mt_sw/10))
summary(model_stress_beek_kempen)
plot_model(model_stress_beek_kempen, "pred", terms = c("aantal_overstorten_500m"))
plot_model(model_stress_beek_kempen, "pred", terms = c("aantal_overstorten_500m"))
simulationOutput <- simulateResiduals(fittedModel = model_stress_beek_kempen, plot = F)
plot(simulationOutput)


model_stress_beek_kempen_positief <- glmmTMB(n_stress ~
                                      scale(aantal_overstorten_500m) + scale(jaar) + scale(hooggroen_afstr) + scale(kjn) + scale(o2) + scale(ec_20) +
                                      (1 | bekken/meetplaats),
                                    family = betabinomial,
                                    weights = ta_xw,
                                    data = data_model %>%
                                      filter(!groep %in% c("rivier", "polder")) %>%
                                      mutate(n_stress = (ep_tw + ns_tw) / ta_xw) %>%
                                      mutate(mt_sw = mt_sw/10))
summary(model_stress_beek_kempen_positief)
plot_model(model_stress_beek_kempen_positief, "pred", terms = c("aantal_overstorten_500m"))
simulationOutput <- simulateResiduals(fittedModel = model_stress_beek_kempen_positief, plot = F)
plot(simulationOutput)
