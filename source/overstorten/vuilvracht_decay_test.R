
afstroomgebieden <- st_read(here("data", "verwerkt", "hydrologisch", "ow_meetpunten_watersheds_nested_all.gpkg"), quiet = TRUE)

locations <- read_sf(here("data", "verwerkt", "hydrologisch", "mi_meetpunten_snapped_to_streams.shp"))

vuilvracht_overstorten <- st_read(here("data", "ruw" , "overstorten", "vuilvracht", "OS_maart2025.shp"))

load(here("data", "verwerkt", "overstorten", "gefilterde_meetplaatsen_overstorten.rdata")) # filter(meetplaats %in% gefilterde_meetplaatsen_overstorten$meetplaats)


library(sf)
library(dplyr)
library(purrr)
library(progress)

calculate_catchment_pressure <- function(locations_sf,
                                         catchments_sf,
                                         sources_sf,
                                         filtered_sites,
                                         max_dist_m = 5000,
                                         decay_distance_km = 0.2,
                                         col_id = "meetplaats",
                                         col_value = "IE") {

  message("--- Start Berekening Catchment Druk ---")

  # 1. Voorbereiding: Filter meetplaatsen
  target_locations <- locations_sf %>%
    filter(!!sym(col_id) %in% filtered_sites[[col_id]])

  # Zorg dat catchments gematcht zijn aan de doellocaties
  target_catchments <- catchments_sf %>%
    filter(!!sym(col_id) %in% target_locations[[col_id]])

  # 2. Progress bar instellen
  pb <- progress_bar$new(
    format = "  [:bar] :percent :elapsedfull | resterend: :eta",
    total = nrow(target_locations), clear = FALSE, width = 60
  )

  # 3. Berekening per meetpunt
  results <- map_df(seq_len(nrow(target_locations)), function(i) {
    pb$tick()

    loc <- target_locations[i, ]
    site_id <- loc[[col_id]]

    # Selecteer het afstroomgebied behorend bij dit meetpunt
    catchment <- target_catchments %>% filter(!!sym(col_id) == site_id)

    if (nrow(catchment) == 0) {
      return(data.frame(meetplaats = site_id, sum_ie_raw = 0, sum_ie_weighted = 0, count_sources = 0))
    }

    # Zoek bronnen (overstorten) die BINNEN dit afstroomgebied vallen
    sources_in_catchment <- sources_sf[st_intersects(sources_sf, catchment, sparse = FALSE), ]

    if (nrow(sources_in_catchment) > 0) {
      # Bereken afstand van elke bron tot het MI-meetpunt (locatie)
      # Let op: dit is Euclidische afstand (rechte lijn)
      dists <- st_distance(sources_in_catchment, loc)
      sources_in_catchment$dist_m <- as.numeric(dists)

      # Filter op max afstand en pas weging toe
      # Formule: IE / (1 + (Afstand / Decay_afstand))
      sources_final <- sources_in_catchment %>%
        filter(dist_m <= max_dist_m) %>%
        mutate(
          weight = 1 / (1 + (dist_m / (decay_distance_km * 1000))),
          ie_weighted = !!sym(col_value) * weight
        )

      return(data.frame(
        meetplaats = site_id,
        sum_ie_raw = sum(sources_final[[col_value]], na.rm = TRUE),
        sum_ie_weighted = sum(sources_final$ie_weighted, na.rm = TRUE),
        count_sources = nrow(sources_final)
      ))

    } else {
      return(data.frame(meetplaats = site_id, sum_ie_raw = 0, sum_ie_weighted = 0, count_sources = 0))
    }
  })

  # 4. Join resultaten terug aan de locaties
  final_output <- target_locations %>%
    left_join(results, by = col_id)

  message("--- Berekening Voltooid ---")
  return(final_output)
}

# Voer de functie uit
overstort_druk_2021 <- calculate_catchment_pressure(
  locations_sf = locations,
  catchments_sf = afstroomgebieden,
  sources_sf = vuilvracht_overstorten,
  filtered_sites = gefilterde_meetplaatsen_overstorten,
  max_dist_m = 5000,
  decay_distance_km = 0.2, # De 200m vervalafstand die we besproken hebben
  col_value = "IE"         # De kolom uit vuilvracht_overstorten
) %>%
  rename(overstorten_vuilvracht_ie = sum_ie_weighted,
         overstorten_vuilvracht_ie_raw = sum_ie_raw)

# Optioneel: bekijk resultaat
head(overstort_druk_2021)
load(file = here("data", "verwerkt", "lozingen_data.rdata"))


test <- lozingen_data %>%
  left_join(overstort_druk_2021 %>%
              st_drop_geometry(),
            by = "meetplaats")

test %>%
  ggplot(aes(overstorten_vuilvracht_ie, overstorten_blootstelling_index)) +
  geom_point() +
  geom_smooth(method = "lm")

test %>%
  ggplot(aes(overstorten_vuilvracht_ie, overstorten_blootstelling_index)) +
  geom_point() +
  geom_smooth(method = "lm")

test %>%
  filter(overstorten_vuilvracht_ie < 2000) %>%
  ggplot(aes(overstorten_vuilvracht_ie, overstorten_blootstelling_index)) +
  geom_point() +
  geom_smooth(method = "lm")

model <- lm(overstorten_blootstelling_index ~ overstorten_vuilvracht_ie,
            data = test)
summary(model)

## test in sem model van vuilvracht vs blootstellingsindex
test2 <- data_sem_clean %>%
  left_join(overstort_druk_2021 %>%
              st_drop_geometry(),
            by = "meetplaats") %>%
  mutate(overstorten_vuilvracht_ie_log = log(overstorten_vuilvracht_ie + 1),
         overstorten_blootstelling_index_log = log(overstorten_blootstelling_index + 1)) %>%
  tidyr::drop_na()

test2 %>%
  ggplot(aes(overstorten_vuilvracht_ie_log, overstorten_blootstelling_index_log)) +
  geom_point() +
  geom_smooth(method = "lm")

model2 <- glmmTMB(
  stress_prop ~ n_t_log + intensiteit_combo_s + spei6_s + n_extreme_3m_s + verharding_afstr_s  + ekc2_waterlichaam_s + o2_verz_fc_s + jaar_s + p_t_log + overstorten_vuilvracht_ie_log + lozingen_industrie_ie_log + lozingen_rwzi_p_t_log + czv_s + lozingen_riool_ie_log + (1 | meetplaats),
  weights = test2$ta_xw,
  data = test2,
  family =  binomial(link = "logit"))
summary(model2)

model3 <- glmmTMB(
  stress_prop ~ n_t_log + intensiteit_combo_s + spei6_s + n_extreme_3m_s + verharding_afstr_s  + ekc2_waterlichaam_s + o2_verz_fc_s + jaar_s + p_t_log + overstorten_blootstelling_index_log + lozingen_industrie_ie_log + lozingen_rwzi_p_t_log + czv_s + lozingen_riool_ie_log + (1 | meetplaats),
  weights = test2$ta_xw,
  data = test2,
  family =  binomial(link = "logit"))
summary(model3)


## vuilvracht testen voor data 2021

test3 <- mi_nat_sv %>%
  filter(jaar %in% c(2022)) %>%
  left_join(overstort_druk_2021, by = "meetplaats") %>%
  mutate(ta_xw = as.integer(ta_xw),
         ep_tw = as.integer(ep_tw),
         ns_tw = as.integer(ns_tw)) %>%
  select(overstorten_vuilvracht_ie, ta_xw, mmif, mt_sw, ns_tw, ep_tw, bekken, meetplaats, o2_verz, n_t) %>%
  filter(if_all(everything(), ~ !is.na(.))) %>%
  mutate(overstorten_vuilvracht_ie_log = log(overstorten_vuilvracht_ie + 1),
         stress_prop = (ep_tw + ns_tw)/ta_xw,
         n_t_log = log(n_t))

model4 <- glmmTMB(mmif ~
                              overstorten_vuilvracht_ie_log  + scale(o2_verz) + n_t_log +
                              (1 | bekken),
                            family = ordbeta,
                            # control = glmmTMBControl(optCtrl = list(iter.max = 5000, eval.max = 5000)),
                            data = test3
                            )
summary(model4)

model5 <- glmmTMB(
  stress_prop ~ overstorten_vuilvracht_ie_log  + scale(o2_verz) + n_t_log + (1 | bekken),
  weights = test3$ta_xw,
  data = test3,
  family =  binomial(link = "logit"))
summary(model5)
