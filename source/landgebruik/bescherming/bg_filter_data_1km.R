
# ==============================================================================
# 5. PA-METRICS BEREKENEN PER MEETPLAATS
# ==============================================================================
bereken_pa_gain <- function(df) {
  df_clean <- df %>% filter(!is.na(beschermd_pct), !is.na(meet_jaar))
  if (nrow(df_clean) < 2) return(NA_real_)

  model <- lm(beschermd_pct ~ meet_jaar, data = df_clean)
  return(coef(model)["meet_jaar"])
}

pa_metrics_per_punt_1km <- pa_1km_tijdreeks %>%
  group_by(meetplaats) %>%
  tidyr::nest() %>%
  mutate(
    mean_PA_cover = map_dbl(data, ~ mean(.x$beschermd_pct, na.rm = TRUE)),
    rate_of_PA_gain = map_dbl(data, bereken_pa_gain)
  ) %>%
  select(meetplaats, mean_PA_cover, rate_of_PA_gain)

# # ==============================================================================
# # 6. KOPPELEN AAN OPPERVLAKTE (Voor de controlevariabele riviergrootte)
# # ==============================================================================
# # We halen de oppervlakte uit je originele watersheds dataset
# oppervlakte_data <- pa_afstroomgebied_1km_tijdreeks %>%
#   st_drop_geometry() %>%
#   select(meetplaats, oppervlakte) %>%
#   mutate(oppervlakte = as.numeric(oppervlakte))

# ==============================================================================
# 7. DE DEFINITIEVE DATASET BOUWEN
# ==============================================================================
analyse_finaal_1km <- analyse_klaar %>%
  # 1. Voeg de PA metrics toe
  left_join(pa_metrics_per_punt_1km, by = "meetplaats") %>%

  # # 2. Voeg de oppervlakte van het afstroomgebied toe
  # left_join(oppervlakte_data, by = "meetplaats") %>%

  # 3. Bereken alle benodigde modelvariabelen
  mutate(
    # log_oppervlakte = log10(oppervlakte),
    lengte_tijdreeks = year(monsternamedatum) - start_jaar,
    X_coord = st_coordinates(geometry)[,1],
    Y_coord = st_coordinates(geometry)[,2]
  ) %>%

  # 4. Verwijder de geometrie (we hebben de X en Y nu als losse kolommen)
  st_drop_geometry() %>%

  # 5. Filter rijen waarvoor we geen volledige data hebben
  filter(complete.cases(trend_pct_jaar, mean_PA_cover, rate_of_PA_gain, initiele_mmif))

# ==============================================================================
# 8. OPSLAAN VOOR MODELLERING
# ==============================================================================
saveRDS(analyse_finaal_1km, here::here("data", "verwerkt", "analyse_finaal_modellen_1km.rds"))
print("✅ Dataset analyse_finaal is succesvol gebouwd en opgeslagen!")
