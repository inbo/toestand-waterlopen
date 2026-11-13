# Installeer en laad benodigde packages als ze nog niet geïnstalleerd zijn
if (!exists("packages_geladen")) {
  source(here::here("source", "inladen_packages.R"))
}
source(here::here("source", "functies.R"))
source(here::here("source", "analyse", "mi_datasets_prep.R"))

# ==============================================================================
# 1. FILTER MEETPLAATSEN OP BASIS VAN NIEUWE CRITERIA
# ==============================================================================
# Basis dataset voorbereiden
df_basis <- mi_data_analyse %>%
  mutate(jaar = lubridate::year(monsternamedatum)) %>%
  filter(jaar >= 2000, !is.na(mmif))

# Bepaal per meetplaats of ze aan de 3 strenge regels voldoen
meetplaats_criteria <- df_basis %>%
  group_by(meetplaats) %>%
  summarise(
    # Regel 2: Minimaal 5 metingen in totaal
    aantal_metingen = n(),
    allereerste_jaar = min(jaar, na.rm = TRUE),

    # Zoek het meest recente jaar dat binnen de 2019-2023 window valt
    recentste_in_window = max(jaar[jaar >= 2019 & jaar <= 2023], na.rm = TRUE)
  ) %>%
  # Filter eventuele 'Inf' of '-Inf' eruit (locaties die geen data hadden in 2019-2023)
  filter(!is.infinite(recentste_in_window)) %>%

  # Toepassen van de logische filters
  filter(
    aantal_metingen >= 5,                                      # Regel 2
    (recentste_in_window - allereerste_jaar) >= 10             # Regel 3
  )

# Behoud enkel de meetplaatsen die de test hebben doorstaan
geldige_locaties <- meetplaats_criteria$meetplaats

df_gefilterd <- df_basis %>%
  filter(meetplaats %in% geldige_locaties)

# ==============================================================================
# 2. BEREKEN INITIËLE KWALITEIT
# ==============================================================================
initiele_kwaliteit <- df_gefilterd %>%
  st_drop_geometry() %>%
  arrange(meetplaats, monsternamedatum) %>%
  group_by(meetplaats) %>%

  # 1. Bepaal het absolute startjaar voor deze specifieke meetplaats
  mutate(allereerste_jaar = min(jaar, na.rm = TRUE)) %>%

  # 2. De tijdsfilter: behoud ALLES wat binnen 5 jaar van dat allereerste jaar valt
  # (Bijv. start in 2000 = behoud alles t.e.m. 2005)
  filter(jaar <= (allereerste_jaar + 5)) %>%

  # 3. Vat samen zoals je al deed, maar nu gegarandeerd binnen het juiste tijdsvak
  summarise(
    initiele_mmif = mean(mmif, na.rm = TRUE),
    start_jaar = min(jaar, na.rm = TRUE),

    # Optioneel, maar heel handig voor je methodologie:
    eind_jaar_window = max(jaar, na.rm = TRUE), # Checken of het echt max 5 jaar is
    aantal_metingen = n(),                      # Hoeveel metingen zaten er in dit window?

    .groups = "drop"
  )

# initiele_kwaliteit <- df_gefilterd %>%
#   st_drop_geometry() %>%
#   arrange(meetplaats, monsternamedatum) %>%
#   group_by(meetplaats) %>%
#   slice_head(n = 1) %>%
#   summarise(
#     initiele_mmif = mean(mmif, na.rm = TRUE),
#     start_jaar = min(jaar)
#   )

# ==============================================================================
# 3. BEREKEN DE TEMPORELE TREND (% verandering per jaar)
# ==============================================================================
bereken_trend <- function(df) {
  df_clean <- df %>% filter(!is.na(mmif), !is.na(jaar))

  if(nrow(df_clean) < 2) {
    return(NA_real_)
  }

  df_clean$log_mmif <- log(df_clean$mmif + 0.01)
  model <- lm(log_mmif ~ jaar, data = df_clean)
  helling <- coef(model)["jaar"]
  pct_verandering <- (exp(helling) - 1) * 100

  return(pct_verandering)
}

trends <- df_gefilterd %>%
  st_drop_geometry() %>%
  group_by(meetplaats) %>%
  tidyr::nest() %>%
  mutate(trend_pct_jaar = purrr::map_dbl(data, bereken_trend)) %>%
  select(meetplaats, trend_pct_jaar)

# ==============================================================================
# 4. SELECTEER MEEST RECENTE METING (2019-2023) EN VOEG ALLES SAMEN
# ==============================================================================
# Regel 1: Haal de meest recente opname per meetplaats tussen 2019 en 2023
laatste_metingen <- df_gefilterd %>%
  filter(jaar >= 2019 & jaar <= 2023) %>%
  arrange(meetplaats, desc(monsternamedatum)) %>%
  group_by(meetplaats) %>%
  slice_head(n = 1) %>%
  select(meetplaats, monsternamedatum, mmif_recent = mmif, owl, bekken, groep, geometry)

# Voeg de trend en initiële kwaliteit toe aan deze tabel
analyse_klaar <- laatste_metingen %>%
  left_join(initiele_kwaliteit, by = "meetplaats") %>%
  left_join(trends, by = "meetplaats") %>%
  ungroup()

# Bekijk het resultaat!
head(analyse_klaar)

library(dplyr)
library(purrr)
library(tidyr)
library(sf)
library(lubridate)

# ==============================================================================
# 5. PA-METRICS BEREKENEN PER MEETPLAATS
# ==============================================================================
bereken_pa_gain <- function(df) {
  df_clean <- df %>% filter(!is.na(beschermd_pct), !is.na(meet_jaar))
  if (nrow(df_clean) < 2) return(NA_real_)

  model <- lm(beschermd_pct ~ meet_jaar, data = df_clean)
  return(coef(model)["meet_jaar"])
}

pa_metrics_per_punt <- pa_afstroomgebied_tijdreeks %>%
  group_by(meetplaats) %>%
  tidyr::nest() %>%
  mutate(
    mean_PA_cover = map_dbl(data, ~ mean(.x$beschermd_pct, na.rm = TRUE)),
    rate_of_PA_gain = map_dbl(data, bereken_pa_gain)
  ) %>%
  select(meetplaats, mean_PA_cover, rate_of_PA_gain)

# ==============================================================================
# 6. KOPPELEN AAN OPPERVLAKTE (Voor de controlevariabele riviergrootte)
# ==============================================================================
# We halen de oppervlakte uit je originele watersheds dataset
oppervlakte_data <- watersheds_buffered %>%
  st_drop_geometry() %>%
  select(meetplaats, oppervlakte) %>%
  mutate(oppervlakte = as.numeric(oppervlakte))

# ==============================================================================
# 7. DE DEFINITIEVE DATASET BOUWEN
# ==============================================================================
analyse_finaal <- analyse_klaar %>%
  # 1. Voeg de PA metrics toe
  left_join(pa_metrics_per_punt, by = "meetplaats") %>%

  # 2. Voeg de oppervlakte van het afstroomgebied toe
  left_join(oppervlakte_data, by = "meetplaats") %>%

  # 3. Bereken alle benodigde modelvariabelen
  mutate(
    log_oppervlakte = log10(oppervlakte),
    lengte_tijdreeks = year(monsternamedatum) - start_jaar,
    X_coord = st_coordinates(geometry)[,1],
    Y_coord = st_coordinates(geometry)[,2]
  ) %>%

  # 4. Verwijder de geometrie (we hebben de X en Y nu als losse kolommen)
  st_drop_geometry() %>%

  # 5. Filter rijen waarvoor we geen volledige data hebben
  filter(complete.cases(trend_pct_jaar, mean_PA_cover, rate_of_PA_gain, initiele_mmif, log_oppervlakte))

# ==============================================================================
# 8. OPSLAAN VOOR MODELLERING
# ==============================================================================
saveRDS(analyse_finaal, here::here("data", "verwerkt", "analyse_finaal_modellen.rds"))
print("✅ Dataset analyse_finaal is succesvol gebouwd en opgeslagen!")
