# ==============================================================================
# ALTERNATIEVE STAP 3: ALLEEN BESTAANDE TUSSENSTAPPEN SAMENVOEGEN
# ==============================================================================
map_tabellen <- here("data", "verwerkt", "landgebruik", "pa_tabellen_tussenstappen")

# Zoek alle .rds bestanden die momenteel in de map staan
beschikbare_tabellen <- list.files(path = map_tabellen, pattern = "\\.rds$", full.names = TRUE)

cat("Er zijn momenteel", length(beschikbare_tabellen), "jaren berekend en beschikbaar.\n")

# Laad ze allemaal in en voeg ze direct samen
finale_pa_data <- map(beschikbare_tabellen, readRDS) %>%
  reduce(left_join, by = "meetplaats")

# ==============================================================================
# 4. KOPPELEN AAN BIOLOGISCHE DATA VIA HET MEETJAAR
# ==============================================================================
# Vanaf hier is het exact jouw originele code!
pa_long <- finale_pa_data %>%
  pivot_longer(
    cols = -meetplaats,
    names_to = "pa_jaar",
    names_prefix = "beschermd_pct_",
    values_to = "beschermd_pct"
  ) %>%
  mutate(pa_jaar = as.numeric(pa_jaar))

pa_afstroomgebied_tijdreeks <- mi_data %>%
  select(meetplaats, monsternamedatum) %>%
  st_drop_geometry() %>%
  bind_rows(mafy_data %>% select(meetplaats, monsternamedatum) %>% st_drop_geometry()) %>%

  mutate(meet_jaar = lubridate::year(monsternamedatum)) %>%
  left_join(pa_long, by = c("meetplaats", "meet_jaar" = "pa_jaar")) %>%
  mutate(beschermd_pct = replace_na(beschermd_pct, 0))



library(dplyr)
library(purrr)
library(tidyr)

# ==============================================================================
# 1. BEREKEN DE PA-METRICS PER MEETPLAATS
# ==============================================================================
# Functie om de toename/afname in beschermd gebied te berekenen (PA gain)
bereken_pa_gain <- function(df) {
  df_clean <- df %>% filter(!is.na(beschermd_pct), !is.na(meet_jaar))

  # Zorg dat we minstens 2 meetpunten hebben om een helling te trekken
  if(nrow(df_clean) < 2) {
    return(NA_real_)
  }

  # Lineair model: hoeveel % beschermd gebied komt er per jaar bij?
  model <- lm(beschermd_pct ~ meet_jaar, data = df_clean)
  helling <- coef(model)["meet_jaar"]

  return(helling)
}

# Samenvatten van de ruwe PA-tijdreeks naar één rij per meetplaats
pa_metrics_per_punt <- pa_afstroomgebied_tijdreeks %>%
  group_by(meetplaats) %>%
  tidyr::nest() %>%
  mutate(
    # 1. Bereken het gemiddelde aandeel beschermd gebied over de tijdreeks
    mean_PA_cover = map_dbl(data, ~ mean(.x$beschermd_pct, na.rm = TRUE)),

    # 2. Bereken de veranderingssnelheid (PA gain) over de tijdreeks
    rate_of_PA_gain = map_dbl(data, bereken_pa_gain)
  ) %>%
  select(meetplaats, mean_PA_cover, rate_of_PA_gain)

# ==============================================================================
# 2. KOPPELEN AAN JOUW 'ANALYSE_KLAAR' DATAFRAME
# ==============================================================================
analyse_finaal <- analyse_klaar %>%
  left_join(pa_metrics_per_punt, by = "meetplaats")

# Bekijk het eindresultaat!
head(analyse_finaal)
