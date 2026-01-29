pesticide_data <- tu_per_sample %>%
  select(meetplaats, monsternamedatum, TU_sum) %>%
  left_join(meetnetten %>%
              select(vhas, nummer, owl_code),
            by = c("meetplaats" = "nummer"))

library(dplyr)
library(slider) # Voor het rekenen met rolling windows
library(lubridate)

# 1. Voorbereiden van de pesticide data
# We berekenen eerst de maximale TU_sum per waterlichaam per jaar.
pesticide_per_jaar <- pesticide_data %>%
  mutate(jaar = year(monsternamedatum)) %>%
  group_by(owl_code, jaar) %>%
  summarise(max_tu_jaar = max(TU_sum, na.rm = TRUE), .groups = "drop")

# 2. Bereken de rollende maximumwaarde (venster: jaar-1, jaar, jaar+1)
# Dit geeft voor elk jaar de hoogste waarde die in de omgeving van dat jaar gemeten is.
pesticide_rolling_max <- pesticide_per_jaar %>%
  group_by(owl_code) %>%
  arrange(jaar) %>%
  mutate(
    pesticide_max_3j = slide_index_dbl(
      .x = max_tu_jaar,
      .i = jaar,
      .f = ~max(.x, na.rm = TRUE),
      .before = 2,  # jaar - 1
      .after = 2    # jaar + 1
    )
  ) %>%
  ungroup()

# 3. Koppelen aan de MI data (data_sem_clean)
# We gaan ervan uit dat data_sem_clean ook een 'owl_code' kolom heeft
# (of koppel deze eerst via meetplaats).
data_met_pesticiden <- data_sem_clean %>%
  mutate(jaar = year(monsternamedatum),
         owl_code = owl.x) %>%
  left_join(pesticide_rolling_max %>% select(owl_code, jaar, pesticide_max_3j),
            by = c("owl_code", "jaar"))

# Resultaat bekijken
head(data_met_pesticiden)

# Indien nodig: voeg owl_code toe aan MI data
locatie_map <- pesticide_data %>%
  distinct(meetplaats, owl_code)

data_sem_clean <- data_sem_clean %>%
  left_join(locatie_map, by = "meetplaats")
