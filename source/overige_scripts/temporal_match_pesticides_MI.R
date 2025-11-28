library(dplyr)
library(ggplot2)
library(lubridate)

# Stap A: Bereken voor elk MI staal het tijdsverschil met de dichtstbijzijnde TU meting
# We doen dit slim om geheugenproblemen te voorkomen:
# We zoeken per MI staal alleen de dichtste TU datum op dezelfde locatie.

# 1. Zorg dat datums Date objecten zijn
tu_dates <- tu_per_sample %>%
  select(meetplaats, date_tu = monsternamedatum) %>%
  distinct()

mi_dates <- mi_nat_sv %>%
  st_drop_geometry() %>%
  select(meetplaats, date_mi = monsternamedatum) %>%
  distinct()

# 2. De Join (Cartesiaans per meetplaats, maar alleen datums dus lichter)
mismatch_analysis <- mi_dates %>%
  inner_join(tu_dates, by = "meetplaats") %>%
  mutate(
    diff_days = as.numeric(date_mi - date_tu) # Positief = TU was in verleden
  ) %>%
  # Stap 3: Vind voor elk MI staal de dichtstbijzijnde TU meting (absoluut gezien)
  group_by(meetplaats, date_mi) %>%
  summarise(
    days_to_nearest_tu = diff_days[which.min(abs(diff_days))],
    days_to_nearest_prev_tu = ifelse(any(diff_days > 0), min(diff_days[diff_days > 0]), NA), # Dichtste in verleden
    .groups = "drop"
  )

# Stap B: Visualiseren

# Plot 1: Histogram van het "Gat" in dagen
# Dit toont hoe ver je moet zoeken om een match te vinden
ggplot(mismatch_analysis, aes(x = days_to_nearest_tu)) +
  geom_histogram(binwidth = 30, fill = "steelblue", color = "white") +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Tijdsgat tussen MI staalname en dichtstbijzijnde Pesticide meting",
    subtitle = "Negatief = Pesticide gemeten NA biologie | Positief = Pesticide gemeten VOOR biologie",
    x = "Dagen verschil (MI datum - TU datum)",
    y = "Aantal MI stalen"
  ) +
  theme_minimal()

# Plot 2: Scatterplot Locaties vs Tijd (De "Gatenkaas" check)
# Kies een paar meetplaatsen waar je beide data hebt om het patroon te zien
sample_locations <- unique(mismatch_analysis$meetplaats)[1:20] # Eerste 20 locaties

ggplot() +
  # Biologische metingen (Rode punten)
  geom_point(data = mi_dates %>% filter(meetplaats %in% sample_locations),
             aes(x = date_mi, y = meetplaats), color = "red", size = 3, shape = 4) +
  # Chemische metingen (Blauwe puntjes)
  geom_point(data = tu_dates %>% filter(meetplaats %in% sample_locations),
             aes(x = date_tu, y = meetplaats), color = "blue", size = 1, alpha = 0.5) +
  labs(
    title = "Sampling Frequentie: Chemie (Blauw) vs Biologie (Rood)",
    x = "Tijd", y = "Meetplaats"
  ) +
  theme_minimal()
