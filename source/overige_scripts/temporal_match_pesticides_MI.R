library(dplyr)
library(ggplot2)
library(lubridate)
library(here)
library(sf)
conflicted::conflicts_prefer(lubridate::year)
conflicted::conflicts_prefer(dplyr::filter)
load(here("data", "verwerkt", "tu_resultaten.rdata"))
load(here("data", "verwerkt", "mi_nat_sv.rdata"))
#### Hoeveel pesticiden gemeten per staal??? ####

tu_per_sample %>%
  ggplot(aes(x = monsternamedatum, y = aantal_pesticiden_gemeten)) +

  # 1. Gebruik punten met een beetje transparantie en "jitter" (rammelen)
  # zodat ze niet exact op elkaar vallen
  geom_jitter(alpha = 0.3, width = 0, height = 0.5, color = "steelblue") +

  # 2. Voeg een trendlijn toe om de evolutie te zien
  geom_smooth(method = "loess", color = "red", se = FALSE) +

  # 3. Opmaak
  theme_minimal() +
  labs(
    title = "Evolutie van Monitoring-inspanning",
    subtitle = "Hoeveel pesticiden werden er per staal gemeten doorheen de tijd?",
    x = "Datum",
    y = "Aantal gemeten stoffen per staal"
  )

tu_per_sample %>%
  mutate(jaar = factor(lubridate::year(monsternamedatum))) %>% # Maak een factor van het jaar
  ggplot(aes(x = jaar, y = aantal_pesticiden_gemeten)) +

  # Boxplot toont de mediaan en spreiding
  geom_boxplot(fill = "lightblue", outlier.alpha = 0.2) +

  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Jaren schuin zetten
  labs(
    title = "Verdeling aantal gemeten pesticiden per jaar",
    x = "Jaar",
    y = "Aantal stoffen"
  )

# relatie met TU_sum

tu_per_sample %>%
  # Filter extreme TU uitschieters even weg voor de leesbaarheid van de plot (optioneel)
  filter(TU_sum < 10) %>%

  ggplot(aes(x = aantal_pesticiden_gemeten, y = TU_sum)) +

  # Kleur de punten op basis van het jaar
  geom_point(aes(color = monsternamedatum), alpha = 0.5) +

  geom_smooth(method = "lm", color = "black") + # Lineaire trend

  scale_color_viridis_c(trans = "date") + # Mooie kleurschaal voor tijd
  theme_minimal() +
  labs(
    title = "Correlatie: Inspanning vs. Gevonden Toxiciteit",
    subtitle = "Leidt meer meten tot een hogere berekende TU?",
    x = "Aantal gemeten stoffen",
    y = "Toxic Units (TU Som)"
  )

#### oplossingen voor hogere aantal stoffen meting en positeive relatie met TUsum####

# -------------------------------------------------------------------------
# STAP 1: BEPAAL DE "CORE LIST" (Uit de TU dataset)
# -------------------------------------------------------------------------

# We kijken in tu_dataset (dus stoffen die EN pesticide zijn EN een EC50 hebben)
jaren_per_stof <- tu_dataset %>%
  mutate(jaar = lubridate::year(monsternamedatum)) %>%
  group_by(parameter_symbool) %>%
  summarise(
    aantal_jaren_gemeten = n_distinct(jaar),
    eerste_jaar = min(jaar),
    laatste_jaar = max(jaar),
    # Extra check: hoe vaak kwam deze stof boven de detectielimiet/droeg hij bij aan TU?
    aantal_keren_bijdraag = n()
  ) %>%
  # Filter: Stoffen die in minstens 10 van de 15 jaren (2010-2024) data hebben
  filter(aantal_jaren_gemeten >= 10)

core_stoffen <- jaren_per_stof$parameter_symbool

cat("Aantal stoffen in de Core List (Pesticiden met EC50):", length(core_stoffen), "\n")
print(head(core_stoffen, 20))

# -------------------------------------------------------------------------
# STAP 2: BEREKEN TU_CORE NAAST TU_TOTAL
# -------------------------------------------------------------------------

tu_vergelijking <- tu_dataset %>%
  group_by(meetplaats, monsternamedatum) %>%
  summarise(
    # 1. De originele som (alle pesticiden met EC50 op dat moment gemeten)
    TU_total = sum(TU_individual, na.rm = TRUE),

    # 2. De gecorrigeerde som (alleen de pesticiden uit de core list)
    TU_core = sum(TU_individual[parameter_symbool %in% core_stoffen], na.rm = TRUE),

    # Metadata voor de grafiek
    aantal_stoffen_totaal = n(),
    aantal_stoffen_core = sum(parameter_symbool %in% core_stoffen),

    .groups = "drop"
  )

# -------------------------------------------------------------------------
# STAP 3: VISUALISATIE VAN DE BIAS
# -------------------------------------------------------------------------

# Zet om naar long format
tu_long <- tu_vergelijking %>%
  select(meetplaats, monsternamedatum, TU_total, TU_core) %>%
  tidyr::pivot_longer(cols = c(TU_total, TU_core), names_to = "Type_TU", values_to = "TU_waarde")

# Plot de trend
ggplot(tu_long, aes(x = monsternamedatum, y = TU_waarde, color = Type_TU)) +
  # Gebruik een General Additive Model (GAM) voor een soepele trendlijn
  geom_smooth(method = "gam", se = TRUE) +

  # Zoom in op het relevante bereik (de uitschieters van 100 maken de lijn anders plat)
  coord_cartesian(ylim = c(0, 2)) +

  labs(
    title = "Monitoring Bias Check: Totale TU vs. Core TU",
    subtitle = "Zwart/Blauw (Total) vs Rood (Core): Divergentie toont invloed van nieuwe stoffen",
    x = "Tijd",
    y = "Gemiddelde Toxic Units",
    color = "TU Type"
  ) +
  theme_minimal()


# Selecteer de "Big Bad Wolves" die we eerder identificeerden
boosdoeners <- top_stoffen # op basis van volgende code!

# Maak een subset voor de plot
drivers_over_time <- tu_dataset %>%
  filter(stof_naam %in% boosdoeners) %>%
  mutate(jaar = lubridate::year(monsternamedatum)) %>%
  group_by(jaar, stof_naam) %>%
  summarise(
    avg_TU = mean(TU_individual, na.rm = TRUE),
    .groups = "drop"
  )

# Plotten
ggplot(drivers_over_time, aes(x = jaar, y = avg_TU, color = stof_naam)) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(
    title = "Evolutie van de specifieke 'Drivers'",
    subtitle = "Verklaren verboden (2018-2020) de daling in toxiciteit?",
    y = "Gemiddelde Toxic Unit (TU)",
    x = "Jaar"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = 2010:2024) # Toon elk jaar

# top driver analyse

# -------------------------------------------------------------------------
# STAP 1: WIE ZIJN DE GROOTSTE BOOSDOENERS? (Top N Drivers bepalen)
# -------------------------------------------------------------------------

# We berekenen de totale som van TU over ALLE jaren heen per stof
top_drivers_rank <- tu_dataset %>%
  group_by(stof_naam) %>%
  summarise(total_impact = sum(TU_individual, na.rm = TRUE)) %>%
  arrange(desc(total_impact)) %>%
  slice(1:8) # Kies de Top 8. Pas dit getal aan als je meer/minder detail wilt.

# Maak een lijstje van de namen
top_stoffen <- top_drivers_rank$stof_naam

cat("De 8 belangrijkste drivers van toxiciteit zijn:\n")
print(top_stoffen)

# -------------------------------------------------------------------------
# STAP 2: DATA VOORBEREIDEN VOOR DE PLOT
# -------------------------------------------------------------------------

driver_plot_data <- tu_dataset %>%
  mutate(jaar = year(monsternamedatum)) %>%

  # Classificeer elke meting: Is het een Top Stof of 'Andere'?
  mutate(
    plot_label = ifelse(stof_naam %in% top_stoffen, stof_naam, "Andere (Rest)")
  ) %>%

  # Aggregeer naar Jaar niveau
  # We berekenen de gemiddelde TU bijdrage van elke stof(groep) per jaar
  group_by(jaar, plot_label) %>%
  summarise(
    avg_TU_contribution = mean(TU_individual, na.rm = TRUE),
    # Alternatief: Som (als je absolute vracht wilt zien), maar Mean is beter vergelijkbaar
    .groups = "drop"
  )

# -------------------------------------------------------------------------
# STAP 3: DE STACKED AREA CHART
# -------------------------------------------------------------------------

ggplot(driver_plot_data, aes(x = jaar, y = avg_TU_contribution, fill = forcats::fct_reorder(plot_label, avg_TU_contribution, .desc = TRUE))) +
  # Area chart: stapelt de waarden op elkaar
  geom_area(alpha = 0.8, size = 0.5, color = "white") +

  # Visuele opmaak
  scale_x_continuous(breaks = 2010:2024) +
  scale_fill_viridis_d(option = "turbo", name = "Stofnaam") + # Duidelijke kleuren

  labs(
    title = "Driver Analyse: Welke stoffen vormen de Toxiciteits-mix?",
    subtitle = "Gestapelde bijdrage aan de gemiddelde TU per jaar",
    x = "Jaar",
    y = "Gemiddelde Toxic Units (Bijdrage)"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

#### temporele macht pesticiden ent MI??? ####
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
sample_locations <- unique(mismatch_analysis$meetplaats)[1:100] # Eerste 20 locaties

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

###geen ruimtelijke overlap precies

locs_mi <- unique(mi_nat_sv$meetplaats)
locs_tu <- unique(tu_per_sample$meetplaats)

# Venn diagram in tekstvorm
venn_overlap <- list(
  Alleen_Bio = setdiff(locs_mi, locs_tu),
  Alleen_Chem = setdiff(locs_tu, locs_mi),
  Overlap = base::intersect(locs_mi, locs_tu)
)
lengths(venn_overlap)

###rolling join -> identiek als mijn koppeling in de tijd ###
load("data/verwerkt/mi_nat_sv.rdata")

# 1. Bereid de tabellen voor
dt_mi <- as.data.table(
  mi_nat_sv %>%
    st_drop_geometry() %>%
    mutate(ID_biologie = row_number()) %>%
    select(meetplaats, monsternamedatum, ID_biologie)
)

dt_tu <- as.data.table(tu_per_sample)

# --- CRUCIALE STAP ---
# Maak een kopie van de datum in de TU set.
# De kolom 'monsternamedatum' wordt namelijk gebruikt als sleutel en overschreven door de MI-datum.
# Door 'datum_chemie' te maken, redden we de originele meetdatum.
dt_tu[, datum_chemie := monsternamedatum]

# Zet sleutels
setkey(dt_mi, meetplaats, monsternamedatum)
setkey(dt_tu, meetplaats, monsternamedatum)

# 2. De Rolling Join
# We zoeken voor elke MI de laatste TU meting (roll = Inf = look backwards)
matches <- dt_tu[dt_mi, roll = Inf, on = .(meetplaats, monsternamedatum)]

# 3. Bereken het verschil
# Nu hebben we twee datum-kolommen in 'matches':
# - 'monsternamedatum': Dit is de datum van de BIOLOGIE (uit dt_mi)
# - 'datum_chemie': Dit is de datum van de CHEMIE (die we net gekopieerd hebben)
matches[, diff_days := as.numeric(monsternamedatum - datum_chemie)]

# 4. Filteren (bvb max 1 jaar oud)
# diff_days is hier positief (want chemie was in het verleden)
matches_clean <- matches[diff_days >= 0 & diff_days < 365]

cat("Aantal matches gevonden via Rolling Join:", nrow(matches_clean))

# Check het resultaat
head(matches_clean)
