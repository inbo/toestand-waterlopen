library(here)
library(tidyverse)
library(naniar)
library(sf)
library(missRanger)

clean_like_janitor <- function(x) { # soort janitor::clean_names() voor waarden
  x |>
    str_to_lower() |>                     # lowercase
    str_replace_all("[^a-z0-9]+", "_") |> # replace non-alphanumeric with _
    str_replace_all("^_|_$", "")          # trim leading/trailing underscores
}

load(file = here("data", "verwerkt", "fc_data.rdata"))

spatial_fc <- fc_data %>% filter(!is.na(lambert_x)) %>% st_as_sf(., coords = c("lambert_x", "lambert_y"), crs = 31370) %>% select(meetplaats) %>% unique()
mapview::mapview(test)

fd <- st_read(here("data", "ruw", "netwerk", "Flow_direction_coordinates.shp"), quiet = T)
spatial_fc_met_vhag <- st_join(spatial_fc, fd["VHAG"], join = st_nearest_feature)


# 1. Vind de index van de dichtstbijzijnde rivier voor elk punt
nearest_idx <- st_nearest_feature(spatial_fc, fd)

# 2. Bereken de afstand tussen punt en DIE specifieke rivier
# 'by_element = TRUE' is cruciaal! Anders berekent hij een enorme matrix.
dist_to_nearest <- st_distance(spatial_fc, fd[nearest_idx, ], by_element = TRUE)

# 3. Voeg samen en filter op tolerantie (bijv. 50 meter)
spatial_fc_met_vhag <- spatial_fc %>%
  mutate(
    # Haal de vhag op basis van de index
    VHAG = fd$VHAG[nearest_idx],

    # Zet de berekende afstand erbij (maak numeriek om 'units' weg te halen)
    afstand = as.numeric(dist_to_nearest)
  ) %>%
  # Pas de tolerantie toe: als > 50m, zet vhag op NA
  mutate(VHAG = ifelse(afstand > 100, NA, as.character(VHAG)))

nutrient_imp_data <- fc_data %>%
  filter(parameter_omschrijving %in% c("Fosfor, totaal",
                                       "Orthofosfaat",
                                       "Stikstof, totaal",
                                       "Nitriet",
                                       "Nitraat",
                                       "Kjeldahlstikstof",
                                       "Ammonium",
                                       "Nitraat+nitriet",
                                       "Nitraat+nitriet+ammonium",
                                       "Zuurstof, verzadiging",
                                       "Chemisch zuurstofverbruik",
                                       "pH",
                                       "Zuurstof, opgelost",
                                       "Geleidbaarheid (20°C)",
                                       "Geleidbaarheid (25°C)",
                                       "Temperatuur",
                                       "Biochemisch zuurstofverbruik na 5d."
                                       )) %>%
  mutate(parameter_symbool = clean_like_janitor(parameter_symbool)) %>%
  select(meetplaats, monsternamedatum, parameter_symbool, resultaat_ug_L,
         waterloop, bekken) %>% # wide maken
  pivot_wider(., names_from = parameter_symbool, values_from = resultaat_ug_L, values_fn = mean) # gemiddelde nemen van dubbele metingen; hier geen reden voor gevonden want zelfde plaats, dag en uur voor parameter soms dubbele waarde.

# vis_miss(nutrient_imp_data)

load(here("data", "verwerkt", "mi_data.rdata"))
mi_data_analyse <- mi_data %>%
  filter(categorie != "Vijver") %>%
  filter(waterlooptype != "Geïsoleerd water") %>%
  filter(waterlichaamcategorie != "meer") %>%
  filter(!meetplaats %in% c("OW113500", "OW12000", "OW179000", "OW536050", "OW669032", "OW690015", "OW917000", "OW981010", "OW981200"))



#####################
# imputatietest
######################

library(dplyr)
library(lubridate)
library(missRanger)

# 1. Maak de dataset klaar voor het algoritme
df_chem <- nutrient_imp_data %>%
  mutate(
    meetplaats = as.factor(meetplaats),
    bekken = as.factor(bekken),
    waterloop = as.factor(waterloop),
    maand = month(monsternamedatum),
    jaar = year(monsternamedatum)) %>%
  left_join(spatial_fc_met_vhag %>%
              st_drop_geometry() %>%
            select(meetplaats, VHAG)) %>%
  drop_na(VHAG) %>%
  filter(jaar > 2006)

# We selecteren de kolommen die we willen gebruiken voor de voorspelling.
# We sluiten de 'datum_id' uit van de voorspellende variabelen (rechterkant van de tilde).

# Pak een willekeurige steekproef van 1% van je data
df_sample <- df_chem %>% slice_sample(prop = 0.01)

system.time({
  missRanger(
    df_sample,
    formula = . - waterloop ~ . - monsternamedatum - waterloop,
    num.trees = 50,
    maxiter = 5,
    respect.unordered.factors = "order"
  )
})


df_imputed <- missRanger(
  df_chem,
  formula = . - waterloop ~ . - monsternamedatum - waterloop,
  num.trees = 50,
  pmm.k = 3,
  # respect.unordered.factors = "order", # Snelheidsboost
  verbose = 1
)

# Het resultaat is een dataset (df_imputed) zonder NA's in de kolommen die gebruikt zijn.

#####
# imputation checks
#######""

library(dplyr)
library(ggplot2)
library(tidyr)

# 1. We hebben de originele data (met NA's) en de nieuwe (gevuld) nodig
# df_chem = je originele dataset met NA's
# df_imputed = de dataset die uit missRanger kwam

# 2. Voeg een label toe aan de imputed dataset
df_check <- df_imputed %>%
  mutate(
    # Check voor N_totaal: Was hij NA in het origineel?
    status_n = ifelse(is.na(df_chem$n_t), "Geïmputeerd", "Origineel"),

    # Check voor P_totaal
    status_p = ifelse(is.na(df_chem$p_t), "Geïmputeerd", "Origineel")
  )

# Voor Stikstof (n_t)
ggplot(df_check, aes(x = n_t, fill = status_n)) +
  geom_density(alpha = 0.5) + # alpha maakt het doorzichtig
  scale_fill_manual(values = c("Geïmputeerd" = "red", "Origineel" = "blue")) +
  theme_minimal() +
  labs(title = "Verdeling Stikstof: Origineel vs Imputatie",
       x = "Concentratie n_t", y = "Dichtheid")

# Voor Fosfor (p_t) - let op log-schaal kan handig zijn bij scheve verdeling
ggplot(df_check, aes(x = p_t, fill = status_p)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("Geïmputeerd" = "red", "Origineel" = "blue")) +
  scale_x_log10() + # Vaak handig bij nutriënten
  theme_minimal() +
  labs(title = "Verdeling Fosfor (Log schaal)", fill = "Status")

# N_totaal vs Nitraat (no3)
# We hopen dat de rode punten op dezelfde lijn liggen als de blauwe
ggplot(df_check, aes(x = no3, y = n_t, color = status_n)) +
  geom_point(alpha = 0.6, size = 1.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") + # Referentielijn x=y
  scale_color_manual(values = c("Geïmputeerd" = "red", "Origineel" = "darkgrey")) +
  theme_minimal() +
  labs(title = "Relatie Nitraat vs N-Totaal",
       subtitle = "Liggen de rode punten in de blauwe wolk?")

ggplot(df_check, aes(x = as.factor(maand), y = n_t, fill = status_n)) +
  geom_boxplot(outlier.shape = NA) + # Outliers verbergen voor duidelijkheid
  scale_fill_manual(values = c("Geïmputeerd" = "tomato", "Origineel" = "lightblue")) +
  theme_minimal() +
  ylim(0, 20) + # Zoom in op het interessante deel (pas aan aan je data)
  labs(title = "Seizoenspatroon Stikstof", x = "Maand", y = "Concentratie")

# Check de hoogste waarden in de originele dataset
df_chem %>%
  arrange(desc(p_t)) %>%
  select(meetplaats, monsternamedatum, p_t, opo4, VHAG) %>%
  head(20)

# Kijk naar de rijen die extreem hoog geïmputeerd zijn
check_high <- df_imputed %>%
  filter(is.na(df_chem$p_t)) %>% # Alleen de geïmputeerde rijen
  filter(p_t > 10000)            # Alleen de extreme waarden

head(check_high)

library(dplyr)
library(ggplot2)

# 1. Bekijk eerst even waar de 'gekte' begint
# Dit helpt je om een grens te kiezen. Waarschijnlijk ligt de 'natuurlijke' grens ergens rond de 2000-5000.
ggplot(nutrient_imp_data, aes(x = p_t)) +
  geom_histogram(bins = 100) +
  scale_x_log10() +
  geom_vline(xintercept = 5000, color = "red", linetype = "dashed") +
  labs(title = "Waar stopt de normale data?", subtitle = "Rode lijn = 5000 µg/L")

# 2. Stel de Limieten in (in µg/l)
# 5.000 µg/l = 5 mg/l (Al zeer extreem voor oppervlaktewater)
limit_p <- 5000
# 20.000 µg/l = 20 mg/l (Voor stikstof mag de grens hoger liggen)
limit_n <- 20000

# 3. Zet extreme waarden op NA
# We gooien de rij niet weg (want de datum/locatie is nuttig),
# maar we zeggen: "Deze waarde is onbetrouwbaar, schat hem maar opnieuw".
df_clean <- df_chem %>%
  mutate(
    # Als p_t hoger is dan de limiet, maak hem NA. Anders behoud p_t.
    p_t = ifelse(p_t > limit_p, NA, p_t),
    opo4 = ifelse(opo4 > limit_p, NA, opo4), # Vergeet de componenten niet!

    n_t = ifelse(n_t > limit_n, NA, n_t),
    no3 = ifelse(no3 > limit_n, NA, no3),
    nh4 = ifelse(nh4 > limit_n, NA, nh4)
  )

# 4. Draai missRanger opnieuw
# Nu heeft het model geen 'giftige' donoren meer van 124.000.
df_imputed_corrected <- missRanger(
  df_clean,
  formula = . - waterloop ~ . - monsternamedatum - waterloop,
  num.trees = 50,
  pmm.k = 3,
  respect.unordered.factors = "order",
  verbose = 1
)

# 5. Check de plot opnieuw
# De rode bult zou nu weg moeten zijn.

#########################################
#modeltest
####################################
test <- mi_data_analyse %>%
  left_join(df_imputed_corrected %>%
              select(meetplaats, monsternamedatum, n_t, p_t),
            by = c("meetplaats", "monsternamedatum"),
                   suffix = c("", "_imp"))

load("data/verwerkt/mi_nat_sv.rdata")


test <- mi_nat_sv %>%
  left_join(df_imputed_corrected %>%
              select(meetplaats, monsternamedatum, n_t, p_t),
            by = c("meetplaats", "monsternamedatum"),
            suffix = c("", "_imp"))

cor.test(test$n_t, test$n_t_imp
         )
cor.test(test$p_t, test$p_t_imp
)
summary(test$n_t
        )

summary(test$n_t_imp
        )
library(glmmTMB)

model <- glmmTMB(
  mmif ~ scale(n_t) + (1 | meetplaats),
  family = ordbeta,
  data = test %>%
    filter(groep %in% c("beek"))
    )
summary(model)

