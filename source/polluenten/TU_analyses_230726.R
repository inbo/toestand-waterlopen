# Zorg dat de benodigde packages geladen zijn
library(dplyr)
library(lubridate)
source(here::here("source", "analyse", "mi_datasets_prep.R"))
load(file = here("data", "verwerkt", "polluenten", "tu_master_macroinvertebrates.rdata"))

# ==============================================================================
# STAP 1: UNIEKE ID TOEVOEGEN & VENSTERS MAKEN
# ==============================================================================

# Voeg een rij-nummer toe als unieke ID voor je biologische stalen
mi_data_prep <- data5 %>%
  mutate(
    mi_sample_id = row_number(),
    monsternamedatum_mi = as.Date(monsternamedatum),
    start_window = monsternamedatum_mi - months(24),
    eind_window  = monsternamedatum_mi
  )

# Bereid pesticiden voor, maar we LATEN DE ORIGINELE DATUM VALLEN
# (om .x en .y conflicten te vermijden!)
tu_master_prep <- tu_master_mi %>%
  mutate(monsternamedatum_pest = as.Date(monsternamedatum)) %>%
  select(-monsternamedatum)

# ==============================================================================
# STAP 2: DE ROLLING JOIN UITVOEREN
# ==============================================================================

mi_pest_ruw <- mi_data_prep %>%
  inner_join(
    tu_master_prep,
    by = join_by(
      meetplaats == meetplaats,
      start_window <= monsternamedatum_pest,
      eind_window >= monsternamedatum_pest
    )
  )

# ==============================================================================
# STAP 3: AGGREGEREN OP BASIS VAN DE UNIEKE ID
# ==============================================================================

pest_geaggregeerd <- mi_pest_ruw %>%
  # We groeperen nu ENKEL op de unieke ID. Geen gedoe met geometry of andere kolommen.
  group_by(mi_sample_id) %>%
  summarise(
    aantal_chem_stalen_in_window = n(),
    concentratie_som = max(concentratie_som, na.rm = TRUE),
    window_TU_max = max(TU_max, na.rm = TRUE),
    window_TU_sum_max = max(TU_sum, na.rm = TRUE),
    window_TU_insecticide_max = max(TU_insecticide_max, na.rm = TRUE),
    window_TU_fungicide_sum = max(TU_fungicide_sum, na.rm = TRUE),

    datum_van_hoogste_piek = monsternamedatum_pest[which.max(TU_max)],
    window_driver_stof = driver_stof[which.max(TU_max)],

    .groups = "drop"
  )

# ==============================================================================
# STAP 4: RESULTAAT TERUGKOPPELEN AAN JOUW ORIGINELE DATA
# ==============================================================================

# Nu pakken we je pure, originele dataset terug en plakken we de berekeningen eraan vast.
mi_pest_finaal <- data5 %>%
  mutate(mi_sample_id = row_number()) %>% # Zelfde ID als in stap 1
  inner_join(pest_geaggregeerd, by = "mi_sample_id") %>%
  select(-mi_sample_id) # ID weer netjes weggooien

cat("✅ Voltooid. Aantal rijen in nieuwe dataset:", nrow(mi_pest_finaal), "\n")

ggplot(mi_pest_finaal %>%
         filter(ekc2_waterlichaam > 0.5)
       , aes(log(concentratie_som), mmif)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~groep)
ggplot(mi_pest_finaal, aes(log(concentratie_som), ep_tw)) +
  geom_point() +
  geom_smooth(method = "lm")
ggplot(mi_pest_finaal, aes(log(window_TU_max), ta_xw)) +
  geom_point() +
  geom_smooth(method = "lm")

model_test <- glmmTMB(mmif ~ log(concentratie_som) + (1 | meetplaats) +
                   (1 | bekken) + (1 | owl) + (1|groep),
                 data = mi_pest_finaal,
                 family = ordbeta
)
summary(model_test)

model_test <- glmmTMB(spear_pesticides ~ log_c_som + ekc2_waterlichaam + (1 | meetplaats) +
                        (1 | bekken) + (1 | owl) ,
                      data = mi_pest_finaal %>%
                        mutate(log_c_som = log(concentratie_som+0.01)),
                      family = ordbeta
)
model_test <- glmmTMB(mmif ~ log_c_som + ekc2_waterlichaam + (1 | meetplaats) +
                        (1 | bekken) + (1 | owl) ,
                      data = mi_pest_finaal %>%
                        mutate(log_c_som = log(concentratie_som+0.01)),
                      family = ordbeta
)

model_test <- glmmTMB(ept_prop ~ log_pest * ekc2_waterlichaam + (1 | meetplaats) +
                        (1 | bekken) + (1 | owl) ,
                      data = mi_pest_finaal %>%
                        mutate(log_pest = log(window_TU_max+0.01),
                               ept_prop = ep_tw/ta_xw),
                      family = binomial,
                      weights = ta_xw
)
summary(model_test)
model_test <- glmmTMB(sw_dw ~ log_pest * ekc2_waterlichaam + (1 | meetplaats) +
                        (1 | bekken) + (1 | owl) ,
                      data = mi_pest_finaal %>%
                        mutate(log_pest = log(window_TU_max+0.01)),
                      family =gaussian
)

summary(model_test)
plot_model(model_test, type = "pred")
# Gebruik een vector c("var1", "var2") in plaats van de dubbele punt
plot_model(model_test,
           type = "pred",
           terms = c("log_pest", "ekc2_waterlichaam"),
           show.data = TRUE) +
  theme_minimal() +
  labs(
    title = "Effect van pesticiden op SPEAR per habitatklasse",
    x = "Log(Concentratie Som)",
    y = "SPEAR Pesticides (Voorspeld)"
  )
simulateResiduals(model_test, plot = TRUE)
mi_pest_finaal$spear_pesticides %>% log %>% hist


######
#visueel checken spearpest en TU
#####
library(ggplot2)
library(dplyr)

# 1. Data voorbereiden: TU log-transformeren en categoriseren
df_viz <- tu_owl_geaggregeerd %>%
  filter(!is.na(window_TU_max), !is.na(spear_pesticides)) %>%
  mutate(
    # We voegen een heel klein getal toe om log10(0) te vermijden
    log_TU_max = log10(window_TU_max + 1e-5),

    # Drempelwaarden instellen (pas deze gerust aan op basis van jouw data!)
    tu_klasse = case_when(
      log_TU_max < -4 ~ "1. Geen/Laag risico (< -4)",
      log_TU_max >= -4 & log_TU_max < -2 ~ "2. Matig risico (-4 tot -2)",
      log_TU_max >= -2 ~ "3. Hoog risico (> -2)"
    )
  )

# 2. De Boxplot tekenen
ggplot(df_viz, aes(x = tu_klasse, y = spear_pesticides, fill = tu_klasse)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.4, color = "black") + # Toont de ruwe datapunten erachter
  scale_fill_manual(values = c("#2c7bb6", "#ffffbf", "#d7191c")) +
  theme_minimal() +
  labs(
    title = "Effect van Pesticiden (TU max) op SPEAR Index",
    subtitle = "Een duidelijke daling in SPEAR bij overschrijding van de toxiciteitsdrempel",
    x = "Log10(TU max) Risicoklasse",
    y = "SPEAR Pesticides [0-1]"
  ) +
  theme(legend.position = "none")

ggplot(df_viz, aes(x = log_TU_max, y = spear_pesticides)) +
  geom_point(alpha = 0.6, color = "steelblue") +

  # Vloeiende trendlijn die de daling volgt (niet lineair)
  geom_smooth(method = "loess", color = "darkred", fill = "red", alpha = 0.2) +

  # Visuele drempelwaarde toevoegen (bv. log(TU) = -2)
  geom_vline(xintercept = -2, linetype = "dashed", color = "black", size = 1) +

  annotate("text", x = -1.8, y = 0.9, label = "Toxische Drempelwaarde\n(Ecosysteem crash)", hjust = 0) +

  theme_minimal() +
  labs(
    title = "Scatterplot: SPEAR vs. Toxische Druk",
    x = "Maximale Toxische Druk (log10 TU)",
    y = "SPEAR Pesticides [0-1]"
  )


#####
# OWL score
######
meetnetten <- read.delim("data/ruw/vmm/meetnetten.txt")

tu_owl <- tu_master_mi %>%
  left_join(meetnetten %>%
              select(owl_code, nummer),
            by = c("meetplaats" = "nummer"))


bereken_owl_tu <- function(doeljaar, data) {

  # Definieer het venster: 2 jaar vóór 1 april van het doeljaar tot eind maart van het doeljaar
  start_datum <- make_date(doeljaar - 2, 4, 1)
  eind_datum  <- make_date(doeljaar, 3, 31)

  data %>%
    # Filter de rijen die binnen het 2-jarig venster vallen
    filter(monsternamedatum >= start_datum & monsternamedatum <= eind_datum) %>%

    # Groepeer op jouw specifieke waterlichaam code
    group_by(owl_code) %>%

    summarise(
      analyse_jaar = doeljaar,

      # --- METADATA ---
      aantal_stalen_in_venster = n(),
      aantal_unieke_meetplaatsen = n_distinct(meetplaats),

      # --- TOXISCHE DRUK (De pieken over 2 jaar) ---
      owl_TU_max = max(TU_max, na.rm = TRUE),
      owl_TU_sum_max = max(TU_sum, na.rm = TRUE),
      owl_TU_insecticide_max = max(TU_insecticide_max, na.rm = TRUE),
      owl_TU_herbicide_max = max(TU_herbicide_sum, na.rm = TRUE),
      owl_TU_neonics_max = max(TU_neonics_sum, na.rm = TRUE),
      owl_TU_fungicide_max = max(TU_fungicide_sum, na.rm = TRUE),
      owl_concentratie_som_max = max(concentratie_som, na.rm = TRUE),

      # --- IDENTIFICATIE VAN DE PIEK ---
      # Welke stof, op welke plaats en datum veroorzaakte de absolute piek in dit waterlichaam?
      piek_driver_stof = driver_stof[which.max(TU_max)],
      piek_meetplaats = meetplaats[which.max(TU_max)],
      piek_datum = monsternamedatum[which.max(TU_max)],

      .groups = "drop"
    ) %>%
    # Filter eventuele NA owl_codes eruit (voor het geval er ergens lege coördinaten waren)
    filter(!is.na(owl_code))
}

# ==============================================================================
# STAP 2: UITVOEREN VOOR ALLE JAREN (2012 - 2023)
# ==============================================================================

# Definieer de jaren waarvoor je de MI-data wilt voorspellen
target_jaren <- 2012:2023

# purrr::map_dfr loopt razendsnel over elk jaar en plakt de resultaten onder elkaar
tu_owl_geaggregeerd <- map_dfr(
  target_jaren,
  ~bereken_owl_tu(doeljaar = .x, data = tu_owl)
)

# Bekijk het resultaat
cat("✅ Aggregatie voltooid.\n")
cat("Aantal rijen in nieuwe dataset:", nrow(tu_owl_geaggregeerd), "\n")
head(tu_owl_geaggregeerd)

# ==============================================================================
# STAP 3: DE KOPPELING AAN JE MACROINVERTEBRATEN DATA
# ==============================================================================

# Stel dat je MI data 'mi_data' heet, en ook een 'owl_code' en 'jaar' kolom heeft:
# (Zorg dat het jaar in je MI data overeenkomt met het voorjaarsjaar)

finale_model_dataset <- data5 %>%
  mutate( nst_prop = ns_tw / ta_xw,
          stress_prop = (ep_tw + ns_tw)/ta_xw,
          mt_sw_prop = mt_sw / 10,
          ept_prop = ep_tw / ta_xw) %>%
  inner_join(tu_owl_geaggregeerd, by = c("owl" = "owl_code", "jaar" = "analyse_jaar"))

owl_pest_model <- glmmTMB(mmif ~ owl_concentratie_som_max* ekc2_waterlichaam + (1|owl) + (1|meetplaats) + (1|bekken),
                          data = finale_model_dataset,
                          family = ordbeta)
summary(owl_pest_model)
plot_model(owl_pest_model, "pred")

owl_pest_model <- glmmTMB(log(owl_TU_sum_max) ~ scale(intensiteit_combo_afstr) + (1|owl)  + (1|bekken),
                          data = finale_model_dataset %>% filter(groep == "beek"),
                          family = gaussian)
summary(owl_pest_model)

# Log-transformatie is cruciaal, want TU data is extreem scheef verdeeld!
model_data <- finale_model_dataset %>%
  mutate(

    log_TU_max = log10(owl_TU_max + 1e-6),
    log_TU_ins = log10(owl_TU_insecticide_max + 1e-6),
    log_TU_sum = log10(owl_TU_sum_max + 1e-6)
  )

# Bouw de modellen
m1 <- lm(spear_pesticides ~ log_TU_max, data = model_data)
m2 <- lm(spear_pesticides ~ log_TU_ins, data = model_data)
m3 <- glmmTMB(spear_pesticides ~ log_TU_sum + (1|owl) + (1|meetplaats), data = model_data, family = ordbeta)
summary(m3)

# Vergelijk ze (de laagste AIC is het beste model)
AIC(m1, m2, m3)


df_viz <- model_data %>%
  filter(!is.na(log_TU_sum), !is.na(spear_pesticides)) %>%
  mutate(
    # We voegen een heel klein getal toe om log10(0) te vermijden

    # Drempelwaarden instellen (pas deze gerust aan op basis van jouw data!)
    tu_klasse = case_when(
      log_TU_max < -4 ~ "1. Geen/Laag risico (< -4)",
      log_TU_max >= -4 & log_TU_max < -2 ~ "2. Matig risico (-4 tot -2)",
      log_TU_max >= -2 ~ "3. Hoog risico (> -2)"
    )
  )


glmmTMB(spear_pesticides ~ log_TU_ins * log_TU_fungicide + (1|meetplaats)...)


# koppeling aan mafy data
finale_model_dataset_mafy <- mafy_data3 %>%
  inner_join(tu_owl_geaggregeerd, by = c("owl" = "owl_code", "jaar" = "analyse_jaar"))

owl_pest_model <- glmmTMB(v_zonder_gep ~ owl_concentratie_som_max_log + ekc2_waterlichaam + (1|owl) + (1|meetplaats) + (1|bekken),
                          data = finale_model_dataset_mafy %>%
                            mutate(owl_concentratie_som_max_log = log(owl_concentratie_som_max)),
                          family = ordbeta)
summary(owl_pest_model)
plot_model(owl_pest_model, "pred")

plot_model(owl_pest_model,
           type = "pred",
           terms = c("owl_concentratie_som_max_log", "ekc2_waterlichaam"),
           show.data = TRUE) +
  theme_minimal() +
  labs(
    title = "Effect van pesticiden op SPEAR per habitatklasse",
    x = "Log(Concentratie Som)",
    y = "SPEAR Pesticides (Voorspeld)"
  )
