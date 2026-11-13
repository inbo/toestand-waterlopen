# ==============================================================================
# STAP 0: SETUP & LIBRARIES
# ==============================================================================
library(dplyr)
library(lubridate)
library(purrr)
library(glmmTMB)
library(broom.mixed) # Voor het netjes uittrekken van modelresultaten
library(ggplot2)
library(tidyr)

source(here::here("source", "analyse", "mafy_datasets_prep.R"))
load(file = here("data", "verwerkt", "polluenten", "tu_master_macrophytes.rdata"))

# Zorg dat tu_owl beschikbaar is
meetnetten <- read.delim("data/ruw/vmm/meetnetten.txt")
tu_owl <- tu_master_mafy %>%
  left_join(meetnetten %>% select(owl_code, nummer), by = c("meetplaats" = "nummer"))

# ==============================================================================
# STAP 1: OWL METHODE - 2-JARIG VENSTER PESTICIDEN
# ==============================================================================

bereken_owl_tu <- function(doeljaar, data) {
  start_datum <- make_date(doeljaar - 2, 4, 1)
  eind_datum  <- make_date(doeljaar, 3, 31)

  data %>%
    filter(monsternamedatum >= start_datum & monsternamedatum <= eind_datum) %>%
    group_by(owl_code) %>%
    summarise(
      analyse_jaar = doeljaar,
      aantal_stalen_in_venster = n(),

      owl_TU_max = max(TU_max, na.rm = TRUE),
      owl_TU_sum_max = max(TU_sum, na.rm = TRUE),
      owl_TU_herbicide_sum = max(TU_insecticide_sum, na.rm = TRUE),
      owl_TU_fungicide_sum = max(TU_fungicide_sum, na.rm = TRUE),
      owl_concentratie_som_max = max(concentratie_som, na.rm = TRUE),

      .groups = "drop"
    ) %>%
    filter(!is.na(owl_code)) %>%
    # Log-transformaties direct hier toepassen!
    mutate(
      log_TU_max = log10(owl_TU_max + 1e-3),
      log_TU_sum = log10(owl_TU_sum_max + 1e-3),
      log_TU_her = log10(owl_TU_herbicide_sum + 1e-3),
      log_TU_fun = log10(owl_TU_fungicide_sum + 1e-3),
      log_c_som  = log10(owl_concentratie_som_max + 1e-3)
    )
}

# Uitvoeren voor alle jaren
target_jaren <- 2012:2023
tu_owl_geaggregeerd <- map_dfr(target_jaren, ~bereken_owl_tu(doeljaar = .x, data = tu_owl))
cat("✅ Pesticiden aggregatie voltooid. Rijen:", nrow(tu_owl_geaggregeerd), "\n")

# ==============================================================================
# STAP 2: KOPPELEN AAN MACROINVERTEBRATEN (data5)
# ==============================================================================

finale_model_dataset <- mafy_data3 %>%
  # Koppelen op waterlichaam en jaar
  inner_join(tu_owl_geaggregeerd, by = c("owl" = "owl_code", "jaar" = "analyse_jaar"))

cat("✅ Dataset klaar voor modellen. Rijen:", nrow(finale_model_dataset), "\n")

# ==============================================================================
# STAP 1: SETUP & CONFIGURATIE
# ==============================================================================
library(dplyr)
library(purrr)
library(glmmTMB)
library(broom.mixed)
library(ggplot2)
library(tidyr) # Voor expand_grid

# 1. Definieer je modelconfiguratie (met weights!)
model_config <- list(
  index_nieuw   = list(family = ordbeta, weight = NULL),
  v_zonder_gep  = list(family = ordbeta, weight = NULL),
  vo_zonder_gep = list(family = ordbeta, weight = NULL),
  gv_zonder_gep = list(family = ordbeta, weight = NULL),
  ts_zonder_gep = list(family = ordbeta, weight = NULL)
)

# 2. Welke predictoren willen we allemaal testen?
predictors_to_test <- c(
  "log_TU_max",
  "log_TU_sum",
  "log_TU_her",
  "log_TU_fun",
  "log_c_som"
)

# Maak een tabel met álle 40 combinaties (8 indexen * 5 predictoren)
model_tasks <- expand_grid(
  respons = names(model_config),
  predictor = predictors_to_test
)

# ==============================================================================
# STAP 2: DE SUPER-LOOP MET TRYCATCH
# ==============================================================================

# Functie om één specifiek model (1 index + 1 predictor) te fitten
run_combo_model <- function(respons, predictor, data, config_list) {

  config <- config_list[[respons]]
  familie <- config$family
  weight_col <- config$weight

  formule_str <- paste0(respons, " ~ ", predictor, " + (1|owl)")
  formule <- as.formula(formule_str)

  # tryCatch voorkomt dat een error de hele loop van 40 modellen stopt
  fit <- tryCatch({
    if (!is.null(weight_col)) {
      glmmTMB(formule, data = data, family = familie(), weights = data[[weight_col]])
    } else {
      glmmTMB(formule, data = data, family = familie())
    }
  }, error = function(e) {
    message(paste("⚠️ Model faalde voor:", respons, "~", predictor))
    return(NULL)
  })

  if (is.null(fit)) return(NULL) # Skip naar de volgende als hij faalt

  # Haal de statistiek eruit
  res <- tidy(fit, effects = "fixed", conf.int = TRUE) %>%
    filter(term == predictor) %>%
    mutate(
      respons_var = respons,
      predictor_var = predictor,
      significatie = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01 ~ "**",
        p.value < 0.05 ~ "*",
        TRUE ~ "ns"
      )
    )

  return(res)
}

# START DE BEREKENING (Dit zal 1 tot 5 minuten duren)
cat("⏳ 40 modellen aan het fitten... pak even een koffie...\n")
alle_resultaten <- map2_dfr(
  model_tasks$respons,
  model_tasks$predictor,
  ~run_combo_model(respons = .x, predictor = .y, data = finale_model_dataset, config_list = model_config)
)
cat("✅ Alle modellen voltooid!\n")

# ==============================================================================
# STAP 3: VISUEEL OVERZICHT (GEFACETTEERDE FOREST PLOT)
# ==============================================================================

# Zorg voor een logische vaste volgorde van de Y-as over alle grafieken
alle_resultaten <- alle_resultaten %>%
  mutate(respons_var = factor(respons_var, levels = rev(names(model_config))))

# De ultieme plot
overzichtsplot <- ggplot(alle_resultaten, aes(x = estimate, y = respons_var, color = p.value < 0.05)) +
  # Nullijn toevoegen
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.8) +

  # Punten (effect size) en errorbars (betrouwbaarheidsintervallen)
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), height = 0.3, linewidth = 0.8, orientation = "y") +

  # Kleuren: Rood = Significant effect, Grijs = Niet significant
  scale_color_manual(values = c("FALSE" = "#B0B0B0", "TRUE" = "#D55E00"),
                     name = "Significant (p < 0.05)") +

  # Matrix maken op basis van de gebruikte predictor
  # scales = "free_x" zorgt dat de assen per predictor optimaal schalen
  facet_wrap(~ predictor_var, ncol = 3, scales = "free_x") +

  # Vormgeving
  theme_bw(base_size = 12) +
  labs(
    title = "Effect van Pesticidenvariabelen op Macroinvertebraten-Indexen",
    subtitle = "Vergelijking van effectgroottes en significantie (Mixed Models op OWL-niveau)",
    x = "Model Estimate (Hellingsgraad: Negatief = Index daalt door pesticide)",
    y = "Biologische Variabele"
  ) +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "#EFEFEF", color = "black"),
    strip.text = element_text(face = "bold", size = 11),
    panel.grid.major.y = element_line(color = "gray90", linetype = "dotted")
  )

# Laat de plot zien
print(overzichtsplot)

# Exporteer optioneel in hoge resolutie (handig voor rapportages)
ggsave("pesticiden_model_overzicht_owl_mafy.png", plot = overzichtsplot, width = 14, height = 8, dpi = 300)

# ==============================================================================
# STAP X:
# ==============================================================================

test <- glmmTMB(data = finale_model_dataset,
                log_TU_max ~ scale(intensiteit_combo_afstr) + scale(intensiteit_combo_oever) + log(qual_p_t) + log(qual_n_t) + scale(ekc2_waterlichaam) + scale(jaar) + (1|owl),
                family = gaussian)
summary(test)
simulateResiduals(test, plot = TRUE)

finale_model_dataset$log_TU_max %>% hist()
