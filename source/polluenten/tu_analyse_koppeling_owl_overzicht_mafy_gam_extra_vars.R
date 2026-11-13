# ==============================================================================
# STAP 0: SETUP & LIBRARIES
# ==============================================================================
library(dplyr)
library(lubridate)
library(purrr)
library(mgcv)      # Voor de GAM modellen ipv glmmTMB
library(ggeffects) # Voor de effect curves
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
      owl_TU_herbicide_sum = max(TU_herbicide_sum, na.rm = TRUE),
      owl_TU_fungicide_sum = max(TU_fungicide_sum, na.rm = TRUE),
      owl_concentratie_som_max = max(concentratie_som, na.rm = TRUE),

      .groups = "drop"
    ) %>%
    filter(!is.na(owl_code)) %>%
    # Log-transformaties (Offset op 1e-4 gezet om 'staart' netjes te laten aansluiten)
    mutate(
      log_TU_max = log10(owl_TU_max + 1e-4),
      log_TU_sum = log10(owl_TU_sum_max + 1e-4),
      log_TU_her = log10(owl_TU_herbicide_sum + 1e-4),
      log_TU_fun = log10(owl_TU_fungicide_sum + 1e-4),
      log_c_som  = log10(owl_concentratie_som_max + 1e-4)
    )
}

# Uitvoeren voor alle jaren
target_jaren <- 2012:2023
tu_owl_geaggregeerd <- map_dfr(target_jaren, ~bereken_owl_tu(doeljaar = .x, data = tu_owl))
cat("✅ Pesticiden aggregatie voltooid. Rijen:", nrow(tu_owl_geaggregeerd), "\n")

# ==============================================================================
# STAP 2: KOPPELEN AAN MACROFYTEN & DATA PREP VOOR MGCV
# ==============================================================================

finale_model_dataset <- mafy_data3 %>%
  inner_join(tu_owl_geaggregeerd, by = c("owl" = "owl_code", "jaar" = "analyse_jaar")) %>%
  mutate(
    # mgcv eist dat random effects een factor zijn!
    owl = as.factor(owl),

    # AANPASSING: Maak p_t_log en n_t_log aan voor de covarianties (als ze nog niet bestonden)
    p_t_log = log(qual_p_t),
    n_t_log = log(qual_n_t),

    # Beta regressie in mgcv mag geen exacte 0 of 1 bevatten. We "squeezen" de grenzen lichtjes.
    across(c(index_nieuw, v_zonder_gep, vo_zonder_gep, gv_zonder_gep, ts_zonder_gep),
           ~ case_when(. == 0 ~ 0.001, . == 1 ~ 0.999, TRUE ~ .))
  ) %>%
  # Filter NAs uit de covariabelen, anders faalt gam() op ontbrekende rijen
  filter(!is.na(p_t_log) & !is.na(n_t_log))

cat("✅ Dataset klaar voor GAM modellen. Rijen:", nrow(finale_model_dataset), "\n")

# ==============================================================================
# STAP 3: DE GAM SUPER-LOOP MET NUTRIENTEN CORRECTIE
# ==============================================================================

# Configuratie: alles is nu betar (Beta regression)
model_config <- list(
  index_nieuw   = list(family = betar(link="logit")),
  v_zonder_gep  = list(family = betar(link="logit")),
  vo_zonder_gep = list(family = betar(link="logit")),
  gv_zonder_gep = list(family = betar(link="logit")),
  ts_zonder_gep = list(family = betar(link="logit"))
)

predictors_to_test <- c("log_TU_max", "log_TU_sum", "log_TU_her", "log_TU_fun", "log_c_som")

model_tasks <- expand_grid(
  respons = names(model_config),
  predictor = predictors_to_test
)

# Functie om de GAM te fitten en direct de curve (ggeffects) te extraheren
run_gam_combo <- function(respons, predictor, data, config_list) {

  familie <- config_list[[respons]]$family

  # AANPASSING: Formule uitgebreid met fosfor en stikstof smoothers
  formule_str <- paste0(respons, " ~ s(", predictor, ", k = 4) + s(p_t_log, k = 4) + s(n_t_log, k = 4) + s(owl, bs = 're')")

  fit <- tryCatch({
    gam(as.formula(formule_str), data = data, family = familie, method = "REML")
  }, error = function(e) return(NULL))

  if (is.null(fit)) return(NULL)

  # Trek de p-waarde van de pesticide smooth term eruit
  # (rij 1 is je pesticide predictor, rij 2 is P, rij 3 is N, rij 4 is owl)
  p_val <- summary(fit)$s.table[1, 4]

  # Genereer de voorspelde curve via ggeffects
  # ggeffects houdt p_t_log en n_t_log nu automatisch op hun gemiddelde waarde.
  pred_data <- ggpredict(fit, terms = paste0(predictor, " [all]")) %>%
    as_tibble() %>%
    mutate(
      respons_var = respons,
      predictor_var = predictor,
      p_waarde = p_val,
      significant = p_val < 0.05
    )

  return(pred_data)
}

cat("⏳ GAM Curven aan het berekenen... dit duurt even...\n")
alle_voorspellingen <- map2_dfr(
  model_tasks$respons,
  model_tasks$predictor,
  ~run_gam_combo(respons = .x, predictor = .y, data = finale_model_dataset, config_list = model_config)
)
cat("✅ Alle GAMs voltooid!\n")

# ==============================================================================
# STAP 4: HET GRID VAN GGEFFECTS CURVES PLOTTEN
# ==============================================================================

# Zorg voor logische volgorde op de assen
alle_voorspellingen <- alle_voorspellingen %>%
  mutate(respons_var = factor(respons_var, levels = names(model_config)))

overzichtsplot_gams <- ggplot(alle_voorspellingen, aes(x = x, y = predicted)) +
  # Betrouwbaarheidsinterval
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = significant), alpha = 0.2) +
  # De effectcurve
  geom_line(aes(color = significant), linewidth = 1.2) +

  # Kleuren
  scale_color_manual(values = c("FALSE" = "#B0B0B0", "TRUE" = "#D55E00"), name = "Significant Effect (p < 0.05)") +
  scale_fill_manual(values = c("FALSE" = "#B0B0B0", "TRUE" = "#D55E00"), name = "Significant Effect (p < 0.05)") +

  # Raster: Y-as = Respons, X-as = Predictor
  facet_grid(respons_var ~ predictor_var, scales = "free_x") +

  theme_bw(base_size = 12) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    title = "Niet-lineaire Effecten van Pesticiden op Macrofyten-Indexen (GAMs)",
    subtitle = "Gecorrigeerd voor Total P en Total N. Oranje = significant onafhankelijk effect.",
    x = "Pesticidendruk (Log schaal)",
    y = "Voorspelde Index [0-1]"
  ) +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "#EFEFEF", color = "black"),
    strip.text = element_text(face = "bold", size = 10)
  )

print(overzichtsplot_gams)

# Exporteer in hoge resolutie (dit wordt een forse plot door de matrix structuur)
# ggsave("pesticiden_gams_nutriënten_gecorrigeerd_overzicht.png", plot = overzichtsplot_gams, width = 16, height = 10, dpi = 300)
