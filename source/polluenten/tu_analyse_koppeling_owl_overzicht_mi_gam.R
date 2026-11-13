# ==============================================================================
# STAP 0: SETUP & LIBRARIES
# ==============================================================================
library(dplyr)
library(lubridate)
library(purrr)
library(mgcv)      # Voor GAMs in plaats van glmmTMB
library(ggeffects) # Voor het plotten van de niet-lineaire voorspellingen
library(ggplot2)
library(tidyr)

source(here::here("source", "analyse", "mi_datasets_prep.R"))

load(file = here("data", "verwerkt", "polluenten", "tu_master_macroinvertebrates.rdata"))
# Zorg dat tu_owl beschikbaar is
meetnetten <- read.delim("data/ruw/vmm/meetnetten.txt")
tu_owl <- tu_master_mi %>%
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
      owl_TU_insecticide_max = max(TU_insecticide_max, na.rm = TRUE),
      owl_TU_fungicide_sum = max(TU_fungicide_sum, na.rm = TRUE),
      owl_concentratie_som_max = max(concentratie_som, na.rm = TRUE),

      .groups = "drop"
    ) %>%
    filter(!is.na(owl_code)) %>%
    # Log-transformaties (Offset op 1e-2 zoals in jouw input)
    mutate(
      log_TU_max = log10(owl_TU_max + 1e-2),
      log_TU_sum = log10(owl_TU_sum_max + 1e-2),
      log_TU_ins = log10(owl_TU_insecticide_max + 1e-2),
      log_TU_fun = log10(owl_TU_fungicide_sum + 1e-2),
      log_c_som  = log10(owl_concentratie_som_max + 1e-2)
    )
}

# Uitvoeren voor alle jaren
target_jaren <- 2012:2023
tu_owl_geaggregeerd <- map_dfr(target_jaren, ~bereken_owl_tu(doeljaar = .x, data = tu_owl))
cat("✅ Pesticiden aggregatie voltooid. Rijen:", nrow(tu_owl_geaggregeerd), "\n")

# ==============================================================================
# STAP 2: KOPPELEN AAN MACROINVERTEBRATEN (data5) & DATA PREP
# ==============================================================================

finale_model_dataset <- data5 %>%
  # filter(groep == "beek") %>%
  mutate(
    nst_prop = ns_tw / ta_xw,
    stress_prop = (ep_tw + ns_tw) / ta_xw,
    mt_sw_prop = mt_sw / 10,
    ept_prop = ep_tw / ta_xw
  ) %>%
  inner_join(tu_owl_geaggregeerd, by = c("owl" = "owl_code", "jaar" = "analyse_jaar")) %>%
  mutate(
    # mgcv eist dat random effects een factor zijn
    owl = as.factor(owl),

    # Beta regressie (betar) vereist waarden strikt tussen 0 en 1.
    # LET OP: We squeezen HIER ENKEL de 0-1 indexen, NIET de count of continue data!
    across(c(mmif, spear_pesticides, mt_sw_prop),
           ~ case_when(. <= 0 ~ 0.001, . >= 1 ~ 0.999, TRUE ~ .))
  )

cat("✅ Dataset klaar voor GAM-modellen. Rijen:", nrow(finale_model_dataset), "\n")

# ==============================================================================
# STAP 3: SETUP, CONFIGURATIE & SUPER-LOOP VOOR MGCV
# ==============================================================================

# 1. Definieer je modelconfiguratie: Aangepast naar de juiste families in mgcv
model_config <- list(
  mmif             = list(family = betar(link="logit"), weight = NULL),
  spear_pesticides = list(family = betar(link="logit"), weight = NULL),
  ept_prop         = list(family = binomial(link="logit"), weight = "ta_xw"),
  nst_prop         = list(family = binomial(link="logit"), weight = "ta_xw"),
  stress_prop      = list(family = binomial(link="logit"), weight = "ta_xw"),
  mt_sw_prop       = list(family = betar(link="logit"), weight = NULL),
  sw_dw            = list(family = gaussian(), weight = NULL),
  ta_xw            = list(family = nb(), weight = NULL) # Negatief binomiaal voor counts
)

predictors_to_test <- c("log_TU_max", "log_TU_sum", "log_TU_ins", "log_TU_fun", "log_c_som")

model_tasks <- expand_grid(
  respons = names(model_config),
  predictor = predictors_to_test
)

# Functie om de GAM te fitten en direct de voorspelde curve uit te trekken
run_gam_combo <- function(respons, predictor, data, config_list) {

  config <- config_list[[respons]]
  familie <- config$family
  weight_col <- config$weight

  # GAM formule: k=4 beperkt het tot soepele, ecologisch logische curven.
  # s(owl, bs = 're') voegt de random effect intercept toe op waterlichaam-niveau.
  formule_str <- paste0(respons, " ~ s(", predictor, ", k = 4) + s(owl, bs = 're')")
  formule <- as.formula(formule_str)

  fit <- tryCatch({
    if (!is.null(weight_col)) {
      gam(formule, data = data, family = familie, method = "REML", weights = data[[weight_col]])
    } else {
      gam(formule, data = data, family = familie, method = "REML")
    }
  }, error = function(e) {
    message(paste("⚠️ Model faalde voor:", respons, "~", predictor))
    return(NULL)
  })

  if (is.null(fit)) return(NULL)

  # P-waarde uittrekken (de predictor is de eerste term in de smooth tabel)
  p_val <- summary(fit)$s.table[1, 4]

  # Genereer de curve via ggeffects
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

cat("⏳ GAM Curven aan het berekenen... pak even een koffie...\n")
alle_voorspellingen <- map2_dfr(
  model_tasks$respons,
  model_tasks$predictor,
  ~run_gam_combo(respons = .x, predictor = .y, data = finale_model_dataset, config_list = model_config)
)
cat("✅ Alle GAMs voltooid!\n")

# ==============================================================================
# STAP 4: VISUEEL OVERZICHT (GEFACETTEERDE CURVE MATRIX)
# ==============================================================================

# Vaste logische volgorde op de assen
alle_voorspellingen <- alle_voorspellingen %>%
  mutate(respons_var = factor(respons_var, levels = names(model_config)))
alle_voorspellingen <- alle_voorspellingen %>%
  mutate(
    # 1. Hernoem de predictoren (kolommen)
    predictor_var = case_when(
      predictor_var == "log_TU_max" ~ "TU max (log)",
      predictor_var == "log_TU_sum" ~ "TU som (log)",
      predictor_var == "log_TU_ins" ~ "TU insecticiden (log)",
      predictor_var == "log_TU_fun" ~ "TU fungiciden (log)",
      predictor_var == "log_c_som"  ~ "Concentratie som (log)",
      TRUE ~ predictor_var
    ),
    # 2. Factoriseer predictoren in jouw gewenste volgorde
    predictor_var = factor(predictor_var, levels = c(
      "TU max (log)",
      "TU som (log)",
      "TU insecticiden (log)",
      "TU fungiciden (log)",
      "Concentratie som (log)"
    )),

    # 3. Hernoem de responsen (rijen)
    respons_var = case_when(
      respons_var == "ta_xw"       ~ "tax",
      respons_var == "ept_prop"    ~ "ept",
      respons_var == "nst_prop"    ~ "nst",
      respons_var == "sw_dw"       ~ "swd",
      respons_var == "mt_sw_prop"  ~ "mts",
      respons_var == "stress_prop" ~ "stress",
      respons_var == "spear_pesticides" ~ "spear index",
      TRUE ~ respons_var # mmif en spear_pesticides blijven gelijk
    ),
    # 4. Factoriseer responsen in jouw gewenste volgorde
    respons_var = factor(respons_var, levels = c(
      "mmif",
      "tax",
      "ept",
      "nst",
      "swd",
      "mts",
      "spear index",
      "stress"
    ))
  )

overzichtsplot_gams <- ggplot(alle_voorspellingen, aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = significant), alpha = 0.2) +
  geom_line(aes(color = significant), linewidth = 1.2) +

  # Kleuren: Blauw voor significante effecten (typisch voor MI), Grijs voor ns
  scale_color_manual(values = c("FALSE" = "#B0B0B0", "TRUE" = "#0072B2"), name = "Significant Effect (p < 0.05)") +
  scale_fill_manual(values = c("FALSE" = "#B0B0B0", "TRUE" = "#0072B2"), name = "Significant Effect (p < 0.05)") +

  # scales = "free_y" is hier CRUCIAAL: ta_xw heeft waarden 10-100, mmif is 0-1
  facet_grid(respons_var ~ predictor_var, scales = "free_y") +

  theme_bw(base_size = 12) +
  labs(
    title = "Niet-lineaire Effecten van Pesticiden op Macroinvertebraten (GAMs)",
    subtitle = "Voorspelde curves. Blauwe curven wijzen op een significant drempeleffect.",
    x = "Pesticidendruk (Log schaal)",
    y = "Voorspelde Biologische Index / Variabele"
  ) +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "#EFEFEF", color = "black"),
    strip.text = element_text(face = "bold", size = 10)
  )

print(overzichtsplot_gams)

# Exporteer in hoge resolutie (dit wordt een forse plot, pas hoogte/breedte aan indien nodig)
ggsave("pesticiden_gams_overzicht_mi.png", plot = overzichtsplot_gams, width = 16, height = 14, dpi = 300)

# ==============================================================================
# STAP X: UITGEBREID TESTMODEL (Aangepast voor GAM)
# ==============================================================================
# Let op de overstap naar s(variabele) voor GAM smooth termen en s(owl, bs="re") voor random effects.

test_gam <- gam(
  log_TU_max ~ s(intensiteit_combo_afstr) + s(intensiteit_combo_oeverzone) +
    s(log(p_t)) + s(log(n_t)) + s(ekc2_waterlichaam) + s(jaar) +
    s(owl, bs = "re"),
  data = finale_model_dataset,
  family = gaussian(),
  method = "REML"
)

summary(test_gam)
# plot() voor GAMs toont direct de partiële effecten van elke smooth term
plot(test_gam, pages = 1, scheme = 2, shade = TRUE, shade.col = "lightblue")
