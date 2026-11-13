# ==============================================================================
# STAP 0: LIBRARIES LADEN
# ==============================================================================
library(dplyr)
library(purrr)
library(glmmTMB)
library(broom.mixed)
library(ggplot2)
library(tidyr)
library(sf)
load(file = "data/verwerkt/koppeling/koppeling_mi_paks.rdata")
source(here::here("source", "analyse", "mi_datasets_prep.R"))
load(file = here("data", "verwerkt", "polluenten", "tu_master_paks_macroinvertebrates.rdata"))

# ==============================================================================
# STAP 1: DE 1-OP-1 KOPPELING
# ==============================================================================
koppeling_sleutel_paks <- koppeling_mi_paks %>%
  select(meetplaats, monsternamedatum, qual_meetplaats, qual_monsternamedatum) %>%
  mutate(
    monsternamedatum = as.Date(monsternamedatum),
    qual_monsternamedatum = as.Date(qual_monsternamedatum)
  )

gekoppelde_data_mi_paks <- data5 %>%
  mutate(
    nst_prop = ns_tw / ta_xw,
    stress_prop = (ep_tw + ns_tw) / ta_xw,
    mt_sw_prop = mt_sw / 10,
    ept_prop = ep_tw / ta_xw
  ) %>%
  select(-any_of(c("qual_meetplaats", "qual_monsternamedatum"))) %>%

  # 1. Koppel de match-informatie aan je MI data
  left_join(koppeling_sleutel_paks %>% st_drop_geometry(), by = c("meetplaats", "monsternamedatum")) %>%

  # 2. Koppel de pesticiden master dataset
  left_join(tu_master_mi_pak,
            by = c("qual_meetplaats" = "meetplaats",
                   "qual_monsternamedatum" = "monsternamedatum"),
            suffix = c("", "_pak")) %>%

  # 3. Bereken de log-predictoren
  mutate(
    log_TU_max = log10(TU_max + 1e-2),
    log_TU_sum = log10(TU_sum + 1e-2),
    log_TU_core_max = log10(TU_core_max + 1e-2),
    log_TU_core_sum = log10(TU_core_sum + 1e-2),
    log_TU_target_paks_sum = log10(TU_target_paks_sum + 1e-2),
    log_TU_target_paks_max = log10(TU_target_paks_max + 1e-2),
    log_c_som  = log10(concentratie_som + 1e-2),
    n_t_log = log(n_t),
    p_t_log = log(p_t),
    o2_s = scale(o2),
    ec_20_log = log(ec_20),
    czv_log = log(czv)
  ) %>%

  # Zorg dat we alleen rijen overhouden waar effectief pesticiden/paks voor gekoppeld zijn
  filter(!is.na(log_TU_max)) %>%
  drop_na(mmif) %>%
  drop_na(ept_prop)

cat("✅ Dataset gekoppeld. Aantal MI-stalen met paks:",     nrow(gekoppelde_data_mi_paks), "\n")

# ==============================================================================
# STAP 2: CONFIGURATIE VAN DE MODELLEN
# ==============================================================================

model_config <- list(
  mmif             = list(family = ordbeta, weight = NULL),
  ept_prop         = list(family = binomial, weight = "ta_xw"),
  nst_prop         = list(family = binomial, weight = "ta_xw"),
  mt_sw_prop       = list(family = ordbeta, weight = NULL),
  sw_dw            = list(family = gaussian, weight = NULL),
  ta_xw            = list(family = nbinom2,  weight = NULL)
)

predictors_to_test <- c(
  "log_TU_max", "log_TU_sum", "log_TU_target_paks_max", "log_TU_target_paks_sum", "log_c_som"
)

# Definieer de gewenste volgorde voor de biologische variabelen (korte namen)
respons_levels <- rev(c("mmif", "tax", "swd", "nst", "mts", "ept"))

model_tasks <- expand_grid(
  respons = names(model_config),
  predictor = predictors_to_test
)

# ==============================================================================
# STAP 3: DE MODEL-LOOP
# ==============================================================================

run_combo_model <- function(respons, predictor, data, config_list) {

  config <- config_list[[respons]]
  familie <- config$family
  weight_col <- config$weight

  formule_str <- paste0(respons, " ~ ", predictor, "+ n_t_log + p_t_log + czv_log + o2_s + ec_20_log + (1 | groep) + (1|meetplaats) + (1|owl)")
  formule <- as.formula(formule_str)

  # Deel 1: Fit het model en vang de foutmelding op
  fit <- tryCatch({
    if (!is.null(weight_col)) {
      glmmTMB(formule, data = data, family = familie(), weights = data[[weight_col]])
    } else {
      glmmTMB(formule, data = data, family = familie())
    }
  }, error = function(e) {
    # Geef een log-rij terug met de specifieke error in plaats van NULL
    return(tibble(
      term = predictor, estimate = NA, std.error = NA, statistic = NA, p.value = NA,
      conf.low = NA, conf.high = NA, respons_var = respons, predictor_var = predictor,
      status = "GEFAALD", error_message = e$message, significatie = NA
    ))
  })

  # Check of het resultaat uit de tryCatch onze fouten-tibble is
  if (is.data.frame(fit) && "status" %in% names(fit) && fit$status[1] == "GEFAALD") {
    return(fit)
  }

  # Deel 2: Als het model lukte, trek de statistiek eruit
  res <- tryCatch({
    tidy(fit, effects = "fixed", conf.int = TRUE) %>%
      filter(term == predictor) %>%
      mutate(
        respons_var = respons,
        predictor_var = predictor,
        status = "OK",
        error_message = NA,
        significatie = case_when(
          p.value < 0.001 ~ "***",
          p.value < 0.01 ~ "**",
          p.value < 0.05 ~ "*",
          TRUE ~ "ns"
        )
      )
  }, error = function(e) {
    return(tibble(
      term = predictor, estimate = NA, std.error = NA, statistic = NA, p.value = NA,
      conf.low = NA, conf.high = NA, respons_var = respons, predictor_var = predictor,
      status = "GEFAALD_NA_FIT", error_message = e$message, significatie = NA
    ))
  })

  return(res)
}

# Draai de loop
alle_resultaten_direct <- map2_dfr(
  model_tasks$respons,
  model_tasks$predictor,
  ~run_combo_model(respons = .x, predictor = .y, data = gekoppelde_data_mi_paks, config_list = model_config)
)

foutenrapport <- alle_resultaten_direct %>%
  filter(status != "OK") %>%
  select(respons_var, predictor_var, error_message)

print(foutenrapport)

# ==============================================================================
# STAP 4: VISUEEL OVERZICHT (FOREST PLOT)
# ==============================================================================

# Definieer de NIEUWE namen voor de volgorde van de panelen
predictors_to_test_new <- c(
  "TU max (log)",
  "TU som (log)",
  "TU target PAKs max (log)",
  "TU target PAKs som (log)",
  "Concentratie (log)"
)

# Orden de variabelen logisch voor de Y-as en pas namen aan
alle_resultaten_direct <- alle_resultaten_direct %>%
  filter(!predictor_var %in% c("log_TU_core_max", "log_TU_core_sum")) %>%
  mutate(
    # Hernoem de biologische variabelen (respons)
    respons_var = case_when(
      respons_var == "ta_xw" ~ "tax",
      respons_var == "ept_prop" ~ "ept",
      respons_var == "nst_prop" ~ "nst",
      respons_var %in% c("mt_sw", "mt_sw_prop") ~ "mts",
      respons_var == "sw_dw" ~ "swd",
      TRUE ~ respons_var
    ),
    # Zet om naar factor met de nieuwe kortere namen
    respons_var = factor(respons_var, levels = respons_levels),

    # Hernoem de predictoren
    predictor_var = case_when(
      predictor_var == "log_TU_max" ~ "TU max (log)",
      predictor_var == "log_TU_sum" ~ "TU som (log)",
      predictor_var == "log_TU_target_paks_max" ~ "TU target PAKs max (log)",
      predictor_var == "log_TU_target_paks_sum" ~ "TU target PAKs som (log)",
      predictor_var == "log_c_som" ~ "Concentratie (log)",
      TRUE ~ predictor_var
    ),
    # Sorteer de panelen met de NIEUWE vector!
    predictor_var = factor(predictor_var, levels = predictors_to_test_new)
  )


overzichtsplot_direct <- ggplot(alle_resultaten_direct, aes(x = estimate, y = respons_var, color = p.value < 0.05)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.8) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.3, linewidth = 0.8) +
  scale_color_manual(values = c("FALSE" = "#B0B0B0", "TRUE" = "#0072B2"),
                     name = "Significant (p < 0.05)") +
  # Aangepast naar ncol = 3
  facet_wrap(~ predictor_var, ncol = 3, scales = "free_x") +
  theme_bw(base_size = 12) +
  labs(
    title = "Effect van PAKs op Macroinvertebraten (Directe Koppeling)",
    subtitle = "Modellen op basis van specifieke gekoppelde stalen",
    x = "Model Estimate (Negatief = Index daalt door paks)",
    y = "Biologische Variabele"
  ) +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "#EFEFEF", color = "black"),
    strip.text = element_text(face = "bold", size = 10),
    panel.grid.major.y = element_line(color = "gray90", linetype = "dotted")
  )

print(overzichtsplot_direct)

ggsave(
  filename =  here("output", "figuren", "PAKs_netwerk_overzicht.png"),
  plot = overzichtsplot_direct, # Expliciet de laatste plot kiezen
  width = 40,
  height = 20,
  units = "cm",
  dpi = 300,
  bg = "white"
)
# ==============================================================================
# STAP 1: COVARIATEN EXTRAHEREN (UIT 1 REPRESENTATIEVE SET MODELLEN)
# ==============================================================================
library(purrr)

run_covariates_model <- function(respons, data, config_list) {

  config <- config_list[[respons]]
  familie <- config$family
  weight_col <- config$weight

  # We gebruiken log_TU_max als standaard predictor om de modellen te fitten,
  # maar we zijn nu enkel geïnteresseerd in de covariaten.
  formule_str <- paste0(respons, " ~ log_TU_max + n_t_log + p_t_log + czv_log + o2_s + ec_20_log + (1 | groep) + (1|meetplaats) + (1|owl)")
  formule <- as.formula(formule_str)

  fit <- tryCatch({
    if (!is.null(weight_col)) {
      glmmTMB(formule, data = data, family = familie(), weights = data[[weight_col]])
    } else {
      glmmTMB(formule, data = data, family = familie())
    }
  }, error = function(e) {
    return(tibble(
      term = NA, estimate = NA, std.error = NA, statistic = NA, p.value = NA,
      conf.low = NA, conf.high = NA, respons_var = respons,
      status = "GEFAALD", error_message = e$message
    ))
  })

  if (is.data.frame(fit) && "status" %in% names(fit) && fit$status[1] == "GEFAALD") {
    return(fit)
  }

  res <- tryCatch({
    tidy(fit, effects = "fixed", conf.int = TRUE) %>%
      # We filteren hier specifiek de covariaten eruit
      filter(term %in% c("n_t_log", "p_t_log", "czv_log", "o2_s", "ec_20_log")) %>%
      mutate(
        respons_var = respons,
        status = "OK",
        error_message = NA
      )
  }, error = function(e) {
    return(tibble(
      term = NA, estimate = NA, std.error = NA, statistic = NA, p.value = NA,
      conf.low = NA, conf.high = NA, respons_var = respons,
      status = "GEFAALD_NA_FIT", error_message = e$message
    ))
  })

  return(res)
}

cat("⏳ Modellen fitten om covariaten te extraheren (dit gaat sneller, het zijn er maar 8)...\n")
resultaten_covariaten <- map_dfr(
  names(model_config),
  ~run_covariates_model(respons = .x, data = gekoppelde_data_mi_paks, config_list = model_config)
)
cat("✅ Extractie covariaten voltooid!\n")


# ==============================================================================
# STAP 2: VISUEEL OVERZICHT COVARIATEN (FOREST PLOT)
# ==============================================================================

# Maak de dataset klaar voor de plot
plot_data_covariaten <- resultaten_covariaten %>%
  filter(status == "OK") %>%
  mutate(
    # Logische volgorde responsen (net als in je vorige plot)
    respons_var = factor(respons_var, levels = rev(names(model_config))),

    # Geef de covariaten leesbare en nette namen voor in de plot panelen
    term_label = case_when(
      term == "n_t_log"   ~ "Log(Stikstof totaal)",
      term == "p_t_log"   ~ "Log(Fosfor totaal)",
      term == "czv_log"   ~ "Log(CZV)",
      term == "o2_s"      ~ "Zuurstof (Geschaald)",
      term == "ec_20_log" ~ "Log(Geleidbaarheid)",
      TRUE ~ term
    ),
    # Specificeer de volgorde van de panelen indien gewenst
    term_label = factor(term_label, levels = c(
      "Zuurstof (Geschaald)", "Log(Stikstof totaal)", "Log(Fosfor totaal)",
      "Log(CZV)", "Log(Geleidbaarheid)"
    ))
  )

# Maak de forest plot
overzichtsplot_covariaten <- ggplot(plot_data_covariaten, aes(x = estimate, y = respons_var, color = p.value < 0.05)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.8) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.3, linewidth = 0.8) +
  # Gebruik een andere contrastkleur (bijv. donker oranje) om hem visueel te scheiden van de paks plot
  scale_color_manual(values = c("FALSE" = "#B0B0B0", "TRUE" = "#D55E00"),
                     name = "Significant (p < 0.05)") +
  # Facet per covariaat
  facet_wrap(~ term_label, ncol = 3, scales = "free_x") +
  theme_bw(base_size = 12) +
  labs(
    title = "Effect van Fysicochemische Covariaten op Macroinvertebraten",
    subtitle = "Forest plots voor covariaten (gecontroleerd voor de invloed van zware paks)",
    x = "Model Estimate (Negatief = Index daalt bij toename covariaat)",
    y = "Biologische Variabele"
  ) +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "#EFEFEF", color = "black"),
    strip.text = element_text(face = "bold", size = 10),
    panel.grid.major.y = element_line(color = "gray90", linetype = "dotted"),
    panel.spacing = unit(1, "lines") # Wat extra ademruimte tussen de facetten
  )

print(overzichtsplot_covariaten)


# ==============================================================================
# STAP X:
# ==============================================================================

gekoppelde_data_mi_paks$TU_max %>% log %>% hist


test <- glmmTMB(data = gekoppelde_data_mi_paks,
                log_TU_core_max ~ scale(intensiteit_combo_afstr) + scale(intensiteit_combo_oeverzone) + scale(verharding_afstr) + log(lozingen_industrie_ie + 0.001) + log(lozingen_rwzi_ie + 0.001) + log(lozingen_riool_ie + 0.001) + log(overstorten_blootstelling_index + 0.001) + scale(ekc2_waterlichaam) + scale(jaar) + (1|owl) ,
                family = gaussian,
                na.action = na.omit)
summary(test)

lozingen_industrie_ie + lozingen_rwzi_ie + lozingen_rwzi_p_t + lozingen_riool_ie + overstorten_blootstelling_index

simulateResiduals(test, plot = TRUE)

finale_model_dataset$log_TU_max %>% hist()
