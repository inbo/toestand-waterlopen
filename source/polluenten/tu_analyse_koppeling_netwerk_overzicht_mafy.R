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
source(here::here("source", "analyse", "mafy_datasets_prep.R"))
load(file = here("data", "verwerkt", "polluenten", "tu_master_macrophytes.rdata"))
load(file = "data/verwerkt/koppeling/koppeling_mafy_pesticides.rdata")

# ==============================================================================
# STAP 1: DE 1-OP-1 KOPPELING (Jouw Code)
# ==============================================================================
koppeling_sleutel_pesticides <- koppeling_mafy_pesticides %>%
  select(meetplaats, monsternamedatum, qual_meetplaats, qual_monsternamedatum) %>%
  mutate(
    monsternamedatum = as.Date(monsternamedatum),
    qual_monsternamedatum = as.Date(qual_monsternamedatum)
  )

gekoppelde_data_mafy_pesticides <- mafy_data3 %>%
  select(-any_of(c("qual_meetplaats", "qual_monsternamedatum"))) %>% # Veilige manier om kolommen te verwijderen

  # 1. Koppel de match-informatie aan je MI data
  left_join(koppeling_sleutel_pesticides %>% st_drop_geometry(), by = c("meetplaats", "monsternamedatum")) %>%

  # 2. Koppel de pesticiden master dataset
  left_join(tu_master_mafy,
            by = c("qual_meetplaats" = "meetplaats",
                   "qual_monsternamedatum" = "monsternamedatum"),
            suffix = c("", "_pest")) %>%

  # 3. Bereken de log-predictoren
  mutate(
    log_TU_max = log10(TU_max + 1e-3),
    log_TU_sum = log10(TU_sum + 1e-3),
    log_TU_core_max = log10(TU_core_max + 1e-3),
    log_TU_core_sum = log10(TU_core_sum + 1e-3),
    log_TU_her = log10(TU_herbicide_sum + 1e-3),
    log_TU_fun = log10(TU_fungicide_sum + 1e-3),
    log_TU_neo = log10(TU_neonics_sum + 1e-3),
    log_c_som  = log10(concentratie_som + 1e-3)
  ) %>%

  # Zorg dat we alleen rijen overhouden waar effectief pesticiden voor gekoppeld zijn
  filter(!is.na(log_TU_max))

cat("✅ Dataset gekoppeld. Aantal MI-stalen met pesticiden:", nrow(gekoppelde_data_mafy_pesticides), "\n")

# ==============================================================================
# STAP 2: CONFIGURATIE VAN DE MODELLEN
# ==============================================================================

model_config <- list(
  index_nieuw   = list(family = ordbeta, weight = NULL),
  v_zonder_gep  = list(family = ordbeta, weight = NULL),
  vo_zonder_gep = list(family = ordbeta, weight = NULL),
  gv_zonder_gep = list(family = ordbeta, weight = NULL),
  ts_zonder_gep = list(family = ordbeta, weight = NULL)
)

predictors_to_test <- c(
  "log_TU_max", "log_TU_sum",
  "log_TU_core_max", "log_TU_core_sum",
  "log_TU_her", "log_TU_fun",
  "log_TU_neo", "log_c_som"
)

model_tasks <- expand_grid(
  respons = names(model_config),
  predictor = predictors_to_test
)

# ==============================================================================
# STAP 3: DE MODEL-LOOP
# ==============================================================================
#
# run_combo_model <- function(respons, predictor, data, config_list) {
#
#   config <- config_list[[respons]]
#   familie <- config$family
#   weight_col <- config$weight
#
#   # Formule opstellen. Omdat dit specifieke meetplaatsen zijn, behouden we (1|meetplaats) en ruimtelijke geneste factoren
#   formule_str <- paste0(respons, " ~ ", predictor, " + scale(jaar) + (1|owl) + (1|meetplaats) + (1|bekken)")
#   formule <- as.formula(formule_str)
#
#   fit <- tryCatch({
#     if (!is.null(weight_col)) {
#       glmmTMB(formule, data = data, family = familie(), weights = data[[weight_col]])
#     } else {
#       glmmTMB(formule, data = data, family = familie())
#     }
#   }, error = function(e) {
#     message(paste("⚠️ Model faalde voor:", respons, "~", predictor))
#     return(NULL)
#   })
#
#   if (is.null(fit)) return(NULL)
#
#   res <- tidy(fit, effects = "fixed", conf.int = TRUE) %>%
#     filter(term == predictor) %>%
#     mutate(
#       respons_var = respons,
#       predictor_var = predictor,
#       significatie = case_when(
#         p.value < 0.001 ~ "***",
#         p.value < 0.01 ~ "**",
#         p.value < 0.05 ~ "*",
#         TRUE ~ "ns"
#       )
#     )
#   return(res)
# }
#
# cat("⏳ 64 modellen aan het fitten... dit duurt even...\n")
# alle_resultaten_direct <- map2_dfr(
#   model_tasks$respons,
#   model_tasks$predictor,
#   ~run_combo_model(respons = .x, predictor = .y, data = gekoppelde_data_mafy_pesticides, config_list = model_config)
# )
# cat("✅ Alle modellen voltooid!\n")
#
# library(tibble)

run_combo_model <- function(respons, predictor, data, config_list) {

  config <- config_list[[respons]]
  familie <- config$family
  weight_col <- config$weight

  formule_str <- paste0(respons, " ~ ", predictor, " + (1|owl)")
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
    # Voor als het uittrekken van de statistiek (broom.mixed) crasht
    return(tibble(
      term = predictor, estimate = NA, std.error = NA, statistic = NA, p.value = NA,
      conf.low = NA, conf.high = NA, respons_var = respons, predictor_var = predictor,
      status = "GEFAALD_NA_FIT", error_message = e$message, significatie = NA
    ))
  })

  return(res)
}

# Draai de loop opnieuw (gebruik je gekoppelde dataset)
alle_resultaten_direct <- map2_dfr(
  model_tasks$respons,
  model_tasks$predictor,
  ~run_combo_model(respons = .x, predictor = .y, data = gekoppelde_data_mafy_pesticides, config_list = model_config)
)

foutenrapport <- alle_resultaten_direct %>%
  filter(status != "OK") %>%
  select(respons_var, predictor_var, error_message)

print(foutenrapport)

# ==============================================================================
# STAP 4: VISUEEL OVERZICHT (FOREST PLOT)
# ==============================================================================

# Orden de variabelen logisch voor de Y-as
alle_resultaten_direct <- alle_resultaten_direct %>%
  mutate(
    respons_var = factor(respons_var, levels = rev(names(model_config))),
    # Sorteer ook de panelen (predictoren) logisch
    predictor_var = factor(predictor_var, levels = predictors_to_test)
  )

overzichtsplot_direct <- ggplot(alle_resultaten_direct, aes(x = estimate, y = respons_var, color = p.value < 0.05)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.8) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.3, linewidth = 0.8) +
  scale_color_manual(values = c("FALSE" = "#B0B0B0", "TRUE" = "#0072B2"), # Blauw voor significant
                     name = "Significant (p < 0.05)") +
  # 4 kolommen, 2 rijen om 8 plots mooi weer te geven
  facet_wrap(~ predictor_var, ncol = 4, scales = "free_x") +
  theme_bw(base_size = 12) +
  labs(
    title = "Effect van Pesticiden op Macroinvertebraten (Directe Koppeling)",
    subtitle = "Modellen op basis van specifieke gekoppelde stalen",
    x = "Model Estimate (Negatief = Index daalt door pesticide)",
    y = "Biologische Variabele"
  ) +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "#EFEFEF", color = "black"),
    strip.text = element_text(face = "bold", size = 10),
    panel.grid.major.y = element_line(color = "gray90", linetype = "dotted")
  )

print(overzichtsplot_direct)


# ==============================================================================
# STAP X:
# ==============================================================================

test <- glmmTMB(data = finale_model_dataset,
                log_TU_max ~ scale(intensiteit_combo_afstr) + scale(intensiteit_combo_oever) + log(qual_p_t) + log(qual_n_t) + scale(ekc2_waterlichaam) + scale(jaar) + (1|owl),
                family = gaussian)
summary(test)
simulateResiduals(test, plot = TRUE)

finale_model_dataset$log_TU_max %>% hist()
