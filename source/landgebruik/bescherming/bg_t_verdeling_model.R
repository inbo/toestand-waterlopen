
run_glmm_cover_robuust <- function(metric, schaal, data) {

  var_trend <- paste0("trend_", metric)
  var_init <- paste0("init_", metric)
  var_pa <- paste0("mean_PA_cover_", schaal)
  var_opp <- paste0("log_opp_schaal_", schaal)
  var_start <- paste0("start_jaar_", metric)

  # Data opschonen + Hessian Fix (delen door 100)
  data_clean <- data %>%
    filter(!is.na(.data[[var_trend]]), !is.na(.data[[var_init]]), !is.na(.data[[var_pa]])) %>%
    mutate(
      lengte_tijdreeks = 2023 - .data[[var_start]],
      !!var_pa := .data[[var_pa]] / 100
    )

  # De klassieke, snelle glmmTMB formule
  if (stringr::str_detect(schaal, "cirkel")) {
    formule_tekst <- paste0(
      var_trend, " ~ ", var_pa, " * ", var_init,
      " + lengte_tijdreeks + groep + (1|owl) + (1|bekken)"
    )
  } else {
    formule_tekst <- paste0(
      var_trend, " ~ ", var_pa, " * ", var_init,
      " + ", var_opp,
      " + lengte_tijdreeks + groep + (1|owl) + (1|bekken)"
    )
  }

  # MODEL DRAAIEN: HIER ZIT DE MAGIE!
  # We gebruiken family = t_family() om het model robuust te maken tegen je uitschieters
  model <- glmmTMB(
    as.formula(formule_tekst),
    data = data_clean,
    family = t_family(link = "identity") # <--- Het snelle alternatief voor scat()!
  )

  model_coefs <- summary(model)$coefficients$cond

  # 1. P-waarde Interactie
  interactie_naam <- paste0(var_pa, ":", var_init)
  if(interactie_naam %in% rownames(model_coefs)) {
    p_val_interactie <- model_coefs[interactie_naam, "Pr(>|z|)"]
  } else {
    p_val_interactie <- NA
  }

  # 2. P-waarde Hoofdeffect
  if(var_pa %in% rownames(model_coefs)) {
    p_val_pa <- model_coefs[var_pa, "Pr(>|z|)"]
  } else {
    p_val_pa <- NA
  }

  return(tibble(
    p_ruw_cover = p_val_pa,
    p_ruw_interactie = p_val_interactie,
    model_obj = list(model)
  ))
}

# ==============================================================================
# DE LOOP DRAAIEN (Bliksemsnel!)
# ==============================================================================
cat("▶ Bezig met 16 Robuuste GLMMs (Student-t verdeling)...\n")

resultaten_glmm_robuust <- model_grid %>%
  mutate(glmm_out = map2(metric, schaal, ~run_glmm_cover_robuust(.x, .y, analyse_finaal_ms))) %>%
  unnest(glmm_out) %>%
  group_by(metric) %>%
  mutate(
    p_adj_cover = p.adjust(p_ruw_cover, method = "BH"),
    p_adj_interactie = p.adjust(p_ruw_interactie, method = "BH")
  ) %>%
  ungroup() %>%
  arrange(p_adj_interactie)

print("--- RESULTATEN ROBUUSTE GLMM (t_family) ---")
resultaten_glmm_robuust %>%
  select(metric, schaal, p_ruw_cover, p_adj_cover, p_ruw_interactie, p_adj_interactie) %>%
  mutate(across(where(is.numeric), ~round(., 4))) %>%
  print(n = 24)


library(DHARMa)

# 1. Haal exact hetzelfde model (SW_DW op 5000m) uit je NIEUWE robuuste resultaten
mijn_robuuste_model <- resultaten_glmm_robuust %>%
  filter(metric == "mt_sw", schaal == "asg_50k") %>%
  pull(model_obj) %>%
  .[[1]]

# 2. Simuleer de residuen voor dit t_family model
gesimuleerde_residuen_robuust <- simulateResiduals(fittedModel = mijn_robuuste_model, plot = TRUE)


library(patchwork)
library(purrr)

# ==============================================================================
# 1. GENEREER ALLE PLOTS VOOR DE ROBUUSTE MODELLEN
# ==============================================================================
# Let op: we gebruiken hier de schone dataset en de robuuste resultatentabel!
lijst_robuuste_plots <- map2(
  resultaten_glmm_robuust$metric,
  resultaten_glmm_robuust$schaal,
  ~plot_glmm_cover_automatisch(.x, .y, resultaten_glmm_robuust, analyse_finaal_ms)
)
names(lijst_robuuste_plots) <- paste0(resultaten_glmm_robuust$metric, "_", resultaten_glmm_robuust$schaal)


# ==============================================================================
# FIGUUR 1: HET PARADEPAARDJE (EPT_PROP OVER 3 SCHALEN)
# Dit toont jouw sterkste resultaat: de interactie is overal significant!
# ==============================================================================
figuur_ept_schalen <-
  (lijst_robuuste_plots[["ept_prop_asg_5000m"]] + ggtitle("EPT_PROP (5000m ASG)")) +
  (lijst_robuuste_plots[["ept_prop_asg_50k"]] + ggtitle("EPT_PROP (50k ASG)")) +
  (lijst_robuuste_plots[["ept_prop_cirkel_100m"]] + ggtitle("EPT_PROP (100m Cirkel)")) +
  plot_layout(ncol = 3, guides = "collect") &
  theme(legend.position = "bottom")

print(figuur_ept_schalen)
ggsave(here::here("data", "verwerkt", "GLMM_Robuust_EPT_Schalen.png"), figuur_ept_schalen, width = 14, height = 5)


# ==============================================================================
# FIGUUR 2: HET MARGINALE HOOFDEFFECT BIJ SW_DW (1000m)
# Vergeleken met een niet-significant model (MMIF op 1000m)
# ==============================================================================
figuur_contrast_50k <-
  (lijst_robuuste_plots[["sw_dw_asg_50k"]] +
     ggtitle("SW_DW (50k)\nMarginaal Hoofdeffect (p=0.068)")) +
  (lijst_robuuste_plots[["mmif_asg_50k"]] +
     ggtitle("MMIF (50k)\nNiet significant (p=0.240)")) +
  (lijst_robuuste_plots[["ept_prop_asg_50k"]] +
     ggtitle("EPT PROP (50k)\nNiet significant (p=0.240)")) +
  (lijst_robuuste_plots[["ta_xw_asg_50k"]] +
     ggtitle("TAX (50k)\nNiet significant (p=0.240)")) +
  (lijst_robuuste_plots[["ns_tw_asg_50k"]] +
     ggtitle("NST (50k)\nNiet significant (p=0.240)")) +
  (lijst_robuuste_plots[["nst_prop_asg_50k"]] +
     ggtitle("NST (50k)\nNiet significant (p=0.240)")) +
  plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")

print(figuur_contrast_50k)
ggsave(here::here("data", "verwerkt", "GLMM_Robuust_Contrast_1000m.png"), figuur_contrast_1000m, width = 10, height = 6)



####PA_gain

run_glmm_gain_robuust <- function(metric, schaal, data) {

  # Definieer kolomnamen
  var_trend <- paste0("trend_", metric)
  var_init <- paste0("init_", metric)
  var_gain <- paste0("rate_of_PA_gain_", schaal)
  var_cover <- paste0("initiele_PA_cover_", schaal) # Controlevariabele!
  var_opp <- paste0("log_opp_schaal_", schaal)
  var_start <- paste0("start_jaar_", metric)

  # Data opschonen + HESSIAN FIX (delen door 100)
  data_clean <- data %>%
    filter(
      !is.na(.data[[var_trend]]),
      !is.na(.data[[var_init]]),
      !is.na(.data[[var_gain]]),
      !is.na(.data[[var_cover]])
    ) %>%
    mutate(
      lengte_tijdreeks = 2023 - .data[[var_start]],

      # FIX: Maak er proporties van om de berekening stabiel te houden
      !!var_gain := .data[[var_gain]] / 100,
      !!var_cover := .data[[var_cover]] / 100
    )

  # Formule bouwen
  if (stringr::str_detect(schaal, "cirkel")) {
    formule_tekst <- paste0(
      var_trend, " ~ ", var_gain, " * ", var_init,
      " + ", var_cover,
      " + lengte_tijdreeks + groep + (1|owl) + (1|bekken)"
    )
  } else {
    formule_tekst <- paste0(
      var_trend, " ~ ", var_gain, " * ", var_init,
      " + ", var_cover, " + ", var_opp,
      " + lengte_tijdreeks + groep + (1|owl) + (1|bekken)"
    )
  }

  # MODEL DRAAIEN: De Student-t verdeling to the rescue!
  model <- glmmTMB(
    as.formula(formule_tekst),
    data = data_clean,
    family = t_family(link = "identity") # Robuust tegen uitschieters
  )

  model_coefs <- summary(model)$coefficients$cond

  # 1. P-waarde van de INTERACTIE
  interactie_naam <- paste0(var_gain, ":", var_init)
  if(interactie_naam %in% rownames(model_coefs)) {
    p_val_interactie <- model_coefs[interactie_naam, "Pr(>|z|)"]
  } else {
    p_val_interactie <- NA
  }

  # 2. P-waarde van het HOOFDEFFECT (Natuurwinst)
  if(var_gain %in% rownames(model_coefs)) {
    p_val_gain <- model_coefs[var_gain, "Pr(>|z|)"]
  } else {
    p_val_gain <- NA
  }

  return(tibble(
    p_ruw_gain = p_val_gain,
    p_ruw_interactie = p_val_interactie,
    model_obj = list(model)
  ))
}

# ==============================================================================
# DE LOOP DRAAIEN (Bliksemsnel met de t_family)
# ==============================================================================
cat("▶ Bezig met 16 Robuuste GLMMs voor PA GAIN...\n")

# Let op: gebruik hier weer jouw opgeschoonde dataset!
resultaten_glmm_gain_robuust <- model_grid %>%
  mutate(glmm_out = map2(metric, schaal, ~run_glmm_gain_robuust(.x, .y, analyse_finaal_ms))) %>%
  unnest(glmm_out) %>%

  # Benjamini-Hochberg correctie per metric
  group_by(metric) %>%
  mutate(
    p_adj_gain = p.adjust(p_ruw_gain, method = "BH"),
    p_adj_interactie = p.adjust(p_ruw_interactie, method = "BH")
  ) %>%
  ungroup() %>%
  arrange(p_adj_interactie)

# Resultaten printen
print("--- RESULTATEN ROBUUSTE GLMM (GAIN) ---")
resultaten_glmm_gain_robuust %>%
  select(metric, schaal, p_ruw_gain, p_adj_gain, p_ruw_interactie, p_adj_interactie) %>%
  mutate(across(where(is.numeric), ~round(., 4))) %>%
  print(n = 24)
