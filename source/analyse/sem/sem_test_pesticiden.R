
## om dit te lopen run deel van tu_ec50_pesticiden_totaal_script en vervolgens EC50_tu_pesticiden_macroinvertebrates

################################################################################
# Pesticiden
################################################################################

load(file = "data/verwerkt/koppeling/koppeling_mi_pesticides.rdata")
load(file = here("data", "verwerkt", "polluenten", "tu_specific_groups_ec50_macroinvertebrates.rdata"))

koppeling_sleutel_pesticides <-
  koppeling_mi_pesticides %>%
  select(meetplaats, monsternamedatum, qual_meetplaats, qual_monsternamedatum) %>%
  mutate(
    monsternamedatum = as.Date(monsternamedatum),
    qual_monsternamedatum = as.Date(qual_monsternamedatum)
  )

gekoppelde_data_mi_pesticides <- data4 %>%
  select(-qual_meetplaats, -qual_monsternamedatum) %>%
  # 1. Koppel de match-informatie aan je MI data
  # (Zodat we weten WELK pesticiden staal bij welk MI staal hoort)
  left_join(koppeling_sleutel_pesticides %>% st_drop_geometry() , by = c("meetplaats", "monsternamedatum")) %>%

  # LET OP DE 'BY': We koppelen de 'qual_' kolommen van links aan de originele kolommen van rechts
  left_join(tu_specific_groups_mi,
            by = c("qual_meetplaats" = "meetplaats",
                   "qual_monsternamedatum" = "monsternamedatum"),
            suffix = c("", "_pest")) %>%
  left_join(spear_data,
            by = c("meetplaats", "monsternamedatum"))


gekoppelde_data_mi_pesticides %>%
  select(meetplaats, monsternamedatum,
         groep, categorie,
         TU_sum) %>%
  group_by(groep) %>%
  mutate(totaal = n()) %>%
  na.omit() %>%
  group_by(groep, totaal) %>%
  summarise(n())

test_beek <- gekoppelde_data_mi_pesticides %>%
  filter(groep %in% c("kempen")) %>%
  filter(statuut %in% c("Sterk Veranderd", "Natuurlijk")) %>%
  drop_na(meetplaats, jaar) %>%
  select(meetplaats, monsternamedatum, jaar, bekken, groep, owl,
         mmif, ta_xw, ep_tw, sw_dw, ns_tw, mt_sw,
         t, p_h, o2, o2_verz, ec_20,
         czv, n_t, no2, no3, nh4, p_t, zs,
         TU_sum, TU_max, TU_insecticide, TU_neonicotinoids, spear_pesticides, concentratie_pesticiden_sum, concentratie_insecticide, TU_insecticide_max, TU_core_sum, TU_core_insecticide_max,
         breedte_diepte_ratio, sinuositeit, bodemsub, doodhout, profiel, ekc2_waterlichaam, ekc2_traject, stroomsnelheid, # verstuwing weglaten want te veel NA
         verharding_afstr, natuur_afstr, intensiteit_combo_afstr, intensiteit_gewasbescherming_afstr, verharding_oever, natuur_oever, intensiteit_combo_oeverzone,
         spei6, n_extreme_3m, p_sum_7d,
         lozingen_industrie_ie, lozingen_rwzi_ie, lozingen_rwzi_p_t, lozingen_riool_ie, overstorten_index, overstorten_blootstelling_index, aantal_overstorten_weighted
  ) %>%
  mutate(across(.cols = c(jaar, t:aantal_overstorten_weighted), # Selects n_t and all columns to the end
                .fns = ~as.numeric(scale(.x)),
                .names = "{.col}_s")) %>%
  dplyr::mutate(ep_tw = as.integer(ep_tw),
                ta_xw = as.integer(ta_xw),
                ns_tw = as.integer(ns_tw),
                mt_sw_prop = mt_sw / 10,
                bekken = as.factor(bekken),
                nst_prop = ns_tw / ta_xw,
                stress_prop = (ep_tw + ns_tw)/ta_xw,
                ept_prop = ep_tw / ta_xw,
                n_t_log = log(n_t),
                p_t_log = log(p_t),
                no2_log = log(no2),
                no3_log = log(no3),
                czv_log = log(czv),
                nh4_log = log(nh4),
                ec_20_log = log(ec_20),
                zs_log = log(zs),
                overstorten_index_log = log(overstorten_index + 1),
                overstorten_blootstelling_index_log = log(overstorten_blootstelling_index + 1),
                lozingen_rwzi_ie_log = log(lozingen_rwzi_ie + 1),
                lozingen_rwzi_p_t_log = log(lozingen_rwzi_p_t + 1),
                lozingen_industrie_ie_log = log(lozingen_industrie_ie + 1),
                lozingen_riool_ie_log = log(lozingen_riool_ie + 1),
                TU_sum_log = log(TU_sum + 1),
                TU_insecticide_max_log = log(TU_insecticide_max + 1))

test2_beek <- test_beek %>%
  select(
    meetplaats, monsternamedatum, jaar_s, bekken, owl,
    mmif, ept_prop, ta_xw, sw_dw, mt_sw_prop, nst_prop, stress_prop,
    n_t_log, p_t_log, czv_log,
    ekc2_waterlichaam_s,
    all_of(clean_klimaat),
    all_of(clean_lozingen),
    all_of(clean_landuse), intensiteit_gewasbescherming_afstr_s,
    all_of(clean_fysico),
    TU_sum_s, TU_max_s, TU_insecticide_s, TU_insecticide_max_s, TU_neonicotinoids_s, spear_pesticides_s, concentratie_pesticiden_sum_s, concentratie_insecticide_s, TU_sum_log, spear_pesticides, TU_core_sum_s, TU_core_insecticide_max_s, TU_insecticide_max_log,
    groep
  ) %>%
  na.omit %>%
  filter(TU_insecticide_max_s < 1)

test_kempen <- gekoppelde_data_mi_pesticides %>%
  filter(groep %in% c("beek")) %>%
  filter(statuut %in% c("Sterk Veranderd", "Natuurlijk")) %>%
  drop_na(meetplaats, jaar) %>%
  select(meetplaats, monsternamedatum, jaar, bekken, groep, owl,
         mmif, ta_xw, ep_tw, sw_dw, ns_tw, mt_sw,
         t, p_h, o2, o2_verz, ec_20,
         czv, n_t, no2, no3, nh4, p_t, zs,
         TU_sum, TU_max, TU_insecticide, TU_neonicotinoids, spear_pesticides, concentratie_pesticiden_sum, concentratie_insecticide, TU_insecticide_max,TU_core_sum, TU_core_insecticide_max,
         breedte_diepte_ratio, sinuositeit, bodemsub, doodhout, profiel, ekc2_waterlichaam, ekc2_traject, stroomsnelheid, # verstuwing weglaten want te veel NA
         verharding_afstr, natuur_afstr, intensiteit_combo_afstr, intensiteit_gewasbescherming_afstr, verharding_oever, natuur_oever, intensiteit_combo_oeverzone,
         spei6, n_extreme_3m, p_sum_7d,
         lozingen_industrie_ie, lozingen_rwzi_ie, lozingen_rwzi_p_t, lozingen_riool_ie, overstorten_index, overstorten_blootstelling_index, aantal_overstorten_weighted
  ) %>%
  mutate(across(.cols = c(jaar, t:aantal_overstorten_weighted), # Selects n_t and all columns to the end
                .fns = ~as.numeric(scale(.x)),
                .names = "{.col}_s")) %>%
  dplyr::mutate(ep_tw = as.integer(ep_tw),
                ta_xw = as.integer(ta_xw),
                ns_tw = as.integer(ns_tw),
                mt_sw_prop = mt_sw / 10,
                bekken = as.factor(bekken),
                nst_prop = ns_tw / ta_xw,
                stress_prop = (ep_tw + ns_tw)/ta_xw,
                ept_prop = ep_tw / ta_xw,
                n_t_log = log(n_t),
                p_t_log = log(p_t),
                no2_log = log(no2),
                no3_log = log(no3),
                czv_log = log(czv),
                nh4_log = log(nh4),
                ec_20_log = log(ec_20),
                overstorten_index_log = log(overstorten_index + 1),
                overstorten_blootstelling_index_log = log(overstorten_blootstelling_index + 1),
                lozingen_rwzi_ie_log = log(lozingen_rwzi_ie + 1),
                lozingen_rwzi_p_t_log = log(lozingen_rwzi_p_t + 1),
                lozingen_industrie_ie_log = log(lozingen_industrie_ie + 1),
                lozingen_riool_ie_log = log(lozingen_riool_ie + 1),
                TU_sum_log = log(TU_sum + 1))

test2_kempen <- test_kempen %>%
  select(
    meetplaats, monsternamedatum, jaar_s, bekken, owl,
    mmif, ept_prop, ta_xw, sw_dw, mt_sw_prop, nst_prop, stress_prop,
    n_t_log, p_t_log, czv_log,
    ekc2_waterlichaam_s,
    all_of(clean_klimaat),
    all_of(clean_lozingen),
    all_of(clean_landuse), intensiteit_gewasbescherming_afstr_s,
    all_of(clean_fysico),
    TU_sum_s, TU_max_s, TU_insecticide_s, TU_insecticide_max_s, TU_neonicotinoids_s, spear_pesticides_s, concentratie_pesticiden_sum_s, concentratie_insecticide_s, TU_sum_log, spear_pesticides,TU_core_sum_s, TU_core_insecticide_max_s,
    groep
  ) %>%
  na.omit %>%
  filter(TU_insecticide_max_s < 1)

plot_groep_correlogram(test2_beek, c("TU_sum_s", "TU_max_s", "TU_insecticide_s", "TU_insecticide_max_s", "TU_neonicotinoids_s", "spear_pesticides_s", "concentratie_pesticiden_sum_s", "concentratie_insecticide_s", "TU_sum_log"))

plot_groep_correlogram(test2_beek, c("spear_pesticides_s", "mmif", "sw_dw", "ep_tw"))

model_mmif_beek <- glmmTMB(data = test2_beek, formula = mmif ~ TU_insecticide_max_log + ec_20_log + spei6_s + o2_s + t_s + p_t_log + n_t_log + verharding_afstr_s + jaar_s + intensiteit_combo_afstr_s + (1 | meetplaats) + (1 | owl),
                           REML = TRUE,
                           family = ordbeta)
summary(model_mmif_beek)
plot_model(model_mmif_beek, type = "pred", show.data = T)
plot_model_vif(model_mmif_beek)

model_mmif_kempen <- glmmTMB(data = test2_kempen, formula = mmif ~ TU_insecticide_max_s + ec_20_log + p_h_s + spei6_s + p_t_log + intensiteit_combo_afstr_s + n_t_log + o2_s + t_s + verharding_afstr_s +  n_extreme_3m_s + lozingen_riool_ie_log + jaar_s + (1|meetplaats) + (1 | owl),
                             REML = TRUE,
                             family = ordbeta)
summary(model_mmif_kempen)
plot_model(model_mmif, type = "pred", show.data = T)

model_mts <- glmmTMB(data = test2_beek, formula = mt_sw_prop ~ TU_insecticide_max_s + jaar_s + (1|groep) + (1 | meetplaats),
                     REML = TRUE,
                     family = ordbeta)
summary(model_mts)

model_ept <- glmmTMB(data = test2_beek, formula = ept_prop ~ TU_insecticide_max_log + ec_20_log + spei6_s + p_t_log + intensiteit_combo_afstr_s + n_t_log + o2_s + t_s + verharding_afstr_s +  n_extreme_3m_s + lozingen_riool_ie_log + jaar_s + (1 | meetplaats) + (1 | owl),
                     REML = TRUE,
                     family = binomial(link = "logit"),
                     weights = test2_beek$ta_xw)
summary(model_ept)

model_stress <- glmmTMB(data = test2, formula = stress_prop ~ TU_insecticide_max_s + jaar_s + (1 | meetplaats),
                        REML = TRUE,
                        family = binomial(link = "logit"),
                        weights = test2$ta_xw)
summary(model_ept)

model_tax <- glmmTMB(data = test2, formula = ta_xw ~ TU_insecticide_max_s + jaar_s + (1 | meetplaats),
                     REML = TRUE,
                     family = poisson)
summary(model_tax)

model_swd_beek <- glmmTMB(data = test2_beek, formula = sw_dw ~ TU_insecticide_max_log + ec_20_log + p_t_log + o2_s + n_t_log + spear_pesticides_s + zs_log + jaar_s + (1 | meetplaats) + (1 | owl)+ (1| groep),
                          REML = TRUE,
                          family = gaussian)
summary(model_swd_beek)

model_swd_kempen <- glmmTMB(data = test2_kempen, formula = sw_dw ~ TU_insecticide_max_s + ec_20_log + p_t_log + o2_s + zs_s + spear_pesticides_s + jaar_s + (1 | meetplaats) + (1 | owl) + (1| groep),
                            REML = TRUE,
                            family = gaussian)
summary(model_swd_kempen)

model_TU <- glmmTMB(data = test2_beek, formula = TU_insecticide_max_log ~ intensiteit_combo_afstr_s + intensiteit_combo_oeverzone_s + verharding_afstr_s + n_t_log + lozingen_riool_ie_log + spear_pesticides_s + jaar_s + (1 | meetplaats) + (1 | owl) + (1|groep),
                    REML = TRUE,
                    family = gaussian)
summary(model_TU)
plot_model(model_TU, type = "pred", show.data = T)

plot_model_vif(model_TU)

model_spear <- glmmTMB(data = test2_beek, formula = spear_pesticides ~ TU_insecticide_max_log + intensiteit_combo_afstr_s + natuur_oever_s + n_t_log + p_t_log + lozingen_rwzi_ie_log + lozingen_riool_ie_log + jaar_s + (1 | meetplaats) + (1 | owl),
                       REML = TRUE,
                       family = ordbeta)
summary(model_spear)
plot_model_vif(model_spear)


plot_groep_correlogram(test2, c("TU_max_s", "spear_pesticides_s", "n_t_log", "p_t_log", "ec_20_log"), "Responsen (Beken)")

# sem pesticiden beek

pest_swd_beek <- glmmTMB(data = test2_beek, formula = sw_dw ~ TU_insecticide_max_s + ec_20_log + p_t_log + o2_s + spear_pesticides_s + zs_s + jaar_s + n_t_log + (1 | meetplaats) + (1 | owl),
                         REML = TRUE,
                         family = gaussian)
summary(pest_swd_beek)


pest_ept_beek <- glmmTMB(data = test2_beek, formula = ept_prop ~ TU_insecticide_max_s + ec_20_log + p_t_log + o2_s + spear_pesticides_s + zs_s + jaar_s + n_t_log + (1 | meetplaats) + (1 | owl),
                         REML = TRUE,
                         family = binomial(link = "logit"),
                         weights = test2_beek$ta_xw)
summary(pest_ept_beek)

pest_TU <- glmmTMB(data = test2_beek, formula = TU_insecticide_max_s ~ ec_20_log + intensiteit_combo_afstr_s + intensiteit_combo_oeverzone_s + verharding_afstr_s + n_t_log + p_t_log + lozingen_riool_ie_log + jaar_s + (1 | owl),
                   REML = TRUE,
                   family = gaussian)
summary(pest_TU)

pest_ptot <- glmmTMB(data = test2_beek, formula = p_t_log ~ ec_20_log + intensiteit_combo_afstr_s + intensiteit_combo_oeverzone_s + verharding_afstr_s + zs_s + lozingen_riool_ie_log + jaar_s + (1 | owl),
                     REML = TRUE,
                     family = gaussian)
summary(pest_ptot)

pest_ntot <- glmmTMB(data = test2_beek, formula = n_t_log ~ ec_20_log + intensiteit_combo_afstr_s + intensiteit_combo_oeverzone_s + verharding_afstr_s + zs_s + lozingen_riool_ie_log + jaar_s  + (1 | owl),
                     REML = TRUE,
                     family = gaussian)
summary(pest_ntot)

pest_o2 <- glmmTMB(data = test2_beek, formula = o2_s ~ ec_20_log + intensiteit_combo_afstr_s + intensiteit_combo_oeverzone_s + verharding_afstr_s + n_t_log + p_t_log + zs_s + lozingen_riool_ie_log + jaar_s + (1 | owl),
                   REML = TRUE,
                   family = gaussian)
summary(pest_o2)

pest_zs <- glmmTMB(data = test2_beek, formula = zs_s ~ ec_20_log + intensiteit_combo_afstr_s + intensiteit_combo_oeverzone_s + verharding_afstr_s  + lozingen_riool_ie_log + jaar_s + (1 | owl),
                   REML = TRUE,
                   family = gaussian)
summary(pest_zs)

pest_spear <- glmmTMB(data = test2_beek, formula = spear_pesticides_s ~ TU_insecticide_max_s + o2_s + intensiteit_combo_afstr_s + natuur_oever_s + lozingen_rwzi_ie_log + lozingen_riool_ie_log + n_t_log + overstorten_blootstelling_index_log + verharding_afstr_s + jaar_s + (1 | meetplaats) + (1 | owl),
                      REML = TRUE,
                      family = gaussian)
summary(pest_spear)

pest_sem <- psem(pest_swd_beek,
                 pest_ptot,
                 pest_spear,
                 pest_TU,
                 pest_zs,
                 pest_o2,
                 pest_ntot,
                 n_t_log %~~% p_t_log)
summary(pest_sem)

standardize_psem <- function(psem_obj) {
  # 1. Haal de coëfficiënten op (ongestandaardiseerd + gestandaardiseerd waar mogelijk)
  coef_table <- piecewiseSEM::coefs(psem_obj, standardize = "scale")

  # 2. Identificeer welke rijen de Std.Estimate missen
  # We checken op NA of op het "-" teken dat piecewiseSEM vaak gebruikt
  missing_idx <- which(is.na(coef_table$Std.Estimate) | coef_table$Std.Estimate == "-")

  if (length(missing_idx) == 0) {
    message("✅ Alle Std.Estimates zijn al aanwezig.")
    return(coef_table)
  }

  # 3. Haal unieke responses op die reparatie nodig hebben
  responses_to_fix <- unique(coef_table$Response[missing_idx])

  for (resp_name in responses_to_fix) {
    # Zoek het specifieke model in de psem lijst
    # We zoeken op basis van de naam van de afhankelijke variabele
    model_obj <- psem_obj[[which(sapply(psem_obj, function(x)
      tryCatch(insight::find_response(x) == resp_name, error = function(e) FALSE)))]]

    if (is.null(model_obj)) next

    message("🔧 Berekenen gestandaardiseerde waarden voor: ", resp_name)

    # --- De 'insight/performance' magie ---
    # Haal de varianties op (Nakagawa methode)
    vars <- insight::get_variance(model_obj)

    # Bereken totale latente SD: sqrt(Var_fixed + Var_random + Var_distribution)
    # De 'var.distribution' is cruciaal voor non-gaussian modellen (Varm)
    total_latent_sd <- sqrt(sum(unlist(vars[c("var.fixed", "var.random", "var.distribution")]), na.rm = TRUE))

    # Haal de data op om SD van de predictors te berekenen
    model_data <- insight::get_data(model_obj)

    # Update de specifieke rijen in de tabel
    curr_rows <- which(coef_table$Response == resp_name & (is.na(coef_table$Std.Estimate) | coef_table$Std.Estimate == "-"))

    for (idx in curr_rows) {
      pred_name <- coef_table$Predictor[idx]
      beta_unstd <- coef_table$Estimate[idx]

      if (pred_name %in% names(model_data)) {
        sd_x <- sd(model_data[[pred_name]], na.rm = TRUE)
        # De formule: beta * (SD_x / SD_y_totaal)
        coef_table$Std.Estimate[idx] <- as.numeric(beta_unstd * (sd_x / total_latent_sd))
      }
    }
  }

  # Dwing de kolom naar numeriek voor verdere analyse
  coef_table$Std.Estimate <- as.numeric(coef_table$Std.Estimate)

  return(coef_table)
}


coefs_filled <- standardize_psem(pest_sem)[,-9]
source(here("source", "analyse", "sem", "figuur_sem_zonder_corrfout.R")) #zonder cluster gecorreleerde fouten

pest_ept_sem <- psem(pest_ept_beek,
                     pest_ptot,
                     pest_spear,
                     pest_TU,
                     pest_zs,
                     pest_o2,
                     pest_ntot,
                     n_t_log %~~% p_t_log)
summary(pest_ept_sem)


sem_resultaat <- pest_sem
coefs_missing <- coefs(sem_resultaat)[,-9]
source("source/analyse/sem/sem_standardised_coef_flexible.R")
coefs_filled <- coefs_missing
source(here("source", "analyse", "sem", "figuur_sem.R"))

