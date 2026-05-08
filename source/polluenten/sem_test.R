
################################################################################
# Pesticiden
################################################################################

load(file = "data/verwerkt/koppeling/koppeling_mi_pesticides.rdata")

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
  filter(groep %in% c("beek")) %>%
  filter(statuut %in% c("Sterk Veranderd", "Natuurlijk")) %>%
  drop_na(meetplaats, jaar) %>%
  select(meetplaats, monsternamedatum, jaar, bekken, groep,
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
                overstorten_index_log = log(overstorten_index + 1),
                overstorten_blootstelling_index_log = log(overstorten_blootstelling_index + 1),
                lozingen_rwzi_ie_log = log(lozingen_rwzi_ie + 1),
                lozingen_rwzi_p_t_log = log(lozingen_rwzi_p_t + 1),
                lozingen_industrie_ie_log = log(lozingen_industrie_ie + 1),
                lozingen_riool_ie_log = log(lozingen_riool_ie + 1),
                TU_sum_log = log(TU_sum + 1))

test2_beek <- test_beek %>%
  select(
    meetplaats, monsternamedatum, jaar_s, bekken,
    mmif, ept_prop, ta_xw, sw_dw, mt_sw_prop, nst_prop, stress_prop,
    n_t_log, p_t_log, czv_log,
    ekc2_waterlichaam_s,
    all_of(clean_klimaat),
    all_of(clean_lozingen),
    all_of(clean_landuse), intensiteit_gewasbescherming_afstr_s,
    all_of(clean_fysico),
    TU_sum_s, TU_max_s, TU_insecticide_s, TU_insecticide_max_s, TU_neonicotinoids_s, spear_pesticides_s, concentratie_pesticiden_sum_s, concentratie_insecticide_s, TU_sum_log, spear_pesticides, TU_core_sum_s, TU_core_insecticide_max_s,
    groep
  ) %>%
  na.omit %>%
  filter(TU_insecticide_max_s < 1)

test_kempen <- gekoppelde_data_mi_pesticides %>%
  filter(groep %in% c("kempen")) %>%
  filter(statuut %in% c("Sterk Veranderd", "Natuurlijk")) %>%
  drop_na(meetplaats, jaar) %>%
  select(meetplaats, monsternamedatum, jaar, bekken, groep,
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
                overstorten_index_log = log(overstorten_index + 1),
                overstorten_blootstelling_index_log = log(overstorten_blootstelling_index + 1),
                lozingen_rwzi_ie_log = log(lozingen_rwzi_ie + 1),
                lozingen_rwzi_p_t_log = log(lozingen_rwzi_p_t + 1),
                lozingen_industrie_ie_log = log(lozingen_industrie_ie + 1),
                lozingen_riool_ie_log = log(lozingen_riool_ie + 1),
                TU_sum_log = log(TU_sum + 1))

test2_kempen <- test_kempen %>%
  select(
    meetplaats, monsternamedatum, jaar_s, bekken,
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

plot_groep_correlogram(test2, c("TU_sum_s", "TU_max_s", "TU_insecticide_s", "TU_insecticide_max_s", "TU_neonicotinoids_s", "spear_pesticides_s", "concentratie_pesticiden_sum_s", "concentratie_insecticide_s", "TU_sum_log"))

plot_groep_correlogram(test2, c("spear_pesticides_s", "mmif", "sw_dw", "ep_tw"))

model_mmif_beek <- glmmTMB(data = test2_beek, formula = mmif ~ TU_insecticide_max_s + ec_20_s + spei6_s + o2_s + t_s + p_t_log + n_t_log + verharding_afstr_s + jaar_s + intensiteit_combo_afstr_s + (1 | meetplaats) + (1 | bekken),
                           REML = TRUE,
                           family = ordbeta)
summary(model_mmif_beek)
plot_model(model_mmif_beek, type = "pred", show.data = T)
plot_model_vif(model_mmif_beek)

model_mmif_kempen <- glmmTMB(data = test2_kempen, formula = mmif ~ TU_insecticide_max_s + ec_20_s + p_h_s + spei6_s + p_t_log + intensiteit_combo_afstr_s + n_t_log + o2_s + t_s + verharding_afstr_s +  n_extreme_3m_s + lozingen_riool_ie_log + jaar_s + (1|meetplaats) + (1 | bekken),
                             REML = TRUE,
                             family = ordbeta)
summary(model_mmif_kempen)
plot_model(model_mmif, type = "pred", show.data = T)

model_mts <- glmmTMB(data = test2, formula = mt_sw_prop ~ TU_insecticide_max_s + jaar_s + (1|groep) + (1 | meetplaats),
                     REML = TRUE,
                     family = ordbeta)
summary(model_mts)

model_ept <- glmmTMB(data = test2_beek, formula = ept_prop ~ TU_insecticide_max_s + ec_20_s + p_h_s + spei6_s + p_t_log + intensiteit_combo_afstr_s + n_t_log + o2_s + t_s + verharding_afstr_s +  n_extreme_3m_s + lozingen_riool_ie_log + jaar_s + (1 | meetplaats),
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

model_swd_beek <- glmmTMB(data = test2_beek, formula = sw_dw ~ TU_insecticide_max_s + ec_20_s + p_t_log + o2_s + + spear_pesticides_s + zs_s + jaar_s + (1 | meetplaats),
                          REML = TRUE,
                          family = gaussian)
summary(model_swd_beek)

model_swd_kempen <- glmmTMB(data = test2_kempen, formula = sw_dw ~ TU_insecticide_max_s + ec_20_s + p_t_log + o2_s + zs_s + spear_pesticides_s + jaar_s + (1 | meetplaats),
                            REML = TRUE,
                            family = gaussian)
summary(model_swd_kempen)

model_TU <- glmmTMB(data = test2_beek, formula = TU_insecticide_max_s ~ intensiteit_combo_afstr_s + intensiteit_combo_oeverzone_s + verharding_afstr_s + n_t_log + lozingen_riool_ie_log + spear_pesticides_s + jaar_s + (1 | meetplaats),
                    REML = TRUE,
                    family = gaussian)
summary(model_TU)
plot_model(model_TU, type = "pred", show.data = T)

plot_model_vif(model_TU)

model_spear <- glmmTMB(data = test2_beek, formula = spear_pesticides ~ TU_insecticide_max_s + intensiteit_combo_afstr_s + natuur_oever_s + n_t_log + p_t_log + lozingen_rwzi_ie_log + lozingen_riool_ie_log + jaar_s + (1 | meetplaats),
                       REML = TRUE,
                       family = ordbeta)
summary(model_spear)
plot_model_vif(model_spear)


plot_groep_correlogram(test2, c("TU_max_s", "spear_pesticides_s", "n_t_log", "p_t_log", "ec_20_s"), "Responsen (Beken)")

# sem pesticiden beek

pest_swd_beek <- glmmTMB(data = test2_beek, formula = sw_dw ~ TU_insecticide_max_s + ec_20_s + p_t_log + o2_s + spear_pesticides_s + zs_s + jaar_s + n_t_log + (1 | meetplaats),
                         REML = TRUE,
                         family = gaussian)
summary(pest_swd_beek)


pest_ept_beek <- glmmTMB(data = test2_beek, formula = ept_prop ~ TU_insecticide_max_s + ec_20_s + p_t_log + o2_s + spear_pesticides_s + zs_s + jaar_s + n_t_log + (1 | meetplaats),
                         REML = TRUE,
                         family = binomial(link = "logit"),
                         weights = test2_beek$ta_xw)
summary(pest_ept_beek)

pest_TU <- glmmTMB(data = test2_beek, formula = TU_insecticide_max_s ~ ec_20_s + intensiteit_combo_afstr_s + intensiteit_combo_oeverzone_s + verharding_afstr_s + n_t_log + p_t_log + lozingen_riool_ie_log + jaar_s + (1 | meetplaats),
                   REML = TRUE,
                   family = gaussian)
summary(pest_TU)

pest_ptot <- glmmTMB(data = test2_beek, formula = p_t_log ~ ec_20_s + intensiteit_combo_afstr_s + intensiteit_combo_oeverzone_s + verharding_afstr_s + zs_s + lozingen_riool_ie_log + jaar_s + (1 | meetplaats),
                     REML = TRUE,
                     family = gaussian)
summary(pest_ptot)

pest_ntot <- glmmTMB(data = test2_beek, formula = n_t_log ~ ec_20_s + intensiteit_combo_afstr_s + intensiteit_combo_oeverzone_s + verharding_afstr_s + zs_s + lozingen_riool_ie_log + jaar_s + (1 | meetplaats),
                     REML = TRUE,
                     family = gaussian)
summary(pest_ntot)

pest_o2 <- glmmTMB(data = test2_beek, formula = o2_s ~ ec_20_s + intensiteit_combo_afstr_s + intensiteit_combo_oeverzone_s + verharding_afstr_s + n_t_log + p_t_log + zs_s + lozingen_riool_ie_log + jaar_s + (1 | meetplaats),
                   REML = TRUE,
                   family = gaussian)
summary(pest_o2)

pest_zs <- glmmTMB(data = test2_beek, formula = zs_s ~ ec_20_s + intensiteit_combo_afstr_s + intensiteit_combo_oeverzone_s + verharding_afstr_s  + lozingen_riool_ie_log + jaar_s + (1 | meetplaats),
                   REML = TRUE,
                   family = gaussian)
summary(pest_zs)

pest_spear <- glmmTMB(data = test2_beek, formula = spear_pesticides_s ~ TU_insecticide_max_s + o2_s + intensiteit_combo_afstr_s + natuur_oever_s + lozingen_rwzi_ie_log + lozingen_riool_ie_log + n_t_log + overstorten_blootstelling_index_log + verharding_afstr_s + jaar_s + (1 | meetplaats),
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

