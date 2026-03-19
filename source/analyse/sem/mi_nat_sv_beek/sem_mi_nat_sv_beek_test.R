
data_sem_clean0 <- data3 %>%
  filter(groep == "beek") %>%
  filter(statuut %in% c("Natuurlijk", "Sterk Veranderd")) %>%
  select(groep, monsternamedatum, bekken, statuut, meetplaats, owl, ep_tw, ta_xw, ns_tw, sw_dw, mt_sw, mmif, n_t, ph, t_fc, ec_20_fc, o2_verz_fc, o2_fc, o2, o2_verz, p_t, czv, natuur_oever, landbouw_intens_buffer, landbouw_intens_afstr, akker_afstr, hooggroen_afstr, hooggroen_oever, jaar, kjn, landbouw_intens_oever, ekc2_waterlichaam, ekc2_traject, sinuositeit, profiel, overstorten_index, aantal_overstorten_weighted, overstorten_blootstelling_index, lozingen_rwzi_ie, lozingen_rwzi_p_t, lozingen_riool_ie, lozingen_riool_ie_conservative, lozingen_industrie_ie, verharding_afstr, verharding_oever, spei6, n_extreme_3m, p_sum_7d, intensiteit_combo_afstr) %>%
  tidyr::drop_na() %>%
  mutate(across(.cols = n_t:intensiteit_combo_afstr, # Selects n_t and all columns to the end
                .fns = ~as.numeric(scale(.x)),
                .names = "{.col}_s"))
data_sem_clean <- data_sem_clean0 %>%
  dplyr::mutate(ep_tw = as.integer(ep_tw),
                ta_xw = as.integer(ta_xw),
                ns_tw = as.integer(ns_tw),
                mt_sw_prop = mt_sw / 10,
                owl = as.factor(owl),
                bekken = as.factor(bekken),
                nst_prop = ns_tw / ta_xw,
                stress_prop = (ep_tw + ns_tw)/ta_xw,
                ept_prop = ep_tw / ta_xw,
                kjn_log = log(kjn),
                n_t_log = log(n_t),
                p_t_log = log(p_t),
                czv_log = log(czv),
                overstorten_index_log = log(overstorten_index + 1),
                overstorten_blootstelling_index_log = log(overstorten_blootstelling_index + 1),
                lozingen_rwzi_ie_log = log(lozingen_rwzi_ie + 1),
                lozingen_rwzi_p_t_log = log(lozingen_rwzi_p_t + 1),
                lozingen_industrie_ie_log = log(lozingen_industrie_ie + 1),
                lozingen_riool_ie_log = log(lozingen_riool_ie + 1),
                lozingen_riool_ie_conservative_log = log(lozingen_riool_ie_conservative + 1),
                groep_dummy = ifelse(groep == "beek", 0, 1)
  )



# M1: N_T (Gaussian)
m1 <- glmmTMB(data = data_sem_clean,
              n_t_log ~ intensiteit_combo_afstr_s + ekc2_waterlichaam_s + jaar_s + spei6_s + n_extreme_3m_s + overstorten_blootstelling_index_log + verharding_afstr_s + lozingen_industrie_ie_log + lozingen_rwzi_ie_log + lozingen_riool_ie_log + (1 | meetplaats),
              family = gaussian)

simulationOutput <- simulateResiduals(m1, plot = TRUE)
testDispersion(simulationOutput) # geen overdispersie
testZeroInflation(simulationOutput) # geen zero_inflation
testUniformity(simulationOutput)

m3 <- glmmTMB(data = data_sem_clean,
              p_t_log ~ intensiteit_combo_afstr_s + ekc2_waterlichaam_s + n_t_log + jaar_s + overstorten_blootstelling_index_log +
                spei6_s + n_extreme_3m_s + verharding_afstr_s + lozingen_industrie_ie_log + lozingen_rwzi_ie_log +
                lozingen_riool_ie_log + (1 | meetplaats),
              family = gaussian)

simulationOutput <- simulateResiduals(m3, plot = TRUE)
testDispersion(simulationOutput) # geen overdispersie
testZeroInflation(simulationOutput) # geen zero_inflation
testUniformity(simulationOutput)

m4 <- glmmTMB(data = data_sem_clean,
              o2_verz_s ~  intensiteit_combo_afstr_s + p_t_log + n_t_log  + overstorten_blootstelling_index_log +  spei6_s + n_extreme_3m_s + verharding_afstr_s + czv_log +
                ekc2_waterlichaam_s + jaar_s + lozingen_industrie_ie_log + lozingen_rwzi_ie_log + lozingen_riool_ie_log + (1 | meetplaats),
              family = gaussian)

m5 <- glmmTMB(data = data_sem_clean,
              czv_log ~  intensiteit_combo_afstr_s + overstorten_blootstelling_index_log +  spei6_s + n_extreme_3m_s + verharding_afstr_s + p_t_log + n_t_log + ekc2_waterlichaam_s + lozingen_industrie_ie_log + lozingen_rwzi_ie_log + jaar_s + lozingen_riool_ie_log + (1 | meetplaats),
              family = gaussian)

m2 <- glmmTMB(
  mmif ~ n_t_log + intensiteit_combo_afstr_s + spei6_s + n_extreme_3m_s + verharding_afstr_s  + ekc2_waterlichaam_s + o2_verz_s + jaar_s + p_t_log + overstorten_blootstelling_index_log + czv_log + lozingen_industrie_ie_log + lozingen_rwzi_ie_log + lozingen_riool_ie_log + (1 | meetplaats),
  family = ordbeta,
  data = data_sem_clean)

m2 <- glmmTMB(
  mmif ~ I(jaar_s)^2 + (1 | meetplaats),
  family = ordbeta,
  data = data_sem_clean)

simulationOutput <- simulateResiduals(m2, plot = TRUE)
testDispersion(simulationOutput) # geen overdispersie
testZeroInflation(simulationOutput) # geen zero_inflation
testUniformity(simulationOutput)

# #
# m2 <- glmmTMB(
#   ept_prop ~ n_t_log + intensiteit_combo_afstr_s + spei6_s + n_extreme_3m_s + verharding_afstr_s  + ekc2_waterlichaam_s + o2_verz_s + jaar_s + p_t_log + overstorten_blootstelling_index_log + czv_s + lozingen_industrie_ie_log + lozingen_rwzi_p_t_log + (1 | meetplaats),
#   weights = data_sem_clean$ta_xw,
#   data = data_sem_clean,
#   family =  binomial)
# #
# m2 <- glmmTMB(
#   ta_xw ~ n_t_log + intensiteit_combo_afstr_s + spei6_s + n_extreme_3m_s + verharding_afstr_s  + ekc2_waterlichaam_s + o2_verz_s + jaar_s + p_t_log + overstorten_blootstelling_index_log + czv_s + (1 | meetplaats),
#   family = poisson,
#   data = data_sem_clean)
#
# m2 <- glmmTMB(
#   mt_sw_prop ~ n_t_log + intensiteit_combo_afstr_s + spei6_s + n_extreme_3m_s + verharding_afstr_s  + ekc2_waterlichaam_s + o2_verz_s + jaar_s + p_t_log + overstorten_blootstelling_index_log + czv_s + (1 | meetplaats),
#   family = ordbeta,
#   data = data_sem_clean)
#
m2 <- glmmTMB(
  sw_dw ~ n_t_log + intensiteit_combo_afstr_s + spei6_s + n_extreme_3m_s + verharding_afstr_s  + ekc2_waterlichaam_s + o2_verz_s + jaar_s + p_t_log + overstorten_blootstelling_index_log + lozingen_industrie_ie_log + lozingen_rwzi_ie_log + czv_log + lozingen_riool_ie_log + (1 | meetplaats),
  family = gaussian,
  data = data_sem_clean)
#
# m2 <- glmmTMB(
#   nst_prop ~ n_t_log + intensiteit_combo_afstr_s + spei6_s + n_extreme_3m_s + verharding_afstr_s  + ekc2_waterlichaam_s + o2_verz_s + jaar_s + p_t_log + overstorten_blootstelling_index_log + czv_s + (1 | meetplaats),
#   weights = data_sem_clean$ta_xw,
#   data = data_sem_clean,
#   family =  binomial(link = "logit"))
# # #
#
m2 <- glmmTMB(
  stress_prop ~ n_t_log + intensiteit_combo_afstr_s + spei6_s + n_extreme_3m_s + verharding_afstr_s  + ekc2_waterlichaam_s + o2_verz_s + jaar_s + p_t_log + overstorten_blootstelling_index_log + czv_log + lozingen_industrie_ie_log + lozingen_rwzi_ie_log + lozingen_riool_ie_log  + (1 | meetplaats),
  weights = data_sem_clean$ta_xw,
  data = data_sem_clean,
  family =  binomial(link = "logit"))

# simulationOutput <- simulateResiduals(m1, plot = TRUE)
# testDispersion(simulationOutput) # geen overdispersie
# testZeroInflation(simulationOutput) # geen zero_inflation
# testUniformity(simulationOutput)

sem_resultaat <- psem(m1, m2, m3, m4, m5)
# multigroup(sem_resultaat, group = data_sem_clean$groep_dummy)
summary(sem_resultaat)
# coefs(sem_resultaat)
# plot(sem_resultaat)

# 1️⃣ Extract coëfficiënten uit je SEM
coefs_missing <- coefs(sem_resultaat)[,-9]

# source("source/sem/sem_standardised_coef_manually_enkel_ordbeta.R")
source("source/analyse/sem/sem_standardised_coef_flexible.R")

coefs_filled <- coefs_missing
# 2️⃣ Filter enkel significante paden (p < 0.05)

source(here("source", "analyse", "sem", "figuur_sem.R"))

