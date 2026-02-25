###EC20 aanpassen in het model
load("data/verwerkt/mi_nat_sv.rdata")

source("source/inladen_packages.R")
# Selecteer alleen de noodzakelijke variabelen en verwijder NA's
data_sem_clean0 <- mi_nat_sv %>%
  dplyr::select(groep, monsternamedatum, bekken, statuut, meetplaats, owl.x, ep_tw, ta_xw, ns_tw, sw_dw, mt_sw, mmif, mmif_20, n_t, ph, t_fc, ec_20_fc, o2_verz_fc, o2_fc, p_t, czv, natuur_oever, landbouw_intens_afstr, akker_afstr, hooggroen_afstr, hooggroen_oever, jaar, kjn, aantal_pesticiden_met_overschrijding, aantal_zware_metalen_met_overschrijding, Neerslag_som_10dagen, Neerslag_som_1jaar, ekc2_waterlichaam, ekc2_traject, sinuositeit, aantal_overstorten_500m, score_overstorten_500m, overstorten_index, overstorten_blootstelling_index, lozingen_rwzi_ie, lozingen_rwzi_p_t, lozingen_riool_ie, lozingen_industrie_ie, verharding_afstr, spear_pesticides, verharding_oever, spei6, n_extreme_3m, p_sum_7d, intensiteit_combo) %>%
  tidyr::drop_na() %>%
  filter(groep %in% c("polder")) %>%
  mutate(across(.cols = n_t:intensiteit_combo, # Selects n_t and all columns to the end
                .fns = ~as.numeric(scale(.x)),
                .names = "{.col}_s"))

# # Correlatie en VIF
#
# numerieke_var <- data_sem_clean %>%
#   dplyr::select(ep_tw, ta_xw, sw_dw, mt_sw, mmif, n_t, p_h, ec_20, o2, p_t, landbouw_intens_afstr, hooggroen_afstr, hooggroen_oever, jaar_scaled, kjn,
#                 # aantal_pesticiden_met_overschrijding,
#                 aantal_overstorten_500m,
#                 Neerslag_som_10dagen, Neerslag_som_1jaar,
#                 ekc2_waterlichaam) #
# cor_matrix <- cor(numerieke_var)
# corrplot(cor_matrix, method = "circle", type = "upper", diag = FALSE, addCoef.col = "black")

# Zorg ervoor dat de respons term (20 - mmif_20) ook correct is
data_sem_clean <- data_sem_clean0 %>%
  dplyr::mutate(ep_tw = as.integer(ep_tw),
                ta_xw = as.integer(ta_xw),
                ns_tw = as.integer(ns_tw),
                mt_sw_prop = mt_sw / 10,
                owl = as.factor(owl.x),
                bekken = as.factor(bekken),
                nst_prop = ns_tw / ta_xw,
                stress_prop = (ep_tw + ns_tw)/ta_xw,
                ept_prop = ep_tw / ta_xw,
                kjn_log = log(kjn),
                n_t_log = log(n_t),
                p_t_log = log(p_t),
                overstorten_index_log = log(overstorten_index + 1),
                overstorten_blootstelling_index_log = log(overstorten_blootstelling_index + 1),
                lozingen_rwzi_ie_log = log(lozingen_rwzi_ie + 1),
                lozingen_rwzi_p_t_log = log(lozingen_rwzi_p_t + 1),
                lozingen_industrie_ie_log = log(lozingen_industrie_ie + 1),
                lozingen_riool_ie_log = log(lozingen_riool_ie + 1),
                groep_dummy = ifelse(groep == "beek", 0, 1)
  )

# # VIF
# vif_model <- glm(ep_tw ~ n_t + p_h + ec_20 + o2 + p_t + landbouw_intens_afstr + hooggroen_afstr + hooggroen_oever + jaar_scaled + kjn + aantal_overstorten_500m  + Neerslag_som_10dagen + Neerslag_som_1jaar + ekc2_waterlichaam,
#                  family = poisson(link = "log"),
#                  na.action = na.omit,
#                  data = data_sem_clean)
# vif(vif_model)
# vif(update(vif_model, . ~ . - hooggroen_afstr - kjn))

# M1: N_T (Gaussian)
m1 <- glmmTMB(data = data_sem_clean,
              n_t_log ~ intensiteit_combo_s + ekc2_waterlichaam_s + jaar_s + spei6_s + n_extreme_3m_s + overstorten_blootstelling_index_log + verharding_afstr_s + lozingen_industrie_ie_log + lozingen_rwzi_p_t_log + lozingen_riool_ie_log + (1 | meetplaats),
              family = gaussian)


m3 <- glmmTMB(data = data_sem_clean,
              p_t_log ~ intensiteit_combo_s + ekc2_waterlichaam_s  + n_t_log + jaar_s + overstorten_blootstelling_index_log +
                spei6_s + n_extreme_3m_s + verharding_afstr_s + lozingen_industrie_ie_log + lozingen_rwzi_p_t_log +
                lozingen_riool_ie_log+ (1 | meetplaats),
              family = gaussian)

m4 <- glmmTMB(data = data_sem_clean,
              o2_verz_fc_s ~  intensiteit_combo_s + p_t_log + n_t_log  + overstorten_blootstelling_index_log +  spei6_s + n_extreme_3m_s + verharding_afstr_s + czv_s +
                ekc2_waterlichaam_s + jaar_s + lozingen_industrie_ie_log + lozingen_rwzi_p_t_log + lozingen_riool_ie_log + (1 | meetplaats),
              family = gaussian)

m5 <- glmmTMB(data = data_sem_clean,
              czv_s ~  intensiteit_combo_s + overstorten_blootstelling_index_log +  spei6_s + n_extreme_3m_s + verharding_afstr_s + p_t_log + n_t_log + ekc2_waterlichaam_s + lozingen_industrie_ie_log + lozingen_rwzi_p_t_log + jaar_s + lozingen_riool_ie_log + (1 | meetplaats),
              family = gaussian)

m2 <- glmmTMB(
  mmif ~ n_t_log + intensiteit_combo_s + spei6_s + n_extreme_3m_s + verharding_afstr_s  + ekc2_waterlichaam_s + o2_verz_fc_s + jaar_s + p_t_log + overstorten_blootstelling_index_log + czv_s + lozingen_industrie_ie_log + lozingen_rwzi_p_t_log + lozingen_riool_ie_log + (1 | meetplaats),
  family = ordbeta,
  data = data_sem_clean)
# #
# m2 <- glmmTMB(
#   ept_prop ~ n_t_log + intensiteit_combo_s + spei6_s + n_extreme_3m_s + verharding_afstr_s  + ekc2_waterlichaam_s + o2_verz_fc_s + jaar_s + p_t_log + overstorten_blootstelling_index_log + czv_s + lozingen_industrie_ie_log + lozingen_rwzi_p_t_log + (1 | meetplaats),
#   weights = data_sem_clean$ta_xw,
#   data = data_sem_clean,
#   family =  binomial)
# #
# m2 <- glmmTMB(
#   ta_xw ~ n_t_log + intensiteit_combo_s + spei6_s + n_extreme_3m_s + verharding_afstr_s  + ekc2_waterlichaam_s + o2_verz_fc_s + jaar_s + p_t_log + overstorten_blootstelling_index_log + czv_s + (1 | meetplaats),
#   family = poisson,
#   data = data_sem_clean)
#
# m2 <- glmmTMB(
#   mt_sw_prop ~ n_t_log + intensiteit_combo_s + spei6_s + n_extreme_3m_s + verharding_afstr_s  + ekc2_waterlichaam_s + o2_verz_fc_s + jaar_s + p_t_log + overstorten_blootstelling_index_log + czv_s + (1 | meetplaats),
#   family = ordbeta,
#   data = data_sem_clean)
#
# m2 <- glmmTMB(
#   sw_dw ~ n_t_log + intensiteit_combo_s + spei6_s + n_extreme_3m_s + verharding_afstr_s  + ekc2_waterlichaam_s + o2_verz_fc_s + jaar_s + p_t_log + overstorten_blootstelling_index_log + czv_s + (1 | meetplaats),
#   family = gaussian,
#   data = data_sem_clean)
#
# m2 <- glmmTMB(
#   nst_prop ~ n_t_log + intensiteit_combo_s + spei6_s + n_extreme_3m_s + verharding_afstr_s  + ekc2_waterlichaam_s + o2_verz_fc_s + jaar_s + p_t_log + overstorten_blootstelling_index_log + czv_s + (1 | meetplaats),
#   weights = data_sem_clean$ta_xw,
#   data = data_sem_clean,
#   family =  binomial(link = "logit"))
# #

m2 <- glmmTMB(
  stress_prop ~ n_t_log + intensiteit_combo_s + spei6_s + n_extreme_3m_s + verharding_afstr_s  + ekc2_waterlichaam_s + o2_verz_fc_s + jaar_s + p_t_log + overstorten_blootstelling_index_log + lozingen_industrie_ie_log + lozingen_rwzi_p_t_log + czv_s + lozingen_riool_ie_log + (1 | meetplaats),
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
coefs_df <- coefs(sem_resultaat)[,-9]

# source("source/sem/sem_standardised_coef_manually_enkel_ordbeta.R")
source("source/sem/sem_standardised_coef_flexible.R")

coef_df <- coefs_df
# 2️⃣ Filter enkel significante paden (p < 0.05)

source(here("source", "sem", "figuur_sem.R"))


