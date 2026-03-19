
match_key_mi_pesticiden <- pesticide_result %>%
  st_drop_geometry() %>%
  select(
    # De unieke ID van het MI staal (pas aan indien je een unieke ID kolom hebt, bv 'analyse_id')
    meetplaats, monsternamedatum,

    # De match resultaten (De sleutels naar de pesticiden data)
    qual_meetplaats, qual_monsternamedatum,

    # Metadata over de match (optioneel, maar handig voor filtering later)
    qual_river_dist_m, qual_n_matches
  ) %>%
  mutate(
    monsternamedatum = as.Date(monsternamedatum),
    qual_monsternamedatum = as.Date(qual_monsternamedatum)
  )

gekoppelde_data_mi_tu <- mi_nat_sv %>%
  # 1. Koppel de match-informatie aan je MI data
  # (Zodat we weten WELK pesticiden staal bij welk MI staal hoort)
  left_join(match_key_mi_pesticiden, by = c("meetplaats", "monsternamedatum")) %>%

  # 2. Koppel nu de daadwerkelijke pesticiden waarden
  # LET OP DE 'BY': We koppelen de 'qual_' kolommen van links aan de originele kolommen van rechts
  left_join(tu_pesticiden_per_sample,
            by = c("qual_meetplaats" = "meetplaats",
                   "qual_monsternamedatum" = "monsternamedatum"))

# Controleer het resultaat
head(gekoppelde_data_mi_tu)

gekoppelde_data_mi_tu %>%
  filter(!is.na(TU_sum)) %>%
  group_by(groep) %>%
  summarise(n())

###test koppelen aan sem_data_clean

test_data0 <- data_sem_clean %>%
  # 1. Koppel de match-informatie aan je MI data
  # (Zodat we weten WELK pesticiden staal bij welk MI staal hoort)
  left_join(match_key_mi_pesticiden, by = c("meetplaats", "monsternamedatum")) %>%

  # 2. Koppel nu de daadwerkelijke pesticiden waarden
  # LET OP DE 'BY': We koppelen de 'qual_' kolommen van links aan de originele kolommen van rechts
  left_join(tu_pesticiden_per_sample,
            by = c("qual_meetplaats" = "meetplaats",
                   "qual_monsternamedatum" = "monsternamedatum"))

test_data <- test_data0 %>%
  filter(!is.na(TU_sum)) %>%
  mutate(tu_sum_s = as.numeric(scale(TU_sum)),
         tu_sum_log = log(TU_sum))

### sem test


# M1: N_T (Gaussian)
m1 <- glmmTMB(data = test_data,
              n_t_log ~ intensiteit_combo_s + ekc2_waterlichaam_s + jaar_s + spei6_s + n_extreme_3m_s + score_overstorten_500m_s + verharding_afstr_s + (1 | meetplaats),
              family = gaussian)


m3 <- glmmTMB(data = test_data,
              p_t_log ~ intensiteit_combo_s + ekc2_waterlichaam_s  + n_t_log + jaar_s + score_overstorten_500m_s +
                spei6_s + n_extreme_3m_s + verharding_afstr_s  + (1 | meetplaats),
              family = gaussian)

m4 <- glmmTMB(data = test_data,
              o2_verz_fc_s ~  intensiteit_combo_s + p_t_log + n_t_log  + score_overstorten_500m_s +  spei6_s + n_extreme_3m_s + verharding_afstr_s + czv_s + tu_sum_log+
                ekc2_waterlichaam_s + jaar_s + (1 | meetplaats),
              family = gaussian)

m5 <- glmmTMB(data = test_data,
              czv_s ~  intensiteit_combo_s + score_overstorten_500m_s +  spei6_s + n_extreme_3m_s + verharding_afstr_s + p_t_log + n_t_log + ekc2_waterlichaam_s + jaar_s + (1 | meetplaats),
              family = gaussian)

m6 <- glmmTMB(data = test_data,
              tu_sum_log ~  intensiteit_combo_s + score_overstorten_500m_s +  spei6_s + n_extreme_3m_s + verharding_afstr_s + p_t_log + ekc2_waterlichaam_s + jaar_s + n_t_log+ (1 | meetplaats),
              family = gaussian)


m2 <- glmmTMB(
  mmif ~ n_t_log + intensiteit_combo_s + spei6_s + n_extreme_3m_s + verharding_afstr_s  + ekc2_waterlichaam_s + o2_verz_fc_s + jaar_s + p_t_log + score_overstorten_500m_s + czv_s + tu_sum_log + (1 | meetplaats),
  family = ordbeta,
  data = test_data)
#
m2 <- glmmTMB(
  ept_prop ~ n_t_log + intensiteit_combo_s + spei6_s + n_extreme_3m_s + verharding_afstr_s  + ekc2_waterlichaam_s + o2_verz_fc_s + jaar_s + p_t_log + score_overstorten_500m_s + czv_s + tu_sum_log + (1 | meetplaats),
  weights = test_data$ta_xw,
  data = test_data,
  family =  binomial)

m2 <- glmmTMB(
  ta_xw ~ n_t_log + intensiteit_combo_s + spei6_s + n_extreme_3m_s + verharding_afstr_s  + ekc2_waterlichaam_s + o2_verz_fc_s + jaar_s + p_t_log + score_overstorten_500m_s + czv_s+ tu_sum_log + (1 | meetplaats),
  family = poisson,
  data = test_data)

m2 <- glmmTMB(
  mt_sw_prop ~ n_t_log + intensiteit_combo_s + spei6_s + n_extreme_3m_s + verharding_afstr_s  + ekc2_waterlichaam_s + o2_verz_fc_s + jaar_s + p_t_log + score_overstorten_500m_s + czv_s+ tu_sum_log + (1 | meetplaats),
  family = ordbeta,
  data = test_data)

m2 <- glmmTMB(
  sw_dw ~ n_t_log + intensiteit_combo_s + spei6_s + n_extreme_3m_s + verharding_afstr_s  + ekc2_waterlichaam_s + o2_verz_fc_s + jaar_s + p_t_log + score_overstorten_500m_s + czv_s + tu_sum_log+ (1 | meetplaats),
  family = gaussian,
  data = test_data)

m2 <- glmmTMB(
  nst_prop ~ n_t_log + intensiteit_combo_s + spei6_s + n_extreme_3m_s + verharding_afstr_s  + ekc2_waterlichaam_s + o2_verz_fc_s + jaar_s + p_t_log + score_overstorten_500m_s + czv_s + (1 | meetplaats),
  weights = test_data$ta_xw,
  data = test_data,
  family =  binomial(link = "logit"))
#

m2 <- glmmTMB(
  stress_prop ~ n_t_log + intensiteit_combo_s + spei6_s + n_extreme_3m_s + verharding_afstr_s  + ekc2_waterlichaam_s + o2_verz_fc_s + jaar_s + p_t_log + score_overstorten_500m_s + czv_s + tu_sum_log + (1 | meetplaats),
  weights = test_data$ta_xw,
  data = test_data,
  family =  binomial(link = "logit"))
#
# simulationOutput <- simulateResiduals(m1, plot = TRUE)
# testDispersion(simulationOutput) # geen overdispersie
# testZeroInflation(simulationOutput) # geen zero_inflation
# testUniformity(simulationOutput)

sem_resultaat <- psem(m1, m2, m3, m4, m5, m6)
# multigroup(sem_resultaat, group = test_data$groep_dummy)
summary(sem_resultaat)
# coefs(sem_resultaat)
# plot(sem_resultaat)

# 1️⃣ Extract coëfficiënten uit je SEM
coefs_df <- coefs(sem_resultaat)[,-9]

source("source/sem/sem_standardised_coef_manually_enkel_ordbeta.R")

coef_df <- coefs_df
# 2️⃣ Filter enkel significante paden (p < 0.05)

source(here("source", "sem", "figuur_sem.R"))


###spear test

load(file = here("data", "verwerkt", "spear_data.rdata"))
test_data <- test_data %>%
  left_join(spear_data %>%
              select(meetplaats, monsternamedatum, tu_estimated, spear_pesticides))

ggplot(test_data %>%
         filter(TU_sum < 2), aes(TU_sum, spear_pesticides)) +
  geom_point() +
  geom_smooth(method = "lm")

spear_tu <- glmmTMB(data = test_data %>%
          filter(TU_sum < 2),
        spear_pesticides ~ TU_sum + (1|meetplaats))
summary(spear_tu)
r2(spear_tu)
