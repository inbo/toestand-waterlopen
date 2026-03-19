load(file = here("data", "verwerkt" , "koppeling", "koppeling_mi_mafy_2km_2jaar.rdata"))
load(here("data", "verwerkt", "mi_data.rdata")) # macroinvertebraten

mi_data_analyse <- mi_data %>%
  filter(categorie != "Vijver") %>%
  filter(waterlooptype != "Geïsoleerd water") %>%
  filter(waterlichaamcategorie != "meer") %>%
  filter(!meetplaats %in% c("OW113500", "OW12000", "OW179000", "OW536050", "OW669032", "OW690015", "OW917000", "OW981010", "OW981200"))

koppeling_sleutel <-
  mi_met_mafy_2km_2jaar %>%
  select(meetplaats, monsternamedatum, mp_meetplaats, mp_monsternamedatum) %>%
  mutate(
    monsternamedatum = as.Date(monsternamedatum),
    mp_monsternamedatum = as.Date(mp_monsternamedatum)
  )

load(here("data", "verwerkt", "mafy_data.rdata"))

gekoppelde_data_mi_mafy <- mi_data_analyse %>%
  st_drop_geometry() %>%
  filter(jaar > 2009) %>%
  # 1. Koppel de match-informatie aan je MI data
  # (Zodat we weten WELK pesticiden staal bij welk MI staal hoort)
  left_join(koppeling_sleutel, by = c("meetplaats", "monsternamedatum")) %>%

  # LET OP DE 'BY': We koppelen de 'qual_' kolommen van links aan de originele kolommen van rechts
  left_join(mafy_data,
            by = c("mp_meetplaats" = "meetplaats",
                   "mp_monsternamedatum" = "monsternamedatum"),
            suffix = c("", "_mafy"))

gekoppelde_data_mi_mafy %>%
  filter(is.na(index_zonder_gep)) %>%
  group_by(categorie) %>%
  summarise(n())

 test <- gekoppelde_data_mi_mafy %>%
  filter(groep == "beek") %>%
  filter(statuut %in% c("Natuurlijk", "Sterk Veranderd")) %>%
  filter(!is.na(index_zonder_gep))

 ggplot(test, aes(index_zonder_gep, mmif)) +
   geom_point() +
   geom_smooth()

#### ultieme test -> mafy toevoegen aan sem data mi_nat_sv_beek

mi_mafy_data <-  data_subset %>%
   st_drop_geometry() %>%
   filter(jaar > 2009) %>%
   # 1. Koppel de match-informatie aan je MI data
   # (Zodat we weten WELK pesticiden staal bij welk MI staal hoort)
   left_join(koppeling_sleutel, by = c("meetplaats", "monsternamedatum")) %>%

   # LET OP DE 'BY': We koppelen de 'qual_' kolommen van links aan de originele kolommen van rechts
   left_join(mafy_data,
             by = c("mp_meetplaats" = "meetplaats",
                    "mp_monsternamedatum" = "monsternamedatum"),
             suffix = c("", "_mafy")) %>%
  select(meetplaats, jaar_s, mmif, ept_prop, ta_xw, index_zonder_gep, o2_verz_s, n_t_log, p_t_log, czv_log, intensiteit_combo_afstr_s, overstorten_blootstelling_index_s, ekc2_waterlichaam_s) %>%
  na.omit() %>%
  mutate(jaar_s_sq = jaar_s^2,
          o2_verz_s_sq = o2_verz_s^2)


# mmif <- glmmTMB(
#   mmif ~ index_zonder_gep + o2_verz_s + o2_verz_s_sq + n_t_log + p_t_log + czv_log + intensiteit_combo_afstr_s + overstorten_blootstelling_index_s + ekc2_waterlichaam_s + jaar_s + jaar_s_sq + (1 | meetplaats),
#   family = ordbeta,
#   data = mi_mafy_data)
# summary(mmif)

ept <- glmmTMB(
  ept_prop ~ index_zonder_gep + o2_verz_s + n_t_log + p_t_log + czv_log + intensiteit_combo_afstr_s + overstorten_blootstelling_index_s + ekc2_waterlichaam_s + jaar_s + (1 | meetplaats),
  family = binomial(link = "logit"),
  weights = ta_xw,
  data = mi_mafy_data)
summary(ept)

ntot <- glmmTMB(
  n_t_log ~ intensiteit_combo_afstr_s + overstorten_blootstelling_index_s + ekc2_waterlichaam_s + jaar_s + (1 | meetplaats),
  family = gaussian,
  data = mi_mafy_data)
summary(ntot)

ptot <- glmmTMB(
  p_t_log ~ intensiteit_combo_afstr_s + n_t_log + overstorten_blootstelling_index_s + ekc2_waterlichaam_s + jaar_s + (1 | meetplaats),
  family = gaussian,
  data = mi_mafy_data)
summary(ptot)

czv <- glmmTMB(
  czv_log ~ n_t_log + p_t_log + overstorten_blootstelling_index_s + ekc2_waterlichaam_s + jaar_s + (1 | meetplaats),
  family = gaussian,
  data = mi_mafy_data)
summary(czv)

o2 <- glmmTMB(
  o2_verz_s ~ czv_log + overstorten_blootstelling_index_s + ekc2_waterlichaam_s + jaar_s + (1 | meetplaats),
  family = gaussian,
  data = mi_mafy_data)
summary(o2)

mafy <- glmmTMB(
  index_zonder_gep ~ o2_verz_s + n_t_log + p_t_log + czv_log + intensiteit_combo_afstr_s + ekc2_waterlichaam_s + jaar_s + (1 | meetplaats),
  family = ordbeta,
  data = mi_mafy_data)
summary(mafy)


sem_resultaat <- psem(ept, mafy, o2, ptot, ntot, czv, data = mi_mafy_data)
# multigroup(sem_resultaat, group = data_sem_clean$groep_dummy)
summary(sem_resultaat)
# coefs(sem_resultaat)
# plot(sem_resultaat)

# 1️⃣ Extract coëfficiënten uit je SEM
coefs_missing <- coefs(sem_resultaat)[,-9]

# source("source/sem/sem_standardised_coef_manually_enkel_ordbeta.R")
source(here("source", "analyse", "sem", "sem_standardised_coef_flexible.R"))

coefs_filled <- coefs_missing
# 2️⃣ Filter enkel significante paden (p < 0.05)

source(here("source", "analyse", "sem", "figuur_sem.R"))


