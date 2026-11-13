load(file = "data/verwerkt/koppeling/koppeling_mi_all_matches_o2.rdata")

o2_ec20_aggr <- koppeling_mi_all_matches_o2 %>%
  st_drop_geometry() %>%
  select(meetplaats, monsternamedatum, qual_o2, qual_ec_20) %>%
  group_by(meetplaats, monsternamedatum) %>%
  summarise(mean_o2 = mean(qual_o2, na.rm = T),
            min_o2 = min(qual_o2, na.rm = T),
            max_o2 = max(qual_o2, na.rm = T),
            mean_ec20 = mean(qual_ec_20, na.rm = T),
            min_ec20 = min(qual_ec_20, na.rm = T),
            max_ec20 = max(qual_ec_20, na.rm = T)
            ) %>%
  drop_na(mean_o2)

load(file = here("source", "analyse", "sem", "mi_nat_sv_kempen", "dredge_data_kempen.rdata"))


test <- dredge_data_kempen %>%
  left_join(o2_ec20_aggr,
            by = c("meetplaats", "monsternamedatum"))

model_test <- glmmTMB(data = test,
        formula = ept_prop ~ czv_log + natuur_oever_s + scale(min_o2) + zs_log + jaar_s +      (1 | bekken) + (1 | owl),
        family = binomial,
        weights = test$ta_xw)
summary(model_test)

model_test2 <- glmmTMB(data = test,
                      formula = scale(min_o2) ~ intensiteit_combo_oeverzone_s + overstorten_blootstelling_index_log +
                        p_t_log + t_s + jaar_s + (1 | meetplaats) + (1 | bekken) +      (1 | owl),
                      family = gaussian)
summary(model_test2)

###########
# Nutrients
##########

nutrients_aggr <- koppeling_mi_all_matches_nutrient %>%
  st_drop_geometry() %>%
  # select(meetplaats, monsternamedatum, qual_o2, qual_ec_20) %>%
  group_by(meetplaats, monsternamedatum) %>%
  summarise(mean_no2 = mean(qual_no2, na.rm = T),
            min_no2 = min(qual_no2, na.rm = T),
            max_no2 = max(qual_no2, na.rm = T),
            mean_zs = mean(qual_zs, na.rm = T),
            min_zs = min(qual_zs, na.rm = T),
            max_zs = max(qual_zs, na.rm = T),
            mean_no3 = mean(qual_no3, na.rm = T),
            min_no3 = min(qual_no3, na.rm = T),
            max_no3 = max(qual_no3, na.rm = T),
            mean_n_t = mean(qual_n_t, na.rm = T),
            min_n_t = min(qual_n_t, na.rm = T),
            max_n_t = max(qual_n_t, na.rm = T)
  ) %>%
  drop_na(mean_no2)

test2 <- test %>%
  left_join(nutrients_aggr,
            by = c("meetplaats", "monsternamedatum"))

model_test <- glmmTMB(data = test2,
                      formula = ept_prop ~ czv_log + natuur_oever_s + scale(min_o2) + zs_log + jaar_s +      (1 | bekken) + (1 | owl),
                      family = binomial,
                      weights = test$ta_xw)
summary(model_test)

model_test2<- glmmTMB(data = test2,
                      formula = log(max_zs) ~ intensiteit_combo_oeverzone_s + overstorten_blootstelling_index_log +
                        spei6_s + verharding_afstr_s + jaar_s + (1 | owl) + (1 | bekken),
                      family = gaussian)
summary(model_test2)

model_test2 <- glmmTMB(data = test2,
                      formula = log(mean_n_t) ~ intensiteit_combo_afstr_s + lozingen_riool_ie_log +
                        natuur_oever_s + zs_log + jaar_s + (1 | meetplaats) +      (1 | bekken) + (1 | owl),
                      family = gaussian)
summary(model_test2)
r.squaredGLMM(model_test2)

