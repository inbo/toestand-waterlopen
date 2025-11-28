vif <- glm(data = fc_lu_data_clean,
              kjn ~ landbouw_intens_afstr_s + ekc2_waterlichaam_s + jaar_s + Neerslag_som_1jaar_s + Neerslag_som_10dagen_s + aantal_overstorten_500m_s + hooggroen_oever_s)
vif(vif)

cor.test(fc_lu_data_clean$jaar_s, fc_lu_data_clean$Neerslag_som_1jaar_s)

mean(fc_lu_data_clean$jaar_s)
hist(fc_lu_data_clean$kjn)
hist(log(fc_lu_data_clean$kjn))

hist(fc_lu_data_clean$o2)
hist(log(fc_lu_data_clean$o2))

fc_lu_data_clean$Neerslag_som_1jaar_s %>% mean()

m1 <- glmmTMB(data = fc_lu_data_clean,
              kjn_log ~ landbouw_intens_afstr_s + ekc2_waterlichaam_s + jaar_s + Neerslag_som_1jaar_s + Neerslag_som_10dagen_s + aantal_overstorten_500m_s + hooggroen_oever_s + (1 | meetplaats) ,
              family = gaussian)

m1x <- glmmTMB(data = fc_lu_data_clean,
              kjn ~ landbouw_intens_afstr_s + ekc2_waterlichaam_s + jaar_s + Neerslag_som_1jaar_s + Neerslag_som_10dagen_s + aantal_overstorten_500m_s + hooggroen_oever_s + (1 | meetplaats) ,
              family = gaussian)


m3x <- glmmTMB(data = fc_lu_data_clean,
              p_t_log ~ landbouw_intens_afstr_s + ekc2_waterlichaam_s  + kjn_log + jaar_s + aantal_overstorten_500m_s +
                Neerslag_som_1jaar_s + Neerslag_som_10dagen_s + hooggroen_oever_s + (1 | meetplaats),
              family = gaussian)

m4 <- glmmTMB(data = fc_lu_data_clean,
              o2 ~  landbouw_intens_afstr_s + p_t_log + kjn_log + aantal_pesticiden_met_overschrijding + aantal_overstorten_500m_s +  Neerslag_som_1jaar_s + Neerslag_som_10dagen_s + hooggroen_oever_s + t_s +
                ekc2_waterlichaam_s + jaar_s + (1 | meetplaats),
              family = tweedie)

check_residuals(m4x)
summary(m4)
summary(m4x)
simulationOutput <- simulateResiduals(m1, plot = TRUE)
AIC(m1)
r2(m1)

data("meadows")
jutila_psem <- psem(
  lm(rich ~ elev + mass, meadows),
  lm(mass ~ elev, meadows)
)

multigroup(jutila_psem, group = "grazed")


#test aantal punten

# Selecteer alleen de noodzakelijke variabelen en verwijder NA's
data_sem_clean <- mi_nat_sv %>%
  dplyr::select(groep, bekken, statuut, meetplaats, owl.x, ep_tw, ta_xw, ns_tw, sw_dw, mt_sw, mmif, mmif_20, n_t, ph, t_fc, ec_20_fc, o2_fc, p_t, landbouw_intens_afstr, hooggroen_afstr, hooggroen_oever, jaar, kjn, aantal_pesticiden_met_overschrijding, aantal_zware_metalen_met_overschrijding, Neerslag_som_10dagen, Neerslag_som_1jaar, ekc2_waterlichaam, aantal_overstorten_500m) %>%
  tidyr::drop_na() %>%
  filter(groep %in% c("beek")) %>%
  mutate(across(.cols = n_t:aantal_overstorten_500m, # Selects n_t and all columns to the end
                .fns = ~as.numeric(scale(.x)),
                .names = "{.col}_s"))
load("data/verwerkt/mi_nat_sv.rdata")
load(here("data", "verwerkt", "landgebruik", "intensiteit_landbouw_scores.rdata"))
test <- mi_nat_sv %>%
  left_join(intensiteit_landbouw_scores)

test %>%
  ggplot(aes(intensiteit_gewasbescherming, intensiteit_nitraatresidu)) +
  geom_point() +
  geom_smooth()

cor.test(test$intensiteit_combo, test$intensiteit_gewasbescherming)
cor.test(test$intensiteit_nitraatresidu, test$intensiteit_gewasbescherming)

test %>%
  ggplot(aes(intensiteit_gewasbescherming, aantal_pesticiden_met_overschrijding)) +
  geom_point() +
  geom_smooth()

test %>%
  ggplot(aes(intensiteit_nitraatresidu, kjn)) +
  geom_point() +
  geom_smooth()

test %>%
  ggplot(aes(intensiteit_nitraatresidu, landbouw_intens_afstr)) +
  geom_point() +
  geom_smooth()


model <- glmmTMB(data = test,
                 log(nh4) ~ intensiteit_nitraatresidu + (1 | meetplaats))
summary(model)
r2(model)
r.squaredGLMM(model)


model <- glmmTMB(data = test,
                 intensiteit_nitraatresidu ~ landbouw_intens_afstr + (1 | meetplaats))
summary(model)
r2(model)
r.squaredGLMM(model)





###toxicolo

data_EC50_master %>%
  filter(!class %in% niet_relevante_soortgroepen) %>%
  select(cas) %>%
  unique() %>%
  filter(
    cas %in% uw_CAS_nummers)
