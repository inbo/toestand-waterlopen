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
simulationOutput <- simulateResiduals(m4, plot = TRUE)
AIC(m4)
r2(m4)

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
