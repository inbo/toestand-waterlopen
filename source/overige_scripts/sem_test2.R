# Selecteer alleen de noodzakelijke variabelen en verwijder NA's
fc_lu_data_clean <- fc_lu_data %>%
  dplyr::select(ta_xw, mmif, mmif_20, n_t, p_h, landbouw_intens_afstr, jaar, meetplaats, statuut) %>%
  tidyr::drop_na()

# Zorg ervoor dat de respons term (20 - mmif_20) ook correct is
fc_lu_data_clean <- fc_lu_data_clean %>%
  dplyr::mutate(mmif_20 = as.integer(mmif_20))
# M1: N_T (Gaussian)
m1 <- glmmTMB(data = fc_lu_data_clean,
              n_t ~ landbouw_intens_afstr + jaar + (1 | meetplaats),
              family = gaussian)

# M2: MMIF (Binomial)
# m2 <- glmmTMB(data = fc_lu_data_clean,
#               cbind(mmif_20, 20 - mmif_20) ~ n_t + landbouw_intens_afstr + jaar + (1 | meetplaats),
#               family = binomial(link = "logit"))

m2 <- glmmTMB(
  mmif ~ n_t + landbouw_intens_afstr + jaar + p_h + (1 | meetplaats),
  data = fc_lu_data_clean,
  family = binomial,
  weights = rep(20, nrow(fc_lu_data_clean))  # 20 trials per observatie
)

sem_resultaat <- psem(m1, m2)

summary(sem_resultaat)
coefs(sem_resultaat)
