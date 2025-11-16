library(brms)
# conflicted::conflicts_prefer(brms::ar)
# conflicted::conflicts_prefer(dplyr::as_data_frame)
# conflicted::conflicts_prefer(brms::autocor)
# conflicted::conflicts_prefer(igraph::blocks)
# conflicted::conflicts_prefer(igraph::compare)
# 1. Definieer de brms formules (bf objecten)
# LET OP: we moeten de hiërarchische random effecten vertalen naar brms syntaxis:
# (1 | bekken/meetplaats)  ==> (1 | bekken) + (1 | bekken:meetplaats)

# M1: kjn (Gaussian)
brm_m1 <- bf(kjn ~ landbouw_intens_afstr + ekc2_waterlichaam + jaar_scaled +
               (1 | bekken) + (1 | bekken:meetplaats),
             family = gaussian())

# M3: p_t (Gaussian) - Afhankelijk van kjn
brm_m3 <- bf(p_t ~ landbouw_intens_afstr + ekc2_waterlichaam + kjn + jaar_scaled +
               (1 | bekken) + (1 | bekken:meetplaats),
             family = gaussian())

# M5: aantal_pesticiden_met_overschrijding (Gaussian)
brm_m5 <- bf(aantal_pesticiden_met_overschrijding ~ landbouw_intens_afstr + ekc2_waterlichaam + jaar_scaled +
               (1 | bekken) + (1 | bekken:meetplaats),
             family = gaussian())

# M4: o2 (Gaussian) - Afhankelijk van p_t, kjn, aantal_pesticiden_met_overschrijding
brm_m4 <- bf(o2 ~ landbouw_intens_afstr + p_t + kjn + aantal_pesticiden_met_overschrijding +
               ekc2_waterlichaam + jaar_scaled + aantal_overstorten +
               (1 | bekken) + (1 | bekken:meetplaats),
             family = gaussian())

# M2: ep_tw (Poisson) - Afhankelijk van alle tussenliggende variabelen (kjn, aantal_pesticiden_met_overschrijding, o2, p_t)
# Gebruik het offset argument als term in de vaste effecten formule
brm_m2 <- bf(ep_tw ~ offset(log(ta_xw)) + kjn + landbouw_intens_afstr + aantal_pesticiden_met_overschrijding +
               ekc2_waterlichaam + o2 + jaar_scaled + p_t +
               (1 | bekken) + (1 | bekken:meetplaats),
             family = poisson(link = "log"))


# 2. Combineer de formules in één brms model
# De volgorde waarin je ze combineert (m1 t/m m5) weerspiegelt de structuur van de pSEM.
brms_sem_model <- brm(
  brm_m1 + brm_m3 + brm_m5 + brm_m4 + brm_m2 +
    set_rescor(FALSE), # Cruciaal: set_rescor(FALSE) is nodig om een recursief (pSEM) model te fitten.
  # Dit zorgt ervoor dat de residuen tussen de modellen niet gecorreleerd zijn.
  data = fc_lu_data_clean,
  chains = 4,
  iter = 2000,
  cores = 4,
  control = list(adapt_delta = 0.95) # Vaak nodig bij complexe modellen
)

