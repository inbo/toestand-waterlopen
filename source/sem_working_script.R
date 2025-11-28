###EC20 aanpassen in het model
load("data/verwerkt/mi_nat_sv.rdata")

source("source/inladen_packages.R")
# Selecteer alleen de noodzakelijke variabelen en verwijder NA's
data_sem_clean0 <- mi_nat_sv %>%
  dplyr::select(groep, monsternamedatum, bekken, statuut, meetplaats, owl.x, ep_tw, ta_xw, ns_tw, sw_dw, mt_sw, mmif, mmif_20, n_t, ph, t_fc, ec_20_fc, o2_verz_fc, o2_fc, p_t, landbouw_intens_afstr, akker, hooggroen_afstr, hooggroen_oever, jaar, kjn, aantal_pesticiden_met_overschrijding, aantal_zware_metalen_met_overschrijding, Neerslag_som_10dagen, Neerslag_som_1jaar, ekc2_waterlichaam, aantal_overstorten_500m, intensiteit_combo) %>%
  tidyr::drop_na() %>%
  filter(groep %in% c("beek")) %>%
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
                o2 = o2_fc,
                groep_dummy = ifelse(groep == "beek", 0, 1)
                )

# # VIF
# vif_model <- glm(ep_tw ~ n_t + p_h + ec_20 + o2 + p_t + landbouw_intens_afstr + hooggroen_afstr + hooggroen_oever + jaar_scaled + kjn + aantal_overstorten_500m + aantal_pesticiden_met_overschrijding + Neerslag_som_10dagen + Neerslag_som_1jaar + ekc2_waterlichaam,
#                  family = poisson(link = "log"),
#                  na.action = na.omit,
#                  data = data_sem_clean)
# vif(vif_model)
# vif(update(vif_model, . ~ . - hooggroen_afstr - kjn))



# M1: N_T (Gaussian)
m1 <- glmmTMB(data = data_sem_clean,
              kjn_log ~ landbouw_intens_afstr_s + ekc2_waterlichaam_s + t_fc_s + jaar_s + Neerslag_som_1jaar_s + Neerslag_som_10dagen_s + aantal_overstorten_500m_s + hooggroen_oever_s + (1 | meetplaats),
              family = gaussian)


m3 <- glmmTMB(data = data_sem_clean,
              p_t_log ~ landbouw_intens_afstr_s + ekc2_waterlichaam_s  + kjn_log + jaar_s + aantal_overstorten_500m_s +
               Neerslag_som_1jaar_s + Neerslag_som_10dagen_s + hooggroen_oever_s + t_fc_s  + (1 | meetplaats),
              family = gaussian)

m4 <- glmmTMB(data = data_sem_clean,
              o2_fc_s ~  landbouw_intens_afstr_s + p_t_log + kjn_log + aantal_pesticiden_met_overschrijding + aantal_overstorten_500m_s +  Neerslag_som_1jaar_s + Neerslag_som_10dagen_s + hooggroen_oever_s + t_fc_s +
                ekc2_waterlichaam_s + jaar_s + (1 | meetplaats),
              family = gaussian)

m5 <- glmmTMB(data = data_sem_clean,
             aantal_pesticiden_met_overschrijding ~ landbouw_intens_afstr_s + t_fc_s + hooggroen_oever_s + Neerslag_som_1jaar_s  + Neerslag_som_10dagen_s +  ekc2_waterlichaam_s + jaar_s + aantal_overstorten_500m_s +  (1 | meetplaats),
              family = nbinom1)


m2 <- glmmTMB(
  mmif ~ kjn_log + landbouw_intens_afstr_s + Neerslag_som_1jaar_s + Neerslag_som_10dagen_s + hooggroen_oever_s + aantal_pesticiden_met_overschrijding + ekc2_waterlichaam_s + o2_fc_s + jaar_s + p_t_log + aantal_overstorten_500m_s + t_fc_s + (1 | meetplaats),
  family = ordbeta,
  data = data_sem_clean)

# m2 <- glmmTMB(
#   ept_prop ~ kjn_log + landbouw_intens_afstr_s + Neerslag_som_1jaar_s + Neerslag_som_10dagen_s + hooggroen_oever_s + aantal_pesticiden_met_overschrijding + ekc2_waterlichaam_s + o2_fc_s + jaar_s + p_t_log + aantal_overstorten_500m_s + t_fc_s + (1 | meetplaats),
#   weights = data_sem_clean$ta_xw,
#   data = data_sem_clean,
#   family =  binomial)

# m2 <- glmmTMB(
#   ta_xw ~ kjn + landbouw_intens_afstr + Neerslag_som_1jaar_s + Neerslag_som_10dagen_s + hooggroen_oever_s + aantal_pesticiden_met_overschrijding_s + ekc2_waterlichaam_s + o2_s + jaar_s + p_t_s + aantal_overstorten_500m_s + (1 | bekken/meetplaats),
#   data = data_sem_clean,
#   family =  poisson)
#
# m2 <- glmmTMB(
#   mt_sw_prop ~ kjn_s + landbouw_intens_afstr_s + Neerslag_som_1jaar_s + Neerslag_som_10dagen_s + hooggroen_oever_s + aantal_pesticiden_met_overschrijding + ekc2_waterlichaam_s + o2_s + jaar_s + p_t_s + aantal_overstorten_500m_s + (1 | meetplaats),
#   family = ordbeta,
#   data = data_sem_clean)
#
# m2 <- glmmTMB(
#   mt_sw ~ kjn + landbouw_intens_afstr + scaled_neerslag_jaar + scaled_neerslag_piek + aantal_pesticiden_met_overschrijding + ekc2_waterlichaam + o2 + jaar_scaled + p_t  + aantal_overstorten_500m  + (1 | bekken/meetplaats),
#   data = data_sem_clean)
#
# m2 <- glmmTMB(
#   nst_prop ~ kjn_s + landbouw_intens_afstr_s + Neerslag_som_1jaar_s + Neerslag_som_10dagen_s + hooggroen_oever_s + aantal_pesticiden_met_overschrijding + ekc2_waterlichaam_s + o2_s + jaar_s + p_t_s + aantal_overstorten_500m_s + (1 | meetplaats),
#   weights = data_sem_clean$ta_xw,
#   data = data_sem_clean,
#   family =  binomial(link = "logit"))
#
# m2 <- glmmTMB(
#   stress_prop ~ kjn + landbouw_intens_afstr + scaled_neerslag_jaar + aantal_pesticiden_met_overschrijding + ekc2_waterlichaam + o2 + jaar_scaled + p_t + aantal_overstorten_500m + (1 | meetplaats),
#   weights = data_sem_clean$ta_xw,
#   data = data_sem_clean,
#   family =  binomial(link = "logit"))
#
# simulationOutput <- simulateResiduals(m1, plot = TRUE)
# testDispersion(simulationOutput) # geen overdispersie
# testZeroInflation(simulationOutput) # geen zero_inflation
# testUniformity(simulationOutput)

sem_resultaat <- psem(m1, m2, m3, m4, m5)
# multigroup(sem_resultaat, group = data_sem_clean$groep_dummy)
summary(sem_resultaat)
# coefs(sem_resultaat)
# plot(sem_resultaat)


library(piecewiseSEM)
library(dplyr)
library(igraph)

# 1️⃣ Extract coëfficiënten uit je SEM
coefs_df <- coefs(sem_resultaat)[,-9]

source("source/Overige scripts/sem_standardised_coef_manually.R")

coef_df <- coefs_df
# 2️⃣ Filter enkel significante paden (p < 0.05)
sig_coefs <- coef_df %>%
  filter(P.Value < 0.05)

# 3️⃣ Maak een igraph-object van de significante relaties
g <- igraph::graph_from_data_frame(
  d = sig_coefs %>%
    select(Predictor, Response, Std.Estimate),
  directed = TRUE
)

# 4️⃣ Pijldikte & kleur volgens effectgrootte
E(g)$width <- abs(sig_coefs$Std.Estimate) * 6
E(g)$color <- ifelse(sig_coefs$Std.Estimate > 0, "darkgreen", "firebrick")

# 5️⃣ Labels toevoegen met effectwaarden (afgerond)
E(g)$label <- round(sig_coefs$Std.Estimate, 2)
E(g)$label.cex <- 1
E(g)$label.color <- "black"

# 6️⃣ Maak een nette plot
plot(
  g,
  # layout = layout_as_tree(g, root = "landbouw_intens_afstr"),
  vertex.size = 30,
  vertex.color = "lightblue",
  vertex.frame.color = "grey40",
  vertex.label.color = "black",
  vertex.label.cex = 1.1,
  edge.arrow.size = 0.6,
  edge.curved = 0.1,
  main = "Significante paden (p < 0.05) met effectgroottes"
)

