###EC20 aanpassen in het model
load("data/temp/fc_lu_data.rdata")
load("data/verwerkt/neerslag_beide_periodes.rdata")
load(here("data", "verwerkt", "overschrijdingen.rdata"))
load(here("data", "verwerkt", "hm_data.rdata"))
load(file = here("data", "verwerkt", "overstorten", "overstort_tellingen_df.rdata"))

source("source/inladen_packages.R")
# Selecteer alleen de noodzakelijke variabelen en verwijder NA's
fc_lu_data_clean <- fc_lu_data %>%
  left_join(overschrijdingen %>%
              group_by(meetplaats, jaar) %>%
              summarise(aantal_stoffen_met_overschrijding =
                          mean(aantal_stoffen_met_overschrijding),
                        aantal_pesticiden_met_overschrijding =
                          mean(aantal_pesticiden_met_overschrijding),
                        aantal_zware_metalen_met_overschrijding =
                          mean(aantal_zware_metalen_met_overschrijding)),
            by = c("meetplaats", "jaar")) %>%
  left_join(hm_data, by = "meetplaats") %>%
  left_join(finale_resultaten_sequentieel,
            by = c("meetplaats", "monsternamedatum")) %>%
  dplyr::select(groep, bekken, statuut, meetplaats, owl, ep_tw, ta_xw, ns_tw, sw_dw, mt_sw, mmif, mmif_20, n_t, p_h, t, ec_20, o2, p_t, landbouw_intens_afstr, hooggroen_afstr, hooggroen_oever, jaar, kjn, aantal_pesticiden_met_overschrijding, aantal_zware_metalen_met_overschrijding, Neerslag_som_10dagen, Neerslag_som_1jaar,
                ekc2_waterlichaam) %>%
  left_join(overstort_tellingen_df %>%
              select(meetplaats, aantal_overstorten_500m),
            by = "meetplaats") %>%
  mutate(across(.cols = n_t:aantal_overstorten_500m, # Selects n_t and all columns to the end
                .fns = ~as.numeric(scale(.x)),
                .names = "{.col}_s")) %>%
  tidyr::drop_na() %>%
  filter(groep %in% c("beek"))

# # Correlatie en VIF
#
# numerieke_var <- fc_lu_data_clean %>%
#   dplyr::select(ep_tw, ta_xw, sw_dw, mt_sw, mmif, n_t, p_h, ec_20, o2, p_t, landbouw_intens_afstr, hooggroen_afstr, hooggroen_oever, jaar_scaled, kjn,
#                 # aantal_pesticiden_met_overschrijding,
#                 aantal_overstorten_500m,
#                 Neerslag_som_10dagen, Neerslag_som_1jaar,
#                 ekc2_waterlichaam) #
# cor_matrix <- cor(numerieke_var)
# corrplot(cor_matrix, method = "circle", type = "upper", diag = FALSE, addCoef.col = "black")

# Zorg ervoor dat de respons term (20 - mmif_20) ook correct is
fc_lu_data_clean <- fc_lu_data_clean %>%
  dplyr::mutate(ep_tw = as.integer(ep_tw),
                ta_xw = as.integer(ta_xw),
                ns_tw = as.integer(ns_tw),
                mt_sw_prop = mt_sw / 10,
                owl = as.factor(owl),
                bekken = as.factor(bekken),
                nst_prop = ns_tw / ta_xw,
                stress_prop = (ep_tw + ns_tw)/ta_xw,
                ept_prop = ep_tw / ta_xw)

# # VIF
# vif_model <- glm(ep_tw ~ n_t + p_h + ec_20 + o2 + p_t + landbouw_intens_afstr + hooggroen_afstr + hooggroen_oever + jaar_scaled + kjn + aantal_overstorten_500m + aantal_pesticiden_met_overschrijding + Neerslag_som_10dagen + Neerslag_som_1jaar + ekc2_waterlichaam,
#                  family = poisson(link = "log"),
#                  na.action = na.omit,
#                  data = fc_lu_data_clean)
# vif(vif_model)
# vif(update(vif_model, . ~ . - hooggroen_afstr - kjn))



# M1: N_T (Gaussian)
m1 <- glmmTMB(data = fc_lu_data_clean,
              kjn_s ~ landbouw_intens_afstr_s + ekc2_waterlichaam_s + jaar_s + Neerslag_som_1jaar_s + Neerslag_som_10dagen_s + aantal_overstorten_500m_s + hooggroen_oever_s + (1 | meetplaats),
              family = gaussian)


m3 <- glmmTMB(data = fc_lu_data_clean,
              p_t_s ~ landbouw_intens_afstr_s + ekc2_waterlichaam_s  + kjn_s + jaar_s + aantal_overstorten_500m_s +
               Neerslag_som_1jaar_s + Neerslag_som_10dagen_s + hooggroen_oever_s + (1 | meetplaats),
              family = gaussian)

m4 <- glmmTMB(data = fc_lu_data_clean,
              o2_s ~  landbouw_intens_afstr_s + p_t_s + kjn_s + aantal_pesticiden_met_overschrijding + aantal_overstorten_500m_s +  Neerslag_som_1jaar_s + Neerslag_som_10dagen_s + hooggroen_oever_s + t_s +
                ekc2_waterlichaam_s + jaar_s + (1 | meetplaats),
              family = gaussian)

m5 <- glmmTMB(data = fc_lu_data_clean,
             aantal_pesticiden_met_overschrijding ~ landbouw_intens_afstr_s + hooggroen_oever_s + Neerslag_som_1jaar_s + Neerslag_som_10dagen_s +  ekc2_waterlichaam_s + jaar_s + (1 | meetplaats),
              family = nbinom1)
MuMIn::r.squaredGLMM(m5)

m2 <- glmmTMB(
  mmif ~ kjn_s + landbouw_intens_afstr_s + Neerslag_som_1jaar_s + Neerslag_som_10dagen_s + hooggroen_oever_s + aantal_pesticiden_met_overschrijding + ekc2_waterlichaam_s + o2_s + jaar_s + p_t_s + aantal_overstorten_500m_s + t_s + (1 | meetplaats),
  family = ordbeta,
  data = fc_lu_data_clean)

m2 <- glmmTMB(
  ept_prop ~ kjn_s + landbouw_intens_afstr_s + Neerslag_som_1jaar_s + Neerslag_som_10dagen_s + hooggroen_oever_s + aantal_pesticiden_met_overschrijding + ekc2_waterlichaam_s + o2_s + jaar_s + p_t_s + aantal_overstorten_500m_s + t_s + (1 | meetplaats),
  weights = fc_lu_data_clean$ta_xw,
  data = fc_lu_data_clean,
  family =  binomial)

m2 <- glmmTMB(
  ta_xw ~ kjn + landbouw_intens_afstr + Neerslag_som_1jaar_s + Neerslag_som_10dagen_s + hooggroen_oever_s + aantal_pesticiden_met_overschrijding_s + ekc2_waterlichaam_s + o2_s + jaar_s + p_t_s + aantal_overstorten_500m_s + (1 | bekken/meetplaats),
  data = fc_lu_data_clean,
  family =  poisson)

m2 <- glmmTMB(
  mt_sw_prop ~ kjn_s + landbouw_intens_afstr_s + Neerslag_som_1jaar_s + Neerslag_som_10dagen_s + hooggroen_oever_s + aantal_pesticiden_met_overschrijding + ekc2_waterlichaam_s + o2_s + jaar_s + p_t_s + aantal_overstorten_500m_s + (1 | meetplaats),
  family = ordbeta,
  data = fc_lu_data_clean)

m2 <- glmmTMB(
  mt_sw ~ kjn + landbouw_intens_afstr + scaled_neerslag_jaar + scaled_neerslag_piek + aantal_pesticiden_met_overschrijding + ekc2_waterlichaam + o2 + jaar_scaled + p_t  + aantal_overstorten_500m  + (1 | bekken/meetplaats),
  data = fc_lu_data_clean)

m2 <- glmmTMB(
  nst_prop ~ kjn_s + landbouw_intens_afstr_s + Neerslag_som_1jaar_s + Neerslag_som_10dagen_s + hooggroen_oever_s + aantal_pesticiden_met_overschrijding + ekc2_waterlichaam_s + o2_s + jaar_s + p_t_s + aantal_overstorten_500m_s + (1 | meetplaats),
  weights = fc_lu_data_clean$ta_xw,
  data = fc_lu_data_clean,
  family =  binomial(link = "logit"))

m2 <- glmmTMB(
  stress_prop ~ kjn + landbouw_intens_afstr + scaled_neerslag_jaar + aantal_pesticiden_met_overschrijding + ekc2_waterlichaam + o2 + jaar_scaled + p_t + aantal_overstorten_500m + (1 | meetplaats),
  weights = fc_lu_data_clean$ta_xw,
  data = fc_lu_data_clean,
  family =  binomial(link = "logit"))

simulationOutput <- simulateResiduals(m5, plot = TRUE)
testDispersion(simulationOutput) # geen overdispersie
testZeroInflation(simulationOutput) # geen zero_inflation
testUniformity(simulationOutput)

sem_resultaat <- psem(m1, m2, m3, m4, m5)

summary(sem_resultaat)
coefs(sem_resultaat)
plot(sem_resultaat)


library(piecewiseSEM)
library(dplyr)
library(igraph)

# 1️⃣ Extract coëfficiënten uit je SEM
coefs_df <- coefs(sem_resultaat)[,-9]

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

