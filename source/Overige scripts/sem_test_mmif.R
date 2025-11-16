# Selecteer alleen de noodzakelijke variabelen en verwijder NA's
fc_lu_data_clean <- fc_lu_data %>%
  left_join(overschrijdingen %>%
              group_by(meetplaats, jaar) %>%
              summarise(aantal_stoffen_met_overschrijding =
                          mean(aantal_stoffen_met_overschrijding),
                        aantal_pesticiden_met_overschrijding =
                          mean(aantal_pesticiden_met_overschrijding)),
            by = c("meetplaats", "jaar")) %>%
  ungroup() %>%
  mutate(jaar_scaled = as.numeric(scale(jaar))) %>%
  dplyr::select(ep_tw, ta_xw, mmif, mmif_20, n_t, p_h, o2, p_t, landbouw_intens_afstr, landbouw_intens_oever, hooggroen_afstr, hooggroen_oever, jaar, jaar_scaled, meetplaats, statuut, kjn, groep, aantal_pesticiden_met_overschrijding, aantal_stoffen_met_overschrijding) %>%
  tidyr::drop_na() %>%
  filter(groep %in% c( "beek"))


# Zorg ervoor dat de respons term (20 - mmif_20) ook correct is
fc_lu_data_clean <- fc_lu_data_clean %>%
  dplyr::mutate(ep_tw = as.integer(ep_tw),
                ta_xw = as.integer(ta_xw))
# M1: N_T (Gaussian)
m1 <- glmmTMB(data = fc_lu_data_clean,
              kjn ~ landbouw_intens_afstr + hooggroen_oever + jaar_scaled + (1 | meetplaats),
              family = gaussian)

m3 <- glmmTMB(data = fc_lu_data_clean,
              p_t ~ landbouw_intens_afstr + hooggroen_oever + kjn + jaar_scaled + (1 | meetplaats),
              family = gaussian)

m4 <- glmmTMB(data = fc_lu_data_clean,
              o2 ~ landbouw_intens_afstr + p_t + kjn + hooggroen_oever + aantal_pesticiden_met_overschrijding + jaar_scaled + (1 | meetplaats),
              family = gaussian)
m5 <- glmmTMB(data = fc_lu_data_clean,
              aantal_pesticiden_met_overschrijding ~ landbouw_intens_afstr + hooggroen_oever  + jaar_scaled + (1 | meetplaats),
              family = gaussian)

# M2: MMIF (Binomial)
# m2 <- glmmTMB(data = fc_lu_data_clean,
#               cbind(mmif_20, 20 - mmif_20) ~ n_t + landbouw_intens_afstr + jaar_scaled + (1 | meetplaats),
#               family = binomial(link = "logit"))

m2 <- glmmTMB(
  mmif ~ kjn + landbouw_intens_afstr + jaar_scaled + aantal_pesticiden_met_overschrijding + hooggroen_oever + p_t + (1 | meetplaats),
  data = fc_lu_data_clean,
  family = binomial,
  weights = rep(20, nrow(fc_lu_data_clean))  # 20 trials per observatie
)

m2 <- glmmTMB(
  ep_tw ~ kjn + landbouw_intens_afstr + aantal_pesticiden_met_overschrijding + o2 + jaar_scaled + p_t + hooggroen_oever +  (1 | meetplaats),
  data = fc_lu_data_clean,
  family =  poisson(link = "log"))

sem_resultaat <- psem(m1, m2, m3, m4, m5)

summary(sem_resultaat)
coefs(sem_resultaat)
plot(sem_resultaat)


library(piecewiseSEM)
library(dplyr)

# Extracteer coëfficiënten
coefs_df <- coefs(sem_resultaat)[,-9]

# Bepaal significantie (p < 0.05)
coefs_df <- coefs_df %>%
  mutate(sig = ifelse(P.Value < 0.05, TRUE, FALSE))

# Maak edge-attribuutlijst
edge_colors <- ifelse(coefs_df$sig, "black", "grey90")
edge_widths <- ifelse(coefs_df$sig, 2, 0.5)
edge_lty <- ifelse(coefs_df$sig, "solid", "dashed")

# Plot met aangepaste randstijlen
plot(sem_resultaat,
     edge_attrs = list(color = edge_colors,
                       width = edge_widths,
                       lty = edge_lty),
     node_attrs = list(label.cex = 0.9))

library(piecewiseSEM)
library(dplyr)
library(igraph)

# 1️⃣ Extract coëfficiënten uit je SEM
coef_df <- coefs(sem_resultaat)[,-9]

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

