library(glmmTMB)
library(sf)

# 1. Haal XY coördinaten uit de geom (Lambert 72)
dredge_data_spatial <- dredge_data %>%
  # Zorg dat de geom gekoppeld is
  left_join(mi_nat_sv_beek %>% select(meetplaats, monsternamedatum, geom),
            by = c("meetplaats", "monsternamedatum")) %>%
  mutate(
    x = st_coordinates(geom)[,1] / 1000, # Omzetten naar km voor stabiliteit
    y = st_coordinates(geom)[,2] / 1000
  ) %>%
  # glmmTMB heeft een factor nodig om te weten welke punten bij elkaar horen
  # We maken een dummy variabele 'groep' (meestal gewoon 1 voor de hele dataset)
  mutate(pos = numFactor(x, y),
         groep = factor(1))

mmif_spatial_model <- glmmTMB(
  formula =mmif ~ ec_20_log + o2_s + spei6_s + jaar_s +
    (1 | bekken) + intensiteit_combo_afstr_s + t_s + n_t_log +      p_t_log + verharding_afstr_s +
    exp(pos + 0 | groep), # De spatiale correlatie term
  data = dredge_data_spatial,
  family = ordbeta(),
  # Soms heeft dit model wat extra hulp nodig om te convergeren
  control = glmmTMBControl(optCtrl = list(iter.max = 1000, eval.max = 1000))
)
summary(mmif_spatial_model)

mmif_spatial_model %>% AIC()
model %>% AIC()

res <- simulateResiduals(fittedModel = mmif_spatial_model, plot = TRUE)
res_grouped <- recalculateResiduals(res, group = data_model$meetplaats)

# res_grouped <- recalculateResiduals(res, group = dredge_data$bekken)

# 2. Haal de unieke locaties op voor de geaggregeerde residuen
# Belangrijk: de volgorde van de locaties moet matchen met de groepen
group_coords <- data_model %>%
  group_by(x, y) %>%
  summarize(.groups = "drop")

# group_coords <- aggregate(cbind(x, y) ~ bekken, data = data_model, mean)


# 3. Voer de test uit op de geaggregeerde residuen
testSpatialAutocorrelation(res_grouped,
                           x = group_coords$x,
                           y = group_coords$y)
