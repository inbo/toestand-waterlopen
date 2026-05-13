library(glmmTMB)
library(DHARMa)

# 1. Haal de conditionele voorspellingen op (Xb + Zu)
# Dus inclusief de afwijkingen per meetplaats
preds <- predict(model, type = "link") # Op de link-schaal (voor gaussian is dit gelijk aan response)

# 2. Haal de residuele standaardafwijking (sigma) op
sig <- sigma(model)

# 3. Simuleer handmatig nieuwe data (nsim = 250 is de DHARMa standaard)
nsim <- 1000
sim_data <- matrix(nrow = length(preds), ncol = nsim)

for(i in 1:nsim) {
  # We voegen normale ruis toe aan onze conditionele voorspellingen
  sim_data[,i] <- rnorm(length(preds), mean = preds, sd = sig)
}

# 4. Maak een custom DHARMa object
# We vergelijken de geobserveerde data met onze handmatige simulaties
res_cond <- createDHARMa(
  simulatedResponse = sim_data,
  observedResponse = data_model$mmif,
  fittedPredictedResponse = preds,
  integerResponse = FALSE
)

# 5. Voer de SAC-test uit op deze conditionele residuelen
# Gebruik hier weer je gegroepeerde locaties zoals eerder
res_grouped_cond <- recalculateResiduals(res_cond, group = data_model$meetplaats)

testSpatialAutocorrelation(
  res_grouped_cond,
  x = locaties_match$x,
  y = locaties_match$y
)

#### zonder RE ####

# 1. Haal de marginale voorspellingen op (Enkel Xb, zonder Zu)
# re.form = ~0 zorgt ervoor dat alle random effects op 0 worden gezet
preds_marg <- predict(model, re.form = ~0, type = "link")

# 2. Haal de residuele standaardafwijking op
sig <- sigma(model)

# 3. Simuleer handmatig (Marginaal)
nsim <- 1000
sim_data_marg <- matrix(nrow = length(preds_marg), ncol = nsim)

for(i in 1:nsim) {
  # We gebruiken hier enkel de populatie-gemiddelde voorspelling
  sim_data_marg[,i] <- rnorm(length(preds_marg), mean = preds_marg, sd = sig)
}

# 4. Maak het custom DHARMa object
res_marg_manual <- createDHARMa(
  simulatedResponse = sim_data_marg,
  observedResponse = data_model$mmif,
  fittedPredictedResponse = preds_marg,
  integerResponse = FALSE
)



res_grouped_marg <- recalculateResiduals(res_marg_manual, group = data_model$meetplaats)

testSpatialAutocorrelation(
  res_grouped_marg,
  x = locaties_match$x,
  y = locaties_match$y
)

# 5. Vergelijkingstest
# Check of deze residuelen hetzelfde zijn als je originele DHARMa object
cor(res_marg_manual$scaledResiduals, res$scaledResiduals)

#### ordbeta ####

library(glmmTMB)
library(DHARMa)

# 1. Haal de conditionele parameters uit het model
# mu_cond is de voorspelling inclusief de random effects (Zu)
mu_cond <- predict(model, type = "response")
phi <- sigma(model)

# Voor Ordbeta moeten we ook rekening houden met de 'theta' (de drempels voor 0 en 1)
# We gebruiken de interne glmmTMB simulatie-logica in een loop
nsim <- 1000
sim_data_cond <- matrix(nrow = length(mu_cond), ncol = nsim)

# We simuleren handmatig uit de verdeling die glmmTMB gebruikt
for(i in 1:nsim) {
  # simulate() werkt wel als we een 'nieuw' modelobject zouden hebben,
  # maar we doen het hier via de verdelingsfunctie van glmmTMB:
  sim_data_cond[,i] <- simulate(model, nsim = 1, re.form = NULL, seed = i)[[1]]
}

# 2. Maak het handmatige DHARMa object
# We gebruiken mu_cond als de 'fittedPredictedResponse'
res_cond <- createDHARMa(
  simulatedResponse = sim_data_cond,
  observedResponse = data_model$mmif,
  fittedPredictedResponse = mu_cond,
  integerResponse = FALSE
)

# 3. Maak het overzicht van UNIEKE locaties (zoals je eerder deed)
locaties_match <- data_model %>%
  group_by(meetplaats) %>%
  summarize(x = dplyr::first(x), y = dplyr::first(y), .groups = "drop") %>%
  arrange(meetplaats)

# 4. Aggregeer de residuelen per meetplaats
res_grouped_cond <- recalculateResiduals(res_cond, group = data_model$meetplaats)

# 5. De Ruimtelijke Autocorrelatie Test
# Als deze P > 0.05 is, dan heeft je model de SAC succesvol opgevangen via de RE's
testSpatialAutocorrelation(res_grouped_cond,
                           x = locaties_match$x,
                           y = locaties_match$y)

# 6. Bonus: Check correlatie met de standaard DHARMa output
res_standard <- simulateResiduals(model) # De marginale versie
cat("Correlatie tussen Marginaal en Conditioneel:",
    cor(res_cond$scaledResiduals, res_standard$scaledResiduals))


library(glmmTMB)
library(DHARMa)

# 1. Haal de MARGINALE parameters uit het model
# mu_marg is de voorspelling ZONDER de random effects (enkel Xb)
mu_marg <- predict(model, type = "response", re.form = ~0)
phi <- sigma(model)

# 2. Handmatige simulatie loop (Marginaal: re.form = ~0)
nsim <- 1000
sim_data_marg <- matrix(nrow = length(mu_marg), ncol = nsim)

for(i in 1:nsim) {
  # Door re.form = ~0 te gebruiken, simuleren we alsof de random effects er niet zijn
  # We gebruiken een vaste seed per i om vergelijking stabiel te houden
  sim_data_marg[,i] <- simulate(model, nsim = 1, re.form = ~0)[[1]]
}

# 3. Maak het handmatige DHARMa object (Marginaal)
res_marg_manual <- createDHARMa(
  simulatedResponse = sim_data_marg,
  observedResponse = data_model$mmif,
  fittedPredictedResponse = mu_marg,
  integerResponse = FALSE
)

# 4. Aggregeer de residuelen per meetplaats
res_grouped_marg <- recalculateResiduals(res_marg_manual, group = data_model$meetplaats)

# 5. De Ruimtelijke Autocorrelatie Test op de marginale residuelen
# Deze zou (net als je originele DHARMa plot) significant moeten testen op SAC
cat("\n--- SAC Test: Handmatig Marginaal (Zonder RE) ---\n")
testSpatialAutocorrelation(res_grouped_marg,
                           x = locaties_match$x,
                           y = locaties_match$y)

# 6. De ultieme check: Correlatie met de standaard DHARMa output
res_standard <- simulateResiduals(model, nsim = 1000)

cat("\n--- Vergelijking met standaard DHARMa ---\n")
cat("Correlatie tussen Standaard DHARMa en Handmatig Marginaal:",
    cor(res_marg_manual$scaledResiduals, res_standard$scaledResiduals))
