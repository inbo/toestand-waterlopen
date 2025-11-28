# Laad de benodigde pakketten
library(dplyr)
library(party) # Voor cforest
library(glmmTMB)

# Maak een fictieve dataset
set.seed(42)
n <- 300
df_jouwdata <- data.frame(
  meetplaats = factor(paste0("MP_", 1:100)),
  mmif = runif(n, 0.4, 0.8),
  jaar = sample(c(2013, 2016, 2019, 2022), n, replace = TRUE),
  verharding_afstroomgebied = runif(n, 0, 50),
  hooggroen_afstroomgebied = runif(n, 0, 80),
  verharding_oever = runif(n, 0, 20),
  hooggroen_oever = runif(n, 0, 40),
  groep = factor(sample(c("AGR", "URB", "NAT"), n, replace = TRUE)),
  statuut = factor(sample(c("beschermd", "standaard"), n, replace = TRUE))
)

# Voeg een realistische relatie toe aan de data
df_jouwdata$mmif <- df_jouwdata$mmif -
  0.005 * df_jouwdata$verharding_afstroomgebied -
  0.01 * df_jouwdata$verharding_oever +
  0.008 * df_jouwdata$hooggroen_afstroomgebied
df_jouwdata$mmif[df_jouwdata$groep == "URB"] <- df_jouwdata$mmif[df_jouwdata$groep == "URB"] - 0.1
df_jouwdata$mmif[df_jouwdata$mmif < 0] <- 0
df_jouwdata$mmif[df_jouwdata$mmif > 1] <- 1

# Deel de data op in een matrix (drivers) en een vector (respons)
# Verwijder de meetplaatsen en de respons
drivers <- select(df_jouwdata, -meetplaats, -mmif)
respons <- df_jouwdata$mmif

# Voer de cforest variabele-selectie uit
# De opties 'mtry' en 'ntree' kunnen worden aangepast voor optimale prestaties
# De optie 'sampsize' stelt de grootte van de subsamples in
# 'replace = FALSE' is cruciaal voor onbevooroordeelde selectie
cf_model <- cforest(
  mmif ~ .,
  data = select(df_jouwdata, -meetplaats),
  controls = cforest_unbiased(
    ntree = 500,
    mtry = 3, # Aantal variabelen per split
    sampsize = floor(0.632 * n),
    replace = FALSE # Subsampling zonder teruglegging
  )
)

# Bereken de Permutation Importance
# Gebruik 'mincriterion = 0' om de belangrijkste variabelen te identificeren
importance_scores <- varimp(cf_model, pre1.0_0 = TRUE)
importance_scores <- importance_scores[order(-importance_scores)]

# Toon de belangrijkheid van de variabelen
print(importance_scores)

# Selecteer de top 5 variabelen
top_vars <- names(importance_scores[1:5])
print(paste("Geselecteerde variabelen voor GLMM:", paste(top_vars, collapse = ", ")))

# Bouw de formule voor het glmmTMB-model met de geselecteerde variabelen
# De meetplaats wordt toegevoegd als een random effect
formule <- as.formula(
  paste("mmif ~", paste(top_vars, collapse = " + "), " + (1 | meetplaats)")
)

# Train het glmmTMB-model
glmmtmb_model <- glmmTMB(
  formule,
  data = df_jouwdata,
  family = "gaussian"
)

# Bekijk de resultaten van het model
summary(glmmtmb_model)
