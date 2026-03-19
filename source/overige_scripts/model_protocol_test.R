# --- 0. Preprocessing ---

# Laad de packages
library(readr)
library(dplyr)
library(lme4)   # Voor Mixed Models (LMM, GLMM)
library(car)    # Voor VIF
library(mgcv)   # Voor GAM(M)
library(DHARMa) # Voor modelvalidatie
library(gamm4)
library(glmmTMB)
library(performance)
source(here::here("source", "inladen_packages.R"))

# Lees de data in
load(here("data", "verwerkt", "mi_data.rdata"))

df <- mi_data %>%
  filter(categorie != "Vijver") %>%
  filter(waterlooptype != "Geïsoleerd water") %>%
  filter(waterlichaamcategorie != "meer") %>%
  filter(!meetplaats %in% c("OW113500", "OW12000", "OW179000", "OW536050", "OW669032", "OW690015", "OW917000", "OW981010", "OW981200"))
# %>%
#   filter(jaar > 2006)

# Sla overbodige kolommen over
df <- df %>%
  select(-deelmonster_id, -monsternamedatum, -secchi, -geometry, -ec_25)

# Verwijder rijen met missende waarden in de relevante kolommen
df_clean <- df %>%
  filter(!is.na(mmif) &
           !is.na(p_h) &
           !is.na(t) &
           !is.na(o2) &
           !is.na(o2_verz) &
           !is.na(ec_20) &
           !is.na(groep) &
           !is.na(bekken) &
           !is.na(jaar)) %>%
  filter(jaar > 2006)

# Transformeer de MMIF-score naar een integer (0-20)
df_clean$mmif_scaled <- df_clean$mmif * 20

# Zorg ervoor dat `bekken` en `meetplaats` als categorische variabelen worden behandeld
df_clean$bekken <- as.factor(df_clean$bekken)
df_clean$meetplaats <- as.factor(df_clean$meetplaats)
df_clean$groep <- as.factor(df_clean$groep)
df_clean$jaar <- as.integer(df_clean$jaar)

# --- 1. Detectie van Multicollineariteit (VIF) ---
# Een simpele lineaire model (lm) gebruiken om VIF te berekenen
# De random effects worden hier weggelaten, omdat VIF voor de vaste effecten is
vif_model <- lm(mmif ~ p_h + t + o2 + o2_verz + ec_20, data = df_clean)
vif_result <- vif(vif_model)
print("VIF-waarden:")
print(vif_result)

# Resultaten van de VIF-analyse:
# ec_20 en o2_verz lijken sterk gecorreleerd te zijn, wat logisch is.
# De VIF-waarden voor deze variabelen zullen waarschijnlijk hoog zijn.
# Overweeg om één van deze te verwijderen, of de resultaten voorzichtig te interpreteren.

# --- 2. Modellen ---

# A) Lineair Mixed Model (Gaussian) - Eerste referentiepunt, maar niet ideaal
# De data moet gefilterd worden op meetplaatsen met meerdere metingen
# Hieronder een voorbeeldmodel op basis van de user input
lmm_model <- lmer(mmif ~ scale(p_h) + scale(t) + scale(o2) + scale(o2_verz) + scale(ec_20) + groep + (1 | bekken / meetplaats),
                  data = df_clean)
summary(lmm_model)

# B) Binomiaal GLMM (aanbevolen)
# Dit model is de beste keuze voor de discrete MMIF-score.
# De respons is de telling (`mmif_scaled`), de totale trials zijn 20.
glmm_model <- glmer(cbind(mmif_scaled, 20 - mmif_scaled) ~
                      scale(p_h) + scale(t) + scale(o2) + scale(o2_verz) + scale(ec_20) + groep + (1 | bekken / meetplaats),
                    data = df_clean,
                    family = binomial)
summary(glmm_model)

# C) GAMM voor trendanalyse (indien niet-lineaire relaties verwacht)
# Smoothing spline voor `jaar` en `t` om niet-lineaire effecten te vangen.

gamm_model <- gam(cbind(mmif_scaled, 20 - mmif_scaled) ~
                    s(jaar) + s(t) + s(p_h) + s(o2) + s(ec_20) + groep +
                    s(meetplaats, bs = "re") + s(bekken, bs = "re"),
                  data = df_clean,
                  family = binomial,
                  method = "REML")
summary(gamm_model)

# Modelvalidatie voor de GLMM
# Een cruciaal aspect van elk model is validatie. De DHARMa package helpt om de aannames van het model te controleren.
simulationOutput <- simulateResiduals(fittedModel = glmm_model, plot = T)
plot(simulationOutput)

# De DHARMa plot helpt om te zien of de residuen uniform verdeeld zijn, wat een indicatie is van een goed passend model.
# Je kunt ook controleren op overdispersie met DHARMa
testDispersion(simulationOutput)

t1 <- Sys.time()
gamm4_model <- gamm4(cbind(mmif_scaled, 20 - mmif_scaled) ~
                       s(jaar) + s(t) + s(p_h) + s(o2_verz) + s(ec_20) + groep,
                     random = ~(1 | meetplaats),
                     data = df_clean,
                     family = binomial)
t2 <- Sys.time()
t2-t1
# Samenvatting van het model
summary(gamm4_model$gam)
summary(gamm4_model$mer)

plot_model(gamm4_model, type = "pred")

# Het object bevat twee delen: $gam voor de gladde termen en $mer voor de willekeurige effecten.
# Controleer de validatie van dit model met DHARMa.
# Omdat gamm4 een andere structuur heeft dan gam, moeten we de residuen iets anders berekenen:
# Hier is een voorbeeld van hoe dit te doen.
# simulationOutput_gamm4 <- simulateResiduals(fittedModel = gamm4_model$mer, plot = TRUE)
# testDispersion(simulationOutput_gamm4)

glmmtmb_model <- glmmTMB(cbind(mmif_scaled, 20 - mmif_scaled) ~
                           jaar + s(o2_verz) +
                           (1|bekken/meetplaats) + (1 | groep),
                         data = df_clean,
                         family = binomial(link = "logit"))

glmmtmb_model2 <- glmmTMB(mt_sw ~
                           scale(jaar) + scale(o2_verz) + groep +
                           (1|bekken/meetplaats),
                         data = df_clean,
                         family = glmmTMB::tweedie(link = "log"))

glmmtmb_model3 <- glmmTMB::glmmTMB(ta_xw ~
                            scale(jaar) + scale(o2_verz) + scale(p_h) + groep +
                            (1|bekken/meetplaats),
                          data = df_clean,
                          family = poisson(link = "log"))

# Samenvatting van het model
summary(glmmtmb_model3)
plot_model(glmmtmb_model2, type = "pred", terms="jaar [all]")
plot_model(glmmtmb_model2, type = "pred", terms="o2_verz [all]", bias_correction = T)

https://cran.r-project.org/web/packages/performance/refman/performance.html#r2 #interessante link voor performance package

# De DHARMa-package werkt ook met glmmTMB-modellen voor validatie.
cat("\n--- Validatie van glmmTMB-model met DHARMa ---\n")
simulationOutput_glmmtmb <- simulateResiduals(fittedModel = glmmtmb_model, n = 2500, plot = T)
testDispersion(simulationOutput_glmmtmb)
testZeroInflation(simulationOutput_glmmtmb)
testUniformity(simulationOutput_glmmtmb)
plot(simulationOutput_glmmtmb, form = ~jaar)

# Plot de residuen tegen de p_h variabele
# Je hoort hier een willekeurige, 'wolkachtige' spreiding van punten te zien
# zonder een duidelijk patroon.
plot(x = df_clean$p_h, y = residuals(simulationOutput_glmmtmb))
abline(h = 0.5, col = "red", lty = 2) # Voeg een referentielijn toe

# U kunt ook de ingebouwde plotfunctie van DHARMa gebruiken, die nog meer informatie geeft
plot(simulationOutput_glmmtmb, form = df_clean$jaar)
