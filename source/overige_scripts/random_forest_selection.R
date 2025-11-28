library(dplyr)
library(randomForest)
install.packages("devtools") # Devtools is a package which allows to do this.
devtools::install_github("dustinfife/flexplot")
library(flexplot)

# dataset

rf_data <- mi_data %>%
  group_by(meetplaats) %>%
  filter(jaar == max(jaar)) %>%
  ungroup() %>%
  filter(categorie != "Vijver") %>%
  filter(waterlooptype != "Geïsoleerd water") %>%
  filter(waterlichaamcategorie != "meer") %>%
  filter(!meetplaats %in% c("OW113500", "OW12000", "OW179000", "OW536050", "OW669032", "OW690015", "OW917000", "OW981010", "OW981200")) %>% #weglaten punten buiten Vlaanderen
  left_join(watershed_landuse_reclass, by = "meetplaats") %>%
  # left_join(landgebruik_koppeling, by = c("meetplaats", "monsternamedatum")) %>%
  left_join(oever_landuse_reclass, by = "meetplaats") %>%
  left_join(buffer_landuse_reclass, by = "meetplaats") %>%
  st_drop_geometry() %>%
  filter(jaar > 2006) %>%
  select(meetplaats, jaar, mmif, groep, statuut, water_afstroomgebied:landbouw_extensief_buffer, -oppervlakte, -geom) %>%
  # select(meetplaats, jaar, mmif, verharding_afstroomgebied, verharding_oever, verharding_buffer,
  #        hooggroen_afstroomgebied, hooggroen_oever, hooggroen_buffer, groep, statuut) %>%
  na.omit()

# Train het Random Forest model
# We sluiten 'meetplaats' uit van de voorspellers
rf_model <- randomForest(
  mmif ~ . - meetplaats,
  data = rf_data,
  ntree = 500,
  importance = TRUE # Essentieel voor het berekenen van de belangrijkheid
)

# Haal de 'variable importance' scores op
importance_scores <- importance(rf_model) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("variabele") %>%
  arrange(desc(IncNodePurity))

# Toon de belangrijkheid van de variabelen
print(importance_scores)

# Selecteer de top 4 variabelen op basis van de Random Forest-scores
top_vars <- importance_scores$variabele[1:4]

# Maak de formule voor het glmmTMB-model
# Voeg de meetplaats als random effect toe: (1|meetplaats)
formule <- as.formula(
  paste("mmif ~", paste(top_vars, collapse = " + "), " + (1 | meetplaats)")
)

# Train het glmmTMB-model met de geselecteerde variabelen en random effects
glmmtmb_model <- glmmTMB(formule, data = rf_data, family = "gaussian")

# Bekijk de resultaten van het model
summary(glmmtmb_model)


#VSURF test

library(VSURF)
# Voer de VSURF-analyse uit
# Vervang 'y' door de naam van je respons variabele (bijv. mmif)
# En 'x' met een matrix van je landgebruiksvariabelen (zonder meetplaats)
vsurf_resultaat <- VSURF(x = rf_data %>%
                           select(water_afstroomgebied:landbouw_extensief_buffer),
                         y = rf_data$mmif)

vsurf_resultaat <- VSURF(mmif~ . - meetplaats,
                         data = rf_data)
summary(vsurf_resultaat)
# Toon de geselecteerde variabelen
print(vsurf_resultaat)

vsurf_resultaat$varselect.thres
variables <- attr(vsurf_resultaat$terms, "term.labels")
variables[vsurf_resultaat$varselect.thres] #variabelen in volgorde van importance

# Selecteer de uiteindelijke variabelen
variabelen_selectie <- vsurf_resultaat$varselect.pred

# Bouw je glmmTMB model met de geselecteerde variabelen en random effects
formule <- as.formula(paste("mmif ~", paste(variabelen_selectie, collapse = " + "), " + (1 | meetplaats)"))
glmmtmb_model <- glmmTMB(formule, data = rf_data, family = "gaussian")

# Bekijk de resultaten
summary(glmmtmb_model)
