library(dplyr)
library(DHARMa)

# 1. Genereer de residuelen voor het volledige model
res <- simulateResiduals(fittedModel = model_gam_kempen)

jaren <- sort(unique(data_model$jaar))

for(j in jaren) {
  # 1. Filter de data voor dit specifieke jaar
  idx <- which(data_model$jaar == j)
  data_jaar <- data_model[idx, ]

  # 2. Maak een DHARMa subset object
  res_subset <- createDHARMa(
    simulatedResponse = res$simulatedResponse[idx, ],
    observedResponse = res$observedResponse[idx],
    fittedPredictedResponse = res$fittedPredictedResponse[idx],
    integerResponse = res$integerResponse
  )

  # 3. Aggregeer de residuelen per meetplaats (voor het geval er >1 meting per jaar is)
  # We groeperen op de unieke ID van de locatie
  res_grouped_jaar <- recalculateResiduals(res_subset, group = data_jaar$meetplaats)

  # 4. Zorg dat de coördinaten uniek zijn en in de juiste volgorde staan
  locaties_jaar <- data_jaar %>%
    group_by(meetplaats) %>%
    summarize(x = dplyr::first(x), y = dplyr::first(y), .groups = "drop") %>%
    arrange(meetplaats)

  cat("\n--- Ruimtelijke Autocorrelatie Test voor jaar:", j, "---\n")

  # 5. Voer de test uit
  test_result <- testSpatialAutocorrelation(
    res_grouped_jaar,
    x = locaties_jaar$x,
    y = locaties_jaar$y,
    plot = FALSE
  )

  # Print een korte samenvatting van het resultaat
  print(test_result)
}
