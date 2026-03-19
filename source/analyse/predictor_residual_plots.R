library(DHARMa)

# 1. Simuleer de residuen (als je dat nog niet gedaan had)
# Tip: voeg set.seed() toe voor reproduceerbaarheid
set.seed(123)
sim_res <- simulateResiduals(fittedModel = m2) # Gebruik hier je definitieve model


# 2. Maak een lijstje van de namen van je voorspellers (exact zoals in je dataset)
# 1. Haal letterlijk álle variabelen uit de formule van je model
alle_variabelen <- all.vars(formula(m2))

# 2. Filter de afhankelijke (Y) en de random effect variabele eruit
mijn_x_variabelen <- setdiff(alle_variabelen, c("mmif", "meetplaats"))

model_data <- data_sem_clean

# 3. Zet je plot-venster in een 2x2 grid, zodat ze allemaal mooi naast elkaar passen
par(mfrow = c(2, 2))

# 4. De Loop: Plot de residuen tegen elke X-variabele
for (var in mijn_x_variabelen) {
  # Haal de kolom met data op uit je dataset
  predictor_data <- model_data[[var]]

  # Plot! DHARMa berekent hier automatisch weer die kwantiel-lijnen voor je
  plotResiduals(sim_res,
                form = predictor_data,
                xlab = var)
}

# 5. Reset je plot-venster terug naar 1x1 voor je volgende plots
par(mfrow = c(1, 1))
