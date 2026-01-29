#
# coefs_df <- coefs(sem_resultaat)[,-9]
#

# Aanname: de objecten data_sem_clean, m5 en coefs_df zijn geladen in uw R-sessie.

library(dplyr)
library(tibble)
library(glmmTMB)

#####
# voor m2 model
######

# ----------------------------------------------------------------------
# 3. HANDMATIGE STANDAARDISATIE VOOR M2 (Ordbeta)
# ----------------------------------------------------------------------

# A) Latente Variantie Componenten Bepalen (Ordbeta Logit Link)

# Observatie Variantie (Varm): Voor de ordbeta logit-link is de residuele variantie
# vaak ook vastgesteld op pi^2/3, net als bij de standaard Binomiale logit.

Varm_ordbeta <- (pi^2) / 3

# Willekeurige Effecten Variantie (sigma^2_random)
random_effects_df_m2 <- unlist(VarCorr(m2)) %>% as_tibble()
sigma2_random_m2 <- random_effects_df_m2 # Variantie van meetplaats

# Vaste Effecten Variantie (Var_Fixed)
eta_fixed_part_m2 <- model.matrix(m2) %*% fixef(m2)$cond
Var_Fixed_m2 <- var(eta_fixed_part_m2)

# Totale Latente Variantie & SD
Var_latent_m2 <- Var_Fixed_m2 + sigma2_random_m2 + Varm_ordbeta
SD_Latente_Y_m2 <- sqrt(Var_latent_m2)


# B) Bepaal de predictoren en hun SD's

# Selecteer de rijnummers voor M2 in coef_df (Rijen 8 t/m 18)
rijen_m2 <- 8:18
predictoren_m2 <- coefs_df$Predictor[rijen_m2]

# DataFrame voor de SD(X) extractie
data_voor_sd_x_m2 <- data_sem_clean %>%
  select(all_of(predictoren_m2))

# Vector om de 11 handmatige estimates op te slaan
manual_std_estimates_m2 <- numeric(length(rijen_m2))


# C) Loop en Bereken de 11 Estimates

for (i in 1:length(rijen_m2)) {
  predictor_name <- predictoren_m2[i]

  # 1. Haal de ongestandaardiseerde estimate op
  beta_fixed <- coefs_df$Estimate[rijen_m2[i]]

  # 2. Haal de SD(X) op (moet werken omdat alle predictoren geschaald zijn)
  SD_predictor_X <- sd(data_voor_sd_x_m2[[predictor_name]])

  # 3. Bereken de gestandaardiseerde estimate
  beta_std <- beta_fixed * (SD_predictor_X / SD_Latente_Y_m2)

  manual_std_estimates_m2[i] <- beta_std
}


# ----------------------------------------------------------------------
# 4. VUL COEF_DF IN
# ----------------------------------------------------------------------

# A) Schoon de kolom op (verwijder '-' en zet om naar numeric)
# coefs_df$Std.Estimate <- gsub(" - ", NA, coef_df$Std.Estimate)
# coefs_df$Std.Estimate <- as.numeric(coef_df$Std.Estimate)

# B) Vul de handmatige waarden in de rijen 8 t/m 18
coefs_df[rijen_m2, "Std.Estimate"] <- unlist(manual_std_estimates_m2)
coefs_df$Std.Estimate <- as.numeric(coefs_df$Std.Estimate)


# C) Toon de resultaten voor m2 en m5 ter controle
cat("\n✅ Standardisatie voor Ordbeta (M2) en Nbinom1 (M5) voltooid.\n")
cat("----------------------------------------------------------------------\n")
cat("--- Controle van de ingevulde Std.Estimate waarden (Rijen 8-18) ---\n")
print(coefs_df[c(8:19), c("Response", "Predictor", "Estimate", "Std.Estimate")])
cat("----------------------------------------------------------------------\n")
