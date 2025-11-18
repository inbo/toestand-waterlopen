
coefs_df <- coefs(sem_resultaat)[,-9]


# Aanname: de objecten fc_lu_data_clean, m5 en coefs_df zijn geladen in uw R-sessie.

library(dplyr)
library(tibble)
library(glmmTMB)

# --- 1. BEREKEN SD VAN DE LATENTE RESPONS (SD(Y_latent)) VOOR M5 ---

# m5 gebruikt family = nbinom1 (Negative Binomial Type 1).
# De observatievariantie (Varm) voor nbinom1 met log-link is NIET een constante,
# maar wordt geschat via de dispersieparameter (phi). Echter, de meest robuuste
# methode voor pSEM-compatibiliteit is het gebruik van de SD van de lineaire predictor (eta).

# Bereken de lineaire predictor (eta) op de log-link schaal
# Gebruik type = "link" om de schattingen op de link-schaal te krijgen
fc_lu_data_clean$eta_pesticiden <- predict(m5, type = "link", se.fit = FALSE)

# SD van de Latente Respons (SD(Y_latent))
SD_Latente_Y_Pest <- sd(fc_lu_data_clean$eta_pesticiden)


# --- 2. BEREKEN DE 6 GESTANDARDISEERDE ESTIMATES ---

# De lijst van predictoren in m5
predictoren_m5 <- c("landbouw_intens_afstr_s", "hooggroen_oever_s", "Neerslag_som_1jaar_s",
                    "Neerslag_som_10dagen_s", "ekc2_waterlichaam_s", "jaar_s")

# DataFrame om de SD(X) waarden te extraheren
data_voor_sd_x <- fc_lu_data_clean %>%
  select(all_of(predictoren_m5))

# Maak een vector om de 6 handmatige estimates op te slaan
manual_std_estimates <- numeric(length(predictoren_m5))

# Loop over de 6 predictoren
for (i in 1:length(predictoren_m5)) {
  predictor_name <- predictoren_m5[i]

  # A) Haal de ongestandaardiseerde estimate (Estimate) op uit coefs_df
  # Filter op de respons 'aantal_pesticiden_met_overschrijding' en de huidige predictor
  beta_fixed <- coefs_df %>%
    filter(Response == "aantal_pesticiden_met_overschrijding", Predictor == predictor_name) %>%
    pull(Estimate)

  # B) Haal de SD(X) van de predictor op
  SD_predictor_X <- sd(data_voor_sd_x[[predictor_name]])

  # C) Bereken de gestandaardiseerde estimate
  beta_std <- beta_fixed * (SD_predictor_X / SD_Latente_Y_Pest)

  manual_std_estimates[i] <- beta_std
}


# --- 3. VUL DE ONTBREKENDE WAARDEN IN coefs_df IN ---

# De rijen voor model m5 beginnen bij rij 37 en eindigen bij rij 42 in de output.
# We vullen de berekende vector in de kolom 'Std.Estimate' van de rijen 37 t/m 42
coefs_df[37:42, "Std.Estimate"] <- manual_std_estimates
coefs_df$Std.Estimate <- as.numeric(coefs_df$Std.Estimate)

# --- 4. CONTROLE OUTPUT ---

cat("\n✅ Succes! De 6 missende Std.Estimates zijn ingevuld.\n\n")
cat("--- Resultaten voor Model M5 (Pesticiden) ---\n")

# Toon de gevulde rijen 37 t/m 42 ter controle
print(coef_df[37:42, ])

cat("\nDe kolom 'Std.Estimate' bevat nu de handmatig berekende waarden.\n")

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
predictoren_m2 <- coef_df$Predictor[rijen_m2]

# DataFrame voor de SD(X) extractie
data_voor_sd_x_m2 <- fc_lu_data_clean %>%
  select(all_of(predictoren_m2))

# Vector om de 11 handmatige estimates op te slaan
manual_std_estimates_m2 <- numeric(length(rijen_m2))


# C) Loop en Bereken de 11 Estimates

for (i in 1:length(rijen_m2)) {
  predictor_name <- predictoren_m2[i]

  # 1. Haal de ongestandaardiseerde estimate op
  beta_fixed <- coef_df$Estimate[rijen_m2[i]]

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


# C) Toon de resultaten voor m2 en m5 ter controle
cat("\n✅ Standardisatie voor Ordbeta (M2) en Nbinom1 (M5) voltooid.\n")
cat("----------------------------------------------------------------------\n")
cat("--- Controle van de ingevulde Std.Estimate waarden (Rijen 8-18) ---\n")
print(coefs_df[c(8:18, 37:42), c("Response", "Predictor", "Estimate", "Std.Estimate")])
cat("----------------------------------------------------------------------\n")
