# === VEREISTE LIBRARIES ===
library(dplyr)
library(tibble)
library(glmmTMB)

# Aanname: de objecten fc_lu_data_clean, m2, m4, m5 en coefs_df zijn geladen.

# ----------------------------------------------------------------------
## I. STANDAARDISATIE VOOR M4 (Tweedie) - Respons: o2
# ----------------------------------------------------------------------

# 1. Bepaal de SD van de Latente Respons (SD(Y_latent))
# Tweedie gebruikt een flexibele link, maar de SD van de lineaire predictor
# (eta) op de link-schaal is de meest robuuste benadering.
fc_lu_data_clean$eta_o2 <- predict(m4, type = "link", se.fit = FALSE)
SD_Latente_Y_o2 <- sd(fc_lu_data_clean$eta_o2)

# 2. Bepaal de predictoren en hun SD's
rijen_m4 <- 28:38
# We filteren het intercept, want dat wordt niet gestandaardiseerd.
predictoren_m4 <- coefs_df$Predictor[rijen_m4]
predictoren_m4 <- predictoren_m4[predictoren_m4 != "(Intercept)"]

data_voor_sd_x_m4 <- fc_lu_data_clean %>%
  select(all_of(predictoren_m4))

# 3. Bereken en vul de Std.Estimates in
manual_std_estimates_m4 <- numeric(length(predictoren_m4))

for (i in 1:length(predictoren_m4)) {
  predictor_name <- predictoren_m4[i]

  # Haal de ongestandaardiseerde estimate (beta_fixed) op
  beta_fixed <- coefs_df %>%
    filter(Response == "o2", Predictor == predictor_name) %>%
    pull(Estimate)

  # Haal de SD(X) van de predictor op
  SD_predictor_X <- sd(data_voor_sd_x_m4[[predictor_name]])

  # Bereken de gestandaardiseerde estimate
  beta_std <- beta_fixed * (SD_predictor_X / SD_Latente_Y_o2)
  manual_std_estimates_m4[i] <- beta_std
}

# Vul de resultaten in coefs_df (rijen 28 t/m 38, exclusief Intercept)
rijen_te_vullen_m4 <- coefs_df %>%
  rownames_to_column(var = "RowNumber") %>%
  filter(Response == "o2", Predictor %in% predictoren_m4) %>%
  pull(RowNumber) %>%
  as.numeric()

coefs_df[rijen_te_vullen_m4, "Std.Estimate"] <- manual_std_estimates_m4


# ----------------------------------------------------------------------
## II. STANDAARDISATIE VOOR M5 (Nbinom1) - Respons: aantal_pesticiden_met_overschrijding
# ----------------------------------------------------------------------

# 1. Bepaal de SD van de Latente Respons (SD(Y_latent))
# Methode: SD van de lineaire predictor (eta) op de link-schaal ("log" link)
fc_lu_data_clean$eta_pesticiden <- predict(m5, type = "link", se.fit = FALSE)
SD_Latente_Y_Pest <- sd(fc_lu_data_clean$eta_pesticiden)

# 2. Bepaal de predictoren en hun SD's
rijen_m5 <- 39:44 # Aangepast op basis van uw eerdere script.
predictoren_m5 <- coefs_df$Predictor[rijen_m5]
predictoren_m5 <- predictoren_m5[predictoren_m5 != "(Intercept)"]

data_voor_sd_x_m5 <- fc_lu_data_clean %>%
  select(all_of(predictoren_m5))

# 3. Bereken en vul de Std.Estimates in
manual_std_estimates_m5 <- numeric(length(predictoren_m5))

for (i in 1:length(predictoren_m5)) {
  predictor_name <- predictoren_m5[i]

  beta_fixed <- coefs_df %>%
    filter(Response == "aantal_pesticiden_met_overschrijding", Predictor == predictor_name) %>%
    pull(Estimate)

  SD_predictor_X <- sd(data_voor_sd_x_m5[[predictor_name]])

  beta_std <- beta_fixed * (SD_predictor_X / SD_Latente_Y_Pest)
  manual_std_estimates_m5[i] <- beta_std
}

# Vul de resultaten in coefs_df
rijen_te_vullen_m5 <- coefs_df %>%
  rownames_to_column(var = "RowNumber") %>%
  filter(Response == "aantal_pesticiden_met_overschrijding", Predictor %in% predictoren_m5) %>%
  pull(RowNumber) %>%
  as.numeric()

coefs_df[rijen_te_vullen_m5, "Std.Estimate"] <- manual_std_estimates_m5


# ----------------------------------------------------------------------
## III. STANDAARDISATIE VOOR M2 (Ordbeta) - Latente Variantie Componenten
# ----------------------------------------------------------------------

# 1. Bepaal de SD van de Latente Respons (SD(Y_latent))
# Deze methode telt de varianties van de vaste effecten, willekeurige effecten en residuele variantie op.

# Residuele Variantie (Varm): Voor de Ordbeta logit-link is deze vastgesteld op pi^2/3.
Varm_ordbeta <- (pi^2) / 3

# Willekeurige Effecten Variantie (sigma^2_random)
random_effects_df_m2 <- unlist(VarCorr(m2)) %>% as_tibble()
sigma2_random_m2 <- random_effects_df_m2 # Variantie van meetplaats

# Vaste Effecten Variantie (Var_Fixed): Variantie van de lineaire predictor zonder willekeurige effecten.
# Let op: dit gebruikt de schattingen van M2.
eta_fixed_part_m2 <- model.matrix(m2) %*% fixef(m2)$cond
Var_Fixed_m2 <- var(eta_fixed_part_m2)

# Totale Latente Variantie & SD
Var_latent_m2 <- Var_Fixed_m2 + sigma2_random_m2 + Varm_ordbeta
SD_Latente_Y_m2 <- sqrt(Var_latent_m2)


# 2. Bepaal de predictoren en hun SD's
rijen_m2 <- 8:19
# We gebruiken de predictoren uit de rijen om de juiste SD(X) te pakken
predictoren_m2_all <- coefs_df$Predictor[rijen_m2]
predictoren_m2 <- predictoren_m2_all[!predictoren_m2_all %in% c("(Intercept)", "Threshold")] # Haal intercept en Thresholds eruit

data_voor_sd_x_m2 <- fc_lu_data_clean %>%
  select(all_of(predictoren_m2))

# 3. Bereken en vul de Std.Estimates in
manual_std_estimates_m2 <- numeric(length(predictoren_m2))

# We moeten de juiste rijnummers van de coëfficiënten selecteren in de loop
rijen_te_vullen_m2 <- coefs_df %>%
  rownames_to_column(var = "RowNumber") %>%
  filter(Predictor %in% predictoren_m2, RowNumber %in% rijen_m2) %>%
  pull(RowNumber) %>%
  as.numeric()

for (i in 1:length(predictoren_m2)) {
  predictor_name <- predictoren_m2[i]

  # 1. Haal de ongestandaardiseerde estimate op (gebruik de rij uit rijen_te_vullen_m2)
  rij_index <- rijen_te_vullen_m2[i]
  beta_fixed <- coefs_df$Estimate[rij_index]

  # 2. Haal de SD(X) op
  SD_predictor_X <- sd(data_voor_sd_x_m2[[predictor_name]])

  # 3. Bereken de gestandaardiseerde estimate
  beta_std <- beta_fixed * (SD_predictor_X / SD_Latente_Y_m2)
  manual_std_estimates_m2[i] <- beta_std
}

# Vul de handmatige waarden in
coefs_df[rijen_te_vullen_m2, "Std.Estimate"] <- unlist(manual_std_estimates_m2)
coefs_df$Std.Estimate <- as.numeric(coefs_df$Std.Estimate)

# ----------------------------------------------------------------------
## IV. CONTROLE OUTPUT
# ----------------------------------------------------------------------

cat("\n✅ Standardisatie voltooid voor M4 (Tweedie), M5 (Nbinom1) en M2 (Ordbeta).\n")
cat("----------------------------------------------------------------------\n")
cat("--- Controle van de ingevulde Std.Estimate waarden (Enkele Rijen) ---\n")
print(coefs_df[c(8:19, 28:38, 39:44), c("Response", "Predictor", "Estimate", "Std.Estimate")])
cat("----------------------------------------------------------------------\n")
