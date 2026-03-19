library(dplyr)
library(tibble)
library(glmmTMB)

# --- 1. Detectie van modellen die handmatige standardisatie nodig hebben ---
# We zoeken naar rijen waar Std.Estimate "-" of NA is
missing_indices <- which(coefs_missing$Std.Estimate == "-" | is.na(coefs_missing$Std.Estimate))

if (length(missing_indices) > 0) {
  message("⚠️ ", length(missing_indices), " paden gevonden zonder Std.Estimate. Starten flexibele berekening...")

  # Haal alle unieke response variabelen op die een estimate missen
  responses_missing <- unique(coefs_missing$Response[missing_indices])

  for (resp_name in responses_missing) {
    message("👉 Verwerken model voor: ", resp_name)

    # 1. Zoek het bijbehorende model-object in de Global Environment
    # We zoeken in de lijst van modellen die in psem() zitten
    # Als je de modellen los hebt staan (m1, m2...), zoeken we op basis van de formule
    model_obj <- NULL

    # Zoek door alle objecten in de environment naar het glmmTMB model met de juiste response
    # 1. Zoek het bijbehorende model-object DIRECT in het psem object
    model_obj <- NULL

    # We lopen door de lijst van modellen die in je sem_resultaat zitten
    for (i in 1:length(sem_resultaat)) {
      obj <- sem_resultaat[[i]]

      # Check of het een model is (en geen data of andere info)
      if (inherits(obj, c("glmmTMB", "glm", "lm", "lmerMod"))) {
        current_resp <- all.vars(formula(obj))[1]

        if (current_resp == resp_name) {
          model_obj <- obj
          break
        }
      }
    }

    if (is.null(model_obj)) {
      warning("Kon geen glmmTMB model vinden voor response: ", resp_name)
      next
    }

    # --- NIEUW: Automatisch de juiste data ophalen uit het model ---
    # glmmTMB slaat de gebruikte data op in 'model_obj$frame'
    model_data <- model_obj$frame

    # 2. Bereken de Latente Variantie Componenten op basis van de familie
    family_info <- family(model_obj)

    # Residuele variantie (Varm)
    Varm <- case_when(
      family_info$family == "ordbeta" ~ (pi^2) / 3,
      family_info$family == "nbinom1" ~ (pi^2) / 3, # Of specifieke nbinom variantie
      family_info$link == "logit"     ~ (pi^2) / 3,
      TRUE                            ~ 0 # Voor gaussian vult piecewiseSEM het meestal zelf al in
    )

    # Willekeurige effecten variantie
    vc <- VarCorr(model_obj)

    # Gebruik de specifieke glmmTMB methode om naar een data.frame te dwingen
    # Als de standaard as.data.frame faalt, gebruiken we de lme4-stijl extractie
    vc_df <- tryCatch({
      as.data.frame(vc)
    }, error = function(e) {
      # Fallback: handmatige extractie uit de glmmTMB lijst
      # We tellen de varianties van de 'cond' (conditional) model component op
      do.call(rbind, lapply(names(vc$cond), function(grp) {
        data.frame(grp = grp, vcov = attr(vc$cond[[grp]], "stddev")^2)
      }))
    })

    # Bereken de som van de varianties (sigma2_random)
    sigma2_random <- sum(vc_df$vcov, na.rm = TRUE)

    # Vaste effecten variantie (gebaseerd op de conditional fixef)
    fe <- fixef(model_obj)$cond
    mm <- model.matrix(model_obj)

    # Zorg dat we alleen variabelen gebruiken die ook in de fixef zitten (voorkomt mismatch bij NA's)
    mm <- mm[, names(fe), drop = FALSE]
    eta_fixed_part <- mm %*% fe
    Var_Fixed <- var(as.numeric(eta_fixed_part))

    # Totale Latente SD
    SD_Latente_Y <- sqrt(Var_Fixed + sigma2_random + Varm)

    # 3. Update de rijen in coefs_missing voor deze specifieke response
    curr_rows <- which(coefs_missing$Response == resp_name & (coefs_missing$Std.Estimate == "-" | is.na(coefs_missing$Std.Estimate)))

    for (idx in curr_rows) {
      predictor_name <- coefs_missing$Predictor[idx]
      beta_fixed <- coefs_missing$Estimate[idx]

      # Haal SD(X) uit de data_sem_clean
      if (predictor_name %in% names(model_data)) {
        SD_predictor_X <- sd(model_data[[predictor_name]], na.rm = TRUE)

        # De Berekening
        beta_std <- beta_fixed * (SD_predictor_X / SD_Latente_Y)
        coefs_missing[idx, "Std.Estimate"] <- as.numeric(beta_std)
      }
    }
  }

  # Finale opschoning: dwing de hele kolom naar numeric
  coefs_missing$Std.Estimate <- as.numeric(coefs_missing$Std.Estimate)
  message("✅ Alle ontbrekende estimates zijn berekend en ingevuld.")

} else {
  message("✅ Geen ontbrekende estimates gevonden.")
}
