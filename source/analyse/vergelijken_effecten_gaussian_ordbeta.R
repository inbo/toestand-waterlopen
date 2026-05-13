# 0. Instellingen
fixed_vars <- c("ec_20_log", "o2_s", "spei6_s", "jaar_s",
                "p_t_log", "p_h_s", "overstorten_blootstelling_index_log")

# Hier je modellen definiëren (zorg dat deze geladen zijn)
# model_gaussian <- glmmTMB(mmif ~ ..., family = gaussian, data = data_model)
# model_ordbeta  <- model # Je bestaande ordbeta model

all_comparisons <- list()

for (v in fixed_vars) {
  # 1. Grid maken voor de huidige variabele v
  grid <- data.frame(val = seq(min(data_model[[v]], na.rm = TRUE),
                               max(data_model[[v]], na.rm = TRUE),
                               length.out = 100))

  # Maak de kolom aan met de originele naam voor de predictie-functies
  grid[[v]] <- grid$val

  # Vul andere fixed predictors aan (gemiddelde of 0 voor geschaalde variabelen)
  for (ov in setdiff(fixed_vars, v)) {
    grid[[ov]] <- ifelse(grepl("_s$", ov), 0, mean(data_model[[ov]], na.rm = TRUE))
  }

  # Dummy data voor random effects (glmmTMB heeft deze nodig in de dataframe,
  # maar we negeren ze via re.form = NA)
  grid$meetplaats <- data_model$meetplaats[1]
  grid$bekken     <- data_model$bekken[1]
  grid$owl        <- data_model$owl[1]
  grid$ID         <- factor(1)
  # Voor ruimtelijke effecten (indien aanwezig in glmmTMB model)
  if("x" %in% names(data_model)) grid$x <- data_model$x[1]
  if("y" %in% names(data_model)) grid$y <- data_model$y[1]

  # 2. Gaussiaans GLMM Predicties (glmmTMB)
  # We gebruiken re.form = NA om enkel de fixed effects te tonen
  preds_gaus <- predict(model_gaussian, newdata = grid,
                        re.form = NA, se.fit = TRUE)

  grid$gaus_fit <- as.numeric(preds_gaus$fit)
  grid$gaus_lwr <- as.numeric(preds_gaus$fit - (1.96 * preds_gaus$se.fit))
  grid$gaus_upr <- as.numeric(preds_gaus$fit + (1.96 * preds_gaus$se.fit))

  # 3. Ordbeta Predicties (via link schaal voor betere CI)
  # Hier gaan we ervan uit dat 'model' je ordbeta model is
  preds_ob <- predict(model, newdata = grid,
                      re.form = NA, type = "link", se.fit = TRUE)

  grid$ob_fit <- plogis(preds_ob$fit)
  grid$ob_lwr <- plogis(preds_ob$fit - (1.96 * preds_ob$se.fit))
  grid$ob_upr <- plogis(preds_ob$fit + (1.96 * preds_ob$se.fit))

  # 4. Opslaan
  all_comparisons[[v]] <- grid %>%
    mutate(variable = v) %>%
    select(val, variable, starts_with("gaus_"), starts_with("ob_"))
}

# 5. Samenvoegen en Plotten
df_long <- bind_rows(all_comparisons) %>%
  pivot_longer(cols = -c(val, variable),
               names_to = c("Model", ".value"),
               names_sep = "_")

ggplot(df_long, aes(x = val, y = fit, color = Model, fill = Model)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1) +
  facet_wrap(~variable, scales = "free_x") +
  scale_color_manual(values = c("gaus" = "steelblue", "ob" = "firebrick"),
                     labels = c("Gaussian GLMM", "Ordbeta")) +
  scale_fill_manual(values = c("gaus" = "steelblue", "ob" = "firebrick"),
                    labels = c("Gaussian GLMM", "Ordbeta")) +
  theme_minimal() +
  labs(title = "Vergelijking Model-effecten (95% CI)",
       subtitle = "Rood = Ordbeta (Logit link), Blauw = Gaussian (Identity link)",
       x = "Waarde van de predictor",
       y = "Voorspelde MMIF (0-1)")
