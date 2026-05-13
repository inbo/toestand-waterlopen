# 0. Voorbereiding
# model_glmmtmb <- glmmTMB(...)
# model_ordbetareg <- ordbetareg(...)

fixed_vars <- c("ec_20_log", "o2_s", "spei6_s", "jaar_s",
                "p_t_log", "p_h_s", "overstorten_blootstelling_index_log")

all_comparisons <- list()

for (v in fixed_vars) {
  # 1. Grid maken
  grid <- data.frame(val = seq(min(data_model[[v]], na.rm = TRUE),
                               max(data_model[[v]], na.rm = TRUE),
                               length.out = 100))
  grid[[v]] <- grid$val

  # Gemiddelden voor overige variabelen
  for (ov in setdiff(fixed_vars, v)) {
    grid[[ov]] <- ifelse(grepl("_s$", ov), 0, mean(data_model[[ov]], na.rm = TRUE))
  }

  # Dummy data voor random effects
  grid$meetplaats <- data_model$meetplaats[1]
  grid$owl        <- data_model$owl[1]

  # 2. glmmTMB Predicties (Frequentistisch)
  # Gebruik type = "link" om CI's te berekenen en daarna te transformeren
  preds_tmb <- predict(model, newdata = grid,
                       re.form = NA, type = "link", se.fit = TRUE)

  grid$tmb_fit <- plogis(preds_tmb$fit)
  grid$tmb_lwr <- plogis(preds_tmb$fit - (1.96 * preds_tmb$se.fit))
  grid$tmb_upr <- plogis(preds_tmb$fit + (1.96 * preds_tmb$se.fit))

  # 3. ordbetareg Predicties (Bayesiaans)
  # Gebruik 'fitted' in plaats van 'predict' om de expected value (Credible Intervals)
  # te krijgen, vergelijkbaar met de Confidence Intervals van glmmTMB.
  preds_brs <- fitted(model_ordbetareg, newdata = grid,
                      re_formula = NA, probs = c(0.025, 0.975))

  grid$brs_fit <- preds_brs[, "Estimate"]
  grid$brs_lwr <- preds_brs[, "Q2.5"]
  grid$brs_upr <- preds_brs[, "Q97.5"]

  # 4. Opslaan
  all_comparisons[[v]] <- grid %>%
    mutate(variable = v) %>%
    select(val, variable, starts_with("tmb_"), starts_with("brs_"))
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
  scale_color_manual(values = c("tmb" = "darkorange", "brs" = "purple"),
                     labels = c("glmmTMB (ML)", "ordbetareg (Bayesian)")) +
  scale_fill_manual(values = c("tmb" = "darkorange", "brs" = "purple"),
                    labels = c("glmmTMB (ML)", "ordbetareg (Bayesian)")) +
  theme_minimal() +
  labs(title = "Vergelijking glmmTMB vs ordbetareg",
       subtitle = "Beide modellen gebruiken de Ordered Beta distributie",
       x = "Waarde van de predictor",
       y = "Voorspelde MMIF")
