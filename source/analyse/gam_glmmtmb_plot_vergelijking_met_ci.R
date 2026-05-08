library(dplyr)
library(tidyr)
library(ggplot2)
# 1. Definieer de variabelen die je wilt vergelijken
fixed_vars <- c("ec_20_log", "o2_s", "spei6_s", "jaar_s",
                "p_t_log", "p_h_s", "overstorten_blootstelling_index_log")

fixed_vars <- c("ec_20_log", "o2_s", "spei6_s", "jaar_s", "intensiteit_combo_afstr_s", "t_s", "n_t_log", "p_t_log", "verharding_afstr_s")

all_comparisons <- list()

for (v in fixed_vars) {
  # 1. Grid maken
  grid <- data.frame(val = seq(min(data_model[[v]], na.rm = TRUE),
                               max(data_model[[v]], na.rm = TRUE),
                               length.out = 100))

  # Maak een kopie met de echte naam voor de predictie-functies
  grid[[v]] <- grid$val

  # Vul andere variabelen aan
  for (ov in setdiff(fixed_vars, v)) {
    grid[[ov]] <- ifelse(grepl("_s$", ov), 0, mean(data_model[[ov]], na.rm = TRUE))
  }

  # Dummy data voor de modellen
  grid$meetplaats <- data_model$meetplaats[1]
  grid$bekken     <- data_model$bekken[1]
  grid$owl     <- data_model$owl[1]
  grid$x <- data_model$x[1]; grid$y <- data_model$y[1]; grid$ID <- factor(1)

  # 2. GAM Predicties
  preds_gam <- predict(model_gam, newdata = grid,
                       exclude = c("s(meetplaats)", "s(x,y)", "s(owl)", "s(bekken)"),
                       type = "response", se.fit = TRUE)
  grid$gam_fit <- as.numeric(preds_gam$fit)
  grid$gam_lwr <- as.numeric(preds_gam$fit - (1.96 * preds_gam$se.fit))
  grid$gam_upr <- as.numeric(preds_gam$fit + (1.96 * preds_gam$se.fit))

  # 3. Ordbeta Predicties (via link schaal voor betere CI)
  preds_ob <- predict(model, newdata = grid,
                      re.form = NA, type = "link", se.fit = TRUE)
  grid$ob_fit <- plogis(preds_ob$fit)
  grid$ob_lwr <- plogis(preds_ob$fit - (1.96 * preds_ob$se.fit))
  grid$ob_upr <- plogis(preds_ob$fit + (1.96 * preds_ob$se.fit))

  # 4. Opslaan: We houden nu 'val' als vaste naam voor de x-as
  all_comparisons[[v]] <- grid %>%
    mutate(variable = v) %>%
    select(val, variable, starts_with("gam_"), starts_with("ob_"))
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
  scale_color_manual(values = c("gam" = "steelblue", "ob" = "firebrick"),
                     labels = c("GAM", "Ordbeta")) +
  scale_fill_manual(values = c("gam" = "steelblue", "ob" = "firebrick"),
                    labels = c("GAM", "Ordbeta")) +
  theme_minimal() +
  labs(title = "Vergelijking Model-effecten (95% CI)",
       subtitle = "Rood = Ordbeta (SEM aanname), Blauw = GAM (Flexibel)",
       x = "Waarde van de predictor",
       y = "Voorspelde MMIF (0-1)")

##########gam psem kempen ##########"


# Gebruik een interactie in plaats van een 2D spline voor de SEM
model_gam <- gam(
  mmif ~ ec_20_log + overstorten_blootstelling_index_log + p_h_s +
    jaar_s + p_t_log + o2_s + spei6_s +
    s(meetplaats, bs = "re") +
    s(x, bs = "tp") + s(y, bs = "tp"), # Splits de coördinaten in twee 1D splines
  data = data_model,
  family = gaussian() # Let op: mmif is 0-1, gaussian is gewaagd, maar voor psem vaker stabieler
)

ptot <- mmif_sem_nat_sv_kempen[[2]]
ptot_glmm <- glmmTMB(p_t_log ~ intensiteit_combo_afstr_s + verharding_afstr_s + verharding_oever_s +
  jaar_s + (1 | meetplaats) + (1 | bekken) + t_s,
  data = data_model)
summary(ptot)
ptot_gam <- gam(p_t_log ~ intensiteit_combo_afstr_s + verharding_afstr_s + verharding_oever_s +
                       jaar_s + t_s + s(meetplaats, bs = "re") +
                  s(x, y, bs = "gp", k = 100),
                     data = data_model)
psem_mix <- psem(model_gam, ptot_gam, data = data_model)
summary(psem_mix)
dredge_data



