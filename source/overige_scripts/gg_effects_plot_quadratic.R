library(ggeffects)
library(ggplot2)

test_data <- mi_nat_sv_data %>%
mutate(o2_scaled = scale(o2),
       o2_scaled2 = o2_scaled^2,
       jaar_scaled = scale(jaar),
       jaar_scaled2 = jaar_scaled^2
       )


voorbeeld_model2 <- glmmTMB(cbind(mmif_20, 20 - mmif_20) ~  jaar_scaled + jaar_scaled2 +
                               + o2_scaled + o2_scaled2 + groep +
                              (1|bekken) + (1 | meetplaats),
                            data = test_data,
                            family = binomial(link = "logit"),
                            na.action = na.omit)


# Genereer voorspelde data voor het effect van 'jaar'
predicted_data <- ggpredict(
  voorbeeld_model2,
  terms = c("jaar_scaled2 [all]"),
  type = "fixed", # Gebruik fixed effects
  bias_correction = TRUE
  )

# Optioneel: converteer de voorspelde score terug naar de originele schaal
# Omdat de voorspelde y-waarden in de 0-1 schaal van de binomiale link zijn.
predicted_data$predicted_original <- predicted_data$predicted * 20

p <- ggplot(predicted_data, aes(x = x, y = predicted)) +
  geom_line(aes(color = group)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  # geom_point(data = mi_nat_sv_data, aes(x = jaar, y = mmif)) +
  labs(
    title = "Effect van Jaar op MMIF-score",
    x = "Jaar",
    y = "Voorspelde MMIF-score",
    color = "Groep",
    fill = "Groep"
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0.4, 0.75)) # Stel de y-as in op de originele schaal

print(p)
