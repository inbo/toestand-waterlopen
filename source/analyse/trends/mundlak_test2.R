library(dplyr)
library(glmmTMB)

df_temporal <- mi_nat_sv_beek %>%
  group_by(meetplaats) %>%
  mutate(
    # Voor Stikstof (n_t)
    n_t_mean = mean(n_t, na.rm = TRUE),         # Ruimtelijk (gemiddelde van de locatie)
    n_t_dev  = n_t - n_t_mean,                  # Temporeel (fluctuatie over de tijd)

    # Voor Fosfor (p_t)
    p_t_mean = mean(p_t, na.rm = TRUE),
    p_t_dev  = p_t - p_t_mean,

    # Voor Zuurstof (o2)
    o2_mean  = mean(o2, na.rm = TRUE),
    o2_dev   = o2 - o2_mean,

    # Voor Geleidbaarheid (ec_20)
    ec_20_mean = mean(ec_20, na.rm = TRUE),
    ec_20_dev  = ec_20 - ec_20_mean,

    # Voor landbouw (intensiteit_combo_afstr)
    lb_mean = mean(intensiteit_combo_afstr, na.rm = TRUE),
    lb_dev  = intensiteit_combo_afstr - lb_mean
  ) %>%
  ungroup() %>%
  # Schaal de variabelen om model-convergentie te helpen
  mutate(across(ends_with("_mean") | ends_with("_dev") | matches("jaar"), scale))

model_echt_temporeel <- glmmTMB(
  data = df_temporal,
  mmif ~
    # Temporele effecten (binnen een locatie)
    n_t_dev + p_t_dev + o2_dev + ec_20_dev + lb_dev +

    # Ruimtelijke effecten (tussen locaties)
    n_t_mean + p_t_mean + o2_mean + ec_20_mean + lb_mean +

    # Overall tijdstrend en covariabelen
    jaar +
    (1 | owl/meetplaats),
  family = ordbeta
)

summary(model_echt_temporeel)


library(sjPlot)
library(ggplot2)

# Plot de effectgroottes
plot_model(model_echt_temporeel,
           type = "est",
           vline.color = "red", # Trekt een rode lijn bij 0 (geen effect)
           sort.est = TRUE,     # Sorteert van meest negatieve naar meest positieve effect
           title = "Invloed van waterkwaliteit op de mmif-score",
           axis.title = "Effectgrootte (verandering in mmif per 1 SD stijging)")

# Plot voor het temporele effect (hoe mmif verandert bij fluctuaties binnen 1 beek)
p_temp <- plot_model(model_echt_temporeel,
                     type = "pred",
                     terms = "ec_20_dev",
                     title = "Temporeel Effect (Binnen een beek)",
                     axis.title = c("Afwijking EC20 t.o.v. eigen gemiddelde (SD)", "mmif"))

# Plot voor het ruimtelijke effect (hoe mmif verschilt tussen beken)
p_ruim <- plot_model(model_echt_temporeel,
                     type = "pred",
                     terms = "ec_20_mean",
                     title = "Ruimtelijk Effect (Tussen beken)",
                     axis.title = c("Gemiddelde EC20 van de beek (SD)", "mmif"))

# Toon de plots (je kunt ze ook samenvoegen met bijv. het pakket 'patchwork')
p_temp
p_ruim
