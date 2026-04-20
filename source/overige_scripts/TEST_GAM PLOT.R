



gam <- glmmTMB(data = mi_nat_sv_beek %>%
                 mutate(ept = ept / 4,
                        bekken = as.factor(bekken),
                        meetplaats = as.factor(meetplaats)),
               formula = mmif ~ 1 + jaar + (1|meetplaats) + (1|bekken),
               family = ordbeta,
               na.action = na.omit)
summary(gam)
plot_model(gam, type = "pred", terms = "jaar [all]", show.data = TRUE)


######### gam

library(mgcv)


gam_data <- mi_nat_sv_beek %>%
  select(mmif, bekken, meetplaats, jaar) %>%
  mutate(bekken = as.factor(bekken),
         meetplaats = as.factor(meetplaats)) %>%
  na.omit

# 1. Transformeer mmif heel licht als er exacte 0 of 1 waarden zijn
# Een veelgebruikte formule: (y * (n-1) + 0.5) / n
n <- nrow(gam_data)
gam_data <- gam_data %>%
  mutate(mmif_beta = (mmif * (n - 1) + 0.5) / n)

# 2. Fit het model met de Beta familie
test_gam_beta <- gam(mmif_beta ~ s(jaar, k = 3) +
                       s(meetplaats, bs = "re") +
                       s(bekken, bs = "re"),
                     data = gam_data,
                     family = betar(link = "logit"), # De sleutel tot succes
                     method = "REML")

# 3. Plotten (let op: de y-as is nu op de 'logit' schaal)
plot(test_gam_beta, select = 1, shade = TRUE, pages = 1, trans = plogis,
     shift = coef(test_gam_beta)[1], seWithMean = TRUE)



# --- 1. Data Voorbereiding (met transformatie voor Beta) ---
n_total <- nrow(data4)

mi_nested <- data4 %>%
  # Eerst de scores naar 0-1 brengen
  mutate(across(c("ept", "swd", "nst", "tax", "mts"), ~ .x / 4)) %>%
  # Transformeren om exacte 0 en 1 te vermijden voor de Beta-familie
  mutate(across(c(mmif, ept, swd, nst, tax, mts),
                ~ (.x * (n_total - 1) + 0.5) / n_total)) %>%
  mutate(subset = case_when(
    statuut %in% c("Natuurlijk", "Sterk Veranderd") & groep == "beek"   ~ "nat_sv_beek",
    statuut %in% c("Natuurlijk", "Sterk Veranderd") & groep == "kempen" ~ "nat_sv_kempen",
    statuut %in% c("Natuurlijk", "Sterk Veranderd") & groep == "polder" ~ "nat_sv_polder",
    statuut %in% c("Natuurlijk", "Sterk Veranderd") & groep == "rivier" ~ "nat_sv_rivier",
    statuut == "Kunstmatig"                                            ~ "kunstmatig",
    type == "RtNt"                                                     ~ "rtnt",
    TRUE                                                               ~ "overig"
  )) %>%
  filter(subset != "overig") %>%
  pivot_longer(cols = c(mmif, tax, swd, nst, mts, ept),
               names_to = "index",
               values_to = "waarde") %>%
  mutate(jaar_num = as.numeric(format(monsternamedatum, "%Y")),
         subset = as.factor(subset),
         index = factor(index, levels = c("mmif", "tax", "swd", "nst", "mts", "ept")),
         meetplaats = as.factor(meetplaats),
         bekken = as.factor(bekken)) %>%
  group_nest(subset, index)

# %>%
#   filter(index == "mmif" & subset == "nat_sv_beek")

# --- 2. Modellen fitten met de Beta-familie ---
mi_models <- mi_nested %>%
  mutate(
    model = map(data, ~ gam(waarde ~ s(jaar_num, k = 3) +
                              s(meetplaats, bs = "re") +
                              s(bekken, bs = "re"),
                            data = .x,
                            family = betar(link = "logit"), # DIT VOORKOMT NEGATIEVE WAARDEN
                            method = "REML")),

    # Gebruik ggpredict en zet terug naar de 0-1 schaal
    # ggpredict doet dit automatisch voor de link-functie

    predictions = map(model, ~ {
      if (is.null(.x)) return(NULL)

      # ggaverage is de sleutel: het middelt over de random effects
      # (meetplaats/bekken) om het populatie-gemiddelde te vinden.
      res <- ggaverage(.x, terms = "jaar_num [all]")

      as.data.frame(res)
    })  )

# --- 3. Uitpakken en Visualiseren ---
mi_trends <- mi_models %>%
  select(subset, index, predictions) %>%
  unnest(predictions)

# Optioneel: als je de deelmaatlatten terug naar 0-4 wilt schalen in de plot:
# mi_trends <- mi_trends %>%
#   mutate(across(c(predicted, conf.low, conf.high), ~ ifelse(index == "mmif", .x, .x * 4)))

plot_mi_trend_subsets <- ggplot(mi_trends, aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = subset), alpha = 0.2) +
  geom_line(aes(color = subset), size = 1) +
  facet_grid(index ~ subset, scales = "free_y") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 1)) + # Forceert de as tussen 0 en 1
  labs(title = "Correcte GAM-trends (Beta-regressie)",
       subtitle = "Indices genormaliseerd naar 0-1 schaal",
       x = "Jaar", y = "Voorspelde waarde (0-1)")

# Sla de plot op als een PDF (vector-formaat, beste voor rapporten) of PNG
ggsave(
  filename = here("output", "figuren", "GAM_trends_waterkwaliteit.pdf"),
  plot = plot_mi_trend_subsets,
  width = 16,        # Breedte in inches
  height = 12,       # Hoogte in inches
  units = "in",
  device = "pdf"     # Je kunt ook "png" gebruiken
)
