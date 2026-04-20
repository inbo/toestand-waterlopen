library(mgcv)
library(purrr)
library(tidyr)
library(dplyr)
library(ggeffects)
library(ggplot2)

# --- 1. Data Voorbereiding ---
mi_subsets_plot <- data4 %>%
  mutate(across(c("ept", "swd", "nst", "tax", "mts"), ~ .x / 4)) %>%
  mutate(
    subset = case_when(
      statuut %in% c("Natuurlijk", "Sterk Veranderd") & groep == "beek"   ~ "nat_sv_beek",
      statuut %in% c("Natuurlijk", "Sterk Veranderd") & groep == "kempen" ~ "nat_sv_kempen",
      statuut %in% c("Natuurlijk", "Sterk Veranderd") & groep == "polder" ~ "nat_sv_polder",
      statuut %in% c("Natuurlijk", "Sterk Veranderd") & groep == "rivier" ~ "nat_sv_rivier",
      statuut == "Kunstmatig"                                            ~ "kunstmatig",
      type == "RtNt"                                                     ~ "rtnt",
      TRUE                                                               ~ "overig"
    )
  )

# Nesten voor mgcv
mi_nested <- mi_subsets_plot %>%
  filter(subset != "overig") %>%
  pivot_longer(cols = c(mmif, tax, swd, nst, mts, ept),
               names_to = "index",
               values_to = "waarde") %>%
  mutate(jaar_num = as.numeric(format(monsternamedatum, "%Y")),
         subset = as.factor(subset),
         index = factor(index, levels = c("mmif", "tax", "swd", "nst", "mts", "ept")),
         # mgcv heeft soms moeite met factoren als ze niet expliciet als factor in de data zitten
         meetplaats = as.factor(meetplaats),
         bekken = as.factor(bekken)) %>%
  group_nest(subset, index)

# --- 2. Modellen fitten met MGCV ---
mi_models <- mi_nested %>%
  mutate(
    # Fit model met gam(). k=5 geeft meer ruimte voor bochten dan k=4.
    # method = "REML" is de aanbevolen methode voor betrouwbare smoothing parameters.
    model = map(data, ~ gam(waarde ~ s(jaar_num, k = 3) +
                              s(meetplaats, bs = "re") +
                              s(bekken, bs = "re"),
                            data = .x,
                            method = "REML")),

    # Extraheer trends. ggpredict werkt naadloos met mgcv objecten.
    predictions = map(model, ~ as.data.frame(ggpredict(.x, terms = "jaar_num [all]")))
  )

# --- 3. Uitpakken en Visualiseren ---
mi_trends <- mi_models %>%
  select(subset, index, predictions) %>%
  unnest(predictions)

ggplot(mi_trends, aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = subset), alpha = 0.2) +
  geom_line(aes(color = subset), size = 1) +
  # Gebruik facet_grid(index ~ subset) zoals in je laatste voorbeeld
  facet_grid(index ~ subset, scales = "free_y") +
  theme_minimal() +
  labs(title = "MGCV GAM-trends per Subset en Index",
       subtitle = "Model: waarde ~ s(jaar, k=5) + s(meetplaats, bs='re') + s(bekken, bs='re')",
       x = "Jaar", y = "Voorspelde waarde",
       color = "Subset", fill = "Subset") +
  scale_y_continuous(limits = c(0, 1)) +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))
