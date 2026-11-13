library(mgcv)
library(purrr)
library(tidyr)
library(dplyr)
library(ggeffects)
library(ggplot2)
library(parallel)
library(here)
library(stringr) # Voor het splitsen van kolomnamen

# --- 1. Data Voorbereiding ---
fd_subsets_plot <- mi_data_analyse %>%
  mutate(
    subset = case_when(
      statuut %in% c("Natuurlijk", "Sterk Veranderd") & groep == "beek"   ~ "nat_sv_beek",
      statuut %in% c("Natuurlijk", "Sterk Veranderd") & groep == "kempen" ~ "nat_sv_kempen",
      statuut %in% c("Natuurlijk", "Sterk Veranderd") & groep == "polder" ~ "nat_sv_polder",
      statuut %in% c("Natuurlijk", "Sterk Veranderd") & groep == "rivier" ~ "nat_sv_rivier",
      statuut == "Kunstmatig" | groep %in% c("rivier", "beek", "kempen", "polder") ~ "kunstmatig",
      type == "RtNt"                                                      ~ "rtnt",
      TRUE                                                                ~ "overig"
    )
  ) %>%
  left_join(., fd_mi_filtered, by = c("meetplaats", "monsternamedatum")) %>%
  mutate(jaar_num = as.numeric(format(monsternamedatum, "%Y"))) %>%
  filter(jaar_num >= 1989 & jaar_num <= 2023) %>%
  filter(subset != "overig")

# Data pivoten: scheiden in 'metric' (fdisp, fric, feve) en 'trait_subset'
fd_nested <- fd_subsets_plot %>%
  # Selecteer enkel de variabelen die we nodig hebben voor sneller pivoten
  select(meetplaats, monsternamedatum, jaar_num, subset, bekken, owl, vhag,
         starts_with("fdisp_"), starts_with("fric_"), starts_with("feve_")) %>%
  drop_na(fdisp_full) %>% # Of een andere logische check om lege rijen te droppen
  pivot_longer(
    cols = matches("^(fdisp|fric|feve)_(full|habitat|waterkwaliteit)$"),
    names_to = c("metric", "trait_subset"),
    names_sep = "_",
    values_to = "waarde"
  ) %>%
  mutate(
    subset = as.factor(subset),
    metric = factor(metric, levels = c("fdisp", "feve", "fric")),
    trait_subset = factor(trait_subset, levels = c("full", "habitat", "waterkwaliteit")),
    meetplaats = as.factor(meetplaats),
    bekken = as.factor(bekken),
    owl = as.factor(owl)
  ) %>%
  group_nest(subset, metric, trait_subset)

# --- 1.5. Parallel cluster opzetten ---
n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)

# --- 2. Modellen fitten met MGCV (BAM) ---
fd_models <- fd_nested %>%
  mutate(
    model = map(data, ~ bam(waarde ~ s(jaar_num, k = 4) +
                              s(meetplaats, bs = "re") +
                              s(bekken, bs = "re") +
                              s(owl, bs = "re"),
                            family = gaussian(), # Aangepast van quasibinomial naar gaussian
                            data = .x,
                            method = "fREML",
                            discrete = TRUE,
                            nthreads = n_cores)),

    p_val_jaar = map_dbl(model, ~ summary(.x)$s.table["s(jaar_num)", "p-value"]),
    is_significant = factor(ifelse(p_val_jaar < 0.05, "Ja", "Nee"), levels = c("Ja", "Nee")),

    predictions = map(model, ~ as.data.frame(
      ggpredict(.x,
                terms = "jaar_num [all]",
                type = "fixed",
                exclude = c("s(meetplaats)", "s(bekken)", "s(owl)"))
    ))
  )

stopCluster(cl)

# --- 3. Uitpakken en Visualiseren ---
fd_trends <- fd_models %>%
  select(subset, metric, trait_subset, is_significant, predictions) %>%
  unnest(predictions)

plot <- ggplot(fd_trends, aes(x = x, y = predicted)) +
  # Betrouwbaarheidsinterval: vulkleur bepaald door trait_subset, transparantie door significantie
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = trait_subset, alpha = is_significant)) +
  # Trendlijn
  geom_line(aes(color = trait_subset, linetype = is_significant), linewidth = 1) +
  # Raster per metric (rijen) en per waterloopprofiel (kolommen)
  facet_grid(metric ~ subset, scales = "free_y") +
  theme_minimal() +
  labs(title = "Trends in Functionele Diversiteit Indices (1989-2023)",
       subtitle = "GAM fits gecorrigeerd voor meetplaats, bekken en owl\nVolle lijn = significant effect over de tijd (p < 0.05)",
       x = "Jaar", y = "Voorspelde waarde",
       color = "Trait Subset", fill = "Trait Subset") +
  # Vaste kleuren voor de subsets (zoals in de voorbeeldfoto)
  scale_color_manual(values = c("full" = "#F8766D", "habitat" = "#00BA38", "waterkwaliteit" = "#619CFF")) +
  scale_fill_manual(values =  c("full" = "#F8766D", "habitat" = "#00BA38", "waterkwaliteit" = "#619CFF")) +
  scale_linetype_manual(values = c("Ja" = "solid", "Nee" = "dashed"), name = "Trend significant?") +
  scale_alpha_manual(values = c("Ja" = 0.2, "Nee" = 0.05), guide = "none") +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename =  here("output", "figuren", "trend_fd_bam_1989-2023.png"),
  plot = plot,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 300,
  bg = "white"
)
