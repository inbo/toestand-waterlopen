library(mgcv)
library(purrr)
library(tidyr)
library(dplyr)
library(ggeffects)
library(ggplot2)
library(parallel)
library(here) # Toegevoegd omdat here() onderaan wordt gebruikt

source("source/analyse/mi_datasets_prep.R")

# --- 1. Data Voorbereiding ---
mi_subsets_plot <- mi_data_analyse %>%
  drop_na(mmif) %>%
  filter(if_all(c("ept", "swd", "nst", "tax", "mts"), ~ .x %% 1 == 0)) %>%
  mutate(across(c("ept", "swd", "nst", "tax", "mts"), ~ .x / 4)) %>%
  mutate(
    subset = case_when(
      statuut %in% c("Natuurlijk", "Sterk Veranderd") & groep == "beek"   ~ "nat_sv_beek",
      statuut %in% c("Natuurlijk", "Sterk Veranderd") & groep == "kempen" ~ "nat_sv_kempen",
      statuut %in% c("Natuurlijk", "Sterk Veranderd") & groep == "polder" ~ "nat_sv_polder",
      statuut %in% c("Natuurlijk", "Sterk Veranderd") & groep == "rivier" ~ "nat_sv_rivier",
      statuut == "Kunstmatig" | groep %in% c("rivier", "beek", "kempen", "polder")                                        ~ "kunstmatig",
      type == "RtNt"                                                      ~ "rtnt",
      TRUE                                                                ~ "overig"
    )
  ) %>%
  left_join(., fd_mi_filtered,
            by = c("meetplaats", "monsternamedatum")) %>%
  filter(jaar > 2006) %>%
  # --- NIEUWE STAP: Filter meetplaatsen met >= 5 metingen ---
  group_by(meetplaats) %>%
  filter(n_distinct(monsternamedatum) >= 5) %>%
  ungroup()
# ----------------------------------------------------------

# Nesten voor mgcv
mi_nested <- mi_subsets_plot %>%
  filter(subset != "overig") %>%
  pivot_longer(cols = c(mmif, tax, swd, nst, mts, ept),
               names_to = "index",
               values_to = "waarde") %>%
  mutate(jaar_num = as.numeric(format(monsternamedatum, "%Y")),
         subset = as.factor(subset),
         index = factor(index, levels = c("mmif", "tax", "swd", "nst", "mts", "ept")),
         meetplaats = as.factor(meetplaats),
         bekken = as.factor(bekken),
         owl = as.factor(owl),
         vhag = as.factor(vhag),
         maand = factor(lubridate::month(monsternamedatum)),
         maand_num = lubridate::month(monsternamedatum)) %>%
  group_nest(subset, index)

# --- 1.5. Parallel cluster opzetten ---
n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)

# --- 2. Modellen fitten met MGCV (BAM) ---
mi_models <- mi_nested %>%
  mutate(
    model = map(data, ~ bam(waarde ~ s(jaar_num, k = 4) +
                              s(meetplaats, bs = "re") +
                              s(bekken, bs = "re") +
                              s(owl, bs = "re"),
                            family = quasibinomial,
                            data = .x,
                            method = "fREML",
                            discrete = TRUE,
                            nthreads = n_cores)),

    # Extraheer de p-waarde voor de jaar-trend uit de summary table
    p_val_jaar = map_dbl(model, ~ summary(.x)$s.table["s(jaar_num)", "p-value"]),

    # Bepaal of het significant is (p < 0.05)
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
mi_trends <- mi_models %>%
  select(subset, index, is_significant, predictions) %>%
  unnest(predictions)

ggplot(mi_trends, aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = subset, alpha = is_significant)) +
  geom_line(aes(color = subset, linetype = is_significant), linewidth = 1) +
  geom_hline(yintercept = 0.6, linetype = "dashed") +
  facet_grid(index ~ subset, scales = "free_y") +
  theme_minimal() +
  labs(title = "MGCV BAM-trends per Subset en Index",
       subtitle = "Volle lijn = significant effect over de tijd (p < 0.05)",
       x = "Jaar", y = "Voorspelde waarde") +
  scale_y_continuous(limits = c(0, 1)) +
  scale_linetype_manual(values = c("Ja" = "solid", "Nee" = "dashed"), name = "Trend significant?") +
  scale_alpha_manual(values = c("Ja" = 0.2, "Nee" = 0.05), guide = "none") +
  guides(color = "none", fill = "none") +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename =  here("output", "figuren", "trend_mi_bam_2007-2023.png"),
  plot = last_plot(),
  width = 40,
  height = 20,
  units = "cm",
  dpi = 300,
  bg = "white"
)
