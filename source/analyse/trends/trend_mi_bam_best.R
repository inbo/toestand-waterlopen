library(mgcv)
library(purrr)
library(tidyr)
library(dplyr)
library(ggeffects)
library(ggplot2)
library(parallel)

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
      statuut == "Kunstmatig" & groep %in% c("rivier", "beek", "kempen", "polder")                                            ~ "kunstmatig", # aanpssen zodat enkel waterlopen zijn en geen zout!
      type == "RtNt"                                                      ~ "rtnt",
      TRUE                                                                ~ "overig"
    )
  ) %>%
  left_join(., fd_mi_filtered,
            by = c("meetplaats", "monsternamedatum"))

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

# --- 2. Modellen fitten met MGCV (BAM) ---
mi_models <- mi_nested %>%
  # filter(index %in% c("ept")) %>%
  # filter(subset %in% c("nat_sv_beek", "nat_sv_kempen")) %>%
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
                type = "fixed", # Of gebruik condition = c(...) afhankelijk van je ggeffects versie
                exclude = c("s(meetplaats)", "s(bekken)", "s(owl)"))
    ))
  )


# --- 3. Uitpakken en Visualiseren ---
mi_trends <- mi_models %>%
  select(subset, index, is_significant, predictions) %>%
  unnest(predictions)

mi_trends <- mi_trends %>%
  mutate(subset = fct_recode(subset,
                             "Kunstmatig" = "kunstmatig",
                             "Beken" = "nat_sv_beek",
                             "Kempense beken" = "nat_sv_kempen",
                             "Rivieren" = "nat_sv_rivier",
                             "Polderlopen" = "nat_sv_polder",
                             "Bovenlopen" = "rtnt"
  ))

ggplot(mi_trends, aes(x = x, y = predicted)) +
  # We maken het betrouwbaarheidsinterval transparanter als het niet significant is
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = subset, alpha = is_significant)) +
  # Gebruik linetype voor significantie
  geom_line(aes(color = subset, linetype = is_significant), linewidth = 1) +
  geom_hline(yintercept = 0.7, linetype = "dashed") +
  facet_grid(index ~ subset, scales = "free_y") +
  theme_minimal() +
  labs(title = "MGCV BAM-trends per Subset en Index",
       subtitle = "Volle lijn = significant effect over de tijd (p < 0.05)",
       x = "Jaar", y = "Voorspelde waarde") +
  scale_y_continuous(limits = c(0, 1)) +
  # Stel linetype, alpha en legends in
  scale_linetype_manual(values = c("Ja" = "solid", "Nee" = "dashed"), name = "Trend significant?") +
  scale_alpha_manual(values = c("Ja" = 0.2, "Nee" = 0.05), guide = "none") +
  # Verberg de kleur/fill legendes, maar toon de linetype legende onderaan
  guides(color = "none", fill = "none") +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename =  here("output", "figuren", "trend_mi_bam_1989-2023.png"),
  plot = last_plot(), # Expliciet de laatste plot kiezen
  width = 40,
  height = 20,
  units = "cm",
  dpi = 300,
  bg = "white"
)

# Haal de ruwe data uit je geneste dataframe
mi_raw <- mi_nested %>%
  select(subset, index, data) %>%
  unnest(data)

mi_raw <- mi_raw %>%
  mutate(subset = fct_recode(subset,
                             "Kunstmatig" = "kunstmatig",
                             "Beken" = "nat_sv_beek",
                             "Kempense beken" = "nat_sv_kempen",
                             "Rivieren" = "nat_sv_rivier",
                             "Polderlopen" = "nat_sv_polder",
                             "Bovenlopen" = "rtnt"
  ))

p <- ggplot(mi_trends, aes(x = x, y = predicted)) +
  # 1. Ruwe populatie-data als transparante punten op de achtergrond
  geom_jitter(data = mi_raw, aes(x = jaar_num, y = waarde),
              alpha = 0.05, size = 0.5, color = "grey50", width = 0.2, height = 0) +

  # 2. Jouw bestaande CI ribbon (onzekerheid rond gemiddelde)
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = subset, alpha = is_significant)) +

  # 3. Jouw trendlijn
  geom_line(aes(color = subset, linetype = is_significant), linewidth = 1) +

  geom_hline(yintercept = 0.7, linetype = "dashed") +
  facet_grid(index ~ subset, scales = "free_y") +
  theme_minimal() +
  labs(title = "Gemodelleerde trendlijn per element en typologie",
       # subtitle = "Volle lijn = significant effect over de tijd (p < 0.05)",
       x = "Jaar", y = "Gefitte score") +
  scale_y_continuous(limits = c(0, 1)) +
  # Stel linetype, alpha en legends in
  scale_linetype_manual(values = c("Ja" = "solid", "Nee" = "dashed"), name = "Trend significant?") +
  scale_alpha_manual(values = c("Ja" = 0.2, "Nee" = 0.05), guide = "none") +
  # Verberg de kleur/fill legendes, maar toon de linetype legende onderaan
  guides(color = "none", fill = "none") +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))
p
ggsave(
  filename =  here("output", "figuren", "trend_mi_bam_1989-2023_raw_points.png"),
  plot = p, # Expliciet de laatste plot kiezen
  width = 40,
  height = 20,
  units = "cm",
  dpi = 300,
  bg = "white"
)

###################
# trend voor O2
###################
# --- 4. Extra: Modelleren en plotten van O2 ---

# Nesten voor o2 (zonder de pivot_longer van de indexen)
o2_nested <- mi_subsets_plot %>%
  # filter(subset %in% c("nat_sv_beek")) %>%
  filter(subset != "overig") %>%
  drop_na(o2) %>% # Zorg dat we enkel rijen met een O2 meting overhouden
  mutate(jaar_num = as.numeric(format(monsternamedatum, "%Y")),
         subset = as.factor(subset),
         meetplaats = as.factor(meetplaats),
         bekken = as.factor(bekken),
         owl = as.factor(owl),
         maand_num = lubridate::month(monsternamedatum)) %>%
  group_nest(subset) %>%
  mutate(index = "o2") # Voeg een hulpkolom toe voor de consistentie in het plotten

# Parallel cluster opzetten voor o2
cl <- makeCluster(n_cores)

# Modellen fitten voor o2 met BAM
o2_models <- o2_nested %>%
  mutate(
    # Let op: we gebruiken family = gaussian() in plaats van quasibinomial!
    model = map(data, ~ bam(o2 ~ s(jaar_num, k = 4) +
                              s(meetplaats, bs = "re") +
                              s(bekken, bs = "re") +
                              s(owl, bs = "re"),
                              # s(maand_num, bs = "cc", k = 8),
                            family = gaussian(),
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

# --- 5. O2 Resultaten Uitpakken en Visualiseren ---
o2_trends <- o2_models %>%
  select(subset, index, is_significant, predictions) %>%
  unnest(predictions)

ggplot(o2_trends, aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = subset, alpha = is_significant)) +
  geom_line(aes(color = subset, linetype = is_significant), linewidth = 1) +
  # We gebruiken facet_wrap met nrow = 1 voor 1 rij aan plots
  facet_wrap(~ subset, nrow = 1, scales = "free_y") +
  theme_minimal() +
  labs(title = "MGCV BAM-trends voor Zuurstof (O2) per Subset",
       subtitle = "Volle lijn = significant effect over de tijd (p < 0.05)",
       x = "Jaar", y = "Voorspelde O2 (mg/L)") +
  # Geen vaste y-limiet van 0 tot 1, want O2 heeft een andere range
  scale_linetype_manual(values = c("Ja" = "solid", "Nee" = "dashed"), name = "Trend significant?") +
  scale_alpha_manual(values = c("Ja" = 0.2, "Nee" = 0.05), guide = "none") +
  scale_y_continuous(limits = c(0, 10)) +
  guides(color = "none", fill = "none") +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename =  here("output", "figuren", "trend_o2_bam_1989-2023.png"),
  plot = last_plot(),
  width = 40,
  height = 12, # Iets minder hoog gemaakt (12 ipv 20 cm) omdat het maar 1 rij is
  units = "cm",
  dpi = 300,
  bg = "white"
)

###################
# trend voor ec_20
###################
# --- 4. Extra: Modelleren en plotten van ec_20 ---

# Nesten voor ec_20 (zonder de pivot_longer van de indexen)
ec_20_nested <- mi_subsets_plot %>%
  # filter(subset %in% c("nat_sv_beek")) %>%
  filter(subset != "overig") %>%
  drop_na(ec_20) %>% # Zorg dat we enkel rijen met een ec_20 meting overhouden
  mutate(jaar_num = as.numeric(format(monsternamedatum, "%Y")),
         subset = as.factor(subset),
         meetplaats = as.factor(meetplaats),
         bekken = as.factor(bekken),
         owl = as.factor(owl),
         maand_num = lubridate::month(monsternamedatum)) %>%
  group_nest(subset) %>%
  mutate(index = "ec_20") # Voeg een hulpkolom toe voor de consistentie in het plotten

# Parallel cluster opzetten voor ec_20
n_cores <- detectCores() - 1

# Modellen fitten voor ec_20 met BAM
ec_20_models <- ec_20_nested %>%
  mutate(
    # Let op: we gebruiken family = gaussian() in plaats van quasibinomial!
    model = map(data, ~ bam(ec_20 ~ s(jaar_num, k = 4) +
                              s(meetplaats, bs = "re") +
                              s(bekken, bs = "re") +
                              s(owl, bs = "re"),
                            # s(maand_num, bs = "cc", k = 8),
                            family = gaussian(),
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


# --- 5. ec_20 Resultaten Uitpakken en Visualiseren ---
ec_20_trends <- ec_20_models %>%
  select(subset, index, is_significant, predictions) %>%
  unnest(predictions)

ggplot(ec_20_trends, aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = subset, alpha = is_significant)) +
  geom_line(aes(color = subset, linetype = is_significant), linewidth = 1) +
  # We gebruiken facet_wrap met nrow = 1 voor 1 rij aan plots
  facet_wrap(~ subset, nrow = 1, scales = "free_y") +
  theme_minimal() +
  labs(title = "MGCV BAM-trends voor geleidbaarheid (ec_20) per Subset",
       subtitle = "Volle lijn = significant effect over de tijd (p < 0.05)",
       x = "Jaar", y = "Voorspelde ec_20 (sieffert)") +
  # Geen vaste y-limiet van 0 tot 1, want ec_20 heeft een andere range
  scale_linetype_manual(values = c("Ja" = "solid", "Nee" = "dashed"), name = "Trend significant?") +
  scale_alpha_manual(values = c("Ja" = 0.2, "Nee" = 0.05), guide = "none") +
  # scale_y_continuous(limits = c(0, 10)) +
  guides(color = "none", fill = "none") +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename =  here("output", "figuren", "trend_ec_20_bam_1989-2023.png"),
  plot = last_plot(),
  width = 40,
  height = 12, # Iets minder hoog gemaakt (12 ipv 20 cm) omdat het maar 1 rij is
  units = "cm",
  dpi = 300,
  bg = "white"
)

###################################################################
#######bemonsteringsbias beken en kempense beken voor trend #####
################################
# Neem je data zoals je die hebt voorbereid, maar stop vóór de 'group_nest()'
mi_ruw_voor_plot <- mi_subsets_plot %>%
  filter(subset != "overig") %>%
  pivot_longer(cols = c(mmif, tax, swd, nst, mts, ept),
               names_to = "index",
               values_to = "waarde") %>%
  mutate(jaar_num = as.numeric(format(monsternamedatum, "%Y")),
         subset = as.factor(subset),
         index = factor(index, levels = c("mmif", "tax", "swd", "nst", "mts", "ept")),
         meetplaats = as.factor(meetplaats),
         bekken = as.factor(bekken),
         owl = as.factor(owl))

# --- STAP 1: Tabel maken met extremen en tellingen ---
owl_samenvatting <- mi_ruw_voor_plot %>%
  # Filter op mmif en de twee relevante subsets
  filter(index == "mmif", subset %in% c("nat_sv_beek", "nat_sv_kempen")) %>%
  group_by(subset, owl) %>%
  summarise(
    aantal_metingen = n(),
    gemiddelde_mmif = mean(waarde, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Sorteer aflopend op aantal metingen om de "zwaargewichten" bovenaan te zien
  arrange(desc(aantal_metingen))

# Toon de top 15 meest gemeten waterlichamen
print(head(owl_samenvatting, 15))


# --- STAP 2: Bemonsteringsbias Visualiseren (Bubble Plot) ---
ggplot(owl_samenvatting, aes(x = forcats::fct_reorder(owl, gemiddelde_mmif),
                             y = gemiddelde_mmif,
                             color = subset)) +
  geom_point(aes(size = aantal_metingen), alpha = 0.6) +
  facet_wrap(~ subset, scales = "free_x") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), # Verberg de OWL-namen voor een strakker plot
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom") +
  labs(
    title = "Check bemonsteringsbias: Gemiddelde MMIF per OWL",
    subtitle = "Grote bollen trekken het ruwe populatiegemiddelde naar zich toe",
    x = "Oppervlaktewaterlichamen (OWL) - Gesorteerd van lage naar hoge score",
    y = "Gemiddelde ruwe MMIF-waarde",
    size = "Aantal metingen",
    color = "Waterlichaam type"
  )
