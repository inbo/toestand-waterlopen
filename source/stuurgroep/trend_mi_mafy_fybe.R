library(mgcv)
library(purrr)
library(tidyr)
library(dplyr)
library(forcats)
library(lubridate)
library(ggeffects)
library(ggplot2)
library(parallel)
library(here)
meetnetten <- read.delim("data/ruw/vmm/meetnetten.txt")

# --- 0. Instellingen ---
n_cores <- max(1, detectCores() - 1)
start_jaar <- 2007

subset_labels <- c(
  "nat_sv_beek"   = "Beken",
  "nat_sv_kempen" = "Kempense beken",
  "nat_sv_rivier" = "Rivieren",
  "nat_sv_polder" = "Polderlopen",
  "rtnt"          = "Bovenlopen"
)

# =========================================================================
# 1. Data Voorbereiding per Indicator
# =========================================================================

# --- 1.1 Macro-invertebraten (MMIF) ---
source("source/analyse/mi_datasets_prep.R")

mi_clean <- mi_data_analyse %>%
  drop_na(mmif) %>%
  mutate(
    jaar_num = as.numeric(format(monsternamedatum, "%Y")),
    subset = case_when(
      statuut %in% c("Natuurlijk", "Sterk Veranderd") & groep == "beek"   ~ "nat_sv_beek",
      statuut %in% c("Natuurlijk", "Sterk Veranderd") & groep == "kempen" ~ "nat_sv_kempen",
      statuut %in% c("Natuurlijk", "Sterk Veranderd") & groep == "polder" ~ "nat_sv_polder",
      statuut %in% c("Natuurlijk", "Sterk Veranderd") & groep == "rivier" ~ "nat_sv_rivier",
      statuut == "Kunstmatig" & groep %in% c("rivier", "beek", "kempen", "polder") ~ "kunstmatig",
      type == "RtNt"                                                      ~ "rtnt",
      TRUE                                                                ~ "overig"
    ),
    index = "Macro-invertebraten (MMIF)",
    waarde = mmif
  ) %>%
  filter(jaar_num >= start_jaar, !subset %in% c("kunstmatig", "overig")) %>%
  select(meetplaats, bekken, owl, jaar_num, monsternamedatum, subset, index, waarde)

# --- 1.2 Macrofyten (EKC) ---
source("source/analyse/mafy_datasets_prep.R")

mafy_clean <- mafy_data3 %>%
  drop_na(ekc_mafy) %>%
  mutate(
    jaar_num = as.numeric(format(monsternamedatum, "%Y")),
    subset = case_when(
      status == "NAT" & groep == "beek"   ~ "nat_sv_beek",
      status == "NAT" & groep == "kempen" ~ "nat_sv_kempen",
      status == "NAT" & groep == "polder" ~ "nat_sv_polder",
      status == "NAT" & groep == "rivier" ~ "nat_sv_rivier",
      status == "KUNST"                   ~ "kunstmatig",
      waterlichaamcategorietype == "RtNt" ~ "rtnt",
      TRUE                                ~ "overig"
    ),
    index = "Macrofyten",
    waarde = ekc_mafy
  ) %>%
  filter(jaar_num >= start_jaar, !subset %in% c("kunstmatig", "overig")) %>%
  select(meetplaats, bekken, owl, jaar_num, monsternamedatum, subset, index, waarde)

# --- 1.3 Fytobenthos (Index) ---
load(file = here("data", "verwerkt", "fybe_data.rdata"))

# Join toekennen aan fybe_data
# fybe_data_joined <- fybe_data %>%
#   mutate(meetplaats = as.character(meetplaats)) %>%
#   inner_join(meetnetten %>% mutate(nummer = as.character(nummer)),
#              by = c("meetplaats" = "nummer"))

fybe_clean <- fybe_data %>%
  drop_na(fybe_index) %>%
  filter(groep != "meer") %>%
  mutate(
    jaar_num = as.numeric(format(monsternamedatum, "%Y")),
    subset = case_when(
      status == "NAT" & groep == "beek"   ~ "nat_sv_beek",
      status == "NAT" & groep == "kempen" ~ "nat_sv_kempen",
      status == "NAT" & groep == "polder" ~ "nat_sv_polder",
      status == "NAT" & groep == "rivier" ~ "nat_sv_rivier",
      status == "KUNST"                   ~ "kunstmatig",
      waterlichaamcategorietype == "RtNt" ~ "rtnt",
      TRUE                                ~ "overig"
    ),
    index = "Fytobenthos",
    waarde = fybe_index
  ) %>%
  filter(jaar_num >= start_jaar, !subset %in% c("kunstmatig", "overig")) %>%
  select(meetplaats, bekken, owl, jaar_num, monsternamedatum, subset, index, waarde)

# =========================================================================
# 2. Gecombineerde Dataset & Nesten
# =========================================================================

bqe_combined <- bind_rows(mi_clean, mafy_clean, fybe_clean) %>%
  mutate(
    meetplaats = as.factor(meetplaats),
    bekken     = as.factor(bekken),
    owl        = as.factor(owl),
    # Zet niveaus en labels voor de facetten vast
    subset     = factor(subset, levels = names(subset_labels), labels = subset_labels),
    index      = factor(index, levels = c(
      "Macro-invertebraten (MMIF)",
      "Macrofyten",
      "Fytobenthos"
    ))
  )

# Nesten per combinatie van Kwaliteitselement x Typologie (3 x 5 = 15 modellen)
bqe_nested <- bqe_combined %>%
  group_nest(index, subset)

# =========================================================================
# 3. MGCV BAM Modellen Fitten & Predicties
# =========================================================================

bqe_models <- bqe_nested %>%
  mutate(
    model = map(data, ~ bam(
      waarde ~ s(jaar_num, k = 4) +
        s(meetplaats, bs = "re") +
        s(bekken, bs = "re") +
        s(owl, bs = "re"),
      family   = quasibinomial,
      data     = .x,
      method   = "fREML",
      discrete = TRUE,
      nthreads = n_cores
    )),

    # P-waarde van de jaartrend extraheren
    p_val_jaar = map_dbl(model, ~ summary(.x)$s.table["s(jaar_num)", "p-value"]),
    is_significant = factor(ifelse(p_val_jaar < 0.05, "Ja", "Nee"), levels = c("Ja", "Nee")),

    # Populatiegemiddelde voorspellingen berekenen (fixed effects)
    predictions = map(model, ~ as.data.frame(
      ggpredict(.x,
                terms = "jaar_num [all]",
                type = "fixed",
                exclude = c("s(meetplaats)", "s(bekken)", "s(owl)"))
    ))
  )

# =========================================================================
# 4. Data Uitpakken
# =========================================================================

# Gemodelleerde trendlijnen + CI
bqe_trends <- bqe_models %>%
  select(index, subset, is_significant, predictions) %>%
  unnest(predictions)

# Ruwe observaties
bqe_raw <- bqe_nested %>%
  select(index, subset, data) %>%
  unnest(data)

# =========================================================================
# 5. Gecombineerde Visualisatie met element-specifieke drempels
# =========================================================================

# 1. Definieer de specifieke grenzen voor de goede toestand per index
drempels_bqe <- tibble(
  index = factor(
    c("Macro-invertebraten (MMIF)", "Macrofyten", "Fytobenthos"),
    levels = c("Macro-invertebraten (MMIF)", "Macrofyten", "Fytobenthos")
  ),
  drempel = c(0.7, 0.6, 0.6)
)

# 2. Plot opbouwen
p_bqe <- ggplot(bqe_trends, aes(x = x, y = predicted)) +
  # Ruwe observaties als jitter-punten
  geom_jitter(
    data = bqe_raw,
    aes(x = jaar_num, y = waarde),
    alpha = 0.08, size = 0.6, color = "grey40", width = 0.25, height = 0
  ) +

  # Betrouwbaarheidsinterval
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = subset, alpha = is_significant)) +

  # Gefitte trendlijn
  geom_line(aes(color = subset, linetype = is_significant), linewidth = 1) +

  # Dynamische drempel per rij (0.7 voor MMIF, 0.6 voor Mafy & Fybe)
  geom_hline(
    data = drempels_bqe,
    aes(yintercept = drempel),
    linetype = "dashed",
    color = "grey20",
    linewidth = 0.6
  ) +

  # Raster: Rijen = Indicator, Kolommen = Typologie
  facet_grid(index ~ subset) +

  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(breaks = seq(start_jaar, 2023, by = 4)) +
  scale_linetype_manual(values = c("Ja" = "solid", "Nee" = "dashed"), name = "Trend significant (p < 0.05)?") +
  scale_alpha_manual(values = c("Ja" = 0.25, "Nee" = 0.08), guide = "none") +
  guides(color = "none", fill = "none") +
  theme_minimal(base_size = 11) +
  labs(
    title = "Gemodelleerde trends van biologische kwaliteitselementen per typologie",
    subtitle = "Volle lijn = significante trend (p < 0.05) | Stippellijn = Goede toestand (0.7 voor MMIF, 0.6 voor Macrofyten & Fytobenthos)",
    x = "Jaar",
    y = "EQR-score (Gefit & Ruwe punten)"
  ) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0.8, "lines")
  )

p_bqe
# =========================================================================
# 6. Opslaan
# =========================================================================

ggsave(
  filename = here("output", "figuren", "sg", "trend_bqe_bam_2007-2023_overview.png"),
  plot = p_bqe,
  width = 38,
  height = 22,
  units = "cm",
  dpi = 300,
  bg = "white"
)
