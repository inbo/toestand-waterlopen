# ==============================================================================
# MASTER R2-TABEL uit de per-model opgeslagen R2-bestanden
#   Bestanden: source/analyse/sem/synthese/sem_r2/<groep>_<typologie>_<maatlat>.rds
#   bv. mi_beek_ept.rds, mi_kempen_mmif.rds, mafy_beek_vo.rds
#   Inhoud:   resultaat van r2_psem_mumin(<psem>) (Response, Class, Family, Marginal, Conditional)
# ==============================================================================
library(tidyverse)
library(here)

# ---- 0. Instellingen ---------------------------------------------------------
map_r2 <- here("source", "analyse", "sem", "synthese", "sem_r2")

typologie_labels <- c(
  beek = "Beken", kempen = "Kempense beken", rivier = "Rivieren",
  polder = "Polderlopen", rtnt = "Bovenlopen", kunstmatig = "Kunstmatig"
)
groep_labels <- c(mafy = "Macrofyten (MAFY)", mi = "Macro-invertebraten (MI)")
biologische_vars <- c("mmif", "tax", "ept", "nst", "swd", "mts",
                      "index_nieuw", "v", "gv", "ts", "vo")

kort_naam <- function(x) {
  x <- str_remove(x, "_zonder_gep$")
  case_match(x,
             "ta_xw" ~ "tax", "ept_prop" ~ "ept", "nst_prop" ~ "nst",
             c("mt_sw", "mt_sw_prop") ~ "mts", "sw_dw" ~ "swd",
             .default = x)
}

# ---- 1. Alle R2-bestanden inlezen --------------------------------------------
bestanden <- list.files(map_r2, pattern = "\\.rds$", full.names = TRUE)
stopifnot("Geen R2-bestanden gevonden: controleer map_r2" = length(bestanden) > 0)

master_r2_df <- tibble(pad = bestanden) %>%
  mutate(
    bron      = tools::file_path_sans_ext(basename(pad)),
    # <groep>_<typologie>_<maatlat>; maatlat mag zelf underscores bevatten
    groep     = str_match(bron, "^([^_]+)_([^_]+)_(.+)$")[, 2],
    typologie = str_match(bron, "^([^_]+)_([^_]+)_(.+)$")[, 3],
    maatlat   = str_match(bron, "^([^_]+)_([^_]+)_(.+)$")[, 4],
    r2        = map(pad, readRDS)
  )

# Bestanden waarvan de naam niet herkend werd
if (anyNA(master_r2_df$maatlat)) {
  warning("Naam niet herkend voor: ",
          paste(master_r2_df$bron[is.na(master_r2_df$maatlat)], collapse = ", "))
}

master_r2_df <- master_r2_df %>%
  filter(!is.na(maatlat)) %>%
  select(groep, typologie, maatlat, r2) %>%
  unnest(r2)

saveRDS(master_r2_df, here("source", "analyse", "sem", "synthese", "sem_r2_master_df.rds"))

# Overzicht: hoeveel modellen per groep x typologie x maatlat?
master_r2_df %>% distinct(groep, typologie, maatlat) %>% count(groep, typologie) %>% print(n = Inf)

# ---- 2. Opschonen ------------------------------------------------------------
# - namen inkorten en typologie netjes labelen
# - hetzelfde submodel (bv. n_t_log) zit in meerdere maatlat-bestanden van dezelfde
#   groep x typologie: gemiddelde per groep x typologie x respons
r2_data <- master_r2_df %>%
  mutate(
    groep     = tolower(groep),
    typologie = factor(coalesce(unname(typologie_labels[typologie]), typologie),
                       levels = typologie_labels),
    Response  = kort_naam(Response)
  ) %>%
  filter(groep %in% c("mi", "mafy")) %>%
  group_by(groep, typologie, Response) %>%
  summarise(Marginal = mean(Marginal, na.rm = TRUE),
            Conditional = mean(Conditional, na.rm = TRUE),
            n_modellen = n(), .groups = "drop") %>%
  mutate(across(c(Marginal, Conditional), ~ if_else(is.nan(.x), NA_real_, .x)),
         type = if_else(Response %in% biologische_vars, "Biologie", "Abiotisch"))

# Sanity checks
controle <- r2_data %>% filter(Marginal > Conditional + 1e-6 | Marginal < 0 | Conditional > 1)
if (nrow(controle) > 0) {
  warning("Onverwachte R2-waarden (Marginal > Conditional, <0 of >1):")
  print(controle)
}

r2_lang <- r2_data %>%
  pivot_longer(c(Marginal, Conditional), names_to = "R2_type", values_to = "R2") %>%
  mutate(R2_type = factor(R2_type, levels = c("Marginal", "Conditional"),
                          labels = c("Marginaal (enkel vaste effecten)",
                                     "Conditioneel (+ random effecten)")))

# ---- 3A. Heatmap: respons x typologie, marginal en conditional naast elkaar --
plot_r2_heatmap <- function(g) {
  d <- r2_lang %>%
    filter(groep == g) %>%
    mutate(Response = fct_reorder(Response, R2, .fun = \(x) median(x, na.rm = TRUE)))

  ggplot(d, aes(x = typologie, y = Response, fill = R2)) +
    geom_tile(color = "white", linewidth = 0.6) +
    geom_text(aes(label = if_else(is.na(R2), "", sprintf("%.2f", R2))), size = 3) +
    scale_fill_gradient(low = "white", high = "#2c7bb6",
                        limits = c(0, max(r2_lang$R2, na.rm = TRUE)),
                        na.value = "grey95", name = expression(R^2)) +
    scale_x_discrete(drop = FALSE) +
    facet_grid(type ~ R2_type, scales = "free_y", space = "free_y") +
    labs(title = paste0("Verklaarde variantie (R²) per submodel: ", groep_labels[[g]]),
         x = NULL, y = "Afhankelijke variabele (submodel)") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          strip.text = element_text(face = "bold"),
          panel.grid = element_blank())
}

heatmaps <- set_names(unique(r2_data$groep)) %>% map(plot_r2_heatmap)
# heatmaps$mi ; heatmaps$mafy

# ---- 3B. Boxplot-samenvatting -------------------------------------------------
plot_r2_box <- ggplot(
  drop_na(r2_lang, R2),
  aes(x = R2, y = fct_reorder(Response, R2, .fun = median), fill = R2_type, color = R2_type)
) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA, position = position_dodge(width = 0.8)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8),
             size = 1.8, alpha = 0.7) +
  facet_wrap(~ groep, ncol = 2, scales = "free_y", labeller = labeller(groep = groep_labels)) +
  scale_fill_manual(name = "Verklaarde variantie:", values = c("#fc8d62", "#8da0cb")) +
  scale_color_manual(name = "Verklaarde variantie:", values = c("#fc8d62", "#8da0cb")) +
  labs(title = "Verklaarde variantie (R²) per submodel in het pSEM",
       x = expression(R^2), y = "Afhankelijke variabele (submodel)") +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        strip.background = element_rect(fill = "grey90", color = "white"),
        strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())

# print(plot_r2_box)

# ---- 4. Opslaan (optioneel) ---------------------------------------------------
# dir.create(here("output", "figuren"), recursive = TRUE, showWarnings = FALSE)
# iwalk(heatmaps, ~ ggsave(here("output", "figuren", paste0("r2_heatmap_", .y, ".png")),
#                          .x, width = 11, height = 9, dpi = 300))
# ggsave(here("output", "figuren", "r2_boxplot.png"), plot_r2_box, width = 12, height = 8, dpi = 300)
