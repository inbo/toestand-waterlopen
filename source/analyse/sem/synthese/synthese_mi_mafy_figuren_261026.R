# ==============================================================================
# SEM-SYNTHESE: directe + indirecte effecten uit piecewise SEM-modellen
# Opbouw:
#   0. Instellingen & hulpfuncties
#   1. Modellen inlezen en samenvoegen
#   2. Schone padentabel (1 rij per Predictor -> Response)
#   3. ALLE ketens (paden) berekenen: enige bron voor effecten + hover-tekst
#   4. Effectentabel afleiden uit de ketens
#   5. Plotfuncties
#   6. Figuren: categorieen, enkel indexen, fysicochemie
#   7. Top-ketens (tabellen)
# ==============================================================================
library(tidyverse)
library(here)
library(plotly)
library(htmlwidgets)
library(conflicted)
conflicts_prefer(
  base::intersect, base::setdiff, base::union,
  dplyr::select, dplyr::filter, dplyr::rename, dplyr::slice,
  plotly::layout
)

# ==============================================================================
# 0. INSTELLINGEN & HULPFUNCTIES
# ==============================================================================
map_pad     <- here("source", "analyse", "sem", "synthese", "sem_output")
sig_drempel <- 0.05
max_stappen <- 4   # 1 = direct pad, 2 = via 1 tussenvariabele, ... (zoals je 3 inner_joins)

# Labels (typologie-volgorde = volgorde van de facetten)
typologie_labels <- c(
  beek = "Beken", kempen = "Kempense beken", rivier = "Rivieren",
  polder = "Polderlopen", rtnt = "Bovenlopen", kunstmatig = "Kunstmatig"
)

physico_labels <- c(
  zs_log = "Zwevende Stof", czv_log = "CZV", o2_s = "Zuurstof",
  n_t_log = "Stikstof (N)", p_t_log = "Fosfor (P)", ec_20_log = "Geleidbaarheid"
)
target_physico <- names(physico_labels)

hoofd_categorieen <- c("LG-Natuur", "LG-Landbouw", "LG-Verharding",
                       "Hydromorfologie", "Neerslag", "Lozingen en overstorten")

# Biologische eindpunten (KORTE namen, zie kort_naam())
biologische_vars <- c("mmif", "tax", "ept", "nst", "swd", "mts",   # MI
                      "ekc_mafy", "v", "gv", "ts", "vo")        # MAFY
volgorde_top <- biologische_vars

# EEN plek waar namen worden ingekort. Wordt op Predictor EN Response toegepast
# direct na het inlezen, zodat alle latere stappen dezelfde namen zien.
kort_naam <- function(x) {
  x <- str_remove(x, "_zonder_gep$")
  case_match(x,
             "ta_xw" ~ "tax", "ept_prop" ~ "ept", "nst_prop" ~ "nst",
             c("mt_sw", "mt_sw_prop") ~ "mts", "sw_dw" ~ "swd",
             .default = x)
}

# Verwijdert _s / _log suffixen overal in een (pad-)string, niet enkel op het einde
schoon <- function(x) str_replace_all(x, "_(s|log)(?=\\s|$)", "")

bepaal_categorie <- function(id) {
  case_when(
    id %in% c("spei6_s", "p_sum_7d_s", "n_extreme_3m_s") ~ "Neerslag",
    id %in% c("lozingen_rwzi_ie_log", "lozingen_industrie_ie_log", "lozingen_riool_ie_log",
              "overstorten_blootstelling_index_log", "overstorten_index_log",
              "lozingen_rwzi_p_t_log") ~ "Lozingen en overstorten",
    id %in% c("czv_log", "o2_s", "o2_verz_s", "ec_20_s", "zs_log", "t_s", "ec_20_log",
              "cl_s", "p_h_s") ~ "Fysico-chemie",
    id %in% c("ekc2_waterlichaam_s", "ekc2_traject_s", "breedte_diepte_ratio_s", "sinuositeit_s",
              "bodemsub_s", "doodhout_s", "profiel_s", "stroomsnelheid_s") ~ "Hydromorfologie",
    id %in% c("n_t_log", "p_t_log", "no2_log", "no3_log", "nh4_log") ~ "Nutriënten",
    id %in% c("natuur_afstr_s", "natuur_oever_s", "perc_schaduw_s") ~ "LG-Natuur",
    id %in% c("verharding_afstr_s", "verharding_oever_s") ~ "LG-Verharding",
    id %in% c("intensiteit_combo_afstr_s", "intensiteit_combo_oeverzone_s") ~ "LG-Landbouw",
    id %in% biologische_vars ~ "Ecologische respons",
    TRUE ~ "Overig"
  )
}

# ==============================================================================
# 1. MODELLEN INLEZEN EN SAMENVOEGEN
# ==============================================================================
lees_model <- function(pad) {
  # bestandsnaam: groep_typologie_maatlat (maatlat mag zelf underscores bevatten)
  delen <- str_split_fixed(tools::file_path_sans_ext(basename(pad)), "_", 3)
  readRDS(pad) %>%
    mutate(groep = delen[1], typologie = delen[2], maatlat = delen[3],
           Std.Error = as.numeric(Std.Error), .before = 1)
}

bestanden <- list.files(map_pad, pattern = "\\.rds$", full.names = TRUE)
stopifnot("Geen .rds bestanden gevonden" = length(bestanden) > 0)
message(length(bestanden), " .rds bestanden worden samengevoegd...")

master_df <- map_dfr(bestanden, lees_model) %>%
  filter(P.Value < sig_drempel)

saveRDS(master_df, here("source", "analyse", "sem", "synthese", "sem_effecten_master_df.rds"))

# ==============================================================================
# 2. SCHONE PADENTABEL
#    - enkel regressiepaden (geen ~~ correlaties)
#    - namen 1x inkorten, typologie 1x hernoemen
#    - paden die in meerdere modellen voorkomen: gemiddelde (dedupe)
# ==============================================================================
paden <- master_df %>%
  filter(!str_detect(Predictor, "~~"), !str_detect(Response, "~~")) %>%
  mutate(
    Predictor = kort_naam(Predictor),
    Response  = kort_naam(Response),
    typologie = factor(coalesce(unname(typologie_labels[typologie]), typologie),
                       levels = typologie_labels)
  ) %>%
  group_by(groep, typologie, Predictor, Response) %>%
  summarise(Std.Estimate = mean(Std.Estimate, na.rm = TRUE), .groups = "drop")

# ==============================================================================
# 3. ALLE KETENS BEREKENEN (vervangt ind_1..3 EN keten_0..3)
#    Elke rij = 1 pad van een bronvariabele naar een biologisch eindpunt, met
#    de padstring en het product van de gestandaardiseerde coefficienten.
# ==============================================================================
bouw_ketens <- function(paden, max_stappen = 4) {
  directe <- paden %>% filter(Response %in% biologische_vars)
  milieu  <- paden %>% filter(!Response %in% biologische_vars)

  huidig <- directe %>%
    transmute(groep, typologie, Bron = Predictor, Eind = Response,
              Pad = paste(Predictor, "➔", Response), Effect = Std.Estimate, Stappen = 1L)
  alle <- list(huidig)

  for (i in 2:max_stappen) {
    huidig <- milieu %>%
      select(groep, typologie, Bron = Predictor, Tussen = Response, Beta = Std.Estimate) %>%
      inner_join(rename(huidig, Tussen = Bron),
                 by = c("groep", "typologie", "Tussen"),
                 relationship = "many-to-many") %>%
      transmute(groep, typologie, Bron, Eind,
                Pad = paste(Bron, "➔", Pad), Effect = Beta * Effect, Stappen = i)
    if (nrow(huidig) == 0) break
    alle[[length(alle) + 1]] <- huidig
  }
  bind_rows(alle)
}

ketens <- bouw_ketens(paden, max_stappen)

# ==============================================================================
# 4. EFFECTENTABEL + DOMINANTE INDIRECTE PADEN (afgeleid uit dezelfde ketens)
# ==============================================================================
df_effecten <- ketens %>%
  group_by(groep, typologie, Predictor = Bron, Response = Eind) %>%
  summarise(Direct   = sum(Effect[Stappen == 1]),
            Indirect = sum(Effect[Stappen > 1]),
            .groups = "drop") %>%
  mutate(Totaal = Direct + Indirect,
         Predictor_Cat = bepaal_categorie(Predictor)) %>%
  filter(Direct != 0 | Indirect != 0)

dominant_indirect <- ketens %>%
  filter(Stappen > 1) %>%
  group_by(groep, typologie, Predictor = Bron, Response = Eind) %>%
  slice_max(abs(Effect), n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(groep, typologie, Predictor, Response, Dom_Indirect_Pad = Pad)

# Y-as volgorde (vast voor alle figuren)
volgorde_responsen <- c(volgorde_top,
                        sort(setdiff(unique(df_effecten$Response), volgorde_top)))

# ==============================================================================
# 5. PLOTFUNCTIES
# ==============================================================================
naar_lang <- function(d) {
  d %>%
    pivot_longer(c(Direct, Indirect), names_to = "Effect_Type", values_to = "Waarde") %>%
    mutate(Effect_Type = paste(Effect_Type, "Effect"))
}

hover_effect <- function(type, waarde, detail) {
  if_else(waarde == 0,
          paste0(type, ": 0"),
          paste0("<b>", type, "</b><br>Effect: ", round(waarde, 3), "<br>", detail))
}

# Basisplot: rijen = `rij` (string met kolomnaam), kolommen = typologie
plot_effecten <- function(data, rij, titel, niveaus) {
  data <- data %>%
    mutate(Response = factor(Response, levels = rev(niveaus))) %>%
    filter(!is.na(Response))

  ggplot(data, aes(y = Response)) +
    geom_vline(xintercept = 0, color = "grey40", linetype = "dashed") +
    geom_col(aes(x = Waarde, fill = Effect_Type, text = Hover_Tekst),
             position = position_dodge(width = 0.7), width = 0.6, alpha = 0.85) +
    geom_point(aes(x = Totaal, shape = "Totaal Netto Effect", text = Hover_Totaal),
               size = 1.5, color = "black") +
    scale_fill_manual(values = c("Direct Effect" = "#2c7bb6", "Indirect Effect" = "#fdae61"),
                      name = "Effecttype") +
    scale_shape_manual(values = c("Totaal Netto Effect" = 18), name = NULL) +
    scale_y_discrete(drop = FALSE) +
    facet_grid(rows = vars(.data[[rij]]), cols = vars(typologie)) +
    theme_minimal() +
    theme(
      panel.border = element_rect(color = "grey40", fill = NA, linewidth = 0.8),
      legend.position = "bottom",
      axis.text.y  = element_text(size = 11, face = "bold", color = "black"),
      strip.text.x = element_text(face = "bold", size = 13, margin = margin(t = 5, b = 5)),
      strip.text.y = element_text(face = "bold", size = 9.5)
    ) +
    labs(title = titel, x = "Netto gestandaardiseerde effectgrootte", y = NULL)
}

# 1 ggplot per groep (MI / MAFY) -> named list
plots_per_groep <- function(data, rij, titel) {
  set_names(unique(data$groep)) %>%
    map(function(g) {
      niveaus <- intersect(volgorde_responsen, df_effecten$Response[df_effecten$groep == g])
      plot_effecten(filter(data, groep == g), rij,
                    paste0(titel, " (", toupper(g), ")"), niveaus)
    })
}

# ggplot -> plotly, met opgeschoonde legende (1 item per effecttype)
naar_plotly <- function(p) {
  ply <- ggplotly(p, tooltip = "text") %>%
    plotly::layout(
      margin = list(b = 100, l = 80, r = 40),
      hoverdistance = 2,
      legend = list(title = list(text = "<b>Effecttype</b>"),
                    orientation = "h", x = 0.5, xanchor = "center", y = -0.12)
    ) %>%
    htmlwidgets::onRender("
      function(el) {
        new ResizeObserver(function() { Plotly.Plots.resize(el); }).observe(el);
      }")

  gezien <- character()
  for (i in seq_along(ply$x$data)) {
    naam <- ply$x$data[[i]]$name
    echt <- if (is.null(naam)) NA_character_ else
      str_extract(naam, "Direct Effect|Indirect Effect|Totaal Netto Effect")
    if (!is.na(echt)) {
      ply$x$data[[i]]$name        <- echt
      ply$x$data[[i]]$legendgroup <- echt
      ply$x$data[[i]]$showlegend  <- !(echt %in% gezien)
      gezien <- base::union(gezien, echt)
    } else {
      ply$x$data[[i]]$showlegend <- FALSE
    }
  }
  ply
}

opslaan <- function(lijst, prefix) {
  dir.create(here("output", "figuren", "sg"), recursive = TRUE, showWarnings = FALSE)
  iwalk(lijst, ~ saveWidget(.x, here("output", "figuren", "sg", paste0(prefix, "_", toupper(.y), ".html")),
                            selfcontained = TRUE))
}

# ==============================================================================
# 6A. FIGUREN PER THEMA (Predictor_Cat) -> biologie
# ==============================================================================
cat_long <- df_effecten %>%
  filter(Predictor_Cat %in% hoofd_categorieen) %>%
  group_by(groep, typologie, Predictor_Cat, Response) %>%
  # LET OP: Dom_* moeten VOOR de sommen staan (summarise werkt sequentieel)
  summarise(
    Dom_Dir   = Predictor[which.max(abs(Direct))],   Val_Dir   = Direct[which.max(abs(Direct))],
    Dom_Indir = Predictor[which.max(abs(Indirect))], Val_Indir = Indirect[which.max(abs(Indirect))],
    Direct = sum(Direct), Indirect = sum(Indirect), Totaal = sum(Totaal),
    .groups = "drop"
  ) %>%
  mutate(Predictor_Cat = factor(Predictor_Cat, levels = hoofd_categorieen)) %>%
  naar_lang() %>%
  mutate(
    Dom     = if_else(Effect_Type == "Direct Effect", Dom_Dir, Dom_Indir),
    Dom_Val = if_else(Effect_Type == "Direct Effect", Val_Dir, Val_Indir),
    Hover_Tekst  = hover_effect(Effect_Type, Waarde,
                                paste0("Dominante bron: ", schoon(Dom), " (", round(Dom_Val, 3), ")")),
    Hover_Totaal = paste0("<b>Totaal Netto Effect</b><br>Waarde: ", round(Totaal, 3))
  )

plots_cat  <- plots_per_groep(cat_long, "Predictor_Cat", "Synthese: totale effecten van bronvariabelen")
plotly_cat <- map(plots_cat, naar_plotly)

# plots_cat$mi        # statisch
# plotly_cat$mi       # interactief
opslaan(plotly_cat, "synthese")

# Enkel de twee hoofdindexen, beide groepen in 1 figuur
plotly_indexen <- plot_effecten(cat_long, "Predictor_Cat",
                                "Systeemdruk op de hoofdgraadmeters (MMIF & EKC MAFY)",
                                niveaus = c("mmif", "ekc_mafy")) %>%
  naar_plotly()
saveWidget(plotly_indexen, here("output", "figuren", "sg", "synthese_indexen.html"), selfcontained = TRUE)

# ==============================================================================
# 6B. FIGUREN FYSICOCHEMIE -> biologie (hover: dominant pad + sterkste hoofddruk)
# ==============================================================================
# Sterkste DIRECTE hoofddruk op elke fysicochemische parameter
sterkste_bron <- paden %>%
  filter(Response %in% target_physico) %>%
  mutate(Bron_Cat = bepaal_categorie(Predictor)) %>%
  filter(Bron_Cat %in% hoofd_categorieen) %>%
  group_by(groep, typologie, Response) %>%
  slice_max(abs(Std.Estimate), n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(groep, typologie, Physico = Response,
         Dom_Bron_Cat = Bron_Cat, Dom_Bron = Predictor, Bron_Effect = Std.Estimate)

physico_long <- df_effecten %>%
  filter(Predictor %in% target_physico) %>%
  left_join(sterkste_bron, by = c("groep", "typologie", "Predictor" = "Physico")) %>%
  left_join(dominant_indirect, by = c("groep", "typologie", "Predictor", "Response")) %>%
  replace_na(list(Dom_Bron = "geen data", Dom_Bron_Cat = "onbekend", Bron_Effect = 0)) %>%
  mutate(
    Rij = factor(unname(physico_labels[Predictor]), levels = physico_labels),
    Hoofddruk = paste0("<i>Sterkste hoofddruk op ", Rij, ":</i><br>",
                       Dom_Bron_Cat, " (", schoon(Dom_Bron), ": ", round(Bron_Effect, 3), ")")
  ) %>%
  naar_lang() %>%
  mutate(
    Hover_Tekst = hover_effect(
      Effect_Type, Waarde,
      if_else(Effect_Type == "Direct Effect",
              Hoofddruk,
              paste0("<i>Loopt dominant via:</i><br>", schoon(Dom_Indirect_Pad),
                     "<br>-----------------------------<br>", Hoofddruk))),
    Hover_Totaal = paste0("<b>Netto Effect (", Rij, ")</b><br>Waarde: ", round(Totaal, 3))
  )

plots_physico  <- plots_per_groep(physico_long, "Rij", "Effecten van fysicochemie op biologie")
plotly_physico <- map(plots_physico, naar_plotly)
opslaan(plotly_physico, "synthese_fysico")

plotly_physico_indexen <- plot_effecten(physico_long, "Rij",
                                        "Effecten van fysicochemie op de hoofdgraadmeters (MMIF & EKC MAFY)",
                                        niveaus = c("mmif", "ekc_mafy")) %>%
  naar_plotly()
saveWidget(plotly_physico_indexen, here("output", "figuren", "sg", "synthese_fysicochemie.html"),
           selfcontained = TRUE)

# ==============================================================================
# 7. TOP-KETENS (tabellen)
# ==============================================================================
top_ketens <- function(ketens, n = 5, alleen_indirect = FALSE) {
  ketens %>%
    filter(!alleen_indirect | Stappen > 1) %>%
    distinct(groep, typologie, Pad, .keep_all = TRUE) %>%
    group_by(groep, typologie) %>%
    slice_max(abs(Effect), n = n, with_ties = FALSE) %>%
    ungroup() %>%
    arrange(groep, typologie, desc(abs(Effect))) %>%
    transmute(groep, typologie, Pad, Effect = round(Effect, 3))
}

print_top <- function(df) {
  df %>%
    group_by(groep, typologie) %>%
    group_walk(~ {
      cat("\n======================================================\n")
      cat("groep:", toupper(.y$groep), "| typologie:", toupper(as.character(.y$typologie)), "\n")
      cat("======================================================\n")
      print(as.data.frame(.x), row.names = FALSE)
    })
}

print_top(top_ketens(ketens))                          # alle ketens
print_top(top_ketens(ketens, alleen_indirect = TRUE))  # enkel indirecte ketens
