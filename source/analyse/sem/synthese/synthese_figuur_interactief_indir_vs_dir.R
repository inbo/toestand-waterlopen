library(tidyverse)
library(ggplot2)
library(ggh4x)
library(stringr)
library(plotly)

master_df <- readRDS(here("source", "analyse", "sem", "synthese", "sem_effecten_master_df.rds")) %>%
  mutate(
    typologie = case_when(
      typologie == "beek"   ~ "Beken",
      typologie == "kempen" ~ "Kempense beken",
      typologie == "polder" ~ "Polderlopen",
      typologie == "rivier" ~ "Rivieren",
      typologie == "rtnt"          ~ "Bovenlopen",
      typologie == "kunstmatig"    ~ "Kunstmatig", # Voor het geval deze er ook nog in zit
      TRUE ~ as.character(typologie) # Behoud de originele naam als hij niet in dit lijstje staat
    ),
    typologie = factor(typologie, levels = c(
      "Beken",
      "Kempense beken",
      "Rivieren",
      "Polderlopen",
      "Bovenlopen",
      "Kunstmatig" # Voeg deze toe als je hem ook in de dataset hebt zitten
    ))
  )

biologische_vars <- c("mmif", "ept_prop", "sw_dw", "ta_xw", "mt_sw_prop", "nst_prop", "index_nieuw", "gv_zonder_gep", "v_zonder_gep", "vo_zonder_gep", "ts_zonder_gep")

# ==============================================================================
# HULPFUNCTIE: RESPONSEN HERNOEMEN
# ==============================================================================
hernoem_respons <- function(x) {
  x <- str_remove_all(x, "_zonder_gep$")
  case_when(
    x == "ta_xw" ~ "tax",
    x == "ept_prop" ~ "ept",
    x == "nst_prop" ~ "nst",
    x %in% c("mt_sw", "mt_sw_prop") ~ "mts",
    x == "sw_dw" ~ "swd",
    TRUE ~ x
  )
}

# ==============================================================================
# UPDATE: BEPAAL groep FUNCTIE (Nu met gesplitst landgebruik)
# ==============================================================================
bepaal_groep <- function(id) {
  case_when(
    id %in% c("spei6_s", "p_sum_7d_s", "n_extreme_3m_s") ~ "Neerslag",
    id %in% c("lozingen_rwzi_ie_log", "lozingen_industrie_ie_log", "lozingen_riool_ie_log",
              "overstorten_blootstelling_index_log", "overstorten_index_log", "lozingen_rwzi_p_t_log") ~ "Lozingen en overstorten",
    id %in% c("czv_log", "o2_s", "o2_verz_s", "ec_20_s", "zs_log", "t_s", "ec_20_log", "cl_s", "p_h_s") ~ "Fysico-chemie",
    id %in% c("ekc2_waterlichaam_s", "ekc2_traject_s", "breedte_diepte_ratio_s", "sinuositeit_s",
              "bodemsub_s", "doodhout_s", "profiel_s", "stroomsnelheid_s") ~ "Hydromorfologie",
    id %in% c("n_t_log", "p_t_log", "no2_log", "no3_log", "nh4_log") ~ "Nutriënten",

    # --- DE SPLITSING VAN LANDGEBRUIK ---
    id %in% c("natuur_afstr_s", "natuur_oever_s", "perc_schaduw_s") ~ "LG-Natuur",
    id %in% c("verharding_afstr_s", "verharding_oever_s") ~ "LG-Verharding",
    id %in% c("intensiteit_combo_afstr_s", "intensiteit_combo_oeverzone_s") ~ "LG-Landbouw",
    # ------------------------------------

    id %in% biologische_vars ~ "Ecologische respons",
    TRUE ~ "Overig"
  )
}

# ==============================================================================
# STAP 1: Basispaden isoleren
# ==============================================================================
alle_paden <- master_df %>%
  filter(!grepl("~~", Predictor) & !grepl("~~", Response)) %>%
  select(groep, typologie, Predictor, Response, Std.Estimate)

# Directe pijlen naar de biologie (Lengte = 1)
directe_effecten <- alle_paden %>%
  filter(Response %in% biologische_vars)

# Paden puur binnen het abiotische netwerk (voor de dominostenen)
milieu_paden <- alle_paden %>%
  filter(!Response %in% biologische_vars)

# ==============================================================================
# STAP 2: De kettingreacties berekenen (Domino-effecten) - GEFIXED
# ==============================================================================
ind_1 <- milieu_paden %>%
  inner_join(directe_effecten, by = c("groep", "typologie", "Response" = "Predictor"), relationship = "many-to-many") %>%
  mutate(Deel_Effect = Std.Estimate.x * Std.Estimate.y) %>%
  select(groep, typologie, Bron_Predictor = Predictor, Eind_Response = Response.y, Deel_Effect)

ind_2 <- milieu_paden %>%
  inner_join(ind_1, by = c("groep", "typologie", "Response" = "Bron_Predictor"), relationship = "many-to-many") %>%
  mutate(Deel_Effect = Std.Estimate * Deel_Effect) %>%
  select(groep, typologie, Bron_Predictor = Predictor, Eind_Response, Deel_Effect)

ind_3 <- milieu_paden %>%
  inner_join(ind_2, by = c("groep", "typologie", "Response" = "Bron_Predictor"), relationship = "many-to-many") %>%
  mutate(Deel_Effect = Std.Estimate * Deel_Effect) %>%
  select(groep, typologie, Bron_Predictor = Predictor, Eind_Response, Deel_Effect)

# ==============================================================================
# STAP 3: Alle indirecte routes optellen
# ==============================================================================
indirecte_effecten <- bind_rows(ind_1, ind_2, ind_3) %>%
  group_by(groep, typologie, Predictor = Bron_Predictor, Response = Eind_Response) %>%
  summarise(Indirect_Effect = sum(Deel_Effect, na.rm = TRUE), .groups = "drop")

# ==============================================================================
# STAP 4: Combineer tot de definitieve df_effecten tabel
# ==============================================================================
df_effecten <- directe_effecten %>%
  rename(Direct_Effect = Std.Estimate) %>%
  full_join(indirecte_effecten, by = c("groep", "typologie", "Predictor", "Response")) %>%
  mutate(
    Direct_Effect = replace_na(Direct_Effect, 0),
    Indirect_Effect = replace_na(Indirect_Effect, 0),
    Totaal_Effect = Direct_Effect + Indirect_Effect
  ) %>%
  mutate(Predictor_Cat = bepaal_groep(Predictor)) %>%
  filter(Direct_Effect != 0 | Indirect_Effect != 0)

# ==============================================================================
# 1. DE Y-AS VOLGORDE DEFINIËREN (Met nieuwe namen!)
# ==============================================================================
gewenste_top_volgorde <- c(
  "mmif", "tax", "ept", "nst", "swd", "mts",         # MI (aangepast)
  "index_nieuw", "v", "gv", "ts", "vo"               # MAFY
)

alle_korte_responsen <- unique(hernoem_respons(df_effecten$Response))
rest_volgorde <- sort(setdiff(alle_korte_responsen, gewenste_top_volgorde))
definitieve_volgorde <- c(gewenste_top_volgorde, rest_volgorde)

# ==============================================================================
# 2. DATA AGGREGEREN PER CATEGORIE
# ==============================================================================
df_categorie_effecten <- df_effecten %>%
  filter(Predictor_Cat %in% c("LG-Natuur", "LG-Verharding", "LG-Landbouw", "Neerslag", "Lozingen en overstorten", "Hydromorfologie")) %>%
  group_by(groep, typologie, Predictor_Cat, Response) %>%
  summarise(
    Categorie_Direct = sum(Direct_Effect, na.rm = TRUE),
    Categorie_Indirect = sum(Indirect_Effect, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Categorie_Totaal = Categorie_Direct + Categorie_Indirect) %>%
  mutate(
    Korte_Response = hernoem_respons(Response), # Toepassen van de nieuwe namen
    Korte_Response = factor(Korte_Response, levels = rev(definitieve_volgorde))
  ) %>%
  mutate(Predictor_Cat = factor(Predictor_Cat, levels = c(
    "LG-Natuur", "LG-Landbouw", "LG-Verharding", "Neerslag", "Lozingen en overstorten", "Hydromorfologie"
  ))) %>%
  pivot_longer(
    cols = c(Categorie_Direct, Categorie_Indirect),
    names_to = "Effect_Type",
    values_to = "Waarde"
  ) %>%
  mutate(Effect_Type = case_when(
    Effect_Type == "Categorie_Direct" ~ "Direct Effect",
    Effect_Type == "Categorie_Indirect" ~ "Indirect Effect"
  ))

# ==============================================================================
# 3. DE OVERZICHTSFIGUREN GENEREREN (STATISCH)
# ==============================================================================
groepen <- unique(df_categorie_effecten$groep)

for (g in groepen) {

  responsen_in_groep <- df_effecten %>%
    filter(groep == g) %>%
    mutate(Korte_Response = hernoem_respons(Response)) %>%
    pull(Korte_Response) %>%
    unique()

  volgorde_groep <- definitieve_volgorde[definitieve_volgorde %in% responsen_in_groep]

  data_groep <- df_categorie_effecten %>%
    filter(groep == g) %>%
    mutate(Korte_Response = factor(Korte_Response, levels = rev(volgorde_groep)))

  plot_groep <- ggplot(data_groep, aes(y = Korte_Response)) +
    geom_vline(xintercept = 0, color = "grey40", linetype = "dashed", linewidth = 0.8) +
    geom_col(aes(x = Waarde, fill = Effect_Type),
             position = position_dodge(width = 0.7),
             width = 0.6, alpha = 0.85) +
    geom_point(aes(x = Categorie_Totaal, shape = "Totaal Netto Effect"),
               size = 2.5, color = "black", fill = "black") +

    # AANGEPAST: name = "Effecttype" voor zowel fill als shape zodat de legende samenkomt en netjes is
    scale_fill_manual(values = c("Direct Effect" = "#2c7bb6", "Indirect Effect" = "#fdae61"),
                      name = "Effecttype") +
    scale_shape_manual(values = c("Totaal Netto Effect" = 18), name = "Effecttype") +
    scale_y_discrete(drop = FALSE) +
    facet_nested(Predictor_Cat ~ typologie) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 4)) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.major.y = element_line(color = "grey90", linetype = "dotted"),
      panel.grid.minor.x = element_blank(),
      axis.text.y = element_text(size = 12, face = "bold", color = "black"),
      axis.text.x = element_text(size = 9),

      # AANGEPAST: strip.text.x en y zijn iets groter gemaakt (naar 14 en 13)
      strip.text.x = element_text(face = "bold", size = 14, margin = margin(t = 5, b = 5)),
      strip.text.y = element_text(face = "bold", size = 13, angle = 0),
      strip.background.x = element_rect(fill = "grey90", color = "grey40", linewidth = 0.8),
      strip.background.y = element_rect(fill = "grey95", color = NA),
      panel.border = element_rect(color = "grey40", fill = NA, linewidth = 0.8),
      panel.spacing.x = unit(0.3, "cm"),
      panel.spacing.y = unit(0.2, "cm"),
      legend.position = "bottom",
      legend.box = "horizontal"
    ) +
    labs(
      title = paste("Beleidssynthese: Totale Systeemdruk per Thema (", toupper(g), ")", sep = ""),
      subtitle = "Geaggregeerde directe en indirecte effecten op de biologische maatlatten",
      x = "Netto Gestandaardiseerde Effectgrootte",
      y = "Biologisch Eindpunt"
    )

  print(plot_groep)
}


# ==============================================================================
# HOVER-FIGUUR (INTERACTIEF)
# ==============================================================================

# 1. Bereid de hover-data voor
dom_paden <- df_effecten %>%
  filter(Predictor_Cat %in% c("LG-Natuur", "LG-Verharding", "LG-Landbouw", "Neerslag", "Lozingen en overstorten", "Hydromorfologie")) %>%
  mutate(Korte_Response = hernoem_respons(Response)) # Toepassen van de nieuwe namen

dom_dir <- dom_paden %>% filter(Direct_Effect != 0) %>%
  group_by(groep, typologie, Predictor_Cat, Korte_Response) %>%
  slice_max(order_by = abs(Direct_Effect), n = 1, with_ties = FALSE) %>%
  select(groep, typologie, Predictor_Cat, Korte_Response, Dom_Dir = Predictor, Val_Dir = Direct_Effect)

dom_indir <- dom_paden %>% filter(Indirect_Effect != 0) %>%
  group_by(groep, typologie, Predictor_Cat, Korte_Response) %>%
  slice_max(order_by = abs(Indirect_Effect), n = 1, with_ties = FALSE) %>%
  select(groep, typologie, Predictor_Cat, Korte_Response, Dom_Indir = Predictor, Val_Indir = Indirect_Effect)

plot_data_interactief <- df_categorie_effecten %>%
  left_join(dom_dir, by = c("groep", "typologie", "Predictor_Cat", "Korte_Response")) %>%
  left_join(dom_indir, by = c("groep", "typologie", "Predictor_Cat", "Korte_Response")) %>%
  mutate(Predictor_Cat = factor(Predictor_Cat, levels = c(
    "LG-Natuur", "LG-Landbouw", "LG-Verharding", "Hydromorfologie", "Neerslag", "Lozingen en overstorten"
  ))) %>%
  mutate(
    Hover_Tekst = case_when(
      Effect_Type == "Direct Effect" & Waarde != 0 ~ paste0(
        "<b>", Effect_Type, "</b><br>",
        "Totale druk: ", round(Waarde, 3), "<br>",
        "Dominante bron: ", str_remove_all(Dom_Dir, "_s$|_log$"), " (", round(Val_Dir, 3), ")"
      ),
      Effect_Type == "Indirect Effect" & Waarde != 0 ~ paste0(
        "<b>", Effect_Type, "</b><br>",
        "Totale druk: ", round(Waarde, 3), "<br>",
        "Dominante bron: ", str_remove_all(Dom_Indir, "_s$|_log$"), " (", round(Val_Indir, 3), ")"
      ),
      TRUE ~ paste0(Effect_Type, ": 0")
    ),
    Hover_Totaal = paste0(
      "<b>Totaal Netto Effect</b><br>",
      "Waarde: ", round(Categorie_Totaal, 3)
    )
  )

# 2. De interactieve figuren genereren
mijn_plotly_lijst <- list()

for(g in groepen) {

  responsen_in_groep <- df_effecten %>%
    filter(groep == g) %>%
    pull(Response) %>%
    hernoem_respons() %>%
    unique()

  volgorde_groep <- definitieve_volgorde[definitieve_volgorde %in% responsen_in_groep]

  data_groep <- plot_data_interactief %>%
    filter(groep == g) %>%
    mutate(Korte_Response = factor(Korte_Response, levels = rev(volgorde_groep)))

  p <- ggplot(data_groep, aes(y = Korte_Response)) +
    geom_vline(xintercept = 0, color = "grey40", linetype = "dashed") +
    geom_col(aes(x = Waarde, fill = Effect_Type, text = Hover_Tekst),
             position = position_dodge(width = 0.7), width = 0.6, alpha = 0.85) +
    geom_point(aes(x = Categorie_Totaal, shape = "Totaal Netto Effect", text = Hover_Totaal),
               size = 1, color = "black", fill = "black") +

    # AANGEPAST: Ook hier name = "Effecttype" toevoegen
    scale_fill_manual(values = c("Direct Effect" = "#2c7bb6", "Indirect Effect" = "#fdae61"), name = "Effecttype") +
    scale_shape_manual(values = c("Totaal Netto Effect" = 18), name = NULL) +
    scale_y_discrete(drop = FALSE) +
    facet_grid(Predictor_Cat ~ typologie) +
    theme_minimal() +
    theme(
      panel.border = element_rect(color = "grey40", fill = NA, linewidth = 0.8),
      legend.position = "bottom",
      # AANGEPAST: En ook voor de interactieve plotly plot de teksten voor rijen en kolommen vergroten!
      strip.text.x = element_text(face = "bold", size = 14),
      strip.text.y = element_text(face = "bold", size = 9.5)
    ) +
    labs(title = paste("Synthese: Totale effecten van bronvariabelen (", toupper(g), ")", sep = ""),
         x = "Netto Gestandaardiseerde Effectgrootte", y = "")

  # Maak interactief en pas legend title netjes aan (zorgt dat Plotly 'Effecttype' als subtitel pakt)
  ply <- ggplotly(p, tooltip = "text") %>%
    plotly::layout(
      margin = list(b = 80, l = 80),
      hoverdistance = 2,
      legend = list(title = list(text = "<b>Effecttype</b>"), orientation = "h", x = 0.5, xanchor = "center", y = -0.1)
    )

  toegevoegd_aan_legende <- c()

  for (i in seq_along(ply$x$data)) {
    huidige_naam <- ply$x$data[[i]]$name
    echte_naam <- stringr::str_extract(huidige_naam, "Direct Effect|Indirect Effect|Totaal Netto Effect")

    if (!is.na(echte_naam)) {
      ply$x$data[[i]]$name <- echte_naam
      ply$x$data[[i]]$legendgroup <- echte_naam

      if (!(echte_naam %in% toegevoegd_aan_legende)) {
        ply$x$data[[i]]$showlegend <- TRUE
        toegevoegd_aan_legende <- c(toegevoegd_aan_legende, echte_naam)
      } else {
        ply$x$data[[i]]$showlegend <- FALSE
      }
    } else {
      ply$x$data[[i]]$showlegend <- FALSE
    }
  }

  mijn_plotly_lijst[[g]] <- ply
}


# ==============================================================================
# BEKIJK DE FIGUREN!
# ==============================================================================
# Typ in je console:
mijn_plotly_lijst[["mafy"]]
# of
mijn_plotly_lijst[["mi"]]

# Sla de MAFY-plot op
saveWidget(
  widget = mijn_plotly_lijst[["mafy"]],
  file = "output/figuren/synthese_MAFY.html",
  selfcontained = TRUE
)

# Sla de MI-plot op
saveWidget(
  widget = mijn_plotly_lijst[["mi"]],
  file = "output/figuren/synthese_MI.html",
  selfcontained = TRUE
)

######
# clean figuur enkel indexen
#######

# ==============================================================================
# 1. DATA FILTEREN VOOR DE HOOFDGRAADMETERS
# ==============================================================================
data_gecombineerd <- plot_data_interactief %>%
  # Filter uitsluitend op jouw twee master-indexen
  filter(Korte_Response %in% c("mmif", "index_nieuw")) %>%
  # Zet de factor-volgorde vast (reversed voor de Y-as)
  mutate(Korte_Response = factor(Korte_Response, levels = rev(c("mmif", "index_nieuw"))))

# ==============================================================================
# 2. DE GECOMBINEERDE GGPLOT MAKEN
# ==============================================================================
p_comb <- ggplot(data_gecombineerd, aes(y = Korte_Response)) +
  geom_vline(xintercept = 0, color = "grey40", linetype = "dashed") +

  geom_col(aes(x = Waarde, fill = Effect_Type, text = Hover_Tekst),
           position = position_dodge(width = 0.7), width = 0.6, alpha = 0.85) +

  geom_point(aes(x = Categorie_Totaal, shape = "Totaal Netto Effect", text = Hover_Totaal),
             size = 1.5, color = "black", fill = "black") +

  scale_fill_manual(values = c("Direct Effect" = "#2c7bb6", "Indirect Effect" = "#fdae61")) +
  scale_shape_manual(values = c("Totaal Netto Effect" = 18)) +

  # Hier plaatsen we ze allemaal samen in het grid!
  facet_grid(Predictor_Cat ~ typologie) +

  theme_minimal() +
  theme(
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 0.8),
    legend.position = "bottom"
  ) +
  labs(
    title = "Systeemdruk op de Hoofdgraadmeters (MMIF & Index_Nieuw)",
    x = "Netto Gestandaardiseerde Effectgrootte",
    y = ""
  )

# ==============================================================================
# 3. OMZETTEN NAAR PLOTLY & LEGENDE OPSCHONEN
# ==============================================================================
ply_comb <- ggplotly(p_comb, tooltip = "text") %>%
  plotly::layout(
    margin = list(b = 80, l = 80),
    hoverdistance = 2,
    legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.1)
  )

toegevoegd_aan_legende <- c()

for (i in seq_along(ply_comb$x$data)) {
  huidige_naam <- ply_comb$x$data[[i]]$name
  echte_naam <- stringr::str_extract(huidige_naam, "Direct Effect|Indirect Effect|Totaal Netto Effect")

  if (!is.na(echte_naam)) {
    ply_comb$x$data[[i]]$name <- echte_naam
    ply_comb$x$data[[i]]$legendgroup <- echte_naam

    if (!(echte_naam %in% toegevoegd_aan_legende)) {
      ply_comb$x$data[[i]]$showlegend <- TRUE
      toegevoegd_aan_legende <- c(toegevoegd_aan_legende, echte_naam)
    } else {
      ply_comb$x$data[[i]]$showlegend <- FALSE
    }
  } else {
    ply_comb$x$data[[i]]$showlegend <- FALSE
  }
}

# ==============================================================================
# 4. BEKIJKEN EN OPSLAAN
# ==============================================================================
# Bekijk hem direct:
ply_comb

# Sla hem op als HTML:
saveWidget(ply_comb, file = "output/figuren/synthese_indexen.html", selfcontained = TRUE)

# belangrijkste pathways


# ==============================================================================
# 1. DATA KLAARZETTEN
# ==============================================================================
alle_paden <- master_df %>%
  filter(!grepl("~~", Predictor) & !grepl("~~", Response)) %>%
  select(groep, typologie, Predictor, Response, Std.Estimate)

directe_effecten <- alle_paden %>% filter(Response %in% biologische_vars)
milieu_paden <- alle_paden %>% filter(!Response %in% biologische_vars)


# ==============================================================================
# 2. KETTINGEN (STRINGS) BOUWEN EN EFFECTEN VERMENIGVULDIGEN
# ==============================================================================
# A. Directe paden
keten_0 <- directe_effecten %>%
  mutate(
    Pad = paste(Predictor, "➔", Response),
    Effect = Std.Estimate
  ) %>%
  select(groep, typologie, Pad, Effect)

# B. 1 Tussenstap (A ➔ B ➔ Bio)
keten_1 <- milieu_paden %>%
  inner_join(directe_effecten, by = c("groep", "typologie", "Response" = "Predictor"), relationship = "many-to-many") %>%
  mutate(
    Pad = paste(Predictor, "➔", Response, "➔", Response.y),
    Effect = Std.Estimate.x * Std.Estimate.y
  ) %>%
  select(groep, typologie, Bron_Predictor = Predictor, Pad, Eind_Response = Response.y, Effect)

# C. 2 Tussenstappen (A ➔ B ➔ C ➔ Bio)
keten_2 <- milieu_paden %>%
  inner_join(keten_1, by = c("groep", "typologie", "Response" = "Bron_Predictor"), relationship = "many-to-many") %>%
  mutate(
    Pad = paste(Predictor, "➔", Pad),
    Effect = Std.Estimate * Effect
  ) %>%
  select(groep, typologie, Bron_Predictor = Predictor, Pad, Eind_Response, Effect)

# D. 3 Tussenstappen (A ➔ B ➔ C ➔ D ➔ Bio)
keten_3 <- milieu_paden %>%
  inner_join(keten_2, by = c("groep", "typologie", "Response" = "Bron_Predictor"), relationship = "many-to-many") %>%
  mutate(
    Pad = paste(Predictor, "➔", Pad),
    Effect = Std.Estimate * Effect
  ) %>%
  select(groep, typologie, Bron_Predictor = Predictor, Pad, Eind_Response, Effect)


# ==============================================================================
# 3. ALLES COMBINEREN EN DE TOP 5 PER typologie HALEN
# ==============================================================================
alle_ketens <- bind_rows(
  keten_0,
  keten_1 %>% select(groep, typologie, Pad, Effect),
  keten_2 %>% select(groep, typologie, Pad, Effect),
  keten_3 %>% select(groep, typologie, Pad, Effect)
)

top_ketens <- alle_ketens %>%
  group_by(groep, typologie) %>%
  # Sorteer op de absoluut sterkste effecten (dus sterke negatieve (-0.6) én positieve (+0.6) effecten)
  slice_max(order_by = abs(Effect), n = 5, with_ties = FALSE) %>%
  # Zet de data netjes op volgorde voor de tabel
  arrange(groep, typologie, desc(abs(Effect))) %>%
  ungroup() %>%
  # Rond het effect netjes af voor weergave
  mutate(Effect = round(Effect, 3))


# ==============================================================================
# 4. HET RESULTAAT PRINTEN (Leesbaar formaat)
# ==============================================================================
# Print een overzichtelijke tabel naar de console
top_ketens %>%
  group_by(groep, typologie) %>%
  group_walk(~ {
    cat("\n======================================================\n")
    cat("groep:", toupper(.y$groep), "| typologie:", toupper(.y$typologie), "\n")
    cat("======================================================\n")
    print(as.data.frame(.x), row.names = FALSE)
  })

#met enkel indirecte???

# ==============================================================================
# 1. DATA KLAARZETTEN (Met ontdubbeling!)
# ==============================================================================
alle_paden <- master_df %>%
  filter(!grepl("~~", Predictor) & !grepl("~~", Response)) %>%
  # --- DE FIX: Neem het gemiddelde van paden die in meerdere modellen voorkomen ---
  group_by(groep, typologie, Predictor, Response) %>%
  summarise(Std.Estimate = mean(Std.Estimate, na.rm = TRUE), .groups = "drop")
# --------------------------------------------------------------------------------

directe_effecten <- alle_paden %>% filter(Response %in% biologische_vars)
milieu_paden <- alle_paden %>% filter(!Response %in% biologische_vars)

# (STAP 2 BLIJFT EXACT HETZELFDE)

# ==============================================================================
# 3. ALLES COMBINEREN EN DE TOP 5 HALEN
# ==============================================================================
alle_indirecte_ketens <- bind_rows(
  keten_1 %>% select(groep, typologie, Pad, Effect),
  keten_2 %>% select(groep, typologie, Pad, Effect),
  keten_3 %>% select(groep, typologie, Pad, Effect)
)

top_indirecte_ketens <- alle_indirecte_ketens %>%
  # Extra veiligheid: verwijder eventuele identieke rijen
  distinct(groep, typologie, Pad, .keep_all = TRUE) %>%
  group_by(groep, typologie) %>%
  slice_max(order_by = abs(Effect), n = 5, with_ties = FALSE) %>%
  arrange(groep, typologie, desc(abs(Effect))) %>%
  ungroup() %>%
  mutate(Effect = round(Effect, 3))

# ==============================================================================
# 4. HET RESULTAAT PRINTEN
# ==============================================================================
top_indirecte_ketens %>%
  group_by(groep, typologie) %>%
  group_walk(~ {
    cat("\n======================================================\n")
    cat("groep:", toupper(.y$groep), "| typologie:", toupper(.y$typologie), "\n")
    cat("======================================================\n")
    print(as.data.frame(.x), row.names = FALSE)
  })

