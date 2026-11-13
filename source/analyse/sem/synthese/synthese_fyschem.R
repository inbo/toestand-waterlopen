library(tidyverse)
library(plotly)
library(stringr)

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
    )),
    Response = case_when(
      Response == "ta_xw" ~ "tax",
      Response == "ept_prop" ~ "ept",
      Response == "nst_prop" ~ "nst",
      Response %in% c("mt_sw", "mt_sw_prop") ~ "mts",
      Response == "sw_dw" ~ "swd",
      TRUE ~ Response
    )
  )

df_effecten <- df_effecten %>%
  mutate(Response = case_when(
    Response == "ta_xw" ~ "tax",
    Response == "ept_prop" ~ "ept",
    Response == "nst_prop" ~ "nst",
    Response %in% c("mt_sw", "mt_sw_prop") ~ "mts",
    Response == "sw_dw" ~ "swd",
    TRUE ~ Response
  ))

# ==============================================================================
# 1. ACHTERWAARTS TRACEREN: Wat is de grootste bron van de fysicochemie?
# ==============================================================================
target_physico <- c("zs_log", "czv_log", "o2_s", "n_t_log", "p_t_log", "ec_20_log")

sterkste_bronnen <- master_df %>%
  # Filter de paden uit het netwerk die NAAR jouw fysicochemie lopen
  filter(!grepl("~~", Predictor) & !grepl("~~", Response)) %>%
  filter(Response %in% target_physico) %>%
  group_by(groep, typologie, Response) %>%
  # Pak per parameter in elk ecosysteem de sterkste sturende bron
  slice_max(order_by = abs(Std.Estimate), n = 1, with_ties = FALSE) %>%
  select(groep, typologie, Predictor_Physico = Response, Dom_Bron = Predictor, Bron_Effect = Std.Estimate) %>%
  ungroup()

# ==============================================================================
# 2. DATA KOPPELEN EN KLAARMAKEN VOOR DE PLOT
# ==============================================================================
plot_data_physico <- df_effecten %>%

  # Pak alleen de paden VAN fysicochemie NAAR biologie
  filter(Predictor %in% target_physico) %>%

  # Koppel de achterwaartse bron eraan vast
  left_join(sterkste_bronnen, by = c("groep", "typologie", "Predictor" = "Predictor_Physico")) %>%

  mutate(
    Korte_Response = str_remove_all(Response, "_zonder_gep$"),
    Dom_Bron = str_remove_all(replace_na(Dom_Bron, "Geen specifieke bron (Exogeen)"), "_s$|_log$"),
    Bron_Effect = replace_na(Bron_Effect, 0),

    # Mooie labels voor de rijen in de grafiek
    Mooie_Physico = case_when(
      Predictor == "zs_log" ~ "Zwevende Stof",
      Predictor == "czv_log" ~ "CZV",
      Predictor == "o2_s" ~ "Zuurstof",
      Predictor == "n_t_log" ~ "Stikstof (N)",
      Predictor == "p_t_log" ~ "Fosfor (P)",
      Predictor == "ec_20_log" ~ "Geleidbaarheid",
      TRUE ~ Predictor
    )
  ) %>%

  # Breed maken naar lang formaat voor ggplot
  pivot_longer(cols = c(Direct_Effect, Indirect_Effect), names_to = "Effect_Type", values_to = "Waarde") %>%
  mutate(Effect_Type = if_else(Effect_Type == "Direct_Effect", "Direct Effect", "Indirect Effect")) %>%

  # DE MAGIE: Custom hover text
  mutate(
    Hover_Tekst = case_when(
      Waarde != 0 ~ paste0(
        "<b>", Effect_Type, " op biologie</b><br>",
        "Effectgrootte: ", round(Waarde, 3), "<br>",
        "-----------------------------<br>",
        "<i>Grootste bron van ", Mooie_Physico, ":</i><br>",
        Dom_Bron, " (", round(Bron_Effect, 3), ")"
      ),
      TRUE ~ ""
    ),
    Hover_Totaal = paste0(
      "<b>Netto Effect (", Mooie_Physico, ")</b><br>",
      "Waarde: ", round(Totaal_Effect, 3), "<br>",
      "-----------------------------<br>",
      "<i>Grootste bron:</i><br>",
      Dom_Bron, " (", round(Bron_Effect, 3), ")"
    )
  )

# ==============================================================================
# 3. INTERACTIEVE FIGUREN MAKEN (MAFY vs MI)
# ==============================================================================
physico_plotly_lijst <- list()
groepen <- unique(plot_data_physico$groep)

# ==============================================================================
# 1. DE Y-AS VOLGORDE DEFINIËREN
# ==============================================================================
# Jouw gewenste volgorde voor de top-maatlatten (MI en MAFY gecombineerd)
gewenste_top_volgorde <- c(
  "mmif", "tax", "ept", "nst", "swd", "mts",         # MI
  "index_nieuw", "v", "gv", "ts", "vo"   # MAFY
)

# Zoek uit of er toevallig nog andere responsen in je dataset zitten
alle_korte_responsen <- unique(str_remove_all(df_effecten$Response, "_zonder_gep$"))
rest_volgorde <- sort(setdiff(alle_korte_responsen, gewenste_top_volgorde))

# De complete, vaste lijst
definitieve_volgorde <- c(gewenste_top_volgorde, rest_volgorde)


for(g in groepen) {

  # Factoren en Y-as goedzetten (We gebruiken je definitieve_volgorde van eerder)
  responsen_in_groep <- df_effecten %>% filter(groep == g) %>% pull(Response) %>% str_remove_all("_zonder_gep$") %>% unique()
  volgorde_groep <- definitieve_volgorde[definitieve_volgorde %in% responsen_in_groep]

  data_groep <- plot_data_physico %>%
    filter(groep == g) %>%
    mutate(Korte_Response = factor(Korte_Response, levels = rev(volgorde_groep)))

  p <- ggplot(data_groep, aes(y = Korte_Response)) +
    geom_vline(xintercept = 0, color = "grey40", linetype = "dashed") +

    geom_col(aes(x = Waarde, fill = Effect_Type, text = Hover_Tekst),
             position = position_dodge(width = 0.7), width = 0.6, alpha = 0.85) +

    geom_point(aes(x = Totaal_Effect, shape = "Totaal Netto Effect", text = Hover_Totaal),
               size = 1.5, color = "black", fill = "black") +

    scale_fill_manual(values = c("Direct Effect" = "#2c7bb6", "Indirect Effect" = "#fdae61"), name = "Effecttype") +
    scale_shape_manual(values = c("Totaal Netto Effect" = 18, name = "Effecttype")) +
    scale_y_discrete(drop = FALSE) +

    # Fysicochemie op de rijen, typologie op de kolommen
    facet_grid(Mooie_Physico ~ typologie) +

    theme_minimal() +
    theme(
      panel.border = element_rect(color = "grey40", fill = NA, linewidth = 0.8),
      legend.position = "bottom"
    ) +
    labs(
      title = paste("Effecten van Fysicochemie op Biologie (", toupper(g), ")", sep = ""),
      x = "Gestandaardiseerde Effectgrootte", y = ""
    )

  # Naar Plotly met verborgen legende oplosser
  ply <- ggplotly(p, tooltip = "text") %>%
    plotly::layout(
      margin = list(b = 80, l = 80),
      hoverdistance = 2,
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.1)
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

  physico_plotly_lijst[[g]] <- ply
}

# ==============================================================================
# 4. BEKIJKEN
# ==============================================================================
# Typ in je console om te bekijken:
physico_plotly_lijst[["mafy"]]
# of
physico_plotly_lijst[["mi"]]



#########
# versie 2 met zotte hover
#######"

library(tidyverse)
library(plotly)
library(stringr)

target_physico <- c("zs_log", "czv_log", "o2_s", "n_t_log", "p_t_log", "ec_20_log")
toegestane_bronnen <- c("LG-Natuur", "LG-Verharding", "LG-Landbouw", "Neerslag", "Lozingen en overstorten", "Hydromorfologie")

# ==============================================================================
# 1. ACHTERWAARTS: Sterkste bron, strikt gefilterd op hoofdthema's
# ==============================================================================
sterkste_bronnen <- master_df %>%
  filter(!grepl("~~", Predictor) & !grepl("~~", Response)) %>%
  filter(Response %in% target_physico) %>%
  # Gebruik je bestaande functie om de categorie te bepalen
  mutate(Predictor_Cat = bepaal_groep(Predictor)) %>%
  # Filter uitsluitend op de hoofd-actoren!
  filter(Predictor_Cat %in% toegestane_bronnen) %>%
  group_by(groep, typologie, Response) %>%
  slice_max(order_by = abs(Std.Estimate), n = 1, with_ties = FALSE) %>%
  select(groep, typologie, Predictor_Physico = Response,
         Dom_Bron_Cat = Predictor_Cat, Dom_Bron = Predictor, Bron_Effect = Std.Estimate) %>%
  ungroup()

# ==============================================================================
# 2. VOORWAARTS: Het sterkste indirecte pad (via welke parameter loopt het?)
# ==============================================================================
# We combineren de ketens die we in de dominosteen-stap hadden berekend
dominante_indirecte_paden <- bind_rows(
  keten_1 %>% filter(Bron_Predictor %in% target_physico),
  keten_2 %>% filter(Bron_Predictor %in% target_physico)
) %>%
  group_by(groep, typologie, Bron_Predictor, Eind_Response) %>%
  # Pak het sterkste indirecte pad per fysicochemie-biologie combinatie
  slice_max(order_by = abs(Effect), n = 1, with_ties = FALSE) %>%
  select(groep, typologie, Predictor_Physico = Bron_Predictor,
         Response_Bio = Eind_Response, Dom_Indirect_Pad = Pad) %>%
  ungroup()

# ==============================================================================
# 3. ALLES KOPPELEN EN DE TOOLTIPS BOUWEN
# ==============================================================================
plot_data_physico <- df_effecten %>%
  filter(Predictor %in% target_physico) %>%

  # Koppel de data
  left_join(sterkste_bronnen, by = c("groep", "typologie", "Predictor" = "Predictor_Physico")) %>%
  left_join(dominante_indirecte_paden, by = c("groep", "typologie", "Predictor" = "Predictor_Physico", "Response" = "Response_Bio")) %>%

  mutate(
    Korte_Response = str_remove_all(Response, "_zonder_gep$"),
    Dom_Bron = str_remove_all(replace_na(Dom_Bron, "Geen data"), "_s$|_log$"),
    Dom_Bron_Cat = replace_na(Dom_Bron_Cat, "Onbekend"),
    Bron_Effect = replace_na(Bron_Effect, 0),
    Dom_Indirect_Pad = str_remove_all(replace_na(Dom_Indirect_Pad, ""), "_s$|_log$|_zonder_gep$"),

    Mooie_Physico = case_when(
      Predictor == "zs_log" ~ "Zwevende Stof", Predictor == "czv_log" ~ "CZV",
      Predictor == "o2_s" ~ "Zuurstof", Predictor == "n_t_log" ~ "Stikstof (N)",
      Predictor == "p_t_log" ~ "Fosfor (P)", Predictor == "ec_20_log" ~ "Geleidbaarheid",
      TRUE ~ Predictor
    )
  ) %>%

  pivot_longer(cols = c(Direct_Effect, Indirect_Effect), names_to = "Effect_Type", values_to = "Waarde") %>%
  mutate(Effect_Type = if_else(Effect_Type == "Direct_Effect", "Direct Effect", "Indirect Effect")) %>%

  # De slimme tooltips
  mutate(
    Hover_Tekst = case_when(
      Effect_Type == "Direct Effect" & Waarde != 0 ~ paste0(
        "<b>Direct Effect</b><br>",
        "Effectgrootte: ", round(Waarde, 3), "<br>",
        "-----------------------------<br>",
        "<i>Sterkste hoofddruk op ", Mooie_Physico, ":</i><br>",
        Dom_Bron_Cat, " (", Dom_Bron, ": ", round(Bron_Effect, 3), ")"
      ),
      Effect_Type == "Indirect Effect" & Waarde != 0 ~ paste0(
        "<b>Indirect Effect</b><br>",
        "Netto effectgrootte: ", round(Waarde, 3), "<br>",
        "<i>Loopt dominant via:</i><br>", Dom_Indirect_Pad, "<br>",
        "-----------------------------<br>",
        "<i>Sterkste hoofddruk op ", Mooie_Physico, ":</i><br>",
        Dom_Bron_Cat, " (", Dom_Bron, ": ", round(Bron_Effect, 3), ")"
      ),
      TRUE ~ ""
    ),
    Hover_Totaal = paste0(
      "<b>Netto Effect (", Mooie_Physico, ")</b><br>",
      "Waarde: ", round(Totaal_Effect, 3)
    )
  )

# ==============================================================================
# 4. PLOTS MAKEN (Zelfde methode als voorheen)
# ==============================================================================
physico_plotly_lijst <- list()
groepen <- unique(plot_data_physico$groep)

for(g in groepen) {

  responsen_in_groep <- df_effecten %>% filter(groep == g) %>% pull(Response) %>% str_remove_all("_zonder_gep$") %>% unique()
  volgorde_groep <- definitieve_volgorde[definitieve_volgorde %in% responsen_in_groep]

  data_groep <- plot_data_physico %>%
    filter(groep == g) %>%
    mutate(
      Korte_Response = factor(Korte_Response, levels = rev(volgorde_groep)),
      # ---> VOEG DEZE REGEL TOE OM DE FACET-VOLGORDE AF TE DWINGEN <---
      typologie = factor(typologie, levels = c("Beken", "Kempense beken", "Rivieren", "Polderlopen", "Bovenlopen", "Kunstmatig"))
    )
  p <- ggplot(data_groep, aes(y = Korte_Response)) +
    geom_vline(xintercept = 0, color = "grey40", linetype = "dashed") +
    geom_col(aes(x = Waarde, fill = Effect_Type, text = Hover_Tekst), position = position_dodge(width = 0.7), width = 0.6, alpha = 0.85) +
    geom_point(aes(x = Totaal_Effect, shape = "Totaal Netto Effect", text = Hover_Totaal), size = 1.5, color = "black", fill = "black") +

    scale_fill_manual(values = c("Direct Effect" = "#2c7bb6", "Indirect Effect" = "#fdae61"), name = "Effecttype") +
    scale_shape_manual(values = c("Totaal Netto Effect" = 18), name = NULL) +
    scale_y_discrete(drop = FALSE) +

    facet_grid(Mooie_Physico ~ typologie) +
    theme_minimal() +
    theme(panel.border = element_rect(color = "grey40", fill = NA, linewidth = 0.8), legend.position = "bottom",
          axis.text.y = element_text(size = 12, face = "bold", color = "black"),
          axis.text.x = element_text(size = 10),
          strip.text.x = element_text(face = "bold", size = 12, margin = margin(t = 5, b = 5)),
          strip.text.y = element_text(face = "bold", size = 10)) +
    labs(title = paste("Effecten van Fysicochemie op Biologie (", toupper(g), ")", sep = ""), x = "Gestandaardiseerde Effectgrootte", y = "")

  ply <- ggplotly(p, tooltip = "text") %>%
    plotly::layout(margin = list(b = 80, l = 80), hoverdistance = 2, legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.1))

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
      } else { ply$x$data[[i]]$showlegend <- FALSE }
    } else { ply$x$data[[i]]$showlegend <- FALSE }
  }
  physico_plotly_lijst[[g]] <- ply
}

# Typ dit om de magie te zien:
physico_plotly_lijst[["mi"]]
physico_plotly_lijst[["mafy"]]

# Sla de MAFY-plot op
saveWidget(
  widget = physico_plotly_lijst[["mafy"]],
  file = "output/figuren/synthese_fysico_MAFY.html",
  selfcontained = TRUE
)

# Sla de MI-plot op
saveWidget(
  widget = physico_plotly_lijst[["mi"]],
  file = "output/figuren/synthese_fysico_MI.html",
  selfcontained = TRUE
)

##### figuur met enkel de indexen samen!

# ==============================================================================
# 1. DATA FILTEREN VOOR DE HOOFDGRAADMETERS (Fysicochemie)
# ==============================================================================
data_physico_gecombineerd <- plot_data_physico %>%
  # Filter uitsluitend op de master-indexen
  filter(Korte_Response %in% c("mmif", "index_nieuw")) %>%
  # Zet de factor-volgorde vast (reversed voor de Y-as)
  mutate(Korte_Response = factor(Korte_Response, levels = rev(c("mmif", "index_nieuw"))))

# ==============================================================================
# 2. DE GECOMBINEERDE GGPLOT MAKEN
# ==============================================================================
p_physico_comb <- ggplot(data_physico_gecombineerd, aes(y = Korte_Response)) +
  geom_vline(xintercept = 0, color = "grey40", linetype = "dashed") +

  geom_col(aes(x = Waarde, fill = Effect_Type, text = Hover_Tekst),
           position = position_dodge(width = 0.7), width = 0.6, alpha = 0.85) +

  geom_point(aes(x = Totaal_Effect, shape = "Totaal Netto Effect", text = Hover_Totaal),
             size = 1.5, color = "black", fill = "black") +

  scale_fill_manual(values = c("Direct Effect" = "#2c7bb6", "Indirect Effect" = "#fdae61"), name = "Effecttype") +
  scale_shape_manual(values = c("Totaal Netto Effect" = 18), name = NULL) +

  # Fysicochemie op de rijen, typologie op de kolommen
  facet_grid(Mooie_Physico ~ typologie) +

  theme_minimal() +
  theme(
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 0.8),
    legend.position = "bottom"
  ) +
  labs(
    title = "Effecten van Fysicochemie op de Hoofdgraadmeters (MMIF & Index_Nieuw)",
    x = "Gestandaardiseerde Effectgrootte",
    y = ""
  )

# ==============================================================================
# 3. OMZETTEN NAAR PLOTLY & LEGENDE OPSCHONEN
# ==============================================================================
ply_physico_comb <- ggplotly(p_physico_comb, tooltip = "text") %>%
  plotly::layout(
    margin = list(b = 80, l = 80),
    hoverdistance = 2,
    legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.1)
  )

toegevoegd_aan_legende <- c()

for (i in seq_along(ply_physico_comb$x$data)) {
  huidige_naam <- ply_physico_comb$x$data[[i]]$name
  echte_naam <- stringr::str_extract(huidige_naam, "Direct Effect|Indirect Effect|Totaal Netto Effect")

  if (!is.na(echte_naam)) {
    ply_physico_comb$x$data[[i]]$name <- echte_naam
    ply_physico_comb$x$data[[i]]$legendgroup <- echte_naam

    if (!(echte_naam %in% toegevoegd_aan_legende)) {
      ply_physico_comb$x$data[[i]]$showlegend <- TRUE
      toegevoegd_aan_legende <- c(toegevoegd_aan_legende, echte_naam)
    } else {
      ply_physico_comb$x$data[[i]]$showlegend <- FALSE
    }
  } else {
    ply_physico_comb$x$data[[i]]$showlegend <- FALSE
  }
}

# ==============================================================================
# 4. BEKIJKEN EN OPSLAAN
# ==============================================================================
# Bekijk hem direct in RStudio:
ply_physico_comb

# Sla hem op als HTML (haal de hashtag weg om uit te voeren):
saveWidget(ply_physico_comb, file = "output/figuren/synthese_fysicochemie.html", selfcontained = TRUE)

