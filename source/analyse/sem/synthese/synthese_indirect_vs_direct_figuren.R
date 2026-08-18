library(tidyverse)

# ==============================================================================
# STAP 1: Haal de directe effecten op de biologie eruit
# ==============================================================================
directe_effecten <- master_df %>%
  filter(!grepl("~~", Predictor) & !grepl("~~", Response)) %>%
  filter(Response %in% biologische_vars) %>%
  select(groep, typologie, Predictor, Response, Direct_Effect = Estimate)

# ==============================================================================
# STAP 2: Bepaal de abiotische paden (Milieu ➔ Milieu)
# ==============================================================================
abiotische_paden <- master_df %>%
  filter(!grepl("~~", Predictor) & !grepl("~~", Response)) %>%
  filter(!Response %in% biologische_vars) %>%
  select(groep, typologie, Bron_Predictor = Predictor, Intermediair = Response, Abio_Estimate = Estimate)

# ==============================================================================
# STAP 3: Koppel deze aan de paden die naar de biologie gaan (Intermediair ➔ Biologie)
# ==============================================================================
bio_paden <- master_df %>%
  filter(!grepl("~~", Predictor) & !grepl("~~", Response)) %>%
  filter(Response %in% biologische_vars) %>%
  select(groep, typologie, Intermediair = Predictor, Bio_Response = Response, Bio_Estimate = Estimate)

# ==============================================================================
# STAP 4: Bereken en sommeer de indirecte effecten
# ==============================================================================
indirecte_effecten <- abiotische_paden %>%
  # Zoek matches waar het abiotische eindpunt overlapt met het biologische startpunt
  inner_join(bio_paden, by = c("groep", "typologie", "Intermediair")) %>%

  # Het indirecte effect is de vermenigvuldiging van de twee pad-coëfficiënten
  mutate(Indirect_Deel_Effect = Abio_Estimate * Bio_Estimate) %>%

  # Soms loopt een bron via meerdere routes (bijv. via O2 én Nutriënten) naar dezelfde maatlat.
  # We tellen deze op voor het totale indirecte effect van die bron.
  group_by(groep, typologie, Predictor = Bron_Predictor, Response = Bio_Response) %>%
  summarise(Indirect_Effect = sum(Indirect_Deel_Effect), .groups = "drop")

# ==============================================================================
# STAP 5: Combineer alles tot de definitieve df_effecten tabel
# ==============================================================================
df_effecten <- directe_effecten %>%
  # Full join zorgt dat we paden behouden die ofwel alléén direct, ofwel alléén indirect zijn
  full_join(indirecte_effecten, by = c("groep", "typologie", "Predictor", "Response")) %>%

  # Vervang NA's door 0 (als een pad geen direct effect heeft, is dat wiskundig een 0)
  mutate(
    Direct_Effect = replace_na(Direct_Effect, 0),
    Indirect_Effect = replace_na(Indirect_Effect, 0),
    Totaal_Effect = Direct_Effect + Indirect_Effect
  ) %>%

  # Voeg je vertrouwde categorieën toe!
  mutate(Predictor_Cat = bepaal_groep(Predictor)) %>%

  # Verwijder paden die helemaal geen effect blijken te hebben
  filter(Direct_Effect != 0 | Indirect_Effect != 0)

# Bekijk even of het gelukt is:
head(df_effecten %>% arrange(desc(abs(Totaal_Effect))), 10)


# ==============================================================================
# 2. DATA PREPARATIE VOOR GGPLOT
# ==============================================================================
plot_data <- df_effecten %>%
  # Filter alleen op de top-categorieën die jij interessant vindt
  filter(Predictor_Cat %in% c("Landgebruik", "Klimaat", "Lozingen en overstorten", "Hydromorfologie")) %>%

  # Voor ggplot moeten we de Directe en Indirecte kolommen 'lang' maken
  pivot_longer(
    cols = c(Direct_Effect, Indirect_Effect),
    names_to = "Effect_Type",
    values_to = "Waarde"
  ) %>%
  # Schone namen voor de legenda
  mutate(Effect_Type = case_when(
    Effect_Type == "Direct_Effect" ~ "Direct Effect",
    Effect_Type == "Indirect_Effect" ~ "Indirect Effect"
  )) %>%
  # Maak een logisch label voor de Y-as (Stressor -> Maatlat)
  mutate(Y_Label = paste(Predictor, "➔", Response))


# ==============================================================================
# 3. DE GRAFIEK GENEREREN
# ==============================================================================
plot_componenten <- ggplot(plot_data, aes(y = Y_Label)) +

  # 1. Teken een subtiele verticale lijn op 0 (geen effect)
  geom_vline(xintercept = 0, color = "grey50", linetype = "dashed", linewidth = 0.8) +

  # 2. De balken voor Direct en Indirect (naast elkaar gezet met position_dodge)
  geom_col(aes(x = Waarde, fill = Effect_Type),
           position = position_dodge(width = 0.7),
           width = 0.6, alpha = 0.85) +

  # 3. De marker voor het Totale (Netto) effect
  geom_point(aes(x = Totaal_Effect, shape = "Totaal Netto Effect"),
             size = 4, color = "black", fill = "black") +

  # Kleuren instellen: Blauw/Paars tinten werken hier academisch heel goed
  scale_fill_manual(values = c("Direct Effect" = "#2c7bb6", "Indirect Effect" = "#fdae61"),
                    name = "Pijlers van de pSEM:") +
  scale_shape_manual(values = c("Totaal Netto Effect" = 18), # 18 is een ruit (diamond)
                     name = "") +

  # Splitsen per groep en typologie, net als je heatmaps!
  facet_nested(Predictor_Cat ~ groep + typologie, scales = "free_y", space = "free_y") +

  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),

    # Headers opmaken (identiek aan je heatmaps)
    strip.text.x = element_text(face = "bold", size = 11, margin = margin(t = 4, b = 4)),
    strip.text.y = element_text(face = "bold", size = 10, angle = 0),
    strip.background.x = element_rect(fill = "grey90", color = "grey40", linewidth = 0.8),
    strip.background.y = element_rect(fill = "grey95", color = NA),

    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 0.8),

    legend.position = "bottom",
    legend.box = "horizontal"
  ) +
  labs(
    title = "Systeem-analyse: Directe vs. Indirecte Effecten",
    subtitle = "Hoe sturende brondrukken doorwerken op het biologisch eindpunt",
    x = "Gestandaardiseerde Effectgrootte",
    y = "Specifieke Relatie (Stressor ➔ Eindpunt)"
  )

print(plot_componenten)

# Propere figuur met ES filter etc

# ==============================================================================
# 1. DATA FILTEREN EN OPSCHONEN
# ==============================================================================
# Bepaal je drempelwaarde. Effecten tussen -0.15 en 0.15 worden weggefilterd.
drempelwaarde <- 0.05

plot_data_clean <- df_effecten %>%
  # Filter 1: Behoud alleen jouw geselecteerde top-categorieën
  filter(Predictor_Cat %in% c("Landgebruik", "Klimaat", "Lozingen en overstorten", "Hydromorfologie")) %>%

  # Filter 2: De strenge ecologische selectie!
  # We tonen het pad als het Totale effect óf het Directe effect de drempel haalt.
  filter(abs(Totaal_Effect) >= drempelwaarde | abs(Direct_Effect) >= drempelwaarde) %>%

  # Filter 3: Maak de Y-as labels veel korter en leesbaarder
  mutate(
    Korte_Predictor = str_remove_all(Predictor, "_s$|_log$"),
    Korte_Response = str_remove_all(Response, "_zonder_gep$"),
    Y_Label = paste(Korte_Predictor, "➔", Korte_Response)
  ) %>%

  # Maak de dataset 'lang' voor ggplot (splits Direct en Indirect)
  pivot_longer(
    cols = c(Direct_Effect, Indirect_Effect),
    names_to = "Effect_Type",
    values_to = "Waarde"
  ) %>%
  mutate(Effect_Type = case_when(
    Effect_Type == "Direct_Effect" ~ "Direct Effect",
    Effect_Type == "Indirect_Effect" ~ "Indirect Effect"
  ))


# ==============================================================================
# 2. FIGUREN GENEREREN PER CATEGORIE
# ==============================================================================
# Haal de overgebleven categorieën uit je schone dataset
categorieen <- unique(plot_data_clean$Predictor_Cat)

# R maakt nu voor elke categorie een aparte grafiek aan
for(cat in categorieen) {

  # Selecteer de data voor deze specifieke loop
  data_cat <- plot_data_clean %>% filter(Predictor_Cat == cat)

  plot_cat <- ggplot(data_cat, aes(y = Y_Label)) +

    # 1. Referentielijn op 0
    geom_vline(xintercept = 0, color = "grey40", linetype = "dashed", linewidth = 0.8) +

    # 2. De gestapelde/naast elkaar geplaatste balken
    geom_col(aes(x = Waarde, fill = Effect_Type),
             position = position_dodge(width = 0.7),
             width = 0.6, alpha = 0.85) +

    # 3. Het netto totaal-effect
    geom_point(aes(x = Totaal_Effect, shape = "Totaal Netto Effect"),
               size = 3.5, color = "black", fill = "black") +

    # Kleuren en symbolen
    scale_fill_manual(values = c("Direct Effect" = "#2c7bb6", "Indirect Effect" = "#fdae61"),
                      name = "Pijlers van de pSEM:") +
    scale_shape_manual(values = c("Totaal Netto Effect" = 18), name = "") +

    # 4. De facet indeling (Let op: Predictor_Cat is hier eruit, want dat is nu de titel!)
    facet_nested(~ groep + typologie) +

    # 5. X-as opschonen: R kiest nu automatisch slimme ronde getallen (breaks)
    scale_x_continuous(breaks = scales::pretty_breaks(n = 4)) +

    theme_minimal(base_size = 12) +
    theme(
      # Achtergrond en rasters opschonen
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),

      # Tekst kleiner en strakker voor de leesbaarheid
      axis.text.y = element_text(size = 9, color = "black"),
      axis.text.x = element_text(size = 9),

      # Mooie headers (net als bij je eerdere heatmaps)
      strip.text.x = element_text(face = "bold", size = 11, margin = margin(t = 5, b = 5)),
      strip.background.x = element_rect(fill = "grey90", color = "grey40", linewidth = 0.8),

      # Kaders en witruimte
      panel.border = element_rect(color = "grey40", fill = NA, linewidth = 0.8),
      panel.spacing.x = unit(0.3, "cm"), # Geeft de typologieën wat ademruimte

      legend.position = "bottom",
      legend.box = "horizontal"
    ) +
    labs(
      title = paste("Systeem-analyse:", cat),
      subtitle = paste("Alleen de sturende effecten (Effectgrootte >", drempelwaarde, ")"),
      x = "Gestandaardiseerde Effectgrootte",
      y = ""
    )

  # Print de grafiek naar je Plots venster
  print(plot_cat)
}


# figuur splitsen MI en MAFY

library(tidyverse)
library(ggplot2)
library(ggh4x)
library(stringr)

# ==============================================================================
# 1. DATA FILTEREN EN OPSCHONEN
# ==============================================================================
# Bepaal de drempelwaarde voor ecologische relevantie
drempelwaarde <- 0.02

plot_data_clean <- df_effecten %>%
  # Filter op de top-categorieën
  filter(Predictor_Cat %in% c("Landgebruik", "Klimaat", "Lozingen en overstorten", "Hydromorfologie")) %>%

  # De strenge selectie: behoud enkel robuuste effecten
  filter(abs(Totaal_Effect) >= drempelwaarde | abs(Direct_Effect) >= drempelwaarde) %>%

  # Assen opschonen: achtervoegsels weghalen
  mutate(
    Korte_Predictor = str_remove_all(Predictor, "_s$|_log$"),
    Korte_Response = str_remove_all(Response, "_zonder_gep$"),
    Y_Label = paste(Korte_Predictor, "➔", Korte_Response)
  ) %>%

  # Data 'lang' maken voor de staafjes
  pivot_longer(
    cols = c(Direct_Effect, Indirect_Effect),
    names_to = "Effect_Type",
    values_to = "Waarde"
  ) %>%
  mutate(Effect_Type = case_when(
    Effect_Type == "Direct_Effect" ~ "Direct Effect",
    Effect_Type == "Indirect_Effect" ~ "Indirect Effect"
  ))

# ==============================================================================
# 2. FIGUREN GENEREREN PER SOORTENgroep (MAFY vs MI)
# ==============================================================================
# Bepaal welke groepen in de gefilterde dataset zitten
groepen <- unique(plot_data_clean$groep)

# De loop maakt nu een plot voor MAFY en een aparte plot voor MI
for(g in groepen) {

  # Selecteer uitsluitend de data voor deze soortengroep
  data_groep <- plot_data_clean %>% filter(groep == g)

  plot_groep <- ggplot(data_groep, aes(y = Y_Label)) +

    # 1. Referentielijn op 0
    geom_vline(xintercept = 0, color = "grey40", linetype = "dashed", linewidth = 0.8) +

    # 2. Directe vs. Indirecte balken
    geom_col(aes(x = Waarde, fill = Effect_Type),
             position = position_dodge(width = 0.7),
             width = 0.6, alpha = 0.85) +

    # 3. Het netto totaal-effect met een ruitje
    geom_point(aes(x = Totaal_Effect, shape = "Totaal Netto Effect"),
               size = 3.5, color = "black", fill = "black") +

    # Kleuren en legenda
    scale_fill_manual(values = c("Direct Effect" = "#2c7bb6", "Indirect Effect" = "#fdae61"),
                      name = "Pijlers van de pSEM:") +
    scale_shape_manual(values = c("Totaal Netto Effect" = 18), name = "") +

    # 4. DE NIEUWE INDELING: Categorieën als rijen, typologieën als kolommen
    facet_nested(Predictor_Cat ~ typologie, scales = "free_y", space = "free_y") +

    # 5. X-as slim schalen
    scale_x_continuous(breaks = scales::pretty_breaks(n = 4)) +

    theme_minimal(base_size = 12) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),

      axis.text.y = element_text(size = 9, color = "black"),
      axis.text.x = element_text(size = 9),

      # Headers opmaken
      strip.text.x = element_text(face = "bold", size = 11, margin = margin(t = 5, b = 5)),
      strip.text.y = element_text(face = "bold", size = 10, angle = 0),
      strip.background.x = element_rect(fill = "grey90", color = "grey40", linewidth = 0.8),
      strip.background.y = element_rect(fill = "grey95", color = NA),

      # Kaders en witruimte
      panel.border = element_rect(color = "grey40", fill = NA, linewidth = 0.8),
      panel.spacing.x = unit(0.3, "cm"),
      panel.spacing.y = unit(0.1, "cm"),

      legend.position = "bottom",
      legend.box = "horizontal"
    ) +
    labs(
      title = paste("Systeem-analyse: Directe vs. Indirecte Effecten (", toupper(g), ")", sep = ""),
      subtitle = paste("Overzicht per stressor-categorie (Effectgrootte >", drempelwaarde, ")"),
      x = "Gestandaardiseerde Effectgrootte",
      y = ""
    )

  # Print de figuur naar het scherm
  print(plot_groep)
}

# figuur met samengevoegde categorieeffecten

library(tidyverse)
library(ggplot2)
library(ggh4x)
library(stringr)

# ==============================================================================
# 1. DATA AGGREGEREN PER CATEGORIE
# ==============================================================================
df_categorie_effecten <- df_effecten %>%
  # Filter alleen jouw gewenste hoofdthema's
  filter(Predictor_Cat %in% c("Landgebruik", "Klimaat", "Lozingen en overstorten", "Hydromorfologie")) %>%

  # groepeer op de categorie in plaats van de individuele stressor!
  group_by(groep, typologie, Predictor_Cat, Response) %>%
  summarise(
    Categorie_Direct = sum(Direct_Effect, na.rm = TRUE),
    Categorie_Indirect = sum(Indirect_Effect, na.rm = TRUE),
    .groups = "drop"
  ) %>%

  # Bereken het nieuwe netto totaal voor de hele categorie
  mutate(Categorie_Totaal = Categorie_Direct + Categorie_Indirect) %>%

  # Y-as labels opschonen (we hebben de predictor niet meer nodig in het label!)
  mutate(Korte_Response = str_remove_all(Response, "_zonder_gep$")) %>%

  # Data 'lang' maken voor ggplot
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
# 2. DE OVERZICHTSFIGUREN GENEREREN (MAFY vs MI)
# ==============================================================================
groepen <- unique(df_categorie_effecten$groep)

for(g in groepen) {

  data_groep <- df_categorie_effecten %>% filter(groep == g)

  plot_groep <- ggplot(data_groep, aes(y = Korte_Response)) +

    geom_vline(xintercept = 0, color = "grey40", linetype = "dashed", linewidth = 0.8) +

    # Gestapelde/naast elkaar geplaatste balken per categorie
    geom_col(aes(x = Waarde, fill = Effect_Type),
             position = position_dodge(width = 0.7),
             width = 0.6, alpha = 0.85) +

    # Totaal effect ruitje
    geom_point(aes(x = Categorie_Totaal, shape = "Totaal Netto Effect"),
               size = 3.5, color = "black", fill = "black") +

    scale_fill_manual(values = c("Direct Effect" = "#2c7bb6", "Indirect Effect" = "#fdae61"),
                      name = "Geaggregeerde Systeemdruk:") +
    scale_shape_manual(values = c("Totaal Netto Effect" = 18), name = "") +

    # Categorieën als rijen, typologieën als kolommen
    facet_nested(Predictor_Cat ~ typologie, scales = "free_y", space = "free_y") +

    scale_x_continuous(breaks = scales::pretty_breaks(n = 4)) +

    theme_minimal(base_size = 12) +
    theme(
      panel.grid.major.y = element_line(color = "grey90", linetype = "dotted"),
      panel.grid.minor.x = element_blank(),

      axis.text.y = element_text(size = 10, face = "bold", color = "black"),
      axis.text.x = element_text(size = 9),

      strip.text.x = element_text(face = "bold", size = 12, margin = margin(t = 5, b = 5)),
      strip.text.y = element_text(face = "bold", size = 11, angle = 0),
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
