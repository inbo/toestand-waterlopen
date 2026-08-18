master_df %>% filter(groep == "mi" & Predictor == "intensiteit_combo_afstr_s" & Response %in% biologische_vars ) %>%View
master_df %>% filter(groep == "mi" & Predictor == "n_t_log" & Response %in% biologische_vars ) %>%View


master_df %>% filter(groep == "mafy" & Predictor == "ekc2_waterlichaam_s" & Response %in% biologische_vars ) %>%View


library(tidyverse)
library(ggplot2)

# ==============================================================================
# GRAFIEK 1: De Abiotische Heatmap (Milieu -> Milieu)
# ==============================================================================

# Data prepareren
heatmap_data_abiotisch <- master_df %>%
  filter(!Response %in% biologische_vars) %>%
  # Ontdubbelen voor de abiotiek
  distinct(groep, typologie, Predictor, Response, .keep_all = TRUE) %>%
  # Maak een mooie, leesbare label voor de Y-as
  mutate(Pathway = paste(Predictor, "➔", Response)) %>%
  select(groep, typologie, Pathway, Estimate) %>%
  # HIER GEBEURT DE MAGIE: Vul alle ontbrekende combinaties aan met NA
  complete(Pathway, nesting(groep, typologie))

# De plot maken
plot_abiotisch <- ggplot(heatmap_data_abiotisch, aes(x = typologie, y = Pathway, fill = Estimate)) +
  geom_tile(color = "grey80", size = 0.5) + # Tekent de blokjes met een grijs randje
  # Kleurenschaal: Rood (negatief), Wit (0 of NA), Blauw (positief)
  scale_fill_gradient2(
    low = "#B2182B", mid = "white", high = "#2166AC",
    midpoint = 0, na.value = "white",
    name = "Gestandaardiseerd\nEffect"
  ) +
  facet_wrap(~ groep, scales = "free_x") + # Splits de grafiek in MI en MAFY
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    panel.grid = element_blank(), # Haal achtergrondlijnen weg
    strip.text = element_text(face = "bold", size = 14)
  ) +
  labs(
    title = "Abiotische Paden (Milieu ➔ Milieu)",
    x = "typologie",
    y = "Ecologisch Pad"
  )

print(plot_abiotisch)


# ==============================================================================
# GRAFIEK 2: De Biologische Heatmap (Milieu ➔ Maatlat)
# ==============================================================================

# Data prepareren
heatmap_data_biotisch <- master_df %>%
  filter(Response %in% biologische_vars) %>%
  # Maak unieke labels voor de X-as (typologie + Maatlat) en Y-as (Stressor)
  mutate(
    Model_Naam = paste(typologie, Response, sep = " - "),
    Pathway = Predictor
  ) %>%
  select(groep, Model_Naam, typologie, Pathway, Estimate) %>%
  # Vul alle ontbrekende combinaties aan met NA
  complete(Pathway, nesting(groep, Model_Naam, typologie))

# De plot maken
plot_biotisch <- ggplot(heatmap_data_biotisch, aes(x = Model_Naam, y = Pathway, fill = Estimate)) +
  geom_tile(color = "grey80", size = 0.5) +
  scale_fill_gradient2(
    low = "#B2182B", mid = "white", high = "#2166AC",
    midpoint = 0, na.value = "white",
    name = "Gestandaardiseerd\nEffect"
  ) +
  facet_wrap(~ groep, scales = "free_x") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold", size = 14)
  ) +
  labs(
    title = "Biologische Reacties (Stressor ➔ Maatlat)",
    x = "Model (typologie - Deelmaatlat)",
    y = "Voorspeller (Stressor)"
  )

print(plot_biotisch)


########corfouten
library(tidyverse)

# Data prepareren ZONDER gecorreleerde fouten
heatmap_data_abiotisch <- master_df %>%
  filter(!Response %in% biologische_vars) %>%

  # --- DE SLIMME FILTER ---
  # Gooi elke rij weg waar ergens "~~" in de Predictor of Response staat
  filter(!str_detect(Predictor, "~~") & !str_detect(Response, "~~")) %>%
  # ------------------------

distinct(groep, typologie, Predictor, Response, .keep_all = TRUE) %>%
  mutate(Pathway = paste(Predictor, "➔", Response)) %>%
  select(groep, typologie, Pathway, Estimate) %>%
  complete(Pathway, nesting(groep, typologie))

# En dan gewoon weer de plot maken!
plot_abiotisch <- ggplot(heatmap_data_abiotisch, aes(x = typologie, y = Pathway, fill = Estimate)) +
  geom_tile(color = "grey80", size = 0.5) +
  scale_fill_gradient2(
    low = "#B2182B", mid = "white", high = "#2166AC",
    midpoint = 0, na.value = "white",
    name = "Gestandaardiseerd\nEffect"
  ) +
  facet_wrap(~ groep, scales = "free_x") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold", size = 14)
  ) +
  labs(
    title = "Abiotische Causale Paden (Milieu ➔ Milieu)",
    x = "typologie",
    y = "Ecologisch Pad"
  )

print(plot_abiotisch)


################""
# figuren categorien
####################"

library(tidyverse)
library(ggplot2)

# ==============================================================================
# HULPFUNCTIE: Categorieën toewijzen
# ==============================================================================
# We definiëren dit eenmalig zodat we het makkelijk op Predictor en Response kunnen toepassen
# 1. De CORRECTE lijst met biologische eindpunten (inclusief de _zonder_gep namen)
biologische_vars <- c("mmif", "ept_prop", "sw_dw", "ta_xw", "index_nieuw", "gv_zonder_gep", "v_zonder_gep", "vo_zonder_gep", "ts_zonder_gep")

# 2. De Categorieën-functie (een slimmigheidje: we gebruiken gewoon die lijst van hierboven!)
bepaal_groep <- function(id) {
  case_when(
    id %in% c("spei6_s", "p_sum_7d_s", "n_extreme_3m_s") ~ "Klimaat",
    id %in% c("lozingen_rwzi_ie_log", "lozingen_industrie_ie_log", "lozingen_riool_ie_log",
              "overstorten_blootstelling_index_log", "overstorten_index_log", "lozingen_rwzi_p_t_log") ~ "Lozingen en overstorten",
    id %in% c("czv_log", "o2_s", "o2_verz_s", "ec_20_s", "zs_log", "t_s", "ec_20_log", "cl_s", "p_h_s") ~ "Fysico-chemie",
    id %in% c("ekc2_waterlichaam_s", "ekc2_traject_s", "breedte_diepte_ratio_s", "sinuositeit_s",
              "bodemsub_s", "doodhout_s", "profiel_s", "stroomsnelheid_s") ~ "Hydromorfologie",
    id %in% c("n_t_log", "p_t_log", "no2_log", "no3_log", "nh4_log") ~ "Nutriënten",
    id %in% c("intensiteit_combo_afstr_s", "verharding_afstr_s", "natuur_afstr_s",
              "intensiteit_combo_oeverzone_s", "intensiteit_combo_oever_s", "natuur_oever_s", "verharding_oever_s", "perc_schaduw_s") ~ "Landgebruik",

    # Als het in onze biologische lijst staat, is het een ecologische respons
    id %in% biologische_vars ~ "Ecologische respons",
    TRUE ~ "Overig"
  )
}



# ==============================================================================
# GRAFIEK 1: De Abiotische Heatmap (Geordend op categorie)
# ==============================================================================

heatmap_data_abiotisch <- master_df %>%
  filter(!Response %in% biologische_vars) %>%
  # Filter gecorreleerde fouten eruit
  filter(!str_detect(Predictor, "~~") & !str_detect(Response, "~~")) %>%
  distinct(groep, typologie, Predictor, Response, .keep_all = TRUE) %>%

  # Ken de groepen toe
  mutate(
    Predictor_Cat = bepaal_groep(Predictor),
    Response_Cat = bepaal_groep(Response),
    Pathway = paste(Predictor, "➔", Response)
  ) %>%
  # Fixeer de volgorde van de categorieën (anders doet ggplot dit alfabetisch)
  mutate(Predictor_Cat = factor(Predictor_Cat, levels = c(
    "Klimaat", "Landgebruik", "Lozingen en overstorten",
    "Hydromorfologie", "Nutriënten", "Fysico-chemie", "Overig"
  ))) %>%
  select(groep, typologie, Pathway, Predictor_Cat, Estimate) %>%
  complete(Pathway, nesting(groep, typologie, Predictor_Cat))

plot_abiotisch <- ggplot(heatmap_data_abiotisch, aes(x = typologie, y = Pathway, fill = Estimate)) +
  geom_tile(color = "grey90", size = 0.3) +
  scale_fill_gradient2(
    low = "#B2182B", mid = "white", high = "#2166AC",
    midpoint = 0, na.value = "white",
    name = "Standaard\nEffect"
  ) +
  # facet_grid maakt een matrix: rijen zijn de Predictor-categorieën, kolommen de soortengroepen
  # scales = "free_y" & space = "free_y" zorgt ervoor dat lege rijen netjes verdwijnen en de blokjes even groot blijven
  facet_grid(Predictor_Cat ~ groep, scales = "free_y", space = "free_y") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    panel.grid = element_blank(),
    strip.text.x = element_text(face = "bold", size = 14),
    strip.text.y = element_text(face = "bold", size = 10, angle = 0), # Horizontale tekst voor categorieën!
    strip.background.y = element_rect(fill = "grey95", color = NA),
    panel.border = element_rect(color = "grey80", fill = NA)
  ) +
  labs(
    title = "Abiotische Paden (Invloed op het Milieu)",
    subtitle = "Gegroepeerd per brontype van de voorspeller",
    x = "typologie",
    y = "" # Leeg gelaten omdat de strip-teksten dit al uitleggen
  )

print(plot_abiotisch)


# ==============================================================================
# GRAFIEK 2: De Biologische Heatmap (Gerepareerde Y-as)
# ==============================================================================

heatmap_data_biotisch <- master_df %>%
  filter(Response %in% biologische_vars) %>%
  filter(!str_detect(Predictor, "~~")) %>%
  mutate(
    Predictor_Cat = bepaal_groep(Predictor),
    Model_Naam = paste(typologie, Response, sep = " - "),
    Pathway = Predictor
  ) %>%
  mutate(Predictor_Cat = factor(Predictor_Cat, levels = c(
    "Klimaat", "Landgebruik", "Lozingen en overstorten",
    "Hydromorfologie", "Nutriënten", "Fysico-chemie", "Overig"
  ))) %>%
  select(groep, Model_Naam, typologie, Pathway, Predictor_Cat, Estimate) %>%

  # --- DE FIX ---
  # Door Pathway en Predictor_Cat samen te nesten, blijven de vars strikt in hun eigen categorie!
  complete(nesting(Pathway, Predictor_Cat), nesting(groep, Model_Naam, typologie))
# --------------

plot_biotisch <- ggplot(heatmap_data_biotisch, aes(x = Model_Naam, y = Pathway, fill = Estimate)) +
  geom_tile(color = "grey90", linewidth = 0.3) + # Let op: size is in nieuwere ggplot-versies linewidth geworden
  scale_fill_gradient2(
    low = "#B2182B", mid = "white", high = "#2166AC",
    midpoint = 0, na.value = "white",
    name = "Standaard\nEffect"
  ) +
  # Gebruik specifiek free_y in plaats van free, zodat de X-assen netjes synchroon blijven
  facet_grid(Predictor_Cat ~ groep, scales = "free_y", space = "free_y") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
    axis.text.y = element_text(size = 10), # Maakt de Y-as labels goed leesbaar
    panel.grid = element_blank(),
    strip.text.x = element_text(face = "bold", size = 14),
    strip.text.y = element_text(face = "bold", size = 10, angle = 0),
    strip.background.y = element_rect(fill = "grey95", color = NA),
    panel.border = element_rect(color = "grey80", fill = NA)
  ) +
  labs(
    title = "Biologische Reacties",
    subtitle = "Directe effecten op flora en fauna per stressor-categorie",
    x = "Model (typologie - Deelmaatlat)",
    y = ""
  )

print(plot_biotisch)


# ==============================================================================
# GRAFIEK 2: De Biologische Heatmap (Biotisch) - NU MET ZUIVERE X-ASSEN
# ==============================================================================
heatmap_data_biotisch <- master_df %>%
  filter(!grepl("~~", Predictor) & !grepl("~~", Response)) %>%
  filter(Response %in% biologische_vars) %>%
  mutate(
    Predictor_Cat = bepaal_groep(Predictor),
    Model_Naam = paste(typologie, Response, sep = " - "),
    Pathway = Predictor
  ) %>%
  mutate(Predictor_Cat = factor(Predictor_Cat, levels = c(
    "Klimaat", "Landgebruik", "Lozingen en overstorten",
    "Hydromorfologie", "Nutriënten", "Fysico-chemie", "Overig"
  ))) %>%
  select(groep, Model_Naam, typologie, Pathway, Predictor_Cat, Estimate) %>%
  drop_na(groep, Model_Naam, typologie, Pathway, Predictor_Cat) %>%

  # --- DE OPLOSSING ---
  # groepeer per 'mafy' of 'mi' vóórdat je gaat aanvullen, zodat ze elkaars deelmaatlatten negeren!
  group_by(groep) %>%
  complete(nesting(Pathway, Predictor_Cat), nesting(Model_Naam, typologie)) %>%
  ungroup()
# --------------------

plot_biotisch <- ggplot(heatmap_data_biotisch, aes(x = Model_Naam, y = Pathway, fill = Estimate)) +
  geom_tile(color = "grey90", linewidth = 0.3) +
  scale_fill_gradient2(
    low = "#B2182B", mid = "white", high = "#2166AC",
    midpoint = 0, na.value = "white",
    name = "Standaard\nEffect"
  ) +
  facet_grid(Predictor_Cat ~ groep, scales = "free", space = "free") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    panel.grid = element_blank(),
    strip.text.x = element_text(face = "bold", size = 14),
    strip.text.y = element_text(face = "bold", size = 10, angle = 0),
    strip.background.y = element_rect(fill = "grey95", color = NA),
    panel.border = element_rect(color = "grey80", fill = NA)
  ) +
  labs(
    title = "Biologische Reacties",
    subtitle = "Directe effecten op flora en fauna per stressor-categorie",
    x = "Model (typologie - Deelmaatlat)",
    y = ""
  )

print(plot_biotisch)

# ==============================================================================
# GRAFIEK 2: De Biologische Heatmap (Met typologie-indeling & Dikke lijnen)
# ==============================================================================
heatmap_data_biotisch <- master_df %>%
  filter(!grepl("~~", Predictor) & !grepl("~~", Response)) %>%
  filter(Response %in% biologische_vars) %>%
  mutate(
    Predictor_Cat = bepaal_groep(Predictor),
    Pathway = Predictor
  ) %>%
  mutate(Predictor_Cat = factor(Predictor_Cat, levels = c(
    "Klimaat", "Landgebruik", "Lozingen en overstorten",
    "Hydromorfologie", "Nutriënten", "Fysico-chemie", "Overig"
  ))) %>%
  # Let op: we gebruiken nu gewoon 'Response' voor de X-as, geen samengevoegde naam meer!
  select(groep, typologie, Response, Pathway, Predictor_Cat, Estimate) %>%
  drop_na(groep, typologie, Response, Pathway, Predictor_Cat) %>%

  # groepeer per groep én typologie, zodat R per watersysteem exact de juiste deelmaatlatten invult
  group_by(groep, typologie) %>%
  complete(nesting(Pathway, Predictor_Cat), Response) %>%
  ungroup()

plot_biotisch <- ggplot(heatmap_data_biotisch, aes(x = Response, y = Pathway, fill = Estimate)) +
  geom_tile(color = "grey90", linewidth = 0.3) +
  scale_fill_gradient2(
    low = "#B2182B", mid = "white", high = "#2166AC",
    midpoint = 0, na.value = "white",
    name = "Standaard\nEffect"
  ) +
  # --- DE NIEUWE INDELING ---
  # Voeg '+ typologie' toe aan de facet_grid
  facet_grid(Predictor_Cat ~ groep + typologie, scales = "free", space = "free") +
  # --------------------------
theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    panel.grid = element_blank(),

    # Maak de facet-titels (groep en typologie) goed leesbaar
    strip.text.x = element_text(face = "bold", size = 11, margin = margin(b = 5, t = 5)),
    strip.text.y = element_text(face = "bold", size = 10, angle = 0),
    strip.background.x = element_rect(fill = "grey95", color = "grey50"), # Grijze balkjes boven de typologieën
    strip.background.y = element_rect(fill = "grey95", color = NA),

    # --- DE SCHEIDINGSLIJNEN ---
    # Zet een duidelijk kader om elk blok (typologie)
    panel.border = element_rect(color = "grey30", fill = NA, linewidth = 0.8),
    # Zorg voor een fysieke spatie/witruimte tussen de kolommen (typologieën) op de X-as
    panel.spacing.x = unit(0.3, "cm"),
    # Spatie tussen de stressor-categorieën op de Y-as
    panel.spacing.y = unit(0.1, "cm")
    # ---------------------------
  ) +
  labs(
    title = "Biologische Reacties",
    subtitle = "Directe effecten op flora en fauna per stressor-categorie en typologie",
    x = "Deelmaatlat",
    y = ""
  )

print(plot_biotisch)

library(tidyverse)
library(ggplot2)

# ==============================================================================
# 1. BEPAAL DE VOLGORDE VAN DE X-AS (Hoofdindexen eerst)
# ==============================================================================
# Definieer de belangrijkste eindpunten die je altijd vooraan wilt hebben
hoofd_indexen <- c("mmif", "index_nieuw")

# Pak de rest van jouw biologische vars (die we eerder automatisch vonden) en sorteer ze alfabetisch
andere_maatlatten <- sort(setdiff(biologische_vars, hoofd_indexen))

# Plak ze aan elkaar voor de definitieve rangorde
gewenste_volgorde <- c(hoofd_indexen, andere_maatlatten)


# ==============================================================================
# 2. DATA VOORBEREIDEN EN SORTEREN
# ==============================================================================
heatmap_data_biotisch <- master_df %>%
  filter(!grepl("~~", Predictor) & !grepl("~~", Response)) %>%
  filter(Response %in% biologische_vars) %>%
  mutate(
    Predictor_Cat = bepaal_groep(Predictor),
    Pathway = Predictor,
    Model_Naam = paste(typologie, Response, sep = " - ")
  ) %>%
  # Forceer de volgordes!
  mutate(
    Predictor_Cat = factor(Predictor_Cat, levels = c(
      "Klimaat", "Landgebruik", "Lozingen en overstorten",
      "Hydromorfologie", "Nutriënten", "Fysico-chemie", "Overig"
    )),
    Response = factor(Response, levels = gewenste_volgorde)
  ) %>%
  # Sorteer de data in R zodat alles perfect staat: eerst groep, dan typologie, dan de Maatlat
  arrange(groep, typologie, Response) %>%
  # ZET VAST: nu maken we Model_Naam een factor op basis van deze exacte volgorde
  mutate(Model_Naam = factor(Model_Naam, levels = unique(Model_Naam))) %>%

  select(groep, typologie, Model_Naam, Pathway, Predictor_Cat, Estimate) %>%
  drop_na(groep, typologie, Model_Naam, Pathway, Predictor_Cat) %>%

  group_by(groep) %>%
  complete(nesting(Pathway, Predictor_Cat), nesting(Model_Naam, typologie)) %>%
  ungroup()


# ==============================================================================
# 3. BEREKEN DE POSITIES VOOR DE DIKKE VERTICALE LIJNEN
# ==============================================================================
# We tellen per groep (mafy/mi) hoeveel 'vakjes' een typologie breed is.
# Precies op de overgang (bijv. na 5 vakjes) plaatsen we de lijn op positie 5.5.
scheidingslijnen <- heatmap_data_biotisch %>%
  select(groep, typologie, Model_Naam) %>%
  distinct() %>%
  arrange(groep, Model_Naam) %>%
  group_by(groep) %>%
  mutate(x_positie = row_number()) %>%
  group_by(groep, typologie) %>%
  summarise(grens = max(x_positie) + 0.5, .groups = "drop") %>%
  # We willen geen lijn helemaal op het einde van de plot, dus de laatste gooien we weg
  group_by(groep) %>%
  filter(grens != max(grens))


# ==============================================================================
# 4. GRAFIEK 2: Plotten (Vorige Facet-stijl mét lijnen)
# ==============================================================================
plot_biotisch <- ggplot(heatmap_data_biotisch, aes(x = Model_Naam, y = Pathway, fill = Estimate)) +
  geom_tile(color = "grey90", linewidth = 0.3) +
  scale_fill_gradient2(
    low = "#B2182B", mid = "white", high = "#2166AC",
    midpoint = 0, na.value = "white",
    name = "Standaard\nEffect"
  ) +
  # --- DE DIKKE SCHEIDINGSLIJNEN ---
  geom_vline(data = scheidingslijnen, aes(xintercept = grens), color = "grey30", linewidth = 0.8) +
  # ---------------------------------

# Terug naar de facet-stijl van de *vorige* versie (geen typologie splitsing hier)
facet_grid(Predictor_Cat ~ groep, scales = "free", space = "free") +

  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    panel.grid = element_blank(),
    strip.text.x = element_text(face = "bold", size = 14),
    strip.text.y = element_text(face = "bold", size = 10, angle = 0),
    strip.background.y = element_rect(fill = "grey95", color = NA),
    panel.border = element_rect(color = "grey60", fill = NA, linewidth = 0.5)
  ) +
  labs(
    title = "Biologische Reacties",
    subtitle = "Directe effecten op flora en fauna per stressor-categorie",
    x = "Model (typologie - Deelmaatlat)",
    y = ""
  )

print(plot_biotisch)



###versie x

library(tidyverse)
library(ggplot2)

# ==============================================================================
# 1. BEPAAL DE VOLGORDE VAN DE X-AS (Hoofdindexen eerst)
# ==============================================================================
hoofd_indexen <- c("mmif", "index_nieuw")
andere_maatlatten <- sort(setdiff(biologische_vars, hoofd_indexen))
gewenste_volgorde <- c(hoofd_indexen, andere_maatlatten)

# ==============================================================================
# 2. DATA VOORBEREIDEN EN SORTEREN (GEFIXED)
# ==============================================================================
heatmap_data_biotisch <- master_df %>%
  filter(!grepl("~~", Predictor) & !grepl("~~", Response)) %>%
  filter(Response %in% biologische_vars) %>%
  mutate(
    Predictor_Cat = bepaal_groep(Predictor),
    Pathway = Predictor
  ) %>%
  mutate(
    Predictor_Cat = factor(Predictor_Cat, levels = c(
      "Klimaat", "Landgebruik", "Lozingen en overstorten",
      "Hydromorfologie", "Nutriënten", "Fysico-chemie", "Overig"
    ))
  ) %>%
  select(groep, typologie, Response, Pathway, Predictor_Cat, Estimate) %>%
  drop_na(groep, typologie, Response, Pathway, Predictor_Cat) %>%

  # Zorg dat het rooster wordt aangevuld, maar ENKEL met de maatlatten van die specifieke groep
  group_by(groep, typologie) %>%
  complete(nesting(Pathway, Predictor_Cat), nesting(Response)) %>%
  ungroup() %>%

  # --- DE FIX ---
  # Zet de perfecte volgorde pas vast NADAT het rooster is aangevuld
  mutate(Response = factor(Response, levels = gewenste_volgorde))
# --------------
# ==============================================================================
# 3. GRAFIEK 2: De Naadloze Facet Plot
# ==============================================================================
plot_biotisch <- ggplot(heatmap_data_biotisch, aes(x = Response, y = Pathway, fill = Estimate)) +
  geom_tile(color = "grey90", linewidth = 0.3) +
  scale_fill_gradient2(
    low = "#B2182B", mid = "white", high = "#2166AC",
    midpoint = 0, na.value = "white",
    name = "Standaard\nEffect"
  ) +
  # Gebruik groep én typologie als facet kolommen
  facet_grid(Predictor_Cat ~ groep + typologie, scales = "free", space = "free") +

  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    panel.grid = element_blank(),

    # Maak de headers mooi op
    strip.text.x = element_text(face = "bold", size = 11, margin = margin(t = 4, b = 4)),
    strip.text.y = element_text(face = "bold", size = 10, angle = 0),
    strip.background.x = element_rect(fill = "grey90", color = "grey40", linewidth = 0.8),
    strip.background.y = element_rect(fill = "grey95", color = NA),

    # --- HET GEHEIM VOOR DE NAADLOZE AANSLUITING ---
    panel.spacing.x = unit(0, "cm"), # Trekt alle typologieën strak tegen elkaar aan
    panel.spacing.y = unit(0.1, "cm"), # Behoudt wel een beetje ruimte tussen de Y-as categorieën

    # Teken een dikke kader om elk paneel. Omdat spacing 0 is, worden dit de verticale scheidingslijnen!
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 0.8)
    # -----------------------------------------------
  ) +
  labs(
    title = "Biologische Reacties",
    subtitle = "Directe effecten op flora en fauna per stressor-categorie",
    x = "Deelmaatlat", # X-as label is nu veel korter
    y = ""
  )

print(plot_biotisch)

#### met goeie facettitels

# Installeer ggh4x eenmalig als je dit nog niet hebt:
# install.packages("ggh4x")
library(ggh4x)

# ==============================================================================
# 3. GRAFIEK 2: De Naadloze Facet Plot met Gecentreerde Hoofdgroepen
# ==============================================================================
plot_biotisch <- ggplot(heatmap_data_biotisch, aes(x = Response, y = Pathway, fill = Estimate)) +
  geom_tile(color = "grey90", linewidth = 0.3) +
  scale_fill_gradient2(
    low = "#B2182B", mid = "white", high = "#2166AC",
    midpoint = 0, na.value = "white",
    name = "Standaard\nEffect"
  ) +

  # --- DE OPLOSSING ---
  # Vervang facet_grid door facet_nested!
  facet_nested(Predictor_Cat ~ groep + typologie, scales = "free", space = "free") +
  # --------------------

theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    panel.grid = element_blank(),

    # Maak de headers mooi op
    strip.text.x = element_text(face = "bold", size = 11, margin = margin(t = 4, b = 4)),
    strip.text.y = element_text(face = "bold", size = 10, angle = 0),
    strip.background.x = element_rect(fill = "grey90", color = "grey40", linewidth = 0.8),
    strip.background.y = element_rect(fill = "grey95", color = NA),

    # De panel spacing blijft 0 voor die perfecte aaneensluiting
    panel.spacing.x = unit(0, "cm"),
    panel.spacing.y = unit(0.1, "cm"),

    # De dikke kaders om de blokken
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 0.8)
  ) +
  labs(
    title = "Biologische Reacties",
    subtitle = "Directe effecten op flora en fauna per stressor-categorie",
    x = "Deelmaatlat",
    y = ""
  )

print(plot_biotisch)

###ABIOTIEK BETERE FIGUUR
# ==============================================================================
# 1. DATA VOORBEREIDEN (Nu mét categorieën en veilige complete-functie)
# ==============================================================================
heatmap_data_abiotisch <- master_df %>%
  # Filter biologische variabelen en gecorreleerde fouten eruit
  filter(!Response %in% biologische_vars) %>%
  filter(!str_detect(Predictor, "~~") & !str_detect(Response, "~~")) %>%

  # Zorg voor 1 uniek pad per typologie/groep combinatie
  distinct(groep, typologie, Predictor, Response, .keep_all = TRUE) %>%

  # Voeg categorieën toe via de 'bepaal_groep' functie die we eerder maakten
  mutate(
    Predictor_Cat = bepaal_groep(Predictor),
    Pathway = paste(Predictor, "➔", Response)
  ) %>%

  # Forceer de vaste volgorde van categorieën
  mutate(Predictor_Cat = factor(Predictor_Cat, levels = c(
    "Klimaat", "Landgebruik", "Lozingen en overstorten",
    "Hydromorfologie", "Nutriënten", "Fysico-chemie", "Overig"
  ))) %>%

  select(groep, typologie, Pathway, Predictor_Cat, Estimate) %>%
  drop_na(groep, typologie, Pathway, Predictor_Cat) %>%

  # Groepeer per 'groep' (mafy/mi) zodat R het raster netjes en onafhankelijk aanvult
  group_by(groep) %>%
  complete(nesting(Pathway, Predictor_Cat), typologie) %>%
  ungroup()

# ==============================================================================
# 2. GRAFIEK 1: De Abiotische Naadloze Facet Plot
# ==============================================================================
plot_abiotisch <- ggplot(heatmap_data_abiotisch, aes(x = typologie, y = Pathway, fill = Estimate)) +
  geom_tile(color = "grey90", linewidth = 0.3) +
  scale_fill_gradient2(
    low = "#B2182B", mid = "white", high = "#2166AC",
    midpoint = 0, na.value = "white",
    name = "Gestandaardiseerd\nEffect"
  ) +

  # Gebruik facet_nested! Hier splitsen we de Y-as op categorie en de X-as op groep (mafy/mi)
  # scales = "free_y" zorgt dat categorie-vakken die in de abiotiek niet voorkomen netjes verdwijnen
  facet_nested(Predictor_Cat ~ groep, scales = "free_y", space = "free_y") +

  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 11),
    axis.text.y = element_text(size = 10),
    panel.grid = element_blank(),

    # Prachtige headers instellen
    strip.text.x = element_text(face = "bold", size = 14, margin = margin(t = 6, b = 6)),
    strip.text.y = element_text(face = "bold", size = 10, angle = 0),
    strip.background.x = element_rect(fill = "grey90", color = "grey40", linewidth = 0.8),
    strip.background.y = element_rect(fill = "grey95", color = NA),

    # Zorg voor dikke scheidingslijnen om de vakken
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 0.8),

    # Beetje ruimte tussen MI en MAFY op de X-as voor de leesbaarheid, strakke blokken op de Y-as
    panel.spacing.x = unit(0.3, "cm"),
    panel.spacing.y = unit(0.1, "cm")
  ) +
  labs(
    title = "Abiotische Causale Paden (Milieu ➔ Milieu)",
    subtitle = "Gegroepeerd per brontype van de voorspeller",
    x = "Typologie",
    y = ""
  )

print(plot_abiotisch)

