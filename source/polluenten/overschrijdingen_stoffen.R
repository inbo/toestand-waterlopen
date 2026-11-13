# Installeer/laad benodigde pakketten
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(here) # Toegevoegd voor de bestandspaden

# 1. Data inlezen (opgeschoond met here())
bestand_metingen <- here("data", "ruw", "fys_chem", "250326_Beoordeling GS per SP met norm_2007_2024.xlsx")
df_metingen <- read_excel(bestand_metingen, sheet = "BeoordelingParMPToetswijze")

bestand_categorieen <- here("data", "ruw", "fys_chem", "tabel_overschrijdingen.xlsx")
overschrijdingen_soorten_stoffen <- read_excel(bestand_categorieen)

# 2. Datasets koppelen en filteren
df_gekoppeld <- df_metingen %>%
  left_join(overschrijdingen_soorten_stoffen, by = c("Parameter Omschrijving" = "stofnaam")) %>%
  # Filter rijen zonder categorie weg (indien aanwezig)
  filter(!is.na(categorie)) %>%
  # Sluit de categorie "Diverse Organische Verbindingen / Overige" expliciet uit
  filter(categorie != "Diverse Organische Verbindingen / Overige") %>%
  # Bereken de continue overschrijdingsfactor
  mutate(Overschrijdingsfactor_continu = `Getoetste waarde` / Norm)

# 3. EERSTE AGGREGATIE: Bepaal de maximale factor per individuele stof op de héle waterloop
df_waterloop_stoffen <- df_gekoppeld %>%
  group_by(Waterloop, Jaar, categorie, `Parameter Omschrijving`) %>%
  summarise(
    WL_Max_Factor_Stof = max(Overschrijdingsfactor_continu, na.rm = TRUE),
    WL_Metingen_Stof = sum(`Jaaraggregaat Fysico Chemie Sample Point Aantal Jaar`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Veiligheidsmarge: als er geen metingen zijn, maakt max() er -Inf van. Zet dit om naar NA.
  mutate(WL_Max_Factor_Stof = ifelse(is.infinite(WL_Max_Factor_Stof), NA, WL_Max_Factor_Stof))

# 4. TWEEDE AGGREGATIE: Bereken de variabelen (inclusief proportie!) per waterloop, jaar én categorie
df_waterloop_categorie <- df_waterloop_stoffen %>%
  group_by(Waterloop, Jaar, categorie) %>%
  summarise(
    # Aangezien elke rij nu 1 unieke getoetste stof is, is n() het aantal getoetste stoffen
    Aantal_Getoetst = n(),
    Aantal_Overschreden = sum(WL_Max_Factor_Stof > 1, na.rm = TRUE),

    # DE OPLOSSING VOOR DE MEET-BIAS:
    Proportie_Overschreden = Aantal_Overschreden / Aantal_Getoetst,

    Druk_Som = sum(WL_Max_Factor_Stof, na.rm = TRUE),
    Druk_Max = max(WL_Max_Factor_Stof, na.rm = TRUE),
    Betrouwbaarheid = mean(WL_Metingen_Stof, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Druk_Max = ifelse(is.infinite(Druk_Max), NA, Druk_Max))

# 5. HERVORMEN (PIVOT): Maak er een brede dataset van voor de modellen
df_waterloop_breed <- df_waterloop_categorie %>%
  # Korte namen maken voor schone kolomtitels
  mutate(
    cat_clean = case_when(
      str_detect(categorie, "PAK") ~ "PAK",
      str_detect(categorie, "PFAS") ~ "PFAS",
      str_detect(categorie, "Pesticiden") ~ "Pesticiden",
      str_detect(categorie, "Vluchtige") ~ "VOS",
      str_detect(categorie, "Zware Metalen") ~ "ZwareMetalen",
      TRUE ~ "Andere"
    )
  ) %>%
  select(-categorie) %>%
  pivot_wider(
    names_from = cat_clean,
    # Let op: Aantal_Getoetst en Proportie_Overschreden zijn hier nu toegevoegd
    values_from = c(Aantal_Getoetst, Aantal_Overschreden, Proportie_Overschreden, Druk_Som, Druk_Max, Betrouwbaarheid),
    names_glue = "{cat_clean}_{.value}", # Zorgt voor kolomnamen zoals 'Pesticiden_Proportie_Overschreden'
    values_fill = 0 # Vul 0 in als een categorie niet is gemeten in dat jaar op die waterloop
  )

# 6. KOPPELEN AAN MEETPLAATSEN: 1 rij per Sample Point & Jaar voor koppeling met biologie
df_meetplaatsen_basis <- df_metingen %>%
  select(`Sample Point Naam`, Jaar, Waterloop) %>%
  distinct()

df_finale_dataset <- df_meetplaatsen_basis %>%
  left_join(df_waterloop_breed, by = c("Waterloop", "Jaar")) %>%
  # Verander overgebleven NA's in de nieuwe getalskolommen naar 0
  mutate(across(where(is.numeric), ~replace_na(.x, 0)))

# Bekijk het resultaat
head(df_finale_dataset)
