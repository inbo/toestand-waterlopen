source(here::here("source", "inladen_packages.R"))

##########################################
# Industie lozingen niet op RWZI direct in oppervlaktewater
##########################################

industrie <- read_excel("data/ruw/lozingen/industrie/AW_Modellering_Vrachten Industrie T08_54_18.xlsx") %>%
  janitor::clean_names()
# %>%
#   select(meetput_nummer, pdts_jaar, meetput_lozingswijze, hydraulisch_punt_x_coordinaat, hydraulisch_punt_y_coordinaat,
#          vha_gewestelijke_waterloop_code, vha_waterlichaam_code)

# 1. Definieer de omzettingsfactoren (gram per IE per dag)
ie_normen <- c(
  "ZS"   = 90,
  "CZV"  = 135,
  "BZV5" = 60,
  "N t"  = 10,
  "P t"  = 2
)

# 2. De transformatie
industrie_ie_resultaat <- industrie %>%
  # Filter op de relevante lozingswijze en parameters
  filter(meetput_lozingswijze == "OW DIR") %>%
  # STAP 0: Verwijder meetpunten waarvoor GEEN ENKELE bruikbare vracht-data is
  group_by(meetput_nummer) %>%
  filter(!all(is.na(pdts_netto_vracht_og_pdts_kalenderdagen))) %>%
  ungroup() %>%
  filter(parameter_symbool %in% names(ie_normen)) %>%

  # Stap 1: Omzetten van kg naar gram en IE berekenen per rij (stof)
  mutate(
    vracht_g_dag = pdts_netto_vracht_og_pdts_kalenderdagen * 1000, # van kg totaal naar g/dag
    ie_per_stof = vracht_g_dag / ie_normen[parameter_symbool]
  ) %>%

  # Stap 2: Bereken de Max en Gemiddelde IE per meetput
  group_by(meetput_nummer) %>%
  mutate(
    max_ie_meetpunt = max(ie_per_stof, na.rm = TRUE),
    parameter_max_ie = parameter_symbool[which.max(ie_per_stof)],
    gemiddelde_ie_meetpunt = mean(ie_per_stof, na.rm = TRUE)
  ) %>%
  ungroup()

############################################################################
# RWZI effluent direct in oppervlaktewater droog of regenwaterafvoer?
############################################################################

rwzi <- read_excel("data/ruw/lozingen/RWZI en riool/AW_Modellering_Vrachten RWZI T09_20_51.xlsx") %>%
  janitor::clean_names()

rwzi_ie_resultaat <- rwzi %>%
  filter(meetput_influent_effluent_andere == "Effluent") %>%
  filter(meetput_lozingswijze == "OW DIR") %>%
  # STAP 0: Verwijder meetpunten waarvoor GEEN ENKELE bruikbare vracht-data is
  group_by(meetput_nummer) %>%
  filter(!all(is.na(pdts_netto_vracht_og_pdts_kalenderdagen))) %>%
  ungroup() %>%
  filter(parameter_symbool %in% names(ie_normen)) %>%
  #filter(meetput_dwa_rwa_omschrijving == "Droogweerafvoer") %>%
  # Stap 1: Omzetten van kg naar gram en IE berekenen per rij (stof)
  mutate(
    vracht_g_dag = pdts_netto_vracht_og_pdts_kalenderdagen * 1000, # van kg totaal naar g/dag
    ie_per_stof = vracht_g_dag / ie_normen[parameter_symbool]
  ) %>%

  # Stap 2: Bereken de Max en Gemiddelde IE per meetput
  group_by(meetput_nummer) %>%
  mutate(
    max_ie_meetpunt = max(ie_per_stof, na.rm = TRUE),
    parameter_max_ie = parameter_symbool[which.max(ie_per_stof)],
    gemiddelde_ie_meetpunt = mean(ie_per_stof, na.rm = TRUE)
  ) %>%
  ungroup()

############################################################################
# Uitlaten riool oppervlaktewater
############################################################################
# enkel voor 2018, 19, 20, 21
riool_2018 <- read_excel("data/ruw/lozingen/RWZI en riool/20181210_AWIS_AIW_PEGASE_Vuilvracht Hpunten per zuiveringsgebied Datumprompt.xlsx", sheet = 1, skip = 6) %>%
  janitor::clean_names()

riool_ie_resultaat <- riool_2018 %>%
  filter(uitlaat_type_code == "VUIL") %>%
  rename(ie_meetput = inwoners_uitlaat)
