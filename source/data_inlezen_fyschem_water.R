source(here::here("source", "inladen_packages.R"))

# Analyseresultaten ----
if(!file.exists(here("data", "verwerkt", "fc_data.rdata"))){
# meetresultaten inlezen ----
sheet_names <- excel_sheets(here(
  "data",
  "ruw",
  "fys_chem",
  "241024_Analyseresultaten per meetplaats_2010_2024.xlsx"
))

# Limit to the first 8 sheets (if there are fewer than 8 sheets,
# adjust accordingly)
sheets_to_read <- sheet_names[1:8]

# Read all sheets into a list of dataframes
list_of_dataframes <- lapply(sheets_to_read, function(sheet) {
  read_excel(here(
    "data",
    "ruw",
    "fys_chem",
    "241024_Analyseresultaten per meetplaats_2010_2024.xlsx"
  ),
  sheet = sheet)
})

# Combine all dataframes into one
combined_dataframe <- do.call(rbind, list_of_dataframes)

anaresult1 <- combined_dataframe

anaresult <- janitor::clean_names(anaresult1) %>%
  mutate(sample_datum_monstername = as.Date(sample_datum_monstername),
         sample_tijdstip_monstername = format(sample_tijdstip_monstername, format = "%H:%M:%S"))

anaresult_locations <-
  read_xlsx(here("data", "ruw", "fys_chem", "241024_Analyseresultaten per meetplaats_2010_2024.xlsx"),
            sheet = 10) %>%
  janitor::clean_names()
rm(combined_dataframe)
rm(list_of_dataframes)

fc_data <- anaresult %>%
  left_join(., anaresult_locations, by = "sample_point") %>% # toevoegen kolom met aangepast resultaat met detectielimiet /2
  mutate(resultaat_detectielimiet = ifelse(teken == "<", resultaat/2, resultaat)) %>%
  rename(meetplaats = sample_point,
         monsternamedatum = sample_datum_monstername)

# -------------------------------------------------------------------------
# STANDAARDISATIE EENHEDEN NAAR µg/L
# -------------------------------------------------------------------------

fc_data <- fc_data %>%
  mutate(
    # 1. Bepaal de conversiefactor op basis van de huidige eenheid
    factor_naar_ug = case_when(
      # A. Milligram familie (mg/L, mgN/L, mgP/L, etc.) -> x 1000
      grepl("^mg.*/L$", eenheid) ~ 1000,

      # B. Nanogram familie (ng/L, ngSn/L) -> x 0.001
      grepl("^ng.*/L$", eenheid) ~ 0.001,

      # C. Franse Hardheidsgraden (°F)
      # Relevant voor metaal-toxiciteit correcties (BLM modellen)
      # 1 °F = 10 mg CaCO3/L = 10.000 µg CaCO3/L
      eenheid == "°F" ~ 10000,

      # D. Reeds in µg of niet converteerbaar (bijv. bacteriën, pH, %) -> x 1
      TRUE ~ 1
    ),

    # 2. Bereken de gestandaardiseerde waarde
    resultaat_ug_L = resultaat_detectielimiet * factor_naar_ug,

    # 3. Update de eenheid label (alleen als er geconverteerd is)
    eenheid_std = case_when(
      # Als de factor 1 is, behoud de originele eenheid
      factor_naar_ug == 1 ~ eenheid,

      # Specifieke conversie voor hardheid
      eenheid == "°F" ~ "µg CaCO3/L",

      # Alle andere massa-conversies worden µg/L
      # Let op: mgN/L wordt hiermee µg/L (waarbij 'N' impliciet blijft via de parameter naam)
      TRUE ~ "µg/L"
    )
  )

# -------------------------------------------------------------------------
# CONTROLE & OPSLAAN
# -------------------------------------------------------------------------

# Check even wat er is veranderd
check_conversie <- fc_data %>%
  select(parameter_symbool, resultaat, eenheid, resultaat_ug_L, eenheid_std) %>%
  filter(eenheid != eenheid_std) %>%
  distinct(eenheid, eenheid_std)

print(check_conversie)

save(fc_data, file = here("data", "verwerkt", "fc_data.rdata"))
}

load(file = here("data", "verwerkt", "fc_data.rdata"))

if (!file.exists(here("data", "ruw", "fys_chem", "fc_meetpunten.gpkg"))) {
fc_meetpunten <- fc_data %>%
  select(meetplaats, monsternamedatum, lambert_x, lambert_y) %>%
  drop_na(lambert_x) %>%
  unique() %>%
  st_as_sf(coords = c("lambert_x", "lambert_y"),
           crs = 31370)
st_write(fc_meetpunten, dsn = here("data", "ruw", "fys_chem", "fc_meetpunten.gpkg"), delete_dsn = T)
}


# unieke stoffen
fc_data %>%
  select(parameter_omschrijving) %>%
  unique() %>%
  View()

# selectie van interessante stoffen ----

clean_like_janitor <- function(x) { # soort janitor::clean_names() voor waarden
  x |>
    str_to_lower() |>                     # lowercase
    str_replace_all("[^a-z0-9]+", "_") |> # replace non-alphanumeric with _
    str_replace_all("^_|_$", "")          # trim leading/trailing underscores
}

fc_selectie <- fc_data %>%
  filter(parameter_omschrijving %in% c("Chemisch zuurstofverbruik",
                                       "pH",
                                       "Fosfor, totaal",
                                       "Stikstof, totaal",
                                       "Zuurstof, opgelost",
                                       "Geleidbaarheid (20°C)",
                                       "Geleidbaarheid (25°C)",
                                       "Zuurstof, opgelost",
                                       "Temperatuur",
                                       "Chloride",
                                       "Nitriet",
                                       "Nitraat",
                                       "Kjeldahlstikstof",
                                       "Ammonium",
                                       "Zuurstof, verzadiging",
                                       "Biochemisch zuurstofverbruik na 5d.",
                                       "Sulfaat",
                                       "Orthofosfaat",
                                       "Zwevende stoffen"
                                       )) %>%
  mutate(parameter_symbool = clean_like_janitor(parameter_symbool)) %>%
  select(meetplaats, monsternamedatum, parameter_symbool, resultaat_ug_L) %>% # wide maken
  pivot_wider(., names_from = parameter_symbool, values_from = resultaat_ug_L, values_fn = mean) # gemiddelde nemen van dubbele metingen; hier geen reden voor gevonden want zelfde plaats, dag en uur voor parameter soms dubbele waarde.


save(fc_selectie, file = here("data", "verwerkt", "fc_selectie.rdata"))



# overschrijdingen polluenten ----
overschrijdingen_aantal <- read_excel(here("data", "ruw", "fys_chem", "250326_Beoordeling GS per SP met norm_2007_2024.xlsx"), sheet = "MPGemMaxLijst") %>%
  janitor::clean_names() %>%
  rename(meetplaats = sample_point_naam) %>%
  select(-x, -y)

overschrijdingen_soorten_stoffen <- read_excel(here("data", "ruw", "fys_chem", "tabel_overschrijdingen.xlsx"))

overschrijdingen_parameters <- read_excel(here("data", "ruw", "fys_chem", "250326_Beoordeling GS per SP met norm_2007_2024.xlsx"), sheet = "OverschrijdingMPPar") %>%
  janitor::clean_names() %>%
  rename(meetplaats = sample_point_naam) %>%
  select(-x, -y) %>%
  left_join(overschrijdingen_soorten_stoffen,
            by = c("parameter_omschrijving" = "stofnaam"))

overschrijdingen0 <- overschrijdingen_aantal %>%
  left_join(., overschrijdingen_parameters, by = c("jaar", "meetplaats"))

aantal_pesticiden_met_overschrijding <- overschrijdingen0 %>%
  filter(categorie == "Pesticiden (Bestrijdingsmiddelen)") %>%
  group_by(jaar, meetplaats) %>%
  summarise(aantal_pesticiden_met_overschrijding = n(), .groups = "drop")

aantal_zware_metalen_met_overschrijding <- overschrijdingen0 %>%
  filter(categorie == "Zware Metalen / Sporenelementen") %>%
  group_by(jaar, meetplaats) %>%
  summarise(aantal_zware_metalen_met_overschrijding = n(), .groups = "drop")

overschrijdingen <- overschrijdingen0 %>%
  left_join(aantal_pesticiden_met_overschrijding, by = c("jaar", "meetplaats")) %>%
  mutate(aantal_pesticiden_met_overschrijding = replace_na(aantal_pesticiden_met_overschrijding, 0)) %>%
  left_join(aantal_zware_metalen_met_overschrijding, by = c("jaar", "meetplaats")) %>%
  mutate(aantal_zware_metalen_met_overschrijding = replace_na(aantal_zware_metalen_met_overschrijding, 0))

save(overschrijdingen, file = here("data", "verwerkt", "overschrijdingen.rdata"))
