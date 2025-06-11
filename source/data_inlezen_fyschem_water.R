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
  mutate(resultaat_detectielimiet = ifelse(teken == "<", resultaat/2, resultaat))
save(fc_data, file = here("data", "verwerkt", "fc_data.rdata"))
}

load(file = here("data", "verwerkt", "fc_data.rdata"))

if (!file.exists(here("data", "ruw", "fys_chem", "fc_meetpunten.gpkg"))) {
fc_meetpunten <- fc_data %>%
  select(sample_point, sample_datum_monstername, lambert_x, lambert_y) %>%
  drop_na(lambert_x) %>%
  rename(meetplaats = sample_point,
         monsternamedatum = sample_datum_monstername) %>%
  unique() %>%
  st_as_sf(coords = c("lambert_x", "lambert_y"),
           crs = 31370)
rm(anaresult)
st_write(fc_meetpunten, dsn = here("data", "ruw", "fys_chem", "fc_meetpunten.gpkg"), delete_dsn = T)
}


# unieke stoffen
fc_data %>%
  select(parameter_omschrijving) %>%
  unique() %>%
  View()

# selectie van interessante stoffen ----
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
                                       ))

save(fc_selectie, file = here("data", "verwerkt", "fc_selectie.rdata"))

# overschrijdingen polluenten ----
overschrijdingen_aantal <- read_excel(here("data", "ruw", "fys_chem", "250326_Beoordeling GS per SP met norm_2007_2024.xlsx"), sheet = "MPGemMaxLijst") %>%
  janitor::clean_names() %>%
  rename(meetplaats = sample_point_naam) %>%
  select(-x, -y)

overschrijdingen_parameters <- read_excel(here("data", "ruw", "fys_chem", "250326_Beoordeling GS per SP met norm_2007_2024.xlsx"), sheet = "OverschrijdingMPPar") %>%
  janitor::clean_names() %>%
  rename(meetplaats = sample_point_naam) %>%
  select(-x, -y)

overschrijdingen <- overschrijdingen_aantal %>%
  left_join(., overschrijdingen_parameters, by = c("jaar", "meetplaats"))
save(overschrijdingen, file = here("data", "verwerkt", "overschrijdingen.rdata"))
