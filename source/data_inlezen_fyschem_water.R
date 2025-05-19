source(here::here("source", "inladen_packages.R"))
# Analyseresultaten ----

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
  left_join(., anaresult_locations, by = "sample_point")

save(fc_data, file = here("data", "verwerkt", "fc_data.rdata"))

load(file = here("data", "verwerkt", "fc_data.rdata"))

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



# unieke stoffen
fc_data %>%
  select(parameter_omschrijving) %>%
  unique() %>%
  View()

selectie <- fc_data %>%
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
                                       "Sulfaat"
                                                ))

temperatuur <- fc_data %>%
  filter(parameter_omschrijving %in% c("Fosfor, totaal"))

totaal_stikstof <- selectie %>%
  filter(parameter_symbool == "N t")
totaal_fosfor <- selectie %>%
  filter(parameter_symbool == "P t")

zuurstof <- selectie %>%
  filter(parameter_symbool %in% c("O2")) %>%
  filter(resultaat < 45) %>%
  mutate(year = format(sample_datum_monstername, "%Y")) %>%
  group_by(year, sample_point) %>%
  summarise(mean_02 = mean(resultaat, na.rm = T))

ggplot(zuurstof, aes(year, mean_02)) +
  geom_point() +
  geom_smooth(method = "lm")

summary(test$resultaat)


test <- anaresult %>% mutate(sample_point2 = stringr::str_replace(sample_point, "OW", ""))
test %>% filter(sample_point2 == "21225470")
