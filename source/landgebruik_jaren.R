source(here::here("source", "inladen_packages.R"))

# landgebruik bepalen (procentueel) binnen elke afstroomgebied
landuse_raster_2013 <- raster::raster(here("data", "ruw", "landgebruik", "niveau1_vla_2013_v3.tif"))
landuse_raster_2016 <- raster::raster(here("data", "ruw", "landgebruik", "niveau1_vla_2016_v3.tif"))
landuse_raster_2019 <- raster::raster(here("data", "ruw", "landgebruik", "niveau1_vla_2019_v3.tif"))
landuse_raster_2022 <- raster::raster(here("data", "ruw", "landgebruik", "niveau1_vla_2022_v3.tif"))


# Definieer de jaren die geanalyseerd moeten worden
jaren <- c(2013, 2016, 2019, 2022)

# Laad de afstroomgebieden
watersheds_buffered <- st_read(here("data", "verwerkt", "hydrologisch", "mi_meetpunten_watersheds_buffered_all.gpkg"))

# Definieer de functies
# Deze functies zijn overgenomen uit je originele code

# Functie om pixelwaarden om te zetten in percentages
convert_pixels_to_percentages <- function(data) {
  data <- data %>%
    mutate(
      total_pixels = rowSums(across(starts_with("VALUE_")), na.rm = TRUE),
      across(starts_with("VALUE_"), ~ .x / total_pixels * 100, .names = "{.col}_pct")
    )
  return(data)
}

# Functie om de landgebruiksklassen te hergroeperen
landuse_reclass <- function(data, suffix) {
  data <- data %>%
    mutate(
      !!sym(paste0("water_", suffix)) := VALUE_24_pct,
      !!sym(paste0("verharding_", suffix)) := VALUE_19_pct + VALUE_22_pct + VALUE_23_pct + VALUE_25_pct,
      !!sym(paste0("natte_natuur_", suffix)) := VALUE_5_pct + VALUE_9_pct + VALUE_10_pct,
      !!sym(paste0("landbouw_intens_", suffix)) := VALUE_11_pct + VALUE_18_pct + VALUE_14_pct + VALUE_15_pct,
      !!sym(paste0("hooggroen_", suffix)) := VALUE_2_pct + VALUE_3_pct + VALUE_4_pct + VALUE_16_pct + VALUE_21_pct,
      !!sym(paste0("laaggroen_", suffix)) := VALUE_1_pct + VALUE_6_pct + VALUE_7_pct + VALUE_8_pct + VALUE_20_pct,
      !!sym(paste0("landbouw_extensief_", suffix)) := VALUE_13_pct + VALUE_17_pct
    ) %>%
    select(meetplaats, starts_with("water_"), starts_with("verharding_"), starts_with("natte_natuur_"),
           starts_with("landbouw_intens_"), starts_with("hooggroen_"),
           starts_with("laaggroen_"), starts_with("landbouw_extensief_")) %>%
    st_drop_geometry()
  return(data)
}

# Functie om de landgebruiksanalyse voor één jaar uit te voeren
bereken_landgebruik_per_jaar <- function(jaar) {
  # Dynamische bestandsnamen op basis van het jaar
  input_raster_path <- here("data", "ruw", "landgebruik", paste0("niveau1_vla_", jaar, "_v3.tif"))
  output_table_path <- here("data", "verwerkt", "landgebruik", paste0("zonal_histogram_landgebruik_afstroomgebieden_", jaar, ".gpkg"))

  # Laad het raster en transformeer de afstroomgebieden
  landuse_raster <- raster::raster(input_raster_path)
  watersheds_buffered_trans <- st_transform(watersheds_buffered, crs = crs(landuse_raster))

  # Voer het algoritme uit als het uitvoerbestand nog niet bestaat
  if (!file.exists(output_table_path)) {
    landuse_raster[landuse_raster < 1 | landuse_raster > 25] <- NA
    qgis_run_algorithm("native:zonalhistogram",
                       INPUT_RASTER = landuse_raster,
                       INPUT_VECTOR = watersheds_buffered_trans,
                       RASTER_BAND = 1,
                       COLUMN_PREFIX = "VALUE_",
                       OUTPUT = output_table_path)
  }

  # Lees de uitvoertabel, converteer en hergroepeer
  watershed_landuse0 <- st_read(output_table_path)
  watershed_landuse <- convert_pixels_to_percentages(data = watershed_landuse0)
  reclassified_data <- landuse_reclass(watershed_landuse, as.character(jaar))

  return(reclassified_data)
}

# Voer de functie uit voor alle jaren en sla de resultaten op in een lijst
resultaten_lijst <- map(jaren, bereken_landgebruik_per_jaar)

# Voeg de resultaten samen in één dataframe
finale_landgebruik <- watersheds_buffered %>%
  select(meetplaats, oppervlakte) %>%
  st_drop_geometry()

for (i in seq_along(resultaten_lijst)) {
  finale_landgebruik <- finale_landgebruik %>%
    inner_join(resultaten_lijst[[i]], by = "meetplaats")
}

# Sla het uiteindelijke resultaat op
save(finale_landgebruik, file = here("data", "verwerkt", "landgebruik", "landgebruik_afstroomgebied_tijdreeks.Rdata"))

#koppeling jaren landgebruik

# Voeg een kolom 'landgebruiksjaar' toe met nauwkeurige drempelwaarden
landgebruik_koppeling0 <- mi_data %>%
  select(meetplaats, monsternamedatum) %>%
  mutate(
    landgebruiksjaar = as.character(case_when(
      monsternamedatum < ymd("2014-07-01") ~ 2013,  # Van 2007 tot halverwege 2014
      monsternamedatum < ymd("2017-07-01") ~ 2016,  # Van halverwege 2014 tot halverwege 2017
      monsternamedatum < ymd("2020-07-01") ~ 2019,  # Van halverwege 2017 tot halverwege 2020
      TRUE ~ 2022                               # Na halverwege 2020 tot en met 2023
    )
  )) %>%
  st_drop_geometry()

landgebruik_koppeling1 <- finale_landgebruik %>%
  pivot_longer(
    cols = -c(meetplaats, oppervlakte), # Kies alle kolommen behalve deze
    names_to = c("landgebruik", "landgebruiksjaar"), # Maak twee tijdelijke kolommen
    names_pattern = "(.*)_([0-9]{4})$" # Splits op het laatste _ gevolgd door 4 cijfers
  ) %>%
  pivot_wider(
    names_from = landgebruik, # Maak nieuwe kolommen op basis van de 'landgebruik' waarden
    values_from = value # Vul de nieuwe kolommen met de waarden uit 'value'
  )

landgebruik_afstroomgebied_jaren <- landgebruik_koppeling0 %>% # me deze df kan je landgebruik op basis van de verschillende jaarlagen koppelen aan mi_data
  left_join(., landgebruik_koppeling1, by = c("meetplaats", "landgebruiksjaar"))
save(landgebruik_afstroomgebied_jaren, file = here("data", "verwerkt", "landgebruik", "landgebruik_afstroomgebied_jaren.rdata"))

# oeverlandgebruik en landgebruik cirkelvormige buffer (door Maarten aangeleverd), omzetten omzetten in percentages en reclassen. Dit zo nog moeten gecorrigeerd worden voor de verschillende jaren van de landgebruikskaart.

landuse_oever <- convert_pixels_to_percentages(data = landuse_oever0)
landuse_buffer <- convert_pixels_to_percentages(data = landuse_buffer0)
save(landuse_oever, file = here("data", "verwerkt", "landgebruik","landgebruik_oever.Rdata"))
save(landuse_buffer, file = here("data", "verwerkt", "landgebruik","landgebruik_buffer.Rdata"))
buffer_landuse_reclass <- landuse_reclass(landuse_buffer, "buffer")
save(buffer_landuse_reclass, file  = here("data", "verwerkt", "landgebruik", "landgebruik_buffer.Rdata"))
oever_landuse_reclass <- landuse_reclass(landuse_oever, "oever")
save(oever_landuse_reclass, file  = here("data", "verwerkt", "landgebruik", "landgebruik_oever.Rdata"))
