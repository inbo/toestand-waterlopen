if (!exists("packages_geladen")) {
  source(here::here("source", "inladen_packages.R"))
}

# Laad de macrofyten meetpunten data (voor de datums)
load(here("data", "verwerkt", "mafy_data.rdata"))

# Definieer de jaren die geanalyseerd moeten worden
jaren <- c(2013, 2016, 2019, 2022)

# Laad de mafy oeverzones (buffers) en bereken oppervlakte
mafy_oeverzones <- st_read(here("data", "ruw", "landgebruik", "oevers", "buffers_mafi_ok.shp")) %>%
  mutate(oppervlakte = st_area(st_geometry(.)))

# --- 1. Definieer de functies ---

# Functie om pixelwaarden om te zetten in percentages
# Aangepaste functie om pixelwaarden om te zetten in percentages (met fail-safe)
convert_pixels_to_percentages <- function(data) {

  # 1. Welke kolommen verwachten we minimaal? (Waarde 1 t/m 25)
  verwachte_kolommen <- paste0("VALUE_", 1:25)

  # 2. Zoek uit welke kolommen ontbreken en voeg deze toe met de waarde 0
  ontbrekende_kolommen <- setdiff(verwachte_kolommen, names(data))
  if (length(ontbrekende_kolommen) > 0) {
    data[ontbrekende_kolommen] <- 0
  }

  # 3. Bereken percentages
  data <- data %>%
    mutate(
      # Bereken het totaal aantal pixels (alleen op de VALUE_ kolommen)
      total_pixels = rowSums(across(all_of(verwachte_kolommen)), na.rm = TRUE),

      # Maak de _pct kolommen aan. Als total_pixels 0 is (lege buffer), zet dan op 0%
      across(
        all_of(verwachte_kolommen),
        ~ ifelse(total_pixels == 0, 0, .x / total_pixels * 100),
        .names = "{.col}_pct"
      )
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
      !!sym(paste0("landbouw_extensief_", suffix)) := VALUE_13_pct + VALUE_17_pct,
      !!sym(paste0("akker_", suffix)) := VALUE_11_pct,
      !!sym(paste0("natuur_", suffix)) := VALUE_2_pct + VALUE_3_pct + VALUE_4_pct + VALUE_16_pct + VALUE_21_pct + VALUE_1_pct + VALUE_6_pct + VALUE_7_pct + VALUE_8_pct + VALUE_20_pct + VALUE_5_pct + VALUE_9_pct + VALUE_10_pct,
    ) %>%
    select(meetplaats, starts_with("water_"), starts_with("verharding_"), starts_with("natte_natuur_"),
           starts_with("landbouw_intens_"), starts_with("hooggroen_"),
           starts_with("laaggroen_"), starts_with("landbouw_extensief_"),
           starts_with("akker_"), starts_with("natuur_")) %>%
    st_drop_geometry()
  return(data)
}

# Functie om de landgebruiksanalyse voor één jaar uit te voeren voor MAFY oeverzones
bereken_landgebruik_per_jaar_mafy <- function(jaar) {
  # Dynamische bestandsnamen op basis van het jaar
  input_raster_path <- here("data", "ruw", "landgebruik", paste0("niveau1_vla_", jaar, "_v3.tif"))
  output_table_path <- here("data", "verwerkt", "landgebruik", paste0("zonal_histogram_landgebruik_mafy_oeverzones_", jaar, ".gpkg"))

  # Laad het raster en transformeer de oeverzones
  landuse_raster <- raster::raster(input_raster_path)
  mafy_oeverzones_trans <- st_transform(mafy_oeverzones, crs = raster::crs(landuse_raster))

  # Voer het algoritme uit als het uitvoerbestand nog niet bestaat
  if (!file.exists(output_table_path)) {
    message(paste0("Zonal histogram berekenen voor jaar ", jaar, "..."))
    landuse_raster[landuse_raster < 1 | landuse_raster > 25] <- NA

    qgisprocess::qgis_run_algorithm("native:zonalhistogram",
                                    INPUT_RASTER = landuse_raster,
                                    INPUT_VECTOR = mafy_oeverzones_trans,
                                    RASTER_BAND = 1,
                                    COLUMN_PREFIX = "VALUE_",
                                    OUTPUT = output_table_path)
  }

  # Lees de uitvoertabel, converteer en hergroepeer
  oeverzone_landuse0 <- st_read(output_table_path, quiet = TRUE)
  oeverzone_landuse <- convert_pixels_to_percentages(data = oeverzone_landuse0)
  reclassified_data <- landuse_reclass(oeverzone_landuse, as.character(jaar))

  return(reclassified_data)
}

# --- 2. Uitvoeren en Samenvoegen ---

message("Start berekening landgebruik voor MAFY oeverzones...")
# Voer de functie uit voor alle jaren en sla de resultaten op in een lijst
resultaten_lijst_mafy <- purrr::map(jaren, bereken_landgebruik_per_jaar_mafy)

# Maak een basis dataframe met meetplaats en oppervlakte
finale_landgebruik_mafy <- mafy_oeverzones %>%
  select(meetplaats, oppervlakte) %>%
  st_drop_geometry()

# Voeg de resultaten per jaar samen in de basis dataframe
for (i in seq_along(resultaten_lijst_mafy)) {
  finale_landgebruik_mafy <- finale_landgebruik_mafy %>%
    inner_join(resultaten_lijst_mafy[[i]], by = "meetplaats")
}

# Sla de brede ruimtelijke dataset op
save(finale_landgebruik_mafy, file = here("data", "verwerkt", "landgebruik", "landgebruik_mafy_oeverzones_tijdreeks.Rdata"))


# --- 3. Koppeling landgebruiksjaren aan monstername datums ---

# Selecteer enkel de MAFY data en voeg de drempelwaarden toe
landgebruik_koppeling_mafy_datums <- mafy_data %>%
  select(meetplaats, monsternamedatum) %>%
  st_drop_geometry() %>%
  mutate(
    landgebruiksjaar = as.character(case_when(
      monsternamedatum < ymd("2014-07-01") ~ 2013,  # Van begin tot halverwege 2014
      monsternamedatum < ymd("2017-07-01") ~ 2016,  # Van halverwege 2014 tot halverwege 2017
      monsternamedatum < ymd("2020-07-01") ~ 2019,  # Van halverwege 2017 tot halverwege 2020
      TRUE ~ 2022                                   # Na halverwege 2020
    ))
  )

# Zet de brede tabel (met alle jaren) om naar een lange tabel en weer terug naar de juiste categorieën
landgebruik_mafy_long <- finale_landgebruik_mafy %>%
  pivot_longer(
    cols = -c(meetplaats, oppervlakte),
    names_to = c("landgebruik", "landgebruiksjaar"),
    names_pattern = "(.*)_([0-9]{4})$"
  ) %>%
  pivot_wider(
    names_from = landgebruik,
    values_from = value
  )

# Koppel de daadwerkelijke monsternamedatums aan de juiste jaren van de oeverzone
landgebruik_mafy_oevergebied_jaren <- landgebruik_koppeling_mafy_datums %>%
  left_join(landgebruik_mafy_long, by = c("meetplaats", "landgebruiksjaar"))

# Sla het eindresultaat op
save(landgebruik_mafy_oevergebied_jaren, file = here("data", "verwerkt", "landgebruik", "landgebruik_mafy_oeverzones_jaren.rdata"))

message("✅ Landgebruik voor MAFY oeverzones succesvol berekend en opgeslagen.")
