source(here::here("source", "inladen_packages.R"))
load(here("data", "verwerkt", "mi_data.rdata"))

# Laad de meetpunten
meetpunten_locaties <- st_read(here("data", "verwerkt", "hydrologisch", "ow_meetpunten_snapped_to_streams.shp"))
jaren <- c(2013, 2016, 2019, 2022)

# --- HELPER FUNCTIES ---

# 1. Functie om pixelwaarden om te zetten in percentages
convert_pixels_to_percentages <- function(data) {
  data <- data %>%
    mutate(
      total_pixels = rowSums(across(starts_with("VALUE_")), na.rm = TRUE),
      # Voorkom delen door 0 als er geen pixels zijn (zou NaN geven)
      total_pixels = ifelse(total_pixels == 0, 1, total_pixels),
      across(starts_with("VALUE_"), ~ .x / total_pixels * 100, .names = "{.col}_pct")
    )
  return(data)
}

# 2. NIEUW: Functie om ontbrekende kolommen toe te voegen (vult met 0)
ensure_all_columns <- function(data) {
  # We verwachten classes 1 t/m 25 (o.b.v. jouw reclass logica)
  expected_cols <- paste0("VALUE_", 1:25)

  for (col in expected_cols) {
    if (!col %in% names(data)) {
      data[[col]] <- 0 # Maak kolom aan en zet op 0
    }
  }
  return(data)
}

# 3. Functie om te hergroeperen (ongewijzigd, maar werkt nu veilig)
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

# --- HOOFDFUNCTIE ---

analyse_landgebruik_buffers <- function(meetpunten, straal = 100, jaren_vector = c(2013, 2016, 2019, 2022)) {

  message(paste0("Start analyse voor buffers van ", straal, " meter..."))

  # FIX FID: Verwijder 'fid' kolommen om GDAL errors te voorkomen
  valid_columns <- names(meetpunten)[!grepl("^fid$", names(meetpunten), ignore.case = TRUE)]
  meetpunten <- meetpunten %>% select(all_of(valid_columns))

  resultaten_lijst <- list()

  for (jaar in jaren_vector) {
    message(paste0("  Bezig met jaar: ", jaar))

    input_raster_path <- here("data", "ruw", "landgebruik", paste0("niveau1_vla_", jaar, "_v3.tif"))
    output_table_path <- here("data", "verwerkt", "landgebruik", paste0("zonal_histogram_buffer_", straal, "m_", jaar, ".gpkg"))

    if (!file.exists(input_raster_path)) stop(paste("Raster niet gevonden:", input_raster_path))
    landuse_raster <- raster::raster(input_raster_path)

    # Transformatie en Buffer
    meetpunten_trans <- st_transform(meetpunten, crs = crs(landuse_raster))
    meetpunten_buffered <- st_buffer(meetpunten_trans, dist = straal)

    # Berekening in QGIS
    if (!file.exists(output_table_path)) {
      landuse_raster[landuse_raster < 1 | landuse_raster > 25] <- NA
      qgis_run_algorithm("native:zonalhistogram",
                         INPUT_RASTER = landuse_raster,
                         INPUT_VECTOR = meetpunten_buffered,
                         RASTER_BAND = 1,
                         COLUMN_PREFIX = "VALUE_",
                         OUTPUT = output_table_path)
    }

    # Inlezen
    buffer_landuse0 <- st_read(output_table_path, quiet = TRUE)

    # STAP 1: Zorg dat alle kolommen bestaan (vullen met 0 indien afwezig)
    buffer_landuse_fixed <- ensure_all_columns(buffer_landuse0)

    # STAP 2: Converteer naar percentages
    buffer_landuse_pct <- convert_pixels_to_percentages(data = buffer_landuse_fixed)

    # STAP 3: Hergroepeer
    reclassified_data <- landuse_reclass(buffer_landuse_pct, as.character(jaar))

    resultaten_lijst[[as.character(jaar)]] <- reclassified_data
  }

  # Samenvoegen resultaten
  finale_df <- meetpunten %>%
    st_drop_geometry() %>%
    select(meetplaats)

  for (jaar in jaren_vector) {
    finale_df <- finale_df %>%
      inner_join(resultaten_lijst[[as.character(jaar)]], by = "meetplaats")
  }

  return(finale_df)
}

# --- UITVOEREN ---

# 1. Analyse draaien
finale_landgebruik_buffers <- analyse_landgebruik_buffers(meetpunten_locaties, straal = 100, jaren_vector = jaren)

# 2. Koppelen aan meetjaren (zoals in je originele script)
landgebruik_koppeling0 <- mi_data %>%
  select(meetplaats, monsternamedatum) %>%
  mutate(
    landgebruiksjaar = as.character(case_when(
      monsternamedatum < ymd("2014-07-01") ~ 2013,
      monsternamedatum < ymd("2017-07-01") ~ 2016,
      monsternamedatum < ymd("2020-07-01") ~ 2019,
      TRUE ~ 2022
    )
    )) %>%
  st_drop_geometry()

landgebruik_koppeling1 <- finale_landgebruik_buffers %>%
  pivot_longer(
    cols = -c(meetplaats),
    names_to = c("landgebruik", "landgebruiksjaar"),
    names_pattern = "(.*)_([0-9]{4})$"
  ) %>%
  pivot_wider(
    names_from = landgebruik,
    values_from = value
  )

landgebruik_buffer_100m_jaren <- landgebruik_koppeling0 %>%
  left_join(landgebruik_koppeling1, by = c("meetplaats", "landgebruiksjaar"))

# Opslaan
save(landgebruik_buffer_100m_jaren, file = here("data", "verwerkt", "landgebruik", "landgebruik_buffer_100m_jaren.rdata"))
message("Klaar!")
