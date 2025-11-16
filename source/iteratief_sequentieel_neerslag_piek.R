# ==============================================================================
# 0. VOORBEREIDING EN SETUP
# ==============================================================================
start_seq_total <- Sys.time()
library(terra)
library(tidyverse)
library(readr)
library(sf)
library(qgisprocess)
library(here)
library(exactextractr)
library(purrr)
library(progressr) # Nodig voor de voortgangsbalk
library(lubridate) # Nodig voor days()

# Activeer de progress bar handler om deze zichtbaar te maken
handlers(global = TRUE)
# U kunt ook een specifieke handler instellen, bijv. handlers(handler_progress(type = "text"))

# ==============================================================================
# 1. DATA INLEZEN EN VOORBEREIDEN (Ongewijzigd)
# ==============================================================================

# 1.1 Neerslag Metadata en Gecombineerde Data inlezen
meta_data <- read_delim(
  here("data", "ruw", "neerslag", "climategrid_pixel_metadata.csv"),
  delim = ";",
  locale = locale(decimal_mark = ".")) %>%
  rename(latitude = PIXEL_LAT_CENTER, longitude = PIXEL_LON_CENTER) %>%
  mutate(pixel_id = as.factor(PIXEL_ID)) %>%
  select(-PIXEL_ID)

path_kmi <- here("data", "ruw", "neerslag", "kmi")
bestandslijst <- list.files(
  path = path_kmi, pattern = "climategrid_.*\\.csv$", full.names = TRUE
)

gecombineerde_neerslag_data <- map_df(
  .x = bestandslijst, .f = read_delim, delim = ";", locale = locale(decimal_mark = ".")
) %>%
  select(pixel_id, day, precip_quantity) %>%
  mutate(pixel_id = as.factor(pixel_id),
         day = as.Date(day)) %>%
  left_join(., meta_data, by = "pixel_id")


# 1.2 Meetpunten en Afstroomgebieden inlezen & Filteren
mi_meetpunten0 <- st_read(here("data", "ruw", "macroinvertebraten", "mi_meetpunten_datum.gpkg")) %>%
  filter(monsternamedatum > "2006-12-31") %>%
  mutate(monsternamedatum = as.Date(monsternamedatum))

afstroomgebieden_geo <- st_read(here("data", "verwerkt", "hydrologisch", "mi_meetpunten_watersheds_buffered_all.gpkg")) %>%
  select(meetplaats, geom) %>%
  mutate(meetplaats = as.character(meetplaats))

samples_met_geo0 <- mi_meetpunten0 %>%
  st_drop_geometry() %>%
  select(meetplaats, monsternamedatum) %>%
  distinct(meetplaats, monsternamedatum, .keep_all = TRUE) %>%
  mutate(meetplaats = as.character(meetplaats)) %>%
  left_join(., afstroomgebieden_geo, by = "meetplaats") %>%
  st_as_sf()

samples_met_geo <- samples_met_geo0 %>%
  filter(!st_is_empty(.)) # Filter samples zonder geldige geometrie

n_verwijderd <- nrow(samples_met_geo0) - nrow(samples_met_geo)
message(sprintf("%d samples zonder afstroomgebied (lege geometrie) zijn verwijderd uit de analyse.", n_verwijderd))


# 1.3 Definitie van het SpatRaster Template
vlaanderen_lambert <- st_read(here("vlaanderen_lambert.shp"))
bbox_sf <- st_bbox(vlaanderen_lambert)
res_meters <- 5000
crs_lambert <- "EPSG:31370"

extent_spat_temp <- rast(
  xmin = bbox_sf["xmin"] - res_meters,
  xmax = bbox_sf["xmax"] + res_meters,
  ymin = bbox_sf["ymin"] - res_meters,
  ymax = bbox_sf["ymax"] + res_meters,
  resolution = res_meters,
  crs = crs_lambert
)

# ==============================================================================
# 2. ITERATIEVE FUNCTIE (10 Dagen & 1 Jaar) - Gebruikt in de map
# ==============================================================================

# Functie om de volledige bewerking per unieke sample uit te voeren (Iteratieve Methode)
bereken_neerslag_per_sample_iteratief_365d <- function(sample_row) {

  # Progressor aanroep (Nodig voor de voortgangsbalk, voegt een lichte overhead toe)
  p <- progressor(steps = 1)
  p()

  # 1. Bepaal de periodes
  meetplaats_id <- sample_row$meetplaats
  datum_eind <- sample_row$monsternamedatum

  datum_start_10d <- datum_eind - days(10)
  datum_start_365d <- datum_eind - years(1)

  # 2. Filter de ruwe data voor de GROOTSTE periode (365 dagen)
  data_365d_periode <- gecombineerde_neerslag_data %>%
    filter(day >= datum_start_365d, day < datum_eind)

  afstroomgebied_poly <- sample_row %>% select(geom)

  # --- CALCULATIE 1: 365 Dagen Som ---
  gesommeerde_punten_365d <- data_365d_periode %>%
    group_by(pixel_id, latitude, longitude) %>%
    summarise(precip_sum_365d = sum(precip_quantity, na.rm = TRUE), .groups = "drop") %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    st_transform(., 31370)

  r_som_365d <- terra::rasterize(x = gesommeerde_punten_365d, y = extent_spat_temp, field = "precip_sum_365d", fun = "sum")
  resultaat_365d <- exact_extract(x = r_som_365d, y = afstroomgebied_poly, fun = "sum", force_df = TRUE)$sum

  # --- CALCULATIE 2: 10 Dagen Som ---
  gesommeerde_punten_10d <- data_365d_periode %>%
    filter(day >= datum_start_10d, day < datum_eind) %>%
    group_by(pixel_id, latitude, longitude) %>%
    summarise(precip_sum_10d = sum(precip_quantity, na.rm = TRUE), .groups = "drop") %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    st_transform(., 31370)

  r_som_10d <- terra::rasterize(x = gesommeerde_punten_10d, y = extent_spat_temp, field = "precip_sum_10d", fun = "sum")
  resultaat_10d <- exact_extract(x = r_som_10d, y = afstroomgebied_poly, fun = "sum", force_df = TRUE)$sum

  # 3. Retourneer beide resultaten
  return(
    tibble(
      meetplaats = meetplaats_id,
      monsternamedatum = datum_eind,
      neerslag_som_10dagen = resultaat_10d,
      neerslag_som_1jaar = resultaat_365d
    )
  )
}

# ==============================================================================
# 3. SEQUENTIËLE UITVOERING MET VOORTGANGBALK
# ==============================================================================

message("Starten van de Sequentiële Iteratieve Methode voor alle samples...")

# Gebruik with_progress om de voortgangsbalk te tonen
start_seq_total <- Sys.time()
with_progress({

  # Stel de progressor in voor het totale aantal stappen
  p <- progressor(steps = nrow(samples_met_geo))

  # Voer de map-functie uit, roep de progressor aan in de functie (zie hierboven)
  resultaten_lijst_seq <- purrr::map(seq_len(nrow(samples_met_geo)), ~ {
    # Roep de progressor aan voor elke iteratie
    p(sprintf("Verwerking sample %d van %d", .x, nrow(samples_met_geo)))

    # Voer de kernberekening uit
    bereken_neerslag_per_sample_iteratief_365d(samples_met_geo[.x, ])
  })
})

# Combineer de resultatenlijst tot één dataframe
finale_resultaten_sequentieel <- bind_rows(resultaten_lijst_seq) %>%
  mutate(sample_id = paste(meetplaats, monsternamedatum, sep = "_")) %>%
  select(sample_id, meetplaats, monsternamedatum, neerslag_som_10dagen, neerslag_som_1jaar)

eind_seq_total <- Sys.time()
time_seq_total <- eind_seq_total - start_seq_total


# ==============================================================================
# 4. RESULTAAT
# ==============================================================================

message(sprintf("\nKlaar! Totale sequentiële tijd (%d samples): %.2f seconden.",
                nrow(samples_met_geo), as.numeric(time_seq_total)))

# Controleer de finale tabel
head(finale_resultaten_sequentieel)
str(finale_resultaten_sequentieel)

# Opslaan van de resultaten
save(finale_resultaten_sequentieel, file = here("data", "verwerkt", "neerslag_beide_periodes.rdata"))
