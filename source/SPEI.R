# ==============================================================================
# 0. INSTELLINGEN EN VEREISTE PACKAGES
# ==============================================================================
library(terra)
library(tidyverse)
library(readr)
library(sf)
library(here)
library(exactextractr)
library(purrr)
library(progressr)
library(lubridate)
library(rlang) # Nodig voor NSE in de SPEI-functie
library(future) # Optioneel, gebruik future_map indien gewenst

# Zorg dat de voortgangsbalk actief is
handlers(global = TRUE)

# ==============================================================================
# 1. DATA INLEZEN EN VOORBEREIDEN (Aanpassing voor Evapotranspiratie)
# ==============================================================================

# 1.1 Neerslag Metadata en Gecombineerde Data inlezen
meta_data <- read_delim(
  here("data", "ruw", "neerslag", "climategrid_pixel_metadata.csv"),
  delim = ";", locale = locale(decimal_mark = ".")) %>%
  rename(latitude = PIXEL_LAT_CENTER, longitude = PIXEL_LON_CENTER) %>%
  mutate(pixel_id = as.factor(PIXEL_ID)) %>% select(-PIXEL_ID)

path_kmi <- here("data", "ruw", "neerslag", "kmi")
bestandslijst <- list.files(path = path_kmi, pattern = "climategrid_.*\\.csv$", full.names = TRUE)

gecombineerde_SPEI_data <- map_df(
  .x = bestandslijst, .f = read_delim, delim = ";", locale = locale(decimal_mark = ".")
) %>%
  # BELANGRIJK: Inclusief evapotrans_ref
  select(pixel_id, day, precip_quantity, evapotrans_ref) %>%
  mutate(pixel_id = as.factor(pixel_id),
         day = as.Date(day),
         # Bereken het dagelijkse neerslagoverschot D op pixelniveau
         D_dagelijks = precip_quantity - evapotrans_ref) %>%
  left_join(., meta_data, by = "pixel_id")


# 1.2 Meetpunten en Afstroomgebieden inlezen & Filteren
mi_meetpunten0 <- st_read(here("data", "ruw", "macroinvertebraten", "mi_meetpunten_datum.gpkg")) %>%
  filter(monsternamedatum > "2006-12-31") %>%
  mutate(monsternamedatum = as.Date(monsternamedatum))

afstroomgebieden_geo <- st_read(here("data", "verwerkt", "hydrologisch", "mi_meetpunten_watersheds_buffered_5000m.gpkg")) %>%
  select(meetplaats, geom) %>%
  mutate(meetplaats = as.character(meetplaats))

samples_met_geo <- mi_meetpunten0 %>%
  st_drop_geometry() %>% select(meetplaats, monsternamedatum) %>%
  distinct(meetplaats, monsternamedatum, .keep_all = TRUE) %>%
  mutate(meetplaats = as.character(meetplaats)) %>%
  left_join(., afstroomgebieden_geo, by = "meetplaats") %>%
  st_as_sf() %>%
  filter(!st_is_empty(.)) # Filter samples zonder geldige geometrie


# 1.3 Definitie van het SpatRaster Template
vlaanderen_lambert <- st_read(here("data", "ruw", "vlaanderen", "vlaanderen_wgs84.shp"))
bbox_sf <- st_bbox(vlaanderen_lambert)
res_meters <- 5000
crs_lambert <- "EPSG:31370"

extent_spat_temp <- rast(
  xmin = bbox_sf["xmin"] - res_meters, xmax = bbox_sf["xmax"] + res_meters,
  ymin = bbox_sf["ymin"] - res_meters, ymax = bbox_sf["ymax"] + res_meters,
  resolution = res_meters, crs = crs_lambert
)

# ==============================================================================
# 2. SPEI REFERENTIEBEREKENING (EENMALIG, 1991-2020)
# ==============================================================================

message("\nStarten van de SPEI Referentieberekening (1991-2020)...")

# Definieer de referentieperiode
datum_start_ref <- as.Date("1991-01-01")
datum_eind_ref <- as.Date("2020-12-31")

# 2.1 Filter de data voor de referentieperiode
data_ref <- gecombineerde_SPEI_data %>%
  filter(day >= datum_start_ref, day <= datum_eind_ref)

# 2.2 Bereken de SOM van D (P-E) over 365 dagen, voor elke dag in de referentieperiode
# Dit is een 'rolling sum' op pixelniveau. Dit is de meest intense rekenstap.
# Door de complexiteit van rolling sums op grid data (tijd+ruimte), is de Stack methode
# hier het meest robuust, maar we doen het sequentiëel:

dagen_ref <- sort(unique(data_ref$day))

bereken_D_som_dag <- function(d, data_ref_df) {
  # Bepaal het 365-dagen venster
  start_dag <- d - days(364)

  # Aggregeer de dagelijkse D (P-E) over 365 dagen per PIXEL
  D_gesommeerd <- data_ref_df %>%
    filter(day >= start_dag, day <= d) %>%
    group_by(pixel_id, latitude, longitude) %>%
    summarise(D_365d_som = sum(D_dagelijks, na.rm = TRUE), .groups = "drop") %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    st_transform(., 31370)

  # Rasteriseer de gesommeerde D
  r_som_D <- terra::rasterize(x = D_gesommeerd, y = extent_spat_temp, field = "D_365d_som", fun = "sum")

  return(r_som_D)
}

# Rasterizeer ALLE 365-daagse D-sommen voor de referentieperiode (zeer langzaam!)
# We nemen een representatieve subset van de referentiedatums om de complexiteit te verminderen
# om de historische statistieken te schatten (bijv. 1 dag per maand)
ref_datums_subset <- days_ref[day(days_ref) == 1]

# U kunt 'future_map' hier gebruiken om de snelheid te verhogen
rstack_D_ref <- purrr::map(ref_datums_subset, bereken_D_som_dag, data_ref_df = data_ref) %>% rast()
names(rstack_D_ref) <- as.character(ref_datums_subset)

# 2.3 Exacte Extractie van D-sommen en Berekening van Statistische Parameters
historische_D_extractie <- exact_extract(
  x = rstack_D_ref,
  y = afstroomgebieden_geo,
  fun = c("mean", "sd"), # Gemiddelde en Standaardafwijking van D-sommen over de referentieperiode
  append_cols = c("meetplaats")
) %>%
  as_tibble() %>%
  # We gebruiken hier 'mean' en 'sd' van de extractie resultaten om de SPEI parameters te schatten
  rename(D_mean_ref = mean, D_sd_ref = sd) %>%
  select(meetplaats, D_mean_ref, D_sd_ref)

message("SPEI Referentieparameters (historische Mean en SD) berekend.")

# ==============================================================================
# 3. ITERATIEVE SPEI-BEREKENING PER SAMPLE
# ==============================================================================

# Functie om de volledige bewerking per unieke sample uit te voeren
bereken_SPEI_per_sample_iteratief <- function(sample_row, D_ref_params) {

  # 1. Bepaal de periodes
  meetplaats_id <- sample_row$meetplaats
  datum_eind <- sample_row$monsternamedatum
  datum_start_365d <- datum_eind - years(1)

  # 2. Filter D-data voor de actuele 365-dagen periode
  data_365d_periode <- gecombineerde_SPEI_data %>%
    filter(day >= datum_start_365d, day < datum_eind)

  # 3. Aggregeer de SOM van D over 365 dagen per PIXEL (Actueel)
  gesommeerde_D_punten <- data_365d_periode %>%
    group_by(pixel_id, latitude, longitude) %>%
    summarise(D_sum_365d = sum(D_dagelijks, na.rm = TRUE), .groups = "drop") %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    st_transform(., 31370)

  # 4. Rasteriseer D_sum
  r_som_D_365d <- terra::rasterize(x = gesommeerde_D_punten, y = extent_spat_temp, field = "D_sum_365d", fun = "sum")

  # 5. Extracteer de som van D (D_zone) voor de huidige sample
  afstroomgebied_poly <- sample_row %>% select(geom)
  D_zone_actueel <- exact_extract(x = r_som_D_365d, y = afstroomgebied_poly, fun = "sum", force_df = TRUE)$sum

  # 6. BEREKEN SPEI
  # Haal de historische parameters op voor de huidige meetplaats
  ref_params <- D_ref_params %>%
    filter(meetplaats == meetplaats_id)

  D_mean <- ref_params$D_mean_ref[1]
  D_sd <- ref_params$D_sd_ref[1]

  # Standaardiseer de actuele D-som (SPEI Index)
  # Formule: Z = (X - Mu) / Sigma
  SPEI_waarde <- (D_zone_actueel - D_mean) / D_sd

  # 7. Retourneer de resultaten
  return(
    tibble(
      meetplaats = meetplaats_id,
      monsternamedatum = datum_eind,
      Neerslagoverschot_D_365d = D_zone_actueel,
      SPEI_365d = SPEI_waarde
    )
  )
}

# ==============================================================================
# 4. FINALE UITVOERING (SPEI)
# ==============================================================================

message("\nStarten finale Iteratieve SPEI-berekening (Actuele waarden)...")

# Sequentiële Uitvoering met Voortgangsbalk
# U kunt dit vervangen door future_map voor snelheid!
with_progress({
  p <- progressor(steps = nrow(samples_met_geo))

  finale_SPEI_resultaten_lijst <- purrr::map(seq_len(nrow(samples_met_geo)), ~ {
    p(sprintf("Berekening SPEI sample %d van %d", .x, nrow(samples_met_geo)))
    bereken_SPEI_per_sample_iteratief(samples_met_geo[.x, ], D_ref_params = historische_D_extractie)
  })
})

finale_SPEI_resultaten <- bind_rows(finale_SPEI_resultaten_lijst)

# Controleer de resultaten
head(finale_SPEI_resultaten)
