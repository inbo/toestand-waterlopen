# Vereiste packages
library(terra)
library(tidyverse)
library(readr)
library(sf)
library(qgisprocess)
library(mapview)
library(here)
library(exactextractr)
library(purrr)
library(furrr)
library(progressr)

# 1. Inlezen van de bestanden
meta_data <- read_delim(
  "data/ruw/neerslag/climategrid_pixel_metadata.csv",
  delim = ";",
  locale = locale(decimal_mark = ".")) %>%
  rename(latitude = PIXEL_LAT_CENTER,
         longitude =  PIXEL_LON_CENTER) %>%
  mutate(pixel_id = as.factor(PIXEL_ID)) %>%
  select(-PIXEL_ID)

# Zoek alle bestanden die beginnen met "climategrid_" en eindigen op ".csv"
path <- "data/ruw/neerslag/kmi"
bestandslijst <- list.files(
  path = path,                 # Zoek in de huidige map
  pattern = "climategrid_.*\\.csv$", # Reguliere expressie voor de bestandsnamen
  full.names = TRUE           # Geeft het volledige pad terug
)
gecombineerde_neerslag_data <- map_df(
  .x = bestandslijst,
  .f = read_delim,
  delim = ";",
  locale = locale(decimal_mark = ".")
) %>% # selecteer enkel neerslaghoeveelheid
  select(pixel_id, day, precip_quantity) %>%
  mutate(pixel_id = as.factor(pixel_id)) %>%
  left_join(., meta_data, by = "pixel_id")

### meetpunten macroinvertebraten

mi_meetpunten0 <- st_read(here("data", "ruw", "macroinvertebraten", "mi_meetpunten_datum.gpkg")) %>%
  filter(monsternamedatum > "2006-12-31")

afstroomgebieden <- st_read(here("data", "verwerkt", "hydrologisch", "mi_meetpunten_watersheds_buffered_all.gpkg"))
st_crs(mi_meetpunten0) == st_crs(afstroomgebieden)

mi_meetpunten <- mi_meetpunten0 %>%
  st_drop_geometry()

## alle stroomgebieden koppelen aan de mi_meetpunten

mi_meetpunten_afstroomgebieden <- mi_meetpunten %>%
  left_join(., afstroomgebieden %>%
              select(-oppervlakte),
            by = "meetplaats")

### voor elke sample -> meetplaats en monsternamedatum 10 dagen ervoor en 1 jaar ervoor alle data.

# 1.1 Maak de tijdsreeksen voor élk uniek monsterpunt/datum
# Belangrijk: De 'mi_meetpunten' dataframe bevat al de unieke meetplaats en monsternamedatum.

# voor elke monsternamedatum: 10 dagen ervoor (1 t.e.m. 10)
tien_dagen_ervoor <- mi_meetpunten %>%
  # Voeg een unieke ID toe voor elke monstername (meetplaats + datum)
  mutate(sample_id = paste(meetplaats, monsternamedatum, sep = "_")) %>%

  # Genereer de 10 dagen terug in de tijd voor ELKE sample_id
  rowwise() %>% # Zorg dat de map/mutate per rij werkt
  mutate(
    tien_dagen_ervoor = list(seq(monsternamedatum - days(10), monsternamedatum - days(1), by = "1 day"))
  ) %>%
  ungroup() %>%
  unnest(tien_dagen_ervoor) %>%

  # Hernoem voor gemakkelijke filtering
  rename(day = tien_dagen_ervoor) %>%
  select(sample_id, meetplaats, monsternamedatum, day)

# 1.2 Selecteer ALLE unieke datums die gerasteriseerd moeten worden
alle_datums_nodig <- sort(unique(tien_dagen_ervoor$day))
# Let op: De gefilterde_neerslag_piek moet nu gefilterd worden op basis van deze complete lijst!

# 1.3 Filter de neerslagdata enkel voor de nodige datums en maak er een sf-object van
gefilterde_neerslag_piek <- gecombineerde_neerslag_data %>%
  # Filter op alle unieke datums die we nodig hebben
  filter(day %in% alle_datums_nodig) %>%

  # Maak er een sf-object van om met QGIS te werken
  st_as_sf(
    .,
    coords = c("longitude", "latitude"),
    crs = 4326) %>%
  st_transform(., 31370)

# ==============================================================================
# 2. PARALLELLE RASTERISATIE (Snelheid optimaliseren)
# ==============================================================================

# Zorg voor een parallel plan (bijv. gebruik 4 cores, pas dit aan op basis van uw systeem)
plan(multisession, workers = 4) # Gebruik 4 kernen voor parallelle verwerking

# Als u problemen ervaart, kunt u terugvallen op de sequentiële map_chr:
# plan(sequential)

# De rasterize_dag functie blijft grotendeels hetzelfde, maar gebruikt de nieuwe, correcte datums
rasterize_dag_function <- function(d, sf_data, extent_str) {
  # OPMERKING: progressr::handler_progress(type = "string") kan hier gebruikt worden
  # om de output netter te maken, maar we houden het simpel

  # filter de vector data voor de huidige datum
  v_sub <- sf_data %>% filter(day == d)

  # Tijdelijk bestand
  tmp_output <- tempfile(pattern = "raster_", fileext = ".tif")

  # Rasteriseren met QGIS-algoritme
  qgis_run_algorithm(
    "gdal:rasterize",
    INPUT = v_sub,
    FIELD = "precip_quantity",
    UNITS = 1, # Meters of projectie-eenheden
    WIDTH = 5000,
    HEIGHT = 5000,
    EXTENT = extent_str,
    NODATA = NaN,
    DATA_TYPE = 5, # Float64 voor neerslaghoeveelheid
    OUTPUT = tmp_output
  )

  return(tmp_output)
}

# --- DEFINITIE VAN HET BBOX (zoals in uw code) ---
vlaanderen_lambert <- st_read(here("vlaanderen_lambert.shp")) # Zorg dat u de juiste pad gebruikt
bbox <- st_bbox(vlaanderen_lambert)
extra_margin <- 5000
xmin <- bbox["xmin"] - extra_margin
xmax <- bbox["xmax"] + extra_margin
ymin <- bbox["ymin"] - extra_margin
ymax <- bbox["ymax"] + extra_margin
extent_string <- paste(xmin, xmax, ymin, ymax, sep = ",")
# --------------------------------------------------

# Voer de parallelle rasterisatie uit
start.time <- Sys.time()

raster_files <- future_map_chr(
  .x = alle_datums_nodig,
  .f = rasterize_dag_function,
  sf_data = gefilterde_neerslag_piek,
  extent_str = extent_string
)

end.time <- Sys.time()
time.taken_future <- end.time - start.time
message("Tijd genomen voor alle rasterisaties (parallel): ", time.taken_future)

# 3. Rasterstack en Namen toewijzen
rstack <- rast(raster_files)
names(rstack) <- as.character(alle_datums_nodig)
writeRaster(rstack, file = here("data", "verwerkt", "neerslag", "precipitation_stack_vlaanderen.tif"), overwrite = TRUE)


# ==============================================================================
# 4. EXACTE ZONALE EXTRACTIE (Koppelen aan afstroomgebieden)
# ==============================================================================

# De afstroomgebieden moeten de geometrie bevatten
afstroomgebieden_geo <- afstroomgebieden %>%
  select(meetplaats, geometry)

# Exact Extract uitvoeren op de RasterStack en de Polygonen
# Dit levert een dataframe op met een rij per polygoon en een kolom per dag
neerslag_extractie_resultaat <- exact_extract(
  x = rstack,
  y = afstroomgebieden_geo,
  fun = "sum", # De som is areaal-gewogen, want de functie telt de neerslag*fractie op
  # Als u het totale neerslagvolume/oppervlakte wil, is 'sum' de juiste functie.
  # De functie 'mean' zou het gemiddelde van de neerslaghoeveelheid geven.
  # Aangezien u 'som van de neerslag' vraagt, is 'sum' correct.
  append_cols = c("meetplaats") # Houd de meetplaats-ID vast
) %>%
  as_tibble() %>%
  # Hernoem de kolommen van dag-namen naar een gestructureerd formaat
  pivot_longer(
    cols = starts_with("sum.layer."),
    names_to = "day",
    values_to = "som_neerslag_afstroomgebied"
  ) %>%
  # Datum opschonen: de namen waren de datums
  mutate(day = str_replace(day, "sum.layer.", "") %>% as.Date())


# ==============================================================================
# 5. FINALE KOPPELING EN AGGREGATIE
# ==============================================================================

# Koppel de berekende neerslagdata terug aan de dataframe met de 10 dagen per sample
finale_resultaten <- tien_dagen_ervoor_compleet %>%
  # Join op zowel de meetplaats als de dag
  left_join(
    neerslag_extractie_resultaat,
    by = c("meetplaats", "day")
  ) %>%
  # Aggregeer de 10 dagen neerslag som per unieke sample_id
  group_by(sample_id, meetplaats, monsternamedatum) %>%
  summarise(
    Neerslag_som_10dagen = sum(som_neerslag_afstroomgebied, na.rm = TRUE),
    .groups = "drop"
  )

# Controleer de finale tabel
head(finale_resultaten)
str(finale_resultaten)

##### eigen code #####

# maak voor elke van die datums een rasterlaag

# extent vlaanderen

vlaanderen_lambert <- st_read("vlaanderen_lambert.shp")
st_crs(vlaanderen_lambert) == st_crs(gefilterde_neerslag_piek)
bbox <- st_bbox(vlaanderen_lambert)

# 1.3 Bereken de nieuwe, aangepaste grenzen (+/- 5000m)
extra_margin <- 5000 # 5 km in meters
xmin <- bbox["xmin"] - extra_margin
xmax <- bbox["xmax"] + extra_margin
ymin <- bbox["ymin"] - extra_margin
ymax <- bbox["ymax"] + extra_margin
extent_string <- paste(
  xmin,
  xmax,
  ymin,
  ymax,
  sep = ","
)

## rasterize

alle_datums <- sort(unique(gefilterde_neerslag_piek$day))

rasterize_dag <- function(d) {
  message("Rasteriseren voor datum: ", d)

  v_sub <- gefilterde_neerslag_piek %>% filter(day == d)
  tmp_output <- tempfile(fileext = ".tif")

  qgis_run_algorithm(
    "gdal:rasterize",
    INPUT = v_sub,
    FIELD = "precip_quantity",
    UNITS = 1,
    WIDTH = 5000,
    HEIGHT = 5000,
    EXTENT = extent_string,
    NODATA = NaN,
    DATA_TYPE = 5,
    OUTPUT = tmp_output
  )

  return(tmp_output)
}

start.time <- Sys.time()

raster_files <- purrr::map_chr(alle_datums, function(d) {
  (sprintf("Processing %s", d))
  rasterize_dag(d)
})
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

rstack <- rast(raster_files)
names(rstack) <- as.character(alle_datums)

# 7️⃣ Controle
test <- rstack[[3]]
plot(rstack[[7]], main = names(rstack)[1])
mapview(test) + mapview(afstroomgebieden)
# Optioneel: opslaan
# writeRaster(rstack, "precipitation_stack_vlaanderen.tif", overwrite = TRUE)






neerslag_afstroomgebieden_som <- exact_extract(test_rasterize, afstroomgebieden, "sum")

rasterize_dag(d = "2011-04-27")
test <- rast(tmp_output)
mapview(test)
# code voor 1 dag (vervang door een dag die in jouw data voorkomt

precip <- read_delim(
  "data/ruw/neerslag/climategrid_201301.csv",
  delim = ";",
  locale = locale(decimal_mark = ".")) %>%
  select(pixel_id, day, precip_quantity) %>%
  mutate(pixel_id = as.factor(pixel_id))

precip_joined <- left_join(precip, meta_data, by = "pixel_id")
dag <- "2007-07-20"

dsub <- precip_joined %>% filter(day == dag)

# 8️⃣ Zet punten om naar een SpatVector en rasteriseer
v <- vect(dsub, geom = c("longitude", "latitude"), crs = "EPSG:4326")
v_lambert <- st_as_sf(v) %>%
  st_transform(., 31370)

# st_write(v_lambert, "precip_grid_testdatum_lambert.shp", append = F)

#########
# rasterize
###########
qgis_show_help("gdal:rasterize") # argumenten bekijken
# qgis_algorithms() %>% View

vlaanderen_lambert <- st_read("vlaanderen_lambert.shp")
bbox <- st_bbox(vlaanderen_lambert)

# 1.3 Bereken de nieuwe, aangepaste grenzen (+/- 5000m)
extra_margin <- 5000 # 5 km in meters
xmin <- bbox["xmin"] - extra_margin
xmax <- bbox["xmax"] + extra_margin
ymin <- bbox["ymin"] - extra_margin
ymax <- bbox["ymax"] + extra_margin
extent_string <- paste(
  xmin,
  xmax,
  ymin,
  ymax,
  sep = ","
)

output_path <- "rasterize_test.tif"
qgis_run_algorithm(
  algorithm = "gdal:rasterize",
  INPUT = v_lambert,
  FIELD = "precip_quantity",
  UNITS = 1,
  WIDTH = 5000,
  HEIGHT = 5000,
  EXTENT = extent_string,
  OUTPUT = output_path,
  DATA_TYPE = 5
  )

test_rasterize <- rast("rasterize_test.tif")
mapview(test_rasterize)

####
# zonal statistics
#####
afstroomgebieden <- st_read(here("data", "verwerkt", "hydrologisch", "mi_meetpunten_watersheds_buffered_all.gpkg")) %>%
  st_transform(., crs = crs(test_rasterize))
neerslag_afstroomgebieden_som <- exact_extract(test_rasterize, afstroomgebieden, "sum")


