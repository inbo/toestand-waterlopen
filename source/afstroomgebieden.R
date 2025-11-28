# =====================================================================
# 1. Packages en setup
# =====================================================================
source(here::here("source", "inladen_packages.R"))

# Opmerkingen:
# - Whitebox tools moet geïnstalleerd zijn (zie instructies in commentaar)
# - QGIS-plugins moeten correct gelinkt zijn

# --- Stap 1: Data inlezen en standaardiseren ---

# Lees MI meetpunten en behoud alleen de naam en de geometrie
mi_meetpunten <- st_read(here("data", "ruw", "macroinvertebraten", "mi_meetpunten.gpkg"), quiet = TRUE) %>%
  select(meetplaats)

# Lees FC meetpunten en behoud alleen de naam en de geometrie
# (We halen de 'unique()' hier nog even weg, dat doen we pas na het samenvoegen)
fc_meetpunten <- st_read(here("data", "ruw", "fys_chem", "fc_meetpunten.gpkg"), quiet = TRUE) %>%
  select(meetplaats)

# --- Stap 2: Zorg voor gelijke CRS (Coördinatensysteem) ---

# Het is cruciaal dat beide dezelfde CRS hebben voor bind_rows.
# We controleren het en transformeren indien nodig naar de CRS van de eerste set.
if (st_crs(mi_meetpunten_std) != st_crs(fc_meetpunten_std)) {
  message("Coördinatensystemen verschillen. FC wordt getransformeerd naar MI CRS.")
  target_crs <- st_crs(mi_meetpunten_std)
  fc_meetpunten_std <- st_transform(fc_meetpunten_std, target_crs)
}

# --- Stap 3, 4 & 5: Samenvoegen, uniek maken en filteren op lege geometrie ---

unieke_meetpunten_met_coords <- bind_rows(mi_meetpunten_std, fc_meetpunten_std) %>%
  # Zorg dat we alleen unieke meetplaatsnamen overhouden.
  # .keep_all = TRUE zorgt dat de geometrie van de eerste 'hit' bewaard blijft.
  distinct(meetplaats, .keep_all = TRUE) %>%
  # Filter punten eruit die geen geometrie hebben (lege punten)
  # st_is_empty geeft TRUE als er geen coördinaten zijn, dus we willen de '!' (niet) empty
  filter(!st_is_empty(st_geometry(.)))


# --- Controle ---

# Bekijk het resultaat
print(unieke_meetpunten_met_coords)

# Controleer hoeveel punten je overhoudt
nrow(unieke_meetpunten_met_coords)

# Optioneel: plot om te checken
plot(st_geometry(unieke_meetpunten_met_coords), pch = 20, col = 'blue')

# =====================================================================
# 2. DEM inladen en voorbereiden
# =====================================================================
dtm_hydro_breached <- rast(here("data", "ruw", "dem",
                                "DHMVIIDTMRAS025mto50m_breachedDTM.tif"))

# =====================================================================
# 3. Flow accumulation berekenen
# =====================================================================
if (!file.exists(here("data", "verwerkt", "hydrologisch",
                      "dhmvii_dtm_50m_d8_flow.tif"))) {
  d8_flow <- qgis_run_algorithm(
    "wbt:D8FlowAccumulation",
    input = here("data", "ruw", "dem",
                 "DHMVIIDTMRAS025mto50m_breachedDTM.tif"),
    output = here("data", "verwerkt", "hydrologisch",
                  "dhmvii_dtm_50m_d8_flow.tif"),
    .quiet = TRUE
  )
}

# =====================================================================
# 4. D8 pointer berekenen
# =====================================================================
if (!file.exists(here("data", "verwerkt", "hydrologisch",
                      "dhmvii_dtm_50m_d8_pointer.tif"))) {
  d8_pointer <- qgis_run_algorithm(
    "wbt:D8Pointer",
    dem = here("data", "ruw", "dem",
               "DHMVIIDTMRAS025mto50m_breachedDTM.tif"),
    output = here("data", "verwerkt", "hydrologisch",
                  "dhmvii_dtm_50m_d8_pointer.tif"),
    .quiet = TRUE
  )
}

# =====================================================================
# 5. Extractie van waterlopen (streams)
# =====================================================================
threshold <- 100
stream_path <- here("data", "verwerkt", "hydrologisch",
                    paste0("dhmvii_dtm_50m_streams_t", threshold, ".tif"))

if (!file.exists(stream_path)) {
  streams <- qgis_run_algorithm(
    "wbt:ExtractStreams",
    flow_accum = here("data", "verwerkt", "hydrologisch",
                      "dhmvii_dtm_50m_d8_flow.tif"),
    threshold = threshold,
    output = stream_path,
    .quiet = TRUE
  )
}

# =====================================================================
# 6. Streams raster → vector
# =====================================================================
qgis_run_algorithm(
  "wbt:RasterStreamsToVector",
  streams = stream_path,
  d8_pntr = here("data", "verwerkt", "hydrologisch",
                 "dhmvii_dtm_50m_d8_pointer.tif"),
  output = here("data", "verwerkt", "hydrologisch",
                "hydro_dtm_stream_network.shp"),
  .quiet = TRUE
)

streams_sf <- st_read(here("data", "verwerkt", "hydrologisch",
                           "hydro_dtm_stream_network.shp"))
st_crs(streams_sf) <- "EPSG:31370"

# =====================================================================
# 7. Meetpunten snappen op waterlopen
# =====================================================================
if (!file.exists(here("data", "verwerkt", "hydrologisch",
                      "mi_meetpunten_snapped_to_streams.shp"))) {

  snapped <- qgis_run_algorithm(
    "wbt:JensonSnapPourPoints",
    pour_pts = here("data", "ruw", "macroinvertebraten",
                    "mi_meetpunten.gpkg"),
    streams = stream_path,
    snap_dist = 300,
    output = here("data", "verwerkt", "hydrologisch",
                  "mi_meetpunten_snapped_to_streams.shp"),
    .quiet = TRUE
  )
}

# =====================================================================
# 8. Afstroomgebieden bepalen (nested watersheds)
# =====================================================================
if (!file.exists(here("data", "verwerkt", "hydrologisch",
                      "mi_meetpunten_watersheds_nested_1.tif"))) {
  watersheds_nested <- qgis_run_algorithm(
    "wbt:UnnestBasins",
    d8_pntr = here("data", "verwerkt", "hydrologisch",
                   "dhmvii_dtm_50m_d8_pointer.tif"),
    pour_pts = here("data", "verwerkt", "hydrologisch",
                    "mi_meetpunten_snapped_to_streams.shp"),
    output = here("data", "verwerkt", "hydrologisch",
                  "mi_meetpunten_watersheds_nested.tif"),
    .quiet = TRUE
  )
}

# =====================================================================
# 9. Afstroomgebieden converteren naar polygonen
# =====================================================================
watershed_files <- paste0("mi_meetpunten_watersheds_nested_", 1:61, ".tif")

watersheds_list <- map(watershed_files, ~ {
  rast(here("data", "verwerkt", "hydrologisch", .x)) %>%
    as.polygons() %>%
    st_as_sf() %>%
    rename(rowname = !!rlang::sym(tools::file_path_sans_ext(.x)))
})

watersheds_nested <- bind_rows(watersheds_list)

# =====================================================================
# 10. Koppelen met meetpunten
# =====================================================================
locations <- read_sf(here("data", "verwerkt", "hydrologisch",
                          "mi_meetpunten_snapped_to_streams.shp"))

watersheds_nested <- watersheds_nested %>%
  inner_join(
    locations %>%
      rownames_to_column() %>%
      st_drop_geometry() %>%
      mutate(rowname = as.integer(rowname))
  ) %>%
  mutate(oppervlakte = st_area(geometry))

# =====================================================================
# 11. Opslaan van gecombineerde afstroomgebieden
# =====================================================================
if (!file.exists(here("data", "verwerkt", "hydrologisch",
                      "mi_meetpunten_watersheds_nested_all.gpkg"))) {
  st_write(watersheds_nested %>% select(-fid),
           here("data", "verwerkt", "hydrologisch",
                "mi_meetpunten_watersheds_nested_all.gpkg"),
           delete_dsn = TRUE)
}

watersheds_nested <- st_read(here("data", "verwerkt", "hydrologisch",
                                  "mi_meetpunten_watersheds_nested_all.gpkg"))

# =====================================================================
# 12. Beperk afstroomgebieden met buffers (meerdere afstanden)
# =====================================================================
buffer_afstanden <- c(100, 250, 500, 1000, 2500, 5000)

for (afstand in buffer_afstanden) {

  # Buffers aanmaken rond meetpunten
  locations_buffer <- locations %>%
    mutate(buffer = st_buffer(geometry, dist = afstand))

  buffers <- locations_buffer %>%
    st_drop_geometry() %>%
    mutate(geometry = st_sfc(buffer, crs = crs(watersheds_nested))) %>%
    select(-buffer) %>%
    st_as_sf()

  # Zorgen dat CRS gelijk is
  watersheds <- st_transform(watersheds_nested, st_crs(buffers$geometry))

  # Matchen op meetplaats
  idx <- match(buffers$meetplaats, watersheds$meetplaats)
  if (any(is.na(idx))) {
    warning(paste0("Sommige meetplaats-ID's komen niet overeen (buffer ", afstand, " m)"))
  }

  # Intersectie uitvoeren
  intersections <- map2(st_geometry(buffers), st_geometry(watersheds)[idx], st_intersection)

  # Nieuwe geometrie aan buffers toevoegen
  result <- buffers %>%
    mutate(geometry = st_sfc(intersections, crs = st_crs(buffers))) %>%
    filter(!st_is_empty(geometry)) %>%
    mutate(oppervlakte = st_area(geometry)) %>%
    select(-fid)

  # Opslaan per bufferafstand
  output_path <- here("data", "verwerkt", "hydrologisch",
                      paste0("mi_meetpunten_watersheds_buffered_", afstand, "m.gpkg"))

  st_write(result, output_path, delete_dsn = TRUE)
}

# =====================================================================
# EINDE SCRIPT
# =====================================================================

afstroomgebied_buffered_100m <- st_read(here("data", "verwerkt", "hydrologisch","mi_meetpunten_watersheds_buffered_100m.gpkg"))

