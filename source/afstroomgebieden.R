source(here::here("source", "inladen_packages.R"))
# whitebox tools installation:
# install.packages("whitebox")
# whitebox::install_whitebox()
# QGIS plugins install search for whitebox and install the plugin
# Go to preferences in QGIS select processing select whitebox and fill in path
# by searching for whitebox_tools.exe file

# Help functies voor qgis process

# qgis_algorithms() %>% View # algoritmes bekijken
# qgis_show_help("native:snapgeometries") # argumenten bekijken

# afstroomgebieden voor alle meetpunten bepalen ----
dtm_hydro_breached <- rast(here("data", "ruw", "dem", "DHMVIIDTMRAS025mto50m_breachedDTM.tif"))

# flow accumulation
#qgis_arguments("wbt:D8FlowAccumulation")
if (!file.exists(here("data", "verwerkt", "hydrologisch", "dhmvii_dtm_50m_d8_flow.tif"))) {
  d8_flow <- qgis_run_algorithm(
    "wbt:D8FlowAccumulation",
    input = here("data", "ruw", "dem", "DHMVIIDTMRAS025mto50m_breachedDTM.tif"),
    output = here("data", "verwerkt", "hydrologisch", "dhmvii_dtm_50m_d8_flow.tif"),
    .quiet = TRUE)
}

# d8 pointer
#qgis_arguments("wbt:D8Pointer")
if (!file.exists(here("data", "verwerkt", "hydrologisch", "dhmvii_dtm_50m_d8_pointer.tif"))) {
d8_pointer <- qgis_run_algorithm(
  "wbt:D8Pointer",
  dem = here("data", "ruw", "dem", "DHMVIIDTMRAS025mto50m_breachedDTM.tif"),
  output = here("data", "verwerkt", "hydrologisch", "dhmvii_dtm_50m_d8_pointer.tif"),
  .quiet = TRUE)
}

# extract streams
#qgis_arguments("wbt:ExtractStreams")
threshold <- 100
if (!file.exists(here("data", "verwerkt", "hydrologisch",
                      paste0("dhmvii_dtm_50m_streams_t",
                             threshold,".tif")))) {
streams <- qgis_run_algorithm(
  "wbt:ExtractStreams",
  flow_accum = here("data", "verwerkt", "hydrologisch", "dhmvii_dtm_50m_d8_flow.tif"),
  threshold = threshold,
  output = here("data", "verwerkt", "hydrologisch",
                paste0("dhmvii_dtm_50m_streams_t",
                       threshold,".tif")),
  .quiet = TRUE
)
}

# omzetten raster streams tot een streamsvector
#qgis_arguments("wbt:RasterStreamsToVector")
qgis_run_algorithm(
  "wbt:RasterStreamsToVector",
  streams = here("data", "verwerkt", "hydrologisch", paste0("dhmvii_dtm_50m_streams_t",
                                              threshold,".tif")),
  d8_pntr = here("data", "verwerkt", "hydrologisch", "dhmvii_dtm_50m_d8_pointer.tif"),
  output = here("data", "verwerkt", "hydrologisch", "hydro_dtm_stream_network.shp"),
  .quiet = TRUE)

streams_sf <- st_read(here("data", "verwerkt", "hydrologisch", "hydro_dtm_stream_network.shp"))
st_crs(streams_sf) <- "EPSG:31370"

#plot(rast(here("data", "dem", paste0("dhmvii_dtm_50m_streams_t", threshold,".tif"))), maxcell = 2500000, col = "black")
# setting pour points

#qgis_arguments("wbt:JensonSnapPourPoints")
if (!file.exists(here("data", "verwerkt", "hydrologisch", "mi_meetpunten_snapped_to_streams.shp"))) {
snapped <- qgis_run_algorithm(
  "wbt:JensonSnapPourPoints",
  pour_pts = here("data", "ruw", "macroinvertebraten", "mi_meetpunten.gpkg"),
  streams = here("data", "verwerkt", "hydrologisch", paste0("dhmvii_dtm_50m_streams_t",
                                       threshold,".tif")),
  snap_dist = 300,
  output = here("data", "verwerkt", "hydrologisch", "mi_meetpunten_snapped_to_streams.shp"),
  .quiet = TRUE
)
}

# delineate watersheds
# om nested watersheds te krijgen -> realistischer dan unnested
#qgis_arguments("wbt:UnnestBasins")

if (!file.exists(here("data", "verwerkt", "hydrologisch", "mi_meetpunten_watersheds_nested_1.tif"))) {
  watersheds_nested <- qgis_run_algorithm(
  "wbt:UnnestBasins",
  d8_pntr = here("data", "verwerkt", "hydrologisch", "dhmvii_dtm_50m_d8_pointer.tif"),
  pour_pts =
    here("data", "verwerkt", "hydrologisch", "mi_meetpunten_snapped_to_streams.shp"),
  output = here("data", "verwerkt", "hydrologisch", "mi_meetpunten_watersheds_nested.tif"),
  .quiet = TRUE
)
}
# Generate file paths dynamically
watershed_files <- paste0("mi_meetpunten_watersheds_nested_", 1:61, ".tif")

# Convert raster watersheds to polygons in a loop
watersheds_list <- map(watershed_files, ~ {
  rast(here("data", "verwerkt", "hydrologisch", .x)) %>%
    as.polygons() %>%
    st_as_sf() %>%
    rename(rowname = !!rlang::sym(tools::file_path_sans_ext(.x)))  # Rename column dynamically
})

# Combine all watersheds into one dataframe
watersheds_nested <- bind_rows(watersheds_list)

# Load locations
locations <- read_sf(here("data", "verwerkt", "hydrologisch", "mi_meetpunten_snapped_to_streams.shp"))

# Join locations to watersheds
watersheds_nested <- watersheds_nested %>%
  inner_join(
    locations %>%
      rownames_to_column() %>%
      st_drop_geometry() %>%
      mutate(rowname = as.integer(rowname))
  )
watersheds_nested <- watersheds_nested %>%
  mutate(oppervlakte = st_area(geometry))

if (!file.exists(here("data", "verwerkt", "hydrologisch", "mi_meetpunten_watersheds_nested_all.gpkg"))) {
st_write(watersheds_nested %>% select(-fid), here("data", "verwerkt", "hydrologisch", "mi_meetpunten_watersheds_nested_all.gpkg"), delete_dsn = T)
}

watersheds_nested <- st_read(here("data", "verwerkt", "hydrologisch", "mi_meetpunten_watersheds_nested_all.gpkg")) #gpkg met alle afstroomgebieden!

# buffered watersheds; sommige afstroomgebieden zijn zo groot als een bekken. Hier overleg ik ze met een cirkelvormige buffer van 5km om dit in te perken.

locations_buffer <- locations %>%
  mutate(buffer = st_buffer(geometry, dist = 5000))

watersheds <- watersheds_nested
buffers <- locations_buffer %>%
  st_drop_geometry() %>%
  mutate(geometry = st_sfc(buffer, crs = crs(watersheds_nested))) %>%
  select(-buffer) %>%
  st_as_sf(.)

# Make sure both layers have the same CRS
watersheds <- st_transform(watersheds, st_crs(buffers$geometry))

# Match rows by meetplaats
idx <- match(buffers$meetplaats, watersheds$meetplaats)

# Check for unmatched IDs (optional safety check)
if (any(is.na(idx))) {
  warning("Some meetplaats values in buffers do not match those in watersheds")
}

# Grab the geometries
buffer_geoms <- st_geometry(buffers)
watershed_geoms <- st_geometry(watersheds)[idx]

# Perform row-by-row intersection
intersections <- map2(buffer_geoms, watershed_geoms, st_intersection)

# Replace geometry in buffers with the intersection result
result <- buffers %>%
  mutate(geometry = st_sfc(intersections, crs = st_crs(buffers)))

# Optional: drop empty geometries (e.g., no intersection)
watersheds_buffered <- result %>% filter(!st_is_empty(geometry))
watersheds_buffered <- watersheds_buffered %>%
  mutate(oppervlakte = st_area(geometry)) %>%
  select(-fid)

if (!file.exists(here("data", "verwerkt", "hydrologisch", "mi_meetpunten_watersheds_buffered_all.gpkg"))) {
st_write(watersheds_buffered, here("data", "verwerkt", "hydrologisch", "mi_meetpunten_watersheds_buffered_all.gpkg"), delete_dsn = T)
}

st_read(here("data", "verwerkt", "hydrologisch", "mi_meetpunten_watersheds_buffered_all.gpkg")) # gpkg met afstroomgebieden ingeperkt tot 5km buffer.

# met Watershed tool ----
# # dit is minder realistische om dat hier geen geneste watersheds uit voortkomen bij punten die sequentieel in een waterloop liggen
# watersheds2 <- qgis_run_algorithm(
#   "wbt:Watershed",
#   d8_pntr = here("data", "dem", "dhmvii_dtm_50m_d8_pointer.tif"),
#   pour_pts =
#     here("data", "meetpunten", "mi_meetpunten_snapped_to_streams.shp"),
#   output = here("data", "meetpunten", "watersheds", "mi_meetpunten_watersheds.tif"),
#   .quiet = TRUE
# )
#
# rast(here("data", "meetpunten", "watersheds", "mi_meetpunten_watersheds.tif")) %>% plot
#
# watersheds_vector <- rast(
#   here("data", "meetpunten", "watersheds", "mi_meetpunten_watersheds.tif")) %>%
#   as.polygons() %>%
#   st_as_sf() %>%
#   rename(rowname = mi_meetpunten_watersheds)
# watersheds_vector
#
# locations <- read_sf(here(
#   "data", "meetpunten", "mi_meetpunten_snapped_to_streams.shp"))
#
#
# watersheds <- watersheds_vector %>%
#   inner_join(locations %>%
#                rownames_to_column() %>%
#                st_drop_geometry() %>%
#                mutate(rowname = as.integer(rowname)))

# plot afstroomgebied bij een enkele meetplaats
random_meetplaats <- "OW161000"
ws <- watersheds_nested %>%
  filter(meetplaats == random_meetplaats)
tt <- rast(here("data", "verwerkt", "hydrologisch", "dhmvii_dtm_50m_d8_pointer.tif")) %>%
  crop(vect(ws %>% st_buffer(dist = 200))) %>%
  raster::raster() %>%
  raster::ratify()

my_palette <- cm.colors(9)

locs <- locations %>%
  filter(meetplaats == random_meetplaats)

mapview(locs, zcol = "meetplaats") +
  mapview(ws, zcol = "meetplaats", alpha.regions = 0) +
  mapview() +
  mapview(tt, col.regions = my_palette)

dtm <- rast(here("data", "ruw", "dem", "DHMVIIDTMRAS5m.tif"))
dt <- dtm %>%
  crop(vect(ws %>% st_buffer(dist = 200))) %>%
  raster::raster()

streams_crop <- streams_sf %>%
  st_crop(ws %>% st_buffer(dist = 200))

mapview(locs, zcol = "meetplaats") +
  mapview(ws, zcol = "meetplaats", alpha.regions = 0) +
  mapview(dt) +
  mapview(streams_crop)

# plot alle meetplaatsen en afstroomgebieden

locs_orig <- read_sf(
  here("data", "ruw", "macroinvertebraten", "mi_meetpunten.shp"))

bekkens_sf <- read_sf(here("data", "ruw", "bekkens", "Wsbekken.shp"))

vha_bekkens <- bekkens_sf %>%
  st_cast("GEOMETRYCOLLECTION") %>%
  st_collection_extract()

# mapview(vha_bekkens, alpha.regions = 0, legend = FALSE) +
#   mapview(locations, legend = FALSE) +
#   mapview(locs_orig, legend = FALSE) +
#   mapview(watersheds, zcol = "meetplaats", legend = FALSE) +
#   mapview(streams_sf, alpha = 0.3, hide = TRUE)

mapview(vha_bekkens, alpha.regions = 0, legend = FALSE) +
  mapview(locations, legend = FALSE) +
  mapview(locs_orig, legend = FALSE) +
  mapview(watersheds_nested, zcol = "meetplaats", legend = FALSE) +
  mapview(streams_sf) +
  mapview(wlas, color = "red")
