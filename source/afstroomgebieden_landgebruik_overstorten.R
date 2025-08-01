source(here::here("source", "inladen_packages.R"))
# whitebox tools installation:
# install.packages("whitebox")
# whitebox::install_whitebox()
# QGIS plugins install search for whitebox and install the plugin
# Go to preferences in QGIS select processing select whitebox and fill in path
# by searching for whitebox_tools.exe file

# Help functies voor qgis process

qgis_algorithms() %>% View # algoritmes bekijken
qgis_show_help("native:snapgeometries") # argumenten bekijken

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

raster(here("data", "verwerkt", "hydrologisch",
            paste0("dhmvii_dtm_50m_streams_t",
                   threshold,".tif"))) %>% mapview()

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
watershed_files <- paste0("mi_meetpunten_watersheds_nested_", 1:60, ".tif")

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
  mutate(oppervlakte = st_area(geom))

if (!file.exists(here("data", "verwerkt", "hydrologisch", "mi_meetpunten_watersheds_nested_all.gpkg"))) {
st_write(watersheds_nested, here("data", "verwerkt", "hydrologisch", "mi_meetpunten_watersheds_nested_all.gpkg"), delete_dsn = T)
}

watersheds_nested <- st_read(here("data", "verwerkt", "hydrologisch", "mi_meetpunten_watersheds_nested_all.gpkg"))

# buffered watersheds

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
  mutate(oppervlakte = st_area(geom))

if (!file.exists(here("data", "verwerkt", "hydrologisch", "mi_meetpunten_watersheds_buffered_all.gpkg"))) {
st_write(watersheds_buffered, here("data", "verwerkt", "hydrologisch", "mi_meetpunten_watersheds_buffered_all.gpkg"), delete_dsn = T)
}

st_read(here("data", "verwerkt", "hydrologisch", "mi_meetpunten_watersheds_buffered_all.gpkg"))

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

# landgebruik binnen afstroomgebied

landuse_raster <- raster::raster(here("data", "ruw", "landgebruik", "niveau1_vla_2022_v3.tif"))
watersheds_buffered <- st_read(here("data", "verwerkt", "hydrologisch", "mi_meetpunten_watersheds_buffered_all.gpkg"))
watersheds_buffered <- st_transform(watersheds_buffered, crs = crs(landuse_raster))
plot(landuse_raster)

# Define input and output paths
output_table <- here("data", "verwerkt", "landgebruik", "zonal_histogram_landgebruik_afstroomgebieden.gpkg")

# Run the zonal histogram algorithm

landuse_raster[landuse_raster < 1 | landuse_raster > 25] <- NA # om vreemde grote waarde als outputkolom te voorkomen
if (!file.exists(output_table)) {

qgis_run_algorithm("native:zonalhistogram",
                   INPUT_RASTER = landuse_raster,
                   INPUT_VECTOR = watersheds_buffered,
                   RASTER_BAND = 1,
                   COLUMN_PREFIX = "VALUE_",  # Prefix for land-use classes
                   OUTPUT = output_table)
}

watershed_landuse0 <- st_read(output_table)
landuse_oever0 <- read_excel(path = here("data", "ruw", "landgebruik", "mi_meetpunten_lu_buffer.xlsx"))
landuse_oever0[["VALUE_13"]] <- 0
landuse_buffer0 <- read_excel(path = here("data", "ruw", "landgebruik", "mi_meetpunten_lu_cirk_min50m.xlsx"))


convert_pixels_to_percentages <- function(data) {
  data <- data %>%
    mutate(
      total_pixels = rowSums(across(starts_with("VALUE_")), na.rm = TRUE),
      across(starts_with("VALUE_"), ~ .x / total_pixels * 100, .names = "{.col}_pct")
    )

  return(data)
}

watershed_landuse <- convert_pixels_to_percentages(data = watershed_landuse0)

landuse_oever <- convert_pixels_to_percentages(data = landuse_oever0)
landuse_buffer <- convert_pixels_to_percentages(data = landuse_buffer0)

st_write(watershed_landuse, here("data", "verwerkt", "landgebruik","finale_watershed_landgebruik.gpkg"), delete_dsn = TRUE)
save(landuse_oever, file = here("data", "verwerkt", "landgebruik","landgebruik_oever.Rdata"))
save(landuse_buffer, file = here("data", "verwerkt", "landgebruik","landgebruik_buffer.Rdata"))

# aantal overstorten in een afstroomgebied ----

overstorten_basis_locaties <- st_read(here("data", "ruw", "overstorten", "P_OS_basis.shp"))
overstorten_uitlaat_vha <- st_read(here("data", "ruw", "overstorten", "P_OS_uitlaat_VHA.shp"))

watersheds_buffered <- st_read(here("data", "verwerkt", "hydrologisch", "mi_meetpunten_watersheds_buffered_all.gpkg"))

# Perform spatial join: Assign points to watersheds
points_in_watersheds <- st_join(overstorten_uitlaat_vha, watersheds_buffered, left = FALSE)

# Count points per watershed
watershed_point_counts <- points_in_watersheds %>%
  count(meetplaats) %>%
  rename(aantal_overstorten = n)

# Merge counts back into watersheds
watersheds_aantal_overstorten <- watersheds_buffered %>%
  left_join(watershed_point_counts %>%
              st_drop_geometry(),
            by = "meetplaats") %>%
  mutate(aantal_overstorten = replace_na(aantal_overstorten, 0)) %>%
  st_drop_geometry()


# Save the output
save(watersheds_aantal_overstorten, file = here("data", "verwerkt", "overstorten", "mi_meetpunten_aantal_overstorten_afstroomgebied.rdata"))

# vuilvracht van overstorten in een afstroomgebied met buffer en afstandsgewogen ----

vuilvracht_overstorten <- st_read(here("data", "ruw" , "overstorten", "vuilvracht", "OS_maart2025.shp"))


# Perform spatial join: Assign points to watersheds
points_in_watersheds <- st_join(vuilvracht_overstorten, watersheds_buffered, left = FALSE)

points_in_watersheds

locations_cum_gewogen_vuilvracht <- locations


# --- 2. Functie voor afstandsgewogen BZV-berekening ---

# Deze functie berekent de gewogen BZV voor EEN ENKEL meetpunt
calculate_weighted_bzv <- function(meetplaats_id_val, overstorten_sf, meetplaatsen_sf, exponent = 1) {
  # Haal de specifieke meetplaats geometrie op
  current_location <- meetplaatsen_sf %>%
    filter(meetplaats == meetplaats_id_val)

  # Haal de overstorten op die bij dit meetpunt horen
  relevant_overstorten <- overstorten_sf %>%
    filter(meetplaats == meetplaats_id_val)

  # Als er geen relevante overstorten zijn, retourneer 0
  if (nrow(relevant_overstorten) == 0) {
    return(0)
  }

  # Bereken de afstand van elk relevant overstortpunt tot de meetplaats
  # st_distance werkt met sf-objecten; het resultaat is een matrix, dus pak de diagonaal of de eerste rij/kolom
  # aangezien current_location slechts 1 punt is.
  afstanden <- st_distance(relevant_overstorten, current_location) %>%
    as.numeric()

  # Voorkom delen door nul als een overstort exact op het meetpunt ligt
  afstanden[afstanden == 0] <- 0.001

  # Bereken de gewichten met inverse afstand
  gewichten <- 1 / (afstanden ^ exponent)

  # Bereken de afstandsgewogen cumulatieve BZV
  weighted_bzv_sum <- sum(relevant_overstorten$BZV * gewichten)

  return(weighted_bzv_sum)
}

# --- 3. Toepassen op alle meetplaatsen ---

# Initialiseer een nieuwe kolom in het 'locations' sf-object
locations_cum_gewogen_vuilvracht$weighted_cumulative_BZV <- NA_real_

# Loop over elke meetplaats in het 'locations' sf-object
for (i in 1:nrow(locations)) {
  current_meetplaats_id <- locations_cum_gewogen_vuilvracht$meetplaats[i]

  # Bereken de gewogen BZV voor de huidige meetplaats
  # Gebruik exponent = 1 voor inverse afstand, exponent = 2 voor inverse afstand kwadraat
  calculated_bzv <- calculate_weighted_bzv(
    meetplaats_id_val = current_meetplaats_id,
    overstorten_sf = points_in_watersheds,
    meetplaatsen_sf = locations_cum_gewogen_vuilvracht,
    exponent = 1 # Pas dit aan indien je een ander exponent wilt (bv. 2 voor 1/d^2)
  )

  # Sla het resultaat op
  locations_cum_gewogen_vuilvracht$weighted_cumulative_BZV[i] <- calculated_bzv
}

# --- 4. Resultaten bekijken ---
print(locations_cum_gewogen_vuilvracht)


# Merge counts back into watersheds
cum_vuilvracht_watershed <- watersheds_nested %>%
  left_join(locations_cum_gewogen_vuilvracht %>%
              st_drop_geometry(),
            by = "meetplaats") %>%
  mutate(cum_gewogen_bzv = replace_na(weighted_cumulative_BZV, 0)) %>%
  select(-weighted_cumulative_BZV) %>%
  st_drop_geometry()

# visualisatie
test <- watersheds_nested %>% filter(meetplaats == "OW389965")
test2 <- watersheds_buffered %>% filter(meetplaats == "OW389965")
mapview(test) +
  mapview(test2) +
  mapview(vuilvracht_overstorten) +
  mapview(locations)

save(cum_vuilvracht_watershed, file = here("data", "verwerkt", "overstorten", "mi_meetpunten_vuilvracht_bzv.rdata"))

# Reclassen van landgebruik ----

watershed_landuse <- st_read(here("data", "verwerkt", "landgebruik","finale_watershed_landgebruik.gpkg"))

landuse_reclass <- function(data, suffix) {
data <- data %>%
  mutate(!!sym(paste0("water_", suffix)) := VALUE_24_pct,
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

watershed_landuse_reclass <- landuse_reclass(watershed_landuse, "afstroomgebied") %>%
  inner_join(., watersheds_buffered %>%
               select(meetplaats, oppervlakte))

save(watershed_landuse_reclass, file  = here("data", "verwerkt", "landgebruik", "landgebruik_afstroomgebied.Rdata"))
buffer_landuse_reclass <- landuse_reclass(landuse_buffer, "buffer")
save(buffer_landuse_reclass, file  = here("data", "verwerkt", "landgebruik", "landgebruik_buffer.Rdata"))
oever_landuse_reclass <- landuse_reclass(landuse_oever, "oever")
save(oever_landuse_reclass, file  = here("data", "verwerkt", "landgebruik", "landgebruik_oever.Rdata"))

load(file  = here("data", "verwerkt", "landgebruik", "landgebruik_afstroomgebied.Rdata"))

