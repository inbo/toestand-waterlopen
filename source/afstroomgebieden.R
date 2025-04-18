source(here::here("source", "inladen_packages.R"))
# whitebox tools installation:
# install.packages("whitebox")
# whitebox::install_whitebox()
# QGIS plugins install search for whitebox and install the plugin
# Go to preferences in QGIS select processing select whitebox and fill in path
# by searching for whitebox_tools.exe file

# Help functies voor qgis process

qgis_algorithms() %>% View # algoritmes bekijken
qgis_show_help("native:zonalhistogram") # argumenten bekijken

# afstroomgebieden voor alle meetpunten bepalen ----
dtm_hydro_breached <- rast(here("data", "dem", "DHMVIIDTMRAS025mto50m_breachedDTM.tif"))
plot(dtm_hydro_breached)

# flow accumulation
#qgis_arguments("wbt:D8FlowAccumulation")
if (!file.exists(here("data", "dem", "dhmvii_dtm_50m_d8_flow.tif"))) {
  d8_flow <- qgis_run_algorithm(
    "wbt:D8FlowAccumulation",
    input = here("data", "dem", "DHMVIIDTMRAS025mto50m_breachedDTM.tif"),
    output = here("data", "dem", "dhmvii_dtm_50m_d8_flow.tif"),
    .quiet = TRUE)
}

plot(rast(here("data", "dem", "dhmvii_dtm_50m_d8_flow.tif")))

# d8 pointer
#qgis_arguments("wbt:D8Pointer")
if (!file.exists(here("data", "dem", "dhmvii_dtm_50m_d8_pointer.tif"))) {
d8_pointer <- qgis_run_algorithm(
  "wbt:D8Pointer",
  dem = here("data", "dem", "DHMVIIDTMRAS025mto50m_breachedDTM.tif"),
  output = here("data", "dem", "dhmvii_dtm_50m_d8_pointer.tif"),
  .quiet = TRUE)
}

# extract streams
#qgis_arguments("wbt:ExtractStreams")
threshold <- 100
if (!file.exists(here("data", "dem",
                      paste0("dhmvii_dtm_50m_streams_t",
                             threshold,".tif")))) {
streams <- qgis_run_algorithm(
  "wbt:ExtractStreams",
  flow_accum = here("data", "dem", "dhmvii_dtm_50m_d8_flow.tif"),
  threshold = threshold,
  output = here("data", "dem",
                paste0("dhmvii_dtm_50m_streams_t",
                       threshold,".tif")),
  .quiet = TRUE
)
}

#plot(rast(here("data", "dem", paste0("dhmvii_dtm_50m_streams_t", threshold,".tif"))), maxcell = 2500000, col = "black")
# setting pour points

#qgis_arguments("wbt:JensonSnapPourPoints")
if (!file.exists(here("data", "meetpunten", "mi_meetpunten_snapped_to_streams.shp"))) {
snapped <- qgis_run_algorithm(
  "wbt:JensonSnapPourPoints",
  pour_pts = here("data", "meetpunten", "mi_meetpunten.shp"),
  streams = here("data", "dem", paste0("dhmvii_dtm_50m_streams_t",
                                       threshold,".tif")),
  snap_dist = 300,
  output = here("data", "meetpunten", "mi_meetpunten_snapped_to_streams.shp"),
  .quiet = TRUE
)
}

# delineate watersheds
# om nested watersheds te krijgen -> realistischer dan unnested
#qgis_arguments("wbt:UnnestBasins")
if (!file.exists(here("data", "meetpunten", "mi_meetpunten_watersheds_nested_1.tif"))){
  watersheds_nested <- qgis_run_algorithm(
  "wbt:UnnestBasins",
  d8_pntr = here("data", "dem", "dhmvii_dtm_50m_d8_pointer.tif"),
  pour_pts =
    here("data", "meetpunten", "mi_meetpunten_snapped_to_streams.shp"),
  output = here("data", "meetpunten", "mi_meetpunten_watersheds_nested.tif"),
  .quiet = TRUE
)
}
# Generate file paths dynamically
watershed_files <- paste0("mi_meetpunten_watersheds_nested_", 1:36, ".tif")

# Convert raster watersheds to polygons in a loop
watersheds_list <- map(watershed_files, ~ {
  rast(here("data", "meetpunten", .x)) %>%
    as.polygons() %>%
    st_as_sf() %>%
    rename(rowname = !!rlang::sym(tools::file_path_sans_ext(.x)))  # Rename column dynamically
})

# Combine all watersheds into one dataframe
watersheds_nested <- bind_rows(watersheds_list)

# Load locations
locations <- read_sf(here("data", "meetpunten", "mi_meetpunten_snapped_to_streams.shp"))

# Join locations to watersheds
watersheds_nested <- watersheds_nested %>%
  inner_join(
    locations %>%
      rownames_to_column() %>%
      st_drop_geometry() %>%
      mutate(rowname = as.integer(rowname))
  )

if (!file.exists(here("data", "meetpunten", "mi_meetpunten_watersheds_nested_all.gpkg"))){
st_write(watersheds_nested, here("data", "meetpunten", "mi_meetpunten_watersheds_nested_all.gpkg"))
}

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

# plot afstroomgebied bij een random meetplaats
random_meetplaats <- "OW101500"
ws <- watersheds_nested %>%
  filter(meetplaats == random_meetplaats)
tt <- rast(here("data", "dem", "dhmvii_dtm_50m_d8_pointer.tif")) %>%
  crop(vect(ws %>% st_buffer(dist = 200))) %>%
  raster::raster() %>%
  raster::ratify()

my_palette <- cm.colors(9)

locs <- locations %>%
  filter(meetplaats == random_meetplaats)

#qgis_arguments("wbt:RasterStreamsToVector")
qgis_run_algorithm(
  "wbt:RasterStreamsToVector",
  streams = here("data", "dem", paste0("dhmvii_dtm_50m_streams_t",
                                       threshold,".tif")),
  d8_pntr = here("data", "dem", "dhmvii_dtm_50m_d8_pointer.tif"),
  output = here("data", "meetpunten", "hydro_dtm_stream_network.shp"),
  .quiet = TRUE)

streams_sf <- read_sf(here("data", "meetpunten", "hydro_dtm_stream_network.shp"))
st_crs(streams_sf) <- "EPSG:31370"

mapview(locs, zcol = "meetplaats") +
  mapview(ws, zcol = "meetplaats", alpha.regions = 0) +
  mapview(tt, col.regions = my_palette)

dtm <- rast(here("data", "dem", "DHMVIIDTMRAS5m.tif"))
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
  here("data", "meetpunten", "mi_meetpunten.shp"))

bekkens_sf <- read_sf(here("data", "bekkens", "Wsbekken.shp"))

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
  mapview(streams_sf, alpha = 0.3, hide = TRUE)

# landgebruik binnen afstroomgebied

landuse_raster <- rast(here("data", "landgebruik", "niveau1_vla_2022_v3.tif"))
watersheds_nested <- st_read(here("data", "meetpunten", "mi_meetpunten_watersheds_nested_all.gpkg"))

plot(landuse_raster)

# Define input and output paths
output_table <- here("data", "landgebruik", "zonal_histogram_landgebruik_afstroomgebieden.gpkg")

# Run the zonal histogram algorithm

landuse_raster[landuse_raster < 1 | landuse_raster > 25] <- NA # om vreemde grote waarde als outputkolom te voorkomen
if (!file.exists(output_table)) {

qgis_run_algorithm("native:zonalhistogram",
                   INPUT_RASTER = landuse_raster,
                   INPUT_VECTOR = watersheds_nested,
                   RASTER_BAND = 1,
                   COLUMN_PREFIX = "VALUE_",  # Prefix for land-use classes
                   OUTPUT = output_table)
}

watershed_landuse0 <- st_read(output_table)
landuse_oever0 <- read_excel(path = here("data", "landgebruik", "mi_meetpunten_lu_buffer.xlsx"))
landuse_oever0[["VALUE_13"]] <- 0
landuse_buffer0 <- read_excel(path = here("data", "landgebruik", "mi_meetpunten_lu_cirk_min50m.xlsx"))


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

st_write(watershed_landuse, here("data", "landgebruik","finale_watershed_landgebruik.gpkg"), delete_dsn = TRUE)
save(landuse_oever, file = here("data", "landgebruik","landgebruik_oever.Rdata"))
save(landuse_buffer, file = here("data", "landgebruik","landgebruik_buffer.Rdata"))
# aantal overstorten in een afstroomgebied ----

overstorten_basis_locaties <- read_sf(here("data", "overstorten", "P_OS_basis.shp"))
watersheds_nested <- st_read(here("data", "meetpunten", "mi_meetpunten_watersheds_nested_all.gpkg"))

# Perform spatial join: Assign points to watersheds
points_in_watersheds <- st_join(overstorten_basis_locaties, watersheds_nested, left = FALSE)

# Count points per watershed
watershed_point_counts <- points_in_watersheds %>%
  count(meetplaats) %>%
  rename(aantal_overstorten = n)

# Merge counts back into watersheds
watersheds_aantal_overstorten <- watersheds_nested %>%
  left_join(watershed_point_counts %>%
              st_drop_geometry(),
            by = "meetplaats") %>%
  mutate(n = replace_na(aantal_overstorten, 0))


# Save the output
st_write(watersheds_aantal_overstorten, here("data", "meetpunten", "mi_meetpunten_watersheds_nested_all_aantal_overstorten.gpkg"))

# Reclassen van landgebruik ----

watershed_landuse <- st_read(here("data", "landgebruik","finale_watershed_landgebruik.gpkg"))

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

watershed_landuse_reclass <- landuse_reclass(watershed_landuse, "afstroomgebied")
save(watershed_landuse_reclass, file  = here("data" , "landgebruik", "landgebruik_afstroomgebied.Rdata"))
buffer_landuse_reclass <- landuse_reclass(landuse_buffer, "buffer")
save(buffer_landuse_reclass , file  = here("data" , "landgebruik", "landgebruik_buffer.Rdata"))
oever_landuse_reclass <- landuse_reclass(landuse_oever, "oever")
save(oever_landuse_reclass, file  = here("data" , "landgebruik", "landgebruik_oever.Rdata"))
