source(here::here("source", "inladen_packages.R"))
# whitebox tools installation:
# install.packages("whitebox")
# whitebox::install_whitebox()
# QGIS plugins install search for whitebox and install the plugin
# Go to preferences in QGIS select processing select whitebox and fill in path
# by searching for whitebox_tools.exe file

qgis_algorithms() %>% View

# afstroomgebieden voor alle meetpunten bepalen ----
dtm_hydro_breached <- rast(here("data", "dem", "DHMVIIDTMRAS025mto50m_breachedDTM.tif"))
plot(dtm_hydro_breached)

dtm_h

# Define the bounding box for Ghent and surroundings (in Belgian Lambert 72)
# Approximate bounding box for Ghent in EPSG:31370
xmin <- 99500   # Minimum X coordinate
xmax <- 110500  # Maximum X coordinate
ymin <- 185000  # Minimum Y coordinate
ymax <- 197000  # Maximum Y coordinate

# Create a SpatExtent object
ghent_extent <- ext(xmin, xmax, ymin, ymax)

# Crop the DEM raster
dtm_cropped <- crop(dtm_hydro, ghent_extent)

qgis_run_algorithm(
  "wbt:Hillshade",
  dem = dtm_cropped,
  output = here("data", "dem", "ghent_hillshade.tif"),
  azimuth = 115
)

hillshade <- rast(here("data", "dem", "ghent_hillshade.tif"))

tmap_mode("view")
tm_shape(hillshade) +
  tm_raster(style = "cont", palette = "-Greys", legend.show = FALSE) +
  tm_scale_bar()

# flow accumulation
#qgis_arguments("wbt:D8FlowAccumulation")
d8_flow <- qgis_run_algorithm(
  "wbt:D8FlowAccumulation",
  input = here("data", "dem", "DHMVIIDTMRAS025mto50m_breachedDTM.tif"),
  output = here("data", "dem", "dhmvii_dtm_50m_d8_flow.tif"),
  .quiet = TRUE)

plot(rast(here("data", "dem", "dhmvii_dtm_50m_d8_flow.tif")))

# d8 pointer
#qgis_arguments("wbt:D8Pointer")
d8_pointer <- qgis_run_algorithm(
  "wbt:D8Pointer",
  dem = here("data", "dem", "DHMVIIDTMRAS025mto50m_breachedDTM.tif"),
  output = here("data", "dem", "dhmvii_dtm_50m_d8_pointer.tif"),
  .quiet = TRUE)

# extract streams
#qgis_arguments("wbt:ExtractStreams")
threshold <- 100
streams <- qgis_run_algorithm(
  "wbt:ExtractStreams",
  flow_accum = here("data", "dem", "dhmvii_dtm_50m_d8_flow.tif"),
  threshold = threshold,
  output = here("data", "dem",
                paste0("dhmvii_dtm_50m_streams_t",
                       threshold,".tif")),
  .quiet = TRUE
)

#plot(rast(here("data", "dem", paste0("dhmvii_dtm_50m_streams_t", threshold,".tif"))), maxcell = 2500000, col = "black")
# setting pour points
# The tool only wants a file name, which must be a shape file
st_write(vmm_meetnet %>%
           group_by(meetplaats) %>%
           summarise(),
         here("data", "vmm", "vmm_macroinvertebraten_meetplaatsen.shp"),
         append = FALSE)

#qgis_arguments("wbt:JensonSnapPourPoints")
snapped <- qgis_run_algorithm(
  "wbt:JensonSnapPourPoints",
  pour_pts = here("data", "meetpunten", "mi_meetpunten.shp"),
  streams = here("data", "dem", paste0("dhmvii_dtm_50m_streams_t",
                                       threshold,".tif")),
  snap_dist = 300,
  output = here("data", "meetpunten", "mi_meetpunten_snapped_to_streams.shp"),
  .quiet = TRUE
)

# delineate watersheds
# note, using wbt:WaterSheds can give rise to nested watersheds
#qgis_arguments("wbt:UnnestBasins")
watersheds <- qgis_run_algorithm(
  "wbt:UnnestBasins",
  d8_pntr = here("data", "dem", "dhmvii_dtm_50m_d8_pointer.tif"),
  pour_pts =
    here("data", "meetpunten", "mi_meetpunten_snapped_to_streams.shp"),
  output = here("data", "meetpunten", "mi_meetpunten_watersheds.tif"),
  .quiet = TRUE
)

watersheds_1 <- rast(
  here("data", "vmm", "vmm_macroinvertebraten_watersheds_1.tif")) %>%
  as.polygons() %>%
  st_as_sf() %>%
  rename(rowname = vmm_macroinvertebraten_watersheds_1)
watersheds_2 <- rast(
  here("data", "vmm", "vmm_macroinvertebraten_watersheds_2.tif")) %>%
  as.polygons() %>%
  st_as_sf() %>%
  rename(rowname = vmm_macroinvertebraten_watersheds_2)
watersheds_3 <- rast(
  here("data", "vmm", "vmm_macroinvertebraten_watersheds_3.tif")) %>%
  as.polygons() %>%
  st_as_sf() %>%
  rename(rowname = vmm_macroinvertebraten_watersheds_3)

locations <- read_sf(here(
  "data", "vmm", "vmm_macroinvertebraten_snapped_to_streams.shp"))


watersheds <- bind_rows(
  watersheds_1,
  watersheds_2,
  watersheds_3) %>%
  inner_join(locations %>%
               rownames_to_column() %>%
               st_drop_geometry() %>%
               mutate(rowname = as.integer(rowname)))
