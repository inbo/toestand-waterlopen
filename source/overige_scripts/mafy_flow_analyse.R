library(sf)
library(terra)
library(whitebox)  # for hydrology functions
library(igraph)    # for graph-based connectivity
library(tidyverse)
library(here)
library(qgisprocess)
library(sf)
library(nngeo)
library(igraph)
library(lubridate)
library(mapview)

dem <- rast(here("data", "ruw", "dem", "DHMVIIDTMRAS025mto50m_breachedDTM.tif"))
rivers <- st_read(here("data", "ruw", "waterlopen", "wlas_network.shp"))
nodes <- st_read(here("data", "ruw", "waterlopen", "vha_network_junctions.shp"))

streams_sf <- st_read(here("data", "verwerkt", "hydrologisch", "hydro_dtm_stream_network.shp"))
st_crs(streams_sf) <- "EPSG:31370"

flow_acc <- rast(here("data", "verwerkt", "hydrologisch", "dhmvii_dtm_50m_d8_flow.tif"))

flow_dir <- rast(here("data", "verwerkt", "hydrologisch", "dhmvii_dtm_50m_d8_pointer.tif"))

start_pts <- st_line_sample(rivers, sample = 0)  # start point
end_pts <- st_line_sample(rivers, sample = 1)    # end point

start_node_id <- st_nearest_feature(start_pts, nodes)
end_node_id <- st_nearest_feature(end_pts, nodes)
rivers$from_node <- start_node_id
rivers$to_node <- end_node_id

start_elev <- terra::extract(dem, vect(start_pts))[[2]]
end_elev <- terra::extract(dem, vect(end_pts))[[2]]

rivers$flow_dir <- ifelse(start_elev > end_elev, "start_to_end", "end_to_start")
rivers$start_elev <- start_elev
rivers$end_elev <- end_elev
# find nearest fc point for mafy point in space upstream and in time ----
# Required packages



# Load your data (adjust paths)
mafy_meetpunten <- st_read(here("data", "ruw", "macrofyten", "mafy_meetpunten.gpkg"))
fc_meetpunten <- st_read(here("data", "ruw", "fys_chem", "fc_meetpunten.gpkg"))

mafy_meetpunten_meetplaats <- mafy_meetpunten %>%
  select(meetplaats, geom) %>%
  unique()

fc_meetpunten_meetplaats <- fc_meetpunten %>%
  select(meetplaats, geom) %>%
  unique()

# Convert dates if necessary
mafy_meetpunten$monsternamedatum <- as.Date(mafy_meetpunten$monsternamedatum)
fc_meetpunten$monsternamedatum <- as.Date(fc_meetpunten$sample_datum_monstername)

# Snap mafy points to river lines

qgis_run_algorithm(
  "native:snapgeometries",
  INPUT = mafy_meetpunten,
  TOLERANCE = 100,
  REFERENCE_LAYER = rivers,
  OUTPUT = here("data", "verwerkt", "hydrologisch", "mafy_snapped.gpkg"),
  .quiet = TRUE
)

mafy_snapped <- st_read(here("data", "verwerkt", "hydrologisch", "mafy_snapped.gpkg"))
# %>%
#   st_join(., mafy_meetpunten %>%
#             select(meetplaats, monsternamedatum),
#           by = "meetplaats")



# Snap quality points to river lines

qgis_run_algorithm(
  "native:snapgeometries",
  INPUT = fc_meetpunten_meetplaats,
  TOLERANCE = 100,
  REFERENCE_LAYER = rivers,
  OUTPUT = here("data", "verwerkt", "hydrologisch", "fc_snapped.gpkg"),
  .quiet = TRUE
)
fc_snapped_meetplaats <- st_read(here("data", "verwerkt", "hydrologisch", "fc_snapped.gpkg"))

fc_no_geom <- fc_meetpunten %>%
  st_drop_geometry()

fc_snapped <- fc_snapped_meetplaats %>%
 left_join(., fc_no_geom, by = "meetplaats")

mapview(mafy_snapped, color = "red", legend = FALSE) +
  # mapview(mafy_meetpunten_meetplaats, color = "green", legend = F) +
  mapview(fc_snapped_meetplaats, color = "yellow", legend = F) +
  # mapview(nodes, color = "blue", legend = F) +
mapview(st_simplify(rivers))

# Assign each snapped point to nearest river segment
mafy_snapped$segment_id <- st_nearest_feature(mafy_snapped, rivers)
fc_snapped$segment_id <- st_nearest_feature(fc_snapped, rivers)

# Map river segments to from_node (based on direction)
segment_to_node <- setNames(rivers$from_node, seq_len(nrow(rivers)))

mafy_snapped$node <- segment_to_node[mafy_snapped$segment_id]
fc_snapped$node <- segment_to_node[fc_snapped$segment_id]

# Build river network graph
edges <- data.frame(
  from = rivers$from_node,
  to = rivers$to_node,
  segment_id = seq_len(nrow(rivers))
)
g <- graph_from_data_frame(edges, directed = TRUE)



# Initialize results
results <- list()

# For each mafy point, find closest upstream quality point within 3 months
for (i in seq_len(nrow(mafy_snapped))) {
  mpt <- mafy_snapped[i, ]
  mdate <- mpt$monsternamedatum
  mnode <- mpt$node

  # Get all upstream nodes
  upstream_nodes <- subcomponent(g, v = as.character(mnode), mode = "in") %>% names()

  # Filter quality points upstream and within 90 days
  candidates <- fc_snapped %>%
    filter(
      as.character(node) %in% upstream_nodes,
      abs(difftime(monsternamedatum, mdate, units = "days")) <= 90
    )

  if (nrow(candidates) > 0) {
    # Find the closest (spatially) among them
    dists <- st_distance(mpt, candidates)
    closest_idx <- which.min(dists)
    match <- candidates[closest_idx, ]
  } else {
    match <- NA
  }

  results[[i]] <- list(mafy = mpt, quality = match)
}

# Convert results into a data frame
matched_mafy <- do.call(rbind, lapply(results, function(x) x$mafy))
# matched_quality <- do.call(rbind, lapply(results, function(x) x$quality))
#
# # Only keep results that have a valid 'quality' entry
# valid_quality <- lapply(results, function(x) {
#   if (!is.null(x$quality) && inherits(x$quality, "sf")) {
#     return(x$quality)
#   } else {
#     return(NULL)
#   }
# })
#
# # Remove NULLs
# valid_quality <- Filter(Negate(is.null), valid_quality)
#
# # Bind all valid quality points into one sf object
# matched_quality <- do.call(rbind, valid_quality)

#lijkt te werken
# Step 1: Get template from first valid entry
first_valid <- NULL
for (x in results) {
  if (!is.null(x$quality) && inherits(x$quality, "sf")) {
    first_valid <- x$quality[0, ]  # zero-row sf with correct structure
    break
  }
}
if (is.null(first_valid)) stop("No valid quality matches found.")

# Step 2: Create empty row constructor
make_empty_row <- function(template) {
  empty <- template[1, ]
  for (col in names(template)) {
    if (col != attr(template, "sf_column")) {
      empty[[col]] <- NA
    }
  }
  # Assign empty point geometry
  st_geometry(empty) <- st_sfc(st_point(), crs = st_crs(template))
  return(empty)
}

# Step 3: Apply logic
matched_quality <- do.call(rbind, lapply(results, function(x) {
  if (!is.null(x$quality) && inherits(x$quality, "sf")) {
    return(x$quality)
  } else {
    return(make_empty_row(first_valid))
  }
}))


# Optionally combine into one data frame
matched_df <- bind_cols(st_drop_geometry(matched_mafy), st_drop_geometry(matched_quality)) %>%
  mutate(monsternamedatum...6 = as.Date(monsternamedatum...6))
matched_sf <- st_sf(matched_df, geometry = st_geometry(matched_mafy))

