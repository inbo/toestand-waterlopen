library(sf)
library(igraph)
library(dplyr)
library(readr)

# 1. Pas dit pad aan naar jouw map
# setwd("Z:/Modellering/3_Ecomod/03_uitvoering/ELMO_SGBP4/input NARA")

message("Pegase data inladen...")

# Data inladen (precies zoals in Pegase code)
waterlichamen <- st_read("data/ruw/vmm/elina_koppeling/input NARA/input NARA/WLV_20230301.shp", quiet = TRUE)
segmenten     <- st_read("data/ruw/vmm/elina_koppeling/input NARA/input NARA/segmenten.shp", quiet = TRUE)
nodelist      <- st_read("data/ruw/vmm/elina_koppeling/input NARA/input NARA/nodelist.shp", quiet = TRUE)
edgelist_df   <- read.csv("data/ruw/vmm/elina_koppeling/input NARA/input NARA/edgelist_df.csv") # Bevat de topologie (van/naar)

# Attributen op segmenten zetten (VHAS, VHAG, etc.)
# Dit is cruciaal voor jouw grouping en vhas-snapping!
segmenten$vhag <- waterlichamen$vhag[match(as.integer(segmenten$WBDREC), waterlichamen$vhas)]
segmenten$vhas <- as.integer(segmenten$WBDREC) # Zorg dat VHAS kolom bestaat

# Zorg dat de projectie klopt (belangrijk voor afstanden)
# We nemen aan dat waterlichamen de juiste CRS heeft (meestal Lambert72)
if (is.na(st_crs(segmenten))) {
  st_crs(segmenten) <- st_crs(waterlichamen)
}

message("Pegase data omvormen naar Match-Format...")

# 1. Bereid de 'from' en 'to' nodes voor
# De edgelist_df bevat de node ID's. We moeten zeker weten dat die als characters worden behandeld.
# We gaan er vanuit dat edgelist_df rij-per-rij overeenkomt met 'segmenten'
# (In de Pegase code stond: segmenten$edge_id <- 1:nrow(segmenten), wat dit impliceert)

if (nrow(edgelist_df) != nrow(segmenten)) {
  stop("Let op: Aantal rijen in edgelist_df en segmenten.shp komt niet overeen!")
}

# Voeg de node informatie toe aan het SF object (nodig voor jouw functie)
# Kolom 1 van edgelist is start (from), Kolom 2 is eind (to)
segmenten$from_node <- as.character(edgelist_df[, 1])
segmenten$to_node   <- as.character(edgelist_df[, 2])
segmenten$weight    <- as.numeric(st_length(segmenten)) # Of segmenten$lengte gebruiken

# 2. Bouw de GERICHTE graaf (Directed Graph)
# Jouw functie gebruikt 'subcomponent(mode="in")' om upstream te vinden.
# Daarvoor moet de graaf 'met de stroom mee' gericht zijn.
edges_for_graph <- data.frame(
  from = segmenten$from_node,
  to   = segmenten$to_node,
  weight = segmenten$weight,
  vhas = segmenten$vhas,
  vhag = segmenten$vhag
)

# directed = TRUE is het grote verschil met de Pegase code!
g_pegase <- graph_from_data_frame(edges_for_graph, directed = TRUE)

# 3. Maak het finale lijst-object
river_network_pegase <- list(
  graph = g_pegase,
  network_sf = segmenten
)

message("Netwerk succesvol omgebouwd!")

# Laad je eigen biota/nutrient data (zoals in je eerdere code)
# ... (inladen mi_data en nutrient_data) ...

# Laad eventueel lozingspunten uit Pegase map
hpunt <- st_read("data/ruw/vmm/elina_koppeling/input NARA/input NARA/hpten.shp", quiet = TRUE) %>%
st_transform(st_crs(segmenten))

# RUNNEN MAAR
resultaat_pegase <- match_upstream_strahler(
  biota_sf = mi_data,
  quality_sf = nutrient_data,

  # HIER GEBRUIK JE HET PEGASE NETWERK:
  network_list = river_network_pegase,

  # Lozingspunten (optioneel)
  discharge_sf = hpunt,

  # Instellingen
  max_dist_m = 5000,
  max_downstream_m = 200,
  snap_tolerance = 25,     # Wordt gebruikt als VHAS match faalt

  days_before = 180,
  days_after = 30,

  selection_mode = "closest_distance",

  # Grouping & VHAS Snapping
  grouping_col = "vhag",       # Let op: kleine letters in segmenten object hierboven
  vhas_col_network = "vhas",   # Let op: kleine letters
  vhas_col_biota = "vhas",     # Kolomnaam in mi_data
  vhas_col_quality = "vhas"    # Kolomnaam in nutrient_data
)

print(paste("Aantal matches met Pegase netwerk:", sum(!is.na(resultaat_pegase$qual_meetplaats))))
