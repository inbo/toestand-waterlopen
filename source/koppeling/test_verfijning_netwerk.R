library(sf)
library(dplyr)
# Installeer lwgeom als je die nog niet hebt: install.packages("lwgeom")
library(lwgeom)

library(sf)
library(dplyr)
library(lwgeom)

hpunt <- st_read("data/ruw/vmm/elina_koppeling/input NARA/input NARA/hpten.shp", quiet = TRUE) %>%
  st_transform(st_crs(fd_fine))

split_lines_equal_length <- function(lines_sf, max_length = 200) {

  message(paste("Lijnen splitsen in segmenten van max", max_length, "meter..."))

  # 1. Voorbereiding
  lines_sf <- st_cast(lines_sf, "LINESTRING", warn = FALSE)
  lines_sf$orig_len <- as.numeric(st_length(lines_sf))
  lines_sf$orig_id  <- 1:nrow(lines_sf)

  # 2. Scheid lijnen
  short_lines <- lines_sf %>% filter(orig_len <= max_length)
  long_lines  <- lines_sf %>% filter(orig_len > max_length)

  if (nrow(long_lines) == 0) {
    return(lines_sf)
  }

  message(paste(" ->", nrow(long_lines), "lange lijnen worden opgeknipt..."))

  # 3. Bereken aantal segmenten en blaas tabel op
  long_lines$n_segments <- ceiling(long_lines$orig_len / max_length)

  expanded_lines <- long_lines[rep(row.names(long_lines), long_lines$n_segments), ]

  expanded_lines <- expanded_lines %>%
    group_by(orig_id) %>%
    mutate(seg_num = row_number()) %>%
    ungroup()

  # 4. Bereken fracties
  expanded_lines <- expanded_lines %>%
    mutate(
      step_size = max_length / orig_len,
      start_frac = (seg_num - 1) * step_size,
      end_frac   = seg_num * step_size
    )

  # Correctie voor afrondingsfouten (nooit meer dan 100% van de lijn)
  expanded_lines$end_frac[expanded_lines$end_frac > 1] <- 1

  message(" -> Geometrieën daadwerkelijk knippen (dit kan even duren)...")

  # --- FIX: GEBRUIK MAPPLY VOOR VEILIGE VERWERKING ---
  # We voeren de functie regel voor regel uit om de vector-fout te voorkomen.

  list_geoms <- mapply(
    function(geom, f_start, f_end) {
      st_linesubstring(geom, from = f_start, to = f_end, tolerance = 0.001)
    },
    expanded_lines$geometry,   # Input 1: De geometrie
    expanded_lines$start_frac, # Input 2: Start
    expanded_lines$end_frac,   # Input 3: Eind
    SIMPLIFY = FALSE           # Zorg dat we een lijst terugkrijgen
  )

  # Zet de lijst met losse stukjes terug om naar een geometry kolom (sfc)
  expanded_lines$geometry <- st_as_sfc(list_geoms, crs = st_crs(lines_sf))

  # ---------------------------------------------------

  # 6. Samenvoegen
  expanded_lines <- expanded_lines %>%
    select(-n_segments, -seg_num, -step_size, -start_frac, -end_frac)

  final_sf <- bind_rows(short_lines, expanded_lines) %>%
    arrange(orig_id)

  final_sf$orig_len <- NULL
  final_sf$orig_id <- NULL

  # Bereken nieuwe lengte
  final_sf$new_length <- as.numeric(st_length(final_sf))

  message(paste("Klaar! Resultaat:", nrow(final_sf), "segmenten."))
  return(final_sf)
}
# 1. Lees ruwe data in
fd_ruw <- st_read(here("data", "ruw", "netwerk", "Flow_direction_coordinates.shp"), quiet = T)

# 2. Splitsen (Nu krijg je segmenten van 100m tot 200m, geen 16m meer!)
fd_fine <- split_lines_equal_length(fd_ruw, max_length = 200)
hpunt <- st_read("data/ruw/vmm/elina_koppeling/input NARA/input NARA/hpten.shp", quiet = TRUE) %>%
  st_transform(st_crs(fd_fine))

build_river_network_auto_nodes <- function(lines_sf) {
  message("Graaf bouwen: data opschonen en knooppunten genereren...")

  # --- STAP 0: DATA WASSTRAAT (Cruciaal na knippen) ---

  # 1. Verwijder lege geometrieën
  lines_sf <- lines_sf[!st_is_empty(lines_sf), ]

  # 2. Als de data een mix is (GEOMETRYCOLLECTION), haal alleen de lijnen eruit
  # Dit repareert situaties waar punten en lijnen door elkaar staan
  if (any(grepl("GEOMETRY|COLLECTION", st_geometry_type(lines_sf)))) {
    lines_sf <- st_collection_extract(lines_sf, "LINESTRING")
  }

  # 3. Filter op geometry type: gooi alles weg dat nu nog steeds 'POINT' is
  # We willen enkel (MULTI)LINESTRINGS
  valid_types <- c("LINESTRING", "MULTILINESTRING")
  lines_sf <- lines_sf[st_geometry_type(lines_sf) %in% valid_types, ]

  # 4. Filter op lengte: alles met lengte 0 (of bijna 0) moet weg
  # Punten die per ongeluk lijnen zijn geworden (start=eind) geven problemen in de graaf
  lines_sf$check_len <- as.numeric(st_length(lines_sf))
  lines_sf <- lines_sf[lines_sf$check_len > 0.001, ] # Filter alles < 1mm
  lines_sf$check_len <- NULL # Opruimen

  # 5. Nu is het veilig om te casten naar LINESTRING
  lines_sf <- st_cast(lines_sf, "LINESTRING", warn = FALSE)

  message(paste(" -> Netwerk bevat", nrow(lines_sf), "geldige lijnsegmenten."))

  # ----------------------------------------------------

  # --- STAP 1: NODES BEPALEN ---

  # Start- en eindpunten bepalen
  p_start <- st_line_sample(lines_sf, sample = 0)
  p_end   <- st_line_sample(lines_sf, sample = 1)

  coords_start <- st_coordinates(p_start)
  coords_end   <- st_coordinates(p_end)

  # Maak Node ID's (afgerond op 1mm)
  node_id_start <- paste(round(coords_start[,1], 3), round(coords_start[,2], 3), sep="_")
  node_id_end   <- paste(round(coords_end[,1], 3),   round(coords_end[,2], 3),   sep="_")

  # --- STAP 2: ATTRIBUTEN ---

  lines_sf$from_node <- node_id_start
  lines_sf$to_node   <- node_id_end
  lines_sf$weight    <- as.numeric(st_length(lines_sf))

  # --- STAP 3: GRAAF BOUWEN ---

  edges <- st_drop_geometry(lines_sf) %>%
    select(from = from_node, to = to_node, weight, everything())

  g <- graph_from_data_frame(edges, directed = TRUE)

  message("Graaf succesvol gebouwd!")
  return(list(graph = g, network_sf = lines_sf))
}
river_network_fine <- build_river_network_auto_nodes(fd_fine)
print(river_network_fine$graph)

nutrient_results_fine <- match_upstream_strahler(
  biota_sf = mi_data,
  quality_sf = nutrient_data,
  network_list = river_network_fine,
  strahler_sf = strahler,
  use_strahler = FALSE,
  strahler_col = "orde",
  max_downstream_m = 200,
  max_dist_m = 5000,       # Max 5km stroomopwaarts
  days_before = 180,       # Kwaliteit mag tot 180 dagen VOOR de biota meting zijn
  days_after = 30,         # Kwaliteit mag tot 14 dagen NA de biota meting zijn
  col_date_biota = "monsternamedatum",
  col_date_quality = "monsternamedatum",
  selection_mode = "closest_distance",
  grouping_col = "VHAG",
  vhas_col_network = "VHAS",
  vhas_col_biota = "vhas",
  vhas_col_quality = "vhas"
)
print(paste("Aantal matches:", sum(!is.na(nutrient_results_fine$qual_meetplaats))))

nutrient_results_fine_hpunt <- match_upstream_strahler(
  biota_sf = mi_data,
  quality_sf = nutrient_data,
  network_list = river_network_fine,
  strahler_sf = strahler,
  discharge_sf = hpunt,
  use_strahler = FALSE,
  strahler_col = "orde",
  max_downstream_m = 200,
  max_dist_m = 5000,       # Max 5km stroomopwaarts
  days_before = 180,       # Kwaliteit mag tot 180 dagen VOOR de biota meting zijn
  days_after = 30,         # Kwaliteit mag tot 14 dagen NA de biota meting zijn
  col_date_biota = "monsternamedatum",
  col_date_quality = "monsternamedatum",
  selection_mode = "closest_distance",
  grouping_col = "VHAG",
  vhas_col_network = "VHAS",
  vhas_col_biota = "vhas",
  vhas_col_quality = "vhas"
)
print(paste("Aantal matches:", sum(!is.na(nutrient_results_fine_hpunt$qual_meetplaats))))

nutrient_results_hpunt <- match_upstream_strahler(
  biota_sf = mi_data,
  quality_sf = nutrient_data,
  network_list = river_network,
  discharge_sf = hpunt,
  strahler_sf = strahler,
  use_strahler = FALSE,
  strahler_col = "orde",
  max_downstream_m = 200,
  max_dist_m = 5000,       # Max 5km stroomopwaarts
  days_before = 180,       # Kwaliteit mag tot 180 dagen VOOR de biota meting zijn
  days_after = 30,         # Kwaliteit mag tot 14 dagen NA de biota meting zijn
  col_date_biota = "monsternamedatum",
  col_date_quality = "monsternamedatum",
  selection_mode = "closest_distance",
  grouping_col = "VHAG",
  vhas_col_network = "VHAS",
  vhas_col_biota = "vhas",
  vhas_col_quality = "vhas"
)
print(paste("Aantal matches:", sum(!is.na(nutrient_results_hpunt$qual_meetplaats))))

print(paste("Aantal matches:", sum(!is.na(nutrient_results$qual_meetplaats))))
print(paste("Aantal matches:", sum(!is.na(nutrient_results_hpunt$qual_meetplaats))))
print(paste("Aantal matches:", sum(!is.na(nutrient_results_fine$qual_meetplaats))))
print(paste("Aantal matches:", sum(!is.na(nutrient_results_fine_hpunt$qual_meetplaats))))
