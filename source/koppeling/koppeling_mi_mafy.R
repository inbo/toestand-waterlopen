library(sf)
library(igraph)
library(tidyverse)
library(progress)
library(here)
library(mapview)
library(lwgeom)

fd <- st_read(here("data", "ruw", "netwerk", "Flow_direction_coordinates.shp"), quiet = T)
nodes <- st_read(here("data", "ruw", "waterlopen", "vha_network_junctions.shp"), quiet = T)

meetnetten <- read.delim("data/ruw/vmm/meetnetten.txt")

#######
# netwerk bouwen
########

build_river_network <- function(lines_sf, nodes_sf) {

  message("Building network graph... (this may take a moment)")

  # 1. Prepareer coordinaten
  # We gaan ervan uit dat lines_sf StartX/Y en EndX/Y heeft, of we berekenen ze
  # Voor veiligheid berekenen we ze vers (consistentie)
  coords <- st_coordinates(lines_sf)

  # Startpunt is het eerste punt van elke lijn, Eindpunt het laatste
  # (Dit werkt voor LINESTRING, check of je MULTILINESTRING hebt, dan moet je casten)
  lines_sf <- st_cast(lines_sf, "LINESTRING", warn = FALSE)

  start_points <- st_line_sample(lines_sf, sample = 0)
  end_points <- st_line_sample(lines_sf, sample = 1)

  # 2. Match lijnen aan nodes
  # We zoeken welke node ID bij het start- en eindpunt hoort
  lines_sf$from_node <- st_nearest_feature(start_points, nodes_sf)
  lines_sf$to_node   <- st_nearest_feature(end_points, nodes_sf)

  # 3. Bereken lengtes voor gewichten
  lines_sf$length_m <- as.numeric(st_length(lines_sf))

  # 4. Bouw de graaf
  edges <- data.frame(
    from = as.character(lines_sf$from_node),
    to = as.character(lines_sf$to_node),
    weight = lines_sf$length_m,
    segment_id = seq_len(nrow(lines_sf))
  )

  g <- graph_from_data_frame(edges, directed = TRUE)

  message("Graph built successfully!")

  # Return een lijst met de graaf EN de ruimtelijke lijnen (nodig voor snapping later)
  return(list(graph = g, network_sf = lines_sf))
}

river_network <- build_river_network(fd, nodes)

# netwerk verfijnen door segmenten op te splitsen

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

fd_fine <- split_lines_equal_length(fd, max_length = 200)

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
  node_id_start <- paste(round(coords_start[,1], 3), round(coords_start[,2], 3), sep = "_")
  node_id_end   <- paste(round(coords_end[,1], 3),   round(coords_end[,2], 3),   sep = "_")

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


# ==============================================================================
# NIEUWE FUNCTIE: Match in een straal (Radius) - Ongeacht stroomrichting
# ==============================================================================
match_nearby <- function(target_sf,              # Bijv. Macro-invertebraten
                         source_sf,              # Bijv. Macrofyten
                         network_list,
                         max_dist_m = 1000,      # Zoek binnen 1km (via rivier)
                         year_window = 0,        # 0 = zelfde jaar, 1 = +/- 1 jaar
                         selection_mode = "closest_distance") {

  g <- network_list$graph
  net_sf <- network_list$network_sf

  # 1. Zorg voor datums en Jaren
  # We gaan ervan uit dat beide datasets een 'monsternamedatum' hebben
  target_sf$datum <- as.Date(target_sf$monsternamedatum)
  target_sf$jaar  <- lubridate::year(target_sf$datum)

  source_sf$datum <- as.Date(source_sf$monsternamedatum)
  source_sf$jaar  <- lubridate::year(source_sf$datum)

  # We droppen geometrie van source voor snellere verwerking in de loop
  source_df <- st_drop_geometry(source_sf)

  # ID voor terugkoppeling
  target_sf$match_id <- seq_len(nrow(target_sf))

  # 2. Snapping naar netwerk (als dat nog niet gebeurd is)
  snap_to_node <- function(points, network) {
    if("node_id" %in% names(points)) return(points$node_id)
    idx <- st_nearest_feature(points, network)
    return(as.character(network$from_node[idx]))
  }

  message("Snapping points to network...")
  target_sf$node_id <- snap_to_node(target_sf, net_sf)
  source_df$node_id <- snap_to_node(source_sf, net_sf)

  message("Matching spatial & temporal...")

  results <- list()
  pb <- progress::progress_bar$new(total = nrow(target_sf), format = "[:bar] :percent eta: :eta")

  for (i in seq_len(nrow(target_sf))) {
    pb$tick()

    t_row <- target_sf[i, ]
    t_node <- t_row$node_id
    t_year <- t_row$jaar

    # STAP A: Temporele filtering (EERST tijd filteren is veel sneller)
    # Zoek kandidaten binnen het jaar-venster
    candidates <- source_df %>%
      filter(jaar >= (t_year - year_window) & jaar <= (t_year + year_window))

    match_found <- NA

    if (nrow(candidates) > 0) {

      # STAP B: Ruimtelijke filtering via Graaf
      # Omdat we 'nearby' zoeken (stroomop EN af), kunnen we niet simpel 'subcomponent' doen
      # want dat pakt hele stroomgebieden.
      # We berekenen afstanden van t_node naar ALLE tijd-kandidaten.
      # 'mode = "all"' is hier de sleutel: het negeert de pijlen!

      # Optimalisatie: Als er HEEL veel kandidaten zijn, is afstanden berekenen traag.
      # Maar omdat we al gefilterd hebben op jaar, valt het vaak mee.

      unique_cand_nodes <- unique(candidates$node_id)

      # Bereken afstand via rivier (ongeacht richting)
      # Let op: distances geeft Inf als ze niet verbonden zijn
      dists <- distances(g, v = t_node, to = unique_cand_nodes, mode = "all")

      # Maak een lookup tabelletje
      dist_lookup <- data.frame(
        node_id = unique_cand_nodes,
        dist_m = as.numeric(dists)
      )

      # Koppel afstand terug aan kandidaten
      candidates <- candidates %>%
        left_join(dist_lookup, by = "node_id") %>%
        filter(dist_m <= max_dist_m) # Filter op max afstand

      # STAP C: Selectie van de beste
      if (nrow(candidates) > 0) {
        if (selection_mode == "closest_distance") {
          match_found <- candidates[which.min(candidates$dist_m), ]
        } else if (selection_mode == "closest_time") {
          # Dichtste in tijd (dagen verschil)
          candidates$dag_diff <- abs(as.numeric(t_row$datum - candidates$datum))
          match_found <- candidates[which.min(candidates$dag_diff), ]
        } else if (selection_mode == "all") {
          match_found <- candidates
        }
      }
    }

    # Lege rij als niks gevonden
    if (!is.data.frame(match_found)) {
      # Maak lege rij met zelfde kolommen als source_df
      match_found <- source_df[1, ]
      match_found[] <- NA
      match_found$dist_m <- NA
    }

    match_found$original_mi_id <- t_row$match_id
    results[[i]] <- match_found
  }

  message("Merging results...")
  final_df <- do.call(rbind, results)

  # Rename kolommen van de match om verwarring te voorkomen
  names(final_df)[names(final_df) != "original_mi_id"] <- paste0("mp_", names(final_df)[names(final_df) != "original_mi_id"])

  # Join terug aan originele data
  output_sf <- left_join(target_sf, final_df, by = c("match_id" = "original_mi_id"))
  output_sf$match_id <- NULL

  return(output_sf)
}


# ==============================================================================
# TOEPASSING
# ==============================================================================

# Laad macrofyten data
mafy_data <- st_read(here("data", "ruw", "macrofyten", "mafy_meetpunten_datum.gpkg")) %>%
  filter(monsternamedatum > '2009-12-31')

mi_data <- st_read(here("data", "ruw", "macroinvertebraten", "mi_meetpunten_datum.gpkg"), quiet = T) %>%
  filter(monsternamedatum > '2009-12-31')

# Voorbeeld aanroep:
# MI koppelen aan Macrofyten (MP)
# - Zelfde jaar (year_window = 0)
# - Maximaal 2km van elkaar verwijderd (via de rivier)
# - Zowel stroomop als stroomafwaarts

mi_met_mafy <- match_nearby(
  target_sf = mi_data,
  source_sf = mafy_data,
  network_list = river_network,
  max_dist_m = 5000,       # 2000 meter radius
  year_window = 0,         # Zelfde kalenderjaar
  selection_mode = "closest_distance"
)

print(paste("Aantal matches:", sum(!is.na(mi_met_mafy$mp_meetplaats))))
# Opslaan
# save(mi_met_mp, file = "mi_met_mp_matched.rdata")

mi_met_mafy_2km_2jaar <- match_nearby(
  target_sf = mi_data,
  source_sf = mafy_data,
  network_list = river_network_fine,
  max_dist_m = 2000,       # 2000 meter radius
  year_window = 1,         # Zelfde kalenderjaar
  selection_mode = "closest_distance"
)
print(paste("Aantal matches:", sum(!is.na(mi_met_mafy_2km_2jaar$mp_meetplaats))))
save(mi_met_mafy_2km_2jaar, file = here("data", "verwerkt" , "koppeling", "koppeling_mi_mafy_2km_2jaar.rdata"))


mi_met_mafy_1km_2jaar %>% filter(!is.na(mp_meetplaats)) %>% select(meetplaats, monsternamedatum)

#### koppelen op jaar en OWL ####

mi_data <- st_read(here("data", "ruw", "macroinvertebraten", "mi_meetpunten_datum.gpkg"), quiet = T) %>%
  filter(monsternamedatum > '2009-12-31') %>%
  left_join(meetnetten %>%
              select(owl_code, vhas, nummer),
            by = c("meetplaats" = "nummer"))
load(file = here("data", "verwerkt", "mafy_data.rdata"))

mafy_data <- mafy_data %>%
  filter(monsternamedatum > '2009-12-31') %>%
  select(meetplaats, monsternamedatum, owl)

# 1. Voeg een 'jaar' kolom toe aan mi_data
mi_data <- mi_data %>%
  mutate(jaar = lubridate::year(monsternamedatum)) %>%
  st_drop_geometry() %>%
  as_tibble()

# 2. Voeg een 'jaar' kolom toe aan mafy_data
mafy_data <- mafy_data %>%
  mutate(jaar = lubridate::year(monsternamedatum)) %>%
  st_drop_geometry()

# 3. Koppel de dataframes
gekoppelde_data <- mi_data %>%
  inner_join(mafy_data,
             by = c("owl_code" = "owl", "jaar" = "jaar"),
             suffix = c("_mi", "_mafy"))
