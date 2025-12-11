library(sf)
library(igraph)
library(tidyverse)
library(progress)

# ==============================================================================
# FUNCTIE 1: Bouw het netwerk (Doe dit 1 keer aan het begin)
# ==============================================================================
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

# ==============================================================================
# FUNCTIE 2: Match Biota aan Kwaliteit (De flexibele functie)
# ==============================================================================
match_upstream <- function(biota_sf,
                           quality_sf,
                           network_list,
                           max_dist_m = 5000, # zelf in te vullen
                           days_before = 14,
                           days_after = 30,
                           col_date_biota = "monsternamedatum",
                           col_date_quality = "monsternamedatum") {

  # Haal graaf en lijnen uit de input lijst
  g <- network_list$graph
  net_sf <- network_list$network_sf

  # Zorg dat datums correct zijn
  biota_sf[[col_date_biota]] <- as.Date(biota_sf[[col_date_biota]])
  quality_sf[[col_date_quality]] <- as.Date(quality_sf[[col_date_quality]])

  message("1/3 Snapping points to river network...")

  # Helper functie voor snapping (zonder QGIS dependency)
  snap_to_node <- function(points, network) {
    # Vind dichtstbijzijnde lijnsegment
    nearest_segment_idx <- st_nearest_feature(points, network)
    # Wijs de 'from_node' van dat segment toe aan het punt
    # (We nemen from_node omdat we stroomopwaarts kijken vanaf het begin van het segment)
    return(as.character(network$from_node[nearest_segment_idx]))
  }

  biota_sf$node_id <- snap_to_node(biota_sf, net_sf)
  quality_sf$node_id <- snap_to_node(quality_sf, net_sf)

  # Drop geometry van quality voor snellere filtering (we gebruiken de node ID)
  quality_df <- st_drop_geometry(quality_sf)

  message("2/3 Matching upstream points...")

  results <- list()

  # Progress bar
  pb <- progress_bar$new(
    format = "  [:bar] :percent eta: :eta",
    total = nrow(biota_sf), clear = FALSE, width = 60
  )

  for (i in seq_len(nrow(biota_sf))) {
    pb$tick()

    b_pt <- biota_sf[i, ]
    b_date <- b_pt[[col_date_biota]]
    b_node <- b_pt$node_id

    # 1. Vind alle stroomopwaartse knopen
    upstream_nodes <- names(subcomponent(g, v = b_node, mode = "in"))

    # 2. Filter kandidaten (Ruimtelijk & Temporeel)
    candidates <- quality_df %>%
      filter(
        node_id %in% upstream_nodes,
        {
          d_diff <- as.numeric(difftime(b_date, .[[col_date_quality]], units = "days"))
          d_diff >= -days_after & d_diff <= days_before
        }
      )

    match <- NA

    if (nrow(candidates) > 0) {
      # 3. Bereken afstanden (Gevectoriseerd)
      d_matrix <- distances(
        g,
        v = candidates$node_id,
        to = b_node,
        mode = "out" # Met de stroom mee van kandidaat naar biota
      )

      candidates$river_dist_m <- as.numeric(d_matrix)

      # 4. Filter op afstand en kies dichtstbijzijnde
      candidates_within <- candidates %>% filter(river_dist_m <= max_dist_m)

      if (nrow(candidates_within) > 0) {
        match <- candidates_within[which.min(candidates_within$river_dist_m), ]
      }
    }

    # Als match NA is, maak een lege rij op basis van quality structuur
    if (!is.data.frame(match)) {
      match <- quality_df[1, ]
      match[] <- NA
    }

    # Voeg afstand toe aan match als die er nog niet is (bij NA)
    if (!"river_dist_m" %in% names(match)) match$river_dist_m <- NA

    results[[i]] <- match
  }

  message("3/3 Merging results...")

  # Bind alles samen
  matched_quality_df <- do.call(rbind, results)

  # Hernoem kolommen van kwaliteit om botsingen te voorkomen
  names(matched_quality_df) <- paste0("qual_", names(matched_quality_df))

  # Voeg samen met originele biota
  final_sf <- bind_cols(biota_sf, matched_quality_df)

  return(final_sf)
}

# --- STAP 0: Data Inladen ---
# (Laad hier je shapefiles in zoals je gewend bent)
fd <- st_read(here("data", "ruw", "netwerk", "Flow_direction_coordinates.shp"), quiet=T)
nodes <- st_read(here("data", "ruw", "waterlopen", "vha_network_junctions.shp"), quiet=T)

mi_data <- st_read(here("data", "ruw", "macroinvertebraten", "mi_meetpunten_datum.gpkg"), quiet=T) %>%
  filter(monsternamedatum > '2009-12-31')

fc_data <- st_read(here("data", "ruw", "fys_chem", "fc_meetpunten.gpkg"), quiet=T) %>%
  filter(monsternamedatum > '2007-12-31')


# --- STAP 1: Bouw het netwerk (slechts 1x nodig!) ---
river_network <- build_river_network(fd, nodes)


# --- STAP 2: Voer de matching uit (Fysico-chemie) ---
# Hier kun je spelen met de parameters
mi_met_fc <- match_upstream(
  biota_sf = mi_data,
  quality_sf = fc_data,
  network_list = river_network,
  max_dist_m = 5000,       # Max 5km stroomopwaarts
  days_before = 180,       # Kwaliteit mag tot 180 dagen VOOR de biota meting zijn
  days_after = 14,         # Kwaliteit mag tot 14 dagen NA de biota meting zijn
  col_date_biota = "monsternamedatum",
  col_date_quality = "monsternamedatum"
)

# --- STAP 3: Voer de matching uit (Nutriënten - voorbeeld) ---
# Stel dat je een aparte nutriënten laag hebt:
# nutrient_data <- ...
# mi_met_nutrienten <- match_upstream(mi_data, nutrient_data, river_network, max_dist_m = 3000, ...)

# Opslaan
save(mi_met_fc, file = "data/verwerkt/mi_met_fc_matched.rdata")

# Check resultaat
print(paste("Aantal matches:", sum(!is.na(mi_met_fc$qual_meetplaats))))
