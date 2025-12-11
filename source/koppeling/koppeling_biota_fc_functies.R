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
                           max_dist_m = 5000,
                           days_before = 14,
                           days_after = 30,
                           col_date_biota = "monsternamedatum",
                           col_date_quality = "monsternamedatum",
                           selection_mode = "closest_distance") { # NIEUW: Keuze optie

  # Validatie van de modus
  valid_modes <- c("closest_distance", "closest_time", "all")
  if (!selection_mode %in% valid_modes) stop("selection_mode moet zijn: 'closest_distance', 'closest_time' of 'all'")

  g <- network_list$graph
  net_sf <- network_list$network_sf

  biota_sf[[col_date_biota]] <- as.Date(biota_sf[[col_date_biota]])
  quality_sf[[col_date_quality]] <- as.Date(quality_sf[[col_date_quality]])

  # Voeg een uniek ID toe aan biota om later correct te kunnen joinen (nodig voor 'all' optie)
  biota_sf$tmp_join_id <- seq_len(nrow(biota_sf))

  message("1/3 Snapping points to river network...")

  snap_to_node <- function(points, network) {
    nearest_segment_idx <- st_nearest_feature(points, network)
    return(as.character(network$from_node[nearest_segment_idx]))
  }

  biota_sf$node_id <- snap_to_node(biota_sf, net_sf)
  quality_sf$node_id <- snap_to_node(quality_sf, net_sf)

  quality_df <- st_drop_geometry(quality_sf)

  message("2/3 Matching upstream points...")

  results <- list()

  pb <- progress_bar$new(
    format = "  [:bar] :percent eta: :eta",
    total = nrow(biota_sf), clear = FALSE, width = 60
  )

  for (i in seq_len(nrow(biota_sf))) {
    pb$tick()

    b_pt <- biota_sf[i, ]
    b_date <- b_pt[[col_date_biota]]
    b_node <- b_pt$node_id
    b_id   <- b_pt$tmp_join_id # We houden het ID bij

    upstream_nodes <- names(subcomponent(g, v = b_node, mode = "in"))

    # We berekenen hier alvast het tijdsverschil en slaan het op als kolom
    # zodat we er later op kunnen sorteren voor 'closest_time'
    candidates <- quality_df %>%
      filter(node_id %in% upstream_nodes) %>%
      mutate(
        temp_diff_days = as.numeric(difftime(b_date, .[[col_date_quality]], units = "days")),
        abs_time_diff  = abs(temp_diff_days) # Absolute verschil voor sortering
      ) %>%
      filter(temp_diff_days >= -days_after & temp_diff_days <= days_before)

    match <- NA

    if (nrow(candidates) > 0) {
      d_matrix <- distances(
        g,
        v = candidates$node_id,
        to = b_node,
        mode = "out"
      )

      candidates$river_dist_m <- as.numeric(d_matrix)
      candidates_within <- candidates %>% filter(river_dist_m <= max_dist_m)

      if (nrow(candidates_within) > 0) {

        # --- HIER ZIT DE NIEUWE LOGICA ---
        if (selection_mode == "closest_distance") {
          # Oude gedrag: pak de kleinste afstand
          match <- candidates_within[which.min(candidates_within$river_dist_m), ]

        } else if (selection_mode == "closest_time") {
          # Nieuw: pak het kleinste tijdsverschil
          match <- candidates_within[which.min(candidates_within$abs_time_diff), ]

        } else if (selection_mode == "all") {
          # Nieuw: behoud alles
          match <- candidates_within
        }
      }
    }

    # Afhandeling als er geen match is (lege rij maken)
    if (!is.data.frame(match)) {
      match <- quality_df[1, ]
      match[] <- NA
      match$river_dist_m <- NA
    }

    # Belangrijk: Voeg het biota ID toe aan de match(es) zodat we weten bij wie ze horen
    match$biota_match_id <- b_id

    # Verwijder tijdelijke hulpkolommen (optioneel, houdt het schoon)
    if("temp_diff_days" %in% names(match)) match$temp_diff_days <- NULL
    if("abs_time_diff" %in% names(match)) match$abs_time_diff <- NULL

    results[[i]] <- match
  }

  message("3/3 Merging results...")

  # Bind alle gevonden matches onder elkaar
  matched_quality_df <- do.call(rbind, results)

  # Hernoem kolommen (behalve het ID waarop we joinen)
  cols_to_rename <- setdiff(names(matched_quality_df), "biota_match_id")
  names(matched_quality_df)[names(matched_quality_df) %in% cols_to_rename] <-
    paste0("qual_", cols_to_rename)

  # Joinen: Nu gebruiken we left_join i.p.v. bind_cols
  # Dit is robuuster en werkt ook voor "all" (waarbij biota rijen gedupliceerd worden)
  final_sf <- left_join(biota_sf, matched_quality_df, by = c("tmp_join_id" = "biota_match_id"))

  # Ruim de tijdelijke ID op
  final_sf$tmp_join_id <- NULL

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
