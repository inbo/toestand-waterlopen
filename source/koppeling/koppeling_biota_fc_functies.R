library(sf)
library(igraph)
library(tidyverse)
library(progress)
library(here)

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
                           discharge_sf = NULL, # NIEUW: Optionele laag met lozingspunten
                           max_dist_m = 5000,
                           days_before = 14,
                           days_after = 30,
                           col_date_biota = "monsternamedatum",
                           col_date_quality = "monsternamedatum",
                           selection_mode = "closest_distance") {

  # 1. Validatie
  valid_modes <- c("closest_distance", "closest_time", "all")
  if (!selection_mode %in% valid_modes) stop("selection_mode moet zijn: 'closest_distance', 'closest_time' of 'all'")

  g <- network_list$graph
  net_sf <- network_list$network_sf

  # Datum conversies
  biota_sf[[col_date_biota]] <- as.Date(biota_sf[[col_date_biota]])
  quality_sf[[col_date_quality]] <- as.Date(quality_sf[[col_date_quality]])
  biota_sf$tmp_join_id <- seq_len(nrow(biota_sf))

  message("1/4 Snapping points to river network...")

  snap_to_node <- function(points, network) {
    nearest_segment_idx <- st_nearest_feature(points, network)
    return(as.character(network$from_node[nearest_segment_idx]))
  }

  biota_sf$node_id <- snap_to_node(biota_sf, net_sf)
  quality_sf$node_id <- snap_to_node(quality_sf, net_sf)
  quality_df <- st_drop_geometry(quality_sf)

  # --- NIEUW: Verwerk lozingspunten indien aanwezig ---
  discharge_nodes <- character(0)
  if (!is.null(discharge_sf)) {
    message("    -> Snapping discharge points...")
    discharge_sf$node_id <- snap_to_node(discharge_sf, net_sf)
    discharge_nodes <- unique(as.character(discharge_sf$node_id))
  }
  # ----------------------------------------------------

  message("2/4 Matching upstream points...")

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
    b_id   <- b_pt$tmp_join_id

    # Vind upstream kandidaten
    upstream_nodes <- names(subcomponent(g, v = b_node, mode = "in"))

    # Filter kandidaten (upstream + tijd)
    candidates <- quality_df %>%
      filter(node_id %in% upstream_nodes) %>%
      mutate(
        temp_diff_days = as.numeric(difftime(b_date, .[[col_date_quality]], units = "days")),
        abs_time_diff  = abs(temp_diff_days)
      ) %>%
      filter(temp_diff_days >= -days_after & temp_diff_days <= days_before)

    match <- NA

    if (nrow(candidates) > 0) {
      # Afstand berekenen
      d_matrix <- distances(g, v = candidates$node_id, to = b_node, mode = "out")
      candidates$river_dist_m <- as.numeric(d_matrix)

      # Filter op max afstand
      candidates_within <- candidates %>% filter(river_dist_m <= max_dist_m)

      # --- NIEUW: Filter op lozingspunten (Topology Check) ---
      if (nrow(candidates_within) > 0 && length(discharge_nodes) > 0) {

        # We maken een vector van TRUE/FALSE om kandidaten te behouden
        keep_candidate <- sapply(candidates_within$node_id, function(cand_node) {

          # Als kandidaat of biota zelf een lozingspunt is, is dat meestal OK (grensgeval).
          # We checken of er een lozingspunt TUSSENIN ligt.
          if (cand_node == b_node) return(TRUE)

          # Bereken het pad van kandidaat (stroomopwaarts) naar biota (stroomafwaarts)
          # mode="out" volgt de stroomrichting
          path_res <- shortest_paths(g, from = cand_node, to = b_node, mode = "out")
          path_nodes <- names(path_res$vpath[[1]])

          # Haal start (kwaliteit) en eind (biota) van het pad af
          # We kijken puur naar wat er 'tussen' ligt
          intermediate_nodes <- setdiff(path_nodes, c(cand_node, b_node))

          # Als een van de tussenliggende nodes een lozingspunt is -> FALSE (verwerp match)
          if (any(intermediate_nodes %in% discharge_nodes)) {
            return(FALSE)
          } else {
            return(TRUE)
          }
        })

        # Filter de kandidatenlijst
        candidates_within <- candidates_within[keep_candidate, ]
      }
      # -------------------------------------------------------

      # Selecteer de beste match op basis van de modus
      if (nrow(candidates_within) > 0) {
        if (selection_mode == "closest_distance") {
          match <- candidates_within[which.min(candidates_within$river_dist_m), ]
        } else if (selection_mode == "closest_time") {
          match <- candidates_within[which.min(candidates_within$abs_time_diff), ]
        } else if (selection_mode == "all") {
          match <- candidates_within
        }
      }
    }

    # Lege rij opvulling indien geen match
    if (!is.data.frame(match)) {
      match <- quality_df[1, ]
      match[] <- NA
      match$river_dist_m <- NA
    }

    match$biota_match_id <- b_id

    if("temp_diff_days" %in% names(match)) match$temp_diff_days <- NULL
    if("abs_time_diff" %in% names(match)) match$abs_time_diff <- NULL

    results[[i]] <- match
  }

  message("3/4 Merging results...")
  matched_quality_df <- do.call(rbind, results)

  cols_to_rename <- setdiff(names(matched_quality_df), "biota_match_id")
  names(matched_quality_df)[names(matched_quality_df) %in% cols_to_rename] <-
    paste0("qual_", cols_to_rename)

  final_sf <- left_join(biota_sf, matched_quality_df, by = c("tmp_join_id" = "biota_match_id"))
  final_sf$tmp_join_id <- NULL

  return(final_sf)
}

# --- STAP 0: Data Inladen ---
# (Laad hier je shapefiles in zoals je gewend bent)
fd <- st_read(here("data", "ruw", "netwerk", "Flow_direction_coordinates.shp"), quiet = T)
nodes <- st_read(here("data", "ruw", "waterlopen", "vha_network_junctions.shp"), quiet = T)

mi_data <- st_read(here("data", "ruw", "macroinvertebraten", "mi_meetpunten_datum.gpkg"), quiet = T) %>%
  filter(monsternamedatum > '2009-12-31')

fc_data <- st_read(here("data", "ruw", "fys_chem", "fc_meetpunten.gpkg"), quiet = T) %>%
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
  col_date_quality = "monsternamedatum",
  selection_mode = "closest_distance"
)

# Opslaan
save(mi_met_fc, file = "data/verwerkt/koppeling/mi_met_fc_matched.rdata")

# Check resultaat
print(paste("Aantal matches:", sum(!is.na(mi_met_fc$qual_meetplaats))))


# --- STAP 3: Voer de matching uit (Nutriënten - voorbeeld) ---
# Stel dat je een aparte nutriënten laag hebt:


if (!file.exists(here("data", "verwerkt", "koppeling", "nutrient_meetpunten_datum.gpkg"))) {
    load(here("data", "verwerkt", "fc_selectie.rdata"))
    fc_meetpunten_datum <- st_read(here("data", "ruw", "fys_chem", "fc_meetpunten.gpkg"), quiet = T) %>%
      filter(monsternamedatum > '2007-12-31')
    nutrient_meetpunten_datum <- fc_selectie %>%
      filter(!is.na(n_t)) %>%
      select(meetplaats, monsternamedatum) %>%
      left_join(fc_meetpunten_datum,
                by = c("meetplaats", "monsternamedatum"))
    st_write(nutrient_meetpunten_datum, dsn = here("data", "verwerkt", "koppeling", "nutrient_meetpunten_datum.gpkg"))
  }
nutrient_data <- st_read(dsn = here("data", "verwerkt", "koppeling", "nutrient_meetpunten_datum.gpkg"), quiet = T)

mi_met_nutrient <- match_upstream(
  biota_sf = mi_data,
  quality_sf = nutrient_data,
  network_list = river_network,
  max_dist_m = 5000,       # Max 5km stroomopwaarts
  days_before = 180,       # Kwaliteit mag tot 180 dagen VOOR de biota meting zijn
  days_after = 14,         # Kwaliteit mag tot 14 dagen NA de biota meting zijn
  col_date_biota = "monsternamedatum",
  col_date_quality = "monsternamedatum",
  selection_mode = "closest_distance"
)

save(mi_met_nutrient, file = "data/verwerkt/koppeling/mi_met_nutrient_matched.rdata")
mi_met_nutrient %>% drop_na(qual_meetplaats) %>% nrow

# --- STAP 4: Voer de matching uit (Nutriënten - maar met overstortenlaag bij) ---
overstorten_uitlaat_vha <- st_read(here("data", "ruw", "overstorten", "P_OS_uitlaat_VHA.shp"))

mi_met_fc_overstorten <- match_upstream(
  biota_sf = mi_data,
  quality_sf = nutrient_data,
  network_list = river_network,
  discharge_sf = overstorten_uitlaat_vha,
  max_dist_m = 5000,       # Max 5km stroomopwaarts
  days_before = 180,       # Kwaliteit mag tot 180 dagen VOOR de biota meting zijn
  days_after = 14,         # Kwaliteit mag tot 14 dagen NA de biota meting zijn
  col_date_biota = "monsternamedatum",
  col_date_quality = "monsternamedatum",
  selection_mode = "closest_distance"
)

# --- STAP 4: Voer de matching uit (pesticiden TU) ---
