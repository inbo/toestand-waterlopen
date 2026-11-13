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
# Match simpel - Ongeacht stroomrichting
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
# mafy_data <- st_read(here("data", "ruw", "macrofyten", "mafy_meetpunten_datum.gpkg")) %>%
#   filter(monsternamedatum > '2009-12-31')

load(here("data", "verwerkt", "mafy_data.rdata"))
source(here::here("source", "analyse", "mafy_datasets_prep.R"))

mafy_data <- mafy_data3 %>%
  select(meetplaats, monsternamedatum, index_zonder_gep, index_nieuw, vo_zonder_gep, v_zonder_gep, gv_zonder_gep, ts_zonder_gep, vhas)

mi_data <- st_read(here("data", "ruw", "macroinvertebraten", "mi_meetpunten_datum.gpkg"), quiet = T) %>%
  filter(monsternamedatum > '2009-12-31')

load(here("data", "ruw", "vis", "traits_all_obs_no_marien_EQR.Rdata"))
vis_data <- data_traits_EQR %>%
  filter(MethodeGroepOmschrijving == "Elektrisch") %>%
  select(Datum, WaarnemingVispuntID, VispuntX, VispuntY, VHAS, VHAG,
         perc_inv_fresh, perc_exoot, EQR) %>%
  st_as_sf(coords = c("VispuntX", "VispuntY"), crs = st_crs(mi_data)) %>%
  rename(meetplaats = WaarnemingVispuntID,
         monsternamedatum = Datum) %>%
  mutate(monsternamedatum = as.Date(monsternamedatum),
         meetplaats = as.factor(meetplaats)) %>%
  filter(!is.na(VHAS))

# Voorbeeld aanroep:
# MI koppelen aan Macrofyten (MP)
# - Zelfde jaar (year_window = 0)
# - Maximaal 2km van elkaar verwijderd (via de rivier)
# - Zowel stroomop als stroomafwaarts

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

mi_met_vis_2km_2jaar <- match_nearby(
  target_sf = mi_data,
  source_sf = vis_data,
  network_list = river_network_fine,
  max_dist_m = 2000,       # 2000 meter radius
  year_window = 1,         # Zelfde kalenderjaar
  selection_mode = "closest_distance"
)
print(paste("Aantal matches:", sum(!is.na(mi_met_vis_2km_2jaar$mp_meetplaats))))
save(mi_met_vis_2km_2jaar, file = here("data", "verwerkt" , "koppeling", "koppeling_mi_vis_2km_2jaar.rdata"))

# ==============================================================================
# nieuwe functie
# ==============================================================================
match_biota_network <- function(sf_base,
                                sf_match,
                                network_list,
                                max_upstream_m = 5000,
                                max_downstream_m = 2000,
                                snap_tolerance = 25,
                                days_before = 30,
                                days_after = 10,
                                col_date_base = "monsternamedatum",
                                col_date_match = "monsternamedatum",
                                selection_mode = "closest_distance",
                                aggr_method = "mean",
                                aggr_cols = NULL,
                                grouping_col = NULL,
                                vhas_col_network = "VHAS",
                                vhas_col_base = NULL,
                                vhas_col_match = NULL) {

  # --- 1. Validatie ---
  valid_modes <- c("closest_distance", "closest_time", "all", "aggregate")
  if (!selection_mode %in% valid_modes) stop("selection_mode moet zijn: 'closest_distance', 'closest_time', 'all' of 'aggregate'")

  valid_aggr <- c("mean", "min", "max")
  if (!aggr_method %in% valid_aggr) stop("aggr_method moet zijn: 'mean', 'min' of 'max'")

  if (!is.null(aggr_cols)) {
    missing_cols <- setdiff(aggr_cols, names(sf_match))
    if (length(missing_cols) > 0) stop(paste("aggr_cols niet gevonden in sf_match:", paste(missing_cols, collapse=", ")))
  }

  g <- network_list$graph
  net_sf <- network_list$network_sf

  if (!is.null(grouping_col)) {
    if (!grouping_col %in% names(net_sf)) stop(paste("De grouping_col", grouping_col, "bestaat niet in je netwerk!"))
    message(paste0("Grouping geactiveerd: enkel matchen binnen zelfde '", grouping_col, "'."))
  }

  # Datum conversies
  sf_base[[col_date_base]] <- as.Date(sf_base[[col_date_base]])
  sf_match[[col_date_match]] <- as.Date(sf_match[[col_date_match]])
  sf_base$tmp_join_id <- seq_len(nrow(sf_base))

  sf_match$orig_index_geo <- seq_len(nrow(sf_match))

  message("1/4 Snapping points to river network...")

  # --- HYBRID SNAP FUNCTIE ---
  snap_and_attribute <- function(points, network, grp_col, tol, vhas_col_pts, vhas_col_net) {
    points$node_id <- NA
    if (!is.null(grp_col)) points$match_group_val <- NA

    # 1. VHAS MATCHING
    has_vhas_config <- !is.null(vhas_col_pts) && !is.null(vhas_col_net)
    can_use_vhas <- has_vhas_config && (vhas_col_pts %in% names(points)) && (vhas_col_net %in% names(network))

    if (can_use_vhas) {
      pts_with_vhas_idx <- which(!is.na(points[[vhas_col_pts]]))
      if (length(pts_with_vhas_idx) > 0) {
        unique_codes <- unique(points[[vhas_col_pts]][pts_with_vhas_idx])
        for (code in unique_codes) {
          net_idx <- which(network[[vhas_col_net]] == code)
          if (length(net_idx) > 0) {
            p_subset_idx <- which(points[[vhas_col_pts]] == code)
            rel_nearest <- st_nearest_feature(points[p_subset_idx,], network[net_idx,])
            abs_nearest <- net_idx[rel_nearest]
            points$node_id[p_subset_idx] <- as.character(network$from_node[abs_nearest])
            if (!is.null(grp_col)) points$match_group_val[p_subset_idx] <- network[[grp_col]][abs_nearest]
          }
        }
      }
    }

    # 2. GEOMETRIC FALLBACK
    missing_idx <- which(is.na(points$node_id))
    if (length(missing_idx) > 0) {
      subset_pts <- points[missing_idx, ]
      nearest_idx <- st_nearest_feature(subset_pts, network)
      dists <- st_distance(subset_pts, network[nearest_idx, ], by_element = TRUE)
      dists_num <- as.numeric(dists)
      dists_num[is.na(dists_num)] <- Inf

      valid_snaps <- dists_num <= tol
      valid_global_idx <- missing_idx[valid_snaps]
      valid_net_idx <- nearest_idx[valid_snaps]

      if (length(valid_global_idx) > 0) {
        points$node_id[valid_global_idx] <- as.character(network$from_node[valid_net_idx])
        if (!is.null(grp_col)) points$match_group_val[valid_global_idx] <- network[[grp_col]][valid_net_idx]
      }

      n_dropped <- length(missing_idx) - length(valid_global_idx)
      if (n_dropped > 0) message(paste("      ->", n_dropped, "points dropped (too far/no VHAS match)."))
    }
    return(points)
  }

  sf_base <- snap_and_attribute(sf_base, net_sf, grouping_col, snap_tolerance, vhas_col_base, vhas_col_network)
  sf_match <- snap_and_attribute(sf_match, net_sf, grouping_col, snap_tolerance, vhas_col_match, vhas_col_network)

  df_match <- st_drop_geometry(sf_match)

  message(paste0("2/4 Matching points (Mode: ", selection_mode, " | Upstream: ", max_upstream_m, "m | Downstream: ", max_downstream_m, "m)..."))

  results <- list()
  pb <- progress_bar$new(format = "  [:bar] :percent eta: :eta", total = nrow(sf_base), clear = FALSE, width = 60)

  for (i in seq_len(nrow(sf_base))) {
    pb$tick()
    b_pt <- sf_base[i, ]
    b_node <- b_pt$node_id
    b_id   <- b_pt$tmp_join_id
    match_row <- NA
    n_matches_found <- 0

    if (is.na(b_node)) {
      match_row <- df_match[1, ]
      match_row[] <- NA; match_row$river_dist_m <- NA
    } else {
      b_date <- b_pt[[col_date_base]]

      # STAP 1: Filter op Groep en Tijd
      subset_match <- df_match %>% filter(!is.na(node_id))

      if (!is.null(grouping_col)) {
        b_group_val <- b_pt$match_group_val
        if (!is.na(b_group_val)) {
          subset_match <- subset_match %>% filter(match_group_val == b_group_val)
        } else {
          subset_match <- subset_match[0, ]
        }
      }

      if (nrow(subset_match) > 0) {
        subset_match <- subset_match %>%
          mutate(
            temp_diff_days = as.numeric(difftime(b_date, .[[col_date_match]], units = "days")),
            abs_time_diff  = abs(temp_diff_days)
          ) %>%
          filter(temp_diff_days >= -days_after & temp_diff_days <= days_before)
      }

      if (nrow(subset_match) > 0) {

        # STAP 2: Bereken afstanden (Stroomopwaarts & Stroomafwaarts via graaf)
        upstream_nodes <- names(subcomponent(g, v = b_node, mode = "in"))
        downstream_nodes <- names(subcomponent(g, v = b_node, mode = "out"))

        subset_match$is_upstream <- subset_match$node_id %in% upstream_nodes
        subset_match$is_downstream <- subset_match$node_id %in% downstream_nodes

        # --- FIX VOOR IGRAPH DUPLICATES ---
        # Bepaal unieke knooppunten in de match-set
        unique_target_nodes <- unique(subset_match$node_id)

        # Afstand stroomopwaarts: van unieke match punten NAAR basis punt
        d_up_matrix <- distances(g, v = unique_target_nodes, to = b_node, mode = "out")

        # Afstand stroomafwaarts: van basis punt NAAR unieke match punten
        d_down_matrix <- distances(g, v = b_node, to = unique_target_nodes, mode = "out")

        # Maak named vectors (lookups)
        lookup_up <- setNames(as.numeric(d_up_matrix), unique_target_nodes)
        lookup_down <- setNames(as.numeric(d_down_matrix), unique_target_nodes)

        # Map de afstanden terug naar de volledige dataframe (zelfs bij dubbele coördinaten)
        subset_match$dist_up <- lookup_up[subset_match$node_id]
        subset_match$dist_down <- lookup_down[subset_match$node_id]
        # -----------------------------------

        # STAP 3: Filter op afstand
        candidates <- subset_match %>%
          filter(
            (is_upstream & dist_up <= max_upstream_m) |
              (is_downstream & dist_down <= max_downstream_m)
          )

        if (nrow(candidates) > 0) {
          # Neem de kortste afstand (een punt kan stroomop én stroomaf liggen als dist=0 is)
          candidates$river_dist_m <- pmin(candidates$dist_up, candidates$dist_down, na.rm = TRUE)

          # STAP 4: Selectie / Aggregatie
          if (selection_mode == "closest_distance") {
            match_row <- candidates[which.min(candidates$river_dist_m), ]; n_matches_found <- 1
          } else if (selection_mode == "closest_time") {
            match_row <- candidates[which.min(candidates$abs_time_diff), ]; n_matches_found <- 1
          } else if (selection_mode == "all") {
            match_row <- candidates; n_matches_found <- nrow(candidates)
          } else if (selection_mode == "aggregate") {
            n_matches_found <- nrow(candidates)
            if (!is.null(aggr_cols)) { cols_to_agg <- aggr_cols } else {
              tech_cols <- c("node_id", "river_dist_m", "dist_up", "dist_down", "temp_diff_days", "abs_time_diff", "match_group_val", "orig_index_geo", "is_upstream", "is_downstream")
              num_cols <- names(select_if(candidates, is.numeric))
              cols_to_agg <- setdiff(num_cols, tech_cols)
            }
            agg_fun <- switch(aggr_method, "mean"=function(x) mean(x, na.rm=T), "min"=function(x) min(x, na.rm=T), "max"=function(x) max(x, na.rm=T))

            if (length(cols_to_agg) > 0) {
              agg_values <- candidates %>% summarise(across(all_of(cols_to_agg), agg_fun))
              match_row <- candidates[1, ]
              match_row[, cols_to_agg] <- agg_values
            } else { match_row <- candidates[1, ] }
            match_row$river_dist_m <- mean(candidates$river_dist_m)
          }
        }
      }
      if (!is.data.frame(match_row)) { match_row <- df_match[1, ]; match_row[] <- NA; match_row$river_dist_m <- NA }
    }
    match_row$base_match_id <- b_id
    match_row$n_matches <- n_matches_found

    # Cleanup extra technische kolommen voor we mergen
    cols_to_remove <- c("temp_diff_days", "abs_time_diff", "match_group_val", "orig_index_geo", "is_upstream", "is_downstream", "dist_up", "dist_down")
    match_row[, base::intersect(names(match_row), cols_to_remove)] <- NULL
    results[[i]] <- match_row
  }

  message("3/4 Merging results...")
  matched_df <- do.call(rbind, results)
  cols_to_rename <- setdiff(names(matched_df), "base_match_id")

  # Prefix wordt 'match_' in plaats van 'qual_'
  names(matched_df)[names(matched_df) %in% cols_to_rename] <- paste0("match_", cols_to_rename)
  final_sf <- left_join(sf_base, matched_df, by = c("tmp_join_id" = "base_match_id"))

  # Laatste opschoning
  final_sf$tmp_join_id <- NULL
  if("match_group_val" %in% names(final_sf)) final_sf$match_group_val <- NULL

  message("4/4 Klaar!")
  return(final_sf)
}

mi_mafy_gekoppeld <- match_biota_network(
  sf_base = mi_data,                  # De MI meetpunten (jouw focus/respons)
  sf_match = mafy_data,               # De MAFY meetpunten (de habitatvormers)
  network_list = river_network_fine,  # Het opgebouwde netwerk

  # --- 1. Ruimtelijke instellingen (Afstanden) ---
  max_upstream_m = 1000,    # Zoek tot 1 km stroomopwaarts
  max_downstream_m = 500,   # Zoek tot 500 m stroomafwaarts
  snap_tolerance = 25,      # Mogen we iets ruimer nemen (50m) om snelle snaps te garanderen

  # --- 2. Temporele instellingen (Tijdsvenster) ---
  days_before = 365,         # Planten tot 3 maanden (1 kwartaal) vóór de MI staalname
  days_after = 180,          # Planten tot 3 maanden ná de MI staalname

  # --- 3. Match & Aggregatie instellingen ---
  selection_mode = "aggregate",  # Neem het gemiddelde als er meerdere mafy-punten in de buurt liggen
  aggr_method = "mean",
  aggr_cols = c("index_zonder_gep", "index_nieuw", "vo_zonder_gep", "v_zonder_gep", "gv_zonder_gep", "ts_zonder_gep"), # PAS AAN: de kolommen in mafy_data die je in je pSEM wil steken

  # --- 4. Verplichte criteria uit jouw vraag ---
  grouping_col = "VHAG",
  vhas_col_network = "VHAS",
  vhas_col_base = "vhas",
  vhas_col_match = "vhas"
)
print(paste("Aantal matches:", sum(!is.na(mi_mafy_gekoppeld$match_meetplaats))))
mi_mafy_gekoppeld

mi_vis_gekoppeld <- match_biota_network(
  sf_base = mi_data,                  # De MI meetpunten (jouw focus/respons)
  sf_match = vis_data,               # De MAFY meetpunten (de habitatvormers)
  network_list = river_network_fine,  # Het opgebouwde netwerk

  # --- 1. Ruimtelijke instellingen (Afstanden) ---
  max_upstream_m = 2000,    # Zoek tot 1 km stroomopwaarts
  max_downstream_m = 1000,   # Zoek tot 500 m stroomafwaarts
  snap_tolerance = 25,      # Mogen we iets ruimer nemen (50m) om snelle snaps te garanderen

  # --- 2. Temporele instellingen (Tijdsvenster) ---
  days_before = 365,         # Planten tot 3 maanden (1 kwartaal) vóór de MI staalname
  days_after = 365,          # Planten tot 3 maanden ná de MI staalname

  # --- 3. Match & Aggregatie instellingen ---
  selection_mode = "aggregate",  # Neem het gemiddelde als er meerdere mafy-punten in de buurt liggen
  aggr_method = "mean",
  aggr_cols = c("perc_inv_fresh", "perc_exoot", "EQR"), # PAS AAN: de kolommen in mafy_data die je in je pSEM wil steken

  # --- 4. Verplichte criteria uit jouw vraag ---
  grouping_col = "VHAG",
  vhas_col_network = "VHAS",
  vhas_col_base = "vhas",
  vhas_col_match = "vhas"
)
print(paste("Aantal matches:", sum(!is.na(mi_vis_gekoppeld$match_meetplaats))))
mi_vis_gekoppeld
