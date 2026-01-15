library(sf)
library(igraph)
library(tidyverse)
library(progress)
library(here)
library(mapview)

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

# --- Data Inladen ---
# (Laad hier je shapefiles in zoals je gewend bent)
fd <- st_read(here("data", "ruw", "netwerk", "Flow_direction_coordinates.shp"), quiet = T)
nodes <- st_read(here("data", "ruw", "waterlopen", "vha_network_junctions.shp"), quiet = T)
# strahler <- st_read(here("data", "ruw", "waterlopen", "strahler_orde.shp"), quiet = T)

meetnetten <- read.delim("data/ruw/vmm/meetnetten.txt")

mi_data <- st_read(here("data", "ruw", "macroinvertebraten", "mi_meetpunten_datum.gpkg"), quiet = T) %>%
  filter(monsternamedatum > '2009-12-31') %>%
  left_join(meetnetten %>%
              select(vhas, nummer),
            by = c("meetplaats" = "nummer"))

fc_data <- st_read(here("data", "ruw", "fys_chem", "fc_meetpunten.gpkg"), quiet = T) %>%
  filter(monsternamedatum > '2007-12-31') %>%
  left_join(meetnetten %>%
              select(vhas, nummer),
            by = c("meetplaats" = "nummer"))

# --- Bouw het netwerk (slechts 1x nodig!) ---
river_network <- build_river_network(fd, nodes)

# --- matching uit (Fysico-chemie) ---
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


# --- matching uit (Nutriënten - voorbeeld) ---

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
nutrient_data <- st_read(dsn = here("data", "verwerkt", "koppeling", "nutrient_meetpunten_datum.gpkg"), quiet = T) %>%
  left_join(meetnetten %>%
              select(vhas, nummer),
            by = c("meetplaats" = "nummer"))

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
print(paste("Aantal matches:", sum(!is.na(mi_met_nutrient$qual_meetplaats))))

save(mi_met_nutrient, file = "data/verwerkt/koppeling/mi_met_nutrient_matched.rdata")
mi_met_nutrient %>% drop_na(qual_meetplaats) %>% nrow

# ---  Voer de matching uit (Nutriënten - maar met overstortenlaag bij) ---
overstorten_uitlaat_vha <- st_read(here("data", "ruw", "overstorten", "P_OS_uitlaat_VHA.shp"))
meetpunten_lozingen <- st_read(here("data", "ruw", "afvalwater", "Lozmtput.shp"))

mi_met_fc_overstorten <- match_upstream(
  biota_sf = mi_data,
  quality_sf = fc_data,
  network_list = river_network,
  discharge_sf = overstorten_uitlaat_vha,
  max_dist_m = 5000,       # Max 5km stroomopwaarts
  days_before = 180,       # Kwaliteit mag tot 180 dagen VOOR de biota meting zijn
  days_after = 14,         # Kwaliteit mag tot 14 dagen NA de biota meting zijn
  col_date_biota = "monsternamedatum",
  col_date_quality = "monsternamedatum",
  selection_mode = "closest_distance"
)
print(paste("Aantal matches:", sum(!is.na(mi_met_fc_overstorten$qual_meetplaats))))

# --- STAP 4: Voer de matching uit (pesticiden TU) ---
if (!file.exists(here("data", "verwerkt", "koppeling", "pesticide_meetpunten_datum.gpkg"))) {
  load(here("data", "verwerkt", "tu_resultaten.rdata"))
  fc_data <- st_read(here("data", "ruw", "fys_chem", "fc_meetpunten.gpkg"), quiet = T) %>%
    filter(monsternamedatum > '2007-12-31')
  pesticide_meetpunten_datum <- tu_per_sample %>%
    select(meetplaats, monsternamedatum) %>%
    left_join(fc_data,
              by = c("meetplaats", "monsternamedatum"))
  st_write(pesticide_meetpunten_datum, dsn = here("data", "verwerkt", "koppeling", "pesticide_meetpunten_datum.gpkg"))
}

pesticide_data <- st_read(here("data", "verwerkt", "koppeling", "pesticide_meetpunten_datum.gpkg"), quiet = T) %>%
  left_join(meetnetten %>%
              select(vhas, nummer),
            by = c("meetplaats" = "nummer"))

mi_met_pesticide <- match_upstream(
  biota_sf = mi_data,
  quality_sf = pesticide_data,
  network_list = river_network,
  discharge_sf = NULL,
  max_dist_m = 5000,       # Max 5km stroomopwaarts
  days_before = 1095,       # Kwaliteit mag tot 180 dagen VOOR de biota meting zijn
  days_after = 730,         # Kwaliteit mag tot 14 dagen NA de biota meting zijn
  col_date_biota = "monsternamedatum",
  col_date_quality = "monsternamedatum",
  selection_mode = "closest_distance"
)
print(paste("Aantal matches:", sum(!is.na(mi_met_pesticide$qual_meetplaats))))

load("data/verwerkt/mi_nat_sv.rdata")
test <- mi_nat_sv %>%
  left_join(mi_met_pesticide)
test %>% drop_na(qual_meetplaats) %>% nrow

# ==============================================================================
# Functie met meer opties: strahler, grouping, selection mode = "aggregate"
# ==============================================================================

# strahler mag maar 1 verschillen, grouping_col -> puntn moeten tot zelfde groep behoren ("VHAG" of "WTRLICH" (owl)); selection_mode = "aggregate" -> om een gemiddelde of min of max (aggr_method) van alle matches te nemen voor een variabele (aggr_cols)

strahler <- st_read("data/ruw/waterlopen/strahler_orde.shp") %>%
  st_transform(., crs = st_crs(fd))

match_upstream_strahler <- function(biota_sf,
                                     quality_sf,
                                     network_list,
                                     discharge_sf = NULL,
                                     strahler_sf = NULL,
                                     use_strahler = FALSE,
                                     strahler_col = "ORDE",
                                     max_dist_m = 5000,
                                     max_downstream_m = 200,   # NIEUW: Max afstand vogelvlucht (ook downstream)
                                     snap_tolerance = 25,
                                     days_before = 14,
                                     days_after = 30,
                                     col_date_biota = "monsternamedatum",
                                     col_date_quality = "monsternamedatum",
                                     selection_mode = "closest_distance",
                                     aggr_method = "mean",
                                     aggr_cols = NULL,
                                     grouping_col = NULL,
                                     vhas_col_network = "VHAS",
                                     vhas_col_biota = NULL,
                                     vhas_col_quality = NULL) {

  # --- 1. Validatie ---
  valid_modes <- c("closest_distance", "closest_time", "all", "aggregate")
  if (!selection_mode %in% valid_modes) stop("selection_mode moet zijn: 'closest_distance', 'closest_time', 'all' of 'aggregate'")

  valid_aggr <- c("mean", "min", "max")
  if (!aggr_method %in% valid_aggr) stop("aggr_method moet zijn: 'mean', 'min' of 'max'")

  if (!is.null(aggr_cols)) {
    missing_cols <- setdiff(aggr_cols, names(quality_sf))
    if (length(missing_cols) > 0) stop(paste("aggr_cols niet gevonden in quality_sf:", paste(missing_cols, collapse=", ")))
  }

  if (use_strahler) {
    if (is.null(strahler_sf)) stop("Je hebt 'use_strahler = TRUE' gezet, maar geen 'strahler_sf' opgegeven.")
    if (!strahler_col %in% names(strahler_sf)) stop(paste("Kolom", strahler_col, "niet gevonden in strahler_sf."))
    message(paste0("Strahler filter geactiveerd (max 1 orde verschil, max snap afstand: ", snap_tolerance, "m)."))
  }

  g <- network_list$graph
  net_sf <- network_list$network_sf

  if (!is.null(grouping_col)) {
    if (!grouping_col %in% names(net_sf)) stop(paste("De grouping_col", grouping_col, "bestaat niet in je netwerk!"))
    message(paste0("Grouping geactiveerd: enkel matchen binnen zelfde '", grouping_col, "'."))
  }

  # Datum conversies
  biota_sf[[col_date_biota]] <- as.Date(biota_sf[[col_date_biota]])
  quality_sf[[col_date_quality]] <- as.Date(quality_sf[[col_date_quality]])
  biota_sf$tmp_join_id <- seq_len(nrow(biota_sf))

  # We voegen een index toe aan quality_sf om later geometries terug te vinden
  quality_sf$orig_index_geo <- seq_len(nrow(quality_sf))

  message("1/5 Snapping points to river network...")

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

  biota_sf <- snap_and_attribute(biota_sf, net_sf, grouping_col, snap_tolerance, vhas_col_biota, vhas_col_network)
  quality_sf <- snap_and_attribute(quality_sf, net_sf, grouping_col, snap_tolerance, vhas_col_quality, vhas_col_network)

  # --- Strahler Snapping ---
  if (use_strahler) {
    message("    -> Determining Strahler order...")
    get_strahler_order <- function(pts, str_layer, str_col, tol) {
      idx <- st_nearest_feature(pts, str_layer)
      dists <- st_distance(pts, str_layer[idx, ], by_element = TRUE)
      dists_num <- as.numeric(dists)
      dists_num[is.na(dists_num)] <- Inf
      vals <- rep(NA, nrow(pts))
      valid <- dists_num <= tol
      vals[valid] <- str_layer[[str_col]][idx[valid]]
      return(vals)
    }
    biota_sf$strahler_val <- as.numeric(get_strahler_order(biota_sf, strahler_sf, strahler_col, snap_tolerance))
    quality_sf$strahler_val <- as.numeric(get_strahler_order(quality_sf, strahler_sf, strahler_col, snap_tolerance))
  }

  quality_df <- st_drop_geometry(quality_sf)

  # --- Discharge Snapping ---
  discharge_nodes <- character(0)
  if (!is.null(discharge_sf)) {
    message("    -> Snapping discharge points...")
    idx_dis <- st_nearest_feature(discharge_sf, net_sf)
    dists_dis <- st_distance(discharge_sf, net_sf[idx_dis, ], by_element = TRUE)
    dists_num <- as.numeric(dists_dis)
    dists_num[is.na(dists_num)] <- Inf
    valid_dis <- dists_num <= snap_tolerance
    if (sum(valid_dis) > 0) {
      discharge_nodes <- unique(as.character(net_sf$from_node[idx_dis[valid_dis]]))
    }
  }

  message(paste0("2/5 Matching points (Mode: ", selection_mode, " | Upstream: ", max_dist_m, "m OR Local: ", max_downstream_m, "m)..."))

  results <- list()
  pb <- progress_bar$new(format = "  [:bar] :percent eta: :eta", total = nrow(biota_sf), clear = FALSE, width = 60)

  for (i in seq_len(nrow(biota_sf))) {
    pb$tick()
    b_pt <- biota_sf[i, ]
    b_node <- b_pt$node_id
    b_id   <- b_pt$tmp_join_id
    match <- NA
    n_matches_found <- 0

    if (is.na(b_node)) {
      match <- quality_df[1, ]
      match[] <- NA; match$river_dist_m <- NA
    } else {
      b_date <- b_pt[[col_date_biota]]

      # STAP 1: Pas eerst alle niet-ruimtelijke filters toe (Datum, Groep, Strahler)
      subset_quality <- quality_df %>% filter(!is.na(node_id))

      if (!is.null(grouping_col)) {
        b_group_val <- b_pt$match_group_val
        if (!is.na(b_group_val)) {
          subset_quality <- subset_quality %>% filter(match_group_val == b_group_val)
        } else {
          subset_quality <- subset_quality[0, ]
        }
      }

      if (use_strahler && nrow(subset_quality) > 0) {
        b_strahler <- b_pt$strahler_val
        if (!is.na(b_strahler)) {
          subset_quality <- subset_quality %>% filter(!is.na(strahler_val), abs(strahler_val - b_strahler) <= 1)
        } else {
          subset_quality <- subset_quality[0, ]
        }
      }

      # Filter op tijd
      if (nrow(subset_quality) > 0) {
        subset_quality <- subset_quality %>%
          mutate(
            temp_diff_days = as.numeric(difftime(b_date, .[[col_date_quality]], units = "days")),
            abs_time_diff  = abs(temp_diff_days)
          ) %>%
          filter(temp_diff_days >= -days_after & temp_diff_days <= days_before)
      }

      if (nrow(subset_quality) > 0) {

        # STAP 2: Bereken afstanden (Graaf EN Euclidisch)

        # A. Graaf Afstand (alleen upstream werkt hier, downstream geeft Inf)
        upstream_nodes <- names(subcomponent(g, v = b_node, mode = "in"))
        subset_quality$is_upstream <- subset_quality$node_id %in% upstream_nodes

        # Bereken graafafstand (alleen zinvol voor upstream, maar we runnen voor subset)
        d_matrix <- distances(g, v = subset_quality$node_id, to = b_node, mode = "out")
        subset_quality$graph_dist <- as.numeric(d_matrix)

        # B. Euclidische Afstand (Vogelvlucht)
        # We moeten de geometry ophalen uit de originele quality_sf
        # Dit doen we via de index die we eerder hebben opgeslagen
        target_geoms <- quality_sf[subset_quality$orig_index_geo, ]
        euclid_dists <- st_distance(b_pt, target_geoms)
        subset_quality$euclid_dist <- as.numeric(euclid_dists)

        # STAP 3: De 'OR' Filter
        # Kandidaat is geldig als:
        # (Is Upstream EN GraphDist <= 5000) OF (EuclidDist <= 200)

        candidates <- subset_quality %>%
          filter(
            (is_upstream & graph_dist <= max_dist_m) |
            (euclid_dist <= max_downstream_m)
          )

        # STAP 4: Bepaal de 'definitive' afstand voor sortering
        # Als het upstream is -> Graph dist
        # Als het downstream is (graph=Inf) -> Euclid dist
        # Als het beide is (kortbij upstream) -> Graph dist (consistentie)
        if (nrow(candidates) > 0) {
           candidates$river_dist_m <- ifelse(is.finite(candidates$graph_dist),
                                             candidates$graph_dist,
                                             candidates$euclid_dist)

           # Topology check (Lozingen) - enkel relevant voor upstream paden!
           # Als een punt downstream ligt, heeft een lozing stroomopwaarts (tussen biota en qual) geen invloed op qual.
           if (length(discharge_nodes) > 0) {
             # We checken alleen de kandidaten die 'upstream' zijn volgens de graaf
             to_check_idx <- which(candidates$is_upstream & candidates$river_dist_m > 0)

             if(length(to_check_idx) > 0) {
               keep_flags <- rep(TRUE, nrow(candidates))

               checks <- sapply(candidates$node_id[to_check_idx], function(cand_node) {
                 if (cand_node == b_node) return(TRUE)
                 path_res <- shortest_paths(g, from = cand_node, to = b_node, mode = "out")
                 intermediate_nodes <- setdiff(names(path_res$vpath[[1]]), c(cand_node, b_node))
                 if (any(intermediate_nodes %in% discharge_nodes)) return(FALSE) else return(TRUE)
               })

               keep_flags[to_check_idx] <- checks
               candidates <- candidates[keep_flags, ]
             }
           }

           candidates_within <- candidates # Naamgeving consistent houden

           if (nrow(candidates_within) > 0) {
             # ... (Selectie logica blijft hetzelfde) ...
             if (selection_mode == "closest_distance") {
               match <- candidates_within[which.min(candidates_within$river_dist_m), ]; n_matches_found <- 1
             } else if (selection_mode == "closest_time") {
               match <- candidates_within[which.min(candidates_within$abs_time_diff), ]; n_matches_found <- 1
             } else if (selection_mode == "all") {
               match <- candidates_within; n_matches_found <- nrow(candidates_within)
             } else if (selection_mode == "aggregate") {
               n_matches_found <- nrow(candidates_within)
               if (!is.null(aggr_cols)) { cols_to_agg <- aggr_cols } else {
                 tech_cols <- c("node_id", "river_dist_m", "temp_diff_days", "abs_time_diff", "strahler_val", "match_group_val", "orig_index_geo", "graph_dist", "euclid_dist", "is_upstream")
                 num_cols <- names(select_if(candidates_within, is.numeric))
                 cols_to_agg <- setdiff(num_cols, tech_cols)
               }
               agg_fun <- switch(aggr_method, "mean"=function(x) mean(x, na.rm=T), "min"=function(x) min(x, na.rm=T), "max"=function(x) max(x, na.rm=T))

               if (length(cols_to_agg) > 0) {
                   agg_values <- candidates_within %>% summarise(across(all_of(cols_to_agg), agg_fun))
                   match <- candidates_within[1, ]
                   match[, cols_to_agg] <- agg_values
               } else { match <- candidates_within[1, ] }
               match$river_dist_m <- mean(candidates_within$river_dist_m)
             }
           }
        }
      }
      if (!is.data.frame(match)) { match <- quality_df[1, ]; match[] <- NA; match$river_dist_m <- NA }
    }
    match$biota_match_id <- b_id
    match$n_matches <- n_matches_found
    # Cleanup extra kolommen
    cols_to_remove <- c("temp_diff_days", "abs_time_diff", "match_group_val", "strahler_val", "orig_index_geo", "graph_dist", "euclid_dist", "is_upstream")
    match[, base::intersect(names(match), cols_to_remove)] <- NULL
    results[[i]] <- match
  }

  message("3/5 Merging results...")
  matched_quality_df <- do.call(rbind, results)
  cols_to_rename <- setdiff(names(matched_quality_df), "biota_match_id")
  names(matched_quality_df)[names(matched_quality_df) %in% cols_to_rename] <- paste0("qual_", cols_to_rename)
  final_sf <- left_join(biota_sf, matched_quality_df, by = c("tmp_join_id" = "biota_match_id"))

  final_sf$tmp_join_id <- NULL
  if("match_group_val" %in% names(final_sf)) final_sf$match_group_val <- NULL
  if("strahler_val" %in% names(final_sf)) final_sf$strahler_val <- NULL

  return(final_sf)
}

nutrient_data <- match_upstream_strahler(
  biota_sf = mi_data,
  quality_sf = nutrient_data,
  network_list = river_network,
  strahler_sf = strahler,
  use_strahler = FALSE,
  strahler_col = "orde",
  max_dist_m = 5000,       # Max 5km stroomopwaarts
  days_before = 180,       # Kwaliteit mag tot 180 dagen VOOR de biota meting zijn
  days_after = 14,         # Kwaliteit mag tot 14 dagen NA de biota meting zijn
  col_date_biota = "monsternamedatum",
  col_date_quality = "monsternamedatum",
  selection_mode = "closest_distance",
  grouping_col = "VHAG",
  vhas_col_network = "VHAS",
  vhas_col_biota = "vhas",
  vhas_col_quality = "vhas"
)
print(paste("Aantal matches:", sum(!is.na(nutrient_data$qual_meetplaats))))


library(ggplot2)
library(dplyr)
library(tidyr)
library(progress)

# ==============================================================================
# OPTIMALE TIJD EN AFSTANDSVENSTER???
# ==============================================================================

library(dplyr)
library(ggplot2)
library(tidyr)
library(sf)

message("--- Start Diagnose & Analyse ---")

# 1. Definieer scenarios
time_steps <- c(90, 365, 730, 1095)
dist_steps <- c(100, 250, 500, 1000, 2500, 5000)

# 2. Master Match uitvoeren (zoals eerder)
# We zorgen dat we zeker 'mode = all' hebben
message("1. Master Match uitvoeren...")
master_match <- match_upstream(
  biota_sf = mi_data %>% mutate(analyse_id = row_number()), # Zeker zijn van ID
  quality_sf = pesticide_data,
  network_list = river_network,
  max_dist_m = 5000,
  days_before = 1095,
  days_after = 365,
  col_date_biota = "monsternamedatum",
  col_date_quality = "monsternamedatum",
  selection_mode = "all"
)

# 3. Data Check & Correctie (BELANGRIJK!)
master_df <- st_drop_geometry(master_match) %>%
  filter(!is.na(qual_meetplaats)) %>%
  mutate(
    # Zeker zijn dat het datums zijn
    date_bio = as.Date(monsternamedatum),
    date_qual = as.Date(qual_monsternamedatum),

    # Bereken verschil in dagen (numeriek maken is cruciaal)
    diff_days = as.numeric(date_bio - date_qual),

    # Zeker zijn dat afstand numeriek is
    qual_river_dist_m = as.numeric(qual_river_dist_m)
  )

# --- DIAGNOSE START ---
message("\n--- DATA CHECK ---")
message("Samenvatting Afstanden (meters):")
print(summary(master_df$qual_river_dist_m))

message("\nSamenvatting Tijdsverschillen (dagen, positief = qual eerder):")
print(summary(master_df$diff_days))

message("\nAantal rijen in master_df (totaal aantal mogelijke koppelingen):")
print(nrow(master_df))
# --- DIAGNOSE EINDE ---

# 4. De Lus
results_df <- data.frame()
total_mi_points <- nrow(mi_data)

message("\n2. Berekenen scenario's...")

for (t_window in time_steps) {
  for (d_window in dist_steps) {

    # Filter de data
    # We gebruiken expliciet de nieuwe kolommen
    matches_in_scenario <- master_df %>%
      filter(qual_river_dist_m <= d_window) %>%
      filter(diff_days <= t_window & diff_days >= -14)

    # Tel unieke biota punten
    n_matches <- n_distinct(matches_in_scenario$analyse_id)

    results_df <- rbind(results_df, data.frame(
      Dagen_Terug = t_window,
      Afstand_Max = d_window,
      Aantal_Matches = n_matches,
      Percentage = (n_matches / total_mi_points) * 100
    ))
  }
}

# Print de tabel om te zien of de getallen echt verschillen
message("\n--- RESULTATEN PREVIEW ---")
print(head(results_df, 10))
print(tail(results_df, 10))


# ==============================================================================
# PLOT
# ==============================================================================

# Maak een factor met de juiste volgorde voor de legenda
results_df$Label_Tijd <- factor(paste(results_df$Dagen_Terug, "dagen"),
                                levels = paste(time_steps, "dagen"))

p <- ggplot(results_df, aes(x = Afstand_Max, y = Aantal_Matches, color = Label_Tijd, group = Label_Tijd)) +
  # Gebruik 'position_dodge' zodat overlappende lijnen iets verschuiven
  # en alpha zodat je door de lijnen heen kijkt
  geom_line(linewidth = 1, alpha = 0.7, position = position_dodge(width = 0.05)) +
  geom_point(size = 3, position = position_dodge(width = 0.05)) +

  # Logaritmische schaal kan helpen als 100-500 heel dicht op elkaar zit tov 5000
  # We gebruiken hier een gewone schaal met vaste breaks
  scale_x_continuous(breaks = dist_steps, guide = guide_axis(n.dodge = 2)) +

  # Y-as begint bij 0
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +

  scale_color_viridis_d(option = "D", end = 0.9) +
  theme_minimal() +
  labs(
    title = "Sensitiviteitsanalyse Matches",
    subtitle = "Als lijnen samenvallen, geeft extra tijd geen extra matches.",
    x = "Afstand (m)",
    y = "Aantal Unieke Matches",
    color = "Tijdsvenster"
  ) +
  theme(legend.position = "bottom")

print(p)

