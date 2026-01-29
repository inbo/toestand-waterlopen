
strahler <- st_read("data/ruw/waterlopen/strahler_orde.shp")

match_upstream_group <- function(biota_sf,
                           quality_sf,
                           network_list,
                           discharge_sf = NULL,
                           max_dist_m = 5000,
                           days_before = 14,
                           days_after = 30,
                           col_date_biota = "monsternamedatum",
                           col_date_quality = "monsternamedatum",
                           selection_mode = "closest_distance",
                           grouping_col = NULL) { # NIEUW: Kolomnaam voor harde match (bv. "VHAG")

  # 1. Validatie
  valid_modes <- c("closest_distance", "closest_time", "all")
  if (!selection_mode %in% valid_modes) stop("selection_mode moet zijn: 'closest_distance', 'closest_time' of 'all'")

  g <- network_list$graph
  net_sf <- network_list$network_sf

  # Check of grouping column in het netwerk bestand zit
  if (!is.null(grouping_col)) {
    if (!grouping_col %in% names(net_sf)) {
      stop(paste("De grouping_col", grouping_col, "bestaat niet in je netwerk (fd) object!"))
    }
    message(paste0("Grouping geactiveerd: enkel matchen binnen zelfde '", grouping_col, "'."))
  }

  # Datum conversies
  biota_sf[[col_date_biota]] <- as.Date(biota_sf[[col_date_biota]])
  quality_sf[[col_date_quality]] <- as.Date(quality_sf[[col_date_quality]])
  biota_sf$tmp_join_id <- seq_len(nrow(biota_sf))

  message("1/4 Snapping points to river network & retrieving attributes...")

  # --- OPTIMALISATIE SNAP FUNCTIE ---
  # We doen de zware ruimtelijke berekening (st_nearest_feature) nu 1 keer
  # en halen daaruit zowel de Node ID als de Grouping Variabele.

  snap_and_attribute <- function(points, network, grp_col) {
    # 1. Vind index van dichtstbijzijnde lijnsegment
    nearest_idx <- st_nearest_feature(points, network)

    # 2. Haal Node ID op (zoals voorheen)
    points$node_id <- as.character(network$from_node[nearest_idx])

    # 3. NIEUW: Haal grouping variabele op uit de lijn en zet op het punt
    if (!is.null(grp_col)) {
      points$match_group_val <- network[[grp_col]][nearest_idx]
    }
    return(points)
  }

  biota_sf <- snap_and_attribute(biota_sf, net_sf, grouping_col)
  quality_sf <- snap_and_attribute(quality_sf, net_sf, grouping_col)

  # Drop geometry voor snelheid (nu zitten de attributes er al in)
  quality_df <- st_drop_geometry(quality_sf)

  # --- Verwerk lozingspunten ---
  discharge_nodes <- character(0)
  if (!is.null(discharge_sf)) {
    message("    -> Snapping discharge points...")
    # Lozingspunten hebben geen grouping nodig voor het matchen zelf, enkel node_id
    idx_dis <- st_nearest_feature(discharge_sf, net_sf)
    discharge_sf$node_id <- as.character(net_sf$from_node[idx_dis])
    discharge_nodes <- unique(as.character(discharge_sf$node_id))
  }

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

    # --- STAP 0: Eerste harde filtering op Grouping (VHAG/Waterlichaam) ---
    # Dit maakt het script veel sneller omdat we kandidaten direct weggooien
    subset_quality <- quality_df

    if (!is.null(grouping_col)) {
      b_group_val <- b_pt$match_group_val

      # Filter: behoud alleen rijen met dezelfde groepswaarde (en skip NA's)
      if (!is.na(b_group_val)) {
        subset_quality <- subset_quality %>%
          filter(match_group_val == b_group_val)
      } else {
        # Als biota punt geen groep heeft (buiten bereik?), kan hij niet matchen op groep
        subset_quality <- subset_quality[0, ]
      }
    }

    match <- NA

    # Als er na de groeps-filter nog kandidaten over zijn, ga door met de zware logica
    if (nrow(subset_quality) > 0) {

      # Vind upstream knopen
      upstream_nodes <- names(subcomponent(g, v = b_node, mode = "in"))

      # Filter kandidaten (upstream + tijd)
      candidates <- subset_quality %>%
        filter(node_id %in% upstream_nodes) %>%
        mutate(
          temp_diff_days = as.numeric(difftime(b_date, .[[col_date_quality]], units = "days")),
          abs_time_diff  = abs(temp_diff_days)
        ) %>%
        filter(temp_diff_days >= -days_after & temp_diff_days <= days_before)

      if (nrow(candidates) > 0) {
        # Afstand berekenen (alleen voor overgebleven kandidaten)
        d_matrix <- distances(g, v = candidates$node_id, to = b_node, mode = "out")
        candidates$river_dist_m <- as.numeric(d_matrix)

        candidates_within <- candidates %>% filter(river_dist_m <= max_dist_m)

        # --- Filter op lozingspunten (Topology Check) ---
        if (nrow(candidates_within) > 0 && length(discharge_nodes) > 0) {
          keep_candidate <- sapply(candidates_within$node_id, function(cand_node) {
            if (cand_node == b_node) return(TRUE)
            path_res <- shortest_paths(g, from = cand_node, to = b_node, mode = "out")
            intermediate_nodes <- setdiff(names(path_res$vpath[[1]]), c(cand_node, b_node))
            if (any(intermediate_nodes %in% discharge_nodes)) return(FALSE) else return(TRUE)
          })
          candidates_within <- candidates_within[keep_candidate, ]
        }

        # Selecteer beste match
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
    }

    # Lege rij indien geen match
    if (!is.data.frame(match)) {
      match <- quality_df[1, ]
      match[] <- NA
      match$river_dist_m <- NA
    }

    match$biota_match_id <- b_id

    # Opschonen hulpkolommen
    cols_to_remove <- c("temp_diff_days", "abs_time_diff", "match_group_val")
    match[cols_to_remove[cols_to_remove %in% names(match)]] <- NULL

    results[[i]] <- match
  }

  message("3/4 Merging results...")
  matched_quality_df <- do.call(rbind, results)

  cols_to_rename <- setdiff(names(matched_quality_df), "biota_match_id")
  names(matched_quality_df)[names(matched_quality_df) %in% cols_to_rename] <-
    paste0("qual_", cols_to_rename)

  final_sf <- left_join(biota_sf, matched_quality_df, by = c("tmp_join_id" = "biota_match_id"))

  # Ruim de tijdelijke kolommen in final_sf op
  final_sf$tmp_join_id <- NULL
  if("match_group_val" %in% names(final_sf)) final_sf$match_group_val <- NULL

  return(final_sf)
}

res_nogroup <- match_upstream_group(
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

print(paste("Aantal matches:", sum(!is.na(res_nogroup$qual_meetplaats))))

res_withgroup <- match_upstream_group(
  biota_sf = mi_data,
  quality_sf = fc_data,
  network_list = river_network,
  max_dist_m = 5000,       # Max 5km stroomopwaarts
  days_before = 180,       # Kwaliteit mag tot 180 dagen VOOR de biota meting zijn
  days_after = 14,         # Kwaliteit mag tot 14 dagen NA de biota meting zijn
  col_date_biota = "monsternamedatum",
  col_date_quality = "monsternamedatum",
  selection_mode = "closest_distance",
  grouping_col = "VHAG"
)
print(paste("Aantal matches:", sum(!is.na(res_withgroup$qual_meetplaats))))

res_withgroup_OWL <- match_upstream_group(
  biota_sf = mi_data,
  quality_sf = fc_data,
  network_list = river_network,
  max_dist_m = 5000,       # Max 5km stroomopwaarts
  days_before = 180,       # Kwaliteit mag tot 180 dagen VOOR de biota meting zijn
  days_after = 14,         # Kwaliteit mag tot 14 dagen NA de biota meting zijn
  col_date_biota = "monsternamedatum",
  col_date_quality = "monsternamedatum",
  selection_mode = "closest_distance",
  grouping_col = "WTRLICHC"
)
print(paste("Aantal matches:", sum(!is.na(res_withgroup_OWL$qual_meetplaats))))


# 1. Vind de vhag codes met meer dan 1 uniek waterlichaam
dubbele_vhags <- fd %>%
  st_drop_geometry() %>%
  group_by(VHAG) %>%
  summarise(aantal_vl = n_distinct(WTRLICHC)) %>%
  filter(aantal_vl > 1)

# Toon de vhag codes die fout zitten
print(dubbele_vhags)

test <- fd %>%
  st_drop_geometry() %>%
  group_by(VHAG) %>%
  summarise(sum_lengte = sum(LENGTE)) %>%
  mutate(mean(sum_lengte))

test2 <- fd %>%
  st_drop_geometry() %>%
  group_by(WTRLICHC) %>%
  summarise(sum_lengte = sum(LENGTE)) %>%
  mutate(mean(sum_lengte))

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
                                 snap_tolerance = 100,
                                 days_before = 14,
                                 days_after = 30,
                                 col_date_biota = "monsternamedatum",
                                 col_date_quality = "monsternamedatum",
                                 selection_mode = "closest_distance",
                                 grouping_col = NULL) {

  # --- 1. Validatie ---
  valid_modes <- c("closest_distance", "closest_time", "all")
  if (!selection_mode %in% valid_modes) stop("selection_mode moet zijn: 'closest_distance', 'closest_time' of 'all'")

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

  message("1/5 Snapping points to river network (with tolerance check)...")

  # --- Slimme Snap Functie ---
  snap_and_attribute <- function(points, network, grp_col, tol) {
    nearest_idx <- st_nearest_feature(points, network)
    dists <- st_distance(points, network[nearest_idx, ], by_element = TRUE)
    dists_num <- as.numeric(dists)

    valid_snaps <- dists_num <= tol

    n_dropped <- sum(!valid_snaps)
    if (n_dropped > 0) message(paste("   ->", n_dropped, "punten liggen verder dan", tol, "m van het netwerk en worden genegeerd."))

    points$node_id <- NA
    points$node_id[valid_snaps] <- as.character(network$from_node[nearest_idx[valid_snaps]])

    if (!is.null(grp_col)) {
      points$match_group_val <- NA
      points$match_group_val[valid_snaps] <- network[[grp_col]][nearest_idx[valid_snaps]]
    }
    return(points)
  }

  biota_sf <- snap_and_attribute(biota_sf, net_sf, grouping_col, snap_tolerance)
  quality_sf <- snap_and_attribute(quality_sf, net_sf, grouping_col, snap_tolerance)

  # --- Strahler Snapping ---
  if (use_strahler) {
    message("    -> Determining Strahler order (with tolerance check)...")
    get_strahler_order <- function(pts, str_layer, str_col, tol) {
      idx <- st_nearest_feature(pts, str_layer)
      dists <- st_distance(pts, str_layer[idx, ], by_element = TRUE)
      vals <- rep(NA, nrow(pts))
      valid <- as.numeric(dists) <= tol
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
    valid_dis <- as.numeric(dists_dis) <= snap_tolerance
    if (sum(valid_dis) > 0) {
      discharge_nodes <- unique(as.character(net_sf$from_node[idx_dis[valid_dis]]))
    }
  }

  message("2/5 Matching upstream points...")

  results <- list()

  pb <- progress_bar$new(
    format = "  [:bar] :percent eta: :eta",
    total = nrow(biota_sf), clear = FALSE, width = 60
  )

  for (i in seq_len(nrow(biota_sf))) {
    pb$tick()

    b_pt <- biota_sf[i, ]
    b_node <- b_pt$node_id
    b_id   <- b_pt$tmp_join_id

    match <- NA

    # 1. SCENARIO: PUNT LIGT TE VER VAN NETWERK (Niet gesnapt)
    if (is.na(b_node)) {
      match <- quality_df[1, ]
      match[] <- NA
      match$river_dist_m <- NA
      # We gaan door naar de cleanup aan het einde
    } else {
      # 2. SCENARIO: PUNT IS GESNAPT -> ZOEKEN MAAR
      b_date <- b_pt[[col_date_biota]]

      # Begin met subset (filter niet-gesnapte qual punten weg)
      subset_quality <- quality_df %>% filter(!is.na(node_id))

      # Filter Grouping
      if (!is.null(grouping_col)) {
        b_group_val <- b_pt$match_group_val
        if (!is.na(b_group_val)) {
          subset_quality <- subset_quality %>% filter(match_group_val == b_group_val)
        } else {
          subset_quality <- subset_quality[0, ]
        }
      }

      # Filter Strahler
      if (use_strahler && nrow(subset_quality) > 0) {
        b_strahler <- b_pt$strahler_val
        if (!is.na(b_strahler)) {
          subset_quality <- subset_quality %>%
            filter(!is.na(strahler_val)) %>%
            filter(abs(strahler_val - b_strahler) <= 1)
        } else {
          subset_quality <- subset_quality[0, ]
        }
      }

      if (nrow(subset_quality) > 0) {
        upstream_nodes <- names(subcomponent(g, v = b_node, mode = "in"))

        candidates <- subset_quality %>%
          filter(node_id %in% upstream_nodes) %>%
          mutate(
            temp_diff_days = as.numeric(difftime(b_date, .[[col_date_quality]], units = "days")),
            abs_time_diff  = abs(temp_diff_days)
          ) %>%
          filter(temp_diff_days >= -days_after & temp_diff_days <= days_before)

        if (nrow(candidates) > 0) {
          d_matrix <- distances(g, v = candidates$node_id, to = b_node, mode = "out")
          candidates$river_dist_m <- as.numeric(d_matrix)

          candidates_within <- candidates %>% filter(river_dist_m <= max_dist_m)

          # Topology Check
          if (nrow(candidates_within) > 0 && length(discharge_nodes) > 0) {
            keep_candidate <- sapply(candidates_within$node_id, function(cand_node) {
              if (cand_node == b_node) return(TRUE)
              path_res <- shortest_paths(g, from = cand_node, to = b_node, mode = "out")
              intermediate_nodes <- setdiff(names(path_res$vpath[[1]]), c(cand_node, b_node))
              if (any(intermediate_nodes %in% discharge_nodes)) return(FALSE) else return(TRUE)
            })
            candidates_within <- candidates_within[keep_candidate, ]
          }

          # Selectie
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
      }

      # Lege rij als er na filteren niks overblijft
      if (!is.data.frame(match)) {
        match <- quality_df[1, ]
        match[] <- NA
        match$river_dist_m <- NA
      }
    }

    # 3. CENTRALE CLEANUP EN ID TOEWIJZING
    # Dit gebeurt nu voor ALLE scenario's (snap failure, no match, success match)
    match$biota_match_id <- b_id

    # We definiëren de lijst kolommen die we ALTIJD weg willen hebben
    cols_to_remove <- c("temp_diff_days", "abs_time_diff", "match_group_val", "strahler_val")

    # Verwijder ze veilig (alleen als ze bestaan)
    cols_present <- intersect(names(match), cols_to_remove)
    if(length(cols_present) > 0) {
      match[, cols_present] <- NULL
    }

    results[[i]] <- match
  }

  message("3/5 Merging results...")
  matched_quality_df <- do.call(rbind, results)

  cols_to_rename <- setdiff(names(matched_quality_df), "biota_match_id")
  names(matched_quality_df)[names(matched_quality_df) %in% cols_to_rename] <-
    paste0("qual_", cols_to_rename)

  final_sf <- left_join(biota_sf, matched_quality_df, by = c("tmp_join_id" = "biota_match_id"))

  # Laatste opruiming van de biota object zelf
  final_sf$tmp_join_id <- NULL
  if("match_group_val" %in% names(final_sf)) final_sf$match_group_val <- NULL
  if("strahler_val" %in% names(final_sf)) final_sf$strahler_val <- NULL

  return(final_sf)
}




res_nogroup_strahler <- match_upstream_strahler(
  biota_sf = mi_data,
  quality_sf = pesticide_data,
  network_list = river_network,
  max_dist_m = 5000,       # Max 5km stroomopwaarts
  days_before = 180,       # Kwaliteit mag tot 180 dagen VOOR de biota meting zijn
  days_after = 14,         # Kwaliteit mag tot 14 dagen NA de biota meting zijn
  col_date_biota = "monsternamedatum",
  col_date_quality = "monsternamedatum",
  selection_mode = "closest_distance"
)

print(paste("Aantal matches:", sum(!is.na(res_nogroup_strahler$qual_meetplaats))))

res_group <- match_upstream_strahler(
  biota_sf = mi_data,
  quality_sf = pesticide_data,
  network_list = river_network,
  max_dist_m = 5000,       # Max 5km stroomopwaarts
  days_before = 180,       # Kwaliteit mag tot 180 dagen VOOR de biota meting zijn
  days_after = 14,         # Kwaliteit mag tot 14 dagen NA de biota meting zijn
  col_date_biota = "monsternamedatum",
  col_date_quality = "monsternamedatum",
  selection_mode = "closest_distance",
  grouping_col = "VHAG"
)
print(paste("Aantal matches:", sum(!is.na(res_group$qual_meetplaats))))

res_strahler <- match_upstream_strahler(
  biota_sf = mi_data,
  quality_sf = pesticide_data,
  network_list = river_network,
  strahler_sf = strahler,
  use_strahler = TRUE,
  strahler_col = "orde",
  max_dist_m = 5000,       # Max 5km stroomopwaarts
  days_before = 180,       # Kwaliteit mag tot 180 dagen VOOR de biota meting zijn
  days_after = 14,         # Kwaliteit mag tot 14 dagen NA de biota meting zijn
  col_date_biota = "monsternamedatum",
  col_date_quality = "monsternamedatum",
  selection_mode = "closest_distance"
)
print(paste("Aantal matches:", sum(!is.na(res_strahler$qual_meetplaats))))

res_strahler_vhag <- match_upstream_strahler(
  biota_sf = mi_data,
  quality_sf = pesticide_data,
  network_list = river_network,
  strahler_sf = strahler,
  use_strahler = TRUE,
  strahler_col = "orde",
  max_dist_m = 5000,       # Max 5km stroomopwaarts
  days_before = 180,       # Kwaliteit mag tot 180 dagen VOOR de biota meting zijn
  days_after = 14,         # Kwaliteit mag tot 14 dagen NA de biota meting zijn
  col_date_biota = "monsternamedatum",
  col_date_quality = "monsternamedatum",
  selection_mode = "closest_distance",
  grouping_col = "VHAG"
)

print(paste("Aantal matches:", sum(!is.na(res_strahler_vhag$qual_meetplaats))))

res_strahler_fc <- match_upstream_strahler(
  biota_sf = mi_data,
  quality_sf = fc_data,
  network_list = river_network,
  strahler_sf = strahler,
  use_strahler = TRUE,
  strahler_col = "orde",
  max_dist_m = 5000,       # Max 5km stroomopwaarts
  days_before = 180,       # Kwaliteit mag tot 180 dagen VOOR de biota meting zijn
  days_after = 14,         # Kwaliteit mag tot 14 dagen NA de biota meting zijn
  col_date_biota = "monsternamedatum",
  col_date_quality = "monsternamedatum",
  selection_mode = "closest_distance"
)
print(paste("Aantal matches:", sum(!is.na(res_strahler_fc$qual_meetplaats))))

test_owl <- match_upstream_strahler(
  biota_sf = mi_data,
  quality_sf = fc_data,
  network_list = river_network,
  # strahler_sf = strahler,
  use_strahler = FALSE,
  # strahler_col = "orde",
  max_dist_m = 5000,       # Max 5km stroomopwaarts
  days_before = 180,       # Kwaliteit mag tot 180 dagen VOOR de biota meting zijn
  days_after = 14,         # Kwaliteit mag tot 14 dagen NA de biota meting zijn
  col_date_biota = "monsternamedatum",
  col_date_quality = "monsternamedatum",
  selection_mode = "closest_distance",
  grouping_col = "WTRLICHC"
)
print(paste("Aantal matches:", sum(!is.na(test_owl$qual_meetplaats))))

test_vhag <- match_upstream_strahler(
  biota_sf = mi_data,
  quality_sf = fc_data,
  network_list = river_network,
  # strahler_sf = strahler,
  use_strahler = FALSE,
  # strahler_col = "orde",
  max_dist_m = 5000,       # Max 5km stroomopwaarts
  days_before = 180,       # Kwaliteit mag tot 180 dagen VOOR de biota meting zijn
  days_after = 14,         # Kwaliteit mag tot 14 dagen NA de biota meting zijn
  col_date_biota = "monsternamedatum",
  col_date_quality = "monsternamedatum",
  selection_mode = "closest_distance",
  grouping_col = "VHAG"
)
print(paste("Aantal matches:", sum(!is.na(test_vhag$qual_meetplaats))))


load("data/verwerkt/mi_nat_sv.rdata")
mi_nat_sv_data <- mi_data %>%
  inner_join(mi_nat_sv,
             by = c("meetplaats", "monsternamedatum")) %>%
  select(meetplaats) %>%
  unique()

mi_data_unique <- mi_data %>%
  select(meetplaats) %>%
  unique()

mapview(fd, color = "red") + mapview(strahler) + mapview(mi_nat_sv_data)
mapview(fd, color = "red") + mapview(strahler) + mapview(mi_data_unique)


load(here("data", "verwerkt", "tu_resultaten.rdata"))
pesticide_data <- pesticide_data %>%
  left_join(tu_per_sample %>%
              select(meetplaats, monsternamedatum, TU_sum))

match_upstream_strahler_fin <- function(biota_sf,
                                    quality_sf,
                                    network_list,
                                    discharge_sf = NULL,
                                    strahler_sf = NULL,
                                    use_strahler = FALSE,
                                    strahler_col = "orde",
                                    max_dist_m = 5000,
                                    snap_tolerance = 100,
                                    days_before = 14,
                                    days_after = 30,
                                    col_date_biota = "monsternamedatum",
                                    col_date_quality = "monsternamedatum",
                                    selection_mode = "closest_distance",
                                    aggr_method = "mean",     # NIEUW: "mean", "min" of "max"
                                    grouping_col = NULL) {

  # --- 1. Validatie ---
  valid_modes <- c("closest_distance", "closest_time", "all", "aggregate") # Aggregate toegevoegd
  if (!selection_mode %in% valid_modes) stop("selection_mode moet zijn: 'closest_distance', 'closest_time', 'all' of 'aggregate'")

  valid_aggr <- c("mean", "min", "max")
  if (!aggr_method %in% valid_aggr) stop("aggr_method moet zijn: 'mean', 'min' of 'max'")

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

  message("1/5 Snapping points to river network (with tolerance check)...")

  # --- Slimme Snap Functie ---
  snap_and_attribute <- function(points, network, grp_col, tol) {
    nearest_idx <- st_nearest_feature(points, network)
    dists <- st_distance(points, network[nearest_idx, ], by_element = TRUE)
    dists_num <- as.numeric(dists)

    valid_snaps <- dists_num <= tol

    n_dropped <- sum(!valid_snaps)
    if (n_dropped > 0) message(paste("   ->", n_dropped, "punten liggen verder dan", tol, "m van het netwerk en worden genegeerd."))

    points$node_id <- NA
    points$node_id[valid_snaps] <- as.character(network$from_node[nearest_idx[valid_snaps]])

    if (!is.null(grp_col)) {
      points$match_group_val <- NA
      points$match_group_val[valid_snaps] <- network[[grp_col]][nearest_idx[valid_snaps]]
    }
    return(points)
  }

  biota_sf <- snap_and_attribute(biota_sf, net_sf, grouping_col, snap_tolerance)
  quality_sf <- snap_and_attribute(quality_sf, net_sf, grouping_col, snap_tolerance)

  # --- Strahler Snapping ---
  if (use_strahler) {
    message("    -> Determining Strahler order (with tolerance check)...")
    get_strahler_order <- function(pts, str_layer, str_col, tol) {
      idx <- st_nearest_feature(pts, str_layer)
      dists <- st_distance(pts, str_layer[idx, ], by_element = TRUE)
      vals <- rep(NA, nrow(pts))
      valid <- as.numeric(dists) <= tol
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
    valid_dis <- as.numeric(dists_dis) <= snap_tolerance
    if (sum(valid_dis) > 0) {
      discharge_nodes <- unique(as.character(net_sf$from_node[idx_dis[valid_dis]]))
    }
  }

  message(paste0("2/5 Matching upstream points (Mode: ", selection_mode, ")..."))

  results <- list()

  pb <- progress_bar$new(
    format = "  [:bar] :percent eta: :eta",
    total = nrow(biota_sf), clear = FALSE, width = 60
  )

  for (i in seq_len(nrow(biota_sf))) {
    pb$tick()

    b_pt <- biota_sf[i, ]
    b_node <- b_pt$node_id
    b_id   <- b_pt$tmp_join_id

    match <- NA
    n_matches_found <- 0 # Teller voor aggregatie

    # 1. SCENARIO: PUNT LIGT TE VER VAN NETWERK
    if (is.na(b_node)) {
      match <- quality_df[1, ]
      match[] <- NA
      match$river_dist_m <- NA
    } else {
      # 2. SCENARIO: PUNT IS GESNAPT
      b_date <- b_pt[[col_date_biota]]

      subset_quality <- quality_df %>% filter(!is.na(node_id))

      # Filter Grouping
      if (!is.null(grouping_col)) {
        b_group_val <- b_pt$match_group_val
        if (!is.na(b_group_val)) {
          subset_quality <- subset_quality %>% filter(match_group_val == b_group_val)
        } else {
          subset_quality <- subset_quality[0, ]
        }
      }

      # Filter Strahler
      if (use_strahler && nrow(subset_quality) > 0) {
        b_strahler <- b_pt$strahler_val
        if (!is.na(b_strahler)) {
          subset_quality <- subset_quality %>%
            filter(!is.na(strahler_val)) %>%
            filter(abs(strahler_val - b_strahler) <= 1)
        } else {
          subset_quality <- subset_quality[0, ]
        }
      }

      if (nrow(subset_quality) > 0) {
        upstream_nodes <- names(subcomponent(g, v = b_node, mode = "in"))

        candidates <- subset_quality %>%
          filter(node_id %in% upstream_nodes) %>%
          mutate(
            temp_diff_days = as.numeric(difftime(b_date, .[[col_date_quality]], units = "days")),
            abs_time_diff  = abs(temp_diff_days)
          ) %>%
          filter(temp_diff_days >= -days_after & temp_diff_days <= days_before)

        if (nrow(candidates) > 0) {
          d_matrix <- distances(g, v = candidates$node_id, to = b_node, mode = "out")
          candidates$river_dist_m <- as.numeric(d_matrix)

          candidates_within <- candidates %>% filter(river_dist_m <= max_dist_m)

          # Topology Check
          if (nrow(candidates_within) > 0 && length(discharge_nodes) > 0) {
            keep_candidate <- sapply(candidates_within$node_id, function(cand_node) {
              if (cand_node == b_node) return(TRUE)
              path_res <- shortest_paths(g, from = cand_node, to = b_node, mode = "out")
              intermediate_nodes <- setdiff(names(path_res$vpath[[1]]), c(cand_node, b_node))
              if (any(intermediate_nodes %in% discharge_nodes)) return(FALSE) else return(TRUE)
            })
            candidates_within <- candidates_within[keep_candidate, ]
          }

          # --- SELECTIE LOGICA ---
          if (nrow(candidates_within) > 0) {

            if (selection_mode == "closest_distance") {
              match <- candidates_within[which.min(candidates_within$river_dist_m), ]
              n_matches_found <- 1

            } else if (selection_mode == "closest_time") {
              match <- candidates_within[which.min(candidates_within$abs_time_diff), ]
              n_matches_found <- 1

            } else if (selection_mode == "all") {
              match <- candidates_within
              n_matches_found <- nrow(candidates_within)

            } else if (selection_mode == "aggregate") {
              # --- AGGREGATIE LOGICA ---
              n_matches_found <- nrow(candidates_within)

              # Identificeer numerieke kolommen om te aggregeren
              # We sluiten technische kolommen uit
              tech_cols <- c("node_id", "river_dist_m", "temp_diff_days", "abs_time_diff", "strahler_val", "match_group_val")
              # We sluiten ook ID kolommen uit als die numeriek zouden zijn (beetje gokwerk, maar veiligheid)

              # Pak alle numerieke kolommen uit de candidates
              num_cols <- names(select_if(candidates_within, is.numeric))
              cols_to_agg <- setdiff(num_cols, tech_cols)

              # Bepaal de functie
              agg_fun <- switch(aggr_method,
                                "mean" = function(x) mean(x, na.rm = TRUE),
                                "min"  = function(x) min(x, na.rm = TRUE),
                                "max"  = function(x) max(x, na.rm = TRUE))

              # Bereken de aggregatie
              agg_values <- candidates_within %>%
                summarise(across(all_of(cols_to_agg), agg_fun))

              # We nemen de structuur van de eerste rij over (voor char/factor kolommen)
              match <- candidates_within[1, ]

              # Overschrijf de numerieke meetwaarden met het gemiddelde/min/max
              match[, cols_to_agg] <- agg_values

              # Zet afstand op gemiddelde afstand van de cluster
              match$river_dist_m <- mean(candidates_within$river_dist_m)
            }
          }
        }
      }

      if (!is.data.frame(match)) {
        match <- quality_df[1, ]
        match[] <- NA
        match$river_dist_m <- NA
      }
    }

    # 3. CENTRALE CLEANUP
    match$biota_match_id <- b_id
    match$n_matches <- n_matches_found # Voeg teller toe aan output

    cols_to_remove <- c("temp_diff_days", "abs_time_diff", "match_group_val", "strahler_val")
    cols_present <- intersect(names(match), cols_to_remove)
    if(length(cols_present) > 0) match[, cols_present] <- NULL

    results[[i]] <- match
  }

  message("3/5 Merging results...")
  matched_quality_df <- do.call(rbind, results)

  cols_to_rename <- setdiff(names(matched_quality_df), "biota_match_id")
  names(matched_quality_df)[names(matched_quality_df) %in% cols_to_rename] <-
    paste0("qual_", cols_to_rename)

  final_sf <- left_join(biota_sf, matched_quality_df, by = c("tmp_join_id" = "biota_match_id"))

  final_sf$tmp_join_id <- NULL
  if("match_group_val" %in% names(final_sf)) final_sf$match_group_val <- NULL
  if("strahler_val" %in% names(final_sf)) final_sf$strahler_val <- NULL

  return(final_sf)
}

test <- match_upstream_strahler_fin(
  biota_sf = mi_data,
  quality_sf = pesticide_data,
  network_list = river_network,
  strahler_sf = strahler,
  use_strahler = TRUE,
  strahler_col = "orde",
  max_dist_m = 5000,       # Max 5km stroomopwaarts
  days_before = 180,       # Kwaliteit mag tot 180 dagen VOOR de biota meting zijn
  days_after = 14,         # Kwaliteit mag tot 14 dagen NA de biota meting zijn
  col_date_biota = "monsternamedatum",
  col_date_quality = "monsternamedatum",
  selection_mode = "closest_distance",
  grouping_col = "VHAG"
)
print(paste("Aantal matches:", sum(!is.na(test$qual_meetplaats))))

test2 <- match_upstream_strahler_fin(
  biota_sf = mi_data,
  quality_sf = pesticide_data,
  network_list = river_network,
  strahler_sf = strahler,
  use_strahler = TRUE,
  strahler_col = "orde",
  max_dist_m = 5000,       # Max 5km stroomopwaarts
  days_before = 180,       # Kwaliteit mag tot 180 dagen VOOR de biota meting zijn
  days_after = 14,         # Kwaliteit mag tot 14 dagen NA de biota meting zijn
  col_date_biota = "monsternamedatum",
  col_date_quality = "monsternamedatum",
  selection_mode = "aggregate",
  aggr_method = "mean",
  grouping_col = "VHAG"
)
print(paste("Aantal matches:", sum(!is.na(test2$qual_meetplaats))))

match_upstream_strahler <- function(biota_sf,
                                    quality_sf,
                                    network_list,
                                    discharge_sf = NULL,
                                    strahler_sf = NULL,
                                    use_strahler = FALSE,
                                    strahler_col = "ORDE",
                                    max_dist_m = 5000,
                                    snap_tolerance = 100,
                                    days_before = 14,
                                    days_after = 30,
                                    col_date_biota = "monsternamedatum",
                                    col_date_quality = "monsternamedatum",
                                    selection_mode = "closest_distance",
                                    aggr_method = "mean",
                                    aggr_cols = NULL,
                                    grouping_col = NULL) {

  # --- 1. Validatie ---
  valid_modes <- c("closest_distance", "closest_time", "all", "aggregate")
  if (!selection_mode %in% valid_modes) stop("selection_mode moet zijn: 'closest_distance', 'closest_time', 'all' of 'aggregate'")

  valid_aggr <- c("mean", "min", "max")
  if (!aggr_method %in% valid_aggr) stop("aggr_method moet zijn: 'mean', 'min' of 'max'")

  if (!is.null(aggr_cols)) {
    missing_cols <- setdiff(aggr_cols, names(quality_sf))
    if (length(missing_cols) > 0) {
      stop(paste("De volgende aggr_cols bestaan niet in quality_sf:", paste(missing_cols, collapse = ", ")))
    }
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

  message("1/5 Snapping points to river network (with tolerance check)...")

  # --- Slimme Snap Functie (BUGFIX: NA Handling) ---
  snap_and_attribute <- function(points, network, grp_col, tol) {
    nearest_idx <- st_nearest_feature(points, network)
    dists <- st_distance(points, network[nearest_idx, ], by_element = TRUE)
    dists_num <- as.numeric(dists)

    # FIX: Vervang NA afstanden door oneindig, zodat de check niet crasht
    dists_num[is.na(dists_num)] <- Inf

    valid_snaps <- dists_num <= tol

    n_dropped <- sum(!valid_snaps) # Nu veilig voor NA
    if (n_dropped > 0) message(paste("   ->", n_dropped, "punten liggen verder dan", tol, "m van het netwerk en worden genegeerd."))

    points$node_id <- NA
    points$node_id[valid_snaps] <- as.character(network$from_node[nearest_idx[valid_snaps]])

    if (!is.null(grp_col)) {
      points$match_group_val <- NA
      points$match_group_val[valid_snaps] <- network[[grp_col]][nearest_idx[valid_snaps]]
    }
    return(points)
  }

  biota_sf <- snap_and_attribute(biota_sf, net_sf, grouping_col, snap_tolerance)
  quality_sf <- snap_and_attribute(quality_sf, net_sf, grouping_col, snap_tolerance)

  # --- Strahler Snapping (BUGFIX: NA Handling) ---
  if (use_strahler) {
    message("    -> Determining Strahler order (with tolerance check)...")
    get_strahler_order <- function(pts, str_layer, str_col, tol) {
      idx <- st_nearest_feature(pts, str_layer)
      dists <- st_distance(pts, str_layer[idx, ], by_element = TRUE)
      dists_num <- as.numeric(dists)

      # FIX: NA afhandeling
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

  # --- Discharge Snapping (BUGFIX: NA Handling) ---
  discharge_nodes <- character(0)
  if (!is.null(discharge_sf)) {
    message("    -> Snapping discharge points...")
    idx_dis <- st_nearest_feature(discharge_sf, net_sf)
    dists_dis <- st_distance(discharge_sf, net_sf[idx_dis, ], by_element = TRUE)
    dists_num <- as.numeric(dists_dis)

    # FIX: NA afhandeling
    dists_num[is.na(dists_num)] <- Inf

    valid_dis <- dists_num <= snap_tolerance
    if (sum(valid_dis) > 0) {
      discharge_nodes <- unique(as.character(net_sf$from_node[idx_dis[valid_dis]]))
    }
  }

  message(paste0("2/5 Matching upstream points (Mode: ", selection_mode, ")..."))

  results <- list()

  pb <- progress_bar$new(
    format = "  [:bar] :percent eta: :eta",
    total = nrow(biota_sf), clear = FALSE, width = 60
  )

  for (i in seq_len(nrow(biota_sf))) {
    pb$tick()

    b_pt <- biota_sf[i, ]
    b_node <- b_pt$node_id
    b_id   <- b_pt$tmp_join_id

    match <- NA
    n_matches_found <- 0

    if (is.na(b_node)) {
      match <- quality_df[1, ]
      match[] <- NA
      match$river_dist_m <- NA
    } else {
      b_date <- b_pt[[col_date_biota]]

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
          subset_quality <- subset_quality %>%
            filter(!is.na(strahler_val)) %>%
            filter(abs(strahler_val - b_strahler) <= 1)
        } else {
          subset_quality <- subset_quality[0, ]
        }
      }

      if (nrow(subset_quality) > 0) {
        upstream_nodes <- names(subcomponent(g, v = b_node, mode = "in"))

        candidates <- subset_quality %>%
          filter(node_id %in% upstream_nodes) %>%
          mutate(
            temp_diff_days = as.numeric(difftime(b_date, .[[col_date_quality]], units = "days")),
            abs_time_diff  = abs(temp_diff_days)
          ) %>%
          filter(temp_diff_days >= -days_after & temp_diff_days <= days_before)

        if (nrow(candidates) > 0) {
          d_matrix <- distances(g, v = candidates$node_id, to = b_node, mode = "out")
          candidates$river_dist_m <- as.numeric(d_matrix)

          candidates_within <- candidates %>% filter(river_dist_m <= max_dist_m)

          if (nrow(candidates_within) > 0 && length(discharge_nodes) > 0) {
            keep_candidate <- sapply(candidates_within$node_id, function(cand_node) {
              if (cand_node == b_node) return(TRUE)
              path_res <- shortest_paths(g, from = cand_node, to = b_node, mode = "out")
              intermediate_nodes <- setdiff(names(path_res$vpath[[1]]), c(cand_node, b_node))
              if (any(intermediate_nodes %in% discharge_nodes)) return(FALSE) else return(TRUE)
            })
            candidates_within <- candidates_within[keep_candidate, ]
          }

          if (nrow(candidates_within) > 0) {

            if (selection_mode == "closest_distance") {
              match <- candidates_within[which.min(candidates_within$river_dist_m), ]
              n_matches_found <- 1

            } else if (selection_mode == "closest_time") {
              match <- candidates_within[which.min(candidates_within$abs_time_diff), ]
              n_matches_found <- 1

            } else if (selection_mode == "all") {
              match <- candidates_within
              n_matches_found <- nrow(candidates_within)

            } else if (selection_mode == "aggregate") {
              n_matches_found <- nrow(candidates_within)

              if (!is.null(aggr_cols)) {
                cols_to_agg <- aggr_cols
              } else {
                tech_cols <- c("node_id", "river_dist_m", "temp_diff_days", "abs_time_diff", "strahler_val", "match_group_val")
                num_cols <- names(select_if(candidates_within, is.numeric))
                cols_to_agg <- setdiff(num_cols, tech_cols)
              }

              agg_fun <- switch(aggr_method,
                                "mean" = function(x) mean(x, na.rm = TRUE),
                                "min"  = function(x) min(x, na.rm = TRUE),
                                "max"  = function(x) max(x, na.rm = TRUE))

              if (length(cols_to_agg) > 0) {
                agg_values <- candidates_within %>%
                  summarise(across(all_of(cols_to_agg), agg_fun))
                match <- candidates_within[1, ]
                match[, cols_to_agg] <- agg_values
              } else {
                match <- candidates_within[1, ]
              }
              match$river_dist_m <- mean(candidates_within$river_dist_m)
            }
          }
        }
      }

      if (!is.data.frame(match)) {
        match <- quality_df[1, ]
        match[] <- NA
        match$river_dist_m <- NA
      }
    }

    match$biota_match_id <- b_id
    match$n_matches <- n_matches_found

    cols_to_remove <- c("temp_diff_days", "abs_time_diff", "match_group_val", "strahler_val")
    cols_present <- intersect(names(match), cols_to_remove)
    if(length(cols_present) > 0) match[, cols_present] <- NULL

    results[[i]] <- match
  }

  message("3/5 Merging results...")
  matched_quality_df <- do.call(rbind, results)

  cols_to_rename <- setdiff(names(matched_quality_df), "biota_match_id")
  names(matched_quality_df)[names(matched_quality_df) %in% cols_to_rename] <-
    paste0("qual_", cols_to_rename)

  final_sf <- left_join(biota_sf, matched_quality_df, by = c("tmp_join_id" = "biota_match_id"))

  final_sf$tmp_join_id <- NULL
  if("match_group_val" %in% names(final_sf)) final_sf$match_group_val <- NULL
  if("strahler_val" %in% names(final_sf)) final_sf$strahler_val <- NULL

  return(final_sf)
}

test2 <- match_upstream_strahler(
  biota_sf = mi_data,
  quality_sf = nutrient_data,
  network_list = river_network,
  strahler_sf = strahler,
  use_strahler = TRUE,
  strahler_col = "orde",
  max_dist_m = 5000,       # Max 5km stroomopwaarts
  days_before = 180,       # Kwaliteit mag tot 180 dagen VOOR de biota meting zijn
  days_after = 14,         # Kwaliteit mag tot 14 dagen NA de biota meting zijn
  col_date_biota = "monsternamedatum",
  col_date_quality = "monsternamedatum",
  selection_mode = "closest_distance",
  # aggr_method = "mean",
  # aggr_cols = c("TU_sum"),
  grouping_col = "VHAG"
)
print(paste("Aantal matches:", sum(!is.na(test2$qual_meetplaats))))

test3 <- match_upstream_strahler_fin(
  biota_sf = mi_data,
  quality_sf = pesticide_data,
  network_list = river_network,
  strahler_sf = strahler,
  use_strahler = TRUE,
  strahler_col = "orde",
  max_dist_m = 5000,       # Max 5km stroomopwaarts
  days_before = 180,       # Kwaliteit mag tot 180 dagen VOOR de biota meting zijn
  days_after = 14,         # Kwaliteit mag tot 14 dagen NA de biota meting zijn
  col_date_biota = "monsternamedatum",
  col_date_quality = "monsternamedatum",
  selection_mode = "closest_distance",
  # aggr_method = "mean",
  # aggr_cols = c("TU_sum"),
  grouping_col = "VHAG"
)
print(paste("Aantal matches:", sum(!is.na(test3$qual_meetplaats))))
