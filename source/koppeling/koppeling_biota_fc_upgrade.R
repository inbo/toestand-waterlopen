# ==============================================================================
# KOPPELING BIOTA <-> KWALITEITSMETINGEN VIA HET RIVIERNETWERK
# ==============================================================================
#
# WAT DOET DIT SCRIPT?
#   Aan elk biota-meetpunt (hier: macroinvertebraten, MI) wordt de meest
#   geschikte kwaliteitsmeting (hier: nutriënten/fys-chem) gekoppeld die
#   STROOMOPWAARTS ligt (of lokaal stroomafwaarts binnen max_downstream_m),
#   binnen een bepaald tijdsvenster.
#
# OPBOUW
#   1. Data inlezen
#   2. Riviernetwerk bouwen (lijnen opknippen + igraph-graaf) en cachen
#   3. Hoofdfunctie match_upstream_strahler() (fractie-methode; exacte afstand)
#   4. Voorbeelden van gebruik (verschillende opties)

# ==============================================================================


# ==============================================================================
# 1. DATA INLEZEN
# ==============================================================================

# Packages (sf, dplyr, igraph, lwgeom, progress, here, ...)
if (!exists("packages_geladen")) {
  source(here::here("source", "inladen_packages.R"))
}

# Netwerk: lijnen met stroomrichting (VHAS/VHAG-kolommen nodig voor matching/grouping)
fd <- st_read(here("data", "ruw", "netwerk", "Flow_direction_coordinates.shp"), quiet = TRUE)

# (niet gebruikt in dit script)
nodes <- st_read(here("data", "ruw", "waterlopen", "vha_network_junctions.shp"), quiet = TRUE)

# Lozingspunten (optioneel, enkel nodig als je discharge_sf gebruikt)
hpunt <- st_read("data/ruw/vmm/elina_koppeling/input NARA/input NARA/hpten.shp", quiet = TRUE) %>%
  st_transform(st_crs(fd))

# Strahler-orde (optioneel, enkel nodig als je use_strahler = TRUE gebruikt)
strahler <- st_read("data/ruw/waterlopen/strahler_orde.shp") %>%
  st_transform(., crs = st_crs(fd))

# Meetnetten: koppeltabel meetplaats (nummer) -> VHAS-segment
meetnetten <- read.delim("data/ruw/vmm/meetnetten.txt")

# Koppeltabel meetplaats -> VHAS, met 1 rij per meetplaats
# (anders worden rijen gedupliceerd bij de left_joins hieronder)
if (anyDuplicated(meetnetten$nummer) > 0) {
  warning("meetnetten bevat dubbele 'nummer'-waarden: enkel de eerste rij per meetplaats wordt behouden.")
}
meetnetten_vhas <- meetnetten %>%
  select(vhas, nummer) %>%
  distinct(nummer, .keep_all = TRUE)

# Biota-meetpunten (MI), vanaf 2010
mi_data <- st_read(here("data", "ruw", "macroinvertebraten", "mi_meetpunten_datum.gpkg"), quiet = TRUE) %>%
  filter(monsternamedatum > "2009-12-31") %>%
  left_join(meetnetten_vhas, by = c("meetplaats" = "nummer"))

# Kwaliteitsmeetpunten (nutriënten): eenmalig opbouwen en opslaan als gpkg
pad_nutrient <- here("data", "verwerkt", "koppeling", "nutrient_meetpunten_datum.gpkg")

if (!file.exists(pad_nutrient)) {
  load(here("data", "verwerkt", "fc_selectie.rdata"))
  fc_meetpunten_datum <- st_read(here("data", "ruw", "fys_chem", "fc_meetpunten.gpkg"), quiet = TRUE) %>%
    filter(monsternamedatum > "2007-12-31")
  nutrient_meetpunten_datum <- fc_selectie %>%
    filter(!is.na(n_t)) %>%
    select(meetplaats, monsternamedatum, n_t, p_t, czv, zs, nh4, no2, no3,
           cl, clfyl_a, ec_20, t, o2) %>%
    left_join(fc_meetpunten_datum, by = c("meetplaats", "monsternamedatum"))
  st_write(nutrient_meetpunten_datum, dsn = pad_nutrient, delete_dsn = TRUE)
}

# Dataset met alle observaties (meetplaats, monsternamedatum, locatie) + VHAS
nutrient_data <- st_read(dsn = pad_nutrient, quiet = TRUE) %>%
  left_join(meetnetten_vhas, by = c("meetplaats" = "nummer"))

# Alle lagen moeten dezelfde CRS hebben als het netwerk (st_nearest_feature
# geeft anders een foutmelding). Doet niets als ze al gelijk zijn.
mi_data       <- st_transform(mi_data, st_crs(fd))
nutrient_data <- st_transform(nutrient_data, st_crs(fd))


# ==============================================================================
# 2. RIVIERNETWERK BOUWEN
# ==============================================================================

# ------------------------------------------------------------------------------
# 2a. Lijnen opknippen in stukken van maximaal `max_length` meter
# ------------------------------------------------------------------------------

split_lines_equal_length <- function(lines_sf, max_length = 200) {

  message(paste("Lijnen splitsen in segmenten van max", max_length, "meter..."))

  # 1. Voorbereiding
  lines_sf <- st_cast(lines_sf, "LINESTRING", warn = FALSE)
  lines_sf$orig_len <- as.numeric(st_length(lines_sf))
  lines_sf$orig_id  <- 1:nrow(lines_sf)

  # 2. Scheid korte en lange lijnen
  short_lines <- lines_sf %>% filter(orig_len <= max_length)
  long_lines  <- lines_sf %>% filter(orig_len > max_length)

  if (nrow(long_lines) == 0) {
    lines_sf$orig_len <- NULL
    lines_sf$orig_id  <- NULL
    lines_sf$new_length <- as.numeric(st_length(lines_sf))
    return(lines_sf)
  }

  message(paste(" ->", nrow(long_lines), "lange lijnen worden opgeknipt..."))

  # 3. Aantal segmenten per lijn, en tabel "opblazen" (elke lijn n keer)
  # (-1e-5: een restant van < ~1 mm wordt niet als apart segment geteld)
  long_lines$n_segments <- ceiling(long_lines$orig_len / max_length - 1e-5)
  expanded_lines <- long_lines[rep(row.names(long_lines), long_lines$n_segments), ]

  expanded_lines <- expanded_lines %>%
    group_by(orig_id) %>%
    mutate(seg_num = row_number()) %>%
    ungroup()

  # 4. Begin- en eindfractie van elk stuk (0 = begin lijn, 1 = einde lijn)
  expanded_lines <- expanded_lines %>%
    mutate(
      step_size  = max_length / orig_len,
      start_frac = (seg_num - 1) * step_size,
      end_frac   = seg_num * step_size
    )

  # Afrondingsfouten: nooit meer dan 100% van de lijn
  expanded_lines$end_frac[expanded_lines$end_frac > 1] <- 1
  # Het laatste stuk van elke lijn loopt altijd tot het einde van de lijn
  expanded_lines$end_frac[expanded_lines$seg_num == expanded_lines$n_segments] <- 1

  # 5. Geometrieën effectief knippen
  message(" -> Geometrieën daadwerkelijk knippen (dit kan even duren)...")

  list_geoms <- mapply(
    function(geom, f_start, f_end) {
      st_linesubstring(geom, from = f_start, to = f_end, tolerance = 0.001)
    },
    expanded_lines$geometry,
    expanded_lines$start_frac,
    expanded_lines$end_frac,
    SIMPLIFY = FALSE
  )
  expanded_lines$geometry <- st_as_sfc(list_geoms, crs = st_crs(lines_sf))

  # 6. Samenvoegen en opkuisen
  expanded_lines <- expanded_lines %>%
    select(-n_segments, -seg_num, -step_size, -start_frac, -end_frac)

  final_sf <- bind_rows(short_lines, expanded_lines) %>%
    arrange(orig_id)

  final_sf$orig_len <- NULL
  final_sf$orig_id  <- NULL
  final_sf$new_length <- as.numeric(st_length(final_sf))

  message(paste("Klaar! Resultaat:", nrow(final_sf), "segmenten."))
  return(final_sf)
}


# ------------------------------------------------------------------------------
# 2b. Gerichte graaf bouwen, met clustering van nodes binnen `snap_tol` meter
# ------------------------------------------------------------------------------
build_river_network_auto_nodes <- function(lines_sf, snap_tol = 1) {
  message("Graaf bouwen: data opschonen en knooppunten genereren...")

  # --- STAP 0: data wassen (ongewijzigd) ---
  lines_sf <- lines_sf[!st_is_empty(lines_sf), ]
  if (any(grepl("GEOMETRY|COLLECTION", st_geometry_type(lines_sf)))) {
    lines_sf <- st_collection_extract(lines_sf, "LINESTRING")
  }
  valid_types <- c("LINESTRING", "MULTILINESTRING")
  lines_sf <- lines_sf[st_geometry_type(lines_sf) %in% valid_types, ]
  lines_sf$check_len <- as.numeric(st_length(lines_sf))
  lines_sf <- lines_sf[lines_sf$check_len > 0.001, ]
  lines_sf$check_len <- NULL
  lines_sf <- st_cast(lines_sf, "LINESTRING", warn = FALSE)
  message(paste(" -> Netwerk bevat", nrow(lines_sf), "geldige lijnsegmenten."))

  # Oude node-kolommen (bv. uit een gecachet netwerk) verwijderen
  lines_sf$from_node <- NULL
  lines_sf$to_node   <- NULL
  lines_sf$weight    <- NULL

  # --- STAP 1: ruwe nodes (1 mm-afronding) ---
  # Begin- en eindpunten rechtstreeks uit de coördinaten (veel sneller dan st_line_sample)
  cc <- st_coordinates(lines_sf)
  is_first <- !duplicated(cc[, "L1"])
  is_last  <- !duplicated(cc[, "L1"], fromLast = TRUE)
  coords_start <- cc[is_first, c("X", "Y"), drop = FALSE]
  coords_end   <- cc[is_last,  c("X", "Y"), drop = FALSE]
  rm(cc)

  id_start <- paste(round(coords_start[, 1], 3), round(coords_start[, 2], 3), sep = "_")
  id_end   <- paste(round(coords_end[, 1], 3),   round(coords_end[, 2], 3),   sep = "_")

  # --- STAP 1b: clusteren van nodes binnen snap_tol meter ---
  ends <- data.frame(
    id = c(id_start, id_end),
    x  = c(coords_start[, 1], coords_end[, 1]),
    y  = c(coords_start[, 2], coords_end[, 2])
  )
  ends <- ends[!duplicated(ends$id), ]

  if (snap_tol > 0) {
    message(paste(" -> Nodes clusteren binnen", snap_tol, "m..."))
    # kd-tree (dbscan::frNN): seconden i.p.v. uren; st_is_within_distance is O(n^2)
    nb <- dbscan::frNN(as.matrix(ends[, c("x", "y")]), eps = snap_tol)$id
    nb <- lapply(seq_along(nb), function(i) c(i, nb[[i]]))       # inclusief zichzelf
    g_nb <- graph_from_adj_list(nb, mode = "all")
    ends$cluster <- components(g_nb)$membership
  } else {
    ends$cluster <- seq_len(nrow(ends))
  }

  ends$node <- paste0("n", ends$cluster)

  # Controle: hoe groot worden de clusters? (ketting-effect opsporen)
  extent <- tapply(seq_len(nrow(ends)), ends$cluster, function(i) {
    max(diff(range(ends$x[i])), diff(range(ends$y[i])))
  })
  n_merged <- sum(table(ends$cluster) > 1)
  message(sprintf(" -> %d clusters met >1 ruwe node samengevoegd; grootste cluster-uitbreiding: %.1f m",
                  n_merged, max(extent)))
  if (max(extent) > 3 * snap_tol) {
    warning("Er zijn clusters die groter zijn dan 3x de tolerantie (ketting-effect). Controleer die in QGIS.")
  }

  lookup <- setNames(ends$node, ends$id)
  lines_sf$from_node <- unname(lookup[id_start])
  lines_sf$to_node   <- unname(lookup[id_end])
  lines_sf$weight    <- as.numeric(st_length(lines_sf))

  # Vertex-tabel: cluster-id + gemiddelde coördinaat
  vertices <- aggregate(cbind(x, y) ~ node, data = ends, FUN = mean)
  names(vertices)[1] <- "name"

  # --- STAP 2: edges. Segmenten met from == to (korter dan tolerantie) vallen weg ---
  edges <- st_drop_geometry(lines_sf)
  edges <- edges[, c("from_node", "to_node", "weight",
                     setdiff(names(edges), c("from_node", "to_node", "weight")))]
  names(edges)[1:2] <- c("from", "to")
  n_loops <- sum(edges$from == edges$to)
  if (n_loops > 0) {
    message(paste(" ->", n_loops, "segmenten met begin = eind (na clustering) niet in de graaf opgenomen."))
  }
  edges <- edges[edges$from != edges$to, ]

  # --- STAP 3: graaf, met alle nodes als vertices ---
  g <- graph_from_data_frame(edges, directed = TRUE, vertices = vertices)

  message("Graaf succesvol gebouwd!")
  return(list(graph = g, network_sf = lines_sf, nodes = vertices))
}


# ------------------------------------------------------------------------------
# 2c. Netwerk bouwen OF inladen (cache)
# ------------------------------------------------------------------------------
max_length <- 200   # m, max. segmentlengte
snap_tol   <- 1     # m, nodes binnen deze afstand worden samengevoegd

pad_netwerk <- here(
  "data", "verwerkt", "koppeling",
  sprintf("river_network_%dm_tol%gm.rds", max_length, snap_tol)
)

if (!file.exists(pad_netwerk)) {
  message("Geen cache gevonden: riviernetwerk opnieuw opbouwen...")
  fd_fine <- split_lines_equal_length(fd, max_length = max_length)
  river_network_fine <- build_river_network_auto_nodes(fd_fine, snap_tol = snap_tol)

  dir.create(dirname(pad_netwerk), recursive = TRUE, showWarnings = FALSE)
  saveRDS(river_network_fine, pad_netwerk)
  message(paste("Netwerk opgeslagen in:", pad_netwerk))
} else {
  message(paste("Netwerk inladen uit cache:", pad_netwerk))
  river_network_fine <- readRDS(pad_netwerk)
  if (inherits(river_network_fine$graph, "igraph")) {
    river_network_fine$graph <- igraph::upgrade_graph(river_network_fine$graph)
  }
}

# ==============================================================================
# 3. HOOFDFUNCTIE: match_upstream_strahler()  -- v2 (fractie-methode)
# ==============================================================================
# Koppelt biota-observaties aan kwaliteitsmetingen langs het gerichte
# riviernetwerk met positie langs het segment (`along_m`).
#
# Belangrijke argumenten (inclusief nieuw / gewijzigd t.o.v. v1):
#   max_dist_m        max. afstand stroomopwaarts langs de graaf (in m)
#   max_downstream_m  max. "lokale" afstand stroomafwaarts langs de graaf (in m)
#   max_vhas_snap     max. afstand (m) waarover een punt met VHAS nog op een segment
#                     met dezelfde VHAS gesnapt wordt; Inf = geen limiet; bij
#                     overschrijding valt het punt terug op geometrisch snappen
#                     binnen `snap_tolerance`
#   keep_time_diff    indien TRUE, blijft `qual_temp_diff_days` (biota -
#                     kwaliteit, in dagen) behouden in de output
#
# Werking v2 (fractie-methode):
#   - Elk punt wordt gesnapt op een SEGMENT, met de positie langs dat segment
#     (`along_m` = afstand vanaf het begin van het segment, in m), beveiligd
#     voor segmenten met minder dan 2 coördinaten (`n < 2`).
#   - Afstanden zijn exact langs de rivier (tot op de snapafstand):
#       stroomopwaartse kandidaat Q -> punt P:
#           (len_Q - along_Q) + graafafstand(to_node_Q -> from_node_P) + along_P
#       stroomafwaartse kandidaat Q van P:
#           (len_P - along_P) + graafafstand(to_node_P -> from_node_Q) + along_Q
#       zelfde segment: |along_P - along_Q|, met richting uit de volgorde
#   - Twee punten op hetzelfde segment hebben dus een echte richting en afstand.
#   - Per biota-punt 2 Dijkstra-runs i.p.v. 1 per kandidaat: veel sneller.
#   - Punten zonder geometrie worden overgeslagen (geen fout meer bij NA).
#   - Lozingscontrole ook met de fractie-methode: lozingen worden op een segment
#     gesnapt (met positie), en een kandidaat vervalt enkel als de lozing echt
#     tussen kandidaat en biota-punt ligt, ook binnen hetzelfde segment.
#   - Elke iteratie start met `empty_match()`, `aggregate` wijst veilig toe via
#     `as.list()`, en `results` wordt samengevoegd met `bind_rows()`.
#
# Vereist: network_list met $graph en $network_sf (kolommen from_node, to_node,
# weight), zoals gebouwd door build_river_network_auto_nodes().

match_upstream_strahler <- function(biota_sf,
                                    quality_sf,
                                    network_list,
                                    discharge_sf = NULL,
                                    strahler_sf = NULL,
                                    use_strahler = FALSE,
                                    strahler_col = "ORDE",
                                    max_dist_m = 5000,
                                    max_downstream_m = 200,
                                    snap_tolerance = 25,
                                    max_vhas_snap = Inf,
                                    days_before = 30,
                                    days_after = 10,
                                    col_date_biota = "monsternamedatum",
                                    col_date_quality = "monsternamedatum",
                                    selection_mode = "closest_distance",
                                    aggr_method = "mean",
                                    aggr_cols = NULL,
                                    grouping_col = NULL,
                                    vhas_col_network = "VHAS",
                                    vhas_col_biota = NULL,
                                    vhas_col_quality = NULL,
                                    keep_time_diff = TRUE) {

  # ---------------------------------------------------------------------------
  # 3.1 Validatie
  # ---------------------------------------------------------------------------
  valid_modes <- c("closest_distance", "closest_time", "all", "aggregate")
  if (!selection_mode %in% valid_modes) {
    stop("selection_mode moet zijn: 'closest_distance', 'closest_time', 'all' of 'aggregate'")
  }
  if (!aggr_method %in% c("mean", "min", "max")) stop("aggr_method moet zijn: 'mean', 'min' of 'max'")

  if (!is.null(aggr_cols)) {
    missing_cols <- setdiff(aggr_cols, names(quality_sf))
    if (length(missing_cols) > 0) {
      stop(paste("aggr_cols niet gevonden in quality_sf:", paste(missing_cols, collapse = ", ")))
    }
  }

  if (use_strahler) {
    if (is.null(strahler_sf)) stop("Je hebt 'use_strahler = TRUE' gezet, maar geen 'strahler_sf' opgegeven.")
    if (!strahler_col %in% names(strahler_sf)) stop(paste("Kolom", strahler_col, "niet gevonden in strahler_sf."))
    message(paste0("Strahler filter geactiveerd (max 1 orde verschil, max snap afstand: ", snap_tolerance, "m)."))
  }

  g      <- network_list$graph
  net_sf <- network_list$network_sf

  if (!all(c("from_node", "to_node", "weight") %in% names(net_sf))) {
    stop("network_sf mist from_node/to_node/weight. Bouw het netwerk opnieuw.")
  }

  if (!is.null(grouping_col)) {
    if (!grouping_col %in% names(net_sf)) stop(paste("De grouping_col", grouping_col, "bestaat niet in je netwerk!"))
    message(paste0("Grouping geactiveerd: enkel matchen binnen zelfde '", grouping_col, "'."))
  }

  # ---------------------------------------------------------------------------
  # 3.2 Voorbereiding
  # ---------------------------------------------------------------------------
  biota_sf[[col_date_biota]]     <- as.Date(biota_sf[[col_date_biota]])
  quality_sf[[col_date_quality]] <- as.Date(quality_sf[[col_date_quality]])

  # Kwaliteitspunten zonder geometrie kunnen nooit gekoppeld worden
  n_empty_q <- sum(st_is_empty(quality_sf))
  if (n_empty_q > 0) {
    message(paste("    ->", n_empty_q, "quality points without geometry removed."))
    quality_sf <- quality_sf[!st_is_empty(quality_sf), ]
  }

  biota_sf$tmp_join_id <- seq_len(nrow(biota_sf))
  quality_sf$orig_index_geo <- seq_len(nrow(quality_sf))

  # Coördinaten van alle netwerksegmenten (voor de positie langs het segment)
  net_xy   <- st_coordinates(net_sf)
  net_rows <- split(seq_len(nrow(net_xy)), net_xy[, "L1"])
  if (length(net_rows) != nrow(net_sf)) {
    stop("network_sf moet enkel LINESTRINGs bevatten (1 rij = 1 lijn).")
  }

  # Positie (m vanaf het begin van het segment) van punt i op segment seg_idx[i]
  along_distance <- function(pts_sf, seg_idx) {
    xy  <- st_coordinates(pts_sf)[, c("X", "Y"), drop = FALSE]
    out <- rep(NA_real_, length(seg_idx))
    for (i in which(!is.na(seg_idx))) {
      L  <- net_xy[net_rows[[seg_idx[i]]], c("X", "Y"), drop = FALSE]
      n  <- nrow(L)
      if (n < 2) {
        out[i] <- 0
        next
      }
      ax <- L[-n, 1]; ay <- L[-n, 2]
      dx <- L[-1, 1] - ax; dy <- L[-1, 2] - ay
      len2 <- dx^2 + dy^2
      t  <- ((xy[i, 1] - ax) * dx + (xy[i, 2] - ay) * dy) / ifelse(len2 == 0, 1, len2)
      t  <- pmin(1, pmax(0, t))
      d2 <- (xy[i, 1] - (ax + t * dx))^2 + (xy[i, 2] - (ay + t * dy))^2
      k  <- which.min(d2)
      sl <- sqrt(len2)
      out[i] <- sum(sl[seq_len(k - 1)]) + t[k] * sl[k]
    }
    out
  }

  # ---------------------------------------------------------------------------
  # 3.3 Snapping op het netwerk (stap 1/5)
  # ---------------------------------------------------------------------------
  message("1/5 Snapping points to river network...")

  # Elk punt krijgt:
  #   seg_idx     rij in net_sf van het gekozen segment
  #   node_id     from_node van dat segment, to_node_id: to_node
  #   along_m     positie langs het segment (m), seg_len_m: lengte segment
  # Volgorde: 1) segment met dezelfde VHAS (binnen max_vhas_snap),
  #           2) anders geometrisch dichtstbijzijnde segment binnen `tol`.

  snap_and_attribute <- function(points, network, grp_col, tol, vhas_col_pts, vhas_col_net) {
    points$seg_idx     <- NA_integer_
    points$node_id     <- NA_character_
    points$to_node_id  <- NA_character_
    points$along_m     <- NA_real_
    points$seg_len_m   <- NA_real_
    if (!is.null(grp_col)) points$match_group_val <- NA
    empty <- st_is_empty(points)

    # --- 1. VHAS-matching ---
    can_use_vhas <- !is.null(vhas_col_pts) && !is.null(vhas_col_net) &&
      (vhas_col_pts %in% names(points)) && (vhas_col_net %in% names(network))

    if (can_use_vhas) {
      unique_codes <- unique(points[[vhas_col_pts]][!is.na(points[[vhas_col_pts]]) & !empty])
      for (code in unique_codes) {
        net_idx <- which(network[[vhas_col_net]] == code)
        if (length(net_idx) == 0) next
        p_idx <- which(points[[vhas_col_pts]] == code & !empty)
        if (length(p_idx) == 0) next
        rel   <- st_nearest_feature(points[p_idx, ], network[net_idx, ])
        abs_n <- net_idx[rel]
        if (is.finite(max_vhas_snap)) {
          dd <- as.numeric(st_distance(points[p_idx, ], network[abs_n, ], by_element = TRUE))
          ok <- !is.na(dd) & dd <= max_vhas_snap
          p_idx <- p_idx[ok]; abs_n <- abs_n[ok]
        }
        points$seg_idx[p_idx] <- abs_n
      }
    }

    # --- 2. Geometrische fallback ---
    missing_idx <- which(is.na(points$seg_idx) & !empty)
    if (length(missing_idx) > 0) {
      subset_pts  <- points[missing_idx, ]
      nearest_idx <- st_nearest_feature(subset_pts, network)
      dists_num   <- as.numeric(st_distance(subset_pts, network[nearest_idx, ], by_element = TRUE))
      dists_num[is.na(dists_num)] <- Inf
      valid <- dists_num <= tol
      points$seg_idx[missing_idx[valid]] <- nearest_idx[valid]

      n_dropped <- sum(!valid)
      if (n_dropped > 0) message(paste("      ->", n_dropped, "points dropped (too far/no VHAS match)."))
    }

    # --- Segmentgegevens en positie langs het segment ---
    has <- which(!is.na(points$seg_idx))
    if (length(has) > 0) {
      s <- points$seg_idx[has]
      points$node_id[has]    <- as.character(network$from_node[s])
      points$to_node_id[has] <- as.character(network$to_node[s])
      points$seg_len_m[has]  <- network$weight[s]
      points$along_m[has]    <- pmin(along_distance(points[has, ], s), network$weight[s])
      if (!is.null(grp_col)) points$match_group_val[has] <- network[[grp_col]][s]
    }
    return(points)
  }

  biota_sf   <- snap_and_attribute(biota_sf,   net_sf, grouping_col, snap_tolerance,
                                   vhas_col_biota,   vhas_col_network)
  quality_sf <- snap_and_attribute(quality_sf, net_sf, grouping_col, snap_tolerance,
                                   vhas_col_quality, vhas_col_network)

  # --- Strahler-orde (ongewijzigd) ---
  if (use_strahler) {
    message("    -> Determining Strahler order...")
    get_strahler_order <- function(pts, str_layer, str_col, tol) {
      vals <- rep(NA_real_, nrow(pts))
      ok   <- !st_is_empty(pts)
      if (!any(ok)) return(vals)
      idx       <- st_nearest_feature(pts[ok, ], str_layer)
      dists_num <- as.numeric(st_distance(pts[ok, ], str_layer[idx, ], by_element = TRUE))
      dists_num[is.na(dists_num)] <- Inf
      v <- rep(NA_real_, sum(ok))
      valid <- dists_num <= tol
      v[valid] <- as.numeric(str_layer[[str_col]][idx[valid]])
      vals[ok] <- v
      vals
    }
    biota_sf$strahler_val   <- get_strahler_order(biota_sf,   strahler_sf, strahler_col, snap_tolerance)
    quality_sf$strahler_val <- get_strahler_order(quality_sf, strahler_sf, strahler_col, snap_tolerance)
  }

  quality_df <- st_drop_geometry(quality_sf)

  # Lozingspunten: gesnapt op een segment, met positie langs het segment (fractie-methode)
  dis_by_seg <- list(); dis_keys <- character(0); dis_loop_nodes <- character(0); n_dis <- 0
  if (!is.null(discharge_sf)) {
    message("    -> Snapping discharge points...")
    discharge_sf <- discharge_sf[!st_is_empty(discharge_sf), ]
    idx_dis   <- st_nearest_feature(discharge_sf, net_sf)
    dists_num <- as.numeric(st_distance(discharge_sf, net_sf[idx_dis, ], by_element = TRUE))
    dists_num[is.na(dists_num)] <- Inf
    valid_dis <- dists_num <= snap_tolerance
    if (sum(valid_dis) > 0) {
      s_dis <- idx_dis[valid_dis]
      a_dis <- pmin(along_distance(discharge_sf[valid_dis, ], s_dis), net_sf$weight[s_dis])
      dis_df <- unique(data.frame(seg_idx = s_dis, along_m = a_dis,
                                  from = as.character(net_sf$from_node[s_dis]),
                                  to   = as.character(net_sf$to_node[s_dis])))
      n_dis          <- nrow(dis_df)
      dis_by_seg     <- split(dis_df$along_m, dis_df$seg_idx)
      dis_keys       <- paste(dis_df$from, dis_df$to)            # "from to" van elk segment met lozing
      dis_loop_nodes <- unique(dis_df$from[dis_df$from == dis_df$to])  # lozing op segmentje < tolerantie
    }
    message(paste("      ->", n_dis, "discharge points snapped."))
  }

  empty_match <- function() {
    m <- quality_df[1, ]
    m[] <- NA
    m$river_dist_m <- NA
    m$temp_diff_days <- NA_real_
    m
  }

  # ---------------------------------------------------------------------------
  # 3.4 Matching per biota-punt (stap 2/5)
  # ---------------------------------------------------------------------------
  message(paste0("2/5 Matching points (Mode: ", selection_mode,
                 " | Upstream: ", max_dist_m, "m OR Local: ", max_downstream_m, "m)..."))

  results <- list()
  pb <- progress_bar$new(format = "  [:bar] :percent eta: :eta",
                         total = nrow(biota_sf), clear = FALSE, width = 60)

  for (i in seq_len(nrow(biota_sf))) {
    pb$tick()
    b_pt   <- biota_sf[i, ]
    b_node <- b_pt$node_id          # from_node van het segment van het biota-punt
    b_id   <- b_pt$tmp_join_id
    match  <- empty_match()
    n_matches_found <- 0

    if (is.na(b_node)) {
      match <- empty_match()

    } else {
      b_date  <- b_pt[[col_date_biota]]
      b_to    <- b_pt$to_node_id
      b_seg   <- b_pt$seg_idx
      b_along <- b_pt$along_m
      b_len   <- b_pt$seg_len_m

      # --- STAP A: niet-ruimtelijke filters (ongewijzigd) ---
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
            filter(!is.na(strahler_val), abs(strahler_val - b_strahler) <= 1)
        } else {
          subset_quality <- subset_quality[0, ]
        }
      }

      if (nrow(subset_quality) > 0) {
        subset_quality <- subset_quality %>%
          mutate(
            temp_diff_days = as.numeric(difftime(b_date, .data[[col_date_quality]], units = "days")),
            abs_time_diff  = abs(temp_diff_days)
          ) %>%
          filter(temp_diff_days >= -days_after & temp_diff_days <= days_before)
      }

      if (nrow(subset_quality) > 0) {

        # --- STAP B: exacte afstanden langs de rivier (fractie-methode) ---
        cand_to   <- unique(subset_quality$to_node_id)
        cand_from <- unique(subset_quality$node_id)

        # Eén Dijkstra-run per richting:
        #  stroomopwaarts: van elke to_node_Q naar from_node_P (mode "in" = omgekeerde graaf)
        #  stroomafwaarts: van to_node_P naar elke from_node_Q
        d_up   <- distances(g, v = b_node, to = cand_to,   mode = "in")
        d_down <- distances(g, v = b_to,   to = cand_from, mode = "out")
        lk_up   <- setNames(as.numeric(d_up),   cand_to)
        lk_down <- setNames(as.numeric(d_down), cand_from)

        qa <- subset_quality$along_m
        ql <- subset_quality$seg_len_m
        up <- (ql - qa) + unname(lk_up[subset_quality$to_node_id]) + b_along
        dn <- (b_len - b_along) + unname(lk_down[subset_quality$node_id]) + qa

        # Zelfde segment: richting volgt uit de positie langs het segment
        same <- subset_quality$seg_idx == b_seg
        up[same] <- ifelse(qa[same] <= b_along, b_along - qa[same], Inf)
        dn[same] <- ifelse(qa[same] >  b_along, qa[same] - b_along, Inf)

        subset_quality$graph_dist  <- up
        subset_quality$down_dist   <- dn
        subset_quality$is_upstream <- is.finite(up)

        # --- STAP C: de "OR"-filter ---
        candidates <- subset_quality %>%
          filter(
            (is_upstream & graph_dist <= max_dist_m) |
              (down_dist <= max_downstream_m)
          )

        if (nrow(candidates) > 0) {

          candidates$via_graph <- candidates$is_upstream & candidates$graph_dist <= max_dist_m
          candidates$river_dist_m <- ifelse(candidates$via_graph,
                                            candidates$graph_dist,
                                            candidates$down_dist)

          # --- STAP D: lozingscontrole (fractie-methode) ---
          # Een stroomopwaartse kandidaat Q vervalt als er een lozing D ligt tussen Q en
          # het biota-punt P:
          #   - zelfde segment:      along_Q < along_D <= along_P
          #   - segment van Q:       along_D > along_Q  (voorbij Q)
          #   - segment van P:       along_D <= along_P (vóór P)
          #   - tussenliggende segmenten: segmenten die volledig op de kortste weg liggen
          # Enkel voor stroomopwaartse kandidaten (zoals voorheen).
          if (n_dis > 0) {
            to_check_idx <- which(candidates$via_graph & candidates$river_dist_m > 0)

            if (length(to_check_idx) > 0) {
              # Kortste wegen in 1 Dijkstra-run (omgekeerde graaf), per unieke to_node van Q
              cand_to_n <- candidates$to_node_id[to_check_idx]
              need_path <- unique(cand_to_n[candidates$seg_idx[to_check_idx] != b_seg & cand_to_n != b_node])
              path_info <- list()
              if (length(need_path) > 0) {
                sp <- shortest_paths(g, from = b_node, to = need_path,
                                     mode = "in", output = "vpath")$vpath
                path_info <- lapply(sp, function(p) {
                  pn <- rev(names(p))                         # to_node_Q ... from_node_P
                  list(nodes = pn, keys = paste(pn[-length(pn)], pn[-1]))
                })
                names(path_info) <- need_path
              }

              blocked <- vapply(to_check_idx, function(ix) {
                sQ  <- candidates$seg_idx[ix]
                aQ  <- candidates$along_m[ix]
                to_n <- candidates$to_node_id[ix]
                dQ  <- dis_by_seg[[as.character(sQ)]]
                if (sQ == b_seg) return(!is.null(dQ) && any(dQ > aQ & dQ <= b_along))
                if (!is.null(dQ) && any(dQ > aQ)) return(TRUE)
                dP <- dis_by_seg[[as.character(b_seg)]]
                if (!is.null(dP) && any(dP <= b_along)) return(TRUE)
                if (to_n == b_node) return(FALSE)
                pk <- path_info[[to_n]]
                any(dis_keys %in% pk$keys) || any(dis_loop_nodes %in% pk$nodes)
              }, logical(1))

              keep_flags <- rep(TRUE, nrow(candidates))
              keep_flags[to_check_idx] <- !blocked
              candidates <- candidates[keep_flags, ]
            }
          }

          # --- STAP E: selectie volgens selection_mode ---
          if (nrow(candidates) > 0) {
            if (selection_mode == "closest_distance") {
              candidates <- candidates %>% arrange(river_dist_m, abs_time_diff)
              match <- candidates[1, ]
              n_matches_found <- 1

            } else if (selection_mode == "closest_time") {
              candidates <- candidates %>% arrange(abs_time_diff, river_dist_m)
              match <- candidates[1, ]
              n_matches_found <- 1

            } else if (selection_mode == "all") {
              match <- candidates
              n_matches_found <- nrow(candidates)

            } else if (selection_mode == "aggregate") {
              n_matches_found <- nrow(candidates)
              if (!is.null(aggr_cols)) {
                cols_to_agg <- aggr_cols
              } else {
                tech_cols <- c("node_id", "to_node_id", "seg_idx", "along_m", "seg_len_m",
                               "river_dist_m", "temp_diff_days", "abs_time_diff",
                               "strahler_val", "match_group_val", "orig_index_geo",
                               "graph_dist", "down_dist", "is_upstream")
                num_cols    <- names(candidates %>% select(where(is.numeric)))
                cols_to_agg <- setdiff(num_cols, c(tech_cols, vhas_col_quality))
              }
              agg_fun <- switch(aggr_method,
                                "mean" = function(x) mean(x, na.rm = TRUE),
                                "min"  = function(x) min(x,  na.rm = TRUE),
                                "max"  = function(x) max(x,  na.rm = TRUE))

              match <- candidates[1, ]
              if (length(cols_to_agg) > 0) {
                agg_values <- candidates %>% summarise(across(all_of(cols_to_agg), agg_fun))
                match[1, cols_to_agg] <- as.list(agg_values[1, , drop = TRUE])
              }
              match$river_dist_m <- mean(candidates$river_dist_m)
              if ("temp_diff_days" %in% names(candidates)) {
                match$temp_diff_days <- mean(candidates$temp_diff_days)
              }
            }
          }
        }
      }

      if (!is.data.frame(match)) match <- empty_match()
    }

    match$biota_match_id <- b_id
    match$n_matches      <- n_matches_found

    cols_to_remove <- c("abs_time_diff", "match_group_val", "strahler_val",
                        "orig_index_geo", "graph_dist", "down_dist", "is_upstream", "via_graph",
                        "seg_idx", "to_node_id", "along_m", "seg_len_m")
    if (!keep_time_diff) cols_to_remove <- c(cols_to_remove, "temp_diff_days")
    match[, base::intersect(names(match), cols_to_remove)] <- NULL
    results[[i]] <- match
  }

  # ---------------------------------------------------------------------------
  # 3.5 Resultaten samenvoegen (stap 3/5)
  # ---------------------------------------------------------------------------
  message("3/5 Merging results...")
  matched_quality_df <- bind_rows(results)

  cols_to_rename <- setdiff(names(matched_quality_df), "biota_match_id")
  names(matched_quality_df)[names(matched_quality_df) %in% cols_to_rename] <-
    paste0("qual_", cols_to_rename)

  final_sf <- left_join(biota_sf, matched_quality_df, by = c("tmp_join_id" = "biota_match_id"))

  final_sf$tmp_join_id <- NULL
  for (cc_drop in c("match_group_val", "strahler_val", "seg_idx", "to_node_id", "along_m", "seg_len_m")) {
    if (cc_drop %in% names(final_sf)) final_sf[[cc_drop]] <- NULL
  }

  return(final_sf)
}

# ==============================================================================
# 4. VOORBEELDEN
# ==============================================================================

# ------------------------------------------------------------------------------
# Voorbeeld 1: jouw huidige koppeling (MI <-> nutriënten)
#   Dichtstbijzijnde nutriëntenmeting op hetzelfde VHAG, max 5 km stroomopwaarts
#   (of max 200 m stroomafwaarts langs de graaf), van 180 dagen voor tot 14 dagen na het MI-staal.
# ------------------------------------------------------------------------------

koppeling_mi_nutrient_nieuw <- match_upstream_strahler(
  biota_sf = mi_data,
  quality_sf = nutrient_data,
  network_list = river_network_fine,
  max_dist_m = 5000,
  max_downstream_m = 200,
  max_vhas_snap = Inf,          # bv. 250 m; Inf = geen limiet
  days_before = 180,
  days_after = 14,
  col_date_biota   = "monsternamedatum",
  col_date_quality = "monsternamedatum",
  selection_mode = "closest_distance",
  grouping_col = "VHAG",
  vhas_col_network = "VHAS",
  vhas_col_biota = "vhas",
  vhas_col_quality = "vhas")

print(paste("Aantal matches:", sum(!is.na(koppeling_mi_nutrient$qual_meetplaats))))

# Snelle controle van het resultaat
summary(koppeling_mi_nutrient$qual_river_dist_m)
table(koppeling_mi_nutrient$qual_n_matches, useNA = "ifany")

# Opgeslagen onder een NIEUWE naam, zodat het oude resultaat bewaard blijft voor de
# vergelijking (test_vergelijking_oud_nieuw.R). Hernoem naar ..._nieuw weg als je tevreden bent.
save(koppeling_mi_nutrient, file = "data/verwerkt/koppeling/koppeling_mi_nutrient_nieuw.rdata")


# ------------------------------------------------------------------------------
# Voorbeeld 2: meetpunt in TIJD het dichtst, afstand pas tweede criterium
# ------------------------------------------------------------------------------
# koppeling_tijd <- match_upstream_strahler(
#   biota_sf = mi_data, quality_sf = nutrient_data, network_list = river_network_fine,
#   days_before = 90, days_after = 30,
#   selection_mode = "closest_time",
#   grouping_col = "VHAG",
#   vhas_col_network = "VHAS", vhas_col_biota = "vhas", vhas_col_quality = "vhas"
# )

# ------------------------------------------------------------------------------
# Voorbeeld 3: gemiddelde van ALLE nutriëntenmetingen in het venster
#   (aggr_cols expliciet opgeven, zodat bv. 'vhas' niet gemiddeld wordt)
# ------------------------------------------------------------------------------
# koppeling_gem <- match_upstream_strahler(
#   biota_sf = mi_data, quality_sf = nutrient_data, network_list = river_network_fine,
#   days_before = 180, days_after = 0,
#   selection_mode = "aggregate",
#   aggr_method = "mean",                 # of "min" / "max"
#   aggr_cols = c("n_t", "p_t", "no3", "nh4"),
#   grouping_col = "VHAG",
#   vhas_col_network = "VHAS", vhas_col_biota = "vhas", vhas_col_quality = "vhas"
# )
# qual_n_matches = aantal metingen waarover gemiddeld werd

# ------------------------------------------------------------------------------
# Voorbeeld 4: alle matches bewaren (1 rij per biota-kwaliteit-paar)
# ------------------------------------------------------------------------------
# koppeling_alle <- match_upstream_strahler(
#   biota_sf = mi_data, quality_sf = nutrient_data, network_list = river_network_fine,
#   selection_mode = "all",
#   grouping_col = "VHAG",
#   vhas_col_network = "VHAS", vhas_col_biota = "vhas", vhas_col_quality = "vhas"
# )

# ------------------------------------------------------------------------------
# Voorbeeld 5: met Strahler-filter (orde mag max 1 verschillen)
# ------------------------------------------------------------------------------
# koppeling_strahler <- match_upstream_strahler(
#   biota_sf = mi_data, quality_sf = nutrient_data, network_list = river_network_fine,
#   strahler_sf = strahler, use_strahler = TRUE, strahler_col = "orde",
#   snap_tolerance = 25,
#   grouping_col = "VHAG",
#   vhas_col_network = "VHAS", vhas_col_biota = "vhas", vhas_col_quality = "vhas"
# )

# ------------------------------------------------------------------------------
# Voorbeeld 6: kwaliteitspunten weigeren als er een lozing tussen ligt (hpunt)
# ------------------------------------------------------------------------------
# koppeling_lozing <- match_upstream_strahler(
#   biota_sf = mi_data, quality_sf = nutrient_data, network_list = river_network_fine,
#   discharge_sf = hpunt,
#   grouping_col = "VHAG",
#   vhas_col_network = "VHAS", vhas_col_biota = "vhas", vhas_col_quality = "vhas"
# )

# ------------------------------------------------------------------------------
# Voorbeeld 7: zonder VHAS-matching en zonder groepsfilter (enkel geometrisch)
#   Punten verder dan snap_tolerance van het netwerk vallen dan weg.
# ------------------------------------------------------------------------------
# koppeling_geo <- match_upstream_strahler(
#   biota_sf = mi_data, quality_sf = nutrient_data, network_list = river_network_fine,
#   snap_tolerance = 50,
#   grouping_col = NULL
# )
