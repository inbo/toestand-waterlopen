# ==============================================================================
# KOPPELING BIOTA <-> KWALITEITSMETINGEN VIA HET RIVIERNETWERK
# ==============================================================================
#
# WAT DOET DIT SCRIPT?
#   Aan elk biota-meetpunt (hier: macroinvertebraten, MI) wordt de meest
#   geschikte kwaliteitsmeting (hier: nutriënten/fys-chem) gekoppeld die
#   STROOMOPWAARTS ligt, binnen een bepaald tijdsvenster.
#
# OPBOUW
#   1. Data inlezen
#   2. Riviernetwerk bouwen (lijnen opknippen + igraph-graaf) en cachen
#   3. Hoofdfunctie match_upstream_strahler() (alle hulpfuncties zitten er, net
#      als in het originele script, binnenin)
#   4. Voorbeelden van gebruik (verschillende opties)
#
# De METHODE is ongewijzigd t.o.v. het originele script. Aangepast:
#   - structuur, commentaar en kleine technische opkuisingen (T -> TRUE,
#     select_if -> select(where()), .[[ ]] -> .data[[ ]])
#   - meetnetten: dubbele 'nummer' worden gededupliceerd (met waarschuwing)
#   - CRS van mi_data en nutrient_data wordt naar die van het netwerk getransformeerd
#   - split-functie: restsegment < 1 mm wordt niet meer afgeknipt maar bij het
#     laatste stuk gehouden (geen gaten meer in de graaf)
#   - vogelvlucht-afstand is weggehaald: "lokale" matches stroomafwaarts worden nu
#     ook langs de graaf gemeten (max_downstream_m = max afstand stroomafwaarts, in m)
#   - nieuw argument keep_time_diff: bewaart qual_temp_diff_days (biota - kwaliteit, in dagen)
#   - aggregate: de VHAS-kolom van quality_sf wordt nooit meer gemiddeld
# LET OP: omdat de split-functie lichtjes wijzigt, moet je het bestaande
# river_network_fine.rds eenmalig verwijderen zodat het netwerk opnieuw gebouwd wordt.
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
# Waarom? Hoe fijner de segmenten, hoe nauwkeuriger de afstanden in de graaf
# (een punt wordt namelijk gesnapt op het BEGINPUNT van het dichtstbijzijnde
# segment, dus de maximale fout is ongeveer `max_length`).
#
# Werking:
#   - lijnen <= max_length blijven ongewijzigd
#   - langere lijnen worden n keer gekopieerd (n = ceiling(lengte / max_length))
#   - kopie k krijgt het stuk van fractie (k-1)*stap tot k*stap van de lijn
#   - alle attributen (VHAS, VHAG, ...) worden meegekopieerd
# Opmerking: de stukken zijn dus max_length lang, het LAATSTE stuk is korter
# (niet allemaal even lang, ondanks de functienaam).
# Opmerking: werkt enkel als de geometriekolom "geometry" heet (is zo bij .shp).

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
# 2b. Gerichte graaf bouwen uit de lijnsegmenten
# ------------------------------------------------------------------------------
# - Stap 0: data wassen (lege/foute geometrieën en lijnen < 1 mm verwijderen)
# - Stap 1: node-ID = afgeronde (1 mm) coördinaten van begin- en eindpunt;
#           twee segmenten die op dezelfde coördinaat eindigen/beginnen delen
#           dus automatisch een node
# - Stap 2: from_node, to_node en weight (= lengte in m) als attributen
# - Stap 3: igraph-graaf (directed = TRUE: richting = stroomrichting)
#
# Geeft een lijst terug:
#   $graph      igraph-object (edges = segmenten, vertices = nodes)
#   $network_sf sf-object met de segmenten + from_node/to_node/weight

build_river_network_auto_nodes <- function(lines_sf) {
  message("Graaf bouwen: data opschonen en knooppunten genereren...")

  # --- STAP 0: data wassen ---
  lines_sf <- lines_sf[!st_is_empty(lines_sf), ]

  # GEOMETRYCOLLECTION -> enkel de lijnen houden
  if (any(grepl("GEOMETRY|COLLECTION", st_geometry_type(lines_sf)))) {
    lines_sf <- st_collection_extract(lines_sf, "LINESTRING")
  }

  # Enkel (MULTI)LINESTRINGS houden
  valid_types <- c("LINESTRING", "MULTILINESTRING")
  lines_sf <- lines_sf[st_geometry_type(lines_sf) %in% valid_types, ]

  # Lijnen met lengte ~0 geven problemen in de graaf (start = eind)
  lines_sf$check_len <- as.numeric(st_length(lines_sf))
  lines_sf <- lines_sf[lines_sf$check_len > 0.001, ]   # alles < 1 mm weg
  lines_sf$check_len <- NULL

  lines_sf <- st_cast(lines_sf, "LINESTRING", warn = FALSE)
  message(paste(" -> Netwerk bevat", nrow(lines_sf), "geldige lijnsegmenten."))

  # --- STAP 1: nodes bepalen ---
  p_start <- st_line_sample(lines_sf, sample = 0)
  p_end   <- st_line_sample(lines_sf, sample = 1)

  coords_start <- st_coordinates(p_start)
  coords_end   <- st_coordinates(p_end)

  node_id_start <- paste(round(coords_start[, 1], 3), round(coords_start[, 2], 3), sep = "_")
  node_id_end   <- paste(round(coords_end[, 1], 3),   round(coords_end[, 2], 3),   sep = "_")

  # --- STAP 2: attributen ---
  lines_sf$from_node <- node_id_start
  lines_sf$to_node   <- node_id_end
  lines_sf$weight    <- as.numeric(st_length(lines_sf))

  # --- STAP 3: graaf bouwen ---
  edges <- st_drop_geometry(lines_sf) %>%
    select(from = from_node, to = to_node, weight, everything())

  g <- graph_from_data_frame(edges, directed = TRUE)

  message("Graaf succesvol gebouwd!")
  return(list(graph = g, network_sf = lines_sf))
}


# ------------------------------------------------------------------------------
# 2c. Netwerk bouwen OF inladen (cache)
# ------------------------------------------------------------------------------
# LET OP: wijzig je max_length of de ruwe shapefile, verwijder dan eerst het
# .rds-bestand, anders wordt het oude netwerk ingeladen.

pad_netwerk <- here("data", "verwerkt", "koppeling", "river_network_fine.rds")

if (!file.exists(pad_netwerk)) {
  fd_fine <- split_lines_equal_length(fd, max_length = 200)
  river_network_fine <- build_river_network_auto_nodes(fd_fine)
  saveRDS(river_network_fine, pad_netwerk)
} else {
  river_network_fine <- readRDS(pad_netwerk)
}


# ==============================================================================
# 3. HOOFDFUNCTIE: match_upstream_strahler()
# ==============================================================================
#
# DOEL
#   Koppel aan elk punt in `biota_sf` de beste meting uit `quality_sf` die
#   stroomopwaarts ligt (via het riviernetwerk) en binnen het tijdsvenster valt.
#
# WERKWIJZE PER BIOTA-PUNT
#   1. Alle punten snappen op het netwerk (VHAS-match of geometrisch, zie 3a)
#   2. Kandidaten filteren op (a) zelfde groep (optioneel), (b) Strahler-orde
#      (optioneel), (c) tijdsvenster
#   3. Afstanden berekenen:
#        - graaf-afstand stroomopwaarts (langs de rivier, kandidaat -> biota-punt)
#        - graaf-afstand stroomafwaarts (langs de rivier, biota-punt -> kandidaat)
#   4. Een kandidaat blijft over als:
#        (stroomopwaarts EN graaf-afstand <= max_dist_m)
#        OF (stroomafwaarts EN graaf-afstand <= max_downstream_m)
#      De "rivierafstand" (river_dist_m) is de graaf-afstand (stroomopwaarts of
#      stroomafwaarts, altijd positief). Punten op een zijtak of op een ander
#      segment zonder verbinding in de stroomrichting komen dus NIET meer in aanmerking.
#   5. (Optioneel) kandidaten verwijderen als er een lozingspunt tussen
#      kandidaat en biota-punt ligt
#   6. Selectie volgens `selection_mode`
#
# PARAMETERS
#   biota_sf          sf-punten waaraan je wil koppelen (bv. MI)
#   quality_sf        sf-punten met de te koppelen variabelen (bv. nutriënten)
#   network_list      uitvoer van build_river_network_auto_nodes()
#   discharge_sf      optioneel: sf-punten van lozingen (bv. hpunt). Een kandidaat
#                     wordt geweigerd als er een lozing tússen kandidaat en
#                     biota-punt ligt. NULL = geen controle.
#   strahler_sf       sf-lijnen met Strahler-orde (nodig als use_strahler = TRUE)
#   use_strahler      TRUE: orde van biota- en kwaliteitspunt mag max 1 verschillen
#   strahler_col      kolomnaam van de orde in strahler_sf
#   max_dist_m        max afstand (m) langs de rivier, stroomopwaarts
#   max_downstream_m  max afstand (m) langs de rivier, STROOMAFWAARTS
#   snap_tolerance    max afstand (m) waarover een punt zonder VHAS-match nog
#                     op het netwerk gesnapt wordt (ook voor Strahler/lozingen)
#   days_before       kwaliteitsmeting mag tot zoveel dagen VOOR het biota-staal
#   days_after        kwaliteitsmeting mag tot zoveel dagen NA het biota-staal
#   col_date_biota / col_date_quality   datumkolommen
#   selection_mode    "closest_distance": kleinste rivierafstand (bij gelijk: kleinste tijdsverschil)
#                     "closest_time"    : kleinste tijdsverschil (bij gelijk: kleinste afstand)
#                     "all"             : alle kandidaten (1 rij per match, biota-rij wordt herhaald)
#                     "aggregate"       : 1 rij met gemiddelde/min/max van alle kandidaten
#   aggr_method       "mean", "min" of "max" (enkel bij "aggregate")
#   aggr_cols         welke kolommen samengevat worden (enkel bij "aggregate");
#                     NULL = alle numerieke kolommen (let op met numerieke ID's)
#   grouping_col      kolom in het netwerk (bv. "VHAG"); biota en kwaliteit moeten
#                     tot dezelfde groep behoren. NULL = geen groepsfilter.
#   vhas_col_network  VHAS-kolom in het netwerk
#   vhas_col_biota / vhas_col_quality   VHAS-kolom in de puntenlagen (NULL = enkel geometrisch snappen)
#   keep_time_diff    TRUE: bewaar qual_temp_diff_days (biota minus kwaliteit, in dagen;
#                     positief = kwaliteitsmeting lag VOOR het biota-staal). Bij
#                     "aggregate" is dit het gemiddelde over alle kandidaten.
#
# OUTPUT
#   sf-object = biota_sf + kolommen van de gekoppelde kwaliteitsmeting, allemaal
#   met prefix "qual_" (bv. qual_n_t, qual_meetplaats, qual_monsternamedatum), plus:
#     qual_river_dist_m   gebruikte afstand (m)
#     qual_n_matches      aantal kandidaten dat voldeed (0 = geen match)
#     qual_temp_diff_days tijdsverschil in dagen (enkel als keep_time_diff = TRUE)
#   Biota-punten zonder match hebben NA in alle qual_-kolommen.

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
  # 3.1 Validatie van de input
  # ---------------------------------------------------------------------------
  valid_modes <- c("closest_distance", "closest_time", "all", "aggregate")
  if (!selection_mode %in% valid_modes) {
    stop("selection_mode moet zijn: 'closest_distance', 'closest_time', 'all' of 'aggregate'")
  }

  valid_aggr <- c("mean", "min", "max")
  if (!aggr_method %in% valid_aggr) stop("aggr_method moet zijn: 'mean', 'min' of 'max'")

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

  if (!is.null(grouping_col)) {
    if (!grouping_col %in% names(net_sf)) stop(paste("De grouping_col", grouping_col, "bestaat niet in je netwerk!"))
    message(paste0("Grouping geactiveerd: enkel matchen binnen zelfde '", grouping_col, "'."))
  }

  # ---------------------------------------------------------------------------
  # 3.2 Voorbereiding: datums, hulp-ID's
  # ---------------------------------------------------------------------------
  biota_sf[[col_date_biota]]     <- as.Date(biota_sf[[col_date_biota]])
  quality_sf[[col_date_quality]] <- as.Date(quality_sf[[col_date_quality]])

  biota_sf$tmp_join_id <- seq_len(nrow(biota_sf))       # om achteraf terug te joinen
  quality_sf$orig_index_geo <- seq_len(nrow(quality_sf)) # om geometrie terug te vinden

  # ---------------------------------------------------------------------------
  # 3.3 Snapping op het netwerk (stap 1/5)
  # ---------------------------------------------------------------------------
  message("1/5 Snapping points to river network...")

  # ----------------------------------------
  # Elk punt krijgt een `node_id` (= from_node van het gekozen segment).
  #   1. EERST via VHAS-code: heeft het punt een VHAS, dan wordt het gesnapt op het
  #      dichtstbijzijnde segment MET DEZELFDE VHAS (ongeacht de afstand!).
  #   2. DAARNA (voor punten zonder VHAS of zonder VHAS-match in het netwerk):
  #      geometrisch, dichtstbijzijnde segment, maar enkel als de afstand
  #      <= `tol` meter. Anders valt het punt weg (node_id blijft NA).
  # Als `grp_col` is opgegeven, wordt ook de groepswaarde (bv. VHAG) van het
  # gekozen segment bewaard in `match_group_val`.
  # ----------------------------------------

  snap_and_attribute <- function(points, network, grp_col, tol, vhas_col_pts, vhas_col_net) {
    points$node_id <- NA
    if (!is.null(grp_col)) points$match_group_val <- NA

    # --- 1. VHAS-matching ---
    has_vhas_config <- !is.null(vhas_col_pts) && !is.null(vhas_col_net)
    can_use_vhas <- has_vhas_config &&
      (vhas_col_pts %in% names(points)) &&
      (vhas_col_net %in% names(network))

    if (can_use_vhas) {
      pts_with_vhas_idx <- which(!is.na(points[[vhas_col_pts]]))
      if (length(pts_with_vhas_idx) > 0) {
        unique_codes <- unique(points[[vhas_col_pts]][pts_with_vhas_idx])
        for (code in unique_codes) {
          net_idx <- which(network[[vhas_col_net]] == code)
          if (length(net_idx) > 0) {
            p_subset_idx <- which(points[[vhas_col_pts]] == code)
            rel_nearest  <- st_nearest_feature(points[p_subset_idx, ], network[net_idx, ])
            abs_nearest  <- net_idx[rel_nearest]
            points$node_id[p_subset_idx] <- as.character(network$from_node[abs_nearest])
            if (!is.null(grp_col)) points$match_group_val[p_subset_idx] <- network[[grp_col]][abs_nearest]
          }
        }
      }
    }

    # --- 2. Geometrische fallback ---
    missing_idx <- which(is.na(points$node_id))
    if (length(missing_idx) > 0) {
      subset_pts  <- points[missing_idx, ]
      nearest_idx <- st_nearest_feature(subset_pts, network)
      dists       <- st_distance(subset_pts, network[nearest_idx, ], by_element = TRUE)
      dists_num   <- as.numeric(dists)
      dists_num[is.na(dists_num)] <- Inf

      valid_snaps      <- dists_num <= tol
      valid_global_idx <- missing_idx[valid_snaps]
      valid_net_idx    <- nearest_idx[valid_snaps]

      if (length(valid_global_idx) > 0) {
        points$node_id[valid_global_idx] <- as.character(network$from_node[valid_net_idx])
        if (!is.null(grp_col)) points$match_group_val[valid_global_idx] <- network[[grp_col]][valid_net_idx]
      }

      n_dropped <- length(missing_idx) - length(valid_global_idx)
      if (n_dropped > 0) message(paste("      ->", n_dropped, "points dropped (too far/no VHAS match)."))
    }
    return(points)
  }

  biota_sf   <- snap_and_attribute(biota_sf,   net_sf, grouping_col, snap_tolerance,
                                   vhas_col_biota,   vhas_col_network)
  quality_sf <- snap_and_attribute(quality_sf, net_sf, grouping_col, snap_tolerance,
                                   vhas_col_quality, vhas_col_network)

  # --- Strahler-orde: dichtstbijzijnde Strahler-lijn binnen snap_tolerance ---
  if (use_strahler) {
    message("    -> Determining Strahler order...")
    get_strahler_order <- function(pts, str_layer, str_col, tol) {
      idx       <- st_nearest_feature(pts, str_layer)
      dists     <- st_distance(pts, str_layer[idx, ], by_element = TRUE)
      dists_num <- as.numeric(dists)
      dists_num[is.na(dists_num)] <- Inf
      vals  <- rep(NA, nrow(pts))
      valid <- dists_num <= tol
      vals[valid] <- str_layer[[str_col]][idx[valid]]
      return(vals)
    }
    biota_sf$strahler_val   <- as.numeric(get_strahler_order(biota_sf,   strahler_sf, strahler_col, snap_tolerance))
    quality_sf$strahler_val <- as.numeric(get_strahler_order(quality_sf, strahler_sf, strahler_col, snap_tolerance))
  }

  # Vanaf hier werken we met een tabel zonder geometrie (sneller); de geometrie
  # van quality_sf wordt hier niet meer gebruikt
  quality_df <- st_drop_geometry(quality_sf)

  # Lozingspunten snappen op het netwerk -> vector met node-ID's
  discharge_nodes <- character(0)
  if (!is.null(discharge_sf)) {
    message("    -> Snapping discharge points...")
    idx_dis   <- st_nearest_feature(discharge_sf, net_sf)
    dists_dis <- st_distance(discharge_sf, net_sf[idx_dis, ], by_element = TRUE)
    dists_num <- as.numeric(dists_dis)
    dists_num[is.na(dists_num)] <- Inf
    valid_dis <- dists_num <= snap_tolerance
    if (sum(valid_dis) > 0) {
      discharge_nodes <- unique(as.character(net_sf$from_node[idx_dis[valid_dis]]))
    }
  }

  # Lege rij (alles NA) voor biota-punten zonder match
  empty_match <- function() {
    m <- quality_df[1, ]
    m[] <- NA
    m$river_dist_m <- NA
    m$temp_diff_days <- NA_real_   # zodat rbind met echte matches klopt
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
    b_node <- b_pt$node_id
    b_id   <- b_pt$tmp_join_id
    match  <- NA
    n_matches_found <- 0

    if (is.na(b_node)) {
      # Biota-punt kon niet gesnapt worden -> geen match mogelijk
      match <- empty_match()

    } else {
      b_date <- b_pt[[col_date_biota]]

      # --- STAP A: niet-ruimtelijke filters (gesnapt, groep, Strahler, tijd) ---
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

      # Tijdsverschil = biota - kwaliteit (positief = kwaliteit lag VOOR biota)
      if (nrow(subset_quality) > 0) {
        subset_quality <- subset_quality %>%
          mutate(
            temp_diff_days = as.numeric(difftime(b_date, .data[[col_date_quality]], units = "days")),
            abs_time_diff  = abs(temp_diff_days)
          ) %>%
          filter(temp_diff_days >= -days_after & temp_diff_days <= days_before)
      }

      if (nrow(subset_quality) > 0) {

        # --- STAP B: afstanden langs de graaf (stroomopwaarts + stroomafwaarts) ---

        # Welke nodes liggen stroomopwaarts van het biota-punt?
        upstream_nodes <- names(subcomponent(g, v = b_node, mode = "in"))
        subset_quality$is_upstream <- subset_quality$node_id %in% upstream_nodes

        # Kortste weg langs de rivier van elke kandidaat-node naar het biota-punt
        # (Inf als de kandidaat niet stroomopwaarts ligt). Via een lookup per
        # unieke node om duplicate-vertex-fouten in igraph te vermijden.
        unique_nodes_step2 <- unique(subset_quality$node_id)
        d_matrix <- distances(g, v = unique_nodes_step2, to = b_node, mode = "out")
        lookup_graph_dist <- setNames(as.numeric(d_matrix), unique_nodes_step2)
        subset_quality$graph_dist <- lookup_graph_dist[subset_quality$node_id]

        # Kortste weg langs de rivier van het biota-punt naar elke kandidaat-node
        # (stroomafwaarts; Inf als de kandidaat niet stroomafwaarts ligt)
        d_down <- distances(g, v = b_node, to = unique_nodes_step2, mode = "out")
        lookup_down_dist <- setNames(as.numeric(d_down), unique_nodes_step2)
        subset_quality$down_dist <- lookup_down_dist[subset_quality$node_id]

        # --- STAP C: de "OR"-filter ---
        candidates <- subset_quality %>%
          filter(
            (is_upstream & graph_dist <= max_dist_m) |
              (down_dist <= max_downstream_m)
          )

        if (nrow(candidates) > 0) {

          # Definitieve afstand: graaf-afstand stroomopwaarts ("via_graph") of,
          # zo niet, de graaf-afstand stroomafwaarts.
          candidates$via_graph <- candidates$is_upstream & candidates$graph_dist <= max_dist_m
          candidates$river_dist_m <- ifelse(candidates$via_graph,
                                            candidates$graph_dist,
                                            candidates$down_dist)

          # --- STAP D: lozingscontrole ---
          # Een stroomopwaartse kandidaat vervalt als er een lozingsnode ligt op
          # de weg tussen kandidaat en biota-punt (begin- en eindnode uitgezonderd).
          if (length(discharge_nodes) > 0) {
            to_check_idx <- which(candidates$via_graph & candidates$river_dist_m > 0)

            if (length(to_check_idx) > 0) {
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
              # Eerste kandidaat = basis; gekozen kolommen krijgen mean/min/max van alle
              # kandidaten. Zonder aggr_cols: ALLE numerieke kolommen (behalve technische
              # kolommen en de VHAS-kolom). Geef best expliciet aggr_cols mee.
              if (!is.null(aggr_cols)) {
                cols_to_agg <- aggr_cols
              } else {
                tech_cols <- c("node_id", "river_dist_m", "temp_diff_days", "abs_time_diff",
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
                match[, cols_to_agg] <- agg_values
              }
              match$river_dist_m <- mean(candidates$river_dist_m)
              # tijdsverschil = gemiddelde van alle kandidaten (niet enkel van de eerste)
              if ("temp_diff_days" %in% names(candidates)) {
                match$temp_diff_days <- mean(candidates$temp_diff_days)
              }
            }
          }
        }
      }

      # Geen match gevonden -> lege rij
      if (!is.data.frame(match)) match <- empty_match()
    }

    match$biota_match_id <- b_id
    match$n_matches      <- n_matches_found

    # Technische hulpkolommen verwijderen
    cols_to_remove <- c("abs_time_diff", "match_group_val", "strahler_val",
                        "orig_index_geo", "graph_dist", "down_dist", "is_upstream", "via_graph")
    if (!keep_time_diff) cols_to_remove <- c(cols_to_remove, "temp_diff_days")
    match[, base::intersect(names(match), cols_to_remove)] <- NULL
    results[[i]] <- match
  }

  # ---------------------------------------------------------------------------
  # 3.5 Resultaten samenvoegen (stap 3/5)
  # ---------------------------------------------------------------------------
  message("3/5 Merging results...")
  matched_quality_df <- do.call(rbind, results)

  # Alle kwaliteitskolommen krijgen prefix "qual_"
  cols_to_rename <- setdiff(names(matched_quality_df), "biota_match_id")
  names(matched_quality_df)[names(matched_quality_df) %in% cols_to_rename] <-
    paste0("qual_", cols_to_rename)

  final_sf <- left_join(biota_sf, matched_quality_df, by = c("tmp_join_id" = "biota_match_id"))

  final_sf$tmp_join_id <- NULL
  if ("match_group_val" %in% names(final_sf)) final_sf$match_group_val <- NULL
  if ("strahler_val"    %in% names(final_sf)) final_sf$strahler_val    <- NULL

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
koppeling_mi_nutrient <- match_upstream_strahler(
  biota_sf         = mi_data,
  quality_sf       = nutrient_data,
  network_list     = river_network_fine,
  use_strahler     = FALSE,
  max_downstream_m = 200,
  max_dist_m       = 5000,
  days_before      = 180,    # kwaliteit mag tot 180 dagen VOOR de biota-meting liggen
  days_after       = 14,     # kwaliteit mag tot 14 dagen NA de biota-meting liggen
  col_date_biota   = "monsternamedatum",
  col_date_quality = "monsternamedatum",
  selection_mode   = "closest_distance",
  grouping_col     = "VHAG",
  vhas_col_network = "VHAS",
  vhas_col_biota   = "vhas",
  vhas_col_quality = "vhas"
)

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
