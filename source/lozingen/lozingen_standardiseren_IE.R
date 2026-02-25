source(here::here("source", "inladen_packages.R"))

# data en functies inlezen



fd <- st_read(here("data", "ruw", "netwerk", "Flow_direction_coordinates.shp"), quiet = T) %>%
  mutate(VHAS = as.character(VHAS),
         VHAG = as.character(VHAG))
nodes <- st_read(here("data", "ruw", "waterlopen", "vha_network_junctions.shp"), quiet = T)

meetnetten <- read.delim("data/ruw/vmm/meetnetten.txt")

mi_sf <- st_read(here("data", "ruw", "macroinvertebraten", "mi_meetpunten_datum.gpkg"), quiet = T) %>%
  filter(monsternamedatum > '2009-12-31') %>%
  left_join(meetnetten %>%
              select(vhas, nummer),
            by = c("meetplaats" = "nummer")) %>%
  mutate(jaar = lubridate::year(monsternamedatum),
         vhas = as.character(vhas))

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

# finer river network
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


##########################################
# Industie lozingen niet op RWZI direct in oppervlaktewater
##########################################

# eerste stap zet concentraties stoffen om in IE volgens factor. vervolgens worden lozingen binnen 5km stroomopwaarts gekoppeld aan biotapunten en wordt een gewogen som berekend voor IE op basis van een distance decay functie (in te stellen). Dit voor de hoogste IE van de 5 omgezette stoffen -> maxIE benadering. Koppeling data lozing en biota in zelfde jaar.

industrie <- read_excel("data/ruw/lozingen/industrie/AW_Modellering_Vrachten Industrie T08_54_18.xlsx") %>%
  janitor::clean_names()
# %>%
#   select(meetput_nummer, pdts_jaar, meetput_lozingswijze, hydraulisch_punt_x_coordinaat, hydraulisch_punt_y_coordinaat,
#          vha_gewestelijke_waterloop_code, vha_waterlichaam_code)

# 1. Definieer de omzettingsfactoren (gram per IE per dag)
ie_normen <- c(
  "ZS"   = 90,
  "CZV"  = 135,
  "BZV5" = 60,
  "N t"  = 10,
  "P t"  = 2
)

industrie_ie_resultaat <- industrie %>%
  # Filter op de relevante lozingswijze en parameters
  filter(meetput_lozingswijze == "OW DIR") %>%
  # STAP 0: Verwijder meetpunten waarvoor GEEN ENKELE bruikbare vracht-data is
  group_by(meetput_nummer, pdts_jaar) %>%
  filter(!all(is.na(pdts_netto_vracht_og_pdts_kalenderdagen))) %>%
  ungroup() %>%
  filter(parameter_symbool %in% names(ie_normen)) %>%

  # Stap 1: Omzetten van kg naar gram en IE berekenen per rij (stof)
  mutate(
    vracht_g_dag = pdts_netto_vracht_og_pdts_kalenderdagen * 1000, # van kg totaal naar g/dag
    ie_per_stof = vracht_g_dag / ie_normen[parameter_symbool]
  ) %>%

  # Stap 2: Bereken de Max en Gemiddelde IE per meetput
  group_by(meetput_nummer, pdts_jaar) %>%
  mutate(
    max_ie_meetpunt = max(ie_per_stof, na.rm = TRUE),
    parameter_max_ie = parameter_symbool[which.max(ie_per_stof)],
    gemiddelde_ie_meetpunt = mean(ie_per_stof, na.rm = TRUE)
  ) %>%
  ungroup()

############################################################################
# RWZI effluent direct in oppervlaktewater
############################################################################

#idem als voor industrie lozingen -> rwzi bevat effluent van huishoudens en industrie die hierop is aangesloten. Ook apart concentratie P naast max IE.

rwzi <- read_excel("data/ruw/lozingen/RWZI en riool/AW_Modellering_Vrachten RWZI T09_20_51.xlsx") %>%
  janitor::clean_names()

rwzi_ie_resultaat <- rwzi %>%
  # Basis filters
  filter(meetput_influent_effluent_andere == "Effluent") %>%
  filter(meetput_lozingswijze == "OW DIR") %>%

  # Stap 0: Verwijder meetpunten/jaren zonder data
  group_by(meetput_nummer) %>%
  filter(!all(is.na(pdts_netto_vracht_og_pdts_kalenderdagen))) %>%
  ungroup() %>%

  # We hebben de 5 IE-parameters nodig + P t voor de concentratie
  filter(parameter_symbool %in% c(names(ie_normen), "P t")) %>% # fosfor is zogezegd voor rwzi meest problematisch

  # Stap 1: IE berekenen (alleen voor de relevante parameters)
  mutate(
    # We berekenen IE enkel als de parameter in onze normen-lijst staat
    vracht_g_dag = pdts_netto_vracht_og_pdts_kalenderdagen * 1000,
    ie_per_stof = ifelse(parameter_symbool %in% names(ie_normen),
                         vracht_g_dag / ie_normen[parameter_symbool],
                         NA_real_)
  ) %>%

  # Stap 2: Statistieken en P t concentratie per meetpunt per jaar
  group_by(meetput_nummer, pdts_jaar) %>%
  mutate(
    # Max IE (limiterende parameter)
    max_ie_meetpunt = max(ie_per_stof, na.rm = TRUE),

    # Welke parameter was de max?
    parameter_max_ie = ifelse(all(is.na(ie_per_stof)),
                              NA_character_,
                              parameter_symbool[which.max(ie_per_stof)]),

    # Gemiddelde IE
    gemiddelde_ie_meetpunt = mean(ie_per_stof, na.rm = TRUE),

    # De concentratie van P t ophalen uit de vrachtkolom
    # (indien er meerdere metingen zijn nemen we het gemiddelde van dat jaar)
    concentratie_P_t = mean(pdts_netto_vracht_og_pdts_kalenderdagen[parameter_symbool == "P t"], na.rm = TRUE)
  ) %>%
  ungroup() %>%

  # Stap 3: Opschonen
  # We behouden nu enkel de rijen van de IE-parameters,
  # de P-concentratie staat nu in een aparte kolom op elke rij.
  filter(parameter_symbool %in% names(ie_normen))

############################################################################
# Uitlaten riool oppervlaktewater
############################################################################
# enkel voor 2018, 19, 20, 21

# 2 opties hier: op basis van zuiveringsgraad Vlaanderen terugrekenen tot 2010 (heel ruw). Of uitgaan van jaar 2018.

# 1. Maak een lijst van de bestanden (paden naar je data)
files <- list(
  "2018" = "data/ruw/lozingen/RWZI en riool/20181210_AWIS_AIW_PEGASE_Vuilvracht Hpunten per zuiveringsgebied Datumprompt.xlsx",
  "2019" = "data/ruw/lozingen/RWZI en riool/20190626_AWIS_AIW_PEGASE_Vuilvracht Hpunten per zuiveringsgebied Datumprompt.xlsx",
  "2020" = "data/ruw/lozingen/RWZI en riool/20200116_AWIS_AIW_PEGASE_Vuilvracht Hpunten per zuiveringsgebied Datumprompt.xlsx",
  "2021" = "data/ruw/lozingen/RWZI en riool/20210101_AWIS_AIW_PEGASE_Vuilvracht Hpunten per zuiveringsgebied Datumprompt.xlsx"
)

# 2. Lees ze in, voeg het jaar toe en plak ze samen
riool_totaal <- map_df(files, ~ read_excel(.x, sheet = 1, skip = 6) %>% janitor::clean_names(), .id = "jaar")

# 3. Voer nu je filters en selecties uit op de volledige dataset
riool_ie_resultaat <- riool_totaal %>%
  filter(uitlaat_type_code == "VUIL") %>%
  rename(ie_meetput = inwoners_uitlaat) %>%
  select(jaar, h_punt_nummer, ie_meetput, uitlaat_vha_x_coord, uitlaat_vha_y_coord, vha_gewestelijke_waterloop_code, vha_segment_code) %>%
  mutate(jaar = as.numeric(jaar))

# waarden voor zuiveringsgraad per jaar. Hier zou ik meer gedetailleerde waarden ruimtelijk willen.
zuiveringsgraad_vlaanderen <- tribble(
  ~jaar, ~percentage,
  2010,  73.59,
  2011,  75.48,
  2012,  77.22,
  2013,  78.94,
  2014,  80.2,
  2015,  81.16,
  2016,  82.35,
  2017,  83.16,
  2018,  83.54,
  2019,  84.04,
  2020,  84.04, # nemen hetzelfde als 2019
  2021,  85.45,
  2022,  86.03,
  2023,  88,
  2024,  88
)

riool_proxy_2021 <- riool_ie_resultaat %>%
  filter(jaar == 2021) %>%
  # We groeperen voor de zekerheid op h_punt_nummer
  # (mochten er dubbele rijen zijn in de broncode)
  group_by(h_punt_nummer) %>%
  summarise(
    ie_meetput = max(ie_meetput, na.rm = TRUE),
    x = dplyr::first(uitlaat_vha_x_coord),
    y = dplyr::first(uitlaat_vha_y_coord),
    vhag = dplyr::first(vha_gewestelijke_waterloop_code),
    vhas = dplyr::first(vha_segment_code),
    .groups = "drop"
  )

# Haal het percentage van 2021 op als referentie
perc_2021 <- zuiveringsgraad_vlaanderen %>%
  filter(jaar == 2021) %>%
  pull(percentage)

riool_dynamisch_sf <- riool_proxy_2021 %>%
  # Koppel alle jaren aan de 2021-data
  tidyr::crossing(zuiveringsgraad_vlaanderen) %>%
  # Bereken de geschaalde IE
  # We berekenen de factor: (onbehandeld deel jaar X) / (onbehandeld deel 2021)
  mutate(
    schaal_factor = (100 - percentage) / (100 - perc_2021),
    ie_dynamisch = ie_meetput * schaal_factor
  ) %>%
  # Maak het klaar voor de functie
  select(h_punt_nummer, jaar, ie_waarde = ie_dynamisch, x, y, vhag, vhas) %>%
  st_as_sf(coords = c("x", "y"), crs = 31370)

### 2. optie door enkel 2018 te gebruiken over alle jaren

# Stap 1: Isoleer het basisjaar 2018
riool_proxy_2018 <- riool_ie_resultaat %>%
  filter(jaar == 2018) %>%
  group_by(h_punt_nummer) %>%
  summarise(
    ie_meetput = max(ie_meetput, na.rm = TRUE),
    x = dplyr::first(uitlaat_vha_x_coord),
    y = dplyr::first(uitlaat_vha_y_coord),
    vhag = dplyr::first(vha_gewestelijke_waterloop_code),
    vhas = dplyr::first(vha_segment_code),
    .groups = "drop"
  )

# Stap 2: Expand naar de volledige tijdreeks van de mi_sf (zoals bij overstorten)
jaren_range <- unique(mi_sf$jaar)

riool_timeseries_sf <- riool_proxy_2018 %>%
  tidyr::crossing(jaar = jaren_range) %>%
  # We hernoemen ie_meetput naar ie_waarde voor consistentie met je aanroep
  rename(ie_waarde = ie_meetput) %>%
  st_as_sf(coords = c("x", "y"), crs = 31370)

############################################################################
# Koppeling mi punten en lozingspunten
############################################################################

industrie_sf <- industrie_ie_resultaat %>%
  select(meetput_nummer, pdts_jaar, hydraulisch_punt_x_coordinaat, hydraulisch_punt_y_coordinaat, vha_gewestelijke_waterloop_code, vha_waterlichaam_code, max_ie_meetpunt, gemiddelde_ie_meetpunt) %>%
  rename(jaar = pdts_jaar,
         vhag = vha_gewestelijke_waterloop_code,
         owl = vha_waterlichaam_code) %>%
  group_by(meetput_nummer, jaar, vhag, owl, hydraulisch_punt_x_coordinaat, hydraulisch_punt_y_coordinaat) %>%
  summarise(max_ie_meetpunt = mean(max_ie_meetpunt),
            gemiddelde_ie_meetpunt = mean(gemiddelde_ie_meetpunt)) %>%
  st_as_sf(coords = c("hydraulisch_punt_x_coordinaat", "hydraulisch_punt_y_coordinaat"), crs = 31370) %>%
  select(meetput_nummer, jaar, max_ie_meetpunt, vhag) %>%
  mutate(jaar = as.numeric(jaar)) %>%
  ungroup

rwzi_sf <- rwzi_ie_resultaat %>%
  select(meetput_nummer, pdts_jaar, hydraulisch_punt_x_coordinaat, hydraulisch_punt_y_coordinaat, vha_gewestelijke_waterloop_code, vha_waterlichaam_code, max_ie_meetpunt, gemiddelde_ie_meetpunt, concentratie_P_t) %>%
  rename(jaar = pdts_jaar,
         vhag = vha_gewestelijke_waterloop_code,
         owl = vha_waterlichaam_code) %>%
  group_by(meetput_nummer, jaar, vhag, owl, hydraulisch_punt_x_coordinaat, hydraulisch_punt_y_coordinaat) %>%
  summarise(max_ie_meetpunt = mean(max_ie_meetpunt),
            gemiddelde_ie_meetpunt = mean(gemiddelde_ie_meetpunt),
            concentratie_P_t = mean(concentratie_P_t)) %>%
  st_as_sf(coords = c("hydraulisch_punt_x_coordinaat", "hydraulisch_punt_y_coordinaat"), crs = 31370) %>%
  select(meetput_nummer, jaar, vhag, max_ie_meetpunt, concentratie_P_t) %>%
  mutate(jaar = as.numeric(jaar)) %>%
  ungroup

# de punten voor rioollozingen liggen ver van waterlopen -> functie om te snappen op VHAG. Biota snappen op VHAS
calculate_upstream_pressure <- function(biota_sf,
                                                  discharge_sf,
                                                  network_list,
                                                  max_dist_m = 5000,
                                                  decay_distance_km = 2,
                                                  backup_tol = 25, # tolerantie als niet kan gesnapt worden op vhas of vhag
                                                  col_biota_snap = "vhas",
                                                  col_net_biota = "VHAS",
                                                  col_discharge_snap = "vhag",
                                                  col_net_discharge = "VHAG",
                                                  col_year_biota = "jaar",
                                                  col_year_discharge = "jaar",
                                                  col_ie_value = "max_ie_meetpunt") {
  require(igraph)
  require(dplyr)
  require(sf)
  require(progress)

  message("--- Start Berekening Lozingsdruk (Flexibele Match + Backup Snap) ---")

  g <- network_list$graph
  net_sf <- network_list$network_sf

  # --- Geoptimaliseerde Helper met Backup Snapping ---
  snap_to_node_hybrid <- function(pts, net, pt_col, net_col, label, tol) {
    message(paste(" -> Snappen", label, "..."))

    # 1. Matching op CODE
    net_lookup <- net %>%
      st_drop_geometry() %>%
      filter(!is.na(!!sym(net_col))) %>%
      group_by(!!sym(net_col)) %>%
      summarise(node_id_match = dplyr::first(from_node),
                .groups = "drop") %>%
      mutate(across(everything(), as.character))

    pts_mapped <- pts %>%
      mutate(join_key = as.character(!!sym(pt_col))) %>%
      left_join(net_lookup, by = setNames(net_col, "join_key")) %>%
      rename(node_id = node_id_match)

    n_code <- sum(!is.na(pts_mapped$node_id))
    message(paste("    -", n_code, "via code gekoppeld."))

    # 2. Backup: GEOMETRISCHE snap voor de overblijvers
    missing_idx <- which(is.na(pts_mapped$node_id))

    if (length(missing_idx) > 0) {
      pts_missing <- pts_mapped[missing_idx, ]

      # Vind dichtstbijzijnde segment in het hele netwerk
      nearest_idx <- st_nearest_feature(pts_missing, net)
      dists <- st_distance(pts_missing, net[nearest_idx, ], by_element = TRUE)
      dists_num <- as.numeric(dists)

      # Alleen snappen als binnen de backup tolerance
      valid_backup <- dists_num <= tol

      if (any(valid_backup)) {
        # Wijs de from_node van het dichtstbijzijnde segment toe
        pts_mapped$node_id[missing_idx[valid_backup]] <- as.character(net$from_node[nearest_idx[valid_backup]])
        message(paste(
          "    -",
          sum(valid_backup),
          "extra via backup snap (",
          tol,
          "m) gekoppeld."
        ))
      }
    }

    n_final <- sum(!is.na(pts_mapped$node_id))
    if (n_final < nrow(pts))
      message(paste("    - LET OP:", nrow(pts) - n_final, "punten niet gekoppeld."))

    return(pts_mapped %>% filter(!is.na(node_id)))
  }

  # 1/4 Snappen
  message("1/4 Punten koppelen aan netwerk nodes...")

  biota_snapped <- snap_to_node_hybrid(biota_sf,
                                       net_sf,
                                       col_biota_snap,
                                       col_net_biota,
                                       "Biota",
                                       backup_tol)
  discharge_snapped <- snap_to_node_hybrid(
    discharge_sf,
    net_sf,
    col_discharge_snap,
    col_net_discharge,
    "Lozingen",
    backup_tol
  )

  # Voorbereiden kolommen voor de loop
  biota_snapped$match_year <- biota_snapped[[col_year_biota]]
  discharge_snapped$match_year <- discharge_snapped[[col_year_discharge]]
  discharge_snapped$ie_val <- discharge_snapped[[col_ie_value]]

  results_list <- vector("list", nrow(biota_snapped))

  message(
    paste0(
      "2/4 Berekenen upstream impact (Max: ",
      max_dist_m,
      "m, Decay: ",
      decay_distance_km,
      "km)..."
    )
  )
  pb <- progress_bar$new(
    format = "  [:bar] :percent eta: :eta",
    total = nrow(biota_snapped),
    clear = FALSE,
    width = 60
  )

  for (i in seq_len(nrow(biota_snapped))) {
    pb$tick()

    b_pt <- biota_snapped[i, ]
    b_node <- b_pt$node_id
    b_year <- b_pt$match_year

    res_row <- data.frame(
      sum_ie_raw = 0,
      sum_ie_weighted = 0,
      count_sources = 0,
      avg_dist_sources = NA
    )

    if (!is.na(b_node)) {
      # igraph subcomponent voor upstream nodes
      upstream_nodes <- names(subcomponent(g, v = b_node, mode = "in"))

      potential_discharges <- discharge_snapped %>%
        filter(node_id %in% upstream_nodes, match_year == b_year)

      if (nrow(potential_discharges) > 0) {
        # Graafafstanden berekenen
        dists_matrix <- distances(g,
                                  v = potential_discharges$node_id,
                                  to = b_node,
                                  mode = "out")
        potential_discharges$river_dist <- as.numeric(dists_matrix)

        valid_discharges <- potential_discharges %>%
          filter(river_dist <= max_dist_m) %>%
          mutate(weight_factor = 1 / (1 + (
            river_dist / (decay_distance_km * 1000)
          )),
          ie_weighted = ie_val * weight_factor,
          count_weighted_val = 1 * weight_factor)

        if (nrow(valid_discharges) > 0) {
          res_row$sum_ie_raw <- sum(valid_discharges$ie_val, na.rm = TRUE)
          res_row$sum_ie_weighted <- sum(valid_discharges$ie_weighted, na.rm = TRUE)
          res_row$count_sources <- nrow(valid_discharges)
          res_row$count_weighted <- sum(valid_discharges$count_weighted_val, na.rm = TRUE)
          res_row$avg_dist_sources <- mean(valid_discharges$river_dist, na.rm = TRUE)
        }
      }
    }
    results_list[[i]] <- res_row
  }

  message("3/4 Resultaten samenvoegen...")
  results_df <- bind_rows(results_list)
  final_sf <- bind_cols(biota_snapped, results_df)

  final_sf <- final_sf %>%
    select(-any_of(
      c("match_year", "node_id", "ie_val", "tmp_join_id", "join_key")
    ))

  message("4/4 Berekening voltooid.")
  return(final_sf)
}

# 1. Bereken druk van Industrie

mi_met_industrie_druk <- calculate_upstream_pressure(
  biota_sf = mi_sf,
  discharge_sf = industrie_sf, # dataframe met max_ie en coords
  network_list = river_network_fine,
  decay_distance_km = 1, # bij 1 km halfwaarde
  max_dist_m = 5000,          # Kijk tot 5km stroomopwaarts
  col_ie_value = "max_ie_meetpunt",
  col_year_discharge = "jaar",
  col_biota_snap = "vhas",      # Kolom in mi_sf
  col_net_biota = "VHAS",       # Kolom in netwerk voor biota
  col_discharge_snap = "vhag",  # Kolom in discharge_sf
  col_net_discharge = "VHAG"   # Kolom in netwerk voor lozingen# Zorg dat je industrie data een kolom 'jaar' heeft
) %>%
  rename(lozingen_industrie_ie = sum_ie_weighted,
         count_ind = count_sources)

# 2.1 Bereken druk van RWZI
mi_met_rwzi_druk <- calculate_upstream_pressure(
  biota_sf = mi_sf,
  discharge_sf = rwzi_sf,
  network_list = river_network_fine,
  max_dist_m = 5000,
  decay_distance_km = 2, # hier grotere decay afstand want continuere lozing
  col_ie_value = "max_ie_meetpunt",
  col_year_discharge = "jaar",
  col_biota_snap = "vhas",      # Kolom in mi_sf
  col_net_biota = "VHAS",       # Kolom in netwerk voor biota
  col_discharge_snap = "vhag",  # Kolom in discharge_sf
  col_net_discharge = "VHAG"
) %>%     # Geometry hoef je niet dubbel
  select(meetplaats, monsternamedatum, lozingen_rwzi_ie = sum_ie_weighted)

# 2.2 Bereken concentratie van fosfor uit rwzi
mi_met_rwzi_druk_p_t <- calculate_upstream_pressure(
  biota_sf = mi_sf,
  discharge_sf = rwzi_sf,
  network_list = river_network_fine,
  max_dist_m = 5000,
  decay_distance_km = 2,
  col_ie_value = "concentratie_P_t",
  col_year_discharge = "jaar",
  col_biota_snap = "vhas",      # Kolom in mi_sf
  col_net_biota = "VHAS",       # Kolom in netwerk voor biota
  col_discharge_snap = "vhag",  # Kolom in discharge_sf
  col_net_discharge = "VHAG"
) %>%     # Geometry hoef je niet dubbel
  select(meetplaats, monsternamedatum, lozingen_rwzi_p_t = sum_ie_weighted)

# 3. Bereken druk van RIOOL op basis van zuiveringsbenadering
mi_met_riool_druk <- calculate_upstream_pressure(
  biota_sf = mi_sf,
  discharge_sf = riool_dynamisch_sf,
  network_list = river_network_fine,
  max_dist_m = 5000,
  decay_distance_km = 2,
  col_ie_value = "ie_waarde",
  col_year_discharge = "jaar",
  col_biota_snap = "vhas",      # Kolom in mi_sf
  col_net_biota = "VHAS",       # Kolom in netwerk voor biota
  col_discharge_snap = "vhas",  # Kolom in discharge_sf
  col_net_discharge = "VHAS"   # Kolom in netwerk voor lozingen
) %>%     # Geometry hoef je niet dubbel
  select(meetplaats, monsternamedatum, lozingen_riool_ie = sum_ie_weighted)

# 4. Bereken druk van RIOOL (op basis van statisch jaar 2018)
mi_met_riool_druk_2018 <- calculate_upstream_pressure(
  biota_sf = mi_sf,
  discharge_sf = riool_timeseries_sf, # Gebruik de nieuwe statische tijdreeks
  network_list = river_network_fine,
  max_dist_m = 5000,
  decay_distance_km = 2,           # Dezelfde decay als voorheen
  col_ie_value = "ie_waarde",
  col_year_discharge = "jaar",
  col_biota_snap = "vhas",         # Kolom in mi_data
  col_net_biota = "VHAS",          # Kolom in netwerk
  col_discharge_snap = "vhas",     # Rioolpunten snappen hier op VHAS (of VHAG indien gewenst)
  col_net_discharge = "VHAS"
) %>%
  select(meetplaats, monsternamedatum, lozingen_riool_ie_conservative = sum_ie_weighted)


#####################################################################
# Overstorten: koppeling en berekening overstortindex per MI meetpunt met graaf
#####################################################################

overstorten_uitlaat_vha_prio_score <- st_read(here("data", "verwerkt", "overstorten", "overstorten_uitlaat_vha_prio_score.gpkg"), quiet = T) # overstorten die uitlaten in vhag met een score voor blootsteling of risk (blootstelling x waarschijnlijkheid)

# STAP 1: Bereid de overstort data voor
overstort_sf <- overstorten_uitlaat_vha_prio_score %>%
  select(overstort_risk_score, Blootstellingsfactor, vha_segm) %>%
  rename(vhas = vha_segm)

# STAP 2: Maak de data "temporeel" (expand to full time range)
# We maken een grid van alle jaren die in je MI-data zitten
jaren_range <- unique(mi_sf$jaar) # assumptie over de tijd

overstort_timeseries <- overstort_sf %>%
  # Dit dupliceert elke overstort voor elk jaar in de lijst
  tidyr::crossing(jaar = jaren_range) %>%
  st_as_sf() # Zorg dat het een sf object blijft

# STAP 3: Draai de functie met een KORTERE decay distance

mi_met_overstort_druk <- calculate_upstream_pressure(
  biota_sf = mi_sf,
  discharge_sf = overstort_timeseries,
  network_list = river_network_fine,
  max_dist_m = 5000,
  decay_distance_km = 0.2, # veel kleinere decay afstand 200m
  col_ie_value = "overstort_risk_score",
  col_year_discharge = "jaar",
  col_biota_snap = "vhas",      # Kolom in mi_sf
  col_net_biota = "VHAS",       # Kolom in netwerk voor biota
  col_discharge_snap = "vhas",  # Kolom in discharge_sf
  col_net_discharge = "VHAS"   # Kolom in netwerk voor lozingen
) %>%
  rename(overstorten_index = sum_ie_weighted,
         count_cso = count_sources)

mi_met_overstort_druk_blootstelling <- calculate_upstream_pressure(
  biota_sf = mi_sf,
  discharge_sf = overstort_timeseries,
  network_list = river_network_fine,
  max_dist_m = 5000,
  decay_distance_km = 0.2,
  col_ie_value = "Blootstellingsfactor",
  col_year_discharge = "jaar",
  col_biota_snap = "vhas",      # Kolom in mi_sf
  col_net_biota = "VHAS",       # Kolom in netwerk voor biota
  col_discharge_snap = "vhas",  # Kolom in discharge_sf
  col_net_discharge = "VHAS"   # Kolom in netwerk voor lozingen
) %>%
  rename(overstorten_blootstelling_index = sum_ie_weighted,
         count_cso_weighted = count_sources)

###########################################
# Data joinen voor SEM
#############################################

lozingen_data <- mi_met_industrie_druk %>%
  st_drop_geometry() %>%
  left_join(mi_met_rwzi_druk,
            by = c("meetplaats", "monsternamedatum")) %>%
  left_join(mi_met_rwzi_druk_p_t,
            by = c("meetplaats", "monsternamedatum")) %>%
  left_join(mi_met_overstort_druk %>% st_drop_geometry(),
            by = c("meetplaats", "monsternamedatum")) %>%
  left_join(mi_met_riool_druk %>% st_drop_geometry(),
            by = c("meetplaats", "monsternamedatum")) %>%
  left_join( mi_met_riool_druk_2018 %>% st_drop_geometry(),
            by = c("meetplaats", "monsternamedatum")) %>%
  left_join(mi_met_overstort_druk_blootstelling,
            by = c("meetplaats", "monsternamedatum")) %>%
  select(meetplaats, monsternamedatum, lozingen_industrie_ie, lozingen_rwzi_ie, lozingen_rwzi_p_t,
         overstorten_index, overstorten_blootstelling_index, lozingen_riool_ie, lozingen_riool_ie_conservative, aantal_overstorten_weighted)

save(lozingen_data , file = here("data", "verwerkt", "lozingen_data.rdata"))
