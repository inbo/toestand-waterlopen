library(sf)
library(terra)
library(whitebox)  # for hydrology functions
library(igraph)    # for graph-based connectivity
library(tidyverse)
library(here)
library(qgisprocess)
library(sf)
library(nngeo)
library(igraph)
library(lubridate)
library(mapview)


  fd <- st_read(here("data", "ruw", "netwerk", "Flow_direction_coordinates.shp"))
  nodes <- st_read(here("data", "ruw", "waterlopen", "vha_network_junctions.shp"))

  # Calculate from_node and to_node based on Start and End coordinates
  # Convert the Start and End coordinates into POINT geometries
  start_points <- st_as_sf(data.frame(
    x = fd$StartX,
    y = fd$StartY
  ), coords = c("x", "y"), crs = st_crs(fd))

  end_points <- st_as_sf(data.frame(
    x = fd$EndX,
    y = fd$EndY
  ), coords = c("x", "y"), crs = st_crs(fd))

  # Find the nearest network node for start and end points
  fd$from_node <- st_nearest_feature(start_points, nodes)
  fd$to_node <- st_nearest_feature(end_points, nodes)

  # Assign flow direction based on these nodes (optional: store as string)
  fd$flow_dir <- "start_to_end"  # All flows are from Start -> End as defined in coords


  # find nearest fc point for mi point in space upstream and in time ----

  # Load your data (adjust paths)
  mi_meetpunten_datum <- st_read(here("data", "ruw", "macroinvertebraten", "mi_meetpunten_datum.gpkg"), quiet = T) %>%
    mutate(monsternamedatum = as.Date(monsternamedatum, "%Y-%m-%d")) %>%
    filter(monsternamedatum > '2009-12-31')

  # filteren samplenames voor stikstof (n_t)
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
  nutrient_meetpunten_datum <- st_read(dsn = here("data", "verwerkt", "koppeling", "nutrient_meetpunten_datum.gpkg"), quiet = T)

  mi_meetpunten_meetplaats <- mi_meetpunten_datum %>% #enkel de meetplaatsen, geen data
    select(meetplaats, geom) %>%
    unique()

  nutrient_meetpunten_meetplaats <- nutrient_meetpunten_datum %>% #enkel de meetplaatsen, geen data
    select(meetplaats, geom) %>%
    unique()

  # Convert dates if necessary
  mi_meetpunten_datum$monsternamedatum <- as.Date(mi_meetpunten_datum$monsternamedatum)
  nutrient_meetpunten_datum$monsternamedatum <- as.Date(nutrient_meetpunten_datum$monsternamedatum)

  # Snap mi points to river lines

  qgis_run_algorithm(
    "native:snapgeometries",
    INPUT = mi_meetpunten_datum,
    TOLERANCE = 100,
    REFERENCE_LAYER = fd,
    OUTPUT = here("data", "verwerkt", "hydrologisch", "mi_snapped.gpkg"),
    .quiet = TRUE
  )

  mi_snapped <- st_read(here("data", "verwerkt", "hydrologisch", "mi_snapped.gpkg"))
  # %>%
  #   st_join(., mi_meetpunten %>%
  #             select(meetplaats, monsternamedatum),
  #           by = "meetplaats")



  # Snap quality points to river lines

  qgis_run_algorithm(
    "native:snapgeometries",
    INPUT = nutrient_meetpunten_meetplaats,
    TOLERANCE = 100,
    REFERENCE_LAYER = fd,
    OUTPUT = here("data", "verwerkt", "hydrologisch", "nutrient_snapped.gpkg"),
    .quiet = TRUE
  )
  nutrient_snapped_meetplaats <- st_read(here("data", "verwerkt", "hydrologisch", "nutrient_snapped.gpkg"))

  nutrient_no_geom <- nutrient_meetpunten_datum %>%
    st_drop_geometry()

  nutrient_snapped <- nutrient_snapped_meetplaats %>% #enkel meetplaatsen snappen om dan terug datums toe te voegen om tijd te besparen
    left_join(., nutrient_no_geom, by = "meetplaats")

  #plotten
  # mapview(mi_snapped, color = "red", legend = FALSE) +
  #   # mapview(mi_meetpunten_meetplaats, color = "green", legend = F) +
  #   mapview(nutrient_snapped_meetplaats, color = "yellow", legend = F) +
  #   mapview(nodes, color = "blue", legend = F) +
  #   mapview(st_simplify(fd))

  # Assign each snapped point to nearest river segment
  mi_snapped$segment_id <- st_nearest_feature(mi_snapped, fd)
  nutrient_snapped$segment_id <- st_nearest_feature(nutrient_snapped, fd)

  # Map river segments to from_node (start) and assign to snapped points
  segment_to_node_from <- setNames(fd$from_node, seq_len(nrow(fd)))
  mi_snapped$node <- segment_to_node_from[mi_snapped$segment_id]
  nutrient_snapped$node <- segment_to_node_from[nutrient_snapped$segment_id]

  # Build river network graph using from_node and to_node directly

  # 1. Bereken lengte van elk segment in het netwerk
  # 'fd' is je flow direction shapefile (de lijnen)
  fd$length_m <- st_length(fd)

  # 2. Bouw de graaf OPNIEUW, maar nu met 'weight' (lengte)
  edges <- data.frame(
    from = as.character(fd$from_node),
    to = as.character(fd$to_node),
    weight = as.numeric(fd$length_m), # Dit is cruciaal!
    segment_id = seq_len(nrow(fd))
  )

  g <- graph_from_data_frame(edges, directed = TRUE)


  # Initialize results
  results <- list()
  # >>> NIEUWE CODE: Progress bar starten <<<
  pb <- txtProgressBar(min = 0, max = nrow(mi_snapped), style = 3)

  # For each mi point, find closest upstream quality point within 3 months
  for (i in seq_len(nrow(mi_snapped))) {
    mpt <- mi_snapped[i, ]
    mdate <- mpt$monsternamedatum
    mnode <- as.character(mpt$node)

    upstream_nodes <- subcomponent(g, v = mnode, mode = "in") %>% names()

    #geen within segment filtering -> maar is niet erg dat er een downstream fc punt wordt genomen, gezien het op hetzelfde segment ligt en dus waarschijnlijk dichtbij

    candidates <- nutrient_snapped %>%
      filter(
        as.character(node) %in% upstream_nodes,
        {
          days_before <- as.numeric(difftime(mdate, monsternamedatum, units = "days"))
          days_before >= -14 &
            days_before <= 180
        }
      )

    # candidates <- nutrient_snapped %>% #code voor 30 dagen voor en na mi meting
    #   filter(
    #     as.character(node) %in% upstream_nodes,
    #     abs(difftime(monsternamedatum, mdate, units = "days")) <= 30
    #   )

    if (nrow(candidates) > 0) {

      # We hebben kandidaten die stroomopwaarts liggen en binnen de tijd vallen.
      # Nu willen we de 'rivier-afstand' weten.

      river_distances <- c()

      for (j in seq_len(nrow(candidates))) {
        cand <- candidates[j, ]

        # Bereken de afstand via de graaf
        # Van candidate (upstream) NAAR mafy punt (downstream)
        # distances() berekent het kortste pad op basis van 'weight'
        d <- distances(
          g,
          v = as.character(cand$node),
          to = as.character(mnode),
          mode = "out" # Volg de stroomrichting naar beneden
        )

        river_distances[j] <- as.numeric(d)
      }

      # Voeg afstand toe aan kandidaten tabel
      candidates$river_dist_m <- river_distances

      # Filter: bvb maximaal 5000m STROOMAFSTAND (veel nauwkeuriger dan vogelvlucht)
      candidates_within <- candidates %>% filter(river_dist_m <= 5000)

      if (nrow(candidates_within) > 0) {
        # Kies de dichtstbijzijnde via de rivier
        match <- candidates_within[which.min(candidates_within$river_dist_m), ]
      } else {
        match <- NA
      }

    } else {
      match <- NA
    }

    results[[i]] <- list(mi = mpt, quality = match)

    # >>> NIEUWE CODE: Progress bar updaten <<<
    setTxtProgressBar(pb, i)
  }

  # >>> NIEUWE CODE: Progress bar sluiten <<<
  close(pb)

  # Convert results into a data frame
  matched_mi <- do.call(rbind, lapply(results, function(x) x$mi))
  # matched_quality <- do.call(rbind, lapply(results, function(x) x$quality))
  #
  # # Only keep results that have a valid 'quality' entry
  # valid_quality <- lapply(results, function(x) {
  #   if (!is.null(x$quality) && inherits(x$quality, "sf")) {
  #     return(x$quality)
  #   } else {
  #     return(NULL)
  #   }
  # })
  #
  # # Remove NULLs
  # valid_quality <- Filter(Negate(is.null), valid_quality)
  #
  # # Bind all valid quality points into one sf object
  # matched_quality <- do.call(rbind, valid_quality)

  #lijkt te werken
  # Step 1: Get template from first valid entry
  first_valid <- NULL
  for (x in results) {
    if (!is.null(x$quality) && inherits(x$quality, "sf")) {
      first_valid <- x$quality[0, ]  # zero-row sf with correct structure
      break
    }
  }
  if (is.null(first_valid)) stop("No valid quality matches found.")

  # Step 2: Create empty row constructor
  make_empty_row <- function(template) {
    empty <- template[1, ]
    for (col in names(template)) {
      if (col != attr(template, "sf_column")) {
        empty[[col]] <- NA
      }
    }
    # Assign empty point geometry
    st_geometry(empty) <- st_sfc(st_point(), crs = st_crs(template))
    return(empty)
  }

  # Step 3: Apply logic
  matched_quality <- do.call(rbind, lapply(results, function(x) {
    if (!is.null(x$quality) && inherits(x$quality, "sf")) {
      return(x$quality)
    } else {
      return(make_empty_row(first_valid))
    }
  }))


  # Optionally combine into one data frame
  matched_df <- bind_cols(st_drop_geometry(matched_mi), st_drop_geometry(matched_quality)) %>%
    mutate(monsternamedatum...6 = as.Date(monsternamedatum...6))
  matched_sf_fd <- st_sf(matched_df, geometry = st_geometry(matched_mi))

  matched_sf_fd %>%
    drop_na(meetplaats...5) %>% nrow()
  save(matched_df, file = "data/verwerkt/matched_df_mi_nutrient.rdata")


load(file = "data/verwerkt/matched_df_mi_nutrient.rdata")

filtered_matches <- matched_df %>%
  filter(!is.na(meetplaats...5)) %>%

  # Count how many measurements matched per location
  duplicates_check <- matched_df %>%
  filter(!is.na(meetplaats...5)) %>% # Filter only successful matches
  group_by(meetplaats...1) %>%       # Group by the MI location ID
  summarise(n = n()) %>%
  arrange(desc(n))

# Show locations with more than 1 match
head(duplicates_check)
