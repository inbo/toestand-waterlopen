library(sf)
library(igraph)
library(tidyverse)
library(lubridate)

# ==============================================================================
# NIEUWE FUNCTIE: Match in een straal (Radius) - Ongeacht stroomrichting
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
  target_sf$jaar  <- year(target_sf$datum)

  source_sf$datum <- as.Date(source_sf$monsternamedatum)
  source_sf$jaar  <- year(source_sf$datum)

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
mafy_data <- st_read(here("data", "ruw", "macrofyten", "mafy_meetpunten_datum.gpkg")) %>%
  filter(monsternamedatum > '2009-12-31')

mi_data <- st_read(here("data", "ruw", "macroinvertebraten", "mi_meetpunten_datum.gpkg"), quiet = T) %>%
  filter(monsternamedatum > '2009-12-31')

# Voorbeeld aanroep:
# MI koppelen aan Macrofyten (MP)
# - Zelfde jaar (year_window = 0)
# - Maximaal 2km van elkaar verwijderd (via de rivier)
# - Zowel stroomop als stroomafwaarts

mi_met_mp <- match_nearby(
  target_sf = mi_data,
  source_sf = mafy_data,
  network_list = river_network,
  max_dist_m = 2000,       # 2000 meter radius
  year_window = 0,         # Zelfde kalenderjaar
  selection_mode = "closest_distance"
)

print(paste("Aantal matches:", sum(!is.na(mi_met_mafy$mp_meetplaats))))
# Opslaan
# save(mi_met_mp, file = "mi_met_mp_matched.rdata")
