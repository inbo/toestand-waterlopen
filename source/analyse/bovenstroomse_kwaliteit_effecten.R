
# zoek op geminithread: macroinvertebraten netwerkanalyse in R

# Voeg je MMIF waarden toe (hier als dummy data om de code te laten werken)
# Vervang de runif() door jouw echte koppeling.

mi_data <- st_read(here("data", "ruw", "macroinvertebraten", "mi_meetpunten_datum.gpkg"), quiet = T) %>%
  filter(monsternamedatum > '2009-12-31') %>%
  left_join(meetnetten %>%
              select(vhas, nummer),
            by = c("meetplaats" = "nummer"))

mi_data <- mi_data %>%
  left_join(mi_data_analyse %>%
              st_drop_geometry() %>%
              select(meetplaats, monsternamedatum, mmif, ep_tw, ns_tw, mt_sw, ta_xw, sw_dw),
            by = c("meetplaats", "monsternamedatum"))

match_upstream_mi_effects <- function(mi_sf,
                                      network_list,
                                      max_dist_m = 5000,
                                      snap_tolerance = 25,
                                      days_before = 365,
                                      days_after = 0,
                                      exclude_same_station = TRUE,
                                      aggr_cols = c("mmif"), # Nu als vector in te vullen
                                      vhas_col_network = "VHAS",
                                      vhas_col_biota = "vhas") {

  g <- network_list$graph
  net_sf <- network_list$network_sf

  mi_sf$temp_obs_id <- seq_len(nrow(mi_sf))
  mi_sf$monsternamedatum <- as.Date(mi_sf$monsternamedatum)

  message("1/4 Snapping MI points to river network...")

  snap_mi <- function(points, network) {
    points$node_id <- NA
    if(!is.null(vhas_col_biota) && !is.null(vhas_col_network)) {
      pts_vhas <- which(!is.na(points[[vhas_col_biota]]))
      for (code in unique(points[[vhas_col_biota]][pts_vhas])) {
        net_idx <- which(network[[vhas_col_network]] == code)
        if (length(net_idx) > 0) {
          p_idx <- which(points[[vhas_col_biota]] == code)
          nearest <- st_nearest_feature(points[p_idx,], network[net_idx,])
          points$node_id[p_idx] <- as.character(network$from_node[net_idx[nearest]])
        }
      }
    }
    missing <- which(is.na(points$node_id))
    if (length(missing) > 0) {
      nearest <- st_nearest_feature(points[missing, ], network)
      dists <- as.numeric(st_distance(points[missing, ], network[nearest, ], by_element = TRUE))
      valid <- dists <= snap_tolerance
      points$node_id[missing[valid]] <- as.character(network$from_node[nearest[valid]])
    }
    return(points)
  }

  mi_sf <- snap_mi(mi_sf, net_sf)
  mi_df <- st_drop_geometry(mi_sf)

  # Hulpfunctie om lege resultaten (met NA's) aan te maken voor alle kolommen
  create_empty_res <- function(cols) {
    res <- data.frame(upstr_n_matches = 0, upstr_min_dist = NA)
    for (col in cols) {
      res[[paste0("upstr_mean_", col)]] <- NA
      res[[paste0("upstr_idw_", col)]] <- NA
    }
    return(res)
  }

  message("2/4 Matching upstream MI points within time window...")
  results <- list()
  pb <- progress::progress_bar$new(format = "  [:bar] :percent eta: :eta", total = nrow(mi_sf), clear = FALSE)

  for (i in seq_len(nrow(mi_sf))) {
    pb$tick()
    target_pt <- mi_sf[i, ]
    b_node <- target_pt$node_id

    if (is.na(b_node)) {
      res <- create_empty_res(aggr_cols)
    } else {
      candidates <- mi_df %>% filter(!is.na(node_id), temp_obs_id != target_pt$temp_obs_id)

      if (exclude_same_station) {
        candidates <- candidates %>% filter(meetplaats != target_pt$meetplaats)
      }

      if (nrow(candidates) > 0) {
        candidates <- candidates %>%
          mutate(days_diff = as.numeric(difftime(target_pt$monsternamedatum, monsternamedatum, units = "days"))) %>%
          filter(days_diff >= -days_after & days_diff <= days_before)
      }

      if (nrow(candidates) > 0) {
        upstream_nodes <- names(igraph::subcomponent(g, v = b_node, mode = "in"))
        candidates <- candidates %>% filter(node_id %in% upstream_nodes)

        if (nrow(candidates) > 0) {
          unique_cand_nodes <- unique(candidates$node_id)
          d_matrix <- igraph::distances(g, v = unique_cand_nodes, to = b_node, mode = "out")
          lookup_dist <- setNames(as.numeric(d_matrix), unique_cand_nodes)

          candidates$river_dist_m <- lookup_dist[candidates$node_id]
          candidates <- candidates %>% filter(river_dist_m <= max_dist_m)
        }
      }

      if (nrow(candidates) > 0) {
        # Maak basis dataframe
        res <- data.frame(
          upstr_n_matches = nrow(candidates),
          upstr_min_dist  = min(candidates$river_dist_m, na.rm = TRUE)
        )

        # Gewichten voor IDW
        weights <- 1 / (candidates$river_dist_m + 1)

        # Loop over alle doorgegeven variabelen
        for (col in aggr_cols) {
          vals <- candidates[[col]]
          valid_idx <- !is.na(vals)

          # 1. Gewoon gemiddelde
          res[[paste0("upstr_mean_", col)]] <- mean(vals, na.rm = TRUE)

          # 2. Afstandsgewogen gemiddelde (IDW)
          if (sum(valid_idx) > 0) {
            res[[paste0("upstr_idw_", col)]] <- sum(weights[valid_idx] * vals[valid_idx]) / sum(weights[valid_idx])
          } else {
            res[[paste0("upstr_idw_", col)]] <- NA
          }
        }

      } else {
        res <- create_empty_res(aggr_cols)
      }
    }

    res$temp_obs_id <- target_pt$temp_obs_id
    results[[i]] <- res
  }

  message("\n3/4 Merging results...")
  matched_df <- do.call(rbind, results)
  final_sf <- left_join(mi_sf, matched_df, by = "temp_obs_id")

  final_sf$temp_obs_id <- NULL

  message("4/4 Klaar!")
  return(final_sf)
}

# 1. Koppel de punten
mi_data_met_effecten <- match_upstream_mi_effects(
  mi_sf = mi_data,
  network_list = river_network_fine,
  max_dist_m = 5000,            # Zoek tot 5km bovenstrooms
  days_before = 365,            # Maximaal 1 jaar oudere data
  exclude_same_station = TRUE,  # Fysiek andere locaties vereist
  aggr_col = c("mmif", "ep_tw", "sw_dw", "ta_xw", "ns_tw", "mt_sw")
)

# 2. Filter de dataset om enkel de punten met een match over te houden
model_data <- mi_data_met_effecten %>%
  filter(upstr_n_matches > 0) %>%
  st_drop_geometry() %>%
  left_join(mi_data_analyse %>%
              st_drop_geometry() %>%
              select(meetplaats, monsternamedatum, groep),
            by = c("meetplaats", "monsternamedatum")) %>%
  filter(groep != "zeer_grote_rivier") %>%
  mutate(jaar = lubridate::year(monsternamedatum))

# 3. Test je hypothese
# Een simpele lineaire regressie om te zien of een stijging in bovenstroomse MMIF
# leidt tot een significante stijging in lokale MMIF.
model <- glmmTMB(mmif ~ upstr_mean_mmif * groep + (1 | meetplaats), data = model_data,
            na.action = na.omit,
            family = ordbeta)
model <- glmmTMB(mmif ~ upstr_idw_mmif * groep + (1 | meetplaats) + (1 | jaar), data = model_data,
                 na.action = na.omit,
                 family = ordbeta)

summary(model)
plot_model(model, type = "int")
pred_data <- ggpredict(model, terms = c("upstr_idw_mmif", "groep"))

# 2. Maak de plot
plot(pred_data) +
  labs(
    title = "Effect van bovenstroomse kwaliteit per waterloop-groep",
    x = "Bovenstroomse MMIF (Gemiddeld)",
    y = "Voorspelde Lokale MMIF",
    color = "Groep / Typologie"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14)
  )
# Installeer indien nodig: install.packages("marginaleffects")
library(marginaleffects)

# Optie A: De voorspellingen plotten (vergelijkbaar met ggeffects)
plot_predictions(model, condition = c("upstr_mean_mmif", "groep")) +
  theme_minimal() +
  labs(y = "Voorspelde MMIF", x = "Bovenstroomse MMIF")

# Optie B: De effectgrootte (slopes) per groep statistisch toetsen
slopes <- avg_slopes(model, variables = "upstr_mean_mmif", by = "groep")
print(slopes)
