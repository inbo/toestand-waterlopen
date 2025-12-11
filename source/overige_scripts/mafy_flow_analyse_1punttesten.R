i <- 3117

mpt <- mafy_snapped[i, ]
mdate <- mpt$monsternamedatum
mnode <- as.character(mpt$node)

upstream_nodes <- subcomponent(g, v = mnode, mode = "in") %>% names()

#geen within segment filtering -> maar is niet erg dat er een downstream fc punt wordt genomen, gezien het op hetzelfde segment ligt en dus waarschijnlijk dichtbij
candidates <- fc_snapped %>%
  filter(
    as.character(node) %in% upstream_nodes,
    abs(difftime(monsternamedatum, mdate, units = "days")) <= 600
  )
#
# #dit nog eerst eens testen
# # Initial candidates based on topology and time
# candidates_raw <- fc_snapped %>%
#   filter(
#     as.character(node) %in% upstream_nodes,
#     abs(difftime(monsternamedatum, mdate, units = "days")) <= 90
#   )
#
# # Filter same-segment downstream points using location on line
# same_segment <- candidates_raw %>% filter(segment_id == mpt$segment_id)
# other_segments <- candidates_raw %>% filter(segment_id != mpt$segment_id)
#
# if (nrow(same_segment) > 0) {
#   river_line <- rivers[mpt$segment_id, ]
#
#   # Get snapped point along river (as point, not line)
#   mpt_proj <- st_nearest_points(mpt, river_line) %>% st_cast("POINT") %>% .[2]
#
#   mpt_dist <- st_line_length(st_segmentize(st_cast(st_union(mpt_proj, river_line), "LINESTRING"), dfMaxLength = 1)) %>% as.numeric()
#
#   # Function to get distance from start of river line
#   point_position_along_line <- function(pt) {
#     proj <- st_nearest_points(pt, river_line) %>% st_cast("POINT") %>% .[2]
#     dist <- st_line_length(st_segmentize(st_cast(st_union(proj, river_line), "LINESTRING"), dfMaxLength = 1))
#     as.numeric(dist)
#   }
#
#   same_segment <- same_segment %>%
#     rowwise() %>%
#     mutate(fc_dist = point_position_along_line(geometry)) %>%
#     ungroup() %>%
#     filter(fc_dist < mpt_dist)
#
# }
#
# candidates <- bind_rows(other_segments, same_segment)


mapview(mpt, color = "red") +
  mapview(candidates, color = "green") +
  mapview(fc_meetpunten_meetplaats, color = "orange", legend = F) +
  mapview(rivers) +
  # mapview(streams_sf, color= "orange") +
  mapview(nodes, color = "blue", legend = F)
