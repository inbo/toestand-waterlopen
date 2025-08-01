# vuilvracht van overstorten in een afstroomgebied met buffer en afstandsgewogen ----

vuilvracht_overstorten <- st_read(here("data", "ruw" , "overstorten", "vuilvracht", "OS_maart2025.shp"))

locations_buffer <- locations %>%
  mutate(buffer = st_buffer(geometry, dist = 5000))

watersheds <- watersheds_nested
buffers <- locations_buffer %>%
  st_drop_geometry() %>%
  mutate(geometry = st_sfc(buffer, crs = crs(watersheds_nested))) %>%
  select(-buffer) %>%
  st_as_sf(.)

# Make sure both layers have the same CRS
watersheds <- st_transform(watersheds, st_crs(buffers$geometry))

# Match rows by meetplaats
idx <- match(buffers$meetplaats, watersheds$meetplaats)

# Check for unmatched IDs (optional safety check)
if (any(is.na(idx))) {
  warning("Some meetplaats values in buffers do not match those in watersheds")
}

# Grab the geometries
buffer_geoms <- st_geometry(buffers)
watershed_geoms <- st_geometry(watersheds)[idx]

# Perform row-by-row intersection
intersections <- map2(buffer_geoms, watershed_geoms, st_intersection)

# Replace geometry in buffers with the intersection result
result <- buffers %>%
  mutate(geometry = st_sfc(intersections, crs = st_crs(buffers)))

# Optional: drop empty geometries (e.g., no intersection)
buffered_watersheds <- result %>% filter(!st_is_empty(geometry))

# Perform spatial join: Assign points to watersheds
points_in_watersheds <- st_join(vuilvracht_overstorten, buffered_watersheds, left = FALSE)

points_in_watersheds

locations_cum_gewogen_vuilvracht <- locations


# --- 2. Functie voor afstandsgewogen BZV-berekening ---

# Deze functie berekent de gewogen BZV voor EEN ENKEL meetpunt
calculate_weighted_bzv <- function(meetplaats_id_val, overstorten_sf, meetplaatsen_sf, exponent = 1) {
  # Haal de specifieke meetplaats geometrie op
  current_location <- meetplaatsen_sf %>%
    filter(meetplaats == meetplaats_id_val)

  # Haal de overstorten op die bij dit meetpunt horen
  relevant_overstorten <- overstorten_sf %>%
    filter(meetplaats == meetplaats_id_val)

  # Als er geen relevante overstorten zijn, retourneer 0
  if (nrow(relevant_overstorten) == 0) {
    return(0)
  }

  # Bereken de afstand van elk relevant overstortpunt tot de meetplaats
  # st_distance werkt met sf-objecten; het resultaat is een matrix, dus pak de diagonaal of de eerste rij/kolom
  # aangezien current_location slechts 1 punt is.
  afstanden <- st_distance(relevant_overstorten, current_location) %>%
    as.numeric()

  # Voorkom delen door nul als een overstort exact op het meetpunt ligt
  afstanden[afstanden == 0] <- 0.001

  # Bereken de gewichten met inverse afstand
  gewichten <- 1 / (afstanden ^ exponent)

  # Bereken de afstandsgewogen cumulatieve BZV
  weighted_bzv_sum <- sum(relevant_overstorten$BZV * gewichten)

  return(weighted_bzv_sum)
}

# --- 3. Toepassen op alle meetplaatsen ---

# Initialiseer een nieuwe kolom in het 'locations' sf-object
locations_cum_gewogen_vuilvracht$weighted_cumulative_BZV <- NA_real_

# Loop over elke meetplaats in het 'locations' sf-object
for (i in 1:nrow(locations)) {
  current_meetplaats_id <- locations_cum_gewogen_vuilvracht$meetplaats[i]

  # Bereken de gewogen BZV voor de huidige meetplaats
  # Gebruik exponent = 1 voor inverse afstand, exponent = 2 voor inverse afstand kwadraat
  calculated_bzv <- calculate_weighted_bzv(
    meetplaats_id_val = current_meetplaats_id,
    overstorten_sf = points_in_watersheds,
    meetplaatsen_sf = locations_cum_gewogen_vuilvracht,
    exponent = 1 # Pas dit aan indien je een ander exponent wilt (bv. 2 voor 1/d^2)
  )

  # Sla het resultaat op
  locations_cum_gewogen_vuilvracht$weighted_cumulative_BZV[i] <- calculated_bzv
}

# --- 4. Resultaten bekijken ---
print(locations_cum_gewogen_vuilvracht)


# Merge counts back into watersheds
cum_vuilvracht_watershed <- watersheds_nested %>%
  left_join(locations_cum_gewogen_vuilvracht %>%
              st_drop_geometry(),
            by = "meetplaats") %>%
  mutate(cum_gewogen_bzv = replace_na(weighted_cumulative_BZV, 0)) %>%
  select(-weighted_cumulative_BZV) %>%
  st_drop_geometry()

test <- watersheds_nested %>% filter(meetplaats == "OW389965")
test2 <- buffered_watersheds %>% filter(meetplaats == "OW389965")
mapview(test) +
  mapview(test2) +
  mapview(vuilvracht_overstorten) +
  mapview(locations)

save(cum_vuilvracht_watershed, file = here("data", "verwerkt", "overstorten", "mi_meetpunten_vuilvracht_bzv.rdata"))
