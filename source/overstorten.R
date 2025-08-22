source(here::here("source", "inladen_packages.R"))

# aantal overstorten in een afstroomgebied ----
# Dit is op basis van de kaart met overstorten aangeleverd door Ine, ism Karel De Visscher. Basis zijn alle locaties, uitlaat zouden locaties zijn die verbonden zijn met een waterloop.
overstorten_basis_locaties <- st_read(here("data", "ruw", "overstorten", "P_OS_basis.shp"))
overstorten_uitlaat_vha <- st_read(here("data", "ruw", "overstorten", "P_OS_uitlaat_VHA.shp"))

watersheds_buffered <- st_read(here("data", "verwerkt", "hydrologisch", "mi_meetpunten_watersheds_buffered_all.gpkg"))

watersheds_nested <- st_read(here("data", "verwerkt", "hydrologisch", "mi_meetpunten_watersheds_nested_all.gpkg")) #gpkg met alle afstroomgebieden!

# Perform spatial join: Assign points to watersheds
overstorten_in_afstroomgebied <- st_join(overstorten_uitlaat_vha, watersheds_buffered, left = FALSE)

# Count points per watershed
watershed_point_counts <- overstorten_in_afstroomgebied  %>%
  count(meetplaats) %>%
  rename(aantal_overstorten = n)

# Merge counts back into watersheds
watersheds_aantal_overstorten <- watersheds_buffered %>%
  left_join(watershed_point_counts %>%
              st_drop_geometry(),
            by = "meetplaats") %>%
  mutate(aantal_overstorten = replace_na(aantal_overstorten, 0)) %>%
  st_drop_geometry()


# Save the output
save(watersheds_aantal_overstorten, file = here("data", "verwerkt", "overstorten", "mi_meetpunten_aantal_overstorten_afstroomgebied.rdata"))

# jaarvuilvracht van overstorten in een afstroomgebied met buffer en afstandsgewogen ----
# Hier bereken ik de cumulatieve vuilvracht (BZV) per afstroomgebied van een meetpunt. Dit wordt afstandsgewogen, omdat een ver overstort minder effect zal hebben dan een dichter (niet kwadratisch; optioneel kan kwadratisch (exponent in functie))
vuilvracht_overstorten <- st_read(here("data", "ruw" , "overstorten", "vuilvracht", "OS_maart2025.shp"))

locations <- read_sf(here("data", "verwerkt", "hydrologisch", "mi_meetpunten_snapped_to_streams.shp"))

# Perform spatial join: Assign points to watersheds
points_in_watersheds <- st_join(vuilvracht_overstorten, watersheds_buffered, left = FALSE)

locations_cum_gewogen_vuilvracht <- locations


# --- 2. Functie voor afstandsgewogen BZV-berekening ----

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

# --- 3. Toepassen op alle meetplaatsen ----

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

save(cum_vuilvracht_watershed, file = here("data", "verwerkt", "overstorten", "mi_meetpunten_vuilvracht_bzv.rdata"))

# visualisatie
plot <- watersheds_nested %>% filter(meetplaats == "OW100000")
plot2 <- watersheds_buffered %>% filter(meetplaats == "OW100000")
plot3 <- points_in_watersheds %>% filter(meetplaats == "OW100000")
plot4 <- locations %>% filter(meetplaats == "OW100000")
plot5 <- overstorten_in_afstroomgebied %>% filter(meetplaats == "OW100000")
mapview(plot) +
  mapview(plot2) +
  mapview(plot3) +
  mapview(plot4) +
  mapview(plot5)

afstroomgebied_full <- watersheds_nested %>% filter(meetplaats == "OW118000")
afstroomgebied_buffer <- watersheds_buffered %>% filter(meetplaats == "OW118000")
overstorten_vuilvracht <- points_in_watersheds %>% filter(meetplaats == "OW118000")
meetplaats_punt <- locations %>% filter(meetplaats == "OW118000")
overstorten_uitlaat <- overstorten_in_afstroomgebied %>% filter(meetplaats == "OW118000")
mapview(afstroomgebied_full) +
  mapview(afstroomgebied_buffer) +
  mapview(meetplaats_punt, col.regions = "red") +
  mapview(overstorten_vuilvracht, col.regions = "green") +
  mapview(overstorten_uitlaat)

