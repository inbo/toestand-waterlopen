source(here::here("source", "inladen_packages.R"))

##########
# aantal overstorten in een afstroomgebied ----
############

# Dit is op basis van de kaart met overstorten aangeleverd door Ine, ism Karel De Visscher. Basis zijn alle locaties, uitlaat zouden locaties zijn die verbonden zijn met een waterloop.
overstorten_basis_locaties <- st_read(here("data", "ruw", "overstorten", "P_OS_basis.shp"))
overstorten_uitlaat_vha <- st_read(here("data", "ruw", "overstorten", "P_OS_uitlaat_VHA.shp"))

watersheds_buffered <- st_read(here("data", "verwerkt", "hydrologisch", "mi_meetpunten_watersheds_buffered_500m.gpkg"))

load(file = here("data", "verwerkt", "afstroomgebieden_binnen_vlaanderen.rdata"))


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
  st_drop_geometry() %>%
  filter(meetplaats %in% afstroomgebieden_binnen_vlaanderen$meetplaats)



# Save the output
save(watersheds_aantal_overstorten, file = here("data", "verwerkt", "overstorten", "mi_meetpunten_aantal_overstorten_afstroomgebied.rdata"))

############
# vuilvracht van overstorten in een afstroomgebied met buffer en afstandsgewogen ----
##############

locations <- read_sf(here("data", "verwerkt", "hydrologisch", "mi_meetpunten_snapped_to_streams.shp"))

vuilvracht_overstorten <- st_read(here("data", "ruw" , "overstorten", "vuilvracht", "OS_maart2025.shp"))

watersheds_buffered <- st_read(here("data", "verwerkt", "hydrologisch", "mi_meetpunten_watersheds_buffered_all.gpkg"))

points_in_watersheds <- st_join(vuilvracht_overstorten, watersheds_buffered, left = FALSE)

load(here("data", "verwerkt", "overstorten", "gefilterde_meetplaatsen_overstorten.rdata"))

locations_cum_gewogen_vuilvracht <- locations


# --- 2. Functie uitbreiden naar alle variabelen ---

calculate_weighted_values <- function(meetplaats_id_val, overstorten_sf, meetplaatsen_sf, exponent = 1) {
  current_location <- meetplaatsen_sf %>%
    filter(meetplaats == meetplaats_id_val)

  relevant_overstorten <- overstorten_sf %>%
    filter(meetplaats == meetplaats_id_val)

  if (nrow(relevant_overstorten) == 0) {
    return(rep(0, 10))
  }

  afstanden <- st_distance(relevant_overstorten, current_location) %>% as.numeric()
  afstanden[afstanden == 0] <- 0.001
  gewichten <- 1 / (afstanden ^ exponent)

  c(
    BZV = sum(relevant_overstorten$BZV * gewichten, na.rm = TRUE),
    CZV = sum(relevant_overstorten$CZV * gewichten, na.rm = TRUE),
    NH4 = sum(relevant_overstorten$NH4 * gewichten, na.rm = TRUE),
    NKJ = sum(relevant_overstorten$NKJ * gewichten, na.rm = TRUE),
    NO2 = sum(relevant_overstorten$NO2 * gewichten, na.rm = TRUE),
    NO3 = sum(relevant_overstorten$NO3 * gewichten, na.rm = TRUE),
    NT  = sum(relevant_overstorten$NT  * gewichten, na.rm = TRUE),
    PT  = sum(relevant_overstorten$PT  * gewichten, na.rm = TRUE),
    SS  = sum(relevant_overstorten$SS  * gewichten, na.rm = TRUE),
    IE  = sum(relevant_overstorten$IE  * gewichten, na.rm = TRUE)
  )
}


# --- 3. Nieuwe kolommen toevoegen ---

vars <- c("BZV", "CZV", "NH4", "NKJ", "NO2", "NO3", "NT", "PT", "SS", "IE")

for (v in vars) {
  locations_cum_gewogen_vuilvracht[[paste0("weighted_cumulative_", v)]] <- NA_real_
}

# --- Berekenen voor alle meetplaatsen ---

for (i in 1:nrow(locations_cum_gewogen_vuilvracht)) {
  current_meetplaats_id <- locations_cum_gewogen_vuilvracht$meetplaats[i]

  values <- calculate_weighted_values(
    meetplaats_id_val = current_meetplaats_id,
    overstorten_sf = points_in_watersheds,
    meetplaatsen_sf = locations_cum_gewogen_vuilvracht,
    exponent = 1
  )

  for (j in seq_along(vars)) {
    locations_cum_gewogen_vuilvracht[[paste0("weighted_cumulative_", vars[j])]][i] <- values[j]
  }
}

print(locations_cum_gewogen_vuilvracht)


# --- 4. Join terug naar watersheds en filter op databeschikbaarheid en Vlaamse extent ---

cum_vuilvracht_watershed_500m <- watersheds_buffered %>%
  left_join(st_drop_geometry(locations_cum_gewogen_vuilvracht),
            by = "meetplaats") %>%
  mutate(across(
    starts_with("weighted_cumulative_"),
    ~ replace_na(.x, 0),
    .names = "cum_gewogen_{sub('weighted_cumulative_', '', .col)}"
  )) %>%
  select(-starts_with("weighted_cumulative_")) %>%
  st_drop_geometry() %>%
  filter(meetplaats %in% gefilterde_meetplaatsen_overstorten$meetplaats)


save(cum_vuilvracht_watershed_500m,
     file = here("data", "verwerkt", "overstorten", "mi_meetpunten_vuilvracht_500m.rdata"))

library(sf)
library(dplyr)
library(here)
library(sf)
library(dplyr)
library(here)

###########
# Koppeling overstorten aan macroinvertebraatpunten (binnen 100m én in afstroomgebied)
###########

# 0. Zorg CRS gelijk
vuilvracht_overstorten1 <- st_transform(vuilvracht_overstorten, st_crs(locations)) %>%
  select(ID) %>%
  rename(id_overstort = ID)
buffered_watersheds1 <- st_transform(buffered_watersheds, st_crs(locations))

# 1. Hernoem meetplaats in buffered_watersheds zodat er geen naamconflict ontstaat
buffered_watersheds2 <- buffered_watersheds1 %>%
  rename(meetplaats_wsh = meetplaats)

# 2. Koppel overstorten aan het afstroomgebied waarin ze liggen (meetplaats_wsh)
overstorten_in_wsh <- st_join(
  vuilvracht_overstorten1,
  buffered_watersheds2 %>% select(meetplaats_wsh),
  join = st_within,
  left = FALSE
)

# 3. Hernoem meetplaats in locations zodat we duidelijk onderscheid maken
locations2 <- locations %>%
  rename(meetplaats_loc = meetplaats) %>%
  select(-fid)

# 4. Vind alle overstort - locatie paren binnen 100 m
pairs_within_100m <- st_join(
  overstorten_in_wsh,
  locations2,
  join = st_is_within_distance,
  dist = 100,
  left = FALSE
)

# 5. Filter enkel de paren waarbij de overstort ook in het afstroomgebied van die locatie ligt
matched_pairs <- pairs_within_100m %>%
  filter(meetplaats_wsh == meetplaats_loc) #punten zijn locaties van overstorten

# 6. Maak een dataframe met enkel IDs (zonder geometrie)
matched_df <- matched_pairs %>%
  st_drop_geometry() %>%
  select(id_overstort, meetplaats = meetplaats_loc)

# 7. Selecteer de unieke meetpunten die een match hebben
matched_meetpunten <- locations2 %>%
  filter(meetplaats_loc %in% unique(matched_pairs$meetplaats_loc))

# 8. Controleer aantallen
cat("Aantal overstorten in matched_pairs:", nrow(matched_pairs), "\n")
cat("Aantal unieke meetpunten met match:", nrow(matched_meetpunten), "\n")

# 9. Optioneel: opslaan als CSV of RDS
save(matched_df, file = here("data", "verwerkt", "overstorten", "overstorten_meetpunten_match_100m.rdata"))

# (optionele visualisatie)
library(mapview)
mapview(buffered_watersheds2, alpha.regions = 0.2) +
  mapview(matched_meetpunten, col.regions = "green") +
  mapview(matched_pairs, col.regions = "red")

meetpunten_OW939000 <- matched_meetpunten %>% filter(meetplaats_loc == "OW939000")
overstorten_OW939000 <- matched_pairs %>% filter(meetplaats_loc == "OW939000")
watershed_OW939000 <- buffered_watersheds2 %>% filter(meetplaats_wsh == "OW939000")
mapview(meetpunten_OW939000, col.regions = "green") + mapview(overstorten_OW939000, col.regions = "red") +
  mapview(watershed_OW939000)

###########
# visualisatie
##############

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

