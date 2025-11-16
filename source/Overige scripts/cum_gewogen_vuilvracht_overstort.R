source(here::here("source", "inladen_packages.R"))

# vuilvracht van overstorten in een afstroomgebied met buffer en afstandsgewogen ----
locations <- read_sf(here("data", "verwerkt", "hydrologisch", "mi_meetpunten_snapped_to_streams.shp"))

vuilvracht_overstorten <- st_read(here("data", "ruw" , "overstorten", "vuilvracht", "OS_maart2025.shp"))

buffered_watersheds <- st_read(here("data", "verwerkt", "hydrologisch", "mi_meetpunten_watersheds_buffered_all.gpkg"))

points_in_watersheds <- st_join(vuilvracht_overstorten, buffered_watersheds, left = FALSE)

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
load(here("data", "verwerkt", "overstorten", "gefilterde_meetplaatsen_overstorten.rdata"))

cum_vuilvracht_watershed <- buffered_watersheds %>%
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


save(cum_vuilvracht_watershed,
     file = here("data", "verwerkt", "overstorten", "mi_meetpunten_vuilvracht.rdata"))

test <- buffered_watersheds %>% filter(meetplaats == "OW389965")
test2 <- buffered_watersheds %>% filter(meetplaats == "OW389965")
mapview(test) +
  mapview(test2) +
  mapview(vuilvracht_overstorten) +
  mapview(locations)
