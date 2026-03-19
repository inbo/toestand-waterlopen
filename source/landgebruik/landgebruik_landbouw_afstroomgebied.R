if (!exists("packages_geladen")) {
  source(here::here("source", "inladen_packages.R"))
}

# --- 1. Data Inladen ---
mi_meetpunten <- st_read(here("data", "ruw", "macroinvertebraten", "mi_meetpunten_datum.gpkg"), quiet = TRUE) %>%
  filter(monsternamedatum > "2009-12-31")

afstroomgebieden <- st_read(here("data", "verwerkt", "hydrologisch", "mi_meetpunten_watersheds_buffered_5000m.gpkg"), quiet = TRUE)
crs_referentie <- st_crs(afstroomgebieden)

lbg_intensiteit_scores <- read_xlsx(here("data", "ruw", "landgebruik", "landbouwgebruikspercelen", "landbouwgebruikspercelen_intensiteitsscores.xlsx")) %>%
  mutate(combo = (gewasbescherming + nitraatresidues)/2)

# --- 2. Tijdsvoorbereiding ---
data_basis <- mi_meetpunten %>%
  st_drop_geometry() %>%
  select(meetplaats, monsternamedatum) %>%
  mutate(jaar = year(monsternamedatum)) %>%
  left_join(afstroomgebieden %>% select(meetplaats, oppervlakte), by = "meetplaats") %>%
  st_as_sf()

# --- 3. Berekening met Optimalisaties ---
jaren <- 2008:2023
resultaten_lijst <- list()

for (j in jaren) {

  message(paste0("🚀 Verwerken jaar: ", j))

  # A. Filter afstroomgebieden voor dit jaar
  data_jaar <- data_basis %>% filter(jaar == j)

  if (nrow(data_jaar) == 0) {
    message(paste0("  -> Geen meetpunten voor jaar ", j, ". Sla over."))
    next
  }

  # B. Laad enkel de benodigde percelenkaart
  pad_shp <- here("data", "ruw", "landgebruik", "landbouwgebruikspercelen", paste0("Lbgebrperc", j, ".shp"))

  if (!file.exists(pad_shp)) {
    message(paste0("  -> Bestand niet gevonden: ", pad_shp))
    next
  }

  # C. Efficiënt inlezen en filteren
  # We selecteren enkel de GEWASGROEP om geheugen te besparen
  percelen_sf <- st_read(pad_shp, quiet = TRUE) %>%
    st_transform(crs_referentie) %>%
    select(GEWASGROEP)

  #

  # OPTIMALISATIE 1: st_filter gebruikt de spatial index (R-tree)
  # Dit is vele malen sneller dan direct st_intersection()
  percelen_filtered <- percelen_sf %>%
    st_filter(data_jaar, .predicate = st_intersects)

  # Koppel scores direct aan de gefilterde set
  percelen_met_scores <- percelen_filtered %>%
    inner_join(lbg_intensiteit_scores, by = "GEWASGROEP")

  if (nrow(percelen_met_scores) == 0) {
    message("  -> Geen landbouwpercelen in de afstroomgebieden gevonden.")
    next
  }

  # D. Ruimtelijke Intersectie
  #
  intersectie <- st_intersection(data_jaar, percelen_met_scores)

  # E. Bereken de gewogen scores
  scores_jaar <- intersectie %>%
    mutate(
      oppervlakte_intersectie = st_area(st_geometry(.)),
      fractie_oppervlakte = as.numeric(oppervlakte_intersectie) / as.numeric(oppervlakte)
    ) %>%
    st_drop_geometry() %>%
    group_by(meetplaats, monsternamedatum) %>%
    summarise(
      intensiteit_gewasbescherming = sum(fractie_oppervlakte * gewasbescherming, na.rm = TRUE),
      intensiteit_nitraatresidu = sum(fractie_oppervlakte * nitraatresidues, na.rm = TRUE),
      intensiteit_combo = sum(fractie_oppervlakte * combo, na.rm = TRUE),
      aandeel_landbouw_totaal = sum(fractie_oppervlakte, na.rm = TRUE),
      .groups = 'drop'
    )

  resultaten_lijst[[as.character(j)]] <- scores_jaar

  # OPTIMALISATIE 2: Geheugen actief vrijmaken
  rm(percelen_sf, percelen_filtered, percelen_met_scores, intersectie)
  gc() # Garbage collection
}

# --- 4. Resultaten Combineren en Opslaan ---
intensiteit_landbouw_afstroomgebieden_scores <- bind_rows(resultaten_lijst)

cat("\n✅ Berekening voltooid.\n")

save(intensiteit_landbouw_afstroomgebieden_scores,
     file = here("data", "verwerkt", "landgebruik", "intensiteit_landbouw_scores_afstroomgebieden.rdata"))
